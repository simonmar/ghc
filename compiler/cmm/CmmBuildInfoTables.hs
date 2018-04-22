{-# LANGUAGE CPP, GADTs, BangPatterns, RecordWildCards,
    GeneralizedNewtypeDeriving, NondecreasingIndentation #-}

-- See Note [Deprecations in Hoopl] in Hoopl module
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module CmmBuildInfoTables
    ( CAFSet, CAFEnv, cafAnal
    , doSRTs, ModuleSRTInfo, emptySRT, isEmptySRT )
where

#include "HsVersions.h"

import Hoopl
import Digraph
import BlockId
import CLabel
import PprCmmDecl ()
import Cmm
import CmmUtils
import Data.List
import DynFlags
import Maybes
import Outputable
import SMRep
import UniqSupply
import CostCentre
import StgCmmHeap

import PprCmm()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Prelude hiding (succ)


{- Note [SRTs]

SRTs are the mechanism by which the garbage collector can determine
the live CAFs in the program.

Representation
^^^^^^^^^^^^^^

An SRT is simply an object in the program's data segment. It has the
same representation as a static constructor.  There are 15
pre-compiled SRT info tables: stg_SRT_2_info, stg_SRT_3_info
.. stg_SRT_16_info, representing SRT objects with 2-16 pointers,
respectively.

The entries of an SRT object point to either:
- A static closure (FUN or THUNK)
- Another SRT

By traversing the transitive closure of an SRT, the GC will reach all
of the CAFs that are reachable from the code associated with this SRT.

If we need to create an SRT with more than 16 entries, we build a
chain of SRT objects with all but the last having 16 entries.


Referring to an SRT
^^^^^^^^^^^^^^^^^^^

The following things have SRTs:

- Static functions (FUN)
- Static thunks (THUNK), ie. CAFs
- Continuations (RET_SMALL, etc.)

In each case, the info table refers to the SRT.

info->srt is non-zero if there's an SRT
info->f.srt_offset refers to the SRT

On x86_64, we can do one optimisation to the info table representation
here.  The offset to the SRT can be stored in 32 bits (all code lives
within a 2GB region in x86_64's small memory model), so we can save a
word in the info table by storing the srt_offset in the srt field,
which is half a word.

EXAMPLE
^^^^^^^

f = \x. ... g ...
  where
    g = \y. ... h ... c1 ...
    h = \z. ... c2 ...

c1 & c2 are CAFs

g and h are local functions, but they have no static closures.  When
we generate code for f, we start with a CmmGroup of four CmmDecls:

   [ f_closure, f_entry, g_entry, h_entry ]

we process each CmmDecl separately in cpsTop, giving us a list of
CmmDecls. e.g. for f_entry, we might end up with

   [ f_entry, f1_ret, f2_proc ]

where f1_ret is a return point, and f2_proc is a proc-point.  We have
a CAFSet for each of these CmmDecls, let's suppose they are

   [ f_entry{g_closure}, f1_ret{g_closure}, f2_proc{} ]
   [ g_entry{h_closure, c1_closure} ]
   [ h_entry{c2_closure} ]

We'll make an SRT for each of these functions:

  f_srt : [g_closure]
  g_srt : [h_closure, c1_closure]
  h_srt : [c2_closure]

Now, note that we cannot use g_closure and h_closure in an SRT,
because there is no static closure.  Instead we want to replace those
with references to g_srt and h_srt respectively.

  f_srt : [g_srt]
  g_srt : [h_srt, c1_closure]
  h_srt : [c2_closure]

Now, when an SRT has a single entry, we don't actually generate an SRT
closure for it, instead we just replace references to it with its
single element.  So, since h_srt == c2_closure, we have

  f_srt : [g_srt]
  g_srt : [c2_closure, c1_closure]
  h_srt : [c2_closure]

and the only closure we generate is

  g_srt = SRT_2 [c2_closure, c1_closure]


Optimisations
^^^^^^^^^^^^^

To reduce the code size overhead and the cost of traversing SRTs in
the GC, we want to simplify SRTs where possible. We therefore apply
the following optimisations.  Each has a [keyword]; search for the
keyword in the code below to see where the optimisation is
implemented.

1. [Shortcut] we never create an SRT with a single entry, instead
   we replace all references to the singleton SRT with a reference
   to its element.  This includes references from info tables.

2. [FUN] A reference to a top-level FUN can point directly to its SRT
   instead.
   - this only works within a single module, where we know what the
     FUN's SRT is.
   - we cannot do this for THUNKs, because after the THUNK is
     evaluated we don't want to keep its CAFs alive any more.

3. [Common] Identical SRTs can be merged

4. [Filter] If an SRT A refers to an SRT B and a closure C, and B also
   refers to C (perhaps transitively), then we can omit the reference
   to C from A.


TODO: alternative to [FUN]: merge the FUN's SRT with the FUN object
itself.
-}

-- ---------------------------------------------------------------------
-- Label types

-- Labels that come from cafAnal can be:
--   - _closure labels for static functions or CAFs
--   - _info labels for dynamic functions, thunks, or continuations
--   - _entry labels for functions or thunks
--
-- Meanwhile the labels on top-level blocks are _entry labels.
--
-- To put everything in the same namespace we convert all labels to
-- closure labels using toClosureLbl.  Note that some of these
-- labels will not actually exist; that's ok because we're going to
-- map them to SRTEntry later, which ranges over labels that do exist.
--
newtype CAFLabel = CAFLabel CLabel
  deriving (Eq,Ord,Outputable)

type CAFSet = Set CAFLabel
type CAFEnv = BlockEnv CAFSet

mkCAFLabel :: CLabel -> CAFLabel
mkCAFLabel lbl = CAFLabel (toClosureLbl lbl)

-- This is a label that we can put in an SRT.  It *must* be a closure label,
-- pointing to either a FUN_STATIC, THUNK_STATIC, or CONSTR_STATIC.
newtype SRTEntry = SRTEntry CLabel
  deriving (Eq, Ord, Outputable)

-- ---------------------------------------------------------------------
-- CAF analysis

-- |
-- For each code block:
--   - collect the references reachable from this code block to FUN,
--     THUNK or RET labels for which hasCAF == False
-- 
-- This gives us a `CAFEnv`: a mapping from code block to sets of labels
--
cafAnal
  :: BlockSet   -- The blocks representing continuations, ie. those
                -- that will get RET info tables.  These labels will
                -- get their own SRTs, so we don't aggregate CAFs from
                -- references to these labels, we just use the label.
  -> CLabel     -- The top label of the proc
  -> CmmGraph
  -> CAFEnv
cafAnal contLbls topLbl g = dataflowAnalBwd g [] $
  analBwd cafLattice (cafTransfers contLbls (g_entry g) topLbl)


cafLattice :: DataflowLattice CAFSet
cafLattice = DataflowLattice "live cafs" Set.empty add
  where
    add _ (OldFact old) (NewFact new) =
      (changeIf $ Set.size new' > Set.size old, new')
     where
      !new' = Set.union old new

cafTransfers :: BlockSet -> BlockId -> CLabel -> BwdTransfer CmmNode CAFSet
cafTransfers contLbls entry topLbl = mkBTransfer3 first middle last
  where first  _ live = live
        middle m live = foldExpDeep addCaf m live
        last   l live = foldExpDeep addCaf l live'
           where
             facts = catMaybes (map successorFact (successors l))
             live' = joinFacts cafLattice (panic "no label") facts

             successorFact s
               -- If this is a loop back to the entry, we can refer to the
               -- entry label.
               | s == entry = Just (add topLbl Set.empty)
               -- If this is a continuation, we want to refer to the
               -- SRT for the continuation's info table
               | s `setMember` contLbls
               = Just (Set.singleton (mkCAFLabel (infoTblLbl s)))
               -- Otherwise, takes the CAF references from the destination
               | otherwise
               = lookupFact s live

        addCaf e set = case e of
               CmmLit (CmmLabel c)              -> add c set
               CmmLit (CmmLabelOff c _)         -> add c set
               CmmLit (CmmLabelDiffOff c1 c2 _ _) -> add c1 $ add c2 set
               _ -> set

        add l s = if hasCAF l then Set.insert (mkCAFLabel l) s
                              else s

-- -----------------------------------------------------------------------------
-- ModuleSRTInfo

data ModuleSRTInfo = ModuleSRTInfo
  { dedupSRTs :: Map (Set SRTEntry) SRTEntry
    -- ^ previous SRTs we've emitted, so we can de-duplicate.
    -- Used to implement the [Common] optimisation.
  , flatSRTs :: Map SRTEntry (Set SRTEntry)
    -- ^ The reverse mapping, so that we can remove redundant
    -- entries. e.g.  if we have an SRT [a,b,c], and we know that b
    -- points to [c,d], we can omit c and emit [a,b].
    -- Used to implement the [Filter] optimisation.
  }
instance Outputable ModuleSRTInfo where
  ppr ModuleSRTInfo{..} = text "ModuleSRTInfo:" <+> ppr dedupSRTs <+> ppr flatSRTs

emptySRT :: ModuleSRTInfo
emptySRT = ModuleSRTInfo { dedupSRTs = Map.empty, flatSRTs = Map.empty }

isEmptySRT :: ModuleSRTInfo -> Bool
isEmptySRT ModuleSRTInfo{..} = True

-- -----------------------------------------------------------------------------
-- Constructing SRTs

{-
- In each CmmDecl there is a mapping from BlockId -> CmmInfoTable
- The one corresponding to g_entry is the closure info table, the
  rest are continuations.
- Each one needs an SRT.
- We get the CAFSet for each one from the CAFEnv
- Make a label for each SRT that we need
- We need to map every label to its corresponding SRT label using
  closureToSRT from ModuleSRTInfo

we have:
   cafEnv: BlockId -> [CAFLabel]
   [CmmDecl]
we need:
   (for each static closure, dynamic closure, and continuation)
   SRTs: [(CLabel, [SRTEntry])]
   Info table SRT: BlockId -> Maybe SRTEntry
   cafMap: CAFLabel -> Maybe SRTEntry

1. build cafMap & SRTs:
   - dependency analysis on cafEnv to get
        [SCC ( BlockId
             , Maybe CAFLabel -- closure label or continuation label
             , [CAFLabel]     -- closure label or info label
                              -- (info for: local closures, continuations)
             )
        ]
   - for each (closureLbl, entries)
     - (srtObj, srt) = case (map cafMap entries) of
         []    -> (Nothing, Nothing)
         [one] -> (Nothing, Just one)
         many  -> (Just (srtLbl, many), Just srtLbl
          where srtLbl = toSRTLabel closure
     - extend cafEnv with (label -> srt)
     - extend SRTs with srtObj
-}


-- | Return a (BlockId,CLabel) pair for each labelled block of a CmmDecl,
--   where the label is
--   - the info label for a continuation or dynamic closure
--   - the closure label for a top-level function (not a CAF)
getLabelledBlocks :: CmmDecl -> [(BlockId, CAFLabel)]
getLabelledBlocks (CmmData _ _) = []
getLabelledBlocks (CmmProc top_info _ _ _) =
  [ (blockId, mkCAFLabel (cit_lbl info))
  | (blockId, info) <- mapToList (info_tbls top_info)
  , let rep = cit_rep info
  , not (isStaticRep rep) || not (isThunkRep rep)
  ]


-- | Get (BlockId,CLabel) pairs for each block that represents a CAF.
-- These are treated differently from other labelled blocks:
--  - we never resolve a reference to a CAF to the contents of its SRT, since
--    the point of SRTs is to keep CAFs alive.
--  - CAFs therefore don't take part in the dependency analysis in depAnalSRTs.
--    instead we generate their SRTs after everything else, so that we can
--    resolve references in the CAF's SRT.
getCAFs :: CmmDecl -> [(BlockId, CAFLabel)]
getCAFs (CmmData _ _) = []
getCAFs (CmmProc top_info topLbl _ g)
  | Just info <- mapLookup (g_entry g) (info_tbls top_info)
  , let rep = cit_rep info
  , isStaticRep rep && isThunkRep rep = [(g_entry g, mkCAFLabel topLbl)]
  | otherwise = []


-- | Put the labelled blocks that we will be annotating with SRTs into
-- dependency order.  This is so that we can process them one at a
-- time, resolving references to earlier blocks to point to their
-- SRTs.
depAnalSRTs
  :: CAFEnv
  -> [CmmDecl]
  -> [SCC (BlockId, CAFLabel, Set CAFLabel)]

depAnalSRTs cafEnv decls =
  srtTrace "depAnalSRTs" (ppr blockToLabel $$ ppr (graph ++ cafSCCs)) $
  (graph ++ cafSCCs)
 where
  cafs = concatMap getCAFs decls
  cafSCCs = [ AcyclicSCC (blockid, lbl, cafs)
            | (blockid, lbl) <- cafs
            , Just cafs <- [mapLookup blockid cafEnv] ]
  labelledBlocks = concatMap getLabelledBlocks decls
  blockToLabel :: BlockEnv CAFLabel
  blockToLabel = mapFromList (cafs ++ labelledBlocks)
  labelToBlock = Map.fromList (map swap labelledBlocks)
  graph = stronglyConnCompFromEdgedVerticesOrd
             [ let cafs' = Set.delete lbl cafs in
               ((l,lbl,cafs'), l,
                 catMaybes (map (flip Map.lookup labelToBlock)
                           (Set.toList cafs')))
             | (l, lbl) <- labelledBlocks
             , Just cafs <- [mapLookup l cafEnv] ]


-- | maps labels from 'cafAnal' to the final CLabel that will appear
-- in the SRT.
--   - closures with singleton SRTs resolve to their single entry
--   - closures with larger SRTs map to the label for that SRT
--   - CAFs must not map to anything!
--   - labels might map to Nothing, if the SRT they would refer to is
--     found to be empty (TODO: when does this happen?)
type SRTMap = Map CAFLabel (Maybe SRTEntry)

-- | resolve a CAFLabel to its SRTEntry using the SRTMap
resolveCAF :: SRTMap -> CAFLabel -> Maybe SRTEntry
resolveCAF srtMap lbl@(CAFLabel l) =
  Map.findWithDefault (Just (SRTEntry (toClosureLbl l))) lbl srtMap


-- | Attach SRTs to all info tables in the CmmDecls, and add SRT
-- declarations to the ModuleSRTInfo.
--
doSRTs
  :: DynFlags
  -> ModuleSRTInfo
  -> [(CAFEnv, [CmmDecl])]
  -> IO (ModuleSRTInfo, [CmmDecl])

doSRTs dflags topSRT tops = do
  us <- mkSplitUniqSupply 'u'

  -- Ignore the original grouping of decls, and combine all the
  -- CAFEnvs into a single CAFEnv.
  let (cafEnvs, declss) = unzip tops
      cafEnv = mapUnions cafEnvs
      decls = concat declss

  -- Put the decls in dependency order
  let sccs = depAnalSRTs cafEnv decls

  -- On each strongly-connected group of decls, construct the SRT
  -- closures and the SRT fields for info tables.
  let (((declss, pairs), _srtMap), topSRT') =
        initUs_ us $
        flip runStateT topSRT $
        flip runStateT Map.empty $
        mapAndUnzipM (doSCC dflags) sccs

  -- Next, update the info tables with the SRTs
  let decls' = map (updInfoSRTs (mapFromList (concat pairs))) decls

  return (topSRT', concat declss ++ decls')


-- | Build the SRT for a strongly-connected component of blocks
doSCC
  :: DynFlags
  -> SCC (BlockId, CAFLabel, Set CAFLabel)
  -> StateT SRTMap
        (StateT ModuleSRTInfo UniqSM)
        ( [CmmDecl]           -- generated SRTs
        , [(BlockId, CLabel)] -- SRT fields for info tables
        )

doSCC dflags  (AcyclicSCC (l, cafLbl, cafs)) =
  oneSRT dflags [l] [cafLbl] cafs

doSCC dflags (CyclicSCC nodes) = do
  -- build a single SRT for the whole cycle
  let (blockids, lbls, cafsets) = unzip3 nodes
      cafs = foldr Set.delete (Set.unions cafsets) lbls
  oneSRT dflags blockids lbls cafs


-- | Build an SRT for a set of blocks
oneSRT
  :: DynFlags
  -> [BlockId]                          -- blocks in this set
  -> [CAFLabel]                         -- labels for those blocks
  -> Set CAFLabel                       -- SRT for this set
  -> StateT SRTMap
       (StateT ModuleSRTInfo UniqSM)
       ( [CmmDecl]                      -- SRT objects we built
       , [(BlockId, CLabel)]            -- SRT fields for these blocks' itbls
       )

oneSRT dflags blockids lbls cafs = do
  srtMap <- get
  topSRT <- lift get
  let
    -- First resolve all the CAFLabels to SRTEntries
    -- implements the [Shortcut] optimisation.
    resolved =
       Set.fromList $
       catMaybes (map (resolveCAF srtMap) (Set.toList cafs))

    -- The set of all SRTEntries in SRTs that we refer to from here.
    allBelow =
      Set.unions [ lbls | caf <- Set.toList resolved
                        , Just lbls <- [Map.lookup caf (flatSRTs topSRT)] ]

    -- Remove SRTEntries that are also in an SRT that we refer to.
    -- Implements the [Filter] optimisation.
    filtered = Set.filter (not . (`Set.member` allBelow)) resolved

  srtTrace "oneSRT:"
     (ppr cafs <+> ppr resolved <+> ppr allBelow <+> ppr filtered) $ return ()

  let
    updateSRTMap srtEntry = do
      let newSRTMap = Map.fromList [(cafLbl, srtEntry) | cafLbl <- lbls]
      put (Map.union newSRTMap srtMap)

  if Set.null filtered
    then do
      srtTrace "oneSRT: empty" (ppr lbls) $ return ()
      updateSRTMap Nothing
      return ([], [])
    else do

  -- Check whether an SRT with the same entries has been emitted already.
  -- Implements the [Common] optimisation.
  case Map.lookup filtered (dedupSRTs topSRT) of
    Just srtEntry@(SRTEntry srtLbl)  -> do
      srtTrace "oneSRT [Common]" (ppr lbls <+> ppr srtLbl) $ return ()
      updateSRTMap (Just srtEntry)
      return ([], [(l, srtLbl) | l <- blockids])
    Nothing -> do
      -- No duplicates: we have to build a new SRT object
      srtTrace "oneSRT: new" (ppr lbls <+> ppr filtered) $ return ()
      (decls, srtEntry) <- lift . lift $ buildSRT dflags (Set.toList filtered)
      updateSRTMap (Just srtEntry)
      let allBelowThis = Set.union allBelow filtered
          oldFlatSRTs = flatSRTs topSRT
          newFlatSRTs
            | Set.size filtered > 1 =
              -- No point in adding an entry to flatSRTs for this
              -- label if we're going to [Shortcut] it.
              Map.insert srtEntry allBelowThis oldFlatSRTs
            | otherwise = oldFlatSRTs
          newDedupSRTs = Map.insert filtered srtEntry (dedupSRTs topSRT)
      lift (put (topSRT { dedupSRTs = newDedupSRTs
                        , flatSRTs = newFlatSRTs }))
      let SRTEntry lbl = srtEntry
      return (decls, [(l, lbl) | l <- blockids])


-- | build a static SRT object (or a chain of objects) from a list of
-- SRTEntries.
buildSRT
   :: DynFlags
   -> [SRTEntry]
   -> UniqSM
        ( [CmmDecl]    -- The SRT object(s)
        , SRTEntry     -- label of the first object in the chain
        )

buildSRT dflags cafSet = do
  case cafSet of
    [] -> panic "buildSRT: empty"

    -- Just one entry: no need to build an SRT object.
    -- Implements the [Shortcut] optimisation.
    [one] -> return ([], one)

    -- Multiple entries: build an SRT object, or a chain.  Each object
    -- in the chain can have at most mAX_SRT_SIZE entries.
    cafRefs -> do
     let
        mAX_SRT_SIZE = 16

        newSRT :: [SRTEntry] -> UniqSM (CmmDecl, SRTEntry)
        newSRT refs = do
          id <- getUniqueM
          let
            lbl = mkSRTLabel id
            srt_n_info = mkSRTInfoLabel (length refs)
            fields =
              mkStaticClosure dflags srt_n_info dontCareCCS
                [ CmmLabel lbl | SRTEntry lbl <- refs ]
                [] -- no padding
                [mkIntCLit dflags 0] -- link field
                [] -- no saved info
          return (mkDataLits (Section Data lbl) lbl fields, SRTEntry lbl)

        mkChain :: [SRTEntry] -> UniqSM ([CmmDecl], SRTEntry)
        mkChain refs = do
          let (these,those) = splitAt mAX_SRT_SIZE refs
          case those of
            [] -> do
              (decl,lbl) <- newSRT these
              return ([decl],lbl)
            _more -> do
              (rest, rest_lbl) <- mkChain (head these : those)
              (decl,lbl) <- newSRT (rest_lbl : tail these)
              return (decl:rest, lbl)

     mkChain cafRefs


{- Note [reverse gs]

   It is important to keep the code blocks in the same order,
   otherwise binary sizes get slightly bigger.  I'm not completely
   sure why this is, perhaps the assembler generates bigger jump
   instructions for forward refs.  --SDM
-}

updInfoSRTs :: BlockEnv CLabel -> CmmDecl -> CmmDecl
updInfoSRTs srt_env (CmmProc top_info top_l live g) =
  CmmProc (top_info {info_tbls = mapMapWithKey updInfoTbl (info_tbls top_info)}) top_l live g
  where updInfoTbl l info_tbl
             = info_tbl { cit_srt = mapLookup l srt_env }
updInfoSRTs _ t = t


srtTrace :: String -> SDoc -> b -> b
srtTrace _ _ b = b
