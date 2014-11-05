%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcPatSyn]{Typechecking pattern synonym declarations}

\begin{code}
{-# LANGUAGE CPP #-}

module TcPatSyn (tcInferPatSynDecl, tcCheckPatSynDecl, tcPatSynWrapper) where

import HsSyn
import TcPat
import TcRnMonad
import TcEnv
import TcMType
import TysPrim
import Name
import SrcLoc
import PatSyn
import NameSet
import Panic
import Outputable
import FastString
import Var
import Id
import IdInfo( IdDetails( VanillaId ) )
import TcBinds
import BasicTypes
import TcSimplify
import TcUnify
import TcType
import TcEvidence
import BuildTyCl
import VarSet
import VarEnv
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid
#endif
import Bag
import Util
import TypeRep
import Control.Monad (forM, forM_)

#include "HsVersions.h"
\end{code}

\begin{code}
tcInferPatSynDecl :: PatSynBind Name Name
                  -> TcM (PatSyn, LHsBinds Id)
tcInferPatSynDecl PSB{ psb_id = lname@(L _ name), psb_args = details,
                       psb_def = lpat, psb_dir = dir }
  = do { tcCheckPatSynPat lpat
       ; traceTc "tcInferPatSynDecl {" $ ppr name
       ; pat_ty <- newFlexiTyVarTy openTypeKind

       ; let (arg_names, is_infix) = case details of
                 PrefixPatSyn names      -> (map unLoc names, False)
                 InfixPatSyn name1 name2 -> (map unLoc [name1, name2], True)
       ; ((lpat', args), wanted) <- captureConstraints       $
                                    tcPat PatSyn lpat pat_ty $
                                    mapM tcLookupId arg_names
       ; let named_taus = (name, pat_ty):map (\arg -> (getName arg, varType arg)) args

       ; (qtvs, req_dicts, _mr_bites, ev_binds) <- simplifyInfer True False named_taus wanted

       ; (ex_vars, prov_dicts) <- tcCollectEx lpat'
       ; let univ_tvs   = filter (not . (`elemVarSet` ex_vars)) qtvs
             ex_tvs     = varSetElems ex_vars
             prov_theta = map evVarPred prov_dicts
             req_theta  = map evVarPred req_dicts

       ; univ_tvs   <- mapM zonkQuantifiedTyVar univ_tvs
       ; ex_tvs     <- mapM zonkQuantifiedTyVar ex_tvs

       ; prov_theta <- zonkTcThetaType prov_theta
       ; req_theta  <- zonkTcThetaType req_theta

       ; pat_ty     <- zonkTcType pat_ty
       ; args       <- mapM zonkId args
       ; let wrapped_args = zip args $ repeat idHsWrapper

       ; traceTc "tcInferPatSynDecl }" $ ppr name
       ; tc_patsyn_finish lname dir is_infix lpat'
                          (univ_tvs, req_theta, ev_binds, req_dicts)
                          (ex_tvs, prov_theta, emptyTcEvBinds, prov_dicts)
                          (zip args $ repeat idHsWrapper)
                          pat_ty }

tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (PatSyn, LHsBinds Id)
tcCheckPatSynDecl PSB{ psb_id = lname@(L loc name), psb_args = details,
                       psb_def = lpat, psb_dir = dir }
                  TPSI{ patsig_tau = tau,
                        patsig_ex = ex_tvs, patsig_univ = univ_tvs,
                        patsig_prov = prov_theta, patsig_req = req_theta }
  = setSrcSpan loc $
    do { tcCheckPatSynPat lpat

       ; traceTc "tcCheckPatSynDecl {" $
         ppr name $$
         ppr (ex_tvs, prov_theta) $$
         ppr (univ_tvs, req_theta) $$
         ppr tau

       ; req_dicts <- newEvVars req_theta

       ; let skol_info = SigSkol (FunSigCtxt name) (mkFunTys arg_tys pat_ty)

       ; let (arg_names, is_infix) = case details of
                 PrefixPatSyn names      -> (map unLoc names, False)
                 InfixPatSyn name1 name2 -> (map unLoc [name1, name2], True)

       ; (req_ev_binds, (lpat', wrapped_args)) <-
           checkConstraints skol_info univ_tvs req_dicts $
           tcPat PatSyn lpat pat_ty $ do
           { ex_sigtvs <- mapM (\tv -> newSigTyVar (getName tv) (tyVarKind tv)) ex_tvs
           ; let subst = mkTvSubst (mkInScopeSet (zipVarEnv ex_sigtvs ex_sigtvs)) $
                         zipTyEnv ex_tvs (map mkTyVarTy ex_sigtvs)
                 arg_tys' = substTys subst arg_tys
           ; forM (zipEqual "tcCheckPatSynDecl" arg_names arg_tys') $ \(arg_name, arg_ty) -> do
               { arg <- tcLookupId arg_name
               ; coi <- unifyType (varType arg) arg_ty
               ; return (setVarType arg arg_ty, coToHsWrapper coi) }}

       ; (ex_vars_rhs, prov_dicts_rhs) <- tcCollectEx lpat'
       ; let ex_tvs_rhs  = varSetElems ex_vars_rhs

       ; (prov_ev_binds, prov_dicts) <-
           checkConstraints skol_info ex_tvs_rhs prov_dicts_rhs $ do
           { ctLoc <- getCtLoc PatSigOrigin
           ; forM prov_theta $ \pred -> do
               { evar <- newEvVar pred
               ; let ctEv = CtWanted{ ctev_pred = pred
                                    , ctev_evar = evar
                                    , ctev_loc = ctLoc
                                    }
               ; emitFlat $ mkNonCanonical ctEv
               ; return evar }}

       ; traceTc "tcCheckPatSynDecl }" $ ppr name
       ; tc_patsyn_finish lname dir is_infix lpat'
                          (univ_tvs, req_theta, req_ev_binds, req_dicts)
                          (ex_tvs, prov_theta, prov_ev_binds, prov_dicts)
                          wrapped_args
                          pat_ty }
  where
    (arg_tys, pat_ty) = tcSplitFunTys tau

tc_patsyn_finish :: Located Name
                 -> HsPatSynDir Name
                 -> Bool
                 -> LPat Id
                 -> ([TcTyVar], [PredType], TcEvBinds, [EvVar])
                 -> ([TcTyVar], [PredType], TcEvBinds, [EvVar])
                 -> [(Var, HsWrapper)]
                 -> TcType
                 -> TcM (PatSyn, LHsBinds Id)
tc_patsyn_finish lname dir is_infix lpat'
                 (univ_tvs, req_theta, req_ev_binds, req_dicts)
                 (ex_tvs, prov_theta, prov_ev_binds, prov_dicts)
                 wrapped_args
                 pat_ty
  = do { (matcher_id, matcher_bind) <- tcPatSynMatcher lname lpat'
                                         (univ_tvs, req_theta, req_ev_binds, req_dicts)
                                         (ex_tvs, prov_theta, prov_ev_binds, prov_dicts)
                                         wrapped_args
                                         pat_ty

       ; wrapper_id <- if isBidirectional dir
                       then fmap Just $ mkPatSynWrapperId lname (map varType args) univ_tvs ex_tvs theta pat_ty
                       else return Nothing

       ; let patSyn = mkPatSyn (unLoc lname) is_infix
                        (univ_tvs, req_theta)
                        (ex_tvs, prov_theta)
                        (map varType args)
                        pat_ty
                        matcher_id wrapper_id
       ; return (patSyn, matcher_bind) }
  where
    theta = prov_theta ++ req_theta
    args = map fst wrapped_args
\end{code}


\begin{code}
tcPatSynMatcher :: Located Name
                -> LPat Id
                -> ([TcTyVar], ThetaType, TcEvBinds, [EvVar])
                -> ([TcTyVar], ThetaType, TcEvBinds, [EvVar])
                -> [(Var, HsWrapper)]
                -> TcType
                -> TcM (Id, LHsBinds Id)
-- See Note [Matchers and wrappers for pattern synonyms] in PatSyn
tcPatSynMatcher (L loc name) lpat
                (univ_tvs, req_theta, req_ev_binds, req_dicts)
                (ex_tvs, prov_theta, prov_ev_binds, prov_dicts)
                arg_w_wraps pat_ty
  = do { res_tv <- zonkQuantifiedTyVar =<< newFlexiTyVar liftedTypeKind
       ; matcher_name <- newImplicitBinder name mkMatcherOcc
       ; let res_ty = TyVarTy res_tv
             cont_ty = mkSigmaTy ex_tvs prov_theta $
                       mkFunTys (map varType args) res_ty

       ; let matcher_tau = mkFunTys [pat_ty, cont_ty, res_ty] res_ty
             matcher_sigma = mkSigmaTy (res_tv:univ_tvs) req_theta matcher_tau
             matcher_id = mkExportedLocalId VanillaId matcher_name matcher_sigma

       ; traceTc "tcPatSynMatcher" (ppr name $$ ppr (idType matcher_id))
       ; let matcher_lid = L loc matcher_id

       ; scrutinee <- mkId "scrut" pat_ty
       ; cont <- mkId "cont" cont_ty
       ; let cont' = nlHsApps cont $ map nlHsVar (ex_tvs ++ prov_dicts) ++ [mkLHsWrap wrap $ nlHsVar arg | (arg, wrap) <- arg_w_wraps]
       ; cont' <- return $ mkLHsWrap (mkWpLet prov_ev_binds) cont'
       ; fail <- mkId "fail" res_ty
       ; let fail' = nlHsVar fail


       ; let args = map nlVarPat [scrutinee, cont, fail]
             lwpat = noLoc $ WildPat pat_ty
             cases = if isIrrefutableHsPat lpat
                     then [mkSimpleHsAlt lpat  cont']
                     else [mkSimpleHsAlt lpat  cont',
                           mkSimpleHsAlt lwpat fail']
             body = mkLHsWrap (mkWpLet req_ev_binds) $
                    L (getLoc lpat) $
                    HsCase (nlHsVar scrutinee) $
                    MG{ mg_alts = cases
                      , mg_arg_tys = [pat_ty]
                      , mg_res_ty = res_ty
                      , mg_origin = Generated
                      }
             body' = noLoc $
                     HsLam $
                     MG{ mg_alts = [mkSimpleMatch args body]
                       , mg_arg_tys = [pat_ty, cont_ty, res_ty]
                       , mg_res_ty = res_ty
                       , mg_origin = Generated
                       }

             match = mkMatch [] (mkHsLams (res_tv:univ_tvs) req_dicts body') EmptyLocalBinds
             mg = MG{ mg_alts = [match]
                    , mg_arg_tys = []
                    , mg_res_ty = res_ty
                    , mg_origin = Generated
                    }

       ; let bind = FunBind{ fun_id = matcher_lid
                           , fun_infix = False
                           , fun_matches = mg
                           , fun_co_fn = idHsWrapper
                           , bind_fvs = emptyNameSet
                           , fun_tick = Nothing }
             matcher_bind = unitBag (noLoc bind)

       ; traceTc "tcPatSynMatcher" (ppr matcher_bind)

       ; return (matcher_id, matcher_bind) }
  where
    mkId s ty = do
        name <- newName . mkVarOccFS . fsLit $ s
        return $ mkLocalId name ty
    args = map fst arg_w_wraps

isBidirectional :: HsPatSynDir a -> Bool
isBidirectional Unidirectional = False
isBidirectional ImplicitBidirectional = True
isBidirectional ExplicitBidirectional{} = True

tcPatSynWrapper :: PatSynBind Name Name
                -> TcM (LHsBinds Id)
-- See Note [Matchers and wrappers for pattern synonyms] in PatSyn
tcPatSynWrapper PSB{ psb_id = L loc name, psb_def = lpat, psb_dir = dir, psb_args = details }
  = case dir of
    Unidirectional -> return emptyBag
    ImplicitBidirectional ->
        do { wrapper_id <- tcLookupPatSynWrapper name
           ; lexpr <- case tcPatToExpr (mkNameSet args) lpat of
                  Nothing -> cannotInvertPatSynErr lpat
                  Just lexpr -> return lexpr
           ; let wrapper_args = map (noLoc . VarPat) args
                 wrapper_lname = L (getLoc lpat) (idName wrapper_id)
                 wrapper_match = mkMatch wrapper_args lexpr EmptyLocalBinds
                 wrapper_bind = mkTopFunBind Generated wrapper_lname [wrapper_match]
           ; mkPatSynWrapper wrapper_id wrapper_bind }
    ExplicitBidirectional mg ->
        do { wrapper_id <- tcLookupPatSynWrapper name
           ; mkPatSynWrapper wrapper_id $
               FunBind{ fun_id = L loc (idName wrapper_id)
                      , fun_infix = False
                      , fun_matches = mg
                      , fun_co_fn = idHsWrapper
                      , bind_fvs = placeHolderNamesTc
                      , fun_tick = Nothing }}
  where
    args = map unLoc $ case details of
        PrefixPatSyn args -> args
        InfixPatSyn arg1 arg2 -> [arg1, arg2]

    tcLookupPatSynWrapper name
      = do { patsyn <- tcLookupPatSyn name
           ; case patSynWrapper patsyn of
               Nothing -> panic "tcLookupPatSynWrapper"
               Just wrapper_id -> return wrapper_id }

mkPatSynWrapperId :: Located Name
                  -> [Type] -> [TyVar] -> [TyVar] -> ThetaType -> Type
                  -> TcM Id
mkPatSynWrapperId (L _ name) arg_tys univ_tvs ex_tvs theta pat_ty
  = do { let qtvs = univ_tvs ++ ex_tvs
       ; (subst, wrapper_tvs) <- tcInstSkolTyVars qtvs
       ; let wrapper_theta = substTheta subst theta
             pat_ty' = substTy subst pat_ty
             arg_tys' = map (substTy subst) arg_tys
             wrapper_tau = mkFunTys arg_tys' pat_ty'
             wrapper_sigma = mkSigmaTy wrapper_tvs wrapper_theta wrapper_tau

       ; wrapper_name <- newImplicitBinder name mkDataConWrapperOcc
       ; traceTc "mkPatSynWrapperId" (ppr wrapper_sigma)
       ; return $ mkExportedLocalId VanillaId wrapper_name wrapper_sigma }

mkPatSynWrapper :: Id
                -> HsBind Name
                -> TcM (LHsBinds Id)
mkPatSynWrapper wrapper_id bind
  = do { (wrapper_binds, _, _) <- tcPolyCheck NonRecursive (const []) sig (noLoc bind)
       ; traceTc "tcPatSynDecl wrapper" $ ppr wrapper_binds
       ; traceTc "tcPatSynDecl wrapper type" $ ppr (varType wrapper_id)
       ; return wrapper_binds }
  where
    sig = TcSigInfo{ sig_id = wrapper_id
                   , sig_tvs = map (\tv -> (Nothing, tv)) wrapper_tvs
                   , sig_theta = wrapper_theta
                   , sig_tau = wrapper_tau
                   , sig_loc = noSrcSpan
                   }
    (wrapper_tvs, wrapper_theta, wrapper_tau) = tcSplitSigmaTy (idType wrapper_id)

\end{code}

Note [As-patterns in pattern synonym definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The rationale for rejecting as-patterns in pattern synonym definitions
is that an as-pattern would introduce nonindependent pattern synonym
arguments, e.g. given a pattern synonym like:

        pattern K x y = x@(Just y)

one could write a nonsensical function like

        f (K Nothing x) = ...

or
        g (K (Just True) False) = ...

\begin{code}
tcCheckPatSynPat :: LPat Name -> TcM ()
tcCheckPatSynPat = go
  where
    go :: LPat Name -> TcM ()
    go = addLocM go1

    go1 :: Pat Name -> TcM ()
    go1   (ConPatIn _ info)   = mapM_ go (hsConPatArgs info)
    go1   VarPat{}            = return ()
    go1   WildPat{}           = return ()
    go1 p@(AsPat _ _)         = asPatInPatSynErr p
    go1   (LazyPat pat)       = go pat
    go1   (ParPat pat)        = go pat
    go1   (BangPat pat)       = go pat
    go1   (PArrPat pats _)    = mapM_ go pats
    go1   (ListPat pats _ _)  = mapM_ go pats
    go1   (TuplePat pats _ _) = mapM_ go pats
    go1   LitPat{}            = return ()
    go1   NPat{}              = return ()
    go1   (SigPatIn pat _)    = go pat
    go1   (ViewPat _ pat _)   = go pat
    go1 p@SplicePat{}         = thInPatSynErr p
    go1 p@QuasiQuotePat{}     = thInPatSynErr p
    go1 p@NPlusKPat{}         = nPlusKPatInPatSynErr p
    go1   ConPatOut{}         = panic "ConPatOut in output of renamer"
    go1   SigPatOut{}         = panic "SigPatOut in output of renamer"
    go1   CoPat{}             = panic "CoPat in output of renamer"

asPatInPatSynErr :: OutputableBndr name => Pat name -> TcM a
asPatInPatSynErr pat
  = failWithTc $
    hang (ptext (sLit "Pattern synonym definition cannot contain as-patterns (@):"))
       2 (ppr pat)

thInPatSynErr :: OutputableBndr name => Pat name -> TcM a
thInPatSynErr pat
  = failWithTc $
    hang (ptext (sLit "Pattern synonym definition cannot contain Template Haskell:"))
       2 (ppr pat)

nPlusKPatInPatSynErr :: OutputableBndr name => Pat name -> TcM a
nPlusKPatInPatSynErr pat
  = failWithTc $
    hang (ptext (sLit "Pattern synonym definition cannot contain n+k-pattern:"))
       2 (ppr pat)

tcPatToExpr :: NameSet -> LPat Name -> Maybe (LHsExpr Name)
tcPatToExpr lhsVars = go
  where
    go :: LPat Name -> Maybe (LHsExpr Name)
    go (L loc (ConPatIn conName info))
      = do
          { let con = L loc (HsVar (unLoc conName))
          ; exprs <- mapM go (hsConPatArgs info)
          ; return $ foldl (\x y -> L loc (HsApp x y)) con exprs }
    go (L loc p) = fmap (L loc) $ go1 p

    go1 :: Pat Name -> Maybe (HsExpr Name)
    go1   (VarPat var)
      | var `elemNameSet` lhsVars  = return $ HsVar var
      | otherwise                  = Nothing
    go1   (LazyPat pat)            = fmap HsPar $ go pat
    go1   (ParPat pat)             = fmap HsPar $ go pat
    go1   (BangPat pat)            = fmap HsPar $ go pat
    go1   (PArrPat pats ptt)
      = do { exprs <- mapM go pats
           ; return $ ExplicitPArr ptt exprs }
    go1   (ListPat pats ptt reb)
      = do { exprs <- mapM go pats
           ; return $ ExplicitList ptt (fmap snd reb) exprs }
    go1   (TuplePat pats box _)
      = do { exprs <- mapM go pats
           ; return (ExplicitTuple (map Present exprs) box)
           }
    go1   (LitPat lit)             = return $ HsLit lit
    go1   (NPat n Nothing _)       = return $ HsOverLit n
    go1   (NPat n (Just neg) _)    = return $ noLoc neg `HsApp` noLoc (HsOverLit n)
    go1   (SigPatIn pat (HsWB ty _ _))
      = do { expr <- go pat
           ; return $ ExprWithTySig expr ty }
    go1   (ConPatOut{})            = panic "ConPatOut in output of renamer"
    go1   (SigPatOut{})            = panic "SigPatOut in output of renamer"
    go1   (CoPat{})                = panic "CoPat in output of renamer"
    go1   _                        = Nothing

cannotInvertPatSynErr :: OutputableBndr name => LPat name -> TcM a
cannotInvertPatSynErr (L loc pat)
  = setSrcSpan loc $ failWithTc $
    hang (ptext (sLit "Right-hand side of bidirectional pattern synonym cannot be used as an expression"))
       2 (ppr pat)

-- Walk the whole pattern and for all ConPatOuts, collect the
-- existentially-bound type variables and evidence binding variables.
--
-- These are used in computing the type of a pattern synonym and also
-- in generating matcher functions, since success continuations need
-- to be passed these pattern-bound evidences.
tcCollectEx :: LPat Id -> TcM (TyVarSet, [EvVar])
tcCollectEx = return . go
  where
    go :: LPat Id -> (TyVarSet, [EvVar])
    go = go1 . unLoc

    go1 :: Pat Id -> (TyVarSet, [EvVar])
    go1 (LazyPat p)         = go p
    go1 (AsPat _ p)         = go p
    go1 (ParPat p)          = go p
    go1 (BangPat p)         = go p
    go1 (ListPat ps _ _)    = mconcat . map go $ ps
    go1 (TuplePat ps _ _)   = mconcat . map go $ ps
    go1 (PArrPat ps _)      = mconcat . map go $ ps
    go1 (ViewPat _ p _)     = go p
    go1 (QuasiQuotePat qq)  = pprPanic "TODO: tcInstPatSyn QuasiQuotePat" $ ppr qq
    go1 con@ConPatOut{}     = mappend (mkVarSet (pat_tvs con), pat_dicts con) $
                                 goConDetails $ pat_args con
    go1 (SigPatOut p _)     = go p
    go1 (CoPat _ p _)       = go1 p
    go1 (NPlusKPat n k geq subtract)
      = pprPanic "TODO: NPlusKPat" $ ppr n $$ ppr k $$ ppr geq $$ ppr subtract
    go1 _                   = mempty

    goConDetails :: HsConPatDetails Id -> (TyVarSet, [EvVar])
    goConDetails (PrefixCon ps) = mconcat . map go $ ps
    goConDetails (InfixCon p1 p2) = go p1 `mappend` go p2
    goConDetails (RecCon HsRecFields{ rec_flds = flds })
      = mconcat . map goRecFd $ flds

    goRecFd :: HsRecField Id (LPat Id) -> (TyVarSet, [EvVar])
    goRecFd HsRecField{ hsRecFieldArg = p } = go p

\end{code}
