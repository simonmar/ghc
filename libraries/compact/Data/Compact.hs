{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Compact
-- Copyright   :  (c) The University of Glasgow 2001-2009
--                (c) Giovanni Campagna <gcampagn@cs.stanford.edu> 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  unstable
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides a data structure, called a Compact, for
-- holding fully evaluated data in a consecutive block of memory.
--
-- /Since: 1.0.0/
module Data.Compact (
  Compact,
  compact,
  getCompact,
  inCompact,
  isCompact,

  newCompact,
  appendCompact,
  appendCompactNoShare,
  ) where

-- Write down all GHC.Prim deps explicitly to keep them at minimum
import GHC.Prim (Compact#,
                 compactNew#,
                 compactAdd#,
                 State#,
                 RealWorld,
                 Int#
                 )
-- We need to import Word from GHC.Types to see the representation
-- and to able to access the Word# to pass down the primops
import GHC.Types (IO(..), Word(..))

import Control.DeepSeq (NFData, force)

import Data.Compact.Internal(Compact(..),
                             isCompact,
                             inCompact,
                             compactAppendEvaledInternal)

-- |Retrieve the object that was stored in a Compact
getCompact :: Compact a -> a
getCompact (Compact _ obj) = obj

compactAppendInternal :: NFData a => Compact# -> a -> Int# ->
                         State# RealWorld -> (# State# RealWorld, Compact a #)
compactAppendInternal buffer root share s =
  case force root of
    !eval -> compactAppendEvaledInternal buffer eval share s

compactAppendInternalIO :: NFData a => Int# -> Compact b -> a -> IO (Compact a)
compactAppendInternalIO share (Compact buffer _) root =
  IO (\s -> compactAppendInternal buffer root share s)

-- |Append a value to a 'Compact', and return a new 'Compact'
-- that shares the same buffer but a different root object.
appendCompact :: NFData a => Compact b -> a -> IO (Compact a)
appendCompact = compactAppendInternalIO 1#

-- |Append a value to a 'Compact'. This function differs from
-- 'appendCompact' in that it will not preserve internal sharing
-- in the passed in value (and it will diverge on cyclic structures).
appendCompactNoShare :: NFData a => Compact b -> a -> IO (Compact a)
appendCompactNoShare = compactAppendInternalIO 0#

compactNewInternal :: NFData a => Int# -> Word -> a -> IO (Compact a)
compactNewInternal share (W# size) root =
  IO (\s -> case compactNew# size s of
         (# s', buffer #) -> compactAppendInternal buffer root share s' )

-- |Create a new 'Compact', with the provided value as suggested block
-- size (which will be adjusted if unsuitable), and append the given
-- value to it, as if calling 'appendCompact'
newCompact :: NFData a => Word -> a -> IO (Compact a)
newCompact = compactNewInternal 1#

-- | Compact a value.  If the structure contains any internal sharing,
-- the shared data will be duplicated during the compaction process.
-- Loops if the structure constains cycles. /O(size of unshared data)/.
--
-- The NFData constraint is just to ensure that the object contains no
-- functions, we do not actually use it.  NB. If you use any custom
-- NFData instances that leave any parts of the structure unevaluated,
-- then 'compact' will likely fail and terminate the program. (TODO:
-- should be an exception).
--
compact :: NFData a => a -> IO (Compact a)
compact a = IO $ \s0 ->
  case compactNew# 31268## s0 of { (# s1, compact# #) ->
  case compactAdd# compact# a s1 of { (# s2, pk #) ->
  (# s2, Compact compact# pk #)
  }}
