{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-name-shadowing #-}

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
  compactAdd,
  getCompact,
  inCompact,
  isCompact,

  newCompact,
  appendCompact,
  compactResize,
  ) where

import Control.Concurrent
import Control.DeepSeq (NFData, force)
import GHC.Prim
import GHC.Types

import Data.Compact.Internal

-- |Retrieve the object that was stored in a Compact
getCompact :: Compact a -> a
getCompact (Compact _ obj _) = obj

compactAppendInternal
  :: NFData a => Compact# -> a -> Int#
  -> State# RealWorld
  -> (# State# RealWorld, Compact# , a #)
compactAppendInternal buffer root share s =
  case force root of { !eval ->
  case compactAppend# buffer eval share s of { (# s', adjustedRoot #) ->
  (# s', buffer, adjustedRoot #) }}

compactAppendInternalIO :: NFData a => Int# -> Compact b -> a -> IO (Compact a)
compactAppendInternalIO share (Compact buffer _ lock) root =
  withMVar lock $ \_ -> IO $ \s ->
  case compactAppendInternal buffer root share s of { (# s1, buffer', a #) ->
  (# s1, Compact buffer' a lock #) }

-- |Append a value to a 'Compact', and return a new 'Compact'
-- that shares the same buffer but a different root object.
appendCompact :: NFData a => Compact b -> a -> IO (Compact a)
appendCompact = compactAppendInternalIO 1#

compactNewInternal :: NFData a => Int# -> Word -> a -> IO (Compact a)
compactNewInternal share (W# size) root = IO $ \s ->
  case compactNew# size s of { (# s1, buffer #) ->
  case compactAppendInternal buffer root share s1 of { (# s2, buffer1, a #) ->
  mkCompact buffer1 a s2 }}

-- |Create a new 'Compact', with the provided value as suggested block
-- size (which will be adjusted if unsuitable), and append the given
-- value to it, as if calling 'appendCompact'
newCompact :: NFData a => Word -> a -> IO (Compact a)
newCompact = compactNewInternal 1#

-- | Compact a value. /O(size of unshared data)/
--
-- If the structure contains any internal sharing, the shared data
-- will be duplicated during the compaction process.  Loops if the
-- structure constains cycles.
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
  mkCompact compact# pk s2 }}

-- |Add a value to an existing 'Compact'.  Behaves exactly like
-- 'compact' with respect to sharing and the 'NFData' constraint.
compactAdd :: NFData a => Compact b -> a -> IO (Compact a)
compactAdd (Compact compact# _ lock) a = withMVar lock $ \_ -> IO $ \s ->
  case compactAdd# compact# a s of { (# s1, pk #) ->
  (# s1, Compact compact# pk lock #) }


-- |Check if the second argument is inside the Compact
inCompact :: Compact b -> a -> IO Bool
inCompact (Compact buffer _ _) !val =
  IO (\s -> case compactContains# buffer val s of
         (# s', v #) -> (# s', isTrue# v #) )

-- |Check if the argument is in any Compact
isCompact :: a -> IO Bool
isCompact !val =
  IO (\s -> case compactContainsAny# val s of
         (# s', v #) -> (# s', isTrue# v #) )

compactResize :: Compact a -> Word -> IO ()
compactResize (Compact oldBuffer _ lock) (W# new_size) =
  withMVar lock $ \_ -> IO $ \s ->
    case compactResize# oldBuffer new_size s of
      s' -> (# s', () #)

