{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Compact.Internal
-- Copyright   :  (c) The University of Glasgow 2001-2009
--                (c) Giovanni Campagna <gcampagn@cs.stanford.edu> 2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  unstable
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides a data structure, called a Compact, for
-- holding fully evaluated data in a consecutive block of memory.
--
-- This is a private implementation detail of the package and should
-- not be imported directly.
--
-- /Since: 1.0.0/

module Data.Compact.Internal
  ( Compact(..)
  , mkCompact
  , compactSized
  ) where

import Control.Concurrent.MVar
import Control.DeepSeq
import GHC.Prim
import GHC.Types

-- | A 'Compact' contains fully evaluated, pure, and immutable data. If
-- any object in the compact is alive, then the whole compact is
-- alive. This means that 'Compact's are very cheap to keep around,
-- because the data inside a compact does not need to be traversed by
-- the garbage collector. However, the tradeoff is that the memory
-- that contains a 'Compact' cannot be recovered until the whole 'Compact'
-- is garbage.
data Compact a = Compact Compact# a (MVar ())
    -- we can *read* from a Compact without taking a lock, but only
    -- one thread can be writing to the compact at any given time.
    -- The MVar here is to enforce mutual exclusion among writers.

mkCompact
  :: Compact# -> a -> State# RealWorld -> (# State# RealWorld, Compact a #)
mkCompact compact# a s =
  case unIO (newMVar ()) s of { (# s1, lock #) ->
  (# s1, Compact compact# a lock #) }
 where
  unIO (IO a) = a

compactSized :: NFData a => Int -> Bool -> a -> IO (Compact a)
compactSized (I# size) share a = IO $ \s0 ->
  case compactNew# (int2Word# size) s0 of { (# s1, compact# #) ->
  case compactAddPrim compact# a s1 of { (# s2, pk #) ->
  mkCompact compact# pk s2 }}
 where
  compactAddPrim
    | share = compactAddWithSharing#
    | otherwise = compactAdd#
