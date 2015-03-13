{-# LANGUAGE ApplicativeDo, RebindableSyntax #-}
{-# OPTIONS_GHC -ddump-types #-}
module Test where

import Control.Monad
import Prelude

-- This is a do expression that typechecks with only an Applicative constraint
test1 :: Applicative f => (Int -> f Int) -> f Int
test1 f = do
  x <- f 3
  y <- f 4
  return (x + y)

-- Test we can also infer the Applicative version of the type
test2 f = do
  x <- f 3
  y <- f 4
  return (x + y)

-- This one will use join
test3 f g = do
  x <- f 3
  y <- f 4
  g y x

-- This one needs a tuple
test4 f g = do
  x <- f 3
  y <- f 4
  let r = g y x
  r
