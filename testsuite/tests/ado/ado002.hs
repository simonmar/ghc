{-# LANGUAGE ApplicativeDo #-}
module Test where

-- Test that type errors aren't affected by ApplicativeDo
f :: IO Int
f = do
  x <- getChar
  y <- getChar 'a' -- type error
  print (x,y)

g :: IO (Int,Int)
g = do
  x <- getChar
  y <- getChar
  return (y,x)
