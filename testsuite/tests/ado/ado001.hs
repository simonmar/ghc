{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, ApplicativeDo #-}
module Main where

import Control.Applicative
import Text.PrettyPrint

test1 :: M ()
test1 = do
  a <- doc "a"
  let x = doc "x"  -- this shouldn't get in the way of grouping a/b
  b <- doc "b"
  c <- const (doc "c") b
  d <- doc "d"
  e <- doc "e"
  let y = doc "y"
  return ()

-- ((a | b) ; (c | d)) | e
test2 :: M ()
test2 = do
  x1 <- doc "a"
  x2 <- doc "b"
  let z1 = (x1,x2)
  x3 <- const (doc "c") x1
  let z2 = (x1,x2)
  x4 <- const (doc "d") z1
  x5 <- doc "e"
  return (const () (x3,x4,x5))

main = do
  run test1
  run test2

-- Testing code, prints out the structure of a monad/applicative expression

data M a = Done Doc a
         | forall b . Bind (M b) (b -> M a)
         | forall b . Two (M (b -> a)) (M b)

eval :: Bool -> M a -> (Doc, a)
eval _ (Done d a) = (d, a)
eval p (Bind m k) =
  let (d1, a) = eval True m
      (d2, b) = eval True (k a)
  in
      (maybeParen p (d1 <+> text ">>=" <+> d2), b)
eval p (Two l r) =
  let (d1, f) = eval False l
      (d2, a) = eval False r
  in
      (maybeParen p (text "<*>" $$ nest 2 (d1 $$ d2)), f a)

maybeParen True d = parens d
maybeParen _ d = d

run :: M a -> IO ()
run m = print d where (d,_) = eval False m

instance Functor M where
  fmap f m = do x <- m; return (f x)

instance Applicative M where
  pure a = Done (text "Done") a
  f <*> a = Two f a

instance Monad M where
  return = pure
  m >>= k = Bind m k

doc :: String -> M ()
doc d = Done (text d) ()
