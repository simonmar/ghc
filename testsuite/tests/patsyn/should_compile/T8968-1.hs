{-# LANGUAGE GADTs, KindSignatures, PatternSynonyms #-}
module ShouldCompile where

data X :: (* -> *) -> * -> * where
  Y :: f a -> X f (Maybe a)

pattern type C a :: X Maybe (Maybe a)
pattern C x = Y (Just x)
