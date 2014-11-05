{-# LANGUAGE PatternSynonyms #-}
module ShouldCompile where

pattern type (Eq a) => Single a :: (Show a) => [a]
-- pattern type Single a :: (Eq a, Show a) => [a]
-- pattern type Single a :: [a]
pattern Single x = [x]

-- f :: (Show a) => [a] -> a
foobar (Single x) = x
