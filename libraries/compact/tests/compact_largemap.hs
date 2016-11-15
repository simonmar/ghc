import Data.Compact
import Data.Compact.Internal
import qualified Data.Map as Map

main = do
  let m = Map.fromList [(x,show x) | x <- [1..(10000::Int)]]
  c <- compact m
  print (length (show (getCompact c)))
  print =<< compactSize c
  c <- compactWithSharing m
  print (length (show (getCompact c)))
  print =<< compactSize c
