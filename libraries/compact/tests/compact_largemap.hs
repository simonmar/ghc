import Data.Compact
import Data.Compact.Internal
import qualified Data.Map as Map

main = do
  c <- compact (Map.fromList [(x,show x) | x <- [1..(10000::Int)]])
  print (length (show (getCompact c)))
