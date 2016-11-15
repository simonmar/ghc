import Control.Exception
import Data.Compact
import Data.Compact.Internal
import qualified Data.Map as Map
import System.Exit

main = do
  res <- try $ compact (cycle "abc")
  case res of
    Left StackOverflow{} -> return ()
    _other -> die "wrong exception"
  c <- compactWithSharing (cycle "abc") -- magic!
  print (length (show (take 100 (getCompact c))))
  print =<< compactSize c
