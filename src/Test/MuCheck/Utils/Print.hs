-- | Common print utilities
module Test.MuCheck.Utils.Print where
import Debug.Trace
import Data.List(intercalate)

import GHC.IO.Handle
import System.IO
import System.Directory

-- | simple wrapper for adding a % at the end.
(./.) :: (Show a, Integral a) => a -> a -> String
n ./. t =  "(" ++ show (n * 100 `div` t) ++ "%)"

-- | join lines together
showAS :: [String] -> String
showAS = intercalate "\n"

-- | make lists into lines in text.
showA :: Show a => [a] -> String
showA =  showAS . map show

-- | convenience function for debug
tt :: Show a => a -> a
tt v = trace (">" ++ show v) v

-- | Capture output and err of an IO action
catchOutput :: IO a -> IO (a,String)
catchOutput f = do
  tmpd <- getTemporaryDirectory
  (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
  stdout_dup <- hDuplicate stdout
  stderr_dup <- hDuplicate stderr
  hDuplicateTo tmph stdout
  hDuplicateTo tmph stderr
  hClose tmph
  res <- f
  hDuplicateTo stdout_dup stdout
  hDuplicateTo stderr_dup stderr
  str <- readFile tmpf
  removeFile tmpf
  return (res,str)
