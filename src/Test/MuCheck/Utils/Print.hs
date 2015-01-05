{-# LANGUAGE TupleSections #-}
-- | Common print utilities
module Test.MuCheck.Utils.Print where
import Debug.Trace
import Data.List(intercalate)
import Control.Monad (liftM)

import GHC.IO.Handle
import System.IO
import System.Directory
import System.Environment
import System.IO.Temp (withSystemTempFile)

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
catchOutputStr :: IO a -> IO (a,String)
catchOutputStr f = do
  isdebug <- lookupEnv "MuDEBUG"
  case isdebug of 
    Just _ -> liftM (,"") f
    Nothing -> withSystemTempFile "_mucheck" $ \tmpf tmph -> do
        res <- redirectToHandle f tmph
        str <- readFile tmpf
        removeFile tmpf
        return (res,str)

-- | Capture output and err of an IO action to a file
catchOutput :: String -> IO a -> IO a
catchOutput fn f = withFile fn WriteMode (redirectToHandle f)

-- | Redirect out and err to handle
redirectToHandle :: IO b -> Handle -> IO b
redirectToHandle f tmph = do
    stdout_dup <- hDuplicate stdout
    stderr_dup <- hDuplicate stderr
    hDuplicateTo tmph stdout
    hDuplicateTo tmph stderr
    hClose tmph
    res <- f
    hDuplicateTo stdout_dup stdout
    hDuplicateTo stderr_dup stderr
    return res

