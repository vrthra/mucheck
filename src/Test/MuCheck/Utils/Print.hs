{-# LANGUAGE TupleSections #-}
-- | Common print utilities
module Test.MuCheck.Utils.Print where
import Debug.Trace
import Data.List(intercalate)
import Control.Monad (liftM)

import GHC.IO.Handle
import System.IO
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
catchOutput :: IO a -> IO (a,String)
catchOutput f = do
  isdebug <- lookupEnv "MuDEBUG"
  case isdebug of 
    Just _ -> liftM (,"") f
    Nothing -> withSystemTempFile "_mucheck" $ \tmpf tmph -> do
        stdout_dup <- hDuplicate stdout
        stderr_dup <- hDuplicate stderr
        hDuplicateTo tmph stdout
        hDuplicateTo tmph stderr
        hClose tmph
        res <- f
        hDuplicateTo stdout_dup stdout
        hDuplicateTo stderr_dup stderr
        str <- readFile tmpf
        return (res,str)

