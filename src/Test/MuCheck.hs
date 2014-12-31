-- | MuCheck base module
module Test.MuCheck (mucheck) where
import System.Environment (getArgs, withArgs)
import Control.Monad (void)

import Test.MuCheck.MuOp
import Test.MuCheck.Config
import Test.MuCheck.Mutation
import Test.MuCheck.Operators
import Test.MuCheck.Utils.Common
import Test.MuCheck.Utils.Print
import Test.MuCheck.Interpreter (mutantCheckSummary)
import Test.MuCheck.TestAdapter

-- | Perform mutation analysis
mucheck :: (Summarizable a, Show a) => ([String] -> [InterpreterOutput a] -> Summary) -> String -> FilePath -> String -> [String] -> IO ()
mucheck resFn mFn file modulename args = do
  numMutants <- genMutants mFn file
  let muts = take numMutants $ genFileNames file
  void $ mutantCheckSummary resFn muts modulename args ("./mucheck-" ++ mFn ++ ".log")

