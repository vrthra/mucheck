-- | MuCheck base module
module Test.MuCheck (mucheck) where
import Control.Monad (void)

import Test.MuCheck.Mutation
import Test.MuCheck.Utils.Common
import Test.MuCheck.Interpreter (mutantCheckSummary)
import Test.MuCheck.TestAdapter

-- | Perform mutation analysis
mucheck :: (Summarizable a, Show a) => ([String] -> [InterpreterOutput a] -> Summary) -> String -> FilePath -> String -> [String] -> IO ()
mucheck resFn mFn file modulename args = do
  numMutants <- genMutants mFn file
  let muts = take numMutants $ genFileNames file
  void $ mutantCheckSummary resFn muts modulename args ("./mucheck-" ++ mFn ++ ".log")

