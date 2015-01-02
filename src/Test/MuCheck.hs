-- | MuCheck base module
module Test.MuCheck (mucheck) where
import Control.Monad (void)

import Test.MuCheck.Mutation
import Test.MuCheck.Config
import Test.MuCheck.Utils.Common
import Test.MuCheck.Interpreter (evaluateMutants)
import Test.MuCheck.TestAdapter

-- | Perform mutation analysis
mucheck :: (Summarizable a, Show a) => ([Mutant] -> [InterpreterOutput a] -> Summary) -> String -> FilePath -> [String] -> IO ()
mucheck resFn mFn file args = do
  mutants <- genMutants mFn file >>= rSample (maxNumMutants defaultConfig)
  void $ evaluateMutants resFn mutants args ("./mucheck-" ++ mFn ++ ".log")

