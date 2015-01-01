-- | MuCheck base module
module Test.MuCheck (mucheck) where
import Control.Monad (void)

import Test.MuCheck.Mutation
import Test.MuCheck.Config
import Test.MuCheck.Utils.Common
import Test.MuCheck.Interpreter (mutantCheckSummary)
import Test.MuCheck.TestAdapter

-- | Perform mutation analysis
mucheck :: (Summarizable a, Show a) => ([String] -> [InterpreterOutput a] -> Summary) -> String -> FilePath -> [String] -> IO ()
mucheck resFn mFn file args = do
  mutants <- genMutants mFn file >>= rSample (maxNumMutants defaultConfig)
  mapM_ (curryM writeFile) mutants
  let muts = map fst mutants
  void $ mutantCheckSummary resFn muts args ("./mucheck-" ++ mFn ++ ".log")

