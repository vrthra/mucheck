-- | MuCheck base module
module Test.MuCheck (mucheck) where

import Test.MuCheck.Mutation
import Test.MuCheck.Config
import Test.MuCheck.Utils.Common
import Test.MuCheck.Interpreter (evaluateMutants, MutantSummary(..))
import Test.MuCheck.TestAdapter
import Test.MuCheck.AnalysisSummary

-- | Perform mutation analysis
mucheck :: (Summarizable a, Show a) => (Mutant -> TestStr -> InterpreterOutput a -> Summary) -> String -> String -> [TestStr] -> IO (MAnalysisSummary, [MutantSummary])
mucheck resFn mutatingFn moduleFile tests = do
  mutants <- genMutants mutatingFn moduleFile >>= rSample (maxNumMutants defaultConfig)
  evaluateMutants resFn mutants tests

