-- | MuCheck base module
module Test.MuCheck (mucheck) where

import Test.MuCheck.Mutation
import Test.MuCheck.Config
import Test.MuCheck.Utils.Common
import Test.MuCheck.Interpreter (evaluateMutants, MutantSummary(..))
import Test.MuCheck.TestAdapter
import Test.MuCheck.AnalysisSummary

-- | Perform mutation analysis using any of the test frameworks that support
-- Summarizable (essentially, after running it on haskell, we should be able to
-- distinguish a successful run without failures from one with failures.)
-- E.g. using the mucheck-quickcheck adapter
--
-- > tFn :: Mutant -> TestStr -> InterpreterOutput QuickCheckSummary`
-- > tFn = testSummary
-- > mucheck tFn "Examples/QuickCheckTest.hs" ["quickCheckResult revProp"]

mucheck :: (Summarizable a, Show a) =>
     (Mutant -> TestStr -> InterpreterOutput a -> Summary) -- ^ The summarization function to use on test results
  -> String                                                -- ^ The module we are mutating
  -> [TestStr]                                             -- ^ The tests we can use to kill mutants
  -> IO (MAnalysisSummary, [MutantSummary])                -- ^ Returns a tuple of full summary, and individual mutant results.
mucheck resFn moduleFile tests = do
  mutants <- genMutants moduleFile >>= rSample (maxNumMutants defaultConfig)
  evaluateMutants resFn mutants tests

