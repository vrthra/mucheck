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
-- > mucheck tFn "qsort" "Examples/QuickCheckTest.hs" ["quickCheckResult revProp"]

mucheck :: (Summarizable a, Show a) =>
     (Mutant -> TestStr -> InterpreterOutput a -> Summary) -- ^ The summarization function to use on test results
  -> String                                                -- ^ The mutating function we are checking the test adequacy of.
  -> String                                                -- ^ The module file where the mutating function was declared
  -> [TestStr]                                             -- ^ The tests we can use to kill mutants
  -> IO (MAnalysisSummary, [MutantSummary])                -- ^ Returns a tuple of full summary, and individual mutant results.
mucheck resFn mutatingFn moduleFile tests = do
  mutants <- genMutants mutatingFn moduleFile >>= rSample (maxNumMutants defaultConfig)
  evaluateMutants resFn mutants tests

