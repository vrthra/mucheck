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

mucheck :: (Show b, Summarizable b, TRun a b) =>
     a                                                     -- ^ The module we are mutating
  -> IO (MAnalysisSummary, [MutantSummary])                -- ^ Returns a tuple of full summary, and individual mutant results.
mucheck moduleFile = do
  -- get tix here.
  mutants <- genMutants (getName moduleFile) >>= rSample (maxNumMutants defaultConfig)
  tests <- getAllTests (getName moduleFile)
  evaluateMutants moduleFile mutants (map (genTest moduleFile) tests)

