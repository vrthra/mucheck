-- | MuCheck base module
module Test.MuCheck (mucheck,
                     checkQuickCheckOnMutants,
                     checkHspecOnMutants,
                     checkHUnitOnMutants) where
import System.Environment (getArgs, withArgs)
import Control.Monad (void)

import Test.MuCheck.MuOp
import Test.MuCheck.Config
import Test.MuCheck.Mutation
import Test.MuCheck.Operators
import Test.MuCheck.Utils.Common
import Test.MuCheck.Utils.Print
import Test.MuCheck.Interpreter (mutantCheckSummary)
import Test.MuCheck.Run.QuickCheck
import Test.MuCheck.Run.SmallCheck
import Test.MuCheck.Run.HUnit
import Test.MuCheck.Run.Hspec

import qualified Test.SmallCheck
import qualified Test.QuickCheck.Test as Qc
import qualified Test.HUnit as HUnit
import qualified Test.Hspec.Core.Runner as Hspec

-- | Perform mutation analysis
--
-- = For QuickCheck
-- 
-- > mucheck "qcheck" "qsort" "Examples/QuickCheckTest.hs" "Examples.QuickCheckTest" [
-- >   "quickCheckResult idEmpProp","quickCheckResult revProp", "quickCheckResult modelProp"]
--
-- = For HUnit
--
-- > mucheck "hunit" "qsort" "Examples/HUnitTest.hs" "Examples.HUnitTest" ["runTestTT tests"]
--
-- = For Hspec
--
-- > mucheck "hspec" "qsort" "Examples/HspecTest.hs" "Examples.HspecTest" ["spec"]
mucheck :: String -> String -> String -> String -> [String] -> IO ()
mucheck t fn file modulename args = do
  numMutants <- genMutants fn file
  let muts = take numMutants $ genFileNames file
      l = "./" ++ t ++ ".log"
      fn f = void $ f muts modulename args l
  case t of
    "qcheck" -> fn checkQuickCheckOnMutants
    "scheck" ->  fn checkSmallCheckOnMutants
    "hspec" ->  fn checkHspecOnMutants
    "hunit" ->  fn checkHUnitOnMutants
    _ -> error "Unexpected test type"

-- | run quickcheck test suite on mutants
--
-- > numMutants <- genMutants "qsort" "Examples/QuickCheckTest.hs"
-- > checkQuickCheckOnMutants (take numMutants $ genFileNames
-- >  "Examples/QuickCheckTest.hs") "Examples.QuickCheckTest" ["quickCheckResult idEmpProp", "quickCheckResult revProp", "quickCheckResult modelProp"] "./quickcheck.log"
checkQuickCheckOnMutants :: [String] -> String -> [String] -> String -> IO [Qc.Result]
checkQuickCheckOnMutants = mutantCheckSummary
 
-- | run hunit test suite on mutants
--
-- > numMutants <- genMutants "qsort" "Examples/HUnitTest.hs"
-- > checkHUnitOnMutants (take numMutants $ genFileNames "Examples/HUnitTest.hs") "Examples.HUnitTest" ["runTestTT tests"] "./hunit.log"
checkHUnitOnMutants :: [String] -> String -> [String] -> String -> IO [HUnit.Counts]
checkHUnitOnMutants = mutantCheckSummary

-- | run hspec test suite on mutants
--
-- > numMutants <- genMutants "qsort" "Examples/HspecTest.hs"
-- > checkHspecOnMutants (take numMutants $ genFileNames "Examples/HspecTest.hs") "Examples.HspecTest" ["spec (with \"qsort1\")"] "./hspec.log"
checkHspecOnMutants :: [String] -> String -> [String] -> String -> IO [Hspec.Summary]
checkHspecOnMutants = mutantCheckSummary

-- | run smallcheck test suite on mutants
--
-- > numMutants <- genMutants "qsort" "Examples/SmallCheckTest.hs"
-- > checkSmallCheckOnMutants (take numMutants $ genFileNames "Examples/SmallChecTest.hs") "Examples.SmallCheckTest" ["smallCheckM 2 revProp"] "./scheck.log"

checkSmallCheckOnMutants :: [String] -> String -> [String] -> String -> IO [SmallSummary]
checkSmallCheckOnMutants = mutantCheckSummary
