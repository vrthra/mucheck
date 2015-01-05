{-# LANGUAGE TupleSections, MultiWayIf, DeriveDataTypeable #-}
-- | The entry point for mucheck
module Test.MuCheck.Interpreter (evaluateMutants, evalMethod, evalMutant, evalTest, summarizeResults, MutantSummary(..)) where

import qualified Language.Haskell.Interpreter as I
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM)
import Data.Typeable
import Test.MuCheck.Utils.Print (catchOutput)
import Data.Either (partitionEithers)
import System.Directory (createDirectoryIfMissing)
import System.Environment (withArgs)

import Test.MuCheck.TestAdapter
import Test.MuCheck.Utils.Common
import Test.MuCheck.AnalysisSummary


-- | Data type to hold results of a single test execution
data MutantSummary = MSumError Mutant String [Summary]         -- errorStr
                   | MSumAlive Mutant [Summary]
                   | MSumKilled Mutant [Summary]
                   | MSumOther Mutant [Summary]
                   deriving (Show, Typeable)

-- | Given the list of tests suites to check, run the test suite on mutants.
evaluateMutants :: (Summarizable a, Show a) => (Mutant -> TestStr -> InterpreterOutput a -> Summary) -> [Mutant] -> [String] -> IO (MAnalysisSummary, [MutantSummary])
evaluateMutants testSummaryFn mutants tests = do
  results <- mapM (evalMutant tests) mutants -- [InterpreterOutput t]
  let singleTestSummaries = map (summarizeResults testSummaryFn tests) $ zip mutants results
      ma  = fullSummary tests results
  return (ma, singleTestSummaries)

summarizeResults :: Summarizable a => (Mutant -> TestStr -> InterpreterOutput a -> Summary) -> [String] -> (Mutant, [InterpreterOutput a]) -> MutantSummary
summarizeResults testSummaryFn tests (mutant, ioresults) = case last results of -- the last result should indicate status because we dont run if there is error.
  Left err -> MSumError mutant (show err) logS
  Right out -> myresult out
  where results = map _io ioresults
        myresult out | isSuccess out = MSumAlive mutant logS
                     | isFailure out = MSumKilled mutant logS
                     | otherwise     = MSumOther mutant logS
        logS :: [Summary]
        logS = zipWith (testSummaryFn mutant) tests ioresults

-- | Run all tests on a mutant
evalMutant :: (Typeable t, Summarizable t) => [TestStr] -> Mutant -> IO [InterpreterOutput t]
evalMutant tests mutant = do
  -- Hint does not provide us a way to evaluate the module without
  -- writing it to disk first, so we go for this hack.
  -- We write the temporary file to disk, run interpreter on it, get
  -- the result (we dont remove the file now, but can be added)
  createDirectoryIfMissing True ".mutants"
  let mutantFile = ".mutants/" ++ hash mutant ++ ".hs"
  writeFile mutantFile mutant
  let logF = mutantFile ++ ".log"
  stopFast (evalTest mutantFile logF) tests

-- | Stop mutant runs at the first sign of problems.
stopFast :: (Typeable t, Summarizable t) => (String -> IO (InterpreterOutput t)) -> [TestStr] -> IO [InterpreterOutput t]
stopFast _ [] = return []
stopFast fn (x:xs) = do
  v <- fn x
  case _io v of
    Left _ -> return [v] -- load error
    Right out -> if isSuccess out
      then liftM (v :) $ stopFast fn xs
      else return [v] -- test failed (mutant detected)

-- | Run one single test on a mutant
evalTest :: (Typeable a, Summarizable a) => String -> String -> String -> IO (InterpreterOutput a)
evalTest mutantFile logF test = do
  val <- withArgs [] $ catchOutput logF $ I.runInterpreter (evalMethod mutantFile test)
  return Io {_io = val, _ioLog = logF}

-- | Given the filename, modulename, test to evaluate, evaluate, and return result as a pair.
--
-- > t = I.runInterpreter (evalMethod
-- >        "Examples/QuickCheckTest.hs"
-- >        "quickCheckResult idEmp")
evalMethod :: (I.MonadInterpreter m, Typeable t) => String -> String -> m t
evalMethod fileName evalStr = do
  I.loadModules [fileName]
  ms <- I.getLoadedModules
  I.setTopLevelModules ms
  I.interpret evalStr (I.as :: (Typeable a => IO a)) >>= liftIO


-- | Summarize the entire run. Passed results are per mutant
fullSummary :: (Show b, Summarizable b) => [TestStr] -> [[InterpreterOutput b]] -> MAnalysisSummary
fullSummary _tests results = MAnalysisSummary {
  maNumMutants = length results,
  maAlive = length alive,
  maKilled = length fails,
  maErrors= length errors}
  where res = map (map _io) results
        lasts = map last res -- get the last test runs
        (errors, completed) = partitionEithers lasts
        fails = filter isFailure completed -- look if others failed or not
        alive = filter isSuccess completed

