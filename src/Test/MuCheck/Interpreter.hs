{-# LANGUAGE TupleSections, MultiWayIf, DeriveDataTypeable #-}
-- | The Interpreter module is responible for invoking the Hint interpreter to
-- evaluate mutants.
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
data MutantSummary = MSumError Mutant String [Summary]         -- ^ Capture the error if one occured
                   | MSumAlive Mutant [Summary]                -- ^ The mutant was alive
                   | MSumKilled Mutant [Summary]               -- ^ The mutant was kileld
                   | MSumOther Mutant [Summary]                -- ^ Undetermined - we will treat it as killed as it is not a success.
                   deriving (Show, Typeable)

-- | Given the list of tests suites to check, run the test suite on mutants.
evaluateMutants :: (Summarizable a, Show a) =>
    (Mutant -> TestStr -> InterpreterOutput a -> Summary)           -- ^ The summary function
 -> [Mutant]                                                        -- ^ The mutants to be evaluated
 -> [TestStr]                                                       -- ^ The tests to be used by mutation analysis
 -> IO (MAnalysisSummary, [MutantSummary])                          -- ^ Returns a tuple of full run summary and individual mutant summary
evaluateMutants testSummaryFn mutants tests = do
  results <- mapM (evalMutant tests) mutants -- [InterpreterOutput t]
  let singleTestSummaries = map (summarizeResults testSummaryFn tests) $ zip mutants results
      ma  = fullSummary tests results
  return (ma, singleTestSummaries)

-- | The `summarizeResults` function evaluates the results of a test run
-- using the supplied `isSuccess` and `testSummaryFn` functions from the adapters
summarizeResults :: Summarizable a =>
     (Mutant -> TestStr -> InterpreterOutput a -> Summary)        -- ^ The summary function
  -> [TestStr]                                                    -- ^ Tests we used to run analysis
  -> (Mutant, [InterpreterOutput a])                              -- ^ The mutant and its corresponding output of test runs.
  -> MutantSummary                                                -- ^ Returns a summary of the run for the mutant
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
evalMutant :: (Typeable t, Summarizable t) =>
    [TestStr]                                                     -- ^ The tests to be used
  -> Mutant                                                       -- ^ Mutant being tested
  -> IO [InterpreterOutput t]                                     -- ^ Returns the result of test runs
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

-- | Stop mutant runs at the first sign of problems (invalid mutants or test
-- failure).
stopFast :: (Typeable t, Summarizable t) =>
     (String -> IO (InterpreterOutput t))  -- ^ The function that given a test, runs it, and returns the result
  -> [TestStr]                             -- ^ The tests to be run
  -> IO [InterpreterOutput t]              -- ^ Returns the output of all tests. If there is an error, then it will be at the last test.
stopFast _ [] = return []
stopFast fn (x:xs) = do
  v <- fn x
  case _io v of
    Left _ -> return [v] -- load error
    Right out -> if isSuccess out
      then liftM (v :) $ stopFast fn xs
      else return [v] -- test failed (mutant detected)

-- | Run one single test on a mutant
evalTest :: (Typeable a, Summarizable a) =>
    String                                 -- ^ The mutant _file_ that we have to evaluate (_not_ the content)
 -> String                                 -- ^ The file where we will write the stdout and stderr during the run. 
 -> TestStr                                -- ^ The test to be run
 -> IO (InterpreterOutput a)               -- ^ Returns the output of given test run
evalTest mutantFile logF test = do
  val <- withArgs [] $ catchOutput logF $ I.runInterpreter (evalMethod mutantFile test)
  return Io {_io = val, _ioLog = logF}

-- | Given the filename, modulename, test to evaluate, evaluate, and return result as a pair.
--
-- > t = I.runInterpreter (evalMethod
-- >        "Examples/QuickCheckTest.hs"
-- >        "quickCheckResult idEmp")
evalMethod :: (I.MonadInterpreter m, Typeable t) =>
     String                               -- ^ The mutant _file_ to load
  -> TestStr                              -- ^ The test to be run
  -> m t                                  -- ^ Returns the monadic computation to be run by I.runInterpreter
evalMethod fileName evalStr = do
  I.loadModules [fileName]
  ms <- I.getLoadedModules
  I.setTopLevelModules ms
  I.interpret evalStr (I.as :: (Typeable a => IO a)) >>= liftIO


-- | Summarize the entire run. Passed results are per mutant
fullSummary :: (Show b, Summarizable b) =>
     [TestStr]                              -- ^ The list of tests we used
  -> [[InterpreterOutput b]]                -- ^ The test ouput (per mutant, (per test))
  -> MAnalysisSummary                       -- ^ Returns the full summary of the run
fullSummary _tests results = MAnalysisSummary {
  _maNumMutants = length results,
  _maAlive = length alive,
  _maKilled = length fails,
  _maErrors= length errors}
  where res = map (map _io) results
        lasts = map last res -- get the last test runs
        (errors, completed) = partitionEithers lasts
        fails = filter isFailure completed -- look if others failed or not
        alive = filter isSuccess completed

