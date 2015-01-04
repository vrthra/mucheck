{-# LANGUAGE TupleSections #-}
-- | The entry point for mucheck
module Test.MuCheck.Interpreter (evaluateMutants, evalMethod, evalMyStr, summarizeResults) where

import qualified Language.Haskell.Interpreter as I
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM)
import Data.Typeable
import Test.MuCheck.Utils.Print (showA, catchOutput)
import Data.Either (partitionEithers, rights)
import Data.List(groupBy, sortBy)
import Data.Function (on)
import System.Directory (createDirectoryIfMissing)

import Test.MuCheck.TestAdapter
import Test.MuCheck.Utils.Common

-- | Datatype to hold results of a single test run (for all mutants).
-- We accept a list of tests to execute, and run each test against all of
-- mutants.
data TSum = TSum {tsumTest::String,
                  tsumNumMutants::Int,
                  tsumLoadError::Int,
                  tsumNotKilled::Int,
                  tsumKilled::Int,
                  tsumOthers::Int,
                  tsumLog::String}

-- | Datatype to hold results of the entire run
data TSSum = TSSum {tssumNumMutants::Int,
                    tssumAlive::Int,
                    tssumErrors::Int,
                    tssumLog::String}

-- | Given the list of tests suites to check, run one test suite at a time on
-- all mutants.
evaluateMutants :: (Summarizable a, Show a) => ([Mutant] -> [InterpreterOutput a] -> Summary) -> [Mutant] -> [String] -> FilePath -> IO (TSSum, [TSum])
evaluateMutants testSummaryFn mutantFiles evalSrcLst logFile  = do
  results <- mapM (runCodeOnMutants mutantFiles) evalSrcLst
  let singleTestSummaries = map (summarizeResults testSummaryFn mutantFiles) $ zip evalSrcLst results
      tssum  = multipleCheckSummary (isSuccess . snd) results
  return (tssum, singleTestSummaries)

summarizeResults :: Summarizable a => ([Mutant] -> [InterpreterOutput a] -> Summary) -> [Mutant] -> (String, [InterpreterOutput a]) -> TSum
summarizeResults testSummaryFn mutantFiles (test, results) = TSum {
    tsumTest = test,
    tsumNumMutants = r,
    tsumLoadError = l,
    tsumNotKilled = s,
    tsumKilled = f,
    tsumOthers = g,
    tsumLog = logStr}
  where (errorCases, executedCases) = partitionEithers $ map fst results
        [successCases, failureCases, otherCases] = map (\c -> filter (c . snd) executedCases) [isSuccess, isFailure, isOther]
        r = length results
        l = length errorCases
        [s,f,g] = map length [successCases, failureCases, otherCases]
        -- errorFiles = mutantFiles \\ map fst executedCases
        Summary logStr = testSummaryFn mutantFiles results


-- | Run one test suite on all mutants
runCodeOnMutants :: Typeable t => [Mutant] -> String -> IO [InterpreterOutput t]
runCodeOnMutants mutantFiles evalStr = mapM (evalMyStr evalStr) mutantFiles

evalMyStr :: Typeable a => String -> String -> IO (InterpreterOutput a)
evalMyStr eStr m = do
  createDirectoryIfMissing True ".mutants"
  let fPath = ".mutants/" ++ hash m ++ ".hs"
  -- Hint does not provide us a way to evaluate the module without
  -- writing it to disk first, so we go for this hack.
  -- We write the temporary file to disk, run interpreter on it, get
  -- the result and remove the file
  writeFile fPath m
  let logF = (fPath ++ ".log")
  liftM (, logF) $ catchOutput logF $ I.runInterpreter (evalMethod fPath eStr)

-- | Given the filename, modulename, test to evaluate, evaluate, and return result as a pair.
--
-- > t = I.runInterpreter (evalMethod
-- >        "Examples/QuickCheckTest.hs"
-- >        "quickCheckResult idEmp")
evalMethod :: (I.MonadInterpreter m, Typeable t) => String -> String -> m (String, t)
evalMethod fileName evalStr = do
  I.loadModules [fileName]
  ms <- I.getLoadedModules
  I.setTopLevelModules ms
  result <- I.interpret evalStr (I.as :: (Typeable a => IO a)) >>= liftIO
  return (fileName, result)


-- | Summarize the entire run
multipleCheckSummary :: Show b => ((String, b) -> Bool) -> [[InterpreterOutput b]] -> TSSum
multipleCheckSummary isSuccessFunction results
  -- we assume that checking each prop results in the same number of errorCases and executedCases
  | not (checkLength results) = error "Output lengths differ for some properties."
  | otherwise = TSSum {tssumNumMutants = countMutants,
                       tssumAlive = countAlive,
                       tssumErrors= countErrors,
                       tssumLog = logMsg}
  where executedCases = groupBy ((==) `on` fst) . sortBy (compare `on` fst) . rights $ map fst $ concat results
        allSuccesses = [rs | rs <- executedCases, length rs == length results, all isSuccessFunction rs]
        countAlive = length allSuccesses
        countErrors = countMutants - length executedCases
        logMsg = showA allSuccesses
        checkLength res = and $ map ((==countMutants) . length) res ++ map ((==countExecutedCases) . length) executedCases
        countExecutedCases = length . head $ executedCases
        countMutants = length . head $ results

