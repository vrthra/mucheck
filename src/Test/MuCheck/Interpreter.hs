{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
-- | The entry point for mucheck
module Test.MuCheck.Interpreter (mutantCheckSummary) where

import qualified Language.Haskell.Interpreter as I
import Control.Monad.Trans ( liftIO )
import Data.Typeable
import Test.MuCheck.Utils.Print (showA, showAS, (./.))
import Data.Either (partitionEithers, rights)
import Data.List(groupBy, sortBy)
import Data.Function (on)

import Test.MuCheck.TestAdapter

-- | Given the list of tests suites to check, run one test suite at a time on
-- all mutants.
mutantCheckSummary :: (Summarizable a, Show a) => ([String] -> [InterpreterOutput a] -> Summary) -> [String] -> String -> [String] -> FilePath -> IO ()
mutantCheckSummary testSummaryFn mutantFiles topModule evalSrcLst logFile  = do
  results <- mapM (runCodeOnMutants mutantFiles topModule) evalSrcLst
  let singleTestSummaries = zip evalSrcLst $ map (mySummaryFn testSummaryFn mutantFiles) results
      tssum  = multipleCheckSummary (isSuccess . snd) results
  -- print results to terminal
  putStrLn $ delim ++ "Overall Results:"
  putStrLn $ terminalSummary tssum
  putStrLn $ showAS $ map showBrief singleTestSummaries
  putStr delim
  -- print results to logfile
  appendFile logFile $ "OVERALL RESULTS:\n" ++ tssum_log tssum ++ showAS (map showDetail singleTestSummaries)
  return ()
  where showDetail (method, msum) = delim ++ showBrief (method, msum) ++ "\n" ++ tsum_log msum
        showBrief (method, msum) = showAS [method,
           "\tTotal number of mutants:\t" ++ show (tsum_numMutants msum),
           "\tFailed to Load:\t" ++ (cpx tsum_loadError),
           "\tNot Killed:\t" ++ (cpx tsum_notKilled),
           "\tKilled:\t" ++ (cpx tsum_killed),
           "\tOthers:\t" ++ (cpx tsum_others),
           ""]
           where cpx fn = show (fn msum) ++ " " ++ (fn msum) ./. (tsum_numMutants msum)
        terminalSummary tssum = showAS [
          "Total number of mutants:\t" ++ show (tssum_numMutants tssum),
          "Total number of alive mutants:\t" ++ (cpx tssum_alive),
          "Total number of load errors:\t" ++ (cpx tssum_errors),
          ""]
           where cpx fn = show (fn tssum) ++ " " ++ (fn tssum) ./. (tssum_numMutants tssum)
        delim = "\n" ++ replicate 25 '=' ++ "\n"


data TSum = TSum {tsum_numMutants::Int,
                  tsum_loadError::Int,
                  tsum_notKilled::Int,
                  tsum_killed::Int,
                  tsum_others::Int,
                  tsum_log::String}

mySummaryFn :: (Summarizable b, Eq a1) => ([a1] -> [Either a (a1, b)] -> Summary) -> [a1] -> [Either a (a1, b)] -> TSum
mySummaryFn testSummaryFn mutantFiles results = TSum {
    tsum_numMutants = r,
    tsum_loadError = l,
    tsum_notKilled = s,
    tsum_killed = f,
    tsum_others = g,
    tsum_log = logStr}
  where (errorCases, executedCases) = partitionEithers results
        [successCases, failureCases, otherCases] = map (\c -> filter (c . snd) executedCases) [isSuccess, isFailure, isOther]
        r = length results
        l = length errorCases
        [s,f,g] = map length [successCases, failureCases, otherCases]
        -- errorFiles = mutantFiles \\ map fst executedCases
        Summary logStr = testSummaryFn mutantFiles results


-- | Run one test suite on all mutants
runCodeOnMutants :: Typeable t => [String] -> String -> String -> IO [InterpreterOutput t]
runCodeOnMutants mutantFiles topModule evalStr = mapM (evalMyStr evalStr) mutantFiles
  where evalMyStr eStr file = do putStrLn $ ">" ++ ":" ++ file ++ ":" ++ topModule ++ ":" ++ evalStr
                                 I.runInterpreter (evalMethod file topModule eStr)

-- | Given the filename, modulename, test to evaluate, evaluate, and return result as a pair.
--
-- > t = I.runInterpreter (evalMethod
-- >        "Examples/QuickCheckTest.hs"
-- >        "Examples.QuickCheckTest"
-- >        "quickCheckResult idEmp)
evalMethod :: (I.MonadInterpreter m, Typeable t) => String -> String -> String -> m (String, t)
evalMethod fileName topModule evalStr = do
  I.loadModules [fileName]
  I.setTopLevelModules [topModule]
  result <- I.interpret evalStr (I.as :: (Typeable a => IO a)) >>= liftIO
  return (fileName, result)

-- | Datatype to hold results of the entire run
data TSSum = TSSum {tssum_numMutants::Int,
                    tssum_alive::Int,
                    tssum_errors::Int,
                    tssum_log::String}

-- | Summarize the entire run
multipleCheckSummary :: Show b => ((String, b) -> Bool) -> [[InterpreterOutput b]] -> TSSum
multipleCheckSummary isSuccessFunction results
  -- we assume that checking each prop results in the same number of errorCases and executedCases
  | not (checkLength results) = error "Output lengths differ for some properties."
  | otherwise = TSSum {tssum_numMutants = countMutants,
                       tssum_alive = countAlive,
                       tssum_errors= countErrors,
                       tssum_log = logMsg}
  where executedCases = groupBy ((==) `on` fst) . sortBy (compare `on` fst) . rights $ concat results
        allSuccesses = [rs | rs <- executedCases, length rs == length results, all isSuccessFunction rs]
        countAlive = length allSuccesses
        countErrors = countMutants - length executedCases
        logMsg = showA allSuccesses
        checkLength res = and $ map ((==countMutants) . length) res ++ map ((==countExecutedCases) . length) executedCases
        countExecutedCases = length . head $ executedCases
        countMutants = length . head $ results

