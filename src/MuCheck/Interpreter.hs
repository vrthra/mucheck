{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
module MuCheck.Interpreter where

import qualified Language.Haskell.Interpreter as I
import Control.Monad.Trans ( liftIO )
import qualified Test.QuickCheck.Test as Qc
import qualified Test.HUnit as HUnit
import qualified Test.Hspec.Core.Runner as Hspec
import Data.Typeable
import MuCheck.Utils.Print (showA, showAS, (./.))
import Data.Either (partitionEithers, rights)
import Data.List((\\), groupBy, sortBy, intercalate, isInfixOf)
import Data.Time.Clock

deriving instance Typeable Qc.Result
deriving instance Typeable HUnit.Counts
deriving instance Typeable Hspec.Summary

type InterpreterOutput a = Either I.InterpreterError (String, a)

checkQuickCheckOnMutants :: [String] -> String -> [String] -> String -> IO [Qc.Result]
checkQuickCheckOnMutants = mutantCheckSummary

checkHUnitOnMutants :: [String] -> String -> [String] -> String -> IO [HUnit.Counts]
checkHUnitOnMutants = mutantCheckSummary

checkHspecOnMutants :: [String] -> String -> [String] -> String -> IO [Hspec.Summary]
checkHspecOnMutants = mutantCheckSummary

-- main entry point
mutantCheckSummary :: Summarizable a => [String] -> String -> [String] -> FilePath -> IO [a]
mutantCheckSummary mutantFiles topModule evalSrcLst logFile  = do
  results <- mapM (runCodeOnMutants mutantFiles topModule) evalSrcLst
  let singleTestSummaries = zip evalSrcLst $ map (testSummary mutantFiles) results
      tssum  = suiteSummary results
  -- print results to terminal
  putStrLn $ delim ++ "Overall Results:"
  putStrLn $ terminalSummary tssum
  putStrLn $ showAS $ map showBrief singleTestSummaries
  putStr delim
  -- print results to logfile
  appendFile logFile $ "OVERALL RESULTS:\n" ++ (tssum_log tssum) ++ (showAS $ map showDetail singleTestSummaries)
  -- hacky solution to avoid printing entire results to stdout and to give
  -- guidance to the type checker in picking specific Summarizable instances
  return $ tail [head $ (map snd) $ snd $ partitionEithers $ head results]
  where showDetail (method, msum) = delim ++ showBrief (method, msum) ++ "\n" ++ detail msum
        showBrief (method, msum) = showAS [method,
                                           "\tTotal number of mutants:\t" ++ show (tsum_numMutants msum),
                                           "\tFailed to Load:\t" ++ show (tsum_loadError msum),
                                           "\tNot Killed:\t" ++ show (tsum_notKilled msum),
                                           "\tKilled:\t" ++ show (tsum_killed msum),
                                           "\tOthers:\t" ++ show (tsum_others msum),
                                           ""]
        detail msum = tsum_log msum
        terminalSummary tssum = showAS ["Total number of mutants:\t" ++ show (tssum_numMutants tssum),
                                        "Total number of alive mutants:\t" ++ show (tssum_alive tssum),
                                        "Total number of load errors:\t" ++ show (tssum_errors tssum),
                                        ""]
        delim = "\n" ++ (take 25 (repeat '=')) ++ "\n"

-- Interpreter Functionalities
-- Examples
-- t = runInterpreter (evalMethod "Examples/Quicksort.hs" "Quicksort" "quickCheckResult idEmp")
runCodeOnMutants mutantFiles topModule evalStr = mapM (evalMyStr evalStr) mutantFiles
  where evalMyStr evalStr file = do putStrLn $ ">" ++ ":" ++ file ++ ":" ++ topModule ++ ":" ++ evalStr ++ ">"
                                    x <- I.runInterpreter (evalMethod file topModule evalStr)
                                    return x

-- Given the filename, modulename, method to evaluate, evaluate, and return
-- result as a pair.
evalMethod :: (I.MonadInterpreter m, Typeable t) => String -> String -> String -> m (String, t)
evalMethod fileName topModule evalStr = do
  I.loadModules [fileName]
  I.setTopLevelModules [topModule]
  result <- I.interpret evalStr (I.as :: (Typeable a => IO a)) >>= liftIO
  return (fileName, result)

data TSum = TSum {tsum_numMutants::Int, tsum_loadError::Int, tsum_notKilled::Int, tsum_killed::Int, tsum_others::Int, tsum_log::String}
data TSSum = TSSum {tssum_numMutants::Int, tssum_alive::Int, tssum_errors::Int, tssum_log::String}

-- Class/Instance declaration
type MutantFilename = String
class Typeable s => Summarizable s where
  testSummary :: [MutantFilename] -> [InterpreterOutput s] -> TSum
  suiteSummary :: [[InterpreterOutput s]] -> TSSum
  isSuccess :: s -> Bool

instance Summarizable HUnit.Counts where
  testSummary mutantFiles results = TSum {
    tsum_numMutants = r,
    tsum_loadError = le,
    tsum_notKilled = s,
    tsum_killed = fl,
    tsum_others = re + ftc,
    tsum_log = logMsg}
    where (loadingErrorCases, executedCases) = partitionEithers results
          loadingErrorFiles = mutantFiles \\ map fst executedCases
          successCases = filter (isSuccess . snd) executedCases
          failuresCases = filter ((>0) . HUnit.failures . snd) executedCases
          runningErrorCases = (filter ((>0) . HUnit.errors . snd) executedCases) \\ failuresCases
          failToFullyTryCases = filter ((\c -> HUnit.cases c > HUnit.tried c) . snd) executedCases
          r = length results
          le = length loadingErrorCases
          [s, fl, re, ftc] = map length [successCases, failuresCases, runningErrorCases, failToFullyTryCases]
          logMsg = showAS ["Details:",
                           "Loading error files:",showA loadingErrorFiles,
                           "Loading error messages:",showA loadingErrorCases,
                           "Successes:", showA successCases,
                           "Failures:", showA failuresCases,
                           "Error while running:", showA runningErrorCases,
                           "Incompletely tested (may include failures and running errors):",showA failToFullyTryCases]
  suiteSummary = multipleCheckSummary (isSuccess . snd)
  isSuccess = (\c -> (HUnit.cases c == HUnit.tried c) && HUnit.failures c == 0 && HUnit.errors c == 0)

instance Summarizable Qc.Result where
  testSummary mutantFiles results = TSum {
    tsum_numMutants = r,
    tsum_loadError = e,
    tsum_notKilled = s,
    tsum_killed = f,
    tsum_others = g,
    tsum_log = logMsg}
    where (errorCases, executedCases) = partitionEithers results
          [successCases, failureCases, gaveUpCases] = map (\c -> filter (c . snd) executedCases) [isSuccess, isFailure, isGaveUp]
          r = length results
          e = length errorCases
          [s,f,g] = map length [successCases, failureCases, gaveUpCases]
          errorFiles = mutantFiles \\ map fst executedCases
          logMsg = showAS ["Details:",
                           "Loading error files:", showA errorFiles,
                           "Loading error messages:", showA errorCases,
                           "Successes:", showA successCases,
                           "Failure:", showA failureCases,
                           "Gaveups:", showA gaveUpCases]
          isFailure :: Qc.Result -> Bool
          isFailure Qc.Failure{} = True
          isFailure _         = False
          isGaveUp :: Qc.Result -> Bool
          isGaveUp Qc.GaveUp{} = True
          isGaveUp _        = False

  suiteSummary = multipleCheckSummary (isSuccess . snd)
  isSuccess = Qc.isSuccess

instance Summarizable Hspec.Summary where
  testSummary mutantFiles results = TSum {
    tsum_numMutants = r,
    tsum_loadError = e,
    tsum_notKilled = s,
    tsum_killed = f,
    tsum_others = 0,
    tsum_log = logMsg}
      where (errorCases, executedCases) = partitionEithers results
            r = length results
            e = length errorCases
            [successCases, failureCases] = map (\c -> filter (c . snd) executedCases) [isSuccess, isFailure]
            [s,f] = map length [successCases, failureCases]
            errorFiles = mutantFiles \\ map fst executedCases
            logMsg = showAS ["Details:",
                             "Loading error files:", showA errorFiles,
                             "Loading error messages:", showA errorCases,
                             "Successes:", showA successCases,
                             "Failure:", showA failureCases]
            isFailure = not . isSuccess
  suiteSummary = multipleCheckSummary (isSuccess . snd)
  isSuccess (Hspec.Summary { Hspec.summaryExamples = se, Hspec.summaryFailures = sf } ) = sf == 0

-- we assume that checking each prop results in the same number of errorCases and executedCases
multipleCheckSummary isSuccessFunction results
  | not (checkLength results) = error "Output lengths differ for some properties."
  | otherwise = TSSum {tssum_numMutants = countMutants, tssum_alive = countAlive, tssum_errors= countErrors, tssum_log = logMsg}
  where executedCases = groupBy (\x y -> fst x == fst y) . sortBy (\x y -> fst x `compare` fst y) . rights $ concat results
        allSuccesses = [rs | rs <- executedCases, length rs == length results, all isSuccessFunction rs]
        countAlive = length allSuccesses
        countErrors = countMutants - length executedCases
        logMsg = showA allSuccesses
        checkLength results = and $ map ((==countMutants) . length) results ++ map ((==countExecutedCases) . length) executedCases
        countExecutedCases = length . head $ executedCases
        countMutants = length . head $ results

