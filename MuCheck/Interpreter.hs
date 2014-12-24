{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
module MuCheck.Interpreter where

import qualified Language.Haskell.Interpreter as I
import Control.Monad.Trans ( liftIO )
import qualified Test.QuickCheck.Test as Qc
import qualified Test.HUnit as HUnit
import Data.Typeable
import qualified MuCheck.Utils.Print as Mu
import Data.Either
import Data.List((\\), groupBy, sortBy)
import Data.Time.Clock

deriving instance Typeable Qc.Result
deriving instance Typeable HUnit.Counts

type InterpreterOutput a = Either I.InterpreterError (String, a)

checkPropsOnMutants :: [String] -> String -> [String] -> String -> IO [Qc.Result]
checkPropsOnMutants = mutantCheckSummary

checkTestSuiteOnMutants :: [String] -> String -> [String] -> String -> IO [HUnit.Counts]
checkTestSuiteOnMutants = mutantCheckSummary

-- main entry point
mutantCheckSummary :: Summarizable a => [String] -> String -> [String] -> FilePath -> IO [a]
mutantCheckSummary mutantFiles topModule evalSrcLst logFile  = do
  results <- mapM (runCodeOnMutants mutantFiles topModule) evalSrcLst
  let delim = "\n" ++ (take 25 (repeat '=')) ++ "\n"
      singleTestSummaries = map (singleSummary mutantFiles) results
      (terminalSummary,logSummary) = multipleSummary results
      evalSrcLst' = map (delim ++) evalSrcLst
  -- print results to terminal
  putStrLn $ delim ++ "Overall Results:"
  putStrLn terminalSummary
  putStrLn $ Mu.showAS $ zipWith (++) evalSrcLst' $ map fst singleTestSummaries
  putStr delim
  -- print results to logfile
  appendFile logFile $ "OVERALL RESULTS:\n" ++ logSummary ++ Mu.showAS (zipWith (++) evalSrcLst' $ map snd singleTestSummaries)
  -- hacky solution to avoid printing entire results to stdout and to give
  -- guidance to the type checker in picking specific Summarizable instances
  return $ tail [head $ (map snd) $ snd $ partitionEithers $ head results]

-- Interpreter Functionalities
-- Examples
-- t = runInterpreter (evalMethod "Examples/Quicksort.hs" "Quicksort" "quickCheckResult idEmp")
runCodeOnMutants mutantFiles topModule evalStr = mapM (evalMyStr evalStr) mutantFiles
  where evalMyStr evalStr file = I.runInterpreter (evalMethod file topModule evalStr)

-- Given the filename, modulename, method to evaluate, evaluate, and return
-- result as a pair.
evalMethod :: (I.MonadInterpreter m, Typeable t) => String -> String -> String -> m (String, t)
evalMethod fileName topModule evalStr = do
  I.loadModules [fileName]
  I.setTopLevelModules [topModule]
  I.setImports ["Prelude", "Test.QuickCheck", "Test.HUnit"]
  result <- I.interpret evalStr (I.as :: (Typeable a => IO a)) >>= liftIO
  return (fileName, result)


-- Class/Instance declaration
type MutantFilename = String
type TerminalSummary = String
type LogSummary = String
class Typeable s => Summarizable s where
    singleSummary :: [MutantFilename] -> [InterpreterOutput s] -> (TerminalSummary, LogSummary)
    multipleSummary :: [[InterpreterOutput s]] -> (TerminalSummary, LogSummary)

instance Summarizable HUnit.Counts where
    singleSummary mutantFiles results = (terminalMsg, logMsg)
        where (loadingErrorCases, executedCases) = partitionEithers results
              loadingErrorFiles = mutantFiles \\ map fst executedCases
              successCases = filter ((\c -> (HUnit.cases c == HUnit.tried c) && HUnit.failures c == 0 && HUnit.errors c == 0) . snd) executedCases
              failuresCases = filter ((>0) . HUnit.failures . snd) executedCases
              runningErrorCases = (filter ((>0) . HUnit.errors . snd) executedCases) \\ failuresCases
              failToFullyTryCases = filter ((\c -> HUnit.cases c > HUnit.tried c) . snd) executedCases
              r = length results
              le = length loadingErrorCases
              [s, fl, re, ftc] = map length [successCases, failuresCases, runningErrorCases, failToFullyTryCases]
              terminalMsg = "\n\nTotal number of mutants: " ++ show r ++
                            "\n\nFailed to load: " ++ show le ++
                            "\nSuccesses (not killed): " ++ show s ++
                            "\nFailures (killed): " ++ show fl ++
                            "\nError while running: " ++ show re ++
                            "\nIncompletely tested (may include failures and running errors): " ++ show ftc
              logMsg = terminalMsg ++ "\n\nDetails: \n\nLoading error files:\n" ++ Mu.showA loadingErrorFiles
                       ++ "\n\nLoading error messages:\n" ++ Mu.showA loadingErrorCases
                       ++ "\n\nSuccesses:\n" ++ Mu.showA successCases
                       ++ "\n\nFailures:\n" ++ Mu.showA failuresCases
                       ++ "\n\nError while running:\n" ++ Mu.showA runningErrorCases
                       ++ "\n\nIncompletely tested (may include failures and running errors):\n"
                       ++ Mu.showA failToFullyTryCases ++ "\n"
    multipleSummary = multipleCheckSummary (\c -> (HUnit.cases c == HUnit.tried c) && HUnit.failures c == 0 && HUnit.errors c == 0)

instance Summarizable Qc.Result where
    singleSummary mutantFiles results = (terminalMsg, logMsg)
      where (errorCases, executedCases) = partitionEithers results
            [successCases, failureCases, gaveUpCases] = map (\c -> filter (c . snd) executedCases) [Qc.isSuccess, isFailure, isGaveUp]
            r = length results
            e = length errorCases
            [s,f,g] = map length [successCases, failureCases, gaveUpCases]
            errorFiles = mutantFiles \\ map fst executedCases
            terminalMsg = "\n\nTotal number of mutants: " ++ show r ++
                          "\n\nErrors: " ++ show e ++ Mu.showPerCent (e `Mu.percent` r) ++
                          "\nSuccesses (not killed): " ++ show s ++ Mu.showPerCent (s `Mu.percent` r) ++
                          "\nFailures (killed): " ++ show f ++ Mu.showPerCent (f `Mu.percent` r) ++
                          "\nGaveups: " ++ show g ++ Mu.showPerCent (g `Mu.percent` r)
            logMsg = terminalMsg ++ "\n\nDetails:\n\nLoading error files:\n" ++ Mu.showA errorFiles
                    ++ "\n\nLoading error messages:\n " ++ Mu.showA errorCases
                    ++ "\n\nSUCCESSES:\n " ++ Mu.showA successCases
                    ++ "\n\nFAILURE:\n " ++ Mu.showA failureCases
                    ++ "\n\nGAVEUPs:\n " ++ Mu.showA gaveUpCases ++ "\n"
            isFailure :: Qc.Result -> Bool
            isFailure Qc.Failure{} = True
            isFailure _         = False
            isGaveUp :: Qc.Result -> Bool
            isGaveUp Qc.GaveUp{} = True
            isGaveUp _        = False

    multipleSummary = multipleCheckSummary Qc.isSuccess

-- we assume that checking each prop results in the same number of errorCases and executedCases
multipleCheckSummary :: Show a => (a -> Bool) -> [[InterpreterOutput a]] -> (String, String)
multipleCheckSummary isSuccessFunction results
  | not (checkLength results) = error "Output lengths differ for some properties."
  | otherwise = (terminalMsg, logMsg)
   where executedCases = groupBy (\x y -> fst x == fst y) . sortBy (\x y -> fst x `compare` fst y) . rights $ concat results
         allSuccesses = [rs | rs <- executedCases, length rs == length results, all (isSuccessFunction . snd) rs]
         countAlive = length allSuccesses
         countErrors = countMutants - length executedCases
         terminalMsg = "\nTotal number of mutants: " ++ show countMutants
                    ++ "\nTotal number of alive and error-free mutants: " ++ show countAlive
                    ++ Mu.showPerCent (countAlive `Mu.percent` countMutants) ++ "\n"
                    ++ "Total number of erroneous mutants (failed to be loaded): " ++ show countErrors ++ Mu.showPerCent (countErrors `Mu.percent` countMutants) ++ "\n"
         logMsg = terminalMsg ++ "\nDetails:\n\n" ++ Mu.showA allSuccesses ++ "\n"

         checkLength results = and $ map ((==countMutants) . length) results ++ map ((==countExecutedCases) . length) executedCases
         countExecutedCases = length . head $ executedCases
         countMutants = length . head $ results

