{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

module MuCheck.Interpreter where

import Language.Haskell.Interpreter
import Control.Monad
import Test.QuickCheck.Test
import Test.HUnit
import Data.Typeable
import MuCheck.Utils
import Data.Either
import Data.List((\\), groupBy, sortBy)
import Data.Time.Clock

deriving instance Typeable Result
deriving instance Typeable Counts

type InterpreterOutput a = Either InterpreterError (String, a)

checkPropsOnMutants :: [String] -> String -> [String] -> String -> IO [Result]
checkPropsOnMutants = mutantCheckSummary

checkTestSuiteOnMutants :: [String] -> String -> [String] -> String -> IO [Counts]
checkTestSuiteOnMutants = mutantCheckSummary

mutantCheckSummary mutantFiles topModule codes logFile  =
    do
        results <- sequence $ map (runCodeOnMutants mutantFiles topModule) codes
        let singleTestSummaries = map (singleSummary mutantFiles) results
            overallSummary = multipleSummary results
            codes' = zipWith (++) (repeat "\n=======================\n") codes
        -- print results to terminal
        curTime <- getCurrentTime
        putStrLn $ "\n\n[]======== OVERALL RESULTS ========[]\n" ++ fst overallSummary 
        putStrLn $ printStringList (zipWith (++) codes' $ map fst singleTestSummaries)
        -- print results to logfile
        appendFile logFile $ "=============================\nTest run at " ++ show curTime ++ "\n" 
        appendFile logFile $ "OVERALL RESULTS:\n" ++ snd overallSummary ++ printStringList (zipWith (++) codes' $ map snd singleTestSummaries)
        putStr "\n[]===== END OF OVERALL RESULTS ====="
        -- hacky solution to avoid printing entire results to stdout and to give
        -- guidance to the type checker in picking specific Summarizable instances
        return $ tail [head . map snd . snd . partitionEithers . head $ results] 

-- Class/Instance declaration
type MutantFilename = String
type TerminalSummary = String
type LogSummary = String
class Typeable s => Summarizable s where
    singleSummary :: [MutantFilename] -> [InterpreterOutput s] -> (TerminalSummary, LogSummary)
    multipleSummary :: [[InterpreterOutput s]] -> (TerminalSummary, LogSummary)

instance Summarizable Counts where
    singleSummary mutantFiles results = (terminalMsg, logMsg)
        where (loadingErrorCases, executedCases) = partitionEithers results
              loadingErrorFiles = mutantFiles \\ map fst executedCases
              successCases = filter ((\c -> (cases c == tried c) && failures c == 0 && errors c == 0) . snd) executedCases
              failuresCases = filter ((>0) . failures . snd) executedCases
              runningErrorCases = (filter ((>0) . errors . snd) executedCases) \\ failuresCases
              failToFullyTryCases = filter ((\c -> cases c > tried c) . snd) executedCases
              r = length results
              le = length loadingErrorCases
              [s, fl, re, ftc] = map length [successCases, failuresCases, runningErrorCases, failToFullyTryCases]
              terminalMsg = "\n\nTotal number of mutants: " ++ show r ++
                            "\n\nFailed to load: " ++ show le ++
                            "\nSuccesses (not killed): " ++ show s ++
                            "\nFailures (killed): " ++ show fl ++
                            "\nError while running: " ++ show re ++
                            "\nIncompletely tested (may include failures and running errors): " ++ show ftc
              logMsg = terminalMsg ++ "\n\nDetails: \n\nLoading error files:\n" ++ printlnList loadingErrorFiles
                       ++ "\n\nLoading error messages:\n" ++ printlnList loadingErrorCases
                       ++ "\n\nSuccesses:\n" ++ printlnList successCases
                       ++ "\n\nFailures:\n" ++ printlnList failuresCases
                       ++ "\n\nError while running:\n" ++ printlnList runningErrorCases
                       ++ "\n\nIncompletely tested (may include failures and running errors):\n" 
                       ++ printlnList failToFullyTryCases ++ "\n"
    multipleSummary = multipleCheckSummary (\c -> (cases c == tried c) && failures c == 0 && errors c == 0)

instance Summarizable Result where
    singleSummary mutantFiles results = (terminalMsg, logMsg)
                  where (errorCases, executedCases) = partitionEithers results
                        [successCases, failureCases, gaveUpCases] = map (\c -> filter (c . snd) executedCases) [isSuccess, isFailure, isGaveUp]
                        r = length results
                        e = length errorCases
                        [s,f,g] = map length [successCases, failureCases, gaveUpCases]
                        errorFiles = mutantFiles \\ map fst executedCases
                        terminalMsg = "\n\nTotal number of mutants: " ++ show r ++
                                      "\n\nErrors: " ++ show e ++ showPerCent (e `percent` r) ++
                                      "\nSuccesses (not killed): " ++ show s ++ showPerCent (s `percent` r) ++
                                      "\nFailures (killed): " ++ show f ++ showPerCent (f `percent` r) ++
                                      "\nGaveups: " ++ show g ++ showPerCent (g `percent` r)
                        logMsg = terminalMsg ++ "\n\nDetails:\n\nLoading error files:\n" ++ printlnList errorFiles
                                ++ "\n\nLoading error messages:\n " ++ printlnList errorCases
                                ++ "\n\nSUCCESSES:\n " ++ printlnList successCases
                                ++ "\n\nFAILURE:\n " ++ printlnList failureCases
                                ++ "\n\nGAVEUPs:\n " ++ printlnList gaveUpCases ++ "\n"
    multipleSummary = multipleCheckSummary isSuccess

-- we assume that checking each prop results in the same number of errorCases and executedCases
multipleCheckSummary :: Show a => (a -> Bool) -> [[InterpreterOutput a]] -> (String, String)
multipleCheckSummary isSuccessFunction results
    | (not . and) (map ((==countMutants) . length) results ++ map ((==countExecutedCases) . length) executedCases) = error "Output lengths differ for some properties."
    | otherwise = (terminalMsg, logMsg)
                where executedCases = groupBy (\x y -> fst x == fst y) . sortBy (\x y -> fst x `compare` fst y) . rights $ concat results
                      allSuccesses = [rs | rs <- executedCases, length rs == length results, and (map (isSuccessFunction . snd) rs)]
                      countAlive = length allSuccesses
                      countMutants = length . head $ results
                      countExecutedCases = length . head $ executedCases
                      countErrors = countMutants - length executedCases
                      terminalMsg = "\nTotal number of mutants: " ++ show countMutants
                                 ++ "\nTotal number of alive and error-free mutants: " ++ show countAlive 
                                 ++ showPerCent (countAlive `percent` countMutants) ++ "\n"
                                 ++ "Total number of erroneous mutants (failed to be loaded): " ++ show countErrors ++ showPerCent (countErrors `percent` countMutants) ++ "\n"
                      logMsg = terminalMsg ++ "\nDetails:\n\n" ++ printlnList allSuccesses ++ "\n"

-- Interpreter Functionalities
runCodeOnMutants mutantFiles topModule code = sequence . map runInterpreter $ map (\mf -> runCodeOnMutant mf topModule code) mutantFiles

runCodeOnMutant fileName topModule code = do
                loadModules [fileName]
                setTopLevelModules [topModule]
                setImportsQ [("Prelude", Nothing), ("Test.QuickCheck", Nothing), ("Test.HUnit", Nothing)]
                result <- interpret code (as :: (Typeable a => IO a)) >>= liftIO
                return (fileName, result)

say :: String -> Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Error: " ++ (show e)

isFailure :: Result -> Bool
isFailure Failure{} = True
isFailure _         = False

isGaveUp :: Result -> Bool
isGaveUp GaveUp{} = True
isGaveUp _        = False

-- Examples
-- t = runI (runCodeOnMutant "Examples\\Quicksort.hs" "Quicksort" "quickCheckResult idEmp" "E:/logfile.txt")
-- mytest = checkPropsOnMutants (take 200 $ genFileNames "Examples/Quicksort.hs") "Examples.Quicksort" ["quickCheckResult idEmpProp",  "quickCheckResult revProp", "quickCheckResult modelProp"] "./logs.txt"
