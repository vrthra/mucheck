{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

module MuCheck.Interpreter where

import Language.Haskell.Interpreter
import Control.Monad
import Test.QuickCheck.Test
import Data.Typeable
import MuCheck.Utils
import Data.Either
import Data.List((\\), intersperse, groupBy, sortBy)
import Data.Time.Clock

deriving instance Typeable Result


checkPropOnMutants mutantFiles topModule code = sequence . map runInterpreter $ map (\mf -> testMutant mf topModule code) mutantFiles

checkPropsOnMutants mutantFiles topModule codes logFile =
    do
        results <- sequence $ map (checkPropOnMutants mutantFiles topModule) codes
        let singleTestSummaries = map (singleTestSummary mutantFiles) results
            overallSummary = testSuiteSummary results
            codes' = zipWith (++) (repeat "=======================\n") codes
        -- print results to terminal
        curTime <- getCurrentTime
        putStrLn $ "\n\nOVERALL RESULTS:\n" ++ fst overallSummary 
        putStrLn $ printStringList (zipWith (++) codes' $ map fst singleTestSummaries)
        -- print results to logfile
        appendFile logFile $ "=============================\nTest run at " ++ show curTime ++ "\n" 
        appendFile logFile $ "OVERALL RESULTS:\n" ++ snd overallSummary ++ printStringList (zipWith (++) codes' $ map snd singleTestSummaries)

singleTestSummary :: [String] -> [Either InterpreterError (String, Result)] -> (String, String)
singleTestSummary mutantFiles result = (terminalMsg, logMsg)
                  where (errorCases, executedCases) = partitionEithers result
                        [successCases, failureCases, gaveUpCases] = map (\c -> filter (c . snd) executedCases) [isSuccess, isFailure, isGaveUp]
                        r = length result
                        e = length errorCases
                        [s,f,g] = map length [successCases, failureCases, gaveUpCases]
                        errorFiles = mutantFiles \\ map fst executedCases
                        terminalMsg = "\n\nTotal number of mutants: " ++ show r ++
                                      "\n\nErrors: " ++ show e ++ showPCent (e `pcent` r) ++
                                      "\nSuccesses (not killed): " ++ show s ++ showPCent (s `pcent` r) ++
                                      "\nFailures (killed): " ++ show f ++ showPCent (f `pcent` r) ++
                                      "\nGaveups: " ++ show g ++ showPCent (g `pcent` r)
                        logMsg = terminalMsg ++ "\n\nDetails:\n\nERROR files:\n " ++ printlnList errorFiles
                                ++ "\n\nERROR messages:\n " ++ printlnList errorCases
                                ++ "\n\nSUCCESSES:\n " ++ printlnList successCases
                                ++ "\n\nFAILURE:\n " ++ printlnList failureCases
                                ++ "\n\nGAVEUPs:\n " ++ printlnList gaveUpCases ++ "\n"

-- we assume that checking each prop results in the same number of errorCases and executedCases
-- testSuiteSummary :: [[Either InterpreterError (String, Result)]] -> (String, String)
testSuiteSummary results
    | (not . and) (map ((==countMutants) . length) results ++ map ((==countExecutedCases) . length) executedCases) = error "Output lengths differ for some properties."
    | otherwise = (terminalMsg, logMsg)
                where executedCases = groupBy (\x y -> fst x == fst y) . sortBy (\x y -> fst x `compare` fst y) . rights $ concat results
                      allSuccesses = [rs | rs <- executedCases, length rs == length results, and (map (isSuccess . snd) rs)]
                      countAlive = length allSuccesses
                      countMutants = length . head $ results
                      countExecutedCases = length . head $ executedCases
                      countErrors = countMutants - length executedCases
                      terminalMsg = "\nTotal number of mutants: " ++ show countMutants
                                 ++ "\nTotal number of alive and error-free mutants: " ++ show countAlive 
                                 ++ showPCent (countAlive `pcent` countMutants) ++ "\n"
                                 ++ "Total number of erroneous mutants: " ++ show countErrors ++ showPCent (countErrors `pcent` countMutants) ++ "\n"
                      logMsg = terminalMsg ++ "\nDetails:\n\n" ++ printlnList allSuccesses ++ "\n"

showPCent x = " (" ++ show x ++ "%)"
n `pcent` t = 100 * n  `div` t

printStringList :: [String] -> String
printStringList = concat . intersperse "\n"

printlnList :: Show a => [a] -> String
printlnList =  printStringList . map show

testMutant :: String -> String -> String -> Interpreter (String, Result)
testMutant fileName topModule code = do
            loadModules [fileName]
            setTopLevelModules [topModule]
            setImportsQ [("Prelude", Nothing), ("Test.QuickCheck", Nothing)]
            result <- interpret code (as :: IO Result) >>= liftIO
            return (fileName, result)

-- testHint1 :: Interpreter ()
-- testHint1 = do
                -- setImportsQ [("Prelude", Nothing)]
                -- let somecode = "putStrLn \"ten ten\""
                -- interpret somecode (as :: IO ()) >>= liftIO
                -- let somecode1 = "writeFile \"E:/test.txt\" \"sheng\"" 
                -- interpret somecode1 (as :: IO ()) >>= liftIO
                -- say "hello"

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
-- t = runI (testMutant "Examples\\Quicksort.hs" "Quicksort" "quickCheckResult idEmp" "E:/logfile.txt")
-- mytest = checkPropsOnMutants (take 200 $ genFileNames "Examples/Quicksort.hs") "Examples.Quicksort" ["quickCheckResult idEmpProp",  "quickCheckResult revProp", "quickCheckResult modelProp"] "./logs.txt"
