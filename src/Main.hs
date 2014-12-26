module Main where
import System.Environment

import MuCheck.MuOp
import Language.Haskell.Exts
import MuCheck.StdArgs
import MuCheck.Interpreter
import MuCheck.Mutation
import MuCheck.Operators
import MuCheck.Utils.Common
import MuCheck.Utils.Print

process :: String -> String -> String -> String -> [String] -> IO ()
process t fn file modulename args = do
  numMutants <- genMutants fn file
  case t of
    "qcheck" -> checkQuickCheckOnMutants (take numMutants $ genFileNames file) modulename args "./qcheck.log" >> return ()
    "hspec" ->  checkHspecOnMutants (take numMutants $ genFileNames file) modulename args "./hspec.log" >> return ()
    "hunit" ->  checkHUnitOnMutants (take numMutants $ genFileNames file) modulename args "./hunit.log" >> return ()
    _ -> error "Unexpected test type"


main :: IO ()
main = do
  val <- getArgs
  case val of
    ("-h" : _ ) -> help
    (t: fn : file : modulename : args) -> withArgs [] $ process t fn file modulename args
    _ -> error "Need [qcheck|hunit|hspec] function file modulename [args]\n\tUse -h to get help"

help :: IO ()
help = putStrLn ("mucheck type function file modulename [args]\n" ++ (showAS ["E.g:",
       " ./mucheck qcheck qsort Examples/QuickCheckTest.hs Examples.QuickCheckTest 'quickCheckResult idEmpProp' 'quickCheckResult revProp' 'quickCheckResult modelProp'",
       " ./mucheck hunit  qsort Examples/HUnitTest.hs Examples.HUnitTest 'runTestTT tests'",
       " ./mucheck hspec  qsort Examples/HspecTest.hs Examples.HspecTest 'spec (with \\\"qsort1\\\")'"]))

