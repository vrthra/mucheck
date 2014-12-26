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
    "hunit" -> checkHUnitOnMutants (take numMutants $ genFileNames file) modulename args "./hunit.log" >> return ()
    "hspec" -> checkHspecOnMutants (take numMutants $ genFileNames file) modulename args "./hspec.log" >> return ()


main :: IO ()
main = do
  val <- getArgs
  case val of
    ("-h" : _ ) -> help
    (t: fn : file : modulename : args) -> process t fn file modulename args
    _ -> error "Need [qcheck|hunit|hpsec] function file modulename [args]\n\tUse -h to get help"

help :: IO ()
help = putStrLn ("mucheck type function file modulename [args]\n" ++ (showAS ["E.g:",
       "\t./mucheck qcheck qsort Examples/QuickCheckTest.hs Examples.QuickCheckTest 'quickCheckResult idEmpProp' 'quickCheckResult revProp' 'quickCheckResult modelProp'",
       "\t./mucheck hunit  qsort Examples/QuickHUnitTest.hs Examples.HUnitTest 'runTestTT tests'",
       "\t./mucheck hspec  qsort Examples/QuickHspecTest.hs Examples.HspecTest 'spec (with \"qsort1\")'"]))

