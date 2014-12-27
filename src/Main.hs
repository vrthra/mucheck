module Main where
import System.Environment (getArgs, withArgs)
import Control.Monad (void)

import Test.MuCheck.MuOp
import Test.MuCheck.Config
import Test.MuCheck.Mutation
import Test.MuCheck.Operators
import Test.MuCheck.Utils.Common
import Test.MuCheck.Utils.Print
import Test.MuCheck.Interpreter
import Test.MuCheck.Run.QuickCheck
import Test.MuCheck.Run.HUnit
import Test.MuCheck.Run.Hspec

process :: String -> String -> String -> String -> [String] -> IO ()
process t fn file modulename args = do
  numMutants <- genMutants fn file
  let muts = take numMutants $ genFileNames file
      l = "./" ++ t ++ ".log"
      fn f = void $ f muts modulename args l
  case t of
    "qcheck" -> fn checkQuickCheckOnMutants
    "hspec" ->  fn checkHspecOnMutants
    "hunit" ->  fn checkHUnitOnMutants
    _ -> error "Unexpected test type"


main :: IO ()
main = do
  val <- getArgs
  case val of
    ("-h" : _ ) -> help
    (t: fn : file : modulename : args) -> withArgs [] $ process t fn file modulename args
    _ -> error "Need [qcheck|hunit|hspec] function file modulename [args]\n\tUse -h to get help"

help :: IO ()
help = putStrLn $ "mucheck type function file modulename [args]\n" ++ showAS ["E.g:",
       " ./mucheck qcheck qsort Examples/QuickCheckTest.hs Examples.QuickCheckTest 'quickCheckResult idEmpProp' 'quickCheckResult revProp' 'quickCheckResult modelProp'",
       " ./mucheck hunit  qsort Examples/HUnitTest.hs Examples.HUnitTest 'runTestTT tests'",
       " ./mucheck hspec  qsort Examples/HspecTest.hs Examples.HspecTest 'spec (with \\\"qsort1\\\")'"]

