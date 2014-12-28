module Main where
import System.Environment (getArgs, withArgs)
import Control.Monad (void)

import Test.MuCheck (mucheck)
import Test.MuCheck.Utils.Print

main :: IO ()
main = do
  val <- getArgs
  case val of
    ("-h" : _ ) -> help
    (t: fn : file : modulename : args) -> withArgs [] $ mucheck t fn file modulename args
    _ -> error "Need [qcheck|hunit|hspec] function file modulename [args]\n\tUse -h to get help"

help :: IO ()
help = putStrLn $ "mucheck type function file modulename [args]\n" ++ showAS ["E.g:",
       " ./mucheck qcheck qsort Examples/QuickCheckTest.hs Examples.QuickCheckTest 'quickCheckResult idEmpProp' 'quickCheckResult revProp' 'quickCheckResult modelProp'",
       " ./mucheck hunit  qsort Examples/HUnitTest.hs Examples.HUnitTest 'runTestTT tests'",
       " ./mucheck hspec  qsort Examples/HspecTest.hs Examples.HspecTest spec"]

