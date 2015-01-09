module Main where
import System.Environment (getArgs)

import Test.MuCheck (mucheck)
import Test.MuCheck.TestAdapter.QuickCheck
import Test.MuCheck.TestAdapter
import Test.MuCheck.Utils.Print

main :: IO ()
main = do
  val <- getArgs
  case val of
    ("-h" : _ ) -> help
    (file : _args) -> do (msum, _tsum) <- mucheck (toRun file :: QuickCheckRun)
                         print msum
    _ -> error "Need function file [args]\n\tUse -h to get help"

help :: IO ()
help = putStrLn $ "mucheck function file [args]\n" ++ showAS ["E.g:",
       " mucheck-quickcheck Examples/QuickCheckTest.hs 'quickCheckResult idEmpProp' 'quickCheckResult revProp' 'quickCheckResult modelProp'",""]
