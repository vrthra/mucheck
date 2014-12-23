module Main where

import MuCheck.MuOp
import Language.Haskell.Exts
import MuCheck.StdArgs
import MuCheck.Interpreter
import MuCheck.Mutation
import MuCheck.Operators
import MuCheck.Utils



extraOps = Var . UnQual . Ident <$> ["qsort", "id", "reverse"]


-- NOTE: On QuickCheck.hs's mutants, it takes ~3-4 minutes to test 323 mutants on three properies.
mytest = checkPropsOnMutants (take 13 mutantFiles) topModule  evalSrc logFile
  where mutantFiles = genFileNames "Examples/Quicksort.hs"
        topModule = "Examples.Quicksort"
        evalSrc = ["quickCheckResult idEmpProp",  "quickCheckResult revProp", "quickCheckResult modelProp"]
        logFile = "./logs.txt"

genMut funcname filename = genMutantsWith (stdArgs {muOps = [Symbol "<" ==> Symbol ">"], maxNumMutants = 10000}) funcname filename
