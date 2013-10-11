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
mytest = checkPropsOnMutants (take 20 $ genFileNames "Examples/Quicksort.hs") "Examples.Quicksort" ["quickCheckResult idEmpProp",  "quickCheckResult revProp", "quickCheckResult modelProp"] "./logs.txt"



{- Sample results:

OVERALL RESULTS:

Total number of mutants: 323
Total number of alive and error-free mutants: 1 (0%)
Total number of erroneous mutants: 0 (0%)

=======================
quickCheckResult idEmpProp

Total number of mutants: 323

Errors: 0 (0%)
Successes (not killed): 30 (9%)
Failures (killed): 293 (90%)
Gaveups: 0 (0%)
=======================
quickCheckResult revProp

Total number of mutants: 323

Errors: 0 (0%)
Successes (not killed): 7 (2%)
Failures (killed): 316 (97%)
Gaveups: 0 (0%)
=======================
quickCheckResult modelProp

Total number of mutants: 323

Errors: 0 (0%)
Successes (not killed): 1 (0%)
Failures (killed): 322 (99%)
Gaveups: 0 (0%)

-}