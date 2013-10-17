module MuCheck.Operators where

import MuCheck.MuOp
import Language.Haskell.Exts
import Data.Generics

-- all available operators
allOps = concat [comparators, predNums, binAriths, arithLists]

-- classes of code elements
comparators = Symbol <$> ["<", ">", "<=", ">=", "/=", "=="]
predNums = Var . UnQual . Ident <$> ["pred", "id", "succ"]
binAriths = Symbol <$> ["+", "-", "*", "/"]
arithLists = Var . UnQual . Ident <$> ["sum", "product", "maximum", "minimum", "head", "last"] 

-- utilities
infixr 0 <$> -- this might not be the right fixity
f <$> ops = let ops' = map f ops
            in [o1 ==> o2 | o1 <- ops', o2 <- ops', o1 /= o2]

-- Legacy Code
-- to be removed soon
op2 = (List []) ==> List [Lit (Int 3)]
ops = (List []) ==>* [List [Lit (Int 4)], List [Lit (Int 5)], List [Lit (Int 6)], List [Lit (Int 7)]]