module MuCheck.Operators where

import MuCheck.MuOp
import Language.Haskell.Exts

-- all available operators
allOps = concat [comparators, predNums, binAriths, arithLists]

-- classes of code elements
comparators = Symbol <$> ["<", ">", "<=", ">=", "/=", "=="]
predNums = Var . UnQual . Ident <$> ["pred", "id", "succ"]
binAriths = Symbol <$> ["+", "-", "*", "/"]
arithLists = Var . UnQual . Ident <$> ["sum", "product", "maximum", "minimum", "head", "last"] 

-- utilities
infixr 0 <$> -- this might not be the right fixity
f <$> ops = [o1 ==> o2 | o1 <- ops', o2 <- ops', o1 /= o2]
  where ops' = map f ops