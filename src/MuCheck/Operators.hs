module MuCheck.Operators (comparators,
                          predNums,
                          binAriths,
                          arithLists,
                          allOps) where

import MuCheck.MuOp
import MuCheck.Utils.Common
import Language.Haskell.Exts (Name(Symbol), Exp(Var), QName(UnQual), Name(Ident))

-- | all available operators
allOps = concat [comparators, predNums, binAriths, arithLists]

-- | comparison operators
comparators = coupling (==>) $ map Symbol ["<", ">", "<=", ">=", "/=", "=="]

-- | predicates
predNums = coupling (==>) $ map varfn ["pred", "id", "succ"]

-- | binary arithmetic
binAriths = coupling (==>) $ map Symbol ["+", "-", "*", "/"]

-- | arithmetic on lists
arithLists = coupling (==>) $ map varfn ["sum", "product", "maximum", "minimum", "head", "last"]

-- utilities
varfn = Var . UnQual . Ident

