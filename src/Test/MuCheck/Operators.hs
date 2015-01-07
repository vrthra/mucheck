-- | Available mutation operators
module Test.MuCheck.Operators (comparators,
                          predNums,
                          binAriths,
                          arithLists,
                          allOps) where

import Test.MuCheck.MuOp
import Test.MuCheck.Utils.Common
import Language.Haskell.Exts.Annotated (Name(Symbol), Exp(Var), QName(UnQual), Name(Ident), SrcSpanInfo(..), SrcSpan(..))

l_ :: SrcSpanInfo
l_ = SrcSpanInfo (SrcSpan "" 0 0 0 0) []

-- | all available operators
allOps,comparators,predNums,binAriths,arithLists :: [MuOp]
allOps = concat [comparators, predNums, binAriths, arithLists]

-- | comparison operators ["<", ">", "<=", ">=", "/=", "=="]
comparators = coupling (==>) $ map (Symbol l_) ["<", ">", "<=", ">=", "/=", "=="]

-- | predicates ["pred", "id", "succ"]
predNums = coupling (==>) $ map varfn ["pred", "id", "succ"]

-- | binary arithmetic ["+", "-", "*", "/"]
binAriths = coupling (==>) $ map (Symbol l_) ["+", "-", "*", "/"]

-- | functions on lists ["sum", "product", "maximum", "minimum", "head", "last"]
arithLists = coupling (==>) $ map varfn ["sum", "product", "maximum", "minimum", "head", "last"]

-- | wrapper to produce Function from String
varfn :: String -> Exp SrcSpanInfo
varfn = (Var l_) . (UnQual l_) . (Ident l_)

