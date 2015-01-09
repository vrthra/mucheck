module Test.MuCheck.Utils.Helpers where
import Language.Haskell.Exts.Annotated
import Test.MuCheck.MuOp
import Data.List

class Showx a where
  showx :: a -> String

data X = Module_x (Module_)
       | Decl_x (Decl_)
       | Decl_xs [Decl_]

instance Showx X where
  showx (Module_x m) = "{ " ++  prettyPrint m ++ " }\n"
  showx (Decl_x m) = "{ " ++  prettyPrint m ++ " }\n"
  showx (Decl_xs decls) = unlines $ map (showx . Decl_x) decls

