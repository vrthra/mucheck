-- | Helper module for easier visualization
module Test.MuCheck.Utils.Helpers where
import Language.Haskell.Exts.Annotated
import Test.MuCheck.MuOp

-- | Class to allow easier visualization of values without munging `show`
class Showx a where
  showx :: a -> String

-- | Temporary holder for easier visualization
data X = Module_x (Module_)
       | Decl_x (Decl_)
       | Decl_xs [Decl_]

-- | showx instances
instance Showx X where
  showx (Module_x m) = "{ " ++  prettyPrint m ++ " }\n"
  showx (Decl_x m) = "{ " ++  prettyPrint m ++ " }\n"
  showx (Decl_xs decls) = unlines $ map (showx . Decl_x) decls

