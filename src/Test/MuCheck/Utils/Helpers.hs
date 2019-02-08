-- | Helper module for easier visualization
module Test.MuCheck.Utils.Helpers where
import Language.Haskell.Exts
import Test.MuCheck.MuOp

-- | Class to allow easier visualization of values without munging `show`
class Showx a where
  showx :: a -> String

-- | Temporary holder for easier visualization
data X = ModuleX Module_
       | DeclX Decl_
       | DeclXs [Decl_]

-- | showx instances
instance Showx X where
  showx (ModuleX m) = "{ " ++  prettyPrint m ++ " }\n"
  showx (DeclX m) = "{ " ++  prettyPrint m ++ " }\n"
  showx (DeclXs decls) = unlines $ map (showx . DeclX) decls

