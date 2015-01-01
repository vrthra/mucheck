-- | Mutation operators
module Test.MuCheck.MuOp (MuOp
          , Mutable(..)
          , (==>*)
          , (*==>*)
          , (~~>)
          , mkMpMuOp
          , same
          ) where

import Language.Haskell.Exts (Name, QName, QOp, Exp, Literal, GuardedRhs, Decl)
import qualified Data.Generics as G
import Control.Monad (MonadPlus, mzero)

-- | MuOp constructor used to specify mutation transformation
data MuOp = N  (Name, Name)
          | QN (QName, QName)
          | QO (QOp, QOp)
          | E  (Exp, Exp)
          | D  (Decl, Decl)
          | L  (Literal, Literal)
          | G  (GuardedRhs, GuardedRhs)
  deriving Eq

-- boilerplate code

-- | The function `same` applies on a `MuOP` determining if transformation is
-- between same values.
same :: MuOp -> Bool
same (N (a,b)) = a == b
same (QN (a,b)) = a == b
same (QO (a,b)) = a == b
same (E (a,b)) = a == b
same (D (a,b)) = a == b
same (L (a,b)) = a == b
same (G (a,b)) = a == b

-- | A wrapper over mkMp
mkMpMuOp :: (MonadPlus m, G.Typeable a) => MuOp -> a -> m a
mkMpMuOp (N (s,t))  = G.mkMp (s ~~> t)
mkMpMuOp (QN (s,t)) = G.mkMp (s ~~> t)
mkMpMuOp (QO (s,t)) = G.mkMp (s ~~> t)
mkMpMuOp (E (s,t))  = G.mkMp (s ~~> t)
mkMpMuOp (D (s,t))  = G.mkMp (s ~~> t)
mkMpMuOp (L (s,t))  = G.mkMp (s ~~> t)
mkMpMuOp (G (s,t))  = G.mkMp (s ~~> t)

-- | Show a specified mutation
showM :: (Show a1, Show a) => (a, a1) -> String
showM (s, t) = "\n" ++ show s ++ " ==> " ++ show t

-- | MuOp instance for Show
instance Show MuOp where
  show (N a)  = showM a
  show (QN a) = showM a
  show (QO a) = showM a
  show (E a)  = showM a
  show (D a)  = showM a
  show (L a)  = showM a
  show (G a)  = showM a

-- end boilerplate code

-- | Mutation operation representing translation from one fn to another fn.
class Mutable a where
  (==>) :: a -> a -> MuOp

-- | The function `==>*` pairs up the given element with all elements of the
-- second list, and applies `==>` on them.
(==>*) :: Mutable a => a -> [a] -> [MuOp]
(==>*) x lst = map (x ==>) lst

-- | The function `*==>*` pairs up all elements of first list with all elements
-- of second list and applies `==>` between them.
(*==>*) :: Mutable a => [a] -> [a] -> [MuOp]
xs *==>* ys = concatMap (==>* ys) xs

-- | The function `~~>` accepts two values, and returns a function
-- that if given a value equal to first, returns second
-- we handle x ~~> x separately
(~~>) :: (MonadPlus m, Eq a) => a -> a -> a -> m a
x ~~> y = \z -> if z == x then return y else mzero

-- | Name instance for Mutable
instance Mutable Name where
  (==>) = (N .) . (,)

-- | QName instance for Mutable
instance Mutable QName where
  (==>) = (QN .) . (,)

-- | QOp instance for Mutable
instance Mutable QOp where
  (==>) = (QO .) . (,)

-- | Exp instance for Mutable
instance Mutable Exp where
  (==>) = (E .) . (,)

-- | Exp instance for Mutable
instance Mutable Decl where
  (==>) = (D .) . (,)

-- | Literal instance for Mutable
instance Mutable Literal where
  (==>) = (L .) . (,)

-- | GuardedRhs instance for Mutable
instance Mutable GuardedRhs where
  (==>) = (G .) . (,)

