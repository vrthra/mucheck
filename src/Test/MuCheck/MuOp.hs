{-#  LANGUAGE Rank2Types #-}
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

apply :: (forall a. (Eq a, G.Typeable a, Show a) => (a,a) -> c) -> MuOp -> c
apply f (N  m) = f m
apply f (QN m) = f m
apply f (QO m) = f m
apply f (E  m) = f m
apply f (D  m) = f m
apply f (L  m) = f m
apply f (G  m) = f m

-- | The function `same` applies on a `MuOP` determining if transformation is
-- between same values.
same :: MuOp -> Bool
same = apply $ uncurry (==)

-- | A wrapper over mkMp
mkMpMuOp :: (MonadPlus m, G.Typeable a) => MuOp -> a -> m a
mkMpMuOp = apply $ G.mkMp . uncurry (~~>)

-- | Show a specified mutation
showM :: (Show a1, Show a) => (a, a1) -> String
showM (s, t) = "\n" ++ show s ++ " ==> " ++ show t

-- | MuOp instance for Show
instance Show MuOp where
  show = apply showM

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

