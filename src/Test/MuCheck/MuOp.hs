{-#  LANGUAGE Rank2Types, TypeSynonymInstances, FlexibleInstances #-}
-- | Mutation operators
module Test.MuCheck.MuOp (MuOp
          , Mutable(..)
          , (==>*)
          , (*==>*)
          , (~~>)
          , mkMpMuOp
          , same
          , Module_
          , Name_
          , QName_
          , QOp_
          , Exp_
          , Decl_
          , Literal_
          , GuardedRhs_
          , Annotation_
          ) where

import qualified Data.Generics as G
import Control.Monad (MonadPlus, mzero)

import Language.Haskell.Exts.Annotated(Module, Name, QName, QOp, Exp, Decl, Literal, GuardedRhs, Annotation, SrcSpanInfo(..))

type Module_ = Module SrcSpanInfo
type Name_ = Name SrcSpanInfo
type QName_ = QName SrcSpanInfo
type QOp_ = QOp SrcSpanInfo
type Exp_ = Exp SrcSpanInfo
type Decl_ = Decl SrcSpanInfo
type Literal_ = Literal SrcSpanInfo
type GuardedRhs_ = GuardedRhs SrcSpanInfo
type Annotation_ = Annotation SrcSpanInfo


-- | MuOp constructor used to specify mutation transformation
data MuOp = N  (Name_, Name_)
          | QN (QName_, QName_)
          | QO (QOp_, QOp_)
          | E  (Exp_, Exp_)
          | D  (Decl_, Decl_)
          | L  (Literal_, Literal_)
          | G  (GuardedRhs_, GuardedRhs_)
  deriving Eq

-- | Apply the given function on the tuple inside MuOp
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
instance Mutable Name_ where
  (==>) = (N .) . (,)

-- | QName instance for Mutable
instance Mutable QName_ where
  (==>) = (QN .) . (,)

-- | QOp instance for Mutable
instance Mutable QOp_ where
  (==>) = (QO .) . (,)

-- | Exp instance for Mutable
instance Mutable Exp_ where
  (==>) = (E .) . (,)

-- | Exp instance for Mutable
instance Mutable Decl_ where
  (==>) = (D .) . (,)

-- | Literal instance for Mutable
instance Mutable Literal_ where
  (==>) = (L .) . (,)

-- | GuardedRhs instance for Mutable
instance Mutable GuardedRhs_ where
  (==>) = (G .) . (,)

