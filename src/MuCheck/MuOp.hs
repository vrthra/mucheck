{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module MuCheck.MuOp (MuOp
          , (==>)
          , (==>*)
          , (*==>*)
          , (~~>)
          , mkMp'
          , Mutable
          ) where

import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty
import Data.Generics
import Control.Monad

data MuOp = N  (Name, Name)
          | QN (QName, QName)
          | QO (QOp, QOp)
          | E  (Exp, Exp)
          | D  (Decl, Decl)
          | L  (Literal, Literal)
          | G  (GuardedRhs, GuardedRhs)

-- boilerplate code
mkMp' (N (s,t))  = mkMp (s ~~> t)
mkMp' (QN (s,t)) = mkMp (s ~~> t)
mkMp' (QO (s,t)) = mkMp (s ~~> t)
mkMp' (E (s,t))  = mkMp (s ~~> t)
mkMp' (D (s,t))  = mkMp (s ~~> t)
mkMp' (L (s,t))  = mkMp (s ~~> t)
mkMp' (G (s,t))  = mkMp (s ~~> t)

showM (s, t) = "\n" ++ show s ++ " ==> " ++ show t
instance Show MuOp where
    show (N a)  = showM a
    show (QN a) = showM a
    show (QO a) = showM a
    show (E a)  = showM a
    show (D a)  = showM a
    show (L a)  = showM a
    show (G a)  = showM a

-- end boilerplate code

class Mutable a where
    (==>) :: a -> a -> MuOp

(==>*) :: Mutable a => a -> [a] -> [MuOp]
(==>*) x = map (x ==>)

(*==>*) :: Mutable a => [a] -> [a] -> [MuOp]
xs *==>* ys = concatMap (flip (==>*) ys) xs

(~~>) :: (MonadPlus m, Eq a) => a -> a -> (a -> m a)
x ~~> y = \z -> if z == x && x /= y then return y else mzero

-- instances

instance Mutable Name where
    (==>) = (N .) . (,)
    
instance Mutable QName where
    (==>) = (QN .) . (,)
    
instance Mutable QOp where
    (==>) = (QO .) . (,)

instance Mutable Exp where
    (==>) = (E .) . (,)
    
instance Mutable Decl where
    (==>) = (D .) . (,)
    
instance Mutable Literal where
    (==>) = (L .) . (,)
    
instance Mutable GuardedRhs where
    (==>) = (G .) . (,)