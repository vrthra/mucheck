{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

module MuCheck.Utils.Syb ( selectMany
                     , selectOne
                     , relevantOps
                     , once
                     , once') where

import Data.Generics (Data, Typeable, GenericM, gmapMo, everything, mkQ, mkMp)
import MuCheck.MuOp
import Control.Monad (MonadPlus, mplus)
import Data.Maybe(fromMaybe, isJust)

-- SYB functions
-- apply a mutating function on a piece of code once at a time
once :: MonadPlus m => GenericM m -> GenericM m
once f x = f x `mplus` gmapMo (once f) x

once' :: (forall a. Data a => a -> Maybe a) -> (forall a. Data a => a -> a)
once' f x = fromMaybe x $ once f x

-- select all code components satisfying a certain predicate
selectMany :: (Data a, Typeable b) => (b -> Bool) -> a -> [b]
selectMany f = everything (++) ([] `mkQ` keep f)
   where keep fn x = [x | fn x]

-- special case of selectMany, which selects the first
-- components satisfying a predicate
selectOne f p = case selectMany f p of
                    [] -> Nothing
                    xs -> Just $ head xs

-- selecting all relevant ops
relevantOps :: (Data a, Eq a) => a -> [MuOp] -> [MuOp]
relevantOps m = filter (relevantOp m)
  -- check if an operator can be applied to a program
  where relevantOp :: (Data a, Eq a) => a -> MuOp -> Bool
        relevantOp m op = isJust $once (mkMp' op) m

