{-# LANGUAGE RankNTypes #-}
-- | SYB functions
module Test.MuCheck.Utils.Syb (relevantOps, once) where

import Data.Generics (Data, GenericM, gmapMo)
import Test.MuCheck.MuOp (mkMpMuOp, MuOp, same)
import Control.Monad (MonadPlus, mplus)
import Data.Maybe(isJust)

-- | apply a mutating function on a piece of code one at a time
-- like somewhere (from so)
once :: MonadPlus m => GenericM m -> GenericM m
once f x = f x `mplus` gmapMo (once f) x

-- | The function `relevantOps` does two filters. For the first, it
-- removes spurious transformations like "Int 1 ~~> Int 1". Secondly, it
-- tries to apply the transformation to the given program on some element
-- if it does not succeed, then we discard that transformation.
relevantOps :: (Data a, Eq a) => a -> [MuOp] -> [MuOp]
relevantOps m oplst = filter (relevantOp m) $ filter (not . same) oplst
  -- check if an operator can be applied to a program
  where relevantOp m' op = isJust $ once (mkMpMuOp op) m'

