{-# LANGUAGE TupleSections #-}
-- | Common functions used by MuCheck
module Test.MuCheck.Utils.Common where

import System.Random
import Data.List
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad (liftM)
import qualified Data.Hashable as H

-- | The `choose` function generates subsets of a given size
choose :: [a] -> Int -> [[a]]
choose xs n = filter (\x -> length x == n) $ subsequences xs

-- | The `coupling` function produces all possible pairings, and applies the
-- given function to each
coupling :: Eq a => (a -> a -> t) -> [a] -> [t]
coupling fn ops = [fn o1 o2 | o1 <- ops, o2 <- ops, o1 /= o2]

-- | The `replaceFst` function replaces first matching element in a list given old and new values as a pair
replaceFst :: Eq a => (a,a) -> [a] -> [a]
replaceFst _ [] = []
replaceFst (o, n) (v:vs)
  | v == o = n : vs
  | otherwise   = v : replaceFst (o,n) vs

-- | The `sample` function takes a random generator and chooses a random sample
-- subset of given size.
sample :: (RandomGen g) => g -> Int -> [t] -> [t]
sample _ 0 _ = []
sample _ n xs | length xs <= n = xs
sample g n xs = val : sample g' (n - 1) (remElt idx xs)
  where val = xs !! idx
        (idx,g')  = randomR (0, length xs - 1) g

-- | Wrapper around sample providing the random seed
rSample :: Int -> [t] -> IO [t]
rSample n t = do g <- genRandomSeed
                 return $ sample g n t

-- | The `sampleF` function takes a random generator, and a fraction and
-- returns subset of size given by fraction
sampleF :: (RandomGen g) => g -> Rational -> [t] -> [t]
sampleF g f xs = sample g l xs
    where l = round $ f * fromIntegral (length xs)

-- | The `remElt` function removes element at index specified from a list
remElt :: Int -> [a] -> [a]
remElt idx xs = front ++ ack
  where (front,_:ack) = splitAt idx xs

-- | The `swapElts` function swaps two elements in a list given their indices
swapElts :: Int -> Int -> [t] -> [t]
swapElts i j ls = [get k x | (k, x) <- zip [0..length ls - 1] ls]
  where get k x | k == i = ls !! j
                | k == j = ls !! i
                | otherwise = x

-- | The `genSwapped` generates a list of lists where each element has been
-- swapped by another
genSwapped :: [t] -> [[t]]
genSwapped lst = map (\(x:y:_) -> swapElts x y lst) swaplst
  where swaplst = choose [0..length lst - 1] 2

-- | Generate a random seed from the time.
genRandomSeed :: IO StdGen
genRandomSeed = liftM (mkStdGen . round) getPOSIXTime

-- | take a function of two args producing a monadic result, and apply it to
-- a pair
curryM :: (t1 -> t2 -> m t) -> (t1, t2) -> m t
curryM fn (a,b) = fn a b

-- | A simple hash
hash :: String -> String
hash s = (if h < 0 then "x" else "y") ++ show (abs h)
  where h = H.hash s

spread :: (a, [b]) -> [(a, b)]
spread (a,lst) = map (a,) lst

apSnd :: (b -> c) -> (a,b) -> (a,c)
apSnd f (a,b) = (a, f b)

