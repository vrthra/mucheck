module MuCheck.Utils.Common where

import System.FilePath (splitExtension)
import System.Random
import Data.List
import Control.Applicative

-- | The `choose` function generates subsets of a given size
choose :: [b] -> Int -> [[b]]
_      `choose` 0       = [[]]
[]     `choose` _       =  []
(x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

-- | The `genFileNames` function lazily generates filenames of mutants
genFileNames :: String -> [String]
genFileNames s =  map newname [1..]
    where (name, ext) = splitExtension s
          newname i= name ++ "_" ++ show i ++ ext

-- | The `replace` function replaces first element in a list given old and new values as a pair
replace :: Eq a => (a,a) -> [a] -> [a]
replace (o,n) lst = map replaceit lst
  where replaceit v
          | v == o = n
          | otherwise = v

-- | The `safeHead` function safely extracts head of a list using Maybe
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- | The `sample` function takes a random generator and chooses a random sample
-- subset of given size.
sample :: (RandomGen g, Num n, Eq n) => g -> n -> [t] -> [t]
sample g 0 xs = []
sample g n xs = val : (sample g' (n - 1) (remElt idx xs))
  where val = xs !! idx
        (idx,g')  = randomR (0, length xs - 1) g

-- | The `remElt` function removes element at index specified from a list
remElt idx xs = front ++ ack
  where (front,b:ack) = (splitAt idx xs)

-- | The `sampleF` function takes a random generator, and a fraction and
-- returns subset of size given by fraction
sampleF :: (RandomGen g, Num n) => g -> Rational -> [t] -> [t]
sampleF g f xs = sample g l xs
    where l = round ((fromIntegral (length xs)) * f)
