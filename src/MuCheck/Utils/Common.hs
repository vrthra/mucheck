module MuCheck.Utils.Common where

import System.FilePath (splitExtension)

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

