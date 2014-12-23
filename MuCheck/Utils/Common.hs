module MuCheck.Utils.Common where

-- chooose [1,2,3,4,5] 4
--  = [[2,3,4,5],[1,3,4,5],[1,2,4,5],[1,2,3,5],[1,2,3,4]]
choose :: [b] -> Int -> [[b]]
_      `choose` 0       = [[]]
[]     `choose` _       =  []
(x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

-- generating mutant files names
-- e.g.: "Quicksort.hs" ==> "Quicksort_1.hs", "Quicksort_2.hs", etc.
genFileNames :: String -> [String]
genFileNames s =  map newname [1..]
    where (name, ext) = splitAt (length s - 3) s
          newname i= name ++ "_" ++ (show i) ++ ext

