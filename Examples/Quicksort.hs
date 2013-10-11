module Examples.Quicksort where
import Test.QuickCheck
import Data.List

-- note that for the current code, quickSort_17.hs should be exactly equivalent to the current version of quickSort

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort l ++ [x] ++ qsort r
    where l = filter (< x) xs
          r = filter (>= x) xs

idEmpProp xs = qsort xs == qsort (qsort xs)

revProp xs = qsort xs == qsort (reverse xs)

modelProp xs = qsort xs == sort xs
