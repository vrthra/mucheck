module Examples.AssertCheckTest where

import Test.MuCheck.TestAdapter.AssertCheck

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort l ++ [x] ++ qsort r
    where l = filter (< x) xs
          r = filter (>= x) xs

uncoveredDummy :: Int -> Int
uncoveredDummy a = 0 + a

{-# ANN sortEmpty "Test" #-}
sortEmpty = assertCheck $ qsort [] == []

{-# ANN sortSorted "Test" #-}
sortSorted = assertCheck $ qsort [1,2,3,4] == [1,2,3,4]

{-# ANN sortRev "Test" #-}
sortRev = assertCheck $ qsort [4,3,2,1] == [1,2,3,4]

{-# ANN sortSame "Test" #-}
sortSame = assertCheck $ qsort [1,1,1,1] == [1,1,1,1]

{-# ANN sortNeg "Test" #-}
sortNeg = assertCheck $ qsort [-1,-2,3] == [-2,-1,3]

