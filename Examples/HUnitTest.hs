module Examples.HUnitTest where
import Test.HUnit

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort l ++ [x] ++ qsort r
        where l = filter (<=x) xs
              r = filter (>x) xs

test1 = TestCase (assertEqual "for (qsort [3,2])" [2,3] (qsort [3,2]))

test2 = TestCase (assertEqual "for (qsort [3,2,1])" [1,2,3] (qsort [3,2,1]))

test3 = TestCase (assertEqual "for (qsort [3,2,1,4])" [1,2,3,4] (qsort [3,2,1,4]))

tests = TestList [TestLabel "test1" test1
                , TestLabel "test2" test2
                , TestLabel "test3" test3]