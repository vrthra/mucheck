module Permutation where

import Test.QuickCheck
import Data.List((\\), nub)

perm :: [a] -> [[a]]
perm [] = []
perm [x] = [[x]]
perm (x:xs) = concatMap (insert x) $ perm xs
    where insert :: a -> [a] -> [[a]]
          insert x [] = [[x]]
          insert x (y:ys) = (x:y:ys) : map (y:) (insert x ys)


xs `setEqual` ys = null (xs \\ ys) && null (ys \\ xs)

fact 0 = 0
fact 1 = 1
fact n = n * fact (n-1)


-- testing Utils.perm
prop_setEqual :: [Int] -> Property
prop_setEqual xs = length xs < 10 && length xs > 0 ==>
                   let xs' = nub xs
                       p = perm xs'
                   in filter (not . setEqual xs') p == []

prop_permLength :: [Int] -> Property
prop_permLength xs = length xs < 10 ==> fact (length xs) == length (perm xs)

prop_permLengthEach :: [Int] -> Property
prop_permLengthEach xs = length xs < 10 && length xs > 0 ==> (length xs) == (head . nub . map length $ perm xs)

prop_permUnique :: [Int] -> Property
prop_permUnique xs = length xs < 8 && length xs > 0 ==>
                     let xs' = nub xs
                         p = perm xs'
                     in p \\ nub p == []

prop_permEquality :: [Int] -> Property
prop_permEquality xs =  length xs < 8 ==>
                        let p1 = perm xs
                            p2 = perm (reverse xs)
                        in p1 \\ p2 == [] && p2 \\ p1 == []