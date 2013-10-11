module Examples.SampleCode where

g x = x + 1

data A = I Int
       | B String
       | C Bool



h 1 = 2
h 2 = 3
h x = x

k x = x + 9

f x = if (x > 0 && x < 10) then if (x > 3) then 1 else 2 else 3

f' x | (x > 0) && (x < 10) = 1
     | (x > 9) = 3
     | (x < 100) = 3
     | otherwise = 2

f'' xs = sum xs