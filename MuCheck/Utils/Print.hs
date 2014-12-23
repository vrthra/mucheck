module MuCheck.Utils.Print where

import Data.List(intersperse)

-- utils for interpreter
showPerCent x = " (" ++ show x ++ "%)"
n `percent` t = 100 * n `div` t

printStringList :: [String] -> String
printStringList = concat . intersperse "\n"

printlnList :: Show a => [a] -> String
printlnList =  printStringList . map show

