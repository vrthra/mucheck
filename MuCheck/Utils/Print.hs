module MuCheck.Utils.Print where

import Data.List(intercalate)

-- utils for interpreter
showPerCent x = " (" ++ show x ++ "%)"
n `percent` t = 100 * n `div` t

printStringList :: [String] -> String
printStringList = intercalate "\n"

printlnList :: Show a => [a] -> String
printlnList =  printStringList . map show

