module MuCheck.Utils.Print where

import Data.List(intercalate)

-- utils for interpreter
showPerCent x = "(" ++ show x ++ "%)"
n `percent` t = 100 * n `div` t

showAS :: [String] -> String
showAS = intercalate "\n"

showA :: Show a => [a] -> String
showA =  showAS . map show

