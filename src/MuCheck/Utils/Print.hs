module MuCheck.Utils.Print where

import Data.List(intercalate)

-- | simple wrapper for adding a % at the end.
n ./. t =  "(" ++ show (n * 100 `div` t) ++ "%)"

-- | join lines together
showAS :: [String] -> String
showAS = intercalate "\n"

-- | make lists into lines in text.
showA :: Show a => [a] -> String
showA =  showAS . map show

