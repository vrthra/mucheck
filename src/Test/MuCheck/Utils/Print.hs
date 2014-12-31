module Test.MuCheck.Utils.Print where
import Debug.Trace
import Data.List(intercalate)

-- | simple wrapper for adding a % at the end.
(./.) :: (Show a, Integral a) => a -> a -> String
n ./. t =  "(" ++ show (n * 100 `div` t) ++ "%)"

-- | join lines together
showAS :: [String] -> String
showAS = intercalate "\n"

-- | make lists into lines in text.
showA :: Show a => [a] -> String
showA =  showAS . map show

tt :: Show a => a -> a
tt v = trace (">" ++ show v) v
