module Examples.SmallCheckTest where
import Data.List
import Control.Exception
import Control.Monad
import Test.SmallCheck.Drivers

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

smallCheckResult v = catch (smallCheckM 5 v) handleErr

handleErr :: SomeException -> IO (Maybe PropertyFailure)
handleErr e = return $ Just (PropertyFalse (Just (show e)))

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort l ++ [x] ++ qsort r
    where l = filter (< x) xs
          r = filter (>= x) xs

idEmpProp xs = qsort xs == qsort (qsort xs)

revProp xs = qsort xs == qsort (reverse xs)

modelProp xs = qsort xs == sort xs

