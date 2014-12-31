module Test.MuCheck.TestAdapter where

import qualified Language.Haskell.Interpreter as I
import Data.Typeable
type InterpreterOutput a = Either I.InterpreterError (String, a)

-- Class/Instance declaration
type MutantFilename = String
newtype Summary = Summary String
class Typeable s => Summarizable s where
  testSummary :: [MutantFilename] -> [InterpreterOutput s] -> Summary
  isSuccess :: s -> Bool
  isFailure :: s -> Bool
  isFailure = not . isSuccess
  isOther :: s -> Bool
  isOther x = not (isSuccess x) && not (isFailure x)

