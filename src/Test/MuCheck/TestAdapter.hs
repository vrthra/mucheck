-- | Module for adapting test framekworks
module Test.MuCheck.TestAdapter where

import qualified Language.Haskell.Interpreter as I
import Data.Typeable

-- | Wrapper for interpreter output
type InterpreterOutput a = Either I.InterpreterError (String, a)

-- | Filename of Mutant
type MutantFilename = String

-- | Summary of test run
newtype Summary = Summary String

-- | Interface to be implemented by a test framework
class Typeable s => Summarizable s where
  -- | Summary of a test run
  testSummary :: [MutantFilename] -> [InterpreterOutput s] -> Summary
  -- | Was the test run a success
  isSuccess :: s -> Bool
  -- | Was the test run a failure
  isFailure :: s -> Bool
  isFailure = not . isSuccess
  -- | Was the test run neither (gaveup/timedout)
  isOther :: s -> Bool
  isOther x = not (isSuccess x) && not (isFailure x)

