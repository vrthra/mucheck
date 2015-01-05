{-# LANGUAGE DeriveDataTypeable #-}
-- | Module for adapting test framekworks
module Test.MuCheck.TestAdapter where

import qualified Language.Haskell.Interpreter as I
import Data.Typeable

-- | Wrapper for interpreter output
data Summarizable a => InterpreterOutput a = Io {_io :: Either I.InterpreterError a, _ioLog::String}

-- | Holding mutant information
type Mutant = String

-- | Holding test information
type TestStr = String

-- | Summary of test run
newtype Summary = Summary String
  deriving (Show, Typeable)

-- | Interface to be implemented by a test framework
class Typeable s => Summarizable s where
  -- | Summary of test suite on a single mutant
  testSummary :: Mutant -> TestStr -> InterpreterOutput s -> Summary
  -- | Was the test run a success
  isSuccess :: s -> Bool
  -- | Was the test run a failure
  isFailure :: s -> Bool
  isFailure = not . isSuccess
  -- | Was the test run neither (gaveup/timedout)
  isOther :: s -> Bool
  isOther x = not (isSuccess x) && not (isFailure x)

