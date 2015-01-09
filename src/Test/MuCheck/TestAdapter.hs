{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FunctionalDependencies #-}
-- | Module for adapting test framekworks
module Test.MuCheck.TestAdapter where

import qualified Language.Haskell.Interpreter as I
import Data.Typeable
import Test.MuCheck.Config
import Test.MuCheck.Tix

-- | Wrapper for interpreter output
data Summarizable a => InterpreterOutput a = Io {_io :: Either I.InterpreterError a, _ioLog::String}

-- | Holding mutant information
data Mutant = Mutant { _mutant::String, _mtype::MuVar, _mspan::Span}
  deriving (Eq, Show)

toMutant :: (MuVar, Span, String) -> Mutant
toMutant (m,s,str) = Mutant {_mutant = str, _mtype = m, _mspan = s}


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

class TRun a s | a -> s where
  -- | Generate a runnable test string out of passed in module name and
  -- annotated test function name.
  genTest ::  a -> String -> TestStr
  getName :: a -> String
  toRun :: String -> a

  -- | Functions to be used to summarize
  summarize_ :: Summarizable s => a -> Mutant -> TestStr -> InterpreterOutput s -> Summary
  success_ :: Summarizable s => a -> s -> Bool
  failure_ :: Summarizable s => a -> s -> Bool
  other_ :: Summarizable s => a -> s -> Bool

