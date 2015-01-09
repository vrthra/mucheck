{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
-- | Module for using quickcheck properties
module Test.MuCheck.TestAdapter.AssertCheckAdapter where
import Test.MuCheck.TestAdapter.AssertCheck as A
import Test.MuCheck.TestAdapter

import Data.Typeable

-- ----------------------
-- | These are the things you need to do to make a new adapter.
deriving instance Typeable A.AssertStatus
type AssertCheckSummary = A.AssertStatus

-- | Summarizable instance of `AssertCheck.AssertStatus`
instance Summarizable AssertCheckSummary where
  testSummary _mutant _test result = Summary $ _ioLog result
  isSuccess = not . isFailure
  isFailure A.AssertFailure = True
  isFailure A.AssertSuccess = False
  isOther   _               = False


data AssertCheckRun = AssertCheckRun String

instance TRun AssertCheckRun AssertCheckSummary where
  genTest _m tstfn = "assertCheckResult " ++ tstfn
  getName (AssertCheckRun str) = str
  toRun s = AssertCheckRun s

  summarize_ _m = testSummary :: Mutant -> TestStr -> InterpreterOutput AssertCheckSummary -> Summary
  success_ _m = isSuccess :: AssertCheckSummary -> Bool
  failure_ _m = isFailure :: AssertCheckSummary -> Bool
  other_ _m = isOther :: AssertCheckSummary -> Bool

