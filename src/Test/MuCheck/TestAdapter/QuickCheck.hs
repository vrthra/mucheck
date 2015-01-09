{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
-- | Module for using quickcheck properties
module Test.MuCheck.TestAdapter.QuickCheck where
import qualified Test.QuickCheck.Test as Qc
import Test.MuCheck.TestAdapter

import Data.Typeable

deriving instance Typeable Qc.Result
type QuickCheckSummary = Qc.Result

-- | Summarizable instance of `QuickCheck.Result`
instance Summarizable QuickCheckSummary where
  testSummary _mutant _test result = Summary $ _ioLog result
  isSuccess = Qc.isSuccess
  isFailure Qc.Failure{} = True
  isFailure _            = False
  isOther   Qc.GaveUp{}  = True
  isOther   _            = False


data QuickCheckRun = QuickCheckRun String

instance TRun QuickCheckRun QuickCheckSummary where
  genTest _m tstfn = "quickCheckResult " ++ tstfn
  getName (QuickCheckRun str) = str 
  toRun s = QuickCheckRun s

  summarize_ _m = testSummary :: Mutant -> TestStr -> InterpreterOutput QuickCheckSummary -> Summary
  success_ _m = isSuccess :: QuickCheckSummary -> Bool
  failure_ _m = isFailure :: QuickCheckSummary -> Bool
  other_ _m = isOther :: QuickCheckSummary -> Bool

