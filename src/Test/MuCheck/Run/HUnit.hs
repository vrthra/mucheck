{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
-- | Module for using HUnit tests
module Test.MuCheck.Run.HUnit where
import qualified Test.HUnit as HUnit
import Test.MuCheck.Run.Common
import Test.MuCheck.Utils.Print (showA, showAS)

import Data.Typeable
import Data.List((\\))
import Data.Either (partitionEithers)

deriving instance Typeable HUnit.Counts

instance Summarizable HUnit.Counts where
  testSummary mutantFiles results = TSum {
    tsum_numMutants = r,
    tsum_loadError = le,
    tsum_notKilled = s,
    tsum_killed = fl,
    tsum_others = re + ftc,
    tsum_log = logMsg}
    where (loadingErrorCases, executedCases) = partitionEithers results
          loadingErrorFiles = mutantFiles \\ map fst executedCases
          successCases = filter (isSuccess . snd) executedCases
          failuresCases = filter ((>0) . HUnit.failures . snd) executedCases
          runningErrorCases = filter ((>0) . HUnit.errors . snd) executedCases \\ failuresCases
          failToFullyTryCases = filter ((\c -> HUnit.cases c > HUnit.tried c) . snd) executedCases
          r = length results
          le = length loadingErrorCases
          [s, fl, re, ftc] = map length [successCases, failuresCases, runningErrorCases, failToFullyTryCases]
          logMsg = showAS ["Details:",
                           "Loading error files:",showA loadingErrorFiles,
                           "Loading error messages:",showA loadingErrorCases,
                           "Successes:", showA successCases,
                           "Failures:", showA failuresCases,
                           "Error while running:", showA runningErrorCases,
                           "Incompletely tested (may include failures and running errors):",showA failToFullyTryCases]
  isSuccess c = (HUnit.cases c == HUnit.tried c) && HUnit.failures c == 0 && HUnit.errors c == 0

