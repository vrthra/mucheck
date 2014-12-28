{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
-- | Module for using Hspec tests
module Test.MuCheck.Run.Hspec where
import qualified Test.Hspec.Core.Runner as Hspec
import Test.MuCheck.Run.Common
import Test.MuCheck.Utils.Print (showA, showAS)

import Data.Typeable
import Data.Either (partitionEithers)
import Data.List((\\))

deriving instance Typeable Hspec.Summary

-- | Summarizable instance of `Hspec.Summary`
instance Summarizable Hspec.Summary where
  testSummary mutantFiles results = TSum {
    tsum_numMutants = r,
    tsum_loadError = e,
    tsum_notKilled = s,
    tsum_killed = f,
    tsum_others = 0,
    tsum_log = logMsg}
      where (errorCases, executedCases) = partitionEithers results
            r = length results
            e = length errorCases
            [successCases, failureCases] = map (\c -> filter (c . snd) executedCases) [isSuccess, isFailure]
            [s,f] = map length [successCases, failureCases]
            errorFiles = mutantFiles \\ map fst executedCases
            logMsg = showAS ["Details:",
                             "Loading error files:", showA errorFiles,
                             "Loading error messages:", showA errorCases,
                             "Successes:", showA successCases,
                             "Failure:", showA failureCases]
            isFailure = not . isSuccess
  isSuccess (Hspec.Summary { Hspec.summaryExamples = se, Hspec.summaryFailures = sf } ) = sf == 0

