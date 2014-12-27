{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
module Test.MuCheck.Run.QuickCheck where
import qualified Test.QuickCheck.Test as Qc
import Test.MuCheck.Run.Common
import Test.MuCheck.Utils.Print (showA, showAS)

import Data.Typeable
import Data.List((\\))
import Data.Either (partitionEithers)

deriving instance Typeable Qc.Result

instance Summarizable Qc.Result where
  testSummary mutantFiles results = TSum {
    tsum_numMutants = r,
    tsum_loadError = e,
    tsum_notKilled = s,
    tsum_killed = f,
    tsum_others = g,
    tsum_log = logMsg}
    where (errorCases, executedCases) = partitionEithers results
          [successCases, failureCases, gaveUpCases] = map (\c -> filter (c . snd) executedCases) [isSuccess, isFailure, isGaveUp]
          r = length results
          e = length errorCases
          [s,f,g] = map length [successCases, failureCases, gaveUpCases]
          errorFiles = mutantFiles \\ map fst executedCases
          logMsg = showAS ["Details:",
                           "Loading error files:", showA errorFiles,
                           "Loading error messages:", showA errorCases,
                           "Successes:", showA successCases,
                           "Failure:", showA failureCases,
                           "Gaveups:", showA gaveUpCases]
          isFailure :: Qc.Result -> Bool
          isFailure Qc.Failure{} = True
          isFailure _         = False
          isGaveUp :: Qc.Result -> Bool
          isGaveUp Qc.GaveUp{} = True
          isGaveUp _        = False
  isSuccess = Qc.isSuccess
