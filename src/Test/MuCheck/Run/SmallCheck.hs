{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
-- | Module for using quickcheck properties
module Test.MuCheck.Run.SmallCheck where
import qualified Test.SmallCheck.Drivers as Sc
import qualified Test.SmallCheck
import Test.MuCheck.Run.Common
import Test.MuCheck.Utils.Print (showA, showAS)

import Data.Typeable
import Data.List((\\))
import Data.Either (partitionEithers)

type SmallSummary = Maybe Sc.PropertyFailure
deriving instance Typeable Sc.PropertyFailure

-- | Summarizable instance of `SmallSummary`
instance Summarizable SmallSummary where
  testSummary mutantFiles results = TSum {
    tsum_numMutants = r,
    tsum_loadError = e,
    tsum_notKilled = s,
    tsum_killed = f,
    tsum_others = 0,
    tsum_log = logMsg}
    where (errorCases, executedCases) = partitionEithers results
          [successCases, failureCases] = map (\c -> filter (c . snd) executedCases) [isSuccess, isFailure]
          r = length results
          e = length errorCases
          [s,f] = map length [successCases, failureCases]
          errorFiles = mutantFiles \\ map fst executedCases
          logMsg = showAS ["Details:",
                           "Loading error files:", showA errorFiles,
                           "Loading error messages:", showA errorCases,
                           "Successes:", showA successCases,
                           "Failure:", showA failureCases]
          isFailure :: SmallSummary -> Bool
          isFailure = not . isSuccess
  isSuccess Nothing = True
  isSuccess _       = False
