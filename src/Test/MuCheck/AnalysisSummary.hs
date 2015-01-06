{-# LANGUAGE RecordWildCards #-}
-- | The AnalysisSummary declares the mutation result datatype, and its
-- instances.
module Test.MuCheck.AnalysisSummary where

import Test.MuCheck.Utils.Print

-- | Datatype to hold results of the entire run
data MAnalysisSummary = MAnalysisSummary {
    _maNumMutants::Int     -- ^ The number of mutants tested
  , _maAlive::Int          -- ^ The number of mutants that were alive after the mutation run
  , _maKilled::Int         -- ^ The number of mutants that were killed
  , _maErrors::Int         -- ^ The number of non-viable mutants.
}

-- | The show instance for MAnalysisSummary
instance Show MAnalysisSummary where
  show (MAnalysisSummary{..}) = let noerrors = _maNumMutants - _maErrors
    in showAS ["Total mutants: " ++ show _maNumMutants,
               "\terrors: " ++  show _maErrors ++ "  "++ _maErrors ./. _maNumMutants,
               "\talive: " ++  show _maAlive  ++ "/" ++ show noerrors,
               "\tkilled: " ++  show _maKilled ++ "/" ++ show noerrors ++ " " ++ _maKilled ./. noerrors]

