{-# LANGUAGE RecordWildCards #-}
-- | The AnalysisSummary declares the mutation result datatype, and its
-- instances.
module Test.MuCheck.AnalysisSummary where

import Test.MuCheck.Utils.Print

-- | Datatype to hold results of the entire run
data MAnalysisSummary = MAnalysisSummary {
    _maOriginalNumMutants::Int  -- ^ The original number of mutants generated
  , _maCoveredNumMutants::Int   -- ^ The number of mutants that was covered by test cases
  , _maNumMutants::Int     -- ^ The number of mutants tested (after sampling)
  , _maAlive::Int          -- ^ The number of mutants that were alive after the mutation run
  , _maKilled::Int         -- ^ The number of mutants that were killed
  , _maErrors::Int         -- ^ The number of non-viable mutants.
}

-- | MAnalysisSummary to tuple
maSummary :: MAnalysisSummary -> (Int, Int, Int, Int, Int, Int)
maSummary MAnalysisSummary{..} = (_maOriginalNumMutants, _maCoveredNumMutants, _maNumMutants, _maAlive, _maKilled, _maErrors)

-- | The show instance for MAnalysisSummary
instance Show MAnalysisSummary where
  show (MAnalysisSummary{..}) = let noerrors = mnum - _maErrors
                                    mnum = max (max _maOriginalNumMutants _maCoveredNumMutants) _maNumMutants
                                    showx a = if a == -1 then "not provided" else (show a)
    in showAS ["Total mutants: " ++ show (max _maOriginalNumMutants _maNumMutants) ++ " (basis for %)",
               "\tCovered: " ++  showx _maCoveredNumMutants,
               "\tSampled: " ++  show _maNumMutants,
               "\tErrors: " ++  show _maErrors ++ "  "++ _maErrors ./. mnum,
               "\tAlive: " ++  show _maAlive  ++ "/" ++ show noerrors,
               "\tKilled: " ++  show _maKilled ++ "/" ++ show noerrors ++ " " ++ _maKilled ./. noerrors]

