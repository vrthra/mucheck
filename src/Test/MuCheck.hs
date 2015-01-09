{-# LANGUAGE RecordWildCards #-}
-- | MuCheck base module
module Test.MuCheck (mucheck) where

import Control.Monad (liftM)
import Test.MuCheck.Mutation
import Test.MuCheck.Config
import Test.MuCheck.Utils.Common
import Test.MuCheck.Interpreter (evaluateMutants, MutantSummary(..))
import Test.MuCheck.TestAdapter
import Test.MuCheck.AnalysisSummary

-- | Perform mutation analysis using any of the test frameworks that support
-- Summarizable (essentially, after running it on haskell, we should be able to
-- distinguish a successful run without failures from one with failures.)
-- E.g. using the mucheck-quickcheck adapter
--
-- > tFn :: Mutant -> TestStr -> InterpreterOutput QuickCheckSummary`
-- > tFn = testSummary
-- > mucheck tFn "Examples/QuickCheckTest.hs" ["quickCheckResult revProp"]

mucheck :: (Show b, Summarizable b, TRun a b) =>
     a                                                     -- ^ The module we are mutating
  -> FilePath                                              -- ^ The HPC <coverage>.tix file
  -> IO (MAnalysisSummary, [MutantSummary])                -- ^ Returns a tuple of full summary, and individual mutant results.
mucheck moduleFile tix = do
  -- get tix here.
  (len, mutants) <- genMutants (getName moduleFile) tix
  -- Should we do random sample on covering alone or on the full?
  smutants <- sampler defaultConfig mutants
  tests <- getAllTests (getName moduleFile)
  (fsum', msum) <- evaluateMutants moduleFile smutants (map (genTest moduleFile) tests)
  -- set the original size of mutants. (We report the results based on original
  -- number of mutants, not just the covered ones.)
  let fsum = case len of
              -1 -> fsum' {_maOriginalNumMutants = -1, _maCoveredNumMutants = -1}
              _  -> fsum' {_maOriginalNumMutants = len, _maCoveredNumMutants = length mutants}
  return (fsum, msum)

-- | Wrapper around sampleF that returns correct sampling ratios according to
-- configuration passed. TODO: Actually use the sampling configuration.
sampler ::
     Config              -- ^ Configuration
  -> [Mutant]            -- ^ The original list of mutation operators
  -> IO [Mutant]            -- ^ Returns the sampled mutation operators
sampler config mv = do
  ms <- liftM concat $ mapM (getSampled config mv) [MutatePatternMatch,
                                                    MutateValues,
                                                    MutateFunctions,
                                                    MutateNegateIfElse,
                                                    MutateNegateGuards,
                                                    MutateOther []]
  rSample (maxNumMutants config) ms

getSampled :: Config -> [Mutant] -> MuVar -> IO [Mutant]
getSampled config ms muvar = rSampleF (getSample muvar config) $ filter (mutantIs muvar) ms
  where mutantIs mvar Mutant{..} = mvar `similar` _mtype

