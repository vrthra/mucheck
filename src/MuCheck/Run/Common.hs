module MuCheck.Run.Common where

import qualified Language.Haskell.Interpreter as I
import Data.Typeable
type InterpreterOutput a = Either I.InterpreterError (String, a)

data TSum = TSum {tsum_numMutants::Int,
                  tsum_loadError::Int,
                  tsum_notKilled::Int,
                  tsum_killed::Int,
                  tsum_others::Int,
                  tsum_log::String}

data TSSum = TSSum {tssum_numMutants::Int,
                    tssum_alive::Int,
                    tssum_errors::Int,
                    tssum_log::String}

-- Class/Instance declaration
type MutantFilename = String
class Typeable s => Summarizable s where
  testSummary :: [MutantFilename] -> [InterpreterOutput s] -> TSum
  isSuccess :: s -> Bool

