-- | Configuration module
module Test.MuCheck.Config where

import Test.MuCheck.MuOp
import Test.MuCheck.Operators (allOps)

-- | The knob controlling if we want first order mutation.
data GenerationMode
  = FirstOrderOnly
  | FirstAndHigherOrder
  deriving (Eq, Show)

-- | The configuration options
-- if 1 is provided, all mutants are selected for that kind, and 0 ensures that
-- no mutants are picked for that kind. Any fraction in between causes that
-- many mutants to be picked randomly from the available pool

data Config = Config {
-- | Mutation operators on operator or function replacement
  muOps :: [MuOp]
-- | Mutate pattern matches for functions?
-- for example
--
-- > first [] = Nothing
-- > first (x:_) = Just x
--
-- is mutated to
--
-- > first (x:_) = Just x
-- > first [] = Nothing
  , doMutatePatternMatches :: Rational
-- | Mutates integer values by +1 or -1 or by replacing it with 0 or 1
  , doMutateValues :: Rational
-- | negate if conditions, that is
--
-- > if True then 1 else 0
--
-- becomes
--
-- > if (not True) then 1 else 0
  , doNegateIfElse :: Rational
-- | negate guarded booleans in case statements, that is
--
-- > case v of
-- >   True -> 1
-- >   otherwise -> 0
--
-- becomes
--
-- > case v of
-- >  (not True) -> 1
-- >  otherwise -> 0
  , doNegateGuards :: Rational
-- | Maximum number of mutants to generate.
  , maxNumMutants :: Int
-- | Generation mode, can be traditional (firstOrder) and
-- higher order (higher order is experimental)
  , genMode :: GenerationMode }
  deriving Show

-- | The default configuration
defaultConfig :: Config
defaultConfig = Config {muOps = allOps
  , doMutatePatternMatches = 1.0
  , doMutateValues = 1.0
  , doNegateIfElse = 1.0
  , doNegateGuards = 1.0
  , maxNumMutants = 300
  , genMode = FirstOrderOnly }

