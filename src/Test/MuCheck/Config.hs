-- | Configuration module
module Test.MuCheck.Config where

import Test.MuCheck.MuOp

-- | The knob controlling if we want first order mutation.
data GenerationMode
  = FirstOrderOnly
  | FirstAndHigherOrder
  deriving (Eq, Show)

-- | The configuration options
-- if 1 is provided, all mutants are selected for that kind, and 0 ensures that
-- no mutants are picked for that kind. Any fraction in between causes that
-- many mutants to be picked randomly from the available pool

data FnType = FnSymbol | FnIdent
  deriving (Eq, Show)
data FnOp = FnOp {_type :: FnType, _fns :: [String]}
  deriving (Eq, Show)


-- | predicates ["pred", "id", "succ"]
predNums :: [String]
predNums = ["pred", "id", "succ"]

-- | functions on lists ["sum", "product", "maximum", "minimum", "head", "last"]
arithLists :: [String]
arithLists = ["sum", "product", "maximum", "minimum", "head", "last"]


-- | comparison operators ["<", ">", "<=", ">=", "/=", "=="]
comparators :: [String]
comparators = ["<", ">", "<=", ">=", "/=", "=="]

-- | binary arithmetic ["+", "-", "*", "/"]
binAriths :: [String]
binAriths = ["+", "-", "*", "/"]

data Config = Config {
-- | Mutation operators on operator or function replacement
  muOp :: [FnOp]
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
-- | Mutates operators and functions, that is
--
-- > i + 1
--
-- becomes
--
-- > i - 1
--
-- > i * 1
--
-- > i / 1
  , doMutateFunctions :: Rational
-- | negate if conditions, that is
--
-- > if True then 1 else 0
--
-- becomes
--
-- > if True then 0 else 1
  , doNegateIfElse :: Rational
-- | negate guarded booleans in guarded definitions
--
-- > myFn x | x == 1 = True
-- > myFn   | otherwise = False
--
-- becomes
--
-- > myFn x | not (x == 1) = True
-- > myFn   | otherwise = False
  , doNegateGuards :: Rational
-- | Maximum number of mutants to generate.
  , maxNumMutants :: Int
-- | Generation mode, can be traditional (firstOrder) and
-- higher order (higher order is experimental)
  , genMode :: GenerationMode }
  deriving Show

-- | The default configuration
defaultConfig :: Config
defaultConfig = Config {
    muOp = [
      FnOp {_type=FnIdent, _fns= predNums},FnOp {_type=FnIdent, _fns= arithLists},
      FnOp {_type=FnSymbol, _fns= comparators},FnOp {_type=FnSymbol, _fns=binAriths}]
  , doMutatePatternMatches = 1.0
  , doMutateValues = 1.0
  , doMutateFunctions = 1.0
  , doNegateIfElse = 1.0
  , doNegateGuards = 1.0
  , maxNumMutants = 300
  , genMode = FirstOrderOnly }

-- | Enumeration of different kinds of mutations
data MuVars = MutatePatternMatch
            | MutateValues
            | MutateFunctions
            | MutateNegateIfElse
            | MutateNegateGuards
            | MutateOther String
  deriving (Eq, Show)

-- | getSample returns the fraction in config corresponding to the enum passed
-- in
getSample :: MuVars -> Config -> Rational
getSample MutatePatternMatch c = doMutatePatternMatches c
getSample MutateValues       c = doMutateValues c
getSample MutateFunctions    c = doMutateFunctions c
getSample MutateNegateIfElse c = doNegateIfElse c
getSample MutateNegateGuards c = doNegateGuards c
getSample MutateOther{} c = 1

