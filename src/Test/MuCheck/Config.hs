module Test.MuCheck.Config where

import Test.MuCheck.MuOp
import Test.MuCheck.Operators

data GenerationMode
  = FirstOrderOnly
  | FirstAndHigherOrder
  deriving (Eq, Show)

data Config = Config { muOps :: [MuOp]
  , doMutatePatternMatches :: Rational
  , doMutateValues :: Rational
  , doNegateIfElse :: Rational
  , doNegateGuards :: Rational
  , maxNumMutants :: Int
  , genMode :: GenerationMode }
  deriving Show

defaultConfig :: Config
defaultConfig = Config {muOps = allOps
  , doMutatePatternMatches = 1.0
  , doMutateValues = 1.0
  , doNegateIfElse = 1.0
  , doNegateGuards = 1.0
  , maxNumMutants = 300
  , genMode = FirstOrderOnly }

