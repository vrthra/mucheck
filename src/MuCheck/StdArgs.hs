module MuCheck.StdArgs where

import MuCheck.MuOp
import MuCheck.Operators

data GenerationMode = FirstOrderOnly
                    | FirstAndHigherOrder
                    deriving (Eq, Show)

data StdArgs = StdArgs {muOps :: [MuOp]
                      , doMutatePatternMatches :: Rational
                      , doMutateValues :: Rational
                      , doNegateIfElse :: Rational
                      , doNegateGuards :: Rational
                      , maxNumMutants :: Int
                      , genMode :: GenerationMode }
                      deriving Show

stdArgs :: StdArgs
stdArgs = StdArgs {muOps = allOps
                 , doMutatePatternMatches = 1.0
                 , doMutateValues = 1.0
                 , doNegateIfElse = 1.0
                 , doNegateGuards = 1.0
                 , maxNumMutants = 300
                 , genMode = FirstOrderOnly }

