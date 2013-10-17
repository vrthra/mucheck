module MuCheck.StdArgs where

import MuCheck.MuOp
import MuCheck.Operators

data GenerationMode = FirstOrderOnly
                    | FirstAndHigherOrder
                    deriving (Eq, Show)

data StdArgs = StdArgs {muOps :: [MuOp]
                      , doMutatePatternMatches :: Bool
                      , doMutateValues :: Bool
                      , doNegateIfElse :: Bool
                      , doNegateGuards :: Bool
                      , maxNumMutants :: Int
                      , genMode :: GenerationMode }

stdArgs :: StdArgs
stdArgs = StdArgs {muOps = allOps
                 , doMutatePatternMatches = True -- False
                 , doMutateValues = True
                 , doNegateIfElse = True
                 , doNegateGuards = True
                 , maxNumMutants = 300
                 , genMode = FirstOrderOnly }
