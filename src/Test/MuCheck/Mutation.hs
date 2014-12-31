{-# LANGUAGE ImpredicativeTypes #-}
-- | Mutation happens here.
module Test.MuCheck.Mutation where

import Language.Haskell.Exts(Literal(Int), Exp(App, Var, If), QName(UnQual),
        Stmt(Qualifier), Module(Module),
        Name(Ident, Symbol), Decl(FunBind, PatBind),
        Pat(PVar), Match(Match), GuardedRhs(GuardedRhs), 
        prettyPrint, fromParseResult, parseFileContents)
import Data.Generics (Data, Typeable, mkMp, listify)
import Data.List(nub, (\\), permutations)
import Control.Monad (liftM, zipWithM)
import System.Random
import Data.Time.Clock.POSIX

import Test.MuCheck.MuOp
import Test.MuCheck.Utils.Syb
import Test.MuCheck.Utils.Common
import Test.MuCheck.Config

-- | The `genMutants` function is a wrapper to genMutantsWith with standard
-- configuraton
genMutants :: String -> FilePath -> IO Int
genMutants = genMutantsWith defaultConfig

-- | The `genMutantsWith` function takes configuration function to mutate,
-- filename the function is defined in, and produces mutants in the same
-- directory as the filename, and returns the number of mutants produced.
genMutantsWith :: Config -> String -> FilePath -> IO Int
genMutantsWith args funcname filename  = liftM length $ do
    ast <- getASTFromFile filename
    g <- liftM (mkStdGen . round) getPOSIXTime
    let f = getFunc funcname ast

        ops, swapOps, valOps, ifElseNegOps, guardedBoolNegOps :: [MuOp]
        ops = relevantOps f (muOps args ++ valOps ++ ifElseNegOps ++ guardedBoolNegOps)
        swapOps = sampleF g (doMutatePatternMatches args) $ permMatches f ++ removeOnePMatch f
        valOps = sampleF g (doMutateValues args) $ selectIntOps f
        ifElseNegOps = sampleF g (doNegateIfElse args) $ selectIfElseBoolNegOps f
        guardedBoolNegOps = sampleF g (doNegateGuards args) $ selectGuardedBoolNegOps f

        patternMatchMutants, ifElseNegMutants, guardedNegMutants, operatorMutants, allMutants :: [Decl]
        allMutants = nub $ patternMatchMutants ++ operatorMutants ++ ifElseNegMutants ++ guardedNegMutants

        patternMatchMutants = mutatesN swapOps f fstOrder
        ifElseNegMutants = mutatesN ifElseNegOps f fstOrder
        guardedNegMutants = mutatesN guardedBoolNegOps f fstOrder
        operatorMutants = case genMode args of
            FirstOrderOnly -> mutatesN ops f fstOrder
            _              -> mutates ops f

        getFunc fname ast' = head $ listify (isFunctionD fname) ast'
        programMutants ast' =  map (putDecls ast) $ mylst ast'
        mylst ast' = [myfn ast' x | x <- take (maxNumMutants args) allMutants]
        myfn ast' fn = replace (getFunc funcname ast', fn) (getDecls ast')

    case ops ++ swapOps of
      [] -> return [] --  putStrLn "No applicable operator exists!"
      _  -> zipWithM writeFile (genFileNames filename) $ map prettyPrint (programMutants ast)
  where fstOrder = 1 -- first order
        getASTFromFile fname = liftM parseModuleFromFile $ readFile fname

-- | Mutating a function's code using a bunch of mutation operators
-- (In all the three mutate functions, we assume working
-- with functions declaration.)
mutates :: [MuOp] -> Decl -> [Decl]
mutates ops m = filter (/= m) $ concatMap (mutatesN ops m) [1..]

-- the third argument specifies whether it's first order or higher order
mutatesN :: [MuOp] -> Decl -> Int -> [Decl]
mutatesN ops ms 1 = concat [mutate op ms | op <- ops ]
mutatesN ops ms c = concat [mutatesN ops m 1 | m <- mutatesN ops ms (c-1)]

-- | Given a function, generate all mutants after applying applying 
-- op once (op might be applied at different places).E.g.:
-- op = "<" ==> ">" and there are two instances of "<"
mutate :: MuOp -> Decl -> [Decl]
mutate op m = once (mkMp' op) m \\ [m]

-- | is the parsed expression the function we are looking for?
isFunctionD :: String -> Decl -> Bool
isFunctionD n (FunBind (Match _ (Ident n') _ _ _ _ : _)) = n == n'
isFunctionD n (FunBind (Match _ (Symbol n') _ _ _ _ : _)) = n == n'
isFunctionD n (PatBind _ (PVar (Ident n')) _ _)          = n == n'
isFunctionD _ _                                  = False

-- | Generate all operators for permutating pattern matches in
-- a function. We don't deal with permutating guards and case for now.
permMatches :: Decl -> [MuOp]
permMatches d@(FunBind ms) = d ==>* map FunBind (permutations ms \\ [ms])
permMatches _  = []

-- | generates transformations that removes one pattern match from a function
-- definition.
removeOnePMatch :: Decl -> [MuOp]
removeOnePMatch (FunBind [_]) = []
removeOnePMatch d@(FunBind ms) = d ==>* map FunBind (removeOneElem ms \\ [ms])
removeOnePMatch _  = []

-- | generate sub-arrays with one less element
removeOneElem :: Eq t => [t] -> [[t]]
removeOneElem l = choose l (length l - 1)

-- AST/module-related operations

-- | Parse a module. Input is the content of the file
parseModuleFromFile :: String -> Module
parseModuleFromFile inp = fromParseResult $ parseFileContents inp

getDecls :: Module -> [Decl]
getDecls (Module _ _ _ _ _ _ decls) = decls

putDecls :: Module -> [Decl] -> Module
putDecls (Module a b c d e f _) decls = Module a b c d e f decls

-- Define all operations on a value
selectValOps :: (Data a, Eq a, Typeable b, Mutable b, Eq b) => (b -> Bool) -> [b -> b] -> a -> [MuOp]
selectValOps predicate fs m = concatMap (\x -> x ==>* map (\f -> f x) fs) vals
  where vals = nub $ listify predicate m

selectValOps' :: (Data a, Eq a, Typeable b, Mutable b) => (b -> Bool) -> (b -> [b]) -> a -> [MuOp]
selectValOps' predicate f m = concatMap (\x -> x ==>* f x) vals
  where vals = listify predicate m

selectIntOps :: (Data a, Eq a) => a -> [MuOp]
selectIntOps m = selectValOps isInt [
      \(Int i) -> Int (i + 1),
      \(Int i) -> Int (i - 1),
      \(Int i) -> if abs i /= 1 then Int 0 else Int i,
      \(Int i) -> if abs (i-1) /= 1 then Int 1 else Int i] m
  where isInt (Int _) = True
        isInt _       = False

-- | negating boolean in if/else statements
selectIfElseBoolNegOps :: (Data a, Eq a) => a -> [MuOp]
selectIfElseBoolNegOps m = selectValOps isIf [\(If e1 e2 e3) -> If (App (Var (UnQual (Ident "not"))) e1) e2 e3] m
  where isIf If{} = True
        isIf _    = False

-- | negating boolean in Guards
selectGuardedBoolNegOps :: (Data a, Eq a) => a -> [MuOp]
selectGuardedBoolNegOps m = selectValOps' isGuardedRhs negateGuardedRhs m
  where isGuardedRhs GuardedRhs{} = True
        boolNegate e@(Qualifier (Var (UnQual (Ident "otherwise")))) = [e]
        boolNegate (Qualifier expr) = [Qualifier (App (Var (UnQual (Ident "not"))) expr)]
        boolNegate x = [x]
        negateGuardedRhs (GuardedRhs srcLoc stmts expr) = [GuardedRhs srcLoc s expr | s <- once (mkMp boolNegate) stmts]

