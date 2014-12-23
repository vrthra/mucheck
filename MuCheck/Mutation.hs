{-# LANGUAGE ImpredicativeTypes #-}
-- Mutation happens here.

module MuCheck.Mutation where

import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty
import Data.Maybe
import Data.Generics
import Data.List(elemIndex,nub,(\\), intersperse, permutations)
import Control.Monad.State
import MuCheck.MuOp
import MuCheck.Utils
import MuCheck.Operators
import MuCheck.StdArgs

-- entry point.
genMutants = genMutantsWith stdArgs
genMutantsWith = genMutantsWithFstIdx 1
genMutantsFstIdx fstIdx = genMutantsWithFstIdx fstIdx stdArgs

genMutantsWithFstIdx :: Int -> StdArgs -> String -> FilePath -> IO Int
genMutantsWithFstIdx fstIdx args funcname filename  = do
                ast <- getASTFromFile filename
                let decls = getDecls ast
                    func = fromJust $ selectOne (isFunctionD funcname) ast
                    valOps = if (doMutateValues args) then (selectIntOps func) else []
                    ifElseNegOps = if (doNegateIfElse args) then (selectIfElseBoolNegOps func) else []
                    guardedBoolNegOps = if (doNegateGuards args) then (selectGuardedBoolNegOps func) else []
                    swapOps = if (doMutatePatternMatches args) then (permMatches func ++ removeOnePMatch func) else []
                    ops = relevantOps func (muOps args ++ valOps)
                    patternMatchMutants = mutatesN swapOps func 1
                    ifElseNegMutants = mutatesN ifElseNegOps func 1
                    guardedNegMutants = mutatesN guardedBoolNegOps func 1
                    operatorMutants = if (genMode args == FirstOrderOnly) then (mutatesN ops func 1) else (mutates ops func)
                    allMutants = take (maxNumMutants args) $ nub $ patternMatchMutants ++ ifElseNegMutants ++ guardedNegMutants ++ operatorMutants
                    programMutants =  map (flip putDecls ast) $ allMutants >>= return . \f -> replaceFst func f decls
                if null ops && null swapOps
                    then return () --  putStrLn "No applicable operator exists!"
                    else sequence_ $ zipWith writeFile (genFileNamesWith fstIdx filename) $ map prettyPrint programMutants
                return $ length programMutants

-- Mutating a function's code using a bunch of mutation operators
-- NOTE: In all the three mutate functions, we assume working
-- with functions declaration.
mutates :: [MuOp] -> Decl -> [Decl]
mutates ops m = filter (/= m) $ concatMap (mutatesN ops m) [1..]

-- the third argument specifies whether it's first order or higher order
mutatesN :: [MuOp] -> Decl -> Int -> [Decl]
mutatesN ops m 1 = ops >>= \op -> mutate op m
mutatesN ops m c = let ms = mutatesN ops m (c-1)
                   in  ms >>= \m -> mutatesN ops m 1

-- given a function, generate all mutants after applying applying 
-- op once (op might be applied at different places). E.g.:
-- op = "<" ==> ">" and there are two instances of "<"
mutate :: MuOp -> Decl -> [Decl]
mutate op m = once (mkMp' op) m \\ [m]

isFunction :: Name -> GenericQ Bool
isFunction (Ident n) = False `mkQ` isFunctionD n

isFunctionD :: String -> Decl -> Bool
isFunctionD n (FunBind (Match _ (Ident n') _ _ _ _ : _)) = n == n'
isFunctionD n (FunBind (Match _ (Symbol n') _ _ _ _ : _)) = n == n'
isFunctionD n (PatBind _ (PVar (Ident n')) _ _)          = n == n'
isFunctionD _ _                                  = False

-- generate all operators for permutating pattern matches in
-- a function. We don't deal with permutating guards and case for now.
permMatches :: Decl -> [MuOp]
permMatches d@(FunBind ms) = d ==>* map FunBind (permutations ms \\ [ms])
permMatches _  = []

removeOnePMatch :: Decl -> [MuOp]
removeOnePMatch d@(FunBind ms) = d ==>* map FunBind (removeOneElem ms \\ [ms])
removeOnePMatch _  = []

removeOneElem :: Eq t => [t] -> [[t]]
removeOneElem l = choose l (length l - 1)

-- AST/module-related operations
-- String is the content of the file
parseModuleFromFile :: String -> Module
parseModuleFromFile inp = fromParseResult $ parseFileContents inp

getASTFromFile filename = readFile filename >>= return . parseModuleFromFile

getModuleName :: String -> IO String
getModuleName  fileName =  getASTFromFile fileName >>= return . getName
-- replacing the first instance of an element 
-- in a list with a new value
replaceFst :: Eq a => a -> a -> [a] -> [a]
replaceFst _ _ [] = []
replaceFst oldVal newVal (v:vs) | v == oldVal = newVal:vs
                                | otherwise   = v:replaceFst oldVal newVal vs

getDecls :: Module -> [Decl]
getDecls (Module _ _ _ _ _ _ decls) = decls

extractStrings :: [Match] -> [String] 
extractStrings [] = []
extractStrings ((Match _ (Symbol name) _ _ _ _):xs) = name : (extractStrings xs)
extractStrings ((Match _ (Ident name) _ _ _ _):xs) = name : (extractStrings xs)

getFuncNames :: [Decl] -> [String]
getFuncNames [] = []
getFuncNames ((FunBind m):xs) = (extractStrings m) ++ getFuncNames xs
getFuncNames (_:xs) = getFuncNames xs
  
putDecls :: [Decl] -> Module -> Module
putDecls decls (Module a b c d e f _) = Module a b c d e f decls

getName :: Module -> String
getName (Module _ (ModuleName name) _ _ _ _ _) = name

----------------------------------------
------------- EXAMPLES -----------------
-- genMutantsWith (stdArgs {muOps = [Symbol "<" ==> Symbol ">"], maxNumMutants = 10000}) funcname filename
