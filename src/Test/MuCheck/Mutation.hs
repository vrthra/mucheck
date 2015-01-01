{-# LANGUAGE ImpredicativeTypes #-}
-- | Mutation happens here.
module Test.MuCheck.Mutation where

import Language.Haskell.Exts(Literal(Int, Char, Frac, String, PrimInt, PrimChar, PrimFloat, PrimDouble, PrimWord, PrimString),
        Exp(App, Var, If), QName(UnQual),
        Stmt(Qualifier), Module(Module),
        Name(Ident, Symbol), Decl(FunBind, PatBind),
        Pat(PVar), Match(Match), GuardedRhs(GuardedRhs),
        prettyPrint, fromParseResult, parseFileContents)
import Data.Generics (Typeable, mkMp, listify)
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
        valOps = sampleF g (doMutateValues args) $ selectLitOps f
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

        getFunc :: String -> Module -> Decl
        getFunc fname ast' = head $ listify (isFunctionD fname) ast'
        programMutants ast' =  map (putDecls ast) $ mylst ast'
        mylst ast' = [myfn ast' x | x <- take (maxNumMutants args) allMutants]
        myfn ast' fn = replace (getFunc funcname ast', fn) (getDecls ast')

    case ops ++ swapOps of
      [] -> return [] --  putStrLn "No applicable operator exists!"
      _  -> zipWithM writeFile (genFileNames filename) $ map prettyPrint (programMutants ast)
  where fstOrder = 1 -- first order
        getASTFromFile :: String -> IO Module
        getASTFromFile fname = liftM parseModuleFromFile $ readFile fname

-- | Higher order mutation of a function's code using a bunch of mutation
-- operators (In all the three mutate functions, we assume working
-- with functions declaration.)
mutates :: [MuOp] -> Decl -> [Decl]
mutates ops m = filter (/= m) $ concat [mutatesN ops m x | x <- enumFrom 1]

-- | First and higher order mutation.
-- The third argument specifies whether it's first order or higher order
mutatesN :: [MuOp] -> Decl -> Int -> [Decl]
mutatesN ops ms 1 = concat [mutate op ms | op <- ops ]
mutatesN ops ms c = concat [mutatesN ops m 1 | m <- mutatesN ops ms (c-1)]

-- | Given a function, generate all mutants after applying applying
-- op once (op might be applied at different places).
-- E.g.: if the operator is (op = "<" ==> ">") and there are two instances of
-- "<" in the AST, then it will return two AST with each replaced.
mutate :: MuOp -> Decl -> [Decl]
mutate op m = once (mkMpMuOp op) m \\ [m]

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

-- | Generates transformations that removes one pattern match from a function
-- definition.
removeOnePMatch :: Decl -> [MuOp]
removeOnePMatch (FunBind [_]) = []
removeOnePMatch d@(FunBind ms) = d ==>* map FunBind (removeOneElem ms \\ [ms])
removeOnePMatch _  = []

-- | Generate sub-arrays with one less element
removeOneElem :: Eq t => [t] -> [[t]]
removeOneElem l = choose l (length l - 1)

-- AST/module-related operations

-- | Parse a module. Input is the content of the file
parseModuleFromFile :: String -> Module
parseModuleFromFile inp = fromParseResult $ parseFileContents inp

-- | Get the declaration part from a module
getDecls :: Module -> [Decl]
getDecls (Module _ _ _ _ _ _ decls) = decls

-- | Set the declaration in a module
putDecls :: Module -> [Decl] -> Module
putDecls (Module a b c d e f _) decls = Module a b c d e f decls

-- | For valops, unlike functions, we specify how any given literal value might
-- change. So we take a predicate specifying how to recognize the literal
-- value, a list of mappings specifying how the literal can change, and the
-- AST, and recurse over the AST looking for literals that match our predicate.
-- When we find any, we apply the given list of mappings to them, and produce
-- a MuOp mapping between the original value and transformed value. This list
-- of MuOp mappings are then returned.
selectValOps :: (Typeable b, Mutable b) => (b -> Bool) -> (b -> [b]) -> Decl -> [MuOp]
selectValOps predicate f m = concat [ x ==>* f x |  x <- vals ]
  where vals = listify predicate m

-- | Look for literal values in AST, and return applicable MuOp transforms.
selectLitOps :: Decl -> [MuOp]
selectLitOps m = selectValOps isLit convert m
  where isLit (Int _) = True
        isLit (PrimInt _) = True
        isLit (Char _) = True
        isLit (PrimChar _) = True
        isLit (Frac _) = True
        isLit (PrimFloat _) = True
        isLit (PrimDouble _) = True
        isLit (String _) = True
        isLit (PrimString _) = True
        isLit (PrimWord _) = True
        convert (Int i) = map Int $ nub [i + 1, i - 1, 0, 1]
        convert (PrimInt i) = map PrimInt $ nub [i + 1, i - 1, 0, 1]
        convert (Char c) = map Char [pred c, succ c]
        convert (PrimChar c) = map Char [pred c, succ c]
        convert (Frac f) = map Frac $ nub [f + 1.0, f - 1.0, 0.0, 1.1]
        convert (PrimFloat f) = map PrimFloat $ nub [f + 1.0, f - 1.0, 0.0, 1.1]
        convert (PrimDouble f) = map PrimDouble $ nub [f + 1.0, f - 1.0, 0.0, 1.1]
        convert (String _) = map String $ nub [""]
        convert (PrimString _) = map PrimString $ nub [""]
        convert (PrimWord i) = map PrimWord $ nub [i + 1, i - 1, 0, 1]

-- | Negating boolean in if/else statements
selectIfElseBoolNegOps :: Decl -> [MuOp]
selectIfElseBoolNegOps m = selectValOps isIf (\(If e1 e2 e3) -> [If (App (Var (UnQual (Ident "not"))) e1) e2 e3]) m
  where isIf If{} = True
        isIf _    = False

-- | Negating boolean in Guards
selectGuardedBoolNegOps :: Decl -> [MuOp]
selectGuardedBoolNegOps m = selectValOps isGuardedRhs negateGuardedRhs m
  where isGuardedRhs GuardedRhs{} = True
        boolNegate e@(Qualifier (Var (UnQual (Ident "otherwise")))) = [e]
        boolNegate (Qualifier expr) = [Qualifier (App (Var (UnQual (Ident "not"))) expr)]
        boolNegate x = [x]
        negateGuardedRhs (GuardedRhs srcLoc stmts expr) = [GuardedRhs srcLoc s expr | s <- once (mkMp boolNegate) stmts]

