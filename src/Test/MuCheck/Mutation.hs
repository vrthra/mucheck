{-# LANGUAGE ImpredicativeTypes #-}
-- | This module handles the mutation of different patterns.
module Test.MuCheck.Mutation where

import Language.Haskell.Exts(Literal(Int, Char, Frac, String, PrimInt, PrimChar, PrimFloat, PrimDouble, PrimWord, PrimString),
        Exp(App, Var, If, Lit), QName(UnQual),
        Match(Match), Pat(PVar),
        Stmt(Qualifier), Module(Module),
        Name(Ident), Decl(FunBind, PatBind, AnnPragma),
        GuardedRhs(GuardedRhs), Annotation(Ann), Name(Symbol, Ident),
        prettyPrint, fromParseResult, parseModule)
import Data.Generics (Typeable, mkMp, listify)
import Data.List(nub, (\\), permutations, partition)
import System.Random (RandomGen)

import Test.MuCheck.MuOp
import Test.MuCheck.Utils.Syb
import Test.MuCheck.Utils.Common
import Test.MuCheck.Config
import Test.MuCheck.TestAdapter

-- | The `genMutants` function is a wrapper to genMutantsWith with standard
-- configuraton
genMutants ::
     FilePath         -- ^ The module we are mutating
  -> IO [Mutant]      -- ^ Returns the mutants produced.
genMutants = genMutantsWith defaultConfig

-- | The `genMutantsWith` function takes configuration function to mutate,
-- function to mutate, filename the function is defined in, and produces
-- mutants in the same directory as the filename, and returns the number
-- of mutants produced.
genMutantsWith ::
     Config                     -- ^ The configuration to be used
  -> FilePath                   -- ^ The module we are mutating
  -> IO [Mutant]                -- ^ Returns the mutants produced
genMutantsWith args filename  = do
      g <- genRandomSeed
      f <- readFile filename
      return $ genMutantsForSrc defaultConfig f (sampler args g)

-- | Wrapper around sampleF that returns correct sampling ratios according to
-- configuration passed.
sampler :: RandomGen g =>
     Config                   -- ^ Configuration
  -> g                        -- ^ The random seed
  -> MuVars                   -- ^ What kind of a mutation are we interested in?
  -> [t]                      -- ^ The original list of mutation operators
  -> [t]                      -- ^ Returns the sampled mutation operators
sampler args g m = sampleF g (getSample m args)

-- | The `genMutantsForSrc` takes the function name to mutate, source where it
-- is defined, and a sampling function, and returns the mutated sources selected
-- using sampling function.
genMutantsForSrc ::
     Config                   -- ^ Configuration
  -> String                   -- ^ The module we are mutating
  -> (MuVars -> [MuOp] -> [MuOp]) -- ^ The sampling function
  -> [Mutant]                 -- ^ Returns the sampled mutants
genMutantsForSrc args src sampleFn = map (prettyPrint . putBack) programMutants
  where origAst = getASTFromStr src
        ast = putDecl (getASTFromStr src) noAnn

        ops, valOps, swapOps, ifElseNegOps, guardedBoolNegOps :: [MuOp]
        ops = relevantOps ast (muOps args ++ valOps ++ ifElseNegOps ++ guardedBoolNegOps)
        swapOps = sampleFn MutatePatternMatch $ selectFnMatches ast
        valOps = sampleFn MutateValues $ selectLitOps ast ++ selectBLitOps ast
        ifElseNegOps = sampleFn MutateNegateIfElse $ selectIfElseBoolNegOps ast
        guardedBoolNegOps = sampleFn MutateNegateGuards $ selectGuardedBoolNegOps ast

        ifElseNegMutants, guardedNegMutants, operatorMutants, allMutants :: [Module]
        allMutants = nub $ patternMatchMutants ++
                           operatorMutants ++
                           ifElseNegMutants ++
                           guardedNegMutants

        patternMatchMutants = mutatesN swapOps ast fstOrder
        ifElseNegMutants = mutatesN ifElseNegOps ast fstOrder
        guardedNegMutants = mutatesN guardedBoolNegOps ast fstOrder
        operatorMutants = case genMode args of
            FirstOrderOnly -> mutatesN ops ast fstOrder
            _              -> mutates ops ast

        programMutants :: [Module]
        programMutants =  allMutants

        fstOrder = 1 -- first order

        annotations :: [String]
        annotations = (getAnn origAst "Test") ++ (getAnn origAst "TestSupport")
        alldecls :: [Decl]
        alldecls = getDecl origAst

        (onlyAnn, noAnn) = partition interesting alldecls
        interesting x = (functionName x ++ pragmaName x) `elem` annotations
        putBack m = putDecl m $ (getDecl m) ++ onlyAnn


-- | Get the embedded declarations from a module.
getDecl :: Module -> [Decl]
getDecl (Module _ _ _ _ _ _ decls) = decls

-- | Put the given declarations into the given module
putDecl :: Module -> [Decl] -> Module
putDecl (Module a b c d e f _) decls = (Module a b c d e f decls)

-- | Higher order mutation of a function's code using a bunch of mutation
-- operators (In all the three mutate functions, we assume working
-- with functions declaration.)
mutates :: [MuOp] -> Module -> [Module]
mutates ops m = filter (/= m) $ concat [mutatesN ops m x | x <- enumFrom 1]

-- | First and higher order mutation.
-- The third argument specifies whether it's first order or higher order
mutatesN :: [MuOp] -> Module -> Int -> [Module]
mutatesN ops ms 1 = concat [mutate op ms | op <- ops ]
mutatesN ops ms c = concat [mutatesN ops m 1 | m <- mutatesN ops ms $ pred c]

-- | Given a function, generate all mutants after applying applying
-- op once (op might be applied at different places).
-- E.g.: if the operator is (op = "<" ==> ">") and there are two instances of
-- "<" in the AST, then it will return two AST with each replaced.
mutate :: MuOp -> Module -> [Module]
mutate op m = once (mkMpMuOp op) m \\ [m]

-- | Generate sub-arrays with one less element
removeOneElem :: Eq t => [t] -> [[t]]
removeOneElem l = choose l (length l - 1)

-- AST/module-related operations

-- | Returns the AST from the file
getASTFromStr :: String -> Module
getASTFromStr fname = fromParseResult $ parseModule fname

-- | get all annotated functions
getAnn :: Module -> String -> [String]
getAnn m s =  [conv name | Ann name _exp <- listify isAnn m]
  where isAnn (Ann (Symbol _name) (Lit (String e))) = e == s
        isAnn (Ann (Ident _name) (Lit (String e))) = e == s
        isAnn _ = False
        conv (Symbol n) = n
        conv (Ident n) = n

-- | Given module source, return all marked tests
allTests :: String -> [String]
allTests modsrc = getAnn (getASTFromStr modsrc) "Test"

-- | The name of a function
functionName :: Decl -> String
functionName (FunBind (Match _ (Ident n) _ _ _ _ : _)) = n
functionName (FunBind (Match _ (Symbol n) _ _ _ _ : _)) = n
-- we also consider where clauses
functionName (PatBind _ (PVar (Ident n)) _ _)          = n
functionName _                                   = []

pragmaName :: Decl -> String
pragmaName (AnnPragma _ (Ann (Ident n) (Lit (String _t)))) = n
pragmaName _ = []

-- but not let, because it has a different type, and for our purposes
-- this is sufficient.
-- (Let Binds Exp) :: Exp


-- | For valops, we specify how any given literal value might
-- change. So we take a predicate specifying how to recognize the literal
-- value, a list of mappings specifying how the literal can change, and the
-- AST, and recurse over the AST looking for literals that match our predicate.
-- When we find any, we apply the given list of mappings to them, and produce
-- a MuOp mapping between the original value and transformed value. This list
-- of MuOp mappings are then returned.
selectValOps :: (Typeable b, Mutable b) => (b -> Bool) -> (b -> [b]) -> Module -> [MuOp]
selectValOps predicate f m = concat [ x ==>* f x |  x <- vals ]
  where vals = listify predicate m

-- | Look for literal values in AST, and return applicable MuOp transforms.
-- Unfortunately booleans are not handled here.
selectLitOps :: Module -> [MuOp]
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
        convert (PrimFloat f) = map PrimFloat $ nub [f + 1.0, f - 1.0, 0.0, 1.0]
        convert (PrimDouble f) = map PrimDouble $ nub [f + 1.0, f - 1.0, 0.0, 1.0]
        convert (String _) = map String $ nub [""]
        convert (PrimString _) = map PrimString $ nub [""]
        convert (PrimWord i) = map PrimWord $ nub [i + 1, i - 1, 0, 1]

-- | Convert Boolean Literals
--
-- > (True, False)
--
-- becomes
--
-- > (False, True)

selectBLitOps :: Module -> [MuOp]
selectBLitOps m = selectValOps isLit convert m
  where isLit (Ident "True") = True
        isLit (Ident "False") = True
        isLit _ = False
        convert (Ident "True") = [Ident "False"]
        convert (Ident "False") = [Ident "True"]
        convert _ = []

-- | Negating boolean in if/else statements
--
-- > if True then 1 else 0
--
-- becomes
--
-- > if True then 0 else 1

selectIfElseBoolNegOps :: Module -> [MuOp]
selectIfElseBoolNegOps m = selectValOps isIf convert m
  where isIf If{} = True
        isIf _    = False
        convert (If e1 e2 e3) = [If e1 e3 e2]
        convert _ = []

-- | Negating boolean in Guards
-- | negate guarded booleans in guarded definitions
--
-- > myFn x | x == 1 = True
-- > myFn   | otherwise = False
--
-- becomes
--
-- > myFn x | not (x == 1) = True
-- > myFn   | otherwise = False

selectGuardedBoolNegOps :: Module -> [MuOp]
selectGuardedBoolNegOps m = selectValOps isGuardedRhs convert m
  where isGuardedRhs GuardedRhs{} = True
        convert (GuardedRhs srcLoc stmts expr) = [GuardedRhs srcLoc s expr | s <- once (mkMp boolNegate) stmts]
        boolNegate e@(Qualifier (Var (UnQual (Ident "otherwise")))) = [e]
        boolNegate (Qualifier expr) = [Qualifier (App (Var (UnQual (Ident "not"))) expr)]
        boolNegate x = [x]

-- | Generate all operators for permuting and removal of pattern guards from
-- function definitions
--
-- > myFn (x:xs) = False
-- > myFn _ = True
--
-- becomes
--
-- > myFn _ = True
-- > myFn (x:xs) = False
--
-- > myFn _ = True
--
-- > myFn (x:xs) = False

selectFnMatches :: Module -> [MuOp]
selectFnMatches m = selectValOps isFunct convert m
  where isFunct FunBind{} = True
        isFunct _    = False
        convert (FunBind ms) = map FunBind $ filter (== ms) (permutations ms ++ removeOneElem ms)
        convert _ = []

