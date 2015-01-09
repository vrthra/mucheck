{-# LANGUAGE ImpredicativeTypes, Rank2Types, TupleSections #-}
-- | This module handles the mutation of different patterns.
module Test.MuCheck.Mutation where

import Language.Haskell.Exts.Annotated(Literal(Int, Char, Frac, String, PrimInt, PrimChar, PrimFloat, PrimDouble, PrimWord, PrimString),
        Exp(App, Var, If, Lit), QName(UnQual),
        Match(Match), Pat(PVar),
        Stmt(Qualifier), Module(Module),
        Name(Ident), Decl(FunBind, PatBind, AnnPragma),
        GuardedRhs(GuardedRhs), Annotation(Ann), Name(Symbol, Ident),
        prettyPrint, fromParseResult, parseModule, SrcSpanInfo(..), SrcSpan(..))
import Data.Generics (Typeable, mkMp, listify)
import Data.List(nub, (\\), permutations, partition)
import System.Random (RandomGen)
import Control.Monad (liftM)

import Test.MuCheck.MuOp
import Test.MuCheck.Utils.Syb
import Test.MuCheck.Utils.Common
import Test.MuCheck.Config
import Test.MuCheck.TestAdapter
import Test.MuCheck.Utils.Helpers

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
      let mutants = genMutantsForSrc defaultConfig f
      return $ sampler args g mutants

-- | Wrapper around sampleF that returns correct sampling ratios according to
-- configuration passed.
sampler :: RandomGen g =>
     Config                   -- ^ Configuration
  -> g                        -- ^ The random seed
  -> [(MuVars, t)]            -- ^ The original list of mutation operators
  -> [t]                      -- ^ Returns the sampled mutation operators
sampler _args _g mv = map snd mv

-- | The `genMutantsForSrc` takes the function name to mutate, source where it
-- is defined, and a sampling function, and returns the mutated sources selected
-- using sampling function.
genMutantsForSrc ::
     Config                   -- ^ Configuration
  -> String                   -- ^ The module we are mutating
  -> [(MuVars, Mutant)]                 -- ^ Returns the sampled mutants
genMutantsForSrc config src = map (apSnd $ prettyPrint . withAnn) $ programMutants config ast
  where origAst = getASTFromStr src
        (onlyAnn, noAnn) = splitAnnotations origAst
        ast = putDecl origAst noAnn
        withAnn mast = putDecl mast $ (getDecl mast) ++ onlyAnn

programMutants :: Config -> Module_ -> [(MuVars, Module_)]
programMutants config ast =  nub $ mutatesN (applicableOps config ast) (MutateOther [], ast) fstOrder
  where fstOrder = 1 -- first order

applicableOps :: Config -> Module_ -> [(MuVars,MuOp)]
applicableOps config ast = relevantOps ast opsList
  where opsList = concatMap spread [
            (MutatePatternMatch, selectFnMatches ast),
            (MutateValues, selectLiteralOps ast),
            (MutateNegateIfElse, selectIfElseBoolNegOps ast),
            (MutateNegateGuards, selectGuardedBoolNegOps ast),
            (MutateOther "User", muOps config)]

-- | Split declarations of the module to annotated and non annotated.
splitAnnotations :: Module_ -> ([Decl_], [Decl_])
splitAnnotations ast = partition fn $ getDecl ast
  where fn x = (functionName x ++ pragmaName x) `elem` getAnnotatedTests ast
        -- only one of pragmaName or functionName will be present at a time.

-- | Returns the annotated tests and their annotations
getAnnotatedTests :: Module_ -> [String]
getAnnotatedTests ast = concatMap (getAnn ast) ["Test","TestSupport"]

-- | Get the embedded declarations from a module.
getDecl :: Module_ -> [Decl_]
getDecl (Module _ _ _ _ decls) = decls
getDecl _ = []

-- | Put the given declarations into the given module
putDecl :: Module_ -> [Decl_] -> Module_
putDecl (Module a b c d _) decls = (Module a b c d decls)
putDecl m _ = m

-- | First and higher order mutation. The actual apply of mutation operators,
-- and generation of mutants happens here.
-- The third argument specifies whether it's first order or higher order
mutatesN :: [(MuVars,MuOp)] -> (MuVars, Module_) -> Int -> [(MuVars, Module_)]
mutatesN ops ms 1 = concat [mutate op ms | op <- ops ]
mutatesN ops ms c = concat [mutatesN ops m 1 | m <- mutatesN ops ms $ pred c]

-- | Given a function, generate all mutants after applying applying
-- op once (op might be applied at different places).
-- E.g.: if the operator is (op = "<" ==> ">") and there are two instances of
-- "<" in the AST, then it will return two AST with each replaced.
mutate :: (MuVars, MuOp) -> (MuVars, Module_) -> [(MuVars, Module_)]
mutate (v, op) (_v, m) = map (v, ) $ once (mkMpMuOp op) m \\ [m]

-- | Generate sub-arrays with one less element
removeOneElem :: Eq t => [t] -> [[t]]
removeOneElem l = choose l (length l - 1)

-- AST/module-related operations

-- | Returns the AST from the file
getASTFromStr :: String -> Module_
getASTFromStr fname = fromParseResult $ parseModule fname

-- | get all annotated functions
getAnn :: Module_ -> String -> [String]
getAnn m s =  [conv name | Ann _l name _exp <- listify isAnn m]
  where isAnn :: Annotation_ -> Bool
        isAnn (Ann _l (Symbol _lsy _name) (Lit _ll (String _ls e _))) = e == s
        isAnn (Ann _l (Ident _lid _name) (Lit _ll (String _ls e _))) = e == s
        isAnn _ = False
        conv (Symbol _l n) = n
        conv (Ident _l n) = n

-- | given the module name, return all marked tests
getAllTests :: String -> IO [String]
getAllTests modname = liftM allTests $ readFile modname

-- | Given module source, return all marked tests
allTests :: String -> [String]
allTests modsrc = getAnn (getASTFromStr modsrc) "Test"

-- | The name of a function
functionName :: Decl_ -> String
functionName (FunBind _l (Match _ (Ident _li n) _ _ _ : _)) = n
functionName (FunBind _l (Match _ (Symbol _ls n) _ _ _ : _)) = n
-- we also consider where clauses
functionName (PatBind _ (PVar _lpv (Ident _li n)) _ _)          = n
functionName _                                   = []

pragmaName :: Decl_ -> String
pragmaName (AnnPragma _ (Ann _l (Ident _li n) (Lit _ll (String _ls _t _)))) = n
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
selectValOps :: (Typeable b, Mutable b) => (b -> Bool) -> (b -> [b]) -> Module_ -> [MuOp]
selectValOps predicate f m = concat [ x ==>* f x |  x <- vals ]
  where vals = listify predicate m

selectLiteralOps :: Module_ -> [MuOp]
selectLiteralOps m = selectLitOps m ++ selectBLitOps m
-- | Look for literal values in AST, and return applicable MuOp transforms.
-- Unfortunately booleans are not handled here.
selectLitOps :: Module_ -> [MuOp]
selectLitOps m = selectValOps isLit convert m
  where isLit :: Literal_ -> Bool
        isLit Int{} = True
        isLit PrimInt{} = True
        isLit Char{} = True
        isLit PrimChar{} = True
        isLit Frac{} = True
        isLit PrimFloat{} = True
        isLit PrimDouble{} = True
        isLit String{} = True
        isLit PrimString{} = True
        isLit PrimWord{} = True
        convert (Int l i _) = map (apX (Int l)) $ nub [i + 1, i - 1, 0, 1]
        convert (PrimInt l i _) = map (apX (PrimInt l)) $ nub [i + 1, i - 1, 0, 1]
        convert (Char l c _) = map (apX (Char l)) [pred c, succ c]
        convert (PrimChar l c _) = map (apX (Char l)) [pred c, succ c]
        convert (Frac l f _) = map (apX (Frac l)) $ nub [f + 1.0, f - 1.0, 0.0, 1.1]
        convert (PrimFloat l f _) = map (apX (PrimFloat l)) $ nub [f + 1.0, f - 1.0, 0.0, 1.0]
        convert (PrimDouble l f _) = map (apX (PrimDouble l)) $ nub [f + 1.0, f - 1.0, 0.0, 1.0]
        convert (String l _ _) = map (apX (String l)) $ nub [""]
        convert (PrimString l _ _) = map (apX (PrimString l)) $ nub [""]
        convert (PrimWord l i _) = map (apX (PrimWord l)) $ nub [i + 1, i - 1, 0, 1]
        apX :: (t1 -> [a] -> t) -> t1 -> t
        apX fn i = fn i []

-- | Convert Boolean Literals
--
-- > (True, False)
--
-- becomes
--
-- > (False, True)

selectBLitOps :: Module_ -> [MuOp]
selectBLitOps m = selectValOps isLit convert m
  where isLit :: Name_ -> Bool
        isLit (Ident _l "True") = True
        isLit (Ident _l "False") = True
        isLit _ = False
        convert (Ident l "True") = [Ident l "False"]
        convert (Ident l "False") = [Ident l "True"]
        convert _ = []

-- | Negating boolean in if/else statements
--
-- > if True then 1 else 0
--
-- becomes
--
-- > if True then 0 else 1

selectIfElseBoolNegOps :: Module_ -> [MuOp]
selectIfElseBoolNegOps m = selectValOps isIf convert m
  where isIf :: Exp_ -> Bool
        isIf If{} = True
        isIf _    = False
        convert (If l e1 e2 e3) = [If l e1 e3 e2]
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

l_ :: SrcSpanInfo
l_ = SrcSpanInfo (SrcSpan "" 0 0 0 0) []

selectGuardedBoolNegOps :: Module_ -> [MuOp]
selectGuardedBoolNegOps m = selectValOps isGuardedRhs convert m
  where isGuardedRhs :: GuardedRhs_ -> Bool
        isGuardedRhs GuardedRhs{} = True
        convert (GuardedRhs l stmts expr) = [GuardedRhs l s expr | s <- once (mkMp boolNegate) stmts]
        boolNegate e@(Qualifier _l (Var _lv (UnQual _lu (Ident _li "otherwise")))) = [e]
        boolNegate (Qualifier l expr) = [Qualifier l (App l_ (Var l_ (UnQual l_ (Ident l_ "not"))) expr)]
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

selectFnMatches :: Module_ -> [MuOp]
selectFnMatches m = selectValOps isFunct convert m
  where isFunct :: Decl_ -> Bool
        isFunct FunBind{} = True
        isFunct _    = False
        convert (FunBind l ms) = map (FunBind l) $ filter (/= ms) (permutations ms ++ removeOneElem ms)
        convert _ = []

