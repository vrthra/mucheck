{-# LANGUAGE ImpredicativeTypes, Rank2Types, TupleSections, RecordWildCards #-}
-- | This module handles the mutation of different patterns.
module Test.MuCheck.Mutation where

import Language.Haskell.Exts.Annotated(Literal(Int, Char, Frac, String, PrimInt, PrimChar, PrimFloat, PrimDouble, PrimWord, PrimString),
        Exp(App, Var, If, Lit), QName(UnQual),
        Match(Match), Pat(PVar),
        Stmt(Qualifier), Module(Module),
        Name(Ident), Decl(FunBind, PatBind, AnnPragma),
        GuardedRhs(GuardedRhs), Annotation(Ann), Name(Symbol, Ident),
        prettyPrint, fromParseResult, parseModule, SrcSpanInfo(..), SrcSpan(..),
        ModuleHead(..), ModuleName(..))
import Data.Generics (Typeable, mkMp, listify)
import Data.List(nub, (\\), permutations, partition)
import Control.Monad (liftM)

import Test.MuCheck.Tix
import Test.MuCheck.MuOp
import Test.MuCheck.Utils.Syb
import Test.MuCheck.Utils.Common
import Test.MuCheck.Config
import Test.MuCheck.TestAdapter

-- | The `genMutants` function is a wrapper to genMutantsWith with standard
-- configuraton
genMutants ::
     FilePath           -- ^ The module we are mutating
  -> IO (Int,[Mutant]) -- ^ Returns the covering mutants produced, and original length.
genMutants = genMutantsWith defaultConfig

-- | The `genMutantsWith` function takes configuration function to mutate,
-- function to mutate, filename the function is defined in, and produces
-- mutants in the same directory as the filename, and returns the number
-- of mutants produced.
genMutantsWith ::
     Config                     -- ^ The configuration to be used
  -> FilePath                   -- ^ The module we are mutating
  -> IO (Int, [Mutant])         -- ^ Returns the covered mutants produced, and the original number
genMutantsWith _config filename  = do
      f <- readFile filename

      let modul = getModuleName (getASTFromStr f)
          mutants :: [Mutant]
          mutants = genMutantsForSrc defaultConfig f

      -- We have a choice here. We could allow users to specify test specific
      -- coverage rather than a single coverage. This can further reduce the
      -- mutants.
      c <- getUnCoveredPatches "test.tix" modul
      -- check if the mutants span is within any of the covered spans.
      let coveredMutants = case c of
                            Nothing -> mutants
                            Just v -> removeUncovered v mutants
      return (length mutants, coveredMutants)

-- | Remove mutants that are not covered by any tests
removeUncovered :: [Span] -> [Mutant] -> [Mutant]
removeUncovered uspans mutants = filter isMCovered mutants -- get only covering mutants.
  where  isMCovered :: Mutant -> Bool
         -- | is it contained in any of the spans? if it is, then return false.
         isMCovered Mutant{..} = not $ any (insideSpan _mspan) uspans

-- | Get the module name from ast
getModuleName :: Module t -> String
getModuleName (Module _ (Just (ModuleHead _ (ModuleName _ name) _ _ )) _ _ _) = name
getModuleName _ = ""

-- | The `genMutantsForSrc` takes the function name to mutate, source where it
-- is defined, and returns the mutated sources
genMutantsForSrc ::
     Config                   -- ^ Configuration
  -> String                   -- ^ The module we are mutating
  -> [Mutant] -- ^ Returns the mutants
genMutantsForSrc config src = map (toMutant . apTh (prettyPrint . withAnn)) $ programMutants config ast
  where origAst = getASTFromStr src
        (onlyAnn, noAnn) = splitAnnotations origAst
        ast = putDecl origAst noAnn
        withAnn mast = putDecl mast $ getDecl mast ++ onlyAnn

-- | Produce all mutants after applying all operators
programMutants ::
     Config                   -- ^ Configuration
  -> Module_                  -- ^ Module to mutate
  -> [(MuVar, Span, Module_)] -- ^ Returns mutated modules
programMutants config ast =  nub $ mutatesN (applicableOps config ast) ast fstOrder
  where fstOrder = 1 -- first order

-- | Returns all mutation operators
applicableOps ::
     Config                   -- ^ Configuration
  -> Module_                  -- ^ Module to mutate
  -> [(MuVar,MuOp)]           -- ^ Returns mutation operators
applicableOps config ast = relevantOps ast opsList
  where opsList = concatMap spread [
            (MutatePatternMatch, selectFnMatches ast),
            (MutateValues, selectLiteralOps ast),
            (MutateFunctions, selectFunctionOps (muOp config) ast),
            (MutateNegateIfElse, selectIfElseBoolNegOps ast),
            (MutateNegateGuards, selectGuardedBoolNegOps ast)]

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
putDecl (Module a b c d _) decls = Module a b c d decls
putDecl m _ = m

-- | First and higher order mutation. The actual apply of mutation operators,
-- and generation of mutants happens here.
-- The third argument specifies whether it's first order or higher order
mutatesN ::
     [(MuVar,MuOp)]     -- ^ Applicable Operators
  -> Module_            -- ^ Module to mutate
  -> Int                -- ^ Order of mutation (usually 1 - first order)
  -> [(MuVar, Span, Module_)] -- ^ Returns the mutated module
mutatesN os ast n = mutatesN' os (MutateOther [], toSpan (0,0,0,0), ast) n
  where mutatesN' ops ms 1 = concat [mutate op ms | op <- ops ]
        mutatesN' ops ms c = concat [mutatesN' ops m 1 | m <- mutatesN' ops ms $ pred c]

-- | Given a function, generate all mutants after applying applying
-- op once (op might be applied at different places).
-- E.g.: if the operator is (op = "<" ==> ">") and there are two instances of
-- "<" in the AST, then it will return two AST with each replaced.
mutate :: (MuVar, MuOp) -> (MuVar, Span, Module_) -> [(MuVar, Span, Module_)]
mutate (v, op) (_v, _s, m) = map (v,toSpan $ getSpan op, ) $ once (mkMpMuOp op) m \\ [m]

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

-- | The identifier of declared pragma
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

-- | Look for literal values in AST, and return applicable MuOp transforms.
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
selectGuardedBoolNegOps :: Module_ -> [MuOp]
selectGuardedBoolNegOps m = selectValOps isGuardedRhs convert m
  where isGuardedRhs :: GuardedRhs_ -> Bool
        isGuardedRhs GuardedRhs{} = True
        convert (GuardedRhs l stmts expr) = [GuardedRhs l s expr | s <- once (mkMp boolNegate) stmts]
        boolNegate _e@(Qualifier _l (Var _lv (UnQual _lu (Ident _li "otherwise")))) = [] -- VERIFY
        boolNegate (Qualifier l expr) = [Qualifier l (App l_ (Var l_ (UnQual l_ (Ident l_ "not"))) expr)]
        boolNegate _x = [] -- VERIFY

-- | dummy 
l_ :: SrcSpanInfo
l_ = SrcSpanInfo (SrcSpan "" 0 0 0 0) []


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

-- | Generate all operators for permuting symbols like binary operators
-- Since we are looking for symbols, we are reasonably sure that it is not
-- locally bound to a variable.
selectSymbolFnOps :: Module_ -> [String] -> [MuOp]
selectSymbolFnOps m s = selectValOps isBin convert m
  where isBin :: Name_ -> Bool
        isBin (Symbol _l n) | n `elem` s = True
        isBin _ = False
        convert (Symbol l n) = map (Symbol l) $ filter (/= n) s
        convert _ = []

-- | Generate all operators for permuting commonly used functions (with
-- identifiers).
selectIdentFnOps :: Module_ -> [String] ->  [MuOp]
selectIdentFnOps m s = selectValOps isCommonFn convert m
  where isCommonFn :: Exp_ -> Bool
        isCommonFn (Var _lv (UnQual _lu (Ident _l n))) | n `elem` s = True
        isCommonFn _ = False
        convert (Var lv_ (UnQual lu_ (Ident li_ n))) = map  (Var lv_ . UnQual lu_ . Ident li_) $ filter (/= n) s
        convert _ = []

-- | Generate all operators depending on whether it is a symbol or not.
selectFunctionOps :: [FnOp] -> Module_ -> [MuOp]
selectFunctionOps fo f = concatMap (selectIdentFnOps f) idents ++ concatMap (selectSymbolFnOps f) syms
  where idents = map _fns $ filter (\a -> _type a == FnIdent) fo
        syms = map _fns $ filter (\a -> _type a == FnSymbol) fo

-- (Var l (UnQual l (Ident l "ab")))
-- (App l (Var l (UnQual l (Ident l "head"))) (Var l (UnQual l (Ident l "b"))))
-- (App l (App l (Var l (UnQual l (Ident l "head"))) (Var l (UnQual l (Ident l "a")))) (Var l (UnQual l (Ident l "b")))))
-- (InfixApp l (Var l (UnQual l (Ident l "a"))) (QVarOp l (UnQual l (Symbol l ">"))) (Var l (UnQual l (Ident l "b"))))
-- (InfixApp l (Var l (UnQual l (Ident l "a"))) (QVarOp l (UnQual l (Ident l "x"))) (Var l (UnQual l (Ident l "b"))))
