{-# LANGUAGE MultiWayIf #-}
-- | Read the HPC Tix and Mix files.
module Test.MuCheck.Tix where
import Trace.Hpc.Tix
import Trace.Hpc.Mix
import Trace.Hpc.Util

type Span = HpcPos

-- | Convert a 4-tuple to a span
toSpan :: (Int, Int, Int, Int) -> Span
toSpan = toHpcPos

data TCovered = TCovered
              | TNotCovered
  deriving (Eq, Show)

isCovered :: TCovered -> Bool
isCovered TCovered = True
isCovered _  = False

-- | insideSpan small big
insideSpan :: Span -> Span -> Bool
insideSpan = insideHpcPos

-- | `mixTix` joins together the location and coverage data.
mixTix :: String -> Mix -> TixModule -> (String, [(Span, TCovered)])
mixTix s (Mix _fp _int _h _i mixEntry) tix = (s, zipWith toLocC mymixes mytixes)
  where mytixes = tixModuleTixs tix
        mymixes = mixEntry
        toLocC (hpos, _) covT = (toSpan (fromHpcPos hpos), isCov covT)
        isCov 0 = TNotCovered
        isCov _ = TCovered

-- | reads a tix file. The tix is named for the binary run, and contains a list
-- of modules involved.
parseTix :: String -> IO [TixModule]
parseTix path = do
  tix <- readTix path
  case tix of
    Nothing -> return []
    Just (Tix tms) -> return tms

-- | Read the corresponding Mix file to a TixModule
getMix :: TixModule -> IO Mix
getMix tm = readMix [".hpc"] (Right tm)

getMixedTix :: String -> IO [(String, [(Span, TCovered)])]
getMixedTix file = do
  tms <- parseTix file
  mixs <- mapM getMix tms
  let names = map tixModuleName tms
  return $ zipWith3 mixTix names mixs tms
{- getMixedTix "tests.tix"
[("Main",[
 (11:12-11:26,TNotCovered),
 (11:3-11:26,TNotCovered),
 (10:9-11:26,TNotCovered),
 (10:1-11:26,TNotCovered),
 (6:14-6:44,TCovered),
 (6:3-6:44,TCovered),
 (7:14-7:46,TCovered),
 (7:3-7:46,TCovered),
 (8:14-8:46,TCovered),
 (8:3-8:46,TCovered),
 (5:8-8:46,TCovered),
 (5:1-8:46,TCovered)])]
-}
-- [10:1-11:26]
-- | getUnCoveredPatches returns the largest parts of the program that are not
-- covered.
getUnCoveredPatches :: String -> String -> IO [Span]
getUnCoveredPatches file name = do
  val <- getMixedTix file
  return $ case val of
    [] -> []
    _ -> removeRedundantSpans $ map fst $ filter (not . isCovered . snd) $ getNamedModule name val

getNamedModule :: String -> [(String, [(Span,TCovered)])] -> [(Span,TCovered)]
getNamedModule mname val = snd . head $ filter (\(a, _b) -> a == mname) val

removeRedundantSpans :: [Span] -> [Span]
removeRedundantSpans [] = []
removeRedundantSpans [x] = [x]
removeRedundantSpans (a:b:cde) = if | insideSpan a b -> removeRedundantSpans (b:cde)
                                    | otherwise      -> a : removeRedundantSpans (b:cde)

