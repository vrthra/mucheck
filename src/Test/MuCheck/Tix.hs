-- | Read the HPC Tix and Mix files.
module Test.MuCheck.Tix where
import Trace.Hpc.Tix
import Trace.Hpc.Mix
import Trace.Hpc.Util

data Span = Span Int Int Int Int

-- | Convert a 4-tuple to a span
toSpan :: (Int, Int, Int, Int) -> Span
toSpan (i,j,k,l) = Span i j k l

-- | `mixTix` joins together the location and coverage data.
mixTix :: Mix -> TixModule ->[(Span, Bool)]
mixTix (Mix _fp _int _h _i mixEntry) tix = zipWith toLocC mymixes mytixes
  where mytixes = tixModuleTixs tix
        mymixes = mixEntry
        toLocC (hpos, _) covT = (toSpan (fromHpcPos hpos), covT > 0)

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

