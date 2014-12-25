To begin:

(1) Install required packages:

```
cabal update
cabal install cabal-install
cabal install syb
cabal install haskell-src-exts
cabal install hint
```
(2) Load Main.hs to Ghci and run the following commmands.

For QuickCheck
```
*Main> numMutants <- genMutants "qsort" "Examples/Quicksort.hs"

*Main> checkPropsOnMutants (take numMutants $ genFileNames "Examples/Quicksort.hs") "Examples.Quicksort" ["quickCheckResult idEmpProp", "quickCheckResult revProp", "quickCheckResult modelProp"] "./test.log"
```
For HUnit
```
*Main> numMutants <- genMutants "qsort" "Examples/HUnitTest.hs"

*Main> checkTestSuiteOnMutants (take numMutants $ genFileNames "Examples/HUnitTest.hs") "Examples.HUnitTest" ["runTestTT tests"] "./test.log"
```
For Hspec: 
```
*Main> numMutants <- genMutants "qsort" "Examples/HspecTest.hs"

*Main> checkHspecOnMutants (take numMutants $ genFileNames "Examples/HspecTest.hs") "Examples.HspecTest" ["spec (with \"qsort1\")"] "./test.log"
```
Note that using MuCheck with Hspec is a little more involved than HUnit qnd QuickCheck. You will need to define the `with` function or something similar if you wish to select specific groups to run. See `Examples/HspecTest.hs` for a simple example.

Using custom list of mutators
```
genMut funcname filename = genMutantsWith (stdArgs {muOps = [Symbol "<" ==> Symbol ">"], maxNumMutants = 10000}) funcname filename
```

With Cabal Sandbox:
```
cd mucheck
cabal sandbox init
cabal install --only-dependencies
cabal configure --flags="--enable-tests"
cabal build
cabal repl
```

Execute directly
```
./mucheck qsort Examples/Quicksort.hs Examples.Quicksort "quickCheckResult idEmpProp" "quickCheckResult revProp" "quickCheckResult modelProp"
```
