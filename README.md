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
```
*Main> numMutants <- genMutants "qsort" "Examples/Quicksort.hs"

*Main> checkPropsOnMutants (take numMutants $ genFileNames "Examples/Quicksort.hs") "Examples.Quicksort" ["quickCheckResult idEmpProp", "quickCheckResult revProp", "quickCheckResult modelProp"] "./test.log"

*Main> numMutants <- genMutants "qsort" "Examples/HUnitTest.hs"

*Main> checkTestSuiteOnMutants (take numMutants $ genFileNames "Examples/HUnitTest.hs") "Examples.HUnitTest" ["runTestTT tests"] "./test.log"
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
