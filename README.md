To begin:

(1) Install required packages:

```
$ cabal update
$ cabal install cabal-install
$ cabal install --only-dependencies --enable-tests
```
(2) Install one of the adapter packages

* [mucheck-quickcheck](https://bitbucket.org/osu-testing/mucheck-quickcheck)
* [mucheck-smallcheck](https://bitbucket.org/osu-testing/mucheck-smallcheck)
* [mucheck-hunit](https://bitbucket.org/osu-testing/mucheck-hunit)
* [mucheck-hspec](https://bitbucket.org/osu-testing/mucheck-hspec)

(3) Execute the adapter help
```
$ $dist/mucheck-quickcheck -h
$ $dist/mucheck-smallcheck -h
$ $dist/mucheck-hunit -h
$ $dist/mucheck-hspec -h
```
Use the example given.

# As library

## QuickCheck
```
$ ./mucheck-quickcheck qsort Examples/QuickCheckTest.hs Examples.QuickCheckTest 'quickCheckResult idEmpProp' 'quickCheckResult revProp' 'quickCheckResult modelProp'
```

(4) Or use it from ghci, after copying one of the examples like this.
```
$ cp ../mucheck-quickcheck/Examples/QuickCheckTest.hs Examples/QuickCheckTest.hs
$ ghci
> :m + Test.MuCheck
> :m + Test.MuCheck.TestAdapter
> :m + Test.MuCheck.TestAdapter.QuickCheck
> :m + Test.QuickCheck
> mucheck (testSummary::[MutantFilename] -> [InterpreterOutput Result] -> TSum) "qsort" "Examples/QuickCheckTest.hs" "Examples.QuickCheckTest" ["quickCheckResult idEmpProp","quickCheckResult revProp","quickCheckResult modelProp"]
```
(4) Or use it from ghci, after copying one of the examples like this.

## QuickCheck
```
$ cp ../mucheck-quickcheck/Examples/QuickCheckTest.hs Examples/QuickCheckTest.hs
$ ghci
> :m + Test.MuCheck
> :m + Test.MuCheck.TestAdapter
> :m + Test.MuCheck.TestAdapter.QuickCheck
> :m + Test.QuickCheck.Test
> mucheck (testSummary::[MutantFilename] -> [InterpreterOutput QuickCheckSummary] -> TSum) "qsort" "Examples/QuickCheckTest.hs" "Examples.QuickCheckTest" ["quickCheckResult idEmpProp","quickCheckResult revProp","quickCheckResult modelProp"]
```
## SmallCheck
```
$ cp ../mucheck-smallcheck/Examples/SmallCheckTest.hs Examples/SmallCheckTest.hs
$ ghci
> :m + Test.MuCheck
> :m + Test.MuCheck.TestAdapter
> :m + Test.MuCheck.TestAdapter.SmallCheck
> :m + Test.SmallCheck
> mucheck (testSummary::[MutantFilename] -> [InterpreterOutput SmallCheckSummary] -> TSum) "qsort" "Examples/SmallCheckTest.hs" "Examples.SmallCheckTest" ["smallCheckResult idEmpProp"]
```

## HUnit
```
$ cp ../mucheck-hunit/Examples/HUnitTest.hs Examples/HUnitTest.hs
$ ghci
> :m + Test.MuCheck
> :m + Test.MuCheck.TestAdapter
> :m + Test.MuCheck.TestAdapter.HUnit
> :m + Test.HUnit
> mucheck (testSummary::[MutantFilename] -> [InterpreterOutput HUnitSummary] -> TSum) "qsort" "Examples/HUnitTest.hs" "Examples.HUnitTest" ["runTestTT tests"]
```

## Hspec
```
$ cp ../mucheck-hspec/Examples/HspecTest.hs Examples/HspecTest.hs
$ ghci
> :m + Test.MuCheck
> :m + Test.MuCheck.TestAdapter
> :m + Test.MuCheck.TestAdapter.Hspec
> :m + Test.Hspec.Core.Runner
> mucheck (testSummary::[MutantFilename] -> [InterpreterOutput HspecSummary] -> TSum) "qsort" "Examples/HspecTest.hs" "Examples.HspecTest" ["spec"]
```

