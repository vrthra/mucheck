To begin:

# Install required packages:

```
$ cabal update
$ cabal install cabal-install
$ cabal install --only-dependencies --enable-tests
```
# Use the provided sample adapter

We are going to use the simplistic `Test.MuCheck.TestAdapter.AssertCheck`
module for our example.

First, we need the coverage information of our tests. While it is not
a required part, it is *strongly* recommended that you provide the coverage
information of your module using `-fhpc` flag to ghc. MuCheck can cut down
on the number of mutants generated drastically by using the `HPC` information.
In order to run `sample-test` with coverage enabled we have to pass the `--enable-coverage` flag to cabal.
The coverage information is written to the file`sample-test.tix` in the current directory.

```
cabal run --enable-coverage exe:sample-test
```

We are now ready to run mucheck, let us run it.

```
cabal run mucheck -- -tix sample-test.tix Examples/AssertCheckTest.hs
```

This results (after a sufficiently large time) in

```
Total mutants: 19 (basis for %)
        Covered: 13
        Sampled: 13
        Errors: 0  (0%)
        Alive: 1/19
        Killed: 12/19 (63%)
```
This suggests that initially `19` mutants were generated, which was reduced to
just 13 mutants that contained mutations where test suites can find them.

The run resulted in just one of the mutants being alive, with a mutation score
of 63%.

All the steps above can also be done by running this make command in MuCheck
directory.

```
make hpcex
```

## Important

Currently `MuCheck` is restricted to running mutation analysis on a single
module at a time. In order for it to work, the module being tested should
contain the tests also. Further the tests should be annotated with
```
{-# ANN <function name> "Test" #-}
```
If you have supporting functions, they should be annotated with "TestSupport".
This allows MuCheck to find the tests to run, and also to figure out which of
the functions to leave alone while mutating.

Take a look at the `Examples/AssertCheckTest.hs` to see how mucheck expects the
module to be.


