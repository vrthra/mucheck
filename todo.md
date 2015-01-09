# TODO list

### Add a json option for stdargs

### Add support for other test frameworks

#### Tasty
* Currently not done because it is hard to figure out the result of a test run
  from Tasty. It requires mucking around with callbacks that requires some
  research.

#### Test-Framework
* Unsure of the livliness of this project.

### Provide the complete test matrix (what tests killed which mutants)
* Requires changing the way fullSummary is done. Currently it looks at the last
  mutant to figure out whether a mutant is dead or alive. This is trivial to
  implement in isolation. Perhaps consider providing it as a branch/patch.

### Avoid writing mutants to disk unless asked for.
* Unfortunately Hint does not allow us to evaluate a module. So Unless we want
  to use GHC Api directly, we are stuck.

* Find a way for using GHC API to load the source file without writing it to
  disk first (even better, if we can make load the AST :: Decl directly with
  GHC API without converting to string first)

### Simpler invocation (Longer term)
* Read all source files from a root directory, and execute
  the entire test suite on all functions (minimal manual oversight)
  The user should be able to just say `mucheck quickcheck` or `mucheck hspec`
  or even just `mucheck` (using cabal test) and mucheck should compute the
  mutation score of the entire project.

### Parallelize mutation analysis without relying on d-mucheck (Longer Term)

* GHC Hint is not [thread safe](https://ghc.haskell.org/trac/ghc/ticket/3373)
  So just using Parallel mapM for mutation eval not work, although test suites
  themselves can be multi threaded in their executions.
  Done by forking processes - see d-mucheck

* We could look at simple forking and networking/file as a means of sharing
  mutant and results.

### Allow specifying which higher order mutants to include, or how to mutate
  depending on the context.

### Make genMutants lazy and randomized, and get runCodeOnMutants to report the
  current progress and mutation percentage online.

### Implement statement deletion by using default constructors where it is
  possible.

### Currently, _each_ operator traverses the source code three times
  * First trying to generate MuOp s
  * Second when trying to determine if the MuOp is relevant (isRelevantOp)
  * Third when applying the MuOp to produce Mutant

### We should verify the use of once on boolNegate

### put the underlying modulefile into a monad so can avoid passing it around.
