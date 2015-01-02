# TODO list

* Add a json option for stdargs
* Add support for other test frameworks
** tasty
** test-framework
* Provide the complete test matrix (what tests killed which mutants)
* Avoid writing mutants to disk unless asked for.
* (Longer term) Read all source files from a root directory, and execute
  the entire test suite on all functions (minimal manual oversight)
  The user should be able to just say `mucheck quickcheck` or `mucheck hspec`
  or even just `mucheck` (using cabal test) and mucheck should compute the
  mutation score of the entire project.
* (Longer Term) Parallelize mutation analysis
** GHC Hint is not [thread safe](https://ghc.haskell.org/trac/ghc/ticket/3373)
   So just using Parallel mapM for mutation eval not work, although test suites
   themselves can be multi threaded in their executions.
** Another option is to have a set of mutation evaluation servers running, and
  send each mutation as a request to these servers, colelct the results back
  and report. This will give us the abilty to restrict the number of processes
  to a fixed number (rather than some emulation of forkMapM), and also let us
  make use of different machines too.

* Allow specifying which higher order mutants to include, or how to mutate
  depending on the context.
* Make genMutants lazy and randomized, and get runCodeOnMutants to report the
  current progress and mutation percentage online.
* Use coverage information when available (-fhpc,.hpc/*.mix) or ask to provide
  so as to remove mutants that dont come under covered portions.
* Find a way for using GHC API to load the source file without writing it to
  disk first (even better, if we can make load the AST :: Decl directly with
  GHC API without converting to string first)
