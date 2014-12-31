# TODO list

* Add a json option for stdargs
* Add a funtion to filter mutants rather than just a fraction
* Add support for other test frameworks
** tasty
** test-framework
* (Longer term) Read all source files from a root directory, and execute
  the entire test suite on all functions (minimal manual oversight)
  The user should be able to just say `mucheck quickcheck` or `mucheck hspec`
  or even just `mucheck` (using cabal test) and mucheck should compute the
  mutation score of the entire project.
* Provide the complete test matrix (what tests killed which mutants)
* Avoid writing mutants to disk unless asked for.
* Parallelize mutation analysis
* Allow specifying which higher order mutants to include, or how to mutate depending on the context.
