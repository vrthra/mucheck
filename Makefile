build:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests
	cabal build

hlint:
	(cd support; ~/.cabal/bin/hlint `find ../src -name \*.hs`)

clean-sandbox:
	- cabal sandbox hc-pkg unregister DMuCheck
	- cabal sandbox hc-pkg unregister MuCheck-QuickCheck
	- cabal sandbox hc-pkg unregister MuCheck-SmallCheck
	- cabal sandbox hc-pkg unregister MuCheck-HUnit
	- cabal sandbox hc-pkg unregister MuCheck-Hspec
	- cabal sandbox hc-pkg unregister MuCheck

sandbox:
	mkdir -p ../mucheck-sandbox
	cabal sandbox init --sandbox ../mucheck-sandbox

install:
	cabal install

prepare:
	cabal haddock
	cabal check
	cabal test
	cabal sdist

clean:
	- rm Examples/*
	- rm *.log

.PHONY: test
test:
	cabal test

# :set -package QuickCheck
# :m +Test.QuickCheck
testrepl:
	cabal repl --ghc-option='-package QuickCheck-2.6'

hpcex:
	- rm Examples/*.hi Examples/*.o *.tix tests
	ghc -fhpc Examples/Main.hs --make -o tests

