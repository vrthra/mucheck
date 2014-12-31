hlint:
	(cd support; ~/.cabal/bin/hlint `find ../src -name \*.hs`)

clean-sandbox:
	- cabal sandbox hc-pkg unregister MuCheck-QuickCheck
	- cabal sandbox hc-pkg unregister MuCheck-SmallCheck
	- cabal sandbox hc-pkg unregister MuCheck-HUnit
	- cabal sandbox hc-pkg unregister MuCheck-Hspec
	- cabal sandbox hc-pkg unregister MuCheck

sandbox:
	mkdir -p ../mucheck-sandbox
	cabal sandbox init --sandbox ../mucheck-sandbox

build:
	cabal build
