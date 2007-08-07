DERIVE=derive

all: warn small quickcheck

warn:
	@echo make sure you have installed the derive you wish to test!

small:
	$(DERIVE) -mSmallInstancess -iSmall -iData.Generics -iData.Monoid \
	        -o Small.out.hs Small.hs
	diff -u Small.expected.hs Small.out.hs
	ghc --make -c Small.out.hs

quickcheck:
	$(DERIVE) -mQuickCheckInstances -iQuickCheck -iTest.QuickCheck \
	        -o QuickCheck.out.hs QuickCheck.hs
	diff -u QuickCheck.expected.hs QuickCheck.out.hs
