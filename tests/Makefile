DERIVE=derive

all: warn small quickcheck

warn:
	@echo make sure you have installed the derive you wish to test!

small:
	$(DERIVE) -mSmallInstances -iSmall -iData.Generics -iData.Monoid \
	        -o Small.out.hs Small.hs
	diff -u Small.expected.hs Small.out.hs
	ghc --make -c Small.out.hs

quickcheck:
	$(DERIVE) -mQuickCheckInstances -iQuickCheck -iTest.QuickCheck \
	        -o QuickCheck.out.hs QuickCheck.hs
	diff -u QuickCheck.expected.hs QuickCheck.out.hs

dataderiving:
	cpp M1.hs > M1.tmp.hs
	cpp M2.hs > M2.tmp.hs
	$(DERIVE) -mM1I -iM1 -iData.Generics -iData.Monoid -o M1I.hs M1.tmp.hs
	$(DERIVE) -mM2I -iM2 -iData.Generics -iData.Monoid -o M2I.hs M2.tmp.hs
	rm M1.tmp.hs M2.tmp.hs
	runhaskell -DGHC_DERIVING MTest.hs > MTest.expected.hs
	runhaskell MTest.hs > MTest.out.hs
	diff -u MTest.expected.hs MTest.out.hs
	