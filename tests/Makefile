DERIVE=../dist/build/derive/derive

all: small big

small:
	$(DERIVE) -mSmallInstancess -iSmall -iData.Generics -o Small.out.hs \
		Small.hs
	diff -u Small.expected.hs Small.out.hs
	ghc --make -c Small.out.hs

big:
	true
