grm = Fun
dir = $(grm)
bnfc_output = $(patsubst %,$(dir)/%,Abs.hs ErrM.hs Print.hs Lex.x Par.y Test.hs)
alex_file = $(dir)/Lex
happy_file = $(dir)/Par
test_file = lab4

bnfc=$(HOME)/.cabal/bin/bnfc
BNFC_OPTS =
  # --java
  # --profile # DOES NOT WORK
  # --haskell-gadt

.PHONY: build test

default : test

ship : hands-on-lab4.zip

hands-on-lab4.zip : Makefile lab4.hs Interpreter.hs InterpreterNotReader.hs Fun.cf test/*.fun
	zip $@ $^

build : $(alex_file).hs $(happy_file).hs $(test_file)

lab4 : lab4.hs Interpreter.hs $(alex_file).hs $(happy_file).hs
	ghc --make lab4.hs -o lab4

test : $(test_file)
	$(test_file) -v test/good0.fun
	$(test_file) -v test/good1.fun
#	$(test_file) -n test/good1.fun
#	$(test_file) -n test/good2.fun
#	$(test_file) test/good3.fun
#	$(test_file) -n test/good3.fun
#	$(test_file) test/good4.fun
#	$(test_file) -n test/good5.fun
#	$(test_file) test/good6.fun
#	$(test_file) test/good7.fun
#	$(test_file) test/good8.fun
#	$(test_file) -n test/good8.fun
#	! $(test_file) test/bad1.fun
#	! $(test_file) test/bad2.fun
#	! $(test_file) test/bad3.fun
#	! $(test_file) test/bad4.fun
#

$(bnfc_output): $(grm).cf
#	-@rm $(bnfc_output)
	$(bnfc) $(BNFC_OPTS) -d $<

$(alex_file).hs: $(alex_file).x
	alex --ghc $<

$(happy_file).hs: $(happy_file).y
	happy --ghc --coerce --array -iGRM.txt $<

.PHONY: clean debug

debug:
	@echo "bnfc_output = $(bnfc_output)"

clean:
	rm -rf bnfc

# EOF
