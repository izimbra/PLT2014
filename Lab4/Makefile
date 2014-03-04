bnfc:
	bnfc FP.cf
	happy -gca ParFP.y
	alex -gi LexFP.x

comp:
	ghc --make lab4.hs -o lab4

all: bnfc comp

clean:
	-rm -f lab4
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocCPP.ps
	-rm -f *.x *.y *.info
	-rm -f *.bak
	-rm -f *.txt *.tex
	-rm -f Err*.hs Par*.hs Abs*.hs Print*.hs Lex*.hs Test*.hs Skel*.hs
