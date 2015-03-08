all: comp

bnfc:
	bnfc Fun.cf
	happy -gca ParFun.y
	alex -gi LexFun.x

comp:
	ghc --make lab4.hs -o lab4

clean:
	-rm -f lab4
	-rm -f *.log *.aux *.hi *.o *.dvi *.hs~
	-rm -f DocCPP.ps
	-rm -f *.x *.y *.info
	-rm -f *.bak
	-rm -f *.txt *.tex
	-rm -f Err*.hs Par*.hs Abs*.hs Print*.hs Lex*.hs Test*.hs Skel*.hs
	-rm -f Makefile~
