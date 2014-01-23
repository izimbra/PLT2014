all:
	happy -gca ParpltCPP.y
	alex -g LexpltCPP.x
	latex DocpltCPP.tex; dvips DocpltCPP.dvi -o DocpltCPP.ps
	ghc --make TestpltCPP.hs -o TestpltCPP
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocpltCPP.ps
distclean: clean
	-rm -f DocpltCPP.* LexpltCPP.* ParpltCPP.* LayoutpltCPP.* SkelpltCPP.* PrintpltCPP.* TestpltCPP.* AbspltCPP.* TestpltCPP ErrM.* SharedString.* pltCPP.dtd XMLpltCPP.* Makefile*

