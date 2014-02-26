all:
	bnfc CPP.cf
	happy -gca ParCPP.y
	alex -g LexCPP.x
	ghc --make lab2.hs -o lab2

comp:
	ghc --make lab3.hs -o lab3
clean:
	-rm -f lab2
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocCPP.ps

distclean: clean
	-rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* CPP.dtd XMLCPP.*

lab3: 
    cp TypeChecker.hs ../Lab3
    cp Runtime.java   ../Lab3
    cp lab3.hs        ../Lab3
    cp Environment.hs ../Lab3
    cp CPP.cf         ../Lab3
    cp Compiler.hs    ../Lab3
    cp jasmin.jar     ../Lab3

    
