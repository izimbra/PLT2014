all: comp

bnfc:
	bnfc CPP.cf
	happy -gca ParCPP.y
	alex -g LexCPP.x

lab2: 
	ghc --make lab2.hs -o lab2

comp:
	ghc --make lab3.hs -o lab3
	javac Runtime.java

clean:
	-rm -f lab2
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocCPP.ps
	-rm -f *.j *.class

distclean: clean
	-rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* \
	       TestCPP ErrM.* SharedString.* CPP.dtd XMLCPP.*

lab3: 
	cp CPP.cf         ../Lab3
	cp Environment.hs ../Lab3
	cp BuiltIns.hs    ../Lab3
	cp TypeCheckerA.hs ../Lab3
	cp Compiler.hs    ../Lab3
	cp lab3.hs        ../Lab3
	cp Runtime.java   ../Lab3
	cp jasmin.jar     ../Lab3

