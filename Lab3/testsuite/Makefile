all: Runtime.class progs-test-lab3.hs
	ghc -threaded --make -o progs-test-lab3 progs-test-lab3.hs

Runtime.class: Runtime.java
	javac Runtime.java

clean:
	rm -f progs-test-lab3 *.o *.hi *.j */*.class */*.j
