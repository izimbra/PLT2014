#!/bin/bash

TESTS="testsuite/good/core001.cc"

# 2, 3, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20,
# 102, 
bnfc --make CPP.cf

make all

ghc --make -o lab2 lab2.hs

./lab2 "testsuite/bad/bad001.cc"
./lab2 "testsuite/bad/bad002.cc"
./lab2 "testsuite/bad/bad003.cc"
./lab2 "testsuite/bad/bad004.cc"
./lab2 "testsuite/bad/bad005.cc"
./lab2 "testsuite/bad/bad006.cc"
./lab2 "testsuite/bad/bad007.cc"
./lab2 "testsuite/bad/bad008.cc"
./lab2 "testsuite/bad/bad009.cc"
./lab2 "testsuite/bad/bad010.cc"
./lab2 "testsuite/bad/bad011.cc"
./lab2 "testsuite/bad/bad012.cc"
./lab2 "testsuite/bad/bad013.cc"
./lab2 "testsuite/bad/bad015.cc"
./lab2 "testsuite/bad/bad016.cc"
./lab2 "testsuite/bad/bad017.cc"
./lab2 "testsuite/bad/bad018.cc"
./lab2 "testsuite/bad/bad019.cc"
./lab2 "testsuite/bad/bad020.cc"
./lab2 "testsuite/bad/bad022.cc"
./lab2 "testsuite/bad/bad023.cc"
./lab2 "testsuite/bad/bad026.cc"
./lab2 "testsuite/bad/bad027.cc"


