#!/bin/bash

TESTS="testsuite/good/core001.cc"

# 2, 3, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20,
# 102, 
bnfc --make CPP.cf

make all

ghc --make -o lab2 lab2.hs

./lab2 "testsuite/good/core001.cc"
./lab2 "testsuite/good/core002.cc"
./lab2 "testsuite/good/core003.cc"
./lab2 "testsuite/good/core005.cc"
./lab2 "testsuite/good/core006.cc"
./lab2 "testsuite/good/core007.cc"
./lab2 "testsuite/good/core009.cc"
./lab2 "testsuite/good/core010.cc"
./lab2 "testsuite/good/core012.cc"
./lab2 "testsuite/good/core013.cc"
./lab2 "testsuite/good/core014.cc"
./lab2 "testsuite/good/core015.cc"
./lab2 "testsuite/good/core016.cc"
./lab2 "testsuite/good/core017.cc"
./lab2 "testsuite/good/core019.cc"
./lab2 "testsuite/good/core020.cc"

./lab2 "testsuite/good/core102.cc"
./lab2 "testsuite/good/core103.cc"
./lab2 "testsuite/good/core104.cc"
./lab2 "testsuite/good/core105.cc"
./lab2 "testsuite/good/core106.cc"
./lab2 "testsuite/good/core107.cc"
./lab2 "testsuite/good/core108.cc"
./lab2 "testsuite/good/core109.cc"
./lab2 "testsuite/good/core110.cc"
./lab2 "testsuite/good/core111.cc"
./lab2 "testsuite/good/core112.cc"
./lab2 "testsuite/good/core113.cc"

./lab2 "testsuite/good/good01.cc"
./lab2 "testsuite/good/good03.cc"
./lab2 "testsuite/good/good05.cc"
./lab2 "testsuite/good/good07.cc"
./lab2 "testsuite/good/good09.cc"
./lab2 "testsuite/good/good11.cc"
./lab2 "testsuite/good/good13.cc"
./lab2 "testsuite/good/good15.cc"
./lab2 "testsuite/good/good17.cc"

