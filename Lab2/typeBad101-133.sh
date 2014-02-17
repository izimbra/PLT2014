#!/bin/bash

TESTS="testsuite/good/core001.cc"

# 2, 3, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20,
# 102, 
bnfc --make CPP.cf

make all

ghc --make -o lab2 lab2.hs

./lab2 "testsuite/bad/bad101.cc"
./lab2 "testsuite/bad/bad102.cc"
./lab2 "testsuite/bad/bad113.cc"
./lab2 "testsuite/bad/bad114.cc"
./lab2 "testsuite/bad/bad115.cc"
./lab2 "testsuite/bad/bad116.cc"
./lab2 "testsuite/bad/bad117.cc"
./lab2 "testsuite/bad/bad118.cc"
./lab2 "testsuite/bad/bad119.cc"
./lab2 "testsuite/bad/bad120.cc"
./lab2 "testsuite/bad/bad121.cc"
./lab2 "testsuite/bad/bad122.cc"
./lab2 "testsuite/bad/bad123.cc"
./lab2 "testsuite/bad/bad124.cc"
./lab2 "testsuite/bad/bad125.cc"
./lab2 "testsuite/bad/bad126.cc"
./lab2 "testsuite/bad/bad127.cc"
./lab2 "testsuite/bad/bad128.cc"
./lab2 "testsuite/bad/bad129.cc"
./lab2 "testsuite/bad/bad130.cc"
./lab2 "testsuite/bad/bad131.cc"
./lab2 "testsuite/bad/bad132.cc"
./lab2 "testsuite/bad/bad133.cc"


