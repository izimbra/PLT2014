#!/bin/bash

TESTS="testsuite/good/core017.cc"

# 2, 3, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16
bnfc --make CPP.cf

make all

ghc --make -o lab2 lab2.hs

./lab2 $TESTS

