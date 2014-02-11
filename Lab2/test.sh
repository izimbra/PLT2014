#!/bin/bash

TESTS="testsuite/good/core003.cc"

bnfc --make CPP.cf

make

ghc --make -o lab2 lab2.hs

lab2 $TESTS

