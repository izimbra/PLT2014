#!/bin/bash

make distclean
bnfc -m --haskell pltCPP.cf
make
./TestpltCPP hello.cc
