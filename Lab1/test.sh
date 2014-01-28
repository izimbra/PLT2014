#!/bin/bash

make distclean
bnfc -m --haskell pltCPP.cf
make
echo ""
echo "Running test 1:"
./TestpltCPP hello.cc
echo ""
echo "Done!"
echo ""
echo ""
echo "Running test 2:"
./TestpltCPP greet.cc
echo ""
echo "Done!"
echo ""
echo ""
echo "Running test 3:"
./TestpltCPP med.cc
echo ""
echo "Done!"
echo ""
echo ""


