#!/bin/bash

make distclean
bnfc --haskell pltCPP.cf
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
echo "Running test 4:"
./TestpltCPP grade.cc
echo ""
echo "Done!"
echo ""
echo ""
echo "Running test 5:"
./TestpltCPP palin.cc
echo ""
echo "Done!"
echo ""
echo ""
echo "Running test 6:"
./TestpltCPP grammar.cc
echo ""
echo "Done!"
echo ""
echo ""


