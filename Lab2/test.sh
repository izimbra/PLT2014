#!/bin/bash

TESTS='testsuite/good/core001.cc'
GOODS='testsuite/good'
BADS='testsuite/bad'

# 2, 3, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20,
# 102, 
bnfc --make CPP.cf

make all

ghc --make -o lab2 lab2.hs

echo 'Type checking good tests...'
for FILE in `find $GOODS | grep -v -e '.cc.output' -e '.cc.input'`; do
    echo `basename $FILE && ./lab2 $FILE`
    
done

echo 'Type checking bad tests...'
for FILE in `find $BADS | grep -v -e '.cc.output' -e '.cc.input'`; do
    echo `basename $FILE && ./lab2 $FILE`
    
done


