#!/bin/bash

GOODS='testsuite/good'
BADS='testsuite/bad'

#bnfc --make CPP.cf

make comp

#ghc --make -o lab2 lab2.hs

echo 'Running good tests...'
for FILE in `find $GOODS -name '*.cc'`; do
    echo `basename $FILE && ./lab3 $FILE`
    java -cp . `basename $FILE`
    
done

echo 'Type checking bad tests...'
for FILE in `find $BADS -name '*.cc'`; do
    echo `basename $FILE && ./lab3 $FILE`
    
done


