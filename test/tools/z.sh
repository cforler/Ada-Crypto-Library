#!/bin/sh
for i in *.ad?; do	
    TEMP=`mktemp`
    cat $i | grep -vi AUnit.Test_Cases.Registration > $TEMP;
    mv $TEMP $i
done