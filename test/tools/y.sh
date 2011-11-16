#!/bin/sh

for i in *.tex; do	
TEMP=`mktemp`
sed s/"[a-zA-Z]*_To_"/To_/g $i > $TEMP;
mv $TEMP $i
done