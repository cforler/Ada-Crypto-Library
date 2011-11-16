#!/bin/sh

for i in $*
do
  TEMP='mktemp'
  cat GMGPL $i > $TEMP
  mv $TEMP $i
done