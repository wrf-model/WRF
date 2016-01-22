#!/bin/bash
#
gfortran -c -g f90split.f90 >& compiler.txt
if [ $? -ne 0 ]; then
  echo "Errors compiling f90split.f90"
  exit
fi
rm compiler.txt
#
gfortran f90split.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading f90split.o"
  exit
fi
rm f90split.o
#
chmod ugo+x a.out
# mv a.out ~/bin/$ARCH/f90split
mv a.out f90split.exe

#
echo "Program installed as f90split.exe"
