#!/bin/sh

for f in */* ; do
  if [ -d $f ] ; then
    for x in "fire.exe" "wrf.exe" "ideal.exe" "fire_input.nc" ; do
      ln -sf ../${x} ${f}/${x}
    done
  fi
done
