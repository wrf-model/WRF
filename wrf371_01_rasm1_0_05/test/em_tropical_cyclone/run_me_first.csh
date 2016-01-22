#!/bin/csh

echo Setting up tropical_cyclone case by linking data files into this directory

echo linking to LANDUSE.TBL in ../../run directory

ln -sf ../../run/LANDUSE.TBL .
ln -sf ../../run/RRTM_DATA .

echo done
