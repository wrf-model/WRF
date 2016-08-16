#!/bin/csh

echo Setting up seabreeze2d_x case by linking data files into this directory

echo linking to LANDUSE.TBL in ../../run directory

ln -sf ../../run/LANDUSE.TBL .
ln -sf ../../run/RRTM_DATA .
ln -sf ../../run/wind-turbine-1.tbl .

echo done
