#!/bin/csh

echo Setting up seabreeze2d_x case by linking data files into this directory

echo linking to some physics data files in ../../run directory

ln -sf ../../run/LANDUSE.TBL .
ln -sf ../../run/RRTM_DATA .
ln -sf ../../run/RRTMG_LW_DATA .
ln -sf ../../run/RRTMG_SW_DATA .

echo done
