#!/bin/csh

echo Setting up em_convrad case by linking data files into this directory

echo linking to some physics data files in ../../run directory

ln -sf ../../run/LANDUSE.TBL .
ln -sf ../../run/RRTM_DATA .
ln -sf ../../run/RRTMG_LW_DATA .
ln -sf ../../run/RRTMG_SW_DATA .
ln -sf ../../run/ozone.formatted .
ln -sf ../../run/ozone_lat.formatted .
ln -sf ../../run/ozone_plev.formatted .

echo done
