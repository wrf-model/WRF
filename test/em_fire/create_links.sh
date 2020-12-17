#!/bin/sh

# Create directories with examples and link files there
# This script is executed from compile em_fire
# The directories are destroyed by clean -a

mkdir -p two_fires rain

( cd two_fires; ln -s ../../../main/ideal.exe . ;  ln -s ../../../main/wrf.exe . ; \
  ln -s ../input_sounding_two_fires input_sounding ; \
  ln -s ../namelist.fire_two_fires namelist.fire ; \
  ln -s ../namelist.input_two_fires namelist.input )

( cd rain; ln -s ../../../main/ideal.exe . ;  ln -s ../../../main/wrf.exe . ; \
  ln -s ../input_sounding_rain input_sounding ; \
  ln -s ../namelist.fire_fmc namelist.fire ; \
  ln -s ../namelist.input_rain namelist.input )
 
for i in ETAMPNEW_DATA LANDUSE.TBL RRTMG_SW_DATA URBPARM.TBL GENPARM.TBL \
RRTMG_LW_DATA SOILPARM.TBL VEGPARM.TBL
do
   ( cd rain; ln -s ../../../run/$i .)
done

