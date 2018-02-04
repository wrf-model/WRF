#!/bin/csh
#
### PBS batch script to run an MPI application
#
#PBS -A P64000510
#PBS -l walltime=00:20:00
#PBS -l select=1:ncpus=36:mpiprocs=36
#PBS -N wrfplus
#PBS -j oe
#PBS -q premium

#run the executable

 ln -sf ../../run/RRTM_DATA_DBL      RRTM_DATA
 ln -sf ../../run/RRTMG_LW_DATA_DBL  RRTMG_LW_DATA
 ln -sf ../../run/RRTMG_SW_DATA_DBL  RRTMG_SW_DATA
 ln -sf ../../run/SOILPARM.TBL       .
 ln -sf ../../run/VEGPARM.TBL        .
 ln -sf ../../run/GENPARM.TBL        .
 ln -sf ../../run/LANDUSE.TBL        .

#mpirun ./da_wrfvar.exe
mpiexec_mpt ../../run/wrf.exe
