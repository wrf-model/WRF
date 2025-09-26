## Installing CRYOWRF v1.0

This document gives instructions about installing CRYOWRF v1.0 on an local linux machine or on Piz Daint (a cluster) 



### Installing on Linux (Ubuntu)

#### Prerequisities 

* cmake
* csh
* m4
* gfortran

Install the above as ``` sudo apt install cmake csh m4 gfortran ```

#### Installation steps

1. Download CRYOWRF v1.0

```
git clone https://github.com/vsharma-next/CRYOWRF.git
```

2. Installing a local copy of netcdf (v 4.1.3) and MPI (mpich, v 3.0.4) [(1)](#notes)
```
cd ./libraries
source ./install_libs.sh 
cd ..
```

3. Installing meteoio, snowpack and the coupler
```
source ./compiler_snow_libs.sh
```

4. Compiling WRF
```
./configure [ choose option 34 or 35 ]
./compile em_real -j 8
cd ..
```

5. Compiling WPS
  
```
./configure [ choose option 2 ]
./compile
```

#### Notes
Whenever running CRYOWRF simulations, safe practice is to repeat this step every time the terminal is opened




### Installing on Piz Daint 

#### Prerequisities 
The prereqs needed for compiling CRYOWRF on Piz Daint (or any other similar cray cluster) is 
* loading the modules for netcdf and various flavours (with netcdf-4, HDF5 compression, parallel netcdf etc) 
```
  module load daint-mc
  module switch PrgEnv-cray PrgEnv-intel
  module unload cray-libsci
  module load cray-netcdf-hdf5parallel
  module load cray-parallel-netcdf
  module load cray-hdf5-parallel
```
* exporting environment variables (in bash) 
```bash
  export NETCDF=$NETCDF_DIR
  export PNETCDF=$PARALLEL_NETCDF_DIR
  export HDF5=$HDF5_DIR

  export WRF_EM_CORE=1
  export WRF_NMM_CORE=0
  export WRF_DA_CORE=0

  export WRF_CHEM=0
  export WRF_KPP=0

  export NETCDF4=1
  export WRFIO_NCD_LARGE_FILE_SUPPORT=1
  export WRFIO_NCD_NO_LARGE_FILE_SUPPORT=0
```

**It is useful to place these commands in the .bashrc file to have these steps taken care of already**

#### Installation steps

1. Download CRYOWRF v1.0

```
git clone https://github.com/vsharma-next/CRYOWRF.git
```

2. Installing meteoio, snowpack and the coupler
```
source ./compiler_snow_libs.sh
```

3. Compiling WRF
```
./configure [ choose option 50 or 51 ]
./compile em_real -j 8
cd ..
```

4. Compiling WPS
  
```
./configure [ choose option 38 ]
./compile
```

