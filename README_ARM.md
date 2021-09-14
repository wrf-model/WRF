# WRF 4.2 Porting Guide(CentOS 7.6)
## Introduction
The Weather Research and Forecasting (WRF) Model can be used for fine-scale weather simulation and forecasting, which is one of the important application scenarios of high-performance computing (HPC).

For more information about the WRF, visit the official WRF website.

Programming language: C/Fortran

Brief description: mesoscale weather forecasting model

Open-source protocol: public domain

##### Recommended Version
The recommended version is WRF V4.2

## Environment Requirements

### Software Requirements
| Item  | Version  |  Download Address |
| ------------ | ------------ | ------------ |
|  OPENMPI  | 4.0.3  | https://download.open-mpi.org/release/open-mpi/v4.0/openmpi-4.0.3.tar.gz  |
|  HDF5     | 1.10.6 | https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.6/src/  |
|  PNETCDF  | 1.12.1 | https://parallel-netcdf.github.io/wiki/Download.html |
|  NETCDF-C | 4.7.3  | https://github.com/Unidata/netcdf-c/releases/tag/v4.7.3 |
|  NETCDF-F | 4.5.2  | https://github.com/Unidata/netcdf-fortran/releases/tag/v4.5.2  |
|  WRF      | 4.2    | https://github.com/wrf-model/WRF/archive/refs/tags/v4.2.tar.gz  |

### OS Requirements
| Item  | Version  | How to Obtain  |
| ------------ | ------------ | ------------ |
|  CentOS | 7.6  |  https://www.centos.org/download/ |
| Kernel  | 4.14.0-115  |  Included in the OS image. |

## Configuring the Compilation Environment

### Installing dependencies

```shell
yum install time -y
yum install curl* -y
yum install libcurl-devel -y
yum install wget -y
yum install csh -y
yum install zlib* -y
yum install perl -y
yum install make -y
yum install m4 -y
yum install file -y
```

### Installing GNU 9.3

```shell
yum install -y centos-release-scl
yum install -y devtoolset-9-gcc
yum install -y devtoolset-9-gcc-c++
yum install -y devtoolset-9-gcc-gfortran
yum install -y devtoolset-9-binutils
scl enable devtoolset-9 bash
echo "source /opt/rh/devtoolset-9/enable" >> /etc/profile
```

### Installing Open MPI
1. Run the following command to install the system dependency package:

```shell
yum install libxml2* systemd-devel.aarch64 numa* -y
```

2. Run the following commands to install Open MPI:

```shell
wget https://download.open-mpi.org/release/open-mpi/v4.0/openmpi-4.0.3.tar.gz
tar -zxvf openmpi-4.0.3.tar.gz
cd openmpi-4.0.3
./configure --prefix=/path/to/OPENMPI --enable-pretty-print-stacktrace --enable-orterun-prefix-by-default  --with-cma --enable-mpi1-compatibility --enable-mpi-fortran=yes
make -j $(nproc) all
make install
```

3. Configure environment variables:

```shell
export PATH=/path/to/OPENMPI/bin:$PATH
export LD_LIBRARY_PATH=/path/to/OPENMPI/lib:$LD_LIBRARY_PATH
```

### Installing HDF5
1. Run the following commands to install HDF5:

```shell
wget https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.6/src/hdf5-1.10.6.tar.gz
tar -zxvf hdf5-1.10.6.tar.gz
cd hdf5-1.10.6
mkdir -p /path/to/HDF5
./configure --prefix=/path/to/HDF5 --build=aarch64-unknown-linux-gnu --enable-fortran --enable-static=yes --enable-parallel --enable-shared CC=mpicc CXX=mpicxx FC=mpifort F77=mpifort
make -j $(nproc)
make install
```

2. Configure environment variables:

```shell
export PATH=/path/to/HDF5/bin:$PATH
export LD_LIBRARY_PATH=/path/to/HDF5/lib:$LD_LIBRARY_PATH
```

### Installing PNETCDF
1. Run the following commands to install PNETCDF:

```shell
wget https://parallel-netcdf.github.io/Release/pnetcdf-1.12.1.tar.gz
tar -zxvf pnetcdf-1.12.1.tar.gz
cd pnetcdf-1.12.1
mkdir -p /path/to/PNETCDF

./configure --prefix=/path/to/PNETCDF --build=aarch64-unknown-linux-gnu CFLAGS="-fPIC -DPIC" CXXFLAGS="-fPIC -DPIC" FCFLAGS="-fPIC" FFLAGS="-fPIC" CC=mpicc CXX=mpicxx FC=mpifort F77=mpifort
make -j $(nproc)
make install
```

2. Configure environment variables:

```shell
export PATH=/path/to/PNETCDF/bin:$PATH
export LD_LIBRARY_PATH=/path/to/PNETCDF/lib:$LD_LIBRARY_PATH
```

### Installing NETCDF-C
1. Run the following commands to install NETCDF-C:

```shell
wget -O netcdf-c-4.7.3.tar.gz https://github.com/Unidata/netcdf-c/archive/refs/tags/v4.7.3.tar.gz
tar -zxvf netcdf-c-4.7.3.tar.gz
cd netcdf-c-4.7.3
mkdir -p /path/to/NETCDF
./configure --prefix=/path/to/NETCDF --build=aarch64-unknown-linux-gnu --enable-shared --enable-netcdf-4 --enable-dap --with-pic --disable-doxygen --enable-static --enable-pnetcdf --enable-largefile CC=mpicc CXX=mpicxx FC=mpifort F77=mpifort CPPFLAGS="-I/path/to/HDF5/include -I/path/to/PNETCDF/include" LDFLAGS="-L/path/to/HDF5/lib -L/path/to/PNETCDF/lib -Wl,-rpath=/path/to/HDF5/lib -Wl,-rpath=/path/to/PNETCDF/lib" CFLAGS="-L/path/to/HDF5/lib -L/path/to/PNETCDF/lib -I/path/to/HDF5/include -I/path/to/PNETCDF/include"
make -j $(nproc)
make install
```

2. Configure environment variables:

```shell
export PATH=/path/to/NETCDF/bin:$PATH
export LD_LIBRARY_PATH=/path/to/NETCDF/lib:$LD_LIBRARY_PATH
```

### Installing NETCDF-FORTRAN
1. Run the following commands to install NETCDF-FORTRAN:

```shell
wget -O netcdf-fortran-4.5.2.tar.gz https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.5.2.tar.gz
tar -zxvf netcdf-fortran-4.5.2.tar.gz
cd netcdf-fortran-4.5.2
./configure --prefix=/path/to/NETCDF --build=aarch64-unknown-linux-gnu --enable-shared --with-pic --disable-doxygen --enable-largefile --enable-static CC=mpicc CXX=mpicxx FC=mpifort F77=mpifort CPPFLAGS="-I/path/to/HDF5/include -I/path/to/NETCDF/include" LDFLAGS="-L/path/to/HDF5/lib -L/path/to/NETCDF/lib -Wl,-rpath=/path/to/HDF5/lib -Wl,-rpath=/path/to/NETCDF/lib" CFLAGS="-L/path/to/HDF5/lib -L/path/to/NETCDF/lib -I/path/to/HDF5/include -I/path/to/NETCDF/include" CXXFLAGS="-L/path/to/HDF5/lib -L/path/to/NETCDF/lib -I/path/to/HDF5/include -I/path/to/NETCDF/include" FCFLAGS="-L/path/to/HDF5/lib -L/path/to/NETCDF/lib -I/path/to/HDF5/include -I/path/to/NETCDF/include"
make -j $(nproc)
make install
```

## Compiling and Installing WRF
1. Run the following command to obtain the source code package:

```shell
wget -O wrf-v4.2.tar.gz https://github.com/wrf-model/WRF/archive/refs/tags/v4.2.tar.gz
```

2. Decompress the WRF installation package:

```shell
tar -zxvf wrf-v4.2.tar.gz
```

3. Run the following command to switch to WRF source code directory:

```shell
cd WRF-4.2
```

4. Run the following commands to configure the pre-compilation environment:

```shell
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export NETCDF=/path/to/NETCDF
export HDF5=/path/to/HDF5
export PNETCDF=/path/to/PNETCDF
export CPPFLAGS="-I$HDF5/include -I$PNETCDF/include -I$NETCDF/include"
export LDFLAGS="-L$HDF5/lib -L$PNETCDF/lib -L$NETCDF/lib -lnetcdf -lnetcdff -lpnetcdf -lhdf5_hl -lhdf5 -lz"
```

5. Edit the arch/configure.defaults file and add the following information before line 1977:


        ############################################################
        #ARCH   Linux   aarch64,gnu OpenMPI #serial smpar dmpar dm+sm
        #
        DESCRIPTION     =       GNU ($SFC/$SCC)
        DMPARALLEL      =        1
        OMPCPP          =        -D_OPENMP
        OMP             =        -fopenmp
        OMPCC           =        -fopenmp
        SFC             =       gfortran
        SCC             =       gcc
        CCOMP           =       gcc
        DM_FC           =       mpif90 -f90=$(SFC)
        DM_CC           =       mpicc -cc=$(SCC) -DMPI2_SUPPORT
        FC              =       CONFIGURE_FC
        CC              =       CONFIGURE_CC
        LD              =       $(FC)
        RWORDSIZE       =       CONFIGURE_RWORDSIZE
        PROMOTION       =       #-fdefault-real-8
        ARCH_LOCAL      =       -DNONSTANDARD_SYSTEM_SUBR  -DWRF_USE_CLM
        CFLAGS_LOCAL    =       -w -O3 -c -march=armv8.2-a
        LDFLAGS_LOCAL   =
        CPLUSPLUSLIB    =
        ESMF_LDFLAG     =      $(CPLUSPLUSLIB)
        FCOPTIM         =       -O3 -ftree-vectorize -funroll-loops -march=armv8.2-a
        FCREDUCEDOPT    =       $(FCOPTIM)
        FCNOOPT         =       -O0
        FCDEBUG         =       # -g $(FCNOOPT)  # -fbacktrace -ggdb-fcheck=bounds,do,mem,pointer -ffpe-trap=invalid,zero,overflow
        FORMAT_FIXED    =       -ffixed-form
        FORMAT_FREE     =       -ffree-form -ffree-line-length-none
        FCSUFFIX        =
        BYTESWAPIO      =       -fconvert=big-endian -frecord-marker=4
        FCBASEOPTS_NO_G =       -w $(FORMAT_FREE) $(BYTESWAPIO)
        FCBASEOPTS      =       $(FCBASEOPTS_NO_G) $(FCDEBUG)
        MODULE_SRCH_FLAG =
        TRADFLAG        =      -traditional
        CPP             =      /lib/cpp -P
        AR              =      ar
        ARFLAGS         =      ru
        M4              =      m4 -G
        RANLIB          =      ranlib
        RLFLAGS         =
        CC_TOOLS        =      $(SCC)

6. Run the following command to generate a configuration file:

```shell
./configure
```

When asked for the Linux aarch64 options select `4` (dm+sm).
When asked whether to compile for nesting select `1` (basic).

7. Run the following command to perform compilation and installation:

```shell
./compile em_real 2>&1 | tee -a compile.log
```

8. Check whether the installation is successful:

```shell
ls main
```

If wrf.exe file is generated then the installation is successful! 
