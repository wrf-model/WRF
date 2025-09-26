#!/bin/bash

# Script to install hdf5 and netCDF4 libraries on a Linux Ubuntu system
# After: https://code.google.com/p/netcdf4-python/wiki/UbuntuInstall
# And http://unidata.github.io/netcdf4-python/ 

# You can check for newer version of the programs on 
# ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4/
# and other sources

BASHRC="~/.bashrc"

mkdir -p ./local_libs
cd ./local_libs

install_lib_path=$(pwd)

cd ..

export DIR=$install_lib_path
export CC=gcc
export CXX=g++
export FC=gfortran
export FCFLAGS=-m64
export F77=gfortran
export FFLAGS=-m64
export PATH=$DIR/bin:$PATH
export NETCDF=$DIR

# Install Netcdf
v=4.1.3
file=netcdf-${v}.tar.gz
if [[ -f "$file" ]]; then
   echo "tarball $file already downloaded !"
else
   echo "downloading $file"
   wget http://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-${v}.tar.gz
fi

if [[ -d "netcdf-${v}" ]]; then
   echo "directory netcdf-${v} already extracted !"
   if [[ -f "./netcdf-${v}/Makefile" ]]; then
      cd netcdf-${v}
      make install
      cd ..
   else
      cd netcdf-${v}
      ./configure --disable-dap --disable-netcdf-4 --disable-shared --prefix=$install_lib_path
      make install
      cd ..
   fi
else
   tar -xf netcdf-${v}.tar.gz && cd netcdf-${v}
   ./configure --disable-dap --disable-netcdf-4 --disable-shared --prefix=$install_lib_path
   make install
   cd ..
fi

# Install mpich
v=3.0.4
file=mpich-${v}.tar.gz
if [[ -f "$file" ]]; then
   echo "tarball $file already downloaded !"
else
   echo "downloading $file"
   wget http://www.mpich.org/static/tarballs/${v}/$file
fi

if [[ -d "mpich-${v}" ]]; then
   echo "directory mpich-${v} already extracted !"
   if [[ -f "./mpich-${v}/Makefile" ]]; then
      cd mpich-${v}
      make install
      cd ..
   else
      cd mpich-${v}
      ./configure --prefix=$install_lib_path
      make
      make install
      cd ..
   fi
else
   echo "extracting mpich-${v}"
   tar -xf $file && cd mpich-${v}
   ./configure --prefix=$install_lib_path
   make
   make install
   cd ..
fi

  export WRF_EM_CORE=1
  export WRF_NMM_CORE=0
  export WRF_DA_CORE=0

  export WRF_CHEM=0
  export WRF_KPP=0
  export WRFIO_NCD_LARGE_FILE_SUPPORT=1

export NETCDF_classic=1
