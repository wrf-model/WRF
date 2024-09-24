### Install and compiling the WRF-Model  ###

## Installing the pre-requisites: ##

```Shell
    sudo apt install csh gfortran m4 mpich libhdf5-mpich-dev libpng-dev libnetcdff-dev netcdf-bin ncl-ncarg build-essential
```

## Install Jasperlib: ## 

```Shell
    wget https://www.ece.uvic.ca/~frodo/jasper/software/jasper-1.900.29.tar.gz
    tar xvf jasper-1.900.29.tar.gz 
    cd jasper-1.900.29/
    ./configure --prefix=/opt/jasper-1.900.29
    make
    sudo make install
```