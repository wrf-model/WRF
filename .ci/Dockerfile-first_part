#
FROM centos:latest
MAINTAINER Dave Gill <gill@ucar.edu>

ENV WRF_VERSION 4.0.3
ENV WPS_VERSION 4.0.2
ENV NML_VERSION 4.0.2

# Set up base OS environment

RUN yum -y update
RUN yum -y install scl file gcc gcc-gfortran gcc-c++ glibc.i686 libgcc.i686 libpng-devel jasper \
  jasper-devel hostname m4 make perl tar bash tcsh time wget which zlib zlib-devel \
  openssh-clients openssh-server net-tools fontconfig libgfortran libXext libXrender \
  ImageMagick sudo epel-release git

# Newer version of GNU compiler, required for WRF 2003 and 2008 Fortran constructs

RUN yum -y install centos-release-scl \
 && yum -y install devtoolset-8 \
 && yum -y install devtoolset-8-gcc devtoolset-8-gcc-gfortran devtoolset-8-gcc-c++ \
 && scl enable devtoolset-8 bash \
 && scl enable devtoolset-8 tcsh 

RUN groupadd wrf -g 9999
RUN useradd -u 9999 -g wrf -G wheel -M -d /wrf wrfuser
RUN mkdir /wrf \
 &&  chown -R wrfuser:wrf /wrf \
 &&  chmod 6755 /wrf

# Build the libraries with a parallel Make
ENV J 4

# Build OpenMPI
RUN mkdir -p /wrf/libs/openmpi/BUILD_DIR
RUN source /opt/rh/devtoolset-8/enable \
 && cd /wrf/libs/openmpi/BUILD_DIR \
 && curl -L -O https://download.open-mpi.org/release/open-mpi/v4.0/openmpi-4.0.0.tar.gz \
 && tar -xf openmpi-4.0.0.tar.gz \
 && cd openmpi-4.0.0 \
 && ./configure --prefix=/usr/local &> /wrf/libs/build_log_openmpi_config \
 && echo dummy printout to keep travis happy openmpi config \
 && make all install \
 && echo "make all install | awk 'NR % 1000 == 0'" \
 && echo "make all install &> /wrf/libs/build_log_openmpi_make" \
 && echo dummy printout to keep travis happy openmpi make \
 && cd / \
 && rm -rf /wrf/libs/openmpi/BUILD_DIR

# Build HDF5 libraries
RUN mkdir -p /wrf/libs/hdf5/BUILD_DIR
RUN source /opt/rh/devtoolset-8/enable \
 && cd /wrf/libs/hdf5/BUILD_DIR \
 && git clone https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git \
 && cd hdf5 \
 && git checkout hdf5-1_10_4 \
 && ./configure --enable-fortran --enable-cxx --enable-shared --prefix=/usr/local/ &> /wrf/libs/build_log_hdf5_config \
 && echo dummy printout to keep travis happy hdf5 config \
 && make install &> /wrf/libs/build_log_hdf5_make \
 && echo dummy printout to keep travis happy hdf5 make \
 && rm -rf /wrf/libs/hdf5/BUILD_DIR
ENV LD_LIBRARY_PATH /usr/local/lib

# Build netCDF C libraries
RUN yum -y install libcurl-devel zlib-devel
ENV NETCDF /wrf/libs/netcdf
RUN mkdir -p ${NETCDF}/BUILD_DIR
RUN source /opt/rh/devtoolset-8/enable \
 && cd ${NETCDF}/BUILD_DIR \
 && curl -L -O https://github.com/Unidata/netcdf-c/archive/v4.6.2.tar.gz \
 && curl -L -O https://github.com/Unidata/netcdf-fortran/archive/v4.4.5.tar.gz \
 && curl -L -O https://github.com/Unidata/netcdf4-python/archive/v1.5.1rel.tar.gz \
 && tar -xf v4.6.2.tar.gz \
 && cd netcdf-c-4.6.2 \
 && ./configure --enable-shared --prefix=${NETCDF} &> /wrf/libs/build_log_ncc_config \
 && echo dummy printout to keep travis happy ncc config \
 && make install &> /wrf/libs/build_log_ncc_make \
 && echo dummy printout to keep travis happy ncc make

# Build netCDF Fortran libraries
RUN source /opt/rh/devtoolset-8/enable \
 && env \
 && cd ${NETCDF}/BUILD_DIR \
 && tar -xf v4.4.5.tar.gz \
 && cd netcdf-fortran-4.4.5/ \
 && export LD_LIBRARY_PATH=${NETCDF}/lib:${LD_LIBRARY_PATH} \
 && CPPFLAGS=-I${NETCDF}/include LDFLAGS=-L${NETCDF}/lib ./configure --enable-shared --prefix=${NETCDF} &> /wrf/libs/build_log_ncf_config \
 && echo dummy printout to keep travis happy ncf config \
 && make install &> /wrf/libs/build_log_ncf_make \
 && echo dummy printout to keep travis happy ncf make

#RUN yum -y install python-pip python-setuptools
RUN yum -y install python-pip
RUN yum -y install python-devel
#RUN pip install Cython
#RUN echo pip istalled Cython
RUN pip install --upgrade pip \
 && pip install numpy \
 && echo pip istalled numpy
#RUN pip install cython \
# && echo pip istalled cython
RUN pip install --upgrade setuptools \
 && echo pip istalled setuptools
RUN ldconfig -v

# Build netCDF4-python libraries
RUN source /opt/rh/devtoolset-8/enable \
 && cd ${NETCDF}/BUILD_DIR \
 && tar -xf v1.5.1rel.tar.gz \
 && cd netcdf4-python-1.5.1rel/ \
 && export LD_LIBRARY_PATH=${NETCDF}/lib:${LD_LIBRARY_PATH} \
 && export NETCDF4_DIR=${NETCDF} \
 && export HDF5_DIR=/usr/local \
 && python setup.py build \
 && CPPFLAGS="-I${NETCDF}/include -I/usr/local/include" LDFLAGS="-L${NETCDF}/lib -L/usr/local/lib" python setup.py install \
 && echo dummy printout to keep travis happy ncf4-python install
#&& python setup.py build &> /wrf/libs/build_log_ncf4-python_build

RUN mkdir -p /var/run/sshd \
    && ssh-keygen -A \
    && sed -i 's/#PermitRootLogin yes/PermitRootLogin yes/g' /etc/ssh/sshd_config \
    && sed -i 's/#RSAAuthentication yes/RSAAuthentication yes/g' /etc/ssh/sshd_config \
    && sed -i 's/#PubkeyAuthentication yes/PubkeyAuthentication yes/g' /etc/ssh/sshd_config

RUN mkdir -p  /wrf/WPS_GEOG /wrf/wrfinput /wrf/wrfoutput \
 &&  chown -R wrfuser:wrf /wrf /wrf/WPS_GEOG /wrf/wrfinput /wrf/wrfoutput /usr/local \
 &&  chmod 6755 /wrf /wrf/WPS_GEOG /wrf/wrfinput /wrf/wrfoutput /usr/local

# Download NCL
RUN curl -SL https://ral.ucar.edu/sites/default/files/public/projects/ncar-docker-wrf/nclncarg-6.3.0.linuxcentos7.0x8664nodapgcc482.tar.gz | tar zxC /usr/local
ENV NCARG_ROOT /usr/local

# Set environment for interactive container shells
RUN echo export LDFLAGS="-lm" >> /etc/bashrc \
 && echo export NETCDF=${NETCDF} >> /etc/bashrc \
 && echo export JASPERINC=/usr/include/jasper/ >> /etc/bashrc \
 && echo export JASPERLIB=/usr/lib64/ >> /etc/bashrc \
 && echo export LD_LIBRARY_PATH="/opt/rh/devtoolset-8/root/usr/lib/gcc/x86_64-redhat-linux/8:/usr/lib64/openmpi/lib:${NETCDF}/lib:${LD_LIBRARY_PATH}" >> /etc/bashrc  \
 && echo export PATH=".:/opt/rh/devtoolset-8/root/usr/bin:/usr/lib64/openmpi/bin:${NETCDF}/bin:$PATH" >> /etc/bashrc

RUN echo setenv LDFLAGS "-lm" >> /etc/csh.cshrc \
 && echo setenv NETCDF "${NETCDF}" >> /etc/csh.cshrc \
 && echo setenv JASPERINC "/usr/include/jasper/" >> /etc/csh.cshrc \
 && echo setenv JASPERLIB "/usr/lib64/" >> /etc/csh.cshrc \
 && echo setenv LD_LIBRARY_PATH "/opt/rh/devtoolset-8/root/usr/lib/gcc/x86_64-redhat-linux/8:/usr/lib64/openmpi/lib:${NETCDF}/lib:${LD_LIBRARY_PATH}" >> /etc/csh.cshrc \
 && echo setenv PATH ".:/opt/rh/devtoolset-8/root/usr/bin:/usr/lib64/openmpi/bin:${NETCDF}/bin:$PATH" >> /etc/csh.cshrc

RUN mkdir /wrf/.ssh ; echo "StrictHostKeyChecking no" > /wrf/.ssh/config
COPY default-mca-params.conf /wrf/.openmpi/mca-params.conf
RUN mkdir -p /wrf/.openmpi
RUN chown -R wrfuser:wrf /wrf/

# all root steps completed above, now below as regular userID wrfuser
USER wrfuser
WORKDIR /wrf

# Download data
ARG argname=tutorial
RUN echo DAVE $argname
RUN if [ "$argname" = "tutorial" ] ; then curl -SL http://www2.mmm.ucar.edu/wrf/src/wps_files/geog_low_res_mandatory.tar.gz | tar -xzC /wrf/WPS_GEOG ; fi
RUN if [ "$argname" = "tutorial" ] ; then curl -SL http://www2.mmm.ucar.edu/wrf/TUTORIAL_DATA/colorado_march16.new.tar.gz | tar -xzC /wrf/wrfinput ; fi
RUN if [ "$argname" = "tutorial" ] ; then curl -SL http://www2.mmm.ucar.edu/wrf/src/namelists_v$NML_VERSION.tar.gz  | tar -xzC /wrf/wrfinput ; fi
RUN if [ "$argname" = "tutorial" ] ; then curl -SL http://www2.mmm.ucar.edu/wrf/TUTORIAL_DATA/WRF_NCL_scripts.tar.gz | tar -xzC /wrf ; fi
#RUN if [ "$argname" = "regtest" ]  ; then curl -SL http://www2.mmm.ucar.edu/wrf/dave/DATA/Data_small/data_SMALL.tar.gz | tar -xzC /wrf ; fi
COPY data_smaller.tar /wrf/data_smaller.tar
RUN tar -xf /wrf/data_smaller.tar \
    && rm /wrf/data_smaller.tar
RUN if [ "$argname" = "regtest" ]  ; then curl -SL http://www2.mmm.ucar.edu/wrf/dave/nml.tar.gz | tar -xzC /wrf ; fi
RUN if [ "$argname" = "regtest" ]  ; then curl -SL http://www2.mmm.ucar.edu/wrf/dave/script.tar | tar -xC /wrf ; fi

# Download wps source
RUN if [ "$argname" = "tutorial" ] ; then git clone https://github.com/wrf-model/WPS.git WPS ; fi

ENV JASPERINC /usr/include/jasper
ENV JASPERLIB /usr/lib64
ENV NETCDF_classic 1
ENV LD_LIBRARY_PATH /opt/rh/devtoolset-8/root/usr/lib/gcc/x86_64-redhat-linux/8:/usr/lib64/openmpi/lib:${NETCDF}/lib:${LD_LIBRARY_PATH}
ENV PATH  .:/opt/rh/devtoolset-8/root/usr/bin:/usr/lib64/openmpi/bin:${NETCDF}/bin:$PATH

RUN ssh-keygen -f /wrf/.ssh/id_rsa -t rsa -N '' \
    && chmod 600 /wrf/.ssh/config \
    && chmod 700 /wrf/.ssh \
    && cp /wrf/.ssh/id_rsa.pub /wrf/.ssh/authorized_keys
#
