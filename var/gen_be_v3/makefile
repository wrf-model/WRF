#!/bin/sh

#serial
FC = gfortran
CPPFLAG = -DUSE_WRFDA

#mpp
#FC = mpif90
#CPPFLAG = -DUSE_WRFDA -DDM_PARALLEL

#openmp (optional, used only for slen_opt=1)
OMP_FLAG = #-fopenmp
OMP_LIB  = #-L/usr/local/lib -lgfortran -lgomp

#for write_ep = .true. application, the code should be compiled with real-4
#for gen_be applications, especially cv_options=5, real-4 works, but real-8 is recommended if memory is not an issue
PROMOTION  = #-fdefault-real-8
FCBASEOPTS = -ffree-form -fconvert=big-endian #-ffree-line-length-none
FCDEBUG    = #-g -O0 -fbacktrace -ggdb -fcheck=bounds,do,mem,pointer -ffpe-trap=invalid,zero,overflow
FFLAGS     = $(PROMOTION) $(FCDEBUG) $(FCBASEOPTS) $(OMP_FLAG)
CPP        = cpp -P -traditional

WRF_SRC_ROOT_DIR = ../..
LIBS = -L$(WRF_SRC_ROOT_DIR)/external/io_netcdf -lwrfio_nf -L$(WRF_SRC_ROOT_DIR)/var/build -lwrfvar -L$(NETCDF)/lib -lnetcdff -lnetcdf
INCS = -I$(WRF_SRC_ROOT_DIR)/var/build

OBJS = gen_be_v3.o

gen_be_v3: ${OBJS}
	${FC} -o gen_be_v3.exe ${FFLAGS} ${OBJS} ${LIBS} $(OMP_LIB)

.SUFFIXES : .F90 .f .o

.F90.f :
	$(RM) $@
	$(CPP) $(CPPFLAG) $*.F90 > $@

.f.o :
	${FC} ${FFLAGS} ${INCS} -c $*.f

clean:
	rm -f *.o *.exe
