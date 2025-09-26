.SUFFIXES: .F .o

FC = xlf90
CPP = /usr/local/bin/cpp -P -traditional
FCFLAGS = -qfree=f90 -qrealsize=8 -qcheck -g
LDFLAGS = -g -qcheck

OBJS = average_be.o              \
       rd_be_cv_5.o              \
       wr_be_cv_5.o              \
       module_readwrf.o          \
       da_interpolate_regcoeff.o \
       da_interpolate_stats.o    \
       da_1d_eigendecomposition.o \
       module_be_type.o

all: average_be.exe

clean:
	rm -f *.o *.mod *.exe

average_be.exe: $(OBJS)
	$(FC) $(LDFLAGS) -o average_be.exe $(OBJS) -L$(NETCDF)/lib -lnetcdf -L$(LAPACK) -llapack -L$(BLAS) -lblas

average_be.o: rd_be_cv_5.o wr_be_cv_5.o module_be_type.o module_readwrf.o

da_interpolate_regcoeff.o:

da_interpolate_stats.o: da_1d_eigendecomposition.o

rd_be_cv_5.o: module_be_type.o

wr_be_cv_5.o: module_be_type.o

module_be_type.o:

.F.o:
	$(CPP) -I$(NETCDF)/include $*.F > $*.f90
	$(FC) $(FCFLAGS) -c $*.f90
	rm -f $*.f90
