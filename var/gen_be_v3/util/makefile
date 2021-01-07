#!/bin/sh

FC     = gfortran
FFLAGS = -fconvert=big-endian

all: combine_be_cv5.exe combine_be_cv7.exe combine_be_cv7_ccv2.exe

combine_be_cv5.exe: combine_be_cv5.o
	rm -f $@
	${FC} -o $@ $(FFLAGS) combine_be_cv5.o

combine_be_cv7.exe: combine_be_cv7.o
	rm -f $@
	${FC} -o $@ $(FFLAGS) combine_be_cv7.o

combine_be_cv7_ccv2.exe: combine_be_cv7_ccv2.o
	rm -f $@
	${FC} -o $@ $(FFLAGS) combine_be_cv7_ccv2.o

.SUFFIXES : .f90 .o

.f90.o :
	$(FC) $(FFLAGS) -c $*.f90

clean:
	rm -f *.o *.exe
