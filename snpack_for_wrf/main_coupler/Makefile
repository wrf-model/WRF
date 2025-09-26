FC = gfortran # gfortran
CXX = g++

COMPNAME := $(shell hostname -s | sed 's/[0-9]//g' )
FCFLAGS = -O3 -g -fbacktrace -ffree-line-length-512
CCFLAGS = -O3 -g 
ifeq ($(COMPNAME),cryospc)
   $(info UNAME is $(COMPNAME))
   FCFLAGS = -O3 -g -fbacktrace -ffree-line-length-512
   CCFLAGS = -O3 -g 
endif

ifeq ($(COMPNAME),daint)
   FC = ftn
   CXX = CC
   FCFLAGS = -O3 -g -traceback
   CCFLAGS = -O3 -g -traceback
endif

all: libcoupler.a

libcoupler.so : coupler_mod.o funcs.o coupler_capi.o Coupler.o  
	gcc -shared $^ -o $@

libcoupler.a : coupler_mod.o funcs.o coupler_capi.o Coupler.o  
	ar rcs $@ $^

%.o : %.f90
	${FC} ${FCFLAGS} -c $^ -o $@ -I../snow_libs/include

%.o : %.cpp
	${CXX} ${CCFLAGS} -c $< -o $@ -I../snow_libs/include

.PHONY : clean

clean :
	${RM} -rf *.o *.mod test.x *.a *.so
