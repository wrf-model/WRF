# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../macros

OBJS = \
	module_date_utils_nudging.o \
	module_stream_nudging.o \
	module_nudging_io.o \
	module_nudging_utils.o


all:	$(OBJS)

#module_RT.o: module_RT.F90
#	@echo ""
#	$(CPP) $(CPPFLAGS) $(*).F90 > $(*).f90
#	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG)  $(*).f90
#	$(RMD) $(*).f90
#	@echo ""
#	cp *.mod ../mod

.F90.o:
	@echo ""
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -o $(@) $(F90FLAGS) $(MODFLAG) -I$(NETCDF_INC) $(*).F90
	@echo ""
	ar -r ../lib/libHYDRO.a $(@)
	cp *.mod ../mod

#
# Dependencies:
#
module_nudging_io.o: ../Data_Rec/module_namelist.o \
	             module_date_utils_nudging.o

module_stream_nudging.o: module_nudging_utils.o \
	      		 module_nudging_io.o


clean:
	rm -f *.o *.mod *.stb *~
