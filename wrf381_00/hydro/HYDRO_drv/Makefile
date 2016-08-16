# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	module_HYDRO_drv.o
all:	$(OBJS) 

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
#	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I../mod $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) -I../mod $(*).f
#	$(RMD) $(*).f
	@echo ""
	ar -r ../lib/libHYDRO.a $(@)
	cp *.mod ../mod

#
# Dependencies:
#
module_HYDRO_drv.o: ../Data_Rec/module_namelist.o ../Data_Rec/module_RT_data.o ../Data_Rec/module_gw_gw2d_data.o \
        ../Routing/module_GW_baseflow.o ../Routing/module_HYDRO_utils.o ../Routing/module_HYDRO_io.o ../Routing/module_RT.o

clean:
	rm -f *.o *.mod *.stb *~ *.f
