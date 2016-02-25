# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F



include ../../macros

MODFLAG =       -I./ -I ../../MPP -I ../../mod 

WRF_ROOT = ../../..
OBJS = \
	module_wrf_HYDRO.o \
	wrf_drv_HYDRO.o    
all:	$(OBJS) 

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I$(WRF_ROOT)/frame -I$(WRF_ROOT)/main -I$(WRF_ROOT)/external/esmf_time_f90 $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../../lib/libHYDRO.a $(@)

#
# Dependencies:
#
module_wrf_HYDRO.o: ../../Data_Rec/module_RT_data.o ../../Data_Rec/module_namelist.o ../../HYDRO_drv/module_HYDRO_drv.o

wrf_drv_HYDRO.o: module_wrf_HYDRO.o

clean:
	rm -f *.o *.mod *.stb *~ *.f
