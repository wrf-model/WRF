# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	module_HYDRO_utils.o \
	module_noah_chan_param_init_rt.o \
	module_GW_baseflow.o \
	module_HYDRO_io.o \
 	module_RT.o Noah_distr_routing.o \
	module_channel_routing.o \
	rtFunction.o module_lsm_forcing.o

all:	$(OBJS)

#module_RT.o: module_RT.F
#	@echo ""
#	$(CPP) $(CPPFLAGS) $(*).F > $(*).f
#	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG)  $(*).f
#	$(RMD) $(*).f
#	@echo ""
#	cp *.mod ../mod

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../lib/libHYDRO.a $(@)
	cp *.mod ../mod

#
# Dependencies:
#
module_GW_baseflow.o: ../Data_Rec/module_GW_baseflow_data.o 

module_HYDRO_io.o:  module_HYDRO_utils.o ../Data_Rec/module_namelist.o ../Data_Rec/module_RT_data.o 

module_HYDRO_utils.o: ../Data_Rec/module_namelist.o ../Data_Rec/module_RT_data.o

module_lsm_forcing.o: module_HYDRO_io.o 

module_RT.o: ../Data_Rec/module_namelist.o ../Data_Rec/module_RT_data.o ../Data_Rec/module_GW_baseflow_data.o \
	module_GW_baseflow.o module_HYDRO_utils.o module_HYDRO_io.o\
	module_noah_chan_param_init_rt.o ../Data_Rec/module_GW_baseflow_data.o 

rtFunction.o: ../Data_Rec/module_RT_data.o ../Data_Rec/module_GW_baseflow_data.o ../Data_Rec/module_namelist.o module_channel_routing.o

clean:
	rm -f *.o *.mod *.stb *~ *.f
