# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	module_date_utilities_rt.o \
	module_UDMAP.o \
	module_HYDRO_utils.o \
	module_noah_chan_param_init_rt.o \
	module_GW_baseflow.o \
	module_gw_gw2d.o \
	module_HYDRO_io.o \
 	module_RT.o \
	Noah_distr_routing.o \
	module_channel_routing.o \
	module_lsm_forcing.o \
	module_date_utilities_rt.o \
	module_NWM_io_dict.o \
	module_NWM_io.o \
        module_reservoir_routing.o

all:	$(OBJS)

#module_RT.o: module_RT.F
#	@echo ""
#	$(CPP) $(CPPFLAGS) $(*).F > $(*).f
#	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG)  $(*).f
#	$(RMD) $(*).f
#	@echo ""
#	cp *.mod ../mod

.F.o:
	@echo "Routing Makefile:"
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -o $(@) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $(*).F
	@echo ""
	ar -r ../lib/libHYDRO.a $(@)
	cp *.mod ../mod

#
# Dependencies:
#
module_gw_gw2d.o: ../Data_Rec/module_gw_gw2d_data.o module_HYDRO_io.o

ifneq ($(WRF_HYDRO_NUDGING),-DWRF_HYDRO_NUDGING)
module_HYDRO_io.o:  module_HYDRO_utils.o \
	            module_date_utilities_rt.o \
                    ../Data_Rec/module_namelist.o \
	 	    ../Data_Rec/module_RT_data.o 
else 
module_HYDRO_io.o:  module_HYDRO_utils.o \
	            module_date_utilities_rt.o \
		    ../nudging/module_date_utils_nudging.o \
	            ../nudging/module_nudging_io.o \
                    ../Data_Rec/module_namelist.o \
	 	    ../Data_Rec/module_RT_data.o 
endif

module_NWM_io_dict: ../Data_Rec/module_namelist.o ../utils/module_version.o

module_NWM_io: module_HYDRO_utils.o \
               module_NWM_io_dict.o \
               module_HYDRO_io.o \
               module_date_utilities_rt.o \
               ../Data_Rec/module_namelist.o \
               ../Data_Rec/module_RT_data.o \
	       ../utils/module_version.o

module_reservoir_routing: ../Data_Rec/module_namelist.o

module_HYDRO_utils.o: ../Data_Rec/module_namelist.o ../Data_Rec/module_RT_data.o

module_lsm_forcing.o: module_HYDRO_io.o 

ifneq ($(WRF_HYDRO_NUDGING),-DWRF_HYDRO_NUDGING)
module_RT.o: module_GW_baseflow.o \
	     module_HYDRO_utils.o \
             module_HYDRO_io.o \
             module_noah_chan_param_init_rt.o \
	     module_UDMAP.o \
	     ../Data_Rec/module_namelist.o \
	     ../Data_Rec/module_RT_data.o \
	     ../Data_Rec/module_gw_gw2d_data.o
else
module_RT.o: module_GW_baseflow.o \
	     module_HYDRO_utils.o \
             module_HYDRO_io.o \
             module_noah_chan_param_init_rt.o \
	     module_UDMAP.o \
	     ../Data_Rec/module_namelist.o \
	     ../Data_Rec/module_RT_data.o \
	     ../Data_Rec/module_gw_gw2d_data.o \
             ../nudging/module_date_utils_nudging.o \
             ../nudging/module_nudging_io.o
endif

module_UDMAP.o: ../Data_Rec/module_namelist.o ../Data_Rec/module_RT_data.o

ifneq ($(WRF_HYDRO_NUDGING),-DWRF_HYDRO_NUDGING)
module_channel_routing.o: module_UDMAP.o
else
module_channel_routing.o: module_UDMAP.o\
			  ../nudging/module_date_utils_nudging.o \
		          ../nudging/module_nudging_utils.o \
          		  ../nudging/module_stream_nudging.o
endif

clean:	
	rm -f *.o *.mod *.stb *~
