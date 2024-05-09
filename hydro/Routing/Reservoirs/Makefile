
include ../../macros

MODFLAG := $(MODFLAG) -I ../../mod

%.o : %.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod test

all: mod

../../MPP/mpp_land.mod:
		make -C ../../MPP

../../utils/module_hydro_stop.mod:
		make -C ../../utils

mod: ../../MPP/module_mpp_land.mod ../../utils/module_hydro_stop.mod
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_utilities.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_read_timeslice_data.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_read_rfc_time_series_data.F90

	ar -r ../../lib/libHYDRO.a module_reservoir_utilities.o
	ar -r ../../lib/libHYDRO.a module_reservoir.o
	ar -r ../../lib/libHYDRO.a module_reservoir_read_timeslice_data.o
	ar -r ../../lib/libHYDRO.a module_reservoir_read_rfc_time_series_data.o

	cp *.mod ../../mod

	#Build the modules
	make -C Level_Pool
	make -C Persistence_Level_Pool_Hybrid
	make -C RFC_Forecasts


test: ../../MPP/module_mpp_land.mod ../../utils/module_hydro_stop.mod
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_utilities.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_read_timeslice_data.F90
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_read_rfc_time_series_data.F90


	make -C Level_Pool
	make -C Persistence_Level_Pool_Hybrid
	make -C RFC_Forecasts

	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) reservoir_tests.F90


	$(COMPILER90) $(NETCDFLIB) -o reservoir_tests \
		../../MPP/mpp_land.o \
		../../MPP/CPL_WRF.o \
		../../utils/module_hydro_stop.o \
		module_reservoir_utilities.o \
		module_reservoir.o \
		module_reservoir_read_timeslice_data.o \
		module_reservoir_read_rfc_time_series_data.o \
		Level_Pool/module_levelpool_properties.o \
		Level_Pool/module_levelpool_state.o \
		Level_Pool/module_levelpool_tests.o \
		Level_Pool/module_levelpool.o \
		Persistence_Level_Pool_Hybrid/module_persistence_levelpool_hybrid_properties.o \
		Persistence_Level_Pool_Hybrid/module_persistence_levelpool_hybrid_state.o \
		Persistence_Level_Pool_Hybrid/module_persistence_levelpool_hybrid_tests.o \
		Persistence_Level_Pool_Hybrid/module_persistence_levelpool_hybrid.o \
		RFC_Forecasts/module_rfc_forecasts_properties.o \
		RFC_Forecasts/module_rfc_forecasts_state.o \
		RFC_Forecasts/module_rfc_forecasts_tests.o \
		RFC_Forecasts/module_rfc_forecasts.o \
		reservoir_tests.o


clean:
	rm -f *.o
	rm -f *.mod
	rm -f reservoir_tests

	make -C Level_Pool clean
	make -C Persistence_Level_Pool_Hybrid clean
	make -C RFC_Forecasts clean
