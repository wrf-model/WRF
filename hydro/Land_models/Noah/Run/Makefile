.SUFFIXES:
.SUFFIXES: .o .f

include ../user_build_options
include ../../../macros

OBJS = \
	../IO_code/Noah_hrldas_driver.o \
	../IO_code/module_hrldas_netcdf_io.o \
	../Noah/module_sf_noahlsm.o \
	../Noah/module_sf_urban.o \
	../Utility_routines/module_Noahlsm_utility.o \
	../Utility_routines/module_model_constants.o \
	../Utility_routines/module_sfcdif_wrf.o \
	../Utility_routines/module_date_utilities.o \
	../Utility_routines/kwm_string_utilities.o

CMD = Noah_hrldas_beta
all:	$(CMD)

Noah_hrldas_beta: $(OBJS)
	@echo ""
# We have to include the modules built in ../IO_code 
	$(COMPILERF90) -o $(@) $(MODFLAG) -I ../IO_code $(OBJS) $(NETCDFLIB) $(HDF5LIB) -L../../../lib  -lHYDRO $(NETCDFLIB) $(LDFLAGS) $(LIBS)
	@echo ""

# This command cleans up
clean:
	$(RM) *~ $(CMD)

