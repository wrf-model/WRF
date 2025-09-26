OBJS = cio.o plotgrid.o input_module.o misc_definitions_module.o module_debug.o parallel_module.o queue_module.o gridinfo_module.o wrf_debug.o

INCLUDEPATH = -I$(WRF_DIR)/external/ioapi_share -I$(WRF_DIR)/external/io_netcdf -I$(WRF_DIR)/external/io_grib1 -I$(WRF_DIR)/external/io_int

.IGNORE:
.SUFFIXES: .c .F .o

.c.o:
	rm -f $@
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<

#.f90.o:
#	rm -f $@ $*.mod
#	cp $< $*.f
#	$(FC) $(FFLAGS) -c $*.f $(INCLUDEPATH)
#	rm -f $*.f

.F.o:
	rm -f $@ $*.mod
	cpp -P -traditional $(CPPFLAGS) $(INCLUDEPATH) $< > $*.f90
	$(FC) $(FFLAGS) -c $*.f90 $(INCLUDEPATH)
	rm -f $*.f90

all: plotgrid.exe

plotgrid.exe: $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJS) $(WRF_DIR)/frame/pack_utils.o $(WRF_DIR)/frame/module_machine.o $(WRF_DIR)/frame/module_internal_header_util.o -L$(NETCDF)/lib -I$(NETCDF)/include -I$(WRF_DIR)/external/io_grib1 -I$(WRF_DIR)/external/io_int -I$(WRF_DIR)/external/io_netcdf -L$(WRF_DIR)/external/io_netcdf -L$(WRF_DIR)/external/io_grib1 -L$(WRF_DIR)/external/io_int -lwrfio_nf -lwrfio_int -lio_grib1 -lnetcdf 

plotgrid.o: input_module.o plotgrid.F

input_module.o: gridinfo_module.o misc_definitions_module.o module_debug.o parallel_module.o queue_module.o input_module.F

module_debug.o: cio.o parallel_module.o module_debug.F

gridinfo_module.o: misc_definitions_module.o module_debug.o gridinfo_module.F

clean:
	rm -f $(OBJS) *.mod

clobber:
	make clean; rm -f plotgrid.exe
