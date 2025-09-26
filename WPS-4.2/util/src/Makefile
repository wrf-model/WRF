include ../../configure.wps

OBJS	=	plotgrids.o avg_tsfc.o calc_ecmwf_p.o elev_angle.o plotfmt.o rd_intermediate.o \
		int2nc.o mod_levs.o height_ukmo.o \
		cio.o gridinfo_module.o misc_definitions_module.o module_debug.o module_stringutil.o \
		read_met_module.o write_met_module.o module_date_pack.o met_data_module.o constants_module.o \
		module_map_utils.o

all:
		clear ;
		@echo " "
		@echo "go up two directories and type './compile util' to build WPS utilities"
		@echo " "
		@echo " "

rd_intermediate.exe: rd_intermediate.o read_met_module.o module_debug.o misc_definitions_module.o met_data_module.o
	$(SFC) -o $(@) $(LDFLAGS) rd_intermediate.o read_met_module.o \
		module_debug.o misc_definitions_module.o cio.o met_data_module.o constants_module.o

rd_intermediate.o: module_debug.o read_met_module.o rd_intermediate.F

plotgrids.exe: plotgrids.o
	$(SFC) -o $(@) $(LDFLAGS) module_map_utils.o module_debug.o cio.o constants_module.o misc_definitions_module.o \
		plotgrids.o $(NCARG_LIBS) $(NCARG_LIBS2)

plotgrids.o: plotgrids.F module_map_utils.o
	$(CP) $< $*.f90
	$(SFC) $(FNGFLAGS) -c $*.f90
#	$(RM) $*.f90

avg_tsfc.exe: avg_tsfc.o cio.o module_debug.o gridinfo_module.o read_met_module.o \
		write_met_module.o module_date_pack.o misc_definitions_module.o met_data_module.o
	$(SFC) $(LDFLAGS) -o $@ avg_tsfc.o cio.o module_debug.o gridinfo_module.o \
		read_met_module.o write_met_module.o module_date_pack.o \
		misc_definitions_module.o met_data_module.o constants_module.o

avg_tsfc.o: avg_tsfc.F cio.o module_debug.o gridinfo_module.o read_met_module.o \
		write_met_module.o module_date_pack.o misc_definitions_module.o

elev_angle.exe: elev_angle.o cio.o module_debug.o gridinfo_module.o \
		write_met_module.o misc_definitions_module.o met_data_module.o
	$(SFC) $(LDFLAGS) -o $@ elev_angle.o cio.o module_debug.o gridinfo_module.o \
		write_met_module.o misc_definitions_module.o met_data_module.o \
		-L$(NETCDF)/lib -I$(NETCDF)/include -lnetcdf

elev_angle.o: elev_angle.F cio.o module_debug.o gridinfo_module.o \
		write_met_module.o misc_definitions_module.o
	$(SFC) -c elev_angle.F -I$(NETCDF)/include

calc_ecmwf_p.exe: calc_ecmwf_p.o cio.o module_debug.o module_stringutil.o gridinfo_module.o read_met_module.o \
		write_met_module.o module_date_pack.o misc_definitions_module.o met_data_module.o constants_module.o
	$(SFC) $(LDFLAGS) -o $@ calc_ecmwf_p.o cio.o module_debug.o module_stringutil.o gridinfo_module.o \
		read_met_module.o write_met_module.o module_date_pack.o \
		misc_definitions_module.o met_data_module.o constants_module.o

calc_ecmwf_p.o: calc_ecmwf_p.F cio.o module_debug.o module_stringutil.o gridinfo_module.o read_met_module.o \
		write_met_module.o module_date_pack.o misc_definitions_module.o

plotfmt.exe: plotfmt.o read_met_module.o met_data_module.o
	$(SFC) -o $(@) $(LDFLAGS) plotfmt.o read_met_module.o module_debug.o \
		misc_definitions_module.o cio.o met_data_module.o \
		$(NCARG_LIBS) $(NCARG_LIBS2)

plotfmt.o: plotfmt.F misc_definitions_module.o read_met_module.o constants_module.o
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(SFC) $(FNGFLAGS) -c $*.f90
#	$(RM) $*.f90

mod_levs.exe: mod_levs.o module_debug.o read_met_module.o write_met_module.o misc_definitions_module.o met_data_module.o
	$(SFC) -o $(@) $(LDFLAGS) mod_levs.o module_debug.o read_met_module.o \
		write_met_module.o misc_definitions_module.o cio.o met_data_module.o constants_module.o

mod_levs.o: module_debug.o read_met_module.o write_met_module.o mod_levs.F

int2nc.exe: int2nc.o module_debug.o misc_definitions_module.o read_met_module.o cio.o
	$(SFC) -o $(@) $(LDFLAGS) $(WRF_INCLUDE) int2nc.o module_debug.o misc_definitions_module.o read_met_module.o cio.o $(WRF_LIB) 

int2nc.o: int2nc.F module_debug.o misc_definitions_module.o read_met_module.o
	$(RM) $*.f90
	$(CPP) $(CPPFLAGS) $< > $*.f90
	$(SFC) $(FFLAGS) $(WRF_INCLUDE) -c $*.f90

height_ukmo.exe: height_ukmo.o cio.o module_debug.o gridinfo_module.o read_met_module.o \
		write_met_module.o module_date_pack.o misc_definitions_module.o met_data_module.o
	$(SFC) $(LDFLAGS) -o $@ height_ukmo.o cio.o module_debug.o gridinfo_module.o \
		read_met_module.o write_met_module.o module_date_pack.o \
		misc_definitions_module.o met_data_module.o constants_module.o

height_ukmo.o: height_ukmo.F cio.o module_debug.o gridinfo_module.o read_met_module.o \
		write_met_module.o module_date_pack.o misc_definitions_module.o

module_map_utils.o: module_map_utils.F misc_definitions_module.o constants_module.o module_debug.o

cio.o: cio.c

met_data_module.o: met_data_module.F

module_stringutil.o: module_stringutil.F

gridinfo_module.o: gridinfo_module.F misc_definitions_module.o module_debug.o

misc_definitions_module.o: misc_definitions_module.F

module_debug.o: module_debug.F cio.o

read_met_module.o: read_met_module.F met_data_module.o constants_module.o module_debug.o misc_definitions_module.o

write_met_module.o: write_met_module.F met_data_module.o module_debug.o misc_definitions_module.o

module_date_pack.o: module_date_pack.F

clean:
	$(RM) $(OBJS) *.f90 *.mod
	$(RM) plotgrids.exe plotfmt.exe avg_tsfc.exe elev_angle.exe calc_ecmwf_p.exe rd_intermediate.exe \
	mod_levs.exe height_ukmo.exe int2nc.exe

superclean:	clean
