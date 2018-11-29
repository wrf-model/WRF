SHELL=/bin/sh
.SUFFIXES:	
.SUFFIXES:	.F .c .o

include ../../user_build_options

OBJS=	module_grib2.o \
	module_grib2_tables.o \
	trig_degrees.o \
	module_input_data_structure.o \
	arguments_module.o \
	gbytesys.o \
	swap4f.o \
	module_llxy.o \
	cio.o \
	kwm_date_utilities.o \
	kwm_grid_utilities.o \
	kwm_timing_utilities.o \
	kwm_string_utilities.o \
	swap4c.o \
	get_unused_unit.o \
	decode_jpeg2000.o  \
	io_f.o \
	module_mapinfo.o \
	module_grib1.o \
	module_grib.o \
	module_grib_common.o \
	module_wrfinputfile.o \
	module_geo_em.o 

CMD=	libsmda.a

GRIBCODE_OPT = -DBIT32
BZIP_CPP = -D_BZIP_$(BZIP2)

all:	$(CMD)

libsmda.a:	$(OBJS)
	$(RM) libsmda.a
	ar q libsmda.a $(OBJS)

swap4c.o:	swap4c.c
	$(CC) -c swap4c.c

io_f.o:	io_f.c
	$(CC) -c $(CCFLAGS) $(BZIP_CPP) $(BZIP2_INCLUDE) io_f.c

decode_jpeg2000.o:	decode_jpeg2000.c
	$(CC) -c $(INCJASPER) $(<)

.F.o:
	@echo ""
	$(RM) $(*).f
	$(CPP) $(CPPFLAGS) $(GRIBCODE_OPT) $(BZIP_CPP) $(*).F > $(*).f
	$(COMPILERF90) -c $(FREESOURCE) $(F90FLAGS) $(NETCDFMOD) $(*).f
	$(RM) $(*).f

.c.o:
	$(CC) -c $(BZIP_CPP) $(<)

clean:
	$(RM) $(OBJS) $(CMD) *.mod *~
#
module_grib2.o:	module_grib2.F module_grib2_tables.o module_grib1.o module_mapinfo.o kwm_date_utilities.o module_grib_common.o
module_grib1.o: module_grib1.F module_mapinfo.o kwm_date_utilities.o module_grib_common.o
module_grib.o:	module_grib.F module_grib1.o module_grib2.o
module_input_data_structure.o:	module_llxy.o
