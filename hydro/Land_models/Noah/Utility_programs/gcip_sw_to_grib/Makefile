.SUFFIXES:
.SUFFIXES: .F .c .o

include ../../user_build_options

OBJS=	cio.o \
	gbytesys.o \
	module_date_utilities.o \
	swap4c.o \
	swap4f.o

CMD=srb_daily_to_grib.exe srb_monthly_to_grib.exe

all: $(CMD)

$(OBJS):$(SRCS)

srb_daily_to_grib.exe:	$(OBJS) srb_daily_to_grib.o
	$(COMPILERF90) -o srb_daily_to_grib.exe $(F90FLAGS) $(^)

srb_monthly_to_grib.exe:	$(OBJS) srb_monthly_to_grib.o
	$(COMPILERF90) -o srb_monthly_to_grib.exe $(F90FLAGS) $(^)

cio.o:	../../HRLDAS_COLLECT_DATA/lib/cio.c
	$(CC) -c $(<)

swap4c.o:	../../HRLDAS_COLLECT_DATA/lib/swap4c.c
	$(CC) -c $(<)

gbytesys.o:	../../HRLDAS_COLLECT_DATA/lib/gbytesys.F
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(<)

swap4f.o:	../../HRLDAS_COLLECT_DATA/lib/swap4f.F
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) -DBIT32 $(<)

module_date_utilities.o:	../../Utility_routines/module_date_utilities.F
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(<)

.F.o:
	$(RM) $(*).f
	$(CPP) $(CPPFLAGS) $(*).F > $(*).f
	$(COMPILERF90) $(FREESOURCE) $(F90FLAGS) -c $(*).f 
	$(RM) $(*).f


neat:
	$(RM) $(OBJS) *~ *.mod srb_daily_to_grib.o srb_monthly_to_grib.o

clean:
	$(RM) $(OBJS) $(CMD) *~ *.mod srb_daily_to_grib.o srb_monthly_to_grib.o
