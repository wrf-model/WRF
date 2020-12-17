# convertor

CONVERTOR_OBJS = \
	module_kma2netcdf_interface.o \
	module_netcdf2kma_interface.o \
	module_kma_wave2grid.o \
	module_wave2grid_kma.o

convertor : kma2netcdf netcdf2kma

k2n : kma2netcdf

n2k : netcdf2kma

kma2netcdf :  $(WRFVAR_LIBS) $(CONVERTOR_OBJS) kma2netcdf.o
	$(FC) -o kma2netcdf.exe $(LDFLAGS) kma2netcdf.o \
         $(CONVERTOR_OBJS) $(WRFVAR_LIB) $(LIB_BUNDLED) $(LIB_EXTERNAL)

netcdf2kma : $(WRFVAR_LIBS) $(CONVERTOR_OBJS) netcdf2kma.o
	$(FC) -o netcdf2kma.exe $(LDFLAGS) netcdf2kma.o \
         $(CONVERTOR_OBJS) $(WRFVAR_LIB) $(LIB_BUNDLED) $(LIB_EXTERNAL)

PREGSM : PREGSM.o  $(CONVERTOR_OBJS)
	$(SFC) -L. $(LDFLAGS) -o PREGSM.exe PREGSM.o $(CONVERTOR_OBJS)

PREGSM.o : PREGSM.F
	$(RM) $@
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) PREGSM.F > PREGSM.f
	$(SFC) -c $(FORMAT_FIXED) $(BYTESWAPIO) $(FCOPTIM) $(PROMOTION) PREGSM.f

module_kma_wave2grid.o : module_kma_wave2grid.f90
	$(RM) $@
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) module_kma_wave2grid.f90 > module_kma_wave2grid.f
	$(SFC) -c $(FORMAT_FIXED) $(BYTESWAPIO) $(FCOPTIM) $(PROMOTION)  module_kma_wave2grid.f

module_wave2grid_kma.o : module_wave2grid_kma.f90
	$(RM) $@
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) module_wave2grid_kma.f90 > module_wave2grid_kma.f
	$(FC) -c $(FORMAT_FIXED) $(BYTESWAPIO) $(FCOPTIM) $(PROMOTION) module_wave2grid_kma.f
