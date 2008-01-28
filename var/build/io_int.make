# io_int

# pack_utils now from frame, removed from here

IO_INT_OBJS = io_int.o module_internal_header_util.o
IO_INT_M4   = -Uinclude -Uindex -Ulen

libwrfio_int.a : $(OBJS)
	$(RM) libwrfio_int.a
	$(AR) libwrfio_int.a $(IO_INT_OBJS)
	$(RANLIB) libwrfio_int.a

io_int.o : io_int.F90 module_internal_header_util.o
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) io_int.F90 | $(M4) $(IO_INT_M4) - > io_int.f
	$(FC) $(FCFLAGS_NOWARN) -c io_int.f

diffwrf_int.o : diffwrf_int.F
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) diffwrf_int.F > diffwrf_int.f
	$(SFC) -c $(FCFLAGS_SIMPLE)  diffwrf_int.f

diffwrf_int.exe : diffwrf_int.o io_int.o module_internal_header_util.o module_wrf_error.o \
   module_machine.o module_driver_constants.o pack_utils.o $(NETCDF_LIBS)
	$(SFC) $(LDFLAGS) -o diffwrf_int.exe io_int.o diffwrf_int.o module_internal_header_util.o \
           module_wrf_error.o module_machine.o module_driver_constants.o pack_utils.o $(NETCDF_LIB) $(LOCAL_LIB)
	(cd ../main; $(LN) ../build/diffwrf_int.exe .)

