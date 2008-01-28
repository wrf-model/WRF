# io_netcdf

IO_NETCDF_OBJS    = wrf_io.o field_routines.o
IO_NETCDF_CODE    = ext_ncd_get_dom_ti.code ext_ncd_get_var_td.code \
                    ext_ncd_get_var_ti.code ext_ncd_put_dom_ti.code \
                    ext_ncd_put_var_td.code ext_ncd_put_var_ti.code transpose.code 
IO_NETCDF_CPPFLAGS = -I$(NETCDF_INC)
IO_NETCDF_FCFLAGS = -I$(NETCDF_INC)
IO_NETCDF_M4FLAGS = -Uinclude -Uindex -Ulen

libwrfio_nf.a : $(IO_NETCDF_OBJS) $(IO_NETCDF_CODE)
	$(RM) libwrfio_nf.a
	$(AR) libwrfio_nf.a $(IO_NETCDF_OBJS)
	$(RANLIB) libwrfio_nf.a

wrf_io.o : wrf_io.F90 $(IO_NETCDF_CODE)
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) wrf_io.F90 | $(M4) $(IO_NETCDF_M4FLAGS) - > wrf_io.f
	$(FC) -c $(FCFLAGS_NOWARN) $(IO_NETCDF_FCFLAGS) wrf_io.f

vort.o : vort.F90 $(IO_NETCDF_CODE)
	$(CPP) $(CPPFLAGS) $(FPPFLAGS)  $(IO_NETCDF_CPPFLAGS) vort.F90 > vort.f
	$(FC) -c $(FCFLAGS_NOWARN) vort.f

diffwrf_netcdf.o : diffwrf_netcdf.F
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) $(IO_NETCDF_CPPFLAGS) diffwrf_netcdf.F > diffwrf_netcdf.f
	$(SFC) -c $(FCFLAGS_SIMPLE) diffwrf_netcdf.f

field_routines.o: field_routines.F90 wrf_io.o
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) field_routines.F90 > field_routines.f
	$(FC) -c $(FCFLAGS_NOWARN) $(IO_NETCDF_FCFLAGS) field_routines.f

diffwrf_netcdf.exe : diffwrf_netcdf.o wrf_debug.o module_wrf_error.o $(NETCDF_LIBS)
	$(SFC) $(LDFLAGS) -o diffwrf_netcdf.exe diffwrf_netcdf.o wrf_debug.o module_wrf_error.o \
           $(NETCDF_LIB) $(LOCAL_LIB)
	(cd ../main; $(LN) ../build/diffwrf_netcdf.exe .)
