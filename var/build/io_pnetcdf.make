# io_pnetcdf

IO_PNETCDF_OBJS    = wrf_io_pnc.o field_routines_pnc.o
IO_PNETCDF_CODE    = ext_pnc_get_dom_ti.code ext_pnc_get_var_td.code \
                    ext_pnc_get_var_ti.code ext_pnc_put_dom_ti.code \
                    ext_pnc_put_var_td.code ext_pnc_put_var_ti.code transpose_pnc.code
IO_PNETCDF_CPPFLAGS = -I$(PNETCDF_INC)
IO_PNETCDF_FCFLAGS = -I$(PNETCDF_INC)
IO_PNETCDF_M4FLAGS = -Uinclude -Uindex -Ulen

libwrfio_pnf.a:		$(IO_PNETCDF_OBJS) $(IO_PNETCDF_CODE)
			$(RM) libwrfio_pnf.a
			$(AR) libwrfio_pnf.a $(IO_PNETCDF_OBJS)
			$(RANLIB) libwrfio_pnf.a

wrf_io_pnc.o : wrf_io_pnc.F90 $(IO_PNETCDF_CODE)
			$(CPP) $(CPPFLAGS) $(FPPFLAGS) wrf_io_pnc.F90 | $(M4) $(IO_PNETCDF_M4FLAGS) - > wrf_io_pnc.f
			$(FC) -c $(FCFLAGS_NOWARN) $(IO_PNETCDF_FCFLAGS) wrf_io_pnc.f

field_routines_pnc.o: field_routines_pnc.F90 wrf_io_pnc.o
			$(CPP) $(CPPFLAGS) $(FPPFLAGS) field_routines_pnc.F90 > field_routines_pnc.f
			$(FC) -c $(FCFLAGS_NOWARN) $(IO_PNETCDF_FCFLAGS) field_routines_pnc.f
