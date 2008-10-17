include ../../configure.wrf

all_wrfvar: serial parallel
# Do not add convertor to serial target, as it does
# not compile with pgi and g95 compilers
serial: wrfvar be da_utils 
parallel : wrfvar

setup : depend

# preserve this makefile from overwriting
links : 
	@ cp makefile makefile.old
	@ $(LN) ../../dyn_em/* .
	@ $(LN) ../gen_be/* .
	@ $(LN) ../../inc/* .
	@ $(LN) ../../share/* .
	@ $(LN) ../../frame/* .
	@ if test -e module_dm_stubs.F; then rm -rf module_dm_stubs.F; fi
	@ $(LN) ../../share/* .
	@ $(LN) da*/* .
	@ $(LN) ../../arch/*.sed .
	@ $(LN) ../../external/esmf_time_f90/* .
	@ $(LN) ../convertor/da_module_convert_tool/* .
	@ $(LN) ../convertor/kma_wave2grid/* .
	@ $(LN) ../convertor/wave2grid_kma/* .
	@ $(LN) ../convertor/kma_netcdf/* .
	@ $(LN) ../convertor/decode_l2_airs/* .
	@ mv makefile.old makefile
	@ touch links

WRFVAR_LIBS = libwrfvar.a $(ESMF_TIME_LIBS) $(NETCDF_LIBS) $(PNETCDF_LIBS) \
        $(GRIB1_LIBS) $(GRIB2_LIBS) $(GRIB_SHARE_LIBS) $(MPI_LIBS) $(FFTPACK_LIB)

WRFVAR_LIB = -L. -lwrfvar $(LAPACK_LIB) $(NETCDF_LIB) $(PNETCDF_LIB) \
        $(GRIB1_LIB) $(GRIB2_LIB) $(GRIB_SHARE_LIB) \
        $(ESMF_TIME_LIB) \
        $(LAPACK_LIB) $(BLAS_LIB) $(BUFR_LIB) $(FFTPACK_LIB) \
        $(RTTOV_LIB) $(CRTM_LIB) $(MPI_LIB) $(LOCAL_LIB)

FPPFLAGS   = -I$(WRF_SRC_ROOT_DIR)/inc

depend : links module_state_description.F md_calls.inc \
                      inc/da_generic_boilerplate.inc
	$(LN) $(WRF_SRC_ROOT_DIR)/var/da/inc/* $(WRF_SRC_ROOT_DIR)/inc/.
	(cd $(WRF_SRC_ROOT_DIR)/var/da/makedepf90-2.8.8; ./setup_makedepf90)
	$(WRF_SRC_ROOT_DIR)/var/da/makedepf90-2.8.8/makedepf90 -free $(ARCHFLAGS) -DNEW_BDYS *.F *.f90 *.F90 2>/dev/null > depend.txt
	touch depend
	
include depend.txt

# Manual rules specified first take precedence

include da.make
include gen_be.make
include convertor.make
include decode_l2_airs.make

libwrfvar.a : $(WRFVAR_OBJS)
	rm -f libwrfvar.a
	$(AR) $(ARFLAGS) libwrfvar.a $(WRFVAR_OBJS)
	$(RANLIB) libwrfvar.a

# Must not call target clean, as there is a script of that name here
clean1 : 
	@ rm -rf *.o *.mod *.b *.a *.f frame/*.F ../../frame/*.f \
          Registry_tmp* module_state_description.f90 bufrlib.prm \
          ../../external/*.o ../../external/*.a ../../external/*/*.o ../../external/*/*.a \
          inc/dm_comm_cpp_flags testall.csh

superclean : clean1
	@ ( cd ../../external; make superclean )
	@ rm -rf *.exe *.f90 *.F90 *.F *.inc inc/*.inc *.int *.c *.h *.m4 *.code \
          bufrlib.param *.sed registry \
          convert_tool kma_wave2grid wave2grid_kma version_decl \
          Utl* macros_utl module_dm_warning \
          scl_plotting_routines \
          decode_l2_airs.README time_window.nl WRFIO.doc Test1.out.correct \
          da_macros_utl links local README* TEST_RESULTS g2lib grib1_util \
          landread.c.dist WGRIB test.grb build MEL_grib1 bacio-1.3 \
          depend depend.txt ./da/*exe ./inc/namelist_script.inc \
          wrf-phdf5attr.F90btg KPP Makefile INSTALL.htm
    
.f90.o:
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) -I$(WRF_SRC_ROOT_DIR)/inc $(CPPFLAGS) $(FPPFLAGS) $*.b  > $*.f
	$(RM) $*.b
	$(FC) -o $@ -c $(FCFLAGS) $(PROMOTION) $*.f

