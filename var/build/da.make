# da

WRFVAR_OBJS = \
   copyfile.o \
   amsr2time_.o \
   da_blas.o \
   da_lapack.o \
   da_par_util.o \
   da_par_util1.o \
   da_setup_structures.o \
   da_transfer_model.o \
   da_minimisation.o \
   da_vtox_transforms.o \
   da_obs.o \
   da_obs_io.o \
   da_join_iv_for_multi_inc.o \
   da_metar.o \
   da_geoamv.o \
   da_polaramv.o \
   da_ships.o \
   da_synop.o \
   da_sound.o \
   da_mtgirs.o \
   da_tamdar.o \
   da_bogus.o \
   da_airep.o \
   da_pilot.o \
   da_radar.o \
   da_rain.o \
   da_gpspw.o \
   da_gpsref.o \
   da_gpseph.o \
   da_ssmi.o \
   module_ssmi.o \
   da_satem.o \
   da_qscat.o \
   da_pseudo.o \
   da_profiler.o \
   da_buoy.o \
   da_dynamics.o \
   da_physics.o \
   f_qv_from_rh.o \
   da_ffts.o \
   module_ffts.o \
   da_test.o \
   da_tools.o \
   da_tools_serial.o \
   da_wrf_interfaces.o \
   da_rsl_interfaces.o \
   da_mat_cv3.o \
   da_rf_cv3.o \
   da_rfz_cv3.o \
   da_recursive_filter.o \
   da_wavelet.o \
   da_interpolation.o \
   da_grid_definitions.o \
   da_statistics.o \
   da_define_structures.o \
   gamma1.o \
   da_spectral.o \
   da_radiance.o \
   da_radiance1.o \
   da_rttov.o \
   da_crtm.o \
   da_varbc.o \
   module_radiance.o \
   da_tracing.o \
   gsi_kinds.o \
   gsi_constants.o \
   gsi_thinning.o \
   da_wrfvar_io.o \
   da_airsr.o \
   da_wrfvar_top.o \
   da_reporting.o \
   da_4dvar.o \
   module_wrf_error.o \
   module_configure.o \
   module_state_description.o \
   module_timing.o \
   module_driver_constants.o \
   module_domain.o \
   module_machine.o \
   module_symbols_util.o \
   module_utility.o \
   module_domain_type.o \
   module_date_time.o \
   module_io_wrf.o \
   module_io.o \
   module_io_domain.o \
   module_io_quilt.o \
   module_dm.o \
   module_comm_dm.o \
   module_bc.o \
   module_model_constants.o \
   module_nesting.o \
   module_tiles.o \
   module_get_file_names.o \
   module_bc_time_utilities.o \
   landread.o \
   da_memory.o \
   wrf_debug.o \
   set_timekeeping.o \
   wrf_shutdown.o \
   init_modules.o \
   mediation_wrfmain.o \
   mediation_integrate.o \
   wrf_num_bytes_between.o \
   input_wrf.o \
   wrf_bdyin.o \
   wrf_bdyout.o \
   output_wrf.o \
   wrf_ext_read_field.o \
   wrf_ext_write_field.o \
   collect_on_comm.o \
   start_domain.o \
   hires_timer.o \
   module_streams.o \
   module_comm_dm.o \
   module_comm_dm_0.o \
   module_comm_dm_1.o \
   module_comm_dm_2.o \
   module_comm_dm_3.o \
   module_comm_dm_4.o \
   module_alloc_space_0.o \
   module_alloc_space_1.o \
   module_alloc_space_2.o \
   module_alloc_space_3.o \
   module_alloc_space_4.o \
   module_alloc_space_5.o \
   module_alloc_space_6.o \
   module_alloc_space_7.o \
   module_alloc_space_8.o \
   module_alloc_space_9.o \
   nl_get_0_routines.o \
   nl_get_1_routines.o \
   nl_get_2_routines.o \
   nl_get_3_routines.o \
   nl_get_4_routines.o \
   nl_get_5_routines.o \
   nl_get_6_routines.o \
   nl_get_7_routines.o \
   nl_set_0_routines.o \
   nl_set_1_routines.o \
   nl_set_2_routines.o \
   nl_set_3_routines.o \
   nl_set_4_routines.o \
   nl_set_5_routines.o \
   nl_set_6_routines.o \
   nl_set_7_routines.o \
   interp_fcn.o  \
   sint.o        \
   da_netcdf_interface.o

# Aliases
var : wrfvar
var_esmf : wrfvar_esmf

wrfvar : da_wrfvar.exe da_advance_time.exe da_update_bc.exe da_update_bc_ad.exe

wrfvar_esmf : setup da_wrfvar_esmf.exe da_advance_time.exe da_update_bc.exe da_update_bc_ad.exe

da_wrfvar.exe : $(WRF_SRC_ROOT_DIR)/frame/module_internal_header_util.o \
                $(WRF_SRC_ROOT_DIR)/frame/pack_utils.o \
                da_control.o $(WRFVAR_LIBS) da_wrfvar_main.o
	$(RM) $@
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          $(LD) -o da_wrfvar.exe $(LDFLAGS) $(MODULE_DIRS) $(ESMF_IO_INC) da_control.o da_wrfvar_main.o $(WRFPLUS_DIR)/main/module_wrf_top.o -L. -lwrfvar $(CRTM_LIB) $(RTTOV_LIB) ${MADIS_LIB} ${BUFR_LIB} -L$(WRFPLUS_DIR)/main -lwrflib $(LIB) $(WAVELET_LIB) ; \
        else                                 \
          $(LD) -o da_wrfvar.exe $(LDFLAGS) $(MODULE_DIRS) $(ESMF_IO_INC) module_quilt_outbuf_ops.o da_control.o da_wrfvar_main.o -L. -lwrfvar $(CRTM_LIB) $(RTTOV_LIB) ${MADIS_LIB} ${BUFR_LIB} $(LIB) $(WAVELET_LIB) ; \
        fi
	@ if test -x $@ ; then cd ../da; $(LN) ../build/$@ . ; fi

da_wrfvar_esmf.exe : $(WRFVAR_LIBS) da_wrfvar_esmf.o da_wrfvar_esmf_super.o
	$(LD) -o $@ $(LDFLAGS) da_wrfvar_esmf.o $(WRFVAR_LIB) \
          da_wrfvar_esmf_super.o
	@ if test -x $@ ; then cd ../da; $(LN) ../build/$@ . ; fi

da_advance_time.exe : da_advance_time.o
	$(SFC) $(LDFLAGS) -o $@ da_advance_time.o
	@ if test -x $@ ; then cd ../da; $(LN) ../build/$@ . ; fi

da_generic_boilerplate.inc: da_generic_boilerplate.m4
	@ $(RM) da_generic_boilerplate.inc
	$(M4) da_generic_boilerplate.m4 > $(WRF_SRC_ROOT_DIR)/var/build/da_generic_boilerplate.inc

da_utils : \
           da_tune_obs_hollingsworth1.exe \
           da_tune_obs_hollingsworth2.exe \
           da_tune_obs_desroziers.exe \
           da_update_bc.exe \
           da_update_bc_ad.exe \
           da_advance_time.exe \
           da_verif_obs.exe \
           da_verif_grid.exe \
           da_bias_airmass.exe \
           da_bias_sele.exe \
           da_bias_scan.exe \
           da_bias_scan.exe \
           da_bias_verif.exe \
           da_rad_diags.exe

da_verif_obs.exe : da_verif_obs.o da_verif_obs_control.o da_verif_obs_init.o da_verif_tools.o $(WRF_SRC_ROOT_DIR)/external/io_netcdf/libwrfio_nf.a
	$(SFC) $(LDFLAGS) -o $@ da_verif_obs.o da_verif_obs_control.o da_verif_obs_init.o da_verif_tools.o $(LIB_EXTERNAL)
	@ if test -x $@ ;  then cd ../da; $(LN) ../build/$@ . ; fi

da_verif_grid.exe : da_verif_grid.o da_verif_grid_control.o da_netcdf_interface.o $(WRF_SRC_ROOT_DIR)/external/io_netcdf/libwrfio_nf.a
	$(SFC) $(LDFLAGS) -o $@ da_verif_grid.o da_netcdf_interface.o \
           da_verif_grid_control.o $(LIB_EXTERNAL)
	@ if test -x $@ ;  then cd ../da; $(LN) ../build/$@ . ; fi

da_tune_obs_hollingsworth1.exe: da_tune_obs_hollingsworth1.o
	$(SFC) $(LDFLAGS) -o $@ da_tune_obs_hollingsworth1.o da_control.o \
	   module_driver_constants.o
	@ if test -x $@ ;  then cd ../da; $(LN) ../build/$@ . ; fi

da_tune_obs_hollingsworth2.exe: da_tune_obs_hollingsworth2.o
	$(SFC) $(LDFLAGS) -o $@ da_tune_obs_hollingsworth2.o da_control.o \
	    module_driver_constants.o
	@ if test -x $@ ;  then cd ../da; $(LN) ../build/$@ . ; fi

da_tune_obs_desroziers.exe: da_tune_obs_desroziers.o
	$(SFC) $(LDFLAGS) -o $@ da_tune_obs_desroziers.o
	@ if test -x $@ ;  then cd ../da; $(LN) ../build/$@ . ; fi

da_update_bc.exe : da_update_bc.o $(WRF_SRC_ROOT_DIR)/external/io_netcdf/libwrfio_nf.a
	$(SFC) $(LDFLAGS) -o $@ da_update_bc.o \
           da_netcdf_interface.o \
           da_module_couple_uv.o $(LIB_EXTERNAL)
	@ if test -x $@ ;  then cd ../da; $(LN) ../build/$@ . ; fi

da_update_bc_ad.exe : da_update_bc_ad.o $(WRF_SRC_ROOT_DIR)/external/io_netcdf/libwrfio_nf.a
	$(SFC) $(LDFLAGS) -o $@ da_update_bc_ad.o \
           da_netcdf_interface.o da_module_couple_uv.o \
           da_module_couple_uv_ad.o $(LIB_EXTERNAL)
	@ if test -x $@ ;  then cd ../da; $(LN) ../build/$@ . ; fi

da_bias_airmass.exe : da_bias_airmass.o  rad_bias.o pythag.o tqli.o tred2.o regress_one.o
	$(SFC) $(LDFLAGS) -o  $@ da_bias_airmass.o rad_bias.o pythag.o tqli.o tred2.o regress_one.o
	@ if test -x $@ ;  then cd ../da; $(LN) ../build/$@ . ; fi

da_bias_sele.exe : da_bias_sele.o rad_bias.o
	$(SFC) $(LDFLAGS) -o $@ da_bias_sele.o rad_bias.o
	@ if test -x $@ ;  then cd ../da; $(LN) ../build/$@ . ; fi

da_bias_scan.exe : da_bias_scan.o rad_bias.o
	$(SFC) $(LDFLAGS) -o $@ da_bias_scan.o rad_bias.o
	@ if test -x $@ ;  then cd ../da; $(LN) ../build/$@ . ; fi

da_bias_verif.exe : da_bias_verif.o rad_bias.o
	$(SFC) $(LDFLAGS) -o $@ da_bias_verif.o rad_bias.o
	@ if test -x $@ ;  then cd ../da; $(LN) ../build/$@ . ; fi

da_rad_diags.exe : da_rad_diags.o 
	$(SFC) $(LDFLAGS) -o $@ da_rad_diags.o $(LIB_EXTERNAL)
	@ if test -x $@ ; then cd ../da; $(LN) ../build/$@ . ; fi

wrf_num_bytes_between.o :
	$(CC) -c $(CFLAGS) wrf_num_bytes_between.c

hires_timer.o :
	$(CC) -c $(CFLAGS) hires_timer.c

da_join_iv_for_multi_inc.o \
copyfile.o :
	$(SCC) -c $(CFLAGS) $*.c

amsr2time_.o :
	$(SCC) -c $(CFLAGS) $*.c

module_state_description.F : ../../Registry/$(REGISTRY)
	(cd $(WRF_SRC_ROOT_DIR); tools/registry $(ARCHFLAGS) -DNEW_BDYS Registry/$(REGISTRY) ; cd $(WRF_SRC_ROOT_DIR)/var/build )
	$(LN) $(WRF_SRC_ROOT_DIR)/frame/module_state_description.F .
	@ $(LN) $(WRF_SRC_ROOT_DIR)/inc/* inc/.

md_calls.inc : md_calls.m4
	if [ "$(M4)" = NA ] ; then \
	  /bin/cp $(WRF_SRC_ROOT_DIR)/arch/md_calls.inc . ; \
	else \
	  $(M4) md_calls.m4 > md_calls.inc ; \
	fi

$(WRF_SRC_ROOT_DIR)/frame/module_internal_header_util.o :
	$(RM) $@
	cp -f $(WRF_SRC_ROOT_DIR)/external/io_int/module_internal_header_util.o $@

$(WRF_SRC_ROOT_DIR)/frame/pack_utils.o :
	$(RM) $@
	$(CC) -o $@ -c $(CFLAGS) $*.c

init_modules.o :
	$(RM) $@
	$(SED_FTN) $*.F > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $*.b  > $*.f90
	$(RM) $*.b
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f90 > $*.f.tmp ; \
          mv $*.f.tmp $*.f90 ; \
        fi
	$(SFC) -c $(FCFLAGS) $(PROMOTION) -I../../external/io_int $*.f90

da_bias_verif.o da_bias_scan.o da_bias_sele.o da_bias_airmass.o da_rad_diags.o \
da_tune_obs_hollingsworth1.o da_tune_obs_hollingsworth2.o da_tune_obs_desroziers.o \
da_verif_obs_control.o da_verif_obs_init.o da_verif_grid_control.o da_verif_obs.o \
da_verif_grid.o da_update_bc.o da_update_bc_ad.o :
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) -I$(NETCDF)/include $*.b  > $*.f
	$(RM) $*.b
	$(SFC) -c $(FCFLAGS) $(PROMOTION) -I$(NETCDF)/include $*.f

rad_bias.o pythag.o tqli.o tred2.o regress_one.o da_update_bc.o :
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $*.b  > $*.f
	$(RM) $*.b
	$(SFC) -c $(FCFLAGS) $(PROMOTION) -I$(NETCDF)/include $*.f

da_netcdf_interface.o da_module_couple_uv.o gen_be_etkf.o netcdf_interface.o \
da_gen_be.o gen_be_ensmean.o gen_be_addmean.o:
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) -I$(NETCDF)/include $*.b  > $*.f
	$(RM) $*.b
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.f90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi
	$(SFC) -c $(FCFLAGS) $(PROMOTION) $*.f

da_etkf.o da_tools.o :
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $*.b  > $*.f
	$(RM) $*.b
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.f90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi
	$(FC) -c $(FCFLAGS) $(PROMOTION) $*.f

da_4dvar.o :
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          ${LN} ${WRFPLUS_DIR}/dyn_em/module_big_step_utilities_em.mod . ; \
          ${LN} ${WRFPLUS_DIR}/wrftladj/g_module_big_step_utilities_em.mod . ; \
          ${LN} ${WRFPLUS_DIR}/wrftladj/a_module_big_step_utilities_em.mod . ; \
          ${LN} ${WRFPLUS_DIR}/main/module_wrf_top.mod . ; \
          ${LN} ${WRFPLUS_DIR}/share/mediation_pertmod_io.mod . ; \
        fi
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $*.b  > $*.f
	$(RM) $*.b
	$(FC) -c $(FCFLAGS) $(PROMOTION) $(WRFPLUS_INC) $*.f

da_wrfvar_main.o \
da_wrfvar_io.o \
da_wrfvar_top.o :
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $(RTTOV_SRC) $*.b  > $*.f
	$(RM) $*.b
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.f90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi
	if $(FGREP) '!$$OMP' $*.f ; then \
          if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITH OMP ; fi ; \
	  $(FC) -c $(FCFLAGS) $(OMP) $(PROMOTION) $(CRTM_SRC) $(RTTOV_SRC) $(WRFPLUS_INC) $(HDF5_INC) -I$(NETCDF)/include $*.f ; \
        else \
          if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITHOUT OMP ; fi ; \
	  $(FC) -c $(FCFLAGS) $(PROMOTION) $(CRTM_SRC) $(RTTOV_SRC) $(WRFPLUS_INC) $(HDF5_INC) -I$(NETCDF)/include $*.f ; \
        fi

da_radiance.o :
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $(RTTOV_SRC) $*.b  > $*.f
	$(RM) $*.b
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.f90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi
	if $(FGREP) '!$$OMP' $*.f ; then \
          if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITH OMP ; fi ; \
	  $(FC) -c $(FCFLAGS) $(OMP) $(PROMOTION) $(CRTM_SRC) $(RTTOV_SRC) $(HDF5_INC) -I$(NETCDF)/include $*.f ; \
        else \
	if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITHOUT OMP ; fi ; \
	  $(FC) -c $(FCFLAGS) $(PROMOTION) $(CRTM_SRC) $(RTTOV_SRC) $(HDF5_INC) -I$(NETCDF)/include $*.f ; \
        fi

da_radiance1.o \
module_radiance.o \
da_setup_structures.o :
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $(RTTOV_SRC) $*.b  > $*.f
	$(RM) $*.b
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
           echo COMPILING $*.f90 for 4DVAR ; \
           $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
           mv $*.f.tmp $*.f ; \
         fi
	if $(FGREP) '!$$OMP' $*.f ; then \
          if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITH OMP ; fi ; \
	  $(FC) -c $(FCFLAGS) $(OMP) $(PROMOTION) $(CRTM_SRC) $(RTTOV_SRC) $(HDF5_INC) $*.f ; \
        else \
	if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITHOUT OMP ; fi ; \
	  $(FC) -c $(FCFLAGS) $(PROMOTION) $(CRTM_SRC) $(RTTOV_SRC) $(HDF5_INC) $*.f ; \
        fi

da_obs_io.o \
da_obs.o \
da_crtm.o \
da_rttov.o \
da_varbc.o :
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $(RTTOV_SRC) $*.b  > $*.f
	$(RM) $*.b
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.f90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi
	if $(FGREP) '!$$OMP' $*.f ; then \
          if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITH OMP ; fi ; \
	  $(FC) -c $(FCFLAGS) $(OMP) $(PROMOTION) $(CRTM_SRC) $(RTTOV_SRC)  $*.f ; \
        else \
          if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITHOUT OMP ; fi ; \
	  $(FC) -c $(FCFLAGS) $(PROMOTION) $(CRTM_SRC) $(RTTOV_SRC) $*.f ; \
        fi

da_test.o \
da_transfer_model.o \
da_minimisation.o :
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $(RTTOV_SRC) $*.b  > $*.f
	$(RM) $*.b
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.f90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi
	if $(FGREP) '!$$OMP' $*.f ; then \
          if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITH OMP ; fi ; \
	  $(FC) -c $(FCFLAGS) $(OMP) $(PROMOTION) $(CRTM_SRC) $(RTTOV_SRC) $(WRFPLUS_INC) $(HDF5_INC) $*.f ; \
        else \
          if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITHOUT OMP ; fi ; \
	  $(FC) -c $(FCFLAGS) $(PROMOTION) $(CRTM_SRC) $(RTTOV_SRC) $(WRFPLUS_INC) $(HDF5_INC) $*.f ; \
        fi

da_blas.o \
da_lapack.o :
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $*.b  > $*.f
	$(RM) $*.b
	$(SFC) -c $(FCFLAGS) $*.f

da_spectral.o da_be_spectral.o :
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $*.b  > $*.f
	$(RM) $*.b
	$(FC) -c $(FCFLAGS) $(PROMOTION) -I../../external/fftpack/fftpack5  $*.f

da_advance_time.o :
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	x=`echo "$(SFC)" | awk '{print $$1}'` ; export x ; \
        if [ $$x = "gfortran" ] ; then \
           echo removing external declaration of iargc for gfortran ; \
           $(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $*.b | sed '/integer *, *external.*iargc/d' > $*.f ;\
        else \
           $(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $*.b > $*.f ; \
        fi
	$(RM) $*.b
	$(SFC) -c $(FCFLAGS) $(PROMOTION) -I$(NETCDF)/include $*.f

nl_set_0_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=0 -I./inc -DNL_set_ROUTINES nl_access_routines.F > xx0.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
	  $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl xx0.f90 > xx0.f90.tmp ; \
	  mv xx0.f90.tmp xx0.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) xx0.f90
	$(RM) xx0.f90

nl_set_1_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=1 -I./inc -DNL_set_ROUTINES nl_access_routines.F > xx1.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl xx1.f90 > xx1.f90.tmp ; \
          mv xx1.f90.tmp xx1.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) xx1.f90
	$(RM) xx1.f90

nl_set_2_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=2 -I./inc -DNL_set_ROUTINES nl_access_routines.F > xx2.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl xx2.f90 > xx2.f90.tmp ; \
          mv xx2.f90.tmp xx2.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) xx2.f90
	$(RM) xx2.f90

nl_set_3_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=3 -I./inc -DNL_set_ROUTINES nl_access_routines.F > xx3.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl xx3.f90 > xx3.f90.tmp ; \
          mv xx3.f90.tmp xx3.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) xx3.f90
	$(RM) xx3.f90

nl_set_4_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=4 -I./inc -DNL_set_ROUTINES nl_access_routines.F > xx4.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl xx4.f90 > xx4.f90.tmp ; \
          mv xx4.f90.tmp xx4.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) xx4.f90
	$(RM) xx4.f90

nl_set_5_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=5 -I./inc -DNL_set_ROUTINES nl_access_routines.F > xx5.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl xx5.f90 > xx5.f90.tmp ; \
          mv xx5.f90.tmp xx5.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) xx5.f90
	$(RM) xx5.f90

nl_set_6_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=6 -I./inc -DNL_set_ROUTINES nl_access_routines.F > xx6.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl xx6.f90 > xx6.f90.tmp ; \
          mv xx6.f90.tmp xx6.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) xx6.f90
	$(RM) xx6.f90

nl_set_7_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=7 -I./inc -DNL_set_ROUTINES nl_access_routines.F > xx7.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl xx7.f90 > xx7.f90.tmp ; \
          mv xx7.f90.tmp xx7.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) xx7.f90
	$(RM) xx7.f90

nl_get_0_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=0 -I./inc -DNL_get_ROUTINES nl_access_routines.F > yy0.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl yy0.f90 > yy0.f90.tmp ; \
          mv yy0.f90.tmp yy0.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) yy0.f90
	$(RM) yy0.f90

nl_get_1_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=1 -I./inc -DNL_get_ROUTINES nl_access_routines.F > yy1.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl yy1.f90 > yy1.f90.tmp ; \
          mv yy1.f90.tmp yy1.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) yy1.f90
	$(RM) yy1.f90

nl_get_2_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=2 -I./inc -DNL_get_ROUTINES nl_access_routines.F > yy2.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl yy2.f90 > yy2.f90.tmp ; \
          mv yy2.f90.tmp yy2.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) yy2.f90
	$(RM) yy2.f90

nl_get_3_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=3 -I./inc -DNL_get_ROUTINES nl_access_routines.F > yy3.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl yy3.f90 > yy3.f90.tmp ; \
          mv yy3.f90.tmp yy3.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) yy3.f90
	$(RM) yy3.f90

nl_get_4_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=4 -I./inc -DNL_get_ROUTINES nl_access_routines.F > yy4.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl yy4.f90 > yy4.f90.tmp ; \
          mv yy4.f90.tmp yy4.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) yy4.f90
	$(RM) yy4.f90

nl_get_5_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=5 -I./inc -DNL_get_ROUTINES nl_access_routines.F > yy5.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl yy5.f90 > yy5.f90.tmp ; \
          mv yy5.f90.tmp yy5.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) yy5.f90
	$(RM) yy5.f90

nl_get_6_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=6 -I./inc -DNL_get_ROUTINES nl_access_routines.F > yy6.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl yy6.f90 > yy6.f90.tmp ; \
          mv yy6.f90.tmp yy6.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) yy6.f90
	$(RM) yy6.f90

nl_get_7_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=7 -I./inc -DNL_get_ROUTINES nl_access_routines.F > yy7.f90
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl yy7.f90 > yy7.f90.tmp ; \
          mv yy7.f90.tmp yy7.f90 ; \
        fi
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS) $(MODULE_DIRS) $(FCSUFFIX) yy7.f90
	$(RM) yy7.f90

# DO NOT DELETE
