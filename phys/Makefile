

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES = \
	module_bl_ysu.o \
	module_bl_mrf.o \
	module_bl_gfs.o \
	module_bl_myjpbl.o \
	module_bl_qnsepbl.o \
	module_bl_acm.o \
	module_bl_mynn.o \
	module_bl_gwdo.o \
	module_bl_myjurb.o \
	module_bl_boulac.o \
	module_cu_g3.o \
	module_cu_kf.o  \
	module_cu_bmj.o \
	module_cu_kfeta.o \
	module_cu_gd.o \
	module_cu_sas.o \
	module_mp_kessler.o \
	module_mp_lin.o  \
	module_mp_wsm3.o \
	module_mp_wsm5.o \
	module_mp_wsm6.o \
	module_mp_etanew.o \
	module_mp_HWRF.o \
	module_mp_thompson.o \
	module_mp_thompson07.o \
	module_mp_gsfcgce.o \
	module_mp_morr_two_moment.o \
	module_mp_milbrandt2mom.o \
	module_mp_wdm5.o \
	module_mp_wdm6.o \
	module_ra_sw.o  \
	module_ra_gsfcsw.o \
	module_ra_rrtm.o  \
	module_ra_rrtmg_lw.o  \
	module_ra_rrtmg_sw.o  \
        module_ra_cam_support.o \
        module_ra_cam.o \
	module_ra_gfdleta.o \
	module_ra_HWRF.o \
	module_ra_hs.o  \
	module_sf_sfclay.o \
	module_sf_gfs.o \
	module_sf_gfdl.o \
	module_sf_slab.o  \
	module_sf_noahdrv.o  \
	module_sf_noahlsm.o  \
        module_sf_urban.o  \
        module_sf_bep.o  \
        module_sf_bep_bem.o \
        module_sf_bem.o \
	module_sf_pxlsm.o \
	module_sf_ruclsm.o \
	module_sf_sfcdiags.o \
	module_sf_sfcdiags_ruclsm.o \
	module_sf_sstskin.o \
	module_sf_tmnupdate.o \
	module_sf_oml.o \
	module_sf_myjsfc.o \
	module_sf_qnsesfc.o \
	module_sf_mynn.o \
	module_sf_pxsfclay.o \
	module_physics_addtendc.o \
	module_physics_init.o \
	module_gfs_machine.o \
	module_gfs_funcphys.o \
	module_gfs_physcons.o \
	module_progtm.o \
	module_pbl_driver.o \
	module_data_gocart_dust.o \
	module_cumulus_driver.o \
	module_microphysics_driver.o \
	module_microphysics_zero_out.o \
	module_mixactivate.o \
	module_radiation_driver.o \
	module_surface_driver.o \
	module_diagnostics.o \
	module_fdda_psufddagd.o \
	module_fdda_spnudging.o \
	module_fddagd_driver.o  \
        module_fddaobs_rtfdda.o \
	module_fddaobs_driver.o

FIRE_MODULES = \
        module_fr_sfire_driver.o \
        module_fr_sfire_atm.o \
        module_fr_sfire_model.o \
        module_fr_sfire_core.o \
        module_fr_sfire_phys.o \
        module_fr_sfire_util.o
  
OBJS    =

NMM_MODULES =  

LIBTARGET    =  physics
TARGETDIR    =  ./

$(LIBTARGET) :
		if [ $(WRF_NMM_CORE) -eq 1 ] ; then \
                  $(MAKE) nmm_contrib                                      ; \
                  $(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS) $(NMM_OBJS) $(NMM_MODULES) ; \
                else                                                         \
                  $(MAKE) non_nmm                                          ; \
                  $(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(FIRE_MODULES) $(OBJS)             ; \
                fi

include ../configure.wrf

nmm_contrib : $(NMM_OBJS) $(NMM_MODULES) $(MODULES) $(OBJS)

non_nmm : $(MODULES) $(FIRE_MODULES) $(OBJS)

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

module_bl_myjpbl.o: ../share/module_model_constants.o

module_bl_myjurb.o: ../share/module_model_constants.o

module_bl_boulac.o: ../share/module_model_constants.o

module_bl_qnsepbl.o: ../share/module_model_constants.o

module_bl_gfs.o: module_gfs_machine.o \
		 module_gfs_physcons.o

module_bl_mynn.o: ../share/module_model_constants.o

module_sf_mynn.o: module_sf_sfclay.o module_bl_mynn.o \
                ../share/module_model_constants.o

module_sf_gfdl.o : \
		module_gfs_machine.o \
		module_gfs_funcphys.o \
		module_gfs_physcons.o

module_cu_bmj.o: ../share/module_model_constants.o

module_cu_kf.o:  ../frame/module_wrf_error.o

module_cu_kfeta.o: ../frame/module_wrf_error.o

module_cu_gd.o:

module_gfs_physcons.o: module_gfs_machine.o

module_gfs_funcphys.o: module_gfs_machine.o \
		       module_gfs_physcons.o

module_cu_sas.o: module_gfs_machine.o \
		 module_gfs_funcphys.o \
		 module_gfs_physcons.o

module_ra_gfdleta.o:  ../frame/module_dm.o \
		module_mp_etanew.o

module_ra_HWRF.o:   ../frame/module_dm.o module_mp_HWRF.o

module_ra_rrtm.o: ../frame/module_wrf_error.o \
		../frame/module_dm.o

module_ra_cam_support.o: ../frame/module_wrf_error.o

module_ra_cam.o: module_ra_cam_support.o \
                ../frame/module_wrf_error.o

module_mp_lin.o : ../frame/module_wrf_error.o

module_mp_milbrandt2mom.o : ../frame/module_wrf_error.o \
		../share/module_model_constants.o

module_mp_thompson.o : ../frame/module_wrf_error.o \
			 ../frame/module_dm.o

module_sf_myjsfc.o: ../share/module_model_constants.o 

module_sf_qnsesfc.o: ../share/module_model_constants.o 

module_sf_gfs.o: module_gfs_machine.o \
		 module_gfs_funcphys.o \
		 module_gfs_physcons.o \
		 module_progtm.o

module_sf_noahdrv.o: module_sf_noahlsm.o module_data_gocart_dust.o \
                     module_sf_urban.o module_sf_bep.o module_sf_bep_bem.o 

module_sf_noahlsm.o: ../share/module_model_constants.o

module_sf_bep.o: ../share/module_model_constants.o module_sf_urban.o

module_sf_bep_bem.o: ../share/module_model_constants.o module_sf_bem.o 

module_sf_bem.o: ../share/module_model_constants.o

module_sf_ruclsm.o: ../frame/module_wrf_error.o module_data_gocart_dust.o 

module_sf_pxlsm.o: ../share/module_model_constants.o

module_ra_rrtmg_sw.o: module_ra_rrtmg_lw.o

module_physics_addtendc.o: \
		module_cu_kf.o			\
		module_cu_kfeta.o		\
		../frame/module_state_description.o \
		../frame/module_configure.o


module_physics_init.o : \
		module_ra_rrtm.o		\
		module_ra_rrtmg_lw.o		\
		module_ra_rrtmg_sw.o		\
		module_ra_cam.o		\
		module_ra_cam_support.o		\
		module_ra_sw.o			\
		module_ra_gsfcsw.o		\
		module_ra_gfdleta.o		\
		module_ra_HWRF.o		\
		module_ra_hs.o			\
		module_sf_sfclay.o		\
		module_sf_slab.o		\
		module_sf_myjsfc.o		\
		module_sf_mynn.o                \
		module_sf_urban.o                \
		module_sf_qnsesfc.o		\
		module_sf_pxsfclay.o		\
		module_sf_noahlsm.o		\
		module_sf_noahdrv.o		\
                module_sf_bep.o                 \
                module_sf_bep_bem.o             \
		module_sf_ruclsm.o		\
		module_sf_pxlsm.o		\
		module_bl_ysu.o			\
		module_bl_mrf.o			\
		module_bl_gfs.o			\
		module_bl_acm.o			\
		module_bl_myjpbl.o		\
		module_bl_qnsepbl.o		\
		module_bl_mynn.o                \
		module_bl_myjurb.o              \
		module_bl_boulac.o              \
		module_cu_kf.o			\
	        module_cu_g3.o                  \
		module_cu_kfeta.o		\
		module_cu_bmj.o			\
		module_cu_gd.o			\
		module_cu_sas.o			\
		module_mp_wsm3.o		\
		module_mp_wsm5.o		\
		module_mp_wsm6.o		\
		module_mp_etanew.o		\
		module_mp_HWRF.o		\
		module_fdda_psufddagd.o		\
		module_fdda_spnudging.o         \
		module_fddaobs_rtfdda.o		\
		module_mp_thompson.o            \
		module_mp_thompson07.o          \
		module_mp_gsfcgce.o 		\
                module_mp_morr_two_moment.o     \
                module_mp_milbrandt2mom.o     \
		module_mp_wdm5.o		\
		module_mp_wdm6.o		\
		../frame/module_state_description.o \
		../frame/module_configure.o \
		../frame/module_wrf_error.o \
		../frame/module_dm.o \
		../share/module_llxy.o \
		../share/module_model_constants.o 

module_microphysics_driver.o: \
		module_mixactivate.o \
		module_mp_kessler.o module_mp_lin.o \
		module_mp_wsm3.o module_mp_wsm5.o \
		module_mp_wsm6.o module_mp_etanew.o \
		module_mp_HWRF.o  \
		module_mp_thompson.o            \
		module_mp_thompson07.o          \
		module_mp_gsfcgce.o 		\
                module_mp_morr_two_moment.o     \
                module_mp_milbrandt2mom.o     \
		module_mp_wdm5.o module_mp_wdm6.o \
		../frame/module_driver_constants.o \
		../frame/module_state_description.o \
		../frame/module_wrf_error.o \
		../frame/module_configure.o \
		../share/module_model_constants.o 


module_cumulus_driver.o: \
                module_cu_kf.o \
	        module_cu_g3.o \
		module_cu_kfeta.o \
		module_cu_bmj.o \
		module_cu_gd.o \
		module_cu_sas.o \
		../frame/module_state_description.o \
		../frame/module_configure.o \
		../frame/module_domain.o \
		../frame/module_dm.o \
		../frame/module_comm_dm.o \
		../frame/module_wrf_error.o \
		../share/module_model_constants.o

module_pbl_driver.o:  \
		module_bl_myjpbl.o \
		module_bl_myjurb.o \
		module_bl_qnsepbl.o \
		module_bl_acm.o \
		module_bl_ysu.o \
		module_bl_mrf.o \
                module_bl_boulac.o \
		module_bl_gfs.o \
		module_bl_mynn.o \
		module_bl_gwdo.o \
		../frame/module_state_description.o \
		../frame/module_configure.o \
		../share/module_model_constants.o 

module_data_gocart_dust.o: 

module_mixactivate.o: \
		module_radiation_driver.o

module_radiation_driver.o: \
		module_ra_sw.o \
		module_ra_gsfcsw.o \
		module_ra_rrtm.o \
		module_ra_rrtmg_lw.o \
		module_ra_rrtmg_sw.o \
		module_ra_cam.o \
		module_ra_gfdleta.o \
		module_ra_HWRF.o \
		module_ra_hs.o \
		../frame/module_driver_constants.o \
		../frame/module_state_description.o \
		../frame/module_dm.o \
		../frame/module_comm_dm.o \
		../frame/module_domain.o \
		../frame/module_wrf_error.o \
		../frame/module_configure.o \
		../share/module_bc.o  \
		../share/module_model_constants.o 

module_surface_driver.o: \
		module_sf_sfclay.o		\
		module_sf_slab.o		\
		module_sf_myjsfc.o		\
		module_sf_qnsesfc.o		\
		module_sf_pxsfclay.o		\
		module_sf_gfs.o  		\
		module_sf_noahdrv.o		\
		module_sf_ruclsm.o		\
		module_sf_pxlsm.o		\
		module_sf_mynn.o                \
		module_sf_sfcdiags.o		\
		module_sf_sfcdiags_ruclsm.o     \
		module_sf_sstskin.o		\
		module_sf_tmnupdate.o		\
		../frame/module_state_description.o \
		../frame/module_configure.o \
		../share/module_model_constants.o  

module_diagnostics.o: ../frame/module_dm.o


module_mixactivate.o: \
		module_radiation_driver.o

module_fddagd_driver.o: \
		module_fdda_spnudging.o \
		module_fdda_psufddagd.o \
		../frame/module_state_description.o \
		../frame/module_configure.o \
		../share/module_model_constants.o

module_fddaobs_driver.o: \
		../frame/module_domain.o \
		../share/module_bc.o  \
		../share/module_model_constants.o  \
		module_fddaobs_rtfdda.o

module_fr_sfire_driver.o: \
		../share/module_model_constants.o  \
                ../frame/module_comm_dm.o \
		module_fr_sfire_atm.o \
		module_fr_sfire_phys.o \
		module_fr_sfire_model.o \
		module_fr_sfire_util.o 

module_fr_sfire_atm.o: \
		../share/module_model_constants.o  \
		module_fr_sfire_util.o 

module_fr_sfire_model.o: \
                fr_sfire_params_args.h \
                fr_sfire_params_decl.h \
	        module_fr_sfire_core.o \
                module_fr_sfire_phys.o \
		module_fr_sfire_util.o 

module_fr_sfire_core.o: \
                fr_sfire_params_args.h \
                fr_sfire_params_decl.h \
		module_fr_sfire_util.o \
	        module_fr_sfire_phys.o 

module_fr_sfire_phys.o: \
		../share/module_model_constants.o  \
                fr_sfire_params_args.h \
                fr_sfire_params_decl.h \
		module_fr_sfire_util.o 

module_fire_debug_output.o: \
	        ../frame/module_domain.o \
	        ../frame/module_configure.o \
	        ../share/mediation_integrate.o

module_fdda_spnudging.o :\
		../frame/module_dm.o \
		../frame/module_state_description.o \
		../frame/module_domain.o \
		../frame/module_wrf_error.o

module_sf_bep.o :\
		module_sf_urban.o

module_mp_wsm5.o :\
		module_mp_wsm5_accel.F

module_mp_wsm3.o :\
		module_mp_wsm3_accel.F


# DO NOT DELETE
