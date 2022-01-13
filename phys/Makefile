

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES = \
	module_bl_gwdo_gsl.o \
	module_bl_myjurb.o \
	module_bl_boulac.o \
	module_checkerror.o \
	module_bl_camuwpbl_driver.o \
	module_bl_mfshconvpbl.o \
        module_cu_gf_ctrans.o \
	module_cu_gf_wrfdrv.o \
	module_cu_gf_deep.o \
	module_cu_gf_sh.o \
	module_cu_ksas.o \
	module_cu_nsas.o \
	module_cu_sas.o \
	module_gocart_coupling.o \
	module_cu_scalesas.o \
	module_cu_osas.o \
	module_cu_kfcup.o \
	module_mp_ntu.o \
	module_mp_cammgmp_driver.o \
	module_ra_sw.o  \
	module_ra_clWRF_support.o  \
	module_ra_gsfcsw.o \
	module_ra_goddard.o \
        module_ra_effective_radius.o \
	module_ra_rrtm.o  \
	module_ra_rrtmg_lw.o  \
	module_ra_rrtmg_sw.o  \
	module_ra_rrtmg_lwf.o  \
	module_ra_rrtmg_swf.o  \
        module_ra_rrtmg_lwk.o  \
        module_ra_rrtmg_swk.o  \
        module_ra_cam_support.o \
        module_ra_cam.o \
	module_ra_gfdleta.o \
        module_ra_flg.o \
	module_ra_HWRF.o \
	module_ra_hs.o  \
	module_sf_ctsm.o \
        module_sf_ssib.o  \
	module_sf_noah_seaice.o \
	module_sf_noah_seaice_drv.o \
	module_sf_noahlsm_glacial_only.o \
        module_sf_noahmp_groundwater.o \
        module_sf_gecros.o \
	module_sf_noahmpdrv.o \
	module_sf_noahmplsm.o \
	module_sf_noahmp_glacier.o \
        module_sf_urban.o  \
        module_sf_bep.o  \
        module_sf_bep_bem.o \
        module_sf_bem.o \
	module_sf_pxlsm.o \
	module_sf_pxlsm_data.o \
	module_sf_ruclsm.o \
	module_sf_sfcdiags.o \
	module_sf_sfcdiags_ruclsm.o \
	module_sf_sstskin.o \
	module_sf_tmnupdate.o \
	module_sf_ocean_driver.o \
	module_sf_oml.o \
	module_sf_3dpwp.o \
	module_sf_myjsfc.o \
	module_sf_qnsesfc.o \
	module_sf_mynn.o \
	module_sf_fogdes.o \
	module_sf_pxsfclay.o \
	module_sf_temfsfclay.o \
	module_sf_idealscmsfclay.o \
	module_sf_scmflux.o \
	module_sf_scmskintemp.o \
	module_physics_addtendc.o \
	module_physics_init.o \
	module_gfs_machine.o \
	module_gfs_funcphys.o \
	module_gfs_physcons.o \
	module_progtm.o \
	module_pbl_driver.o \
	module_data_gocart_dust.o \
        module_dust_emis.o \
	module_cumulus_driver.o \
	module_shallowcu_driver.o \
	module_microphysics_driver.o \
	module_microphysics_zero_out.o \
	module_mixactivate.o \
	module_radiation_driver.o \
	module_surface_driver.o \
	module_lightning_driver.o \
	module_ltng_cpmpr92z.o \
	module_ltng_crmpr92.o \
	module_ltng_iccg.o \
	module_fdda_psufddagd.o \
	module_fdda_spnudging.o \
	module_fddagd_driver.o  \
        module_fddaobs_rtfdda.o \
	module_fddaobs_driver.o \
	module_wind_fitch.o \
	module_sf_lake.o \
	module_diagnostics_driver.o \
	module_irrigation.o

FIRE_MODULES = \
        module_fr_fire_driver.o \
        module_fr_fire_driver_wrf.o \
        module_fr_fire_atm.o \
        module_fr_fire_model.o \
        module_fr_fire_core.o \
        module_fr_fire_phys.o \
        module_fr_fire_util.o

DIAGNOSTIC_MODULES_EM = \
	module_diag_afwa.o \
	module_diag_cl.o \
	module_diag_functions.o \
        module_diag_hailcast.o \
	module_diag_misc.o \
	module_diag_nwp.o \
        module_diag_rasm.o \
	module_diag_pld.o \
	module_diag_zld.o \
	module_diag_trad_fields.o \
        module_diag_solar.o
  
DIAGNOSTIC_MODULES_NMM = \
	module_diag_refl.o

OBJS    =

NMM_MODULES =  

LIBTARGET    =  physics
TARGETDIR    =  ./

$(LIBTARGET) :
		if [ $(WRF_NMM_CORE) -eq 1 ] ; then \
                  $(MAKE) $(J) nmm_contrib  ; \
                  $(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS) \
                                   $(NMM_OBJS) $(NMM_MODULES) \
                                   $(DIAGNOSTIC_MODULES_NMM) ; \
                else                                                         \
                  $(MAKE) $(J) non_nmm      ; \
                  $(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS) \
                                   $(FIRE_MODULES) \
                                   $(DIAGNOSTIC_MODULES_EM)  ; \
                fi

include ../configure.wrf

nmm_contrib : $(NMM_OBJS) $(NMM_MODULES) $(MODULES) $(OBJS) $(DIAGNOSTIC_MODULES_NMM)

non_nmm : $(MODULES) $(FIRE_MODULES) $(WIND_MODULES) $(OBJS) $(DIAGNOSTIC_MODULES_EM)

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
# DO NOT DELETE

include ../main/depend.common
