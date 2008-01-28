# phys

PHYS_OBJS = \
	module_bl_ysu.o \
	module_bl_mrf.o \
	module_bl_gfs.o \
	module_bl_myjpbl.o \
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
	module_mp_thompson.o \
	module_ra_hs.o  \
	module_ra_sw.o  \
	module_ra_gsfcsw.o \
	module_ra_rrtm.o  \
	module_ra_gfdleta.o \
	module_ra_cam.o \
	module_sf_sfclay.o \
	module_sf_gfs.o \
	module_sf_slab.o  \
	module_sf_noahlsm.o  \
	module_sf_ruclsm.o \
	module_sf_sfcdiags.o \
	module_sf_myjsfc.o \
	module_sf_urban.o \
	module_physics_addtendc.o \
	module_physics_init.o \
	module_gfs_machine.o \
	module_gfs_funcphys.o \
	module_gfs_physcons.o \
	module_progtm.o \
	module_pbl_driver.o \
	module_cumulus_driver.o \
	module_microphysics_driver.o \
	module_microphysics_zero_out.o \
	module_radiation_driver.o \
	module_surface_driver.o \
        module_fdda_psufddagd.o \
        module_fddagd_driver.o \
	module_fddaobs_driver.o \
	module_fddaobs_rtfdda.o \
	module_diagnostics.o \
        module_mixactivate.o \
        module_mp_morr_two_moment.o

PHYS_nmm_OBJS = \
	module_sf_lsm_nmm.o
