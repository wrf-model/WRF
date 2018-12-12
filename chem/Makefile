#
#  Changing this file requires mods to 
#	WRFV3_top/chem/KPP/util/wkc/change_chem_Makefile.c
#	Right now it looks at the line in MODULES with module_data_sorgam
#	AND it cares about the word DEPENDENCIES.  DO NOT EVEN CHANGE
#	SPACING ON THESE TWO LINES.

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES =                                 \
        module_data_isrpia_data.o         \
        module_data_ISRPIA.o              \
        module_data_isrpia_asrc.o         \
        module_data_isrpia_solut.o        \
        module_data_isrpia_kmc198.o       \
        module_data_isrpia_kmc223.o       \
        module_data_isrpia_kmc248.o       \
        module_data_isrpia_kmc273.o       \
        module_data_isrpia_kmc298.o       \
        module_data_isrpia_kmc323.o       \
        module_data_isrpia_expnc.o        \
        module_data_isrpia_caseg.o        \
        module_data_isrpia_casej.o        \
        isofwd.o                          \
        isorev.o                          \
        isocom.o                          \
        moduleHETDATA.o                   \
        moduleHETAERO.o                   \
        moduleAERODATA.o                  \
        aerorate_so2.o                    \
        module_aer_opt_out.o              \
        module_add_emiss_burn.o           \
        module_add_emis_cptec.o           \
        module_bioemi_beis314.o           \
        module_chem_utilities.o           \
        module_cmu_dvode_solver.o         \
        module_ctrans_aqchem.o            \
        module_data_cbmz.o                \
        module_data_cmu_bulkaqchem.o      \
        module_data_gocartchem.o          \
        module_data_gocart_seas.o         \
        module_data_mosaic_kind.o         \
        module_data_mosaic_constants.o    \
        module_data_mosaic_aero.o         \
        module_data_mosaic_main.o         \
        module_data_mosaic_asect.o        \
        module_data_mosaic_asecthp.o      \
        module_data_mosaic_boxmod.o       \
        module_data_mosaic_other.o        \
        module_data_mosaic_therm.o        \
        module_data_radm2.o               \
        module_data_rrtmgaeropt.o         \
        module_data_megan2.o              \
        module_data_soa_vbs.o             \
        module_data_sorgam.o              \
        module_data_sorgam_vbs.o          \
        module_ftuv_subs.o                \
        module_ghg_fluxes.o               \
        module_gocart_drydep.o            \
        module_gocart_settling.o          \
        module_gocart_so2so4.o            \
        module_HLaw.o                     \
        module_input_tracer_data.o        \
        module_interpolate.o              \
        module_mosaic_csuesat.o           \
        module_mozcart_wetscav.o          \
        module_peg_util.o                 \
        module_tropopause.o               \
        module_upper_bc_driver.o          \
        module_vertmx_wrf.o               \
        module_wave_data.o                \
        module_wetdep_ls.o                \
        module_zero_plumegen_coms.o       \
        module_vash_settling.o            \
        module_chem_plumerise_scalar.o    \
        module_dep_simple.o               \
        module_gocart_dmsemis.o           \
        module_gocart_aerosols.o          \
        module_gocart_dust.o              \
        module_gocart_dust_afwa.o         \
        module_gocart_seasalt.o           \
        module_uoc_dust.o                 \
        module_qf03.o                     \
        module_soilpsd.o                  \
        module_dust_load.o                \
        module_uoc_dustwd.o               \
        module_data_uoc_wd.o              \
        module_mosaic_addemiss.o          \
        module_mosaic_initmixrats.o       \
        module_mosaic_support.o           \
        module_mosaic_init_aerpar.o       \
        module_mosaic_ext.o               \
        module_mosaic_astem.o             \
        module_mosaic_lsode.o             \
        module_mosaic_box_aerchem.o       \
        module_mosaic_aerchem_intr.o      \
        module_mosaic_coag1d.o            \
        module_mosaic_coag3d.o            \
        module_mosaic_movesect1d.o        \
        module_mosaic_movesect3d.o        \
        module_mosaic_newnucb.o           \
        module_mosaic_sect_intr.o         \
        module_mosaic_aerdynam_intr.o     \
        module_mosaic_movesect.o          \
        module_mosaic_newnuc.o            \
        module_mosaic_soa_vbs.o           \
        module_cbmz_lsodes_solver.o       \
        module_cbmz_rodas3_solver.o       \
        module_cmu_bulkaqchem.o           \
        module_data_mgn2mech.o            \
        module_ftuv_driver.o              \
        module_fastj_data.o               \
        module_fastj_mie.o                \
        module_input_chem_data.o          \
        module_mosaic_coag.o              \
        module_mosaic_gly.o               \
        module_mosaic_wetscav.o           \
        module_mosaic_therm.o             \
        module_phot_mad.o                 \
        params.mod.o                      \
        numer.o                           \
        rdxs.o                            \
        rxn.o                             \
        params_mod.o                      \
        module_phot_tuv.o                 \
        module_subs_tuv.o                 \
        rtrans.o                          \
        la_srb.o                          \
        module_radm.o                     \
        module_sorgam_aqchem.o            \
        module_sorgam_vbs_aqchem.o        \
        module_aerosols_soa_vbs.o         \
        module_aerosols_sorgam.o          \
        module_aerosols_sorgam_vbs.o      \
        module_bioemi_megan2.o            \
        module_bioemi_simple.o            \
        module_cbm4_initmixrats.o         \
        module_cb05_initmixrats.o         \
        module_cb05_vbs_initmixrats.o     \
        module_cbmz.o                     \
        module_cbmz_initmixrats.o         \
        module_cbmz_rodas_prep.o          \
        module_ctrans_grell.o             \
        module_gocart_chem.o              \
        module_input_tracer.o             \
	module_lightning_nox_driver.o     \
	module_lightning_nox_ott.o        \
	module_lightning_nox_decaria.o    \
        module_mixactivate_wrappers.o     \
        module_mosaic_init_aerpar.o       \
        module_mosaic2_driver.o           \
        module_mosaic_sumpm.o             \
        module_mosaic_driver.o            \
        module_optical_averaging.o        \
        module_plumerise1.o               \
        module_mosaic_drydep.o            \
        module_wetscav_driver.o           \
        module_prep_wetscav_sorgam.o      \
        module_input_chem_bioemiss.o      \
        module_input_dust_errosion.o      \
        module_input_gocart_dms.o         \
        module_cbmz_addemiss.o            \
        module_cbm4_addemiss.o            \
        module_cb05_addemiss.o            \
        module_emissions_anthropogenics.o \
        module_aer_drydep.o               \
        module_cam_mam_calcsize.o         \
        module_cam_mam_dust_sediment.o    \
        module_cam_mam_drydep.o           \
        module_cam_mam_init.o             \
        module_cam_mam_initaerodata.o     \
        module_cam_mam_initmixrats.o      \
        module_cam_mam_rename.o           \
        module_cam_mam_wateruptake.o      \
        module_cam_mam_gasaerexch.o       \
        module_cam_mam_coag.o             \
        module_cam_mam_newnuc.o           \
        module_cam_mam_aerchem_driver.o   \
        module_cam_mam_addemiss.o         \
        module_cam_mam_wetscav.o          \
        module_cam_mam_mz_aerosols_intr.o \
        module_cam_mam_wetdep.o           \
        module_cam_mam_cloudchem.o        \
        module_cam_mam_setsox.o           \
        module_cam_mam_mo_chem_utls.o     \
        module_mosaic_cloudchem.o         \
        module_sorgam_cloudchem.o         \
        module_sorgam_vbs_cloudchem.o     \
        module_cam_mam_gas_wetdep_driver.o \
        module_cam_mam_mo_sethet.o         \
        module_phot_fastj.o               \
	module_chem_cup.o      \
        module_isocom.o                   \
        module_isofwd.o                   \
        module_isorev.o

OBJS    =                           \
	chemics_init.o              \
	chem_driver.o               \
	cloudchem_driver.o          \
	photolysis_driver.o         \
	optical_driver.o            \
	mechanism_driver.o          \
	emissions_driver.o          \
	dry_dep_driver.o            \
	aerosol_driver.o 

LIBTARGET    =  chemics
TARGETDIR    =  ./
$(LIBTARGET) :  MODULE DRIVERS
		$(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS)

MODULE  : $(MODULES)

DRIVERS : $(OBJS)

include ../configure.wrf

convert_emiss : convert_emiss.o
	$(RANLIB) ../main/$(LIBWRFLIB)
	$(FC) -o convert_emiss.exe $(LDFLAGS) convert_emiss.o ../main/$(LIBWRFLIB) $(LIB)

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include depend.chem

