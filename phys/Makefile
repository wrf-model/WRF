

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES = \
	ccpp_kind_types.o \
	module_bep_bem_helper.o \
	complex_number_module.o \
	module_cam_shr_kind_mod.o \
	module_cam_support.o \
	module_cam_shr_const_mod.o \
	module_cam_physconst.o \
	module_cam_cldwat.o \
	module_cam_wv_saturation.o \
	module_cam_esinti.o \
	module_cam_gffgch.o \
	module_cam_error_function.o \
	module_cam_constituents.o \
	module_cam_trb_mtn_stress.o \
	module_cam_molec_diff.o \
	module_cam_upper_bc.o \
	module_cam_bl_diffusion_solver.o \
	module_cam_bl_eddy_diff.o \
	module_cam_mp_microp_aero.o \
	module_cam_mp_cldwat2m_micro.o \
	module_cam_mp_ndrop.o \
	module_cam_mp_modal_aero_initialize_data_phys.o \
	module_cam_mp_radconstants.o \
	module_cam_mp_conv_water.o \
	module_cam_mp_qneg3.o \
	module_data_cam_mam_aero.o \
	module_data_cam_mam_asect.o \
	module_cam_infnan.o \
	$(PHYS_PLUS) \
	module_bl_ysu.o \
	module_bl_shinhong.o \
	module_bl_mrf.o \
	module_bl_gfs.o \
        module_bl_gfsedmf.o \
	module_bl_myjpbl.o \
	module_bl_qnsepbl.o \
	module_bl_acm.o \
        module_bl_mynnedmf_common.o \
        module_bl_mynnedmf.o \
        module_bl_mynnedmf_driver.o \
	module_bl_fogdes.o \
	module_bl_gwdo.o \
	module_bl_gwdo_gsl.o \
	module_bl_myjurb.o \
	module_bl_boulac.o \
	module_checkerror.o \
	module_bl_camuwpbl_driver.o \
	module_bl_mfshconvpbl.o \
	module_bl_eepsilon.o \
	module_bl_keps.o \
        module_shcu_camuwshcu_driver.o \
	module_shcu_camuwshcu.o \
	module_shcu_deng.o          \
	module_shcu_grims.o \
	module_shcu_nscv.o \
	module_cu_camzm_driver.o \
	module_cu_camzm.o \
	module_bl_temf.o \
	module_bl_gbmpbl.o \
	module_cu_g3.o \
	module_cu_kf.o  \
	module_cu_bmj.o \
	module_cu_kfeta.o \
	module_cu_mskf.o \
	module_cu_tiedtke.o\
	module_cu_ntiedtke.o\
	module_cu_gd.o \
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
	module_madwrf.o \
	module_mp_radar.o \
	module_mp_kessler.o \
	module_mp_lin.o  \
	module_mp_sbu_ylin.o  \
	module_mp_wsm3.o \
	module_mp_wsm5.o \
	module_mp_wsm6.o \
	module_mp_wsm6r.o \
	module_mp_wsm7.o \
	module_mp_etanew.o \
	module_mp_fer_hires.o \
	module_mp_thompson.o \
	module_mp_tempo_cfgs.o \
	module_mp_tempo_params.o \
	module_mp_tempo_utils.o \
	module_mp_tempo_diags.o \
	module_mp_tempo_ml.o \
	module_mp_tempo_aerosols.o \
	module_mp_tempo_main.o \
	module_mp_tempo_driver.o \
	module_mp_rcon.o \
        module_fire_emis.o \
	module_mp_SBM_polar_radar.o \
	module_mp_full_sbm.o \
	module_mp_fast_sbm.o \
	module_ltng_lpi.o \
	module_mp_gsfcgce.o \
	module_mp_gsfcgce_4ice_nuwrf.o \
	module_mp_morr_two_moment.o \
        module_mp_p3.o \
        module_mp_jensen_ishmael.o \
        module_mp_morr_two_moment_aero.o \
	module_mp_milbrandt2mom.o \
	module_mp_nssl_2mom.o \
	module_mp_wdm5.o \
	module_mp_wdm6.o \
	module_mp_wdm7.o \
	module_mp_udm.o \
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
	module_ra_rrtmg_aero_optical_util_cmaq.o \
	module_ra_rrtmg_lwf.o  \
	module_ra_rrtmg_swf.o  \
	module_ra_rrtmg_lwk.o  \
	module_ra_rrtmg_swk.o  \
	module_ra_cam_support.o \
	module_ra_cam.o \
	module_ra_gfdleta.o \
	module_ra_flg.o \
	module_ra_hs.o  \
	module_ra_eclipse.o \
	module_ra_aerosol.o  \
	module_ra_farms.o  \
	module_sf_sfclay.o \
	module_sf_sfclayrev.o \
	module_sf_gfs.o \
	module_sf_exchcoef.o \
	module_sf_gfdl.o \
	module_sf_slab.o  \
	module_sf_noahdrv.o  \
	module_sf_noahlsm.o  \
	module_sf_clm.o  \
	module_sf_ctsm.o \
        module_sf_ssib.o  \
	module_sf_noah_seaice.o \
	module_sf_noah_seaice_drv.o \
	module_sf_noahlsm_glacial_only.o \
        module_sf_gecros.o \
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
	module_sf_mynnsfc_common.o \
	module_sf_mynnsfc_driver.o \
	module_sf_mynnsfc_land.o \
	module_sf_mynnsfc_water.o \
	module_sf_mynnsfc_ice.o \
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
	module_wind_mav.o \
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
        module_fr_fire_util.o \
        module_firebrand_spotting_mpi.o \
        module_firebrand_spotting.o

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

PHYSMMM_MODULES = \
	physics_mmm/sf_sfclayrev.o \
	physics_mmm/cu_ntiedtke.o \
	physics_mmm/module_libmassv.o \
	physics_mmm/mp_wsm6.o \
	physics_mmm/mp_wsm6_effectRad.o \
	physics_mmm/mp_radar.o \
        physics_mmm/bl_gwdo.o \
        physics_mmm/bl_ysu.o

NOAHMP_MODULES = \
	noahmp/drivers/wrf/NoahmpWRFinitMod.o \
        noahmp/drivers/wrf/NoahmpWRFmainMod.o \
	noahmp/drivers/wrf/NoahmpUrbanDriverMainMod.o \
        noahmp/drivers/wrf/NoahmpGroundwaterInitMod.o \
        noahmp/drivers/wrf/NoahmpSnowInitMod.o \
        noahmp/drivers/wrf/NoahmpInitMainMod.o \
        noahmp/drivers/wrf/NoahmpDriverMainMod.o \
        noahmp/drivers/wrf/NoahmpIOVarType.o \
        noahmp/drivers/wrf/NoahmpIOVarInitMod.o \
        noahmp/drivers/wrf/NoahmpReadTableMod.o \
        noahmp/drivers/wrf/ConfigVarOutTransferMod.o \
        noahmp/drivers/wrf/ForcingVarOutTransferMod.o \
        noahmp/drivers/wrf/EnergyVarOutTransferMod.o \
        noahmp/drivers/wrf/WaterVarOutTransferMod.o \
        noahmp/drivers/wrf/BiochemVarOutTransferMod.o \
        noahmp/drivers/wrf/ConfigVarInTransferMod.o \
        noahmp/drivers/wrf/ForcingVarInTransferMod.o \
        noahmp/drivers/wrf/EnergyVarInTransferMod.o \
        noahmp/drivers/wrf/WaterVarInTransferMod.o \
        noahmp/drivers/wrf/BiochemVarInTransferMod.o \
        noahmp/drivers/wrf/GroundWaterMmfMod.o \
        noahmp/drivers/wrf/PedoTransferSR2006Mod.o \
        noahmp/utility/Machine.o \
        noahmp/utility/CheckNanMod.o \
        noahmp/utility/PiecewiseLinearInterp1dMod.o \
        noahmp/src/ConstantDefineMod.o \
        noahmp/src/ConfigVarType.o \
        noahmp/src/ForcingVarType.o \
        noahmp/src/EnergyVarType.o \
        noahmp/src/WaterVarType.o \
        noahmp/src/BiochemVarType.o \
        noahmp/src/NoahmpVarType.o \
        noahmp/src/ConfigVarInitMod.o \
        noahmp/src/ForcingVarInitMod.o \
        noahmp/src/EnergyVarInitMod.o \
        noahmp/src/WaterVarInitMod.o \
        noahmp/src/BiochemVarInitMod.o \
        noahmp/src/CanopyHydrologyMod.o \
        noahmp/src/GroundWaterTopModelMod.o \
        noahmp/src/IrrigationFloodMod.o \
        noahmp/src/IrrigationInfilPhilipMod.o \
        noahmp/src/IrrigationMicroMod.o \
        noahmp/src/MatrixSolverTriDiagonalMod.o \
        noahmp/src/RunoffSubSurfaceDrainageMod.o \
        noahmp/src/RunoffSubSurfaceEquiWaterTableMod.o \
        noahmp/src/RunoffSubSurfaceGroundWaterMod.o \
        noahmp/src/RunoffSubSurfaceShallowMmfMod.o \
        noahmp/src/RunoffSurfaceBatsMod.o \
        noahmp/src/RunoffSurfaceDynamicVicMod.o \
        noahmp/src/RunoffSurfaceExcessDynamicVicMod.o \
        noahmp/src/RunoffSurfaceFreeDrainMod.o \
        noahmp/src/RunoffSurfaceTopModelEquiMod.o \
        noahmp/src/RunoffSurfaceTopModelGrdMod.o \
        noahmp/src/RunoffSurfaceTopModelMmfMod.o \
        noahmp/src/RunoffSurfaceVicMod.o \
        noahmp/src/RunoffSurfaceXinAnJiangMod.o \
        noahmp/src/RunoffSurfaceWetlandMod.o \
        noahmp/src/WetlandWaterZhang22Mod.o \
        noahmp/src/ShallowWaterTableMmfMod.o \
        noahmp/src/SnowfallBelowCanopyMod.o \
        noahmp/src/SnowLayerCombineMod.o \
        noahmp/src/SnowLayerDivideMod.o \
        noahmp/src/SnowLayerWaterComboMod.o \
        noahmp/src/SnowpackCompactionMod.o \
        noahmp/src/SnowpackCompactionAR24Mod.o \
        noahmp/src/SnowpackHydrologyMod.o \
        noahmp/src/SnowWaterMainMod.o \
        noahmp/src/SoilHydraulicPropertyMod.o \
        noahmp/src/SoilMoistureSolverMod.o \
        noahmp/src/SoilWaterDiffusionRichardsMod.o \
        noahmp/src/SoilWaterInfilGreenAmptMod.o \
        noahmp/src/SoilWaterInfilPhilipMod.o \
        noahmp/src/SoilWaterInfilSmithParlangeMod.o \
        noahmp/src/SoilWaterMainMod.o \
        noahmp/src/TileDrainageEquiDepthMod.o \
        noahmp/src/TileDrainageHooghoudtMod.o \
        noahmp/src/TileDrainageSimpleMod.o \
        noahmp/src/WaterMainMod.o \
        noahmp/src/WaterTableDepthSearchMod.o \
        noahmp/src/WaterTableEquilibriumMod.o \
        noahmp/src/IrrigationTriggerMod.o \
        noahmp/src/IrrigationSprinklerMod.o \
        noahmp/src/CanopyWaterInterceptMod.o \
        noahmp/src/PrecipitationHeatAdvectMod.o \
        noahmp/src/SnowThermalPropertyMod.o \
        noahmp/src/SoilThermalPropertyMod.o \
        noahmp/src/GroundThermalPropertyMod.o \
        noahmp/src/EnergyMainMod.o \
        noahmp/src/NoahmpMainMod.o \
        noahmp/src/SnowAerosolSnicarMod.o \
        noahmp/src/SnowAgingBatsMod.o \
        noahmp/src/SnowAgingSnicarMod.o \
        noahmp/src/SnowAlbedoBatsMod.o \
        noahmp/src/SnowAlbedoClassMod.o \
        noahmp/src/SnowAlbedoSnicarMod.o \
        noahmp/src/SnowFreshRadiusMod.o \
        noahmp/src/SnowInputSnicarMod.o \
        noahmp/src/SnowRadiationSnicarMod.o \
        noahmp/src/GroundAlbedoMod.o \
        noahmp/src/CanopyRadiationTwoStreamMod.o \
        noahmp/src/SurfaceAlbedoMod.o \
        noahmp/src/SurfaceRadiationMod.o \
        noahmp/src/HumiditySaturationMod.o \
        noahmp/src/ResistanceAboveCanopyChen97Mod.o \
        noahmp/src/ResistanceAboveCanopyMostMod.o \
        noahmp/src/ResistanceCanopyStomataBallBerryMod.o \
        noahmp/src/ResistanceCanopyStomataJarvisMod.o \
        noahmp/src/ResistanceLeafToGroundMod.o \
        noahmp/src/VaporPressureSaturationMod.o \
        noahmp/src/SurfaceEnergyFluxVegetatedMod.o \
        noahmp/src/ResistanceBareGroundChen97Mod.o \
        noahmp/src/ResistanceBareGroundMostMod.o \
        noahmp/src/SurfaceEnergyFluxBareGroundMod.o \
        noahmp/src/SoilSnowTemperatureMainMod.o \
        noahmp/src/SoilSnowTemperatureSolverMod.o \
        noahmp/src/SoilSnowThermalDiffusionMod.o \
        noahmp/src/SoilSnowWaterPhaseChangeMod.o \
        noahmp/src/SoilWaterSupercoolKoren99Mod.o \
        noahmp/src/SoilWaterSupercoolNiu06Mod.o \
        noahmp/src/SnowCoverGroundNiu07Mod.o \
        noahmp/src/SnowCoverGroundAR25Mod.o \
        noahmp/src/GroundRoughnessPropertyMod.o \
        noahmp/src/SurfaceEmissivityMod.o \
        noahmp/src/PsychrometricVariableMod.o \
        noahmp/src/ResistanceGroundEvaporationMod.o \
        noahmp/src/SoilWaterTranspirationMod.o \
        noahmp/src/AtmosForcingMod.o \
        noahmp/src/PhenologyMainMod.o \
        noahmp/src/BiochemCropMainMod.o \
        noahmp/src/BiochemNatureVegMainMod.o \
        noahmp/src/CarbonFluxCropMod.o \
        noahmp/src/CarbonFluxNatureVegMod.o \
        noahmp/src/CropGrowDegreeDayMod.o \
        noahmp/src/CropPhotosynthesisMod.o \
        noahmp/src/IrrigationPrepareMod.o \
        noahmp/src/BalanceErrorCheckMod.o \
        noahmp/src/GeneralInitMod.o \
        noahmp/src/BalanceErrorCheckGlacierMod.o \
        noahmp/src/EnergyMainGlacierMod.o \
        noahmp/src/GeneralInitGlacierMod.o \
        noahmp/src/GlacierIceThermalPropertyMod.o \
        noahmp/src/GlacierPhaseChangeMod.o \
        noahmp/src/GlacierTemperatureMainMod.o \
        noahmp/src/GlacierTemperatureSolverMod.o \
        noahmp/src/GlacierThermalDiffusionMod.o \
        noahmp/src/GroundAlbedoGlacierMod.o \
        noahmp/src/GroundRoughnessPropertyGlacierMod.o \
        noahmp/src/GroundThermalPropertyGlacierMod.o \
        noahmp/src/NoahmpMainGlacierMod.o \
        noahmp/src/PrecipitationHeatAdvectGlacierMod.o \
        noahmp/src/PsychrometricVariableGlacierMod.o \
        noahmp/src/ResistanceGroundEvaporationGlacierMod.o \
        noahmp/src/SnowCoverGlacierMod.o \
        noahmp/src/SnowWaterMainGlacierMod.o \
        noahmp/src/SnowpackHydrologyGlacierMod.o \
        noahmp/src/SurfaceAlbedoGlacierMod.o \
        noahmp/src/SurfaceEmissivityGlacierMod.o \
        noahmp/src/SurfaceEnergyFluxGlacierMod.o \
        noahmp/src/SurfaceRadiationGlacierMod.o \
        noahmp/src/WaterMainGlacierMod.o

OBJS    =

LIBTARGET    =  physics
TARGETDIR    =  ./

$(LIBTARGET) :
	(cd .. && ./tools/manage_externals/checkout_externals --externals ./arch/Externals.cfg)
	$(MAKE) $(J) non_nmm ; \
	$(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS)  \
		$(FIRE_MODULES) $(DIAGNOSTIC_MODULES_EM) $(PHYSMMM_MODULES) $(NOAHMP_MODULES)

include ../configure.wrf

non_nmm : $(MODULES) $(FIRE_MODULES) $(OBJS) $(DIAGNOSTIC_MODULES_EM) $(NOAHMP_MODULES)

submodules :
	@if [ \( ! -f run/NoahmpTable.TBL \) -o \
	      \( ! -f noahmp/drivers/wrf/NoahmpWRFmainMod.F90 \) -o \( ! -f noahmp/drivers/wrf/NoahmpWRFinitMod.F90 \) -o \
              \( ! -f noahmp/drivers/wrf/NoahmpIOVarInitMod.F90 \) -o \( ! -f noahmp/drivers/wrf/NoahmpIOVarType.F90 \) -o \
	      \( ! -f noahmp/drivers/wrf/GroundWaterMmfMod.F90 \) -o \( ! -f noahmp/drivers/wrf/NoahmpUrbanDriverMainMod.F90 \) -o \
	      \( ! -f noahmp/src/NoahmpMainMod.F90 \) -o \( ! -f noahmp/src/NoahmpVarType.F90 \) ] ; then \
		echo Pulling in NoahMP submodule ; \
		( cd .. ; git submodule update --init --recursive ) ; \
		( cd ../run ; ln -sf ../phys/noahmp/parameters/NoahmpTable.TBL . ) ; \
		( cd ../run ; ln -sf ../phys/noahmp/parameters/snicar_drdt_bst_fit_60_c070416.nc . ) ; \
		( cd ../run ; ln -sf ../phys/noahmp/parameters/snicar_optics_480bnd_c012422.nc . ) ; \
		( cd ../run ; ln -sf ../phys/noahmp/parameters/snicar_optics_5bnd_c013122.nc . ) ; \
	else \
		echo No action required for NoahMP submodule ; \
	fi
	@if [ \( ! -f phys/physics_mmm/README.md \) ] ; then \
		echo Pulling in MMM-physics submodule ; \
		( cd .. ; git submodule update --init --recursive ) ; \
	else \
		echo No action required for MMM-physics submodule ; \
	fi
	@if [ \( ! -f module_bl_mynnedmf.F \) -o \( ! -f module_bl_mynnedmf_common.F \) -o \
	      \( ! -f module_bl_mynnedmf_driver.F \) ] ; then \
	      	echo Pulling in MYNN-EDMF submodule ; \
		( cd .. ; git submodule update --init --recursive ) ; \
		ln -sf MYNN-EDMF/module_bl_mynnedmf.F90 module_bl_mynnedmf.F ; \
		ln -sf MYNN-EDMF/WRF/module_bl_mynnedmf_common.F90 module_bl_mynnedmf_common.F ; \
		ln -sf MYNN-EDMF/WRF/module_bl_mynnedmf_driver.F90 module_bl_mynnedmf_driver.F ; \
	else \
		echo No action required for MYNN-EDMF submodule ; \
	fi
	@if [ \( ! -f module_mp_tempo_driver.F90 \) -o \( ! -f module_mp_tempo_main.F90 \) -o \
	      \( ! -f module_mp_tempo_cfgs.F90 \) -o \( ! -f module_mp_tempo_aerosols.F90 \) -o \
	      \( ! -f module_mp_tempo_ml.F90 \) -o \( ! -f modulemp_tempo_diags.F90 \) -o \
              \( ! -f module_mp_tempo_utils.F90 \) -o \( ! -f module_mp_tempo_params.F90 \) ] ; then \
                echo Pulling in TEMPO submodule ; \
		( cd .. ; git submodule update --init --recursive ) ; \
		ln -sf TEMPO/src/module_mp_tempo_cfgs.F90 module_mp_tempo_cfgs.F90 ; \
		ln -sf TEMPO/src/module_mp_tempo_params.F90 module_mp_tempo_params.F90 ; \
		ln -sf TEMPO/src/module_mp_tempo_utils.F90 module_mp_tempo_utils.F90 ; \
		ln -sf TEMPO/src/module_mp_tempo_diags.F90 module_mp_tempo_diags.F90 ; \
		ln -sf TEMPO/src/module_mp_tempo_ml.F90 module_mp_tempo_ml.F90 ; \
		ln -sf TEMPO/src/module_mp_tempo_aerosols.F90 module_mp_tempo_aerosols.F90 ; \
		ln -sf TEMPO/src/module_mp_tempo_main.F90 module_mp_tempo_main.F90 ; \
		ln -sf TEMPO/src/module_mp_tempo_driver.F90 module_mp_tempo_driver.F90 ; \
	else \
		echo No action required for TEMPO submodule ; \
	fi
	
	@if [ \( ! -f module_sf_mynnsfc_driver.F \) -o \( ! -f module_sf_mynnsfc_common.F \) -o \
	      \( ! -f module_sf_mynnsfc_land.F \) -o \( ! -f module_sf_mynnsfc_water.F \) -o \
	      \( ! -f module_sf_mynnsfc_ice.F \) ] ; then \
		echo Pulling in MYNN-SFC submodule ; \
		( cd .. ; git submodule update --init --recursive ) ; \
		ln -sf MYNN-SFC/WRF/module_sf_mynnsfc_driver.F90 module_sf_mynnsfc_driver.F ; \
		ln -sf MYNN-SFC/WRF/module_sf_mynnsfc_common.F90 module_sf_mynnsfc_common.F ; \
		ln -sf MYNN-SFC/module_sf_mynnsfc_land.F90 module_sf_mynnsfc_land.F ; \
		ln -sf MYNN-SFC/module_sf_mynnsfc_water.F90 module_sf_mynnsfc_water.F ; \
		ln -sf MYNN-SFC/module_sf_mynnsfc_ice.F90 module_sf_mynnsfc_ice.F ; \
	else \
		echo No action required for MYNN-SFC submodule ; \
	fi
clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
# DO NOT DELETE

include ../main/depend.common
