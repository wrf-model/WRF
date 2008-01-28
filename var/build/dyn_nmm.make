# dyn_nmm

DYN_nmm_OBJS = \
	module_ADVECTION.o \
	module_BC_NMM.o \
	module_BNDRY_COND.o \
	module_CTLBLK.o \
	module_DIFFUSION_NMM.o \
	module_IGWAVE_ADJUST.o \
	module_NONHY_DYNAM.o \
	module_PHYSICS_CALLS.o \
	module_PRECIP_ADJUST.o \
	module_NEST_UTIL.o \
	module_MPP.o \
	module_MPPINIT.o \
	module_TIMERS.o \
	module_ZEROX.o \
	module_si_io_nmm.o \
	init_modules_nmm.o \
	start_domain_nmm.o \
	solve_nmm.o \
	DSTRB.o \
	RDTEMP.o \
	BUCKETS.o \
	CLTEND.o \
	init_modules_nmm.o \
	solve_nmm.o \
	start_domain_nmm.o

solve_nmm.o : module_BC_NMM.o \
               module_IGWAVE_ADJUST.o module_ADVECTION.o  \
               module_NONHY_DYNAM.o module_DIFFUSION_NMM.o    \
               module_BNDRY_COND.o module_PHYSICS_CALLS.o \
               module_CTLBLK.o

module_ADVECTION.o: module_MPP.o module_INDX.o

module_MPPINIT.o: module_MPP.o 

module_DIFFUSION_NMM.o: module_MPP.o module_INDX.o

module_IGWAVE_ADJUST.o: module_MPP.o module_INDX.o module_ZEROX.o module_TIMERS.o

module_PHYSICS_CALLS.o: \
		module_domain.o                \
		module_dm.o            \
		module_configure.o             \
		module_tiles.o         \
		module_state_description.o             \
		module_model_constants.o               \
		module_ra_gfdleta.o  \
		module_radiation_driver.o  \
		module_sf_myjsfc.o  \
		module_surface_driver.o  \
		module_pbl_driver.o  \
		module_cu_bmj.o  \
		module_cumulus_driver.o  \
		module_mp_etanew.o  \
		module_microphysics_driver.o
