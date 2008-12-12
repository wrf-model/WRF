#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES =                 		\
        module_ADVECTION.o         	\
        module_BC_NMM.o         	\
        module_BNDRY_COND.o         	\
        module_NEST_UTIL.o              \
        module_CTLBLK.o         	\
        module_DIFFUSION_NMM.o         	\
        module_IGWAVE_ADJUST.o         	\
        module_NONHY_DYNAM.o         	\
        module_PHYSICS_CALLS.o       	\
        module_GWD.o                    \
        module_MPPINIT.o        	\
        module_TIMERS.o                 \
        module_si_io_nmm.o              \
        module_initialize_real.o        \
        module_PRECIP_ADJUST.o          \
	$(CASE_MODULE)

# moved into share/Makefile
#        module_MPP.o              	\

#        module_INDX.o                   \

# possible CASE_MODULE settings
#	module_initialize_nmm.o        

OBJS    = 		          	\
	init_modules_nmm.o  		\
	start_domain_nmm.o  		\
	shift_domain_nmm.o  		\
	solve_nmm.o         		\
	RDTEMP.o    			\
	BUCKETS.o 			\
        DSTRB.o                         \
	CLTEND.o                        \
        NMM_NEST_UTILS1.o


LIBTARGET    =  dyn_nmm
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS)
		$(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS)

include ../configure.wrf

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

solve_nmm.o:   module_BC_NMM.o \
               module_IGWAVE_ADJUST.o module_ADVECTION.o  \
               module_NONHY_DYNAM.o module_DIFFUSION_NMM.o    \
               module_BNDRY_COND.o module_PHYSICS_CALLS.o \
               module_CTLBLK.o

module_ADVECTION.o: ../share/module_MPP.o module_INDX.o

module_MPPINIT.o: ../share/module_MPP.o 

module_DIFFUSION_NMM.o: ../share/module_MPP.o module_INDX.o

module_IGWAVE_ADJUST.o: ../share/module_MPP.o module_INDX.o module_TIMERS.o

module_PHYSICS_CALLS.o: \
		../frame/module_domain.o		\
		../frame/module_dm.o		\
		../frame/module_configure.o		\
		../frame/module_tiles.o		\
		../frame/module_state_description.o		\
		../share/module_model_constants.o		\
		../phys/module_ra_gfdleta.o  \
		../phys/module_radiation_driver.o  \
		../phys/module_sf_myjsfc.o  \
		../phys/module_surface_driver.o  \
		../phys/module_pbl_driver.o  \
                ./module_GWD.o  \
		../phys/module_cu_bmj.o  \
		../phys/module_cumulus_driver.o  \
		../phys/module_mp_etanew.o  \
		../phys/module_microphysics_driver.o

module_GWD.o: ../frame/module_dm.o

module_initialize_real.o: ../share/module_model_constants.o

# DO NOT DELETE
