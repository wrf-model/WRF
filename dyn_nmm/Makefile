#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES =                 		\
        module_TIMERS.o                 \
        module_GWD.o                    \
        module_ADVECTION.o         	\
        module_BC_NMM.o         	\
        module_BNDRY_COND.o         	\
        module_NEST_UTIL.o              \
        module_CTLBLK.o         	\
        module_DIFFUSION_NMM.o         	\
        module_NONHY_DYNAM.o         	\
	module_STATS_FOR_MOVE.o		\
        module_MPPINIT.o        	\
        module_si_io_nmm.o              \
        module_initialize_real.o        \
        module_PRECIP_ADJUST.o          \
	module_HIFREQ.o                 \
	module_TERRAIN.o                \
	module_SMOOTH_TERRAIN.o         \
        module_PHYSICS_CALLS.o          \
        module_IGWAVE_ADJUST.o          \
	module_membrane_mslp.o		\
	module_swath.o			\
	module_tracker.o		\
	module_tornado_genesis.o	\
	module_relax.o			\
	nmm_get_cpu.o                   \
	$(CASE_MODULE)

# moved into share/Makefile
#        module_MPP.o              	\
#        module_INDX.o                   \

# possible CASE_MODULE settings
#	module_initialize_tropical_cyclone.o        

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

include depend.dyn_nmm

# DO NOT DELETE
