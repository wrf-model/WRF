#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES =                 		\
        module_ADVECTION.o         	\
        module_BC_NMM.o         	\
        module_BNDRY_COND.o         	\
        module_CLDWTR.o         	\
        module_CTLBLK.o         	\
        module_DIFFUSION_NMM.o         	\
        module_IGWAVE_ADJUST.o         	\
        module_NONHY_DYNAM.o         	\
        module_PHYS.o         		\
        module_MPPINIT.o        	\
        module_TIMERS.o                 \
        module_ZEROX.o                  \
	$(CASE_MODULE)

# moved into share/Makefile
#        module_MPP.o              	\

#        module_INDX.o                   \

# possible CASE_MODULE settings
#	module_initialize_nmm.o        

OBJS    = 		          	\
	read_nmm.o			\
	init_modules_nmm.o  		\
	start_domain_nmm.o  		\
	solve_nmm.o         		\
	RADIATION.o 			\
	RDTEMP.o    			\
	TURBL.o 			\
	CUCNVC.o			\
	GSMDRIVE.o			\
	CLTEND.o


LIBTARGET    =  dyn_nmm
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS)
		$(AR) ../main/libwrflib.a $(MODULES) $(OBJS)

include ../configure.wrf

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

solve_nmm.o:   module_BC_NMM.o module_PHYS.o \
               module_IGWAVE_ADJUST.o module_ADVECTION.o  \
               module_NONHY_DYNAM.o module_DIFFUSION_NMM.o    \
               module_BNDRY_COND.o \
               module_CTLBLK.o TURBL.o

module_ADVECTION.o: ../share/module_MPP.o module_INDX.o

module_MPPINIT.o: ../share/module_MPP.o 

module_DIFFUSION_NMM.o: ../share/module_MPP.o module_INDX.o

module_IGWAVE_ADJUST.o: ../share/module_MPP.o module_INDX.o module_ZEROX.o module_TIMERS.o
