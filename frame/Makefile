#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f



MODULES =       module_driver_constants.o  \
                module_domain_type.o       \
                module_alloc_space.o       \
                module_domain.o            \
                module_integrate.o         \
                module_timing.o            \
                module_configure.o         \
                module_tiles.o             \
                module_machine.o           \
                module_nesting.o           \
                module_wrf_error.o         \
                module_state_description.o \
                module_sm.o                \
                module_io.o                \
                module_comm_dm.o           \
                module_dm.o                \
                module_quilt_outbuf_ops.o  \
                module_io_quilt.o

OBJS    =       wrf_num_bytes_between.o    \
                wrf_shutdown.o             \
                wrf_debug.o                \
                libmassv.o                 \
                nl_get_0_routines.o        \
                nl_get_1_routines.o        \
                nl_set_0_routines.o        \
                nl_set_1_routines.o        \
                collect_on_comm.o

#compile as a .o but do not link into the main library
SPECIAL =       module_internal_header_util.o pack_utils.o
   

include ../configure.wrf

LIBTARGET    =  framework
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS) $(SPECIAL)
		$(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS)
		$(RANLIB) ../main/$(LIBWRFLIB)


wrf_num_bytes_between.o :
	$(CC) -c $(CFLAGS) wrf_num_bytes_between.c

pack_utils.o :
	$(CC) -c $(CFLAGS) pack_utils.c

module_internal_header_util.o :
	$(CPP) $(CPPFLAGS) -I../inc module_internal_header_util.F > module_internal_header_util.f90
	$(SFC) -c $(PROMOTION) $(FCSUFFIX) $(FCDEBUG) $(FCBASEOPTS) module_internal_header_util.f90

clean:
	@ echo 'use the clean script'

module_state_description.F : ../Registry/$(REGISTRY)
		  ( cd .. ; tools/registry $(ARCHFLAGS) $(ENVCOMPDEFS) -DNEW_BDYS Registry/$(REGISTRY) ) ; 

md_calls.inc : md_calls.m4
	if [ "$(M4)" = NA ] ; then \
	  /bin/cp ../arch/md_calls.inc . ; \
	else \
	  $(M4) md_calls.m4 > md_calls.inc ; \
	fi

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)


module_configure.o: \
		module_state_description.o \
                module_wrf_error.o \
		module_driver_constants.o 

module_dm.o: module_machine.o module_state_description.o module_wrf_error.o \
		module_domain.o \
		module_driver_constants.o \
		module_timing.o \
		module_configure.o module_comm_dm.o

module_comm_dm.o: \
		module_domain.o \
		module_configure.o

module_dm_stubs.F: module_domain.o

module_domain.o: module_domain_type.o module_alloc_space.o module_driver_constants.o \
		module_configure.o \
		module_machine.o  \
		module_state_description.o \
                module_wrf_error.o \
		$(ESMF_MOD_DEPENDENCE)

module_domain_type.o : module_driver_constants.o $(ESMF_MOD_DEPENDENCE)

module_alloc_space.o : module_domain_type.o module_configure.o

module_driver_constants.o: \
		module_state_description.o \
                module_wrf_error.o

module_integrate.o: module_domain.o \
		module_timing.o \
		module_driver_constants.o \
		module_state_description.o \
		module_nesting.o \
		module_configure.o \
		$(ESMF_MOD_DEPENDENCE)

module_io.o : md_calls.inc \
		module_state_description.o \
		module_configure.o  \
		module_driver_constants.o 

module_io_quilt.o: module_state_description.o \
		module_internal_header_util.o \
		module_quilt_outbuf_ops.o 

module_machine.o: module_driver_constants.o

module_nesting.o: module_machine.o \
		module_driver_constants.o \
		module_configure.o \
		$(ESMF_MOD_DEPENDENCE) \
		module_domain.o 

module_quilt_outbuf_ops.o: module_state_description.o

module_tiles.o: module_domain.o \
		module_driver_constants.o \
		module_machine.o  \
		module_configure.o  \
                module_wrf_error.o
 
module_timing.o: \
		module_state_description.o \
                module_wrf_error.o

module_wrf_error.o: \
		wrf_shutdown.o \
		$(ESMF_MOD_DEPENDENCE)

wrf_debug.o: \
		module_wrf_error.o

# DO NOT DELETE
