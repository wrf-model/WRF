#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f



MODULES =       module_driver_constants.o  \
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
                module_dm.o                \
                module_quilt_outbuf_ops.o  \
                module_io_quilt.o

OBJS    =       wrf_num_bytes_between.o    \
                wrf_shutdown.o             \
                collect_on_comm.o

#compile as a .o but do not link into the main library
SPECIAL =       internal_header_util.o pack_utils.o
   

include ../configure.wrf

LIBTARGET    =  framework
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS) $(SPECIAL)
		$(AR) ../main/libwrflib.a $(MODULES) $(OBJS)
		$(RANLIB) ../main/libwrflib.a


wrf_num_bytes_between.o :
	$(CC) -c $(CFLAGS) wrf_num_bytes_between.c

clean:
	@ echo 'use the clean script'

#module_state_description.F : ../Registry/$(REGISTRY)
#		( cd .. ; ./clean ; $(PERL) -Itools tools/use_registry $(ARCHFLAGS) Registry/$(REGISTRY) )

module_state_description.F : ../Registry/$(REGISTRY)
		( cd .. ; tools/registry $(ARCHFLAGS) Registry/$(REGISTRY) )

md_calls.inc : md_calls.m4
	$(M4) md_calls.m4 > md_calls.inc


# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)


module_configure.o: \
		module_state_description.o \
                module_wrf_error.o \
		module_driver_constants.o 

module_dm.o: module_machine.o module_state_description.o module_wrf_error.o \
		module_domain.o \
		module_driver_constants.o \
		module_timing.o \
		module_configure.o 

module_dm_stubs.F: module_domain.o

module_domain.o: module_driver_constants.o \
		module_configure.o \
		module_machine.o  \
		module_state_description.o \
                module_wrf_error.o \
		../external/esmf_time_f90/ESMF_Mod.o

module_driver_constants.o: \
		module_state_description.o \
                module_wrf_error.o

module_integrate.o: module_domain.o \
		module_timing.o \
		module_driver_constants.o \
		module_state_description.o \
		module_nesting.o \
		module_configure.o \
		../external/esmf_time_f90/ESMF_Mod.o

module_io.o : md_calls.inc \
		module_state_description.o \
		module_driver_constants.o 

module_io_quilt.o: module_state_description.o \
		module_quilt_outbuf_ops.o 

module_machine.o: module_driver_constants.o

module_nesting.o: module_machine.o \
		module_driver_constants.o \
		../external/esmf_time_f90/ESMF_Mod.o \
		module_domain.o 

module_quilt_outbuf_ops.o: module_state_description.o

module_tiles.o: module_domain.o \
		module_driver_constants.o \
		module_machine.o  \
                module_wrf_error.o
 
module_timing.o: \
		module_state_description.o \
                module_wrf_error.o

module_wrf_error.o: wrf_shutdown.o 

# DO NOT DELETE
