# frame

FRAME_OBJS = module_internal_header_util.o \
	module_driver_constants.o \
	module_domain.o \
	module_integrate.o \
	module_timing.o \
	module_configure.o \
	module_tiles.o \
	module_machine.o \
	module_nesting.o \
	module_wrf_error.o \
	module_state_description.o \
	module_sm.o \
	module_io.o \
	module_dm.o module_comm_dm.o \
	module_quilt_outbuf_ops.o \
	module_io_quilt.o \
	wrf_num_bytes_between.o \
	wrf_shutdown.o \
	wrf_debug.o \
	libmassv.o \
	collect_on_comm.o \
	pack_utils.o
   
wrf_num_bytes_between.o :
	$(CC) -c $(CCFLAGS) wrf_num_bytes_between.c

module_state_description.F : registry ../Registry/$(REGISTRY)
	./registry $(REGFLAGS) ../Registry/$(REGISTRY)
	$(LN) frame/module_state_description.F .
	( cd $(INC); $(LN) ../build/inc/namelist_script.inc . )

md_calls.inc : md_calls.m4
	$(M4) md_calls.m4 > md_calls.inc


