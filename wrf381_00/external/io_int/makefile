#makefile to build io_int that does binary i/o

OBJS    = io_int.o io_int_idx.o module_io_int_idx.o module_io_int_read.o
LIB     = libwrfio_int.a
WRF_MOD = ../../frame/pack_utils.o \
	  ../../frame/module_internal_header_util.o \
	  ../../frame/module_driver_constants.o \
	  ../../frame/module_machine.o \
	  ../../frame/module_wrf_error.o \
	  ../../frame/wrf_debug.o

CPP1    = $(CPP) $(TRADFLAG) $(ARCHFLAGS)
M4      = m4 -Uinclude -Uindex -Ulen

.SUFFIXES: .F90 .F .f .f90 .c .h .o .code

all : $(LIB) diffwrf test_io_idx

$(LIB): $(OBJS)
	$(RM) $(LIB)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(LIB)

io_int.f: io_int.F90 module_internal_header_util.o
	$(CPP1)  -I../../inc -I../ioapi_share $*.F90 | $(M4) - > $@
	@if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
           echo COMPILING $*.F90 for 4DVAR ; \
           $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $@ > io_int.tmp; \
           mv io_int.tmp $@ ; \
	fi

io_int.o: io_int.f ../../inc/intio_tags.h
	$(FC) $(FCFLAGS) -I../../inc -I../ioapi_share -o $@ -c $*.f

io_int_idx_tags.h: ../../inc/intio_tags.h
	awk '{print "#define", toupper($$4), $$6}' < ../../inc/intio_tags.h > $@

io_int_idx.o: io_int_idx.c io_int_idx.h io_int_idx_tags.h
	$(CC) -o $@ -c $(CFLAGS_LOCAL) $*.c

module_io_int_idx.o: module_io_int_idx.f
	$(FC) $(FCFLAGS) -o $@ -c $*.f

module_io_int_idx.f: module_io_int_idx.F90
	$(CPP1) $*.F90 > $@

module_io_int_read.o: module_io_int_read.f module_io_int_idx.o
	$(FC) $(FCFLAGS) -I../../frame -o $@ -c $*.f

module_io_int_read.f: module_io_int_read.F90
	$(CPP1) $(ARCHFLAGS) $*.F90 > $@

# this is just so we can get the .mod file, it will be recompiled in
# frame again as part of WRF frmwk
module_internal_header_util.f: ../../frame/module_internal_header_util.F
	-$(RM) $@
	cp ../../frame/module_internal_header_util.F mod_int_hdr_util.b
	$(CPP1) -I../../inc -I../ioapi_share mod_int_hdr_util.b > $@
	$(RM) mod_int_hdr_util.b

module_internal_header_util.o: module_internal_header_util.f
	$(FC) $(FCFLAGS) -o $@ -c $*.f

diffwrf.f: diffwrf.F90 io_int.o
	if [ `echo "$(FC)" | awk '{print $$1}'` = "gfortran" ] ; then \
		echo removing external declaration of iargc for gfortran ; \
		$(CPP1)  -I../ioapi_share $*.F90 | \
		sed '/integer *, *external.*iargc/d' > $@  ; \
	else \
		$(CPP1)  -I../ioapi_share $*.F90 > $@  ; \
	fi ; \
	$(FC) -c $(FCFLAGS) -I../ioapi_share $@ ; \


diffwrf: diffwrf.f $(WRF_MOD) $(ESMF_MOD_DEPENDENCE) $(LIB)
	if [ -f ../../frame/pack_utils.o -a -f ../../frame/clog.o ] ; then \
	  $(FC) $(FCFLAGS) $(LDFLAGS) -I../ioapi_share -o diffwrf $@.f \
		$(WRF_MOD) $(ESMF_IO_LIB_EXT) $(LIB) ; \
	fi

$(WRF_MOD) $(ESMF_MOD_DEPENDENCE):
	@echo "Diffwrf io_int will be built later on in this compile. No need to rerun compile. "

test_io_idx: test_io_idx.f $(LIB)
	$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ $@.f -L. -lwrfio_int

test_io_idx.f: test_io_idx.F90
	$(CPP1) $*.F90 > $@

test_io_mpi: test_io_mpi.f90 $(LIB)
	$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ $@.f90 -L. -lwrfio_int

superclean:
	-$(RM) *.f *.o *.obj *.i *.mod $(LIB) diffwrf io_int_idx_tags.h \
		test_io_idx test_io_mpi io_int_idx_tags.h
