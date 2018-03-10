#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f



MODULES =       module_driver_constants.o  \
                module_domain_type.o       \
                module_streams.o           \
                module_domain.o            \
		$(LLIST) \
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
                module_comm_dm_0.o         \
                module_comm_dm_1.o         \
                module_comm_dm_2.o         \
                module_comm_dm_3.o         \
                module_comm_dm_4.o         \
                module_comm_nesting_dm.o   \
                module_dm.o                \
                module_quilt_outbuf_ops.o  \
                module_io_quilt.o          \
		module_intermediate_nmm.o  \
		module_cpl.o               \
		module_cpl_oasis3.o        \
		module_clear_halos.o
ALOBJS =\
                module_alloc_space_0.o       \
                module_alloc_space_1.o       \
                module_alloc_space_2.o       \
                module_alloc_space_3.o       \
                module_alloc_space_4.o       \
                module_alloc_space_5.o       \
                module_alloc_space_6.o       \
                module_alloc_space_7.o       \
                module_alloc_space_8.o       \
                module_alloc_space_9.o

NLOBJS =\
                nl_get_0_routines.o        \
                nl_get_1_routines.o        \
                nl_get_2_routines.o        \
                nl_get_3_routines.o        \
                nl_get_4_routines.o        \
                nl_get_5_routines.o        \
                nl_get_6_routines.o        \
                nl_get_7_routines.o        \
                nl_set_0_routines.o        \
                nl_set_1_routines.o        \
                nl_set_2_routines.o        \
                nl_set_3_routines.o        \
                nl_set_4_routines.o        \
                nl_set_5_routines.o        \
                nl_set_6_routines.o        \
                nl_set_7_routines.o 

OBJS    =       \
                wrf_num_bytes_between.o    \
                wrf_shutdown.o             \
                wrf_debug.o                \
                libmassv.o                 \
                collect_on_comm.o          \
                hires_timer.o              \
		clog.o

#compile as a .o but do not link into the main library
SPECIAL =       module_internal_header_util.o pack_utils.o
   

include ../configure.wrf

LIBTARGET    =  framework
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS) $(SPECIAL) $(NLOBJS) $(ALOBJS)
		$(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS) $(NLOBJS) $(ALOBJS)
		$(RANLIB) ../main/$(LIBWRFLIB)

nl_set_0_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=0 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx0.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx0.f90
	$(RM) xx0.f90

nl_set_1_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=1 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx1.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx1.f90
	$(RM) xx1.f90

nl_set_2_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=2 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx2.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx2.f90
	$(RM) xx2.f90

nl_set_3_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=3 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx3.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx3.f90
	$(RM) xx3.f90

nl_set_4_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=4 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx4.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx4.f90
	$(RM) xx4.f90

nl_set_5_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=5 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx5.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx5.f90
	$(RM) xx5.f90

nl_set_6_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=6 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx6.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx6.f90
	$(RM) xx6.f90

nl_set_7_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=7 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx7.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx7.f90
	$(RM) xx7.f90

nl_get_0_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=0 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy0.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy0.f90
	$(RM) yy0.f90

nl_get_1_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=1 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy1.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy1.f90
	$(RM) yy1.f90

nl_get_2_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=2 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy2.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy2.f90
	$(RM) yy2.f90

nl_get_3_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=3 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy3.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy3.f90
	$(RM) yy3.f90

nl_get_4_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=4 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy4.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy4.f90
	$(RM) yy4.f90

nl_get_5_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=5 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy5.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy5.f90
	$(RM) yy5.f90

nl_get_6_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=6 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy6.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy6.f90
	$(RM) yy6.f90

nl_get_7_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=7 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy7.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy7.f90
	$(RM) yy7.f90

wrf_num_bytes_between.o :
	$(CC) -c $(CFLAGS) wrf_num_bytes_between.c

pack_utils.o :
	$(CC) -c $(CFLAGS) -DIWORDSIZE=$(IWORDSIZE) pack_utils.c

module_internal_header_util.o :
	$(CPP) $(CPPFLAGS) -I../inc module_internal_header_util.F > module_internal_header_util.f90
	$(SFC) -c $(PROMOTION) $(FCSUFFIX) $(FCDEBUG) $(FCBASEOPTS) module_internal_header_util.f90

clean:
	@ echo 'use the clean script'

module_state_description.F : ../Registry/$(REGISTRY)
		  ( cd .. ; tools/registry $(ARCHFLAGS) $(ENVCOMPDEFS) -DNEW_BDYS Registry/$(REGISTRY) ) ; 

module_io_quilt.o : module_io_quilt.F module_io_quilt_new.F module_io_quilt_old.F

md_calls.inc : md_calls.m4
	if [ "$(M4)" = NA ] ; then \
	  /bin/cp ../arch/md_calls.inc . ; \
	else \
	  $(M4) md_calls.m4 > md_calls.inc ; \
	fi

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include ../main/depend.common

# DO NOT DELETE
