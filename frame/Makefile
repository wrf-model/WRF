#
include ../configure.wrf

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
		module_cpl.o               \
		module_cpl_oasis3.o        \
		module_clear_halos.o
ALOBJS =\
                ../inc/allocs_0.o                  \
                ../inc/allocs_1.o                  \
                ../inc/allocs_2.o                  \
                ../inc/allocs_3.o                  \
                ../inc/allocs_4.o                  \
                ../inc/allocs_5.o                  \
                ../inc/allocs_6.o                  \
                ../inc/allocs_7.o                  \
                ../inc/allocs_8.o                  \
                ../inc/allocs_9.o                  \
                ../inc/allocs_10.o                  \
                ../inc/allocs_11.o                  \
                ../inc/allocs_12.o                  \
                ../inc/allocs_13.o                  \
                ../inc/allocs_14.o                  \
                ../inc/allocs_15.o                  \
                ../inc/allocs_16.o                  \
                ../inc/allocs_17.o                  \
                ../inc/allocs_18.o                  \
                ../inc/allocs_19.o                  \
                ../inc/allocs_20.o                  \
                ../inc/allocs_21.o                  \
                ../inc/allocs_22.o                  \
                ../inc/allocs_23.o                  \
                ../inc/allocs_24.o                  \
                ../inc/allocs_25.o                  \
                ../inc/allocs_26.o                  \
                ../inc/allocs_27.o                  \
                ../inc/allocs_28.o                  \
                ../inc/allocs_29.o                  \
                ../inc/allocs_30.o                  \
                ../inc/allocs_31.o

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

DEALOBJS =\
                ../inc/deallocs_0.o        \
                ../inc/deallocs_1.o        \
                ../inc/deallocs_2.o        \
                ../inc/deallocs_3.o        \
                ../inc/deallocs_4.o        \
                ../inc/deallocs_5.o        \
                ../inc/deallocs_6.o        \
                ../inc/deallocs_7.o        \
                ../inc/deallocs_8.o        \
                ../inc/deallocs_9.o        \
                ../inc/deallocs_10.o       \
                ../inc/deallocs_11.o 

OBJS    =       \
                wrf_num_bytes_between.o    \
                wrf_shutdown.o             \
                wrf_debug.o                \
                libmassv.o                 \
                collect_on_comm.o          \
                hires_timer.o              \
		clog.o

ifeq ($(DMPARALLEL),1)
	RSL_OBJS= \
            ../external/RSL_LITE/feedback_domain_em_part1.o    \
            ../external/RSL_LITE/feedback_domain_em_part2.o    \
            ../external/RSL_LITE/force_domain_em_part2.o       \
            ../external/RSL_LITE/interp_domain_em_part1.o      \
            ../external/RSL_LITE/interp_domain_em_part2.o      \
            ../external/RSL_LITE/interp_domain_em_part3.o      \
            ../external/RSL_LITE/interp_domain_em_small.o
endif

#compile as a .o but do not link into the main library
SPECIAL =       module_internal_header_util.o pack_utils.o


LIBTARGET    =  framework
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS) $(SPECIAL) $(NLOBJS) $(ALOBJS) $(DEALOBJS) $(RSL_OBJS)
		$(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS) $(NLOBJS) $(ALOBJS) $(DEALOBJS) $(RSL_OBJS)
		$(RANLIB) ../main/$(LIBWRFLIB)

nl_set_0_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=0 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx0.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx0.f90

nl_set_1_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=1 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx1.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx1.f90

nl_set_2_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=2 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx2.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx2.f90

nl_set_3_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=3 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx3.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx3.f90

nl_set_4_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=4 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx4.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx4.f90

nl_set_5_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=5 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx5.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx5.f90

nl_set_6_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=6 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx6.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx6.f90

nl_set_7_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=7 -I../inc -DNL_set_ROUTINES nl_access_routines.F > xx7.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) xx7.f90

nl_get_0_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=0 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy0.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy0.f90

nl_get_1_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=1 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy1.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy1.f90

nl_get_2_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=2 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy2.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy2.f90

nl_get_3_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=3 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy3.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy3.f90

nl_get_4_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=4 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy4.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy4.f90

nl_get_5_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=5 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy5.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy5.f90

nl_get_6_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=6 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy6.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy6.f90

nl_get_7_routines.o : nl_access_routines.F module_configure.o
	$(CPP) -DNNN=7 -I../inc -DNL_get_ROUTINES nl_access_routines.F > yy7.f90
	$(FC) -o $@ -c $(PROMOTION) $(FCNOOPT) $(FCBASEOPTS_NO_G) $(MODULE_DIRS) $(FCSUFFIX) yy7.f90

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
		  ( cd .. ; tools/registry $(ARCHFLAGS) $(ENVCOMPDEFS) -DNEW_BDYS Registry/$(REGISTRY) ; true ) ; 

module_io_quilt.o : module_io_quilt.F module_io_quilt_old.F

md_calls.inc : md_calls.m4
	if [ "$(M4)" = NA ] ; then \
	  /bin/cp ../arch/md_calls.inc . ; \
	else \
	  $(M4) md_calls.m4 > md_calls.inc ; \
	fi

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include ../main/depend.common

# DO NOT DELETE
