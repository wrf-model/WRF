include ../../configure.wrf

all_wrfvar: wrfvar be da_utils

gen_be: be

setup : depend

# preserve this makefile from overwriting
links : 
	@ cp makefile makefile.old
	@ $(LN) ../../dyn_em/init_modules_em.F .
	@ $(LN) ../../dyn_em/module_em.F .
	@ $(LN) ../gen_be/* .
	@ $(LN) ../../share/* .
	@ $(LN) ../../frame/* .
	@ if test -e module_dm_stubs.F; then rm -rf module_dm_stubs.F; fi
	@ $(LN) ../da/da*/* .
	@ $(LN) ../external/blas/* .
	@ $(LN) ../external/lapack/* .
	@ $(LN) ../../arch/*.sed .
	@ $(LN) ../../external/esmf_time_f90/* .
#	@ $(LN) ../convertor/da_module_convert_tool/* .
#	@ $(LN) ../convertor/kma_wave2grid/* .
#	@ $(LN) ../convertor/wave2grid_kma/* .
#	@ $(LN) ../convertor/kma_netcdf/* .
#	@ $(LN) ../convertor/decode_l2_airs/* .
	@ mv makefile.old makefile
	@ touch links

WRFVAR_LIBS = libwrfvar.a 

WRFVAR_LIB = -L. -lwrfvar $(RTTOV_LIB) $(CRTM_LIB)

FPPFLAGS   = -I$(WRF_SRC_ROOT_DIR)/inc

depend : links module_state_description.F md_calls.inc \
                      da_generic_boilerplate.inc
#	(cd $(WRF_SRC_ROOT_DIR)/var/da/makedepf90-2.8.8; ./setup_makedepf90)
#	$(WRF_SRC_ROOT_DIR)/var/da/makedepf90-2.8.8/makedepf90 -free $(ARCHFLAGS) -DNEW_BDYS *.F *.f90 *.F90 2>/dev/null > depend.txt
	touch depend
	
include depend.txt

# Manual rules specified first take precedence

include da.make
include gen_be.make
include convertor.make
include decode_l2_airs.make

libwrfvar.a : $(WRFVAR_OBJS)
	rm -f libwrfvar.a
	$(AR) $(ARFLAGS) libwrfvar.a $(WRFVAR_OBJS)
	$(RANLIB) $(RLFLAGS) libwrfvar.a

.f90.o:
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) $*.b  > $*.f
	$(RM) $*.b
	@ if echo $(ARCHFLAGS) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.f90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi
	if $(FGREP) '!$$OMP' $*.f ; then \
          if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITH OMP ; fi ; \
	  $(FC) -o $@ -c $(FCFLAGS) $(OMP) $(PROMOTION) $*.f ; \
        else \
          if [ -n "$(OMP)" ] ; then echo COMPILING $*.f90 WITHOUT OMP ; fi ; \
	  $(FC) -o $@ -c $(FCFLAGS) $(PROMOTION) $*.f ; \
        fi

