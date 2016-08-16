#	Top level Makefile for wrf system

LN      =       ln -s
MAKE    =       make -i -r
MV	=	/bin/mv
RM      =       /bin/rm -f
CHEM_FILES =	../chem/module_aerosols_sorgam.o \
		../chem/module_gocart_aerosols.o \
		../chem/module_mosaic_driver.o \
		../chem/module_input_tracer.o \
		../chem/module_aerosols_soa_vbs.o
CHEM_FILES2 =	../chem/module_data_mosaic_asect.o

deflt :
		@ echo Please compile the code using ./compile

include ./configure.wrf

EM_MODULE_DIR = -I../dyn_em
EM_MODULES =  $(EM_MODULE_DIR)

DA_WRFVAR_MODULES = $(INCLUDE_MODULES)
DA_WRFVAR_MODULES_2 = $(INC_MOD_WRFVAR)

DA_CONVERTOR_MOD_DIR = -I../var/convertor -p../var/convertor
DA_CONVERTOR_MODULES = $(DA_CONVERTOR_MOD_DIR) $(INCLUDE_MODULES)


#### 3.d.   add macros to specify the modules for this core

#EXP_MODULE_DIR = -I../dyn_exp
#EXP_MODULES =  $(EXP_MODULE_DIR)

NMM_MODULE_DIR = -I../dyn_nmm
NMM_MODULES =  $(NMM_MODULE_DIR)

ALL_MODULES =                           \
               $(EM_MODULE_DIR)         \
               $(NMM_MODULES)           \
               $(EXP_MODULES)           \
               $(INCLUDE_MODULES)

configcheck:
	@echo " "
	@echo "============================================================================================== "
	@echo " "
	@echo "The following indicate the compilers selected to build the WRF system"
	@echo " "
	@echo "Serial Fortran compiler (mostly for tool generation):"
	@echo which SFC
	@which `echo $(SFC) | cut -d " " -f1`
	@echo " "
	@echo "Serial C compiler (mostly for tool generation):"
	@echo which SCC
	@which `echo $(SCC) | cut -d " " -f1`
	@echo " "
	@echo "Fortran compiler for the model source code:"
	@echo which FC
	@if command -v timex > /dev/null 2>&1; then \
	  which `echo $(FC) | cut -d " " -f2` ; \
	  echo "Will use 'timex' to report timing information" ; \
	elif command -v time > /dev/null 2>&1; then \
	  which `echo $(FC) | cut -d " " -f2` ; \
	  echo "Will use 'time' to report timing information" ; \
	else \
	  which `echo $(FC) | cut -d " " -f1` ; \
	fi
	@echo " "
	@echo "C compiler for the model source code:"
	@echo which CC
	@which `echo $(CC) | cut -d " " -f1`
	@echo " "
	@echo "============================================================================================== "
	@echo " "
	@if [ "$(A2DCASE)" -a "$(DMPARALLEL)" ] ; then \
	 echo "------------------------------------------------------------------------------" ; \
	 echo "WRF CONFIGURATION ERROR                                                       " ; \
	 echo "The $(A2DCASE) case requires a build for only single domain.                  " ; \
	 echo "The $(A2DCASE) case cannot be used on distributed memory parallel systems.    " ; \
	 echo "Only 3D WRF cases will run with the options that you selected.                " ; \
	 echo "Please choose a different case, or rerun configure and choose a different set of options."  ; \
	 echo "------------------------------------------------------------------------------" ; \
         exit 2 ; \
	fi
 

framework_only : configcheck
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" ext
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" toolsdir
	/bin/rm -f main/libwrflib.a main/libwrflib.lib
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" framework
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" shared

wrf : framework_only
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" physics
	if [ $(WRF_CHEM) -eq 1 ]    ; then $(MAKE) MODULE_DIRS="$(ALL_MODULES)" chemics ; fi
	if [ $(WRF_EM_CORE) -eq 1 ]    ; then $(MAKE) MODULE_DIRS="$(ALL_MODULES)" em_core ; fi
	if [ $(WRF_NMM_CORE) -eq 1 ]   ; then $(MAKE) MODULE_DIRS="$(ALL_MODULES)" nmm_core ; fi
	if [ $(WRF_EXP_CORE) -eq 1 ]   ; then $(MAKE) MODULE_DIRS="$(ALL_MODULES)" exp_core ; fi
	if [ $(WRF_HYDRO) -eq 1 ]   ; then $(MAKE) MODULE_DIRS="$(ALL_MODULES)" wrf_hydro ; fi
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em em_wrf )
	( cd run ; /bin/rm -f wrf.exe ; ln -s ../main/wrf.exe . )
	if [ $(ESMF_COUPLING) -eq 1 ] ; then \
	  ( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em em_wrf_SST_ESMF ) ; \
	fi
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`

all_wrfvar : 
	$(MAKE) MODULE_DIRS="$(DA_WRFVAR_MODULES)" ext
	$(MAKE) MODULE_DIRS="$(DA_WRFVAR_MODULES)" toolsdir
	if [ $(CRTM) -ne 0 ] ; then \
	  (cd var/external/crtm_2.1.3; \
	   export ABI_CRTM="${ABI_CRTM}"; . configure/$(SFC_CRTM).setup; $(MAKE) $(J) ) ; \
	fi
	if [ $(BUFR) ] ; then \
	  (cd var/external/bufr;  \
	  $(MAKE) $(J) FC="$(SFC)" CC="$(SCC)" CPP="$(CPP)" CPPFLAGS="$(CPPFLAGS)" CFLAGS="$(CFLAGS)" FFLAGS="$(FCOPTIM) $(FORMAT_FIXED)" RANLIB="$(RANLIB)" AR="$(AR)" ARFLAGS="$(ARFLAGS)" ) ; \
	fi
### Use 'make' to avoid '-i -r' above:
	if [ $(WAVELET) ] ; then \
	  ( cd var/external/wavelet; \
		make CC="$(SC99) -DNOUNDERSCORE" RM="$(RM)" libWavelet.a; \
		make FC="$(FC)" RM="$(RM)" lib_wavelet.a ) ; \
	fi
#	( cd var/build; touch depend.txt; make links; make depend; $(MAKE) $(J) all_wrfvar )
	( cd var/build; make depend; $(MAKE) $(J) all_wrfvar )
	( cd var/obsproc; $(MAKE) $(J) BUFR_CPP="$(BUFR_CPP)" )
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`

gen_be :
	$(MAKE) MODULE_DIRS="$(DA_WRFVAR_MODULES)" ext
	$(MAKE) MODULE_DIRS="$(DA_WRFVAR_MODULES)" toolsdir
	( cd var/build; make depend; $(MAKE) $(J) gen_be )
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`


### 3.a.  rules to build the framework and then the experimental core

exp_wrf : configcheck
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" ext
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" toolsdir
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" framework
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" shared
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=exp exp_wrf )


nmm_wrf : wrf


#  Eulerian mass coordinate initializations

em_fire : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=fire em_ideal )
	( cd test/em_fire ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_fire ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_fire ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/em_fire ; /bin/sh create_links.sh )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_fire/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_fire/input_sounding . )
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`

em_quarter_ss : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=quarter_ss em_ideal )
	( cd test/em_quarter_ss ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_quarter_ss ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_quarter_ss ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/em_quarter_ss ; /bin/rm -f bulkdens.asc_s_0_03_0_9 ; ln -s ../../run/bulkdens.asc_s_0_03_0_9 . )
	( cd test/em_quarter_ss ; /bin/rm -f bulkradii.asc_s_0_03_0_9 ; ln -s ../../run/bulkradii.asc_s_0_03_0_9 . )
	( cd test/em_quarter_ss ; /bin/rm -f capacity.asc ; ln -s ../../run/capacity.asc . )
	( cd test/em_quarter_ss ; /bin/rm -f coeff_p.asc ; ln -s ../../run/coeff_p.asc . )
	( cd test/em_quarter_ss ; /bin/rm -f coeff_q.asc ; ln -s ../../run/coeff_q.asc . )
	( cd test/em_quarter_ss ; /bin/rm -f constants.asc ; ln -s ../../run/constants.asc . )
	( cd test/em_quarter_ss ; /bin/rm -f kernels.asc_s_0_03_0_9 ; ln -s ../../run/kernels.asc_s_0_03_0_9 . )
	( cd test/em_quarter_ss ; /bin/rm -f kernels_z.asc ; ln -s ../../run/kernels_z.asc . )
	( cd test/em_quarter_ss ; /bin/rm -f masses.asc ; ln -s ../../run/masses.asc . )
	( cd test/em_quarter_ss ; /bin/rm -f termvels.asc ; ln -s ../../run/termvels.asc . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_quarter_ss/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_quarter_ss/input_sounding . )
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/ideal.exe ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi

em_squall2d_x : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=squall2d_x em_ideal )
	( cd test/em_squall2d_x ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_squall2d_x ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_squall2d_x ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_squall2d_x/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_squall2d_x/input_sounding . )
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/ideal.exe ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi

em_squall2d_y : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=squall2d_y em_ideal )
	( cd test/em_squall2d_y ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_squall2d_y ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_squall2d_y ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_squall2d_y/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_squall2d_y/input_sounding . )
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/ideal.exe ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi

em_b_wave : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=b_wave em_ideal )
	( cd test/em_b_wave ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_b_wave ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_b_wave ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_b_wave/namelist.input . )
	( cd run ; /bin/rm -f input_jet ; ln -s ../test/em_b_wave/input_jet . )
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/ideal.exe ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi

em_les : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=les em_ideal )
	( cd test/em_les ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_les ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_les ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_les/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_les/input_sounding . )
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/ideal.exe ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi

em_seabreeze2d_x : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=seabreeze2d_x em_ideal )
	( cd test/em_seabreeze2d_x ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_seabreeze2d_x ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_seabreeze2d_x ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_seabreeze2d_x/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_seabreeze2d_x/input_sounding . )
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/ideal.exe ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi

em_convrad : wrf
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=convrad em_ideal )
	( cd test/em_convrad ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_convrad ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_convrad ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_convrad/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_convrad/input_sounding . )
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`

em_tropical_cyclone : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=tropical_cyclone em_ideal )
	( cd test/em_tropical_cyclone ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_tropical_cyclone ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_tropical_cyclone ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_tropical_cyclone/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_tropical_cyclone/input_sounding . )
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/ideal.exe ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi

em_scm_xy : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=scm_xy em_ideal )
	( cd test/em_scm_xy ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_scm_xy ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_scm_xy ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_scm_xy/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_scm_xy/input_sounding . )
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/ideal.exe ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi

convert_em : framework_only
	if [ $(WRF_CONVERT) -eq 1 ] ; then \
            ( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" convert_em ) ; \
        fi

# Link wrf.exe and wrf_SST_ESMF.exe into 
# test/em_esmf_exp when ESMF_COUPLING is set.  wrf.exe 
# can be used for stand-alone testing in this case.  
# wrf_SST_ESMF.exe is a coupled application.  Note that make 
# target $(SOLVER)_wrf_SST_ESMF builds wrf_SST_ESMF.exe.  
em_real : wrf
	@/bin/rm -f real.exe  > /dev/null 2>&1
	@/bin/rm -f tc.exe    > /dev/null 2>&1
	@/bin/rm -f ndown.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real em_real )
	( cd test/em_real ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	if [ $(ESMF_COUPLING) -eq 1 ] ; then \
	  ( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real em_wrf_SST_ESMF ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f wrf_SST_ESMF.exe ; ln -s ../../main/wrf_SST_ESMF.exe . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f real.exe ; ln -s ../../main/real.exe . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f ETAMPNEW_DATA.expanded_rain ETAMPNEW_DATA RRTM_DATA RRTMG_LW_DATA RRTMG_SW_DATA ; \
               ln -sf ../../run/ETAMPNEW_DATA . ;                      \
               ln -sf ../../run/ETAMPNEW_DATA.expanded_rain . ;        \
               ln -sf ../../run/RRTM_DATA . ;                          \
               ln -sf ../../run/RRTMG_LW_DATA . ;                      \
               ln -sf ../../run/RRTMG_SW_DATA . ;                      \
               ln -sf ../../run/CAM_ABS_DATA . ;                       \
               ln -sf ../../run/CAM_AEROPT_DATA . ;                    \
               ln -sf ../../run/CAMtr_volume_mixing_ratio.RCP4.5 . ;   \
               ln -sf ../../run/CAMtr_volume_mixing_ratio.RCP6   . ;   \
               ln -sf ../../run/CAMtr_volume_mixing_ratio.RCP8.5 CAMtr_volume_mixing_ratio ;   \
               ln -sf ../../run/CAMtr_volume_mixing_ratio.A1B    . ;   \
               ln -sf ../../run/CAMtr_volume_mixing_ratio.A2     . ;   \
               ln -sf ../../run/CLM_ALB_ICE_DFS_DATA . ;               \
               ln -sf ../../run/CLM_ALB_ICE_DRC_DATA . ;               \
               ln -sf ../../run/CLM_ASM_ICE_DFS_DATA . ;               \
               ln -sf ../../run/CLM_ASM_ICE_DRC_DATA . ;               \
               ln -sf ../../run/CLM_DRDSDT0_DATA . ;                   \
               ln -sf ../../run/CLM_EXT_ICE_DFS_DATA . ;               \
               ln -sf ../../run/CLM_EXT_ICE_DRC_DATA . ;               \
               ln -sf ../../run/CLM_KAPPA_DATA . ;                     \
               ln -sf ../../run/CLM_TAU_DATA . ;                       \
               ln -sf ../../run/ozone.formatted . ;                    \
               ln -sf ../../run/ozone_lat.formatted . ;                \
               ln -sf ../../run/ozone_plev.formatted . ;               \
               ln -sf ../../run/aerosol.formatted . ;                  \
               ln -sf ../../run/aerosol_lat.formatted . ;              \
               ln -sf ../../run/aerosol_lon.formatted . ;              \
               ln -sf ../../run/aerosol_plev.formatted . ;             \
               ln -sf ../../run/CCN_ACTIVATE.BIN . ;                   \
               if [ $(RWORDSIZE) -eq 8 ] ; then                        \
                  ln -sf ../../run/ETAMPNEW_DATA_DBL ETAMPNEW_DATA ;   \
                  ln -sf ../../run/ETAMPNEW_DATA.expanded_rain_DBL ETAMPNEW_DATA.expanded_rain ;   \
                  ln -sf ../../run/RRTM_DATA_DBL RRTM_DATA         ;   \
                  ln -sf ../../run/RRTMG_LW_DATA_DBL RRTMG_LW_DATA ;   \
                  ln -sf ../../run/RRTMG_SW_DATA_DBL RRTMG_SW_DATA ;   \
               fi ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f GENPARM.TBL ; ln -s ../../run/GENPARM.TBL . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f LANDUSE.TBL ; ln -s ../../run/LANDUSE.TBL . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f SOILPARM.TBL ; ln -s ../../run/SOILPARM.TBL . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f URBPARM.TBL ; ln -s ../../run/URBPARM.TBL . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f VEGPARM.TBL ; ln -s ../../run/VEGPARM.TBL . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f MPTABLE.TBL ; ln -s ../../run/MPTABLE.TBL . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f tr49t67 ; ln -s ../../run/tr49t67 . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f tr49t85 ; ln -s ../../run/tr49t85 . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f tr67t85 ; ln -s ../../run/tr67t85 . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f grib2map.tbl ; ln -s ../../run/grib2map.tbl . ) ; \
	fi
	( cd test/em_real ; /bin/rm -f real.exe ; ln -s ../../main/real.exe . )
	( cd test/em_real ; /bin/rm -f tc.exe ; ln -s ../../main/tc.exe . )
	( cd test/em_real ; /bin/rm -f ndown.exe ; ln -s ../../main/ndown.exe . )
	#TEMPORARILY REMOVED ( cd test/em_real ; /bin/rm -f nup.exe ; ln -s ../../main/nup.exe . )
	( cd test/em_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/em_real ; /bin/rm -f ETAMPNEW_DATA.expanded_rain ETAMPNEW_DATA RRTM_DATA RRTMG_LW_DATA RRTMG_SW_DATA ;    \
             ln -sf ../../run/ETAMPNEW_DATA . ;                     \
             ln -sf ../../run/ETAMPNEW_DATA.expanded_rain . ;       \
             ln -sf ../../run/RRTM_DATA . ;                         \
             ln -sf ../../run/RRTMG_LW_DATA . ;                     \
             ln -sf ../../run/RRTMG_SW_DATA . ;                     \
             ln -sf ../../run/CAM_ABS_DATA . ;                      \
             ln -sf ../../run/CAM_AEROPT_DATA . ;                   \
             ln -sf ../../run/CAMtr_volume_mixing_ratio.RCP4.5 . ;  \
             ln -sf ../../run/CAMtr_volume_mixing_ratio.RCP6   . ;  \
             ln -sf ../../run/CAMtr_volume_mixing_ratio.RCP8.5 CAMtr_volume_mixing_ratio ;   \
             ln -sf ../../run/CAMtr_volume_mixing_ratio.A1B    . ;  \
             ln -sf ../../run/CAMtr_volume_mixing_ratio.A2     . ;  \
             ln -sf ../../run/CLM_ALB_ICE_DFS_DATA . ;              \
             ln -sf ../../run/CLM_ALB_ICE_DRC_DATA . ;              \
             ln -sf ../../run/CLM_ASM_ICE_DFS_DATA . ;              \
             ln -sf ../../run/CLM_ASM_ICE_DRC_DATA . ;              \
             ln -sf ../../run/CLM_DRDSDT0_DATA . ;                  \
             ln -sf ../../run/CLM_EXT_ICE_DFS_DATA . ;              \
             ln -sf ../../run/CLM_EXT_ICE_DRC_DATA . ;              \
             ln -sf ../../run/CLM_KAPPA_DATA . ;                    \
             ln -sf ../../run/CLM_TAU_DATA . ;                      \
             ln -sf ../../run/ozone.formatted . ;                   \
             ln -sf ../../run/ozone_lat.formatted . ;               \
             ln -sf ../../run/ozone_plev.formatted . ;              \
             ln -sf ../../run/aerosol.formatted . ;                 \
             ln -sf ../../run/aerosol_lat.formatted . ;             \
             ln -sf ../../run/aerosol_lon.formatted . ;             \
             ln -sf ../../run/aerosol_plev.formatted . ;            \
             ln -sf ../../run/capacity.asc . ;            \
             ln -sf ../../run/coeff_p.asc . ;            \
             ln -sf ../../run/coeff_q.asc . ;            \
             ln -sf ../../run/constants.asc . ;            \
             ln -sf ../../run/masses.asc . ;            \
             ln -sf ../../run/termvels.asc . ;            \
             ln -sf ../../run/kernels.asc_s_0_03_0_9 . ;            \
             ln -sf ../../run/kernels_z.asc . ;            \
             ln -sf ../../run/bulkdens.asc_s_0_03_0_9 . ;            \
             ln -sf ../../run/bulkradii.asc_s_0_03_0_9 . ;            \
             ln -sf ../../run/CCN_ACTIVATE.BIN . ;                   \
             if [ $(RWORDSIZE) -eq 8 ] ; then                       \
                ln -sf ../../run/ETAMPNEW_DATA_DBL ETAMPNEW_DATA ;  \
                ln -sf ../../run/ETAMPNEW_DATA.expanded_rain_DBL ETAMPNEW_DATA.expanded_rain ;   \
                ln -sf ../../run/RRTM_DATA_DBL RRTM_DATA ;          \
                ln -sf ../../run/RRTMG_LW_DATA_DBL RRTMG_LW_DATA ;  \
                ln -sf ../../run/RRTMG_SW_DATA_DBL RRTMG_SW_DATA ;  \
             fi )
	( cd test/em_real ; /bin/rm -f GENPARM.TBL ; ln -s ../../run/GENPARM.TBL . )
	( cd test/em_real ; /bin/rm -f LANDUSE.TBL ; ln -s ../../run/LANDUSE.TBL . )
	( cd test/em_real ; /bin/rm -f SOILPARM.TBL ; ln -s ../../run/SOILPARM.TBL . )
	( cd test/em_real ; /bin/rm -f URBPARM.TBL ; ln -s ../../run/URBPARM.TBL . )
	( cd test/em_real ; /bin/rm -f VEGPARM.TBL ; ln -s ../../run/VEGPARM.TBL . )
	( cd test/em_real ; /bin/rm -f MPTABLE.TBL ; ln -s ../../run/MPTABLE.TBL . )
	( cd test/em_real ; /bin/rm -f tr49t67 ; ln -s ../../run/tr49t67 . )
	( cd test/em_real ; /bin/rm -f tr49t85 ; ln -s ../../run/tr49t85 . )
	( cd test/em_real ; /bin/rm -f tr67t85 ; ln -s ../../run/tr67t85 . )
	( cd test/em_real ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . )
	( cd test/em_real ; /bin/rm -f grib2map.tbl ; ln -s ../../run/grib2map.tbl . )
	( cd run ; /bin/rm -f real.exe ; ln -s ../main/real.exe . )
	( cd run ; /bin/rm -f tc.exe ; ln -s ../main/tc.exe . )
	( cd run ; /bin/rm -f ndown.exe ; ln -s ../main/ndown.exe . )
	#TEMPORARILY REMOVED ( cd run ; /bin/rm -f nup.exe ; ln -s ../main/nup.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_real/namelist.input . )
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/real.exe -a -e main/ndown.exe -a -e main/tc.exe  ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi


em_hill2d_x : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=hill2d_x em_ideal )
	( cd test/em_hill2d_x ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_hill2d_x ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_hill2d_x ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_hill2d_x/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_hill2d_x/input_sounding . )
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/ideal.exe ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi

em_grav2d_x : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=grav2d_x em_ideal )
	( cd test/em_grav2d_x ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_grav2d_x ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_grav2d_x ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_grav2d_x/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_grav2d_x/input_sounding . )
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/ideal.exe ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi

em_heldsuarez : wrf
	@/bin/rm -f ideal.exe > /dev/null 2>&1
	@/bin/rm -f wrf.exe   > /dev/null 2>&1
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) RLFLAGS="$(RLFLAGS)" MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=heldsuarez em_ideal )
	( cd test/em_heldsuarez ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_heldsuarez ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_heldsuarez ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_heldsuarez/namelist.input . )
	@echo " "
	@echo "=========================================================================="
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`
	@if test -e main/wrf.exe -a -e main/ideal.exe ; then \
		echo " " ; \
		echo "--->                  Executables successfully built                  <---" ; \
		echo " " ; \
		ls -l main/*.exe ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	else \
		echo " " ; \
		echo "---> Problems building executables, look for errors in the build log  <---" ; \
		echo " " ; \
		echo "==========================================================================" ; \
		echo " " ; \
	fi

#### anthropogenic emissions converter

emi_conv : wrf
	@ echo '--------------------------------------'
	( cd chem ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real convert_emiss )
	( cd test/em_real ; /bin/rm -f convert_emiss.exe ; ln -s ../../chem/convert_emiss.exe . )
	( cd test/em_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_real/namelist.input . )

#### emissions opt 3 converter

opt3_conv : wrf
	@ echo '--------------------------------------'
	( cd chem ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real convert_fireemiss )
	( cd test/em_real ; /bin/rm -f convert_fireemiss.exe ; ln -s ../../chem/convert_fireemiss.exe . )
	( cd test/em_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_real/namelist.input . )

#### biogenic emissions converter

bio_conv : wrf
	@ echo '--------------------------------------'
	( cd chem ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real convert_bioemiss )
	( cd test/em_real ; /bin/rm -f convert_bioemiss.exe ; ln -s ../../chem/convert_bioemiss.exe . )
	( cd test/em_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_real/namelist.input . )

bioemiss_conv_megan2 : wrf
	@ echo '--------------------------------------'
	( cd chem ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real convert_bioemiss_megan2 )
	( cd test/em_real ; /bin/rm -f convert_bioemiss_megan2.exe ; ln -s ../../chem/convert_bioemiss_megan2.exe . )
	( cd test/em_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; if test -f namelist.input ; then \
	        /bin/cp -f namelist.input namelist.input.backup ; fi ; \
	        /bin/rm -f namelist.input ; cp ../test/em_real/namelist.input . )

#### DMS emissions converter

dms_conv : wrf
	@ echo '--------------------------------------'
	( cd chem ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real convert_dms )
	( cd test/em_real ; /bin/rm -f convert_dms.exe ; ln -s ../../chem/convert_dms.exe . )
	( cd test/em_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_real/namelist.input . )


#### Dust errosion factor emissions converter

dust_conv : wrf
	@ echo '--------------------------------------'
	( cd chem ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real convert_dust )
	( cd test/em_real ; /bin/rm -f convert_dust.exe ; ln -s ../../chem/convert_dust.exe . )
	( cd test/em_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_real/namelist.input . )

#### GOCART background state for oh, no3 and h2o2 converter

gocart_conv : wrf
	@ echo '--------------------------------------'
	( cd chem ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real convert_gocart )
	( cd test/em_real ; /bin/rm -f convert_gocart.exe ; ln -s ../../chem/convert_gocart.exe . )
	( cd test/em_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/em_real/namelist.input . )


#### nmm converter

### Idealized NMM tropical cyclone case
nmm_tropical_cyclone : nmm_wrf
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=nmm IDEAL_CASE=tropical_cyclone nmm_ideal )
	( cd test/nmm_tropical_cyclone ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/nmm_tropical_cyclone ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/nmm_tropical_cyclone ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/nmm_tropical_cyclone/namelist.input . )
	@echo "build started:   $(START_OF_COMPILE)"
	@echo "build completed:" `date`

nmm_real : nmm_wrf
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=nmm IDEAL_CASE=real real_nmm )
	( cd test/nmm_real ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/nmm_real ; /bin/rm -f real_nmm.exe ; ln -s ../../main/real_nmm.exe . )
	( cd test/nmm_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/nmm_real ; /bin/rm -f ETAMPNEW_DATA.expanded_rain ETAMPNEW_DATA RRTM_DATA ;    \
	     ln -sf ../../run/ETAMPNEW_DATA . ;                     \
	     ln -sf ../../run/ETAMPNEW_DATA.expanded_rain . ;                     \
	     ln -sf ../../run/RRTM_DATA . ;                         \
	     if [ $(RWORDSIZE) -eq 8 ] ; then                       \
	        ln -sf ../../run/ETAMPNEW_DATA_DBL ETAMPNEW_DATA ;  \
                ln -sf ../../run/ETAMPNEW_DATA.expanded_rain_DBL ETAMPNEW_DATA.expanded_rain ;   \
	        ln -sf ../../run/RRTM_DATA_DBL RRTM_DATA ;          \
	     fi )
	( cd test/nmm_real ; /bin/rm -f GENPARM.TBL ; ln -s ../../run/GENPARM.TBL . )
	( cd test/nmm_real ; /bin/rm -f LANDUSE.TBL ; ln -s ../../run/LANDUSE.TBL . )
	( cd test/nmm_real ; /bin/rm -f SOILPARM.TBL ; ln -s ../../run/SOILPARM.TBL . )
	( cd test/nmm_real ; /bin/rm -f VEGPARM.TBL ; ln -s ../../run/VEGPARM.TBL . )
	( cd test/nmm_real ; /bin/rm -f MPTABLE.TBL ; ln -s ../../run/MPTABLE.TBL . )
	( cd test/nmm_real ; /bin/rm -f tr49t67 ; ln -s ../../run/tr49t67 . )
	( cd test/nmm_real ; /bin/rm -f tr49t85 ; ln -s ../../run/tr49t85 . )
	( cd test/nmm_real ; /bin/rm -f tr67t85 ; ln -s ../../run/tr67t85 . )
	( cd test/nmm_real ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . )
	( cd test/nmm_real ; /bin/rm -f grib2map.tbl ; ln -s ../../run/grib2map.tbl . )
	( cd test/nmm_real ; /bin/rm -f co2_trans ; ln -s ../../run/co2_trans . )
	( cd run ; /bin/rm -f real_nmm.exe ; ln -s ../main/real_nmm.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; cp ../test/nmm_real/namelist.input . )



# semi-Lagrangian initializations


io :
	@ echo '--------------------------------------'
	( cd tools ; $(MAKE) standard.exe )
	(            $(MAKE) io_only )
	(            $(MAKE) MODULE_DIRS="$(ALL_MODULES)" toolsdir )
	( cd frame ; $(MAKE) module_driver_constants.o pack_utils.o module_machine.o module_internal_header_util.o wrf_debug.o )
	( cd frame ; $(AR) $(ARFLAGS) ../main/libwrflib.a module_driver_constants.o pack_utils.o module_machine.o  \
					module_internal_header_util.o module_wrf_error.o wrf_debug.o )

ext :
	@ echo '--------------------------------------'
	( cd frame ; $(MAKE) externals )

framework :
	@ echo '--------------------------------------'
	( cd frame ; $(MAKE) $(J) framework; \
          cd ../external/io_netcdf ; \
          $(MAKE) NETCDFPATH="$(NETCDFPATH)" FC="$(FC) $(FCBASEOPTS)" RANLIB="$(RANLIB)" \
               CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" TRADFLAG="$(TRADFLAG)" ESMF_IO_LIB_EXT="$(ESMF_IO_LIB_EXT)" \
	       LIB_LOCAL="$(LIB_LOCAL)" \
               ESMF_MOD_DEPENDENCE="$(ESMF_MOD_DEPENDENCE)" AR="INTERNAL_BUILD_ERROR_SHOULD_NOT_NEED_AR" diffwrf; \
          cd ../io_netcdf ; \
          $(MAKE) NETCDFPATH="$(NETCDFPATH)" FC="$(SFC) $(FCBASEOPTS)" RANLIB="$(RANLIB)" \
               CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" TRADFLAG="$(TRADFLAG)" ESMF_IO_LIB_EXT="$(ESMF_IO_LIB_EXT)" \
	       LIB_LOCAL="$(LIB_LOCAL)" \
               ESMF_MOD_DEPENDENCE="$(ESMF_MOD_DEPENDENCE)" AR="INTERNAL_BUILD_ERROR_SHOULD_NOT_NEED_AR"; \
          cd ../io_pio ; \
          echo SKIPPING PIO BUILD $(MAKE) NETCDFPATH="$(PNETCDFPATH)" FC="$(SFC) $(FCBASEOPTS)" RANLIB="$(RANLIB)" \
               CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" TRADFLAG="$(TRADFLAG)" ESMF_IO_LIB_EXT="$(ESMF_IO_LIB_EXT)" \
	       LIB_LOCAL="$(LIB_LOCAL)" \
               ESMF_MOD_DEPENDENCE="$(ESMF_MOD_DEPENDENCE)" AR="INTERNAL_BUILD_ERROR_SHOULD_NOT_NEED_AR"; \
          cd ../io_int ; \
          $(MAKE) SFC="$(SFC) $(FCBASEOPTS)" FC="$(SFC) $(FCBASEOPTS)" RANLIB="$(RANLIB)" CPP="$(CPP) $(ARCH_LOCAL)" DM_FC="$(DM_FC) $(FCBASEOPTS)"\
               TRADFLAG="$(TRADFLAG)" ESMF_IO_LIB_EXT="$(ESMF_IO_LIB_EXT)" \
               ESMF_MOD_DEPENDENCE="$(ESMF_MOD_DEPENDENCE)" AR="INTERNAL_BUILD_ERROR_SHOULD_NOT_NEED_AR" diffwrf ; \
          cd ../../frame )

shared :
	@ echo '--------------------------------------'
	if [ "`echo $(J) | sed -e 's/-j//g' -e 's/ \+//g'`" -gt "6" ] ; then \
	  ( cd share ; $(MAKE) -j 6 ) ;  \
	else \
	  ( cd share ; $(MAKE) $(J) ) ;  \
	fi

wrf_hydro :
	@ echo '----------wrf_hydro-----------------------'
	if [ $(WRF_HYDRO) -eq 1 ]   ; then (cd hydro/CPL/WRF_cpl; make -f Makefile.cpl) ; fi

chemics :
	@ echo '--------------------------------------'
	if [ $(WRF_KPP) -eq 1 ] ; then ( cd chem ; $(MAKE) ) ; fi
	if [ $(WRF_KPP) -eq 0 ] ; then \
	  if  [ "`echo $(J) | sed -e 's/-j//g' -e 's/ \+//g'`" -gt "16" ] ; then \
	    ( cd chem ; $(MAKE) -j 16 ) ;  \
	  else \
	    ( cd chem ; $(MAKE) $(J) ) ; \
	  fi \
	fi
#	( cd chem ; $(MAKE) )
#	( cd chem ; $(MAKE) $(J) )

physics :
	@ echo '--------------------------------------'
	if [ $(WRF_CHEM) -eq 0 ] ; then \
		( cd phys ; $(MAKE) CF2=" " ) ; \
	else \
		( cd phys ; $(MAKE) CF2="$(CHEM_FILES2)" ) ; \
	fi

em_core :
	@ echo '--------------------------------------'
	if [ $(WRF_CHEM) -eq 0 ] ; then \
		CF= ; \
	else \
		CF="$(CHEM_FILES)" ; \
	fi
	( cd dyn_em ; $(MAKE) $(J) CF="$(CF)" )

# rule used by configure to test if this will compile with MPI 2 calls MPI_Comm_f2c and _c2f
mpi2_test :
	@ cd tools ; /bin/rm -f mpi2_test ; $(CC) -c mpi2_test.c ; cd ..

# rule used by configure to test if this will compile with MPI 2 calls MPI_Init_thread
mpi2_thread_test :
	@ cd tools ; /bin/rm -f mpi2_thread_test ; $(CC) -c mpi2_thread_test.c ; cd ..

# rule used by configure to test if fseeko and fseeko64 are supported (for share/landread.c to work right)
fseek_test :
	@ cd tools ; /bin/rm -f fseeko_test ; $(SCC) -DTEST_FSEEKO -o fseeko_test fseek_test.c ; cd ..
	@ cd tools ; /bin/rm -f fseeko64_test ; $(SCC) -DTEST_FSEEKO64 -o fseeko64_test fseek_test.c ; cd ..

# rule used by configure to test if this will compile with netcdf4
nc4_test:
	@cd tools ; /bin/rm -f nc4_test.{exe,nc,o} ; $(SCC) -o nc4_test.exe nc4_test.c -I$(NETCDF)/include -L$(NETCDF)/lib -lnetcdf $(NETCDF4_DEP_LIB) ; cd ..

# rule used by configure to test if Fortran 2003 IEEE signaling is available
fortran_2003_ieee_test:
	@cd tools ; /bin/rm -f fortran_2003_ieee_test.{exe,o} ; $(SFC) -o fortran_2003_ieee_test.exe fortran_2003_ieee_test.F ; cd ..

# rule used by configure to test if Fortran 2003 ISO_C support is available
fortran_2003_iso_c_test:
	@cd tools ; /bin/rm -f fortran_2003_iso_c_test.{exe,o} ; $(SFC) -o fortran_2003_iso_c_test.exe fortran_2003_iso_c_test.F ; cd ..

# rule used by configure to test if Fortran 2003 FLUSH intrinsic subroutine support is available
fortran_2003_flush_test:
	@cd tools ; /bin/rm -f fortran_2003_flush_test.{exe,o} ; $(SFC) -o fortran_2003_flush_test.exe fortran_2003_flush_test.F ; cd ..

# rule used by configure to test if Fortran 2003 FLUSH intrinsic subroutine is replaced by FFLUSH (thanks xlf)
fortran_2003_fflush_test:
	@cd tools ; /bin/rm -f fortran_2003_fflush_test.{exe,o} ; $(SFC) -o fortran_2003_fflush_test.exe fortran_2003_fflush_test.F ; cd ..

### 3.b.  sub-rule to build the experimental core

# uncomment the two lines after exp_core for EXP
exp_core :
	@ echo '--------------------------------------'
	( cd dyn_exp ; $(MAKE) )

# uncomment the two lines after nmm_core for NMM
nmm_core :
	@ echo '--------------------------------------'
	if [ "`echo $(J) | sed -e 's/-j//g' -e 's/ \+//g'`" -gt "16" ] ; then \
	  ( cd dyn_nmm ; $(MAKE) -j 16 ) ;  \
	else \
	  ( cd dyn_nmm ; $(MAKE) $(J) ) ;  \
	fi

toolsdir :
	@ echo '--------------------------------------'
	( cd tools ; $(MAKE) CC_TOOLS_CFLAGS="$(CC_TOOLS_CFLAGS)" CC_TOOLS="$(CC_TOOLS) -DIWORDSIZE=$(IWORDSIZE) -DMAX_HISTORY=$(MAX_HISTORY)" )


#	( cd tools ; $(MAKE) CC_TOOLS="$(CC_TOOLS) -DIO_MASK_SIZE=$(IO_MASK_SIZE)" )

# Use this target to build stand-alone tests of esmf_time_f90.  
# Only touches external/esmf_time_f90/.  
esmf_time_f90_only :
	@ echo '--------------------------------------'
	( cd external/esmf_time_f90 ; $(MAKE) FC="$(FC) $(FCFLAGS)" CPP="$(CPP) -DTIME_F90_ONLY" tests )

clean :
		@ echo 'Use the clean script'

# DO NOT DELETE
