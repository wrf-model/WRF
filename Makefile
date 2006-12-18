#	Top level Makefile for wrf system

LN      =       ln -s
MAKE    =       make -i -r
MV	=	/bin/mv
RM      =       /bin/rm -f

deflt :
		@ echo Please compile the code using ./compile

include ./configure.wrf

EM_MODULE_DIR = -I../dyn_em
EM_MODULES =  $(EM_MODULE_DIR)


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
	@if [ "$(A2DCASE)" -a "$(DMPARALLEL)" ] ; then \
	 echo "------------------------------------------------------------------------------" ; \
	 echo "WRF CONFIGURATION ERROR                                                       " ; \
	 echo "The $(A2DCASE) case cannot be used on distributed memory parallel systems." ; \
	 echo "Only 3D WRF cases will run on these systems." ; \
	 echo "Please chose a different case or rerun configure and chose a different option."  ; \
	 echo "------------------------------------------------------------------------------" ; \
         exit 2 ; \
	fi

framework_only : configcheck
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" ext
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" toolsdir
	/bin/rm -f main/libwrflib.a
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" framework
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" shared

wrf : framework_only
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" physics
	if [ $(WRF_CHEM) -eq 1 ]    ; then $(MAKE) MODULE_DIRS="$(ALL_MODULES)" chemics ; fi
	if [ $(WRF_EM_CORE) -eq 1 ]    ; then $(MAKE) MODULE_DIRS="$(ALL_MODULES)" em_core ; fi
	if [ $(WRF_NMM_CORE) -eq 1 ]   ; then $(MAKE) MODULE_DIRS="$(ALL_MODULES)" nmm_core ; fi
	if [ $(WRF_EXP_CORE) -eq 1 ]   ; then $(MAKE) MODULE_DIRS="$(ALL_MODULES)" exp_core ; fi
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em em_wrf )
	( cd run ; /bin/rm -f wrf.exe ; ln -s ../main/wrf.exe . )
	if [ $(ESMF_COUPLING) -eq 1 ] ; then \
	  ( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em em_wrf_ESMFApp ) ; \
	fi

### 3.a.  rules to build the framework and then the experimental core

exp_wrf : configcheck
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" ext
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" toolsdir
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" framework
	$(MAKE) MODULE_DIRS="$(ALL_MODULES)" shared
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=exp exp_wrf )


nmm_wrf : wrf


#  Eulerian mass coordinate initializations

em_quarter_ss : wrf
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=quarter_ss em_ideal )
	( cd test/em_quarter_ss ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_quarter_ss ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_quarter_ss ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/em_quarter_ss ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . )
	( cd test/em_quarter_ss ; /bin/rm -f grib2map.tbl ; ln -s ../../run/grib2map.tbl . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; ln -s ../test/em_quarter_ss/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_quarter_ss/input_sounding . )

em_squall2d_x : wrf
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=squall2d_x em_ideal )
	( cd test/em_squall2d_x ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_squall2d_x ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_squall2d_x ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/em_squall2d_x ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . )
	( cd test/em_squall2d_x ; /bin/rm -f grib2map.tbl ; ln -s ../../run/grib2map.tbl . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; ln -s ../test/em_squall2d_x/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_squall2d_x/input_sounding . )

em_squall2d_y : wrf
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=squall2d_y em_ideal )
	( cd test/em_squall2d_y ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_squall2d_y ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_squall2d_y ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/em_squall2d_y ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . )
	( cd test/em_squall2d_y ; /bin/rm -f grib2map.tbl ; ln -s ../../run/grib2map.tbl . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; ln -s ../test/em_squall2d_y/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_squall2d_y/input_sounding . )

em_b_wave : wrf
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=b_wave em_ideal )
	( cd test/em_b_wave ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_b_wave ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_b_wave ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/em_b_wave ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . )
	( cd test/em_b_wave ; /bin/rm -f grib2map.tbl ; ln -s ../../run/grib2map.tbl . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; ln -s ../test/em_b_wave/namelist.input . )
	( cd run ; /bin/rm -f input_jet ; ln -s ../test/em_b_wave/input_jet . )

convert_em : framework_only
	if [ $(WRF_CONVERT) -eq 1 ] ; then \
            ( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" convert_em ) ; \
        fi

#TBH:  For now, link wrf.exe, wrf_ESMFApp.exe, and wrf_SST_ESMF.exe into 
#TBH:  test/em_esmf_exp when ESMF_COUPLING is set.  Either wrf.exe or 
#TBH:  wrf_ESMFApp.exe can be used for stand-alone testing in this case.  
#TBH:  wrf_SST_ESMF.exe is a coupled application.  Note that single make 
#TBH:  target $(SOLVER)_wrf_ESMFApp builds both wrf_ESMFApp.exe and 
#TBH:  wrf_SST_ESMF.exe.  
#TBH:  Is this a clear violation of the DRY principle?  Oh yeah, you bet.  
em_real : wrf
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real em_real )
	( cd test/em_real ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	if [ $(ESMF_COUPLING) -eq 1 ] ; then \
	  ( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real em_wrf_ESMFApp ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f wrf_ESMFApp.exe ; ln -s ../../main/wrf_ESMFApp.exe . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f wrf_SST_ESMF.exe ; ln -s ../../main/wrf_SST_ESMF.exe . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f real.exe ; ln -s ../../main/real.exe . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f ETAMPNEW_DATA RRTM_DATA ; \
               ln -sf ../../run/ETAMPNEW_DATA . ;                      \
               ln -sf ../../run/RRTM_DATA . ;                          \
               ln -sf ../../run/CAM_ABS_DATA . ;                       \
               ln -sf ../../run/CAM_AEROPT_DATA . ;                    \
               ln -sf ../../run/ozone.formatted . ;                    \
               ln -sf ../../run/ozone_lat.formatted . ;                \
               ln -sf ../../run/ozone_plev.formatted . ;               \
               if [ $(RWORDSIZE) -eq 8 ] ; then                        \
                  ln -sf ../../run/ETAMPNEW_DATA_DBL ETAMPNEW_DATA ;   \
                  ln -sf ../../run/RRTM_DATA_DBL RRTM_DATA ;           \
               fi ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f GENPARM.TBL ; ln -s ../../run/GENPARM.TBL . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f LANDUSE.TBL ; ln -s ../../run/LANDUSE.TBL . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f SOILPARM.TBL ; ln -s ../../run/SOILPARM.TBL . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f urban_param.tbl ; ln -s ../../run/urban_param.tbl . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f VEGPARM.TBL ; ln -s ../../run/VEGPARM.TBL . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f tr49t67 ; ln -s ../../run/tr49t67 . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f tr49t85 ; ln -s ../../run/tr49t85 . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f tr67t85 ; ln -s ../../run/tr67t85 . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . ) ; \
	  ( cd test/em_esmf_exp ; /bin/rm -f grib2map.tbl ; ln -s ../../run/grib2map.tbl . ) ; \
	fi
	( cd test/em_real ; /bin/rm -f real.exe ; ln -s ../../main/real.exe . )
	( cd test/em_real ; /bin/rm -f ndown.exe ; ln -s ../../main/ndown.exe . )
	( cd test/em_real ; /bin/rm -f nup.exe ; ln -s ../../main/nup.exe . )
	( cd test/em_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/em_real ; /bin/rm -f ETAMPNEW_DATA RRTM_DATA ;    \
             ln -sf ../../run/ETAMPNEW_DATA . ;                     \
             ln -sf ../../run/RRTM_DATA . ;                         \
             ln -sf ../../run/CAM_ABS_DATA . ;                         \
             ln -sf ../../run/CAM_AEROPT_DATA . ;                         \
             ln -sf ../../run/ozone.formatted . ;                         \
             ln -sf ../../run/ozone_lat.formatted . ;                         \
             ln -sf ../../run/ozone_plev.formatted . ;                         \
             if [ $(RWORDSIZE) -eq 8 ] ; then                       \
                ln -sf ../../run/ETAMPNEW_DATA_DBL ETAMPNEW_DATA ;  \
                ln -sf ../../run/RRTM_DATA_DBL RRTM_DATA ;          \
             fi )
	( cd test/em_real ; /bin/rm -f GENPARM.TBL ; ln -s ../../run/GENPARM.TBL . )
	( cd test/em_real ; /bin/rm -f LANDUSE.TBL ; ln -s ../../run/LANDUSE.TBL . )
	( cd test/em_real ; /bin/rm -f SOILPARM.TBL ; ln -s ../../run/SOILPARM.TBL . )
	( cd test/em_real ; /bin/rm -f urban_param.tbl ; ln -s ../../run/urban_param.tbl . )
	( cd test/em_real ; /bin/rm -f VEGPARM.TBL ; ln -s ../../run/VEGPARM.TBL . )
	( cd test/em_real ; /bin/rm -f tr49t67 ; ln -s ../../run/tr49t67 . )
	( cd test/em_real ; /bin/rm -f tr49t85 ; ln -s ../../run/tr49t85 . )
	( cd test/em_real ; /bin/rm -f tr67t85 ; ln -s ../../run/tr67t85 . )
	( cd test/em_real ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . )
	( cd test/em_real ; /bin/rm -f grib2map.tbl ; ln -s ../../run/grib2map.tbl . )
	( cd run ; /bin/rm -f real.exe ; ln -s ../main/real.exe . )
	( cd run ; /bin/rm -f ndown.exe ; ln -s ../main/ndown.exe . )
	( cd run ; /bin/rm -f nup.exe ; ln -s ../main/nup.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; ln -s ../test/em_real/namelist.input . )


em_hill2d_x : wrf
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=hill2d_x em_ideal )
	( cd test/em_hill2d_x ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_hill2d_x ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_hill2d_x ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/em_hill2d_x ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . )
	( cd test/em_hill2d_x ; /bin/rm -f grib2map.tbl ; ln -s ../../run/grib2map.tbl . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; ln -s ../test/em_hill2d_x/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_hill2d_x/input_sounding . )

em_grav2d_x : wrf
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=grav2d_x em_ideal )
	( cd test/em_grav2d_x ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/em_grav2d_x ; /bin/rm -f ideal.exe ; ln -s ../../main/ideal.exe . )
	( cd test/em_grav2d_x ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/em_grav2d_x ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . )
	( cd test/em_grav2d_x ; /bin/rm -f grib2map.tbl ; ln -s ../../run/grib2map.tbl . )
	( cd run ; /bin/rm -f ideal.exe ; ln -s ../main/ideal.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; ln -s ../test/em_grav2d_x/namelist.input . )
	( cd run ; /bin/rm -f input_sounding ; ln -s ../test/em_grav2d_x/input_sounding . )

#### anthropogenic emissions converter

emi_conv : wrf
	@ echo '--------------------------------------'
	( cd chem ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real convert_emiss )
	( cd test/em_real ; /bin/rm -f convert_emiss.exe ; ln -s ../../chem/convert_emiss.exe . )
	( cd test/em_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; ln -s ../test/em_real/namelist.input . )

#### biogenic emissions converter

bio_conv : wrf
	@ echo '--------------------------------------'
	( cd chem ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=em IDEAL_CASE=real convert_bioemiss )
	( cd test/em_real ; /bin/rm -f convert_bioemiss.exe ; ln -s ../../chem/convert_bioemiss.exe . )
	( cd test/em_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; ln -s ../test/em_real/namelist.input . )

#### nmm converter

nmm_real : nmm_wrf
	@ echo '--------------------------------------'
	( cd main ; $(MAKE) MODULE_DIRS="$(ALL_MODULES)" SOLVER=nmm IDEAL_CASE=real real_nmm )
	( cd test/nmm_real ; /bin/rm -f wrf.exe ; ln -s ../../main/wrf.exe . )
	( cd test/nmm_real ; /bin/rm -f real_nmm.exe ; ln -s ../../main/real_nmm.exe . )
	( cd test/nmm_real ; /bin/rm -f README.namelist ; ln -s ../../run/README.namelist . )
	( cd test/nmm_real ; /bin/rm -f ETAMPNEW_DATA RRTM_DATA ;    \
	     ln -sf ../../run/ETAMPNEW_DATA . ;                     \
	     ln -sf ../../run/RRTM_DATA . ;                         \
	     if [ $(RWORDSIZE) -eq 8 ] ; then                       \
	        ln -sf ../../run/ETAMPNEW_DATA_DBL ETAMPNEW_DATA ;  \
	        ln -sf ../../run/RRTM_DATA_DBL RRTM_DATA ;          \
	     fi )
	( cd test/nmm_real ; /bin/rm -f GENPARM.TBL ; ln -s ../../run/GENPARM.TBL . )
	( cd test/nmm_real ; /bin/rm -f LANDUSE.TBL ; ln -s ../../run/LANDUSE.TBL . )
	( cd test/nmm_real ; /bin/rm -f SOILPARM.TBL ; ln -s ../../run/SOILPARM.TBL . )
	( cd test/nmm_real ; /bin/rm -f VEGPARM.TBL ; ln -s ../../run/VEGPARM.TBL . )
	( cd test/nmm_real ; /bin/rm -f tr49t67 ; ln -s ../../run/tr49t67 . )
	( cd test/nmm_real ; /bin/rm -f tr49t85 ; ln -s ../../run/tr49t85 . )
	( cd test/nmm_real ; /bin/rm -f tr67t85 ; ln -s ../../run/tr67t85 . )
	( cd test/nmm_real ; /bin/rm -f gribmap.txt ; ln -s ../../run/gribmap.txt . )
	( cd test/nmm_real ; /bin/rm -f grib2map.txt ; ln -s ../../run/grib2map.txt . )
	( cd run ; /bin/rm -f real_nmm.exe ; ln -s ../main/real_nmm.exe . )
	( cd run ; if test -f namelist.input ; then \
		/bin/cp -f namelist.input namelist.input.backup ; fi ; \
		/bin/rm -f namelist.input ; ln -s ../test/nmm_real/namelist.input . )



# semi-Lagrangian initializations


ext :
	@ echo '--------------------------------------'
	( cd frame ; $(MAKE) externals )

framework :
	@ echo '--------------------------------------'
	( cd frame ; $(MAKE) framework; \
	cd ../external/io_netcdf ; make NETCDFPATH="$(NETCDFPATH)" FC="$(SFC) $(FCBASEOPTS)" RANLIB="$(RANLIB)" CPP="$(CPP)" LDFLAGS="$(LDFLAGS)" ESMF_IO_LIB_EXT="$(ESMF_IO_LIB_EXT)" ESMF_MOD_DEPENDENCE="../$(ESMF_MOD_DEPENDENCE)" diffwrf; \
	cd ../io_int ; $(MAKE) SFC="$(SFC) $(FCBASEOPTS)" FC="$(SFC) $(FCBASEOPTS)" RANLIB="$(RANLIB)" CPP="$(CPP)" ESMF_IO_LIB_EXT="$(ESMF_IO_LIB_EXT)" ESMF_MOD_DEPENDENCE="../$(ESMF_MOD_DEPENDENCE)" diffwrf ; cd ../../frame )

shared :
	@ echo '--------------------------------------'
	( cd share ; $(MAKE) )

chemics :
	@ echo '--------------------------------------'
	( cd chem ; $(MAKE) )

physics :
	@ echo '--------------------------------------'
	( cd phys ; $(MAKE) )

em_core :
	@ echo '--------------------------------------'
	( cd dyn_em ; $(MAKE) )

# rule used by configure to test if this will compile with MPI 2 calls MPI_Comm_f2c and _c2f
mpi2_test :
	@ cd tools ; /bin/rm -f mpi2_test ; $(CC) -c mpi2_test.c ; cd ..

# rule used by configure to test if fseeko and fseeko64 are supported (for share/landread.c to work right)
fseek_test :
	@ cd tools ; /bin/rm -f fseeko_test ; $(SCC) -DTEST_FSEEKO -o fseeko_test fseek_test.c ; cd ..
	@ cd tools ; /bin/rm -f fseeko64_test ; $(SCC) -DTEST_FSEEKO64 -o fseeko64_test fseek_test.c ; cd ..

### 3.b.  sub-rule to build the expimental core

# uncomment the two lines after exp_core for EXP
exp_core :
	@ echo '--------------------------------------'
	( cd dyn_exp ; $(MAKE) )

# uncomment the two lines after nmm_core for NMM
nmm_core :
	@ echo '--------------------------------------'
	( cd dyn_nmm ; $(MAKE) )

toolsdir :
	@ echo '--------------------------------------'
	( cd tools ; $(MAKE) CC="$(CC_TOOLS)" )

# Use this target to build stand-alone tests of esmf_time_f90.  
# Only touches external/esmf_time_f90/.  
esmf_time_f90_only :
	@ echo '--------------------------------------'
	( cd external/esmf_time_f90 ; $(MAKE) FC="$(FC) $(FCFLAGS)" CPP="$(CPP) -DTIME_F90_ONLY" tests )

clean :
		@ echo 'Use the clean script'

# DO NOT DELETE
