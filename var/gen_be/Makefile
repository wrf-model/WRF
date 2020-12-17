#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f
DA = ../da

include $(WRF_SRC_ROOT_DIR)/configure.wrf

gen_be : 	gen_be_ensrf.exe	\
		gen_be_etkf.exe	\
		gen_be_ensmean.exe	\
		gen_be_stage0_wrf.exe	\
		gen_be_ep1.exe	\
		gen_be_ep2.exe	\
		gen_be_vertloc.exe	\
		gen_be_addmean.exe	
		gen_be_stage1.exe	\
		gen_be_stage1_1dvar.exe	\
		gen_be_stage2.exe	\
                gen_be_stage2_gsi.exe   \
		gen_be_stage2_1dvar.exe	\
		gen_be_stage2a.exe	\
		gen_be_stage3.exe	\
		gen_be_stage4_global.exe	\
		gen_be_stage4_regional.exe	\
		gen_be_cov2d.exe		\
		gen_be_cov3d.exe		\
		gen_be_diags.exe		\
		gen_be_diags_read.exe
	( cd ../da; $(LN) ../gen_be/*exe . )

MODULE_DIRS = $(INCLUDE_MODULES)

LIB     = \
                     -L$(WRF_SRC_ROOT_DIR)/external/fftpack/fftpack5 -lfftpack \
                     -L$(WRF_SRC_ROOT_DIR)/external/io_grib1 -lio_grib1 \
                     -L$(WRF_SRC_ROOT_DIR)/external/io_grib_share -lio_grib_share \
                     -L$(WRF_SRC_ROOT_DIR)/external/io_int -lwrfio_int \
                     $(ESMF_IO_LIB) \
                     $(WRF_SRC_ROOT_DIR)/external/RSL_LITE/librsl_lite.a \
                     $(LIB_EXTERNAL) $(LIB_LOCAL)

GEN_BE_LIBS = $(WRF_SRC_ROOT_DIR)/external/io_netcdf/libwrfio_nf.a
GEN_BE_LIB = $(LIB) -L$(LAPACK) -llapack -L$(BLAS) -lblas 


GEN_BE_OBJS = $(DA)/da_etkf.o $(DA)/da_gen_be.o $(DA)/da_control.o $(DA)/da_be_spectral.o \
  $(DA)/module_wrf_error.o $(DA)/da_reporting.o $(DA)/module_ffts.o \
  $(DA)/module_driver_constants.o $(DA)/da_memory.o $(DA)/da_tools_serial.o

gen_be_stage0_wrf.exe : gen_be_stage0_wrf.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_stage0_wrf.f90 > gen_be_stage0_wrf.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_stage0_wrf.f
	$(SFC) -o gen_be_stage0_wrf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage0_wrf.o $(GEN_BE_LIB)

gen_be_ep1.exe     : gen_be_ep1.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_ep1.f90 > gen_be_ep1.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_ep1.f
	$(SFC) -o gen_be_ep1.exe $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_ep1.o $(GEN_BE_LIB)

gen_be_ep2.exe     : gen_be_ep2.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_ep2.f90 > gen_be_ep2.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_ep2.f
	$(SFC) -o gen_be_ep2.exe $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_ep2.o $(GEN_BE_LIB)

gen_be_vertloc.exe     : gen_be_vertloc.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
        $(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_vertloc.f90 > gen_be_vertloc.f
        $(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_vertloc.f
        $(SFC) -o gen_be_vertloc.exe $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_vertloc.o $(GEN_BE_LIB)

gen_be_addmean.exe : gen_be_addmean.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc -I$(NETCDFPATH)/include gen_be_addmean.f90 > gen_be_addmean.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_addmean.f
	$(SFC) -o gen_be_addmean.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_addmean.o $(GEN_BE_LIB)

gen_be_stage1.exe : gen_be_stage1.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_stage1.f90 > gen_be_stage1.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_stage1.f
	$(SFC) -o gen_be_stage1.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o $(GEN_BE_LIB)

gen_be_stage1_1dvar.exe : gen_be_stage1_1dvar.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_stage1_1dvar.f90 > gen_be_stage1_1dvar.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_stage1_1dvar.f
	$(SFC) -o gen_be_stage1_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_1dvar.o $(GEN_BE_LIB)

gen_be_stage2.exe : gen_be_stage2.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_stage2.f90 > gen_be_stage2.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_stage2.f
	$(SFC) -o gen_be_stage2.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2.o $(GEN_BE_LIB)

gen_be_stage2_gsi.exe : gen_be_stage2_gsi.f90 
	$(CPP) $(CPPFLAGS)  gen_be_stage2_gsi.f90 > gen_be_stage2_gsi.f
	$(SFC) -c $(FCFLAGS)  gen_be_stage2_gsi.f
	$(SFC) -o gen_be_stage2_gsi.exe $(LDFLAGS)  gen_be_stage2_gsi.o 

gen_be_stage2_1dvar.exe : gen_be_stage2_1dvar.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_stage2_1dvar.f90 > gen_be_stage2_1dvar.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_stage2_1dvar.f
	$(SFC) -o gen_be_stage2_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2_1dvar.o $(GEN_BE_LIB)

gen_be_stage2a.exe : gen_be_stage2a.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_stage2a.f90 > gen_be_stage2a.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_stage2a.f
	$(SFC) -o gen_be_stage2a.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2a.o $(GEN_BE_LIB) 

gen_be_stage3.exe : gen_be_stage3.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_stage3.f90 > gen_be_stage3.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_stage3.f
	$(SFC) -o gen_be_stage3.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage3.o  $(GEN_BE_LIB)

gen_be_stage4_global.exe : gen_be_stage4_global.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_stage4_global.f90 > gen_be_stage4_global.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_stage4_global.f
	$(SFC) -o gen_be_stage4_global.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_global.o  $(GEN_BE_LIB)

gen_be_stage4_regional.exe : gen_be_stage4_regional.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_stage4_regional.f90 > gen_be_stage4_regional.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_stage4_regional.f
	$(SFC) -o gen_be_stage4_regional.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_regional.o $(GEN_BE_LIB)

gen_be_cov2d.exe : gen_be_cov2d.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_cov2d.f90 > gen_be_cov2d.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_cov2d.f
	$(SFC) -o gen_be_cov2d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d.o $(GEN_BE_LIB)

gen_be_cov3d.exe : gen_be_cov3d.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_cov3d.f90 > gen_be_cov3d.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_cov3d.f
	$(SFC) -o gen_be_cov3d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d.o $(GEN_BE_LIB)

gen_be_diags.exe : gen_be_diags.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_diags.f90 > gen_be_diags.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_diags.f
	$(SFC) -o gen_be_diags.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags.o $(GEN_BE_LIB)

gen_be_diags_read.exe : gen_be_diags_read.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_diags_read.f90 > gen_be_diags_read.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_diags_read.f
	$(SFC) -o gen_be_diags_read.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags_read.o $(GEN_BE_LIB)

gen_be_etkf.exe : gen_be_etkf.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc -I$(NETCDFPATH)/include gen_be_etkf.f90 > gen_be_etkf.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_etkf.f
	$(SFC) -o gen_be_etkf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_etkf.o $(GEN_BE_LIB)

gen_be_ensrf.exe : gen_be_ensrf.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc gen_be_ensrf.f90 > gen_be_ensrf.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_ensrf.f
	$(SFC) -o gen_be_ensrf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensrf.o $(GEN_BE_LIB)

gen_be_ensmean.exe : gen_be_ensmean.f90 $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(CPP) $(CPPFLAGS) -I$(WRF_SRC_ROOT_DIR)/inc -I$(NETCDFPATH)/include gen_be_ensmean.f90 > gen_be_ensmean.f
	$(SFC) -c $(FCFLAGS) -I../da $(MODULE_DIRS) $(WRFVAR_INC) $(PROMOTION) gen_be_ensmean.f
	$(SFC) -o gen_be_ensmean.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensmean.o $(GEN_BE_LIB)

clean: 
	@ echo 'use the clean script'

# DEPENDENCIES : $(GEN_BE_OBJS) only dependencies after this line (don't remove the word DEPENDENCIES)

