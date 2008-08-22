# gen_be

GEN_BE_OBJS = da_etkf.o \
	da_gen_be.o \
	da_control.o \
	da_be_spectral.o \
	module_wrf_error.o \
	module_driver_constants.o \
	da_memory.o \
	da_reporting.o \
	da_tools_serial.o \
	module_ffts.o

GEN_BE_LIBS = $(WRF_SRC_ROOT_DIR)/external/io_netcdf/libwrfio_nf.a 

be : setup \
	gen_be_stage0_wrf.exe \
	gen_be_ep1.exe \
	gen_be_ep2.exe \
	gen_be_stage1.exe \
	gen_be_stage1_1dvar.exe	\
	gen_be_stage2.exe \
	gen_be_stage2_1dvar.exe \
	gen_be_stage2a.exe \
	gen_be_stage3.exe \
	gen_be_stage4_global.exe \
	gen_be_stage4_regional.exe \
	gen_be_cov2d.exe \
	gen_be_cov3d.exe \
	gen_be_diags.exe \
	gen_be_diags_read.exe \
	gen_be_ensrf.exe \
	gen_be_etkf.exe \
	gen_be_ensmean.exe \
	da_advance_time.exe

EN_BE_LIBS = $(WRF_SRC_ROOT_DIR)/external/io_netcdf/libwrfio_nf.a
GEN_BE_LIB = -L$(LAPACK) -llapack -L$(BLAS) -lblas -L$(NETCDF)/lib -lnetcdf -L$(WRF_SRC_ROOT_DIR)/external/fftpack/fftpack5 -lfftpack


GEN_BE_OBJS = ./da_etkf.o ./da_gen_be.o ./da_control.o ./da_be_spectral.o \
  ./module_wrf_error.o ./da_reporting.o ./module_ffts.o \
  ./module_driver_constants.o ./da_memory.o ./da_tools_serial.o

gen_be_stage0_wrf.exe : gen_be_stage0_wrf.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_stage0_wrf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage0_wrf.o $(GEN_BE_LIB)

gen_be_ep1.exe     : gen_be_ep1.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_ep1.exe $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_ep1.o $(GEN_BE_LIB)

gen_be_ep2.exe     : gen_be_ep2.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_ep2.exe $(LDFLAGS) $(GEN_BE_OBJS)  gen_be_ep2.o $(GEN_BE_LIB)

gen_be_stage1.exe : gen_be_stage1.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_stage1.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1.o $(GEN_BE_LIB)

gen_be_stage1_1dvar.exe : gen_be_stage1_1dvar.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_stage1_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage1_1dvar.o $(GEN_BE_LIB)

gen_be_stage2.exe : gen_be_stage2.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_stage2.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2.o $(GEN_BE_LIB)

gen_be_stage2_1dvar.exe : gen_be_stage2_1dvar.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_stage2_1dvar.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2_1dvar.o $(GEN_BE_LIB)

gen_be_stage2a.exe : gen_be_stage2a.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_stage2a.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage2a.o $(GEN_BE_LIB) 

gen_be_stage3.exe : gen_be_stage3.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_stage3.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage3.o  $(GEN_BE_LIB)

gen_be_stage4_global.exe : gen_be_stage4_global.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_stage4_global.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_global.o  $(GEN_BE_LIB)

gen_be_stage4_regional.exe : gen_be_stage4_regional.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_stage4_regional.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_stage4_regional.o $(GEN_BE_LIB)

gen_be_cov2d.exe : gen_be_cov2d.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_cov2d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov2d.o $(GEN_BE_LIB)

gen_be_cov3d.exe : gen_be_cov3d.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_cov3d.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_cov3d.o $(GEN_BE_LIB)

gen_be_diags.exe : gen_be_diags.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_diags.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags.o $(GEN_BE_LIB)

gen_be_diags_read.exe : gen_be_diags_read.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_diags_read.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_diags_read.o $(GEN_BE_LIB)

gen_be_etkf.exe : gen_be_etkf.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_etkf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_etkf.o $(GEN_BE_LIB)

gen_be_ensrf.exe : gen_be_ensrf.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_ensrf.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensrf.o $(GEN_BE_LIB)

gen_be_ensmean.exe : gen_be_ensmean.o $(GEN_BE_OBJS) $(GEN_BE_LIBS)
	$(SFC) -o gen_be_ensmean.exe $(LDFLAGS) $(GEN_BE_OBJS) gen_be_ensmean.o $(GEN_BE_LIB)
