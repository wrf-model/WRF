#! /bin/csh -f

# enter the code directory
set objdir = $OBJROOT/atm/obj
set srcdir = $OBJROOT/atm/src
mkdir -p $srcdir
mkdir -p $objdir
cd $srcdir
cp -p -r $CODEROOT/atm/wrf .
cd ./wrf

# modify CCSMCASE in Registry to case name

cd ./Registry
rm -f *.hold >&! /dev/null
foreach file (Registry.* registry.*)
  mv -f ${file} ${file}.hold
  sed -e "s#CCSMCASE#$CASE#" < ${file}.hold > ${file}
end
set regfile = Registry.EM
if ($ATM_GRID =~ wr50a) then
  set regfile = Registry.EM.racm
endif
if ($ATM_GRID =~ wr10a) then
  set regfile = Registry.EM.racm
endif
if ($ATM_GRID =~ wus12) then
  set regfile = Registry.EM.resm
endif
if ($ATM_GRID =~ us20) then
  set regfile = Registry.EM.resm
endif
cp -f ${regfile} Registry.EM

cd $srcdir/wrf


# for atm_mct_comp compiler

if (-f configure.wrf.${MACH}_${COMPILER}) then
  cp -f configure.wrf.${MACH}_${COMPILER} configure.wrf
else if (-f configure.wrf.${MACH}) then
  cp -f configure.wrf.${MACH} configure.wrf
else
  echo "configure.wrf for ${MACH} ${COMPILER} not found"
  exit 0
endif
./compile em_real
echo $DEBUG

# for ccsm compiler

cd $srcdir/wrf/external/esmf_time_f90
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_netcdf
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/fftpack/fftpack5
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_grib_share
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_grib1
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_grib1/grib1_util
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_grib1/MEL_grib1
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_grib1/WGRIB
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/RSL_LITE
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/external/io_int
ar ru $srcdir/wrf/main/libwrflib.a *.o

cd $srcdir/wrf/frame
ar ru $srcdir/wrf/main/libwrflib.a module_internal_header_util.o pack_utils.o

cd $srcdir/wrf/main
ar ru $srcdir/wrf/main/libwrflib.a atm_comp_mct.o module_wrf_top.o wrf_instance.o

# esmf_wrf
cd $srcdir/wrf/external/esmf_time_f90

cp *.mod $LIBROOT/include

cd $srcdir/wrf

cp -p *.mod $objdir/
cp -p */*.mod $objdir/
cp -p */*/*.mod $objdir/
cp -p */*/*/*.mod $objdir/
cp -p $objdir/*.mod $LIBROOT/include
cp -p main/atm_comp*.mod  $LIBROOT/include
cp -p main/libwrflib.a  $LIBROOT/libatm.a


