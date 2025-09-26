if (! $?MACHINE) then
   setenv MACHINE `uname -n`
endif

if (! $?EXT_DIR) then
   setenv EXT_DIR ~wrfhelp/external
endif

if (! -d $EXT_DIR) then
   echo "Cannot find EXT_DIR=$EXT_DIR"
endif

# Search for queuing systems.
# Don't use which, as it always returns 0 on BSD
bjobs -V >& /dev/null
if ($status == 0) then
   setenv SUBMIT LSF
else
   llq >& /dev/null
   if ($status == 0) then
      setenv SUBMIT LoadLeveller
   else
      which qsub >& /dev/null
      # could be SGE of course, so might need better way to check
      if ($? == 0) then
         setenv SUBMIT PBS
      else
         setenv SUBMIT none
      endif
   endif
endif

if (! $?PROCESSOR) then
   # Unix people can't even report processor class properly
   # across different machines or between ksh/bash on Linux
   # They all need their heads banged together
   # This kludge should give powerpc/i686/i386(for intel Mac)
   if ( `uname` == "AIX" ) then
      # Thanks Aix for reporting a hex string with -m, when
      # all I wanted was powerpc
      setenv PROCESSOR `uname -p`
   else
      # Thanks Linux for either reporting nothing with -n,
      # or different values for ksh and bash, FFS
      setenv PROCESSOR `uname -m`
      if ("$PROCESSOR" == "Power Macintosh") then
         setenv PROCESSOR powerpc
      endif
   endif
endif 

if ( `uname` == "AIX" ) then
   # Brain dead Aix /bin/csh cannot handle arguments to 
   # sourced scripts, so force use of ibm
   setenv COMPILER ibm
else
   if ("$1" != "") then
      setenv COMPILER $1
   else
      setenv COMPILER gnu
   endif
endif

if ($COMPILER == g95) then
   setenv COMPILER gnu
endif

if ($COMPILER == xlf) then
   setenv COMPILER ibm
endif

if ($COMPILER == gnu) then
   setenv G95_ENDIAN BIG
endif

if ($COMPILER == cray) then
   setenv PROCESSOR x1
endif


# List options in order of increasing preference

if (-d ${EXT_DIR}/netcdf/netcdf-3.6.1/${COMPILER}_${PROCESSOR}) then
   setenv NETCDF ${EXT_DIR}/netcdf/netcdf-3.6.1/${COMPILER}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/rttov/rttov93/${COMPILER}_${PROCESSOR}) then
   setenv RTTOV ${EXT_DIR}/rttov/rttov93/${COMPILER}_${PROCESSOR}
endif
#if (-d ${EXT_DIR}/crtm/CRTM_02-29-08/${COMPILER}_${PROCESSOR}) then
#   setenv CRTM ${EXT_DIR}/crtm/CRTM_02-29-08/${COMPILER}_${PROCESSOR}
#endif
if (-d ${EXT_DIR}/crtm/CRTM_02_03_09_REL_1_2/${COMPILER}_${PROCESSOR}) then
   setenv CRTM ${EXT_DIR}/crtm/CRTM_02_03_09_REL_1_2/${COMPILER}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/mpi/mpich-1.2.7p1/${COMPILER}_${PROCESSOR}) then
   setenv MPIHOME ${EXT_DIR}/mpi/mpich-1.2.7p1/${COMPILER}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/mpi/mpich2-1.0.6p1/${COMPILER}_${PROCESSOR}) then
   setenv MPIHOME ${EXT_DIR}/mpi/mpich2-1.0.6p1/${COMPILER}_${PROCESSOR}
endif
#if (-d ${EXT_DIR}/blas/blas/${COMPILER}_${PROCESSOR}) then
#   setenv BLAS ${EXT_DIR}/blas/blas/${COMPILER}_${PROCESSOR}
#endif
#if (-d ${EXT_DIR}/lapack/lapack-3.1.1/${COMPILER}_${PROCESSOR}) then
#   setenv LAPACK ${EXT_DIR}/lapack/lapack-3.1.1/${COMPILER}_${PROCESSOR}
#endif
#if (-d ${EXT_DIR}/fftpack/fftpack5/${COMPILER}_${PROCESSOR}) then
#   setenv FFTPACK ${EXT_DIR}/fftpack/fftpack5/${COMPILER}_${PROCESSOR}
#endif
if (-d ${EXT_DIR}/bufr/bufr_ncep_nco/${COMPILER}_${PROCESSOR}) then
   setenv BUFR ${EXT_DIR}/bufr/bufr_ncep_nco/${COMPILER}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/zlib/zlib-1.2.3/${COMPILER}_${PROCESSOR}) then
   setenv ZLIB ${EXT_DIR}/zlib/zlib-1.2.3/${COMPILER}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/jpeg/jpeg-6b/${COMPILER}_${PROCESSOR}) then
   setenv JPEG ${EXT_DIR}/jpeg/jpeg-6b/${COMPILER}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/hdf/hdf4.2r1/${COMPILER}_${PROCESSOR}) then
   setenv HDF4 ${EXT_DIR}/hdf/hdf4.2r1/${COMPILER}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/hdf/hdf5-1.6.5/${COMPILER}_${PROCESSOR}) then
   setenv HDF5 ${EXT_DIR}/hdf/hdf5-1.6.5/${COMPILER}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/hdf/HDF-EOS2.14v1.00/${COMPILER}_${PROCESSOR}) then
   setenv HDFEOS ${EXT_DIR}/hdf/HDF-EOS2.14v1.00/${COMPILER}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/jasper/jasper-1.900.1/${COMPILER}_${PROCESSOR}) then
   setenv JASPER ${EXT_DIR}/jasper/jasper-1.900.1/${COMPILER}_${PROCESSOR}
endif
if (-d ${EXT_DIR}/netcdf/pnetcdf-1.0.1/${COMPILER}_${PROCESSOR}) then
   setenv PNETCDF ${EXT_DIR}/netcdf/pnetcdf-1.0.1/${COMPILER}_${PROCESSOR}
endif
#if (-d ${EXT_DIR}/madis/${COMPILER}_${PROCESSOR}) then
#   setenv MADIS ${EXT_DIR}/madis/${COMPILER}_${PROCESSOR}
#endif

if (-d /usr/lpp/ppe.poe) then
   setenv MPIHOME /usr/lpp/ppe.poe
endif

setenv LINUX_MPIHOME $MPIHOME
setenv PATH $MPIHOME/bin:$PATH


echo
if ($?PROCESSOR) then
   echo "PROCESSOR       " $PROCESSOR
endif
if ($?COMPILER) then
   echo "COMPILER        " $COMPILER       
endif
if ($?MPIHOME) then
   echo "MPIHOME         " $MPIHOME
endif
if ($?RTTOV) then
   echo "RTTOV           " $RTTOV
endif
if ($?CRTM) then
   echo "CRTM            " $CRTM
endif
if ($?NETCDF) then
   echo "NETCDF          " $NETCDF
endif
#if ($?BLAS) then
#   echo "BLAS            " $BLAS
#endif
#if ($?LAPACK) then
#   echo "LAPACK          " $LAPACK
#endif
#if ($?FFTPACK) then
#   echo "FFTPACK         " $FFTPACK
#endif
#if ($?BUFR) then
#   echo "BUFR            " $BUFR
#endif
if ($?ZLIB) then
   echo "ZLIB            " $ZLIB
endif
if ($?JPEG) then
   echo "JPEG            " $JPEG
endif
if ($?HDF4) then
   echo "HDF4            " $HDF4
endif
if ($?HDF5) then
   echo "HDF5            " $HDF5
endif
if ($?HDFEOS) then
   echo "HDFEOS          " $HDFEOS
endif
if ($?JASPER) then
   echo "JASPER          " $JASPER
endif
if ($?PNETCDF) then
   echo "PNETCDF         " $PNETCDF
endif
#if ($?MADIS) then
#   echo "MADIS           " $MADIS
#endif
if ($?SUBMIT) then
   echo "SUBMIT          " $SUBMIT
endif
if ($?SUBMIT_OPTIONS1) then
   echo "SUBMIT_OPTIONS1  $SUBMIT_OPTIONS1"
endif
if ($?SUBMIT_OPTIONS2) then
   echo "SUBMIT_OPTIONS2  $SUBMIT_OPTIONS2"
endif
if ($?SUBMIT_OPTIONS3) then
   echo "SUBMIT_OPTIONS3  $SUBMIT_OPTIONS3"
endif
if ($?SUBMIT_OPTIONS4) then
   echo "SUBMIT_OPTIONS4  $SUBMIT_OPTIONS4"
endif
if ($?SUBMIT_OPTIONS5) then
   echo "SUBMIT_OPTIONS5  $SUBMIT_OPTIONS5"
endif
if ($?SUBMIT_OPTIONS6) then
   echo "SUBMIT_OPTIONS6  $SUBMIT_OPTIONS6"
endif
if ($?SUBMIT_OPTIONS7) then
   echo "SUBMIT_OPTIONS7  $SUBMIT_OPTIONS7"
endif
if ($?SUBMIT_OPTIONS8) then
   echo "SUBMIT_OPTIONS8  $SUBMIT_OPTIONS8"
endif
if ($?SUBMIT_OPTIONS9) then
   echo "SUBMIT_OPTIONS9  $SUBMIT_OPTIONS9"
endif
if ($?SUBMIT_OPTIONS10) then
   echo "SUBMIT_OPTIONS10 $SUBMIT_OPTIONS10"
endif
if ($?SUBMIT_WAIT_FLAG) then
   echo "SUBMIT_WAIT_FLAG $SUBMIT_WAIT_FLAG"
endif

if ($COMPILER == cray) then
   # Cray use COMPILER for their own purposes, so reset
   unsetenv COMPILER
endif
