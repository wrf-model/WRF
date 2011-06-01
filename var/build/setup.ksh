MACHINE=${MACHINE:-`uname -n`}

export EXT_DIR=${EXT_DIR:-~wrfhelp/external}

if ! test -d $EXT_DIR; then
   echo "No directory EXT_DIR=$EXT_DIR"
fi

export COMPILER=${1:-gnu}

if test $COMPILER = g95; then export COMPILER=gnu; fi
if test $COMPILER = xlf; then export COMPILER=ibm; fi

# Search for queuing systems.
# Don't use which, as it always returns 0 on BSD
bjobs -V > /dev/null 2>&1
if test $? = 0 ; then
   export SUBMIT=LSF
else
   llq > /dev/null 2>&1
   if test $? = 0 ; then
      export SUBMIT=LoadLeveller
   else
      csh -c "which qsub" >/dev/null 2>&1
      # could be SGE of course, so need better way to check
      if test $? = 0; then
         export SUBMIT=PBS
      else
         export SUBMIT=none
      fi
   fi
fi

# Unix people can't report processor class properly
# across different machines or between ksh/bash on Linux
# They all need their heads banged together
# This kludge should give powerpc/i686/i386(for intel Mac)

if test `uname` = "AIX"; then
   # Thanks Aix for reporting a hex string with -m, when
   # all I wanted was powerpc
   export PROCESSOR=${PROCESSOR:-`uname -p`}
else
   # Thanks Linux for either reporting nothing with -n,
   # or different values for ksh and bash, FFS
   export PROCESSOR=${PROCESSOR:-`uname -m`}
fi
if test "$PROCESSOR" = "Power Macintosh"; then
   export PROCESSOR=powerpc
fi

if test $COMPILER = gnu; then
   export G95_ENDIAN=BIG
fi

if test $COMPILER = cray; then
   export PROCESSOR=x1
fi

if test -d ${EXT_DIR}/netcdf/netcdf-3.6.1/${COMPILER}_${PROCESSOR}; then
  export NETCDF=${EXT_DIR}/netcdf/netcdf-3.6.1/${COMPILER}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/rttov/rttov93/${COMPILER}_${PROCESSOR}; then
   export RTTOV=${EXT_DIR}/rttov/rttov93/${COMPILER}_${PROCESSOR}
fi
#if test -d ${EXT_DIR}/crtm/CRTM_02-29-08/${COMPILER}_${PROCESSOR}; then
#   export CRTM=${EXT_DIR}/crtm/CRTM_02-29-08/${COMPILER}_${PROCESSOR}
#fi
if test -d ${EXT_DIR}/crtm/CRTM_02_03_09_REL_1_2/${COMPILER}_${PROCESSOR}; then
   export CRTM=${EXT_DIR}/crtm/CRTM_02_03_09_REL_1_2/${COMPILER}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/mpi/mpich-1.2.7p1/${COMPILER}_${PROCESSOR}; then
   export MPIHOME=${EXT_DIR}/mpi/mpich-1.2.7p1/${COMPILER}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/mpi/mpich2-1.0.6p1/${COMPILER}_${PROCESSOR}; then
   export MPIHOME=${EXT_DIR}/mpi/mpich2-1.0.6p1/${COMPILER}_${PROCESSOR}
fi
#if test -d ${EXT_DIR}/blas/blas/${COMPILER}_${PROCESSOR}; then
#   export BLAS=${EXT_DIR}/blas/blas/${COMPILER}_${PROCESSOR}
#fi
#if test -d ${EXT_DIR}/lapack/lapack-3.1.1/${COMPILER}_${PROCESSOR}; then
#   export LAPACK=${EXT_DIR}/lapack/lapack-3.1.1/${COMPILER}_${PROCESSOR}
#fi
#if test -d ${EXT_DIR}/fftpack/fftpack5/${COMPILER}_${PROCESSOR}; then
#   export FFTPACK=${EXT_DIR}/fftpack/fftpack5/${COMPILER}_${PROCESSOR}
#fi
if test -d ${EXT_DIR}/bufr/bufr_ncep_nco/${COMPILER}_${PROCESSOR}; then
   export BUFR=${EXT_DIR}/bufr/bufr_ncep_nco/${COMPILER}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/zlib/zlib-1.2.3/${COMPILER}_${PROCESSOR}; then
   export ZLIB=${EXT_DIR}/zlib/zlib-1.2.3/${COMPILER}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/jpeg/jpeg-6b/${COMPILER}_${PROCESSOR}; then
   export JPEG=${EXT_DIR}/jpeg/jpeg-6b/${COMPILER}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/hdf/hdf4.2r1/${COMPILER}_${PROCESSOR}; then
   export HDF4=${EXT_DIR}/hdf/hdf4.2r1/${COMPILER}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/hdf/hdf5-1.6.5/${COMPILER}_${PROCESSOR}; then
   export HDF5=${EXT_DIR}/hdf/hdf5-1.6.5/${COMPILER}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/hdf/HDF-EOS2.14v1.00/${COMPILER}_${PROCESSOR}; then
   export HDFEOS=${EXT_DIR}/hdf/HDF-EOS2.14v1.00/${COMPILER}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/jasper/jasper-1.900.1/${COMPILER}_${PROCESSOR}; then
   export JASPER=${EXT_DIR}/jasper/jasper-1.900.1/${COMPILER}_${PROCESSOR}
fi
if test -d ${EXT_DIR}/netcdf/pnetcdf-1.0.1/${COMPILER}_${PROCESSOR}; then
   export PNETCDF=${EXT_DIR}/netcdf/pnetcdf-1.0.1/${COMPILER}_${PROCESSOR}
fi
#if test -d ${EXT_DIR}/madis/${COMPILER}_${PROCESSOR}; then
#   export MADIS=${EXT_DIR}/madis/${COMPILER}_${PROCESSOR}
#fi

if test -d /usr/lpp/ppe.poe; then
   export MPIHOME=/usr/lpp/ppe.poe
fi

export LINUX_MPIHOME=$MPIHOME
export MANPATH=$MPIHOME/man:$MANPATH

echo "PROCESSOR       " $PROCESSOR
echo "COMPILER        " $COMPILER
echo "MPIHOME         " $MPIHOME
echo "RTTOV           " $RTTOV
echo "CRTM            " $CRTM
echo "NETCDF          " $NETCDF
#echo "BLAS            " $BLAS
#echo "LAPACK          " $LAPACK
#echo "FFTPACK         " $FFTPACK
#echo "BUFR            " $BUFR
echo "ZLIB            " $ZLIB        
echo "JPEG            " $JPEG        
echo "HDF4            " $HDF4        
echo "HDF5            " $HDF5        
echo "HDFEOS          " $HDFEOS
echo "JASPER          " $JASPER      
echo "PNETCDF         " $PNETCDF     
#echo "MADIS           " $MADIS     
echo "SUBMIT          " $SUBMIT
if test "$SUBMIT_OPTIONS1." != '.'; then
   echo "SUBMIT_OPTIONS1  $SUBMIT_OPTIONS1"
fi
if test "$SUBMIT_OPTIONS2." != '.'; then
   echo "SUBMIT_OPTIONS2  $SUBMIT_OPTIONS2"
fi
if test "$SUBMIT_OPTIONS3." != '.'; then
   echo "SUBMIT_OPTIONS3  $SUBMIT_OPTIONS3"
fi
if test "$SUBMIT_OPTIONS4." != '.'; then
   echo "SUBMIT_OPTIONS4  $SUBMIT_OPTIONS4"
fi
if test "$SUBMIT_OPTIONS5." != '.'; then
   echo "SUBMIT_OPTIONS5  $SUBMIT_OPTIONS5"
fi
if test "$SUBMIT_OPTIONS6." != '.'; then
   echo "SUBMIT_OPTIONS6  $SUBMIT_OPTIONS6"
fi
if test "$SUBMIT_OPTIONS7." != '.'; then
   echo "SUBMIT_OPTIONS7  $SUBMIT_OPTIONS7"
fi
if test "$SUBMIT_OPTIONS8." != '.'; then
   echo "SUBMIT_OPTIONS8  $SUBMIT_OPTIONS8"
fi
if test "$SUBMIT_OPTIONS9." != '.'; then
   echo "SUBMIT_OPTIONS9  $SUBMIT_OPTIONS9"
fi
if test "$SUBMIT_OPTIONS10." != '.'; then
   echo "SUBMIT_OPTIONS10 $SUBMIT_OPTIONS10"
fi
if test "$SUBMIT_WAIT_FLAG." != '.'; then
   echo "SUBMIT_WAIT_FLAG $SUBMIT_WAIT_FLAG"
fi

if [[ $COMPILER == cray ]]; then
   # Cray use environment variable for their own purposes, so unset
   unset COMPILER
fi
