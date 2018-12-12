#!/bin/sh

# parse argument list

thiscmd=$0

FORTRAN_COMPILER_TIMER=""
opt_level="-f"
rword="-r4"
print_usage=""
chemistry=""
wrf_core=""
config_line="$0 "
while [ $# -ge 1 ]; do
    config_line="$config_line $1"
    case $1 in
      -d) opt_level="-d" ;;
      -D) opt_level="-D" ;;
      -s) opt_level="-s" ;;
      -f) opt_level="-f" ;;
      -h) print_usage="yes" ;;
      -help) print_usage="yes" ;;
      -os) shift ; WRF_OS=$1 ;;
      -mach) shift ; WRF_MACH=$1 ;;
      -r8) rword="-r8" ;;
      -time) shift ; FORTRAN_COMPILER_TIMER=$1 ;;
      chem) WRF_CHEM=1  ;;
      kpp) WRF_KPP=1  ;;
      radardfi) WRF_DFI_RADAR=1  ;;
      wrfda) wrf_core=DA_CORE ;;
      4dvar) wrf_core=4D_DA_CORE ;;
      arw) wrf_core=EM_CORE  ;;
      nmm) wrf_core=NMM_CORE  ;;
      coamps) wrf_core=COAMPS_CORE  ;;
      titan) WRF_TITAN=1 ; break ;;
      mars) WRF_MARS=1 ; break ;;
      venus) WRF_VENUS=1 ; break ;;
      wrfplus) wrf_core=WRF_PLUS_CORE ; break ;;
      *) echo "This option is not recognized: $1" ; exit 2 ;;
    esac
    shift
done
if [ -n "$print_usage" ] ; then
  echo ' '
  echo '*****************************************************************************'
  echo usage: $thiscmd '[-d|-D|-s|-f|-os os|-mach mach|-time timecommand] [chem] [kpp]'
  echo '-d    build with debugging information and no optimization'
  echo '-D    build with -d AND floating traps, traceback, uninitialized variables'
  echo '-r8   build with 8 byte reals'
  echo '-help print this message'
  echo '*****************************************************************************'
  echo ' '
  exit 
fi

if `pwd | grep ' ' > /dev/null ` ; then
  echo '************************** W A R N I N G ************************************'
  echo The current working directory has spaces in some components of its path name
  echo and this may cause problems for your build.  This can occur, for example, on
  echo Windows systems.  It is strongly recommended that you install WRF and other
  echo related software such as NetCDF in directories whose path names contain no
  echo white space.  On Win, for example, create and install in a directory under C:.
  echo '*****************************************************************************'
fi


# lifted from the configure file for mpich; 00/03/10 jm
#
# Check for perl and perl version
for p in perl5 perl
do
  # Extract the first word of "$p", so it can be a program name with args.
  set dummy $p; ac_word=$2
  if test -z "$ac_echo_n" ; then
    ac_echo_n=yes
    if (echo "testing\c"; echo 1,2,3) | grep c >/dev/null; then
      # Stardent Vistra SVR4 grep lacks -e, says ghazi@caip.rutgers.edu.
      if (echo -n testing; echo 1,2,3) | sed s/-n/xn/ | grep xn >/dev/null; then
    ac_n= ac_c='
' ac_t='        '
      else
        ac_n=-n ac_c= ac_t=
      fi
    else
      ac_n= ac_c='\c' ac_t=
    fi
    ac_echo_test=`echo foo 1>&1`
    if test -z "$ac_echo_test" ; then
       print_error "Your sh shell does not handle the output redirection"
       print_error "1>&1 correctly.  Configure will work around this problem,"
       print_error "but you should report the problem to your vendor."
    fi
  fi
  if test -z "$ac_echo_test" -a 1 = 1 ; then
    echo $ac_n "checking for $ac_word""... $ac_c"
  else
    echo $ac_n "checking for $ac_word""... $ac_c" 1>&1
  fi
  ac_prog_where=""
  if test -n "$PERL"; then
    ac_pg_PERL="$PERL" # Let the user override the test.
  else
    ac_first_char=`expr "$p" : "\(.\)"`
    if test "$ac_first_char" = "/" -a -x "$p" ; then
         ac_pg_PERL="$p"
         ac_prog_where=$p
    else
        IFS="${IFS=     }"; ac_save_ifs="$IFS"; IFS="${IFS}:"
        for ac_dir in $PATH; do
          test -z "$ac_dir" && ac_dir=.
          if test -f $ac_dir/$ac_word; then
            ac_pg_PERL="$p"
            ac_prog_where=$ac_dir/$ac_word
            break
          fi
        done
        IFS="$ac_save_ifs"
    fi
  fi;PERL="$ac_pg_PERL"
  if test -n "$ac_prog_where" ; then
    if test -z "$ac_echo_test" -a 1 = 1 ; then
      echo "$ac_t""found $ac_prog_where ($PERL)"
    else
      echo "$ac_t""found $ac_prog_where ($PERL)" 1>&1
    fi
    PERLFULLPATH=$ac_prog_where  
  else
    if test -z "$ac_echo_test" -a 1 = 1 ; then
      echo "$ac_t""no"
    else
      echo "$ac_t""no" 1>&1
    fi
  fi
  test -n "$PERL" && break
done

if test -z "$PERL" ; then
    # We have to set this outside of the loop lest the first failure in 
    # PROGRAM_CHECK set the value (which then terminates the effect of the
    # loop, since autoconf macros only set values that are null, they 
    # don't override them
    PERL=""
fi

if test -n "$PERL" ; then
    PERL="$PERL"
    
    perlversion=`$PERL -v | grep 'This is perl' | \
       sed -e 's/^.*v[a-z ]*\([0-9]\).*$/\1/'`

    # Should do a test first for ch_p4 etc.
    if test "$perlversion" -lt 5 ; then
        echo "WRF build requires perl version 5, which configure did not find."
        echo "You can set the environment variable PERL to contain the "
        echo "location of perl version 5."
        echo "Configure believes that $PERL is version $perlversion ."
        PERL=""
    fi
    
fi

if  test -z "$NETCDF"  ; then
  echo ' '
  echo '*****************************************************************************'
  echo 'No environment variable NETCDF set.'
  echo 'Stopping'
  echo '*****************************************************************************'
  echo ' '
  exit 5
fi

if  test -z "$NETCDF_classic"  ; then
  export NETCDF4=1
else
  unset NETCDF4
fi

USENETCDFF=""
USENETCDF=""
if [ -n "$NETCDF" ] ; then
  echo "Will use NETCDF in dir: $NETCDF"
# Oh UNIDATA, why make it so hard ...
  if [ -f "$NETCDF/lib/libnetcdff.a" -o -f "$NETCDF/lib/libnetcdff.so" ] ; then
    USENETCDFF="-lnetcdff"
  else
    USENETCDFF=" "
  fi
  if [ -f "$NETCDF/lib/libnetcdf.a" -o -f "$NETCDF/lib/libnetcdf.so" ] ; then
    USENETCDF="-lnetcdf"
  else
    USENETCDF=" "
  fi
  export USENETCDF=$USENETCDF
  export USENETCDFF=$USENETCDFF
else
  echo ' '
  echo '*****************************************************************************'
  echo 'No environment variable NETCDF set.'
  echo 'Stopping'
  echo '*****************************************************************************'
  echo ' '
  exit 6
fi

# If the user asked for classic netcdf, acquiesce to the request.

if [ "`uname`" = "Linux" ] ; then
  ans="`whereis nf-config`"
elif [ "`uname`" = "Darwin" ] ; then
  ans="`which nf-config`"
else
  ans=""
# echo "Add in your architecture's uname and the command to find executables in the path"
# exit 1
fi
if [ "$ans" = "nf-config:" -o "$ans" = "" ] ; then
    export NETCDF_classic=1
    unset NETCDF4
else
  if [ -n "$NETCDF_classic" ] ; then
    if [ $NETCDF_classic -eq 1 ] ; then
      unset  NETCDF4
    else
      unset  NETCDF_classic
      export NETCDF4=1
    fi
  else
    if [ "`nf-config --has-nc4`" = "yes" ] ; then
      unset  NETCDF_classic
      export NETCDF4=1
    else
      export NETCDF_classic=1
      unset  NETCDF4
    fi
  fi
fi

if [ -z "$HDF5_PATH" ] ; then HDF5_PATH=''; fi
if [ -z "$ZLIB_PATH" ] ; then ZLIB_PATH=''; fi
if [ -z "$GPFS_PATH" ] ; then GPFS_PATH=''; fi
if [ -z "$CURL_PATH" ] ; then CURL_PATH=''; fi

if [ -n "$NETCDF4" ] ; then
  if [ $NETCDF4 -eq 1 ] ; then
    DEP_LIB_PATH=''
    if [ -f $NETCDF/bin/nf-config ] ; then
      nx_config="$NETCDF/bin/nf-config --flibs"
      DEP_LIB_PATH="`$nx_config | awk '{for(i=1;i<=NF;i++){if(match($i, /-L.*/)) {print $i} } }'`"
      CURL="`$nx_config | awk '{for(i=1;i<=NF;i++){if($i == "-lcurl") {print $i} } }'`"
      GPFS="`$nx_config | awk '{for(i=1;i<=NF;i++){if($i == "-lgpfs") {print $i} } }'`"
    fi
    if [ "$DEP_LIB_PATH" = '' ] ; then
      if [ -f $NETCDF/bin/nc-config ] ; then
        nx_config="$NETCDF/bin/nc-config --libs"
        DEP_LIB_PATH="`$nx_config | awk '{for(i=1;i<=NF;i++){if(match($i, /-L.*/)) {print $i} } }'`"
        CURL="`$nx_config | awk '{for(i=1;i<=NF;i++){if($i == "-lcurl") {print $i} } }'`"
        GPFS="`$nx_config | awk '{for(i=1;i<=NF;i++){if($i == "-lgpfs") {print $i} } }'`"
        if [ "$CURL" != '' -a  "$CURL_PATH" = '' ] ; then
           CURL_PATH="DEFAULT"
        fi
        if [ "$GPFS" != '' -a  "$GPFS_PATH" = '' ] ; then
           GPFS_PATH="DEFAULT"
        fi
      fi
    fi
    for P in "$HDF5_PATH" "$ZLIB_PATH" "$GPFS_PATH" "$CURL_PATH"
    do
      if [ "$P" != '' -a "$P" != "DEFAULT" ] ; then 
        if [ "${P#${P%?}}" = "/" ] ; then
          P=`echo $P | sed 's/\/$//'`
        fi
        DEP_LIB_PATH="`echo $DEP_LIB_PATH | awk -v VAR=-L$P/lib '{for(i=1;i<=NF;i++){if ($i != VAR ) {print $i} } }'`"
        DEP_LIB_PATH="$DEP_LIB_PATH -L$P/lib"
      fi
    done
    if [ "${NETCDF#${NETCDF%?}}" = "/" ] ; then
      NETCDF=`echo $NETCDF | sed 's/\/$//'`
    fi
    buff="`ls -l $NETCDF | sed 's/  */ /g'`"
    while [ "`echo $buff | grep lib`" = "" -a "`echo $buff | grep '\->'`" != ""  ]
    do
      buff="`echo $buff | sed -e 's/->//' -e 's/  */ /g'`"
      n=`echo $buff | wc -w`
      lastword=`echo "$buff" | cut -d' ' -f$n | sed 's/\/$//'`
      c="`echo $lastword | cut -c1`"
      if [ "$c" == "/" ] ; then
         NETCDF=$lastword
      else
        c="`echo $lastword | cut -c1-2`"
        if [ "$c" = "./" ] ; then
          lastword="echo $lastword | cut -c3-"
        fi
        NETCDF=${NETCDF%/*}/$lastword
      fi
      buff="`ls -l $NETCDF | sed 's/  */ /g'`"
    done 
    if [ "`echo $NETCDF | grep '..'`"  != "" ] ; then
      buff="`echo $NETCDF | sed -e 's/^\///' -e 's/\// /g'`"
      n=`echo $buff | wc -w`
      i=1
      while [ $i -le $n ]
      do
        if [ "`echo $buff | cut -d' ' -f$i`" = ".." ] ; then
          m=$i 
        fi
        i=$(( $i + 1 ))
      done
      m=$(( $m + 1 ))
      NETCDF=/"`echo $buff | cut -d' ' -f${m}- | sed 's/  */\//g'`"
    fi
    if [ "${DEP_LIB_PATH#${DEP_LIB_PATH%?}}" = "/" ] ; then
      DEP_LIB_PATH=`echo $DEP_LIB_PATH | sed 's/\/$//'`
    fi
    DEP_LIB_PATH="`echo $DEP_LIB_PATH | awk -v VAR=-L$NETCDF/lib '{for(i=1;i<=NF;i++){if ($i != VAR ) {print $i} } }'`"
  fi
fi

if [ -n "$PNETCDF" ] ; then
  echo "Will use PNETCDF in dir: $PNETCDF"
# experimental, so don't tease the user if it is not there
#else
#  echo "Will configure for use without NetCDF"
fi

if [ -n "$HDF5" ] ; then
  echo "Will use HDF5 in dir: $HDF5"
else
  echo "HDF5 not set in environment. Will configure WRF for use without."
fi

if [ -n "$PHDF5" ] ; then
  echo "Will use PHDF5 in dir: $PHDF5"
else
  echo "PHDF5 not set in environment. Will configure WRF for use without."
fi

if [ "$wrf_core" = "DA_CORE" -o "$wrf_core" = "4D_DA_CORE" ] ; then
  if [ -n "$RTTOV" ] ; then
    echo "Will use RTTOV in dir: $RTTOV"
  else
    echo "RTTOV not set in environment. Will configure WRFDA for use without."
  fi
fi

# Set the "traditional" flag and the "cpp" flags

TFL="-traditional-cpp"
CFL="-P -nostdinc"

if [ "$wrf_core" = "4D_DA_CORE" ]; then
   if [ -n "$WRFPLUS_DIR" ] ; then
      echo "Will use WRFPLUS in dir: $WRFPLUS_DIR"
   else
      echo "WRFPLUS_DIR not set in environment. Please compile WRFPLUS and set WRFPLUS_DIR."
      exit
   fi
else
   if [ -n "$WRFPLUS_DIR" ] ; then
      echo 'Unsetting "$WRFPLUS_DIR" environment variable. Use "configure 4dvar" to configure for 4dvar compilation.'
      unset WRFPLUS_DIR
   fi
fi
# Users who are cross-compiling can set environment variable 
# $WRF_OS to override the value normally obtained from `uname`.  
# If $WRF_OS is set, then $WRF_MACH can also be set to override 
# the value normally obtained from `uname -m`.  If $WRF_OS is 
# set and $WRF_MACH is not set, then $WRF_MACH defaults to "ARCH".  
# If $WRF_OS is not set then $WRF_MACH is ignored.  
if [ -n "$WRF_OS" ] ; then
  echo "${0}:  WRF operating system set to \"${WRF_OS}\" via environment variable \$WRF_OS"
  os=$WRF_OS
  mach="ARCH"
  if [ -n "$WRF_MACH" ] ; then
    echo "${0}:  WRF machine set to \"${WRF_MACH}\" via environment variable \$WRF_MACH"
    mach=$WRF_MACH
  fi
else
  # if the uname command exists, give it a shot and see if
  # we can narrow the choices; otherwise, spam 'em
  os="ARCH"
  mach="ARCH"
  type uname > /dev/null
  if [ $? -eq 0 ] ; then
    os=`uname`
    if [ "$os" = "AIX" -o "$os" = "IRIX" -o "$os" = "IRIX64" -o "$os" = "SunOS" -o "$os" = "HP-UX"  -o "$os" = "Darwin" -o "$os" = "Interix" ] ; then
      mach="ARCH"
    else
      xxx=`expr "$os" : '\(.........\).*'`
      if [ "$xxx"  = "CYGWIN_NT" ] ; then
        os=$xxx
      fi
      if [ "$os" = "OSF1" -o "$os" = "Linux" -o "$os" = "UNICOS/mp" -o "$os" = "UNIX_System_V" -o  "$os" = "CYGWIN_NT" ] ; then
        mach=`uname -m`
        if [ "$mach" = "ia64" -a -f /etc/sgi-release ] ; then
          mach="Altix"
        fi
      else
        os="ARCH"
        mach="ARCH"
      fi
    fi
  fi
fi

# an IBM specific hack to adjust the bmaxstack and bmaxdata options if addressing is 32-bit
if [ "$os" = "AIX" ] ; then
      if [ -z "$OBJECT_MODE" ] ; then
         OBJECT_MODE=32
         export OBJECT_MODE
      fi
      if [ "$OBJECT_MODE" = "32" ] ; then
# the bang means nothing to sh in this context; use to represent spaces (perl will unbang)
         ldflags=-bmaxstack:256000000!-bmaxdata:2048000000
      fi
fi

# compile options that come from the environment, such as chemistry
# the "!" is removed by Config.pl
if [ -n "$WRF_HYDRO" ] ; then
  if [ $WRF_HYDRO = 1 ] ; then
      echo building WRF-HYDRO 
      compileflags="${compileflags}!-DWRF_HYDRO"
      echo $compileflags
  fi
fi

# compile options that come from the environment, such as chemistry
# the "!" is removed by Config.pl
if [ -n "$WRF_MARS" ] ; then
  if [ $WRF_MARS = 1 ] ; then
    echo building WRF for Mars
    compileflags="${compileflags}!-DPLANET!-DMARS"
    echo $compileflags
  fi
fi

if [ -n "$WRF_TITAN" ] ; then
  if [ $WRF_TITAN = 1 ] ; then
    echo building WRF for Titan
    compileflags="${compileflags}!-DPLANET!-DTITAN"
  fi
fi

if [ -n "$WRF_VENUS" ] ; then
  if [ $WRF_VENUS = 1 ] ; then
    echo building WRF for Venus
    compileflags="${compileflags}!-DPLANET!-DVENUS"
  fi
fi
if [ -n "$WRF_QUIETLY" ]; then
  echo WRF_QUIETLY is now a synonym for WRF_LOG_BUFFERING
  echo setting WRF_LOG_BUFFERING to 1...
  export WRF_LOG_BUFFERING=1
fi
if [ -n "$WRF_LOG_BUFFERING" ]; then
  if [ $WRF_LOG_BUFFERING = 1 ]; then
    echo building WRF with support for buffering of log messages
    compileflags="${compileflags}!-DWRF_LOG_BUFFERING=1"
  fi
fi
if [ -n "$PNETCDF_QUILT" ]; then
  echo Enabling quilt_pnc I/O server implementation.
  compileflags="${compileflags}!-DPNETCDF_QUILT=1"
fi
if [ -n "$WRF_NMM_CORE" ]; then
  if [ $WRF_NMM_CORE = 1 ]; then
    export WRF_NMM_NEST=1
    if [ -n "$HWRF" ]; then
      if [ $HWRF = 1 ]; then
        echo building WRF with HWRF option
        compileflags="${compileflags}!-DHWRF=1"
        if [ -n "$IDEAL_NMM_TC" ]; then
          echo building WRF with NMM Idealized Tropical Cyclone option
          compileflags="${compileflags}!-DIDEAL_NMM_TC=1"
        fi
      fi
    fi
    if [ -n "$IBM_REDUCE_BUG_WORKAROUND" ]; then
      if [ $IBM_REDUCE_BUG_WORKAROUND = 1 ]; then
        echo adding IBM_REDUCE_BUG_WORKAROUND flag for some IBM systems
        compileflags="${compileflags}!-DIBM_REDUCE_BUG_WORKAROUND"
      fi
    fi
  fi
fi
if [ -n "$WRF_DFI_RADAR" ] ; then
  if [ $WRF_DFI_RADAR = 1 ] ; then
    echo building WRF with radar dfi option
    compileflags="${compileflags}!-DWRF_DFI_RADAR=1"
  fi
fi
if [ -n "$WRF_CHEM" ] ; then
  if [ $WRF_CHEM = 1 ] ; then
    echo building WRF with chemistry option
    compileflags="${compileflags}!-DWRF_CHEM!-DBUILD_CHEM=1"
    if [ -n "$WRF_KPP" ] ; then
      if [ $WRF_KPP = 1 ] ; then    
        echo building WRF with KPP chemistry option
        compileflags="${compileflags}!-DWRF_KPP"
      fi
    fi 
  else
    compileflags="${compileflags} "
  fi
else
  compileflags="${compileflags} "
fi

if [ -n "$WRFPLUS" ] ; then
  if [ $WRFPLUS = 1 ] ; then
    echo building WRF for TL and AD
    compileflags="!-DWRFPLUS"
    echo $compileflags
  fi
fi

if [ -n "$WRF_NMM_CORE" -a -n "$WRF_CHEM" ]; then
  if [ $WRF_NMM_CORE = 1  -a  $WRF_CHEM = 1 ]; then
    echo
    echo "NMM is no longer compatible with the Chemistry option."
    echo
    # alphabetically: c=3, o=15, so co2 = 3+15+2 = 20
    exit 20
  fi
fi

type m4 > /dev/null
if [ $? -ne 0 ] ; then
  echo
  echo "ERROR ERROR ERROR ERROR ERROR ERROR ERROR"
  echo "'m4' utility not found! Can not configure."
  echo
  echo "If on an Ubuntu machine, use the command"
  echo "         sudo apt-get install m4        "
  echo "To download and install the 'm4' utility"
  exit 1
fi

if command -v timex > /dev/null 2>&1; then
  FORTRAN_COMPILER_TIMER=timex
  echo "Will use 'timex' to report timing information"
elif command -v time > /dev/null 2>&1; then
  FORTRAN_COMPILER_TIMER=time
  echo "Will use 'time' to report timing information"
fi

# Found perl, so proceed with configuration
if test -n "$PERL" ; then
   srch=`grep -i "^#ARCH.*$os" arch/configure.defaults | grep -i "$mach"`
   if [ -n "$srch" ] ; then
     $PERL arch/Config.pl -dmparallel=$COMMLIB -ompparallel=$OMP -perl=$PERL \
          -netcdf=$NETCDF -pnetcdf=$PNETCDF -hdf5=$HDF5 -phdf5=$PHDF5 -os=$os -mach=$mach -ldflags=$ldflags \
          -compileflags=$compileflags -opt_level=$opt_level -USENETCDFF=$USENETCDFF -USENETCDF=$USENETCDF \
          -time=$FORTRAN_COMPILER_TIMER -tfl="$TFL" -cfl="$CFL" -config_line="$config_line" \
          -wrf_core=$wrf_core -gpfs=$GPFS_PATH -curl=$CURL_PATH -dep_lib_path="$DEP_LIB_PATH"
     if test ! -f configure.wrf ; then
       echo "configure.wrf not created! Exiting configure script..."
       exit 1
     fi
     if [ "$opt_level" = "-d" ] ; then
        sed -e 's/FCOPTIM[	 ]*=/& # /' -e '/FCDEBUG[	 ]*=/s/#//' configure.wrf > configure.wrf.edit
        /bin/mv configure.wrf.edit configure.wrf
     fi
     if [ "$opt_level" = "-D" ] ; then
        sed -e 's/FCOPTIM[	 ]*=/& # /' -e '/FCDEBUG[	 ]*=/s/#//g' configure.wrf > configure.wrf.edit
        /bin/mv configure.wrf.edit configure.wrf
     fi

     # GNU has a funny way of doing promotion to real*8
     if [ "$rword" = "-r8" ] ; then
       srch=`grep -i "^SFC" configure.wrf | grep -i "gfortran"`
       if [ -n "$srch" ] ; then
         sed -e '/^PROMOTION/s/#//' \
          -e '/^RWORDSIZE/s/$(NATIVE_RWORDSIZE)/8/' configure.wrf > configure.wrf.edit
       else
         sed -e '/^RWORDSIZE/s/$(NATIVE_RWORDSIZE)/8/' configure.wrf > configure.wrf.edit
       fi
       /bin/mv configure.wrf.edit configure.wrf
     fi
   else
     echo '*********************************************************'
     echo '***              ERROR ERROR ERROR ERROR              ***'
     echo '***                                                   ***'
     echo '*** Configuration not found in configure.defaults     ***'
     echo '*********************************************************'
     exit 2
   fi
fi

if test -f configure.wrf ; then
# new dec 2005.  test what fseek is supported (needed for share/landread.c to work correctly)
  echo testing for fseeko and fseeko64
  /bin/rm -f tools/fseeko_test tools/fseeko64_test
  ( make fseek_test 2> /dev/null ) 1> /dev/null
  if [ "$os" = "Darwin" ] ; then
    # fseeko64 does not exist under Darwin fseeko does. Remove the 0 length executable
    # file that might get generated anyway, even though the compiler complains about missing reference.
    /bin/rm -f tools/fseeko64_test 
  fi
  if test -x tools/fseeko64_test ; then
    ( tools/fseeko64_test 2> /dev/null ) 1> /dev/null
    if [ $? = 0 ] ; then
       echo fseeko64 is supported
       sed '/^CC .*=/s/$/ -DFSEEKO64_OK /' configure.wrf > xx$$ ; /bin/mv xx$$ configure.wrf
    fi
  else
    if test -x tools/fseeko_test ; then
      ( tools/fseeko_test 2> /dev/null ) 1> /dev/null
      if [ $? = 0 ] ; then
        echo fseeko is supported and handles 64 bit offsets
        sed '/^CC .*=/s/$/ -DFSEEKO_OK /' configure.wrf > xx$$ ; /bin/mv xx$$ configure.wrf
      else
        echo neither fseeko64 nor fseeko with 64 bit offsets works, landread will be compiled with fseek
        echo but may not work correctly for very high resolution terrain datasets
      fi
    else
      echo neither fseeko64 nor fseeko with 64 bit offsets works, landread will be compiled with fseek
      echo but may not work correctly for very high resolution terrain datasets
    fi
  fi
fi

echo "------------------------------------------------------------------------"
sed -e '1,/#### Architecture specific settings ####/d' -e '/^externals/,$d' configure.wrf

echo "------------------------------------------------------------------------"
echo "Settings listed above are written to configure.wrf."
echo "If you wish to change settings, please edit that file."
echo "If you wish to change the default options, edit the file:"
echo "     arch/configure.defaults"

if test -n "$NETCDF" ; then
  if [ ! -f $NETCDF/include/netcdf.inc ] ; then
    echo
    echo "Error : Not found $NETCDF/include/netcdf.inc"
    echo "        Please check this installation of NetCDF and re-run this configure script"
    exit -1
  fi
  grep nf_format_64bit $NETCDF/include/netcdf.inc > /dev/null
  configure_aaaa=$? ; export configure_aaaa
  if [ $configure_aaaa -a -z "$WRFIO_NCD_NO_LARGE_FILE_SUPPORT" ] ; then
    echo "NetCDF users note:"
    echo " This installation of NetCDF supports large file support.  To DISABLE large file" 
    echo " support in NetCDF, set the environment variable WRFIO_NCD_NO_LARGE_FILE_SUPPORT"
    echo " to 1 and run configure again. Set to any other value to avoid this message."
  fi
fi
echo "  "

if [ "$wrf_core" = "DA_CORE" -o "$wrf_core" = "WRF_PLUS_CORE" -o "$wrf_core" = "4D_DA_CORE" ]; then
  if [ "`grep '^SFC' configure.wrf | grep -i 'gfortran'`" != "" -o "`grep '^SFC' configure.wrf | grep -i 'frtpx'`" != "" ]; then
    echo "WRFDA/WRFPLUS using gfortran/frtpx needs realsize=8"
    sed -e '/^PROMOTION.*=/s/#//' configure.wrf > configure.wrf.edit
    /bin/mv configure.wrf.edit configure.wrf
  fi
fi

if [ -n "$WRFPLUS" ] ; then
  if [ $WRFPLUS = 1 ] ; then
    if [ `grep '^SFC' configure.wrf | cut -d= -f2` = "gfortran" ]; then
      echo "WRFPLUS using gfortran needs realsize=8"
      sed -e '/^PROMOTION.*=/s/#//' configure.wrf > configure.wrf.edit
      /bin/mv configure.wrf.edit configure.wrf
    fi
  fi
fi

#Checking cross-compiling capability for some particular environment 
#on Linux and Mac box

if [ $os = "Linux" -o $os = "Darwin" ]; then

  SFC=`grep '^SFC' configure.wrf | awk '{print $3}'`
  SCC=`grep '^SCC' configure.wrf | awk '{print $3}'`
  CCOMP=`grep '^CCOMP' configure.wrf | awk '{print $3}'`

  SFC="`type $SFC 2>/dev/null | awk '{print $NF}' | sed -e 's/(//g;s/)//g'`"
  SCC="`type $SCC 2>/dev/null | awk '{print $NF}' | sed -e 's/(//g;s/)//g'`"
  CCOMP="`type $CCOMP 2>/dev/null | awk '{print $NF}' | sed -e 's/(//g;s/)//g'`"

  foo=foo_$$

cat > ${foo}.c <<EOF 
 int main(int argc, char ** argv)
 {
     return (0);
 }
EOF

cat > ${foo}.f <<EOF
          program test_exit
            integer :: STATUS = 0
            call EXIT(STATUS)
          end program test_exit
EOF

  # do not do this test for MIC, which cross compiles
  if [ -z "`grep 'SCC.*mmic' configure.wrf`" ] ; then
    # Detecting whether if mpi compiler wrapper supports -cc/-f90 options
    if [ -z "`grep -i -E '^DMPARALLEL[ \t]*=.*#' configure.wrf`" ] ; then
      mpicc -cc=$SCC -o ${foo} ${foo}.c > /dev/null 2>&1
      if [ $? != 0 ]; then
        sed 's/-cc=$(SCC)//' configure.wrf > configure.wrf.edit
        mv configure.wrf.edit configure.wrf
      fi
      rm ${foo} ${foo}.o 2> /dev/null
      mpif90 -f90=$SFC -o ${foo} ${foo}.f > /dev/null 2>&1
      if [ $? != 0 ]; then
        sed 's/-f90=$(SFC)//'  configure.wrf > configure.wrf.edit
        mv configure.wrf.edit configure.wrf
      fi
      rm ${foo} ${foo}.o 2> /dev/null
    fi
  fi

  if [ -e $NETCDF/lib/libnetcdf.a -a "$SFC" != "" -a "$SCC" != "" -a "$CCOMP" != "" ]; then

    SFC_MULTI_ABI=0
    SCC_MULTI_ABI=0
    CCOMP_MULTI_ABI=0
    CROSS_COMPILING=0

    echo
    echo Testing for NetCDF, C and Fortran compiler
    echo

    ar p $NETCDF/lib/libnetcdf.a `ar t $NETCDF/lib/libnetcdf.a | grep -E '\.o' | head -n 1 | sed 's/://'` > ${foo}.o
    netcdf_arch="`file ${foo}.o | grep -o -E '[0-9]{2}-bit|i386'`"
    rm ${foo}.o

    $SFC -o ${foo} ${foo}.f > /dev/null 2>&1 
    SFC_arch="`file ${foo} | grep -o -E '[0-9]{2}-bit|i386'`"
    rm ${foo} ${foo}.o 2> /dev/null

    $SCC -o ${foo} ${foo}.c > /dev/null 2>&1
    SCC_arch="`file ${foo} | grep -o -E '[0-9]{2}-bit|i386'`"
    CCOMP_arch=$SCC_arch
    rm ${foo} ${foo}.o 2> /dev/null

    if [ "$SCC" != "$CCOMP" ]; then
      $CCOMP -o ${foo} ${foo}.c > /dev/null 2>&1
      CCOMP_arch="`file ${foo} | grep -o -E '[0-9]{2}-bit|i386'`"
      rm ${foo} ${foo}.o 2> /dev/null
    fi

    if [ "$SFC_arch" = "" -o "$SCC_arch" = "" -o "$CCOMP_arch" = "" ]; then
      echo "  One of compilers testing failed!"
      echo "  Please check your compiler"
      echo 
      rm -f ${foo} ${foo}.[cfo] 2> /dev/null
      exit
    else
      cp configure.wrf configure.wrf.edit
    fi

    case $netcdf_arch in

      32-bit|i386 )

      if [ "$SFC_arch" = "64-bit" ] ; then
        CROSS_COMPILING=1
        $SFC -m32 -o ${foo} ${foo}.f > /dev/null 2>&1
        if [ $? = 0 ]; then
          SFC_MULTI_ABI=1
          sed '/^SFC.*=/s/$/ -m32/' configure.wrf.edit > configure.wrf.tmp
          mv configure.wrf.tmp configure.wrf.edit
        fi
      fi
      if [ "$SCC_arch" = "64-bit" ] ; then
        CROSS_COMPILING=1
        $SCC -m32 -o ${foo} ${foo}.c > /dev/null 2>&1
        if [ $? = 0 ]; then
          SCC_MULTI_ABI=1
          sed '/^SCC.*=/s/$/ -m32/' configure.wrf.edit > configure.wrf.tmp
          mv configure.wrf.tmp  configure.wrf.edit
        fi
      fi

      if [ "$CCOMP_arch" = "64-bit" ] ; then
        CROSS_COMPILING=1
        if [ "$CCOMP" != "$SCC" ]; then
          $CCOMP -m32 -o ${foo} ${foo}.c > /dev/null 2>&1
          if [ $? = 0 ]; then
            CCOMP_MULTI_ABI=1
            sed '/^CCOMP/ s/$/ -m32/' configure.wrf.edit > configure.wrf.tmp
            mv configure.wrf.tmp  configure.wrf.edit
          fi
        else
          CCOMP_MULTI_ABI=1
          sed '/^CCOMP/ s/$/ -m32/' configure.wrf.edit > configure.wrf.tmp
          mv configure.wrf.tmp  configure.wrf.edit
        fi
      fi

      if [ $CROSS_COMPILING -eq 1 ] ; then
        echo NOTE:
        echo This installation of NetCDF is 32-bit
        if [ \( $SFC_MULTI_ABI -ne 1 -a "$SFC_arch" = "64-bit" \) \
             -o \( $SCC_MULTI_ABI -ne 1 -a "$SCC_arch" = "64-bit" \) \
             -o \( $CCOMP_MULTI_ABI -ne 1 -a "$CCOMP_arch" = "64-bit" \) ] ; then
             rm configure.wrf.edit
             echo One of compilers is 64-bit and doesn\'t support cross-compiling.
             echo Please check your NETCDF lib and compiler
        else
          echo -m32 is appended to configure.wrf
          echo It will be forced to build in 32-bit.
          echo If you don\'t want 32-bit binaries, please use 64-bit NetCDF, and re-run the configure script.
        fi
      fi
      ;;

      64-bit )

      if [ "$SFC_arch" = "32-bit" -o "$SFC_arch" = "i386" ] ; then
        CROSS_COMPILING=1
        $SFC -m64 -o ${foo} ${foo}.f > /dev/null 2>&1
        if [ $? = 0 ]; then
          SFC_MULTI_ABI=1
          sed '/^SFC.*=/s/$/ -m64/' configure.wrf.edit > configure.wrf.tmp
          mv configure.wrf.tmp configure.wrf.edit
        fi
      fi
      if [ "$SCC_arch" = "32-bit" -o "$SCC_arch" = "i386" ] ; then
        CROSS_COMPILING=1
        $SCC -m64 -o ${foo} ${foo}.c > /dev/null 2>&1
        if [ $? = 0 ]; then
          SCC_MULTI_ABI=1
          sed '/^SCC.*=/s/$/ -m64/' configure.wrf.edit > configure.wrf.tmp
          mv configure.wrf.tmp configure.wrf.edit
        fi
      fi

      if [ "$CCOMP_arch" = "32-bit" -o "$CCOMP_arch" = "i386" ] ; then
        CROSS_COMPILING=1
        if [ "$CCOMP" != "$SCC" ]; then
          $CCOMP -m64 -o ${foo} ${foo}.c > /dev/null 2>&1
          if [ $? = 0 ]; then
            CCOMP_MULTI_ABI=1
            sed '/^CCOMP/ s/$/ -m64/' configure.wrf.edit > configure.wrf.tmp
            mv configure.wrf.tmp  configure.wrf.edit
          fi
        else
          CCOMP_MULTI_ABI=1
          sed '/^CCOMP/ s/$/ -m64/' configure.wrf.edit > configure.wrf.tmp
          mv configure.wrf.tmp  configure.wrf.edit
        fi
      fi

      if [ $CROSS_COMPILING -eq 1 ] ; then
        echo NOTE:
        echo This installation of NetCDF is 64-bit
        if [ \( $SFC_MULTI_ABI -ne 1 -a "$SFC_arch" != "64-bit" \) \
            -o \( $SCC_MULTI_ABI -ne 1 -a "$SCC_arch" != "64-bit" \) \
            -o \( $CCOMP_MULTI_ABI -ne 1 -a "$CCOMP_arch" != "64-bit" \) ]; then
            rm configure.wrf.edit
            echo One of Compilers is 32-bit and doesn\'t support cross-compiling.
            echo Please check your NetCDF lib and compiler
        else
          echo -m64 is appended to configure.wrf
          echo It will be forced to build in 64-bit. 
          echo If you don\'t want 64-bit binaries, please use 32-bit NetCDF, and re-run the configure script.
        fi
      fi
      ;;
    esac

    if [ -e configure.wrf.edit ]; then 
      mv configure.wrf.edit configure.wrf
    fi

    if [ $CROSS_COMPILING -eq 0 ] ; then
      echo "This installation of NetCDF is $netcdf_arch"
      echo "                 C compiler is $SCC_arch"
      echo "           Fortran compiler is $SFC_arch"
      echo "              It will build in $netcdf_arch"
    fi
    echo
  fi
  rm -f ${foo} ${foo}.[cfo] 2> /dev/null
fi

# testing for Fortran 2003 IEEE signaling features
make fortran_2003_ieee_test > tools/fortran_2003_ieee_test.log 2>&1
rm -f tools/fortran_2003_ieee_test.log
retval=-1
if [ -f tools/fortran_2003_ieee_test.exe ] ; then
  retval=0
fi
if [ $retval -ne 0 ] ; then
  sed -e '/^ARCH_LOCAL/s/$/ -DNO_IEEE_MODULE/' configure.wrf > configure.wrf.edit
  mv configure.wrf.edit configure.wrf
  echo " "
  echo " "
  echo "************************** W A R N I N G ************************************"
  echo " "
  echo "There are some Fortran 2003 features in WRF that your compiler does not recognize"
  echo "The IEEE signaling call has been removed.  That may not be enough."
  echo " "
  echo "*****************************************************************************"
fi

# testing for Fortran 2003 ISO_C features
make fortran_2003_iso_c_test > tools/fortran_2003_iso_c_test.log 2>&1
rm -f tools/fortran_2003_iso_c_test.log
retval=-1
if [ -f tools/fortran_2003_iso_c_test.exe ] ; then
  retval=0
fi
if [ $retval -ne 0 ] ; then
  sed -e '/^ARCH_LOCAL/s/$/ -DNO_ISO_C_SUPPORT/' configure.wrf > configure.wrf.edit
  mv configure.wrf.edit configure.wrf
  echo " "
  echo " "
  echo "************************** W A R N I N G ************************************"
  echo " "
  echo "There are some Fortran 2003 features in WRF that your compiler does not recognize"
  echo "The routines that utilize ISO_C support have been stubbed out. "
  echo "That may not be enough."
  echo " "
  echo "*****************************************************************************"
fi

# testing for Fortran 2003 FLUSH features
make fortran_2003_flush_test > tools/fortran_2003_flush_test.log 2>&1
rm -f tools/fortran_2003_flush_test.log
retval=-1
if [ -f tools/fortran_2003_flush_test.exe ] ; then
  retval=0
fi
if [ $retval -ne 0 ] ; then
  make fortran_2003_fflush_test > tools/fortran_2003_fflush_test.log 2>&1
  rm -f tools/fortran_2003_fflush_test.log
  retval=-1
  if [ -f tools/fortran_2003_fflush_test.exe ] ; then
    retval=0
  fi
  if [ $retval -eq 0 ] ; then
    sed -e '/^ARCH_LOCAL/s/$/ -DUSE_FFLUSH/' configure.wrf > configure.wrf.edit
    mv configure.wrf.edit configure.wrf
    echo " "
    echo " "
    echo "************************** W A R N I N G ************************************"
    echo " "
    echo "There are some Fortran 2003 features in WRF that your compiler does not recognize"
    echo "The standard FLUSH routine has been replaced by FFLUSH."
    echo "That may not be enough."
    echo " "
    echo "*****************************************************************************"
  fi
  if [ $retval -ne 0 ] ; then
    sed -e '/^ARCH_LOCAL/s/$/ -DNO_FLUSH_SUPPORT/' configure.wrf > configure.wrf.edit
    mv configure.wrf.edit configure.wrf
    echo " "
    echo " "
    echo "************************** W A R N I N G ************************************"
    echo " "
    echo "There are some Fortran 2003 features in WRF that your compiler does not recognize"
    echo "The standard FLUSH routine has been stubbed out."
    echo "That may not be enough."
    echo " "
    echo "*****************************************************************************"
  fi
fi

# testing for Fortran 2008 intrinsic gamma function
make fortran_2008_gamma_test > tools/fortran_2008_gamma.log 2>&1
rm -f tools/fortran_2008_gamma.log
retval=-1
if [ -f tools/fortran_2008_gamma_test.exe ] ; then
  retval=0
fi
if [ $retval -ne 0 ] ; then
  sed -e '/^ARCH_LOCAL/s/$/ -DNO_GAMMA_SUPPORT/' configure.wrf > configure.wrf.edit
  mv configure.wrf.edit configure.wrf
  echo " "
  echo " "
  echo "************************** W A R N I N G ************************************"
  echo " "
  echo "There are some Fortran 2008 features in WRF that your compiler does not recognize"
  echo "The intrinsic gamma function is not available, required by some schemes."
  echo "That code is stubbbed out, and those schemes are unavailable at run-time."
  echo " "
  echo "*****************************************************************************"
fi

# testing for netcdf4 IO features
if [ -n "$NETCDF4" ] ; then
  if [ $NETCDF4 -eq 1 ] ; then
    make nc4_test > tools/nc4_test.log 2>&1
    retval=-1
    if  [ -f tools/nc4_test.exe ] ; then
      retval=0
      rm -f tools/nc4_test.log
    fi
    if [ $retval -ne 0  ] ; then
      echo "************************** W A R N I N G ************************************"
      echo "NETCDF4 IO features are requested, but this installation of NetCDF           "
      echo "  $NETCDF"
      echo "DOES NOT support these IO features.                                          "
      echo
      echo "Please make sure NETCDF version is 4.1.3 or later and was built with         "
      echo "--enable-netcdf4                                                             "
      echo
      echo "OR set NETCDF_classic variable                                               "
      echo "   bash/ksh : export NETCDF_classic=1
      echo "        csh : setenv NETCDF_classic 1
      echo 
      echo "Then re-run this configure script                                            "
      echo
      echo "!!! configure.wrf has been REMOVED !!!"
      echo
      echo "*****************************************************************************"
      rm -f configure.wrf
    else
      echo "*****************************************************************************"
      echo "This build of WRF will use NETCDF4 with HDF5 compression"
      echo "*****************************************************************************"
      echo " "
    fi
  fi
else
  echo "*****************************************************************************"
  echo "This build of WRF will use classic (non-compressed) NETCDF format"
  echo "*****************************************************************************"
  echo " "
fi
