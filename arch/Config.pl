#!/usr/bin/perl
#
# Configuration script for WRF prototype code
# 
# Be sure to run as ./configure (to avoid getting a system configure command by mistake)
#

select((select(STDOUT), $|=1)[0]);
$sw_perl_path = perl ;
$sw_netcdf_path = "" ;
$sw_pnetcdf_path = "" ;
$sw_hdf5_path=""; 
$sw_phdf5_path=""; 
$sw_jasperlib_path=""; 
$sw_jasperinc_path=""; 
$sw_esmflib_path="";
$sw_esmfinc_path="";
$sw_ldflags=""; 
$sw_compileflags=""; 
$sw_opt_level=""; 
$sw_rwordsize="\$\(NATIVE_RWORDSIZE\)";
$sw_rttov_flag = "" ;
$sw_rttov_inc = "" ;
$sw_rttov_path = "" ;
$sw_crtm_flag = "" ;
$sw_cloudcv_flag = "" ;
$sw_4dvar_flag = "" ;
$sw_wrfplus_path = "" ;
$sw_wavelet_flag = "" ;
$WRFCHEM = 0 ;
$sw_os = "ARCH" ;           # ARCH will match any
$sw_mach = "ARCH" ;         # ARCH will match any
$sw_wrf_core = "" ;
$sw_da_core = "-DDA_CORE=\$\(WRF_DA_CORE\)" ;
$sw_wrfplus_core = "-DWRFPLUS=\$\(WRF_PLUS_CORE\)" ;
$sw_nmm_core = "-DNMM_CORE=\$\(WRF_NMM_CORE\)" ;
$sw_em_core = "-DEM_CORE=\$\(WRF_EM_CORE\)" ;
$sw_dmparallel = "" ;
$sw_ompparallel = "" ;
$sw_stubmpi = "" ;
$sw_usenetcdff = "" ;    # UNIDATA switches around library names a bit
$sw_usenetcdf = "" ;    
$sw_time = "" ;          # name of a timer to time fortran compiles, e.g. timex or time
$sw_ifort_r8 = 0 ;
$sw_hdf5 = "-lhdf5 -lhdf5_hl";
$sw_zlib = "-lz";
$sw_dep_lib_path = "";
$sw_gpfs_path = "";
$sw_gpfs_lib  = "-lgpfs";
$sw_curl_path = "";
$sw_curl_lib  = "-lcurl";
$sw_terrain_and_landuse = "";
$sw_tfl = "" ;
$sw_cfl = "" ;
$sw_config_line = "" ;
while ( substr( $ARGV[0], 0, 1 ) eq "-" )
 {
  if ( substr( $ARGV[0], 1, 5 ) eq "perl=" )
  {
    $sw_perl_path = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 7 ) eq "netcdf=" )
  {
    $sw_netcdf_path = substr( $ARGV[0], 8 ) ;
  }
  if ( substr( $ARGV[0], 1, 13 ) eq "dep_lib_path=" )
  {
    $sw_dep_lib_path = substr( $ARGV[0], 14 ) ;
    $sw_dep_lib_path =~ s/\r|\n/ /g ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "gpfs=" )
  {
    $sw_gpfs_path = substr( $ARGV[0], 6 ) ;
    if ( $sw_gpfs_path ne "" ) 
      {
        if ( substr( $sw_gpfs_path, -1, 1 ) eq "/" )
          {
            $sw_gpfs_path = substr($sw_gpfs_path, 0, length($sw_gpfs_path)-1 ) ;
          }
      }
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "curl=" )
  {
    $sw_curl_path = substr( $ARGV[0], 6 ) ;
    if ( $sw_curl_path ne "" )
      {
        if ( substr( $sw_curl_path, -1, 1 ) eq "/" )
          {
            $sw_curl_path = substr($sw_curl_path, 0, length($sw_curl_path)-1 ) ;
          }
      }
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "pnetcdf=" )
  {
    $sw_pnetcdf_path = substr( $ARGV[0], 9 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "hdf5=" )
  {
    $sw_hdf5_path = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 6 ) eq "phdf5=" )
  {
    $sw_phdf5_path = substr( $ARGV[0], 7 ) ;
  }
  if ( substr( $ARGV[0], 1, 3 ) eq "os=" )
  {
    $sw_os = substr( $ARGV[0], 4 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "mach=" )
  {
    $sw_mach = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 10 ) eq "opt_level=" )
  {
    $sw_opt_level = substr( $ARGV[0], 11 ) ;
  }
  if ( substr( $ARGV[0], 1, 11 ) eq "USENETCDFF=" )
  {
    $sw_usenetcdff = substr( $ARGV[0], 12 ) ;
  }
  if ( substr( $ARGV[0], 1, 10 ) eq "USENETCDF=" )
  {
    $sw_usenetcdf = substr( $ARGV[0], 11 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "time=" )
  {
    $sw_time = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "ldflags=" )
  {
    $sw_ldflags = substr( $ARGV[0], 9 ) ;
# multiple options separated by spaces are passed in from sh script
# separated by ! instead. Replace with spaces here.
    $sw_ldflags =~ s/!/ /g ;
  }
  if ( substr( $ARGV[0], 1, 9 ) eq "wrf_core=" )
  {
    $sw_wrf_core = substr( $ARGV[0], 10 ) ;
    if ( index ( $sw_wrf_core , "EM_CORE" ) > -1 ) 
    {
      $sw_em_core = "-DEM_CORE=1" ;
      $sw_da_core = "-DDA_CORE=0" ;
      $sw_wrfplus_core = "-DWRFPLUS=0" ;
      $sw_nmm_core = "-DNMM_CORE=0" ;
    }
    if ( index ( $sw_wrf_core , "WRF_PLUS_CORE" ) > -1 ) 
    {
      $sw_em_core = "-DEM_CORE=1" ;
      $sw_da_core = "-DDA_CORE=0" ;
      $sw_wrfplus_core = "-DWRFPLUS=1" ;
      $sw_nmm_core = "-DNMM_CORE=0" ;
      $sw_dfi_radar = "-DDFI_RADAR=0" ;
    }
    if ( index ( $sw_wrf_core , "DA_CORE" ) > -1 ) 
    {
      $sw_em_core = "-DEM_CORE=1" ;
      $sw_da_core = "-DDA_CORE=1" ;
      $sw_wrfplus_core = "-DWRFPLUS=0" ;
      $sw_nmm_core = "-DNMM_CORE=0" ;
    }
    if ( index ( $sw_wrf_core , "4D_DA_CORE" ) > -1 ) 
    {
      $sw_em_core = "-DEM_CORE=1" ;
      $sw_da_core = "-DDA_CORE=1" ;
      $sw_nmm_core = "-DNMM_CORE=0" ;
    }
    if ( index ( $sw_wrf_core , "4D_DA_CORE" ) > -1 ) 
    {
      $sw_em_core = "-DEM_CORE=1" ;
      $sw_da_core = "-DDA_CORE=1" ;
      $sw_nmm_core = "-DNMM_CORE=0" ;
    }
    if ( index ( $sw_wrf_core , "NMM_CORE" ) > -1 ) 
    {
      $sw_em_core = "-DEM_CORE=0" ;
      $sw_da_core = "-DDA_CORE=0" ;
      $sw_wrfplus_core = "-DWRFPLUS=0" ;
      $sw_nmm_core = "-DNMM_CORE=1" ;
    }
  }
  if ( substr( $ARGV[0], 1, 13 ) eq "compileflags=" )
  {
    $sw_compileflags = substr( $ARGV[0], 14 ) ;
    $sw_compileflags =~ s/!/ /g ;
#   look for each known option
    $where_index = index ( $sw_compileflags , "-DWRF_CHEM" ) ;
    if ( $where_index eq -1 ) 
    {
      $WRFCHEM = 0 ;
    }
    else
    {
      $WRFCHEM = 1 ;
    } 
  }
  if ( substr( $ARGV[0], 1, 11 ) eq "dmparallel=" )
  {
    $sw_dmparallel=substr( $ARGV[0], 12 ) ;
  }
  if ( substr( $ARGV[0], 1, 12 ) eq "ompparallel=" )
  {
    $sw_ompparallel=substr( $ARGV[0], 13 ) ;
  }
  if ( substr( $ARGV[0], 1, 4 ) eq "tfl=" )
  {
    $sw_tfl=substr( $ARGV[0], 5 ) ;
  }
  if ( substr( $ARGV[0], 1, 4 ) eq "cfl=" )
  {
    $sw_cfl=substr( $ARGV[0], 5 ) ;
  }
  if ( substr( $ARGV[0], 1, 12 ) eq "config_line=" )
  {
    $sw_config_line=substr( $ARGV[0], 13 ) ;
  }
  shift @ARGV ;
 }

 $sw_fc = "\$(SFC)" ;
 $sw_cc = "\$(SCC)" ;
 $sw_comms_lib = "" ;
 $sw_comms_include = "" ;
 $sw_dmparallelflag = "" ;
 $sw_nest_opt = "" ; 
 $sw_comms_external = "gen_comms_serial module_dm_serial" ;


 if ( $sw_dmparallel eq "RSL_LITE" ) 
 {
  $sw_fc = "\$(DM_FC)" ;
  $sw_cc = "\$(DM_CC)" ;
  $sw_dmparallelflag = "-DDM_PARALLEL" ;
  $sw_comms_lib = "\$(WRF_SRC_ROOT_DIR)/external/RSL_LITE/librsl_lite.a" ;
  $sw_comms_external = "\$(WRF_SRC_ROOT_DIR)/external/RSL_LITE/librsl_lite.a gen_comms_rsllite module_dm_rsllite" ;
  $sw_comms_include = "-I\$(WRF_SRC_ROOT_DIR)/external/RSL_LITE" ;
 }

# The jasper library is required to build Grib2 I/O.  User must set 
# environment variables JASPERLIB and JASPERINC to paths to library and 
# include files to enable this feature prior to running configure.  

 $I_really_want_to_output_grib2_from_WRF = "FALSE" ;

 if ( $ENV{JASPERLIB} && $ENV{JASPERINC} && $I_really_want_to_output_grib2_from_WRF eq "TRUE" )
   {
   printf "Configuring to use jasper library to build Grib2 I/O...\n" ;
   printf("  \$JASPERLIB = %s\n",$ENV{JASPERLIB});
   printf("  \$JASPERINC = %s\n",$ENV{JASPERINC});
   $sw_jasperlib_path = $ENV{JASPERLIB}; 
   $sw_jasperinc_path = $ENV{JASPERINC}; 
   }
 else
   {
   if ( $ENV{JASPERLIB} && $ENV{JASPERINC} )
     {
     printf "\n\nIf you REALLY want Grib2 output from WRF, modify the arch/Config.pl script.\n" ;
     printf "Right now you are not getting the Jasper lib, from the environment, compiled into WRF.\n\n" ;
     }
   else
     {
     printf "\$JASPERLIB or \$JASPERINC not found in environment, configuring to build without grib2 I/O...\n" ;
     }
   }

# When compiling DA and WRFPLUS code, we need to always use 8-byte reals.
 if ( $ENV{WRF_DA_CORE} eq "1" || $sw_da_core eq "-DDA_CORE=1" )
   {
     $sw_rwordsize = "8";  
     if(defined $ENV{'CRTM'})
       {
       if ( $ENV{CRTM} ne "0" )
         {
         $sw_crtm_flag = "-DCRTM";
         }
       } 
     else 
       {
         {
         $sw_crtm_flag = "-DCRTM";
         }
       }
     if ( $ENV{RTTOV} )
       {
       $sw_rttov_flag = "-DRTTOV";
       $sw_rttov_inc = "-I$ENV{RTTOV}/include -I$ENV{RTTOV}/mod";
       $sw_rttov_path= $ENV{RTTOV};
       }
     if ( $ENV{CLOUD_CV} )
       {
       $sw_cloudcv_flag = "-DCLOUD_CV";
       }
     if ( $sw_wrf_core eq "4D_DA_CORE" )
       {
       $sw_4dvar_flag = "-DVAR4D";
       $sw_wrfplus_path= $ENV{WRFPLUS_DIR};
       }
     if ( $ENV{WAVELET} )
       {
       $sw_wavelet_flag = "-DWAVELET";
       }
   }

 $sw_rwordsize = "8" if ( $sw_wrfplus_core eq "-DWRFPLUS=1" );

# A separately-installed ESMF library is required to build the ESMF 
# implementation of WRF IOAPI in external/io_esmf.  This is needed 
# to couple WRF with other ESMF components.  User must set environment 
# variables ESMFLIB and ESMFINC to paths ESMF to library and include 
# files to enable this feature prior to running configure.
 if ( $ENV{ESMFLIB} && $ENV{ESMFINC} )
   {
   printf "Configuring to use ESMF library to build WRF...\n" ;
   printf "WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING\n" ;
   printf "WARNING:  THIS IS AN EXPERIMENTAL CONFIGURATION\n" ;
   printf "WARNING:  IT DOES NOT WORK WITH NESTING\n" ;
   printf "WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING\n" ;
   printf("  \$ESMFLIB = %s\n",$ENV{ESMFLIB});
   printf("  \$ESMFINC = %s\n",$ENV{ESMFINC});
   $sw_esmflib_path = $ENV{ESMFLIB};
   $sw_esmfinc_path = $ENV{ESMFINC};
   $sw_esmf_ldflag = "yes" ;
   }

# parse the configure.wrf file

$validresponse = 0 ;

if ( ($sw_wrf_core eq "WRF_PLUS_CORE") || ($sw_wrf_core eq "4D_DA_CORE") ) 
   { @platforms = qw ( serial dmpar ) ; }
   else
   { @platforms = qw ( serial smpar dmpar dm+sm ) ; }

# Display the choices to the user and get selection
until ( $validresponse ) {
  printf "------------------------------------------------------------------------\n" ;
  printf "Please select from among the following $sw_os $sw_mach options:\n\n" ;

  $opt = 1 ;
  $optstr = "";
  open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" 
      or die "Cannot open ./arch/configure.defaults for reading" ;
  while ( <CONFIGURE_DEFAULTS> ) {

     $currline = $_;
     chomp $currline;
     # Look for our platform in the configuration option header. 
     # If we're going to list it, print parallelism options
     if ( substr( $currline, 0, 5 ) eq "#ARCH" && ( index( $currline, $sw_os ) >= 0 ) 
         && ( index( $currline, $sw_mach ) >= 0 ) ) {
        $optstr = substr($currline,6) ;

        foreach ( @platforms ) { # Check which parallelism options are valid for this configuration option
           $paropt = $_ ;
           if ( index($optstr, $paropt) >= 0 ) { #If parallelism option is valid, print and assign number
              printf "%3d. (%s) ",$opt,$paropt ;
              $pararray[$opt] = $paropt ;
              $opttemp = $optstr ;
              $opttemp =~ s/#.*$//g ;
              chomp($opttemp) ;
              $optarray[$opt] = $opttemp." (".$paropt.")" ;
              $opt++ ;
           } else { #If parallelism option is not valid, print spaces for formatting/readability
              $paropt =~ s/./ /g ;
              printf "      %s  ",$paropt ;
           }
        }
        next;
     }

     next unless ( length $optstr ) ; # Don't read option lines unless it's valid for our platform

     if ( substr( $currline, 0, 11 ) eq "DESCRIPTION" ) {
        $optstr = $currline ; #Initial value of $optstr is DESCRIPTION line
        next;
     }

     if ( substr( $currline, 0, 3 ) eq "SFC" ) {
        $currline =~ s/^SFC\s*=\s*//g;      #remove "SFC ="
        $currline =~ s/ (\-\S*)*$//g;       #remove trailing arguments and/or spaces
        $optstr =~ s/\$SFC/$currline/g;     #Substitute the fortran compiler name into optstr
        $optstr =~ s/DESCRIPTION\s*=\s*//g; #Remove "DESCRIPTION ="
        next;
     }

     if ( substr( $currline, 0, 3 ) eq "SCC" ) {
        $currline =~ s/^SCC\s*=\s*//g;      #remove "SCC ="
        $currline =~ s/ (\-\S*)*$//g;       #remove trailing arguments and/or spaces
        $optstr =~ s/\$SCC/$currline/g;     #Substitute the C compiler name into optstr
        next;
     }

     if ( substr( $currline, 0, 4 ) eq "####" ) { #reached the end of this option's entry
        chomp($optstr) ;
        printf "  %s\n",$optstr ;
        $optstr = "";
        next;
     }

  }
  close CONFIGURE_DEFAULTS ;

  $opt -- ;

  printf "\nEnter selection [%d-%d] : ",1,$opt ;
  $response = <STDIN> ;

  if ( $response == -1 ) { exit ; }

  if ( $response >= 1 && $response <= $opt ) 
  { $validresponse = 1 ; }
  else
  { printf("\nInvalid response (%d)\n",$response);}
  $response_opt = $response ; 
  chop $response_opt ;
}
printf "------------------------------------------------------------------------\n" ;

$optchoice = $response ;
if ( $response == 2 || $response == 3 ) {
  if ( $ENV{'TERRAIN_AND_LANDUSE'} eq "1" && index($sw_wrf_core, "EM_CORE") > -1 ) { 
    $sw_terrain_and_landuse =" -DTERRAIN_AND_LANDUSE" ;
  }
} 
open CONFIGURE_DEFAULTS, "cat ./arch/configure.defaults |"  ;
$latchon = 0 ;
while ( <CONFIGURE_DEFAULTS> )
{
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 1 )
  {
    close CONFIGURE_DEFAULTS ;
    if ( $sw_opt_level eq "-f" ) {
      open CONFIGURE_DEFAULTS, "cat ./arch/postamble ./arch/noopt_exceptions_f |"  or die "horribly" ;
    } else {
      open CONFIGURE_DEFAULTS, "cat ./arch/postamble ./arch/noopt_exceptions |"  or die "horribly" ;
    }
  }
  $_ =~ s:CONFIGURE_NMM_CORE:$sw_nmm_core:g ;
  if ( $latchon == 1 )
  {
    $_ =~ s/CONFIGURE_PERL_PATH/$sw_perl_path/g ;
    $_ =~ s/CONFIGURE_NETCDF_PATH/$sw_netcdf_path/g ;
    $_ =~ s/CONFIGURE_PNETCDF_PATH/$sw_pnetcdf_path/g ;
    $_ =~ s/CONFIGURE_HDF5_PATH/$sw_hdf5_path/g ;
    $_ =~ s/CONFIGURE_PHDF5_PATH/$sw_phdf5_path/g ;
    $_ =~ s/CONFIGURE_LDFLAGS/$sw_ldflags/g ;
    $_ =~ s/CONFIGURE_COMPILEFLAGS/$sw_compileflags/g ;
    $_ =~ s/CONFIGURE_RWORDSIZE/$sw_rwordsize/g ;
    $_ =~ s/CONFIGURE_FC/$sw_time $sw_fc/g ;
    $_ =~ s/CONFIGURE_CC/$sw_cc/g ;
    $_ =~ s/CONFIGURE_COMMS_LIB/$sw_comms_lib/g ;
    $_ =~ s/CONFIGURE_COMMS_INCLUDE/$sw_comms_include/g ;
    $_ =~ s/CONFIGURE_COMMS_EXTERNAL/$sw_comms_external/g ;
    if ( $sw_os ne "CYGWIN_NT" ) {
      $_ =~ s/#NOWIN// ;
    }
    $_ =~ s/CONFIGURE_DMPARALLEL/$sw_dmparallelflag/g ;
    $_ =~ s/CONFIGURE_STUBMPI/$sw_stubmpi/g ;
    $_ =~ s/CONFIGURE_NESTOPT/$sw_nest_opt/g ;
    $_ =~ s/CONFIGURE_TRADFLAG/$sw_tfl/g ;
    $_ =~ s/CONFIGURE_CPPFLAGS/$sw_cfl/g ;
    $_ =~ s/CONFIGURE_4DVAR_FLAG/$sw_4dvar_flag/g ;
    $_ =~ s/CONFIGURE_WRFPLUS_PATH/$sw_wrfplus_path/g ;
    $_ =~ s/CONFIGURE_CRTM_FLAG/$sw_crtm_flag/g ;
    $_ =~ s/CONFIGURE_RTTOV_FLAG/$sw_rttov_flag/g ;
    $_ =~ s/CONFIGURE_RTTOV_INC/$sw_rttov_inc/g ;
    $_ =~ s/CONFIGURE_RTTOV_PATH/$sw_rttov_path/g ;
    $_ =~ s/CONFIGURE_CLOUDCV_FLAG/$sw_cloudcv_flag/g ;
    $_ =~ s/CONFIGURE_WAVELET_FLAG/$sw_wavelet_flag/g ;
    if ( $sw_ifort_r8 ) {
      $_ =~ s/^PROMOTION.*=/PROMOTION       =       -r8 /g ;
    }
    if ( $sw_dmparallel ne "" && ($_ =~ /^DMPARALLEL[=\t ]/) ) {
       $_ =~ s/#// ;
    }
    if ( $sw_ompparallel ne "" && ( $_ =~ /^OMPCPP[=\t ]/ || $_ =~ /^OMPCC[=\t ]/ || $_ =~ /^OMP[=\t ]/ ) ) {
       $_ =~ s/#// ;
       $_ =~ s/#// ;
       $_ =~ s/#// ;
    }
    if ( $sw_netcdf_path ) 
      { $_ =~ s/CONFIGURE_WRFIO_NF/wrfio_nf/g ;
	$_ =~ s:CONFIGURE_NETCDF_FLAG:-DNETCDF: ;
        if ( $ENV{NETCDF_LDFLAGS} ) {
          $_ =~ s:CONFIGURE_NETCDF_LIB_PATH:\$\(WRF_SRC_ROOT_DIR\)/external/io_netcdf/libwrfio_nf.a $ENV{NETCDF_LDFLAGS} : ;
        } elsif ( $sw_os eq "Interix" ) {
	  $_ =~ s:CONFIGURE_NETCDF_LIB_PATH:\$\(WRF_SRC_ROOT_DIR\)/external/io_netcdf/libwrfio_nf.a -L$sw_netcdf_path/lib $sw_usenetcdff $sw_usenetcdf : ;
        } else {
	  $_ =~ s:CONFIGURE_NETCDF_LIB_PATH:-L\$\(WRF_SRC_ROOT_DIR\)/external/io_netcdf -lwrfio_nf -L$sw_netcdf_path/lib $sw_usenetcdff $sw_usenetcdf : ;
        }
	 }
    else                   
      { $_ =~ s/CONFIGURE_WRFIO_NF//g ;
	$_ =~ s:CONFIGURE_NETCDF_FLAG::g ;
	$_ =~ s:CONFIGURE_NETCDF_LIB_PATH::g ;
	 }

    if ( $sw_pnetcdf_path ) 
      { $_ =~ s/CONFIGURE_WRFIO_PNF/wrfio_pnf/g ;
	$_ =~ s:CONFIGURE_PNETCDF_FLAG:-DPNETCDF: ;
        if ( $sw_os eq "Interix" ) {
	  $_ =~ s:CONFIGURE_PNETCDF_LIB_PATH:\$\(WRF_SRC_ROOT_DIR\)/external/io_pnetcdf/libwrfio_pnf.a -L$sw_pnetcdf_path/lib -lpnetcdf: ;
        } else {
	  $_ =~ s:CONFIGURE_PNETCDF_LIB_PATH:-L\$\(WRF_SRC_ROOT_DIR\)/external/io_pnetcdf -lwrfio_pnf -L$sw_pnetcdf_path/lib -lpnetcdf: ;
        }
	 }
    else                   
      { $_ =~ s/CONFIGURE_WRFIO_PNF//g ;
	$_ =~ s:CONFIGURE_PNETCDF_FLAG::g ;
	$_ =~ s:CONFIGURE_PNETCDF_LIB_PATH::g ;
	 }

    if ( $sw_hdf5_path ) 
      { $_ =~ s:CONFIGURE_HDF5_LIB_PATH:-L$sw_hdf5_path/lib -lhdf5_fortran -lhdf5 -lm -lz: ;
        $_ =~ s:CONFIGURE_HDF5_FLAG:-DHDF5: ;
         }
    else
      { $_ =~ s:CONFIGURE_HDF5_LIB_PATH::g ;
        $_ =~ s:CONFIGURE_HDF5_FLAG::g ;
         }

    if ( $sw_phdf5_path ) 

      { $_ =~ s/CONFIGURE_WRFIO_PHDF5/wrfio_phdf5/g ;
	$_ =~ s:CONFIGURE_PHDF5_FLAG:-DPHDF5: ;
	$_ =~ s:CONFIGURE_PHDF5_LIB_PATH:-L\$\(WRF_SRC_ROOT_DIR\)/external/io_phdf5 -lwrfio_phdf5 -L$sw_phdf5_path/lib -lhdf5_fortran -lhdf5 -lm -lz -L$sw_phdf5_path/lib -lsz: ;
	 }
    else                   
      { $_ =~ s/CONFIGURE_WRFIO_PHDF5//g ;
	$_ =~ s:CONFIGURE_PHDF5_FLAG::g ;
	$_ =~ s:CONFIGURE_PHDF5_LIB_PATH::g ;
	 }

    if ( $sw_jasperlib_path && $sw_jasperinc_path ) 
      { $_ =~ s/CONFIGURE_WRFIO_GRIB2/wrfio_grib2/g ;
        $_ =~ s:CONFIGURE_GRIB2_FLAG:-DGRIB2:g ;
        $_ =~ s:CONFIGURE_GRIB2_INC:-I$sw_jasperinc_path:g ;
        $_ =~ s:CONFIGURE_GRIB2_LIB:-L\$\(WRF_SRC_ROOT_DIR\)/external/io_grib2 -lio_grib2 -L$sw_jasperlib_path -ljasper:g ;
      }
    else                   
      { $_ =~ s/CONFIGURE_WRFIO_GRIB2//g ;
        $_ =~ s:CONFIGURE_GRIB2_FLAG::g ;
        $_ =~ s:CONFIGURE_GRIB2_INC::g ;
        $_ =~ s:CONFIGURE_GRIB2_LIB::g ;
      }

   if ( $sw_terrain_and_landuse )
     { 
        $_ =~ s/CONFIGURE_TERRAIN_AND_LANDUSE/$sw_terrain_and_landuse/g;
     }
   else
     {
       $_  =~ s:CONFIGURE_TERRAIN_AND_LANDUSE::g;
     }

    # ESMF substitutions in configure.defaults
    if ( $sw_esmflib_path && $sw_esmfinc_path )
      {
      $_ =~ s:CONFIGURE_ESMF_FLAG:-DESMFIO:g ;
# pre 5.2.0r
#      $_ =~ s:ESMFIOLIB:-L$sw_esmflib_path -lesmf -L\$\(WRF_SRC_ROOT_DIR\)/external/io_esmf -lwrfio_esmf \$\(ESMF_LIB_FLAGS\):g ;
#      $_ =~ s:ESMFIOEXTLIB:-L$sw_esmflib_path -lesmf -L\$\(WRF_SRC_ROOT_DIR\)/external/io_esmf -lwrfio_esmf \$\(ESMF_LIB_FLAGS\):g ;
# post 5.2.0r
      $_ =~ s:ESMFIOLIB:\$\(ESMF_F90LINKPATHS\) \$\(ESMF_F90ESMFLINKLIBS\) -L\$\(WRF_SRC_ROOT_DIR\)/external/io_esmf -lwrfio_esmf: ;
      $_ =~ s:ESMFIOEXTLIB:\$\(ESMF_IO_LIB\): ;

     
      $_ =~ s:ESMFLIBFLAG:\$\(ESMF_LDFLAG\):g ;
#      $_ =~ s:ESMFINCLUDEGOESHERE:'include $(ESMFLIB)/esmf.mk': ;

      }
    else
      {
        $_ =~ s:CONFIGURE_ESMF_FLAG::g ;
        $_ =~ s:ESMFLIBFLAG::g ;
        if ( $sw_os eq "Interix" ) {
           $_ =~ s:ESMFIOLIB:\$\(WRF_SRC_ROOT_DIR\)/external/esmf_time_f90/libesmf_time.a:g ;
           $_ =~ s:ESMFIOEXTLIB:-L\$\(WRF_SRC_ROOT_DIR\)/external/esmf_time_f90/libesmf_time.a:g ;
        } else {
           $_ =~ s:ESMFIOLIB:-L\$\(WRF_SRC_ROOT_DIR\)/external/esmf_time_f90 -lesmf_time:g ;
           $_ =~ s:ESMFIOEXTLIB:-L\$\(WRF_SRC_ROOT_DIR\)/external/esmf_time_f90 -lesmf_time:g ;
        }
      }
     if ( $ENV{HWRF} )
       {
        $_ =~ s:CONFIGURE_ATMOCN_LIB:-L\$\(WRF_SRC_ROOT_DIR\)/external/atm_ocn  -latm_ocn:g ;
        $_ =~ s:CONFIGURE_ATMOCN_INC:-I\$\(WRF_SRC_ROOT_DIR\)/external/atm_ocn:g;
        $_ =~ s/CONFIGURE_ATMOCN/atm_ocn/g ;
       }
     else
       {
        $_ =~ s:CONFIGURE_ATMOCN_LIB::g ;
        $_ =~ s/CONFIGURE_ATMOCN//g ;
        $_ =~ s:CONFIGURE_ATMOCN_INC::g;
       }
     if ( $ENV{NETCDF4} )
       { if ( $ENV{NETCDF4} eq "1" )
           {
             if ( /(^ARCH_LOCAL.*=|^TRADFLAG.*=)/ ) 
               { $_  =~ s/\r|\n//g; 
                 $_ .= " \$\(NETCDF4_IO_OPTS\)\n" ; 
               }
             if (/^LIB.*=/) 
               { $_  =~ s/\r|\n//g ;
                 $_ .=" \$\(NETCDF4_DEP_LIB\)\n" ;
               }
           }
       }

    if ( ! (substr( $_, 0, 5 ) eq "#ARCH") ) { @machopts = ( @machopts, $_ ) ; }
    if ( substr( $_, 0, 10 ) eq "ENVCOMPDEF" )
    {
      @machopts = ( @machopts, "WRF_CHEM\t=\t$WRFCHEM \n" ) ;
    }
  }

# nesting support 
# 0 = no nesting (only selectable for serial and smpar)
# 1 = basic nesting (serial and smpar compile with RSL_LITE and STUBMPI; dmpar and dm+sm use RSL_LITE and MPI)
# 2 = nesting with prescribed moves  (add -DMOVE_NESTS to ARCHFLAGS)
# 3 = nesting with prescribed moves  (add -DMOVE_NESTS and -DVORTEX_CENTER to ARCHFLAGS) 

  for $paropt ( @platforms )
  {
    if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 0 
          && ( index( $_, $sw_os ) >= 0 ) && ( index( $_, $sw_mach ) >= 0 ) 
          && ( index($_, $paropt) >= 0 ) )
    {
      # We are cycling through the configure.defaults file again.
      # This bit tries to match the line corresponding to the option we previously selected.
      $x=substr($_,6) ;
      $x =~ s/#.*$//g ;
      chomp($x) ;
      $x = $x." (".$paropt.")" ;
      if ( $x eq $optarray[$optchoice] )
      {

        if($ENV{WRF_HYDRO} eq 1) {
           $tt = `cd hydro; ./wrf_hydro_config "$x" "$paropt"`;
        }

        $latchon = 1 ;
        $sw_ompparallel = "" ;
        $sw_dmparallel = "" ;
        $validresponse = 0 ;
        #only allow parallel netcdf if the user has chosen parallel option
        if ( $paropt ne 'dmpar' && $paropt ne 'dm+sm' ) { $sw_pnetcdf_path = "" ; }
        #
        until ( $validresponse ) {
          if ( $ENV{WRF_DA_CORE} eq "1" || $sw_da_core eq "-DDA_CORE=1" ) {
             $response = 1 ;
          } elsif ( $sw_wrfplus_core eq "-DWRFPLUS=1" ) {
             $response = 0 ;
          } elsif ( $ENV{HWRF} ) {
             printf "HWRF requires moving nests";
             $response = "2\n";
          } else {
             if ( $paropt eq 'serial' || $paropt eq 'smpar' ) {
               printf "Compile for nesting? (0=no nesting, 1=basic, 2=preset moves, 3=vortex following) [default 0]: " ;
             } elsif ( $ENV{WRF_NMM_CORE} eq "1" ) {
               printf "Compile for nesting? (1=basic, 2=preset moves) [default 1]: " ;
             } else {
               printf "Compile for nesting? (1=basic, 2=preset moves, 3=vortex following) [default 1]: " ;
             }
             $response = <STDIN> ;
          } 
          printf "\n" ;
          lc $response ;
          chop $response ;
          if ( $response == "" || ($response >= 0 && $response <= 3) )
            { $validresponse = 1 ; }
          else
            { printf("\nInvalid response (%d)\n",$response);}
        }
        if ( $response == "" ) { 
          if ( ( $paropt eq 'serial' || $paropt eq 'smpar' ) ) { $response = 0 ; }
          else                                                 { $response = 1 ; }
        }
        if ( $response == 0 ) {
          if ( ! ( $paropt eq 'serial' || $paropt eq 'smpar' ) ) { $response = 1 ; }
        } 
        $response_nesting = $response ;
        if ( ( $response == 1 ) || ( $response == 2 ) || ( $response == 3 ) ) {
          if ( ( $paropt eq 'serial' || $paropt eq 'smpar' ) ) {   # nesting without MPI
            $sw_stubmpi = "-DSTUBMPI" ;
            if ( $sw_os ne "CYGWIN_NT" ) {
              $sw_comms_lib = "\$(WRF_SRC_ROOT_DIR)/external/RSL_LITE/librsl_lite.a" ;
            } else {
              $sw_comms_lib = "../external/RSL_LITE/librsl_lite.a" ;
            }
            $sw_comms_external = "\$(WRF_SRC_ROOT_DIR)/external/RSL_LITE/librsl_lite.a gen_comms_rsllite module_dm_rsllite" ;
            $sw_dmparallel = "RSL_LITE" ;
            $sw_dmparallelflag = "-DDM_PARALLEL" ;
          }
        } 
        if ( $response == 2 ) {
          $sw_nest_opt = "-DMOVE_NESTS" ; 
          if ( $ENV{'TERRAIN_AND_LANDUSE'} eq "1" ) {
            $sw_terrain_and_landuse =" -DTERRAIN_AND_LANDUSE" ;
            $sw_nest_opt = $sw_nest_opt . $sw_terrain_and_landuse; 
          }  
        } elsif ( $response == 3 ) {
          $sw_nest_opt = "-DMOVE_NESTS -DVORTEX_CENTER" ; 
          if ( $ENV{'TERRAIN_AND_LANDUSE'} eq "1" ) {
            $sw_terrain_and_landuse =" -DTERRAIN_AND_LANDUSE" ;
            $sw_nest_opt = $sw_nest_opt . $sw_terrain_and_landuse; 
          }
        }
        if ( $paropt eq 'smpar' || $paropt eq 'dm+sm' ) { $sw_ompparallel = "OMP" ; }
        if ( $paropt eq 'dmpar' || $paropt eq 'dm+sm' ) { 
          if ( $sw_os ne "CYGWIN_NT" ) {
            $sw_comms_lib = "\$(WRF_SRC_ROOT_DIR)/external/RSL_LITE/librsl_lite.a" ;
            if ( $sw_wrf_core eq "4D_DA_CORE" )
            {
              $sw_comms_lib = "\$(WRFPLUS_DIR)/external/RSL_LITE/librsl_lite.a" ;
            }
          } else {
            $sw_comms_lib = "../external/RSL_LITE/librsl_lite.a" ;
          }
          $sw_comms_external = "\$(WRF_SRC_ROOT_DIR)/external/RSL_LITE/librsl_lite.a gen_comms_rsllite module_dm_rsllite" ;
          $sw_dmparallel = "RSL_LITE" ;
          $sw_dmparallelflag = "-DDM_PARALLEL" ;
          $sw_fc = "\$(DM_FC)" ;
          $sw_cc = "\$(DM_CC)" ;
        }  # only one option in v3.0

        $sw_ifort_r8 = 0 ;
        if ( index ( $x, "ifort" ) > -1 || index ( $x, "intel compiler" ) > -1 ) {
          if ( $sw_rwordsize == 8 ) {
            $sw_ifort_r8 = 1 ;
          }
        }
      }
    }
  }
}

if ($latchon == 0) { # Never hurts to check that we actually found the option again.
  unlink "configure.wrf";
  print "\nERROR ERROR ERROR ERROR\n\n";
  print "SOMETHING TERRIBLE HAS HAPPENED: configure.wrf not created correctly.\n";
  print 'Check "$x" and "$optarray[$optchoice]"';
  die   "\n\nERROR ERROR ERROR ERROR\n\n";
}

close CONFIGURE_DEFAULTS ;
close POSTAMBLE ;
close ARCH_NOOPT_EXCEPTIONS ;

open CONFIGURE_WRF, "> configure.wrf" or die "cannot append configure.wrf" ;
open ARCH_PREAMBLE, "< arch/preamble" or die "cannot open arch/preamble" ;
my @preamble;
# apply substitutions to the preamble...
while ( <ARCH_PREAMBLE> )
  {
  # ESMF substitutions in preamble
  if ( $sw_esmflib_path && $sw_esmfinc_path )
    {
    $_ =~ s/ESMFCOUPLING/1/g ;
    $_ =~ s:ESMFMODDEPENDENCE:\$\(WRF_SRC_ROOT_DIR\)/external/io_esmf/module_utility.o:g ;
    $_ =~ s:ESMFMODINC:-I$sw_esmfinc_path -I\$\(WRF_SRC_ROOT_DIR\)/main:g ;
    $_ =~ s:ESMFIOINC:-I\$\(WRF_SRC_ROOT_DIR\)/external/io_esmf:g ;
    $_ =~ s:ESMFIODEFS:-DESMFIO:g ;
    $_ =~ s:ESMFTARGET:wrfio_esmf:g ;
    $_ =~ s:\# ESMFINCLUDEGOESHERE:include \$\(ESMFLIB\)/esmf.mk: ;

    }
  else
    {
    $_ =~ s/ESMFCOUPLING/0/g ;
    $_ =~ s:ESMFMODDEPENDENCE:\$\(WRF_SRC_ROOT_DIR\)/external/esmf_time_f90/module_utility.o:g ;
    $_ =~ s:ESMFMODINC::g ;
    $_ =~ s:ESMFIOINC:-I\$\(WRF_SRC_ROOT_DIR\)/external/esmf_time_f90:g ;
    $_ =~ s:ESMFIODEFS::g ;
    $_ =~ s:ESMFTARGET:esmf_time:g ;
    }
  if ( $ENV{HWRF} )
    {
    $_ =~ s:CONFIGURE_ATMOCN_LIB:-L\$\(WRF_SRC_ROOT_DIR\)/external/atm_ocn  -latm_ocn:g ;
    $_ =~ s/CONFIGURE_ATMOCN/atm_ocn/g ;
    }
  else
    {
    $_ =~ s:CONFIGURE_ATMOCN_LIB::g ;
    $_ =~ s/CONFIGURE_ATMOCN//g ;
    }
  $_ =~ s:CONFIGURE_EM_CORE:$sw_em_core:g ;
  $_ =~ s:CONFIGURE_DA_CORE:$sw_da_core:g ;
  $_ =~ s:CONFIGURE_WRFPLUS_CORE:$sw_wrfplus_core:g ;
  $_ =~ s:CONFIGURE_NMM_CORE:$sw_nmm_core:g ;
  $_ =~ s/CONFIGURE_CONFIG_LINE/$sw_config_line/g ;
  $_ =~ s/CONFIGURE_CONFIG_NUM/Compiler choice: $response_opt/g ;
  $_ =~ s/CONFIGURE_CONFIG_NEST/Nesting option: $response_nesting/g ;

  $_ =~ s/CONFIGURE_DEP_LIB_PATH/$sw_dep_lib_path/g ;

  if ( $sw_gpfs_path ne "" )
    { if (/^GPFS.*=/)
        { $_  =~ s/\r|\n//g;
          if ( $sw_gpfs_path ne "DEFAULT" )
            { $_ .= " -L" . $sw_gpfs_path ; }
          $_ .= " " . $sw_gpfs_lib . "\n" ;
        }
    }
  if ( $sw_curl_path ne "" )
    { if (/^CURL.*=/)
        { $_  =~ s/\r|\n//g;
          if ( $sw_curl_path ne "DEFAULT" ) 
            { $_ .= " -L" . $sw_curl_path ; }
          $_ .= " " . $sw_curl_lib . "\n" ;
        }
    }
  if ( $sw_dep_lib_path ne "" )
    { if (/^HDF5.*=/)
        { $_  =~ s/\r|\n//g;
          $_ .= " " . $sw_hdf5 . "\n" ;
        }
      if (/^ZLIB.*=/)
        { $_  =~ s/\r|\n//g;
          $_ .= " " . $sw_zlib . "\n" ;
        }
    }

  @preamble = ( @preamble, $_ ) ;
  }
close ARCH_PREAMBLE ;
print CONFIGURE_WRF @preamble  ;
close ARCH_PREAMBLE ;
printf CONFIGURE_WRF "# Settings for %s\n", $optarray[$optchoice] ;
print CONFIGURE_WRF @machopts  ;
print "$ENV{WRF_MARS}" ;
	if ( $ENV{WRF_MARS} || $ENV{WRF_TITAN} || $ENV{WRF_VENUS} )
{
    open ARCH_PLANETAMBLE, "< arch/planetamble" or die "cannot open arch/planetamble" ;
    while ( <ARCH_PLANETAMBLE> ) { print CONFIGURE_WRF } ;
    close ARCH_PLANETAMBLE ;
}

close CONFIGURE_WRF ;

printf "Configuration successful! \n" ;
printf "------------------------------------------------------------------------\n" ;


