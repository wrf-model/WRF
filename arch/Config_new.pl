#!/usr/bin/perl
#
# Configuration script for WRF prototype code
# 
# Be sure to run as ./configure (to avoid getting a system configure command by mistake)
#

$sw_perl_path = perl ;
$sw_netcdf_path = "" ;
$sw_pnetcdf_path = "" ;
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
$sw_crtm_flag = "" ;
$sw_crtm_inc = "" ;
$WRFCHEM = 0 ;
$sw_os = "ARCH" ;           # ARCH will match any
$sw_mach = "ARCH" ;         # ARCH will match any
$sw_wrf_core = "" ;
$sw_da_core = "-DDA_CORE=\$\(WRF_DA_CORE\)" ;
$sw_nmm_core = "-DNMM_CORE=\$\(WRF_NMM_CORE\)" ;
$sw_em_core = "-DEM_CORE=\$\(WRF_EM_CORE\)" ;
$sw_exp_core = "-DEXP_CORE=\$\(WRF_EXP_CORE\)" ;
$sw_coamps_core = "-DCOAMPS_CORE=\$\(WRF_COAMPS_CORE\)" ;
$sw_dfi_radar = "-DDFI_RADAR=\$\(WRF_DFI_RADAR\)" ;
$sw_dmparallel = "" ;
$sw_ompparallel = "" ;
$sw_stubmpi = "" ;
$sw_usenetcdff = "" ;    # for 3.6.2 and greater, the fortran bindings might be in a separate lib file
$sw_time = "" ;          # name of a timer to time fortran compiles, e.g. timex or time
$sw_ifort_r8 = 0 ;

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
  if ( substr( $ARGV[0], 1, 8 ) eq "pnetcdf=" )
  {
    $sw_pnetcdf_path = substr( $ARGV[0], 9 ) ;
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
      $sw_nmm_core = "-DNMM_CORE=0" ;
      $sw_exp_core = "-DEXP_CORE=0" ;
      $sw_coamps_core = "-DCOAMPS_CORE=0" ;
      $sw_dfi_radar = "-DDFI_RADAR=0" ;
    }
    if ( index ( $sw_wrf_core , "DA_CORE" ) > -1 ) 
    {
      $sw_em_core = "-DEM_CORE=1" ;
      $sw_da_core = "-DDA_CORE=1" ;
      $sw_nmm_core = "-DNMM_CORE=0" ;
      $sw_exp_core = "-DEXP_CORE=0" ;
      $sw_coamps_core = "-DCOAMPS_CORE=0" ;
      $sw_dfi_radar = "-DDFI_RADAR=0" ;
    }
    if ( index ( $sw_wrf_core , "DFI_RADAR" ) > -1 )
    {
      $sw_em_core = "-DEM_CORE=1" ;
      $sw_da_core = "-DDA_CORE=0" ;
      $sw_nmm_core = "-DNMM_CORE=0" ;
      $sw_exp_core = "-DEXP_CORE=0" ;
      $sw_coamps_core = "-DCOAMPS_CORE=0" ;
      $sw_dfi_radar = "-DDFI_RADAR=1" ;
    }
    if ( index ( $sw_wrf_core , "NMM_CORE" ) > -1 ) 
    {
      $sw_em_core = "-DEM_CORE=0" ;
      $sw_da_core = "-DDA_CORE=0" ;
      $sw_nmm_core = "-DNMM_CORE=1" ;
      $sw_exp_core = "-DEXP_CORE=0" ;
      $sw_coamps_core = "-DCOAMPS_CORE=0" ;
      $sw_dfi_radar = "-DDFI_RADAR=0" ;
    }
    if ( index ( $sw_wrf_core , "EXP_CORE" ) > -1 ) 
    {
      $sw_em_core = "-DEM_CORE=0" ;
      $sw_da_core = "-DDA_CORE=0" ;
      $sw_nmm_core = "-DNMM_CORE=0" ;
      $sw_exp_core = "-DEXP_CORE=1" ;
      $sw_coamps_core = "-DCOAMPS_CORE=0" ;
      $sw_dfi_radar = "-DDFI_RADAR=0" ;
    }
    if ( index ( $sw_wrf_core , "COAMPS_CORE" ) > -1 ) 
    {
      $sw_em_core = "-DEM_CORE=0" ;
      $sw_da_core = "-DDA_CORE=0" ;
      $sw_nmm_core = "-DNMM_CORE=0" ;
      $sw_exp_core = "-DEXP_CORE=0" ;
      $sw_coamps_core = "-DCOAMPS_CORE=1" ;
      $sw_dfi_radar = "-DDFI_RADAR=0" ;
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
 if ( $ENV{JASPERLIB} && $ENV{JASPERINC} )
   {
   printf "Configuring to use jasper library to build Grib2 I/O...\n" ;
   printf("  \$JASPERLIB = %s\n",$ENV{JASPERLIB});
   printf("  \$JASPERINC = %s\n",$ENV{JASPERINC});
   $sw_jasperlib_path = $ENV{JASPERLIB}; 
   $sw_jasperinc_path = $ENV{JASPERINC}; 
   }
 else
   {
   printf "\$JASPERLIB or \$JASPERINC not found in environment, configuring to build without grib2 I/O...\n" ;
   }

# When compiling DA code, we need to always use 8-byte reals.
 if ( $ENV{WRF_DA_CORE} eq "1" || $sw_da_core eq "-DDA_CORE=1" )
   {
     $sw_rwordsize = "8";  
     if ( $ENV{CRTM} )
       {
       $sw_crtm_flag = "-DCRTM";
       $sw_crtm_inc = "-I$ENV{CRTM}/src";
       }
     if ( $ENV{RTTOV} )
       {
       $sw_rttov_flag = "-DRTTOV";
       $sw_rttov_inc = "-I$ENV{RTTOV}/src";
       }
   }

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

@platforms = qw ( serial smpar dmpar dm+sm ) ;

# Display the choices to the user and get selection
until ( $validresponse ) {
  printf "------------------------------------------------------------------------\n" ;
  printf "Please select from among the following supported platforms.\n\n" ;

  $opt = 1 ;
  open CONFIGURE_DEFAULTS, "< ./arch/configure_new.defaults" 
      or die "Cannot open ./arch/configure_new.defaults for reading" ;
  while ( <CONFIGURE_DEFAULTS> )
  {
    for $paropt ( @platforms )
    {
      if ( substr( $_, 0, 5 ) eq "#ARCH"
          && ( index( $_, $sw_os ) >= 0 ) && ( index( $_, $sw_mach ) >= 0 ) 
          && ( index($_, $paropt) >= 0 ) )
      {
        $optstr[$opt] = substr($_,6) ;
        $optstr[$opt] =~ s/^[ 	]*// ;
        $optstr[$opt] =~ s/#.*$//g ;
        chomp($optstr[$opt]) ;
        $optstr[$opt] = $optstr[$opt]." (".$paropt.")" ;
        if ( substr( $optstr[$opt], 0,4 ) ne "NULL" )
        {
          printf "  %2d.  %s\n",$opt,$optstr[$opt] ;
          $opt++ ;
        }
      }
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
}
printf "------------------------------------------------------------------------\n" ;

$optchoice = $response ;

open CONFIGURE_DEFAULTS, "cat ./arch/configure_new.defaults |"  ;
$latchon = 0 ;
while ( <CONFIGURE_DEFAULTS> )
{
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 1 )
  {
    close CONFIGURE_DEFAULTS ;
    if ( $sw_opt_level eq "-f" ) {
      open CONFIGURE_DEFAULTS, "cat ./arch/postamble_new ./arch/noopt_exceptions_f |"  or die "horribly" ;
    } else {
      open CONFIGURE_DEFAULTS, "cat ./arch/postamble_new ./arch/noopt_exceptions |"  or die "horribly" ;
    }
  }
  if ( $latchon == 1 )
  {
    $_ =~ s/CONFIGURE_PERL_PATH/$sw_perl_path/g ;
    $_ =~ s/CONFIGURE_NETCDF_PATH/$sw_netcdf_path/g ;
    $_ =~ s/CONFIGURE_PNETCDF_PATH/$sw_pnetcdf_path/g ;
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
    $_ =~ s/CONFIGURE_CRTM_FLAG/$sw_crtm_flag/g ;
    $_ =~ s/CONFIGURE_CRTM_INC/$sw_crtm_inc/g ;
    $_ =~ s/CONFIGURE_RTTOV_FLAG/$sw_rttov_flag/g ;
    $_ =~ s/CONFIGURE_RTTOV_INC/$sw_rttov_inc/g ;
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
        if ( $sw_os == Interix ) {
	  $_ =~ s:CONFIGURE_NETCDF_LIB_PATH:\$\(WRF_SRC_ROOT_DIR\)/external/io_netcdf/libwrfio_nf.a -L$sw_netcdf_path/lib $sw_usenetcdff -lnetcdf : ;
        } else {
	  $_ =~ s:CONFIGURE_NETCDF_LIB_PATH:-L\$\(WRF_SRC_ROOT_DIR\)/external/io_netcdf -lwrfio_nf -L$sw_netcdf_path/lib $sw_usenetcdff -lnetcdf : ;
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
        if ( $sw_os == Interix ) {
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


    # ESMF substitutions in configure.defaults
    if ( $sw_esmflib_path && $sw_esmfinc_path )
      {
      $_ =~ s:CONFIGURE_ESMF_FLAG:-DESMFIO:g ;
      $_ =~ s:ESMFIOLIB:-L$sw_esmflib_path -lesmf -L\$\(WRF_SRC_ROOT_DIR\)/external/io_esmf -lwrfio_esmf \$\(ESMF_LIB_FLAGS\):g ;
      $_ =~ s:ESMFIOEXTLIB:-L$sw_esmflib_path -lesmf -L\$\(WRF_SRC_ROOT_DIR\)/external/io_esmf -lwrfio_esmf \$\(ESMF_LIB_FLAGS\):g ;
      $_ =~ s:ESMFLIBFLAG:\$\(ESMF_LDFLAG\):g ;
      }
    else
      {
        $_ =~ s:CONFIGURE_ESMF_FLAG::g ;
        $_ =~ s:ESMFLIBFLAG::g ;
        if ( $sw_os == Interix ) {
           $_ =~ s:ESMFIOLIB:\$\(WRF_SRC_ROOT_DIR\)/external/esmf_time_f90/libesmf_time.a:g ;
           $_ =~ s:ESMFIOEXTLIB:-L\$\(WRF_SRC_ROOT_DIR\)/external/esmf_time_f90/libesmf_time.a:g ;
        } else {
           $_ =~ s:ESMFIOLIB:-L\$\(WRF_SRC_ROOT_DIR\)/external/esmf_time_f90 -lesmf_time:g ;
           $_ =~ s:ESMFIOEXTLIB:-L\$\(WRF_SRC_ROOT_DIR\)/external/esmf_time_f90 -lesmf_time:g ;
        }
      }
     if ( $ENV{HWRF} )
       {
        $_ =~ s:CONFIGURE_ATMPOM_LIB:-L\$\(WRF_SRC_ROOT_DIR\)/external/atm_pom  -latm_pom:g ;
        $_ =~ s:CONFIGURE_ATMPOM_INC:-I\$\(WRF_SRC_ROOT_DIR\)/external/atm_pom:g;
        $_ =~ s/CONFIGURE_ATMPOM/atm_pom/g ;
       }
     else
       {
        $_ =~ s:CONFIGURE_ATMPOM_LIB::g ;
        $_ =~ s/CONFIGURE_ATMPOM//g ;
        $_ =~ s:CONFIGURE_ATMPOM_INC::g;
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
      $x=substr($_,6) ;
      $x=~s/^[     ]*// ;
      $x =~ s/#.*$//g ;
      chomp($x) ;
      $x = $x." (".$paropt.")" ;
      if ( $x eq $optstr[$optchoice] )
      {
        $latchon = 1 ;
        $sw_ompparallel = "" ;
        $sw_dmparallel = "" ;
        $validresponse = 0 ;
        #only allow parallel netcdf if the user has chosen parallel option
        if ( $paropt ne 'dmpar' && $paropt ne 'dm+sm' ) { $sw_pnetcdf_path = "" ; }
        #
        until ( $validresponse ) {
          if ( $paropt eq 'serial' || $paropt eq 'smpar' ) {
            printf "Compile for nesting? (0=no nesting, 1=basic, 2=preset moves, 3=vortex following) [default 0]: " ;
          } else {
            printf "Compile for nesting? (1=basic, 2=preset moves, 3=vortex following) [default 1]: " ;
          }
          if ( $ENV{WRF_DA_CORE} eq "1" || $sw_da_core eq "-DDA_CORE=1" ) {
             $response = 1 ;
          } elsif ( $ENV{HWRF} ) {
             printf "HWRF requires moving nests";
             $response = "2\n";
          } else {
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
        } elsif ( $response == 3 ) {
          $sw_nest_opt = "-DMOVE_NESTS -DVORTEX_CENTER" ; 
        }
        if ( $paropt eq 'smpar' || $paropt eq 'dm+sm' ) { $sw_ompparallel = "OMP" ; }
        if ( $paropt eq 'dmpar' || $paropt eq 'dm+sm' ) { 
          if ( $sw_os ne "CYGWIN_NT" ) {
            $sw_comms_lib = "\$(WRF_SRC_ROOT_DIR)/external/RSL_LITE/librsl_lite.a" ;
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
close CONFIGURE_DEFAULTS ;
close POSTAMBLE ;
close ARCH_NOOPT_EXCEPTIONS ;

open CONFIGURE_WRF, "> configure.wrf" or die "cannot append configure.wrf" ;
open ARCH_PREAMBLE, "< arch/preamble_new" or die "cannot open arch/preamble_new" ;
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
    $_ =~ s:CONFIGURE_ATMPOM_LIB:-L\$\(WRF_SRC_ROOT_DIR\)/external/atm_pom  -latm_pom:g ;
    $_ =~ s/CONFIGURE_ATMPOM/atm_pom/g ;
    }
  else
    {
    $_ =~ s:CONFIGURE_ATMPOM_LIB::g ;
    $_ =~ s/CONFIGURE_ATMPOM//g ;
    }
  $_ =~ s:CONFIGURE_EM_CORE:$sw_em_core:g ;
  $_ =~ s:CONFIGURE_DA_CORE:$sw_da_core:g ;
  $_ =~ s:CONFIGURE_NMM_CORE:$sw_nmm_core:g ;
  $_ =~ s:CONFIGURE_COAMPS_CORE:$sw_coamps_core:g ;
  $_ =~ s:CONFIGURE_EXP_CORE:$sw_exp_core:g ;
  $_ =~ s:CONFIGURE_DFI_RADAR:$sw_dfi_radar:g ;

  @preamble = ( @preamble, $_ ) ;
  }
close ARCH_PREAMBLE ;
print CONFIGURE_WRF @preamble  ;
close ARCH_PREAMBLE ;
printf CONFIGURE_WRF "# Settings for %s\n", $optstr[$optchoice] ;
print CONFIGURE_WRF @machopts  ;
print "$ENV{WRF_MARS}" ;
if ( $ENV{WRF_MARS} || $ENV{WRF_TITAN} || $ENV{WRF_VENUS} )
{
    open ARCH_PLANETAMBLE, "< arch/planetamble" or die "cannot open arch/planetamble" ;
    while ( <ARCH_PLANETAMBLE> ) { print CONFIGURE_WRF } ;
    close ARCH_PLANETAMBLE ;
}

close CONFIGURE_WRF ;

printf "Configuration successful. To build the model type compile . \n" ;
printf "------------------------------------------------------------------------\n" ;


