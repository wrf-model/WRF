#!/usr/bin/perl
#
# Configuration script for WRFVAR code

$sw_registry = "" ;
$sw_em_core = "" ;
$sw_da_core = "" ;
$sw_nmm_core = "" ;
$sw_coamps_core = "" ;
$sw_exp_core = "" ;
$sw_perl_path = perl ;
$sw_netcdf_path = "" ;
$sw_pnetcdf_path = "" ;
$sw_phdf5_path=""; 
$sw_hdf4_path=""; 
$sw_hdfeos_path=""; 
$sw_jpeg_path=""; 
$sw_zlib_path=""; 
$sw_jasper_path=""; 
$sw_esmflib_path="";
$sw_esmfinc_path="";
$sw_blas_path=""; 
$sw_lapack_path=""; 
$sw_fftpack_path=""; 
$sw_bufr_path=""; 
$sw_ldflags=""; 
$sw_compileflags=""; 
$sw_coreflags=""; 
$sw_max_domains="1"; 
$sw_rwordsize="8"; 
$sw_promote_float=""; 
$sw_solver=""; 
$chem = 0 ;
$phdf5 = 0 ;
$pnetcdf = 0 ;
$grib1 = 0 ;
$grib2 = 0 ;
$sw_os = "ARCH" ;           # ARCH will match any
$sw_mach = "ARCH" ;         # ARCH will match any

# Transfer arguments to local variables

while ( substr( $ARGV[0], 0, 1 ) eq "-" ) {
  if ( substr( $ARGV[0], 1, 9 ) eq "registry=" ) {
    $sw_registry = substr( $ARGV[0], 10 ) ;
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "em_core=" ) {
    $sw_em_core = substr( $ARGV[0], 9 ) ;
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "da_core=" ) {
    $sw_da_core = substr( $ARGV[0], 9 ) ;
  }
  if ( substr( $ARGV[0], 1, 9 ) eq "nmm_core=" ) {
    $sw_nmm_core = substr( $ARGV[0], 10 ) ;
  }
  if ( substr( $ARGV[0], 1, 12 ) eq "coamps_core=" ) {
    $sw_coamps_core = substr( $ARGV[0], 13 ) ;
  }
  if ( substr( $ARGV[0], 1, 9 ) eq "exp_core=" ) {
    $sw_exp_core = substr( $ARGV[0], 10 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "perl=" ) {
    $sw_perl_path = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 7 ) eq "netcdf=" ) {
    $sw_netcdf_path = substr( $ARGV[0], 8 ) ;
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "pnetcdf=" ) {
    $sw_pnetcdf_path = substr( $ARGV[0], 9 ) ;
  }
  if ( substr( $ARGV[0], 1, 6 ) eq "phdf5=" ) {
    $sw_phdf5_path = substr( $ARGV[0], 7 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "hdf4=" ) {
    $sw_hdf4_path = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 7 ) eq "hdfeos=" ) {
    $sw_hdfeos_path = substr( $ARGV[0], 8 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "zlib=" ) {
    $sw_zlib_path = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "jpeg=" ) {
    $sw_jpeg_path = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "blas=" ) {
    $sw_blas_path = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 7 ) eq "lapack=" ) {
    $sw_lapack_path = substr( $ARGV[0], 8 ) ;
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "fftpack=" ) {
    $sw_fftpack_path = substr( $ARGV[0], 9 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "bufr=" ) {
    $sw_bufr_path = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 3 ) eq "os=" ) {
    $sw_os = substr( $ARGV[0], 4 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "mach=" ) {
    $sw_mach = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 12 ) eq "max_domains=" ) {
    $sw_max_domains = substr( $ARGV[0], 13 ) ;
  }
  if ( substr( $ARGV[0], 1, 10 ) eq "rwordsize=" ) {
    $sw_rwordsize = substr( $ARGV[0], 11 ) ;
  }
  if ( substr( $ARGV[0], 1, 14 ) eq "promote_float=" ) {
    $sw_promote_float = substr( $ARGV[0], 15 ) ;
  }
  if ( substr( $ARGV[0], 1, 7 ) eq "solver=" ) {
    $sw_solver = substr( $ARGV[0], 8 ) ;
  }
  if ( substr( $ARGV[0], 1, 10 ) eq "coreflags=" ) {
    $sw_coreflags = substr( $ARGV[0], 11 ) ;
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "ldflags=" ) {
    $sw_ldflags = substr( $ARGV[0], 9 ) ;
    # multiple options separated by spaces are passed in from sh script
    # separated by ! instead. Replace with spaces here.
    $sw_ldflags =~ s/!/ /g ;
  }
  if ( substr( $ARGV[0], 1, 13 ) eq "compileflags=" ) {
    $sw_compileflags = substr( $ARGV[0], 14 ) ;
    $sw_compileflags =~ s/!/ /g ;
    # look for each known option
    $where_index = index ( $sw_compileflags , "-DWRF_CHEM" ) ;
    if ( $where_index eq -1 ) {
      $chem = 0 ;
    } else {
      $chem = 1 ;
    } 
    $where_index = index ( $sw_compileflags , "-DPHDF5" ) ;
    if ( $where_index ne -1 ) {
      $phdf5 = 1 ;
    } 
    $where_index = index ( $sw_compileflags , "-DPNETCDF" ) ;
    if ( $where_index ne -1 ) {
      $pnetcdf = 1 ;
    } 
    $where_index = index ( $sw_compileflags , "-DGRIB1" ) ;
    if ( $where_index ne -1 ) {
      $grib1 = 1 ;
    } 
    $where_index = index ( $sw_compileflags , "-DGRIB2" ) ;
    if ( $where_index ne -1 ) {
      $grib2 = 1 ;
    } 
  }
  shift @ARGV ;
}

# The jasper library is required to build Grib2 I/O.  User must set 
# environment variable JASPER to point to it prior to running configure.  
 if ( $ENV{JASPER} )
   {
   printf "Configuring to use jasper library to build Grib2 I/O...\n" ;
   printf("  \$JASPER = %s\n",$ENV{JASPER});
   $sw_jasper_path = $ENV{JASPER}; 
   }
 else
   {
   printf "\$JASPER not found in environment, configuring to build without grib2 I/O...\n" ;
   }

# A separately-installed ESMF library is required to build the ESMF 
# implementation of WRF IOAPI in external/io_esmf.  This is needed 
# to couple WRF with other ESMF components.  User must set environment 
# variables ESMF_LIB and ESMF_INC to paths ESMF to library and include 
# files to enable this feature prior to running configure.
 if ( $ENV{ESMF_LIB} && $ENV{ESMF_INC} )
   {
   printf "Configuring to use ESMF library to build WRF...\n" ;
   printf "WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING\n" ;
   printf "WARNING:  THIS IS AN EXPERIMENTAL CONFIGURATION\n" ;
   printf "WARNING:  IT DOES NOT WORK WITH NESTING\n" ;
   printf "WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING-WARNING\n" ;
   printf("  \$ESMF_LIB = %s\n",$ENV{ESMF_LIB});
   printf("  \$ESMF_INC = %s\n",$ENV{ESMF_INC});
   $sw_esmflib_path = $ENV{ESMF_LIB};
   $sw_esmfinc_path = $ENV{ESMF_INC};
   }

# parse the configure.defaults file

$validresponse = 0 ;

# Display the choices to the user and get selection
until ( $validresponse ) {
  printf "------------------------------------------------------------------------\n" ;
  printf "Please select from among the following supported platforms.\n\n" ;

  $opt = 1 ;
  open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" 
      or die "Cannot open ./arch/configure.default for reading" ;
  while ( <CONFIGURE_DEFAULTS> ) {
    if ( substr( $_, 0, 5 ) eq "#ARCH" && 
       ( index( $_, $sw_os ) >= 0 ) && ( index( $_, $sw_mach ) >= 0 ) ) {
      $optstr[$opt] = substr($_,6) ;
      $optstr[$opt] =~ s/^[ 	]*// ;
      if ( substr( $optstr[$opt], 0,4 ) ne "NULL" ) {
        printf "  %2d.  %s",$opt,$optstr[$opt] ;
        $opt++ ;
      }
    }
  }
  close CONFIGURE_DEFAULTS ;

  $opt -- ;

  printf "\nEnter selection [%d-%d] : ",1,$opt ;
  $response = <STDIN> ;

  if ( $response == -1 ) { exit ; }

  if ( $response >= 1 && $response <= $opt ) { 
    $validresponse = 1 ;
  } else { 
    printf("\nInvalid response (%d)\n",$response);
  }
}
printf "------------------------------------------------------------------------\n" ;

$optchoice = $response ;


open CONFIGURE_PREAMBLE, "< ./arch/preamble" 
  or die "Cannot open ./arch/preamble for reading" ;
$latchon = 0 ;
while ( <CONFIGURE_PREAMBLE> ) {

  $_ =~ s/CONFIGURE_PERL_PATH/$sw_perl_path/g ;
  $_ =~ s/CONFIGURE_LDFLAGS/$sw_ldflags/g ;
  $_ =~ s/CONFIGURE_COREFLAGS/$sw_coreflags/g ;
  $_ =~ s/CONFIGURE_REGISTRY/$sw_registry/g ;
  $_ =~ s/CONFIGURE_DA_CORE/$sw_da_core/g ;
  $_ =~ s/CONFIGURE_EM_CORE/$sw_em_core/g ;
  $_ =~ s/CONFIGURE_NMM_CORE/$sw_nmm_core/g ;
  $_ =~ s/CONFIGURE_COAMPS_CORE/$sw_coamps_core/g ;
  $_ =~ s/CONFIGURE_EXP_CORE/$sw_exp_core/g ;
  $_ =~ s/CONFIGURE_MAX_DOMAINS/$sw_max_domains/g ;
  $_ =~ s/CONFIGURE_RWORDSIZE/$sw_rwordsize/g ;
  $_ =~ s/CONFIGURE_PROMOTE_FLOAT/$sw_promote_float/g ;
  $_ =~ s/CONFIGURE_SOLVER/$sw_solver/g ;
  
  if ( $sw_netcdf_path ) { 
    $_ =~ s:CONFIGURE_NETCDF_PATH:$sw_netcdf_path:g ;
    $_ =~ s:CONFIGURE_WRFIO_NF:wrfio_nf:g ;
    $_ =~ s:CONFIGURE_NETCDF_FLAG:-DNETCDF: ;
    $_ =~ s:CONFIGURE_NETCDF_LIBS:libwrfio_nf.a: ;
    $_ =~ s:CONFIGURE_NETCDF_LIB:-lwrfio_nf -L$sw_netcdf_path/lib -lnetcdf: ;
    $_ =~ s:CONFIGURE_NETCDF_INC:$sw_netcdf_path/include:g ;
  } else { 
    $_ =~ s:CONFIGURE_NETCDF_PATH::g ;
    $_ =~ s:CONFIGURE_WRFIO_NF::g ;
    $_ =~ s:CONFIGURE_NETCDF_FLAG::g ;
    $_ =~ s:CONFIGURE_NETCDF_LIBS::g ;
    $_ =~ s:CONFIGURE_NETCDF_LIB::g ;
    $_ =~ s:CONFIGURE_NETCDF_INC:.:g ;
  }

  if ( $pnetcdf == 1 && $sw_pnetcdf_path ) { 
     $_ =~ s:CONFIGURE_PNETCDF_PATH:$sw_pnetcdf_path:g ;
     $_ =~ s/CONFIGURE_WRFIO_PNF/wrfio_pnf/g ;
     $_ =~ s:CONFIGURE_PNETCDF_FLAG:-DPNETCDF:g ;
     $_ =~ s:CONFIGURE_PNETCDF_LIBS:libwrfio_pnf.a: ;
     $_ =~ s:CONFIGURE_PNETCDF_LIB:-lwrfio_pnf -L$sw_pnetcdf_path/src/lib -lpnetcdf: ;
     $_ =~ s:CONFIGURE_PNETCDF_INC:$sw_pnetcdf_path/src/libf: ;
  } else { 
     $_ =~ s:CONFIGURE_PNETCDF_PATH::g ;
     $_ =~ s/CONFIGURE_WRFIO_PNF//g ;
     $_ =~ s:CONFIGURE_PNETCDF_FLAG::g ;
     $_ =~ s:CONFIGURE_PNETCDF_LIBS::g ;
     $_ =~ s:CONFIGURE_PNETCDF_LIB::g ;
     $_ =~ s:CONFIGURE_PNETCDF_INC:.:g ;
  }

  if ( $phdf5 == 1 && $sw_phdf5_path ) { 
    $_ =~ s:CONFIGURE_PHDF5_PATH:$sw_phdf5_path: ;
    $_ =~ s:CONFIGURE_WRFIO_PHDF5:wrfio_phdf5:g ;
    $_ =~ s:CONFIGURE_PHDF5_FLAG:-DPHDF5: ;
    $_ =~ s:CONFIGURE_PHDF5_LIB:-lwrfio_phdf5 -L$sw_phdf5_path/lib -lhdf5_fortran -lhdf5 -lm -lz -lsz: ;
    $_ =~ s:CONFIGURE_PHDF5_INC:$sw_phdf5_path/include:g ;
  } else { 
    $_ =~ s:CONFIGURE_PHDF5_PATH::g ;
    $_ =~ s:CONFIGURE_WRFIO_PHDF5::g ;
    $_ =~ s:CONFIGURE_PHDF5_FLAG::g ;
    $_ =~ s:CONFIGURE_PHDF5_LIB::g ;
    $_ =~ s:CONFIGURE_PHDF5_INC:.:g ;
  }

  if ( $sw_hdf4_path ) { 
    $_ =~ s:CONFIGURE_HDF4_LIB:-L$sw_hdf4_path/lib -lmfhdf -ldf: ;
    $_ =~ s:CONFIGURE_HDF4_INC:$sw_hdf4_path/include: ;
  } else { 
    $_ =~ s:CONFIGURE_HDF4_LIB::g ;
    $_ =~ s:CONFIGURE_HDF4_INC:.:g ;
  }

  if ( $sw_hdfeos_path ) { 
    $_ =~ s:CONFIGURE_HDFEOS_LIB:-L$sw_hdfeos_path/lib -lhdfeos -lGctp: ;
    $_ =~ s:CONFIGURE_HDFEOS_INC:$sw_hdfeos_path/include: ;
  } else { 
    $_ =~ s:CONFIGURE_HDFEOS_LIB::g ;
    $_ =~ s:CONFIGURE_HDFEOS_INC:.:g ;
  }

  if ( $sw_zlib_path ) { 
    $_ =~ s:CONFIGURE_ZLIB_LIB:-L$sw_zlib_path/lib -lz: ;
  } else { 
    $_ =~ s:CONFIGURE_ZLIB_LIB::g ;
  }

  if ( $sw_jpeg_path ) { 
    $_ =~ s:CONFIGURE_JPEG_LIB:-L$sw_jpeg_path/lib -ljpeg: ;
  } else { 
    $_ =~ s:CONFIGURE_JPEG_LIB::g ;
  }

  if ( $grib1 == 1 || $grib2 == 1) {
    $_ =~ s:CONFIGURE_GRIB_SHARE_LIBS:\$(IO_GRIB_SHARE)/libio_grib1.a: ;
    $_ =~ s:CONFIGURE_GRIB_SHARE_LIB:-L$(IO_GRIB_SHARE) -lio_grib_share:g ;
  } else { 
    $_ =~ s:CONFIGURE_GRIB_SHARE_LIBS::g ;
    $_ =~ s:CONFIGURE_GRIB_SHARE_LIB::g ;
  }

  if ( $grib1 == 1 ) {
    $_ =~ s:CONFIGURE_WRFIO_GRIB1:wrfio_grib1:g ;
    $_ =~ s:CONFIGURE_GRIB1_FLAG:-DGRIB1:g ;
    $_ =~ s:CONFIGURE_GRIB1_INC:\$(IO_GRIB1):g ;
    $_ =~ s:CONFIGURE_GRIB1_LIBS:\$(IO_GRIB1)/libio_grib1.a: ;
    $_ =~ s:CONFIGURE_GRIB1_LIB:-L\$(IO_GRIB1) -lio_grib1:g ;
  } else { 
    $_ =~ s:CONFIGURE_WRFIO_GRIB1::g ;
    $_ =~ s:CONFIGURE_GRIB1_FLAG::g ;
    $_ =~ s:CONFIGURE_GRIB1_INC:.:g ;
    $_ =~ s:CONFIGURE_GRIB1_LIBS:: ;
    $_ =~ s:CONFIGURE_GRIB1_LIB::g ;
  }

  if ( $grib2 == 1 && $sw_jasper_path ) {
    $_ =~ s:CONFIGURE_WRFIO_GRIB2:wrfio_grib2:g ;
    $_ =~ s:CONFIGURE_GRIB2_FLAG:-DGRIB2:g ;
    $_ =~ s:CONFIGURE_GRIB2_INC:$sw_jasper_path/include:g ;
    $_ =~ s:CONFIGURE_GRIB2_LIBS:\$(IO_GRIB2)/libio_grib2.a: ;
    $_ =~ s:CONFIGURE_GRIB2_LIB:-L\$(IO_GRIB2) -lio_grib2 -L$sw_jasper_path/lib -ljasper:g ;
  } else { 
    $_ =~ s:CONFIGURE_WRFIO_GRIB2::g ;
    $_ =~ s:CONFIGURE_GRIB2_FLAG::g ;
    $_ =~ s:CONFIGURE_GRIB2_INC:.:g ;
    $_ =~ s:CONFIGURE_GRIB2_LIBS:: ;
    $_ =~ s:CONFIGURE_GRIB2_LIB::g ;
  }

  if ( $sw_esmflib_path && $sw_esmfinc_path ) {
    $_ =~ s:CONFIGURE_ESMF_IO_LIB:-L$sw_esmflib_path -lesmf -L../external/io_esmf -lwrfio_esmf \$\(ESMF_LIB_FLAGS\):g ;
    $_ =~ s:CONFIGURE_ESMF_IO_EXT_LIB:-L$sw_esmflib_path -lesmf -lwrfio_esmf \$\(ESMF_LIB_FLAGS\):g ;
    $_ =~ s:CONFIGURE_ESMF_COUPLING:1:g ;
    $_ =~ s:CONFIGURE_ESMF_MOD_DEPENDENCE:module_utility.o:g ;
    $_ =~ s:CONFIGURE_ESMF_MOD_INC:-I$sw_esmfinc_path -I../main:g ;
    $_ =~ s:CONFIGURE_ESMF_IO_INC:-I../external/io_esmf:g ;
    $_ =~ s:CONFIGURE_ESMF_IO_DEFS:-DESMFIO:g ;
    $_ =~ s:CONFIGURE_ESMF_TARGET:wrfio_esmf:g ;
  } else {
    $_ =~ s:CONFIGURE_ESMF_IO_LIB: -lesmf_time:g ;
    $_ =~ s:CONFIGURE_ESMF_IO_EXT_LIB: -lesmf_time:g ;
    $_ =~ s:CONFIGURE_ESMF_COUPLING:0:g ;
    $_ =~ s:CONFIGURE_ESMF_MOD_DEPENDENCE:module_utility.o:g ;
    $_ =~ s:CONFIGURE_ESMF_MOD_INC::g ;
    $_ =~ s:CONFIGURE_ESMF_IO_INC:-I../external/esmf_time_f90:g ;
    $_ =~ s:CONFIGURE_ESMF_IO_DEFS::g ;
    $_ =~ s:CONFIGURE_ESMF_TARGET:esmf_time:g ;
  }

  if ( $sw_bufr_path ) {
    $_ =~ s:CONFIGURE_BUFR_PATH:$sw_bufr_path:g ;
    $_ =~ s:CONFIGURE_BUFR_FLAG:-DBUFR: ;
    $_ =~ s:CONFIGURE_BUFR_LIB:-L$sw_bufr_path -lbufr: ;
    $_ =~ s:CONFIGURE_BUFR_INC:$sw_bufr_path: ;
  } else {
    $_ =~ s:CONFIGURE_BUFR_PATH::g ;
    $_ =~ s:CONFIGURE_BUFR_FLAG::g ;
    $_ =~ s:CONFIGURE_BUFR_LIB::g ;
    $_ =~ s:CONFIGURE_BUFR_INC:.:g ;
  }

  if ( $sw_fftpack_path ) {
    $_ =~ s:CONFIGURE_FFTPACK_PATH:$sw_fftpack_path:g ;
    $_ =~ s:CONFIGURE_FFTPACK_FLAG:-DFFTPACK: ;
    $_ =~ s:CONFIGURE_FFTPACK_LIB:-L$sw_fftpack_path -lfftpack: ;
    $_ =~ s:CONFIGURE_FFTPACK_INC:$sw_fftpack_path: ;
  } else {
    $_ =~ s:CONFIGURE_FFTPACK_PATH::g ;
    $_ =~ s:CONFIGURE_FFTPACK_FLAG::g ;
    $_ =~ s:CONFIGURE_FFTPACK_LIB::g ;
    $_ =~ s:CONFIGURE_FFTPACK_INC:.:g ;
  }

  if ( $sw_blas_path ) {
    $_ =~ s:CONFIGURE_BLAS_PATH:$sw_blas_path:g ;
    $_ =~ s:CONFIGURE_BLAS_FLAG:-DBLAS: ;
    $_ =~ s:CONFIGURE_BLAS_LIB:-L$sw_blas_path -lblas: ;
    $_ =~ s:CONFIGURE_BLAS_INC:$sw_blas_path: ;
  } else {
    $_ =~ s:CONFIGURE_BLAS_PATH::g ;
    $_ =~ s:CONFIGURE_BLAS_FLAG::g ;
    $_ =~ s:CONFIGURE_BLAS_LIB::g ;
    $_ =~ s:CONFIGURE_BLAS_INC:.:g ;
  }

  if ( $sw_lapack_path ) {
    $_ =~ s:CONFIGURE_LAPACK_PATH:$sw_lapack_path:g ;
    $_ =~ s:CONFIGURE_LAPACK_FLAG:-DLAPACK: ;
    $_ =~ s:CONFIGURE_LAPACK_LIB:-L$sw_lapack_path -llapack: ;
    $_ =~ s:CONFIGURE_LAPACK_INC:$sw_lapack_path: ;
  } else {
    $_ =~ s:CONFIGURE_LAPACK_PATH::g ;
    $_ =~ s:CONFIGURE_LAPACK_FLAG::g ;
    $_ =~ s:CONFIGURE_LAPACK_LIB::g ;
    $_ =~ s:CONFIGURE_LAPACK_INC:.:g ;
  }

  if ( $chem ) {
    $_ =~ s:CONFIGURE_CHEM_FLAG:-DWRF_CHEM: ;
  } else {
    $_ =~ s:CONFIGURE_CHEM_FLAG::g ;
  }

  @machopts1 = ( @machopts1, $_ ) ;
}
close CONFIGURE_PREAMBLE ;

open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" 
  or die "Cannot open ./arch/configure.defaults for reading" ;
$latchon = 0 ;
while ( <CONFIGURE_DEFAULTS> ) {
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 1 ) {
    $latchon = 0 ;
  }
  if ( $latchon == 1 ) {

    @machopts2 = ( @machopts2, $_ ) ;

  }
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 0 ) {
    $x=substr($_,6) ;
    $x=~s/^[     ]*// ;
    if ( $x eq $optstr[$optchoice] ) {
      $latchon = 1 ;
    }
  }
}
close CONFIGURE_DEFAULTS ;



printf "\nYou have chosen: %s",$optstr[$optchoice] ;
printf "The options for this platform have been written to the file configure.wrfvar\n" ;
printf "If you wish to change settings, please edit that file.\n" ;
printf "If you wish to change the default options, edit the file:\n\n" ;
printf "     arch/configure.defaults\n" ;
printf "\n" ;

open CONFIGURE_WRF, "> configure.wrfvar" 
  or die "cannot append configure.wrfvar" ;

print CONFIGURE_WRF "# configure.wrfvar\n";
print CONFIGURE_WRF "#\n";
print CONFIGURE_WRF "# This file was automatically generated by the configure script in the\n";
print CONFIGURE_WRF "# top level directory. You may make changes to the settings in this\n";
print CONFIGURE_WRF "# file but be aware they will be overwritten each time you run configure.\n";
print CONFIGURE_WRF "# Ordinarily, it is necessary to run configure once, when the code is\n";
print CONFIGURE_WRF "# first installed.\n";
print CONFIGURE_WRF "#\n";
print CONFIGURE_WRF "# To permanently change options, change the settings for your platform\n";
print CONFIGURE_WRF "# in the file arch/configure.defaults then rerun configure.\n";
print CONFIGURE_WRF "#\n";
# add preamble
print CONFIGURE_WRF @machopts1  ;
print CONFIGURE_WRF "#--------------------------------------------------------\n";
print CONFIGURE_WRF "#Platform Dependant\n";
print CONFIGURE_WRF "#--------------------------------------------------------\n";
# add platform dependant
printf CONFIGURE_WRF "# $optstr[$optchoice]\n" ;
print CONFIGURE_WRF @machopts2  ;

print CONFIGURE_WRF "#--------------------------------------------------------\n";
print CONFIGURE_WRF "#Postamble\n";
print CONFIGURE_WRF "#--------------------------------------------------------\n";


open ARCH_POSTAMBLE, "< arch/postamble" 
  or die "cannot open arch/postamble" ;

while ( <ARCH_POSTAMBLE> ) { 
  print CONFIGURE_WRF
}
close ARCH_POSTAMBLE ;
close CONFIGURE_WRF ;

printf "Configuration successful for $sw_os. To build the model './compile target'.\n" ;
printf "--------------------------------------------------------------------------------\n" ;


