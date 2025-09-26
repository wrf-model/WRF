#!/usr/bin/perl
#
# Configuration script for WPS code
# 
# Be sure to run as ./configure (to avoid getting a system configure command by mistake)
# There are two (2) reads of the configure.defaults one to present the user with
# the appropriate options for the type of machine, and the OS, and the compiler!

$sw_perl_path   = perl;
$sw_wrf_path   = "<SET ME>";
$sw_netcdf_path = "";
$sw_netcdff_lib = "";
$sw_phdf5_path  = ""; 
$sw_jasperlib_path = ""; 
$sw_jasperinc_path = ""; 
$sw_ldflags      = ""; 
$sw_compileflags = ""; 
$sw_os   = "ARCH" ;         # ARCH will match any
$sw_mach = "ARCH" ;         # ARCH will match any
#$sw_compL = "";
#$sw_compI = "";
#$sw_fdefs = "";
#$sw_fc = "";
#$sw_cc = "";
#$sw_mpi = "";

while(substr( $ARGV[0], 0, 1 ) eq "-")
{   
    if(substr( $ARGV[0], 1, 5 ) eq "perl=")
    {
        $sw_perl_path = substr( $ARGV[0], 6);
    }
    if(substr( $ARGV[0], 1, 7 ) eq "wrfdir=")
    {
        $sw_wrf_path = substr( $ARGV[0], 8);
    }
    if(substr( $ARGV[0], 1, 7 ) eq "netcdf=")
    {
        $sw_netcdf_path = substr( $ARGV[0], 8);
    }
    if(substr( $ARGV[0], 1, 8 ) eq "netcdff=")
    {
        $sw_netcdff_lib = substr( $ARGV[0], 9);
    }
    if(substr( $ARGV[0], 1, 6 ) eq "phdf5=")
    {
        $sw_phdf5_path = substr( $ARGV[0], 7);
    }
    if(substr( $ARGV[0], 1, 3 ) eq "os=")
    {
        $sw_os = substr( $ARGV[0], 4 );
    }
    if(substr( $ARGV[0], 1, 5 ) eq "mach=")
    {     
        $sw_mach = substr( $ARGV[0], 6 );
    }
    if(substr( $ARGV[0], 1, 8 ) eq "ldflags=")
    {
        $sw_ldflags = substr( $ARGV[0], 9 );
        # multiple options separated by spaces are passed in from sh script
        # separated by ! instead. Replace with spaces here.
        $sw_ldflags =~ s/!/ /g ;
    }    
    shift @ARGV;
} # end while

# The jasper library is required to build Grib2 I/O.  User must set 
# environment variables JASPERLIB and JASPERINC to paths to library and 
# include files to enable this feature prior to running configure.  
if($ENV{JASPERLIB} && $ENV{JASPERINC})
{
   printf "Found Jasper environment variables for GRIB2 support...\n";
   printf("  \$JASPERLIB = %s\n",$ENV{JASPERLIB});
   printf("  \$JASPERINC = %s\n",$ENV{JASPERINC});
   $sw_jasperlib_path = "-L$ENV{JASPERLIB} -ljasper -lpng -lz"; 
   $sw_jasperinc_path = "-I$ENV{JASPERINC}"; 
}
else
{

    $tmp1 = '/usr/local/jasper';
    if (-e $tmp1) {
      $sw_jasperlib_path = '-L/usr/local/jasper/lib -ljasper -L/usr/local/libpng -lpng12 -lpng -L/usr/local/zlib/lib -lz' ;
      $sw_jasperinc_path = '-I/usr/local/zlib/include -I/usr/local/jasper/include -I/usr/local/libpng/' ; 
        printf "\$JASPERLIB or \$JASPERINC not found in environment. Using /usr/local for library paths...\n";
    }
    else {
      $tmp1 = '/opt/local/lib';
      if (-e $tmp1) {
        $sw_jasperlib_path = '-L/opt/local/lib -ljasper -lpng -lz';
	$sw_jasperinc_path = '-I/opt/local/include'; 
        printf "\$JASPERLIB or \$JASPERINC not found in environment. Using /opt/local for library paths...\n";
      }
      else {
      $sw_jasperlib_path = '-L/glade/u/home/wrfhelp/UNGRIB_LIBRARIES/lib -ljasper -lpng -lz';
      $sw_jasperinc_path = '-I/glade/u/home/wrfhelp/UNGRIB_LIBRARIES/include';
        printf "\$JASPERLIB or \$JASPERINC not found in environment. Using default values for library paths...\n";
      }
    }
}

$validresponse = 0 ;
# added this from the WRF Config.pl by John M.
@platforms = ('serial', 'serial_NO_GRIB2', 'dmpar', 'dmpar_NO_GRIB2');
# Display the choices to the user and get selection
until ($validresponse)
{
    printf "------------------------------------------------------------------------\n";
    printf "Please select from among the following supported platforms.\n\n";

    $opt = 1;
    # Read configure.defaults
    open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" 
         || die "Cannot open ./arch/configure.defaults for reading";
    # first read through the .defaults, user select and a read of all appropriate parms is not done here
    while(<CONFIGURE_DEFAULTS>)
    {
       for $paropt (@platforms)
       {
           # read all the System/OS/Compiler appropriate selections and present same to user
           if(substr($_, 0, 5) eq "#ARCH" && (index($_, $sw_os) >= 0) && (index($_, $sw_mach) >= 0) && (index($_, $paropt) >= 0))
           {
               $optstr[$opt] = substr($_,6);
               $optstr[$opt] =~ s/^[     ]*//;
               $optstr[$opt] =~ s/#.*$//g;
               chomp($optstr[$opt]);
               $optstr[$opt] = $optstr[$opt]." (".$paropt.")";
               if(substr($optstr[$opt], 0, 4) ne "NULL")
               {
                   printf "  %2d.  %s\n", $opt, $optstr[$opt];  
                   $opt++;
               }
           }
       }
   }
   close CONFIGURE_DEFAULTS;

   $opt --;

   printf "\nEnter selection [%d-%d] : ", 1, $opt;
   $response = <STDIN>;
   if($response == -1) {exit;}
   if($response >= 1 && $response <= $opt) 
   {
       $validresponse = 1;
   }
   else
   {
       printf("\nInvalid response (%d)\n",$response);
   }
}

printf "------------------------------------------------------------------------\n";
# save user input
$optchoice = $response;

# this HAS to be opened in 'cat' mode... why?
open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" || die "cannot Open for writing... configure.defaults: \n";
$latchon = 0;
while(<CONFIGURE_DEFAULTS>)
{  
    if(substr($_, 0, 5) eq "#ARCH" && $latchon == 1)
    {
        $latchon = 0;
    }
    if($latchon == 1)
    {
        $_ =~ s/CONFIGURE_PERL_PATH/$sw_perl_path/g;
        $_ =~ s/CONFIGURE_NETCDF_PATH/$sw_netcdf_path/g;
        $_ =~ s/CONFIGURE_LDFLAGS/$sw_ldflags/g;
        $_ =~ s/CONFIGURE_COMPILEFLAGS/$sw_compileflags/g;
        $_ =~ s/CONFIGURE_COMP_L/$sw_compL/g;
        $_ =~ s/CONFIGURE_COMP_I/$sw_compI/g; 
        $_ =~ s/CONFIGURE_FDEFS/$sw_fdefs/g; 
        $_ =~ s/CONFIGURE_FC/$sw_fc/g;    
        $_ =~ s/CONFIGURE_CC/$sw_cc/g;
        $_ =~ s/CONFIGURE_MPI/$sw_mpi/g;

        # Load the read in parameters from the configure.defaults file
        if(!(substr($_, 0, 5) eq "#ARCH")) 
        {
            @machopts = (@machopts, $_); 
        }        
    } # end if latchon == 1    

    #-----------------------------------------------------------------------------------------------
    # added for the unified WPS build
    # init the following for the configure.wps write
        
    
    # now loop through the .defaults again and read the parameters to be written into the configure.wps
    for $paropt (@platforms)
    {      
        if(substr($_, 0, 5) eq "#ARCH" && $latchon == 0 && (index( $_, $sw_os) >= 0) && (index( $_, $sw_mach) >= 0) && (index($_, $paropt) >= 0))  
        {
            # after #ARCH the following reads the rest of the line in the configure.defaults
            $x=substr($_,6);
            $x=~s/^[     ]*//;
            $x =~ s/#.*$//g;
            chomp($x);
            $x = $x." (".$paropt.")" ;    

            if($x eq $optstr[$optchoice]) 
            {
                $latchon = 1;
            }
            if($latchon == 1) 
            {                                      
                if($paropt eq 'serial')
                {                    
                    if($ENV{JASPERLIB} && $ENV{JASPERINC})
                    {
                       $sw_compL = $sw_jasperlib_path;
                       $sw_compI = $sw_jasperinc_path;
                    }
                    else
                    {
                       $sw_compL = $sw_jasperlib_path;
                       $sw_compI = $sw_jasperinc_path;
#                      $sw_compL = "-L/contrib/jasper/lib -L/opt/freeware/lib -ljasper -lpng -lz"; 
#                      $sw_compI = "-I/contrib/libpng/include -I/contrib/zlib/include -I/contrib/jasper/include";
                    }
                    $sw_fdefs = "-DUSE_JPEG2000 -DUSE_PNG";                    
                    $sw_fc    = "\$(SFC)";                                        
                    $sw_cc    = "\$(SCC)";                              
                    $sw_mpi   = "";                              
                }
                if($paropt eq 'serial_NO_GRIB2')
                {                    
                    $sw_compL = "";    
                    $sw_compI = "";                   
                    $sw_fdefs = "";                    
                    $sw_fc    = "\$(SFC)";                                        
                    $sw_cc    = "\$(SCC)";                              
                    $sw_mpi   = "";                              
                }
                if($paropt eq 'dmpar') 
                {                         
                    if($ENV{JASPERLIB} && $ENV{JASPERINC})
                    {
                       $sw_compL = $sw_jasperlib_path;
                       $sw_compI = $sw_jasperinc_path;
                    }
                    else
                    {
                       $sw_compL = $sw_jasperlib_path;
		       $sw_compI = $sw_jasperinc_path;
#                      $sw_compL = "-L/contrib/jasper/lib -L/opt/freeware/lib -ljasper -lpng -lz"; 
#                      $sw_compI = "-I/contrib/libpng/include -I/contrib/zlib/include -I/contrib/jasper/include";
                    }
                    $sw_fdefs = "-DUSE_JPEG2000 -DUSE_PNG";
                    $sw_fc    = "\$(DM_FC)";
                    $sw_cc    = "\$(DM_CC)";
                    $sw_mpi   = "-D_MPI";
                }
                if($paropt eq 'dmpar_NO_GRIB2') 
                {                         
                    $sw_compL = "";
                    $sw_compI = ""; 
                    $sw_fdefs = "";
                    $sw_fc    = "\$(DM_FC)";                                        
                    $sw_cc    = "\$(DM_CC)";       
                    $sw_mpi   = "-D_MPI";                              
                }

                ##################################################################################### 
            } # end if latchon == 1
        } # end if
    } # end for
}
close CONFIGURE_DEFAULTS ;

#printf "\n------------------------------------------------------------------------\n";
#foreach $f (@machopts)
#{
#    if(substr($f , 0 , 8) eq "external") 
#    { 
#        last ; 
#    }
#    print $f;
#}
#printf "--------------------------------------------------------------------------\n";
#printf "\nYou have chosen: %s\n",$optstr[$optchoice];
#printf "Listed above are the default options for this platform.\n";
#printf "Settings are written to the file configure.wps here in the top-level\n";
#printf "directory.  If you wish to change settings, please edit that file.\n";
#printf "If you wish to change the default options, edit the file:\n\n";
#printf "     arch/configure.defaults\n";
#printf "\n";
#printf "------------------------------------------------------------------------\n";

open CONFIGURE_WRF, "> configure.wps" || die "cannot Open for writing... configure.wps: \n";
    open ARCH_PREAMBLE, "< arch/preamble" || die "cannot Open for reading... arch/preamble: \n";
    my @preamble;
    # apply substitutions to the preamble...
    while (<ARCH_PREAMBLE>)
    {
        if($sw_os eq "CYGWIN_NT")
        {
            $_ =~ s/^WRF_DIR.*$/COMPILING_ON_CYGWIN_NT = yes/;  # will get from environment
        }

        $_ =~ s:CONFIGURE_NETCDFF_LIB:$sw_netcdff_lib:g; 
        $_ =~ s:CONFIGURE_WRF_PATH:$sw_wrf_path:g; 
        @preamble = ( @preamble, $_ ) ;
    }
    close ARCH_PREAMBLE;


    print CONFIGURE_WRF @preamble;
    close ARCH_PREAMBLE;
    printf CONFIGURE_WRF "#\n";
    printf CONFIGURE_WRF "#   Settings for %s \n", $optstr[$optchoice];
    printf CONFIGURE_WRF "#\n";    
    print CONFIGURE_WRF @machopts;

    open ARCH_POSTAMBLE, "< arch/postamble" || die "cannot open arch/postamble: \n";
    while(<ARCH_POSTAMBLE>) 
    { 
        $_ =~ s:CONFIGURE_NETCDFF_LIB:$sw_netcdff_lib:g;   
        print CONFIGURE_WRF;
    } 

    close ARCH_POSTAMBLE;
close CONFIGURE_WPS;

printf "Configuration successful. To build the WPS, type: compile\n";
printf "------------------------------------------------------------------------\n";
