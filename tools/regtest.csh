#!/bin/csh
# @ job_type		= parallel
# @ environment		= COPY_ALL;MP_EUILIB=us
# @ job_name		= regtest
# @ output		= regtest_out
# @ error		= regtest_err
# @ network.MPI		= csss,shared,us
# @ node_usage		= shared
# @ checkpoint		= no
# @ wall_clock_limit	= 21600
# @ node		= 1
# @ total_tasks		= 4
# @ class		= share
# @ ja_report		= yes
# @ queue

#PBS -V -A acb
#PBS -lnodes=4:comp -l walltime=40000

if ( ( `uname` == Linux ) && ( `hostname | cut -d. -f2-` == fsl.noaa.gov ) ) then
	source /usr/local/bin/setup-mpi.csh
endif

#	This is a script to test the bit-for-bit reproducibility of
#	the WRF model, when comparing single processor serial runs to
#	OpenMP and MPI parallel runs.  There are several regression tests
#	that are performed.  Failed comparisons get reported, but don't
#	stop the script.  Failed builds or forecasts force an exit from 
#	the script.

#	Approximate time for completion of full test suite
#		Compaq 733 MHz   ev67 :  2.5 hours (empty)
#		Intel  1.2 GHz (4-pe) :  3.0 hours (empty)
#		IBM            P4     :  2.0 hours (empty)

#	These need to be changed for your particular set of runs.  This is
#	where email gets sent.

set FAIL_MAIL = ( ${user}@ucar.edu )
set GOOD_MAIL = ( ${user}@ucar.edu )

unalias cd cp rm ls pushd popd mv
if ( `uname` == Linux ) alias banner echo

#	Get the command line input

set thedate = -999
set thefile = "null"
set thedata = "null"
set clrm = 0          # compile local run mmmtmp, for using clsroom cluster and local disk

#	If this is a batch job (NCAR's IBMs or FSL's Intel and Alpha), we need to muck with the "input"
#	parameters a bit.

if      ( `uname` == AIX ) then
	set argv = ( -here )
	set argv = ( -ftp )
        set argv = ( -D today )
	set argv = ( -env )
	set WRFREGFILE = /mmm/users/gill/wrf.tar
	set argv = ( -f wrf.tar ) 
else if ( ( `uname` == Linux ) && ( `hostname | cut -d. -f2-` == fsl.noaa.gov ) && ( $user == jacquesm ) ) then
	set argv = ( -env )
else if ( ( `uname` == Linux ) && ( `hostname | cut -d. -f2-` == fsl.noaa.gov ) && ( $user == weiwang ) ) then
	set WRFREGFILE = /p16/ncarwrf/WRF_REG_FILES/wrf.tar
	set WRFREGDATA = /p16/ncarwrf/WRF_REG_FILES/data.tar.gz
	set argv = ( -env )
else if ( ( `uname` == OSF1 ) && ( `hostname` == maple ) && ( $user == michalak ) ) then
        set clrm=1
endif

#	Where is the input data located - for a few known NCAR/MMM machines.

if      ( ( `hostname` == master ) || (`hostname | cut -c 1-4` == node ) ) then
	set WRFREGDATAEM = /big/users/gill/WRF-data-EM
	set WRFREGDATANMM = /big/users/gill/WRF-data-NMM
else if   ( `hostname` == duku ) then
	set WRFREGDATAEM = /duku/users/gill/WRF-data-EM
	set WRFREGDATANMM = /duku/users/gill/WRF-data-NMM
else if ( (`hostname | cut -c 1-6` == joshua ) || \
          ( `hostname` == maple ) || (`hostname | cut -c 1-7` == service ) ) then
	set WRFREGDATAEM = /users/gill/WRF-data-EM
	set WRFREGDATANMM = /users/gill/WRF-data-NMM
else if ( ( `hostname | cut -c 1-2` == bs ) || ( `hostname | cut -c 1-2` == bf ) ) then
	set WRFREGDATAEM = /mmm/users/gill/WRF-data-EM
	set WRFREGDATANMM = /mmm/users/gill/WRF-data-NMM
else
	if      ( ( -d /users/gill/WRF-data-EM ) && ( -d /users/gill/WRF-data-NMM ) ) then
		set WRFREGDATAEM = /users/gill/WRF-data-EM
		set WRFREGDATANMM = /users/gill/WRF-data-NMM
	else if ( ( -d /mmm/users/gill/WRF-data-EM ) && ( -d /mmm/users/gill/WRF-data-NMM ) ) then
		set WRFREGDATAEM = /mmm/users/gill/WRF-data-EM
		set WRFREGDATANMM = /mmm/users/gill/WRF-data-NMM
	else
		echo "stick the WRF em and nmm data somewhere, and then fill in the shell vars"
		echo "inside this script, you NEED WRFREGDATAEM and WRFREGDATANMM set"
		exit ( 1 ) 
	endif
endif
#DAVE###################################################
echo DAVE em data is located at $WRFREGDATAEM
ls -ls $WRFREGDATAEM
echo DAVE nmm data is located at $WRFREGDATANMM
ls -ls $WRFREGDATANMM
banner 1
#set ans = "$<"
#DAVE###################################################

if ( $#argv == 0 ) then
	echo "Please enter either a date for cvs checkout. ex regtest.csh -D date"
	echo " or a file name containing WRF. ex regtest.csh -f tarfile"
	echo " or the -ftp flag for the script to pick code off anon ftp"
	exit ( 2 ) 
endif

set theargs = 0
foreach a ( $argv )
	if ( "$a" == "-D" ) then

		rsh -n maple.mmm.ucar.edu w >& /dev/null
		if ( $status ) then
			echo "Cannot execute a remote shell on maple.mmm.ucar.edu, where the"
			echo "WRF code resides."
			echo "Please check that it is up and that you have permission to rsh"
			echo "to this host. (Create a .rhosts file)."
			ping -c 1 maple.mmm.ucar.edu
			exit 2
		endif
		setenv CVSROOT maple.mmm.ucar.edu:/data3/mp/wrfhelp/WRF
		
		set acquire_from = "cvs"
		set thedate = $argv[2]

	endif

	if ( "$a" == "-f" ) then

		set thefile = $argv[2]
		#	Check for absolute path, if not, make it absolute
		echo $thefile | grep '^/' > /dev/null
		if ( $status != 0 ) set thefile = `pwd`/$thefile
		set acquire_from = "filearg"

	endif

	if ( "$a" == "-ftp" ) then
		set acquire_from = "ftp"
echo "anon ftp temporarily disabled"
exit ( 3 )
	endif

	if ( "$a" == "-here" ) then
		set acquire_from = "here"
	endif

	if ( "$a" == "-env" ) then
		set acquire_from = "environment"
		set thefile = $WRFREGFILE
	endif
end

#	Start recording everything - for debug purposes.

set echo 
set date

#	And to tell us how long we've spent on this whole regression test,
#	we should remember when we started.

set start = ( `date` )

#####################################################################

#	Initial set up values

#	Is this a single domain regression test or is this nested.  Well, a nested one
#	is a bit special.  It can only run on machines that have the WRF RSL-but-no-MPI
#	option available.

set NESTED = TRUE
set NESTED = FALSE

if ( ( $NESTED == TRUE ) && ( `uname` != OSF1 ) ) then
	echo NESTED option is only valid on DEC machines
	exit ( 1 ) 
endif

#	Are we shooting for a bit-for-bit run (serial vs OpenMP, serial vs MPI), or not?
#	If you want to do a performance-only run, the forecasts are still short, but you
#	get to insure that the optimized code builds and runs.

set REG_TYPE = OPTIMIZED
set REG_TYPE = BIT4BIT

#	For the real data case, we can run either one of two data cases.

set dataset = jun01
set dataset = jan00

#	Yet another local variable to change the name of where the data is located.

set thedataem = ${WRFREGDATAEM}/${dataset}
set thedatanmm = $WRFREGDATANMM

#	If we are doing nested runs, we are usually trying that non-MPI-but-using-RSL
#	option.  That is not going to work with NMM due to needing MPI.

if      ( $NESTED == TRUE ) then
	set CORES = (  em_real em_b_wave em_quarter_ss          )
else if ( $NESTED != TRUE ) then
	set CORES = (  em_real em_b_wave em_quarter_ss nmm_real )
endif

set PHYSOPTS =	( 1 2 3 )

#	This is an ugly kludge.  The MP=2 does not work with the ideal cases with the
#	special DEC build options.

if      ( $NESTED == TRUE ) then
	set Max_Ideal_Physics_Options = 2
else if ( $NESTED != TRUE ) then
	set Max_Ideal_Physics_Options = 3
endif

set CUR_DIR = `pwd`

#	How many domains to run (nest tests).  Only em_real and ideals use this.
#	The max is 3 due to the number of columns in the namelist that are 
#	currently filled in.

if      ( $NESTED == TRUE ) then
if      ( $dataset == jan00 ) then
cat >! dom_real << EOF
 time_step                           = 180,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 2,
 s_we                                = 1,     1,     1,
 e_we                                = 74,    31,    31,
 s_sn                                = 1,     1,     1,
 e_sn                                = 61,    31,    31,
 s_vert                              = 1,     1,     1,
 e_vert                              = 28,    28,    28,
 dx                                  = 30000, 10000,  3333.333333,
 dy                                  = 30000, 10000,  3333.333333,
 grid_id                             = 1,     2,     3,
 level                               = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     31,    11,
 j_parent_start                      = 0,     17,    11,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0
EOF
else if ( $dataset == jun01 ) then
cat >! dom_real << EOF
 time_step                           = 60,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 2,
 s_we                                = 1,     1,     1,
 e_we                                = 91,    31,    31,
 s_sn                                = 1,     1,     1,
 e_sn                                = 82,    31,    31,
 s_vert                              = 1,     1,     1,
 e_vert                              = 28,    28,    28,
 dx                                  = 10000,  3333.333333,  1111.111111,
 dy                                  = 10000,  3333.333333,  1111.111111,
 grid_id                             = 1,     2,     3,
 level                               = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     30,    11,
 j_parent_start                      = 0,     20,    11,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0
EOF
endif
cat >! dom_ideal << EOF
 max_dom                             = 2,
EOF
else if ( $NESTED != TRUE ) then
if      ( $dataset == jan00 ) then
cat >! dom_real << EOF
 time_step                           = 180,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,     1,     1,
 e_we                                = 74,    31,    31,
 s_sn                                = 1,     1,     1,
 e_sn                                = 61,    31,    31,
 s_vert                              = 1,     1,     1,
 e_vert                              = 28,    28,    28,
 dx                                  = 30000, 10000,  3333,
 dy                                  = 30000, 10000,  3333,
 grid_id                             = 1,     2,     3,
 level                               = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     31,    30,
 j_parent_start                      = 0,     17,    30,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0
EOF
else if ( $dataset == jun01 ) then
cat >! dom_real << EOF
 time_step                           = 60,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,     1,     1,
 e_we                                = 91,    31,    31,
 s_sn                                = 1,     1,     1,
 e_sn                                = 82,    31,    31,
 s_vert                              = 1,     1,     1,
 e_vert                              = 28,    28,    28,
 dx                                  = 10000,  3333.333333,  1111.111111,
 dy                                  = 10000,  3333.333333,  1111.111111,
 grid_id                             = 1,     2,     3,
 level                               = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 0,     30,    11,
 j_parent_start                      = 0,     20,    11,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0
EOF
endif
cat >! dom_ideal << EOF
 max_dom                             = 1,
EOF
endif

#	The em_real entire physics namelist.  Change what you want.

cat >! phys_real_1  << EOF
 mp_physics                          = 3,     3,     3,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = 1,     1,     1,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 1,     1,     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 5,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
EOF

cat >! phys_real_2 << EOF
 mp_physics                          = 4,     4,     4,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 2,     2,     2,
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 2,     2,     2,
 sf_surface_physics                  = 2,     2,     2,
 bl_pbl_physics                      = 2,     2,     2,
 bldt                                = 0,     0,     0,
 cu_physics                          = 2,     2,     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 4,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
EOF

cat >! phys_real_3 << EOF
 mp_physics                          = 5,     5,     5,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 2,     2,     2,
 radt                                = 30,    30,    30,
 sf_sfclay_physics                   = 2,     2,     2,
 sf_surface_physics                  = 3,     3,     3,
 bl_pbl_physics                      = 2,     2,     2,
 bldt                                = 0,     0,     0,
 cu_physics                          = 3,     3,     0,
 cudt                                = 5,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 6,
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
EOF

#	Tested options for ideal case em_b_wave.  Modifying these
#	parameters is acceptable.  Adding to these requires changes
#	to the ideal namelist build below.

cat >! phys_b_wave_1a << EOF
 diff_opt                            = 1,
 km_opt                              = 1,
 damp_opt                            = 0,
EOF
cat >! phys_b_wave_1b << EOF
 mp_physics                          = 1,     1,     1,
EOF
cat >! phys_b_wave_1c << EOF
 non_hydrostatic                     = .true., .true., .true.,
EOF

cat >! phys_b_wave_2a << EOF
 diff_opt                            = 1,
 km_opt                              = 1,
 damp_opt                            = 0,
EOF
cat >! phys_b_wave_2b << EOF
 mp_physics                          = 1,     1,     1,
EOF
cat >! phys_b_wave_2c << EOF
 non_hydrostatic                     = .false., .false., .false.,
EOF

cat >! phys_b_wave_3a << EOF
 diff_opt                            = 1,
 km_opt                              = 1,
 damp_opt                            = 0,
EOF
cat >! phys_b_wave_3b << EOF
 mp_physics                          = 2,     2,     2,
EOF
cat >! phys_b_wave_3c << EOF
 non_hydrostatic                     = .false., .false., .false.,
EOF

#	Tested options for ideal case em_quarter_ss.  Modifying these
#	parameters is acceptable.  Adding to these requires changes
#	to the ideal namelist build below.

cat >! phys_quarter_ss_1a << EOF
 diff_opt                            = 1,
 km_opt                              = 1,
 damp_opt                            = 0,
EOF
cat >! phys_quarter_ss_1b << EOF
 mp_physics                          = 1,     1,     1,
EOF
cat >! phys_quarter_ss_1c << EOF
 non_hydrostatic                     = .true., .true., .true.,
EOF
cat >! phys_quarter_ss_1d << EOF
 periodic_x                          = .false.,.false.,.false.,
 open_xs                             = .true., .false.,.false.,
 open_xe                             = .true., .false.,.false.,
 periodic_y                          = .false.,.false.,.false.,
 open_ys                             = .true., .false.,.false.,
 open_ye                             = .true., .false.,.false.,
EOF

cat >! phys_quarter_ss_2a << EOF
 diff_opt                            = 2,
 km_opt                              = 2,
 damp_opt                            = 1,
EOF
cat >! phys_quarter_ss_2b << EOF
 mp_physics                          = 1,     1,     1,
EOF
cat >! phys_quarter_ss_2c << EOF
 non_hydrostatic                     = .true., .true., .true.,
EOF
cat >! phys_quarter_ss_2d << EOF
 periodic_x                          = .true., .false.,.false.,
 open_xs                             = .false.,.false.,.false.,
 open_xe                             = .false.,.false.,.false.,
 periodic_y                          = .true., .false.,.false.,
 open_ys                             = .false.,.false.,.false.,
 open_ye                             = .false.,.false.,.false.,
EOF

cat >! phys_quarter_ss_3a << EOF
 diff_opt                            = 2,
 km_opt                              = 3,
 damp_opt                            = 1,
EOF
cat >! phys_quarter_ss_3b << EOF
 mp_physics                          = 2,     2,     2,
EOF
cat >! phys_quarter_ss_3c << EOF
 non_hydrostatic                     = .false., .false., .false.,
EOF
cat >! phys_quarter_ss_3d << EOF
 periodic_x                          = .true., .false.,.false.,
 open_xs                             = .false.,.false.,.false.,
 open_xe                             = .false.,.false.,.false.,
 periodic_y                          = .true., .false.,.false.,
 open_ys                             = .false.,.false.,.false.,
 open_ye                             = .false.,.false.,.false.,
EOF

if ( $dataset == jun01 ) then
	set filetag_real=2001-06-11_12:00:00
else if ( $dataset == jan00 ) then
	set filetag_real=2000-01-24_12:00:00
endif

set filetag_ideal=0001-01-01_00:00:00
#DAVE###################################################
echo did phys, set date to $filetag_real
banner 2
#set ans = "$<"
#DAVE###################################################

#####################################################################

#	Set up info for particular architectures

set ARCH = ( `uname` )

set ZAP_SERIAL          = FALSE
set ZAP_OPENMP          = FALSE
set SERIALRUNCOMMAND	= 
set OMPRUNCOMMAND	= 

touch version_info
if      ( ( $ARCH[1] == AIX ) && ( `hostname | cut -c 1-2` == bf ) ) then
	set DEF_DIR             = $home
	set TMPDIR              = /ptmp/$user
	if ( ! -d $TMPDIR ) mkdir $TMPDIR
	set MAIL                = /usr/bin/mailx
	set COMPOPTS            = ( 1 2 4 )
	set Num_Procs		= 4
	set OPENMP 		= $Num_Procs
        setenv MP_PROCS  $Num_Procs
        setenv MP_RMPOOL 1
	set MPIRUNCOMMAND       =  poe 
	set ZAP_OPENMP		= FALSE
	echo "Compiler version info: " >! version_info
	pmrinfo | grep "FORTRAN:" >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	pmrinfo | grep "AIX:" >>&! version_info
	echo " " >>! version_info
	setenv MP_SHARED_MEMORY yes
else if ( ( $ARCH[1] == AIX ) && ( `hostname | cut -c 1-2` == bs ) ) then
	set DEF_DIR             = $home
	set TMPDIR              = /ptmp/$user
	if ( ! -d $TMPDIR ) mkdir $TMPDIR
	set MAIL                = /usr/bin/mailx
	set COMPOPTS            = ( 1 2 4 )
	set Num_Procs		= 4
	set OPENMP 		= $Num_Procs
        setenv MP_PROCS  $Num_Procs
        setenv MP_RMPOOL 1
	set MPIRUNCOMMAND       =  poe 
	set ZAP_OPENMP		= FALSE
	echo "Compiler version info: " >! version_info
	pmrinfo | grep "FORTRAN:" >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	pmrinfo | grep "AIX:" >>&! version_info
	echo " " >>! version_info
	setenv MP_SHARED_MEMORY yes
else if ( ( $ARCH[1] == AIX ) && ( `hostname | cut -c 1-2` == bb ) ) then
	set DEF_DIR             = $home
	set TMPDIR              = /ptmp/$user
	if ( ! -d $TMPDIR ) mkdir $TMPDIR
	set MAIL                = /usr/bin/mailx
	set COMPOPTS            = ( 1 2 4 )
	set Num_Procs		= 4
	set OPENMP 		= $Num_Procs
        setenv MP_PROCS  $Num_Procs
        setenv MP_RMPOOL 1
	set MPIRUNCOMMAND       = poe
	set ZAP_OPENMP		= FALSE
	echo "Compiler version info: " >! version_info
	pmrinfo | grep "FORTRAN:" >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	pmrinfo | grep "AIX:" >>&! version_info
	echo " " >>! version_info
	setenv MP_SHARED_MEMORY yes
else if ( $ARCH[1] == OSF1 && $clrm == 0 ) then
	if ( ( `hostname` == duku ) && ( -d /data1/$user ) ) then
		set DEF_DIR	= /data1/$user
	else
		set DEF_DIR	= /mmmtmp/${user}/`hostname`
		if ( ! -d $DEF_DIR ) mkdir $DEF_DIR
	endif
	set TMPDIR              = .
	set MAIL		= /usr/bin/mailx
	if      ( $NESTED == TRUE ) then
		set COMPOPTS	= ( 2 4 6 )
	else if ( $NESTED != TRUE ) then
		set COMPOPTS	= ( 1 3 6 )
	endif
	set Num_Procs		= 4
	set OPENMP 		= $Num_Procs
	cat >! `pwd`/machfile << EOF
`hostname`
`hostname`
`hostname`
`hostname`
EOF
        set Mach = `pwd`/machfile
	set SERIALRUNCOMMAND	= 
	set OMPRUNCOMMAND	= 
	set MPIRUNCOMMAND 	= ( /usr/local/mpich/bin/mpirun -np $Num_Procs -machinefile $Mach )
	set ZAP_OPENMP		= FALSE
	echo "Compiler version info: " >! version_info
	f90 -version >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( $ARCH[1] == OSF1 && $clrm == 1 ) then
	set DEF_DIR		= /`hostname | cut -d. -f1`/$user
	set TMPDIR		= /mmmtmp/$user
	set MAIL		= /usr/bin/mailx
	if      ( $NESTED == TRUE ) then
		set COMPOPTS	= ( 2 4 6 )
	else if ( $NESTED != TRUE ) then
		set COMPOPTS	= ( 1 3 6 )
	endif
	set Num_Procs		= 4
	set OPENMP 		= 0
	set ZAP_OPENMP		= TRUE
	cat >! $TMPDIR/machfile << EOF
service03
service04
service05
service06
EOF
        set Mach = $TMPDIR/machfile
	set MPIRUNCOMMAND 	= ( mpirun -np $Num_Procs -machinefile $Mach )
	echo "Compiler version info: " >! version_info
	f90 -version >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname` == master ) ) then
	set DEF_DIR		= /big6/gill/DO_NOT_REMOVE_DIR
	set TMPDIR		= .
	set MAIL		= /bin/mail
	set COMPOPTS		= ( 1 2 3 )
	set Num_Procs		= 4
	set OPENMP		= 2
	set MPIRUNCOMMAND	= ( mpirun -np $Num_Procs )
	set ZAP_OPENMP		= TRUE
	echo "Compiler version info: " >! version_info
	pgf90 -V >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname` == atc-c1 ) ) then
	if ( $user == gill Da) then
		set DEF_DIR	= /data/bourgeoi/DAVE
	else
		set DEF_DIR	= /data/$user
	endif
	set TMPDIR              = .
	set MAIL		= /bin/mail
	set COMPOPTS		= ( 1 2 3 )
	set Num_Procs		= 4
	set OPENMP 		= 2
	set MPIRUNCOMMAND 	= ( mpirun -np $Num_Procs )
	set ZAP_OPENMP		= FALSE
	echo "Compiler version info: " >! version_info
	pgf90 -V >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
	setenv NETCDF /data/bourgeoi/netcdf
else if ( ( $ARCH[1] == Linux ) && ( `hostname` == kola ) ) then
	set DEF_DIR		= /kola2/$user
	set TMPDIR              = .
	set MAIL		= /bin/mail
	set COMPOPTS		= ( 1 2 3 )
	set Num_Procs		= 2
	set OPENMP 		= $Num_Procs
	cat >! machfile << EOF
kola
kola
EOF
	set Mach = `pwd`/machfile
	set MPIRUNCOMMAND 	= ( mpirun -np $Num_Procs -machinefile $Mach )
	set ZAP_OPENMP		= FALSE
	echo "Compiler version info: " >! version_info
	pgf90 -V >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname` == she ) ) then
	set DEF_DIR		= /she/users/$user
	set TMPDIR		= .
	set MAIL		= /bin/mail
	set COMPOPTS		= ( 1 2 3 )
	set Num_Procs		= 2
	set OPENMP		= $Num_Procs
	cat >! machfile << EOF
she
she
EOF
	set Mach		= `pwd`/machfile
	set ZAP_OPENMP		= FALSE
	set MPIRUNCOMMAND       = ( mpirun -np $Num_Procs -machinefile $Mach )
	echo "Compiler version info: " >! version_info
	pgf90 -V >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname` == bay-mmm ) ) then
	set DEF_DIR	= /mmmtmp/${user}/`hostname`
	if ( ! -d $DEF_DIR ) mkdir $DEF_DIR
	set TMPDIR		= .
	set MAIL		= /bin/mail
	set COMPOPTS		= ( 1 2 3 )
	set Num_Procs		= 2
	set OPENMP		= $Num_Procs
	cat >! machfile << EOF
`hostname`
`hostname`
`hostname`
`hostname`
EOF
	set Mach		= `pwd`/machfile
	set ZAP_OPENMP		= FALSE
	set MPIRUNCOMMAND       = ( mpirun -np $Num_Procs -machinefile $Mach )
	echo "Compiler version info: " >! version_info
	pgf90 -V >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname | cut -d. -f2-` == fsl.noaa.gov ) && ( $user == jacquesm ) ) then
	set DEF_DIR		= /p10/acb/users/${user}/wrfReg
	set TMPDIR              = .
	set MAIL		= /usr/bin/Mail
	set COMPOPTS		= ( 1 2 3 )
	set Num_Procs		= 4
	set OPENMP 		= $Num_Procs
	set MPIRUNCOMMAND 	= ( mpirun -np $Num_Procs )
	set ZAP_OPENMP		= TRUE
	echo "Compiler version info: " >! version_info
	fort -version >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == Linux ) && ( `hostname | cut -d. -f2-` == fsl.noaa.gov ) && ( $user == weiwang ) ) then
	set DEF_DIR		= /p16/ncarwrf
	set TMPDIR              = .
	set MAIL		= /usr/bin/Mail
	set COMPOPTS		= ( 1 2 3 )
	set Num_Procs		= 4
	set OPENMP 		= $Num_Procs
	set MPIRUNCOMMAND 	= ( mpirun -np $Num_Procs )
	set ZAP_OPENMP		= TRUE
	echo "Compiler version info: " >! version_info
	fort -version >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
	setenv NETCDF /usr/local/netcdf-3.4
else if ( ( $ARCH[1] == IRIX64 ) && ( `hostname` == dataproc ) ) then
	set DEF_DIR		= /ptmp/$user
	set TMPDIR              = .
	set MAIL		= /usr/sbin/mailx
	set COMPOPTS		= ( 1 2 3 )
	set Num_Procs		= 4
	set OPENMP		= $Num_Procs
	set MPIRUNCOMMAND	= ( mpirun -np $Num_Procs )
	set ZAP_OPENMP		= FALSE
	echo "Compiler version info: " >! version_info
	f90 -version >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
else if ( ( $ARCH[1] == IRIX64 ) && ( `hostname` == gs2 ) ) then
	set DEF_DIR		= /fer2/${user}/DAVE
	set TMPDIR              = .
	set MAIL		= /usr/sbin/mailx
	set COMPOPTS		= ( 1 2 3 )
	set Num_Procs		= 4
	set OPENMP		= $Num_Procs
	set MPIRUNCOMMAND	= ( mpirun -np $Num_Procs )
	echo "Compiler version info: " >! version_info
	set ZAP_OPENMP		= FALSE
	f90 -version >>&! version_info
	echo " " >>! version_info
	echo "OS version info: " >>! version_info
	uname -a >>&! version_info
	echo " " >>! version_info
	setenv NETCDF /disk2/people3/wesley/netcdf_Dave_WRF_64
else
	echo "Unrecognized architecture for regression test"  >! error_message
	echo `uname`                                          >> error_message
	echo `hostname`                                       >> error_message
	$MAIL -s "Unknown architecture $ARCH[1] " $FAIL_MAIL   < error_message
	exit ( 1 )
endif

#####################################################################
#DAVE###################################################
echo did the arch specific stuff
banner 3
#set ans = "$<"
#DAVE###################################################

#	First of all, in which particular directory do we start.

cd $DEF_DIR

#	We want to keep the old regression stuff around

if ( -d regression_test ) then
	if ( -d regression_test.old ) then
		/bin/rm -fr regression_test.old
	endif
	/bin/mv regression_test regression_test.old
endif

#	Go to the regression test directory

mkdir regression_test
set ok = $status
if ( $ok != 0 ) then
	echo "Gee, I cannot make a directory in $DEF_DIR"  >! error_message
	echo `pwd`                                         >> error_message
	echo `\ls -ls`                                     >> error_message
	$MAIL -s "$DEF_DIR not writable $ARCH[1] " $FAIL_MAIL < error_message
	exit ( 1 )
else
	pushd regression_test
endif

if      ( $acquire_from == "cvs" ) then

	#	Checkout the most recent version of WRF from the NCAR cvs repository,
	#	and pick up the required input data from the anonymous ftp site.

	cvs checkout -D $thedate WRFV2
	find ./WRFV2 -exec touch \{\} \;
	ftp -n ftp.ucar.edu < ftp_script_data


else if ( $acquire_from == "filearg" ) then

	#	A tar file of the WRF source was provided, so that is used, along with 
	#	the required input data files from the ftp site.

	tar xvf $thefile
	cd WRFV2
	clean -a
	cd ..
	ftp -n ftp.ucar.edu < ftp_script_data

else if ( $acquire_from == "environment" ) then

	#	A tar file of WRF is assumed to be available.

	tar xvf $thefile

endif

#	And we can stick the input data where we want, the WRFV2 directory has been created.

( cd WRFV2/test/em_real  ; ln -sf $thedataem/* . ) 
( cd WRFV2/test/nmm_real ; ln -sf $thedatanmm/*  . ; ln -sf co2.60_hyb_bot40m co2_trans )
#DAVE###################################################
( cd WRFV2/test/em_real ; ls -ls )
( cd WRFV2/test/nmm_real ; ls -ls )
banner 4
#set ans = "$<"
#DAVE###################################################

#	John-specific stuff for maple is the else; part of the "using service machines".

if ( ! $clrm ) then
	pushd WRFV2
else
	if ( ! -d $TMPDIR ) then
		echo something wrong 1
	endif
	if ( ! -d $TMPDIR/RUN ) then
		mkdir $TMPDIR/RUN
		/bin/rm -fr $TMPDIR/RUN/*
	endif
	if ( -d $TMPDIR/RUN ) then
		tar cf - ./WRFV2/test ./WRFV2/main | ( cd $TMPDIR/RUN ; tar xvf - )
		pushd WRFV2
	else
		echo something wrong 2
		exit
	endif
endif

#	Here we initialize our output message.

if ( -e ${DEF_DIR}/wrftest.output ) rm ${DEF_DIR}/wrftest.output
echo "Architecute: $ARCH[1]      machine: `hostname`" >>! ${DEF_DIR}/wrftest.output
echo "WRFV2 source from: $acquire_from " >>! ${DEF_DIR}/wrftest.output
echo "Number of OpenMP processes to use: $OPENMP" >>! ${DEF_DIR}/wrftest.output
echo "Number of MPI    processes to use: $Num_Procs" >>! ${DEF_DIR}/wrftest.output
set name = ( `grep ^${user}: /etc/passwd | cut -d: -f5` ) 
echo "Test conducted by $name" >>! ${DEF_DIR}/wrftest.output
echo " " >>! ${DEF_DIR}/wrftest.output
echo "Real data case for EM is from $dataset " >>! ${DEF_DIR}/wrftest.output
echo " " >>! ${DEF_DIR}/wrftest.output
echo "The em real and ideal forecasts will be nested: $NESTED " >>! ${DEF_DIR}/wrftest.output
echo " " >>! ${DEF_DIR}/wrftest.output
if ( $REG_TYPE == BIT4BIT ) then
	echo "This is a bit-wise (traditional) regression test. " >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
else if ( $REG_TYPE == OPTIMIZED ) then
	echo "This is a fully optimized regression test. " >>! ${DEF_DIR}/wrftest.output
	echo "No inter-comparisons are made. " >>! ${DEF_DIR}/wrftest.output
	echo " " >>! ${DEF_DIR}/wrftest.output
endif
cat ${CUR_DIR}/version_info >>! ${DEF_DIR}/wrftest.output

#	There are three WRF em executables to be considered that can run in threaded and
#	distributed memory.  The 2d hills and 2d squall lines cannot be parallelized with
#	MPI, and are therefore not considered in this shell.  The nmm is only run with 
#	distributed memory (1 vs 4 procs).

set first_time_in = TRUE
foreach core ( $CORES )
#DAVE###################################################
echo doing core $core
banner 5
#set ans = "$<"
#DAVE###################################################

	#	Cores to test.

        set ZAP_SERIAL_FOR_THIS_CORE          = FALSE
        set ZAP_OPENMP_FOR_THIS_CORE          = FALSE
	if      ( `echo $core | cut -c 1-2` == em ) then
		setenv WRF_EM_CORE	1
		setenv WRF_NMM_CORE	0
		setenv WRF_COAMPS_CORE	0
		setenv WRF_EXP_CORE	0
                set ZAP_SERIAL_FOR_THIS_CORE          = FALSE
                set ZAP_OPENMP_FOR_THIS_CORE          = FALSE
        else if ( `echo $core | cut -c 1-3` == nmm ) then
		setenv WRF_EM_CORE	0
		setenv WRF_NMM_CORE	1
		setenv WRF_COAMPS_CORE	0
		setenv WRF_EXP_CORE	0
                set ZAP_SERIAL_FOR_THIS_CORE          = TRUE
                set ZAP_OPENMP_FOR_THIS_CORE          = TRUE
	endif

	#	Here we are looping over all of the various compilation configurations,
	#	such as serial only, OpenMP only, MPI only, etc.  Each architecture
	#	has its own list of these options.  We build each of the executables for
	#	this particular ${core}.
	
	foreach compopt ( $COMPOPTS )
#DAVE###################################################
echo doing compile option $compopt
banner 6
#set ans = "$<"
#DAVE###################################################

		#	We sometimes are interested in bypassing the OpenMP option.

		if ( $compopt == $COMPOPTS[2] ) then
			if ( $ZAP_OPENMP == TRUE || $ZAP_OPENMP_FOR_THIS_CORE == TRUE ) then
				goto GOT_THIS_EXEC
			endif
		endif

		#	NMM only runs parallel
		if ( $compopt == $COMPOPTS[1] ) then
			if ( $ZAP_SERIAL == TRUE || $ZAP_SERIAL_FOR_THIS_CORE == TRUE ) then
				goto GOT_THIS_EXEC
			endif
		endif

		if ( `uname` == AIX ) goto BUILD_REGARDLESS
	
		#	Did we already build this one?

		if  ( $core == em_real )  then
			if      ( ( $compopt == $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e main/real_${core}.exe.1 ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV2/external/io_netcdf/diffwrf ) ) then
				goto GOT_THIS_EXEC
			else if ( ( $compopt != $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV2/external/io_netcdf/diffwrf ) ) then
				goto GOT_THIS_EXEC
			endif
		else
			if      ( ( $compopt == $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e main/ideal_${core}.exe.1 ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV2/external/io_netcdf/diffwrf ) ) then
				goto GOT_THIS_EXEC
			else if ( ( $compopt != $COMPOPTS[1] ) && \
                                  ( -e main/wrf_${core}.exe.$compopt ) && \
                                  ( -e ${DEF_DIR}/regression_test/WRFV2/external/io_netcdf/diffwrf ) ) then
				goto GOT_THIS_EXEC
			endif
		endif

		BUILD_REGARDLESS:
	
		#	The WRF configuration file works with a single integer
		#	input, which is the compiler option.  By convention, option $COMPOPTS[1] is
		#	serial, $COMPOPTS[2] is OMP, and $COMPOPTS[3] is MPI.
	
#DAVE###################################################
echo start build mechanism
banner 7
#set ans = "$<"
#DAVE###################################################
		./clean
		echo $compopt | ./configure
	
		#	Decide whether this a bit-for-bit run or an fully optimized run.  We are just
		#	tinkering with the configure.user file optimization here.

		if ( $REG_TYPE == BIT4BIT ) then
			if ( `uname` == AIX ) then
				if ( ( $compopt == $COMPOPTS[1] ) || ( $compopt == $COMPOPTS[3] ) ) then
					sed -e '/^OMP/d' -e '/^FCOPTIM/d' -e '/^FCDEBUG/s/#//g' ./configure.wrf >! foo ; /bin/mv foo configure.wrf
				else
					sed -e '/^OMP/s/noauto/noauto:noopt/' \
					                 -e '/^FCOPTIM/d' -e '/^FCDEBUG/s/#//g' ./configure.wrf >! foo ; /bin/mv foo configure.wrf
				endif
			else if ( `uname` == Linux ) then
				if ( ( $compopt == $COMPOPTS[1] ) || ( $compopt == $COMPOPTS[3] ) ) then
					sed -e '/^OMP/d' -e '/^FCOPTIM/d' -e '/^FCDEBUG/s/#-g/-O0/g' ./configure.wrf >! foo ; /bin/mv foo configure.wrf
				else
					sed              -e '/^FCOPTIM/d' -e '/^FCDEBUG/s/#-g/-O0/g' ./configure.wrf >! foo ; /bin/mv foo configure.wrf
				endif
			else
		 		if ( ( $compopt == $COMPOPTS[1] ) || ( $compopt == $COMPOPTS[3] ) ) then
					sed -e '/^OMP/d' -e '/^FCOPTIM/d' -e '/^FCDEBUG/s/#//g' ./configure.wrf >! foo ; /bin/mv foo configure.wrf
				else
					sed              -e '/^FCOPTIM/d' -e '/^FCDEBUG/s/#//g' ./configure.wrf >! foo ; /bin/mv foo configure.wrf
				endif
			endif
		endif
#DAVE###################################################
echo configure built with optim mods removed, ready to compile
banner 8
#set ans = "$<"
#DAVE###################################################
	
		#	Build this executable
		
		./compile $core
#DAVE###################################################
echo compile done
banner 9
#set ans = "$<"
#DAVE###################################################
	
		#	Did the compile work?

		set ok = $status
                if ( ! -x main/wrf.exe ) set ok = 1

		if ( $ok != 0 ) then
			echo "SUMMARY compilation    for $core           parallel $compopt FAIL" >>! ${DEF_DIR}/wrftest.output
			$MAIL -s "REGRESSION FAILURE $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
			exit ( 3 )
		else
			echo "SUMMARY compilation    for $core           parallel $compopt PASS" >>! ${DEF_DIR}/wrftest.output
			echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
			mv main/wrf.exe main/wrf_${core}.exe.$compopt
			if (  ( $core == em_real ) && ( $compopt == $COMPOPTS[1] ) ) then
				mv main/real.exe main/real_${core}.exe.1
			else if ( $compopt == $COMPOPTS[1] ) then
				mv main/ideal.exe main/ideal_${core}.exe.1
			endif
#DAVE###################################################
echo exec exists
ls -ls main/wrf_${core}.exe.$compopt main/real_${core}.exe.1   main/ideal_${core}.exe.1
banner 10
#set ans = "$<"
#DAVE###################################################
		endif

		GOT_THIS_EXEC:

		if ( $clrm ) then
			cp main/*exe* $TMPDIR/RUN/WRFV2/main
		endif
	
	end

	if ( $clrm ) then
		pushd $TMPDIR/RUN/WRFV2
	endif
	
	#	We have all of the executables built, now we run'em.  This is a loop
	#	over all of the various physics options for this particular
	#	${core}.  Inside the physics loop, we loop over the parallel options.
	#	This allows us to use the same WRF input files for each of the parallel
	#	choices for a single physics loop index.

	foreach phys_option ( $PHYSOPTS )
#DAVE###################################################
echo which phys option $phys_option
banner 11
#set ans = "$<"
#DAVE###################################################
	
		#	For each of the executables, we need to run several physics
		#	options.  

		if (  $core == em_real )  then

                        set filetag=$filetag_real

			foreach compopt ( $COMPOPTS )
#DAVE###################################################
echo real if filetag is $filetag
echo compopt = $compopt
banner 12
#set ans = "$<"
#DAVE###################################################

				#	We sometimes are interested in bypassing the OpenMP option.

				if ( $compopt == $COMPOPTS[2] ) then
					if ( $ZAP_OPENMP == TRUE || $ZAP_OPENMP_FOR_THIS_CORE == TRUE ) then
						goto BYPASS_COMP_LOOP_REAL
					endif
				endif

				if ( $compopt == $COMPOPTS[1] ) then
					if ( $ZAP_SERIAL == TRUE || $ZAP_SERIAL_FOR_THIS_CORE == TRUE ) then
						goto BYPASS_COMP_LOOP_REAL
					endif
				endif

				pushd test/$core
				
				#
				#	Create the correct namelist.input file for real data cases.
				#

				cp ${CUR_DIR}/phys_real_${phys_option} phys_opt
				cp ${CUR_DIR}/dom_real dom_real

				set time_step = `awk ' /^ time_step /{ print $3 } ' namelist.input.$dataset | cut -d, -f1`

				#	Wanna do more/less time steps on the real cases?  Easy. Those last two numbers
				#	in the eqns are all you need.  Their product must be 60.  So, instead of 3 and 20,
				#	(3 coarse grid timesteps), you could use 20 and 3 (20 coarse grid time steps).

				if      ( $NESTED == TRUE ) then
					@ run_seconds = $time_step * 3
					@ history_interval = $time_step / 20
				else if ( $NESTED != TRUE ) then
					@ run_seconds = $time_step * 10
					@ history_interval = $time_step / 6
				endif
				rm ed_in namelist.input.temp
				cat >! ed_in << EOF
g/run_seconds/s/[0-9]/$run_seconds
g/history_interval/s/[0-9][0-9][0-9]/$history_interval
w namelist.input.temp
q
EOF
				ed namelist.input.$dataset < ed_in

				sed -e '/^ mp_physics/,/ensdim/d' -e '/^ &physics/r ./phys_opt' \
				    -e '/^ time_step /,/^ smooth_option/d' -e '/^ &domains/r ./dom_real' \
				    -e 's/ frames_per_outfile *= [0-9][0-9]*/ frames_per_outfile = 200/g' \
				    -e 's/ run_days *= [0-9][0-9]*/ run_days = 0/g' \
				    -e 's/ run_hours *= [0-9][0-9]*/ run_hours = 0/g' \
				    -e 's/ run_minutes *= [0-9][0-9]*/ run_minutes = 0/g' \
				namelist.input.temp >! namelist.input

				/bin/cp namelist.input $TMPDIR/namelist.input.$core.${phys_option}.$compopt
#DAVE###################################################
echo built namelist $TMPDIR/namelist.input.$core.${phys_option}.$compopt
cat  $TMPDIR/namelist.input.$core.${phys_option}.$compopt
echo need history interval to be 30
banner 13
#set ans = "$<"
#DAVE###################################################
#DAVE###################################################
echo skipped link of data files, we push them elsewhere
ls -ls wrf_real*
banner 14
#set ans = "$<"
#DAVE###################################################

				#	If this is the serial code, generate the IC and BC.  The real.exe program is not
				#	parallelized, so the data is generated and saved for the rest of the parallel tests.
				#	This data is necessarily updated for each of the physics tests.

				if ( $compopt == $COMPOPTS[1] ) then

					#	Zap any old input data laying around.

					rm wrfinput_d01 >& /dev/null
					rm wrfbdy_d01   >& /dev/null

					if ( $NESTED == TRUE ) then
						setenv OMP_NUM_THREADS 1
						if ( `uname` == AIX ) then
							setenv XLSMPOPTS "parthds=1"
						endif
						$SERIALRUNCOMMAND ../../main/real_${core}.exe.1 >! print.out.real_${core}_Phys=${phys_option}_Parallel=${compopt}
					else if ( $NESTED != TRUE ) then
						../../main/real_${core}.exe.1 >&! print.out.real_${core}_Phys=${phys_option}_Parallel=${compopt}
					endif
#DAVE###################################################
echo finished real
banner 15
#set ans = "$<"
#DAVE###################################################

					grep "SUCCESS COMPLETE" print.out.real_${core}_Phys=${phys_option}_Parallel=${compopt} >& /dev/null
					set success = $status

					#	Did making the IC BC files work?

					if ( ( -e wrfinput_d01 ) && ( -e wrfbdy_d01 ) && ( $success == 0 ) ) then
						echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt PASS" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
					else
						echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt FAIL" >>! ${DEF_DIR}/wrftest.output
						$MAIL -s "WRF FAIL making IC/BC $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
						exit ( 4 )
					endif
#DAVE###################################################
echo IC BC must be OK
ls -ls wrfi* wrfb*
ncdump -v Times wrfb* | tail -20
banner 16
#set ans = "$<"
#DAVE###################################################
				endif
		
				#	Run the forecast for this core, physics package and parallel option

				rm $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt >& /dev/null

				if      ( $compopt == $COMPOPTS[1] ) then
					setenv OMP_NUM_THREADS 1
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=1"
					endif
					if ( $NESTED == TRUE ) then
						$SERIALRUNCOMMAND ../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}.exe
					else if ( $NESTED != TRUE ) then
						../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}.exe
					endif
				else if ( $compopt == $COMPOPTS[2] ) then
					setenv OMP_NUM_THREADS $OPENMP
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=${OPENMP}"
					endif
					if ( $NESTED == TRUE ) then
						$OMPRUNCOMMAND ../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}.exe
					else if ( $NESTED != TRUE ) then
						../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}.exe
					endif
				else if ( $compopt == $COMPOPTS[3] ) then
					setenv OMP_NUM_THREADS 1
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=1"
					endif
					$MPIRUNCOMMAND ../../main/wrf_${core}.exe.$compopt
					mv rsl.error.0000 print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}.exe
				endif
#DAVE###################################################
echo ran wrf fcst compopt = $compopt
banner 17
#set ans = "$<"
#DAVE###################################################

				grep "SUCCESS COMPLETE" print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}.exe
				set success = $status

				#	Did making the forecast work, by that, we mean "is there an output file created?"

				if ( ( -e wrfout_d01_${filetag} ) && ( $success == 0 ) ) then
					ncdump -h wrfout_d01_${filetag} | grep Time | grep UNLIMITED | grep currently | grep -q 2
					set ok = $status
					if ( $ok == 0 ) then
						echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt PASS" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
					else
						echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt FAIL" >>! ${DEF_DIR}/wrftest.output
						$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
						exit ( 5 )
					endif
				else
					echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt FAIL" >>! ${DEF_DIR}/wrftest.output
					$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
					exit ( 6 )
				endif
#DAVE###################################################
echo success or failure of fcst
ncdump -v Times wrfout_d01_${filetag} | tail -20
banner 18
#set ans = "$<"
#DAVE###################################################

				#	We have to save this output file for our biggy comparison after all of the
				#	parallel options have been considered.

				mv wrfout_d01_${filetag} $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt

				#       To save space, we move the executables after we are finished with them.

				if ( $phys_option == $PHYSOPTS[${#PHYSOPTS}] ) then
					mv ../../main/wrf_${core}.exe.$compopt $TMPDIR/wrf_${core}.exe.$compopt
				endif

				popd

				BYPASS_COMP_LOOP_REAL:

			end

		else if ( $core == nmm_real )  then
#DAVE###################################################
echo doing nmm pre
banner 19
#set ans = "$<"
#DAVE###################################################

                        set compopt = $COMPOPTS[3]   # ! parallel only
                        set filetag = 2003-04-17_00:00:00
                        set phys_option=1
                        pushd test/$core

                        rm wrfinput_d01 >& /dev/null
                        rm wrfbdy_d01   >& /dev/null
#DAVE###################################################
echo did rms
echo $filetag $phys_option
banner 19a
#set ans = "$<"
#DAVE###################################################

                        /bin/cp namelist.input.hi_regtest namelist.input
#DAVE###################################################
echo did cp of namelist
ls -ls namelist.input
cat namelist.input
banner 19b
#set ans = "$<"
#DAVE###################################################

			#	IBM machines have a tough time running a 1 proc job if we asked for 
			#	several MPI processes (and NMM is only MPI), so we spoon feed
			#	the IC/BC from a run formerly known as "generated on a single proc".

			if ( `uname` == AIX ) then
				set RUNCOMMAND = $MPIRUNCOMMAND

				if ( ( ! -e /ptmp/gill/wrfinput_d01 ) || ( ! -e /ptmp/gill/wrfbdy_d01 ) ) then
if ( $user != gill ) then
cat >> ${DEF_DIR}/wrftest.output << EOF


Sorry, looks like there are missing files for the IBM.
Tell Dave to get them from 
/GILL/AIX_NMM_IC.tar and to untar the file in /ptmp/gill
on the IBM `hostname`
The Mgmt
EOF
$MAIL -s "REGRESSION FAILURE $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
exit ( 1 ) 
endif

					pushd /ptmp/gill
					msread AIX_NMM_IC.tar /GILL/AIX_NMM_IC.tar 
					tar -xf AIX_NMM_IC.tar
					popd
				endif

				ln -sf /ptmp/gill/wrfinput_d01 .
				ln -sf /ptmp/gill/wrfbdy_d01 .
			else
				set RUNCOMMAND = `echo $MPIRUNCOMMAND | sed "s/$Num_Procs/1/"`
				$RUNCOMMAND ../../main/convert_nmm.exe >&! print.out.convert_${core}_Phys
			endif
#DAVE###################################################
echo ran nmm convert.exe except on IBM
ls -ls print.out.convert_${core}_Phys
tail print.out.convert_${core}_Phys
banner 19c
#set ans = "$<"
#DAVE###################################################
			/bin/mv rsl.error.0000 print.out.convert.exe
#DAVE###################################################
echo did convert for nmm input, it fails on IBM, but we copied 1 p input data over
ls -lsL wrfinput* wrfb*
ncdump -v Times wrfb* | tail -20
banner 20
#set ans = "$<"
#DAVE###################################################

                        # run on 1 and then on Num_Procs processors
                        foreach n ( 1 $Num_Procs )
#DAVE###################################################
echo running nmm on $n procs
banner 21
#set ans = "$<"
#DAVE###################################################
			if ( `uname` == AIX ) then
				set RUNCOMMAND = $MPIRUNCOMMAND
			else
				set RUNCOMMAND = `echo $MPIRUNCOMMAND | sed "s/$Num_Procs/$n/"`
			endif

                         # NMM can fail on spurious fp exceptions that don't affect soln. Retry if necessary.
                         set tries=0
                         while ( $tries < 2 )
#DAVE###################################################
echo try attempt $tries allowed to be less than 2
banner 22
#set ans = "$<"
#DAVE###################################################
                          @ tries = $tries + 1
                          $RUNCOMMAND ../../main/wrf_${core}.exe.$compopt
                          mv rsl.error.0000 print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}.exe_${n}p
			  grep "SUCCESS COMPLETE" print.out.wrf_${core}_Phys=${phys_option}_Parallel=${compopt}.exe_${n}p
			  set success = $status
                          if ( ( -e wrfout_d01_${filetag} ) && ( $success == 0 ) ) then
                                ncdump -h wrfout_d01_${filetag} | grep Time | grep UNLIMITED | grep currently | grep -q 2
                                set ok = $status
                                if ( $ok == 0 ) then
                                       echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt PASS" >>! ${DEF_DIR}/wrftest.output
                                       echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
                                       set tries=2  # success, bail from loop
                                else
                                       echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt FAIL" >>! ${DEF_DIR}/wrftest.output
                                       $MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
                                       if ( $tries == 2 ) exit ( 5 )
                                endif
                           else
                                echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt FAIL" >>! ${DEF_DIR}/wrftest.output
                                $MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
                                if ( $tries == 2 ) exit ( 6 )
                           endif
			   mv wrfout_d01_${filetag} $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.${compopt}_${n}p
#DAVE###################################################
echo did nmm fcst
ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.${compopt}_${n}p
banner 23
#set ans = "$<"
#DAVE###################################################
                         end
                        end

                        popd

		else

#DAVE###################################################
echo doing ideal runs
banner 24
#set ans = "$<"
#DAVE###################################################
			#	The ideal cases have different physics tests than the real cases.  If this is 
			#	more that the total number of ideal physics experiments that we were led to 
			#	believe would exist, jump to the end of the physics loop.

			if ( $phys_option > $Max_Ideal_Physics_Options ) then
				goto BOTTOM_OF_PHYSICS_LOOP
			endif

                        set filetag=$filetag_ideal

			foreach compopt ( $COMPOPTS )
#DAVE###################################################
echo doing compopt = $compopt
echo filetag = $filetag
banner 25
#set ans = "$<"
#DAVE###################################################


				#	We sometimes are interested in bypassing the OpenMP option.

				if ( $compopt == $COMPOPTS[2] ) then
					if ( $ZAP_OPENMP == TRUE ) then
						goto BYPASS_COMP_LOOP_IDEAL
					endif
				endif

				pushd test/$core

				if ( ! -e namelist.input.template ) cp namelist.input namelist.input.template

				#	Create the correct namelist.input file.

				cp ${CUR_DIR}/dom_ideal dom_ideal
				if      ( $core == em_quarter_ss )  then
					cp ${CUR_DIR}/phys_quarter_ss_${phys_option}a phys_tke
					cp ${CUR_DIR}/phys_quarter_ss_${phys_option}b phys_mp
					cp ${CUR_DIR}/phys_quarter_ss_${phys_option}c phys_nh
					cp ${CUR_DIR}/phys_quarter_ss_${phys_option}d phys_bc
				else if ( $core == em_b_wave     ) then
					cp ${CUR_DIR}/phys_b_wave_${phys_option}a     phys_tke
					cp ${CUR_DIR}/phys_b_wave_${phys_option}b     phys_mp
					cp ${CUR_DIR}/phys_b_wave_${phys_option}c     phys_nh
				endif
			
				if      ( $NESTED == TRUE ) then
					if      ( $core == em_quarter_ss )  then
						sed -e 's/ run_days *= *[0-9][0-9]*/ run_days = 00/g'                              		\
						    -e 's/ run_minutes *= *[0-9][0-9]*/ run_minutes = 1/g'                         		\
						    -e 's/ run_seconds *= *[0-9][0-9]*/ run_seconds = 0/g'                         		\
		                                    -e 's/ history_interval *= [0-9][0-9]*/ history_interval = 1/g'                     	\
						    -e 's/ frames_per_outfile *= [0-9][0-9]*/ frames_per_outfile = 100/g'               	\
						    -e '/^ diff_opt/d' -e '/^ km_opt/d' -e '/^ damp_opt/d' -e '/^ rk_ord/r ./phys_tke' 		\
						    -e '/^ mp_physics/d' -e '/^ &physics/r ./phys_mp' 						\
						    -e '/^ non_hydrostatic/d' -e '/^ epssm/r ./phys_nh' 					\
						    -e '/^ periodic_x/d' -e '/^ open_xs/d' -e '/^ open_xe/d' 					\
						    -e '/^ periodic_y/d' -e '/^ open_ys/d' -e '/^ open_ye/d' 					\
						    -e '/^ &bdy_control/r ./phys_bc' 								\
						    -e '/^ max_dom/d' -e '/^ time_step_fract_den/r ./dom_ideal'					\
						    ./namelist.input.template >! namelist.input
					else if ( $core == em_b_wave     ) then
						sed -e 's/ run_days *= *[0-9][0-9]*/ run_days = 00/g'                              		\
						    -e 's/ run_minutes *= *[0-9][0-9]*/ run_minutes = 20/g'                       		\
						    -e 's/ history_interval *= [0-9][0-9]*/ history_interval = 20/g'                   	\
						    -e 's/ frames_per_outfile *= [0-9][0-9]*/ frames_per_outfile = 100/g'               	\
						    -e '/^ diff_opt/d' -e '/^ km_opt/d' -e '/^ damp_opt/d' -e '/^ rk_ord/r ./phys_tke' 		\
						    -e '/^ mp_physics/d' -e '/^ &physics/r ./phys_mp' 						\
						    -e '/^ non_hydrostatic/d' -e '/^ epssm/r ./phys_nh' 					\
						    -e '/^ max_dom/d' -e '/^ time_step_fract_den/r ./dom_ideal'					\
						./namelist.input.template >! namelist.input
					endif
				else if ( $NESTED != TRUE ) then
					if      ( $core == em_quarter_ss )  then
						sed -e 's/ run_days *= *[0-9][0-9]*/ run_days = 00/g'                              		\
						    -e 's/ run_minutes *= *[0-9][0-9]*/ run_minutes = 2/g'                         		\
		                                    -e 's/ history_interval *= [0-9][0-9]*/ history_interval = 2/g'                     	\
						    -e 's/ frames_per_outfile *= [0-9][0-9]*/ frames_per_outfile = 100/g'               	\
						    -e '/^ diff_opt/d' -e '/^ km_opt/d' -e '/^ damp_opt/d' -e '/^ rk_ord/r ./phys_tke' 		\
						    -e '/^ mp_physics/d' -e '/^ &physics/r ./phys_mp' 						\
						    -e '/^ non_hydrostatic/d' -e '/^ epssm/r ./phys_nh' 					\
						    -e '/^ periodic_x/d' -e '/^ open_xs/d' -e '/^ open_xe/d' 					\
						    -e '/^ periodic_y/d' -e '/^ open_ys/d' -e '/^ open_ye/d' 					\
						    -e '/^ &bdy_control/r ./phys_bc' 								\
						    -e '/^ max_dom/d' -e '/^ time_step_fract_den/r ./dom_ideal'					\
						    ./namelist.input.template >! namelist.input
					else if ( $core == em_b_wave     ) then
						sed -e 's/ run_days *= *[0-9][0-9]*/ run_days = 00/g'                              		\
						    -e 's/ run_minutes *= *[0-9][0-9]*/ run_minutes = 100/g'                       		\
						    -e 's/ history_interval *= [0-9][0-9]*/ history_interval = 100/g'                   	\
						    -e 's/ frames_per_outfile *= [0-9][0-9]*/ frames_per_outfile = 100/g'               	\
						    -e '/^ diff_opt/d' -e '/^ km_opt/d' -e '/^ damp_opt/d' -e '/^ rk_ord/r ./phys_tke' 		\
						    -e '/^ mp_physics/d' -e '/^ &physics/r ./phys_mp' 						\
						    -e '/^ non_hydrostatic/d' -e '/^ epssm/r ./phys_nh' 					\
						    -e '/^ max_dom/d' -e '/^ time_step_fract_den/r ./dom_ideal'					\
						./namelist.input.template >! namelist.input
					endif
				endif
#DAVE###################################################
echo built namelist 
ls -ls namelist.input
banner 26
#set ans = "$<"
#DAVE###################################################

				#	If this is the serial code, generate the IC and BC.  The ideal.exe program is not
				#	parallelized, so the data is generated and saved for the rest of the parallel tests.

				if ( $compopt == $COMPOPTS[1] ) then

					#	Zap any old input data laying around.

					rm wrfinput_d01 >& /dev/null
					rm wrfbdy_d01   >& /dev/null

					../../main/ideal_${core}.exe.1 >&! print.out.ideal_${core}_Parallel=${compopt}
#DAVE###################################################
echo ran ideal
ls -ls wrfiput*
banner 27
#set ans = "$<"
#DAVE###################################################

					grep "SUCCESS COMPLETE" print.out.ideal_${core}_Parallel=${compopt} >& /dev/null
					set success = $status

					#	Did making the IC BC files work?

					if ( ( -e wrfinput_d01 ) && ( -e wrfbdy_d01 ) && ( $success == 0 ) ) then
						echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt PASS" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
					else
						echo "SUMMARY generate IC/BC for $core physics $phys_option parallel $compopt FAIL" >>! ${DEF_DIR}/wrftest.output
						$MAIL -s "WRF FAIL making IC/BC $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
						exit ( 7 )
					endif
				endif
		
				#	Run the forecast for this core and parallel option

				rm $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt >& /dev/null

				if      ( $compopt == $COMPOPTS[1] ) then
					setenv OMP_NUM_THREADS 1
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=1"
					endif
					if ( $NESTED == TRUE ) then
						$SERIALRUNCOMMAND ../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Parallel=${compopt}.exe
					else if ( $NESTED != TRUE ) then
						../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Parallel=${compopt}.exe
					endif
				else if ( $compopt == $COMPOPTS[2] ) then
					setenv OMP_NUM_THREADS $OPENMP
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=${OPENMP}"
					endif
					if ( $NESTED == TRUE ) then
						$OMPRUNCOMMAND ../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Parallel=${compopt}.exe
					else if ( $NESTED != TRUE ) then
						../../main/wrf_${core}.exe.$compopt >! print.out.wrf_${core}_Parallel=${compopt}.exe
					endif
				else if ( $compopt == $COMPOPTS[3] ) then
					setenv OMP_NUM_THREADS 1
					if ( `uname` == AIX ) then
						setenv XLSMPOPTS "parthds=1"
					endif
					$MPIRUNCOMMAND ../../main/wrf_${core}.exe.$compopt
					mv rsl.error.0000 print.out.wrf_${core}_Parallel=${compopt}.exe
				endif
#DAVE###################################################
echo ran ideal fcst
banner 28
#set ans = "$<"
#DAVE###################################################

				grep "SUCCESS COMPLETE" print.out.wrf_${core}_Parallel=${compopt}.exe
				set success = $status

				#	Did making the forecast work, by that, we mean "is there an output file created?"

				if ( ( -e wrfout_d01_${filetag} ) && ( $success == 0 ) ) then
					ncdump -h wrfout_d01_${filetag} | grep Time | grep UNLIMITED | grep currently | grep -q 2
					set ok = $status
					if ( $ok == 0 ) then
						echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt PASS" >>! ${DEF_DIR}/wrftest.output
						echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
					else
						echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt FAIL" >>! ${DEF_DIR}/wrftest.output
						$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
						exit ( 8 )
					endif
				else
					echo "SUMMARY generate FCST  for $core physics $phys_option parallel $compopt FAIL" >>! ${DEF_DIR}/wrftest.output
					$MAIL -s "WRF FAIL FCST $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
					exit ( 9 )
				endif

				#	We have to save this output file for our biggy comparison after all of the
				#	parallel options have been considered.

				mv wrfout_d01_${filetag} $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt

				#       To save space, we move the executables after we are finished with them.

				if ( $phys_option == $Max_Ideal_Physics_Options ) then
					mv ../../main/wrf_${core}.exe.$compopt $TMPDIR/wrf_${core}.exe.$compopt
				endif
#DAVE###################################################
echo fcst was a success
ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt
ncdump -v Times $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$compopt | tail -20
banner 29
#set ans = "$<"
#DAVE###################################################

				popd

				BYPASS_COMP_LOOP_IDEAL:

			end

		endif

		#	OK, once more, we gotta check if this is a BIT4BIT run.  If so then there
		#	are a number of comparisons to do.  If this is a an OPTIMIZED run, then the
		#	comparisons will fail the bit-wise comparisons.

		if ( $REG_TYPE == BIT4BIT) then

	                if ( $core == nmm_real ) then
	
				pushd ${DEF_DIR}/regression_test/WRFV2/test/$core
				set DIFFWRF = ${DEF_DIR}/regression_test/WRFV2/external/io_netcdf/diffwrf
	
	                        if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_1p ) && \
	                             ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_${Num_Procs}p ) ) then
	                                set foo1 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_1p `)
	                                set foo2 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_${Num_Procs}p `)
	                                set size1 = $foo1[6]
	                                set size2 = $foo2[6]
	                                if ( $size1 == $size2 ) then
	                                        set RIGHT_SIZE = TRUE
	                                else
	                                        set RIGHT_SIZE = FALSE
	                                endif
	                        else
	                                set RIGHT_SIZE = FALSE
	                        endif
	
	                        #       1p vs Num_Procs MPI
	
	                        rm fort.88 fort.98 >& /dev/null
	                        if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_1p ) && \
	                             ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_${Num_Procs}p ) && \
	                             ( $RIGHT_SIZE == TRUE ) ) then
	                                $DIFFWRF $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_1p \
	                                         $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_${Num_Procs}p >& /dev/null
	                        else
	                                touch fort.88 fort.98
	                        endif
	                        if ( ! -e fort.88 ) then
	                                echo "SUMMARY 1 vs $Num_Procs MPI  for $core physics $phys_option            PASS" >>! ${DEF_DIR}/wrftest.output
	                                echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
	                        else
	                                echo "SUMMARY 1 vs $Num_Procs MPI  for $core physics $phys_option            FAIL" >>! ${DEF_DIR}/wrftest.output
	                                echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
	                        endif
	
	
	                        #       1p vs baseline output from 20031015
	
	                        rm fort.88 fort.98 >& /dev/null
	                        if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_1p ) && \
	                             ( -e wrfout_d01_2003-04-17_00:00:00.nmm_baseline ) ) then
	                                $DIFFWRF $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3]_1p \
	                                         wrfout_d01_2003-04-17_00:00:00.nmm_baseline >& /dev/null
	                        else
	                                touch fort.88 fort.98
	                        endif
	                        if ( ! -e fort.88 ) then
	                                echo "SUMMARY 1 vs 20031015 baseline output for $core physics $phys_option            PASS" >>! ${DEF_DIR}/wrftest.output
	                                echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
	                        else
	                                echo "SUMMARY 1 vs 20031015 baseline output for $core physics $phys_option            FAIL" >>! ${DEF_DIR}/wrftest.output
	                                echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
	                        endif
	
	                        popd
	
	                        # only one pass of physics for NMM right now
	                        goto ALL_SHE_WROTE_FOR_NMM 
	
			else if ( ${#COMPOPTS} != 1 ) then
	
			#	If there is only a single parallel option, then we are just trying to
			#	build the serial code.  That implies no comparisons are needed.
	
				#	All of the forecasts for this set of physics and core have been
				#	generated.  We now compare the WRF model output files to see
				#	if they are S^2D^2.
		
				pushd ${DEF_DIR}/regression_test/WRFV2/test/$core
				set DIFFWRF = ${DEF_DIR}/regression_test/WRFV2/external/io_netcdf/diffwrf
	
				#	Are we skipping the OpenMP runs?
	
				if ( $ZAP_OPENMP == TRUE ) then
					goto BYPASS_OPENMP_SUMMARY1
				endif
	
				#	Are the files the same size?  If not, then only the initial times
				#	will be compared.  That means, on a failure to run a forecast, the
				#	diffwrf will give a pass.  We need to root out this evil.
	
				if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] ) && \
				     ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[2] ) ) then
					set foo1 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] `)
					set foo2 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[2] `)
					set size1 = $foo1[6]
					set size2 = $foo2[6]
					if ( $size1 == $size2 ) then
						set RIGHT_SIZE_OMP = TRUE
					else
						set RIGHT_SIZE_OMP = FALSE
					endif
				else
					set RIGHT_SIZE_OMP = FALSE
				endif
	
				BYPASS_OPENMP_SUMMARY1:
	
				if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] ) && \
				     ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3] ) ) then
					set foo1 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] `)
					set foo3 = ( ` \ls -ls $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3] `)
					set size1 = $foo1[6]
					set size3 = $foo3[6]
					if ( $size1 == $size3 ) then
						set RIGHT_SIZE_MPI = TRUE
					else
						set RIGHT_SIZE_MPI = FALSE
					endif
				else
					set RIGHT_SIZE_MPI = FALSE
				endif
	
				#	Are we skipping the OpenMP runs?
	
				if ( $ZAP_OPENMP == TRUE ) then
					goto BYPASS_OPENMP_SUMMARY2
				endif
		
				#	Serial vs OpenMP
		
				rm fort.88 fort.98 >& /dev/null
				if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] ) && \
				     ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[2] ) && \
				     ( $RIGHT_SIZE_OMP == TRUE ) ) then
					$DIFFWRF $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] \
					         $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[2] >& /dev/null
				else
					touch fort.88 fort.98
				endif
				if ( ! -e fort.88 ) then
					echo "SUMMARY serial vs OMP  for $core physics $phys_option            PASS" >>! ${DEF_DIR}/wrftest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
				else
					echo "SUMMARY serial vs OMP  for $core physics $phys_option            FAIL" >>! ${DEF_DIR}/wrftest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
				endif
	
				BYPASS_OPENMP_SUMMARY2:
		
				#	Serial vs MPI
		
				rm fort.88 fort.98 >& /dev/null
				if ( ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] ) && \
				     ( -e $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3] ) && \
				     ( $RIGHT_SIZE_MPI == TRUE ) ) then
					$DIFFWRF $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[1] \
					         $TMPDIR/wrfout_d01_${filetag}.${core}.${phys_option}.$COMPOPTS[3] >& /dev/null
				else
					touch fort.88 fort.98
				endif
				if ( ! -e fort.88 ) then
					echo "SUMMARY serial vs MPI  for $core physics $phys_option            PASS" >>! ${DEF_DIR}/wrftest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
				else
					echo "SUMMARY serial vs MPI  for $core physics $phys_option            FAIL" >>! ${DEF_DIR}/wrftest.output
					echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output
				endif
		
				popd
	
			endif

		endif

		BOTTOM_OF_PHYSICS_LOOP:
		
	end

ALL_SHE_WROTE_FOR_NMM:

	echo "-------------------------------------------------------------" >> ${DEF_DIR}/wrftest.output

        if ( $clrm ) then
          popd
        endif
		
end


#	How long did this take.

set end = ( `date` )
echo "Start WRF Regression: $start " >> ${DEF_DIR}/wrftest.output
echo "End   WRF Regression: $end   " >> ${DEF_DIR}/wrftest.output

#	We have done all of the tests, and placed the PASS FAIL labels in the
#	output file.  If there are any FAIL messages, we are in trouble.

grep FAIL ${DEF_DIR}/wrftest.output
set ok = $status

#	Send email of the status.

if ( $ok == 0 ) then
	$MAIL -s "REGRESSION FAILURE $ARCH[1] " $FAIL_MAIL < ${DEF_DIR}/wrftest.output
else
	$MAIL -s "REGRESSION SUCCESS $ARCH[1] " $GOOD_MAIL < ${DEF_DIR}/wrftest.output
endif

#	Clean left up detritus

cd $CUR_DIR

rm -rf dom_*eal >& /dev/null
rm -rf phys_real_* >& /dev/null
rm -rf phys_quarter_* >& /dev/null
rm -rf phys_b_wave_* >& /dev/null
rm -rf version_info >& /dev/null
rm -rf machfile >& /dev/null
