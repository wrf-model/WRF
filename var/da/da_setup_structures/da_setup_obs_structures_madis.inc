subroutine da_setup_obs_structures_madis( ob, iv)

!------------------------------------------------------------------------------
! PURPOSE: Define, allocate and read of observation structure.
!
! METHOD:  Define, allocate and read of observation structure.
!
! HISTORY: 05-21-03  M. Barth         The setup_obs routine to ingest data from
!                                     the FSL MADIS netCDF files.
!          10-20-03  M. Barth         - Changed calls to get SATOB data for the
!                                       changes made in the MADIS API when the
!                                       true SATWND dataset was added.  (This code
!                                       originally accessed a dummy SATW dataset.)
!                                     - Changed max_map from 150 to 2000 to 
!                                       accommodate full time window options.
!          04-01-04  M. Barth         Added MADIS COOP surface dataset.
!          08-27-04  M. Barth         - Added sonde_sfc stuff.
!                                     - Initialize new count_obs entries that we
!                                       don't use (radar, profiler, buoy).  Note
!                                       that, as with the BUFR interface, we
!                                       continue to lump profiler in with "sound"
!                                       and buoy with "ships" -- as the downstream
!                                       processing is identical regardless of
!                                       the designation, and we offer MADIS-level
!                                       control of these datasets, this is simpler
!                                       than restructuring this code.
!          02-06-06 M. Barth          Changes for WRFVAR version 2.1.
!                                     - Renamed all "fsl" references to "madis".
!                                       (Exception:  "FSL" is still used as an
!                                        argument to MINIT.)
!                                     - Changed xb to xp.  
!                                     - Removed xbx arg, so analysis time is
!                                       now determined by the NAMELIST ANALYSIS_DATE
!                                       variable.
!                                     - Changed satob to geoamv.
!                                     - Changed max_sfc from 50K to 200K.
!                                     - Changed max_raoblev from 256 to 405.
!                                     - Miscellaneous other minor changes to
!                                       conform to current paradigm.
!                                     - Added protection against invalid lat/lons,
!                                       as well as protection against latitude
!                                       at the poles -- some projections can't
!                                       handle this, and the WRFVAR routines
!                                       that do the coordinate conversions aren't
!                                       smart enough to trap this case.  [Are you
!                                       surprised?]
!          08-07-07 M. Barth          - Fixed problem introduced in 2005 version
!                                       of WRFVAR by filling in the ob_num 
!                                       and current_ob_time data structures in iv.
!          02-06-06 M. Barth          Changes for WRFVAR version 3:
!                                     - Removed xp argument.
!                                     - Changed DA_ll_to_xy call to da_llxy.
!                                     - Added trace usage.
!                                     - Changed ob_type structure to iv_type.
!                                     - Changes in iv structure:
!                                       - current_ob_time -> time
!                                       - New info structure
!                                     - Fill in global vs. local obs counts
!                                       into iv%info.  This is *presumably* the
!                                       replacement for iv%ob_num.
!                                     - Fill in iv%info with stuff that used
!                                       to go into iv%<dataset>%info.
!                                     - Since DA_Constants and DA_Read_Namelist no
!                                       longer exist, the declaration of, and
!                                       input of, the MADIS namelist variables
!                                       have been moved to this routine.
!                                     - Changed print_detail to print_detail_obs.
!                                     - Added duplication logic (see explanation
!                                       in PROCESS_DATASET).
!          11-28-08 M. Barth          Since I finally got the darn thing to
!                                     compile, I debugged the massive changes
!                                     in version 3.
!
! PARENT_MODULE: da_setup_structures
!------------------------------------------------------------------------------

   use da_tools, only : da_llxy
   IMPLICIT NONE
   
! ARGUMENTS:

   TYPE ( y_type), INTENT(OUT)    :: ob          ! Observation structure.
   TYPE (iv_type), INTENT(INOUT)  :: iv          ! O-B structure.

#ifdef MADIS

! LOCAL:

! Primarily used by DA routines:

   TYPE (multi_level_type)      :: platform

   INTEGER                      :: fm

   LOGICAL                      :: outside, outside_all

! Primarily used by MADIS ingest:

! Constants:

                                                         ! Max number of input reports: 
   INTEGER*4, PARAMETER         :: max_sfc   = 200000, & !  Surface
                                   max_raob  =   1000, & !  Radiosonde
                                   max_npn   =    160, & !  NOAA Profiler Network (NPN) winds
                                   max_acars =  60000, & !  ACARS
                                   max_map   =   2000, & !  Multi-Agency profilers (MAP)
                                   max_satw  =  60000, & !  Satellite winds

                                   max_sta   = max(max_sfc,max_raob,max_npn, &
                                                   max_acars,max_map,max_satw), &

                                                         ! Max number of input levels:
                                   max_raoblev = 405, &  !  Radiosonde
                                   max_npnlev_madis = 64, & ! NPN MADIS database
                                   max_npnlev_awips = 43, &!NPN AWIPS database
                                   max_maplev  = 202, &  !  MAP

                                   success_p =      0, & ! Successful status return
                                   qcall =         99, & ! Only return data passing all QC
                                   max_provider = 100    ! Maximum number of data providers
   REAL*4, PARAMETER            :: miss_p    = 999999.0  ! Missing data flag

! Input arrays:

   CHARACTER*10, ALLOCATABLE    :: stanam(:),provdr(:)
   CHARACTER*9, ALLOCATABLE     :: timeob(:)
   CHARACTER*1, ALLOCATABLE     :: qcd(:)
   INTEGER*4, ALLOCATABLE       :: wmoid(:),qca(:),qcr(:),nlevels(:),keep(:)
   REAL*4, ALLOCATABLE          :: lat(:),lon(:),elev(:),p(:),q(:),t(:),z(:), &
                                   dd(:),ff(:),pw(:),slp(:),levtype(:)

! Miscellaneous:

   INTEGER*4                    :: nsta,nobs,istatus,i,j,ntmp,nlev,ntmp_r, &
                                   ntmp_n,ntmp_m,keep_r(max_raob), recwin, &
                                   keep_n(max_npn),keep_m(max_map),minbck,minfwd, &
                                   nsatw_vartype,satw_var_len(5),pvdriostat,n,iost, &
                                   nlocal(num_ob_indexes),ntotal(num_ob_indexes), &
                                   nl_raob,nt_raob,nl_npn,nt_npn,nl_map,nt_map, &
                                   nlocal_out,ntotal_out
   CHARACTER*9                  :: atime
   CHARACTER*13                 :: tstr
   CHARACTER*10                 :: dpname
   CHARACTER*255                :: pvdrfile
   INTEGER*4, PARAMETER         :: lun = 19     ! LUN for namelist and data provider files
   CHARACTER*5                  :: satw_var(2,5)
   CHARACTER*4                  :: pvdriotype
   LOGICAL                      :: map_init,npn_init
   CHARACTER*20                 :: namelist_file      
   CHARACTER*29                 :: errmsg(0:8)
   INTEGER*4                    :: mfbdebug
   INTEGER*4                    :: sfclev(max_map)

!------------------------------------------------------------------------------
! Namelist options for ingest of MADIS/AWIPS datasets into 3DVAR
!------------------------------------------------------------------------------
! The user can select either the MADIS or AWIPS database for each dataset that's 
! ingest by the WRFVAR.  Each dataset's time window can be independently set as 
! well, which allows the user to select the number of minutes before and after 
! the nominal time to be used for the window, and how duplicates should be 
! handled.  For the METAR, PROFILER and GEOAMV datasets, the user can also 
! choose to ingest data from all providers of the METAR and PROFILER datasets 
! (e.g., ASOS) as well as mesonets for METAR, and from operational GOES 
! products from both satellites for GEOAMV), or the user can specify which 
! providers should be used.  For more details on how and why to select 
! different settings, see the README.MADIS_WRFVAR file in the 
! var directory.
!
! The meaning of each variable is the same for all of the datasets, so full
! comments are only included for the METAR dataset below, and where the variables
! are specific to only one or two datasets.
!------------------------------------------------------------------------------

! General

   INTEGER*4     :: madis_debug               ! 0 - no debug
                                              ! 1 - print all obs
                                              ! 2 - print all obs, stop after obs ingest
                                           
! METAR dataset

   CHARACTER*6   :: madis_metar_db            ! Database:  'FSL' or 'AWIPS'
   INTEGER*4     :: madis_metar_minbck        ! Number of minutes relative to nominal
                                              ! time at which to start time window
   INTEGER*4     :: madis_metar_minfwd        ! Number of minutes relative to nominal
                                              ! time at which to end time window
                                              ! (max(minfwd-minbck) is 180)
   INTEGER*4     :: madis_metar_recwin        ! 1 - return one record per fixed station, 
                                              !     closest to nominal time
                                              ! 2 - return one record per fixed station, 
                                              !     closest to start of window
                                              ! 3 - return one record per fixed station, 
                                              !     closest to end of window
                                              ! 4 - return all records within *window*
   LOGICAL       :: madis_metar_all_providers ! .TRUE./.FALSE. - use all providers 
   CHARACTER*255 :: madis_metar_provider_list ! Full path to file with list of providers,
                                              ! one per line; must be specified if 
                                              ! all_providers is false

! SHIPS dataset

   CHARACTER*6   :: madis_ships_db            ! Database:  'FSL' or 'AWIPS'
   INTEGER*4     :: madis_ships_minbck        ! Time window start
   INTEGER*4     :: madis_ships_minfwd        ! Time window end
   INTEGER*4     :: madis_ships_recwin        ! How to handle duplicate records

! GPSPW dataset

   CHARACTER*6   :: madis_gpspw_db            ! Database:  'FSL' or 'AWIPS'
   INTEGER*4     :: madis_gpspw_minbck        ! Time window start
   INTEGER*4     :: madis_gpspw_minfwd        ! Time window end
   INTEGER*4     :: madis_gpspw_recwin        ! How to handle duplicate records

! SOUND dataset

   CHARACTER*6   :: madis_sound_db            ! Database:  'FSL' or 'AWIPS'
   INTEGER*4     :: madis_sound_minbck        ! Time window start
   INTEGER*4     :: madis_sound_minfwd        ! Time window end
   INTEGER*4     :: madis_sound_recwin        ! How to handle duplicate records

! PROFILER dataset

   CHARACTER*6   :: madis_profiler_db            ! Database:  'FSL' or 'AWIPS'
   INTEGER*4     :: madis_profiler_minbck        ! Time window start
   INTEGER*4     :: madis_profiler_minfwd        ! Time window end
   INTEGER*4     :: madis_profiler_recwin        ! How to handle duplicate records
   LOGICAL       :: madis_profiler_all_providers ! .TRUE./.FALSE. - use all providers 
   CHARACTER*255 :: madis_profiler_provider_list ! Full path to file with list of providers

! AIREP dataset

   CHARACTER*6   :: madis_airep_db            ! Database:  'FSL' or 'AWIPS'
   INTEGER*4     :: madis_airep_minbck        ! Time window start
   INTEGER*4     :: madis_airep_minfwd        ! Time window end
   INTEGER*4     :: madis_airep_recwin        ! How to handle duplicate records

! GEOAMV dataset

   CHARACTER*6   :: madis_geoamv_db            ! Database:  'FSL' or 'AWIPS'
   INTEGER*4     :: madis_geoamv_minbck        ! Time window start
   INTEGER*4     :: madis_geoamv_minfwd        ! Time window end
   INTEGER*4     :: madis_geoamv_recwin        ! How to handle duplicate records
   LOGICAL       :: madis_geoamv_all_providers ! .TRUE./.FALSE. - use default providers 
   CHARACTER*255 :: madis_geoamv_provider_list ! Full path to file with list of providers
   LOGICAL       :: madis_geoamv_wv            ! .TRUE./.FALSE. - use water vapor winds
   LOGICAL       :: madis_geoamv_ir            ! .TRUE./.FALSE. - use infrared winds
   LOGICAL       :: madis_geoamv_vis           ! .TRUE./.FALSE. - use visible winds
   LOGICAL       :: madis_geoamv_s10           ! .TRUE./.FALSE. - use sounder channel 10 winds
   LOGICAL       :: madis_geoamv_s11           ! .TRUE./.FALSE. - use sounder channel 11 winds
 
! Note that the meaning of "madis_geoamv_all_providers" is different for GEOAMV than 
! for the other datasets.  If the variable is .TRUE., the *default* providers will be
! selected, i.e., operational products from both GOES satellites.
!------------------------------------------------------------------------------
! End of MADIS/AWIPS WRFVAR options
!------------------------------------------------------------------------------
! Namelist contents :

   NAMELIST/MADIS_GENERAL/madis_debug
   NAMELIST/MADIS_METAR/madis_metar_db,madis_metar_minbck,madis_metar_minfwd,madis_metar_recwin, &
                        madis_metar_all_providers,madis_metar_provider_list
   NAMELIST/MADIS_SHIPS/madis_ships_db,madis_ships_minbck,madis_ships_minfwd,madis_ships_recwin
   NAMELIST/MADIS_GPSPW/madis_gpspw_db,madis_gpspw_minbck,madis_gpspw_minfwd,madis_gpspw_recwin
   NAMELIST/MADIS_SOUND/madis_sound_db,madis_sound_minbck,madis_sound_minfwd,madis_sound_recwin
   NAMELIST/MADIS_PROFILER/madis_profiler_db,madis_profiler_minbck,madis_profiler_minfwd,&
                           madis_profiler_recwin,madis_profiler_all_providers,&
                           madis_profiler_provider_list
   NAMELIST/MADIS_AIREP/madis_airep_db,madis_airep_minbck,madis_airep_minfwd,madis_airep_recwin
   NAMELIST/MADIS_GEOAMV/madis_geoamv_db,madis_geoamv_minbck,madis_geoamv_minfwd,&
                         madis_geoamv_recwin,madis_geoamv_all_providers,&
                         madis_geoamv_provider_list,madis_geoamv_wv,madis_geoamv_ir,&
                         madis_geoamv_vis,madis_geoamv_s10,madis_geoamv_s11

   DATA ERRMSG/'Opening namelist.3dvar_madis',&
               'Reading madis_general namelist',&
               'Reading madis_metar namelist',&
               'Reading madis_ships namelist',&
               'Reading madis_gpspw namelist',&
               'Reading madis_sound namelist',&
               'Reading madis_profiler namelist',&
               'Reading madis_airep namelist',&
               'Reading madis_geoamv namelist'/

! Note that all of the variables sent into MADIS routines are explicitly
! declared INTEGER*4 and REAL*4, rather than letting the generic kind be
! established based on the choices used in the WRF options.  On at least
! one machine, the WRF-default for REAL is REAL*8, but the MADIS library
! uses REAL*4.  To stay on the safe side, we'll declare all the int/real
! MADIS variables to what we want.

   if (trace_use) CALL da_trace_entry("da_setup_obs_structures_madis")

!------------------------------------------------------------------------------
! MADIS-related initialization
!------------------------------------------------------------------------------
! Note that any variables prefixed with "madis_" are namelist variables read in
! from namelist.3dvar_madis.  These are used to give the user the ability to
! set all relevant MADIS options for the various datasets.

   write(6,*)' -----------------------------------------------------'
   write(6,*)'       Read WRFVAR namelist options for MADIS datasets'
   write(6,*)' -----------------------------------------------------'
   write(6,*)

!----------------------------------------------------------------------------
! Set default values.  These represent reasonable choices for an hourly data 
! assimilation cycle, using all datasets, for MADIS database, without 
! debugging info.
!----------------------------------------------------------------------------
   madis_debug = 0

   madis_metar_db = 'FSL'
   madis_metar_minbck = -30
   madis_metar_minfwd = 29
   madis_metar_recwin = 4
   madis_metar_all_providers = .TRUE.

   madis_ships_db = 'FSL'
   madis_ships_minbck = -30
   madis_ships_minfwd = 29
   madis_ships_recwin = 4

   madis_gpspw_db = 'FSL'
   madis_gpspw_minbck = -30
   madis_gpspw_minfwd = 29
   madis_gpspw_recwin = 4

   madis_sound_db = 'FSL'
   madis_sound_minbck = -60
   madis_sound_minfwd = 60
   madis_sound_recwin = 3

   madis_profiler_db = 'FSL'
   madis_profiler_minbck = -30
   madis_profiler_minfwd = 29
   madis_profiler_recwin = 4
   madis_profiler_all_providers = .TRUE.

   madis_airep_db = 'FSL'
   madis_airep_minbck = -30
   madis_airep_minfwd = 29
   madis_airep_recwin = 4

   madis_geoamv_db = 'FSL'
   madis_geoamv_minbck = -150
   madis_geoamv_minfwd = 30
   madis_geoamv_recwin = 4
   madis_geoamv_all_providers = .TRUE.
   madis_geoamv_wv = .TRUE.
   madis_geoamv_ir = .TRUE.
   madis_geoamv_vis = .FALSE.
   madis_geoamv_s10 = .FALSE.
   madis_geoamv_s11 = .FALSE.
   
!----------------------------------------------------------------------------
!  Open namelist file.
!----------------------------------------------------------------------------
   namelist_file = 'namelist.3dvar_madis'
   write(6,*)' WRFVAR MADIS datasets namelist options used are in: ', &
             namelist_file
   iost = 0
   i = 0

   write(6,*) ' Opening WRFVAR MADIS namelist input file...'

   open(file=namelist_file,unit=lun,status='old', &
        access='sequential',form='formatted',action='read', &
        err=8500,iostat=iost)

!----------------------------------------------------------------------------
!  Read namelist and close.
!----------------------------------------------------------------------------
   iost = 0
   i = i + 1
   read(unit=lun,nml=madis_general,err=8500,iostat=iost)
   i = i + 1
   read(unit=lun,nml=madis_metar,err=8500,iostat=iost)
   i = i + 1
   read(unit=lun,nml=madis_ships,err=8500,iostat=iost)
   i = i + 1
   read(unit=lun,nml=madis_gpspw,err=8500,iostat=iost)
   i = i + 1
   read(unit=lun,nml=madis_sound,err=8500,iostat=iost)
   i = i + 1
   read(unit=lun,nml=madis_profiler,err=8500,iostat=iost)
   i = i + 1
   read(unit=lun,nml=madis_airep,err=8500,iostat=iost)
   i = i + 1
   read(unit=lun,nml=madis_geoamv,err=8500,iostat=iost)
   close(lun)

   write(6,*) ' WRFVAR MADIS namelist input file successfully read.'

!------------------------------------------------------------------------------
!  The input will be validated when the user's choices are employed in the
!  da_setup_obs_structures_madis routine, and the MADIS library routines that
!  it calls.  Therefore, don't duplicate the validation here.
!------------------------------------------------------------------------------

!----------------------------------------------------------------------------
!  Print namelist.
!----------------------------------------------------------------------------

   if (print_detail_obs) then

      write(6,*) ' Printing contents of WRFVAR MADIS namelist input file...'

      write(6,*)
      write(6,*)'madis_debug                  = ',madis_debug
      write(6,*)
      write(6,*)'madis_metar_db               = ',madis_metar_db 
      write(6,*)'madis_metar_minbck           = ',madis_metar_minbck 
      write(6,*)'madis_metar_minfwd           = ',madis_metar_minfwd 
      write(6,*)'madis_metar_recwin           = ',madis_metar_recwin 
      write(6,*)'madis_metar_all_providers    = ',madis_metar_all_providers 
      write(6,*)'madis_metar_provider_list    = ',trim(madis_metar_provider_list)
      write(6,*)
      write(6,*)'madis_sound_db               = ',madis_sound_db 
      write(6,*)'madis_sound_minbck           = ',madis_sound_minbck 
      write(6,*)'madis_sound_minfwd           = ',madis_sound_minfwd 
      write(6,*)'madis_sound_recwin           = ',madis_sound_recwin 
      write(6,*)
      write(6,*)'madis_profiler_db            = ',madis_profiler_db 
      write(6,*)'madis_profiler_minbck        = ',madis_profiler_minbck 
      write(6,*)'madis_profiler_minfwd        = ',madis_profiler_minfwd 
      write(6,*)'madis_profiler_recwin        = ',madis_profiler_recwin 
      write(6,*)'madis_profiler_all_providers = ',madis_profiler_all_providers 
      write(6,*)'madis_profiler_provider_list = ',trim(madis_profiler_provider_list)
      write(6,*)
      write(6,*)'madis_airep_db               = ',madis_airep_db 
      write(6,*)'madis_airep_minbck           = ',madis_airep_minbck 
      write(6,*)'madis_airep_minfwd           = ',madis_airep_minfwd 
      write(6,*)'madis_airep_recwin           = ',madis_airep_recwin 
      write(6,*)
      write(6,*)'madis_geoamv_db              = ',madis_geoamv_db 
      write(6,*)'madis_geoamv_minbck          = ',madis_geoamv_minbck 
      write(6,*)'madis_geoamv_minfwd          = ',madis_geoamv_minfwd 
      write(6,*)'madis_geoamv_recwin          = ',madis_geoamv_recwin 
      write(6,*)'madis_geoamv_all_providers   = ',madis_geoamv_all_providers 
      write(6,*)'madis_geoamv_provider_list   = ',trim(madis_geoamv_provider_list)
      write(6,*)'madis_geoamv_wv              = ',madis_geoamv_wv 
      write(6,*)'madis_geoamv_ir              = ',madis_geoamv_ir 
      write(6,*)'madis_geoamv_vis             = ',madis_geoamv_vis 
      write(6,*)'madis_geoamv_s10             = ',madis_geoamv_s10 
      write(6,*)'madis_geoamv_s11             = ',madis_geoamv_s11 
 
   endif
   
   write(6,*)

! Allocate the local memory needed to read in the MADIS arrays that are
! dimensioned by station.

   write(6,*) ' Allocating memory for MADIS arrays...'

   allocate (stanam(max_sta))
   allocate (provdr(max_sta))
   allocate (timeob(max_sta))
   allocate (wmoid(max_sta))
   allocate (lat(max_sta))
   allocate (lon(max_sta))
   allocate (elev(max_sta))
   allocate (pw(max_sta))
   allocate (slp(max_sta))
   allocate (keep(max_sta))
   allocate (nlevels(max_sta))

! Initialize the dataset ob counts.

   do i = 1, num_ob_indexes
      nlocal(i) = 0
      ntotal(i) = 0
   enddo

   write(6,*) ' Memory allocated, obcounts set to zero'

! Initialize the MADIS subsystems to be used, using the database
! selected for each subsystem by the user, and with error messages 
! echoed to standard output.  For subsystems that have optional
! data provider selection files, read those in as required, and
! set the subset of data providers to be used.

   pvdriostat = 0                  ! Indicates I/O errors on provider files.

   if(use_metarobs .or. use_shipsobs .or. use_gpspwobs)then

! Because these three WRF datasets map into only one MADIS subsystem, and
! the user has database & time window control over each of them independently,
! we can only initialize one of them here.  We'll initialize the first one
! that will be used below when we read in the data.  (This is because something
! has to be initialized so we can get proper error message handling inside
! the MADIS routines.)

      if(use_metarobs)then

         CALL MINIT('SFC',madis_metar_db,.true.,istatus)
         if(istatus /= success_p)stop
         CALL MSETSFCPVDR('ALL-SFC',.false.,istatus)
         if(istatus /= success_p)stop

         if(.not. madis_metar_all_providers)then

            if(print_detail_obs)then
               write(6,*)
               write(6,*)'Selecting individual METAR providers from ',&
                         trim(madis_metar_provider_list)
            endif
            if(istatus /= success_p)stop

            pvdriotype = 'Open'
            pvdrfile = madis_metar_provider_list
            open(unit=lun,file=trim(pvdrfile),status='OLD', &
                 iostat=pvdriostat,err=8000)

            pvdriotype = 'Read'

            do i = 1, max_provider
               read(lun,1,err=8000,iostat=pvdriostat,end=2)dpname
 1             format(a)
               if(.not. (dpname(1:1) == '#' .or. len(trim(dpname)) == 0 .or. &
                         dpname(1:8) == 'MARITIME'))then
                  if(print_detail_obs)write(6,*)'  Selecting:  ',trim(dpname)
                  CALL MSETSFCPVDR(trim(dpname),.true.,istatus)
                  if(istatus /= success_p)stop
               endif
            enddo

 2          pvdriostat = 0
            close(lun)

         else

! Select all of the surface datasets other than maritime.

            CALL MSETSFCPVDR('ALL-MTR',.true.,istatus)
            if(istatus /= success_p)stop
            CALL MSETSFCPVDR('SAO',.true.,istatus)
            if(istatus /= success_p)stop
            CALL MSETSFCPVDR('ALL-MESO',.true.,istatus)
            if(istatus /= success_p)stop
            CALL MSETSFCPVDR('COOP',.true.,istatus)
            if(istatus /= success_p)stop

         endif

      else if(use_shipsobs)then

         CALL MINIT('SFC',madis_ships_db,.true.,istatus)
         if(istatus /= success_p)stop

      else if(use_gpspwobs)then

         CALL MINIT('SFC',madis_gpspw_db,.true.,istatus)
         if(istatus /= success_p)stop

      endif

   endif

   if(use_soundobs)then

      CALL MINIT('RAOB',madis_sound_db,.true.,istatus)
      if(istatus /= success_p)stop

   endif

! ***** Remember to add use_xxxobs logic for profilers once that's added.

   if(use_soundobs)then   ! ***** change to use_profilerobs...

      map_init = .false.
      npn_init = .false.

      if(.not. madis_profiler_all_providers)then

         if(print_detail_obs)then
            write(6,*)
            write(6,*)'Selecting individual PROFILER providers from ',&
                         trim(madis_profiler_provider_list)
         endif

         pvdriotype = 'Open'
         pvdrfile = madis_profiler_provider_list
         open(unit=lun,file=trim(pvdrfile),status='OLD', &
              iostat=pvdriostat,err=8000)

         pvdriotype = 'Read'

         do i = 1, max_provider

! Because the WRF profiler dataset maps into two MADIS subsystems (NPN and MAP)
! we need to make sure that we initialize each subsystem as it's encountered
! in the user's list.
               
            read(lun,1,err=8000,iostat=pvdriostat,end=3)dpname
            if(.not. (dpname(1:1) == '#' .or. len(trim(dpname)) == 0))then

               if(print_detail_obs)write(6,*)'  Selecting:  ',trim(dpname)

               if(trim(dpname) == 'NOAA')then

                  CALL MINIT('NPN',madis_profiler_db,.true.,istatus)
                  if(istatus /= success_p)stop
                  npn_init = .true.

               else if(trim(dpname) == 'ALL-PROF')then

                  CALL MINIT('NPN',madis_profiler_db,.true.,istatus)
                  if(istatus /= success_p)stop
                  npn_init = .true.
                  CALL MINIT('MAP','FSL',.true.,istatus)   ! No AWIPS database for MAP.
                  if(istatus /= success_p)then

! We'll be forgiving if the AWIPS profiler database has been selected, and just
! set things so no MAP data will be read.  If MADIS (FSL) has been selected, 
! however, this is obviously an error on the user's part, and he/she should fix it.
                  
                     if(madis_profiler_db(1:3).eq.'FSL')stop
                     map_init = .false.
                  else
                     map_init = .true.
                  endif

               else if(trim(dpname) == 'ALL-MAP')then

                  CALL MINIT('MAP','FSL',.true.,istatus)   
                  if(istatus /= success_p)then
                     if(madis_profiler_db(1:3).eq.'FSL')stop
                     map_init = .false.
                  else
                     map_init = .true.
                  endif

! ***** Get rid of the NO-PROF else once profiler data structures have
! been added.

               else if(trim(dpname) == 'NO-PROF')then

                  continue	

               else

! An individual MAP provider is being selected.  Make sure we're initialized
! for MAP, then add in the provider.

                  if(.not. map_init)then
                     CALL MINIT('MAP','FSL',.true.,istatus)   
                     if(istatus /= success_p)then
                        if(madis_profiler_db(1:3).eq.'FSL')stop
                        map_init = .false.
                     else
                        map_init = .true.
                        CALL MSETMAPPVDR('ALL-MAP',.false.,istatus)
                        if(istatus /= success_p)stop
                     endif
                  endif

                  if(map_init)then
                     CALL MSETMAPPVDR(trim(dpname),.true.,istatus)
                     if(istatus /= success_p)stop
                  endif

               endif

            endif

         enddo

 3       pvdriostat = 0
         close(lun)

      else

! "All profilers" consists of both the NPN and the MAP subsystems.  We'll be
! forgiving if an AWIPS database user naively included MAP (by not specifically
! setting the providers).  A user of the MADIS database, however, should know
! better -- we'll stop with a fatal exit.

         CALL MINIT('NPN',madis_profiler_db,.true.,istatus)
         if(istatus /= success_p)stop
         npn_init = .true.
         CALL MINIT('MAP','FSL',.true.,istatus) ! No AWIPS database for MAP.
         if(istatus /= success_p)then
            if(madis_profiler_db(1:3).eq.'FSL')stop
            map_init = .false.
         else
            map_init = .true.
         endif

      endif

   endif

   if(use_airepobs)then

      CALL MINIT('ACARS',madis_airep_db,.true.,istatus)
      if(istatus /= success_p)stop

   endif

   if(use_geoamvobs)then

      CALL MINIT('SATWND',madis_geoamv_db,.true.,istatus)
      if(istatus /= success_p)stop

      if(.not. madis_geoamv_all_providers)then

         if(print_detail_obs)then
            write(6,*)
            write(6,*)'Selecting individual SATOB providers from ',&
                       trim(madis_geoamv_provider_list)
         endif
         CALL MSETSATWNDPVDR('ALL-GOES-O',.false.,istatus)
         if(istatus /= success_p)stop

         pvdriotype = 'Open'
         pvdrfile = madis_geoamv_provider_list
         open(unit=lun,file=trim(pvdrfile),status='OLD', &
              iostat=pvdriostat,err=8000)

         pvdriotype = 'Read'

         do i = 1, max_provider
            read(lun,1,err=8000,iostat=pvdriostat,end=4)dpname
            if(.not. (dpname(1:1) == '#' .or. len(trim(dpname)) == 0))then
               if(print_detail_obs)write(6,*)'  Selecting:  ',trim(dpname)
               CALL MSETSATWNDPVDR(trim(dpname),.true.,istatus)
               if(istatus /= success_p)stop
            endif
         enddo

 4       pvdriostat = 0
         close(lun)

      endif

! Set the number and types of SATW variables to read.

      nsatw_vartype = 0

      if(madis_geoamv_ir.and.madis_geoamv_wv.and.madis_geoamv_vis.and. &
         madis_geoamv_s10.and.madis_geoamv_s11)then

         nsatw_vartype = 1
         satw_var_len(1) = 2
         satw_var(1,1) = 'DD'
         satw_var(2,1) = 'FF'

      else

         if(madis_geoamv_ir)then
            nsatw_vartype = nsatw_vartype + 1
            satw_var_len(nsatw_vartype) = 4
            satw_var(1,nsatw_vartype) = 'DDIR'
            satw_var(2,nsatw_vartype) = 'FFIR'
         endif

         if(madis_geoamv_wv)then
            nsatw_vartype = nsatw_vartype + 1
            satw_var_len(nsatw_vartype) = 4
            satw_var(1,nsatw_vartype) = 'DDWV'
            satw_var(2,nsatw_vartype) = 'FFWV'
         endif

         if(madis_geoamv_vis)then
            nsatw_vartype = nsatw_vartype + 1
            satw_var_len(nsatw_vartype) = 5
            satw_var(1,nsatw_vartype) = 'DDVIS'
            satw_var(2,nsatw_vartype) = 'FFVIS'
         endif

         if(madis_geoamv_s10)then
            nsatw_vartype = nsatw_vartype + 1
            satw_var_len(nsatw_vartype) = 5
            satw_var(1,nsatw_vartype) = 'DDS10'
            satw_var(2,nsatw_vartype) = 'FFS10'
         endif

         if(madis_geoamv_s11)then
            nsatw_vartype = nsatw_vartype + 1
            satw_var_len(nsatw_vartype) = 5
            satw_var(1,nsatw_vartype) = 'DDS11'
            satw_var(2,nsatw_vartype) = 'FFS11'
         endif

      endif	

   endif

! Select the QC level to be returned to "max possible QC level for each
! variable".

   CALL MSETQC(qcall,istatus)
   if(istatus /= success_p)stop

! Convert the user's desired time to the format used by MADIS
! (YYYYMMDD_HHMM).

   tstr = analysis_date(1:4)//analysis_date(6:7)//analysis_date(9:10)// &
          '_'//analysis_date(12:13)//analysis_date(15:16)

! ***** This commented-out section is used only by MADIS developers to
!       hardwire the desired time when doing stand-alone testing.
!
!   open(unit=19,file='mfb_time.txt',status='old')
!   read(19,*)tstr
!   close(unit=19)
!
! ***** End of test code section.

! Now convert 'YYYYMMDD_HH00' to 'YYJJJHH00' (JJJ = Julian date).

   CALL MTRNTIM(tstr,1,atime,istatus)
   if(istatus /= success_p)stop

!------------------------------------------------------------------------------
! MADIS surface datasets --> WRF datasets METAR, GPSPW, SHIPS
!------------------------------------------------------------------------------
! If we're not processing any of these WRF datasets, skip to the next dataset.

   if(.not. use_metarobs .and. .not. use_shipsobs .and. .not. use_gpspwobs)go to 150

! Allocate the local memory for the MADIS arrays that have a level dimension
! (only 1 level in this case).

   allocate (p(max_sfc))
   allocate (q(max_sfc))
   allocate (t(max_sfc))
   allocate (z(max_sfc))
   allocate (dd(max_sfc))
   allocate (ff(max_sfc))
   allocate (qca(max_sfc))
   allocate (qcr(max_sfc))
   allocate (qcd(max_sfc))

   if(use_metarobs)then

! Set the time window for this dataset with the user's choices.

      CALL MSETWIN(madis_metar_minbck,madis_metar_minfwd,madis_metar_recwin,istatus)
      if(istatus /= success_p)stop

! Load in the stations that have data in this time window.  If we have any 
! problems reading in the stations we'll skip this dataset.  When we're done
! with all the datasets, if nothing's been read in there's a problem
! and we'll stop with an error.  If a particular dataset isn't there, but
! the others aren't, that's not as fundamental of a problem.

      CALL MSFCSTA(atime,nsta,stanam,wmoid,lat,lon,elev,timeob,provdr,istatus)
      if(istatus /= success_p)go to 50

! Read in the observations, with QC filtering as specified in MSETQC --
! this will put MADIS missing flags in all obs that didn't pass the QC.
! Note that we don't check the status on each variable read.  It's simpler
! to fill in missing flags into each variable before reading them, then
! we can sort this all out below.

      p(1:nsta) = miss_p
      CALL MGETSFC(atime,'P',ntmp,nobs,p,qcd,qca,qcr,istatus)
      q(1:nsta) = miss_p
      CALL MGETSFC(atime,'Q',ntmp,nobs,q,qcd,qca,qcr,istatus)
      t(1:nsta) = miss_p
      CALL MGETSFC(atime,'T',ntmp,nobs,t,qcd,qca,qcr,istatus)
      dd(1:nsta) = miss_p
      CALL MGETSFC(atime,'DD',ntmp,nobs,dd,qcd,qca,qcr,istatus)
      ff(1:nsta) = miss_p
      CALL MGETSFC(atime,'FF',ntmp,nobs,ff,qcd,qca,qcr,istatus)
      slp(1:nsta) = miss_p
      CALL MGETSFC(atime,'SLP',ntmp,nobs,slp,qcd,qca,qcr,istatus)

! Fill in missing flags for the PW variable, copy ELEV to Z
! and fill in 1 level for each station for use by the PROCESS_DATASET 
! routine.  Also, any stations at sea level that have SLP but not P,
! put SLP into P -- while a useful idea in general, this particularly
! makes a difference with maritime obs -- usually at sea level, with
! a lot of stations measuring SLP but not P.

      do i = 1, nsta
         pw(i) = miss_p
         z(i) = elev(i)
         nlevels(i) = 1
         if(p(i) == miss_p)then
            if(elev(i) == 0.0 .and. slp(i) /= miss_p)p(i) = slp(i)
         endif
      enddo

! Determine which ones are inside the user's domain, and then allocate 
! memory for the IV data for this dataset.  Also note that we toss surface obs 
! that don't have P.

      ntmp = 0                  ! Count of records inside domain.

      do i = 1, nsta

! Put LAT/LON into PLATFORM and call a DA subroutine to determine if
! the station is in the domain.  Also check for bad (mesonet) station
! locations -- we have at least one provider who puts these into his
! station table when he doesn't have valid coordinates.
	
	 if(abs(lat(i)).lt.90.and.abs(lon(i)).le.180)then
            platform % info % lat = lat(i)
            platform % info % lon = lon(i)

            CALL da_llxy(platform%info,platform%loc,outside,outside_all)

            if (outside_all .or. p(i) == miss_p)then

                keep(i) = 0         ! 0 = Not in domain or no P.

            else 

		ntotal(metar) = ntotal(metar) + 1
		if(outside)then

                   keep(i) = 0

		else

                   keep(i) = 1         ! 1 = Process.
                   ntmp = ntmp + 1

                   if (global .and. (platform%loc%i < ids.or.platform%loc%i >= ide)) then
                      nlocal(metar) = nlocal(metar) + 2
                   else
                      nlocal(metar) = nlocal(metar) + 1
                   endif

		endif

             endif

	 else

            keep(i) = 0

         endif

      enddo

      if(nlocal(metar) > 0)then

         allocate (iv % metar(1:nlocal(metar)))
   
! Call all land surface data METAR, and tell the sub that there's one level.

         fm = 15
         nlev = 1
         iv%info(metar)%max_lev = 1

! Move the data out of the ingest arrays into the IV structure, 
! including attendant processing that fills in the location within
! the user's domain, etc.

         CALL PROCESS_DATASET(nsta,nlev,fm,keep,lat,lon,elev,timeob, &
                              slp,pw,stanam,p,q,t,dd,ff,z,nlocal,ntotal, &
                              0,0,nlevels,iv,nlocal_out,ntotal_out,sfclev)

! Note that if PROCESS_DATASET detects that there's no useful data for
! a station, it won't put it into IV.  Therefore we'll adjust NLOCAL
! accordingly (FILL_IV_INFO adjusts NTOTAL in the structure where it's
! sent back to the data assimilation code).

         iv%info(metar)%nlocal = nlocal_out

      endif

   endif

 50 if(use_shipsobs)then

! Because this WRF dataset is a subset of the MADIS surface subsystem,
! we have to re-initialize the MADIS subsystem here, with provider selection,
! QC level selection, etc.

      CALL MINIT('SFC',madis_ships_db,.true.,istatus)
      if(istatus /= success_p)stop
      CALL MSETSFCPVDR('ALL-SFC',.false.,istatus)
      if(istatus /= success_p)stop
      CALL MSETSFCPVDR('MARITIME',.true.,istatus)
      if(istatus /= success_p)stop
      CALL MSETQC(qcall,istatus)
      if(istatus /= success_p)stop

! Set the time window for this dataset with the user's choices.

      CALL MSETWIN(madis_ships_minbck,madis_ships_minfwd,madis_ships_recwin,istatus)
      if(istatus /= success_p)stop

! Load in the stations that have data in this time window.

      CALL MSFCSTA(atime,nsta,stanam,wmoid,lat,lon,elev,timeob,provdr,istatus)
      if(istatus /= success_p)go to 75

! Read in the observations.

      p(1:nsta) = miss_p
      CALL MGETSFC(atime,'P',ntmp,nobs,p,qcd,qca,qcr,istatus)
      q(1:nsta) = miss_p
      CALL MGETSFC(atime,'Q',ntmp,nobs,q,qcd,qca,qcr,istatus)
      t(1:nsta) = miss_p
      CALL MGETSFC(atime,'T',ntmp,nobs,t,qcd,qca,qcr,istatus)
      dd(1:nsta) = miss_p
      CALL MGETSFC(atime,'DD',ntmp,nobs,dd,qcd,qca,qcr,istatus)
      ff(1:nsta) = miss_p
      CALL MGETSFC(atime,'FF',ntmp,nobs,ff,qcd,qca,qcr,istatus)
      slp(1:nsta) = miss_p
      CALL MGETSFC(atime,'SLP',ntmp,nobs,slp,qcd,qca,qcr,istatus)

! Fill in missing flags for the PW variable, copy ELEV to Z
! and fill in 1 level for each station for use by the PROCESS_DATASET 
! routine.  Also, any stations at sea level that have SLP but not P,
! put SLP into P -- while a useful idea in general, this particularly
! makes a difference with maritime obs -- usually at sea level, with
! a lot of stations measuring SLP but not P.

      do i = 1, nsta
         pw(i) = miss_p
         z(i) = elev(i)
         nlevels(i) = 1
         if(p(i) == miss_p)then
            if(elev(i) == 0.0 .and. slp(i) /= miss_p)p(i) = slp(i)
         endif
      enddo

! Determine which ones are inside the user's domain, and then allocate 
! memory for the IV data for this dataset.  Also note that we toss ships obs 
! that don't have P.

      ntmp = 0                  ! Count of records inside domain.

      do i = 1, nsta

! Put LAT/LON into PLATFORM and call a DA subroutine to determine if
! the station is in the domain.

	 if(abs(lat(i)).lt.90.and.abs(lon(i)).le.180)then
            platform % info % lat = lat(i)
            platform % info % lon = lon(i)
            CALL da_llxy(platform%info,platform%loc,outside,outside_all)

            if (outside_all .or. p(i) == miss_p)then
               keep(i) = 0
            else 
		ntotal(ships) = ntotal(ships) + 1
		if(outside)then
                   keep(i) = 0
		else
                   keep(i) = 1         
                   ntmp = ntmp + 1
                   if (global .and. (platform%loc%i < ids .or. platform%loc%i >= ide)) then
                      nlocal(ships) = nlocal(ships) + 2
                   else
                      nlocal(ships) = nlocal(ships) + 1
                   endif
                endif
            endif
         else
            keep(i) = 0
         endif

      enddo

      if(nlocal(ships) > 0)then

         allocate (iv % ships(1:nlocal(ships)))
         fm = 13
         iv%info(ships)%max_lev = 1
         CALL PROCESS_DATASET(nsta,nlev,fm,keep,lat,lon,elev,timeob, &
                              slp,pw,stanam,p,q,t,dd,ff,z,nlocal,ntotal, &
                              0,0,nlevels,iv,nlocal_out,ntotal_out,sfclev)
         iv%info(ships)%nlocal = nlocal_out
      endif

   endif

 75 if(use_gpspwobs)then

! Reinitialize the MADIS surface subsystem for GPSPW.

      CALL MINIT('SFC',madis_gpspw_db,.true.,istatus)
      if(istatus /= success_p)stop
      CALL MSETSFCPVDR('ALL-SFC',.false.,istatus)
      if(istatus /= success_p)stop
      CALL MSETSFCPVDR('GPSMET',.true.,istatus)
      if(istatus /= success_p)stop
      CALL MSETQC(qcall,istatus)
      if(istatus /= success_p)stop

! Set the time window for this dataset with the user's choices.

      CALL MSETWIN(madis_gpspw_minbck,madis_gpspw_minfwd,madis_gpspw_recwin,istatus)
      if(istatus /= success_p)stop

! Load in the stations that have data in this time window.

      CALL MSFCSTA(atime,nsta,stanam,wmoid,lat,lon,elev,timeob,provdr,istatus)
      if(istatus /= success_p)go to 100

! Read in the observations.  Note that we put missing flags into DD/FF so
! that the DD/FF->U/V conversion won't be attempted in PROCESS_DATASETS.

      dd(1:nsta) = miss_p
      ff(1:nsta) = miss_p

      pw(1:nsta) = miss_p
      CALL MGETSFC(atime,'PWV',ntmp,nobs,pw,qcd,qca,qcr,istatus)
      slp(1:nsta) = miss_p
      CALL MGETSFC(atime,'SLP',ntmp,nobs,slp,qcd,qca,qcr,istatus)

! Copy ELEV to Z and fill in 1 level for each station for use by the 
! PROCESS_DATASET routine.  

      do i = 1, nsta
         z(i) = elev(i)
         nlevels(i) = 1
      enddo

! Determine which ones are inside the user's domain and have PW, and then 
! allocate memory for the IV data for this dataset.

      ntmp = 0

      do i = 1, nsta

	 if(abs(lat(i)).lt.90.and.abs(lon(i)).le.180)then
            platform % info % lat = lat(i)
            platform % info % lon = lon(i)
            CALL da_llxy(platform%info,platform%loc,outside,outside_all)
            
            if (outside_all .or. pw(i) == miss_p)then
               keep(i) = 0
            else 
		ntotal(gpspw) = ntotal(gpspw) + 1
		if(outside)then
                   keep(i) = 0
		else
                   keep(i) = 1         
                   pw(i) = pw(i) * 100.0 ! Convert from m to cm.
                   ntmp = ntmp + 1
                   if (global .and. (platform%loc%i < ids .or. platform%loc%i >= ide)) then
                      nlocal(gpspw) = nlocal(gpspw) + 2
                   else
                      nlocal(gpspw) = nlocal(gpspw) + 1
                   endif
                endif
            endif
         else
            keep = 0
         endif

      enddo

      if(nlocal(gpspw) > 0)then

         allocate (iv % gpspw(1:nlocal(gpspw)))
         fm = 111
         iv%info(gpspw)%max_lev = 1
         CALL PROCESS_DATASET(nsta,nlev,fm,keep,lat,lon,elev,timeob, &
                              slp,pw,stanam,p,q,t,dd,ff,z,nlocal,ntotal, &
                              0,0,nlevels,iv,nlocal_out,ntotal_out,sfclev)
         iv%info(gpspw)%nlocal = nlocal_out
      endif

   endif

! Finish up with the MADIS surface datasets by deallocating the 
! levels-dimensioned arrays.

 100  deallocate (p)
   deallocate (q)
   deallocate (t)
   deallocate (z)
   deallocate (dd)
   deallocate (ff)
   deallocate (qca)
   deallocate (qcr)
   deallocate (qcd)

!------------------------------------------------------------------------------
! MADIS ACARS dataset --> WRF AIREP dataset
!------------------------------------------------------------------------------
! If we're not using this WRF dataset, skip to the next one.

 150  if(.not. use_airepobs)go to 250

! Set the time window for this dataset with the user's choices.

   CALL MSETWIN(madis_airep_minbck,madis_airep_minfwd,madis_airep_recwin,istatus)
   if(istatus /= success_p)stop

! Load in the stations with data for this time window.

   CALL MACARSSTA(atime,nsta,stanam,timeob,istatus)
   if(istatus /= success_p)go to 200

! Allocate the local memory for the MADIS arrays that have a level dimension.

   allocate (p(nsta))
   allocate (q(nsta))
   allocate (t(nsta))
   allocate (z(nsta))
   allocate (dd(nsta))
   allocate (ff(nsta))
   allocate (qca(nsta))
   allocate (qcr(nsta))
   allocate (qcd(nsta))

! Read in the observations.  Note that unlike the other MADIS datasets,
! the lat/lon are read in as obs, not station info.

   p(1:nsta) = miss_p
   CALL MGETACARS(atime,'P',ntmp,nobs,p,qcd,qca,qcr,istatus)
   t(1:nsta) = miss_p
   CALL MGETACARS(atime,'T',ntmp,nobs,t,qcd,qca,qcr,istatus)
   dd(1:nsta) = miss_p
   CALL MGETACARS(atime,'DD',ntmp,nobs,dd,qcd,qca,qcr,istatus)
   ff(1:nsta) = miss_p
   CALL MGETACARS(atime,'FF',ntmp,nobs,ff,qcd,qca,qcr,istatus)
   lat(1:nsta) = miss_p
   CALL MGETACARS(atime,'LAT',ntmp,nobs,lat,qcd,qca,qcr,istatus)
   lon(1:nsta) = miss_p
   CALL MGETACARS(atime,'LON',ntmp,nobs,lon,qcd,qca,qcr,istatus)
   z(1:nsta) = miss_p
   CALL MGETACARS(atime,'HT',ntmp,nobs,z,qcd,qca,qcr,istatus)

! Fill in missing flags for the PW, SLP and ELEV variables, and fill in 
! 1 level for each station for use by the PROCESS_DATASET routine.

   do i = 1, nsta
      pw(i) = miss_p
      slp(i) = miss_p
      elev(i) = miss_p
      nlevels(i) = 1
   enddo

! Determine which ones are inside the user's domain, and then allocate 
! memory for the IV data for this dataset.  

   ntmp = 0                      ! Count of records inside domain.

   do i = 1, nsta

      if (abs(lat(i)).lt.90.and.abs(lon(i)).le.180) then 
         platform % info % lat = lat(i)
         platform % info % lon = lon(i)
         CALL da_llxy(platform%info,platform%loc,outside,outside_all)

         if (outside_all) then

            keep(i) = 0         ! 0 = Not in domain.

         else 

            ntotal(airep) = ntotal(airep) + 1
            if (outside) then
               keep(i) = 0
	    else	
               keep(i) = 1         ! 1 = Process.
               ntmp = ntmp + 1
               if (global .and. (platform%loc%i < ids .or. platform%loc%i >= ide)) then
                  nlocal(airep) = nlocal(airep) + 2
               else
                  nlocal(airep) = nlocal(airep) + 1
               endif
            endif

         endif

      else

         keep(i) = 0

      endif

   enddo

   if(nlocal(airep) > 0)then
      allocate (iv % airep(1:nlocal(airep)))
   endif
   
   fm = 96
   nlev = 1
   iv%info(airep)%max_lev = 1

! Move the data out of the ingest arrays into the IV structure.

   CALL PROCESS_DATASET(nsta,nlev,fm,keep,lat,lon,elev,timeob, &
                        slp,pw,stanam,p,q,t,dd,ff,z,nlocal,ntotal, &
                        0,0,nlevels,iv,nlocal_out,ntotal_out,sfclev)

   iv%info(airep)%nlocal = nlocal_out
   iv%info(airep)%plocal(1) = nlocal_out
   iv%info(airep)%ptotal(1) = ntotal(airep)
   iv%info(airep)%ntotal = ntotal(airep)


! Finish up with the MADIS ACARS datasets by deallocating the 
! levels-dimensioned arrays.

 200  deallocate (p)
   deallocate (q)
   deallocate (t)
   deallocate (z)
   deallocate (dd)
   deallocate (ff)
   deallocate (qca)
   deallocate (qcr)
   deallocate (qcd)

!------------------------------------------------------------------------------
! MADIS RAOB/NPN/MAP datasets --> WRF SOUND dataset
!------------------------------------------------------------------------------

 250  if(.not. use_soundobs)go to 300

! ***** Split off the RAOB dataset from this section once profiler data
! structures have been added.

! ***** Remember to add the use_xxxobs logic for profilers.

! This MADIS-->WRF dataset transition is more complicated than the other
! ones we've done above, because we need to map multiple MADIS datasets
! into one WRF dataset, and we need to know how many reports we're going
! to have in the domain in order to allocate the IV memory.  Therefore,
! we'll set the time window for each MADIS dataset, read its station info,
! and count up the reports inside the domain over all the MADIS datasets.
! Once that's been done, then the same tasks will be done as for the
! simpler datasets above.

! RAOB

! Set the time window for this dataset with the user's choices.

   CALL MSETWIN(madis_sound_minbck,madis_sound_minfwd,madis_sound_recwin,istatus)
   if(istatus /= success_p)stop
   ntmp_r = 0
   CALL MRAOBSTA(atime,nsta,stanam,wmoid,lat,lon,elev,timeob,istatus)
   if(istatus == success_p)then
      do i = 1, nsta
	 if(abs(lat(i)).lt.90.and.abs(lon(i)).le.180)then
            platform % info % lat = lat(i)
            platform % info % lon = lon(i)
            CALL da_llxy(platform%info,platform%loc,outside,outside_all)
            if (outside_all) then
               keep_r(i) = 0    ! 0 = Not in domain.
            else
	       ntotal(sound) = ntotal(sound) + 1
	       if(outside)then
                   keep(i) = 0
	       else
                   keep_r(i) = 1    ! 1 = Process.
                   ntmp_r = ntmp_r + 1
                   if (global .and. (platform%loc%i < ids .or. platform%loc%i >= ide)) then
                      nlocal(sound) = nlocal(sound) + 2
                   else
                      nlocal(sound) = nlocal(sound) + 1
                   endif
               endif
            endif
         else
            keep_r(i) = 0
         endif

      enddo
   endif

! Keep track of nlocal and notal for use by PROCESS_DATASET when it needs to
! know these values over several calls.

   nl_raob = nlocal(sound)
   nt_raob = ntotal(sound)

! NPN

   write(unit=message(1),fmt='(A,I10)')'ntotal_raob=',ntotal(sound)
   call da_message(message(1:1))
   ntmp_n = 0

   if(npn_init)then

! Set the time window for this dataset with the user's choices.

      CALL MSETWIN(madis_profiler_minbck,madis_profiler_minfwd,madis_profiler_recwin,istatus)
      if(istatus /= success_p)stop
      CALL MNPNSTA(atime,nsta,stanam,wmoid,lat,lon,elev,timeob,istatus)
      if(istatus == success_p)then
         do i = 1, nsta
            if(abs(lat(i)).lt.90.and.abs(lon(i)).le.180)then
               platform % info % lat = lat(i)
               platform % info % lon = lon(i)
               CALL da_llxy(platform%info,platform%loc,outside,outside_all)
               if (outside_all)then
                  keep_n(i) = 0 ! 0 = Not in domain.
               else
	          ntotal(sound) = ntotal(sound) + 1
	          if(outside)then
                     keep(i) = 0
	          else
                     keep_n(i) = 1 ! 1 = Process.
                     ntmp_n = ntmp_n + 1
                     if (global .and. (platform%loc%i < ids .or. platform%loc%i >= ide)) then
                        nlocal(sound) = nlocal(sound) + 2
                     else
                        nlocal(sound) = nlocal(sound) + 1
                     endif
                  endif
               endif
            else
               keep_n(i) = 0
            endif
         enddo
      endif

   endif

! Keep track of nlocal and notal for use by PROCESS_DATASET when it needs to
! know these values over several calls.

   nl_npn = nlocal(sound) - nl_raob
   nt_npn = ntotal(sound) - nt_raob

   write(unit=message(1),fmt='(A,I10)')'ntotal_npn=',ntotal(sound)
   call da_message(message(1:1))
! MAP

   ntmp_m = 0

   if(map_init)then

! Set the time window for this dataset with the user's choices.

      CALL MSETWIN(madis_profiler_minbck,madis_profiler_minfwd,madis_profiler_recwin,istatus)
      if(istatus /= success_p)stop
      CALL MMAPSTA(atime,nsta,stanam,wmoid,lat,lon,elev,timeob,provdr, &
                   istatus)
      if(istatus == success_p)then
         do i = 1, nsta
            if(abs(lat(i)).lt.90.and.abs(lon(i)).le.180)then
               platform % info % lat = lat(i)
               platform % info % lon = lon(i)
               CALL da_llxy(platform%info,platform%loc,outside,outside_all)
               if (outside_all)then
                  keep_m(i) = 0 ! 0 = Not in domain.
               else
	          ntotal(sound) = ntotal(sound) + 1
	          if(outside)then
                     keep(i) = 0
	          else
                     keep_m(i) = 1 ! 1 = Process.
                     ntmp_m = ntmp_m + 1
                     if (global .and. (platform%loc%i < ids .or. platform%loc%i >= ide)) then
                        nlocal(sound) = nlocal(sound) + 2
                      else
                        nlocal(sound) = nlocal(sound) + 1
                      endif
                  endif
               endif
            else
               keep_m(i) = 0
            endif
         enddo
      endif

   endif

! Keep track of nlocal and notal for use by PROCESS_DATASET when it needs to
! know these values over several calls.

   nl_map = nlocal(sound) - nl_raob - nl_npn
   nt_map = ntotal(sound) - nt_raob - nt_npn

   write(unit=message(1),fmt='(A,I10)')'ntotal_map=',ntotal(sound)
   call da_message(message(1:1))
! If we didn't find anything, we're done with this WRF dataset.

   if (nlocal(sound) > 0) then
      allocate (iv % sound(1:nlocal(sound)))
      allocate (iv % sonde_sfc(1:nlocal(sound)))
   endif
  
   fm = 35
   iv%info(sound)%max_lev = max_raoblev
   iv%info(sonde_sfc)%max_lev=1

! Since we have the MAP station info, start with that dataset.
  
   if(ntmp_m /= 0)then

! Allocate the local memory for the MADIS arrays that have a level dimension.

      allocate (p(max_maplev*nsta))
      allocate (q(max_maplev*nsta))
      allocate (t(max_maplev*nsta))
      allocate (z(max_maplev*nsta))
      allocate (dd(max_maplev*nsta))
      allocate (ff(max_maplev*nsta))
      allocate (qca(max_maplev*nsta))
      allocate (qcr(max_maplev*nsta))
      allocate (qcd(max_maplev*nsta))

      nlev = max_maplev

! Read in the observations.

      p(1:nsta*nlev) = miss_p
      CALL MGETMAP(atime,'P',ntmp,nobs,nlevels,p,qcd,qca,qcr,istatus)
      dd(1:nsta*nlev) = miss_p
      CALL MGETMAP(atime,'DD',ntmp,nobs,nlevels,dd,qcd,qca,qcr,istatus)
      ff(1:nsta*nlev) = miss_p
      CALL MGETMAP(atime,'FF',ntmp,nobs,nlevels,ff,qcd,qca,qcr,istatus)
      z(1:nsta*nlev) = miss_p
      CALL MGETMAP(atime,'HT',ntmp,nobs,nlevels,z,qcd,qca,qcr,istatus)

! Fill in missing flags for the variables that aren't in this dataset,
! process it, deallocate the memory.

      slp(1:nsta) = miss_p
      pw(1:nsta) = miss_p
      q(1:nsta*nlev) = miss_p
      t(1:nsta*nlev) = miss_p
      sfclev(1:nsta) = 0

      CALL PROCESS_DATASET(nsta,nlev,fm,keep_m,lat,lon,elev,timeob, &
                           slp,pw,stanam,p,q,t,dd,ff,z,nlocal,ntotal, &
                           0,0,nlevels,iv,nlocal_out,ntotal_out,sfclev)
      deallocate (p)
      deallocate (q)
      deallocate (t)
      deallocate (z)
      deallocate (dd)
      deallocate (ff)
      deallocate (qca)
      deallocate (qcr)
      deallocate (qcd)

   else

      nlocal_out = 0
      ntotal_out = 0

   endif

! Now do the NPN dataset.

   if(ntmp_n /= 0)then

! Allocate the local memory for the MADIS arrays that have a level dimension.

      if(madis_profiler_db(1:3) == 'FSL')then
         nlev = max_npnlev_madis
      else
         nlev = max_npnlev_awips
      endif         

! Set the time window and load in the station info.

      CALL MSETWIN(madis_profiler_minbck,madis_profiler_minfwd,madis_profiler_recwin,istatus)
      if(istatus /= success_p)stop
      CALL MNPNSTA(atime,nsta,stanam,wmoid,lat,lon,elev,timeob,istatus)
      if(istatus /= success_p)stop   ! Can't possibly happen, so stop if it does.

      allocate (p(nlev*nsta))
      allocate (q(nlev*nsta))
      allocate (t(nlev*nsta))
      allocate (z(nlev*nsta))
      allocate (dd(nlev*nsta))
      allocate (ff(nlev*nsta))
      allocate (qca(nlev*nsta))
      allocate (qcr(nlev*nsta))
      allocate (qcd(nlev*nsta))

! Read in the observations.

      p(1:nsta*nlev) = miss_p
      CALL MGETNPN(atime,'P',ntmp,nobs,nlevels,p,qcd,qca,qcr,istatus)
      dd(1:nsta*nlev) = miss_p
      CALL MGETNPN(atime,'DD',ntmp,nobs,nlevels,dd,qcd,qca,qcr,istatus)
      ff(1:nsta*nlev) = miss_p
      CALL MGETNPN(atime,'FF',ntmp,nobs,nlevels,ff,qcd,qca,qcr,istatus)
      z(1:nsta*nlev) = miss_p
      CALL MGETNPN(atime,'HT',ntmp,nobs,nlevels,z,qcd,qca,qcr,istatus)

! Fill in missing flags for the variables that aren't in this dataset,
! process it, deallocate the memory.

      slp(1:nsta) = miss_p
      pw(1:nsta) = miss_p
      q(1:nsta*nlev) = miss_p
      t(1:nsta*nlev) = miss_p
      sfclev(1:nsta) = 0

      CALL PROCESS_DATASET(nsta,nlev,fm,keep_n,lat,lon,elev,timeob, &
                           slp,pw,stanam,p,q,t,dd,ff,z,nlocal,ntotal, &
                           nlocal_out,ntotal_out,nlevels,iv,nlocal_out, &
                           ntotal_out,sfclev)

      deallocate (p)
      deallocate (q)
      deallocate (t)
      deallocate (z)
      deallocate (dd)
      deallocate (ff)
      deallocate (qca)
      deallocate (qcr)
      deallocate (qcd)

   endif

! Do the RAOB dataset.

   if(ntmp_r /= 0)then

      nlev = max_raoblev

! Set the time window and load in the station info.

      CALL MSETWIN(madis_sound_minbck,madis_sound_minfwd,madis_sound_recwin,istatus)
      if(istatus /= success_p)stop
      CALL MRAOBSTA(atime,nsta,stanam,wmoid,lat,lon,elev,timeob,istatus)
      if(istatus /= success_p)stop   ! Can't possibly happen, so stop if it does.

! Allocate the local memory for the MADIS arrays that have a level dimension.

      allocate (p(max_raoblev*nsta))
      allocate (q(max_raoblev*nsta))
      allocate (t(max_raoblev*nsta))
      allocate (z(max_raoblev*nsta))
      allocate (dd(max_raoblev*nsta))
      allocate (ff(max_raoblev*nsta))
      allocate (qca(max_raoblev*nsta))
      allocate (qcr(max_raoblev*nsta))
      allocate (qcd(max_raoblev*nsta))
      allocate (levtype(max_raoblev*nsta))

! Read in the observations.

      p(1:nsta*nlev) = miss_p
      CALL MGETRAOB(atime,'P',ntmp,nobs,nlevels,p,qcd,qca,qcr,istatus)
      q(1:nsta*nlev) = miss_p
      CALL MGETRAOB(atime,'Q',ntmp,nobs,nlevels,q,qcd,qca,qcr,istatus)
      t(1:nsta*nlev) = miss_p
      CALL MGETRAOB(atime,'T',ntmp,nobs,nlevels,t,qcd,qca,qcr,istatus)
      dd(1:nsta*nlev) = miss_p
      CALL MGETRAOB(atime,'DD',ntmp,nobs,nlevels,dd,qcd,qca,qcr,istatus)
      ff(1:nsta*nlev) = miss_p
      CALL MGETRAOB(atime,'FF',ntmp,nobs,nlevels,ff,qcd,qca,qcr,istatus)
      z(1:nsta*nlev) = miss_p
      CALL MGETRAOB(atime,'HT',ntmp,nobs,nlevels,z,qcd,qca,qcr,istatus)
      levtype(1:nsta*nlev) = miss_p
      CALL MGETRAOB(atime,'LEVTYPE',ntmp,nobs,nlevels,levtype,qcd,qca,qcr,istatus)

! Knock out any stations with zero levels of actual data.  Also find the
! surface level for each station and fill in its P into SLP.

      do i = 1, nsta

         slp(i) = miss_p

         if(nlevels(i) == 0)then

            keep_r(i) = 0

         else

            CALL FIND_SFC_LEVEL(i,nlev,nsta,nlevels(i),levtype,p,slp(i),sfclev(i))

         endif

      enddo

! Fill in missing flags for the variables that aren't in this dataset,
! process the data, deallocate the memory.

 260  pw(1:nsta) = miss_p

      CALL PROCESS_DATASET(nsta,nlev,fm,keep_r,lat,lon,elev,timeob, &
                           slp,pw,stanam,p,q,t,dd,ff,z,nlocal,ntotal, &
                           nlocal_out,ntotal_out,nlevels,iv,nlocal_out, &
                           ntotal_out,sfclev)
      iv%info(sound)%nlocal = nlocal_out

      deallocate (p)
      deallocate (q)
      deallocate (t)
      deallocate (z)
      deallocate (dd)
      deallocate (ff)
      deallocate (qca)
      deallocate (qcr)
      deallocate (qcd)
      deallocate (levtype)

   endif

   iv%info(sound)%plocal(1) = nlocal_out
   iv%info(sound)%ptotal(1) = ntotal(sound)
   iv%info(sound)%ntotal = ntotal(sound)
   iv%info(sonde_sfc)%plocal(1) = nlocal_out
   iv%info(sonde_sfc)%ptotal(1) = ntotal(sound)
   iv%info(sonde_sfc)%ntotal = ntotal(sound)
   iv%info(sonde_sfc)%nlocal = nlocal_out

 300  continue

!***** Put the profiler or raob dataset here (at statement 300) when these
!      get split up.

!------------------------------------------------------------------------------
! MADIS SATWND dataset --> WRF GeoAMV dataset
!------------------------------------------------------------------------------
! If we're not using this WRF dataset, skip to the next one.

 450  if(.not. use_geoamvobs)go to 550

! Allocate the local memory for the MADIS arrays that have a level dimension.

   allocate (p(max_satw))
   allocate (q(max_satw))
   allocate (t(max_satw))
   allocate (z(max_satw))
   allocate (dd(max_satw))
   allocate (ff(max_satw))
   allocate (qca(max_satw))
   allocate (qcr(max_satw))
   allocate (qcd(max_satw))

! Set the time window for this dataset with the user's choices.

   CALL MSETWIN(madis_geoamv_minbck,madis_geoamv_minfwd,madis_geoamv_recwin,istatus)
   if(istatus /= success_p)stop

! Load in the stations with data for this hour.  Note that the station call
! covers all variable types.  Therefore, when we read each variable type's
! data separately, we'll have to overlay that into the dd/ff arrays so
! we don't wipe out data read for an earlier variable type.  We'll use the
! not-applicable pw/slp arrays for temporary storage.

   CALL MSATWNDSTA(atime,nsta,provdr,lat,lon,p,timeob,istatus)
   if(istatus /= success_p)go to 500  

! Read in the observations, appending each variable type as we go along.

   dd(1:nsta) = miss_p 
   ff(1:nsta) = miss_p 

   do i = 1, nsatw_vartype

      CALL MGETSATWND(atime,satw_var(1,i)(1:satw_var_len(i)),ntmp,nobs, &
                      pw,qcd,qca,qcr,istatus)
      CALL MGETSATWND(atime,satw_var(2,i)(1:satw_var_len(i)),ntmp,nobs, &
                      slp,qcd,qca,qcr,istatus)

      if(istatus == success_p)then

         do j = 1, ntmp

            if(pw(j) /= miss_p)then

               dd(j) = pw(j)
               ff(j) = slp(j)

            endif

         enddo

      endif

   enddo

! Fill in missing flags for the variables that aren't in this dataset,
! and fill in 1 level for each station for use by the PROCESS_DATASET routine.

   pw(1:nsta) = miss_p
   slp(1:nsta) = miss_p
   elev(1:nsta) = miss_p
   z(1:nsta) = miss_p
   nlevels(1:nsta) = 1

! Determine which ones are inside the user's domain, and then allocate 
! memory for the IV data for this dataset.  Also toss those that don't
! have any data -- we only have to check one of the wind variables,
! if it exists, then all variables will exist.

   ntmp = 0                      ! Count of records inside domain.

   do i = 1, nsta

      if(abs(lat(i)).lt.90.and.abs(lon(i)).le.180)then
         platform % info % lat = lat(i)
         platform % info % lon = lon(i)
         CALL da_llxy(platform%info,platform%loc,outside,outside_all)
         if (outside_all .or. dd(i) == miss_p)then
            keep(i) = 0         ! 0 = Not in domain or no data.
         else 
	    ntotal(geoamv) = ntotal(geoamv) + 1
	    if(outside)then
               keep(i) = 0
	    else
               keep(i) = 1         ! 1 = Process.
               ntmp = ntmp + 1
               if (global .and. (platform%loc%i < ids .or. platform%loc%i >= ide)) then
                  nlocal(geoamv) = nlocal(geoamv) + 2
               else
                  nlocal(geoamv) = nlocal(geoamv) + 1
               endif
            endif
         endif
      else
         keep(i) = 0
      endif

   enddo

   if(nlocal(geoamv) > 0)then

      allocate (iv % geoamv(1:nlocal(geoamv)))
   
      fm = 88
      nlev = 1
      iv%info(geoamv)%max_lev = 1

! Move the data out of the ingest arrays into the IV structure.

      CALL PROCESS_DATASET(nsta,nlev,fm,keep,lat,lon,elev,timeob, &
                           slp,pw,provdr,p,q,t,dd,ff,z,nlocal,ntotal, &
                           0,0,nlevels,iv,nlocal_out,ntotal_out,sfclev)
      iv%info(geoamv)%nlocal = nlocal_out

   endif

! Finish up with the MADIS SATWND datasets by deallocating the 
! levels-dimensioned arrays.

 500  deallocate (p)
   deallocate (q)
   deallocate (t)
   deallocate (z)
   deallocate (dd)
   deallocate (ff)
   deallocate (qca)
   deallocate (qcr)
   deallocate (qcd)

 550  continue              ! Placeholder for the next dataset that's added.

   go to 8000

!----------------------------------------------------------------------------
!  Report error reading namelist.
!----------------------------------------------------------------------------
8500  write(0,*)' ERROR:  ', errmsg(i), ' IOSTAT = ', IOST, &
                ' while reading MADIS namelist'
   stop

!------------------------------------------------------------------------------
! Endgame processing
!------------------------------------------------------------------------------

 8000 if(pvdriostat /= 0)then

! I/O error on opening or reading a provider-selection file.

      write(0,*)'ERROR:  ',pvdriotype,' failure on ',trim(pvdrfile), &
                ' IOSTAT = ',pvdriostat
      stop

! If we didn't get *anything*, output a suitable message and stop.

   else if(iv%info(sound)%nlocal+iv%info(geoamv)%nlocal+iv%info(airep)%nlocal+ &
           iv%info(gpspw)%nlocal+iv%info(metar)%nlocal+iv%info(ships)%nlocal &
           == 0)then
      write(0,*)'ERROR:  No MADIS observations were found.'
!     stop
   endif

! Close the MADIS files and deallocate the memory used for the
! MADIS arrays that are dimensioned only by station.

   CALL MMADISCLOSE
   deallocate (stanam)
   deallocate (provdr)
   deallocate (timeob)
   deallocate (wmoid)
   deallocate (lat)
   deallocate (lon)
   deallocate (elev)
   deallocate (pw)
   deallocate (slp)
   deallocate (keep)
   deallocate (nlevels)

! Output the same summary info as the MM5 ingest routine, but just
! for those datasets we handle.

   if ( print_detail_obs ) then
 
      write(6, fmt='(5x,a,i6,a)') &
           'Read:  ', iv%info(sound)%nlocal, ' SOUND reports,', &
           'Read:  ', iv%info(geoamv)%nlocal, ' Geo AMV reports,', & 
           'Read:  ', iv%info(airep)%nlocal, ' AIREP reports,', &
           'Read:  ', iv%info(gpspw)%nlocal, ' GPSPW/GPSZD reports,', &
           'Read:  ', iv%info(metar)%nlocal, ' METAR reports,', &
           'Read:  ', iv%info(ships)%nlocal, ' SHIP  reports,'

   endif

! Output a dump of all the obs, when desired, for testing purposes.

   if(madis_debug > 0)then

      write(6,*)
      write(6,*)'METAR:  ',iv%info(metar)%nlocal
      write(6,*)
      
      do i = 1, iv%info(metar)%nlocal
         write(6,1000)iv%info(metar)%name(i)(1:8), &
                      iv%info(metar)%date_char(i)(1:16), &
                      iv%info(metar)%platform(i), &
                      iv%info(metar)%lat(1,i),iv%info(metar)%lon(1,i), &
                      iv%info(metar)%elv(i),iv%info(metar)%pstar(i), &
                      iv%info(metar)%levels(i), &
                      iv%info(metar)%slp(i)%inv,iv%info(metar)%slp(i)%qc, &
                      iv%info(metar)%slp(i)%error,iv%info(metar)%pw(i)%inv, &
                      iv%info(metar)%pw(i)%qc,iv%info(metar)%pw(i)%error
 1000    format(a,1x,a,1x,a,f6.2,1x,f7.2,1x,f10.2,1x,f8.2,1x,i3,1x, &
                f10.2,1x,i3,1x,f10.2,1x,f12.4,1x,i3,1x,f10.2)
         write(6,1001)1,' H/Z/U/V',iv%metar(i)%h,missing_data, &
                      iv%info(metar)%zk(1,i), &
                      iv%metar(i)%u%inv,iv%metar(i)%u%qc,iv%metar(i)%u%error, &
                      iv%metar(i)%v%inv,iv%metar(i)%v%qc,iv%metar(i)%v%error
 1001    format(i3,a,3(1x,f11.3,1x,i3,1x,f11.3))
         write(6,1001)1,' P/T/Q  ',iv%metar(i)%p%inv,iv%metar(i)%p%qc, &
                      iv%metar(i)%p%error,iv%metar(i)%t%inv,iv%metar(i)%t%qc, &
                      iv%metar(i)%t%error,iv%metar(i)%q%inv,iv%metar(i)%q%qc, &
                      iv%metar(i)%q%error
      enddo

      write(6,*)
      write(6,*)'SHIPS:  ',iv%info(ships)%nlocal
      write(6,*)
      
      do i = 1, iv%info(ships)%nlocal
         write(6,1000)iv%info(ships)%name(i)(1:8), &
                      iv%info(ships)%date_char(i)(1:16), &
                      iv%info(ships)%platform(i), &
                      iv%info(ships)%lat(1,i),iv%info(ships)%lon(1,i), &
                      iv%info(ships)%elv(i),iv%info(ships)%pstar(i), &
                      iv%info(ships)%levels(i), &
                      iv%info(ships)%slp(i)%inv,iv%info(ships)%slp(i)%qc, &
                      iv%info(ships)%slp(i)%error,iv%info(ships)%pw(i)%inv, &
                      iv%info(ships)%pw(i)%qc,iv%info(ships)%pw(i)%error
         write(6,1001)1,' H/Z/U/V',iv%ships(i)%h,missing_data, &
                      iv%info(ships)%zk(1,i), &
                      iv%ships(i)%u%inv,iv%ships(i)%u%qc,iv%ships(i)%u%error, &
                      iv%ships(i)%v%inv,iv%ships(i)%v%qc,iv%ships(i)%v%error
         write(6,1001)1,' P/T/Q  ',iv%ships(i)%p%inv,iv%ships(i)%p%qc, &
                      iv%ships(i)%p%error,iv%ships(i)%t%inv,iv%ships(i)%t%qc, &
                      iv%ships(i)%t%error,iv%ships(i)%q%inv,iv%ships(i)%q%qc, &
                      iv%ships(i)%q%error
      enddo

      write(6,*)
      write(6,*)'GPSPW:  ',iv%info(gpspw)%nlocal
      write(6,*)
      
      do i = 1, iv%info(gpspw)%nlocal
         write(6,1002)iv%info(gpspw)%name(i)(1:8), &
                      iv%info(gpspw)%date_char(i)(1:16), &
                      iv%info(gpspw)%platform(i), &
                      iv%info(gpspw)%lat(1,i),iv%info(gpspw)%lon(1,i), &
                      iv%info(gpspw)%elv(i),iv%info(gpspw)%pstar(i), &
                      iv%info(gpspw)%levels(i), &
                      iv%info(gpspw)%slp(i)%inv,iv%info(gpspw)%slp(i)%qc, &
                      iv%info(gpspw)%slp(i)%error,iv%info(gpspw)%pw(i)%inv, &
                      iv%info(gpspw)%pw(i)%qc,iv%info(gpspw)%pw(i)%error, &
                      iv%gpspw(i)%tpw%inv,iv%gpspw(i)%tpw%qc, &
                      iv%gpspw(i)%tpw%error
 1002    format(a,1x,a,1x,a,f6.2,1x,f7.2,1x,f7.2,1x,f8.2,1x,i3,1x, &
                f10.2,1x,i3,1x,f10.2,1x,f12.4,1x,i3,1x,f10.2,1x, &
                f12.4,1x,i3,1x,f10.2)
      enddo

      write(6,*)
      write(6,*)'SOUND:  ',iv%info(sound)%nlocal
      write(6,*)
      
      do i = 1, iv%info(sound)%nlocal
         write(6,1000)iv%info(sound)%name(i)(1:8), &
                      iv%info(sound)%date_char(i)(1:16), &
                      iv%info(sound)%platform(i), &
                      iv%info(sound)%lat(1,i),iv%info(sound)%lon(1,i), &
                      iv%info(sound)%elv(i),iv%info(sound)%pstar(i), &
                      iv%info(sound)%levels(i), &
                      iv%info(sound)%slp(i)%inv,iv%info(sound)%slp(i)%qc, &
                      iv%info(sound)%slp(i)%error,iv%info(sound)%pw(i)%inv, &
                      iv%info(sound)%pw(i)%qc,iv%info(sound)%pw(i)%error
         do j = 1, iv%info(sound)%levels(i)
            write(6,1001)j,' H/Z/U/V',iv%sound(i)%h(j),missing_data, &
                         iv%info(sound)%zk(j,i), &
                         iv%sound(i)%u(j)%inv,iv%sound(i)%u(j)%qc, &
                         iv%sound(i)%u(j)%error,iv%sound(i)%v(j)%inv, &
                         iv%sound(i)%v(j)%qc,iv%sound(i)%v(j)%error
            write(6,1001)j,' P/T/Q  ',iv%sound(i)%p(j),missing_data,missing_r, &
                         iv%sound(i)%t(j)%inv, &
                         iv%sound(i)%t(j)%qc,iv%sound(i)%t(j)%error, &
                         iv%sound(i)%q(j)%inv,iv%sound(i)%q(j)%qc, &
                         iv%sound(i)%q(j)%error
         enddo
      enddo

      write(6,*)
      write(6,*)'SONDE_SFC:  ',iv%info(sonde_sfc)%nlocal
      write(6,*)
      
      do i = 1, iv%info(sonde_sfc)%nlocal
         write(6,1000)iv%info(sonde_sfc)%name(i)(1:8), &
                      iv%info(sonde_sfc)%date_char(i)(1:16), &
                      iv%info(sonde_sfc)%platform(i), &
                      iv%info(sonde_sfc)%lat(1,i),iv%info(sonde_sfc)%lon(1,i), &
                      iv%info(sonde_sfc)%elv(i),iv%info(sonde_sfc)%pstar(i), &
                      iv%info(sonde_sfc)%levels(i), &
                      iv%info(sonde_sfc)%slp(i)%inv,iv%info(sonde_sfc)%slp(i)%qc, &
                      iv%info(sonde_sfc)%slp(i)%error,iv%info(sonde_sfc)%pw(i)%inv, &
                      iv%info(sonde_sfc)%pw(i)%qc,iv%info(sonde_sfc)%pw(i)%error
         write(6,1001)1,' H/Z/U/V',iv%sonde_sfc(i)%h,missing_data, &
                      iv%info(sonde_sfc)%zk(1,i), &
                      iv%sonde_sfc(i)%u%inv,iv%sonde_sfc(i)%u%qc,iv%sonde_sfc(i)%u%error, &
                      iv%sonde_sfc(i)%v%inv,iv%sonde_sfc(i)%v%qc,iv%sonde_sfc(i)%v%error
         write(6,1001)1,' P/T/Q  ',iv%sonde_sfc(i)%p%inv,iv%sonde_sfc(i)%p%qc, &
                      iv%sonde_sfc(i)%p%error,iv%sonde_sfc(i)%t%inv,iv%sonde_sfc(i)%t%qc, &
                      iv%sonde_sfc(i)%t%error,iv%sonde_sfc(i)%q%inv,iv%sonde_sfc(i)%q%qc, &
                      iv%sonde_sfc(i)%q%error
      enddo

      write(6,*)
      write(6,*)'AIREP:  ',iv%info(airep)%nlocal
      write(6,*)
      
      do i = 1, iv%info(airep)%nlocal
         write(6,1000)iv%info(airep)%name(i)(1:8), &
                      iv%info(airep)%date_char(i)(1:16), &
                      iv%info(airep)%platform(i), &
                      iv%info(airep)%lat(1,i),iv%info(airep)%lon(1,i), &
                      iv%info(airep)%elv(i),iv%info(airep)%pstar(i), &
                      iv%info(airep)%levels(i), &
                      iv%info(airep)%slp(i)%inv,iv%info(airep)%slp(i)%qc, &
                      iv%info(airep)%slp(i)%error,iv%info(airep)%pw(i)%inv, &
                      iv%info(airep)%pw(i)%qc,iv%info(airep)%pw(i)%error
         write(6,1001)1,' H/Z/U/V',iv%airep(i)%h(1),missing_data, &
                      iv%info(airep)%zk(1,i), &
                      iv%airep(i)%u(1)%inv,iv%airep(i)%u(1)%qc, &
                      iv%airep(i)%u(1)%error,iv%airep(i)%v(1)%inv, &
                      iv%airep(i)%v(1)%qc,iv%airep(i)%v(1)%error
         write(6,1001)1,' P/T    ',iv%airep(i)%p(1),missing_data, &
                      missing_r,iv%airep(i)%t(1)%inv, &
                      iv%airep(i)%t(1)%qc,iv%airep(i)%t(1)%error
      enddo

      write(6,*)
      write(6,*)'GEOAMV:  ',iv%info(geoamv)%nlocal
      write(6,*)
      
      do i = 1, iv%info(geoamv)%nlocal
         write(6,1000)iv%info(geoamv)%name(i)(1:8), &
                      iv%info(geoamv)%date_char(i)(1:16), &
                      iv%info(geoamv)%platform(i), &
                      iv%info(geoamv)%lat(1,i),iv%info(geoamv)%lon(1,i), &
                      iv%info(geoamv)%elv(i),iv%info(geoamv)%pstar(i), &
                      iv%info(geoamv)%levels(i), &
                      iv%info(geoamv)%slp(i)%inv,iv%info(geoamv)%slp(i)%qc, &
                      iv%info(geoamv)%slp(i)%error,iv%info(geoamv)%pw(i)%inv, &
                      iv%info(geoamv)%pw(i)%qc,iv%info(geoamv)%pw(i)%error
         write(6,1001)1,' P/Z/U/V',iv%geoamv(i)%p(1),missing_data, &
                      iv%info(geoamv)%zk(1,i), &
                      iv%geoamv(i)%u(1)%inv,iv%geoamv(i)%u(1)%qc, &
                      iv%geoamv(i)%u(1)%error,iv%geoamv(i)%v(1)%inv, &
                      iv%geoamv(i)%v(1)%qc,iv%geoamv(i)%v(1)%error
      enddo

   endif         ! if(madis_debug > 0)

! Fill in the IV time.

   iv%time = 1


! Copy data out of the IV structure into the OB structure.

   CALL da_fill_obs_structures(iv,ob)

   if(madis_debug > 0)then

! If we're in debugging mode, indicate that we made it to the bottom of
! the routine OK.  Also, if requested, just stop after the obs ingest.

      write(6,*)
      write(6,*)'Successful MADIS ingest.'

      if(madis_debug == 2)stop

   endif   

   return

#else
   call da_error(__FILE__,__LINE__,(/"Needs to be compiled with a MADIS library"/))
#endif

   if (trace_use) CALL da_trace_exit("da_setup_obs_structures_madis")

END SUBROUTINE da_setup_obs_structures_madis

#ifdef MADIS

SUBROUTINE PROCESS_DATASET(nsta,nlev,fm,keep,lat,lon,elev,timeob, &
                           slp,pw,stanam,p,q,t,dd,ff,z, &
                           nlocal,ntotal,nloc_init,ntot_init,nlevels,iv, &
                           nlocal_out,ntotal_out,sfclev)

!------------------------------------------------------------------------------
! PURPOSE: Move obs out of MADIS ingest arrays into the WRF innovation
!          vector structure, including attendant processing that fills in the 
!          location within the user's domain, etc., for one MADIS dataset.
!
! METHOD:  Move obs & metadata from MADIS ingest arrays to MM5-like
!          PLATFORM structure, manipulate PLATFORM with DA_Obs routines,
!          then move the resulting stuff into IV.
!
! HISTORY: 05-21-03  M. Barth         Original version.
!          08-27-04  M. Barth         Removed psfc and added sonde_sfc.
!          02-06-06  M. Barth         Removed arguments no longer in use,
!                                     changed xb to xp, removed DA_Obs_Count.
!          05-30-08  M. Barth         Removed xp, changed DA_ll_to_xy to da_llxy,
!                                     used the new iv%info structures,
!                                     changed ob_type structure to iv_type,
!                                     added nlocal and ntotal arrays and
!                                     nloc_init, ntot_init.  Added
!                                     use of FILL_IV_INFO subroutine.
!          11-28-08 M. Barth          Since I finally got the darn thing to
!                                     compile, I debugged the massive changes
!                                     in version 3.
!                              
!
! PARENT_MODULE: da_setup_structures
!------------------------------------------------------------------------------

   use da_grid_definitions, only : da_ffdduv
   use da_tools, only : da_llxy
   use da_obs, only : da_obs_proc_station
   IMPLICIT NONE
   
! ARGUMENTS:

   INTEGER*4, INTENT(IN)        :: nsta               ! Number of stations
   INTEGER*4, INTENT(IN)        :: nlev               ! Number of levels
   INTEGER, INTENT(IN)          :: fm                 ! GTS FM number for this dataset
   INTEGER*4, INTENT(IN)        :: keep(nsta)         ! 1 = process this station
   REAL*4,    INTENT(IN)        :: lat(nsta)          ! Latitude (deg N)
   REAL*4,    INTENT(IN)        :: lon(nsta)          ! Longitude (deg E)
   REAL*4,    INTENT(IN)        :: elev(nsta)         ! Elevation (m)
   CHARACTER*9, INTENT(IN)      :: timeob(nsta)       ! Time (YYJJJHHMM)
   REAL*4,    INTENT(IN)        :: slp(nsta)          ! Sea level pressure (Pa)
   REAL*4,    INTENT(IN)        :: pw(nsta)           ! Precipitable water vapor (cm)
   CHARACTER*10, INTENT(IN)     :: stanam(nsta)       ! Station ID
   REAL*4,    INTENT(IN)        :: p(nlev,nsta)       ! Pressure (Pa)
   REAL*4,    INTENT(IN)        :: q(nlev,nsta)       ! Mixing ratio (kg/kg)
   REAL*4,    INTENT(IN)        :: t(nlev,nsta)       ! Temperature (K)
   REAL*4,    INTENT(INOUT)     :: dd(nlev,nsta)      ! Wind speed (m/s)
   REAL*4,    INTENT(INOUT)     :: ff(nlev,nsta)      ! Wind direction (0-360 N)
   REAL*4,    INTENT(IN)        :: z(nlev,nsta)       ! Height (m)
   INTEGER*4, INTENT(IN)        :: nlocal(*)          ! Obs counts per dataset, in domain
   INTEGER*4, INTENT(IN)        :: ntotal(*)          ! Obs counts per dataset, in or out
   INTEGER*4, INTENT(IN)        :: nloc_init          ! Initial value for nlocal index
   INTEGER*4, INTENT(IN)        :: ntot_init          ! Initial value for ntotal index
   INTEGER*4, INTENT(IN)        :: nlevels(nsta)      ! Num levels per station
   TYPE (iv_type), INTENT(INOUT) :: iv                ! Innovation vector
   INTEGER*4, INTENT(OUT)       :: nlocal_out         ! Obs count in domain, and with useful data
   INTEGER*4, INTENT(OUT)       :: ntotal_out         ! Obs count, and with useful data
   INTEGER*4, INTENT(IN)        :: sfclev(nsta)       ! Index to raob sfc level per station

! LOCAL:

   TYPE (multi_level_type)      :: platform
   INTEGER                      :: i,j,k,n,istatus,obs_index,ntot,ndup,l
   REAL*4, PARAMETER            :: miss_p    = 999999.0  ! Missing data flag
   CHARACTER*13                 :: tstr
   LOGICAL                      :: outside,cycle_report,anyvar,outside_all
   REAL                         :: xff,xdd,xlon
   INTEGER*4                    :: sfclev_j,m

!------------------------------------------------------------------------------
! The MM5-like PLATFORM structure is used to move data out of the MADIS
! ingest arrays into a structure that can then be manipulated by the DA_Obs
! routines.  Then, the single-station data in PLATFORM is moved to where
! it belongs in the output innovation vector, IV.
!------------------------------------------------------------------------------
! Fill in dataset-specific info.

   SELECT CASE(fm)
      CASE(15)
         platform % info % platform = 'FM-015 METAR'
         obs_index = metar
      CASE(13)
         platform % info % platform = 'FM-013 SHIP '
         obs_index = ships
      CASE(96)
         platform % info % platform = 'FM-096 AIREP'
         obs_index = airep
      CASE(35)
         platform % info % platform = 'FM-035 SOUND'
         obs_index = sound
      CASE(111)
         platform % info % platform = 'FM-111 GPSPW'
         obs_index = gpspw
      CASE(88)
         platform % info % platform = 'FM-088 SATOB'
         obs_index = geoamv
   END SELECT

! Only allocate stuff the first time we're here for a WRF dataset, since multiple
! MADIS datasets may comprise a single WRF dataset (e.g., MAP, NPN, raob).

   if(nloc_init.eq.0)then

     iv%info(obs_index)%nlocal = nlocal(obs_index)
     iv%info(obs_index)%ntotal = ntotal(obs_index)
     if (iv%info(obs_index)%nlocal > 0) then
        allocate (iv%info(obs_index)%name(iv%info(obs_index)%nlocal))     
        allocate (iv%info(obs_index)%platform(iv%info(obs_index)%nlocal)) 
        allocate (iv%info(obs_index)%id(iv%info(obs_index)%nlocal))       
        allocate (iv%info(obs_index)%date_char(iv%info(obs_index)%nlocal))
        allocate (iv%info(obs_index)%levels(iv%info(obs_index)%nlocal))   
        allocate (iv%info(obs_index)%lat(iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))    
        allocate (iv%info(obs_index)%lon(iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))    
        allocate (iv%info(obs_index)%elv(iv%info(obs_index)%nlocal))      
        allocate (iv%info(obs_index)%pstar(iv%info(obs_index)%nlocal))    
        allocate (iv%info(obs_index)%slp(iv%info(obs_index)%nlocal))   
        allocate (iv%info(obs_index)%pw(iv%info(obs_index)%nlocal))    
        allocate (iv%info(obs_index)%x  (kms:kme,iv%info(obs_index)%nlocal))   
        allocate (iv%info(obs_index)%y  (kms:kme,iv%info(obs_index)%nlocal))   
        allocate (iv%info(obs_index)%i  (kms:kme,iv%info(obs_index)%nlocal))   
        allocate (iv%info(obs_index)%j  (kms:kme,iv%info(obs_index)%nlocal))      
        allocate (iv%info(obs_index)%dx (kms:kme,iv%info(obs_index)%nlocal))  
        allocate (iv%info(obs_index)%dxm(kms:kme,iv%info(obs_index)%nlocal)) 
        allocate (iv%info(obs_index)%dy (kms:kme,iv%info(obs_index)%nlocal))  
        allocate (iv%info(obs_index)%dym(kms:kme,iv%info(obs_index)%nlocal)) 
        allocate (iv%info(obs_index)%k  (iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))
        allocate (iv%info(obs_index)%dz (iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal))  
        allocate (iv%info(obs_index)%dzm(iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal)) 
        allocate (iv%info(obs_index)%zk (iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal)) 
        allocate (iv%info(obs_index)%proc_domain(iv%info(obs_index)%max_lev,iv%info(obs_index)%nlocal)) 
        allocate (iv%info(obs_index)%obs_global_index(iv%info(obs_index)%nlocal)) 
        iv%info(obs_index)%proc_domain(:,:)  = .false.
        iv%info(obs_index)%zk(:,:)           = missing_r

        if(fm == 35)then

! Now allocate the sonde_sfc iv%info arrays.

           allocate (iv%info(sonde_sfc)%name(iv%info(obs_index)%nlocal))     
           allocate (iv%info(sonde_sfc)%platform(iv%info(obs_index)%nlocal)) 
           allocate (iv%info(sonde_sfc)%id(iv%info(obs_index)%nlocal))       
           allocate (iv%info(sonde_sfc)%date_char(iv%info(obs_index)%nlocal))
           allocate (iv%info(sonde_sfc)%levels(iv%info(obs_index)%nlocal))   
           allocate (iv%info(sonde_sfc)%lat(iv%info(sonde_sfc)%max_lev,iv%info(obs_index)%nlocal))    
           allocate (iv%info(sonde_sfc)%lon(iv%info(sonde_sfc)%max_lev,iv%info(obs_index)%nlocal))    
           allocate (iv%info(sonde_sfc)%elv(iv%info(obs_index)%nlocal))      
           allocate (iv%info(sonde_sfc)%pstar(iv%info(obs_index)%nlocal))    
           allocate (iv%info(sonde_sfc)%slp(iv%info(obs_index)%nlocal))   
           allocate (iv%info(sonde_sfc)%pw(iv%info(obs_index)%nlocal))    
           allocate (iv%info(sonde_sfc)%x  (kms:kme,iv%info(obs_index)%nlocal))   
           allocate (iv%info(sonde_sfc)%y  (kms:kme,iv%info(obs_index)%nlocal))   
           allocate (iv%info(sonde_sfc)%i  (kms:kme,iv%info(obs_index)%nlocal))   
           allocate (iv%info(sonde_sfc)%j  (kms:kme,iv%info(obs_index)%nlocal))      
           allocate (iv%info(sonde_sfc)%dx (kms:kme,iv%info(obs_index)%nlocal))  
           allocate (iv%info(sonde_sfc)%dxm(kms:kme,iv%info(obs_index)%nlocal)) 
           allocate (iv%info(sonde_sfc)%dy (kms:kme,iv%info(obs_index)%nlocal))  
           allocate (iv%info(sonde_sfc)%dym(kms:kme,iv%info(obs_index)%nlocal)) 
           allocate (iv%info(sonde_sfc)%k  (iv%info(sonde_sfc)%max_lev,iv%info(obs_index)%nlocal))
           allocate (iv%info(sonde_sfc)%dz (iv%info(sonde_sfc)%max_lev,iv%info(obs_index)%nlocal))  
           allocate (iv%info(sonde_sfc)%dzm(iv%info(sonde_sfc)%max_lev,iv%info(obs_index)%nlocal)) 
           allocate (iv%info(sonde_sfc)%zk (iv%info(sonde_sfc)%max_lev,iv%info(obs_index)%nlocal)) 
           allocate (iv%info(sonde_sfc)%proc_domain(iv%info(sonde_sfc)%max_lev,iv%info(obs_index)%nlocal)) 
           allocate (iv%info(sonde_sfc)%obs_global_index(iv%info(obs_index)%nlocal)) 
           iv%info(sonde_sfc)%proc_domain(:,:)  = .false.
           iv%info(sonde_sfc)%zk(:,:)           = missing_r

       endif

     endif

   endif

! Process each record that's in the user's domain.  
!
! Note that the PLATFORM structure includes some variables that
! aren't needed, so we don't deal with them:  RH, TD, SPEED.

   n = nloc_init                 ! Index into iv.
   ntot = ntot_init              ! Value to use for current ntotal within iv-filling
                                 ! loop.  See explanation of duplication logic.

   do i = 1, nsta

      if(keep(i) == 1)then

! Fill in the header-type info for this record.

         platform % info % lat = lat(i)
         platform % info % lon = lon(i)
         platform % info % elv = elev(i)
         platform % info % name = stanam(i)
         platform % info % id = stanam(i)(1:5)
         platform % info % levels = nlevels(i)
         
! Convert time from YYJJJHHMM to YYYYMMDD_HHMM, then put it into
! the info % date_char format of YYYY-MM-DD_HH:MM:SS.

         CALL MTRNTIM(timeob(i),2,tstr,istatus)
         platform % info % date_char = tstr(1:4)//'-'//tstr(5:6)//'-'// &
                                       tstr(7:11)//':'//tstr(12:13)//':00'

! Fill in PW and SLP.  

! ***** Note that I'm not sure if good QC is indicated by qc_good (1) or
!       good_quality (0), or both... I'm guessing qc_good should work
!       since da_read_obs_bufr accepts any value < 4.

         if(pw(i) /= miss_p)then
            platform % loc % pw % inv = pw(i)
            platform % loc % pw % qc = qc_good
         else
            platform % loc % pw % inv = missing_r
            platform % loc % pw % qc = missing_data
         endif
         platform % loc % pw % error = .2    ! Same reference as in GET_OB_ERRORS.

! Fill in SLP.

         if(slp(i) /= miss_p)then
            platform % loc % slp % inv = slp(i)
            platform % loc % slp % qc = qc_good
         else
            platform % loc % slp % inv = missing_r
            platform % loc % slp % qc = missing_data
         endif
         platform % loc % slp % error = 200. ! Same reference as in GET_OB_ERRORS.

! Fill in the data variables that are a function of level.  

         j = 1       ! J = level index for what's gonna get used
         sfclev_j = 0

         do k = 1, nlevels(i)

            if(z(k,i) /= miss_p)then
               platform % each(j) % height = z(k,i)
            else
               platform % each(j) % height = missing_r
            endif

            if(p(k,i) /= miss_p)then
               platform % each(j) % p % inv = p(k,i)
               platform % each(j) % p % qc = qc_good
            else
               platform % each(j) % p % inv = missing_r
               platform % each(j) % p % qc = missing_data
            endif

! If there isn't any P or Z, this level isn't going to make the cut, so 
! skip the rest of its processing.

            if(.not.(p(k,i) == miss_p .and. z(k,i) == miss_p))then

               anyvar = .false.

! If DD and FF exist, convert to U,V with rotation to the user's grid.

               if(dd(k,i) /= miss_p .and. ff(k,i) /= miss_p)then
                  xff = ff(k,i)
                  xdd = dd(k,i)
                  xlon = lon(i)
                  CALL DA_FFDDUV(xff,xdd,platform%each(j)%u%inv, &
                              platform%each(j)%v%inv,xlon,1)
                  platform % each(j) % u % qc = qc_good
                  platform % each(j) % v % qc = qc_good
                  anyvar = .true.
               else
                  platform % each(j) % u % inv = missing_r
                  platform % each(j) % u % qc = missing_data
                  platform % each(j) % v % inv = missing_r
                  platform % each(j) % v % qc = missing_data
               endif

               if(t(k,i) /= miss_p)then
                  platform % each(j) % t % inv = t(k,i)
                  platform % each(j) % t % qc = qc_good
                  anyvar = .true.
               else
                  platform % each(j) % t % inv = missing_r
                  platform % each(j) % t % qc = missing_data
               endif

               if(q(k,i) /= miss_p)then
                  platform % each(j) % q % inv = q(k,i)
                  platform % each(j) % q % qc = qc_good
                  anyvar = .true.
               else
                  platform % each(j) % q % inv = missing_r
                  platform % each(j) % q % qc = missing_data
               endif

! If we don't have any variables for profilers, punt.  (For the
! other datasets, we can't be here without having at least P or Z,
! which for the non-profiler datasets, must be actual observations.
! For profilers, the P/Z variables are only coordinates of the
! observations, and aren't really observations.)

	       if(anyvar .or. fm /= 35)then    !***** change 35 when profilers defined

! Fill in the observation errors for this level.

                  CALL GET_OB_ERRORS(platform%each(j))

! Compute the (x,y,z) and increment the counter of levels that are used.

                  CALL da_llxy(platform%info,platform%loc,outside,outside_all)
                                   
                  if(fm == 35 .and. k == sfclev(i))sfclev_j = j
                  j = j + 1

               else
    
! Subtract this level from the total count for this station.

                  platform % info % levels = platform % info % levels - 1

               endif  ! If any observed variables exist

            else

               platform % info % levels = platform % info % levels - 1

            endif     ! If P/Z exist

         enddo        ! Level loop

! It's possible that there will be no data at this point.  One example of this
! is a RASS (temperature) only Multi-Agency Profiler report.

         if(platform % info % levels == 0) go to 5

! Handle surface corrections, and other end-of-station processing.

         CALL da_obs_proc_station(platform, fm)

! Move the data for this station from PLATFORM to IV.

         if (global .and. (platform%loc%i < ids .or. platform%loc%i >= ide)) then
            ndup = 2
         else
            ndup = 1
         endif

!------------------------------------------------------------------------
! Duplication explanation:
!
! - Assume two reports (1 and 2)
!
! - What this does is that if you have N obs available what goes into 
!   iv is 1,1,...,N,N.
!
!   Also note that the ntotal that's put into iv%info%obs_global_index
!   is the "incremented of first pass only" local value, so:
!
!   if ndup = 2 then
!
!   index    orig ob index       iv%info%obs_global_index
!     1           1                  1
!     2           1                  1
!     3           2                  2
!     4           2                  2
!
! It's a bit more complicated than that, because of the test on
! platform%loc%i.  This means we're only duplicating obs that are in
! a particular subset of the domain.
!------------------------------------------------------------------------

         do l = 1, ndup

          SELECT CASE(fm)

            CASE(15)              

               n = n + 1
               if(l.eq.1)ntot = ntot + 1
               j = n
               iv % metar(j) % h = platform % each(1) % height
               iv % info(metar) % zk(1,j) = platform % each(1) % zk
               iv % metar(j) % u = platform % each(1) % u
               iv % metar(j) % v = platform % each(1) % v
               iv % metar(j) % t = platform % each(1) % t
               iv % metar(j) % p = platform % each(1) % p
               iv % metar(j) % q = platform % each(1) % q

            CASE(13)              

               n = n + 1
               if(l.eq.1)ntot = ntot + 1
               j = n
               iv % ships(j) % h = platform % each(1) % height
               iv % info(ships) % zk(1,j) = platform % each(1) % zk
               iv % ships(j) % u = platform % each(1) % u
               iv % ships(j) % v = platform % each(1) % v
               iv % ships(j) % t = platform % each(1) % t
               iv % ships(j) % p = platform % each(1) % p
               iv % ships(j) % q = platform % each(1) % q

            CASE(88)              

               n = n + 1
               if(l.eq.1)ntot = ntot + 1
               j = n

! With the datasets that have levels, we have to allocate the memory.
! Note that this is needed for GEOAMV and AIREP even though the MADIS data
! will produce a single-level report for each "station" (as contrasted
! with multiple-level reports for a single satellite or aircraft).  For example,
! if you have 10 aircraft reporting 5 levels of info each, this
! will produce 50 distinct reports.

               allocate (iv % geoamv(j) % u(1:1))
               allocate (iv % geoamv(j) % v(1:1))
               allocate (iv % geoamv(j) % p(1:1))

               iv % info(geoamv) % zk(1,j) = platform % each(1) % zk
               iv % geoamv(j) % u(1) = platform % each(1) % u
               iv % geoamv(j) % v(1) = platform % each(1) % v
               iv % geoamv(j) % p(1) = platform % each(1) % p % inv

            CASE(96)              

               n = n + 1
               if(l.eq.1)ntot = ntot + 1
               j = n

               allocate (iv % airep(j) % h(1:1))
               allocate (iv % airep(j) % u(1:1))
               allocate (iv % airep(j) % v(1:1))
               allocate (iv % airep(j) % t(1:1))
               allocate (iv % airep(j) % p(1:1))

               iv % airep(j) % h(1) = platform % each(1) % height
               iv % info(airep) % zk(1,j) = platform % each(1) % zk
               iv % airep(j) % u(1) = platform % each(1) % u
               iv % airep(j) % v(1) = platform % each(1) % v
               iv % airep(j) % t(1) = platform % each(1) % t
               iv % airep(j) % p(1) = platform % each(1) % p % inv

            CASE(35)

! If this is a raob sfc level, put it into sonde_sfc and not into sound.

               n = n + 1
               if(l.eq.1)ntot = ntot + 1
               j = n

               m = platform%info%levels

	       if(sfclev_j /= 0)then

                  iv%sonde_sfc(j)%h = platform%each(sfclev_j)%height
                  iv%sonde_sfc(j)%u = platform%each(sfclev_j)%u
                  iv%sonde_sfc(j)%v = platform%each(sfclev_j)%v
                  iv%sonde_sfc(j)%t = platform%each(sfclev_j)%t
                  iv%sonde_sfc(j)%q = platform%each(sfclev_j)%q
                  iv%sonde_sfc(j)%p = platform%each(sfclev_j)%p
                  iv%info(sonde_sfc)%zk(1,j) = platform%each(sfclev_j)%zk
 
                  platform%info%levels = platform%info%levels - 1
                  
               else

                  iv % sonde_sfc(j) % h = missing_r
                  iv % sonde_sfc(j) % u % inv = missing_r
                  iv % sonde_sfc(j) % u % qc = missing_data
                  iv % sonde_sfc(j) % u % error = missing_r
                  iv % sonde_sfc(j) % v = iv % sonde_sfc(j) % u
                  iv % sonde_sfc(j) % t = iv % sonde_sfc(j) % u
                  iv % sonde_sfc(j) % p = iv % sonde_sfc(j) % u
                  iv % sonde_sfc(j) % q = iv % sonde_sfc(j) % u

               endif

               allocate (iv % sound(j) % h        (1:platform % info % levels))
               allocate (iv % sound(j) % u        (1:platform % info % levels))
               allocate (iv % sound(j) % v        (1:platform % info % levels))
               allocate (iv % sound(j) % t        (1:platform % info % levels))
               allocate (iv % sound(j) % p        (1:platform % info % levels))
               allocate (iv % sound(j) % q        (1:platform % info % levels))

               do k = 1, m

! Note:  I don't have a testcase that involves the duplication logic,
! and I'm not sure if the "j-1" logic below will work correctly under
! those circumstances.

                  if(sfclev_j.eq.0)then
                     iv % sound(j) % h(k) = platform % each(k) % height
                     iv % info(sound) % zk(k,j) = platform % each(k) % zk
                     iv % sound(j) % u(k) = platform % each(k) % u
                     iv % sound(j) % v(k) = platform % each(k) % v
                     iv % sound(j) % t(k) = platform % each(k) % t
                     iv % sound(j) % p(k) = platform % each(k) % p % inv
                     iv % sound(j) % q(k) = platform % each(k) % q
                  else if(k.lt.sfclev_j)then
                     iv % sound(j) % h(k) = platform % each(k) % height
                     iv % info(sound) % zk(k,j) = platform % each(k) % zk
                     iv % sound(j) % u(k) = platform % each(k) % u
                     iv % sound(j) % v(k) = platform % each(k) % v
                     iv % sound(j) % t(k) = platform % each(k) % t
                     iv % sound(j) % p(k) = platform % each(k) % p % inv
                     iv % sound(j) % q(k) = platform % each(k) % q
                  else if(k.ne.sfclev_j)then
                     iv % sound(j) % h(k-1) = platform % each(k) % height
                     iv % info(sound) % zk(k-1,j) = platform % each(k) % zk
                     iv % sound(j) % u(k-1) = platform % each(k) % u
                     iv % sound(j) % v(k-1) = platform % each(k) % v
                     iv % sound(j) % t(k-1) = platform % each(k) % t
                     iv % sound(j) % p(k-1) = platform % each(k) % p % inv
                     iv % sound(j) % q(k-1) = platform % each(k) % q
                  endif

               enddo

            CASE(111)              

               n = n + 1
               if(l.eq.1)ntot = ntot + 1
               j = n
               iv % gpspw(j) % tpw = platform % loc % pw

          END SELECT

          CALL FILL_IV_INFO(obs_index,n,platform,ntot,platform % info % levels,iv)

         enddo      ! duplication loop

      endif         ! if(keep(i) == 1)

5  enddo            ! station loop

   nlocal_out = n
   ntotal_out = ntot

!zx   iv%info(obs_index)%plocal(1) = nlocal_out
!zx   iv%info(obs_index)%ptotal(1) = ntotal(obs_index)
!zx   iv%info(obs_index)%ntotal = ntotal(obs_index)
!zx   if(fm == 35)then
!zx      iv%info(sonde_sfc)%obs_global_index(obs_index) = ntotal_out
!zx      iv%info(sonde_sfc)%plocal(1) = nlocal_out
!zx      iv%info(sonde_sfc)%ptotal(1) = ntotal(sonde_sfc)
!zx      iv%info(sonde_sfc)%ntotal = ntotal(sonde_sfc)
!zx      iv%info(sonde_sfc)%nlocal = nlocal_out
!zx   endif

   return

END SUBROUTINE PROCESS_DATASET


SUBROUTINE GET_OB_ERRORS(each)

!------------------------------------------------------------------------------
! PURPOSE: Fill in the observational errors, 
!
! METHOD:  The method was adapted from the MM5 3DVAR obs preprocessor 
!          obs_err_ncep subroutine.  The observational error is defined at 
!          1000, 700, 500, 300, 100, 50mb for U, V, T, RH and P.  Values at 
!          actual obs position are obtained by interpolation between 
!          these levels (logarithmic for U and V) and linear for (T and RH).
!          Errors are extrapolated below 1000mb and above 50mb.
!
!          Also note that the RH error is put into the Q error -- the
!          WRF 3DVAR moisture variable is Q, but the error table is
!          specified in RH.  The da_fill_obs_structures routine (called
!          after this on) will take the RH error in the Q slot, and
!          use that in calculating the actual error to be used for Q.
!        
! REFERENCES:
!
!          Parrish S. and J. Derber 1992: The National Meteorological Centers
!             Spectral Statistical Analysis-Interpolation System.
!             Month. Wea. Rev., 120,1747-1763.
!
! HISTORY: 07-22-02  M. Barth         Original version.
!
! PARENT_MODULE: da_setup_structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
! ARGUMENTS:

   TYPE (each_level_type), INTENT(INOUT) :: each          ! Data for this level.

! LOCAL:

   real err_k(6),err_u(6),err_v(6),err_t(6),err_rh(6),err_p(6)

!...NCEP errors (U in m/s, V in m/s, T in k, P in Pa)
!...pressure is obtained from surface value linearly decreasing 
!...to 10% of its value at 10hPa
!...RH has been divided by 2
!
   data err_k/100000.,70000.,50000.,30000.,10000.,5000./
   data err_u/1.4,   2.4,   2.8,   3.4,   2.5,  2.7/
   data err_v/1.4,   2.4,   2.8,   3.4,   2.5,  2.7/
   data err_t/1.8,   1.3,   1.3,   2.0,   3.1,  4.0/
   data err_rh/10.0,  10.0,  10.0,  10.0,  10.0,  10.0/
   data err_p/100.0,  72.7,  54.5,  36.4,  18.2,  13.6/

!------------------------------------------------------------------------------!
! At this point, I don't think we can have missing P.  To stay on the safe side,
! if P is missing --> set all errors to missing.

   if(each % p % inv == missing_r)then

      each % u % error = missing_r
      each % v % error = missing_r
      each % p % error = missing_r
      each % t % error = missing_r
      each % q % error = missing_r

   else

! Vertical interpolation of NCEP observational error.

      each % u % error = INTPLIN(each%p%inv,err_k,err_u)
      each % v % error = INTPLIN(each%p%inv,err_k,err_v)
      each % p % error = INTPLIN(each%p%inv,err_k,err_p)
      each % t % error = INTPLOG(each%p%inv,err_k,err_t)
      each % q % error = INTPLOG(each%p%inv,err_k,err_rh)

    endif
    
    return

END SUBROUTINE GET_OB_ERRORS

FUNCTION INTPLIN (x,xx,yy) RESULT (val)
!------------------------------------------------------------------------------!
   implicit none

   real, dimension (:) :: xx, yy
   real                :: x
   real                :: val

   integer             :: n,m,jl
!------------------------------------------------------------------------------!
   n = size (xx)
   m = size (yy)

   jl = LOCATE(x,xx)

   if (jl .le. 0) then    
      val = yy (1)
   else if (jl .ge. n) then    
      val = yy (n)
   else
      val = (xx (jl+1) - x) * yy (jl) + (x - xx (jl)) * yy (jl+1)
      val = val / (xx (jl+1) - xx (jl))
   endif

END FUNCTION INTPLIN

FUNCTION INTPLOG (x,xx,yy) result (val)
!------------------------------------------------------------------------------!
   implicit none

   real, dimension (:) :: xx, yy
   real                :: x
   real                :: val

   integer             :: n,m,jl
!------------------------------------------------------------------------------!
   n = size (xx)
   m = size (yy)

   jl = LOCATE(x,xx)

   if (jl .le. 0) then    
      val = yy (1)
   else if (jl .ge. n) then    
      val = yy (n)
   else
      val = log (xx (jl+1) / x) * yy (jl) + log (x / xx (jl)) * yy (jl+1)
      val = val / log (xx (jl+1) / xx (jl))
   endif

END FUNCTION INTPLOG

FUNCTION LOCATE (x,xx) result (index)
!------------------------------------------------------------------------------!
   implicit none

   real, dimension (:) :: xx
   real                :: x
   integer             :: index

   integer             :: n,jl,jm,ju
   logical             :: ascnd
!------------------------------------------------------------------------------!
   n = size (xx)
   ascnd = (xx (n) >= xx (1))   ! true if ascending order, false otherwise
   jl = 0                       ! initialize lower limit
   ju = n+1                     ! initialize upper limit

   do

      if (ju-jl <= 1) exit      ! repeat until this condition is satisfied

      jm = (ju+jl) / 2.         ! compute a mid point

      if (ascnd .eqv. (x >= xx (jm))) then
         jl = jm                ! replace mid point by lower limit
      else
         ju = jm                ! replace mid point by upper limit
      endif

   enddo

   if (x .eq. xx (1)) then      ! set the output, being careful with endpoints
      index = 1
   else if (x .eq. xx (n)) then
      index = n-1 
   else
      index = jl
   endif

END FUNCTION LOCATE

SUBROUTINE FIND_SFC_LEVEL(ista,nlev,nsta,nlevels,levtype,p,slp,lev)
!------------------------------------------------------------------------------!
   IMPLICIT NONE
   
! ARGUMENTS:

   INTEGER*4, INTENT(IN)        :: ista               ! Index of current station
   INTEGER*4, INTENT(IN)        :: nlev               ! Number of levels dimension
   INTEGER*4, INTENT(IN)        :: nsta               ! Number of stations dimension
   INTEGER*4, INTENT(IN)        :: nlevels            ! Number of levels for this station
   INTEGER*4                    :: lev                ! Index to sfc level
   REAL*4,    INTENT(IN)        :: levtype(nlev,nsta) ! Level type (1 is sfc)
   REAL*4,    INTENT(IN)        :: p(nlev,nsta)       ! Pressure (Pa)
   REAL*4,    INTENT(OUT)       :: slp                ! Surface pressure (Pa)

! Note that lev is not dimensioned with INTENT(IN) due to some picky compilers...

   do lev = 1, nlevels

      if(levtype(lev,ista).eq.1)then

         slp = p(lev,ista)
         return

      endif

   enddo

   return

END SUBROUTINE FIND_SFC_LEVEL

SUBROUTINE FILL_IV_INFO(obs_index,iv_index,platform,ntotal,nlevels,iv)

!------------------------------------------------------------------------------
! PURPOSE: Fill in the iv%info structures used for each station's reports.
!
! METHOD:  Take it out of platform and put it into iv%info.
!
! HISTORY: 06-05-08  M. Barth         Original version.

!                              
!
! PARENT_MODULE: da_setup_structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
! ARGUMENTS:

   INTEGER*4, INTENT(IN)        :: obs_index       ! iv%info() index
   INTEGER*4, INTENT(IN)        :: iv_index        ! iv%info()%<item>() index
   TYPE (multi_level_type), INTENT(IN) :: platform ! Structure with the data
   INTEGER*4, INTENT(IN)        :: ntotal          ! NTOTAL at this point
   INTEGER*4, INTENT(IN)        :: nlevels         ! Num levels for the station
   TYPE (iv_type), INTENT(INOUT) :: iv             ! Innovation vector
   
   iv%info(obs_index)%name(iv_index)      = platform%info%name
   iv%info(obs_index)%platform(iv_index)  = platform%info%platform
   iv%info(obs_index)%id(iv_index)        = platform%info%id
   iv%info(obs_index)%date_char(iv_index) = platform%info%date_char

   iv%info(obs_index)%levels(iv_index)    = nlevels
   iv%info(obs_index)%lat(:,iv_index)     = platform%info%lat
   iv%info(obs_index)%lon(:,iv_index)     = platform%info%lon
   iv%info(obs_index)%elv(iv_index)       = platform%info%elv
   iv%info(obs_index)%pstar(iv_index)     = platform%info%pstar

   iv%info(obs_index)%slp(iv_index)           = platform%loc%slp
   iv%info(obs_index)%pw(iv_index)            = platform%loc%pw
   iv%info(obs_index)%x(:,iv_index)           = platform%loc%x
   iv%info(obs_index)%y(:,iv_index)           = platform%loc%y
   iv%info(obs_index)%i(:,iv_index)           = platform%loc%i
   iv%info(obs_index)%j(:,iv_index)           = platform%loc%j
   iv%info(obs_index)%dx(:,iv_index)          = platform%loc%dx
   iv%info(obs_index)%dxm(:,iv_index)         = platform%loc%dxm
   iv%info(obs_index)%dy(:,iv_index)          = platform%loc%dy
   iv%info(obs_index)%dym(:,iv_index)         = platform%loc%dym
   iv%info(obs_index)%proc_domain(:,iv_index) = platform%loc%proc_domain

   iv%info(obs_index)%obs_global_index(iv_index) = ntotal


! special case for sonde_sfc, duplicate sound info

   if (obs_index == sound) then

      iv%info(sonde_sfc)%name(iv_index)      = platform%info%name
      iv%info(sonde_sfc)%platform(iv_index)  = platform%info%platform
      iv%info(sonde_sfc)%id(iv_index)        = platform%info%id
      iv%info(sonde_sfc)%date_char(iv_index) = platform%info%date_char
      iv%info(sonde_sfc)%levels(iv_index)    = 1
      iv%info(sonde_sfc)%lat(:,iv_index)     = platform%info%lat
      iv%info(sonde_sfc)%lon(:,iv_index)     = platform%info%lon
      iv%info(sonde_sfc)%elv(iv_index)       = platform%info%elv
      iv%info(sonde_sfc)%pstar(iv_index)     = platform%info%pstar

      iv%info(sonde_sfc)%slp(iv_index)           = platform%loc%slp
      iv%info(sonde_sfc)%pw(iv_index)            = platform%loc%pw
      iv%info(sonde_sfc)%x(:,iv_index)           = platform%loc%x
      iv%info(sonde_sfc)%y(:,iv_index)           = platform%loc%y
      iv%info(sonde_sfc)%i(:,iv_index)           = platform%loc%i
      iv%info(sonde_sfc)%j(:,iv_index)           = platform%loc%j
      iv%info(sonde_sfc)%dx(:,iv_index)          = platform%loc%dx
      iv%info(sonde_sfc)%dxm(:,iv_index)         = platform%loc%dxm
      iv%info(sonde_sfc)%dy(:,iv_index)          = platform%loc%dy
      iv%info(sonde_sfc)%dym(:,iv_index)         = platform%loc%dym
      iv%info(sonde_sfc)%proc_domain(:,iv_index) = platform%loc%proc_domain

      iv%info(sonde_sfc)%obs_global_index(iv_index) = ntotal

   end if

   return

END SUBROUTINE FILL_IV_INFO

#endif
