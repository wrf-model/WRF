!WRF:DRIVER_LAYER:TOP

MODULE atm_comp_mct
!<DESCRIPTION>
! This module defines top-level atm_init_mct(), atm_run_mct(), and atm_final_mct()
!
! Juanxing He, 12 Apr 2009: WRF 3.0
! Juanxing He, 04 Dec 2009
! Juanxing He, 10 May 2010: WRF 3.2
! Matt Higgins, 13 Jul 2010: fixed incorrect precipitation coupling
! Matt Higgins, 17 Aug 2010: fixed snowh units to be consistent with WRF registry
! Matt Higgins, 23 Feb 2011: added WRF wind rotation on fields sent to coupler
! Matt Higgins, 04 Mar 2011: complete review of coupling code
! Matt Higgins, 06 Jun 2011: fixed double-counting of precip export
! Matt Higgins, 24 Jun 2011: averaged values, rather than instantaneous values, now exported
! Tony Craig  , 23 Mar 2012: updates to CESM1.1 beta11, new index approach
! Alice DuVivier, 3 Oct 2012: changed non-convective precip calculations
! Alice DuVivier, 4 Jan 2013: added roughness length import from coupler, fixed initialization
!                             for rounding, added calculations for albedo diagnostic variables
! Alice DuVivier, 28 May 2013: made constants consistent with CCSM, changed area calculation
! Andrew Roberts, 21 June 2013: Added capability to pass orbital parameters from CPL
! Alice DuVivier, 15 July 2013: changed literal constants from WRF and commented code



!</DESCRIPTION>

  use mct_mod
  use esmf, CCSM_Clock=>ESMF_Clock
  use seq_flds_mod
  use seq_drydep_mod, only: drydep_fields_token, lnd_drydep
  use seq_infodata_mod
  use seq_timemgr_mod
  use seq_comm_mct, only:seq_comm_iamroot,seq_comm_iamin
  use seq_cdata_mod
  use shr_file_mod     , only: shr_file_getunit, shr_file_freeunit, &
                               shr_file_setLogUnit, shr_file_setLogLevel, &
                               shr_file_getLogUnit, shr_file_getLogLevel, &
		               shr_file_setIO
  use shr_sys_mod      , only: shr_sys_flush, shr_sys_abort
! Alice DuVivier note: load values for CESM constants
  use shr_const_mod

! for WRF
	
   USE module_wrf_top, ONLY : wrf_init, wrf_run, wrf_finalize, config_flags
   USE module_domain, ONLY : head_grid, get_ijk_from_grid
   USE module_state_description
   USE module_streams

   USE module_machine
   USE module_domain
   USE module_domain_type
   USE module_integrate
   USE module_driver_constants
   USE module_configure
   USE module_io_domain
   USE module_io
   USE module_io_wrf
   USE module_streams, only : RESTART_NOW, END_NOW
   USE module_timing
   USE module_utility  ! for using ESMF function in WRF
   USE module_wrf_error
   USE wrf_instance

! next line added by Andrew Roberts for CPL solar synchronization
   USE module_ra_rasm 

! Alice DuVivier note: load WRF constant values
   use module_model_constants, only: t0,p0

!   USE module_atm_communicator, only: mpi_communicator_atm
#ifdef DM_PARALLEL
   USE module_dm
   USE module_comm_dm, ONLY : halo_em_phys_a_sub
#endif


! !PUBLIC TYPES:
  implicit none
  save
  private ! except

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

  public :: atm_init_mct
  public :: atm_run_mct
  public :: atm_final_mct

!--------------------------------------------------------------------------
! Private data
!--------------------------------------------------------------------------

  integer :: index_a2x_Sa_z            ! bottom atm level height
  integer :: index_a2x_Sa_u            ! bottom atm level zon wind
  integer :: index_a2x_Sa_v            ! bottom atm level mer wind
  integer :: index_a2x_Sa_tbot         ! bottom atm level temp
  integer :: index_a2x_Sa_ptem         ! bottom atm level pot temp
  integer :: index_a2x_Sa_shum         ! bottom atm level spec hum
  integer :: index_a2x_Sa_dens         ! bottom atm level air den
  integer :: index_a2x_Sa_pbot         ! bottom atm level pressure
  integer :: index_a2x_Sa_pslv         ! sea level atm pressure
  integer :: index_a2x_Faxa_lwdn       ! downward lw heat flux
  integer :: index_a2x_Faxa_rainc      ! prec: liquid "convective"
  integer :: index_a2x_Faxa_rainl      ! prec: liquid "large scale"
  integer :: index_a2x_Faxa_snowc      ! prec: frozen "convective"
  integer :: index_a2x_Faxa_snowl      ! prec: frozen "large scale"
  integer :: index_a2x_Faxa_swndr      ! sw: nir direct  downward
  integer :: index_a2x_Faxa_swvdr      ! sw: vis direct  downward
  integer :: index_a2x_Faxa_swndf      ! sw: nir diffuse downward
  integer :: index_a2x_Faxa_swvdf      ! sw: vis diffuse downward
  integer :: index_a2x_Faxa_swnet      ! sw: net
  integer :: index_a2x_Faxa_bcphidry   ! flux: Black Carbon hydrophilic dry deposition
  integer :: index_a2x_Faxa_bcphodry   ! flux: Black Carbon hydrophobic dry deposition
  integer :: index_a2x_Faxa_bcphiwet   ! flux: Black Carbon hydrophilic wet deposition
  integer :: index_a2x_Faxa_ocphidry   ! flux: Organic Carbon hydrophilic dry deposition
  integer :: index_a2x_Faxa_ocphodry   ! flux: Organic Carbon hydrophobic dry deposition
  integer :: index_a2x_Faxa_ocphiwet   ! flux: Organic Carbon hydrophilic dry deposition
  integer :: index_a2x_Faxa_dstwet1    ! flux: Size 1 dust -- wet deposition
  integer :: index_a2x_Faxa_dstwet2    ! flux: Size 2 dust -- wet deposition
  integer :: index_a2x_Faxa_dstwet3    ! flux: Size 3 dust -- wet deposition
  integer :: index_a2x_Faxa_dstwet4    ! flux: Size 4 dust -- wet deposition
  integer :: index_a2x_Faxa_dstdry1    ! flux: Size 1 dust -- dry deposition
  integer :: index_a2x_Faxa_dstdry2    ! flux: Size 2 dust -- dry deposition
  integer :: index_a2x_Faxa_dstdry3    ! flux: Size 3 dust -- dry deposition
  integer :: index_a2x_Faxa_dstdry4    ! flux: Size 4 dust -- dry deposition
  integer :: index_a2x_Sa_co2prog      ! bottom atm level prognostic co2
  integer :: index_a2x_Sa_co2diag      ! bottom atm level diagnostic co2

  integer :: index_x2a_Sx_t            ! surface temperature             
  integer :: index_x2a_So_t            ! sea surface temperature         
  integer :: index_x2a_Sf_lfrac        ! surface land fraction           
  integer :: index_x2a_Sf_ifrac        ! surface ice fraction            
  integer :: index_x2a_Sf_ofrac        ! surface ocn fraction            
  integer :: index_x2a_Sx_tref         ! 2m reference temperature        
  integer :: index_x2a_Sx_qref         ! 2m reference specific humidity  
  integer :: index_x2a_Sx_logz0        ! roughness length
  integer :: index_x2a_Sx_avsdr        ! albedo, visible, direct         
  integer :: index_x2a_Sx_anidr        ! albedo, near-ir, direct         
  integer :: index_x2a_Sx_avsdf        ! albedo, visible, diffuse        
  integer :: index_x2a_Sx_anidf        ! albedo, near-ir, diffuse        
  integer :: index_x2a_Sl_snowh        ! surface snow depth over land
  integer :: index_x2a_Si_snowh        ! surface snow depth over ice
  integer :: index_x2a_Sl_fv           ! friction velocity
  integer :: index_x2a_Sl_ram1         ! aerodynamical resistance
  integer :: index_x2a_Faxx_taux       ! wind stress, zonal              
  integer :: index_x2a_Faxx_tauy       ! wind stress, meridional         
  integer :: index_x2a_Faxx_lat        ! latent          heat flux       
  integer :: index_x2a_Faxx_sen        ! sensible        heat flux       
  integer :: index_x2a_Faxx_lwup       ! upward longwave heat flux       
  integer :: index_x2a_Faxx_evap       ! evaporation    water flux       
  integer :: index_x2a_Fall_flxdst1    ! dust flux size bin 1    
  integer :: index_x2a_Fall_flxdst2    ! dust flux size bin 2    
  integer :: index_x2a_Fall_flxdst3    ! dust flux size bin 3    
  integer :: index_x2a_Fall_flxdst4    ! dust flux size bin 4
  integer :: index_x2a_Fall_flxvoc1    ! voc flux size bin 1    
  integer :: index_x2a_Fall_flxvoc2    ! voc flux size bin 2    
  integer :: index_x2a_Fall_flxvoc3    ! voc flux size bin 3    
  integer :: index_x2a_Fall_flxvoc4    ! voc flux size bin 4    
  integer :: index_x2a_Fall_flxvoc5    ! voc flux size bin 5    
  integer :: index_x2a_Fall_fco2_lnd   ! co2 flux from land   
  integer :: index_x2a_Faoo_fco2_ocn   ! co2 flux from ocean  
  integer :: index_x2a_Faoo_fdms_ocn   ! dms flux from ocean
  integer :: index_x2a_So_ustar        ! surface friction velocity in ocean
  integer :: index_x2a_So_re           ! square of atm/ocn exch. coeff 
  integer :: index_x2a_So_ssq          ! surface saturation specific humidity in ocean 
  integer :: index_x2a_Sl_ddvel        ! dry deposition velocities from land
  integer :: index_x2a_Sx_u10          ! 10m wind

  type(mct_aVect)   :: a2x_a_SNAP
  type(mct_aVect)   :: a2x_a_SUM
  
  integer, parameter :: r8 = 8

! Time averaged counter for flux fields
  integer :: avg_count

! Time averaged flux fields
  character(*), parameter :: a2x_avg_flds = &
     "Faxa_rainc:Faxa_rainl:Faxa_snowc:Faxa_snowl:Sa_u:Sa_v:Sa_pslv:Sa_z:"// &
     "Sa_pbot:Sa_ptem:Sa_tbot:Sa_dens:Sa_shum:Faxa_swnet:Faxa_lwdn:Faxa_swndr:"// &
     "Faxa_swvdr:Faxa_swndf:Faxa_swvdf"
!
! Are all surface types present   
!
  logical :: lnd_present ! if true => land is present
  logical :: ocn_present ! if true => ocean is present
     
  integer :: iulog  ! for I/O
  logical :: exists
  integer  :: shrlogunit,shrloglev
  integer :: ATMID

  type coupling_state_variables
    real,dimension(:,:),allocatable::tsk,rasdir,rasdif,raldir,raldif
    real::atm_cpl_dt
  end type coupling_state_variables  
  real,   dimension(0:1000 ) :: PSIMTB,PSIHTB

  type (coupling_state_variables) :: ragrid 

!  character(len=*), private, parameter :: &
!     seq_timemgr_noLeap            = "NO_LEAP"   , &
!     seq_timemgr_gregorian         = "GREGORIAN" 

!
!  wrf
!
	
   REAL    :: time

   INTEGER :: loop , &
              levels_to_process
   integer :: mpicom_atm

! in wrf_init
!   TYPE (domain) , POINTER :: keep_grid, grid_ptr, null_domain
!   TYPE (grid_config_rec_type), SAVE :: config_flags
!   INTEGER                 :: number_at_same_level
!   INTEGER                 :: time_step_begin_restart

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level
   LOGICAL :: input_from_file

#ifdef DM_PARALLEL
   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* CONFIG_BUF_LEN
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#endif

   CHARACTER ( 80)      :: rstname
   CHARACTER ( 80)      :: message

CONTAINS


   SUBROUTINE atm_init_mct(EClock, cdata_a, x2a_a, a2x_a,NLFilename)
!<DESCRIPTION>
!     WRF initialization routine.
!</DESCRIPTION>
    type(CCSM_Clock), intent(in)                :: EClock
    type(seq_cdata), intent(inout)              :: cdata_a
    type(mct_aVect), intent(inout)              :: x2a_a
    type(mct_aVect), intent(inout)              :: a2x_a   
    character( *), optional,   intent(IN)    :: NLFilename ! Namelist filename
!
! Locals
!
     type(mct_gsMap), pointer   :: gsMap_atm
     type(mct_gGrid), pointer   :: dom_a
     type(seq_infodata_type),pointer :: infodata
     integer :: lsize 

! This line removed and placed in module_ra_rasm by Andrew Roberts
!     real(r8):: nextsw_cday      ! calendar of next atm shortwave		 

     real(r8):: dayofYear        ! day of year
     integer :: wrfsec           ! wrf time in seconds
     integer :: radsec           ! radiation time in seconds
     integer :: nradt
     integer :: stepno           ! time step			 
     integer :: dtime_sync       ! integer timestep size
     integer :: currentymd       ! current year-month-day
     integer :: dtime            ! time step increment (sec)
     integer :: atm_cpl_dt       ! driver atm coupling time step 
     integer :: start_ymd        ! Start date (YYYYMMDD)
     integer :: start_tod        ! Start time of day (sec)
     integer :: restart_ymd        ! Start date (YYYYMMDD)
     integer :: restart_tod        ! Start time of day (sec)
     integer :: ref_ymd          ! Reference date (YYYYMMDD)
     integer :: ref_tod          ! Reference time of day (sec)
     integer :: stop_ymd         ! Stop date (YYYYMMDD)
     integer :: stop_tod         ! Stop time of day (sec)
     logical :: perpetual_run    ! If in perpetual mode or not
     integer :: perpetual_ymd    ! Perpetual date (YYYYMMDD)

! These lines removed and placed in module_ra_rasm by Andrew Roberts
!     real(r8) :: eccen       ! Eccentricity of orbit
!     real(r8) :: mvelp       ! Locatn of vernal equinox
!     real(r8) :: obliqr      ! Obliquity in radians
!     real(r8) :: lambm0      ! Long of perh. radians
!     real(r8) :: mvelpp      ! Locatn of vernal equinox at perh. 

     integer :: plon,plat    ! global grid number in the lon and lat direction
     character( 80) :: start_type    ! Start type
     character( 80) :: caseid     ! Short case identification
     character( 80) :: ctitle       ! Long case description 
     logical  :: adiabatic ! atm adiabatic mode
     logical  :: ideal_phys! atm idealized-physics mode
     logical   :: aqua_planet   ! aqua_planet mode
     logical   :: brnch_retain_casename
     logical    :: single_column
     real (R8) :: scmlat
     real (R8)  :: scmlon
     integer :: nsrest 
     integer :: phase
   
     character( 80) :: calendar  ! Calendar type
     character( 80) :: starttype ! infodata start type

     TYPE(WRFU_Time) :: current_time
     type(WRFU_TimeInterval) :: off
     integer :: ymd              ! WRF current date (YYYYMMDD)
     integer :: yr               ! WRF current year
     integer :: mon              ! WRF current month
     integer :: day              ! WRF current day
     integer :: hour             ! WRF current hour
     integer :: minuate            ! WRF current minuate
     integer :: second             ! WRF current second
     integer :: tod              ! WRF current time of day (sec)
     logical :: restart
     real,dimension(:),allocatable::value
     
     integer :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
                  start_year,start_month,start_day,start_hour,start_minute,start_second,&
		  i,j,ig
     real::x,y,zoln
     integer::n,ierr,mpicom_driver

      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor	
      TYPE(domain) , POINTER :: tmpgrid	  ! for the temporary integration
      TYPE(domain) , target :: tmpgrid1	  ! for the temporary integration
      INTEGER :: mpicomcart
      INTEGER, DIMENSION(2) :: dims, coords
	
     INTEGER myproc, nproc, hostid, loccomm, buddcounter, mydevice
     INTEGER, ALLOCATABLE :: hostids(:), budds(:)
     CHARACTER*512 hostname
     character(len=*), parameter :: subname = "atm_init_mct"
#if defined(DM_PARALLEL) && defined(RUN_ON_GPU)
     include "mpif.h"
#endif
#include "version_decl"


!<DESCRIPTION>
! Program_name, a global variable defined in frame/module_domain.F, is
! set, then a routine <a href=init_modules.html>init_modules</a> is
! called. This calls all the init programs that are provided by the
! modules that are linked into WRF.  These include initialization of
! external I/O packages.   Also, some key initializations for
! distributed-memory parallelism occur here if DM_PARALLEL is specified
! in the compile: setting up I/O quilt processes to act as I/O servers
! and dividing up MPI communicators among those as well as initializing
! external communication packages such as RSL or RSL_LITE.
!
!</DESCRIPTION>

   program_name = "WRF " // TRIM(release_version) // " MODEL"

! 
! Get data from driver routine
!
   
    call seq_cdata_setptrs(cdata_a, ID=ATMID, &
!                 mpicomcart=mpicomcart,mpicomx=local_communicator_x,&
!                 mpicomy=local_communicator_y,ntasks=ntasks,ntasks_x=ntasks_x,ntasks_y=ntasks_y,& 
                 gsMap=gsMap_atm, dom=dom_a, infodata=infodata)
    call seq_infodata_GetData(infodata,atm_phase=phase)
    call wrf_instance_init(ATMID)

    RESTART_NOW = .false.
    END_NOW = .false.
    if (phase == 1) then

       call seq_cdata_setptrs(cdata_a, mpicom=mpicom_driver)
       call mpi_comm_dup(mpicom_driver,mpicom_atm,ierr)
       CALL wrf_set_dm_communicator( mpicom_atm )

       if (seq_comm_iamroot(ATMID)) then
          inquire(file='atm_modelio.nml',exist=exists)
          if (exists) then
             iulog = shr_file_getUnit()
             call shr_file_setIO('atm_modelio.nml',iulog)
          endif
          write(iulog,*) "WRF initialization"
          call shr_sys_flush(iulog)
       endif
       
       call shr_file_getLogUnit (shrlogunit)
       call shr_file_getLogLevel(shrloglev)
       call shr_file_setLogUnit (iulog)

!       write(iulog,*)'********ATM communicator*********',mpi_communicator_atm,ATMID
! 
! Get data from infodata object
! Call changed by Andrew Roberts to include get of nextsw_cday for WRF solar calcs
!
       call seq_infodata_GetData( infodata,                                           &
            case_name=caseid, case_desc=ctitle,                                       &
            start_type=starttype,                                                     &
            atm_adiabatic=adiabatic,                                                  &
            atm_ideal_phys=ideal_phys,                                                &
            aqua_planet=aqua_planet,                                                  &
            brnch_retain_casename=brnch_retain_casename,                              &
            single_column=single_column, scmlat=scmlat, scmlon=scmlon,                &
            orb_eccen=eccen, orb_mvelpp=mvelpp, orb_lambm0=lambm0, orb_obliqr=obliqr, &
            lnd_present=lnd_present, ocn_present=ocn_present,                         & 
            perpetual=perpetual_run, perpetual_ymd=perpetual_ymd,                     &
            nextsw_cday=nextsw_cday)
    
! Get nsrest from startup type methods
!
       if (     trim(starttype) == trim(seq_infodata_start_type_start)) then
          nsrest = 0
       else if (trim(starttype) == trim(seq_infodata_start_type_cont) ) then
          nsrest = 1
       else if (trim(starttype) == trim(seq_infodata_start_type_brnch)) then
          nsrest = 3
       else
          write(iulog,*) 'wrf_init: ERROR: unknown starttype'
          call shr_sys_abort()
       end if
!-----------------------------------------------
   
       ! Initialize utilities (time manager, etc.)
       call seq_timemgr_EClockGetData(EClock, calendar=calendar )
       if (trim(calendar) == trim(seq_timemgr_noLeap)) then
          CALL WRFU_Initialize( defaultCalendar=ESMF_CALKIND_NOLEAP )
       elseif (trim(calendar) == trim(seq_timemgr_gregorian)) then
          CALL WRFU_Initialize( defaultCalendar=ESMF_CALKIND_GREGORIAN )
       else
          write(iulog,*) 'wrf_init: ERROR: unknown calendar ',trim(calendar)
          call shr_sys_abort('wrf_init: ERROR: unknown calendar')
       endif

       ! Call WRF "init" routine, which, for a DM_PARALLEL run, will recognize 
       ! that ESMF has already called MPI_INIT and respond appropriately.  

       CALL wrf_init( no_init1=.TRUE. )

       call get_ijk_from_grid (  head_grid ,                   &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

       allocate(ragrid%tsk(ids:ide,jds:jde))
       allocate(ragrid%rasdir(ids:ide,jds:jde))
       allocate(ragrid%rasdif(ids:ide,jds:jde))
       allocate(ragrid%raldir(ids:ide,jds:jde))
       allocate(ragrid%raldif(ids:ide,jds:jde))

       do n=0,1000
          zoln=-float(n)*0.01
          x=(1-16.*zoln)**0.25
          psimtb(n)=2*alog(0.5*(1+x))+alog(0.5*(1+x*x))- 2.*atan(x)+2.*atan(1.)
          y=(1-16*zoln)**0.5
          psihtb(n)=2*alog(0.5*(1+y))
       enddo

! set default values for subtimes
       head_grid%start_subtime = domain_get_start_time ( head_grid )
       head_grid%stop_subtime = domain_get_stop_time ( head_grid )
      
!
! Check consistency of outside clock and WRF clock
! Check consistency of restart time information with input clock
!	
!
! Initialize time manager (outside clock).
!
       call seq_timemgr_EClockGetData(EClock, &
                                      start_ymd=start_ymd, start_tod=start_tod, &
                                      ref_ymd=ref_ymd, ref_tod=ref_tod,         &
                                      stop_ymd=stop_ymd, stop_tod=stop_tod,     &
                                      calendar=calendar )
				      
       call nl_get_restart(1,restart)
       if (restart) then
          CALL nl_get_start_year(head_grid%id,start_year)
          CALL nl_get_start_month(head_grid%id,start_month)
          CALL nl_get_start_day(head_grid%id,start_day)
          CALL nl_get_start_hour(head_grid%id,start_hour)
          CALL nl_get_start_minute(head_grid%id,start_minute)
          CALL nl_get_start_second(head_grid%id,start_second)    
       end if

!
! Initialize MCT gsMap, domain and attribute vectors
!
       call atm_SetgsMap_mct( head_grid, mpicom_atm, ATMID, gsMap_atm )
       lsize = mct_gsMap_lsize(gsMap_atm, mpicom_atm)
!
! Initialize MCT domain 
!
       call atm_domain_mct( head_grid, lsize, gsMap_atm, dom_a )
!
! Initialize MCT attribute vectors
!
       call mct_aVect_init(a2x_a, rList=seq_flds_a2x_fields, lsize=lsize)
       call mct_aVect_zero(a2x_a)
       
       call mct_aVect_init(x2a_a, rList=seq_flds_x2a_fields, lsize=lsize) 
       call mct_aVect_zero(x2a_a)
       
       call mct_aVect_init(a2x_a_SNAP, rList=a2x_avg_flds, lsize=lsize)
       call mct_aVect_zero(a2x_a_SNAP)
       
       call mct_aVect_init(a2x_a_SUM , rList=a2x_avg_flds, lsize=lsize)
       call mct_aVect_zero(a2x_a_SUM )

!
! Initialize indices
!
       index_x2a_Sx_avsdr      = mct_avect_indexra(x2a_a,'Sx_avsdr')
       index_x2a_Sx_anidr      = mct_avect_indexra(x2a_a,'Sx_anidr')
       index_x2a_Sx_avsdf      = mct_avect_indexra(x2a_a,'Sx_avsdf')
       index_x2a_Sx_anidf      = mct_avect_indexra(x2a_a,'Sx_anidf')
       index_x2a_Sx_t          = mct_avect_indexra(x2a_a,'Sx_t')
       index_x2a_So_t          = mct_avect_indexra(x2a_a,'So_t')
       index_x2a_Sl_snowh      = mct_avect_indexra(x2a_a,'Sl_snowh')
       index_x2a_Si_snowh      = mct_avect_indexra(x2a_a,'Si_snowh')
       index_x2a_Sx_tref       = mct_avect_indexra(x2a_a,'Sx_tref')
       index_x2a_Sx_qref       = mct_avect_indexra(x2a_a,'Sx_qref')
       index_x2a_Sx_logz0      = mct_avect_indexra(x2a_a,'Sx_logz0')

       index_x2a_Sf_ifrac      = mct_avect_indexra(x2a_a,'Sf_ifrac')
       index_x2a_Sf_ofrac      = mct_avect_indexra(x2a_a,'Sf_ofrac')
       index_x2a_Sf_lfrac      = mct_avect_indexra(x2a_a,'Sf_lfrac')

       index_x2a_Sx_u10        = mct_avect_indexra(x2a_a,'Sx_u10')
       index_x2a_Faxx_taux     = mct_avect_indexra(x2a_a,'Faxx_taux')
       index_x2a_Faxx_tauy     = mct_avect_indexra(x2a_a,'Faxx_tauy')
       index_x2a_Faxx_lat      = mct_avect_indexra(x2a_a,'Faxx_lat')
       index_x2a_Faxx_sen      = mct_avect_indexra(x2a_a,'Faxx_sen')
       index_x2a_Faxx_lwup     = mct_avect_indexra(x2a_a,'Faxx_lwup')
       index_x2a_Faxx_evap     = mct_avect_indexra(x2a_a,'Faxx_evap')
       index_x2a_So_ustar      = mct_avect_indexra(x2a_a,'So_ustar')
       index_x2a_So_re         = mct_avect_indexra(x2a_a,'So_re')
       index_x2a_So_ssq        = mct_avect_indexra(x2a_a,'So_ssq')
       index_x2a_Sl_fv         = mct_avect_indexra(x2a_a,'Sl_fv')
       index_x2a_Sl_ram1       = mct_avect_indexra(x2a_a,'Sl_ram1')
       index_x2a_Fall_flxdst1  = mct_avect_indexra(x2a_a,'Fall_flxdst1')
       index_x2a_Fall_flxdst2  = mct_avect_indexra(x2a_a,'Fall_flxdst2')
       index_x2a_Fall_flxdst3  = mct_avect_indexra(x2a_a,'Fall_flxdst3')
       index_x2a_Fall_flxdst4  = mct_avect_indexra(x2a_a,'Fall_flxdst4')
       index_x2a_Fall_flxvoc1  = mct_avect_indexra(x2a_a,'Fall_flxvoc1' ,perrWith='quiet')
       index_x2a_Fall_flxvoc2  = mct_avect_indexra(x2a_a,'Fall_flxvoc2' ,perrWith='quiet')
       index_x2a_Fall_flxvoc3  = mct_avect_indexra(x2a_a,'Fall_flxvoc3' ,perrWith='quiet')
       index_x2a_Fall_flxvoc4  = mct_avect_indexra(x2a_a,'Fall_flxvoc4' ,perrWith='quiet')
       index_x2a_Fall_flxvoc5  = mct_avect_indexra(x2a_a,'Fall_flxvoc5' ,perrWith='quiet')
       index_x2a_Fall_fco2_lnd = mct_avect_indexra(x2a_a,'Fall_fco2_lnd',perrWith='quiet')
       index_x2a_Faoo_fco2_ocn = mct_avect_indexra(x2a_a,'Faoo_fco2_ocn',perrWith='quiet')
       index_x2a_Faoo_fdms_ocn = mct_avect_indexra(x2a_a,'Faoo_fdms_ocn',perrWith='quiet')
       if ( lnd_drydep ) then
          index_x2a_Sl_ddvel   = mct_avect_indexra(x2a_a, trim(drydep_fields_token))
       else
          index_x2a_Sl_ddvel   = 0
       end if

       index_a2x_Sa_z          = mct_avect_indexra(a2x_a,'Sa_z')
       index_a2x_Sa_u          = mct_avect_indexra(a2x_a,'Sa_u')
       index_a2x_Sa_v          = mct_avect_indexra(a2x_a,'Sa_v')
       index_a2x_Sa_tbot       = mct_avect_indexra(a2x_a,'Sa_tbot')
       index_a2x_Sa_ptem       = mct_avect_indexra(a2x_a,'Sa_ptem')
       index_a2x_Sa_pbot       = mct_avect_indexra(a2x_a,'Sa_pbot')
       index_a2x_Sa_pslv       = mct_avect_indexra(a2x_a,'Sa_pslv')
       index_a2x_Sa_shum       = mct_avect_indexra(a2x_a,'Sa_shum')
       index_a2x_Sa_dens       = mct_avect_indexra(a2x_a,'Sa_dens')
       index_a2x_Faxa_swnet    = mct_avect_indexra(a2x_a,'Faxa_swnet')
       index_a2x_Faxa_lwdn     = mct_avect_indexra(a2x_a,'Faxa_lwdn')
       index_a2x_Faxa_rainc    = mct_avect_indexra(a2x_a,'Faxa_rainc')
       index_a2x_Faxa_rainl    = mct_avect_indexra(a2x_a,'Faxa_rainl')
       index_a2x_Faxa_snowc    = mct_avect_indexra(a2x_a,'Faxa_snowc')
       index_a2x_Faxa_snowl    = mct_avect_indexra(a2x_a,'Faxa_snowl')
       index_a2x_Faxa_swndr    = mct_avect_indexra(a2x_a,'Faxa_swndr')
       index_a2x_Faxa_swvdr    = mct_avect_indexra(a2x_a,'Faxa_swvdr')
       index_a2x_Faxa_swndf    = mct_avect_indexra(a2x_a,'Faxa_swndf')
       index_a2x_Faxa_swvdf    = mct_avect_indexra(a2x_a,'Faxa_swvdf')
       index_a2x_Faxa_bcphidry = mct_avect_indexra(a2x_a,'Faxa_bcphidry')
       index_a2x_Faxa_bcphodry = mct_avect_indexra(a2x_a,'Faxa_bcphodry')
       index_a2x_Faxa_bcphiwet = mct_avect_indexra(a2x_a,'Faxa_bcphiwet')
       index_a2x_Faxa_ocphidry = mct_avect_indexra(a2x_a,'Faxa_ocphidry')
       index_a2x_Faxa_ocphodry = mct_avect_indexra(a2x_a,'Faxa_ocphodry')
       index_a2x_Faxa_ocphiwet = mct_avect_indexra(a2x_a,'Faxa_ocphiwet')
       index_a2x_Faxa_dstdry1  = mct_avect_indexra(a2x_a,'Faxa_dstdry1')
       index_a2x_Faxa_dstdry2  = mct_avect_indexra(a2x_a,'Faxa_dstdry2')
       index_a2x_Faxa_dstdry3  = mct_avect_indexra(a2x_a,'Faxa_dstdry3')
       index_a2x_Faxa_dstdry4  = mct_avect_indexra(a2x_a,'Faxa_dstdry4')
       index_a2x_Faxa_dstwet1  = mct_avect_indexra(a2x_a,'Faxa_dstwet1')
       index_a2x_Faxa_dstwet2  = mct_avect_indexra(a2x_a,'Faxa_dstwet2')
       index_a2x_Faxa_dstwet3  = mct_avect_indexra(a2x_a,'Faxa_dstwet3')
       index_a2x_Faxa_dstwet4  = mct_avect_indexra(a2x_a,'Faxa_dstwet4')
       index_a2x_Sa_co2prog    = mct_avect_indexra(a2x_a,'Sa_co2prog',perrWith='quiet')
       index_a2x_Sa_co2diag    = mct_avect_indexra(a2x_a,'Sa_co2diag',perrWith='quiet')

!
! Initialize averaging counter
!
       avg_count = 0
!
! Create initial atm export state
!
       call atm_export_mct( head_grid, a2x_a )
!
! Set flag to specify that an extra albedo calculation is to be done (i.e. specify active)
!		
        
       plon=ide-ids  !for global domain
       plat=jde-jds  !for global domain
           
       call seq_infodata_PutData(infodata, atm_prognostic=.true.)
       call seq_infodata_PutData(infodata, atm_nx=plon, atm_ny=plat)
       ! Set flag to indicate that CAM will provide carbon and dust deposition fluxes.
       ! This is now hardcoded to .true. since the ability of CICE/CLM to read these
       ! fluxes from a file has been removed.
       call seq_infodata_PutData(infodata, atm_aero=.true.)
!
! Set time step of radiation computation as the current calday
! This will only be used on the first timestep of an initial run
!

       write(iulog,*)'plon=',plon,mytask 
       write(iulog,*)'plat=',plat,mytask
       write(iulog,*)'nextsw_cday = ', nextsw_cday
         
       call seq_infodata_PutData( infodata, nextsw_cday=nextsw_cday )
       
       write(iulog,*) '******local comunicator*******',local_communicator
	  
       call shr_file_setLogUnit (shrlogunit)
       call shr_file_setLogLevel(shrloglev)

    elseif (phase == 2) then
       
! For initial run, nothing wnated to read
! For restart run, read restart x2a_a

!  obtain the begin date of CCSM again
       write(iulog,*) subname,' phase 2 starting'
       call shr_sys_flush(iulog)

       call shr_file_getLogUnit (shrlogunit)
       call shr_file_getLogLevel(shrloglev)
       call shr_file_setLogUnit (iulog)
       
!       write(iulog,*) '******local comunicator*******',local_communicator
       call seq_timemgr_EClockGetData(EClock,curr_ymd=CurrentYMD, StepNo=StepNo, dtime=DTime_Sync ) 
       call domain_clock_get( head_grid, current_time=current_time)
       call WRFU_TimeGet( current_time, yy=yr,mm=mon,dd=day)   
       ymd=yr*10000+mon*100+day
       if( currentymd.ne.ymd) then
          write(iulog,*) 'ymd=',ymd,'currentymd=',currentymd
          write(iulog,*)'WRF begintime must be the same with CCSM'
          call shr_sys_abort()
       endif
   
!  determine whether be restart       
       call nl_get_restart(1,restart)
       if (.NOT.restart) then       
          tmpgrid=>tmpgrid1
          tmpgrid=head_grid
          write(iulog,*)'wrf: calling import' 	      
          call atm_import_mct( x2a_a, tmpgrid )
          write(iulog,*)'wrf: calling integrate' 
          write(iulog,*) associated(tmpgrid,head_grid)
          call integrate ( tmpgrid,.true. )
          write(iulog,*)'wrf: back from integrate'
          call atm_export_mct( tmpgrid, a2x_a )
          nullify(tmpgrid)
          write(iulog,*)'wrf: export finish'           
       else
          call atm_export_mct( head_grid, a2x_a )
       end if

! compute day of next radiation computation, assume wrf and rad 
! timestep divide a year evenly

       ! need rad freq, wrf timestep, coupling period
       nradt = config_flags%radt*60
       dtime = head_grid%dt
       call seq_timemgr_EClockGetData(Eclock,dtime=atm_cpl_dt)

       ! get current day of year
       call domain_clock_get( head_grid, current_time=current_time)
       call WRFU_TimeGet( current_time, dayOfYear_r8=dayofYear)

       ! compute wrf time of year in seconds
       wrfsec = nint(dayofYear*86400.0_r8)

       ! compute a wrf radiation time (either past or next)
       radsec = int(wrfsec/nradt)*nradt + dtime

       ! if radsec is in past, compute next radsec
       if (wrfsec >= radsec) radsec = radsec + nradt
       if (wrfsec >= radsec) call shr_sys_abort('wrf_init nextsw1')

       ! set nextsw_cday if the next coupling period is a radiation ts
       if ((wrfsec + atm_cpl_dt) >= radsec .or. .not.restart) then

         ! This line changed by Andrew Roberts to sync WRF and CPL sun position
         ! nextsw_cday = mod(real(wrfsec,r8)/86400.0_r8-1.0_r8,365.0_r8)+1.0_r8
         nextsw_cday = mod(real(wrfsec+0.5*nradt,r8)/86400.0_r8-1.0_r8,365.0_r8)+1.0_r8

       else

         nextsw_cday = -1._r8

       endif

       call seq_infodata_PutData( infodata, nextsw_cday=nextsw_cday )

! check timestep and couple interval consistency
       
       dtime = head_grid%dt
!       if (atm_cpl_dt*1.0/dtime-int(atm_cpl_dt/dtime) .ne. 0.0) then
!	 write(iulog,*)atm_cpl_dt*1.0/dtime,atm_cpl_dt,dtime
       if (abs(atm_cpl_dt*1.0/dtime-int(atm_cpl_dt/dtime)) > 1.0e-5) then
          write(iulog,*) atm_cpl_dt*1.0/dtime,atm_cpl_dt,dtime, &
              (atm_cpl_dt*1.0/dtime-int(atm_cpl_dt/dtime)) 
          write(iulog,*)'atm_cpl_dt must be integer times of time step'
          call shr_sys_abort()
       endif
       if (dtime > atm_cpl_dt) then
          write(iulog,*)'timestep must be less than or equal to atm_cpl_dt'
 	  call shr_sys_abort()
       end if 

    else    ! phase
       write(iulog,*)'phase invalid',phase
       call shr_sys_abort()

    end if  ! phase

    call shr_file_setLogUnit (shrlogunit)
    call shr_file_setLogLevel(shrloglev)
     
   END SUBROUTINE atm_init_mct

   SUBROUTINE atm_run_mct(EClock, cdata_a, x2a_a, a2x_a )
!<DESCRIPTION>
!     WRF run routine.
!</DESCRIPTION>

!<DESCRIPTION>
! Once the top-level domain has been allocated, configured, and
! initialized, the model time integration is ready to proceed.  The start
! and stop times for the domain are set to the start and stop time of the
! model run, and then <a href=integrate.html>integrate</a> is called to
! advance the domain forward through that specified time interval.  On
! return, the simulation is completed.
!
!</DESCRIPTION>

!  The forecast integration for the most coarse grid is now started.  The
!  integration is from the first step (1) to the last step of the simulation.
!-----------------------------------------------------------------------
!
! Arguments
!
    type(CCSM_Clock)            ,intent(in)    :: EClock
    type(seq_cdata)             ,intent(inout) :: cdata_a
    type(mct_aVect)             ,intent(inout) :: x2a_a
    type(mct_aVect)             ,intent(inout) :: a2x_a
!
! Local variables
!
    type(seq_infodata_type),pointer :: infodata
    integer :: lsize           ! size of attribute vector
    integer :: DTime_Sync      ! integer timestep size
    integer :: iradsw          ! shortwave radation frequency (time steps)
    logical :: dosend          ! true => send data back to driver
    integer :: dtime           ! time step increment (sec)
    integer :: atm_cpl_dt      ! driver atm coupling time step
    integer :: ymd_sync        ! Sync date (YYYYMMDD)
    integer :: yr_sync         ! Sync current year
    integer :: mon_sync        ! Sync current month
    integer :: day_sync        ! Sync current day
    integer :: tod_sync        ! Sync current time of day (sec)

! This line removed and placed in module_ra_rasm by Andrew Roberts
!    real(r8):: nextsw_cday     ! calendar of next atm shortwave

    logical :: rstwr_sync      ! .true. ==> write restart file before returning
    logical :: nlend_sync      ! Flag signaling last time-step
    logical :: first_time = .true.    
    real(r8):: dayofYear        ! day of year
    integer :: wrfsec           ! wrf time in seconds
    integer :: radsec           ! radiation time in seconds
    integer :: nradt
    
    TYPE(WRFU_Time) :: current_time
    type(WRFU_TimeInterval) :: off
    integer :: ymd             ! WRF current date (YYYYMMDD)
    integer :: yr              ! WRF current year
    integer :: mon             ! WRF current month
    integer :: day             ! WRF current day
    integer :: hour             ! WRF current hour
    integer :: minuate            ! WRF current minuate
    integer :: second             ! WRF current second
    integer :: tod             ! WRF current time of day (sec)
    integer :: nstep           ! WRF nstep

    integer :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,&
		  i,j,ig
    TYPE(domain) , POINTER :: tmpgrid	  ! for the temporary integration
    TYPE(domain) , target :: tmpgrid1	  ! for the temporary integration
    real(r8)::tv
    real :: st
    character(len=*), parameter :: subname = "atm_run_mct"
    
!-----------------------------------------------------------------------


! Note that sync clock time should match wrf time at end of time step/loop not beginning

    call seq_cdata_setptrs(cdata_a, infodata=infodata)
    call seq_timemgr_EClockGetData(EClock,curr_ymd=ymd_sync,curr_tod=tod_sync, &
            curr_yr=yr_sync,curr_mon=mon_sync,curr_day=day_sync)

    nlend_sync = seq_timemgr_StopAlarmIsOn(EClock)
    rstwr_sync = seq_timemgr_RestartAlarmIsOn(EClock)

! Added Andrew Roberts June 2013, load orbital parameters from CPL
    call seq_infodata_GetData( infodata, &
       orb_eccen=eccen, orb_mvelpp=mvelpp, orb_lambm0=lambm0, orb_obliqr=obliqr)

! Map input from mct to wrf data structure

    call atm_import_mct(x2a_a, head_grid )
    
! Cycle over all time steps in a wrf radiation coupling interval

    dosend = .false.
    do while (.not. dosend)
!        write(iulog,*)  'wrf: calling integrate' 
!       call atm_sfcclay_mct(head_grid)
       call domain_clock_get(head_grid, current_time=current_time)
       call WRFU_timeget(current_time, yy=yr, mm=mon, dd=day, h=hour, m=minuate, s=second)
       ymd = yr*10000 + mon*100 + day
       tod = hour*3600+minuate*60+second
!       write(iulog,*) 'b4 integrate',ymd,tod

! Determine if time to write wrf restart and stop
! Need to check time at end of next timestep

       call domain_clock_get(head_grid, current_time=current_time)
       dtime = head_grid%dt
       call WRFU_TimeIntervalSet( off, s=dtime)
       current_time = current_time + off
       call WRFU_timeget(current_time, yy=yr, mm=mon, dd=day, h=hour, m=minuate, s=second)
       ymd = yr*10000 + mon*100 + day
       tod = hour*3600+minuate*60+second
       dosend = (seq_timemgr_EClockDateInSync( EClock, ymd, tod))

       RESTART_NOW = .false.
       if (rstwr_sync .and. dosend) RESTART_NOW = .true.
       END_NOW = .false.
       if (nlend_sync .and. dosend) END_NOW = .true.

!       if (dosend) write(iulog,*) 'b4 integrate dosend ',ymd,tod,dosend,RESTART_NOW

       call integrate ( head_grid,.false. )

!      write(iulog,*)  'wrf: back from integrate' 
! Determine if dosend
! When time is not updated at the end of the loop - then return only if
! are in sync with clock before time is updated

       call domain_clock_get(head_grid, current_time=current_time)
       call WRFU_timeget(current_time, yy=yr, mm=mon, dd=day, h=hour, m=minuate, s=second)
       ymd = yr*10000 + mon*100 + day
       tod = hour*3600+minuate*60+second
       dosend = (seq_timemgr_EClockDateInSync( EClock, ymd, tod))
!       if (dosend) write(iulog,*) 'af integrate dosend ',ymd,tod,dosend
       
! Map output from wrf to mct data structures

       call atm_export_mct( head_grid, a2x_a )
       
! Compute snapshot attribute vector for accumulation

! don't accumulate on first coupling freq ts1 and ts2
! for consistency with ccsm3 when flxave is off       
       nstep = head_grid%itimestep        
       if (nstep <= 2) then
          call mct_aVect_copy( a2x_a, a2x_a_SUM )
          avg_count = 1
       else
          call mct_aVect_copy( a2x_a, a2x_a_SNAP )
          call mct_aVect_accum( aVin=a2x_a_SNAP, aVout=a2x_a_SUM )
          avg_count = avg_count + 1
       endif

    end do

! Finish accumulation of attribute vector and average and copy accumulation
! field into output attribute vector

    call mct_aVect_avg ( a2x_a_SUM, avg_count)
    call mct_aVect_copy( a2x_a_SUM, a2x_a )
    call mct_aVect_zero( a2x_a_SUM)
    avg_count = 0

! Get time of next radiation calculation - albedos will need to be
! calculated by each surface model at this time

! compute day of next radiation computation, assume wrf and rad 
! timestep divide a year evenly

       ! need rad freq, wrf timestep, coupling period
       nradt = config_flags%radt*60
       dtime = head_grid%dt
       call seq_timemgr_EClockGetData(Eclock,dtime=atm_cpl_dt)

       ! get current day of year
       call domain_clock_get( head_grid, current_time=current_time)
       call WRFU_TimeGet( current_time, dayOfYear_r8=dayofYear)

       ! compute wrf time of year in seconds
       wrfsec = nint(dayofYear*86400.0_r8)

       ! compute a wrf radiation time (either past or next)
       radsec = int(wrfsec/nradt)*nradt + dtime

       ! if radsec is in past, compute next radsec
       if (wrfsec >= radsec) radsec = radsec + nradt
       if (wrfsec >= radsec) call shr_sys_abort('wrf_init nextsw1')

       ! set nextsw_cday if the next coupling period is a radiation ts
       if ((wrfsec + atm_cpl_dt) >= radsec) then

        ! This line changed by Andrew Roberts to sync WRF and CPL sun position
        ! nextsw_cday = mod(real(wrfsec,r8)/86400.0_r8-1.0_r8,365.0_r8)+1.0_r8
        nextsw_cday = mod(real(wrfsec+0.5*nradt,r8)/86400.0_r8-1.0_r8,365.0_r8)+1.0_r8

       else

        nextsw_cday = -1._r8

       endif

!       write(iulog,*) 'tcx nextsw1 ',nradt,dtime,atm_cpl_dt
!       write(iulog,*) 'tcx nextsw2 ',dayofYear,wrfsec,radsec,nextsw_cday

       call seq_infodata_PutData( infodata, nextsw_cday=nextsw_cday )

! Write merged surface data restart file if appropriate

!    if (rstwr_sync) then
!       call atm_write_srfrest_mct( head_grid,  config_flags)
!    end if

! Check for consistency of internal wrf clock with master sync clock

    call domain_clock_get(head_grid, current_time=current_time)    
    call WRFU_timeget(current_time, yy=yr, mm=mon, dd=day, h=hour, m=minuate, s=second)
    ymd = yr*10000 + mon*100 + day
    tod = hour*3600+minuate*60+second
       
    if ( .not. seq_timemgr_EClockDateInSync( EClock, ymd, tod ) )then
       call seq_timemgr_EClockGetData(EClock, curr_ymd=ymd_sync, curr_tod=tod_sync )
       write(iulog,*)  ' wrf ymd=',ymd     ,'  wrf tod= ',tod 
       write(iulog,*)  ' sync ymd=',ymd_sync,' sync tod= ',tod_sync
       call shr_sys_abort()
    end if

   END SUBROUTINE atm_run_mct

   subroutine atm_final_mct( EClock, cdata_a, x2a_a, a2x_a)
!<DESCRIPTION>
!     WRF finalize routine.
!</DESCRIPTION>

!<DESCRIPTION>
! A Mediation Layer-provided
! subroutine, <a href=med_shutdown_io.html>med_shutdown_io</a> is called
! to allow the the model to do any I/O specific cleanup and shutdown, and
! then the WRF Driver Layer routine <a
! href=wrf_shutdown.html>wrf_shutdown</a> (quilt servers would be
! directed to shut down here) is called to properly end the run,
! including shutting down the communications (for example, most comm
! layers would call MPI_FINALIZE at this point if they're using MPI).
!
!</DESCRIPTION>

    type(CCSM_Clock)            ,intent(in)    :: EClock
    type(seq_cdata)             ,intent(inout) :: cdata_a
    type(mct_aVect)             ,intent(inout) :: x2a_a
    type(mct_aVect)             ,intent(inout) :: a2x_a

    character(len=*), parameter :: subname = "atm_final_mct"

    deallocate(ragrid%tsk)
    deallocate(ragrid%rasdir)
    deallocate(ragrid%raldir)
    deallocate(ragrid%rasdif)
    deallocate(ragrid%raldif)

   ! shut down I/O
   CALL med_shutdown_io ( head_grid , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: back from med_shutdown_io' )

   CALL       wrf_debug (   0 , 'wrf: SUCCESS COMPLETE WRF' )

   ! Call wrf_shutdown() (which calls MPI_FINALIZE()
   ! for DM parallel runs).

   END SUBROUTINE atm_final_mct

!================================================================================

    subroutine atm_SetgsMap_mct( grid, mpicom_atm, ATMID, GSMap_atm )

!-------------------------------------------------------------------
!
! Arguments
!
    TYPE(domain) , POINTER :: grid
    integer        , intent(in)  :: mpicom_atm
    integer        , intent(in)  :: ATMID
    type(mct_gsMap), intent(out) :: GSMap_atm
!
! Local variables
!
    integer, allocatable :: gindex(:)
    integer :: i, j, k, n,lsize,gsize
    integer :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe
    integer :: ier            ! error status
!-------------------------------------------------------------------
! Build the atmosphere grid numbering for MCT
! NOTE:  Numbering scheme is: West to East and South to North
! starting at south pole.  Should be the same as what's used in SCRIP
! Determine global seg map
    call get_ijk_from_grid (  grid ,                   &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )
    if(jpe.eq.jde) jpe=jpe-1
    if(ipe.eq.ide) ipe=ipe-1
			      
    lsize=0
    do j=jps, jpe             
       do i=ips,ipe
             lsize = lsize+1  !local index
       end do
    end do

!    gsize=(ide-ids+1)*(jde-jds+1)
    gsize=(ide-ids)*(jde-jds)
    allocate(gindex(lsize))

    n=0
    do j=jps, jpe             
       do i=ips,ipe
          n=n+1
!          gindex(n) = (j-1)*(ide-ids+1)+i  ! global index
          gindex(n) = (j-1)*(ide-ids)+i  ! global index
       end do
    end do

    call mct_gsMap_init( gsMap_atm, gindex, mpicom_atm, ATMID, lsize, gsize)

    deallocate(gindex)

  end subroutine atm_SetgsMap_mct

!===============================================================================

  subroutine atm_import_mct( x2a_a, grid )

!-----------------------------------------------------------------------
!
! Arguments
!
    TYPE(domain) , POINTER:: grid
    type(mct_aVect)   , intent(inout) :: x2a_a

!
! Local variables
!		
    integer  :: mype            ! processor ID
    integer  :: m             ! indices
    integer  :: i,j,k,ig  ! indices    
    integer :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe    
    logical :: intp
    logical, save :: first_time=.true.
    real :: tau, taux, tauy, logz0   ! for ustar and roughness length calcs
    real :: avdr, andr, avdf, andf, svdr, sndr, svdf, sndf, nswi ! for albedo calc
    real(r8):: rhowater
    character(len=*), parameter :: subname = "atm_import_mct"
!-----------------------------------------------------------------------
!
       call shr_file_getLogUnit (shrlogunit)
       call shr_file_getLogLevel(shrloglev)
       call shr_file_setLogUnit (iulog)
       
    call get_ijk_from_grid (  grid ,                   &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

!    write(iulog,*)local_communicator
      
    if(jpe.eq.jde) jpe=jpe-1
    if(ipe.eq.ide) ipe=ipe-1

!     write(iulog,*)ips,ipe,jps,jpe,kps,kpe
    ig=1
    do j=jps,jpe                                                        
       do i =ips,ipe  

rhowater = SHR_CONST_RHOFW                                                                 

! WRF expects latent heat, sensible heat, upward longwave, and upward moisture flux
! to be positive upward (reversed from what coupler expects). CPL7 expects fluxes to
! be positive downards. Therefore, must multiply by -1 for these fluxes:
!
! surface latent heat flux in W m-2 mapped to -1 * WRF's latent heat flux at surface in W m-2
          grid%lh(i,j)       = -x2a_a%rAttr(index_x2a_Faxx_lat, ig)

! surface sensible heat flux in W m-2 mapped to -1 * WRF's upwards heat flux at the surface in W m-2
          grid%hfx(i,j)       = -x2a_a%rAttr(index_x2a_Faxx_sen, ig)

! surface upwards longwave heat flux in W m-2 mapped to -1 * WRF's surface upward longwave 
! radiation in W m-2. lwups is not a field normally in WRF's Registry. Instead,
! this has been added to the WRF Registry and the WRF radiation module has been 
! modified to correctly utilize lwups.
          grid%lwups(i,j)      = -x2a_a%rAttr(index_x2a_Faxx_lwup,ig)

! evaporation water flux in kg m-2 s-1 mapped to -1 * WRF's upward moisture flux at the surface
! in kg m-2 s-1
          grid%qfx(i,j)    = -x2a_a%rAttr(index_x2a_Faxx_evap,ig)

! calculate ALBEDO and SWDOWN (shortwave down) diagnostic variables for WRF output.
! This needs to be done before the new albedos are loaded so that the net ALBEDO
! variable corresponds to the weighted average albedo using the spectral albedos used
! with the spectral shortwave components in the internal WRF calculations.
! Calculate albedo by a weighted combination of the spectral shortwave values from 
! WRF and spectral albedos from the coupler. SWDOWN is just the sum of the four 
! shortwave components down (incident) at the surface.
! a) get incoming spectral albedos from coupler
          avdr =  grid%rasdir(i,j)*1.0_r8         ! visible direct albedo
          andr =  grid%raldir(i,j)*1.0_r8         ! near-IR direct albedo
          avdf =  grid%rasdif(i,j)*1.0_r8         ! visible diffuse albedo
          andf =  grid%raldif(i,j)*1.0_r8         ! near-IR diffuse albedo

! b) get the spectral downwelling shortwave from WRF and net radiation
          svdr =  grid%swvdr(i,j)*1.0_r8          ! visible direct incident radiation
          sndr =  grid%swndr(i,j)*1.0_r8          ! near-IR direct incident radiation
          svdf =  grid%swvdf(i,j)*1.0_r8          ! visible diffuse incident radiation   
          sndf =  grid%swndf(i,j)*1.0_r8          ! near-IR diffuse incident radiation  
          nswi =  svdr + sndr + svdf + sndf       ! net shortwave incident radiation 
          grid%swdown(i,j) = nswi                 ! write total shortwave down WRF variable

! c) calculate ALBEDO using spectral components.Make sure to account for when there is
!    no sunlight (nswi ~ 0) because the model will blow up otherwise.
          if(nswi .lt. 1.0e-3) then               ! nighttime - threshold from module_surface_driver.F
          grid%albedo(i,j) = -100.0               ! set a fill value if no shortwave
          else
          grid%albedo(i,j) = (avdr*(svdr/nswi))+(andr*(sndr/nswi))+(avdf*(svdf/nswi))+(andf*(sndf/nswi))
          end if

! Load the new spectral albedos from the coupler.
! The following four albedo terms are not part of stand-alone WRF and have been
! added to the WRF Regsitry. The WRF radiation module has been modified to correctly
! utilize the terms.
! direct albedo (visible radiation); unitless
          grid%rasdir(i,j)     =  x2a_a%rAttr(index_x2a_Sx_avsdr, ig)

! direct albedo (near-infrared radiation); unitless
          grid%raldir(i,j)     =  x2a_a%rAttr(index_x2a_Sx_anidr, ig)

! diffuse albedo (visible radiation); unitless 
          grid%rasdif(i,j)     =  x2a_a%rAttr(index_x2a_Sx_avsdf, ig)

! diffuse albedo (near-infrared radiation); unitless
          grid%raldif(i,j)     =  x2a_a%rAttr(index_x2a_Sx_anidf, ig)

! snow water equivalent over land imported from coupler in m and immediately 
! converted to kg/m2 as WRF expects snow water equiv to be in kg/m2. Note that 
! the coupler field description implies this field is physical snow depth. However,
! according to Tony Craig, it is snow water equivalent.
          grid%snow(i,j) =  rhowater*x2a_a%rAttr(index_x2a_Sl_snowh, ig)

! Note that, despite being a coupler field, x2a_si_snowh (snow on ice) is NOT 
! used by WRF (or CAM). Therefore, that field is not imported.
     
! reference temperature at 2 m in K mapped to WRF's temperature at 2 m in K
          grid%t2(i,j)      =  x2a_a%rAttr(index_x2a_Sx_tref,  ig)

! reference specific humidity at 2 m in kg/kg mapped to WRF's specific humidity at 2 m in kg/kg
          grid%q2(i,j)      =  x2a_a%rAttr(index_x2a_Sx_qref,  ig)

! surface roughness length in m
! The roughness length that is imported from the coupler is an area weighted average of the 
! natural log of the roughness lengths for land/ice/ocean for each grid cell. The natural 
! log of roughness length is imported because fluxes depend on ln(z0), not z0 directly, so
! to properly back out stability using fluxes and roughness we need the roughness and flux values
! to be consistent. Here we unpack the ln(z0) value and name it the WRF variable that is used in 
! surface layer calculations.
!  a) get incoming ln(z0) value:
          logz0      =  x2a_a%rAttr(index_x2a_Sx_logz0,  ig)
!  b) use exponential to get just z0:
          grid%znt(i,j) = exp(logz0)

! surface ice fraction mapped to WRF's ice fraction
          grid%xice(i,j)   =  x2a_a%rAttr(index_x2a_Sf_ifrac, ig)

! surface land fraction mapped to WRF's land mask. In WRF, 1 is land and 2 is water
          grid%xland(i,j)  =  2-x2a_a%rAttr(index_x2a_Sf_lfrac, ig)


! zonal and meridional surface stress in N/m2
! WRF does not need these fields (x2a_Faxx_taux and x2a_Faxx_tauy) as long as it has u*
! Therefore, these fields are not imported. Previous versions of RACM imported these 
! stresses for the sole purpose of calculating u*.

! friction velocity (u*) in m/s
! u* is calculated from input values of wind stress, rather than the input values of u*. This
! is because while both the land model and the ocean model output u*, the ice model does
! not. Wind stresses, however, are combined by the coupler from all three components. Perhaps
! a future version of CPL7 will output a combined u* or at least a u* from the ice model. 
! Until then, calculate u* here for use by WRF:
!
! a) get incoming wind stresses:
          taux = -x2a_a%rAttr(index_x2a_Faxx_taux,ig)     
          tauy = -x2a_a%rAttr(index_x2a_Faxx_tauy,ig) 

! b) calculate stress from component vectors. Note: we do not rotate incoming vectors to
! WRF grid because all we are interested in is the magnitude of the stress
          tau = sqrt(taux*taux + tauy*tauy)

! c) calculate u* as sqrt (tau / rho)
! rho is taken as density at lowest model level
          grid%ust(i,j) = sqrt( tau / (1.0/grid%alt(i,1,j)*(1+grid%moist(i,1,j,P_QV))))

! surface temperature mapped to WRF's surface skin temperature, both in K
          grid%tsk(i,j)       =  x2a_a%rAttr(index_x2a_Sx_t,ig)

! sea surface temperature mapped to WRF's sea surface skin temperature, both in K
          grid%sst(i,j)       =  x2a_a%rAttr(index_x2a_So_t,ig)

! put WRF's imported sea surface temperature into skin temp variable if we are over
! an ocean and do not have a value for skin temp
          if(grid%tsk(i,j).eq.0.and.grid%xland(i,j).eq.2) grid%tsk(i,j)=grid%sst(i,j) 

! Fix several defaults for snow cover and energy fluxes; CPL7 default is 1.0e30 and this
! will cause problems for WRF. To account for rounding errors in the coupler value, the
! calculation uses a normalized absolute value of a difference
          if(abs((grid%snow(i,j)/1.0e30)-1.0) .lt. 1.0e-12) grid%snow(i,j)=0.0 
          if(abs((grid%hfx(i,j)/1.0e30)-1.0) .lt. 1.0e-12)  grid%hfx(i,j)=0.0
          if(abs((grid%lh(i,j)/1.0e30)-1.0) .lt. 1.0e-12)   grid%lh(i,j)=0.0
          if(abs((grid%qfx(i,j)/1.0e30)-1.0) .lt. 1.0e-12)  grid%qfx(i,j)=0.0

          ig=ig+1

       end do
      end do
      
      if (grid%itimestep.eq.0.and.first_time) then     
         do j=jps,jpe	
            do i=ips,ipe	
               grid%lwups(i,j) = 5.67e-8*(grid%tsk(i,j)**4)
               grid%pblh(i,j)= 2000.0 ! need an initial value
               grid%ust(i,j) = 0.3    ! need an initial value
               grid%znt(i,j) = 0.01   ! need an initial value
               first_time=.false.
            enddo
         enddo     
      end if  

      call shr_file_setLogUnit (shrlogunit)
      call shr_file_setLogLevel(shrloglev)
     
  end subroutine atm_import_mct

!===============================================================================

  subroutine atm_export_mct( grid, a2x_a )
  
  USE module_state_description
!-------------------------------------------------------------------
!
! Arguments
!
    TYPE(domain) , POINTER:: grid
    type(mct_aVect)    , intent(out) :: a2x_a
!
! Local variables
!
    real :: u_phy,v_phy
    real :: earth_u, earth_v
    real(r8)::z,tv,rd,g,cp,xlapse,pi,rad_per_deg,alpha,tstar,tt0,alph,beta,psfc,pslv,p
    integer :: i,j,k,ig       ! indices    
    integer :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe
    integer :: xtime_week
    real :: uv_factor
    real :: xtime_week_rl
    character (256) :: msg
    character(len=*), parameter :: subname = "atm_export_mct"
!-----------------------------------------------------------------------
! Copy from component arrays into chunk array data structure
! Rearrange data from chunk structure into lat-lon buffer and subsequently
! create attribute vector
    rd=SHR_CONST_RDAIR
    g=SHR_CONST_G
    cp=SHR_CONST_CPDAIR
    xlapse= 6.5e-3_r8
    pi = SHR_CONST_PI
    rad_per_deg = pi/180.

    call shr_file_getLogUnit (shrlogunit)
    call shr_file_getLogLevel(shrloglev)
    call shr_file_setLogUnit (iulog)

    call get_ijk_from_grid (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe    )

#ifdef DM_PARALLEL
#     include "HALO_EM_PHYS_A.inc"
#endif	
!    write(iulog,*) local_communicator 
    if(jpe.eq.jde) jpe=jpe-1
    if(ipe.eq.ide) ipe=ipe-1

    ig=1
    do j=jps, jpe             
       do i=ips,ipe 

! Sea level pressure, Pa
! First, calculate virtual temperature at lowest model level in K.
! Note that the model potential temperature (t_2) is a perturbation potential temperature and must have
! a base state potential temperature (t0) added to it. Note also that the total pressure is the perturbation
! pressure (p) plus the base pressure (pb). To convert to temperature we need the pressure at 1000mb (p0).
           tv=(grid%t_2(i,1,j)+t0)*&  ! This is potential tempearture at lowest model level.
               (((grid%p(i,1,j) + grid%pb(i,1,j))/p0)**(rd/cp))*& ! mult by inverted Poisson's equation
               (1+grid%moist(i,1,j,P_QV)*0.61)*1.0_r8       ! multiply by (1+0.61*mixing ratio) to get virtual temp

! Calculate geopotential at surface in m2 s-2
         z=grid%ht(i,j) *g ! terrain height in m
         alpha = rd*xlapse/g ! 0.1903, no units

! Get surface pressure in Pa
         psfc= grid%psfc(i,j)*1.0_r8

! Calculate pressure in WRF at lowest model level in Pa
! Note: this is the perturbation pressure (p) plus the base pressure (pb)
         p=(grid%p(i,1,j) + grid%pb(i,1,j))*1.0_r8

! Use surface pressure for SLP if we are at sea level.
         if ( abs(z/g) < 1.e-4_r8 )then
            pslv=psfc
! Othewise, calculate based on ECMWF method (modified from vintp2p_ecmwf.F)
         else
            tstar=tv*(1._r8+alpha*(psfc/p-1._r8)) 
            tt0=tstar + xlapse*z/g

            if ( tstar<=290.5_r8 .and. tt0>290.5_r8 ) then     
               alph=rd/z*(290.5_r8-tstar)  
            else if (tstar>290.5_r8  .and. tt0>290.5_r8) then  
               alph=0._r8
               tstar= 0.5_r8 * (290.5_r8 + tstar)  
            else  
               alph=alpha  
               if (tstar<255._r8) then  
                  tstar= 0.5_r8 * (255._r8 + tstar)             
               endif
            endif
            beta = z/(rd*tstar)
            pslv=psfc*exp( beta*(1._r8-alph*beta/2._r8+((alph*beta)**2)/3._r8))
         end if
      
! Finally, output sea level pressure to coupler.
         tv=pslv  
         a2x_a%rAttr(index_a2x_Sa_pslv   ,ig) = tv 

! bottom atm level height (depth), m
         tv=(grid%phb(i,1,j)+grid%ph_2(i,1,j)+grid%phb(i,2,j)+grid%ph_2(i,2,j))*0.5_r8/g-z/g
         a2x_a%rAttr(index_a2x_Sa_z      ,ig) = tv 

! zonal wind at lowest model level, m/s
! meridional wind at lowest model level, m/s
! First, get winds from WRF at lowest model level
         u_phy=(grid%u_2(i,1,j)+grid%u_2(i+1,1,j))/2.0
         v_phy=(grid%v_2(i,1,j)+grid%v_2(i,1,j+1))/2.0
! Get difference between grid center longitudes and wrf projection stand_lon
         tv = (grid%xlong(i,j)-config_flags%stand_lon)*rad_per_deg
! Now rotate winds from WRF grid to true grid
! Note: this rotation is appropriate for isotropic grids only. Anisotropic
! grids would need to take the map factors into consideration for the rotation.
         earth_u = (u_phy*cos(tv)) + (v_phy*sin(tv))
         earth_v = (v_phy*cos(tv)) - (u_phy*sin(tv))

! Output rotated winds to coupler.
! U
         tv=earth_u*1.0_r8
         a2x_a%rAttr(index_a2x_Sa_u      ,ig) = tv
! V
         tv=earth_v*1.0_r8
         a2x_a%rAttr(index_a2x_Sa_v      ,ig) = tv

! pressure at the lowest model level, Pa  
         u_phy=grid%pb(i,1,j) + grid%p(i,1,j)
         tv=u_phy*1.0_r8
         a2x_a%rAttr(index_a2x_Sa_pbot   ,ig) = tv 

! potential temperature at the lowest model level, K
! Note: the potential temperature is the perturbation potential temperature plus the base temperature
         u_phy=grid%t_2(i,1,j)+t0
         tv=u_phy*1.0_r8
         a2x_a%rAttr(index_a2x_Sa_ptem   ,ig) = tv 

! temperature at the lowest model level, K
! Note: this converts the model potential temperature to temperature. It requires the
! perturbation temperature (t_2), base temperature (t0), perturbation pressure (p)
! base pressure (pb) and pressure at 1000 mb (p0).
         u_phy=(grid%t_2(i,1,j)+t0)*((grid%pb(i,1,j) + grid%p(i,1,j))/p0)**(rd/cp)
         tv=u_phy*1.0_r8
         a2x_a%rAttr(index_a2x_Sa_tbot   ,ig) = tv

! air density at the lowest model level, kg/m3
! this includes moisture; density in WRF is dry air density
         if(grid%moist(i,1,j,P_QV).gt.0) then 
            u_phy=1.0/grid%alt(i,1,j)*(1+grid%moist(i,1,j,P_QV))
         else
            u_phy=1.0/grid%alt(i,1,j)
         endif
         tv=u_phy*1.0_r8      
         a2x_a%rAttr(index_a2x_Sa_dens   ,ig) =tv

! specific humidity at the lowest model level, kg/kg
! calculated from mixing ratio in WRF
         if(grid%moist(i,1,j,P_QV).gt.0) then
            u_phy=grid%moist(i,1,j,P_QV)/(1+grid%moist(i,1,j,P_QV)) 
         else
            u_phy=0.0
         endif 
         tv=u_phy*1.0_r8      
         a2x_a%rAttr(index_a2x_Sa_shum ,ig) = tv

! net shortwave radiation, W/m2
         tv=grid%gsw(i,j)*1.0_r8
         a2x_a%rAttr(index_a2x_Faxa_swnet,ig) = tv

! downward longwave heat flux, W/m2
         tv=grid%glw(i,j)*1.0_r8
         a2x_a%rAttr(index_a2x_Faxa_lwdn ,ig) = tv

! Precipitation must be passed to the coupler in flux units (kg m-2 s-1)
! so the wrf precip values given in mm must be converted using 1000mm/m and
! 1000 kg/m3 (which cancel out) and divided by the individual model timesteps
! which is set in the wrf namelist (time_step). We use the density for water 
! in all calculations because we're passing liquid water equivalent.

! convective precipitation rate in kg m-2 s-1
! Note that the above coupler description is very misleading. The coupler
! wants non-frozen convective precip here, not all precip. However,WRF's WRF's
! cumulus scheme (Grell, module_cu_gd.F specifically) does not output frozen
! and non-frozen convective precip seperately, and so as we are exporting
! to the coupler, this variable contains both frozen and unfrozen.
! To calculate, divide WRF's time-step cumulus precipitation in mm by the time step,   
! multiply by 1000 kg/m3 and divide by 1000 mm/m 
          tv=grid%raincv(i,j)/grid%dt*1.0_r8
          a2x_a%rAttr(index_a2x_Faxa_rainc,ig) = tv

! convective frozen precip rate (water equivalent) in kg m-2 s-1
! WRF's cumulus schemes do not output convective frozen precip variables at all, and
! there is no way to get at this information. So, we set this equal to zero here. Therefore,
! important to note that the convective non-frozen precip is actually frozen+non-frozen.
          tv = 0.*grid%dt*1.0_r8
          a2x_a%rAttr(index_a2x_Faxa_snowc,ig) = tv

! large scale (stable) precipitation rate in kg m-2 s-1               
! The coupler wants non-frozen, nonconvective precip, not total nonconvective precip.
! To calculate this, subtract the frozen nonconvective precip (snow and graupel only for 
! goddard, which doesn't calculate hail) from the total nonconvective precip. Then, divide by the 
! WRF time step,multiply by 1000 kg/m3 (want snow water equivalent, so use
! density of water, not density of snow) and divide by 1000 mm/m.  
! Note: rainncv and sr are both single timestep values. Because they're internally consistent it's okay
! to use them together in these calculations to partition liquid/ice precip. Then this code
! averages the values of liquid/ice precip every 20 minutes.

! Goddard precip calcs
!         tv=(grid%rainncv(i,j)-grid%snowncv(i,j)-grid%graupelncv(i,j)-grid%hailncv(i,j))/grid%dt*1.0_r8

! Morrison precip calcs
          grid%snowncv(i,j)=(grid%rainncv(i,j)+1.E-12)*grid%SR(i,j) ! Prep for Morrison Snow+Rain
          tv=(grid%rainncv(i,j)-grid%snowncv(i,j))/grid%dt*1.0_r8   ! Morrison rain

! Make sure there is no negative precip (conservation!?)
          a2x_a%rAttr(index_a2x_Faxa_rainl,ig) =  max(0.0_r8,tv)

! large-scale (stable) frozen precip rate (water equivalent) in kg m-2 s-1
! This is equal to the sum of the frozen non-convective precip terms. (snow, ice, and graupel
! in mm). Then divide by the time step, multiply by 1000 kg/m3 and divide by 1000 mm/m.

! Goddard frozen precip calcs
!          tv=(grid%snowncv(i,j)+grid%graupelncv(i,j)+grid%hailncv(i,j))/grid%dt*1.0_r8

! Morrison frozen precip calcs
          tv=grid%snowncv(i,j)/grid%dt*1.0_r8  ! Morrison snow

          a2x_a%rAttr(index_a2x_Faxa_snowl,ig) = tv

! The following four radiation fields are not normally part of WRF's Registry.
! Instead, the fields have been added to the WRF Registry and the WRF 
! radiation module has been modified to export these fields to the Registry
! so they may then be exported here to the coupler.

! direct near-infrared incident solar radiation in W/m2
          tv=grid%swndr(i,j)*1.0_r8
          a2x_a%rAttr(index_a2x_Faxa_swndr,ig) = tv

! direct visible incident solar radiation in W/m2
          tv=grid%swvdr(i,j)*1.0_r8
          a2x_a%rAttr(index_a2x_Faxa_swvdr,ig) = tv

! diffuse near-infrared incident solar radiation in W/m2
          tv=grid%swndf(i,j)*1.0_r8
          a2x_a%rAttr(index_a2x_Faxa_swndf,ig) = tv

! diffuse visible incident solar radiation in W/m2
          tv=grid%swvdf(i,j)*1.0_r8
          a2x_a%rAttr(index_a2x_Faxa_swvdf,ig) = tv

          ! aerosol deposition fluxes
          a2x_a%rAttr(index_a2x_Faxa_bcphidry,ig) = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_bcphodry,ig) = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_bcphiwet,ig) = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_ocphidry,ig) = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_ocphodry,ig) = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_ocphiwet,ig) = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_dstwet1,ig)  = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_dstdry1,ig)  = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_dstwet2,ig)  = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_dstdry2,ig)  = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_dstwet3,ig)  = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_dstdry3,ig)  = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_dstwet4,ig)  = 0.0_r8
          a2x_a%rAttr(index_a2x_Faxa_dstdry4,ig)  = 0.0_r8

          if (index_a2x_Sa_co2prog /= 0) then
             a2x_a%rAttr(index_a2x_Sa_co2prog,ig) = 3.55e-4_r8 ! atm prognostic co2
          end if
          if (index_a2x_Sa_co2diag /= 0) then
             a2x_a%rAttr(index_a2x_Sa_co2diag,ig) = 3.55e-4_r8 ! atm diagnostic co2
          end if
          ig=ig+1
       end do
    end do
       call shr_file_setLogUnit (shrlogunit)
       call shr_file_setLogLevel(shrloglev)    

  end subroutine atm_export_mct

!===============================================================================

  subroutine atm_domain_mct( grid, lsize, gsMap_a, dom_a )

!-------------------------------------------------------------------
! Arguments
!
    TYPE(domain) , POINTER:: grid
    integer        , intent(in)   :: lsize
    type(mct_gsMap), intent(in)   :: gsMap_a
    type(mct_ggrid), intent(inout):: dom_a    
!
! Local Variables
!
    integer  :: i,j,k,m,n           ! indices	
    integer :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe
    real(r8),dimension(:,:,:),allocatable :: ew_vert,ns_vert,xcenterlat,xcenterlon ! vertex of cell    
    real(r8),dimension(:,:),allocatable :: area          ! area in radians squared for each grid point
    real(r8), pointer  :: data(:)     ! temporary
    integer , pointer  :: idata(:)    ! temporary
    
      REAL :: lat, lon
		  
   real, parameter :: PI = SHR_CONST_PI
   real, parameter :: OMEGA_E = SHR_CONST_OMEGA ! Angular rotation rate of the earth

   real, parameter :: DEG_PER_RAD = 180./PI
   real, parameter :: RAD_PER_DEG = PI/180.
 
  real, parameter :: EARTH_RADIUS_M = SHR_CONST_REARTH   ! same as CESM  
!  real, parameter :: EARTH_RADIUS_M = 6370000.   ! same as MM5 system
    
!-------------------------------------------------------------------
! Initialize mct atm domain
!
    call mct_gGrid_init( GGrid=dom_a, CoordChars=trim(seq_flds_dom_coord), OtherChars=trim(seq_flds_dom_other), lsize=lsize )
!
! Allocate memory
!
    allocate(data(lsize))
!
! Initialize attribute vector with special value
!
    call mct_gsMap_orderedPoints(gsMap_a, mytask, idata)
    call mct_gGrid_importIAttr(dom_a,'GlobGridNum',idata,lsize)
!
! Determine domain (numbering scheme is: West to East and South to North to South pole)
! Initialize attribute vector with special value
!
    data(:) = -9999.0_R8 
    call mct_gGrid_importRAttr(dom_a,"lat"  ,data,lsize) 
    call mct_gGrid_importRAttr(dom_a,"lon"  ,data,lsize) 
    call mct_gGrid_importRAttr(dom_a,"area" ,data,lsize) 
    call mct_gGrid_importRAttr(dom_a,"aream",data,lsize) 
    data(:) = 0.0_R8     
    call mct_gGrid_importRAttr(dom_a,"mask" ,data,lsize) 
    data(:) = 1.0_R8
    call mct_gGrid_importRAttr(dom_a,"frac" ,data,lsize)

!
! Fill in correct values for domain components
!

    call get_ijk_from_grid (grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &    
			    ips, ipe, jps, jpe, kps, kpe    )
    
    if(jpe.eq.jde) jpe=jpe-1
    if(ipe.eq.ide) ipe=ipe-1
    m=2*(ide-ids)+3
    n=2*(jde-jds)+3
	
       allocate(area(ips:ipe,jps:jpe))
        allocate(xcenterlat(1:m,1:n,1:1))
        allocate(xcenterlon(1:m,1:n,1:1))
   	allocate(ew_vert(4,ids:ide,jds:jde))
	allocate(ns_vert(4,ids:ide,jds:jde))

! lat	
    n=0
    do j=jps, jpe             
       do i=ips,ipe
          n = n+1
          data(n) = grid%xlat(i,j)
       end do
    end do
    call mct_gGrid_importRAttr(dom_a,"lat",data,lsize) 

! lon
    n=0
    do j=jps, jpe             
       do i=ips,ipe
          n = n+1
          data(n) = grid%xlong(i,j)
       end do
    end do
    call mct_gGrid_importRAttr(dom_a,"lon",data,lsize) 

! area - Need to take into account the map factors while calculating area
! area in radians^2, and 2pi*rearth [km] = 2pi [radians]
    n=0
    do j=jps, jpe             
       do i=ips,ipe
          n = n+1
	  data(n) = ((grid%dx*grid%dy)/(grid%msftx(i,j)*grid%msfty(i,j)))/(EARTH_RADIUS_M*EARTH_RADIUS_M)
       end do
    end do
    call mct_gGrid_importRAttr(dom_a,"area",data,lsize)

! aream - assign this at the same time. The coupler will write over this 
    call mct_gGrid_importRAttr(dom_a,"aream",data,lsize)

! mask
    n=0
    do j=jps, jpe             
       do i=ips,ipe
          n = n+1
          data(n) = 1._r8 ! mask
       end do
    end do
    call mct_gGrid_importRAttr(dom_a,"mask"   ,data,lsize) 

! frac
    n=0
    do j=jps, jpe             
       do i=ips,ipe
          n = n+1
          data(n) = 1._r8 ! mask
       end do
    end do
    call mct_gGrid_importRAttr(dom_a,"frac"   ,data,lsize) 
    
	deallocate(data)
	deallocate(area)
	deallocate(xcenterlat)
	deallocate(xcenterlon)
	deallocate(ew_vert)
	deallocate(ns_vert)

  end subroutine atm_domain_mct
 
END MODULE atm_comp_mct
