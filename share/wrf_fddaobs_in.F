!WRF:MEDIATION_LAYER:IO
!  ---

! This obs-nudging FDDA module (RTFDDA) is developed by the 
! NCAR/RAL/NSAP (National Security Application Programs), under the 
! sponsorship of ATEC (Army Test and Evaluation Commands). ATEC is 
! acknowledged for releasing this capability for WRF community 
! research applications.
!
! The NCAR/RAL RTFDDA module was adapted, and significantly modified 
! from the obs-nudging module in the standard MM5V3.1 which was originally 
! developed by PSU (Stauffer and Seaman, 1994). 
! 
! Yubao Liu (NCAR/RAL): lead developer of the RTFDDA module 
! Al Bourgeois (NCAR/RAL): lead engineer implementing RTFDDA into WRF-ARW
! Nov. 2006
! 
! References:
!   
!   Liu, Y., A. Bourgeois, T. Warner, S. Swerdlin and J. Hacker, 2005: An
!     implementation of obs-nudging-based FDDA into WRF for supporting 
!     ATEC test operations. 2005 WRF user workshop. Paper 10.7.
!
!   Liu, Y., A. Bourgeois, T. Warner, S. Swerdlin and W. Yu, 2006: An update 
!     on "obs-nudging"-based FDDA for WRF-ARW: Verification using OSSE 
!     and performance of real-time forecasts. 2006 WRF user workshop. Paper 4.7. 

!   
!   Stauffer, D.R., and N.L. Seaman, 1994: Multi-scale four-dimensional data 
!     assimilation. J. Appl. Meteor., 33, 416-434.
!
!   http://www.rap.ucar.edu/projects/armyrange/references.html
!

  SUBROUTINE wrf_fddaobs_in (grid ,config_flags)

    USE module_domain
    USE module_configure
    USE module_model_constants        !rovg

    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN)    :: config_flags
#if ( EM_CORE == 1 )

! Local variables
    integer            :: ktau            ! timestep index corresponding to xtime
    integer            :: krest           ! restart timestep
    integer            :: inest           ! nest level
    integer            :: infreq          ! input frequency
    integer            :: nstlev          ! nest level
    real               :: dtmin           ! dt in minutes
    real               :: xtime           ! forecast time in minutes
    logical            :: iprt_in4dob     ! print flag

    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe
    INTEGER ij, its, ite, jts, jte

!   Modified to also call in4dob intially, since subr in4dob is no
!   longer called from subr fddaobs_init. Note that itimestep is now
!   the model step BEFORE the model integration step, because this
!   routine is now called by med_before_solve_io.
    ktau   = grid%itimestep               ! ktau corresponds to xtime
    krest  = grid%fdob%ktaur
    inest  = grid%grid_id
    nstlev = grid%fdob%levidn(inest) 
    infreq = grid%obs_ionf*(grid%parent_grid_ratio**nstlev)
    iprt_in4dob = grid%obs_ipf_in4dob

    IF( ((ktau.GT.krest.AND.MOD(ktau,infreq).EQ.0)                            &
                                         .OR.(ktau.EQ.krest)) .AND. grid%xtime <= grid%fdda_end ) then
! Calculate forecast time.
      dtmin = grid%dt/60.
      xtime = grid%xtime

      CALL get_ijk_from_grid (  grid ,                                       &
                                ids, ide, jds, jde, kds, kde,                &
                                ims, ime, jms, jme, kms, kme,                &
                                ips, ipe, jps, jpe, kps, kpe    )

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles
         its = grid%i_start(ij)
         ite = min(grid%i_end(ij),ide-1)
         jts = grid%j_start(ij)
         jte = min(grid%j_end(ij),jde-1)

         CALL in4dob(inest, xtime, ktau, krest, dtmin,                              &
                     grid%julyr, grid%julday, grid%gmt,                             &    !obsnypatch
                     grid%obs_nudge_opt,  grid%obs_nudge_wind, grid%obs_nudge_temp, &
                     grid%obs_nudge_mois, grid%obs_nudge_pstr, grid%obs_coef_wind,  &
                     grid%obs_coef_temp,  grid%obs_coef_mois,  grid%obs_coef_pstr,  &
                     grid%obs_rinxy,      grid%obs_rinsig,     grid%fdob%window,    &
                     grid%obs_npfi,       grid%obs_ionf,                            &
                     grid%obs_prt_max,    grid%obs_prt_freq,                        &
                     grid%obs_idynin,                                               &
                     grid%obs_dtramp,     grid%fdob,           grid%fdob%varobs,    &
                     grid%fdob%timeob,    grid%fdob%nlevs_ob,  grid%fdob%lev_in_ob, &
                     grid%fdob%plfo,      grid%fdob%elevob,    grid%fdob%rio,       &
                     grid%fdob%rjo,       grid%fdob%rko,                            &
                     grid%xlat, grid%xlong,                                         &
                     config_flags%cen_lat,                                          &
                     config_flags%cen_lon,                                          &
                     config_flags%stand_lon,                                        &
                     config_flags%truelat1, config_flags%truelat2,                  &
                     grid%fdob%known_lat, grid%fdob%known_lon,                      &
                     config_flags%dx, config_flags%dy, rovg, t0,                    &
                     grid%fdob%obsprt,                                              &
                     grid%fdob%latprt, grid%fdob%lonprt,                            &
                     grid%fdob%mlatprt, grid%fdob%mlonprt,                          &
                     grid%fdob%stnidprt,                                            &
                     ide, jde,                                                      &
                     ims, ime, jms, jme,                                            &
                     its, ite, jts, jte,                                            &
                     config_flags%map_proj,                                         &
                     model_config_rec%parent_grid_ratio,                            &
                     model_config_rec%i_parent_start(inest),                        &
                     model_config_rec%j_parent_start(inest),                        &
                     model_config_rec%max_dom,                                      &
                     model_config_rec%nobs_ndg_vars, grid%max_obs, iprt_in4dob)
       ENDDO
      !$OMP END PARALLEL DO

    ENDIF

    RETURN
#endif
  END SUBROUTINE wrf_fddaobs_in
#if ( EM_CORE == 1 )
!------------------------------------------------------------------------------
! Begin subroutine in4dob and its subroutines
!------------------------------------------------------------------------------
  SUBROUTINE in4dob(inest, xtime, ktau, ktaur, dtmin,                    &
                    myear, julday, gmt,                                  &      !obsnypatch
                    nudge_opt, iswind, istemp,                           &
                    ismois, ispstr, giv,                                 &
                    git, giq, gip,                                       &
                    rinxy, rinsig, twindo,                               &
                    npfi, ionf, prt_max, prt_freq, idynin,               &
                    dtramp, fdob, varobs,                                &
                    timeob, nlevs_ob, lev_in_ob,                         &
                    plfo, elevob, rio,                                   &
                    rjo, rko,                                            &
                    xlat, xlong,                                         &
                    cen_lat,                                             &
                    cen_lon,                                             &
                    stand_lon,                                           &
                    true_lat1, true_lat2,                                &
                    known_lat, known_lon,                                &
                    dxm, dym, rovg, t0,                                  &
                    obs_prt,                                             &
                    lat_prt, lon_prt,                                    &
                    mlat_prt, mlon_prt,                                  &
                    stnid_prt,                                           &
                    e_we, e_sn,                                          &
                    ims, ime, jms, jme,                                  &
                    its, ite, jts, jte,                                  &
                    map_proj,                                            &
                    parent_grid_ratio,                                   &
                    i_parent_start,                                      &
                    j_parent_start,                                      &
                    maxdom,                                              &
                    nndgv, niobf, iprt)

  USE module_domain
  USE module_model_constants, ONLY : rcp
  USE module_date_time , ONLY : geth_idts
  USE module_llxy

  IMPLICIT NONE

! THIS IS SUBROUTINE READS AN OBSERVATION DATA FILE AND
! SELECTS ONLY THOSE VALUES OBSERVED AT TIMES THAT FALL
! WITHIN A TIME WINDOW (TWINDO) CENTERED ABOUT THE CURRENT
! FORECAST TIME (XTIME).  THE INCOMING OBS FILES MUST BE
! IN CHRONOLOGICAL ORDER.
!
! NOTE: This routine was originally designed for MM5, which uses
!       a nonstandard (I,J) coordinate system. For WRF, I is the 
!       east-west running coordinate, and J is the south-north
!       running coordinate. So "J-slab" here is west-east in
!       extent, not south-north as for MM5. RIO and RJO have
!       the opposite orientation here as for MM5. -ajb 06/10/2004

! NOTE - IN4DOB IS CALLED ONLY FOR THE COARSE MESH TIMES
!      - VAROBS(IVAR,N) HOLDS THE OBSERVATIONS.
!        IVAR=1   OBS U
!        IVAR=2   OBS V
!        IVAR=3   OBS T
!        IVAR=4   OBS Q
!        IVAR=5   OBS Pressure
!        IVAR=6   OBS Height

  INTEGER, intent(in) :: niobf          ! maximum number of observations
  INTEGER, intent(in) :: nndgv          ! number of nudge variables
  INTEGER, intent(in)  :: INEST         ! nest level
  REAL, intent(in)     :: xtime         ! model time in minutes
  INTEGER, intent(in)  :: KTAU          ! current timestep
  INTEGER, intent(in)  :: KTAUR         ! restart timestep
  REAL, intent(in)     :: dtmin         ! dt in minutes
  INTEGER, intent(in)  :: myear         ! model year                           !obsnypatch
  INTEGER, intent(in)  :: julday        ! Julian day
  REAL, intent(in)     :: gmt           ! Model GMT at start of run
  INTEGER, intent(in)  :: nudge_opt     ! obs-nudge flag for this nest
  INTEGER, intent(in)  :: iswind        ! nudge flag for wind
  INTEGER, intent(in)  :: istemp        ! nudge flag for temperature
  INTEGER, intent(in)  :: ismois        ! nudge flag for moisture
  INTEGER, intent(in)  :: ispstr        ! nudge flag for pressure (obsolete)
  REAL, intent(in)     :: giv           ! coefficient for wind
  REAL, intent(in)     :: git           ! coefficient for temperature
  REAL, intent(in)     :: giq           ! coefficient for moisture
  REAL, intent(in)     :: gip           ! coefficient for pressure
  REAL, intent(in)     :: rinxy         ! horizontal radius of influence (km)
  REAL, intent(in)     :: rinsig        ! vertical radius of influence (on sigma)
  REAL, intent(inout)  :: twindo        ! (time window)/2 (min) for nudging
  INTEGER, intent(in)  :: npfi          ! coarse-grid time-step frequency for diagnostics
  INTEGER, intent(in)  :: ionf          ! coarse-grid time-step frequency for obs-nudging calcs
  INTEGER, intent(in)  :: prt_max       ! max number of entries of obs for diagnostic printout
  INTEGER, intent(in)  :: prt_freq      ! frequency (in obs index) for diagnostic printout.
  INTEGER, intent(in)  :: idynin        ! for dynamic initialization using a ramp-down function
  REAL, intent(in)     :: dtramp        ! time period in minutes for ramping
  TYPE(fdob_type), intent(inout)  :: fdob     ! derived data type for obs data
  REAL, intent(inout) :: varobs(nndgv,niobf)  ! observational values in each variable
  REAL, intent(inout) :: timeob(niobf)        ! model times for each observation (hours)
  REAL, intent(inout) :: nlevs_ob(niobf)      ! numbers of levels in sounding obs
  REAL, intent(inout) :: lev_in_ob(niobf)     ! level in sounding-type obs
  REAL, intent(inout) :: plfo(niobf)          ! index for type of obs-platform
  REAL, intent(inout) :: elevob(niobf)        ! elevations of observations  (meters)
  REAL, intent(inout) :: rio(niobf)           ! west-east grid coordinate (non-staggered grid)
  REAL, intent(inout) :: rjo(niobf)           ! south-north grid coordinate (non-staggered grid)
  REAL, intent(inout) :: rko(niobf)           ! vertical grid coordinate
  REAL, DIMENSION( ims:ime, jms:jme ),                            &
        INTENT(IN )       :: xlat, xlong      ! lat/lon on mass-pt grid (for diagnostics only)
  REAL, intent(in) :: cen_lat                 ! center latitude for map projection
  REAL, intent(in) :: cen_lon                 ! center longitude for map projection
  REAL, intent(in) :: stand_lon               ! standard longitude for map projection
  REAL, intent(in) :: true_lat1               ! truelat1 for map projection
  REAL, intent(in) :: true_lat2               ! truelat2 for map projection
  REAL, intent(in) :: known_lat               ! latitude  of domain origin point (i,j)=(1,1) 
  REAL, intent(in) :: known_lon               ! longigude of domain origin point (i,j)=(1,1)
  REAL, intent(in) :: dxm                     ! grid size in x (meters)
  REAL, intent(in) :: dym                     ! grid size in y (meters)
  REAL, intent(in) :: rovg                    ! constant rho over g
  REAL, intent(in) :: t0                      ! background temperature
  INTEGER, intent(inout) :: obs_prt(prt_max)  ! For printout of obs index
  REAL, intent(inout) :: lat_prt(prt_max)     ! For printout of obs latitude 
  REAL, intent(inout) :: lon_prt(prt_max)     ! For printout of obs longitude
  REAL, intent(inout) :: mlat_prt(prt_max)    ! For printout of model lat at obs (ri,rj)
  REAL, intent(inout) :: mlon_prt(prt_max)    ! For printout of model lon at obs (ri,rj)
  INTEGER, intent(inout) :: stnid_prt(40,prt_max) ! For printout of model lon at obs (ri,rj)
  INTEGER, intent(in) :: e_we                 ! max grid index in south-north coordinate
  INTEGER, intent(in) :: e_sn                 ! max grid index in west-east   coordinate
  INTEGER, intent(in) :: ims                  ! grid memory start index (west-east dim)
  INTEGER, intent(in) :: ime                  ! grid memory end   index (west-east dim)
  INTEGER, intent(in) :: jms                  ! grid memory start index (south-north dim)
  INTEGER, intent(in) :: jme                  ! grid memory end   index (south-north dim)
  INTEGER, intent(in) :: its                  ! grid tile   start index (west-east dim)
  INTEGER, intent(in) :: ite                  ! grid tile   end   index (west-east dim)
  INTEGER, intent(in) :: jts                  ! grid tile   start index (south-north dim)
  INTEGER, intent(in) :: jte                  ! grid tile   end   index (south-north dim)
  INTEGER, intent(in) :: map_proj             ! map projection index
  INTEGER, intent(in) :: parent_grid_ratio    ! parent to nest grid ration
  INTEGER, intent(in) :: i_parent_start       ! starting i coordinate in parent domain
  INTEGER, intent(in) :: j_parent_start       ! starting j coordinate in parent domain
  INTEGER, intent(in) :: maxdom               ! maximum number of domains
  LOGICAL, intent(in) :: iprt                 ! print flag
      
!***  DECLARATIONS FOR IMPLICIT NONE                                    
  integer :: n, ndum, nopen, nvol, idate, imm, iss
  integer :: nlast                      ! last obs in list of valid obs from prev window
  integer :: nsta                       ! number of stations held in timeobs array 
  integer :: nstaw                      ! # stations within the actual time window
  integer :: nprev                      ! previous n in obs read loop (for printout only)
  integer :: meas_count, imc, njend, njc, njcc, julob, kn
  real    :: hourob, rjulob
  real    :: xhour, tback, tforwd, rjdate1, timanl1, rtimob
  real    :: rj, ri, elevation, pressure_data
  real    :: pressure_qc, height_data, height_qc, temperature_data
  real    :: temperature_qc, u_met_data, u_met_qc, v_met_data
  real    :: v_met_qc, rh_data, rh_qc, r_data, slp_data, slp_qc
  real    :: ref_pres_data, ref_pres_qc, psfc_data, psfc_qc
  real    :: precip_data, precip_qc, tbar, twdop
  real*8  :: tempob
  INTEGER, EXTERNAL :: nvals_le_limit         ! for finding #obs with timeobs <= tforwd

! Local variables
  TYPE (PROJ_INFO)   :: obs_proj        ! Structure for obs projection information.
  character*14 date_char
  character*19 obs_date                                                        !obsnypatch
  integer idts                                                                 !obsnypatch
  character*40 platform,source,id,namef
  character*2 fonc
  character(len=200) :: msg       ! Argument to wrf_message
  real latitude,longitude
  logical :: newpass          ! New pass flag (used for printout)
  logical is_sound,bogus
  LOGICAL OPENED,exist
  integer :: ieof(5),ifon(5)
  data ieof/0,0,0,0,0/
  data ifon/0,0,0,0,0/
  integer :: nmove, nvola
  integer :: iyear, itimob                                                     !obsnypatch
  integer :: errcnt
  DATA NMOVE/0/,NVOLA/61/
  character*140 file_name_on_obs_unit, obs_domain_file_name
  integer :: obs_domain_file_unit

! if(ieof(inest).eq.2.and.fdob%nstat.eq.0)then
!   IF (iprt) print *,'returning from in4dob'
!   return
! endif
! IF (iprt) print *,'start in4dob ',inest,xtime
  IF(nudge_opt.NE.1)RETURN

! Initialize obs for printout
  obs_prt = -999
  newpass = .true.
  errcnt  = 0 

! if start time, or restart time, set obs array to missing value
  IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR) THEN
    DO N=1,NIOBF
      TIMEOB(N)=99999.
    ENDDO
    fdob%xtime_at_rest = xtime    !yliu 20080127
  ENDIF
! set number of obs=0 if at start or restart
  IF(KTAU.EQ.KTAUR)fdob%NSTAT=0
  NSTA=fdob%NSTAT

  XHOUR=XTIME/60.
  XHOUR=AMAX1(XHOUR,0.0)

! DEFINE THE MAX LIMITS OF THE WINDOW
  TBACK=XHOUR-TWINDO
  TFORWD=XHOUR+TWINDO

  IF (iprt) then
     write(msg,fmt='(2(a,f8.3),a,i2)')                                            &
                  'OBS NUDGING: Reading new obs for time window TBACK = ',  &
                  tback,' TFORWD = ',tforwd,' for grid = ',inest
     call wrf_message(msg)
  ENDIF

! For obs that have become invalid because they are too old for the current time
! window, mark with 99999 to set up for removal from the rolling valid-obs list.

  IF(NSTA.NE.0) THEN
    NDUM=0
    t_window : DO N=1,NSTA+1
      IF((TIMEOB(N)-TBACK).LT.0) THEN
        TIMEOB(N)=99999.
      ENDIF
      IF(TIMEOB(N).LT.9.E4) EXIT t_window
      NDUM=N
    ENDDO t_window

    IF (iprt .and. ndum>0) THEN
      write(msg,fmt='(a,i5,2a)') 'OBS NUDGING: ',ndum,' previously read obs ',  &
           'are now too old for the current window and have been removed.'
      call wrf_message(msg)
    ENDIF

! REMOVE OLD OBS DENOTED BY 99999. AT THE FRONT OF TIMEOB ARRAY
!   IF (iprt) print *,'ndum at 20=',ndum
    NDUM=ABS(NDUM)
    NMOVE=NIOBF-NDUM
    IF(NMOVE.GT.0 .AND. NDUM.NE.0 ) THEN  
      DO N=1,NMOVE
        do KN = 1,nndgv
          VAROBS(KN,N)=VAROBS(KN,N+NDUM)
        enddo
! RIO is the west-east coordinate. RJO is south-north. (ajb)
        RJO(N)=RJO(N+NDUM)
        RIO(N)=RIO(N+NDUM)
        RKO(N)=RKO(N+NDUM)
        TIMEOB(N)=TIMEOB(N+NDUM)
        nlevs_ob(n)=nlevs_ob(n+ndum)
        lev_in_ob(n)=lev_in_ob(n+ndum)
        plfo(n)=plfo(n+ndum)
        elevob(n)=elevob(n+ndum) 
      ENDDO
    ENDIF
    NOPEN=NMOVE+1
    IF(NOPEN.LE.NIOBF) THEN
      DO N=NOPEN,NIOBF
        do KN = 1,nndgv
          VAROBS(KN,N)=99999.
        enddo
        RIO(N)=99999.
        RJO(N)=99999.
        RKO(N)=99999.
        TIMEOB(N)=99999.
        nlevs_ob(n)=99999.
        lev_in_ob(n)=99999.
        plfo(n)=99999.
        elevob(n)=99999.
      ENDDO
    ENDIF
  ENDIF

! Compute map projection info.
  call set_projection(obs_proj, map_proj, cen_lat, cen_lon,            &
                      true_lat1, true_lat2, stand_lon,                 &
                      known_lat, known_lon,                            &
                      e_we, e_sn, dxm, dym )

! FIND THE LAST OBS IN THE LIST
  NLAST=0
  last_ob : DO N=1,NIOBF
!   print *,'nlast,n,timeob(n)=',nlast,n,timeob(n)
    IF(TIMEOB(N).GT.9.E4) EXIT last_ob
    NLAST=N
  ENDDO last_ob

! print *,'in4dob, after 90 ',nlast,ktau,ktaur,nsta
! open file if at beginning or restart
  IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR) THEN
    fdob%RTLAST=-999.
    obs_domain_file_unit = NVOLA+INEST-1
    INQUIRE (obs_domain_file_unit,NAME=file_name_on_obs_unit,OPENED=OPENED)
    !Build obs nudging file name (OBS_DOMAINX01 where X is the current domain number)
      ifon(inest)=1
      write(fonc(1:2),'(i2)')ifon(inest)
      if(fonc(1:1).eq.' ')fonc(1:1)='0'
    obs_domain_file_name='OBS_DOMAIN'//CHAR(INEST+ICHAR('0'))//fonc(1:2)  
    IF (.NOT. OPENED) THEN
      INQUIRE (file=obs_domain_file_name,EXIST=exist)
      if(exist)then
        IF (iprt) THEN
          write(msg,*) 'opening first fdda obs file, fonc=',              &
                       fonc,' inest=',inest
          call wrf_message(msg)
          write(msg,*) 'ifon=',ifon(inest)
          call wrf_message(msg)
        ENDIF
        OPEN(obs_domain_file_unit,FILE=obs_domain_file_name,FORM='FORMATTED',STATUS='OLD')
      else
! no first file to open
        IF (iprt) call wrf_message("there are no fdda obs files to open")
        return
      endif
    ELSE
     !If the unit for observation nudging file is already open make sure the file
     !name matches the expected name to ensure that some other part of WRF has
     !not opened a file using the same unit number
     IF(file_name_on_obs_unit .ne. obs_domain_file_name) THEN 
      write(msg,*) 'File open on obs nudging unit (',obs_domain_file_unit,') with wrong name'
      call wrf_message(msg)
      write(msg,*) 'File open on obs nudging unit is named ',&
       trim(adjustl(file_name_on_obs_unit)),' but it should be named ',&
       trim(adjustl(obs_domain_file_name))
      call wrf_message(msg)
      write(msg,*) 'This likely means this unit number was opened elsewhere in WRF'
      call wrf_message(msg)
      call wrf_error_fatal ( 'wrf_fddaobs_in: in4dob STOP Obs nudging file name mismatch' )
     ENDIF
    ENDIF
  ENDIF  !end if(KTAU.EQ.0.OR.KTAU.EQ.KTAUR)
! print *,'at jc check1'
 
!**********************************************************************
!       --------------   BIG 100 LOOP OVER N  --------------
!**********************************************************************
! NOW CHECK TO SEE IF EXTRA DATA MUST BE READ IN FROM THE
! DATA FILE.  CONTINUE READING UNTIL THE REACHING THE EOF
! (DATA TIME IS NEGATIVE) OR FIRST TIME PAST TFORWD. THE
! LAST OBS CURRENTLY AVAILABLE IS IN N=NMOVE.

  N=NLAST
  IF(N.EQ.0)GOTO 110

 1001 continue

! ieof=2 means no more files

    IF(IEOF(inest).GT.1) then
      GOTO 130
    endif

100 CONTINUE
!ajb 20070116 bugfix for zero array index. N=0 if first obs is not in the domain.
    IF(N.ne.0) THEN
      IF(TIMEOB(N).GT.TFORWD.and.timeob(n).lt.99999.) THEN
         GOTO 130
      ENDIF
    ENDIF
 
! OBSFILE: no more data in the obsfile 
! AJB note: This is where we would implement multi-file reading.
    if(ieof(inest).eq.1 )then
      ieof(inest)=2
      goto 130
    endif

!**********************************************************************
!       --------------   110 SUBLOOP OVER N  --------------
!**********************************************************************
  110 continue
! THE TIME OF THE MOST RECENTLY ACQUIRED OBS IS .LE. TFORWD,
! SO CONTINUE READING

      IF(N.GT.NIOBF-1)GOTO 120
! REPLACE NVOLA WITH LUN 70, AND USE NVOLA AS A FILE COUNTER
      NVOL=NVOLA+INEST-1
      IF(fdob%IEODI.EQ.1)GOTO 111
      read(nvol,101,end=111,err=111)date_char
 101  FORMAT(1x,a14)

      n=n+1

! Convert the form of the observation date for geth_idts.
      call fmt_date(date_char, obs_date)

! Compute the time period in seconds from the model reference
! date (fdob%sdate) until the observation date.

      call geth_idts(obs_date, fdob%sdate(1:19), idts)

! Convert time in seconds to hours.
      ! In case of restart, correct for new sdate.
      idts = idts + nint(fdob%xtime_at_rest*60.)  ! yliu 20080127

      rtimob =float(idts)/3600.
      timeob(n)=rtimob

!     print *,'read in ob ',n,timeob(n),rtimob
      IF(IDYNIN.EQ.1.AND.TIMEOB(N)*60..GT.fdob%DATEND) THEN
        IF (iprt) THEN
          write(msg,*) ' IN4DOB: FOR INEST = ',INEST,' AT XTIME = ',XTIME,    &
                       ' TIMEOB = ',TIMEOB(N)*60.,' AND DATEND = ',fdob%DATEND,' :'
          call wrf_message(msg)
          write(msg,*) '         END-OF-DATA FLAG SET FOR OBS-NUDGING',       &
                       ' DYNAMIC INITIALIZATION'
          call wrf_message(msg)
        ENDIF
        fdob%IEODI=1
        TIMEOB(N)=99999.
        rtimob=timeob(n)
      ENDIF
      read(nvol,102)latitude,longitude
 102  FORMAT(2x,2(f9.4,1x))

!     if(ifon.eq.4)print *,'ifon=4',latitude,longitude
! this works only for lc projection
! yliu: add llxy for all 3 projection
          
!ajb Arguments ri and rj have been switched from MM5 orientation.

      CALL latlon_to_ij(obs_proj, latitude, longitude, ri, rj)

!ajb  ri and rj are referenced to the non-staggered grid (not mass-pt!).
!     (For MM5, they were referenced to the dot grid.)

      ri = ri + .5      !ajb Adjust from mass-pt to non-staggered grid.
      rj = rj + .5      !ajb Adjust from mass-pt to non-staggered grid.

      rio(n)=ri
      rjo(n)=rj

      read(nvol,1021)id,namef
 1021 FORMAT(2x,2(a40,3x))
      read(nvol,103)platform,source,elevation,is_sound,bogus,meas_count
 103  FORMAT( 2x,2(a16,2x),f8.0,2x,2(l4,2x),i5)

!     write(6,*) '----- OBS description ----- '
!     write(6,*) 'platform,source,elevation,is_sound,bogus,meas_count:'
!     write(6,*) platform,source,elevation,is_sound,bogus,meas_count

! yliu 
      elevob(n)=elevation
! jc
! change platform from synop to profiler when needed
      if(namef(2:9).eq.'PROFILER')platform(7:14)='PROFILER'
! yliu
      if(namef(2:6).eq.'ACARS')platform(7:11)='ACARS'
      if(namef(1:7).eq.'SATWNDS') platform(1:11)='SATWNDS    '
      if(namef(1:8).eq.'CLASS DA')platform(7:10)='TEMP'
! yliu end
 
      rko(n)=-99.
!yliu 20050706
!     if((platform(7:11).eq.'METAR').or.(platform(7:11).eq.'SPECI').or.
!    1   (platform(7:10).eq.'SHIP').or.(platform(7:11).eq.'SYNOP').or.
!    1    (platform(1:4).eq.'SAMS'))
!    1   rko(n)=1.0
      if(.NOT. is_sound) rko(n)=1.0
!yliu 20050706 end

! plfo is inFORMATion on what platform. May use this later in adjusting weights
      plfo(n)=99.
      if(platform(7:11).eq.'METAR')plfo(n)=1.
      if(platform(7:11).eq.'SPECI')plfo(n)=2.
      if(platform(7:10).eq.'SHIP')plfo(n)=3.
      if(platform(7:11).eq.'SYNOP')plfo(n)=4.
      if(platform(7:10).eq.'TEMP')plfo(n)=5.
      if(platform(7:11).eq.'PILOT')plfo(n)=6.
      if(platform(1:7).eq.'SATWNDS')plfo(n)=7.
      if(platform(7:11).eq.'SATWI')plfo(n)=7.
      if(platform(1:4).eq.'SAMS')plfo(n)=8.
      if(platform(7:14).eq.'PROFILER')plfo(n)=9.
! yliu: ACARS->SATWINDS
      if(platform(7:11).eq.'ACARS')plfo(n)=7.
! yliu: end
      if(plfo(n).eq.99.) then
         IF (iprt) then
           write(msg,*) 'n=',n,' unknown ob of type ',platform
           call wrf_message(msg)
         ENDIF
      endif

!======================================================================
!======================================================================
! THIS PART READS SOUNDING INFO
      IF(is_sound)THEN
        nlevs_ob(n)=real(meas_count)
        lev_in_ob(n)=1.
        do imc=1,meas_count
!             write(6,*) '0 inest = ',inest,' n = ',n
! the sounding has one header, many levels. This part puts it into 
! "individual" observations. There's no other way for nudob to deal
! with it.
          if(imc.gt.1)then                          ! sub-loop over N
            n=n+1
            if(n.gt.niobf)goto 120
            nlevs_ob(n)=real(meas_count)
            lev_in_ob(n)=real(imc)
            timeob(n)=rtimob
            rio(n)=ri
            rjo(n)=rj
            rko(n)=-99.
            plfo(n)=plfo(n-imc+1)
            elevob(n)=elevation
          endif

          read(nvol,104)pressure_data,pressure_qc,                  &
                        height_data,height_qc,                      &
                        temperature_data,temperature_qc,            &
                        u_met_data,u_met_qc,                        &
                        v_met_data,v_met_qc,                        &
                        rh_data,rh_qc
 104      FORMAT( 1x,6(f11.3,1x,f11.3,1x))

! yliu: Ensemble - add disturbance to upr obs
!         if(plfo(n).eq.5.or.plfo(n).eq.6.or.plfo(n).eq.9) then                  FORE07E08
!          if(imc.eq.1) then                                                     FORE07E08
!     call srand(n)
!     t_rand =- (rand(2)-0.5)*6
!     call srand(n+100000)
!     u_rand =- (rand(2)-0.5)*6
!     call srand(n+200000)
!     v_rand =- (rand(2)-0.5)*6
!          endif                                                                 FORE07E08
!     if(temperature_qc.ge.0..and.temperature_qc.lt.30000..and.
!    &   temperature_data .gt. -88880.0 )
!    & temperature_data = temperature_data  + t_rand
!     if((u_met_qc.ge.0..and.u_met_qc.lt.30000.).and.
!    &   (v_met_qc.ge.0..and.v_met_qc.lt.30000.).and.
! make sure at least 1 of the components is .ne.0
!    &   (u_met_data.ne.0..or.v_met_data.ne.0.) .and.
!    &   (u_met_data.gt.-88880.0 .and. v_met_data.gt.-88880.0) )then
!         u_met_data = u_met_data + u_rand
!         v_met_data = v_met_data + v_rand
!     endif
!         endif                                                                  FORE07E08
! yliu: Ens test - end

 
! jc
! hardwire to switch -777777. qc to 0. here temporarily
! -777777. is a sounding level that no qc was done on.
 
          if(temperature_qc.eq.-777777.)temperature_qc=0.
          if(pressure_qc.eq.-777777.)pressure_qc=0.
          if(height_qc.eq.-777777.)height_qc=0.
          if(u_met_qc.eq.-777777.)u_met_qc=0.
          if(v_met_qc.eq.-777777.)v_met_qc=0.
          if(rh_qc.eq.-777777.)rh_qc=0.
          if(temperature_data.eq.-888888.)temperature_qc=-888888.
          if(pressure_data.eq.-888888.)pressure_qc=-888888.
          if(height_data.eq.-888888.)height_qc=-888888.
          if(u_met_data.eq.-888888.)u_met_qc=-888888.
          if(v_met_data.eq.-888888.)v_met_qc=-888888.
          if(rh_data.eq.-888888.)rh_qc=-888888.
 
! jc
! Hardwire so that only use winds in pilot obs (no winds from temp) and
!    only use temperatures and rh in temp obs (no temps from pilot obs)
! Do this because temp and pilot are treated as 2 platforms, but pilot 
!    has most of the winds, and temp has most of the temps. If use both,
!    the second will smooth the effect of the first. Usually temps come in after
!    pilots. pilots usually don't have any temps, but temp obs do have some 
!    winds usually.
! plfo=5 is TEMP ob, range sounding is an exception
!yliu start -- comment out to test with merged PILOT and TEMP and 
!        do not use obs interpolated by little_r
!       if(plfo(n).eq.5. .and. namef(1:8).ne.'CLASS DA')then
!         u_met_data=-888888.
!         v_met_data=-888888.
!         u_met_qc=-888888.
!         v_met_qc=-888888.
!       endif
          if(plfo(n).eq.5..and.(u_met_qc.eq.256..or.v_met_qc.eq.256.))then
            u_met_data=-888888.
            v_met_data=-888888.
            u_met_qc=-888888.
            v_met_qc=-888888.
          endif
!yliu end
! plfo=6 is PILOT ob
          if(plfo(n).eq.6.)then
            temperature_data=-888888.
            rh_data=-888888.
            temperature_qc=-888888.
            rh_qc=-888888.
          endif

!ajb Store temperature for WRF
!    NOTE: The conversion to potential temperature, performed later in subroutine
!    errob, requires good pressure data, either directly or via good height data.
!    So here, in addition to checking for good temperature data,  we must also
!    do a check for good pressure or height.
          if(temperature_qc.ge.0..and.temperature_qc.lt.30000.)then

            if( (pressure_qc.ge.0..and.pressure_qc.lt.30000.) .or.    &
                (height_qc  .ge.0..and.height_qc  .lt.30000.) ) then

              varobs(3,n) = temperature_data
            else
              varobs(3,n)=-888888.
            endif

          else
            varobs(3,n)=-888888.
          endif

!ajb Store obs height
          if(height_qc.ge.0..and.height_qc.lt.30000.)then
            varobs(6,n)=height_data
          else
            varobs(6,n)=-888888.
          endif

          if(pressure_qc.ge.0..and.pressure_qc.lt.30000.)then
!           if(pressure_qc.ge.0.)then
            varobs(5,n)=pressure_data
          else
            varobs(5,n)=-888888.
            IF (iprt) THEN
              if(varobs(6,n).eq.-888888.000) then
                if (errcnt.le.10) then
                  write(msg,*) '*** PROBLEM: sounding, p and ht undefined',latitude,longitude
                  call wrf_message(msg)
                  errcnt = errcnt + 1
                  if (errcnt.gt.10) call wrf_message("MAX of 10 warnings issued.")
                endif
              endif
            ENDIF
          endif 
          if(varobs(5,n).ge.0.)varobs(5,n)=varobs(5,n)*1.e-3
! don't use data above 80 mb
          if((varobs(5,n).gt.0.).and.(varobs(5,n).le.8.))then
            u_met_data=-888888.
            v_met_data=-888888.
            u_met_qc=-888888.
            v_met_qc=-888888.
            temperature_data=-888888.
            temperature_qc=-888888.
            rh_data=-888888.
            rh_qc=-888888.
          endif


! Store horizontal wind components for WRF
          if((u_met_qc.ge.0..and.u_met_qc.lt.30000.).and.  &
             (v_met_qc.ge.0..and.v_met_qc.lt.30000.).and.  &
! make sure at least 1 of the components is .ne.0
             (u_met_data.ne.0..or.v_met_data.ne.0.))then

! If Earth-relative wind vector, need to rotate it to grid-relative coords
               if(u_met_qc.eq.129. .and. v_met_qc.eq.129.) then
                  CALL rotate_vector(longitude,u_met_data,v_met_data,   &
                                     obs_proj,map_proj)
               endif
               varobs(1,n)=u_met_data
               varobs(2,n)=v_met_data
          else
               varobs(1,n)=-888888.
               varobs(2,n)=-888888.
          endif

          r_data=-888888.

          if(rh_qc.ge.0..and.rh_qc.lt.30000.)then
            if((pressure_qc.ge.0.).and.(temperature_qc.ge.0.).and.       &
              (pressure_qc.lt.30000.).and.(temperature_qc.lt.30000.))then
              call rh2r(rh_data,temperature_data,pressure_data*.01,      &
                        r_data,0)            ! yliu, change last arg from 1 to 0
            else
!             print *,'rh, but no t or p to convert',temperature_qc,     &
!             pressure_qc,n
              r_data=-888888.
            endif
          endif
          varobs(4,n)=r_data
        enddo    ! end do imc=1,meas_count
!       print *,'--- sdng n=',n,nlevs_ob(n),lev_in_ob(n),timeob(n)
!       read in non-sounding obs

      ELSEIF(.NOT.is_sound)THEN
        nlevs_ob(n)=1.
        lev_in_ob(n)=1.
        read(nvol,105)slp_data,slp_qc,                                 &
                      ref_pres_data,ref_pres_qc,                       &
                      height_data,height_qc,                           &
                      temperature_data,temperature_qc,                 &
                      u_met_data,u_met_qc,                             &
                      v_met_data,v_met_qc,                             &
                      rh_data,rh_qc,                                   &
                      psfc_data,psfc_qc,                               &
                      precip_data,precip_qc
 105    FORMAT( 1x,9(f11.3,1x,f11.3,1x))

! Ensemble: add disturbance to sfc obs
!     call srand(n)
!     t_rand =+ (rand(2)-0.5)*5
!     call srand(n+100000)
!     u_rand =+ (rand(2)-0.5)*5
!     call srand(n+200000)
!     v_rand =+ (rand(2)-0.5)*5
!     if(temperature_qc.ge.0..and.temperature_qc.lt.30000.  .and.
!    &   temperature_data .gt. -88880.0 )
!    & temperature_data = temperature_data  + t_rand
!     if((u_met_qc.ge.0..and.u_met_qc.lt.30000.).and.
!    &   (v_met_qc.ge.0..and.v_met_qc.lt.30000.).and.
! make sure at least 1 of the components is .ne.0
!    &   (u_met_data.ne.0..or.v_met_data.ne.0.) .and.
!    &   (u_met_data.gt.-88880.0 .and. v_met_data.gt.-88880.0) )then
!         u_met_data = u_met_data + u_rand
!         v_met_data = v_met_data + v_rand
!      endif
! yliu: Ens test - end

!Lilis

! calculate psfc if slp is there
        if((psfc_qc.lt.0.).and.(slp_qc.ge.0..and.slp_qc.lt.30000.).and.   &
              (temperature_qc.ge.0..and.temperature_qc.lt.30000.).and.    &
              (slp_data.gt.90000.))then
          tbar=temperature_data+0.5*elevation*.0065
          psfc_data=slp_data*exp(-elevation/(rovg*tbar))
          varobs(5,n)=psfc_data
          psfc_qc=0.
        endif

!c *No* **Very rough** estimate of psfc from sfc elevation if UUtah ob and elev>1000m
! estimate psfc from temp and elevation
!   Do not know sfc pressure in model at this point.
!      if((psfc_qc.lt.0.).and.(elevation.gt.1000.).and.
!     1   (temperature_qc.ge.0..and.temperature_qc.lt.30000.)
!     1    .and.(platform(7:16).eq.'SYNOP PRET'))then
        if((psfc_qc.lt.0.).and.                                          &
          (temperature_qc.ge.0..and.temperature_qc.lt.30000.))then
          tbar=temperature_data+0.5*elevation*.0065
          psfc_data=100000.*exp(-elevation/(rovg*tbar))
          varobs(5,n)=psfc_data
          psfc_qc=0.
        endif

        if((psfc_qc.ge.0..and.psfc_qc.lt.30000.).and.(psfc_data.gt.70000.  &
        .and.psfc_data.lt.105000.))then
          varobs(5,n)=psfc_data
        else
          varobs(5,n)=-888888.
        endif

        if(varobs(5,n).ge.0.)varobs(5,n)=varobs(5,n)*1.e-3

!Lilie
!ajb Store temperature for WRF
        if(temperature_qc.ge.0..and.temperature_qc.lt.30000.)then

          if((psfc_qc.ge.0..and.psfc_qc.lt.30000.).and.          &
             (psfc_data.gt.70000. .and.psfc_data.lt.105000.))then

            varobs(3,n) = temperature_data
          else
            varobs(3,n)=-888888.
          endif
        else
          varobs(3,n)=-888888.
        endif

! Store horizontal wind components for WRF
        if((u_met_qc.ge.0..and.u_met_qc.lt.30000.).and.            &
           (v_met_qc.ge.0..and.v_met_qc.lt.30000.).and.            &
! make sure at least 1 of the components is .ne.0
           (u_met_data.ne.0..or.v_met_data.ne.0.))then

! If Earth-relative wind vector, need to rotate it to grid-relative coords
             if(u_met_qc.eq.129. .and. v_met_qc.eq.129.) then
                CALL rotate_vector(longitude,u_met_data,v_met_data,   &
                                   obs_proj,map_proj)
             endif
             varobs(1,n)=u_met_data
             varobs(2,n)=v_met_data
        else
             varobs(1,n)=-888888.
             varobs(2,n)=-888888.
        endif

! jc
! if a ship ob has rh<70%, then throw out

        if(plfo(n).eq.3..and.rh_qc.ge.0..and.rh_data.lt.70.)then
          rh_qc=-888888.
          rh_data=-888888.
        endif
!
        r_data=-888888.
        if(rh_qc.ge.0..and.rh_qc.lt.30000.)then
          if((psfc_qc.ge.0..and.psfc_qc.lt.30000.)                       &
          .and.(temperature_qc.ge.0..and.temperature_qc.lt.30000.))then
!           rh_data=amin1(rh_data,96.) ! yliu: do not allow surface to be saturated
            call rh2r(rh_data,temperature_data,psfc_data*.01,            &
                      r_data,0)            ! yliu, change last arg from 1 to 0
          else
!           print *,'rh, but no t or p',temperature_data,
!    1 psfc_data,n
            r_data=-888888.
          endif
        endif
        varobs(4,n)=r_data
      ELSE
        IF (iprt) THEN
           call wrf_message(" ======  ")
           call wrf_message(" NO Data Found ")
        ENDIF
      ENDIF   !end if(is_sound)
! END OF SFC OBS INPUT SECTION
!======================================================================
!======================================================================
! check if ob time is too early (only applies to beginning)
      IF(RTIMOB.LT.TBACK-TWINDO)then
        IF (iprt) call wrf_message("ob too early")
        n=n-1
        GOTO 110
      ENDIF

! check if this ob is a duplicate
! this check has to be before other checks
      njend=n-1
      if(is_sound)njend=n-meas_count
      do njc=1,njend
! Check that time, lat, lon, and platform all match exactly.
! Platforms 1-4 (surface obs) can match with each other. Otherwise,
!   platforms have to match exactly. 
        if( (timeob(n).eq.timeob(njc)) .and.                     &
            (rio(n).eq.rio(njc))       .and.                     &
            (rjo(n).eq.rjo(njc))       .and.                     &
            (plfo(njc).ne.99.) ) then
!yliu: if two sfc obs are departed less than 1km, consider they are redundant
!              (abs(rio(n)-rio(njc))*dscg.gt.1000.)   &
!         .or. (abs(rjo(n)-rjo(njc))*dscg.gt.1000.)   &
!         .or. (plfo(njc).eq.99.) )goto 801
!yliu end
! If platforms different, and either > 4, jump out
          if( ( (plfo(n).le.4.).and.(plfo(njc).le.4.) ) .or.     &
                (plfo(n).eq.plfo(njc)) ) then

! if not a sounding, and levels are the same then replace first occurrence 
            if((.not.is_sound).and.(rko(njc).eq.rko(n))) then
!             print *,'dup single ob-replace ',n,inest,
!             plfo(n),plfo(njc)
! this is the sfc ob replacement part
              do KN = 1,nndgv
                VAROBS(KN,njc)=VAROBS(KN,n)
              enddo
! don't need to switch these because they're the same
!             RIO(njc)=RIO(n)
!             RJO(njc)=RJO(n)
!             RKO(njc)=RKO(n)
!             TIMEOB(njc)=TIMEOB(n)
!             nlevs_ob(njc)=nlevs_ob(n)
!             lev_in_ob(njc)=lev_in_ob(n)
!             plfo(njc)=plfo(n)
! end sfc ob replacement part

              n=n-1
              goto 100
! It's harder to fix the soundings, since the number of levels may be different
! The easiest thing to do is to just set the first occurrence to all missing, and
!    keep the second occurrence, or vice versa.
! For temp or profiler keep the second, for pilot keep the one with more levs
! This is for a temp or prof sounding, equal to same
!  also if a pilot, but second one has more obs
            elseif( (is_sound).and.(plfo(njc).eq.plfo(n)) .and.            &
                    ( (plfo(njc).eq.5.).or.(plfo(njc).eq.9.).or.           &
                    ( (plfo(njc).eq.6.).and.                               &
                      (nlevs_ob(n).ge.nlevs_ob(njc)) ) ) )then
              IF (iprt) THEN
                write(msg,*) 'duplicate sounding - eliminate first occurrence', &
                                       n,inest,meas_count,nlevs_ob(njc),        &
                                       latitude,longitude,plfo(njc)
                call wrf_message(msg)
              ENDIF
              if(lev_in_ob(njc).ne.1.) then
                IF (iprt) THEN
                  write(msg,*) 'problem ******* - dup sndg ',                   &
                               lev_in_ob(njc),nlevs_ob(njc)
                  call wrf_message(msg)
                ENDIF
              endif
!             n=n-meas_count
! set the first sounding ob to missing
              do njcc=njc,njc+nint(nlevs_ob(njc))-1
                do KN = 1,nndgv
                  VAROBS(KN,njcc)=-888888.
                enddo
                plfo(njcc)=99.
              enddo
              goto 100
!  if a pilot, but first one has more obs
            elseif( (is_sound).and.(plfo(njc).eq.plfo(n)) .and.            &
                    (plfo(njc).eq.6.).and.                                 &
                    (nlevs_ob(n).lt.nlevs_ob(njc)) )then
              IF (iprt) THEN
                write(msg,*)                                               &
                 'duplicate pilot sounding - eliminate second occurrence', &
                                 n,inest,meas_count,nlevs_ob(njc),         &
                                 latitude,longitude,plfo(njc)
                call wrf_message(msg)
              ENDIF
              if(lev_in_ob(njc).ne.1.) then
                IF (iprt) THEN
                  write(msg,*) 'problem ******* - dup sndg ',              &
                           lev_in_ob(njc),nlevs_ob(njc)
                  call wrf_message(msg)
                ENDIF
              endif
              n=n-meas_count

!ajb  Reset timeob for discarded indices.
              do imc = n+1, n+meas_count
                timeob(imc) = 99999.
              enddo
              goto 100
! This is for a single-level satellite upper air ob - replace first
            elseif( (is_sound).and.                                        &
                    (nlevs_ob(njc).eq.1.).and.                             &
                    (nlevs_ob(n).eq.1.).and.                               &
                    (varobs(5,njc).eq.varobs(5,n)).and.                    &
                    (plfo(njc).eq.7.).and.(plfo(n).eq.7.) ) then
              IF (iprt) then
                write(msg,*)                                               &
                'duplicate single lev sat-wind ob - replace first',n,      &
                                 inest,meas_count,varobs(5,n)
                call wrf_message(msg)
              ENDIF
! this is the single ua ob replacement part
              do KN = 1,nndgv
                VAROBS(KN,njc)=VAROBS(KN,n)
              enddo
! don't need to switch these because they're the same
!           RIO(njc)=RIO(n)
!           RJO(njc)=RJO(n)
!           RKO(njc)=RKO(n)
!           TIMEOB(njc)=TIMEOB(n)
!           nlevs_ob(njc)=nlevs_ob(n)
!           lev_in_ob(njc)=lev_in_ob(n)
!           plfo(njc)=plfo(n)
! end single ua ob replacement part
              n=n-1
              goto 100
            else
!             IF (iprt) THEN
!               write(msg,*) 'duplicate location, but no match otherwise',n,njc,  &
!                            plfo(n),varobs(5,n),nlevs_ob(n),lev_in_ob(n),        &
!                            plfo(njc),varobs(5,njc),nlevs_ob(njc),lev_in_ob(njc)
!               call wrf_message(msg)
!             ENDIF
            endif
          endif
        endif
! end of njc do loop
      enddo

! check if ob is a sams ob that came in via UUtah - discard
      if( plfo(n).eq.4..and.(platform(7:16).eq.'SYNOP PRET').and.          &
          (id(7:15).eq.'METNET= 3') )then
!       print *,'elim metnet=3',latitude,longitude,rtimob
        n=n-1
        goto 100
      endif

! check if ob is in the domain
      if( (ri.lt.2.).or.(ri.gt.real(e_we-1)).or.(rj.lt.2.).or.         &
          (rj.gt.real(e_sn-1)) ) then

          n=n-meas_count
!ajb  Reset timeob for discarded indices.
          do imc = n+1, n+meas_count
            timeob(imc) = 99999.
          enddo
          goto 100
      endif

      IF(TIMEOB(N).LT.fdob%RTLAST) THEN
        IF (iprt) THEN
          call wrf_message("2 OBS ARE NOT IN CHRONOLOGICAL ORDER")
          call wrf_message("NEW YEAR?")
          write(msg,*) 'timeob,rtlast,n=',timeob(n),fdob%rtlast,n
          call wrf_message(msg)
        ENDIF
        call wrf_error_fatal ( 'wrf_fddaobs_in: in4dob STOP 111' )
      ELSE
        fdob%RTLAST=TIMEOB(N)
      ENDIF
! Save obs and model latitude and longitude for printout
      CALL collect_obs_info(newpass,inest,n,latitude,longitude,              &
                         nlast,nprev,niobf,id,stnid_prt,                     &
                         rio,rjo,prt_max,prt_freq,xlat,xlong,                &
                         obs_prt,lat_prt,lon_prt,mlat_prt,mlon_prt,          &
                         e_we,e_sn,ims,ime,jms,jme,its,ite,jts,jte)
      GOTO 100
  111 CONTINUE
!**********************************************************************
!       --------------   END BIG 100 LOOP OVER N  --------------
!**********************************************************************

      if (iprt) then
        write(msg,5403) NVOL,XTIME
        call wrf_message(msg)
      endif
      IEOF(inest)=1

      close(NVOLA+INEST-1)
      IF (iprt) then
         write(msg,*) 'closed fdda file for inest=',inest,nsta
         call wrf_message(msg)
      ENDIF

! AJB note: Go back and check for more files. (Multi-file implementation)
  goto 1001

120 CONTINUE
! THE OBSERVATION ARRAYS ARE FULL AND THE MOST RECENTLY
! ACQUIRED OBS STILL HAS TIMEOB .LE. TFORWD.  SO START
! DECREASING THE SIZE OF THE WINDOW
! get here if too many obs
  IF (iprt) THEN
    write(msg,121) N,NIOBF
    call wrf_message(msg)
  ENDIF
  call wrf_error_fatal ( 'wrf_fddaobs_in: in4dob STOP 122' )

130 CONTINUE
! READ CYCLE IS COMPLETED. DETERMINE THE NUMBER OF OBS IN
! THE CURRENT WINDOW
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! BUT FIRST, WHEN KTAU.EQ.0 (OR IN GENERAL, KTAUR), DISCARD THE
! "OLD" OBS FIRST...

! Get here if at end of file, or if obs time is beyond what we need right now.
! On startup, we report the index of the last obs read.
! For restarts, we need to remove any old obs and then repack obs list.
  IF(KTAU.EQ.KTAUR)THEN
    NSTA=0
    keep_obs : DO N=1,NIOBF
! try to keep all obs, but just don't use yet
!  (don't want to throw away last obs read in - especially if
!  its a sounding, in which case it looks like many obs)
      IF(TIMEOB(N).GT.9.e4) EXIT keep_obs
      if(timeob(n).gt.tforwd) then
        if(iprt) then
           write(msg,950) inest
           call wrf_message(msg)
           write(msg,951) n,timeob(n),tforwd
           call wrf_message(msg)
        endif
 950    FORMAT('Saving index of first ob after end of current time window ', &
               'for nest = ', i3,':')
 951    FORMAT('  ob index = ',i8,',   time of ob = ',f8.4,                  &
               ' hrs,   end of time window = ',f8.4,' hrs')
      endif
      NSTA=N
    ENDDO keep_obs

    NDUM=0
! make time=99999. if ob is too old
!   print *,'tback,nsta=',tback,nsta
    old_obs : DO N=1,NSTA+1
      IF((TIMEOB(N)-TBACK).LT.0)THEN
        TIMEOB(N)=99999.
      ENDIF
!     print *,'n,ndum,timeob=',n,ndum,timeob(n)
      IF(TIMEOB(N).LT.9.E4) EXIT old_obs
      NDUM=N
    ENDDO old_obs

! REMOVE OLD OBS DENOTED BY 99999. AT THE FRONT OF TIMEOB ARRAY
    IF (iprt .and. ktaur > 0) THEN
      write(msg,fmt='(a,i5,a)') 'OBS NUDGING: Upon restart, skipped over ',ndum,   &
                ' obs that are now too old for the current obs window.'
      call wrf_message(msg)
    ENDIF

    NDUM=ABS(NDUM)
    NMOVE=NIOBF-NDUM
    IF( NMOVE.GT.0 .AND. NDUM.NE.0) THEN
      DO N=1,NMOVE
        do KN = 1,nndgv
          VAROBS(KN,N)=VAROBS(KN,N+NDUM)
        enddo
        RJO(N)=RJO(N+NDUM)
        RIO(N)=RIO(N+NDUM)
        RKO(N)=RKO(N+NDUM)
        TIMEOB(N)=TIMEOB(N+NDUM)
        nlevs_ob(n)=nlevs_ob(n+ndum)
        lev_in_ob(n)=lev_in_ob(n+ndum)
        plfo(n)=plfo(n+ndum)
      ENDDO
    ENDIF
! moved obs up. now fill remaining space with 99999.
    NOPEN=NMOVE+1
    IF(NOPEN.LE.NIOBF) THEN
      DO N=NOPEN,NIOBF
        do KN = 1,nndgv
          VAROBS(KN,N)=99999.
        enddo
        RIO(N)=99999.
        RJO(N)=99999.
        RKO(N)=99999.
        TIMEOB(N)=99999.
      ENDDO
    ENDIF
  ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  NSTA=0
! print *,'nsta at restart setting is ',nsta
! recalculate nsta after moving things around
  recalc : DO N=1,NIOBF
! try to save all obs - don't throw away latest read in
    IF(TIMEOB(N).GT.9.e4) EXIT recalc
    NSTA=N
!   nsta=n-1         ! yliu test
  ENDDO recalc

! Find the number of stations that are actually within the time window.
  nstaw = nvals_le_limit(nsta, timeob, tforwd)

  IF (iprt) then
      write(msg,160) KTAU,XTIME,NSTAW
      call wrf_message(msg)
  ENDIF
  IF(KTAU.EQ.KTAUR)THEN
    IF(nudge_opt.EQ.1)THEN
      TWDOP=TWINDO*60.
      IF (iprt) THEN
        write(msg,1449) INEST,RINXY,RINSIG,TWDOP
        call wrf_message(msg)
        IF(ISWIND.EQ.1) then
          write(msg,1450) GIV
          call wrf_message(msg)
        ELSE
          write(msg,1455) INEST
          call wrf_message("")
          call wrf_message(msg)
          call wrf_message("")
        ENDIF
        IF(ISTEMP.EQ.1) then
          write(msg,1451) GIT
          call wrf_message(msg)
        ELSE
          write(msg,1456) INEST
          call wrf_message("")
          call wrf_message(msg)
        ENDIF
        IF(ISMOIS.EQ.1) then
          call wrf_message("")
          write(msg,1452) GIQ
          call wrf_message(msg)
        ELSE
          write(msg,1457) INEST
          call wrf_message("")
          call wrf_message(msg)
          call wrf_message("")
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF(KTAU.EQ.KTAUR)THEN
    IF(fdob%IWTSIG.NE.1)THEN
      IF (iprt) THEN
        write(msg,555)
        call wrf_message(msg)
        write(msg,556) fdob%RINFMN*RINXY,fdob%RINFMX*RINXY,fdob%PFREE*10.
        call wrf_message(msg)
      ENDIF
      IF(fdob%RINFMN.GT.fdob%RINFMX) then
         call wrf_error_fatal ( 'wrf_fddaobs_in: in4dob STOP 556' )
      ENDIF
! IS MINIMUM GREATER THAN MAXIMUM?

      IF (iprt) then
        write(msg,557) fdob%DPSMX*10.,fdob%DCON
        call wrf_message(msg)
      ENDIF
      IF(fdob%DPSMX.GT.10.) then
         call wrf_error_fatal ( 'wrf_fddaobs_in: in4dob STOP 557' )
      ENDIF
    ENDIF
  ENDIF
 
  IF(KTAU.EQ.KTAUR)THEN
    IF (iprt) then
      write(msg,601) INEST,IONF
      call wrf_message(msg)
      call wrf_message("")
    ENDIF
  ENDIF
  fdob%NSTAT=NSTA
  fdob%NSTAW=NSTAW

555   FORMAT(1X,'   ABOVE THE SURFACE LAYER, OBS NUDGING IS PERFORMED',  &
      ' ON PRESSURE LEVELS,')
556   FORMAT(1X,'   WHERE RINXY VARIES LINEARLY FROM ',E11.3,' KM AT',   &
      ' THE SURFACE TO ',E11.3,' KM AT ',F7.2,' MB AND ABOVE')
557   FORMAT(1X,'   IN THE SURFACE LAYER, WXY IS A FUNCTION OF ',        &
      'DPSMX = ',F7.2,' MB WITH DCON = ',E11.3,                          &
      ' - SEE SUBROUTINE NUDOB')
601   FORMAT('FOR EFFICIENCY, THE OBS NUDGING FREQUENCY ',               &
        'FOR MESH #',I2,' IS ',1I2,' CGM TIMESTEPS ')
121   FORMAT('  WARNING: NOBS  = ',I4,' IS GREATER THAN NIOBF = ',       &
      I4,': INCREASE PARAMETER NIOBF')
5403  FORMAT(1H0,'-------------EOF REACHED FOR NVOL = ',I3,              &
       ' AND XTIME = ',F10.2,'-------------------')
160   FORMAT('****** CALL IN4DOB AT KTAU = ',I5,' AND XTIME = ',         &
      F10.2,':  NSTA = ',I7,' ******')
1449  FORMAT('*****NUDGING INDIVIDUAL OBS ON MESH #',I2,                 &
       ' WITH RINXY = ',                                                 &
      E11.3,' KM, RINSIG = ',E11.3,' AND TWINDO (HALF-PERIOD) = ',       &
      E11.3,' MIN')
1450  FORMAT(1X,'NUDGING IND. OBS WINDS WITH GIV = ',E11.3)
1451  FORMAT(1X,'NUDGING IND. OBS TEMPERATURE WITH GIT = ',E11.3)
1452  FORMAT(1X,'NUDGING IND. OBS MOISTURE WITH GIQ = ',E11.3)
1455  FORMAT(1X,'*** OBS WIND NUDGING FOR MESH ',I2,' IS TURNED OFF!!')
1456  FORMAT(1X,'*** OBS TEMPERATURE NUDGING FOR MESH ',I2,' IS TURNED OFF!!')
1457  FORMAT(1X,'*** OBS MOISTURE NUDGING FOR MESH ',I2,' IS TURNED OFF!!')

  RETURN
  END SUBROUTINE in4dob

  SUBROUTINE julgmt(mdate,julgmtn,timanl,julday,gmt,ind)
! CONVERT MDATE YYMMDDHH TO JULGMT (JULIAN DAY * 100. +GMT)
! AND TO TIMANL (TIME IN MINUTES WITH RESPECT TO MODEL TIME)
! IF IND=0  INPUT MDATE, OUTPUT JULGMTN AND TIMANL
! IF IND=1  INPUT TIMANL, OUTPUT JULGMTN
! IF IND=2  INPUT JULGMTN, OUTPUT TIMANL
      INTEGER, intent(in) :: MDATE
      REAL, intent(out) :: JULGMTN
      REAL, intent(out) :: TIMANL
      INTEGER, intent(in) :: JULDAY
      REAL, intent(in) :: GMT
      INTEGER, intent(in) :: IND 

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real :: MO(12), rjulanl, houranl, rhr

      integer :: iyr, idate1, imo, idy, ihr, my1, my2, my3, ileap
      integer :: juldayn, juldanl, idymax, mm
      
      
      IF(IND.EQ.2)GOTO 150
      IYR=INT(MDATE/1000000.+0.001)
      IDATE1=MDATE-IYR*1000000
      IMO=INT(IDATE1/10000.+0.001)
      IDY=INT((IDATE1-IMO*10000.)/100.+0.001)
      IHR=IDATE1-IMO*10000-IDY*100
      MO(1)=31
      MO(2)=28
! IS THE YEAR A LEAP YEAR? (IN THIS CENTURY)
      IYR=IYR+1900
      MY1=MOD(IYR,4)
      MY2=MOD(IYR,100)
      MY3=MOD(IYR,400)
      ILEAP=0
! jc
!      IF(MY1.EQ.0.AND.MY2.NE.0.OR.MY3.EQ.0)THEN
      IF(MY1.EQ.0)THEN
        ILEAP=1
        MO(2)=29
      ENDIF
      IF(IND.EQ.1)GOTO 200
      MO(3)=31
      MO(4)=30
      MO(5)=31
      MO(6)=30
      MO(7)=31
      MO(8)=31
      MO(9)=30
      MO(10)=31
      MO(11)=30
      MO(12)=31
      JULDAYN=0
      DO 100 MM=1,IMO-1
        JULDAYN=JULDAYN+MO(MM)
 100     CONTINUE

      IF(IHR.GE.24)THEN
        IDY=IDY+1
        IHR=IHR-24
      ENDIF
      JULGMTN=(JULDAYN+IDY)*100.+IHR
! CONVERT JULGMT TO TIMANL WRT MODEL TIME IN MINUTES (XTIME)
 150   CONTINUE
      JULDANL=INT(JULGMTN/100.+0.000001)
      RJULANL=FLOAT(JULDANL)*100.
      HOURANL=JULGMTN-RJULANL
      TIMANL=(FLOAT(JULDANL-JULDAY)*24.-GMT+HOURANL)*60.
      RETURN
 200   CONTINUE
      RHR=GMT+TIMANL/60.+0.000001
      IDY=JULDAY
      IDYMAX=365+ILEAP
 300   IF(RHR.GE.24.0)THEN
        RHR=RHR-24.0
        IDY=IDY+1
        GOTO 300
      ENDIF
      IF(IDY.GT.IDYMAX)IDY=IDY-IDYMAX
      JULGMTN=FLOAT(IDY)*100.+RHR
      RETURN
  END SUBROUTINE julgmt

  SUBROUTINE rh2r(rh,t,p,r,iice)
 
! convert rh to r
! if iice=1, use saturation with respect to ice
! rh is 0-100.
! r is g/g
! t is K
! p is mb
!
      REAL, intent(in)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(out) :: r
      INTEGER, intent(in)  :: iice

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real eps, e0, eslcon1, eslcon2, esicon1, esicon2, t0, rh1
      real esat, rsat

      eps=0.62197
      e0=6.1078
      eslcon1=17.2693882
      eslcon2=35.86
      esicon1=21.8745584
      esicon2=7.66
      t0=260.
 
!     print *,'rh2r input=',rh,t,p
      rh1=rh*.01
 
      if(iice.eq.1.and.t.le.t0)then
        esat=e0*exp(esicon1*(t-273.16)/(t-esicon2))
      else
        esat=e0*exp(eslcon1*(t-273.16)/(t-eslcon2))
      endif
      rsat=eps*esat/(p-esat)
!     print *,'rsat,esat=',rsat,esat
      r=rh1*rsat
 
!      print *,'rh2r rh,t,p,r=',rh1,t,p,r
 
      return
  END SUBROUTINE rh2r

  SUBROUTINE rh2rb(rh,t,p,r,iice)
 
! convert rh to r
! if iice=1, use daturation with respect to ice
! rh is 0-100.
! r is g/g
! t is K
! p is mb
 
      REAL, intent(in)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(out) :: r
      INTEGER, intent(in)  :: iice

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real eps, e0, eslcon1, eslcon2, esicon1, esicon2, t0, rh1
      real esat, rsat
      character(len=200) :: msg       ! Argument to wrf_message

      eps=0.622
      e0=6.112
      eslcon1=17.67
      eslcon2=29.65
      esicon1=22.514
      esicon2=6.15e3
      t0=273.15
 
      write(msg,*) 'rh2r input=',rh,t,p
      call wrf_message(msg)
      rh1=rh*.01
 
      if(iice.eq.1.and.t.le.t0)then
        esat=e0*exp(esicon1-esicon2/t)
      else
        esat=e0*exp(eslcon1*(t-t0)/(t-eslcon2))
      endif
      rsat=eps*esat/(p-esat)
!     print *,'rsat,esat=',rsat,esat
      r=rh1*eps*rsat/(eps+rsat*(1.-rh1))
 
      write(msg,*) 'rh2r rh,t,p,r=',rh1,t,p,r
      call wrf_message(msg)
      rh1=rh*.01
 
      return
END SUBROUTINE rh2rb

  SUBROUTINE set_projection (obs_proj, map_proj, cen_lat, cen_lon,     &
                             true_lat1, true_lat2, stand_lon,          &
                             known_lat, known_lon,                     &
                             e_we, e_sn, dxm, dym )

  USE module_llxy

!*************************************************************************
! Purpose: Set map projection information which will be used to map the
!          observation (lat,lon) location to its corresponding (x,y)
!          location on the WRF (coarse) grid. using the selected map
!          projection (e.g., Lambert, Mercator, Polar Stereo, etc).
!*************************************************************************

      IMPLICIT NONE

  TYPE(PROJ_INFO), intent(out)  :: obs_proj   ! structure for obs projection info.
  INTEGER, intent(in) :: map_proj             ! map projection index
  REAL, intent(in) :: cen_lat                 ! center latitude for map projection
  REAL, intent(in) :: cen_lon                 ! center longiture for map projection
  REAL, intent(in) :: true_lat1               ! truelat1 for map projection
  REAL, intent(in) :: true_lat2               ! truelat2 for map projection
  REAL, intent(in) :: stand_lon               ! standard longitude for map projection
  INTEGER, intent(in) :: e_we                 ! max grid index in south-north coordinate
  INTEGER, intent(in) :: e_sn                 ! max grid index in west-east   coordinate
  REAL, intent(in) :: known_lat               ! latitude  of domain origin point (i,j)=(1,1) 
  REAL, intent(in) :: known_lon               ! longigude of domain origin point (i,j)=(1,1)
  REAL, intent(in) :: dxm                     ! grid size in x (meters)
  REAL, intent(in) :: dym                     ! grid size in y (meters)

! Set up map transformation structure
      CALL map_init(obs_proj)

      ! Mercator
      IF (map_proj == PROJ_MERC) THEN
         CALL map_set(PROJ_MERC, obs_proj,                                &
                      truelat1 = true_lat1,                               &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      dx       = dxm)

      ! Lambert conformal
      ELSE IF (map_proj == PROJ_LC) THEN
      CALL map_set(PROJ_LC, obs_proj,                                     &
                      truelat1 = true_lat1,                               &
                      truelat2 = true_lat2,                               &
                      stdlon   = stand_lon,                               &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      dx       = dxm)

      ! Polar stereographic
      ELSE IF (map_proj == PROJ_PS) THEN
         CALL map_set(PROJ_PS, obs_proj,                                  &
                      truelat1 = true_lat1,                               &
                      stdlon   = stand_lon,                               &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      dx       = dxm)
      ! Cassini (global ARW)
      ELSE IF (map_proj == PROJ_CASSINI) THEN
         CALL map_set(PROJ_CASSINI, obs_proj,                             &
                      latinc   = dym*360.0/(2.0*EARTH_RADIUS_M*PI),       &
                      loninc   = dxm*360.0/(2.0*EARTH_RADIUS_M*PI),       &
                      lat1     = known_lat,                               &
                      lon1     = known_lon,                               &
! We still need to get POLE_LAT and POLE_LON metadata variables before
!   this will work for rotated poles.
                      lat0     = 90.0,                                    &
                      lon0     = 0.0,                                     &
                      knowni   = 1.,                                      &
                      knownj   = 1.,                                      &
                      stdlon   = stand_lon)

      ! Rotated latitude-longitude
      ELSE IF (map_proj == PROJ_ROTLL) THEN
         CALL map_set(PROJ_ROTLL, obs_proj,                               &
! I have no idea how this should work for NMM nested domains
                      ixdim    = e_we-1,                                  &
                      jydim    = e_sn-1,                                  &
                      phi      = real(e_sn-2)*dym/2.0,                    &
                      lambda   = real(e_we-2)*dxm,                        &
                      lat1     = cen_lat,                                 &
                      lon1     = cen_lon,                                 &
                      latinc   = dym,                                     &
                      loninc   = dxm,                                     &
                      stagger  = HH)

      END IF

  END SUBROUTINE set_projection

  SUBROUTINE fmt_date(idate,odate)                                             !obsnypatch

!*************************************************************************
! Purpose: Re-format a character date string from YYYYMMDDHHmmss form
!          to YYYY-MM-DD_HH:mm:ss form.
! INPUT:
!     IDATE - Date string as YYYYMMDDHHmmss
! OUTPUT:
!     ODATE - Date string as YYYY-MM-DD_HH:mm:ss
!*************************************************************************

      IMPLICIT NONE

      CHARACTER*14, intent(in)  :: idate        ! input  date string
      CHARACTER*19, intent(out) :: odate        ! output date string

      odate(1:19) = "0000-00-00_00:00:00"
      odate(1:4)   = idate(1:4)                 ! Year
      odate(6:7)   = idate(5:6)                 ! Month
      odate(9:10)  = idate(7:8)                 ! Day
      odate(12:13) = idate(9:10)                ! Hours
      odate(15:16) = idate(11:12)               ! Minutes
      odate(18:19) = idate(13:14)               ! Seconds

      RETURN
  END SUBROUTINE fmt_date

  INTEGER FUNCTION nvals_le_limit(isize, values, limit)
!------------------------------------------------------------------------------
! PURPOSE: Return the number of values in a (real) non-decreasing array that
!          are less than or equal to the specified upper limit.
! NOTE: It is important that the array is non-decreasing!
!      
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: isize           ! Size of input array
  REAL,    INTENT(IN) :: values(isize)   ! Input array of reals
  REAL,    INTENT(IN) :: limit           ! Upper limit

! Local variables
  integer :: n

! Search the array from largest to smallest values.
   find_nvals: DO n = isize, 1, -1
                 if(values(n).le.limit) EXIT find_nvals
               ENDDO find_nvals
  nvals_le_limit = n

  RETURN
  END FUNCTION nvals_le_limit

  SUBROUTINE collect_obs_info(newpass,inest,n,latitude,longitude,             &
                              nlast,nprev,niobf,station_id,stnid,             &
                              rio,rjo,prt_max,prt_freq,xlat,xlong,            &
                              obs, lat,lon, mlat,mlon,                        &
                              e_we,e_sn,ims,ime,jms,jme,its,ite,jts,jte)
!*************************************************************************
! Purpose: Collect the obs index, obs latitude, obs longitude, obs station
!          id, and model latitude and longitude values for print
!          diagnostics. Note that THIS SUBROUTINE IS CALLED INTERATIVELY
!          FROM IN4DOB, WITHIN THE OBS READ LOOP that reads new obser-
!          vations needed for the new time window. Flag newpass is true
!          the first time collect_obs_info is called from the read-loop
!          for a new time window. So for each pass of IN4DOB, newpass is
!          true the first time IN4DOB calls collec_obs_info.

!          OBS (soundings) contain multiple obs levels. So on each sub-
!          sequent call of collect_obs_info for a specific pass of IN4DOB,
!          n will jump by the number of levels in the sounding.
!
!          Here, nlast refers to the index of the last valid-time obs
!          that was read in during the last pass of IN4DOB (after the old
!          obs were removed). This way we can properly start storing
!          obs information for the new obs that are being read on this
!          pass of IN4DOB, beginning with the first newly read obs for
!          this pass of IN4DOB.
!
!          Note that nprev is needed to properly handle soundings. On
!          each pass, n is stored into nprev, and on each subsequent
!          pass of collect_obs_info, a loop is performed from nprev+1 to n.
!*************************************************************************

  IMPLICIT NONE

  LOGICAL, intent(inout) :: newpass        ! New pass flag
  INTEGER, intent(in)    :: inest          ! nest index
  INTEGER, intent(in)    :: n              ! Observation index
  REAL,    intent(in)    :: latitude       ! Latitude of obs
  REAL,    intent(in)    :: longitude      ! Latitude of obs
  INTEGER, intent(in)    :: nlast          ! Last obs of valid obs, prev window
  INTEGER, intent(inout) :: nprev          ! Previous obs in new window read seq
  INTEGER, intent(in)    :: niobf          ! Maximum number of observations
  CHARACTER*15, intent(in) :: station_id   ! First 15 chars of station id for obs n
  INTEGER, intent(in)    :: prt_max        ! Max no. of obs for diagnostic printout
  INTEGER, intent(inout) :: stnid(40,prt_max) ! Station ids for diagnostic printout
  REAL,    intent(in)    :: rio(niobf)     ! West-east coord (non-stagger)
  REAL,    intent(in)    :: rjo(niobf)     ! South-north coord (non-stagger)
  INTEGER, intent(in)    :: prt_freq       ! Frequency for diagnostic printout
  REAL, DIMENSION( ims:ime, jms:jme ),                                   &
           intent(in )   :: xlat, xlong    ! Lat/lon on mass-pt grid
  INTEGER, intent(inout) :: obs(prt_max)   ! Obs index for printout
  REAL,    intent(inout) :: lat(prt_max)   ! Obs latitude for printout
  REAL,    intent(inout) :: lon(prt_max)   ! Obs longitude for printout
  REAL,    intent(inout) :: mlat(prt_max)  ! Model latitude at (rio,rjo) for printout
  REAL,    intent(inout) :: mlon(prt_max)  ! Model longitude at (rio,rjo) for printout
  INTEGER, intent(in)    :: e_we           ! Max grid index in south-north
  INTEGER, intent(in)    :: e_sn           ! Max grid index in west-east
  INTEGER, intent(in)    :: ims            ! Grid mem start (west-east)
  INTEGER, intent(in)    :: ime            ! Grid mem end   (west-east)
  INTEGER, intent(in)    :: jms            ! Grid mem start (south-north)
  INTEGER, intent(in)    :: jme            ! Grid mem end   (south-north)
  INTEGER, intent(in)    :: its            ! Grid tile start (west-east)
  INTEGER, intent(in)    :: ite            ! Grid tile end   (west-east)
  INTEGER, intent(in)    :: jts            ! Grid tile start (south-north)
  INTEGER, intent(in)    :: jte            ! Grid tile end   (south-north)

! Local variables
  integer i                       ! Loop counter over station id character
  integer nn                      ! Loop counter over obs index
  integer ndx,ndxp                ! Index into printout arrays (ndx and prev ndx)
  real    :: ri, rj               ! Mass-pt coord of obs
  integer :: ril, rjl             ! Mass-pt integer coord immed sw of obs
  integer :: iend, jend           ! Upper i, j index for interpolation
  real    :: dxob, dyob           ! Grid fractions for interp
  logical :: llsave               ! Save lat/lon values if true
  character(len=200) :: msg       ! Argument to wrf_message

  if(newpass) then
    newpass = .false.
    nprev = nlast       ! Reset in case old obs have been discarded from prev window
  endif

! Start iteration only if we have not yet stored prt_max number of obs for printing.
! Note: The loop below could represent multiple levels in a sounding, so we
!       go ahead and start the loop if the beginning index (ndx) is prt_max or
!       less, and then exit the loop if ndx exceeds prt_max.
    if(prt_freq.gt.0) then
       ndx  = (n-nlast-1)/prt_freq + 1
       ndxp = (nprev-nlast-1)/prt_freq + 1
    else
       write(msg,*) 'STOP! OBS NAMELIST INPUT obs_prt_freq MUST BE GREATER THAN ZERO.'
       call wrf_message(msg)
       write(msg,*) 'THE NAMELIST VALUE IS',prt_freq,' FOR NEST ',inest
       call wrf_message(msg)
       call wrf_error_fatal ( 'wrf_fddaobs_in: in4dob STOP' )
    endif

!   write(6,'5(a,i5),a,a15') 'n = ',n,'  nlast = ',nlast,'  ndx = ',ndx,  &
!                            '  nprev = ',nprev,'  ndxp = ',ndxp,         &
!                            '  station id = ',station_id

    if(ndxp .lt. prt_max) then

   MODCHK : do nn = nprev+1, n
        llsave = .false.

!       if( mod(nn-1,prt_freq) .eq. 0 ) then
        if( mod(nn-nlast-1,prt_freq) .eq. 0 ) then
           ndx = (nn-nlast-1)/prt_freq + 1
           if(ndx.gt.prt_max) EXIT MODCHK       ! Limit printout to prt_max entries
           llsave = .true.
        endif
        if(llsave) then

! Collect obs index and latitude and longitude.
          obs(ndx) = nn
          lat(ndx) = latitude
          lon(ndx) = longitude

! Collect first 15 chars of obs station id (in integer format).
          do i = 1,15
            stnid(i,ndx) = ichar(station_id(i:i))
          enddo

! Compute and collect the model latitude and longitude at the obs point.
          CALL get_model_latlon(nn,niobf,rio,rjo,xlat,xlong,e_we,e_sn,    &
                                ims,ime,jms,jme,its,ite,jts,jte,          &
                                mlat(ndx),mlon(ndx))
        endif  !end if(llsave)
      enddo MODCHK

    endif  !end if(ndx .le. prt_max)

! Save index of previous obs in read loop.
    nprev = n

  END SUBROUTINE collect_obs_info

  SUBROUTINE get_model_latlon(n,niobf,rio,rjo,xlat,xlong,e_we,e_sn,   &
                              ims,ime,jms,jme,its,ite,jts,jte,        &
                              mlat,mlon)
!*************************************************************************
! Purpose: Use bilinear interpolation to compute the model latitude and 
!          longitude at the observation point.
!*************************************************************************

  IMPLICIT NONE

  INTEGER, intent(in)    :: n              ! Observation index
  INTEGER, intent(in)    :: niobf          ! Maximum number of observations
  REAL,    intent(in)    :: rio(niobf)     ! West-east coord (non-stagger)
  REAL,    intent(in)    :: rjo(niobf)     ! South-north coord (non-stagger)
  REAL, DIMENSION( ims:ime, jms:jme ),                                   &
           intent(in )   :: xlat, xlong    ! Lat/lon on mass-pt grid
  INTEGER, intent(in)    :: e_we           ! Max grid index in south-north
  INTEGER, intent(in)    :: e_sn           ! Max grid index in west-east
  INTEGER, intent(in)    :: ims            ! Grid mem start (west-east)
  INTEGER, intent(in)    :: ime            ! Grid mem end   (west-east)
  INTEGER, intent(in)    :: jms            ! Grid mem start (south-north)
  INTEGER, intent(in)    :: jme            ! Grid mem end   (south-north)
  INTEGER, intent(in)    :: its            ! Grid tile start (west-east)
  INTEGER, intent(in)    :: ite            ! Grid tile end   (west-east)
  INTEGER, intent(in)    :: jts            ! Grid tile start (south-north)
  INTEGER, intent(in)    :: jte            ! Grid tile end   (south-north)
  REAL,    intent(out)   :: mlat           ! Model latitude at obs point
  REAL,    intent(out)   :: mlon           ! Model longitude at obs point

! Local variables
  integer ndx                     ! Index into save arrays
  real    :: ri, rj               ! Mass-pt coord of obs 
  integer :: ril, rjl             ! Mass-pt integer coord immed sw of obs 
  integer :: iend, jend           ! Upper i, j index for interpolation
  real    :: dxob, dyob           ! Grid fractions for interp

! Compute model latitude and longitude if point on tile.
  ri  = rio(n) - .5            ! mass-pt west-east obs grid coord
  rj  = rjo(n) - .5            ! mass-pt south-north obs grid coord
  ril = int(ri)
  rjl = int(rj)
  dxob = ri - float(ril)
  dyob = rj - float(rjl)
  iend = min(ite+1,e_we-2)
  jend = min(jte+1,e_sn-2)
  mlat = -999
  mlon = -999

  if(ri.ge.its .and. ri.lt.iend .and. rj.ge.jts .and. rj.lt.jend) then

! bilinear interpolation
     mlat = ((1.-dyob)*((1.-dxob)*xlat(ril,rjl)+             &
            dxob*xlat(ril+1,rjl)                             &
            )+dyob*((1.-dxob)*xlat(ril,rjl+1)+               &
            dxob*xlat(ril+1,rjl+1)))

     mlon = ((1.-dyob)*((1.-dxob)*xlong(ril,rjl)+            &
            dxob*xlong(ril+1,rjl)                            &
            )+dyob*((1.-dxob)*xlong(ril,rjl+1)+              &
            dxob*xlong(ril+1,rjl+1)))
  endif

  END SUBROUTINE get_model_latlon

  SUBROUTINE rotate_vector(lon,u,v,obs_proj,map_proj)

  USE module_llxy

!*************************************************************************
! Purpose: Rotate a single Earth-relative wind vector to a grid-relative
!          wind vector.
!*************************************************************************

  IMPLICIT NONE

  REAL,           intent(in)    :: lon        ! Longitude (deg)
  REAL,           intent(inout) :: u          ! U-component of wind vector
  REAL,           intent(inout) :: v          ! V-component of wind vector
  TYPE(PROJ_INFO),intent(in)    :: obs_proj   ! Structure for obs projection
  INTEGER,        intent(in)    :: map_proj   ! Map projection index

! Local variables
  real diff, alpha
  double precision udbl, vdbl

! Only rotate winds for Lambert conformal or polar stereographic
  if (map_proj == PROJ_LC .or. map_proj == PROJ_PS) then

     diff = obs_proj%stdlon - lon
     if (diff > 180.) then
        diff = diff - 360.
     else if (diff < -180.) then
        diff = diff + 360.
     end if

! Calculate the rotation angle, alpha, in radians
     if (map_proj == PROJ_LC) then
        alpha = diff * obs_proj%cone * rad_per_deg * obs_proj%hemi
     else
        alpha = diff * rad_per_deg * obs_proj%hemi
     end if

     udbl = v*sin(alpha) + u*cos(alpha)
     vdbl = v*cos(alpha) - u*sin(alpha)
     u = udbl
     v = vdbl

  endif
  END SUBROUTINE rotate_vector

#endif
!-----------------------------------------------------------------------
! End subroutines for in4dob
!-----------------------------------------------------------------------
