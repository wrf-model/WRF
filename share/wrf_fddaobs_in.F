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

    IF( (ktau.GT.krest.AND.MOD(ktau,infreq).EQ.0)                            &
                                         .OR.(ktau.EQ.krest) ) then
! Calculate forecast time.
      dtmin = grid%dt/60.
      xtime = dtmin*grid%itimestep

      CALL in4dob(inest, xtime, ktau, krest, dtmin, grid%julday, grid%gmt,       &
                  grid%obs_nudge_opt,  grid%obs_nudge_wind, grid%obs_nudge_temp, &
                  grid%obs_nudge_mois, grid%obs_nudge_pstr, grid%obs_coef_wind,  & 
                  grid%obs_coef_temp,  grid%obs_coef_mois,  grid%obs_coef_pstr,  &
                  grid%obs_rinxy,      grid%obs_rinsig,     grid%obs_twindo,     &
                  grid%obs_npfi,       grid%obs_ionf,       grid%obs_idynin,     &
                  grid%obs_dtramp,     grid%fdob,           grid%fdob%varobs,    &
                  grid%fdob%timeob,    grid%fdob%nlevs_ob,  grid%fdob%lev_in_ob, &
                  grid%fdob%plfo,      grid%fdob%elevob,    grid%fdob%rio,       &
                  grid%fdob%rjo,       grid%fdob%rko,                            & 
                  model_config_rec%cen_lat(1),                                   &
                  model_config_rec%cen_lon(1),                                   &
                  config_flags%truelat1, config_flags%truelat2,                  &
                  rovg, grid%fdob%xn, grid%fdob%ds_cg, t0,                       &
                  grid%fdob%sn_maxcg, grid%fdob%we_maxcg, config_flags%map_proj, &
                  model_config_rec%parent_grid_ratio,                            &
                  model_config_rec%i_parent_start(inest),                        &
                  model_config_rec%j_parent_start(inest),                        &
                  model_config_rec%nobs_ndg_vars, grid%max_obs, iprt_in4dob)
    ENDIF

    RETURN
#endif
  END SUBROUTINE wrf_fddaobs_in
#if ( EM_CORE == 1 )
!------------------------------------------------------------------------------
! Begin subroutine in4dob and its subroutines
!------------------------------------------------------------------------------
  SUBROUTINE in4dob(inest, xtime, ktau, ktaur, dtmin, julday, gmt,       &
                    nudge_opt, iswind, istemp,                           &
                    ismois, ispstr, giv,                                 &
                    git, giq, gip,                                       &
                    rinxy, rinsig, twindo,                               &
                    npfi, ionf, idynin,                                  &
                    dtramp, fdob, varobs,                                &
                    timeob, nlevs_ob, lev_in_ob,                         &
                    plfo, elevob, rio,                                   &
                    rjo, rko,                                            &
                    xlatc_cg,                                            &
                    xlonc_cg,                                            &
                    true_lat1, true_lat2,                                &
                    rovg, xn, dscg, t0,                                  &
                    sn_maxcg, we_maxcg, map_proj,                        &
                    parent_grid_ratio,                                   &
                    i_parent_start,                                      &
                    j_parent_start,                                      &
                    nndgv, niobf, iprt)

  USE module_domain
  USE module_model_constants, ONLY : rcp
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

! NOTE - IN4DOB IS CALLED ONLY FOR THE COARSE MESH TIMES                         IN4DOB.10
!      - VAROBS(IVAR,N) HOLDS THE OBSERVATIONS.                                  IN4DOB.11
!        IVAR=1   UOBS                                                           IN4DOB.12
!        IVAR=2   VOBS                                                           IN4DOB.13
!        IVAR=3   TOBS                                                           IN4DOB.14
!        IVAR=4   QOBS                                                           IN4DOB.15
!        IVAR=5   PSOBS (CROSS)                                                  IN4DOB.16

  INTEGER, intent(in) :: niobf          ! maximum number of observations
  INTEGER, intent(in) :: nndgv          ! number of nudge variables
  INTEGER, intent(in)  :: INEST         ! nest level
  REAL, intent(in)     :: xtime         ! model time in minutes
  INTEGER, intent(in)  :: KTAU          ! current timestep
  INTEGER, intent(in)  :: KTAUR         ! restart timestep
  REAL, intent(in)     :: dtmin         ! dt in minutes
  INTEGER, intent(in)  :: julday        ! Julian day
  REAL, intent(in)     :: gmt           ! Greenwich Mean Time
  INTEGER, intent(in)  :: nudge_opt     ! obs-nudge flag for this nest
  INTEGER, intent(in)  :: iswind        ! nudge flag for wind
  INTEGER, intent(in)  :: istemp        ! nudge flag for temperature
  INTEGER, intent(in)  :: ismois        ! nudge flag for moisture
  INTEGER, intent(in)  :: ispstr        ! nudge flag for pressure
  REAL, intent(in)     :: giv           ! coefficient for wind
  REAL, intent(in)     :: git           ! coefficient for temperature
  REAL, intent(in)     :: giq           ! coefficient for moisture
  REAL, intent(in)     :: gip           ! coefficient for pressure
  REAL, intent(in)     :: rinxy         ! horizontal radius of influence (km)
  REAL, intent(in)     :: rinsig        ! vertical radius of influence (on sigma)
  REAL, intent(in)     :: twindo        ! (time window)/2 (min) for nudging
  INTEGER, intent(in)  :: npfi          ! coarse-grid time-step frequency for diagnostics
  INTEGER, intent(in)  :: ionf          ! coarse-grid time-step frequency for obs-nudging calcs
  INTEGER, intent(in)  :: idynin        ! for dynamic initialization using a ramp-down function
  REAL, intent(in)     :: dtramp        ! time period in minutes for ramping
  TYPE(fdob_type), intent(inout)  :: fdob     ! derived data type for obs data
  REAL, intent(inout) :: varobs(nndgv,niobf)  ! observational values in each variable
  REAL, intent(inout) :: timeob(niobf)        ! model times for each observation (hours)
  REAL, intent(inout) :: nlevs_ob(niobf)      ! numbers of levels in sounding obs
  REAL, intent(inout) :: lev_in_ob(niobf)     ! level in sounding-type obs
  REAL, intent(inout) :: plfo(niobf)          ! index for type of obs-platform
  REAL, intent(inout) :: elevob(niobf)        ! elevations of observations  (meters)
  REAL, intent(inout) :: rio(niobf)           ! west-east grid coordinate
  REAL, intent(inout) :: rjo(niobf)           ! south-north grid coordinate
  REAL, intent(inout) :: rko(niobf)           ! vertical grid coordinate
  REAL, intent(in) :: xlatc_cg                ! coarse grid center latitude 
  REAL, intent(in) :: xlonc_cg                ! coarse grid center longiture
  REAL, intent(in) :: true_lat1               ! truelat1 for Lambert map projection
  REAL, intent(in) :: true_lat2               ! truelat2 for Lambert map projection
  REAL, intent(in) :: rovg                    ! constant rho over g
  REAL, intent(in) :: xn                      ! cone factor for Lambert projection
  REAL, intent(in) :: dscg                    ! coarse grid size (km)
  REAL, intent(in) :: t0                      ! background temperature
  INTEGER, intent(in) :: sn_maxcg             ! maximum coarse grid south-north coordinate
  INTEGER, intent(in) :: we_maxcg             ! maximum coarse grid west-east   coordinate
  INTEGER, intent(in) :: map_proj             ! map projection index
  INTEGER, intent(in) :: parent_grid_ratio    ! parent to nest grid ration
  INTEGER, intent(in) :: i_parent_start       ! starting i coordinate in parent domain
  INTEGER, intent(in) :: j_parent_start       ! starting j coordinate in parent domain
  LOGICAL, intent(in) :: iprt                 ! print flag
      
!***  DECLARATIONS FOR IMPLICIT NONE                                    
  integer :: n, nsta, ndum, nopen, nlast, nvol, idate, imm, iss
  integer :: meas_count, imc, njend, njc, njcc, julob
  real    :: hourob, rjulob
  real    :: xhour, tback, tforwd, rjdate1, timanl1, rtimob
  real    :: rj, ri, elevation, pressure_data
  real    :: pressure_qc, height_data, height_qc, temperature_data
  real    :: temperature_qc, u_met_data, u_met_qc, v_met_data
  real    :: v_met_qc, rh_data, rh_qc, r_data, slp_data, slp_qc
  real    :: ref_pres_data, ref_pres_qc, psfc_data, psfc_qc
  real    :: precip_data, precip_qc, tbar, twdop
  real*8  :: tempob

! Local variables
  character*14 date_char
  character*40 platform,source,id,namef
  character*2 fonc
  real latitude,longitude
  logical is_sound,bogus
  LOGICAL OPENED,exist
  integer :: ieof(5),ifon(5)
  data ieof/0,0,0,0,0/
  data ifon/0,0,0,0,0/
  integer :: nmove, nvola
  DATA NMOVE/0/,NVOLA/61/

  if(ieof(inest).eq.2.and.fdob%nstat.eq.0)then
    IF (iprt) print *,'returning from in4dob'
    return
  endif
  IF (iprt) print *,'start in4dob ',inest,xtime
  IF(nudge_opt.NE.1)RETURN

! if start time, or restart time, set obs array to missing value
  IF(KTAU.EQ.0.OR.KTAU.EQ.KTAUR) THEN
    DO N=1,NIOBF
      TIMEOB(N)=99999.
    ENDDO
  ENDIF
! set number of obs=0 if at start or restart
  IF(KTAU.EQ.KTAUR)fdob%NSTAT=0
  NSTA=fdob%NSTAT
  fdob%WINDOW=TWINDO
  XHOUR=(XTIME-DTMIN)/60.
  XHOUR=AMAX1(XHOUR,0.0)

10 CONTINUE

! DEFINE THE MAX LIMITS OF THE WINDOW
  TBACK=XHOUR-fdob%WINDOW
  TFORWD=XHOUR+fdob%WINDOW

      if (iprt) write(6,*) 'TBACK = ',tback,' TFORWD = ',tforwd

  IF(NSTA.NE.0) THEN
    NDUM=0
    t_window : DO N=1,NSTA+1
      IF((TIMEOB(N)-TBACK).LT.0) THEN
        TIMEOB(N)=99999.
      ENDIF
      IF(TIMEOB(N).LT.9.E4) EXIT t_window
      NDUM=N
    ENDDO t_window

! REMOVE OLD OBS DENOTED BY 99999. AT THE FRONT OF TIMEOB ARRAY
    IF (iprt) print *,'ndum at 20=',ndum
    NDUM=ABS(NDUM)
    NMOVE=NIOBF-NDUM
    IF(NMOVE.GT.0 .AND. NDUM.NE.0 ) THEN  
      DO N=1,NMOVE
        VAROBS(1,N)=VAROBS(1,N+NDUM)
        VAROBS(2,N)=VAROBS(2,N+NDUM)
        VAROBS(3,N)=VAROBS(3,N+NDUM)
        VAROBS(4,N)=VAROBS(4,N+NDUM)
        VAROBS(5,N)=VAROBS(5,N+NDUM)
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
        VAROBS(1,N)=99999.
        VAROBS(2,N)=99999.
        VAROBS(3,N)=99999.
        VAROBS(4,N)=99999.
        VAROBS(5,N)=99999.
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

! print *,'in4dob, after setting RIO, RJO: nsta = ',nsta

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
    INQUIRE (NVOLA+INEST-1,OPENED=OPENED)
    IF (.NOT. OPENED) THEN
      ifon(inest)=1
      write(fonc(1:2),'(i2)')ifon(inest)
      if(fonc(1:1).eq.' ')fonc(1:1)='0'
      INQUIRE (file='OBS_DOMAIN'//CHAR(INEST+ICHAR('0'))//fonc(1:2)  &
              ,EXIST=exist)
      if(exist)then
        IF (iprt) THEN
          print *,'opening first fdda obs file, fonc=',              &
                   fonc,' inest=',inest
          print *,'ifon=',ifon(inest)
        ENDIF
        OPEN(NVOLA+INEST-1,                                          &
        FILE='OBS_DOMAIN'//CHAR(INEST+ICHAR('0'))//fonc(1:2),        &
              FORM='FORMATTED',STATUS='OLD')
      else
! no first file to open
        IF (iprt) print *,'there are no fdda obs files to open'
        return
      endif

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
! print *,'after 1001,n,timeob(n)=',n,timeob(n)

    IF(IEOF(inest).GT.1) then
      GOTO 130
    endif

100 IF(TIMEOB(N).GT.TFORWD.and.timeob(n).lt.99999.) THEN
       GOTO 130
    ENDIF
 
! OBSFILE: no more data in the obsfile 
    if(ieof(inest).eq.1 )then
      ieof(inest)=2
      goto 130
    endif

!**********************************************************************
!       --------------   110 SUBLOOP OVER N  --------------
!**********************************************************************
! THE TIME OF THE MOST RECENTLY ACQUIRED OBS IS .LE. TFORWD,
! SO CONTINUE READING
  110 continue
      IF(N.GT.NIOBF-1)GOTO 120
! REPLACE NVOLA WITH LUN 70, AND USE NVOLA AS A FILE COUNTER
      NVOL=NVOLA+INEST-1
      IF(fdob%IEODI.EQ.1)GOTO 111
      read(nvol,101,end=111,err=111)date_char
 101  FORMAT(1x,a14)

      n=n+1

      read(date_char(3:10),'(i8)')idate
      read(date_char(11:12),'(i2)')imm
      read(date_char(13:14),'(i2)')iss
! output is rjdate (jjjhh.) and timanl (time in minutes since model start)
      call julgmt(idate,rjdate1,timanl1,julday,gmt,0)
      rtimob=rjdate1+real(imm)/60.+real(iss)/3600.
      timeob(n)=rtimob
      timeob(n) = int(timeob(n)*1000)/1000.0

! CONVERT TIMEOB FROM JULIAN DATE AND GMT FORM TO FORECAST
! TIME IN HOURS (EX. TIMEOB=13002.4 REPRESENTS JULDAY 130
! AND GMT (HOUR) = 2.4)
      JULOB=TIMEOB(N)/100.+0.000001
      RJULOB=FLOAT(JULOB)*100.
      tempob = (timeob(n)*1000.)
      tempob = int(tempob)
      tempob = tempob/1000.
      timeob(n) = tempob
      HOUROB=TIMEOB(N)-RJULOB
      TIMEOB(N)=FLOAT(JULOB-JULDAY)*24.-GMT+HOUROB
      rtimob=timeob(n)

!     print *,'read in ob ',n,timeob(n),rtimob
      IF(IDYNIN.EQ.1.AND.TIMEOB(N)*60..GT.fdob%DATEND) THEN
        IF (iprt) THEN
          PRINT*,' IN4DOB: FOR INEST = ',INEST,' AT XTIME = ',XTIME,    &
          ' TIMEOB = ',TIMEOB(N)*60.,' AND DATEND = ',fdob%DATEND,' :'
          PRINT*,'         END-OF-DATA FLAG SET FOR OBS-NUDGING',       &
          ' DYNAMIC INITIALIZATION'
        ENDIF
        fdob%IEODI=1
        TIMEOB(N)=99999.
        rtimob=timeob(n)
      ENDIF
      read(nvol,102)latitude,longitude
 102  FORMAT(2x,2(f7.2,3x))

!     if(ifon.eq.4)print *,'ifon=4',latitude,longitude
! this works only for lc projection
! yliu: add llxy for all 3 projection
          
!ajb Arguments ri and rj have been switched from MM5 orientation.
!      call llxy (latitude,longitude,rj,ri,xlatc,xlonc,map_proj,
      call llxy (latitude,longitude,ri,rj,xlatc_cg,xlonc_cg,map_proj, &
                 true_lat1,true_lat2,dscg,xn,sn_maxcg,we_maxcg,       &
                 1,1,1)

!ajb  ri and rj are referenced to the non-staggered grid. (For MM5, they
!     were referenced to the dot grid.)

      rio(n)=ri
      rjo(n)=rj

      if (iprt) THEN
         if(n.le.10) then
                     write(6,'(/,a,i2,a,f5.2,a,f5.2,/)')        &
                                            ' OBS N = ',n,      &
                                            ' RIO = ',rio(n),   &
                                            ' RJO = ',rjo(n)
         endif
      endif

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
      if(platform(1:4).eq.'SAMS')plfo(n)=8.
      if(platform(7:14).eq.'PROFILER')plfo(n)=9.
! yliu: ACARS->SATWINDS
      if(platform(7:11).eq.'ACARS')plfo(n)=7.
! yliu: end
      if(plfo(n).eq.99.) then
         IF (iprt) print *,'n=',n,' unknown ob of type',platform
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

!ajb Store potential temperature for WRF
          if(temperature_qc.ge.0..and.temperature_qc.lt.30000.)then

            if(pressure_qc.ge.0..and.pressure_qc.lt.30000.)then

              varobs(3,n) =                                             &
                  temperature_data*(100000./pressure_data)**RCP - t0

!      write(6,*) 'reading data for N = ',n,' RCP = ',rcp
!      write(6,*) 'temperature_data = ',temperature_data
!      write(6,*) 'pressure_data = ',pressure_data
!      write(6,*) 'varobs(3,n) = ',varobs(3,n)

            else
              varobs(3,n)=-888888.
            endif

          else
            varobs(3,n)=-888888.
          endif

          if(pressure_qc.ge.0..and.pressure_qc.lt.30000.)then
!           if(pressure_qc.ge.0.)then
            varobs(5,n)=pressure_data
          else
            varobs(5,n)=-888888.
            IF (iprt) THEN
              print *,'********** PROBLEM *************'
              print *,'sounding, p undefined',latitude,longitude
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

! yliu: add special processing of NPN and Range profiler
!       only little_r interpolated and QC-ed data is used
          if(namef(2:9).eq."PROFILER") then               
            if((u_met_qc.ge.0..and.u_met_qc.lt.30000.).and.  &
              (v_met_qc.ge.0..and.v_met_qc.lt.30000.))then
!!yliu little_r already rotated the winds
!             call vect(longitude,u_met_data,v_met_data,xlonc,xlatc,xn)
              varobs(1,n)=u_met_data
              varobs(2,n)=v_met_data
            else
              varobs(1,n)=-888888.
              varobs(2,n)=-888888.
            endif
          else
            if((u_met_qc.ge.0..and.u_met_qc.lt.30000.).and.  &
              (v_met_qc.ge.0..and.v_met_qc.lt.30000.))then
!!yliu little_r already rotated the winds
!             call vect(longitude,u_met_data,v_met_data,xlonc,xlatc,xn)
              varobs(1,n)=u_met_data
              varobs(2,n)=v_met_data
            else
              varobs(1,n)=-888888.
              varobs(2,n)=-888888.
            endif
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

!ajb Store potential temperature for WRF
        if(temperature_qc.ge.0..and.temperature_qc.lt.30000.)then

          if((psfc_qc.ge.0..and.psfc_qc.lt.30000.).and.          &
             (psfc_data.gt.70000. .and.psfc_data.lt.105000.))then

            varobs(3,n) =                                        &
                  temperature_data*(100000./psfc_data)**RCP - t0
          else
            varobs(3,n)=-888888.
          endif
        else
          varobs(3,n)=-888888.
        endif

        if((psfc_qc.ge.0..and.psfc_qc.lt.30000.).and.(psfc_data.gt.70000.  &
        .and.psfc_data.lt.105000.))then
          varobs(5,n)=psfc_data
        else
          varobs(5,n)=-888888.
        endif
        if(varobs(5,n).ge.0.)varobs(5,n)=varobs(5,n)*1.e-3

        if((u_met_qc.ge.0..and.u_met_qc.lt.30000.).and.            &
           (v_met_qc.ge.0..and.v_met_qc.lt.30000.).and.            &
! make sure at least 1 of the components is .ne.0
           (u_met_data.ne.0..or.v_met_data.ne.0.))then
!!yliu little_r already rotated the winds
!!yliu   call vect(longitude,u_met_data,v_met_data,xlonc,xlatc,xn)
          varobs(1,n)=u_met_data
          varobs(2,n)=v_met_data
        else
          varobs(1,n)=-888888.
          varobs(2,n)=-888888.
        endif
! calculate psfc if slp is there
        if((psfc_qc.lt.0.).and.(slp_qc.ge.0..and.slp_qc.lt.30000.).and.   &
              (temperature_qc.ge.0..and.temperature_qc.lt.30000.).and.    &
              (slp_data.gt.90000.))then
          tbar=temperature_data+0.5*elevation*.0065
          psfc_data=slp_data*exp(-elevation/(rovg*tbar))
          varobs(5,n)=psfc_data*1.e-3
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
          varobs(5,n)=psfc_data*1.e-3
          psfc_qc=0.
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
           print *,' ======  '
           print *,' NO Data Found '
        ENDIF
      ENDIF   !end if(is_sound)
! END OF SFC OBS INPUT SECTION
!======================================================================
!======================================================================
! check if ob time is too early (only applies to beginning)
      IF(RTIMOB.LT.TBACK-fdob%WINDOW)then
        IF (iprt) print *,'ob too early'
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
              VAROBS(1,njc)=VAROBS(1,n)
              VAROBS(2,njc)=VAROBS(2,n)
              VAROBS(3,njc)=VAROBS(3,n)
              VAROBS(4,njc)=VAROBS(4,n)
              VAROBS(5,njc)=VAROBS(5,n)
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
                print *,'duplicate sounding - eliminate first occurrence', &
                                  n,inest,meas_count,nlevs_ob(njc),        &
                                  latitude,longitude,plfo(njc)
              ENDIF
              if(lev_in_ob(njc).ne.1.) then
                IF (iprt) THEN
                  print *, 'problem ******* - dup sndg ',                  &
                           lev_in_ob(njc),nlevs_ob(njc)
                ENDIF
              endif
!             n=n-meas_count
! set the first sounding ob to missing
              do njcc=njc,njc+nint(nlevs_ob(njc))-1
                VAROBS(1,njcc)=-888888.
                VAROBS(2,njcc)=-888888.
                VAROBS(3,njcc)=-888888.
                VAROBS(4,njcc)=-888888.
                VAROBS(5,njcc)=-888888.
                plfo(njcc)=99.
              enddo
              goto 100
!  if a pilot, but first one has more obs
            elseif( (is_sound).and.(plfo(njc).eq.plfo(n)) .and.            &
                    (plfo(njc).eq.6.).and.                                 &
                    (nlevs_ob(n).lt.nlevs_ob(njc)) )then
              IF (iprt) THEN
                print *,                                                   &
                 'duplicate pilot sounding - eliminate second occurrence', &
                                 n,inest,meas_count,nlevs_ob(njc),         &
                                 latitude,longitude,plfo(njc)
              ENDIF
              if(lev_in_ob(njc).ne.1.) then
                IF (iprt) THEN
                  print *, 'problem ******* - dup sndg ',                  &
                           lev_in_ob(njc),nlevs_ob(njc)
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
              IF (iprt) print *,                                        &
                'duplicate single lev sat-wind ob - replace first',n,      &
                                 inest,meas_count,varobs(5,n)
! this is the single ua ob replacement part
              VAROBS(1,njc)=VAROBS(1,n)
              VAROBS(2,njc)=VAROBS(2,n)
              VAROBS(3,njc)=VAROBS(3,n)
              VAROBS(4,njc)=VAROBS(4,n)
              VAROBS(5,njc)=VAROBS(5,n)
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
              IF (iprt) THEN
                print *,'duplicate location, but no match otherwise',n,njc,  &
                        plfo(n),varobs(5,n),nlevs_ob(n),lev_in_ob(n),        &
                        plfo(njc),varobs(5,njc),nlevs_ob(njc),lev_in_ob(njc)
              ENDIF
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

! check if ob is in coarse mesh domain  (061404 switched sn/we)
      if( (ri.lt.2.).or.(ri.gt.real(we_maxcg-1)).or.(rj.lt.2.).or.         &
          (rj.gt.real(sn_maxcg-1)) ) then

!         if (iprt) write(6,*) 'Obs out of coarse mesh domain'
!         write(6,*) 'we_maxcg-1 = ',real(we_maxcg-1)
!         write(6,*) 'sn_maxcg-1 = ',real(sn_maxcg-1)

!       n=n-1
!       if(is_sound)n=n-meas_count+1

        n=n-meas_count
!ajb  Reset timeob for discarded indices.
        do imc = n+1, n+meas_count
          timeob(imc) = 99999.
        enddo
        goto 100
      endif

! check if an upper air ob is too high
! the ptop here is hardwired
! this check has to come after other checks - usually the last few
!   upper air obs are too high
!      if(is_sound)then
!        njc=meas_count
!        do jcj=meas_count,1,-1
! 6. is 60 mb - hardwired
!          if((varobs(5,n).lt.6.).and.(varobs(5,n).gt.0.))then
!            print *,'obs too high - eliminate,n,p=',n,varobs(5,n)
!            n=n-1
!          else
!            if(varobs(5,n).gt.0.)goto 100
!          endif
!        enddo
!      endif
!
      IF(TIMEOB(N).LT.fdob%RTLAST) THEN
        IF (iprt) THEN
          PRINT *,'2 OBS ARE NOT IN CHRONOLOGICAL ORDER'
          PRINT *,'NEW YEAR?'
          print *,'timeob,rtlast,n=',timeob(n),fdob%rtlast,n
        ENDIF
        STOP 111
      ELSE
        fdob%RTLAST=TIMEOB(N)
      ENDIF
      GOTO 100
  111 CONTINUE
!**********************************************************************
!       --------------   END BIG 100 LOOP OVER N  --------------
!**********************************************************************
      IF (iprt) write(6,5403) NVOL,XTIME
      IEOF(inest)=1

      close(NVOLA+INEST-1)
      IF (iprt) print *,'closed fdda file for inest=',inest,nsta

!     if(nsta.eq.1.and.timeob(1).gt.9.e4)nsta=0
  goto 1001

! THE OBSERVATION ARRAYS ARE FULL AND THE MOST RECENTLY
! ACQUIRED OBS STILL HAS TIMEOB .LE. TFORWD.  SO START
! DECREASING THE SIZE OF THE WINDOW
! get here if too many obs
120 CONTINUE
  IF (iprt) THEN
    write(6,121) N,NIOBF
    write(6,122)
  ENDIF
  STOP 122
  fdob%WINDOW=fdob%WINDOW-0.1*TWINDO
  IF(TWINDO.LT.0)STOP 120
! IF THE WINDOW BECOMES NEGATIVE, THE INCOMING DATA IS
! PROBABLY GARBLED. STOP.
  GOTO 10
!
! READ CYCLE IS COMPLETED. DETERMINE THE NUMBER OF OBS IN
! THE CURRENT WINDOW
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! BUT FIRST, WHEN KTAU.EQ.0 (OR IN GENERAL, KTAUR), DISCARD THE
! "OLD" OBS FIRST...
130 CONTINUE

! get here if at end of file, or if obs time is beyond what we
! need right now
  IF(KTAU.EQ.KTAUR)THEN
    NSTA=0
    keep_obs : DO N=1,NIOBF

! try to keep all obs, but just don't use yet
!  (don't want to throw away last obs read in - especially if
!  its a sounding, in which case it looks like many obs)
      IF(TIMEOB(N).GT.9.e4) EXIT keep_obs
      if(timeob(n).gt.tforwd) then
        if(iprt) write(6,951)inest,n,timeob(n),tforwd
 951    FORMAT('saving ob beyond window,inest,n,timeob,tforwd=',   &
               2i5,2f13.4) 
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
    IF (iprt) THEN
      print *,'after 190 ndum=',ndum,nsta
      print *,'timeob=',timeob(1),timeob(2)
    ENDIF
    NDUM=ABS(NDUM)
    NMOVE=NIOBF-NDUM
    IF( NMOVE.GT.0 .AND. NDUM.NE.0) THEN
      DO N=1,NMOVE
        VAROBS(1,N)=VAROBS(1,N+NDUM)
        VAROBS(2,N)=VAROBS(2,N+NDUM)
        VAROBS(3,N)=VAROBS(3,N+NDUM)
        VAROBS(4,N)=VAROBS(4,N+NDUM)
        VAROBS(5,N)=VAROBS(5,N+NDUM)
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
        VAROBS(1,N)=99999.
        VAROBS(2,N)=99999.
        VAROBS(3,N)=99999.
        VAROBS(4,N)=99999.
        VAROBS(5,N)=99999.
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
 
  IF (iprt) write(6,160) KTAU,XTIME,NSTA
  IF(KTAU.EQ.KTAUR)THEN
    IF(nudge_opt.EQ.1)THEN
      TWDOP=TWINDO*60.
      IF (iprt) THEN
        write(6,1449) INEST,RINXY,RINSIG,TWDOP
        IF(ISWIND.EQ.1) write(6,1450) GIV
        IF(ISTEMP.EQ.1) write(6,1451) GIT
        IF(ISMOIS.EQ.1) write(6,1452) GIQ
        IF(ISPSTR.EQ.1) write(6,1453) GIP
      ENDIF
    ENDIF
  ENDIF
  IF(KTAU.EQ.KTAUR)THEN
    IF (iprt) THEN
      write(6,553)
      write(6,554)
    ENDIF
    IF(fdob%IWTSIG.NE.1)THEN
      IF (iprt) THEN
        write(6,555)
        write(6,556) fdob%RINFMN*RINXY,fdob%RINFMX*RINXY,fdob%PFREE*10.
      ENDIF
      IF(fdob%RINFMN.GT.fdob%RINFMX)STOP 556
! IS MINIMUM GREATER THAN MAXIMUM?
      IF (iprt) write(6,557) fdob%DPSMX*10.,fdob%DCON
      IF(fdob%DPSMX.GT.10.)STOP 557
    ENDIF
  ENDIF
! IS DPSMX IN CB?
 
  IF(KTAU.EQ.KTAUR)THEN
    IF (iprt) write(6,601) INEST,IONF
  ENDIF
  fdob%NSTAT=NSTA

555   FORMAT(1X,'   ABOVE THE SURFACE LAYER, OBS NUDGING IS PERFORMED',  &
      ' ON PRESSURE LEVELS,')
556   FORMAT(1X,'   WHERE RINXY VARIES LINEARLY FROM ',E11.3,' KM AT',   &
      ' THE SURFACE TO ',E11.3,' KM AT ',F7.2,' MB AND ABOVE')
557   FORMAT(1X,'   IN THE SURFACE LAYER, WXY IS A FUNCTION OF ',        &
      'DPSMX = ',F7.2,' MB WITH DCON = ',E11.3,                          &
      ' - SEE SUBROUTINE NUDOB')
601   FORMAT('0','FOR EFFICIENCY, THE OBS NUDGING FREQUENCY ',           &
        'FOR MESH #',I2,' IS ',1I2,' CGM TIMESTEPS ',/)
121   FORMAT('0','  WARNING: NOBS  = ',I4,' IS GREATER THAN NIOBF = ',   &
      I4,': INCREASE PARAMETER NIOBF')
5403  FORMAT(1H0,'-------------EOF REACHED FOR NVOL = ',I3,              &
       ' AND XTIME = ',F10.2,'-------------------')
122   FORMAT(1X,'     ...OR THE CODE WILL REDUCE THE TIME WINDOW')
160   FORMAT('0','****** CALL IN4DOB AT KTAU = ',I5,' AND XTIME = ',     &
      F10.2,':  NSTA = ',I7,' ******')
1449  FORMAT(1H0,'*****NUDGING INDIVIDUAL OBS ON MESH #',I2,             &
       ' WITH RINXY = ',                                                 &
      E11.3,' KM, RINSIG = ',E11.3,' AND TWINDO (HALF-PERIOD) = ',       &
      E11.3,' MIN')
1450  FORMAT(1X,'NUDGING IND. OBS WINDS WITH GIV = ',E11.3)
1451  FORMAT(1X,'NUDGING IND. OBS TEMPERATURE WITH GIT = ',E11.3)
1452  FORMAT(1X,'NUDGING IND. OBS MOISTURE WITH GIQ = ',E11.3)
1453  FORMAT(1X,'NUDGING IND. OBS SURFACE PRESSURE WITH GIP = ,'E11.3)
1469  FORMAT(1H0,'*****NUDGING INDIVIDUAL OBS ON FGM WITH RINXY = ',     &
      E11.3,' KM, RINSIG = ',E11.3,' AND TWINDO (HALF-PERIOD) = ',       &
      E11.3,' MIN')
553   FORMAT(1X,'BY DEFAULT: OBS NUDGING OF TEMPERATURE AND MOISTURE ',  &
      'IS RESTRICTED TO ABOVE THE BOUNDARY LAYER')
554   FORMAT(1X,'...WHILE OBS NUDGING OF WIND IS INDEPENDENT OF THE ',   &
      'BOUNDARY LAYER')

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

  SUBROUTINE vect(xlon,e1,e2,xlonc,xlatc,xn)
 
! THIS ROUTINE CONVERTS INCOMING U AND V COMPS INTO MAP U AND V COMPS.
! iproj is projection (1=lamconf, 2=polarst, 3=mercator)
! xlonc is center longitude
! xn is cone factor (.716 for current lc)
!
      REAL, intent(in) :: XLON
      REAL, intent(inout) :: E1
      REAL, intent(inout) :: E2
      REAL, intent(in) :: xlonc
      REAL, intent(in) :: xlatc
      REAL, intent(in) :: xn

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real :: pi, degran, u, v, xlonr, angle

      pi=3.1415926535
      DEGRAN=PI/180.
!
!
         u=e1
         v=e2
         XLONR=XLONC-XLON
         IF(XLONR.GT.180.) XLONR=XLONR-360.
         IF(XLONR.LT.-180.) XLONR=XLONR+360.
         ANGLE=XLONR*XN*DEGRAN
         IF (xlatC.LT.0.0) ANGLE=-ANGLE
         E1=V*SIN(ANGLE)+U*COS(ANGLE)
         E2=V*COS(ANGLE)-U*SIN(ANGLE)
      RETURN
  END SUBROUTINE vect

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

      eps=0.622
      e0=6.112
      eslcon1=17.67
      eslcon2=29.65
      esicon1=22.514
      esicon2=6.15e3
      t0=273.15
 
      print *,'rh2r input=',rh,t,p
      rh1=rh*.01
 
      if(iice.eq.1.and.t.le.t0)then
        esat=e0*exp(esicon1-esicon2/t)
      else
        esat=e0*exp(eslcon1*(t-t0)/(t-eslcon2))
      endif
      rsat=eps*esat/(p-esat)
!     print *,'rsat,esat=',rsat,esat
      r=rh1*eps*rsat/(eps+rsat*(1.-rh1))
 
      print *,'rh2r rh,t,p,r=',rh1,t,p,r
 
      return
END SUBROUTINE rh2rb

SUBROUTINE llxy_lam (xlat,xlon,X,Y,xlatc, xlonc,xn,ds,       &
           imax, jmax, true_lat1, true_lat2 )

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real :: pi, conv, a

      PARAMETER(pi=3.14159,CONV=180./pi,a =6370.)

      REAL  TRUE_LAT1 , PHI1 , POLE , XLATC , PHIC , XC , YC ,       &
            XN , FLP , XLON , XLONC , PSX , XLAT , R , XX , YY ,     &
            CENTRI , CENTRJ , X , DS , Y , TRUE_LAT2

      INTEGER  IMAX , JMAX

!  Calculate x and y given latitude and longitude for Lambert conformal projection

      IF(ABS(true_lat1).GT.90) THEN
        PHI1 = 90. - 30.
      ELSE
        PHI1 = 90. - true_lat1
      END IF
      POLE = 90.0
      IF ( XLATC.LT.0.0 ) THEN
        PHI1 = -PHI1
        POLE = -POLE
      END IF
      PHIC = ( POLE - XLATC )/CONV
      PHI1 = PHI1/CONV
      XC = 0.0
      YC = -A/XN*SIN(PHI1)*(TAN(PHIC/2.0)/TAN(PHI1/2.0))**XN

!  CALCULATE X,Y COORDS. RELATIVE TO POLE

      FLP = XN*( XLON - XLONC )/CONV
      PSX = ( POLE - XLAT )/CONV
      R = -A/XN*SIN(PHI1)*(TAN(PSX/2.0)/TAN(PHI1/2.0))**XN
      IF ( XLATC.LT.0.0 ) THEN
        XX = R*SIN(FLP)
        YY = R*COS(FLP)
      ELSE
        XX = -R*SIN(FLP)
        YY = R*COS(FLP)
      END IF

!  TRANSFORM (1,1) TO THE ORIGIN

      CENTRI = FLOAT(IMAX + 1)/2.0
      CENTRJ = FLOAT(JMAX + 1)/2.0
      X = ( XX - XC )/DS + CENTRJ
      Y = ( YY - YC )/DS + CENTRI

      return
  END SUBROUTINE llxy_lam

  SUBROUTINE llxy(xlat,xlon,x,y,xlatc,xlonc,kproj,psi1,psi2,ds,    &
                      xn,sn_max,we_max,parent_grid_ratio,        &
                      i_parent_start,j_parent_start)
 
      IMPLICIT NONE
 
!     CALCULATE X AND Y GIVEN LATITUDE AND LONGITUDE.
!     PETER HOWELLS, NCAR, 1984
 
      REAL, intent(in)    :: xlat
      REAL, intent(inout) :: xlon
      REAL, intent(out)   :: x
      REAL, intent(out)   :: y
      REAL, intent(in)    :: xlatc
      REAL, intent(in)    :: xlonc
      INTEGER, intent(in) :: kproj 
      REAL, intent(in)    :: psi1
      REAL, intent(in)    :: psi2 
      REAL, intent(in)    :: ds
      REAL, intent(in)    :: xn
      INTEGER, intent(in) :: sn_max
      INTEGER, intent(in) :: we_max
      INTEGER, intent(in) :: parent_grid_ratio
      INTEGER, intent(in) :: i_parent_start
      INTEGER, intent(in) :: j_parent_start

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real conv, a, phi1, pole, c2, xc, phicr, cell, yc, xlatr
      real phic, xx, yy, centri, centrj, ylon, flp, psx, r
      integer imax, jmax, imapst, jmapst

!       write(6,*) 'enter llxy'
!       write(6,*) 'enter llxy: xlatc = ',xlatc,' xlonc = ',xlonc
!       write(6,*) 'xlat = ',xlat,' xlon = ',xlon
!       write(6,*) 'psi1 = ',psi1,' psi2 = ',psi2
!       write(6,*) 'xn = ',xn,' kproj = ',kproj,' ds = ',ds
!       write(6,*) 'sn_max = ',sn_max,' we_max = ',we_max
!       write(6,*) 'parent_grid_ratio = ',parent_grid_ratio
!       write(6,*) 'i_parent_start = ',i_parent_start
!       write(6,*) 'j_parent_start = ',j_parent_start

      conv = 57.29578
      a = 6370.0
!      imax  = sn_max*parent_grid_ratio+1
!      jmax  = we_max*parent_grid_ratio+1
       imax  = sn_max*parent_grid_ratio       !ajb for WRF
       jmax  = we_max*parent_grid_ratio       !ajb for WRF
      imapst= (j_parent_start-1)*parent_grid_ratio+1
      jmapst= (i_parent_start-1)*parent_grid_ratio+1
      phi1 = 90.0-psi2
      pole = 90.0

      if ( xlatc.lt.0.0 ) then
        phi1 = -90.0-psi2
        pole = -pole
      endif
 
      if (kproj.eq.3) then
! MERCATOR PROJECTION
        C2     = A*COS(PSI1)
        XC     = 0.0
        PHICR  = XLATC/CONV
        CELL   = COS(PHICR)/(1.0+SIN(PHICR))
        YC     = - C2*ALOG(CELL)
        IF (XLAT.NE.-90.) THEN
           XLATR = XLAT/CONV
           CELL = COS(XLATR)/(1.0+SIN(XLATR))
           YY = -C2*ALOG(CELL)
           IF (XLONC.LT.0.0) THEN
             IF (XLON.GT.0.0) XLON=XLON-360.
           ELSE
             IF (XLON.LT.0.0) XLON=360.+XLON
           ENDIF
           XX = C2*(XLON-XLONC)/CONV
        ENDIF
 
      ELSE IF (KPROJ.EQ.1) THEN
! LAMBERT-COMFORMAL or POLAR-STEREO PROJECTION
      PHIC = ( POLE - XLATC )/CONV
      PHI1 = PHI1/CONV
      XC = 0.0
      YC = -A/XN*SIN(PHI1)*(TAN(PHIC/2.0)/TAN(PHI1/2.0))**XN
 
!     CALCULATE X,Y COORDS. RELATIVE TO POLE
 
      YLON = XLON - XLONC
      IF(YLON.GT.180) YLON = YLON - 360.
      IF(YLON.LT.-180) YLON = YLON + 360.
      FLP = XN*YLON/CONV
      PSX = ( POLE - XLAT )/CONV
      R = -A/XN*SIN(PHI1)*(TAN(PSX/2.0)/TAN(PHI1/2.0))**XN
      IF ( XLATC.LT.0.0 ) THEN
         XX = R*SIN(FLP)
         YY = R*COS(FLP)
      ELSE
         XX = -R*SIN(FLP)
         YY = R*COS(FLP)
      END IF
      END IF
 
!  TRANSFORM (1,1) TO THE ORIGIN
 
      CENTRI = FLOAT(IMAX + 1)/2.0
      CENTRJ = FLOAT(JMAX + 1)/2.0
      X = ( XX - XC )/DS + CENTRJ  - jmapst + 1      
      Y = ( YY - YC )/DS + CENTRI  - imapst + 1

      RETURN
  END SUBROUTINE llxy

  SUBROUTINE llxy_try(xlat,xlon,x,y,xlatc,xlonc,kproj,psi1a,psi2,ds,    &
                    xn,sn_max,we_max,parent_grid_ratio,                   &
                    i_parent_start,j_parent_start)
 
      IMPLICIT NONE
 
!     CALCULATE X AND Y GIVEN LATITUDE AND LONGITUDE.
!     PETER HOWELLS, NCAR, 1984
 
      REAL, intent(in)    :: xlat
      REAL, intent(inout) :: xlon
      REAL, intent(out)   :: x
      REAL, intent(out)   :: y
      REAL, intent(in)    :: xlatc
      REAL, intent(in)    :: xlonc
      INTEGER, intent(in) :: kproj 
      REAL, intent(in)    :: psi1a
      REAL, intent(in)    :: psi2 
      REAL, intent(in)    :: ds
      REAL, intent(in)    :: xn
      INTEGER, intent(in) :: sn_max
      INTEGER, intent(in) :: we_max
      INTEGER, intent(in) :: parent_grid_ratio
      INTEGER, intent(in) :: i_parent_start
      INTEGER, intent(in) :: j_parent_start

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real conv, a, phi1, pole, c2, xc, phicr, cell, yc, xlatr
      real phic, xx, yy, centri, centrj, ylon, flp, psx, r
      integer imax, jmax, imapst, jmapst
      integer jmxc,imxc,iratio
      real ric0,rjc0,rs,yind,xind,rix,rjx,psi1

!       write(6,*) 'enter llxy'
!       write(6,*) 'enter llxy: xlatc = ',xlatc,' xlonc = ',xlonc
!       write(6,*) 'psi1 = ',psi1a,' psi2 = ',psi2
!       write(6,*) 'xn = ',xn,' kproj = ',kproj,' ds = ',ds
!       write(6,*) 'sn_max = ',sn_max,' we_max = ',we_max
!       write(6,*) 'parent_grid_ratio = ',parent_grid_ratio
!       write(6,*) 'i_parent_start = ',i_parent_start
!       write(6,*) 'j_parent_start = ',j_parent_start

      conv = 57.29578
      a = 6370.0
      imax  = sn_max*parent_grid_ratio+1
      jmax  = we_max*parent_grid_ratio+1
      imapst= (j_parent_start-1)*parent_grid_ratio+1
      jmapst= (i_parent_start-1)*parent_grid_ratio+1
      iratio = parent_grid_ratio
      imxc = imax
      jmxc = jmax
      rix=imapst
      rjx=jmapst

      phi1 = psi1a
      if(xlatc .gt. 0.) then
       psi1=90.-phi1
      else
       psi1=-90+abs(phi1)
      endif

      ric0=(imxc+1.)*0.5
      rjc0=(jmxc+1.)*0.5

!      if(kproj .eq. 1) then
       if(kproj .le. 2) then
       if(xlatc .gt. 0.) then
        rs=a/xn*sin(psi1/conv)*(tan((90.-xlat)/conv/2.)           &
            /tan(psi1/2./conv))**xn
        yc=-a/xn*sin(psi1/conv)*(tan((90.-xlatc)/conv/2.)         &
            /tan(psi1/2./conv))**xn
       else
        rs=a/xn*sin(psi1/conv)*(tan((-90.-xlat)/conv/2.)          &
            /tan(psi1/2./conv))**xn
        yc=-a/xn*sin(psi1/conv)*(tan((-90.-xlatc)/conv/2.)        &
            /tan(psi1/2./conv))**xn
       endif
!     elseif(kproj .eq. 2) then
!       if(xlatc .gt. 0.) then
!        rs=a*sin((90.-xlat)/conv)*(1.+cos(psi1/conv))
!     &     /(1.+cos((90.-xlat)/conv))
!        yc=-a*sin((90.-xlatc)/conv)*(1.+cos(psi1/conv))
!     &     /(1.+cos((90.-xlatc)/conv))
!       else
!        rs=a*sin((-90.-xlat)/conv)*(1.+cos(psi1/conv))
!     &     /(1.+cos((-90.-xlat)/conv))
!        yc=-a*sin((-90.-xlatc)/conv)*(1.+cos(psi1/conv))
!     &     /(1.+cos((-90.-xlatc)/conv))
!      endif
      else
       psi1 = 0           ! yliu added
       c2=a*cos(psi1/conv)
       yc=c2*alog((1.+sin(xlatc/conv))/cos(xlatc/conv))
      endif

      if(kproj .le. 2) then
        y=(ric0-(yc/ds+rs/ds*cos(xn*(xlon-xlonc)/conv))-rix)      &
             *iratio+1.0
        if(xlatc .gt. 0.) then
         x=(rjc0+rs/ds*sin(xn*(xlon-xlonc)/conv)-rjx)*iratio+1.0
        else
         x=(rjc0-rs/ds*sin(xn*(xlon-xlonc)/conv)-rjx)*iratio+1.0
        endif
      else
       y=c2*alog((1.+sin(xlat/conv))/cos(xlat/conv))
       x=c2*(xlon-xlonc)/conv
       y=(ric0+(y-yc)/ds-rix)*iratio+1.0
       x=(rjc0+x/ds-rjx)*iratio+1.0
      endif
        write(6,*) 'xj = ',x, 'yi = ',y,"xlat",xlat,"xlon",xlon

      RETURN
  END SUBROUTINE llxy_try

#endif
!-----------------------------------------------------------------------
! End subroutines for in4dob
!-----------------------------------------------------------------------
