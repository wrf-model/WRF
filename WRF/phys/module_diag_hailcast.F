#if (NMM_CORE == 1)
MODULE module_diag_hailcast
CONTAINS
   SUBROUTINE diag_hailcast_stub
   END SUBROUTINE diag_hailcast_stub
END MODULE module_diag_hailcast
#else

MODULE module_diag_hailcast

CONTAINS

  SUBROUTINE hailcast_diagnostic_driver (   grid , config_flags     &
                             , moist                             &
                             , rho  &
                             , ids, ide, jds, jde, kds, kde      &
                             , ims, ime, jms, jme, kms, kme      &
                             , ips, ipe, jps, jpe, kps, kpe      &
                             , its, ite, jts, jte                &
                             , k_start, k_end                    &
                             , dt, itimestep                     &
                             , haildt,curr_secs                  &
                             , haildtacttime                     )

    USE module_domain, ONLY : domain , domain_clock_get
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec
    USE module_state_description
    USE module_model_constants
    USE module_utility
    USE module_streams, ONLY: history_alarm, auxhist2_alarm
#ifdef DM_PARALLEL
    USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
#endif

    IMPLICIT NONE

    TYPE ( domain ), INTENT(INOUT) :: grid
    TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe
    INTEGER             :: k_start , k_end, its, ite, jts, jte

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_moist),    &
         INTENT(IN   ) ::                                moist

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                 rho

    REAL,       INTENT(IN   ),OPTIONAL    ::     haildt
    REAL,       INTENT(IN   ),OPTIONAL    ::     curr_secs
    REAL,       INTENT(INOUT),OPTIONAL    ::     haildtacttime
    INTEGER,    INTENT(IN   )             ::     itimestep
    REAL,       INTENT(IN   )             ::     dt


    ! Local
    ! -----
    CHARACTER*512 :: message
    CHARACTER*256 :: timestr 
    INTEGER :: i,j,k,nz
    INTEGER :: i_start, i_end, j_start, j_end
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::      qr  &
                                              ,          qs  &
                                              ,          qg  &
                                              ,          qv  &
                                              ,          qc  &
                                              ,          qi  &
                                              ,        ptot 
    REAL, DIMENSION( ims:ime, jms:jme ) ::       wup_mask_prev  &
                                              ,      wdur_prev  
    REAL :: dhail1,dhail2,dhail3,dhail4,dhail5
    
    ! Timing
    ! ------
    TYPE(WRFU_Time) :: hist_time, aux2_time, CurrTime, StartTime
    TYPE(WRFU_TimeInterval) :: dtint, histint, aux2int
    LOGICAL :: is_after_history_dump, is_output_timestep, is_first_timestep
    LOGICAL :: doing_adapt_dt, run_param, decided
    INTEGER :: stephail, itimestep_basezero


    ! Chirp the routine name for debugging purposes
    ! ---------------------------------------------
    write ( message, * ) 'inside hailcast_diagnostics_driver'
    CALL wrf_debug( 100 , message )

    ! Get timing info 
    ! Want to know if when the last history output was
    ! Check history and auxhist2 alarms to check last ring time and how often
    ! they are set to ring
    ! -----------------------------------------------------------------------
    CALL WRFU_ALARMGET( grid%alarms( HISTORY_ALARM ), prevringtime=hist_time, &
         ringinterval=histint)
    CALL WRFU_ALARMGET( grid%alarms( AUXHIST2_ALARM ), prevringtime=aux2_time, &
         ringinterval=aux2int)

    ! Get domain clock
    ! ----------------
    CALL domain_clock_get ( grid, current_time=CurrTime, &
         simulationStartTime=StartTime, &            
         current_timestr=timestr, time_step=dtint )

    ! Set some booleans for use later
    ! Following uses an overloaded .lt.
    ! ---------------------------------
    is_after_history_dump = ( Currtime .lt. hist_time + dtint )

    ! Following uses an overloaded .ge.
    ! ---------------------------------
    is_output_timestep = (Currtime .ge. hist_time + histint - dtint .or. &
                         Currtime .ge. aux2_time + aux2int - dtint )
    write ( message, * ) 'is output timestep? ', is_output_timestep
    CALL wrf_debug( 100 , message )

    ! Following uses an overloaded .eq.
    ! ---------------------------------
    is_first_timestep = ( Currtime .eq. StartTime + dtint )
        

    ! After each history dump, reset max/min value arrays
    ! ----------------------------------------------------------------------
    IF ( is_after_history_dump ) THEN
      DO j = jts, jte
        DO i = its, ite
           grid%hailcast_dhail1(i,j) = 0.
           grid%hailcast_dhail2(i,j) = 0.
           grid%hailcast_dhail3(i,j) = 0.
           grid%hailcast_dhail4(i,j) = 0.
           grid%hailcast_dhail5(i,j) = 0.
        ENDDO
      ENDDO
    ENDIF  ! is_after_history_dump


    ! We need to do some neighboring gridpoint comparisons for the updraft
    ! duration calculations; set i,j start and end values so we don't go off 
    ! the edges of the domain.  Updraft duration on domain edges will always be 0.
    ! ----------------------------------------------------------------------
    i_start = its
    i_end   = ite
    j_start = jts
    j_end   = jte

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-2, ite )  !-1
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-2, jte )  !-1
    IF ( config_flags%periodic_x ) i_start = its
    IF ( config_flags%periodic_x ) i_end = ite
    

    ! Make a copy of the updraft duration, mask variables
    ! ---------------------------------------------------
    !wdur_prev(:,:) = grid%hailcast_wdur(:,:)
    !wup_mask_prev(:,:) = grid%hailcast_wup_mask(:,:)
    DO j = MAX( jts-1 , jds ), MIN( jte+1 , jde-1 )
        DO i = MAX( its-1 , ids ), MIN( ite+1 , ide-1)
            wdur_prev(i,j) = grid%hailcast_wdur(i,j)
            wup_mask_prev(i,j) = grid%hailcast_wup_mask(i,j)
        ENDDO
    ENDDO


    ! Determine updraft mask (where updraft greater than some threshold)
    ! ---------------------------------------------------
    DO j = jts, jte
      DO i = its, ite
        grid%hailcast_wup_mask(i,j) = 0
        grid%hailcast_wdur(i,j) = 0

        DO k = k_start, k_end
          IF ( grid%w_2(i,k,j) .ge. 10. ) THEN
              grid%hailcast_wup_mask(i,j) = 1
          ENDIF
        ENDDO
      ENDDO
    ENDDO

    ! Determine updraft duration; make sure not to call point outside the domain
    ! ---------------------------------------------------
    DO j = j_start, j_end
      DO i = i_start, i_end

        ! Determine updraft duration using updraft masks
        ! ---------------------------------------------------
        IF ( (grid%hailcast_wup_mask(i,j).eq.1) .OR.                 &
           (MAXVAL(wup_mask_prev(i-1:i+1,j-1:j+1)).eq.1) ) THEN
             grid%hailcast_wdur(i,j) =                                 &
                  MAXVAL(wdur_prev(i-1:i+1,j-1:j+1)) + grid%dt
        ENDIF
      ENDDO
    ENDDO


    ! Only run the scheme every "haildt" seconds as set in the namelist.
    ! Code borrowed from module_pbl_driver, although here haildt is
    ! in seconds, not minutes (bldt is in minutes).
    ! ---------------------------------------------------

    ! Are we using adaptive timestepping?
    doing_adapt_dt = .FALSE.
    IF ( (config_flags%use_adaptive_time_step) .and. &
        ( (.not. grid%nested) .or. &
        ( (grid%nested) .and. (abs(grid%dtbc) < 0.0001) ) ) )THEN
       doing_adapt_dt = .TRUE.
       IF ( haildtacttime .eq. 0. ) THEN
          haildtacttime = CURR_SECS + haildt
       END IF
    END IF

    !  Calculate STEPHAIL 
    stephail = NINT(haildt / dt)      
    stephail = MAX(stephail,1)

    ! itimestep starts at 1 - we want it to start at 0 so correctly
    ! divisibile by stephail.
    itimestep_basezero = itimestep -1 


    !  Do we run through this scheme or not?
    !    Test 1:  If this is the initial model time, then yes.
    !                ITIMESTEP=1
    !    Test 2:  If the user asked for hailcast to be run every time step, then yes.
    !                HAILDT=0  or STEPHAIL=1
    !    Test 3:  If not adaptive dt, and this is on the requested hail frequency,
    !    then yes.
    !                MOD(ITIMESTEP,STEPHAIL)=0
    !    Test 4:  If using adaptive dt and the current time is past the last
    !    requested activate hailcast time, then yes.
    !                CURR_SECS >= HAILDTACTTIME

    !  If we do run through the scheme, we set the flag run_param to TRUE and we set
    !  the decided flag to TRUE.  The decided flag says that one of these tests was 
    !  able to say "yes", run the scheme. We only proceed to other tests if the 
    !  previous tests all have left decided as FALSE. If we set run_param to TRUE 
    !  and this is adaptive time stepping, we set the time to the next hailcast run.

    run_param = .FALSE.
    decided = .FALSE.
    IF ( ( .NOT. decided ) .AND. &
         ( itimestep_basezero .EQ. 0 ) ) THEN
       run_param   = .TRUE.
       decided     = .TRUE.
    END IF

    IF ( PRESENT(haildt) )THEN
       IF ( ( .NOT. decided ) .AND. &
            ( ( haildt .EQ. 0. ) .OR. ( stephail .EQ. 1 ) ) ) THEN
          run_param   = .TRUE.
          decided     = .TRUE.
       END IF
    ELSE
       IF ( ( .NOT. decided ) .AND. &
                                    ( stephail .EQ. 1 )   ) THEN
          run_param   = .TRUE.
          decided     = .TRUE.
       END IF
    END IF

    IF ( ( .NOT. decided ) .AND. &
         ( .NOT. doing_adapt_dt ) .AND. &
         ( MOD(itimestep_basezero,stephail) .EQ. 0 ) ) THEN
       run_param   = .TRUE.
       decided     = .TRUE.
    END IF

    IF ( ( .NOT. decided ) .AND. &
         ( doing_adapt_dt ) .AND. &
         ( curr_secs .GE. haildtacttime ) ) THEN
       run_param   = .TRUE.
       decided     = .TRUE.
       haildtacttime = curr_secs + haildt
    END IF

    write ( message, * ) 'timestep to run HAILCAST? ', run_param
    CALL wrf_debug( 100 , message )

    IF (run_param) THEN
    
        ! 3-D arrays for moisture variables
        ! ---------------------------------
        DO i=its, ite !ims, ime
          DO k=kms, kme
            DO j=jts, jte !jms, jme
              qv(i,k,j) = moist(i,k,j,P_QV)
              qr(i,k,j) = moist(i,k,j,P_QR)
              qs(i,k,j) = moist(i,k,j,P_QS)
              qg(i,k,j) = moist(i,k,j,P_QG)
              qc(i,k,j) = moist(i,k,j,P_QC)
              qi(i,k,j) = moist(i,k,j,P_QI)
            ENDDO
          ENDDO
        ENDDO

        ! Total pressure
        ! -------------- 
        DO i=its, ite! ims, ime
          DO k=kms, kme
            DO j=jts, jte !jms, jme
              ptot(i,k,j)=grid%pb(i,k,j)+grid%p(i,k,j)
            ENDDO
          ENDDO
        ENDDO

        ! Hail diameter in millimeters (HAILCAST)
        ! ---------------------------------------------------
        nz = k_end - k_start
        DO j = jts, jte
          DO i = its, ite

            ! Only call hailstone driver if updraft has been
            ! around longer than 15 min
            ! ----------------------------------------------
            IF (grid%hailcast_wdur(i,j) .gt. 900) THEN
              CALL hailstone_driver ( grid%t_phy(i,kms:kme,j), &
                                      grid%z(i,kms:kme,j),     &
                                      grid%ht(i,       j),     &
                                      ptot(i,kms:kme,j),     &
                                      rho(i,kms:kme,j),   &
                                      qv(i,kms:kme,j),    &
                                      qi(i,kms:kme,j),    &
                                      qc(i,kms:kme,j),    &
                                      qr(i,kms:kme,j),    &
                                      qs(i,kms:kme,j),    &
                                      qg(i,kms:kme,j),    &
                                      grid%w_2(i,kms:kme,j),   &
                                      grid%hailcast_wdur(i,j),          &
                                      nz,                 &
                                      dhail1, dhail2,     &
                                      dhail3, dhail4,     &
                                      dhail5              )
              IF (dhail1 .gt. grid%hailcast_dhail1(i,j)) THEN
                  grid%hailcast_dhail1(i,j) = dhail1
              ENDIF
              IF (dhail2 .gt. grid%hailcast_dhail2(i,j)) THEN
                  grid%hailcast_dhail2(i,j) = dhail2
              ENDIF
              IF (dhail3 .gt. grid%hailcast_dhail3(i,j)) THEN
                  grid%hailcast_dhail3(i,j) = dhail3
              ENDIF
              IF (dhail4 .gt. grid%hailcast_dhail4(i,j)) THEN
                  grid%hailcast_dhail4(i,j) = dhail4
              ENDIF
              IF (dhail5 .gt. grid%hailcast_dhail5(i,j)) THEN
                  grid%hailcast_dhail5(i,j) = dhail5
              ENDIF
            ENDIF
          ENDDO
        ENDDO

        ! Calculate the mean and standard deviation of the hail diameter
        ! distribution over different embryo sizes
        ! ----------------------------------------
        DO j = jts, jte !jms, jme
          DO i = its, ite !ims, ime
            !mean
            grid%hailcast_diam_mean(i,j)=(grid%hailcast_dhail1(i,j)+&
                 grid%hailcast_dhail2(i,j) +grid%hailcast_dhail3(i,j)+&
                 grid%hailcast_dhail4(i,j) +grid%hailcast_dhail5(i,j))/5.
            !max
            grid%hailcast_diam_max(i,j)=MAX(grid%hailcast_dhail1(i,j),&
                 grid%hailcast_dhail2(i,j), grid%hailcast_dhail3(i,j),&
                 grid%hailcast_dhail4(i,j), grid%hailcast_dhail5(i,j))
            !sample standard deviation
            grid%hailcast_diam_std(i,j) = SQRT( ( &
              (grid%hailcast_dhail1(i,j)-grid%hailcast_diam_mean(i,j))**2.+&
              (grid%hailcast_dhail2(i,j)-grid%hailcast_diam_mean(i,j))**2.+&
              (grid%hailcast_dhail3(i,j)-grid%hailcast_diam_mean(i,j))**2.+&
              (grid%hailcast_dhail4(i,j)-grid%hailcast_diam_mean(i,j))**2.+&
              (grid%hailcast_dhail5(i,j)-grid%hailcast_diam_mean(i,j))**2.)&
              / 4.0)
          ENDDO
        ENDDO

      END IF

END SUBROUTINE hailcast_diagnostic_driver

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!
!!!! Hailstone driver, adapted from hailstone subroutine in HAILCAST
!!!!  Inputs:
!!!!    1-d (nz)
!!!!     TCA          temperature (K) 
!!!!     h1d          height above sea level (m) 
!!!!     PA           total pressure (Pa)
!!!!     rho1d        density (kg/m3)
!!!!     RA           vapor mixing ratio (kg/kg)
!!!!     qi1d         cloud ice mixing ratio (kg/kg)
!!!!     qc1d         cloud water mixing ratio (kg/kg)
!!!!     qr1d         rain water mixing ratio (kg/kg)
!!!!     qg1d         graupel mixing ratio (kg/kg)
!!!!     qs1d         snow mixing ratio (kg/kg)
!!!!     VUU          updraft speed at each level (m/s)
!!!!    Float
!!!!     ht         terrain height (m)
!!!!     wdur       duration of any updraft > 10 m/s within 1 surrounding 
!!!!                 gridpoint 
!!!!    Integer
!!!!     nz         number of vertical levels
!!!!
!!!!  Output:
!!!!     dhail      hail diameter in mm 
!!!!                1st-5th rank-ordered hail diameters returned
!!!!
!!!!  13 Aug 2013 .................................Becky Adams-Selin AER
!!!!     adapted from hailstone subroutine in SPC's HAILCAST
!!!!  18 Mar 2014 .................................Becky Adams-Selin AER
!!!!     added variable rime layer density, per Ziegler et al. (1983)
!!!!  4 Jun 2014 ..................................Becky Adams-Selin AER
!!!!     removed initial embryo size dependency on microphysics scheme
!!!!  5 Jun 2014 ..................................Becky Adams-Selin AER
!!!!     used smaller initial embryo sizes
!!!!  25 Jun 2015..................................Becky Adams-Selin AER
!!!!     Significant revamping.  Fixed units bug in HEATBUD that caused
!!!!     hailstone temperature instabilities.  Similar issue fixed in BREAKUP
!!!!     subroutine.  Removed graupel from ice content.  Changed initial
!!!!     embryo size and location to better match literature.  Added
!!!!     enhanced melting when hailstone collides with liquid water
!!!!     in regions above freezing.  Final diameter returned is ice diameter
!!!!     only. Added hailstone temperature averaging over previous timesteps 
!!!!     to decrease initial temperature instability at small embyro diameters.  
!!!!  3 Sep 2015...................................Becky Adams-Selin AER
!!!!    Insert embryos at -13C; interpret pressure and other variables to
!!!!    that exact temperature level.
!!!! 16 Nov 2015...................................Becky Adams-Selin AER
!!!!     Hailstone travels horizontally through updraft instead of being
!!!!     locked in the center.
!!!!  9 Jul 2016...................................Becky Adams-Selin AER
!!!!     Uses an adiabatic liquid cloud water profile generated from
!!!!     the saturation vapor pressure using the model temperature
!!!!     profile.
!!!! 04 Feb 2017...................................Becky Adams-Selin AER
!!!!     Added adaptive time-stepping option.  
!!!!     ***Don't set any higher than 60 seconds***
!!!! 06 May 2017...................................Becky Adams-Selin AER
!!!!     Bug fixes for some memory issues in the adiabatic liquid
!!!!     water profile code.
!!!!    
!!!! See Adams-Selin and Ziegler 2016, MWR for further documentation.
!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE hailstone_driver ( TCA, h1d, ht, PA, rho1d,&
                                RA, qi1d,qc1d,qr1d,qs1d,qg1d,   &
                                VUU, wdur,                          &
                                nz,dhail1,dhail2,dhail3,dhail4,     &
                                dhail5                             )
    IMPLICIT NONE
    INTEGER, INTENT(IN ) :: nz

    REAL, DIMENSION( nz ),             &
         INTENT(IN   ) ::                                  TCA  & ! temperature (K)
                                              ,          rho1d  &
                                              ,            h1d  &
                                              ,             PA  & ! pressure (Pa)
                                              ,             RA  & ! vapor mixing ratio (kg/kg)
                                              ,            VUU  & ! updraft speed (m/s)
                                              , qi1d,qc1d,qr1d  &
                                              , qs1d,qg1d

    REAL, INTENT(IN   ) ::                                  ht  &
                                              ,           wdur
    
    !Output: 1st-5th rank-ordered hail diameters returned
    REAL, INTENT(INOUT) ::                              dhail1 & ! hail diameter (mm);
                                              ,         dhail2 &
                                              ,         dhail3 &
                                              ,         dhail4 &
                                              ,         dhail5
    !Local variables
    REAL ZBAS, TBAS, WBASP     ! height, temp, pressure of cloud base
    REAL RBAS                  ! mix ratio of cloud base
    REAL cwitot                ! total cloud water, ice mix ratio
    INTEGER KBAS               ! k of cloud base
    REAL tk_embryo             ! temperature at which initial embryo is inserted
    REAL ZFZL, TFZL, WFZLP     ! height, temp, pressure of embryo start point
    REAL RFZL                  ! mix ratio of embryo start point
    REAL VUFZL, DENSAFZL       ! updraft speed, density of embryo start point
    INTEGER KFZL               ! k of embryo start point
    INTEGER nofroze            ! keeps track if hailstone has ever been frozen
    INTEGER CLOUDON            ! use to zero out cloud water, ice once past updraft duration
    REAL RTIME                 ! real updraft duration (sec)
    REAL TAU, TAU_1, TAU_2     ! upper time limit of simulation (sec)
    REAL delTAU                ! difference between TAU_2 and TAU_1 (sec)
    REAL g                     ! gravity (m/s)
    REAL r_d                   ! constant
    !hailstone parameters
    REAL*8 DD, D, D_ICE        ! hail diameter (m)
    REAL VT                    ! terminal velocity (m/s)
    REAL V                     ! actual stone velocity (m/s)
    REAL TS                    ! hailstone temperature (K)
    !HAILSTONE temperature differencing
    REAL TSm1, TSm2            ! hailstone temperature at previous 3 timesteps
    REAL FW                    ! fraction of stone that is liquid
    REAL WATER                 ! mass of stone that is liquid
    REAL CRIT                  ! critical water mass allowed on stone surface
    REAL DENSE                 ! hailstone density (kg/m3)
    INTEGER ITYPE              ! wet (2) or dry (1) growth regime
    !1-d column arrays of updraft parameters
    REAL, DIMENSION( nz ) ::  &
      RIA, &                   ! frozen content mix ratio (kg/kg)
      RWA, &                   ! liquid content mix ratio (kg/kg)
      VUU_pert                 ! perturbed updraft profile (m/s)
    !1-d column arrays of updraft characteristics for adiabatic water profile
    REAL, DIMENSION( nz ) :: &
      RWA_adiabat, &           ! adiabatic liquid content mixing ratio (kg/kg)
      RWA_new, &
      ESVA, &                  ! saturation vapor pressure
      RSA                      ! saturation mixing ratio
    !in-cloud updraft parameters at location of hailstone
    REAL P                     ! in-cloud pressure (Pa)
    REAL RS                    ! in-cloud saturation mixing ratio 
    REAL RI, RW                ! ice, liquid water mix. ratio (kg/kg)
    REAL XI, XW                ! ice, liquid water content (kg/m3 air)
    REAL PC                    ! in-cloud fraction of frozen water
    REAL TC                    ! in-cloud temperature (K)
    REAL VU                    ! in-cloud updraft speed (m/s)
    REAL VUMAX                 ! in-cloud updraft speed read from WRF (max allowed)
    REAL VUCORE                ! perturbed in-cloud updraft speed
    REAL DENSA                 ! in-cloud updraft density (kg/m3)
    REAL Z                     ! height of hailstone (m)
    REAL DELRW                 ! diff in sat vap. dens. between hail and air (kg/m3)
    REAL, DIMENSION(5) :: dhails     !hail diameters from 5 different embryo size
    !mean sub-cloud layer variables
    REAL TLAYER,RLAYER,PLAYER  ! mean sub-cloud temp, mix ratio, pres
    REAL TSUM,RSUM,PSUM        ! sub-cloud layer T, R, P sums
    REAL LDEPTH                ! layer depth
    !internal function variables
    REAL GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DGMV,DI,ANU,RE,AE
    REAL dum, icefactor, adiabatic_factor
      
    REAL sec, secdel           ! time step, increment in seconds
    INTEGER i, j, k, IFOUT, ind(1)
    CHARACTER*256 :: message


    !secdel = 0.05
    secdel = 5.0
    g=9.81
    r_d = 287.
            
    !!!!!!!!!!!!!!!! 1. UPDRAFT PROPERTIES !!!!!!!!!!!!!!!!!!!!!!!
    !!!              DEFINE UPDRAFT PROPERTIES                 !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Upper limit of simulation in seconds
    TAU = 7200.    !simulation ends

    !Set initial updraft strength - you could reduce to simulate the embryo
    ! hovering around the edges of the updraft, as in Heymsfield and Musil (1982)
    DO i=1,nz
       VUU_pert(i) = VUU(i) * 1.
    ENDDO

      
!   Initialize diameters to 0.
    DO i=1,5
       dhails(i) = 0.
    ENDDO

!   Cap updraft lifetime at 2000 sec.
    IF (wdur .GT. 2000) THEN
        RTIME  = 2000.
    ELSE
        RTIME = wdur
    ENDIF

 
    !!!!!!!!!!!!!!!! 2. INITIAL EMBRYO !!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!       FIND PROPERTIES OF INITIAL EMBRYO LOCATION       !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Find the cloud base for end-of-algorithm purposes.
    KBAS=nz
    !KFZL=nz
    DO k=1,nz
         cwitot = qi1d(k) + qc1d(k)
         !No longer include graupel in in-cloud ice amounts
         !RIA(k) = qi1d(k) + qs1d(k) + qg1d(k)
         RIA(k) = qi1d(k) + qs1d(k)
         !RWA(k) = qc1d(k) + qr1d(k)
         RWA(k) = qc1d(k)
         !IF ((RIA(k) .ge. 0.0001) .and. (TCA(k).lt.260.155) .and. &
         !    (k .lt. KFZL)) THEN
         !   KFZL = k
         !ENDIF
         IF ((cwitot .ge. 1.E-12) .and. (k .lt. KBAS)) THEN
            KBAS = k
         ENDIF
    ENDDO
    !QC - our embryo can't start below the cloud base.
    !IF (KFZL .lt. KBAS) THEN
    !   KFZL = KBAS
    !ENDIF

    !Pull heights, etc. of these levels out of 1-d arrays.
    ZBAS = h1d(KBAS)
    TBAS = TCA(KBAS)
    WBASP = PA(KBAS)
    RBAS = RA(KBAS)


    !At coarser resolutions WRF underestimates liquid water aloft.
    !A fairer estimate is of the adiabatic liquid water profile, or 
    !the difference between the saturation mixing ratio at each level
    !and the cloud base water vapor mixing ratio
    DO k=1,nz
       IF (k.LT.KBAS) THEN
           RWA_adiabat(k) = 0.
           CYCLE
       ENDIF
       !saturation vapor pressure
       !ESVA(k) = 6.112*exp((2.453E6/461)*(1./273. - 1./TCA(k))) !hPa
       ESVA(k) = 611.2*exp(17.67*(TCA(k)-273.155)/(TCA(k)-29.655)) !Pa
       !saturation vapor mixing ratio
       RSA(k) = 0.62197 * ESVA(k) / (PA(k) - ESVA(k))
       !Up above -31, start converting to ice, entirely by -38
       !(Rosenfeld and Woodley 2000)
       IF (TCA(k).gt.242.155) THEN
           icefactor = 1.
       ELSE IF ((TCA(k).LE.242.155).AND.(TCA(k).GT.235.155)) THEN
           icefactor = (1-(242.155-TCA(k))/5.)
       ELSE
           icefactor = 0.
       ENDIF
       !Don't want any negative liquid water values
       IF (RBAS.GT.RSA(k)) THEN
          RWA_adiabat(k) = (RBAS - RSA(k))*icefactor
       ELSE
          RWA_adiabat(k) = RWA(k)
       ENDIF
       !Remove cloud liquid water outputted at previous lower levels
       ! -- bug fix 170506 -- !
       IF (k.eq.KBAS) THEN
          RWA_new(k) = RWA_adiabat(k)
       ELSE IF ((k.ge.KBAS+1).AND.(RWA_adiabat(k).ge.1.E-12)) THEN
          RWA_new(k) = RWA_adiabat(k)*(h1d(k)-h1d(k-1)) - RWA_new(k-1)
          IF (RWA_new(k).LT.0) RWA_new(k) = 0.
       ELSE
          RWA_new(k) = RWA(k)
       ENDIF
       ! - old code - ! 
       !IF (k.eq.KBAS+1) THEN
       !   RWA_new(k) = RWA_adiabat(k)*(h1d(k)-h1d(k-1))
       !ELSE IF ((k.ge.KBAS+2).AND.(RWA_adiabat(k).ge.1.E-12)) THEN
       !   RWA_new(k) = RWA_adiabat(k)*(h1d(k)-h1d(k-1)) - RWA_new(k-1)
       !   IF (RWA_new(k).LT.0) RWA_new(k) = 0.
       !ENDIF
    ENDDO
    !remove the height factor from RWA_new
    DO k=KBAS,nz
       RWA_new(k) = RWA_new(k) / (h1d(k)-h1d(k-1))
    ENDDO



    !!!!!!!!!!!!!!!! 3. INITIAL EMBRYO SIZE  !!!!!!!!!!!!!!!!!!!!!
    !!!      SET CONSTANT RANGE OF INITIAL EMBRYO SIZES        !!!
    !!!                     START LOOP                         !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! See Adams-Selin and Ziegler 2016 MWR for explanation of why
    ! these sizes were picked.
    !Run each initial embryo size perturbation
    DO i=1,5
      SELECT CASE (i)   
        CASE (1)
        !Initial hail embryo diameter in m, at cloud base
          DD = 5.E-3
          tk_embryo = 265.155  !-8C
        CASE (2)
          DD = 7.5E-3
          tk_embryo = 265.155  !-8C
        CASE (3)  
          DD = 5.E-3
          tk_embryo = 260.155  !-13C
        CASE (4)
          DD = 7.5E-3
          tk_embryo = 260.155  !-13C
        CASE (5)
          tk_embryo = 260.155  !-13C
          DD = 1.E-2
      END SELECT


      RTIME = 2000.
      IF (wdur .LT. RTIME) RTIME = wdur

      TFZL = tk_embryo
      CALL INTERPP(PA, WFZLP, TCA, tk_embryo, IFOUT, nz)
      CALL INTERP(h1d, ZFZL, WFZLP, IFOUT, PA, nz)
      CALL INTERP(RA,  RFZL, WFZLP, IFOUT, PA, nz)
      CALL INTERP(VUU_pert, VUFZL, WFZLP, IFOUT, PA, nz)
      CALL INTERP(rho1d, DENSAFZL, WFZLP, IFOUT, PA, nz)

      !Begin hail simulation time (seconds)
      sec = 0.
 

      !!!!!!!!!!!!!!!!!!  4. INITIAL PARAMETERS    !!!!!!!!!!!!!!!!!
      !!!          PUT INITIAL PARAMETERS IN VARIABLES           !!!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Set initial values for parameters at freezing level
      P = WFZLP
      RS = RFZL
      TC = TFZL
      VU = VUFZL  
      Z = ZFZL - ht
      LDEPTH = Z
      DENSA = DENSAFZL

      !Set initial hailstone parameters
      nofroze=1 !Set test for embryo: 0 for never been frozen; 1 frozen
      TS = TC
      TSm1 = TS
      TSm2 = TS      
      D = DD   !hailstone diameter in m
      FW = 0.0
      DENSE = 500.  !kg/m3  
      ITYPE=1.  !Assume starts in dry growth.
      CLOUDON=1  !we'll eventually turn cloud "off" once updraft past time limit

      !Start time loop.
      DO WHILE (sec .lt. TAU)
         sec = sec + secdel
         
         !!!!!!!!!!!!!!!!!!  5. CALCULATE PARAMETERS  !!!!!!!!!!!!!!!!!
         !!!              CALCULATE UPDRAFT PROPERTIES              !!!
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !Intepolate vertical velocity to our new pressure level
         CALL INTERP(VUU_pert,VUMAX,P,IFOUT,PA,nz)
         !print *, 'INTERP VU: ', VU, P
         
         !Outside pressure levels?  If so, exit loop
         IF (IFOUT.EQ.1) GOTO 100
                  
         !Sine wave multiplier on updraft strength
         IF (SEC .GT. 0.0 .AND. SEC .LT. RTIME) THEN
            VUCORE = VUMAX * (0.5*SIN((3.14159*SEC)/(RTIME))+0.5)*1.2
            VU = VUCORE      
         ELSEIF (SEC .GE. RTIME) THEN
            VU = 0.0
            CLOUDON = 0
         ENDIF
         
         !Calculate terminal velocity of the hailstone 
         ! (use previous values)
         CALL TERMINL(DENSA,DENSE,D,VT,TC)
         
         !Actual velocity of hailstone (upwards positive)
         V = VU - VT
         
         !Use hydrostatic eq'n to calc height of next level
         P = P - DENSA*g*V*secdel
         Z = Z + V*secdel

         !Interpolate cloud temp, qvapor at new p-level
         CALL INTERP(TCA,TC,P,IFOUT,PA,nz)
         CALL INTERP(RA,RS,P,IFOUT,PA,nz)
         
         !New density of in-cloud air
         DENSA=P/(r_d*(1.+0.609*RS/(1.+RS))*TC)
         
         !Interpolate liquid, frozen water mix ratio at new level
         CALL INTERP(RIA,RI,P,IFOUT,PA,nz)
         CALL INTERP(RWA_new,RW,P,IFOUT,PA,nz)
         XI = RI * DENSA * CLOUDON
         XW = RW * DENSA * CLOUDON
         IF( (XW+XI).GT.0) THEN
           PC = XI / (XW+XI)
         ELSE
           PC = 1.
         ENDIF
         
          
        ! SATURATION VAPOUR DENSITY DIFFERENCE BETWTEEN STONE AND CLOUD
        CALL VAPORCLOSE(DELRW,PC,TS,TC,ITYPE)
      
        
        !!!!!!!!!!!!!!!!!!  6. STONE'S MASS GROWTH !!!!!!!!!!!!!!!!!!!!
        CALL MASSAGR(D,GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DGMV,DI,ANU,RE,AE,&
                 TC,TS,P,DENSE,DENSA,FW,VT,XW,XI,secdel,ITYPE,DELRW) 


        !!!!!!!!!!!!!!!!!!  7. HEAT BUDGET OF HAILSTONE !!!!!!!!!!!!!!!
        CALL HEATBUD(TS,TSm1,TSm2,FW,TC,VT,DELRW,D,DENSA,GM1,GM,DGM,DGMW,  & 
                     DGMV,DGMI,GMW,GMI,DI,ANU,RE,AE,secdel,ITYPE,P)

 
        !!!!! 8. TEST DIAMETER OF STONE AND HEIGHT ABOVE GROUND !!!!!!!
        !!!  TEST IF DIAMETER OF STONE IS GREATER THAN CRITICAL LIMIT, IF SO  
        !!!  BREAK UP 
        WATER=FW*GM  !KG
        ! CRTICAL MASS CAPABLE OF BEING "SUPPORTED" ON THE STONE'S SURFACE 
        CRIT = 2.0E-4
        IF (WATER.GT.CRIT)THEN
           CALL BREAKUP(DENSE,D,GM,FW,CRIT)
        ENDIF
        
        !!! Has stone reached below cloud base?
        !IF (Z .LE. 0) GOTO 200
        IF (Z .LE. ZBAS) GOTO 200

        !calculate ice-only diameter size
        D_ICE = ( (6*GM*(1.-FW)) / (3.141592654*DENSE) )**0.33333333 

        !Has the stone entirely melted and it's below the freezing level?  
        IF ((D_ICE .LT. 1.E-8) .AND. (TC.GT.273.155)) GOTO 300

        !move values to previous timestep value
        TSm1 = TS
        TSm2 = TSm1
        
      ENDDO  !end cloud lifetime loop

100   CONTINUE !outside pressure levels in model
200   CONTINUE !stone reached surface
300   CONTINUE !stone has entirely melted and is below freezing level

      !!!!!!!!!!!!!!!!!! 9. MELT STONE BELOW CLOUD !!!!!!!!!!!!!!!!!!!!
      !Did the stone shoot out the top of the storm? 
      !Then let's assume it's lost in the murky "outside storm" world.
      IF (P.lt.PA(nz)) THEN
         D=0.0
      !Is the stone entirely water? Then set D=0 and exit.
      ELSE IF(ABS(FW - 1.0).LT.0.001) THEN
         D=0.0
      ELSE IF (Z.GT.0) THEN
         !If still frozen, then use melt routine to melt below cloud
         ! based on mean below-cloud conditions.
        
         !Calculate mean sub-cloud layer conditions
         TSUM = 0.
         RSUM = 0.
         PSUM = 0.
         DO k=1,KBAS
            TSUM = TSUM + TCA(k)
            PSUM = PSUM + PA(k)
            RSUM = RSUM + RA(k)
         ENDDO
         TLAYER = TSUM / KBAS
         PLAYER = PSUM / KBAS
         RLAYER = RSUM / KBAS
         
         !MELT is expecting a hailstone of only ice.  At the surface
         !we're only interested in the actual ice diameter of the hailstone,
         !so let's shed any excess water now.
         D_ICE = ( (6*GM*(1.-FW)) / (3.141592654*DENSE) )**0.33333333 
         D = D_ICE  
         CALL MELT(D,TLAYER,PLAYER,RLAYER,LDEPTH,VT)

      ENDIF !end check for melting call

      !Check to make sure something hasn't gone horribly wrong
      IF (D.GT.0.254) D = 0.  !just consider missing for now if > 10 in
      
      !assign hail size in mm for output
      dhails(i) = D * 1000

    ENDDO  !end embryo size loop
  
    
    dhail1 = dhails(1)
    dhail2 = dhails(2)
    dhail3 = dhails(3)
    dhail4 = dhails(4)
    dhail5 = dhails(5)
 
  END SUBROUTINE hailstone_driver


  SUBROUTINE INTERPP(PA,PVAL,TA,TVAL,IFOUT,ITEL)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!
  !!!! INTERP: to linearly interpolate value of pval at temperature tval
  !!!!   between two levels of pressure array pa and temperatures ta
  !!!!
  !!!! INPUT: PA    1D array of pressure, to be interpolated
  !!!!        TA    1D array of temperature
  !!!!        TVAL  temperature value at which we want to calculate pressure
  !!!!        IFOUT set to 0 if TVAL outside range of TA
  !!!!        ITEL  number of vertical levels
  !!!! OUTPUT: PVAL interpolated pressure variable at temperature tval
  !!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT NONE
      
      REAL PVAL, TVAL
      REAL, DIMENSION( ITEL) :: TA, PA
      INTEGER ITEL, IFOUT
      !local variables
      INTEGER I
      REAL FRACT
      
      IFOUT=1
      
      DO I=1,ITEL-1
         IF ( (TVAL .LT. TA(I) .AND. TVAL .GE. TA(I+1)) .or.  &   ! dT/dz < 0
              (TVAL .GT. TA(I) .AND. TVAL .LE. TA(I+1)) ) THEN    ! dT/dz > 0

            FRACT = (TA(I) - TVAL) / (TA(I) - TA(I+1))
            !.... compute the pressure value pval at temperature tval
            PVAL = ((1.0 - FRACT) * PA(I)) + (FRACT * PA(I+1))
          
            !End loop
            IFOUT=0
            EXIT
         ENDIF
      ENDDO
      
  END SUBROUTINE INTERPP



  SUBROUTINE INTERP(AA,A,P,IFOUT,PA,ITEL)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!
  !!!! INTERP: to linearly interpolate values of A at level P
  !!!!   between two levels of AA (at levels PA)
  !!!!
  !!!! INPUT: AA    1D array of variable
  !!!!        PA    1D array of pressure
  !!!!        P     new pressure level we want to calculate A at
  !!!!        IFOUT set to 0 if P outside range of PA
  !!!!        ITEL  number of vertical levels
  !!!! OUTPUT: A    variable at pressure level P
  !!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT NONE
      
      REAL A, P
      REAL, DIMENSION( ITEL) :: AA, PA
      INTEGER ITEL, IFOUT
      !local variables
      INTEGER I
      REAL PDIFF, VDIFF, RDIFF, VERH, ADIFF
      
      IFOUT=1
      
      DO I=1,ITEL-1
        IF (P.LE.PA(I) .AND. P.GT.PA(I+1)) THEN
          !Calculate ratio between vdiff and pdiff
          PDIFF = PA(I)-PA(I+1)
          VDIFF = PA(I)-P
          VERH = VDIFF/PDIFF     
          
          !Calculate the difference between the 2 A values
          RDIFF = AA(I+1) - AA(I)
          
          !Calculate new value
          A = AA(I) + RDIFF*VERH
          
          !End loop
          IFOUT=0
          EXIT
        ENDIF
      ENDDO
      
  END SUBROUTINE INTERP
      

  SUBROUTINE TERMINL(DENSA,DENSE,D,VT,TC)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!
  !!!! INTERP: Calculate terminal velocity of the hailstone
  !!!!
  !!!! INPUT: DENSA  density of updraft air (kg/m3)
  !!!!        DENSE  density of hailstone
  !!!!        D      diameter of hailstone (m)
  !!!!        TC     updraft temperature (K)
  !!!! OUTPUT:VT     hailstone terminal velocity (m/s)
  !!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT NONE
      
      REAL*8 D
      REAL DENSA, DENSE, TC, VT
      REAL GMASS, GX, RE, W, Y
      REAL, PARAMETER :: PI = 3.141592654, G = 9.78956
      REAL ANU
      
      !Mass of stone in kg
      GMASS = (DENSE * PI * (D**3.)) / 6.
      
      !Dynamic viscosity
      ANU = (0.00001718)*(273.155+120.)/(TC+120.)*(TC/273.155)**(1.5)
      
      !CALC THE BEST NUMBER, X AND REYNOLDS NUMBER, RE 
      GX=(8.0*GMASS*G*DENSA)/(PI*(ANU*ANU))
      RE=(GX/0.6)**0.5

      !SELECT APPROPRIATE EQUATIONS FOR TERMINAL VELOCITY DEPENDING ON 
      !THE BEST NUMBER
      IF (GX.LT.550) THEN
        W=LOG10(GX)
        Y= -1.7095 + 1.33438*W - 0.11591*(W**2.0)      
        RE=10**Y
        VT=ANU*RE/(D*DENSA)
      ELSE IF (GX.GE.550.AND.GX.LT.1800) THEN
        W=LOG10(GX)
        Y= -1.81391 + 1.34671*W - 0.12427*(W**2.0) + 0.0063*(W**3.0)
        RE=10**Y
        VT=ANU*RE/(D*DENSA)
      ELSE IF (GX.GE.1800.AND.GX.LT.3.45E08) THEN
        RE=0.4487*(GX**0.5536)
        VT=ANU*RE/(D*DENSA)
      ELSE 
        RE=(GX/0.6)**0.5
        VT=ANU*RE/(D*DENSA)
      ENDIF
      
  END SUBROUTINE TERMINL   

   
   
  SUBROUTINE VAPORCLOSE(DELRW,PC,TS,TC,ITYPE)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!  VAPORCLOSE: CALC THE DIFFERENCE IN SATURATION VAPOUR DENSITY 
  !!!  BETWEEN THAT OVER THE HAILSTONE'S SURFACE AND THE IN-CLOUD 
  !!!  AIR, DEPENDS ON THE WATER/ICE RATIO OF THE UPDRAFT, 
  !!!  AND IF THE STONE IS IN WET OR DRY GROWTH REGIME
  !!!
  !!!  INPUT:  PC    fraction of updraft water that is frozen
  !!!          TS    temperature of hailstone (K)
  !!!          TC    temperature of updraft air (K)
  !!!          ITYPE wet (2) or dry (1) growth regime
  !!!  OUTPUT: DELRW difference in sat vap. dens. between hail and air
  !!!          (kg/m3)
  !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IMPLICIT NONE
      REAL DELRW, PC, TS, TC
      INTEGER ITYPE
      !local variables
      REAL RV, ALV, ALS, RATIO
      DATA RV/461.48/,ALV/2500000./,ALS/2836050./ 
      REAL ESAT, RHOKOR, ESATW, RHOOMGW, ESATI, RHOOMGI, RHOOMG

      !!!  FOR HAILSTONE:  FIRST TEST IF STONE IS IN WET OR DRY GROWTH
      RATIO = 1./273.155
      IF(ITYPE.EQ.2) THEN !!WET GROWTH
        ESAT=611.*EXP(ALV/RV*(RATIO-1./TS))
      ELSE  !!DRY GROWTH
        ESAT=611.*EXP(ALS/RV*(RATIO-1./TS))
      ENDIF
      RHOKOR=ESAT/(RV*TS)
      
      !!!  NOW FOR THE AMBIENT/IN-CLOUD CONDITIONS 
      ESATW=611.*EXP(ALV/RV*(RATIO-1./TC))
      RHOOMGW=ESATW/(RV*TC)
      ESATI=611.*EXP(ALS/RV*(RATIO-1./TC))
      RHOOMGI=ESATI/(RV*TC)
      !RHOOMG=PC*(RHOOMGI-RHOOMGW)+RHOOMGW
      RHOOMG = RHOOMGI  !done as in hailtraj.f

      !!!  CALC THE DIFFERENCE(KG/M3): <0 FOR CONDENSATION, 
      !!!  >0 FOR EVAPORATION
      DELRW=(RHOKOR-RHOOMG) 

  END SUBROUTINE VAPORCLOSE
     
      

  SUBROUTINE MASSAGR(D,GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DGMV,DI,ANU,RE,AE,& 
                 TC,TS,P,DENSE,DENSA,FW,VT,XW,XI,SEKDEL,ITYPE,DELRW) 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! CALC THE STONE'S INCREASE IN MASS 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
      IMPLICIT NONE
      REAL*8 D
      REAL GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DI,ANU,RE,AE,  &
                 TC,TS,P,DENSE,DENSA,FW,VT,XW,XI,SEKDEL,DELRW
      INTEGER ITYPE 
      !local variables
      REAL PI, D0, GMW2, GMI2, EW, EI,DGMV
      REAL DENSEL, DENSELI, DENSELW 
      REAL DC !MEAN CLOUD DROPLET DIAMETER (MICRONS, 1E-6M)
      REAL VOLL, VOLT !VOLUME OF NEW LAYER, TOTAL (M3)
      REAL VOL1, DGMW_NOSOAK, SOAK, SOAKM
      REAL DENSAC, E, AFACTOR, NS, TSCELSIUS, VIMP, W
      PI=3.141592654

      !!!  CALCULATE THE DIFFUSIVITY DI (m2/s)
      D0=0.226*1.E-4  ! change to m2/s, not cm2/s
      DI=D0*(TC/273.155)**1.81*(100000./P)
  
      !!!  COLLECTION EFFICIENCY FOR WATER AND ICE 
      !EW=1.0      
      !!!!   IF TS WARMER THAN -5C THEN ACCRETE ALL THE ICE (EI=1.0) 
      !!!!   OTHERWISE EI=0.21      
      !IF(TS.GE.268.15)THEN
      !  EI=1.00
      !ELSE
      !  EI=0.21
      !ENDIF

      !!!  COLLECTION EFFICIENCY FOR WATER AND ICE 
      EW=1.0      
      !!!  Linear function for ice accretion efficiency
      IF (TC .GE. 273.155) THEN
         EI=1.00
      ELSE IF (TC.GE.233.155) THEN
         EI= 1.0 - ( (273.155 - TS) / 40. )
      ELSE  !cooler than -40C
         EI = 0.0
      ENDIF

      !!!  CALCULATE THE VENTILATION COEFFICIENT - NEEDED FOR GROWTH FROM VAPOR
      !The coefficients in the ventilation coefficient equations have been 
      !experimentally derived, and are expecting cal-C-g units.  Do some conversions.
      DENSAC = DENSA * (1.E3) * (1.E-6)
      !dynamic viscosity 
      ANU=1.717E-4*(393.0/(TC+120.0))*(TC/273.155)**1.5
      !!!  CALCULATE THE REYNOLDS NUMBER - unitless
      RE=D*VT*DENSAC/ANU   
      E=(0.60)**(0.333333333)*(RE**0.50) !ventilation coefficient vapor (fv)
      !!!   SELECT APPROPRIATE VALUES OF AE ACCORDING TO RE
      IF(RE.LT.6000.0)THEN
         AE=0.78+0.308*E
      ELSEIF(RE.GE.6000.0.AND.RE.LT.20000.0)THEN
         AE=0.76*E
      ELSEIF(RE.GE.20000.0) THEN
         AE=(0.57+9.0E-6*RE)*E
      ENDIF


      !!!  CALC HAILSTONE'S MASS (GM), MASS OF WATER (GMW) AND THE  
      !!!  MASS OF ICE IN THE STONE (GMI)
      GM=PI/6.*(D**3.)*DENSE
      GMW=FW*GM
      GMI=GM-GMW
  
      !!!  STORE THE MASS
      GM1=GM
      
      !!! NEW MASS GROWTH CALCULATIONS WITH VARIABLE RIME 
      !!! LAYER DENSITY BASED ON ZIEGLER ET AL. (1983)
      
      !!! CALCULATE INCREASE IN MASS DUE INTERCEPTED CLD WATER, USE
      !!! ORIGINAL DIAMETER
      GMW2=GMW+SEKDEL*(PI/4.*D**2.*VT*XW*EW)
      DGMW=GMW2-GMW 
      GMW=GMW2

      !!!  CALCULATE THE INCREASE IN MASS DUE INTERCEPTED CLOUD ICE
      GMI2=GMI+SEKDEL*(PI/4.*D**2.*VT*XI*EI)
      DGMI=GMI2-GMI 
      GMI=GMI2
  
      !!! CALCULATE INCREASE IN MASS DUE TO SUBLIMATION/CONDENSATION OF 
      !!! WATER VAPOR
      DGMV = SEKDEL*2*PI*D*AE*DI*DELRW
      IF (DGMV .LT. 0) DGMV=0

      !!!  CALCULATE THE TOTAL MASS CHANGE 
      DGM=DGMW+DGMI+DGMV
      !!! CALCULATE DENSITY OF NEW LAYER, DEPENDS ON FW AND ITYPE
      IF (ITYPE.EQ.1) THEN !DRY GROWTH
          !If hailstone encountered supercooled water, calculate new layer density 
          ! using Macklin form
          IF ((DGMW.GT.0).OR.(DGMV.GT.0)) THEN
             !MEAN CLOUD DROPLET RADIUS, ASSUME CLOUD DROPLET CONC OF 3E8 M-3 (300 CM-3)
             DC = (0.74*XW / (3.14159*1000.*3.E8))**0.33333333 * 1.E6 !MICRONS
             !!! FIND THE STOKES NUMBER  (rasmussen heymsfield 1985)
             NS = 2*VT*100.*(DC*1.E-4)**2. / (9*ANU*D*50)  !need hail radius in cm
             !!! FIND IMPACT VELOCITY (rasmussen heymsfield 1985)
             W = LOG10(NS)
             IF (RE.GT.200) THEN
                IF (NS.LT.0.1) THEN
                   VIMP = 0.0
                ELSEIF ((NS.GE.0.1).AND.(NS.LE.10)) THEN
                   VIMP = (0.356 + 0.4738*W - 0.1233*W**2. &
                           -0.1618*W**3. + 0.0807*W**4.)*VT
                ELSEIF (NS.GT.10) THEN
                   VIMP = 0.63*VT
                ENDIF
             ELSEIF ((RE.GT.65).AND.(RE.LE.200)) THEN
                IF (NS.LT.0.1) THEN
                   VIMP = 0.0
                ELSEIF ((NS.GE.0.1).AND.(NS.LE.10)) THEN
                   VIMP = (0.3272 + 0.4907*W - 0.09452*W**2. &
                           -0.1906*W**3. + 0.07105*W**4.)*VT
                ELSEIF (NS.GT.10) THEN
                   VIMP = 0.61*VT
                ENDIF
             ELSEIF ((RE.GT.20).AND.(RE.LE.65)) THEN
                IF (NS.LT.0.1) THEN
                   VIMP = 0.0
                ELSEIF ((NS.GE.0.1).AND.(NS.LE.10)) THEN
                   VIMP = (0.2927 + 0.5085*W - 0.03453*W**2. &
                           -0.2184*W**3. + 0.03595*W**4.)*VT
                ELSEIF (NS.GT.10) THEN
                   VIMP = 0.59*VT
                ENDIF
             ELSEIF (RE.LE.20) THEN
                IF (NS.LT.0.4) THEN
                   VIMP = 0.0
                ELSEIF ((NS.GE.0.4).AND.(NS.LE.10)) THEN
                   VIMP = (0.1701 + 0.7246*W + 0.2257*W**2. &
                           -1.13*W**3. + 0.5756*W**4.)*VT
                ELSEIF (NS.GT.10) THEN
                   VIMP = 0.57*VT
                ENDIF
             ENDIF
              
             !RIME LAYER DENSITY, HEYMSFIELD AND PFLAUM 1985 FORM
             TSCELSIUS = TS - 273.16 
             AFACTOR = -DC*VIMP/TSCELSIUS
             IF ((TSCELSIUS.LE.-5.).OR.(AFACTOR.GE.-1.60)) THEN
                 DENSELW = 0.30*(AFACTOR)**0.44
             ELSEIF (TSCELSIUS.GT.-5.) THEN
                 DENSELW = EXP(-0.03115 - 1.7030*AFACTOR + &
                               0.9116*AFACTOR**2. - 0.1224*AFACTOR**3.)
             ENDIF

             DENSELW = DENSELW * 1000. !KG M-3
             !BOUND POSSIBLE DENSITIES
             IF (DENSELW.LT.100) DENSELW=100
             IF (DENSELW.GT.900) DENSELW=900
             !WRITE(12,*) 'MASSAGR, PFLAUM, MACKLIN: ', DENSELW, &
             !             MACKLIN_DENSELW
          ENDIF
          IF (DGMI.GT.0) THEN
             !Ice collection main source of growth, so set new density layer
             DENSELI = 700.
          ENDIF
          
          !All liquid water contributes to growth, none is soaked into center.
          DGMW_NOSOAK = DGMW  !All liquid water contributes to growth,
                              ! none of it is soaked into center.

      ELSE !WET GROWTH
          !Collected liquid water can soak into the stone before freezing,
          ! increasing mass and density but leaving volume constant.
          !Volume of current drop, before growth 
          VOL1 = GM/DENSE
          !Difference b/w mass of stone if density is 900 kg/m3, and
          ! current mass
          SOAK = 900*VOL1 - GM
          !Liquid mass available
          SOAKM = DGMW
          !Soak up as much liquid as we can, up to a density of 900 kg/m3
          IF (SOAKM.GT.SOAK) SOAKM=SOAK
          GM = GM+SOAKM  !Mass of current drop, plus soaking
          !New density of current drop, including soaking but before growth
          DENSE = GM/VOL1 
          !Mass increment of liquid water growth that doesn't
          ! include the liquid water we just soaked into the stone.
          DGMW_NOSOAK = DGMW - SOAKM
          
          !Whatever growth does occur has high density
          DENSELW = 900.  !KG M-3
          DENSELI = 900.
         
      ENDIF

      !!!VOLUME OF NEW LAYER
      !VOLL = (DGM) / DENSEL
      !VOLL = (DGMI+DGMV+DGMW_NOSOAK) / DENSEL
      !VOLL = (DGMI) / DENSELI + (DGMW_NOSOAK+DGMV) / DENSELW
      IF (DGMI.LE.0) THEN
         VOLL = (DGMW_NOSOAK+DGMV) / DENSELW
      ELSE IF (DGMW.LE.0) THEN
         VOLL = (DGMI) / DENSELI
      ELSE
         VOLL = (DGMI) / DENSELI + (DGMW_NOSOAK+DGMV) / DENSELW
      ENDIF

      !!!NEW TOTAL VOLUME, DENSITY, DIAMETER
      VOLT = VOLL + GM/DENSE
      !DENSE = (GM+DGM) / VOLT
      DENSE = (GM+DGMI+DGMV+DGMW_NOSOAK) / VOLT
      !D=D+SEKDEL*0.5*VT/DENSE*(XW*EW+XI*EI)      
      GM = GM+DGMI+DGMW_NOSOAK+DGMV
      D = ( (6*GM) / (PI*DENSE) )**0.33333333 

  END SUBROUTINE MASSAGR



  SUBROUTINE HEATBUD(TS,TSm1,TSm2,FW,TC,VT,DELRW,D,DENSA,GM1,GM,DGM,DGMW,     &
                     DGMV,DGMI,GMW,GMI,DI,ANU,RE,AE,SEKDEL,ITYPE,P)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! CALCULATE HAILSTONE'S HEAT BUDGET 
  !!! See Rasmussen and Heymsfield 1987; JAS
  !!! Original Hailcast's variable units
  !!! TS - Celsius
  !!! FW - unitless, between 0 and 1
  !!! TC - Celsius
  !!! VT - m/s
  !!! D  - m
  !!! DELRW - g/cm3 (per comment)
  !!! DENSA - g/cm3 (per comment)
  !!! GM1, DMG, DGMW, DGMV, DGMI, GMW, GMI - should all be kg
  !!! DI - cm2 / sec
  !!! P  - hPa
  !!! Original HAILCAST HEATBUD subroutine uses c-g-s units, so do some conversions
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT NONE
      REAL*8 D
      REAL TS,TSm1,TSm2,FW,TC,VT,DELRW,DENSA,GM1,GM,DGM,DGMW,DGMV,  &
                    DGMI,GMW,GMI,DI,ANU,RE,AE,SEKDEL,P
      INTEGER ITYPE
      
      REAL RV, RD, G, PI, ALF, ALV, ALS, CI, CW, AK
      REAL H, AH, TCC, TSC, DELRWC, DENSAC, TDIFF
      REAL DMLT
      REAL TSCm1, TSCm2
      DATA RV/461.48/,RD/287.04/,G/9.78956/
      DATA PI/3.141592654/,ALF/79.7/,ALV/597.3/
      DATA ALS/677.0/,CI/0.5/,CW/1./
      
      !Convert values to non-SI units here
      TSC = TS - 273.155
      TSCm1 = TSm1 - 273.155
      TSCm2 = TSm2 - 273.155
      TCC = TC - 273.155
      DELRWC = DELRW * (1.E3) * (1.E-6)
      DENSAC = DENSA * (1.E3) * (1.E-6)
      !DI still in cm2/sec


      !!!  CALCULATE THE CONSTANTS 
      AK=(5.8+0.0184*TCC)*1.E-5  !thermal conductivity - cal/(cm*sec*K)
      !dynamic viscosity - calculated in MASSAGR
      !ANU=1.717E-4*(393.0/(TC+120.0))*(TC/273.155)**1.5

      !!!  CALCULATE THE REYNOLDS NUMBER - unitless
      !RE=D*VT*DENSAC/ANU  - calculated in MASSAGR  
      
      H=(0.71)**(0.333333333)*(RE**0.50) !ventilation coefficient heat (fh)
      !E=(0.60)**(0.333333333)*(RE**0.50) !ventilation coefficient vapor (fv)

      !!!   SELECT APPROPRIATE VALUES OF AH AND AE ACCORDING TO RE
      IF(RE.LT.6000.0)THEN
         AH=0.78+0.308*H
         !AE=0.78+0.308*E
      ELSEIF(RE.GE.6000.0.AND.RE.LT.20000.0)THEN
         AH=0.76*H
         !AE=0.76*E
      ELSEIF(RE.GE.20000.0) THEN
         AH=(0.57+9.0E-6*RE)*H
         !AE=(0.57+9.0E-6*RE)*E  calculated in MASSAGR
      ENDIF

      !!!  FOR DRY GROWTH FW=0, CALCULATE NEW TS, ITIPE=1 
      !!!  FOR WET GROWTH TS=0, CALCULATE NEW FW, ITIPE=2


      IF(ITYPE.EQ.1) THEN
      !!!  DRY GROWTH; CALC NEW TEMP OF THE STONE 
         !Original Hailcast algorithm (no time differencing)
         !TSC=TSC-TSC*DGM/GM1+SEKDEL/(GM1*CI)*                &
         !   (2.*PI*D*(AH*AK*(TCC-TSC)-AE*ALS*DI*DELRWC)+     &
         !   DGMW/SEKDEL*(ALF+CW*TCC)+DGMI/SEKDEL*CI*TCC)
         TSC=0.6*(TSC-TSC*DGM/GM1+SEKDEL/(GM1*CI)*                &
            (2.*PI*D*(AH*AK*(TCC-TSC)-AE*ALS*DI*DELRWC)+     &
            DGMW/SEKDEL*(ALF+CW*TCC)+DGMI/SEKDEL*CI*TCC)) + &
            0.2*TSCm1 + 0.2*TSCm2
         
         TS = TSC+273.155
         IF (TS.GE.273.155) THEN 
            TS=273.155
         ENDIF
         TDIFF = ABS(TS-273.155)         
         IF (TDIFF.LE.1.E-6) ITYPE=2  !NOW IN WET GROWTH
     
      ELSE IF (ITYPE.EQ.2) THEN
      !!!  WET GROWTH; CALC NEW FW          
         
         IF (TCC.LT.0.) THEN
            !Original Hailcast algorithm
            FW=FW-FW*DGM/GM1+SEKDEL/(GM1*ALF)*               &
                (2.*PI*D*(AH*AK*TCC-AE*ALV*DI*DELRWC)+          &
                DGMW/SEKDEL*(ALF+CW*TCC)+DGMI/SEKDEL*CI*TCC)
         ELSE
            !Calculate decrease in ice mass due to melting
            DMLT = (2.*PI*D*AH*AK*TCC + 2.*PI*D*AE*ALV*DI*DELRWC + &
                    DGMW/SEKDEL*CW*TCC) / ALF
            FW = (FW*GM + DMLT) / GM
         ENDIF
         
         IF(FW.GT.1.)FW=1.
         IF(FW.LT.0.)FW=0.

         !IF ALL OUR ACCRETED WATER WAS FROZEN, WE ARE BACK IN DRY GROWTH
         IF(FW.LE.1.E-6) THEN
            ITYPE=1  
         ENDIF
         
      ENDIF

  END SUBROUTINE HEATBUD


  
  SUBROUTINE BREAKUP(DENSE,D,GM,FW,CRIT)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!  TEST IF AMOUNT OF WATER ON SURFACE EXCEEDS CRTICAL LIMIT- 
  !!!  IF SO INVOKE SHEDDING SCHEME 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IMPLICIT NONE
      REAL*8 D
      REAL DENSE, GM, FW, CRIT
      !local variables
      REAL WATER, GMI, WAT, PI
      DATA PI/3.141592654/

      WATER=FW*GM  !KG
      !GMI=(GM-WATER) !KG
      !REMAIN = CRIT*GM

      ! CALC CRTICAL MASS CAPABLE OF BEING "SUPPORTED" ON THE STONE'S 
      ! SURFACE 
      !CRIT=0.268+0.1389*GMI 
      !CRIT=0.268*1.E-3 + 0.1389*1.E-3*GMI  !mass now in kg instead of g
      !CRIT = 1.0E-10
      !CRIT - now passed from main subroutine

      WAT=WATER-CRIT
      GM=GM-WAT
      FW=(CRIT)/GM
    
      IF(FW.GT.1.0) FW=1.0
      IF(FW.LT.0.0) FW=0.0

      ! RECALCULATE DIAMETER AFTER SHEDDING 
      ! Assume density remains the same
      D=(6.*GM/(PI*DENSE))**(0.333333333)
  END SUBROUTINE BREAKUP
  
  
  SUBROUTINE MELT(D,TLAYER,PLAYER,RLAYER,LDEPTH,VT)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!  This is a spherical hail melting estimate based on the Goyer 
  !!!  et al. (1969) eqn (3).  The depth of the warm layer, estimated 
  !!!  terminal velocity, and mean temperature of the warm layer are 
  !!!  used.  DRB.  11/17/2003.
  !!!
  !!!  INPUT:  TLAYER   mean sub-cloud layer temperature (K)
  !!!          PLAYER   mean sub-cloud layer pressure (Pa)
  !!!          RLAYER   mean sub-cloud layer mixing ratio (kg/kg)
  !!!          VT       terminal velocity of stone (m/s)
  !!!  OUTPUT: D        diameter (m)
  !!!          
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT NONE

      REAL*8 D
      REAL TLAYER, PLAYER, RLAYER, LDEPTH, VT
      REAL eenv, delta, ewet, de, der, wetold, wetbulb, wetbulbk
      REAL tdclayer, tclayer, eps, b, hplayer
      REAL*8 a
      REAL sd, lt, ka, lf, lv, t0, dv, pi, rv, rhoice, &
           tres, re, delt, esenv, rhosenv, essfc, rhosfc, dsig, &
           dmdt, mass, massorg, newmass, gamma, r, rho
      INTEGER wcnt
      
      !Convert temp to Celsius, calculate dewpoint in celsius
      eps = 0.622
      tclayer = TLAYER - 273.155
      a = 2.53E11
      b = 5.42E3
      tdclayer = b / LOG(a*eps / (rlayer*player))
      hplayer = player / 100.
      
      !Calculate partial vapor pressure
      eenv = (player*rlayer) / (rlayer+eps)
      eenv = eenv / 100.  !convert to mb
      
      !Estimate wet bulb temperature (C)
      gamma = 6.6E-4*player
      delta = (4098.0*eenv)/((tdclayer+237.7)*(tdclayer+237.7))
      wetbulb = ((gamma*tclayer)+(delta*tdclayer))/(gamma+delta)
      
      !Iterate to get exact wet bulb
      wcnt = 0
      DO WHILE (wcnt .lt. 11)
        ewet = 6.108*(exp((17.27*wetbulb)/(237.3 + wetbulb))) 
        de = (0.0006355*hplayer*(tclayer-wetbulb))-(ewet-eenv)
        der= (ewet*(.0091379024 - (6106.396/(273.155+wetbulb)**2))) &
             - (0.0006355*hplayer)
        wetold = wetbulb
        wetbulb = wetbulb - de/der
        wcnt = wcnt + 1
        IF ((abs(wetbulb-wetold)/wetbulb.gt.0.0001)) THEN
           EXIT
        ENDIF
      ENDDO
      
      wetbulbk = wetbulb + 273.155  !convert to K
      ka = .02 ! thermal conductivity of air
      lf = 3.34e5 ! latent heat of melting/fusion
      lv = 2.5e6  ! latent heat of vaporization
      t0 = 273.155 ! temp of ice/water melting interface
      dv = 0.25e-4 ! diffusivity of water vapor (m2/s)
      pi = 3.1415927
      rv = 1004. - 287. ! gas constant for water vapor
      rhoice = 917.0 ! density of ice (kg/m**3)
      r = D/2. ! radius of stone (m)
      
      !Compute residence time in warm layer
      tres = LDEPTH / VT
        
      !Calculate dmdt based on eqn (3) of Goyer et al. (1969)
      !Reynolds number...from pg 317 of Atmo Physics (Salby 1996)
      !Just use the density of air at 850 mb...close enough.
      rho = 85000./(287.*TLAYER)
      re = rho*r*VT*.01/1.7e-5
      
      !Temperature difference between environment and hailstone surface
      delt = wetbulb !- 0.0 !assume stone surface is at 0C
                            !wetbulb is in Celsius

      !Difference in vapor density of air stream and equil vapor
      !density at the sfc of the hailstone
      esenv = 610.8*(exp((17.27*wetbulb)/  &
               (237.3 + wetbulb))) ! es environment in Pa
      rhosenv = esenv/(rv*wetbulbk)
      essfc = 610.8*(exp((17.27*(t0-273.155))/  &
               (237.3 + (t0-273.155)))) ! es environment in Pa
      rhosfc = essfc/(rv*t0)
      dsig = rhosenv - rhosfc

      !Calculate new mass growth
      dmdt = (-1.7*pi*r*(re**0.5)/lf)*((ka*delt)+((lv-lf)*dv*dsig))
      IF (dmdt.gt.0.) dmdt = 0
      mass = dmdt*tres
      
      !Find the new hailstone diameter
      massorg = 1.33333333*pi*r*r*r*rhoice
      newmass = massorg + mass
      if (newmass.lt.0.0) newmass = 0.0
      D = 2.*(0.75*newmass/(pi*rhoice))**0.333333333
  END SUBROUTINE MELT

  
END MODULE module_diag_hailcast

#endif
