
!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_fddaobs_rtfdda

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
! Modification History:
!   03212007 Modified fddaobs_init to compute Lambert cone factor. -Al Bourgeois

CONTAINS

!------------------------------------------------------------------------------
  SUBROUTINE fddaobs_init(nudge_opt, maxdom, inest, parid,             &
                          idynin, dtramp, fdaend, restart,             &
                          twindo_cg, twindo, itimestep,                &
                          no_pbl_nudge_uv,                             &
                          no_pbl_nudge_t,                              &
                          no_pbl_nudge_q,                              &
                          sfc_scheme_horiz, sfc_scheme_vert,           &
                          maxsnd_gap,                                  &
                          sfcfact, sfcfacr, dpsmx,                     &
                          nudge_wind, nudge_temp, nudge_mois,          &
                          nudgezfullr1_uv, nudgezrampr1_uv,            &
                          nudgezfullr2_uv, nudgezrampr2_uv,            &
                          nudgezfullr4_uv, nudgezrampr4_uv,            &
                          nudgezfullr1_t,  nudgezrampr1_t,             &
                          nudgezfullr2_t,  nudgezrampr2_t,             &
                          nudgezfullr4_t,  nudgezrampr4_t,             &
                          nudgezfullr1_q,  nudgezrampr1_q,             &
                          nudgezfullr2_q,  nudgezrampr2_q,             &
                          nudgezfullr4_q,  nudgezrampr4_q,             &
                          nudgezfullmin,   nudgezrampmin, nudgezmax,   &
                          xlat, xlong,                                 &
                          start_year, start_month, start_day,          &
                          start_hour, start_minute, start_second,      &
                          p00, t00, tlp,                               &
                          znu, p_top,                                  &
#if ( EM_CORE == 1 )
                          fdob,                                        &
#endif
                          iprt,                                        &
                          ids,ide, jds,jde, kds,kde,                   &
                          ims,ime, jms,jme, kms,kme,                   &
                          its,ite, jts,jte, kts,kte)     
!-----------------------------------------------------------------------
!  This routine does initialization for real time fdda obs-nudging.
!
!-----------------------------------------------------------------------
  USE module_model_constants, ONLY : g, r_d
  USE module_domain
  USE module_dm, ONLY : wrf_dm_min_real
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

!=======================================================================
! Definitions
!-----------
  INTEGER, intent(in)  :: maxdom
  INTEGER, intent(in)  :: nudge_opt(maxdom)
  INTEGER, intent(in)  :: ids,ide, jds,jde, kds,kde,                 &
                          ims,ime, jms,jme, kms,kme,                 &
                          its,ite, jts,jte, kts,kte
  INTEGER, intent(in)  :: inest
  INTEGER, intent(in)  :: parid(maxdom)
  INTEGER, intent(in)  :: idynin         ! flag for dynamic initialization
  REAL,    intent(in)  :: dtramp         ! time period for idynin ramping (min)
  REAL,    intent(in)  :: fdaend(maxdom) ! nudging end time for domain (min)
  LOGICAL, intent(in)  :: restart
  REAL, intent(in)     :: twindo_cg      ! time window on coarse grid
  REAL, intent(in)     :: twindo
  INTEGER, intent(in)  :: itimestep
  INTEGER , INTENT(IN) :: no_pbl_nudge_uv(maxdom)  ! flags for no wind nudging in pbl
  INTEGER , INTENT(IN) :: no_pbl_nudge_t(maxdom)   ! flags for no temperature nudging in pbl
  INTEGER , INTENT(IN) :: no_pbl_nudge_q(maxdom)   ! flags for no moisture nudging in pbl
  INTEGER , INTENT(IN) :: sfc_scheme_horiz ! horizontal spreading scheme for surf obs (wrf or orig mm5)
  INTEGER , INTENT(IN) :: sfc_scheme_vert  ! vertical spreading scheme for surf obs (orig or regime vif)
  REAL    , INTENT(IN) :: maxsnd_gap      ! max allowed pressure gap in soundings, for interp (centibars)
  REAL, intent(in)     :: sfcfact      ! scale factor applied to time window for surface obs  
  REAL, intent(in)     :: sfcfacr      ! scale fac applied to horiz rad of infl for sfc obs
  REAL, intent(in)     :: dpsmx        ! max press change allowed within hor rad of infl
  INTEGER , INTENT(IN) :: nudge_wind(maxdom)       ! wind-nudging flag
  INTEGER , INTENT(IN) :: nudge_temp(maxdom)       ! temperature-nudging flag
  INTEGER , INTENT(IN) :: nudge_mois(maxdom)       ! moisture-nudging flag
  REAL, INTENT(IN)     :: nudgezfullr1_uv  ! vert infl fcn, regime=1 full-wt   hght, winds
  REAL, INTENT(IN)     :: nudgezrampr1_uv  ! vert infl fcn, regime=1 ramp down hght, winds
  REAL, INTENT(IN)     :: nudgezfullr2_uv  ! vert infl fcn, regime=2 full-wt   hght, winds
  REAL, INTENT(IN)     :: nudgezrampr2_uv  ! vert infl fcn, regime=2 ramp down hght, winds
  REAL, INTENT(IN)     :: nudgezfullr4_uv  ! vert infl fcn, regime=4 full-wt   hght, winds
  REAL, INTENT(IN)     :: nudgezrampr4_uv  ! vert infl fcn, regime=4 ramp down hght, winds
  REAL, INTENT(IN)     :: nudgezfullr1_t   ! vert infl fcn, regime=1 full-wt   hght, temp
  REAL, INTENT(IN)     :: nudgezrampr1_t   ! vert infl fcn, regime=1 ramp down hght, temp
  REAL, INTENT(IN)     :: nudgezfullr2_t   ! vert infl fcn, regime=2 full-wt   hght, temp
  REAL, INTENT(IN)     :: nudgezrampr2_t   ! vert infl fcn, regime=2 ramp down hght, temp
  REAL, INTENT(IN)     :: nudgezfullr4_t   ! vert infl fcn, regime=4 full-wt   hght, temp
  REAL, INTENT(IN)     :: nudgezrampr4_t   ! vert infl fcn, regime=4 ramp down hght, temp
  REAL, INTENT(IN)     :: nudgezfullr1_q   ! vert infl fcn, regime=1 full-wt   hght, mois
  REAL, INTENT(IN)     :: nudgezrampr1_q   ! vert infl fcn, regime=1 ramp down hght, mois
  REAL, INTENT(IN)     :: nudgezfullr2_q   ! vert infl fcn, regime=2 full-wt   hght, mois
  REAL, INTENT(IN)     :: nudgezrampr2_q   ! vert infl fcn, regime=2 ramp down hght, mois
  REAL, INTENT(IN)     :: nudgezfullr4_q   ! vert infl fcn, regime=4 full-wt   hght, mois
  REAL, INTENT(IN)     :: nudgezrampr4_q   ! vert infl fcn, regime=4 ramp down hght, mois
  REAL, INTENT(IN)     :: nudgezfullmin    ! min dpth thru which vert infl fcn remains 1.0 (m)
  REAL, INTENT(IN)     :: nudgezrampmin    ! min dpth thru which vif decreases 1.0 to 0.0 (m)
  REAL, INTENT(IN)     :: nudgezmax        ! max dpth in which vif is nonzero (m)
  REAL, INTENT(IN)     :: xlat ( ims:ime, jms:jme )        ! latitudes on mass-point grid
  REAL, INTENT(IN)     :: xlong( ims:ime, jms:jme )        ! longitudes on mass-point grid
  INTEGER, intent(in)  :: start_year   ! Model start year
  INTEGER, intent(in)  :: start_month  ! Model start month
  INTEGER, intent(in)  :: start_day    ! Model start day
  INTEGER, intent(in)  :: start_hour   ! Model start hour
  INTEGER, intent(in)  :: start_minute ! Model start minute
  INTEGER, intent(in)  :: start_second ! Model start second
  REAL, INTENT(IN)     :: p00          ! base state pressure
  REAL, INTENT(IN)     :: t00          ! base state temperature
  REAL, INTENT(IN)     :: tlp          ! base state lapse rate
  REAL, INTENT(IN)     :: p_top        ! pressure at top of model
  REAL, INTENT(IN)     :: znu( kms:kme )      ! eta values on half (mass) levels
#if ( EM_CORE == 1 )
  TYPE(fdob_type), intent(inout)  :: fdob
#endif
  LOGICAL, intent(in)  :: iprt         ! Flag enabling printing warning messages

! Local variables
  logical            :: nudge_flag      ! nudging flag for this nest
  integer            :: ktau            ! current timestep
  integer            :: nest            ! loop counter
  integer            :: idom            ! domain id
  integer            :: parent          ! parent domain
  real               :: conv            ! 180/pi
  real               :: tl1             ! Lambert standard parallel 1
  real               :: tl2             ! Lambert standard parallel 2
  real               :: xn1
  real               :: known_lat       ! Latitude of domain point (i,j)=(1,1)
  real               :: known_lon       ! Longitude of domain point (i,j)=(1,1)
  character(len=200) :: msg             ! Argument to wrf_message
  real               :: z_at_p( kms:kme )  ! height at p levels
  integer            :: i,j,k           ! loop counters

#if ( EM_CORE == 1 )

! Check to see if the nudging flag has been set. If not,
! simply RETURN.
  nudge_flag = (nudge_opt(inest) .eq. 1)
  if (.not. nudge_flag) return

  call wrf_message("")
  write(msg,fmt='(a,i2)') ' OBSERVATION NUDGING IS ACTIVATED FOR MESH ',inest
  call wrf_message(msg)

  ktau  = itimestep
  if(restart) then
    fdob%ktaur = ktau
  else
    fdob%ktaur = 0
  endif

! Create character string containing model starting-date
  CALL date_string(start_year, start_month, start_day, start_hour,        &
                   start_minute, start_second, fdob%sdate)

! Set flag for nudging on pressure (not sigma) surfaces.
  fdob%iwtsig = 0

!**************************************************************************
! *** Initialize datend for dynamic initialization (ajb added 08132008) ***
!**************************************************************************
! Set ending nudging date (used with dynamic ramp-down) to zero.
  fdob%datend = 0.
  fdob%ieodi = 0

! Check for dynamic initialization flag
  if(idynin.eq.1)then
!    Set datend to time in minutes after which data are assumed to have ended.
     if(dtramp.gt.0.)then
        fdob%datend = fdaend(inest) - dtramp
     else
        fdob%datend = fdaend(inest)
     endif
     if(iprt) then
        call wrf_message("")
        write(msg,fmt='(a,i3,a)')                                              &
          ' *** DYNAMIC-INITIALIZATION OPTION FOR INEST = ', inest, ' ***'
        call wrf_message(msg)
        write(msg,*) ' FDAEND,DATEND,DTRAMP: ',fdaend(inest),fdob%datend,dtramp
        call wrf_message(msg)
        call wrf_message("")
     endif
  endif
! *** end dynamic initialization section ***
!**************************************************************************

! Store flags for surface obs spreading schemes
  if(sfc_scheme_horiz.eq.1) then
     call wrf_message('MM5 scheme selected for horizontal spreading of surface obs')
  elseif (sfc_scheme_horiz.eq.0) then
     call wrf_message('WRF scheme selected for horizontal spreading of surface obs')
  else
     write(msg,fmt='(a,i3)') 'Unknown h-spreading scheme for surface obs: ',sfc_scheme_horiz
     call wrf_message(msg)
     call wrf_message("Valid selections: 0=WRF scheme, 1=Original MM5 scheme")
     call wrf_error_fatal ( 'fddaobs_init: module_fddaobs_rtfdda STOP' )
  endif

  if(sfc_scheme_vert.eq.1) then
     call wrf_message('Original simple scheme selected for vertical spreading of surface obs')
  elseif (sfc_scheme_vert.eq.0) then
     call wrf_message("Regime-based VIF scheme selected for vertical spreading of surface obs")
  else
     write(msg,fmt='(a,i3)') 'Unknown v-spreading scheme for surface obs: ',sfc_scheme_vert
     call wrf_message(msg)
     call wrf_message("Valid selections: 0=Regime-based VIF scheme, 1=Original simple scheme")
     call wrf_error_fatal ( 'fddaobs_init: module_fddaobs_rtfdda STOP' )
  endif
  fdob%sfc_scheme_horiz = sfc_scheme_horiz
  fdob%sfc_scheme_vert  = sfc_scheme_vert


! Store temporal and spatial scales
  fdob%sfcfact = sfcfact
  fdob%sfcfacr = sfcfacr

! Set time window.
  fdob%window = twindo
  call wrf_message("")
  write(msg,fmt='(a,i3)') '*** TIME WINDOW SETTINGS FOR NEST ',inest
  call wrf_message(msg)
  write(msg,fmt='(a,f6.3,2(a,f5.3))') '    TWINDO (hrs) = ',twindo,      &
            '  SFCFACT = ',sfcfact,'  SFCFACR = ',sfcfacr
  call wrf_message(msg)
  call wrf_message("")

  if(inest.eq.1) then
    if(twindo .eq. 0.) then
      if(iprt) then
        call wrf_message("")
        write(msg,*) '*** WARNING: TWINDO=0 on the coarse domain.'
        call wrf_message(msg)
        write(msg,*) '*** Did you forget to set twindo in the fdda namelist?'
        call wrf_message(msg)
        call wrf_message("")
      endif
    endif
  else        ! nest
    if(twindo .eq. 0.) then
      fdob%window = twindo_cg
      if(iprt) then
        call wrf_message("")
        write(msg,fmt='(a,i2)') 'WARNING: TWINDO=0. for nest ',inest
        call wrf_message(msg)
        write(msg,fmt='(a,f12.5,a)') 'Default to coarse-grid value of ', twindo_cg,' hrs'
        call wrf_message(msg)
        call wrf_message("")
      endif
    endif
  endif

! Initialize flags.

  fdob%domain_tot=0
  do nest=1,maxdom
    fdob%domain_tot = fdob%domain_tot + nudge_opt(nest)
  end do

! Calculate and store dcon from dpsmx
  if(dpsmx.gt.0.) then
       fdob%dpsmx = dpsmx
       fdob%dcon = 1.0/fdob%dpsmx
  else
       call wrf_error_fatal('fddaobs_init: Namelist variable dpsmx must be greater than zero!')
  endif

! Calculate and store base-state heights at half (mass) levels.
  CALL get_base_state_height_column( p_top, p00, t00, tlp, g, r_d, znu,   &
                                     fdob%base_state,  kts, kte, kds,kde, kms,kme )

! Initialize flags for nudging within PBL.
  fdob%nudge_uv_pbl  = .true.
  fdob%nudge_t_pbl   = .true.
  fdob%nudge_q_pbl   = .true.
  if(no_pbl_nudge_uv(inest) .eq. 1) fdob%nudge_uv_pbl  = .false.
  if(no_pbl_nudge_t(inest) .eq. 1)  fdob%nudge_t_pbl   = .false.
  if(no_pbl_nudge_q(inest) .eq. 1)  fdob%nudge_q_pbl   = .false.

  if(no_pbl_nudge_uv(inest) .eq. 1) then
    fdob%nudge_uv_pbl  = .false.
    write(msg,*) '   --> Obs nudging for U/V is turned off in PBL'
    call wrf_message(msg)
  endif
  if(no_pbl_nudge_t(inest) .eq. 1)  then
    fdob%nudge_t_pbl   = .false.
    write(msg,*) '   --> Obs nudging for T is turned off in PBL'
    call wrf_message(msg)
  endif
  if(no_pbl_nudge_q(inest) .eq. 1)  then
    fdob%nudge_q_pbl   = .false.
    write(msg,*) '   --> Obs nudging for Q is turned off in PBL'
    call wrf_message(msg)
  endif

! Store max allowed pressure gap for interpolating between soundings
  fdob%max_sndng_gap = maxsnd_gap
  write(msg,fmt='(a,f6.1)')  &
  '*** MAX PRESSURE GAP (cb) for interpolation between soundings = ',maxsnd_gap
  call wrf_message(msg)
  call wrf_message("")

! Initialize vertical influence fcn for LML obs
  if(sfc_scheme_vert.eq.0) then
    fdob%vif_uv(1) = nudgezfullr1_uv
    fdob%vif_uv(2) = nudgezrampr1_uv
    fdob%vif_uv(3) = nudgezfullr2_uv
    fdob%vif_uv(4) = nudgezrampr2_uv
    fdob%vif_uv(5) = nudgezfullr4_uv
    fdob%vif_uv(6) = nudgezrampr4_uv
    fdob%vif_t (1) = nudgezfullr1_t
    fdob%vif_t (2) = nudgezrampr1_t
    fdob%vif_t (3) = nudgezfullr2_t
    fdob%vif_t (4) = nudgezrampr2_t
    fdob%vif_t (5) = nudgezfullr4_t
    fdob%vif_t (6) = nudgezrampr4_t
    fdob%vif_q (1) = nudgezfullr1_q
    fdob%vif_q (2) = nudgezrampr1_q
    fdob%vif_q (3) = nudgezfullr2_q
    fdob%vif_q (4) = nudgezrampr2_q
    fdob%vif_q (5) = nudgezfullr4_q
    fdob%vif_q (6) = nudgezrampr4_q

! Sanity checks
    if(nudgezmax.le.0.) then
      write(msg,*) 'STOP! OBS NAMELIST INPUT obs_nudgezmax MUST BE GREATER THAN ZERO.'
      call wrf_message(msg)
      write(msg,*) 'THE NAMELIST VALUE IS',nudgezmax
      call wrf_message(msg)
      call wrf_error_fatal ( 'fddaobs_init: STOP on bad obs_nudgemax value' )
    endif
    if(nudgezfullmin.lt.0.) then
      write(msg,*) 'STOP! OBS NAMELIST INPUT obs_nudgezfullmin MUST BE NONNEGATIVE.'
      call wrf_message(msg)
      write(msg,*) 'THE NAMELIST VALUE IS',nudgezfullmin
      call wrf_message(msg)
      call wrf_error_fatal ( 'fddaobs_init: STOP on bad obs_nudgefullmin value' )
    endif
    if(nudgezrampmin.lt.0.) then
      write(msg,*) 'STOP! OBS NAMELIST INPUT obs_nudgezrampmin MUST BE NONNEGATIVE.'
      call wrf_message(msg)
      write(msg,*) 'THE NAMELIST VALUE IS',nudgezrampmin
      call wrf_message(msg)
      call wrf_error_fatal ( 'fddaobs_init: STOP on bad obs_nudgerampmin value' )
    endif
    if(nudgezmax.lt.nudgezfullmin+nudgezrampmin) then
      write(msg,*) 'STOP! INCONSISTENT OBS NAMELIST INPUTS.'
      call wrf_message(msg)
      write(msg,fmt='(3(a,f12.3))') 'obs_nudgezmax = ',nudgezmax,                &
                              ' obs_nudgezfullmin = ',nudgezfullmin,       &
                              ' obs_nudgezrampmin = ',nudgezrampmin
      call wrf_message(msg)
      write(msg,*) 'REQUIRE NUDGEZMAX >= NUDGEZFULLMIN + NUDGEZRAMPMIN'
      call wrf_message(msg)
      call wrf_error_fatal ( 'fddaobs_init: STOP on inconsistent namelist values' )
    endif

    fdob%vif_fullmin = nudgezfullmin
    fdob%vif_rampmin = nudgezrampmin
    fdob%vif_max     = nudgezmax

! Check to make sure that if nudgzfullmin > 0, then it must be at least as large as the
! first model half-level will be anywhere in the domain at any time within the simulation.
! We use 1.1 times the base-state value fdob%base_state(1) for this purpose.

    if(nudgezfullmin.gt.0.0) then
        if(nudgezfullmin .lt. 1.1*fdob%base_state(1)) then
           fdob%vif_fullmin = 1.1*fdob%base_state(1)
        endif
    endif

! Print vertical weight info only if wind, temperature, or moisture nudging is requested.
    if( (nudge_wind(inest).eq.1) .or. (nudge_temp(inest).eq.1)                             &
                                               .or. (nudge_mois(inest).eq.1) ) then
    call wrf_message("")
    write(msg,fmt='(a,i2,a)') ' *** SETUP DESCRIPTION FOR SURFACE OBS NUDGING ON MESH ',inest,' :'
    call wrf_message(msg)

    call wrf_message("")
    write(msg,fmt='(a,i5,a)') '  NUDGEZMAX: The maximum height at which nudging will be'//     &
                        ' applied from surface obs is ', nint(nudgezmax),' m AGL.'
    call wrf_message(msg)
    call wrf_message("")
    write(msg,fmt='(a,i3,a)') '  NUDGEZFULLMIN: The minimum height of full nudging weight'//   &
                          ' for surface obs is ', nint(fdob%vif_fullmin),' m.'
    call wrf_message(msg)
    if(nudgezfullmin.lt.fdob%vif_fullmin) then
        write(msg,fmt='(a,i3,a)') '  ***WARNING***: NUDGEZFULLMIN has been increased from'//   &
                              ' the user-input value of ',nint(nudgezfullmin),' m.'
        call wrf_message(msg)
        write(msg,fmt='(a,i3,a)') '  to ensure that at least the bottom model level is'//      &
                              ' included in full nudging.'
        call wrf_message(msg)
    endif
    call wrf_message("")
    write(msg,fmt='(a,i3,a)') '  NUDGEZRAMPMIN: The minimum height to ramp from full to no'//  &
                          ' nudging for surface obs is ', nint(nudgezrampmin),' m.'
    call wrf_message(msg)
    call wrf_message("")
    endif   !endif either wind, temperature, or moisture nudging is requested

! Print vif settings
    if(nudge_wind(inest) .eq. 1) then
      call print_vif_var('wind', fdob%vif_uv, nudgezfullmin, nudgezrampmin)
      call wrf_message("")
    endif
    if(nudge_temp(inest) .eq. 1) then
      call print_vif_var('temp', fdob%vif_t,  nudgezfullmin, nudgezrampmin)
      call wrf_message("")
    endif
    if(nudge_mois(inest) .eq. 1) then
      call print_vif_var('mois', fdob%vif_q,  nudgezfullmin, nudgezrampmin)
      call wrf_message("")
    endif

    if( (nudge_wind(inest).eq.1) .or. (nudge_temp(inest).eq.1)                             &
                                                 .or. (nudge_mois(inest).eq.1) ) then
    write(msg,fmt='(a,i2)') ' *** END SETUP DESCRIPTION FOR SURFACE OBS NUDGING ON MESH ',inest
    call wrf_message(msg)
    call wrf_message("")
    endif
  endif   !endif(sfc_scheme_vert.EQ.0)

! Set parameters.
  fdob%pfree = 50.0
  fdob%rinfmn = 1.0
  fdob%rinfmx = 2.0

! Get known lat and lon and store these on all processors in fdob structure, for
! later use in projection x-formation to map (lat,lon) to (x,y) for each obs.
      IF (its .eq. 1 .AND. jts .eq. 1) then
         known_lat = xlat(1,1)
         known_lon = xlong(1,1)
      ELSE
         known_lat = 9999.
         known_lon = 9999.
      END IF
      fdob%known_lat = wrf_dm_min_real(known_lat)
      fdob%known_lon = wrf_dm_min_real(known_lon)

! Calculate the nest levels, levidn. Note that each nest
! must know the nest levels levidn(maxdom) of each domain.
  do nest=1,maxdom

! Initialize nest level for each domain.
    if (nest .eq. 1) then
      fdob%levidn(nest) = 0  ! Mother domain has nest level 0
    else
      fdob%levidn(nest) = 1  ! All other domains start with 1
    endif
    idom = nest
100 parent = parid(idom)      ! Go up the parent tree

      if (parent .gt. 1) then   ! If not up to mother domain
        fdob%levidn(nest) = fdob%levidn(nest) + 1
        idom = parent
        goto 100
      endif
  enddo


! fdob%LML_OBS_HT1_LEV = kte
! HT1: do k = kte, kts, -1
!        if( LML_HT1 .gt. z_at_p(k) ) then
!          fdob%LML_OBS_HT1_LEV = k
!          EXIT HT1
!        endif
!      enddo  HT1

! fdob%LML_OBS_HT2_LEV = kte
! HT2: do k = kte, kts, -1
!        if( LML_HT2 .gt. z_at_p(k) ) then
!          fdob%LML_OBS_HT2_LEV = k
!          EXIT HT2
!        endif
!      enddo HT2
  RETURN
#endif
  END SUBROUTINE fddaobs_init

#if ( EM_CORE == 1 )
!-----------------------------------------------------------------------
SUBROUTINE errob(inest, ub, vb, tb, t0, qvb, pbase, pp, rovcp,  &
                 z,                                             &
                 uratx, vratx, tratx, kpbl,                     &
                 nndgv, nerrf, niobf, maxdom,                   &
                 levidn, parid, nstat, nstaw,                   &
                 iswind, istemp, ismois, ispstr,                &
                 timeob, rio, rjo, rko,                         &
                 varobs, errf, ktau, xtime,                     &
                 iratio, npfi,                                  &
                 prt_max, prt_freq, iprt,                       &
                 obs_prt, stnid_prt, lat_prt, lon_prt,          &
                 mlat_prt, mlon_prt,                            &
                 ids,ide, jds,jde, kds,kde,                     &
                 ims,ime, jms,jme, kms,kme,                     &
                 its,ite, jts,jte, kts,kte  )

!-----------------------------------------------------------------------
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  USE module_dm, ONLY : get_full_obs_vector, wrf_dm_sum_real
#else
  USE module_dm, ONLY :                      wrf_dm_sum_real
#endif
  USE module_model_constants, ONLY : rcp

!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------
!
! PURPOSE: THIS SUBROUTINE CALCULATES THE DIFFERENCE BETWEEN THE
!     OBSERVED VALUES AND THE FORECAST VALUES AT THE OBSERVATION
!     POINTS.  THE OBSERVED VALUES CLOSEST TO THE CURRENT
!     FORECAST TIME (XTIME) WERE DETERMINED IN SUBROUTINE
!     IN4DOB AND STORED IN ARRAY VAROBS.  THE DIFFERENCES
!     CALCULATED BY SUBROUTINE ERROB WILL BE STORED IN ARRAY
!     ERRF FOR THE NSTA OBSERVATION LOCATIONS.  MISSING
!     OBSERVATIONS ARE DENOTED BY THE DUMMY VALUE 99999.
!
!     HISTORY: Original author: MM5 version???
!              02/04/2004 - Creation of WRF version.           Al Bourgeois
!              08/28/2006 - Conversion from F77 to F90         Al Bourgeois
!------------------------------------------------------------------------------

! THE STORAGE ORDER IN ERRF IS AS FOLLOWS:
!        IVAR                VARIABLE TYPE(TAU-1)
!        ----                --------------------
!         1                    U error at obs loc
!         2                    V error at obs loc
!         3                    T error at obs loc
!         4                    Q error at obs loc
!         5                    Vertical layer index for PBL top at IOB, JOB
!         6                    Model surface press at obs loc (T-points)
!         7                    Model surface press at obs loc (U-points)
!         8                    Model surface press at obs loc (V-points)
!         9                    RKO at U-points
!         10                   Model Q at obs loc (T-points)

!-----------------------------------------------------------------------
!
!     Description of input arguments.
!
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: inest                  ! Domain index.
  INTEGER, INTENT(IN)  :: nndgv                  ! Number of nudge variables.
  INTEGER, INTENT(IN)  :: nerrf                  ! Number of error fields.
  INTEGER, INTENT(IN)  :: niobf                  ! Number of observations.
  INTEGER, INTENT(IN)  :: maxdom                 ! Maximum number of domains.
  INTEGER, INTENT(IN)  :: levidn(maxdom)         ! Level of nest.
  INTEGER, INTENT(IN)  :: parid(maxdom)          ! Id of parent grid.
  INTEGER, INTENT(IN)  :: ktau                   ! Model time step index
  REAL, INTENT(IN)     :: xtime                  ! Forecast time in minutes
  INTEGER, INTENT(IN)  :: iratio                 ! Nest to parent gridsize ratio.
  INTEGER, INTENT(IN)  :: npfi                   ! Coarse-grid diagnostics freq.
  INTEGER, INTENT(IN)  :: prt_max                ! Max number of obs to print.
  INTEGER, INTENT(IN)  :: prt_freq               ! Frequency of obs to print.
  LOGICAL, INTENT(IN)  :: iprt                   ! Print flag
  INTEGER, INTENT(IN)  :: obs_prt(prt_max)       ! Print obs indices
  INTEGER, INTENT(IN)  :: stnid_prt(40,prt_max)  ! Print obs station ids
  REAL, INTENT(IN)     :: lat_prt(prt_max)       ! Print obs latitude
  REAL, INTENT(IN)     :: lon_prt(prt_max)       ! Print obs longitude
  REAL, INTENT(IN)     :: mlat_prt(prt_max)      ! Print model lat at obs loc
  REAL, INTENT(IN)     :: mlon_prt(prt_max)      ! Print model lon at obs loc
  INTEGER, INTENT(IN)  :: nstat                  ! # stations held for use
  INTEGER, INTENT(IN)  :: nstaw                  ! # stations in current window
  INTEGER, intent(in)  :: iswind
  INTEGER, intent(in)  :: istemp
  INTEGER, intent(in)  :: ismois
  INTEGER, intent(in)  :: ispstr
  INTEGER, INTENT(IN)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, INTENT(IN)  :: its,ite, jts,jte, kts,kte  ! tile   dims.

  REAL,   INTENT(IN) :: ub( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: vb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: tb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: t0
  REAL,   INTENT(IN) :: qvb( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: pbase( ims:ime, kms:kme, jms:jme )
  REAL,   INTENT(IN) :: pp( ims:ime, kms:kme, jms:jme ) ! Press. perturbation (Pa)
  REAL,   INTENT(IN)  :: rovcp
  REAL,    INTENT(IN) :: z( ims:ime, kms:kme, jms:jme ) ! Ht above sl on half-levels
  REAL,   INTENT(IN) :: uratx( ims:ime, jms:jme ) ! U to U10 ratio on mass points.
  REAL,   INTENT(IN) :: vratx( ims:ime, jms:jme ) ! V to V10 ratio on mass points.
  REAL,   INTENT(IN) :: tratx( ims:ime, jms:jme ) ! T to TH2 ratio on mass points.
  INTEGER,INTENT(IN) :: kpbl( ims:ime, jms:jme )  ! Vertical layer index for PBL top
  REAL,   INTENT(IN) :: timeob(niobf)             ! Obs time (hrs)
  REAL,   INTENT(IN) :: rio(niobf)                ! Obs west-east coordinate (non-stag grid).
  REAL,   INTENT(IN) :: rjo(niobf)                ! Obs south-north coordinate (non-stag grid).
  REAL,   INTENT(INOUT) :: rko(niobf)             ! Obs bottom-top coordinate
  REAL,   INTENT(INOUT) :: varobs(nndgv, niobf)
  REAL,   INTENT(INOUT) :: errf(nerrf, niobf)

! Local variables
  INTEGER :: iobmg(niobf)   ! Obs i-coord on mass grid
  INTEGER :: jobmg(niobf)   ! Obs j-coord on mass grid
  INTEGER :: ia(niobf)
  INTEGER :: ib(niobf)
  INTEGER :: ic(niobf)
  REAL :: pbbo(kds:kde)    ! column base pressure (cb) at obs loc.
  REAL :: ppbo(kds:kde)    ! column pressure perturbation (cb) at obs loc.

  REAL :: ra(niobf)
  REAL :: rb(niobf)
  REAL :: rc(niobf)
  REAL :: dxobmg(niobf)     ! Interp. fraction (x dir) referenced to mass-grid
  REAL :: dyobmg(niobf)     ! Interp. fraction (y dir) referenced to mass-grid
  INTEGER MM(MAXDOM)
  INTEGER NNL
  real :: uratio( ims:ime, jms:jme )   ! U to U10 ratio on momentum points.
  real :: vratio( ims:ime, jms:jme )   ! V to V10 ratio on momentum points.
  real :: pug1,pug2,pvg1,pvg2
  character(len=200) :: msg            ! Argument to wrf_message

! Define staggers for U, V, and T grids, referenced from non-staggered grid.
  real, parameter :: gridx_t = 0.5     ! Mass-point x stagger
  real, parameter :: gridy_t = 0.5     ! Mass-point y stagger
  real, parameter :: gridx_u = 0.0     ! U-point x stagger
  real, parameter :: gridy_u = 0.5     ! U-point y stagger
  real, parameter :: gridx_v = 0.5     ! V-point x stagger
  real, parameter :: gridy_v = 0.0     ! V-point y stagger

  real :: dummy = 99999.

  real :: pbhi, pphi
  real :: obs_pottemp                  ! Potential temperature at observation

!***  DECLARATIONS FOR IMPLICIT NONE
  integer nsta,ivar,n,ityp
  integer iob,job,kob,iob_ms,job_ms
  integer k,kbot,nml,nlb,nle
  integer iobm,jobm,iobp,jobp,kobp,inpf,i,j
  integer i_start,i_end,j_start,j_end    ! loop ranges for uratio,vratio calc.
  integer k_start,k_end
  integer ips                            ! For printing obs information
  integer pnx                            ! obs index for printout

  real gridx,gridy,dxob,dyob,dzob,dxob_ms,dyob_ms
  real pob
  real hob
  real uratiob,vratiob,tratiob,tratxob,fnpf

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
  LOGICAL MP_LOCAL_DUMMASK(NIOBF)  ! Mask for work to be done on this processor
  LOGICAL MP_LOCAL_UOBMASK(NIOBF)  ! Dot-point mask
  LOGICAL MP_LOCAL_VOBMASK(NIOBF)  ! Dot-point mask
  LOGICAL MP_LOCAL_COBMASK(NIOBF)  ! Cross-point mask
#endif

! LOGICAL, EXTERNAL :: TILE_MASK

  NSTA=NSTAT

! FIRST, DETERMINE THE GRID TYPE CORRECTION FOR U-momentum, V-momentum,
! AND MASS POINTS, AND WHEN INEST=2, CONVERT THE STORED COARSE MESH INDICES
! TO THE FINE MESH INDEX EQUIVALENTS

! ITYP=1 FOR U-POINTS, ITYP=2 for V-POINTS, and ITYP=3 FOR MASS POINTS

  if (iprt) then
    write(msg,fmt='(a,i5,a,i2,a,i5,a)') '++++++CALL ERROB AT KTAU = ', &
            KTAU,' AND INEST = ',INEST,':  NSTA = ',NSTAW,' ++++++'
    call wrf_message(msg)
  endif

  ERRF = 0.0    ! Zero out errf array

! Set up loop bounds for this grid's boundary conditions
  i_start = max( its-1,ids )
  i_end   = min( ite+1,ide-1 )
  j_start = max( jts-1,jds )
  j_end   = min( jte+1,jde-1 )
  k_start = kts
  k_end = min( kte, kde-1 )

  DO ITYP=1,3   ! Big loop: ityp=1 for U, ityp=2 for V, ityp=3 for T,Q,SP

!   Set grid stagger
    IF(ITYP.EQ.1) THEN        ! U-POINT CASE
       GRIDX = gridx_u
       GRIDY = gridy_u
    ELSE IF(ITYP.EQ.2) THEN   ! V-POINT CASE
       GRIDX = gridx_v
       GRIDY = gridy_v
    ELSE                      ! MASS-POINT CASE
       GRIDX = gridx_t
       GRIDY = gridy_t
    ENDIF

!   Compute URATIO and VRATIO fields on momentum (u,v) points.
    IF(ityp.eq.1)THEN
      call upoint(i_start,i_end, j_start,j_end, ids,ide, ims,ime, jms,jme, uratx, uratio)
    ELSE IF (ityp.eq.2) THEN
      call vpoint(i_start,i_end, j_start,j_end, jds,jde, ims,ime, jms,jme, vratx, vratio)
    ENDIF

    IF(INEST.EQ.1) THEN       ! COARSE MESH CASE...
      DO N=1,NSTA
        RA(N)=RIO(N)-GRIDX
        RB(N)=RJO(N)-GRIDY
        IA(N)=RA(N)
        IB(N)=RB(N)
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
        DXOB=RA(N)-FLOAT(IA(N))
        DYOB=RB(N)-FLOAT(IB(N))

!       Save mass-point arrays for computing rko for all var types
        if(ityp.eq.1) then
          iobmg(n) = MIN0(MAX0(1,int(RIO(n)-gridx_t)),ide-1)
          jobmg(n) = MIN0(MAX0(1,int(RJO(n)-gridy_t)),jde-1)
          dxobmg(n) = RIO(N)-gridx_t-FLOAT(int(RIO(N)-gridx_t))
          dyobmg(n) = RJO(N)-gridy_t-FLOAT(int(RJO(N)-gridy_t))
        endif
        iob_ms = iobmg(n)
        job_ms = jobmg(n)
        dxob_ms = dxobmg(n)
        dyob_ms = dyobmg(n)


! ajb 20090423: BUGFIX TO OBS_IN_HEIGHT OPTION
! This is tricky: Initialize pob to zero in all procs. Only one proc actually
! calculates pob. If this is an obs to be converted from height-to-pressure, then
! by definition, varobs(5,n) will initially have the missing value -888888. After
! the pob calculation, pob will be zero in all procs except the one that calculated
! it, and so pob is dm_summed over all procs and stored into varobs(5,n). So on
! subsequent passes, the dm_sum will not occur because varobs(5,n) will not have a
! missing value. If this is not an obs-in-height,  

        pob = 0.0

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for obs to be handled by this processor
        MP_LOCAL_DUMMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
  
        IF ( MP_LOCAL_DUMMASK(N) ) THEN
#endif

!         Interpolate pressure to obs location column and convert from Pa to cb.

          do k = kds, kde
            pbbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS) +     &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS) ) + &
                   DYOB_MS* ( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS+1) +   &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS+1) ) )  
            ppbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS) +        &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS) ) +    &
                   DYOB_MS* ( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS+1) +      &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS+1) ) )  
          enddo

!ajb      20040119: Note based on bugfix for dot/cross points split across processors,
!ajb                which was actually a serial code fix: The ityp=2 (v-points) and
!ajb                ityp=3 (mass-points) cases should not use the ityp=1 (u-points)
!ajb                case rko! This is necessary for bit-for-bit reproducability
!ajb                with the parallel run.   (coarse mesh)

          if(abs(rko(n)+99).lt.1.)then
            pob = varobs(5,n)

            if(pob .eq.-888888.) then
               hob = varobs(6,n)
               if(hob .gt. -800000. ) then
                 pob = ht_to_p( hob, ppbo, pbbo, z, iob_ms, job_ms,          &
                                dxob_ms, dyob_ms, k_start, k_end, kds,kde,   &
                                ims,ime, jms,jme, kms,kme )
               endif
            endif

            if(pob .gt.-800000.)then
              do k=k_end-1,1,-1
                kbot = k
                if(pob .le. pbbo(k)+ppbo(k)) then
                  goto 199
                endif
              enddo
 199          continue

              pphi = ppbo(kbot+1)
              pbhi = pbbo(kbot+1)

              rko(n) = real(kbot+1)-                                    &
                 ( (pob-pbhi-pphi) / (pbbo(kbot)+ppbo(kbot)-pbhi-pphi) )

              rko(n)=max(rko(n),1.0)
            endif
          endif

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
        ENDIF       !end IF( MP_LOCAL_DUMMASK(N) )                                 !ajb
#endif

! ajb 20090423: If obs-in-height, varobs(5,n) is sum of pob (see note above).
        if(varobs(5,n) .eq. -888888. .and. varobs(6,n) .gt. -800000.) then
           varobs(5,n) = wrf_dm_sum_real ( pob )
        endif

        RC(N)=RKO(N)

      ENDDO      ! END COARSE MESH LOOP OVER NSTA

    ELSE       ! FINE MESH CASE

!**********************************************************************
!ajb_07012008: Conversion of obs coordinates to the fine mesh here
!ajb   is no longer necessary, since implementation of the WRF map pro-
!ajb   jections (to each nest directly) is implemented in sub in4dob.
!**********************************************************************
!ajb
!ajb  GET (I,J,K) OF OBSERVATIONS ON FINE MESH VALUES.
      DO N=1,NSTA
          RA(N)=RIO(N)-GRIDX           ! ajb added 07012008
          RB(N)=RJO(N)-GRIDY           ! ajb added 07012008
          IA(N)=RA(N)
          IB(N)=RB(N)
          IOB=MAX0(1,IA(N))
          IOB=MIN0(IOB,ide-1)
          JOB=MAX0(1,IB(N))
          JOB=MIN0(JOB,jde-1)
          DXOB=RA(N)-FLOAT(IA(N))
          DYOB=RB(N)-FLOAT(IB(N))

!         Save mass-point arrays for computing rko for all var types
          if(ityp.eq.1) then
            iobmg(n) = MIN0(MAX0(1,int(RIO(n)-gridx_t)),ide-1)
            jobmg(n) = MIN0(MAX0(1,int(RJO(n)-gridy_t)),jde-1)
            dxobmg(n) = RIO(N)-gridx_t-FLOAT(int(RIO(N)-gridx_t))
            dyobmg(n) = RJO(N)-gridy_t-FLOAT(int(RJO(N)-gridy_t))
          endif
          iob_ms = iobmg(n)
          job_ms = jobmg(n)
          dxob_ms = dxobmg(n)
          dyob_ms = dyobmg(n)

! ajb 20090423: BUGFIX TO OBS_IN_HEIGHT OPTION (see COARSE MESH comments)
        pob = 0.0

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for obs to be handled by this processor
        MP_LOCAL_DUMMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)

        IF ( MP_LOCAL_DUMMASK(N) ) THEN
#endif

!         Interpolate pressure to obs location column and convert from Pa to cb.

          do k = kds, kde
            pbbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS) +     &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS) ) + &
                   DYOB_MS* ( (1.-DXOB_MS)*pbase(IOB_MS,K,JOB_MS+1) +   &
                                  DXOB_MS *pbase(IOB_MS+1,K,JOB_MS+1) ) )
            ppbo(k) = .001*(                                            &
               (1.-DYOB_MS)*( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS) +        &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS) ) +    &
                   DYOB_MS* ( (1.-DXOB_MS)*pp(IOB_MS,K,JOB_MS+1) +      &
                                  DXOB_MS *pp(IOB_MS+1,K,JOB_MS+1) ) )
          enddo

!ajb      20040119: Note based on bugfix for dot/cross points split across processors,
!ajb                which was actually a serial code fix: The ityp=2 (v-points) and
!ajb                itype=3 (mass-points) cases should not use the ityp=1 (u-points)
!ajb                case) rko! This is necessary for bit-for-bit reproducability
!ajb                with parallel run.   (fine mesh)

          if(abs(rko(n)+99).lt.1.)then
            pob = varobs(5,n)

            if(pob .eq.-888888.) then
               hob = varobs(6,n)
               if(hob .gt. -800000. ) then
                 pob = ht_to_p( hob, ppbo, pbbo, z, iob_ms, job_ms,          &
                                dxob_ms, dyob_ms, k_start, k_end, kds,kde,   &
                                ims,ime, jms,jme, kms,kme )
               endif
            endif

            if(pob .gt.-800000.)then
              do k=k_end-1,1,-1
                kbot = k
                if(pob .le. pbbo(k)+ppbo(k)) then
                  goto 198
                endif
              enddo
 198          continue

              pphi = ppbo(kbot+1)
              pbhi = pbbo(kbot+1)

              rko(n) = real(kbot+1)-                                    &
                 ( (pob-pbhi-pphi) / (pbbo(kbot)+ppbo(kbot)-pbhi-pphi) )
              rko(n)=max(rko(n),1.0)
            endif
          endif

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
        ENDIF       !end IF( MP_LOCAL_DUMMASK(N) )                                 !ajb
#endif

! ajb 20090423: If obs-in-height, varobs(5,n) is sum of pob (see note above).
        if(varobs(5,n) .eq. -888888. .and. varobs(6,n) .gt. -800000.) then
           varobs(5,n) = wrf_dm_sum_real ( pob )
        endif

        RC(N)=RKO(N)

      ENDDO      ! END FINE MESH LOOP OVER NSTA
    
    ENDIF      ! end if(inest.eq.1)

!   INITIALIZE THE ARRAY OF DIFFERENCES BETWEEN THE OBSERVATIONS
!   AND THE LOCAL FORECAST VALUES TO ZERO.  FOR THE FINE MESH
!   ONLY, SET THE DIFFERENCE TO A LARGE DUMMY VALUE IF THE
!   OBSERVATION IS OUTSIDE THE FINE MESH DOMAIN.

!   SET DIFFERENCE VALUE TO A DUMMY VALUE FOR OBS POINTS OUTSIDE
!   CURRENT DOMAIN
    IF(ITYP.EQ.1) THEN
      NLB=1
      NLE=1
    ELSE IF(ITYP.EQ.2) THEN
      NLB=2
      NLE=2
    ELSE
      NLB=3
      NLE=5
    ENDIF
    DO IVAR=NLB,NLE
      DO N=1,NSTA
        IF((RA(N)-1.).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        IF((RB(N)-1.).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        IF((FLOAT(ide)-2.0*GRIDX-RA(N)-1.E-10).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        IF((FLOAT(jde)-2.0*GRIDY-RB(N)-1.E-10).LT.0)THEN
           ERRF(IVAR,N)=ERRF(IVAR,N)+DUMMY
        ENDIF
        if(rc(n).lt.1.)errf(ivar,n)=errf(ivar,n)+dummy
      ENDDO
    ENDDO

!   NOW FIND THE EXACT OFFSET OF EACH OBSERVATION FROM THE
!   GRID POINT TOWARD THE LOWER LEFT
    DO N=1,NSTA
        IA(N)=RA(N)
        IB(N)=RB(N)
        IC(N)=RC(N)
    ENDDO
    DO N=1,NSTA
        RA(N)=RA(N)-FLOAT(IA(N))
        RB(N)=RB(N)-FLOAT(IB(N))
        RC(N)=RC(N)-FLOAT(IC(N))
    ENDDO
!   PERFORM A TRILINEAR EIGHT-POINT (3-D) INTERPOLATION
!   TO FIND THE FORECAST VALUE AT THE EXACT OBSERVATION
!   POINTS FOR U, V, T, AND Q.

!   Compute local masks for dot and cross points.
    if(ityp.eq.1) then
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for U-momemtum points to be handled by this processor
        MP_LOCAL_UOBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
#endif
      ENDDO
    endif
    if(ityp.eq.2) then
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for V-momentum points to be handled by this processor
        MP_LOCAL_VOBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
#endif
      ENDDO
    endif
    if(ityp.eq.3) then
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
!       Set mask for cross (mass) points to be handled by this processor
        MP_LOCAL_COBMASK(N) = TILE_MASK(IOB, JOB, its, ite, jts, jte)
#endif
      ENDDO
    endif

!**********************************************************
!   PROCESS U VARIABLE (IVAR=1)
!**********************************************************
    IF(ITYP.EQ.1) THEN
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      DO N=1,NSTA
        IF(MP_LOCAL_UOBMASK(N)) THEN
          ERRF(9,N)=rko(n)       !RKO is needed by neighboring processors     !ajb
        ENDIF
      ENDDO
#endif
      IF(ISWIND.EQ.1) THEN
        DO N=1,NSTA
          IOB=MAX0(2,IA(N))
          IOB=MIN0(IOB,ide-1)
          IOBM=MAX0(1,IOB-1)
          IOBP=MIN0(ide-1,IOB+1)
          JOB=MAX0(2,IB(N))
          JOB=MIN0(JOB,jde-1)
          JOBM=MAX0(1,JOB-1)
          JOBP=MIN0(jde-1,JOB+1)
          KOB=MIN0(K_END,IC(N))

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          IF(MP_LOCAL_UOBMASK(N))THEN     ! Do if obs on this processor
#endif
            KOBP=MIN0(KOB+1,k_end)
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)

!           Compute surface pressure values at surrounding U and V points
            PUG1 = .5*( pbase(IOBM,1,JOB) + pbase(IOB,1,JOB) )
            PUG2 = .5*( pbase(IOB,1,JOB) + pbase(IOBP,1,JOB) )

!           This is to correct obs to model sigma level using reverse similarity theory
            if(rko(n).eq.1.0)then
              uratiob=((1.-DYOB)*((1.-DXOB)*uratio(IOB,JOB)+     &
                    DXOB*uratio(IOBP,JOB)                        &
                  )+DYOB*((1.-DXOB)*uratio(IOB,JOBP)+            &
                  DXOB*uratio(IOBP,JOBP)))
            else
              uratiob=1.
            endif
!YLIU       Some PBL scheme do not define the vratio/uratio
            if(abs(uratiob).lt.1.0e-3) then
              uratiob=1.
            endif

!           INITIAL ERRF(IVAR,N) IS ZERO FOR OBSERVATIONS POINTS
!           WITHIN THE DOMAIN, AND A LARGE DUMMY VALUE FOR POINTS
!           OUTSIDE THE CURRENT DOMAIN

!           U COMPONENT WIND ERROR
            ERRF(1,N)=ERRF(1,N)+uratiob*VAROBS(1,N)-((1.-DZOB)*        &
                      ((1.-DyOB)*((1.-                                 &
                      DxOB)*UB(IOB,KOB,JOB)+DxOB*UB(IOB+1,KOB,JOB)     &
                      )+DyOB*((1.-DxOB)*UB(IOB,KOB,JOB+1)+DxOB*        &
                      UB(IOB+1,KOB,JOB+1)))+DZOB*((1.-DyOB)*((1.-DxOB) &
                      *UB(IOB,KOBP,JOB)+DxOB*UB(IOB+1,KOBP,JOB))+      &
                      DyOB*((1.-DxOB)*UB(IOB,KOBP,JOB+1)+DxOB*         &
                      UB(IOB+1,KOBP,JOB+1))))

!      if(n.le.10) then
!        write(6,*)
!        write(6,'(a,i3,i3,i3,a,i3,a,i2)') 'ERRF1 at ',iob,job,kob,   &
!                                     ' N = ',n,' inest = ',inest
!        write(6,*) 'VAROBS(1,N) = ',varobs(1,n)
!        write(6,*) 'VAROBS(5,N) = ',varobs(5,n)
!        write(6,*) 'UB(IOB,KOB,JOB) = ',UB(IOB,KOB,JOB)
!        write(6,*) 'UB(IOB+1,KOB,JOB) = ',UB(IOB+1,KOB,JOB)
!        write(6,*) 'UB(IOB,KOB,JOB+1) = ',UB(IOB,KOB,JOB+1)
!        write(6,*) 'UB(IOB+1,KOB,JOB+1) = ',UB(IOB+1,KOB,JOB+1)
!        write(6,*) 'UB(IOB,KOBP,JOB) = ',UB(IOB,KOBP,JOB)
!        write(6,*) 'UB(IOB+1,KOBP,JOB) = ',UB(IOB+1,KOBP,JOB)
!        write(6,*) 'UB(IOB,KOBP,JOB+1) = ',UB(IOB,KOBP,JOB+1)
!        write(6,*) 'UB(IOB+1,KOBP,JOB+1) = ',UB(IOB+1,KOBP,JOB+1)
!        write(6,*) 'uratiob = ',uratiob
!        write(6,*) 'DXOB,DYOB,DZOB = ',DXOB,DYOB,DZOB
!        write(6,*) 'ERRF(1,N) = ',errf(1,n)
!        write(6,*)
!      endif


!           Store model surface pressure (not the error!) at U point.
            ERRF(7,N)=.001*( (1.-DXOB)*PUG1 + DXOB*PUG2 )
  
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          ENDIF       ! end IF( MP_LOCAL_UOBMASK(N) )
#endif
        ENDDO    ! END U-POINT LOOP OVER OBS

      ENDIF    ! end if(iswind.eq.1)

    ENDIF   ! ITYP=1: PROCESS U

!**********************************************************
!   PROCESS V VARIABLE (IVAR=2)
!**********************************************************
    IF(ITYP.EQ.2) THEN

      IF(ISWIND.EQ.1) THEN
        DO N=1,NSTA
          IOB=MAX0(2,IA(N))
          IOB=MIN0(IOB,ide-1)
          IOBM=MAX0(1,IOB-1)
          IOBP=MIN0(ide-1,IOB+1)
          JOB=MAX0(2,IB(N))
          JOB=MIN0(JOB,jde-1)
          JOBM=MAX0(1,JOB-1)
          JOBP=MIN0(jde-1,JOB+1)
          KOB=MIN0(K_END,IC(N))

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          IF(MP_LOCAL_VOBMASK(N))THEN     ! Do if obs on this processor
#endif
            KOBP=MIN0(KOB+1,k_end)
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)

!           Compute surface pressure values at surrounding U and V points
            PVG1 = .5*( pbase(IOB,1,JOBM) + pbase(IOB,1,JOB) )
            PVG2 = .5*( pbase(IOB,1,JOB) + pbase(IOB,1,JOBP) )

!           This is to correct obs to model sigma level using reverse similarity theory
            if(rko(n).eq.1.0)then
              vratiob=((1.-DYOB)*((1.-DXOB)*vratio(IOB,JOB)+     &
                    DXOB*vratio(IOBP,JOB)                        &
                  )+DYOB*((1.-DXOB)*vratio(IOB,JOBP)+            &
                  DXOB*vratio(IOBP,JOBP)))
            else
              vratiob=1.
            endif
!YLIU       Some PBL scheme do not define the vratio/uratio
            if(abs(vratiob).lt.1.0e-3) then
              vratiob=1.
            endif

!           INITIAL ERRF(IVAR,N) IS ZERO FOR OBSERVATIONS POINTS
!           WITHIN THE DOMAIN, AND A LARGE DUMMY VALUE FOR POINTS
!           OUTSIDE THE CURRENT DOMAIN
  
!           V COMPONENT WIND ERROR
            ERRF(2,N)=ERRF(2,N)+vratiob*VAROBS(2,N)-((1.-DZOB)*        &
                     ((1.-DyOB)*((1.-                                  &
                      DxOB)*VB(IOB,KOB,JOB)+DxOB*VB(IOB+1,KOB,JOB)     &
                      )+DyOB*((1.-DxOB)*VB(IOB,KOB,JOB+1)+DxOB*        &
                      VB(IOB+1,KOB,JOB+1)))+DZOB*((1.-DyOB)*((1.-DxOB) &
                      *VB(IOB,KOBP,JOB)+DxOB*VB(IOB+1,KOBP,JOB))+      &
                      DyOB*((1.-DxOB)*VB(IOB,KOBP,JOB+1)+DxOB*         &
                      VB(IOB+1,KOBP,JOB+1))))

!      if(n.le.10) then
!        write(6,*)
!        write(6,'(a,i3,i3,i3,a,i3,a,i2)') 'ERRF2 at ',iob,job,kob,   &
!                                     ' N = ',n,' inest = ',inest
!        write(6,*) 'VAROBS(2,N) = ',varobs(2,n)
!        write(6,*) 'VAROBS(5,N) = ',varobs(5,n)
!        write(6,*) 'VB(IOB,KOB,JOB) = ',VB(IOB,KOB,JOB)
!        write(6,*) 'ERRF(2,N) = ',errf(2,n)
!        write(6,*) 'vratiob = ',vratiob
!        write(6,*)
!      endif

  
!           Store model surface pressure (not the error!) at V point.
            ERRF(8,N)=.001*( (1.-DYOB)*PVG1 + DYOB*PVG2 )
  
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          ENDIF       ! end IF( MP_LOCAL_VOBMASK(N) )
#endif
        ENDDO    ! END V-POINT LOOP OVER OBS

      ENDIF    ! end if(iswind.eq.1)

    ENDIF   ! ITYP=1: PROCESS V

!**********************************************************
!   PROCESS MASS-POINT VARIABLES IVAR=3 (T) AND IVAR=4 (QV)
!**********************************************************
    IF(ITYP.EQ.3) THEN

      IF(ISTEMP.EQ.1 .OR. ISMOIS.EQ.1) THEN
        DO N=1,NSTA
          IOB=MAX0(1,IA(N))
          IOB=MIN0(IOB,ide-1)
          JOB=MAX0(1,IB(N))
          JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          IF(MP_LOCAL_COBMASK(N)) THEN     ! Do if obs on this processor
#endif
            KOB=MIN0(k_end,IC(N))
            KOBP=MIN0(KOB+1,K_END)
            DXOB=RA(N)
            DYOB=RB(N)
            DZOB=RC(N)

!           This is to correct obs to model sigma level using reverse similarity theory
            if(rko(n).eq.1.0)then
              tratxob=((1.-DYOB)*((1.-DXOB)*tratx(IOB,JOB)+        &
                    DXOB*tratx(IOB+1,JOB)                          &
                  )+DYOB*((1.-DXOB)*tratx(IOB,JOB+1)+              &
                  DXOB*tratx(IOB+1,JOB+1)))
            else
              tratxob=1.
            endif

!yliu
            if(abs(tratxob) .lt. 1.0E-3) tratxob=1.

!ajb We must convert temperature to potential temperature
            obs_pottemp = -888888.
            if(varobs(3,n).gt.-800000. .and. varobs(5,n).gt.-800000) then
              obs_pottemp = varobs(3,n)*(100./varobs(5,n))**RCP - t0
            endif

            ERRF(3,N)=ERRF(3,N)+tratxob*obs_pottemp-((1.-DZOB)*     &
                      ((1.-DyOB)*((1.-                              &
                      DxOB)*(TB(IOB,KOB,JOB))+DxOB*                 &
                      (TB(IOB+1,KOB,JOB)))+DyOB*((1.-DxOB)*         &
                      (TB(IOB,KOB,JOB+1))+DxOB*                     &
                      (TB(IOB+1,KOB,JOB+1))))+DZOB*((1.-            &
                      DyOB)*((1.-DxOB)*(TB(IOB,KOBP,JOB))+DxOB*     &
                      (TB(IOB+1,KOBP,JOB)))+DyOB*((1.-DxOB)*        &
                      (TB(IOB,KOBP,JOB+1))+DxOB*                    &
                      (TB(IOB+1,KOBP,JOB+1)))))

!       if(n.le.10) then
!         write(6,*)
!         write(6,'(a,i3,i3,i3,a,i3,a,i2)') 'ERRF3 at ',iob,job,kob,   &
!                                      ' N = ',n,' inest = ',inest
!         write(6,*) 'VAROBS(3,N) = ',varobs(3,n)
!         write(6,*) 'VAROBS(5,N) = ',varobs(5,n)
!         write(6,*) 'TB(IOB,KOB,JOB) = ',TB(IOB,KOB,JOB)
!         write(6,*) 'TB(IOB+1,KOB,JOB) = ',TB(IOB+1,KOB,JOB)
!         write(6,*) 'TB(IOB,KOB,JOB+1) = ',TB(IOB,KOB,JOB+1)
!         write(6,*) 'TB(IOB+1,KOB,JOB+1) = ',TB(IOB+1,KOB,JOB+1)
!         write(6,*) 'TB(IOB,KOBP,JOB) = ',TB(IOB,KOBP,JOB)
!         write(6,*) 'TB(IOB+1,KOBP,JOB) = ',TB(IOB+1,KOBP,JOB)
!         write(6,*) 'TB(IOB,KOBP,JOB+1) = ',TB(IOB,KOBP,JOB+1)
!         write(6,*) 'TB(IOB+1,KOBP,JOB+1) = ',TB(IOB+1,KOBP,JOB+1)
!         write(6,*) 'tratxob = ',tratxob
!         write(6,*) 'DXOB,DYOB,DZOB = ',DXOB,DYOB,DZOB
!         write(6,*) 'ERRF(3,N) = ',errf(3,n)
!         write(6,*)
!       endif

!           MOISTURE ERROR
            ERRF(4,N)=ERRF(4,N)+VAROBS(4,N)-((1.-DZOB)*((1.-DyOB)*((1.- &
                      DxOB)*QVB(IOB,KOB,JOB)+DxOB*                      &
                      QVB(IOB+1,KOB,JOB))+DyOB*((1.-DxOB)*              &
                      QVB(IOB,KOB,JOB+1)+DxOB*                          &
                      QVB(IOB+1,KOB,JOB+1)))+DZOB*((1.-                 &
                      DyOB)*((1.-DxOB)*QVB(IOB,KOBP,JOB)+DxOB           &
                      *QVB(IOB+1,KOBP,JOB))+DyOB*((1.-DxOB              &
                      )*QVB(IOB,KOBP,JOB+1)+DxOB*                       &
                      QVB(IOB+1,KOBP,JOB+1))))

!           Store model moisture value (not the error!) at the location
!           that the error was calculated
            ERRF(10,N)=ERRF(10,N)+((1.-DZOB)*((1.-DyOB)*((1.- &
                      DxOB)*QVB(IOB,KOB,JOB)+DxOB*                      &
                      QVB(IOB+1,KOB,JOB))+DyOB*((1.-DxOB)*              &
                      QVB(IOB,KOB,JOB+1)+DxOB*                          &
                      QVB(IOB+1,KOB,JOB+1)))+DZOB*((1.-                 &
                      DyOB)*((1.-DxOB)*QVB(IOB,KOBP,JOB)+DxOB           &
                      *QVB(IOB+1,KOBP,JOB))+DyOB*((1.-DxOB              &
                      )*QVB(IOB,KOBP,JOB+1)+DxOB*                       &
                      QVB(IOB+1,KOBP,JOB+1))))


!           Store model surface pressure (not the error!) at T-point
            ERRF(6,N)= .001*                                            &
                      ((1.-DyOB)*((1.-DxOB)*pbase(IOB,1,JOB)+DxOB*      &
                      pbase(IOB+1,1,JOB))+DyOB*((1.-DxOB)*              &
                      pbase(IOB,1,JOB+1)+DxOB*pbase(IOB+1,1,JOB+1) ))

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
          ENDIF       ! end IF( MP_LOCAL_COBMASK(N) )
#endif
        ENDDO     ! END T and QV LOOP OVER OBS

      ENDIF   !end if(istemp.eq.1 .or. ismois.eq.1)

!*******************************************************
!   USE ERRF(5,N) TO STORE KPBL AT (I,J) NEAREST THE OBS
!*******************************************************
      DO N=1,NSTA
        IOB=MAX0(1,IA(N))
        IOB=MIN0(IOB,ide-1)
        JOB=MAX0(1,IB(N))
        JOB=MIN0(JOB,jde-1)
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
        IF(MP_LOCAL_COBMASK(N)) THEN    ! Do if obs on this processor
#endif
          DXOB=RA(N)
          DYOB=RB(N)
          ERRF(5,N) = kpbl(iob+nint(dxob),job+nint(dyob))

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
        ENDIF       ! end IF( MP_LOCAL_COBMASK(N) )
#endif
      ENDDO
!!**********************************************************
    ENDIF   ! end if(ityp.eq.3)

  ENDDO   ! END BIG LOOP

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
! Gather the errf values calculated by the processors with the obs.
  CALL get_full_obs_vector(nsta, nerrf, niobf, mp_local_uobmask,     &
                           mp_local_vobmask, mp_local_cobmask, errf)

! 02252010: Go ahead and assign rko for "obs-off" processors here, to
!           fix the problem where duplicate obs can be dropped from
!           the "obs-on" processor, but not from the others, due to
!           rko being -99 on the "obs-off" processors.
  do n=1,nsta
    rko(n) = errf(9,n)
  enddo
! 02252010: End bugfix.
#endif

! Print obs information.
  CALL print_obs_info(iprt,inest,niobf,rio,rjo,rko,                  &
                      prt_max,prt_freq,obs_prt,stnid_prt,            &
                      lat_prt,lon_prt,mlat_prt,mlon_prt,             &
                      timeob,xtime)

! DIFFERENCE BETWEEN OBS AND FCST IS COMPLETED
  IF(INEST.EQ.1)THEN
    INPF=NPFI
  ELSE
    FNPF=IRATIO**LEVIDN(INEST)
    INPF=FNPF*NPFI
  ENDIF
! Gross error check for temperature. Set all vars bad.
  do n=1,nsta
    if((abs(errf(3,n)).gt.20.).and.           &
           (errf(3,n).gt.-800000.))then

       errf(1,n)=-888888.
       errf(2,n)=-888888.
       errf(3,n)=-888888.
       errf(4,n)=-888888.
       varobs(1,n)=-888888.
       varobs(2,n)=-888888.
       varobs(3,n)=-888888.
       varobs(4,n)=-888888.
    endif
  enddo

! For printout
!  IF(MOD(KTAU,INPF).NE.0) THEN
!      RETURN
!  ENDIF

  RETURN
  END SUBROUTINE errob

  SUBROUTINE upoint(i_start,i_end, j_start,j_end, ids,ide, ims,ime, jms,jme,  &
                    arrin, arrout)
!------------------------------------------------------------------------------
!     PURPOSE: This subroutine interpolates a real 2D array defined over mass
!              coordinate points, to U (momentum) points.
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: i_start     ! Starting i index for this model tile
  INTEGER, INTENT(IN) :: i_end       ! Ending   i index for this model tile
  INTEGER, INTENT(IN) :: j_start     ! Starting j index for this model tile
  INTEGER, INTENT(IN) :: j_end       ! Ending   j index for this model tile
  INTEGER, INTENT(IN) :: ids         ! Starting i index for entire model domain
  INTEGER, INTENT(IN) :: ide         ! Ending   i index for entire model domain
  INTEGER, INTENT(IN) :: ims         ! Starting i index for model patch
  INTEGER, INTENT(IN) :: ime         ! Ending   i index for model patch
  INTEGER, INTENT(IN) :: jms         ! Starting j index for model patch
  INTEGER, INTENT(IN) :: jme         ! Ending   j index for model patch
  REAL,   INTENT(IN)  :: arrin ( ims:ime, jms:jme )  ! input array on mass points
  REAL,   INTENT(OUT) :: arrout( ims:ime, jms:jme )  ! output array on U points

! Local variables
  integer :: i, j

! Do domain interior first
  do j = j_start, j_end
    do i = max(2,i_start), i_end
       arrout(i,j) = 0.5*(arrin(i,j)+arrin(i-1,j))
    enddo
  enddo

! Do west-east boundaries
  if(i_start .eq. ids) then
    do j = j_start, j_end
      arrout(i_start,j) = arrin(i_start,j)
    enddo
  endif
  if(i_end .eq. ide-1) then
    do j = j_start, j_end
      arrout(i_end+1,j) = arrin(i_end,j)
    enddo
  endif

  RETURN
  END SUBROUTINE upoint

  SUBROUTINE vpoint(i_start,i_end, j_start,j_end, jds,jde, ims,ime, jms,jme,  &
                    arrin, arrout)
!------------------------------------------------------------------------------
!     PURPOSE: This subroutine interpolates a real 2D array defined over mass
!              coordinate points, to V (momentum) points.
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: i_start     ! Starting i index for this model tile
  INTEGER, INTENT(IN) :: i_end       ! Ending   i index for this model tile
  INTEGER, INTENT(IN) :: j_start     ! Starting j index for this model tile
  INTEGER, INTENT(IN) :: j_end       ! Ending   j index for this model tile
  INTEGER, INTENT(IN) :: jds         ! Starting j index for entire model domain
  INTEGER, INTENT(IN) :: jde         ! Ending   j index for entire model domain
  INTEGER, INTENT(IN) :: ims         ! Starting i index for model patch
  INTEGER, INTENT(IN) :: ime         ! Ending   i index for model patch
  INTEGER, INTENT(IN) :: jms         ! Starting j index for model patch
  INTEGER, INTENT(IN) :: jme         ! Ending   j index for model patch
  REAL,   INTENT(IN)  :: arrin ( ims:ime, jms:jme )  ! input array on mass points
  REAL,   INTENT(OUT) :: arrout( ims:ime, jms:jme )  ! output array on V points

! Local variables
  integer :: i, j

! Do domain interior first
  do j = max(2,j_start), j_end
    do i = i_start, i_end
      arrout(i,j) = 0.5*(arrin(i,j)+arrin(i,j-1))
    enddo
  enddo

! Do south-north boundaries
  if(j_start .eq. jds) then
    do i = i_start, i_end
      arrout(i,j_start) = arrin(i,j_start)
    enddo
  endif
  if(j_end .eq. jde-1) then
    do i = i_start, i_end
      arrout(i,j_end+1) = arrin(i,j_end)
    enddo
  endif

  RETURN
  END SUBROUTINE vpoint

  LOGICAL FUNCTION TILE_MASK(iloc, jloc, its, ite, jts, jte)
!------------------------------------------------------------------------------
! PURPOSE: Check to see if an i, j grid coordinate is in the tile index range.
!      
! Returns: TRUE if the grid coordinate (ILOC,JLOC) is in the tile defined by
!                  tile-range indices (its,jts) and (ite,jte)
!          FALSE otherwise.
!
!------------------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: iloc
  INTEGER, INTENT(IN) :: jloc
  INTEGER, INTENT(IN) :: its
  INTEGER, INTENT(IN) :: ite
  INTEGER, INTENT(IN) :: jts
  INTEGER, INTENT(IN) :: jte

! Local variables
  LOGICAL :: retval

  TILE_MASK = (iloc .LE. ite .AND. iloc .GE. its .AND.    &
               jloc .LE. jte .AND. jloc .GE. jts )

  RETURN
  END FUNCTION TILE_MASK

!-----------------------------------------------------------------------
  SUBROUTINE nudob(j, ivar, aten, inest, ifrest, ktau, ktaur,         &
                       xtime, mu, c1h, c2h, msfx, msfy,               &
                       nndgv, nerrf, niobf, maxdom,                   &
                       npfi, ionf, rinxy, twindo,                     &
                       nudge_pbl,                                     &
                       sfcfact, sfcfacr,                              &
                       levidn,                                        &
                       parid, nstat,                                  &
                       rinfmn, rinfmx, pfree, dcon, tfaci,            &
                       sfc_scheme_horiz, sfc_scheme_vert, maxsnd_gap, &
                       lev_in_ob, plfo, nlevs_ob,                     &
                       iratio, dx, dtmin, rio, rjo, rko,              &
                       timeob, varobs, errf, pbase, ptop, pp,         &
                       iswind, istemp, ismois, giv, git, giq,         &
                       savwt, kpblt, nscan,                           &
                       vih1, vih2, terrh, zslab,                      &
                       iprt,                                          &
                       ids,ide, jds,jde, kds,kde,                     &  ! domain dims
                       ims,ime, jms,jme, kms,kme,                     &  ! memory dims
                       its,ite, jts,jte, kts,kte,                     &  ! tile   dims
                       qvb, obs_scl_neg_qv_innov ) ! Water vapor mixing ratio / scale negative qv innovations

!-----------------------------------------------------------------------
  USE module_model_constants
  USE module_domain
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------
!
! PURPOSE: THIS SUBROUTINE GENERATES NUDGING TENDENCIES FOR THE J-TH
!     VERTICAL SLICE (I-K PLANE) FOR FOUR-DIMENSIONAL DATA
!     ASSIMILATION FROM INDIVIDUAL OBSERVATIONS.  THE NUDGING
!     TENDENCIES ARE FOUND FROM A ONE-PASS CALCULATION OF
!     WEIGHTING FACTORS SIMILAR TO THE BENJAMIN-SEAMAN OBJECTIVE
!     ANALYSIS.  THIS SUBROUTINE IS DESIGNED FOR RAPID EXECUTION
!     AND MINIMAL STORAGE REQUIREMENTS.  ALGORITHMS SHOULD BE
!     VECTORIZED WHEREVER POSSIBLE.
!
!     HISTORY: Original author: MM5 version???
!              02/04/2004 - Creation of WRF version.           Al Bourgeois
!              08/28/2006 - Conversion from F77 to F90         Al Bourgeois
!------------------------------------------------------------------------------
!
! NOTE: This routine was originally designed for MM5, which uses
!       a nonstandard (I,J) coordinate system. For WRF, I is the
!       east-west running coordinate, and J is the south-north
!       running coordinate. So a "J-slab" here is west-east in
!       extent, not south-north as for MM5.      -ajb 06/10/2004
!
!     NET WEIGHTING (WT) OF THE DIFFERENCE BETWEEN THE OBSERVATIONS
!     AND LOCAL FORECAST VALUES IS BASED ON THE MULTIPLE OF THREE
!     TYPES OF FACTORS:
!       1) TIME WEIGHTING - ONLY OBSERVATIONS WITHIN A SELECTED
!          TIME WINDOW (TWINDO) CENTERED AT THE CURRENT FORECAST
!          TIME (XTIME) ARE USED.  OBSERVATIONS CLOSEST TO
!          XTIME ARE TIME-WEIGHTED MOST HEAVILY (TIMEWT)
!       2) VERTICAL WEIGHTING - NON-ZERO WEIGHTS (WTSIG) ARE
!          CALCULATED WITHIN A VERTICAL REGION OF INFLUENCE
!          (RINSIG).
!       3) HORIZONTAL WEIGHTING - NON-ZERO WEIGHTS (WTIJ) ARE
!          CALCULATED WITHIN A RADIUS OF INFLUENCE (RINXY).  THE
!          VALUE OF RIN IS DEFINED IN KILOMETERS, AND CONVERTED
!          TO GRID LENGTHS FOR THE APPROPRIATE MESH SIZE.
!
!     THE FIVE FORECAST VARIABLES ARE PROCESSED BY CHANGING THE
!     VALUE OF IVAR AS FOLLOWS:
!             IVAR                     VARIABLE(TAU-1)
!             ----                     ---------------
!               1                             U
!               2                             V
!               3                             T
!               4                             QV
!               5                          PSB(CROSS)   REMOVED IN V3
!              (6)                           PSB(DOT)
!
!-----------------------------------------------------------------------
!
!     Description of input arguments.
!
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: ids,ide, jds,jde, kds,kde  ! domain dims.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme  ! memory dims.
  INTEGER, INTENT(IN)  :: its,ite, jts,jte, kts,kte  ! tile   dims.
  INTEGER, INTENT(IN)  :: j                          ! south-north running coordinate.
  INTEGER, INTENT(IN)  :: ivar
  INTEGER, INTENT(IN)  :: inest                      ! domain index
  LOGICAL, INTENT(IN)  :: ifrest
  INTEGER, INTENT(IN)  :: ktau
  INTEGER, INTENT(IN)  :: ktaur
  REAL, INTENT(IN)     :: xtime                      ! forecast time in minutes
  INTEGER, INTENT(IN)  :: nndgv                      ! number of nudge variables
  INTEGER, INTENT(IN)  :: nerrf                      ! number of error fields
  INTEGER, INTENT(IN)  :: niobf                      ! number of observations
  INTEGER, INTENT(IN)  :: maxdom                     ! maximum number of domains
  INTEGER, INTENT(IN)  :: npfi
  INTEGER, INTENT(IN)  :: ionf
  REAL, INTENT(IN)     :: rinxy
  REAL, INTENT(IN)     :: twindo
  REAL, intent(in)     :: sfcfact                    ! scale for time window (surface obs)
  REAL, intent(in)     :: sfcfacr                    ! scale for hor rad inf (surface obs)
  LOGICAL, intent(in)  :: nudge_pbl                  ! flag for nudging in pbl
  INTEGER, INTENT(IN)  :: levidn(maxdom)             ! level of nest
  INTEGER, INTENT(IN)  :: parid(maxdom)              ! parent domain id
  INTEGER, INTENT(IN)  :: nstat                      ! number of obs stations
  REAL,    INTENT(IN)  :: rinfmn          ! minimum radius of influence
  REAL,    INTENT(IN)  :: rinfmx          ! maximum radius of influence
  REAL,    INTENT(IN)  :: pfree           ! pressure level (cb) where terrain effect becomes small
  REAL,    INTENT(IN)  :: dcon            ! 1/DPSMX
  REAL,    INTENT(IN)  :: tfaci           ! scale factor used for ramp-down in dynamic initialization
  INTEGER , INTENT(IN) :: sfc_scheme_horiz ! horizontal spreading scheme for surf obs (wrf or orig mm5)
  INTEGER , INTENT(IN) :: sfc_scheme_vert  ! vertical   spreading scheme for surf obs (orig or regime vif)
  REAL    , INTENT(IN) :: maxsnd_gap      ! max allowed pressure gap in soundings, for interp (centibars)
  REAL, INTENT(IN)     :: lev_in_ob(niobf)           ! Level in sounding-type obs.
  REAL, intent(IN)     :: plfo(niobf)                ! Index for type of obs platform
  REAL, INTENT(IN)     :: nlevs_ob(niobf)            ! Number of levels in sounding.
  INTEGER, INTENT(IN)  :: iratio                     ! Nest to parent gridsize ratio.
  REAL, INTENT(IN)     :: dx                         ! This domain grid cell-size (m)
  REAL, INTENT(IN)     :: dtmin                      ! Model time step in minutes
  REAL, INTENT(IN)     :: rio(niobf)                 ! Obs west-east coordinate (non-stag grid).
  REAL, INTENT(IN)     :: rjo(niobf)                 ! Obs south-north coordinate (non-stag grid).
  REAL, INTENT(INOUT)  :: rko(niobf)                 ! Obs vertical coordinate.
  REAL, INTENT(IN)     :: timeob(niobf)
  REAL, INTENT(IN)     :: varobs(nndgv,niobf)
  REAL, INTENT(IN)     :: errf(nerrf, niobf)
  REAL, INTENT(IN)     :: pbase( ims:ime, kms:kme )  ! Base pressure.
  REAL, INTENT(IN)     :: ptop
  REAL, INTENT(IN)     :: pp( ims:ime, kms:kme ) ! Pressure perturbation (Pa)
  REAL, INTENT(IN) , DIMENSION(ims:ime)    :: mu   ! Air mass on u, v, or mass-grid
  REAL, INTENT(IN) , DIMENSION(kms:kme)    :: c1h  ! Hybrid coordinate weight
  REAL, INTENT(IN) , DIMENSION(kms:kme)    :: c2h  ! Hybrid coordinate weight
  REAL, INTENT(IN)     :: msfx(ims:ime)  ! Map scale (only used for vars u & v)
  REAL, INTENT(IN)     :: msfy(ims:ime)  ! Map scale (only used for vars u & v)
  INTEGER, intent(in)  :: iswind        ! Nudge flag for wind
  INTEGER, intent(in)  :: istemp        ! Nudge flag for temperature
  INTEGER, intent(in)  :: ismois        ! Nudge flag for moisture
  REAL, intent(in)     :: giv           ! Coefficient for wind
  REAL, intent(in)     :: git           ! Coefficient for temperature
  REAL, intent(in)     :: giq           ! Coefficient for moisture
  REAL, INTENT(INOUT)  :: aten( ims:ime, kms:kme)
  REAL, INTENT(INOUT)  :: savwt( nndgv, ims:ime, kms:kme )
  INTEGER, INTENT(IN)  :: kpblt(ims:ime)
  INTEGER, INTENT(IN)  :: nscan                      ! number of scans
  REAL, INTENT(IN)     :: vih1(its:ite) ! Vert infl ht (m) abv grd for full wts
  REAL, INTENT(IN)     :: vih2(its:ite) ! Vert infl ht (m) abv grd for ramp
  REAL, INTENT(IN)     :: terrh(ims:ime) ! Terrain height (m)
! INTEGER, INTENT(IN)  :: vik1(its:ite) ! Vertical infl k-level for full wts
! INTEGER, INTENT(IN)  :: vik2(its:ite) ! Vertical infl k-level for ramp
  REAL, INTENT(IN)     :: zslab(ims:ime, kms:kme)    ! model ht above sea level (m)
  LOGICAL, INTENT(IN)  :: iprt                       ! print flag
  REAL                 :: abs_pdiff_below, abs_pdiff_above
  REAL,   INTENT(IN)   :: qvb( ims:ime, kms:kme, jms:jme ) ! Water vapor mixing ratio (QV)
  INTEGER, INTENT(IN)  :: obs_scl_neg_qv_innov ! User choice on whether negative qv innovations should be scaled
  REAL                 :: qvb_at_cur_loc, qvb_at_ob_loc ! QV at the current model or the observation location
  REAL                 :: SCALE_FACTOR_NEG_QV_INNOV ! Multiply QV innovation by this factor to avoid nudging toward negative QV
  REAL                 :: QVB_CUR_LOC_OVER_OB_LOC ! Ratio of QV at current model location compared to ob location

! Local variables
  integer :: mm(maxdom)
  integer :: kobs                  ! k-lev below obs (for obs straddling pblt)
  integer :: kpbl_obs(nstat)       ! kpbl at grid point (IOB,JOB) (ajb 20090519)
  real :: ra(niobf)
  real :: rb(niobf)
  real :: psurf(niobf)
  real :: wtsig(kms:kme),wt(ims:ime,kms:kme),wt2err(ims:ime,kms:kme)
  real :: rscale(ims:ime)           ! For converting to rho-coupled units.
  real :: wtij(ims:ime)             ! For holding weights in i-loop
  real :: reserf(kms:kme)
  character*40 name
  character*3 chr_hr
  character(len=200) :: msg            ! Argument to wrf_message

!***  DECLARATIONS FOR IMPLICIT NONE
  integer :: i,k,iplo,icut,ipl,inpf,infr,jjjn
  integer :: igrid,n,nml,nnl,nsthis,nsmetar,nsspeci,nsship
  integer :: nssynop,nstemp,nspilot,nssatwnds,nssams,nsprofs
  integer :: maxi,mini,maxj,minj,nnn,nsndlev,njcsnd,kob
  integer :: komin,komax,nn,nhi,nlo,nnjc
  integer :: i_s,i_e
  integer :: istq
  real :: gfactor,rfactor,gridx,gridy,rindx,ris
  real :: grfacx,grfacy
  real :: timewt,pob
  real :: ri,rj,rx,ry,rsq,pdfac,erfivr,dk,slope,rinfac
  real :: dprim,dsq,d     ! vars for mm5 surface-ob weighting
  real :: rinprs,pijk,pobhi,poblo,pdiffj,w2eowt,gitq
  real :: dz_ramp         ! For ramping weights for surface obs

  real :: scratch
  integer :: kk !ajb temp

!      print *,'start nudob, nstat,j,ivar=',nstat,j,ivar
!      if(ivar.ne.4)return
!yliu start -- for multi-scans: NSCAN=0: original
!                               NSCAN=1: added a scan with a larger Ri and smaller G
!       if(NSCAN.ne.0 .and. NSCAN.ne.1)  stop
! ajb note: Will need to increase memory for SAVWT if NSCAN=1:
  if(NSCAN.ne.0) then
    IF (iprt) then
        write(msg,*) 'SAVWT must be resized for NSCAN=1'
        call wrf_message(msg)
    ENDIF
    call wrf_error_fatal ( 'wrf_fddaobs_in: in4dob' )
  endif
  IPLO=0  + NSCAN*4
  GFACTOR=1. +  NSCAN*(-1. + 0.33333)
  RFACTOR=1. +  NSCAN*(-1. + 3.0)
!yliu end
! jc

! return if too close to j boundary
  if(inest.eq.1.and.ivar.lt.3.and.(j.le.2.or.j.ge.jde-1)) then
!       write(6,*) '1 RETURN: IVAR = ',ivar,' J = ',j,
!    $             ' too close to boundary.'
    return
  endif
  if(inest.eq.1.and.ivar.ge.3.and.(j.le.2.or.j.ge.jde-2)) then
!       write(6,*) '2 RETURN: IVAR = ',ivar,' J = ',j,
!    $             ' too close to boundary.'
    return
  endif

! COMPUTE IPL WHICH REPRESENTS IVAR FOR EACH MESH IN SAVWT MODS
  ICUT=0
  IF(INEST.GT.1)ICUT=1
  i_s = max0(2+icut,its)
  i_e = min0(ide-1-icut,ite)

  IPL=IVAR    + IPLO     !yliu +IPLO

! DEFINE GRID-TYPE OFFSET FACTORS, IGRID AND GRID

  INPF=(IRATIO**LEVIDN(INEST))*NPFI
  INFR=(IRATIO**LEVIDN(INEST))*IONF

  GRIDX=0.0
  GRIDY=0.0
  IGRID=0
  IF(IVAR.GE.3)THEN
    GRIDX=0.5
    GRIDY=0.5
    IGRID=1
  ELSEIF(IVAR.eq.1) THEN
    GRIDY=0.5
    GRIDX=0.0
    IGRID=1
  ELSEIF(IVAR.eq.2) THEN
    GRIDX=0.5
    GRIDY=0.0
    IGRID=1
  ENDIF

! TRANSFORM THE HORIZONTAL RADIUS OF INFLUENCE, RINXY, FROM
! KILOMETERS TO GRID LENGTHS, RINDX

  RINDX=RINXY*1000./DX          * RFACTOR   !yliu *RFACTOR
  RIS=RINDX*RINDX
  IF(IFREST.AND.KTAU.EQ.KTAUR)GOTO 5
  IF(MOD(KTAU,INFR).NE.0)GOTO 126
5 CONTINUE
  IF (iprt) THEN
   IF(J.EQ.10) then
       write(msg,6) INEST,J,KTAU,XTIME,IVAR,IPL,rindx
       call wrf_message(msg)
   ENDIF
  ENDIF
6 FORMAT(1X,'OBS NUDGING FOR IN,J,KTAU,XTIME,',                    &
            'IVAR,IPL: ',I2,1X,I2,1X,I5,1X,F8.2,1X,I2,1X,I2,       &
            ' rindx=',f4.1)

!********************************************************************
!ajb_07012008  Setting ra and rb for the fine mesh her is now simple.
!              Values are no longer calculated here based on the
!              coarse mesh, since direct use of WRF map projections
!              on each nest was implemented in subroutine in4dob.
!********************************************************************
! SET RA AND RB
  DO N=1,NSTAT
    RA(N)=RIO(N)-GRIDX
    RB(N)=RJO(N)-GRIDY
  ENDDO

! INITIALIZE WEIGHTING ARRAYS TO ZERO
  DO I=its,ite
    DO K=1,kte
      WT(I,K)=0.0
      WT2ERR(I,K)=0.0
    ENDDO
  ENDDO

! DO P* COMPUTATIONS ON DOT POINTS FOR IVAR.LT.3 (U AND V)
! AND CROSS POINTS FOR IVAR.GE.3 (T,Q,P*).
!
! COMPUTE P* AT OBS LOCATION (RA,RB).  DO THIS AS SEPARATE VECTOR LOOP H
! SO IT IS ALREADY AVAILABLE IN NSTAT LOOP 120 BELOW

! PSURF IS NOT AVAILABLE GLOBALLY, THEREFORE, THE BILINEAR INTERPOLATION
! AROUND THE OBS POINT IS DONE IN ERROB() AND STORED IN ERRF([678],N) FOR
! THE POINT (6=PRESS, 7=U-MOM, 8=V-MOM).
! ajb 05052009: psurf is actually pbase(k=1) interpolated to obs (i,j).
  DO N=1,NSTAT
    IF(IVAR.GE.3)THEN
      PSURF(N)=ERRF(6,N)
    ELSE
      IF(IVAR.EQ.1)THEN
        PSURF(N)=ERRF(7,N)        ! U-points
      ELSE
        PSURF(N)=ERRF(8,N)        ! V-points
      ENDIF
    ENDIF
  ENDDO

! DETERMINE THE LIMITS OF THE SEARCH REGION FOR THE CURRENT
! J-STRIP

  MAXJ=J+IFIX(RINDX*RINFMX+0.99)                                             !ajb
  MINJ=J-IFIX(RINDX*RINFMX+0.99)                                             !ajb

! jc comment out this? want to use obs beyond the domain?
!      MAXJ=MIN0(JL-IGRID,MAXJ)           !yliu
!      MINJ=MAX0(1,MINJ)                  !yliu

  n=1

!***********************************************************************
  DO nnn=1,NSTAT   ! BEGIN OUTER LOOP FOR THE NSTAT OBSERVATIONS
!***********************************************************************
! Soundings are consecutive obs, but they need to be treated as a single
! entity. Thus change the looping to nnn, with n defined separately.


!yliu
!  note for sfc data: nsndlev=1 and njcsnd=1
    nsndlev=int(nlevs_ob(n)-lev_in_ob(n))+1   

! yliu start -- set together with the other parts
! test: do the sounding levels as individual obs
!   nsndlev=1
! yliu end
    njcsnd=nsndlev
! set pob here, to be used later
    pob=varobs(5,n)

!     print *, "s-- n=,nsndlev",n,njcsnd,J, ipl
!     print *, "s--",ivar,(errf(ivar,i),i=n,n+njcsnd)
! CHECK TO SEE OF STATION N HAS DATA FOR VARIABLE IVAR
! AND IF IT IS SUFFICIENTLY CLOSE TO THE J STRIP.  THIS
! SHOULD ELIMINATE MOST STATIONS FROM FURTHER CONSIDER-
! ATION.

!yliu: Skip bad obs if it is sfc or single level sounding.
!yliu: Before this (020918), a snd will be skipped if its first
!yliu        level has bad data- often true due to elevation

    IF( ABS(ERRF(IVAR,N)).GT.9.E4 .and. njcsnd.eq.1 ) THEN
!     print *, " bad obs skipped"

    ELSEIF( RB(N).LT.FLOAT(MINJ) .OR. RB(N).GT.FLOAT(MAXJ) ) THEN
!     print *, " skipped obs far away from this J-slice"

!----------------------------------------------------------------------
    ELSE    ! BEGIN SECTION FOR PROCESSING THE OBSERVATION
!----------------------------------------------------------------------

! DETERMINE THE LIMITS OF APPLICATION OF THE OBS IN THE VERTICAL
! FOR THE VERTICAL WEIGHTING, WTSIG

! ASSIMILATE OBSERVATIONS ON PRESSURE LEVELS, EXCEPT FOR SURFACE
!ajb 20021210: (Bugfix) RKO is not available globally. It is computed in
!ajb ERROB() by the processor handling the obs point, and stored in ERRF(9,N).

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      rko(n) = errf(9,n)        !ajb 20021210
      kpbl_obs(n) = errf(5,n)   !ajb 20090519
#endif
!ajb 20090427 added .45 to rko so KOB is equal to 1 only if RKO > 1.05
      KOB=nint(RKO(N)+0.45)
      KOB=MIN0(kte,KOB)
      KOB=MAX0(1,KOB)

! ASSIMILATE SURFACE LAYER DATA ON SIGMA
      IF(KOB.EQ.1.AND.IVAR.LE.4.and.nlevs_ob(n).lt.1.5) THEN

! Compute temporal weight
        timewt = get_timewt(xtime,dtmin,twindo,sfcfact,timeob(n))

        DO K=1,kte
          WTSIG(K)=0.0
        ENDDO
! DEFINE WTSIG: (FOR SRP: SPREAD SURFACE DATA THROUGH LOWEST 200 M)
!       WTSIG(1)=1.0
!       WTSIG(2)=0.67
!       WTSIG(3)=0.33
!       KOMIN=3
!       KOMAX=1
! DEFINE THE MAX AND MIN I VALUES FOR POSSIBLE NONZERO
! WEIGHTS, BASED ON THE RADIUS OF INFLUENCE, RINDX (IN GRID LENGTHS).
! fix this because kpblt at 1 and il is 0
        MAXI=IFIX(RA(N)+0.99+RINDX*sfcfacr)
        MAXI=MIN0(ide-1,MAXI)
        MINI=IFIX(RA(N)-RINDX*sfcfacr-0.99)
        MINI=MAX0(2,MINI)
!yliu start
! use also obs outside of this domain  -- surface obs
!     if(RA(N).LT.0.-RINDX .or. RA(N).GT.float(IL+RINDX) .or.
!    &   RB(N).LT.0.-RINDX .or. RB(N).GT.float(JL+RINDX)) then
!        print *, " skipped obs far away from this domain"
! currently can use obs within this domain or ones very close to (1/3
!   influence of radius in the coarse domain) this
!   domain. In later case, use BC column value to approximate the model value
!   at obs point -- ERRF need model field in errob.F !!
        if (  RA(N).GE.(0.-RINDX*sfcfacr/3)                        &
        .and. RA(N).LE.float(ide)+RINDX*sfcfacr/3                  &
        .and. RB(N).GE.(0.-RINDX*sfcfacr/3)                        &
        .and. RB(N).LE.float(jde)+RINDX*sfcfacr/3) then

! or use obs within this domain only
!     if(RA(N).LT.1 .or. RA(N).GT.float(IL) .or.
!    &   RB(N).LT.1 .or. RB(N).GT.float(JL)) then
!        print *, " skipped obs far outside of this domain"
!        if(j.eq.3 .and. ivar.eq.3) then
!           write(6,*) 'N = ',n,' exit 120 3'
!        endif
!yliu end
!
! LOOP THROUGH THE NECESSARY GRID POINTS SURROUNDING
! OBSERVATION N.  COMPUTE THE HORIZONTAL DISTANCE TO
! THE OBS AND FIND THE WEIGHTING SUM OVER ALL OBS
          RJ=FLOAT(J)
          RX=RJ-RB(N)
! WEIGHTS FOR THE 3-D VARIABLES
          ERFIVR=ERRF(IVAR,N)
          QVB_AT_OB_LOC=ERRF(10,N)

!ajb Compute and add weights to sum only if nudge_pbl switch is on.
          if(nudge_pbl) then

! Apply selected horizontal spreading scheme.
            if(SFC_SCHEME_HORIZ.eq.1) then        ! original mm5 scheme
              DO I=max0(its,MINI),min0(ite,MAXI)
                RI=FLOAT(I)
                RY=RI-RA(N)
                RIS=RINDX*RINDX*sfcfacr*sfcfacr
                RSQ=RX*RX+RY*RY
! THIS FUNCTION DECREASES WTIJ AS PSFC CHANGES WITHIN SEARCH RADIUS
                DPRIM=SQRT(RSQ)
                D=DPRIM+RINDX*DCON*ABS(psurf(n)-.001*pbase(i,1))
                DSQ=D*D
                WTIJ(i)=(RIS-DSQ)/(RIS+DSQ)
                WTIJ(i)=AMAX1(0.0,WTIJ(i))
              ENDDO
            else                                 ! wrf scheme
              DO I=max0(its,MINI),min0(ite,MAXI)
                RI=FLOAT(I)
                RY=RI-RA(N)
                RIS=RINDX*RINDX*sfcfacr*sfcfacr
                RSQ=RX*RX+RY*RY
! THIS FUNCTION DECREASES WTIJ AS PSFC CHANGES WITHIN SEARCH RADIUS
                wtij(i)=(ris-rsq)/(ris+rsq)
                scratch = (abs (psurf(n)-.001*pbase(i,1))*DCON)
                pdfac=1.-AMIN1(1.0,scratch)
                wtij(i)=wtij(i)*pdfac
                WTIJ(i)=AMAX1(0.0,WTIJ(i))
              ENDDO
            endif

! Apply selected vertical spreading scheme.
            if(SFC_SCHEME_VERT.eq.1) then         ! original simple scheme

              DO I=max0(its,MINI),min0(ite,MAXI)

! try making sfc obs weighting go thru pbl
                komax=max0(3,kpblt(i))
                IF (iprt) THEN
                  if (kpblt(i).gt.25 .and. ktau.ne.0)                         &
                                     write(6,552)inest,i,j,kpblt(i)
552               FORMAT('kpblt is gt 25, inest,i,j,kpblt=',4i4)
                ENDIF

                if(kpblt(i).gt.25) komax=3
                komin=1
                dk=float(komax)

                do k=komin,komax

                  wtsig(k)=float(komax-k+1)/dk
                  WT(I,K)=WT(I,K)+TIMEWT*WTSIG(K)*WTIJ(i)

                  !See Reen et al. poster/extended abstract (P13) from 2013 WRF
                  !User's Workshop regarding the following moisture innovation 
                  !scaling code.
                  !If dealing with moisture and user chose to scale certain
                  !negative QV innovations
                  IF((IVAR.EQ.4).AND.(obs_scl_neg_qv_innov.gt.0)) THEN
                   QVB_AT_CUR_LOC = MAX(QVB(I,K,J),0.0)
                   !If the moisture innovation is negative and the model moisture
                   ! is less at the current location where we are about to apply
                   ! the innovation than at the location where the ob was taken
                   IF((ERFIVR.LT.0).AND.(QVB_AT_CUR_LOC.LT.QVB_AT_OB_LOC)) THEN
                    !The ratio of the model moisture at the current location
                    ! compared to the ob location will be used to scale the
                    !  innovation.
                    QVB_CUR_LOC_OVER_OB_LOC = QVB_AT_CUR_LOC/QVB_AT_OB_LOC
                    IF(obs_scl_neg_qv_innov.eq.1) THEN
                     !Limit the innovation such that it cannot nudge towards a
                     !negative value
                     SCALE_FACTOR_NEG_QV_INNOV = MIN(1.0,ABS(QVB_AT_CUR_LOC/ERFIVR))
                    ELSE
                     !If the user chose a value for obs_scl_neg_qv_innov that
                     !this code is unaware of, stop WRF
                     IF (iprt) then
                      write(msg,*) 'Unknown value of obs_scl_neg_qv_innov = ',obs_scl_neg_qv_innov
                      call wrf_message(msg)
                     ENDIF
                     call wrf_error_fatal ( 'module_fddaobs_rtfdda: nudob: Unknown value of obs_scl_neg_qv_innov' )
                    ENDIF
                    !Scale the innovation
                    ERFIVR = ERFIVR*SCALE_FACTOR_NEG_QV_INNOV
                   ENDIF

                  ENDIF
                  WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ(i)*WTIJ(i)*WTSIG(K)    &
                              *WTSIG(K)*ERFIVR

                enddo
              ENDDO

            else                                ! regime-based vif scheme

! Here we calculate weights in the vertical coordinate, based on vih1 and vih2.
! In the equation for wtsig(k), Z=zslab(i,k)-terrh(i) contains the model Z-values
! (height above ground in meters) on a J-slab. The equation produces wtsig = 1.0 at
! levels 1 to K, where z(K) < vih1 < z(K+1). For the example below, in the equation
! for wtsig(k), the expression vih1(i)-Z(i,k) is strictly positive for k=1,2,3 since
! levels 1, 2, and 3 are below vih1. So xtsig(k)=min(1.0, 1.0-x) where x > 0 ==>
! wtsig(k)=1 for k=1,2,3.
!
!    For levels K+1 and up, wtsig will decrease linearly with height, with values
! along the ramp that has value 1.0 at vih1 and 0.0 at vih2. In the example:
!
!   dz_ramp  = 1/(200-150) = 1/50
!   xtsig(4) = 1 + (150-175)/50 = 1 - 1/2 = 1/2
!
!       WTSIG
!         1 -|*    *     *     *     *     *
!            |
!            |                                *
!            |
!            |                                   *
!            |
!            |                                      *
!         0 -|--|-------|-----------|------|----|----|---------|---->  Z = HT ABOVE
!              15      55          115    150  175  200       250          GROUND
!              k=1     k=2         k=3    vih1 k=4  vih2      k=5

              DO I=max0(its,MINI),min0(ite,MAXI)

                dz_ramp = 1.0 / max( 1.0, vih2(i)-vih1(i) )   ! vih2 >= vih1 by construct

           LML: do k = kts, kte
                  wtsig(k) = min( 1.0, 1.0 + ( vih1(i)-zslab(i,k)+terrh(i) ) * dz_ramp )
                  wtsig(k) = max( 0.0, wtsig(k))

                  if(wtsig(k).le.0.0) EXIT LML
                    WT(I,K)=WT(I,K)+TIMEWT*WTSIG(K)*WTIJ(i)

                   !See Reen et al. poster/extended abstract (P13) from 2013 WRF
                   !User's Workshop regarding the following moisture innovation
                   !If dealing with moisture and user chose to scale certain
                   !negative QV innovations
                   IF((IVAR.EQ.4).AND.(obs_scl_neg_qv_innov.gt.0)) THEN
                    QVB_AT_CUR_LOC = MAX(QVB(I,K,J),0.0)
                    !If the moisture innovation is negative and the model
                    !moisture
                    ! is less at the current location where we are about to apply
                    ! the innovation than at the location where the ob was taken
                    IF((ERFIVR.LT.0).AND.(QVB_AT_CUR_LOC.LT.QVB_AT_OB_LOC)) THEN
                     !The ratio of the model moisture at the current location
                     ! compared to the ob location will be used to scale the
                     !  innovation.
                     QVB_CUR_LOC_OVER_OB_LOC = QVB_AT_CUR_LOC/QVB_AT_OB_LOC
                     IF(obs_scl_neg_qv_innov.eq.1) THEN
                      !Limit the innovation such that it cannot nudge towards a
                      !negative value
                      SCALE_FACTOR_NEG_QV_INNOV = MIN(1.0,ABS(QVB_AT_CUR_LOC/ERFIVR))
                     ELSE
                      !If the user chose a value for obs_scl_neg_qv_innov that
                      !this code is unaware of, stop WRF
                      IF (iprt) then
                       write(msg,*) 'Unknown value of obs_scl_neg_qv_innov = ',obs_scl_neg_qv_innov
                       call wrf_message(msg)
                      ENDIF
                      call wrf_error_fatal ( 'module_fddaobs_rtfdda: nudob: Unknown value of obs_scl_neg_qv_innov' )
                     ENDIF
                     !Scale the innovation
                     ERFIVR = ERFIVR*SCALE_FACTOR_NEG_QV_INNOV

                    ENDIF
                   ENDIF


                   WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ(i)*WTIJ(i)*WTSIG(K)    &
                                *WTSIG(K)*ERFIVR
                  enddo LML
              ENDDO
            endif

          endif ! end if nudge-pbl switch is on

        endif   ! end check for obs in domain
! END SURFACE-LAYER OBS NUDGING

      ELSE    

! Compute temporal weight
        timewt = get_timewt(xtime,dtmin,twindo,1.,timeob(n))


! BEGIN CALCULATIONS TO SPREAD OBS INFLUENCE ALONG PRESSURE LEVELS
!
!       print *,'in upper air section'
! DEFINE THE MAX AND MIN I VALUES FOR POSSIBLE NONZERO
! WEIGHTS, BASED ON THE RADIUS OF INFLUENCE, RINDX, AND RINFAC.
! RINFAC VARIES AS A LINEAR FUNCTION FROM FROM RINFMN AT P*+PTOP
! TO RINFMX AT PFREE AND "ABOVE" (LOWER PRESSURE).
!ajb          SLOPE=(RINFMN-RINFMX)/(PSBO(N)+PTOP-PFREE)

        slope = (RINFMN-RINFMX)/(psurf(n)-PFREE)

        RINFAC=SLOPE*POB+RINFMX-SLOPE*pfree
        RINFAC=AMAX1(RINFAC,RINFMN)
        RINFAC=AMIN1(RINFAC,RINFMX)
!yliu: for multilevel upper-air data, take the maximum
!      for the I loop.
        if(nsndlev.gt.1) RINFAC = RINFMX
!yliu end

        MAXI=IFIX(RA(N)+0.99+RINDX*RINFAC)
        MAXI=MIN0(ide-IGRID,MAXI)
        MINI=IFIX(RA(N)-RINDX*RINFAC-0.99)
        MINI=MAX0(1,MINI)

! yliu start
! use also obs outside of but close to this domain  -- upr data   
!     if(   RA(N).LT.(0.-RINFAC*RINDX)
!    & .or. RA(N).GT.float(IL)+RINFAC*RINDX
!    & .or. RB(N).LT.(0.-RINFAC*RINDX)
!    & .or. RB(N).GT.float(JL)+RINFAC*RINDX)then          
!        print *, " skipped obs far away from this I-range"
! currently can use obs within this domain or ones very close to (1/3
!   influence of radius in the coarse domain) this
!   domain. In later case, use BC column value to approximate the model value
!   at obs point -- ERRF need model field in errob.F !!

!cc         if (i.eq.39 .and. j.eq.34) then
!cc            write(6,*) 'RA(N) = ',ra(n)
!cc            write(6,*) 'rinfac = ',rinfac,' rindx = ',rindx
!cc         endif
        if(   RA(N).GE.(0.-RINFAC*RINDX/3)                      &
        .and. RA(N).LE.float(ide)+RINFAC*RINDX/3                &
        .and. RB(N).GE.(0.-RINFAC*RINDX/3)                      &
        .and. RB(N).LE.float(jde)+RINFAC*RINDX/3) then
! or use obs within this domain only
!     if(RA(N).LT.1 .or. RA(N).GT.float(IL) .or.
!    &   RB(N).LT.1 .or. RB(N).GT.float(JL)) then
!        print *, " skipped obs far outside of this domain"

! yliu end
! is this 2 needed here - kpbl not used?
!          MINI=MAX0(2,MINI)

! LOOP THROUGH THE NECESSARY GRID POINTS SURROUNDING
! OBSERVATION N.  COMPUTE THE HORIZONTAL DISTANCE TO
! THE OBS AND FIND THE WEIGHTING SUM OVER ALL OBS
          RJ=FLOAT(J)
          RX=RJ-RB(N)
! WEIGHTS FOR THE 3-D VARIABLES
!
          ERFIVR=ERRF(IVAR,N)
! Model QV at the observation location
          QVB_AT_OB_LOC=ERRF(10,N)
! jc
          nsndlev=int(nlevs_ob(n)-lev_in_ob(n))+1
! yliu start
! test: do the sounding levels as individual obs
!        nsndlev=1
! yliu end
          njcsnd=nsndlev
!
          DO I=max0(its,MINI),min0(ite,MAXI)
! jc
            RI=FLOAT(I)
            RY=RI-RA(N)
            RIS=RINDX*RINFAC*RINDX*RINFAC
            RSQ=RX*RX+RY*RY
! yliu test: for upper-air data, keep D1 influence radii
            WTIJ(i)=(RIS-RSQ)/(RIS+RSQ)

            WTIJ=AMAX1(0.0,WTIJ(i))
! weight ob in vertical with +- 50 mb
! yliu: 75 hba for single upper-air, 30hba for multi-level soundings
            if(nsndlev.eq.1) then
              rinprs=7.5
!
            else
             rinprs=3.0
            endif
! yliu end

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   ---  HANDLE 1-LEVEL and MULTI-LEVEL OBSERVATIONS SEPARATELY  ---
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

            if(nsndlev.eq.1)then
!----------------------------------------------------------------------
!         ---   HANDLE 1-LEVEL OBSERVATIONS  ---
!----------------------------------------------------------------------

!         if(I.eq.MINI) print *, "  Single snd "
! ERFIVR is the residual (difference) between the ob and the model
! at that point. We can analyze that residual up and down.
! First find komin for ob.
!yliu start -- in the old code, komax and komin were reversed!
              do k=kte,1,-1
                pijk = .001*(pbase(i,k)+pp(i,k))
!               print *,'k,pijk,pob,rinprs=',k,pijk,pob,rinprs
                if(pijk.ge.(pob+rinprs)) then
                  komin=k
                  go to 325
                endif
              enddo
              komin=1
 325          continue
! now find komax for ob
              do k=3,kte
                pijk = .001*(pbase(i,k)+pp(i,k))
                if(pijk.le.(pob-rinprs)) then
                  komax=k
                  go to 326
                endif
              enddo
              komax=kte   ! yliu 20050706
 326          continue

! yliu: single-level upper-air data will act either above or below the PBL top
! ajb: Reset komin or komax. if kobs>kpbl_obs, komin=kpbl_obs+1, else komax=kpbl_obs  

              if( (kpblt(i).le.komax) .and. (kpblt(i).ge.komin) ) then
                 kobs = 1
                 OBS_K: do k = komin, komax
                     if( pob .gt. .001*(pbase(i,k)+pp(i,k)) ) then
                        kobs = k
                        EXIT OBS_K
                     endif
                 enddo OBS_K

                 if(kobs.gt.kpbl_obs(n)) then
! Obs will act only above the PBL top
                     komin=max0(kobs, komin)   ! kobs here is kpblt(i)+1
                 else                          ! Obs acts below PBL top
! Obs will act only below the PBL top
                     komax=min0(kpblt(i), komax)
                 endif
              endif
! yliu end
!
!           print *,'1 level, komin,komax=',komin,komax
!           if(i.eq.MINI) then
!             print *, "yyyyyyyyyyS IPL erfivr=", IPL, ERFIVR,J,pob
!             ERFIVR=0
!           endif
              do k=1,kte
                reserf(k)=0.0
                wtsig(k)=0.0
              enddo
!yliu end

!ajb Add weights to sum only if nudge_pbl switch is on OR obs is above pbl top.
              if(nudge_pbl .or. komin.ge.kpblt(i)) then
                do k=komin,komax
                  pijk = .001*(pbase(i,k)+pp(i,k))
                  reserf(k)=erfivr
                  wtsig(k)=1.-abs(pijk-pob)/rinprs
                  wtsig(k)=amax1(wtsig(k),0.0)
!             print *,'k,pijk,pob,rinprs,wtsig=',k,pijk,pob,rinprs,wtsig(k)
! Now calculate WT and WT2ERR for each i,j,k point                      cajb
                  WT(I,K)=WT(I,K)+TIMEWT*WTIJ(i)*wtsig(k)


                  !See Reen et al. poster/extended abstract (P13) from 2013 WRF
                  !User's Workshop regarding the following moisture innovation
                  !scaling code.
                  !If dealing with moisture and user chose to scale certain
                  !negative QV innovations
                  IF((IVAR.EQ.4).AND.(obs_scl_neg_qv_innov.GT.0)) THEN
                   QVB_AT_CUR_LOC = MAX(QVB(I,K,J),0.0)
                   !If the moisture innovation is negative and the model moisture
                   ! is less at the current location where we are about to apply
                   ! the innovation than at the location where the ob was taken
                   IF((ERFIVR.LT.0).AND.(QVB_AT_CUR_LOC.LT.QVB_AT_OB_LOC)) THEN
                    !The ratio of the model moisture at the current location
                    ! compared to the ob location will be used to scale the
                    !  innovation.
                    QVB_CUR_LOC_OVER_OB_LOC = QVB_AT_CUR_LOC/QVB_AT_OB_LOC
                    IF(obs_scl_neg_qv_innov.eq.1) THEN
                     !Limit the innovation such that it cannot nudge towards a
                     !negative value
                     SCALE_FACTOR_NEG_QV_INNOV = MIN(1.0,ABS(QVB_AT_CUR_LOC/ERFIVR))
                    ELSE
                     !If the user chose an value for obs_scl_neg_qv_innov that
                     !this code is unaware of, stop WRF
                     IF (iprt) then
                      write(msg,*) 'Unknown value of obs_scl_neg_qv_innov = ',obs_scl_neg_qv_innov
                      call wrf_message(msg)
                     ENDIF
                     call wrf_error_fatal ( 'module_fddaobs_rtfdda: nudob: Unknown value of obs_scl_neg_qv_innov' )
                    ENDIF
                    reserf(k) = reserf(k)*SCALE_FACTOR_NEG_QV_INNOV
                   ENDIF
                  ENDIF

                  WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ(i)*WTIJ(i)*        &
                              reserf(k)*wtsig(k)*wtsig(k)
                enddo
              endif

            else

!----------------------------------------------------------------------
!         ---   HANDLE MULTI-LEVEL OBSERVATIONS  ---
!----------------------------------------------------------------------
!yliu start
!           if(I.eq.MINI) print *, "  Multi-level  snd "
!             print *, "   n=,nsndlev",n,nsndlev,nlevs_ob(n),lev_in_ob(n)  &
!                  ,nlevs_ob(n+nsndlev-1),lev_in_ob(n+nsndlev-1)
              if(nlevs_ob(n+nsndlev-1).ne.lev_in_ob(n+nsndlev-1)) then
                IF (iprt) THEN
                  write(msg,*) "n = ",n,"nsndlev = ",nsndlev
                  call wrf_message(msg)
                  write(msg,*) "nlevs_ob,lev_in_ob",                          &
                           nlevs_ob(n+nsndlev-1),lev_in_ob(n+nsndlev-1)
                  call wrf_message(msg)
                  call wrf_message("in nudobs.F: sounding level messed up, stopping")
                ENDIF
                call wrf_error_fatal ( 'wrf_fddaobs_in: in4dob' )
             endif       
!yliu end
! This is for a multi-level observation
! The trick here is that the sounding is "one ob". You don't
!    want multiple levels to each be treated like separate
!    and independent observations.
! At each i,j want to interpolate sounding to the model levels at that
!    particular point.
              komin=1
              komax=kte-2
! this loop goes to 1501
! do from kte-2 to 1 so don't adjust top of model. Arbitrary.
!yliu start
              do k=1,kte
                reserf(k)=0.0
                wtsig(k)=0.0
              enddo
!yliu end

              do k=komax,komin,-1
  
                pijk = .001*(pbase(i,k)+pp(i,k))

! if sigma level pressure is .gt. than the lowest ob level, don't interpolate
                if(pijk.gt.varobs(5,n)) then
                  go to 1501
                endif

! if sigma level pressure is .lt. than the highest ob level, don't interpolate
                if(pijk.le.varobs(5,n+nsndlev-1)) then
                  go to 1501
                endif

! now interpolate sounding to this k
! yliu start-- recalculate WTij for each k-level
!ajb      SLOPE = (RINFMN-RINFMX)/(pdoc(i,j)+PTOP-PFREE)        
                slope = (RINFMN-RINFMX)/ (.001*pbase(i,1)-PFREE)
                RINFAC=SLOPE*pijk+RINFMX-SLOPE*PFREE              
                RINFAC=AMAX1(RINFAC,RINFMN)      
                RINFAC=AMIN1(RINFAC,RINFMX)
                RIS=RINDX*RINFAC*RINDX*RINFAC  
                RSQ=RX*RX+RY*RY               

! for upper-air data, keep D1 influence radii
                WTIJ(i)=(RIS-RSQ)/(RIS+RSQ)      
                WTIJ(i)=AMAX1(0.0,WTIJ(i))
! yliu end
! Set the pressure difference between the current model level and
! both the level above and below to a number larger than the maximum (BPR)
               abs_pdiff_above = maxsnd_gap+1.0
               abs_pdiff_below = maxsnd_gap+1.0

! this loop goes to 1503
                do nn=2,nsndlev

! only set pobhi if varobs(ivar) is ok
                  pobhi=-888888.

                  if(varobs(ivar,n+nn-1).gt.-800000.                           &
                  .and. varobs(5,n+nn-1).gt.-800000.) then
                    pobhi=varobs(5,n+nn-1)
                    nhi=n+nn-1
! Check if current level in the innovation is above the current
! model level but within maxsnd_gap (BPR)
                    abs_pdiff_above=abs(pobhi-pijk)
                    if(pobhi.le.pijk .and. abs_pdiff_above.le.maxsnd_gap) then
                      go to 1502        ! within maxsnd_gap of obs height
                    endif
                  endif

                enddo

! OLD: did not find any ob above within maxsnd_gap/2, so jump out
! NEW: did not find any ob above within maxsnd_gap, so jump out

                go to 1501
 1502           continue

                nlo=nhi-1
                do nnjc=nhi-1,n,-1
                  if(varobs(ivar,nnjc).gt.-800000.                             &
                  .and. varobs(5,nnjc).gt.-800000.) then
                    poblo=varobs(5,nnjc)
                    nlo=nnjc
! Check if current level in the innovation is below the current
! model level but within maxsnd_gap (BPR)
                    abs_pdiff_below=abs(poblo-pijk)
                    if(poblo.ge.pijk .and. abs_pdiff_below.le.maxsnd_gap) then
                      go to 1505        ! within maxsnd_gap of obs height
                    endif
                  endif
                enddo
!yliu end --

! OLD: did not find any ob below within maxsnd_gap/2, so jump out
! NEW: did not find any ob below within maxsnd_gap, so jump out

                go to 1501
 1505           continue
!BPR BEGIN
! Ensure that sum of gap between model level and the closest
! innovation level above that combined with the sum of the gap
! between the model level and the closest innovation level below
! that is less than or equal to the maximum allowed gap
                if((abs_pdiff_below+abs_pdiff_above).gt.maxsnd_gap) then
                 goto 1501
                endif

! interpolate to model level
! Avoid potential division by zero (or near-zero) that may now
! occur when the model pressure exactly matches the pressure of
! one of the levels in the innovation profile.
! Note that these variables are in terms of cb so we are assuming that
! if the closest innovation level below the model level is within
! 0.00001 hPa of the the closest innovation level above the model
! level then those two levels are identical. (BPR)
                IF(abs(pobhi-poblo).lt.0.000001) THEN
                 pdiffj=0
                ELSE
                 !Original code just used following statement
                 pdiffj=alog(pijk/poblo)/alog(pobhi/poblo)
                ENDIF

                reserf(k)=errf(ivar,nlo)+                               &
                            (errf(ivar,nhi)-errf(ivar,nlo))*pdiffj

                !See Reen et al. poster/extended abstract (P13) from 2013 WRF
                !User's Workshop regarding the following moisture innovation
                !scaling code.
                !If dealing with moisture and user chose to scale certain
                !negative QV innovations
                IF((IVAR.EQ.4).AND.(obs_scl_neg_qv_innov.GT.0)) THEN
                 QVB_AT_CUR_LOC = QVB(I,K,J)
                 !Vertically intepolate observed moisture
                 QVB_AT_OB_LOC=errf(10,nlo)+                               &
                              (errf(10,nhi)-errf(10,nlo))*pdiffj
                 !If the moisture innovation is negative and the model
                 !moisture
                 ! is less at the current location where we are about to apply
                 ! the innovation than at the location where the ob was taken
                 IF((reserf(k).LT.0).AND.(QVB_AT_CUR_LOC.LT.QVB_AT_OB_LOC)) THEN
                  !The ratio of the model moisture at the current location
                  ! compared to the ob location will be used to scale the
                  !  innovation.
                  QVB_CUR_LOC_OVER_OB_LOC = QVB_AT_CUR_LOC/QVB_AT_OB_LOC
                  IF(obs_scl_neg_qv_innov.eq.1) THEN
                   !Limit the innovation such that it cannot nudge towards a
                   !negative value
                   SCALE_FACTOR_NEG_QV_INNOV = MIN(1.0,ABS(QVB_AT_CUR_LOC/reserf(k)))
                  ELSE
                   !If the user chose a value for obs_scl_neg_qv_innov that
                   !this code is unaware of, stop WRF
                   IF (iprt) then
                    write(msg,*) 'Unknown value of obs_scl_neg_qv_innov = ',obs_scl_neg_qv_innov
                    call wrf_message(msg)
                   ENDIF
                   call wrf_error_fatal ( 'module_fddaobs_rtfdda: nudob: Unknown value of obs_scl_neg_qv_innov' )
                  ENDIF
                  reserf(k) = reserf(k)*SCALE_FACTOR_NEG_QV_INNOV
                 ENDIF

                ENDIF

                wtsig(k)=1.
  
 1501           continue

! now calculate WT and WT2ERR for each i,j,k point                               cajb
!ajb Add weights to sum only if nudge_pbl switch is on OR k > kpblt.
                if(nudge_pbl .or. k.gt.kpblt(i)) then

                  WT(I,K)=WT(I,K)+TIMEWT*WTIJ(i)*wtsig(k)
  
                  WT2ERR(I,K)=WT2ERR(I,K)+TIMEWT*TIMEWT*WTIJ(i)*WTIJ(i)*        &
                              reserf(k)*wtsig(k)*wtsig(k)
                endif

              enddo   ! enddo k levels

! end multi-levels
            endif  ! end if(nsndlev.eq.1)
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!   END 1-LEVEL AND MULTI-LEVEL OBSERVATIONS
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
          ENDDO ! END DO MINI,MAXI LOOP

        endif ! check for obs in domain

! END OF NUDGING TO OBS ON PRESSURE LEVELS

      ENDIF !end IF(KOB.EQ.1.AND.IVAR.LE.4.and.nlevs_ob(n).lt.1.5)

!----------------------------------------------------------------------
    ENDIF  ! END SECTION FOR PROCESSING OF OBSERVATION
!----------------------------------------------------------------------

!   n=n+1
    n=n+njcsnd

!yliu  1202 continue
    if(n.gt.nstat)then
!     print *,'n,nstat=',n,nstat,ivar,j
      go to 1203
    endif
!   print *, "e-- n=,nsndlev",n,njcsnd,nlevs_ob(n),lev_in_ob(n)

!***********************************************************************
  ENDDO  ! END OUTER LOOP FOR THE NSTAT OBSERVATIONS
!***********************************************************************

 1203 continue

! WEIGHTS AND WEIGHTED DIFFERENCES HAVE BEEN SUMMED.  NOW
! APPLY THE NUDGING FACTOR AND THE RESULTANT TENDENCY TO
! THE ATEN ARRAY
! ASSURE THAT WT(I,K) AND WTP(I,K) ARE NONZERO SINCE
! THEY ARE USED BELOW IN THE DENOMINATOR.
  DO K=kts,kte
    DO I=its,ite
      IF(WT(I,K).EQ.0)THEN
        WT2ERR(I,K)=0.0
      ENDIF
      IF(WT(I,K).EQ.0)THEN
        WT(I,K)=1.0
      ENDIF
    ENDDO
  ENDDO

126 CONTINUE

  IF(IVAR.GE.3)GOTO 170
! this is for u,v
! 3-D DOT POINT TENDENCIES

! Calculate scales for converting nudge factor from u (v)
! to rho_u (or rho_v) units.

  IF (IVAR == 1) THEN
     call calc_rcouple_scales(mu,msfy,rscale,ims,ime,its,ite)
  ELSE IF (IVAR == 2) THEN
     call calc_rcouple_scales(mu,msfx,rscale,ims,ime,its,ite)
  END IF

  DO K=1,kte

    DO I=i_s,i_e

      IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN
        W2EOWT=WT2ERR(I,K)/WT(I,K)
      ELSE
        W2EOWT=SAVWT(IPL,I,K)

!        write(6,'(a,4i4,f8.3)') 'i,j,k,ipl,W2EOWT = ',i,j,k,ipl,W2EOWT

      ENDIF

!      if(ivar .eq. 1 .and. i.eq.38 .and. j.eq.78 .and. k.eq.1) then
!           scratch = GIV*RSCALE(I)*W2EOWT*TFACI*ISWIND*GFACTOR
!           write(6,*) 'ATEN calc: k = ',k
!           write(6,*) 'U before: aten = ',aten(i,k),' scr = ',scratch
!           write(6,*) 'GIV = ',giv,' rscale = ',rscale(i),
!     $               ' W2EOWT = ',w2eowt
!           write(6,*) 'TFACI = ',tfaci,' ISWIND = ',iswind,
!     $               ' GFACTOR = ',gfactor
!       endif
!
!      if(ivar .eq. 2 .and. i.eq.39 .and. j.eq.29) then
!           scratch = GIV*RSCALE(I)*W2EOWT*TFACI*ISWIND*GFACTOR
!           write(6,*) 'ATEN calc: k = ',k
!           write(6,*) 'V before: aten = ',aten(i,k),' scr = ',scratch
!           write(6,*) 'GIV = ',giv,' rscale = ',rscale(i),
!     $                ' W2EOWT = ',w2eowt
!           write(6,*) 'TFACI = ',tfaci,' ISWIND = ',iswind,
!     $                ' GFACTOR = ',gfactor
!       endif

!      if(ivar .eq. 1 .and. i.eq.38 .and. (j.ge.36.and.j.le.62) .and. k.eq.7) then
!           scratch = GIV*RSCALE(I)*W2EOWT*TFACI*ISWIND*GFACTOR
!           write(6,*)
!           write(6,*) 'ATEN calc: j = ',j,' k = ',k
!           write(6,*) 'U before: aten = ',aten(i,k),' scr = ',scratch
!           write(6,*) 'GIV = ',giv,' rscale = ',rscale(i),' W2EOWT = ',w2eowt
!           write(6,*) 'TFACI = ',tfaci,' ISWIND = ',iswind,' GFACTOR = ',gfactor
!       endif

        ATEN(i,k)=ATEN(i,k)+GIV*RSCALE(I)                        &
                    *W2EOWT*TFACI                           &
                    *ISWIND       *GFACTOR   !yliu *GFACTOR

!      if(ivar .eq. 1 .and. i.eq.38 .and. j.eq.78 .and. k.eq.1) then
!           write(6,*) 'U after: aten = ',aten(i,k),' scr = ',scratch
!      endif
!      if(ivar .eq. 2 .and. i.eq.39 .and. j.eq.29) then
!           write(6,*) 'V after: aten = ',aten(i,k),' scr = ',scratch
!      endif

    ENDDO
  ENDDO

  IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN
    DO K=1,kte
      DO I=its,ite
        SAVWT(IPL,I,K)=WT2ERR(I,K)/WT(I,K)

!        write(6,'(a,4i4,f8.3)') 'i,j,k,ipl,savwt = ',i,j,k,ipl,savwt(ipl,i,k)
      ENDDO
    ENDDO
  ENDIF

  RETURN

170 CONTINUE

! 3-D CROSS-POINT TENDENCIES
! this is for t (ivar=3) and q (ivsr=4)
  IF(3-IVAR.LT.0)THEN
    GITQ=GIQ
  ELSE
    GITQ=GIT
  ENDIF
  IF(3-IVAR.LT.0)THEN
    ISTQ=ISMOIS
  ELSE
    ISTQ=ISTEMP
  ENDIF

  DO K=1,kte
    DO I=i_s,i_e
      IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR))THEN
        W2EOWT=WT2ERR(I,K)/WT(I,K)
      ELSE
        W2EOWT=SAVWT(IPL,I,K)
      ENDIF

!        if(ivar .eq. 3 .and. i.eq.39 .and. j.eq.49) then
!            scratch = GITQ*(c1h(k)*MU(I)+c2h(k))*W2EOWT*TFACI*ISTQ*GFACTOR
!            write(6,*) 'ATEN calc: k = ',k
!            write(6,*) 'T before: aten = ',aten(i,k),' scr = ',scratch
!            write(6,*) 'GITQ = ',gitq,' MU = ',(c1h(k)*MU(i)+c2h(k)),                  &
!                       ' W2EOWT = ',w2eowt
!            write(6,*) ' TFACI = ',tfaci,' ISTQ = ',istq,              &
!                       ' GFACTOR = ',gfactor
!        endif

!        if(ivar .eq. 4 .and. i.eq.39 .and. j.eq.29) then
!            scratch = GITQ*(c1h(k)*MU(I)+c2h(k))*W2EOWT*TFACI*ISTQ*GFACTOR
!            write(6,*) 'ATEN calc: k = ',k
!            write(6,*) 'Q before: aten = ',aten(i,k),' scr = ',scratch
!            write(6,*) 'GITQ = ',gitq,' MU = ',(c1h(k)*MU(i)+c2h(k)),
!     $                 ' W2EOWT = ',w2eowt
!            write(6,*) ' TFACI = ',tfaci,' ISTQ = ',istq,
!     $                 ' GFACTOR = ',gfactor
!        endif

      ATEN(i,k)=ATEN(i,k)+GITQ*(c1h(k)*MU(I)+c2h(k))                       &
                  *W2EOWT*TFACI*ISTQ       *GFACTOR   !yliu *GFACTOR

!        if(ivar .eq. 3 .and. i.eq.39 .and. j.eq.49) then
!            write(6,*) 'T after: aten = ',aten(i,k),' scr = ',scratch
!        endif
!        if(ivar .eq. 4 .and. i.eq.39 .and. j.eq.29) then
!            write(6,*) 'Q after: aten = ',aten(i,k),' scr = ',scratch
!        endif

    ENDDO
  ENDDO

  IF(MOD(KTAU,INFR).EQ.0.OR.(IFREST.AND.KTAU.EQ.KTAUR)) THEN
    DO K=1,kte
      DO I=its,ite
        SAVWT(IPL,I,K)=WT2ERR(I,K)/WT(I,K)
      ENDDO
    ENDDO
  ENDIF

  RETURN
  END SUBROUTINE nudob

  SUBROUTINE date_string(year, month, day, hour, minute, second, cdate)
!-----------------------------------------------------------------------
!  PURPOSE: Form a date string (YYYY-MM-DD_hh:mm:ss) from integer
!           components.
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: year
  INTEGER, INTENT(IN)  :: month
  INTEGER, INTENT(IN)  :: day
  INTEGER, INTENT(IN)  :: hour
  INTEGER, INTENT(IN)  :: minute
  INTEGER, INTENT(IN)  :: second
  CHARACTER*19, INTENT(INOUT) :: cdate

! Local variables
  integer   :: ic                    ! loop counter

      cdate(1:19)  = "0000-00-00_00:00:00"
      write(cdate( 1: 4),'(i4)') year
      write(cdate( 6: 7),'(i2)') month
      write(cdate( 9:10),'(i2)') day
      write(cdate(12:13),'(i2)') hour
      write(cdate(15:16),'(i2)') minute
      write(cdate(18:19),'(i2)') second
      do ic = 1,19
        if(cdate(ic:ic) .eq. " ") cdate(ic:ic) = "0"
      enddo

  RETURN
  END SUBROUTINE date_string

  SUBROUTINE calc_rcouple_scales(a, msf, rscale, ims,ime, its,ite)
!-----------------------------------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------------------------------

  INTEGER, INTENT(IN)  :: ims,ime           ! Memory dimensions
  INTEGER, INTENT(IN)  :: its,ite           ! Tile   dimensions
  REAL, INTENT(IN)     :: a( ims:ime )      ! Air mass array
  REAL, INTENT(IN)     :: msf( ims:ime )    ! Map scale factor array
  REAL, INTENT(OUT)    :: rscale( ims:ime ) ! Scales for rho-coupling

! Local variables
  integer :: i

! Calculate scales to be used for producing rho-coupled nudging factors.
  do i = its,ite
    rscale(i) = a(i)/msf(i)
  enddo

  RETURN
  END SUBROUTINE calc_rcouple_scales

  SUBROUTINE print_obs_info(iprt,inest,niobf,rio,rjo,rko,                &
                            prt_max,prt_freq,obs,stnid,lat,lon,          &
                            mlat,mlon,timeob,xtime)
!*************************************************************************
! Purpose: Print obs information.
!*************************************************************************

  IMPLICIT NONE

  LOGICAL, intent(in)    :: iprt          ! Print flag
  INTEGER, intent(in)    :: inest         ! Nest level
  INTEGER, intent(in)    :: niobf         ! Maximum number of observations
  REAL,    intent(in)    :: rio(niobf)    ! West-east coord (non-stagger)
  REAL,    intent(in)    :: rjo(niobf)    ! South-north coord (non-stagger)
  REAL,    intent(in)    :: rko(niobf)    ! Bottom-top north coord (non-stagger)
  INTEGER, intent(in)    :: prt_max        ! Max no. of obs for diagnostic printout
  INTEGER, intent(in)    :: prt_freq       ! Frequency for diagnostic printout
  INTEGER, intent(in)    :: obs(prt_max)  ! Saved obs indices to print
  INTEGER, intent(in)    :: stnid(40,prt_max) ! Saved station ids
  REAL,    intent(in)    :: lat(prt_max)  ! Saved latitudes
  REAL,    intent(in)    :: lon(prt_max)  ! Saved longitudes
  REAL,    intent(in)    :: mlat(prt_max) ! Saved model latitudes
  REAL,    intent(in)    :: mlon(prt_max) ! Saved longitudes
  REAL,    intent(in)    :: timeob(niobf) ! Times of each observation (hours)
  REAL,    intent(in)    :: xtime         ! Model time in minutes

! Local variables
  integer :: i                    ! Loop counter over obs station chars
  integer :: n                    ! Loop counter over obs
  integer :: pnx                  ! Obs index for printout
  character(len=200) :: msg       ! Argument to wrf_message
  character(len=20)  :: station_id ! Station id of observation

  if(iprt) then
    if(prt_max.gt.0) then

      ! If ANY of the obs are NOT missing, then execute these prints (which 
      ! include the headers for the rows of data to be printed in the next 
      ! do loop)
      if(ANY(obs.ne.-999)) then

        call wrf_message("")
        write(msg,fmt='(a,i4,a,f8.1,a)') 'REPORTING OBS MASS-PT LOCS FOR NEST ',  &
                                     inest,' AT XTIME=',xtime,' MINUTES'
        call wrf_message(msg)

        write(msg,fmt='(a,i4,a,i5,a)') 'FREQ=',prt_freq,', MAX=',prt_max,         &
                           ' LOCS, NEWLY READ OBS ONLY, -999 => OBS OFF PROC'
        call wrf_message(msg)
        call wrf_message("")

        write(msg,fmt='(3a)') '    OBS#     I       J       K     OBS LAT',       &
                          '  OBS LON   XLAT(I,J)  XLONG(I,J)  TIME(hrs)',     &
                          '  OBS STATION ID'
        call wrf_message(msg)

      endif
    endif

! Note: rio and rjo are referenced to non-staggered grid (not mass-point!)
!       Hence subtract .5 from each to get mass-point coords.
    do n=1,prt_max
       pnx = obs(n)
       if(pnx.ne.-999) then
!          Retrieve 15 chars of station id
           do i = 1,15
              station_id(i:i) = char(stnid(i,n))
           enddo
           write(msg,fmt='(2x,i7,3f8.3,2f9.3,2x,f9.3,2x,f9.3,3x,f6.2,7x,a15)')    &
               pnx,rio(pnx)-.5,rjo(pnx)-.5,rko(pnx),lat(n),lon(n),            &
               mlat(n),mlon(n),timeob(pnx),station_id
        call wrf_message(msg)
       endif
    enddo
    if(obs(1).ne.-999) call wrf_message("")
  endif
  END SUBROUTINE print_obs_info

  REAL FUNCTION ht_to_p( h, pbbc, ppbc, z, ic, jc, dx, dy,                    &
                         k_start, k_end, kds,kde, ims,ime, jms,jme, kms,kme )

!******************************************************************************
! Purpose: Interpolate pressure at a specified x (ic), y (jc), and height (h).
!          The input pressure column pbbc+ppbc (base and perturbn) must already
!          be horizontally interpolated to the x, y position. The subroutine
!          get_height_column is called here to horizontally interpolated the
!          3D height field z to get a height column at (iob, job).
!******************************************************************************

  IMPLICIT NONE

  REAL,    INTENT(IN)  :: h                                ! height value (m)
  INTEGER, INTENT(IN)  :: k_start, k_end                   ! loop bounds  
  INTEGER, INTENT(IN)  :: kds,kde                          ! vertical dim.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme        ! memory dims.
  REAL,    INTENT(IN)  :: pbbc(kds:kde)                    ! column base pressure (cb)
  REAL,    INTENT(IN)  :: ppbc(kds:kde)                    ! column pressure perturbation (cb)
  REAL,    INTENT(IN)  :: z( ims:ime, kms:kme, jms:jme )   ! ht (m) above sl on half-levels
  INTEGER, INTENT(IN)  :: ic                               ! i-coord of desired p
  INTEGER, INTENT(IN)  :: jc                               ! j-coord of desired p
  REAL,    INTENT(IN)  :: dx                               ! interp. fraction (x dir)
  REAL,    INTENT(IN)  :: dy                               ! interp. fraction (y dir)

! Local variables
  INTEGER :: k               ! loop counter
  INTEGER :: klo             ! lower k bound
  REAL :: zlo                ! lower z bound for h
  REAL :: zhi                ! upper z bound for h
  REAL :: p                  ! interpolated pressure value
  REAL :: ln_p               ! log p
  REAL :: ln_plo             ! log plo
  REAL :: ln_phi             ! log phi
  REAL :: z_at_p( kms:kme )  ! height at p levels

! Get interpolated z column on pressure (half-) levels at (ic,jc)
  call get_height_column(z, ic, jc, dx, dy, z_at_p,                   &
                         k_start, k_end, kds,kde,                     &
                         ims,ime, jms,jme, kms,kme )

! Now we have pbbc, ppbc, z_at_p, so compute p at h. First, find
! bounding layers klo and khi so that z_at_p(klo) <= h <= z_at_p(khi)

  ZLEVS: do k = k_start+1, k_end
    klo = k-1
    if(h .le. z_at_p(k)) then
      EXIT ZLEVS
    endif
  enddo ZLEVS

  zlo = z_at_p(klo)
  zhi = z_at_p(klo+1)

! Interpolate natural log of pressure
  ln_plo = log( pbbc(klo+1) + ppbc(klo+1) )
  ln_phi = log( pbbc(klo) + ppbc(klo) )
  if(h.le.zlo) then
    ln_p = ln_phi     ! set to k=1 pressure
  else if (h.ge.zhi) then
    ln_p = ln_plo     ! set to k=k_end pressure
  else
    ln_p = ln_plo + (ln_phi-ln_plo)*((zhi-h)/(zhi-zlo))
  endif

! Return pressure
  p = exp(ln_p)
  ht_to_p = p
  RETURN
  END FUNCTION ht_to_p

  SUBROUTINE get_height_column( z, ic, jc, dx, dy, z_at_p,                  &
                                k_start, k_end, kds,kde,                    &
                                ims,ime, jms,jme, kms,kme )
!*************************************************************************
! Purpose: Compute column height at ic, jc location on p levels
!*************************************************************************

  IMPLICIT NONE

  INTEGER, INTENT(IN)  :: k_start, k_end                   ! Loop bounds  
  INTEGER, INTENT(IN)  :: kds,kde                          ! vertical dim.
  INTEGER, INTENT(IN)  :: ims,ime, jms,jme, kms,kme        ! memory dims.
  REAL,    INTENT(IN)  :: z( ims:ime, kms:kme, jms:jme )   ! ht (m) on half-levels
  INTEGER, INTENT(IN)  :: ic                               ! i-coord of desired p
  INTEGER, INTENT(IN)  :: jc                               ! j-coord of desired p
  REAL,    INTENT(IN)  :: dx                               ! interp. fraction (x dir)
  REAL,    INTENT(IN)  :: dy                               ! interp. fraction (y dir)
  REAL,    INTENT(OUT) :: z_at_p( kms:kme )                ! column height at p levels

! Local variables
  INTEGER :: k             ! loop counter


  do k = kds, kde
      z_at_p(k) =                                     &
         (1.-DY)*( (1.-DX)*z(IC,K,JC) +               &
                            DX *z(IC+1,K,JC) ) +      &
             DY* ( (1.-DX)*z(IC,K,JC+1) +             &
                            DX *z(IC+1,K,JC+1) )
  enddo

  END SUBROUTINE get_height_column

  SUBROUTINE get_base_state_height_column( p_top, p00, t00, a, g, r_d,    &
                               znu, z_at_p,  k_start, k_end, kds,kde, kms,kme )
!********************************************************
! Purpose: Compute base-state column height on p levels.
!    See, e.g., eqns 1.3-1.5 in MM5 User's Guide.
!    Height is a function of pressure plus the constants:
!    p00  - sea level pressure (Pa)
!    t00  - sea level temperature (K)
!    a    - base state lapse rate (k/m)
!    r_d  - gas constant (J/Kg/K)   (Joule=1kg/m**2/s**2)
!    g    - gravitational constant (m/s**2)
!********************************************************

  IMPLICIT NONE

  REAL, INTENT(IN)     :: p_top        ! pressure at top of model
  REAL, INTENT(IN)     :: p00          ! base state pressure
  REAL, INTENT(IN)     :: t00          ! base state temperature
  REAL, INTENT(IN)     :: a            ! base state lapse rate
  REAL, INTENT(IN)     :: g                ! gravity constant
  REAL, INTENT(IN)     :: r_d              ! gas constant
  INTEGER, INTENT(IN)  :: k_start, k_end   ! Loop bounds  
  INTEGER, INTENT(IN)  :: kds,kde          ! vertical dim.
  INTEGER, INTENT(IN)  :: kms,kme          ! vertical memory dim.
  REAL, INTENT(IN)  :: znu( kms:kme )      ! eta values on half (mass) levels
  REAL, INTENT(OUT) :: z_at_p( kms:kme )   ! column height at p levels

! Local variables
  integer :: k             ! loop counter
  real    :: ps0           ! base state pressure at surface
  real    :: pb(kms:kme)   ! pressure on half eta levels
  real    :: logterm       ! temporary for Z calculation
  real    :: ginv          ! 1/g
  
  ginv = 1/g

! Compute base state pressure on half eta levels.
   do k = k_start, k_end
     pb(k) = znu(k)*(p00 - p_top) + p_top
   enddo

! Use hydrostatic relation to compute height at pressure levels.
   do k = k_start, k_end
     logterm = log(pb(k)/p00)
     z_at_p(k) = .5*r_d*a*ginv*logterm*logterm - r_d*t00*ginv*logterm
   enddo

  END SUBROUTINE get_base_state_height_column

  REAL FUNCTION get_timewt(xtime,dtmin,twindo,scalef,obtime)
!*************************************************************************
! Purpose: Compute the temporal weight factor for an observation
!*************************************************************************

  IMPLICIT NONE

  REAL, INTENT(IN)  :: xtime              ! model time (minutes)
  REAL, INTENT(IN)  :: dtmin              ! model timestep (minutes)
  REAL, INTENT(IN)  :: twindo             ! half window (hours)
  REAL, INTENT(IN)  :: scalef             ! window scale factor
  REAL, INTENT(IN)  :: obtime             ! observation time (hours)

! Local variables
  real :: fdtim            ! reference time (minutes)
  real :: tw1              ! half of twindo, scaled, in minutes
  real :: tw2              ! twindo, scaled, in minutes
  real :: tconst           ! reciprical of tw1
  real :: ttim             ! obtime in minutes
  real :: dift             ! | fdtim-ttim |
  real :: timewt           ! returned weight

! DETERMINE THE TIME-WEIGHT FACTOR FOR N
  FDTIM=XTIME-DTMIN
! TWINDO IS IN MINUTES:
  TW1=TWINDO/2.*60.*scalef
  TW2=TWINDO*60.*scalef
  TCONST=1./TW1
  TIMEWT=0.0
  TTIM=obtime*60.
!***********TTIM=TARGET TIME IN MINUTES
  DIFT=ABS(FDTIM-TTIM)
  IF(DIFT.LE.TW1)TIMEWT=1.0
  IF(DIFT.GT.TW1.AND.DIFT.LE.TW2) THEN
     IF(FDTIM.LT.TTIM)TIMEWT=(FDTIM-(TTIM-TW2))*TCONST
     IF(FDTIM.GT.TTIM)TIMEWT=((TTIM+TW2)-FDTIM)*TCONST
  ENDIF
  get_timewt = timewt
  END FUNCTION get_timewt

  SUBROUTINE print_vif_var(var, vif, nfullmin, nrampmin )
!********************************************************
! Purpose: Print a description of the vertical influence
!          function for a given variable.
!********************************************************
  IMPLICIT NONE

  character(len=4), intent(in)  :: var      ! Variable (wind, temp, mois)
  real,             intent(in)  :: vif(6)   ! Vertical influence function
  real,             intent(in)  :: nfullmin ! Vert infl fcn full nudge min
  real,             intent(in)  :: nrampmin ! Vert infl fcn ramp decr min

! Local variables
  character(len=200) :: msg1, msg2
  character(len=8) :: regime
  real :: nfullr1, nrampr1
  real :: nfullr2, nrampr2
  real :: nfullr4, nrampr4

  nfullr1 = vif(1)
  nrampr1 = vif(2)
  nfullr2 = vif(3)
  nrampr2 = vif(4)
  nfullr4 = vif(5)
  nrampr4 = vif(6)

  if(var.eq.'wind') then
    write(msg1,fmt='(a)') '  For winds:'
  elseif (var.eq.'temp') then
    write(msg1,fmt='(a)') '  For temperature:'
  elseif (var.eq.'mois') then
    write(msg1,fmt='(a)') '  For moisture:'
  else
    write(msg1,fmt='(a,a4)') 'Unknown variable type: ',var
    call wrf_message(msg1)
    call wrf_error_fatal ( 'print_vif_var: module_fddaobs_rtfdda STOP' )
  endif
      
  call wrf_message(msg1)

! For this variable, print a description of the vif for each regime
  call print_vif_regime(1, nfullr1, nrampr1, nfullmin, nrampmin)
  call print_vif_regime(2, nfullr2, nrampr2, nfullmin, nrampmin)
  call print_vif_regime(4, nfullr4, nrampr4, nfullmin, nrampmin)

  END SUBROUTINE print_vif_var

  SUBROUTINE print_vif_regime(reg, nfullr, nrampr, nfullmin, nrampmin )
!********************************************************
! Purpose: Print a description of the vertical influence
!          function for a given regime.
!********************************************************
  IMPLICIT NONE

  integer, intent(in)  :: reg          ! Regime number (1, 2, 4)
  real,    intent(in)  :: nfullr       ! Full nudge range for regime
  real,    intent(in)  :: nrampr       ! Rampdown range for regime
  real,    intent(in)  :: nfullmin     ! Vert infl fcn full nudge min
  real,    intent(in)  :: nrampmin     ! Vert infl fcn ramp decr min

! Local variables
  character(len=200) :: msg1, msg2
  character(len=8) :: regime

  if(reg.eq.1) then
     write(regime,fmt='(a)') 'Regime 1'
  elseif (reg.eq.2) then
     write(regime,fmt='(a)') 'Regime 2'
  elseif (reg.eq.4) then
     write(regime,fmt='(a)') 'Regime 4'
  else
     write(msg1,fmt='(a,i3)') 'Unknown regime number: ',reg
     call wrf_message(msg1)
     call wrf_error_fatal ( 'print_vif_regime: module_fddaobs_rtfdda STOP' )
  endif

!Set msg1 for description of full weighting range
  if(nfullr.lt.0) then
     if(nfullr.eq.-5000) then
       write(msg1,fmt='(2x,a8,a)') regime, ': Full weighting to the PBL top'
     elseif (nfullr.lt.-5000) then
       write(msg1,fmt='(2x,a8,a,i4,a)') regime, ': Full weighting to ',int(-5000.-nfullr), &
                                          ' m above the PBL top'
     else
       write(msg1,fmt='(2x,a8,a,i4,a)') regime, ': Full weighting to ',int(nfullr+5000.),  &
                                          ' m below the PBL top'
     endif
  else
     write(msg1,fmt='(2x,a8,a,i4,a)') regime, ': Full weighting through ',                 &
                                     int(max(nfullr,nfullmin)),' m'
  endif

!Set msg2 for description of rampdown range
  if(nrampr.lt.0) then
     if(nrampr.eq.-5000) then
       write(msg2,fmt='(a)') ' and a vertical rampdown up to the PBL top.'
     elseif (nrampr.lt.-5000) then
       write(msg2,fmt='(a,i4,a)') ' and a vertical rampdown to ',int(-5000.-nrampr),    &
                            ' m above the PBL top.'
     else
       write(msg2,fmt='(a,i4,a)') ' and a vertical rampdown to ',int(nrampr+5000.),     &
                            ' m below the PBL top.'
     endif
  else
     write(msg2,fmt='(a,i4,a)') ' and a vertical rampdown in the next ',                &
                          int(max(nrampr,nrampmin)),' m.'
  endif
  call wrf_message(TRIM(msg1)//msg2)

  END SUBROUTINE print_vif_regime
#endif

END MODULE module_fddaobs_rtfdda

