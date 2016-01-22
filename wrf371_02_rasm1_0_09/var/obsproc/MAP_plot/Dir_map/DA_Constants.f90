 module DA_Constants

!-----------------------------------------------------------------------------
! PURPOSE: Common reference point for constants.
!
! METHOD:  Straightforward definitions.
!
! HISTORY: 11/08/1999 - Creation.                           Dale Barker
!------------------------------------------------------------------------------

 implicit none

!------------------------------------------------------------------------------
! [1.0] Parameter constants:
!------------------------------------------------------------------------------

 real, parameter   :: pi = 3.1415926535897932346
 real, parameter   :: gas_constant = 287.04 ! Value used in MM5.
 real, parameter   :: cp = 1004.0           ! Value used in MM5.
 real, parameter   :: kappa = gas_constant / cp
 real, parameter   :: gravity = 9.81        ! m/s - value used in MM5.
 REAL, PARAMETER   :: earth_radius = 6378.15

!       real, parameter:: CONV=57.29578, A=6370.
 integer, parameter :: jpin = 41            ! Unit number for xb input.
 integer, parameter :: jpout = 11           ! Unit number for x output.
 integer, parameter :: be_unit = 410        ! Unit number for be input.

! MM5 constants:

 INTEGER            :: map_projection       ! 1=LamConf/2=PolarSte/3=Mercator
 INTEGER            :: kix                  ! COARSE DOMAIN DIM IN I DIRECTION.
 INTEGER            :: kjx                  ! COARSE DOMAIN DIM IN Y DIRECTION.

 REAL               :: dsc                  ! Coarse domain gridlength (km)
! REAL               :: phic                 ! COARSE DOMAIN CENTRAL LAT(DEGREE)
! REAL               :: xlonc                ! COARSE DOMAIN CENTRAL LON(DEGREE)
 REAL               :: cone_factor          ! Cone Factor
! REAL               :: truelat1             ! True latitude 1 (degrees)
! REAL               :: truelat2             ! True latitude 2 (degrees)
! REAL               :: pole                 ! Pole latitude (degrees)
 REAL               :: dsm                  ! Current domain gridlength (km)
 REAL               :: xim11                ! i posn. of (1,1) in coarse domain.
 REAL               :: xjm11                ! j posn. of (1,1) in coarse domain.
! REAL               :: psi1                 ! ?
! REAL               :: c2                   ! earth_radius * COS(psi1)
! REAL               :: xcntr                ! ?
! REAL               :: ycntr                ! ?
 REAL               :: xxj                  ! ?
 REAL               :: yyi                  ! ?

      REAL                               :: ptop
      REAL                               :: ps0
      REAL                               :: ts0
      REAL                               :: tlp
      INTEGER                            :: m_ix
      INTEGER                            :: m_jx
      INTEGER                            :: m_kx
      REAL, allocatable, dimension(:)    :: sigmah, sigmaf, dsigma

 integer, parameter :: max_num_of_var = 20  ! Maximum number of stored MM5 variables.

 real, parameter    :: pplow = 1.0e-8       ! Machine lowest number?
 real, parameter    :: pp_umin = 1.0e-2     ! Minimum u back. error (m/s).
 real, parameter    :: pp_vmin = 1.0e-2     ! Minimum v back. error (m/s).
 real, parameter    :: pp_tmin = 1.0e-2     ! Minimum t back. error (K).
 real, parameter    :: pp_qmin = 1.0e-6     ! Minimum q back. error (kg/kg)
 real, parameter    :: pp_pmin= 1.0e+1      ! Minimum pp back. error (Pa).

 real, parameter    :: std_scaling = 0.9    ! Empirical beck. error scaling factor.

 CHARACTER*8, PARAMETER :: cpscale = 'SPECTRUM' ! Scaling for B (DIRAC OR SPECTRUM)

!Adjoint tests:

 real, parameter    :: typical_u_rms = 2.0    ! m/s
 real, parameter    :: typical_v_rms = 2.0    ! m/s
 real, parameter    :: typical_t_rms = 1.0    ! K
 real, parameter    :: typical_p_rms = 100.0  ! Pa
 real, parameter    :: typical_q_rms = 0.0005 ! g/kg
 real, parameter    :: typical_rho_rms = 100.0 ! kg/m^3
 real, parameter    :: typical_tpw_rms = 0.2  ! cm

! The following typical mean squared values depend on control variable. They 
! are calculated in DA_Setup_Background_Errors and used in the VvToVp adjoint 
! test:

 real, parameter    :: inv_typ_vp1_sumsq = 0.00001 ! 1/SUM(psi**2)
 real, parameter    :: inv_typ_vp2_sumsq = 0.00001 ! 1/SUM(chi**2)
 real, parameter    :: inv_typ_vp3_sumsq = 0.00001 ! 1/SUM(phi_u**2)
 real, parameter    :: inv_typ_vp4_sumsq = 10000.0 ! 1/SUM(q**2)
 real, parameter    :: inv_typ_vp5_sumsq = 0.00001 ! 1/SUM(?**2)

! Minimisation:

  INTEGER, PARAMETER :: MP = 6
  INTEGER, PARAMETER :: LP = 6
  INTEGER, PARAMETER :: MAXFEV = 10
  REAL, PARAMETER    :: FTOL = 1.0E-4
  REAL, PARAMETER    :: GTOL = 0.9
  REAL, PARAMETER    :: XTOL = 1.0E-17
  REAL, PARAMETER    :: STPMIN = 1.0E-20
  REAL, PARAMETER    :: STPMAX = 1.0E+20

! FFTs:

  INTEGER, PARAMETER :: Forward_FFT     = -1 ! Grid to spectral
  INTEGER, PARAMETER :: Inverse_FFT     =  1 ! Spectral to grid.

  INTEGER, PARAMETER :: num_fft_factors = 10 ! Max number of factors.

! Balance:

  INTEGER, PARAMETER :: balance_geo = 1      ! Geostrophic balance.
  INTEGER, PARAMETER :: balance_cyc = 2      ! Cyclostrophic balance.
  INTEGER, PARAMETER :: balance_geocyc = 3   ! Geo/cyclostrophic balance.

!------------------------------------------------------------------------------
! Finite-difference constants:
!------------------------------------------------------------------------------

 integer                :: dx          ! Grid spacing in the x-direction.
 integer                :: dy          ! Grid spacing in the y-direction.
 integer                :: ds          ! Grid spacing when dx = dy.

!------------------------------------------------------------------------------
! 3DVAR scalar constants:
!------------------------------------------------------------------------------
 integer                :: Anal_Space  ! Space of analysis
                                       ! ( 1 = Full model,
                                       !   2 = Transformed grid,
                                       !   3 = Ob space (PSAS) )

! integer                :: mix         ! 1st dimension of analysis grid.
! integer                :: mjx         ! 2nd dimension of analysis grid.
 integer                :: mkx         ! 3rd dimension of analysis grid.

!Minimisation:
 integer                :: ntmax       ! Max. number of minim. its.
 integer                :: nsave       ! #Minimisation states to store.
 real                   :: eps0        ! Fraction of initial gradient 
                                            ! required for min. convergence.

!Recursive filter:
 integer                :: npass       ! Number of passes in filter.
 integer                :: ncorr       ! Correlation scale (# grid_pts)

!Observations:
 integer, parameter     :: max_sound = 2000,   &  ! Maximum Number of radiosonde obs.
                           max_synop = 10000,  &  ! Maximum Number of surface obs.
                           max_satob = 30000,  &  ! Maximum Number of satellite wind obs.
                           max_airep = 20000,  &  ! Maximum Number of AIREP obs.
                           max_satem = 20000,  &  ! Maximum Number of SATEM obs.
                           max_pilot = 2000,   &  ! Maximum Number of PILOT obs.
                           max_amdar = 10000,  &  ! Maximum Number of PILOT obs.
                           max_metar = 10000,  &  ! Maximum Number of METAR obs.
                           max_gpspw = 5000,   &  ! Maximum Number of GPSPW obs.
                           max_ships = 5000,   &  ! Maximum Number of SHIP obs.
                           max_ssmt1 = 20000,  &  ! Maximum Number of SSMT1 obs.
                           max_ssmt2 = 20000,  &  ! Maximum Number of SSMT2 obs.
                           max_ssmi  = 90000,  &  ! Maximum Number of SSMI obs.
                           max_tovs  = 20000,  &  ! Maximum Number of TOVS obs.
                           max_qscat = 60000,  &  ! Maximum Number of QSCAT obs.
                           max_profl =   600,  &  ! Maximum Number of PROFL obs.
                           max_buoys = 60000,  &  ! Maximum Number of BUOY obs.
                           max_bogus =  2000,  &  ! Maximum Number of BOGUS obs.
                           max_gpsref = 40000, &  ! Maximum Number of GPSRF obs.
                           max_airsret = 30000   ! Maximum Number of AIRS retrieval

 integer, parameter     :: max_levels      = 100    ! Maximum levels for single observation.

 integer                :: max_sound_input,   &
                           max_synop_input,   &
                           max_satob_input,   &
                           max_airep_input,   &
                           max_satem_input,   &
                           max_pilot_input,   &
                           max_amdar_input,   &
                           max_metar_input,   &
                           max_gpspw_input,   &
                           max_ships_input,   &
                           max_ssmt1_input,   &
                           max_ssmt2_input,   &
                           max_ssmi_input,    &
                           max_tovs_input,    &
                           max_qscat_input,   &
                           max_profl_input,   &
                           max_buoys_input,   &
                           max_bogus_input,   &
                           max_gpsref_input,  &
                           max_airsret_input

!------------------------------------------------------------------------------
!3DVAR array constants:
!------------------------------------------------------------------------------

 real, allocatable      :: gba_inv(:,:) ! 1 / grid box area.
 real, allocatable      :: gscalc(:,:)  ! Scaling factor at cross points.

!------------------------------------------------------------------------------
! 2.0 Namelist parameters:
!------------------------------------------------------------------------------

 CHARACTER*(2) :: MM5_VERSION   ! MM5 version used.
 CHARACTER*(8) :: ANALYSIS_TYPE ! Either '3D-PSAS' OR '3D-VAR'
 CHARACTER*(8) :: VERTICAL_COOR ! Vertical coordinate ('HEIGHT'/'PRESSURE')
 CHARACTER*(8) :: PROCESS_OBS   ! Observation preprocessing parameter.
 CHARACTER*(24):: ANALYSIS_DATE ! Analysis date.
!REAL          :: TIME_WINDOW   ! Assimilation time window.
 INTEGER       :: NVERIF        ! Gradient verification switch(0/1=no/yes)
 INTEGER       :: NSMOOTH       ! # times to smooth final increments.
 REAL          :: T_FORECAST
 REAL          :: T_ANALYSIS
 LOGICAL       :: Testing_3DVAR ! Adjoint, gradient tests if done if .TRUE.

 INTEGER       :: cv_options
 INTEGER       :: balance_type
 INTEGER       :: vert_corr
 REAL          :: max_vert_var1, max_vert_var2, max_vert_var3, &
                  max_vert_var4, max_vert_var5
 LOGICAL       :: test_transforms
 LOGICAL       :: test_statistics
 LOGICAL       :: print_obs_info

!other


 CHARACTER*80  CHEADL1
 CHARACTER*80  CHEADL2
 CHARACTER*160 CHEADL3


 integer, parameter :: jperr = 6

!C...NCEP ERRORS (U in m/s, V in m/s, T in K, H in %, P in Pa)
!C...RH HAS BEEN DIVIDED BY 2

 real, parameter :: err_k(0:jperr+1) = &
                    (/200000., 100100.,70000.,50000.,30000.,10000.,5000., 1./)
 real, parameter :: err_u(0:jperr+1) = &
                    (/ 1.4, 1.4,   2.4,   2.8,   3.4,   2.5,  2.7,  2.7/)
 real, parameter :: err_v(0:jperr+1) = &
                    (/ 1.4, 1.4,   2.4,   2.8,   3.4,   2.5,  2.7 , 2.7 /)
 real, parameter :: err_t(0:jperr+1) = &
                    (/ 1.8, 1.8,   1.3,   1.3,   2.0,   3.1,  4.0 , 4.0 /)
 real, parameter :: err_rh(0:jperr+1) = &
                    (/ 10.0, 10.0,  10.0,  10.0,  10.0,  10.0, 10.0,  10.0/)
 real, parameter :: err_p(0:jperr+1) = &
                    (/ 100.0,100.0, 100.0, 100.0, 100.0, 100.0,100.0,100.0 /)


 end module DA_Constants
