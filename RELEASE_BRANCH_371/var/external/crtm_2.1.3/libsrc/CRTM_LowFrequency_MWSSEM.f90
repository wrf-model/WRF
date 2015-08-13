!
! CRTM_LowFrequency_MWSSEM
!
! Module containgin routines to compute microwave ocean emissivity components
! (FWD, TL, and AD) for low frequencies.
!
!
! CREATION HISTORY:
!       Written by:     Masahiro Kazumori, JCSDA 31-Jul-2006
!                       Masahiro.Kazumori@noaa.gov
!                       Quanhua Liu, QSS Group Inc.
!                       Quanhua.Liu@noaa.gov
!
!       Refactored by:  Paul van Delst, CIMSS/SSEC, April 2007
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_LowFrequency_MWSSEM

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds        , ONLY: fp
  USE Fresnel           , ONLY: fVar_type => iVar_type , &
                                Fresnel_Reflectivity   , &
                                Fresnel_Reflectivity_TL, &
                                Fresnel_Reflectivity_AD
  USE Guillou           , ONLY: gpVar_type => iVar_type, &
                                Guillou_Ocean_Permittivity   , &
                                Guillou_Ocean_Permittivity_TL, &
                                Guillou_Ocean_Permittivity_AD
  USE Ellison           , ONLY: epVar_type => iVar_type, &
                                Ellison_Ocean_Permittivity   , &
                                Ellison_Ocean_Permittivity_TL, &
                                Ellison_Ocean_Permittivity_AD
  USE CRTM_Parameters   , ONLY: PI, DEGREES_TO_RADIANS, &
                                ZERO, ONE, TWO, POINT_5
  USE CRTM_Interpolation, ONLY: NPTS        , &
                                LPoly_type  , &
                                Clear_LPoly , &
                                Find_Index  , &
                                LPoly       , &
                                LPoly_TL    , &
                                LPoly_AD    , &
                                Interp_2D   , &
                                Interp_2D_TL, &
                                Interp_2D_AD
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: LowFrequency_MWSSEM
  PUBLIC :: LowFrequency_MWSSEM_TL
  PUBLIC :: LowFrequency_MWSSEM_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_LowFrequency_MWSSEM.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'

  ! Various quantities
  REAL(fp), PARAMETER :: LOW_F_THRESHOLD        = 20.0_fp  ! Frequency threshold for permittivity models(GHz)
  REAL(fp), PARAMETER :: SMALLSCALE_F_THRESHOLD = 15.0_fp  ! Frequency threshold for small scale limit(GHz)
  REAL(fp), PARAMETER :: FOAM_THRESHOLD   = 7.0_fp         ! Wind speed threshold (m/s) for foam
  REAL(fp), PARAMETER :: GHZ_TO_HZ        = 1.0e+09_fp     ! Frequency units conversion

  ! LUT dimension arrays
  REAL(fp), PARAMETER :: D_FREQUENCY = 2.0_fp
  INTEGER , PARAMETER :: N_FREQUENCIES = 11
  REAL(fp), PARAMETER :: FREQUENCY_SDD(N_FREQUENCIES) = (/ &
      3.0_fp,  5.0_fp,  7.0_fp,  9.0_fp, 11.0_fp, &
     13.0_fp, 15.0_fp, 17.0_fp, 19.0_fp, 21.0_fp, &
     23.0_fp /)

  REAL(fp), PARAMETER :: D_WIND_SPEED = 0.5_fp
  INTEGER , PARAMETER :: N_WIND_SPEEDS = 40
  REAL(fp), PARAMETER :: WIND_SPEED_SDD(N_WIND_SPEEDS) = (/ &
     0.5_fp, 1.0_fp, 1.5_fp, 2.0_fp, 2.5_fp, 3.0_fp, 3.5_fp, 4.0_fp, &
     4.5_fp, 5.0_fp, 5.5_fp, 6.0_fp, 6.5_fp, 7.0_fp, 7.5_fp, 8.0_fp, &
     8.5_fp, 9.0_fp, 9.5_fp,10.0_fp,10.5_fp,11.0_fp,11.5_fp,12.0_fp, &
    12.5_fp,13.0_fp,13.5_fp,14.0_fp,14.5_fp,15.0_fp,15.5_fp,16.0_fp, &
    16.5_fp,17.0_fp,17.5_fp,18.0_fp,18.5_fp,19.0_fp,19.5_fp,20.0_fp /)

  ! Foam coverage coefficients
  ! See Eqn.(1) in
  !   Liu, Q. et al. (1998) Monte Carlo simulations of the
  !     microwave emissivity of the sea surface.
  !     JGR, Volume 103, No.C11, Pages 24983-24989
  REAL(fp), PARAMETER :: FC1 = 7.751e-6_fp  ! FWD model
  REAL(fp), PARAMETER :: FC2 = 3.231_fp     ! FWD model
  REAL(fp), PARAMETER :: FC3 = FC1*FC2      ! TL model
  REAL(fp), PARAMETER :: FC4 = FC2-ONE      ! TL model

  ! Reflectivity large scale correction coefficients
  REAL(fp), PARAMETER :: LSC_COEFF(8) = &
    (/ 5.209e-04_fp,  1.582e-05_fp, -3.510e-07_fp, &  ! Rv coefficients
       5.209e-04_fp, -1.550e-05_fp, -1.356e-07_fp, &  ! Rh coefficients
         1.90896_fp,   0.120448_fp /)                 ! Frequency coefficients


  ! --------------------------------------
  ! Structure definition to hold internal
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! Validity indicator
    LOGICAL :: Is_Valid = .FALSE.
    ! Forward model input values
    REAL(fp) :: Frequency    = ZERO
    REAL(fp) :: Zenith_Angle = ZERO
    REAL(fp) :: Temperature  = ZERO
    REAL(fp) :: Salinity     = ZERO
    REAL(fp) :: Wind_Speed   = ZERO
    ! The zenith angle terms
    REAL(fp) :: cos_z = ZERO, cos2_z = ZERO
    ! The interpolating polynomials
    TYPE(LPoly_type) :: flp  ! Frequency
    TYPE(LPoly_type) :: wlp  ! Wind speed
    ! The LUT interpolation indices
    INTEGER :: i1 = -1, i2 = -1  ! Frequency
    INTEGER :: j1 = -1, j2 = -1  ! Wind speed
    ! The interpolation input
    REAL(fp) :: f_int = ZERO  ! Frequency
    REAL(fp) :: w_int = ZERO  ! Wind speed
    ! The interpolation output
    REAL(fp) :: sdd_int = ZERO  ! Ocean surface height variance
    ! Ocean permittivity
    COMPLEX(fp) :: Permittivity = ZERO
    ! Fresnel reflectivties
    REAL(fp) :: Rv_Fresnel = ZERO
    REAL(fp) :: Rh_Fresnel = ZERO
    ! Foam reflectivties
    REAL(fp) :: Rv_Foam = ZERO
    REAL(fp) :: Rh_Foam = ZERO
    ! Foam coverage
    REAL(fp) :: Foam_Cover = ZERO
    ! Small scale correction intermediate term
    REAL(fp) :: R_STerm = ZERO
    ! Small scale correction to reflectivities
    REAL(fp) :: Rv_Small = ZERO
    REAL(fp) :: Rh_Small = ZERO
    ! Large scale correction intermediate terms
    REAL(fp) :: Rv_LTerm = ZERO
    REAL(fp) :: Rh_LTerm = ZERO
    ! Final reflectivity
    REAL(fp) :: Rv = ZERO
    REAL(fp) :: Rh = ZERO
    ! Internal variables for subcomponents
    TYPE(fVar_type)  :: fVar
    TYPE(epVar_type) :: epVar
    TYPE(gpVar_type) :: gpVar
  END TYPE iVar_type


  ! -----------------------------------------------------
  ! LUT data. Ocean surface height variance. Units of m^2
  ! -----------------------------------------------------
  REAL(fp) :: sdd(N_FREQUENCIES,N_WIND_SPEEDS)
  INTEGER  :: n

  DATA (sdd(n,1),n=1,N_FREQUENCIES) / &
     3.45700E-03_fp, 9.60280E-03_fp, 1.86610E-02_fp, 2.63970E-02_fp, 2.99600E-02_fp, &
     3.11630E-02_fp, 3.11300E-02_fp, 3.02250E-02_fp, 2.87810E-02_fp, 2.73210E-02_fp, &
     2.57200E-02_fp  /

  DATA (sdd(n,2),n=1,N_FREQUENCIES) / &
     4.04780E-02_fp, 4.26570E-02_fp, 4.30630E-02_fp, 4.22650E-02_fp, 4.07540E-02_fp, &
     3.94740E-02_fp, 3.78900E-02_fp, 3.60050E-02_fp, 3.41810E-02_fp, 3.24220E-02_fp, &
     3.04150E-02_fp  /

  DATA (sdd(n,3),n=1,N_FREQUENCIES) / &
     4.59590E-02_fp, 4.48090E-02_fp, 4.46420E-02_fp, 4.36330E-02_fp, 4.24890E-02_fp, &
     4.15140E-02_fp, 4.01950E-02_fp, 3.88670E-02_fp, 3.74780E-02_fp, 3.60030E-02_fp, &
     3.53260E-02_fp  /

  DATA (sdd(n,4),n=1,N_FREQUENCIES) / &
     4.59450E-02_fp, 4.58550E-02_fp, 4.47040E-02_fp, 4.46270E-02_fp, 4.41120E-02_fp, &
     4.32700E-02_fp, 4.28010E-02_fp, 4.19510E-02_fp, 4.10440E-02_fp, 4.08170E-02_fp, &
     4.11710E-02_fp  /

  DATA (sdd(n,5),n=1,N_FREQUENCIES) / &
     4.64550E-02_fp, 4.56360E-02_fp, 4.59130E-02_fp, 4.55520E-02_fp, 4.53160E-02_fp, &
     4.51440E-02_fp, 4.53070E-02_fp, 4.51610E-02_fp, 4.52720E-02_fp, 4.59090E-02_fp, &
     4.67220E-02_fp  /

  DATA (sdd(n,6),n=1,N_FREQUENCIES) / &
     4.66200E-02_fp, 4.59520E-02_fp, 4.58590E-02_fp, 4.60200E-02_fp, 4.59480E-02_fp, &
     4.64690E-02_fp, 4.65140E-02_fp, 4.70470E-02_fp, 4.72740E-02_fp, 4.85060E-02_fp, &
     4.90510E-02_fp  /

  DATA (sdd(n,7),n=1,N_FREQUENCIES) / &
     4.61430E-02_fp, 4.62460E-02_fp, 4.63810E-02_fp, 4.67430E-02_fp, 4.77040E-02_fp, &
     4.85840E-02_fp, 4.95640E-02_fp, 5.12130E-02_fp, 5.25020E-02_fp, 5.29830E-02_fp, &
     5.38460E-02_fp  /

  DATA (sdd(n,8),n=1,N_FREQUENCIES) / &
     4.66920E-02_fp, 4.60310E-02_fp, 4.66870E-02_fp, 4.77130E-02_fp, 4.86120E-02_fp, &
     4.99180E-02_fp, 5.17960E-02_fp, 5.37230E-02_fp, 5.46800E-02_fp, 5.54400E-02_fp, &
     5.60010E-02_fp  /

  DATA (sdd(n,9),n=1,N_FREQUENCIES) / &
     4.63630E-02_fp, 4.69850E-02_fp, 4.79640E-02_fp, 4.94210E-02_fp, 5.30620E-02_fp, &
     5.50180E-02_fp, 5.59020E-02_fp, 5.75140E-02_fp, 5.88560E-02_fp, 5.99310E-02_fp, &
     6.00960E-02_fp  /

  DATA (sdd(n,10),n=1,N_FREQUENCIES) / &
     4.65880E-02_fp, 4.71020E-02_fp, 4.87240E-02_fp, 5.08410E-02_fp, 5.46400E-02_fp, &
     5.65520E-02_fp, 5.82880E-02_fp, 5.97860E-02_fp, 6.10270E-02_fp, 6.20060E-02_fp, &
     6.20490E-02_fp  /

  DATA (sdd(n,11),n=1,N_FREQUENCIES) / &
     4.63770E-02_fp, 4.79790E-02_fp, 5.07890E-02_fp, 5.49620E-02_fp, 5.83330E-02_fp, &
     6.02050E-02_fp, 6.19130E-02_fp, 6.33790E-02_fp, 6.45780E-02_fp, 6.55040E-02_fp, &
     6.61630E-02_fp  /

  DATA (sdd(n,12),n=1,N_FREQUENCIES) / &
     4.66490E-02_fp, 4.87630E-02_fp, 5.48840E-02_fp, 5.66550E-02_fp, 6.01050E-02_fp, &
     6.20090E-02_fp, 6.37430E-02_fp, 6.52270E-02_fp, 6.64330E-02_fp, 6.73580E-02_fp, &
     6.80090E-02_fp  /

  DATA (sdd(n,13),n=1,N_FREQUENCIES) / &
     4.73370E-02_fp, 5.04070E-02_fp, 5.78990E-02_fp, 5.98650E-02_fp, 6.35580E-02_fp, &
     6.55950E-02_fp, 6.74360E-02_fp, 6.90000E-02_fp, 7.02620E-02_fp, 7.12200E-02_fp, &
     7.18850E-02_fp  /

  DATA (sdd(n,14),n=1,N_FREQUENCIES) / &
     4.79880E-02_fp, 5.43970E-02_fp, 6.08900E-02_fp, 6.31260E-02_fp, 6.56630E-02_fp, &
     6.93250E-02_fp, 7.01740E-02_fp, 7.19480E-02_fp, 7.33710E-02_fp, 7.44530E-02_fp, &
     7.52120E-02_fp  /

  DATA (sdd(n,15),n=1,N_FREQUENCIES) / &
     4.88910E-02_fp, 5.87050E-02_fp, 6.30810E-02_fp, 6.58100E-02_fp, 6.87010E-02_fp, &
     7.13730E-02_fp, 7.37010E-02_fp, 7.56450E-02_fp, 7.72040E-02_fp, 7.83900E-02_fp, &
     7.83620E-02_fp  /

  DATA (sdd(n,16),n=1,N_FREQUENCIES) / &
     5.08950E-02_fp, 6.31620E-02_fp, 6.58390E-02_fp, 7.13070E-02_fp, 7.45450E-02_fp, &
     7.61300E-02_fp, 7.88420E-02_fp, 7.99710E-02_fp, 8.18550E-02_fp, 8.23400E-02_fp, &
     8.34470E-02_fp  /

  DATA (sdd(n,17),n=1,N_FREQUENCIES) / &
     5.54760E-02_fp, 6.46340E-02_fp, 6.81130E-02_fp, 7.41470E-02_fp, 7.61420E-02_fp, &
     7.96030E-02_fp, 8.12890E-02_fp, 8.38430E-02_fp, 8.48200E-02_fp, 8.54770E-02_fp, &
     8.67520E-02_fp  /

  DATA (sdd(n,18),n=1,N_FREQUENCIES) / &
     5.87440E-02_fp, 6.91110E-02_fp, 7.31410E-02_fp, 7.77400E-02_fp, 8.03610E-02_fp, &
     8.28910E-02_fp, 8.64080E-02_fp, 8.81030E-02_fp, 8.94090E-02_fp, 9.03320E-02_fp, &
     9.08970E-02_fp  /

  DATA (sdd(n,19),n=1,N_FREQUENCIES) / &
     6.37450E-02_fp, 7.11440E-02_fp, 7.64340E-02_fp, 8.18560E-02_fp, 8.50240E-02_fp, &
     8.79790E-02_fp, 9.05320E-02_fp, 9.26230E-02_fp, 9.31090E-02_fp, 9.43720E-02_fp, &
     9.52240E-02_fp  /

  DATA (sdd(n,20),n=1,N_FREQUENCIES) / &
     6.75330E-02_fp, 7.14800E-02_fp, 7.78540E-02_fp, 8.39610E-02_fp, 8.75990E-02_fp, &
     9.09100E-02_fp, 9.23600E-02_fp, 9.47960E-02_fp, 9.67050E-02_fp, 9.70420E-02_fp, &
     9.80910E-02_fp  /

  DATA (sdd(n,21),n=1,N_FREQUENCIES) / &
     6.83640E-02_fp, 7.42600E-02_fp, 8.16470E-02_fp, 8.63360E-02_fp, 9.07880E-02_fp, &
     9.47130E-02_fp, 9.66050E-02_fp, 9.94340E-02_fp, 1.00460E-01_fp, 1.01120E-01_fp, &
     1.02470E-01_fp  /

  DATA (sdd(n,22),n=1,N_FREQUENCIES) / &
     7.34960E-02_fp, 7.95300E-02_fp, 8.45850E-02_fp, 9.00820E-02_fp, 9.51380E-02_fp, &
     9.79040E-02_fp, 1.00300E-01_fp, 1.02240E-01_fp, 1.04930E-01_fp, 1.05860E-01_fp, &
     1.06410E-01_fp  /

  DATA (sdd(n,23),n=1,N_FREQUENCIES) / &
     7.10970E-02_fp, 8.02870E-02_fp, 8.95720E-02_fp, 9.32400E-02_fp, 9.89780E-02_fp, &
     1.02230E-01_fp, 1.03530E-01_fp, 1.05910E-01_fp, 1.07750E-01_fp, 1.09070E-01_fp, &
     1.09960E-01_fp  /

  DATA (sdd(n,24),n=1,N_FREQUENCIES) / &
     7.36820E-02_fp, 8.40180E-02_fp, 9.12120E-02_fp, 9.81670E-02_fp, 1.02370E-01_fp, &
     1.04470E-01_fp, 1.07850E-01_fp, 1.10580E-01_fp, 1.11430E-01_fp, 1.13090E-01_fp, &
     1.14270E-01_fp  /

  DATA (sdd(n,25),n=1,N_FREQUENCIES) / &
     8.18210E-02_fp, 8.72020E-02_fp, 9.52010E-02_fp, 1.00400E-01_fp, 1.05370E-01_fp, &
     1.08020E-01_fp, 1.11860E-01_fp, 1.13550E-01_fp, 1.16100E-01_fp, 1.16820E-01_fp, &
     1.18310E-01_fp  /

  DATA (sdd(n,26),n=1,N_FREQUENCIES) / &
     8.28020E-02_fp, 8.99060E-02_fp, 9.88250E-02_fp, 1.02260E-01_fp, 1.08030E-01_fp, &
     1.11260E-01_fp, 1.15580E-01_fp, 1.17650E-01_fp, 1.19200E-01_fp, 1.20280E-01_fp, &
     1.22110E-01_fp  /

  DATA (sdd(n,27),n=1,N_FREQUENCIES) / &
     8.31730E-02_fp, 9.21910E-02_fp, 9.91400E-02_fp, 1.06140E-01_fp, 1.10390E-01_fp, &
     1.16000E-01_fp, 1.17460E-01_fp, 1.20050E-01_fp, 1.22050E-01_fp, 1.23510E-01_fp, &
     1.25680E-01_fp  /

  DATA (sdd(n,28),n=1,N_FREQUENCIES) / &
     8.30700E-02_fp, 9.41130E-02_fp, 1.02090E-01_fp, 1.09790E-01_fp, 1.14520E-01_fp, &
     1.18740E-01_fp, 1.22290E-01_fp, 1.23670E-01_fp, 1.26010E-01_fp, 1.27770E-01_fp, &
     1.29050E-01_fp  /

  DATA (sdd(n,29),n=1,N_FREQUENCIES) / &
     8.91170E-02_fp, 9.98440E-02_fp, 1.04780E-01_fp, 1.13210E-01_fp, 1.16370E-01_fp, &
     1.21240E-01_fp, 1.25310E-01_fp, 1.27100E-01_fp, 1.29790E-01_fp, 1.30580E-01_fp, &
     1.32220E-01_fp  /

  DATA (sdd(n,30),n=1,N_FREQUENCIES) / &
     8.81060E-02_fp, 1.01160E-01_fp, 1.07230E-01_fp, 1.13940E-01_fp, 1.20090E-01_fp, &
     1.25390E-01_fp, 1.28140E-01_fp, 1.30340E-01_fp, 1.33410E-01_fp, 1.34500E-01_fp, &
     1.35220E-01_fp  /

  DATA (sdd(n,31),n=1,N_FREQUENCIES) / &
     9.35810E-02_fp, 1.02220E-01_fp, 1.12600E-01_fp, 1.16940E-01_fp, 1.23650E-01_fp, &
     1.27510E-01_fp, 1.30780E-01_fp, 1.33410E-01_fp, 1.35440E-01_fp, 1.38270E-01_fp, &
     1.39280E-01_fp  /

  DATA (sdd(n,32),n=1,N_FREQUENCIES) / &
     9.35500E-02_fp, 1.04710E-01_fp, 1.13220E-01_fp, 1.21530E-01_fp, 1.26710E-01_fp, &
     1.31310E-01_fp, 1.35180E-01_fp, 1.38300E-01_fp, 1.40760E-01_fp, 1.41310E-01_fp, &
     1.42850E-01_fp  /

  DATA (sdd(n,33),n=1,N_FREQUENCIES) / &
     9.85130E-02_fp, 1.09590E-01_fp, 1.18240E-01_fp, 1.24170E-01_fp, 1.29910E-01_fp, &
     1.34960E-01_fp, 1.37450E-01_fp, 1.41030E-01_fp, 1.43870E-01_fp, 1.44760E-01_fp, &
     1.46610E-01_fp  /

  DATA (sdd(n,34),n=1,N_FREQUENCIES) / &
     9.61960E-02_fp, 1.09960E-01_fp, 1.19920E-01_fp, 1.26660E-01_fp, 1.32990E-01_fp, &
     1.36530E-01_fp, 1.41320E-01_fp, 1.43620E-01_fp, 1.46860E-01_fp, 1.48090E-01_fp, &
     1.50270E-01_fp  /

  DATA (sdd(n,35),n=1,N_FREQUENCIES) / &
     1.00720E-01_fp, 1.14610E-01_fp, 1.21450E-01_fp, 1.29020E-01_fp, 1.35940E-01_fp, &
     1.39920E-01_fp, 1.43320E-01_fp, 1.47690E-01_fp, 1.49730E-01_fp, 1.51310E-01_fp, &
     1.52550E-01_fp  /

  DATA (sdd(n,36),n=1,N_FREQUENCIES) / &
     1.06720E-01_fp, 1.16120E-01_fp, 1.24380E-01_fp, 1.32860E-01_fp, 1.38180E-01_fp, &
     1.44960E-01_fp, 1.48770E-01_fp, 1.50260E-01_fp, 1.52880E-01_fp, 1.54980E-01_fp, &
     1.56700E-01_fp  /

  DATA (sdd(n,37),n=1,N_FREQUENCIES) / &
     1.03610E-01_fp, 1.15990E-01_fp, 1.28950E-01_fp, 1.34940E-01_fp, 1.40860E-01_fp, &
     1.46100E-01_fp, 1.50500E-01_fp, 1.54100E-01_fp, 1.57010E-01_fp, 1.57970E-01_fp, &
     1.60030E-01_fp  /

  DATA (sdd(n,38),n=1,N_FREQUENCIES) / &
     1.09100E-01_fp, 1.21630E-01_fp, 1.31540E-01_fp, 1.38450E-01_fp, 1.45050E-01_fp, &
     1.50820E-01_fp, 1.53830E-01_fp, 1.57980E-01_fp, 1.59840E-01_fp, 1.62740E-01_fp, &
     1.63900E-01_fp  /

  DATA (sdd(n,39),n=1,N_FREQUENCIES) / &
     1.13210E-01_fp, 1.21190E-01_fp, 1.32510E-01_fp, 1.40300E-01_fp, 1.47510E-01_fp, &
     1.53760E-01_fp, 1.57150E-01_fp, 1.59950E-01_fp, 1.63770E-01_fp, 1.65520E-01_fp, &
     1.67040E-01_fp  /

  DATA (sdd(n,40),n=1,N_FREQUENCIES) / &
     1.06990E-01_fp, 1.22540E-01_fp, 1.33950E-01_fp, 1.41810E-01_fp, 1.49100E-01_fp, &
     1.53340E-01_fp, 1.58850E-01_fp, 1.61700E-01_fp, 1.64030E-01_fp, 1.65960E-01_fp, &
     1.68950E-01_fp  /


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


!--------------------------------------------------------------------------------
!
! NAME:
!       LowFrequency_MWSSEM
!
! PURPOSE:
!       Subroutine to compute microwave sea surface emissivity for the
!       frequency range 5GHz < f < 20GHz
!
! CALLING SEQUENCE:
!       CALL LowFrequency_MWSSEM( &
!              Frequency   , &  ! Input
!              Zenith_Angle, &  ! Input
!              Temperature , &  ! Input
!              Salinity    , &  ! Input
!              Wind_Speed  , &  ! Input
!              Emissivity  , &  ! Output
!              iVar          )  ! Internal variable output
!
! INPUT ARGUMENTS:
!       Frequency:      Microwave frequency. Valid input range is
!                       UNITS:      GHz
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Zenith_Angle:   Satellite zenith angle at the
!                       sea surface
!                       UNITS:      Degrees
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Temperature:    Sea surface temperature
!                       UNITS:      Kelvin, K
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Salinity:       Water salinity
!                       UNITS:      ppt (parts per thousand)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Wind_Speed:     Sea surface wind speed
!                       UNITS:      m/s
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Emissivity:     The surface emissivity at vertical
!                       and horizontal polarizations.
!                       UNITS:      N/A
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(OUT)
!
!       iVar:           Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of this module.
!                       UNITS:      N/A
!                       TYPE:       iVar_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       For computational speed, no input checking is done.
!       -  Valid frequency range is 5GHz < f < 20GHz
!       -  The size of the output emissivity array is not checked on input.
!          It is assumed the caller has dimensioned it correctly.
!
!--------------------------------------------------------------------------------

  SUBROUTINE LowFrequency_MWSSEM( Frequency   , &  ! Input
                                  Zenith_Angle, &  ! Input
                                  Temperature , &  ! Input
                                  Salinity    , &  ! Input
                                  Wind_Speed  , &  ! Input
                                  Emissivity  , &  ! Output
                                  iVar          )  ! Internal variable output
    ! Arguments
    REAL(fp),        INTENT(IN)     :: Frequency
    REAL(fp),        INTENT(IN)     :: Zenith_Angle
    REAL(fp),        INTENT(IN)     :: Temperature
    REAL(fp),        INTENT(IN)     :: Salinity
    REAL(fp),        INTENT(IN)     :: Wind_Speed
    REAL(fp),        INTENT(OUT)    :: Emissivity(:)
    TYPE(iVar_type), INTENT(IN OUT) :: iVar
    ! Local variables
    LOGICAL  :: f_outbound, w_outbound
    REAL(fp) :: Rv_Large, Rh_Large

    ! Setup
    ! ...Save forward input variables for TL and AD calculations
    iVar%Frequency    = Frequency
    iVar%Zenith_Angle = Zenith_Angle
    iVar%Temperature  = Temperature
    iVar%Salinity     = Salinity
    iVar%Wind_Speed   = Wind_Speed
    ! ...Save derived variables
    iVar%cos_z  = COS(Zenith_Angle*DEGREES_TO_RADIANS)
    iVar%cos2_z = iVar%cos_z * iVar%cos_z


    ! Find the wind speed and frequency indices for interpolation
    iVar%f_int = MAX(MIN(FREQUENCY_SDD(N_FREQUENCIES),Frequency),FREQUENCY_SDD(1))
    CALL Find_Index(FREQUENCY_SDD, D_FREQUENCY, iVar%f_int, iVar%i1, iVar%i2, f_outbound)

    iVar%w_int = MAX(MIN(WIND_SPEED_SDD(N_WIND_SPEEDS),Wind_Speed),WIND_SPEED_SDD(1))
    CALL Find_Index(WIND_SPEED_SDD, D_WIND_SPEED, iVar%w_int, iVar%j1, iVar%j2, w_outbound)


    ! Calculate the interpolating polynomials
    ! ...Frequency term
    CALL LPoly( FREQUENCY_SDD(iVar%i1:iVar%i2), &  ! Input
                iVar%f_int                    , &  ! Input
                iVar%flp                        )  ! Output
    ! ...Wind speed term
    CALL LPoly( WIND_SPEED_SDD(iVar%j1:iVar%j2), &  ! Input
                iVar%w_int                     , &  ! Input
                iVar%wlp                         )  ! Output


    ! Perform the 2-D interpolation
    CALL Interp_2D( sdd(iVar%i1:iVar%i2, iVar%j1:iVar%j2), &
                    iVar%flp, iVar%wlp, iVar%sdd_int       )


    ! Permittivity Calculation
    IF( Frequency < LOW_F_THRESHOLD ) THEN
      CALL Guillou_Ocean_Permittivity( Temperature, Salinity, Frequency, &
                                       iVar%Permittivity, &
                                       iVar%gpVar )
    ELSE
      CALL Ellison_Ocean_Permittivity( Temperature, Frequency, &
                                       iVar%Permittivity, &
                                       iVar%epVar )
    END IF


    ! Fresnel reflectivity calculation
    CALL Fresnel_Reflectivity( iVar%Permittivity,iVar%cos_z, &
                               iVar%Rv_Fresnel,iVar%Rh_Fresnel,&
                               iVar%fVar )


    ! Foam reflectivity calculation
    CALL Foam_Reflectivity( Zenith_Angle, iVar%Rv_Foam, iVar%Rh_Foam )


    ! Foam Coverage calculation
    CALL Foam_Coverage( Wind_Speed, iVar%Foam_Cover )


    ! Small scale correction calculation
    CALL Small_Scale_Correction( iVar%sdd_int, iVar%cos2_z, Frequency, &
                                 iVar%Rv_Small, iVar%Rh_Small, &
                                 iVar%R_STerm )
    iVar%Rv = iVar%Rv_Fresnel * iVar%Rv_Small
    iVar%Rh = iVar%Rh_Fresnel * iVar%Rh_Small


    ! Large Scale Correction Calculation
    CALL Large_Scale_Correction( Wind_Speed, Zenith_Angle, Frequency, &
                                 Rv_Large, Rh_Large, &
                                 iVar%Rv_Lterm, iVar%Rh_Lterm )
    iVar%Rv = iVar%Rv + Rv_Large
    iVar%Rh = iVar%Rh + Rh_Large


    ! Emissivity Calculation
    Emissivity(1) = ONE - (ONE-iVar%Foam_Cover)*iVar%Rv - iVar%Foam_Cover*iVar%Rv_Foam
    Emissivity(2) = ONE - (ONE-iVar%Foam_Cover)*iVar%Rh - iVar%Foam_Cover*iVar%Rh_Foam


    ! Flag the internal variable structure as valid
    iVar%Is_Valid = .TRUE.

  END SUBROUTINE LowFrequency_MWSSEM


!--------------------------------------------------------------------------------
!
! NAME:
!       LowFrequency_MWSSEM_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear microwave sea surface
!       emissivity for the frequency range 5GHz < f < 20GHz
!
! CALLING SEQUENCE:
!       CALL LowFrequency_MWSSEM_TL( &
!              Temperature_TL, &  ! TL  Input
!              Salinity_TL   , &  ! TL  Input
!              Wind_Speed_TL , &  ! TL  Input
!              Emissivity_TL , &  ! TL  Output
!              iVar            )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       Temperature_TL:  The tangent-linear sea surface temperature
!                        UNITS:      Kelvin, K
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Salinity_TL:     The tangent-linear water salinity
!                        UNITS:      ppt (parts per thousand)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Wind_Speed_TL:   The tangent-linear sea surface wind speed
!                        UNITS:      m/s
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       iVar:            Structure containing internal forward variables
!                        required for tangent-linear calculations. This
!                        structure is output from the forward model.
!                        The contents of this structure are NOT accessible
!                        outside of this module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Emissivity_TL:   The tangent-linear sea surface emissivity at
!                        vertical and horizontal polarizations.
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       For computational speed, no input checking is done.
!       -  Valid frequency range is 5GHz < f < 20GHz
!       -  The size of the output emissivity array is not checked on input.
!          It is assumed the caller has dimensioned it correctly.
!
!--------------------------------------------------------------------------------

  SUBROUTINE LowFrequency_MWSSEM_TL( &
    Temperature_TL, &  ! TL  Input
    Salinity_TL   , &  ! TL  Input
    Wind_Speed_TL , &  ! TL  Input
    Emissivity_TL , &  ! TL  Output
    iVar            )  ! Internal variable input
    ! Arguments
    REAL(fp),        INTENT(IN)  :: Temperature_TL
    REAL(fp),        INTENT(IN)  :: Salinity_TL
    REAL(fp),        INTENT(IN)  :: Wind_Speed_TL
    REAL(fp),        INTENT(OUT) :: Emissivity_TL(:)
    TYPE(iVar_type), INTENT(IN)  :: iVar
    ! Local variables
    REAL(fp)    :: sdd_int_TL
    COMPLEX(fp) :: Permittivity_TL
    REAL(fp)    :: Foam_Cover_TL
    REAL(fp)    :: Rv_Fresnel_TL, Rh_Fresnel_TL
    REAL(fp)    :: Rv_Foam_TL   , Rh_Foam_TL
    REAL(fp)    :: Rv_Small_TL  , Rh_Small_TL
    REAL(fp)    :: Rv_Large_TL  , Rh_Large_TL
    REAL(fp)    :: Rv_TL        , Rh_TL
    REAL(fp) :: w_int_TL
    REAL(fp), DIMENSION(NPTS) :: w_TL
    REAL(fp), DIMENSION(NPTS,NPTS) :: sdd_TL
    TYPE(LPoly_type) :: flp_TL, wlp_TL


    ! Setup
    ! ...Check internal structure
    IF ( .NOT. iVar%Is_Valid ) THEN
      Emissivity_TL = ZERO
      RETURN
    END IF
    ! ...Initialise local TL variables
    w_int_TL = Wind_Speed_TL
    w_TL   = ZERO
    sdd_TL = ZERO


    ! Calculate the TL interpolating polynomials
    ! ...Frequency term.
    !    The TL frequency term is always zero (so far at least)
    !    so we just need to initialise the polynomial so it has
    !    no impact on the TL interpolation below.
    CALL Clear_LPoly(flp_TL)
    ! Wind speed term
    CALL LPoly_TL( WIND_SPEED_SDD(iVar%j1:iVar%j2), &  ! FWD Input
                   iVar%w_int                     , &  ! FWD Input
                   iVar%wlp                       , &  ! FWD Input
                   w_TL                           , &  ! TL  Input
                   w_int_TL                       , &  ! TL  Input
                   wlp_TL                           )  ! TL  Output


    ! Perform the 2-D TL interpolation
    CALL Interp_2D_TL( sdd(iVar%i1:iVar%i2, iVar%j1:iVar%j2), &  ! FWD Input
                       iVar%flp, iVar%wlp,                    &  ! FWD Input
                       sdd_TL,                                &  ! TL  Input
                       flp_TL  , wlp_TL,                      &  ! TL  Input
                       sdd_int_TL                             )  ! TL  Output


    ! Permittivity Calculation
    IF( iVar%Frequency < LOW_F_THRESHOLD ) THEN
      CALL Guillou_Ocean_Permittivity_TL( Temperature_TL, Salinity_TL, iVar%Frequency, &
                                          Permittivity_TL, &
                                          iVar%gpVar)
    ELSE
      CALL Ellison_Ocean_Permittivity_TL( Temperature_TL, &
                                          Permittivity_TL, &
                                          iVar%epVar )
    END IF


    ! Fresnel reflectivity calculation
    CALL Fresnel_Reflectivity_TL( Permittivity_TL, iVar%cos_z, &
                                  Rv_Fresnel_TL, Rh_Fresnel_TL, &
                                  iVar%fVar )


    ! Foam reflectivity "calculation"
    Rv_Foam_TL = ZERO
    Rh_Foam_TL = ZERO


    ! Foam coverage calculation
    CALL Foam_Coverage_TL( iVar%Wind_Speed, Wind_Speed_TL, Foam_Cover_TL )


    ! Small scale correction calculation
    CALL Small_Scale_Correction_TL( sdd_int_TL, iVar%cos2_z, iVar%Frequency, &
                                    Rv_Small_TL, Rh_Small_TL, &
                                    iVar%R_STerm )
    Rv_TL = iVar%Rv_Fresnel*Rv_Small_TL + Rv_Fresnel_TL*iVar%Rv_Small
    Rh_TL = iVar%Rh_Fresnel*Rh_Small_TL + Rh_Fresnel_TL*iVar%Rh_Small


    ! Large scale correction calculation
    CALL Large_Scale_Correction_TL( Wind_Speed_TL, Rv_Large_TL, Rh_Large_TL, &
                                    iVar%Rv_Lterm, iVar%Rh_Lterm )
    Rv_TL = Rv_TL + Rv_Large_TL
    Rh_TL = Rh_TL + Rh_Large_TL


    ! Emissivity calculation
    Emissivity_TL(1) = (iVar%Foam_Cover-ONE)*Rv_TL + &
                       (iVar%Rv-iVar%Rv_Foam)*Foam_Cover_TL - &
                       iVar%Foam_Cover*Rv_Foam_TL
    Emissivity_TL(2) = (iVar%Foam_Cover-ONE)*Rh_TL + &
                       (iVar%Rh-iVar%Rh_Foam)*Foam_Cover_TL - &
                       iVar%Foam_Cover*Rh_Foam_TL

  END SUBROUTINE LowFrequency_MWSSEM_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       LowFrequency_MWSSEM_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint microwave sea surface emissivity
!       for the frequency range 5GHz < f < 20GHz
!
! CALLING SEQUENCE:
!       CALL LowFrequency_MWSSEM_AD( &
!              Emissivity_AD , &  ! AD  Input
!              Temperature_AD, &  ! AD  Output
!              Salinity_AD   , &  ! AD  Output
!              Wind_Speed_AD , &  ! AD  Output
!              iVar            )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       Emissivity_AD:   The adjoint sea surface emissivity at
!                        vertical and horizontal polarizations.
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       iVar:            Structure containing internal forward variables
!                        required for adjoint calculations. This
!                        structure is output from the forward model.
!                        The contents of this structure are NOT accessible
!                        outside of this module.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Temperature_AD:  The adjoint sea surface temperature
!                        UNITS:      (Kelvin, K)^-1
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Salinity_AD:     The adjoint water salinity
!                        UNITS:      (ppt)^-1
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Wind_Speed_AD:   The adjoint sea surface wind speed
!                        UNITS:      (m/s)^-1
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! SIDE EFFECTS:
!       The input adjoint variable, Emissivity_AD, is set to zero upon
!       exiting this routine.
!
! COMMENTS:
!       For computational speed, no input checking is done.
!       -  Valid frequency range is 5GHz < f < 20GHz
!       -  The size of the input emissivity array is not checked on input.
!          It is assumed the caller has dimensioned it correctly.
!
!--------------------------------------------------------------------------------

  SUBROUTINE LowFrequency_MWSSEM_AD( &
    Emissivity_AD , &  ! AD  Input
    Temperature_AD, &  ! AD  Output
    Salinity_AD   , &  ! AD  Output
    Wind_Speed_AD , &  ! AD  Output
    iVar            )  ! Internal variable input
    ! Arguments
    REAL(fp),        INTENT(IN OUT) :: Emissivity_AD(:)
    REAL(fp),        INTENT(IN OUT) :: Temperature_AD
    REAL(fp),        INTENT(IN OUT) :: Salinity_AD
    REAL(fp),        INTENT(IN OUT) :: Wind_Speed_AD
    TYPE(iVar_type), INTENT(IN)     :: iVar
    ! Local variables
    REAL(fp)    :: sdd_int_AD
    COMPLEX(fp) :: Permittivity_AD
    REAL(fp)    :: Foam_Cover_AD
    REAL(fp)    :: Rv_Fresnel_AD, Rh_Fresnel_AD
    REAL(fp)    :: Rv_Foam_AD   , Rh_Foam_AD
    REAL(fp)    :: Rv_Small_AD  , Rh_Small_AD
    REAL(fp)    :: Rv_Large_AD  , Rh_Large_AD
    REAL(fp)    :: Rv_AD        , Rh_AD
    REAL(fp) :: w_int_AD
    REAL(fp), DIMENSION(NPTS) :: w_AD
    REAL(fp), DIMENSION(NPTS,NPTS) :: sdd_AD
    TYPE(LPoly_type) :: flp_AD, wlp_AD

    ! Setup
    ! ...Check internal structure
    IF ( .NOT. iVar%Is_Valid ) THEN
      Temperature_AD = ZERO
      Salinity_AD    = ZERO
      Wind_Speed_AD  = ZERO
      RETURN
    END IF
    ! ...Initialise local adjoint variables
    w_int_AD = ZERO
    w_AD = ZERO
    sdd_AD = ZERO
    sdd_int_AD = ZERO
    Permittivity_AD = CMPLX(ZERO,ZERO)
    CALL Clear_LPoly(flp_AD)
    CALL Clear_LPoly(wlp_AD)


    ! Emissivity calculation
    Rh_Foam_AD    = -iVar%Foam_Cover      *Emissivity_AD(2)
    Foam_Cover_AD = (iVar%Rh-iVar%Rh_Foam)*Emissivity_AD(2)
    Rh_AD         = (iVar%Foam_Cover-ONE) *Emissivity_AD(2)

    Rv_Foam_AD    = -iVar%Foam_Cover      *Emissivity_AD(1)
    Foam_Cover_AD = (iVar%Rv-iVar%Rv_Foam)*Emissivity_AD(1) + Foam_Cover_AD
    Rv_AD         = (iVar%Foam_Cover-ONE) *Emissivity_AD(1)

    Emissivity_AD = ZERO


    ! Large scale correction calculation
    Rh_Large_AD = Rh_AD
    Rv_Large_AD = Rv_AD
    CALL Large_Scale_Correction_AD( Rv_Large_AD, Rh_Large_AD, Wind_Speed_AD,  &
                                    iVar%Rv_Lterm, iVar%Rh_Lterm )

    ! Small scale correction calculation
    Rv_Small_AD   = iVar%Rv_Fresnel*Rv_AD
    Rv_Fresnel_AD = iVar%Rv_Small*Rv_AD
    Rv_AD = ZERO
    Rh_Small_AD   = iVar%Rh_Fresnel*Rh_AD
    Rh_Fresnel_AD = iVar%Rh_Small*Rh_AD
    Rh_AD = ZERO
    CALL Small_Scale_Correction_AD( Rv_Small_AD, Rh_Small_AD, iVar%cos2_z, iVar%Frequency, &
                                    sdd_int_AD, &
                                    iVar%R_STerm )

    ! Foam coverage calculation
    CALL Foam_Coverage_AD( iVar%Wind_Speed, Foam_Cover_AD, Wind_Speed_AD)

    ! Fresnel reflectivity calculation
    CALL Fresnel_Reflectivity_AD( Rv_Fresnel_AD, Rh_Fresnel_AD, iVar%cos_z, &
                                  Permittivity_AD, &
                                  iVar%fVar)

    ! Permittivity Calculation
    IF( iVar%Frequency < LOW_F_THRESHOLD ) THEN
      CALL Guillou_Ocean_Permittivity_AD( Permittivity_AD, iVar%Frequency, &
                                          Temperature_AD, Salinity_AD, &
                                          iVar%gpVar)
    ELSE
      CALL Ellison_Ocean_Permittivity_AD( Permittivity_AD, &
                                          Temperature_AD, &
                                          iVar%epVar)
    END IF


    ! Perform the adjoint interpolation
    CALL Interp_2D_AD( sdd(iVar%i1:iVar%i2, iVar%j1:iVar%j2), &  ! FWD Input
                       iVar%flp, iVar%wlp,                    &  ! FWD Input
                       sdd_int_AD,                            &  ! AD  Input
                       sdd_AD,                                &  ! AD  Output
                       flp_AD  , wlp_AD                       )  ! AD  Output

    ! Compute the adjoint of the interpolating polynomials
    ! ...Wind speed term
    CALL LPoly_AD( WIND_SPEED_SDD(iVar%j1:iVar%j2), &  ! FWD Input
                   iVar%w_int                     , &  ! FWD Input
                   iVar%wlp                       , &  ! FWD Input
                   wlp_AD                         , &  ! AD  Input
                   w_AD                           , &  ! AD  Input
                   w_int_AD                         )  ! AD  Output
    ! ...No frequency term LPoly_AD.


    ! The AD outputs
    Wind_Speed_AD = Wind_Speed_AD + w_int_AD

  END SUBROUTINE LowFrequency_MWSSEM_AD


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! =============================================================
  ! Routine for forward model foam reflectivity
  ! Function dependence is on zenith angle only
  ! so no TL or AD routine.
  ! See Eqns(18.44a) (18.44b) in
  !   Ulaby, F.T. et al. (1986) Microwave Remote Sensing, Active
  !     and Passive, vol.3, From Theory to Applications, pp1457.
  ! =============================================================
  SUBROUTINE Foam_Reflectivity(z, Rv, Rh)
    ! Arguments
    REAL(fp), INTENT(IN)  :: z ! Zenith angle
    REAL(fp), INTENT(OUT) :: Rv, Rh
    ! Parameters
    REAL(fp), PARAMETER :: FR_COEFF(9) = &
      (/ -9.946e-4_fp, 3.218e-5_fp, -1.187e-6_fp, &
            7.e-20_fp,     0.07_fp, -1.748e-3_fp, &
         -7.336e-5_fp, 1.044e-7_fp,     -0.93_fp /)
    ! Local variables
    REAL(fp) :: Fv, Fh
    ! The vertical component
    Fv = ONE + z*(FR_COEFF(1)+ z*(FR_COEFF(2) + z*FR_COEFF(3))) + FR_COEFF(4)*z**10
    Rv = FR_COEFF(5)
    ! The horizontal component
    Fh = ONE + z*(FR_COEFF(6) +  z*(FR_COEFF(7) + z*FR_COEFF(8)))
    Rh = ONE + FR_COEFF(9)*Fh
  END SUBROUTINE Foam_Reflectivity


  ! ======================================================
  ! Foam coverage routines
  ! See Eqn.(1) in
  !   Liu, Q. et al. (1998) Monte Carlo simulations of the
  !     microwave emissivity of the sea surface.
  !     JGR, Volume 103, No.C11, Pages 24983-24989
  ! ======================================================
  ! Forward model
  SUBROUTINE Foam_Coverage(wind_speed, coverage)
    REAL(fp), INTENT(IN)  :: wind_speed
    REAL(fp), INTENT(OUT) :: coverage
    IF ( wind_speed < FOAM_THRESHOLD ) THEN
       coverage = ZERO
    ELSE
       coverage = FC1 * (wind_speed**FC2)
    END IF
  END SUBROUTINE Foam_Coverage

  ! Tangent-linear model
  SUBROUTINE Foam_Coverage_TL(wind_speed, wind_speed_TL, coverage_TL)
    REAL(fp), INTENT(IN)  :: wind_speed
    REAL(fp), INTENT(IN)  :: wind_speed_TL
    REAL(fp), INTENT(OUT) :: coverage_TL
    IF ( wind_speed < FOAM_THRESHOLD ) THEN
       coverage_TL = ZERO
    ELSE
       coverage_TL = FC3 * (wind_speed**FC4) * wind_speed_TL
    END IF
  END SUBROUTINE Foam_Coverage_TL

  ! Adjoint model
  SUBROUTINE Foam_Coverage_AD(wind_speed, coverage_AD, wind_speed_AD)
    REAL(fp), INTENT(IN)     :: wind_speed     ! Input
    REAL(fp), INTENT(IN OUT) :: coverage_AD    ! Input
    REAL(fp), INTENT(IN OUT) :: wind_speed_AD  ! Output
    IF ( .NOT. (wind_speed < FOAM_THRESHOLD) ) THEN
      wind_speed_AD = wind_speed_AD + FC3*(wind_speed**FC4)*coverage_AD
    END IF
    coverage_AD = ZERO
  END SUBROUTINE Foam_Coverage_AD


  ! ======================================================
  ! Reflectivity small scale correction routines
  ! See Eqns.(17a) and (17b) in
  !   Liu, Q. et al. (1998) Monte Carlo simulations of the
  !     microwave emissivity of the sea surface.
  !     JGR, Volume 103, No.C11, Pages 24983-24989
  ! ======================================================
  ! Forward model
  SUBROUTINE Small_Scale_Correction( sdd, cos2_z, f, Rv, Rh, &
                                     R_term )
    ! Arguments
    REAL(fp), INTENT(IN)  :: sdd     ! Ocean surface height variance
    REAL(fp), INTENT(IN)  :: cos2_z  ! cos^2 of zenith angle
    REAL(fp), INTENT(IN)  :: f       ! Frequency
    REAL(fp), INTENT(OUT) :: Rv, Rh  ! Reflectivity correction
    REAL(fp), INTENT(OUT) :: R_term  ! Intermediate variable output

    IF ( f > SMALLSCALE_F_THRESHOLD ) THEN
      R_term = EXP(-sdd*cos2_z)
    ELSE
      R_term = ONE
    END IF
    Rv = R_term
    Rh = R_term
  END SUBROUTINE Small_Scale_Correction

  ! Tangent-linear model
  SUBROUTINE Small_Scale_Correction_TL( sdd_TL, cos2_z, f, Rv_TL, Rh_TL, &
                                        R_term )
    ! Arguments
    REAL(fp), INTENT(IN)  :: sdd_TL
    REAL(fp), INTENT(IN)  :: cos2_z
    REAL(fp), INTENT(IN)  :: f
    REAL(fp), INTENT(OUT) :: Rv_TL, Rh_TL
    REAL(fp), INTENT(IN)  :: R_term  ! Intermediate variable input
    ! Local variables
    REAL(fp) :: R_term_TL

    IF ( f > SMALLSCALE_F_THRESHOLD ) THEN
      R_term_TL = -cos2_z * R_term * sdd_TL
    ELSE
      R_term_TL = ZERO
    END IF
    Rv_TL = R_term_TL
    Rh_TL = R_term_TL
  END SUBROUTINE Small_Scale_Correction_TL

  ! Adjoint model
  SUBROUTINE Small_Scale_Correction_AD( Rv_AD, Rh_AD, cos2_z, f, sdd_AD, &
                                        R_term )
    ! Arguments
    REAL(fp), INTENT(IN OUT) :: Rv_AD, Rh_AD  ! Input
    REAL(fp), INTENT(IN)     :: cos2_z        ! Input
    REAL(fp), INTENT(IN)     :: f             ! Input
    REAL(fp), INTENT(IN OUT) :: sdd_AD        ! Output
    REAL(fp), INTENT(IN)     :: R_term        ! Intermediate variable input
    ! Local variables
    REAL(fp) :: R_term_AD

    R_term_AD = Rv_AD            ;  Rv_AD = ZERO
    R_term_AD = R_term_AD + Rh_AD;  Rh_AD = ZERO
    IF ( f > SMALLSCALE_F_THRESHOLD ) THEN
      sdd_AD = sdd_AD - cos2_z*R_term*R_term_AD
    ELSE
      sdd_AD = ZERO
    END IF
  END SUBROUTINE Small_Scale_Correction_AD


  ! ============================================
  ! Reflectivity large scale correction routines
  ! ============================================
  ! Forward model
  SUBROUTINE Large_Scale_Correction( v, z, f, Rv, Rh, &
                                     Rv_term, Rh_term )
    ! Arguments
    REAL(fp), INTENT(IN)  :: v       ! Wind speed
    REAL(fp), INTENT(IN)  :: z       ! Zenith angle
    REAL(fp), INTENT(IN)  :: f       ! Frequency
    REAL(fp), INTENT(OUT) :: Rv, Rh  ! Reflectivity correction
    REAL(fp), INTENT(OUT) :: Rv_term, Rh_term  ! Intermediate variable output
    ! Local variables
    REAL(fp) :: f_term

    f_term  = f / (LSC_COEFF(7) + f*LSC_COEFF(8))
    Rv_Term = (LSC_COEFF(1) + z*(LSC_COEFF(2) + z*LSC_COEFF(3))) * f_term
    Rh_term = (LSC_COEFF(4) + z*(LSC_COEFF(5) + z*LSC_COEFF(6))) * f_term

    Rv = Rv_term * v
    Rh = Rh_term * v
  END SUBROUTINE Large_Scale_Correction

  ! Tangent-linear model
  SUBROUTINE Large_Scale_Correction_TL( v_TL, Rv_TL, Rh_TL, &
                                        Rv_term, Rh_term )
    ! Arguments
    REAL(fp), INTENT(IN)  :: v_TL
    REAL(fp), INTENT(OUT) :: Rv_TL, Rh_TL
    REAL(fp), INTENT(IN)  :: Rv_term, Rh_term  ! Intermediate variable input

    Rv_TL = Rv_term * v_TL
    Rh_TL = Rh_term * v_TL
  END SUBROUTINE Large_Scale_Correction_TL

  ! Adjoint model
  SUBROUTINE Large_Scale_Correction_AD( Rv_AD, Rh_AD, v_AD, &
                                        Rv_term, Rh_term )
    ! Arguments
    REAL(fp), INTENT(IN OUT) :: Rv_AD, Rh_AD      ! Input
    REAL(fp), INTENT(IN OUT) :: v_AD              ! Output
    REAL(fp), INTENT(IN)     :: Rv_term, Rh_term  ! Intermediate variable input

    v_AD = v_AD + Rv_term*Rv_AD
    v_AD = v_AD + Rh_term*Rh_AD

    Rv_AD = ZERO
    Rh_AD = ZERO
  END SUBROUTINE Large_Scale_Correction_AD

END MODULE CRTM_LowFrequency_MWSSEM
