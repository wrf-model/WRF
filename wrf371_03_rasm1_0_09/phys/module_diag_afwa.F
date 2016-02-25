#if (NMM_CORE == 1)
MODULE module_diag_afwa
CONTAINS
   SUBROUTINE diag_afwa_stub
   END SUBROUTINE diag_afwa_stub
END MODULE module_diag_afwa
#else

!WRF:MEDIATION_LAYER:PHYSICS

MODULE diag_functions

CONTAINS

  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~ 
  !~ Name:
  !~    calc_rh
  !~
  !~ Description:
  !~    This function calculates relative humidity given pressure, 
  !~    temperature, and water vapor mixing ratio.
  !~ 
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION calc_rh ( p, t, qv ) result ( rh )
    
    IMPLICIT NONE
 
    REAL, INTENT(IN) :: p, t, qv
    REAL :: rh

    ! Local
    ! -----
    REAL, PARAMETER :: pq0=379.90516
    REAL, PARAMETER :: a2=17.2693882
    REAL, PARAMETER :: a3=273.16
    REAL, PARAMETER :: a4=35.86
    REAL, PARAMETER :: rhmin=1.
    REAL :: q, qs
    INTEGER :: i,j,k
  
    ! Following algorithms adapted from WRFPOST
    ! May want to substitute with another later
    ! -----------------------------------------
      q=qv/(1.0+qv)
      qs=pq0/p*exp(a2*(t-a3)/(t-a4))
      rh=100.*q/qs
      IF (rh .gt. 100.) THEN
        rh=100.
      ELSE IF (rh .lt. rhmin) THEN
        rh=rhmin
      ENDIF

  END FUNCTION calc_rh



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~ 
  !~ Name:
  !~    uv_wind
  !~
  !~ Description:
  !~    This function calculates the wind speed given U and V wind
  !~    components.
  !~ 
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION uv_wind ( u, v ) result ( wind_speed )
 
    IMPLICIT NONE
 
    REAL, INTENT(IN) :: u, v
    REAL :: wind_speed

    wind_speed = sqrt( u*u + v*v )

  END FUNCTION uv_wind


  
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    Theta
  !~
  !~ Description:
  !~    This function calculates potential temperature as defined by
  !~    Poisson's equation, given temperature and pressure ( hPa ).
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION Theta ( t, p )
  IMPLICIT NONE

     !~ Variable declaration
     !  --------------------
     REAL, INTENT ( IN ) :: t
     REAL, INTENT ( IN ) :: p
     REAL                :: theta

     REAL :: Rd ! Dry gas constant
     REAL :: Cp ! Specific heat of dry air at constant pressure
     REAL :: p0 ! Standard pressure ( 1000 hPa )
  
     Rd =  287.04
     Cp = 1004.67
     p0 = 1000.00

     !~ Poisson's equation
     !  ------------------
     theta = t * ( (p0/p)**(Rd/Cp) )
  
  END FUNCTION Theta



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    Thetae
  !~
  !~ Description:
  !~    This function returns equivalent potential temperature using the 
  !~    method described in Bolton 1980, Monthly Weather Review, equation 43.
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION Thetae ( tK, p, rh, mixr )
  IMPLICIT NONE

     !~ Variable Declarations
     !  ---------------------
     REAL :: tK        ! Temperature ( K )
     REAL :: p         ! Pressure ( hPa )
     REAL :: rh        ! Relative humidity
     REAL :: mixr      ! Mixing Ratio ( kg kg^-1)
     REAL :: te        ! Equivalent temperature ( K )
     REAL :: thetae    ! Equivalent potential temperature
  
     REAL, PARAMETER :: R  = 287.04         ! Universal gas constant (J/deg kg)
     REAL, PARAMETER :: P0 = 1000.0         ! Standard pressure at surface (hPa)
     REAL, PARAMETER :: lv = 2.54*(10**6)   ! Latent heat of vaporization
                                            ! (J kg^-1)
     REAL, PARAMETER :: cp = 1004.67        ! Specific heat of dry air constant
                                            ! at pressure (J/deg kg)
     REAL :: tlc                            ! LCL temperature
  
     !~ Calculate the temperature of the LCL
     !  ------------------------------------
     tlc = TLCL ( tK, rh )
  
     !~ Calculate theta-e
     !  -----------------
     thetae = (tK * (p0/p)**( (R/Cp)*(1.- ( (.28E-3)*mixr*1000.) ) ) )* &
                 exp( (((3.376/tlc)-.00254))*&
                    (mixr*1000.*(1.+(.81E-3)*mixr*1000.)) )
  
  END FUNCTION Thetae



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    The2T.f90
  !~
  !~ Description:
  !~    This function returns the temperature at any pressure level along a
  !~    saturation adiabat by iteratively solving for it from the parcel
  !~    thetae.
  !~
  !~ Dependencies:
  !~    function thetae.f90
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION The2T ( thetaeK, pres, flag ) result ( tparcel )
  IMPLICIT NONE
  
     !~ Variable Declaration
     !  --------------------
     REAL,    INTENT     ( IN ) :: thetaeK
     REAL,    INTENT     ( IN ) :: pres
     LOGICAL, INTENT ( INOUT )  :: flag
     REAL                       :: tparcel
  
     REAL :: thetaK
     REAL :: tovtheta
     REAL :: tcheck
     REAL :: svpr, svpr2
     REAL :: smixr, smixr2
     REAL :: thetae_check, thetae_check2
     REAL :: tguess_2, correction
  
     LOGICAL :: found
     INTEGER :: iter
  
     REAL :: R     ! Dry gas constant
     REAL :: Cp    ! Specific heat for dry air
     REAL :: kappa ! Rd / Cp
     REAL :: Lv    ! Latent heat of vaporization at 0 deg. C
  
     R     = 287.04
     Cp    = 1004.67
     Kappa = R/Cp
     Lv    = 2.500E+6

     !~ Make initial guess for temperature of the parcel
     !  ------------------------------------------------
     tovtheta = (pres/100000.0)**(r/cp)
     tparcel  = thetaeK/exp(lv*.012/(cp*295.))*tovtheta

     iter = 1
     found = .false.
     flag = .false.

     DO
        IF ( iter > 105 ) EXIT

        tguess_2 = tparcel + REAL ( 1 )

        svpr   = 6.122 * exp ( (17.67*(tparcel-273.15)) / (tparcel-29.66) )
        smixr  = ( 0.622*svpr ) / ( (pres/100.0)-svpr )
        svpr2  = 6.122 * exp ( (17.67*(tguess_2-273.15)) / (tguess_2-29.66) )
        smixr2 = ( 0.622*svpr2 ) / ( (pres/100.0)-svpr2 )

        !  ------------------------------------------------------------------ ~!
        !~ When this function was orinially written, the final parcel         ~!
        !~ temperature check was based off of the parcel temperature and      ~!
        !~ not the theta-e it produced.  As there are multiple temperature-   ~!
        !~ mixing ratio combinations that can produce a single theta-e value, ~!
        !~ we change the check to be based off of the resultant theta-e       ~!
        !~ value.  This seems to be the most accurate way of backing out      ~!
        !~ temperature from theta-e.                                          ~!
        !~                                                                    ~!
        !~ Rentschler, April 2010                                             ~!
        !  ------------------------------------------------------------------  !

        !~ Old way...
        !thetaK = thetaeK / EXP (lv * smixr  /(cp*tparcel) )
        !tcheck = thetaK * tovtheta

        !~ New way
        thetae_check  = Thetae ( tparcel,  pres/100., 100., smixr  )
        thetae_check2 = Thetae ( tguess_2, pres/100., 100., smixr2 )

        !~ Whew doggies - that there is some accuracy...
        !IF ( ABS (tparcel-tcheck) < .05) THEN
        IF ( ABS (thetaeK-thetae_check) < .001) THEN
           found = .true.
           flag  = .true.
           EXIT
        END IF

        !~ Old
        !tparcel = tparcel + (tcheck - tparcel)*.3

        !~ New
        correction = ( thetaeK-thetae_check ) / ( thetae_check2-thetae_check )
        tparcel = tparcel + correction

        iter = iter + 1
     END DO

     !IF ( .not. found ) THEN
     !   print*, "Warning! Thetae to temperature calculation did not converge!"
     !   print*, "Thetae ", thetaeK, "Pressure ", pres
     !END IF

  END FUNCTION The2T



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    VirtualTemperature
  !~
  !~ Description:
  !~    This function returns virtual temperature given temperature ( K )
  !~    and mixing ratio.
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION VirtualTemperature ( tK, w ) result ( Tv )
  IMPLICIT NONE

     !~ Variable declaration
     real, intent ( in ) :: tK !~ Temperature
     real, intent ( in ) :: w  !~ Mixing ratio ( kg kg^-1 )
     real                :: Tv !~ Virtual temperature

     Tv = tK * ( 1.0 + (w/0.622) ) / ( 1.0 + w )

  END FUNCTION VirtualTemperature




  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    SaturationMixingRatio
  !~
  !~ Description:
  !~    This function calculates saturation mixing ratio given the
  !~    temperature ( K ) and the ambient pressure ( Pa ).  Uses 
  !~    approximation of saturation vapor pressure.
  !~
  !~ References:
  !~    Bolton (1980), Monthly Weather Review, pg. 1047, Eq. 10
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION SaturationMixingRatio ( tK, p ) result ( ws )

    IMPLICIT NONE

    REAL, INTENT ( IN ) :: tK
    REAL, INTENT ( IN ) :: p
    REAL                :: ws

    REAL :: es

    es = 6.122 * exp ( (17.67*(tK-273.15))/ (tK-29.66) )
    ws = ( 0.622*es ) / ( (p/100.0)-es )

  END FUNCTION SaturationMixingRatio



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                     
  !~ Name:                                                                
  !~    tlcl                                                               
  !~                                                                        
  !~ Description:                                                            
  !~    This function calculates the temperature of a parcel of air would have
  !~    if lifed dry adiabatically to it's lifting condensation level (lcl).  
  !~                                                                          
  !~ References:                                                              
  !~    Bolton (1980), Monthly Weather Review, pg. 1048, Eq. 22
  !~                                                                          
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION TLCL ( tk, rh )
    
    IMPLICIT NONE
 
    REAL, INTENT ( IN ) :: tK   !~ Temperature ( K )
    REAL, INTENT ( IN ) :: rh   !~ Relative Humidity ( % )
    REAL                :: tlcl
    
    REAL :: denom, term1, term2

    term1 = 1.0 / ( tK - 55.0 )
    IF ( rh > REAL (0) ) THEN
      term2 = ( LOG (rh/100.0)  / 2840.0 )
    ELSE
      term2 = ( LOG (0.001/1.0) / 2840.0 )
    END IF
    denom = term1 - term2
    tlcl = ( 1.0 / denom ) + REAL ( 55 ) 

  END FUNCTION TLCL



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    PWat
  !~
  !~ Description:
  !~    This function calculates precipitable water by summing up the 
  !~    water vapor in a column from the first eta layer to model top
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION Pwat  ( nz, qv, qc, dz8w, rho )

    IMPLICIT NONE

     !~ Variable declaration
     !  --------------------
     INTEGER, INTENT ( IN ) :: nz          !~ Number of vertical levels
     REAL, INTENT ( IN )    :: qv   ( nz ) !~ Specific humidity in layer (kg/kg)
     REAL, INTENT ( IN )    :: qc   ( nz ) !~ Cloud water in layer (kg/kg)
     REAL, INTENT ( IN )    :: dz8w ( nz ) !~ Dist between vertical levels (m)
     REAL, INTENT ( IN )    :: rho  ( nz ) !~ Air density (kg/m^3)
     REAL                   :: Pwat        !~ Precipitable water (kg/m^2)
     INTEGER                :: k           !~ Vertical index

     !~ Precipitable water (kg/m^2)
     !  ---------------------------
     Pwat=0
     DO k = 1, nz
       Pwat = Pwat + (qv(k) + qc(k)) * dz8w(k) * rho(k)
     ENDDO
             
  END FUNCTION Pwat
 


  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                          ~!
  !~ Name:                                                                    ~!
  !~    Buoyancy                                                              ~!
  !~                                                                          ~!
  !~ Description:                                                             ~!
  !~    This function computes Convective Available Potential Energy (CAPE)   ~!
  !~    with inhibition as a result of water loading given the data required  ~!
  !~    to run up a sounding.                                                 ~!
  !~                                                                          ~!
  !~    Additionally, since we are running up a sounding anyways, this        ~!
  !~    function returns the height of the Level of Free Convection (LFC) and ~!
  !~    the pressure at the LFC.  That-a-ways, we don't have to run up a      ~!
  !~    sounding later, saving a relatively computationally expensive         ~!
  !~    routine.                                                              ~!
  !~                                                                          ~!
  !~ Usage:                                                                   ~!
  !~    ostat = Buoyancy ( tK, rh, p, hgt, sfc, CAPE, ZLFC, PLFC, parcel )    ~!
  !~                                                                          ~!
  !~ Where:                                                                   ~!
  !~                                                                          ~!
  !~    IN                                                                    ~!
  !~    --                                                                    ~!
  !~    tK   = Temperature ( K )                                              ~!
  !~    rh   = Relative Humidity ( % )                                        ~!
  !~    p    = Pressure ( Pa )                                                ~!
  !~    hgt  = Geopotential heights ( m )                                     ~!
  !~    sfc  = integer rank within submitted arrays that represents the       ~!
  !~           surface                                                        ~!
  !~                                                                          ~!
  !~    OUT                                                                   ~!
  !~    ---                                                                   ~!
  !~    ostat         INTEGER return status. Nonzero is bad.                  ~!
  !~    CAPE ( J/kg ) Convective Available Potential Energy                   ~!
  !~    ZLFC ( gpm )  Height at the LFC                                       ~!
  !~    PLFC ( Pa )   Pressure at the LFC                                     ~!
  !~                                                                          ~!
  !~    tK, rh, p, and hgt are all REAL arrays, arranged from lower levels    ~!
  !~    to higher levels.                                                     ~!
  !~                                                                          ~!
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
    FUNCTION Buoyancy ( nz, tk, rh, p, hgt, sfc, cape, cin, zlfc, plfc, lidx,  &
                        parcel ) result (ostat)
  
      IMPLICIT NONE
  
      INTEGER, INTENT ( IN )  :: nz          !~ Number of vertical levels
      INTEGER, INTENT ( IN )  :: sfc         !~ Surface level in the profile
      REAL,    INTENT ( IN )  :: tk   ( nz ) !~ Temperature profile ( K )
      REAL,    INTENT ( IN )  :: rh   ( nz ) !~ Relative Humidity profile ( % )
      REAL,    INTENT ( IN )  :: p    ( nz ) !~ Pressure profile ( Pa )
      REAL,    INTENT ( IN )  :: hgt  ( nz ) !~ Height profile ( gpm )
      REAL,    INTENT ( OUT ) :: cape        !~ CAPE ( J kg^-1 )
      REAL,    INTENT ( OUT ) :: cin         !~ CIN  ( J kg^-1 )
      REAL,    INTENT ( OUT ) :: zlfc        !~ LFC Height ( gpm )
      REAL,    INTENT ( OUT ) :: plfc        !~ LFC Pressure ( Pa )
      REAL,    INTENT ( OUT ) :: lidx        !~ Lifted index
      INTEGER                 :: ostat       !~ Function return status
                                             !~ Nonzero is bad.

      INTEGER, INTENT ( IN  ) :: parcel      !~ Most Unstable = 1 (default)
                                             !~ Mean layer    = 2
                                             !~ Surface based = 3
  
      !~ Derived profile variables
      !  -------------------------
      REAL                    :: ws   ( nz ) !~ Saturation mixing ratio
      REAL                    :: w    ( nz ) !~ Mixing ratio
      REAL                    :: dTvK ( nz ) !~ Parcel / ambient Tv difference
      REAL                    :: buoy ( nz ) !~ Buoyancy
      REAL                    :: tlclK       !~ LCL temperature ( K )
      REAL                    :: plcl        !~ LCL pressure ( Pa )
      REAL                    :: nbuoy       !~ Negative buoyancy
      REAL                    :: pbuoy       !~ Positive buoyancy
  
      !~ Source parcel information
      !  -------------------------
      REAL                    :: srctK       !~ Source parcel temperature ( K )
      REAL                    :: srcrh       !~ Source parcel rh ( % )
      REAL                    :: srcws       !~ Source parcel sat. mixing ratio
      REAL                    :: srcw        !~ Source parcel mixing ratio
      REAL                    :: srcp        !~ Source parcel pressure ( Pa )
      REAL                    :: srctheta    !~ Source parcel theta ( K )
      REAL                    :: srcthetaeK  !~ Source parcel theta-e ( K )
      INTEGER                 :: srclev      !~ Level of the source parcel
      REAL                    :: spdiff      !~ Pressure difference
   
      !~ Parcel variables
      !  ----------------
      REAL                    :: ptK        !~ Parcel temperature ( K )
      REAL                    :: ptvK       !~ Parcel virtual temperature ( K )
      REAL                    :: tvK        !~ Ambient virtual temperature ( K )
      REAL                    :: pw         !~ Parcel mixing ratio
  
      !~ Other utility variables
      !  -----------------------
      INTEGER                 :: i, j, k    !~ Dummy iterator
      INTEGER                 :: lfclev     !~ Level of LFC
      INTEGER                 :: prcl       !~ Internal parcel type indicator
      INTEGER                 :: mlev       !~ Level for ML calculation
      INTEGER                 :: lyrcnt     !~ Number of layers in mean layer
      LOGICAL                 :: flag       !~ Dummy flag
      LOGICAL                 :: wflag      !~ Saturation flag
      REAL                    :: freeze     !~ Water loading multiplier
      REAL                    :: pdiff      !~ Pressure difference between levs 
      REAL                    :: pm, pu, pd !~ Middle, upper, lower pressures
      REAL                    :: lidxu      !~ Lifted index at upper level
      REAL                    :: lidxd      !~ Lifted index at lower level
  
      !~ Thermo / dynamical constants
      !  ----------------------------
      REAL                    :: Rd         !~ Dry gas constant
         PARAMETER ( Rd = 287.058 )         !~ J deg^-1 kg^-1
      REAL                    :: Cp         !~ Specific heat constant pressure
         PARAMETER ( Cp = 1004.67 )         !~ J deg^-1 kg^-1
      REAL                    :: g          !~ Acceleration due to gravity
         PARAMETER ( g  = 9.80665 )         !~ m s^-2
      REAL                    :: RUNDEF
         PARAMETER ( RUNDEF = -9.999E30 )
  
      !~ Initialize variables
      !  --------------------
      ostat  = 0
      CAPE   = REAL ( 0 )
      CIN    = REAL ( 0 )
      ZLFC   = RUNDEF
      PLFC   = RUNDEF
  
      !~ Look for submitted parcel definition
      !~ 1 = Most unstable
      !~ 2 = Mean layer
      !~ 3 = Surface based
      !  -------------------------------------
      IF ( parcel > 3 .or. parcel < 1 ) THEN
         prcl = 1
      ELSE
         prcl =  parcel
      END IF
  
      !~ Initalize our parcel to be (sort of) surface based.  Because of
      !~ issues we've been observing in the WRF model, specifically with
      !~ excessive surface moisture values at the surface, using a true
      !~ surface based parcel is resulting a more unstable environment
      !~ than is actually occuring.  To address this, our surface parcel
      !~ is now going to be defined as the parcel between 25-50 hPa
      !~ above the surface. UPDATE - now that this routine is in WRF,
      !~ going to trust surface info. GAC 20140415
      !  ----------------------------------------------------------------
  
      !~ Compute mixing ratio values for the layer
      !  -----------------------------------------
      DO k = sfc, nz
        ws  ( k )   = SaturationMixingRatio ( tK(k), p(k) )
        w   ( k )   = ( rh(k)/100.0 ) * ws ( k )
      END DO
  
      srclev      = sfc
      srctK       = tK    ( sfc )
      srcrh       = rh    ( sfc )
      srcp        = p     ( sfc )
      srcws       = ws    ( sfc )
      srcw        = w     ( sfc )
      srctheta    = Theta ( tK(sfc), p(sfc)/100.0 )
   
      !~ Compute the profile mixing ratio.  If the parcel is the MU parcel,
      !~ define our parcel to be the most unstable parcel within the lowest
      !~ 180 mb.
      !  -------------------------------------------------------------------
      mlev = sfc + 1
      DO k = sfc + 1, nz
   
         !~ Identify the last layer within 100 hPa of the surface
         !  -----------------------------------------------------
         pdiff = ( p (sfc) - p (k) ) / REAL ( 100 )
         IF ( pdiff <= REAL (100) ) mlev = k

         !~ If we've made it past the lowest 180 hPa, exit the loop
         !  -------------------------------------------------------
         IF ( pdiff >= REAL (180) ) EXIT

         IF ( prcl == 1 ) THEN
            !IF ( (p(k) > 70000.0) .and. (w(k) > srcw) ) THEN
            IF ( (w(k) > srcw) ) THEN
               srctheta = Theta ( tK(k), p(k)/100.0 )
               srcw = w ( k )
               srclev  = k
               srctK   = tK ( k )
               srcrh   = rh ( k )
               srcp    = p  ( k )
            END IF
         END IF
   
      END DO
   
      !~ If we want the mean layer parcel, compute the mean values in the
      !~ lowest 100 hPa.
      !  ----------------------------------------------------------------
      lyrcnt =  mlev - sfc + 1
      IF ( prcl == 2 ) THEN
   
         srclev   = sfc
         srctK    = SUM ( tK (sfc:mlev) ) / REAL ( lyrcnt )
         srcw     = SUM ( w  (sfc:mlev) ) / REAL ( lyrcnt )
         srcrh    = SUM ( rh (sfc:mlev) ) / REAL ( lyrcnt )
         srcp     = SUM ( p  (sfc:mlev) ) / REAL ( lyrcnt )
         srctheta = Theta ( srctK, srcp/100. )
   
      END IF
   
      srcthetaeK = Thetae ( srctK, srcp/100.0, srcrh, srcw )
   
      !~ Calculate temperature and pressure of the LCL
      !  ---------------------------------------------
      tlclK = TLCL ( tK(srclev), rh(srclev) )
      plcl  = p(srclev) * ( (tlclK/tK(srclev))**(Cp/Rd) )
   
      !~ Now lift the parcel
      !  -------------------
   
      buoy  = REAL ( 0 )
      pw    = srcw
      wflag = .false.
      DO k  = srclev, nz
         IF ( p (k) <= plcl ) THEN
   
            !~ The first level after we pass the LCL, we're still going to
            !~ lift the parcel dry adiabatically, as we haven't added the
            !~ the required code to switch between the dry adiabatic and moist
            !~ adiabatic cooling.  Since the dry version results in a greater
            !~ temperature loss, doing that for the first step so we don't over
            !~ guesstimate the instability.
            !  ----------------------------------------------------------------
   
            IF ( wflag ) THEN
               flag  = .false.
   
               !~ Above the LCL, our parcel is now undergoing moist adiabatic
               !~ cooling.  Because of the latent heating being undergone as
               !~ the parcel rises above the LFC, must iterative solve for the
               !~ parcel temperature using equivalant potential temperature,
               !~ which is conserved during both dry adiabatic and
               !~ pseudoadiabatic displacements.
               !  --------------------------------------------------------------
               ptK   = The2T ( srcthetaeK, p(k), flag )
   
               !~ Calculate the parcel mixing ratio, which is now changing
               !~ as we condense moisture out of the parcel, and is equivalent
               !~ to the saturation mixing ratio, since we are, in theory, at
               !~ saturation.
               !  ------------------------------------------------------------
               pw = SaturationMixingRatio ( ptK, p(k) )
   
               !~ Now we can calculate the virtual temperature of the parcel
               !~ and the surrounding environment to assess the buoyancy.
               !  ----------------------------------------------------------
               ptvK  = VirtualTemperature ( ptK, pw )
               tvK   = VirtualTemperature ( tK (k), w (k) )
   
               !~ Modification to account for water loading
               !  -----------------------------------------
               freeze = 0.033 * ( 263.15 - pTvK )
               IF ( freeze > 1.0 ) freeze = 1.0
               IF ( freeze < 0.0 ) freeze = 0.0
   
               !~ Approximate how much of the water vapor has condensed out
               !~ of the parcel at this level
               !  ---------------------------------------------------------
               freeze = freeze * 333700.0 * ( srcw - pw ) / 1005.7
   
               pTvK = pTvK - pTvK * ( srcw - pw ) + freeze
               dTvK ( k ) = ptvK - tvK
               buoy ( k ) = g * ( dTvK ( k ) / tvK )
   
            ELSE
   
               !~ Since the theta remains constant whilst undergoing dry
               !~ adiabatic processes, can back out the parcel temperature
               !~ from potential temperature below the LCL
               !  --------------------------------------------------------
               ptK   = srctheta / ( 100000.0/p(k) )**(Rd/Cp)
   
               !~ Grab the parcel virtual temperture, can use the source
               !~ mixing ratio since we are undergoing dry adiabatic cooling
               !  ----------------------------------------------------------
               ptvK  = VirtualTemperature ( ptK, srcw )
   
               !~ Virtual temperature of the environment
               !  --------------------------------------
               tvK   = VirtualTemperature ( tK (k), w (k) )
   
               !~ Buoyancy at this level
               !  ----------------------
               dTvK ( k ) = ptvK - tvK
               buoy ( k ) = g * ( dtvK ( k ) / tvK )
   
               wflag = .true.
   
            END IF
   
         ELSE
   
            !~ Since the theta remains constant whilst undergoing dry
            !~ adiabatic processes, can back out the parcel temperature
            !~ from potential temperature below the LCL
            !  --------------------------------------------------------
            ptK   = srctheta / ( 100000.0/p(k) )**(Rd/Cp)
   
            !~ Grab the parcel virtual temperture, can use the source
            !~ mixing ratio since we are undergoing dry adiabatic cooling
            !  ----------------------------------------------------------
            ptvK  = VirtualTemperature ( ptK, srcw )
   
            !~ Virtual temperature of the environment
            !  --------------------------------------
            tvK   = VirtualTemperature ( tK (k), w (k) )
   
            !~ Buoyancy at this level
            !  ---------------------
            dTvK ( k ) = ptvK - tvK
            buoy ( k ) = g * ( dtvK ( k ) / tvK )
   
         END IF

         !~ Chirp
         !  -----
  !          WRITE ( *,'(I15,6F15.3)' )k,p(k)/100.,ptK,pw*1000.,ptvK,tvK,buoy(k)
   
      END DO
   
      !~ Add up the buoyancies, find the LFC
      !  -----------------------------------
      flag   = .false.
      lfclev = -1
      nbuoy  = REAL ( 0 )
      pbuoy = REAL ( 0 )
      DO k = sfc + 1, nz
         IF ( tK (k) < 253.15 ) EXIT
         CAPE = CAPE + MAX ( buoy (k), 0.0 ) * ( hgt (k) - hgt (k-1) )
         CIN  = CIN  + MIN ( buoy (k), 0.0 ) * ( hgt (k) - hgt (k-1) )
   
         !~ If we've already passed the LFC
         !  -------------------------------
         IF ( flag .and. buoy (k) > REAL (0) ) THEN
            pbuoy = pbuoy + buoy (k)
         END IF
   
         !~ We are buoyant now - passed the LFC
         !  -----------------------------------
         IF ( .not. flag .and. buoy (k) > REAL (0) .and. p (k) < plcl ) THEN
            flag = .true.
            pbuoy = pbuoy + buoy (k)
            lfclev = k
         END IF
   
         !~ If we think we've passed the LFC, but encounter a negative layer
         !~ start adding it up.
         !  ----------------------------------------------------------------
         IF ( flag .and. buoy (k) < REAL (0) ) THEN
            nbuoy = nbuoy + buoy (k)

            !~ If the accumulated negative buoyancy is greater than the
            !~ positive buoyancy, then we are capped off.  Got to go higher
            !~ to find the LFC. Reset positive and negative buoyancy summations
            !  ----------------------------------------------------------------
            IF ( ABS (nbuoy) > pbuoy ) THEN
               flag   = .false.
               nbuoy  = REAL ( 0 )
               pbuoy  = REAL ( 0 )
               lfclev = -1
            END IF
         END IF

      END DO

      !~ Calculate lifted index by interpolating difference between
      !~ parcel and ambient Tv to 500mb.
      !  ----------------------------------------------------------
      DO k = sfc + 1, nz

         pm = 50000.
         pu = p ( k )
         pd = p ( k - 1 )

         !~ If we're already above 500mb just set lifted index to 0.
         !~ --------------------------------------------------------
         IF ( pd .le. pm ) THEN
            lidx = 0.
            EXIT
   
         ELSEIF ( pu .le. pm .and. pd .gt. pm) THEN

            !~ Found trapping pressure: up, middle, down.
            !~ We are doing first order interpolation.  
            !  ------------------------------------------
            lidxu = -dTvK ( k ) * ( pu / 100000. ) ** (Rd/Cp)
            lidxd = -dTvK ( k-1 ) * ( pd / 100000. ) ** (Rd/Cp)
            lidx = ( lidxu * (pm-pd) + lidxd * (pu-pm) ) / (pu-pd)
            EXIT

         ENDIF

      END DO
   
      !~ Assuming the the LFC is at a pressure level for now
      !  ---------------------------------------------------
      IF ( lfclev > 0 ) THEN
         PLFC = p   ( lfclev )
         ZLFC = hgt ( lfclev )
      END IF
   
      IF ( PLFC /= PLFC .OR. PLFC < REAL (0) ) THEN
         PLFC = REAL ( -1 )
         ZLFC = REAL ( -1 )
      END IF
   
      IF ( CAPE /= CAPE ) cape = REAL ( 0 )
   
      IF ( CIN  /= CIN  ) cin  = REAL ( 0 )

      !~ Chirp
      !  -----
  !       WRITE ( *,* ) ' CAPE: ', cape, ' CIN:  ', cin
  !       WRITE ( *,* ) ' LFC:  ', ZLFC, ' PLFC: ', PLFC
  !       WRITE ( *,* ) ''
  !       WRITE ( *,* ) ' Exiting buoyancy.'
  !       WRITE ( *,* ) ' ==================================== '
  !       WRITE ( *,* ) ''
   
  END FUNCTION Buoyancy 



!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    NGMSLP      NMC SEA LEVEL PRESSURE REDUCTION
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-02-02
!
! ABSTRACT:
!
!     THIS ROUTINE COMPUTES SEA LEVEL PRESSURE USING THE
!     HYDROSTATIC EQUATION WITH THE SHUELL CORRECTION.  THE
!     FOLLOWING IS BASED ON DOCUMENTATION IN SUBROUTINE
!     OUTHYDRO OF THE NGM:
!
!     THE FUNDAMENTAL HYDROSTATIC EQUATION IS
!        D(HEIGHT)
!        ---------  =  TAU = VIRTUAL TEMPERATURE * (RGAS/GRAVITY)
!        D (Z)
!      WHERE
!        Z = MINUS LOG OF PRESSURE (-LN(P)).
!
!     SEA-LEVEL PRESSURE IS COMPUTED FROM THE FORMULA
!        PRESS(MSL) = PRESS(GROUND) * EXP( F)
!     WHERE
!        F        = HEIGHT OF GROUND / MEAN TAU
!        MEAN TAU = ( TAU(GRND) + TAU(SL) ) / 2
!
!     IN THE NGM TAU(GRND) AND TAU(SL) ARE FIRST SET USING A
!     6.5DEG/KM LAPSE RATE FROM LOWEST MDL LEVEL.  THIS IS MODIFIED
!     BY A CORRECTION BASED ON THE CRITICAL TAU OF THE SHUELL
!     CORRECTION:
!                  TAUCR=(RGASD/GRAVITY) * 290.66
!  
!     1) WHERE ONLY TAU(SL) EXCEEDS TAUCR, CHANGE TAU(SL) TO TAUCR.
!
!     2) WHERE BOTH TAU(SL) AND TAU(GRND) EXCEED TAUCR,
!        CHANGE TAU(SL) TO TAUCR-CONST*(TAU(GRND)-TAUCR  )**2
!        WHERE CONST = .005 (GRAVITY/RGASD)
!  
!     THE AVERAGE OF TAU(SL) AND TAU(GRND) IS THEN USED TOGETHER
!     WITH THE GROUND HEIGHT AND PRESSURE TO DERIVE THE PRESSURE
!     AT SEA LEVEL.
!    
!     HEIGHT OF THE 1000MB SURFACE IS COMPUTED FROM THE MSL PRESSURE
!     FIELD USING THE FORMULA:
!    
!       P(MSL) - P(1000MB) = MEAN DENSITY * GRAVITY * HGT(1000MBS)
!    
!     WHERE P(MSL) IS THE SEA LEVEL PRESSURE FIELD WE HAVE JUST
!     COMPUTED.
!    
!
!     MEB 6/13/02: THIS CODE HAS BEEN SIMPLIFIED CONSIDERABLY FROM
!     THE ONE USED IN ETAPOST.  HORIZONTAL SMOOTHING HAS BEEN
!     REMOVED AND THE FIRST MODEL LEVEL IS USED RATHER
!     THAN THE MEAN OF THE VIRTUAL TEMPERATURES IN
!     THE LOWEST 30MB ABOVE GROUND TO COMPUTE TAU(GRND).
!    
!   . 
!    
! PROGRAM HISTORY LOG:
!   93-02-02  RUSS TREADON
!   98-06-08  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   01-11-02  H CHUANG - MODIFIED LINE 234 FOR COMPUTATION OF
!                         SIGMA/HYBRID SLP
!   01-12-18  H CHUANG - INCLUDED SMOOTHING ALONG BOUNDARIES TO BE
!                         CONSISTENT WITH MESINGER SLP
!   02-06-13  MIKE BALDWIN - WRF VERSION
!   06-12-18  H CHUANG - BUG FIX TO CORRECT TAU AT SFC
!   14-04-17  G CREIGHTON - MODIFIED TO INSERT INTO AFWA DIAGNOSTICS IN WRF
!    
!$$$ 

  FUNCTION MSLP ( zsfc, psfc, zlev1, qlev1, tlev1 )

      implicit none
     
     
!     DECLARE VARIABLES

      REAL,    INTENT ( IN )  :: zsfc         !~ Surface height ( m )
      REAL,    INTENT ( IN )  :: psfc         !~ Surface height ( m )
      REAL,    INTENT ( IN )  :: zlev1        !~ Level 1 height ( m )
      REAL,    INTENT ( IN )  :: qlev1        !~ Level 1 mixing ratio ( kg/kg )
      REAL,    INTENT ( IN )  :: tlev1        !~ Level 1 temperature ( K )
      real,PARAMETER :: G=9.81
      real,PARAMETER :: GI=1./G
      real,PARAMETER :: RD=287.0
      real,PARAMETER :: ZSL=0.0
      real,PARAMETER :: TAUCR=RD*GI*290.66,CONST=0.005*G/RD
      real,PARAMETER :: GORD=G/RD,DP=60.E2
      real,PARAMETER :: GAMMA=6.5E-3

      real MSLP,TVRT,TVRSFC,TAUSFC,TVRSL,TAUSL,TAUAVG
!    
!**********************************************************************
!     START NGMSLP HERE.
!
         MSLP = PSFC
!
!        COMPUTE LAYER TAU (VIRTUAL TEMP*RD/G).
         TVRT = TLEV1*(1.0+0.608*QLEV1)
         !TAU  = TVRT*RD*GI
!    
!        COMPUTE TAU AT THE GROUND (Z=ZSFC) AND SEA LEVEL (Z=0)
!        ASSUMING A CONSTANT LAPSE RATE OF GAMMA=6.5DEG/KM.
         TVRSFC = TVRT + (ZLEV1 - ZSFC)*GAMMA
         TAUSFC = TVRSFC*RD*GI
         TVRSL  = TVRT + (ZLEV1 - ZSL)*GAMMA
         TAUSL  = TVRSL*RD*GI
!    
!        IF NEED BE APPLY SHEULL CORRECTION.
         IF ((TAUSL.GT.TAUCR).AND.(TAUSFC.LE.TAUCR)) THEN
            TAUSL=TAUCR
         ELSEIF ((TAUSL.GT.TAUCR).AND.(TAUSFC.GT.TAUCR)) THEN
            TAUSL = TAUCR-CONST*(TAUSFC-TAUCR)**2
         ENDIF
!    
!        COMPUTE MEAN TAU.
         TAUAVG = 0.5*(TAUSL+TAUSFC)
!    
!        COMPUTE SEA LEVEL PRESSURE.
         MSLP = PSFC*EXP(ZSFC/TAUAVG)

  END FUNCTION MSLP



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                          ~!
  !~ Name:                                                                    ~!
  !~    calc_fits                                                             ~!
  !~                                                                          ~!
  !~ Description:                                                             ~!
  !~    This function computes Fighter Index Thermal Stress values given      ~!
  !~    dry bulb temperature, relative humidity, and pressure.                ~!
  !~                                                                          ~!
  !~ Usage:                                                                   ~!
  !~    fitsval = calc_fits ( p, tK, rh )                                     ~!
  !~                                                                          ~!
  !~ Where:                                                                   ~!
  !~    p   = Pressure ( Pa )                                                 ~!
  !~    tK  = Temperature ( K )                                               ~!
  !~    rh  = Relative Humidity ( % )                                         ~!
  !~                                                                          ~!
  !~ Reference:                                                               ~!
  !~    Stribley, R.F., S. Nunneley, 1978: Fighter Index of Thermal Stress:   ~!
  !~    Development of interim guidance for hot-weather USAF operations.      ~!
  !~    SAM-TR-78-6. Eqn. 9                                                   ~!
  !~                                                                          ~!
  !~    Formula:                                                              ~!
  !~       FITS = 0.8281*Twb + 0.3549*Tdb + 5.08 (degrees Celsius)            ~!
  !~                                                                          ~!
  !~    Where:                                                                ~!
  !~       Twb = Wet Bulb Temperature                                         ~!
  !~       Tdb = Dry Bulb Temperature                                         ~!
  !~                                                                          ~!
  !~ Written:                                                                 ~!
  !~    Scott Rentschler, Software Engineering Services                       ~!
  !~    Fine Scale Models Team                                                ~!
  !~    Air Force Weather Agency, 16WS/WXN                                    ~!
  !~    DSN: 271-3331 Comm: (402) 294-3331                                    ~!
  !~    scott.rentschler@offutt.af.mil                                        ~!
  !~                                                                          ~!
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION calc_fits ( p, tK, rh ) RESULT ( fits )
 
    implicit none

    !~ Variable declaration
    !  --------------------
    real, intent ( in ) :: p               !~ Pressure ( Pa )
    real, intent ( in ) :: tK              !~ Temperature ( K )
    real, intent ( in ) :: rh              !~ Rel Humidity ( % )
    real                :: fits            !~ FITS index value
 
    !~ Utility variables
    !  --------------------------
    real                :: twb             !~ Wet bulb temperature ( C )
    real                :: wbt
 
    ! ---------------------------------------------------------------------- !
    ! ---------------------------------------------------------------------- !

    !~ Initialize variables
    !  --------------------
    fits = REAL ( 0 )

    !~ Get the wet bulb temperature in degrees Celsius
    !  -----------------------------------------------
    twb =  WetBulbTemp ( p, tK, rh ) - 273.15 

    !~ Compute the FITS index
    !  ----------------------
    fits = 0.8281*twb + 0.3549*( tK - 273.15 ) + 5.08
 
    !~ Convert the index to Kelvin
    !  ---------------------------
    fits = fits + 273.15

  END FUNCTION calc_fits



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    calc_wc   
  !~
  !~ Description:
  !~    This function calculates wind chill given temperature ( K ) and
  !~    wind speed ( m/s )
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION calc_wc ( tK, wspd ) RESULT ( wc )

    implicit none

    !~ Variable Declarations
    !  ---------------------
    real, intent ( in  ) :: tK
    real, intent ( in  ) :: wspd

    real                 :: tF, wc, wspd_mph

    wspd_mph = wspd * 2.23693629 ! convert to mph
    tF  = (( tK - 273.15 ) * ( REAL (9) / REAL (5) ) ) + REAL ( 32 )

    wc =    35.74                           &
       +  (  0.6215 * tF                  ) &
       -  ( 35.75   *      ( wspd_mph**0.16 ) ) &
       +  (  0.4275 * tF * ( wspd_mph**0.16 ) )

    wc = (( wc - REAL (32) ) * ( REAL (5) / REAL (9) ) ) + 273.15

  END FUNCTION calc_wc



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    calc_hi   
  !~
  !~ Description:
  !~    This subroutine calculates the heat index.  Requires temperature ( K )
  !~    and relative humidity ( % ).
  !~ 
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION calc_hi ( Tk, RH ) result ( HI )

    implicit none

    !~ Variable declarations
    !  ---------------------
    real, intent ( in  ) :: Tk
    real, intent ( in  ) :: RH

    real :: tF, tF2, rh2, HI

    !~ If temperature > 70F then calculate heat index, else set it equal
    !~ to dry temperature
    !  -----------------------------------------------------------------
    IF ( Tk > 294.26111 ) THEN

      tF   = ( (Tk - 273.15) * (REAL (9)/REAL (5))  ) + REAL ( 32 )
      tF2  = tF ** 2
      rh2  = RH ** 2

      HI =  -42.379 &
         +  (  2.04901523   * tF              ) &
         +  ( 10.14333127   * RH              ) &
         -  (  0.22475541   * tF  * RH        ) &
         -  (  6.83783E-03  * tF2             ) &
         -  (  5.481717E-02 * rh2             ) &
         +  (  1.22874E-03  * tF2 * RH        ) &
         +  (  8.5282E-04   * tF  * rh2       ) &
         -  (  1.99E-06     * tF2 * rh2       )

      HI = ((HI - REAL (32)) * (REAL (5)/REAL (9))) + 273.15
    ELSE
      HI = Tk
    END IF

  END FUNCTION calc_hi

  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                          ~!
  !~ Name:                                                                    ~!
  !~    WetBulbTemp                                                           ~!
  !~                                                                          ~!
  !~ Description:                                                             ~!
  !~    This function approximates the Wet Bulb Temperature (K) provided      ~!
  !~    dry bulb temperature (K), relative humidity (%), and pressure (Pa).   ~!
  !~                                                                          ~!
  !~ Usage:                                                                   ~!
  !~    wbt = WetBulbTemperature ( p, tK, rh )                                ~!
  !~                                                                          ~!
  !~ Where:                                                                   ~!
  !~    p  = Pressure ( Pa )                                                  ~!
  !~    tK = Temperature ( K )                                                ~!
  !~    rh = Relative Humidity ( % )                                          ~!
  !~                                                                          ~!
  !~ Reference:                                                               ~!
  !~    American Society of Civil Engineers                                   ~!
  !~    Evapotraspiration and Irrigation Water Requirements                   ~!
  !~    Jensen et al (1990) ASCE Manual No. 70, pp 176-177                    ~!
  !~                                                                          ~!
  !~ Written:                                                                 ~!
  !~    Scott Rentschler, Software Engineering Services                       ~!
  !~    Fine Scale Models Team                                                ~!
  !~    Air Force Weather Agency                                              ~!
  !~    DSM: 271-3331 Comm: (402) 294-3331                                    ~!
  !~    scott.rentschler@offutt.af.mil                                        ~!
  !~                                                                          ~!
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION WetBulbTemp ( p, tK, rh) result( wbt )

    implicit none

    !~ Variable delclaration
    !  ---------------------
    real, intent ( in ) :: p        !~ Pressure ( Pa )
    real, intent ( in ) :: tK       !~ Temperature ( K )
    real, intent ( in ) :: rh       !~ Relative Humidity ( % )
    real                :: wbt      !~ Wet Bulb Temperature ( K )
 
    !~ Utility variables
    !  -----------------
    real                :: tdK      !~ Dewpoint temperature ( K )
    real                :: tC       !~ Temperature ( C )
    real                :: tdC      !~ Dewpoint temperature ( K )
    real                :: svapr    !~ Saturation vapor pressure ( Pa )
    real                :: vapr     !~ Ambient vapor pressure ( Pa )
    real                :: gamma    !~ Dummy term
    real                :: delta    !~ Dummy term

    ! ---------------------------------------------------------------------- !
    ! ---------------------------------------------------------------------- !          
    !~ Initialize variables
    !  --------------------
    wbt = REAL ( 0 )
    tC  = tK - 273.15

    !~ Compute saturation vapor pressure ( Pa )
    !  ----------------------------------------
    svapr = calc_es ( tK ) * REAL ( 100 )

    !~ Compute vapor pressure
    !  ----------------------
    vapr  = svapr * ( rh / REAL (100) )

    !~ Grab the dewpoint
    !  -----------------
    tdC = calc_Dewpoint ( tC, rh )
    tdK = tdC + 273.15

    !~ Compute dummy terms
    !  -------------------
    gamma = 0.00066 * ( p / REAL (1000) )
    delta = REAL ( 4098 ) * ( vapr / REAL(1000) )  / ( (tC+237.3)**2 )

    !~ Compute the wet bulb temperature
    !  --------------------------------
    wbt = ( ((gamma * tC) + (delta * tdC)) / (gamma + delta) ) + 273.15

  END FUNCTION WetBulbTemp


  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    calc_Dewpoint
  !~
  !~ Description:
  !~    This function approximates dewpoint given temperature and rh.
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION calc_Dewpoint ( tC, rh) result( Dewpoint )

    implicit none

    !~ Variable Declaration
    !  --------------------
    real, intent ( in ) :: tC
    real, intent ( in ) :: rh
    real                :: Dewpoint
 
    real :: term, es, e1, e, logs, expon

    expon    = ( 7.5*tC ) / ( 237.7+tC )
    es       = 6.112 * ( 10**expon )     ! Saturated vapor pressure
    e        = es * ( rh/100.0 )         ! Vapor pressure
    logs     = LOG10 ( e/6.112 )
    Dewpoint = ( 237.7*logs ) / ( 7.5-logs )

  END FUNCTION calc_Dewpoint


  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~
  !~ Name:
  !~    calc_es 
  !~
  !~ Description:
  !~    This function returns the saturation vapor pressure over water ( hPa )
  !~    given temperature ( K ).
  !~
  !~ References:
  !~    The algorithm is due to Nordquist, W.S., 1973: "Numerical approximations
  !~    of selected meteorological parameters for cloud physics problems,"
  !~    ecom-5475, Atmospheric Sciences Laboratory, U.S. Army Electronics
  !~    Command, White Sands Missile Range, New Mexico, 88002
  !~
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION calc_es ( tK ) result ( es )

    implicit none

    !~ Variable Declaration
    !  --------------------
    real, intent ( in ) :: tK
    real                :: es
 
    real                :: p1, p2, c1

    p1 = 11.344    - 0.0303998 * tK
    p2 = 3.49149   - 1302.8844 / tK
    c1 = 23.832241 - 5.02808   * ALOG10 ( tK )
    es = 10.**(c1-1.3816E-7*10.**p1+8.1328E-3*10.**p2-2949.076/tK)

  END FUNCTION calc_es



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                          ~!
  !~ Name:                                                                    ~!
  !~    CATTurbulence                                                         ~!
  !~                                                                          ~!
  !~ Description:                                                             ~!
  !~    This function calculates the turbulence index ( TI ) for one layer    ~!
  !~    in the atmosphere given the horizontal wind components and the geo-   ~!
  !~    potential height of two pressure levels.  The index is computed for   ~!
  !~    the layer between the levels using the deformation and convergence    ~!
  !~    of the wind field at the top and bottom of the layer and the vertical ~!
  !~    wind shear is calculated within the layer.  The equation used for     ~!
  !~    calculating TI is given by:                                           ~!
  !~                                                                          ~!
  !~                                                                          ~!
  !~       TI = VWS * ( DEF + CONV )                                          ~!
  !~                                                                          ~!
  !~    Where:                                                                ~!
  !~       VWS  = Vertical wind shear                                         ~!
  !~       DEF  = Deformation                                                 ~!
  !~       CONV = Convergence                                                 ~!
  !~                                                                          ~!
  !~ Notes:                                                                   ~!
  !~                                                                          ~!
  !~ References:                                                              ~!
  !~    Ellrod, G.P. and D.J. Knapp, An objective clear-air turbulence        ~!
  !~      forecasting technique: verification and operational use, Weather    ~!
  !~      and Forecasting, 7, March 1992, pp. 150-165.                        ~!
  !~                                                                          ~!
  !~ Written:                                                                 ~!
  !~    Scott Rentschler, Software Engineering Services                       ~!
  !~    Fine Scale Models Team                                                ~!
  !~    Air Force Weather Agency, 16WS/WXN                                    ~!
  !~    DSN: 271-3331 Comm: (402) 294-3331                                    ~!
  !~    scott.rentschler@offutt.af.mil                                        ~!
  !~                                                                          ~!
  !~ History:                                                                 ~!
  !~    1 February 2008 ................... Scott Rentschler, (SES), 2WG/WEA  ~!
  !~    INITIAL VERSION                                                       ~!
  !~                                                                          ~!
  !~    8 July 2009 ....................... Scott Rentschler, (SES), 2WG/WEA  ~!
  !~    Adapted for new driver.                                               ~!
  !~                                                                          ~!
  !~    1 November 2012 ......................... Scott Rentschler, 16WS/WXN  ~!
  !~    Modified to accept layer argument, which adds the flexibility to make ~!
  !~    the computation for whichever flight level is desired.  Cleaned up    ~!
  !~    some of the code and added a couple comments.                         ~!
  !~                                                                          ~!
  !~    28 August 2013 .................... Braedi Wickard, SEMS/NG/16WS/WXN  ~!
  !~    Adapted for use within the Unified Post Processor. UPP can not handle ~!
  !~    the layer argument for flight levels, so reverted to hardcoded levels ~!
  !~                                                                          ~!
  !~    25 April 2014 ............................. Glenn Creighton, 16WS/WXN ~!
  !~    Adapted for use within WRF. WRF already computes many of these terms. ~!
  !~    Stripped everything down to its bare bones to remove need to compute  ~!
  !~    horizontal terms, now using deformation variables already within WRF. ~!
  !~                                                                          ~!
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION CATTurbulence ( ugrdbot, ugrdtop, vgrdbot, vgrdtop &
                           ,defor11bot, defor11top, defor12bot, defor12top &
                           ,defor22bot, defor22top, zbot, ztop ) result ( ti )

    IMPLICIT NONE

    !~ Variable declarations
    !  ---------------------
    REAL,    INTENT ( IN )  :: ugrdbot       !~ U-wind bottom of layer
    REAL,    INTENT ( IN )  :: ugrdtop       !~ U-wind top of layer
    REAL,    INTENT ( IN )  :: vgrdbot       !~ V-wind bottom of layer
    REAL,    INTENT ( IN )  :: vgrdtop       !~ V-wind top of layer
    REAL,    INTENT ( IN )  :: defor11bot    !~ 2*du/dx at bottom of layer
    REAL,    INTENT ( IN )  :: defor11top    !~ 2*du/dx at top of layer
    REAL,    INTENT ( IN )  :: defor12bot    !~ du/dy+dv/dx at bottom of layer
    REAL,    INTENT ( IN )  :: defor12top    !~ du/dy+dv/dx at top of layer
    REAL,    INTENT ( IN )  :: defor22bot    !~ 2*dv/dy at bottom of layer
    REAL,    INTENT ( IN )  :: defor22top    !~ 2*dv/dy at top of layer
    REAL,    INTENT ( IN )  :: zbot          !~ Height grid bottom
    REAL,    INTENT ( IN )  :: ztop          !~ Height grid top
    REAL                    :: ti            !~ Turbulence index

    !~ Function utility variables
    !  --------------------------
    REAL    :: dudx, dudx1, dudx2 !~ Wind differentials
    REAL    :: dvdy, dvdy1, dvdy2
    REAL    :: dudz, dvdz

    REAL    :: depth, vws, conv    !~ Depth, vertical wind shear, convergence
    REAL    :: def, shear, stretch !~ Deformation, shear, stretching terms

    !~ Initialize variables.
    !  ----------------------
    ti = REAL ( 0 )

    !~ Compute vertical wind shear
    !  ---------------------------
    depth = ABS ( ztop - zbot )
    dudz  = ( ugrdbot - ugrdtop ) / depth
    dvdz  = ( vgrdbot - vgrdtop ) / depth
    vws   = SQRT ( dudz**2 + dvdz**2  )

    dudx1 = defor11top / 2.
    dudx2 = defor11bot / 2.
    dudx  = ( dudx1 + dudx2 ) / REAL ( 2 )

    dvdy1 = defor22top / 2.
    dvdy2 = defor22bot / 2.
    dvdy  = ( dvdy1 + dvdy2 ) / REAL ( 2 )

    !~ Compute the deformation
    !  -----------------------
    stretch = dudx - dvdy
    shear   = ( defor12top + defor12bot ) / REAL ( 2 )
    def     = SQRT ( stretch**2 + shear**2 )

    !~ Compute the convergence
    !  -----------------------
    conv    = - ( dudx + dvdy )

    !~ Compute the turbulence index
    !  ----------------------------
    ti = vws * ( def + conv ) * 1.0E+07

    IF ( ti /= ti ) ti = REAL ( 0 )
    IF ( ti < 0   ) ti = REAL ( 0 )

  END FUNCTION CATTurbulence



  FUNCTION lin_interp ( x, f, y ) result ( g )

    ! Purpose:
    !   interpolates f(x) to point y
    !   assuming f(x) = f(x0) + a * (x - x0)
    !   where a = ( f(x1) - f(x0) ) / (x1 - x0)
    !   x0 <= x <= x1
    !   assumes x is monotonically increasing

    ! Author: D. Fillmore ::  J. Done changed from r8 to r4
    ! Pilfered for AFWA diagnostics - G Creighton

    implicit none

    real, intent(in), dimension(:) :: x  ! grid points
    real, intent(in), dimension(:) :: f  ! grid function values
    real, intent(in) :: y                ! interpolation point
    real :: g                            ! interpolated function value      

    integer :: k  ! interpolation point index
    integer :: n  ! length of x
    real    :: a

    n = size(x)

    ! find k such that x(k) < y =< x(k+1)
    ! set k = 1 if y <= x(1)  and  k = n-1 if y > x(n)

    if (y <= x(1)) then
      k = 1
    else if (y >= x(n)) then
      k = n - 1
    else
      k = 1
      do while (y > x(k+1) .and. k < n)
        k = k + 1
      end do
    end if
    ! interpolate
    a = (  f(k+1) - f(k) ) / ( x(k+1) - x(k) )
    g = f(k) + a * (y - x(k))

  END FUNCTION lin_interp



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                           ~!
  !~ Name:                                                                     ~!
  !~    LLT_Windspeed                                                          ~!
  !~                                                                           ~!
  !~ Description:                                                              ~!
  !~    This function computes the dynamic term for the low-level turbulence   ~!
  !~    algorithm.                                                             ~!
  !~                                                                           ~!
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION LLT_Windspeed ( nlayer, u, v ) RESULT ( dynamic )
    IMPLICIT NONE
 
    !~ Variable Declaration
    !  --------------------
    INTEGER, INTENT ( IN )         :: nlayer
    REAL, INTENT ( IN )            :: u     ( nlayer )
    REAL, INTENT ( IN )            :: v     ( nlayer )
    REAL                           :: dynamic
 
    !~ Internal function variables
    !  ---------------------------
    INTEGER           :: i
    REAL              :: this_windspeed ( nlayer )
    REAL              :: PI
       PARAMETER ( PI = 3.14159265359 )

    !  --------------------------------------------------------------------  !
    !  --------------------------------------------------------------------  !           
    !~ Initialize variables
    !  --------------------
    dynamic = REAL ( 0 )

    !~ Compute the windspeed
    !  ---------------------
    DO i = 1, nlayer
       this_windspeed ( i ) = SQRT ( u(i)**2 + v(i)**2 )
    END DO

    !~ Compute the dynamic term
    !  -------------------------
    dynamic = ( this_windspeed(1)+this_windspeed(nlayer) ) / REAL (20)
    IF ( dynamic > REAL (2) ) dynamic = REAL ( 2 )
    dynamic = ( dynamic + REAL (3) ) / REAL ( 2 )
    dynamic = SIN ( dynamic*PI )
    dynamic = ( dynamic + REAL (1) ) / REAL ( 2 )


  END FUNCTION LLT_Windspeed


  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                           ~!
  !~ Name:                                                                     ~!
  !~    LLT_Thermodynamic                                                      ~!
  !~                                                                           ~!
  !~ Description:                                                              ~!
  !~    This function computes the thermodynamic term for the low-level        ~!
  !~    turbulence algorithm.                                                  ~!
  !~                                                                           ~!
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION LLT_Thermodynamic ( nlayer, tK, hgt ) RESULT ( thermodynamic )
  IMPLICIT NONE
 
    !~ Variable Declaration
    !  --------------------
    INTEGER, INTENT ( IN )         :: nlayer
    REAL, INTENT ( IN )            :: tK     ( nlayer ) !~ Temperature (K)
    REAL, INTENT ( IN )            :: hgt    ( nlayer ) !~ Heights ( m )
    REAL                           :: thermodynamic

    !~ Internal function variables
    !  ---------------------------
    INTEGER :: i
    REAL    :: lapse
    REAL    :: PI
       PARAMETER ( PI = 3.14159265359 )

    !  --------------------------------------------------------------------  !
    !  --------------------------------------------------------------------  !

    !~ Initialize variables
    !  --------------------
    thermodynamic = REAL ( 0 )

    !~ Compute the lapse rate over the layer.  The sign gets goofy here,
    !~ but works as coded below.
    !  -----------------------------------------------------------------
    lapse = ( tk(1) - tk(nlayer) ) * REAL ( 1000 )
    lapse = lapse / ( hgt(nlayer) - hgt(1) )

    !~ Compute the thermodynamic component
    !  -----------------------------------
    thermodynamic = lapse / REAL ( 10 )
    thermodynamic = ( thermodynamic + REAL (3) ) / REAL ( 2 )
    thermodynamic = SIN ( thermodynamic * PI )
    thermodynamic = ( thermodynamic + REAL (1) ) / REAL ( 2 )

  END FUNCTION LLT_Thermodynamic


  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                           ~!
  !~ Name:                                                                     ~!
  !~    LLT_MountainWave                                                       ~!
  !~                                                                           ~!
  !~ Description:                                                              ~!
  !~    This function computes the mountain wave term for the low-level        ~!
  !~    turbulence algorithm.                                                  ~!
  !~                                                                           ~!
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION LLT_MountainWave ( nlayer, tdx, tdy, u, v, tK, hgt) &
                                                         RESULT ( MountainWave )
    IMPLICIT NONE

    !~ Variable Declaration
    !  --------------------
    INTEGER, INTENT ( IN )           :: nlayer
    REAL, INTENT ( IN )              :: tdx               !~ Terrain dx
    REAL, INTENT ( IN )              :: tdy               !~ Terrain dy
    REAL, INTENT ( IN )              :: u   ( nlayer )    !~ U components f
    REAL, INTENT ( IN )              :: v   ( nlayer )    !~ V components
    REAL, INTENT ( IN )              :: tK  ( nlayer )    !~ Temperatures (K)
    REAL, INTENT ( IN )              :: hgt ( nlayer )    !~ Heights ( m )
    REAL                             :: MountainWave      !~ Mountain wave term
 
    !~ Internal function variables
    !  ---------------------------
    REAL    :: u_term
    REAL    :: v_term
    REAL    :: uv_term
    REAL    :: lapse
    REAL    :: total_mw, this_total_mw
    REAL    :: this_uv_term
    REAL    :: min_uv_term, cross_terrain, max_total_mw
    INTEGER :: i, j, k

    REAL    :: PI
       PARAMETER ( PI = 3.14159265359 )

    !  --------------------------------------------------------------------  !
    !  --------------------------------------------------------------------  !

    !~ Initialize variables
    !  --------------------
    MountainWave = REAL ( 0 )

    !~ Loop through the layer
    !  ----------------------
    DO i = 2, nlayer

      !~ Wind terrain term
      !  -----------------
      u_term       = ( (u(i-1) + u(i) ) / REAL(2) ) * tdx
      v_term       = ( (v(i-1) + v(i) ) / REAL(2) ) * tdy
      this_uv_term = ( u_term + v_term ) * REAL ( -1 )
      !IF ( uv_term < REAL (0) ) uv_term = REAL ( 0 )
      IF ( min_uv_term < REAL (0) ) min_uv_term = REAL ( 0 )
      IF ( i == 2 ) THEN
        !uv_term = this_uv_term
        min_uv_term = this_uv_term
      ELSE
        !IF ( this_uv_term < uv_term ) uv_term = this_uv_term
        IF ( this_uv_term < min_uv_term ) min_uv_term = this_uv_term
      END IF

      !~ Lapse rate
      !  ----------
      lapse = ( tK (i-1) - tK (i) ) * REAL ( 1000 )
      lapse  = lapse / ABS ( hgt(i)-hgt(i-1) )
      IF ( lapse > REAL (0) ) lapse = REAL ( 0 )
      lapse = lapse * REAL ( -1 )

      this_total_mw = this_uv_term * lapse * REAL ( 40000 )
      IF ( i == 2 ) THEN
        total_mw = this_total_mw
      ELSE
        IF ( this_total_mw > total_mw ) total_mw = this_total_mw
      END IF
 
    END DO

    !min_uv_term = uv_term
    cross_terrain = min_uv_term * REAL ( 500 )

    IF ( min_uv_term < 0.03 ) THEN
      cross_terrain = REAL ( 0 )
    END IF

    IF ( cross_terrain > REAL (50) ) cross_terrain = REAL ( 50 )

    !~ Multiply the lapse (inversion) array and the mountain wave array
    !  ----------------------------------------------------------------
    IF ( total_mw > REAL (50) ) total_mw = REAL ( 50 )

    !~ Add the cross terrain flow and inversion term
    !  ---------------------------------------------
    MountainWave = ( total_mw*(cross_terrain/50.) ) + cross_terrain
    MountainWave = MountainWave / REAL ( 100 )

  END FUNCTION LLT_MountainWave



  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  !~                                                                           ~!
  !~ Name:                                                                     ~!
  !~    LLT_TrappedWave                                                        ~!
  !~                                                                           ~!
  !~ Description:                                                              ~!
  !~    This function computes the trapped wave term for the low-level         ~!
  !~    turbulence algorithm.                                                  ~!
  !~                                                                           ~!
  !!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!!!
  FUNCTION LLT_TrappedWave ( nlayer, u, v, p ) RESULT ( trapped )
     IMPLICIT NONE

     !~ Variable Declaration
     !  --------------------
     INTEGER, INTENT ( IN )         :: nlayer
     REAL, INTENT ( IN )            :: u     ( nlayer )
     REAL, INTENT ( IN )            :: v     ( nlayer )
     REAL, INTENT ( IN )            :: p     ( nlayer )
     REAL                           :: trapped

     !~ Internal function variables
     !  ---------------------------
     INTEGER           :: i
     REAL              :: du, dv
     REAL              :: scale_fact, this_p
     REAL              :: dudv, this_dudv
     REAL              :: PI
        PARAMETER ( PI = 3.14159265359 )

     !~ Scale parameters
     !  ----------------
     REAL, PARAMETER :: scale_950 = 0.050000  !~ 1/20
     REAL, PARAMETER :: scale_925 = 0.040000  !~ 1/25
     REAL, PARAMETER :: scale_900 = 0.025000  !~ 1/40
     REAL, PARAMETER :: scale_850 = 0.010000  !~ 1/100
     REAL, PARAMETER :: scale_800 = 0.005000  !~ 1/200
     REAL, PARAMETER :: scale_750 = 0.002941  !~ 1/340
     REAL, PARAMETER :: scale_700 = 0.001923  !~ 1/520
     REAL, PARAMETER :: scale_650 = 0.001351  !~ 1/740
     REAL, PARAMETER :: scale_600 = 0.001000  !~ 1/1000
     REAL, PARAMETER :: scale_550 = 0.000800  !~ 1/1250

     !  --------------------------------------------------------------------  !
     !  --------------------------------------------------------------------  !

     !~ Initialize variables
     !  --------------------
     trapped = REAL ( 0 )

     !~ Compute the trapped wave term
     !  ------------------
     dudv = REAL ( 0 )
     DO i = 2, nlayer

       !~ Compute dudv first
       !  ------------------
       du         = u ( i-1 ) - u ( i )
       dv         = v ( i-1 ) - v ( i )

       !~ Scale based on pressure level
       !  -----------------------------
       this_p = p ( i ) / REAL ( 100 )
       IF ( this_p > REAL (950) ) THEN
         scale_fact = scale_950
       ELSE IF ( this_p <= REAL (950) .AND. this_p > REAL (925) ) THEN
         scale_fact = scale_925
       ELSE IF ( this_p <= REAL (925) .AND. this_p > REAL (900) ) THEN
         scale_fact = scale_900
       ELSE IF ( this_p <= REAL (900) .AND. this_p > REAL (850) ) THEN
         scale_fact = scale_850
       ELSE IF ( this_p <= REAL (850) .AND. this_p > REAL (800) ) THEN
         scale_fact = scale_800
       ELSE IF ( this_p <= REAL (800) .AND. this_p > REAL (750) ) THEN
         scale_fact = scale_750
       ELSE IF ( this_p <= REAL (750) .AND. this_p > REAL (700) ) THEN
         scale_fact = scale_700
       ELSE IF ( this_p <= REAL (700) .AND. this_p > REAL (650) ) THEN
         scale_fact = scale_650
       ELSE IF ( this_p <= REAL (650) .AND. this_p > REAL (600) ) THEN
         scale_fact = scale_600
       ELSE IF ( this_p <= REAL (600) ) THEN
         scale_fact = scale_550
       END IF

       this_dudv = ( (du**2)*(dv**2) ) * scale_fact
       IF ( this_dudv > dudv ) dudv = this_dudv

    END DO

    trapped = dudv
    IF ( trapped > REAL ( 1 ) ) trapped = REAL ( 1 )
    trapped = trapped / REAL ( 4 )

  END FUNCTION LLT_TrappedWave

END MODULE diag_functions



MODULE module_diag_afwa

    USE diag_functions

CONTAINS

  SUBROUTINE afwa_diagnostics_driver (   grid , config_flags     &
                             , moist                             &
                             , scalar                            &
                             , chem                              &
                             , th_phy , pi_phy , p_phy           &
                             , u_phy , v_phy                     &
                             , dz8w , p8w , t8w , rho_phy , rho  &
                             , ids, ide, jds, jde, kds, kde      &
                             , ims, ime, jms, jme, kms, kme      &
                             , ips, ipe, jps, jpe, kps, kpe      &
                             , its, ite, jts, jte                &
                             , k_start, k_end               )

    USE module_domain, ONLY : domain , domain_clock_get
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec
    USE module_state_description
    USE module_model_constants
    USE module_utility
    USE module_streams, ONLY: history_alarm, auxhist2_alarm
#ifdef DM_PARALLEL
    USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
#endif
    USE module_diag_afwa_hail

    IMPLICIT NONE

    TYPE ( domain ), INTENT(INOUT) :: grid
    TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe
    INTEGER             :: k_start , k_end, its, ite, jts, jte

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_moist),    &
         INTENT(IN   ) ::                                moist

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_scalar),    &
         INTENT(IN   ) ::                                scalar

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme , num_chem),     &
         INTENT(IN   ) ::                                 chem

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                               th_phy  &
                                              ,         pi_phy  &
                                              ,          p_phy  &
                                              ,          u_phy  &
                                              ,          v_phy  &
                                              ,           dz8w  &
                                              ,            p8w  &
                                              ,            t8w  &
                                              ,        rho_phy  &
                                              ,            rho

    ! Local
    ! -----
    CHARACTER*512 :: message
    CHARACTER*256 :: timestr 
    INTEGER :: i,j,k,nz,ostat
    INTEGER :: icing_opt
    REAL :: bdump
    INTEGER :: i_start, i_end, j_start, j_end
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::      qrain  &
                                              ,          qsnow  &
                                              ,          qgrpl  &
                                              ,          qvapr  &
                                              ,         qcloud  &
                                              ,           qice  &
                                              ,         ncloud  &
                                              ,         ngraup  &
                                              ,             rh  &
                                              ,         rh_cld  &
                                              ,           ptot  &
                                              ,            z_e  &
                                              ,           zagl

    REAL, DIMENSION( ims:ime, jms:jme, 5 ) ::            dustc
    REAL, DIMENSION( ims:ime, jms:jme ) ::                rh2m  &
                                              ,          rh20m  &
                                              ,           tv2m  &
                                              ,          tv20m  &
                                              ,        wind10m  &
                                              ,       wup_mask  &
                                              ,       wind125m  &
                                              ,           llws  &
                                              ,         pwater

    LOGICAL :: do_buoy_calc, do_hailcast_calc
    REAL :: zlfc_msl, dum1, dum2, dum3, wind_vel, wind_blend
    REAL :: prate_mm_per_hr, factor
    REAL :: u1km, v1km, ublend, vblend, u2000, v2000, us, vs
    LOGICAL :: is_target_level

    ! Timing
    ! ------
    TYPE(WRFU_Time) :: hist_time, aux2_time, CurrTime, StartTime
    TYPE(WRFU_TimeInterval) :: dtint, histint, aux2int
    LOGICAL :: is_after_history_dump, is_output_timestep, is_first_timestep

    ! Chirp the routine name for debugging purposes
    ! ---------------------------------------------
    write ( message, * ) 'inside afwa_diagnostics_driver'
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
        
    ! Here is an optional check for bad data in case the model has gone
    ! off the hinges unchecked until now.  This happens under certain
    ! configurations and can lead to very wrong data writes.  This is just
    ! a simple check for sane winds and potential temperatures.
    ! --------------------------------------------------------------------
    IF ( config_flags%afwa_bad_data_check .GT. 0 ) THEN
      IF ( ( is_output_timestep ) .AND. ( .NOT. is_first_timestep ) ) THEN      
        DO i=its, MIN( ide-1, ite )
          DO k=k_start, k_end
            DO j=jts, MIN( jde-1, jte )
              IF ( ( u_phy(i,k,j)  .GT.  300.  ) .OR. &
                   ( u_phy(i,k,j)  .LT. -300.  ) .OR. &
                   ( v_phy(i,k,j)  .GT.  300.  ) .OR. &
                   ( v_phy(i,k,j)  .LT. -300.  ) .OR. &
                   ( th_phy(i,k,j) .GT.  9999. ) .OR. &
                   ( th_phy(i,k,j) .LT.  99.   ) ) THEN
                write ( message, * ) "AFWA Diagnostics: ERROR - Model winds and/or " // &
                "potential temperature appear to be bad. If you do not want this check, " // &
                "set afwa_bad_data_check=0. i=",i,", j=",j,", k=",k,", u_phy=",u_phy(i,k,j), &
                ", v_phy=", v_phy(i,k,j),", th_phy=",th_phy(i,k,j)
                CALL wrf_error_fatal( message )
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
    ENDIF

    ! 3-D arrays for moisture variables
    ! ---------------------------------
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          qvapr(i,k,j) = moist(i,k,j,P_QV)
          qrain(i,k,j) = moist(i,k,j,P_QR)
          qsnow(i,k,j) = moist(i,k,j,P_QS)
          qgrpl(i,k,j) = moist(i,k,j,P_QG)
          qcloud(i,k,j) = moist(i,k,j,P_QC)
          qice(i,k,j) = moist(i,k,j,P_QI)
          ncloud(i,k,j) = scalar(i,k,j,P_QNC)
        ENDDO
      ENDDO
    ENDDO
    
    ! Total pressure
    ! -------------- 
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          ptot(i,k,j)=grid%pb(i,k,j)+grid%p(i,k,j)
        ENDDO
      ENDDO
    ENDDO

    ! ZAGL (height above ground)
    ! --------------------------
    DO i=ims, ime
      DO k=kms, kme
        DO j=jms, jme
          zagl(i,k,j)=grid%z(i,k,j)-grid%ht(i,j)
        ENDDO
      ENDDO
    ENDDO

    ! Calculate relative humidity
    ! ---------------------------
    DO i=ims,ime
      DO k=kms,kme    
        DO j=jms,jme
          rh(i,k,j)=calc_rh(ptot(i,k,j),grid%t_phy(i,k,j), qvapr(i,k,j))
          rh_cld(i,k,j)=calc_rh(ptot(i,k,j),grid%t_phy(i,k,j), qvapr(i,k,j)+qcloud(i,k,j)+qice(i,k,j))
        ENDDO
      ENDDO
    ENDDO

    ! Time-step precipitation (convective + nonconvective)
    ! --------------------------------------------------------------
    DO i=ims,ime
      DO j=jms,jme
        grid % afwa_precip(i,j) = grid%raincv(i,j) + grid%rainncv(i,j)
        grid % afwa_totprecip(i,j) = grid%rainc(i,j) + grid%rainnc(i,j)
      ENDDO
    ENDDO

    ! Calculate precipitable water
    ! ----------------------------
    nz=kme-kms+1
    DO i=ims,ime
      DO j=jms,jme
        grid % afwa_pwat ( i, j ) = Pwat( nz,                  &
                                          qvapr(i,kms:kme,j),  &
                                          qcloud(i,kms:kme,j), &
                                          dz8w(i,kms:kme,j),   &
                                          rho(i,kms:kme,j) )
      ENDDO
    ENDDO

    ! After each history dump, reset max/min value arrays
    ! ----------------------------------------------------------------------
    IF ( is_after_history_dump ) THEN
      DO j = jms, jme
        DO i = ims, ime
          grid % wspd10max(i,j) = 0.
          grid % afwa_llws(i,j) = 0.
        ENDDO
      ENDDO
    ENDIF 

    ! Calculate the max 10 m wind speed between output times
    ! ------------------------------------------------------
    ! UPDATE 20150112 - GAC
    ! Diagnose from model 10 m winds, and blend with 1 km AGL
    ! winds when precipitation rate is > 50 mm/hr to account
    ! for increased surface wind gust potential when precip
    ! is heavy and when winds aloft are strong.  Will use the
    ! higher of the surface and the blended winds. Blending
    ! is linear weighted between 50-150 mm/hr precip rates.
    ! -------------------------------------------------------
    DO j = jms, jme
      DO i = ims, ime
        wind_vel = uv_wind ( grid % u10(i,j) , grid % v10(i,j) )
        prate_mm_per_hr = ( grid % afwa_precip(i,j) / grid % dt ) * 3600.

        ! Is this an area of heavy precip?  Calculate 1km winds to blend down
        ! -------------------------------------------------------------------
        IF ( prate_mm_per_hr .GT. 50. ) THEN
          is_target_level=.false.
          DO k=kms,kme    
            IF ( ( zagl(i,k,j) >= 1000. ) .and. &
                 ( .NOT. is_target_level ) .and. &
                 ( k .ne. kms ) ) THEN
              is_target_level = .true.
              u1km = u_phy(i,k-1,j) + (1000. - (zagl(i,k-1,j))) &
                     * ((u_phy(i,k,j) - u_phy(i,k-1,j))/(zagl(i,k,j)))
              v1km = v_phy(i,k-1,j) + (1000. - (zagl(i,k-1,j))) &
                     * ((v_phy(i,k,j) - v_phy(i,k-1,j))/(zagl(i,k,j)))
              EXIT ! We've found our level, break the loop
            ENDIF
          ENDDO
          
          ! Compute blended wind
          ! --------------------
          factor = MAX ( ( ( 150. - prate_mm_per_hr ) / 100. ), 0. )
          ublend = grid % u10(i,j) * factor + u1km * (1. - factor)
          vblend = grid % v10(i,j) * factor + v1km * (1. - factor)
          wind_blend = uv_wind ( ublend, vblend )

          ! Set the surface wind to the blended wind if higher
          ! --------------------------------------------------
          IF ( wind_blend .GT. wind_vel ) THEN
            wind_vel = wind_blend
          ENDIF
        ENDIF

        IF ( wind_vel .GT. grid % wspd10max(i,j) ) THEN
          grid % wspd10max(i,j) = wind_vel
        ENDIF
      ENDDO
    ENDDO

    ! Calculate 0-2000 foot (0 - 609.6 meter) shear.
    ! ----------------------------------------------
    DO j = jts, jte
      DO i = its, ite
        is_target_level=.false.
        DO k=kms,kme    
          IF ( ( zagl(i,k,j) >= 609.6 ) .and. &
               ( .NOT. is_target_level ) .and. &
               ( k .ne. kms ) ) THEN
            is_target_level = .true.
            u2000 = u_phy(i,k-1,j) + (609.6 - (zagl(i,k-1,j))) &
                    * ((u_phy(i,k,j) - u_phy(i,k-1,j))/(zagl(i,k,j)))
            v2000 = v_phy(i,k-1,j) + (609.6 - (zagl(i,k-1,j))) &
                    * ((v_phy(i,k,j) - v_phy(i,k-1,j))/(zagl(i,k,j)))
            us = u2000 - grid % u10(i,j) 
            vs = v2000 - grid % v10(i,j) 
            llws(i,j) = uv_wind ( us , vs )
            IF ( llws(i,j) .gt. grid % afwa_llws(i,j) ) THEN
              grid % afwa_llws(i,j) = llws(i,j)
            ENDIF
            EXIT ! We've found our level, break the loop
          ENDIF
        ENDDO
      ENDDO
    ENDDO

#if ( WRF_CHEM == 1 )
    ! Surface dust concentration array (ug m-3)
    ! ----------------------------------------- 
    DO i=ims, ime
      DO j=jms, jme
        dustc(i,j,1)=chem(i,k_start,j,p_dust_1)*rho(i,k_start,j)
        dustc(i,j,2)=chem(i,k_start,j,p_dust_2)*rho(i,k_start,j)
        dustc(i,j,3)=chem(i,k_start,j,p_dust_3)*rho(i,k_start,j)
        dustc(i,j,4)=chem(i,k_start,j,p_dust_4)*rho(i,k_start,j)
        dustc(i,j,5)=chem(i,k_start,j,p_dust_5)*rho(i,k_start,j)
      ENDDO
    ENDDO
#else
    dustc(ims:ime,jms:jme,:)=0.
#endif
   
    ! Calculate severe weather diagnostics.  These variables should only be
    ! output at highest frequency output.  (e.g. auxhist2)
    ! ---------------------------------------------------------------------
    IF ( config_flags % afwa_severe_opt == 1 ) THEN

      ! After each history dump, reset max/min value arrays
      ! Note: This resets up_heli_max which is currently calculated within
      ! rk_first_rk_step_part2.F, may want to move to this diagnostics package
      ! later
      ! ----------------------------------------------------------------------
      IF ( is_after_history_dump ) THEN
        DO j = jms, jme
          DO i = ims, ime
!            grid%wspd10max(i,j) = 0.
            grid%w_up_max(i,j) = 0.
            grid%w_dn_max(i,j) = 0.
            grid%tcoli_max(i,j) = 0.
            grid%grpl_flx_max(i,j) = 0.
            grid%up_heli_max(i,j) = 0.
!            grid%refd_max(i,j) = 0.
            grid%afwa_tornado(i,j) = 0.
            grid%midrh_min_old(i,j) = grid%midrh_min(i,j) ! Save old midrh_min
            grid%midrh_min(i,j) = 999.
            grid%afwa_hail(i,j) = 0.
            IF ( config_flags % afwa_hailcast_opt == 1 ) THEN
              grid%afwa_hail_new1(i,j) = 0.
              grid%afwa_hail_new2(i,j) = 0.
              grid%afwa_hail_new3(i,j) = 0.
              grid%afwa_hail_new4(i,j) = 0.
              grid%afwa_hail_new5(i,j) = 0.
            ENDIF
          ENDDO
        ENDDO
      ENDIF  ! is_after_history_dump

      IF ( ( is_first_timestep ) .OR. ( is_output_timestep ) ) THEN
        do_buoy_calc = .true.
      ELSE
        do_buoy_calc = .false.
      ENDIF

      IF ( config_flags % afwa_hailcast_opt == 1 ) THEN
        do_hailcast_calc = .true.
      ELSE
        do_hailcast_calc = .false.
      ENDIF

      !-->RAS
      ! We need to do some neighboring gridpoint comparisons in this next function;
      ! set these values so we don't go off the edges of the domain.  Updraft
      ! duration on domain edges will always be 0.
      ! ----------------------------------------------------------------------
      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = jte

      IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
           config_flags%nested) i_start = MAX( ids+1, its )
      IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
           config_flags%nested) i_end   = MIN( ide-1, ite )
      IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
           config_flags%nested) j_start = MAX( jds+1, jts )
      IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
           config_flags%nested) j_end   = MIN( jde-1, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

      CALL severe_wx_diagnostics ( grid % wspd10max             &
                             , grid % w_up_max                  &
                             , grid % w_dn_max                  &
                             , grid % up_heli_max               &
                             , grid % tcoli_max                 &
                             , grid % midrh_min_old             &
                             , grid % midrh_min                 &
                             , grid % afwa_hail                 &
                             , grid % afwa_hail_new1            &
                             , grid % afwa_hail_new2            &
                             , grid % afwa_hail_new3            &
                             , grid % afwa_hail_new4            &
                             , grid % afwa_hail_new5            &
                             , grid % afwa_cape                 &
                             , grid % afwa_cin                  &
!                             , grid % afwa_cape_mu              &
!                             , grid % afwa_cin_mu               &
                             , grid % afwa_zlfc                 &
                             , grid % afwa_plfc                 &
                             , grid % afwa_lidx                 &
                             , llws                             &
                             , grid % afwa_tornado              &
                             , grid % grpl_flx_max              &
                             , grid % u10                       &
                             , grid % v10                       &
                             , grid % w_2                       &
                             , grid % uh                        &
                             , grid % t_phy                     &
                             , grid % t2                        &
                             , grid % z                         &
                             , grid % ht                        &
                             , grid % wup_mask                  &
                             , grid % wdur                      &
                             , grid % tornado_mask              &
                             , grid % tornado_dur               &
                             , grid % dt                        &
                             , grid % afwa_pwat                 &
                             , u_phy                            &
                             , v_phy                            &
                             , ptot                             &
                             , qice                             &
                             , qsnow                            &
                             , qgrpl                            &
                             , ngraup                           &
                             , qvapr, qrain, qcloud             &
                             , rho                              &
                             , dz8w                             &
                             , rh                               &
                             , do_buoy_calc                     &
                             , do_hailcast_calc                 &
                             , ims, ime, jms, jme, kms, kme     &
                             , its, ite, jts, jte               &
                             , k_start, k_end                   &
                             , j_start, j_end, i_start, i_end   )

    ENDIF   ! afwa_severe_opt == 1

    ! Calculate precipitation type diagnostics
    ! ----------------------------------------
    IF ( config_flags % afwa_ptype_opt == 1 ) THEN
    
      ! First initialize precip buckets
      ! -------------------------------
      IF ( grid % itimestep .eq. 1) THEN
        DO i=ims,ime
          DO j=jms,jme
            grid % afwa_rain(i,j)=0.
            grid % afwa_snow(i,j)=0.
            grid % afwa_ice(i,j)=0.
            grid % afwa_fzra(i,j)=0.
            grid % afwa_snowfall(i,j)=0.
          ENDDO
        ENDDO
      ENDIF
  
      ! Diagnose precipitation type
      ! ---------------------------
      CALL precip_type_diagnostics ( grid % t_phy               &
                             , grid % t2                        &
                             , rh                               &
                             , grid % z                         &
                             , grid % ht                        &
                             , grid % afwa_precip               &
                             , grid % swdown                    &
                             , grid % afwa_rain                 &
                             , grid % afwa_snow                 &
                             , grid % afwa_ice                  &
                             , grid % afwa_fzra                 &
                             , grid % afwa_snowfall             &
                             , grid % afwa_ptype_ccn_tmp        &
                             , grid % afwa_ptype_tot_melt       &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
    ENDIF  ! afwa_ptype_opt == 1
  
    ! --------------------------------------------------------------
    ! The following packages are calculated only on output timesteps
    ! --------------------------------------------------------------
    IF ( is_output_timestep ) THEN      

      ! Calculate mean sea level pressure
      ! ---------------------------------
      DO j = jms, jme
        DO i = ims, ime
          grid % afwa_mslp  ( i, j ) =   MSLP ( grid % ht ( i, j )           & 
                                              , grid % psfc ( i, j )         &
                                              , grid % z ( i, kms, j )       & 
                                              , qvapr ( i, kms, j )          &  
                                              , grid % t_phy ( i, kms, j ) )
        ENDDO
      ENDDO

      ! Calculate 10 meter winds
      ! ------------------------
      DO i=ims,ime
        DO j=jms,jme
          wind10m(i,j)=uv_wind(grid%u10(i,j),grid%v10(i,j))
        ENDDO
      ENDDO

      ! Calculate 2 meter relative humidity/Tv
      ! --------------------------------------
      DO i=ims,ime
        DO j=jms,jme
          rh2m(i,j)=calc_rh(grid%psfc(i,j), grid%t2(i,j), grid%q2(i,j))
          tv2m(i,j)=grid%t2(i,j) * (1 + 0.61 * grid%q2(i,j))
        ENDDO
      ENDDO
 
      ! Calculate the mean and standard deviation of the hail diameter
      ! distribution over different embryo sizes
      ! ----------------------------------------
      IF ( config_flags % afwa_hailcast_opt == 1 ) THEN
        DO j = jms, jme
          DO i = ims, ime
            !mean
            grid%afwa_hail_newmean(i,j)=(grid%afwa_hail_new1(i,j)+&
               grid%afwa_hail_new2(i,j) +grid%afwa_hail_new3(i,j)+&
               grid%afwa_hail_new4(i,j) +grid%afwa_hail_new5(i,j))/5.
            !sample standard deviation
            grid%afwa_hail_newstd(i,j) = SQRT( ( &
            (grid%afwa_hail_new1(i,j)-grid%afwa_hail_newmean(i,j))**2.+&
            (grid%afwa_hail_new2(i,j)-grid%afwa_hail_newmean(i,j))**2.+&
            (grid%afwa_hail_new3(i,j)-grid%afwa_hail_newmean(i,j))**2.+&
            (grid%afwa_hail_new4(i,j)-grid%afwa_hail_newmean(i,j))**2.+&
            (grid%afwa_hail_new5(i,j)-grid%afwa_hail_newmean(i,j))**2.)&
            / 4.0)
          ENDDO
        ENDDO
      ENDIF

      ! Calculate buoyancy parameters.
      ! ------------------------------
      IF ( config_flags % afwa_buoy_opt == 1 ) THEN
        nz = k_end - k_start + 1

        ! Calculate buoyancy for surface parcel
        ! -------------------------------------
        DO j = jts, jte
          DO i = its, ite
            ostat = Buoyancy (                                   nz &
                                     ,      grid%t_phy(i,kms:kme,j) &
                                     ,              rh(i,kms:kme,j) &
                                     ,            ptot(i,kms:kme,j) &
                                     ,        grid % z(i,kms:kme,j) &
                                     ,                            1 &
                                     ,        grid % afwa_cape(i,j) &
                                     ,         grid % afwa_cin(i,j) &
                                     ,                     zlfc_msl &
                                     ,        grid % afwa_plfc(i,j) &
                                     ,        grid % afwa_lidx(i,j) &
                                     ,                        3 ) !Surface
        
            ! Subtract terrain height to convert ZLFC from MSL to AGL
            ! -------------------------------------------------------
            IF ( zlfc_msl .ge. grid % ht ( i, j ) ) THEN
              grid % afwa_zlfc ( i, j ) = zlfc_msl - grid % ht ( i, j )
            ELSE
              grid % afwa_zlfc( i, j ) = -1.
            ENDIF

            ! Add 273.15 to lifted index per some standard I don't understand
            ! ---------------------------------------------------------------
            IF ( grid % afwa_lidx ( i, j ) .ne. 999. ) THEN
              grid % afwa_lidx ( i, j ) = grid % afwa_lidx ( i, j ) + 273.15
            ENDIF
 
            ! Calculate buoyancy again for most unstable layer
            ! ------------------------------------------------
            ostat = Buoyancy (                                   nz &
                                     ,      grid%t_phy(i,kms:kme,j) &
                                     ,              rh(i,kms:kme,j) &
                                     ,            ptot(i,kms:kme,j) &
                                     ,        grid % z(i,kms:kme,j) &
                                     ,                            1 &
                                     ,     grid % afwa_cape_mu(i,j) &
                                     ,      grid % afwa_cin_mu(i,j) &
                                     ,                         dum1 &
                                     ,                         dum2 &
                                     ,                         dum3 &
                                     ,                        1 ) !Most unstable
        
          ENDDO
        ENDDO
      ENDIF  ! afwa_buoy_opt == 1

      IF ( config_flags % afwa_therm_opt == 1 ) THEN
        write ( message, * ) 'Calculating thermal indices'
        CALL wrf_debug( 100 , message )
        CALL thermal_diagnostics ( grid % t2                        &
                                 , grid % psfc                      &
                                 , rh2m                             &
                                 , wind10m                          &
                                 , grid % afwa_heatidx              &
                                 , grid % afwa_wchill               &
                                 , grid % afwa_fits                 &
                                 , ids, ide, jds, jde, kds, kde     &
                                 , ims, ime, jms, jme, kms, kme     &
                                 , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_therm_opt == 1

      IF ( config_flags % afwa_turb_opt == 1 ) THEN
        write ( message, * ) 'Calculating turbulence indices'

        !~ For now, hard code turbulence layer bottom and top AGL heights
        !  --------------------------------------------------------------
        grid % afwa_tlyrbot = (/ 1500., 3000., 4600., 6100., 7600., 9100.,   &
                                  10700. /)
        grid % afwa_tlyrtop = (/ 3000., 4600., 6100., 7600., 9100., 10700.,  &
                                  12700. /)
        call turbulence_diagnostics ( u_phy                     &
                             , v_phy                            &
                             , grid % t_phy                     &
                             , ptot                             &
                             , zagl                             &
                             , grid % defor11                   &
                             , grid % defor12                   &
                             , grid % defor22                   &
                             , grid % afwa_turb                 &
                             , grid % afwa_llturb               &
                             , grid % afwa_llturblgt            &
                             , grid % afwa_llturbmdt            &
                             , grid % afwa_llturbsvr            &
                             !, config_flags % num_turb_layers   &
                             , 7                                &
                             , grid % afwa_tlyrbot              &
                             , grid % afwa_tlyrtop              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_turb_opt == 1

      ! Calculate equivalent radar reflectivity factor (z_e) using 
      ! old RIP code (2004) if running radar or VIL packages.
      ! ----------------------------------------------------------
      IF ( config_flags % afwa_radar_opt == 1 .or. &
         config_flags % afwa_vil_opt == 1 ) THEN
        write ( message, * ) 'Calculating Radar'
        CALL wrf_debug( 100 , message )
        CALL wrf_dbzcalc ( rho                                  &
                             , grid%t_phy                       &
                             , qrain                            &
                             , qsnow                            &
                             , qgrpl                            &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_radar_opt == 1 .or. afwa_vil_opt == 1

      ! Calculate derived radar variables
      ! ---------------------------------
      ! UPDATE: removed refd_max calculation, which was not working correctly.
      ! To correctly calculate refd_max, this routine could be called every
      ! time step, but instead, we are only going to calculate reflectivity
      ! on output time steps and avoid cost of calculating refd_max. GAC2014
      ! ----------------------------------------------------------------------
      IF ( config_flags % afwa_radar_opt == 1 ) THEN
        write ( message, * ) 'Calculating derived radar variables'
        CALL wrf_debug( 100 , message )
        CALL radar_diagnostics ( grid % refd                    &
                             , grid % refd_com                  &
!                             , grid % refd_max                  &
                             , grid % echotop                   &
                             , grid % z                         &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_radar_opt == 1

      ! Calculate VIL and reflectivity every history output timestep
      ! ------------------------------------------------------------
      IF ( config_flags % afwa_vil_opt == 1 ) THEN
        write ( message, * ) 'Calculating VIL'
        CALL wrf_debug( 100 , message )
        CALL vert_int_liquid_diagnostics ( grid % vil           &
                             , grid % radarvil                  &
                             , grid % t_phy                     &
                             , qrain                            &
                             , qsnow                            &
                             , qgrpl                            &
                             , z_e                              &
                             , dz8w                             &
                             , rho                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_vil_opt ==1 

      ! Calculate icing and freezing level
      ! ----------------------------------
      IF ( config_flags % afwa_icing_opt == 1 ) THEN

        ! Determine icing option from microphysics scheme
        ! -----------------------------------------------
        
        IF ( config_flags % mp_physics == GSFCGCESCHEME ) THEN
          icing_opt=1
        ELSEIF ( config_flags % mp_physics == ETAMPNEW ) THEN
          icing_opt=2
        ELSEIF ( config_flags % mp_physics == THOMPSON ) THEN
          icing_opt=3
        ELSEIF ( config_flags % mp_physics == WSM5SCHEME .OR.   &
                 config_flags % mp_physics == WSM6SCHEME ) THEN
          icing_opt=4
        ELSEIF ( config_flags % mp_physics == MORR_TWO_MOMENT ) THEN

          ! Is this run with prognostic cloud droplets or no?
          ! -------------------------------------------------
          IF (config_flags % progn > 0) THEN
             icing_opt=6
          ELSE
             icing_opt=5
          ENDIF
        ELSEIF ( config_flags % mp_physics == WDM6SCHEME ) THEN
          icing_opt=7
        ELSE
          icing_opt=0  ! Not supported
        ENDIF
 
        write ( message, * ) 'Calculating Icing with icing opt ',icing_opt 
        CALL wrf_debug( 100 , message )
        CALL icing_diagnostics ( icing_opt                      &
                             , grid % fzlev                     &
                             , grid % icing_lg                  &
                             , grid % icing_sm                  &
                             , grid % qicing_lg_max             &
                             , grid % qicing_sm_max             &
                             , grid % qicing_lg                 &
                             , grid % qicing_sm                 &
                             , grid % icingtop                  &
                             , grid % icingbot                  &
                             , grid % t_phy                     &
                             , grid % z                         &
                             , dz8w                             &
                             , rho                              &
                             , qrain                            &
                             , qcloud                           &
                             , ncloud                           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF  ! afwa_icing_opt

      ! Calculate visiblility diagnostics
      ! ---------------------------------
      IF ( config_flags % afwa_vis_opt == 1 ) THEN
   
        ! Interpolate 20m temperature and RH
        ! ----------------------------------
        DO i=ims,ime
          DO j=jms,jme
            tv20m(i,j) = -999.
            rh20m(i,j) = -999.
            DO k = kps, MIN(kpe,kde-1)
              IF (tv20m (i,j) .eq. -999. .AND. zagl (i,k,j) .ge. 20.) THEN

                ! If the lowest model level > 20 m AGL, interpolate using 2-m temperature/RH
                ! --------------------------------------------------------------------------
                IF (k .eq. kps) THEN
                  tv20m(i,j) = tv2m(i,j) + &
                               (20. - 2.) / &
                               (zagl(i,k,j) - 2.) * &
                               (grid%t_phy(i,k,j) * (1 + 0.61 * qvapr(i,k,j)) - tv2m(i,j))
                  rh20m(i,j) = rh2m(i,j) + &
                               (20. - 2.) / &
                               (zagl(i,k,j) - 2.) * &
                               (rh(i,k,j) - rh2m(i,j))
                ELSE
                  tv20m(i,j) = grid%t_phy(i,k-1,j) * (1 + 0.61 * qvapr(i,k-1,j)) + &
                               ((20. - zagl(i,k-1,j)) / &
                               (zagl(i,k,j) - zagl(i,k-1,j))) * &
                               (grid%t_phy(i,k,j) * (1 + 0.61 * qvapr(i,k,j)) - &
                               grid%t_phy(i,k-1,j) * (1 + 0.61 * qvapr(i,k-1,j)))
                  rh20m(i,j) = rh (i,k-1,j) + &
                               ((20. - zagl (i,k-1,j)) / &
                               (zagl (i,k,j) - zagl (i,k-1,j))) * &
                               (rh (i,k,j) - rh (i,k-1,j))
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO

        ! Calculate 125 meter winds
        ! -------------------------
        DO i=ims,ime
          DO j=jms,jme
            wind125m(i,j) = -999.
            DO k = kps, MIN(kpe,kde-1)
              IF (wind125m (i,j) .eq. -999. .AND. zagl (i,k,j) .ge. 125.) THEN

                ! If the lowest model level > 125 m AGL, use level 1 wind
                ! -------------------------------------------------------
                IF (k .eq. kps) THEN
                  wind125m(i,j) = uv_wind(u_phy(i,k,j),v_phy(i,k,j))
                ELSE
                  wind125m(i,j) = uv_wind(u_phy(i,k-1,j),v_phy(i,k-1,j)) + &
                               ((125. - zagl(i,k-1,j)) / &
                               (zagl(i,k,j) - zagl(i,k-1,j))) * &
                               (uv_wind(u_phy(i,k,j),v_phy(i,k,j)) - &
                               uv_wind(u_phy(i,k-1,j),v_phy(i,k-1,j)))
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO

        write ( message, * ) 'Calculating visibility'
        CALL wrf_debug( 100 , message )
        CALL vis_diagnostics ( qcloud(ims:ime,k_start,jms:jme)  &
                             , qrain(ims:ime,k_start,jms:jme)   &
                             , qice(ims:ime,k_start,jms:jme)    &
                             , qsnow(ims:ime,k_start,jms:jme)   &
                             , qgrpl(ims:ime,k_start,jms:jme)   &
                             , rho(ims:ime,k_start,jms:jme)     &
                             , wind10m                          &
                             , wind125m                         &
                             , grid % afwa_pwat                 &
                             , grid % q2                        &
                             , rh2m                             &
                             , rh20m                            &
                             , tv2m                             &
                             , tv20m                            &
                             , dustc                            &
                             , grid % afwa_vis                  &
                             , grid % afwa_vis_dust             &
                             , grid % afwa_vis_alpha            &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe ) 
      ENDIF

      ! Calculate cloud diagnostics
      ! ---------------------------
      IF ( config_flags % afwa_cloud_opt == 1 ) THEN
        write ( message, * ) 'Calculating cloud'
        CALL wrf_debug( 100 , message )
        CALL cloud_diagnostics (qcloud                          &
                             , qice                             &
                             , qsnow                            &
                             , rh_cld                           &
                             , dz8w                             &
                             , rho                              &
                             , grid % z                         &
                             , grid % ht                        &
                             , grid % afwa_cloud                &
                             , grid % afwa_cloud_ceil           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )
      ENDIF

    ENDIF  ! is_output_timestep

  END SUBROUTINE afwa_diagnostics_driver



  SUBROUTINE severe_wx_diagnostics ( wspd10max                  &
                             , w_up_max                         &
                             , w_dn_max                         &
                             , up_heli_max                      &
                             , tcoli_max                        &
                             , midrh_min_old                    &
                             , midrh_min                        &
                             , afwa_hail                        &
                             , afwa_hail_new1                   &
                             , afwa_hail_new2                   &
                             , afwa_hail_new3                   &
                             , afwa_hail_new4                   &
                             , afwa_hail_new5                   &
                             , cape                             &
                             , cin                              &
!                             , cape_mu                          &
!                             , cin_mu                           &
                             , zlfc                             &
                             , plfc                             &
                             , lidx                             &
                             , llws                             &
                             , afwa_tornado                     &
                             , grpl_flx_max                     &
                             , u10                              &
                             , v10                              &
                             , w_2                              &
                             , uh                               &
                             , t_phy                            &
                             , t2                               &
                             , z                                &
                             , ht                               &
                             , wup_mask                         &
                             , wdur                             &
                             , tornado_mask                     &
                             , tornado_dur                      &
                             , dt                               &
                             , pwat                             &
                             , u_phy                            &
                             , v_phy                            &
                             , p                                &
                             , qi                               &
                             , qs                               &
                             , qg                               &
                             , ng                               &
                             , qv, qr, qc                       &
                             , rho                              &
                             , dz8w                             &
                             , rh                               &
                             , do_buoy_calc                     &
                             , do_hailcast_calc                 &
                             , ims, ime, jms, jme, kms, kme     &
                             , its, ite, jts, jte               &
                             , k_start, k_end                   &
                             , j_start, j_end, i_start, i_end   )

    USE module_diag_afwa_hail

    INTEGER, INTENT(IN) :: its, ite, jts, jte, k_start, k_end   &
                         , ims, ime, jms, jme, kms, kme         &
                         , j_start, j_end, i_start, i_end


    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                    p  &
                                              ,            w_2  &
                                              ,          t_phy  &
                                              ,          u_phy  &
                                              ,          v_phy  &
                                              ,             qi  &
                                              ,             qs  &
                                              ,             qg  &
                                              ,             ng  &
                                              ,     qv, qr, qc  &
                                              ,            rho  &
                                              ,              z  &
                                              ,           dz8w  &
                                              ,             rh


    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                  u10  &
                                              ,            v10  &
                                              ,      wspd10max  &
                                              ,             uh  &
                                              ,             t2  &
                                              ,             ht  &
                                              ,  midrh_min_old  &
                                              ,    up_heli_max  &
                                              ,           llws  &
                                              ,           pwat


    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                             w_up_max  & 
                                              ,       w_dn_max  &
                                              ,      tcoli_max  &
                                              ,      midrh_min  &
                                              ,      afwa_hail  &
                                              , afwa_hail_new1  &
                                              , afwa_hail_new2  &
                                              , afwa_hail_new3  &
                                              , afwa_hail_new4  &
                                              , afwa_hail_new5  &
                                              ,   afwa_tornado  &
                                              ,   grpl_flx_max  &
                                              ,       wup_mask  &
                                              ,           wdur  &
                                              ,   tornado_mask  &
                                              ,    tornado_dur 


!    REAL, DIMENSION( ims:ime, jms:jme ),                        &
!         INTENT(  OUT) ::                                 cape  &
!                                              ,            cin  &
!                                              ,        cape_mu  &
!                                              ,         cin_mu  &
!                                              ,           zlfc  &
!                                              ,           plfc  &
!                                              ,           lidx
    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                                 cape  &
                                              ,            cin  &
                                              ,           zlfc  &
                                              ,           plfc  &
                                              ,           lidx

    REAL, INTENT(IN) ::                                     dt
    LOGICAL, INTENT(IN) ::                        do_buoy_calc  &
                                           ,  do_hailcast_calc

    ! Local
    ! -----
    INTEGER :: i,j,k
    INTEGER :: kts,kte
    REAL    :: zagl, zlfc_msl, melt_term, midrh_term, hail, midrh
    REAL    :: dum1, dum2, dum3
    REAL    :: tornado, lfc_term, shr_term, midrh2_term, uh_term
    REAL    :: wind_vel, p_tot, tcoli, grpl_flx, w_n15, qg_n15
    INTEGER :: nz, ostat
    REAL, DIMENSION( ims:ime, jms:jme ) ::                w_up  &
                                              ,           w_dn  &
                                              ,  wup_mask_prev  &
                                              ,      wdur_prev  &
                                           , tornado_mask_prev  &
                                           ,  tornado_dur_prev
    REAL :: dhail1,dhail2,dhail3,dhail4,dhail5
    REAL :: time_factor, time_factor_prev
    LOGICAL :: is_target_level

    ! Calculate midlevel relative humidity minimum
    ! --------------------------------------------
    DO i=ims,ime
      DO j=jms,jme
        is_target_level=.false.
        DO k=kms,kme    
          zagl = z(i,k,j) - ht(i,j)
          IF ( ( zagl >= 3500. ) .and. &
               ( .NOT. is_target_level ) .and. &
               ( k .ne. kms ) ) THEN
            is_target_level = .true.
            midrh = rh(i,k-1,j) + (3500. - (z(i,k-1,j) - ht(i,j))) &
                    * ((rh(i,k,j) - rh(i,k-1,j))/(z(i,k,j) - z(i,k-1,j)))
            IF ( midrh .lt. midrh_min(i,j) ) THEN
              midrh_min(i,j) = midrh
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO

    ! Calculate the max 10 m wind speed between output times
    ! ------------------------------------------------------
    !DO j = jts, jte
    !  DO i = its, ite
    !    wind_vel = uv_wind ( u10(i,j) , v10(i,j) )
    !    IF ( wind_vel .GT. wspd10max(i,j) ) THEN
    !      wspd10max(i,j) = wind_vel
    !    ENDIF
    !  ENDDO
    !ENDDO
 
    ! Vertical velocity quantities between output times
    ! -------------------------------------------------
    w_up=0.
    w_dn=0.
    DO j = jts, jte
      DO k = k_start, k_end
        DO i = its, ite
          p_tot = p(i,k,j) / 100.
 
          ! Check vertical velocity field below 400 mb
          IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .GT. w_up(i,j) ) THEN
            w_up(i,j) = w_2(i,k,j)
            IF ( w_up(i,j) .GT. w_up_max(i,j) ) THEN
              w_up_max(i,j) = w_up(i,j)
            ENDIF
          ENDIF
          IF ( p_tot .GT. 400. .AND. w_2(i,k,j) .LT. w_dn(i,j) ) THEN
            w_dn(i,j) = w_2(i,k,j)
            IF ( w_dn(i,j) .LT. w_dn_max(i,j) ) THEN
              w_dn_max(i,j) = w_dn(i,j)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO
   
    ! Hail diameter in millimeters (Weibull distribution)
    ! ---------------------------------------------------
    DO j = jts, jte
      DO i = its, ite
        melt_term=max(t2(i,j)-288.15,0.)
        midrh_term=max(2*(min(midrh_min(i,j),midrh_min_old(i,j))-70.),0.)
        ! Change exponent to 1.1 to reduce probabilities for large sizes
        !hail=max((w_up(i,j)/1.4)**1.25-melt_term-midrh_term,0.)
        hail=max((w_up(i,j)/1.4)**1.1-melt_term-midrh_term,0.)
        hail=hail*((uh(i,j)/100)+0.25)
        IF ( hail .gt. afwa_hail(i,j) ) THEN
          afwa_hail(i,j)=hail
        ENDIF
      ENDDO
    ENDDO

    ! Hailcast calculation.  Please note: this is VERY
    ! expensive, and needs to be optimized in the future to
    ! reduce its expense. Currently, on domains with plenty
    ! of convection, we have seen timesteps increase upwards
    ! of 300%.  Also, because it is not calculated at every
    ! grid point, only where there is an updraft, this can 
    ! lead to very unpredictable run time behavior.  Until
    ! we are able to speed it up significantly (either by 
    ! modifications to the algorithm, or by calling it less
    ! frequently) we are not running this in production at
    ! AFWA and are turning it off by default.  To run hailcast
    ! please use afwa_hailcast_opt=1 in the namelist. GAC2014
    ! --------------------------------------------------------
    IF ( do_hailcast_calc ) THEN

      ! --> RAS
      ! Make a copy of the updraft duration, mask variables
      ! ---------------------------------------------------
      wdur_prev(:,:) = wdur(:,:)
      wup_mask_prev(:,:) = wup_mask(:,:)

      ! Determine updraft mask (where updraft greater than some threshold)
      ! ---------------------------------------------------
      DO j = jts, jte
        DO i = its, ite
          wup_mask(i,j) = 0
          wdur(i,j) = 0

          DO k = k_start, k_end
            IF ( w_2(i,k,j) .ge. 10. ) THEN
                wup_mask(i,j) = 1
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
          IF ( (wup_mask(i,j).eq.1) .OR.                 &
             (MAXVAL(wup_mask_prev(i-1:i+1,j-1:j+1)).eq.1) ) THEN
             wdur(i,j) = MAXVAL(wdur_prev(i-1:i+1,j-1:j+1)) + dt
          ENDIF
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
          IF (wdur(i,j) .gt. 900) THEN
            CALL hailstone_driver ( t_phy(i,kms:kme,j), &
                                z(i,kms:kme,j),     &
                                ht(i,       j),     &
                                p(i,kms:kme,j),     &
                                rho(i,kms:kme,j),   &
                                qv(i,kms:kme,j),    &
                                qi(i,kms:kme,j),    &
                                qc(i,kms:kme,j),    &
                                qr(i,kms:kme,j),    &
                                qs(i,kms:kme,j),    &
                                qg(i,kms:kme,j),    &
                                ng(i,kms:kme,j),    &
                                w_2(i,kms:kme,j),   &
                                wdur(i,j),          &
                                nz,                 &
                                dhail1, dhail2,     &
                                dhail3, dhail4,     &
                                dhail5              )
            IF (dhail1 .gt. afwa_hail_new1(i,j)) THEN
                afwa_hail_new1(i,j) = dhail1
            ENDIF
            IF (dhail2 .gt. afwa_hail_new2(i,j)) THEN
                afwa_hail_new2(i,j) = dhail2
            ENDIF
            IF (dhail3 .gt. afwa_hail_new3(i,j)) THEN
                afwa_hail_new3(i,j) = dhail3
            ENDIF
            IF (dhail4 .gt. afwa_hail_new4(i,j)) THEN
                afwa_hail_new4(i,j) = dhail4
            ENDIF
            IF (dhail5 .gt. afwa_hail_new5(i,j)) THEN
                afwa_hail_new5(i,j) = dhail5
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      ! <-- RAS

    ENDIF  !~ End if do_hailcast_calc

    ! Lightning (total column-integrated cloud ice)
    ! Note this formula is basically stolen from the VIL calculation.
    ! ---------------------------------------------------------------
    DO j = jts, jte
      DO i = its, ite
        tcoli=0.
        DO k = k_start, k_end
          tcoli =  tcoli + &
          (qi (i,k,j) + &
           qs (i,k,j) + &
           qg (i,k,j))  &
           *dz8w (i,k,j) * rho(i,k,j)
        ENDDO
        IF ( tcoli .GT. tcoli_max(i,j) ) THEN
          tcoli_max(i,j) = tcoli
        ENDIF
      ENDDO
    ENDDO

    ! Lighting (pseudo graupel flux calculation)
    ! Model graupel mixing ration (g/kg) times w (m/s) at the -15C level
    ! Values should range from around 25 in marginal lightning situations,
    ! to over 400 in situations with very frequent lightning.
    ! --------------------------------------------------------------------
    DO j = jts, jte
      DO i = its, ite
        grpl_flx=0
        w_n15=-999.
        DO k = k_start, k_end
          
          ! Interpolate w and qg to -15C level and calculate graupel flux
          ! as simply graupel x vertical velocity at -15C
          ! -------------------------------------------------------------
          IF ( t_phy (i,k,j) .LE. 258.15 .AND. w_n15 .EQ. -999. .AND.   &
               k .GT. k_start .AND. qg (i,k,j) .GT. 1.E-20 ) THEN
            w_n15 = w_2 (i,k,j)
            qg_n15 = 1000. * qg (i,k,j) ! g/kg
            grpl_flx =  qg_n15 * w_n15   
          ENDIF
        ENDDO
        IF ( grpl_flx .GT. grpl_flx_max(i,j) ) THEN
          grpl_flx_max(i,j) = grpl_flx
        ENDIF
      ENDDO
    ENDDO

    ! Update buoyancy parameters.
    ! ---------------------------
    IF ( do_buoy_calc ) THEN
      nz = k_end - k_start + 1
      DO j = jts, jte
        DO i = its, ite
          ostat = Buoyancy (                                   nz &
                                       , t_phy(i,kms:kme      ,j) &
                                       ,    rh(i,kms:kme      ,j) &
                                       ,     p(i,kms:kme      ,j) &
                                       ,     z(i,kms:kme      ,j) &
                                       ,                        1 &
                                       ,                cape(i,j) &
                                       ,                 cin(i,j) &
                                       ,                 zlfc_msl &
                                       ,                plfc(i,j) &
                                       ,                lidx(i,j) &
                                       ,                        3 ) !Surface

          ! Add 273.15 to lifted index per some standard I don't understand
          ! ---------------------------------------------------------------
          IF ( lidx ( i, j ) .ne. 999. ) lidx ( i, j ) = lidx ( i, j ) + 273.15
  
          ! Subtract terrain height to convert ZLFC from MSL to AGL
          ! -------------------------------------------------------
          IF ( zlfc_msl .ge. 0. ) THEN
            zlfc ( i, j ) = zlfc_msl - ht ( i, j )
          ELSE
            zlfc( i, j ) = -1.
          ENDIF

        ENDDO
      ENDDO
    ENDIF

    ! Maximum tornado wind speed in ms-1.
    ! First, save off old tornado mask and duration arrays
    ! ----------------------------------------------------
    tornado_dur_prev(:,:)=tornado_dur(:,:)
    tornado_mask_prev(:,:)=tornado_mask(:,:)

    ! Initialize all tornado variables
    ! --------------------------------
    tornado_mask(:,:)=0.
    tornado_dur(:,:)=0.

    DO j = j_start, j_end
      DO i = i_start, i_end
        tornado = 0.

        ! Current tornado algorithm
        ! -------------------------
        IF ( zlfc(i,j) .ge. 0. ) THEN
          uh_term = min(max((uh(i,j) - 25.) / 50., 0.), 1.)
          shr_term = min(max((llws(i,j) - 2.) / 10., 0.), 1.)
          lfc_term = min(max((3000. - zlfc(i,j)) / 1500., 0.), 1.)
          midrh2_term = min(max((90. - &
                        min(midrh_min(i,j),midrh_min_old(i,j))) / 30., 0.), 1.)
          tornado = 50. * uh_term * shr_term * lfc_term * midrh2_term
        ENDIF

        ! Does tornado algorithm indicate all possible ingredients
        ! for a minimum tornado, if so turn on mask
        ! --------------------------------------------------------
        IF (tornado .GT. 0.) THEN
          tornado_mask(i,j) = 1.
        ENDIF
  
        ! Update the duration of this tornado if there was previously
        ! a tornado mask at this or an adjacent point
        ! -----------------------------------------------------------
        IF ( ( tornado_mask(i,j) .GT. 0.5 )  .OR. &
           ( MAXVAL(tornado_mask_prev(i-1:i+1,j-1:j+1)) .GT. 0.5 ) ) THEN
          tornado_dur(i,j) = MAXVAL(tornado_dur_prev(i-1:i+1,j-1:j+1)) + dt
        ENDIF

        ! Ramp up value of tornado beta value linearly in time until &
        ! it has been sustained 5 minutes
        ! ------------------------------------------------------------
        time_factor = MIN(tornado_dur(i,j)/900.,1.)
        tornado = tornado * time_factor

        ! OPTIONAL
        ! Adjust previous max tornado wind upward to account for longer 
        ! duration - if after 5 minutes, no adjustment made as
        ! time_factor/time_factor_prev=1
        ! -------------------------------------------------------------
        !time_factor_prev = MIN((tornado_dur(i,j) - dt)/900.,1.)
        !IF ( ( time_factor_prev .GT. 0. ) .AND. &
        !   ( time_factor_prev .LT. 1. ) ) THEN
        !  afwa_tornado(i,j) = afwa_tornado(i,j) * time_factor/time_factor_prev
        !ENDIF

        ! Now that we are comparing apples to apples, see if current tornado
        ! wind is higher than previous highest value for this point
        ! ------------------------------------------------------------------
        IF ( tornado .GT. afwa_tornado(i,j) ) THEN
          afwa_tornado(i,j) = tornado
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE severe_wx_diagnostics



  SUBROUTINE vert_int_liquid_diagnostics ( vil                  &
                             , radarvil                         &
                             , t_phy                            &
                             , qrain                            &
                             , qsnow                            &
                             , qgrpl                            &
                             , z_e                              &
                             , dz8w                             &
                             , rho                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN) ::                                     rho  &
                                              ,          qrain  &
                                              ,          qsnow  &
                                              ,          qgrpl  & 
                                              ,          t_phy  &
                                              ,            z_e  & 
                                              ,           dz8w

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                                  vil  &
                                              ,       radarvil

    ! Local
    ! -----
    INTEGER :: i,j,k,ktime

    ! Calculate vertically integrated liquid water (though its mostly not
    ! "liquid" now is it?) [kg/m^2]
    ! -------------------------------------------------------------------
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)
      vil (i,j) = 0.0
      DO k = kps, MIN(kpe,kde-1)
        vil (i,j) =  vil (i,j) + &
         (qrain (i,k,j) + &
          qsnow (i,k,j) + &
          qgrpl (i,k,j))  &
          *dz8w (i,k,j) * rho(i,k,j)
      ENDDO
    ENDDO
    ENDDO

    ! Diagnose "radar-derived VIL" from equivalent radar reflectivity
    ! radarVIL = (integral of LW*dz )/1000.0  (in kg/m^2)
    ! LW = 0.00344 * z_e** (4/7)  in g/m^3
    ! ---------------------------------------------------------------
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)
      radarvil (i,j) = 0.0
      DO k = kps, MIN(kpe,kde-1)
        radarvil (i,j) = radarvil (i,j) + &
        0.00344 * z_e(i,k,j)**0.57143 &
        *dz8w (i,k,j)/1000.0
      END DO
    END DO
    END DO

  END SUBROUTINE vert_int_liquid_diagnostics



  SUBROUTINE icing_diagnostics ( icing_opt                      &
                             , fzlev                            &
                             , icing_lg                         &
                             , icing_sm                         & 
                             , qicing_lg_max                    &
                             , qicing_sm_max                    &
                             , qicing_lg                        &
                             , qicing_sm                        &
                             , icingtop                         &
                             , icingbot                         &
                             , t_phy                            &
                             , z                                &
                             , dz8w                             &
                             , rho                              &
                             , qrain                            &
                             , qcloud                           &
                             , ncloud                           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    INTEGER, INTENT(IN) :: icing_opt

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN) ::                                       z  &
                                              ,          qrain  &
                                              ,         qcloud  &
                                              ,         ncloud  &
                                              ,            rho  &
                                              ,           dz8w  &
                                              ,          t_phy

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                fzlev  &
                                              ,       icing_lg  &
                                              ,       icing_sm  &
                                              ,  qicing_lg_max  &
                                              ,  qicing_sm_max  &
                                              ,       icingtop  &
                                              ,       icingbot

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(  OUT) ::                            qicing_lg  &
                                              ,      qicing_sm
         

    ! Local
    ! -----
    INTEGER :: i,j,k,ktime,ktop,kbot
    REAL    :: qcfrac_lg, qcfrac_sm, qc, qr, small, all

    ! Initializations
    ! ---------------
    fzlev (ips:ipe,jps:jpe) = -999.        ! Arbitrary unset/initial value
    icingtop (ips:ipe,jps:jpe) = -999.     ! Arbitrary unset/initial value
    icingbot (ips:ipe,jps:jpe) = -999.     ! Arbitrary unset/initial value
    icing_lg (ips:ipe,jps:jpe) = 0.        
    icing_sm (ips:ipe,jps:jpe) = 0.
    qicing_lg_max (ips:ipe,jps:jpe) = 0. 
    qicing_sm_max (ips:ipe,jps:jpe) = 0. 
    qicing_sm(ips:ipe,kps:kpe,jps:jpe)=0.
    qicing_lg(ips:ipe,kps:kpe,jps:jpe)=0.   

    ! Loop through i and j
    ! --------------------
    DO i = ips, MIN(ipe,ide-1)
    DO j = jps, MIN(jpe,jde-1)

      ! Go up the column and look for sub freezing temperatures
      ! -------------------------------------------------------
      ktop=-1
      kbot=-1
      DO k = kps, MIN(kpe,kde-1)
        IF (t_phy(i,k,j) .lt. 273.15) THEN

          ! Any cloud water we find will be supercooled.
          ! Based on microphysics scheme, determine the fraction of
          ! large (>50 um) supercooled cloud water drops.
          ! Source: Becky Selin, 16WS
          ! -------------------------------------------------------
          qc = qcloud (i,k,j)
          qr = qrain (i,k,j)
          nc = ncloud(i,k,j)
          den = rho(i,k,j)
          qcfrac_lg = 0.
          qcfrac_sm = 0.
          
          ! Eta (Ferrier)
          ! -------------
          IF (icing_opt .eq. 2) THEN
            IF (qc .lt. 2.5E-4) THEN
              qcfrac_lg = 395000. * qc**2. + 102.9 * qc
            ELSEIF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 276.1 * qc - 0.01861
            ELSE
              qcfrac_lg = 0.3 * log(641.789 * qc) + 0.4
            ENDIF

          ! Thompson
          ! --------
          ! RAS Per James McCormick's stats, more large supercooled
          ! drops are needed from the Thompson members.  Changing 
          ! calculation to be like WSM5/6 members.
          !ELSEIF (icing_opt .eq. 3) THEN
          !  IF (qc .lt. 1.0E-3) THEN
          !    qcfrac_lg = 2205.0 * qc**2. + 3.232 * qc
          !  ELSEIF (qc .lt. 3.0E-3) THEN
          !    qcfrac_lg = 24.1 * qc - 0.01866
          !  ELSE
          !    qcfrac_lg = 0.127063 * log(550.0 * qc) - 0.01
          !  ENDIF

          ! Thompson or WSM5/6
          ! ------------------
          !ELSEIF (icing_opt .eq. 4) THEN
          ELSEIF ((icing_opt .eq. 3) .OR. (icing_opt .eq. 4)) THEN
            IF (qc .lt. 5.E-4) THEN
              qcfrac_lg = 50420.0 * qc**2. + 29.39 * qc
            ELSEIF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 97.65 * qc - 0.02152
            ELSE
              qcfrac_lg = 0.2 * log(646.908 * qc) + 0.135
            ENDIF

          ! Morrison 2-moment, constant CCN
          ! -------------------------------
          ELSEIF (icing_opt .eq. 5) THEN
            IF (qc .lt. 1.4E-3) THEN
              qcfrac_lg = 28000. * qc**2. + 0.1 * qc 
            ELSEIF (qc .lt. 2.6E-3) THEN
              qcfrac_lg = 112.351 * qc - 0.102272
            ELSE 
              qcfrac_lg = 0.3 * log(654.92 * qc) * 0.301607
            ENDIF

          ! WDM6 or Morrison 2-moment w/ prognostic CCN
          ! -------------------------------------------
          ELSEIF ((icing_opt .eq. 6) .OR. (icing_opt .eq. 7)) THEN
            IF ((qc .gt. 1.0E-12) .and. (nc .gt. 1.0E-12)) THEN
               small = -nc * exp(-nc*3141.59265*(5.E-5)**3./(6000.*den*qc))+nc
               all = -nc * exp(-nc*3141.59265*(2.)**3./(6000.*den*qc))+nc
               qcfrac_lg = 1. - (small / all)
            ELSE
               qcfac_lg = 0.
            ENDIF
          ENDIF
          qcfrac_lg = max(qcfrac_lg, 0.)
          
          ! Small (<50 um) supercooled cloud water drop fraction (1 - large).
          ! -----------------------------------------------------------------
          IF (icing_opt .ne. 0 ) THEN
            qcfrac_sm = 1 - qcfrac_lg
          ENDIF

          ! Supercooled drop mixing ratio
          ! -----------------------------
          qicing_lg (i,k,j) = max(qr + qcfrac_lg * qc, 0.)
          qicing_sm (i,k,j) = max(qcfrac_sm * qc, 0.)        

          ! Column integrated icing
          ! -----------------------
          icing_lg (i,j) = icing_lg (i,j) + qicing_lg (i,k,j) &
                            * dz8w (i,k,j) * rho(i,k,j)
          icing_sm (i,j) = icing_sm (i,j) + qicing_sm (i,k,j) &
                            * dz8w (i,k,j) * rho(i,k,j)

          ! Column maximum supercooled drop mixing ratio 
          ! --------------------------------------------
          IF ( qicing_lg(i,k,j) .gt. qicing_lg_max(i,j) ) THEN
            qicing_lg_max (i,j) = qicing_lg(i,k,j)
          ENDIF
          IF ( qicing_sm(i,k,j) .gt. qicing_sm_max(i,j) ) THEN
            qicing_sm_max (i,j) = qicing_sm(i,k,j)
          ENDIF
           
          ! Freezing level calculation
          ! --------------------------
          IF (fzlev (i,j) .eq. -999.) THEN  ! At freezing level
            IF (k .ne. kps) THEN  ! If not at surface, interpolate.      
              fzlev (i,j) = z (i,k-1,j) + &
                             ((273.15 - t_phy (i,k-1,j)) &
                            /(t_phy (i,k,j) - t_phy (i,k-1,j))) &
                            *(z (i,k,j) - z (i,k-1,j))
            ELSE  ! If at surface, use first level.
              fzlev(i,j) = z (i,k,j)
            ENDIF
          ENDIF

          ! Icing layer top and bottom indices (where icing > some arbitrary
          ! small value). Set bottom index of icing layer to current k index 
          ! if not yet set. Set top index of icing layer to current k index.
          ! ----------------------------------------------------------------
          IF ((qicing_lg (i,k,j) + qicing_sm (i,k,j)) .ge. 1.E-5) THEN
            IF (kbot .eq. -1) kbot = k  
            ktop=k
          ENDIF
        ENDIF
      END DO

      ! Interpolate bottom of icing layer from kbot (bottom index of icing
      ! layer). Icing bottom should not go below freezing level.
      ! ------------------------------------------------------------------
      IF (kbot .ne. -1) THEN
        IF (kbot .ne. kps) THEN ! If not at surface, interpolate
          icingbot (i,j) = z (i,kbot-1,j) + ((1.E-5 - &
                   (qicing_lg (i,kbot-1,j) + qicing_sm (i,kbot-1,j))) &
                  / ((qicing_lg (i,kbot,j) + qicing_sm (i,kbot,j)) &
                  - (qicing_lg (i,kbot-1,j) + qicing_sm (i,kbot-1,j)))) &
                  * (z (i,kbot,j) - z (i,kbot-1,j))
          icingbot (i,j) = MAX(icingbot (i,j), fzlev (i,j))
        ELSE  ! If at surface use first level.
          icingbot (i,j) = z(i,kbot,j)
        ENDIF
      ENDIF

      ! Interpolate top of icing layer from ktop (top index of icing layer).
      ! Icing top should not go below icing bottom (obviously).
      ! --------------------------------------------------------------------
      IF (ktop .ne. -1 .and. ktop .ne. kpe) THEN ! If not undefined or model top
        icingtop (i,j) = z (i,ktop,j) + ((1.E-5 - &
                 (qicing_lg (i,ktop,j) + qicing_sm (i,ktop,j))) &
                 / ((qicing_lg (i,ktop+1,j) + qicing_sm (i,ktop+1,j)) &
                 - (qicing_lg (i,ktop,j) + qicing_sm (i,ktop,j)))) &
                 * (z (i,ktop+1,j) - z (i,ktop,j))
        icingtop (i,j) = MAX(icingtop (i,j), icingbot (i,j))
      ENDIF
    END DO
    END DO

  END SUBROUTINE icing_diagnostics



  SUBROUTINE radar_diagnostics ( refd                           &
                             , refd_com                         &
!                             , refd_max                         &
                             , echotop                          &
                             , z                                &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN) ::                                       z  &
                                              ,            z_e

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                                 refd  &
                                              ,       refd_com  &
!                                              ,       refd_max  &
                                              ,        echotop

    ! Local
    ! -----
    INTEGER :: i,j,k,ktime
    
    DO j = jps, MIN(jpe,jde-1)
    DO i = ips, MIN(ipe,ide-1)
      ktop = -1  ! Undefined
      echotop (i,j) = 0.
      refd_com (i,j) = 0.
      refd (i,j) = 0.
      DO k = kps, MIN(kpe,kde-1)
        IF (z_e(i,k,j) .gt. 1.e-20) THEN

          ! Reflectivity (first level)
          ! --------------------------
          IF (k == kps) refd(i,j) = MAX(10.0 * log10(z_e(i,k,j)),0.)
   
!          ! Max reflectivity over the output interval
!          ! -----------------------------------------
!          IF (refd(i,j) .gt. refd_max(i,j)) refd_max(i,j) = refd(i,j)

          ! Composite reflectivity calc (max reflectivity in the column)
          ! ------------------------------------------------------------
          IF (10.0 * log10(z_e(i,k,j)) .gt. refd_com(i,j)) THEN
            refd_com(i,j) = 10.0 * log10(z_e(i,k,j))
          ENDIF
        ENDIF
        
        ! Echo top - the highest level w/ dBZ > 18 (z_e > 63.0957)
        ! --------------------------------------------------------
        IF ( z_e(i,k,j) .gt. 63.0957) THEN
          ktop = k
        ENDIF
      END DO
      IF ( ktop .ne. -1 ) THEN  ! Interpolate to echo top height (GAC)
        echotop (i,j) = z (i,ktop,j) + &
                          ((63.0957 - z_e (i,ktop,j)) &
                         /(z_e (i,ktop+1,j) - z_e (i,ktop,j))) &
                         *(z (i,ktop+1,j) - z (i,ktop,j))
      ENDIF
    END DO
    END DO

  END SUBROUTINE radar_diagnostics



  SUBROUTINE wrf_dbzcalc( rho                                   &
                             , t_phy                            &
                             , qr                               &
                             , qs                               &
                             , qg                               &
                             , z_e                              &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                  rho  &
                                              ,          t_phy  &
                                              ,             qr  &
                                              ,             qs  &
                                              ,             qg

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(  OUT) ::                                  z_e

    REAL :: factor_r, factor_s, factor_g, factorb_s, factorb_g, ronv, sonv, gonv
    REAL :: temp_c, rhoair, qgr, qra, qsn
    INTEGER :: i, j, k

    INTEGER, PARAMETER :: iBrightBand = 1
    REAL, PARAMETER :: T_0 = 273.15
    REAL, PARAMETER :: PI = 3.1415926536
    REAL, PARAMETER :: rgas=287.04, gamma_seven = 720.0, alpha2 = 0.224

    ! Densities of rain, snow, graupel, and cloud ice.
    ! ------------------------------------------------
    REAL, PARAMETER :: rho_w = 1000.0, rho_r = 1000.0, rho_s = 100.0
    REAL, PARAMETER :: rho_g = 400.0, rho_i = 890.0
    REAL, PARAMETER :: ron=8.e6, son=2.e7, gon=5.e7, r1=1.e-15
    REAL, PARAMETER :: ron_min = 8.e6, ron2=1.e10
    REAL, PARAMETER :: ron_qr0 = 0.0001, ron_delqr0 = 0.25*ron_qr0
    REAL, PARAMETER :: ron_const1r = (ron2-ron_min)*0.5
    REAL, PARAMETER :: ron_const2r = (ron2+ron_min)*0.5

    ! Constant intercepts
    ! -------------------
    ronv = 8.e6    ! m^-4
    sonv = 2.e7    ! m^-4
    gonv = 4.e6    ! m^-4

    factor_r = gamma_seven * 1.e18 * (1./(pi*rho_r))**1.75
    factor_s = gamma_seven * 1.e18 * (1./(pi*rho_s))**1.75  &
              * (rho_s/rho_w)**2 * alpha2
    factor_g = gamma_seven * 1.e18 * (1./(pi*rho_g))**1.75  &
              * (rho_g/rho_w)**2 * alpha2

    ! For each grid point
    ! -------------------
    DO j = jps, jpe
    DO k = kps, kpe
    DO i = ips, ipe

      factorb_s = factor_s
      factorb_g = factor_g

      ! In this case snow or graupel particle scatters like liquid
      ! water because it is assumed to have a liquid skin
      ! ----------------------------------------------------------
      IF( iBrightBand == 1 ) THEN
        IF (t_phy(i,k,j) > T_0) THEN
          factorb_s = factor_s /alpha2
          factorb_g = factor_g /alpha2
        ENDIF
      ENDIF
 
      ! Calculate variable intercept parameters
      ! ---------------------------------------
      temp_c = amin1(-0.001, t_phy(i,k,j)- T_0)
      sonv = amin1(2.0e8, 2.0e6*exp(-0.12*temp_c))
      gonv = gon
      qgr = QG(i,k,j)
      qra = QR(i,k,j)
      qsn = QS(i,k,j)
      IF (qgr.gt.r1) THEN
        gonv = 2.38*(pi*rho_g/(rho(i,k,j)*qgr))**0.92
        gonv = max(1.e4, min(gonv,gon))
      ENDIF
      ronv = ron2
      IF (qra.gt. r1) THEN
        ronv = ron_const1r*tanh((ron_qr0-qra)/ron_delqr0) + ron_const2r
      ENDIF
 
      IF (qra < 0.0 ) qra = 0.0
      IF (qsn < 0.0 ) qsn = 0.0
      IF (qgr < 0.0 ) qgr = 0.0
      z_e(i,k,j) = factor_r * (rho(i,k,j) * qra)**1.75 / ronv**.75 + &
                     factorb_s * (rho(i,k,j) * qsn)**1.75 / sonv**.75 + &
                     factorb_g * (rho(i,k,j) * qgr)**1.75 / gonv**.75
 
      IF ( z_e(i,k,j) < 0.0 ) z_e(i,k,j) = 0.0
 
    END DO
    END DO
    END DO

  END SUBROUTINE wrf_dbzcalc



  SUBROUTINE precip_type_diagnostics ( t_phy                    &
                             , t2                               &
                             , rh                               &
                             , z                                &
                             , ht                               &
                             , precip                           &
                             , swdown                           &
                             , rain                             &
                             , snow                             &
                             , ice                              &
                             , frz_rain                         &
                             , snowfall                         &
                             , ccn_tmp                          &
                             , total_melt                       &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                t_phy  &
                                              ,             rh  &
                                              ,              z
    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                   t2  &
                                              ,             ht  &
                                              ,         precip  &
                                              ,         swdown
    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT) ::                             snowfall  &
                                              ,           rain  &
                                              ,       frz_rain  &
                                              ,           snow  &
                                              ,            ice
    REAL, INTENT(IN) :: ccn_tmp
    REAL, INTENT(IN) :: total_melt

    ! Local
    ! -----
    REAL, DIMENSION( ims:ime, jms:jme ) ::                      &
                                     melt                       &
                                   , mod_2m_tmp                 &
                                   , cloud_top_tmp              &
                                   , maxtmp

    INTEGER, DIMENSION( ims:ime, jms:jme ) ::                   &
                                     cloud_top_k_index          &
                                   , precip_type

    LOGICAL, DIMENSION (ims:ime, jms:jme ) ::                   &
                                     saturation 

    REAL, PARAMETER :: snow_ratio=5.0

    ! Loop through all points
    ! Search vertically twice--first to find the cloud top temperature and the 
    ! maximum temperature. Second, determine if any melting or re-freezing will
    ! occur to make ice pellets or freezing rain
    ! -------------------------------------------------------------------------
    DO i=ips,ipe
    DO j=jps,jpe
  
      saturation(i,j)=.false.
      melt(i,j)=0.0 
      precip_type(i,j)=0
        
      ! Modify surface temperature for solar insolation (W/m2)
      ! Set max temperature in the atmopshere
      ! ------------------------------------------------------
      mod_2m_tmp(i,j)=t2(i,j)+(swdown(i,j)/100.0)
      maxtmp(i,j)=mod_2m_tmp(i,j)
  
      ! Only look at points that have precip and are not warm at the surface
      ! --------------------------------------------------------------------
      IF (precip(i,j) .gt. 0.0) THEN
        !IF (mod_2m_tmp(i,j) .gt. 277.15) THEN
        IF (mod_2m_tmp(i,j) .gt. 275.15) THEN
          precip_type(i,j)=1  ! Rain
        ELSE
  
          ! Check sounding from top for saturation (RH-water gt 80%)--this is 
          ! the cloud top. Erase saturation if RH lt 70% (spurious moist layer
          ! aloft)
          ! ------------------------------------------------------------------
          cloud_top_k_index(i,j)=kpe
          DO k=kpe,kps,-1
            IF ((z(i,k,j)-ht(i,j)) .gt. 0.0) THEN
              IF (t_phy(i,k,j) .gt. maxtmp(i,j)) THEN
                maxtmp(i,j)=t_phy(i,k,j)
              ENDIF
              IF (rh(i,k,j) .gt. 80 .and. saturation(i,j) .eqv. .false.) THEN
                cloud_top_tmp(i,j)=t_phy(i,k,j)
                cloud_top_k_index(i,j)=k
                saturation(i,j)=.true.
                precip_type(i,j)=2 ! Snow
              ENDIF
              IF (rh(i,k,j) .le. 70 .and. saturation(i,j) .eqv. .true.) THEN
                saturation(i,j)=.false.
              ENDIF
            ENDIF
          ENDDO

          ! Perform simple check to assign types with no melting layer
          ! shenanigans going on
          ! ----------------------------------------------------------------
          IF (cloud_top_tmp(i,j) .le. ccn_tmp .and. &
          maxtmp(i,j) .le. 273.15) THEN
            precip_type(i,j)=2  ! Snow
          ENDIF

          ! ELSE, have to go through the profile again to see if snow melts, 
          ! and if anything re-freezes
          ! ----------------------------------------------------------------
          DO k=cloud_top_k_index(i,j),kps,-1
            IF ((z(i,k,j)-ht(i,j)) .gt. 0.0) THEN
 
              ! Condition 0--assign falling rain when we get to the 
              ! supercooled temperature if too warm
              ! ---------------------------------------------------
              IF (cloud_top_tmp(i,j) .eq. t_phy(i,k,j) .and. &
              cloud_top_tmp(i,j) .gt. ccn_tmp) THEN
                 precip_type(i,j)=1  ! Rain
              ENDIF

              ! Condition 1--falling frozen precip that will start to melt
              ! Add up melting energy over warm layers--if enough, turn to 
              ! liquid
              ! ----------------------------------------------------------
              IF ((precip_type(i,j) .eq. 2 .or. precip_type(i,j) .eq. 3) .and. &
              t_phy(i,k,j) .gt. 273.15) THEN
                melt(i,j)=melt(i,j)+9.8*(((t_phy(i,k,j)-273.15)/273.15)* &
                          (z(i,k,j)-z(i,k-1,j)))
                IF (melt(i,j) .gt. total_melt) THEN
                  precip_type(i,j)=1  ! Rain
                  melt(i,j)=0.0  ! Reset melting energy in case it re-freezes
                ENDIF
              ENDIF

              ! Condition 2--falling partially melted precip encounters 
              ! sub-freezing air. Snow will be converted to ice pellets if 
              ! at least 1/4 of it melted. Instantaneous freeze-up, simplistic 
              ! --------------------------------------------------------------
              IF (t_phy(i,k,j) .le. 273.15 .and. &
              melt(i,j) .gt. total_melt/4.0 .and. &
              (precip_type(i,j) .eq. 2 .or. precip_type(i,j) .eq. 3)) THEN
                precip_type(i,j)=3  ! Ice
                melt(i,j)=0.0
              ENDIF
             
              ! Condition 3--falling liquid that will re-freeze--must reach 
              ! nucleation temperature
              ! -----------------------------------------------------------
              IF (precip_type(i,j) .eq. 1) THEN
                IF (t_phy(i,k,j) .le. ccn_tmp) THEN
                  precip_type(i,j)=3  ! Ice
                ENDIF
              ENDIF
            ENDIF  ! End if (z-ht)>0
          ENDDO  ! End do k=kpe,kps,-1
        ENDIF  ! End if mod_2m_tmp>273.15

        ! Accumulate precip according to precip_type
        ! ------------------------------------------
        IF (precip_type(i,j) .eq. 3) THEN 
          ice(i,j)=ice(i,j)+precip(i,j)
        ENDIF
        IF (precip_type(i,j) .eq. 2) THEN
          snow(i,j)=snow(i,j)+precip(i,j)
          snowfall(i,j)=snowfall(i,j)+snow_ratio*precip(i,j) &
                        *(5.-mod_2m_tmp(i,j)+273.15)**0.4
        ENDIF
        IF (precip_type(i,j) .eq. 1) THEN
          IF (mod_2m_tmp(i,j) .gt. 273.15) THEN
            rain(i,j)=rain(i,j)+precip(i,j)
          ELSE
            frz_rain(i,j)=frz_rain(i,j)+precip(i,j)
          ENDIF
        ENDIF

      ENDIF  ! End if precip>0

    ENDDO  ! End do j=jps,jpe
    ENDDO  ! End do i=ips,ipe

  END SUBROUTINE precip_type_diagnostics



  SUBROUTINE vis_diagnostics ( qcloud                           &
                             , qrain                            &
                             , qice                             &
                             , qsnow                            &
                             , qgrpl                            &
                             , rho                              &
                             , wind10m                          &
                             , wind125m                         &
                             , pwater                           &
                             , q2m                              &
                             , rh2m                             &
                             , rh20m                            &
                             , tv2m                             &
                             , tv20m                            &
                             , dustc                            &
                             , vis                              &
                             , vis_dust                         &
                             , vis_alpha                        &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    INTEGER, PARAMETER :: ndust=5

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                               qcloud  &
                                              ,          qrain  &
                                              ,           qice  & 
                                              ,          qsnow  & 
                                              ,          qgrpl  & 
                                              ,            rho  & 
                                              ,        wind10m  & 
                                              ,       wind125m  & 
                                              ,         pwater  & 
                                              ,           rh2m  &
                                              ,            q2m  &
                                              ,          rh20m  &
                                              ,           tv2m  &
                                              ,          tv20m
    REAL, DIMENSION( ims:ime, jms:jme, ndust ),                 &
         INTENT(IN   ) ::                                dustc
    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                  vis  &
                                              ,       vis_dust  &
                                              ,      vis_alpha

    ! Local
    ! -----
    INTEGER :: i,j,k,d
    REAL, PARAMETER :: visfactor=3.912
    REAL, DIMENSION (ndust) :: dustfact
    REAL :: bc, br, bi, bs, dust_extcoeff, hydro_extcoeff, extcoeff, vis_haze
    REAL :: tvd, rh, prob_ext_coeff_gt_p29, haze_ext_coeff
    REAL :: vis_hydlith, alpha_haze

    ! Dust factor based on 5 bin AFWA dust scheme.  This is a simplification
    ! of the scheme in WRFPOST.  More weight is applied to smaller particles.
    ! -----------------------------------------------------------------------
    dustfact=(/1.470E-6,7.877E-7,4.623E-7,2.429E-7,1.387E-7/)

    DO i=ims,ime
      DO j=jms,jme

        ! Hydrometeor extinction coefficient
        ! For now, lump graupel in with rain
        ! -------------------------------------------------------------------
        ! Update: GAC 20131213  Our test results with surface cloud and ice
        ! are very unfavorable.  Model doesn't do a great job handling clouds
        ! at the model surface.  Therefore, we will not trust surface
        ! cloud water/ice.  (Commented out below).
        ! -------------------------------------------------------------------
        !br=2.240*qrain(i,j)**0.75
        !bs=10.36*qsnow(i,j)**0.78
        !bc=144.7*qcloud(i,j)**0.88
        !bi=327.8*qice(i,j)
        !hydro_extcoeff=bc+br+bi+bs
        !br=2.240*(qrain(i,j)+qgrpl(i,j))**0.75
        !bs=10.36*(qsnow(i,j)*rho(i,j))**0.78
        ! Update: moisture variables should be in mass concentration (g m^-3)
        br=1.1*(1000.*rho(i,j)*(qrain(i,j)+qgrpl(i,j)))**0.75
        bs=10.36*(1000.*rho(i,j)*qsnow(i,j))**0.78
        hydro_extcoeff=(br+bs)/1000.   ! m^-1

        ! Dust extinction coefficient
        ! ---------------------------
        dust_extcoeff=0.
        DO d=1,ndust
          dust_extcoeff=dust_extcoeff+dustfact(d)*dustc(i,j,d)
        ENDDO

        ! UPDATE: GAC 20131213 Old algorithm commented out below
        !! Visibility due to haze obscuration
        !! -------------------------------------------------------
        !vis_haze=1500.*(105.-rh2m(i,j)+wind10m(i,j))
        !
        !! Calculate total visibility
        !! Take minimum visibility from hydro/lithometeors and haze
        !! Define maximum visibility as 20 km (UPDATE: 999.999 km)
        !! --------------------------------------------------------
        !extcoeff=hydro_extcoeff+dust_extcoeff
        !IF (extcoeff .gt. 0.) THEN
        !  vis(i,j)=MIN(visfactor/extcoeff,vis_haze)
        !ELSE
        !  vis(i,j)=999999.
        !ENDIF

        ! Update: GAC 20131213  New haze/fog visibility algorithm
        ! Start with relative humidity predictor.  Increase 
        ! predicted visibility as mixing ratio decreases (as
        ! there is less water to condense).
        ! -------------------------------------------------------
        vis_haze=999999.
        IF (q2m(i,j) .gt. 0.) THEN
          !vis_haze=1500.*(105.-rh2m(i,j))*(15./(1000.*q2m(i,j)))
          vis_haze=1500.*(105.-rh2m(i,j))*(5./min(1000.*q2m(i,j),5.))
        ENDIF
 
        ! Calculate a Weibull function "alpha" term.  This can be
        ! used later with visibility (which acts as the "beta" term
        ! in the Weibull function) to create a probability distribution
        ! for visibility. Alpha can be thought of as the "level of
        ! certainty" that beta (model visibility) is correct. Fog is
        ! notoriously difficult to model. In the below algorithm,
        ! the alpha value (certainty) decreases as PWAT, mixing ratio,
        ! or winds decrease (possibly foggy conditions), but increases
        ! if RH decreases (more certainly not foggy).  If PWAT is lower
        ! then there is a higher chance of radiation fog because there
        ! is less insulating cloud above.
        ! -------------------------------------------------------------
        alpha_haze=3.6
        IF (q2m(i,j) .gt. 0.) THEN
          alpha_haze=0.1 + pwater(i,j)/25.     + wind125m(i,j)/3. + &
                          (100.-rh2m(i,j))/10. + 1./(1000.*q2m(i,j))
          alpha_haze=min(alpha_haze,3.6)
        ENDIF
        
        ! Calculate visibility from hydro/lithometeors
        ! Maximum visibility -> 999999 meters
        ! --------------------------------------------
        extcoeff=hydro_extcoeff+dust_extcoeff
        IF (extcoeff .gt. 0.) THEN
          vis_hydlith=min(visfactor/extcoeff, 999999.)
        ELSE
          vis_hydlith=999999.
        ENDIF

        ! Calculate total visibility
        ! Take minimum visibility from hydro/lithometeors and haze
        ! Set alpha to be alpha_haze if haze dominates, or 3.6
        ! (a Guassian distribution) when hydro/lithometeors dominate
        ! ----------------------------------------------------------
        IF (vis_hydlith < vis_haze) THEN
           vis(i,j)=vis_hydlith
           vis_alpha(i,j)=3.6
        ELSE
           vis(i,j)=vis_haze
           vis_alpha(i,j)=alpha_haze
        ENDIF

        ! Calculate dust visibility
        ! Again, define maximum visibility as 20 km
        ! -----------------------------------------
        IF (dust_extcoeff .gt. 0.) THEN
          vis_dust(i,j)=MIN(visfactor/dust_extcoeff,999999.)
        ELSE
          vis_dust(i,j)=999999.
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE vis_diagnostics
  
  

  SUBROUTINE cloud_diagnostics (qcloud                          &
                             , qice                             &
                             , qsnow                            &
                             , rh                               &
                             , dz8w                             &
                             , rho                              &
                             , z                                &
                             , ht                               &
                             , cloud                            &
                             , cloud_ceil                       &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                               qcloud  &
                                              ,           qice  & 
                                              ,          qsnow  & 
                                              ,             rh  & 
                                              ,           dz8w  & 
                                              ,            rho  &
                                              ,              z

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                   ht

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                                cloud  &
                                              ,     cloud_ceil

    ! Local
    ! -----
    INTEGER :: i, j, k
    REAL    :: tot_cld_cond, maxrh, cld_frm_cnd, cld_frm_rh, z_maxrh
    REAL    :: snow_extcoeff, vis_snow, cloud_lo, zagl_up, zagl_lo
    REAL, PARAMETER :: min_ceil = 125.    ! Minimum ceiling of 125 m

    ! Calculate cloud cover based on total cloud condensate, or if none
    ! present, from maximum relative humidity in the column.
    ! -----------------------------------------------------------------
    DO i=ims,ime
      DO j=jms,jme

        ! Initialize some key variables
        ! -----------------------------
        tot_cld_cond = 0.
        cloud(i,j) = 0.
        maxrh = -9999.
        cloud_ceil(i,j) = -9999.
        cloud_lo = 0.

        ! Now go up the column to find our cloud base
        ! -------------------------------------------
        DO k=kms,kme

          ! Total cloud condensate
          ! Don't trust modeled cloud below 125 m.  We
          ! let the alpha curve determine probabilities
          ! below 125 m...
          ! ---------------------------------------------
          IF ( z(i,k,j) - ht (i,j) .gt. min_ceil ) THEN

            ! Maximum column relative humidity
            ! --------------------------------
            IF (rh (i,k,j) .gt. maxrh) THEN
              maxrh = rh (i,k,j)
              z_maxrh = z(i,k,j)
            ENDIF

!            ! Cloud cover parameterization. Take maximum value
!            ! from condensate and relative humidity terms.
!            ! ------------------------------------------------
!            tot_cld_cond = tot_cld_cond + (qcloud (i,k,j) + qice (i,k,j) &
!                           + qsnow (i,k,j)) * dz8w (i,k,j) * rho(i,k,j)
!            cld_frm_cnd = 50. * tot_cld_cond
!            cld_frm_rh = MAX(((maxrh - 70.) / 30.),0.)
!            cloud (i,j) = MAX(cld_frm_cnd,cld_frm_rh)

            ! Calculate cloud cover beta value by summing 
            ! relative humidity above 70% as we go up the
            ! column.  Assume a higher probability of a
            ! cloud if we have an accumulation of high
            ! relative humidity over a typical cloud
            ! depth of 500m. (Note dz8w is distance 
            ! between full eta levels. Note also that rh
            ! is derived from the sum of qvapor, qcloud,
            ! and qcloud, with a maximum of 100%). 
            ! -------------------------------------------
            cld_frm_rh = MAX(((rh (i,k,j) - 90.) / 10.),0.)
            cloud (i,j) = cloud (i,j) + ( cld_frm_rh * dz8w (i,k,j) ) / 250.

            ! Calculate cloud ceiling, the level at which
            ! parameterization of cloud cover exceeds 80%
            ! Once we exceed the 80% threshold, we will
            ! interpolate downward to find the ceiling.
            ! If this is the lowest level, then we will
            ! simply set ceiling to that level.  After
            ! we interpolate, if ceiling is below the 
            ! minimum we trust, we set it to the minimum.
            ! -------------------------------------------
            IF ( cloud_ceil (i,j) .eq. -9999. .and. cloud (i,j) .gt. 0.8 ) THEN
              zagl_up = z (i,k,j) - ht (i,j)
              IF ( k .EQ. kps ) THEN
                cloud_ceil (i,j) = zagl_up
              ELSE
                zagl_lo = z (i,k-1,j) - ht (i,j)
                cloud_ceil (i,j) = zagl_lo + &
                             ((0.8 - cloud_lo) / &
                             (cloud (i,j) - cloud_lo)) * &
                             (zagl_up - zagl_lo)
                cloud_ceil (i,j) = MAX(cloud_ceil (i,j),ceil_min)
              ENDIF
            ENDIF
            
            ! Save cloud amount here to use for interpolation
            ! -----------------------------------------------
            cloud_lo=cloud(i,j)
          ENDIF
        ENDDO

        ! If we did not encounter any definitive cloud earlier, we set
        ! cloud ceiling to the level of maximum relative humidity. (If
        ! there is any cloud, this is our best guess as to where it
        ! will reside in the vertical). Height is AGL.
        ! ------------------------------------------------------------
        IF (cloud_ceil (i,j) .eq. -9999.) THEN
          cloud_ceil (i,j) = z_maxrh - ht (i,j)
        ENDIF

        ! Compare cloud ceiling to surface visibility reduction due to snow
        ! Note difference from horizontal visibility algorithm for snow
        ! -----------------------------------------------------------------
        IF (qsnow (i,1,j) .GT. 0. .AND. rho (i,1,j) .GT. 0.) THEN
          snow_extcoeff = 25. * (1000. * rho(i,1,j) * qsnow (i,1,j))**0.78
          snow_extcoeff = snow_extcoeff / 1000.
          vis_snow = 3.912 / snow_extcoeff
          IF (vis_snow .LT. cloud_ceil (i,j)) cloud_ceil (i,j) = vis_snow
        ENDIF

      ENDDO
    ENDDO

  END SUBROUTINE cloud_diagnostics



  SUBROUTINE thermal_diagnostics ( t2                           &
                             , psfc                             &
                             , rh2m                             &
                             , wind10m                          &
                             , heatidx                          &
                             , wchill                           &
                             , fits                             &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(IN   ) ::                                   t2  &
                                              ,           psfc  &
                                              ,           rh2m  & 
                                              ,        wind10m

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                              heatidx  &
                                              ,         wchill  & 
                                              ,           fits

    ! Local
    ! -----
    INTEGER :: i, j

    DO i=ims,ime
      DO j=jms,jme
       
        ! Heat Index
        ! ----------
        heatidx ( i, j ) = calc_hi   (   t2      ( i, j )    &
                                       , rh2m    ( i, j ) )

        ! Wind Chill
        ! ----------
        wchill  ( i, j ) = calc_wc   (   t2      ( i, j )    &
                                       , wind10m ( i, j ) )

        ! Fighter Index of Thermal Stress
        ! -------------------------------
        fits    ( i, j ) = calc_fits (   psfc    ( i, j )    &
                                       , t2      ( i, j )    &
                                       , rh2m    ( i, j ) )

      ENDDO
    ENDDO

  END SUBROUTINE thermal_diagnostics



  SUBROUTINE turbulence_diagnostics ( u_phy                     &
                             , v_phy                            &
                             , t_phy                            &
                             , p                                &
                             , zagl                             &
                             , defor11                          &
                             , defor12                          &
                             , defor22                          &
                             , turb                             &
                             , llturb                           &
                             , llturblgt                        &
                             , llturbmdt                        &
                             , llturbsvr                        &
                             , nlyrs                            &
                             , lyrbot                           &
                             , lyrtop                           &
                             , ids, ide, jds, jde, kds, kde     &
                             , ims, ime, jms, jme, kms, kme     &
                             , ips, ipe, jps, jpe, kps, kpe )

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe

    INTEGER, INTENT(IN) :: nlyrs

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               &
         INTENT(IN   ) ::                                u_phy  &
                                              ,          v_phy  &
                                              ,          t_phy  & 
                                              ,              p  & 
                                              ,           zagl  &
                                              ,        defor11  & 
                                              ,        defor12  & 
                                              ,        defor22

    REAL, DIMENSION( nlyrs ),                                   &
         INTENT(IN   ) ::                               lyrtop  &
                                              ,         lyrbot

    REAL, DIMENSION( ims:ime, nlyrs, jms:jme ),                 &
         INTENT(  OUT) ::                                 turb   

    REAL, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(  OUT) ::                               llturb  &
                                              ,      llturblgt  &
                                              ,      llturbmdt  & 
                                              ,      llturbsvr

    ! Local
    ! -----
    INTEGER :: i, j, k, n, bot, top, nlayer

    REAL ::                                            ugrdtop  &
                                              ,        ugrdbot  &
                                              ,        vgrdtop  &
                                              ,        vgrdbot  &
                                              ,     defor11top  &
                                              ,     defor11bot  &
                                              ,     defor12top  &
                                              ,     defor12bot  &
                                              ,     defor22top  &
                                              ,     defor22bot

    REAL, DIMENSION( kms:kme )            ::         this_zagl  &
                                              ,        this_tK  &
                                              ,         this_p  &
                                              ,         this_u  &
                                              ,         this_v

    REAL :: wind, therm, mtn_wave, tpd_wave

    !~ Initialize variables.
    !  ----------------------
    turb = REAL ( 0 )
    llturb = REAL ( 0 )
    llturblgt = REAL ( 0 )
    llturbsvr = REAL ( 0 )

    !~ Loop through the grid.
    !  ----------------------
    DO i=ims,ime
      DO j=jms,jme
       
        !~ Loop through the turbulence layers
        !  ----------------------------------
        DO n = 1, nlyrs

          !~ Interpolate relevent variables to turbulence layers
          !  ---------------------------------------------------
          ugrdtop    = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   ,   u_phy ( i, kms:kme-1, j )  &
                                   ,             lyrtop  ( n ) )
          ugrdbot    = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   ,   u_phy ( i, kms:kme-1, j )  &
                                   ,             lyrbot  ( n ) )
          vgrdtop    = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   ,   v_phy ( i, kms:kme-1, j )  &
                                   ,             lyrtop  ( n ) )
          vgrdbot    = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   ,   v_phy ( i, kms:kme-1, j )  &
                                   ,             lyrbot  ( n ) )
          defor11top = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor11 ( i, kms:kme-1, j )  &
                                   ,             lyrtop  ( n ) )
          defor11bot = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor11 ( i, kms:kme-1, j )  &
                                   ,             lyrbot  ( n ) )
          defor12top = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor12 ( i, kms:kme-1, j )  &
                                   ,             lyrtop  ( n ) )
          defor12bot = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor12 ( i, kms:kme-1, j )  &
                                   ,             lyrbot  ( n ) )
          defor22top = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor22 ( i, kms:kme-1, j )  &
                                   ,             lyrtop  ( n ) )
          defor22bot = lin_interp (     zagl ( i, kms:kme-1, j )  &
                                   , defor22 ( i, kms:kme-1, j )  &
                                   ,             lyrbot  ( n ) )

          !~ Compute Knapp-Ellrod clear air turbulence
          !  -----------------------------------------
          turb ( i, n, j ) = CATTurbulence (                       ugrdbot  &
                                               ,                   ugrdtop  &
                                               ,                   vgrdbot  &
                                               ,                   vgrdtop  &
                                               ,                defor11bot  &
                                               ,                defor11top  &
                                               ,                defor12bot  &
                                               ,                defor12top  &
                                               ,                defor22bot  &
                                               ,                defor22top  &
                                               ,                lyrbot (n)  &
                                               ,                lyrtop (n) )

        ENDDO
 
        !~ Get top and bottom index of 0-1500 m AGL layer
        !  ----------------------------------------------
        bot = kms
        top = kms
        DO k=kms+1,kme
          IF ( zagl ( i, k, j ) .gt. 1500. ) THEN
            top = k
            EXIT
          ENDIF
        ENDDO
        nlayer = top - bot + 1  !~ Number of layers from bottom to top

        !~ Copy current column at this i,j point into working arrays
        !  ---------------------------------------------------------
        this_zagl = zagl  ( i, kms:kme, j )
        this_tK   = t_phy ( i, kms:kme, j )
        this_p    = p     ( i, kms:kme, j )
        this_u    = u_phy ( i, kms:kme, j )
        this_v    = v_phy ( i, kms:kme, j )
                           
        !~ Interpolate req'd vars to the 1500 m level
        !~ Overwrite the "top" index with these values
        !  -------------------------------------------
        this_zagl ( top ) = 1500.
        this_tK ( top )   = lin_interp (  zagl ( i, kms:kme-1, j )  &
                                     ,   t_phy ( i, kms:kme-1, j )  &
                                     ,           this_zagl (top) )
        this_p ( top )    = lin_interp (  zagl ( i, kms:kme-1, j )  &
                                     ,       p ( i, kms:kme-1, j )  &
                                     ,           this_zagl (top) )
        this_u ( top )    = lin_interp (  zagl ( i, kms:kme-1, j )  &
                                     ,   u_phy ( i, kms:kme-1, j )  &
                                     ,           this_zagl (top) )
        this_v ( top )    = lin_interp (  zagl ( i, kms:kme-1, j )  &
                                     ,   v_phy ( i, kms:kme-1, j )  &
                                     ,           this_zagl (top) )

        !~ Compute the low level turbulence index (from 0 - 1500 m AGL)
        !~ There are four components to this index: a wind speed term,
        !~ thermodynamic term, mountain wave term, and trapped wave term.
        !~ These terms will utilize the working arrays we have just
        !~ defined, using only the portion of the array valid in the
        !~ 0-1500 m layer, e.g. this_u (bot:top).
        !~ The algorithm itself was developed by:
        !~ 
        !~ Mr. James McCormick
        !~ Aviation Hazards Team
        !~ Air Force Weather Agency 16WS/WXN
        !~ DSN: 271-1689 Comm: (402) 294-1689
        !~ James.McCormick.ctr@offutt.af.mil
        !  --------------------------------------------------------------  !
        !~ Step 1: Compute the wind speed term                            ~!
        !  --------------------------------------------------------------  !
        wind = LLT_WindSpeed ( nlayer, this_u (bot:top) &
                                , this_v (bot:top) )

        !  --------------------------------------------------------------  !
        !~ Step 2: Compute the thermodynamic term                         ~!
        !  --------------------------------------------------------------  !
        therm = LLT_Thermodynamic ( nlayer, this_tK(bot:top) &
                                , this_zagl(bot:top) )

        !  --------------------------------------------------------------  !
        !~ Step 3: Compute the mountain wave term                         ~!
        !  --------------------------------------------------------------  !
        mtn_wave = LLT_MountainWave ( nlayer, terrain_dx, terrain_dy &
                                , this_u(bot:top), this_v(bot:top)   &
                                , this_tK(bot:top), this_zagl(bot:top) )

        !  --------------------------------------------------------------  !
        !~ Step 4: Compute the trapped wave term                          ~!
        !  --------------------------------------------------------------  !
        tpd_wave = LLT_TrappedWave ( nlayer, this_u(bot:top) &
                                , this_v(bot:top), this_p(bot:top) )

        !  --------------------------------------------------------------  !
        !~ Step 5: Combine the above and arrive at the turbulence index.  ~!
        !  --------------------------------------------------------------  !
        llturb ( i,j ) = 1.-((1.-wind)*(1.-therm)*(1.-mtn_wave)*(1.-tpd_wave))

        !~ Compute probabilities of light, moderate, and severe LLT
        !  --------------------------------------------------------
        llturblgt ( i,j ) = (((((((llturb (i,j) * REAL (100))-REAL (30)) &
                                        *2.5)*.01)**2)*0.75)*REAL(100))
        IF ( llturb (i,j) < 0.3   ) llturblgt ( i,j ) = REAL ( 0 )
        IF ( llturblgt (i,j) > REAL (90) ) llturblgt ( i,j ) = REAL ( 90 )

        llturbmdt ( i,j ) = (((((((llturb (i,j) * REAL (100))-REAL (35)) &
                                     *2.22222)*.01)*0.75)**2)*88.88888)
        IF ( llturb (i,j) < 0.35  ) llturbmdt ( i,j ) = REAL ( 0 )
        IF ( llturbmdt (i,j) > REAL (70) ) llturbmdt ( i,j ) = REAL ( 70 )

        llturbsvr ( i,j ) = (((((((llturb (i,j) * REAL (100))-REAL (40)) &
                                     *REAL(2))*.01)*0.5)**2)*REAL(100))
        IF ( llturb (i,j) < 0.40  ) llturbsvr ( i,j ) = REAL ( 0 )
        IF ( llturbsvr (i,j) > REAL (35) ) llturbsvr ( i,j ) = REAL ( 35 )

      ENDDO
    ENDDO

  END SUBROUTINE turbulence_diagnostics

END MODULE module_diag_afwa

#endif
