!WRF:MEDIATION_LAYER:PHYSICS

#if (NMM_CORE == 1)
MODULE diag_functions
CONTAINS
   SUBROUTINE diag_functions_stub
   END SUBROUTINE diag_functions_stub
END MODULE diag_functions
#else

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
#endif
