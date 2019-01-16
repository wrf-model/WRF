!
! CRTM_Parameters
!
! Module of parameter definitions for the CRTM.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 31-Jul-2000
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Parameters


  ! ---------------------
  ! Module use statements
  ! ---------------------
  USE Type_Kinds ,   ONLY: fp

  ! ------------------
  ! Default visibility
  ! ------------------
  ! Everything PRIVATE by default
  PRIVATE
  ! The MAX_N_CHANNELS methods
  PUBLIC :: CRTM_Set_Max_nChannels    ! Subroutine
  PUBLIC :: CRTM_Reset_Max_nChannels  ! Subroutine
  PUBLIC :: CRTM_Get_Max_nChannels    ! Function
  PUBLIC :: CRTM_IsSet_Max_nChannels  ! Function


  !#----------------------------------------------------------------------------#
  !#                    -- ALGORITHM INDEPENDENT PARAMETERS --                  #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------------------------------
  ! Number of channels (for ALL satellites - really the number
  ! of satellites x number of channels USED per satellite)
  !
  ! This is also the number of lines in the satellite 
  ! information (satinfo) file used in the NCEP GDAS.
  !
  ! The number of channels that can be used is determined,
  ! and SET, during the model initialisation.
  !
  ! In this module it is a protected variable in that it can
  ! only be set, reset, or retrieved via the *_N_CHANNELS
  ! methods.
  ! ----------------------------------------------------------
  INTEGER, PRIVATE, PARAMETER :: RESET_VALUE = -1
  INTEGER, PRIVATE, SAVE      :: MAX_N_CHANNELS = RESET_VALUE


  ! -----------------------------------------
  ! The maximum number of sensors that can be
  ! specified in a single initialisation call
  ! -----------------------------------------
  INTEGER, PUBLIC, PARAMETER :: MAX_N_SENSORS = 100

  ! -----------------------------------------------------
  ! The maximum number of atmospheric profiles and layers
  ! accepted. These values are arbitrary. Nothing magical
  ! -----------------------------------------------------
  INTEGER, PUBLIC, PARAMETER :: MAX_N_LAYERS   = 200


  ! -----------------
  ! Literal constants
  ! -----------------
  REAL(fp), PUBLIC, PARAMETER :: ZERO          =  0.0_fp
  REAL(fp), PUBLIC, PARAMETER :: ONE           =  1.0_fp
  REAL(fp), PUBLIC, PARAMETER :: TWO           =  2.0_fp
  REAL(fp), PUBLIC, PARAMETER :: THREE         =  3.0_fp
  REAL(fp), PUBLIC, PARAMETER :: FOUR          =  4.0_fp
  REAL(fp), PUBLIC, PARAMETER :: FIVE          =  5.0_fp
  REAL(fp), PUBLIC, PARAMETER :: TEN           = 10.0_fp
  REAL(fp), PUBLIC, PARAMETER :: POINT_25      =  0.25_fp
  REAL(fp), PUBLIC, PARAMETER :: POINT_5       =  0.5_fp
  REAL(fp), PUBLIC, PARAMETER :: POINT_75      =  0.75_fp
  REAL(fp), PUBLIC, PARAMETER :: ONEpointFIVE  =  1.5_fp
                                                                                                        

  ! --------------------
  ! PI-related constants
  ! --------------------
  REAL(fp), PUBLIC, PARAMETER :: PI = 3.141592653589793238462643383279_fp
  REAL(fp), PUBLIC, PARAMETER :: TWOPI = TWO * PI
  REAL(fp), PUBLIC, PARAMETER :: DEGREES_TO_RADIANS = PI / 180.0_fp
  REAL(fp), PUBLIC, PARAMETER :: RADIANS_TO_DEGREES = 180.0_fp / PI


  ! -------------------------
  ! Direction flags
  !   DOWN == From TOA to SFC
  !   UP   == From SFC to TOA
  ! -------------------------
  INTEGER, PUBLIC, PARAMETER :: DOWN = 0
  INTEGER, PUBLIC, PARAMETER :: UP   = 1


  ! ------------------------
  ! Invalid sensor ID values
  ! ------------------------
  INTEGER, PUBLIC, PARAMETER :: INVALID_NCEP_SENSOR_ID   = -1
  INTEGER, PUBLIC, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PUBLIC, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  

  ! -------------------------
  ! Yes/No, Set/Unset flags
  ! -------------------------
  INTEGER, PUBLIC, PARAMETER :: NO      = 0, YES = 1
  INTEGER, PUBLIC, PARAMETER :: NOT_SET = 0, SET = 1

  ! Default string length for SensorIDs
  INTEGER, PUBLIC, PARAMETER :: STRLEN = 20


  !#----------------------------------------------------------------------------#
  !#                       -- AtmAbsorption PARAMETERS --                       #
  !#----------------------------------------------------------------------------#

  ! Algorithm IDs
  INTEGER, PUBLIC, PARAMETER :: ODAS_ALGORITHM     = 1
  INTEGER, PUBLIC, PARAMETER :: ODPS_ALGORITHM     = 2
  INTEGER, PUBLIC, PARAMETER :: ODSSU_ALGORITHM    = 3

  ! Maximum number of absorbers in the gas absorption models
  ! ---------------------------------------------------------------
  INTEGER, PUBLIC, PARAMETER :: MAX_N_ABSORBERS = 7

  ! ----------------------------------------------
  ! The minimum absorber amount allowed based upon
  ! the smallest representable numbers.
  ! This value is equivalent to TINY(ONE)**0.25
  ! ----------------------------------------------
  REAL(fp), PUBLIC, PARAMETER :: MINIMUM_ABSORBER_AMOUNT = 1.0e-076_fp   ! TEN**(-RANGE(ONE)/4)

  ! ---------------------------------------
  ! Numerical limits for the gas absorption
  ! coefficient reconstruction
  ! ---------------------------------------
  ! Numerical limits based on precision
!  REAL(fp), PUBLIC, PARAMETER :: LIMIT_EXP = 36.0436_fp   ! ABS( LOG( TOLERANCE ) )
!  REAL(fp), PUBLIC, PARAMETER :: LIMIT_LOG = 4.5e+15_fp   ! EXP( LIMIT_EXP )
  ! Numerical limits based on experiment.
  REAL(fp), PUBLIC, PARAMETER :: LIMIT_EXP = 20.0_fp
  REAL(fp), PUBLIC, PARAMETER :: LIMIT_LOG = 4.8e+08_fp   ! EXP( LIMIT_EXP )


  ! ---------------------------------------
  ! Top-Of-Atmosphere (TOA) pressure in hPa
  ! ---------------------------------------
  REAL(fp), PUBLIC, PARAMETER :: TOA_PRESSURE = 0.005_fp


  ! -------------------------------------------------------
  ! Reciprocal gravity (scaled by 100 for use with pressure
  ! in hPa) used in computing integrated absorber amounts
  ! -------------------------------------------------------
  REAL(fp), PUBLIC, PARAMETER :: RECIPROCAL_GRAVITY = ONE / 980.665_fp


  ! ----------------------------------------------------------
  ! Cosmic background temperature. Taken from
  ! Mather,J.C. et. al., 1999, "Calibrator Design for the COBE
  !    Far-Infrared Absolute Spectrophotometer (FIRAS)"
  !    Astrophysical Journal, vol 512, pp 511-520
  ! ----------------------------------------------------------
  REAL(fp), PUBLIC, PARAMETER :: TSPACE = 2.7253_fp



  !#----------------------------------------------------------------------------#
  !#                         -- Geometry PARAMETERS --                          #
  !#----------------------------------------------------------------------------#

  ! -----------------
  ! Distance defaults
  ! -----------------
  REAL(fp), PUBLIC, PARAMETER :: EARTH_RADIUS     = 6370.0_fp  ! Mean earth radius 
  REAL(fp), PUBLIC, PARAMETER :: SATELLITE_HEIGHT = 800.0_fp


  ! ----------------
  ! Altitude extrema
  ! ----------------
  REAL(fp), PUBLIC, PARAMETER :: MIN_SURFACE_ALTITUDE = -400.0_fp ! Dead sea
  REAL(fp), PUBLIC, PARAMETER :: MAX_SURFACE_ALTITUDE = 8900.0_fp ! Chomolungma


  ! -----------------------------------
  ! Limits on sensor angles.
  ! - Azimuth angles that are multiples
  !   of 2pi are not accepted.
  ! -----------------------------------
  REAL(fp), PUBLIC, PARAMETER :: MAX_SENSOR_SCAN_ANGLE    = 80.0_fp
  REAL(fp), PUBLIC, PARAMETER :: MAX_SENSOR_ZENITH_ANGLE  = 80.0_fp
  REAL(fp), PUBLIC, PARAMETER :: MAX_SENSOR_AZIMUTH_ANGLE = 360.0_fp
  REAL(fp), PUBLIC, PARAMETER :: MAX_OPTRAN_SECANT  = 2.36620_fp   ! = 1/cosd(65.0)
  ! The maximum angle used to train ODAS transmittance 
  REAL(fp), PUBLIC, PARAMETER :: MAX_TRANS_ZENITH_ANGLE = 63.6122_fp !corresponding to amass 2.25

  ! -------------------------------------------------
  ! Limits on source angles.
  ! - The maximum source zenith angle should
  !   be determined by the maximum angle secant
  !   used in generating the gas absorption model
  !   coefficients, i.e. a secant of 2.25 => 63.6deg.
  !   Users have requested the Value be 85deg which
  !   has a secant of ~11.47.
  ! - Azimuth angles that are multiples
  !   of 2pi are not accepted.
  ! -------------------------------------------------
  REAL(fp), PUBLIC, PARAMETER :: MAX_SOURCE_ZENITH_ANGLE  = 85.0_fp
  REAL(fp), PUBLIC, PARAMETER :: MAX_SECANT_SOURCE_ZENITH = 11.473711738554476_fp
  REAL(fp), PUBLIC, PARAMETER :: MAX_SOURCE_AZIMUTH_ANGLE = 360.0_fp


  ! ----------------------------------------
  ! Default diffusivity angle and secant
  ! ACOS( 3/5 ) in degrees is (~53.13)
  ! Used to approximate the downwelling flux
  ! ----------------------------------------
  REAL(fp), PUBLIC, PARAMETER :: DIFFUSIVITY_ANGLE  = 53.130102354156_fp
  REAL(fp), PUBLIC, PARAMETER :: DIFFUSIVITY_RADIAN = 0.927295218002_fp
  REAL(fp), PUBLIC, PARAMETER :: SECANT_DIFFUSIVITY = FIVE / THREE


  ! -----------------------------------------------------------
  ! Maximum flux angle definitions. Determined by the maximum
  ! angle secant used in generating the gas absorption model
  ! coefficients, i.e. a secant of 2.25 => 63.6deg. If the user
  ! inputs a value larger than this for the Flux_Zenith_Angle,
  ! the diffusivity angles are used instead
  ! -----------------------------------------------------------
  REAL(fp), PUBLIC, PARAMETER :: MAX_FLUX_ZENITH_ANGLE  = 63.612200038757_fp
  REAL(fp), PUBLIC, PARAMETER :: MAX_SECANT_FLUX_ZENITH = 2.25_fp


  !#----------------------------------------------------------------------------#
  !#            -- CloudScatter, RTSolution, AtmOptics PARAMETERS --            #
  !#----------------------------------------------------------------------------#
  INTEGER, PUBLIC, PARAMETER :: RT_ADA = 56
  INTEGER, PUBLIC, PARAMETER :: RT_SOI = 168
  
  INTEGER, PUBLIC, PARAMETER :: MAX_N_CLOUDS   = 4 ! Max. number of clouds/profile. Needed for CSV...
  INTEGER, PUBLIC, PARAMETER :: MAX_N_AEROSOLS = 20 ! Max. number of aerosols/profile. Needed for ASV
  
  REAL(fp), PUBLIC, PARAMETER :: AEROSOL_CONTENT_THRESHOLD = 0.000000001_fp
  REAL(fp), PUBLIC, PARAMETER :: WATER_CONTENT_THRESHOLD = 0.000001_fp
  REAL(fp), PUBLIC, PARAMETER :: OPTICAL_DEPTH_THRESHOLD = 0.000001_fp

  REAL(fp), PUBLIC, PARAMETER :: BS_THRESHOLD  = 1.0e-10_fp  ! Was SCATTERING_ALBEDO_THRESHOLD
  REAL(fp), PUBLIC, PARAMETER :: SCATTERING_ALBEDO_THRESHOLD = BS_THRESHOLD  ! Eventually replace this with BS_THRESHOLD


  INTEGER, PUBLIC, PARAMETER :: MAX_N_LEGENDRE_TERMS = 16
  INTEGER, PUBLIC, PARAMETER :: MAX_N_PHASE_ELEMENTS = 1
  INTEGER, PUBLIC, PARAMETER :: MAX_N_STREAMS = 16 
  INTEGER, PUBLIC, PARAMETER :: MAX_N_ANGLES = 16
  INTEGER, PUBLIC, PARAMETER :: MAX_N_STOKES = 4
  INTEGER, PUBLIC, PARAMETER :: MAX_N_AZIMUTH_FOURIER = 16   ! maximum number of Fourier components for azimuth angles
    
!### SOI uses HG phase function (modified by Tahara, Feb 2008)
  LOGICAL, PUBLIC, PARAMETER :: HGPHASE = .FALSE.
! LOGICAL, PUBLIC, PARAMETER :: HGPHASE = .TRUE.
!### SOI uses HG phase function (modified by Tahara, Feb 2008)
                                                                                                        

  !#----------------------------------------------------------------------------#
  !#            -- Aircraft/profile pressure difference threshold --            #
  !#----------------------------------------------------------------------------#
  REAL(fp), PUBLIC, PARAMETER :: AIRCRAFT_PRESSURE_THRESHOLD = 0.1_fp
  
  
CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Set_Max_nChannels
! 
! PURPOSE:
!       Subroutine to set the protected variable MAX_N_CHANNELS value in the
!       CRTM_Parameters module. This should *only* be done during the CRTM
!       initialisation.
!
! CALLING SEQUENCE:
!       CALL CRTM_Set_Max_nChannels( Value )
!
! INPUT ARGUMENTS:
!       Value:        The number of channels.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! SIDE EFFECTS:
!       This subroutines changes the value of the MAX_N_CHANNELS pseudo-parameter
!       in the CRTM_Parameters module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Set_Max_nChannels( Value )
    INTEGER, INTENT(IN) :: Value
    MAX_N_CHANNELS = Value
  END SUBROUTINE CRTM_Set_Max_nChannels


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Reset_Max_nChannels
! 
! PURPOSE:
!       Subroutine to reset the protected variable MAX_N_CHANNELS value in the
!       CRTM_Parameters module to an invalid value. This should *only* be done
!       during the CRTM destruction.
!
! CALLING SEQUENCE:
!       CALL CRTM_Reset_Max_nChannels()
!
! SIDE EFFECTS:
!       This subroutines changes the value of the MAX_N_CHANNELS pseudo-parameter
!       in the CRTM_Parameters module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Reset_Max_nChannels()
    MAX_N_CHANNELS = RESET_VALUE
  END SUBROUTINE CRTM_Reset_Max_nChannels


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Get_Max_nChannels
! 
! PURPOSE:
!       Function to GET the protected variable MAX_N_CHANNELS value stored
!       in the CRTM_Parameters module.
!
! CALLING SEQUENCE:
!       Value = CRTM_Get_Max_nChannels()
!
! FUNCTION RESULT:
!       Value:  The number of channels.
!               UNITS:      N/A
!               TYPE:       INTEGER
!               DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Get_Max_nChannels() RESULT(Value)
    INTEGER :: Value
    Value = MAX_N_CHANNELS
  END FUNCTION CRTM_Get_Max_nChannels


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_IsSet_Max_nChannels
! 
! PURPOSE:
!       Function to determie if the MAX_N_CHANNELS protected variable in the
!       CRTM_Parameters module has been set to a valid value
!
! CALLING SEQUENCE:
!       Is_Set = CRTM_IsSet_Max_nChannels()
!
! FUNCTION RESULT:
!       Is_Set:  Logical flag for determining whether or not the
!                number of channels has been set.
!                If == .TRUE.  the MAX_N_CHANNELS protected variable is
!                              set to a valid value.
!                   == .FALSE. the MAX_N_CHANNELS protected variable
!                              value is invalid
!                UNITS:      N/A
!                TYPE:       LOGICAL
!                DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Aug-2001
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_IsSet_Max_nChannels() RESULT(Is_Set)
    LOGICAL :: Is_Set
    Is_Set = (MAX_N_CHANNELS /= RESET_VALUE)
  END FUNCTION CRTM_IsSet_Max_nChannels

END MODULE CRTM_Parameters
