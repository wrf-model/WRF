!
! CRTM_RTSolution
!
! Module containing the CRTM raditive transfer solution routines.
!
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu,    QSS at JCSDA;    Quanhua.Liu@noaa.gov 
!                       Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       08-Jun-2004

MODULE CRTM_RTSolution

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use statements
  USE Type_Kinds,                ONLY: fp
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,           ONLY: SET, ZERO, ONE, TWO, PI, &
                                       MAX_N_LAYERS, MAX_N_ANGLES, MAX_N_LEGENDRE_TERMS, &
                                       DEGREES_TO_RADIANS, &
                                       SECANT_DIFFUSIVITY, &
                                       SCATTERING_ALBEDO_THRESHOLD, &
                                       OPTICAL_DEPTH_THRESHOLD
  USE CRTM_SpcCoeff,             ONLY: SC, INFRARED_SENSOR, SOLAR_FLAG, IsFlagSet_SpcCoeff
  USE CRTM_Atmosphere_Define,    ONLY: CRTM_Atmosphere_type
  USE CRTM_Surface_Define,       ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type
  USE CRTM_Planck_Functions,     ONLY: CRTM_Planck_Radiance      , &
                                       CRTM_Planck_Radiance_TL   , &
                                       CRTM_Planck_Radiance_AD   , &
                                       CRTM_Planck_Temperature   , &
                                       CRTM_Planck_Temperature_TL, &
                                       CRTM_Planck_Temperature_AD
! **** REPLACE 
  USE CRTM_AtmScatter_Define,    ONLY: CRTM_AtmOptics_type => CRTM_AtmScatter_type 
! **** WITH THE FOLLOWING 
!  USE CRTM_AtmOptics_Define,     ONLY: CRTM_AtmOptics_type 
! ****
  USE CRTM_SfcOptics
  USE CRTM_RTSolution_Define, ONLY: CRTM_RTSolution_type      , &
                                    OPERATOR(==)              , &
                                    CRTM_RTSolution_Associated, &
                                    CRTM_RTSolution_Destroy   , &
                                    CRTM_RTSolution_Create
  USE CRTM_Utility
  USE RTV_Define
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------
  ! Everything private by default
  PRIVATE
  ! RTSolution structure entities
  ! ...Datatypes
  PUBLIC :: CRTM_RTSolution_type
  ! ...Operators
  PUBLIC :: OPERATOR(==)
  ! ...Procedures
  PUBLIC :: CRTM_RTSolution_Associated
  PUBLIC :: CRTM_RTSolution_Destroy
  PUBLIC :: CRTM_RTSolution_Create
  ! RTV structure entities
  ! ...Datatypes
  PUBLIC :: RTV_type
  ! Module procedures
  PUBLIC :: CRTM_Compute_RTSolution
  PUBLIC :: CRTM_Compute_RTSolution_TL
  PUBLIC :: CRTM_Compute_RTSolution_AD
  PUBLIC :: CRTM_Compute_nStreams
  PUBLIC :: CRTM_RTSolution_Version

  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*),  PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_RTSolution.f90 8130 2010-05-28 18:59:06Z paul.vandelst@noaa.gov $'


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
!       CRTM_Compute_RTSolution
!
! PURPOSE:
!       Function to solve the radiative transfer equation.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_RTSolution( Atmosphere  , &  ! Input
!                                               Surface     , &  ! Input
!                                               AtmOptics   , &  ! Input
!                                               SfcOptics   , &  ! Input
!                                               GeometryInfo, &  ! Input
!                                               SensorIndex , &  ! Input
!                                               ChannelIndex, &  ! Input
!                                               RTSolution  , &  ! Output
!                                               RTV           )  ! Internal variable output
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the atmospheric state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface:        Structure containing the surface state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       AtmOptics:      Structure containing the combined atmospheric
!                       optical properties for gaseous absorption, clouds,
!                       and aerosols.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:      Structure containing the surface optical properties
!                       data. Argument is defined as INTENT (IN OUT ) as
!                       different RT algorithms may compute the surface
!                       optics properties before this routine is called.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SfcOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       GeometryInfo:   Structure containing the view geometry data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_GeometryInfo_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:    Sensor index id. This is a unique index associated
!                       with a (supported) sensor used to access the
!                       shared coefficient data for a particular sensor.
!                       See the ChannelIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:   Channel index id. This is a unique index associated
!                       with a (supported) sensor channel used to access the
!                       shared coefficient data for a particular sensor's
!                       channel.
!                       See the SensorIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       RTSolution:     Structure containing the soluition to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       RTV:            Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       RTV_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the computation was sucessful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output RTSolution argument is IN OUT rather than
!       just OUT. This is necessary because the argument is defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_RTSolution( &
    Atmosphere  , &  ! Input
    Surface     , &  ! Input
    AtmOptics   , &  ! Input
    SfcOptics   , &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    RTSolution  , &  ! Output
    RTV         ) &  ! Internal variable output
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_AtmOptics_type),    INTENT(IN)     :: AtmOptics 
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_RTSolution_type),   INTENT(IN OUT) :: RTSolution
    TYPE(RTV_type),               INTENT(IN OUT) :: RTV
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_RTSolution'
    ! Local variables
    CHARACTER(256) :: Message 
    INTEGER :: i, k, nZ
    INTEGER :: no, na, nt
    REAL(fp) :: u       ! COS( sensor zenith angle )
    REAL(fp) :: Factor  ! SfcOptics quadrature weights normalisation factor
    REAL(fp) :: User_Emissivity, Direct_Reflectivity
    REAL(fp) :: Cosmic_Background_Radiance
    REAL(fp) :: Radiance
    INTEGER  :: Sensor_Type
    LOGICAL  :: Is_Solar_Channel

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Short named local copies
    u = ONE / GeometryInfo%Secant_Sensor_Zenith
    ! Populate the RTV structure
    RTV%n_Added_Layers = Atmosphere%n_Added_Layers
    RTV%n_Layers       = Atmosphere%n_Layers
    RTV%n_Streams      = RTSolution%n_Full_Streams/2
    ! Save the optical depth if required
    IF ( CRTM_RTSolution_Associated( RTSolution ) ) THEN
      ! Shorter names for indexing
      no = RTSolution%n_Layers  ! Original no. of layers
      na = RTV%n_Added_Layers   ! No. of added layers
      nt = RTV%n_Layers         ! Current total no. of layers
      ! Assign only the optical depth profile
      ! defined by the user input layering
      RTSolution%Layer_Optical_Depth(1:no) = AtmOptics%Optical_Depth(na+1:nt)
    END IF
    ! Required SpcCoeff components
    Cosmic_Background_Radiance = SC(SensorIndex)%Cosmic_Background_Radiance(ChannelIndex)
    Sensor_Type                = SC(SensorIndex)%Sensor_Type
    Is_Solar_Channel           = IsFlagSet_SpcCoeff(SC(SensorIndex)%Channel_Flag(ChannelIndex),SOLAR_FLAG)
    RTV%COS_SUN                = cos(GeometryInfo%Source_Zenith_Radian)
    RTV%Solar_Irradiance       = SC(SensorIndex)%Solar_Irradiance(ChannelIndex) * GeometryInfo%AU_ratio2
   
    ! Determine the surface emission behavior
    !   By default, surface is SPECULAR.
    RTV%Diffuse_Surface = .FALSE.


    ! -------------------------------------------
    ! Determine the quadrature angles and weights
    ! -------------------------------------------
    ! Default is to assume scattering RT
    RTV%Scattering_RT = .FALSE.

    ! Check for scattering conditions
    Determine_Stream_Angles: IF( RTSolution%Scattering_FLAG .AND. &
                                 MAXVAL(AtmOptics%Single_Scatter_Albedo) >= SCATTERING_ALBEDO_THRESHOLD ) THEN 

      ! --------------------------
      ! Set the scattering RT flag
      ! --------------------------
      RTV%Scattering_RT = .TRUE.


      ! ---------------------------------------
      ! Compute the Gaussian angles and weights
      ! ---------------------------------------
      CALL Double_Gauss_Quadrature( RTV%n_Streams, RTV%COS_Angle, RTV%COS_Weight )


      ! ----------------------------------------------------------------
      ! Is an additional stream required for the satellite zenith angle?
      ! That is, is the satellite zenith angle sufficiently different
      ! from the stream angles?
      ! ----------------------------------------------------------------
      ! Search for a matching angle
      DO i = 1, RTV%n_Streams
        IF( ABS( RTV%COS_Angle(i) - u ) < ANGLE_THRESHOLD ) THEN
          SfcOptics%Index_Sat_Ang = i         ! Identify the sensor zenith angle
          RTV%n_Angles = RTV%n_Streams
          ! *****FLAW*****
          ! CAN WE REPLACE THIS CONSTRUCT WITH ANOTHER
          GO TO 100
          ! *****FLAW*****
        END IF
      END DO

      ! No match found, so flag an additional
      ! spot for the satellite zenith angle
      RTV%n_Angles = RTV%n_Streams + 1
      SfcOptics%Index_Sat_Ang = RTV%n_Angles      ! Identify the sensor zenith angle
      RTV%COS_Angle( RTV%n_Angles )  = u
      RTV%COS_Weight( RTV%n_Angles ) = ZERO

      100 CONTINUE


      ! ------------------------
      ! Compute the phase matrix
      ! ------------------------
      CALL CRTM_Phase_Matrix( AtmOptics, & ! Input
                              RTV        ) ! Internal variable


    ELSE IF( RTV%Diffuse_Surface) THEN

      ! ------------------
      ! Lambertian surface
      ! ------------------
      ! The default diffusivity angle
      RTV%n_Streams       = 1
      RTV%COS_Angle( 1 )  = ONE/SECANT_DIFFUSIVITY
      RTV%COS_Weight( 1 ) = ONE
      ! The satellite zenith angle
      RTV%n_Angles                   = RTV%n_Streams + 1
      SfcOptics%Index_Sat_Ang        = RTV%n_Angles      ! Identify the sensor zenith angle
      RTV%COS_Angle( RTV%n_Angles )  = u
      RTV%COS_Weight( RTV%n_Angles ) = ZERO


    ELSE

      ! ----------------
      ! Specular surface
      ! ----------------
      RTV%n_Streams = 0
      RTV%n_Angles  = 1
      RTV%COS_Angle( RTV%n_Angles )  = u
      RTV%COS_Weight( RTV%n_Angles ) = ONE
      SfcOptics%Index_Sat_Ang        = RTV%n_Angles

    END IF Determine_Stream_Angles


    ! -------------------------------------------------
    ! Copy the RTV angles to a short name for use below
    ! -------------------------------------------------
    nZ = RTV%n_Angles


    ! ---------------------------------------------------------------------
    ! Assign the number of Stokes parameters. Currently, this is set to 1
    ! for decoupled polarization between surface and atmosphere.
    ! Remember for polarised microwave instruments, each frequency's Stokes
    ! parameter is treated as a separate channel.
    ! ---------------------------------------------------------------------
    SfcOptics%n_Stokes = 1

    
    ! ----------------------------------------------------
    ! Assign the angles and weights to SfcOptics structure
    ! ----------------------------------------------------
    SfcOptics%n_Angles = RTV%n_Angles
    Factor = ZERO
    DO i = 1, nZ
      SfcOptics%Angle(i)  = ACOS( RTV%COS_Angle(i) ) / DEGREES_TO_RADIANS
      SfcOptics%Weight(i) = RTV%COS_Angle(i) * RTV%COS_Weight(i)
      ! Compute the weight normalisation factor
      Factor = Factor + SfcOptics%Weight(i)
    END DO
    ! Normalise the quadrature weights                                               
    IF( Factor > ZERO) SfcOptics%Weight(1:nZ) = SfcOptics%Weight(1:nZ) / Factor


    ! --------------------------------
    ! Populate the SfcOptics structure
    ! --------------------------------
    IF ( SfcOptics%Compute_Switch == SET ) THEN
      Error_Status = CRTM_Compute_SfcOptics( &
                       Surface     , & ! Input
                       GeometryInfo, & ! Input
                       SensorIndex , & ! Input
                       ChannelIndex, & ! Input
                       SfcOptics   , & ! In/Output
                       RTV%SOV       ) ! Internal variable output
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error computing SfcOptics for ",a," channel ",i0)' ) &
               TRIM(SC(SensorIndex)%Sensor_Id), &
               SC(SensorIndex)%Sensor_Channel(ChannelIndex)
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF

    ELSE

      IF( RTV%Scattering_RT ) THEN
        ! Replicate the user emissivity for all angles
        SfcOptics%Reflectivity = ZERO
        User_Emissivity = SfcOptics%Emissivity(1,1)
        SfcOptics%Emissivity(1,1) = ZERO
        Direct_Reflectivity = SfcOptics%Direct_Reflectivity(1,1)/PI
        SfcOptics%Emissivity(1:nZ,1) = User_Emissivity
        ! Replicate the user reflectivities for all angles
        SfcOptics%Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity
        IF( RTV%Diffuse_Surface) THEN
          DO i = 1, nZ 
            SfcOptics%Reflectivity(1:nZ, 1, i, 1) = (ONE-SfcOptics%Emissivity(i,1))*SfcOptics%Weight(i)   
          END DO
        ELSE ! Specular surface
          DO i = 1, nZ 
            SfcOptics%Reflectivity(i, 1, i, 1) = (ONE-SfcOptics%Emissivity(i,1))
          END DO
        END IF
      ELSE
        User_Emissivity = SfcOptics%Emissivity(1,1)
        SfcOptics%Emissivity( SfcOptics%Index_Sat_Ang,1 ) = User_Emissivity
        SfcOptics%Reflectivity(1,1,1,1) = ONE - User_Emissivity
      END IF

    END IF


    ! ------------------------------------------------------
    ! Save the emissivity in the RTSolution output structure
    ! ------------------------------------------------------
    RTSolution%Surface_Emissivity = SfcOptics%Emissivity( SfcOptics%Index_Sat_Ang, 1 )


    ! ------------------------
    ! Compute Planck radiances
    ! ------------------------
    IF( RTV%mth_Azi == 0 ) THEN
      DO k = 1, Atmosphere%n_Layers 
        CALL CRTM_Planck_Radiance( &
               SensorIndex              , & ! Input
               ChannelIndex             , & ! Input
               Atmosphere%Temperature(k), & ! Input
               RTV%Planck_Atmosphere(k)   ) ! Output
      END DO
      ! Surface radiance
      CALL CRTM_Planck_Radiance( &
             SensorIndex                  , & ! Input
             ChannelIndex                 , & ! Input
             SfcOptics%Surface_Temperature, & ! Input
             RTV%Planck_Surface             ) ! Output
    ELSE
      RTV%Planck_Atmosphere = ZERO
      RTV%Planck_Surface = ZERO
    END IF


    ! ------------------------------
    ! Perform the radiative transfer
    ! ------------------------------
    ! Select the RT model
    IF( RTV%Scattering_RT ) THEN

      ! -----------------------------------------------------
      ! Scattering RT. NESDIS advanced adding-doubling method 
      ! -----------------------------------------------------
      CALL CRTM_ADA( &
             Atmosphere%n_Layers                       , & ! Input, number of atmospheric layers
             AtmOptics%Single_Scatter_Albedo           , & ! Input, layer single scattering albedo
             AtmOptics%Asymmetry_Factor                , & ! Input, layer asymmetry factor
             AtmOptics%Optical_Depth                   , & ! Input, layer optical depth
             Cosmic_Background_Radiance                , & ! Input, cosmic background radiation
             SfcOptics%Emissivity( 1:nZ, 1 )           , & ! Input, surface emissivity
             SfcOptics%Reflectivity( 1:nZ, 1, 1:nZ, 1 ), & ! Input, surface reflectivity
             SfcOptics%Direct_Reflectivity(1:nZ,1)     , & ! Input, surface reflectivity for a point source
             RTV                                         ) ! Output, Internal variables
      ! The output radiance
      Radiance = RTV%s_Level_Rad_UP( SfcOptics%Index_Sat_Ang, 0 )
 
    ELSE


      ! -----------------
      ! Emission model RT
      ! -----------------
      CALL CRTM_Emission( &
             Atmosphere%n_Layers,                   & ! Input, number of atmospheric layers
             RTV%n_Angles,                          & ! Input, number of discrete zenith angles
             RTV%Diffuse_Surface,                   & ! Input, surface behavior
             u,                                     & ! Input, cosine of sensor zenith angle
             AtmOptics%Optical_Depth,               & ! Input, layer optical depth
             RTV%Planck_Atmosphere,                 & ! Input, layer radiances
             RTV%Planck_Surface,                    & ! Input, surface radiance
             SfcOptics%Emissivity(1:nZ,1),          & ! Input, surface emissivity
             SfcOptics%Reflectivity(1:nZ,1,1:nZ,1), & ! Input, surface reflectivity                
             SfcOptics%Direct_Reflectivity(1:nZ,1), & ! Input, surface reflectivity for a point source
             Cosmic_Background_Radiance,            & ! Input, cosmic background radiation 
             RTV%Solar_Irradiance,                  & ! Input, Source irradiance at TOA
             Is_Solar_Channel,                      & ! Input, Source sensitive channel info.
             GeometryInfo%Source_Zenith_Radian,     & ! Input, Source zenith angle
             RTV                                    ) ! Output, Internal variables
      ! The output radiance
      Radiance = RTV%e_Level_Rad_UP(0)

      ! Other emission-only output
      RTSolution%Up_Radiance             = RTV%Up_Radiance
      RTSolution%Down_Radiance           = RTV%e_Level_Rad_DOWN(Atmosphere%n_Layers)
      RTSolution%Down_Solar_Radiance     = RTV%Down_Solar_Radiance
      RTSolution%Surface_Planck_Radiance = RTV%Planck_Surface
      IF ( CRTM_RTSolution_Associated( RTSolution ) ) THEN
        ! Shorter names for indexing
        no = RTSolution%n_Layers  ! Original no. of layers
        na = RTV%n_Added_Layers   ! No. of added layers
        nt = RTV%n_Layers         ! Current total no. of layers
        ! Assign only the upwelling radiance profile
        ! defined by the user input layering
        RTSolution%Upwelling_Radiance(1:no) = RTV%e_Level_Rad_UP(na+1:nt)
      END IF
    END IF
 
    ! accumulate Fourier component
    RTSolution%Radiance = RTSolution%Radiance + Radiance*  &
       cos( RTV%mth_Azi*(GeometryInfo%Sensor_Azimuth_Radian-GeometryInfo%Source_Azimuth_Radian) )

    ! ------------------------------------------------
    ! Compute the corresponding brightness temperature
    ! ------------------------------------------------
    IF( RTV%mth_Azi == 0 ) THEN
    CALL CRTM_Planck_Temperature( &
           SensorIndex,                      & ! Input
           ChannelIndex,                     & ! Input
           RTSolution%Radiance,              & ! Input
           RTSolution%Brightness_Temperature ) ! Output
    END IF

  END FUNCTION CRTM_Compute_RTSolution



!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_RTSolution_TL
!
! PURPOSE:
!       Function to solve the tangent-linear radiative transfer equation.
!
! CALLING SEQUENCE:
!      Error_Status = CRTM_Compute_RTSolution_TL( Atmosphere   , &  ! FWD Input
!                                                 Surface      , &  ! FWD Input
!                                                 AtmOptics    , &  ! FWD Input
!                                                 SfcOptics    , &  ! FWD Input
!                                                 RTSolution   , &  ! FWD Input
!                                                 Atmosphere_TL, &  ! TL Input
!                                                 Surface_TL   , &  ! TL Input
!                                                 AtmOptics_TL , &  ! TL Input
!                                                 SfcOptics_TL , &  ! TL Input 
!                                                 GeometryInfo , &  ! Input
!                                                 SensorIndex  , &  ! Input
!                                                 ChannelIndex , &  ! Input
!                                                 RTSolution_TL, &  ! TL Output
!                                                 RTV            )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the atmospheric state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface:        Structure containing the surface state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       AtmOptics:      Structure containing the combined atmospheric
!                       optical properties for gaseous absorption, clouds,
!                       and aerosols.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:      Structure containing the surface optical properties
!                       data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SfcOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:  Structure containing the tangent-linear atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface_TL:     Structure containing the tangent-linear surface state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       AtmOptics_TL:   Structure containing the tangent-linear atmospheric  
!                       optical properties.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       SfcOptics_TL:   Structure containing the tangent-linear surface optical
!                       properties. Argument is defined as INTENT (IN OUT ) as
!                       different RT algorithms may compute the surface optics
!                       properties before this routine is called.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SfcOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT)
!
!       GeometryInfo:   Structure containing the view geometry data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_GeometryInfo_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:    Sensor index id. This is a unique index associated
!                       with a (supported) sensor used to access the
!                       shared coefficient data for a particular sensor.
!                       See the ChannelIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:   Channel index id. This is a unique index associated
!                       with a (supported) sensor channel used to access the
!                       shared coefficient data for a particular sensor's
!                       channel.
!                       See the SensorIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       RTV:            Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       RTV_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       RTSolution_TL:  Structure containing the solution to the tangent-linear
!                       RT equation for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the computation was sucessful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output RTSolution_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_RTSolution_TL( &
    Atmosphere   , &  ! FWD Input
    Surface      , &  ! FWD Input
    AtmOptics    , &  ! FWD Input
    SfcOptics    , &  ! FWD Input
    RTSolution   , &  ! FWD Input
    Atmosphere_TL, &  ! TL Input
    Surface_TL   , &  ! TL Input
    AtmOptics_TL , &  ! TL Input
    SfcOptics_TL , &  ! TL Input 
    GeometryInfo , &  ! Input
    SensorIndex  , &  ! Input
    ChannelIndex , &  ! Input
    RTSolution_TL, &  ! TL Output
    RTV          ) &  ! Internal variable input
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_AtmOptics_type),    INTENT(IN)     :: AtmOptics 
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_RTSolution_type),   INTENT(IN)     :: RTSolution
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere_TL
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface_TL 
    TYPE(CRTM_AtmOptics_type),    INTENT(IN)     :: AtmOptics_TL
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_TL
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_RTSolution_type),   INTENT(IN OUT) :: RTSolution_TL
    TYPE(RTV_type),               INTENT(IN)     :: RTV
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_RTSolution_TL'
    ! Local variables
    CHARACTER(256) :: Message 
    INTEGER :: i, k, nZ
    INTEGER :: no, na, nt
    REAL(fp) :: u       ! COS( sensor zenith angle )
    REAL(fp) :: Cosmic_Background_Radiance
    LOGICAL  :: Is_Solar_Channel
    REAL(fp) :: User_Emissivity_TL, Direct_Reflectivity_TL
    REAL(fp)                                     :: Planck_Surface_TL    ! Surface TL radiance
    REAL(fp), DIMENSION( 0:Atmosphere%n_Layers ) :: Planck_Atmosphere_TL ! *LAYER* TL radiances

    ! The following variables are RT model specific
    REAL(fp), DIMENSION( MAX_N_ANGLES, &
                         MAX_N_ANGLES+1, &
                         Atmosphere%n_Layers ) :: Pff_TL ! Forward scattering TL phase matrix
    REAL(fp), DIMENSION( MAX_N_ANGLES, &
                         MAX_N_ANGLES+1, &
                         Atmosphere%n_Layers ) :: Pbb_TL ! Backward scattering TL phase matrix
    REAL(fp), DIMENSION( MAX_N_ANGLES ) :: Scattering_Radiance_TL
    REAL(fp) :: Radiance_TL

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Short named local varibale copies
    u  = ONE / GeometryInfo%Secant_Sensor_Zenith
    nz = RTV%n_Angles
    ! Set the sensor zenith angle index
    SfcOptics_TL%Index_Sat_Ang = SfcOptics%Index_Sat_Ang
    ! Save the TL optical depth if required
    IF ( CRTM_RTSolution_Associated( RTSolution_TL ) ) THEN
      ! Shorter names for indexing
      no = RTSolution_TL%n_Layers  ! Original no. of layers
      na = RTV%n_Added_Layers      ! No. of added layers
      nt = RTV%n_Layers            ! Current total no. of layers
      ! Assign only the optical depth profile
      ! defined by the user input layering
      RTSolution_TL%Layer_Optical_Depth(1:no) = AtmOptics_TL%Optical_Depth(na+1:nt)
    END IF
    ! Required SpcCoeff components
    Cosmic_Background_Radiance = SC(SensorIndex)%Cosmic_Background_Radiance(ChannelIndex)
    Is_Solar_Channel           = IsFlagSet_SpcCoeff(SC(SensorIndex)%Channel_Flag(ChannelIndex),SOLAR_FLAG)


    ! ---------------------------------------------------
    ! Compute the tangent-linear phase matrix if required
    ! ---------------------------------------------------
    IF ( RTV%Scattering_RT ) THEN
      CALL CRTM_Phase_Matrix_TL( &
             AtmOptics,    & ! Input
             AtmOptics_TL, & ! Input
             Pff_TL,       & ! Output - TL forward scattering
             Pbb_TL,       & ! Output - TL backward scattering
             RTV           ) ! Internal variable
    END IF


    ! ---------------------------------------------------------------------
    ! Assign the number of Stokes parameters. Currently, this is set to 1
    ! for decoupled polarization between surface and atmosphere.
    ! Remember for polarised microwave instruments, each frequency's Stokes
    ! parameter is treated as a separate channel.
    ! ---------------------------------------------------------------------
    SfcOptics_TL%n_Stokes = 1

    
    ! ----------------------------------------------------
    ! Assign the angles and weights to SfcOptics structure
    ! ----------------------------------------------------
    SfcOptics_TL%n_Angles = SfcOptics%n_Angles
    SfcOptics_TL%Angle    = SfcOptics%Angle 
    SfcOptics_TL%Weight   = SfcOptics%Weight


    ! -----------------------------------------
    ! Populate the SfcOptics_TL structure based
    ! on FORWARD model SfcOptics Compute_Switch
    ! -----------------------------------------
    IF ( SfcOptics%Compute_Switch == SET ) THEN
      Error_Status = CRTM_Compute_SfcOptics_TL( &
                       Surface     , & ! Input
                       SfcOptics   , & ! Input
                       Surface_TL  , & ! Input
                       GeometryInfo, & ! Input
                       SensorIndex , & ! Input
                       ChannelIndex, & ! Input
                       SfcOptics_TL, & ! In/Output
                       RTV%SOV       ) ! Internal variable input
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error computing SfcOptics_TL for ",a," channel ",i0)' ) &
               TRIM(SC(SensorIndex)%Sensor_Id), &
               SC(SensorIndex)%Sensor_Channel(ChannelIndex)
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF
    ELSE
      IF( RTV%Scattering_RT ) THEN
        ! Replicate the user emissivity for all angles
        SfcOptics_TL%Reflectivity       = ZERO
        User_Emissivity_TL              = SfcOptics_TL%Emissivity(1,1)
        SfcOptics_TL%Emissivity(1,1)    = ZERO
        Direct_Reflectivity_TL          = SfcOptics_TL%Direct_Reflectivity(1,1)/PI
        SfcOptics_TL%Emissivity(1:nZ,1) = User_Emissivity_TL
        ! Replicate the user reflectivities for all angles
        SfcOptics_TL%Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity_TL
        IF( RTV%Diffuse_Surface) THEN
          DO i = 1, nZ 
            SfcOptics_TL%Reflectivity(1:nZ, 1, i, 1) = -SfcOptics_TL%Emissivity(i,1)*SfcOptics%Weight(i)   
          END DO
        ELSE ! Specular surface
          DO i = 1, nZ 
            SfcOptics_TL%Reflectivity(i, 1, i, 1) = -SfcOptics_TL%Emissivity(i,1)
          END DO
        END IF
      ELSE
        User_Emissivity_TL = SfcOptics_TL%Emissivity(1,1)
        SfcOptics_TL%Emissivity( SfcOptics%Index_Sat_Ang,1 ) = User_Emissivity_TL
        SfcOptics_TL%Reflectivity(1,1,1,1) = - User_Emissivity_TL
      END IF
    END IF


    ! ------------------------------------------------------------
    ! Save the TL emissivity in the RTSolution_TL output structure
    ! ------------------------------------------------------------
    RTSolution_TL%Surface_Emissivity = SfcOptics_TL%Emissivity( SfcOptics_TL%Index_Sat_Ang, 1 )


    ! -------------------------------------------
    ! Compute the tangent-linear planck radiances
    ! -------------------------------------------
    IF( RTV%mth_Azi == 0 ) THEN
      ! Atmospheric layer TL radiances
      DO k = 1, Atmosphere%n_Layers 
        CALL CRTM_Planck_Radiance_TL( &
               SensorIndex                 , & ! Input
               ChannelIndex                , & ! Input
               Atmosphere%Temperature(k)   , & ! Input
               Atmosphere_TL%Temperature(k), & ! Input
               Planck_Atmosphere_TL(k)       ) ! Output
      END DO
      ! Surface TL radiance
      CALL CRTM_Planck_Radiance_TL( &
             SensorIndex                     , & ! Input
             ChannelIndex                    , & ! Input
             SfcOptics%Surface_Temperature   , & ! Input
             SfcOptics_TL%Surface_Temperature, & ! Input
             Planck_Surface_TL                 ) ! Output
    ELSE
      Planck_Atmosphere_TL = ZERO
      Planck_Surface_TL = ZERO
    END IF


    ! ---------------------------------------------
    ! Perform the tangent-linear radiative transfer
    ! ---------------------------------------------
    ! Select the RT model
    IF( RTV%Scattering_RT ) THEN


      ! -----------------------------------------------------
      ! Scattering RT. NESDIS advanced adding-doubling method 
      ! -----------------------------------------------------
      CALL CRTM_ADA_TL( &
             Atmosphere%n_Layers,                      & ! Input, number of atmospheric layers
             AtmOptics%Single_Scatter_Albedo,          & ! Input, FWD layer single scattering albedo
             AtmOptics%Asymmetry_Factor,               & ! Input, FWD layer asymmetry factor
             AtmOptics%Optical_Depth,                  & ! Input, FWD layer optical depth
             Cosmic_Background_Radiance,               & ! cosmic background radiation
             SfcOptics%Emissivity(1:nZ,1),             & ! Input, FWD surface emissivity
             SfcOptics%Direct_Reflectivity(1:nZ,1),    & ! Input, surface direct reflectivity
             RTV,                                      & ! Input, structure containing forward results 
             Planck_Atmosphere_TL,                     & ! Input, TL layer radiances
             Planck_Surface_TL,                        & ! Input, TL surface radiance
             AtmOptics_TL%Single_Scatter_Albedo,       & ! Input, TL layer single scattering albedo
             AtmOptics_TL%Asymmetry_Factor,            & ! Input, TL layer asymmetry factor
             AtmOptics_TL%Optical_Depth,               & ! Input, TL layer optical depth
             SfcOptics_TL%Emissivity(1:nZ,1),          & ! Input, TL surface emissivity
             SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1), & ! Input, TL surface reflectivity
             SfcOptics_TL%Direct_Reflectivity(1:nZ,1), & ! Input, TL surface direct reflectivity
             Pff_TL(1:nZ,1:(nZ+1),:),                  & ! Input, TL layer forward phase matrix
             Pbb_TL(1:nZ,1:(nZ+1),:),                  & ! Input, TL layer backward phase matrix
             Scattering_Radiance_TL(1:nZ)              ) ! Output, TL radiances
      ! The output TL radiance for the sensor zenith angle
      Radiance_TL = Scattering_Radiance_TL( SfcOptics%Index_Sat_Ang )

    ELSE


      ! -----------------
      ! Emission model RT
      ! -----------------
      CALL CRTM_Emission_TL( &
             Atmosphere%n_Layers,                      & ! Input, number of atmospheric layers
             RTV%n_Angles,                             & ! Input, number of discrete zenith angles
             u,                                        & ! Input, cosine of sensor zenith angle
             AtmOptics%Optical_Depth,                  & ! Input, FWD layer optical depth
             RTV%Planck_Atmosphere,                    & ! Input, FWD layer radiances
             RTV%Planck_Surface,                       & ! Input, FWD surface radiance
             SfcOptics%Emissivity(1:nZ,1),             & ! Input, FWD surface emissivity
             SfcOptics%Reflectivity(1:nZ,1,1:nZ,1),    & ! Input, FWD surface reflectivity
             SfcOptics%Direct_Reflectivity(1:nZ,1),    & ! Input, FWD surface reflectivity for a point source
             RTV%Solar_Irradiance,                     & ! Input, Source irradiance at TOA
             Is_Solar_Channel,                         & ! Input, Source sensitive channel info.
             GeometryInfo%Source_Zenith_Radian,        & ! Input, Source zenith angle
             RTV,                                      & ! Input, internal variables
             AtmOptics_TL%Optical_Depth,               & ! Input, TL layer optical depth
             Planck_Atmosphere_TL,                     & ! Input, TL layer radiances
             Planck_Surface_TL,                        & ! Input, TL surface radiance
             SfcOptics_TL%Emissivity(1:nZ,1),          & ! Input, TL surface emissivity
             SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1), & ! Input, TL surface reflectivity
             SfcOptics_TL%Direct_Reflectivity(1:nZ,1), & ! Input, TL surface reflectivity for a point source
             Radiance_TL                               ) ! Output, TL radiances
    END IF

    ! accumulate Fourier component
    RTSolution_TL%Radiance = RTSolution_TL%Radiance + Radiance_TL*  &
       COS( RTV%mth_Azi*(GeometryInfo%Sensor_Azimuth_Radian-GeometryInfo%Source_Azimuth_Radian) )

     
    ! ---------------------------------------------------------------
    ! Compute the corresponding tangent-linear brightness temperature
    ! ---------------------------------------------------------------
    IF( RTV%mth_Azi == 0 ) &
      CALL CRTM_Planck_Temperature_TL( &
             SensorIndex                        , & ! Input
             ChannelIndex                       , & ! Input
             RTSolution%Radiance                , & ! Input
             RTSolution_TL%Radiance             , & ! Input
             RTSolution_TL%Brightness_Temperature ) ! Output
    
  END FUNCTION CRTM_Compute_RTSolution_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_RTSolution_AD
!
! PURPOSE:
!       Function to solve the adjoint radiative transfer equation.
!
! CALLING SEQUENCE:
!      Error_Status = CRTM_Compute_RTSolution_AD( Atmosphere   , &  ! FWD Input
!                                                 Surface      , &  ! FWD Input
!                                                 AtmOptics    , &  ! FWD Input
!                                                 SfcOptics    , &  ! FWD Input
!                                                 RTSolution   , &  ! FWD Input
!                                                 RTSolution_AD, &  ! AD Input
!                                                 GeometryInfo , &  ! Input
!                                                 SensorIndex  , &  ! Input
!                                                 ChannelIndex , &  ! Input
!                                                 Atmosphere_AD, &  ! AD Output
!                                                 Surface_AD   , &  ! AD Output
!                                                 AtmOptics_AD , &  ! AD Output
!                                                 SfcOptics_AD , &  ! AD Output
!                                                 RTV            )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the atmospheric state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface:        Structure containing the surface state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       AtmOptics:      Structure containing the combined atmospheric
!                       optical properties for gaseous absorption, clouds,
!                       and aerosols.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:      Structure containing the surface optical properties
!                       data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SfcOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       RTSolution_AD:  Structure containing the RT solution adjoint inputs. 
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       GeometryInfo:   Structure containing the view geometry data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_GeometryInfo_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:    Sensor index id. This is a unique index associated
!                       with a (supported) sensor used to access the
!                       shared coefficient data for a particular sensor.
!                       See the ChannelIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:   Channel index id. This is a unique index associated
!                       with a (supported) sensor channel used to access the
!                       shared coefficient data for a particular sensor's
!                       channel.
!                       See the SensorIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       RTV:            Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       RTV_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:  Structure containing the adjoint atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       Surface_AD:     Structure containing the adjoint surface state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       AtmOptics_AD:   Structure containing the adjoint combined atmospheric
!                       optical properties for gaseous absorption, clouds,
!                       and aerosols.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       SfcOptics_AD:   Structure containing the adjoint surface optical
!                       properties data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SfcOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the computation was sucessful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on all of the adjoint arguments (whether input or output)
!       is IN OUT rather than just OUT. This is necessary because the Input
!       adjoint arguments are modified, and the Output adjoint arguments must
!       be defined prior to entry to this routine. So, anytime a structure is
!       to be output, to prevent memory leaks the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_RTSolution_AD( &
    Atmosphere   , &  ! FWD Input
    Surface      , &  ! FWD Input
    AtmOptics    , &  ! FWD Input
    SfcOptics    , &  ! FWD Input
    RTSolution   , &  ! FWD Input
    RTSolution_AD, &  ! AD Input
    GeometryInfo , &  ! Input
    SensorIndex  , &  ! Input
    ChannelIndex , &  ! Input
    Atmosphere_AD, &  ! AD Output
    Surface_AD   , &  ! AD Output
    AtmOptics_AD , &  ! AD Output
    SfcOptics_AD , &  ! AD Output
    RTV          ) &  ! Internal variable input
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),   INTENT(IN)     :: Atmosphere
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_AtmOptics_type),    INTENT(IN)     :: AtmOptics
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_RTSolution_type),   INTENT(IN)     :: RTSolution
    TYPE(CRTM_RTSolution_type),   INTENT(IN OUT) :: RTSolution_AD
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Atmosphere_type),   INTENT(IN OUT) :: Atmosphere_AD
    TYPE(CRTM_Surface_type),      INTENT(IN OUT) :: Surface_AD
    TYPE(CRTM_AtmOptics_type),    INTENT(IN OUT) :: AtmOptics_AD
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_AD
    TYPE(RTV_type),               INTENT(IN)     :: RTV
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_RTSolution_AD'
    ! Local variables
    CHARACTER(256) :: Message 
    INTEGER :: i, k, nZ
    INTEGER :: no, na, nt
    REAL(fp) :: u       ! COS( sensor zenith angle )
    REAL(fp) :: Cosmic_Background_Radiance
    LOGICAL  :: Is_Solar_Channel
    REAL(fp)                                     :: Planck_Surface_AD    ! Surface AD radiance
    REAL(fp), DIMENSION( 0:Atmosphere%n_Layers ) :: Planck_Atmosphere_AD ! *LAYER* AD radiances
    REAL(fp) :: User_Emissivity_AD    ! Temporary adjoint variable for SfcOptics calcs.
    ! The following variables are RT model specific
    REAL(fp), DIMENSION( MAX_N_ANGLES, &
                         MAX_N_ANGLES+1, &
                         Atmosphere%n_Layers ) :: Pff_AD ! Forward scattering AD phase matrix
    REAL(fp), DIMENSION( MAX_N_ANGLES, &
                         MAX_N_ANGLES+1, &
                         Atmosphere%n_Layers ) :: Pbb_AD ! Backward scattering AD phase matrix
    REAL (fp),DIMENSION( MAX_N_ANGLES ) :: Scattering_Radiance_AD
    REAL (fp) :: Radiance_AD

    ! -----
    ! Setup
    ! -----
    Error_Status = SUCCESS
    ! Short named local copies
    u  = ONE / GeometryInfo%Secant_Sensor_Zenith
    nZ = RTV%n_Angles
    ! Set the sensor zenith angle index
    SfcOptics_AD%Index_Sat_Ang = SfcOptics%Index_Sat_Ang
    ! Required SpcCoeff components
    Cosmic_Background_Radiance = SC(SensorIndex)%Cosmic_Background_Radiance(ChannelIndex)
    Is_Solar_Channel           = IsFlagSet_SpcCoeff(SC(SensorIndex)%Channel_Flag(ChannelIndex),SOLAR_FLAG)
    ! Initialise local adjoint variables
    Planck_Surface_AD    = ZERO
    Planck_Atmosphere_AD = ZERO
    Radiance_AD = ZERO

    ! ------------------------------------------
    ! Compute the brightness temperature adjoint
    ! ------------------------------------------
    IF( RTV%mth_Azi == 0 ) THEN
      CALL CRTM_Planck_Temperature_AD( &
             SensorIndex                         , & ! Input
             ChannelIndex                        , & ! Input
             RTSolution%Radiance                 , & ! Input
             RTSolution_AD%Brightness_Temperature, & ! Input
             Radiance_AD                           ) ! Output
      RTSolution_AD%Brightness_Temperature = ZERO 
    END IF

    ! accumulate Fourier component
    Radiance_AD = Radiance_AD + RTSolution_AD%Radiance * &
         COS( RTV%mth_Azi*(GeometryInfo%Sensor_Azimuth_Radian-GeometryInfo%Source_Azimuth_Radian) )


    ! --------------------------------------
    ! Perform the adjoint radiative transfer
    ! --------------------------------------
    ! Select the RT model
    IF( RTV%Scattering_RT ) THEN


      ! -----------------------------------------------------
      ! Scattering RT. NESDIS advanced adding-doubling method 
      ! -----------------------------------------------------
      ! Initialise the input adjoint radiance
      Scattering_Radiance_AD = ZERO
      Scattering_Radiance_AD( SfcOptics%Index_Sat_Ang ) = Radiance_AD
!!      RTSolution_AD%Radiance = ZERO
      ! Call the RT Solver
      CALL CRTM_ADA_AD( &
             Atmosphere%n_Layers,                      & ! Input, number of atmospheric layers
             AtmOptics%Single_Scatter_Albedo,          & ! Input, FWD layer single scattering albedo
             AtmOptics%Asymmetry_Factor,               & ! Input, FWD layer asymmetry factor
             AtmOptics%Optical_Depth,                  & ! Input, FWD layer optical depth
             Cosmic_Background_Radiance,               & ! Input, cosmic background radiation
             SfcOptics%Emissivity(1:nZ,1),             & ! Input, FWD surface emissivity
             SfcOptics%Direct_Reflectivity(1:nZ,1),    & ! Input, FWD surface reflectivity for a point source
             RTV,                                      & ! In/Output, internal variables
             Scattering_Radiance_AD(1:nZ),             & ! Input, AD radiances
             Planck_Atmosphere_AD,                     & ! Output, AD layer radiances
             Planck_Surface_AD,                        & ! Output, AD surface radiance
             AtmOptics_AD%Single_Scatter_Albedo,       & ! Output, AD layer single scattering albedo
             AtmOptics_AD%Asymmetry_Factor,            & ! Output, AD layer asymmetry factor
             AtmOptics_AD%Optical_Depth,               & ! Output, AD layer optical depth
             SfcOptics_AD%Emissivity(1:nZ,1),          & ! Output, AD surface emissivity
             SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1), & ! Output, AD surface reflectivity
             SfcOptics_AD%Direct_Reflectivity(1:nZ,1), & ! Output, AD surface reflectivity for a point source
             Pff_AD(1:nZ,1:(nZ+1),:),                  & ! Output, AD layer forward phase matrix
             Pbb_AD(1:nZ,1:(nZ+1),:)                   ) ! Output, AD layer backward phase matrix

    ELSE


      ! -----------------
      ! Emission model RT
      ! -----------------
      CALL CRTM_Emission_AD( &
             Atmosphere%n_Layers,                      & ! Input, number of atmospheric layers
             RTV%n_Angles,                             & ! Input, number of discrete zenith angles
             u,                                        & ! Input, cosine of sensor zenith angle
             AtmOptics%Optical_Depth,                  & ! Input, FWD layer optical depth
             RTV%Planck_Atmosphere,                    & ! Input, FWD layer radiances
             RTV%Planck_Surface,                       & ! Input, FWD surface radiance
             SfcOptics%Emissivity(1:nZ,1),             & ! Input, FWD surface emissivity
             SfcOptics%Reflectivity(1:nZ,1,1:nZ,1),    & ! Input, FWD surface reflectivity
             SfcOptics%Direct_Reflectivity(1:nZ,1),    & ! Input, FWD surface reflectivity for a point source
             RTV%Solar_Irradiance,                     & ! Input, Source irradiance at TOA
             Is_Solar_Channel,                         & ! Input, Source sensitive channel info.
             GeometryInfo%Source_Zenith_Radian,        & ! Input, Source zenith angle
             RTV,                                      & ! Input, internal variables
             Radiance_AD,                              & ! Input, AD radiance
             AtmOptics_AD%Optical_Depth,               & ! Output, AD layer optical depth
             Planck_Atmosphere_AD,                     & ! Output, AD layer radiances
             Planck_Surface_AD,                        & ! Output, AD surface radiance
             SfcOptics_AD%Emissivity(1:nZ,1),          & ! Output, AD surface emissivity
             SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1), & ! Output, AD surface reflectivity
             SfcOptics_AD%Direct_Reflectivity(1:nZ,1)  ) ! Output, AD surface reflectivity for a point source
    END IF



    ! ------------------------------------
    ! Compute the adjoint planck radiances
    ! ------------------------------------
    IF( RTV%mth_Azi == 0 ) THEN
      ! Surface AD radiance
      CALL CRTM_Planck_Radiance_AD( &
             SensorIndex                    , & ! Input
             ChannelIndex                   , & ! Input
             SfcOptics%Surface_Temperature  , & ! Input
             Planck_Surface_AD              , & ! Input
             SfcOptics_AD%Surface_Temperature ) ! In/Output
      Planck_Surface_AD = ZERO
      ! Atmospheric layer AD radiances
      DO k = 1, Atmosphere%n_Layers
        CALL CRTM_Planck_Radiance_AD( &
               SensorIndex                , & ! Input
               ChannelIndex               , & ! Input
               Atmosphere%Temperature(k)  , & ! Input
               Planck_Atmosphere_AD(k)    , & ! Input
               Atmosphere_AD%Temperature(k) ) ! In/Output
        Planck_Atmosphere_AD(k) = ZERO
      END DO
    ELSE
      Planck_Surface_AD = ZERO
      Planck_Atmosphere_AD = ZERO
    END IF


    ! ---------------------------------------------------------------------
    ! Assign the number of Stokes parameters. Currently, this is set to 1
    ! for decoupled polarization between surface and atmosphere.
    ! Remember for polarised microwave instruments, each frequency's Stokes
    ! parameter is treated as a separate channel.
    ! ---------------------------------------------------------------------
    SfcOptics_AD%n_Stokes = 1

    
    ! ----------------------------------------------------
    ! Assign the angles and weights to SfcOptics structure
    ! ----------------------------------------------------
    SfcOptics_AD%n_Angles = SfcOptics%n_Angles
    SfcOptics_AD%Angle    = SfcOptics%Angle 
    SfcOptics_AD%Weight   = SfcOptics%Weight


    ! --------------------------------------------------------------------------------
    ! This part is designed for specific requirement for the total sensitivity
    ! of emissivity. The requirement is regardless whether having user input or not.
    ! Therefore, this part is common part with/without optional input.
    ! The outputs of CRTM_Compute_RTSolution are the partial derivative
    ! of emissivity and reflectivity. Since SfcOptics_AD is used as input only in 
    ! CRTM_Compute_SfcOptics_AD, the total sensitivity of the emissivity is taken into
    ! account for here.
    ! --------------------------------------------------------------------------------
    IF( RTV%Scattering_RT ) THEN
      User_Emissivity_AD = ZERO
      IF( RTV%Diffuse_Surface) THEN
        DO i = nZ, 1, -1
          User_Emissivity_AD = User_Emissivity_AD - &
            (SUM(SfcOptics_AD%Reflectivity(1:nZ,1,i,1))*SfcOptics%Weight(i))
        END DO
      ELSE ! Specular surface
        DO i = nZ, 1, -1
          User_Emissivity_AD = User_Emissivity_AD - SfcOptics_AD%Reflectivity(i,1,i,1)
        END DO
      END IF
!      Direct_Reflectivity_AD = SUM(SfcOptics_AD%Direct_Reflectivity(1:nZ,1))
!      SfcOptics_AD%Direct_Reflectivity(1,1) = SfcOptics_AD%Direct_Reflectivity(1,1) +
!                                              (Direct_Reflectivity_AD/PI)
      RTSolution_AD%Surface_Emissivity = User_Emissivity_AD
    ELSE
      RTSolution_AD%Surface_Emissivity = SfcOptics_AD%Emissivity(SfcOptics_AD%Index_Sat_Ang,1) - &
                                         SfcOptics_AD%Reflectivity(1,1,1,1)
    END IF


    ! -----------------------------------------
    ! Populate the SfcOptics_AD structure based
    ! on FORWARD model SfcOptics Compute_Switch
    ! -----------------------------------------
    IF ( SfcOptics%Compute_Switch == SET ) THEN
      Error_Status = CRTM_Compute_SfcOptics_AD( &
                       Surface     , & ! Input
                       SfcOptics   , & ! Input
                       SfcOptics_AD, & ! Input
                       GeometryInfo, & ! Input
                       SensorIndex , & ! Input
                       ChannelIndex, & ! Input
                       Surface_AD  , & ! In/Output
                       RTV%SOV       ) ! Internal variable input
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error computing SfcOptics_AD for ",a," channel ",i0)' ) &
               TRIM(SC(SensorIndex)%Sensor_Id), &
               SC(SensorIndex)%Sensor_Channel(ChannelIndex)
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF
    END IF



    ! --------------------------------------------
    ! Compute the adjoint phase matrix if required
    ! --------------------------------------------
    IF ( RTV%Scattering_RT ) THEN
      CALL CRTM_Phase_Matrix_AD( &
             AtmOptics,    &  ! Input - FWD
             Pff_AD,       &  ! Input - AD forward scattering
             Pbb_AD,       &  ! Input - AD backward scattering
             AtmOptics_AD, &  ! Output - AD
             RTV           )  ! Internal variable
    END IF


    ! ------------------------------------------
    ! Save the adjoint optical depth if required
    ! ------------------------------------------
    IF ( CRTM_RTSolution_Associated( RTSolution_AD ) ) THEN
      ! Shorter names for indexing
      no = RTSolution_AD%n_Layers  ! Original no. of layers
      na = RTV%n_Added_Layers      ! No. of added layers
      nt = RTV%n_Layers            ! Current total no. of layers
      ! Assign only the optical depth profile
      ! defined by the user input layering
      RTSolution_AD%Layer_Optical_Depth(1:no) = AtmOptics_AD%Optical_Depth(na+1:nt)
    END IF

  END FUNCTION CRTM_Compute_RTSolution_AD



!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_n_Streams
!
! PURPOSE:
!       Function to compute the number of streams required for subsequent
!       radiative transfer calculations
!
! CALLING SEQUENCE:
!       nStreams = CRTM_Compute_n_Streams( Atmosphere,   &  ! Input
!                                          SensorIndex,  &  ! Input
!                                          ChannelIndex, &  ! Input
!                                          RTSolution    )  ! Output
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the atmospheric state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:    Sensor index id. This is a unique index associated
!                       with a (supported) sensor used to access the
!                       shared coefficient data for a particular sensor.
!                       See the ChannelIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:   Channel index id. This is a unique index associated
!                       with a (supported) sensor channel used to access the
!                       shared coefficient data for a particular sensor's
!                       channel.
!                       See the SensorIndex argument.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       RTSolution:     Structure containing the scattering flag to be set
!                       for the RT calcs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       nStreams:       The number of RT streams required to perform radiative
!                       transfer in a scattering atmosphere.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_nStreams( &
    Atmosphere  , &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    RTSolution  ) &  ! Output
  RESULT( nStreams )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atmosphere
    INTEGER,                    INTENT(IN)     :: SensorIndex
    INTEGER,                    INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: RTSolution
    ! Function result
    INTEGER :: nStreams
    ! Local variables
    REAL(fp) :: maxReff, Reff, MieParameter
    INTEGER :: n
    
    ! Set up
    nStreams = 0
    RTSolution%n_full_Streams = nStreams
    RTSolution%Scattering_FLAG = .FALSE.

    ! If no clouds and no aerosols, no scattering, so return
    IF ( Atmosphere%n_Clouds   == 0 .AND. &
         Atmosphere%n_Aerosols == 0       ) RETURN
    
    ! Determine the maximum cloud particle size
    maxReff = ZERO
    DO n = 1, Atmosphere%n_Clouds
      Reff = MAXVAL(Atmosphere%Cloud(n)%Effective_Radius)
      IF( Reff > maxReff) maxReff = Reff
    END DO
    DO n = 1, Atmosphere%n_Aerosols
      Reff = MAXVAL(Atmosphere%Aerosol(n)%Effective_Radius)
      IF( Reff > maxReff) maxReff = Reff
    END DO

    ! Compute the Mie parameter, 2.pi.Reff/lambda
    MieParameter = TWO * PI * maxReff * SC(SensorIndex)%Wavenumber(ChannelIndex)/10000.0_fp
    
    ! Determine the number of streams based on Mie parameter
    IF ( MieParameter < 0.01_fp ) THEN
      nStreams = 2
    ELSE IF( MieParameter < ONE ) THEN
      nStreams = 4
    ELSE
      nStreams = 6
    END IF

    ! Set RTSolution scattering info
    RTSolution%Scattering_Flag = .TRUE.
    RTSolution%n_full_Streams  = nStreams + 2

  END FUNCTION CRTM_Compute_nStreams


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RTSolution_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_RTSolution_Version( Id )
!
! OUTPUT ARGUMENTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_RTSolution_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_RTSolution_Version


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


  SUBROUTINE CRTM_Emission(n_Layers, & ! Input  number of atmospheric layers
                           n_Angles, & ! number angles used in SfcOptics
                    Diffuse_Surface, & ! Input  TRUE: Lambertian, FALSE: specular
                                  u, & ! Input  cosine of local viewing angle
                               T_OD, & ! Input  nadir layer optical depth
                  Planck_Atmosphere, & ! Input  atmospheric layer Planck radiance
                     Planck_Surface, & ! Input  surface Planck radiance 
                         emissivity, & ! Input  surface emissivity
                       reflectivity, & ! Input  surface reflectivity matrix
                direct_reflectivity, & ! Input  reflectivity for direct irradiance 
                  cosmic_background, & ! Input  cosmic background radiance
                   Solar_irradiance, & ! Input  Solar spectral irradiance
                   Is_Solar_Channel, & ! Input  Indicate solar affected channel
               Source_Zenith_Radian, & ! Input  Point source (e.g. solar) zenith angle
                                RTV)   ! Output TOA radiance and others
! ----------------------------------------------------------------------------- !
!  FUNCTION: Compute IR/MW upward radiance at the top of the profile.           !
!    This code heritages the concept from previous operational code.            !
!    It starts from cosmic background downward.                                 !
!    The downward radiance at the lower level is the transmitted radiance       !
!    from upper level adding the layer downward source function.                !
!    The downward angle is either the same as satellite viewing zenith for a    !
!    specular surface or the diffuse angle for a lambertian surface. The upward !
!    radiance at the surface is the surface emission term adding from surface   !
!    reflected downward radiance. Then, the upward radiance is the sum of       !
!    from the lower level transmitted radiance adding the upward layer          !
!    source function.                                                           !
!                                                                               !
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                        !
! ----------------------------------------------------------------------------- !

    ! Arguments
    INTEGER,                     INTENT(IN)     :: n_Layers
    INTEGER,                     INTENT(IN)     :: n_Angles
    LOGICAL,                     INTENT(IN)     :: Diffuse_Surface
    REAL(fp),                    INTENT(IN)     :: u
    REAL(fp), DIMENSION(:),      INTENT(IN)     :: T_OD
    REAL(fp), DIMENSION(0:),     INTENT(IN)     :: Planck_Atmosphere
    REAL(fp),                    INTENT(IN)     :: Planck_Surface
    REAL(fp), DIMENSION(:),      INTENT(IN)     :: emissivity
    REAL(fp), DIMENSION(:,:),    INTENT(IN)     :: reflectivity 
    REAL(fp), DIMENSION(:),      INTENT(IN)     :: direct_reflectivity 
    REAL(fp),                    INTENT(IN)     :: cosmic_background
    REAL(fp),                    INTENT(IN)     :: Solar_irradiance
    LOGICAL,                     INTENT(IN)     :: Is_Solar_Channel
    REAL(fp),                    INTENT(IN)     :: Source_Zenith_Radian
    TYPE(RTV_type),              INTENT(IN OUT) :: RTV
    ! Local variables
    REAL(fp) :: layer_source_up, cosine_u0 
    INTEGER :: k

    ! --------------------
    ! Downwelling radiance
    ! --------------------
    ! Determing secant downward angle from surface behavior
    IF( Diffuse_Surface ) THEN
      RTV%Secant_Down_Angle = SECANT_DIFFUSIVITY 
    ELSE
      RTV%Secant_Down_Angle = ONE/u
    END IF

    ! Start from the top of the atmosphere
    RTV%e_Level_Rad_DOWN(0) = cosmic_background 
    RTV%Total_OD = ZERO

    ! Loop from top layer to bottom layer
    DO k = 1, n_Layers
      ! Accumulate optical depth 
      RTV%Total_OD = RTV%Total_OD + T_OD(k)
      ! Layer downward transmittance
      RTV%e_Layer_Trans_DOWN(k) = EXP(-T_OD(k)*RTV%Secant_Down_Angle)
      ! Downward radiance  
      RTV%e_Level_Rad_DOWN(k) = (RTV%e_Level_Rad_DOWN(k-1)*RTV%e_Layer_Trans_DOWN(k)) + &
                                (Planck_Atmosphere(k)*(ONE-RTV%e_Layer_Trans_DOWN(k)))
    END DO

    ! ----------------
    ! Surface radiance
    ! ----------------
    ! upward radiance at the surface ( emission part + reflection part)
    RTV%e_Level_Rad_UP(n_Layers) = (emissivity(n_Angles)*Planck_Surface) + &
                                   (reflectivity(1,1)*RTV%e_Level_Rad_DOWN(n_Layers))

    ! Solar contribution to the upward radiance at the surface
    RTV%Down_Solar_Radiance = ZERO
    IF( Is_Solar_Channel ) THEN
      cosine_u0 = COS(Source_Zenith_Radian)
      IF( cosine_u0 > ZERO) THEN
        RTV%Down_Solar_Radiance = cosine_u0*EXP(-RTV%Total_OD/cosine_u0)*Solar_Irradiance/PI
        RTV%e_Level_Rad_UP(n_Layers) = RTV%e_Level_Rad_UP(n_Layers) + &
          (RTV%Down_Solar_Radiance*direct_reflectivity(1))
      END IF
    END IF

    ! ------------------
    ! Upwelling radiance
    ! ------------------
    ! Initialise upwelling radiance
    RTV%Up_Radiance = ZERO

    ! Loop from SFC->TOA
    DO k = n_Layers, 1, -1
      ! layer upwelling transmittance
      RTV%e_Layer_Trans_UP(k) = EXP(-T_OD(k)/u)
      ! layer upwelling source function
      layer_source_up = Planck_Atmosphere(k) * ( ONE - RTV%e_Layer_Trans_UP(k) )
      ! upwelling radiance (including reflected downwelling and surface)
      RTV%e_Level_Rad_UP(k-1) = (RTV%e_Level_Rad_UP(k)*RTV%e_Layer_Trans_UP(k)) + &
                                layer_source_up 
      ! upwelling radiance (atmospheric portion only)
      RTV%Up_Radiance = (RTV%Up_Radiance*RTV%e_Layer_Trans_UP(k)) + layer_source_up
    END DO

  END SUBROUTINE CRTM_Emission 


   SUBROUTINE CRTM_Emission_TL(n_Layers, & ! Input  number of atmospheric layers
                               n_Angles, & ! number angles used in SfcOptics
                                      u, & ! Input  cosine of local viewing angle
                                   T_OD, & ! Input  nadir layer optical depth
                      Planck_Atmosphere, & ! Input  atmospheric layer Planck radiance
                         Planck_Surface, & ! Input  surface Planck radiance 
                             emissivity, & ! Input  surface emissivity
                           reflectivity, & ! Input  surface reflectivity matrix
                    direct_reflectivity, & ! Input  reflectivity for direct irradiance 
                       Solar_irradiance, & ! Input  Solar spectral irradiance
                       Is_Solar_Channel, & ! Input  Indicate solar affected channel 
                   Source_Zenith_Radian, & ! Input  Point source (e.g. solar) zenith angle
                                    RTV, & ! Input  Structure containing forward part results 
                                T_OD_TL, & ! Input  tangent-linear of layer optical depth
                   Planck_Atmosphere_TL, & ! Input  TL atmospheric layer Planck radiance
                      Planck_Surface_TL, & ! Input  TL surface Planck radiance
                          emissivity_TL, & ! Input  TL surface emissivity
                        reflectivity_TL, & ! Input  TL surface reflectivity matrix
                 direct_reflectivity_TL, & ! Input  TL surface ditrct reflectivity
                              up_rad_TL)   ! Output TL TOA radiance
! --------------------------------------------------------------------------- !
!  FUNCTION: Compute tangent-linear upward radiance at the top of the         !
!    atmosphere using carried results in RTV structure from forward           !
!    calculation.                                                             !
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                      !
! --------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers, n_Angles
      LOGICAL, INTENT(IN) :: Is_Solar_Channel
      REAL (fp), INTENT(IN) :: Solar_irradiance, Source_Zenith_Radian
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  T_OD, emissivity,T_OD_TL,emissivity_TL
      REAL (fp), INTENT(IN), DIMENSION( :,: ) :: reflectivity ,reflectivity_TL
      REAL (fp), INTENT(IN), DIMENSION( : ) :: direct_reflectivity,direct_reflectivity_TL
      REAL (fp), INTENT(IN), DIMENSION( 0: ) :: Planck_Atmosphere,Planck_Atmosphere_TL
      REAL (fp), INTENT(IN) :: Planck_Surface,u,Planck_Surface_TL
      REAL (fp), INTENT(INOUT) :: up_rad_TL

    !   Structure RTV carried in variables from forward calculation. 
      TYPE(RTV_type), INTENT( IN) :: RTV
    !  internal variables
      REAL (fp) :: layer_source_up_TL, layer_source_down_TL,a_TL,down_rad_TL
      REAL (fp) :: Total_OD, Total_OD_TL
      INTEGER :: k
      REAL( fp) :: cosine_u0

    !#--------------------------------------------------------------------------#
    !#                -- Downwelling TL radiance   --                           #
    !#--------------------------------------------------------------------------#

      down_rad_TL = ZERO 
      Total_OD_TL = ZERO
    
      Total_OD = RTV%Total_OD
 
      DO k = 1, n_Layers
       ! accumulate tangent-linear optical depth
       Total_OD_TL = Total_OD_TL + T_OD_TL(k)
       a_TL = -T_OD_TL(k) * RTV%Secant_Down_Angle

       layer_source_down_TL = Planck_Atmosphere_TL(k) * ( ONE - RTV%e_Layer_Trans_DOWN(k) ) &
                            - Planck_Atmosphere(k) * RTV%e_Layer_Trans_DOWN(k) * a_TL
 
     ! downward tangent-linear radiance
     !    down_rad(k) = down_rad(k-1) * layer_trans(k) + layer_source_down 
       down_rad_TL = down_rad_TL*RTV%e_Layer_Trans_DOWN(k)  &
       +RTV%e_Level_Rad_DOWN(k-1)*RTV%e_Layer_Trans_DOWN(k)*a_TL+layer_source_down_TL
      ENDDO

    !#--------------------------------------------------------------------------#
    !#                -- at surface   --                                        #
    !#--------------------------------------------------------------------------#

      ! upward tangent-linear radiance at the surface 
       up_rad_TL =emissivity_TL(n_Angles)*Planck_Surface+emissivity(n_Angles)*Planck_Surface_TL &
       +reflectivity_TL(1,1)*RTV%e_Level_Rad_DOWN(n_Layers)+reflectivity(1,1)*down_rad_TL

      ! point source (e.g. solar radiation)
       IF( Is_Solar_Channel ) THEN
        cosine_u0 = cos(Source_Zenith_Radian)
        IF( cosine_u0 > ZERO) THEN
        up_rad_TL = up_rad_TL + cosine_u0*Solar_Irradiance/PI &
                  * direct_reflectivity_TL(1) * exp(-Total_OD/cosine_u0)   &
                  - Solar_Irradiance/PI * direct_reflectivity(1)    &
                  * Total_OD_TL * exp(-Total_OD/cosine_u0)
        ENDIF
       ENDIF

    !#--------------------------------------------------------------------------#
    !#            -- Upwelling TL radiance   --                                 #
    !#--------------------------------------------------------------------------#

      DO k = n_Layers, 1, -1
       a_TL = -T_OD_TL(k)/u 
       layer_source_up_TL = Planck_Atmosphere_TL(k) * ( ONE - RTV%e_Layer_Trans_UP(k) ) &
                          - Planck_Atmosphere(k) * RTV%e_Layer_Trans_UP(k) * a_TL
  
      ! upward tangent linear radiance
       up_rad_TL=up_rad_TL*RTV%e_Layer_Trans_UP(k)  &
       +RTV%e_Level_Rad_UP(k)*RTV%e_Layer_Trans_UP(k)*a_TL+layer_source_up_TL 
      ENDDO
!
      RETURN
      END SUBROUTINE CRTM_Emission_TL 
!
!
      SUBROUTINE CRTM_Emission_AD(n_Layers, & ! Input  number of atmospheric layers
                                  n_Angles, & ! number angles used in SfcOptics
                                         u, & ! Input  cosine of local viewing angle
                                      T_OD, & ! Input  nadir layer optical depth
                         Planck_Atmosphere, & ! Input  atmospheric layer Planck radiance
                            Planck_Surface, & ! Input  surface Planck radiance 
                                emissivity, & ! Input  surface emissivity
                              reflectivity, & ! Input  surface reflectivity matrix 
                       direct_reflectivity, & ! Input  surface reflectivity matrix 
                          Solar_irradiance, & ! Input  Solar spectral irradiance
                          Is_Solar_Channel, & ! Input  Indicate solar affected channel 
                      Source_Zenith_Radian, & ! Input  Point source (e.g. solar) zenith angle
                                       RTV, & ! Input  Structure containing forward part results 
                              up_rad_AD_in, & ! Input  adjoint radiance at the top
                                   T_OD_AD, & ! Output AD layer optical depth
                      Planck_Atmosphere_AD, & ! Output AD atmospheric layer Planck radiance
                         Planck_Surface_AD, & ! Output AD surface Planck radiance
                             emissivity_AD, & ! Output AD surface emissivity
                           reflectivity_AD, & ! Output AD surface reflectivity matrix
                    direct_reflectivity_AD)   ! Output AD surface direct reflectivity
! --------------------------------------------------------------------------- !
!  FUNCTION: Compute adjoint upward radiance at the top of the                !
!    atmosphere using carried results in RTV structure from forward           !
!    calculation.                                                             !
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                      !
! --------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers, n_Angles
      LOGICAL, INTENT(IN) :: Is_Solar_Channel
      REAL (fp), INTENT(IN) :: Solar_Irradiance, Source_Zenith_Radian
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  T_OD, emissivity
      REAL (fp), INTENT(IN), DIMENSION( :,: ) :: reflectivity 
      REAL (fp), INTENT(IN), DIMENSION( : ) :: direct_reflectivity 
      REAL (fp), INTENT(IN), DIMENSION( 0: ) ::  Planck_Atmosphere
      REAL (fp), INTENT(IN) :: Planck_Surface,u
      REAL (fp), INTENT(IN) :: up_rad_AD_in
      REAL (fp), INTENT(IN OUT), DIMENSION( : ) ::  T_OD_AD,emissivity_AD
      REAL (fp), INTENT(IN OUT), DIMENSION( :,: ) :: reflectivity_AD
      REAL (fp), INTENT(IN OUT), DIMENSION( : ) :: direct_reflectivity_AD
      REAL (fp), INTENT(IN OUT), DIMENSION( 0: ) ::  Planck_Atmosphere_AD
      REAL (fp), INTENT(IN OUT) :: Planck_Surface_AD
      TYPE(RTV_type), INTENT( IN) :: RTV
    !  internal variables
      REAL (fp) :: layer_source_up_AD, layer_source_down_AD,a_AD,down_rad_AD
      REAL (fp) :: cosine_u0, up_rad_AD, Total_OD, Total_OD_AD
      INTEGER :: k
!
    ! Initialize variables
      Total_OD_AD = ZERO
      T_OD_AD = ZERO
      Planck_Atmosphere_AD = ZERO
      Planck_Surface_AD = ZERO
      emissivity_AD = ZERO
      reflectivity_AD = ZERO
      direct_reflectivity_AD = ZERO
      up_rad_AD = up_rad_AD_in

    ! Total column optical depth carried from forward part
      Total_OD = RTV%Total_OD 

    !#--------------------------------------------------------------------------#
    !#                -- Upwelling adjoint radiance   --                        #
    !#--------------------------------------------------------------------------#
!
      DO k = 1, n_Layers
       a_AD = RTV%e_Level_Rad_UP(k)*RTV%e_Layer_Trans_UP(k)*up_rad_AD
       layer_source_up_AD = up_rad_AD
       up_rad_AD = up_rad_AD * RTV%e_Layer_Trans_UP(k)

       Planck_Atmosphere_AD(k) = Planck_Atmosphere_AD(k) + &
              layer_source_up_AD * (ONE - RTV%e_Layer_Trans_UP(k))
       a_AD = a_AD - Planck_Atmosphere(k) * RTV%e_Layer_Trans_UP(k)* layer_source_up_AD
 
       T_OD_AD(k) = T_OD_AD(k) - a_AD/u 
      ENDDO
    !#--------------------------------------------------------------------------#
    !#                -- at surface   --                                        #
    !#--------------------------------------------------------------------------#

       IF( Is_Solar_Channel ) THEN
        cosine_u0 = cos(Source_Zenith_Radian)
        IF( cosine_u0 > ZERO) THEN
        Total_OD_AD = -Solar_Irradiance/PI * direct_reflectivity(1) &
                    * up_rad_AD * exp(-Total_OD/cosine_u0)
        direct_reflectivity_AD(1) = cosine_u0 * Solar_Irradiance/PI &
                    * up_rad_AD* exp(-Total_OD/cosine_u0)
        ENDIF
       ENDIF

      emissivity_AD(n_Angles)=up_rad_AD*Planck_Surface
      Planck_Surface_AD = emissivity(n_Angles)*up_rad_AD
      reflectivity_AD(1,1)=up_rad_AD*RTV%e_Level_Rad_DOWN(n_Layers)
      down_rad_AD = reflectivity(1,1)*up_rad_AD
!
    !#--------------------------------------------------------------------------#
    !#                -- Downward adjoint radiance   --                         #
    !#--------------------------------------------------------------------------#
      DO k = n_Layers, 1, -1

       a_AD = RTV%e_Level_Rad_DOWN(k-1)*RTV%e_Layer_Trans_DOWN(k)*down_rad_AD
       layer_source_down_AD = down_rad_AD
       down_rad_AD = down_rad_AD*RTV%e_Layer_Trans_DOWN(k)

       Planck_Atmosphere_AD(k) = Planck_Atmosphere_AD(k) + layer_source_down_AD * &
                                 (ONE - RTV%e_Layer_Trans_DOWN(k))
       a_AD = a_AD - Planck_Atmosphere(k) * RTV%e_Layer_Trans_DOWN(k)* layer_source_down_AD
 

       T_OD_AD(k) = T_OD_AD(k) - a_AD * RTV%Secant_Down_Angle

       T_OD_AD(k) = T_OD_AD(k) + Total_OD_AD
      ENDDO

      down_rad_AD = ZERO 

      RETURN
      END SUBROUTINE CRTM_Emission_AD 
!
!
!
   SUBROUTINE CRTM_ADA(n_Layers, & ! Input  number of atmospheric layers
                              w, & ! Input  layer scattering albedo
                              g, & ! Input  layer asymmetry factor
                           T_OD, & ! Input  layer optical depth
              cosmic_background, & ! Input  cosmic background radiance
                     emissivity, & ! Input  surface emissivity
                   reflectivity, & ! Input  surface reflectivity matrix 
            direct_reflectivity, & ! Input  surface direct reflectivity 
                            RTV)   ! IN/Output upward radiance and others
! ------------------------------------------------------------------------- !
! FUNCTION:                                                                 !
!   This subroutine calculates IR/MW radiance at the top of the atmosphere  !
!   including atmospheric scattering. The scheme will include solar part.   !
!   The ADA algorithm computes layer reflectance and transmittance as well  !
!   as source function by the subroutine CRTM_Doubling_layer, then uses     !
!   an adding method to integrate the layer and surface components.         !
!                                                                           ! 
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                    !
! ------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers
      INTEGER nZ
      TYPE(RTV_type), INTENT( INOUT ) :: RTV
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  g,w,T_OD
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity, direct_reflectivity
      REAL (fp), INTENT(IN), DIMENSION( :,: ) :: reflectivity 
      REAL (fp), INTENT(IN) ::  cosmic_background

   ! -------------- internal variables --------------------------------- !
   !  Abbreviations:                                                     !
   !      s: scattering, rad: radiance, trans: transmission,             !
   !         refl: reflection, up: upward, down: downward                !
   ! --------------------------------------------------------------------!
      REAL (fp), DIMENSION(RTV%n_Angles, RTV%n_Angles) :: temporal_matrix
      REAL (fp), DIMENSION( RTV%n_Angles, n_Layers) :: refl_down 
      REAL (fp), DIMENSION(0:n_Layers) :: total_opt
      INTEGER :: i, j, k, Error_Status
      CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_ADA'
      CHARACTER(256) :: Message
!
       total_opt(0) = ZERO
       DO k = 1, n_Layers
         total_opt(k) = total_opt(k-1) + T_OD(k)
       END DO
       
       nZ = RTV%n_Angles
       RTV%s_Layer_Trans = ZERO
       RTV%s_Layer_Refl = ZERO
       RTV%s_Level_Refl_UP = ZERO
       RTV%s_Level_Rad_UP = ZERO
       RTV%s_Layer_Source_UP = ZERO
       RTV%s_Layer_Source_DOWN = ZERO
!
       RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,n_Layers)=reflectivity(1:RTV%n_Angles,1:RTV%n_Angles)
       
       IF( RTV%mth_Azi == 0 ) THEN
         RTV%s_Level_Rad_UP(1:RTV%n_Angles,n_Layers ) = emissivity(1:RTV%n_Angles)*RTV%Planck_Surface
       END IF
       
       IF( RTV%Solar_Flag_true ) THEN
         RTV%s_Level_Rad_UP(1:nZ,n_Layers ) = RTV%s_Level_Rad_UP(1:nZ,n_Layers )+direct_reflectivity(1:nZ)* &
           RTV%COS_SUN*RTV%Solar_irradiance/PI*exp(-total_opt(n_Layers)/RTV%COS_SUN)       
       END IF
!
!         UPWARD ADDING LOOP STARTS FROM BOTTOM LAYER TO ATMOSPHERIC TOP LAYER.
       DO 10 k = n_Layers, 1, -1
!
!      Compute tranmission and reflection matrices for a layer
      IF(w(k) > SCATTERING_ALBEDO_THRESHOLD) THEN 

       !  ----------------------------------------------------------- !
       !    CALL  multiple-stream algorithm for computing layer       !
       !    transmission, reflection, and source functions.           !
       !  ----------------------------------------------------------- !

       CALL CRTM_AMOM_layer(RTV%n_Streams,RTV%n_Angles,k,w(k),T_OD(k),total_opt(k-1),RTV%COS_Angle, & ! Input
        RTV%COS_Weight,RTV%Pff(:,:,k),RTV%Pbb(:,:,k),        & ! Input
        RTV%Planck_Atmosphere(k),                            & ! Input
        RTV)  
       !  ----------------------------------------------------------- !
       !    Adding method to add the layer to the present level       !
       !    to compute upward radiances and reflection matrix         !
       !    at new level.                                             !
       !  ----------------------------------------------------------- !

         temporal_matrix = -matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                            RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k))
          DO i = 1, RTV%n_Angles 
              temporal_matrix(i,i) = ONE + temporal_matrix(i,i)
          ENDDO

         RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k) = matinv(temporal_matrix, Error_Status)
         IF( Error_Status /= SUCCESS  ) THEN
         WRITE( Message,'("Error in matrix inversion matinv(temporal_matrix, Error_Status) ")' ) 
          CALL Display_Message( ROUTINE_NAME, &                                                    
                                TRIM(Message), &                                                   
                                Error_Status )                                          
          RETURN                                                                                    
         END IF
         
         RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k) =   &
          matmul(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k), RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k))
         refl_down(1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                                       RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))

         RTV%s_Level_Rad_UP(1:RTV%n_Angles,k-1 )=RTV%s_Layer_Source_UP(1:RTV%n_Angles,k)+ &
         matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),refl_down(1:RTV%n_Angles,k) &
               +RTV%s_Level_Rad_UP(1:RTV%n_Angles,k ))
         RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k), &
               RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k))
         RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k-1)=RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k) + &
         matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)) 

      ELSE
         DO i = 1, RTV%n_Angles 
           RTV%s_Layer_Trans(i,i,k) = exp(-T_OD(k)/RTV%COS_Angle(i))
           RTV%s_Layer_Source_UP(i,k) = RTV%Planck_Atmosphere(k) * (ONE - RTV%s_Layer_Trans(i,i,k) )
           RTV%s_Layer_Source_DOWN(i,k) = RTV%s_Layer_Source_UP(i,k)

         ENDDO

!         Adding method
         DO i = 1, RTV%n_Angles 
         RTV%s_Level_Rad_UP(i,k-1 )=RTV%s_Layer_Source_UP(i,k)+ &
         RTV%s_Layer_Trans(i,i,k)*(sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)*RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))  &
           +RTV%s_Level_Rad_UP(i,k ))
        ENDDO
        DO i = 1, RTV%n_Angles 
        DO j = 1, RTV%n_Angles 
         RTV%s_Level_Refl_UP(i,j,k-1)=RTV%s_Layer_Trans(i,i,k)*RTV%s_Level_Refl_UP(i,j,k)*RTV%s_Layer_Trans(j,j,k)
        ENDDO
        ENDDO
      ENDIF
   10     CONTINUE
!
!  Adding reflected cosmic background radiation
   IF( RTV%mth_Azi == 0 ) THEN
      DO i = 1, RTV%n_Angles 
      RTV%s_Level_Rad_UP(i,0)=RTV%s_Level_Rad_UP(i,0)+sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,0))*cosmic_background
      ENDDO
   END IF
!
     !   Forward part End

      RETURN
      END SUBROUTINE CRTM_ADA 
!
!
   SUBROUTINE CRTM_ADA_TL(n_Layers, & ! Input  number of atmospheric layers
                                 w, & ! Input  layer scattering albedo
                                 g, & ! Input  layer asymmetry factor
                              T_OD, & ! Input  layer optical depth
                 cosmic_background, & ! Input  cosmic background radiance
                        emissivity, & ! Input  surface emissivity
                                      !   reflectivity is stored in RTV !
               direct_reflectivity, & ! Input  direct reflectivity
                               RTV, & ! Input  structure containing forward part results 
              Planck_Atmosphere_TL, & ! Input  tangent-linear atmospheric layer Planck radiance 
                 Planck_Surface_TL, & ! Input  TL surface Planck radiance
                              w_TL, & ! Input  TL layer scattering albedo
                              g_TL, & ! Input  TL layer asymmetry factor
                           T_OD_TL, & ! Input  TL layer optical depth
                     emissivity_TL, & ! Input  TL surface emissivity
                   reflectivity_TL, & ! Input  TL  reflectivity
            direct_reflectivity_TL, & ! Input  TL  direct reflectivity
                            Pff_TL, & ! Input  TL forward phase matrix
                            Pbb_TL, & ! Input  TL backward phase matrix
                         s_rad_up_TL) ! Output TL upward radiance 
! ------------------------------------------------------------------------- !
! FUNCTION:                                                                 !
!   This subroutine calculates IR/MW tangent-linear radiance at the top of  !
!   the atmosphere including atmospheric scattering. The structure RTV      !
!   carried in forward part results.                                        !
!   The CRTM_ADA_TL algorithm computes layer tangent-linear reflectance and !
!   transmittance as well as source function by the subroutine              !
!   CRTM_Doubling_layer as source function by the subroutine                !
!   CRTM_Doubling_layer, then uses                                          !
!   an adding method to integrate the layer and surface components.         !
!                                                                           ! 
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                    !
! ------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers
      TYPE(RTV_type), INTENT(IN) :: RTV
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  g,w,T_OD
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity,direct_reflectivity
      REAL (fp), INTENT(IN) ::  cosmic_background

      REAL (fp),INTENT(IN),DIMENSION( :,:,: ) ::  Pff_TL, Pbb_TL
      REAL (fp),INTENT(IN),DIMENSION( : ) ::  g_TL,w_TL,T_OD_TL
      REAL (fp),INTENT(IN),DIMENSION( 0: ) ::  Planck_Atmosphere_TL
      REAL (fp),INTENT(IN) ::  Planck_Surface_TL
      REAL (fp),INTENT(IN),DIMENSION( : ) ::  emissivity_TL
      REAL (fp),INTENT(IN),DIMENSION( :,: ) :: reflectivity_TL 
      REAL (fp),INTENT(INOUT),DIMENSION( : ) :: s_rad_up_TL 
      REAL (fp),INTENT(INOUT),DIMENSION( : ) :: direct_reflectivity_TL
   ! -------------- internal variables --------------------------------- !
   !  Abbreviations:                                                     !
   !      s: scattering, rad: radiance, trans: transmission,             !
   !         refl: reflection, up: upward, down: downward                !
   ! --------------------------------------------------------------------!
      REAL (fp), DIMENSION(RTV%n_Angles,RTV%n_Angles) :: temporal_matrix_TL

      REAL (fp), DIMENSION( RTV%n_Angles, n_Layers) :: refl_down 
      REAL (fp), DIMENSION( RTV%n_Angles ) :: s_source_up_TL,s_source_down_TL,refl_down_TL
 
      REAL (fp), DIMENSION( RTV%n_Angles, RTV%n_Angles ) :: s_trans_TL,s_refl_TL,Refl_Trans_TL 
      REAL (fp), DIMENSION( RTV%n_Angles, RTV%n_Angles ) :: s_refl_up_TL,Inv_Gamma_TL,Inv_GammaT_TL
      REAL (fp), DIMENSION(0:n_Layers) :: total_opt, total_opt_TL
      INTEGER :: i, j, k, nZ
!
       nZ = RTV%n_Angles
       
       total_opt(0) = ZERO
       total_opt_TL(0) = ZERO
       DO k = 1, n_Layers
         total_opt(k) = total_opt(k-1) + T_OD(k)
         total_opt_TL(k) = total_opt_TL(k-1) + T_OD_TL(k)
       END DO
       
       Refl_Trans_TL = ZERO
       s_rad_up_TL = ZERO
       s_refl_up_TL = reflectivity_TL
       IF( RTV%mth_Azi == 0 ) THEN
       s_rad_up_TL = emissivity_TL * RTV%Planck_Surface + emissivity * Planck_Surface_TL
       END IF
 
       IF( RTV%Solar_Flag_true ) THEN
         s_rad_up_TL = s_rad_up_TL+direct_reflectivity_TL*RTV%COS_SUN*RTV%Solar_irradiance/PI  &
                     * exp(-total_opt(n_Layers)/RTV%COS_SUN) &
                     - direct_reflectivity * RTV%Solar_irradiance/PI  &
                     * total_opt_TL(n_Layers) * exp(-total_opt(n_Layers)/RTV%COS_SUN)
       END IF 
     
       DO 10 k = n_Layers, 1, -1
         s_source_up_TL = ZERO
         s_source_down_TL = ZERO
         s_trans_TL = ZERO
         s_refl_TL = ZERO
         Inv_GammaT_TL = ZERO
         Inv_Gamma_TL = ZERO
         refl_down_TL = ZERO
!
!      Compute tranmission and reflection matrices for a layer
      IF(w(k) > SCATTERING_ALBEDO_THRESHOLD) THEN 
       !  ----------------------------------------------------------- !
       !    CALL Doubling algorithm to computing forward and tagent   !
       !    layer transmission, reflection, and source functions.     !
       !  ----------------------------------------------------------- !

      call CRTM_AMOM_layer_TL(RTV%n_Streams,RTV%n_Angles,k,w(k),T_OD(k),total_opt(k-1), & !Input
         RTV%COS_Angle(1:RTV%n_Angles),RTV%COS_Weight(1:RTV%n_Angles)                   , & !Input
         RTV%Pff(:,:,k), RTV%Pbb(:,:,k),RTV%Planck_Atmosphere(k)                        , & !Input
         w_TL(k),T_OD_TL(k),total_opt_TL(k-1),Pff_TL(:,:,k)                             , & !Input
         Pbb_TL(:,:,k),Planck_Atmosphere_TL(k),RTV                                      , & !Input
         s_trans_TL,s_refl_TL,s_source_up_TL,s_source_down_TL)                              !Output
   
!         Adding method
         temporal_matrix_TL = -matmul(s_refl_up_TL,RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                            - matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),s_refl_TL)

         temporal_matrix_TL = matmul(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k),temporal_matrix_TL)
         Inv_Gamma_TL = -matmul(temporal_matrix_TL,RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k))

         Inv_GammaT_TL = matmul(s_trans_TL, RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                       + matmul(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k), Inv_Gamma_TL)

         refl_down(1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                               RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))
         refl_down_TL(:) = matmul(s_refl_up_TL,RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k)) &
                           + matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),s_source_down_TL(:))
         s_rad_up_TL(1:RTV%n_Angles)=s_source_up_TL(1:RTV%n_Angles)+ &
         matmul(Inv_GammaT_TL,refl_down(:,k)+RTV%s_Level_Rad_UP(1:RTV%n_Angles,k))  &
         +matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),refl_down_TL(1:RTV%n_Angles)+s_rad_up_TL(1:RTV%n_Angles))

         Refl_Trans_TL = matmul(s_refl_up_TL,RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                       + matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),s_trans_TL)

         s_refl_up_TL=s_refl_TL+matmul(Inv_GammaT_TL,RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                     +matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),Refl_Trans_TL)

         Refl_Trans_TL = ZERO
         
      ELSE

         DO i = 1, RTV%n_Angles
           s_trans_TL(i,i) = -T_OD_TL(k)/RTV%COS_Angle(i) * RTV%s_Layer_Trans(i,i,k)
           s_source_up_TL(i) = Planck_Atmosphere_TL(k) * (ONE - RTV%s_Layer_Trans(i,i,k) ) &
                             - RTV%Planck_Atmosphere(k) * s_trans_TL(i,i)
           s_source_down_TL(i) = s_source_up_TL(i)
         ENDDO

!         Adding method
        DO i = 1, RTV%n_Angles 
        s_rad_up_TL(i)=s_source_up_TL(i) &
        +s_trans_TL(i,i)*(sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)  &
        *RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))+RTV%s_Level_Rad_UP(i,k)) &
        +RTV%s_Layer_Trans(i,i,k)  &
        *(sum(s_refl_up_TL(i,1:RTV%n_Angles)*RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k)  &
        +RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)*s_source_down_TL(1:RTV%n_Angles))+s_rad_up_TL(i))

        ENDDO
                                       
        DO i = 1, RTV%n_Angles 
        DO j = 1, RTV%n_Angles 
        s_refl_up_TL(i,j)=s_trans_TL(i,i)*RTV%s_Level_Refl_UP(i,j,k)  &
        *RTV%s_Layer_Trans(j,j,k) &
        +RTV%s_Layer_Trans(i,i,k)*s_refl_up_TL(i,j)*RTV%s_Layer_Trans(j,j,k)  &
        +RTV%s_Layer_Trans(i,i,k)*RTV%s_Level_Refl_UP(i,j,k)*s_trans_TL(j,j)
        ENDDO
        ENDDO

      ENDIF
   10     CONTINUE
!
!  Adding reflected cosmic background radiation
    IF( RTV%mth_Azi == 0 ) THEN
      DO i = 1, RTV%n_Angles 
      s_rad_up_TL(i)=s_rad_up_TL(i)+sum(s_refl_up_TL(i,:))*cosmic_background
      ENDDO
    END IF
!
      RETURN
      END SUBROUTINE CRTM_ADA_TL
!
!
   SUBROUTINE CRTM_ADA_AD(n_Layers, & ! Input  number of atmospheric layers
                                 w, & ! Input  layer scattering albedo
                                 g, & ! Input  layer asymmetry factor
                              T_OD, & ! Input  layer optical depth
                 cosmic_background, & ! Input  cosmic background radiance
                        emissivity, & ! Input  surface emissivity
                                      !   surface reflectivity is stored in RTV !
               direct_reflectivity, & ! surface direct reflectivity
                               RTV, & ! Input  structure containing forward results 
                       s_rad_up_AD, & ! Input  adjoint upward radiance 
              Planck_Atmosphere_AD, & ! Output AD atmospheric layer Planck radiance
                 Planck_Surface_AD, & ! Output AD surface Planck radiance
                              w_AD, & ! Output AD layer scattering albedo
                              g_AD, & ! Output AD layer asymmetry factor
                           T_OD_AD, & ! Output AD layer optical depth
                     emissivity_AD, & ! Output AD surface emissivity
                   reflectivity_AD, & ! Output AD surface reflectivity
            direct_reflectivity_AD, & ! Output AD surface direct reflectivity
                            Pff_AD, & ! Output AD forward phase matrix
                            Pbb_AD)   ! Output AD backward phase matrix
! ------------------------------------------------------------------------- !
! FUNCTION:                                                                 !
!   This subroutine calculates IR/MW adjoint radiance at the top of         !
!   the atmosphere including atmospheric scattering. The structure RTV      !
!   carried in forward part results.                                        !
!   The CRTM_ADA_AD algorithm computes layer tangent-linear reflectance and !
!   transmittance as well as source function by the subroutine              !
!   CRTM_Doubling_layer as source function by the subroutine                !
!   CRTM_Doubling_layer, then uses                                          !
!   an adding method to integrate the layer and surface components.         !
!                                                                           ! 
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                    !
! ------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers
      TYPE(RTV_type), INTENT(IN) :: RTV
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  g,w,T_OD
      REAL (fp), INTENT(IN), DIMENSION( : ) ::  emissivity,direct_reflectivity
      REAL (fp), INTENT(IN) ::  cosmic_background

      REAL (fp),INTENT(INOUT),DIMENSION( :,:,: ) ::  Pff_AD, Pbb_AD
      REAL (fp),INTENT(INOUT),DIMENSION( : ) ::  g_AD,w_AD,T_OD_AD
      REAL (fp),INTENT(INOUT),DIMENSION( 0: ) ::  Planck_Atmosphere_AD
      REAL (fp),INTENT(INOUT) ::  Planck_Surface_AD
      REAL (fp),INTENT(INOUT),DIMENSION( : ) ::  emissivity_AD,direct_reflectivity_AD
      REAL (fp),INTENT(INOUT),DIMENSION( :,: ) :: reflectivity_AD 
      REAL (fp),INTENT(INOUT),DIMENSION( : ) :: s_rad_up_AD 
   ! -------------- internal variables --------------------------------- !
   !  Abbreviations:                                                     !
   !      s: scattering, rad: radiance, trans: transmission,             !
   !         refl: reflection, up: upward, down: downward                !
   ! --------------------------------------------------------------------!
      REAL (fp), DIMENSION(RTV%n_Angles,RTV%n_Angles) :: temporal_matrix_AD

      REAL (fp), DIMENSION( RTV%n_Angles, n_Layers) :: refl_down 
      REAL (fp), DIMENSION( RTV%n_Angles ) :: s_source_up_AD,s_source_down_AD,refl_down_AD
 
      REAL (fp), DIMENSION( RTV%n_Angles, RTV%n_Angles) :: s_trans_AD,s_refl_AD,Refl_Trans_AD
      REAL (fp), DIMENSION( RTV%n_Angles, RTV%n_Angles) :: s_refl_up_AD,Inv_Gamma_AD,Inv_GammaT_AD
      REAL (fp) :: sum_s_AD, sums_AD, xx
      REAL (fp), DIMENSION(0:n_Layers) :: total_opt, total_opt_AD
      INTEGER :: i, j, k,nZ
!
      nZ = RTV%n_Angles
      
       total_opt_AD = ZERO
       total_opt(0) = ZERO
       DO k = 1, n_Layers
         total_opt(k) = total_opt(k-1) + T_OD(k)
       END DO
!
       s_trans_AD = ZERO
       Planck_Atmosphere_AD = ZERO
       Planck_Surface_AD = ZERO
       s_refl_up_AD = ZERO

      Pff_AD = ZERO
      Pbb_AD = ZERO
!      T_OD_AD = ZERO
!  Adding reflected cosmic background radiation

      DO i = 1, RTV%n_Angles 
      sum_s_AD = s_rad_up_AD(i)*cosmic_background
      DO j = 1, RTV%n_Angles 
      s_refl_up_AD(i,j) = sum_s_AD
      ENDDO
      ENDDO

!
       DO 10 k = 1, n_Layers 
       s_source_up_AD = ZERO
       s_source_down_AD = ZERO
       s_trans_AD = ZERO
!
!      Compute tranmission and reflection matrices for a layer
      IF(w(k) > SCATTERING_ALBEDO_THRESHOLD) THEN 

        refl_down(1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                               RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))

        s_refl_AD = s_refl_up_AD
        Inv_GammaT_AD = matmul(s_refl_up_AD,transpose(RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        Refl_Trans_AD = matmul(transpose(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k)),s_refl_up_AD)

        s_refl_up_AD=matmul(Refl_Trans_AD,transpose(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        s_trans_AD=matmul(transpose(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k)),Refl_Trans_AD)
 
        s_source_up_AD(1:RTV%n_Angles) = s_rad_up_AD(1:RTV%n_Angles)

        DO i = 1, RTV%n_Angles 
        sums_AD = s_rad_up_AD(i)
        DO j = 1, RTV%n_Angles 
        Inv_GammaT_AD(i,j)=Inv_GammaT_AD(i,j)+sums_AD*(refl_down(j,k)+RTV%s_Level_Rad_UP(j,k))
        ENDDO 
        ENDDO

        refl_down_AD(1:RTV%n_Angles)=matmul(transpose(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k)),s_rad_up_AD(1:RTV%n_Angles))
        s_rad_up_AD(1:RTV%n_Angles)=matmul(transpose(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k)),s_rad_up_AD(1:RTV%n_Angles))

        DO i = 1, RTV%n_Angles 
        sums_AD = refl_down_AD(i)
        DO j = 1, RTV%n_Angles 
        s_refl_up_AD(i,j)=s_refl_up_AD(i,j)+sums_AD*RTV%s_Layer_Source_DOWN(j,k)
        ENDDO 
        ENDDO

        s_source_down_AD=matmul(transpose(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k)),refl_down_AD(:)) 

        s_trans_AD=s_trans_AD+matmul(Inv_GammaT_AD,transpose(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        Inv_Gamma_AD= matmul(transpose(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)),Inv_GammaT_AD)

        temporal_matrix_AD= -matmul(Inv_Gamma_AD,transpose(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        temporal_matrix_AD=matmul(transpose(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k)),temporal_matrix_AD)

        s_refl_up_AD=s_refl_up_AD-matmul(temporal_matrix_AD,transpose(RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        s_refl_AD=s_refl_AD-matmul(transpose(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k)),temporal_matrix_AD)
       !  ----------------------------------------------------------- !
       !    CALL Doubling algorithm to computing forward and tagent   !
       !    layer transmission, reflection, and source functions.     !
       !  ----------------------------------------------------------- !

      call CRTM_AMOM_layer_AD(RTV%n_Streams,RTV%n_Angles,k,w(k),T_OD(k),total_opt(k-1)      , & !Input
         RTV%COS_Angle,RTV%COS_Weight,RTV%Pff(:,:,k),RTV%Pbb(:,:,k),RTV%Planck_Atmosphere(k), & !Input
         s_trans_AD,s_refl_AD,s_source_up_AD,s_source_down_AD,RTV,w_AD(k),T_OD_AD(k)        , &
         total_opt_AD(k-1),Pff_AD(:,:,k),Pbb_AD(:,:,k),Planck_Atmosphere_AD(k))  !Output

      ELSE
        DO i = 1, RTV%n_Angles 
        DO j = 1, RTV%n_Angles 
        s_trans_AD(j,j)=s_trans_AD(j,j)+RTV%s_Layer_Trans(i,i,k)*RTV%s_Level_Refl_UP(i,j,k)*s_refl_up_AD(i,j)
        s_trans_AD(i,i)=s_trans_AD(i,i)+s_refl_up_AD(i,j)*RTV%s_Level_Refl_UP(i,j,k)*RTV%s_Layer_Trans(j,j,k)
        s_refl_up_AD(i,j)=RTV%s_Layer_Trans(i,i,k)*s_refl_up_AD(i,j)*RTV%s_Layer_Trans(j,j,k)
        ENDDO
        ENDDO
!         Adding method
       DO i = 1, RTV%n_Angles 

         s_source_up_AD(i)=s_rad_up_AD(i)
         s_trans_AD(i,i)=s_trans_AD(i,i)+s_rad_up_AD(i)*(sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)  &
         *RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))+RTV%s_Level_Rad_UP(i,k))

         sum_s_AD=RTV%s_Layer_Trans(i,i,k)*s_rad_up_AD(i)
         DO j = 1, RTV%n_Angles 
          s_refl_up_AD(i,j)=s_refl_up_AD(i,j)+sum_s_AD*RTV%s_Layer_Source_DOWN(j,k)
         ENDDO
                                            
         sum_s_AD=RTV%s_Layer_Trans(i,i,k)*s_rad_up_AD(i)
         DO j = 1, RTV%n_Angles 
          s_source_down_AD(j)=s_source_down_AD(j)+sum_s_AD*RTV%s_Level_Refl_UP(i,j,k)
         ENDDO
         s_rad_up_AD(i)=RTV%s_Layer_Trans(i,i,k)*s_rad_up_AD(i)

       ENDDO

         DO i = RTV%n_Angles, 1, -1
           s_source_up_AD(i) = s_source_up_AD(i) +  s_source_down_AD(i)
           s_trans_AD(i,i) = s_trans_AD(i,i) - RTV%Planck_Atmosphere(k) * s_source_up_AD(i)
           Planck_Atmosphere_AD(k) = Planck_Atmosphere_AD(k) + s_source_up_AD(i) * (ONE - RTV%s_Layer_Trans(i,i,k) )
           s_source_up_AD(i) = ZERO

           T_OD_AD(k)=T_OD_AD(k)-s_trans_AD(i,i)/RTV%COS_Angle(i)*RTV%s_Layer_Trans(i,i,k)
         ENDDO

      ENDIF
   10     CONTINUE

! 

       IF( RTV%Solar_Flag_true ) THEN         
         xx = RTV%Solar_irradiance/PI * exp(-total_opt(n_Layers)/RTV%COS_SUN)
         total_opt_AD(n_Layers) = total_opt_AD(n_Layers)  &
            - xx*sum(direct_reflectivity(1:RTV%n_Angles)*s_rad_up_AD(1:RTV%n_Angles))
         direct_reflectivity_AD = direct_reflectivity_AD + s_rad_up_AD * RTV%COS_SUN * xx
       END IF 
 
        emissivity_AD = s_rad_up_AD * RTV%Planck_Surface
        Planck_Surface_AD = sum(emissivity(:) * s_rad_up_AD(:) )

     
      reflectivity_AD = s_refl_up_AD
!
       s_rad_up_AD = ZERO
       s_refl_up_AD = ZERO
 
       
       DO k = n_Layers, 1, -1
         T_OD_AD(k) = T_OD_AD(k) + total_opt_AD(k)
         total_opt_AD(k-1) = total_opt_AD(k-1) + total_opt_AD(k)
       END DO
       
      RETURN
      END SUBROUTINE CRTM_ADA_AD
!
!
      SUBROUTINE CRTM_AMOM_layer(    n_streams, & ! Input, number of streams
                                            nZ, & ! Input, number of angles
                                            KL, & ! Input, KL-th layer 
                                 single_albedo, & ! Input, single scattering albedo
                                 optical_depth, & ! Input, layer optical depth
                                     total_opt, & ! Input, accumulated optical depth from the top to current layer top
                                     COS_Angle, & ! Input, COSINE of ANGLES
                                    COS_Weight, & ! Input, GAUSSIAN Weights
                                            ff, & ! Input, Phase matrix (forward part)
                                            bb, & ! Input, Phase matrix (backward part)
                                   Planck_Func, & ! Input, Planck for layer temperature
                                           RTV)   ! Output, layer transmittance, reflectance, and source 
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   Method and References
!    The transmittance and reflectance matrices is further derived from 
!    matrix operator method. The matrix operator method is referred to the paper by
!
!    Weng, F., and Q. Liu, 2003: Satellite Data Assimilation in Numerical Weather Prediction
!    Model: Part 1: Forward Radiative Transfer and Jacobian Modeling in Cloudy Atmospheres,
!    J. Atmos. Sci., 60, 2633-2646.
!
!   see also ADA method.
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: n_streams,nZ,KL
     TYPE(RTV_type), INTENT( INOUT ) :: RTV
     REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff,bb
     REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
     REAL(fp) :: single_albedo,optical_depth,Planck_Func,total_opt

     ! internal variables
     REAL(fp), DIMENSION(nZ,nZ) :: trans, refl, tempo
     REAL(fp) :: s, c, xx
     INTEGER :: i,j,N2,N2_1
     INTEGER :: Error_Status
     REAL(fp) :: EXPfactor,Sfactor,s_transmittance,Solar(2*nZ),V0(2*nZ,2*nZ),Solar1(2*nZ)
     REAL(fp) :: V1(2*nZ,2*nZ),Sfac2,source_up(nZ),source_down(nZ)    
     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AMOM_layer'
     CHARACTER(256) :: Message
     !
     ! for small layer optical depth, single scattering is applied.  
     IF( optical_depth < DELTA_OPTICAL_DEPTH ) THEN
        s = optical_depth * single_albedo
        DO i = 1, nZ
          RTV%Thermal_C(i,KL) = ZERO
          c = s/COS_Angle(i)
          DO j = 1, nZ
            RTV%s_Layer_Refl(i,j,KL) = c * bb(i,j) * COS_Weight(j)
            RTV%s_Layer_Trans(i,j,KL) = c * ff(i,j) * COS_Weight(j)
            IF( i == j ) THEN
              RTV%s_Layer_Trans(i,i,KL) = RTV%s_Layer_Trans(i,i,KL) + &
                ONE - optical_depth/COS_Angle(i)
            END IF
          IF( RTV%mth_Azi == 0 ) THEN
            RTV%Thermal_C(i,KL) = RTV%Thermal_C(i,KL) + &
              ( RTV%s_Layer_Refl(i,j,KL) + RTV%s_Layer_Trans(i,j,KL) )
          END IF
          ENDDO
         
          IF( RTV%mth_Azi == 0 ) THEN
          RTV%s_Layer_Source_UP(i,KL) = ( ONE - RTV%Thermal_C(i,KL) ) * Planck_Func
          RTV%s_Layer_Source_DOWN(i,KL) = RTV%s_Layer_Source_UP(i,KL)
          END IF
        ENDDO
        RETURN
     END IF
     !
     ! for numerical stability, 
     IF( single_albedo < max_albedo ) THEN
       s = single_albedo
     ELSE
       s = max_albedo
     END IF
     !
     ! building phase matrices
     DO i = 1, nZ
       c = s/COS_Angle(i)
       DO j = 1, nZ
         RTV%PM(i,j,KL) = c * bb(i,j) * COS_Weight(j)
         RTV%PP(i,j,KL) = c * ff(i,j) * COS_Weight(j)
       ENDDO
         RTV%PP(i,i,KL) = RTV%PP(i,i,KL) - ONE/COS_Angle(i)
     ENDDO
     RTV%PPM(1:nZ,1:nZ,KL) = RTV%PP(1:nZ,1:nZ,KL) - RTV%PM(1:nZ,1:nZ,KL)
     RTV%i_PPM(1:nZ,1:nZ,KL) = matinv( RTV%PPM(1:nZ,1:nZ,KL), Error_Status )
     IF( Error_Status /= SUCCESS  ) THEN
       WRITE( Message,'("Error in matrix inversion matinv( RTV%PPM(1:nZ,1:nZ,KL), Error_Status ) ")' ) 
       CALL Display_Message( ROUTINE_NAME, &                                                    
                             TRIM(Message), &                                                   
                             Error_Status )                                          
       RETURN                                                                                    
     END IF
         
     RTV%PPP(1:nZ,1:nZ,KL) = RTV%PP(1:nZ,1:nZ,KL) + RTV%PM(1:nZ,1:nZ,KL)
     RTV%HH(1:nZ,1:nZ,KL) = matmul( RTV%PPM(1:nZ,1:nZ,KL), RTV%PPP(1:nZ,1:nZ,KL) )   
     !
     ! save phase element RTV%HH, call ASYMTX for calculating eigenvalue and vectors.
     tempo = RTV%HH(1:nZ,1:nZ,KL)
     CALL ASYMTX(tempo,nZ,nZ,nZ,RTV%EigVe(1:nZ,1:nZ,KL),RTV%EigVa(1:nZ,KL),Error_Status)
     DO i = 1, nZ
       IF( RTV%EigVa(i,KL) > ZERO ) THEN         
         RTV%EigValue(i,KL) = sqrt( RTV%EigVa(i,KL) )
       ELSE
         RTV%EigValue(i,KL) = ZERO
       END IF
     END DO
     
     DO i = 1, nZ
       DO j = 1, nZ
         RTV%EigVeVa(i,j,KL) = RTV%EigVe(i,j,KL) * RTV%EigValue(j,KL)
       END DO
     END DO       
     RTV%EigVeF(1:nZ,1:nZ,KL) = matmul( RTV%i_PPM(1:nZ,1:nZ,KL), RTV%EigVeVa(1:nZ,1:nZ,KL) )
     !
     ! compute layer reflection, transmission and source function
     RTV%Gp(1:nZ,1:nZ,KL) = ( RTV%EigVe(1:nZ,1:nZ,KL) + RTV%EigVeF(1:nZ,1:nZ,KL) )/2.0_fp
     RTV%Gm(1:nZ,1:nZ,KL) = ( RTV%EigVe(1:nZ,1:nZ,KL) - RTV%EigVeF(1:nZ,1:nZ,KL) )/2.0_fp
     RTV%i_Gm(1:nZ,1:nZ,KL) = matinv( RTV%Gm(1:nZ,1:nZ,KL), Error_Status)

     IF( Error_Status /= SUCCESS  ) THEN
       WRITE( Message,'("Error in matrix inversion matinv( RTV%Gm(1:nZ,1:nZ,KL), Error_Status) ")' ) 
       CALL Display_Message( ROUTINE_NAME, &                                                    
                             TRIM(Message), &                                                   
                             Error_Status )                                          
       RETURN                                                                                    
     END IF             
     !
     DO i = 1, nZ
       xx = RTV%EigValue(i,KL)*optical_depth
       RTV%Exp_x(i,KL) = exp(-xx)
     END DO
     !
     DO i = 1, nZ
       DO j = 1, nZ
         RTV%A1(i,j,KL) = RTV%Gp(i,j,KL) * RTV%Exp_x(j,KL)
         RTV%A4(i,j,KL) = RTV%Gm(i,j,KL) * RTV%Exp_x(j,KL)
       END DO
     END DO
     !
     RTV%A2(1:nZ,1:nZ,KL) = matmul( RTV%i_Gm(1:nZ,1:nZ,KL), RTV%A1(1:nZ,1:nZ,KL) )
     RTV%A3(1:nZ,1:nZ,KL) = matmul( RTV%Gp(1:nZ,1:nZ,KL), RTV%A2(1:nZ,1:nZ,KL) )
     RTV%A5(1:nZ,1:nZ,KL) = matmul( RTV%A1(1:nZ,1:nZ,KL), RTV%A2(1:nZ,1:nZ,KL) )
     RTV%A6(1:nZ,1:nZ,KL) = matmul( RTV%A4(1:nZ,1:nZ,KL), RTV%A2(1:nZ,1:nZ,KL) )
     RTV%Gm_A5(1:nZ,1:nZ,KL) = RTV%Gm(1:nZ,1:nZ,KL) - RTV%A5(1:nZ,1:nZ,KL)     
     RTV%i_Gm_A5(1:nZ,1:nZ,KL) = matinv(RTV%Gm_A5(1:nZ,1:nZ,KL), Error_Status)
     IF( Error_Status /= SUCCESS  ) THEN
       WRITE( Message,'("Error in matrix inversion matinv(RTV%Gm_A5(1:nZ,1:nZ,KL), Error_Status) ")' ) 
       CALL Display_Message( ROUTINE_NAME, &                                                    
                             TRIM(Message), &                                                   
                             Error_Status )                                          
       RETURN                                                                                    
     END IF
     trans = matmul( RTV%A4(1:nZ,1:nZ,KL) - RTV%A3(1:nZ,1:nZ,KL), RTV%i_Gm_A5(1:nZ,1:nZ,KL) ) 
     refl = matmul( RTV%Gp(1:nZ,1:nZ,KL) - RTV%A6(1:nZ,1:nZ,KL), RTV%i_Gm_A5(1:nZ,1:nZ,KL) )
     !
     ! post processing  
     RTV%s_Layer_Trans(1:nZ,1:nZ,KL) = trans(:,:)
     RTV%s_Layer_Refl(1:nZ,1:nZ,KL) = refl(:,:)
     RTV%s_Layer_Source_UP(:,KL) = ZERO
     IF( RTV%mth_Azi == 0 ) THEN
       DO i = 1, nZ
         RTV%Thermal_C(i,KL) = ZERO
         DO j = 1, n_Streams
           RTV%Thermal_C(i,KL) = RTV%Thermal_C(i,KL) + (trans(i,j) + refl(i,j) )
         ENDDO
         IF ( i == nZ .AND. nZ == (n_Streams+1) ) THEN
           RTV%Thermal_C(i,KL) = RTV%Thermal_C(i,KL) + trans(nZ,nZ)
         END IF
         RTV%s_Layer_Source_UP(i,KL) = ( ONE - RTV%Thermal_C(i,KL) ) * Planck_Func
         RTV%s_Layer_Source_DOWN(i,KL) = RTV%s_Layer_Source_UP(i,KL)
       ENDDO
     END IF
     !
     !  compute visible part for visible channels during daytime
     IF( RTV%Visible_Flag_true ) THEN
       N2 = 2 * nZ
       N2_1 = N2 - 1
       source_up = ZERO
       source_down = ZERO
       !
       ! Solar source  
       Sfactor = single_albedo*RTV%Solar_irradiance/PI
       IF( RTV%mth_Azi == 0 ) Sfactor = Sfactor/TWO
         EXPfactor = exp(-optical_depth/RTV%COS_SUN)
         s_transmittance = exp(-total_opt/RTV%COS_SUN)
        
         DO i = 1, nZ     
           Solar(i) = -bb(i,nZ+1)*Sfactor
           Solar(i+nZ) = -ff(i,nZ+1)*Sfactor

           DO j = 1, nZ
             V0(i,j) = single_albedo * ff(i,j) * COS_Weight(j)
             V0(i+nZ,j) = single_albedo * bb(i,j) * COS_Weight(j)
             V0(i,j+nZ) = V0(i+nZ,j)
             V0(nZ+i,j+nZ) = V0(i,j)
           ENDDO
           V0(i,i) = V0(i,i) - ONE - COS_Angle(i)/RTV%COS_SUN
           V0(i+nZ,i+nZ) = V0(i+nZ,i+nZ) - ONE + COS_Angle(i)/RTV%COS_SUN
         ENDDO
   
         V1(1:N2_1,1:N2_1) = matinv(V0(1:N2_1,1:N2_1), Error_Status)
         IF( Error_Status /= SUCCESS  ) THEN
           WRITE( Message,'("Error in matrix inversion matinv(V0(1:N2_1,1:N2_1), Error_Status) ")' ) 
           CALL Display_Message( ROUTINE_NAME, &                                                    
                                 TRIM(Message), &                                                   
                                 Error_Status )                                          
           RETURN                                                                                    
         END IF         
         
         Solar1(1:N2_1) = matmul( V1(1:N2_1,1:N2_1), Solar(1:N2_1) )
         Solar1(N2) = ZERO
         Sfac2 = Solar(N2) - sum( V0(N2,1:N2_1)*Solar1(1:N2_1) )
             
      
         DO i = 1, nZ
           source_up(i) = Solar1(i)
           source_down(i) = EXPfactor*Solar1(i+nZ)
           DO j = 1, nZ
            source_up(i) =source_up(i)-refl(i,j)*Solar1(j+nZ)-trans(i,j)*EXPfactor*Solar1(j)
            source_down(i) =source_down(i) -trans(i,j)*Solar1(j+nZ) -refl(i,j)*EXPfactor*Solar1(j)
           END DO
         END DO
         ! specific treatment for downeward source function
         IF( abs( V0(N2,N2) ) > 0.0001_fp ) THEN
           source_down(nZ) =source_down(nZ) +(EXPfactor-trans(nZ,nZ))*Sfac2/V0(N2,N2)
         ELSE
           source_down(nZ) =source_down(nZ) -EXPfactor*Sfac2*optical_depth/COS_Angle(nZ)
         END IF
     
         source_up(1:nZ) = source_up(1:nZ)*s_transmittance
         source_down(1:nZ) = source_down(1:nZ)*s_transmittance

         RTV%s_Layer_Source_UP(1:nZ,KL) = RTV%s_Layer_Source_UP(1:nZ,KL)+source_up(1:nZ)
         RTV%s_Layer_Source_DOWN(1:nZ,KL) = RTV%s_Layer_Source_DOWN(1:nZ,KL)+source_down(1:nZ)
      END IF
       
      RETURN

      END SUBROUTINE CRTM_AMOM_layer
!
!
      SUBROUTINE CRTM_AMOM_layer_TL( n_streams, & ! Input, number of streams
                                            nZ, & ! Input, number of angles
                                            KL, & ! Input, KL-th layer 
                                 single_albedo, & ! Input, single scattering albedo
                                 optical_depth, & ! Input, layer optical depth
                                     total_opt, & ! Input, accumulated optical depth from the top to current layer top
                                     COS_Angle, & ! Input, COSINE of ANGLES
                                    COS_Weight, & ! Input, GAUSSIAN Weights
                                            ff, & ! Input, Phase matrix (forward part)
                                            bb, & ! Input, Phase matrix (backward part)
                                   Planck_Func, & ! Input, Planck for layer temperature
                              single_albedo_TL, & ! Input, tangent-linear single albedo
                              optical_depth_TL, & ! Input, TL layer optical depth
                                  total_opt_TL, & ! Input, accumulated TL optical depth from the top to current layer top
                                         ff_TL, & ! Input, TL forward Phase matrix
                                         bb_TL, & ! Input, TL backward Phase matrix
                                Planck_Func_TL, & ! Input, TL Planck for layer temperature
                                           RTV, & ! Input, structure containing forward results 
                                      trans_TL, & ! Output, layer tangent-linear trans 
                                       refl_TL, & ! Output, layer tangent-linear refl 
                                  source_up_TL, & ! Output, layer tangent-linear source_up 
                                source_down_TL)   ! Output, layer tangent-linear source_down 
                                                                
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute TL layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   see also ADA method.
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: n_streams,nZ,KL
     TYPE(RTV_type), INTENT( IN ) :: RTV
     REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff,bb
     REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
     REAL(fp) :: single_albedo,optical_depth,Planck_Func,total_opt
     !
     ! internal variables
     REAL(fp) :: s, c, s_TL,c_TL,xx_TL
     INTEGER :: i,j
     INTEGER :: Error_Status
     !
     ! Tangent-Linear Part
     REAL(fp), INTENT(OUT), DIMENSION( :,: ) :: trans_TL,refl_TL
     REAL(fp), INTENT(OUT), DIMENSION( : ) :: source_up_TL,source_down_TL
     
     REAL(fp), INTENT(IN) :: single_albedo_TL
     REAL(fp), INTENT(IN) :: optical_depth_TL,Planck_Func_TL,total_opt_TL
     REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff_TL,bb_TL

     REAL(fp), DIMENSION(nZ) :: Exp_x_TL,EigVa_TL,EigValue_TL
     REAL(fp), DIMENSION(nZ,nZ) :: i_Gm_TL, Gm_TL, Gp_TL, EigVe_TL, EigVeF_TL, EigVeVa_TL
     REAL(fp), DIMENSION(nZ,nZ) :: A1_TL,A2_TL,A3_TL,A4_TL,A5_TL,A6_TL,Gm_A5_TL,i_Gm_A5_TL
     REAL(fp), DIMENSION(nZ,nZ) :: HH_TL,PM_TL,PP_TL,PPM_TL,i_PPM_TL,PPP_TL

     REAL(fp) :: EXPfactor,Sfactor,s_transmittance,Solar(2*nZ),V0(2*nZ,2*nZ),Solar1(2*nZ)
     REAL(fp) :: V1(2*nZ,2*nZ),Sfac2,source_up(nZ),source_down(nZ) 
     REAL(fp) :: EXPfactor_TL,Sfactor_TL,s_transmittance_TL,Solar_TL(2*nZ),V0_TL(2*nZ,2*nZ),Solar1_TL(2*nZ)
     REAL(fp) :: Sfac2_TL
     REAL(fp), DIMENSION( nZ ) :: thermal_up_TL,thermal_down_TL,C1_TL,C2_TL,C1,C2
     REAL(fp), DIMENSION(nZ,nZ) :: trans, refl
     INTEGER :: N2, N2_1
     REAL(fp) :: Thermal_C_TL
     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AMOM_layer_TL'
     CHARACTER(256) :: Message
     !
     ! for small layer optical depth, single scattering is applied.  
     IF( optical_depth < DELTA_OPTICAL_DEPTH ) THEN
       s = optical_depth * single_albedo
       s_TL = optical_depth_TL * single_albedo + optical_depth * single_albedo_TL
       DO i = 1, nZ
         Thermal_C_TL = ZERO
         c = s/COS_Angle(i)
         c_TL = s_TL/COS_Angle(i)
         DO j = 1, nZ
           refl_TL(i,j) = (c_TL * bb(i,j) + c*bb_TL(i,j) )* COS_Weight(j)
           trans_TL(i,j) = (c_TL * ff(i,j) + c*ff_TL(i,j) )* COS_Weight(j)
           IF( i == j ) THEN
             trans_TL(i,j) = trans_TL(i,j) - optical_depth_TL/COS_Angle(i)
           END IF
         
           IF( RTV%mth_Azi == 0 ) THEN
             Thermal_C_TL = Thermal_C_TL + refl_TL(i,j) + trans_TL(i,j)
           END IF
         ENDDO
         IF( RTV%mth_Azi == 0 ) THEN
           source_up_TL(i) = -Thermal_C_TL * Planck_Func + &
             ( ONE - RTV%Thermal_C(i,KL) ) * Planck_Func_TL
           source_down_TL(i) = source_up_TL(i)
         END IF
       ENDDO
       RETURN
     END IF

     !
     ! for numerical stability, 
     IF( single_albedo < max_albedo ) THEN
       s = single_albedo
       s_TL = single_albedo_TL
     ELSE
       s = max_albedo
       s_TL = 0.0_fp
     END IF
     !
     ! building TL phase matrices
     DO i = 1, nZ
       c = s/COS_Angle(i)
       c_TL = s_TL/COS_Angle(i)
       DO j = 1, nZ
         PM_TL(i,j) = (c_TL * bb(i,j) + c*bb_TL(i,j) ) * COS_Weight(j)
         PP_TL(i,j) = (c_TL * ff(i,j) + c*ff_TL(i,j) ) * COS_Weight(j)
       END DO
     ENDDO

     PPM_TL(:,:) = PP_TL(:,:) - PM_TL(:,:)
     i_PPM_TL(:,:) = - matmul( RTV%i_PPM(1:nZ,1:nZ,KL), matmul(PPM_TL(:,:),RTV%i_PPM(1:nZ,1:nZ,KL)) )     
     PPP_TL(:,:) = PP_TL(:,:) + PM_TL(:,:)
     HH_TL(:,:) = matmul( PPM_TL(:,:), RTV%PPP(1:nZ,1:nZ,KL) )+matmul( RTV%PPM(1:nZ,1:nZ,KL), PPP_TL(:,:) )
     !
     ! compute TL eigenvectors EigVe, and eigenvalues EigVa
     CALL ASYMTX_TL(COS_Angle(nZ),n_streams,nZ,RTV%EigVe(1:nZ,1:nZ,KL),RTV%EigVa(1:nZ,KL),HH_TL, &
          EigVe_TL,EigVa_TL,Error_Status)

     DO i = 1, nZ
       IF( RTV%EigVa(i,KL) > ZERO ) THEN
         EigValue_TL(i) = 0.5_fp*EigVa_TL(i)/RTV%EigValue(i,KL)
       ELSE
         EigValue_TL(i) = ZERO 
       END IF
     END DO
     EigVeVa_TL = ZERO
     
     DO i = 1, nZ
       DO j = 1, nZ
         EigVeVa_TL(i,j) = EigVe_TL(i,j) * RTV%EigValue(j,KL)+RTV%EigVe(i,j,KL) * EigValue_TL(j)
       END DO
     END DO     
     EigVeF_TL(:,:) = matmul( i_PPM_TL(:,:), RTV%EigVeVa(1:nZ,1:nZ,KL) )  &
                    + matmul( RTV%i_PPM(1:nZ,1:nZ,KL), EigVeVa_TL(:,:) )
     !  
     ! compute TL reflection and transmission matrices, TL source function     
     Gp_TL(:,:) = ( EigVe_TL(:,:) + EigVeF_TL(:,:) )/2.0_fp
     Gm_TL(:,:) = ( EigVe_TL(:,:) - EigVeF_TL(:,:) )/2.0_fp
     i_Gm_TL = -matmul( RTV%i_Gm(1:nZ,1:nZ,KL), matmul(Gm_TL,RTV%i_Gm(1:nZ,1:nZ,KL)) )
     DO i = 1, nZ
       xx_TL = EigValue_TL(i)*optical_depth+RTV%EigValue(i,KL)*optical_depth_TL
       Exp_x_TL(i) = -xx_TL*RTV%Exp_x(i,KL)
     END DO

     DO i = 1, nZ
       DO j = 1, nZ
         A1_TL(i,j) = Gp_TL(i,j)* RTV%Exp_x(j,KL)+ RTV%Gp(i,j,KL)* Exp_x_TL(j)
         A4_TL(i,j) = Gm_TL(i,j)* RTV%Exp_x(j,KL)+ RTV%Gm(i,j,KL)* Exp_x_TL(j)
       END DO
     END DO        
     A2_TL(:,:) = matmul(i_Gm_TL(:,:),RTV%A1(1:nZ,1:nZ,KL))+matmul(RTV%i_Gm(1:nZ,1:nZ,KL),A1_TL(:,:))          
     A3_TL(:,:) = matmul(Gp_TL(:,:),RTV%A2(1:nZ,1:nZ,KL))+matmul(RTV%Gp(1:nZ,1:nZ,KL),A2_TL(:,:))
     A5_TL(:,:) = matmul(A1_TL(:,:),RTV%A2(1:nZ,1:nZ,KL))+matmul(RTV%A1(1:nZ,1:nZ,KL),A2_TL(:,:))
     A6_TL(:,:) = matmul(A4_TL(:,:),RTV%A2(1:nZ,1:nZ,KL))+matmul(RTV%A4(1:nZ,1:nZ,KL),A2_TL(:,:))
  
     Gm_A5_TL(:,:) = Gm_TL(:,:) - A5_TL(:,:)
     i_Gm_A5_TL(:,:) = -matmul( RTV%i_Gm_A5(1:nZ,1:nZ,KL),matmul(Gm_A5_TL,RTV%i_Gm_A5(1:nZ,1:nZ,KL)))
     !
     ! T = matmul( RTV%A4(:,:,KL) - RTV%A3(:,:,KL), RTV%i_Gm_A5(:,:,KL) )
     trans_TL = matmul( A4_TL(:,:) - A3_TL(:,:), RTV%i_Gm_A5(1:nZ,1:nZ,KL) )  &
          + matmul( RTV%A4(1:nZ,1:nZ,KL) - RTV%A3(1:nZ,1:nZ,KL), i_Gm_A5_TL(:,:) )
     refl_TL = matmul( Gp_TL(:,:) - A6_TL(:,:), RTV%i_Gm_A5(1:nZ,1:nZ,KL) )  &
          + matmul( RTV%Gp(1:nZ,1:nZ,KL) - RTV%A6(1:nZ,1:nZ,KL), i_Gm_A5_TL(:,:) )

     trans(1:nZ,1:nZ) = RTV%s_Layer_Trans(1:nZ,1:nZ,KL)
     refl(1:nZ,1:nZ) = RTV%s_Layer_Refl(1:nZ,1:nZ,KL)
     Source_UP_TL = ZERO
     Source_DOWN_TL = ZERO    
     !
     ! Thermal part
     IF( RTV%mth_Azi == 0 ) THEN  
       DO i = 1, nZ
         Thermal_C_TL = ZERO
         DO j = 1, n_Streams 
           Thermal_C_TL = Thermal_C_TL + (trans_TL(i,j) + refl_TL(i,j))
         ENDDO
         IF(i == nZ .AND. nZ == (n_Streams+1)) THEN
           Thermal_C_TL = Thermal_C_TL + trans_TL(nZ,nZ)
         ENDIF
         thermal_up_TL(i) = -Thermal_C_TL * Planck_Func  &
           + ( ONE - RTV%Thermal_C(i,KL) ) * Planck_Func_TL
         thermal_down_TL(i) = thermal_up_TL(i)
       ENDDO
     END IF
     
     !
     ! for visible channels at daytime
     IF( RTV%Visible_Flag_true ) THEN 
       N2 = 2 * nZ
       N2_1 = N2 - 1
       V0 = ZERO
       V1 = ZERO
       Solar = ZERO
       Solar1 = ZERO
       Sfac2 = ZERO
       V0_TL = ZERO
       Solar_TL = ZERO
       Solar1_TL = ZERO
       Sfac2_TL = ZERO
       !
       ! Solar source  
       Sfactor = single_albedo*RTV%Solar_irradiance/PI
       Sfactor_TL = single_albedo_TL*RTV%Solar_irradiance/PI
               
       IF( RTV%mth_Azi == 0 ) THEN
         Sfactor = Sfactor/TWO
         Sfactor_TL = Sfactor_TL/TWO
       END IF
       EXPfactor = exp(-optical_depth/RTV%COS_SUN)
       EXPfactor_TL = -optical_depth_TL/RTV%COS_SUN*EXPfactor
      
       s_transmittance = exp(-total_opt/RTV%COS_SUN)
       s_transmittance_TL = -total_opt_TL/RTV%COS_SUN*s_transmittance
       
       DO i = 1, nZ     
         Solar(i) = -bb(i,nZ+1)*Sfactor
         Solar_TL(i) = -bb_TL(i,nZ+1)*Sfactor-bb(i,nZ+1)*Sfactor_TL
         Solar(i+nZ) = -ff(i,nZ+1)*Sfactor
         Solar_TL(i+nZ) = -ff_TL(i,nZ+1)*Sfactor-ff(i,nZ+1)*Sfactor_TL
         DO j = 1, nZ
           V0(i,j) = single_albedo * ff(i,j) * COS_Weight(j)
           V0_TL(i,j) = single_albedo_TL*ff(i,j)*COS_Weight(j)+single_albedo*ff_TL(i,j)*COS_Weight(j)
           V0(i+nZ,j) = single_albedo * bb(i,j) * COS_Weight(j)
           V0_TL(i+nZ,j) = single_albedo_TL*bb(i,j)*COS_Weight(j)+single_albedo*bb_TL(i,j)*COS_Weight(j)
           V0(i,j+nZ) = V0(i+nZ,j)
           V0_TL(i,j+nZ) = V0_TL(i+nZ,j)
           V0(nZ+i,j+nZ) = V0(i,j)
           V0_TL(nZ+i,j+nZ) = V0_TL(i,j)
         ENDDO
         V0(i,i) = V0(i,i) - ONE - COS_Angle(i)/RTV%COS_SUN
         V0(i+nZ,i+nZ) = V0(i+nZ,i+nZ) - ONE + COS_Angle(i)/RTV%COS_SUN
       ENDDO

       V1(1:N2_1,1:N2_1) = matinv(V0(1:N2_1,1:N2_1), Error_Status)
       IF( Error_Status /= SUCCESS  ) THEN
         WRITE( Message,'("Error in matrix inversion matinv(V0(1:N2_1,1:N2_1), Error_Status) ")' ) 
         CALL Display_Message( ROUTINE_NAME, &                                                    
                               TRIM(Message), &                                                   
                               Error_Status )                                          
         RETURN                                                                                    
       END IF           
       
       Solar1(1:N2_1) = matmul( V1(1:N2_1,1:N2_1), Solar(1:N2_1) )
       
       Solar(1:N2_1) =  matmul( V1(1:N2_1,1:N2_1),Solar(1:N2_1) )
       Solar1_TL(1:N2_1) = matmul( V0_TL(1:N2_1,1:N2_1),Solar(1:N2_1) )
       Solar1_TL(1:N2_1) = -matmul(  V1(1:N2_1,1:N2_1),Solar1_TL(1:N2_1) )  &
                         + matmul( V1(1:N2_1,1:N2_1), Solar_TL(1:N2_1) )
       
       Solar1(N2) = ZERO
       Solar1_TL(N2) = ZERO
       Sfac2 = Solar(N2) - sum( V0(N2,1:N2_1)*Solar1(1:N2_1) )
       Sfac2_TL = Solar_TL(N2) - sum( V0_TL(N2,1:N2_1)*Solar1(1:N2_1) )  &
                - sum( V0(N2,1:N2_1)*Solar1_TL(1:N2_1) )
               
       DO i = 1, nZ
         source_up(i) = Solar1(i)
         source_up_TL(i) = Solar1_TL(i)
         source_down(i) = EXPfactor*Solar1(i+nZ)
         source_down_TL(i) = EXPfactor_TL*Solar1(i+nZ)+EXPfactor*Solar1_TL(i+nZ)
         DO j = 1, nZ
           source_up(i) =source_up(i)-refl(i,j)*Solar1(j+nZ)-trans(i,j)*EXPfactor*Solar1(j)
           source_up_TL(i) =source_up_TL(i)-refl_TL(i,j)*Solar1(j+nZ) -refl(i,j)*Solar1_TL(j+nZ) &
           - trans_TL(i,j)*EXPfactor*Solar1(j) - trans(i,j)*EXPfactor_TL*Solar1(j) -trans(i,j)*EXPfactor*Solar1_TL(j)
           source_down(i) =source_down(i) -trans(i,j)*Solar1(j+nZ) -refl(i,j)*EXPfactor*Solar1(j)
           source_down_TL(i) =source_down_TL(i) -trans_TL(i,j)*Solar1(j+nZ) -trans(i,j)*Solar1_TL(j+nZ) &
           -refl_TL(i,j)*EXPfactor*Solar1(j) -refl(i,j)*EXPfactor_TL*Solar1(j) -refl(i,j)*EXPfactor*Solar1_TL(j)
         END DO
       END DO
       !
       ! specific treatment for downeward source function
       IF( abs( V0(N2,N2) ) > 0.0001_fp ) THEN
         source_down(nZ) =source_down(nZ) +(EXPfactor-trans(nZ,nZ))*Sfac2/V0(N2,N2)
         source_down_TL(nZ) =source_down_TL(nZ) +(EXPfactor_TL-trans_TL(nZ,nZ))*Sfac2/V0(N2,N2) &
          +(EXPfactor-trans(nZ,nZ))*Sfac2_TL/V0(N2,N2)-(EXPfactor-trans(nZ,nZ))*Sfac2*V0_TL(N2,N2)/V0(N2,N2)/V0(N2,N2)
       ELSE
         source_down(nZ) =source_down(nZ) -EXPfactor*Sfac2*optical_depth/COS_Angle(nZ)
         source_down_TL(nZ) =source_down_TL(nZ) -EXPfactor_TL*Sfac2*optical_depth/COS_Angle(nZ)  &
         -EXPfactor*Sfac2_TL*optical_depth/COS_Angle(nZ)-EXPfactor*Sfac2*optical_depth_TL/COS_Angle(nZ)
       END IF
        
       ! source_up(1:nZ) = source_up(1:nZ)*s_transmittance
        source_up_TL(1:nZ) = source_up_TL(1:nZ)*s_transmittance+source_up(1:nZ)*s_transmittance_TL
       ! source_down(1:nZ) = source_down(1:nZ)*s_transmittance
        source_down_TL(1:nZ) = source_down_TL(1:nZ)*s_transmittance + source_down(1:nZ)*s_transmittance_TL
     END IF

     source_up_TL(:) = source_up_TL(:) + thermal_up_TL(:)
     source_down_TL(:) = source_down_TL(:) + thermal_down_TL(:)
    
     RETURN

     END SUBROUTINE CRTM_AMOM_layer_TL
!
!
     SUBROUTINE CRTM_AMOM_layer_AD(  n_streams, & ! Input, number of streams
                                            nZ, & ! Input, number of angles
                                            KL, & ! Input, KL-th layer 
                                 single_albedo, & ! Input, single scattering albedo
                                 optical_depth, & ! Input, layer optical depth
                                     total_opt, & ! Input, 
                                     COS_Angle, & ! Input, COSINE of ANGLES
                                    COS_Weight, & ! Input, GAUSSIAN Weights
                                            ff, & ! Input, Phase matrix (forward part)
                                            bb, & ! Input, Phase matrix (backward part)
                                   Planck_Func, & ! Input, Planck for layer temperature
                                      trans_AD, & ! Input, layer tangent-linear trans 
                                       refl_AD, & ! Input, layer tangent-linear refl 
                                  source_up_AD, & ! Input, layer tangent-linear source_up 
                                source_down_AD, & ! Input, layer tangent-linear source_down 
                                           RTV, & ! Input, structure containing forward results 
                              single_albedo_AD, & ! Output adjoint single scattering albedo
                              optical_depth_AD, & ! Output AD layer optical depth
                                  total_opt_AD, & ! Output AD accumulated optical depth ftom TOA to current layer top
                                         ff_AD, & ! Output AD forward Phase matrix
                                         bb_AD, & ! Output AD backward Phase matrix
                                Planck_Func_AD)   ! Output AD Planck for layer temperature
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute AD layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   see also ADA method.
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: n_streams,nZ,KL
     TYPE(RTV_type), INTENT( IN ) :: RTV
     REAL(fp), INTENT(IN), DIMENSION(:,:) :: ff,bb
     REAL(fp), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
     REAL(fp) :: single_albedo,optical_depth,Planck_Func,total_opt

     ! internal variables
     REAL(fp) :: s, c, s_AD,c_AD,xx_AD
     INTEGER :: i,j
     INTEGER :: Error_Status

     ! Tangent-Linear Part
     REAL(fp), INTENT( INOUT ), DIMENSION( :,: ) :: trans_AD,refl_AD
     REAL(fp), INTENT( INOUT ), DIMENSION( : ) :: source_up_AD,source_down_AD
     REAL(fp), INTENT( INOUT ) :: single_albedo_AD
     REAL(fp), INTENT( INOUT ) :: optical_depth_AD,Planck_Func_AD,total_opt_AD
     REAL(fp), INTENT(INOUT), DIMENSION(:,:) :: ff_AD,bb_AD

     REAL(fp), DIMENSION(nZ) :: Exp_x_AD,EigVa_AD,EigValue_AD
     REAL(fp), DIMENSION(nZ,nZ) :: i_Gm_AD, Gm_AD, Gp_AD, EigVe_AD, EigVeF_AD, EigVeVa_AD
     REAL(fp), DIMENSION(nZ,nZ) :: A1_AD,A2_AD,A3_AD,A4_AD,A5_AD,A6_AD,Gm_A5_AD,i_Gm_A5_AD
     REAL(fp), DIMENSION(nZ,nZ) :: HH_AD,PM_AD,PP_AD,PPM_AD,i_PPM_AD,PPP_AD
    
     REAL(fp), DIMENSION(nZ) :: thermal_up_AD, thermal_down_AD
     REAL(fp) :: EXPfactor,Sfactor,s_transmittance,Solar(2*nZ),V0(2*nZ,2*nZ),Solar1(2*nZ)
     REAL(fp) :: V1(2*nZ,2*nZ),Sfac2,source_up(nZ),source_down(nZ) 
     REAL(fp) :: EXPfactor_AD,Sfactor_AD,s_transmittance_AD,Solar_AD(2*nZ),V0_AD(2*nZ,2*nZ),Solar1_AD(2*nZ)
     REAL(fp) :: V1_AD(2*nZ,2*nZ),Sfac2_AD
     REAL(fp), DIMENSION( nZ ) :: C1_AD,C2_AD,C1,C2
     REAL(fp), DIMENSION(nZ,nZ) :: trans, refl
     INTEGER :: N2, N2_1
     REAL(fp) :: Thermal_C_AD
     CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AMOM_layer_AD'
     CHARACTER(256) :: Message
          
     s_AD = ZERO
     c_AD = ZERO
     !
     ! for small layer optical depth, single scattering is applied.  
     IF( optical_depth < DELTA_OPTICAL_DEPTH ) THEN
       s = optical_depth * single_albedo
       DO i = 1, nZ         
         c = s/COS_Angle(i)
         IF( RTV%mth_Azi == 0 ) THEN
           source_up_AD(i) = source_up_AD(i) + source_down_AD(i)
           source_down_AD(i) = ZERO
           Planck_Func_AD = Planck_Func_AD + (ONE - RTV%Thermal_C(i,KL))*source_up_AD(i)
           Thermal_C_AD = -source_up_AD(i) * Planck_Func
         END IF

         DO j = 1, nZ   
           IF( RTV%mth_Azi == 0 ) THEN
             refl_AD(i,j) = refl_AD(i,j) + Thermal_C_AD
             trans_AD(i,j) = trans_AD(i,j) + Thermal_C_AD
           END IF
           IF( i == j ) THEN
             optical_depth_AD = optical_depth_AD - trans_AD(i,j)/COS_Angle(i)
           END IF
            
           c_AD = c_AD + trans_AD(i,j) * ff(i,j) * COS_Weight(j)
           ff_AD(i,j) = ff_AD(i,j) + c * trans_AD(i,j) * COS_Weight(j)
           c_AD = c_AD + refl_AD(i,j) * bb(i,j) * COS_Weight(j)
           bb_AD(i,j) = bb_AD(i,j) + c * refl_AD(i,j) * COS_Weight(j)
         ENDDO

         source_up_AD(i) = ZERO
         s_AD = s_AD + c_AD/COS_Angle(i)
         c_AD = ZERO
       ENDDO
       optical_depth_AD = optical_depth_AD + s_AD * single_albedo
       single_albedo_AD = single_albedo_AD + optical_depth * s_AD
       RETURN
     END IF

     trans(1:nZ,1:nZ) = RTV%s_Layer_Trans(1:nZ,1:nZ,KL)
     refl(1:nZ,1:nZ) = RTV%s_Layer_Refl(1:nZ,1:nZ,KL)

     thermal_up_AD(:) = source_up_AD(:)
     thermal_down_AD(:) = source_down_AD(:)

     IF( RTV%Visible_Flag_true ) THEN 
       N2 = 2 * nZ
       N2_1 = N2 - 1

       ! forward part  start   ********
       source_up = ZERO
       source_down = ZERO
       Solar_AD = ZERO
       Solar1_AD = ZERO
       Sfactor_AD = ZERO
       Sfac2_AD = ZERO
       EXPfactor_AD = ZERO
       s_transmittance_AD = ZERO
       V0_AD = ZERO
       V1_AD = ZERO
       !
       ! Solar source  
       Sfactor = single_albedo*RTV%Solar_irradiance/PI
       IF( RTV%mth_Azi == 0 ) Sfactor = Sfactor/TWO
       EXPfactor = exp(-optical_depth/RTV%COS_SUN)
       s_transmittance = exp(-total_opt/RTV%COS_SUN)
       
       DO i = 1, nZ     
         Solar(i) = -bb(i,nZ+1)*Sfactor
         Solar(i+nZ) = -ff(i,nZ+1)*Sfactor

         DO j = 1, nZ
           V0(i,j) = single_albedo * ff(i,j) * COS_Weight(j)
           V0(i+nZ,j) = single_albedo * bb(i,j) * COS_Weight(j)
           V0(i,j+nZ) = V0(i+nZ,j)
           V0(nZ+i,j+nZ) = V0(i,j)
         ENDDO
       V0(i,i) = V0(i,i) - ONE - COS_Angle(i)/RTV%COS_SUN
       V0(i+nZ,i+nZ) = V0(i+nZ,i+nZ) - ONE + COS_Angle(i)/RTV%COS_SUN
       ENDDO
       
       V1(1:N2_1,1:N2_1) = matinv(V0(1:N2_1,1:N2_1), Error_Status)
       IF( Error_Status /= SUCCESS  ) THEN
         WRITE( Message,'("Error in matrix inversion matinv(V0(1:N2_1,1:N2_1), Error_Status) ")' ) 
         CALL Display_Message( ROUTINE_NAME, &                                                    
                               TRIM(Message), &                                                   
                               Error_Status )                                          
         RETURN                                                                                    
       END IF               
       
       Solar1(1:N2_1) = matmul( V1(1:N2_1,1:N2_1), Solar(1:N2_1) )
       Solar1(N2) = ZERO
       Sfac2 = Solar(N2) - sum( V0(N2,1:N2_1)*Solar1(1:N2_1) )

       DO i = 1, nZ
         source_up(i) = Solar1(i)
         source_down(i) = EXPfactor*Solar1(i+nZ)
         DO j = 1, nZ
           source_up(i) =source_up(i)-refl(i,j)*Solar1(j+nZ)-trans(i,j)*EXPfactor*Solar1(j)
           source_down(i) =source_down(i) -trans(i,j)*Solar1(j+nZ) -refl(i,j)*EXPfactor*Solar1(j)
         END DO
       END DO
       ! specific treatment for downeward source function
       IF( abs( V0(N2,N2) ) > 0.0001_fp ) THEN
         source_down(nZ) =source_down(nZ) +(EXPfactor-trans(nZ,nZ))*Sfac2/V0(N2,N2)
       ELSE
         source_down(nZ) =source_down(nZ) -EXPfactor*Sfac2*optical_depth/COS_Angle(nZ)
       END IF
        
       ! forward part end  ********      
       !
       s_transmittance_AD = s_transmittance_AD+sum (source_down(1:nZ)*source_down_AD(1:nZ) )
       source_down_AD(1:nZ) = source_down_AD(1:nZ)*s_transmittance
       s_transmittance_AD = s_transmittance_AD + sum (source_up(1:nZ)*source_up_AD(1:nZ) )
       source_up_AD(1:nZ) = source_up_AD(1:nZ)*s_transmittance
       !
       ! specific treatment for downeward source function
       IF( abs( V0(N2,N2) ) > 0.0001_fp ) THEN
         V0_AD(N2,N2)=V0_AD(N2,N2)-(EXPfactor-trans(nZ,nZ))*Sfac2*source_down_AD(nZ)/V0(N2,N2)/V0(N2,N2)
         Sfac2_AD = Sfac2_AD+(EXPfactor-trans(nZ,nZ))*source_down_AD(nZ)/V0(N2,N2)
         EXPfactor_AD = EXPfactor_AD+source_down_AD(nZ)*Sfac2/V0(N2,N2)
         trans_AD(nZ,nZ) = trans_AD(nZ,nZ)-source_down_AD(nZ)*Sfac2/V0(N2,N2)
       ELSE
         optical_depth_AD = optical_depth_AD -EXPfactor*Sfac2*source_down_AD(nZ)/COS_Angle(nZ)
         Sfac2_AD = Sfac2_AD-EXPfactor*source_down_AD(nZ)*optical_depth/COS_Angle(nZ)
         EXPfactor_AD = EXPfactor_AD-source_down_AD(nZ)*Sfac2*optical_depth/COS_Angle(nZ)
       END IF

       DO i = nZ, 1, -1
         DO j = nZ, 1, -1
           Solar1_AD(j)=Solar1_AD(j)-refl(i,j)*EXPfactor*source_down_AD(i)
           EXPfactor_AD = EXPfactor_AD -refl(i,j)*source_down_AD(i)*Solar1(j)
           refl_AD(i,j) = refl_AD(i,j) -source_down_AD(i)*EXPfactor*Solar1(j)
           Solar1_AD(j+nZ) = Solar1_AD(j+nZ) -trans(i,j)*source_down_AD(i)
           trans_AD(i,j) = trans_AD(i,j) -source_down_AD(i)*Solar1(j+nZ)
           
           Solar1_AD(j)=Solar1_AD(j)-trans(i,j)*EXPfactor*source_up_AD(i)
           EXPfactor_AD = EXPfactor_AD - trans(i,j)*source_up_AD(i)*Solar1(j)
           trans_AD(i,j)=trans_AD(i,j) - source_up_AD(i)*EXPfactor*Solar1(j)
           Solar1_AD(j+nZ) = Solar1_AD(j+nZ) -refl(i,j)*source_up_AD(i)
           refl_AD(i,j) = refl_AD(i,j) -source_up_AD(i)*Solar1(j+nZ)
         END DO
         
         Solar1_AD(i+nZ) = Solar1_AD(i+nZ) + EXPfactor * source_down_AD(i)
         EXPfactor_AD = EXPfactor_AD + source_down_AD(i)*Solar1(i+nZ)
         Solar1_AD(i) = Solar1_AD(i) + source_up_AD(i)          
       END DO

       Solar1_AD(1:N2_1)=Solar1_AD(1:N2_1) -Sfac2_AD*V0(N2,1:N2_1)
       V0_AD(N2,1:N2_1)=V0_AD(N2,1:N2_1) -Sfac2_AD*Solar1(1:N2_1)
       Solar_AD(N2) = Solar_AD(N2) + Sfac2_AD

       Solar1_AD(N2) = ZERO
       Solar_AD(1:N2_1)=Solar_AD(1:N2_1)+matmul( transpose(V1(1:N2_1,1:N2_1)),Solar1_AD(1:N2_1) )
       Solar(1:N2_1) =  matmul( V1(1:N2_1,1:N2_1),Solar(1:N2_1) )                  
       Solar1_AD(1:N2_1) = -matmul( transpose(V1(1:N2_1,1:N2_1)),Solar1_AD(1:N2_1) ) 
       DO i = 1, N2_1
       DO j = 1, N2_1
         V0_AD(i,j)=V0_AD(i,j)+Solar1_AD(i)*Solar(j)
       END DO
       END DO        
        
       ! Solar source            
       DO i = nZ, 1, -1                
         DO j = nZ, 1, -1    
           V0_AD(i,j)=V0_AD(i,j) + V0_AD(nZ+i,j+nZ)
           V0_AD(i+nZ,j)=V0_AD(i+nZ,j) + V0_AD(i,j+nZ)
           bb_AD(i,j)=bb_AD(i,j) + single_albedo*V0_AD(i+nZ,j)*COS_Weight(j)
           single_albedo_AD=single_albedo_AD + V0_AD(i+nZ,j)*bb(i,j)*COS_Weight(j)
           ff_AD(i,j)=ff_AD(i,j) + single_albedo*V0_AD(i,j)*COS_Weight(j)
           single_albedo_AD=single_albedo_AD +V0_AD(i,j)*ff(i,j)*COS_Weight(j)
         ENDDO
       
         Sfactor_AD = Sfactor_AD -ff(i,nZ+1)*Solar_AD(i+nZ)
         ff_AD(i,nZ+1) = ff_AD(i,nZ+1) -Solar_AD(i+nZ)*Sfactor
         Sfactor_AD = Sfactor_AD -bb(i,nZ+1)*Solar_AD(i)
         bb_AD(i,nZ+1)=bb_AD(i,nZ+1) - Solar_AD(i)*Sfactor
       ENDDO
        
       total_opt_AD = total_opt_AD -s_transmittance_AD/RTV%COS_SUN*s_transmittance
       optical_depth_AD = optical_depth_AD -EXPfactor_AD/RTV%COS_SUN*EXPfactor
       
       IF( RTV%mth_Azi == 0 ) THEN
         Sfactor_AD = Sfactor_AD/TWO
       END IF
             
       single_albedo_AD = single_albedo_AD + Sfactor_AD*RTV%Solar_irradiance/PI
       
     END IF

    ! Thermal part
     IF( RTV%mth_Azi == 0 ) THEN
       DO i = nZ, 1, -1
         thermal_up_AD(i) = thermal_up_AD(i) + thermal_down_AD(i)
         thermal_down_AD(i) = ZERO
         Planck_Func_AD = Planck_Func_AD + ( ONE - RTV%Thermal_C(i,KL) ) * thermal_up_AD(i)
         Thermal_C_AD = -thermal_up_AD(i) * Planck_Func

         IF ( i == nZ .AND. nZ == (n_Streams+1) ) THEN
           trans_AD(nZ,nZ) = trans_AD(nZ,nZ) + Thermal_C_AD
         END IF
       
         DO j = n_Streams, 1, -1
           trans_AD(i,j) = trans_AD(i,j) + Thermal_C_AD
           refl_AD(i,j) = refl_AD(i,j) + Thermal_C_AD
         ENDDO
         thermal_up_AD(i) = ZERO
       ENDDO
     END IF
!     
     i_Gm_A5_AD = matmul( transpose(RTV%Gp(1:nZ,1:nZ,KL)-RTV%A6(1:nZ,1:nZ,KL)),refl_AD )
     Gp_AD(:,:) = matmul( refl_AD, transpose(RTV%i_Gm_A5(1:nZ,1:nZ,KL)) )       
     
     A6_AD = - GP_AD  
     i_Gm_A5_AD = i_Gm_A5_AD + matmul( transpose(RTV%A4(1:nZ,1:nZ,KL)-RTV%A3(1:nZ,1:nZ,KL)),trans_AD )
     A4_AD(:,:) = matmul( trans_AD, transpose(RTV%i_Gm_A5(1:nZ,1:nZ,KL)) )
     A3_AD = - A4_AD
     Gm_A5_AD = -matmul( transpose(RTV%i_Gm_A5(1:nZ,1:nZ,KL)) ,matmul( i_Gm_A5_AD,transpose(RTV%i_Gm_A5(1:nZ,1:nZ,KL)) ) )       
     Gm_AD = Gm_A5_AD
     A5_AD = - Gm_A5_AD

     A4_AD = A4_AD + matmul( A6_AD(:,:), transpose(RTV%A2(1:nZ,1:nZ,KL)) )
     A2_AD = matmul( transpose(RTV%A4(1:nZ,1:nZ,KL)),A6_AD(:,:) )

     A1_AD = matmul( A5_AD(:,:), transpose(RTV%A2(1:nZ,1:nZ,KL)) )
     A2_AD = A2_AD + matmul( transpose(RTV%A1(1:nZ,1:nZ,KL)), A5_AD(:,:) )

     Gp_AD = Gp_AD + matmul( A3_AD(:,:), transpose(RTV%A2(1:nZ,1:nZ,KL)) )
     A2_AD = A2_AD + matmul( transpose(RTV%Gp(1:nZ,1:nZ,KL)),A3_AD(:,:) )

     i_Gm_AD = matmul( A2_AD(:,:), transpose(RTV%A1(1:nZ,1:nZ,KL)) )
     A1_AD = A1_AD + matmul( transpose(RTV%i_Gm(1:nZ,1:nZ,KL)), A2_AD(:,:) )

     Exp_x_AD = ZERO
     
     DO i = nZ, 1, -1
       DO j = nZ, 1, -1
         Gm_AD(i,j) = Gm_AD(i,j) + A4_AD(i,j)* RTV%Exp_x(j,KL)
         Exp_x_AD(j) = Exp_x_AD(j) + RTV%Gm(i,j,KL)*A4_AD(i,j)
         Gp_AD(i,j) = Gp_AD(i,j) + A1_AD(i,j)* RTV%Exp_x(j,KL)
         Exp_x_AD(j) = Exp_x_AD(j) + RTV%Gp(i,j,KL)*A1_AD(i,j)
       END DO
     END DO
    
     DO i = nZ, 1, -1
       xx_AD = -Exp_x_AD(i)*RTV%Exp_x(i,KL)
       Exp_x_AD(i) = ZERO
       EigValue_AD(i) = xx_AD*optical_depth
       optical_depth_AD = optical_depth_AD + RTV%EigValue(i,KL)*xx_AD
     END DO

     Gm_AD = Gm_AD -matmul( transpose(RTV%i_Gm(1:nZ,1:nZ,KL)), matmul( i_Gm_AD, transpose(RTV%i_Gm(1:nZ,1:nZ,KL)) ) )
 
     EigVe_AD(:,:) = Gm_AD(:,:)/2.0_fp           
     EigVeF_AD(:,:) = - Gm_AD(:,:)/2.0_fp         

     EigVe_AD = EigVe_AD + Gp_AD(:,:)/2.0_fp
     EigVeF_AD = EigVeF_AD + Gp_AD(:,:)/2.0_fp

     i_PPM_AD(:,:) = matmul( EigVeF_AD(:,:), transpose(RTV%EigVeVa(1:nZ,1:nZ,KL)) )
     EigVeVa_AD(:,:) = matmul( transpose(RTV%i_PPM(1:nZ,1:nZ,KL)), EigVeF_AD(:,:) )           

     DO i = nZ, 1, -1
       DO j = nZ, 1, -1              
         EigVe_AD(i,j)=EigVe_AD(i,j)+EigVeVa_AD(i,j)* RTV%EigValue(j,KL)
         EigValue_AD(j) = EigValue_AD(j)+RTV%EigVe(i,j,KL)*EigVeVa_AD(i,j)
       END DO
     END DO
    
     DO i = nZ, 1, -1
       IF( RTV%EigVa(i,KL) > ZERO ) THEN
         EigVa_AD(i) = 0.5_fp*EigValue_AD(i)/RTV%EigValue(i,KL)
       ELSE
         EigValue_AD(i) = ZERO
         EigVa_AD(i) = ZERO 
       END IF
     END DO

     ! compute eigenvectors EigVe, and eigenvalues EigVa
     CALL ASYMTX_AD(COS_Angle(nZ),n_Streams,nZ,RTV%EigVe(1:nZ,1:nZ,KL),RTV%EigVa(1:nZ,KL), &
          EigVe_AD,EigVa_AD,HH_AD,Error_Status) 

     PPM_AD(:,:) = matmul( HH_AD(:,:), transpose(RTV%PPP(1:nZ,1:nZ,KL)) )
     PPP_AD(:,:) = matmul( transpose(RTV%PPM(1:nZ,1:nZ,KL)), HH_AD(:,:) )

     PP_AD = PPP_AD
     PM_AD = PPP_AD
   
     PPM_AD(:,:) = PPM_AD(:,:)-matmul( transpose(RTV%i_PPM(1:nZ,1:nZ,KL)),matmul(i_PPM_AD(:,:),transpose(RTV%i_PPM(1:nZ,1:nZ,KL))) )

     PP_AD = PP_AD + PPM_AD
     PM_AD = PM_AD - PPM_AD

     IF( single_albedo < max_albedo ) THEN
       s = single_albedo
     ELSE
       s = max_albedo
     END IF 
     
       c_AD = ZERO
       s_AD = ZERO
     DO i = nZ, 1, -1
       c = s/COS_Angle(i) 
       DO j = nZ, 1, -1
       c_AD = c_AD + PP_AD(i,j) * ff(i,j) * COS_Weight(j)
       ff_AD(i,j) = ff_AD(i,j) + c * PP_AD(i,j) * COS_Weight(j)
       c_AD = c_AD + PM_AD(i,j) * bb(i,j) * COS_Weight(j)
       bb_AD(i,j) = bb_AD(i,j) + c * PM_AD(i,j) * COS_Weight(j)
       END DO
       s_AD = s_AD + c_AD/COS_Angle(i)
       c_AD = ZERO
     ENDDO
!
     IF( single_albedo < max_albedo ) THEN
       s = single_albedo
       single_albedo_AD = s_AD + single_albedo_AD       
     ELSE
       s = max_albedo
       s_AD = 0.0_fp
     END IF
!       
     RETURN

     END SUBROUTINE CRTM_AMOM_layer_AD


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Phase_Matrix
!
! PURPOSE:
!       Subroutine to calculate the phase function for the scattering model.
!
! CALLING SEQUENCE:
!       CALL CRTM_Phase_Matrix( AtmOptics,  &  ! Input
!                               RTV         )  ! Internal variable
!
! INPUT ARGUMENTS:
!       AtmOptics:      Structure containing the atmospheric optical
!                       parameters
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       RTV:            Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       RTV_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       RTV:            Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       RTV_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note that the INTENT on the RTV         argument is IN OUT as it 
!       contains data prior to this call and is filled with data within this
!       routine.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Phase_Matrix( &
    AtmOptics, &  ! Input
    RTV        )  ! Internal variable
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN)     :: AtmOptics
    TYPE(RTV_type),            INTENT(IN OUT) :: RTV
    ! Local variables
    INTEGER :: i, j, k, l, ifac, jn



    !#--------------------------------------------------------------------------#
    !#                   -- COMPUTING LEGENDRE FUNCTION --                      #
    !#                   Pplus  for positive cosine of angles                   #
    !#                   Pminus for negative cosine of angles                   #
    !#--------------------------------------------------------------------------#

    DO i = 1, RTV%n_Angles 
       CALL Legendre_M( RTV%mth_Azi               , &
                        AtmOptics%n_Legendre_Terms, &
                        RTV%COS_Angle(i)          , &
                        RTV%Pleg(0:,i)              )
    END DO

    IF(RTV%Solar_Flag_true) &
      CALL Legendre_M( RTV%mth_Azi                , &
                       AtmOptics%n_Legendre_Terms , &
                       RTV%COS_SUN                , &
                       RTV%Pleg(0:,RTV%n_Angles+1)  )

    IF( RTV%Solar_Flag_true ) THEN
      jn = RTV%n_Angles + 1
    ELSE
      jn = RTV%n_Angles
    END IF


    !#--------------------------------------------------------------------------#
    !#                    -- COMPUTE THE PHASE MATRICES --                      #
    !#--------------------------------------------------------------------------#

    Layer_Loop: DO  k = 1, RTV%n_Layers


      ! ------------------------------    
      ! Only proceed if the scattering    
      ! coefficient is significant        
      ! ------------------------------    

      Significant_Scattering: IF( AtmOptics%Single_Scatter_Albedo(k) > SCATTERING_ALBEDO_THRESHOLD) THEN
      
        DO j = 1, jn
          ! add solar angle
          DO i = 1, RTV%n_Angles

            RTV%Off(i,j,k)=ZERO
            RTV%Obb(i,j,k)=ZERO

            DO l = RTV%mth_Azi, AtmOptics%n_Legendre_Terms - 1
              ifac = (-1) ** (l - RTV%mth_Azi)
              RTV%Off(i,j,k) = RTV%Off(i,j,k) + ( AtmOptics%Phase_Coefficient(l,1,k)*RTV%Pleg(l,i)*RTV%Pleg(l,j) )
              RTV%Obb(i,j,k) = RTV%Obb(i,j,k) + ( AtmOptics%Phase_Coefficient(l,1,k)*RTV%Pleg(l,i)*RTV%Pleg(l,j)*ifac )
            END DO

            RTV%Pff(i,j,k) = RTV%Off(i,j,k)
            RTV%Pbb(i,j,k) = RTV%Obb(i,j,k)

            ! For intensity, the phase matrix element must >= ZERO   
            IF ( RTV%mth_Azi == 0 ) THEN          
              IF(RTV%Pff(i,j,k) < ZERO) RTV%Pff(i,j,k) = PHASE_THRESHOLD
              IF(RTV%Pbb(i,j,k) < ZERO) RTV%Pbb(i,j,k) = PHASE_THRESHOLD
            END IF

          END DO
        END DO

        ! Normalization to ensure energy conservation
        IF (  RTV%mth_Azi == 0 ) CALL Normalize_Phase( k, RTV )
             
      END IF Significant_Scattering

    END DO Layer_Loop

  END SUBROUTINE CRTM_Phase_Matrix


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Phase_Matrix_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear phase function for the
!       scattering model.
!
! CALLING SEQUENCE:                                                        
!       CALL CRTM_Phase_Matrix_TL( AtmOptics,    &  ! FWD Input
!                                  AtmOptics_TL, &  ! TL  Input
!                                  Pff_TL,       &  ! TL Output
!                                  Pff_TL,       &  ! TL Output
!                                  RTV           )  ! Internal variable
!
! INPUT ARGUMENTS:                                                         
!       AtmOptics:      Structure containing the atmospheric optical         
!                       parameters                                         
!                       UNITS:      N/A                                    
!                       TYPE:       CRTM_AtmOptics_type                     
!                       DIMENSION:  Scalar                                 
!                       ATTRIBUTES: INTENT(IN)                             
!
!       AtmOptics_TL:   Structure containing the tangent-linear atmospheric  
!                       optical parameters                                 
!                       UNITS:      N/A                                    
!                       TYPE:       CRTM_AtmOptics_type                     
!                       DIMENSION:  Scalar                                 
!                       ATTRIBUTES: INTENT(IN)                             
!
!       RTV:            Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.               
!                       UNITS:      N/A                                    
!                       TYPE:       RTV_type                    
!                       DIMENSION:  Scalar                                 
!                       ATTRIBUTES: INTENT(IN)                             
!
! OUTPUT ARGUMENTS:
!       Pff_TL:         Array containing the tangent-linear of the 
!                       forward phase matrix.
!                       UNITS:      N/A
!                       TYPE:       REAL
!                       DIMENSION:  Rank-3, n_Angles x n_Angles x n_Layers
!                       ATTRIBUTES: INTENT(OUT)
!
!       Pbb_TL:         Array containing the tangent-linear of the 
!                       backward phase matrix.
!                       UNITS:      N/A
!                       TYPE:       REAL
!                       DIMENSION:  Rank-3, n_Angles x n_Angles x n_Layers
!                       ATTRIBUTES: INTENT(OUT)
!
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Phase_Matrix_TL( &
    AtmOptics,    &  ! FWD Input
    AtmOptics_TL, &  ! TL  Input
    Pff_TL,       &  ! TL Output
    Pbb_TL,       &  ! TL Output
    RTV           )  ! Internal variable
    ! Arguments
    TYPE(CRTM_AtmOptics_type)  , INTENT(IN)  :: AtmOptics
    TYPE(CRTM_AtmOptics_type)  , INTENT(IN)  :: AtmOptics_TL
    REAL(fp)                   , INTENT(OUT) :: Pff_TL(:,:,:) ! n_Angles x n_Angles x n_Layers
    REAL(fp)                   , INTENT(OUT) :: Pbb_TL(:,:,:) ! n_Angles x n_Angles x n_Layers
    TYPE(RTV_type)             , INTENT(IN)  :: RTV
    ! Local variables
    INTEGER :: i, j, k, l, nZ, ifac, jn
    REAL(fp), DIMENSION(RTV%n_Angles,RTV%n_Angles+1) :: Lff, Lbb



    !#--------------------------------------------------------------------------#
    !#             -- COMPUTE THE TANGENT-LINEAR PHASE MATRICES --              #
    !#--------------------------------------------------------------------------#

    nZ = RTV%n_Angles
    IF( RTV%Solar_Flag_true ) THEN
      jn = RTV%n_Angles + 1
    ELSE
      jn = RTV%n_Angles
    END IF

    Layer_Loop: DO  k = 1, RTV%n_Layers
        
      ! ------------------------------    
      ! Only proceed if the scattering    
      ! coefficient is significant        
      ! ------------------------------    

      Significant_Scattering: IF( AtmOptics%Single_Scatter_Albedo(k) > SCATTERING_ALBEDO_THRESHOLD) THEN
                 
        Lff(1:nZ,1:jn) = RTV%Off(1:nZ,1:jn,k)
        Lbb(1:nZ,1:jn) = RTV%Obb(1:nZ,1:jn,k)


        DO j = 1, jn
          DO i = 1, RTV%n_Angles

            Pff_TL(i,j,k) = ZERO
            Pbb_TL(i,j,k) = ZERO    
                
            DO l = RTV%mth_Azi, AtmOptics%n_Legendre_Terms - 1
              ifac = (-1) ** (l - RTV%mth_Azi)
              Pff_TL(i,j,k) = Pff_TL(i,j,k) + ( AtmOptics_TL%Phase_Coefficient(l,1,k)*RTV%Pleg(l,i)*RTV%Pleg(l,j) )
              Pbb_TL(i,j,k) = Pbb_TL(i,j,k) + ( AtmOptics_TL%Phase_Coefficient(l,1,k)*RTV%Pleg(l,i)*RTV%Pleg(l,j)*ifac )
            END DO
              
            ! For intensity, the FWD phase matrix element must >= ZERO
            ! so the TL form is always == zero.
            IF ( RTV%mth_Azi == 0 ) THEN
              IF ( RTV%Off(i,j,k) < ZERO ) THEN
                Pff_TL(i,j,k) = ZERO
                Lff(i,j)      = PHASE_THRESHOLD 
              END IF

              IF ( RTV%Obb(i,j,k) < ZERO ) THEN
                Pbb_TL(i,j,k) = ZERO
                Lbb(i,j) = PHASE_THRESHOLD 
              END IF
            END IF
             
          END DO
        END DO

        IF ( RTV%mth_Azi == 0 ) THEN
          ! Normalisation for energy conservation
          CALL Normalize_Phase_TL( &
                 k, RTV, &
                 Lff,           & ! FWD Input
                 Lbb,           & ! FWD Input
                 Pff_TL(:,:,k), & ! TL  Output
                 Pbb_TL(:,:,k)  ) ! TL  Output
        END IF
      END IF Significant_Scattering
    
    END DO Layer_Loop

  END SUBROUTINE CRTM_Phase_Matrix_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Phase_Matrix_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint of the phase function for the
!       scattering model.
!
! CALLING SEQUENCE:
!       CALL CRTM_Phase_Matrix_AD( AtmOptics,    &  ! FWD Input
!                                  Pff_AD,       &  ! AD Input
!                                  Pbb_AD,       &  ! AD Input
!                                  AtmOptics_AD, &  ! AD Output
!                                  RTV           )  ! Internal variable
!
! INPUT ARGUMENTS:
!       AtmOptics:      Structure containing the atmospheric optical
!                       parameters
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Pff_AD:         Array containing the adjoint of the 
!                       forward phase matrix.
!                       ** NOTE: This argument will be zeroed upon exit
!                       **       from this routine.
!                       UNITS:      N/A
!                       TYPE:       REAL
!                       DIMENSION:  Rank-3, n_Angles x n_Angles x n_Layers
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       Pbb_AD:         Array containing the adjoint of the 
!                       backward phase matrix.
!                       ** NOTE: This argument will be zeroed upon exit
!                       **       from this routine.
!                       UNITS:      N/A
!                       TYPE:       REAL
!                       DIMENSION:  Rank-3, n_Angles x n_Angles x n_Layers
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       RTV:            Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       RTV_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AtmOptics_AD:   Structure containing the adjoint atmospheric optical
!                       parameters
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output AtmOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument MUST be defined upon
!       input. To prevent memory leaks, and in this case errors in accessing
!       unallocated memory, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Phase_Matrix_AD( &
    AtmOptics,    &  ! FWD Input
    Pff_AD,       &  ! AD Input
    Pbb_AD,       &  ! AD Input
    AtmOptics_AD, &  ! AD Output
    RTV           )  ! Internal variable
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN)     :: AtmOptics
    REAL(fp)                 , INTENT(IN OUT) :: Pff_AD(:,:,:) ! n_Angles x n_Angles x n_Layers
    REAL(fp)                 , INTENT(IN OUT) :: Pbb_AD(:,:,:) ! n_Angles x n_Angles x n_Layers
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: AtmOptics_AD
    TYPE(RTV_type)           , INTENT(IN)     :: RTV
    ! Local variables
    INTEGER :: i, j, k, l, nZ, ifac, jn
    REAL(fp), DIMENSION(RTV%n_Angles,RTV%n_Angles+1) :: Lff, Lbb



    !#--------------------------------------------------------------------------#
    !#             -- COMPUTE THE ADJOINT OF THE PHASE MATRICES --              #
    !#--------------------------------------------------------------------------#

    nZ = RTV%n_Angles
    IF( RTV%Solar_Flag_true ) THEN
      jn = RTV%n_Angles + 1
    ELSE
      jn = RTV%n_Angles
    END IF
        
    Layer_Loop: DO  k = 1, RTV%n_Layers
    
    
      ! ------------------------------    
      ! Only proceed if the scattering    
      ! coefficient is significant        
      ! ------------------------------    

      Significant_Scattering: IF( AtmOptics%Single_Scatter_Albedo(k) > SCATTERING_ALBEDO_THRESHOLD) THEN


        ! AD normalization to ensure energy conservation
     !!   AtmOptics_AD%Phase_Coefficient(0:,1,k) = ZERO
        
        Lff(1:nZ,1:jn) = RTV%Off(1:nZ,1:jn,k)
        Lbb(1:nZ,1:jn) = RTV%Obb(1:nZ,1:jn,k)

        IF ( RTV%mth_Azi == 0 ) THEN              
          DO j = 1, jn
            DO i = 1, RTV%n_Angles
           
              ! For intensity, the FWD phase matrix element must >= ZERO
              ! so the TL, and thus the AD, for is always == zero.

                IF ( RTV%Off(i,j,k) < ZERO) Lff(i,j) = PHASE_THRESHOLD
                IF ( RTV%Obb(i,j,k) < ZERO) Lbb(i,j) = PHASE_THRESHOLD
             
            END DO
          END DO

          CALL Normalize_Phase_AD( &
                 k, RTV, &
                 Lff, Lbb,      & ! FWD Input
                 Pff_AD(:,:,k), & ! AD  Output
                 Pbb_AD(:,:,k)  ) ! AD  Output
        END IF

        DO j = 1, jn
          DO i = 1, RTV%n_Angles
         
            ! For intensity, the FWD phase matrix element must >= ZERO
            ! so the TL, and thus the AD, for is always == zero.
            IF ( RTV%mth_Azi == 0 ) THEN
              IF ( RTV%Off(i,j,k) < ZERO) Pff_AD(i,j,k) = ZERO
              IF ( RTV%Obb(i,j,k) < ZERO) Pbb_AD(i,j,k) = ZERO
            END IF

            DO l = RTV%mth_Azi, AtmOptics%n_Legendre_Terms - 1
              ifac = (-1) ** (l - RTV%mth_Azi)
              AtmOptics_AD%Phase_Coefficient(l,1,k) = AtmOptics_AD%Phase_Coefficient(l,1,k) + &
                                                      ( Pff_AD(i,j,k)*RTV%Pleg(l,i)*RTV%Pleg(l,j) )
              AtmOptics_AD%Phase_Coefficient(l,1,k) = AtmOptics_AD%Phase_Coefficient(l,1,k) + &
                                                      ( Pbb_AD(i,j,k)*RTV%Pleg(l,i)*RTV%Pleg(l,j)*ifac ) 
            END DO

            Pff_AD(i,j,k) = ZERO
            Pbb_AD(i,j,k) = ZERO           

          END DO
        END DO

      END IF Significant_Scattering

    END DO Layer_Loop

  END SUBROUTINE CRTM_Phase_Matrix_AD


  SUBROUTINE Normalize_Phase( k, RTV )
    ! Arguments
    INTEGER,        INTENT(IN)     :: k
    TYPE(RTV_type), INTENT(IN OUT) :: RTV
    ! Local variables
    INTEGER :: i, j, nZ                                           

    nZ = RTV%n_Angles

    ! Normalisation for stream angles
    RTV%Sum_Fac(0,k)=ZERO
    DO i = 1, RTV%n_Streams
      RTV%n_Factor(i,k)=ZERO
      DO j = i,nZ
        RTV%n_Factor(i,k)=RTV%n_Factor(i,k)+(RTV%Pff(i,j,k)+RTV%Pbb(i,j,k))*RTV%COS_Weight(j)
      END DO
      DO j=i,nZ
        RTV%Pff(i,j,k)=RTV%Pff(i,j,k)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k))
        RTV%Pbb(i,j,k)=RTV%Pbb(i,j,k)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k))
      END DO
      RTV%Sum_Fac(i,k)=ZERO
      IF( i < nZ ) THEN
        DO j=i+1,nZ
          RTV%Pff(j,i,k)=RTV%Pff(i,j,k)
          RTV%Pbb(j,i,k)=RTV%Pbb(i,j,k)
        END DO
        DO j = 1, i
          RTV%Sum_Fac(i,k)=RTV%Sum_Fac(i,k) + (RTV%Pff(i+1,j,k)+RTV%Pbb(i+1,j,k))*RTV%COS_Weight(j)
        END DO
      END IF
    END DO

    IF( RTV%n_Streams < nZ ) THEN 
      ! Sensor viewing angle differs from the Gaussian angles
      RTV%n_Factor(nZ,k) =  RTV%Sum_Fac(nZ-1,k)
      DO j = 1, nZ
        RTV%Pff(j,nZ,k) = RTV%Pff(j,nZ,k)/RTV%n_Factor(nZ,k)
        RTV%Pbb(j,nZ,k) = RTV%Pbb(j,nZ,k)/RTV%n_Factor(nZ,k)
        ! Symmetric condition
        IF( j < nZ ) THEN
          RTV%Pff(nZ,j,k) = RTV%Pff(j,nZ,k)
          RTV%Pbb(nZ,j,k) = RTV%Pbb(j,nZ,k)
        END IF
      END DO      
    END IF

  END SUBROUTINE Normalize_Phase


  SUBROUTINE Normalize_Phase_TL( k, RTV, Pff, Pbb, Pff_TL, Pbb_TL )
    ! Arguments
    INTEGER       , INTENT(IN)     :: k
    TYPE(RTV_type), INTENT(IN)     :: RTV
    REAL(fp)      , INTENT(IN)     :: Pff(:,:)
    REAL(fp)      , INTENT(IN)     :: Pbb(:,:)
    REAL(fp)      , INTENT(IN OUT) :: Pff_TL(:,:)
    REAL(fp)      , INTENT(IN OUT) :: Pbb_TL(:,:)
    ! Local variables
    REAL(fp) :: n_Factor_TL
    REAL(fp) :: Sum_Fac_TL(0:RTV%n_Angles)
    INTEGER :: i, j, nZ                                           

    nZ = RTV%n_Angles

    ! Normalisation for stream angles
    Sum_Fac_TL(0) = ZERO
    DO i = 1, RTV%n_Streams
      n_Factor_TL = ZERO
      DO j = i,nZ
        n_Factor_TL=n_Factor_TL+(Pff_TL(i,j)+Pbb_TL(i,j))*RTV%COS_Weight(j)
      END DO
      DO j=i,nZ
        Pff_TL(i,j) = Pff_TL(i,j)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k)) - &
                      Pff(i,j)/RTV%n_Factor(i,k)/RTV%n_Factor(i,k)*n_Factor_TL*(ONE-RTV%Sum_Fac(i-1,k)) - &
                      Pff(i,j)/RTV%n_Factor(i,k)*Sum_Fac_TL(i-1)
        
        Pbb_TL(i,j) = Pbb_TL(i,j)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k)) - &
                      Pbb(i,j)/RTV%n_Factor(i,k)/RTV%n_Factor(i,k)*n_Factor_TL*(ONE-RTV%Sum_Fac(i-1,k)) - &
                      Pbb(i,j)/RTV%n_Factor(i,k)*Sum_Fac_TL(i-1)
      END DO
      Sum_Fac_TL(i)=ZERO
      ! Symmetric condition
      IF( i < nZ ) THEN
        DO j = i+1, nZ
          Pff_TL(j,i) = Pff_TL(i,j)
          Pbb_TL(j,i) = Pbb_TL(i,j)
        END DO
        DO j = 1, i
          Sum_Fac_TL(i)=Sum_Fac_TL(i) + (Pff_TL(j,i+1)+Pbb_TL(j,i+1))*RTV%COS_Weight(j)
        END DO
     END IF
    END DO

    IF( RTV%n_Streams < nZ ) THEN 
      ! Sensor viewing angle differs from the Gaussian angles
      n_Factor_TL = Sum_Fac_TL(nZ-1)
      DO j = 1, nZ
        Pff_TL(j,nZ) = Pff_TL(j,nZ)/RTV%n_Factor(nZ,k) - &
                       Pff(j,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*n_Factor_TL
                     
        Pbb_TL(j,nZ) = Pbb_TL(j,nZ)/RTV%n_Factor(nZ,k) - &
                       Pbb(j,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*n_Factor_TL             
        ! Symmetric condition
        IF( j < nZ ) THEN
          Pff_TL(nZ,j) = Pff_TL(j,nZ)
          Pbb_TL(nZ,j) = Pbb_TL(j,nZ)
        END IF
      END DO
    END IF

  END SUBROUTINE Normalize_Phase_TL


  SUBROUTINE Normalize_Phase_AD( k, RTV, Pff, Pbb, Pff_AD, Pbb_AD )
    ! Arguments
    INTEGER       , INTENT(IN)     :: k
    TYPE(RTV_type), INTENT(IN)     :: RTV
    REAL(fp)      , INTENT(IN)     :: Pff(:,:)
    REAL(fp)      , INTENT(IN)     :: Pbb(:,:)
    REAL(fp)      , INTENT(IN OUT) :: Pff_AD(:,:)
    REAL(fp)      , INTENT(IN OUT) :: Pbb_AD(:,:)
    ! Local variables
    INTEGER :: i, j, nZ                                           
    REAL(fp) :: n_Factor_AD
    REAL(fp) :: Sum_Fac_AD(0:RTV%n_Angles)


    nZ = RTV%n_Angles
    Sum_Fac_AD = ZERO

    n_Factor_AD = ZERO
    IF( RTV%n_Streams < nZ ) THEN 
      ! Sensor viewing angle diffs from the Gaussian angles
      DO j = nZ, 1, -1
        ! Symmetric condition
        IF( j < nZ ) THEN
          Pff_AD(j,nZ) = Pff_AD(j,nZ) + Pff_AD(nZ,j)
          Pff_AD(nZ,j) = ZERO
          Pbb_AD(j,nZ) = Pbb_AD(j,nZ) + Pbb_AD(nZ,j)
          Pbb_AD(nZ,j) = ZERO
        END IF

        n_Factor_AD = n_Factor_AD - Pff(j,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*Pff_AD(j,nZ)
        Pff_AD(j,nZ) = Pff_AD(j,nZ)/RTV%n_Factor(nZ,k)

        n_Factor_AD = n_Factor_AD - Pbb(j,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*Pbb_AD(j,nZ)
        Pbb_AD(j,nZ) = Pbb_AD(j,nZ)/RTV%n_Factor(nZ,k)             
      END DO   
      Sum_Fac_AD(nZ-1) = n_Factor_AD      
      n_Factor_AD = ZERO
    END IF
    
    DO i = RTV%n_Streams, 1, -1 
      ! Symmetric condition
      IF( i < nZ ) THEN
        DO j = i, 1, -1       
          Pbb_AD(j,i+1) = Pbb_AD(j,i+1) + Sum_Fac_AD(i)*RTV%COS_Weight(j)
          Pff_AD(j,i+1) = Pff_AD(j,i+1) + Sum_Fac_AD(i)*RTV%COS_Weight(j)        
        END DO
        DO j = nZ,i+1,-1
          Pff_AD(i,j) = Pff_AD(i,j) + Pff_AD(j,i)
          Pff_AD(j,i) = ZERO
          Pbb_AD(i,j) = Pbb_AD(i,j) + Pbb_AD(j,i)
          Pbb_AD(j,i) = ZERO
        END DO
      END IF
      Sum_Fac_AD(i) = ZERO
      DO j = nZ, i, -1
        Sum_Fac_AD(i-1) = Sum_Fac_AD(i-1) - Pbb(i,j)/RTV%n_Factor(i,k)*Pbb_AD(i,j)
        n_Factor_AD = n_Factor_AD -Pbb(i,j)/RTV%n_Factor(i,k)/RTV%n_Factor(i,k) * &
                      Pbb_AD(i,j)*(ONE-RTV%Sum_Fac(i-1,k))
        Pbb_AD(i,j) = Pbb_AD(i,j)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k))
        
        Sum_Fac_AD(i-1) = Sum_Fac_AD(i-1) - Pff(i,j)/RTV%n_Factor(i,k)*Pff_AD(i,j)
        n_Factor_AD = n_Factor_AD -Pff(i,j)/RTV%n_Factor(i,k)/RTV%n_Factor(i,k) * &
                      Pff_AD(i,j)*(ONE-RTV%Sum_Fac(i-1,k))
        Pff_AD(i,j) = Pff_AD(i,j)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k))
      END DO
      DO j = nZ, i, -1
        Pbb_AD(i,j) = Pbb_AD(i,j) + n_Factor_AD*RTV%COS_Weight(j)
        Pff_AD(i,j) = Pff_AD(i,j) + n_Factor_AD*RTV%COS_Weight(j)
      END DO
      n_Factor_AD = ZERO
    END DO    
    Sum_Fac_AD(0) = ZERO
  END SUBROUTINE Normalize_Phase_AD

END MODULE CRTM_RTSolution
