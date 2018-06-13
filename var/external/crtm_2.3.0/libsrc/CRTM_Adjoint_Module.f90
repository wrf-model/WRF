!
! CRTM_Adjoint_Module
!
! Module containing the CRTM adjoint model function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Jan-2005
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Adjoint_Module


  ! ------------
  ! Module usage
  ! ------------
  USE Type_Kinds,                 ONLY: fp
  USE Message_Handler,            ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters,            ONLY: SET, NOT_SET, ZERO, ONE, &
                                        MAX_N_LAYERS           , &
                                        MAX_N_PHASE_ELEMENTS   , &
                                        MAX_N_LEGENDRE_TERMS   , &
                                        MAX_N_STOKES           , &
                                        MAX_N_ANGLES           , &
                                        MAX_N_AZIMUTH_FOURIER  , &
                                        MAX_SOURCE_ZENITH_ANGLE, &
                                        MAX_N_STREAMS          , &
                                        SCATTERING_ALBEDO_THRESHOLD
  USE CRTM_SpcCoeff,              ONLY: SC, &
                                        SpcCoeff_IsInfraredSensor , &
                                        SpcCoeff_IsMicrowaveSensor, &
                                        SpcCoeff_IsVisibleSensor
  USE CRTM_Atmosphere_Define,     ONLY: CRTM_Atmosphere_type           , &
                                        CRTM_Atmosphere_Destroy        , &
                                        CRTM_Atmosphere_IsValid        , &
                                        CRTM_Atmosphere_Zero           , &
                                        CRTM_Atmosphere_AddLayerCopy   , &
                                        CRTM_Atmosphere_NonVariableCopy, &
                                        CRTM_Get_PressureLevelIdx
  USE CRTM_Surface_Define,        ONLY: CRTM_Surface_type           , &
                                        CRTM_Surface_IsValid        , &
                                        CRTM_Surface_NonVariableCopy
  USE CRTM_Geometry_Define,       ONLY: CRTM_Geometry_type, &
                                        CRTM_Geometry_IsValid
  USE CRTM_ChannelInfo_Define,    ONLY: CRTM_ChannelInfo_type, &
                                        CRTM_ChannelInfo_n_Channels
  USE CRTM_RTSolution_Define,     ONLY: CRTM_RTSolution_type, &
                                        CRTM_RTSolution_Destroy, &
                                        CRTM_RTSolution_Zero
  USE CRTM_Options_Define,        ONLY: CRTM_Options_type, &
                                        CRTM_Options_IsValid
  USE CRTM_Atmosphere,            ONLY: CRTM_Atmosphere_AddLayers      , &
                                        CRTM_Atmosphere_AddLayers_AD   , &
                                        CRTM_Atmosphere_IsFractional   , &
                                        CRTM_Atmosphere_Coverage       , &
                                        CRTM_Atmosphere_ClearSkyCopy   , &
                                        CRTM_Atmosphere_ClearSkyCopy_AD
  USE CRTM_GeometryInfo_Define,   ONLY: CRTM_GeometryInfo_type, &
                                        CRTM_GeometryInfo_SetValue, &
                                        CRTM_GeometryInfo_GetValue
  USE CRTM_GeometryInfo,          ONLY: CRTM_GeometryInfo_Compute
  USE CRTM_Predictor_Define,      ONLY: CRTM_Predictor_type      , &
                                        CRTM_Predictor_Associated, &
                                        CRTM_Predictor_Destroy   , &
                                        CRTM_Predictor_Create
  USE CRTM_Predictor,             ONLY: CRTM_PVar_type => iVar_type, &
                                        CRTM_Compute_Predictors    , &
                                        CRTM_Compute_Predictors_AD
  USE CRTM_AtmAbsorption,         ONLY: CRTM_AAVar_type => iVar_type , &
                                        CRTM_Compute_AtmAbsorption   , &
                                        CRTM_Compute_AtmAbsorption_AD
  USE CRTM_AtmOptics_Define,      ONLY: CRTM_AtmOptics_type      , &
                                        CRTM_AtmOptics_Associated, &
                                        CRTM_AtmOptics_Create    , &
                                        CRTM_AtmOptics_Destroy   , &
                                        CRTM_AtmOptics_Zero
  USE CRTM_AerosolScatter,        ONLY: CRTM_Compute_AerosolScatter   , &
                                        CRTM_Compute_AerosolScatter_AD
  USE CRTM_CloudScatter,          ONLY: CRTM_Compute_CloudScatter   , &
                                        CRTM_Compute_CloudScatter_AD
  USE CRTM_AtmOptics,             ONLY: CRTM_Include_Scattering        , &
                                        CRTM_Compute_Transmittance     , &
                                        CRTM_Compute_Transmittance_AD  , &
                                        CRTM_AtmOptics_Combine         , &
                                        CRTM_AtmOptics_Combine_AD      , &
                                        CRTM_AtmOptics_NoScatterCopy   , &
                                        CRTM_AtmOptics_NoScatterCopy_AD
  USE CRTM_SfcOptics_Define,      ONLY: OPERATOR(+)              , &
                                        CRTM_SfcOptics_type      , &
                                        CRTM_SfcOptics_Associated, &
                                        CRTM_SfcOptics_Create    , &
                                        CRTM_SfcOptics_Destroy   , &
                                        CRTM_SfcOptics_Zero
  USE CRTM_SfcOptics,             ONLY: CRTM_Compute_SurfaceT   , &
                                        CRTM_Compute_SurfaceT_AD
  USE CRTM_RTSolution,            ONLY: CRTM_Compute_nStreams     , &
                                        CRTM_Compute_RTSolution   , &
                                        CRTM_Compute_RTSolution_AD
  USE CRTM_AntennaCorrection,     ONLY: CRTM_Compute_AntCorr, &
                                        CRTM_Compute_AntCorr_AD
  USE CRTM_MoleculeScatter,       ONLY: CRTM_Compute_MoleculeScatter, &
                                        CRTM_Compute_MoleculeScatter_AD
  USE CRTM_AncillaryInput_Define, ONLY: CRTM_AncillaryInput_type
  USE CRTM_CloudCoeff,            ONLY: CRTM_CloudCoeff_IsLoaded
  USE CRTM_AerosolCoeff,          ONLY: CRTM_AerosolCoeff_IsLoaded
  USE CRTM_NLTECorrection,        ONLY: NLTE_Predictor_type       , &
                                        NLTE_Predictor_IsActive   , &
                                        Compute_NLTE_Predictor    , &
                                        Compute_NLTE_Predictor_AD , &
                                        Compute_NLTE_Correction   , &
                                        Compute_NLTE_Correction_AD
  USE ACCoeff_Define,             ONLY: ACCoeff_Associated
  USE NLTECoeff_Define,           ONLY: NLTECoeff_Associated
  USE CRTM_Planck_Functions,      ONLY: CRTM_Planck_Temperature   , &
                                        CRTM_Planck_Temperature_AD
  USE CRTM_CloudCover_Define,     ONLY: CRTM_CloudCover_type

  ! Internal variable definition modules
  ! ...AtmOptics
  USE AOvar_Define, ONLY: AOvar_type, &
                          AOvar_Associated, &
                          AOvar_Destroy   , &
                          AOvar_Create
  ! ...CloudScatter
  USE CSvar_Define, ONLY: CSvar_type, &
                          CSvar_Associated, &
                          CSvar_Destroy   , &
                          CSvar_Create
  ! ...AerosolScatter
  USE ASvar_Define, ONLY: ASvar_type, &
                          ASvar_Associated, &
                          ASvar_Destroy   , &
                          ASvar_Create
  ! ...Radiative transfer
  USE RTV_Define,   ONLY: RTV_type      , &
                          RTV_Associated, &
                          RTV_Destroy   , &
                          RTV_Create

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public procedures
  PUBLIC :: CRTM_Adjoint
  PUBLIC :: CRTM_Adjoint_Version


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Adjoint_Module.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Adjoint
!
! PURPOSE:
!       Function that calculates the adjoint of top-of-atmosphere (TOA)
!       radiances and brightness temperatures for an input atmospheric
!       profile or profile set and user specified satellites/channels.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Adjoint( Atmosphere       , &
!                                    Surface          , &
!                                    RTSolution_AD    , &
!                                    Geometry         , &
!                                    ChannelInfo      , &
!                                    Atmosphere_AD    , &
!                                    Surface_AD       , &
!                                    RTSolution       , &
!                                    Options = Options  )
!
! INPUTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Rank-1 (n_Profiles)
!                       ATTRIBUTES: INTENT(IN)
!
!       Surface:        Structure containing the Surface data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN)
!
!       RTSolution_AD:  Structure containing the RT solution adjoint inputs.
!                       **NOTE: On EXIT from this function, the contents of
!                               this structure may be modified (e.g. set to
!                               zero.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       Geometry:       Structure containing the view geometry
!                       information.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Geometry_type
!                       DIMENSION:  Same as input Atmosphere argument
!                       ATTRIBUTES: INTENT(IN)
!
!       ChannelInfo:    Structure returned from the CRTM_Init() function
!                       that contains the satellite/sensor channel index
!                       information.
!                       UNITS:      N/A
!                       TYPE:       CRTM_ChannelInfo_type
!                       DIMENSION:  Rank-1 (n_Sensors)
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Options:        Options structure containing the optional forward model
!                       arguments for the CRTM.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUTS:
!       Atmosphere_AD:  Structure containing the adjoint Atmosphere data.
!                       **NOTE: On ENTRY to this function, the contents of
!                               this structure should be defined (e.g.
!                               initialized to some value based on the
!                               position of this function in the call chain.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Same as input Atmosphere argument
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       Surface_AD:     Structure containing the tangent-linear Surface data.
!                       **NOTE: On ENTRY to this function, the contents of
!                               this structure should be defined (e.g.
!                               initialized to some value based on the
!                               position of this function in the call chain.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input Atmosphere argument
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Same as input RTSolution_AD argument
!                       ATTRIBUTES: INTENT(IN OUT)
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
! SIDE EFFECTS:
!      Note that the input adjoint arguments are modified upon exit, and
!      the output adjoint arguments must be defined upon entry. This is
!      a consequence of the adjoint formulation where, effectively, the
!      chain rule is being used and this function could reside anywhere
!      in the chain of derivative terms.
!
! COMMENTS:
!       - The Options optional structure arguments contain
!         spectral information (e.g. emissivity) that must have the same
!         spectral dimensionality (the "L" dimension) as the RTSolution
!         structures.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Adjoint( &
    Atmosphere   , &  ! FWD Input, M
    Surface      , &  ! FWD Input, M
    RTSolution_AD, &  ! AD  Input, L x M
    Geometry     , &  ! Input, M
    ChannelInfo  , &  ! Input, Scalar
    Atmosphere_AD, &  ! AD  Output, M
    Surface_AD   , &  ! AD  Output, M
    RTSolution   , &  ! FWD Output, L x M
    Options      ) &  ! Optional FWD input,  M
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type)       , INTENT(IN)     :: Atmosphere(:)      ! M
    TYPE(CRTM_Surface_type)          , INTENT(IN)     :: Surface(:)         ! M
    TYPE(CRTM_RTSolution_type)       , INTENT(IN OUT) :: RTSolution_AD(:,:) ! L x M
    TYPE(CRTM_Geometry_type)         , INTENT(IN)     :: Geometry(:)        ! M
    TYPE(CRTM_ChannelInfo_type)      , INTENT(IN)     :: ChannelInfo(:)     ! n_Sensors
    TYPE(CRTM_Atmosphere_type)       , INTENT(IN OUT) :: Atmosphere_AD(:)   ! M
    TYPE(CRTM_Surface_type)          , INTENT(IN OUT) :: Surface_AD(:)      ! M
    TYPE(CRTM_RTSolution_type)       , INTENT(IN OUT) :: RTSolution(:,:)    ! L x M
    TYPE(CRTM_Options_type), OPTIONAL, INTENT(IN)     :: Options(:)         ! M
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Adjoint'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Options_Present
    LOGICAL :: compute_antenna_correction
    LOGICAL :: Atmosphere_Invalid, Surface_Invalid, Geometry_Invalid, Options_Invalid
    INTEGER :: Status_FWD, Status_AD
    INTEGER :: iFOV
    INTEGER :: n, n_Sensors,  SensorIndex
    INTEGER :: l, n_Channels, ChannelIndex
    INTEGER :: m, n_Profiles
    INTEGER :: ln
    INTEGER :: n_Full_Streams, mth_Azi
    INTEGER :: cloud_coverage_flag
    REAL(fp) :: Source_ZA
    REAL(fp) :: Wavenumber
    REAL(fp) :: transmittance, transmittance_AD
    REAL(fp) :: transmittance_clear, transmittance_clear_AD
    REAL(fp) :: r_cloudy
    ! Local ancillary input structure
    TYPE(CRTM_AncillaryInput_type) :: AncillaryInput
    ! Local options structure for default values
    TYPE(CRTM_Options_type) :: Default_Options, Opt
    ! Local atmosphere structure for extra layering
    TYPE(CRTM_Atmosphere_type) :: Atm, Atm_AD
    ! Clear sky structures
    TYPE(CRTM_Atmosphere_type) :: Atm_Clear       , Atm_Clear_AD
    TYPE(CRTM_AtmOptics_type)  :: AtmOptics_Clear , AtmOptics_Clear_AD
    TYPE(CRTM_SfcOptics_type)  :: SfcOptics_Clear , SfcOptics_Clear_AD
    TYPE(CRTM_RTSolution_type) :: RTSolution_Clear, RTSolution_Clear_AD
    TYPE(RTV_type)             :: RTV_Clear
    ! Component variables
    TYPE(CRTM_GeometryInfo_type) :: GeometryInfo
    TYPE(CRTM_Predictor_type)    :: Predictor, Predictor_AD
    TYPE(CRTM_AtmOptics_type)    :: AtmOptics, AtmOptics_AD
    TYPE(CRTM_SfcOPtics_type)    :: SfcOptics, SfcOptics_AD
    ! Component variable internals
    TYPE(CRTM_PVar_type)  :: PVar   ! Predictor
    TYPE(CRTM_AAVar_type) :: AAVar  ! AtmAbsorption
    TYPE(CSvar_type)      :: CSvar  ! CloudScatter
    TYPE(ASvar_type)      :: ASvar  ! AerosolScatter
    TYPE(AOvar_type)      :: AOvar  ! AtmOptics
    TYPE(RTV_type)        :: RTV    ! RTSolution
    ! NLTE correction term predictors
    TYPE(NLTE_Predictor_type) :: NLTE_Predictor, NLTE_Predictor_AD
    ! Cloud cover object
    TYPE(CRTM_CloudCover_type) :: CloudCover, CloudCover_AD


    ! ------
    ! SET UP
    ! ------
    Error_Status = SUCCESS


    ! If no sensors or channels, simply return
    n_Sensors  = SIZE(ChannelInfo)
    n_Channels = SUM(CRTM_ChannelInfo_n_Channels(ChannelInfo))
    IF ( n_Sensors == 0 .OR. n_Channels == 0 ) RETURN


    ! Check spectral arrays
    IF ( SIZE(RTSolution   ,DIM=1) < n_Channels .OR. &
         SIZE(RTSolution_AD,DIM=1) < n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Output RTSolution structure arrays too small (",i0," and ",i0,&
             &") to hold results for the number of requested channels (",i0,")")') &
             SIZE(RTSolution,DIM=1), SIZE(RTSolution_AD,DIM=1), n_Channels
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! Check the number of profiles
    ! ...Number of atmospheric profiles.
    n_Profiles = SIZE(Atmosphere)
    ! ...Check the profile dimensionality of the other mandatory arguments
    IF ( SIZE(Surface)             /= n_Profiles .OR. &
         SIZE(RTSolution_AD,DIM=2) /= n_Profiles .OR. &
         SIZE(Geometry)            /= n_Profiles .OR. &
         SIZE(Atmosphere_AD)       /= n_Profiles .OR. &
         SIZE(Surface_AD)          /= n_Profiles .OR. &
         SIZE(RTSolution,   DIM=2) /= n_Profiles      ) THEN
      Error_Status = FAILURE
      Message = 'Inconsistent profile dimensionality for input arguments.'
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF
    ! ...Check the profile dimensionality of the other optional arguments
    Options_Present = .FALSE.
    IF ( PRESENT(Options) ) THEN
      Options_Present = .TRUE.
      IF ( SIZE(Options) /= n_Profiles ) THEN
        Error_Status = FAILURE
        Message = 'Inconsistent profile dimensionality for Options optional input argument.'
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
    END IF


    ! Reinitialise the output RTSolution
    CALL CRTM_RTSolution_Zero(RTSolution)


    ! Allocate the profile independent surface optics local structure
    CALL CRTM_SfcOptics_Create( SfcOptics   , MAX_N_ANGLES, MAX_N_STOKES )
    CALL CRTM_SfcOptics_Create( SfcOptics_AD, MAX_N_ANGLES, MAX_N_STOKES )
    IF ( (.NOT. CRTM_SfcOptics_Associated(SfcOptics   )) .OR. &
         (.NOT. CRTM_SfcOptics_Associated(SfcOptics_AD)) ) THEN
      Error_Status = FAILURE
      Message = 'Error allocating SfcOptics data structures'
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! ------------
    ! PROFILE LOOP
    ! ------------
    Profile_Loop: DO m = 1, n_Profiles


      ! Check the cloud and aerosol coeff. data for cases with clouds and aerosol
      IF( Atmosphere(m)%n_Clouds > 0 .AND. .NOT. CRTM_CloudCoeff_IsLoaded() )THEN
         Error_Status = FAILURE
         WRITE( Message,'("The CloudCoeff data must be loaded (with CRTM_Init routine) ", &
                &"for the cloudy case profile #",i0)' ) m
         CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
         RETURN
      END IF
      IF( Atmosphere(m)%n_Aerosols > 0 .AND. .NOT. CRTM_AerosolCoeff_IsLoaded() )THEN
         Error_Status = FAILURE
         WRITE( Message,'("The AerosolCoeff data must be loaded (with CRTM_Init routine) ", &
                &"for the aerosol case profile #",i0)' ) m
         CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
         RETURN
      END IF


      ! Copy over forward "non-variable" inputs to adjoint outputs
      CALL CRTM_Atmosphere_NonVariableCopy( Atmosphere(m), Atmosphere_AD(m) )
      CALL CRTM_Surface_NonVariableCopy( Surface(m), Surface_AD(m) )


      ! Check the optional Options structure argument
      Opt = Default_Options
      IF ( Options_Present ) THEN
        Opt = Options(m)
        ! Copy over ancillary input
        AncillaryInput%SSU    = Options(m)%SSU
        AncillaryInput%Zeeman = Options(m)%Zeeman
      END IF
      ! ...Assign the option specific SfcOptics input
      SfcOptics%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM


      ! Check the input data if required
      IF ( Opt%Check_Input ) THEN
        ! ...Mandatory inputs
        Atmosphere_Invalid = .NOT. CRTM_Atmosphere_IsValid( Atmosphere(m) )
        Surface_Invalid    = .NOT. CRTM_Surface_IsValid( Surface(m) )
        Geometry_Invalid   = .NOT. CRTM_Geometry_IsValid( Geometry(m) )
        IF ( Atmosphere_Invalid .OR. Surface_Invalid .OR. Geometry_Invalid ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Input data check failed for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
        ! ...Optional input
        IF ( Options_Present ) THEN
          Options_Invalid = .NOT. CRTM_Options_IsValid( Options(m) )
          IF ( Options_Invalid ) THEN
            Error_Status = FAILURE
            WRITE( Message,'("Options data check failed for profile #",i0)' ) m
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF
          ! Are the channel dimensions consistent if emissivity is passed?
          IF ( Options(m)%Use_Emissivity ) THEN
            IF ( Options(m)%n_Channels < n_Channels ) THEN
              Error_Status = FAILURE
              WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
                     &"than the number of requested channels (",i0, ")" )' ) &
                     Options(m)%n_Channels, n_Channels
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF
          ! Check value for user-defined n_Streams
          IF ( Options(m)%Use_N_Streams ) THEN
            IF ( Options(m)%n_Streams <= 0 .OR. MOD(Options(m)%n_Streams,2) /= 0 .OR. &
                 Options(m)%n_Streams > MAX_N_STREAMS ) THEN
                Error_Status = FAILURE
                WRITE( Message,'( "Input Options n_Streams (", i0, ") is invalid" )' ) &
                       Options(m)%n_Streams
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                RETURN
            END IF
          END IF
        END IF
      END IF


      ! Process geometry
      ! ...Compute derived geometry
      CALL CRTM_GeometryInfo_SetValue( GeometryInfo, Geometry=Geometry(m) )
      CALL CRTM_GeometryInfo_Compute( GeometryInfo )
      ! ...Retrieve components into local variable
      CALL CRTM_GeometryInfo_GetValue( &
             GeometryInfo, &
             iFOV = iFOV, &
             Source_Zenith_Angle = Source_ZA )


      ! Add extra layers to current atmosphere profile
      ! if necessary to handle upper atmosphere
      Error_Status = CRTM_Atmosphere_AddLayers( Atmosphere(m), Atm )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error adding FWD extra layers to profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
      ! ...Check the total number of Atm layers
      IF ( Atm%n_Layers > MAX_N_LAYERS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Added layers [",i0,"] cause total [",i0,"] to exceed the ",&
               &"maximum allowed [",i0,"] for profile #",i0)' ) &
               Atm%n_Added_Layers, Atm%n_Layers, MAX_N_LAYERS, m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
      ! ...Similarly extend a copy of the input adjoint atmosphere
      Atm_AD = CRTM_Atmosphere_AddLayerCopy( Atmosphere_AD(m), Atm%n_Added_Layers )


      ! Prepare the atmospheric optics structures
      ! ...Allocate the atmospheric optics structures based on Atm extension
      CALL CRTM_AtmOptics_Create( AtmOptics, &
                                  Atm%n_Layers        , &
                                  MAX_N_LEGENDRE_TERMS, &
                                  MAX_N_PHASE_ELEMENTS  )
      CALL CRTM_AtmOptics_Create( AtmOptics_AD, &
                                  Atm%n_Layers        , &
                                  MAX_N_LEGENDRE_TERMS, &
                                  MAX_N_PHASE_ELEMENTS  )
      IF ( .NOT. CRTM_AtmOptics_Associated( Atmoptics ) .OR. &
           .NOT. CRTM_AtmOptics_Associated( Atmoptics_AD ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error allocating AtmOptics data structures for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
      ! ...Set the Scattering Switch
      AtmOptics%Include_Scattering    = Opt%Include_Scattering
      AtmOptics_AD%Include_Scattering = Opt%Include_Scattering
      ! ...Allocate the atmospheric optics internal structure
      CALL AOvar_Create( AOvar, Atm%n_Layers )


      ! Allocate the scattering internal variables if necessary
      ! ...Cloud
      IF ( Atm%n_Clouds > 0 ) THEN
        CALL CSvar_Create( CSvar, &
                           MAX_N_LEGENDRE_TERMS, &
                           MAX_N_PHASE_ELEMENTS, &
                           Atm%n_Layers        , &
                           Atm%n_Clouds          )
      END IF
      ! ...Aerosol
      IF ( Atm%n_Aerosols > 0 ) THEN
        CALL ASvar_Create( ASvar, &
                           MAX_N_LEGENDRE_TERMS, &
                           MAX_N_PHASE_ELEMENTS, &
                           Atm%n_Layers        , &
                           Atm%n_Aerosols        )
      END IF


      ! Determine the type of cloud coverage
      cloud_coverage_flag = CRTM_Atmosphere_Coverage( atm )


      ! Setup for fractional cloud coverage
      IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
      
        ! Compute cloudcover
        Error_Status = CloudCover%Compute_CloudCover(atm, Overlap = opt%Overlap_Id)
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error computing cloud cover in profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
        ! ...Mold the adjoint object based on the forward, and reinitialise
        CloudCover_AD = CloudCover
        CALL CloudCover_AD%Set_To_Zero()

        ! Allocate some of the CLEAR sky structure for fractional cloud coverage
        ! (The AtmOptics structures are allocated during a copy)
        ! ...Clear sky atmosphere
        Status_FWD = CRTM_Atmosphere_ClearSkyCopy(Atm, Atm_Clear)
        Status_AD  = CRTM_Atmosphere_ClearSkyCopy(Atm, Atm_Clear_AD)
        IF ( Status_FWD /= SUCCESS .OR. Status_AD /= SUCCESS ) THEN
          Error_status = FAILURE
          WRITE( Message,'("Error copying CLEAR SKY Atmosphere structures for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
        CALL CRTM_Atmosphere_Zero( Atm_Clear_AD )
        ! ...Clear sky SfcOptics
        CALL CRTM_SfcOptics_Create( SfcOptics_Clear   , MAX_N_ANGLES, MAX_N_STOKES )
        CALL CRTM_SfcOptics_Create( SfcOptics_Clear_AD, MAX_N_ANGLES, MAX_N_STOKES )
        IF ( (.NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear)) .OR. &
             (.NOT. CRTM_SfcOptics_Associated(SfcOptics_Clear_AD))) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Error allocating CLEAR SKY SfcOptics data structures for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
        CALL CRTM_SfcOptics_Zero( SfcOptics_Clear_AD )
        ! ...Copy over surface optics input
        SfcOptics_Clear%Use_New_MWSSEM    = .NOT. Opt%Use_Old_MWSSEM
        SfcOptics_Clear_AD%Use_New_MWSSEM = .NOT. Opt%Use_Old_MWSSEM
        ! ...CLEAR SKY average surface skin temperature for multi-surface types
        CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics_Clear )
      END IF


      ! Average surface skin temperature for multi-surface types
      CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )


      ! -----------
      ! SENSOR LOOP
      ! -----------
      ! Initialise channel counter for sensor(n)/channel(l) count
      ln = 0

      Sensor_Loop: DO n = 1, n_Sensors


        ! Shorter name
        SensorIndex = ChannelInfo(n)%Sensor_Index


        ! Check if antenna correction to be applied for current sensor
        compute_antenna_correction = ( Opt%Use_Antenna_Correction               .AND. &
                                       ACCoeff_Associated( SC(SensorIndex)%AC ) .AND. &
                                       iFOV /= 0 )


        ! Allocate the AtmAbsorption predictor structures
        CALL CRTM_Predictor_Create( &
               Predictor   , &
               atm%n_Layers, &
               SensorIndex , &
               SaveFWV = 1   )
        CALL CRTM_Predictor_Create( &
               Predictor_AD, &
               atm%n_Layers, &
               SensorIndex   )
        IF ( (.NOT. CRTM_Predictor_Associated(Predictor)) .OR. &
             (.NOT. CRTM_Predictor_Associated(Predictor_AD)) ) THEN
          Error_Status=FAILURE
          WRITE( Message,'("Error allocating predictor structures for profile #",i0, &
                 &" and ",a," sensor.")' ) m, SC(SensorIndex)%Sensor_Id
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
        ! ...Compute forward model predictors
        CALL CRTM_Compute_Predictors( SensorIndex   , &  ! Input
                                      Atm           , &  ! Input
                                      GeometryInfo  , &  ! Input
                                      AncillaryInput, &  ! Input
                                      Predictor     , &  ! Output
                                      PVar            )  ! Internal variable output


        ! Allocate the RTV structure if necessary
        IF( ( Atm%n_Clouds   > 0 .OR. &
              Atm%n_Aerosols > 0 .OR. &
              SpcCoeff_IsVisibleSensor(SC(SensorIndex)) ) .AND. &
            AtmOptics%Include_Scattering ) THEN
          CALL RTV_Create( RTV, MAX_N_ANGLES, MAX_N_LEGENDRE_TERMS, Atm%n_Layers )
          IF ( .NOT. RTV_Associated(RTV) ) THEN
            Error_Status=FAILURE
            WRITE( Message,'("Error allocating RTV structure for profile #",i0, &
                   &" and ",a," sensor.")' ) m, TRIM(SC(SensorIndex)%Sensor_Id)
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF
          ! Assign algorithm selector
          RTV%RT_Algorithm_Id = Opt%RT_Algorithm_Id
        END IF


        ! Compute NLTE correction predictors
        IF ( Opt%Apply_NLTE_Correction ) THEN
          CALL Compute_NLTE_Predictor( &
                 SC(SensorIndex)%NC, &  ! Input
                 Atm               , &  ! Input
                 GeometryInfo      , &  ! Input
                 NLTE_Predictor      )  ! Output
        END IF


        ! ------------
        ! CHANNEL LOOP
        ! ------------
        Channel_Loop: DO l = 1, ChannelInfo(n)%n_Channels

          ! Channel setup
          ! ...Skip channel if requested
          IF ( .NOT. ChannelInfo(n)%Process_Channel(l) ) CYCLE Channel_Loop
          ! ...Shorter name
          ChannelIndex = ChannelInfo(n)%Channel_Index(l)
          ! ...Increment the processed channel counter
          ln = ln + 1
          ! ...Assign sensor+channel information to output
          RTSolution(ln,m)%Sensor_Id        = ChannelInfo(n)%Sensor_Id
          RTSolution(ln,m)%WMO_Satellite_Id = ChannelInfo(n)%WMO_Satellite_Id
          RTSolution(ln,m)%WMO_Sensor_Id    = ChannelInfo(n)%WMO_Sensor_Id
          RTSolution(ln,m)%Sensor_Channel   = ChannelInfo(n)%Sensor_Channel(l)
          RTSolution_AD(ln,m)%Sensor_Id        = RTSolution(ln,m)%Sensor_Id
          RTSolution_AD(ln,m)%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
          RTSolution_AD(ln,m)%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id
          RTSolution_AD(ln,m)%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel
          ! ...Same for clear structures
          RTSolution_Clear%Sensor_Id        = RTSolution(ln,m)%Sensor_Id
          RTSolution_Clear%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
          RTSolution_Clear%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id
          RTSolution_Clear%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel
          RTSolution_Clear_AD%Sensor_Id        = RTSolution(ln,m)%Sensor_Id
          RTSolution_Clear_AD%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
          RTSolution_Clear_AD%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id
          RTSolution_Clear_AD%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel


          ! Initialisations
          CALL CRTM_AtmOptics_Zero( AtmOptics )
          CALL CRTM_AtmOptics_Zero( AtmOptics_AD )
          transmittance_AD = ZERO
          CALL CRTM_RTSolution_Zero( RTSolution_Clear )
          CALL CRTM_RTSolution_Zero( RTSolution_Clear_AD )


          ! Determine the number of streams (n_Full_Streams) in up+downward directions
          IF ( Opt%Use_N_Streams ) THEN
            n_Full_Streams = Options(m)%n_Streams
            RTSolution(ln,m)%n_Full_Streams = n_Full_Streams + 2
            RTSolution(ln,m)%Scattering_Flag = .TRUE.
          ELSE
            n_Full_Streams = CRTM_Compute_nStreams( Atm             , &  ! Input
                                                    SensorIndex     , &  ! Input
                                                    ChannelIndex    , &  ! Input
                                                    RTSolution(ln,m)  )  ! Output
          END IF
          ! ...Transfer stream count to scattering structures
          AtmOptics%n_Legendre_Terms    = n_Full_Streams
          AtmOptics_AD%n_Legendre_Terms = n_Full_Streams
          ! ...Ensure clear-sky object dimensions are consistent
          AtmOptics_Clear_AD%n_Legendre_Terms = AtmOptics_AD%n_Legendre_Terms


          ! Compute the gas absorption
          CALL CRTM_Compute_AtmAbsorption( SensorIndex   , &  ! Input
                                           ChannelIndex  , &  ! Input
                                           AncillaryInput, &  ! Input
                                           Predictor     , &  ! Input
                                           AtmOptics     , &  ! Output
                                           AAVar           )  ! Internal variable output


          ! Compute the molecular scattering properties
          ! ...Solar radiation
          IF( SC(SensorIndex)%Solar_Irradiance(ChannelIndex) > ZERO .AND. &
              Source_ZA < MAX_SOURCE_ZENITH_ANGLE) THEN
             RTV%Solar_Flag_true = .TRUE.
          END IF
          ! ...Visible channel with solar radiation
          IF( SpcCoeff_IsVisibleSensor( SC(SensorIndex) ) .AND. RTV%Solar_Flag_true ) THEN
            RTV%Visible_Flag_true = .TRUE.
            ! Rayleigh phase function has 0, 1, 2 components.
            IF( AtmOptics%n_Legendre_Terms < 4 ) THEN
              AtmOptics%n_Legendre_Terms = 4
              AtmOptics_AD%n_Legendre_Terms = AtmOptics%n_Legendre_Terms
              RTSolution(ln,m)%Scattering_FLAG = .TRUE.
              RTSolution(ln,m)%n_Full_Streams = AtmOptics%n_Legendre_Terms + 2
            END IF
            RTV%n_Azi = MIN( AtmOptics%n_Legendre_Terms - 1, MAX_N_AZIMUTH_FOURIER )
            ! Get molecular scattering and extinction
            Wavenumber = SC(SensorIndex)%Wavenumber(ChannelIndex)
            Error_Status = CRTM_Compute_MoleculeScatter( &
                             Wavenumber, &
                             Atm       , &
                             AtmOptics   )
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing MoleculeScatter for ",a,&
                     &", channel ",i0,", profile #",i0)') &
                     TRIM(ChannelInfo(n)%Sensor_ID), &
                     ChannelInfo(n)%Sensor_Channel(l), &
                     m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          ELSE
            RTV%Visible_Flag_true = .FALSE.
            RTV%n_Azi = 0
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              RTV_Clear%Visible_Flag_true = .FALSE.
              RTV_Clear%n_Azi = 0
            END IF
          END IF


          ! Copy the clear-sky AtmOptics
          IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
            Status_FWD = CRTM_AtmOptics_NoScatterCopy( AtmOptics, AtmOptics_Clear )
            Status_AD  = CRTM_AtmOptics_NoScatterCopy( AtmOptics, AtmOptics_Clear_AD )
            IF ( Status_FWD /= SUCCESS .OR. Status_AD /= SUCCESS ) THEN
              Error_Status = FAILURE
              WRITE( Message,'("Error copying CLEAR SKY AtmOptics for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
            ! Initialise the adjoint
            CALL CRTM_AtmOptics_Zero( AtmOptics_Clear_AD )
          END IF


          ! Compute the cloud particle absorption/scattering properties
          IF( Atm%n_Clouds > 0 ) THEN
            Error_Status = CRTM_Compute_CloudScatter( Atm         , &  ! Input
                                                      SensorIndex , &  ! Input
                                                      ChannelIndex, &  ! Input
                                                      AtmOptics   , &  ! Output
                                                      CSvar         )  ! Internal variable output
            IF (Error_Status /= SUCCESS) THEN
              WRITE( Message,'("Error computing CloudScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF


          ! Compute the aerosol absorption/scattering properties
          IF ( Atm%n_Aerosols > 0 ) THEN
            Error_Status = CRTM_Compute_AerosolScatter( Atm         , &  ! Input
                                                        SensorIndex , &  ! Input
                                                        ChannelIndex, &  ! Input
                                                        AtmOptics   , &  ! In/Output
                                                        ASvar         )  ! Internal variable output
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing AerosolScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF


          ! Compute the combined atmospheric optical properties
          IF( AtmOptics%Include_Scattering ) THEN
            CALL CRTM_AtmOptics_Combine( AtmOptics, AOvar )
          END IF
          ! ...Save vertically integrated scattering optical depth for output
          RTSolution(ln,m)%SOD = AtmOptics%Scattering_Optical_Depth


          ! Compute the all-sky atmospheric transmittance
          ! for use in FASTEM-X reflection correction
          CALL CRTM_Compute_Transmittance(AtmOptics,transmittance)
          SfcOptics%Transmittance = transmittance
          ! ...Clear sky for fractional cloud cover
          IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
            CALL CRTM_Compute_Transmittance(AtmOptics_Clear,transmittance_clear)
            SfcOptics_Clear%Transmittance = transmittance_clear
          END IF


          ! Fill the SfcOptics structure for the optional emissivity input case.
          SfcOptics%Compute       = .TRUE.
          SfcOptics_Clear%Compute = .TRUE.
          IF ( Opt%Use_Emissivity ) THEN
            SfcOptics%Compute = .FALSE.
            SfcOptics%Emissivity(1,1)       = Opt%Emissivity(ln)
            SfcOptics%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
            IF ( Opt%Use_Direct_Reflectivity ) THEN
              SfcOptics%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
            ELSE
              SfcOptics%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
            END IF
            ! ...Repeat for fractional clear-sky case
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              SfcOptics_Clear%Compute = .FALSE.
              SfcOptics_Clear%Emissivity(1,1)       = Opt%Emissivity(ln)
              SfcOptics_Clear%Reflectivity(1,1,1,1) = ONE - Opt%Emissivity(ln)
              IF ( Opt%Use_Direct_Reflectivity ) THEN
                SfcOptics_Clear%Direct_Reflectivity(1,1) = Opt%Direct_Reflectivity(ln)
              ELSE
                SfcOptics_Clear%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
              END IF
            END IF
          END IF


          ! Fourier component loop for azimuth angles (VIS).
          ! mth_Azi = 0 is for an azimuth-averaged value (IR, MW)
          ! ...Initialise radiance
          RTSolution(ln,m)%Radiance = ZERO



          ! ###################################################
          ! TEMPORARY FIX : SENSOR-DEPENDENT AZIMUTH ANGLE LOOP
          ! ###################################################
          Sensor_Dependent_RTSolution: &
          IF ( SpcCoeff_IsInfraredSensor( SC(SensorIndex) ) .OR. &
               SpcCoeff_IsMicrowaveSensor( SC(SensorIndex) ) ) THEN
            ! ------------------------------
            ! INFRARED and MICROWAVE sensors
            ! ------------------------------

            ! Set dependent component counters
            RTV%mth_Azi = 0
            SfcOptics%mth_Azi = 0

            ! Solve the forward radiative transfer problem
            Error_Status = CRTM_Compute_RTSolution( &
                             Atm             , &  ! Input
                             Surface(m)      , &  ! Input
                             AtmOptics       , &  ! Input
                             SfcOptics       , &  ! Input
                             GeometryInfo    , &  ! Input
                             SensorIndex     , &  ! Input
                             ChannelIndex    , &  ! Input
                             RTSolution(ln,m), &  ! Output
                             RTV               )  ! Internal variable output
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'( "Error computing RTSolution for ", a, &
                     &", channel ", i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF


            ! Repeat clear sky for fractionally cloudy atmospheres
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              RTV_Clear%mth_Azi = 0 !RTV%mth_Azi
              SfcOptics_Clear%mth_Azi = 0 !SfcOptics%mth_Azi
              Error_Status = CRTM_Compute_RTSolution( &
                               Atm_Clear       , &  ! Input
                               Surface(m)      , &  ! Input
                               AtmOptics_Clear , &  ! Input
                               SfcOptics_Clear , &  ! Input
                               GeometryInfo    , &  ! Input
                               SensorIndex     , &  ! Input
                               ChannelIndex    , &  ! Input
                               RTSolution_Clear, &  ! Output
                               RTV_Clear         )  ! Internal variable output
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'( "Error computing CLEAR SKY RTSolution for ", a, &
                       &", channel ", i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                RETURN
              END IF
            END IF


            ! Combine cloudy and clear radiances for fractional cloud coverage
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              r_cloudy = RTSolution(ln,m)%Radiance  ! Save the 100% cloudy radiance
              RTSolution(ln,m)%Radiance = &
                  ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_Clear%Radiance) + &
                  (CloudCover%Total_Cloud_Cover * RTSolution(ln,m)%Radiance)
              ! ...Save the cloud cover in the output structure
              RTSolution(ln,m)%Total_Cloud_Cover = CloudCover%Total_Cloud_Cover
            END IF


            ! The radiance post-processing
            CALL Post_Process_RTSolution(RTSolution(ln,m))

            
            ! Perform clear-sky post and pre-processing
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
              ! Radiance post-processing
              CALL Post_Process_RTSolution(RTSolution_Clear)
              RTSolution(ln,m)%R_Clear  = RTSolution_Clear%Radiance
              RTSolution(ln,m)%Tb_Clear = RTSolution_Clear%Brightness_Temperature

              ! Adjoint radiance pre-processing
              RTSolution_Clear_AD%Brightness_Temperature = RTSolution_Clear_AD%Brightness_Temperature + &
                                                           RTSolution_AD(ln,m)%Tb_Clear
              RTSolution_AD(ln,m)%Tb_Clear               = ZERO
              RTSolution_Clear_AD%Radiance = RTSolution_Clear_AD%Radiance + &
                                             RTSolution_AD(ln,m)%R_Clear
              RTSolution_AD(ln,m)%R_Clear  = ZERO
              CALL Pre_Process_RTSolution_AD(RTSolution_Clear, RTSolution_Clear_AD)
            END IF


            ! The adjoint radiance pre-processing
            CALL Pre_Process_RTSolution_AD(RTSolution(ln,m), RTSolution_AD(ln,m))

          
            ! More fractionally cloudy atmospheres processing
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN

              ! The adjoint of the clear and cloudy radiance combination
              CloudCover_AD%Total_Cloud_Cover = CloudCover_AD%Total_Cloud_Cover + &
                                                RTSolution_AD(ln,m)%Total_Cloud_Cover
              RTSolution_AD(ln,m)%Total_Cloud_Cover = ZERO
              RTSolution_Clear_AD%Radiance    = RTSolution_Clear_AD%Radiance + &
                                                ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_AD(ln,m)%Radiance)
              CloudCover_AD%Total_Cloud_Cover = CloudCover_AD%Total_Cloud_Cover + &
                                                ((r_cloudy - RTSolution_Clear%Radiance) * RTSolution_AD(ln,m)%Radiance)
              RTSolution_AD(ln,m)%Radiance    = CloudCover%Total_Cloud_Cover * RTSolution_AD(ln,m)%Radiance

              ! The adjoint of the clear sky radiative transfer for fractionally cloudy atmospheres
              Error_Status = CRTM_Compute_RTSolution_AD( &
                               Atm_Clear          , &  ! FWD Input
                               Surface(m)         , &  ! FWD Input
                               AtmOptics_Clear    , &  ! FWD Input
                               SfcOptics_Clear    , &  ! FWD Input
                               RTSolution_Clear   , &  ! FWD Input
                               RTSolution_Clear_AD, &  ! AD  Input
                               GeometryInfo       , &  ! Input
                               SensorIndex        , &  ! Input
                               ChannelIndex       , &  ! Input
                               Atm_Clear_AD       , &  ! AD Output
                               Surface_AD(m)      , &  ! AD Output
                               AtmOptics_Clear_AD , &  ! AD Output
                               SfcOptics_Clear_AD , &  ! AD Output
                               RTV_Clear            )  ! Internal variable input
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'( "Error computing CLEAR SKY RTSolution_AD for ", a, &
                       &", channel ", i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                RETURN
              END IF
            END IF


            ! The adjoint of the radiative transfer
            Error_Status = CRTM_Compute_RTSolution_AD( &
                             Atm                , &  ! FWD Input
                             Surface(m)         , &  ! FWD Input
                             AtmOptics          , &  ! FWD Input
                             SfcOptics          , &  ! FWD Input
                             RTSolution(ln,m)   , &  ! FWD Input
                             RTSolution_AD(ln,m), &  ! AD  Input
                             GeometryInfo       , &  ! Input
                             SensorIndex        , &  ! Input
                             ChannelIndex       , &  ! Input
                             Atm_AD             , &  ! AD Output
                             Surface_AD(m)      , &  ! AD Output
                             AtmOptics_AD       , &  ! AD Output
                             SfcOptics_AD       , &  ! AD Output
                             RTV                  )  ! Internal variable input
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'( "Error computing RTSolution_AD for ", a, &
                     &", channel ", i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF


          ELSE Sensor_Dependent_RTSolution
            ! --------------
            ! VISIBLE sensor
            ! --------------
            ! Fourier expansion over azimuth angle
            Azimuth_Fourier_Loop: DO mth_Azi = 0, RTV%n_Azi

              ! Set dependent component counters
              RTV%mth_Azi = mth_Azi
              SfcOptics%mth_Azi = mth_Azi

              ! Solve the forward radiative transfer problem
              Error_Status = CRTM_Compute_RTSolution( &
                               Atm             , &  ! Input
                               Surface(m)      , &  ! Input
                               AtmOptics       , &  ! Input
                               SfcOptics       , &  ! Input
                               GeometryInfo    , &  ! Input
                               SensorIndex     , &  ! Input
                               ChannelIndex    , &  ! Input
                               RTSolution(ln,m), &  ! Output
                               RTV               )  ! Internal variable output
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'( "Error computing RTSolution for ", a, &
                       &", channel ", i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                RETURN
              END IF

              ! Repeat clear sky for fractionally cloudy atmospheres
              IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
                RTV_Clear%mth_Azi = RTV%mth_Azi
                SfcOptics_Clear%mth_Azi = SfcOptics%mth_Azi
                Error_Status = CRTM_Compute_RTSolution( &
                                 Atm_Clear       , &  ! Input
                                 Surface(m)      , &  ! Input
                                 AtmOptics_Clear , &  ! Input
                                 SfcOptics_Clear , &  ! Input
                                 GeometryInfo    , &  ! Input
                                 SensorIndex     , &  ! Input
                                 ChannelIndex    , &  ! Input
                                 RTSolution_Clear, &  ! Output
                                 RTV_Clear         )  ! Internal variable output
                IF ( Error_Status /= SUCCESS ) THEN
                  WRITE( Message,'( "Error computing CLEAR SKY RTSolution for ", a, &
                         &", channel ", i0,", profile #",i0)' ) &
                         TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                  CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                  RETURN
                END IF
              END IF
            END DO Azimuth_Fourier_Loop


            ! All of the "in-between" FWD and AD processing is for fractional cloud coverage only
            IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN

              ! FORWARD #1: Combine cloudy and clear radiances for fractional cloud coverage
              r_cloudy = RTSolution(ln,m)%Radiance  ! Save the 100% cloudy radiance
              RTSolution(ln,m)%Radiance = &
                  ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_Clear%Radiance) + &
                  (CloudCover%Total_Cloud_Cover * RTSolution(ln,m)%Radiance)
              ! FORWARD #2: Save the cloud cover and clear radiance in the output structure
              RTSolution(ln,m)%Total_Cloud_Cover = CloudCover%Total_Cloud_Cover
              RTSolution(ln,m)%R_Clear           = RTSolution_Clear%Radiance
              RTSolution(ln,m)%Tb_Clear          = ZERO      ! No Tb for visible

              ! ADJOINT #2: Of the cloud cover and clear radiance saving
              RTSolution_Clear_AD%Tb_Clear = ZERO   ! No Tb for visible
              RTSolution_Clear_AD%Radiance = RTSolution_Clear_AD%Radiance + &
                                             RTSolution_AD(ln,m)%R_Clear
              RTSolution_AD(ln,m)%R_Clear  = ZERO
              CloudCover_AD%Total_Cloud_Cover = CloudCover_AD%Total_Cloud_Cover + &
                                                RTSolution_AD(ln,m)%Total_Cloud_Cover
              RTSolution_AD(ln,m)%Total_Cloud_Cover = ZERO

              ! ADJOINT #1: Of the clear+cloudy combination
              RTSolution_Clear_AD%Radiance    = RTSolution_Clear_AD%Radiance + &
                                                ((ONE - CloudCover%Total_Cloud_Cover) * RTSolution_AD(ln,m)%Radiance)
              CloudCover_AD%Total_Cloud_Cover = CloudCover_AD%Total_Cloud_Cover + &
                                                ((r_cloudy - RTSolution_Clear%Radiance) * RTSolution_AD(ln,m)%Radiance)
              RTSolution_AD(ln,m)%Radiance    = CloudCover%Total_Cloud_Cover * RTSolution_AD(ln,m)%Radiance
            END IF


            ! Adjoint Fourier expansion over azimuth angle
            Azimuth_Fourier_Loop_AD: DO mth_Azi = 0, RTV%n_Azi


              ! Set dependent component counters
              RTV%mth_Azi = mth_Azi
              SfcOptics%mth_Azi = mth_Azi


              ! The adjoint of the clear sky radiative transfer for fractionally cloudy atmospheres
              IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
                RTV_Clear%mth_Azi = RTV%mth_Azi
                SfcOptics_Clear%mth_Azi = SfcOptics%mth_Azi
                Error_Status = CRTM_Compute_RTSolution_AD( &
                                 Atm_Clear          , &  ! FWD Input
                                 Surface(m)         , &  ! FWD Input
                                 AtmOptics_Clear    , &  ! FWD Input
                                 SfcOptics_Clear    , &  ! FWD Input
                                 RTSolution_Clear   , &  ! FWD Input
                                 RTSolution_Clear_AD, &  ! AD  Input
                                 GeometryInfo       , &  ! Input
                                 SensorIndex        , &  ! Input
                                 ChannelIndex       , &  ! Input
                                 Atm_Clear_AD       , &  ! AD Output
                                 Surface_AD(m)      , &  ! AD Output
                                 AtmOptics_Clear_AD , &  ! AD Output
                                 SfcOptics_Clear_AD , &  ! AD Output
                                 RTV_Clear            )  ! Internal variable input
                IF ( Error_Status /= SUCCESS ) THEN
                  WRITE( Message,'( "Error computing CLEAR SKY RTSolution_AD for ", a, &
                         &", channel ", i0,", profile #",i0)' ) &
                         TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                  CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                  RETURN
                END IF
              END IF


              ! The adjoint of the radiative transfer
              Error_Status = CRTM_Compute_RTSolution_AD( &
                               Atm                , &  ! FWD Input
                               Surface(m)         , &  ! FWD Input
                               AtmOptics          , &  ! FWD Input
                               SfcOptics          , &  ! FWD Input
                               RTSolution(ln,m)   , &  ! FWD Input
                               RTSolution_AD(ln,m), &  ! AD  Input
                               GeometryInfo       , &  ! Input
                               SensorIndex        , &  ! Input
                               ChannelIndex       , &  ! Input
                               Atm_AD             , &  ! AD Output
                               Surface_AD(m)      , &  ! AD Output
                               AtmOptics_AD       , &  ! AD Output
                               SfcOptics_AD       , &  ! AD Output
                               RTV                  )  ! Internal variable input
              IF ( Error_Status /= SUCCESS ) THEN
                WRITE( Message,'( "Error computing RTSolution_AD for ", a, &
                       &", channel ", i0,", profile #",i0)' ) &
                       TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
                CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
                RETURN
              END IF
            END DO Azimuth_Fourier_Loop_AD

          END IF Sensor_Dependent_RTSolution
          ! ###################################################
          ! TEMPORARY FIX : SENSOR-DEPENDENT AZIMUTH ANGLE LOOP
          ! ###################################################


          ! Compute the adjoint of the all-sky atmospheric transmittance
          ! for use in FASTEM-X reflection correction
          transmittance_AD = SfcOptics_AD%transmittance
          SfcOptics_AD%transmittance = ZERO
          CALL CRTM_Compute_Transmittance_AD(AtmOptics,transmittance_AD,AtmOptics_AD)
          ! ...Clear sky for fractional cloud cover
          IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
            transmittance_clear_AD = SfcOptics_Clear_AD%transmittance
            SfcOptics_Clear_AD%transmittance = ZERO
            CALL CRTM_Compute_Transmittance_AD(AtmOptics_Clear,transmittance_clear_AD,AtmOptics_Clear_AD)
          END IF


          ! Compute the adjoint of the combined atmospheric optical properties
          AtmOptics_AD%Scattering_Optical_Depth = AtmOptics_AD%Scattering_Optical_Depth + &
                                                  RTSolution_AD(ln,m)%SOD
          RTSolution_AD(ln,m)%SOD               = ZERO
          IF( AtmOptics%Include_Scattering ) THEN
            CALL CRTM_AtmOptics_Combine_AD( AtmOptics, AtmOptics_AD, AOvar )
          END IF


          ! Compute the adjoint aerosol absorption/scattering properties
          IF ( Atm%n_Aerosols > 0 ) THEN
            Error_Status = CRTM_Compute_AerosolScatter_AD( Atm         , &  ! FWD Input
                                                           AtmOptics   , &  ! FWD Input
                                                           AtmOptics_AD, &  ! AD  Input
                                                           SensorIndex , &  ! Input
                                                           ChannelIndex, &  ! Input
                                                           Atm_AD      , &  ! AD  Output
                                                           ASvar         )  ! Internal variable input
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing AerosolScatter_AD for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF


          ! Compute the adjoint cloud absorption/scattering properties
          IF ( Atm%n_Clouds > 0 ) THEN
            Error_Status = CRTM_Compute_CloudScatter_AD( Atm         , &  ! FWD Input
                                                         AtmOptics   , &  ! FWD Input
                                                         AtmOptics_AD, &  ! AD  Input
                                                         SensorIndex , &  ! Input
                                                         ChannelIndex, &  ! Input
                                                         Atm_AD      , &  ! AD  Output
                                                         CSvar         )  ! Internal variable input
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing CloudScatter_AD for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF


          ! Adjoint of clear-sky AtmOptics copy
          IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN
            Error_Status = CRTM_AtmOptics_NoScatterCopy_AD( AtmOptics, AtmOptics_Clear_AD, AtmOptics_AD )
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing CLEAR SKY AtmOptics_AD for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF


          ! Compute the adjoint molecular scattering properties
          IF( RTV%Visible_Flag_true ) THEN
            Wavenumber = SC(SensorIndex)%Wavenumber(ChannelIndex)
            Error_Status = CRTM_Compute_MoleculeScatter_AD( &
                             Wavenumber  , &
                             AtmOptics_AD, &
                             Atm_AD        )
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing MoleculeScatter_AD for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), &
                     ChannelInfo(n)%Sensor_Channel(l), &
                     m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF


          ! Compute the adjoint gaseous absorption
          CALL CRTM_Compute_AtmAbsorption_AD( SensorIndex     , &  ! Input
                                              ChannelIndex    , &  ! Input
                                              Predictor       , &  ! FWD Input
                                              AtmOptics_AD    , &  ! AD  Input
                                              Predictor_AD    , &  ! AD  Output
                                              AAVar             )  ! Internal variable input
          ! Gamma correction to optical depth
          RTSolution_AD(ln,m)%Gamma  = SUM(AtmOptics%Optical_Depth * AtmOptics_AD%Optical_Depth)
          AtmOptics_AD%Optical_Depth = AtmOptics_AD%Optical_Depth * (RTSolution(ln,m)%Gamma + ONE)

        END DO Channel_Loop


        ! Adjoint of the NLTE correction predictor calculations
        IF ( Opt%Apply_NLTE_Correction ) THEN
          CALL Compute_NLTE_Predictor_AD( &
                 NLTE_Predictor   , &  ! Input
                 NLTE_Predictor_AD, &  ! Input
                 Atm_AD             )  ! Output
        END IF


        ! Adjoint of the predictor calculations
        CALL CRTM_Compute_Predictors_AD( SensorIndex   , &  ! Input
                                         Atm           , &  ! FWD Input
                                         Predictor     , &  ! FWD Input
                                         Predictor_AD  , &  ! AD  Input
                                         AncillaryInput, &  ! Input
                                         Atm_AD        , &  ! AD  Output
                                         PVar            )  ! Internal variable input

      END DO Sensor_Loop


      ! Adjoint of average surface skin temperature for multi-surface types
      CALL CRTM_Compute_SurfaceT_AD( Surface(m), SfcOptics_AD, Surface_AD(m) )


      ! Adjoint of cloud cover setup
      IF ( CRTM_Atmosphere_IsFractional(cloud_coverage_flag) ) THEN

        ! Post process the CLEAR sky structures for fractional cloud coverage
        ! ...Clear sky SfcOptics
        CALL CRTM_Compute_SurfaceT_AD( Surface(m), SfcOptics_Clear_AD, Surface_AD(m) )
        ! ...Clear sky atmosphere
        Error_Status = CRTM_Atmosphere_ClearSkyCopy_AD(Atm, Atm_Clear_AD, Atm_AD)
        IF ( Error_Status /= SUCCESS ) THEN
          Error_status = FAILURE
          WRITE( Message,'("Error copying CLEAR SKY adjoint Atmosphere structure for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF

        ! Adjoint of the cloud coverage
        Error_Status = CloudCover_AD%Compute_CloudCover_AD(CloudCover, atm, atm_AD)
        IF ( Error_Status /= SUCCESS ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Error computing ADJOINT cloud cover for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
      END IF


      ! Adjoint of the atmosphere layer addition
      Error_Status = CRTM_Atmosphere_AddLayers_AD( Atmosphere(m), Atm_AD, Atmosphere_AD(m) )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error adding ADJOINT extra layers to profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF

    END DO Profile_Loop


    ! Clean up
    CALL CRTM_Predictor_Destroy( Predictor )
    CALL CRTM_Predictor_Destroy( Predictor_AD )
    CALL CRTM_AtmOptics_Destroy( AtmOptics )
    CALL CRTM_AtmOptics_Destroy( AtmOptics_AD )
    CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear )
    CALL CRTM_AtmOptics_Destroy( AtmOptics_Clear_AD )
    CALL CRTM_SfcOptics_Destroy( SfcOptics )
    CALL CRTM_SfcOptics_Destroy( SfcOptics_AD )
    CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear )
    CALL CRTM_SfcOptics_Destroy( SfcOptics_Clear_AD )
    CALL CRTM_Atmosphere_Destroy( Atm )
    CALL CRTM_Atmosphere_Destroy( Atm_AD )
    CALL CRTM_Atmosphere_Destroy( Atm_Clear )
    CALL CRTM_Atmosphere_Destroy( Atm_Clear_AD )
    ! ...Internal variables
    CALL AOvar_Destroy( AOvar )
    CALL CSvar_Destroy( CSvar )
    CALL ASvar_Destroy( ASvar )
    CALL RTV_Destroy( RTV )


CONTAINS


    ! ----------------------------------------------------------------
    ! Local subroutine to post-process the FORWARD radiance, as it is
    ! the same for all-sky and fractional clear-sky cases.
    !
    !   1. Apply non-LTE correction to radiance
    !   2. Convert radiance to brightness temperature
    !   3. Apply antenna correction to brightness temperature
    ! ----------------------------------------------------------------

    SUBROUTINE Post_Process_RTSolution(rts)
      TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: rts

      ! Compute non-LTE correction to radiance if required
      IF ( Opt%Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
        CALL Compute_NLTE_Correction( &
               SC(SensorIndex)%NC, &  ! Input
               ChannelIndex      , &  ! Input
               NLTE_Predictor    , &  ! Input
               rts%Radiance        )  ! In/Output
      END IF
      ! Convert the radiance to brightness temperature
      CALL CRTM_Planck_Temperature( &
             SensorIndex               , & ! Input
             ChannelIndex              , & ! Input
             rts%Radiance              , & ! Input
             rts%Brightness_Temperature  ) ! Output
      ! Compute Antenna correction to brightness temperature if required
      IF ( compute_antenna_correction ) THEN
        CALL CRTM_Compute_AntCorr( &
               GeometryInfo, &  ! Input
               SensorIndex , &  ! Input
               ChannelIndex, &  ! Input
               rts           )  ! Output
      END IF

    END SUBROUTINE Post_Process_RTSolution


    ! ----------------------------------------------------------------
    ! Local subroutine to pre-process the ADJOINT radiance, as it is
    ! the same for all-sky and fractional clear-sky cases.
    !
    !   1. Apply adjoint antenna correction to brightness temperatures
    !   2. Convert adjoint radiances to brightness temperatures
    !   3. Apply adjoint non-LTE correction to radiances
    ! ----------------------------------------------------------------

    SUBROUTINE Pre_Process_RTSolution_AD(rts, rts_AD)
      TYPE(CRTM_RTSolution_type), INTENT(IN OUT) :: rts, rts_AD

      ! Compute adjoint antenna correction to brightness temperature if required
      IF ( compute_antenna_correction ) THEN
        CALL CRTM_Compute_AntCorr_AD( &
               GeometryInfo, &  ! Input
               SensorIndex , &  ! Input
               ChannelIndex, &  ! Input
               rts_AD        )  ! Output
      END IF
      ! Compute the Planck temperature adjoint
      CALL CRTM_Planck_Temperature_AD( &
             SensorIndex                  , & ! Input
             ChannelIndex                 , & ! Input
             rts%Radiance                 , & ! Input
             rts_AD%Brightness_Temperature, & ! Input
             rts_AD%Radiance                ) ! Output
      rts_AD%Brightness_Temperature = ZERO
      ! Compute non-LTE correction adjoint if required
      IF ( Opt%Apply_NLTE_Correction .AND. NLTE_Predictor_IsActive(NLTE_Predictor) ) THEN
        CALL Compute_NLTE_Correction_AD( &
               SC(SensorIndex)%NC, &  ! Input
               ChannelIndex      , &  ! Input
               rts_AD%Radiance   , &  ! Input
               NLTE_Predictor_AD   )  ! Output
      END IF

    END SUBROUTINE Pre_Process_RTSolution_AD

  END FUNCTION CRTM_Adjoint


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Adjoint_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Adjoint_Version( Id )
!
! OUTPUTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Adjoint_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Adjoint_Version

END MODULE CRTM_Adjoint_Module
