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
                                        MAX_SOURCE_ZENITH_ANGLE
  USE CRTM_SpcCoeff,              ONLY: SC, VISIBLE_SENSOR
  USE CRTM_Atmosphere_Define,     ONLY: CRTM_Atmosphere_type, &
                                        CRTM_Atmosphere_Destroy, &
                                        CRTM_Atmosphere_IsValid, &
                                        CRTM_Atmosphere_AddLayerCopy
  USE CRTM_Surface_Define,        ONLY: CRTM_Surface_type, &
                                        CRTM_Surface_IsValid
  USE CRTM_Geometry_Define,       ONLY: CRTM_Geometry_type, &
                                        CRTM_Geometry_IsValid
  USE CRTM_ChannelInfo_Define,    ONLY: CRTM_ChannelInfo_type
  USE CRTM_Options_Define,        ONLY: CRTM_Options_type, &
                                        CRTM_Options_IsValid
  USE CRTM_Atmosphere,            ONLY: CRTM_Atmosphere_AddLayers, &
                                        CRTM_Atmosphere_AddLayers_AD
  USE CRTM_GeometryInfo_Define,   ONLY: CRTM_GeometryInfo_type, &
                                        CRTM_GeometryInfo_SetValue, &
                                        CRTM_GeometryInfo_GetValue
  USE CRTM_GeometryInfo,          ONLY: CRTM_GeometryInfo_Compute
  USE CRTM_AtmAbsorption,         ONLY: CRTM_AAVariables_type        , &
                                        CRTM_Compute_AtmAbsorption, &
                                        CRTM_Compute_AtmAbsorption_AD, &
                                        CRTM_Destroy_Predictor     , &
                                        CRTM_Allocate_Predictor    , &
                                        CRTM_Compute_Predictors    , &
                                        CRTM_Compute_Predictors_AD , &
                                        CRTM_Predictor_type        , &
                                        CRTM_APVariables_type    

! **** REPLACE
  USE CRTM_AtmScatter_Define,     ONLY: CRTM_AtmOptics_type     => CRTM_AtmScatter_type    , &
                                        CRTM_Allocate_AtmOptics => CRTM_Allocate_AtmScatter, &
                                        CRTM_Destroy_AtmOptics  => CRTM_Destroy_AtmScatter
! **** WITH THE FOLLOWING
!  USE CRTM_AtmOptics_Define,      ONLY: CRTM_AtmOptics_type    , &
!                                        CRTM_Allocate_AtmOptics, &
!                                        CRTM_Destroy_AtmOptics 
! ****

  USE CRTM_AerosolScatter,        ONLY: CRTM_ASVariables_type         , &
                                        CRTM_Compute_AerosolScatter   , &
                                        CRTM_Compute_AerosolScatter_AD
  USE CRTM_CloudScatter,          ONLY: CRTM_CSVariables_type       , &
                                        CRTM_Compute_CloudScatter   , &
                                        CRTM_Compute_CloudScatter_AD
  USE CRTM_AtmOptics,             ONLY: CRTM_AOVariables_type    , &
                                        CRTM_Combine_AtmOptics   , &
                                        CRTM_Combine_AtmOptics_AD
  USE CRTM_SfcOptics,             ONLY: CRTM_SfcOptics_type     , &
                                        CRTM_Allocate_SfcOptics , &
                                        CRTM_Destroy_SfcOptics  , &
                                        CRTM_Compute_SurfaceT   , &
                                        CRTM_Compute_SurfaceT_AD
  USE CRTM_RTSolution,            ONLY: CRTM_RTSolution_type      , &
                                        CRTM_Compute_nStreams     , &
                                        CRTM_Compute_RTSolution   , &
                                        CRTM_Compute_RTSolution_AD
  USE RTV_Define,                 ONLY: RTV_type      , &
                                        RTV_Associated, &
                                        RTV_Destroy   , &
                                        RTV_Create
  USE CRTM_AntCorr,               ONLY: CRTM_Compute_AntCorr, &
                                        CRTM_Compute_AntCorr_AD
  USE CRTM_MoleculeScatter,       ONLY: CRTM_Compute_MoleculeScatter, &
                                        CRTM_Compute_MoleculeScatter_AD
  USE CRTM_AncillaryInput_Define, ONLY: CRTM_AncillaryInput_type

  USE CRTM_CloudCoeff,            ONLY: CRTM_CloudCoeff_IsLoaded
  USE CRTM_AerosolCoeff,          ONLY: CRTM_AerosolCoeff_IsLoaded

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
  '$Id: CRTM_Adjoint_Module.f90 8133 2010-05-28 20:26:31Z paul.vandelst@noaa.gov $'


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
!       - The INTENT on the output RTSolution, Atmosphere_AD, and
!         Surface_AD arguments are IN OUT rather than just OUT. This is
!         necessary because the arguments should be defined upon input.
!         To prevent memory leaks, the IN OUT INTENT is a must.
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
    LOGICAL :: Check_Input
    LOGICAL :: User_Emissivity, User_Direct_Reflectivity
    LOGICAL :: User_AntCorr, Compute_AntCorr
    LOGICAL :: Atmosphere_Invalid, Surface_Invalid, Geometry_Invalid, Options_Invalid
    INTEGER :: iFOV
    INTEGER :: n, n_Sensors,  SensorIndex
    INTEGER :: l, n_Channels, ChannelIndex
    INTEGER :: m, n_Profiles
    INTEGER :: j, ln
    INTEGER :: n_Full_Streams, mth_Azi
    INTEGER :: AllocStatus(2), AllocStatus_AD(2)
    REAL(fp) :: Source_ZA
    REAL(fp) :: Wavenumber
    ! Local ancillary input structure
    TYPE(CRTM_AncillaryInput_type) :: AncillaryInput
    ! Local atmosphere structure for extra layering
    TYPE(CRTM_Atmosphere_type) :: Atm, Atm_AD
    ! Component variables
    TYPE(CRTM_GeometryInfo_type) :: GeometryInfo
    TYPE(CRTM_Predictor_type)    :: Predictor, Predictor_AD
    TYPE(CRTM_AtmOptics_type)    :: AtmOptics, AtmOptics_AD 
    TYPE(CRTM_SfcOPtics_type)    :: SfcOptics, SfcOptics_AD
    ! Component variable internals
    TYPE(CRTM_APVariables_type) :: APV  ! Predictor
    TYPE(CRTM_AAVariables_type) :: AAV  ! AtmAbsorption
    TYPE(CRTM_CSVariables_type) :: CSV  ! CloudScatter
    TYPE(CRTM_ASVariables_type) :: ASV  ! AerosolScatter
    TYPE(CRTM_AOVariables_type) :: AOV  ! AtmOptics
    TYPE(RTV_type) :: RTV  ! RTSolution

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! ----------------------------------------
    ! If no sensors or channels, simply return
    ! ----------------------------------------
    n_Sensors  = SIZE(ChannelInfo)
    n_Channels = SUM(ChannelInfo%n_Channels)
    IF ( n_Sensors == 0 .OR. n_Channels == 0 ) RETURN


    ! ---------------------------
    ! RTSolution arrays too small
    ! ---------------------------
    IF ( SIZE(RTSolution   ,DIM=1) < n_Channels .OR. &
         SIZE(RTSolution_AD,DIM=1) < n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Output RTSolution structure arrays too small (",i0," and ",i0,&
             &") to hold results for the number of requested channels (",i0,")")') &
             SIZE(RTSolution,DIM=1), SIZE(RTSolution_AD,DIM=1), n_Channels
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
      RETURN
    END IF


    ! ----------------------------
    ! Check the number of profiles
    ! ----------------------------
    ! Number of atmospheric profiles.
    n_Profiles = SIZE(Atmosphere)

    ! Check the profile dimensionality
    ! of the other mandatory arguments
    IF ( SIZE(Surface)             /= n_Profiles .OR. &
         SIZE(RTSolution_AD,DIM=2) /= n_Profiles .OR. &
         SIZE(Geometry)            /= n_Profiles .OR. &
         SIZE(Atmosphere_AD)       /= n_Profiles .OR. &
         SIZE(Surface_AD)          /= n_Profiles .OR. &
         SIZE(RTSolution,   DIM=2) /= n_Profiles      ) THEN
      Error_Status = FAILURE
      Message = 'Inconsistent profile dimensionality for input arguments.'
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
      RETURN
    END IF

    ! Check the profile dimensionality
    ! of the other optional arguments
    Options_Present = .FALSE.
    IF ( PRESENT(Options) ) THEN
      Options_Present = .TRUE.
      IF ( SIZE(Options) /= n_Profiles ) THEN
        Error_Status = FAILURE
        Message = 'Inconsistent profile dimensionality for Options optional input argument.'
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF
    END IF

 

    !#--------------------------------------------------------------------------#
    !#                           -- PROFILE LOOP --                             #
    !#--------------------------------------------------------------------------#
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
         CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )                      
         RETURN                                                                                 
      END IF

      ! Copy over forward "non-variable" inputs to adjoint outputs
      ! ...Atmosphere
      Atmosphere_AD(m)%Climatology = Atmosphere(m)%Climatology
      DO j = 1, Atmosphere(m)%n_Absorbers
        Atmosphere_AD(m)%Absorber_ID(j)    = Atmosphere(m)%Absorber_ID(j)
        Atmosphere_AD(m)%Absorber_Units(j) = Atmosphere(m)%Absorber_Units(j)
      END DO
      ! ...Surface
      Surface_AD(m)%Land_Coverage  = Surface(m)%Land_Coverage 
      Surface_AD(m)%Water_Coverage = Surface(m)%Water_Coverage
      Surface_AD(m)%Snow_Coverage  = Surface(m)%Snow_Coverage 
      Surface_AD(m)%Ice_Coverage   = Surface(m)%Ice_Coverage  
      Surface_AD(m)%Land_Type  = Surface(m)%Land_Type 
      Surface_AD(m)%Water_Type = Surface(m)%Water_Type
      Surface_AD(m)%Snow_Type  = Surface(m)%Snow_Type 
      Surface_AD(m)%Ice_Type   = Surface(m)%Ice_Type  


      ! Check the optional Options structure argument
      ! ...Specify default actions
      Check_Input     = .TRUE.
      User_Emissivity = .FALSE.
      User_AntCorr    = .FALSE.
      ! ...Check the Options argument
      IF (Options_Present) THEN
        ! Override input checker with option
        Check_Input = Options(m)%Check_Input
        ! Check if the supplied emissivity should be used
        User_Emissivity = Options(m)%Use_Emissivity
        IF ( Options(m)%Use_Emissivity ) THEN
          ! Are the channel dimensions consistent
          IF ( Options(m)%n_Channels < n_Channels ) THEN
            Error_Status = FAILURE
            WRITE( Message,'( "Input Options channel dimension (", i0, ") is less ", &
                   &"than the number of requested channels (",i0, ")" )' ) &
                   Options(m)%n_Channels, n_Channels
            CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
            RETURN
          END IF
          ! Check if the supplied direct reflectivity should be used
          User_Direct_Reflectivity = Options(m)%Use_Direct_Reflectivity
        END IF
        ! Check if antenna correction should be attempted
        User_AntCorr = Options(m)%Use_Antenna_Correction
        ! Copy over ancillary input
        AncillaryInput%SSU    = Options(m)%SSU
        AncillaryInput%Zeeman = Options(m)%Zeeman
      END IF


      ! Check the input data if required
      IF ( Check_Input ) THEN
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


      ! ----------------------------------------------
      ! Add extra layers to current atmosphere profile
      ! if necessary to handle upper atmosphere
      ! ----------------------------------------------
      ! ...Forward model
      Error_Status = CRTM_Atmosphere_AddLayers( Atmosphere(m), &  ! Input
                                                Atm            )  ! Output
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error adding FWD extra layers to profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF
      ! ...Check the total number of Atm layers
      IF ( Atm%n_Layers > MAX_N_LAYERS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Added layers [",i0,"] cause total [",i0,"] to exceed the ",&
               &"maximum allowed [",i0,"] for profile #",i0)' ) &
               Atm%n_Added_Layers, Atm%n_Layers, MAX_N_LAYERS, m
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF
      ! ...Similarly extend a copy of the input adjoint atmosphere
      Atm_AD = CRTM_Atmosphere_AddLayerCopy( Atmosphere_AD(m), Atm%n_Added_Layers )
      

      ! -----------------------------------------------------
      ! Allocate all local sensor independent data structures
      ! -----------------------------------------------------
      ! The AtmOptics structure
      AllocStatus(1) = CRTM_Allocate_AtmOptics( Atm%n_Layers        , &  ! Input
                                                MAX_N_LEGENDRE_TERMS, &  ! Input
                                                MAX_N_PHASE_ELEMENTS, &  ! Input
                                                AtmOptics             )  ! Output
      AllocStatus_AD(1) = CRTM_Allocate_AtmOptics( Atm%n_Layers        , &  ! Input
                                                   MAX_N_LEGENDRE_TERMS, &  ! Input
                                                   MAX_N_PHASE_ELEMENTS, &  ! Input
                                                   AtmOptics_AD          )  ! Output
      ! The SfcOptics structure
      AllocStatus(2) = CRTM_Allocate_SfcOptics( MAX_N_ANGLES, &  ! Input
                                                MAX_N_STOKES, &  ! Input
                                                SfcOptics     )  ! Output
      AllocStatus_AD(2) = CRTM_Allocate_SfcOptics( MAX_N_ANGLES, &  ! Input
                                                   MAX_N_STOKES, &  ! Input
                                                   SfcOptics_AD  )  ! Output
      IF ( ANY(AllocStatus    /= SUCCESS) .OR. &
           ANY(AllocStatus_AD /= SUCCESS) ) THEN
        Error_Status=FAILURE
        WRITE( Message,'("Error allocating local sensor independent data structures for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF


      ! --------------------------
      ! Preprocess some input data
      ! --------------------------
      ! Average surface skin temperature for multi-surface types
      CALL CRTM_Compute_SurfaceT( Surface(m), SfcOptics )


      ! Initialise channel counter for sensor(n)/channel(l) count
      ln = 0
    
      ! -----------
      ! Sensor loop
      ! -----------
      Sensor_Loop: DO n = 1, n_Sensors

        ! Shorter name
        SensorIndex = ChannelInfo(n)%Sensor_Index

        ! Check if antenna correction to be applied for current sensor
        IF ( User_AntCorr .AND. SC(SensorIndex)%AC_Present .AND. iFOV /= 0 ) THEN
          Compute_AntCorr = .TRUE.
        ELSE
          Compute_AntCorr = .FALSE.
        END IF


        ! Allocate the predictor structures
        AllocStatus(1) = CRTM_Allocate_Predictor( SensorIndex , &  ! Input
                                                  Atm%n_Layers, &  ! Input
                                                  Predictor   , &  ! Output
                                                  SaveFWV = 1   )  ! Optional input
        AllocStatus_AD(1) = CRTM_Allocate_Predictor( SensorIndex , &  ! Input
                                                     Atm%n_Layers, &  ! Input
                                                     Predictor_AD  )  ! Output
        IF ( AllocStatus(1) /= SUCCESS .OR. AllocStatus_AD(1) /= SUCCESS ) THEN
          Error_Status=FAILURE
          WRITE( Message,'("Error allocating predictor structures for profile #",i0, &
                 &" and ",a," sensor.")' ) m, SC(SensorIndex)%Sensor_Id
          CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
          RETURN
        END IF
        ! Allocate the RTV structure if necessary
        IF( Atm%n_Clouds   > 0 .OR. &
            Atm%n_Aerosols > 0 .OR. &
            SC(SensorIndex)%Sensor_Type == VISIBLE_SENSOR ) THEN
          CALL RTV_Create( RTV, MAX_N_ANGLES, MAX_N_LEGENDRE_TERMS, Atm%n_Layers )
          IF ( .NOT. RTV_Associated(RTV) ) THEN
            Error_Status=FAILURE
            WRITE( Message,'("Error allocating RTV structure for profile #",i0, &
                   &" and ",a," sensor.")' ) m, TRIM(SC(SensorIndex)%Sensor_Id)
            CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
            RETURN
          END IF
        END IF

 
        ! ------------------------------------------
        ! Compute predictors for AtmAbsorption calcs
        ! ------------------------------------------
        CALL CRTM_Compute_Predictors( SensorIndex   , &  ! Input
                                      Atm           , &  ! Input
                                      GeometryInfo  , &  ! Input
                                      AncillaryInput, &  ! Input
                                      Predictor     , &  ! Output
                                      APV             )  ! Internal variable output


        ! ------------
        ! Channel loop
        ! ------------
        Channel_Loop: DO l = 1, ChannelInfo(n)%n_Channels

          ! Shorter name
          ChannelIndex = ChannelInfo(n)%Channel_Index(l)

          ! Increment channel counter
          ln = ln + 1
          
          ! Initialisations
          RTSolution(ln,m)%Radiance       = ZERO
          AtmOptics%Optical_Depth         = ZERO
          AtmOptics%Phase_Coefficient     = ZERO
          AtmOptics%Delta_Truncation      = ZERO
          AtmOptics%Single_Scatter_Albedo = ZERO

          ! ---------------------------------------------------------------
          ! Determine the number of streams (n_Full_Streams) in up+downward
          ! directions. Currently, n_Full_Streams is determined from the
          ! cloud parameters only. It will also use the aerosol parameters 
          ! when aerosol scattering is included.
          ! ---------------------------------------------------------------
          n_Full_Streams = CRTM_Compute_nStreams( Atm             , &  ! Input
                                                  SensorIndex     , &  ! Input
                                                  ChannelIndex    , &  ! Input
                                                  RTSolution(ln,m)  )  ! Output
          ! Transfer the number of streams
          ! to all the scattering structures
          AtmOptics%n_Legendre_Terms    = n_Full_Streams
          AtmOptics_AD%n_Legendre_Terms = n_Full_Streams
          
          
          ! --------------------------
          ! Compute the gas absorption
          ! --------------------------
          CALL CRTM_Compute_AtmAbsorption( SensorIndex   , &  ! Input
                                           ChannelIndex  , &  ! Input
                                           AncillaryInput, &  ! Input
                                           Predictor     , &  ! Input
                                           AtmOptics     , &  ! Output
                                           AAV             )  ! Internal variable output


          ! -------------------------------------------
          ! Compute the molecular scattering properties
          ! -------------------------------------------
           ! Solar radiation
          IF( SC(SensorIndex)%Solar_Irradiance(ChannelIndex) > ZERO .AND. &
              Source_ZA < MAX_SOURCE_ZENITH_ANGLE) THEN
             RTV%Solar_Flag_true = .TRUE.
          END IF
          
          IF( SC(SensorIndex)%Sensor_Type == VISIBLE_SENSOR .and. RTV%Solar_Flag_true ) THEN 
            RTV%Visible_Flag_true = .TRUE.
            ! Rayleigh phase function has 0, 1, 2 components.
            IF( AtmOptics%n_Legendre_Terms < 4 ) THEN
              AtmOptics%n_Legendre_Terms = 4
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
              CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
              RETURN
            END IF
          ELSE
            RTV%Visible_Flag_true = .FALSE.
            RTV%n_Azi = 0
          END IF        
          
          
          ! -----------------------------------------------------------
          ! Compute the cloud particle absorption/scattering properties
          ! -----------------------------------------------------------
          IF( Atm%n_Clouds > 0 ) THEN
            Error_Status = CRTM_Compute_CloudScatter( Atm         , &  ! Input
                                                      SensorIndex , &  ! Input
                                                      ChannelIndex, &  ! Input
                                                      AtmOptics   , &  ! Output
                                                      CSV           )  ! Internal variable output
            IF (Error_Status /= SUCCESS) THEN
              WRITE( Message,'("Error computing CloudScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
              RETURN
            END IF
          END IF


          ! ----------------------------------------------------
          ! Compute the aerosol absorption/scattering properties
          ! ----------------------------------------------------
          IF ( Atm%n_Aerosols > 0 ) THEN
            Error_Status = CRTM_Compute_AerosolScatter( Atm         , &  ! Input
                                                        SensorIndex , &  ! Input
                                                        ChannelIndex, &  ! Input
                                                        AtmOptics   , &  ! In/Output
                                                        ASV           )  ! Internal variable output
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing AerosolScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
              RETURN
            END IF
          END IF


          ! ---------------------------------------------------
          ! Compute the combined atmospheric optical properties
          ! ---------------------------------------------------
          CALL CRTM_Combine_AtmOptics( AtmOptics     , & ! Output
                                       AOV             ) ! Internal variable output


          ! ------------------------------------
          ! Fill the SfcOptics structure for the
          ! optional emissivity input case.
          ! ------------------------------------
          ! Indicate SfcOptics ARE to be computed
          SfcOptics%Compute_Switch = SET
          ! Change SfcOptics emissivity/reflectivity
          ! contents/computation status
          IF ( User_Emissivity ) THEN
            SfcOptics%Compute_Switch  = NOT_SET
            SfcOptics%Emissivity(1,1)       = Options(m)%Emissivity(ln)
            SfcOptics%Reflectivity(1,1,1,1) = ONE - Options(m)%Emissivity(ln)
            IF ( User_Direct_Reflectivity ) THEN
              SfcOptics%Direct_Reflectivity(1,1) = Options(m)%Direct_Reflectivity(ln)
            ELSE
              SfcOptics%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
            END IF

          END IF

          
        ! ------------
        ! Fourier component loop for azimuth angles,
        ! mth_Azi = 0 is for an azimuth-averaged value,
        ! for example IR and MW thermal radiation
        ! ------------

          RTSolution(ln,m)%Radiance = ZERO
          AtmOptics_AD%Optical_Depth         = ZERO
          AtmOptics_AD%Single_Scatter_Albedo = ZERO
          IF ( AtmOptics%n_Legendre_Terms > 0 ) THEN
            AtmOptics_AD%Phase_Coefficient = ZERO
            AtmOptics_AD%Delta_Truncation  = ZERO
          END IF

        Azi_Fourier_Loop: DO mth_Azi = 0, RTV%n_Azi
          RTV%mth_Azi = mth_Azi
          SfcOptics%mth_Azi = mth_Azi
                    
          ! ------------------------------------
          ! Solve the radiative transfer problem
          ! ------------------------------------
          Error_Status = CRTM_Compute_RTSolution( Atm             , &  ! Input
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
            CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
            RETURN
          END IF


          ! --------------------------------------
          ! Compute Antenna correction if required
          ! --------------------------------------
          IF ( Compute_AntCorr ) THEN
            CALL CRTM_Compute_AntCorr( GeometryInfo    , &  ! Input
                                       SensorIndex     , &  ! Input
                                       ChannelIndex    , &  ! Input
                                       RTSolution(ln,m)  )  ! Output
            CALL CRTM_Compute_AntCorr_AD( GeometryInfo       , &  ! Input
                                          SensorIndex        , &  ! Input
                                          ChannelIndex       , &  ! Input
                                          RTSolution_AD(ln,m)  )  ! Output
          END IF


          ! -------------------------------------
          ! The adjoint of the radiative transfer
          ! -------------------------------------
          Error_Status = CRTM_Compute_RTSolution_AD( Atm                , &  ! FWD Input
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
            CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
            RETURN
          END IF
 

        END DO Azi_Fourier_Loop
        
          ! ------------------------------------------------------------------
          ! Compute the adjoint of the combined atmospheric optical properties
          ! ------------------------------------------------------------------
          CALL CRTM_Combine_AtmOptics_AD( AtmOptics        , &  ! FWD Input
                                          AtmOptics_AD     , &  ! AD  Input
                                          AOV                )  ! Internal variable input


          ! ------------------------------------------------------------
          ! Compute the adjoint aerosol absorption/scattering properties
          ! ------------------------------------------------------------
          IF ( Atm%n_Aerosols > 0 ) THEN
            Error_Status = CRTM_Compute_AerosolScatter_AD( Atm         , &  ! FWD Input
                                                           AtmOptics   , &  ! FWD Input
                                                           AtmOptics_AD, &  ! AD  Input
                                                           SensorIndex , &  ! Input
                                                           ChannelIndex, &  ! Input
                                                           Atm_AD      , &  ! AD  Output
                                                           ASV           )  ! Internal variable input
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing AerosolScatter_AD for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
              RETURN
            END IF
          END IF


          ! ----------------------------------------------------------
          ! Compute the adjoint cloud absorption/scattering properties
          ! ----------------------------------------------------------
          IF ( Atm%n_Clouds > 0 ) THEN
            Error_Status = CRTM_Compute_CloudScatter_AD( Atm         , &  ! FWD Input
                                                         AtmOptics   , &  ! FWD Input
                                                         AtmOptics_AD, &  ! AD  Input
                                                         SensorIndex , &  ! Input
                                                         ChannelIndex, &  ! Input
                                                         Atm_AD      , &  ! AD  Output
                                                         CSV           )  ! Internal variable input
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing CloudScatter_AD for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
              RETURN
            END IF
          END IF
          
          ! ---------------------------------------------------
          ! Compute the adjoint molecular scattering properties
          ! ---------------------------------------------------
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
              CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
              RETURN
            END IF
          END IF         


          ! --------------------------------------
          ! Compute the adjoint gaseous absorption
          ! --------------------------------------
          CALL CRTM_Compute_AtmAbsorption_AD( SensorIndex     , &  ! Input
                                              ChannelIndex    , &  ! Input
                                              Predictor       , &  ! FWD Input
                                              AtmOptics_AD    , &  ! AD  Input
                                              Predictor_AD    , &  ! AD  Output
                                              AAV               )  ! Internal variable input
        END DO Channel_Loop


        ! -------------------------------------
        ! Adjoint of the predictor calculations
        ! -------------------------------------
        CALL CRTM_Compute_Predictors_AD( SensorIndex   , &  ! Input
                                         Atm           , &  ! FWD Input
                                         Predictor     , &  ! FWD Input
                                         Predictor_AD  , &  ! AD  Input
                                         GeometryInfo  , &  ! Input
                                         AncillaryInput, &  ! Input
                                         Atm_AD        , &  ! AD  Output
                                         APV             )  ! Internal variable input


        ! Deallocate local sensor dependent data structures
        ! ...RTV structure
        IF ( RTV_Associated(RTV) ) CALL RTV_Destroy(RTV)
        ! ...Predictor structures
        AllocStatus(1)    = CRTM_Destroy_Predictor( SensorIndex, Predictor ) 
        AllocStatus_AD(1) = CRTM_Destroy_Predictor( SensorIndex, Predictor_AD )
        IF ( AllocStatus(1) /= SUCCESS .OR. AllocStatus_AD(1) /= SUCCESS ) THEN
          WRITE( Message,'("Error deallocating predictor structures for profile #",i0, &
                 &" and ",a," sensor.")' ) m, SC(SensorIndex)%Sensor_Id
          CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
          RETURN
        END IF

      END DO Sensor_Loop

                                        
      ! ---------------------------
      ! Postprocess some input data
      ! ---------------------------
      ! Adjoint of average surface skin temperature for multi-surface types
      CALL CRTM_Compute_SurfaceT_AD( Surface(m), SfcOptics_AD, Surface_AD(m) )


      ! ----------------------------------------
      ! Adjoint of the atmosphere layer addition
      ! ----------------------------------------
      Error_Status = CRTM_Atmosphere_AddLayers_AD( Atmosphere(m)   , &  ! Input
                                                   Atm_AD          , &  ! Input
                                                   Atmosphere_AD(m)  )  ! Output
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error adding AD extra layers to profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN
      END IF
      

      ! --------------------------------------------------- 
      ! Deallocate local sensor independent data structures   
      ! ---------------------------------------------------
      AllocStatus_AD(2)=CRTM_Destroy_SfcOptics( SfcOptics_AD )
      AllocStatus(2)   =CRTM_Destroy_SfcOptics( SfcOptics )
      AllocStatus_AD(1)=CRTM_Destroy_AtmOptics( AtmOptics_AD )
      AllocStatus(1)   =CRTM_Destroy_AtmOptics( AtmOptics )
      IF ( ANY(AllocStatus /= SUCCESS ) .OR. ANY(AllocStatus_AD /= SUCCESS ) ) THEN                                              
        Error_Status = FAILURE                                                              
        WRITE( Message,'("Error deallocating local sensor independent data structures for profile #",i0)' ) m  
        CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
        RETURN                                                                              
      END IF                                                                                

    END DO Profile_Loop


    ! ---------------------------------
    ! Destroy "extra layers" structures
    ! ---------------------------------
    CALL CRTM_Atmosphere_Destroy( Atm_AD )
    CALL CRTM_Atmosphere_Destroy( Atm )

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
