!
! CRTM_AOD_Module
!
! Module containing the CRTM Aerosol Optical Depth (AOD) at nadir
! functions.
!
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu, 29-Jun-2010
!                       Quanhua.Liu@noaa.gov
!

MODULE CRTM_AOD_Module


  ! ------------
  ! Module usage
  ! ------------
  USE Message_Handler,          ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,          ONLY: ZERO, &
                                      MAX_N_PHASE_ELEMENTS, &
                                      MAX_N_LEGENDRE_TERMS
  USE CRTM_Atmosphere_Define,   ONLY: CRTM_Atmosphere_type, &
                                      CRTM_Atmosphere_IsValid
  USE CRTM_ChannelInfo_Define,  ONLY: CRTM_ChannelInfo_type, &
                                      CRTM_ChannelInfo_n_Channels
  USE CRTM_Options_Define,      ONLY: CRTM_Options_type
  USE CRTM_AtmOptics_Define,    ONLY: CRTM_AtmOptics_type      , &
                                      CRTM_AtmOptics_Associated, &
                                      CRTM_AtmOptics_Create    , &
                                      CRTM_AtmOptics_Destroy   , &
                                      CRTM_AtmOptics_Zero
  USE CRTM_AerosolScatter,      ONLY: CRTM_Compute_AerosolScatter, &
                                      CRTM_Compute_AerosolScatter_TL, &
                                      CRTM_Compute_AerosolScatter_AD
  USE CRTM_RTSolution_Define,   ONLY: CRTM_RTSolution_type, &
                                      CRTM_RTSolution_Associated
  USE CRTM_AerosolCoeff,        ONLY: CRTM_AerosolCoeff_IsLoaded

  ! Internal variable definition modules
  ! ...AerosolScatter
  USE ASvar_Define, ONLY: ASvar_type, &
                          ASvar_Associated, &
                          ASvar_Destroy   , &
                          ASvar_Create    


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
  PUBLIC :: CRTM_AOD
  PUBLIC :: CRTM_AOD_TL
  PUBLIC :: CRTM_AOD_AD
  PUBLIC :: CRTM_AOD_K
  PUBLIC :: CRTM_AOD_Version


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_AOD_Module.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
 

CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AOD
!
! PURPOSE:
!       Function that calculates layer total optical depth profile at nadir.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AOD( Atmosphere       , &
!                                ChannelInfo      , &
!                                RTSolution       , &
!                                Options = Options  )
!
! INPUTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Rank-1 (n_Profiles)
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
! OUTPUTS:
!       RTSolution:     Structure containing the layer aerosol optical
!                       profile for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Options:        Options structure containing the optional arguments
!                       for the CRTM.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
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
!       - Many of the components of the Options optional input structure
!         are not used in this function. Consult the CRTM User Guide for
!         which Options components are usable for AOD calculations.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_AOD( &
    Atmosphere , &  ! Input, M
    ChannelInfo, &  ! Input, M
    RTSolution , &  ! Output, L x M 
    Options    ) &  ! Optional input, M
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),        INTENT(IN)     :: Atmosphere(:)     ! M
    TYPE(CRTM_ChannelInfo_type),       INTENT(IN)     :: ChannelInfo(:)    ! n_Sensors 
    TYPE(CRTM_RTSolution_type),        INTENT(IN OUT) :: RTSolution(:,:)   ! L x M
    TYPE(CRTM_Options_type), OPTIONAL, INTENT(IN)     :: Options(:)        ! M
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AOD'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Options_Present
    LOGICAL :: Check_Input
    INTEGER :: n, n_Sensors,  SensorIndex
    INTEGER :: l, n_Channels, ChannelIndex
    INTEGER :: m, n_Profiles
    INTEGER :: ln
    ! Component variables
    TYPE(CRTM_AtmOptics_type) :: AtmOptics 
    TYPE(ASVar_type) :: ASvar


    ! ------
    ! SET UP
    ! ------
    Error_Status = SUCCESS


    ! If no sensors or channels, simply return
    n_Sensors  = SIZE(ChannelInfo)
    n_Channels = SUM(CRTM_ChannelInfo_n_Channels(ChannelInfo))
    IF ( n_Sensors == 0 .OR. n_Channels == 0 ) RETURN


    ! Check the number of channels
    IF ( SIZE(RTSolution,DIM=1) < n_Channels ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Output RTSolution structure array too small (",i0,&
             &") to hold results for the number of requested channels (",i0,")")') &
             SIZE(RTSolution,DIM=1), n_Channels
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! Check the number of profiles
    ! ...Number of atmospheric profiles.
    n_Profiles = SIZE(Atmosphere)
    ! ...Check the profile dimensionality of the other mandatory arguments
    IF ( SIZE(RTSolution,DIM=2) /= n_Profiles ) THEN
      Error_Status = FAILURE
      Message = 'Inconsistent profile dimensionality for RTSolution argument.'
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF
    ! ...Check the profile dimensionality of the other optional arguments
    Options_Present = PRESENT(Options)
    IF ( Options_Present ) THEN
      IF ( SIZE(Options) /= n_Profiles ) THEN
        Error_Status = FAILURE
        Message = 'Inconsistent profile dimensionality for Options optional input argument.'
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
    END IF

    
    ! Check the RTSolution structure has been allocated
    IF ( ANY(.NOT. CRTM_RTSolution_Associated(RTSolution)) ) THEN
      Error_Status = FAILURE
      Message = 'RTSolution output structure components have not been allocated'
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! ------------
    ! PROFILE LOOP
    ! ------------
    Profile_Loop: DO m = 1, n_Profiles
    
    
      ! Check the aerosol coeff. data for cases with aerosols
      IF( Atmosphere(m)%n_Aerosols > 0 .AND. .NOT. CRTM_AerosolCoeff_IsLoaded() )THEN
         Error_Status = FAILURE                                                                 
         WRITE( Message,'("The AerosolCoeff data must be loaded (with CRTM_Init routine) ", &   
                &"for the aerosol case profile #",i0)' ) m                                      
         CALL Display_Message( ROUTINE_NAME, Message, Error_Status )                      
         RETURN                                                                                 
      END IF

      
      ! Check the optional Options structure argument
      Check_Input = .TRUE.
      IF (Options_Present) THEN
        Check_Input = Options(m)%Check_Input
      END IF


      ! Check the input atmosphere if required
      IF ( Check_Input ) THEN
        IF ( .NOT. CRTM_Atmosphere_IsValid( Atmosphere(m) ) ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Input data check failed for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
      END IF


      ! Check the RTSolution layer dimension
      IF ( ANY(RTSolution(:,m)%n_Layers < Atmosphere(m)%n_Layers) ) THEN
        Error_Status=FAILURE
        WRITE( Message,'("Number of RTSolution layers < Atmosphere for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN      
      END IF
      
      
      ! Allocate AtmOptics based on Atmosphere dimension
      CALL CRTM_AtmOptics_Create( AtmOptics, &
                                  Atmosphere(m)%n_Layers, &
                                  MAX_N_LEGENDRE_TERMS, &
                                  MAX_N_PHASE_ELEMENTS  )
      IF ( .NOT. CRTM_AtmOptics_Associated( Atmoptics ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error allocating AtmOptics data structure for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
      ! ...Set default number of streams
      AtmOptics%n_Legendre_Terms = 4


      ! Allocate the aerosol scattering internal variable if necessary
      IF ( Atmosphere(m)%n_Aerosols > 0 ) THEN
        CALL ASvar_Create( ASvar, &
                           MAX_N_LEGENDRE_TERMS    , &
                           MAX_N_PHASE_ELEMENTS    , &
                           Atmosphere(m)%n_Layers  , &
                           Atmosphere(m)%n_Aerosols  )
      END IF


      ! -----------
      ! SENSOR LOOP
      ! -----------
      ! Initialise channel counter for channel(l)/sensor(n) count
      ln = 0
      
      Sensor_Loop: DO n = 1, n_Sensors

        ! Shorter name
        SensorIndex = ChannelInfo(n)%Sensor_Index


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

          
          ! Initialisations
          CALL CRTM_AtmOptics_Zero( AtmOptics )


          ! Compute the aerosol absorption/scattering properties
          IF ( Atmosphere(m)%n_Aerosols > 0 ) THEN
            Error_Status = CRTM_Compute_AerosolScatter( Atmosphere(m), &  ! Input
                                                        SensorIndex  , &  ! Input
                                                        ChannelIndex , &  ! Input
                                                        AtmOptics    , &  ! In/Output
                                                        ASVar          )  ! Internal variable output
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error computing AerosolScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF


          ! Save the nadir optical depth
          RTSolution(ln,m)%Layer_Optical_Depth(1:Atmosphere(m)%n_Layers) = AtmOptics%Optical_Depth

        END DO Channel_Loop

      END DO Sensor_Loop
      

      ! Deallocate local sensor independent data structures
      CALL CRTM_AtmOptics_Destroy( AtmOptics )

    END DO Profile_Loop

  END FUNCTION CRTM_AOD


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AOD_TL
!
! PURPOSE:
!       Function that calculates tangent-linear layer total optical depth.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AOD_TL( Atmosphere       , &
!                                   Atmosphere_TL    , &
!                                   ChannelInfo      , &
!                                   RTSolution       , &
!                                   RTSolution_TL    , &
!                                   Options = Options  )
!
! INPUTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Rank-1 (n_Profiles)
!                       ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:  Structure containing the tangent-linear Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Same as input Atmosphere structure
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
! OUTPUTS:
!       RTSolution:     Structure containing the layer aerosol optical
!                       profile for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       RTSolution_TL:  Structure containing the tangent-linear aerosol
!                       optical depth profile for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Same as RTSolution output
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Options:        Options structure containing the optional arguments
!                       for the CRTM.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
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
!       - Many of the components of the Options optional input structure
!         are not used in this function. Consult the CRTM User Guide for
!         which Options components are usable for AOD calculations.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_AOD_TL( &
    Atmosphere   , &  ! Input, M
    Atmosphere_TL, &  ! Input, M
    ChannelInfo  , &  ! Input, M
    RTSolution   , &  ! Output, L x M 
    RTSolution_TL, &  ! Output, L x M 
    Options      ) &  ! Optional FWD input, M
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),        INTENT(IN)     :: Atmosphere(:)      ! M
    TYPE(CRTM_Atmosphere_type),        INTENT(IN)     :: Atmosphere_TL(:)   ! M
    TYPE(CRTM_ChannelInfo_type),       INTENT(IN)     :: ChannelInfo(:)     ! n_Sensors 
    TYPE(CRTM_RTSolution_type),        INTENT(IN OUT) :: RTSolution(:,:)    ! L x M
    TYPE(CRTM_RTSolution_type),        INTENT(IN OUT) :: RTSolution_TL(:,:) ! L x M
    TYPE(CRTM_Options_type), OPTIONAL, INTENT(IN)     :: Options(:)         ! M
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AOD_TL'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Options_Present
    LOGICAL :: Check_Input
    INTEGER :: Status_FWD, Status_TL
    INTEGER :: n, n_Sensors,  SensorIndex
    INTEGER :: l, n_Channels, ChannelIndex
    INTEGER :: m, n_Profiles
    INTEGER :: ln
    ! Component variables
    TYPE(CRTM_AtmOptics_type)   :: AtmOptics, AtmOptics_TL
    TYPE(ASVar_type) :: ASvar


    ! ------
    ! SET UP
    ! ------
    Error_Status = SUCCESS


    ! If no sensors or channels, simply return
    n_Sensors  = SIZE(ChannelInfo)
    n_Channels = SUM(CRTM_ChannelInfo_n_Channels(ChannelInfo))
    IF ( n_Sensors == 0 .OR. n_Channels == 0 ) RETURN


    ! Check the number of channels
    IF ( SIZE(RTSolution,   DIM=1) < n_Channels .OR. &
         SIZE(RTSolution_TL,DIM=1) < n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Output RTSolution structure arrays too small (",i0,&
             &") to hold results for the number of requested channels (",i0,")")') &
             SIZE(RTSolution,DIM=1), n_Channels
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! Check the number of profiles
    n_Profiles = SIZE(Atmosphere)
    ! ...Check the profile dimensionality of the other mandatory arguments
    IF ( SIZE(Atmosphere_TL)       /= n_Profiles .OR. &
         SIZE(RTSolution,   DIM=2) /= n_Profiles .OR. &
         SIZE(RTSolution_TL,DIM=2) /= n_Profiles      ) THEN
      Error_Status = FAILURE
      Message = 'Inconsistent profile dimensionality for input arguments.'
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
      RETURN
    END IF
    ! ...Check the profile dimensionality of the other optional arguments
    Options_Present = PRESENT(Options)
    IF ( Options_Present ) THEN
      IF ( SIZE(Options) /= n_Profiles ) THEN
        Error_Status = FAILURE
        Message = 'Inconsistent profile dimensionality for Options optional input argument.'
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
    END IF

    
    ! Check the RTSolution structures have been allocated
    IF ( ANY(.NOT. CRTM_RTSolution_Associated(RTSolution)) .OR. &
         ANY(.NOT. CRTM_RTSolution_Associated(RTSolution_TL)) ) THEN
      Error_Status = FAILURE
      Message = 'RTSolution output structure components have not been allocated'
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! ------------
    ! PROFILE LOOP
    ! ------------
    Profile_Loop: DO m = 1, n_Profiles
    
    
      ! Check the aerosol coeff. data for cases with aerosols
      IF( Atmosphere(m)%n_Aerosols > 0 .AND. .NOT. CRTM_AerosolCoeff_IsLoaded() )THEN
         Error_Status = FAILURE                                                                 
         WRITE( Message,'("The AerosolCoeff data must be loaded (with CRTM_Init routine) ", &   
                &"for the aerosol case profile #",i0)' ) m                                      
         CALL Display_Message( ROUTINE_NAME, Message, Error_Status )                      
         RETURN                                                                                 
      END IF

      
      ! Check the optional Options structure argument
      Check_Input = .TRUE.
      IF (Options_Present) THEN
        Check_Input = Options(m)%Check_Input
      END IF


      ! Check the input atmosphere if required
      IF ( Check_Input ) THEN
        IF ( .NOT. CRTM_Atmosphere_IsValid( Atmosphere(m) ) ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Input data check failed for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
      END IF


      ! Check the RTSolution layer dimensions
      IF ( ANY(RTSolution(:,m)%n_Layers    < Atmosphere(m)%n_Layers) .OR. &
           ANY(RTSolution_TL(:,m)%n_Layers < Atmosphere(m)%n_Layers) ) THEN
        Error_Status=FAILURE
        WRITE( Message,'("Number of RTSolution layers < Atmosphere for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN      
      END IF
      

      ! Allocate AtmOptics based on Atmosphere dimensions
      CALL CRTM_AtmOptics_Create( AtmOptics, &
                                  Atmosphere(m)%n_Layers, &
                                  MAX_N_LEGENDRE_TERMS  , &
                                  MAX_N_PHASE_ELEMENTS    )
      CALL CRTM_AtmOptics_Create( AtmOptics_TL, &
                                  Atmosphere(m)%n_Layers, &
                                  MAX_N_LEGENDRE_TERMS  , &
                                  MAX_N_PHASE_ELEMENTS    )
      IF ( .NOT. CRTM_AtmOptics_Associated( Atmoptics ) .OR. &
           .NOT. CRTM_AtmOptics_Associated( Atmoptics_TL ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error allocating AtmOptics data structures for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
      ! ...Set default number of streams
      AtmOptics%n_Legendre_Terms    = 4
      AtmOptics_TL%n_Legendre_Terms = AtmOptics%n_Legendre_Terms


      ! Allocate the aerosol scattering internal variable if necessary
      IF ( Atmosphere(m)%n_Aerosols > 0 ) THEN
        CALL ASvar_Create( ASvar, &
                           MAX_N_LEGENDRE_TERMS    , &
                           MAX_N_PHASE_ELEMENTS    , &
                           Atmosphere(m)%n_Layers  , &
                           Atmosphere(m)%n_Aerosols  )
      END IF


      ! -----------
      ! SENSOR LOOP
      ! -----------
      ! Initialise channel counter for channel(l)/sensor(n) count
      ln = 0
      
      Sensor_Loop: DO n = 1, n_Sensors

        ! Shorter name
        SensorIndex = ChannelInfo(n)%Sensor_Index


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
          RTSolution_TL(ln,m)%Sensor_Id        = RTSolution(ln,m)%Sensor_Id       
          RTSolution_TL(ln,m)%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
          RTSolution_TL(ln,m)%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id   
          RTSolution_TL(ln,m)%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel  

          
          ! Initialisations
          CALL CRTM_AtmOptics_Zero( AtmOptics )
          CALL CRTM_AtmOptics_Zero( AtmOptics_TL )


          ! Compute the aerosol absorption/scattering properties
          IF ( Atmosphere(m)%n_Aerosols > 0 ) THEN
            Status_FWD = CRTM_Compute_AerosolScatter( Atmosphere(m), &  ! Input
                                                      SensorIndex  , &  ! Input
                                                      ChannelIndex , &  ! Input
                                                      AtmOptics    , &  ! In/Output
                                                      ASVar          )  ! Internal variable output
            Status_TL = CRTM_Compute_AerosolScatter_TL( Atmosphere(m)   , &  ! FWD Input
                                                        AtmOptics       , &  ! FWD Input 
                                                        Atmosphere_TL(m), &  ! TL  Input
                                                        SensorIndex     , &  ! Input
                                                        ChannelIndex    , &  ! Input
                                                        AtmOptics_TL    , &  ! TL  Output  
                                                        ASVar             )  ! Internal variable 
            IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS) THEN
              Error_Status = FAILURE
              WRITE( Message,'("Error computing AerosolScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
              RETURN
            END IF

          END IF

          ! Save the nadir optical depths
          RTSolution(ln,m)%Layer_Optical_Depth(1:Atmosphere(m)%n_Layers) = AtmOptics%Optical_Depth
          RTSolution_TL(ln,m)%Layer_Optical_Depth(1:Atmosphere(m)%n_Layers) = AtmOptics_TL%Optical_Depth
        
        END DO Channel_Loop

      END DO Sensor_Loop
      

      ! Deallocate local sensor independent data structures
      CALL CRTM_AtmOptics_Destroy( AtmOptics )
      CALL CRTM_AtmOptics_Destroy( AtmOptics_TL )

    END DO Profile_Loop

  END FUNCTION CRTM_AOD_TL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AOD_AD
!
! PURPOSE:
!       Function that calculates the adjoint nadir aerosol optical depth.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AOD_AD( Atmosphere       , &
!                                   RTSolution_AD    , &
!                                   ChannelInfo      , &
!                                   RTSolution       , &
!                                   Atmosphere_AD    , &
!                                   Options = Options  )
!
! INPUTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Rank-1 (n_Profiles)
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
!       ChannelInfo:    Structure returned from the CRTM_Init() function
!                       that contains the satellite/sensor channel index
!                       information.
!                       UNITS:      N/A
!                       TYPE:       CRTM_ChannelInfo_type
!                       DIMENSION:  Rank-1 (n_Sensors)
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       RTSolution:     Structure containing the soluition to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
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
! OPTIONAL INPUTS:
!       Options:        Options structure containing the optional arguments
!                       for the CRTM.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
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
!       - Many of the components of the Options optional input structure
!         are not used in this function. Consult the CRTM User Guide for
!         which Options components are usable for AOD calculations.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_AOD_AD( &
    Atmosphere   , &  ! Input, M
    RTSolution_AD, &  ! Input, M
    ChannelInfo  , &  ! Input, M
    RTSolution   , &  ! Output, L x M 
    Atmosphere_AD, &  ! Output, L x M 
    Options      ) &  ! Optional input, M
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),        INTENT(IN)     :: Atmosphere(:)      ! M
    TYPE(CRTM_RTSolution_type),        INTENT(IN OUT) :: RTSolution_AD(:,:) ! L x M
    TYPE(CRTM_ChannelInfo_type),       INTENT(IN)     :: ChannelInfo(:)     ! n_Sensors
    TYPE(CRTM_RTSolution_type),        INTENT(IN OUT) :: RTSolution(:,:)    ! L x M
    TYPE(CRTM_Atmosphere_type),        INTENT(IN OUT) :: Atmosphere_AD(:)   ! M
    TYPE(CRTM_Options_type), OPTIONAL, INTENT(IN)     :: Options(:)         ! M
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AOD_AD'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Options_Present
    LOGICAL :: Check_Input
    INTEGER :: Status_FWD, Status_AD
    INTEGER :: n, n_Sensors,  SensorIndex
    INTEGER :: l, n_Channels, ChannelIndex
    INTEGER :: m, n_Profiles
    INTEGER :: na
    INTEGER :: ln
    ! Component variables
    TYPE(CRTM_AtmOptics_type) :: AtmOptics, AtmOptics_AD
    TYPE(ASVar_type) :: ASvar


    ! ------
    ! SET UP
    ! ------
    Error_Status = SUCCESS


    ! If no sensors or channels, simply return
    n_Sensors  = SIZE(ChannelInfo)
    n_Channels = SUM(CRTM_ChannelInfo_n_Channels(ChannelInfo))
    IF ( n_Sensors == 0 .OR. n_Channels == 0 ) RETURN


    ! Check the number of channels
    IF ( SIZE(RTSolution,   DIM=1) < n_Channels .OR. &
         SIZE(RTSolution_AD,DIM=1) < n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Output RTSolution structure arrays too small (",i0,&
             &") to hold results for the number of requested channels (",i0,")")') &
             SIZE(RTSolution,DIM=1), n_Channels
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! Check the number of profiles
    n_Profiles = SIZE(Atmosphere)
    ! ...Check the profile dimensionality of the other mandatory arguments
    IF ( SIZE(Atmosphere_AD)       /= n_Profiles .OR. &
         SIZE(RTSolution,   DIM=2) /= n_Profiles .OR. &
         SIZE(RTSolution_AD,DIM=2) /= n_Profiles      ) THEN
      Error_Status = FAILURE
      Message = 'Inconsistent profile dimensionality for input arguments.'
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status )
      RETURN
    END IF
    ! ...Check the profile dimensionality of the other optional arguments
    Options_Present = PRESENT(Options)
    IF ( Options_Present ) THEN
      IF ( SIZE(Options) /= n_Profiles ) THEN
        Error_Status = FAILURE
        Message = 'Inconsistent profile dimensionality for Options optional input argument.'
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
    END IF

    
    ! Check the RTSolution structures have been allocated
    IF ( ANY(.NOT. CRTM_RTSolution_Associated(RTSolution)) .OR. &
         ANY(.NOT. CRTM_RTSolution_Associated(RTSolution_AD)) ) THEN
      Error_Status = FAILURE
      Message = 'RTSolution output structure components have not been allocated'
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! ------------
    ! PROFILE LOOP
    ! ------------
    Profile_Loop: DO m = 1, n_Profiles
    
    
      ! Check the aerosol coeff. data for cases with aerosols
      IF( Atmosphere(m)%n_Aerosols > 0 .AND. .NOT. CRTM_AerosolCoeff_IsLoaded() )THEN
         Error_Status = FAILURE                                                                 
         WRITE( Message,'("The AerosolCoeff data must be loaded (with CRTM_Init routine) ", &   
                &"for the aerosol case profile #",i0)' ) m                                      
         CALL Display_Message( ROUTINE_NAME, Message, Error_Status )                      
         RETURN                                                                                 
      END IF

      
      ! Check the optional Options structure argument
      Check_Input = .TRUE.
      IF (Options_Present) THEN
        Check_Input = Options(m)%Check_Input
      END IF


      ! Check the input atmosphere if required
      IF ( Check_Input ) THEN
        IF ( .NOT. CRTM_Atmosphere_IsValid( Atmosphere(m) ) ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Input data check failed for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
      END IF


      ! Check the RTSolution layer dimensions
      IF ( ANY(RTSolution(:,m)%n_Layers    < Atmosphere(m)%n_Layers) .OR. &
           ANY(RTSolution_AD(:,m)%n_Layers < Atmosphere(m)%n_Layers) ) THEN
        Error_Status=FAILURE
        WRITE( Message,'("Number of RTSolution layers < Atmosphere for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN      
      END IF
      

      ! Allocate AtmOptics based on Atmosphere dimensions
      CALL CRTM_AtmOptics_Create( AtmOptics, &
                                  Atmosphere(m)%n_Layers, &
                                  MAX_N_LEGENDRE_TERMS  , &
                                  MAX_N_PHASE_ELEMENTS    )
      CALL CRTM_AtmOptics_Create( AtmOptics_AD, &
                                  Atmosphere(m)%n_Layers, &
                                  MAX_N_LEGENDRE_TERMS  , &
                                  MAX_N_PHASE_ELEMENTS    )
      IF ( .NOT. CRTM_AtmOptics_Associated( Atmoptics ) .OR. &
           .NOT. CRTM_AtmOptics_Associated( Atmoptics_AD ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error allocating AtmOptics data structures for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
      ! ...Set default number of streams
      AtmOptics%n_Legendre_Terms    = 4
      AtmOptics_AD%n_Legendre_Terms = AtmOptics%n_Legendre_Terms


      ! Allocate the aerosol scattering internal variable if necessary
      IF ( Atmosphere(m)%n_Aerosols > 0 ) THEN
        CALL ASvar_Create( ASvar, &
                           MAX_N_LEGENDRE_TERMS    , &
                           MAX_N_PHASE_ELEMENTS    , &
                           Atmosphere(m)%n_Layers  , &
                           Atmosphere(m)%n_Aerosols  )
      END IF


      ! Copy over atmosphere info to adjoint output
      ! ...Climatology
      Atmosphere_AD(m)%Climatology = Atmosphere(m)%Climatology
      ! ...Absorber info
      Atmosphere_AD(m)%Absorber_Id    = Atmosphere(m)%Absorber_Id   
      Atmosphere_AD(m)%Absorber_Units = Atmosphere(m)%Absorber_Units
      ! ...Aerosol info
      DO na = 1, Atmosphere(m)%n_Aerosols
        Atmosphere_AD(m)%Aerosol(na)%Type = Atmosphere(m)%Aerosol(na)%Type      
      END DO


      ! -----------
      ! SENSOR LOOP
      ! -----------
      ! Initialise channel counter for sensor(n)/channel(l) count
      ln = 0
    
      Sensor_Loop: DO n = 1, n_Sensors


        ! Shorter name
        SensorIndex = ChannelInfo(n)%Sensor_Index


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

          
          ! Initialisations
          CALL CRTM_AtmOptics_Zero( AtmOptics )
          CALL CRTM_AtmOptics_Zero( AtmOptics_AD )
          AtmOptics_AD%Optical_Depth = RTSolution_AD(ln,m)%Layer_Optical_Depth(1:Atmosphere(m)%n_Layers)


          ! Compute the aerosol absorption/scattering properties
          IF ( Atmosphere(m)%n_Aerosols > 0 ) THEN
            Status_FWD = CRTM_Compute_AerosolScatter( Atmosphere(m), &  ! Input
                                                      SensorIndex  , &  ! Input
                                                      ChannelIndex , &  ! Input
                                                      AtmOptics    , &  ! In/Output
                                                      ASVar          )  ! Internal variable output
            Status_AD = CRTM_Compute_AerosolScatter_AD( Atmosphere(m)   , &  ! FWD Input
                                                        AtmOptics       , &  ! FWD Input
                                                        AtmOptics_AD    , &  ! AD  Input
                                                        SensorIndex     , &  ! Input
                                                        ChannelIndex    , &  ! Input
                                                        Atmosphere_AD(m), &  ! AD  Output
                                                        ASVar             )  ! Internal variable input
            IF ( Status_FWD /= SUCCESS .OR. Status_AD /= SUCCESS) THEN
              Error_Status = FAILURE
              WRITE( Message,'("Error computing AerosolScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF


          ! Save the nadir optical depths
          RTSolution(ln,m)%Layer_Optical_Depth(1:Atmosphere(m)%n_Layers) = AtmOptics%Optical_Depth
         
        END DO Channel_Loop

      END DO Sensor_Loop
      

      ! Deallocate local sensor independent data structures
      CALL CRTM_AtmOptics_Destroy( AtmOptics )
      CALL CRTM_AtmOptics_Destroy( AtmOptics_AD )

    END DO Profile_Loop

  END FUNCTION CRTM_AOD_AD


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AOD_K
!
! PURPOSE:
!       Function that calculates the K-matrix nadir aerosol optical depth.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AOD_K( Atmosphere        , &
!                                  RTSolution_K      , &
!                                  ChannelInfo       , &
!                                  RTSolution        , &
!                                  Atmosphere_K      , &
!                                  Opttions = Options  )
!
! INPUTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Rank-1 (n_Profiles)
!                       ATTRIBUTES: INTENT(IN)
!
!       RTSolution_K:   Structure containing the aerosol optical depth
!                       profile K-matrix input.
!                       **NOTE: On EXIT from this function, the contents of
!                               this structure may be modified (e.g. set to
!                               zero.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       ChannelInfo:    Structure returned from the CRTM_Init() function
!                       that contains the satellite/sensor channel index
!                       information.
!                       UNITS:      N/A
!                       TYPE:       CRTM_ChannelInfo_type
!                       DIMENSION:  Rank-1 (n_Sensors)
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       RTSolution:     Structure containing the layer aerosol optical 
!                       depth profile for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-2 (n_Channels x n_Profiles)
!                       ATTRIBUTES: INTENT(IN OUT)
!
!       Atmosphere_K:   Structure containing the K-matrix Atmosphere data.
!                       **NOTE: On ENTRY to this function, the contents of
!                               this structure should be defined (e.g.
!                               initialized to some value based on the
!                               position of this function in the call chain.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Same as input RTSolution_K argument
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Options:        Options structure containing the optional arguments
!                       for the CRTM.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
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
!       - Many of the components of the Options optional input structure
!         are not used in this function. Consult the CRTM User Guide for
!         which Options components are usable for AOD calculations.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_AOD_K ( &
    Atmosphere  , &  ! Input, M
    RTSolution_K, &  ! Input, M
    ChannelInfo , &  ! Input, M
    RTSolution  , &  ! Output, L x M 
    Atmosphere_K, &  ! Output, L x M 
    Options     ) &  ! Optional input, M
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type),        INTENT(IN)     :: Atmosphere(:)      ! M
    TYPE(CRTM_RTSolution_type),        INTENT(IN OUT) :: RTSolution_K(:,:)  ! L x M
    TYPE(CRTM_ChannelInfo_type),       INTENT(IN)     :: ChannelInfo(:)     ! n_Sensors 
    TYPE(CRTM_RTSolution_type),        INTENT(IN OUT) :: RTSolution(:,:)    ! L x M
    TYPE(CRTM_Atmosphere_type),        INTENT(IN OUT) :: Atmosphere_K(:,:)  ! L x M
    TYPE(CRTM_Options_type), OPTIONAL, INTENT(IN)     :: Options(:)         ! M
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AOD_K'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Options_Present
    LOGICAL :: Check_Input
    INTEGER :: Status_FWD, Status_K
    INTEGER :: n, n_Sensors,  SensorIndex
    INTEGER :: l, n_Channels, ChannelIndex
    INTEGER :: m, n_Profiles
    INTEGER :: na
    INTEGER :: ln
    ! Component variables
    TYPE(CRTM_AtmOptics_type) :: AtmOptics, AtmOptics_K
    TYPE(ASVar_type) :: ASvar


    ! ------
    ! SET UP
    ! ------
    Error_Status = SUCCESS


    ! If no sensors or channels, simply return
    n_Sensors  = SIZE(ChannelInfo)
    n_Channels = SUM(CRTM_ChannelInfo_n_Channels(ChannelInfo))
    IF ( n_Sensors == 0 .OR. n_Channels == 0 ) RETURN


    ! Check spectral arrays
    IF ( SIZE(RTSolution  ,DIM=1) < n_Channels .OR. &
         SIZE(Atmosphere_K,DIM=1) < n_Channels .OR. &
         SIZE(RTSolution_K,DIM=1) < n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("RTSolution and K-matrix (Atm,RT) structure arrays too small (",&
             &2(i0,","),i0,") for the number of requested channels (",i0,")")') &
             SIZE(RTSolution  ,DIM=1), &
             SIZE(Atmosphere_K,DIM=1), &
             SIZE(RTSolution_K,DIM=1), &
             n_Channels
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! Check the number of profiles
    ! ...Number of atmospheric profiles.
    n_Profiles = SIZE(Atmosphere)
    ! ...Check the profile dimensionality of the other mandatory arguments
    IF ( SIZE(RTSolution_K,DIM=2) /= n_Profiles .OR. &
         SIZE(Atmosphere_K,DIM=2) /= n_Profiles .OR. &
         SIZE(RTSolution  ,DIM=2) /= n_Profiles      ) THEN
      Error_Status = FAILURE
      Message = 'Inconsistent profile dimensionality for input arguments.'
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF
    ! ...Check the profile dimensionality of the other optional arguments
    Options_Present = PRESENT(Options)
    IF ( Options_Present ) THEN
      IF ( SIZE( Options ) /= n_Profiles ) THEN
        Error_Status = FAILURE
        Message = 'Inconsistent profile dimensionality for Options optional input argument.'
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
    END IF


    ! Check the RTSolution structures have been allocated
    IF ( ANY(.NOT. CRTM_RTSolution_Associated(RTSolution)) .OR. &
         ANY(.NOT. CRTM_RTSolution_Associated(RTSolution_K)) ) THEN
      Error_Status = FAILURE
      Message = 'RTSolution output structure components have not been allocated'
      CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
      RETURN
    END IF


    ! ------------
    ! PROFILE LOOP
    ! ------------
    Profile_Loop: DO m = 1, n_Profiles


      ! Check the aerosol coeff. data for cases with aerosols
      IF( Atmosphere(m)%n_Aerosols > 0 .AND. .NOT. CRTM_AerosolCoeff_IsLoaded() )THEN
         Error_Status = FAILURE
         WRITE( Message,'("The AerosolCoeff data must be loaded (with CRTM_Init routine) ", &
                &"for the aerosol case profile #",i0)' ) m
         CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
         RETURN
      END IF
      
      
      ! Check the optional Options structure argument
      Check_Input = .TRUE.
      IF (Options_Present) THEN
        Check_Input = Options(m)%Check_Input
      END IF


      ! Check the input atmosphere if required
      IF ( Check_Input ) THEN
        IF ( .NOT. CRTM_Atmosphere_IsValid( Atmosphere(m) ) ) THEN
          Error_Status = FAILURE
          WRITE( Message,'("Input data check failed for profile #",i0)' ) m
          CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
          RETURN
        END IF
      END IF


      ! Check the RTSolution layer dimensions
      IF ( ANY(RTSolution(:,m)%n_Layers   < Atmosphere(m)%n_Layers) .OR. &
           ANY(RTSolution_K(:,m)%n_Layers < Atmosphere(m)%n_Layers) ) THEN
        Error_Status=FAILURE
        WRITE( Message,'("Number of RTSolution layers < Atmosphere for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF


      ! Allocate AtmOptics based on Atmosphere dimensions
      CALL CRTM_AtmOptics_Create( AtmOptics, &
                                  Atmosphere(m)%n_Layers, &
                                  MAX_N_LEGENDRE_TERMS  , &
                                  MAX_N_PHASE_ELEMENTS    )
      CALL CRTM_AtmOptics_Create( AtmOptics_K, &
                                  Atmosphere(m)%n_Layers, &
                                  MAX_N_LEGENDRE_TERMS  , &
                                  MAX_N_PHASE_ELEMENTS    )
      IF ( .NOT. CRTM_AtmOptics_Associated( Atmoptics ) .OR. &
           .NOT. CRTM_AtmOptics_Associated( Atmoptics_K ) ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error allocating AtmOptics data structures for profile #",i0)' ) m
        CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
        RETURN
      END IF
      ! ...Set default number of streams
      AtmOptics%n_Legendre_Terms    = 4
      AtmOptics_K%n_Legendre_Terms = AtmOptics%n_Legendre_Terms


      ! Allocate the aerosol scattering internal variable if necessary
      IF ( Atmosphere(m)%n_Aerosols > 0 ) THEN
        CALL ASvar_Create( ASvar, &
                           MAX_N_LEGENDRE_TERMS    , &
                           MAX_N_PHASE_ELEMENTS    , &
                           Atmosphere(m)%n_Layers  , &
                           Atmosphere(m)%n_Aerosols  )
      END IF


      ! -----------
      ! SENSOR LOOP
      ! -----------
      ! Initialise channel counter for sensor(n)/channel(l) count
      ln = 0
    
      Sensor_Loop: DO n = 1, n_Sensors


        ! Shorter name
        SensorIndex = ChannelInfo(n)%Sensor_Index


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
          RTSolution_K(ln,m)%Sensor_Id        = RTSolution(ln,m)%Sensor_Id       
          RTSolution_K(ln,m)%WMO_Satellite_Id = RTSolution(ln,m)%WMO_Satellite_Id
          RTSolution_K(ln,m)%WMO_Sensor_Id    = RTSolution(ln,m)%WMO_Sensor_Id   
          RTSolution_K(ln,m)%Sensor_Channel   = RTSolution(ln,m)%Sensor_Channel  


          ! Copy over atmosphere info to k-matrix output
          ! ...Climatology
          Atmosphere_K(ln,m)%Climatology = Atmosphere(m)%Climatology
          ! ...Absorber info
          Atmosphere_K(ln,m)%Absorber_Id    = Atmosphere(m)%Absorber_Id   
          Atmosphere_K(ln,m)%Absorber_Units = Atmosphere(m)%Absorber_Units
          ! ...Aerosol info
          DO na = 1, Atmosphere(m)%n_Aerosols
            Atmosphere_K(ln,m)%Aerosol(na)%Type = Atmosphere(m)%Aerosol(na)%Type      
          END DO

          
          ! Initialisations
          CALL CRTM_AtmOptics_Zero( AtmOptics )
          CALL CRTM_AtmOptics_Zero( AtmOptics_K )
          AtmOptics_K%Optical_Depth = RTSolution_K(ln,m)%Layer_Optical_Depth(1:Atmosphere(m)%n_Layers)


          ! Compute the aerosol absorption/scattering properties
          IF ( Atmosphere(m)%n_Aerosols > 0 ) THEN
            Status_FWD = CRTM_Compute_AerosolScatter( Atmosphere(m), &  ! Input
                                                      SensorIndex  , &  ! Input
                                                      ChannelIndex , &  ! Input
                                                      AtmOptics    , &  ! In/Output
                                                      ASVar          )  ! Internal variable output
            Status_K = CRTM_Compute_AerosolScatter_AD( Atmosphere(m)     , &  ! FWD Input
                                                       AtmOptics         , &  ! FWD Input
                                                       AtmOptics_K       , &  ! AD  Input
                                                       SensorIndex       , &  ! Input
                                                       ChannelIndex      , &  ! Input
                                                       Atmosphere_K(ln,m), &  ! AD  Output
                                                       ASVar               )  ! Internal variable input
            IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS) THEN
              Error_Status = FAILURE
              WRITE( Message,'("Error computing AerosolScatter for ",a,&
                     &", channel ",i0,", profile #",i0)' ) &
                     TRIM(ChannelInfo(n)%Sensor_ID), ChannelInfo(n)%Sensor_Channel(l), m
              CALL Display_Message( ROUTINE_NAME, Message, Error_Status )
              RETURN
            END IF
          END IF


          ! Save the nadir optical depths
          RTSolution(ln,m)%Layer_Optical_Depth(1:Atmosphere(m)%n_Layers) = AtmOptics%Optical_Depth
         
        END DO Channel_Loop

      END DO Sensor_Loop
      

      ! Deallocate local sensor independent data structures
      CALL CRTM_AtmOptics_Destroy( AtmOptics )
      CALL CRTM_AtmOptics_Destroy( AtmOptics_K )

    END DO Profile_Loop

  END FUNCTION CRTM_AOD_K


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AOD_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_AOD_Version( Id )
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

  SUBROUTINE CRTM_AOD_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_AOD_Version

END MODULE CRTM_AOD_Module
