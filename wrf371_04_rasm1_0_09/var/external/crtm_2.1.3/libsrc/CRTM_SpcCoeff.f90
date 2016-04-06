!
! CRTM_SpcCoeff
!
! Module containing the shared CRTM spectral coefficients (SpcCoeff)
! and their load/destruction routines. 
!
! PUBLIC DATA:
!       SC:  Data structure array containing the spectral coefficient
!            data for the requested sensors.
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure SC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 12-Jun-2000
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_SpcCoeff

  ! ----------------
  ! Enviroment setup
  ! ----------------
  ! Module use
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE SensorInfo_Parameters, ONLY: N_POLARIZATION_TYPES   , &
                                   INVALID_POLARIZATION   , &
                                   UNPOLARIZED            , &
                                   INTENSITY              , &
                                   FIRST_STOKES_COMPONENT , &
                                   SECOND_STOKES_COMPONENT, &
                                   THIRD_STOKES_COMPONENT , &
                                   FOURTH_STOKES_COMPONENT, &
                                   VL_POLARIZATION        , &
                                   HL_POLARIZATION        , &
                                   plus45L_POLARIZATION   , &
                                   minus45L_POLARIZATION  , &
                                   VL_MIXED_POLARIZATION  , &
                                   HL_MIXED_POLARIZATION  , &
                                   RC_POLARIZATION        , &
                                   LC_POLARIZATION        , &
                                   POLARIZATION_TYPE_NAME 
  USE SpcCoeff_Define      , ONLY: SpcCoeff_type               , &
                                   SpcCoeff_Associated         , &
                                   SpcCoeff_Destroy            , &
                                   SpcCoeff_IsSolar            , &
                                   SpcCoeff_IsZeeman           , &
                                   SpcCoeff_IsMicrowaveSensor  , &
                                   SpcCoeff_IsInfraredSensor   , &
                                   SpcCoeff_IsVisibleSensor    , &
                                   SpcCoeff_IsUltravioletSensor
  USE SpcCoeff_Binary_IO   , ONLY: SpcCoeff_Binary_ReadFile
  USE CRTM_Parameters      , ONLY: CRTM_Set_Max_nChannels  , &
                                   CRTM_Reset_Max_nChannels
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! The shared data
  PUBLIC :: SC
  ! Public routines in this module
  PUBLIC :: CRTM_SpcCoeff_Load
  PUBLIC :: CRTM_SpcCoeff_Destroy
  PUBLIC :: CRTM_SpcCoeff_IsLoaded
  ! Flag and sensor check procedures passed through from SpcCoeff_Define
  PUBLIC :: SpcCoeff_IsSolar
  PUBLIC :: SpcCoeff_IsZeeman  
  PUBLIC :: SpcCoeff_IsMicrowaveSensor  
  PUBLIC :: SpcCoeff_IsInfraredSensor   
  PUBLIC :: SpcCoeff_IsVisibleSensor    
  PUBLIC :: SpcCoeff_IsUltravioletSensor
  ! Polarisation flag parameters passed through from SensorInfo_Parameters
  PUBLIC :: N_POLARIZATION_TYPES   
  PUBLIC :: INVALID_POLARIZATION   
  PUBLIC :: UNPOLARIZED            
  PUBLIC :: INTENSITY              
  PUBLIC :: FIRST_STOKES_COMPONENT 
  PUBLIC :: SECOND_STOKES_COMPONENT
  PUBLIC :: THIRD_STOKES_COMPONENT 
  PUBLIC :: FOURTH_STOKES_COMPONENT
  PUBLIC :: VL_POLARIZATION        
  PUBLIC :: HL_POLARIZATION        
  PUBLIC :: plus45L_POLARIZATION   
  PUBLIC :: minus45L_POLARIZATION  
  PUBLIC :: VL_MIXED_POLARIZATION  
  PUBLIC :: HL_MIXED_POLARIZATION  
  PUBLIC :: RC_POLARIZATION        
  PUBLIC :: LC_POLARIZATION        
  PUBLIC :: POLARIZATION_TYPE_NAME
  

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_SpcCoeff.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! -------------------------------------
  ! The shared spectral coefficients data
  ! -------------------------------------
  TYPE(SpcCoeff_type), SAVE, ALLOCATABLE :: SC(:)


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SpcCoeff_Load
!
! PURPOSE:
!       Function to load the SpcCoeff spectral coefficient data into
!       the public data structure SC.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Load_SpcCoeff( &
!                        Sensor_ID                            , &
!                        File_Path         = File_Path        , &
!                        Quiet             = Quiet            , &
!                        Process_ID        = Process_ID       , &
!                        Output_Process_ID = Output_Process_ID  )
!
! INPUTS:
!       Sensor_ID:          List of the sensor IDs (e.g. hirs3_n17, amsua_n18,
!                           ssmis_f16, etc) with which the CRTM is to be
!                           initialised. These Sensor ID are used to construct
!                           the sensor specific SpcCoeff filenames containing
!                           the necessary coefficient data, i.e.
!                             <Sensor_ID>.SpcCoeff.bin
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Rank-1
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       File_Path:          Character string specifying a file path for the
!                           input data files. If not specified, the current
!                           directory is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:              Set this logical argument to suppress INFORMATION
!                           messages being printed to stdout
!                           If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                              == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                           If not specified, default is .FALSE.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      None
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all INFORMATION messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the INFORMATION messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      None
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the SpcCoeff data load was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure SC.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_SpcCoeff_Load( &
    Sensor_ID        , &  ! Input
    File_Path        , &  ! Optional input
    Quiet            , &  ! Optional input
    Process_ID       , &  ! Optional input
    Output_Process_ID) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Sensor_ID(:)
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: File_Path
    LOGICAL     , OPTIONAL, INTENT(IN)  :: Quiet             
    INTEGER     , OPTIONAL, INTENT(IN)  :: Process_ID        
    INTEGER     , OPTIONAL, INTENT(IN)  :: Output_Process_ID 
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SpcCoeff_Load'
    ! Local variables
    CHARACTER(ML) :: msg, pid_msg
    CHARACTER(ML) :: path
    CHARACTER(ML) :: spccoeff_file
    LOGICAL :: noisy
    INTEGER :: alloc_stat
    INTEGER :: n, n_sensors

    ! Setup 
    err_stat = SUCCESS
    ! ...Check the File_Path argument
    IF ( PRESENT(File_Path) ) THEN
      path = ADJUSTL(File_Path)
    ELSE
      path = ''
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Check the MPI Process Ids
    IF ( noisy .AND. PRESENT(Process_ID) .AND. PRESENT(Output_Process_ID) ) THEN
      IF ( Process_Id /= Output_Process_Id ) noisy = .FALSE.
    END IF
    ! ...Create a process ID message tag for error messages
    IF ( PRESENT(Process_Id) ) THEN
      WRITE( pid_msg,'("; Process ID: ",i0)' ) Process_ID
    ELSE
      pid_msg = ''
    END IF

    
    ! Allocate the SpcCoeff shared data structure array
    n_sensors = SIZE(Sensor_ID)
    ALLOCATE( SC(n_sensors), STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      err_stat = FAILURE
      WRITE( msg,'("SpcCoeff structure array allocation failed. STAT=",i0)') alloc_stat
      CALL Display_Message( ROUTINE_NAME, TRIM(msg)//TRIM(pid_msg), err_stat ); RETURN
    END IF


    ! Read the SpcCoeff data files
    DO n = 1, n_Sensors
      spccoeff_file = TRIM(path)//TRIM(ADJUSTL(Sensor_ID(n)))//'.SpcCoeff.bin'
      err_stat = SpcCoeff_Binary_ReadFile( &
        spccoeff_file      , &
        SC(n)              , &
        Quiet = .NOT. noisy  )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading SpcCoeff file #",i0,", ",a)') n, TRIM(spccoeff_file)
        CALL Display_Message( ROUTINE_NAME, TRIM(msg)//TRIM(pid_msg), err_stat ); RETURN
      END IF
    END DO


    ! Set the protected variable MAX_N_CHANNELS
    CALL CRTM_Set_Max_nChannels( SUM(SC%n_Channels) )

  END FUNCTION CRTM_SpcCoeff_Load


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SpcCoeff_Destroy
!
! PURPOSE:
!       Function to deallocate the public data structure array containing
!       the CRTM SpcCoeff spectral coefficient data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_SpcCoeff( Process_ID = Process_ID )
!
! OPTIONAL INPUTS:
!       Process_ID:       Set this argument to the MPI process ID that this
!                         function call is running under. This value is used
!                         solely for controlling message output. If MPI is not
!                         being used, ignore this argument.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the deallocation of the public SC data
!                                       structure was successful
!                            == FAILURE an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public shared data
!       structures in this module.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_SpcCoeff_Destroy( Process_ID ) RESULT( err_stat )
    ! Arguments
    INTEGER, OPTIONAL, INTENT(IN) :: Process_ID
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_SpcCoeff_Destroy'
    ! Local variables
    CHARACTER(ML) :: msg, pid_msg
    INTEGER :: alloc_stat

    ! Setup
    err_stat = SUCCESS
    ! ...Create a process ID message tag for error messages
    IF ( PRESENT(Process_Id) ) THEN
      WRITE( pid_msg,'("; Process ID: ",i0)' ) Process_ID
    ELSE
      pid_msg = ''
    END IF


    ! Destroy the structure array elements
    CALL SpcCoeff_Destroy( SC )
    IF ( ANY(SpcCoeff_Associated( SC )) )THEN
      err_stat = FAILURE
      msg = 'Error deallocating SpcCoeff shared data structures'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      ! No return here...keep deallocating
    END IF


    ! Deallocate the structure array itself
    DEALLOCATE( SC, STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      err_stat = FAILURE
      WRITE( msg,'("Error deallocating SpcCoeff structure array. STAT=",i0)') alloc_stat
      CALL Display_Message( ROUTINE_NAME, TRIM(msg)//TRIM(pid_msg), err_stat ); RETURN
      ! Again, no return.
    END IF


    ! Reset the protected variable MAX_N_CHANNELS
    CALL CRTM_Reset_Max_nChannels()

  END FUNCTION CRTM_SpcCoeff_Destroy


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_SpcCoeff_IsLoaded
!
! PURPOSE:
!       Function to test if the SpcCoeff spectral coefficient data has
!       been loaded into the public data structure array SC.
!
! CALLING SEQUENCE:
!       status = CRTM_SpcCoeff_IsLoaded()
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_SpcCoeff_IsLoaded() RESULT( IsLoaded )
    LOGICAL :: IsLoaded
    IsLoaded = ALLOCATED(SC)
    IF ( IsLoaded ) IsLoaded = IsLoaded .AND. ALL(SpcCoeff_Associated( SC ))
  END FUNCTION CRTM_SpcCoeff_IsLoaded

END MODULE CRTM_SpcCoeff
