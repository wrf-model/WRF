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
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_SpcCoeff

  ! ----------------
  ! Enviroment setup
  ! ----------------
  ! Module use
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE SpcCoeff_Define   , ONLY: SpcCoeff_type          , &
                                Destroy_SpcCoeff       , &
                                N_SENSOR_TYPES         , &
                                INVALID_SENSOR         , &
                                MICROWAVE_SENSOR       , &
                                INFRARED_SENSOR        , &
                                VISIBLE_SENSOR         , &
                                SENSOR_TYPE_NAME       , &
                                N_POLARIZATION_TYPES   , &
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
                                POLARIZATION_TYPE_NAME , &
                                ! SpcCoeff flag set check
                                IsFlagSet_SpcCoeff     , &
                                SOLAR_FLAG             , &
                                ZEEMAN_FLAG
  USE SpcCoeff_Binary_IO, ONLY: Read_SpcCoeff_Binary
  USE CRTM_Parameters   , ONLY: MAX_N_SENSORS           , &
                                CRTM_Set_Max_nChannels  , &
                                CRTM_Reset_Max_nChannels, &
                                CRTM_Get_Max_nChannels  , &
                                CRTM_IsSet_Max_nChannels
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
  PUBLIC :: CRTM_Load_SpcCoeff
  PUBLIC :: CRTM_Destroy_SpcCoeff
  ! Sensor type module parameters inherited from SpcCoeff_Define
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR
  PUBLIC :: VISIBLE_SENSOR
  PUBLIC :: SENSOR_TYPE_NAME
  ! Polarisation flag parameters inherited from SpcCoeff_Define
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
  ! Flag set function and values inherited from SpcCoeff_Define
  PUBLIC :: IsFlagSet_SpcCoeff
  PUBLIC :: SOLAR_FLAG        
  PUBLIC :: ZEEMAN_FLAG
  

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_SpcCoeff.f90 1879 2008-03-03 19:45:30Z paul.vandelst@noaa.gov $'


  ! -------------------------------------
  ! The shared spectral coefficients data
  ! -------------------------------------
  TYPE(SpcCoeff_type), SAVE, ALLOCATABLE :: SC(:)


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Load_SpcCoeff
!
! PURPOSE:
!       Function to load the SpcCoeff spectral coefficient data into
!       the public data structure SC.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Load_SpcCoeff( Sensor_ID        =Sensor_ID,         &  ! Optional input
!                                          File_Path        =File_Path,         &  ! Optional input
!                                          Quiet            =Quiet,             &  ! Optional input
!                                          Process_ID       =Process_ID,        &  ! Optional input
!                                          Output_Process_ID=Output_Process_ID, &  ! Optional input
!                                          Message_Log      =Message_Log        )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       Sensor_ID:          List of the sensor IDs (e.g. hirs3_n17, amsua_n18,
!                           ssmis_f16, etc) with which the CRTM is to be
!                           initialised. These Sensor ID are used to construct
!                           the sensor specific SpcCoeff filenames containing
!                           the necessary coefficient data, i.e.
!                             <Sensor_ID>.SpcCoeff.bin
!                           If this argument is not specified, the default
!                           SpcCoeff filename is
!                             SpcCoeff.bin
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Rank-1
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       File_Path:          Character string specifying a file path for the
!                           input data files. If not specified, the current
!                           directory is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:              Set this argument to suppress INFORMATION messages
!                           being printed to standard output (or the message
!                           log file if the Message_Log optional argument is
!                           used.) By default, INFORMATION messages are printed.
!                           If QUIET = 0, INFORMATION messages are OUTPUT.
!                              QUIET = 1, INFORMATION messages are SUPPRESSED.
!                           UNITS:      None
!                           TYPE:       INTEGER
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
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the SpcCoeff data load was successful
!                              == FAILURE an unrecoverable error occurred.
!                              == WARNING the number of channels read in differs
!                                         from that stored in the CRTM_Parameters
!                                         module.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure SC.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Load_SpcCoeff( Sensor_ID        , &  ! Input
                               File_Path        , &  ! Optional input
                               Quiet            , &  ! Optional input
                               Process_ID       , &  ! Optional input
                               Output_Process_ID, &  ! Optional input
                               Message_Log      ) &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    CHARACTER(*), DIMENSION(:), OPTIONAL, INTENT(IN)  :: Sensor_ID
    CHARACTER(*),               OPTIONAL, INTENT(IN)  :: File_Path
    INTEGER,                    OPTIONAL, INTENT(IN)  :: Quiet             
    INTEGER,                    OPTIONAL, INTENT(IN)  :: Process_ID        
    INTEGER,                    OPTIONAL, INTENT(IN)  :: Output_Process_ID 
    CHARACTER(*),               OPTIONAL, INTENT(IN)  :: Message_Log       
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Load_SpcCoeff'
    ! Local variables
    CHARACTER(256) :: Message 
    CHARACTER(256) :: Process_ID_Tag
    CHARACTER(256), DIMENSION(MAX_N_SENSORS) :: SpcCoeff_File
    INTEGER :: Allocate_Status
    INTEGER :: n, n_Sensors, n_Channels
    INTEGER :: Max_n_Channels  ! Maximum channels protected variable

    ! Setup 
    Error_Status = SUCCESS
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT(Process_ID) ) THEN
      WRITE( Process_ID_Tag, '(";  MPI Process ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Determine the number of sensors and construct their filenames
    IF ( PRESENT(Sensor_ID) ) THEN
      ! Construct filenames for specified sensors
      n_Sensors=SIZE(Sensor_ID)
      IF ( n_Sensors > MAX_N_SENSORS ) THEN
        Error_Status = FAILURE
        WRITE(Message,'("Too many sensors, ",i0," specified. Maximum of ",i0," sensors allowed.")') &
                      n_Sensors, MAX_N_SENSORS
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message)//TRIM(Process_ID_Tag), &
                              Error_Status, &
                              Message_Log=Message_Log)
        RETURN
      END IF
      DO n=1,n_Sensors
        SpcCoeff_File(n) = TRIM(ADJUSTL(Sensor_ID(n)))//'.SpcCoeff.bin'
      END DO
    ELSE
      ! No sensors specified. Use default filename.
      n_Sensors=1
      SpcCoeff_File(1)='SpcCoeff.bin'
    END IF

    ! Add the file path
    IF ( PRESENT(File_Path) ) THEN
      DO n=1,n_Sensors
        SpcCoeff_File(n) = TRIM(ADJUSTL(File_Path))//TRIM(SpcCoeff_File(n))
      END DO
    END IF
    
    ! Allocate the SpcCoeff shared data structure array
    ALLOCATE(SC(n_Sensors),STAT=Allocate_Status)
    IF ( Allocate_Status/=0 ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("SpcCoeff structure array allocation failed. STAT=",i0)') Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message)//TRIM(Process_ID_Tag), &
                            Error_Status, &
                            Message_Log=Message_Log)
      RETURN
    END IF

    ! Read the SpcCoeff data files
    DO n = 1, n_Sensors
      Error_Status = Read_SpcCoeff_Binary( TRIM(SpcCoeff_File(n))             , &  ! Input
                                           SC(n)                              , &  ! Output
                                           Quiet            =Quiet            , &
                                           Process_ID       =Process_ID       , &
                                           Output_Process_ID=Output_Process_ID, &
                                           Message_Log      =Message_Log        )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE(Message,'("Error reading SpcCoeff file #",i0,", ",a)') &
                      n, TRIM(SpcCoeff_File(n))
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message)//TRIM(Process_ID_Tag), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END DO

    ! Determine the total number of channels
    n_Channels = SUM(SC%n_Channels)
    
    ! Set the protected variable MAX_N_CHANNELS
    !
    ! Get the current value, if any
    Max_n_Channels = CRTM_Get_Max_nChannels()
    ! Has the number of channels been set?
    IF ( CRTM_IsSet_Max_nChannels() ) THEN
      ! Yes. Check the value      
      IF ( Max_n_Channels /= n_Channels ) THEN
        Error_Status = WARNING
        WRITE( Message, '( "MAX_N_CHANNELS already set to different value, ",i0,", ", &
                          &"than defined by SpcCoeff file(s), ",i0, &
                          &". Overwriting" )' ) &
                        Max_n_Channels, n_Channels
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message)//TRIM(Process_ID_Tag), &
                              Error_Status, &
                              Message_Log=Message_Log )
        CALL CRTM_Set_Max_nChannels(n_Channels)
      END IF
    ELSE
      ! No. Set the value
      CALL CRTM_Set_Max_nChannels(n_Channels)
    END IF

  END FUNCTION CRTM_Load_SpcCoeff


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_SpcCoeff
!
! PURPOSE:
!       Function to deallocate the public data structure array containing
!       the CRTM SpcCoeff spectral coefficient data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_SpcCoeff( Process_ID  = Process_ID, &  ! Optional input
!                                             Message_Log = Message_Log )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       Process_ID:       Set this argument to the MPI process ID that this
!                         function call is running under. This value is used
!                         solely for controlling message output. If MPI is not
!                         being used, ignore this argument.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to the screen.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
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
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_SpcCoeff( Process_ID,   &  ! Optional input
                                  Message_Log ) &  ! Error messaging
                                RESULT( Error_Status )
    ! Arguments
    INTEGER,      OPTIONAL, INTENT(IN)  :: Process_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_SpcCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    CHARACTER(256) :: Process_ID_Tag
    INTEGER :: n, Destroy_Status, Allocate_Status

    ! Set up
    Error_Status = SUCCESS
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '(";  MPI Process ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Destroy the structure array elements
    DO n = 1, SIZE(SC)
      Destroy_Status = Destroy_SpcCoeff( SC(n), &
                                         Message_Log=Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE(Message,'("Error destroying SpcCoeff structure array element #",i0)') n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message)//TRIM(Process_ID_Tag), &
                              Error_Status, &
                              Message_Log=Message_Log )
        ! No return here. Continue deallocating
      END IF
    END DO

    ! Deallocate the structure array itself
    DEALLOCATE( SC, STAT=Allocate_Status )
    IF ( Allocate_Status/=0 ) THEN
      Error_Status = FAILURE
      WRITE(Message,'("Error deallocating SpcCoeff structure array. STAT=",i0)') Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message)//TRIM(Process_ID_Tag), &
                            Error_Status, &
                            Message_Log=Message_Log)
      ! Again, no return.
    END IF

    ! Reset the protected variable MAX_N_CHANNELS
    CALL CRTM_Reset_Max_nChannels()

  END FUNCTION CRTM_Destroy_SpcCoeff

END MODULE CRTM_SpcCoeff
