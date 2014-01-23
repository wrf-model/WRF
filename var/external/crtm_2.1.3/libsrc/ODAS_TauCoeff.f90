!
! ODAS_TauCoeff
!
! Module containing the shared absorption coefficients (TauCoeff)
! and their load/destruction routines for the Optical Depth Absorber 
! Space (ODAS). 
!
! PUBLIC DATA:
!       TC:  Data structure array containing the transmittance model
!            coefficient data for the requested sensors.
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure TC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                       paul.vandelst@ssec.wisc.edu
!
!       Modifed by:     Yong Han, NESDIS/STAR 26-June-2008
!                       yong.han@noaa.gov
!
MODULE ODAS_TauCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE ODAS_Define       , ONLY: ODAS_TauCoeff_type    => ODAS_type, &          
                                ODAS_Destroy_TauCoeff => Destroy_ODAS        
  USE ODAS_Binary_IO    , ONLY: Read_TauCoeff_Binary  => Read_ODAS_Binary
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
  PUBLIC :: TC
  ! Structure defined in ODAS_Define
  PUBLIC :: ODAS_TauCoeff_type
  ! Public routines in this module
  PUBLIC :: Load_TauCoeff
  PUBLIC :: Destroy_TauCoeff


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: ODAS_TauCoeff.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'


  ! --------------------------------------
  ! The shared data for the gas absorption
  ! (AtmAbsorption) model
  ! --------------------------------------
  TYPE(ODAS_TauCoeff_type), SAVE, ALLOCATABLE, TARGET :: TC(:)


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       Load_TauCoeff
!
! PURPOSE:
!       Function to load the TauCoeff transmittance coefficient data into
!       the shared data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Load_TauCoeff( Sensor_ID        =Sensor_ID,         &  ! Optional input
!                                     File_Path        =File_Path,         &  ! Optional input      
!                                     Quiet            =Quiet,             &  ! Optional input      
!                                     Process_ID       =Process_ID,        &  ! Optional input      
!                                     Output_Process_ID=Output_Process_ID, &  ! Optional input      
!                                     Message_Log      =Message_Log        )  ! Error messaging     
!
! OPTIONAL INPUT ARGUMENTS:
!       Sensor_ID:          List of the sensor IDs (e.g. hirs3_n17, amsua_n18,
!                           ssmis_f16, etc) with which the CRTM is to be
!                           initialised. These Sensor ID are used to construct
!                           the sensor specific TauCoeff filenames containing
!                           the necessary coefficient data, i.e.
!                             <Sensor_ID>.TauCoeff.bin
!                           If this argument is not specified, the default
!                           TauCoeff filename is
!                             TauCoeff.bin
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Rank-1
!                           ATTRIBUTES: INTENT(IN)
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
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
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
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the TauCoeff data load was successful
!                              == FAILURE an unrecoverable error occurred.
!                              == WARNING the number of channels read in differs
!                                         from that stored in the CRTM_Parameters
!                                         module.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structures
!       in this module.
!
!------------------------------------------------------------------------------

  FUNCTION Load_TauCoeff( Sensor_ID        , &  ! Input
                          File_Path        , &  ! Optional input      
                          Quiet            , &  ! Optional input      
                          Process_ID       , &  ! Optional input      
                          Output_Process_ID, &  ! Optional input      
                          Message_Log      ) &  ! Error messaging     
                        RESULT( Error_Status )                        
    ! Arguments
    CHARACTER(*), DIMENSION(:), OPTIONAL, INTENT(IN) :: Sensor_ID
    CHARACTER(*),               OPTIONAL, INTENT(IN) :: File_Path
    INTEGER,                    OPTIONAL, INTENT(IN) :: Quiet
    INTEGER,                    OPTIONAL, INTENT(IN) :: Process_ID
    INTEGER,                    OPTIONAL, INTENT(IN) :: Output_Process_ID
    CHARACTER(*),               OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Load_TauCoeff'
    ! Local variables
    CHARACTER(256) :: Message
    CHARACTER(256) :: Process_ID_Tag
    CHARACTER(256), DIMENSION(MAX_N_SENSORS) :: TauCoeff_File
    INTEGER :: Allocate_Status
    INTEGER :: n, n_Sensors

    ! Set up
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
      n_Sensors = SIZE(Sensor_ID)
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
        TauCoeff_File(n) = TRIM(ADJUSTL(Sensor_ID(n)))//'.TauCoeff.bin'
      END DO
    ELSE
      ! No sensors specified. Use default filename.
      n_Sensors=1
      TauCoeff_File(1) = 'TauCoeff.bin'
    END IF
    
    ! Add the file path
    IF ( PRESENT(File_Path) ) THEN
      DO n=1,n_Sensors
        TauCoeff_File(n) = TRIM(ADJUSTL(File_Path))//TRIM(TauCoeff_File(n))
      END DO
    END IF
    
    ! Allocate the TauCoeff shared data structure array
    ALLOCATE(TC(n_Sensors), STAT=Allocate_Status)
    IF( Allocate_Status /= 0 )THEN
      WRITE(Message,'("TauCoeff structure array allocation failed. STAT=",i0)') Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message)//TRIM(Process_ID_Tag), & 
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    
    ! Read the TauCoeff data files
    DO n = 1, n_Sensors
      Error_Status = Read_TauCoeff_Binary( TRIM(TauCoeff_File(n))             , &  ! Input
                                           TC(n)                              , &  ! Output
                                           Quiet            =Quiet            , &
                                           Process_ID       =Process_ID       , &
                                           Output_Process_ID=Output_Process_ID, &
                                           Message_Log      =Message_Log        )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE(Message,'("Error reading TauCoeff file #",i0,", ",a)') &
                      n, TRIM(TauCoeff_File(n))
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message)//TRIM(Process_ID_Tag), &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END DO

  END FUNCTION Load_TauCoeff 


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_TauCoeff
!
! PURPOSE:
!       Function to deallocate the public shared data structure containing
!       the CRTM TauCoeff transmittance coefficient data.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_TauCoeff( Process_ID  = Process_ID, &  ! Optional input
!                                        Message_Log = Message_Log )  ! Error messaging
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
!                         If == SUCCESS the deallocation of the public TC data
!                                       structure was successful
!                            == FAILURE an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structures
!       in this module.
!
!------------------------------------------------------------------------------

  FUNCTION Destroy_TauCoeff( Process_ID,   &  ! Optional input
                             Message_Log ) &  ! Error messaging     
                           RESULT( Error_Status )                   
    ! Arguments
    INTEGER,      OPTIONAL, INTENT(IN)  :: Process_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_TauCoeff'
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
    DO n = 1, SIZE(TC)
      Destroy_Status = ODAS_Destroy_TauCoeff( TC(n), &
                                              Message_Log=Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE(Message,'("Error destroying TauCoeff structure array element #",i0)') n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message)//TRIM(Process_ID_Tag), &
                              Error_Status, &
                              Message_Log=Message_Log )
        ! No return here. Continue deallocating
      END IF
    END DO

    ! Deallocate the structure array
    DEALLOCATE(TC, STAT=Allocate_Status)
    IF( Allocate_Status /= 0 )THEN
      WRITE(Message,'("RTTOV TC structure deallocation failed. STAT=",i0)') Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message)//TRIM(Process_ID_Tag), &
                            Error_Status, &
                            Message_Log=Message_Log)
      
      ! Again, no return.
    END IF

    ! Reset the protected variable Max_n_Channels
    CALL CRTM_Reset_Max_nChannels()

  END FUNCTION Destroy_TauCoeff

END MODULE ODAS_TauCoeff
