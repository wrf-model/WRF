!
! CRTM_BeCoeff
!
! Module containing the shared CRTM geomagnetic field data (BeCoeff)
! and their load/destruction routines. 
!
! PUBLIC DATA:
!       BeC:  Data structure containing the geomagentic field data
!             data for the requested sensors.
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure BeC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 14-Feb-2010
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_BeCoeff

  ! ----------------
  ! Enviroment setup
  ! ----------------
  ! Module use
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE BeCoeff_Define   , ONLY: BeCoeff_type, BeCoeff_Associated, BeCoeff_Destroy
  USE BeCoeff_Binary_IO, ONLY: BeCoeff_Binary_ReadFile
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! The shared data
  PUBLIC :: BeC
  ! Public routines in this module
  PUBLIC :: CRTM_BeCoeff_Load
  PUBLIC :: CRTM_BeCoeff_Destroy
  

  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*),  PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_BeCoeff.f90 6629 2010-02-14 19:48:43Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! ---------------------------------
  ! The shared geomagnetic field data
  ! ---------------------------------
  TYPE(BeCoeff_type), SAVE :: BeC


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_BeCoeff_Load
!
! PURPOSE:
!       Function to load the BeCoeff geomagnetic field data into
!       the public data structure BeC.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_BeCoeff_Load( &
!                        Filename, &
!                        File_Path         = File_Path        , &
!                        Quiet             = Quiet            , &
!                        Process_ID        = Process_ID       , &
!                        Output_Process_ID = Output_Process_ID  )
!
! INPUTS:
!       Filename:           Name of the Binary format BeCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       File_Path:          Character string specifying a file path for the
!                           input data file. If not specified, the current
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
!                           solely for controlling INFORMATION message output.
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
!                           If MPI is not being used, ignore this argument.
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
!                           If == SUCCESS the BeCoeff data load was successful.
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure BeC.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_BeCoeff_Load( &
    Filename         , &  ! Input
    File_Path        , &  ! Optional input
    Quiet            , &  ! Optional input
    Process_ID       , &  ! Optional input
    Output_Process_ID) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: File_Path
    LOGICAL     , OPTIONAL, INTENT(IN)  :: Quiet             
    INTEGER     , OPTIONAL, INTENT(IN)  :: Process_ID        
    INTEGER     , OPTIONAL, INTENT(IN)  :: Output_Process_ID 
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_BeCoeff_Load'
    ! Local variables
    CHARACTER(ML) :: msg, pid_msg
    CHARACTER(ML) :: BeCoeff_File
    LOGICAL :: noisy

    ! Setup 
    err_stat = SUCCESS
    ! ...Assign the filename to local variable
    BeCoeff_File = ADJUSTL(Filename)
    ! ...Add the file path
    IF ( PRESENT(File_Path) ) BeCoeff_File = TRIM(ADJUSTL(File_Path))//TRIM(BeCoeff_File)
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
    
    ! Read the BeCoeff data file
    err_stat = BeCoeff_Binary_ReadFile( &
                 BeCoeff_File, &
                 BeC, &
                 Quiet = .NOT. noisy )
    IF ( err_stat /= SUCCESS ) THEN
      WRITE( msg,'("Error reading BeCoeff file ",a)') TRIM(BeCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF

  END FUNCTION CRTM_BeCoeff_Load


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_BeCoeff_Destroy
!
! PURPOSE:
!       Function to deallocate the public data structure, BeC, that
!       contains the BeCoeff geomagnetic field data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_BeCoeff_Destroy( Process_ID = Process_ID )
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
!                         If == SUCCESS the deallocation of the public data
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

  FUNCTION CRTM_BeCoeff_Destroy( Process_ID ) RESULT( err_stat )
    ! Arguments
    INTEGER, OPTIONAL, INTENT(IN) :: Process_ID
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_BeCoeff_Destroy'
    ! Local variables
    CHARACTER(ML) :: msg, pid_msg

    ! Set up
    err_stat = SUCCESS
    ! ...Create a process ID message tag for error messages
    IF ( PRESENT(Process_Id) ) THEN
      WRITE( pid_msg,'("; Process ID: ",i0)' ) Process_ID
    ELSE
      pid_msg = ''
    END IF

    ! Destroy the structure array elements
    CALL BeCoeff_Destroy( BeC )
    IF ( BeCoeff_Associated( BeC ) ) THEN
      err_stat = FAILURE
      msg = 'Error deallocating BeCoeff shared data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
      RETURN
    END IF

  END FUNCTION CRTM_BeCoeff_Destroy

END MODULE CRTM_BeCoeff
