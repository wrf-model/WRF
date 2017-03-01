!
! CRTM_CloudCoeff
!
! Module containing the shared CRTM scattering coefficient data
! (CloudCoeff) and their load/destruction routines. 
!
! PUBLIC DATA:
!       CloudC:  Data structure containing the cloud bulk optical
!                properties data
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure CloudC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 24-Jun-2004
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_CloudCoeff

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Message_Handler,      ONLY: SUCCESS, FAILURE, Display_Message
  USE CloudCoeff_Define,    ONLY: CloudCoeff_type, &
                                  CloudCoeff_Associated, &
                                  CloudCoeff_Destroy                  
  USE CloudCoeff_Binary_IO, ONLY: CloudCoeff_Binary_ReadFile
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! The shared data
  PUBLIC :: CloudC
  ! Procedures
  PUBLIC :: CRTM_CloudCoeff_Load
  PUBLIC :: CRTM_CloudCoeff_Destroy
  PUBLIC :: CRTM_CloudCoeff_IsLoaded


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_CloudCoeff.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! ---------------------------------
  ! The shared cloud coefficient data
  ! ---------------------------------
  TYPE(CloudCoeff_type), TARGET, SAVE :: CloudC


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_CloudCoeff_Load
!
! PURPOSE:
!       Function to load the CloudCoeff scattering coefficient data into
!       the public data structure CloudC.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_CloudCoeff_Load( &
!                        Filename,                              &
!                        File_Path         = File_Path        , &
!                        Quiet             = Quiet            , &
!                        Process_ID        = Process_ID       , &
!                        Output_Process_ID = Output_Process_ID  )
!
! INPUT ARGUMENTS:
!       Filename:           Name of the Binary format CloudCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
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
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the CloudCoeff data load was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure CloudC.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_CloudCoeff_Load( &
    Filename         , &  ! Input
    File_Path        , &  ! Optional input
    Quiet            , &  ! Optional input
    Process_ID       , &  ! Optional input
    Output_Process_ID) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    CHARACTER(*), OPTIONAL, INTENT(IN) :: File_Path
    LOGICAL     , OPTIONAL, INTENT(IN) :: Quiet             
    INTEGER     , OPTIONAL, INTENT(IN) :: Process_ID
    INTEGER     , OPTIONAL, INTENT(IN) :: Output_Process_ID
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_CloudCoeff_Load'
    ! Local variables
    CHARACTER(ML) :: msg, pid_msg
    CHARACTER(ML) :: CloudCoeff_File
    LOGICAL :: noisy

    ! Setup 
    err_stat = SUCCESS
    ! ...Assign the filename to local variable
    CloudCoeff_File = ADJUSTL(Filename)
    ! ...Add the file path
    IF ( PRESENT(File_Path) ) CloudCoeff_File = TRIM(ADJUSTL(File_Path))//TRIM(CloudCoeff_File)
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
    
    ! Read the CloudCoeff data file
    err_stat = CloudCoeff_Binary_ReadFile( &
                 CloudCoeff_File, &
                 CloudC, &
                 Quiet = .NOT. noisy )
    IF ( err_stat /= SUCCESS ) THEN
      WRITE( msg,'("Error reading CloudCoeff file ",a)') TRIM(CloudCoeff_File)
      CALL Display_Message( ROUTINE_NAME,TRIM(msg)//TRIM(pid_msg),err_stat )
      RETURN
    END IF

  END FUNCTION CRTM_CloudCoeff_Load


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_CloudCoeff_Destroy
!
! PURPOSE:
!       Function to deallocate the public data structure CloudC containing
!       the CRTM CloudCoeff scattering coefficient data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_CloudCoeff_Destroy( Process_ID =Process_ID )
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
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error
!                         status. The error codes are defined in the
!                         Message_Handler module.
!                         If == SUCCESS the deallocation of the public CloudC data
!                                       structure was successful
!                            == FAILURE an unrecoverable error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure CloudC.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_CloudCoeff_Destroy( Process_ID ) RESULT( err_stat )
    ! Arguments
    INTEGER, OPTIONAL, INTENT(IN) :: Process_ID
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_CloudCoeff_Destroy'
    ! Local variables
    CHARACTER(ML) :: msg, pid_msg

    ! Setup
    err_stat = SUCCESS
    ! ...Create a process ID message tag for error messages
    IF ( PRESENT(Process_Id) ) THEN
      WRITE( pid_msg,'("; Process ID: ",i0)' ) Process_ID
    ELSE
      pid_msg = ''
    END IF

    ! Destroy the structure
    CALL CloudCoeff_Destroy( CloudC )
    IF ( CloudCoeff_Associated( CloudC ) ) THEN
      err_stat = FAILURE
      msg = 'Error deallocating CloudCoeff shared data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
      RETURN
    END IF

  END FUNCTION CRTM_CloudCoeff_Destroy


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_CloudCoeff_IsLoaded
!
! PURPOSE:
!       Function to test if the CloudCoeff scattering coefficient data has
!       loaded into the public data structure CloudC.
!
! CALLING SEQUENCE:
!       status = CRTM_CloudCoeff_IsLoaded()
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_CloudCoeff_IsLoaded() RESULT( IsLoaded )
    LOGICAL :: IsLoaded
    IsLoaded = CloudCoeff_Associated( CloudC )
  END FUNCTION CRTM_CloudCoeff_IsLoaded

END MODULE CRTM_CloudCoeff
