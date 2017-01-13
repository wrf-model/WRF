!
! CRTM_VISwaterCoeff
!
! Module containing the shared CRTM visible water surface emissivity
! data and their load/destruction routines. 
!
! PUBLIC DATA:
!   VISwaterC:  Data structure containing the visible water surface
!               emissivity data.
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure VISwaterC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 20-Jan-2012
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_VISwaterCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, Display_Message
  USE SEcategory_Define, ONLY: SEcategory_type, &
                               SEcategory_Associated, &
                               SEcategory_Destroy, &
                               SEcategory_ReadFile
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! The shared data
  PUBLIC :: VISwaterC
  ! Procedures
  PUBLIC :: CRTM_VISwaterCoeff_Load
  PUBLIC :: CRTM_VISwaterCoeff_Destroy
  PUBLIC :: CRTM_VISwaterCoeff_IsLoaded


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_VISwaterCoeff.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 512


  ! ------------------------------------------------
  ! The shared visible water surface emissivity data
  ! ------------------------------------------------
  TYPE(SEcategory_type), SAVE :: VISwaterC


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_VISwaterCoeff_Load
!
! PURPOSE:
!       Function to load the visible water surface emissivity data into
!       the public data structure VISwaterC
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_VISwaterCoeff_Load( &
!                        Filename,                              &
!                        File_Path         = File_Path        , &
!                        Quiet             = Quiet            , &
!                        Process_ID        = Process_ID       , &
!                        Output_Process_ID = Output_Process_ID  )
!
! INPUT ARGUMENTS:
!       Filename:           Name of the VISwaterCoeff file.
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
!                           If == SUCCESS the data load was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data
!       structure VISwaterC.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_VISwaterCoeff_Load( &
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_VISwaterCoeff_Load'
    ! Local variables
    CHARACTER(ML) :: msg, pid_msg
    CHARACTER(ML) :: VISwaterCoeff_File
    LOGICAL :: noisy

    ! Setup 
    err_stat = SUCCESS
    ! ...Assign the filename to local variable
    VISwaterCoeff_File = ADJUSTL(Filename)
    ! ...Add the file path
    IF ( PRESENT(File_Path) ) VISwaterCoeff_File = TRIM(ADJUSTL(File_Path))//TRIM(VISwaterCoeff_File)
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
    
    
    ! Read the VIS water SEcategory file
    err_stat = SEcategory_ReadFile( &
                 VISwaterC, &
                 VISwaterCoeff_File, &
                 Quiet = .NOT. noisy )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading VISwaterCoeff SEcategory file '//TRIM(VISwaterCoeff_File)//TRIM(pid_msg)
      CALL Load_Cleanup(); RETURN
    END IF


   CONTAINS
   
     SUBROUTINE Load_CleanUp()
       CALL SEcategory_Destroy( VISwaterC )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Load_CleanUp

  END FUNCTION CRTM_VISwaterCoeff_Load


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_VISwaterCoeff_Destroy
!
! PURPOSE:
!       Function to deallocate the public data structure VISwaterC containing
!       the CRTM visible water surface emissivity data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_VISwaterCoeff_Destroy( Process_ID = Process_ID )
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
!       This function modifies the contents of the public data
!       structure VISwaterC.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_VISwaterCoeff_Destroy( Process_ID ) RESULT( err_stat )
    ! Arguments
    INTEGER, OPTIONAL, INTENT(IN) :: Process_ID
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_VISwaterCoeff_Destroy'
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
    CALL SEcategory_Destroy( VISwaterC )
    IF ( SEcategory_Associated( VISwaterC ) ) THEN
      err_stat = FAILURE
      msg = 'Error deallocating VISwaterCoeff shared data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF

  END FUNCTION CRTM_VISwaterCoeff_Destroy

  
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_VISwaterCoeff_IsLoaded
!
! PURPOSE:
!       Function to test if visible water surface emissivity data has
!       been loaded into the public data structure VISwaterC.
!
! CALLING SEQUENCE:
!       status = CRTM_VISwaterCoeff_IsLoaded()
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_VISwaterCoeff_IsLoaded() RESULT( IsLoaded )
    LOGICAL :: IsLoaded
    IsLoaded = SEcategory_Associated( VISwaterC )
  END FUNCTION CRTM_VISwaterCoeff_IsLoaded

END MODULE CRTM_VISwaterCoeff
