!
! CRTM_IRlandCoeff
!
! Module containing the shared CRTM infrared land surface emissivity
! data and their load/destruction routines.
!
! PUBLIC DATA:
!   IRlandC:  Data structure containing the infrared land surface
!             emissivity data.
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure IRlandC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 19-Aug-2011
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_IRlandCoeff

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
  PUBLIC :: IRlandC
  ! Procedures
  PUBLIC :: CRTM_IRlandCoeff_Load
  PUBLIC :: CRTM_IRlandCoeff_Destroy
  PUBLIC :: CRTM_IRlandCoeff_IsLoaded
  PUBLIC :: CRTM_IRlandCoeff_Classification


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_IRlandCoeff.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 512


  ! ------------------------------------------------
  ! The shared infrared land surface emissivity data
  ! ------------------------------------------------
  TYPE(SEcategory_type), SAVE :: IRlandC


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_IRlandCoeff_Load
!
! PURPOSE:
!       Function to load the infrared land surface emissivity data into
!       the public data structure IRlandC
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_IRlandCoeff_Load( &
!                        Filename,                              &
!                        File_Path         = File_Path        , &
!                        Quiet             = Quiet            , &
!                        Process_ID        = Process_ID       , &
!                        Output_Process_ID = Output_Process_ID  )
!
! INPUT ARGUMENTS:
!       Filename:           Name of the IRlandCoeff file.
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
!       structure IRlandC.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_IRlandCoeff_Load( &
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_IRlandCoeff_Load'
    ! Local variables
    CHARACTER(ML) :: msg, pid_msg
    CHARACTER(ML) :: IRlandCoeff_File
    LOGICAL :: noisy

    ! Setup
    err_stat = SUCCESS
    ! ...Assign the filename to local variable
    IRlandCoeff_File = ADJUSTL(Filename)
    ! ...Add the file path
    IF ( PRESENT(File_Path) ) IRlandCoeff_File = TRIM(ADJUSTL(File_Path))//TRIM(IRlandCoeff_File)
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


    ! Read the IR land SEcategory file
    err_stat = SEcategory_ReadFile( &
                 IRlandC, &
                 IRlandCoeff_File, &
                 Quiet = .NOT. noisy )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading IRlandCoeff SEcategory file '//TRIM(IRlandCoeff_File)//TRIM(pid_msg)
      CALL Load_Cleanup(); RETURN
    END IF


   CONTAINS

     SUBROUTINE Load_CleanUp()
       CALL SEcategory_Destroy( IRlandC )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Load_CleanUp

  END FUNCTION CRTM_IRlandCoeff_Load


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_IRlandCoeff_Destroy
!
! PURPOSE:
!       Function to deallocate the public data structure IRlandC containing
!       the CRTM infrared land surface emissivity data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_IRlandCoeff_Destroy( Process_ID = Process_ID )
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
!       structure IRlandC.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_IRlandCoeff_Destroy( Process_ID ) RESULT( err_stat )
    ! Arguments
    INTEGER, OPTIONAL, INTENT(IN) :: Process_ID
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_IRlandCoeff_Destroy'
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
    CALL SEcategory_Destroy( IRlandC )
    IF ( SEcategory_Associated( IRlandC ) ) THEN
      err_stat = FAILURE
      msg = 'Error deallocating IRlandCoeff shared data structure'//TRIM(pid_msg)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF

  END FUNCTION CRTM_IRlandCoeff_Destroy

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_IRlandCoeff_Classification
!
! PURPOSE:
!       Function to return the classification name of the public
!       IRlandC structure
!
! CALLING SEQUENCE:
!       Classification = CRTM_IRlandCoeff_Classification()
!
! FUNCTION RESULT:
!   Classification:       The classification name field of IRlandC
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  PURE FUNCTION CRTM_IRlandCoeff_Classification() RESULT( Classification )
    CHARACTER(LEN(IRlandC%Classification_Name)) :: Classification
    Classification = IRlandC%Classification_Name
  END FUNCTION

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_IRlandCoeff_IsLoaded
!
! PURPOSE:
!       Function to test if infrared land surface emissivity data has
!       been loaded into the public data structure IRlandC.
!
! CALLING SEQUENCE:
!       status = CRTM_IRlandCoeff_IsLoaded()
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_IRlandCoeff_IsLoaded() RESULT( IsLoaded )
    LOGICAL :: IsLoaded
    IsLoaded = SEcategory_Associated( IRlandC )
  END FUNCTION CRTM_IRlandCoeff_IsLoaded

END MODULE CRTM_IRlandCoeff
