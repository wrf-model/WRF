!
! CRTM_EmisCoeff
!
! Module containing the Infrared Sea Surface Emissivity Model (IRSSEM)
! emissivity coefficient data (EmisCoeff) and their load/destruction
! routines. 
!
! PUBLIC DATA:
!       EmisC:  Data structure containing the IRSSE lookup table.
!
!
! SIDE EFFECTS:
!       Routines in this module modify the contents of the public
!       data structure EmisC.
!
! RESTRICTIONS:
!       Routines in this module should only be called during the
!       CRTM initialisation.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_EmisCoeff

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, Display_Message
  USE EmisCoeff_Define,    ONLY: EmisCoeff_type, Destroy_EmisCoeff
  USE EmisCoeff_Binary_IO, ONLY: Read_EmisCoeff_Binary
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! The shared data
  PUBLIC :: EmisC
  ! Procedures
  PUBLIC :: CRTM_Load_EmisCoeff
  PUBLIC :: CRTM_Destroy_EmisCoeff


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: CRTM_EmisCoeff.f90 1036 2007-10-16 20:23:14Z paul.vandelst@noaa.gov $'

  ! ---------------------------------
  ! The shared IRSSEM emissivity data
  ! ---------------------------------
  TYPE(EmisCoeff_type), SAVE :: EmisC


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Load_EmisCoeff
!
! PURPOSE:
!       Function to load the CRTM EmisCoeff data into the public data
!       structure EmisC.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Load_EmisCoeff( EmisCoeff_File,                      &  ! Input
!                                           Quiet            =Quiet,             &  ! Optional input
!                                           Process_ID       =Process_ID,        &  ! Optional input
!                                           Output_Process_ID=Output_Process_ID, &  ! Optional input
!                                           Message_Log      =Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       EmisCoeff_File:     Name of the Binary format EmisCoeff file.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
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
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the EmisCoeff read was successful
!                              == FAILURE an unrecoverable error occurred during
!                                         the data read.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure EmisC.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Load_EmisCoeff( EmisCoeff_File   , &  ! Input
                                Quiet            , &  ! Optional input
                                Process_ID       , &  ! Optional input
                                Output_Process_ID, &  ! Optional input
                                Message_Log      ) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: EmisCoeff_File
    INTEGER,      OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,      OPTIONAL, INTENT(IN)  :: Process_ID
    INTEGER,      OPTIONAL, INTENT(IN)  :: Output_Process_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Load_EmisCoeff'
    ! Local variables
    CHARACTER(256) :: Process_ID_Tag

    ! Setup 
    Error_Status = SUCCESS
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '(";  MPI Process ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Read the EmisCoeff data file
    Error_Status = Read_EmisCoeff_Binary( TRIM(EmisCoeff_File)               , &  ! Input
                                          EmisC                              , &  ! Output
                                          Quiet            =Quiet            , &
                                          Process_ID       =Process_ID       , &
                                          Output_Process_ID=Output_Process_ID, &
                                          Message_Log      =Message_Log        )
    IF ( Error_Status == FAILURE ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading IRSSE coefficients from '//&
                            TRIM(EmisCoeff_File)//TRIM(Process_ID_Tag), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Load_EmisCoeff 


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Destroy_EmisCoeff
!
! PURPOSE:
!       Function to deallocate the public data structure EmisC containing
!       the CRTM EmisCoeff data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_EmisCoeff( Process_ID =Process_ID, &  ! Optional input
!                                              Message_Log=Message_Log )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       Process_ID:   Set this argument to the MPI process ID that this
!                     function call is running under. This value is used
!                     solely for controlling message output. If MPI is not
!                     being used, ignore this argument.
!                     UNITS:      None
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to the screen.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the deallocation of the public EmisC data
!                                   structure was successful.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function modifies the contents of the public data structure EmisC.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_EmisCoeff( Process_ID,   &  ! Optional input
                                   Message_Log ) &  ! Error messaging
                                 RESULT( Error_Status )
    ! Arguments
    INTEGER,      OPTIONAL, INTENT(IN)  :: Process_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_EmisCoeff'
    ! Local variables
    CHARACTER(256) :: Process_ID_Tag

    ! Setup
    Error_Status = SUCCESS
    ! Create a process ID message tag for
    ! WARNING and FAILURE messages
    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '(";  MPI Process ID: ",i0)' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF

    ! Destroy the structure
    Error_Status = Destroy_EmisCoeff( EmisC, &
                                      Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred deallocating the public EmisC structure'//&
                            TRIM(Process_ID_Tag), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Destroy_EmisCoeff 

END MODULE CRTM_EmisCoeff
