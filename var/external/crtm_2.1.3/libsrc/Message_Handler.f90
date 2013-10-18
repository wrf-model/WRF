! Module to define simple error/exit codes
! and output messages.
!
MODULE Message_Handler

  ! Module use statements
  USE File_Utility, ONLY: Get_Lun

  ! Disable all implicit typing
  IMPLICIT NONE

  ! Visibilities
  PRIVATE
  ! Module parameters
  PUBLIC :: SUCCESS    
  PUBLIC :: INFORMATION
  PUBLIC :: WARNING    
  PUBLIC :: FAILURE    
  PUBLIC :: EOF        
  PUBLIC :: UNDEFINED
  ! Module procedures  
  PUBLIC :: Program_Message
  PUBLIC :: Display_Message
  PUBLIC :: Open_Message_Log

  ! Integer values that define the error or exit state.
  ! Note: These values are totally arbitrary.
  INTEGER, PARAMETER :: SUCCESS     = 0
  INTEGER, PARAMETER :: INFORMATION = 1
  INTEGER, PARAMETER :: WARNING     = 2
  INTEGER, PARAMETER :: FAILURE     = 3
  INTEGER, PARAMETER :: EOF         = 4
  INTEGER, PARAMETER :: UNDEFINED   = 5

  ! Character descriptors of the error states
  INTEGER,      PARAMETER :: MAX_N_STATES = 5
  CHARACTER(*), PARAMETER, DIMENSION( 0:MAX_N_STATES ) :: &
    STATE_DESCRIPTOR = (/ 'SUCCESS    ', &
                          'INFORMATION', &
                          'WARNING    ', &
                          'FAILURE    ', &
                          'END-OF-FILE', &
                          'UNDEFINED  ' /)


CONTAINS


  ! Subroutine to output a program header consisting of 
  ! the program name, description, and its revision
  !
  SUBROUTINE Program_Message( Name, Description, Revision )
    ! Arguments
    CHARACTER(*), INTENT(IN) :: Name
    CHARACTER(*), INTENT(IN) :: Description
    CHARACTER(*), INTENT(IN) :: Revision
    ! Local parameters
    CHARACTER(*), PARAMETER :: PROGRAM_HEADER = &
    '**********************************************************'
    CHARACTER(*), PARAMETER :: SPACE = ' '
    ! Local variables
    INTEGER       :: pn_pos
    CHARACTER(80) :: pn_fmt
    INTEGER :: phLen
    INTEGER :: dLen
    INTEGER :: i, i1, i2

    ! Determine the format for outputing the name
    pn_pos = ( LEN(PROGRAM_HEADER) / 2 ) - ( LEN_TRIM(ADJUSTL(Name)) / 2 )
    pn_pos = MAX( pn_pos, 0 ) + 5
    WRITE( pn_fmt, '( "( ",i2,"x, a, / )" )' ) pn_pos

    ! Write the program header and program name
    WRITE(*,'(/5x, a )' ) PROGRAM_HEADER
    WRITE(*,FMT=TRIM(pn_fmt)) TRIM(ADJUSTL(Name))

    ! Write the program description splitting lines at spaces
    phLen = LEN(PROGRAM_HEADER)-1
    dLen  = LEN_TRIM(Description)
    i1=1
    i2=phLen

    DO
      IF ( dLen > phLen ) THEN
        IF ( Description(i2:i2) /= SPACE .AND. i2 /= dLen) THEN
          ! Search for a space character
          i = INDEX( Description(i1:i2), SPACE, BACK=.TRUE. )
          IF ( i > 0 ) THEN
            ! Found one. Update end-of-line
            i2 = i1 + i - 1
          ELSE
            ! No space. Output rest of description
            i2 = dLen
          END IF
        END IF
      ELSE
        i2 = dLen
      END IF
      WRITE(*,'(6x, a )' ) Description(i1:i2)
      i1 = i2+1
      i2 = MIN(i1+phLen-1,dLen)
      IF ( i1 > dLen ) EXIT
    END DO

    ! Write the program revision and end header
    WRITE(*,'(/6x, a )' ) TRIM(Revision)
    WRITE(*,'(5x, a, / )' ) PROGRAM_HEADER

  END SUBROUTINE Program_Message


  ! Subroutine to display messages.
  !
  ! This routine calls itself if the optional argument Message_Log 
  ! is passed and an error occurs opening the output log file.
  !
  RECURSIVE SUBROUTINE Display_Message(Routine_Name, &
                                       Message,      &
                                       Error_State,  &
                                       Message_Log   )
    ! Arguments
    CHARACTER(*), INTENT(IN)           :: Routine_Name
    CHARACTER(*), INTENT(IN)           :: Message
    INTEGER,      INTENT(IN)           :: Error_State
    CHARACTER(*), INTENT(IN), OPTIONAL :: Message_Log
    ! Local parameters
    CHARACTER(*), PARAMETER :: THIS_ROUTINE_NAME = 'Display_Message'
    CHARACTER(*), PARAMETER :: FMT_STRING = '( 1x, a, "(", a, ") : ", a )'
    ! Local variables
    INTEGER :: Error_State_To_Use
    LOGICAL :: Log_To_StdOut
    INTEGER :: File_ID
    INTEGER :: Error_Status

    ! Check the input error state
    Error_State_To_Use = Error_State
    IF ( Error_State < 0 .OR. Error_State > MAX_N_STATES ) THEN
      Error_State_To_Use = UNDEFINED
    END IF

    ! Set the message log. Default is output to stdout
    Log_To_StdOut = .TRUE.
    IF ( PRESENT( Message_Log ) ) THEN
      Log_To_StdOut = .FALSE.
      Error_Status = Open_Message_Log( TRIM( Message_Log ), File_ID )
      IF ( Error_Status /= 0 ) THEN
        CALL Display_Message( THIS_ROUTINE_NAME, &
                              'Error opening message log file', &
                              FAILURE )
        Log_To_StdOut = .TRUE.
      END IF
    END IF

    ! Output the message
    IF ( Log_To_StdOut ) THEN
      WRITE( *, FMT = FMT_STRING ) &
                TRIM( Routine_Name ), &
                TRIM( STATE_DESCRIPTOR( Error_State_To_Use ) ), &
                TRIM( Message )
    ELSE
      WRITE( File_ID, FMT = FMT_STRING ) &
                      TRIM( Routine_Name ), &
                      TRIM( STATE_DESCRIPTOR( Error_State_To_Use ) ), &
                      TRIM( Message )
      CLOSE( File_ID )
    END IF

  END SUBROUTINE Display_Message


  ! Function to open the message log file.
  !
  ! SIDE EFFECTS:
  !   The file is opened for SEQUENTIAL, FORMATTED access with
  !   UNKNOWN status, position of APPEND, and action of READWRITE.
  !
  !   Hopefully all of these options will not cause an existing file
  !   to be inadvertantly overwritten.
  !
  FUNCTION Open_Message_Log(Message_Log, File_ID) RESULT(Error_Status)
    ! Arguments
    CHARACTER(*), INTENT(IN)  :: Message_Log
    INTEGER,      INTENT(OUT) :: File_ID
    ! Function result
    INTEGER :: Error_Status
    ! Local variables
    INTEGER :: Lun
    INTEGER :: IO_Status

    ! Set successful return status
    Error_Status = SUCCESS

    ! Get a file unit number
    Lun = Get_Lun()
    IF ( Lun < 0 ) THEN
      Error_Status = FAILURE
      RETURN
    END IF

    ! Open the file
    OPEN( Lun, FILE     = TRIM( Message_Log ), &
               ACCESS   = 'SEQUENTIAL', &
               FORM     = 'FORMATTED', &
               STATUS   = 'UNKNOWN', &
               POSITION = 'APPEND', &
               ACTION   = 'READWRITE', &
               IOSTAT   = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      RETURN
    END IF

    ! Return the file ID
    File_ID = Lun

  END FUNCTION Open_Message_Log

END MODULE Message_Handler
