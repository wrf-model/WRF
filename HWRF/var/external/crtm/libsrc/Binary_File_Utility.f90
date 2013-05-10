!
! Binary_File_Utility
!
! Module for utility routines for "Binary" datafiles (unformatted,
! sequential) that conform to the required format.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Binary_File_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,      ONLY: Long, n_Bytes_Long
  USE File_Utility,    ONLY: Get_Lun, File_Exists
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Endian_Utility,  ONLY: Swap_Endian
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Open_Binary_File


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Binary_File_Utility.f90 6551 2010-02-02 00:05:49Z paul.vandelst@noaa.gov $'
  ! Magic number header value for byte-swap checks
  INTEGER(Long), PARAMETER :: MAGIC_NUMBER = 123456789_Long


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Open_Binary_File
!
! PURPOSE:
!       Function to open the unformatted, sequential access Binary files
!
! CALLING SEQUENCE:
!       Error_Status = Open_Binary_File( Filename,                 &  ! Input
!                                        FileID,                   &  ! Output
!                                        For_Output  = For_Output, &  ! Optional input
!                                        No_Check    = No_Check  , &  ! Optional input
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUTS:
!       Filename:         Name of the Binary file to open.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       For_Output:       Set this optional argument to open a new file for
!                         writing. Default action is to open an existing file
!                         for read access. Note, if the file already exists and
!                         it is opened with this keyword set, the file is
!                         overwritten.
!                         If == 0, existing file is opened for READ access (DEFAULT)
!                                  ACTION='READ', STATUS='OLD'
!                            == 1, new file is opened for WRITE access.
!                                  ACTION='WRITE', STATUS='REPLACE'
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_Check:         Set this optional argument to suppress the byte-order
!                         check made on an existing file by NOT reading the file
!                         header magic number.  Default action is to check the
!                         file. This argument is ignored if the FOR_OUTPUT 
!                         optional argument is set.
!                         If == 0, existing file magic number is read and the
!                                  byte order is checked (DEFAULT)
!                            == 1, magic number is *NOT* read from file and
!                                  checked for validity.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         Messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output Messages to the screen.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUTS:
!       FileID:           File unit number.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the
!                         error status. The error codes are defined in
!                         the Message_Handler module. Values returned by
!                         this function are:
!                           SUCCESS == file open was successful
!                           FAILURE == - error occurred during file open,
!                                      - error occurred during file check.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Open_Binary_File( Filename,     &  ! Input
                             FileID,       &  ! Output
                             For_Output,   &  ! Optional input
                             No_Check,     &  ! Optional input
                             Message_Log ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,                INTENT(OUT) :: FileID
    INTEGER,      OPTIONAL, INTENT(IN)  :: For_Output
    INTEGER,      OPTIONAL, INTENT(IN)  :: No_Check
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Open_Binary_File'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: File_Check
    LOGICAL :: File_Input
    INTEGER :: IO_Status
    INTEGER(Long) :: Magic_Number_Read
    CHARACTER(7) :: File_Status
    CHARACTER(5) :: File_Action

    ! Set up
    Error_Status = SUCCESS

    ! Default action is to check the file byte order...
    File_Check = .TRUE.
    ! Unless the No_Check argument is set
    IF ( PRESENT( No_Check ) ) THEN
      IF ( No_Check == 1 ) File_Check = .FALSE.
    END IF

    ! Default action is to READ file
    File_Input = .TRUE.
    ! ...unless the For_Output keyword is set
    IF ( PRESENT( For_Output ) ) THEN
      IF ( For_Output == 1 ) THEN
        File_Input = .FALSE.
        File_Check = .FALSE.
      END IF
    END IF

    ! Branch depending on type of file I/O
    IF ( File_Input ) THEN
      ! File is to be READ. If the file
      ! does not exist, return an error
      IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAMe, &
                              'File '//TRIM( Filename )//' not found.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
      ! Set OPEN keywords for READING
      File_Status = 'OLD'
      File_Action = 'READ'
    ELSE
      ! File is to be WRITTEN.
      ! Set OPEN keywords for WRITING
      File_Status = 'REPLACE'
      File_Action = 'WRITE'
    END IF

    ! Check the file byte order
    IF ( File_Check ) THEN
      Error_Status = Check_Binary_File( TRIM( Filename ), &
                                        Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error checking ", a )' ) &
                        TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! Get a free unit number
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Open the file
    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = TRIM( File_Status ), &
                  ACTION = TRIM( File_Action ), &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'UNFORMATTED', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Skip past, or write the magic number
    IF ( File_Input ) THEN
      READ( FileID, IOSTAT=IO_Status ) Magic_Number_Read
      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading magic number from ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF
    ELSE
      WRITE( FileID, IOSTAT = IO_Status ) MAGIC_NUMBER
      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error writing magic number to ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF
    END IF
    
  END FUNCTION Open_Binary_File


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Check_Binary_File
!
! PURPOSE:
!       Function to determine if the unformatted Binary file is in the correct
!       byte order.
!
! CALLING SEQUENCE:
!       Error_Status = Check_Binary_File( Filename                  &  ! Input
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUTS:
!       Filename:         Name of the Binary file to check.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Message_Log:      Character string specifying a filename in which any
!                         Messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output Messages to the screen.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the
!                         error status. The error codes are defined in
!                         the Message_Handler module. Values returned by
!                         this function are:
!                           SUCCESS == file check was successful
!                           FAILURE == - error occurred reading a file record,
!                                      - 8- and/or 32-bit integers not supported.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Check_Binary_File( Filename,     &
                              Message_Log ) &
                            RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*),  PARAMETER :: ROUTINE_NAME = 'Check_Binary_File'
    ! Local variables
    CHARACTER(256) :: Message
    INTEGER :: FileID
    INTEGER :: IO_Status
    INTEGER(Long) :: Magic_Number_Read
    INTEGER(Long) :: Magic_Number_Swapped

    ! Set up
    Error_Status = SUCCESS

    ! Check that 4-byte integers are supported
    IF ( BIT_SIZE( 1_Long ) /= 32 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            '32-bit integers not supported. '//&
                            'Unable to determine endian-ness', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Get a free unit number
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Open the file as direct access
    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = 'OLD', &
                  ACTION = 'READ', &
                  ACCESS = 'DIRECT', &
                  FORM   = 'UNFORMATTED', &
                  RECL   = n_Bytes_Long, &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! Read the magic number
    READ( FileID, REC=2, IOSTAT=IO_Status ) Magic_Number_Read
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading file magic number. IOSTAT = ", i5 )' ) &
                      IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! Close the file
    CLOSE( FileID )

    ! Compare the magic numbers
    IF ( Magic_Number_Read /= MAGIC_NUMBER ) THEN

      ! Set the return error status
      Error_Status = FAILURE

      ! Byte swap the magic number
      Magic_Number_Swapped = Swap_Endian( Magic_Number_Read )

      ! Check the magic number again
      IF ( Magic_Number_Swapped /= MAGIC_NUMBER ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Unrecognised file format. Invalid magic number.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! If we get here then the data does need to be byte-swapped
      CALL Display_Message( ROUTINE_NAME, &
                            'Data file needs to be byte-swapped.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN

    END IF

  END FUNCTION Check_Binary_File

END MODULE Binary_File_Utility
