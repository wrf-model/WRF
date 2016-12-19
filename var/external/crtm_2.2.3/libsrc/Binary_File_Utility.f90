!
! Binary_File_Utility
!
! Module for utility routines for "Binary" datafiles (unformatted,
! sequential) that conform to the required format.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 12-Jun-2000
!                       paul.vandelst@noaa.gov
!

MODULE Binary_File_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,      ONLY: Long, n_Bytes_Long
  USE File_Utility,    ONLY: Get_Lun, File_Exists, File_Open
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Endian_Utility,  ONLY: Swap_Endian
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Open_Binary_File
  PUBLIC :: WriteGAtts_Binary_File
  PUBLIC :: ReadGAtts_Binary_File
  PUBLIC :: WriteLogical_Binary_File
  PUBLIC :: ReadLogical_Binary_File


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE WriteLogical_Binary_File
    MODULE PROCEDURE WriteLogical_Scalar
    MODULE PROCEDURE WriteLogical_Rank1
  END INTERFACE WriteLogical_Binary_File

  INTERFACE ReadLogical_Binary_File
    MODULE PROCEDURE ReadLogical_Scalar
    MODULE PROCEDURE ReadLogical_Rank1
  END INTERFACE ReadLogical_Binary_File


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Binary_File_Utility.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Magic number header value for byte-swap checks
  INTEGER(Long), PARAMETER :: MAGIC_NUMBER = 123456789_Long
  ! Integer "logicals" for I/O
  INTEGER(Long), PARAMETER :: FALSE = 0_Long
  INTEGER(Long), PARAMETER :: TRUE  = 1_Long
  ! String lengths
  INTEGER, PARAMETER :: ML = 256   ! For messages
  INTEGER, PARAMETER :: GL = 5000  ! For local global attribute values
  ! Global attribute names
  CHARACTER(*), PARAMETER :: WRITE_MODULE_GATTNAME = 'write_module'
  CHARACTER(*), PARAMETER :: CREATED_ON_GATTNAME   = 'created_on'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME        = 'title'
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME      = 'history'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME      = 'comment'
  

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
!       Function to open unformatted, sequential access "Binary" files
!
! CALLING SEQUENCE:
!       Error_Status = Open_Binary_File( Filename,                &
!                                        FileID,                  &
!                                        For_Output = For_Output, &
!                                        No_Check   = No_Check    )
!
! INPUTS:
!       Filename:         Name of the Binary file to open.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       For_Output:       Set this logical argument to open a new file for
!                         writing. Default action is to open an existing file
!                         for read access. Note, if the file already exists and
!                         it is opened with this keyword set, the file is
!                         overwritten.
!                         If == .FALSE., existing file is opened for READ access (DEFAULT)
!                                        ACTION='READ', STATUS='OLD'
!                            == .TRUE. , new file is opened for WRITE access.
!                                        ACTION='WRITE', STATUS='REPLACE'
!                         UNITS:      N/A
!                         TYPE:       LOGICAL
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_Check:         Set this logical argument to suppress the byte-order
!                         check made on an existing file by NOT reading the file
!                         header magic number.  Default action is to check the
!                         file. This argument is ignored if the FOR_OUTPUT 
!                         optional argument is set.
!                         If == .FALSE., existing file magic number is read and the
!                                        byte order is checked (DEFAULT)
!                            == .TRUE.,  magic number is *NOT* read from file and
!                                        checked for validity.
!                         UNITS:      N/A
!                         TYPE:       LOGICAL
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
!                           FAILURE == an unrecoverable error occurred
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Open_Binary_File( &
    Filename,   &  ! Input
    FileID,     &  ! Output
    For_Output, &  ! Optional input
    No_Check  ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,                INTENT(OUT) :: FileID
    LOGICAL,      OPTIONAL, INTENT(IN)  :: For_Output
    LOGICAL,      OPTIONAL, INTENT(IN)  :: No_Check
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Open_Binary_File'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: file_check
    LOGICAL :: file_input
    INTEGER :: io_stat
    INTEGER(Long) :: magic_number_read
    CHARACTER(7) :: file_status
    CHARACTER(5) :: file_action

    ! Set up
    err_stat = SUCCESS
    ! ...Check the For_Output argument
    file_input = .TRUE.
    IF ( PRESENT(For_Output) ) file_input = .NOT. For_Output
    ! ...Check the No_Check argument
    file_check = file_input
    IF ( PRESENT(No_Check) ) file_check = (.NOT. No_Check) .AND. file_input


    ! Branch depending on type of file I/O
    IF ( file_input ) THEN
      ! Set OPEN keywords for READING
      file_status = 'OLD'
      file_action = 'READ'
    ELSE
      ! Set OPEN keywords for WRITING
      file_status = 'REPLACE'
      file_action = 'WRITE'
    END IF


    ! Check the file byte order
    IF ( file_check ) THEN
      err_stat = Check_Binary_File( Filename )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error checking '//TRIM(Filename)//' file byte order'
        CALL CleanUp(); RETURN
      END IF
    END IF


    ! Get a free unit number
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      msg = 'Error obtaining file unit number for '//TRIM(Filename)
      CALL CleanUp(); RETURN
    END IF


    ! Open the file
    OPEN( FileID, FILE   = Filename     , &
                  STATUS = file_status  , &
                  ACTION = file_action  , &
                  ACCESS = 'SEQUENTIAL' , &
                  FORM   = 'UNFORMATTED', &
                  IOSTAT = io_stat      , &
                  IOMSG  = io_msg         )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error opening '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL CleanUp(); RETURN
    END IF


    ! Skip past, or write the magic number
    IF ( File_Input ) THEN
      READ( FileID, IOSTAT=io_stat, IOMSG=io_msg ) magic_number_read
      IF ( io_stat /= 0 ) THEN
        msg = 'Error reading magic number from '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL CleanUp(); RETURN
      END IF
    ELSE
      WRITE( FileID, IOSTAT=io_stat, IOMSG=io_msg ) MAGIC_NUMBER
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing magic number to '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL CleanUp(); RETURN
      END IF
    END IF
    
  CONTAINS
   
     SUBROUTINE CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( FileID, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE CleanUp

  END FUNCTION Open_Binary_File





  ! Function to write standard global attributes to a Binary file.

  FUNCTION WriteGAtts_Binary_File( &
    fid         , &  ! Input
    Write_Module, &  ! Optional input
    Created_On  , &  ! Optional input
    Title       , &  ! Optional input
    History     , &  ! Optional input
    Comment     ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    INTEGER     ,           INTENT(IN) :: fid
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Write_Module
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Created_On  
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteGAtts_Binary_File'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: io_stat

    ! Set up
    err_stat = SUCCESS
    msg = ''

    ! Software ID
    CALL WriteSingleGAtt( WRITE_MODULE_GATTNAME, gattvalue = Write_Module )
    IF ( err_stat /= SUCCESS ) RETURN
    
    ! Creation date/time
    CALL DATE_AND_TIME( cdate, ctime, czone )
    IF ( PRESENT(Created_On) ) THEN
      CALL WriteSingleGAtt( CREATED_ON_GATTNAME, gattvalue = Created_On )
    ELSE
      CALL WriteSingleGAtt( CREATED_ON_GATTNAME, &
                            gattvalue = &
                              cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                              ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                              czone//'UTC')
    END IF
    IF ( err_stat /= SUCCESS ) RETURN


    ! The title
    CALL WriteSingleGAtt( TITLE_GATTNAME, gattvalue = Title )
    IF ( err_stat /= SUCCESS ) RETURN


    ! The history
    CALL WriteSingleGAtt( HISTORY_GATTNAME, gattvalue = History )
    IF ( err_stat /= SUCCESS ) RETURN


    ! The comment
    CALL WriteSingleGAtt( COMMENT_GATTNAME, gattvalue = Comment )
    IF ( err_stat /= SUCCESS ) RETURN
    
  CONTAINS
  
    SUBROUTINE WriteSingleGAtt(gattname, gattvalue)
      CHARACTER(*),           INTENT(IN) :: gattname
      CHARACTER(*), OPTIONAL, INTENT(IN) :: gattvalue
      INTEGER :: gattlen
      CHARACTER(GL) :: l_gattvalue
      ! Setup
      l_gattvalue = ''
      IF ( PRESENT(gattvalue) ) THEN
        IF ( LEN_TRIM(gattvalue) /= 0 ) l_gattvalue = TRIM(gattname)//': '//TRIM(gattvalue)
      END IF
      gattlen = LEN_TRIM(l_gattvalue)
      ! Write the string length
      WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) gattlen
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing '//TRIM(gattname)//' attribute length - '//TRIM(io_msg)
        CALL WriteGatts_Cleanup(); RETURN
      END IF
      IF ( gattlen == 0 ) RETURN
      ! Write the attribute
      WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) TRIM(l_gattvalue)
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing '//TRIM(gattname)//' attribute - '//TRIM(io_msg)
        CALL WriteGatts_Cleanup(); RETURN
      END IF
    END SUBROUTINE WriteSingleGAtt
     
    SUBROUTINE WriteGAtts_Cleanup()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE WriteGAtts_Cleanup
    
  END FUNCTION WriteGAtts_Binary_File



  ! Function to read standard global attributes from a Binary file.

  FUNCTION ReadGAtts_Binary_File( &
    fid         , &  ! Input
    Write_Module, &  ! Optional output
    Created_On  , &  ! Optional output
    Title       , &  ! Optional output
    History     , &  ! Optional output
    Comment     ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    INTEGER     ,           INTENT(IN)  :: fid
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Write_Module
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Created_On  
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadGAtts_Binary_File'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat

    ! Set up
    err_stat = SUCCESS
    msg = ''

    ! 
    ! Software ID
    CALL ReadSingleGAtt( WRITE_MODULE_GATTNAME, gattvalue = Write_Module )
    IF ( err_stat /= SUCCESS ) RETURN
    
    ! Creation date/time
    CALL ReadSingleGAtt( CREATED_ON_GATTNAME, gattvalue = Created_On )
    IF ( err_stat /= SUCCESS ) RETURN


    ! The title
    CALL ReadSingleGAtt( TITLE_GATTNAME, gattvalue = Title )
    IF ( err_stat /= SUCCESS ) RETURN


    ! The history
    CALL ReadSingleGAtt( HISTORY_GATTNAME, gattvalue = History )
    IF ( err_stat /= SUCCESS ) RETURN


    ! The comment
    CALL ReadSingleGAtt( COMMENT_GATTNAME, gattvalue = Comment )
    IF ( err_stat /= SUCCESS ) RETURN
    
  CONTAINS
  
    SUBROUTINE ReadSingleGAtt( gattname, gattvalue)
      CHARACTER(*),           INTENT(IN)  :: gattname
      CHARACTER(*), OPTIONAL, INTENT(OUT) :: gattvalue
      INTEGER :: i, gattlen
      CHARACTER(GL) :: l_gattvalue
      ! Setup
      IF ( PRESENT(gattvalue) ) gattvalue = ''
      l_gattvalue = ''
      ! Read the string length
      READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) gattlen
      IF ( io_stat /= 0 ) THEN
        msg = 'Error reading '//TRIM(gattname)//' attribute length - '//TRIM(io_msg)
        CALL ReadGatts_Cleanup(); RETURN
      END IF
      IF ( gattlen == 0 ) RETURN
      ! Read the attribute
      READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) l_gattvalue(1:gattlen)
      IF ( io_stat /= 0 ) THEN
        msg = 'Error reading '//TRIM(gattname)//' attribute - '//TRIM(io_msg)
        CALL ReadGatts_Cleanup(); RETURN
      END IF
      ! Strip out the attribute name
      IF ( PRESENT(gattvalue) ) THEN
        i = INDEX(l_gattvalue,': ')
        gattvalue = l_gattvalue(i+2:gattlen)
      END IF    
    END SUBROUTINE ReadSingleGAtt
     
    SUBROUTINE ReadGAtts_Cleanup()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE ReadGAtts_Cleanup
    
  END FUNCTION ReadGAtts_Binary_File




  !
  ! NAME:
  !   ReadLogical_Binary_File
  !
  ! PURPOSE:
  !   Utility function to read an integer "logical" value from file
  !

  FUNCTION ReadLogical_Scalar( &
    fid, &
    logical_value ) &
  RESULT( err_stat )
    ! Arguments
    INTEGER, INTENT(IN)  :: fid
    LOGICAL, INTENT(OUT) :: logical_value
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadLogical_Binary_File(Scalar)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER(Long) :: logical_integer

    ! Setup
    err_stat = SUCCESS

    ! Read the integer
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) logical_integer
    IF ( io_stat /= 0 ) THEN
      err_stat = FAILURE
      msg = 'Error reading logical integer value - '//TRIM(io_msg)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Convert integer to a logical value
    logical_value = (logical_integer == TRUE)

  END FUNCTION ReadLogical_Scalar

  FUNCTION ReadLogical_Rank1( &
    fid, &
    logical_value ) &
  RESULT( err_stat )
    ! Arguments
    INTEGER, INTENT(IN)  :: fid
    LOGICAL, INTENT(OUT) :: logical_value(:)
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadLogical_Binary_File(Rank-1)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER(Long) :: logical_integer(SIZE(logical_value)) 

    ! Setup
    err_stat = SUCCESS

    ! Read the integer
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) logical_integer
    IF ( io_stat /= 0 ) THEN
      err_stat = FAILURE
      msg = 'Error reading logical integer rank-1 array - '//TRIM(io_msg)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Convert integer to a logical value
    logical_value = (logical_integer == TRUE)

  END FUNCTION ReadLogical_Rank1


  !
  ! NAME:
  !   WriteLogical_Binary_File
  !
  ! PURPOSE:
  !   Utility function to write an integer "logical" value to file
  !

  FUNCTION WriteLogical_Scalar( &
    fid, &
    logical_value ) &
  RESULT( err_stat )
    ! Arguments
    INTEGER, INTENT(IN) :: fid
    LOGICAL, INTENT(IN) :: logical_value
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteLogical_Binary_File(Scalar)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER(Long) :: logical_integer

    ! Setup
    err_stat = SUCCESS


    ! Convert the logical to an integer value
    IF ( logical_value ) THEN
      logical_integer = TRUE
    ELSE
      logical_integer = FALSE
    END IF


    ! Write the integer
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) logical_integer
    IF ( io_stat /= 0 ) THEN
      err_stat = FAILURE
      msg = 'Error writing logical integer - '//TRIM(io_msg)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

  END FUNCTION WriteLogical_Scalar

  FUNCTION WriteLogical_Rank1( &
    fid, &
    logical_value ) &
  RESULT( err_stat )
    ! Arguments
    INTEGER, INTENT(IN) :: fid
    LOGICAL, INTENT(IN) :: logical_value(:)
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteLogical_Binary_File(Rank-1)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER(Long) :: logical_integer(SIZE(logical_value)) 

    ! Setup
    err_stat = SUCCESS


    ! Convert the logical to an integer value
    WHERE ( logical_value )
      logical_integer = TRUE
    ELSEWHERE
      logical_integer = FALSE
    END WHERE


    ! Write the integer
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) logical_integer
    IF ( io_stat /= 0 ) THEN
      err_stat = FAILURE
      msg = 'Error writing logical integer rank-1 array - '//TRIM(io_msg)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

  END FUNCTION WriteLogical_Rank1



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
!       Error_Status = Check_Binary_File( Filename )
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

  FUNCTION Check_Binary_File( Filename ) RESULT( err_stat )
    ! Arguments
    CHARACTER(*), INTENT(IN) :: Filename
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*),  PARAMETER :: ROUTINE_NAME = 'Check_Binary_File'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: fid
    INTEGER :: io_stat
    INTEGER(Long) :: magic_number_read
    INTEGER(Long) :: magic_number_swapped

    ! Set up
    err_stat = SUCCESS


    ! Check that 4-byte integers are supported
    IF ( BIT_SIZE( 1_Long ) /= 32 ) THEN
      msg = '32-bit integers not supported. Unable to determine endian-ness'
      CALL CleanUp(); RETURN
    END IF


    ! Get a free unit number
    fid = Get_Lun()
    IF ( fid < 0 ) THEN
      msg = 'Error obtaining file unit number for '//TRIM(Filename)
      CALL CleanUp(); RETURN
    END IF


    ! Open the file as direct access
    OPEN( fid, FILE   = Filename     , &
               STATUS = 'OLD'        , &
               ACTION = 'READ'       , &
               ACCESS = 'DIRECT'     , &
               FORM   = 'UNFORMATTED', &
               RECL   = n_Bytes_Long , &
               IOSTAT = io_stat      , &
               IOMSG  = io_msg         )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error opening '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL CleanUp(); RETURN
    END IF


    ! Read the magic number
    READ( fid, REC=2, IOSTAT=io_stat, IOMSG=io_msg ) magic_number_read
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading file magic number - '//TRIM(io_msg)
      CALL CleanUp(); RETURN
    END IF


    ! Close the file
    CLOSE( fid )


    ! Compare the magic numbers
    IF ( magic_number_read /= MAGIC_NUMBER ) THEN

      ! Byte swap the magic number
      magic_number_swapped = Swap_Endian( magic_number_read )
      IF ( magic_number_swapped /= MAGIC_NUMBER ) THEN
        msg = 'Unrecognised file format. Invalid magic number.'
        CALL CleanUp(); RETURN
      END IF

      ! If we get here then the data does need to be byte-swapped
      msg = 'Data file needs to be byte-swapped.'
      CALL CleanUp(); RETURN

    END IF
    
  CONTAINS
   
     SUBROUTINE CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE CleanUp

  END FUNCTION Check_Binary_File

END MODULE Binary_File_Utility
