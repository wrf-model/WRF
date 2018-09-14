! Utility to define a timing structure and
! timing utility routines.
!
MODULE Timing_Utility

  ! Module usage
  USE Type_Kinds      , ONLY: fp
  USE Message_Handler , ONLY: SUCCESS, INFORMATION, FAILURE, Display_Message
  USE File_Utility    , ONLY: Get_Lun, File_Exists
  USE DateTime_Utility, ONLY: DateTime_type    , &
                              DateTime_Now     , &
                              DateTime_ToString
  ! Disable all implicit typing
  IMPLICIT NONE


  ! Visibilities
  PRIVATE
  ! ...Datatypes
  PUBLIC :: Timing_type
  ! ...Procedures
  PUBLIC :: Timing_Begin
  PUBLIC :: Timing_End
  PUBLIC :: Timing_Display
  PUBLIC :: Timing_Inspect
  PUBLIC :: Timing_ToString
  PUBLIC :: Timing_Set
  PUBLIC :: Timing_Get
  PUBLIC :: Timing_WriteFile
  ! ...Old named procedures
  PUBLIC :: Begin_Timing
  PUBLIC :: End_Timing
  PUBLIC :: Display_Timing


  ! Parameters
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Timing_Utility.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  INTEGER, PARAMETER :: ML = 256


  ! Overloads
  INTERFACE Begin_Timing
    MODULE PROCEDURE Timing_Begin
  END INTERFACE Begin_Timing

  INTERFACE End_Timing
    MODULE PROCEDURE Timing_End
  END INTERFACE End_Timing

  INTERFACE Display_Timing
    MODULE PROCEDURE Timing_Display
  END INTERFACE Display_Timing


  ! Derived type definitions
  !:tdoc+:
  TYPE :: Timing_type
    PRIVATE
    LOGICAL :: Is_Valid = .FALSE.
    INTEGER :: Hertz       = 0
    INTEGER :: Begin_Clock = 0
    INTEGER :: End_Clock   = 0
  END TYPE Timing_type
  !:tdoc-:


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
!   Timing_Begin
!
! PURPOSE:
!   Subroutine to set the begin time count in a timing object
!
! CALLING SEQUENCE:
!   CALL Timing_Begin( timing )
!
! INPUTS:
!   timing:  Timing object.
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_Begin( self )  ! In/Output
    TYPE(Timing_type), INTENT(OUT) :: self
    CALL SYSTEM_CLOCK( COUNT_RATE=self%Hertz, &
                       COUNT     =self%Begin_Clock )
    IF ( self%Hertz == 0 ) RETURN
    self%Is_Valid = .TRUE.
  END SUBROUTINE Timing_Begin


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_End
!
! PURPOSE:
!   Subroutine to set the end time count in a timing object
!
! CALLING SEQUENCE:
!   CALL Timing_End( timing )
!
! INPUTS:
!   timing:  Timing object.
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_End( self )  ! In/Output
    TYPE(Timing_type), INTENT(IN OUT) :: self
    CALL SYSTEM_CLOCK( COUNT=self%End_Clock )
  END SUBROUTINE Timing_End


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_Display
!
! PURPOSE:
!   Subroutine to display the elapsed time defined by the begin and end time
!   counts in the timing object.
!
! CALLING SEQUENCE:
!   CALL Timing_Display( timing, Caller = caller )
!
! INPUTS:
!   timing:  Timing object.
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   caller:  String containing the name of the calling routine.
!            If not specified, the name of this procedure is used.
!            UNITS:      N/A
!            TYPE:       CHARACTER(*)
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_Display( &
    self  , &  ! Input
    Caller  )  ! Optional input
    ! Arguments
    TYPE(Timing_type),      INTENT(IN) :: self
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Caller
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: routine_name

    ! Set up
    routine_name = 'Timing_Display'
    IF ( PRESENT(Caller) ) routine_name = TRIM(ADJUSTL(Caller))
    ! ...Check if timing structure valid for display
    IF ( .NOT. self%Is_Valid ) THEN
      msg = 'Invalid timing structure!'
      CALL Display_Message( routine_name, msg, FAILURE ); RETURN
    END IF

    ! Construct the character string
    WRITE( msg, '("Elapsed time-- ",a)' ) TRIM(Timing_ToString(self))
    CALL Display_Message( routine_name, msg, INFORMATION )

  END SUBROUTINE Timing_Display


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_Inspect
!
! PURPOSE:
!   Subroutine to print the contents of a Timing object to stdout.
!
! CALLING SEQUENCE:
!   CALL Timing_Inspect( timing )
!
! OBJECTS:
!   Timing:  Timing object to display.
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_Inspect( self )
    TYPE(Timing_type), INTENT(IN) :: self
    WRITE(*,'(1x,"Timing OBJECT")')
    WRITE(*,'(3x,"Hertz       : ",i0)') self%Hertz
    WRITE(*,'(3x,"Begin_Clock : ",i0)') self%Begin_Clock
    WRITE(*,'(3x,"End_Clock   : ",i0)') self%End_Clock
    WRITE(*,'(3x,"Is_Valid    : ",l1)') self%Is_Valid
  END SUBROUTINE Timing_Inspect


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Timing_ToString
!
! PURPOSE:
!       Elemental function to return the equivalent string representation
!       of the Timing object.
!
! CALLING SEQUENCE:
!       string = Timing_ToString( Timing, Seconds=seconds, Fmt_String=fmt_string )
!
! OBJECTS:
!   Timing:        Timing structure containing the timed data.
!                  UNITS:      N/A
!                  TYPE:       Timing_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   Seconds:       Set this logical argument to create a string that is
!                  simply the number of seconds of elpased time.
!                  If == .FALSE., the output string format is HH:MM:SS.sss [DEFAULT].
!                     == .TRUE.,  the output string format is XX.XXXXXXe+EE (es13.6).
!                  If not specified, default is .FALSE.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Conformable with the Timing input
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Fmt_String:    Character string holding the format string to use for
!                  seconds of elasped time output. If not supplied, the
!                  default is '(es13.6)'.
!                  This argument is ignored if the "Seconds" argument is
!                  not specified, or not set.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER(*)
!                  DIMENSION:  Conformable with the Timing input.
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!   string:        String containing the time elapsed in seconds.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER(80)
!                  DIMENSION:  Conformable with input Timing argument.
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Timing_ToString( &
    self      , &  ! Input
    Seconds   , &  ! Optional input
    Fmt_String) &  ! Optional input
  RESULT( string )
    ! Arguments
    TYPE(Timing_type),           INTENT(IN) :: self
    LOGICAL          , OPTIONAL, INTENT(IN) :: Seconds
    CHARACTER(*)     , OPTIONAL, INTENT(IN) :: Fmt_String
    ! Function result
    CHARACTER(80) :: string
    ! Local parameters
    REAL(fp), PARAMETER :: N_SECONDS_IN_HOUR        = 3600.0_fp
    REAL(fp), PARAMETER :: N_SECONDS_IN_MINUTE      =   60.0_fp
    REAL(fp), PARAMETER :: N_MILLISECONDS_IN_SECOND = 1000.0_fp
    ! Local variables
    CHARACTER(80) :: fmt
    LOGICAL  :: hhmmss
    REAL(fp) :: total_time
    INTEGER  :: n_hours
    INTEGER  :: n_minutes
    INTEGER  :: n_seconds
    INTEGER  :: n_milliseconds

    ! Setup
    ! ...Process keyword arguments
    hhmmss = .TRUE.
    IF ( PRESENT(Seconds) ) hhmmss = .NOT. Seconds
    fmt = '(es13.6)'
    IF ( PRESENT(Fmt_String) ) fmt = ADJUSTL(Fmt_String)


    ! Compute the total time in seconds
    total_time = Timing_ElapsedTime( self )


    ! Create the string
    IF ( hhmmss ) THEN
      ! ...Split the total time into hours, minutes, seconds, and millseconds
      n_hours        = INT(total_time / N_SECONDS_IN_HOUR)
      n_minutes      = INT(MOD(total_time,N_SECONDS_IN_HOUR) / N_SECONDS_IN_MINUTE)
      n_seconds      = INT(MOD(MOD(total_time,N_SECONDS_IN_HOUR), N_SECONDS_IN_MINUTE))
      n_milliseconds = INT((total_time - AINT(total_time,fp)) * N_MILLISECONDS_IN_SECOND)
      ! ...Construct the HH:MM:SS.sss string
      WRITE( string,'(i2.2,":",i2.2,":",i2.2,".",i3.3 )' ) &
                    n_hours, n_minutes, n_seconds, n_milliseconds
    ELSE
      ! ...Construct the number of seconds string
      WRITE( string,FMT=fmt ) total_time
    END IF

  END FUNCTION Timing_ToString


  ! Subroutine to set the components of a timing object
  ! Public, but only for testing so it's undocumented.
  SUBROUTINE Timing_Set( &
    self       , &
    Hertz      , &
    Begin_Clock, &
    End_Clock  , &
    Is_Valid     )
    ! Arguments
    TYPE(Timing_type), INTENT(IN OUT) :: self
    INTEGER, OPTIONAL, INTENT(IN)     :: Hertz
    INTEGER, OPTIONAL, INTENT(IN)     :: Begin_Clock
    INTEGER, OPTIONAL, INTENT(IN)     :: End_Clock
    LOGICAL, OPTIONAL, INTENT(IN)     :: Is_Valid
    ! Set object components
    IF ( PRESENT(Hertz      ) ) self%Hertz       = Hertz
    IF ( PRESENT(Begin_Clock) ) self%Begin_Clock = Begin_Clock
    IF ( PRESENT(End_Clock  ) ) self%End_Clock   = End_Clock
    IF ( PRESENT(Is_Valid   ) ) self%Is_Valid    = Is_Valid
  END SUBROUTINE Timing_Set


  ! Subroutine to get the components of a timing object
  ! Public, but only for testing so it's undocumented.
  SUBROUTINE Timing_Get( &
    self       , &
    Hertz      , &
    Begin_Clock, &
    End_Clock  , &
    Is_Valid     )
    ! Arguments
    TYPE(Timing_type), INTENT(IN)  :: self
    INTEGER, OPTIONAL, INTENT(OUT) :: Hertz
    INTEGER, OPTIONAL, INTENT(OUT) :: Begin_Clock
    INTEGER, OPTIONAL, INTENT(OUT) :: End_Clock
    LOGICAL, OPTIONAL, INTENT(OUT) :: Is_Valid
    ! Set object components
    IF ( PRESENT(Hertz      ) ) Hertz       = self%Hertz
    IF ( PRESENT(Begin_Clock) ) Begin_Clock = self%Begin_Clock
    IF ( PRESENT(End_Clock  ) ) End_Clock   = self%End_Clock
    IF ( PRESENT(Is_Valid   ) ) Is_Valid    = self%Is_Valid
  END SUBROUTINE Timing_Get


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_WriteFile
!
! PURPOSE:
!   Function to write timing object information to an ASCII file.
!
! CALLING SEQUENCE:
!   Error_Status = Timing_WriteFile( &
!                    Timing_Array, &
!                    Filename    , &
!                    Clobber = clobber, &
!                    Heading = heading  )
!
! OBJECTS:
!   Timing_Array:  Array of Timing objects to write to file. All elements
!                  must be valid.
!                  UNITS:      N/A
!                  TYPE:       Timing_type
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!   Filename:      Name of the file to write.
!                  - If file does not exist, it is created.
!                  - If file does exist, it is opened for appending data.
!                  UNITS:      N/A
!                  TYPE:       Timing_type
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   Clobber:       Set this logical argument to overwrite an existing filename.
!                  If == .FALSE., an existing file is opened with OLD status and
!                                 positioned at end-of-file for appending data [DEFAULT].
!                     == .TRUE.,  an existing file is opened with REPLACE status.
!                  If not specified, default is .FALSE.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Heading:       Array of character strings to use as heading labels for each
!                  timing object element.
!                  - If not specified, default is "Time 1", "Time 2", etc..
!                  - If specified, size must be same as Timing_Array.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:  The return value is an integer defining the error status.
!                  The error codes are defined in the Message_Handler module.
!                  If == SUCCESS the data write was successful
!                     == FAILURE an unrecoverable error occurred.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Timing_WriteFile( &
    Timing_Array, &
    Filename    , &
    Clobber     , &
    Heading     ) &
  RESULT( err_stat )
    ! Arguments
    TYPE(Timing_type),           INTENT(IN) :: Timing_Array(:)
    CHARACTER(*)     ,           INTENT(IN) :: Filename
    LOGICAL          , OPTIONAL, INTENT(IN) :: Clobber
    CHARACTER(*)     , OPTIONAL, INTENT(IN) :: Heading(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Timing_WriteFile'
    ! Local variables
    CHARACTER(ML) :: msg, io_msg
    CHARACTER(ML) :: current_time
    CHARACTER(ML) :: title(SIZE(Timing_Array))
    CHARACTER(ML) :: elapsed_time(SIZE(Timing_Array))
    CHARACTER(ML) :: output_fmt
    CHARACTER(8) :: status, position
    LOGICAL :: append
    INTEGER :: fid
    INTEGER :: io_stat
    INTEGER :: i, n_times
    INTEGER :: sl_current_time
    INTEGER :: sl_elapsed_time

    ! Set up
    err_stat = SUCCESS
    ! ...Check structures
    IF ( .NOT. ALL(Timing_Array%Is_Valid) ) THEN
      err_stat = FAILURE
      msg = 'Input timing array contains invalid elements'
      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
    END IF
    n_times = SIZE(Timing_Array)
    ! ...Check clobber argument
    append = .TRUE.
    IF ( PRESENT(Clobber) ) append = .NOT. Clobber
    ! ...Check header argument
    IF ( PRESENT(Heading) ) THEN
      IF ( SIZE(Heading) /= n_times ) THEN
        err_stat = FAILURE
        msg = 'Input heading array different size from timing array'
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      DO i = 1, n_times
        title(i) = '| '//TRIM(Heading(i))
      END DO
    ELSE
      DO i = 1, n_times
        WRITE(title(i),'("| Time ",i0)') i
      END DO
    END IF


    ! Open the file
    ! ...Set specifiers for write
    IF ( append ) THEN
      status   = 'OLD'
      position = 'APPEND'
    ELSE
      status   = 'REPLACE'
      position = 'REWIND'
    END IF
    ! ...Special case for a file that doesn't exist yet
    IF ( .NOT. File_Exists(filename) ) status = 'NEW'
    ! ...Get a free unit number
    fid = Get_Lun()
    IF ( fid < 0 ) THEN
      err_stat = FAILURE
      msg = 'Error obtaining free logical unit number'
      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
    END IF
    ! ...Open the file for output
    OPEN( fid, FILE     = filename   , &
               FORM     = 'FORMATTED', &
               STATUS   = status     , &
               POSITION = position   , &
               IOSTAT   = io_stat    , &
               IOMSG    = io_msg       )
    IF ( io_stat /= 0 ) THEN
      err_stat = FAILURE
      msg = 'Error opening file '//TRIM(filename)//' - '//TRIM(io_msg)
      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
    END IF


    ! Get the current date/time string, and its length, for output
    current_time    = DateTime_ToString(DateTime_Now(),Format='s')
    sl_current_time = LEN_TRIM(current_time)


    ! Transform the timing info to strings, and get their length
    elapsed_time    = Timing_ToString(Timing_Array,Seconds=.TRUE.)
    sl_elapsed_time = MAX(MAXVAL(LEN_TRIM(elapsed_time)), MAXVAL(LEN_TRIM(title)))


    ! Construct the output write format
    WRITE(output_fmt,'("(a",i0,",",i0,"(2x,a",i0,"))")') &
                     sl_current_time, n_times, sl_elapsed_time


    ! Write the header line if necessary
    IF ( TRIM(status) == 'NEW' ) THEN
      WRITE(fid,FMT = output_fmt, IOSTAT = io_stat, IOMSG = io_msg) &
        'Current time', (TRIM(title(i)), i = 1, n_times)
      IF ( io_stat /= 0 ) THEN
        err_stat = FAILURE
        msg = 'Error writing header to '//TRIM(filename)//' - '//TRIM(io_msg)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
    END IF


    ! Write the current set of times to file
    WRITE(fid,FMT = output_fmt, IOSTAT = io_stat, IOMSG = io_msg) &
      TRIM(current_time), &
      (TRIM(elapsed_time(i)), i = 1, n_times)
    IF ( io_stat /= 0 ) THEN
      err_stat = FAILURE
      msg = 'Error writing timing data to '//TRIM(filename)//' - '//TRIM(io_msg)
      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
    END IF


    ! Done
    CLOSE(fid, IOSTAT = io_stat, &
               IOMSG  = io_msg   )
    IF ( io_stat /= 0 ) THEN
      err_stat = FAILURE
      msg = 'Error closing file '//TRIM(filename)//' - '//TRIM(io_msg)
      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
    END IF

  END FUNCTION Timing_WriteFile



!##############################################################################
!##############################################################################
!##                                                                          ##
!##                      ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                          ##
!##############################################################################
!##############################################################################

  ! Private elemental function to compute the elapsed time in seconds
  ELEMENTAL FUNCTION Timing_ElapsedTime( self ) RESULT( Elapsed_Time )
    TYPE(Timing_type), INTENT(IN) :: self
    REAL(fp) :: Elapsed_Time
    Elapsed_Time = 0.0_fp
    IF ( .NOT. self%Is_Valid ) RETURN
    Elapsed_Time = REAL(self%End_Clock - self%Begin_Clock, fp) / REAL(self%Hertz, fp)
  END FUNCTION Timing_ElapsedTime

END MODULE Timing_Utility
