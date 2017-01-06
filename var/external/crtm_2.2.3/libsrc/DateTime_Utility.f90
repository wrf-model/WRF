!
! DateTime_Utility
!
! Module defining the DateTime structure and utility routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 14-Sep-2007
!                       paul.vandelst@noaa.gov
!

MODULE DateTime_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  USE Type_Kinds  , ONLY: fp
  USE Date_Utility, ONLY: IsLeapYear , &
                          DayOfYear  , &
                          DaysInMonth, &
                          NameOfMonth, &
                          DayOfWeek
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Datatpye
  PUBLIC :: DateTime_type
  ! Procedures
  PUBLIC :: DateTime_Now
  PUBLIC :: DateTime_IsLeapYear
  PUBLIC :: DateTime_DayOfYear
  PUBLIC :: DateTime_DaysInMonth
  PUBLIC :: DateTime_NameOfMonth
  PUBLIC :: DateTime_DayOfWeek
  PUBLIC :: DateTime_Inspect
  PUBLIC :: DateTime_ToString


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: DateTime_Utility.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  INTEGER, PARAMETER :: NL = 20


  ! -----------------------
  ! Derived type definition
  ! -----------------------
  !:tdoc+:
  TYPE :: DateTime_type
    INTEGER :: Year        = 0
    INTEGER :: Month       = 0
    INTEGER :: Day         = 0
    INTEGER :: UTC_Delta   = 0
    INTEGER :: Hour        = 0
    INTEGER :: Minute      = 0
    INTEGER :: Second      = 0
    INTEGER :: Millisecond = 0
    INTEGER :: DoY         = 0
    CHARACTER(NL) :: Month_Name
    CHARACTER(NL) :: Day_Name
  END TYPE DateTime_type
  !:tdoc-:


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       DateTime_Now
!
! PURPOSE:
!       Function to return a DateTime structure with the current
!       date and time
!
! CALLING SEQUENCE:
!       DateTime = DateTime_Now()
!
! FUNCTION RESULT:
!       DateTime:  DateTime structure containing current date and time.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION DateTime_Now() RESULT(DateTime)
    TYPE(DateTime_type) :: DateTime
    INTEGER :: Values(8)
    CALL DATE_AND_TIME(VALUES=Values)
    DateTime%Year        = Values(1)
    DateTime%Month       = Values(2)
    DateTime%Day         = Values(3)
    DateTime%UTC_Delta   = Values(4)
    DateTime%Hour        = Values(5)
    DateTime%Minute      = Values(6)
    DateTime%Second      = Values(7)
    DateTime%Millisecond = Values(8)
    DateTime%DoY        = DateTime_DayOfYear( DateTime )
    DateTime%Month_Name = DateTime_NameOfMonth( DateTime )
    DateTime%Day_Name   = DateTime_DayOfWeek( DateTime )
  END FUNCTION DateTime_Now


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       DateTime_IsLeapYear
!
! PURPOSE:
!       Elemental function to determine if a specified DateTime structure
!       is for a leap year.
!
! CALLING SEQUENCE:
!       Result = DateTime_IsLeapYear( DateTime )
!
! INPUTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! FUNCTION RESULT:
!       Result:    The return value is a logical value indicating whether
!                  the specified year is a leap year.
!                  If .TRUE.  the specified year IS a leap year.
!                     .FALSE. the specified year is NOT a leap year
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as input
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DateTime_IsLeapYear( DateTime )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    LOGICAL :: DateTime_IsLeapYear
    DateTime_IsLeapYear = IsLeapYear( DateTime%Year )
  END FUNCTION DateTime_IsLeapYear


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       DateTime_DayOfYear
!
! PURPOSE:
!       Elemental function to determine the day-of-year value for a
!       a DateTime structure.
!
! CALLING SEQUENCE:
!       DoY = DateTime_DayOfYear( DateTime )
!
! INPUTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       DoY:       Integer defining the day-of-year.
!                  Return value is 0 for invalid input.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DateTime_DayOfYear( DateTime ) RESULT( DoY )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    INTEGER :: DoY
    DoY = DayOfYear( DateTime%Day, DateTime%Month, DateTime%Year )
  END FUNCTION DateTime_DayOfYear


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       DateTime_DaysInMonth
!
! PURPOSE:
!       Elemental function to return the number of days in a given
!       month and year.
!
! CALLING SEQUENCE:
!       n_Days = DateTime_DaysInMonth( DateTime )
!
! INPUTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n_Days:    The number of days in the month.
!                  Return value is 0 for invalid input.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as input
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DateTime_DaysInMonth( DateTime ) RESULT( n_Days )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    INTEGER :: n_Days
    n_Days = DaysInMonth( DateTime%Month, DateTime%Year )
  END FUNCTION DateTime_DaysInMonth


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       DateTime_NameOfMonth
!
! PURPOSE:
!       Elemental function to return the name of the month.
!
! CALLING SEQUENCE:
!       name = DateTime_NameOfMonth( DateTime )
!
! INPUT ARGUMENTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       name:      The return value is a character string containing the
!                  name of the month.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER
!                  DIMENSION:  Conformable with input DateTime arugment
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DateTime_NameOfMonth( DateTime ) RESULT( name )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    CHARACTER(NL) :: name
    name = NameOfMonth( DateTime%Month )
  END FUNCTION DateTime_NameOfMonth


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       DateTime_DayOfWeek
!
! PURPOSE:
!       Elemental function to return the name of the day of week.
!
! CALLING SEQUENCE:
!       name = DateTime_DayOfWeek( DateTime )
!
! INPUT ARGUMENTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       name:      The return value is a character string containing the
!                  name of the day of the week.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER
!                  DIMENSION:  Conformable with input DateTime argument.
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DateTime_DayOfWeek( DateTime ) RESULT( name )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    CHARACTER(NL) :: name
    name = DayOfWeek( DateTime%Day, DateTime%Month, DateTime%Year )
  END FUNCTION DateTime_DayOfWeek


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   DateTime_Inspect
!
! PURPOSE:
!   Subroutine to print the contents of a DateTime object to stdout.
!
! CALLING SEQUENCE:
!   CALL DateTime_Inspect( datetime )
!
! OBJECTS:
!   DateTime:  DateTime object to display.
!              UNITS:      N/A
!              TYPE:       DateTime_type
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE DateTime_Inspect( self )
    TYPE(DateTime_type), INTENT(IN) :: self
    WRITE(*,'(1x,"DateTime OBJECT")')
    WRITE(*,'(3x,"Year        : ",i0)') self%Year
    WRITE(*,'(3x,"Month       : ",i0)') self%Month
    WRITE(*,'(3x,"Day         : ",i0)') self%Day
    WRITE(*,'(3x,"UTC_Delta   : ",i0)') self%UTC_Delta
    WRITE(*,'(3x,"Hour        : ",i0)') self%Hour
    WRITE(*,'(3x,"Minute      : ",i0)') self%Minute
    WRITE(*,'(3x,"Second      : ",i0)') self%Second
    WRITE(*,'(3x,"Millisecond : ",i0)') self%Millisecond
    WRITE(*,'(3x,"DoY         : ",i0)') self%DoY
    WRITE(*,'(3x,"Month_Name  : ",a)') TRIM(self%Month_Name)
    WRITE(*,'(3x,"Day_Name    : ",a)') TRIM(self%Day_Name)
  END SUBROUTINE DateTime_Inspect


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       DateTime_ToString
!
! PURPOSE:
!       Elemental function to return the equivalent string representation
!       of the DateTime object.
!
! CALLING SEQUENCE:
!       string = DateTime_ToString( DateTime, Format=format )
!
! OBJECTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Format:    A single character code to determine the output
!                  string format. Valid format codes are:
!
!   'd'        Short Date pattern.               6/15/2009 1:45:30 PM -> 06/15/2009
!   'D'        Long Date pattern.                6/15/2009 1:45:30 PM -> Monday, June 15, 2009
!   'f'        Full Date Short Time pattern.     6/15/2009 1:45:30 PM -> Monday, June 15, 2009 13:45
!   'F'        Full Date Long Time pattern.      6/15/2009 1:45:30 PM -> Monday, June 15, 2009 13:45:30
!   'g'        General Date Short Time pattern.  6/15/2009 1:45:30 PM -> 06/15/2009 13:45
!   'G'        General Date Long Time pattern.   6/15/2009 1:45:30 PM -> 06/15/2009 13:45:30
!   'm','M'    Month pattern.                    6/15/2009 1:45:30 PM -> June 15
!   'o','O'    Round-trip pattern.               6/15/2009 1:45:30 PM -> 2009-06-15T13:45:30.0900000
!   'r','R'  * RFC1123 pattern.                  6/15/2009 1:45:30 PM -> Mon, 15 Jun 2009 20:45:30 GMT
!   's'        Sortable pattern.                 6/15/2009 1:45:30 PM -> 2009-06-15T13:45:30
!   't'        Short Time pattern.               6/15/2009 1:45:30 PM -> 13:45
!   'T'        Long Time pattern.                6/15/2009 1:45:30 PM -> 13:45:30
!   'u'      * Universal Sortable pattern.       6/15/2009 1:45:30 PM -> 2009-06-15 20:45:30Z
!   'U'      * Universal Full pattern.           6/15/2009 1:45:30 PM -> Monday, June 15, 2009 20:45:30
!   'y','Y'    Year Month pattern.               6/15/2009 1:45:30 PM -> June, 2009
!
!                  Format codes marked with a "*" are not yet implemented.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER
!                  DIMENSION:  Conformable with the DateTime input.
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       string:    Equivalent string representation of the DateTime object.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER
!                  DIMENSION:  Conformable with input DateTime argument.
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DateTime_ToString( DateTime, Format ) RESULT( string )
    ! Arguments
    TYPE(DateTime_type),           INTENT(IN) :: DateTime
    CHARACTER(*)       , OPTIONAL, INTENT(IN) :: Format
    ! Function result
    CHARACTER(80) :: string
    ! Local variables
    CHARACTER(1) :: string_format
    REAL(fp) :: seconds

    ! Define default format
    string_format = 'd'
    IF ( PRESENT(Format) ) string_format = TRIM(ADJUSTL(Format))


    ! Begin monster select construct
    ! NOTE: This is a brain dead way to implement. But quick. :o)
    SELECT CASE(string_format)

      CASE('d')      ! Short Date pattern.  6/15/2009 1:45:30 PM -> 06/15/2009
        WRITE(string,'(i2.2,"/",i2.2,"/",i4)') &
                     DateTime%Month, DateTime%Day, DateTime%Year

      CASE('D')      ! Long Date pattern.  6/15/2009 1:45:30 PM -> Monday, June 15, 2009
        WRITE(string,'(a,", ",a,1x,i0,", ",i4)') &
                     TRIM(DateTime_DayOfWeek(DateTime)), &
                     TRIM(DateTime_NameOfMonth(DateTime)), &
                     DateTime%Day, DateTime%Year

      CASE('f')      ! Full Date Short Time pattern.  6/15/2009 1:45:30 PM -> Monday, June 15, 2009 13:45
        WRITE(string,'(a,", ",a,1x,i0,", ",i4,1x,i2.2,":",i2.2)') &
                     TRIM(DateTime_DayOfWeek(DateTime)), &
                     TRIM(DateTime_NameOfMonth(DateTime)), &
                     DateTime%Day, DateTime%Year, &
                     DateTime%Hour, DateTime%Minute

      CASE('F')      ! Full Date Long Time pattern.  6/15/2009 1:45:30 PM -> Monday, June 15, 2009 13:45:30
        WRITE(string,'(a,", ",a,1x,i0,", ",i4,1x,i2.2,":",i2.2,":",i2.2)') &
                     TRIM(DateTime_DayOfWeek(DateTime)), &
                     TRIM(DateTime_NameOfMonth(DateTime)), &
                     DateTime%Day, DateTime%Year, &
                     DateTime%Hour, DateTime%Minute, DateTime%Second

      CASE('g')      ! General Date Short Time pattern.  6/15/2009 1:45:30 PM -> 06/15/2009 13:45
        WRITE(string,'(i2.2,"/",i2.2,"/",i4,1x,i2.2,":",i2.2)') &
                     DateTime%Month, DateTime%Day, DateTime%Year, &
                     DateTime%Hour, DateTime%Minute

      CASE('G')      ! General Date Long Time pattern.  6/15/2009 1:45:30 PM -> 06/15/2009 13:45:30
        WRITE(string,'(i2.2,"/",i2.2,"/",i4,1x,i2.2,":",i2.2,":",i2.2)') &
                     DateTime%Month, DateTime%Day, DateTime%Year, &
                     DateTime%Hour, DateTime%Minute, DateTime%Second

      CASE('m','M')  ! Month pattern.  6/15/2009 1:45:30 PM -> June 15
        WRITE(string,'(a,1x,i0)') &
                     TRIM(DateTime_NameOfMonth(DateTime)), DateTime%Day

      CASE('o','O')  ! Round-trip pattern.  6/15/2009 1:45:30 PM -> 2009-06-15T13:45:30.0900000
        seconds = REAL(DateTime%Second,fp) + &
                  REAL(DateTime%Millisecond,fp)*1.0e-03_fp
        WRITE(string,'(i4,"-",i2.2,"-",i2.2,"T",i2.2,":",i2.2,":",f10.7)') &
                     DateTime%Year, DateTime%Month, DateTime%Day, &
                     DateTime%Hour, DateTime%Minute, seconds

      CASE('r','R')  ! RFC1123 pattern.  6/15/2009 1:45:30 PM -> Mon, 15 Jun 2009 20:45:30 GMT
        string = 'RFC1123 format not yet implemented'

      CASE('s')      ! Sortable pattern.  6/15/2009 1:45:30 PM -> 2009-06-15T13:45:30
        WRITE(string,'(i4,"-",i2.2,"-",i2.2,"T",i2.2,":",i2.2,":",i2.2)') &
                     DateTime%Year, DateTime%Month, DateTime%Day, &
                     DateTime%Hour, DateTime%Minute, DateTime%Second

      CASE('t')      ! Short Time pattern.  6/15/2009 1:45:30 PM -> 13:45
        WRITE(string,'(i2.2,":",i2.2)') &
                     DateTime%Hour, DateTime%Minute

      CASE('T')      ! Long Time pattern.  6/15/2009 1:45:30 PM -> 13:45:30
        WRITE(string,'(i2.2,":",i2.2,":",i2.2)') &
                     DateTime%Hour, DateTime%Minute, DateTime%Second

      CASE('u')      ! Universal Sortable pattern.  6/15/2009 1:45:30 PM -> 2009-06-15 20:45:30Z
        string = 'Universal Sortable format not yet implemented'

      CASE('U')      ! Universal Full pattern.  6/15/2009 1:45:30 PM -> Monday, June 15, 2009 20:45:30
        string = 'Universal Full format not yet implemented'

      CASE('y','Y')  ! Year Month pattern. 6/15/2009 1:45:30 PM -> June, 2009
        WRITE(string,'(a,", ",i4)') &
                     TRIM(DateTime_NameOfMonth(DateTime)), DateTime%Year
      CASE DEFAULT
        string = 'Invalid DateTime string format!'

    END SELECT

  END FUNCTION DateTime_ToString

END MODULE DateTime_Utility
