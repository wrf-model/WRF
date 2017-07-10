!
! Date_Utility
!
! Module containing date conversion routines
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Apr-2000
!                       paul.vandelst@noaa.gov

MODULE Date_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  IMPLICIT NONE



  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Parameters
  PUBLIC :: N_MONTHS
  PUBLIC :: DAYS_PER_MONTH_IN_NONLEAP
  PUBLIC :: MONTH_NAME
  PUBLIC :: N_DAYS
  PUBLIC :: DAY_NAME
  ! Procedures
  PUBLIC :: IsLeapYear
  PUBLIC :: DayOfYear
  PUBLIC :: DaysInMonth
  PUBLIC :: NameOfMonth
  PUBLIC :: DayOfWeek


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Date_Utility.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! String length for character functions
  INTEGER, PARAMETER :: NL = 20
  ! Number of Months in a Year
  INTEGER, PARAMETER :: N_MONTHS = 12
  ! Days per Month in a non leap Year
  INTEGER, PARAMETER :: DAYS_PER_MONTH_IN_NONLEAP(N_MONTHS) = &
  (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  ! Month names
  CHARACTER(*), PARAMETER :: MONTH_NAME(N_MONTHS) = &
  (/'January  ','February ','March    ','April    ','May      ','June     ', &
    'July     ','August   ','September','October  ','November ','December ' /)
  ! Number of Days in a Week
  INTEGER, PARAMETER :: N_DAYS = 7
  ! Day names
  CHARACTER(*), PARAMETER :: DAY_NAME(N_DAYS) = &
  (/'Sunday   ','Monday   ','Tuesday  ','Wednesday','Thursday ','Friday   ','Saturday '/)


CONTAINS


!##############################################################################
!##############################################################################
!##                                                                          ##
!##                       ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                          ##
!##############################################################################
!##############################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IsLeapYear
!
! PURPOSE:
!       Elemental function to determine if a specified year is a leap year.
!
! CALLING SEQUENCE:
!       Result = IsLeapYear( Year )
!
! INPUT ARGUMENTS:
!       Year:     The year in 4-digit format, e.g. 1997.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Result:   The return value is a logical value indicating whether
!                 the specified year is a leap year.
!                 If .TRUE.  the specified year IS a leap year.
!                    .FALSE. the specified year is NOT a leap year.
!                 UNITS:      N/A
!                 TYPE:       LOGICAL
!                 DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION IsLeapYear( Year )
    INTEGER, INTENT(IN) :: Year
    LOGICAL :: IsLeapYear

    IsLeapYear = ( (MOD(Year,4)   == 0) .AND. (MOD(Year,100) /= 0) ) .OR. &
                 (MOD(Year,400) == 0)

  END FUNCTION IsLeapYear


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       DayOfYear
!
! PURPOSE:
!       Elemental function to convert input numeric (e.g. DD,MM,YYYY) date
!       information to a sequential day of year.
!
! CALLING SEQUENCE:
!       DoY = DayOfYear ( Day, Month, Year )
!
! INPUTS:
!       Day:       The day-of-month.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
!       Month:     The month-of-year.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as Day input
!                  ATTRIBUTES: INTENT(IN)
!
!       Year:      The year in 4-digit format, e.g. 1997.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as Day input
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       DoY:       Integer defining the day-of-year.
!                  Return value is 0 for invalid input.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as Day input
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DayOfYear( &
    Day  , &  ! Input
    Month, &  ! Input
    Year ) &  ! Input
  RESULT( DoY )
    ! Arguments
    INTEGER, INTENT(IN) :: Day
    INTEGER, INTENT(IN) :: Month
    INTEGER, INTENT(IN) :: Year
    ! Function result
    INTEGER :: DoY
    ! Local variables
    INTEGER :: Days_per_Month( N_MONTHS )

    ! Set up
    DoY = 0
    ! ...Check year and month input
    IF ( Year  < 1 .OR. &
         Month < 1 .OR. &
         Month > N_MONTHS ) RETURN
    ! ...Check the day of month
    Days_per_Month = DAYS_PER_MONTH_IN_NONLEAP
    IF ( IsLeapYear(Year) ) Days_per_Month(2) = 29
    IF ( Day > Days_per_Month(Month) ) RETURN

    ! Compute the day of year
    DoY = SUM(Days_per_Month(1:Month-1)) + Day

  END FUNCTION DayOfYear


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       DaysInMonth
!
! PURPOSE:
!       Elemental function to return the number of days in a given
!       month and year.
!
! CALLING SEQUENCE:
!       n_Days = DaysInMonth( Month, Year )
!
! INPUTS:
!       Month:     The month of the year (1-12).
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
!       Year:      The year in 4-digit format, e.g. 1997.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as Month input
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

  ELEMENTAL FUNCTION DaysInMonth(Month, Year) RESULT(n_Days)
    ! Arguments
    INTEGER, INTENT(IN) :: Month
    INTEGER, INTENT(IN) :: Year
    ! Function result
    INTEGER :: n_Days
    ! Local variables
    INTEGER :: Days_per_Month(N_MONTHS)

    ! Set up
    n_Days = 0
    ! ...Check year and month input
    IF ( Year  < 1 .OR. &
         Month < 1 .OR. &
         Month > N_MONTHS ) RETURN

    ! Assemble the days of month
    Days_per_Month = DAYS_PER_MONTH_IN_NONLEAP
    IF ( IsLeapYear(Year=Year) ) Days_per_Month(2) = 29

    ! Set the number of days
    n_Days = Days_per_Month(Month)

  END FUNCTION DaysInMonth


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NameOfMonth
!
! PURPOSE:
!       Elemental function to return the name of the month.
!
! CALLING SEQUENCE:
!       name = NameOfMonth( Month )
!
! INPUT ARGUMENTS:
!       Month:    The month of the year (1-12).
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar or any rank.
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       name:     The return value is a character string containing the
!                 name of the month.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER
!                 DIMENSION:  Conformable with input Month arugment
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION NameOfMonth( Month )
    INTEGER, INTENT(IN) :: Month
    CHARACTER(NL) :: NameOfMonth
    NameOfMonth = 'Invalid'
    IF ( Month < 1 .OR. Month > N_MONTHS ) RETURN
    NameOfMonth = MONTH_NAME( Month )
  END FUNCTION NameOfMonth


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       DayOfWeek
!
! PURPOSE:
!       Elemental function to return the name of the day of week.
!
!       NOTE:
!       - Only valid for Gregorian calendar.
!       - Since different places switched to the Gregorian calendar at
!         different times, this routine will only output day of week names
!         for dates AFTER 1918 (the year Russia adopted the Gregorian
!         calendar).
!
! CALLING SEQUENCE:
!       name = DayOfWeek ( Day, Month, Year )
!
! INPUTS:
!       Day:       The day of the month.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
!       Month:     The month of the year (1-12).
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Conformable with input Day argument.
!                  ATTRIBUTES: INTENT(IN)
!
!       Year:      The year in 4-digit format, e.g. 1997.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Conformable with input Day argument.
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       name:      The return value is a character string containing the
!                  name of the day-of-week.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER
!                  DIMENSION:  Conformable with input Day argument.
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DayOfWeek( Day, Month, Year )
    INTEGER, INTENT(IN) :: Day
    INTEGER, INTENT(IN) :: Month
    INTEGER, INTENT(IN) :: Year
    CHARACTER(NL) :: DayOfWeek
    INTEGER :: i
    DayOfWeek = 'Invalid'
    i = iDayOfWeek( Day, Month, Year )
    IF ( i == 0 ) RETURN
    DayOfWeek = DAY_NAME(i)
  END FUNCTION DayOfWeek



!##############################################################################
!##############################################################################
!##                                                                          ##
!##                      ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                          ##
!##############################################################################
!##############################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       iDayOfWeek
!
! PURPOSE:
!       Elemental function to convert input numeric (e.g. DD,MM,YYYY) date
!       information to a day of week index, 1-7.
!
!       NOTE:
!       - Only valid for Gregorian calendar.
!       - Since different places switched to the Gregorian calendar at
!         different times, this routine will only output valid day of week
!         indices for dates AFTER 1918 (the year Russia adopted the Gregorian
!         calendar).
!
! CALLING SEQUENCE:
!       iDoW = iDayOfWeek ( Day, Month, Year )
!
! INPUTS:
!       Day:       The day of the month.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
!       Month:     The month of the year (1-12).
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as Day input
!                  ATTRIBUTES: INTENT(IN)
!
!       Year:      The year in 4-digit format, e.g. 1997.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as Day input
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       iDoW:      Integer defining the day-of-week (1-7).
!                  Return value is 0 for invalid input.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as Day input
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION iDayOfWeek( Day, Month, Year ) RESULT( iDoW )
    ! Arguments
    INTEGER, INTENT(IN) :: Day
    INTEGER, INTENT(IN) :: Month
    INTEGER, INTENT(IN) :: Year
    ! Function result
    INTEGER :: iDoW
    ! Local variables
    INTEGER :: jdn

    iDoW = 0
    jdn = JulianDay( Day, Month, Year )
    IF ( jdn < 0 ) RETURN

    iDoW = MOD(jdn+1, 7) + 1
    IF ( iDoW < 1 .OR. iDoW > 7 ) iDow = 0

  END FUNCTION iDayOfWeek


!------------------------------------------------------------------------------
!
! NAME:
!       JulianDay
!
! PURPOSE:
!       Elemental function to convert input numeric (e.g. DD,MM,YYYY) date
!       information to a Julian Day Number, which is defined as the number of
!       days since noon January 1, 4713 BCE.
!
!       NOTE:
!       - Only valid for Gregorian calendar.
!       - Since different places switched to the Gregorian calendar at different
!         times, this routine will only output valid Julian day numbers for dates
!         AFTER 1918 (the year Russia adopted the Gregorian calendar).
!
! CALLING SEQUENCE:
!       jdn = JulianDay( Day, Month, Year )
!
! INPUTS:
!       Day:       The day of the month.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
!       Month:     The month of the year (1-12).
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as Day input
!                  ATTRIBUTES: INTENT(IN)
!
!       Year:      The year in 4-digit format, e.g. 1997.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as Day input
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       jdn:       Julian Day Number.
!                  Return value is negative for invalid input.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as Day input
!
! REFERENCES:
!   - http://en.wikipedia.org/wiki/Julian_day
!   - http://www.cs.utsa.edu/~cs1063/projects/Spring2011/Project1/jdn-explanation.html
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION JulianDay( Day, Month, Year ) RESULT( jdn )
    ! Arguments
    INTEGER, INTENT(IN) :: Day
    INTEGER, INTENT(IN) :: Month
    INTEGER, INTENT(IN) :: Year
    ! Function result
    INTEGER :: jdn
    ! Local variables
    INTEGER :: m, y, a

    jdn = -1
    IF ( year <= 1918 ) RETURN

    ! Compute the number of years and months since March 1, 4801 BCE
    a = (14 - month)/12       !  1 for Jan, 2 for Feb, 0 for other months.
    y = year + 4800 - a       !  Add 4800 to start counting from -4800.
    m = month + (12*a) - 3    !  Pretend the year begins in March and ends in Feb.

    jdn = day + &
          (153*m + 2)/5 + &               !  Number of days in the previous months
          (365*y) + &                     !  Duh.
          (y/4) - (y/100) + (y/400) - &   !  Number of leap years since -4800
          32045                           !  Ensure result is 0 for Jan 1, 4713 BCE.

  END FUNCTION JulianDay

END MODULE Date_Utility
