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

  ! Environment setup
  ! -----------------
  IMPLICIT NONE

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
  PUBLIC :: Is_Leap_Year
  PUBLIC :: Day_of_Year
  PUBLIC :: Days_in_Month


  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Date_Utility.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'

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


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Is_Leap_Year
!
! PURPOSE:
!       Elemental function to determine if a specified year is a leap year.
!
! CALLING SEQUENCE:
!       Result = Is_Leap_Year( Year )
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

  ELEMENTAL FUNCTION Is_Leap_Year( Year )
    INTEGER, INTENT(IN) :: Year
    LOGICAL :: Is_Leap_Year

    Is_Leap_Year = ( (MOD(Year,4)   == 0) .AND. &
                     (MOD(Year,100) /= 0)       ) .OR. &
                   (MOD(Year,400) == 0)

  END FUNCTION Is_Leap_Year


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Day_of_Year
!
! PURPOSE:
!       Elemental function to convert input numeric (e.g. DD,MM,YYYY) date
!       information to a day of year.
!
! CALLING SEQUENCE:
!       DoY = Day_of_Year ( Day, Month, Year )
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

  ELEMENTAL FUNCTION Day_of_Year( &
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
    IF ( Is_Leap_Year(Year) ) Days_per_Month(2) = 29
    IF ( Day > Days_per_Month(Month) ) RETURN

    ! Compute the day of year
    DoY = SUM(Days_per_Month(1:Month-1)) + Day

  END FUNCTION Day_of_Year
  
  
!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Days_in_Month
!
! PURPOSE:
!       Elemental function to return the number of days in a given
!       month and year.
!
! CALLING SEQUENCE:
!       n_Days = Days_in_Month( Month, Year )
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

  ELEMENTAL FUNCTION Days_in_Month(Month, Year) RESULT(n_Days)
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
    IF ( Is_Leap_Year(Year=Year) ) Days_per_Month(2) = 29

    ! Set the number of days
    n_Days = Days_per_Month(Month)

  END FUNCTION Days_in_Month

END MODULE Date_Utility
