!
! Search_Utility
!
! Module containing data searching routines
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 06-Oct-2006
!                       paul.vandelst@noaa.gov
!

MODULE Search_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Modules used
  USE Type_Kinds,      ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Value_Locate
  PUBLIC :: Bisection_Search


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: Search_Utility.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       Bisection_Search
!
! PURPOSE:
!       Function to search an array using the bisection method. This function
!       is an adaptation from Numerical Recipes and is most efficient across
!       multiple calls when the value to be searched for in the array occurs
!       randomly.
!
! CALLING SEQUENCE:
!       j = Bisection_Search( x, u,          &  ! Input
!                             xLower=xLower, &  ! Optional input
!                             xUpper=xUpper  )  ! Optional input
!
! INPUT ARGUMENTS:
!       x:         The array to be searched.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
!       u:         The value to be searched for in the array.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       xLower:   Set this optional argument to the index of the input
!                  array corresponding to the LOWER search boundary.
!                  If not specified, the default value is 1.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       xUpper:   Set this optional argument to the index of the input
!                  array corresponding to the UPPER search boundary.
!                  If not specified, the default value is SIZE(x).
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       j:         The lower index of the two values in the input array, x,
!                  that bracket the input value, u, i.e.
!                    x(j) < u < x(j+1)
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Nov-2000
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Bisection_Search( x, u, xLower, xUpper ) RESULT( j )
    ! Arguments
    REAL(fp), DIMENSION(:), INTENT(IN) :: x
    REAL(fp),               INTENT(IN) :: u
    INTEGER,  OPTIONAL,     INTENT(IN) :: xLower
    INTEGER,  OPTIONAL,     INTENT(IN) :: xUpper
    ! Function result
    INTEGER :: j
    ! Local variables
    INTEGER :: n
    INTEGER :: jLower
    INTEGER :: jMiddle
    INTEGER :: jUpper

    ! Set up
    n = SIZE( x )

    ! Initialise upper and lower limits to
    ! the valid maximums, 1 and n
    IF ( PRESENT(xLower) ) THEN
      jLower = xLower
    ELSE
      jLower = 1
    END IF

    IF ( PRESENT(xUpper) ) THEN
      jUpper = xUpper
    ELSE
      jUpper = n
    END IF


    ! Search for the required index by bisection
    Bisection_Search_Loop: DO

      ! If the index ranges have converged, we're done
      IF ( (jUpper-jLower) <= 1 ) EXIT Bisection_Search_Loop

      ! Define a middle point
      jMiddle = ( jLower + jUpper ) / 2

      ! Which half is the required value in?
      IF ( ( x(n) > x(1) ) .EQV. ( u > x(jMiddle) ) ) THEN
        jLower = jMiddle ! The "upper" half
      ELSE
        jUpper = jMiddle ! The "lower" half
      END IF

    END DO Bisection_Search_Loop

    ! Define the return value
    j = jLower

  END FUNCTION Bisection_Search


!------------------------------------------------------------------------------
!
! NAME:
!       Value_Locate
!
! PURPOSE:
!       Function that finds the intervals within a given monotonic
!       vector that brackets a given set of one or more search values.
!
!       This function is an adaptation of the locate() routine in 
!       Numerical Recipes and uses the bisection method to locate the
!       interval.
!
! CALLING SEQUENCE:
!       j = Value_Locate( x, u ) ! Input
!
! INPUT ARGUMENTS:
!       x:         The array to be searched.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
!       u:         The array of values to be searched for in the array.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       j:         The integer array of lower indices of the two values in
!                  the input array, x, that bracket the input values, u.
!                  E.g. for a given u(i):
!                     x(j) < u(i) < x(j+1)
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Rank-1, same size as u input argument.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Nov-2000
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Value_Locate( x, u ) RESULT( j )
    ! Arguments
    REAL(fp), DIMENSION(:), INTENT(IN) :: x
    REAL(fp), DIMENSION(:), INTENT(IN) :: u
    ! Function result
    INTEGER, DIMENSION(SIZE(u)) :: j
    ! Local variables
    INTEGER :: nx
    INTEGER :: nu, iu
    INTEGER :: xLower, xUpper
    LOGICAL :: ascending

    ! Set up
    nx = SIZE(x)
    nu = SIZE(u)

    ! Determine if arrays are sorted in ascending or descending order
    IF ( x(nx) > x(1) .AND. u(nu) > u(1) ) THEN
      ascending = .TRUE.
    ELSEIF ( x(nx) < x(1) .AND. u(nu) < u(1) ) THEN
      ascending = .FALSE.
    ELSE
      j(:) = -1
      RETURN
    END IF

    ! Perform the bisection search for each element of u
    IF ( ascending ) THEN
      ! Going up
      xLower = 1
      DO iu = 1, nu
        j(iu) = Bisection_Search( x, u(iu), xLower = xLower )
        xLower = MAX( 1, j(iu) )
      END DO
    ELSE
      ! Going down
      xUpper = nx
      DO iu = 1, nu
        j(iu) = Bisection_Search( x, u(iu), xUpper = xUpper )
        xLower = MIN( j(iu), nx )
      END DO
    END IF

  END FUNCTION Value_Locate

END MODULE Search_Utility
