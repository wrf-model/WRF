!
! Sort_Utility
!
! Module containing routines for sorting
!
!
! CREATION HISTORY:
!       Written by: Paul van Delst, 30-May-2006
!                   paul.vandelst@noaa.gov
!

MODULE Sort_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  ! Diable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public routines in this module
  PUBLIC :: InsertionSort
  PUBLIC :: UniqueCount
  PUBLIC :: Unique


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE InsertionSort
    MODULE PROCEDURE iSort_Long
    MODULE PROCEDURE iSortIdx_Long
    MODULE PROCEDURE iSort_Char
    MODULE PROCEDURE iSortIdx_Char
!    MODULE PROCEDURE iSort_Single
!    MODULE PROCEDURE iSort_Double
  END INTERFACE InsertionSort

  INTERFACE UniqueCount
    MODULE PROCEDURE UniqueCount_Long
    MODULE PROCEDURE UniqueCount_Char
  END INTERFACE UniqueCount
  
  INTERFACE Unique
    MODULE PROCEDURE Unique_Long
    MODULE PROCEDURE Unique_Char
  END INTERFACE Unique
  
  
CONTAINS


  PURE SUBROUTINE iSort_Long( x )
    INTEGER(Long), DIMENSION(:), INTENT(IN OUT) :: x
    INTEGER(Long) :: t
    INTEGER :: n, i, j
    n=SIZE(x)
    DO i = 2, n
      t = x(i)
      j = i
      DO
        IF (    j   < 2 ) EXIT
        IF ( x(j-1) < t ) EXIT  ! Separate exit since no short circuit
        x(j) = x(j-1)
        j = j-1
      END DO
      x(j) = t
    END DO
  END SUBROUTINE iSort_Long

  PURE SUBROUTINE iSortIdx_Long( x, Idx )
    INTEGER(Long), DIMENSION(:),       INTENT(IN)  :: x
    INTEGER,       DIMENSION(SIZE(x)), INTENT(OUT) :: Idx
    INTEGER(Long) :: t
    INTEGER :: u
    INTEGER :: n, i, j
    n=SIZE(x)
    Idx=(/(i,i=1,n)/)
    DO i = 2, n
      u = Idx(i)
      t = x(u)
      j = i
      DO 
        IF (     j       < 2 ) EXIT
        IF ( x(Idx(j-1)) < t ) EXIT  ! Separate exit since no short circuit
        Idx(j)   = Idx(j-1)
        j = j-1
      END DO
      Idx(j) = u
    END DO
  END SUBROUTINE iSortIdx_Long


  PURE SUBROUTINE iSort_Char( x )
    CHARACTER(*), DIMENSION(:), INTENT(IN OUT) :: x
    CHARACTER(LEN(x(1))) :: t
    INTEGER :: n, i, j
    n=SIZE(x)
    DO i = 2, n
      t = x(i)
      j = i
      DO
        IF (      j < 2    ) EXIT
        IF ( LLT(x(j-1),t) ) EXIT  ! Separate exit since no short circuit
        x(j) = x(j-1)
        j = j-1
      END DO
      x(j) = t
    END DO
  END SUBROUTINE iSort_Char


  PURE SUBROUTINE iSortIdx_Char( x, Idx )
    CHARACTER(*), DIMENSION(:),       INTENT(IN)  :: x
    INTEGER,      DIMENSION(SIZE(x)), INTENT(OUT) :: Idx
    CHARACTER(LEN(x(1))) :: t
    INTEGER :: u
    INTEGER :: n, i, j
    n=SIZE(x)
    Idx=(/(i,i=1,n)/)
    DO i = 2, n
      u = Idx(i)
      t = x(u)
      j = i
      DO 
        IF (        j < 2       ) EXIT
        IF ( LLT(x(Idx(j-1)),t) ) EXIT  ! Separate exit since no short circuit
        Idx(j)   = Idx(j-1)
        j = j-1
      END DO
      Idx(j) = u
    END DO
  END SUBROUTINE iSortIdx_Char


  PURE FUNCTION UniqueCount_Long( x ) RESULT( n )
    INTEGER(Long), INTENT(IN) :: x(:)
    INTEGER(Long) :: n
    INTEGER(Long) :: lx(SIZE(x))
    n = SIZE(x)
    IF ( n < 2 ) RETURN
    lx = x
    CALL InsertionSort(lx)
    n = COUNT(lx /= CSHIFT(lx,-1))
  END FUNCTION UniqueCount_Long


  PURE FUNCTION UniqueCount_Char( x ) RESULT( n )
    CHARACTER(*), INTENT(IN) :: x(:)
    INTEGER(Long) :: n
    CHARACTER(LEN(x)) :: lx(SIZE(x))
    n = SIZE(x)
    IF ( n < 2 ) RETURN
    lx = x
    CALL InsertionSort(lx)
    n = COUNT(lx /= CSHIFT(lx,-1))
  END FUNCTION UniqueCount_Char


  PURE FUNCTION Unique_Long( x ) RESULT( ux )
    INTEGER(Long), INTENT(IN) :: x(:)
    INTEGER(Long) :: ux(UniqueCount(x))
    INTEGER(Long) :: lx(SIZE(x))
    IF ( SIZE(x) == 1 ) THEN
      ux = x
      RETURN
    END IF
    lx = x
    CALL InsertionSort(lx)
    ux = PACK(lx, lx /= CSHIFT(lx,-1))
  END FUNCTION Unique_Long


  PURE FUNCTION Unique_Char( x ) RESULT( ux )
    CHARACTER(*), INTENT(IN) :: x(:)
    CHARACTER(LEN(x)) :: ux(UniqueCount(x))
    CHARACTER(LEN(x)) :: lx(SIZE(x))
    IF ( SIZE(x) == 1 ) THEN
      ux = x
      RETURN
    END IF
    lx = x
    CALL InsertionSort(lx)
    ux = PACK(lx, lx /= CSHIFT(lx,-1))
  END FUNCTION Unique_Char

END MODULE Sort_Utility
