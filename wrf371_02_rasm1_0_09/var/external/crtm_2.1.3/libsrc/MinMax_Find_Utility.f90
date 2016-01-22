MODULE MinMax_Find_Utility

  USE Type_Kinds, ONLY: fp
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: MinVal_Find
  PUBLIC :: MaxVal_Find
  PUBLIC :: MaxDiff_Find
  
  ! Public
  INTERFACE MinVal_Find
    MODULE PROCEDURE integer_MinVal_Find
    MODULE PROCEDURE real_MinVal_Find
  END INTERFACE MinVal_Find
  
  INTERFACE MaxVal_Find
    MODULE PROCEDURE integer_MaxVal_Find
    MODULE PROCEDURE real_MaxVal_Find
  END INTERFACE MaxVal_Find

  INTERFACE MaxDiff_Find
    MODULE PROCEDURE integer_MaxDiff_Find
    MODULE PROCEDURE real_MaxDiff_Find
  END INTERFACE MaxDiff_Find

  ! Private
  INTERFACE MinLoc_Find
    MODULE PROCEDURE integer_MinLoc_Find
    MODULE PROCEDURE real_MinLoc_Find
  END INTERFACE MinLoc_Find
  
  INTERFACE MaxLoc_Find
    MODULE PROCEDURE integer_MaxLoc_Find
    MODULE PROCEDURE real_MaxLoc_Find
  END INTERFACE MaxLoc_Find
  

CONTAINS


  FUNCTION integer_MinVal_Find(array) RESULT(minimum_value)
    INTEGER, INTENT(IN) :: array(:)
    INTEGER :: minimum_value
    INTEGER :: idx  
    idx = MinLoc_Find(array)
    minimum_value = SIGN(array(idx),array(idx))
  END FUNCTION integer_MinVal_Find
  
  FUNCTION integer_MaxVal_Find(array) RESULT(maximum_value)
    INTEGER, INTENT(IN) :: array(:)
    INTEGER :: maximum_value
    INTEGER :: idx  
    idx = MaxLoc_Find(array)
    maximum_value = SIGN(array(idx),array(idx))
  END FUNCTION integer_MaxVal_Find
  
  SUBROUTINE integer_MaxDiff_Find(x,y, xval,yval,diff)
    INTEGER, INTENT(IN) :: x(:), y(:)
    INTEGER :: xval, yval, diff
    INTEGER :: idx  
    idx = MaxLoc_Find(x-y)
    xval = x(idx)
    yval = y(idx)
    diff = xval - yval
  END SUBROUTINE integer_MaxDiff_Find

  FUNCTION real_MinVal_Find(array) RESULT(minimum_value)
    REAL(fp), INTENT(IN) :: array(:)
    REAL(fp) :: minimum_value
    INTEGER :: idx  
    idx = MinLoc_Find(array)
    minimum_value = SIGN(array(idx),array(idx))
  END FUNCTION real_MinVal_Find
  
  FUNCTION real_MaxVal_Find(array) RESULT(maximum_value)
    REAL(fp), INTENT(IN) :: array(:)
    REAL(fp) :: maximum_value
    INTEGER :: idx  
    idx = MaxLoc_Find(array)
    maximum_value = SIGN(array(idx),array(idx))
  END FUNCTION real_MaxVal_Find

  SUBROUTINE real_MaxDiff_Find(x,y, xval,yval,diff)
    REAL(fp), INTENT(IN) :: x(:), y(:)
    REAL(fp) :: xval, yval, diff
    INTEGER :: idx  
    idx = MaxLoc_Find(x-y)
    xval = x(idx)
    yval = y(idx)
    diff = xval - yval
  END SUBROUTINE real_MaxDiff_Find


! PRIVATE PROCEDURES

  FUNCTION integer_MinLoc_Find(array) RESULT(idx)
    INTEGER, INTENT(IN) :: array(:)
    INTEGER :: idx  
    idx = MINLOC(ABS(array),DIM=1)
  END FUNCTION integer_MinLoc_Find
  
  FUNCTION integer_MaxLoc_Find(array) RESULT(idx)
    INTEGER, INTENT(IN) :: array(:)
    INTEGER :: idx  
    idx = MAXLOC(ABS(array),DIM=1)
  END FUNCTION integer_MaxLoc_Find
  
  FUNCTION real_MinLoc_Find(array) RESULT(idx)
    REAL(fp), INTENT(IN) :: array(:)
    INTEGER :: idx  
    idx = MINLOC(ABS(array),DIM=1)
  END FUNCTION real_MinLoc_Find
  
  FUNCTION real_MaxLoc_Find(array) RESULT(idx)
    REAL(fp), INTENT(IN) :: array(:)
    INTEGER :: idx  
    idx = MAXLOC(ABS(array),DIM=1)
  END FUNCTION real_MaxLoc_Find
  
  
END MODULE MinMax_Find_Utility
