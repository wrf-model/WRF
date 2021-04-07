      PROGRAM fortran_2003_test         

      IMPLICIT NONE

      REAL :: x, y

      x = -1.
      y =  sqrt(x)

      IF ( ISNAN(y) ) THEN
         PRINT *,'Apparently y is a NaN
      ENDIF

      END PROGRAM fortran_2003_test         
