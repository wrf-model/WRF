SUBROUTINE error_handler (calling_routine,message1,message2,fatal)

!------------------------------------------------------------------------------!
!  Print out an error message
! 
!  D. GILL,         April 1998
!  F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------!

   IMPLICIT NONE

   CHARACTER (LEN=*) , INTENT ( IN )          :: calling_routine
   CHARACTER (LEN=*) , INTENT ( IN )          :: message1
   CHARACTER (LEN=*) , INTENT ( IN )          :: message2
   LOGICAL , INTENT ( IN )                    :: fatal 

   WRITE (0,'(/,A,1X,A)') TRIM (message1),TRIM (message2)

   IF (fatal) THEN
      WRITE (0,'(/,A,A,/)') "FATAL ERROR: STOP in ",TRIM (calling_routine)
      STOP
   ELSE
      WRITE (0,'(/,A,A,/)') "WARNING: ERROR in ",TRIM (calling_routine)
   ENDIF

END SUBROUTINE error_handler
