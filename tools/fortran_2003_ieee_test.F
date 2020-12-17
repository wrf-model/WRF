      PROGRAM fortran_2003_test         

      USE , INTRINSIC :: IEEE_EXCEPTIONS

      IMPLICIT NONE

      TYPE bunch_of_stuff
         INTEGER,POINTER,DIMENSION(:) :: i
         REAL   ,POINTER,DIMENSION(:) :: x
         LOGICAL,POINTER,DIMENSION(:) :: l
      END TYPE bunch_of_stuff

      TYPE outer_wrapper
         TYPE(bunch_of_stuff), ALLOCATABLE, DIMENSION(:) :: ddt_things
      END TYPE outer_wrapper

      TYPE(outer_wrapper), DIMENSION(15) :: combo

      REAL :: x , y , z
      INTEGER :: return_code
      CHARACTER (LEN=256) :: my_shell

      x = 1.e+10
      y = 1.e-10
      z = x + y

      ALLOCATE(combo(1)%ddt_things(10))
      ALLOCATE(combo(2)%ddt_things(10))
      ALLOCATE(combo(3)%ddt_things(10))

      CALL IEEE_SET_FLAG ( IEEE_INEXACT , .false. )

      CALL GET_ENVIRONMENT_VARIABLE ( NAME="SHELL" , VALUE=my_shell ,   &
     &                                STATUS=return_code )

      PRINT *,'x,y,z = ',x,y,z
      PRINT *,'SHELL = ',TRIM(my_shell)

      FLUSH (UNIT=6)

      STOP
      END PROGRAM fortran_2003_test         
