      PROGRAM fortran_2003_test         

      USE , INTRINSIC :: ISO_C_BINDING

      IMPLICIT NONE

      INTEGER , PARAMETER :: STR_LEN = 10

      type, bind(c) :: r_info
          integer(c_int64_t)                         :: offset
          integer(c_int64_t)                         :: data_offset
          integer(c_int32_t)                         :: data_count
          integer(c_int32_t)                         :: data_type
          character(kind=c_char), dimension(STR_LEN) :: name
          character(kind=c_char), dimension(STR_LEN) :: date
      end type r_info

      REAL :: x , y , z

      x = 1.e+10
      y = 1.e-10
      z = x + y

      END PROGRAM fortran_2003_test         
