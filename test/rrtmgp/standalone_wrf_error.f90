  SUBROUTINE wrf_error_fatal3(file,line,message)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: file,message
    INTEGER, INTENT(IN) :: line
    WRITE(*,'(A,":",I0,": ",A)') TRIM(file),line,TRIM(message)
    ERROR STOP 1
  END SUBROUTINE wrf_error_fatal3
