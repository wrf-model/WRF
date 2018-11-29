!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

!WRF:DRIVER_LAYER:UTIL
!

SUBROUTINE wrf_message( str )
  IMPLICIT NONE

  CHARACTER*(*) str
  print *,trim(str)

END SUBROUTINE wrf_message

SUBROUTINE wrf_error_fatal( str )
  IMPLICIT NONE
  CHARACTER*(*) str
#ifdef NCEP_WCOSS
  write(78,*) 'FATAL ERROR: ',trim(str)
  call flush(78)
  close(78)
#else 
  write(6,*),'FATAL ERROR: ',trim(str)
  call flush(6)
#endif

  CALL wrf_abort
END SUBROUTINE wrf_error_fatal

SUBROUTINE wrf_abort
      STOP 'wrf_abort'
END SUBROUTINE wrf_abort

SUBROUTINE wrf_debug( level , str ) 
  IMPLICIT NONE 
  CHARACTER*(*) str 
  INTEGER , INTENT (IN) :: level 
  CALL wrf_message( str ) 
  RETURN 
END SUBROUTINE wrf_debug 

subroutine wrf_dm_bcast_real(rval, ival)
  implicit none
  real,    intent(in) :: rval
  integer, intent(in) :: ival
end subroutine wrf_dm_bcast_real

subroutine wrf_dm_bcast_integer(rval, ival)
  implicit none
  integer, intent(in) :: rval
  integer, intent(in) :: ival
end subroutine wrf_dm_bcast_integer

subroutine wrf_dm_bcast_string(rval, ival)
  implicit none
  character(len=*), intent(in) :: rval
  integer,          intent(in) :: ival
end subroutine wrf_dm_bcast_string
