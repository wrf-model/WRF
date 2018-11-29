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

integer function get_unused_unit() result(iunit)
  implicit none
  integer :: i
  logical :: used

  do i = 11, 255
     inquire(unit=i, opened=used)
     if (.not. used) then
        iunit = i
        return
     endif
  enddo

  print*, "GET_UNUSED_UNIT:  "
  print*, "      Problem getting unused unit number."
  call abort()

end function get_unused_unit
