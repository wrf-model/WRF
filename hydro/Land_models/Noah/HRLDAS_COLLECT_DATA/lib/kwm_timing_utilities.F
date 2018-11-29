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

module kwm_timing_utilities
  implicit none
  integer, parameter :: kwm_timing_dimension = 100
  integer, dimension(kwm_timing_dimension) :: Acount, Asum
  
contains

  subroutine timing_start(num)
    implicit none
    integer, intent(in) :: num
    if (num > kwm_timing_dimension) then
       stop "TIMING_START PROBLEM.  USE A SMALLER NUM."
    endif
    call system_clock(Acount(num))
  end subroutine timing_start

  subroutine timing_end(num, string, ivrb)
    implicit none
    integer, intent(in) :: num, ivrb
    integer :: count_rate, count_max
    character(len=*), intent(in) :: string
    integer :: Bcount
    call system_clock(count=Bcount, count_rate=count_rate, count_max=count_max)

    Asum(num) = Asum(num)+(Bcount-Acount(num))
    if (ivrb /= 0) then
       print*,' Timing for '//string//': ', float(Bcount-Acount(num))/float(count_rate), &
            float(Asum(num))/float(count_rate)
    endif
  end subroutine timing_end

end module kwm_timing_utilities
