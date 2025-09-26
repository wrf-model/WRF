!#**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! miscellaneous debuging routines for CBMZ and MOSAIC
!**********************************************************************************  
	module module_peg_util


	contains


!-----------------------------------------------------------------------
	subroutine peg_debugmsg( lun, level, str )
!
! when lun >  0, writes "str" to unit "lun"
! when lun <= 0, passes "str" on to wrf_debug
!
	implicit none
! subr arguments
	integer, intent(in) :: lun, level
	character(len=*), intent(in) :: str
! local variables
	integer n

	n = max( 1, len_trim(str) )
	if (lun .gt. 0) then
	    write(lun,'(a)') str(1:n)
	else
	    call wrf_debug( level, str(1:n) )
	end if
	return
	end subroutine peg_debugmsg


!-----------------------------------------------------------------------
	subroutine peg_message( lun, str )
!
! when lun >  0, writes "str" to unit "lun"
! when lun <= 0, passes "str" on to wrf_message
!
	implicit none
! subr arguments
	integer, intent(in) :: lun
	character(len=*), intent(in) :: str
! local variables
	integer n

	n = max( 1, len_trim(str) )
	if (lun .gt. 0) then
	    write(lun,'(a)') str(1:n)
	else
	    call wrf_message( str(1:n) )
	end if
	return
	end subroutine peg_message


!-----------------------------------------------------------------------
	subroutine peg_error_fatal( lun, str )
!
! when lun >  0, writes "str" to unit "lun"
! then (always)  passes "str" on to wrf_error_fatal
!
	implicit none
! subr arguments
	integer, intent(in) :: lun
	character(len=*), intent(in) :: str
! local variables
	integer n

	n = max( 1, len_trim(str) )
	if (lun .gt. 0) write(lun,'(a)') str(1:n)
	call wrf_error_fatal( str(1:n) )
	return
	end subroutine peg_error_fatal


!-----------------------------------------------------------------------
	end module module_peg_util
