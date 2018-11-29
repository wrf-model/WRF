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

subroutine swap4f(in,nn)
#if defined (__alpha) || defined (__linux) || defined (__G95__)
! swaps bytes in groups of 4 to compensate for byte swapping within
!    words which occurs on DEC machines.
  implicit none
  integer, intent(in) :: nn ! number of bytes to be swapped
  logical*1 , dimension(nn) , intent(inout) :: in  ! Array to be swapped
#if defined (BIT32)
  integer, parameter :: nbytes=4
#elif defined (BIT64)
  integer, parameter :: nbytes=8
#else
  "Please use the CPP option -BIT32 or -BIT64"
#endif

  logical*1, dimension(nbytes) :: ia
  integer :: i
  do i=1,nn,nbytes
     ia = in(i+(nbytes-1):i:-1)
     in(i:i+(nbytes-1)) = ia
  enddo
	
#endif
end
!KWM -------------------------------------------------------------------------
!KWM -------------------------------------------------------------------------
!KWM -------------------------------------------------------------------------
!KWM
!KWM    Here are the original swap routines.  I use only SWAP4.  I modified
!KWM    SWAP4 to use the same array on input and output, rather than make a
!KWM    new output array IO.  I keep these around just for reference.
!KWM
!KWM -------------------------------------------------------------------------
!KWM -------------------------------------------------------------------------
!KWM -------------------------------------------------------------------------
!KWM
!KWM	subroutine swap4(in,io,nn)
!KWM#if defined (DEC) || defined (ALPHA)
!KWM! swaps bytes in groups of 4 to compensate for byte swapping within
!KWM!    words which occurs on DEC (VAX) and PC machines.
!KWM!
!KWM! in - input array to be swapped
!KWM! io - ouput array with bytes swapped
!KWM! nn - number of bytes to be swapped
!KWM	logical*1   in(1),io(1),ih
!KWM!       character*1 in(1),io(1),ih             ! Cray CF90 (Version 3.0.1.3)
!KWM!          Use character*1 instead of logical*1 when compling on a Cray
!KWM	do 10 i=1,nn,4
!KWM	ih=in(i)
!KWM	io(i)=in(i+3)
!KWM	io(i+3)=ih
!KWM	ih=in(i+1)
!KWM	io(i+1)=in(i+2)
!KWM	io(i+2)=ih
!KWM   10	continue
!KWM	return
!KWM#endif
!KWM	end
!KWM	subroutine swap2(in,io,nn)
!KWM#if defined (DEC) || defined (ALPHA)
!KWM! swaps bytes in groups of 2 to compensate for byte swapping within
!KWM!    words which occurs on DEC (VAX) and PC machines.
!KWM!
!KWM! in - input array to be swapped
!KWM! io - ouput array with bytes swapped
!KWM! nn - number of bytes to be swapped
!KWM	logical*1   in(1),io(1),ih
!KWM!       character*1 in(1),io(1),ih             ! Cray CF90 (Version 3.0.1.3)
!KWM!          Use character*1 instead of logical*1 when compling on a Cray
!KWM	do 10 i=1,nn,2
!KWM	ih=in(i)
!KWM	io(i)=in(i+1)
!KWM	io(i+1)=ih
!KWM   10	continue
!KWM	return
!KWM#endif
!KWM	end
!KWM
!KWM	subroutine filt(m,n,in)
!KWM#if defined (DEC) || defined (ALPHA)
!KWM	logical*1 m(1),n(1),l,u
!KWM	data l/32/,u/127/
!KWM	do 10 i=1,in
!KWM	n(i)=m(i)
!KWM	if(n(i).lt.  l) n(i)=l  
!KWM	if(n(i).gt.  u) n(i)=u   
!KWM   10	continue
!KWM	return
!KWM#endif
!KWM	end
!KWM Emacs Local Variables: ***
!KWM Emacs mode: f90        ***
!KWM Emacs End:             ***
