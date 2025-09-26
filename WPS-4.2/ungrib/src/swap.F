subroutine swap4(in,nn)
!#if defined (DEC) || defined (ALPHA) || defined (alpha) || defined (LINUX)
#ifdef BYTESWAP
! swaps bytes in groups of 4 to compensate for byte swapping within
!    words
  implicit none
  integer, intent(in) :: nn ! number of bytes to be swapped
  logical*1 , dimension(nn) , intent(inout) :: in  ! Array to be swapped

  logical*1, dimension(4) :: ia
  integer :: i
  do i=1,nn,4
     ia = in(i+3:i:-1)
     in(i:i+3) = ia
  enddo
	
#endif
end
