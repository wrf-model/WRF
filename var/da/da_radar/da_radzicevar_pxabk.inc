  subroutine da_radzicevar_pxabk(alpha_rxa,alpha_rxb,k,n,pxabk)
  !----------------------------------------------------------------------
  ! purpose: calculate PxA, PxB, PxC for Pxk
  !----------------------------------------------------------------------
  implicit none
  integer :: n
  integer :: k
  real :: alpha_rxa(n),alpha_rxb(n)
  real :: pxabk
  integer :: i,j

  pxabk=0
  do j=0,n-1
    do i=0,n-1
      if(i+j==k) then
        pxabk=pxabk+alpha_rxa(i+1)*alpha_rxb(j+1)
      endif
    enddo
  enddo

  end subroutine da_radzicevar_pxabk
