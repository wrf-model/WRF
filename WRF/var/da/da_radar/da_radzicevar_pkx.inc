  subroutine da_radzicevar_pkx(ice_abc,pxabk_all,pkx)
  !----------------------------------------------------------------------
  ! purpose: calculate Pkx
  !----------------------------------------------------------------------
  implicit none
  real :: ice_abc(3),pxabk_all(3)
  real :: pkx
  integer :: i

  pkx=0
  do i=1,3
    if(i<3) then
      pkx=pkx+ice_abc(i)*pxabk_all(i)
    else
      pkx=pkx+2.*ice_abc(i)*pxabk_all(i)
    endif
  enddo

  end subroutine da_radzicevar_pkx
