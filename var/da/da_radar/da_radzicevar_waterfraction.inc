  subroutine da_radzicevar_waterfraction(qr,qice,waterfraction)
  !----------------------------------------------------------------------
  ! purpose: calculate the water fraction fw
  !----------------------------------------------------------------------
  implicit none
  real :: qr,qice
  real :: waterfraction
 
  if(qr<1.e-8.and.qice<1.e-8) then
    waterfraction=1.e-8
  else
    waterfraction=qr/(qr+qice) 
  endif

  end subroutine da_radzicevar_waterfraction
