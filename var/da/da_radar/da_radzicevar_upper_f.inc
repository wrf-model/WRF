  subroutine da_radzicevar_upper_f(upper_f,qr,qice,qthres,flg)
  !----------------------------------------------------------------------
  ! purpose: calculate the fraction F
  !----------------------------------------------------------------------
  implicit none
  real :: qr, qice,qthres
  real :: upper_f
  real :: fmax=0.5
  integer :: flg

  if(flg==1) fmax=0.5 ! for snow
  if(flg==2) fmax=0.3 ! for hail/graupel
      
  upper_f=0
  if(qr>=qthres.and.qice>=qthres) then
    upper_f=fmax*min(qice/qr,qr/qice)**0.3
  endif

  end subroutine da_radzicevar_upper_f
