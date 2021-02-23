  subroutine da_radzicevar_sigma_in_abc(qg,fw,gsflag,sigma_in_abc)
  !----------------------------------------------------------------------
  ! purpose: calculate the standard deviation of canting angle
  !----------------------------------------------------------------------
  implicit none
  real :: qg,fw
  integer :: gsflag
  real :: sigma_in_abc
  real,parameter :: pi=3.1415926
  real :: a2a=pi/180.

  if(gsflag==1) then ! for snow
    sigma_in_abc=20.*a2a
  elseif(gsflag==2) then ! for graupel
    if(qg>0.2*1e-3) then 
      sigma_in_abc=60.*a2a*(1-0.8*fw)
    else
      sigma_in_abc=60.*a2a*(1-4.*qg*1e3*fw)
    endif
  endif

  end subroutine da_radzicevar_sigma_in_abc
