  subroutine da_radzicevar_calc_ice_abc(phimean,sigma,ice_abc)
  !----------------------------------------------------------------------
  ! purpose: calculate the A B C in ice-phase equations
  !----------------------------------------------------------------------
  implicit none
  real :: phimean,sigma
  real :: ice_abc(3)  ! 1 for A, 2 for B, 3 for C
  real,parameter :: pi=3.1415926
  real :: a2a=pi/180.

  ice_abc(1)=1./8.*(3.+4.*cos(2.*phimean*pi)*exp(-2.*sigma**2)  &
              +cos(4.*phimean)*exp(-8.*sigma**2))
  ice_abc(2)=1./8.*(3.-4.*cos(2.*phimean*pi)*exp(-2.*sigma**2) &
              +cos(4.*phimean)*exp(-8.*sigma**2))
  ice_abc(3)=1./8.*(1.                                          &
              -cos(4.*phimean)*exp(-8.*sigma**2)) 

  end subroutine da_radzicevar_calc_ice_abc
