!
! An adaptation of KWM's subroutine for computing RH.
!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     PURPOSE  COMPUTES THE RELATIVE HUMIDITY FROM THE TEMPERATURE,
!              MIXING RATIO, AND PRESSURE.
!
!     INPUT       Q        MIXING RATIO               kg/kg
!                 T        T                          K
!                 P        P                          Pa
!
!     OUTPUT      RH       RELATIVE HUMIDITY          %
!
!      DIMENSION Q ,T, P
!      DIMENSION RH
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function calc_rh(t, q, p)

   implicit none

   ! Arguments
   real t, q, p

   ! Return value
   real calc_rh

   ! Local variables
   real :: es, qs
 
   real, parameter :: e0    = 611.2
   real, parameter :: svp2  = 17.67
   real, parameter :: svp3  = 29.65
   real, parameter :: t00   = 273.15
   real, parameter :: eps   = 0.622

   es=e0*exp(svp2*(t-t00)/(t-svp3))
   qs=eps*es/(p-es)
   calc_rh=100.*q/qs

end function calc_rh
