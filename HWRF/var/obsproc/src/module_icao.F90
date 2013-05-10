MODULE MODULE_ICAO

!------------------------------------------------------------------------------!
! Collection of functions to compute h, p and t using standard atmosphere
! t is the temperaure in K,
! h is the height in m (h=0 at 1013.25hPa)
! P is pressure in Pa
!
! References:
! ----------
!  Vasiljevic et al. 1992: ECMWF 3D Variational data assimilation 
!                          of conventional observations
!  In proceedings of the ECMWF Workshop on Variational assimilation, with
!  special emphasis on three dimensional aspects. 9-12 November 1992, pp 389-435
!
!  F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------!

  include 'constants.inc'
  include 'missing.inc'

  REAL, PARAMETER :: p_0    = 101325.,  & ! ICAO reference pressure Pa
                     t_0    = 288.,     & ! ICAO reference temperature K
                     lambda = 0.0065,   & ! ICAO temperature lapse rate K/m
                     alpha  = lambda * gasr / g ! ICAO constant (no unit)

  REAL            :: height_max_icao

CONTAINS

!------------------------------------------------------------------------------!
 FUNCTION t_from_h_icao (h) RESULT (t)

 IMPLICIT NONE
 REAL :: h, t

 t = t_0 - lambda * h

 END FUNCTION t_from_h_icao
!------------------------------------------------------------------------------!
 FUNCTION t_from_p_icao (p) RESULT (t)

 IMPLICIT NONE
 REAL :: p, t

 t = t_0 *(p / p_0) ** alpha

 END FUNCTION t_from_p_icao
!------------------------------------------------------------------------------!
 FUNCTION h_from_p_icao (p) RESULT (h)

 IMPLICIT NONE
 REAL :: p, h

 h = t_0 / lambda * (1. - (p / p_0) ** alpha)

 END FUNCTION h_from_p_icao
!------------------------------------------------------------------------------!
 FUNCTION p_from_h_icao (h) RESULT (p)

 IMPLICIT NONE
 REAL :: p, h, one_over_alpha

 one_over_alpha = 1. /alpha

   p = p_0 * (1. - lambda * h / t_0)** one_over_alpha

 END FUNCTION p_from_h_icao
!------------------------------------------------------------------------------!
END MODULE MODULE_ICAO
