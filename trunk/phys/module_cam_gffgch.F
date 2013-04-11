#define WRF_PORT
#define MODAL_AERO
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
!------------------------------------------------------------------------
! Based on gffgch.F90 from CAM
! Ported to WRF by William.Gustafson@pnl.gov, Nov. 2009
!------------------------------------------------------------------------

#ifdef WRF_PORT
module module_cam_gffgch

  implicit none

  private
  public gffgch

contains
#endif

subroutine gffgch(t       ,es      ,itype   )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Computes saturation vapor pressure over water and/or over ice using
! Goff & Gratch (1946) relationships. 
! <Say what the routine does> 
! 
! Method: 
! T (temperature), and itype are input parameters, while es (saturation
! vapor pressure) is an output parameter.  The input parameter itype
! serves two purposes: a value of zero indicates that saturation vapor
! pressures over water are to be returned (regardless of temperature),
! while a value of one indicates that saturation vapor pressures over
! ice should be returned when t is less than freezing degrees.  If itype
! is negative, its absolute value is interpreted to define a temperature
! transition region below freezing in which the returned
! saturation vapor pressure is a weighted average of the respective ice
! and water value.  That is, in the temperature range 0 => -itype
! degrees c, the saturation vapor pressures are assumed to be a weighted
! average of the vapor pressure over supercooled water and ice (all
! water at 0 c; all ice at -itype c).  Maximum transition range => 40 c
! 
! Author: J. Hack
! 
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use physconst,    only: tmelt
#ifdef WRF_PORT
   use module_cam_support, only: endrun, &
                                 iulog
#else
   use abortutils,   only: endrun
   use cam_logfile,  only: iulog
#endif
    
   implicit none
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   real(r8), intent(in) :: t          ! Temperature
!
! Output arguments
!
   integer, intent(inout) :: itype   ! Flag for ice phase and associated transition

   real(r8), intent(out) :: es         ! Saturation vapor pressure
!
!---------------------------Local variables-----------------------------
!
   real(r8) e1         ! Intermediate scratch variable for es over water
   real(r8) e2         ! Intermediate scratch variable for es over water
   real(r8) eswtr      ! Saturation vapor pressure over water
   real(r8) f          ! Intermediate scratch variable for es over water
   real(r8) f1         ! Intermediate scratch variable for es over water
   real(r8) f2         ! Intermediate scratch variable for es over water
   real(r8) f3         ! Intermediate scratch variable for es over water
   real(r8) f4         ! Intermediate scratch variable for es over water
   real(r8) f5         ! Intermediate scratch variable for es over water
   real(r8) ps         ! Reference pressure (mb)
   real(r8) t0         ! Reference temperature (freezing point of water)
   real(r8) term1      ! Intermediate scratch variable for es over ice
   real(r8) term2      ! Intermediate scratch variable for es over ice
   real(r8) term3      ! Intermediate scratch variable for es over ice
   real(r8) tr         ! Transition range for es over water to es over ice
   real(r8) ts         ! Reference temperature (boiling point of water)
   real(r8) weight     ! Intermediate scratch variable for es transition
   integer itypo   ! Intermediate scratch variable for holding itype
!
!-----------------------------------------------------------------------
!
! Check on whether there is to be a transition region for es
!
   if (itype < 0) then
      tr    = abs(real(itype,r8))
      itypo = itype
      itype = 1
   else
      tr    = 0.0_r8
      itypo = itype
   end if
   if (tr > 40.0_r8) then
      write(iulog,900) tr
#ifdef WRF_PORT
      call wrf_message(iulog)
#endif
      call endrun ('GFFGCH')                ! Abnormal termination
   end if
!
   if(t < (tmelt - tr) .and. itype == 1) go to 10
!
! Water
!
   ps = 1013.246_r8
   ts = 373.16_r8
   e1 = 11.344_r8*(1.0_r8 - t/ts)
   e2 = -3.49149_r8*(ts/t - 1.0_r8)
   f1 = -7.90298_r8*(ts/t - 1.0_r8)
   f2 = 5.02808_r8*log10(ts/t)
   f3 = -1.3816_r8*(10.0_r8**e1 - 1.0_r8)/10000000.0_r8
   f4 = 8.1328_r8*(10.0_r8**e2 - 1.0_r8)/1000.0_r8
   f5 = log10(ps)
   f  = f1 + f2 + f3 + f4 + f5
   es = (10.0_r8**f)*100.0_r8
   eswtr = es
!
   if(t >= tmelt .or. itype == 0) go to 20
!
! Ice
!
10 continue
   t0    = tmelt
   term1 = 2.01889049_r8/(t0/t)
   term2 = 3.56654_r8*log(t0/t)
   term3 = 20.947031_r8*(t0/t)
   es    = 575.185606e10_r8*exp(-(term1 + term2 + term3))
!
   if (t < (tmelt - tr)) go to 20
!
! Weighted transition between water and ice
!
   weight = min((tmelt - t)/tr,1.0_r8)
   es = weight*es + (1.0_r8 - weight)*eswtr
!
20 continue
   itype = itypo
   return
!
900 format('GFFGCH: FATAL ERROR ******************************',/, &
           'TRANSITION RANGE FOR WATER TO ICE SATURATION VAPOR', &
           ' PRESSURE, TR, EXCEEDS MAXIMUM ALLOWABLE VALUE OF', &
           ' 40.0 DEGREES C',/, ' TR = ',f7.2)
!
end subroutine gffgch
#ifdef WRF_PORT
end module module_cam_gffgch
#endif
