!
!*** Jan Mandel August-October 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
! With contributions by Minjeong Kim.
#define DEBUG_OUT
#define DEBUG_PRINT
!#define FUEL_LEFT
!#define DEBUG_OUT_FUEL_LEFT    

module module_fr_sfire_core

use module_fr_sfire_phys
use module_fr_sfire_util

! The mathematical core of the fire spread model. No physical constants here.
! 
! subroutine sfire_core: only this routine should be called from the outside.
! subroutine fuel_left:  compute remaining fuel from time of ignition.
! subroutine prop_ls: propagation of curve in normal direction.

contains

!
!****************************************
!
    
subroutine init_no_fire(&
    ifds,ifde,jfds,jfde, &
    ifms,ifme,jfms,jfme, &
    ifts,ifte,jfts,jfte, &
    fdx,fdy,time_now,    & ! scalars in
    fuel_frac,fire_area,lfn,tign)    ! arrays out            
implicit none
             
!*** purpose: initialize model to no fire

!*** arguments
integer, intent(in):: ifds,ifde,jfds,jfde   ! fire domain bounds
integer, intent(in):: ifts,ifte,jfts,jfte   ! fire tile bounds
integer, intent(in):: ifms,ifme,jfms,jfme   ! array bounds
real, intent(in) :: fdx,fdy,time_now        ! mesh spacing, time
real, intent(out), dimension (ifms:ifme,jfms:jfme) :: & 
                   fuel_frac,fire_area,lfn,tign       ! model state


end subroutine init_no_fire

end module module_fr_sfire_core
