#define WRF_PORT
#define MODAL_AERO
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov

!------------------------------------------------------------------------
! Based on esinti.F90 from CAM
! Ported to WRF by William.Gustafson@pnl.gov, Nov. 2009
! Updated to CESM_1_0_1, Nov. 2010
!------------------------------------------------------------------------

#ifdef WRF_PORT
module module_cam_esinti

  implicit none

  private
  public esinti

contains
#endif

subroutine esinti(epslon  ,latvap  ,latice  ,rh2o    ,cpair   ,tmelt   )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Initialize es lookup tables
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: J. Hack
! 
!-----------------------------------------------------------------------
   use shr_kind_mod, only: r8 => shr_kind_r8
   use wv_saturation, only: gestbl
   implicit none
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   real(r8), intent(in) :: epslon          ! Ratio of h2o to dry air molecular weights
   real(r8), intent(in) :: latvap          ! Latent heat of vaporization
   real(r8), intent(in) :: latice          ! Latent heat of fusion
   real(r8), intent(in) :: rh2o            ! Gas constant for water vapor
   real(r8), intent(in) :: cpair           ! Specific heat of dry air
   real(r8), intent(in) :: tmelt           ! Melting point of water (K)
!
!---------------------------Local workspace-----------------------------
!
   real(r8) tmn             ! Minimum temperature entry in table
   real(r8) tmx             ! Maximum temperature entry in table
   real(r8) trice           ! Trans range from es over h2o to es over ice
   logical ip           ! Ice phase (true or false)
!
!-----------------------------------------------------------------------
!
! Specify control parameters first
!
#if ( defined WACCM_PHYS)
   tmn   = 127.16_r8
#else
   tmn   = 173.16_r8
#endif
   tmx   = 375.16_r8
   trice =  20.00_r8
   ip    = .true.
!
! Call gestbl to build saturation vapor pressure table.
!
   call gestbl(tmn     ,tmx     ,trice   ,ip      ,epslon  , &
               latvap  ,latice  ,rh2o    ,cpair   ,tmelt )
!
   return
end subroutine esinti
#ifdef WRF_PORT
end module module_cam_esinti
#endif


