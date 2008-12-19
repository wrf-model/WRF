subroutine da_transform_vptox(grid, vp, be, ep)

   !-----------------------------------------------------------------------
   ! Purpose: Physical transform of analysis increment variables.
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)         :: grid
   
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   type (be_type), intent(in), optional :: be  ! Background errors.
   type (ep_type), intent(in), optional :: ep  ! Ensemble perturbations.

   integer :: k, j, k1            ! Loop counters.

   if (trace_use) call da_trace_entry("da_transform_vptox") 

   !---------------------------------------------------------------------------
   !  [1] Add flow-dependent increments in control variable space (vp):
   !---------------------------------------------------------------------------

   if (be % ne > 0 .and. alphacv_method == alphacv_method_vp) then
      call da_add_flow_dependence_vp(be % ne, ep, vp, its,ite, jts,jte, kts,kte)
   end if

   !--------------------------------------------------------------------------
   ! [2] Impose statistical balance constraints:
   !--------------------------------------------------------------------------

   ! Chi:
   do k = kts, kte
      do j = jts, jte
         vp%v2(its:ite,j,k) = vp%v2(its:ite,j,k) + be%reg_chi(j,k)* vp%v1(its:ite,j,k)
      end do
   end do
  
   ! Temperature:
   grid%xa%t(its:ite,jts:jte,kts:kte) = vp%v3(its:ite,jts:jte,kts:kte)


   do k1 = kts, kte
      do k = kts, kte
         do j = jts, jte
            grid%xa%t(its:ite,j,k) = grid%xa%t(its:ite,j,k) + be%reg_t(j,k,k1)*vp%v1(its:ite,j,k1)
         end do
      end do
   end do

   ! Surface Pressure
   grid%xa%psfc(its:ite,jts:jte) = vp%v5(its:ite,jts:jte,1) 
   do k = kts,kte
      do j = jts,jte
         grid%xa%psfc(its:ite,j) = grid%xa%psfc(its:ite,j) + be%reg_ps(j,k)*vp%v1(its:ite,j,k)
      end do
   end do

   !--------------------------------------------------------------------------
   ! [3] Transform to model variable space:
   !--------------------------------------------------------------------------
  
#ifdef A2C
  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
     ipe = ipe + 1
     ide = ide + 1
  end if

  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
     jpe = jpe + 1
     jde = jde + 1
  end if
#endif

#ifdef DM_PARALLEL
#include "HALO_PSICHI_UV.inc"
#endif

#ifdef A2C
  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
     ipe = ipe - 1
     ide = ide - 1
  end if

  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
     jpe = jpe - 1
     jde = jde - 1
  end if
#endif

   ! Psi and chi to u and v:

   call da_psichi_to_uv(vp % v1, vp % v2, grid%xb % coefx, &
      grid%xb % coefy , grid%xa % u, grid%xa % v)

   if ((use_radarobs .and. use_radar_rf) .or. (use_rad .and. crtm_cloud)) then
      ! Pseudo RH --> Total water mixing ratio:
      grid%xa % qt(its:ite,jts:jte,kts:kte) = vp%v4(its:ite,jts:jte,kts:kte) * &
         grid%xb%qs(its:ite,jts:jte,kts:kte)
   else
      ! Pseudo RH --> Water vapor mixing ratio:
      grid%xa % q(its:ite,jts:jte,kts:kte) = vp%v4(its:ite,jts:jte,kts:kte) * &
         grid%xb%qs(its:ite,jts:jte,kts:kte)
   end if

   !---------------------------------------------------------------------------
   !  [4] Add flow-dependent increments in model space (grid%xa):
   !---------------------------------------------------------------------------

   if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
      call da_add_flow_dependence_xa(grid, be % ne, ep, vp)
   end if

   if (trace_use) call da_trace_exit("da_transform_vptox") 
 
end subroutine da_transform_vptox

