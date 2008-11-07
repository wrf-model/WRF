subroutine da_transform_vptox_adj(grid, vp, be, ep)

   !--------------------------------------------------------------------------
   ! Purpose: Adjoint for Physical transform of variables 
   !--------------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout)        :: grid
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbation.
   type (be_type), intent(in), optional :: be  ! Background errors.

   integer :: k, j, k1              ! Loop counters.

   if (trace_use) call da_trace_entry("da_transform_vptox_adj")

   !---------------------------------------------------------------------------
   !  [4] Add flow-dependent increments in model space (grid%xa):
   !---------------------------------------------------------------------------
      
   if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
      call da_add_flow_dependence_xa_adj(be % ne, ep, grid%xa, vp)
   end if

   !--------------------------------------------------------------------------
   ! [3] Transform to model variable space:
   !--------------------------------------------------------------------------

   if ((use_radarobs .and. use_radar_rf) .or. (use_rad .and. crtm_cloud) ) then
      ! Pseudo RH --> Total water mixing ratio:
      vp%v4(its:ite,jts:jte,kts:kte)  = vp%v4(its:ite,jts:jte,kts:kte) &
         + grid%xa%qt(its:ite,jts:jte,kts:kte) * grid%xb%qs(its:ite,jts:jte,kts:kte)
   else 
      ! Pseudo RH --> Water vapor mixing ratio:
      vp%v4(its:ite,jts:jte,kts:kte)  = vp%v4(its:ite,jts:jte,kts:kte) &
         + grid%xa%q (its:ite,jts:jte,kts:kte) * grid%xb%qs(its:ite,jts:jte,kts:kte)
   end if

#ifdef DM_PARALLEL
#include "HALO_PSICHI_UV_ADJ.inc"
#endif

   ! Transform psi and chi to u and v:
   call da_psichi_to_uv_adj(grid%xa % u, grid%xa % v, grid%xb % coefx, grid%xb % coefy, vp % v1, vp % v2)

   !--------------------------------------------------------------------------
   ! [2] Impose statistical balance constraints:
   !--------------------------------------------------------------------------

   ! Surface Pressure
   do k=kts,kte
      do j=jts,jte
         vp%v1(its:ite,j,k) = vp%v1(its:ite,j,k) + be%reg_ps(j,k)*grid%xa%psfc(its:ite,j)
      end do
   end do
   vp%v5(its:ite,jts:jte,1) = grid%xa%psfc(its:ite,jts:jte) 

   ! Temperature
   do k1 = kts,kte
      do k = kts,kte
         do j = jts,jte
            vp%v1(its:ite,j,k1) = vp%v1(its:ite,j,k1) + be%reg_t(j,k,k1)*grid%xa%t(its:ite,j,k)
         end do
      end do
   end do
   vp%v3(its:ite,jts:jte,kts:kte) = grid%xa%t(its:ite,jts:jte,kts:kte)

   ! Chi
   do k = kts,kte
      do j = jts,jte
         vp%v1(its:ite,j,k) = vp%v1(its:ite,j,k) + be%reg_chi(j,k)*vp%v2(its:ite,j,k)
       end do
   end do

   !---------------------------------------------------------------------------
   !  [1] Add flow-dependent increments in control variable space (vp):
   !---------------------------------------------------------------------------
   
   if (be % ne > 0 .and. alphacv_method == alphacv_method_vp) then
      call da_add_flow_dependence_vp_adj(be % ne, ep, vp)
   end if

   if (trace_use) call da_trace_exit("da_transform_vptox_adj")

end subroutine da_transform_vptox_adj


