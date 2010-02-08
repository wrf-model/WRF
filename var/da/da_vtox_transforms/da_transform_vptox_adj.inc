subroutine da_transform_vptox_adj(grid, vp, be, ep)

   !--------------------------------------------------------------------------
   ! Purpose: Adjoint for Physical transform of variables 
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !--------------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout)        :: grid
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbation.
   type (be_type), intent(in), optional :: be  ! Background errors.

   integer :: i, k, j, ij, k1              ! Loop counters.

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
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      do ij = 1 , grid%num_tiles
         do k = kts,kte
            do j = grid%j_start(ij),grid%j_end(ij)
               do i =  its, ite
                  vp%v4(i,j,k)  = vp%v4(i,j,k) + grid%xa%qt(i,j,k) * grid%xb%qs(i,j,k)
               end do
            end do
         end do
      end do
      !$OMP END PARALLEL DO
   else 
      ! Pseudo RH --> Water vapor mixing ratio:
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      do ij = 1 , grid%num_tiles
         do k = kts,kte
            do j = grid%j_start(ij),grid%j_end(ij)
               do i =  its, ite
                  vp%v4(i,j,k)  = vp%v4(i,j,k) + grid%xa%q(i,j,k) * grid%xb%qs(i,j,k)   
               end do
            end do
         end do
      end do
      !$OMP END PARALLEL DO
   end if

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
#include "HALO_PSICHI_UV_ADJ.inc"
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


   ! Transform psi and chi to u and v:

   call da_psichi_to_uv_adj(grid%xa % u, grid%xa % v, grid%xb % coefx, grid%xb % coefy, vp % v1, vp % v2)

   !--------------------------------------------------------------------------
   ! [2] Impose statistical balance constraints:
   !--------------------------------------------------------------------------

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, k, j, k1, i )
   do ij = 1 , grid%num_tiles

   ! Surface Pressure
   do k= kts,kte
      do j= grid%j_start(ij),grid%j_end(ij)
         do i =  its, ite
            vp%v1(i,j,k) = vp%v1(i,j,k) + be%reg_ps(j,k)*grid%xa%psfc(i,j)
         end do
      end do
   end do
   do j= grid%j_start(ij),grid%j_end(ij)
      do i =  its, ite
         vp%v5(i,j,1) = grid%xa%psfc(i,j) 
      end do
   end do

   ! Temperature
   do k1 = kts,kte
      do k = kts,kte
         do j = grid%j_start(ij),grid%j_end(ij)
            do i =  its, ite
               vp%v1(i,j,k1) = vp%v1(i,j,k1) + be%reg_t(j,k,k1)*grid%xa%t(i,j,k)
            end do
         end do
      end do
   end do
   do k = kts,kte
      do j = grid%j_start(ij),grid%j_end(ij)
         do i =  its, ite
            vp%v3(i,j,k) = grid%xa%t(i,j,k)
         end do
      end do
   end do

   ! Chi
   do k = kts,kte
      do j = grid%j_start(ij),grid%j_end(ij)
         do i =  its, ite
            vp%v1(i,j,k) = vp%v1(i,j,k) + be%reg_chi(j,k)*vp%v2(i,j,k)
         end do
       end do
   end do

   enddo
   !$OMP END PARALLEL DO

   !---------------------------------------------------------------------------
   !  [1] Add flow-dependent increments in control variable space (vp):
   !---------------------------------------------------------------------------
   
   if (be % ne > 0 .and. alphacv_method == alphacv_method_vp) then
      call da_add_flow_dependence_vp_adj(be % ne, ep, vp)
   end if

   if (trace_use) call da_trace_exit("da_transform_vptox_adj")

end subroutine da_transform_vptox_adj


