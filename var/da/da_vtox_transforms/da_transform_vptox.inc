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

   integer :: i, k, j, k1, ij            ! Loop counters.

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

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, k1, k, j, i)
   do ij = 1 , grid%num_tiles

   ! Chi:
   do k = kts, kte
      do j = grid%j_start(ij), grid%j_end(ij)
         do i = its, ite
            vp%v2(i,j,k) = vp%v2(i,j,k) + be%reg_chi(j,k)* vp%v1(i,j,k)
         end do
      end do
   end do
  
   ! Temperature:
   do k = kts, kte
      do j = grid%j_start(ij), grid%j_end(ij)
         do i = its, ite
            grid%xa%t(i,j,k) = vp%v3(i,j,k)
         end do
      end do
   end do


   do k1 = kts, kte
      do k = kts, kte
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its, ite
               grid%xa%t(i,j,k) = grid%xa%t(i,j,k) + be%reg_t(j,k,k1)*vp%v1(i,j,k1)
            end do
         end do
      end do
   end do

   ! Surface Pressure
   do j = grid%j_start(ij), grid%j_end(ij)
      do i = its, ite
         grid%xa%psfc(i,j) = vp%v5(i,j,1) 
      end do
   end do

   do k = kts,kte
      do j = grid%j_start(ij), grid%j_end(ij)
         do i = its, ite
            grid%xa%psfc(i,j) = grid%xa%psfc(i,j) + be%reg_ps(j,k)*vp%v1(i,j,k)
         end do
      end do
   end do

   end do
   !$OMP END PARALLEL DO
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
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij, i, j, k )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  grid%xa % qt(i,j,k) = vp%v4(i,j,k) * grid%xb%qs(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
   else
      ! Pseudo RH --> Water vapor mixing ratio:
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij, i, j, k )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  grid%xa % q(i,j,k) =  vp%v4(i,j,k) * grid%xb%qs(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
   end if

   !---------------------------------------------------------------------------
   !  [4] Add flow-dependent increments in model space (grid%xa):
   !---------------------------------------------------------------------------

   if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
      call da_add_flow_dependence_xa(grid, be % ne, ep, vp)
   end if

   if (trace_use) call da_trace_exit("da_transform_vptox") 
 
end subroutine da_transform_vptox

