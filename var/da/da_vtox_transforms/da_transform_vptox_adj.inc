subroutine da_transform_vptox_adj(grid, vp, be, ep)

   !--------------------------------------------------------------------------
   ! Purpose: Adjoint for Physical transform of variables 
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !
   ! Updates:
   !
   !       Implementation of multi-variate BE for cv_options=6
   !       Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 02/01/2010
   !--------------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout)        :: grid
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbation.
   type (be_type), intent(in), optional :: be  ! Background errors.

   integer :: i, k, j, ij, k1              ! Loop counters.
   real, allocatable                    :: chi_u(:,:,:)  ! Unbalanced chi

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

  if ( (use_radarobs .and. use_radar_rf) .or. (use_rad .and. crtm_cloud) ) then
   if ( cloud_cv_options == 1 ) then
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
#ifdef CLOUD_CV
!qc
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      do ij = 1 , grid%num_tiles
         do k = kts,kte
            do j = grid%j_start(ij),grid%j_end(ij)
               do i =  its, ite
                 if(cloud_cv_options == 2)then
                  vp%v6(i,j,k)  = vp%v6(i,j,k) + grid%xa%qcw(i,j,k) * grid%xb%qs(i,j,k)
                 elseif(cloud_cv_options == 3)then
                  vp%v6(i,j,k)  = vp%v6(i,j,k) + grid%xa%qcw(i,j,k)
!                 elseif(mcv_options.eq.4)then
!                  vp%v6(i,j,k)  = 0.0
                 endif
               end do
            end do
         end do
      end do
      !$OMP END PARALLEL DO
!     vp%v6  = 0.0
!qr
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      do ij = 1 , grid%num_tiles
         do k = kts,kte
            do j = grid%j_start(ij),grid%j_end(ij)
               do i =  its, ite
                 if(cloud_cv_options == 2)then
                  vp%v7(i,j,k)  = vp%v7(i,j,k) + grid%xa%qrn(i,j,k) * grid%xb%qs(i,j,k)
                 elseif(cloud_cv_options == 3)then
                  vp%v7(i,j,k)  = vp%v7(i,j,k) + grid%xa%qrn(i,j,k)
!                 elseif(mcv_options.eq.4)then
!                  vp%v7(i,j,k)  = 0.0
                 endif
               end do
            end do
         end do
      end do
      !$OMP END PARALLEL DO
!ice
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                 if(cloud_cv_options == 2)then
                  vp%v8(i,j,k)  = vp%v8(i,j,k) + grid%xa%qci(i,j,k) * grid%xb%qs(i,j,k)
                 elseif(cloud_cv_options == 3)then
                  vp%v8(i,j,k)  = vp%v8(i,j,k) + grid%xa%qci(i,j,k)
!                elseif(mcv_options.eq.4)then
!                  vp%v8(i,j,k)  = 0.0
                 endif
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
!      vp%v8 = 0.0
#endif
   end if
  else ! no rf or cloud radiance
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
   allocate (chi_u(its:ite,grid%j_start(ij):grid%j_end(ij),kts:kte) )
   chi_u = 0

   ! Moisture
!
   do k = kts, kte
      do j = grid%j_start(ij), grid%j_end(ij)
         do i = its, ite
            vp%v5(i,j,1) = vp%v5(i,j,1) + be%reg_ps_u_rh(j,k) * vp%v4(i,j,k)
         end do
      end do
   end do

   do k1 = kts, kte
      do k = kts, kte
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its, ite
              vp%v3(i,j,k1) = vp%v3(i,j,k1) + be%reg_t_u_rh(j,k,k1)*vp%v4(i,j,k)
              vp%v1(i,j,k1) = vp%v1(i,j,k1) + be%reg_psi_rh(j,k,k1)*vp%v4(i,j,k)
              chi_u(i,j,k1) = chi_u(i,j,k1) + be%reg_chi_u_rh(j,k,k1)*vp%v4(i,j,k)
            end do
         end do
      end do
   end do

   ! Surface Pressure
   do k= kts,kte
      do j= grid%j_start(ij),grid%j_end(ij)
         do i =  its, ite
            vp%v1(i,j,k) = vp%v1(i,j,k) + be%reg_psi_ps(j,k)*grid%xa%psfc(i,j)
            chi_u(i,j,k) = chi_u(i,j,k) + be%reg_chi_u_ps(j,k)*grid%xa%psfc(i,j)
         end do
      end do
   end do
   do j= grid%j_start(ij),grid%j_end(ij)
      do i =  its, ite
         vp%v5(i,j,1) = vp%v5(i,j,1) + grid%xa%psfc(i,j) 
      end do
   end do

   ! Temperature
   do k1 = kts,kte
      do k = kts,kte
         do j = grid%j_start(ij),grid%j_end(ij)
            do i =  its, ite
               vp%v1(i,j,k1) = vp%v1(i,j,k1) + be%reg_psi_t(j,k,k1)*grid%xa%t(i,j,k)
               chi_u(i,j,k1) = chi_u(i,j,k1) + be%reg_chi_u_t(j,k,k1)*grid%xa%t(i,j,k)
            end do
         end do
      end do
   end do
   do k = kts,kte
      do j = grid%j_start(ij),grid%j_end(ij)
         do i =  its, ite
            vp%v3(i,j,k) = vp%v3(i,j,k) + grid%xa%t(i,j,k)
         end do
      end do
   end do

   ! Chi
   do k = kts,kte
      do j = grid%j_start(ij),grid%j_end(ij)
         do i =  its, ite
            vp%v1(i,j,k) = vp%v1(i,j,k) + be%reg_psi_chi(j,k)*vp%v2(i,j,k)
            vp%v2(i,j,k) = vp%v2(i,j,k) + chi_u(i,j,k)
         end do
       end do
   end do
   deallocate (chi_u)

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


