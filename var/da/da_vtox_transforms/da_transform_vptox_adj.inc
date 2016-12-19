subroutine da_transform_vptox_adj(grid, vp, be, ep)
!subroutine da_transform_vptox_adj(grid, vp, be, ep, nobwin)

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
!   integer, intent(in), optional        :: nobwin

   integer :: i, k, j, ij, k1              ! Loop counters.
   real, allocatable                    :: chi_u(:,:,:)  ! Unbalanced chi

   if (trace_use) call da_trace_entry("da_transform_vptox_adj")

!   !---------------------------------------------------------------------------
!   !  [4] Add flow-dependent increments in model space (grid%xa):
!   !---------------------------------------------------------------------------
!
!!  if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
!!     call da_add_flow_dependence_xa_adj(be % ne, ep, grid%xa, vp)
!!  end if
!   if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
!      if ( anal_type_hybrid_dual_res ) then
!         if( present(nobwin) ) then
!            call da_add_flow_dependence_xa_adj_dual_res(be % ne, ep, grid, vp, nobwin)
!         else
!            call da_add_flow_dependence_xa_adj_dual_res(be % ne, ep, grid, vp)
!         end if
!      else
!         if( present(nobwin) ) then
!            call da_add_flow_dependence_xa_adj(be % ne, ep, grid%xa, vp, nobwin)
!         else
!            call da_add_flow_dependence_xa_adj(be % ne, ep, grid%xa, vp)
!         end if
!      endif
!   endif

   !--------------------------------------------------------------------------
   ! [3] Transform to model variable space:
   !--------------------------------------------------------------------------

   if ( cloud_cv_options /= 1 ) then
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
   else ! cloud_cv_options=1
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
   end if

   if ( cloud_cv_options >= 2) then 
      !qcloud
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      do ij = 1 , grid%num_tiles
         do k = kts,kte
            do j = grid%j_start(ij),grid%j_end(ij)
               do i =  its, ite
                  vp%v6(i,j,k)  = vp%v6(i,j,k) + grid%xa%qcw(i,j,k)
               end do
            end do
         end do
      end do
      !$OMP END PARALLEL DO
      !qrain
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      do ij = 1 , grid%num_tiles
         do k = kts,kte
            do j = grid%j_start(ij),grid%j_end(ij)
               do i =  its, ite
                  vp%v7(i,j,k)  = vp%v7(i,j,k) + grid%xa%qrn(i,j,k)
               end do
            end do
         end do
      end do
      !$OMP END PARALLEL DO
      !qice
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  vp%v8(i,j,k)  = vp%v8(i,j,k) + grid%xa%qci(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
      !qsnow
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  vp%v9(i,j,k)  = vp%v9(i,j,k) + grid%xa%qsn(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO

      !qgraupel
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  vp%v10(i,j,k)  = vp%v10(i,j,k) + grid%xa%qgr(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
   end if ! cloud_cv_options >=2

   if ( use_cv_w ) then
      !vertical velocity
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  vp%v11(i,j,k)  = vp%v11(i,j,k) + grid%xa%w(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
   end if ! use_cv_w

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
   if ( cv_options == 5 .or. cv_options == 6 ) then
      call da_psichi_to_uv_adj(grid%xa % u, grid%xa % v, grid%xb % coefx, grid%xb % coefy, vp % v1, vp % v2)
   else if ( cv_options == 7) then
      vp%v1 = grid%xa%u + vp%v1
      vp%v2 = grid%xa%v + vp%v2
   end if

   !--------------------------------------------------------------------------
   ! [2] Impose statistical balance constraints:
   !--------------------------------------------------------------------------

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, k, j, k1, i )
   do ij = 1 , grid%num_tiles

   if ( cv_options == 6 ) then
      allocate (chi_u(its:ite,grid%j_start(ij):grid%j_end(ij),kts:kte) )
      chi_u = 0
   end if

   ! Moisture
!
   if ( cv_options == 6 ) then
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
   end if

   ! Surface Pressure
   if ( cv_options == 6 ) then
      do k= kts,kte
         do j= grid%j_start(ij),grid%j_end(ij)
            do i =  its, ite
               chi_u(i,j,k) = chi_u(i,j,k) + be%reg_chi_u_ps(j,k)*grid%xa%psfc(i,j)
            end do
         end do
      end do
   end if
   if (cv_options /= 7) then
      do k= kts,kte
         do j= grid%j_start(ij),grid%j_end(ij)
            do i =  its, ite
               vp%v1(i,j,k) = vp%v1(i,j,k) + be%reg_psi_ps(j,k)*grid%xa%psfc(i,j)
            end do
         end do
      end do
   end if
   do j= grid%j_start(ij),grid%j_end(ij)
      do i =  its, ite
         vp%v5(i,j,1) = vp%v5(i,j,1) + grid%xa%psfc(i,j) 
      end do
   end do

   ! Temperature
   if ( cv_options == 6 ) then
      do k1 = kts,kte
         do k = kts,kte
            do j = grid%j_start(ij),grid%j_end(ij)
               do i =  its, ite
                  chi_u(i,j,k1) = chi_u(i,j,k1) + be%reg_chi_u_t(j,k,k1)*grid%xa%t(i,j,k)
               end do
            end do
         end do
      end do
   end if
   if (cv_options /= 7) then
      do k1 = kts,kte
         do k = kts,kte
            do j = grid%j_start(ij),grid%j_end(ij)
               do i =  its, ite
                  vp%v1(i,j,k1) = vp%v1(i,j,k1) + be%reg_psi_t(j,k,k1)*grid%xa%t(i,j,k)
               end do
            end do
         end do
      end do
   end if
   do k = kts,kte
      do j = grid%j_start(ij),grid%j_end(ij)
         do i =  its, ite
            vp%v3(i,j,k) = vp%v3(i,j,k) + grid%xa%t(i,j,k)
         end do
      end do
   end do

   ! Chi
   if (cv_options /= 7) then
      do k = kts,kte
         do j = grid%j_start(ij),grid%j_end(ij)
            do i =  its, ite
               vp%v1(i,j,k) = vp%v1(i,j,k) + be%reg_psi_chi(j,k)*vp%v2(i,j,k)
            end do
          end do
      end do
   end if
   if ( cv_options == 6 ) then
      do k = kts,kte
         do j = grid%j_start(ij),grid%j_end(ij)
            do i =  its, ite
               vp%v2(i,j,k) = vp%v2(i,j,k) + chi_u(i,j,k)
            end do
          end do
      end do
      deallocate (chi_u)
   end if

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


