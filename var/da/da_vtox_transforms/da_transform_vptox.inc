subroutine da_transform_vptox(grid, vp, be, ep)
!subroutine da_transform_vptox(grid, vp, be, ep, nobwin)

   !-----------------------------------------------------------------------
   ! Purpose: Physical transform of analysis increment variables.
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   ! Updates:
   !
   !       Implementation of multi-variate BE for cv_options=6
   !       Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 02/01/2010
   !-----------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)         :: grid
   
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   type (be_type), intent(in), optional :: be  ! Background errors.
   type (ep_type), intent(in), optional :: ep  ! Ensemble perturbations.
!   integer, intent(in), optional         :: nobwin

   integer :: i, k, j, k1, ij            ! Loop counters.
   real, allocatable          :: chi_u(:,:,:)  ! Unbalanced chi

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

   if ( cv_options == 6 ) then
      allocate (chi_u(its:ite,grid%j_start(ij):grid%j_end(ij),kts:kte) )
      do k = kts, kte
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its, ite
               chi_u(i,j,k) = vp%v2(i,j,k)
            end do
         end do
      end do
   end if

   ! Chi:
   if (cv_options /= 7) then
      do k = kts, kte
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its, ite
               vp%v2(i,j,k) = vp%v2(i,j,k) + be%reg_psi_chi(j,k)* vp%v1(i,j,k)
            end do
         end do
      end do
   end if
  
   ! Temperature:
   do k = kts, kte
      do j = grid%j_start(ij), grid%j_end(ij)
         do i = its, ite
            grid%xa%t(i,j,k) = vp%v3(i,j,k)
         end do
      end do
   end do

   if (cv_options /= 7) then
      do k1 = kts, kte
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  grid%xa%t(i,j,k) = grid%xa%t(i,j,k) + be%reg_psi_t(j,k,k1)*vp%v1(i,j,k1)
               end do
            end do
         end do
      end do
   end if

   if ( cv_options == 6 ) then
      do k1 = kts, kte
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  grid%xa%t(i,j,k) = grid%xa%t(i,j,k) + be%reg_chi_u_t(j,k,k1)*chi_u(i,j,k1)
               end do
            end do
         end do
      end do
   end if

   ! Surface Pressure
   do j = grid%j_start(ij), grid%j_end(ij)
      do i = its, ite
         grid%xa%psfc(i,j) = vp%v5(i,j,1) 
      end do
   end do

   if (cv_options /= 7) then
      do k = kts,kte
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its, ite
               grid%xa%psfc(i,j) = grid%xa%psfc(i,j) + be%reg_psi_ps(j,k)*vp%v1(i,j,k)
            end do
         end do
      end do
   end if

   if ( cv_options == 6 ) then
      do k = kts,kte
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its, ite
               grid%xa%psfc(i,j) = grid%xa%psfc(i,j) + be%reg_chi_u_ps(j,k)*chi_u(i,j,k)
            end do
         end do
      end do
   end if

   ! Moisture
   if ( cv_options == 6 ) then
      do k1 = kts, kte
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  vp%v4(i,j,k1) = vp%v4(i,j,k1) + be%reg_psi_rh(j,k1,k)*vp%v1(i,j,k) + &
                  be%reg_chi_u_rh(j,k1,k)*chi_u(i,j,k) + be%reg_t_u_rh(j,k1,k)*vp%v3(i,j,k)
               end do
            end do
         end do
      end do
!
      do k = kts, kte
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its, ite
               vp%v4(i,j,k) = vp%v4(i,j,k) + be%reg_ps_u_rh(j,k)*vp%v5(i,j,1)
            end do
         end do
      end do
   end if

!
   if ( cv_options == 6 ) deallocate (chi_u )

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
   if ( cv_options == 5 .or. cv_options == 6 ) then
      call da_psichi_to_uv(vp % v1, vp % v2, grid%xb % coefx, &
           grid%xb % coefy , grid%xa % u, grid%xa % v)
   else if ( cv_options == 7 ) then
      grid%xa%u = vp%v1
      grid%xa%v = vp%v2
   end if

   if ( cloud_cv_options /= 1 ) then
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
   else ! cloud_cv_options=1
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
   end if

   if ( cloud_cv_options >= 2 ) then
      !qcloud
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij, i, j, k )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  grid%xa % qcw(i,j,k) =  vp%v6(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
      !qrain
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij, i, j, k )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  grid%xa % qrn(i,j,k) =  vp%v7(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
      !qice
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij, i, j, k )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  grid%xa % qci(i,j,k) =  vp%v8(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
      !qsnow
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij, i, j, k )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  grid%xa % qsn(i,j,k) =  vp%v9(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
      !qgraupel
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij, i, j, k )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  grid%xa % qgr(i,j,k) =  vp%v10(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
   end if ! cloud_cv_options>=2

   if ( use_cv_w ) then
      !vertical velocity
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij, i, j, k )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  grid%xa % w(i,j,k) =  vp%v11(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
   end if ! use_cv_w

!   !---------------------------------------------------------------------------
!   !  [4] Add flow-dependent increments in model space (grid%xa):
!   !---------------------------------------------------------------------------
!
!!  if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
!!     call da_add_flow_dependence_xa(grid, be % ne, ep, vp)
!!  end if
!   if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
!      if ( anal_type_hybrid_dual_res ) then
!         if( present(nobwin) ) then
!            call da_add_flow_dependence_xa_dual_res(grid, be % ne, ep, vp, nobwin)
!         else
!            call da_add_flow_dependence_xa_dual_res(grid, be % ne, ep, vp)
!         end if
!      else
!         if( present(nobwin) ) then
!            call da_add_flow_dependence_xa(grid, be % ne, ep, vp, nobwin)
!         else
!            call da_add_flow_dependence_xa(grid, be % ne, ep, vp)
!         end if
!      endif
!   end if

   if (trace_use) call da_trace_exit("da_transform_vptox") 
 
end subroutine da_transform_vptox

