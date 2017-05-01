subroutine da_transform_vptox_inv(grid, vp, be, ep)

   !-----------------------------------------------------------------------
   ! Purpose: Inverse of balance (physical) transform of increment
   ! 
   ! Author: Zhiquan (Jake) Liu,  NCAR/MMM,  2015-9
   !-----------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)         :: grid
   
   type (vp_type), intent(inout)        :: vp  ! input: full variables
                                        ! output: unbalanced variables on model grid
   type (be_type), intent(in), optional :: be  ! Background errors.
   type (ep_type), intent(in), optional :: ep  ! Ensemble perturbations.

   integer :: i, k, j, k1, ij            ! Loop counters.
   real, allocatable          :: chi_u(:,:,:)  ! Unbalanced chi

   if (trace_use) call da_trace_entry("da_transform_vptox_inv") 

   !---------------------------------------------------------------------------
   !  [1] Add flow-dependent increments in control variable space (vp):
   !---------------------------------------------------------------------------

   if (be % ne > 0 .and. alphacv_method == alphacv_method_vp) then
      call da_add_flow_dependence_vp(be % ne, ep, vp, its,ite, jts,jte, kts,kte)
   end if

   !--------------------------------------------------------------------------
   ! [2] Impose statistical balance constraints:
   ! Assume input vp%* is full variable, out vp% is unbalanced variables
   ! to avoid (Psi,Chi) -> (U,V) transform, which has no exact inverse,
   ! we need to store full variables at vp%* after each outloop.
   ! da_transform_vptox.inc is also modified for this purpose.
   !
   ! for cv7, control variables are all full variables w/o multi-variate correlation.
   ! so there is no need for balance transform and its inverse.
   !--------------------------------------------------------------------------

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, k1, k, j, i)
   do ij = 1 , grid%num_tiles

   ! 2.1 Psi, Chi --> Psi, Chi_u
   !-------------------------
   ! there is no need for Psi --> Psi transform

   ! Chi --> Chi_u
   !--------------------
   if (cv_options /= 7) then
      do k = kts, kte
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its, ite
               vp%v2(i,j,k) = vp%v2(i,j,k) - be%reg_psi_chi(j,k)* vp%v1(i,j,k)
            end do
         end do
      end do
   end if
  
   ! 2.2 T --> T_u
   !-------------------
   if (cv_options /= 7) then ! - balance contri. from psi
      do k1 = kts, kte
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  !vp%v3(i,j,k) = grid%xa%t(i,j,k) - be%reg_psi_t(j,k,k1)*vp%v1(i,j,k1)
                  vp%v3(i,j,k) = vp%v3(i,j,k) - be%reg_psi_t(j,k,k1)*vp%v1(i,j,k1)
               end do
            end do
         end do
      end do
   end if

   if ( cv_options == 6 ) then ! - balance contri. from Chi_u 
      do k1 = kts, kte
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  vp%v3(i,j,k) = vp%v3(i,j,k) - be%reg_chi_u_t(j,k,k1)*vp%v2(i,j,k1)
               end do
            end do
         end do
      end do
   end if

   ! 2.3 Ps --> Ps_u
   !------------------------
   !do j = grid%j_start(ij), grid%j_end(ij)
   !   do i = its, ite
   !      grid%xa%psfc(i,j) = vp%v5(i,j,1) 
   !   end do
   !end do

   if (cv_options /= 7) then ! - balance contri. from psi
      do k = kts,kte
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its, ite
               !vp%v5(i,j,1) = grid%xa%psfc(i,j) - be%reg_psi_ps(j,k)*vp%v1(i,j,k)
               vp%v5(i,j,1) = vp%v5(i,j,1) - be%reg_psi_ps(j,k)*vp%v1(i,j,k)
            end do
         end do
      end do
   end if

   if ( cv_options == 6 ) then ! - balance contri. from Chi_u
      do k = kts,kte
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its, ite
               vp%v5(i,j,1) = vp%v5(i,j,1) - be%reg_chi_u_ps(j,k)*vp%v2(i,j,k)
            end do
         end do
      end do
   end if

   ! 2.4 q --> pseudo rh=q/qs(background)
   !----------------------------
   ! if cv5 or cv7, no need for pseudo rh transform

     !do k = kts, kte
     !   do j = grid%j_start(ij), grid%j_end(ij)
     !      do i = its, ite
     !          vp%v4(i,j,k) =  grid%xa % q(i,j,k) / grid%xb%qs(i,j,k)
     !      enddo
     !   enddo
     !enddo

   if ( cv_options == 6 ) then
      do k1 = kts, kte
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  vp%v4(i,j,k1) = vp%v4(i,j,k1) -  &
                                  be%reg_psi_rh(j,k1,k)*vp%v1(i,j,k) - &
                                be%reg_chi_u_rh(j,k1,k)*vp%v2(i,j,k) - &
                                  be%reg_t_u_rh(j,k1,k)*vp%v3(i,j,k)
               end do
            end do
         end do
      end do
!
      do k = kts, kte
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = its, ite
               vp%v4(i,j,k) = vp%v4(i,j,k) - be%reg_ps_u_rh(j,k)*vp%v5(i,j,1)
            end do
         end do
      end do
   end if

   end do
   !$OMP END PARALLEL DO
   !--------------------------------------------------------------------------
   ! [3] Transform to model variable space:
   !--------------------------------------------------------------------------
  
!!#ifdef A2C
!  if ((fg_format==fg_format_wrf_arw_regional  .or. &
!       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
!     ipe = ipe + 1
!     ide = ide + 1
!  end if
!
!  if ((fg_format==fg_format_wrf_arw_regional  .or. &
!       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
!     jpe = jpe + 1
!     jde = jde + 1
!  end if
!!#endif

!!#ifdef DM_PARALLEL
!!#include "HALO_PSICHI_UV.inc"
!!#endif

!!#ifdef A2C
!!  if ((fg_format==fg_format_wrf_arw_regional  .or. &
!       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
!     ipe = ipe - 1
!     ide = ide - 1
!  end if

!  if ((fg_format==fg_format_wrf_arw_regional  .or. &
!       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
!     jpe = jpe - 1
!     jde = jde - 1
!  end if
!#endif

   ! Psi and chi to u and v:
!   if ( cv_options == 5 .or. cv_options == 6 ) then
!      call da_psichi_to_uv(vp % v1, vp % v2, grid%xb % coefx, &
!           grid%xb % coefy , grid%xa % u, grid%xa % v)
!   else if ( cv_options == 7 ) then
!      grid%xa%u = vp%v1
!      grid%xa%v = vp%v2
!   end if

  if ( (use_radarobs .and. use_radar_rf) .or. (use_rad .and. crtm_cloud).or. &
       (use_radarobs .and. use_radar_rhv) .or. (use_radarobs .and. use_radar_rqv) .or. cloud_cv_options .ge. 2 .or. & 
       (grid%pseudo_var(1:1).eq.'q' .and. grid%pseudo_var(2:2).ne.' ') .or.  &
       (grid%pseudo_var(1:1).eq.'Q' .and. grid%pseudo_var(2:2).ne.' ') ) then

!     if ( cloud_cv_options == 1 .and. use_3dvar_phy) then
!      ! Pseudo RH --> Total water mixing ratio:
!      !$OMP PARALLEL DO &
!      !$OMP PRIVATE ( ij, i, j, k )
!      do ij = 1 , grid%num_tiles
!        do k = kts, kte
!            do j = grid%j_start(ij), grid%j_end(ij)
!               do i = its, ite
!                 grid%xa % qt(i,j,k) = vp%v4(i,j,k) * grid%xb%qs(i,j,k)
!               enddo
!            enddo
!         enddo
!      enddo
!      !$OMP END PARALLEL DO
!     end if
!     if ( cloud_cv_options .ge. 2 ) then 
!      ! Pseudo RH --> Water vapor mixing ratio:
!      !$OMP PARALLEL DO &
!      !$OMP PRIVATE ( ij, i, j, k )
!      do ij = 1 , grid%num_tiles
!         do k = kts, kte
!            do j = grid%j_start(ij), grid%j_end(ij)
!               do i = its, ite
!                  grid%xa % q(i,j,k) =  vp%v4(i,j,k) * grid%xb%qs(i,j,k)
!               enddo
!            enddo
!         enddo
!      enddo
!      !$OMP END PARALLEL DO
#ifdef CLOUD_CV
      !qcloud
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij, i, j, k )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  vp%v6(i,j,k) = grid%xa % qcw(i,j,k)
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
                  vp%v7(i,j,k) = grid%xa % qrn(i,j,k)
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
                  vp%v8(i,j,k) = grid%xa % qci(i,j,k)
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
                  vp%v9(i,j,k) = grid%xa % qsn(i,j,k)
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
                  vp%v10(i,j,k) = grid%xa % qgr(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
      !vertical velocity
      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( ij, i, j, k )
      do ij = 1 , grid%num_tiles
         do k = kts, kte
            do j = grid%j_start(ij), grid%j_end(ij)
               do i = its, ite
                  vp%v11(i,j,k) = grid%xa % w(i,j,k)
               enddo
            enddo
         enddo
      enddo
      !$OMP END PARALLEL DO
#endif
!     end if
  !else  ! no rf or cloud radiance
  !    ! Pseudo RH --> Water vapor mixing ratio:
  !    !$OMP PARALLEL DO &
  !    !$OMP PRIVATE ( ij, i, j, k )
  !    do ij = 1 , grid%num_tiles
  !       do k = kts, kte
  !          do j = grid%j_start(ij), grid%j_end(ij)
  !             do i = its, ite
  !                grid%xa % q(i,j,k) =  vp%v4(i,j,k) * grid%xb%qs(i,j,k)
  !             enddo
  !          enddo
  !       enddo
  !    enddo
  !    !$OMP END PARALLEL DO
  end if ! RF or Radiance
   !---------------------------------------------------------------------------
   !  [4] Add flow-dependent increments in model space (grid%xa):
   !---------------------------------------------------------------------------

!  if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
!     call da_add_flow_dependence_xa(grid, be % ne, ep, vp)
!  end if
!   if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
!      if ( anal_type_hybrid_dual_res ) then
!         call da_add_flow_dependence_xa_dual_res(grid, be % ne, ep, vp)
!      else
!         call da_add_flow_dependence_xa(grid, be % ne, ep, vp)
!      endif
!   end if

   if (trace_use) call da_trace_exit("da_transform_vptox_inv") 
 
end subroutine da_transform_vptox_inv

