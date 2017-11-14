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

   !if (be % ne > 0 .and. alphacv_method == alphacv_method_vp) then
   !   call da_add_flow_dependence_vp(be % ne, ep, vp, its,ite, jts,jte, kts,kte)
   !   call da_add_flow_dependence_vp_inv !!! ??
   !end if

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

