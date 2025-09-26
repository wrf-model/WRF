subroutine da_transform_xtopsfc_adj(grid, iv, obs_index, synop, j_synop_y, jo_grad_x) 

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),              intent(in)    :: grid
   type (iv_type),             intent(in)    :: iv
   integer,                    intent(in)    :: obs_index
   type (synop_type),          intent(in)    :: synop(:)
   type (residual_synop_type), intent(inout) :: j_synop_y(:)   ! grad_y(jo)
   type (x_type),              intent(inout) :: jo_grad_x   ! grad_x(jo)
   

   integer :: n

   real :: to, qo
   real, allocatable :: hsm(:,:)
   real, allocatable :: tsm(:,:)
   real, allocatable :: qsm(:,:)
   real, allocatable :: psm(:,:)
   real, allocatable :: psm_prime(:,:)
   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: t(:,:)
   real, allocatable :: q(:,:)

   if (trace_use) call da_trace_entry("da_transform_xtopsfc_adj")

   allocate (hsm(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (tsm(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (qsm(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (psm(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (psm_prime(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (u(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (v(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (t(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (q(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))

   psm_prime = 0.0

   ! 2.1 Terrain height at the observed site (xj, yi):

   call da_interp_lin_2d(grid%xb%terr, iv%info(obs_index), 1, hsm)
   call da_interp_lin_2d(grid%xb%t2,   iv%info(obs_index), 1, tsm)
   call da_interp_lin_2d(grid%xb%q2,   iv%info(obs_index), 1, qsm)
   call da_interp_lin_2d(grid%xb%psfc, iv%info(obs_index), 1, psm)

   do n=iv%info(obs_index)%n1,iv%info(obs_index)%n2
      if (synop(n)%p%qc >= 0) then
         !------------------------------------------------------------------------
         ! 2.0 Calculate gradient with respect the pressure at the observed height: 
         !------------------------------------------------------------------------

         to = -888888.0
         qo = -888888.0

         ! 2.3 Zero out the adjoint variables:

         !----------------------------------------------------------------
         ! 3.0 Surface pressure gradient at the observed height
         !----------------------------------------------------------------

         ! 4.0 Get the surface pressure gradient at the model surface height (hsm)
         ! 4.1 Both observed to and qo available:
         if (synop(n)%t%qc >= 0 .and. synop(n)%q%qc >= 0) then
            to = tsm(1,n) + synop(n)%t%inv
            qo = qsm(1,n) + synop(n)%q%inv
            call da_sfc_pre_adj (j_synop_y(n)%p, psm_prime(1,n), j_synop_y(n)%t, j_synop_y(n)%q, &
               psm(1,n), tsm(1,n), qsm(1,n), hsm(1,n), synop(n)%h, to, qo)

            ! 4.2 only observed to available:
         else if (synop(n)%t%qc >= 0 .and. synop(n)%q%qc < 0) then
            to = tsm(1,n) + synop(n)%t%inv
            call da_sfc_pre_adj (j_synop_y(n)%p, psm_prime(1,n), j_synop_y(n)%t, j_synop_y(n)%q, &
               psm(1,n), tsm(1,n), qsm(1,n), hsm(1,n), synop(n)%h, to)

            ! 4.3 Both observed to and qo missing:
         else
            call da_sfc_pre_adj (j_synop_y(n)%p, psm_prime(1,n), j_synop_y(n)%t, j_synop_y(n)%q, &
               psm(1,n), tsm(1,n), qsm(1,n), hsm(1,n), synop(n)%h)
         end if
      end if
      t(1,n)=j_synop_y(n)%t
      q(1,n)=j_synop_y(n)%q
      u(1,n)=j_synop_y(n)%u
      v(1,n)=j_synop_y(n)%v
   end do

   ! 2.2 convert the jo_grad_y to the model grids (t2, q2, u10, v10, and psfc)
   call da_interp_lin_2d_adj(jo_grad_x%t2,   iv%info(obs_index), 1, t)
   call da_interp_lin_2d_adj(jo_grad_x%q2,   iv%info(obs_index), 1, q)
   call da_interp_lin_2d_adj(jo_grad_x%u10,  iv%info(obs_index), 1, u)
   call da_interp_lin_2d_adj(jo_grad_x%v10,  iv%info(obs_index), 1, v)
   call da_interp_lin_2d_adj(jo_grad_x%psfc, iv%info(obs_index), 1, psm_prime)

   deallocate (hsm)
   deallocate (tsm)
   deallocate (qsm)
   deallocate (psm)
   deallocate (psm_prime)
   deallocate (u)
   deallocate (v)
   deallocate (t)
   deallocate (q)

   if (trace_use) call da_trace_exit("da_transform_xtopsfc_adj")

end subroutine da_transform_xtopsfc_adj


