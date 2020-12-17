subroutine da_transform_xtopsfc(grid, iv, obs_index, synop, y_synop)

   !---------------------------------------------------------------------
   ! Purpose: TBD
   !---------------------------------------------------------------------

   implicit none

   type (domain),              intent(in)  :: grid
   type (iv_type),             intent(in)  :: iv
   integer,                    intent(in)  :: obs_index
   type (synop_type),          intent(in)  :: synop(:)
   type (residual_synop_type), intent(out) :: y_synop(:)

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

   if (trace_use) call da_trace_entry("da_transform_xtopsfc")

   allocate (hsm(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (tsm(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (qsm(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (psm(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (psm_prime(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (u(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (v(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (t(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))
   allocate (q(1,iv%info(obs_index)%n1:iv%info(obs_index)%n2))

   ! 2.0 Surface assimilation approach 2 (2-m t and q, 10-m u and v)

   call da_interp_lin_2d(grid%xa%u10,  iv%info(obs_index), 1, u)
   call da_interp_lin_2d(grid%xa%v10,  iv%info(obs_index), 1, v)
   call da_interp_lin_2d(grid%xa%psfc, iv%info(obs_index), 1, psm_prime)
   call da_interp_lin_2d(grid%xa%t2,   iv%info(obs_index), 1, t)
   call da_interp_lin_2d(grid%xa%q2,   iv%info(obs_index), 1, q)
   do n=iv%info(obs_index)%n1,iv%info(obs_index)%n2
      y_synop(n)%u=u(1,n)
      y_synop(n)%v=v(1,n)
      y_synop(n)%t=t(1,n)
      y_synop(n)%q=q(1,n)
   end do

   ! 3.2 model background surface p, t, q, h at observed site:

   call da_interp_lin_2d(grid%xb%terr, iv%info(obs_index), 1, hsm)
   call da_interp_lin_2d(grid%xb%t2,   iv%info(obs_index), 1, tsm)
   call da_interp_lin_2d(grid%xb%q2,   iv%info(obs_index), 1, qsm)
   call da_interp_lin_2d(grid%xb%psfc, iv%info(obs_index), 1, psm)

   do n=iv%info(obs_index)%n1,iv%info(obs_index)%n2
      if (synop(n)%p%qc < 0) then
         y_synop(n)%p = 0.0
      else

         ! 3.0 The pressure at the observed height: 

         ! 3.1 Constants:

          to = -888888.0
          qo = -888888.0
             
         ! Terrain height at the observed site:

         ! 3.3 perturbations t, q, p at the model surface:

         ! 4.0 Compute the perturbation of the surface pressure perturbation 
         !     at the observed height

         if (synop(n)%t%qc >= 0 .and. synop(n)%q%qc >= 0) then
            ! 4.1 Observed value = background + innovation: both t, q available:
            !     ----------------------------------------

            to = tsm(1,n) + synop(n)%t%inv
            qo = qsm(1,n) + synop(n)%q%inv
            call da_sfc_pre_lin(y_synop(n)%p, psm_prime(1,n), y_synop(n)%t, y_synop(n)%q, &
               psm(1,n), tsm(1,n), qsm(1,n), hsm(1,n), synop(n)%h, to, qo)
         else if (synop(n)%t%qc >= 0 .and. synop(n)%q%qc < 0) then

            ! 4.2 Observed value = background + innovation: only t available
            !     ----------------------------------------

            to = tsm(1,n) + synop(n)%t%inv
            call da_sfc_pre_lin(y_synop(n)%p, psm_prime(1,n), y_synop(n)%t, y_synop(n)%q, &
               psm(1,n), tsm(1,n), qsm(1,n), hsm(1,n), synop(n)%h, to)
         else
            ! 4.3 No observed t and q available:
            !     -----------------------------

            call da_sfc_pre_lin(y_synop(n)%p, psm_prime(1,n), y_synop(n)%t, y_synop(n)%q, &
               psm(1,n), tsm(1,n), qsm(1,n), hsm(1,n), synop(n)%h)
         end if
      end if
   end do

   deallocate (hsm)
   deallocate (tsm)
   deallocate (qsm)
   deallocate (psm)
   deallocate (psm_prime)
   deallocate (u)
   deallocate (v)
   deallocate (t)
   deallocate (q)

   if (trace_use) call da_trace_exit("da_transform_xtopsfc")

end subroutine da_transform_xtopsfc


