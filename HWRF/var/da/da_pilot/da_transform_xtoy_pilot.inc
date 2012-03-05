subroutine da_transform_xtoy_pilot (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n  ! Loop counter.

   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_pilot")

   allocate (model_u(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))
   allocate (model_v(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))

#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(pilot), model_u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(pilot), model_v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(pilot), model_u)
   call da_interp_lin_3d (grid%xa%v, iv%info(pilot), model_v)
#endif

   do n=iv%info(pilot)%n1,iv%info(pilot)%n2
      y%pilot(n)%u(:) = model_u(1:iv%info(pilot)%levels(n),n)
      y%pilot(n)%v(:) = model_v(1:iv%info(pilot)%levels(n),n)
   end do

   deallocate (model_u)
   deallocate (model_v)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_pilot")

end subroutine da_transform_xtoy_pilot 


