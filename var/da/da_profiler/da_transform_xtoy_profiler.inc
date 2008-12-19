subroutine da_transform_xtoy_profiler (grid, iv, y)

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

   real, allocatable :: model_u(:,:)  ! Model value u at ob location.
   real, allocatable :: model_v(:,:)  ! Model value v at ob location.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_profiler")

   allocate (model_u(iv%info(profiler)%max_lev,iv%info(profiler)%n1:iv%info(profiler)%n2))
   allocate (model_v(iv%info(profiler)%max_lev,iv%info(profiler)%n1:iv%info(profiler)%n2))

#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(profiler), model_u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(profiler), model_v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(profiler), model_u)
   call da_interp_lin_3d (grid%xa%v, iv%info(profiler), model_v)
#endif

   do n=iv%info(profiler)%n1,iv%info(profiler)%n2
      y%profiler(n)%u(:) = model_u(1:iv%info(profiler)%levels(n),n)
      y%profiler(n)%v(:) = model_v(1:iv%info(profiler)%levels(n),n)
   end do

   deallocate (model_u)
   deallocate (model_v)
   
   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_profiler")

end subroutine da_transform_xtoy_profiler 


