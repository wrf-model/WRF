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

   integer :: n, k  ! Loop counter.

   real, allocatable :: model_u(:,:)  ! Model value u at ob location.
   real, allocatable :: model_v(:,:)  ! Model value v at ob location.
   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_profiler")

   allocate (model_u(iv%info(profiler)%max_lev,iv%info(profiler)%n1:iv%info(profiler)%n2))
   allocate (model_v(iv%info(profiler)%max_lev,iv%info(profiler)%n1:iv%info(profiler)%n2))
   allocate (ub(iv%info(profiler)%max_lev,iv%info(profiler)%n1:iv%info(profiler)%n2))
   allocate (vb(iv%info(profiler)%max_lev,iv%info(profiler)%n1:iv%info(profiler)%n2))

#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(profiler), model_u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(profiler), model_v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(profiler), model_u)
   call da_interp_lin_3d (grid%xa%v, iv%info(profiler), model_v)
#endif

   call da_interp_lin_3d (grid%xb%u, iv%info(profiler), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(profiler), vb)

   do n=iv%info(profiler)%n1,iv%info(profiler)%n2
      do k = 1, iv%info(profiler)%levels(n)

         if(wind_sd_profiler) then
            call da_uv_to_sd_lin(y%profiler(n)%u(k),y%profiler(n)%v(k),model_u(k,n),model_v(k,n),ub(k,n),vb(k,n))
         else
            y%profiler(n)%u(k) = model_u(k,n)
            y%profiler(n)%v(k) = model_v(k,n)
         end if

      end do
   end do

   deallocate (model_u)
   deallocate (model_v)
   deallocate (ub)
   deallocate (vb)   
   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_profiler")

end subroutine da_transform_xtoy_profiler 


