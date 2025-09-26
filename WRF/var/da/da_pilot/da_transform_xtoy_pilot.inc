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

   integer :: n, k  ! Loop counter.

   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_pilot")

   allocate (model_u(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))
   allocate (model_v(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))
   allocate (ub(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))
   allocate (vb(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))

#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(pilot), model_u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(pilot), model_v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(pilot), model_u)
   call da_interp_lin_3d (grid%xa%v, iv%info(pilot), model_v)
#endif
   call da_interp_lin_3d (grid%xb%u, iv%info(pilot), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(pilot), vb)

   do n=iv%info(pilot)%n1,iv%info(pilot)%n2
      do k = 1, iv%info(pilot)%levels(n)

         if(wind_sd_pilot) then
            call da_uv_to_sd_lin(y%pilot(n)%u(k),y%pilot(n)%v(k),model_u(k,n),model_v(k,n),ub(k,n),vb(k,n))
         else
            y%pilot(n)%u(k) = model_u(k,n)
            y%pilot(n)%v(k) = model_v(k,n)
         end if

      end do

   end do

   deallocate (model_u)
   deallocate (model_v)
   deallocate (ub)
   deallocate (vb)
   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_pilot")

end subroutine da_transform_xtoy_pilot 


