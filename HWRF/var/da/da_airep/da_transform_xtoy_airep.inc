subroutine da_transform_xtoy_airep (grid, iv, y)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !--------------------------------------------------------------------------

   implicit none

   type(domain),  intent(in)    :: grid
   type(iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type(y_type),  intent(inout) :: y        ! y = h (grid%grid%xa) (linear)

   integer :: n        ! Loop counter.

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: t(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_airep")

   allocate (u(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))
   allocate (v(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))
   allocate (t(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))
  
#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(airep), u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(airep), v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(airep), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(airep), v)
#endif
   call da_interp_lin_3d (grid%xa%t, iv%info(airep), t)

   do n=iv%info(airep)%n1,iv%info(airep)%n2
      y%airep(n)%u(:) = u(1:size(y%airep(n)%u),n)
      y%airep(n)%v(:) = v(1:size(y%airep(n)%v),n)
      y%airep(n)%t(:) = t(1:size(y%airep(n)%t),n)
   end do

   deallocate (u)
   deallocate (v)
   deallocate (t)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_airep")

end subroutine da_transform_xtoy_airep


