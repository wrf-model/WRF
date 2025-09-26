subroutine da_transform_xtoy_airsr (grid, iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: Does transforms from model space to AIRS locations
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-------------------------------------------------------------------------

   implicit none

   type (domain),     intent(in)    :: grid     ! gridded analysis increment.
   type (iv_type),    intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),     intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n  ! Loop counter.

   real, allocatable :: t(:,:)
   real, allocatable :: q(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_airsr")

   allocate (t(iv%info(airsr)%max_lev,iv%info(airsr)%n1:iv%info(airsr)%n2))
   allocate (q(iv%info(airsr)%max_lev,iv%info(airsr)%n1:iv%info(airsr)%n2))
  
   call da_interp_lin_3d (grid%xa%t, iv%info(airsr), t)
   call da_interp_lin_3d (grid%xa%q, iv%info(airsr), q)

   do n=iv%info(airsr)%n1,iv%info(airsr)%n2
      y%airsr(n)%t(:) = t(1:size(y%airsr(n)%t),n)
      y%airsr(n)%q(:) = q(1:size(y%airsr(n)%q),n)
   end do

   deallocate (t)
   deallocate (q)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_airsr")

end subroutine da_transform_xtoy_airsr


