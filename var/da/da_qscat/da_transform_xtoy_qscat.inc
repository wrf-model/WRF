subroutine da_transform_xtoy_qscat(grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n        ! Loop counter.

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_qscat")

   allocate (u(1,iv%info(qscat)%n1:iv%info(qscat)%n2))
   allocate (v(1,iv%info(qscat)%n1:iv%info(qscat)%n2))

#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(qscat), u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(qscat), v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(qscat), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(qscat), v)
#endif

   do n=iv%info(qscat)%n1,iv%info(qscat)%n2
      y%qscat(n)%u = u(1,n)
      y%qscat(n)%v = v(1,n)
   end do

   deallocate (u)
   deallocate (v)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_qscat")

end subroutine da_transform_xtoy_qscat


