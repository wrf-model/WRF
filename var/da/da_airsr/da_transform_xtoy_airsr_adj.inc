subroutine da_transform_xtoy_airsr_adj(iv, jo_grad_y, jo_grad_x)

   !----------------------------------------------------------------------
   ! Purpose: Does adjoint computation at AIRS retrieval locations
   !----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n  ! Loop counter.

   real, allocatable :: t(:,:)
   real, allocatable :: q(:,:)           

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_airsr_adj")

   allocate (t(iv%info(airsr)%max_lev,iv%info(airsr)%n1:iv%info(airsr)%n2))
   allocate (q(iv%info(airsr)%max_lev,iv%info(airsr)%n1:iv%info(airsr)%n2))

   do n=iv%info(airsr)%n1,iv%info(airsr)%n2
      t(1:size(jo_grad_y%airsr(n)%t),n)  = jo_grad_y%airsr(n)%t
      q(1:size(jo_grad_y%airsr(n)%q),n)  = jo_grad_y%airsr(n)%q
   end do

   ! [1.1] Adjoint feedback from Y to X for u and v:

   call da_interp_lin_3d_adj (jo_grad_x%t, iv%info(airsr), t)
   call da_interp_lin_3d_adj (jo_grad_x%q, iv%info(airsr), q)

   deallocate (t)
   deallocate (q)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_airsr_adj")

end subroutine da_transform_xtoy_airsr_adj


