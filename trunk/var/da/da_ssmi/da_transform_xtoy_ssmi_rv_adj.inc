subroutine da_transform_xtoy_ssmi_rv_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)       :: grid
   type (iv_type), intent(in)       :: iv          ! obs. inc. vector (o-b).
   type (y_type) , intent(in)       :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout)    :: jo_grad_x   ! grad_x(jo)

   integer :: n

   real, allocatable :: tpw(:)
   real, allocatable :: speed(:)

   if (trace_use) call da_trace_entry("da_transform_xtoy_ssmi_rv_adj")

   allocate (tpw(iv%info(ssmi_rv)%n1:iv%info(ssmi_rv)%n2))
   allocate (speed(iv%info(ssmi_rv)%n1:iv%info(ssmi_rv)%n2))

   do n=iv%info(ssmi_rv)%n1,iv%info(ssmi_rv)%n2
      tpw(n)   = jo_grad_y%ssmi_rv(n)%tpw
      speed(n) = jo_grad_y%ssmi_rv(n)%speed
   end do

   call da_interp_lin_2d_adj(jo_grad_x%tpw,   iv%info(ssmi_rv), 1, tpw)
   call da_interp_lin_2d_adj(jo_grad_x%speed, iv%info(ssmi_rv), 1, speed)
   
   deallocate (tpw)
   deallocate (speed)

   if (trace_use) call da_trace_exit("da_transform_xtoy_ssmi_rv_adj")

end subroutine da_transform_xtoy_ssmi_rv_adj


