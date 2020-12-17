subroutine da_transform_xtoy_ssmi_tb_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer                       :: n                ! loop counter.

   real, allocatable :: tb19v(:)
   real, allocatable :: tb19h(:)
   real, allocatable :: tb22v(:)
   real, allocatable :: tb37v(:)
   real, allocatable :: tb37h(:)
   real, allocatable :: tb85v(:)
   real, allocatable :: tb85h(:)

   if (trace_use) call da_trace_entry("da_transform_xtoy_ssmi_tb_adj")

   allocate(tb19v(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb19h(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb22v(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb37v(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb37h(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb85v(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))
   allocate(tb85v(iv%info(ssmi_tb)%n1:iv%info(ssmi_tb)%n2))

   do n=iv%info(ssmi_tb)%n1,iv%info(ssmi_tb)%n2
      tb19v(n) = jo_grad_y%ssmi_tb(n)%tb19v
      tb19h(n) = jo_grad_y%ssmi_tb(n)%tb19h
      tb22v(n) = jo_grad_y%ssmi_tb(n)%tb22v
      tb37v(n) = jo_grad_y%ssmi_tb(n)%tb37v
      tb37h(n) = jo_grad_y%ssmi_tb(n)%tb37h
      tb85v(n) = jo_grad_y%ssmi_tb(n)%tb85v
      tb85h(n) = jo_grad_y%ssmi_tb(n)%tb85h
   end do

   call da_interp_lin_2d_adj(jo_grad_x%tb19v, iv%info(ssmi_tb), 1, tb19v)
   call da_interp_lin_2d_adj(jo_grad_x%tb19h, iv%info(ssmi_tb), 1, tb19h)
   call da_interp_lin_2d_adj(jo_grad_x%tb22v, iv%info(ssmi_tb), 1, tb22v)
   call da_interp_lin_2d_adj(jo_grad_x%tb37v, iv%info(ssmi_tb), 1, tb37v)
   call da_interp_lin_2d_adj(jo_grad_x%tb37h, iv%info(ssmi_tb), 1, tb37h)
   call da_interp_lin_2d_adj(jo_grad_x%tb85v, iv%info(ssmi_tb), 1, tb85v)
   call da_interp_lin_2d_adj(jo_grad_x%tb85h, iv%info(ssmi_tb), 1, tb85h)
  
   deallocate(tb19v)
   deallocate(tb19h)
   deallocate(tb22v)
   deallocate(tb37v)
   deallocate(tb37h)
   deallocate(tb85v)
   deallocate(tb85v)

   if (trace_use) call da_trace_exit("da_transform_xtoy_ssmi_tb_adj")

end subroutine da_transform_xtoy_ssmi_tb_adj


