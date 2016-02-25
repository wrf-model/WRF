subroutine da_transform_xtoy_ssmi_rv(grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! obs. increment vector (o-b).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n        ! loop counter.

   real, allocatable :: tpw(:)
   real, allocatable :: speed(:)

   if (trace_use) call da_trace_entry("da_transform_xtoy_ssmi_rv")

   ! SSMI observation operator y = H(x):

   allocate (tpw(iv%info(ssmi_rv)%n1:iv%info(ssmi_rv)%n2))
   allocate (speed(iv%info(ssmi_rv)%n1:iv%info(ssmi_rv)%n2))

   call da_interp_lin_2d (grid%xa%tpw,   iv%info(ssmi_rv), 1, tpw)
   call da_interp_lin_2d (grid%xa%speed, iv%info(ssmi_rv), 1, speed)   
   
   do n=iv%info(ssmi_rv)%n1,iv%info(ssmi_rv)%n2
      y%ssmi_rv(n)%tpw   = tpw(n)
      y%ssmi_rv(n)%speed = speed(n)      
   end do

   deallocate (tpw)
   deallocate (speed)

   if (trace_use) call da_trace_exit("da_transform_xtoy_ssmi_rv")

end subroutine da_transform_xtoy_ssmi_rv


