subroutine da_jo_and_grady_ssmi_tb(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! Ob Inc. structure.
   type (y_type),  intent(in)    :: re          ! Residual structure.
   type (y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(out)   :: jo          ! Obs cost function.

   integer :: n

   if (trace_use) call da_trace_entry("da_jo_and_grady_ssmi_tb")

   jo % ssmi_tb19v = 0.0
   jo % ssmi_tb19h = 0.0
   jo % ssmi_tb22v = 0.0
   jo % ssmi_tb37v = 0.0
   jo % ssmi_tb37h = 0.0
   jo % ssmi_tb85v = 0.0
   jo % ssmi_tb85h = 0.0

   do n=1, iv%info(ssmi_tb)%nlocal

      jo_grad_y%ssmi_tb(n)%tb19v = - re%ssmi_tb(n)%tb19v / &
         (iv%ssmi_tb(n)%tb19v%error * iv%ssmi_tb(n)%tb19v%error)

      jo_grad_y%ssmi_tb(n)%tb19h = - re%ssmi_tb(n)%tb19h / &
          (iv%ssmi_tb(n)%tb19h%error * iv%ssmi_tb(n)%tb19h%error)

      jo_grad_y%ssmi_tb(n)%tb22v = - re%ssmi_tb(n)%tb22v / &
         (iv%ssmi_tb(n)%tb22v%error * iv%ssmi_tb(n)%tb22v%error)

      jo_grad_y%ssmi_tb(n)%tb37v = - re%ssmi_tb(n)%tb37v / &
         (iv%ssmi_tb(n)%tb37v%error * iv%ssmi_tb(n)%tb37v%error)

      jo_grad_y%ssmi_tb(n)%tb37h = - re%ssmi_tb(n)%tb37h / &
         (iv%ssmi_tb(n)%tb37h%error * iv%ssmi_tb(n)%tb37h%error)

      jo_grad_y%ssmi_tb(n)%tb85v = - re%ssmi_tb(n)%tb85v / &
         (iv%ssmi_tb(n)%tb85v%error * iv%ssmi_tb(n)%tb85v%error)

      jo_grad_y%ssmi_tb(n)%tb85h = - re%ssmi_tb(n)%tb85h / &
         (iv%ssmi_tb(n)%tb85h%error * iv%ssmi_tb(n)%tb85h%error)

      if (iv%info(ssmi_tb)%proc_domain(1,n)) then
         jo % ssmi_tb19v = jo % ssmi_tb19v - re%ssmi_tb(n)%tb19v * jo_grad_y%ssmi_tb(n)%tb19v 
         jo % ssmi_tb19h = jo % ssmi_tb19h - re%ssmi_tb(n)%tb19h * jo_grad_y%ssmi_tb(n)%tb19h
         jo % ssmi_tb22v = jo % ssmi_tb22v - re%ssmi_tb(n)%tb22v * jo_grad_y%ssmi_tb(n)%tb22v
         jo % ssmi_tb37v = jo % ssmi_tb37v - re%ssmi_tb(n)%tb37v * jo_grad_y%ssmi_tb(n)%tb37v
         jo % ssmi_tb37h = jo % ssmi_tb37h - re%ssmi_tb(n)%tb37h * jo_grad_y%ssmi_tb(n)%tb37h
         jo % ssmi_tb85v = jo % ssmi_tb85v - re%ssmi_tb(n)%tb85v * jo_grad_y%ssmi_tb(n)%tb85v
         jo % ssmi_tb85h = jo % ssmi_tb85h - re%ssmi_tb(n)%tb85h * jo_grad_y%ssmi_tb(n)%tb85h
      end if
   end do
   
   jo % ssmi_tb19h = 0.5 * jo % ssmi_tb19h
   jo % ssmi_tb19v = 0.5 * jo % ssmi_tb19v
   jo % ssmi_tb22v = 0.5 * jo % ssmi_tb22v
   jo % ssmi_tb37h = 0.5 * jo % ssmi_tb37h
   jo % ssmi_tb37v = 0.5 * jo % ssmi_tb37v
   jo % ssmi_tb85h = 0.5 * jo % ssmi_tb85h
   jo % ssmi_tb85v = 0.5 * jo % ssmi_tb85v

   if (trace_use) call da_trace_exit("da_jo_and_grady_ssmi_tb")

end subroutine da_jo_and_grady_ssmi_tb


