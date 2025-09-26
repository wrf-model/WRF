subroutine da_jo_and_grady_satem(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_satem")

   jo % satem_thickness = 0.0

   do n=1, iv%info(satem)%nlocal
      do k=1, iv%info(satem)%levels(n)
         jo_grad_y%satem(n)%thickness(k) = -re%satem(n)%thickness(k) / &
            (iv%satem(n)%thickness(k)%error * iv%satem(n)%thickness(k)%error)
      end do

      if (iv%info(satem)%proc_domain(1,n)) then
         do k=1, iv%info(satem)%levels(n)
            jo % satem_thickness = jo % satem_thickness - &
               re%satem(n)%thickness(k) * jo_grad_y%satem(n)%thickness(k)
         end do
      end if
   end do

   jo % satem_thickness = 0.5 * jo % satem_thickness

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_satem")

end subroutine da_jo_and_grady_satem


