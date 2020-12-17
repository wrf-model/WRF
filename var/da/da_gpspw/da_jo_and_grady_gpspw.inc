subroutine da_jo_and_grady_gpspw(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type), intent(in)    :: re          ! Residual vector.
   type (y_type), intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer                      :: n

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_gpspw")

   jo % gpspw_tpw = 0.0

   if (iv%info(gpspw)%nlocal > 0) then
      do n=1, iv%info(gpspw)%nlocal
         jo_grad_y%gpspw(n)%tpw = -re%gpspw(n)%tpw / (iv%gpspw(n)%tpw%error * iv%gpspw(n)%tpw%error)

         if (iv%info(gpspw)%proc_domain(1,n)) then
            jo % gpspw_tpw = jo % gpspw_tpw - re%gpspw(n)%tpw * jo_grad_y%gpspw(n)%tpw
         end if
      end do

      jo % gpspw_tpw = 0.5 * jo % gpspw_tpw
   end if

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_gpspw")

end subroutine da_jo_and_grady_gpspw


