subroutine da_jo_and_grady_tamdar_sfc( iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)    :: iv          ! Innovation vector.
   type(y_type),  intent(in)    :: re          ! Residual vector.
   type(y_type),  intent(inout) :: jo_grad_y   ! Grad_y(Jo)
   type(jo_type), intent(inout) :: jo          ! Obs cost function.

   integer                      :: n
   ! the following "global" objects are used only when testing
   type(iv_type) :: iv_glob         ! Global Innovation vector(O-B).
   type(y_type)  :: re_glob         ! Global Residual vector(O-A).
   type(y_type)  :: jo_grad_y_glob  ! Global Grad_y(Jo)

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_tamdar_sfc")

   jo % tamdar_sfc_u = 0.0
   jo % tamdar_sfc_v = 0.0
   jo % tamdar_sfc_t = 0.0
   jo % tamdar_sfc_p = 0.0
   jo % tamdar_sfc_q = 0.0

   if (test_dm_exact) then
      if (iv%info(tamdar_sfc)%ntotal == 0) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_tamdar_sfc")
         return
      end if
   else
      if (iv%info(tamdar_sfc)%nlocal < 1) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_tamdar_sfc")
         return
      end if
   end if

   do n=1, iv%info(tamdar_sfc)%nlocal
      jo_grad_y%tamdar_sfc(n)%u = -re%tamdar_sfc(n)%u / (iv%tamdar_sfc(n)%u%error * iv%tamdar_sfc(n)%u%error)
      jo_grad_y%tamdar_sfc(n)%v = -re%tamdar_sfc(n)%v / (iv%tamdar_sfc(n)%v%error * iv%tamdar_sfc(n)%v%error)
      jo_grad_y%tamdar_sfc(n)%t = -re%tamdar_sfc(n)%t / (iv%tamdar_sfc(n)%t%error * iv%tamdar_sfc(n)%t%error)
      jo_grad_y%tamdar_sfc(n)%p = -re%tamdar_sfc(n)%p / (iv%tamdar_sfc(n)%p%error * iv%tamdar_sfc(n)%p%error)
      jo_grad_y%tamdar_sfc(n)%q = -re%tamdar_sfc(n)%q / (iv%tamdar_sfc(n)%q%error * iv%tamdar_sfc(n)%q%error)
   end do

      call da_jo_tamdar_sfc_uvtq( iv, re, jo_grad_y, jo)

   jo % tamdar_sfc_u = 0.5 * jo % tamdar_sfc_u
   jo % tamdar_sfc_v = 0.5 * jo % tamdar_sfc_v
   jo % tamdar_sfc_t = 0.5 * jo % tamdar_sfc_t
   jo % tamdar_sfc_p = 0.5 * jo % tamdar_sfc_p
   jo % tamdar_sfc_q = 0.5 * jo % tamdar_sfc_q
   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_tamdar_sfc")

end subroutine da_jo_and_grady_tamdar_sfc


