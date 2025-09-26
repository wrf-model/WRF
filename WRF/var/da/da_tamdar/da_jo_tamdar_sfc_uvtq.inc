subroutine da_jo_tamdar_sfc_uvtq(iv, re, jo_grad_y, jo)

   !-----------------------------------------------------------------------
   ! Purpose: Ensures that exactly the same code is used for both
   ! serial and parallel computations when testing_dm_bitwise_exact is set.
   !-----------------------------------------------------------------------
 
   implicit none

   type (iv_type), intent(in  ) :: iv         ! Innovation vector.
   type (y_type),  intent(in  ) :: re         ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo         ! Obs cost function.
  
   integer :: n

   if (trace_use_dull) call da_trace_entry("da_jo_tamdar_sfc_uvtq")

   do n=1, iv%info(tamdar_sfc)%nlocal
      if (iv%info(tamdar_sfc)%proc_domain(1,n)) then
        jo % tamdar_sfc_u = jo % tamdar_sfc_u - re%tamdar_sfc(n)%u * jo_grad_y%tamdar_sfc(n)%u
        jo % tamdar_sfc_v = jo % tamdar_sfc_v - re%tamdar_sfc(n)%v * jo_grad_y%tamdar_sfc(n)%v
        jo % tamdar_sfc_t = jo % tamdar_sfc_t - re%tamdar_sfc(n)%t * jo_grad_y%tamdar_sfc(n)%t
        jo % tamdar_sfc_p = jo % tamdar_sfc_p - re%tamdar_sfc(n)%p * jo_grad_y%tamdar_sfc(n)%p
        jo % tamdar_sfc_q = jo % tamdar_sfc_q - re%tamdar_sfc(n)%q * jo_grad_y%tamdar_sfc(n)%q
      end if
   end do

   if (trace_use_dull) call da_trace_exit("da_jo_tamdar_sfc_uvtq")

end subroutine da_jo_tamdar_sfc_uvtq


