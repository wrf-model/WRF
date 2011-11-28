subroutine da_jo_synop_uvtq(iv, re, jo_grad_y, jo)

   !-----------------------------------------------------------------------
   ! Purpose: Ensures that exactly the same code is used for both
   ! serial and parallel computations when testing_dm_bitwise_exact is set.
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv         ! Innovation vector.
   type (y_type),  intent(in)    :: re         ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo         ! Obs cost function.

   integer :: n

   if (trace_use_dull) call da_trace_entry("da_jo_synop_uvtq")

   do n=1, iv%info(synop)%nlocal
      if (iv%info(synop)%proc_domain(1,n)) then
         jo % synop_u = jo % synop_u - re%synop(n)%u * jo_grad_y%synop(n)%u
         jo % synop_v = jo % synop_v - re%synop(n)%v * jo_grad_y%synop(n)%v
         jo % synop_t = jo % synop_t - re%synop(n)%t * jo_grad_y%synop(n)%t
         jo % synop_p = jo % synop_p - re%synop(n)%p * jo_grad_y%synop(n)%p
         jo % synop_q = jo % synop_q - re%synop(n)%q * jo_grad_y%synop(n)%q
     end if
  end do

   if (trace_use_dull) call da_trace_exit("da_jo_synop_uvtq")

end subroutine da_jo_synop_uvtq


