subroutine da_jo_mtgirs_uvtq (iv, re, jo_grad_y, jo)

   !-----------------------------------------------------------------------
   ! Purpose: Ensures that exactly the same code is used for both 
   ! serial and parallel computations when testing_dm_bitwise_exact is set.
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv         ! Innovation vector.
   type (y_type),  intent(in)    :: re         ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo         ! Obs cost function.
 
   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_jo_mtgirs_uvtq")

   do n=1, iv%info(mtgirs)%nlocal
      do k=1, iv%info(mtgirs)%levels(n)
         if (iv%info(mtgirs)%proc_domain(1,n)) then
            jo % mtgirs_u = jo % mtgirs_u - re%mtgirs(n)%u(k) * jo_grad_y%mtgirs(n)%u(k)
            jo % mtgirs_v = jo % mtgirs_v - re%mtgirs(n)%v(k) * jo_grad_y%mtgirs(n)%v(k)
            jo % mtgirs_t = jo % mtgirs_t - re%mtgirs(n)%t(k) * jo_grad_y%mtgirs(n)%t(k)
            jo % mtgirs_q = jo % mtgirs_q - re%mtgirs(n)%q(k) * jo_grad_y%mtgirs(n)%q(k)
         end if
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_jo_mtgirs_uvtq")

end subroutine da_jo_mtgirs_uvtq


