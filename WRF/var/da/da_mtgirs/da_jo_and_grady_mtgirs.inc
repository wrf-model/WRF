subroutine da_jo_and_grady_mtgirs(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv         ! Innovation vector.
   type (y_type),  intent(in)    :: re         ! Residual vector.
   type (y_type),  intent(inout) :: jo_grad_y  ! Grad_y(Jo)
   type (jo_type), intent(inout) :: jo         ! Obs cost function.

   integer                      :: n, k
   ! the following "global" objects are used only when testing
   type (iv_type) :: iv_glob         ! Global Innovation vector (O-B).
   type (y_type)  :: re_glob         ! Global Residual vector (O-A).
   type (y_type)  :: jo_grad_y_glob  ! Global Grad_y(Jo)
   
   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_mtgirs")

   jo % mtgirs_u = 0.0
   jo % mtgirs_v = 0.0
   jo % mtgirs_t = 0.0
   jo % mtgirs_q = 0.0

   if (test_dm_exact) then
      if (iv%info(mtgirs)%ntotal == 0) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_mtgirs")
         return
      end if
   else
      if (iv%info(mtgirs)%nlocal < 1) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_mtgirs")
         return
      end if
   end if

   do n=1, iv%info(mtgirs)%nlocal
       do k=1, iv%info(mtgirs)%levels(n)
          jo_grad_y%mtgirs(n)%u(k) = -re%mtgirs(n)%u(k) / (iv%mtgirs(n)%u(k)%error * iv%mtgirs(n)%u(k)%error)
          jo_grad_y%mtgirs(n)%v(k) = -re%mtgirs(n)%v(k) / (iv%mtgirs(n)%v(k)%error * iv%mtgirs(n)%v(k)%error)
          jo_grad_y%mtgirs(n)%t(k) = -re%mtgirs(n)%t(k) / (iv%mtgirs(n)%t(k)%error * iv%mtgirs(n)%t(k)%error)
          jo_grad_y%mtgirs(n)%q(k) = -re%mtgirs(n)%q(k) / (iv%mtgirs(n)%q(k)%error * iv%mtgirs(n)%q(k)%error)
      end do
   end do

   ! Bitwise-exact reduction preserves operation order of serial code for 
   ! testing, at the cost of much-increased run-time.  Turn it off when not 
   ! testing.  This will always be .false. for a serial or 1-MPI-process run.  
   if (test_dm_exact) then
      ! collect all obs in serial order and allocate global objects
!      call da_to_global_mtgirs(iv, re, jo_grad_y, iv_glob, re_glob, jo_grad_y_glob)
      ! perform remaining computations
      call da_jo_mtgirs_uvtq(iv_glob, re_glob, jo_grad_y_glob, jo)
      ! free global objects
!      call da_deallocate_global_mtgirs(iv_glob, re_glob, jo_grad_y_glob)
   else
      ! perform remaining computations
      call da_jo_mtgirs_uvtq(iv, re, jo_grad_y, jo)
   end if

   jo % mtgirs_u = 0.5 * jo % mtgirs_u
   jo % mtgirs_v = 0.5 * jo % mtgirs_v
   jo % mtgirs_t = 0.5 * jo % mtgirs_t
   jo % mtgirs_q = 0.5 * jo % mtgirs_q

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_mtgirs")

end subroutine da_jo_and_grady_mtgirs


