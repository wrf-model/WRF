subroutine da_jo_and_grady_synop(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type),  intent(in)   :: re          ! Residual vector.
   type (y_type),  intent(inout):: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer        :: n

   ! the following "global" objects are used only when testing
   type (iv_type) :: iv_glob         ! Global Innovation vector (O-B).
   type (y_type)  :: re_glob         ! Global Residual vector (O-A).
   type (y_type)  :: jo_grad_y_glob  ! Global Grad_y(Jo)

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_synop")

   jo % synop_u = 0.0
   jo % synop_v = 0.0
   jo % synop_t = 0.0
   jo % synop_p = 0.0
   jo % synop_q = 0.0

   if (test_dm_exact) then
      if (iv%info(synop)%ntotal == 0) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_synop")
         return
      end if
   else
      if (iv%info(synop)%nlocal < 1) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_synop")
         return
      end if
   end if

   do n=1, iv%info(synop)%nlocal
      jo_grad_y%synop(n)%u = -re%synop(n)%u / (iv%synop(n)%u%error * iv%synop(n)%u%error)
      jo_grad_y%synop(n)%v = -re%synop(n)%v / (iv%synop(n)%v%error * iv%synop(n)%v%error)
      jo_grad_y%synop(n)%t = -re%synop(n)%t / (iv%synop(n)%t%error * iv%synop(n)%t%error)
      jo_grad_y%synop(n)%p = -re%synop(n)%p / (iv%synop(n)%p%error * iv%synop(n)%p%error)
      jo_grad_y%synop(n)%q = -re%synop(n)%q / (iv%synop(n)%q%error * iv%synop(n)%q%error)
   end do

   ! Bitwise-exact reduction preserves operation order of serial code for
   ! testing, at the cost of much-increased run-time.  Turn it off when not
   ! testing.  This will always be .false. for a serial run.
   if (test_dm_exact) then
      ! collect all obs in serial order and allocate global objects
      call da_to_global_synop(iv, re, jo_grad_y, iv_glob, re_glob, jo_grad_y_glob)
      ! perform remaining computations
      call da_jo_synop_uvtq(iv_glob, re_glob, jo_grad_y_glob, jo)
      ! free global objects
      call da_deallocate_global_synop(iv_glob, re_glob, jo_grad_y_glob)
   else
      ! perform remaining computations
      call da_jo_synop_uvtq(iv, re, jo_grad_y, jo)
   end if

   jo % synop_u = 0.5 * jo % synop_u
   jo % synop_v = 0.5 * jo % synop_v
   jo % synop_t = 0.5 * jo % synop_t
   jo % synop_p = 0.5 * jo % synop_p
   jo % synop_q = 0.5 * jo % synop_q

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_synop")

end subroutine da_jo_and_grady_synop


