subroutine da_jo_and_grady_sonde_sfc( iv, re, jo, jo_grad_y)

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

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_sonde_sfc")

   jo % sonde_sfc_u = 0.0
   jo % sonde_sfc_v = 0.0
   jo % sonde_sfc_t = 0.0
   jo % sonde_sfc_p = 0.0
   jo % sonde_sfc_q = 0.0

   if (test_dm_exact) then
      if (iv%info(sound)%ntotal == 0) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_sonde_sfc")
         return
      end if
   else
      if (iv%info(sound)%nlocal < 1) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_sonde_sfc")
         return
      end if
   end if

   do n=1, iv%info(sound)%nlocal
      jo_grad_y%sonde_sfc(n)%u = -re%sonde_sfc(n)%u / (iv%sonde_sfc(n)%u%error * iv%sonde_sfc(n)%u%error)
      jo_grad_y%sonde_sfc(n)%v = -re%sonde_sfc(n)%v / (iv%sonde_sfc(n)%v%error * iv%sonde_sfc(n)%v%error)
      jo_grad_y%sonde_sfc(n)%t = -re%sonde_sfc(n)%t / (iv%sonde_sfc(n)%t%error * iv%sonde_sfc(n)%t%error)
      jo_grad_y%sonde_sfc(n)%p = -re%sonde_sfc(n)%p / (iv%sonde_sfc(n)%p%error * iv%sonde_sfc(n)%p%error)
      jo_grad_y%sonde_sfc(n)%q = -re%sonde_sfc(n)%q / (iv%sonde_sfc(n)%q%error * iv%sonde_sfc(n)%q%error)
   end do

   ! Bitwise-exact reduction preserves operation order of serial code for
   ! testing, at the cost of much-increased run-time.  Turn it off when not
   ! testing.  This will always be .false. for a serial run.
   if (test_dm_exact) then
      ! collect all obs in serial order and allocate global objects
      call da_to_global_sonde_sfc( iv, re, jo_grad_y, iv_glob, re_glob, jo_grad_y_glob)
      ! perform remaining computations
      call da_jo_sonde_sfc_uvtq( iv_glob, re_glob, jo_grad_y_glob, jo)
      ! free global objects
      call da_deallocate_global_sonde_sfc( iv_glob, re_glob, jo_grad_y_glob)
   else
      ! perform remaining computations
      call da_jo_sonde_sfc_uvtq( iv, re, jo_grad_y, jo)
   end if

   jo % sonde_sfc_u = 0.5 * jo % sonde_sfc_u
   jo % sonde_sfc_v = 0.5 * jo % sonde_sfc_v
   jo % sonde_sfc_t = 0.5 * jo % sonde_sfc_t
   jo % sonde_sfc_p = 0.5 * jo % sonde_sfc_p
   jo % sonde_sfc_q = 0.5 * jo % sonde_sfc_q

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_sonde_sfc")

end subroutine da_jo_and_grady_sonde_sfc


