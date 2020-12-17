subroutine da_jo_and_grady_sound(iv, re, jo, jo_grad_y)

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
   
   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_sound")

   jo % sound_u = 0.0
   jo % sound_v = 0.0
   jo % sound_t = 0.0
   jo % sound_q = 0.0

   if (test_dm_exact) then
      if (iv%info(sound)%ntotal == 0) then 
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_sound")
         return
      end if
   else
      if (iv%info(sound)%nlocal < 1) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_sound")
         return
      end if
   end if

   do n=1, iv%info(sound)%nlocal
       do k=1, iv%info(sound)%levels(n)
          jo_grad_y%sound(n)%u(k) = -re%sound(n)%u(k) / (iv%sound(n)%u(k)%error * iv%sound(n)%u(k)%error)
          jo_grad_y%sound(n)%v(k) = -re%sound(n)%v(k) / (iv%sound(n)%v(k)%error * iv%sound(n)%v(k)%error)
          jo_grad_y%sound(n)%t(k) = -re%sound(n)%t(k) / (iv%sound(n)%t(k)%error * iv%sound(n)%t(k)%error)
          jo_grad_y%sound(n)%q(k) = -re%sound(n)%q(k) / (iv%sound(n)%q(k)%error * iv%sound(n)%q(k)%error)
      end do
   end do

   ! Bitwise-exact reduction preserves operation order of serial code for 
   ! testing, at the cost of much-increased run-time.  Turn it off when not 
   ! testing.  This will always be .false. for a serial or 1-MPI-process run.  
   if (test_dm_exact) then
      ! collect all obs in serial order and allocate global objects
      call da_to_global_sound(iv, re, jo_grad_y, iv_glob, re_glob, jo_grad_y_glob)
      ! perform remaining computations
      call da_jo_sound_uvtq(iv_glob, re_glob, jo_grad_y_glob, jo)
      ! free global objects
      call da_deallocate_global_sound(iv_glob, re_glob, jo_grad_y_glob)
   else
      ! perform remaining computations
      call da_jo_sound_uvtq(iv, re, jo_grad_y, jo)
   end if

   jo % sound_u = 0.5 * jo % sound_u
   jo % sound_v = 0.5 * jo % sound_v
   jo % sound_t = 0.5 * jo % sound_t
   jo % sound_q = 0.5 * jo % sound_q

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_sound")

end subroutine da_jo_and_grady_sound


