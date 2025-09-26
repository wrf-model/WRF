subroutine da_jo_and_grady_tamdar(iv, re, jo, jo_grad_y)

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
   
   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_tamdar")

   jo % tamdar_u = 0.0
   jo % tamdar_v = 0.0
   jo % tamdar_t = 0.0
   jo % tamdar_q = 0.0

   if (test_dm_exact) then
      if (iv%info(tamdar)%ntotal == 0) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_tamdar")
         return
      end if
   else
      if (iv%info(tamdar)%nlocal < 1) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_tamdar")
         return
      end if
   end if

   do n=1, iv%info(tamdar)%nlocal
       do k=1, iv%info(tamdar)%levels(n)
          jo_grad_y%tamdar(n)%u(k) = -re%tamdar(n)%u(k) / (iv%tamdar(n)%u(k)%error * iv%tamdar(n)%u(k)%error)
          jo_grad_y%tamdar(n)%v(k) = -re%tamdar(n)%v(k) / (iv%tamdar(n)%v(k)%error * iv%tamdar(n)%v(k)%error)
          jo_grad_y%tamdar(n)%t(k) = -re%tamdar(n)%t(k) / (iv%tamdar(n)%t(k)%error * iv%tamdar(n)%t(k)%error)
          jo_grad_y%tamdar(n)%q(k) = -re%tamdar(n)%q(k) / (iv%tamdar(n)%q(k)%error * iv%tamdar(n)%q(k)%error)
      end do
   end do

   ! Bitwise-exact reduction preserves operation order of serial code for 
   ! testing, at the cost of much-increased run-time.  Turn it off when not 
   ! testing.  This will always be .false. for a serial or 1-MPI-process run.  
   if (test_dm_exact) then
      ! perform remaining computations
      call da_jo_tamdar_uvtq(iv_glob, re_glob, jo_grad_y_glob, jo)
   else
      ! perform remaining computations
      call da_jo_tamdar_uvtq(iv, re, jo_grad_y, jo)
   end if

   jo % tamdar_u = 0.5 * jo % tamdar_u
   jo % tamdar_v = 0.5 * jo % tamdar_v
   jo % tamdar_t = 0.5 * jo % tamdar_t
   jo % tamdar_q = 0.5 * jo % tamdar_q

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_tamdar")

end subroutine da_jo_and_grady_tamdar


