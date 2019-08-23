subroutine da_set_randomcv(cv_size, rcv)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate components of control variable.
   !
   ! Method:  Allocate component in turn.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in) :: cv_size
   real, intent(inout) :: rcv(1:cv_size)     ! Control variable structure.

   integer             :: i
   real                :: mean_cv, rms_cv, std_dev_cv
   real                :: sum_cv, sum_cv2

   if (trace_use) call da_trace_entry("da_set_randomcv")

   ! [1] Initialize random number generator and scalars:

   !moved to da_solve.inc
   !call da_random_seed

   sum_cv = 0.0
   sum_cv2 = 0.0

   ! [2] Calculate random numbers with Gaussian distribution:

   do i = 1, cv_size
      call da_gauss_noise(rcv(i))

      sum_cv = sum_cv + rcv(i)
      sum_cv2 = sum_cv2 + rcv(i) * rcv(i)
   end do

   mean_cv = sum_cv / real(cv_size)
   rms_cv = sqrt(sum_cv2 / real(cv_size))
   std_dev_cv = sqrt(rms_cv * rms_cv - mean_cv * mean_cv)

   write(unit=stdout,fmt=*)
   write(unit=stdout,fmt='(a)')' Gaussian (Normal) noise statistics:'
   write(unit=stdout,fmt='(a,f15.5)')' Mean    = ', mean_cv
   write(unit=stdout,fmt='(a,f15.5)')' RMS     = ', rms_cv
   write(unit=stdout,fmt='(a,f15.5)')' STD DEV = ', std_dev_cv

   if (trace_use) call da_trace_exit("da_set_randomcv")

end subroutine da_set_randomcv


