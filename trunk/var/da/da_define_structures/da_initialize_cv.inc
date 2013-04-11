subroutine da_initialize_cv(cv_size, cv)

   !---------------------------------------------------------------------------
   ! Purpose: Initialize components of control variable.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)   :: cv_size
   real,    intent(out)  :: cv(1:cv_size)    ! Control variable structure.

   integer                              :: i
   real                                 :: z, mean_cv, rms_cv, std_dev_cv

   if (trace_use) call da_trace_entry("da_initialize_cv")

   !---------------------------------------------------------------------------
   ! [1.0] Initialize cv:
   !---------------------------------------------------------------------------

   if (anal_type_randomcv) then
   
      ! [2.1] Initialize random number generator and scalars:

      call da_random_seed
      
      if( use_rf )then

         ! [2.2] Calculate random numbers with Gaussian distribution:

         do i = 1, cv_size
            call da_gauss_noise(z)
            cv(i) = z
         end do
      else
         write(stdout,'("da_initialize_cv.inc: need to inject cv into wavelet space")')
         call wrf_abort
      endif

      mean_cv = sum(cv) / real(cv_size)
      rms_cv = sqrt(sum(cv*cv) / real(cv_size))
      std_dev_cv = sqrt(rms_cv * rms_cv - mean_cv * mean_cv)

      write(unit=stdout,fmt=*)
      write(unit=stdout,fmt='(a)')' Gaussian (Normal) noise statistics:'
      write(unit=stdout,fmt='(a,f15.5)')' Mean = ',mean_cv
      write(unit=stdout,fmt='(a,f15.5)')' RMS = ', rms_cv
      write(unit=stdout,fmt='(a,f15.5)')' STD DEV = ', std_dev_cv
   else
      cv = 0.0
   end if

   if (trace_use) call da_trace_exit("da_initialize_cv")

end subroutine da_initialize_cv


