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

   cv = 0.0

   if (trace_use) call da_trace_exit("da_initialize_cv")

end subroutine da_initialize_cv


