subroutine da_residual_gpsref(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residiual for gpsref obs
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type),intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) ,intent(in)    :: y      ! y = H (xa)
   type (y_type) ,intent(inout) :: re     ! Residual vector (O-A).

   integer       ,intent(inout) :: np_available
   integer       ,intent(inout) :: np_obs_used
   integer       ,intent(inout) :: np_missing
   integer       ,intent(inout) :: np_bad_data

   type (bad_data_type) :: n_obs_bad
   integer              :: n, k

   if (trace_use_dull) call da_trace_entry("da_residual_gpsref")

   n_obs_bad % gpsref % num = number_type(0, 0, 0)

   do n=1, iv%info(gpsref)%nlocal
      do k=1, iv%info(gpsref)%levels(n)
         np_available = np_available + 1
         re%gpsref(n)%ref(k) = &
            da_residual(n, k, y%gpsref(n)%ref(k), iv%gpsref(n)%ref(k), n_obs_bad%gpsref)
      end do
   end do

   np_missing  = np_missing  + n_obs_bad % gpsref % num % miss
   np_bad_data = np_bad_data + n_obs_bad % gpsref % num % bad
   np_obs_used = np_obs_used + n_obs_bad % gpsref % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_gpsref")

   
end subroutine da_residual_gpsref


