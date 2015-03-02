subroutine da_residual_radar(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals for radar obs
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual vector (O-A).

   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type) :: n_obs_bad
   integer              :: n, k

   if (trace_use) call da_trace_entry("da_residual_radar")

   n_obs_bad % rv % num = number_type(0, 0, 0)
   n_obs_bad % rf % num = number_type(0, 0, 0)
   n_obs_bad % rrn % num = number_type(0, 0, 0)
   n_obs_bad % rsn % num = number_type(0, 0, 0)
   n_obs_bad % rgr % num = number_type(0, 0, 0)
   n_obs_bad % rqv % num = number_type(0, 0, 0)

   do n=1, iv%info(radar)%nlocal
      do k=1, iv%info(radar)%levels(n)
         if (use_radar_rv) then
            np_available = np_available + 1
            re%radar(n)%rv(k) = da_residual(n, k, y%radar(n)%rv(k), iv%radar(n)%rv(k), n_obs_bad % rv)
         end if

         if (use_radar_rf) then
            np_available = np_available + 1
            if (.not. use_radar_rhv) then
               re%radar(n)%rf(k) = da_residual(n, k, y%radar(n)%rf(k), iv%radar(n)%rf(k), n_obs_bad % rf)
            end if
         end if

        if (.not.use_radar_rf .and. use_radar_rhv) then
           re%radar(n)%rrn(k) = da_residual(n, k, y%radar(n)%rrn(k), iv%radar(n)%rrn(k), n_obs_bad % rrn)
           re%radar(n)%rsn(k) = da_residual(n, k, y%radar(n)%rsn(k), iv%radar(n)%rsn(k), n_obs_bad % rsn)
           re%radar(n)%rgr(k) = da_residual(n, k, y%radar(n)%rgr(k), iv%radar(n)%rgr(k), n_obs_bad % rgr)
        end if

        if (use_radar_rqv) then
           re%radar(n)%rqv(k) = da_residual(n, k, y%radar(n)%rqv(k), iv%radar(n)%rqv(k), n_obs_bad % rqv)
        end if
      end do
   end do

   np_missing  = np_missing  + n_obs_bad % rv % num % miss + n_obs_bad % rf % num % miss + &
                               n_obs_bad % rrn% num % miss + n_obs_bad % rsn% num % miss + &
                               n_obs_bad % rqv% num % miss + n_obs_bad % rqv% num % miss 
   np_bad_data = np_bad_data + n_obs_bad % rv % num % bad  + n_obs_bad % rf % num % bad  + &
                               n_obs_bad % rrn% num % bad  + n_obs_bad % rsn% num % bad  + &
                               n_obs_bad % rgr% num % bad  + n_obs_bad % rqv% num % bad
   np_obs_used = np_obs_used + n_obs_bad % rv % num % use  + n_obs_bad % rf % num % use  + &
                               n_obs_bad % rrn% num % use  + n_obs_bad % rsn% num % use  + &
                               n_obs_bad % rgr% num % use  + n_obs_bad % rqv% num % use 

   if (trace_use) call da_trace_exit("da_residual_radar")

end subroutine da_residual_radar


