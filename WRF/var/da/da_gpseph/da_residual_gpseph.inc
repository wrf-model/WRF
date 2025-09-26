subroutine da_residual_gpseph(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residiual for gpseph obs
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type),intent(inout) :: iv     ! Innovation vector (O-B).
   type (y_type) ,intent(in)    :: y      ! y = H (xa)
   type (y_type) ,intent(inout) :: re     ! Residual vector (O-A).

   integer       ,intent(inout) :: np_available
   integer       ,intent(inout) :: np_obs_used
   integer       ,intent(inout) :: np_missing
   integer       ,intent(inout) :: np_bad_data

   type (bad_data_type) :: n_obs_bad
   integer              :: n, k
!
   real                 :: constant0, g_lat, g_height, weight1, weight2, &
                           gpseph_org

   if (trace_use_dull) call da_trace_entry("da_residual_gpseph")

! Assuming the highest latitude is 90.0 degree:
   constant0= sin(radian * 90.0)

   n_obs_bad % gpseph % num = number_type(0, 0, 0)

   do n=1, iv%info(gpseph)%nlocal
      do k=1, iv%info(gpseph)%levels(n)
         np_available = np_available + 1
!
! Weighted the GPSEPH innovation with the latitude:
         if (iv%gpseph(n)%eph(k)%qc >= obs_qc_pointer ) then

! depend on the height: above 7km, set to 1.0, below 7km, decrease to 0.0:
            g_height = iv%gpseph(n)% h(k)
            weight1 = 1.0 - (7000.0 - g_height) / 7000.0
            if ( weight1 > 1.0 ) weight1 = 1.0
! not depend on height:
            weight1 = 1.0

! depend on the latitude, at 90 degree, weight = 1.0, at 0 degree, weight = 0.0
            g_lat    = iv%info(gpseph)%lat(k,n)
            weight2  = abs(sin(radian * g_lat) / constant0)
! not depend on the latitude:
            weight2   = 1.0

            gpseph_org = iv%gpseph(n)%eph(k)%inv
            iv%gpseph(n)%eph(k)%inv = gpseph_org * weight1 * weight2
         endif
!.............................................................
         if (iv%gpseph(n)%eph(k)%qc >= obs_qc_pointer ) then
            re%gpseph(n)%eph(k) = &
               da_residual(n, k, y%gpseph(n)%eph(k), iv%gpseph(n)%eph(k), n_obs_bad%gpseph)
         endif
!
         if (iv%gpseph(n)%eph(k)%qc >= obs_qc_pointer ) &
           iv%gpseph(n)%eph(k)%inv = gpseph_org

      end do
   end do

   np_missing  = np_missing  + n_obs_bad % gpseph % num % miss
   np_bad_data = np_bad_data + n_obs_bad % gpseph % num % bad
   np_obs_used = np_obs_used + n_obs_bad % gpseph % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_gpseph")

end subroutine da_residual_gpseph
