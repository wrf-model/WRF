subroutine da_set_obs_missing (iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv

   integer :: n, i, k
   real    :: xmiss

   if (trace_use) call da_trace_entry("da_set_obs_missing")

   xmiss = -888.0

   do n = 1, num_ob_indexes
      do i = 1, iv%info(n)%nlocal
         do k = 1, iv%info(n)%levels(i)
            if ( .not. iv%info(n)%proc_domain(k,i) ) then
               iv%info(n)%slp(i) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               iv%info(n)%pw(i)  = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               select case (n)
               case (sound)
                  iv%sound(i)%h(k) = missing_r
                  iv%sound(i)%p(k) = missing_r
                  iv%sound(i)%u(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%sound(i)%v(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%sound(i)%t(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%sound(i)%q(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case (synop)
                  iv%synop(i)%h = missing_r
                  iv%synop(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%synop(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%synop(i)%t = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%synop(i)%p = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%synop(i)%q = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case (pilot)
                  iv%pilot(i)%p(k) = missing_r
                  iv%pilot(i)%u(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%pilot(i)%v(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(satem)
                  iv%satem(i)%ref_p        = missing_r
                  iv%satem(i)%p(k)         = missing_r
                  iv%satem(i)%thickness(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(geoamv)
                  iv%geoamv(i)%p(k) = missing_r
                  iv%geoamv(i)%u(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%geoamv(i)%v(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(polaramv)
                  iv%polaramv(i)%p(k) = missing_r
                  iv%polaramv(i)%u(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%polaramv(i)%v(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(airep)
                  iv%airep(i)%h(k) = missing_r
                  iv%airep(i)%p(k) = missing_r
                  iv%airep(i)%u(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%airep(i)%v(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%airep(i)%t(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(gpspw)
                  iv%gpspw(i)%tpw = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(gpsref)
                  iv%gpsref(i)%h(k)   = missing_r
                  iv%gpsref(i)%ref(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%gpsref(i)%p(k)   = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%gpsref(i)%t(k)   = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%gpsref(i)%q(k)   = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(metar)
                  iv%metar(i)%h = missing_r
                  iv%metar(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%metar(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%metar(i)%t = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%metar(i)%p = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%metar(i)%q = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(ships)
                  iv%ships(i)%h = missing_r
                  iv%ships(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%ships(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%ships(i)%t = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%ships(i)%p = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%ships(i)%q = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(ssmi_rv)
                  iv%ssmi_rv(i)%speed = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%ssmi_rv(i)%tpw   = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(qscat)
                  iv%qscat(i)%h = missing_r
                  iv%qscat(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%qscat(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(profiler)
                  iv%profiler(i)%p(k) = missing_r
                  iv%profiler(i)%u(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%profiler(i)%v(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(buoy)
                  iv%buoy(i)%h = missing_r
                  iv%buoy(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%buoy(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%buoy(i)%t = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%buoy(i)%p = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%buoy(i)%q = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(bogus)
                  iv%bogus(i)%h(k) = missing_r
                  iv%bogus(i)%p(k) = missing_r
                  iv%bogus(i)%u(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%bogus(i)%v(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%bogus(i)%t(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%bogus(i)%q(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%bogus(i)%slp  = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case(sonde_sfc)
                  iv%sonde_sfc(i)%h = missing_r
                  iv%sonde_sfc(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%sonde_sfc(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%sonde_sfc(i)%t = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%sonde_sfc(i)%p = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
                  iv%sonde_sfc(i)%q = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
               case default
               end select
            end if
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_set_obs_missing")
 
end subroutine da_set_obs_missing


