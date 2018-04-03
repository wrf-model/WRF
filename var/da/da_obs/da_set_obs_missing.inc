subroutine da_set_obs_missing (iv, n)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: n    ! obs index

   integer :: i, k

   if (trace_use) call da_trace_entry("da_set_obs_missing")

   do i = 1, iv%info(n)%nlocal
      if ( iv%info(n)%thinned(1,i) ) then
         iv%info(n)%slp(i) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         iv%info(n)%pw(i)  = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
      end if
   end do

   select case (n)
   case (sound)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%sound(i)%h(:) = missing_r
            iv%sound(i)%p(:) = missing_r
            iv%sound(i)%u(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%sound(i)%v(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%sound(i)%t(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%sound(i)%q(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case (synop)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%synop(i)%h = missing_r
            iv%synop(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%synop(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%synop(i)%t = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%synop(i)%p = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%synop(i)%q = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case (pilot)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%pilot(i)%h(:) = missing_r
            iv%pilot(i)%p(:) = missing_r
            iv%pilot(i)%u(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%pilot(i)%v(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(satem)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%satem(i)%ref_p        = missing_r
            iv%satem(i)%p(:)         = missing_r
            iv%satem(i)%thickness(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(geoamv)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%geoamv(i)%p(:) = missing_r
            iv%geoamv(i)%u(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%geoamv(i)%v(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(polaramv)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%polaramv(i)%p(:) = missing_r
            iv%polaramv(i)%u(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%polaramv(i)%v(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(airep)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%airep(i)%h(:) = missing_r
            iv%airep(i)%p(:) = missing_r
            iv%airep(i)%u(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%airep(i)%v(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%airep(i)%t(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%airep(i)%q(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(gpspw)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%gpspw(i)%tpw = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(gpsref)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%gpsref(i)%h(:)   = missing_r
            iv%gpsref(i)%ref(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%gpsref(i)%p(:)   = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%gpsref(i)%t(:)   = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%gpsref(i)%q(:)   = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(gpseph)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%gpseph(i)%h(:)   = missing_r
            iv%gpseph(i)%eph(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%gpseph(i)%ref(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%gpseph(i)%azim(:)= missing_r
            iv%gpseph(i)%lat(:) = missing_r
            iv%gpseph(i)%lon(:) = missing_r
         end if
      end do
   case(metar)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%metar(i)%h = missing_r
            iv%metar(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%metar(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%metar(i)%t = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%metar(i)%p = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%metar(i)%q = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(ships)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%ships(i)%h = missing_r
            iv%ships(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%ships(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%ships(i)%t = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%ships(i)%p = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%ships(i)%q = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(ssmi_rv)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%ssmi_rv(i)%speed = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%ssmi_rv(i)%tpw   = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(qscat)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%qscat(i)%h = missing_r
            iv%qscat(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%qscat(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(profiler)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%profiler(i)%h(:) = missing_r
            iv%profiler(i)%p(:) = missing_r
            iv%profiler(i)%u(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%profiler(i)%v(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(buoy)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%buoy(i)%h = missing_r
            iv%buoy(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%buoy(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%buoy(i)%t = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%buoy(i)%p = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%buoy(i)%q = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(bogus)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%bogus(i)%h(:) = missing_r
            iv%bogus(i)%p(:) = missing_r
            iv%bogus(i)%u(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%bogus(i)%v(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%bogus(i)%t(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%bogus(i)%q(:) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%bogus(i)%slp  = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(sonde_sfc)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%sonde_sfc(i)%h = missing_r
            iv%sonde_sfc(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%sonde_sfc(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%sonde_sfc(i)%t = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%sonde_sfc(i)%p = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%sonde_sfc(i)%q = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case(rain)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%rain(i)%rain = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case default
      if (trace_use) call da_trace_exit("da_set_obs_missing")
      return
   end select

   if (trace_use) call da_trace_exit("da_set_obs_missing")
 
end subroutine da_set_obs_missing


