subroutine da_write_obs_etkf(ob, iv, re)

   !-------------------------------------------------------------------------
   ! Purpose: Writes out components of iv=O-B structure.
   !-------------------------------------------------------------------------   

   implicit none

   type (y_type), intent(in)     :: ob      ! Observation structure.
   type (iv_type), intent(in)    :: iv      ! O-B structure.
   type (y_type), intent(inout)  :: re      ! residual vector.
      
   integer                       :: n, k, num_obs, ios
   integer                       :: ounit     ! Output unit           
   character(len=20)             :: filename

   if (trace_use) call da_trace_entry("da_write_obs_etkf")

   !-------------------------------------------------------------------------
   ! Fix output unit
   !-------------------------------------------------------------------------

   call da_get_unit(ounit)

#ifdef DM_PARALLEL
    write(unit=filename, fmt='(a,i3.3)') 'ob.etkf.', myproc
#else
    write(unit=filename, fmt='(a)') 'ob.etkf.000'
#endif

   open (unit=ounit,file=trim(filename),form='formatted',status='replace', &
      iostat=ios)
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open ETKF observation file"//filename/))
   end if

   ! [1] Transfer surface obs:

   if (iv%info(synop)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(synop)%nlocal
         if (iv%info(synop)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(synop)%nlocal
            if (iv%info(synop)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               if ( iv%synop(n)%u%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%synop(n)%u, iv%synop(n)%u%inv, iv%synop(n)%u%error
               end if
               if ( iv%synop(n)%v%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%synop(n)%v, iv%synop(n)%v%inv, iv%synop(n)%v%error
               end if
               if ( iv%synop(n)%t%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%synop(n)%t, iv%synop(n)%t%inv, iv%synop(n)%t%error
               end if
               if ( iv%synop(n)%p%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%synop(n)%p, iv%synop(n)%p%inv, iv%synop(n)%p%error
               end if
               if ( iv%synop(n)%q%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%synop(n)%q, iv%synop(n)%q%inv, iv%synop(n)%q%error
               end if
            end if
         end do
      end if
   end if

   ! [2] Transfer metar obs:

   if (iv%info(metar)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(metar)%nlocal
         if (iv%info(metar)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(metar)%nlocal
            if (iv%info(metar)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               if ( iv%metar(n)%u%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%metar(n)%u, iv%metar(n)%u%inv, iv%metar(n)%u%error
               end if
               if ( iv%metar(n)%v%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%metar(n)%v, iv%metar(n)%v%inv, iv%metar(n)%v%error
               end if
               if ( iv%metar(n)%t%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%metar(n)%t, iv%metar(n)%t%inv, iv%metar(n)%t%error
               end if
               if ( iv%metar(n)%p%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%metar(n)%p, iv%metar(n)%p%inv, iv%metar(n)%p%error
               end if
               if ( iv%metar(n)%q%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%metar(n)%q, iv%metar(n)%q%inv, iv%metar(n)%q%error
               end if
            end if
         end do
      end if
   end if

   ! [3] Transfer ships obs:

   if (iv%info(ships)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(ships)%nlocal
         if (iv%info(ships)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(ships)%nlocal
            if (iv%info(ships)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               if ( iv%ships(n)%u%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%ships(n)%u, iv%ships(n)%u%inv, iv%ships(n)%u%error
               end if
               if ( iv%ships(n)%v%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%ships(n)%v, iv%ships(n)%v%inv, iv%ships(n)%v%error
               end if
               if ( iv%ships(n)%t%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%ships(n)%t, iv%ships(n)%t%inv, iv%ships(n)%t%error
               end if
               if ( iv%ships(n)%p%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%ships(n)%p, iv%ships(n)%p%inv, iv%ships(n)%p%error
               end if
               if ( iv%ships(n)%q%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%ships(n)%q, iv%ships(n)%q%inv, iv%ships(n)%q%error
               end if
            end if
         end do
      end if
   end if

  ! [4.1] Transfer Geo AMVs Obs:

   if (iv%info(geoamv)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(geoamv)%nlocal
        if (iv%info(geoamv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(geoamv)%nlocal
            if (iv%info(geoamv)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               do k = 1, iv%info(geoamv)%levels(n)
                  if ( iv%geoamv(n)%u(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%geoamv(n)%u(k), iv%geoamv(n)%u(k)%inv, iv%geoamv(n)%u(k)%error
                  end if
                  if ( iv%geoamv(n)%v(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%geoamv(n)%v(k), iv%geoamv(n)%v(k)%inv, iv%geoamv(n)%v(k)%error
                  end if
               end do
            end if
         end do
      end if
   end if

  ! [4.2] Transfer Polar AMVs Obs:

   if (iv%info(polaramv)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(polaramv)%nlocal
        if (iv%info(polaramv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(polaramv)%nlocal
            if (iv%info(polaramv)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               do k = 1, iv%info(polaramv)%levels(n)
                  if ( iv%polaramv(n)%u(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%polaramv(n)%u(k), iv%polaramv(n)%u(k)%inv, iv%polaramv(n)%u(k)%error
                  end if
                  if ( iv%polaramv(n)%v(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%polaramv(n)%v(k), iv%polaramv(n)%v(k)%inv, iv%polaramv(n)%v(k)%error
                  end if
               end do
            end if
         end do
      end if
   end if

   ! [5] Transfer gpspw obs:

   if (iv%info(gpspw)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(gpspw)%nlocal
         if (iv%info(gpspw)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(gpspw)%nlocal
            if (iv%info(gpspw)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               if ( iv%gpspw(n)%tpw%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%gpspw(n)%tpw, iv%gpspw(n)%tpw%inv, iv%gpspw(n)%tpw%error
               end if
            end if
         end do
      end if
   end if

   ! [6] Transfer sonde obs:

   if (iv%info(sound)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(sound)%nlocal
        if (iv%info(sound)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(sound)%nlocal
            if (iv%info(sound)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               do k = 1, iv%info(sound)%levels(n)
                  if ( iv%sound(n)%u(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sound(n)%u(k), iv%sound(n)%u(k)%inv, iv%sound(n)%u(k)%error
                  end if
                  if ( iv%sound(n)%v(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sound(n)%v(k), iv%sound(n)%v(k)%inv, iv%sound(n)%v(k)%error
                  end if
                  if ( iv%sound(n)%t(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sound(n)%t(k), iv%sound(n)%t(k)%inv, iv%sound(n)%t(k)%error
                  end if
                  if ( iv%sound(n)%q(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sound(n)%q(k), iv%sound(n)%q(k)%inv, iv%sound(n)%q(k)%error
                  end if
               end do
            end if
         end do
      end if
   end if

   if (iv%info(sonde_sfc)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(sonde_sfc)%nlocal
        if (iv%info(sonde_sfc)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(sonde_sfc)%nlocal
            if (iv%info(sonde_sfc)%proc_domain(1,n)) then
               num_obs = num_obs + 1
                  if ( iv%sonde_sfc(n)%u%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sonde_sfc(n)%u, iv%sonde_sfc(n)%u%inv, iv%sonde_sfc(n)%u%error
                  end if
                  if ( iv%sonde_sfc(n)%v%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sonde_sfc(n)%v, iv%sonde_sfc(n)%v%inv, iv%sonde_sfc(n)%v%error
                  end if
                  if ( iv%sonde_sfc(n)%t%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sonde_sfc(n)%t, iv%sonde_sfc(n)%t%inv, iv%sonde_sfc(n)%t%error
                  end if
                  if ( iv%sonde_sfc(n)%p%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sonde_sfc(n)%p, iv%sonde_sfc(n)%p%inv, iv%sonde_sfc(n)%p%error
                  end if
                  if ( iv%sonde_sfc(n)%q%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%sonde_sfc(n)%q, iv%sonde_sfc(n)%q%inv, iv%sonde_sfc(n)%q%error
                  end if
            end if
         end do
      end if
   end if

  ! [7] Transfer airep obs:

   if (iv%info(airep)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(airep)%nlocal
        if (iv%info(airep)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(airep)%nlocal
            if (iv%info(airep)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               do k = 1, iv%info(airep)%levels(n)
                  if ( iv%airep(n)%u(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%airep(n)%u(k), iv%airep(n)%u(k)%inv, iv%airep(n)%u(k)%error
                  end if
                  if ( iv%airep(n)%v(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%airep(n)%v(k), iv%airep(n)%v(k)%inv, iv%airep(n)%v(k)%error
                  end if
                  if ( iv%airep(n)%t(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%airep(n)%t(k), iv%airep(n)%t(k)%inv, iv%airep(n)%t(k)%error
                  end if
               end do
            end if
         end do
      end if
   end if

   ! [8] Transfer pilot obs:

   if (iv%info(pilot)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(pilot)%nlocal
        if (iv%info(pilot)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(pilot)%nlocal
            if (iv%info(pilot)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               do k = 1, iv%info(pilot)%levels(n)
                  if ( iv%pilot(n)%u(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%pilot(n)%u(k), iv%pilot(n)%u(k)%inv, iv%pilot(n)%u(k)%error
                  end if
                  if ( iv%pilot(n)%v(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%pilot(n)%v(k), iv%pilot(n)%v(k)%inv, iv%pilot(n)%v(k)%error
                  end if
               end do
            end if
         end do
      end if
   end if

   ! [9] Transfer SSM/I obs:SSMI:

   if (iv%info(ssmi_rv)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(ssmi_rv)%nlocal
         if (iv%info(ssmi_rv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(ssmi_rv)%nlocal
            if (iv%info(ssmi_rv)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               if ( iv%ssmi_rv(n)%speed%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%ssmi_rv(n)%speed, iv%ssmi_rv(n)%speed%inv, &
                                          iv%ssmi_rv(n)%speed%error
               end if
               if ( iv%ssmi_rv(n)%tpw%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%ssmi_rv(n)%tpw, iv%ssmi_rv(n)%tpw%inv, &
                                          iv%ssmi_rv(n)%tpw%error
               end if
            end if
         end do
      end if
   end if

! SSM/I TB not coded.

   ! [10] Transfer satem obs:

   if (iv%info(satem)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(satem)%nlocal
        if (iv%info(satem)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(satem)%nlocal
            if (iv%info(satem)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               do k = 1, iv%info(satem)%levels(n)
                  if ( iv%satem(n)%thickness(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%satem(n)%thickness(k), iv%satem(n)%thickness(k)%inv, &
                                             iv%satem(n)%thickness(k)%error
                  end if
               end do
            end if
         end do
      end if
   end if

!  SSMT1 SSMT2 not coded.

  ! [11] Transfer scatterometer obs:

   if (iv%info(qscat)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(qscat)%nlocal
         if (iv%info(qscat)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(qscat)%nlocal
            if (iv%info(qscat)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               if ( iv%qscat(n)%u%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%qscat(n)%u, iv%qscat(n)%u%inv, iv%qscat(n)%u%error
               end if
               if ( iv%qscat(n)%v%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%qscat(n)%v, iv%qscat(n)%v%inv, iv%qscat(n)%v%error
               end if
            end if
         end do
      end if
   end if

  ! [12] Transfer profiler obs:

   if (iv%info(profiler)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(profiler)%nlocal
        if (iv%info(profiler)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(profiler)%nlocal
            if (iv%info(profiler)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               do k = 1, iv%info(profiler)%levels(n)
                  if ( iv%profiler(n)%u(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%profiler(n)%u(k), iv%profiler(n)%u(k)%inv, iv%profiler(n)%u(k)%error
                  end if
                  if ( iv%profiler(n)%v(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%profiler(n)%v(k), iv%profiler(n)%v(k)%inv, iv%profiler(n)%v(k)%error
                  end if
               end do
            end if
         end do
      end if
   end if

   ! Transfer Buoy obs:

   if (iv%info(buoy)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(buoy)%nlocal
         if (iv%info(buoy)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(buoy)%nlocal
            if (iv%info(buoy)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               if ( iv%buoy(n)%u%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%buoy(n)%u, iv%buoy(n)%u%inv, iv%buoy(n)%u%error
               end if
               if ( iv%buoy(n)%v%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%buoy(n)%v, iv%buoy(n)%v%inv, iv%buoy(n)%v%error
               end if
               if ( iv%buoy(n)%t%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%buoy(n)%t, iv%buoy(n)%t%inv, iv%buoy(n)%t%error
               end if
               if ( iv%buoy(n)%p%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%buoy(n)%p, iv%buoy(n)%p%inv, iv%buoy(n)%p%error
               end if
               if ( iv%buoy(n)%q%qc >= 0 ) then
                  write(ounit,'(3f17.7)') ob%buoy(n)%q, iv%buoy(n)%q%inv, iv%buoy(n)%q%error
               end if
            end if
         end do
      end if
   end if

   ! Transfer TC bogus obs:

   if (iv%info(bogus)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(bogus)%nlocal
        if (iv%info(bogus)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(bogus)%nlocal
            if (iv%info(bogus)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               do k = 1, iv%info(bogus)%levels(n)
                  if ( iv%bogus(n)%u(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%bogus(n)%u(k), iv%bogus(n)%u(k)%inv, iv%bogus(n)%u(k)%error
                  end if
                  if ( iv%bogus(n)%v(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%bogus(n)%v(k), iv%bogus(n)%v(k)%inv, iv%bogus(n)%v(k)%error
                  end if
                  if ( iv%bogus(n)%t(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%bogus(n)%t(k), iv%bogus(n)%t(k)%inv, iv%bogus(n)%t(k)%error
                  end if
                  if ( iv%bogus(n)%q(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%bogus(n)%q(k), iv%bogus(n)%q(k)%inv, iv%bogus(n)%q(k)%error
                  end if
               end do
            end if
         end do
      end if
   end if

   ! Transfer AIRS retrievals:

   if (iv%info(airsr)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(airsr)%nlocal
        if (iv%info(airsr)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(airsr)%nlocal
            if (iv%info(airsr)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               do k = 1, iv%info(airsr)%levels(n)
                  if ( iv%airsr(n)%t(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%airsr(n)%t(k), iv%airsr(n)%t(k)%inv, iv%airsr(n)%t(k)%error
                  end if
                  if ( iv%airsr(n)%q(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%airsr(n)%q(k), iv%airsr(n)%q(k)%inv, iv%airsr(n)%q(k)%error
                  end if
               end do
            end if
         end do
      end if
   end if

   ! Transfer gpsref obs:
 
   if (iv%info(gpsref)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(gpsref)%nlocal
        if (iv%info(gpsref)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(gpsref)%nlocal
            if (iv%info(gpsref)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               do k = 1, iv%info(gpsref)%levels(n)
                  if ( iv%gpsref(n)%ref(k)%qc >= 0 ) then
                     write(ounit,'(3f17.7)') ob%gpsref(n)%ref(k), iv%gpsref(n)%ref(k)%inv, iv%gpsref(n)%ref(k)%error
                  end if
               end do
            end if
         end do
      end if
   end if

   close (ounit)
   call da_free_unit(ounit)

   if (trace_use) call da_trace_exit("da_write_obs_etkf")

end subroutine da_write_obs_etkf

