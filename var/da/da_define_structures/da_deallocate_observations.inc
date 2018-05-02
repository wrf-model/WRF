subroutine da_deallocate_observations (iv)

   !---------------------------------------------------------------------------
   ! Purpose: Deallocate components of observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout)        :: iv     ! Observation structure.
   integer   :: n

   if (trace_use) call da_trace_entry("da_deallocate_observations")

   !---------------------------------------------------------------------------
   ! [1.0] Deallocate:
   !---------------------------------------------------------------------------

   if (iv%info(sound)%nlocal > 0) then
      do n = 1, iv%info(sound)%nlocal
         if (iv%info(sound)%levels(n) > 0) then
            deallocate (iv%sound (n) % h)
            deallocate (iv%sound (n) % p)
            deallocate (iv%sound (n) % u)
            deallocate (iv%sound (n) % v)
            deallocate (iv%sound (n) % t)
            deallocate (iv%sound (n) % q)
         end if
      end do

      deallocate (iv%sound)
   end if

   if (iv%info(sonde_sfc)%nlocal > 0) deallocate (iv%sonde_sfc)
      
   if (iv%info(mtgirs)%nlocal > 0) then
      do n = 1, iv%info(mtgirs)%nlocal
         if (iv%info(mtgirs)%levels(n) > 0) then
            deallocate (iv%mtgirs (n) % h)
            deallocate (iv%mtgirs (n) % p)
            deallocate (iv%mtgirs (n) % u)
            deallocate (iv%mtgirs (n) % v)
            deallocate (iv%mtgirs (n) % t)
            deallocate (iv%mtgirs (n) % q)
         end if
      end do

      deallocate (iv%mtgirs)

   end if

   if (iv%info(tamdar)%nlocal > 0) then
      do n = 1, iv%info(tamdar)%nlocal
         if (iv%info(tamdar)%levels(n) > 0) then
            deallocate (iv%tamdar (n) % h)
            deallocate (iv%tamdar (n) % p)
            deallocate (iv%tamdar (n) % u)
            deallocate (iv%tamdar (n) % v)
            deallocate (iv%tamdar (n) % t)
            deallocate (iv%tamdar (n) % q)
         end if
      end do

      deallocate (iv%tamdar)
   end if
   if (iv%info(tamdar_sfc)%nlocal > 0) deallocate (iv%tamdar_sfc)
   if (iv%info(synop)%nlocal > 0) deallocate (iv%synop)

   if (iv%info(airep)%nlocal > 0) then
      do n = 1, iv%info(airep)%nlocal
         deallocate (iv%airep (n) % h)
         deallocate (iv%airep (n) % p)
         deallocate (iv%airep (n) % u)
         deallocate (iv%airep (n) % v)
         deallocate (iv%airep (n) % t)
         deallocate (iv%airep (n) % q)
      end do

      deallocate (iv%airep)
   end if

   if (iv%info(satem)%nlocal > 0) then
      do n = 1, iv%info(satem)%nlocal
         deallocate (iv%satem(n) % p)
         deallocate (iv%satem(n) % thickness)
         deallocate (iv%satem(n) % org_thickness)
      end do
      deallocate (iv%satem)
   end if

   if (iv%info(geoamv)%nlocal > 0) then
      do n = 1, iv%info(geoamv)%nlocal
         deallocate (iv%geoamv(n) % p)
         deallocate (iv%geoamv(n) % u)
         deallocate (iv%geoamv(n) % v)
      end do
      deallocate (iv%geoamv)
   end if


   if (iv%info(polaramv)%nlocal > 0) then
      do n = 1, iv%info(polaramv)%nlocal
         deallocate (iv%polaramv(n) % p)
         deallocate (iv%polaramv(n) % u)
         deallocate (iv%polaramv(n) % v)
      end do
      deallocate (iv%polaramv)
   end if

   if (iv%info(metar)%nlocal > 0) deallocate (iv%metar)
   if (iv%info(ships)%nlocal > 0) deallocate (iv%ships)

   if (iv%info(pilot)%nlocal > 0) then
      do n = 1, iv%info(pilot)%nlocal
         deallocate (iv%pilot (n) % h)
         deallocate (iv%pilot (n) % p)
         deallocate (iv%pilot (n) % u)
         deallocate (iv%pilot (n) % v)
      end do

      deallocate (iv%pilot)
   end if

   if (iv%info(bogus)%nlocal > 0) then
      do n = 1, iv%info(bogus)%nlocal
         deallocate (iv%bogus (n) % h)
         deallocate (iv%bogus (n) % p)
         deallocate (iv%bogus (n) % u)
         deallocate (iv%bogus (n) % v)
         deallocate (iv%bogus (n) % t)
         deallocate (iv%bogus (n) % q)
      end do

      deallocate (iv%bogus)
   end if

   if (iv%info(radar)%nlocal > 0) then
      do n = 1, iv%info(radar)%nlocal
         deallocate (iv%radar (n) % model_p)
         deallocate (iv%radar (n) % model_rho)
         deallocate (iv%radar (n) % model_qrn)
         deallocate (iv%radar (n) % height  )
         deallocate (iv%radar (n) % height_qc)
         deallocate (iv%radar (n) % rv      )
         deallocate (iv%radar (n) % rf      )
      end do

      deallocate (iv%radar)
   end if

   if (iv%info(rain)%nlocal > 0) deallocate (iv%rain)
   if (iv%info(gpspw)%nlocal > 0) deallocate (iv%gpspw)

   if (iv%info(gpsref)%nlocal > 0) then
      do n = 1, iv%info(gpsref)%nlocal
         deallocate (iv%gpsref(n) %  h)
         deallocate (iv%gpsref(n) % ref)
         deallocate (iv%gpsref(n) %   p)
         deallocate (iv%gpsref(n) %   t)
         deallocate (iv%gpsref(n) %   q)
      end do
      deallocate (iv%gpsref)
   end if

   if (iv%info(gpseph)%nlocal > 0) then
      do n = 1, iv%info(gpseph)%nlocal
         if ( iv%info(gpseph)%levels(n) > 0 ) then
            deallocate (iv%gpseph(n) %  h)
            deallocate (iv%gpseph(n) % eph)
            deallocate (iv%gpseph(n) % ref)
            deallocate (iv%gpseph(n) %azim)
            deallocate (iv%gpseph(n) % lat)
            deallocate (iv%gpseph(n) % lon)
         end if
      end do
      deallocate (iv%gpseph)
   end if

   if (iv%info(ssmi_tb)%nlocal > 0) deallocate (iv%ssmi_tb)
   if (iv%info(ssmi_rv)%nlocal > 0) deallocate (iv%ssmi_rv)

   if (iv%info(ssmt1)%nlocal > 0) then
      do n = 1, iv%info(ssmt1)%nlocal
         deallocate (iv%ssmt1(n) % h)
         deallocate (iv%ssmt1(n) % p)
         deallocate (iv%ssmt1(n) % t)
      end do
   
      deallocate (iv%ssmt1)
   end if
   
   if (iv%info(ssmt2)%nlocal > 0) then
      do n = 1, iv%info(ssmt2)%nlocal
         deallocate (iv%ssmt2(n) % h)
         deallocate (iv%ssmt2(n) % p)
         deallocate (iv%ssmt2(n) % rh)
      end do
   
      deallocate (iv%ssmt2)
   end if

   if (iv%info(qscat)%nlocal > 0) deallocate (iv%qscat)

   if (iv%info(profiler)%nlocal > 0) then
      do n = 1, iv%info(profiler)%nlocal
         deallocate (iv%profiler(n)%h)
         deallocate (iv%profiler(n)%p)
         deallocate (iv%profiler(n)%u)
         deallocate (iv%profiler(n)%v)
      end do

      deallocate(iv%profiler)
   end if

   if (iv%info(buoy)%nlocal     > 0) deallocate(iv%buoy)

   if (iv%info(airsr)%nlocal > 0) then
      do n = 1, iv%info(airsr)%nlocal
         deallocate (iv%airsr (n) % h)
         deallocate (iv%airsr (n) % p)
         deallocate (iv%airsr (n) % t)
         deallocate (iv%airsr (n) % q)
      end do

      deallocate (iv%airsr)
   end if

   do n = 1, num_ob_indexes
      if (n .ne. radiance .and. iv%info(n)%nlocal > 0) then
         deallocate (iv%info(n)%name)
         deallocate (iv%info(n)%platform)
         deallocate (iv%info(n)%id)
         deallocate (iv%info(n)%date_char)
         deallocate (iv%info(n)%levels)
         deallocate (iv%info(n)%lat)
         deallocate (iv%info(n)%lon)
         deallocate (iv%info(n)%elv)
         if ( n /= radar ) then
            deallocate (iv%info(n)%pstar)
            deallocate (iv%info(n)%slp)
            deallocate (iv%info(n)%pw)
         end if
         deallocate (iv%info(n)%x)
         deallocate (iv%info(n)%y)
         deallocate (iv%info(n)%i)
         deallocate (iv%info(n)%j)
         deallocate (iv%info(n)%dx)
         deallocate (iv%info(n)%dxm)
         deallocate (iv%info(n)%dy)
         deallocate (iv%info(n)%dym)
         deallocate (iv%info(n)%k)
         deallocate (iv%info(n)%dz)
         deallocate (iv%info(n)%dzm)
         deallocate (iv%info(n)%zk)
         deallocate (iv%info(n)%proc_domain)
         deallocate (iv%info(n)%thinned)
         deallocate (iv%info(n)%obs_global_index)
      end if
   end do

   if (trace_use) call da_trace_exit("da_deallocate_observations")

end subroutine da_deallocate_observations


