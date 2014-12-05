subroutine da_search_obs (ob_type_string, unit_in, num_obs, nth, iv, found_flag)

   !-----------------------------------------------------------------------
   ! Purpose: Search obs. in gts_omb.000 
   !-----------------------------------------------------------------------

   !-------------------------------------------------------------------------
   ! read iv=O-B structure written by WRFVAR
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout)    :: iv      ! O-B structure.
   integer, intent(in)              :: unit_in, nth, num_obs
   character(len=20), intent(in)    :: ob_type_string
   logical, intent(out)             :: found_flag

   character*5  :: stn_id
   real         :: lat, lon
   integer      :: n, n_dummy, k, levels
   real, parameter :: MIN_ERR=1.0E-6

   if (trace_use) call da_trace_entry("da_search_obs")

   found_flag = .true. 
   SELECT CASE (trim(adjustl(ob_type_string)))

   CASE ('synop')

   do n = 1, num_obs
      read(unit_in,'(i8,a5,2E22.13)') n_dummy, stn_id, lat, lon
      if ( trim(iv%info(synop)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(synop)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(synop)%lon(1,nth) - lon ) < MIN_ERR ) then

         read(unit_in,'(E22.13,5(E22.13,i8,3E22.13))')&
              iv%synop(nth)%h, &
              iv%synop(nth)%u, &!  O-B u
              iv%synop(nth)%v, &!  O-B v
              iv%synop(nth)%t, &!  O-B t
              iv%synop(nth)%p, &!  O-B p
              iv%synop(nth)%q  !  O-B q
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('metar')

   do n = 1, num_obs
      read(unit_in,'(i8,a5,2E22.13)') n_dummy, stn_id, lat, lon
      if ( trim(iv%info(metar)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(metar)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(metar)%lon(1,nth) - lon ) < MIN_ERR ) then

         read(unit_in,'(E22.13,5(E22.13,i8,3E22.13))')&
              iv%metar(nth)%h, &
              iv%metar(nth)%u, &!  O-B u
              iv%metar(nth)%v, &!  O-B v
              iv%metar(nth)%t, &!  O-B t
              iv%metar(nth)%p, &!  O-B p
              iv%metar(nth)%q  !  O-B q
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('ships')

   do n = 1, num_obs
      read(unit_in,'(i8,a5,2E22.13)') n_dummy, stn_id, lat, lon
      if ( trim(iv%info(ships)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(ships)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(ships)%lon(1,nth) - lon ) < MIN_ERR ) then

         read(unit_in,'(E22.13,5(E22.13,i8,3E22.13))')&
              iv%ships(nth)%h, &
              iv%ships(nth)%u, &!  O-B u
              iv%ships(nth)%v, &!  O-B v
              iv%ships(nth)%t, &!  O-B t
              iv%ships(nth)%p, &!  O-B p
              iv%ships(nth)%q  !  O-B q
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('sonde_sfc')

   do n = 1, num_obs
      read(unit_in,'(i8,a5,2E22.13)') n_dummy, stn_id, lat, lon
      if ( trim(iv%info(sonde_sfc)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(sonde_sfc)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(sonde_sfc)%lon(1,nth) - lon ) < MIN_ERR ) then

         read(unit_in,'(E22.13,5(E22.13,i8,3E22.13))')&
              iv%sonde_sfc(nth)%h, &
              iv%sonde_sfc(nth)%u, &!  O-B u
              iv%sonde_sfc(nth)%v, &!  O-B v
              iv%sonde_sfc(nth)%t, &!  O-B t
              iv%sonde_sfc(nth)%p, &!  O-B p
              iv%sonde_sfc(nth)%q  !  O-B q
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('sound')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(sound)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(sound)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(sound)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(2E22.13,4(E22.13,i8,3E22.13))')&
                 iv % sound(nth) % h(k), &
                 iv % sound(nth) % p(k), &             ! Obs Pressure
                 iv%sound(nth)%u(k), &! O-B u
                 iv%sound(nth)%v(k), &! O-B v
                 iv%sound(nth)%t(k), &! O-B t
                 iv%sound(nth)%q(k)   ! O-B q
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('mtgirs')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(mtgirs)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(mtgirs)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(mtgirs)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(2E22.13,4(E22.13,i8,3E22.13))')&
                 iv % mtgirs(nth) % h(k), &
                 iv % mtgirs(nth) % p(k), &             ! Obs Pressure
                 iv%mtgirs(nth)%u(k), &! O-B u
                 iv%mtgirs(nth)%v(k), &! O-B v
                 iv%mtgirs(nth)%t(k), &! O-B t
                 iv%mtgirs(nth)%q(k)   ! O-B q
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('tamdar')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(tamdar)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(tamdar)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(tamdar)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(2E22.13,4(E22.13,i8,3E22.13))')&
                 iv % tamdar(nth) % h(k), &
                 iv % tamdar(nth) % p(k), &             ! Obs Pressure
                 iv%tamdar(nth)%u(k), &! O-B u
                 iv%tamdar(nth)%v(k), &! O-B v
                 iv%tamdar(nth)%t(k), &! O-B t
                 iv%tamdar(nth)%q(k)   ! O-B q
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('tamdar_sfc')

   do n = 1, num_obs
      read(unit_in,'(i8,a5,2E22.13)') n_dummy, stn_id, lat, lon
      if ( trim(iv%info(tamdar_sfc)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(tamdar_sfc)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(tamdar_sfc)%lon(1,nth) - lon ) < MIN_ERR ) then

         read(unit_in,'(E22.13,5(E22.13,i8,3E22.13))')&
              iv%tamdar_sfc(nth)%h, &
              iv%tamdar_sfc(nth)%u, &!  O-B u
              iv%tamdar_sfc(nth)%v, &!  O-B v
              iv%tamdar_sfc(nth)%t, &!  O-B t
              iv%tamdar_sfc(nth)%p, &!  O-B p
              iv%tamdar_sfc(nth)%q  !  O-B q
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('buoy')

   do n = 1, num_obs
      read(unit_in,'(i8,a5,2E22.13)') n_dummy, stn_id, lat, lon
      if ( trim(iv%info(buoy)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(buoy)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(buoy)%lon(1,nth) - lon ) < MIN_ERR ) then

         read(unit_in,'(E22.13,5(E22.13,i8,3E22.13))')&
              iv%buoy(nth)%h, &
              iv%buoy(nth)%u, &!  O-B u
              iv%buoy(nth)%v, &!  O-B v
              iv%buoy(nth)%t, &!  O-B t
              iv%buoy(nth)%p, &!  O-B p
              iv%buoy(nth)%q  !  O-B q
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('geoamv')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(geoamv)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(geoamv)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(geoamv)%lon(1,nth) - lon ) < MIN_ERR ) then
         do k = 1, levels
            read(unit_in,'(E22.13,2(E22.13,i8,3E22.13))')&
                 iv % geoamv(nth) % p(k), &                ! Obs Pressure
                 iv%geoamv(nth)%u(k), &! O-B u
                 iv%geoamv(nth)%v(k)
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('gpspw')

   do n = 1, num_obs
      read(unit_in,'(i8,a5,2E22.13)') n_dummy, stn_id, lat, lon
      if ( trim(iv%info(gpspw)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(gpspw)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(gpspw)%lon(1,nth) - lon ) < MIN_ERR ) then

         read(unit_in,'(E22.13,i8,3E22.13)')&
              iv%gpspw(nth)%tpw
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('radar')

   do n = 1, num_obs
      read(unit_in,'(2i8,2E22.13)') n_dummy, levels, lat, lon

      if ( abs(iv%info(radar)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(radar)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(E22.13,i8,3E22.13)')&
                 iv%radar(nth)%rv(k) 
         enddo

         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('ssmir')

   do n = 1, num_obs
      read(unit_in,'(i8,2E22.13)') n_dummy, lat, lon
      if ( abs(iv%info(ssmi_rv)%lat(1,nth) - lat ) < MIN_ERR .and. &
           abs(iv%info(ssmi_rv)%lon(1,nth) - lon ) < MIN_ERR ) then

         read(unit_in,'(2(E22.13,i8,3E22.13))')&
              iv%ssmi_rv(nth)%speed, & ! O-B speed
              iv%ssmi_rv(nth)%tpw ! O-BA tpw
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('airep')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
 
      if ( trim(iv%info(airep)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(airep)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(airep)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(2E22.13,4(E22.13,i8,3E22.13))')&
                 iv % airep(nth) % h(k), &
                 iv % airep(nth) % p(k), &             ! Obs pressure
                 iv%airep(nth)%u(k), &! O-B u
                 iv%airep(nth)%v(k), &! O-B v
                 iv%airep(nth)%t(k), &! 
                 iv%airep(nth)%q(k)
         enddo

         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('polaramv')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(polaramv)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(polaramv)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(polaramv)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(E22.13,2(E22.13,i8,3E22.13))')&
                 iv % polaramv(nth) % p(k), &                ! Obs Pressure
                 iv%polaramv(nth)%u(k), &! O-B u
                 iv%polaramv(nth)%v(k)
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('pilot')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(pilot)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(pilot)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(pilot)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(E22.13,2(E22.13,i8,3E22.13))')&
                 iv % pilot(nth) % p(k), &                ! Obs Pressure
                 iv%pilot(nth)%u(k), &! O-B u
                 iv%pilot(nth)%v(k)
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('ssmi_tb')

   do n = 1, num_obs
      read(unit_in,'(i8,2E22.13)') n_dummy, lat, lon
      if ( abs(iv%info(ssmi_tb)%lat(1,nth) - lat ) < MIN_ERR .and. &
           abs(iv%info(ssmi_tb)%lon(1,nth) - lon ) < MIN_ERR ) then

         read(unit_in,'(7(E22.13,i8,3E22.13))')&
              iv%ssmi_tb(nth)%tb19h, & ! O-B Tb19h
              iv%ssmi_tb(nth)%tb19v, & ! O-B Tb19v
              iv%ssmi_tb(nth)%tb22v, & ! O-B Tb22v
              iv%ssmi_tb(nth)%tb37h, & ! O-B Tb37h
              iv%ssmi_tb(nth)%tb37v, & ! O-B Tb37v
              iv%ssmi_tb(nth)%tb85h, & ! O-B Tb85h
              iv%ssmi_tb(nth)%tb85v    ! O-B Tb85v
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('satem')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(satem)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(satem)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(satem)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(E22.13,(E22.13,i8,3E22.13))')&
                 iv % satem(nth) % p(k), &             ! Obs Pressure
                 iv%satem(nth)%thickness(k)
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('ssmt1')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(ssmt1)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(ssmt1)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(ssmt1)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(E22.13,(E22.13,i8,3E22.13))')&
                 iv % ssmt1(nth) % h(k), &             ! Obs Pressure
                 iv%ssmt1(nth)%t(k)
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('ssmt2')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(ssmt2)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(ssmt2)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(ssmt2)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(E22.13,(E22.13,i8,3E22.13))')&
                 iv % ssmt2(nth) % h(k), &             ! Obs Pressure
                 iv%ssmt2(nth)%rh(k)
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('qscat')

   do n = 1, num_obs
      read(unit_in,'(i8,a5,2E22.13)') n_dummy, stn_id, lat, lon
      if ( trim(iv%info(qscat)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(qscat)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(qscat)%lon(1,nth) - lon ) < MIN_ERR ) then

         read(unit_in,'(E22.13,2(E22.13,i8,3E22.13))')&
              iv % qscat(nth) % h, &                ! Obs height
              iv%qscat(nth)%u, &! O-B u
              iv%qscat(nth)%v   ! O-B v
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('profiler')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(profiler)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(profiler)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(profiler)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(E22.13,2(E22.13,i8,3E22.13))')&
                 iv % profiler(nth) % p(k), &             ! Obs Pressure
                 iv%profiler(nth)%u(k), &! O-B u
                 iv%profiler(nth)%v(k) ! O-B v
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('bogus')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(bogus)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(bogus)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(bogus)%lon(1,nth) - lon ) < MIN_ERR ) then

         read(unit_in,'(E22.13,i8,3E22.13)') iv%bogus(nth)%slp
         do k = 1, levels
            read(unit_in,'(2E22.13,4(E22.13,i8,3E22.13))')&
                 iv % bogus(nth) % h(k), &
                 iv % bogus(nth) % p(k), &             ! Obs Pressure
                 iv%bogus(nth)%u(k), &! O-B u
                 iv%bogus(nth)%v(k), &! O-B v
                 iv%bogus(nth)%t(k), &! O-B t
                 iv%bogus(nth)%q(k)   ! O-B q
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         read(unit_in,*)
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('airsr')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(airsr)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(airsr)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(airsr)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(E22.13,2(E22.13,i8,3E22.13))')&
                 iv % airsr(nth) % p(k), &             ! Obs Pressure
                 iv%airsr(nth)%t(k), &! O-B t
                 iv%airsr(nth)%q(k)   ! O-B q
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
        do k = 1, levels
          read(unit_in,*)
        enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE ('gpsref')

   do n = 1, num_obs
      read(unit_in,'(2i8,a5,2E22.13)') n_dummy, levels, stn_id, lat, lon
      if ( trim(iv%info(gpsref)%id(nth))  == trim(adjustl(stn_id)) .and.  &
           abs(iv%info(gpsref)%lat(1,nth) - lat ) < MIN_ERR     .and.  &
           abs(iv%info(gpsref)%lon(1,nth) - lon ) < MIN_ERR ) then

         do k = 1, levels
            read(unit_in,'(E22.13,(E22.13,i8,3E22.13))')&
                 iv % gpsref(nth) % h(k), &             ! Obs Height
                 iv%gpsref(nth)%ref(k) ! O-B ref
         enddo
         !found_flag = .true.
         rewind (unit_in)
         read(unit_in,*)
         if (trace_use) call da_trace_exit("da_search_obs")
         return
      else
         do k = 1, levels
            read(unit_in,*)
         enddo
      endif
   enddo
   !found_flag = .false.
   rewind (unit_in)
   read(unit_in,*)

   CASE default;
 
   write(unit=message(1), fmt='(a,a20,a,i3)') &
        'Got unknown obs_type string:', trim(ob_type_string),' on unit ',unit_in
   call da_error(__FILE__,__LINE__,message(1:1))

   END SELECT

   if (trace_use) call da_trace_exit("da_search_obs")
   return
end subroutine da_search_obs
