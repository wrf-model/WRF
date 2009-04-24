subroutine da_scan_obs_bufr (iv, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),             intent(inout) :: iv
   character(len=*), optional, intent(in)    :: filename

#ifdef BUFR

   logical                      :: match, end_of_file
   character(len=8)             :: subst2, csid, csid2
   integer                      :: idate2, nlevels2, lv1, lv2
   real                         :: hdr2(7), hdr_save(7), r8sid, r8sid2
   real                         :: pob1, pob2
   real                         :: obs2(8,255), obs_save(8,255)
   equivalence                     (r8sid, csid), (r8sid2, csid2)
   ! for thinning
   real                         :: tdiff                       ! DHR
   real                         :: dlat_earth,dlon_earth,crit
   integer                      :: itt,itx,iout
   logical                      :: iuse
   

   type (multi_level_type)      :: platform
   logical                      :: outside, outside_all, outside_time

   character(len=40)     :: obstr,hdstr
   character(len=8)      :: subset
   character(len=14)     :: cdate, dmn, obs_date
   real                  :: hdr(7)
   real                  :: obs(8,255)
   real                  :: obs_time
   integer               :: iyear, imonth, iday, ihour, imin

   integer               :: iost, ndup, n, i, j, k, surface_level, num_report
   integer               :: iret, idate, kx, nlevels, t29
   integer               :: iunit, fm, obs_index
   integer               :: num_outside_all, num_outside_time, num_thinned

   if (trace_use) call da_trace_entry("da_scan_obs_bufr")

   ! open file
   !  ---------
   call da_get_unit(iunit)
   if (present(filename)) then
      call closbf(iunit)
      open(unit   = iunit, FILE   = trim(filename), &
         iostat =  iost, form = 'unformatted', STATUS = 'OLD')
      if (iost /= 0) then
         write(unit=message(1),fmt='(A,I5,A)') &
            "Error",iost," opening PREPBUFR obs file "//trim(filename)
         call da_warning(__FILE__,__LINE__,message(1:1))
         call da_free_unit(iunit)
         if (trace_use) call da_trace_exit("da_scan_obs_bufr")
         return
      end if
   end if

   hdstr='SID XOB YOB DHR TYP ELV T29     '
   obstr='POB QOB TOB ZOB UOB VOB PWO CAT '

   !--------------------------------
   ! open bufr file then check date
   !--------------------------------

   call openbf(iunit,'IN',iunit)
   call datelen(10)
   call readns(iunit,subset,idate,iret)  ! read in the next subset
   if ( iret /= 0 ) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iret," reading PREPBUFR obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      call closbf(iunit)
      call da_free_unit(iunit)
      if (trace_use) call da_trace_exit("da_scan_obs_bufr")
      return
   end if
   write(unit=message(1),fmt='(a,i10)') 'BUFR file date is: ', idate
   call da_message(message(1:1))

   num_report       = 0
   num_outside_all  = 0
   num_outside_time = 0
   num_thinned      = 0

   match        = .false.
   end_of_file  = .false.
   outside_all  = .false.
   outside_time = .false.

   reports: do while ( .not. end_of_file )

      if ( match .or. outside_all .or. outside_time ) then
         call readns(iunit,subset,idate,iret)  ! read in the next subset
         if ( iret /= 0 ) then
            write(unit=message(1),fmt='(A,I3,A,I3)') & 
               "return code from readns",iret,       &
               "reach the end of PREPBUFR obs unit",iunit
            !call da_warning(__FILE__,__LINE__,message(1:1))
            exit reports
         end if
      end if

      num_report = num_report+1

      call ufbint(iunit,hdr,7,1,iret,hdstr)
      call ufbint(iunit,obs,8,255,nlevels,obstr)
      
      r8sid = hdr(1)
      platform % info % name(1:8) = subset
      platform % info % id        = csid(1:5)
      platform % info % dhr       = hdr(4)     ! difference in hour
      platform % info % elv       = hdr(6)
      platform % info % lon       = hdr(2)
      platform % info % lat       = hdr(3)

      ! Put a check on Lon and Lat
      if ( platform%info%lon >= 180.0) platform%info%lon = platform%info%lon -360.0
      ! Fix funny wind direction at Poles
      !if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
      !   platform%info%lon = 0.0
      !end if
      platform%info%lat = max(platform%info%lat, -89.95)
      platform%info%lat = min(platform%info%lat,  89.95)

      ! Restrict to a range of reports, useful for debugging

      if (num_report < report_start) cycle reports
      if (num_report > report_end) exit reports

      call da_llxy (platform%info, platform%loc, outside, outside_all)

      if (outside_all) then
         num_outside_all = num_outside_all + 1
         if ( print_detail_obs ) then
            write(unit=stdout,fmt='(a,1x,a,2(1x,f8.3),a)')  &
               platform%info%name(1:8),platform%info%id(1:5), &
               platform%info%lat, platform%info%lon, '  -> outside_domain'
         end if
         cycle reports
      end if

      ! check date
      write(cdate,'(i10)') idate
      write(dmn,'(i4,a1)') int(platform%info%dhr*60.0), 'm'
      call da_advance_time (cdate(1:10), trim(dmn), obs_date)
      if ( obs_date(13:14) /= '00' ) then
         write(0,*) 'wrong date: ', trim(cdate), trim(dmn), trim(obs_date)
         call da_error(__FILE__,__LINE__,(/"Wrong date"/))
      else
         read (obs_date(1:12),'(i4,4i2)') iyear, imonth, iday, ihour, imin
      end if
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
      if (obs_time < time_slots(0) .or.  &
         obs_time >= time_slots(num_fgat_time)) then
         outside_time = .true.
         num_outside_time = num_outside_time + 1
         if ( print_detail_obs ) then
            write(unit=stdout,fmt='(a,1x,a,1x,a,a)')  &
               platform%info%name(1:8),platform%info%id(1:5), &
               trim(obs_date), '  -> outside_time'
         end if
         cycle reports       
      else
         outside_time = .false.
      end if

      t29 = int(0.1 + hdr(7))
      kx=int(0.1+hdr(5))

      call readns(iunit,subst2,idate2,iret)

      if ( iret /= 0 ) then
         end_of_file = .true.
      else
         match_check: do
            call ufbint(iunit,hdr2,7,1,iret,hdstr)
            ! check if this subset and the previous one are matching mass and wind
            match = .true.
            if ( subset /= subst2 ) then
               match = .false.
               exit match_check
            end if
            r8sid2 = hdr2(1)
            if ( csid /= csid2 ) then   ! check SID
               match = .false.
               exit match_check
            end if
            do i = 2, 4   ! check XOB, YOB, DHR
               if ( hdr(i) /= hdr2(i) ) then
                  match = .false.
                  exit match_check
               end if
            end do
            if ( hdr(6) /= hdr2(6) ) then   ! check ELV
               match = .false.
               exit match_check
            end if
            ! match found, merge two reports and find out the merged nlevel
            call ufbint(iunit,obs2,8,255,nlevels2,obstr)
            ! If this is a surface report, the wind subset precedes the
            ! mass subset - switch the subsets around in order to combine
            ! the surface pressure properly
            if ( kx == 280 .or. kx == 281 .or. kx == 284  .or.  &
                 kx == 287 .or. kx == 288  ) then
               obs_save = obs2
               obs2 = obs
               obs = obs_save
               hdr_save = hdr2
               hdr2 = hdr
               hdr = hdr_save
            end if
            lev_loop: do lv2 = 1, nlevels2
               do lv1 = 1, nlevels
                  pob1 = obs(1,lv1)
                  pob2 = obs2(1,lv2)
                  if ( pob1 == pob2 ) then
                     cycle lev_loop
                  else if ( (pob2 > pob1) .or. (lv1 .eq. nlevels) ) then
                     nlevels = nlevels + 1
                     cycle lev_loop
                  end if
               end do
            end do lev_loop
            exit match_check
         end do match_check

         if ( .not. match ) then
            subset = subst2
            idate  = idate2
         end if

      end if

      ! skip some types
      !  61: Satellite soundings/retrievals/radiances
      !  66: SSM/I rain rate product
      !  72: NEXTRAD VAD winds
      if ( t29 == 61 .or. t29 == 66 .or. t29 == 72 ) cycle reports       

      if (nlevels > max_ob_levels) nlevels = max_ob_levels
      if ( nlevels < 1 ) then
         if ( (kx /= 164) .and. (kx /= 174) .and.   &
              (kx /= 165) .and. (kx /= 175) .and. (kx /= 74) ) cycle reports
      end if

      tdiff = abs(platform%info%dhr-0.1)
      dlat_earth = platform%info%lat
      dlon_earth = platform%info%lon
      if (dlon_earth < 0.0) dlon_earth = dlon_earth + 360.0
      if (dlon_earth >= 360.0) dlon_earth = dlon_earth - 360.0
      dlat_earth = dlat_earth * deg2rad
      dlon_earth = dlon_earth * deg2rad

      !---------------------------------------------------------------------------
      ! This is basically converting  rh to q i
      ! Method : 
      !  if rh, temp and pr all available computes Qs otherwise sets Qs= missing
      !  if rh > 100 sets q = qs otherwise q = rh*Qs/100.0 
      ! Note: Currently da_obs_proc_station is active only for ob_format_ascii
      !      call da_obs_proc_station(platform)
      !---------------------------------------------------------------------------

      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. &
         (platform%loc%i < ids .or. platform%loc%i >= ide)) ndup= 2
      if (test_transforms) ndup = 1

      ! It is possible that logic for counting obs is incorrect for the
      ! global case with >1 MPI tasks due to obs duplication, halo, etc.
      ! TBH:  20050913
      dup_loop: do n = 1, ndup
         select case(t29)
         case (11, 12, 13, 22, 23, 31)
            select case (kx)
            case (120, 122, 132, 220, 222, 232) ;         ! Sound
               if (.not.use_soundobs) cycle reports
               if (n==1) iv%info(sound)%ntotal     = iv%info(sound)%ntotal + 1
               if (n==1) iv%info(sonde_sfc)%ntotal = iv%info(sonde_sfc)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(sound,dlat_earth,dlon_earth,crit,iv%info(sound)%nlocal,itx,1,itt,iout,iuse)
                  call map2grids_conv(sonde_sfc,dlat_earth,dlon_earth,crit,iv%info(sonde_sfc)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(sound)%nlocal     = iv%info(sound)%nlocal + 1
                  iv%info(sonde_sfc)%nlocal = iv%info(sound)%nlocal
               end if
               fm = 35
            case (221) ;                   ! Pilot
               if (.not.use_pilotobs) cycle reports
               if (n==1) iv%info(pilot)%ntotal = iv%info(pilot)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(pilot,dlat_earth,dlon_earth,crit,iv%info(pilot)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(pilot)%nlocal = iv%info(pilot)%nlocal + 1
               end if
               fm = 32
            case default
               exit dup_loop
            end select

         case (41)
            ! case (130:131, 133, 230:231, 233) ; ! Airep
               if (.not.use_airepobs) cycle reports
               if (n==1) iv%info(airep)%ntotal = iv%info(airep)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(airep,dlat_earth,dlon_earth,crit,iv%info(airep)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(airep)%nlocal = iv%info(airep)%nlocal + 1
               end if
               fm = 42

         case (522, 523);        ! Ships
               if (.not.use_shipsobs) cycle reports
               if (n==1) iv%info(ships)%ntotal = iv%info(ships)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(ships,dlat_earth,dlon_earth,crit,iv%info(ships)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(ships)%nlocal = iv%info(ships)%nlocal + 1
               end if
               fm = 13

         case (531, 532, 561, 562) ;          ! Buoy  
               if (.not.use_buoyobs) cycle reports
               if (n==1) iv%info(buoy)%ntotal = iv%info(buoy)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(buoy,dlat_earth,dlon_earth,crit,iv%info(buoy)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(buoy)%nlocal = iv%info(buoy)%nlocal + 1
               end if
               fm = 18

         case (511, 514)
            ! case (181, 281) ;                   ! Synop
               if (.not.use_synopobs) cycle reports
               if (n==1) iv%info(synop)%ntotal = iv%info(synop)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(synop,dlat_earth,dlon_earth,crit,iv%info(synop)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(synop)%nlocal = iv%info(synop)%nlocal + 1
               end if
               fm = 12

         case (512)
            ! case (187, 287) ;                        ! Metar
               if (.not.use_metarobs) cycle reports
               if (n==1) iv%info(metar)%ntotal = iv%info(metar)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(metar,dlat_earth,dlon_earth,crit,iv%info(metar)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(metar)%nlocal = iv%info(metar)%nlocal + 1
               end if
               fm = 15

         case (63)
            ! case (242:246, 252:253, 255) ;         ! Geo. CMVs
               if (.not.use_geoamvobs) cycle reports
               if (n==1) iv%info(geoamv)%ntotal = iv%info(geoamv)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(geoamv,dlat_earth,dlon_earth,crit,iv%info(geoamv)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(geoamv)%nlocal = iv%info(geoamv)%nlocal + 1
               end if
               fm = 88

         case (582, 583)      ! QuikSCAT 582 and WindSat 583
               if (.not.use_qscatobs) cycle reports
               if (n==1) iv%info(qscat)%ntotal = iv%info(qscat)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(qscat,dlat_earth,dlon_earth,crit,iv%info(qscat)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(qscat)%nlocal = iv%info(qscat)%nlocal + 1
               end if
               fm = 281

         case (74)       ! GPS PW
               if (.not.use_gpspwobs) cycle reports
               if (n==1) iv%info(gpspw)%ntotal = iv%info(gpspw)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(gpspw,dlat_earth,dlon_earth,crit,iv%info(gpspw)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(gpspw)%nlocal = iv%info(gpspw)%nlocal + 1
               end if
               fm = 111

         case (71, 73, 75, 76, 77)    ! Profiler
               if (.not.use_profilerobs) cycle reports
               if (n==1) iv%info(profiler)%ntotal = iv%info(profiler)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(profiler,dlat_earth,dlon_earth,crit,iv%info(profiler)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(profiler)%nlocal = iv%info(profiler)%nlocal + 1
               end if
               fm = 132
         case (571, 65)
              if (.not. use_ssmiretrievalobs) cycle reports
               if (n==1) iv%info(ssmi_rv)%ntotal = iv%info(ssmi_rv)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(ssmi_rv,dlat_earth,dlon_earth,crit,iv%info(ssmi_rv)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(ssmi_rv)%nlocal = iv%info(ssmi_rv)%nlocal + 1
               end if
               fm = 125      ! ssmi wind speed & tpw
         case default 
            select case (kx)
            case (111 , 210)    ;         !  Tropical Cyclone Bogus
               ! Note Tropical cyclone Bougus is given type 135 in Obs-ascii
               if (.not.use_bogusobs) cycle reports
               if (n==1) iv%info(bogus)%ntotal = iv%info(bogus)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(bogus,dlat_earth,dlon_earth,crit,iv%info(bogus)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(bogus)%nlocal = iv%info(bogus)%nlocal + 1
               end if
               fm = 135

            case default
               if ( print_detail_obs ) then
                  write(unit=message(1), fmt='(a, 2i12)') &
                     'unsaved obs found with kx & t29= ',kx,t29
                  call da_warning(__FILE__,__LINE__,message(1:1))
               end if
               exit dup_loop
            end select
         end select
         obs_index=fm_index(fm)
         iv%info(obs_index)%max_lev = max(iv%info(obs_index)%max_lev, nlevels)
      end do dup_loop   
   end do reports

   iv%info(synop)%max_lev     = 1
   iv%info(metar)%max_lev     = 1
   iv%info(ships)%max_lev     = 1
   iv%info(buoy)%max_lev      = 1
   iv%info(sonde_sfc)%max_lev = 1

   write(unit=message(1),fmt='(A,4(1x,i7))') & 
      'da_scan_obs_bufr: num_report, num_outside_all, num_outside_time, num_thinned: ', &
      num_report, num_outside_all, num_outside_time, num_thinned
   call da_message(message(1:1))

   if ( thin_conv ) then
      do n = 1, num_ob_indexes
         call cleangrids_conv(n)
      end do
   end if

   call closbf(iunit)
   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_scan_obs_bufr")
#else
   call da_error(__FILE__,__LINE__,(/"must compile with BUFR library"/))
#endif

end subroutine da_scan_obs_bufr

