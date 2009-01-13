subroutine da_scan_obs_bufr (iv, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),             intent(inout) :: iv
   character(len=*), optional, intent(in)    :: filename

#ifdef BUFR

   real, parameter              :: r8bfms = 9.0D08  ! BUFR missing value threshold
   integer, parameter           :: mxr8pm = 9       ! max number of BUFR parameters
   integer, parameter           :: mxr8lv = 255     ! max number of BUFR levels
   integer, parameter           :: mxr8vn = 10      ! max number of BUFR events
   logical                      :: match, end_of_file
   character(len=8)             :: subst2
   integer                      :: idate2, nlevels2, lv1, lv2, idate_save
   real                         :: hdr2(7)
   real                         :: pob1, pob2
   real                         :: obs2(9,255)
   ! for thinning
   real                         :: tdiff                       ! DHR
   real                         :: dlat_earth,dlon_earth,crit
   integer                      :: itt,itx,iobs,iout
   logical                      :: iuse
   

   type (multi_level_type)      :: platform
   logical                      :: outside, outside_all

   character(len=40)     :: obstr,hdstr
   character(len=8)      :: subset
   character(len=80)     :: date, dmn, obs_date
   real                  :: hdr(7)
   real                  :: pmo(2,1)
   real                  :: obs(9,255)
   real                  :: obs_time
   integer               :: iyear, imonth, iday, ihour, imin

   integer               :: iost, ndup, n, i, j, k, surface_level, report
   integer               :: iret, idate, kx, nlevels, t29
   integer               :: iunit, fm, obs_index

   if (trace_use) call da_trace_entry("da_scan_obs_bufr")

   ! open file
   !  ---------
   call da_get_unit(iunit)
   if (present(filename)) then
      open(unit   = iunit, FILE   = trim(filename), &
         iostat =  iost, form = 'unformatted', STATUS = 'OLD')
      if (iost /= 0) then
         write(unit=stdout, fmt='(/A,I3,3(2X,A)/)') &
              'error in obs input file unit ',iunit, &
              'obs filename:', trim(filename), &
              'for gts observations cannot be found or cannot be opened'
         return
      end if
   end if

   hdstr='SID XOB YOB DHR TYP ELV T29     '
   obstr='POB QOB TOB ZOB UOB VOB PWO ROB CAT '

   !--------------------------------
   ! open bufr file then check date
   !--------------------------------

   call datelen(10)
   call openbf(iunit,'IN',iunit)
   call readns(iunit,subset,idate,iret)  ! read in the next subset
   if ( iret /= 0 ) then
      write(unit=stdout, fmt='(a, i4)') &
         'return code from readns:', iret, &
         'Reach the end of obs unit: ', iunit
      call da_error(__FILE__,__LINE__, &
         (/"No BUFR observations"/))
   end if
   rewind(iunit)

   match = .true.
   outside_all = .false.
   end_of_file = .false.
   outside_all = .false.
   report = 0 ! report number in file
   reports: do while ( .not. end_of_file )
      report = report+1

      if ( match .or. outside_all ) then
         call readns(iunit,subset,idate,iret)  ! read in the next subset
         if ( iret /= 0 ) then
            write(unit=stdout, fmt='(a, i4)') &
                 'return code from readns:', iret, &
                 'Reach the end of obs unit: ', iunit
            exit reports
         end if
      end if

      call ufbint(iunit,hdr,7,1,iret,hdstr)
      
      platform % info % name(1:8) = subset
      platform % info % id        = hdstr(1:5)
      platform % info % dhr       = hdr(4)     ! difference in hour
      platform % info % elv       = hdr(6)
      platform % info % lon       = hdr(2)
      platform % info % lat       = hdr(3)

      ! Put a check on Lon and Lat
      if ( platform%info%lon >= 180.0) platform%info%lon = platform%info%lon -360.0
      !platform%info%lat = max(platform%info%lat, -89.95)
      !platform%info%lat = min(platform%info%lat,  89.95)
      ! Fix funny wind direction at Poles
      if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
         platform%info%lon = 0.0
      end if
      platform%info%lat = max(platform%info%lat, -89.95)
      platform%info%lat = min(platform%info%lat,  89.95)

      ! Restrict to a range of reports, useful for debugging

      if (report < report_start) cycle
      if (report > report_end) exit

      call da_llxy (platform%info, platform%loc, outside, outside_all)

      if (outside_all) cycle reports

      t29 = int(0.1 + hdr(7))
      kx=int(0.1+hdr(5))

      if (kx == 183) then          ! reset kx based on t29
         if (t29 .eq. 511) kx = 181
         if (t29 .eq. 512) kx = 187
         if (t29 .eq. 522) kx = 180
         if (t29 .eq. 523) kx = 180
         if (t29 .eq. 531) kx = 180
         if (t29 .eq. 561) kx = 180
         if (t29 .eq. 562) kx = 180
      end if

      call ufbint(iunit,obs,9,255,nlevels,obstr)
      if (nlevels > max_ob_levels) nlevels = max_ob_levels

      !hcl begin
      call readns(iunit,subst2,idate2,iret)
      match_check: do
         if ( iret /= 0 ) then
            end_of_file = .true.
            exit match_check
         end if
         call ufbint(iunit,hdr2,7,1,iret,hdstr)
         !hcl check if this subset and the previous one are matching mass and wind
         match = .true.
         if ( subset /= subst2 ) then
            match = .false.
            exit match_check
         end if
         do i = 1, 4   ! check SID, XOB, YOB, DHR
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
         call ufbint(iunit,obs2,9,255,nlevels2,obstr)
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

      idate_save = idate
      if ( .not. match ) then
         subset = subst2
         idate  = idate2
      end if

      ! skip some types
      !  61: Satellite soundings/retrievals/radiances
      !  66: SSM/I rain rate product
      if ( t29 == 61 .or. t29 == 66 ) then
         if ( end_of_file ) then
            exit reports        
         else                   
            cycle reports       
         end if                 
      end if

      ! check date
      write(date,'(i10)') idate_save
      write(dmn,'(i4,a1)') int(platform%info%dhr*60.0), 'm'
      call da_advance_time (date, trim(dmn), obs_date)
      if ( len_trim(obs_date) == 12 ) then
         read (obs_date,'(i4,4i2)') iyear, imonth, iday, ihour, imin
      else if ( len_trim(obs_date) == 10 ) then
         imin = 0
         read (obs_date,'(i4,3i2)') iyear, imonth, iday, ihour
      end if
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
      if (obs_time < time_slots(0) .or.  &
         obs_time >= time_slots(num_fgat_time)) then
         if ( end_of_file ) then
            exit reports        
         else                   
            cycle reports       
         end if                 
      end if

      tdiff = abs(platform%info%dhr-0.1)
      dlat_earth = platform%info%lat
      dlon_earth = platform%info%lon
      if (dlon_earth < 0.0) dlon_earth = dlon_earth + 360.0
      if (dlon_earth >= 360.0) dlon_earth = dlon_earth - 360.0
      dlat_earth = dlat_earth * deg2rad
      dlon_earth = dlon_earth * deg2rad
      !hcl end

      if ( nlevels < 1 ) then
         if ( (kx /= 164) .and. (kx /= 174) .and.   &
              (kx /= 165) .and. (kx /= 175) .and. (kx /= 74) ) cycle reports
      end if

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
               if (.not.use_soundobs .or. iv%info(sound)%ntotal == max_sound_input) cycle reports
               if (n==1) iv%info(sound)%ntotal     = iv%info(sound)%ntotal + 1
               if (n==1) iv%info(sonde_sfc)%ntotal = iv%info(sonde_sfc)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(sound,dlat_earth,dlon_earth,crit,iv%info(sound)%nlocal,itx,1,itt,iout,iuse)
                  call map2grids_conv(sonde_sfc,dlat_earth,dlon_earth,crit,iv%info(sonde_sfc)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(sound)%nlocal     = iv%info(sound)%nlocal + 1
                  iv%info(sonde_sfc)%nlocal = iv%info(sound)%nlocal
               end if
               fm = 35
            case (221) ;                   ! Pilot
               if (.not.use_pilotobs .or. iv%info(pilot)%ntotal == max_pilot_input) cycle reports
               if (n==1) iv%info(pilot)%ntotal = iv%info(pilot)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(pilot,dlat_earth,dlon_earth,crit,iv%info(pilot)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(pilot)%nlocal = iv%info(pilot)%nlocal + 1
               end if
               fm = 32
            end select

         case (41)
            ! case (130:131, 133, 230:231, 233) ; ! Airep
               if (.not.use_airepobs .or. iv%info(airep)%ntotal == max_airep_input) cycle reports
               if (n==1) iv%info(airep)%ntotal = iv%info(airep)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(airep,dlat_earth,dlon_earth,crit,iv%info(airep)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(airep)%nlocal = iv%info(airep)%nlocal + 1
               end if
               fm = 42

         case (522, 523);        ! Ships
               if (.not.use_shipsobs .or. iv%info(ships)%ntotal == max_ships_input) cycle reports
               if (n==1) iv%info(ships)%ntotal = iv%info(ships)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(ships,dlat_earth,dlon_earth,crit,iv%info(ships)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(ships)%nlocal = iv%info(ships)%nlocal + 1
               end if
               fm = 13

         case (531, 532, 561, 562) ;          ! Buoy  
               if (.not.use_buoyobs .or. iv%info(buoy)%ntotal == max_buoy_input) cycle reports
               if (n==1) iv%info(buoy)%ntotal = iv%info(buoy)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(buoy,dlat_earth,dlon_earth,crit,iv%info(buoy)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(buoy)%nlocal = iv%info(buoy)%nlocal + 1
               end if
               fm = 18

         case (511, 514)
            ! case (181, 281) ;                   ! Synop
               if (.not.use_synopobs .or. iv%info(synop)%ntotal == max_synop_input) cycle reports
               if (n==1) iv%info(synop)%ntotal = iv%info(synop)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(synop,dlat_earth,dlon_earth,crit,iv%info(synop)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(synop)%nlocal = iv%info(synop)%nlocal + 1
               end if
               fm = 12

         case (512)
            ! case (187, 287) ;                        ! Metar
               if (.not.use_metarobs .or. iv%info(metar)%ntotal == max_metar_input) cycle reports
               if (n==1) iv%info(metar)%ntotal = iv%info(metar)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(metar,dlat_earth,dlon_earth,crit,iv%info(metar)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(metar)%nlocal = iv%info(metar)%nlocal + 1
               end if
               fm = 15

         case (63)
            ! case (242:246, 252:253, 255) ;         ! Geo. CMVs
               if (.not.use_geoamvobs .or. iv%info(geoamv)%ntotal == max_geoamv_input) cycle reports
               if (n==1) iv%info(geoamv)%ntotal = iv%info(geoamv)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(geoamv,dlat_earth,dlon_earth,crit,iv%info(geoamv)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(geoamv)%nlocal = iv%info(geoamv)%nlocal + 1
               end if
               fm = 88

         case (582)
               if (.not.use_qscatobs .or. iv%info(qscat)%ntotal == max_qscat_input) cycle reports
               if (n==1) iv%info(qscat)%ntotal = iv%info(qscat)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(qscat,dlat_earth,dlon_earth,crit,iv%info(qscat)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(qscat)%nlocal = iv%info(qscat)%nlocal + 1
               end if
               fm = 281

         !hcl case (583)       ! GPS PW
         case (74)       ! GPS PW
               if (.not.use_gpspwobs .or. iv%info(gpspw)%ntotal == max_gpspw_input) cycle reports
               if (n==1) iv%info(gpspw)%ntotal = iv%info(gpspw)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(gpspw,dlat_earth,dlon_earth,crit,iv%info(gpspw)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(gpspw)%nlocal = iv%info(gpspw)%nlocal + 1
               end if
               fm = 111

         case (584)       ! GPS REF
               if (.not.use_gpsrefobs .or. iv%info(gpsref)%ntotal == max_gpsref_input) cycle reports
               if (n==1) iv%info(gpsref)%ntotal = iv%info(gpsref)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(gpsref,dlat_earth,dlon_earth,crit,iv%info(gpsref)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(gpsref)%nlocal = iv%info(gpsref)%nlocal + 1
               end if
               fm = 116

         !hcl case (71, 72)
         case (71, 72, 73, 75, 76)    ! Profiler and VADWND
               if (.not.use_profilerobs .or. iv%info(profiler)%ntotal == max_profiler_input) cycle reports
               if (n==1) iv%info(profiler)%ntotal = iv%info(profiler)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(profiler,dlat_earth,dlon_earth,crit,iv%info(profiler)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(profiler)%nlocal = iv%info(profiler)%nlocal + 1
               end if
               fm = 132
         case (571, 65)
              if (.not. use_ssmiretrievalobs .or. iv%info(ssmi_rv)%ntotal == max_ssmi_rv_input) cycle reports
               if (n==1) iv%info(ssmi_rv)%ntotal = iv%info(ssmi_rv)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(ssmi_rv,dlat_earth,dlon_earth,crit,iv%info(ssmi_rv)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(ssmi_rv)%nlocal = iv%info(ssmi_rv)%nlocal + 1
               end if
               fm = 125      ! ssmi wind speed & tpw
         case default 
            select case (kx)
            case (111 , 210)    ;         !  Tropical Cyclone Bogus
               ! Note Tropical cyclone Bougus is given type 135 in Obs-ascii
               if (.not.use_bogusobs .or. iv%info(bogus)%ntotal == max_bogus_input) cycle reports
               if (n==1) iv%info(bogus)%ntotal = iv%info(bogus)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(bogus,dlat_earth,dlon_earth,crit,iv%info(bogus)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                  iv%info(bogus)%nlocal = iv%info(bogus)%nlocal + 1
               end if
               fm = 135

            case default
               write(unit=message(1), fmt='(a, 2i12)') &
                  'unsaved obs found with kx & t29= ',kx,t29
               call da_warning(__FILE__,__LINE__,message(1:1))
            end select
         end select
         obs_index=fm_index(fm)
         iv%info(obs_index)%max_lev = max(iv%info(obs_index)%max_lev, nlevels)
      end do dup_loop   
      if ( end_of_file ) exit reports
   end do reports
   iv%info(sonde_sfc)%max_lev=1

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

