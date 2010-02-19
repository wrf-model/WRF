subroutine da_scan_obs_ascii (iv, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Scan WRFVAR GTS observation file
   !
   !       Date: 03/19/2009 -        Y.-R. Guo
   !
   !           Added the time range check when reading in observations.
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(*),      intent(in)    :: filename

   character (len =  10)   :: fmt_name
   character (len = 160)   :: info_string
!   character (len = 160)   :: fmt_info
!   character (len = 160)   :: fmt_srfc
!   character (len = 160)   :: fmt_each

   integer                 :: i, iost, fm, report, iunit
   type (multi_level_type) :: platform
   logical                 :: outside, outside_all
   real                    :: height_error
   integer                 :: ndup, n, obs_index

   real*8                :: obs_time
   integer               :: iyear, imonth, iday, ihour, imin

   if (trace_use) call da_trace_entry("da_scan_obs_ascii")

   ! open file
   ! ---------
   call da_get_unit(iunit)
   open(unit   = iunit,     &
      FILE   = trim(filename), &
      FORM   = 'FORMATTED',  &
      ACCESS = 'SEQUENTIAL', &
      iostat =  iost,     &
      STATUS = 'OLD')

   if (iost /= 0) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iost," opening gts obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      call da_free_unit(iunit)
      if (trace_use) call da_trace_exit("da_scan_obs_ascii")
      return
   end if

   ! read header

   head_info: do
      read (unit = iunit, fmt = '(A)', iostat = iost) info_string
      if (iost /= 0) then
         write(unit=message(1),fmt='(A,I3,A,I3)') &
            "Error",iost,"reading gts obs header on unit",iunit
         call da_warning(__FILE__,__LINE__,message(1:1))
      if (trace_use) call da_trace_exit("da_scan_obs_ascii")
         return
      end if
      if (info_string(1:6) == 'EACH  ') exit
   end do head_info

   ! read formats

   read (iunit, fmt = '(A,1X,A)', iostat = iost) &
       fmt_name, fmt_info, &
       fmt_name, fmt_srfc,  &
       fmt_name, fmt_each

   if (iost /= 0) then
      write(unit=message(1),fmt='(A,I3,A,I3)') &
         "Error",iost,"reading gts obs formats on unit",iunit
         call da_warning(__FILE__,__LINE__,message(1:1))
      if (trace_use) call da_trace_exit("da_scan_obs_ascii")
      return
   end if

   ! skip units
   read (iunit, fmt = '(A)') fmt_name

   ! loop over records

   report = 0 ! report number in file

   reports: do
      report = report+1

      ! read station general info

      read (iunit, fmt = fmt_info, iostat = iost) &
         platform%info%platform,    &
         platform%info%date_char,   &
         platform%info%name,        &
         platform%info%levels,      &
         platform%info%lat,         &
         platform%info%lon,         &
         platform%info%elv,         &
         platform%info%id

      if (iost /= 0) then
         ! FIX? This is expected, but its unclear how we handle failure
         ! here without assuming the fortran2003 convention on
         ! error statuses
         exit reports
      end if

      if (print_detail_obs) then
         write(unit=stdout, fmt = fmt_info) &
            platform%info%platform,    &
            platform%info%date_char,   &
            platform%info%name,        &
            platform%info%levels,      &
            platform%info%lat,         &
            platform%info%lon,         &
            platform%info%elv,         &
            platform%info%id
      end if

      if (platform%info%lon == 180.0) platform%info%lon =-180.000
      ! WHY?
      ! Fix funny wind direction at South Poles
      ! if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
      !    platform%info%lon = 0.0
      ! end if

      read (platform%info%platform(4:6), '(I3)') fm

      ! read model location
      read (iunit, fmt = fmt_srfc)  &
         platform%loc%slp%inv, platform%loc%slp%qc, platform%loc%slp%error, &
         platform%loc%pw%inv, platform%loc%pw%qc, platform%loc%pw%error

      ! levels < 1 and .not.GPSPW and .not.GPSZTD, go back to reports

      if ((platform%info%levels < 1) .AND.            &
         ( (index(platform%info%platform, 'GPSPW') <= 0) .and. &
           (index(platform%info%platform, 'GPSZD') <= 0) )) then
         cycle reports
      end if

      ! read each level

      do i = 1, platform%info%levels
         platform%each (i) = each_level_type(missing_r, missing, -1.0, & ! height
            field_type(missing_r, missing, missing_r, missing, missing_r), & ! u
            field_type(missing_r, missing, missing_r, missing, missing_r), & ! v
            field_type(missing_r, missing, missing_r, missing, missing_r), & ! p
            field_type(missing_r, missing, missing_r, missing, missing_r), & ! t
            field_type(missing_r, missing, missing_r, missing, missing_r), & ! q
            field_type(missing_r, missing, missing_r, missing, missing_r), & ! rh
            field_type(missing_r, missing, missing_r, missing, missing_r), & ! td
            field_type(missing_r, missing, missing_r, missing, missing_r))  ! speed 

         read (unit = iunit, fmt = trim (fmt_each)) &
            platform%each(i)%p%inv, platform%each(i)%p%qc, platform%each(i)%p%error, &
            platform%each(i)%speed%inv, platform%each(i)%speed%qc, &
            platform%each(i)%speed%error, &
            ! Here the 'direction' is stored in platform%each (i)%v:
            platform%each(i)%v%inv, platform%each(i)%v%qc, platform%each(i)%v%error, &
            platform%each(i)%height,       &
            platform%each(i)%height_qc,    &
            height_error,                   &
            platform%each(i)%t%inv, platform%each(i)%t%qc, platform%each(i)%t%error, &
            platform%each(i)%td%inv, platform%each(i)%td%qc, platform%each(i)%td%error, &
            platform%each(i)%rh%inv, platform%each(i)%rh%qc, platform%each(i)%rh%error
      end do

      ! Check if outside of the time range:

      read (platform%info%date_char,'(i4,4(1x,i2))') &
                                    iyear, imonth, iday, ihour, imin
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
      if ( obs_time < time_slots(0) .or. &
           obs_time >= time_slots(num_fgat_time) ) then
           cycle
      endif

      ! Restrict to a range of reports, useful for debugging

      if (report < report_start) cycle
      if (report > report_end) exit

      call da_llxy (platform%info, platform%loc, outside, outside_all)

      if (platform%info%levels < 1) then
         if (fm /= 111 .and. fm /= 114) then
            cycle reports
         end if
      end if

      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. (platform%loc%i < ids .or. platform%loc%i >= ide)) ndup= 2

      if (test_transforms) ndup = 1
      obs_index = fm_index(fm)
      do n = 1, ndup
         select case(fm)

         case (12) ; 
            if (.not.use_synopobs .or. iv%info(synop)%ntotal == max_synop_input) cycle reports
            if (n==1) iv%info(synop)%ntotal = iv%info(synop)%ntotal + 1
            if (outside) cycle reports
            iv%info(synop)%nlocal = iv%info(synop)%nlocal + 1

         case (13, 17) ;    
            if (.not.use_shipsobs .or. iv%info(ships)%ntotal == max_ships_input) cycle reports
            if (n==1) iv%info(ships)%ntotal = iv%info(ships)%ntotal + 1
            if (outside) cycle reports
            iv%info(ships)%nlocal = iv%info(ships)%nlocal + 1

         case (15:16) ;     
            if (.not.use_metarobs .or. iv%info(metar)%ntotal == max_metar_input) cycle reports
            if (n==1) iv%info(metar)%ntotal = iv%info(metar)%ntotal + 1
            if (outside) cycle reports
            iv%info(metar)%nlocal = iv%info(metar)%nlocal + 1

         case (32:34) ;
            if (.not.use_pilotobs .or. iv%info(pilot)%ntotal == max_pilot_input) cycle reports
            if (n==1) iv%info(pilot)%ntotal = iv%info(pilot)%ntotal + 1
            if (outside) cycle reports
            iv%info(pilot)%nlocal = iv%info(pilot)%nlocal + 1

         case (35:38) ;
            if (.not.use_soundobs .or. iv%info(sound)%ntotal == max_sound_input) cycle reports
            if (n==1) iv%info(sound)%ntotal     = iv%info(sound)%ntotal + 1
            if (n==1) iv%info(sonde_sfc)%ntotal = iv%info(sonde_sfc)%ntotal + 1
            if (outside) cycle reports
            iv%info(sound)%nlocal     = iv%info(sound)%nlocal + 1
            iv%info(sonde_sfc)%nlocal = iv%info(sonde_sfc)%nlocal + 1
         case (101) ;
            if (.not.use_tamdarobs .or. iv%info(tamdar)%ntotal == max_tamdar_input) cycle reports
            if (n==1) iv%info(tamdar)%ntotal     = iv%info(tamdar)%ntotal + 1
            if (n==1) iv%info(tamdar_sfc)%ntotal     = iv%info(tamdar_sfc)%ntotal + 1
            if (outside) cycle reports
            iv%info(tamdar)%nlocal         = iv%info(tamdar)%nlocal + 1
            iv%info(tamdar_sfc)%nlocal     = iv%info(tamdar_sfc)%nlocal + 1
         case (161) ;
            if (.not.use_mtgirsobs .or. iv%info(mtgirs)%ntotal == max_mtgirs_input) cycle reports
            if (n==1) iv%info(mtgirs)%ntotal     = iv%info(mtgirs)%ntotal + 1
            if (outside) cycle reports
            iv%info(mtgirs)%nlocal     = iv%info(mtgirs)%nlocal + 1
         case (86) ;
            if (.not.use_satemobs .or. iv%info(satem)%ntotal == max_satem_input) cycle reports
            ! Reject cloudy satem obs.
            if (platform%loc%pw%inv > 10.0) then
               cycle reports
            end if
            if (n==1) iv%info(satem)%ntotal = iv%info(satem)%ntotal + 1
            if (outside) cycle reports
            iv%info(satem)%nlocal = iv%info(satem)%nlocal + 1

         case (88)    ;
            ! Geostationary or Polar orbitting Satellite AMVs:
            if (index(platform%info%name, 'MODIS') > 0 .or. &
                index(platform%info%name, 'modis') > 0)  then
               if (.not.use_polaramvobs .or. iv%info(polaramv)%ntotal == max_polaramv_input) cycle reports
               if (n==1) iv%info(polaramv)%ntotal = iv%info(polaramv)%ntotal + 1
               if (outside) cycle reports
               iv%info(polaramv)%nlocal = iv%info(polaramv)%nlocal + 1
	       obs_index = polaramv ! geoamv is the fm_index value for 88
            else
               if (.not.use_geoamvobs .or. iv%info(geoamv)%ntotal == max_geoamv_input) cycle reports
               if (n==1) iv%info(geoamv)%ntotal = iv%info(geoamv)%ntotal + 1
               if (outside) cycle reports
               iv%info(geoamv)%nlocal = iv%info(geoamv)%nlocal + 1
            end if

         case (42,96:97) ;
            if (.not.use_airepobs .or. iv%info(airep)%ntotal == max_airep_input) cycle reports
            if (n==1) iv%info(airep)%ntotal = iv%info(airep)%ntotal + 1
            if (outside) cycle reports
            iv%info(airep)%nlocal = iv%info(airep)%nlocal + 1

         case (111, 114) ;       
            if ( (.not.use_gpspwobs  .and. fm == 111) .or. &
                  iv%info(gpspw)%ntotal == max_gpspw_input) cycle reports
            if ( (.not.use_gpsztdobs  .and. fm == 114) .or. &
                  iv%info(gpspw)%ntotal == max_gpspw_input) cycle reports
            if (n==1) iv%info(gpspw)%ntotal = iv%info(gpspw)%ntotal + 1
            if (outside) cycle reports
            iv%info(gpspw)%nlocal = iv%info(gpspw)%nlocal + 1

         case (116) ;
            if (.not.use_gpsrefobs .or. iv%info(gpsref)%ntotal == max_gpsref_input) cycle reports
            if (n==1) iv%info(gpsref)%ntotal = iv%info(gpsref)%ntotal + 1
            if (outside) cycle reports
            iv%info(gpsref)%nlocal = iv%info(gpsref)%nlocal + 1

          case (121) ;
            ! SSM/T1 temperatures
            if (.not.use_ssmt1obs .or. iv%info(ssmt1)%ntotal == max_ssmt1_input) cycle reports
            if (n==1) iv%info(ssmt1)%ntotal = iv%info(ssmt1)%ntotal + 1
            if (outside) cycle reports
            iv%info(ssmt2)%nlocal = iv%info(ssmt2)%nlocal + 1

         case (122) ;
            ! SSM/T2 relative humidities:
            if (.not.use_ssmt2obs .or. iv%info(ssmt2)%ntotal == max_ssmt2_input) cycle reports
            if (n==1) iv%info(ssmt2)%ntotal = iv%info(ssmt2)%ntotal + 1
            if (outside) cycle reports
            iv%info(ssmt2)%nlocal = iv%info(ssmt2)%nlocal + 1

         case (281)    ;
            ! Scatterometer:
            if (.not.use_qscatobs .or. iv%info(qscat)%ntotal == max_qscat_input) cycle reports
            if (n==1) iv%info(qscat)%ntotal = iv%info(qscat)%ntotal + 1
            if (outside) cycle reports
            iv%info(qscat)%nlocal = iv%info(qscat)%nlocal + 1

         case (132) ;
            if (.not.use_profilerobs .or. iv%info(profiler)%ntotal == max_profiler_input) cycle reports
            if (n==1) iv%info(profiler)%ntotal = iv%info(profiler)%ntotal + 1
            if (outside) cycle reports
            iv%info(profiler)%nlocal = iv%info(profiler)%nlocal + 1

         case (135) ;
            if (.not.use_bogusobs .or. iv%info(bogus)%ntotal == max_bogus_input) cycle reports
            if (n==1) iv%info(bogus)%ntotal = iv%info(bogus)%ntotal + 1
            if (outside) cycle reports
            iv%info(bogus)%nlocal = iv%info(bogus)%nlocal + 1

         case (18,19) ;             ! buoy
            if (.not.use_buoyobs .or. iv%info(buoy)%ntotal == max_buoy_input) cycle reports
            if (n==1) iv%info(buoy)%ntotal = iv%info(buoy)%ntotal + 1
            if (outside) cycle reports
            iv%info(buoy)%nlocal = iv%info(buoy)%nlocal + 1

         case (133) ;               ! AIRS retrievals
            if (.not.use_airsretobs .or. iv%info(airsr)%ntotal == max_airsr_input) cycle reports
            if (n==1) iv%info(airsr)%ntotal = iv%info(airsr)%ntotal + 1
            if (outside) cycle reports
            iv%info(airsr)%nlocal = iv%info(airsr)%nlocal + 1

         case default;
            write(unit=message(1), fmt='(a)') 'unsaved obs found:'
            write(unit=message(2), fmt='(2a)') &
               'platform%info%platform=', platform%info%platform
            write(unit=message(3), fmt='(a, i3)') &
                 'platform%info%levels=', platform%info%levels
            call da_warning(__FILE__,__LINE__,message(1:3))
            cycle
         end select
         iv%info(obs_index)%max_lev = max(iv%info(obs_index)%max_lev, platform%info%levels)
      end do        !  loop over duplicate
   end do reports

   iv%info(sonde_sfc)%max_lev=1
   iv%info(tamdar_sfc)%max_lev=1
   iv%info(synop)%max_lev=1   ! To prevent some bad observations with more than 1 level, should I add more?
   iv%info(ships)%max_lev=1   
   iv%info(qscat)%max_lev=1   
   iv%info(metar)%max_lev=1   

   close(iunit)

   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_scan_obs_ascii")

end subroutine da_scan_obs_ascii


