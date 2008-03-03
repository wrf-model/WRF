subroutine da_scan_obs_bufr (iv, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),             intent(inout) :: iv
   character(len=*), optional, intent(in)    :: filename

#ifdef BUFR

   type (multi_level_type)      :: platform
   logical                      :: outside, outside_all
   integer                      :: nlocal(num_ob_indexes)
   integer                      :: ntotal(num_ob_indexes)

   character(len=40)     :: obstr,hdstr
   character(len=8)      :: subset
   real                  :: hdr(7)
   real                  :: pmo(2,1)
   ! real :: drf9,255)
   real                  :: obs(9,255),qms(9,255),oes(9,255)
   real                  :: time,woe,qob,toe,qoe,poe,pob,tob,zob,rob,roe

   integer               :: iost, ndup, n, i, j, k, surface_level, report
   integer               :: iret, idate, kx, nlevels, nrecs,miscd, t29, jx, ix
!   integer               :: cat,zqm,pqm,qqm,tqm,wqm,pwq,pmq,rqm
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

   nrecs = 0
   miscd = 0

   !--------------------------------
   ! open bufr file then check date
   !--------------------------------

   call datelen(10)

   call openbf(iunit,'IN',iunit)

   call readmg(iunit,subset,idate,iret)

   if (iret/=0) then
      write(unit=stdout, fmt='(a, i4)') &
           'return code from readmg:', iret, &
           'Reach the end of obs unit: ', iunit

      call da_error(__FILE__,__LINE__, &
         (/"No BUFR observations"/))
   end if

   report = 0 ! report number in file
   reports: do
      report = report+1

      call readsb(iunit,iret)

      if (iret/=0) then
         call readmg(iunit,subset,idate,iret)

         if (iret/=0) then
            write(unit=stdout, fmt='(a, i4)') &
                 'return code from readmg:', iret, &
                 'Reach the end of prepbufr obs unit: ', iunit

            exit reports
         end if

         cycle reports
      end if

      nrecs=nrecs+1

      call ufbint(iunit,hdr,7,1,iret,hdstr)
      
      platform  % info % name(1:8) = subset
      platform  % info % id        = hdstr(1:5)

      if (hdr(2) >  180.0) hdr(2)=hdr(2)-360.0

      ! Put a check on Lat
      
      hdr(3) = MAX(hdr(3), -89.95)
      hdr(3) = Min(hdr(3),  89.95)
      platform%info%lon = hdr(2)
      platform%info%lat = hdr(3)

      ! Put a check on Lat

      if (platform%info%lon == 180.0  ) platform%info%lon =-180.000
      ! Fix funny wind direction at Poles
      if (platform%info%lat <= -89.95 .or. platform%info%lat >= 89.95) then
         platform%info%lon = 0.0
      end if

      ! Restrict to a range of reports, useful for debugging

      if (report < report_start) cycle

      if (report > report_end) exit

      call da_llxy (platform%info, platform%loc,outside, outside_all)

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

      ! WHY?
      ! if ((kx >= 160) .and. (kx <= 179)) then    ! bypass satellite data
      ! if (t29 .eq. 61 .or. t29 .eq. 63 .or. t29 .ge. 571) then
      !    cycle reports
      ! end if

      ! Conventional data

      call ufbint(iunit,obs,9,255,nlevels,obstr)
      if (nlevels > max_ob_levels) nlevels = max_ob_levels

      if ((nlevels < 1) .and. ((kx /= 164) .or. (kx /= 174))) cycle reports

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
               iv%info(sound)%nlocal = iv%info(sound)%nlocal + 1
               iv%info(sonde_sfc)%nlocal = iv%info(sonde_sfc)%nlocal + 1
               fm = 35
            case (221) ;                   ! Pilot
               if (.not.use_pilotobs .or. iv%info(pilot)%ntotal == max_pilot_input) cycle reports
               if (n==1) iv%info(pilot)%ntotal = iv%info(pilot)%ntotal + 1
               if (outside) cycle reports
               iv%info(pilot)%nlocal = iv%info(pilot)%nlocal + 1
               fm = 32
            end select

         case (41)
            ! case (130:131, 133, 230:231, 233) ; ! Airep
               if (.not.use_airepobs .or. iv%info(airep)%ntotal == max_airep_input) cycle reports
               if (n==1) iv%info(airep)%ntotal = iv%info(airep)%ntotal + 1
               if (outside) cycle reports
               iv%info(airep)%nlocal = iv%info(airep)%nlocal + 1
               fm = 42

         case (522, 523);        ! Ships
               if (.not.use_shipsobs .or. iv%info(ships)%ntotal == max_ships_input) cycle reports
               if (n==1) iv%info(ships)%ntotal = iv%info(ships)%ntotal + 1
               if (outside) cycle reports
               iv%info(ships)%nlocal = iv%info(ships)%nlocal + 1
               fm = 13

         case (531, 561, 562) ;          ! Buoy  
               if (.not.use_buoyobs .or. iv%info(buoy)%ntotal == max_buoy_input) cycle reports
               if (n==1) iv%info(buoy)%ntotal = iv%info(buoy)%ntotal + 1
               if (outside) cycle reports
               iv%info(buoy)%nlocal = iv%info(buoy)%nlocal + 1
               fm = 18

         case (511)
            ! case (181, 281) ;                   ! Synop
               if (.not.use_synopobs .or. iv%info(synop)%ntotal == max_synop_input) cycle reports
               if (n==1) iv%info(synop)%ntotal = iv%info(synop)%ntotal + 1
               if (outside) cycle reports
               iv%info(synop)%nlocal = iv%info(synop)%nlocal + 1
               fm = 12

         case (512)
            ! case (187, 287) ;                        ! Metar
               if (.not.use_metarobs .or. iv%info(metar)%ntotal == max_metar_input) cycle reports
               if (n==1) iv%info(metar)%ntotal = iv%info(metar)%ntotal + 1
               if (outside) cycle reports
               iv%info(metar)%nlocal = iv%info(metar)%nlocal + 1
               fm = 15

         case (63)
            ! case (242:246, 252:253, 255) ;         ! Geo. CMVs
               if (.not.use_geoamvobs .or. iv%info(geoamv)%ntotal == max_geoamv_input) cycle reports
               if (n==1) iv%info(geoamv)%ntotal = iv%info(geoamv)%ntotal + 1
               if (outside) cycle reports
               iv%info(geoamv)%nlocal = iv%info(geoamv)%nlocal + 1
               fm = 88

         case (582)
               if (.not.use_qscatobs .or. iv%info(qscat)%ntotal == max_qscat_input) cycle reports
               if (n==1) iv%info(qscat)%ntotal = iv%info(qscat)%ntotal + 1
               if (outside) cycle reports
               iv%info(qscat)%nlocal = iv%info(qscat)%nlocal + 1
               fm = 281

         case (583)       ! GPS PW
               if (.not.use_gpspwobs .or. iv%info(gpspw)%ntotal == max_gpspw_input) cycle reports
               if (n==1) iv%info(gpspw)%ntotal = iv%info(gpspw)%ntotal + 1
               if (outside) cycle reports
               iv%info(gpspw)%nlocal = iv%info(gpspw)%nlocal + 1
               fm = 111

         case (584)       ! GPS REF
               if (.not.use_gpsrefobs .or. iv%info(gpsref)%ntotal == max_gpsref_input) cycle reports
               if (n==1) iv%info(gpsref)%ntotal = iv%info(gpsref)%ntotal + 1
               if (outside) cycle reports
               iv%info(gpsref)%nlocal = iv%info(gpsref)%nlocal + 1
               fm = 116

         case (71, 72)
               if (.not.use_profilerobs .or. iv%info(profiler)%ntotal == max_profiler_input) cycle reports
               if (n==1) iv%info(profiler)%ntotal = iv%info(profiler)%ntotal + 1
               if (outside) cycle reports
               iv%info(profiler)%nlocal = iv%info(profiler)%nlocal + 1
               fm = 132
         case (571, 65)
              if (.not. use_ssmiretrievalobs .or. iv%info(ssmi_rv)%ntotal == max_ssmi_rv_input) cycle reports
               if (n==1) iv%info(ssmi_rv)%ntotal = iv%info(ssmi_rv)%ntotal + 1
               if (outside) cycle reports
               iv%info(ssmi_rv)%nlocal = iv%info(ssmi_rv)%nlocal + 1
               fm = 125      ! ssmi wind speed & tpw
         case default 
            select case (kx)
            case (111 , 210)    ;         !  Tropical Cyclone Bogus
               ! Note Tropical cyclone Bougus is given type 135 in Obs-ascii
               if (.not.use_bogusobs .or. iv%info(bogus)%ntotal == max_bogus_input) cycle reports
               if (n==1) iv%info(bogus)%ntotal = iv%info(bogus)%ntotal + 1
               if (outside) cycle reports
               iv%info(bogus)%nlocal = iv%info(bogus)%nlocal + 1
               fm = 135

            case default
               write(unit=message(1), fmt='(a, 2i12)') &
                  'unsaved obs found with kx & t29= ',kx,t29
               !call da_warning(__FILE__,__LINE__,message(1:1))
            end select
         end select
      end do dup_loop   
         obs_index=fm_index(fm)
         iv%info(obs_index)%max_lev = max(iv%info(obs_index)%max_lev, nlevels)
   end do reports
   iv%info(sonde_sfc)%max_lev=1

   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_scan_obs_bufr")
#endif

end subroutine da_scan_obs_bufr

