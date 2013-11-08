subroutine da_read_obs_ascii (iv, filename, uvq_direct)

   !---------------------------------------------------------------------------
   ! Purpose: Read a GTS observation file
   !
   ! Modifications:      10/26/2004            Syed RH Rizvi
   !
   !              Fix Obs-Long as -180 if it is +180
   !
   !      Date:  03/04/2005 -       Syed  RH  Rizvi
   !
   !      (a)  AMVs from Geostationary and Polar orbiting satellite are
   !           seperated & used as profile. Currently there is a provision
   !           to use MODIS winds only.
   !      (b)  MODIS obs error are currently assigned through namelist
   !           parameter (modis_cmv_error)
   !
   !                     03/30/2005           Syed RH Rizvi
   !              For global option duplicate obs at East/West boundary
   !                        
   !                     08/30/2005           Syed RH Rizvi
   !           Writing original errors & thicknesses
   !           desired for outputting QC obs with NTMAX = 0
   !
   !       Date: 02/28/2008 -        Y.-R. Guo
   !
   !           Satem thickness error should not be divided by gravity because
   !           the unit from obsproc is already meter, not geopotential meter.
   !
   !       Date: 03/19/2009 -        Y.-R. Guo
   !
   !           Added the time range check when reading in observations.
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(len=*),  intent(in)    :: filename

   character (len =  10)        :: fmt_name

   character (len = 160)        :: info_string

!   character (len = 160)        :: fmt_info, fmt_srfc, fmt_each

   integer                      :: i, j, iost, nlevels, old_nlevels, fm,iunit

   type (multi_level_type)      :: platform

   logical                      :: outside
   logical                      :: outside_all
   logical                      :: uvq_direct               ![cys_add]
   integer                      :: surface_level
   real                         :: height_error, u_comp, v_comp, sp, dir
   integer                      :: nlocal(num_ob_indexes)
   integer                      :: ntotal(num_ob_indexes)

   integer                      :: ndup, n, report, obs_index

   real*8                :: obs_time
   integer               :: iyear, imonth, iday, ihour, imin

   if (trace_use) call da_trace_entry("da_read_obs_ascii")

   ! Local counts of obs, initialise from end of last file

   nlocal(:) = iv%info(:)%plocal(iv%time-1)
   ntotal(:) = iv%info(:)%ptotal(iv%time-1)

   ! open file
   ! =========

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
      if (trace_use) call da_trace_exit("da_read_obs_ascii")
      return
   end if

   ! read header
   ! ===========

   do
      read (unit = iunit, fmt = '(A)', iostat = iost) info_string
      if (iost /= 0) then
         call da_warning(__FILE__,__LINE__, &
            (/"Problem reading gts obs header, skipping file"/))
         if (trace_use) call da_trace_exit("da_read_obs_ascii")
         return
      end if

      if (info_string(1:6) == 'EACH  ') exit
   end do

   !  read formats
   !  ------------

   read (iunit, fmt = '(A,1X,A)', iostat = iost) &
      fmt_name, fmt_info, &
      fmt_name, fmt_srfc,  &
      fmt_name, fmt_each

   if (iost /= 0) then
      call da_warning(__FILE__,__LINE__, &
         (/"Problem reading gts obs file, skipping"/))
         if (trace_use) call da_trace_exit("da_read_obs_ascii")
      return
   end if

   if (print_detail_obs) then
      write(unit=stdout, fmt='(2a)') &
         'fmt_info=', fmt_info, &
         'fmt_srfc =', fmt_srfc,  &
         'fmt_each=', fmt_each
   end if

   ! skip line
   ! ----------

   ![cys_change]
   ! read (iunit, fmt = '(a)') fmt_name
   read (iunit, fmt = '(a)') info_string
   if (info_string(1:21) == 'MODIFIED FILTERED OBS') then
      uvq_direct=.true.
   else
      uvq_direct=.false.
   endif

   ! loop over records
   ! -----------------

   report = 0 ! report number in file

   reports: &
   do

      report = report+1

      ! read station general info
      ! =============================

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
         write(unit=stdout, fmt=fmt_info) &
            platform%info%platform,    &
            platform%info%date_char,   &
            platform%info%name,        &
            platform%info%levels,      &
            platform%info%lat,         &
            platform%info%lon,         &
            platform%info%elv,         &
            platform%info%id
      end if

      if (platform%info%lon == 180.0  ) platform%info%lon =-180.000 
      ! Fix funny wind direction at Poles
      if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
         platform%info%lon = 0.0
      end if

      if (platform%info%platform(6:6) == ' ') then
         read(platform%info%platform(4:5), '(I2)') fm
      else
         read(platform%info%platform(4:6), '(I3)') fm
      end if

      ! read model location
      ! =========================

      read (iunit, fmt = fmt_srfc)  &
         platform%loc%slp%inv, platform%loc%slp%qc, platform%loc%slp%error, &
         platform%loc%pw%inv, platform%loc%pw%qc, platform%loc%pw%error

      ! levels < 1 and .not.GPSPW and .not.GPSZD, go back to reports

      if ((platform%info%levels < 1) .AND.   &
         ( (index(platform%info%platform, 'GPSPW') <= 0) .and. &
           (index(platform%info%platform, 'GPSZD') <= 0) )) then
         cycle reports
      end if

      ! read each level
      ! ---------------

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
            height_error,                  &
            platform%each(i)%t%inv, platform%each(i)%t%qc, platform%each(i)%t%error, &
            platform%each(i)%td%inv, platform%each(i)%td%qc, platform%each(i)%td%error, &
            platform%each(i)%rh%inv, platform%each(i)%rh%qc, platform%each(i)%rh%error

! cys_add
         if (uvq_direct) then
            platform%each (i)%u = platform%each (i)%speed
            if (platform%each(i)%rh%inv .gt. missing_r)  &
               platform%each(i)%rh%inv=platform%each(i)%rh%inv / 1e3    !convert back to kg/kg
            if (platform%each(i)%rh%error .gt. 0.0)  &
               platform%each(i)%rh%error=platform%each(i)%rh%error / 1e3  !convert back to kg/kg
         end if
! cys_add

         if (print_detail_obs) then
            write(unit=stdout, fmt = '(a, i6)') 'i=', i
! because now the field_type included: inv, qc, error, sens, and imp (YRG, 02/23/2009):              
            write(unit=stdout, fmt = trim (fmt_each)) &
               platform%each(i)%p%inv, platform%each(i)%p%qc, platform%each(i)%p%error, &
               platform%each(i)%speed%inv, platform%each(i)%speed%qc, &
               platform%each(i)%speed%error,  &
               platform%each(i)%v%inv, platform%each(i)%v%qc, platform%each(i)%v%error, &
               platform%each(i)%height,       &
               platform%each(i)%height_qc,    &
               height_error,                  &
               platform%each(i)%t%inv, platform%each(i)%t%qc, platform%each(i)%t%error, &
               platform%each(i)%td%inv, platform%each(i)%td%qc, platform%each(i)%td%error, &
               platform%each(i)%rh%inv, platform%each(i)%rh%qc, platform%each(i)%rh%error
         end if

         ! Uncomment the following if errors in reported heights (test later):             
         ! if (fm == 97 .or. fm == 96 .or. fm == 42) then
         !    platform%each (i)%height_qc = -5 ! Aircraft heights are not above surface.
         ! end if

         ! To convert the wind speed and direction to 
         !      the model wind components: u, v

         if (.not. uvq_direct) then   !cys_add
         if (platform%each (i)%speed%qc /= missing_data .and. &
             platform%each (i)%v%qc /= missing_data) then
             if (.not. var_wind) then
                 call da_ffdduv (platform%each (i)%speed%inv, platform%each (i)%v%inv,     &
                                 U_comp, V_comp, platform%info%lon, convert_fd2uv)
                 platform%each (i)%u = platform%each (i)%speed
                 platform%each (i)%v = platform%each (i)%speed
                 platform%each (i)%u%inv = U_comp
                 platform%each (i)%v%inv = V_comp
             else
                 call da_ffdduv (platform%each (i)%speed%inv, platform%each (i)%v%inv,     &
                                 U_comp, V_comp, platform%info%lon, convert_fd2uv)
                 call da_ffdduv2 (sp, dir, U_comp, V_comp, platform%info%lon, convert_uv2fd)
                 platform%each (i)%u = platform%each (i)%speed
                 platform%each (i)%u%inv = sp
                 platform%each (i)%v%inv = dir
             end if
         else
            platform%each (i)%u%inv   = missing_r
            platform%each (i)%v%inv   = missing_r
            platform%each (i)%u%error = missing_r
            platform%each (i)%v%error = missing_r
            platform%each (i)%u%qc    = missing_data
            platform%each (i)%v%qc    = missing_data
         end if
         end if   !cys_add
      end do

      ! Check if outside of the time range:

      read (platform%info%date_char,'(i4,4(1x,i2))') &
                                    iyear, imonth, iday, ihour, imin
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
      if ( obs_time < time_slots(0) .or. &
           obs_time >= time_slots(num_fgat_time) ) then
         if (print_detail_obs) then
           write(unit=stdout, fmt='(a)') '*** Outside of the time range:'
           write(unit=stdout, fmt=fmt_info) &
            platform%info%platform,    &
            platform%info%date_char,   &
            platform%info%name,        &
            platform%info%levels,      &
            platform%info%lat,         &
            platform%info%lon,         &
            platform%info%elv,         &
            platform%info%id
         end if
         cycle
      endif

      ! Restrict to a range of reports, useful for debugging

      if (report < report_start) then
         cycle
      end if

      if (report > report_end) then
         exit
      end if

      call da_llxy (platform%info, platform%loc, outside, outside_all)

      if (outside_all) then
         cycle reports
      end if

      if (print_detail_obs) then
         ! Simplistic approach, could be improved to get it all done on PE 0
         if (.NOT. outside) then
            write(unit=stdout,fmt='(A,I5,A,F8.2,A,F8.2,A,I3,A,2F8.2)') &
               "Report",report," at",platform%info%lon,"E",platform%info%lat, &
               "N on processor", myproc,", position", platform%loc%x,platform%loc%y
         end if
      end if

      nlevels = platform%info%levels
      if (nlevels > max_ob_levels) then
         nlevels = max_ob_levels
         write(unit=message(1), fmt='(/4(2a,2x),a,2f8.2,2x,2(a,f9.2,2x),2(a,i4,2x)/)') &
            'Station: ', trim(platform%info%name), &
            'ID: ', trim(platform%info%id), &
            'Platform: ', trim(platform%info%platform), &
            'Date: ', trim(platform%info%date_char), &
            'At Loc(lat, lon): ', platform%info%lat, platform%info%lon, &
            'At elv: ', platform%info%elv, &
            'with pstar: ', platform%info%pstar, &
            'Has level: ', platform%info%levels, &
            'which is great than max_ob_levels: ', max_ob_levels

         write (unit=message(2), fmt = '(A,1X,A,1X,A,1X,I4,2f9.3,f9.1,1X,A)') &
            platform%info%name,        &
            platform%info%platform,    &
            platform%info%date_char,   &
            platform%info%levels,      &
            platform%info%lat,         &
            platform%info%lon,         &
            platform%info%elv,         &
            platform%info%id
         call da_warning(__FILE__,__LINE__,message(1:2))

         platform%info%levels = nlevels
      else if (nlevels < 1) then
         ! Not GPSPW and GPSZD data:
         if (fm /= 111 .and. fm /= 114) then
            cycle reports
         end if
      end if

      if (fm /= 86) call da_obs_proc_station(platform,fm,uvq_direct)  !cys_change

      nlevels = platform%info%levels
      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. (platform%loc%i < ids .or. platform%loc%i >= ide)) ndup= 2

      ! It is possible that logic for counting obs is incorrect for the 
      ! global case with >1 MPI tasks due to obs duplication, halo, etc.  
      ! TBH:  20050913

      if (.not.outside) then
         if (print_detail_obs .and. ndup > 1) then
            write(unit=stdout, fmt = fmt_info) &
               platform%info%platform,    &
               platform%info%date_char,   &
               platform%info%name,        &
               platform%info%levels,      &
               platform%info%lat,         &
               platform%info%lon,         &
               platform%info%elv,         &
               platform%info%id

            write(unit=stdout, fmt = '(a,2i5,4e20.10)') &
               ' duplicating obs since loc% i,j,dx,dxm,dy & dym ', &
               platform%loc%i,  platform%loc%j,   &
               platform%loc%dx, platform%loc%dxm, &
              platform%loc%dy, platform%loc%dym
         end if
      end if
      
      obs_index = fm_index(fm)

      dup_loop: do n = 1, ndup
         select case(fm)

         case (12) ;
            if (.not. use_synopobs) cycle reports
            if (n==1) ntotal(synop) = ntotal(synop)+1
            if (outside) cycle reports
            nlocal(synop) = nlocal(synop) + 1
            if (nlocal(synop) > iv%info(synop)%nlocal) cycle reports

            iv%synop(nlocal(synop))%h = platform%each(1)%height
            iv%synop(nlocal(synop))%u = platform%each(1)%u
            iv%synop(nlocal(synop))%v = platform%each(1)%v
            iv%synop(nlocal(synop))%t = platform%each(1)%t
            iv%synop(nlocal(synop))%q = platform%each(1)%q
            iv%synop(nlocal(synop))%p = platform%each(1)%p

         case (13, 17) ;                  ! ships 
            if (.not. use_shipsobs) cycle reports
            if (n==1) ntotal(ships) = ntotal(ships) + 1 
            if (outside) cycle reports
            nlocal(ships) = nlocal(ships) + 1
            if (nlocal(ships) > iv%info(ships)%nlocal) cycle reports

            iv%ships(nlocal(ships))%h = platform%each(1)%height
            iv%ships(nlocal(ships))%u = platform%each(1)%u
            iv%ships(nlocal(ships))%v = platform%each(1)%v
            iv%ships(nlocal(ships))%t = platform%each(1)%t
            iv%ships(nlocal(ships))%p = platform%each(1)%p
            iv%ships(nlocal(ships))%q = platform%each(1)%q

         case (15:16) ;
            if (.not. use_metarobs) cycle reports
            if (n==1) ntotal(metar) = ntotal(metar) + 1 
            if (outside) cycle reports
            nlocal(metar) = nlocal(metar) + 1
            if (nlocal(metar) > iv%info(metar)%nlocal) cycle reports

            iv%metar(nlocal(metar))%h = platform%each(1)%height
            iv%metar(nlocal(metar))%u = platform%each(1)%u
            iv%metar(nlocal(metar))%v = platform%each(1)%v
            iv%metar(nlocal(metar))%t = platform%each(1)%t
            iv%metar(nlocal(metar))%p = platform%each(1)%p
            iv%metar(nlocal(metar))%q = platform%each(1)%q

         case (32:34) ;
            if (.not. use_pilotobs) cycle reports
            if (n==1) ntotal(pilot) = ntotal(pilot) + 1 
            if (outside) cycle reports
            nlocal(pilot) = nlocal(pilot) + 1
            if (nlocal(pilot) > iv%info(pilot)%nlocal) cycle reports

            allocate (iv%pilot(nlocal(pilot))%h (1:nlevels))
            allocate (iv%pilot(nlocal(pilot))%p (1:nlevels))
            allocate (iv%pilot(nlocal(pilot))%u (1:nlevels))
            allocate (iv%pilot(nlocal(pilot))%v (1:nlevels))

            do i = 1, nlevels
              iv%pilot(nlocal(pilot))%h(i) = platform%each(i)%height
              iv%pilot(nlocal(pilot))%p(i) = platform%each(i)%p%inv
              iv%pilot(nlocal(pilot))%u(i) = platform%each(i)%u
              iv%pilot(nlocal(pilot))%v(i) = platform%each(i)%v
            end do

         case (35:38) ;
            if (.not. use_soundobs) cycle reports
            if (n==1) ntotal(sound) = ntotal(sound) + 1 
            if (n==1) ntotal(sonde_sfc) = ntotal(sonde_sfc) + 1 
            if (outside) cycle reports
            nlocal(sound) = nlocal(sound) + 1
            nlocal(sonde_sfc) = nlocal(sonde_sfc) + 1
            if (nlocal(sound) > iv%info(sound)%nlocal) cycle reports

            ! Search to see if we have surface obs.

            surface_level = 0

            do i = 1, nlevels
               ! if (elevation and height are the same, it is surface.)
               if (platform%info%elv.ne.missing_r.and.platform%each(i)%height.ne.missing_r)then
                  if (abs(platform%info%elv - platform%each(i)%height) < 0.1) then
                     surface_level = i

                     ! Save surface pressure.
                     iv%sonde_sfc(nlocal(sonde_sfc))%h = platform%each(i)%height
                     iv%sonde_sfc(nlocal(sonde_sfc))%u = platform%each(i)%u
                     iv%sonde_sfc(nlocal(sonde_sfc))%v = platform%each(i)%v
                     iv%sonde_sfc(nlocal(sonde_sfc))%t = platform%each(i)%t
                     iv%sonde_sfc(nlocal(sonde_sfc))%q = platform%each(i)%q
                     iv%sonde_sfc(nlocal(sonde_sfc))%p = platform%each(i)%p

                     exit
                  end if
               end if
            end do

            ! processing the sonde_sfc data:

            old_nlevels = nlevels

            if (surface_level > 0) then
               nlevels = nlevels - 1
            else
               iv%sonde_sfc(nlocal(sonde_sfc))%h = missing_r
               iv%sonde_sfc(nlocal(sonde_sfc))%u%inv   = missing_r
               iv%sonde_sfc(nlocal(sonde_sfc))%u%qc    = missing
               iv%sonde_sfc(nlocal(sonde_sfc))%u%error = abs(missing_r)
               iv%sonde_sfc(nlocal(sonde_sfc))%v = iv%sonde_sfc(nlocal(sonde_sfc))%u
               iv%sonde_sfc(nlocal(sonde_sfc))%t = iv%sonde_sfc(nlocal(sonde_sfc))%u
               iv%sonde_sfc(nlocal(sonde_sfc))%p = iv%sonde_sfc(nlocal(sonde_sfc))%u
               iv%sonde_sfc(nlocal(sonde_sfc))%q = iv%sonde_sfc(nlocal(sonde_sfc))%u
            end if

            if (nlevels > 0) then

               allocate (iv%sound(nlocal(sound))%h (1:nlevels))
               allocate (iv%sound(nlocal(sound))%p (1:nlevels))
               allocate (iv%sound(nlocal(sound))%u (1:nlevels))
               allocate (iv%sound(nlocal(sound))%v (1:nlevels))
               allocate (iv%sound(nlocal(sound))%t (1:nlevels))
               allocate (iv%sound(nlocal(sound))%q (1:nlevels))

               j = 0
!               do i = 1, iv%sound(nlocal(sound))%info%levels
               do i = 1, old_nlevels
                  if (i == surface_level) cycle

                  j=j+1

                  iv%sound(nlocal(sound))%h(j) = platform%each(i)%height
                  iv%sound(nlocal(sound))%p(j) = platform%each(i)%p%inv
                  iv%sound(nlocal(sound))%u(j) = platform%each(i)%u
                  iv%sound(nlocal(sound))%v(j) = platform%each(i)%v
                  iv%sound(nlocal(sound))%t(j) = platform%each(i)%t
                  iv%sound(nlocal(sound))%q(j) = platform%each(i)%q
               end do
            end if

!            iv%sound(nlocal(sound))%info%levels = nlevels
         case (101) ;
            if (.not. use_tamdarobs) cycle reports
            if (n==1) ntotal(tamdar) = ntotal(tamdar) + 1
            if (n==1) ntotal(tamdar_sfc) = ntotal(tamdar_sfc) + 1
            if (outside) cycle reports
            nlocal(tamdar) = nlocal(tamdar) + 1
            nlocal(tamdar_sfc) = nlocal(tamdar_sfc) + 1
            if (nlocal(tamdar) > iv%info(tamdar)%nlocal) cycle reports
            ! Search to see if we have surface obs.

            surface_level = 0

            do i = 1, nlevels
               ! if (elevation and height are the same, it is surface.)
               if (platform%info%elv.ne.missing_r.and.platform%each(i)%height.ne.missing_r)then
                  if (abs(platform%info%elv - platform%each(i)%height) < 0.1) then
                     surface_level = i

                     ! Save surface pressure.
                     iv%tamdar_sfc(nlocal(tamdar_sfc))%h = platform%each(i)%height
                     iv%tamdar_sfc(nlocal(tamdar_sfc))%u = platform%each(i)%u
                     iv%tamdar_sfc(nlocal(tamdar_sfc))%v = platform%each(i)%v
                     iv%tamdar_sfc(nlocal(tamdar_sfc))%t = platform%each(i)%t
                     iv%tamdar_sfc(nlocal(tamdar_sfc))%q = platform%each(i)%q
                     iv%tamdar_sfc(nlocal(tamdar_sfc))%p = platform%each(i)%p
                     exit
                  end if
               end if
            end do
            ! processing the tamdar_sfc data:

            old_nlevels = nlevels

           if (surface_level > 0) then
               nlevels = nlevels - 1
            else
               iv%tamdar_sfc(nlocal(tamdar_sfc))%h = missing_r
               iv%tamdar_sfc(nlocal(tamdar_sfc))%u%inv   = missing_r
               iv%tamdar_sfc(nlocal(tamdar_sfc))%u%qc    = missing
               iv%tamdar_sfc(nlocal(tamdar_sfc))%u%error = abs(missing_r)
               iv%tamdar_sfc(nlocal(tamdar_sfc))%v = iv%tamdar_sfc(nlocal(tamdar_sfc))%u
               iv%tamdar_sfc(nlocal(tamdar_sfc))%t = iv%tamdar_sfc(nlocal(tamdar_sfc))%u
               iv%tamdar_sfc(nlocal(tamdar_sfc))%p = iv%tamdar_sfc(nlocal(tamdar_sfc))%u
               iv%tamdar_sfc(nlocal(tamdar_sfc))%q = iv%tamdar_sfc(nlocal(tamdar_sfc))%u
            end if

            if (nlevels > 0) then

               allocate (iv%tamdar(nlocal(tamdar))%h (1:nlevels))
               allocate (iv%tamdar(nlocal(tamdar))%p (1:nlevels))
               allocate (iv%tamdar(nlocal(tamdar))%u (1:nlevels))
               allocate (iv%tamdar(nlocal(tamdar))%v (1:nlevels))
               allocate (iv%tamdar(nlocal(tamdar))%t (1:nlevels))
               allocate (iv%tamdar(nlocal(tamdar))%q (1:nlevels))

               j = 0
               do i = 1, nlevels

                  j=j+1

                  iv%tamdar(nlocal(tamdar))%h(j) = platform%each(i)%height
                  iv%tamdar(nlocal(tamdar))%p(j) = platform%each(i)%p%inv
                  iv%tamdar(nlocal(tamdar))%u(j) = platform%each(i)%u
                  iv%tamdar(nlocal(tamdar))%v(j) = platform%each(i)%v
                  iv%tamdar(nlocal(tamdar))%t(j) = platform%each(i)%t
                  iv%tamdar(nlocal(tamdar))%q(j) = platform%each(i)%q
                 if(iv%tamdar(nlocal(tamdar))%q(j)%inv.ne.missing_r)then
                 iv%tamdar(nlocal(tamdar))%q(j)%inv = iv%tamdar(nlocal(tamdar))%q(j)%inv
                 endif
                 if(iv%tamdar(nlocal(tamdar))%q(j)%error.ne.missing_r)then
                 iv%tamdar(nlocal(tamdar))%q(j)%error = iv%tamdar(nlocal(tamdar))%q(j)%error
                 endif

               end do
            else
               nlevels = 1
               allocate (iv%tamdar(nlocal(tamdar))%h (1:nlevels))
               allocate (iv%tamdar(nlocal(tamdar))%p (1:nlevels))
               allocate (iv%tamdar(nlocal(tamdar))%u (1:nlevels))
               allocate (iv%tamdar(nlocal(tamdar))%v (1:nlevels))
               allocate (iv%tamdar(nlocal(tamdar))%t (1:nlevels))
               allocate (iv%tamdar(nlocal(tamdar))%q (1:nlevels))
               j=1
               iv%tamdar(nlocal(tamdar))%h = missing_r
               iv%tamdar(nlocal(tamdar))%p = missing_r
               iv%tamdar(nlocal(tamdar))%u%inv   = missing_r
               iv%tamdar(nlocal(tamdar))%u%qc    = missing
               iv%tamdar(nlocal(tamdar))%u%error = abs(missing_r)
               iv%tamdar(nlocal(tamdar))%v = iv%tamdar(nlocal(tamdar))%u
               iv%tamdar(nlocal(tamdar))%t = iv%tamdar(nlocal(tamdar))%u
               iv%tamdar(nlocal(tamdar))%q = iv%tamdar(nlocal(tamdar))%u

            end if
      
         case (161) ;
            if (.not. use_mtgirsobs) cycle reports
            if (n==1) ntotal(mtgirs) = ntotal(mtgirs) + 1
            if (outside) cycle reports
            nlocal(mtgirs) = nlocal(mtgirs) + 1
            if (nlocal(mtgirs) > iv%info(mtgirs)%nlocal) cycle reports

            if (nlevels > 0) then

               allocate (iv%mtgirs(nlocal(mtgirs))%h (1:nlevels))
               allocate (iv%mtgirs(nlocal(mtgirs))%p (1:nlevels))
               allocate (iv%mtgirs(nlocal(mtgirs))%u (1:nlevels))
               allocate (iv%mtgirs(nlocal(mtgirs))%v (1:nlevels))
               allocate (iv%mtgirs(nlocal(mtgirs))%t (1:nlevels))
               allocate (iv%mtgirs(nlocal(mtgirs))%q (1:nlevels))

               j = 0
               do i = 1, nlevels 

                  j=j+1

                  iv%mtgirs(nlocal(mtgirs))%h(j) = platform%each(i)%height
                  iv%mtgirs(nlocal(mtgirs))%p(j) = platform%each(i)%p%inv
                  iv%mtgirs(nlocal(mtgirs))%u(j) = platform%each(i)%u
                  iv%mtgirs(nlocal(mtgirs))%v(j) = platform%each(i)%v
                  iv%mtgirs(nlocal(mtgirs))%t(j) = platform%each(i)%t
                  iv%mtgirs(nlocal(mtgirs))%q(j) = platform%each(i)%q
                 if(iv%mtgirs(nlocal(mtgirs))%q(j)%inv.ne.missing_r)then
                 iv%mtgirs(nlocal(mtgirs))%q(j)%inv = iv%mtgirs(nlocal(mtgirs))%q(j)%inv/1000.0
                 endif
                 if(iv%mtgirs(nlocal(mtgirs))%q(j)%error.ne.missing_r)then
                 iv%mtgirs(nlocal(mtgirs))%q(j)%error = iv%mtgirs(nlocal(mtgirs))%q(j)%error/1000.0/100.0
                 endif

               end do
            end if


         case (86)    ;
            ! Reject cloudy satem obs.
            if (.not. use_satemobs .or. platform%loc%pw%inv > 10.0) cycle reports
            if (n==1) ntotal(satem) = ntotal(satem) + 1 
            if (outside) cycle reports
            nlocal(satem) = nlocal(satem) + 1
            if (nlocal(satem) > iv%info(satem)%nlocal) cycle reports

            ! The satem ref_p is put in the SLP position in OBSPROC data stream.

            iv%satem(nlocal(satem))%ref_p= platform%loc%slp%inv

            allocate (iv%satem(nlocal(satem))%p        (1:nlevels))
            allocate (iv%satem(nlocal(satem))%thickness(1:nlevels))
            allocate (iv%satem(nlocal(satem))%org_thickness(1:nlevels))

            iv%satem(nlocal(satem))%p(1) = platform%each(1)%p%inv
            iv%satem(nlocal(satem))%thickness(1) = platform%each(1)%t
            !  write original thickness errors for filtered obs
            if (anal_type_qcobs) then
               do i = 1, nlevels
                  iv%satem(nlocal(satem))%org_thickness(i) = platform%each(i)%t
               end do 
            end if

            ! Splitting the reported satem data into smaller layers.

            do i = 2, nlevels
               iv%satem(nlocal(satem))%p(i) = platform%each(i)%p%inv
               iv%satem(nlocal(satem))%thickness(i) = platform%each(i)%t
               if (platform%each(i)%t%qc /= missing_data   .and. &
                  platform%each(i-1)%t%qc /= missing_data) then
                  iv%satem(nlocal(satem))%thickness(i)%inv =            &
                  platform%each(i)%t%inv - platform%each(i-1)%t%inv
               else
                  iv%satem(nlocal(satem))%thickness(i)%inv = missing_r
                  iv%satem(nlocal(satem))%thickness(i)%qc  = missing_data
               end if
            end do

            ! Thickness error (m):

            do i = nlevels, 2, -1
               iv%satem(nlocal(satem))%thickness(i)%error = &
               sqrt(iv%satem(nlocal(satem))%thickness(i )%error ** 2 + &
                  iv%satem(nlocal(satem))%thickness(i-1)%error ** 2) 
!                  iv%satem(nlocal(satem))%thickness(i-1)%error ** 2) / gravity
            end do

            iv%satem(nlocal(satem))%thickness(1)%error = &
                           sqrt(iv%satem(nlocal(satem))%thickness(1)%error ** 2 + &
                           platform%loc%pw%error ** 2) 
!                           platform%loc%pw%error ** 2) / gravity

            ! Geostationary ot Polar orbitting Satellite AMVs:

         case (88)    ;
            if (index(platform%info%name, 'MODIS') > 0 .or. &
               index(platform%info%name, 'modis') > 0)  then
               if (.not. use_polaramvobs) cycle reports
               if (n==1) ntotal(polaramv) = ntotal(polaramv) + 1 
               if (outside) cycle reports
               nlocal(polaramv) = nlocal(polaramv) + 1
               if (nlocal(polaramv) > iv%info(polaramv)%nlocal) cycle reports

               allocate (iv%polaramv(nlocal(polaramv))%p (1:nlevels))
               allocate (iv%polaramv(nlocal(polaramv))%u (1:nlevels))
               allocate (iv%polaramv(nlocal(polaramv))%v (1:nlevels))

               do i = 1, nlevels
                  iv%polaramv(nlocal(polaramv))%p(i) = platform%each(i)%p%inv
                  iv%polaramv(nlocal(polaramv))%u(i) = platform%each(i)%u
                  iv%polaramv(nlocal(polaramv))%v(i) = platform%each(i)%v
               end do
	       obs_index = polaramv ! geoamv is the fm_index value for 88
            else
               if (.not. use_geoamvobs) cycle reports
               if (n==1) ntotal(geoamv) = ntotal(geoamv) + 1 
               if (outside) cycle reports
               nlocal(geoamv) = nlocal(geoamv) + 1
               if (nlocal(geoamv) > iv%info(geoamv)%nlocal) cycle reports

               allocate (iv%geoamv(nlocal(geoamv))%p (1:nlevels))
               allocate (iv%geoamv(nlocal(geoamv))%u (1:nlevels))
               allocate (iv%geoamv(nlocal(geoamv))%v (1:nlevels))

               do i = 1, nlevels
                  iv%geoamv(nlocal(geoamv))%p(i) = platform%each(i)%p%inv
                  iv%geoamv(nlocal(geoamv))%u(i) = platform%each(i)%u
                  iv%geoamv(nlocal(geoamv))%v(i) = platform%each(i)%v
               end do
            end if

         case (42,96:97) ;
            if (.not. use_airepobs) cycle reports
            if (n==1) ntotal(airep) = ntotal(airep) + 1 
            if (outside) cycle reports
            nlocal(airep) = nlocal(airep) + 1
            if (nlocal(airep) > iv%info(airep)%nlocal) cycle reports

            allocate (iv%airep(nlocal(airep))%h        (1:nlevels))
            allocate (iv%airep(nlocal(airep))%p        (1:nlevels))
            allocate (iv%airep(nlocal(airep))%u        (1:nlevels))
            allocate (iv%airep(nlocal(airep))%v        (1:nlevels))
            allocate (iv%airep(nlocal(airep))%t        (1:nlevels))

            do i = 1, nlevels
               iv%airep(nlocal(airep))%h(i) = platform%each(i)%height
               iv%airep(nlocal(airep))%p(i) = platform%each(i)%p%inv
               iv%airep(nlocal(airep))%u(i) = platform%each(i)%u
               iv%airep(nlocal(airep))%v(i) = platform%each(i)%v
               iv%airep(nlocal(airep))%t(i) = platform%each(i)%t
            end do

         case (111, 114) ;
            if(.not.use_GpspwObs  .and. fm == 111) cycle reports
            if(.not.use_GpsztdObs .and. fm == 114) cycle reports

            if (n==1) ntotal(gpspw) = ntotal(gpspw) + 1 
            if (outside) cycle reports
            nlocal(gpspw) = nlocal(gpspw) + 1
            if (nlocal(gpspw) > iv%info(gpspw)%nlocal) cycle reports

            iv%gpspw(nlocal(gpspw))%tpw  = platform%loc%pw

         case (116) ;
            if (.not. use_gpsrefobs) cycle reports
            if ( ob_format_gpsro /= ob_format_ascii ) cycle reports
            if (n==1) ntotal(gpsref) = ntotal(gpsref) + 1 
            if (outside) cycle reports
            nlocal(gpsref) = nlocal(gpsref) + 1
            if (nlocal(gpsref) > iv%info(gpsref)%nlocal) cycle reports
!
! discarded the GPSRO data above 30-km (refere to GSI, Lidia Cucurull, Nov. 2009). 
! YRG, 02/01/2010.
            do i = 1, nlevels
               if ( platform%each(i)%height> 30000.0 ) then
                  nlevels = i-1
                  exit
               endif
            enddo
!           
            allocate (iv%gpsref(nlocal(gpsref))%h (1:nlevels))
            allocate (iv%gpsref(nlocal(gpsref))%ref(1:nlevels))
            allocate (iv%gpsref(nlocal(gpsref))%  p(1:nlevels))
            allocate (iv%gpsref(nlocal(gpsref))%  t(1:nlevels))
            allocate (iv%gpsref(nlocal(gpsref))%  q(1:nlevels))

            do i = 1, nlevels
               iv%gpsref(nlocal(gpsref))%h(i)   = platform%each(i)%height

               ! In OBSPROC, use "td" field to store "gpsref"
               iv%gpsref(nlocal(gpsref))%ref(i) = platform%each(i)%td

               ! check height, only keep data below and above certain height
               if ( iv%gpsref(nlocal(gpsref))%h(i) > top_km_gpsro*1000.0 .or. &
                    iv%gpsref(nlocal(gpsref))%h(i) < bot_km_gpsro*1000.0 ) then
                  iv%gpsref(nlocal(gpsref))%ref(i)%qc = -77
               end if

               ! Keep the retrieved p and t (and q) as "field_type":   
               iv%gpsref(nlocal(gpsref))%p(i)   = platform%each(i)%p
               iv%gpsref(nlocal(gpsref))%t(i)   = platform%each(i)%t
               iv%gpsref(nlocal(gpsref))%q(i)   = platform%each(i)%q
            end do

         case (121) ;
            if (.not. use_ssmt1obs) cycle reports
            if (n==1) ntotal(ssmt1) = ntotal(ssmt1) + 1 
            if (outside) cycle reports
            nlocal(ssmt1) = nlocal(ssmt1) + 1
            if (nlocal(ssmt1) > iv%info(ssmt2)%nlocal) cycle reports

            allocate (iv%ssmt1(nlocal(ssmt1))%h (1:nlevels))
            allocate (iv%ssmt1(nlocal(ssmt1))%p (1:nlevels))
            allocate (iv%ssmt1(nlocal(ssmt1))%t (1:nlevels))

            do i = 1, nlevels
              iv%ssmt1(nlocal(ssmt1))%h(i) = platform%each(i)%height
              iv%ssmt1(nlocal(ssmt1))%p(i) = platform%each(i)%p%inv
              iv%ssmt1(nlocal(ssmt1))%t(i) = platform%each(i)%t
            end do
         
         case (122) ;
            if (.not. use_ssmt2obs) cycle reports
            if (n==1) ntotal(ssmt2) = ntotal(ssmt2) + 1
            if (outside) cycle reports
            nlocal(ssmt2) = nlocal(ssmt2) + 1
            if (nlocal(ssmt2) > iv%info(ssmt2)%nlocal) cycle reports

            allocate (iv%ssmt2(nlocal(ssmt2))%h (1:nlevels))
            allocate (iv%ssmt2(nlocal(ssmt2))%p (1:nlevels))
            allocate (iv%ssmt2(nlocal(ssmt2))%rh(1:nlevels))

            do i = 1, nlevels
               iv%ssmt2(nlocal(ssmt2))% h(i) = platform%each(i)%height
               iv%ssmt2(nlocal(ssmt2))% p(i) = platform%each(i)%p%inv
               iv%ssmt2(nlocal(ssmt2))%rh(i) = platform%each(i)%rh
            end do

         case (281)    ;
            if (.not. use_qscatobs) cycle reports
            if (n==1) ntotal(qscat) = ntotal(qscat) + 1
            if (outside) cycle reports
            nlocal(qscat) = nlocal(qscat)  + 1
            if (nlocal(qscat) > iv%info(qscat)%nlocal) cycle reports

            iv%qscat(nlocal(qscat))%h = platform%each(1)%height
            iv%qscat(nlocal(qscat))%u = platform%each(1)%u
            iv%qscat(nlocal(qscat))%v = platform%each(1)%v

            ! Impose minimum observation error = 1.0m/s for Quikscat data:
            iv%qscat(nlocal(qscat))%u%error = max(platform%each(1)%u%error,1.0)
            iv%qscat(nlocal(qscat))%v%error = max(platform%each(1)%v%error,1.0)

         case (132) ; ! profiler
            if (.not. use_profilerobs) cycle reports
            if (n==1) ntotal(profiler) = ntotal(profiler) + 1
            if (outside) cycle reports
            nlocal(profiler) = nlocal(profiler) + 1
            if (nlocal(profiler) > iv%info(profiler)%nlocal) cycle reports

            allocate (iv%profiler(nlocal(profiler))%h (1:nlevels))
            allocate (iv%profiler(nlocal(profiler))%p (1:nlevels))
            allocate (iv%profiler(nlocal(profiler))%u (1:nlevels))
            allocate (iv%profiler(nlocal(profiler))%v (1:nlevels))

            do i = 1, nlevels
               iv%profiler(nlocal(profiler))%h(i) = platform%each(i)%height
               iv%profiler(nlocal(profiler))%p(i) = platform%each(i)%p%inv
               iv%profiler(nlocal(profiler))%u(i) = platform%each(i)%u
               iv%profiler(nlocal(profiler))%v(i) = platform%each(i)%v
            end do

         case (135) ; ! Bogus
            if (.not. use_bogusobs) cycle reports
            if (n==1) ntotal(bogus) = ntotal(bogus) + 1
            if (outside) cycle reports
            nlocal(bogus) = nlocal(bogus) + 1
            if (nlocal(bogus) > iv%info(bogus)%nlocal) cycle reports

            if (nlocal(bogus) > max_bogus_input) then
               write(unit=message(1),fmt='(A,I6,A,I6)') &
                  'Bogus #=', nlocal(bogus), ' > max_bogus_input=', max_bogus_input
               call da_error(__FILE__,__LINE__,message(1:1))
            end if

            allocate (iv%bogus(nlocal(bogus))%h (1:nlevels))
            allocate (iv%bogus(nlocal(bogus))%p (1:nlevels))
            allocate (iv%bogus(nlocal(bogus))%u (1:nlevels))
            allocate (iv%bogus(nlocal(bogus))%v (1:nlevels))
            allocate (iv%bogus(nlocal(bogus))%t (1:nlevels))
            allocate (iv%bogus(nlocal(bogus))%q (1:nlevels))

            do i = 1, nlevels
               iv%bogus(nlocal(bogus))%h(i) = platform%each(i)%height
               iv%bogus(nlocal(bogus))%p(i) = platform%each(i)%p%inv
               iv%bogus(nlocal(bogus))%u(i) = platform%each(i)%u
               iv%bogus(nlocal(bogus))%v(i) = platform%each(i)%v
               iv%bogus(nlocal(bogus))%t(i) = platform%each(i)%t
               iv%bogus(nlocal(bogus))%q(i) = platform%each(i)%q
            end do

            iv%bogus(nlocal(bogus))%slp    = platform%loc%slp

            if (print_detail_obs) then
               write(unit=stdout,fmt=*) 'nlevels=', nlevels
               write(unit=stdout,fmt=*) 'iv%info(bogus)%nlocal,slp', iv%info(bogus)%nlocal,  &
                  iv % bogus (nlocal(bogus)) % slp
               do i=1,nlevels
                  write(unit=stdout,fmt=*) 'nlocal(bogus), i ', nlocal(bogus),i
                  write(unit=stdout,fmt=*) 'iv%bogus(nlocal(bogus))%u,v=',  &
                     iv%bogus(nlocal(bogus))%u(i),iv%bogus(nlocal(bogus))%v(i)
                  write(unit=stdout,fmt=*) 'iv%bogus(nlocal(bogus))%t,q=',  &
                     iv%bogus(nlocal(bogus))%t(i),iv%bogus(nlocal(bogus))%q(i)
               end do
               write(unit=stdout,fmt='(2(a,i4))') 'nlocal(bogus)=',nlocal(bogus), &
                  'nlevels=',nlevels
            end if

         case (18,19) ;             ! buoy
            if (.not. use_buoyobs) cycle reports
            if (n==1) ntotal(buoy) = ntotal(buoy) + 1
            if (outside) cycle reports
            nlocal(buoy) = nlocal(buoy)  + 1
            if (nlocal(buoy) > iv%info(buoy)%nlocal) cycle reports

            iv%buoy(nlocal(buoy))%h = platform%each(1)%height
            iv%buoy(nlocal(buoy))%u = platform%each(1)%u
            iv%buoy(nlocal(buoy))%v = platform%each(1)%v
            iv%buoy(nlocal(buoy))%t = platform%each(1)%t
            iv%buoy(nlocal(buoy))%p = platform%each(1)%p
            iv%buoy(nlocal(buoy))%q = platform%each(1)%q
   
         case (133)    ;         ! AIRS retrievals  
            if (.not. use_airsretobs) cycle reports
            if (n==1) ntotal(airsr) = ntotal(airsr) + 1
            if (outside) cycle reports
            nlocal(airsr) = nlocal(airsr) + 1
            if (nlocal(airsr) > iv%info(airsr)%nlocal) cycle reports

            allocate (iv%airsr(nlocal(airsr))%h (1:nlevels))
            allocate (iv%airsr(nlocal(airsr))%p (1:nlevels))
            allocate (iv%airsr(nlocal(airsr))%t (1:nlevels))
            allocate (iv%airsr(nlocal(airsr))%q (1:nlevels))
            do i = 1, nlevels
               iv%airsr(nlocal(airsr))%h(i) = platform%each(i)%height
               iv%airsr(nlocal(airsr))%p(i) = platform%each(i)%p%inv
               iv%airsr(nlocal(airsr))%t(i) = platform%each(i)%t
               iv%airsr(nlocal(airsr))%q(i) = platform%each(i)%q
            end do

         case default;

            write(unit=message(1), fmt='(a)') 'unsaved obs found:'
            write(unit=message(2), fmt='(2a)') &
               'platform%info%platform=', platform%info%platform
            write(unit=message(3), fmt='(a, i3)') &
               'platform%info%levels=', platform%info%levels
            call da_warning(__FILE__,__LINE__,message(1:3))
            cycle
         end select
	   
         iv%info(obs_index)%name(nlocal(obs_index))      = platform%info%name
         iv%info(obs_index)%platform(nlocal(obs_index))  = platform%info%platform
         iv%info(obs_index)%id(nlocal(obs_index))        = platform%info%id
         iv%info(obs_index)%date_char(nlocal(obs_index)) = platform%info%date_char
         ! nlevels adjusted for some obs types so use that
         iv%info(obs_index)%levels(nlocal(obs_index))    = nlevels
         iv%info(obs_index)%lat(:,nlocal(obs_index))     = platform%info%lat
         iv%info(obs_index)%lon(:,nlocal(obs_index))     = platform%info%lon
         iv%info(obs_index)%elv(nlocal(obs_index))       = platform%info%elv
         iv%info(obs_index)%pstar(nlocal(obs_index))     = platform%info%pstar

         iv%info(obs_index)%slp(nlocal(obs_index))           = platform%loc%slp
         iv%info(obs_index)%pw(nlocal(obs_index))            = platform%loc%pw
         iv%info(obs_index)%x(:,nlocal(obs_index))           = platform%loc%x
         iv%info(obs_index)%y(:,nlocal(obs_index))           = platform%loc%y 
         iv%info(obs_index)%i(:,nlocal(obs_index))           = platform%loc%i 
         iv%info(obs_index)%j(:,nlocal(obs_index))           = platform%loc%j 
         iv%info(obs_index)%dx(:,nlocal(obs_index))          = platform%loc%dx
         iv%info(obs_index)%dxm(:,nlocal(obs_index))         = platform%loc%dxm
         iv%info(obs_index)%dy(:,nlocal(obs_index))          = platform%loc%dy
         iv%info(obs_index)%dym(:,nlocal(obs_index))         = platform%loc%dym
         iv%info(obs_index)%proc_domain(:,nlocal(obs_index)) = platform%loc%proc_domain

         iv%info(obs_index)%obs_global_index(nlocal(obs_index)) = ntotal(obs_index)

         ! special case for sonde_sfc, duplicate sound info
         if (obs_index == sound) then
            iv%info(sonde_sfc)%name(nlocal(sonde_sfc))      = platform%info%name
            iv%info(sonde_sfc)%platform(nlocal(sonde_sfc))  = platform%info%platform
            iv%info(sonde_sfc)%id(nlocal(sonde_sfc))        = platform%info%id
            iv%info(sonde_sfc)%date_char(nlocal(sonde_sfc)) = platform%info%date_char
            iv%info(sonde_sfc)%levels(nlocal(sonde_sfc))    = 1
            iv%info(sonde_sfc)%lat(:,nlocal(sonde_sfc))     = platform%info%lat
            iv%info(sonde_sfc)%lon(:,nlocal(sonde_sfc))     = platform%info%lon
            iv%info(sonde_sfc)%elv(nlocal(sonde_sfc))       = platform%info%elv
            iv%info(sonde_sfc)%pstar(nlocal(sonde_sfc))     = platform%info%pstar

            iv%info(sonde_sfc)%slp(nlocal(sonde_sfc))           = platform%loc%slp
            iv%info(sonde_sfc)%pw(nlocal(sonde_sfc))            = platform%loc%pw
            iv%info(sonde_sfc)%x(:,nlocal(sonde_sfc))           = platform%loc%x
            iv%info(sonde_sfc)%y(:,nlocal(sonde_sfc))           = platform%loc%y 
            iv%info(sonde_sfc)%i(:,nlocal(sonde_sfc))           = platform%loc%i 
            iv%info(sonde_sfc)%j(:,nlocal(sonde_sfc))           = platform%loc%j 
            iv%info(sonde_sfc)%dx(:,nlocal(sonde_sfc))          = platform%loc%dx
            iv%info(sonde_sfc)%dxm(:,nlocal(sonde_sfc))         = platform%loc%dxm
            iv%info(sonde_sfc)%dy(:,nlocal(sonde_sfc))          = platform%loc%dy
            iv%info(sonde_sfc)%dym(:,nlocal(sonde_sfc))         = platform%loc%dym
            iv%info(sonde_sfc)%proc_domain(:,nlocal(sonde_sfc)) = platform%loc%proc_domain

            iv%info(sonde_sfc)%obs_global_index(nlocal(sonde_sfc)) = ntotal(sonde_sfc)
         end if

         if (obs_index == tamdar) then
            iv%info(tamdar_sfc)%name(nlocal(tamdar_sfc))      = platform%info%name
            iv%info(tamdar_sfc)%platform(nlocal(tamdar_sfc))  = platform%info%platform
            iv%info(tamdar_sfc)%id(nlocal(tamdar_sfc))        = platform%info%id
            iv%info(tamdar_sfc)%date_char(nlocal(tamdar_sfc)) = platform%info%date_char
            iv%info(tamdar_sfc)%levels(nlocal(tamdar_sfc))    = 1
            iv%info(tamdar_sfc)%lat(:,nlocal(tamdar_sfc))     = platform%info%lat
            iv%info(tamdar_sfc)%lon(:,nlocal(tamdar_sfc))     = platform%info%lon
            iv%info(tamdar_sfc)%elv(nlocal(tamdar_sfc))       = platform%info%elv
            iv%info(tamdar_sfc)%pstar(nlocal(tamdar_sfc))     = platform%info%pstar

            iv%info(tamdar_sfc)%slp(nlocal(tamdar_sfc))           = platform%loc%slp
            iv%info(tamdar_sfc)%pw(nlocal(tamdar_sfc))            = platform%loc%pw
            iv%info(tamdar_sfc)%x(:,nlocal(tamdar_sfc))           = platform%loc%x
            iv%info(tamdar_sfc)%y(:,nlocal(tamdar_sfc))           = platform%loc%y
            iv%info(tamdar_sfc)%i(:,nlocal(tamdar_sfc))           = platform%loc%i
            iv%info(tamdar_sfc)%j(:,nlocal(tamdar_sfc))           = platform%loc%j
            iv%info(tamdar_sfc)%dx(:,nlocal(tamdar_sfc))          = platform%loc%dx
            iv%info(tamdar_sfc)%dxm(:,nlocal(tamdar_sfc))         = platform%loc%dxm
            iv%info(tamdar_sfc)%dy(:,nlocal(tamdar_sfc))          = platform%loc%dy
            iv%info(tamdar_sfc)%dym(:,nlocal(tamdar_sfc))         = platform%loc%dym
            iv%info(tamdar_sfc)%proc_domain(:,nlocal(tamdar_sfc)) = platform%loc%proc_domain

            iv%info(tamdar_sfc)%obs_global_index(nlocal(tamdar_sfc)) = ntotal(tamdar_sfc)
         end if

         if (global .and. n < 2) then
            if (test_transforms) exit dup_loop
            if (platform%loc % i >= ide) then
               platform%loc%i = platform%loc % i - ide
            else if (platform%loc % i < ids) then
               platform%loc%i = platform%loc % i + ide
            end if

            platform%loc%proc_domain = .not. platform%loc%proc_domain
         end if
      end do dup_loop
   end do reports

   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_read_obs_ascii")

end subroutine da_read_obs_ascii


