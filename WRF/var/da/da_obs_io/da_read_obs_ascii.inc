subroutine da_read_obs_ascii (iv, filename, uvq_direct, grid)

   !---------------------------------------------------------------------------
   ! Purpose: Read a GTS observation file
   !
   ! Modifications:      
   !      10/26/2004 - Syed RH Rizvi
   !      Fix Obs-Long as -180 if it is +180
   !
   !      03/04/2005 - Syed RH Rizvi
   !      (a)  AMVs from Geostationary and Polar orbiting satellite are
   !           seperated & used as profile. Currently there is a provision
   !           to use MODIS winds only.
   !      (b)  MODIS obs error are currently assigned through namelist
   !           parameter (modis_cmv_error)
   !
   !      03/30/2005 - Syed RH Rizvi
   !      For global option duplicate obs at East/West boundary
   !                        
   !      08/30/2005 - Syed RH Rizvi
   !      Writing original errors & thicknesses desired for 
   !      outputting QC obs with NTMAX = 0
   !
   !      02/28/2008 - Y.-R. Guo
   !      Satem thickness error should not be divided by gravity because
   !      the unit from obsproc is already meter, not geopotential meter.
   !
   !      03/19/2009 - Y.-R. Guo
   !      Added the time range check when reading in observations.
   !
   !      02/21/2013 - Syed RH Rizvi
   !      Updated with observation-thinning option
   !
   !      03/07/2014 - Feng Gao
   !      Updated wind_sd option for wind obs. types
   !
   !      12-2017 - Jamie Bresch
   !      Add a call to da_gpseph_create_ob for gpseph
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(len=*),  intent(in)    :: filename
   type(domain),     intent(in)     :: grid     ! first guess state.

   character (len =  10)        :: fmt_name

   character (len = 160)        :: info_string

!   character (len = 160)        :: fmt_info, fmt_srfc, fmt_each

   integer                      :: i, j, iost, nlevels, old_nlevels, fm,iunit

   type (multi_level_type)      :: platform
   logical                      :: outside
   logical                      :: outside_all
   logical                      :: uvq_direct
   integer                      :: surface_level
   real                         :: height_error, u_comp, v_comp
   integer                      :: nlocal(num_ob_indexes)
   integer                      :: ilocal(num_ob_indexes)
   integer                      :: ntotal(num_ob_indexes)

   integer                      :: ndup, n, report, obs_index

   real*8                       :: obs_time, analysis_time
   integer                      :: iyear, imonth, iday, ihour, imin
   real                         :: tdiff, dlat_earth,dlon_earth,crit
   integer                      :: itt,itx,iout
   real, allocatable            :: in(:), out(:)
   logical                      :: found, iuse, thin_3d, is_surface
   integer                      :: i1,j1,k, levs
   real                         :: dx,dy,dxm,dym,zk
   real                         :: v_p(kms:kme),v_h(kms:kme)

   ! for gpseph
   type(ob_in_mean_h) :: pseudo_ob
   integer            :: lowest_level

   if (trace_use) call da_trace_entry("da_read_obs_ascii")
 ! Initialize counts
   ntotal(:) = iv%info(:)%ptotal(iv%time-1)
   nlocal(:) = iv%info(:)%plocal(iv%time-1)
   ilocal    = nlocal
   if ( thin_conv_ascii ) then
       do n = 1, num_ob_indexes
          if ( n == radar ) cycle
          call cleangrids_conv(n)
       end do
   end if
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

   if ( use_gpsephobs ) then
      allocate (pseudo_ob%ref(kds:kde))
      allocate (pseudo_ob%lat(kds:kde))
      allocate (pseudo_ob%lon(kds:kde))
      allocate (pseudo_ob%azim(kds:kde))
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

      ! read each level
      ! ---------------

      do i = 1, platform%info%levels
         if (i > max_ob_levels) then
            ! platform%each only has size "max_ob_levels", so if we exceed this limit
            ! we should just read past these lines and not assign them to platform%each
            read (unit = iunit, fmt = trim (fmt_each))
            cycle
         end if
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

         if (uvq_direct) then
            platform%each (i)%u = platform%each (i)%speed
            if (platform%each(i)%rh%inv .gt. missing_r)  &
               platform%each(i)%rh%inv=platform%each(i)%rh%inv / 1e3    !convert back to kg/kg
            if (platform%each(i)%rh%error .gt. 0.0)  &
               platform%each(i)%rh%error=platform%each(i)%rh%error / 1e3  !convert back to kg/kg
         end if

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

         if (.not. uvq_direct) then
         if (fm /= 116 .and. fm /= 118 ) then !not for gpsref, gpseph
         if (platform%each (i)%speed%qc /= missing_data .and. &
             platform%each (i)%v%qc /= missing_data) then

             call da_ffdduv (platform%each (i)%speed%inv, platform%each (i)%v%inv,     &
                             U_comp, V_comp, platform%info%lon, convert_fd2uv)
             platform%each (i)%u = platform%each (i)%speed
             platform%each (i)%v = platform%each (i)%v
             platform%each (i)%u%inv = U_comp
             platform%each (i)%v%inv = V_comp
            
         else
            platform%each (i)%u%inv   = missing_r
            platform%each (i)%v%inv   = missing_r
            platform%each (i)%u%error = missing_r
            platform%each (i)%v%error = missing_r
            platform%each (i)%u%qc    = missing_data
            platform%each (i)%v%qc    = missing_data
         end if
         end if !not for gpsref, gpseph
         end if
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

      read (analysis_date,'(i4,4(1x,i2))') &
                                    iyear, imonth, iday, ihour, imin
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,analysis_time)
      tdiff = abs((obs_time - analysis_time)-0.02)
      dlat_earth = platform%info%lat
      dlon_earth = platform%info%lon
      if (dlon_earth < 0.0) dlon_earth = dlon_earth + 360.0
      if (dlon_earth >= 360.0) dlon_earth = dlon_earth - 360.0
      dlat_earth = dlat_earth * deg2rad
      dlon_earth = dlon_earth * deg2rad

      nlevels = platform%info%levels
      if (nlevels > max_ob_levels) then
         nlevels = max_ob_levels
         platform%info%levels = nlevels
      else if (nlevels < 1) then
         ! Not GPSPW and GPSZD data:
         if (fm /= 111 .and. fm /= 114) then
            cycle reports
         end if
      end if

      levs = nlevels

      if (fm /= 86 .and. fm /= 118) call da_obs_proc_station(platform,fm,uvq_direct)

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

      IF ( wind_sd ) THEN
         wind_sd_buoy   = .true.
         wind_sd_synop  = .true.
         wind_sd_ships  = .true.
         wind_sd_metar  = .true.
         wind_sd_sound  = .true.
         wind_sd_pilot  = .true.
         wind_sd_airep  = .true.
         wind_sd_qscat  = .true.
         wind_sd_tamdar = .true.
         wind_sd_geoamv = .true.
         wind_sd_mtgirs = .true.
       wind_sd_polaramv = .true.
       wind_sd_profiler = .true.
      END IF

      dup_loop: do n = 1, ndup
         is_surface=.false.
         select case(fm)

         case (12) ;
            if (.not. use_synopobs .or. ntotal(synop) == max_synop_input  ) cycle reports
            if (n==1) ntotal(synop) = ntotal(synop)+1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(synop,dlat_earth,dlon_earth,crit,nlocal(synop),itx,1,itt,ilocal(synop),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(synop) = nlocal(synop) + 1
               ilocal(synop) = ilocal(synop) + 1
            end if

            if (.not. wind_sd_synop) platform%each(1)%v%error = platform%each(1)%u%error

            iv%synop(ilocal(synop))%h = platform%each(1)%height
            iv%synop(ilocal(synop))%u = platform%each(1)%u
            iv%synop(ilocal(synop))%v = platform%each(1)%v
            iv%synop(ilocal(synop))%t = platform%each(1)%t
            iv%synop(ilocal(synop))%q = platform%each(1)%q
            iv%synop(ilocal(synop))%p = platform%each(1)%p

            if (wind_sd_synop .and. &
                platform%each(1)%u%qc /= missing_data .and. platform%each(1)%v%qc /= missing_data ) &
                call da_ffdduv_model (iv%synop(ilocal(synop))%u%inv, iv%synop(ilocal(synop))%v%inv, &
                                      platform%each(1)%u%inv, platform%each(1)%v%inv, convert_uv2fd)

         case (13, 17) ;                  ! ships 
            if (.not. use_shipsobs .or. ntotal(ships) == max_ships_input  ) cycle reports
            if (n==1) ntotal(ships) = ntotal(ships) + 1 
            if (outside) cycle reports
             if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(ships,dlat_earth,dlon_earth,crit,nlocal(ships),itx,1,itt,ilocal(ships),iuse)
                if ( .not. iuse ) cycle reports
             else
                nlocal(ships) = nlocal(ships) + 1
                ilocal(ships) = ilocal(ships) + 1
             end if

            if (.not. wind_sd_ships) platform%each(1)%v%error = platform%each(1)%u%error

            iv%ships(ilocal(ships))%h = platform%each(1)%height
            iv%ships(ilocal(ships))%u = platform%each(1)%u
            iv%ships(ilocal(ships))%v = platform%each(1)%v
            iv%ships(ilocal(ships))%t = platform%each(1)%t
            iv%ships(ilocal(ships))%p = platform%each(1)%p
            iv%ships(ilocal(ships))%q = platform%each(1)%q

            if (wind_sd_ships .and. &
                platform%each(1)%u%qc /= missing_data .and. platform%each(1)%v%qc /= missing_data ) & 
                call da_ffdduv_model (iv%ships(ilocal(ships))%u%inv, iv%ships(ilocal(ships))%v%inv, &
                                      platform%each(1)%u%inv, platform%each(1)%v%inv, convert_uv2fd)

         case (15:16) ;
            if (.not. use_metarobs .or. ntotal(metar) == max_metar_input  ) cycle reports
            if (n==1) ntotal(metar) = ntotal(metar) + 1 
            if (outside) cycle reports
             if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(metar,dlat_earth,dlon_earth,crit,nlocal(metar),itx,1,itt,ilocal(metar),iuse)
                if ( .not. iuse ) cycle reports
             else
                nlocal(metar) = nlocal(metar) + 1
                ilocal(metar) = ilocal(metar) + 1
             end if
 
            if (.not. wind_sd_metar) platform%each(1)%v%error = platform%each(1)%u%error

            iv%metar(ilocal(metar))%h = platform%each(1)%height
            iv%metar(ilocal(metar))%u = platform%each(1)%u
            iv%metar(ilocal(metar))%v = platform%each(1)%v
            iv%metar(ilocal(metar))%t = platform%each(1)%t
            iv%metar(ilocal(metar))%p = platform%each(1)%p
            iv%metar(ilocal(metar))%q = platform%each(1)%q

            if (wind_sd_metar .and. &
                platform%each(1)%u%qc /= missing_data .and. platform%each(1)%v%qc /= missing_data ) & 
                call da_ffdduv_model (iv%metar(ilocal(metar))%u%inv, iv%metar(ilocal(metar))%v%inv, &
                                      platform%each(1)%u%inv, platform%each(1)%v%inv, convert_uv2fd)

         case (32:34) ;
            if (.not. use_pilotobs .or. ntotal(pilot) == max_pilot_input  ) cycle reports
            if (n==1) ntotal(pilot) = ntotal(pilot) + 1 
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(pilot,dlat_earth,dlon_earth,crit,nlocal(pilot),itx,1,itt,ilocal(pilot),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(pilot) = nlocal(pilot) + 1
               ilocal(pilot) = ilocal(pilot) + 1
            end if

            if (nlocal(pilot) == ilocal(pilot)) then
               allocate (iv%pilot(ilocal(pilot))%h(1:iv%info(pilot)%max_lev))
               allocate (iv%pilot(ilocal(pilot))%p(1:iv%info(pilot)%max_lev))
               allocate (iv%pilot(ilocal(pilot))%u(1:iv%info(pilot)%max_lev))
               allocate (iv%pilot(ilocal(pilot))%v(1:iv%info(pilot)%max_lev))
            end if
            do i = 1, nlevels

               if (.not. wind_sd_pilot) platform%each(i)%v%error = platform%each(i)%u%error

               iv%pilot(ilocal(pilot))%h(i) = platform%each(i)%height
               iv%pilot(ilocal(pilot))%p(i) = platform%each(i)%p%inv
               iv%pilot(ilocal(pilot))%u(i) = platform%each(i)%u
               iv%pilot(ilocal(pilot))%v(i) = platform%each(i)%v

               if ( .not. wind_sd_pilot ) platform%each(i)%v%error = platform%each(i)%u%error

               if (wind_sd_pilot .and. &
                   platform%each(i)%u%qc /= missing_data .and. platform%each(i)%v%qc /= missing_data ) then
                   call da_ffdduv_model (iv%pilot(ilocal(pilot))%u(i)%inv,iv%pilot(ilocal(pilot))%v(i)%inv, &
                                         platform%each(i)%u%inv, platform%each(i)%v%inv, convert_uv2fd)
               end if
            end do

         case (35:38) ;
            if (.not. use_soundobs .or. ntotal(sound) == max_sound_input  ) cycle reports
            if (n==1) ntotal(sound) = ntotal(sound) + 1 
            if (n==1) ntotal(sonde_sfc) = ntotal(sonde_sfc) + 1 
            if (outside) cycle reports

! determine if level corresponds to surface

            levs = nlevels
            surface_level = 0
            do i = 1, nlevels
               ! if (elevation and height are the same, it is surface.)
               if (platform%info%elv.ne.missing_r.and.platform%each(i)%height.ne.missing_r)then
                  if (abs(platform%info%elv - platform%each(i)%height) < 0.1) then
                     is_surface = .true.
                     surface_level = i
                     levs = nlevels - 1
                     exit
                  end if
               end if
            end do

            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(sound,dlat_earth,dlon_earth,crit,nlocal(sound),itx,1,itt,ilocal(sound),iuse)
               crit = tdiff
               call map2grids_conv(sonde_sfc,dlat_earth,dlon_earth,crit,nlocal(sonde_sfc),itx,1,itt,ilocal(sonde_sfc),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(sound) = nlocal(sound) + 1
               ilocal(sound) = ilocal(sound) + 1
               nlocal(sonde_sfc) = nlocal(sonde_sfc) + 1
               ilocal(sonde_sfc) = ilocal(sonde_sfc) + 1
            end if

            ! Search to see if we have surface obs.

            surface_level = 0

            do i = 1, nlevels
               ! if (elevation and height are the same, it is surface.)
               if (platform%info%elv.ne.missing_r.and.platform%each(i)%height.ne.missing_r)then
                  if (abs(platform%info%elv - platform%each(i)%height) < 0.1) then
                     surface_level = i

                     if (.not. wind_sd_sound) platform%each(i)%v%error = platform%each(i)%u%error

                     ! Save surface pressure.
                     iv%sonde_sfc(ilocal(sonde_sfc))%h = platform%each(i)%height
                     iv%sonde_sfc(ilocal(sonde_sfc))%u = platform%each(i)%u
                     iv%sonde_sfc(ilocal(sonde_sfc))%v = platform%each(i)%v
                     iv%sonde_sfc(ilocal(sonde_sfc))%t = platform%each(i)%t
                     iv%sonde_sfc(ilocal(sonde_sfc))%q = platform%each(i)%q
                     iv%sonde_sfc(ilocal(sonde_sfc))%p = platform%each(i)%p

                     if (wind_sd_sound .and. &
                         platform%each(i)%u%qc /= missing_data .and. platform%each(i)%v%qc /= missing_data ) & 
                         call da_ffdduv_model (iv%sonde_sfc(ilocal(sonde_sfc))%u%inv,iv%sonde_sfc(ilocal(sonde_sfc))%v%inv, &
                                               platform%each(i)%u%inv, platform%each(i)%v%inv, convert_uv2fd)
                     exit
                  end if
               end if
            end do

            ! processing the sonde_sfc data:

            old_nlevels = nlevels

            if (surface_level > 0) then
                nlevels = nlevels - 1
            else
               iv%sonde_sfc(ilocal(sonde_sfc))%h = missing_r
               iv%sonde_sfc(ilocal(sonde_sfc))%u%inv   = missing_r
               iv%sonde_sfc(ilocal(sonde_sfc))%u%qc    = missing
               iv%sonde_sfc(ilocal(sonde_sfc))%u%error = abs(missing_r)
               iv%sonde_sfc(ilocal(sonde_sfc))%v = iv%sonde_sfc(ilocal(sonde_sfc))%u
               iv%sonde_sfc(ilocal(sonde_sfc))%t = iv%sonde_sfc(ilocal(sonde_sfc))%u
               iv%sonde_sfc(ilocal(sonde_sfc))%p = iv%sonde_sfc(ilocal(sonde_sfc))%u
               iv%sonde_sfc(ilocal(sonde_sfc))%q = iv%sonde_sfc(ilocal(sonde_sfc))%u
            end if

            if (nlevels > 0) then
               if( nlocal(sound) == ilocal(sound)) then
                  allocate (iv%sound(ilocal(sound))%h (1:iv%info(sound)%max_lev))   
                  allocate (iv%sound(ilocal(sound))%p (1:iv%info(sound)%max_lev))   
                  allocate (iv%sound(ilocal(sound))%u (1:iv%info(sound)%max_lev))   
                  allocate (iv%sound(ilocal(sound))%v (1:iv%info(sound)%max_lev))   
                  allocate (iv%sound(ilocal(sound))%t (1:iv%info(sound)%max_lev))   
                  allocate (iv%sound(ilocal(sound))%q (1:iv%info(sound)%max_lev))   
               end if

               j = 0
               do i = 1, old_nlevels
                  if (i == surface_level) cycle

                  j=j+1

                  if (.not. wind_sd_sound) platform%each(i)%v%error = platform%each(i)%u%error

                  iv%sound(ilocal(sound))%h(j) = platform%each(i)%height
                  iv%sound(ilocal(sound))%p(j) = platform%each(i)%p%inv
                  iv%sound(ilocal(sound))%u(j) = platform%each(i)%u
                  iv%sound(ilocal(sound))%v(j) = platform%each(i)%v
                  iv%sound(ilocal(sound))%t(j) = platform%each(i)%t
                  iv%sound(ilocal(sound))%q(j) = platform%each(i)%q

                  if (wind_sd_sound .and. &
                      platform%each(i)%u%qc /= missing_data .and. platform%each(i)%v%qc /= missing_data ) &
                      call da_ffdduv_model (iv%sound(ilocal(sound))%u(j)%inv,iv%sound(ilocal(sound))%v(j)%inv, &
                                            platform%each(i)%u%inv, platform%each(i)%v%inv, convert_uv2fd)
               end do

           end if


         case (101) ;
            if (.not. use_tamdarobs .or. ntotal(tamdar) == max_tamdar_input  ) cycle reports

! determine if level corresponds to surface
            levs = nlevels
            surface_level = 0
            do i = 1, nlevels
               ! if (elevation and height are the same, it is surface.)
               if (platform%info%elv.ne.missing_r.and.platform%each(i)%height.ne.missing_r)then
                  if (abs(platform%info%elv - platform%each(i)%height) < 0.1) then
                     is_surface = .true.
                     surface_level = i
                     levs = nlevels - 1
                    exit
                  end if
               end if
            end do

          if( is_surface) then
            if (n==1) ntotal(tamdar_sfc) = ntotal(tamdar_sfc) + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(tamdar_sfc,dlat_earth,dlon_earth,crit,nlocal(tamdar_sfc),itx,1,itt,ilocal(tamdar_sfc),iuse)
                if ( .not. iuse ) cycle reports
             else
                nlocal(tamdar_sfc) = nlocal(tamdar_sfc) + 1
                ilocal(tamdar_sfc) = ilocal(tamdar_sfc) + 1
             end if

            if (.not. wind_sd_tamdar) platform%each(surface_level)%v%error = platform%each(surface_level)%u%error

            iv%tamdar_sfc(ilocal(tamdar_sfc))%h = platform%each(surface_level)%height
            iv%tamdar_sfc(ilocal(tamdar_sfc))%u = platform%each(surface_level)%u
            iv%tamdar_sfc(ilocal(tamdar_sfc))%v = platform%each(surface_level)%v
            iv%tamdar_sfc(ilocal(tamdar_sfc))%t = platform%each(surface_level)%t
            iv%tamdar_sfc(ilocal(tamdar_sfc))%q = platform%each(surface_level)%q
            iv%tamdar_sfc(ilocal(tamdar_sfc))%p = platform%each(surface_level)%p

            if (wind_sd_tamdar .and. &
                platform%each(surface_level)%u%qc /= missing_data .and. platform%each(surface_level)%v%qc /= missing_data ) &
                call da_ffdduv_model (iv%tamdar_sfc(ilocal(tamdar_sfc))%u%inv,iv%tamdar_sfc(ilocal(tamdar_sfc))%v%inv, &
                                      platform%each(surface_level)%u%inv, platform%each(surface_level)%v%inv, convert_uv2fd)

           else ! not is_surface

            if ( levs > 0 .and. n==1) ntotal(tamdar) = ntotal(tamdar) + 1
            if (outside) cycle reports
            if( .not. thin_conv_ascii) then
                   nlocal(tamdar) = nlocal(tamdar) + 1
                   ilocal(tamdar) = ilocal(tamdar) + 1
                if( nlocal(tamdar) == ilocal(tamdar)) then
                allocate (iv%tamdar(ilocal(tamdar))%h (1:iv%info(tamdar)%max_lev))
                allocate (iv%tamdar(ilocal(tamdar))%p (1:iv%info(tamdar)%max_lev))
                allocate (iv%tamdar(ilocal(tamdar))%u (1:iv%info(tamdar)%max_lev))
                allocate (iv%tamdar(ilocal(tamdar))%v (1:iv%info(tamdar)%max_lev))
                allocate (iv%tamdar(ilocal(tamdar))%t (1:iv%info(tamdar)%max_lev))
                allocate (iv%tamdar(ilocal(tamdar))%q (1:iv%info(tamdar)%max_lev))
             end if

             do i = 1, nlevels

                if (.not. wind_sd_tamdar) platform%each(i)%v%error = platform%each(i)%u%error

                iv%tamdar(ilocal(tamdar))%h(i) = platform%each(i)%height
                iv%tamdar(ilocal(tamdar))%p(i) = platform%each(i)%p%inv
                iv%tamdar(ilocal(tamdar))%u(i) = platform%each(i)%u
                iv%tamdar(ilocal(tamdar))%v(i) = platform%each(i)%v
                iv%tamdar(ilocal(tamdar))%t(i) = platform%each(i)%t
                iv%tamdar(ilocal(tamdar))%q(i) = platform%each(i)%q

                if (wind_sd_tamdar .and. &
                    platform%each(i)%u%qc /= missing_data .and. platform%each(i)%v%qc /= missing_data ) &
                    call da_ffdduv_model (iv%tamdar(ilocal(tamdar))%u(i)%inv,iv%tamdar(ilocal(tamdar))%v(i)%inv, &
                                          platform%each(i)%u%inv, platform%each(i)%v%inv, convert_uv2fd)
             end do

            else ! if thin_conv_ascii==true
              thin_3d=.true.
              i1   = platform%loc%i
              j1   = platform%loc%j
              dx   = platform%loc%dx
              dy   = platform%loc%dy
              dxm  = platform%loc%dxm
              dym  = platform%loc%dym

                 do k=kms,kme
                  v_p(k) = dym*(dxm*grid%xb%p(i1,j1,k)+dx*grid%xb%p(i1+1,j1,k)) + &
                            dy*(dxm*grid%xb%p(i1,j1+1,k)+dx*grid%xb%p(i1+1,j1+1,k))
                 end do
                 do k=kms,kme
                  v_h(k) = dym*(dxm*grid%xb%h(i1,j1,k)+dx*grid%xb%h(i1+1,j1,k)) + &
                            dy*(dxm*grid%xb%h(i1,j1+1,k)+dx*grid%xb%h(i1+1,j1+1,k))
                 end do
              do k= 1, nlevels
               zk = missing_r
               if( platform%each(k)%p%qc  >= 0 ) then
                 call da_to_zk(platform%each(k)%p%inv, v_p, v_interp_p, zk)
               else if( platform%each(k)%height_qc  >= 0 ) then
                 call da_to_zk(platform%each(k)%height , v_h, v_interp_h, zk)
               else
                 write(unit=message(1),fmt='(A)')' For tamdar: neither height nor pressure qc is good'
                 call da_error(__FILE__,__LINE__,message(1:1))
               end if
               if ( zk == missing_r ) cycle
               crit = tdiff
               call map2grids_conv(tamdar,dlat_earth,dlon_earth,crit,nlocal(tamdar),itx,1,itt,ilocal(tamdar),iuse,zk,thin_3d)
               if ( .not. iuse ) cycle
               iv%info(tamdar)%levels(ilocal(tamdar))    = 1
               iv%info(tamdar)%name(ilocal(tamdar))      = platform%info%name
               iv%info(tamdar)%platform(ilocal(tamdar))  = platform%info%platform
               iv%info(tamdar)%id(ilocal(tamdar))        = platform%info%id
               iv%info(tamdar)%date_char(ilocal(tamdar)) = platform%info%date_char
               iv%info(tamdar)%lat(:,ilocal(tamdar))     = platform%info%lat
               iv%info(tamdar)%lon(:,ilocal(tamdar))     = platform%info%lon
               iv%info(tamdar)%elv(ilocal(tamdar))       = platform%info%elv
               iv%info(tamdar)%pstar(ilocal(tamdar))     = platform%info%pstar

               iv%info(tamdar)%slp(ilocal(tamdar))           = platform%loc%slp
               iv%info(tamdar)%pw(ilocal(tamdar))            = platform%loc%pw
               iv%info(tamdar)%x(:,ilocal(tamdar))           = platform%loc%x
               iv%info(tamdar)%y(:,ilocal(tamdar))           = platform%loc%y
               iv%info(tamdar)%i(:,ilocal(tamdar))           = platform%loc%i
               iv%info(tamdar)%j(:,ilocal(tamdar))           = platform%loc%j
               iv%info(tamdar)%dx(:,ilocal(tamdar))          = platform%loc%dx
               iv%info(tamdar)%dxm(:,ilocal(tamdar))         = platform%loc%dxm
               iv%info(tamdar)%dy(:,ilocal(tamdar))          = platform%loc%dy
               iv%info(tamdar)%dym(:,ilocal(tamdar))         = platform%loc%dym
               iv%info(tamdar)%proc_domain(:,ilocal(tamdar)) = platform%loc%proc_domain

               iv%info(tamdar)%obs_global_index(ilocal(tamdar)) = ntotal(tamdar)
               if( nlocal(tamdar) == ilocal(tamdar)) then
                allocate (iv%tamdar(ilocal(tamdar))%h(1:iv%info(tamdar)%max_lev))
                allocate (iv%tamdar(ilocal(tamdar))%p(1:iv%info(tamdar)%max_lev))
                allocate (iv%tamdar(ilocal(tamdar))%u(1:iv%info(tamdar)%max_lev))
                allocate (iv%tamdar(ilocal(tamdar))%v(1:iv%info(tamdar)%max_lev))
                allocate (iv%tamdar(ilocal(tamdar))%t(1:iv%info(tamdar)%max_lev))
                allocate (iv%tamdar(ilocal(tamdar))%q(1:iv%info(tamdar)%max_lev))
               end if

               do i = 1, 1

                  if (.not. wind_sd_tamdar) platform%each(k)%v%error = platform%each(k)%u%error

                  iv%tamdar(ilocal(tamdar))%h(i) = platform%each(k)%height
                  iv%tamdar(ilocal(tamdar))%p(i) = platform%each(k)%p%inv
                  iv%tamdar(ilocal(tamdar))%u(i) = platform%each(k)%u
                  iv%tamdar(ilocal(tamdar))%v(i) = platform%each(k)%v
                  iv%tamdar(ilocal(tamdar))%t(i) = platform%each(k)%t
                  iv%tamdar(ilocal(tamdar))%q(i) = platform%each(k)%q

                  if (wind_sd_tamdar .and. &
                      platform%each(k)%u%qc /= missing_data .and. platform%each(k)%v%qc /= missing_data ) &
                      call da_ffdduv_model (iv%tamdar(ilocal(tamdar))%u(i)%inv,iv%tamdar(ilocal(tamdar))%v(i)%inv, &
                                            platform%each(k)%u%inv, platform%each(k)%v%inv, convert_uv2fd)
               end do
              end do ! loop over k levels

            end if ! if over thin_conv_ascii

           end if ! if  is_surface

         case (161) ;
            if (.not. use_mtgirsobs .or. ntotal(mtgirs) == max_mtgirs_input ) cycle reports
            if (n==1) ntotal(mtgirs) = ntotal(mtgirs) + 1
            if (outside) cycle reports
            if (outside) cycle reports
             if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(mtgirs,dlat_earth,dlon_earth,crit,nlocal(mtgirs),itx,1,itt,ilocal(mtgirs),iuse)
                if ( .not. iuse ) cycle reports
             else
                nlocal(mtgirs) = nlocal(mtgirs) + 1
                ilocal(mtgirs) = ilocal(mtgirs) + 1
             end if

            if (nlevels > 0) then

               if( nlocal(mtgirs) == ilocal(mtgirs)) then
                  allocate (iv%mtgirs(ilocal(mtgirs))%h (1:iv%info(mtgirs)%max_lev))
                  allocate (iv%mtgirs(ilocal(mtgirs))%p (1:iv%info(mtgirs)%max_lev))
                  allocate (iv%mtgirs(ilocal(mtgirs))%u (1:iv%info(mtgirs)%max_lev))
                  allocate (iv%mtgirs(ilocal(mtgirs))%v (1:iv%info(mtgirs)%max_lev))
                  allocate (iv%mtgirs(ilocal(mtgirs))%t (1:iv%info(mtgirs)%max_lev))
                  allocate (iv%mtgirs(ilocal(mtgirs))%q (1:iv%info(mtgirs)%max_lev))
               end if

               j = 0
               do i = 1, nlevels

                  j=j+1

                  if (.not. wind_sd_mtgirs) platform%each(i)%v%error = platform%each(i)%u%error

                  iv%mtgirs(ilocal(mtgirs))%h(j) = platform%each(i)%height
                  iv%mtgirs(ilocal(mtgirs))%p(j) = platform%each(i)%p%inv
                  iv%mtgirs(ilocal(mtgirs))%u(j) = platform%each(i)%u
                  iv%mtgirs(ilocal(mtgirs))%v(j) = platform%each(i)%v
                  iv%mtgirs(ilocal(mtgirs))%t(j) = platform%each(i)%t
                  iv%mtgirs(ilocal(mtgirs))%q(j) = platform%each(i)%q

                  if(iv%mtgirs(ilocal(mtgirs))%q(j)%inv.ne.missing_r)then
                     iv%mtgirs(ilocal(mtgirs))%q(j)%inv = iv%mtgirs(ilocal(mtgirs))%q(j)%inv/1000.0
                  endif
                  if(iv%mtgirs(ilocal(mtgirs))%q(j)%error.ne.missing_r)then
                     iv%mtgirs(ilocal(mtgirs))%q(j)%error = iv%mtgirs(ilocal(mtgirs))%q(j)%error/1000.0/100.0
                  endif

                  if (wind_sd_mtgirs .and. &
                      platform%each(i)%u%qc /= missing_data .and. platform%each(i)%v%qc /= missing_data ) &
                      call da_ffdduv_model (iv%mtgirs(ilocal(mtgirs))%u(j)%inv,iv%mtgirs(ilocal(mtgirs))%v(j)%inv, &
                                            platform%each(i)%u%inv, platform%each(i)%v%inv, convert_uv2fd)
               end do
            end if

         case (86)    ;
            ! Reject cloudy satem obs.
            if (.not.use_satemobs .or. ntotal(satem) == max_satem_input) cycle reports
            if (platform%loc%pw%inv > 10.0) cycle reports
            if (n==1) ntotal(satem) = ntotal(satem) + 1
            if (outside) cycle reports
             if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(satem,dlat_earth,dlon_earth,crit,nlocal(satem),itx,1,itt,ilocal(satem),iuse)
                if ( .not. iuse ) cycle reports
             else
                nlocal(satem) = nlocal(satem) + 1
                ilocal(satem) = ilocal(satem) + 1
             end if


            ! The satem ref_p is put in the SLP position in OBSPROC data stream.

            iv%satem(nlocal(satem))%ref_p= platform%loc%slp%inv

            if( nlocal(satem) == ilocal(satem)) then
             allocate (iv%satem(ilocal(satem))%p        (1:iv%info(satem)%max_lev))
             allocate (iv%satem(ilocal(satem))%thickness(1:iv%info(satem)%max_lev))
             allocate (iv%satem(ilocal(satem))%org_thickness(1:iv%info(satem)%max_lev))
            end if

            iv%satem(ilocal(satem))%p(1) = platform%each(1)%p%inv
            iv%satem(ilocal(satem))%thickness(1) = platform%each(1)%t
            !  write original thickness errors for filtered obs
            if (anal_type_qcobs) then
               do i = 1, nlevels
                  iv%satem(ilocal(satem))%org_thickness(i) = platform%each(i)%t
               end do
            end if

            ! Splitting the reported satem data into smaller layers.

            do i = 2, nlevels
               iv%satem(ilocal(satem))%p(i) = platform%each(i)%p%inv
               iv%satem(ilocal(satem))%thickness(i) = platform%each(i)%t
               if (platform%each(i)%t%qc /= missing_data   .and. &
                  platform%each(i-1)%t%qc /= missing_data) then
                  iv%satem(ilocal(satem))%thickness(i)%inv =            &
                  platform%each(i)%t%inv - platform%each(i-1)%t%inv
               else
                  iv%satem(ilocal(satem))%thickness(i)%inv = missing_r
                  iv%satem(ilocal(satem))%thickness(i)%qc  = missing_data
               end if
            end do

            ! Thickness error (m):

            do i = nlevels, 2, -1
               iv%satem(ilocal(satem))%thickness(i)%error = &
               sqrt(iv%satem(ilocal(satem))%thickness(i )%error ** 2 + &
                  iv%satem(ilocal(satem))%thickness(i-1)%error ** 2)
!                  iv%satem(ilocal(satem))%thickness(i-1)%error ** 2) / gravity
            end do

            iv%satem(ilocal(satem))%thickness(1)%error = &
                           sqrt(iv%satem(ilocal(satem))%thickness(1)%error ** 2 + &
                           platform%loc%pw%error ** 2)
!                           platform%loc%pw%error ** 2) / gravity


            ! Geostationary ot Polar orbitting Satellite AMVs:
         case (88)    ;
            if (index(platform%info%name, 'MODIS') > 0 .or. &
                index(platform%info%name, 'modis') > 0 .or. &
                index(platform%info%id, 'AVHRR') > 0)  then
               if (.not.use_polaramvobs .or. ntotal(polaramv) == max_polaramv_input) cycle reports
               if (n==1) ntotal(polaramv) = ntotal(polaramv) + 1
               if (outside) cycle reports
               if (n==1) ntotal(Polaramv) = ntotal(polaramv) + 1
                if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(polaramv,dlat_earth,dlon_earth,crit,nlocal(polaramv),itx,1,itt,ilocal(polaramv),iuse)
                   if ( .not. iuse ) cycle reports
                else
                   nlocal(polaramv) = nlocal(polaramv) + 1
                   ilocal(polaramv) = ilocal(polaramv) + 1
                end if

               if (nlocal(polaramv) == ilocal(polaramv) ) then
                allocate (iv%polaramv(ilocal(polaramv))%p (1:iv%info(polaramv)%max_lev))
                allocate (iv%polaramv(ilocal(polaramv))%u (1:iv%info(polaramv)%max_lev))
                allocate (iv%polaramv(ilocal(polaramv))%v (1:iv%info(polaramv)%max_lev))
               end if

               do i = 1, nlevels

                  if (.not. wind_sd_polaramv) platform%each(i)%v%error = platform%each(i)%u%error

                  iv%polaramv(ilocal(polaramv))%p(i) = platform%each(i)%p%inv
                  iv%polaramv(ilocal(polaramv))%u(i) = platform%each(i)%u
                  iv%polaramv(ilocal(polaramv))%v(i) = platform%each(i)%v

                  if (wind_sd_polaramv .and. &
                      platform%each(i)%u%qc /= missing_data .and. platform%each(i)%v%qc /= missing_data ) &
                      call da_ffdduv_model (iv%polaramv(ilocal(polaramv))%u(i)%inv,iv%polaramv(ilocal(polaramv))%v(i)%inv, &
                                            platform%each(i)%u%inv, platform%each(i)%v%inv, convert_uv2fd)
               end do
               obs_index = polaramv ! geoamv is the fm_index value for 88
            else
               if (.not.use_geoamvobs .or. ntotal(geoamv) == max_geoamv_input) cycle reports
               if (n==1) ntotal(geoamv) = ntotal(geoamv) + 1
               if (outside) cycle reports
                if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(geoamv,dlat_earth,dlon_earth,crit,nlocal(geoamv),itx,1,itt,ilocal(geoamv),iuse)
                   if ( .not. iuse ) cycle reports
                else
                   nlocal(geoamv) = nlocal(geoamv) + 1
                   ilocal(geoamv) = ilocal(geoamv) + 1
                end if

               if( nlocal(geoamv) == ilocal(geoamv) )then
                allocate (iv%geoamv(ilocal(geoamv))%p (1:iv%info(geoamv)%max_lev))
                allocate (iv%geoamv(ilocal(geoamv))%u (1:iv%info(geoamv)%max_lev))
                allocate (iv%geoamv(ilocal(geoamv))%v (1:iv%info(geoamv)%max_lev))
               end if

               do i = 1, nlevels

                  if (.not. wind_sd_geoamv) platform%each(i)%v%error = platform%each(i)%u%error

                  iv%geoamv(ilocal(geoamv))%p(i) = platform%each(i)%p%inv
                  iv%geoamv(ilocal(geoamv))%u(i) = platform%each(i)%u
                  iv%geoamv(ilocal(geoamv))%v(i) = platform%each(i)%v

                  if (wind_sd_geoamv .and. &
                      platform%each(i)%u%qc /= missing_data .and. platform%each(i)%v%qc /= missing_data ) &
                      call da_ffdduv_model (iv%geoamv(ilocal(geoamv))%u(i)%inv,iv%geoamv(ilocal(geoamv))%v(i)%inv, &
                                            platform%each(i)%u%inv, platform%each(i)%v%inv, convert_uv2fd)
               end do

            end if

         case (42,96:97) ;

            if (.not.use_airepobs .or. ntotal(airep) == max_airep_input) cycle reports
            if (n==1) ntotal(airep) = ntotal(airep) + 1
            if (outside) cycle reports

            if( .not. thin_conv_ascii) then
                   nlocal(airep) = nlocal(airep) + 1
                   ilocal(airep) = ilocal(airep) + 1
              if( nlocal(airep) == ilocal(airep)) then
              allocate (iv%airep(ilocal(airep))%h        (1:iv%info(airep)%max_lev))
              allocate (iv%airep(ilocal(airep))%p        (1:iv%info(airep)%max_lev))
              allocate (iv%airep(ilocal(airep))%u        (1:iv%info(airep)%max_lev))
              allocate (iv%airep(ilocal(airep))%v        (1:iv%info(airep)%max_lev))
              allocate (iv%airep(ilocal(airep))%t        (1:iv%info(airep)%max_lev))
              allocate (iv%airep(ilocal(airep))%q        (1:iv%info(airep)%max_lev))
             end if

             do i = 1, nlevels

                if (.not. wind_sd_airep) platform%each(i)%v%error = platform%each(i)%u%error

                iv%airep(ilocal(airep))%h(i) = platform%each(i)%height
                iv%airep(ilocal(airep))%p(i) = platform%each(i)%p%inv
                iv%airep(ilocal(airep))%u(i) = platform%each(i)%u
                iv%airep(ilocal(airep))%v(i) = platform%each(i)%v
                iv%airep(ilocal(airep))%t(i) = platform%each(i)%t
                iv%airep(ilocal(airep))%q(i) = platform%each(i)%q

                if (wind_sd_airep .and. &
                    platform%each(i)%u%qc /= missing_data .and. platform%each(i)%v%qc /= missing_data ) &
                    call da_ffdduv_model (iv%airep(ilocal(airep))%u(i)%inv,iv%airep(ilocal(airep))%v(i)%inv, &
                                          platform%each(i)%u%inv, platform%each(i)%v%inv, convert_uv2fd)
             end do

            else ! if thin_conv_ascii==true
              thin_3d=.true.
              i1   = platform%loc%i
              j1   = platform%loc%j
              dx   = platform%loc%dx
              dy   = platform%loc%dy
              dxm  = platform%loc%dxm
              dym  = platform%loc%dym

                 do k=kms,kme
                  v_p(k) = dym*(dxm*grid%xb%p(i1,j1,k)+dx*grid%xb%p(i1+1,j1,k)) + &
                            dy*(dxm*grid%xb%p(i1,j1+1,k)+dx*grid%xb%p(i1+1,j1+1,k))
                 end do
                 do k=kms,kme
                  v_h(k) = dym*(dxm*grid%xb%h(i1,j1,k)+dx*grid%xb%h(i1+1,j1,k)) + &
                            dy*(dxm*grid%xb%h(i1,j1+1,k)+dx*grid%xb%h(i1+1,j1+1,k))
                 end do
              do k= 1, nlevels
               zk = missing_r
               if( platform%each(k)%p%qc  >= 0 ) then
                 call da_to_zk(platform%each(k)%p%inv, v_p, v_interp_p, zk)
               else if( platform%each(k)%height_qc  >= 0 ) then
                 call da_to_zk(platform%each(k)%height , v_h, v_interp_h, zk)
               else
                 write(unit=message(1),fmt='(A)')' For airep: neither height nor pressure qc is good'
                 call da_error(__FILE__,__LINE__,message(1:1))
               end if
               if ( zk == missing_r ) cycle
               crit = tdiff
               call map2grids_conv(airep,dlat_earth,dlon_earth,crit,nlocal(airep),itx,1,itt,ilocal(airep),iuse,zk,thin_3d)
               if ( .not. iuse ) cycle
               iv%info(airep)%levels(ilocal(airep))    = 1
               iv%info(airep)%name(ilocal(airep))      = platform%info%name
               iv%info(airep)%platform(ilocal(airep))  = platform%info%platform
               iv%info(airep)%id(ilocal(airep))        = platform%info%id
               iv%info(airep)%date_char(ilocal(airep)) = platform%info%date_char
               iv%info(airep)%lat(:,ilocal(airep))     = platform%info%lat
               iv%info(airep)%lon(:,ilocal(airep))     = platform%info%lon
               iv%info(airep)%elv(ilocal(airep))       = platform%info%elv
               iv%info(airep)%pstar(ilocal(airep))     = platform%info%pstar

               iv%info(airep)%slp(ilocal(airep))           = platform%loc%slp
               iv%info(airep)%pw(ilocal(airep))            = platform%loc%pw
               iv%info(airep)%x(:,ilocal(airep))           = platform%loc%x
               iv%info(airep)%y(:,ilocal(airep))           = platform%loc%y
               iv%info(airep)%i(:,ilocal(airep))           = platform%loc%i
               iv%info(airep)%j(:,ilocal(airep))           = platform%loc%j
               iv%info(airep)%dx(:,ilocal(airep))          = platform%loc%dx
               iv%info(airep)%dxm(:,ilocal(airep))         = platform%loc%dxm
               iv%info(airep)%dy(:,ilocal(airep))          = platform%loc%dy
               iv%info(airep)%dym(:,ilocal(airep))         = platform%loc%dym
               iv%info(airep)%proc_domain(:,ilocal(airep)) = platform%loc%proc_domain

               iv%info(airep)%obs_global_index(ilocal(airep)) = ntotal(airep)
               if( nlocal(airep) == ilocal(airep)) then
                allocate (iv%airep(ilocal(airep))%h        (1:iv%info(airep)%max_lev))
                allocate (iv%airep(ilocal(airep))%p        (1:iv%info(airep)%max_lev))
                allocate (iv%airep(ilocal(airep))%u        (1:iv%info(airep)%max_lev))
                allocate (iv%airep(ilocal(airep))%v        (1:iv%info(airep)%max_lev))
                allocate (iv%airep(ilocal(airep))%t        (1:iv%info(airep)%max_lev))
                allocate (iv%airep(ilocal(airep))%q        (1:iv%info(airep)%max_lev))
               end if

               do i = 1, 1

                  if (.not. wind_sd_airep) platform%each(k)%v%error = platform%each(k)%u%error

                  iv%airep(ilocal(airep))%h(i) = platform%each(k)%height
                  iv%airep(ilocal(airep))%p(i) = platform%each(k)%p%inv
                  iv%airep(ilocal(airep))%u(i) = platform%each(k)%u
                  iv%airep(ilocal(airep))%v(i) = platform%each(k)%v
                  iv%airep(ilocal(airep))%t(i) = platform%each(k)%t
                  iv%airep(ilocal(airep))%q(i) = platform%each(k)%q

                  if (wind_sd_airep .and. &
                      platform%each(k)%u%qc /= missing_data .and. platform%each(k)%v%qc /= missing_data ) &
                      call da_ffdduv_model (iv%airep(ilocal(airep))%u(i)%inv,iv%airep(ilocal(airep))%v(i)%inv, &
                                            platform%each(k)%u%inv, platform%each(k)%v%inv, convert_uv2fd)

               end do
              end do ! loop over k levels

            end if ! if over thin_conv_ascii

         case (111, 114) ;
            if((.not.use_gpspwobs  .and. fm == 111) .or. ntotal(gpspw) == max_gpspw_input ) cycle reports
            if((.not.use_gpsztdobs .and. fm == 114) .or. ntotal(gpspw) == max_gpspw_input ) cycle reports
            if (n==1) ntotal(gpspw) = ntotal(gpspw) + 1
            if (outside) cycle reports
                if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(gpspw,dlat_earth,dlon_earth,crit,nlocal(gpspw),itx,1,itt,ilocal(gpspw),iuse)
                   if ( .not. iuse ) cycle reports
                else
                   nlocal(gpspw) = nlocal(gpspw) + 1
                   ilocal(gpspw) = ilocal(gpspw) + 1
                end if

            iv%gpspw(ilocal(gpspw))%tpw  = platform%loc%pw

         case (116) ;
            if(.not.use_gpsrefobs  .or. ntotal(gpsref) == max_gpsref_input ) cycle reports
            if ( ob_format_gpsro /= ob_format_ascii ) cycle reports
            if (n==1) ntotal(gpsref) = ntotal(gpsref) + 1
            if (outside) cycle reports
                if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(gpsref,dlat_earth,dlon_earth,crit,nlocal(gpsref),itx,1,itt,ilocal(gpsref),iuse)
                   if ( .not. iuse ) cycle reports
                else
                   nlocal(gpsref) = nlocal(gpsref) + 1
                   ilocal(gpsref) = ilocal(gpsref) + 1
                end if
            if( nlocal(gpsref) == ilocal(gpsref)) then
               allocate (iv%gpsref(ilocal(gpsref))%h (1:iv%info(gpsref)%max_lev))
               allocate (iv%gpsref(ilocal(gpsref))%ref(1:iv%info(gpsref)%max_lev))
               allocate (iv%gpsref(ilocal(gpsref))%  p(1:iv%info(gpsref)%max_lev))
               allocate (iv%gpsref(ilocal(gpsref))%  t(1:iv%info(gpsref)%max_lev))
               allocate (iv%gpsref(ilocal(gpsref))%  q(1:iv%info(gpsref)%max_lev))
            end if

            do i = 1, nlevels
               iv%gpsref(ilocal(gpsref))%h(i)   = platform%each(i)%height

               ! In OBSPROC, use "td" field to store "gpsref"
               iv%gpsref(ilocal(gpsref))%ref(i) = platform%each(i)%td

               ! check height, only keep data below and above certain height
               if ( iv%gpsref(ilocal(gpsref))%h(i) > top_km_gpsro*1000.0 .or. &
                    iv%gpsref(ilocal(gpsref))%h(i) < bot_km_gpsro*1000.0 ) then
                  iv%gpsref(ilocal(gpsref))%ref(i)%qc = -77
               end if

               ! Keep the retrieved p and t (and q) as "field_type":
               iv%gpsref(ilocal(gpsref))%p(i)   = platform%each(i)%p
               iv%gpsref(ilocal(gpsref))%t(i)   = platform%each(i)%t
               iv%gpsref(ilocal(gpsref))%q(i)   = platform%each(i)%q
            end do

         CASE (118) ;

            if ( .not.use_GpsephObs .or. ntotal(gpseph) == max_gpseph_input ) cycle reports
            if ( ob_format_gpsro /= ob_format_ascii ) cycle reports
            if (n==1) ntotal(gpseph) = ntotal(gpseph) + 1
            if ( gpseph_loadbalance ) then
               if ( myproc /= mod( (ntotal(gpseph)-1), num_procs ) ) cycle reports
            else
               if (outside) cycle reports
            endif

            if (gpsro_drift ==  0) then
               ! lat stored in speed, lon stored in v
               ! replacing all levels with one lat/lon for gpsro_drift=0
               platform%each(1:nlevels)%speed%inv = platform%info%lat
               platform%each(1:nlevels)%v%inv = platform%info%lon
            end if

            !create pseudo_ob on grid mean altitude for gpseph
            call da_gpseph_create_ob(              &
               nlevels,                            &
               platform%each(1:nlevels)%height,    &
               platform%each(1:nlevels)%td%inv,    & ! ref stored in td
               platform%each(1:nlevels)%speed%inv, & ! lat stored in speed
               platform%each(1:nlevels)%v%inv,     & ! lon stored in v
               platform%each(1:nlevels)%rh%inv,    & ! azim stored in rh
               pseudo_ob, lowest_level)

            ! cycle when no valid levels
            if ( lowest_level < 1 ) cycle reports

            nlocal(gpseph) = nlocal(gpseph) + 1
            ilocal(gpseph) = ilocal(gpseph) + 1

            iv%gpseph(ilocal(gpseph))%level1 = lowest_level+1
            iv%gpseph(ilocal(gpseph))%level2 = kde-1
            iv%gpseph(ilocal(gpseph))%rfict  = platform%loc%pw%inv

            levs = kde - kds + 1
            allocate (iv%gpseph(ilocal(gpseph))%h  (kds:kde))
            allocate (iv%gpseph(ilocal(gpseph))%ref(kds:kde))
            allocate (iv%gpseph(ilocal(gpseph))%eph(kds:kde))
            ! iv%gpseph includes additional azim/lat/lon info of each level
            allocate (iv%gpseph(ilocal(gpseph))%lat(kds:kde))
            allocate (iv%gpseph(ilocal(gpseph))%lon(kds:kde))
            allocate (iv%gpseph(ilocal(gpseph))%azim(kds:kde))

            ! initialize eph
            ! eph will be calculated in da_get_innov_vector_gpseph
            iv%gpseph(ilocal(gpseph))%eph(:)%inv   = missing_r
            iv%gpseph(ilocal(gpseph))%eph(:)%qc    = missing_data
            iv%gpseph(ilocal(gpseph))%eph(:)%error = xmiss

            do k = kts, kte
               iv%gpseph(ilocal(gpseph))%h(k)         = global_h_mean(k)*1000.0 !km to m
               iv%gpseph(ilocal(gpseph))%lat(k)       = pseudo_ob%lat(k)
               iv%gpseph(ilocal(gpseph))%lon(k)       = pseudo_ob%lon(k)
               iv%gpseph(ilocal(gpseph))%azim(k)      = pseudo_ob%azim(k)
               iv%gpseph(ilocal(gpseph))%ref(k)%inv   = pseudo_ob%ref(k)
               iv%gpseph(ilocal(gpseph))%ref(k)%qc    = missing_data
               iv%gpseph(ilocal(gpseph))%ref(k)%error = xmiss
            end do

         case (121) ;
            if(.not.use_ssmt1obs  .or. ntotal(ssmt1) == max_ssmt1_input ) cycle reports
            if (n==1) ntotal(ssmt1) = ntotal(ssmt1) + 1
            if (outside) cycle reports
            if (n==1) ntotal(ssmt1) = ntotal(ssmt1) + 1
                if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(ssmt1,dlat_earth,dlon_earth,crit,nlocal(ssmt1),itx,1,itt,ilocal(ssmt1),iuse)
                   if ( .not. iuse ) cycle reports
                else
                   nlocal(ssmt1) = nlocal(ssmt1) + 1
                   ilocal(ssmt1) = ilocal(ssmt1) + 1
                end if

            if ( nlocal(ssmt1) == ilocal(ssmt1)) then
             allocate (iv%ssmt1(ilocal(ssmt1))%h (1:iv%info(ssmt1)%max_lev))
             allocate (iv%ssmt1(ilocal(ssmt1))%p (1:iv%info(ssmt1)%max_lev))
             allocate (iv%ssmt1(ilocal(ssmt1))%t (1:iv%info(ssmt1)%max_lev))
            end if

            do i = 1, nlevels
              iv%ssmt1(ilocal(ssmt1))%h(i) = platform%each(i)%height
              iv%ssmt1(ilocal(ssmt1))%p(i) = platform%each(i)%p%inv
              iv%ssmt1(ilocal(ssmt1))%t(i) = platform%each(i)%t
            end do

         case (122) ;
            if(.not.use_ssmt2obs  .or. ntotal(ssmt2) == max_ssmt2_input ) cycle reports
            if (n==1) ntotal(ssmt2) = ntotal(ssmt2) + 1
            if (outside) cycle reports
                if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(ssmt2,dlat_earth,dlon_earth,crit,nlocal(ssmt2),itx,1,itt,ilocal(ssmt2),iuse)
                   if ( .not. iuse ) cycle reports
                else
                   nlocal(ssmt2) = nlocal(ssmt2) + 1
                   ilocal(ssmt2) = ilocal(ssmt2) + 1
                end if
            if ( nlocal(ssmt2) == ilocal(ssmt2)) then
             allocate (iv%ssmt2(ilocal(ssmt2))%h (1:iv%info(ssmt2)%max_lev))
             allocate (iv%ssmt2(ilocal(ssmt2))%p (1:iv%info(ssmt2)%max_lev))
             allocate (iv%ssmt2(ilocal(ssmt2))%rh(1:iv%info(ssmt2)%max_lev))
            end if

            do i = 1, nlevels
               iv%ssmt2(ilocal(ssmt2))% h(i) = platform%each(i)%height
               iv%ssmt2(ilocal(ssmt2))% p(i) = platform%each(i)%p%inv
               iv%ssmt2(ilocal(ssmt2))%rh(i) = platform%each(i)%rh
            end do

         case (281)    ;
            if(.not.use_qscatobs  .or. ntotal(qscat) == max_qscat_input ) cycle reports
            if (n==1) ntotal(qscat) = ntotal(qscat) + 1
            if (outside) cycle reports
                if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(qscat,dlat_earth,dlon_earth,crit,nlocal(qscat),itx,1,itt,ilocal(qscat),iuse)
                   if ( .not. iuse ) cycle reports
                else
                   nlocal(qscat) = nlocal(qscat) + 1
                   ilocal(qscat) = ilocal(qscat) + 1
                end if

!            if (nlocal(qscat) == ilocal(qscat)) then

                if (.not. wind_sd_qscat) platform%each(1)%v%error = platform%each(1)%u%error

                iv%qscat(ilocal(qscat))%h = platform%each(1)%height
                iv%qscat(ilocal(qscat))%u = platform%each(1)%u
                iv%qscat(ilocal(qscat))%v = platform%each(1)%v

                if (wind_sd_qscat .and. &
                    platform%each(1)%u%qc /= missing_data .and. platform%each(1)%v%qc /= missing_data ) &
                    call da_ffdduv_model (iv%qscat(ilocal(qscat))%u%inv,iv%qscat(ilocal(qscat))%v%inv, &
                                          platform%each(1)%u%inv, platform%each(1)%v%inv, convert_uv2fd)

!            end if

            ! Impose minimum observation error = 1.0m/s for Quikscat data:
            iv%qscat(ilocal(qscat))%u%error = max(platform%each(1)%u%error,1.0)
            iv%qscat(ilocal(qscat))%v%error = max(platform%each(1)%v%error,1.0)

         case (132) ; ! profiler
            if (.not. use_profilerobs .or. ntotal(profiler) == max_profiler_input ) cycle reports
            if (n==1) ntotal(profiler) = ntotal(profiler) + 1
            if (outside) cycle reports
                if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(profiler,dlat_earth,dlon_earth,crit,nlocal(profiler),itx,1,itt,ilocal(profiler),iuse)
                   if ( .not. iuse ) cycle reports
                else
                   nlocal(profiler) = nlocal(profiler) + 1
                   ilocal(profiler) = ilocal(profiler) + 1
                end if

            if (nlocal(profiler) == ilocal(profiler) ) then
               allocate (iv%profiler(ilocal(profiler))%h (1:iv%info(profiler)%max_lev))
               allocate (iv%profiler(ilocal(profiler))%p (1:iv%info(profiler)%max_lev))
               allocate (iv%profiler(ilocal(profiler))%u (1:iv%info(profiler)%max_lev))
               allocate (iv%profiler(ilocal(profiler))%v (1:iv%info(profiler)%max_lev))
            end if

            do i = 1, nlevels

               if (.not. wind_sd_profiler) platform%each(i)%v%error = platform%each(i)%u%error

               iv%profiler(ilocal(profiler))%h(i) = platform%each(i)%height
               iv%profiler(ilocal(profiler))%p(i) = platform%each(i)%p%inv
               iv%profiler(ilocal(profiler))%u(i) = platform%each(i)%u
               iv%profiler(ilocal(profiler))%v(i) = platform%each(i)%v

               if (wind_sd_profiler .and. &
                   platform%each(i)%u%qc /= missing_data .and. platform%each(i)%v%qc /= missing_data ) &
                   call da_ffdduv_model (iv%profiler(ilocal(profiler))%u(i)%inv,iv%profiler(ilocal(profiler))%v(i)%inv, &
                                         platform%each(i)%u%inv, platform%each(i)%v%inv, convert_uv2fd)
            end do

         case (135) ; ! Bogus
            if (.not. use_bogusobs .or. ntotal(bogus) == max_bogus_input ) cycle reports
            if (n==1) ntotal(bogus) = ntotal(bogus) + 1
            if (outside) cycle reports
            if (n==1) ntotal(bogus) = ntotal(bogus) + 1
                if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(bogus,dlat_earth,dlon_earth,crit,nlocal(bogus),itx,1,itt,ilocal(bogus),iuse)
                   if ( .not. iuse ) cycle reports
                else
                   nlocal(bogus) = nlocal(bogus) + 1
                   ilocal(bogus) = ilocal(bogus) + 1
                end if

            if (ilocal(bogus) > max_bogus_input) then
               write(unit=message(1),fmt='(A,I6,A,I6)') &
                  'Bogus #=', ilocal(bogus), ' > max_bogus_input=', max_bogus_input
               call da_error(__FILE__,__LINE__,message(1:1))
            end if

            if (nlocal(bogus) == ilocal(bogus)) then
               allocate (iv%bogus(ilocal(bogus))%h (1:iv%info(bogus)%max_lev))
               allocate (iv%bogus(ilocal(bogus))%p (1:iv%info(bogus)%max_lev))
               allocate (iv%bogus(ilocal(bogus))%u (1:iv%info(bogus)%max_lev))
               allocate (iv%bogus(ilocal(bogus))%v (1:iv%info(bogus)%max_lev))
               allocate (iv%bogus(ilocal(bogus))%t (1:iv%info(bogus)%max_lev))
               allocate (iv%bogus(ilocal(bogus))%q (1:iv%info(bogus)%max_lev))
            end if

            do i = 1, nlevels
               if (.not. wind_sd) platform%each(i)%v%error = platform%each(i)%u%error
               iv%bogus(ilocal(bogus))%h(i) = platform%each(i)%height
               iv%bogus(ilocal(bogus))%p(i) = platform%each(i)%p%inv
               iv%bogus(ilocal(bogus))%u(i) = platform%each(i)%u
               iv%bogus(ilocal(bogus))%v(i) = platform%each(i)%v
               iv%bogus(ilocal(bogus))%t(i) = platform%each(i)%t
               iv%bogus(ilocal(bogus))%q(i) = platform%each(i)%q
            end do

            iv%bogus(ilocal(bogus))%slp    = platform%loc%slp

            if (print_detail_obs) then
               write(unit=stdout,fmt=*) 'nlevels=', nlevels
               write(unit=stdout,fmt=*) 'iv%info(bogus)%nlocal,slp', ilocal(bogus),  &
                  iv % bogus (ilocal(bogus)) % slp
               do i=1,nlevels
                  write(unit=stdout,fmt=*) 'nlocal(bogus), i ', nlocal(bogus),i
                  write(unit=stdout,fmt=*) 'iv%bogus(nlocal(bogus))%u,v=',  &
                     iv%bogus(ilocal(bogus))%u(i),iv%bogus(ilocal(bogus))%v(i)
                  write(unit=stdout,fmt=*) 'iv%bogus(nlocal(bogus))%t,q=',  &
                     iv%bogus(ilocal(bogus))%t(i),iv%bogus(ilocal(bogus))%q(i)
               end do
               write(unit=stdout,fmt='(2(a,i4))') 'nlocal(bogus)=',ilocal(bogus), &
                  'nlevels=',nlevels
            end if

         case (18,19) ;             ! buoy
            if (.not. use_buoyobs .or. ntotal(buoy) == max_buoy_input ) cycle reports
            if (n==1) ntotal(buoy) = ntotal(buoy) + 1
            if (outside) cycle reports
                if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(buoy,dlat_earth,dlon_earth,crit,nlocal(buoy),itx,1,itt,ilocal(buoy),iuse)
                   if ( .not. iuse ) cycle reports
                else
                   nlocal(buoy) = nlocal(buoy)  + 1
                   ilocal(buoy) = ilocal(buoy)  + 1
                end if

            if (.not. wind_sd_buoy) platform%each(1)%v%error = platform%each(1)%u%error

            iv%buoy(ilocal(buoy))%h = platform%each(1)%height
            iv%buoy(ilocal(buoy))%u = platform%each(1)%u
            iv%buoy(ilocal(buoy))%v = platform%each(1)%v
            iv%buoy(ilocal(buoy))%t = platform%each(1)%t
            iv%buoy(ilocal(buoy))%p = platform%each(1)%p
            iv%buoy(ilocal(buoy))%q = platform%each(1)%q

            if (wind_sd_buoy .and. &
                platform%each(1)%u%qc /= missing_data .and. platform%each(1)%v%qc /= missing_data ) &
                call da_ffdduv_model (iv%buoy(ilocal(buoy))%u%inv,iv%buoy(ilocal(buoy))%v%inv, &
                                      platform%each(1)%u%inv, platform%each(1)%v%inv, convert_uv2fd)


         case (133)    ;         ! AIRS retrievals  
            if (.not. use_airsretobs .or. ntotal(airsr) == max_airsr_input ) cycle reports
            if (n==1) ntotal(airsr) = ntotal(airsr) + 1
            if (outside) cycle reports
                if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(airsr,dlat_earth,dlon_earth,crit,nlocal(airsr),itx,1,itt,ilocal(airsr),iuse)
                   if ( .not. iuse ) cycle reports
                else
                   nlocal(airsr) = nlocal(airsr)  + 1
                   ilocal(airsr) = ilocal(airsr)  + 1
                end if

            if (nlocal(airsr) == ilocal(airsr)) then
               allocate (iv%airsr(ilocal(airsr))%h (1:iv%info(airsr)%max_lev))
               allocate (iv%airsr(ilocal(airsr))%p (1:iv%info(airsr)%max_lev))
               allocate (iv%airsr(ilocal(airsr))%t (1:iv%info(airsr)%max_lev))
               allocate (iv%airsr(ilocal(airsr))%q (1:iv%info(airsr)%max_lev))
            end if
            do i = 1, nlevels
               iv%airsr(ilocal(airsr))%h(i) = platform%each(i)%height
               iv%airsr(ilocal(airsr))%p(i) = platform%each(i)%p%inv
               iv%airsr(ilocal(airsr))%t(i) = platform%each(i)%t
               iv%airsr(ilocal(airsr))%q(i) = platform%each(i)%q
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
         if( is_surface .or. (obs_index == gpspw) .or. (levs > 0 .and. .not. thin_conv_ascii) .or. &
            (levs > 0 .and. (thin_conv_ascii .and. (obs_index /=  airep .and. obs_index /= tamdar))) ) then
            iv%info(obs_index)%name(ilocal(obs_index))      = platform%info%name
            iv%info(obs_index)%platform(ilocal(obs_index))  = platform%info%platform
            iv%info(obs_index)%id(ilocal(obs_index))        = platform%info%id
            iv%info(obs_index)%date_char(ilocal(obs_index)) = platform%info%date_char
            ! nlevels adjusted for some obs types so use that
            iv%info(obs_index)%levels(ilocal(obs_index))    = min(iv%info(obs_index)%max_lev, levs)
            iv%info(obs_index)%lat(:,ilocal(obs_index))     = platform%info%lat
            iv%info(obs_index)%lon(:,ilocal(obs_index))     = platform%info%lon
            iv%info(obs_index)%elv(ilocal(obs_index))       = platform%info%elv
            iv%info(obs_index)%pstar(ilocal(obs_index))     = platform%info%pstar

            iv%info(obs_index)%slp(ilocal(obs_index))           = platform%loc%slp
            iv%info(obs_index)%pw(ilocal(obs_index))            = platform%loc%pw
            iv%info(obs_index)%x(:,ilocal(obs_index))           = platform%loc%x
            iv%info(obs_index)%y(:,ilocal(obs_index))           = platform%loc%y
            iv%info(obs_index)%i(:,ilocal(obs_index))           = platform%loc%i
            iv%info(obs_index)%j(:,ilocal(obs_index))           = platform%loc%j
            iv%info(obs_index)%dx(:,ilocal(obs_index))          = platform%loc%dx
            iv%info(obs_index)%dxm(:,ilocal(obs_index))         = platform%loc%dxm
            iv%info(obs_index)%dy(:,ilocal(obs_index))          = platform%loc%dy
            iv%info(obs_index)%dym(:,ilocal(obs_index))         = platform%loc%dym
            iv%info(obs_index)%proc_domain(:,ilocal(obs_index)) = platform%loc%proc_domain

            iv%info(obs_index)%obs_global_index(nlocal(obs_index)) = ntotal(obs_index)

            ! special case for sonde_sfc, duplicate sound info
            if (obs_index == sound) then
               iv%info(sonde_sfc)%name(ilocal(sonde_sfc))      = platform%info%name
               iv%info(sonde_sfc)%platform(ilocal(sonde_sfc))  = platform%info%platform
               iv%info(sonde_sfc)%id(ilocal(sonde_sfc))        = platform%info%id
               iv%info(sonde_sfc)%date_char(ilocal(sonde_sfc)) = platform%info%date_char
               iv%info(sonde_sfc)%levels(ilocal(sonde_sfc))    = 1
               iv%info(sonde_sfc)%lat(:,ilocal(sonde_sfc))     = platform%info%lat
               iv%info(sonde_sfc)%lon(:,ilocal(sonde_sfc))     = platform%info%lon
               iv%info(sonde_sfc)%elv(ilocal(sonde_sfc))       = platform%info%elv
               iv%info(sonde_sfc)%pstar(ilocal(sonde_sfc))     = platform%info%pstar

               iv%info(sonde_sfc)%slp(ilocal(sonde_sfc))           = platform%loc%slp
               iv%info(sonde_sfc)%pw(ilocal(sonde_sfc))            = platform%loc%pw
               iv%info(sonde_sfc)%x(:,ilocal(sonde_sfc))           = platform%loc%x
               iv%info(sonde_sfc)%y(:,ilocal(sonde_sfc))           = platform%loc%y
               iv%info(sonde_sfc)%i(:,ilocal(sonde_sfc))           = platform%loc%i
               iv%info(sonde_sfc)%j(:,ilocal(sonde_sfc))           = platform%loc%j
               iv%info(sonde_sfc)%dx(:,ilocal(sonde_sfc))          = platform%loc%dx
               iv%info(sonde_sfc)%dxm(:,ilocal(sonde_sfc))         = platform%loc%dxm
               iv%info(sonde_sfc)%dy(:,ilocal(sonde_sfc))          = platform%loc%dy
               iv%info(sonde_sfc)%dym(:,ilocal(sonde_sfc))         = platform%loc%dym
               iv%info(sonde_sfc)%proc_domain(:,ilocal(sonde_sfc)) = platform%loc%proc_domain
               iv%info(sonde_sfc)%obs_global_index(ilocal(sonde_sfc)) = ntotal(obs_index)
            end if

            if (is_surface .and. obs_index == tamdar) then
               iv%info(tamdar_sfc)%name(ilocal(tamdar_sfc))      = platform%info%name
               iv%info(tamdar_sfc)%platform(ilocal(tamdar_sfc))  = platform%info%platform
               iv%info(tamdar_sfc)%id(ilocal(tamdar_sfc))        = platform%info%id
               iv%info(tamdar_sfc)%date_char(ilocal(tamdar_sfc)) = platform%info%date_char
               iv%info(tamdar_sfc)%levels(ilocal(tamdar_sfc))    = 1
               iv%info(tamdar_sfc)%lat(:,ilocal(tamdar_sfc))     = platform%info%lat
               iv%info(tamdar_sfc)%lon(:,ilocal(tamdar_sfc))     = platform%info%lon
               iv%info(tamdar_sfc)%elv(ilocal(tamdar_sfc))       = platform%info%elv
               iv%info(tamdar_sfc)%pstar(ilocal(tamdar_sfc))     = platform%info%pstar

               iv%info(tamdar_sfc)%slp(ilocal(tamdar_sfc))           = platform%loc%slp
               iv%info(tamdar_sfc)%pw(ilocal(tamdar_sfc))            = platform%loc%pw
               iv%info(tamdar_sfc)%x(:,ilocal(tamdar_sfc))           = platform%loc%x
               iv%info(tamdar_sfc)%y(:,ilocal(tamdar_sfc))           = platform%loc%y
               iv%info(tamdar_sfc)%i(:,ilocal(tamdar_sfc))           = platform%loc%i
               iv%info(tamdar_sfc)%j(:,ilocal(tamdar_sfc))           = platform%loc%j
               iv%info(tamdar_sfc)%dx(:,ilocal(tamdar_sfc))          = platform%loc%dx
               iv%info(tamdar_sfc)%dxm(:,ilocal(tamdar_sfc))         = platform%loc%dxm
               iv%info(tamdar_sfc)%dy(:,ilocal(tamdar_sfc))          = platform%loc%dy
               iv%info(tamdar_sfc)%dym(:,ilocal(tamdar_sfc))         = platform%loc%dym
               iv%info(tamdar_sfc)%proc_domain(:,ilocal(tamdar_sfc)) = platform%loc%proc_domain

               iv%info(tamdar_sfc)%obs_global_index(ilocal(tamdar_sfc)) = ntotal(tamdar_sfc)
            end if
         end if  ! for thin_conv_ascii skipping obs_index for which thin_3d is true like airep and tamdir

         if (obs_index == gpseph) then
            iv%info(gpseph)%levels(ilocal(gpseph))   = kde - kds + 1
            iv%info(gpseph)%lat(:,ilocal(gpseph))    = iv%gpseph(ilocal(gpseph))%lat(:)
            iv%info(gpseph)%lon(:,ilocal(gpseph))    = iv%gpseph(ilocal(gpseph))%lon(:)
            if (gpsro_drift ==  0) then
               iv%info(gpseph)%lat(:,ilocal(gpseph)) = platform%info%lat
               iv%info(gpseph)%lon(:,ilocal(gpseph)) = platform%info%lon
            end if
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

   if ( use_gpsephobs ) then
      deallocate (pseudo_ob%ref)
      deallocate (pseudo_ob%lat)
      deallocate (pseudo_ob%lon)
      deallocate (pseudo_ob%azim)
   end if

   close(iunit)

   call da_free_unit(iunit)

   ! thinning check
   if ( thin_conv_ascii ) then
      do n = 1, num_ob_indexes
          if (n==radar) cycle
          allocate ( in(thinning_grid_conv(n)%itxmax) )
          allocate (out(thinning_grid_conv(n)%itxmax) )
            do i = 1, thinning_grid_conv(n)%itxmax
               in(i) = thinning_grid_conv(n)%score_crit(i)
            end do
#ifdef DM_PARALLEL
            ! Get minimum crit and associated processor index.
            call mpi_reduce(in, out, thinning_grid_conv(n)%itxmax, true_mpi_real, mpi_min, root, comm, ierr)
            call wrf_dm_bcast_real (out, thinning_grid_conv(n)%itxmax)
#else
            out = in
#endif
            do i = 1, thinning_grid_conv(n)%itxmax
              if( out(i) < 9.99e6) iv%info(n)%thin_ntotal=  iv%info(n)%thin_ntotal + 1
               if ( abs(out(i)-thinning_grid_conv(n)%score_crit(i)) > 1.0E-10 ) then
                  thinning_grid_conv(n)%ibest_obs(i) = 0
               end if
            end do
!            do j = iv%info(n)%plocal(iv%time -1)+1 , iv%info(n)%plocal(iv%time -1)+nlocal(n)
            do j = iv%info(n)%plocal(iv%time -1)+1 , nlocal(n)
               found = .false.
               do i = 1, thinning_grid_conv(n)%itxmax
                  if ( thinning_grid_conv(n)%ibest_obs(i) == j .and.         &
                       thinning_grid_conv(n)%score_crit(i) < 9.99e6 ) then
                   iv%info(n)%thin_nlocal =  iv%info(n)%thin_nlocal + 1
                     found = .true.

                     exit
                  end if
               end do
               if ( .not. found ) then
                  iv%info(n)%thinned(:,j) = .true.
               end if
            end do
         deallocate( in  )
         deallocate( out )
      end do ! loop over num_ob_indexes
   end if  ! thin_conv_ascii
   if (trace_use) call da_trace_exit("da_read_obs_ascii")

end subroutine da_read_obs_ascii


