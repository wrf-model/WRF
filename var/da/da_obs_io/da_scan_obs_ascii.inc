subroutine da_scan_obs_ascii (iv, filename, grid)

   !---------------------------------------------------------------------------
   ! Purpose: Scan WRFVAR GTS observation file
   ! Updates:
   !       Date: 03/19/2009 -        Y.-R. Guo
   !           Added the time range check when reading in observations.
   !       Syed RH Rizvi NCAR/NESL/MMM/DAS Date:  02/21/2013 
   !          Updated with thinning option
   !
   !      12-2017 - Jamie Bresch
   !      Add a call to da_gpseph_create_ob for gpseph
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(*),      intent(in)    :: filename
   type(domain),     intent(in)     :: grid     ! first guess state.


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

   real*8                :: obs_time, analysis_time
   integer               :: iyear, imonth, iday, ihour, imin
   real                  :: tdiff, dlat_earth,dlon_earth,crit
   integer               :: itt,itx,iout
   logical               :: iuse, thin_3d, is_surface
   integer               :: i1,j1,k, nlevels, levs
   real                  :: dx,dy,dxm,dym,zk
   real                  :: v_p(kms:kme),v_h(kms:kme)

   ! for gpseph
   type(ob_in_mean_h) :: pseudo_ob
   integer            :: lowest_level

   if (trace_use) call da_trace_entry("da_scan_obs_ascii")
! Initialize 
      if ( thin_conv_ascii ) then
          do n = 1, num_ob_indexes
             if ( n == radar ) cycle
             call cleangrids_conv(n)
          end do
      end if
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

   if ( use_gpsephobs ) then
      allocate (pseudo_ob%ref(kds:kde))
      allocate (pseudo_ob%lat(kds:kde))
      allocate (pseudo_ob%lon(kds:kde))
      allocate (pseudo_ob%azim(kds:kde))
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

      ! read each level

      nlevels= platform%info%levels 
      if (nlevels > max_ob_levels) then
         write(unit=message(1), fmt='(4(2a,2x),a,2f8.2,2x,2(a,f9.2,2x),2(a,i0,2x))') &
            'Station: ', trim(platform%info%name), &
            'ID: ', trim(platform%info%id), &
            'Platform: ', trim(platform%info%platform), &
            'Date: ', trim(platform%info%date_char), &
            'At Location (lat, lon): ', platform%info%lat, platform%info%lon, &
            'At elevation: ', platform%info%elv, &
            'with pstar: ', platform%info%pstar, &
            'Has ', platform%info%levels, &
            'levels, which is greater than max_ob_levels: ', max_ob_levels

         write (unit=message(2), fmt = '(A,1X,A,1X,A,1X,I4,2f9.3,f9.1,1X,A)') &
            platform%info%name,        &
            platform%info%platform,    &
            platform%info%date_char,   &
            platform%info%levels,      &
            platform%info%lat,         &
            platform%info%lon,         &
            platform%info%elv,         &
            platform%info%id

         write (unit=message(3), fmt = '(A,I4,A)')'Only the first ',max_ob_levels,' levels will be used!'

         call da_warning(__FILE__,__LINE__,message(1:3))
      end if

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
      if( outside_all ) cycle reports

      if (platform%info%levels < 1) then
         if (fm /= 111 .and. fm /= 114) then
            cycle reports
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
             if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(synop,dlat_earth,dlon_earth,crit,iv%info(synop)%nlocal,itx,1,itt,iout,iuse)
                if ( .not. iuse ) cycle reports
             else
                iv%info(synop)%nlocal = iv%info(synop)%nlocal + 1
             end if

         case (13, 17) ;    
            if (.not.use_shipsobs .or. iv%info(ships)%ntotal == max_ships_input) cycle reports
            if (n==1) iv%info(ships)%ntotal = iv%info(ships)%ntotal + 1
            if (outside) cycle reports
             if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(ships,dlat_earth,dlon_earth,crit,iv%info(ships)%nlocal,itx,1,itt,iout,iuse)
                if ( .not. iuse ) cycle reports
             else
                iv%info(ships)%nlocal = iv%info(ships)%nlocal + 1
             end if


         case (15:16) ;     
            if (.not.use_metarobs .or. iv%info(metar)%ntotal == max_metar_input) cycle reports
            if (n==1) iv%info(metar)%ntotal = iv%info(metar)%ntotal + 1
            if (outside) cycle reports
             if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(metar,dlat_earth,dlon_earth,crit,iv%info(metar)%nlocal,itx,1,itt,iout,iuse)
                if ( .not. iuse ) cycle reports
             else
                iv%info(metar)%nlocal = iv%info(metar)%nlocal + 1
             end if

         case (32:34) ;
            if (.not.use_pilotobs .or. iv%info(pilot)%ntotal == max_pilot_input) cycle reports
            if (n==1) iv%info(pilot)%ntotal = iv%info(pilot)%ntotal + 1
            if (outside) cycle reports
             if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(pilot,dlat_earth,dlon_earth,crit,iv%info(pilot)%nlocal,itx,1,itt,iout,iuse)
                if ( .not. iuse ) cycle reports
             else
                iv%info(pilot)%nlocal = iv%info(pilot)%nlocal + 1
             end if

         case (35:38) ;
            if (.not.use_soundobs .or. iv%info(sound)%ntotal == max_sound_input) cycle reports
            if (n==1) iv%info(sound)%ntotal     = iv%info(sound)%ntotal + 1
            if (n==1) iv%info(sonde_sfc)%ntotal = iv%info(sonde_sfc)%ntotal + 1
            if (outside) cycle reports
             if ( thin_conv_ascii ) then
                crit = tdiff
                call map2grids_conv(sound,dlat_earth,dlon_earth,crit,iv%info(sound)%nlocal,itx,1,itt,iout,iuse)
                crit = tdiff
                call map2grids_conv(sonde_sfc,dlat_earth,dlon_earth,crit,iv%info(sonde_sfc)%nlocal,itx,1,itt,iout,iuse)
                if ( .not. iuse ) cycle reports
             else
                iv%info(sound)%nlocal     = iv%info(sound)%nlocal + 1
                iv%info(sonde_sfc)%nlocal = iv%info(sonde_sfc)%nlocal + 1
             end if

         case (101) ;
            if (.not.use_tamdarobs .or. iv%info(tamdar)%ntotal == max_tamdar_input) cycle reports

! determine if level corresponds to surface 
            is_surface=.false.    
            levs = nlevels
            do i = 1, nlevels
               ! if (elevation and height are the same, it is surface.)
               if (platform%info%elv.ne.missing_r.and.platform%each(i)%height.ne.missing_r)then
                  if (abs(platform%info%elv - platform%each(i)%height) < 0.1) then
                     is_surface = .true.
                     levs = nlevels - 1
                    exit
                  end if
               end if
            end do

           if( is_surface) then

             if (n==1) iv%info(tamdar_sfc)%ntotal = iv%info(tamdar_sfc)%ntotal + 1
             if (outside) cycle reports
             if ( thin_conv_ascii ) then
                crit = tdiff
                call map2grids_conv(tamdar_sfc,dlat_earth,dlon_earth,crit,iv%info(tamdar_sfc)%nlocal,itx,1,itt,iout,iuse)
                if ( .not. iuse ) cycle reports
             else
                iv%info(tamdar_sfc)%nlocal = iv%info(tamdar_sfc)%nlocal + 1
             end if
           else ! not is_surface
             if ( levs > 0 .and. n==1) iv%info(tamdar)%ntotal = iv%info(tamdar)%ntotal + 1
             if (outside) cycle reports
             if( .not. thin_conv_ascii ) then
                iv%info(tamdar)%nlocal         = iv%info(tamdar)%nlocal + 1
             else  ! if thin_conv_ascii
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
                do k=1,nlevels
                   zk = missing_r
                   if( platform%each(k)%p%qc  >= 0 ) then
                      call da_to_zk(platform%each(k)%p%inv, v_p, v_interp_p, zk)
                   else if( platform%each(k)%height_qc  >= 0 ) then
                      call da_to_zk(platform%each(k)%height, v_h, v_interp_h, zk)
                   else
                      write(unit=message(1),fmt='(A)')' For tamdar: neither height nor pressure qc is good'
                      call da_error(__FILE__,__LINE__,message(1:1))
                   end if
                   if ( zk == missing_r ) cycle
                   crit = tdiff
                   call map2grids_conv(tamdar,dlat_earth,dlon_earth,crit,iv%info(tamdar)%nlocal,itx,1,itt,iout,iuse,zk,thin_3d)
                   if ( .not. iuse ) cycle
                end do ! loop over k levels
             end if ! if over thin_conv_ascii

           end if ! if is_surface 

         case (161) ;
            if (.not.use_mtgirsobs .or. iv%info(mtgirs)%ntotal == max_mtgirs_input) cycle reports
            if (n==1) iv%info(mtgirs)%ntotal     = iv%info(mtgirs)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(mtgirs,dlat_earth,dlon_earth,crit,iv%info(mtgirs)%nlocal,itx,1,itt,iout,iuse)
                if ( .not. iuse ) cycle reports
            else
                iv%info(mtgirs)%nlocal     = iv%info(mtgirs)%nlocal + 1
            end if

         case (86) ;
            if (.not.use_satemobs .or. iv%info(satem)%ntotal == max_satem_input) cycle reports
            ! Reject cloudy satem obs.
            if (platform%loc%pw%inv > 10.0) cycle reports
            if (n==1) iv%info(satem)%ntotal = iv%info(satem)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(satem,dlat_earth,dlon_earth,crit,iv%info(satem)%nlocal,itx,1,itt,iout,iuse)
                if ( .not. iuse ) cycle reports
            else
                iv%info(satem)%nlocal = iv%info(satem)%nlocal + 1
            end if

         case (88)    ;
            ! Geostationary or Polar orbitting Satellite AMVs:
            if (index(platform%info%name, 'MODIS') > 0 .or. &
                index(platform%info%name, 'modis') > 0 .or. &
                index(platform%info%id, 'AVHRR') > 0 )  then
               if (.not.use_polaramvobs .or. iv%info(polaramv)%ntotal == max_polaramv_input) cycle reports
               if (n==1) iv%info(polaramv)%ntotal = iv%info(polaramv)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(polaramv,dlat_earth,dlon_earth,crit,iv%info(polaramv)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) cycle reports
               else
                   iv%info(polaramv)%nlocal = iv%info(polaramv)%nlocal + 1
               end if
	       obs_index = polaramv ! geoamv is the fm_index value for 88
            else
               if (.not.use_geoamvobs .or. iv%info(geoamv)%ntotal == max_geoamv_input) cycle reports
               if (n==1) iv%info(geoamv)%ntotal = iv%info(geoamv)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv_ascii ) then
                  crit = tdiff
                  call map2grids_conv(geoamv,dlat_earth,dlon_earth,crit,iv%info(geoamv)%nlocal,itx,1,itt,iout,iuse)
                   if ( .not. iuse ) cycle reports
               else
                   iv%info(geoamv)%nlocal = iv%info(geoamv)%nlocal + 1
               end if
            end if

         case (42,96:97) ;
            if (.not.use_airepobs .or. iv%info(airep)%ntotal == max_airep_input) cycle reports
            if (n==1) iv%info(airep)%ntotal = iv%info(airep)%ntotal + 1
            if (outside) cycle reports

            if( .not. thin_conv_ascii ) then
               iv%info(airep)%nlocal         = iv%info(airep)%nlocal + 1
            else  ! if thin_conv_ascii
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
               do k=1,nlevels
                  zk = missing_r
                  if( platform%each(k)%p%qc  >= 0 ) then
                     call da_to_zk(platform%each(k)%p%inv, v_p, v_interp_p, zk)
                  else if( platform%each(k)%height_qc  >= 0 ) then
                     call da_to_zk(platform%each(k)%height, v_h, v_interp_h, zk)
                  else
                     write(unit=message(1),fmt='(A)')' For airep: neither height nor pressure qc is good'
                     call da_error(__FILE__,__LINE__,message(1:1))
                  end if
                  if ( zk == missing_r ) cycle
                  crit = tdiff
                  call map2grids_conv(airep,dlat_earth,dlon_earth,crit,iv%info(airep)%nlocal,itx,1,itt,iout,iuse,zk,thin_3d)
                  if ( .not. iuse ) cycle
               end do ! loop over k levels

            end if ! if over thin_conv_ascii

         case (111, 114) ;       
            if ( (.not.use_gpspwobs  .and. fm == 111) .or. &
                  iv%info(gpspw)%ntotal == max_gpspw_input) cycle reports
            if ( (.not.use_gpsztdobs  .and. fm == 114) .or. &
                  iv%info(gpspw)%ntotal == max_gpspw_input) cycle reports
            if (n==1) iv%info(gpspw)%ntotal = iv%info(gpspw)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(gpspw,dlat_earth,dlon_earth,crit,iv%info(gpspw)%nlocal,itx,1,itt,iout,iuse)
               if ( .not. iuse ) cycle reports
            else
               iv%info(gpspw)%nlocal = iv%info(gpspw)%nlocal + 1
            end if

         case (116) ;
            if (.not.use_gpsrefobs .or. iv%info(gpsref)%ntotal == max_gpsref_input) cycle reports
            if ( ob_format_gpsro /= ob_format_ascii ) cycle reports
            if (n==1) iv%info(gpsref)%ntotal = iv%info(gpsref)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(gpsref,dlat_earth,dlon_earth,crit,iv%info(gpsref)%nlocal,itx,1,itt,iout,iuse)
               if ( .not. iuse ) cycle reports
            else
               iv%info(gpsref)%nlocal = iv%info(gpsref)%nlocal + 1
            end if

         case (118) ;
            if (.not.use_gpsephobs .or. iv%info(gpseph)%ntotal == max_gpseph_input) cycle reports
            if (n==1) iv%info(gpseph)%ntotal = iv%info(gpseph)%ntotal + 1
            if ( gpseph_loadbalance ) then
               if ( myproc /= mod( (iv%info(gpseph)%ntotal-1), num_procs ) ) cycle reports
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
            iv%info(gpseph)%nlocal = iv%info(gpseph)%nlocal + 1

          case (121) ;
            ! SSM/T1 temperatures
            if (.not.use_ssmt1obs .or. iv%info(ssmt1)%ntotal == max_ssmt1_input) cycle reports
            if (n==1) iv%info(ssmt1)%ntotal = iv%info(ssmt1)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(ssmt1,dlat_earth,dlon_earth,crit,iv%info(ssmt1)%nlocal,itx,1,itt,iout,iuse)
               if ( .not. iuse ) cycle reports
            else
               iv%info(ssmt1)%nlocal = iv%info(ssmt1)%nlocal + 1
            end if


         case (122) ;
            ! SSM/T2 relative humidities:
            if (.not.use_ssmt2obs .or. iv%info(ssmt2)%ntotal == max_ssmt2_input) cycle reports
            if (n==1) iv%info(ssmt2)%ntotal = iv%info(ssmt2)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(ssmt2,dlat_earth,dlon_earth,crit,iv%info(ssmt2)%nlocal,itx,1,itt,iout,iuse)
               if ( .not. iuse ) cycle reports
            else
               iv%info(ssmt2)%nlocal = iv%info(ssmt2)%nlocal + 1
            end if

         case (281)    ;
            ! Scatterometer:
            if (.not.use_qscatobs .or. iv%info(qscat)%ntotal == max_qscat_input) cycle reports
            if (n==1) iv%info(qscat)%ntotal = iv%info(qscat)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(qscat,dlat_earth,dlon_earth,crit,iv%info(qscat)%nlocal,itx,1,itt,iout,iuse)
               if ( .not. iuse ) cycle reports
            else
               iv%info(qscat)%nlocal = iv%info(qscat)%nlocal + 1
            end if
         case (132) ;
            if (.not.use_profilerobs .or. iv%info(profiler)%ntotal == max_profiler_input) cycle reports
            if (n==1) iv%info(profiler)%ntotal = iv%info(profiler)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(profiler,dlat_earth,dlon_earth,crit,iv%info(profiler)%nlocal,itx,1,itt,iout,iuse)
               if ( .not. iuse ) cycle reports
            else
               iv%info(profiler)%nlocal = iv%info(profiler)%nlocal + 1
            end if

         case (135) ;
            if (.not.use_bogusobs .or. iv%info(bogus)%ntotal == max_bogus_input) cycle reports
            if (n==1) iv%info(bogus)%ntotal = iv%info(bogus)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(bogus,dlat_earth,dlon_earth,crit,iv%info(bogus)%nlocal,itx,1,itt,iout,iuse)
               if ( .not. iuse ) cycle reports
            else
              iv%info(bogus)%nlocal = iv%info(bogus)%nlocal + 1
            end if

         case (18,19) ;             ! buoy
            if (.not.use_buoyobs .or. iv%info(buoy)%ntotal == max_buoy_input) cycle reports
            if (n==1) iv%info(buoy)%ntotal = iv%info(buoy)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(buoy,dlat_earth,dlon_earth,crit,iv%info(buoy)%nlocal,itx,1,itt,iout,iuse)
               if ( .not. iuse ) cycle reports
            else
               iv%info(buoy)%nlocal = iv%info(buoy)%nlocal + 1
            end if

         case (133) ;               ! AIRS retrievals
            if (.not.use_airsretobs .or. iv%info(airsr)%ntotal == max_airsr_input) cycle reports
            if (n==1) iv%info(airsr)%ntotal = iv%info(airsr)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
               call map2grids_conv(airsr,dlat_earth,dlon_earth,crit,iv%info(airsr)%nlocal,itx,1,itt,iout,iuse)
               if ( .not. iuse ) cycle reports
            else
               iv%info(airsr)%nlocal = iv%info(airsr)%nlocal + 1
            end if

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

   if ( use_gpsephobs ) then
      deallocate (pseudo_ob%ref)
      deallocate (pseudo_ob%lat)
      deallocate (pseudo_ob%lon)
      deallocate (pseudo_ob%azim)
   end if

   iv%info(sonde_sfc)%max_lev=1
   iv%info(tamdar_sfc)%max_lev=1
   iv%info(synop)%max_lev=1   ! To prevent some bad observations with more than 1 level, should I add more?
   iv%info(ships)%max_lev=1   
   iv%info(qscat)%max_lev=1   
   iv%info(metar)%max_lev=1   

   iv%info(gpseph)%max_lev= kde-kds+1

   close(iunit)

   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_scan_obs_ascii")

end subroutine da_scan_obs_ascii


