subroutine da_read_obs_radar (iv, filename, grid)

   !-----------------------------------------------------------------------
   ! Purpose: Read the radar observation file
   !----------------------------------------------------------------------------------------!

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(len=*),  intent(in)    :: filename
   type(domain),     intent(in)     :: grid     ! first guess state.

   integer                       :: i, j, n, nn, iost, nlevels, fm
   integer                       :: total_radar
   integer                       :: iunit

   type (radar_multi_level_type) :: platform

   character (LEN = 120)         :: char_total_radar
   character (LEN = 120)         :: char_ned

   logical                       :: outside, outside_all
   integer                       :: n_dup, ndup

   real*8                        :: obs_time
   integer                       :: iyear, imonth, iday, ihour, imin

   integer                       :: ntotal,nlocal,ilocal
   integer                       :: radar_nlocal
   real, allocatable             :: in(:), out(:)


   if (trace_use) call da_trace_entry("da_read_obs_radar")

   ntotal = iv%info(radar)%ptotal(iv%time-1)
   nlocal = iv%info(radar)%plocal(iv%time-1)
   ilocal = nlocal 
   radar_nlocal = nlocal
 
   ! 1. open file

   call da_get_unit(iunit)
   open(unit   = iunit,     &
        FILE   = trim(filename), &
        FORM   = 'FORMATTED',  &
        ACCESS = 'SEQUENTIAL', &
        iostat =  iost,     &
        STATUS = 'OLD')

   if (iost /= 0) then
      ! Missing file does not matter
      call da_warning(__FILE__,__LINE__, &
         (/"Cannot open radar file "//filename/))
      call da_free_unit(iunit) 
      if (trace_use) call da_trace_exit("da_read_obs_radar")
      return
   end if

   ! 2. read total radar

   !  2.1 read first line

   read (unit=iunit, fmt = '(A)', iostat = iost) char_total_radar

   !  2.2 process error

   if (iost /= 0) then
     call da_error(__FILE__,__LINE__, &
        (/"Cannot read radar file"/))
   end if

   !  2.3 total radar number

   read (unit=char_total_radar (15:17),fmt='(I3)', iostat = iost) total_radar

   if (print_detail_radar) write (unit=stdout,fmt='(/,A,I3,/)') &
       ' TOTAL RADAR: ', total_radar

   !  2.4 skip one line

   read (unit=iunit, fmt = '(A)', iostat = iost)

   ! 3. read radar data

   do nn = 1, total_radar
 
      ! 3.1 skip one blank line

      read (unit=iunit, fmt = '(A)', iostat = iost)

      ! 3.2 read header

      read (unit=iunit, fmt = '(A)', iostat = iost) char_ned

      ! 3.3 read header information

      read (unit=char_ned (1:5),fmt='(A5)', iostat = iost) platform % stn % platform

      if (print_detail_radar) write (unit=stdout,fmt='(A)') 'RADAR Observations information'

      read (unit=char_ned (8:19),fmt='(A12)', iostat = iost) platform % stn % name

!      if (print_detail_radar) write (unit=stdout,fmt='(A,A5,A,A12)')  &
       write (unit=stdout,fmt='(A,A5,A,A12)')  &
                           ' Reading ',platform % stn % platform, &
                           ' data at station:', platform % stn % name

      read (unit=char_ned(20:27),fmt='(F8.3)', iostat = iost) platform % stn % lon

      read (unit=char_ned (30:37),fmt='(F8.3)', iostat = iost) platform % stn % lat

      read (unit=char_ned (40:47),fmt='(F8.1)', iostat = iost) platform % stn % elv

      if (print_detail_radar) write (unit=stdout,fmt='(A,2(F8.3,2X),F8.1)')  &
         'The station longitude, latitude, and altitude are: ', &
         platform % stn % lon, &
         platform % stn % lat, platform % stn % elv

      read (unit=char_ned (50:68),fmt='(A19)', iostat = iost) platform % stn % date_char

      if (print_detail_radar) write (unit=stdout,fmt='(A,A19)')   &
         'The observation time for this data is ',     &
         platform % stn % date_char

      read (unit=char_ned (69:74),fmt='(I6)', iostat = iost) platform % stn % numobs

      if (print_detail_radar) write (unit=stdout,fmt='(A,I6)')   &
         'Total number of Super-observations is ', &
         platform % stn % numobs


      read (unit=char_ned (75:80),fmt='(I6)', iostat = iost) platform % stn % levels

      if (print_detail_radar) write (unit=stdout,fmt='(A,I6)')   &
         'Vertical layers for each Super-observation is ', &
         platform % stn % levels

      ! 3.4 skip two lines

      read (unit=iunit, fmt = '(A)', iostat = iost)
      read (unit=iunit, fmt = '(A)', iostat = iost)

      ! 3.5 loop over records

      reports: do j = 1, platform % stn % numobs

         ! 3.5.1 read station general info

         read (unit = iunit, iostat = iost, &
                      fmt = '(A12,3X,A19,2X,2(F12.3,2X),F8.1,2X,I6)') &
                      platform % info % platform,  &
                      platform % info % date_char, &
                      platform % info % lat,       &
                      platform % info % lon,       &
                      platform % info % elv,       &
                      platform % info % levels
      if (platform%info%lon == 180.0  ) platform%info%lon =-180.000
      ! Fix funny wind direction at Poles
      if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
         platform%info%lon = 0.0
      end if

         read(platform % info % platform (4:6), '(I3)') fm

         ! 3.5.2 read each level

         do i = 1, platform % info % levels
            ! height
            platform%each(i) = radar_each_level_type(missing_r, missing, -1.0,       &
               field_type(missing_r, missing, missing_r, missing, missing_r), & ! rv
               field_type(missing_r, missing, missing_r, missing, missing_r))   ! rf

            read (unit = iunit, fmt = '(3X, F12.1, 2(F12.3,I4,F12.3,2X))') &
                             platform % each (i) % height,           &
                             platform % each (i) % rv % inv,         &
                             platform % each (i) % rv % qc,          &
                             platform % each (i) % rv % error,       &
                             platform % each (i) % rf % inv,         &
                             platform % each (i) % rf % qc,          &
                             platform % each (i) % rf % error

            if (platform % each (i) % rv % error == 0.0 .or. &
                abs(platform % each (i) % rv % error - missing_r) < 1.0) then
                 platform % each (i) % rv % error  = 1.0
            end if

            if (platform % each (i) % rf % error == 0.0 .or. &
                abs(platform % each (i) % rf % error - missing_r) < 1.0) then
                 platform % each (i) % rf % error  = 1.0
            end if

            if (platform % each (i) % rv % inv   == missing_r .or. &
                platform % each (i) % rv % error == missing_r) then
                platform % each (i) % rv % qc     = missing_data
            end if

            if (platform % each (i) % rf % inv   == missing_r .or. &
                platform % each (i) % rf % error == missing_r) then
                platform % each (i) % rf % qc     = missing_data
            end if
         end do

         ! Check if outside of the time range:

         read (platform%info%date_char,'(i4,4(1x,i2))') &
               iyear, imonth, iday, ihour, imin
         call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
         ! If you skip this part, you have to skip in da_scan_obs_radar.inc too!
         if ( obs_time < time_slots(0) .or. &
              obs_time >= time_slots(num_fgat_time) ) then
            if (print_detail_radar) then
               write(unit=stdout, fmt='(a)') '*** Outside of the time range:'
               write(unit=stdout, fmt=fmt_info) &
                     platform%info%platform,    &
                     platform%info%date_char,   &
                     platform%stn%name
            end if
            cycle reports
         endif

         call da_llxy (platform%info, platform%loc, outside, outside_all)
         if( outside_all ) then
            if (print_detail_radar) then
               write(unit=stdout, fmt='(a)') '*** Report is outside of domain:'
               write(unit=stdout, fmt='(2x,a,2(2x,f8.3),2x,a)') &
                     platform%info%platform,    &
                     platform%info%lat,   &
                     platform%info%lon,   &
                     platform%stn%name
            end if
            cycle reports
         end if

        read (analysis_date,'(i4,4(1x,i2))') &
                                    iyear, imonth, iday, ihour, imin

         nlevels = platform%info%levels

            iv%info(radar)%max_lev = max(iv%info(radar)%max_lev, platform%info%levels)

         if (nlevels > max_ob_levels) then
            write(unit=message(1),fmt='(A,2I8)') &
               ' radar=> nlevels > max_ob_levels:',nlevels, max_ob_levels
            call da_warning(__FILE__,__LINE__,message(1:1)) 
            nlevels = max_ob_levels
             platform%info%levels = nlevels
         else if (nlevels < 1) then
            cycle reports
         end if

         ! Loop over duplicating obs for global
         n_dup = 1
         if (global .and. &
            (platform%loc%i == ids .or. platform%loc%i == ide)) n_dup= 2
         do ndup = 1, n_dup
            select case (fm)
            case (128)
            if (.not.use_radarobs .or. ntotal == max_radar_input) cycle reports

               if (ndup==1 ) ntotal = ntotal + 1
               if (outside) cycle reports
               nlocal = nlocal + 1
               ilocal = ilocal + 1
               iv % radar (ilocal) % stn_loc % lon = platform % stn % lon
               iv % radar (ilocal) % stn_loc % lat = platform % stn % lat
               iv % radar (ilocal) % stn_loc % elv = platform % stn % elv

               iv%info(radar)%levels(ilocal)    = nlevels
               iv%info(radar)%name(ilocal)      = platform%info%name
               iv%info(radar)%platform(ilocal)  = platform%info%platform
               iv%info(radar)%id(ilocal)        = platform%info%id
               iv%info(radar)%date_char(ilocal) = platform%info%date_char
               iv%info(radar)%lat(:,ilocal)     = platform%info%lat
               iv%info(radar)%lon(:,ilocal)     = platform%info%lon
               iv%info(radar)%elv(ilocal)       = platform%info%elv
               iv%info(radar)%x(:,ilocal)           = platform%loc%x
               iv%info(radar)%y(:,ilocal)           = platform%loc%y
               iv%info(radar)%i(:,ilocal)           = platform%loc%i
               iv%info(radar)%j(:,ilocal)           = platform%loc%j
               iv%info(radar)%dx(:,ilocal)          = platform%loc%dx
               iv%info(radar)%dxm(:,ilocal)         = platform%loc%dxm
               iv%info(radar)%dy(:,ilocal)          = platform%loc%dy
               iv%info(radar)%dym(:,ilocal)         = platform%loc%dym
               iv%info(radar)%proc_domain(:,ilocal) = platform%loc%proc_domain
               iv%info(radar)%obs_global_index(ilocal) = ntotal
               if( nlocal == ilocal) then
                  allocate (iv % radar (ilocal) % model_p  (1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % model_rho(1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % model_qrn(1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % model_qcl(1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % model_qci(1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % model_qsn(1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % model_qgr(1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % model_zmm(1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % height   (1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % height_qc(1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % rv       (1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % rf       (1:iv%info(radar)%max_lev))
                  allocate (iv % radar (ilocal) % zmm      (1:iv%info(radar)%max_lev))
                  if ( use_radar_rhv ) then
                     allocate (iv % radar (ilocal) % rrn      (1:iv%info(radar)%max_lev))
                     allocate (iv % radar (ilocal) % rcl      (1:iv%info(radar)%max_lev))
                     allocate (iv % radar (ilocal) % rci      (1:iv%info(radar)%max_lev))
                     allocate (iv % radar (ilocal) % rsn      (1:iv%info(radar)%max_lev))
                     allocate (iv % radar (ilocal) % rgr      (1:iv%info(radar)%max_lev))
                     allocate (iv % radar (ilocal) % rrno     (1:iv%info(radar)%max_lev))
                     allocate (iv % radar (ilocal) % rclo     (1:iv%info(radar)%max_lev))
                     allocate (iv % radar (ilocal) % rcio     (1:iv%info(radar)%max_lev))
                     allocate (iv % radar (ilocal) % rsno     (1:iv%info(radar)%max_lev))
                     allocate (iv % radar (ilocal) % rgro     (1:iv%info(radar)%max_lev))
                  end if
                  if ( use_radar_rqv ) then
                     allocate (iv % radar (ilocal) % rqv      (1:iv%info(radar)%max_lev))
                     allocate (iv % radar (ilocal) % rqvo     (1:iv%info(radar)%max_lev))
                  end if
               end if
               do i = 1, nlevels
                  iv % radar (ilocal) % height(i)    = platform % each(i) % height
                  iv % radar (ilocal) % height_qc(i) = platform % each(i) % height_qc
                  iv % radar (ilocal) % rv(i)        = platform % each(i) % rv
                  iv % radar (ilocal) % rf(i)        = platform % each(i) % rf

                  if ( use_radar_rhv ) then
                     iv % radar (ilocal) % rrn(i) % inv   = missing_r
                     iv % radar (ilocal) % rrn(i) % qc    = missing_data
                     iv % radar (ilocal) % rrn(i) % error = missing_r
                     iv % radar (ilocal) % rrno(i)        = missing_r

                     iv % radar (ilocal) % rcl(i) % inv   = missing_r
                     iv % radar (ilocal) % rcl(i) % qc    = missing_data
                     iv % radar (ilocal) % rcl(i) % error = missing_r
                     iv % radar (ilocal) % rclo(i)        = missing_r

                     iv % radar (ilocal) % rci(i) % inv   = missing_r
                     iv % radar (ilocal) % rci(i) % qc    = missing_data
                     iv % radar (ilocal) % rci(i) % error = missing_r
                     iv % radar (ilocal) % rcio(i)        = missing_r

                     iv % radar (ilocal) % rsn(i) % inv   = missing_r
                     iv % radar (ilocal) % rsn(i) % qc    = missing_data
                     iv % radar (ilocal) % rsn(i) % error = missing_r
                     iv % radar (ilocal) % rsno(i)        = missing_r

                     iv % radar (ilocal) % rgr(i) % inv   = missing_r
                     iv % radar (ilocal) % rgr(i) % qc    = missing_data
                     iv % radar (ilocal) % rgr(i) % error = missing_r
                     iv % radar (ilocal) % rgro(i)        = missing_r
                  end if

                  if ( use_radar_rqv ) then
                     iv % radar (ilocal) % rqv(i) % inv   = missing_r
                     iv % radar (ilocal) % rqv(i) % qc    = missing_data
                     iv % radar (ilocal) % rqv(i) % error = missing_r
                     iv % radar (ilocal) % rqvo(i)        = missing_r
                  end if
               end do

            case default;
               write(unit=message(1), fmt='(a)') 'Unsaved obs found:'
               write(unit=message(2), fmt='(2a)') &
                  'platform % info % platform=', platform % info % platform
               write(unit=message(3), fmt='(a, i3)') &
                  'platform % info % levels=', platform % info % levels
               call da_warning(__FILE__,__LINE__,message(1:3))
            end select
            if (global .and. ndup == 1) then
               if (platform%loc % i >= ide) then
                  platform%loc%i = ids
                  platform%loc%proc_domain = .false.
               else if (platform%loc % i <= ids) then
                  platform%loc%i = ide
                  platform%loc%proc_domain = .false.
               end if
            end if
         end do        !  loop over duplicate
      end do reports

                radar_nlocal = nlocal             


   end do  ! total_radar

   if (print_detail_radar) write (unit=stdout,fmt='(/,A,I3,/)') &
       ' Processed TOTAL RADAR: ', total_radar
   close(iunit)
   call da_free_unit(iunit)
   if (trace_use) call da_trace_exit("da_read_obs_radar")
end subroutine da_read_obs_radar

