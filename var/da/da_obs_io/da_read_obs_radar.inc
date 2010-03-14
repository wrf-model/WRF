subroutine da_read_obs_radar (iv, filename)

   !-----------------------------------------------------------------------
   ! Purpose: Read the radar observation file
   !----------------------------------------------------------------------------------------!

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(len=*),  intent(in)    :: filename

   character (len = 120)         :: char_total_radar
   character (len = 120)         :: char_ned

   integer                       :: i, j, n, iost, nlevels, fm

   type (radar_multi_level_type) :: platform

   logical                       :: outside

   integer                       :: total_radar
   integer                       :: n_dup, ndup, iunit
   integer                       :: nlocal(num_ob_indexes)
   integer                       :: ntotal(num_ob_indexes)

   real                          :: obs_time
   integer                       :: iyear, imonth, iday, ihour, imin

   if (trace_use) call da_trace_entry("da_read_obs_radar")

   nlocal(:) = iv%info(:)%plocal(iv%time-1)
   ntotal(:) = iv%info(:)%ptotal(iv%time-1)
 
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

   do n = 1, total_radar

      ! 3.1 skip one blank line

      read (unit=iunit, fmt = '(A)', iostat = iost)

      ! 3.2 read header

      read (unit=iunit, fmt = '(A)', iostat = iost) char_ned

      ! 3.3 read header information

      read (unit=char_ned (1:5),fmt='(A5)', iostat = iost) platform % stn % platform

      if (print_detail_radar) write (unit=stdout,fmt='(A)') 'RADAR Observations information'

      read (unit=char_ned (8:19),fmt='(A12)', iostat = iost) platform % stn % name

      if (print_detail_radar) write (unit=stdout,fmt='(A,A5,A,A12)')  &
                           ' Expect: ',platform % stn % platform, &
                           'data at station:', platform % stn % name

      read (unit=char_ned(20:27),fmt='(F8.3)', iostat = iost) platform % stn % lon

      read (unit=char_ned (30:37),fmt='(F8.3)', iostat = iost) platform % stn % lat

      read (unit=char_ned (40:47),fmt='(F8.1)', iostat = iost) platform % stn % elv

      if (print_detail_radar) write (unit=stdout,fmt='(A,2(F8.3,2X),F8.1)')  &
         'The station longitude, latutude, and altitude are: ', &
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

            if (platform % each (i) % rv % error == 0.0) then
                 platform % each (i) % rv % error  = 1.0
            end if

            if (platform % each (i) % rf % error == 0.0) then
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
         if ( obs_time < time_slots(0) .or. &
              obs_time >= time_slots(num_fgat_time) ) then
            if (print_detail_radar) then
               write(unit=stdout, fmt='(a)') '*** Outside of the time range:'
               write(unit=stdout, fmt=fmt_info) &
                     platform%info%platform,    &
                     platform%info%date_char,   &
                     platform%stn%name
            end if
            cycle
         endif

         call da_llxy (platform%info, platform%loc, outside)

         nlevels = platform%info%levels

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
               if (n==1) ntotal(radar) = ntotal(radar) + 1
               if (outside) cycle reports
               nlocal(radar) = nlocal(radar) + 1
               if (nlocal(radar) > iv%info(radar)%nlocal) cycle reports

               iv % radar (nlocal(radar)) % stn_loc % lon = platform % stn % lon
               iv % radar (nlocal(radar)) % stn_loc % lat = platform % stn % lat
               iv % radar (nlocal(radar)) % stn_loc % elv = platform % stn % elv

               iv%info(radar)%levels(nlocal(radar))    = nlevels
               iv%info(radar)%name(nlocal(radar))      = platform%info%name
               iv%info(radar)%platform(nlocal(radar))  = platform%info%platform
               iv%info(radar)%id(nlocal(radar))        = platform%info%id
               iv%info(radar)%date_char(nlocal(radar)) = platform%info%date_char
               iv%info(radar)%lat(:,nlocal(radar))     = platform%info%lat
               iv%info(radar)%lon(:,nlocal(radar))     = platform%info%lon
               iv%info(radar)%elv(nlocal(radar))       = platform%info%elv
               iv%info(radar)%pstar(nlocal(radar))     = platform%info%pstar

               iv%info(radar)%slp(nlocal(radar))           = platform%loc%slp
               iv%info(radar)%pw(nlocal(radar))            = platform%loc%pw
               iv%info(radar)%x(:,nlocal(radar))           = platform%loc%x
               iv%info(radar)%y(:,nlocal(radar))           = platform%loc%y 
               iv%info(radar)%i(:,nlocal(radar))           = platform%loc%i 
               iv%info(radar)%j(:,nlocal(radar))           = platform%loc%j 
               iv%info(radar)%dx(:,nlocal(radar))          = platform%loc%dx
               iv%info(radar)%dxm(:,nlocal(radar))         = platform%loc%dxm
               iv%info(radar)%dy(:,nlocal(radar))          = platform%loc%dy
               iv%info(radar)%dym(:,nlocal(radar))         = platform%loc%dym
               iv%info(radar)%proc_domain(:,nlocal(radar)) = platform%loc%proc_domain

               iv%info(radar)%obs_global_index(nlocal(radar)) = ntotal(radar)

               allocate (iv % radar (nlocal(radar)) % model_p  (1:nlevels))
               allocate (iv % radar (nlocal(radar)) % model_rho(1:nlevels))
               allocate (iv % radar (nlocal(radar)) % model_qrn(1:nlevels))
               allocate (iv % radar (nlocal(radar)) % height   (1:nlevels))
               allocate (iv % radar (nlocal(radar)) % height_qc(1:nlevels))
               allocate (iv % radar (nlocal(radar)) % rv       (1:nlevels))
               allocate (iv % radar (nlocal(radar)) % rf       (1:nlevels))

               do i = 1, nlevels
                  iv % radar (nlocal(radar)) % height(i)    = platform % each(i) % height
                  iv % radar (nlocal(radar)) % height_qc(i) = platform % each(i) % height_qc
                  iv % radar (nlocal(radar)) % rv(i)        = platform % each(i) % rv
                  iv % radar (nlocal(radar)) % rf(i)        = platform % each(i) % rf
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
   end do

   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_read_obs_radar")


end subroutine da_read_obs_radar


