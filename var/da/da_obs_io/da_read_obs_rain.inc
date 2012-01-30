subroutine da_read_obs_rain (iv, filename, time)

   !-----------------------------------------------------------------------
   ! Purpose: Read the rain observation file
   !----------------------------------------------------------------------------------------!

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(len=*),  intent(in)    :: filename
   integer,           intent(in)    :: time

   character (len = 120)         :: char_total_rain
   character (len = 160)         :: info_string
   
   integer                       :: i, j, n, iost, nlevels, fm

   type (rain_single_level_type) :: platform

   logical                       :: outside, outside_all

   integer                       :: total_rain
   integer                       :: n_dup, ndup, iunit
   integer                       :: nlocal(num_ob_indexes)
   integer                       :: ntotal(num_ob_indexes)

   real*8                        :: obs_time
   integer                       :: iyear, imonth, iday, ihour, imin

   if (trace_use) call da_trace_entry("da_read_obs_rain")

   do i = 1, num_ob_indexes
      nlocal(i) = iv%info(i)%plocal(time-1)
      ntotal(i) = iv%info(i)%ptotal(time-1)
   enddo

   ! 1. open file
   ! ============
   call da_get_unit(iunit)

   open(unit   = iunit,     &
        FILE   = trim(filename), &
        FORM   = 'FORMATTED',  &
        ACCESS = 'SEQUENTIAL', &
        iostat =  iost,     &
        STATUS = 'OLD')

   if (iost /= 0) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iost," opening rainfall obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      call da_free_unit(iunit)
      if (trace_use) call da_trace_exit("da_read_obs_rain")
      return
   end if

   ! 2. read rainfall info 
   ! ==================

   !  2.1 read first line
   !      ---------------

   read (unit=iunit, fmt = '(A)', iostat = iost) char_total_rain

   !  2.2 process error

   if (iost /= 0) then
     call da_error(__FILE__,__LINE__, &
        (/"Cannot read rainfall file"/))
   end if

   !  2.3 read header info

   head_info: do
      read (unit=iunit, fmt = '(A)', iostat = iost) info_string
      if (iost /= 0) then
         write(unit=message(1),fmt='(A,I3,A,I3)') &
            "Error",iost,"reading rainfall obs header on unit",iunit
         call da_warning(__FILE__,__LINE__,message(1:1))
      if (trace_use) call da_trace_exit("da_scan_obs_rain")
      return
      end if
      if (info_string(1:6) == 'EACH  ') exit
   end do head_info

   !  2.4 total rainfall data info

   read (unit=char_total_rain (8:14),fmt='(I7)', iostat = iost) total_rain

   if (print_detail_rain) write (unit=stdout,fmt='(/,A,I7,/)') &
       ' TOTAL RAINFALL DATA: ', total_rain

   !  2.5 skip one line

   read (unit=iunit, fmt = '(A)', iostat = iost)

   ! 3. read rainfall data
   ! =================  

   reports:   do n = 1, total_rain

      if (print_detail_rain) write (unit=stdout,fmt='(A)') 'RAIN Observations information'

      ! 3.1 read station general info

      read (unit = iunit, iostat = iost, &
                   fmt = '(A12,1X,A19,1X,I6,2(F12.3,2X),F8.1,1X,A5)') &
                   platform % info % platform,  &
                   platform % info % date_char, &
                   platform % info % levels,    &
                   platform % info % lat,       &
                   platform % info % lon,       &
                   platform % info % elv,       &
                   platform % info % id 

      if (print_detail_rain) then
         write(unit = stdout, fmt ='(A12,1X,A19,1X,I6,2(F12.3,2X),F8.1,1X,A5)') &
            platform%info%platform,    &
            platform%info%date_char,   &
            platform%info%levels,      &
            platform%info%lat,         &
            platform%info%lon,         &
            platform%info%elv,         &
            platform%info%id
      end if

      read(platform % info % platform (4:6), '(I3)') fm

      ! 3.2 read rainfall data 

      platform%each(1) = rain_each_type(missing_r, missing, -1.0,       &
         field_type(missing_r, missing, missing_r, missing, missing_r))

      read (unit = iunit, fmt = '(F12.3,F12.3,I4,F12.3)') &
         platform % each (1) % height,             &
         platform % each (1) % rain % inv,         &
         platform % each (1) % rain % qc,          &
         platform % each (1) % rain % error

      if (platform % each (1) % rain % error == 0.0) then
         platform % each (1) % rain % error  = 1.0
      end if

      if (platform % each (1) % rain % inv   == missing_r .or. &
          platform % each (1) % rain % error == missing_r) then
          platform % each (1) % rain % qc     = missing_data
      end if

      ! 3.3 Check if outside of the time range:

      read (platform%info%date_char,'(i4,4(1x,i2))') &
            iyear, imonth, iday, ihour, imin
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
      if ( obs_time < time_slots(0) .or. &
           obs_time >= time_slots(num_fgat_time) ) then
         if (print_detail_rain) then
            write(unit=stdout, fmt='(a)') '*** Outside of the time range:'
            write(unit=stdout, fmt=fmt_info) &
               platform%info%platform,    &
               platform%info%date_char,   &
               platform%stn%name
         end if
         cycle
      endif

      call da_llxy (platform%info, platform%loc, outside, outside_all)

      if (outside_all) then
         cycle reports
      end if

      nlevels = platform%info%levels

      ! 3.4 Loop over duplicating obs for global

      n_dup = 1
      if (global .and. &
         (platform%loc%i == ids .or. platform%loc%i == ide)) n_dup= 2
      do ndup = 1, n_dup
         select case (fm)

         case (129)
            if (n==1) ntotal(rain) = ntotal(rain) + 1
            if (outside) cycle reports
            nlocal(rain) = nlocal(rain) + 1
            if (nlocal(rain) > iv%info(rain)%nlocal) cycle reports

            iv % rain (nlocal(rain)) % stn_loc % lon = platform % stn % lon
            iv % rain (nlocal(rain)) % stn_loc % lat = platform % stn % lat
            iv % rain (nlocal(rain)) % stn_loc % elv = platform % stn % elv

            iv%info(rain)%levels(nlocal(rain))    = nlevels
            iv%info(rain)%name(nlocal(rain))      = platform%info%name
            iv%info(rain)%platform(nlocal(rain))  = platform%info%platform
            iv%info(rain)%id(nlocal(rain))        = platform%info%id
            iv%info(rain)%date_char(nlocal(rain)) = platform%info%date_char
            iv%info(rain)%lat(:,nlocal(rain))     = platform%info%lat
            iv%info(rain)%lon(:,nlocal(rain))     = platform%info%lon
            iv%info(rain)%elv(nlocal(rain))       = platform%info%elv
            iv%info(rain)%pstar(nlocal(rain))     = platform%info%pstar

            iv%info(rain)%slp(nlocal(rain))           = platform%loc%slp
            iv%info(rain)%pw(nlocal(rain))            = platform%loc%pw
            iv%info(rain)%x(:,nlocal(rain))           = platform%loc%x
            iv%info(rain)%y(:,nlocal(rain))           = platform%loc%y 
            iv%info(rain)%i(:,nlocal(rain))           = platform%loc%i 
            iv%info(rain)%j(:,nlocal(rain))           = platform%loc%j 
            iv%info(rain)%dx(:,nlocal(rain))          = platform%loc%dx
            iv%info(rain)%dxm(:,nlocal(rain))         = platform%loc%dxm
            iv%info(rain)%dy(:,nlocal(rain))          = platform%loc%dy
            iv%info(rain)%dym(:,nlocal(rain))         = platform%loc%dym
            iv%info(rain)%proc_domain(:,nlocal(rain)) = platform%loc%proc_domain

            iv%info(rain)%obs_global_index(nlocal(rain)) = ntotal(rain)

            iv % rain (nlocal(rain)) % height    = platform % each(1) % height
            iv % rain (nlocal(rain)) % height_qc = platform % each(1) % height_qc
            iv % rain (nlocal(rain)) % rain      = platform % each(1) % rain

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

   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_read_obs_rain")


end subroutine da_read_obs_rain


