subroutine da_scan_obs_rain (iv, filename, ifgat)

   !---------------------------------------------------------------------------
   ! Purpose: Scan the rain observation file
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(len=*),  intent(in)    :: filename
   integer,           intent(in)    :: ifgat
   integer                       :: i, j, n, iost, nlevels, fm
   integer                       :: file_rain
   integer                       :: iunit

   type (rain_single_level_type) :: platform

   character (len = 120)         :: char_file_rain
   character (len = 120)         :: fmt_name
   character (len = 160)         :: info_string
   logical                       :: outside, outside_all
   integer                       :: n_dup, ndup

   real*8                        :: obs_time
   integer                       :: iyear, imonth, iday, ihour, imin

   ! for thinning
   real                          :: dlat_earth,dlon_earth,crit
   integer                       :: itt,itx,iout
   logical                       :: iuse
   integer                       :: tp, nlocal
   real, allocatable             :: in(:), out(:)

   integer                      :: num_outside_all, num_outside_time, num_thinned, num_report

   if (trace_use) call da_trace_entry("da_scan_obs_rain")

   num_report       = 0
   num_outside_all  = 0
   num_outside_time = 0
   num_thinned      = 0
   tp               = iv%info(rain)%plocal(ifgat-1)
   nlocal           = 0

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
      if (trace_use) call da_trace_exit("da_scan_obs_rain")
      return
   end if

   ! 2. read rainfall data
   ! ===================

   ! 2.1 read first line
   !     ---------------

   read (unit=iunit, fmt = '(A)', iostat = iost) char_file_rain

   ! 2.2 process error

   if (iost /= 0) then
      ! Does matter if present and unreadable
      call da_error(__FILE__,__LINE__, &
         (/"Cannot read rainfall file"/))
   end if

   ! 2.3read header info

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

   ! 2.3 total rainfall data info

   read (unit=char_file_rain (8:14),fmt='(I7)', iostat = iost) file_rain

   ! 2.4 skip one lines

   read (unit=iunit, fmt = '(A)', iostat = iost)

   ! 3. read rain data

   reports:   do n = 1, file_rain

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
         write(unit=stdout, fmt = '(A12,1X,A19,1X,I6,2(F12.3,2X),F8.1,1X,A5)') &
            platform % info % platform,  &
            platform % info % date_char, &
            platform % info % levels,    &
            platform % info % lat,       &
            platform % info % lon,       &
            platform % info % elv,       &
            platform % info % id
      end if         

      read(unit=platform % info % platform (4:6), fmt='(I3)') fm

      num_report = num_report+1

      ! 3.2 read rainfall data 

      platform%each (1) = rain_each_type(missing_r, missing, -1.0,&
         field_type(missing_r, missing, missing_r, missing, missing_r))

      read (unit = iunit, fmt = '(F12.3,F12.3,I4,F12.3)') &
         platform % each (1) % height,             &
         platform % each (1) % rain % inv,         &
         platform % each (1) % rain % qc,          &
         platform % each (1) % rain % error       

     ! 3.3 Check if outside of the time range:

      read (platform%info%date_char,'(i4,4(1x,i2))') &
            iyear, imonth, iday, ihour, imin
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
      if ( obs_time < time_slots(0) .or. &
           obs_time >= time_slots(num_fgat_time) ) then
           num_outside_time = num_outside_time + 1
         cycle reports
      endif

      call da_llxy (platform%info, platform%loc, outside, outside_all)

      nlevels = platform%info%levels

      if (outside_all) then
         num_outside_all = num_outside_all + 1
         cycle reports
      end if

      dlat_earth = platform%info%lat
      dlon_earth = platform%info%lon
      if (dlon_earth < 0.0) dlon_earth = dlon_earth + 360.0
      if (dlon_earth >= 360.0) dlon_earth = dlon_earth - 360.0
      dlat_earth = dlat_earth * deg2rad
      dlon_earth = dlon_earth * deg2rad

      ! 3.4  Loop over duplicating obs for global
      n_dup = 1
      if (global .and. &
         (platform%loc%i == ids .or. platform%loc%i == ide)) n_dup= 2
  
      if (test_transforms) ndup = 1
   
      do n_dup = 1, n_dup
         select case (fm)

         case (129)
            if (iv%info(rain)%nlocal > max_rain_input) then
               write(unit=message(1),fmt='(A,I6,A,I6)') &
                  ' rain #= ',iv%info(rain)%nlocal, ' > max_rain_input = ', max_rain_input
               call da_error(__FILE__,__LINE__,message(1:1))
            end if
            iv%info(rain)%ntotal = iv%info(rain)%ntotal + 1
            if (outside) cycle reports
            if ( thin_rainobs ) then
               crit = 1.0
               call map2grids(rain,ifgat,dlat_earth,dlon_earth,crit,iv%info(rain)%nlocal,itx,1,itt,iout,iuse)
               if ( .not. iuse ) then
                  num_thinned = num_thinned + 1
                  cycle reports
               end if
            else
               iv%info(rain)%nlocal = iv%info(rain)%nlocal + 1
            endif

         case default;
            write(unit=message(1), fmt='(a)') 'unsaved obs found:'
            write(unit=message(2), fmt='(2a)') &
               'platform%info%platform=', platform%info%platform
            write(unit=message(3), fmt='(a, i3)') &
                 'platform%info%levels=', platform%info%levels
            call da_warning(__FILE__,__LINE__,message(1:3))
            cycle reports
         end select

         iv%info(rain)%max_lev = 1 

      end do        !  loop over duplicate
   end do reports

   ! thinning check
   if ( thin_rainobs ) then
     if ( iv%info(rain)%ntotal > 0 ) then

#ifdef DM_PARALLEL
         ! Get minimum crit and associated processor index.
         allocate ( in  (thinning_grid(rain,ifgat)%itxmax) )
         allocate ( out (thinning_grid(rain,ifgat)%itxmax) )
         do i = 1, thinning_grid(rain,ifgat)%itxmax
            in(i) = thinning_grid(rain,ifgat)%score_crit(i)
         end do

         call mpi_reduce(in, out, thinning_grid(rain,ifgat)%itxmax, true_mpi_real, mpi_min, root, comm, ierr)
         call wrf_dm_bcast_real (out, thinning_grid(rain,ifgat)%itxmax)

         do i = 1, thinning_grid(rain,ifgat)%itxmax
            if ( abs(out(i)-thinning_grid(rain,ifgat)%score_crit(i)) > 1.0E-10 ) then
               thinning_grid(rain,ifgat)%ibest_obs(i) = 0
            end if
         end do

         thinning_grid(rain,ifgat)%score_crit(:) = out(:)
               
         deallocate( in  )
         deallocate( out )
#endif         
            
         do j = (1+tp), iv%info(rain)%nlocal
            do i = 1, thinning_grid(rain,ifgat)%itxmax
               if ( thinning_grid(rain,ifgat)%ibest_obs(i) == j .and.         &
                    thinning_grid(rain,ifgat)%score_crit(i) < 9.99e6 ) then
                  nlocal = nlocal + 1
                  exit
               end if
            end do
         end do

      num_thinned = num_thinned + iv%info(rain)%nlocal - nlocal
      iv%info(rain)%nlocal = tp + nlocal
      end if 
   end if  ! thin_rainobs

   write(unit=message(1),fmt='(A,4(1x,i7))') &
      'da_scan_obs_rain: num_report, num_outside_all, num_outside_time, num_thinned: ', &
      num_report, num_outside_all, num_outside_time, num_thinned
   call da_message(message(1:1))

   close (iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_scan_obs_rain")


end subroutine da_scan_obs_rain


