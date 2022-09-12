subroutine da_scan_obs_chem_sfc (iv, filename, grid)

   !-------------------------------------------------------------------------
   ! Purpose:        Read surface chem IC observation files
   !
   ! History: 03/2019  Creation (Wei Sun)
   !
   !          06/2021  Add maximum thresholds for each species (Soyoung Ha)
   ! Caution: chem_cv_options == 10 or 20 assumes all six species in [ug/m3]
   !          chem_cv_options == 108 assumes pm25 and pm10 in [ug/m3], but
   !                                         so2, no2, o3, co in [ppmv]
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(len=*),  intent(in)    :: filename
   type(domain),     intent(in)     :: grid     ! first guess state.

   ! chem_cv_options == 108: Maximum thresholds for observation values - treat as missing above these values
   ! FIXME: We may want to put these maximum thresholds in namelist later.
   real, dimension(6)  :: max_pm = (/500.,999.,2.0, 2.0, 2.0, 50./)  ! [ug/m3, ug/m3, ppmv, ppmv, ppmv, ppmv]
   real                         :: pm25, pm10, so2, no2, o3, co

   character (len =   6)        :: SiteID
   character (len =  10)        :: fmt_name

   character (len = 160)        :: info_string

   integer                      :: i, j, iost, nlevels, old_nlevels, fm,iunit
   integer                      :: ichem, ipm
   real                         :: dattmp,lattmp,lontmp

   type (singl_level_type)      :: platform
   logical                      :: outside
   logical                      :: outside_all
   integer                      :: surface_level
   real                         :: height_error, u_comp, v_comp
   integer                      :: nlocal(num_ob_indexes)
   integer                      :: ilocal(num_ob_indexes)
   integer                      :: ntotal(num_ob_indexes)

   integer                      :: ndup, n, report, obs_index

   real*8                       :: obs_time, analysis_time
   integer                      :: iyear, imonth, iday, ihour, imin, isite
   real                         :: tdiff, dlat_earth,dlon_earth,crit
   integer                      :: itt,itx,iout
   real, allocatable            :: in(:), out(:)
   logical                      :: found, iuse, thin_3d, is_surface
   integer                      :: i1,j1,k, levs
   real                         :: dx,dy,dxm,dym,zk
   real                         :: v_p(kms:kme),v_h(kms:kme)

   if (trace_use) call da_trace_entry("da_scan_obs_chem_sfc")

 ! Initialize counts

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
         "Error",iost," opening chemic surface obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      call da_free_unit(iunit)
      if (trace_use) call da_trace_exit("da_scan_obs_chem_sfc")
      return
   end if

   allocate ( platform%chem (num_chemic_surf) )
   obs_index = chemic_surf

   imin=0     ! minutes

   ! loop over records
   ! -----------------

   report = 0 ! report number in file

   reports: &
   do

      report = report+1

      ! Initialization
      ! =============================
      platform%chem(:)%inv = missing_r
      platform%chem(:)%qc  = missing
      platform%chem(:)%error = missing_r

      ! read station general info
      ! =============================

      if (chem_cv_options == 10) then !!!! ium data !!!!

        write(*,*) 'read ium data'
            read (iunit, *, iostat =  iost)  &
            iyear, imonth, iday, ihour, imin, &
            platform%info%lat,                &
            platform%info%lon,                &
            platform%chem(PARAM_FIRST_SCALAR)%inv        !!! pm25 only !!!
            write (platform%info%date_char,'(i4,3(i2))')  iyear, imonth, iday, ihour
            platform%info%id = "333"

      else if (chem_cv_options == 20) then

         if (chemicda_opt == 1) then
            read (iunit, *, iostat =  iost)  &
            iyear, imonth, iday, ihour, imin, &
            platform%info%lat,                &
            platform%info%lon,                &
            platform%chem(PARAM_FIRST_SCALAR)%inv        !!! pm25 only !!!
         else if (chemicda_opt == 2) then
            read (iunit, *, iostat =  iost)  &
            iyear, imonth, iday, ihour, imin, &
            platform%info%lat,                &
            platform%info%lon,                &
            platform%chem(PARAM_FIRST_SCALAR)%inv,  &    !!! pm25 read !!!
            platform%chem(PARAM_FIRST_SCALAR)%inv        !!! pm10 only !!!
         else if (chemicda_opt == 3) then
            read (iunit, *, iostat =  iost)  &
            iyear, imonth, iday, ihour, imin, &
            platform%info%lat,                &
            platform%info%lon,                &
            platform%chem(PARAM_FIRST_SCALAR)%inv,  &
            platform%chem(PARAM_FIRST_SCALAR+1)%inv        !!! pm10 & pm25 !!!
         else if (chemicda_opt == 4) then
            read (iunit, *, iostat =  iost)  &
            iyear, imonth, iday, ihour, imin, &
            platform%info%lat,                &
            platform%info%lon,                &
            platform%chem(PARAM_FIRST_SCALAR)%inv,  &
            platform%chem(PARAM_FIRST_SCALAR)%inv,  &
            platform%chem(PARAM_FIRST_SCALAR)%inv,  &      !!! so2 !!!
            platform%chem(PARAM_FIRST_SCALAR+1)%inv,  &    !!! no2 !!!
            platform%chem(PARAM_FIRST_SCALAR+2)%inv        !!! o3 !!!
            write (platform%info%date_char,'(i4,3(i2))')  iyear, imonth, iday, ihour
         else if (chemicda_opt == 5) then
            read (iunit, *, iostat =  iost)  &
            iyear, imonth, iday, ihour, imin, &
            platform%info%lat,                &
            platform%info%lon,                &
            platform%chem(PARAM_FIRST_SCALAR)%inv,  &      !!! pm25 !!!
            platform%chem(PARAM_FIRST_SCALAR+1)%inv,  &    !!! pm10 !!!
            platform%chem(PARAM_FIRST_SCALAR+2)%inv,  &    !!! so2 !!! 
            platform%chem(PARAM_FIRST_SCALAR+3)%inv,  &    !!! no2 !!! 
            platform%chem(PARAM_FIRST_SCALAR+4)%inv,  &    !!! o3 !!! 
            platform%chem(PARAM_FIRST_SCALAR+5)%inv        !!! co !!!
            write (platform%info%date_char,'(i4,3(i2))')  iyear, imonth, iday, ihour
            platform%info%id = "333"
         end if

      else if (chem_cv_options == 108) then

      read (iunit, fmt='(A6,F10.6,1X,F10.6,1X,I4,3I2,2F9.1,4F9.3)', iostat=iost)  &
            SiteID,                                   &
            platform%info%lat,                        &
            platform%info%lon,                        &
            iyear, imonth, iday, ihour,               &
            pm25,  pm10,   so2,  no2,   o3, co

      platform%info%id = trim(SiteID)
      read(SiteID,'(I6)') isite
      write(platform%info%date_char,'(i4,3(i2.2))')  iyear, imonth, iday, ihour

      select case ( chemicda_opt )
      case ( 1 )
            platform%chem(PARAM_FIRST_SCALAR  )%inv = pm25
            platform%info%name                      = "pm25"
      case ( 2 )
            platform%chem(PARAM_FIRST_SCALAR  )%inv = pm10
            platform%info%name                      = "pm10"
      case ( 3 )
            platform%chem(PARAM_FIRST_SCALAR  )%inv = pm25
            platform%chem(PARAM_FIRST_SCALAR+1)%inv = pm10
            platform%info%name                      = "all_pm"
      case ( 4 )
            platform%chem(PARAM_FIRST_SCALAR  )%inv = so2
            platform%chem(PARAM_FIRST_SCALAR+1)%inv = no2
            platform%chem(PARAM_FIRST_SCALAR+2)%inv = o3
            platform%chem(PARAM_FIRST_SCALAR+3)%inv = co
            platform%info%name                      = "gas"
      case ( 5 )
            platform%chem(PARAM_FIRST_SCALAR  )%inv = pm25
            platform%chem(PARAM_FIRST_SCALAR+1)%inv = pm10
            platform%chem(PARAM_FIRST_SCALAR+2)%inv = so2
            platform%chem(PARAM_FIRST_SCALAR+3)%inv = no2
            platform%chem(PARAM_FIRST_SCALAR+4)%inv = o3
            platform%chem(PARAM_FIRST_SCALAR+5)%inv = co
            platform%info%name                      = "all"
      case default
            platform%chem(PARAM_FIRST_SCALAR  )%inv = pm25
            platform%info%name                      = ""
      end select

      ! Sanity check
      !!!!!!!!!!!!!!!!!!!
      do ichem = PARAM_FIRST_SCALAR, num_chemic_surf
         ipm = ichem - PARAM_FIRST_SCALAR + 1
         if (platform%chem(ichem)%inv<=0.or.abs(platform%chem(ichem)%inv)>=max_pm(ipm)) then
             platform%chem(ichem)%inv=missing_r
         else
             platform%chem(ichem)%qc = 0
         endif
      end do

      end if !(chem_cv_options == 108)

      if (iost /= 0) then
         ! FIX? This is expected, but its unclear how we handle failure
         ! here without assuming the fortran2003 convention on
         ! error statuses
         exit reports
      end if

      if (platform%info%lon == 180.0  ) platform%info%lon =-180.000 
      ! Fix funny wind direction at Poles
      if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
         platform%info%lon = 0.0
      end if

      ! read model location
      ! =========================

      ! Check if outside of the time range:

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
               "N on proc", myproc," at x/y:", platform%loc%x,platform%loc%y
         end if
      end if

      call da_get_julian_time (iyear,imonth,iday,ihour,imin,analysis_time)
      tdiff = abs((obs_time - analysis_time)-0.02)
      dlat_earth = platform%info%lat
      dlon_earth = platform%info%lon
      if (dlon_earth < 0.0) dlon_earth = dlon_earth + 360.0
      if (dlon_earth >= 360.0) dlon_earth = dlon_earth - 360.0
      dlat_earth = dlat_earth * deg2rad
      dlon_earth = dlon_earth * deg2rad

      levs = 1

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
      
      dup_loop: do n = 1, ndup
         is_surface=.true.
         if (use_chemic_surfobs) then
            if (.not. use_chemic_surfobs .or. ntotal(chemic_surf) == max_synop_input  ) cycle reports
            if (n==1) iv%info(chemic_surf)%ntotal = iv%info(chemic_surf)%ntotal + 1
            if (outside) cycle reports
            if ( thin_conv_ascii ) then
               crit = tdiff
!!!               call map2grids_conv(chemic_surf,dlat_earth,dlon_earth,crit,nlocal(chemic_surf),itx,1,itt,ilocal(chemic_surf),iuse)
               call map2grids_conv(chemic_surf,dlat_earth,dlon_earth,crit,iv%info(chemic_surf)%nlocal,itx,1,itt,iout,iuse)
               if ( .not. iuse ) cycle reports
            else
                iv%info(chemic_surf)%nlocal = iv%info(chemic_surf)%nlocal + 1
            end if

	 else

            write(unit=message(1), fmt='(a)') 'unsaved obs found:'
            write(unit=message(2), fmt='(2a)') &
               'platform%info%platform=', platform%info%platform
            write(unit=message(3), fmt='(a, i3)') &
               'platform%info%levels=', platform%info%levels
            call da_warning(__FILE__,__LINE__,message(1:3))
            cycle

         end if

      end do dup_loop

   end do reports

   close(iunit)

   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_scan_obs_chem_sfc")

end subroutine da_scan_obs_chem_sfc


