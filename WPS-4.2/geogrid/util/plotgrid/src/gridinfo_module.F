!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE GRIDINFO_MODULE
!
! This module handles (i.e., acquires, stores, and makes available) all data
!   describing the model domains to be processed.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module gridinfo_module

   use misc_definitions_module
   use module_debug
 
   ! Parameters
   integer, parameter :: MAX_DOMAINS = 21
 
   ! Variables
   integer :: interval_seconds, max_dom, io_form_input, io_form_output, debug_level
   integer, dimension(MAX_DOMAINS) :: subgrid_ratio_x, subgrid_ratio_y
   integer :: process_only_bdy
   character (len=MAX_FILENAME_LEN) :: opt_output_from_geogrid_path, &
                          opt_output_from_metgrid_path, opt_metgrid_tbl_path 
   character (len=128), dimension(MAX_DOMAINS) :: start_date, end_date
   character (len=MAX_FILENAME_LEN), dimension(MAX_DOMAINS) :: fg_name, constants_name
   logical :: do_tiled_input, do_tiled_output, nocolons
   logical, dimension(MAX_DOMAINS) :: grid_is_active
   character (len=1) :: gridtype
 
   contains
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_namelist_params
   !
   ! Purpose: Read namelist parameters.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_namelist_params()
 
      implicit none
  
      ! Local variables
      integer :: i, io_form_geogrid, io_form_metgrid
      integer, dimension(MAX_DOMAINS) :: start_year, start_month, start_day, start_hour, start_minute, start_second, &
                                         end_year, end_month, end_day, end_hour, end_minute, end_second
      logical, dimension(MAX_DOMAINS) :: active_grid
      integer :: funit
      logical :: is_used
      character (len=3) :: wrf_core

      namelist /share/ wrf_core, max_dom, start_date, end_date, &
                        start_year, end_year, start_month, end_month, &
                        start_day, end_day, start_hour, end_hour, &
                        start_minute, end_minute, start_second, end_second, &
                        interval_seconds, io_form_geogrid, opt_output_from_geogrid_path, &
                        debug_level, active_grid, nocolons, &
                        subgrid_ratio_x, subgrid_ratio_y
      namelist /metgrid/ io_form_metgrid, fg_name, constants_name, process_only_bdy, opt_output_from_metgrid_path, &
                         opt_metgrid_tbl_path
        
      ! Set defaults
      io_form_geogrid = 2
      io_form_metgrid = 2
      max_dom = 1
      wrf_core = 'ARW'
      debug_level = 0
      nocolons = .false.
      do i=1,MAX_DOMAINS
         fg_name(i) = '*'
         constants_name(i) = '*'
         start_year(i) = 0
         start_month(i) = 0
         start_day(i) = 0
         start_hour(i) = 0
         start_minute(i) = 0
         start_second(i) = 0
         end_year(i) = 0
         end_month(i) = 0
         end_day(i) = 0
         end_hour(i) = 0
         end_minute(i) = 0
         end_second(i) = 0
         start_date(i) = '0000-00-00_00:00:00'
         end_date(i) = '0000-00-00_00:00:00'
         active_grid(i) = .true.
         subgrid_ratio_x(i) = 1
         subgrid_ratio_y(i) = 1
      end do
      process_only_bdy = 0
      opt_output_from_geogrid_path = './'
      opt_output_from_metgrid_path = './'
      opt_metgrid_tbl_path = 'metgrid/'
      interval_seconds = INVALID
  
      ! Read parameters from Fortran namelist
      do funit=10,100
         inquire(unit=funit, opened=is_used)
         if (.not. is_used) exit
      end do
      open(funit,file='namelist.wps',status='old',form='formatted',err=1000)
      read(funit,share)
      read(funit,metgrid)
      close(funit)

! BUG: Better handle debug_level in module_debug
      if ( debug_level .gt. 100 ) then
         call set_debug_level(DEBUG)
      else
         call set_debug_level(WARN)
      end if

      call mprintf(.true.,LOGFILE,'Using the following namelist variables:')
      call mprintf(.true.,LOGFILE,'&SHARE')
      call mprintf(.true.,LOGFILE,'  WRF_CORE         = %s',s1=wrf_core)
      call mprintf(.true.,LOGFILE,'  MAX_DOM          = %i',i1=max_dom)
      call mprintf(.true.,LOGFILE,'  START_YEAR       = %i',i1=start_year(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_year(i))
      end do
      call mprintf(.true.,LOGFILE,'  START_MONTH      = %i',i1=start_month(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_month(i))
      end do
      call mprintf(.true.,LOGFILE,'  START_DAY        = %i',i1=start_day(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_day(i))
      end do
      call mprintf(.true.,LOGFILE,'  START_HOUR       = %i',i1=start_hour(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_hour(i))
      end do
      call mprintf(.true.,LOGFILE,'  START_MINUTE     = %i',i1=start_minute(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_minute(i))
      end do
      call mprintf(.true.,LOGFILE,'  START_SECOND     = %i',i1=start_second(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=start_second(i))
      end do
      call mprintf(.true.,LOGFILE,'  END_YEAR         = %i',i1=end_year(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_year(i))
      end do
      call mprintf(.true.,LOGFILE,'  END_MONTH        = %i',i1=end_month(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_month(i))
      end do
      call mprintf(.true.,LOGFILE,'  END_DAY          = %i',i1=end_day(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_day(i))
      end do
      call mprintf(.true.,LOGFILE,'  END_HOUR         = %i',i1=end_hour(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_hour(i))
      end do
      call mprintf(.true.,LOGFILE,'  END_MINUTE       = %i',i1=end_minute(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_minute(i))
      end do
      call mprintf(.true.,LOGFILE,'  END_SECOND       = %i',i1=end_second(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=end_second(i))
      end do
      call mprintf(.true.,LOGFILE,'  START_DATE       = %s',s1=start_date(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %s',s1=start_date(i))
      end do
      call mprintf(.true.,LOGFILE,'  END_DATE         = %s',s1=end_date(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %s',s1=end_date(i))
      end do
      call mprintf(.true.,LOGFILE,'  INTERVAL_SECONDS = %i',i1=interval_seconds)
      call mprintf(.true.,LOGFILE,'  IO_FORM_GEOGRID  = %i',i1=io_form_geogrid)
      call mprintf(.true.,LOGFILE,'  OPT_OUTPUT_FROM_GEOGRID_PATH = %s',s1=opt_output_from_geogrid_path)
      call mprintf(.true.,LOGFILE,'  SUBGRID_RATIO_X  = %i',i1=subgrid_ratio_x(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=subgrid_ratio_x(i))
      enddo
      call mprintf(.true.,LOGFILE,'  SUBGRID_RATIO_Y  = %i',i1=subgrid_ratio_y(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %i',i1=subgrid_ratio_y(i))
      enddo
      call mprintf(.true.,LOGFILE,'  DEBUG_LEVEL      = %i',i1=debug_level)
      call mprintf(.true.,LOGFILE,'  ACTIVE_GRID      = %l',l1=active_grid(1))
      do i=2,max_dom
         call mprintf(.true.,LOGFILE,'                   = %l',l1=active_grid(i))
      end do
      call mprintf(.true.,LOGFILE,'  NOCOLONS         = %l',l1=nocolons)
      call mprintf(.true.,LOGFILE,'/')

      call mprintf(.true.,LOGFILE,'&METGRID')
      do i=1,MAX_DOMAINS
         if (i == 1) then
            if (fg_name(i) == '*') then
               call mprintf(.true.,LOGFILE,'  FG_NAME               = ')
            else
               call mprintf(.true.,LOGFILE,'  FG_NAME               = %s',s1=fg_name(i))
            end if
         else
            if (fg_name(i) == '*') exit
            call mprintf(.true.,LOGFILE,'                        = %s',s1=fg_name(i))
         end if
      end do
      do i=1,MAX_DOMAINS
         if (i == 1) then
            if (constants_name(i) == '*') then
               call mprintf(.true.,LOGFILE,'  CONSTANTS_NAME        = ')
            else
               call mprintf(.true.,LOGFILE,'  CONSTANTS_NAME        = %s',s1=constants_name(i))
            end if
         else
            if (constants_name(i) == '*') exit
            call mprintf(.true.,LOGFILE,'                        = %s',s1=constants_name(i))
         end if
      end do
      call mprintf(.true.,LOGFILE,'  IO_FORM_METGRID       = %i',i1=io_form_metgrid)
      call mprintf(.true.,LOGFILE,'  PROCESS_ONLY_BDY      = %i',i1=process_only_bdy)
      call mprintf(.true.,LOGFILE,'  OPT_OUTPUT_FROM_METGRID_PATH = %s',s1=opt_output_from_metgrid_path)
      call mprintf(.true.,LOGFILE,'  OPT_METGRID_TBL_PATH  = %s',s1=opt_metgrid_tbl_path)
      call mprintf(.true.,LOGFILE,'/')


      ! Convert wrf_core to uppercase letters
      do i=1,3
         if (ichar(wrf_core(i:i)) >= ichar('a') .and. ichar(wrf_core(i:i)) <= ichar('z') ) &
             wrf_core(i:i) = char(ichar(wrf_core(i:i))-ichar('a')+ichar('A'))
      end do

      ! Before doing anything else, we must have a valid grid type 
      gridtype = ' '
      if (wrf_core == 'ARW') then
         gridtype = 'C'
      else if (wrf_core == 'NMM') then
         gridtype = 'E'
      end if

      call mprintf(gridtype /= 'C' .and. gridtype /= 'E', ERROR, &
                   'A valid wrf_core must be specified in the namelist. '// &
                   'Currently, only "ARW" and "NMM" are supported.')

      call mprintf(max_dom > MAX_DOMAINS, ERROR, &
                   'In namelist, max_dom must be <= %i. To run with more'// &
                   ' than %i domains, increase the MAX_DOMAINS parameter.', &
                   i1=MAX_DOMAINS, i2=MAX_DOMAINS)

      call mprintf(gridtype /= 'C' .and. process_only_bdy /= 0, ERROR, &
                   'The use of process_only_bdy is only currently supported in the "ARW" core. '// &
                   'For "NMM", please set process_only_bdy to 0 in the namelist.')
  
      ! Handle IO_FORM+100
      if (io_form_geogrid > 100) then
         io_form_geogrid = io_form_geogrid - 100
         do_tiled_input = .true.
      else
         do_tiled_input = .false.
      end if
      if (io_form_metgrid > 100) then
         io_form_metgrid = io_form_metgrid - 100
         do_tiled_output = .true.
      else
         do_tiled_output = .false.
      end if
  
      ! Check for valid io_form_geogrid
      if ( &
#ifdef IO_BINARY
          io_form_geogrid /= BINARY .and. & 
#endif
#ifdef IO_NETCDF
          io_form_geogrid /= NETCDF .and. & 
#endif
#ifdef IO_GRIB1
          io_form_geogrid /= GRIB1 .and. & 
#endif
          .true. ) then
         call mprintf(.true.,WARN,'Valid io_form_geogrid values are:')
#ifdef IO_BINARY
         call mprintf(.true.,WARN,'       %i (=BINARY)',i1=BINARY)
#endif
#ifdef IO_NETCDF
         call mprintf(.true.,WARN,'       %i (=NETCDF)',i1=NETCDF)
#endif
#ifdef IO_GRIB1
         call mprintf(.true.,WARN,'       %i (=GRIB1)',i1=GRIB1)
#endif
         call mprintf(.true.,ERROR,'No valid value for io_form_geogrid was specified in the namelist.')
      end if
      io_form_input = io_form_geogrid
  
      ! Check for valid io_form_metgrid
      if ( &
#ifdef IO_BINARY
          io_form_metgrid /= BINARY .and. &
#endif
#ifdef IO_NETCDF
          io_form_metgrid /= NETCDF .and. &
#endif
#ifdef IO_GRIB1
          io_form_metgrid /= GRIB1 .and. &
#endif
          .true. ) then
         call mprintf(.true.,WARN,'Valid io_form_metgrid values are:')
#ifdef IO_BINARY
         call mprintf(.true.,WARN,'       %i (=BINARY)',i1=BINARY)
#endif
#ifdef IO_NETCDF
         call mprintf(.true.,WARN,'       %i (=NETCDF)',i1=NETCDF)
#endif
#ifdef IO_GRIB1
         call mprintf(.true.,WARN,'       %i (=GRIB1)',i1=GRIB1)
#endif
         call mprintf(.true.,ERROR,'No valid value for io_form_metgrid was specified in the namelist.')
      end if
      io_form_output = io_form_metgrid
  
      if (start_date(1) == '0000-00-00_00:00:00') then
         do i=1,max_dom
            ! Build starting date string
            write(start_date(i), '(i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2)') &
               start_year(i),'-',start_month(i),'-',start_day(i),'_',start_hour(i),':',start_minute(i),':',start_second(i)
     
            ! Build ending date string
            write(end_date(i), '(i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2)') &
               end_year(i),'-',end_month(i),'-',end_day(i),'_',end_hour(i),':',end_minute(i),':',end_second(i)
         end do
      end if
  

      ! Paths need to end with a /
      i = len_trim(opt_metgrid_tbl_path)
      if (opt_metgrid_tbl_path(i:i) /= '/') then
         opt_metgrid_tbl_path(i+1:i+1) = '/'
      end if
  
      i = len_trim(opt_output_from_geogrid_path)
      if (opt_output_from_geogrid_path(i:i) /= '/') then
         opt_output_from_geogrid_path(i+1:i+1) = '/'
      end if
  
      i = len_trim(opt_output_from_metgrid_path)
      if (opt_output_from_metgrid_path(i:i) /= '/') then
         opt_output_from_metgrid_path(i+1:i+1) = '/'
      end if


      ! Blank strings should be set to flag values
      do i=1,max_dom
         if (len_trim(constants_name(i)) == 0) then
            constants_name(i) = '*'
         end if
         if (len_trim(fg_name(i)) == 0) then
            fg_name(i) = '*'
         end if
      end do

      do i=1,max_dom
         grid_is_active(i) = active_grid(i)
      end do

      return
  
 1000 call mprintf(.true.,ERROR,'Error opening file namelist.wps')
 
   end subroutine get_namelist_params
  
end module gridinfo_module
