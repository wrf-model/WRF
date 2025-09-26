subroutine read_namelist(hstart, hend, delta_time, ntimes,&
     ordered_by_date, debug_level, out_format, prefix,    &
     add_lvls, new_plvl_in, interp_type, ec_rec_len, pmin)

  use misc_definitions_module
  use module_debug

  implicit none
  integer , parameter :: maxim_doms = 21
  character(len=200) :: extdataroot, file_name_namelist
  character(len=19) :: hstart, hend
  integer :: delta_time
  integer :: ntimes
  logical :: ordered_by_date
  integer :: debug_level
  real, dimension(:) :: new_plvl_in
  logical :: add_lvls
  integer :: interp_type, ec_rec_len
  real :: pmin

  integer :: ierr
  integer :: idts

! Declare the namelist variables:

  integer , dimension(maxim_doms) :: start_year
  integer , dimension(maxim_doms) :: start_month
  integer , dimension(maxim_doms) :: start_day
  integer , dimension(maxim_doms) :: start_hour
  integer , dimension(maxim_doms) :: start_minute
  integer , dimension(maxim_doms) :: start_second

  integer , dimension(maxim_doms) :: end_year
  integer , dimension(maxim_doms) :: end_month
  integer , dimension(maxim_doms) :: end_day
  integer , dimension(maxim_doms) :: end_hour
  integer , dimension(maxim_doms) :: end_minute
  integer , dimension(maxim_doms) :: end_second

  logical , dimension(maxim_doms) :: active_grid
  integer , dimension(maxim_doms) :: subgrid_ratio_x
  integer , dimension(maxim_doms) :: subgrid_ratio_y

  character (len=128) , dimension(maxim_doms) :: start_date, end_date
  character (len=MAX_FILENAME_LEN) :: opt_output_from_geogrid_path
  integer :: interval_seconds = 0
  character (len=3) :: wrf_core = 'ARW'
  integer :: max_dom, io_form_geogrid

  character(len=3) :: out_format
  character(len=MAX_FILENAME_LEN) :: prefix
  logical :: nocolons

  real :: target_end, incr
  integer :: il

  real, dimension(:), allocatable :: new_plvl

  namelist /share/ wrf_core, max_dom, &
       start_year, start_month, start_day, start_hour, &
       start_minute, start_second, &
       end_year, end_month, end_day, end_hour, &
       end_minute, end_second,&
       interval_seconds, &
       start_date, end_date, &
       io_form_geogrid, opt_output_from_geogrid_path, &
       debug_level, active_grid, &
       subgrid_ratio_x, subgrid_ratio_y, &
       nocolons

  namelist /ungrib/ out_format, &
       ordered_by_date, prefix, &
       add_lvls, new_plvl, interp_type, ec_rec_len, pmin

  allocate(new_plvl(size(new_plvl_in)))

  start_year = 0
  start_month = 0
  start_day = 0
  start_hour = 0
  start_minute = 0
  start_second = 0

  end_year = 0
  end_month = 0
  end_day = 0
  end_hour = 0
  end_minute = 0
  end_second = 0

  ! Set defaults. 
  io_form_geogrid = 2
  max_dom = 1
  wrf_core = 'ARW'
  debug_level = 0
  nocolons = .false.

  add_lvls = .false.
  new_plvl = -99999.
  interp_type = 0
  ec_rec_len = 0
  pmin = 100.    ! default is 1 hPa (48 km)

! Start routine:

! Build the namelist file name:

#ifndef __crayx1
  CALL GETENV('EXT_DATAROOT',extdataroot)
#endif
  file_name_namelist =  'namelist.wps'
 
! Open the namelist file:
  open(10, file=file_name_namelist, status='old', iostat=ierr)
  call mprintf((ierr.ne.0),ERROR,"**** Error opening namelist file namelist.wps")

   REWIND (10) 

  ! set default:
  ordered_by_date = .TRUE.
  start_date(1)(1:4) = '0000'
  end_date(1)(1:4)   = '0000'

  read(10,NML=share)

  if (debug_level.gt.100) then
     call set_debug_level(DEBUG)
  else
     call set_debug_level(WARN)
  end if


! Build the Starting date HSTART and the ending date HEND from the namelist
! date/time information. start_date takes priority over the multi-variable method.

  if ( start_date(1)(1:4) .eq. '0000' ) then
    call build_hdate(hstart, start_year(1), start_month(1), start_day(1), start_hour(1), &
       start_minute(1), start_second(1))
  else
    hstart = start_date(1)(1:19)
  endif
  if ( end_date(1)(1:4) .eq. '0000' ) then
    call build_hdate(hend, end_year(1), end_month(1), end_day(1), end_hour(1), &
       end_minute(1), end_second(1))
  else
    hend = end_date(1)(1:19)
  endif

! Compute the time difference between start date and end date:

  call geth_idts(hend, hstart, idts)

! Check that INTERVAL is greater than zero:

  if (interval_seconds <= 0) then
     call mprintf(.true.,STDOUT,"ERROR STOP IN READ_NAMELIST")
     call mprintf(.true.,STDOUT,"INTERVAL must be greater than zero:")
     call mprintf(.true.,STDOUT,"Start time: %s",s1=hstart)
     call mprintf(.true.,STDOUT,"End time:   %s",s1=hend)
     call mprintf(.true.,STDOUT,"INTERVAL:   %i",i1=interval_seconds)
     call mprintf(.true.,LOGFILE,"ERROR STOP IN READ_NAMELIST")
     call mprintf(.true.,LOGFILE,"INTERVAL must be greater than zero:")
     call mprintf(.true.,LOGFILE,"Start time: %s",s1=hstart)
     call mprintf(.true.,LOGFILE,"End time:   %s",s1=hend)
     call mprintf(.true.,LOGFILE,"INTERVAL:   %i",i1=interval_seconds)
     call mprintf(.true.,ERROR,"Change your namelist, and resubmit")
  endif

! Check that the selected INTERVAL evenly fills difference between 
! start time and end time:

  if ((idts/interval_seconds)*interval_seconds /= idts) then
     call mprintf(.true.,STDOUT,"ERROR STOP IN READ_NAMELIST")
     call mprintf(.true.,STDOUT,"INTERVAL must fit evenly between start time and end time:")
     call mprintf(.true.,STDOUT,"Start time: %s",s1=hstart)
     call mprintf(.true.,STDOUT,"End time:   %s",s1=hend)
     call mprintf(.true.,STDOUT,"INTERVAL  : %i seconds,  %f hours",&
          i1=interval_seconds,f1=float(interval_seconds)/3600.)
     call mprintf(.true.,LOGFILE,"ERROR STOP IN READ_NAMELIST")
     call mprintf(.true.,LOGFILE,"INTERVAL must fit evenly between start time and end time:")
     call mprintf(.true.,LOGFILE,"Start time: %s",s1=hstart)
     call mprintf(.true.,LOGFILE,"End time:   %s",s1=hend)
     call mprintf(.true.,LOGFILE,"INTERVAL  : %i seconds,  %f hours",&
          i1=interval_seconds,f1=float(interval_seconds)/3600.)
     call mprintf(.true.,ERROR,"Change your namelist, and resubmit")
  endif

! Check that start time is not later than end time:

  if (hstart > hend) then
     call mprintf(.true.,STDOUT,"ERROR STOP IN READ_NAMELIST")
     call mprintf(.true.,STDOUT,"Start time must not be later than end time:")
     call mprintf(.true.,STDOUT,"Start time: %s",s1=hstart)
     call mprintf(.true.,STDOUT,"End time:   %s",s1=hend)
     call mprintf(.true.,STDOUT,"INTERVAL:   %i",i1=interval_seconds)
     call mprintf(.true.,LOGFILE,"ERROR STOP IN READ_NAMELIST")
     call mprintf(.true.,LOGFILE,"Start time must not be later than end time:")
     call mprintf(.true.,LOGFILE,"Start time: %s",s1=hstart)
     call mprintf(.true.,LOGFILE,"End time:   %s",s1=hend)
     call mprintf(.true.,LOGFILE,"INTERVAL:   %i",i1=interval_seconds)
     call mprintf(.true.,ERROR,"Change your namelist, and resubmit")
  endif

! Compute the number of time periods to process:

  ntimes = idts/interval_seconds + 1

  call mprintf(.true.,STDOUT, &
  "Start_date =  %s ,      End_date = %s ",s1=hstart,s2=hend)
  call mprintf(.true.,LOGFILE, &
  "Start_date =  %s ,      End_date = %s ",s1=hstart,s2=hend)

  if (debug_level.gt.0) then
     call mprintf(.true.,LOGFILE,"Namelist information (coarse domain): ")
     call mprintf(.true.,LOGFILE,'  START_YEAR       = %i',i1=start_year(1))
     call mprintf(.true.,LOGFILE,'  START_MONTH      = %i',i1=start_month(1))
     call mprintf(.true.,LOGFILE,'  START_DAY        = %i',i1=start_day(1))
     call mprintf(.true.,LOGFILE,'  START_HOUR       = %i',i1=start_hour(1))
!    call mprintf(.true.,LOGFILE,"start_minute = %i",i1=start_minute(1))
!    call mprintf(.true.,LOGFILE,"start_second = %i",i1=start_second(1))
     call mprintf(.true.,LOGFILE,'  END_YEAR         = %i',i1=end_year(1))
     call mprintf(.true.,LOGFILE,'  END_MONTH        = %i',i1=end_month(1))
     call mprintf(.true.,LOGFILE,'  END_DAY          = %i',i1=end_day(1))
     call mprintf(.true.,LOGFILE,'  END_HOUR         = %i',i1=end_hour(1))
!    call mprintf(.true.,LOGFILE,"end_minute = %i",i1=end_minute(1))
!    call mprintf(.true.,LOGFILE,"end_second = %i",i1=end_second(1))
     call mprintf(.true.,LOGFILE,'  START_DATE       = %s',s1=start_date(1))
     call mprintf(.true.,LOGFILE,'  END_DATE         = %s',s1=end_date(1))
     call mprintf(.true.,LOGFILE,'  INTERVAL_SECONDS = %i',i1=interval_seconds)
     call mprintf(.true.,LOGFILE,'  DEBUG_LEVEL      = %i',i1=debug_level)
     call mprintf(.true.,LOGFILE,'/')
  else
     debug_level=0
  endif

  delta_time = interval_seconds

  rewind(10)
  out_format = 'WPS'
  prefix = 'FILE'
  read(10,NML=ungrib,END=100)

     call mprintf(.true.,LOGFILE,'&UNGRIB')
     call mprintf(.true.,LOGFILE,"out_format = %s",s1=out_format)
     if (ordered_by_date) then
       call mprintf(.true.,LOGFILE,"ordered_by_date = %s",s1='TRUE')
     else
       call mprintf(.true.,LOGFILE,"ordered_by_date = %s",s1='FALSE')
     endif
     call mprintf(.true.,LOGFILE,"prefix = %s",s1=trim(prefix))
     call mprintf(.true.,LOGFILE,'/')

100  continue
   if (out_format(1:2) .eq. 'WP' .or. out_format(1:2) .eq. 'wp') then
     out_format = 'WPS'
     call mprintf(.true.,STDOUT,'output format is WPS')
     call mprintf(.true.,LOGFILE,'output format is WPS')
   else if (out_format(1:2) .eq. 'SI' .or. out_format(1:2) .eq. 'si') then
     out_format = 'SI '
     call mprintf(.true.,STDOUT,'output format is SI')
     call mprintf(.true.,LOGFILE,'output format is SI')
   else if (out_format(1:2) .eq. 'MM' .or. out_format(1:2) .eq. 'mm') then
     out_format = 'MM5'
     call mprintf(.true.,STDOUT,'output format is MM5 pregrid')
     call mprintf(.true.,LOGFILE,'output format is MM5 pregrid')
   else 
     call mprintf(.true.,ERROR, &
 'read_namelist: I do not recognize the output format, %s , stopping.',s1=out_format)
   endif

! Check to see if I should create my own set of new_plvl's
  if ( add_lvls .AND. new_plvl(2) > -99999. .AND. new_plvl(2) < 0.0 ) then
    target_end = abs(new_plvl(2))
    incr = new_plvl(3)
    il = 2
    make_plvl : do
      if(il.gt.size(new_plvl)) then
       call mprintf(.true.,ERROR,&
        'Too many new levels specified via new_plvl. Increase maxlvl in ungrib.F')
      end if
      new_plvl(il) = new_plvl(il-1) - incr
      ! If we are past the end of the range of pressures over which new levels
      ! are to be created, then discard the pressure we just calculated. 
      ! This occurs when the user-chosen increment did not evenly divide
      ! the range of pressures over which new pressures were to be added.  
      if ( new_plvl(il) < target_end ) then 
        new_plvl(il) = -99999.
        exit make_plvl
      end if 
      if ( new_plvl(il) == target_end ) exit make_plvl
      il = il + 1
    end do make_plvl
  endif

! Close the namelist file:

  close(10)

  new_plvl_in(:) = new_plvl(:)
  deallocate(new_plvl)
  
end subroutine read_namelist
