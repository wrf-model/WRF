subroutine datint(fuldates, nful, hstart, ntimes, interval, out_format, prefix)
!                                                                             !
!*****************************************************************************!
!                                                                             !
!   interpolate missing data in time
!    out_format: requested output format
!                                                                             !
!*****************************************************************************!

  use gridinfo
  use storage_module
  use module_debug
  use misc_definitions_module

  implicit none
  integer :: nful
  integer :: interval
  character(len=*), dimension(nful) :: fuldates
  character(len=*) :: hstart
  integer :: ntimes

  character(len=24) :: hdate = "0000-00-00_00:00:00.0000"
  character(len=24) :: hdate_output, jdate
  character(len=9) :: field
  character(len=25) :: units
  character(len=46) :: desc
  character(LEN=3)  :: out_format
  character(LEN=MAX_FILENAME_LEN)  :: prefix
  real :: xfcst

  real :: level
  real, allocatable, dimension(:,:) :: scr2d, bfr2d
  integer :: iful, intervala, intervalb, ifv
  real :: awt
  integer :: itime

! DATELEN:  length of date strings to use for our output file names.
  integer :: datelen

! Decide the length of date strings to use for output file names.  
! DATELEN is 13 for hours, 16 for minutes, and 19 for seconds.
  if (mod(interval,3600) == 0) then
     datelen = 13
  else if (mod(interval, 60) == 0) then
     datelen = 16
  else
     datelen = 19
  end if

  call mprintf(.true.,STDOUT,"Subroutine DATINT: Interpolating 3-d files to fill in any missing data...")
  call mprintf(.true.,LOGFILE,"Subroutine DATINT: Interpolating 3-d files to fill in any missing data...")

  TIMELOOP : do itime = 1, ntimes
     call geth_newdate(hdate(1:19), hstart(1:19), (itime-1)*interval)
     call mprintf(.true.,STDOUT,"Looking for data at time %s",s1=hdate(1:datelen))
     call mprintf(.true.,LOGFILE,"Looking for data at time %s",s1=hdate(1:datelen))
     do iful = 1, nful
        if (fuldates(iful).eq.hdate) then
	   call mprintf(.true.,STDOUT,"Found file:      %s:%s", &
	         s1=trim(prefix),s2=hdate(1:datelen))
	   call mprintf(.true.,LOGFILE,"Found file:      %s:%s", &
	         s1=trim(prefix),s2=hdate(1:datelen))
           cycle TIMELOOP
        else if ((fuldates(iful).lt.hdate) .and. &
             (fuldates(iful+1).gt.hdate) )then

	   call mprintf(.true.,STDOUT,"Found surrounding files:      %s: %s  and %s: %s", &
	       s1=trim(prefix),s2=fuldates(iful)(1:datelen), &
	       s3=trim(prefix),s4=fuldates(iful+1)(1:datelen))
	   call mprintf(.true.,LOGFILE,"Found surrounding files:      %s: %s  and %s: %s", &
	       s1=trim(prefix),s2=fuldates(iful)(1:datelen), &
	       s3=trim(prefix),s4=fuldates(iful+1)(1:datelen))
	   call mprintf(.true.,STDOUT,"Interpolating to create file:      %s: %s", &
	         s1=trim(prefix),s2=hdate(1:datelen))
	   call mprintf(.true.,LOGFILE,"Interpolating to create file:      %s: %s", &
	         s1=trim(prefix),s2=hdate(1:datelen))
           call geth_idts(hdate(1:19), fuldates(iful)(1:19), intervalA)
	   call mprintf(.true.,STDOUT,"A Time Difference = %f",f1=float(intervalA) / 3600.)
	   call mprintf(.true.,LOGFILE,"A Time Difference = %f",f1=float(intervalA) / 3600.)
           call geth_idts(fuldates(iful+1)(1:19), hdate(1:19), intervalB)
	   call mprintf(.true.,STDOUT,"B Time Difference = %f",f1=float(intervalB) / 3600.)
	   call mprintf(.true.,LOGFILE,"B Time Difference = %f",f1=float(intervalB) / 3600.)
           AWT = 1. - (float(intervalA)/float(intervalA+intervalB))

           open(10, file=trim(prefix)//':'//fuldates(iful)(1:datelen), form='unformatted', &
                status='old')
           call clear_storage
           READLOOP1 : do
              read(10, end=44) ifv
	      if ( ifv .eq. 5) then     ! WPS
                read (10) jdate, xfcst, map%source, field, units, desc, level, &
	             map%nx, map%ny, map%igrid
	        select case (map%igrid)
                case (0, 4)
                   read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx, map%r_earth
                case (3)
                   read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                        map%lov, map%truelat1, map%truelat2, map%r_earth
                case (5)
                   read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                        map%lov, map%truelat1, map%r_earth
                case default
                  call mprintf(.true.,ERROR, &
                    "Unrecognized map%%igrid: %i in DATINT 1",i1=map%igrid)
                end select
                read (10) map%grid_wind
	      else if ( ifv .eq. 4 ) then          ! SI
                read (10) jdate, xfcst, map%source, field, units, desc, level, &
	              map%nx, map%ny, map%igrid
	        select case (map%igrid)
                case (0, 4)
                   read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx
                case (3)
                   read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                        map%lov, map%truelat1, map%truelat2
                case (5)
                   read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                        map%lov, map%truelat1
                case default
                  call mprintf(.true.,ERROR, &
                    "Unrecognized map%%igrid: %i in DATINT 2",i1=map%igrid)
                end select
	      else if ( ifv .eq. 3 ) then          ! MM5
                read(10) jdate, xfcst, field, units, desc, level,&
                     map%nx, map%ny, map%igrid
	        select case (map%igrid)
	        case (3)      ! lamcon
                   read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                        map%truelat1, map%truelat2
	        case (5)      ! Polar Stereographic
                   read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                        map%truelat1
	        case (0, 4)      ! lat/lon
                   read (10) map%lat1, map%lon1, map%dy, map%dx
	        case (1)      ! Mercator
                   read (10) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
                case default
                  call mprintf(.true.,ERROR, &
                    "Unrecognized map%%igrid: %i in DATINT 3",i1=map%igrid)
                end select 
	      else
                call mprintf(.true.,ERROR, &
                  "Unknown out_format: %i in DATINT ",i1=ifv)
              endif
              allocate(scr2d(map%nx, map%ny))
              read (10) scr2d
              call put_storage(nint(level), field, scr2d, map%nx, map%ny)
              deallocate(scr2d)
           enddo READLOOP1
44         close(10)

           open(10, file=trim(prefix)//':'//fuldates(iful+1)(1:datelen), status='old', &
                form = 'unformatted')
           open(11, file=trim(prefix)//':'//hdate(1:datelen), status='new', form='unformatted')
           READLOOP2 : do
              read (10,END=45) ifv
              if ( ifv .eq. 5) then     ! WPS
                read (10) jdate, xfcst, map%source, field, units, desc, level, &
                      map%nx, map%ny, map%igrid
	        select case (map%igrid)
	        case (0, 4)
                   read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx, map%r_earth
	        case (3)
                   read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                        map%lov, map%truelat1, map%truelat2, map%r_earth
	        case (5)
                   read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1, map%r_earth
                case default
                   call mprintf(.true.,ERROR, &
                     "Unrecognized map%%igrid: %i in DATINT ",i1=map%igrid)
                end select
                read (10) map%grid_wind
	      else if ( ifv .eq. 4 ) then          ! SI
                read (10) jdate, xfcst, map%source, field, units, desc, level, &
                      map%nx, map%ny, map%igrid
	        select case (map%igrid)
		case (0, 4)
                   read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx
		case (1)
                   read(10) map%startloc, map%lat1, map%lon1, map%dy, map%dx, map%truelat1
		case (3)
                   read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1, map%truelat2
		case (5)
                   read (10) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1
                case default
                   call mprintf(.true.,ERROR, &
                     "Unrecognized map%%igrid: %i in DATINT ",i1=map%igrid)
                end select

              else if ( ifv .eq. 3 ) then          ! MM5
                read(10) jdate, xfcst, field, units, desc, level,&
                     map%nx, map%ny, map%igrid
	        select case (map%igrid)
		case (3)    ! lamcon
                   read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                        map%truelat1, map%truelat2
	        case (5)    ! Polar Stereographic
                   read (10) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1
	        case (0, 4)    ! lat/lon
                   read (10) map%lat1, map%lon1, map%dy, map%dx
	        case (1)    ! Mercator
                   read (10) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
                case default
                   call mprintf(.true.,ERROR, &
                     "Unrecognized map%%igrid: %i in DATINT ",i1=map%igrid)
                end select

              else
                call mprintf(.true.,ERROR, &
                  "Unknown out_format: %i in DATINT ",i1=ifv)
              endif

              allocate(scr2d(map%nx, map%ny))
              read (10) scr2d
              if (is_there(nint(level), field)) then
                 allocate(bfr2d(map%nx,map%ny))
                 call get_storage(nint(level), field, bfr2d, map%nx, map%ny)
                 scr2d = bfr2d * (AWT) + scr2d * (1.-AWT)
                 hdate_output = hdate

		 if (out_format(1:2) .eq. 'SI') then
                 write(11) ifv
                 write(11) hdate_output, xfcst, map%source, field, units, desc, &
                      level, map%nx, map%ny, map%igrid
                 if (map%igrid == 0 .or. map%igrid == 4) then
                    write(11) map%startloc, map%lat1, map%lon1, map%dy, map%dx
                 elseif (map%igrid == 1) then
                    write(11) map%startloc, map%lat1, map%lon1, map%dy, map%dx, map%truelat1
                 elseif (map%igrid == 3) then
                    write (11) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                         map%lov, map%truelat1, map%truelat2
                 elseif (map%igrid == 5) then
                    write (11) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                         map%lov, map%truelat1
                 else
                   call mprintf(.true.,ERROR, &
                     "Unrecognized map%%igrid: %i in DATINT ",i1=map%igrid)
                 endif

		 else if (out_format(1:2) .eq. 'WP') then
                 write(11) ifv
                 write(11) hdate_output, xfcst, map%source, field, units, desc, &
                      level, map%nx, map%ny, map%igrid
                 if (map%igrid == 0 .or. map%igrid == 4) then
                    write(11) map%startloc, map%lat1, map%lon1, map%dy, map%dx, &
		              map%r_earth
                 elseif (map%igrid == 1) then
                    write(11) map%startloc, map%lat1, map%lon1, map%dy, map%dx, &
		              map%truelat1, map%r_earth
                 elseif (map%igrid == 3) then
                    write (11) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                         map%lov, map%truelat1, map%truelat2, map%r_earth
                 elseif (map%igrid == 5) then
                    write (11) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                         map%lov, map%truelat1, map%r_earth
                 else
                   call mprintf(.true.,ERROR, &
                     "Unrecognized map%%igrid: %i in DATINT ",i1=map%igrid)
                 endif
		 write(11) map%grid_wind

		 else if (out_format(1:2) .eq. 'MM') then
                 write (11) ifv
                 write (11) hdate_output, xfcst, field, units, Desc, level,&
                   map%nx, map%ny, map%igrid
                 if (map%igrid .eq. 3) then ! lamcon
                   write (11) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1, map%truelat2
                 elseif (map%igrid .eq. 5) then ! Polar Stereographic
                   write (11) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1
                 elseif (map%igrid .eq. 0 .or. map%igrid .eq. 4)then ! lat/lon
                   write (11) map%lat1, map%lon1, map%dy, map%dx
                 elseif (map%igrid.eq.1)then ! Mercator
                   write (11) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
                 else
                   call mprintf(.true.,ERROR, &
                     "Unrecognized map%%igrid: %i in DATINT ",i1=map%igrid)
                 endif
                 endif
                 write(11) scr2d
              else
                 call mprintf(.true.,ERROR, &
                  "hdate = %s , fuldates = %s %s, Field = %s",s1=hdate,s2=fuldates(iful),s3=fuldates(iful+1),s4=field)
              endif
              deallocate(scr2d, bfr2d)
           enddo READLOOP2
45         close(10)
           close(11)
           cycle TIMELOOP
        endif
     enddo

     call mprintf(.true.,ERROR, &
        "Data not found: %s",s1=hdate)
     
  enddo TIMELOOP

  call mprintf(.true.,STDOUT, &
   "End Subroutine DATINT.")
  call mprintf(.true.,LOGFILE, &
   "End Subroutine DATINT.")

end subroutine datint
