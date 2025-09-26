subroutine output(hdate, nlvl, maxlvl, plvl, interval, iflag, out_format, prefix, debug_level)
!                                                                             !
!*****************************************************************************!
!  Write output to a file.
!                                                                             !
!    hdate       :  date string
!    nlvl        :  number of pressure levels
!    maxlvl      :  dimension of the pressure level array (plvl)
!    plvl        :  pressure level array
!    interval    :  period between processing times (seconds)
!    iflag       :  1 = output for ingest into rrpr ; 2 = final intermediate-format output
!    out_format  :  requested output format (WPS, SI, or MM5)
!    prefix      :  file name prefix
!    debug_level :  debug output parameter
!                                                                             !
!*****************************************************************************!

  use table
  use gridinfo
  use storage_module
  use filelist
  use module_debug
  use misc_definitions_module
  use stringutil

  implicit none

  character(LEN=19) :: hdate
  character(LEN=24) :: hdate_output
  character(LEN=3)  :: out_format
  character(LEN=MAX_FILENAME_LEN)  :: prefix
  integer :: iunit = 13

  real, pointer, dimension(:,:) :: scr2d

  integer :: maxlvl
  integer nlvl, debug_level
  real , dimension(maxlvl) :: plvl
  character (LEN=9) :: field
  real :: level
  integer :: sunit = 14
  integer :: interval
  integer :: iflag
! Local Miscellaneous
  integer :: k, n, mm, ilev
  integer :: ii, jj
  real :: maxv, minv
  real :: xplv
  real :: xfcst = 0.
  character (LEN=25) :: units
  character (LEN=46) :: Desc
  character (LEN=9) :: tmp9
  logical lopen

! DATELEN:  length of date strings to use for our output file names.
  integer :: datelen

! Decide the length of date strings to use for output file names.  
! DATELEN is 13 for hours, 16 for minutes, and 19 for seconds.
  if (mod(interval,3600) == 0) then
     datelen = 13
  elseif (mod(interval,60) == 0) then
     datelen = 16
  else
     datelen = 19
  endif
 
  call get_plvls(plvl, maxlvl, nlvl)

  if ( debug_level .ge. 0 ) then
  write(*,119) hdate(1:10), hdate(12:19)
119 format(/,79('#'),//,'Inventory for date = ', A10,1x,A8,/)
  call mprintf(.true.,LOGFILE,"Inventory for date = %s %s",s1=hdate(1:10),s2=hdate(12:19))

  write(*,advance='NO', fmt='("PRES", 2x)')
  write(tmp9,'(a9)') 'PRES'
  call right_justify(tmp9,9)
  call mprintf(.true.,LOGFILE,tmp9,newline=.false.)
  WRTLOOP : do n = 1, maxvar
     do k = 1, n-1
        if (namvar(k).eq.namvar(n)) cycle WRTLOOP
     enddo
     write(*,advance='NO', fmt='(1x,A8)') namvar(n)
     write(tmp9,'(A9)') namvar(n)(1:9)
     call right_justify(tmp9,9)
     call mprintf(.true.,LOGFILE,tmp9,newline=.false.)
  enddo WRTLOOP
  write(*,advance='YES', fmt='(1x)')
  call mprintf(.true.,LOGFILE,' ',newline=.true.)

  write(*,FMT='(79("-"))')
  call mprintf(.true.,LOGFILE,"-------------------------------------------------")
  end if
  KLOOP : do k = 1, nlvl
     if ((iflag.eq.2).and.(plvl(k).gt.200100) .and. (plvl(k).lt.200200)) then
        cycle KLOOP
     endif
     ilev = nint(plvl(k))
     if ( debug_level .ge. 0 ) then
       write(*, advance='NO', FMT='(F6.1)') plvl(k)/100.
       write(tmp9,'(I9)') nint(plvl(k))
       call mprintf(.true.,LOGFILE,'%s ',s1=tmp9,newline=.false.)
     end if
     MLOOP : do mm = 1, maxvar
        do n = 1, mm-1
           if (namvar(mm).eq.namvar(n)) cycle MLOOP
        enddo
        if ( debug_level .ge. 0 ) then
        if (is_there(ilev,namvar(mm))) then
           write(*, advance='NO', FMT='("  X      ")')
	   call mprintf(.true.,LOGFILE,'        X',newline=.false.)
        else
	   if ( plvl(k).gt.200000 ) then
             write(*, advance='NO', FMT='("  O      ")')
	     call mprintf(.true.,LOGFILE,'        O',newline=.false.)
	   else
             write(*, advance='NO', FMT='("         ")')
	     call mprintf(.true.,LOGFILE,'        -',newline=.false.)
	   endif
        endif
        endif
     enddo MLOOP
     if ( debug_level .ge. 0 ) then
     write(*,advance='YES', fmt='(1x)')
     call mprintf(.true.,LOGFILE,' ',newline=.true.)
     endif
  enddo KLOOP
  if ( debug_level .ge. 0 ) then
  write(*,FMT='(79("-"))')
  call mprintf(.true.,LOGFILE,"-------------------------------------------------")
  endif
  
  if (iflag.eq.1) then
     if (nfiles.eq.0) then
        open(iunit, file=trim(get_path(prefix))//'PFILE:'//HDATE(1:datelen), form='unformatted', &
             position='REWIND')
        nfiles = nfiles + 1
        filedates(nfiles)(1:datelen) = hdate(1:datelen)
     else
        DOFILES : do k = 1, nfiles
           if (hdate(1:datelen).eq.filedates(k)(1:datelen)) then
              open(iunit, file=trim(get_path(prefix))//'PFILE:'//HDATE(1:datelen), form='unformatted',&
                   position='APPEND')
           endif
        enddo DOFILES
        inquire (iunit, OPENED=LOPEN)
        if (.not. LOPEN) then
           open(iunit, file=trim(get_path(prefix))//'PFILE:'//HDATE(1:datelen), form='unformatted', &
                position='REWIND')
           nfiles = nfiles + 1
           filedates(nfiles)(1:datelen) = hdate(1:datelen)
        endif
     endif
  else if (iflag.eq.2) then
     open(iunit, file=trim(prefix)//':'//HDATE(1:datelen), form='unformatted', &
          position='REWIND')
  endif

!MGD  if ( debug_level .gt. 100 ) then
!MGD     write(6,*) 'begin nloop'
!MGD  end if
  NLOOP : do n = 1, nlvl

!MGD  if ( debug_level .gt. 100 ) then
!MGD     write(6,*) 'begin outloop'
!MGD  end if
     OUTLOOP : do mm = 1, maxvar
        field = namvar(mm)
        do k = 1, mm-1
           if (field.eq.namvar(k)) cycle OUTLOOP
        enddo
        level = plvl(n)
        if ((iflag.eq.2).and.(level.gt.200100) .and. (level.lt.200200)) then
           cycle NLOOP
        endif
        ilev = nint(level)
        desc = ddesc(mm)
        if (iflag.eq.2) then
           if (desc.eq.' ') cycle OUTLOOP
        endif
        units = dunits(mm)
        if ((iflag.eq.1).or.(iflag.eq.2.and.desc(1:1).ne.' ')) then
          if (is_there(ilev,field)) then 
            call get_dims(ilev, field)

!MGD            if ( debug_level .gt. 100 ) then
!MGD               write(6,*) 'call refr_storage'
!MGD            end if
            call refr_storage(ilev, field, scr2d, map%nx, map%ny)

!MGD            if ( debug_level .gt. 100 ) then
!MGD               write(6,*) 'back from refr'
!MGD               write(6,*) 'out_format = ',out_format
!MGD            end if

	    if (out_format(1:2) .eq. 'SI') then
!MGD              if ( debug_level .gt. 100 ) then
!MGD                 write(6,*) 'writing in SI format'
!MGD              end if
              write(iunit) 4
              hdate_output = hdate
              write (iunit) hdate_output, xfcst, map%source, field, units, &
                   Desc, level, map%nx, map%ny, map%igrid
              if (map%igrid.eq.3) then ! lamcon
                 write (iunit) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1, map%truelat2
              elseif (map%igrid.eq.5) then ! Polar Stereographic
                 write (iunit) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1
              elseif (map%igrid.eq.0 .or. map%igrid.eq.4)then ! lat/lon
                 write (iunit) map%startloc, map%lat1, map%lon1, map%dy, map%dx
              elseif (map%igrid.eq.1)then ! Mercator
                 write (iunit) map%startloc, map%lat1, map%lon1, map%dy, map%dx, &
                      map%truelat1
              else
                 call mprintf(.true.,ERROR, &
                "Unrecognized map%%igrid: %i in subroutine output 1",i1=map%igrid)
              endif
              write (iunit) scr2d
	    else if (out_format(1:2) .eq. 'WP') then   
                call mprintf(.true.,DEBUG, &
         "writing in WPS format  iunit = %i, map%%igrid = %i",i1=iunit,i2=map%igrid)
              write(iunit) 5
              hdate_output = hdate
              write (iunit) hdate_output, xfcst, map%source, field, units, &
                   Desc, level, map%nx, map%ny, map%igrid
              if (map%igrid.eq.3) then ! lamcon
                 write (iunit) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1, map%truelat2, map%r_earth
              elseif (map%igrid.eq.5) then ! Polar Stereographic
                 write (iunit) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                      map%lov, map%truelat1, map%r_earth
              elseif (map%igrid.eq.0 .or. map%igrid.eq.4)then ! lat/lon
                 write (iunit) map%startloc, map%lat1, map%lon1, map%dy, map%dx, &
		      map%r_earth
              elseif (map%igrid.eq.1)then ! Mercator
                 write (iunit) map%startloc, map%lat1, map%lon1, map%dy, map%dx, &
                      map%truelat1, map%r_earth
              elseif (map%igrid.eq.6)then ! CASSINI
                 write (iunit) map%startloc, map%lat1, map%lon1, map%dy, map%dx, &
                      map%lat0, map%lon0, map%r_earth  ! refer to gridinfo.F
              else
                 call mprintf(.true.,ERROR, &
                "Unrecognized map%%igrid: %i in subroutine output 1",i1=map%igrid)
              endif
	      write (iunit) map%grid_wind
              write (iunit) scr2d
	    else if (out_format(1:2) .eq. 'MM') then
!MGD              if ( debug_level .gt. 100 ) then
!MGD	         write(6,*) 'writing in MM5 format'
!MGD              end if
              if (iflag .eq. 2) then  ! make sure the field names are MM5-compatible
                if ( field .eq. 'TT' ) field = 'T'
                if ( field .eq. 'UU' ) field = 'U'
                if ( field .eq. 'VV' ) field = 'V'
                if ( field .eq. 'SNOW' ) field = 'WEASD'
              endif
              write(iunit) 3
              hdate_output = hdate
              write (iunit) hdate_output, xfcst, field, units, Desc, level,&
                   map%nx, map%ny, map%igrid
              if (map%igrid.eq.3) then ! lamcon
                 write (iunit) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1, map%truelat2
              elseif (map%igrid.eq.5) then ! Polar Stereographic
                 write (iunit) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                      map%truelat1
              elseif (map%igrid.eq.0 .or. map%igrid.eq.4)then ! lat/lon
                 write (iunit) map%lat1, map%lon1, map%dy, map%dx
              elseif (map%igrid.eq.1)then ! Mercator
                 write (iunit) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
              else
                 call mprintf(.true.,ERROR, &
                "Unrecognized map%%igrid: %i in subroutine output 1",i1=map%igrid)
              endif
              write (iunit) scr2d
	    endif
              if ( debug_level .gt. 100 ) then
	        call mprintf(.true.,DEBUG, &
	        "hdate = %s,  xfcst = %f ",s1=hdate_output,f1=xfcst)
	        call mprintf(.true.,DEBUG, &
           "map%%source = %s, field = %s, units = %s",s1=map%source,s2=field,s3=units)
	        call mprintf(.true.,DEBUG, &
	            "Desc = %s, level = %f",s1=Desc,f1=level)
	        call mprintf(.true.,DEBUG, &
	            "map%%nx = %i, map%%ny = %i",i1=map%nx,i2=map%ny)
              else if ( debug_level .gt. 0 ) then
	        call mprintf(.true.,STDOUT, &
	      " field = %s, level = %f",s1=field,f1=level)
	        call mprintf(.true.,LOGFILE, &
	      " field = %s, level = %f",s1=field,f1=level)
              end if
              if ( debug_level .gt. 100 ) then
	      maxv = -99999.
	      minv = 999999.
	      do jj = 1, map%ny
	      do ii = 1, map%nx
	        if (scr2d(ii,jj) .gt. maxv) maxv = scr2d(ii,jj)
	        if (scr2d(ii,jj) .lt. minv) minv = scr2d(ii,jj)
	      enddo
	      enddo
	      call mprintf(.true.,DEBUG, &
	         "max value = %f , min value = %f",f1=maxv,f2=minv)
              end if

              nullify(scr2d)

           endif
        endif
     enddo OUTLOOP
  enddo NLOOP

  close(iunit)

end subroutine output

