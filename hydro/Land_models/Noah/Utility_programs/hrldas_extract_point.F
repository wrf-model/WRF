!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>


module module_hrldas_netcdf
  use netcdf
  implicit none

  character(len=2), dimension(3), parameter :: projection_list = (/ "LC", "ST", "ME" /)
  type hrldas_mapinfo_type
     character(len=2) :: projection
     integer :: iproj
     real    :: lat1
     real    :: lon1
     real    :: xlonc
     real    :: dxm
     real    :: truelat1
     real    :: truelat2
  end type hrldas_mapinfo_type

contains

!------------------------------------------------------------------------------
! Subroutine get_map_info
!------------------------------------------------------------------------------

  subroutine get_map_info(flnm, ncid, mapinfo)
    use netcdf
    implicit none

    character(len=*),          intent(in)  :: flnm
    integer,                   intent(out) :: ncid
    type(hrldas_mapinfo_type), intent(out) :: mapinfo

    integer :: ierr

    ierr = nf90_open(flnm, NF90_NOWRITE, ncid)
    call handle_error(ierr, "file:  "//trim(flnm))

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "MAP_PROJ", mapinfo%iproj)
    call handle_error(ierr, "MAP_PROJ:  ")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "LAT1", mapinfo%lat1)
    call handle_warning(ierr, "LAT1:  ")

    if (ierr /= NF90_NOERR) then
       write(*,'("Looking instead for LA1",/)')
       ierr = nf90_get_att(ncid, NF90_GLOBAL, "LA1", mapinfo%lat1)
       call handle_error(ierr, "LA1:  ")
    endif

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "LON1", mapinfo%lon1)
    call handle_warning(ierr, "LON1:  ")

    if (ierr /= NF90_NOERR) then
       write(*,'("Looking instead for LO1",/)')
       ierr = nf90_get_att(ncid, NF90_GLOBAL, "LO1", mapinfo%lon1)
       call handle_error(ierr, "LO1:  ")
    endif

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "DX", mapinfo%dxm)
    call handle_error(ierr, "DX:  ")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT1", mapinfo%truelat1)
    call handle_error(ierr, "TRUELAT1:  ")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT2", mapinfo%truelat2)
    call handle_error(ierr, "TRUELAT2:  ")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "STAND_LON", mapinfo%xlonc)
    call handle_error(ierr, "STAND_LON:  ")

    mapinfo%projection = projection_list(mapinfo%iproj)

  end subroutine get_map_info

!------------------------------------------------------------------------------
! Subroutine close_hrldas_file
!------------------------------------------------------------------------------

  subroutine close_hrldas_file(ncid)
    implicit none
    integer, intent(in) :: ncid
    integer :: ierr
    ierr = nf90_close(ncid)
    call handle_error(ierr, "Trying to close file.")
  end subroutine close_hrldas_file

!------------------------------------------------------------------------------
! Subroutine lltoxy_hrldas
!------------------------------------------------------------------------------

  subroutine lltoxy_hrldas(lat, lon, x, y, mapinfo)
    use module_llxy_generic
    implicit none
    real, intent(in) :: lat, lon
    real, intent(out) :: x, y
    type(hrldas_mapinfo_type) :: mapinfo

    call lltoxy_generic(lat, lon, x, y, &
         mapinfo%projection, mapinfo%dxm*1.E-3, mapinfo%lat1, mapinfo%lon1, 1.0, 1.0, &
         mapinfo%xlonc, mapinfo%truelat1, mapinfo%truelat2)
    
  end subroutine lltoxy_hrldas

!------------------------------------------------------------------------------
! Subroutine xytoll_hrldas
!------------------------------------------------------------------------------

  subroutine xytoll_hrldas(x, y, lat, lon, mapinfo)
    use module_llxy_generic
    implicit none
    real, intent(in) :: lat, lon
    real, intent(out) :: x, y
    type(hrldas_mapinfo_type) :: mapinfo

    call xytoll_generic(x, y, lat, lon, &
         mapinfo%projection, mapinfo%dxm*1.E-3, mapinfo%lat1, mapinfo%lon1, 1.0, 1.0, &
         mapinfo%xlonc, mapinfo%truelat1, mapinfo%truelat2)
    
  end subroutine xytoll_hrldas

!------------------------------------------------------------------------------
! Subroutine handle_error
!------------------------------------------------------------------------------

  subroutine handle_error(ival, str)
    implicit none
    integer, intent(in) :: ival
    character(len=*), intent(in) :: str

    if (ival == NF90_NOERR) return

    print*, "----ERROR---------------------------------------------------"
    print *, trim(nf90_strerror(ival))
    if (str /= "") print *, str
    print*, "------------------------------------------------------------"
    stop
  end subroutine handle_error

!------------------------------------------------------------------------------
! Subroutine handle_warning
!------------------------------------------------------------------------------

  subroutine handle_warning(ival, str)
    implicit none
    integer, intent(in) :: ival
    character(len=*), intent(in) :: str

    if (ival == NF90_NOERR) return

    print*, "----WARNING-------------------------------------------------"
    print *, trim(nf90_strerror(ival))
    if (str /= "") print *, str
    print*, "------------------------------------------------------------"
  end subroutine handle_warning

end module module_hrldas_netcdf

!------------------------------------------------------------------------------
! ****************************************************************************
!------------------------------------------------------------------------------

program hrldas_extract_point
!
! Given a lat/lon pair or an x/y pair, find the nearest grid point
! and print out all data from that point over a specified period of
! time.
!
  use arguments_module
  use module_date_utilities
  use module_hrldas_netcdf
  implicit none

  real    :: lat
  real    :: lon
  real    :: x
  real    :: y
  integer :: ival
  integer :: jval
  character(len=256) :: flnm
  character(len=256) :: datadir
  integer :: outunit = 10

  integer :: ierr
  integer :: ncid
  type(hrldas_mapinfo_type) :: mapinfo

  character(len=10) :: nowdate, startdate, enddate
  

  arguments_help = '(" [-lat <latitude>] [-lon <longitude>] ",/,&
       &"              [-x <x-coordinate>] [-y <y-coordinate>]",/,&
       & "             -startdate <yyyymmddhh> -enddate <yyyymmddhh> <LDASOUT directory>")'


!------------------------------------------------------------------------------
! Handle the command-line arguments.
!------------------------------------------------------------------------------

  call arg("-startdate",  startdate)
  call arg("-enddate", enddate)
  call arg("-lat", -1.E33, lat)
  call arg("-lon", -1.E33, lon)
  call arg("-x",   -1.E33, x)
  call arg("-y",   -1.E33, y)
  call arg("Null", datadir)

  if ( (lat > -1.E25) .and. (lon > -1.E25)) then
     if ((x > -1.E25) .or. (y > -1.E25)) then
        write(*,'(/,"  ***** Lat/lon specified.  Do not specify x/y *****")')
        call print_help
     endif
  else if ( (x > -1.E25) .and. (y > -1.E25)) then
     if ((lat > -1.E25) .or. (lon > -1.E25)) then
        write(*,'(/,"  ***** X/y specified.  Do not specify lat/lon *****")')
        call print_help
     endif
  else 
     write(*,'(/,"  ***** Point not specified.                         *****")')
     write(*,'(  "  ***** Specify desired point in lat/lon or in x/y.  *****")')
     call print_help
  endif

  if (datadir == "Null") then
     write(*,'(/,"  ***** Directory name not specified. *****")')
     call print_help
  endif

!------------------------------------------------------------------------------
! Start us off at the starting time, and loop until our end time
!------------------------------------------------------------------------------

  nowdate = startdate
  TIMELOOP : do while ( nowdate <= enddate )
     print*, nowdate

     !------------------------------------------------------------------------------
     ! Get map information
     !------------------------------------------------------------------------------

     flnm = trim(datadir)//"/"//nowdate//".LDASOUT_DOMAIN1"
     call get_map_info(trim(flnm), ncid, mapinfo)

     !------------------------------------------------------------------------------
     ! Get integer Ival/Jval values of the requested point.
     !------------------------------------------------------------------------------

     if ( (lat > -1.E25) .and. (lon > -1.E25) ) then

        call lltoxy_hrldas(lat, lon, x, y, mapinfo)
        write(*, '("lat, lon = ", F8.4, 1x, F9.4, ",    x, y = ", F9.4, 1x, F9.4)') &
             lat, lon, x, y

     else

        call xytoll_hrldas(x, y, lat, lon, mapinfo)
        write(*, '("x, y = ", F9.4, 1x, F9.4, ",    lat, lon = ", F8.4, 1x, F9.4)') &
             x, y, lat, lon

     endif
  
     ival = int(x)
     jval = int(y)

     !------------------------------------------------------------------------------
     ! Make the header for the table.
     !------------------------------------------------------------------------------

     if (nowdate == startdate) call make_header(ncid, outunit, lat, lon, ival, jval)

     !------------------------------------------------------------------------------
     ! Loop through the list of variables, pulling out all data at the I/J point.
     !------------------------------------------------------------------------------

     call loop_through_variable_list(ncid, ival, jval, nowdate, outunit)

     !------------------------------------------------------------------------------
     ! Shut things down for this time period before proceeding to the next.
     !------------------------------------------------------------------------------

     call close_hrldas_file(ncid)

     !------------------------------------------------------------------------------
     ! Update our time and repeat the time loop.
     !------------------------------------------------------------------------------

     call geth_newdate(nowdate, nowdate, 1)

  enddo TIMELOOP
  
end program hrldas_extract_point

!------------------------------------------------------------------------------
! Subroutine make_header
!------------------------------------------------------------------------------

subroutine make_header(ncid, outunit, lat, lon, ival, jval)
  use module_hrldas_netcdf
  implicit none
  integer,          intent(in) :: ncid
  integer,          intent(in) :: outunit
  real,             intent(in) :: lat
  real,             intent(in) :: lon
  integer,          intent(in) :: ival
  integer,          intent(in) :: jval

  character(len=NF90_MAX_NAME) :: varname
  integer :: xtype
  integer :: ndims
  integer, dimension(NF90_MAX_VAR_DIMS) :: dimids
  integer :: natts
  integer :: ierr
  integer :: varid
  integer :: nvars
  integer :: idim
  character(len=NF90_MAX_NAME), dimension(NF90_MAX_DIMS) :: dimname
  integer                     , dimension(NF90_MAX_DIMS) :: dimlen
  character(len=18) :: blank = "                  "
  integer :: zdim
  integer :: i
  character(len=18) :: text

  write(outunit,'("lat/lon:  ", F12.6, F12.6, "      Grid I/J: ", I6, I6)') lat, lon, ival, jval


  ierr = nf90_inquire(ncid, nVariables=nvars)
  call handle_error(ierr, "Trying to find number of variables.")

!------------------------------------------------------------------------------
! A preliminary loop through the variables to get the heading for the output files.
!------------------------------------------------------------------------------
  do varid = 1, nvars

     ierr = nf90_inquire_variable(ncid, varid, varname, xtype, ndims, dimids, nAtts)
     call handle_error(ierr, "Inquiring about a variable.")

     do idim = 1, ndims
        ierr = nf90_inquire_dimension(ncid, dimids(idim), dimname(idim), dimlen(idim))
        call handle_error(ierr, "Trying to get dimension information for variable "//trim(varname))
        if (dimname(idim) == "soil_layers_stag") zdim = dimlen(idim)
     enddo

     if (any(dimname == "soil_layers_stag")) then
        do i = 1, zdim
           write(text, '(A, "(",i1,")")') trim(varname), i
           write(outunit, '(A18)', advance="no") blank(1:(18-len_trim(text))/2)//text
        enddo
     else
        write(outunit, '(A18)', advance="no") blank(1:(18-len_trim(varname))/2)//varname
     endif

  enddo
  write(outunit,*)
end subroutine make_header

!------------------------------------------------------------------------------
! Subroutine loop_through_variable_list
!------------------------------------------------------------------------------

subroutine loop_through_variable_list(ncid, ival, jval, nowdate, outunit)
  use module_hrldas_netcdf
  implicit none
  integer,          intent(in) :: ncid
  integer,          intent(in) :: ival
  integer,          intent(in) :: jval
  character(len=*), intent(in) :: nowdate
  integer,          intent(in) :: outunit

  integer                      :: nvars
  integer                      :: varid
  integer                      :: ierr

  integer :: idim
  integer :: i

  ! Information returned from nf90_inquire_variable:
  character(len=NF90_MAX_NAME) :: varname
  integer :: xtype
  integer :: ndims
  integer, dimension(NF90_MAX_VAR_DIMS) :: dimids
  integer :: natts

  character(len=NF90_MAX_NAME) :: dimname
  integer                      :: dimlen

  !
  integer, dimension(NF90_MAX_DIMS) :: start
  integer, dimension(NF90_MAX_DIMS) :: array_count
  real                              :: xval
  real, allocatable, dimension(:)   :: xarr
  integer                           :: idata
  logical                           :: multi_layer

  character(len=1), dimension(19)   :: hdata
  character(len=19)                 :: hdate

!------------------------------------------------------------------------------
! Find the number of variables in the file.
!------------------------------------------------------------------------------

  ierr = nf90_inquire(ncid, nVariables=nvars)
  call handle_error(ierr, "Trying to find number of variables.")
  print*, 'nvars = ', nvars

!------------------------------------------------------------------------------
! Loop through the variables
!------------------------------------------------------------------------------
  do varid = 1, nvars

     ierr = nf90_inquire_variable(ncid, varid, varname, xtype, ndims, dimids, nAtts)
     call handle_error(ierr, "Inquiring about a variable.")

     start = 1
     array_count = 1
     if (allocated(xarr)) deallocate(xarr)
     multi_layer = .FALSE.

!------------------------------------------------------------------------------
! Loop over the number of dimensions on the variable, making sure we get the 
! right indices in the right order.
!------------------------------------------------------------------------------
     do idim = 1, ndims
        ierr = nf90_inquire_dimension(ncid, dimids(idim), dimname, dimlen)
        call handle_error(ierr, "Trying to get dimension information for variable "//trim(varname))

        if (dimname == "west_east") then
           start(idim) = ival
        else if (dimname == "south_north") then
           start(idim) = jval
        else if (dimname == "soil_layers_stag") then
           array_count(idim) = dimlen
           allocate(xarr(dimlen))
           multi_layer = .TRUE.
        else if ( (dimname == "Times") .or. (dimname == "Time") )then
           if (dimlen /= 1) then
              stop "So far, this program only handles single-time per output file.  Sorry."
           endif
           start(idim) = 1
           array_count(idim) = 1
        else if (dimname == "DateStrLen") then
           start(idim) = 1
           array_count(idim) = dimlen
        else
           print*, 'Unrecognzied dimension:  ', trim(dimname)
           stop "Unrecognized dimension."
        endif
     enddo

     if (xtype == NF90_FLOAT) then

        if (multi_layer) then
           ierr = nf90_get_var(ncid, varid, xarr, start(1:ndims), array_count(1:ndims))
           call handle_error(ierr, "Trying to get variable "//trim(varname))
           ! print*, varid, trim(varname), xarr(1:ndims)
           do i = 1, ndims
              write(outunit, '(G18.10)', advance="no") xarr(i)
           enddo
        else
           ierr = nf90_get_var(ncid, varid, xval, start(1:ndims))
           call handle_error(ierr, "Trying to get variable "//trim(varname))
           write(outunit, '(G18.10)', advance="no") xval
           ! print*, varid, trim(varname), xval
        endif

     else if (xtype == NF90_INT) then
        if (multi_layer) stop "No multi-layer integer variables expected."

        ierr = nf90_get_var(ncid, varid, idata, start(1:ndims))
        call handle_error(ierr, "Trying to get variable "//trim(varname))
        write(outunit, '(I18)', advance="no") idata
        
     else if (xtype == NF90_CHAR) then
        ierr = nf90_get_var(ncid, varid, hdata, start, array_count)
        call handle_error(ierr, "Trying to get variable "//trim(varname))
        hdate = " "
        if (hdata(1) == char(0)) then
           hdate = nowdate(1:4)//"-"//nowdate(5:6)//"-"//nowdate(7:8)//"_"//nowdate(9:10)
        else
           do i = 1, 19
              hdate(i:i) = hdata(i)
           enddo
        endif
        write(outunit, '(A18)', advance="no") hdate(1:13)

     else
        print*, varid, trim(varname), " ??? xtype = ", xtype
        stop "Unexpected variable type"
     endif
  enddo
  write(outunit,*)

end subroutine loop_through_variable_list

