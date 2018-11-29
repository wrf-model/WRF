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

module pt_data

contains

  subroutine get_point_data(fldname, lat, lon, flnm_template, nowdate, &
       xval, level, iloc_return, jloc_return)

    use netcdf
    use kwm_string_utilities
    implicit none

    character(len=*),      intent(in) :: fldname
    real,                  intent(in) :: lat
    real,                  intent(in) :: lon
    character(len=*),      intent(in) :: flnm_template
    character(len=*),      intent(in) :: nowdate
    integer,               intent(in) :: level
    real,                  intent(out) :: xval
    integer,               intent(out) :: iloc_return, jloc_return

    character(len=256) :: flnm
    integer :: ierr, ncid

    integer :: iproj = -1
    real, save :: lat1, lon1, xlonc, dxm, truelat1, truelat2
    real, save :: xloc, yloc
    integer, save :: iloc, jloc
    character(len=2), parameter, dimension(3) :: project = (/"LC", "ST", "ME"/)

    flnm = flnm_template
    call strrep(flnm, "<yyyymmddhh>", nowdate)

    ierr = nf90_open(flnm, NF90_NOWRITE, ncid)
    call errhandler(ierr, "file:  "//trim(flnm))

    if (iproj < 0) then

       call gridinfo(ncid, iproj, lat1, lon1, xlonc, dxm, truelat1, truelat2)

    endif

!KWM       print*, 'iproj = ', iproj
!KWM       print*, 'lat1/lon1 = ', lat1, lon1
!KWM       print*, 'xlonc = ', xlonc
!KWM       print*, 'dxm = ', dxm
!KWM       print*, 'truelat1, truelat2 = ', truelat1, truelat2

       call lltoxy_generic(lat, lon, xloc, yloc, &
            project(iproj), dxm*0.001, lat1, lon1, 1.5, 1.5, &
            xlonc, truelat1, truelat2)

       iloc = nint(xloc)
       jloc = nint(yloc)
       iloc_return = iloc
       jloc_return = jloc

       ! print*, 'xloc, yloc = ', xloc, yloc, iloc, jloc


    call get_value(ncid, trim(fldname), iloc, jloc, level, xval)

    ierr = nf90_close(ncid)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif


  end subroutine get_point_data

  subroutine get_value(ncid, name, i, j, level, xval)
    use netcdf
    implicit none
    integer, intent(in) :: ncid, i, j
    character(len=*), intent(in) :: name
    integer, intent(in) :: level
    real, intent(out) :: xval

    integer :: varid, ierr, ndims, soillayers_dimid
    integer, dimension(3) :: nstart
    integer, dimension(NF90_MAX_VAR_DIMS) :: dimids
    logical :: threed

    ierr = nf90_inq_varid(ncid, name, varid)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

! Find information about the dimensions of this variable, so we can determine whether
! we're looking at a 2-d field or a 3-d field (not counting the time dimension).

    ierr = nf90_inq_dimid(ncid, "soil_layers_stag", soillayers_dimid)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr)), "  ", "soil_layers_stag"
       soillayers_dimid = 1
       ! stop
    endif


    ierr = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

    ! If the <dimids> array contains an element equal to <soillayers_dimid>, then
    ! this is a 3-d dataset.

    if (any(dimids(1:ndims)==soillayers_dimid)) then
       threed = .TRUE.
    else
       threed = .FALSE.
    endif

    nstart = (/ i, j, level /)

    ierr = nf90_get_var(ncid, varid, xval, start=nstart)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

  end subroutine get_value

  subroutine get_value_vector(ncid, name, i, j, xval)
    use netcdf
    implicit none
    integer, intent(in) :: ncid, i, j
    character(len=*), intent(in) :: name
    real, dimension(4), intent(out) :: xval

    integer :: varid, ierr
    integer, dimension(3) :: nstart
    integer, dimension(3) :: ncount

    ierr = nf90_inq_varid(ncid, name, varid)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

    nstart = (/ i, j, 1 /)

    ncount = (/ 1, 1, 4 /)

    ierr = nf90_get_var(ncid, varid, xval, start=nstart, count=ncount)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

  end subroutine get_value_vector

  subroutine gridinfo(ncid, iproj, lat1, lon1, xlonc, dxm, truelat1, truelat2)
    use netcdf
    implicit none
    integer, intent(in) :: ncid
    integer, intent(out) :: iproj
    real, intent(out) :: lat1, lon1, xlonc, dxm, truelat1, truelat2

    integer :: ierr

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "MAP_PROJ", iproj)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr)), "  ", "MAP_PROJ"
       stop
    endif

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "LAT1", lat1)
    if (ierr /= NF90_NOERR) then
       ierr = nf90_get_att(ncid, NF90_GLOBAL, "LA1", lat1)
       if (ierr /= NF90_NOERR) then
          print *, trim(nf90_strerror(ierr)), "  ", "LAT1 or LA1"
          stop
       endif
    endif

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "LON1", lon1)
    if (ierr /= NF90_NOERR) then
       ierr = nf90_get_att(ncid, NF90_GLOBAL, "LO1", lon1)
       if (ierr /= NF90_NOERR) then
          print *, trim(nf90_strerror(ierr)), "  ", "LON1 or LO1"
          stop
       endif
    endif

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "DX", dxm)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT1", truelat1)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT2", truelat2)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "STAND_LON", xlonc)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

  end subroutine gridinfo

  subroutine errhandler(ival, str)
    use netcdf
    implicit none
    integer, intent(in) :: ival
    character(len=*), intent(in), optional :: str

    if (ival == NF90_NOERR) return

    print*, "------------------------------------------------------------"
    print *, trim(nf90_strerror(ival))
    if (present(str)) print *, str
    print*, "------------------------------------------------------------"
    stop
  end subroutine errhandler

end module pt_data

