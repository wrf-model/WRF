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

module module_2ddata

  interface get_field
     module procedure get_field_2d ! , get_field_3d
  end interface

  type fieldinfo_type
     character(len=256) :: name
     character(len=256) :: units
     character(len=256) :: desc
     character(len=10)  :: date
     integer :: level
  end type fieldinfo_type
  type(fieldinfo_type) :: fieldinfo

  type gridinfo_type
     integer :: iproj
     integer :: idim, jdim
     real :: lat1, lon1, xlonc, dxm, truelat1, truelat2
  end type gridinfo_type

contains

  subroutine error_handler(status, failure, success)
    use netcdf
    !
    ! Check the error flag from a NetCDF function call, and print appropriate
    ! error message.
    !
    implicit none
    integer,                    intent(in) :: status
    character(len=*), optional, intent(in) :: failure
    character(len=*), optional, intent(in) :: success

    if (status .ne. NF90_NOERR) then
       if (present(failure)) then
          write(*,'(/," ***** ", A)') failure
       endif
       write(*,'(" ***** ",A,/)') nf90_strerror(status)
       stop 'Stopped'
    endif

    if (present(success)) then
       write(*,'(A)') success
    endif

  end subroutine error_handler


!
!#############################################################################################################
!

  subroutine get_field_2d(flnm_template, nowdate, fldname, fldptr, gridinfo, level)
    use netcdf
    use kwm_string_utilities
    implicit none

    character(len=*), intent(in) :: flnm_template
    character(len=*), intent(in) :: nowdate
    character(len=*), intent(in) :: fldname
    real, pointer, dimension(:,:) :: fldptr
    type(gridinfo_type), intent(out) :: gridinfo
    integer,          intent(in) :: level

    character(len=256) :: flnm
    integer :: ierr, ncid, varid, dimid, i
    integer :: ndims
    integer, dimension(NF90_MAX_VAR_DIMS) :: dimids
    integer :: soillayers_dimid
    logical :: threed

    flnm = flnm_template
    call strrep(flnm, "<yyyymmddhh>", nowdate)
    call strrep(flnm, "<hh>", nowdate(9:10))

    ierr = nf90_open(flnm, NF90_NOWRITE, ncid)
    call error_handler(ierr, "Problem opening file '"//trim(flnm)//"'")

    call get_gridinfo(ncid, gridinfo%iproj, gridinfo%lat1, gridinfo%lon1, &
         gridinfo%xlonc, gridinfo%dxm, gridinfo%truelat1, gridinfo%truelat2, &
         gridinfo%idim, gridinfo%jdim)

    ierr = nf90_inq_varid(ncid, fldname, varid)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

! Find information about the dimensions of this variable, so we can determine whether
! we're looking at a 2-d field or a 3-d field (not counting the time dimension).

    ierr = nf90_inq_dimid(ncid, "soil_layers_stag", soillayers_dimid)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
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

! So let's get the field

    allocate(fldptr(gridinfo%idim,gridinfo%jdim))

    if (threed) then
       ierr = nf90_get_var(ncid, varid, fldptr, start = (/1,1,level/), &
            count=(/gridinfo%idim, gridinfo%jdim,1/) )
       fieldinfo%level = level
    else
       ierr = nf90_get_var(ncid, varid, fldptr)
    endif
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

! And get the units string

    fieldinfo%units = " "
    ierr = nf90_get_att(ncid, varid, "units", fieldinfo%units)
    call error_handler(ierr, 'Attribute "units"')
    do i = 1, len(fieldinfo%units)
       if (ichar(fieldinfo%units(i:i)) == 0) fieldinfo%units(i:i) = " "
    enddo

! And get the description string.

    fieldinfo%desc = " "
    ierr = nf90_get_att(ncid, varid, "description", fieldinfo%desc)
    call error_handler(ierr, "Attribute 'Description'")
    do i = 1, len(fieldinfo%desc)
       if (ichar(fieldinfo%desc(i:i)) == 0) fieldinfo%desc(i:i) = " "
    enddo

    fieldinfo%name = fldname
    fieldinfo%date = nowdate

  end subroutine get_field_2d

!
!#############################################################################################################
!

  subroutine get_gridinfo(ncid, iproj, lat1, lon1, xlonc, dxm, truelat1, truelat2, idim, jdim)
    use netcdf
    implicit none
    integer, intent(in) :: ncid
    integer, intent(out) :: iproj
    integer, intent(out) :: idim, jdim
    real, intent(out) :: lat1, lon1, xlonc, dxm, truelat1, truelat2

    integer :: ierr, dimid

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "MAP_PROJ", iproj)
    call error_handler(ierr, "Attribute 'MAP_PROJ'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "LAT1", lat1)
    call error_handler(ierr, "Attribute 'LAT1'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "LON1", lon1)
    call error_handler(ierr, "Attribute 'LON1'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "DX", dxm)
    call error_handler(ierr, "Attribute 'DX'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT1", truelat1)
    call error_handler(ierr, "Attribute 'TRUELAT1'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT2", truelat2)
    call error_handler(ierr, "Attribute 'TRUELAT2'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "STAND_LON", xlonc)
    call error_handler(ierr, "Attribute 'STAND_LON'")

    ierr = nf90_inq_dimid(ncid, "west_east", dimid)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

    ierr = nf90_inquire_dimension(ncid, dimid, len=idim)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

    ierr = nf90_inq_dimid(ncid, "south_north", dimid)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif

    ierr = nf90_inquire_dimension(ncid, dimid, len=jdim)
    if (ierr /= NF90_NOERR) then
       print *, trim(nf90_strerror(ierr))
       stop
    endif
  end subroutine get_gridinfo

!
!#############################################################################################################
!

end module module_2ddata

