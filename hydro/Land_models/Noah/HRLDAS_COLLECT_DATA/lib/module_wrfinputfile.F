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

module module_wrfinputfile
  use netcdf
  use module_llxy
  implicit none
  ! Define a data structure for wrfinput file

  type wrfinput_type
     type(proj_info) :: proj
     integer :: grid_id
     integer :: idim
     integer :: jdim

     character(len=256) :: landuse_dataset  ! "MODIFIED_IGBP_MODIS_NOAH" or "USGS"
     integer :: iswater
     !
     ! Some 2d arrays of grid/geographical information:
     !
     real, pointer, dimension(:,:)   :: lat    ! Grid point latitudes
     real, pointer, dimension(:,:)   :: lon    ! Grid point longitudes
     real, pointer, dimension(:,:)   :: ter    ! Terrain field
     real, pointer, dimension(:,:)   :: use    ! Land-use (vegetation) field
!KWM     real, pointer, dimension(:,:)   :: tmn    ! Deep-soil temperature field
     real, pointer, dimension(:,:,:) :: veg    ! 12-month Green Vegetation Fraction climatology
  end type wrfinput_type

contains

  subroutine read_wrfinput_file(flnm, wrfinput, ierr)
    ! 
    ! Reads the wrfinput file identified by <flnm>, and fills out 
    ! the <wrfinput> structure with the appropriate values.
    !
    implicit none

    character(len=*),     intent(in)  :: flnm
    type (wrfinput_type), intent(out) :: wrfinput
    integer, intent(out)              :: ierr
    ! Local:
    integer                           :: ncid
    integer                           :: iret
    integer                           :: dimid
    integer                           :: map_proj
    real                              :: truelat1
    real                              :: truelat2
    real                              :: stdlon
    real                              :: dx
    real                              :: latinc
    real                              :: loninc
    real                              :: la1
    real                              :: lo1
    real                              :: pole_lat
    real                              :: pole_lon

! Open the NetCDF file.
    iret = nf90_open(flnm, NF90_NOWRITE, ncid)
    call error_handler(iret, "Problem with wrfinput file: "//flnm)

! Find out about dimensions in the file.

    iret = nf90_inq_dimid(ncid, "west_east", dimid)
    call error_handler(iret, "STOP:  Problem finding west_east dimension")
    iret = nf90_inquire_dimension(ncid, dimid, len=wrfinput%idim)
    call error_handler(iret, "STOP:  Problem finding west_east dimension")

    iret = nf90_inq_dimid(ncid, "south_north", dimid)
    call error_handler(iret, "STOP:  Problem finding south_north dimension")
    iret = nf90_inquire_dimension(ncid, dimid, len=wrfinput%jdim)
    call error_handler(iret, "STOP:  Problem finding south_north dimension")

    ! Grid id.  Check for the string "GRID_ID" or the string "grid_id"
    iret = nf90_get_att(ncid, NF90_GLOBAL,"GRID_ID", wrfinput%grid_id)
    if (iret /= 0) then
       iret = nf90_get_att(ncid, NF90_GLOBAL,"grid_id", wrfinput%grid_id)
       call error_handler(iret, "STOP:  nf90_get_att:  'grid_id' or 'GRID_ID' not found.")
    endif

    ! Get Latitude
    allocate(wrfinput%lat(wrfinput%idim, wrfinput%jdim))
    call get_2d("XLAT", ncid, wrfinput%lat, wrfinput%idim, wrfinput%jdim)
    la1 = wrfinput%lat(1,1)

    ! Get Longitude
    allocate(wrfinput%lon(wrfinput%idim, wrfinput%jdim))
    call get_2d("XLONG", ncid, wrfinput%lon, wrfinput%idim, wrfinput%jdim)
    lo1 = wrfinput%lon(1,1)

    ! Get Terrain
    allocate(wrfinput%ter(wrfinput%idim, wrfinput%jdim))
    call get_2d("HGT", ncid, wrfinput%ter, wrfinput%idim, wrfinput%jdim)

    ! Get Land Use categories 
    allocate(wrfinput%use(wrfinput%idim, wrfinput%jdim))
    call get_2d("IVGTYP", ncid, wrfinput%use, wrfinput%idim, wrfinput%jdim)

!KWM  ! Get deep soil Temperature field
!KWM  allocate(wrfinput%tmn(wrfinput%idim, wrfinput%jdim))
!KWM  call get_2d("TMN", ncid, wrfinput%tmn, wrfinput%idim, wrfinput%jdim)

    ! Projection information:

    call map_init(wrfinput%proj)

    iret = nf90_get_att(ncid, NF90_GLOBAL,"MAP_PROJ", map_proj)
    call error_handler(iret, "STOP:  nf90_get_att:  map projection problem")

    if (map_proj == PROJ_LC) then

       iret = nf90_get_att(ncid, NF90_GLOBAL, "STAND_LON", stdlon)
       call error_handler(iret, "STOP:  nf90_get_att:  STAND_LON")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT1" , truelat1)
       call error_handler(iret, "STOP:  nf90_get_att:  TRUELAT1")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT2" , truelat2)
       call error_handler(iret, "STOP:  nf90_get_att:  TRUELAT2")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "DX" , dx)
       call error_handler(iret, "STOP:  nf90_get_att:  DX")

       call map_set(PROJ_LC, wrfinput%proj, lat1=la1, lon1=lo1, knowni=1.0, knownj=1.0, &
            truelat1=truelat1, truelat2=truelat2, stdlon=stdlon, dx=dx)

    else if (map_proj == PROJ_PS) then

       iret = nf90_get_att(ncid, NF90_GLOBAL, "STAND_LON", stdlon)
       call error_handler(iret, "STOP:  nf90_get_att:  STAND_LON")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT1" , truelat1)
       call error_handler(iret, "STOP:  nf90_get_att:  TRUELAT1")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "DX" , dx)
       call error_handler(iret, "STOP:  nf90_get_att:  DX")

       call map_set(PROJ_PS, wrfinput%proj, lat1=la1, lon1=lo1, truelat1=truelat1, &
            knowni=1.0, knownj=1.0, stdlon=stdlon, dx=dx)

    else if (map_proj == PROJ_MERC) then

       iret = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT1" , truelat1)
       call error_handler(iret, "STOP:  nf90_get_att:  TRUELAT1")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "DX" , dx)
       call error_handler(iret, "STOP:  nf90_get_att:  DX")

       call map_set(PROJ_MERC, wrfinput%proj, lat1=la1, lon1=lo1, knowni=1.0, knownj=1.0, &
            truelat1=truelat1, dx=dx)

    else if (map_proj == PROJ_CASSINI) then

       iret = nf90_get_att(ncid, NF90_GLOBAL, "DX" , dx)
       call error_handler(iret, "STOP:  nf90_get_att:  DX")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "POLE_LAT" , pole_lat)
       call error_handler(iret, "STOP:  nf90_get_att:  POLE_LAT")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "POLE_LON" , pole_lon)
       call error_handler(iret, "STOP:  nf90_get_att:  POLE_LON")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "STAND_LON", stdlon)
       call error_handler(iret, "STOP:  nf90_get_att:  STAND_LON")

       ! For Cassini projection, module_llxy assumes a spherical earth
       ! with circumference EARTH_CIRC_M
       latinc = 360.0*dx/EARTH_CIRC_M
       loninc = 360.0*dx/EARTH_CIRC_M

       call map_set(PROJ_CASSINI, wrfinput%proj, lat1=la1, lon1=lo1, latinc=latinc, loninc=loninc, &
            knowni=1.0, knownj=1.0, lat0=pole_lat, lon0=pole_lon, stdlon=stdlon)

    else if (map_proj == PROJ_LATLON) then
       stop "PROJ_LATLON is not a valid map projection for WRF."

       ! iret = nf90_get_att(ncid, NF90_GLOBAL, "DX" , dx)
       ! call error_handler(iret, "STOP:  nf90_get_att:  DX")

       ! For lat/lon projection, module_llxy assumes a spherical earth
       ! with circumference EARTH_CIRC_M
       ! latinc = 360.0*dx/EARTH_CIRC_M
       ! loninc = 360.0*dx/EARTH_CIRC_M

       ! call map_set(PROJ_LATLON, wrfinput%proj, lat1=la1, lon1=lo1, &
       !      latinc=latinc, loninc=loninc, knowni=1.0, knownj=1.0)

    else if (map_proj == PROJ_PS_WGS84) then
       stop "PROJ_PS_WGS84 is not a valid map projection for WRF."
       ! call map_set(PROJ_PS_WGS84, wrfinput%proj, lat1=wrfinput%la1, lon1=wrfinput%lo1, knowni=1.0, knownj=1.0, &
       !     stdlon=wrfinput%lov, dx=wrfinput%dx)
    else if (map_proj == PROJ_GAUSS) then
       stop "PROJ_GAUSS is not a valid map projection for WRF."
       ! call map_set(PROJ_GAUSS, wrfinput%proj, lat1=, lon1=, nlat=, loninc=)
    else if (map_proj == PROJ_CYL) then
       stop "PROJ_CYL is not a valid map projection for WRF."
       ! call map_set(PROJ_CYL, wrfinput%proj, latinc=, loninc=, stdlon=)
    else if (map_proj == PROJ_ALBERS_NAD83) then
       stop "PROJ_ALBERS_NAD83 is not a valid map projection for WRF."
       !call map_set(PROJ_ALBERS_NAD83, wrfinput%proj, lat1=, lon1=, knowni=, knownj=, truelat1=, truelat2=, stdlon=, dx=)
    else if (map_proj == PROJ_ROTLL) then
       stop "PROJ_ROTLL is not a valid map projection for WRF."
       ! call map_set(PROJ_ROTLL, wrfinput%proj, lat1=wrfinput%lat1, lon1=wrfinput%lon1, ixdim=, jydim=, phi=, lambda=)
    else
       write(*,'("Unknown wrfinput map projection:  map_proj = ", I10)') map_proj
       stop
    endif

    iret = nf90_get_att(ncid, NF90_GLOBAL, "ISWATER" , wrfinput%iswater)
    call error_handler(iret, "STOP:  nf90_get_att:  ISWATER")

    iret = nf90_get_att(ncid, NF90_GLOBAL, "MMINLU" , wrfinput%landuse_dataset)
    call error_handler(iret, "STOP:  nf90_get_att:  MMINLU")

    ! Close the file and get out of here

    iret = nf90_close(ncid)
    call error_handler(iret, "STOP:  Problem closing file??")

    ierr = 0
    print*, "Done with subroutine read_wrfinput_file"

  end subroutine read_wrfinput_file

  subroutine get_2d(name, ncid, array, idim, jdim)
    !
    ! From the NetCDF unit <ncid>, read the variable named <name> with  
    ! dimensions <idim> and <jdim>, filling the pre-dimensioned array <array>
    !
    implicit none
    character(len=*),           intent(in)  :: name
    integer,                    intent(in)  :: ncid
    integer,                    intent(in)  :: idim
    integer,                    intent(in)  :: jdim
    real, dimension(idim,jdim), intent(out) :: array
    ! Local:
    integer                                 :: ierr
    integer                                 :: varid

    ierr = nf90_inq_varid(ncid,  name,  varid)
    ! If the variable is "XLAT", and "XLAT" is not found, look for "XLAT_M"
    ! If the variable is "XLAT_M", and "XLAT_M" is not found, look for "XLAT"
    ! If the variable is "XLONG", and "XLONG" is not found, look for "XLONG_M"
    ! If the variable is "XLONG_M", and "XLONG_M" is not found, look for "XLONG"
    if (name == "XLAT") then
       if (ierr /= 0) then
          ierr = nf90_inq_varid(ncid,  "XLAT_M",  varid)
       endif
    else if (name == "XLAT_M") then
       if (ierr /= 0) then
          ierr = nf90_inq_varid(ncid,  "XLAT",  varid)
       endif
    else  if (name == "XLONG") then
       if (ierr /= 0) then
          ierr = nf90_inq_varid(ncid,  "XLONG_M",  varid)
       endif
    else if (name == "XLONG_M") then
       if (ierr /= 0) then
          ierr = nf90_inq_varid(ncid,  "XLONG",  varid)
       endif
    endif
    call error_handler(ierr, "STOP:  READ_WRFINPUT: Problem finding variable '"//trim(name)//"' in wrfinput file.")


    ierr = nf90_get_var(ncid, varid, array)
    call error_handler(ierr, "STOP:  READ_WRFINPUT:  Problem retrieving variable '"//trim(name)//"' from wrfinput file.")

  end subroutine get_2d


  subroutine error_handler(status, failure, success)
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


end module module_wrfinputfile
