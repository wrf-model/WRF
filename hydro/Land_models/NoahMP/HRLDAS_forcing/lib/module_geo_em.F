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

module module_geo_em
!-------------------------------------------------------------
! At the moment, all that is used from the geo_em file
! is the array of monthly green vegetation fractions.
!
! We also grab projection/grid information, to ensure that
! the geo_em and wrfinput files are consistent.
!
! Modified by Barlage (v3.7) to remove dependence on wrfinput file
!  Therefore, all grid information is read in this module now.
!
!-------------------------------------------------------------
  use netcdf
  use module_llxy
  implicit none
  ! Define a data structure for WRF geo_em file

  type geo_em_type
     type(proj_info) :: proj
     integer :: grid_id
     integer :: idim
     integer :: jdim
     character(len=256) :: landuse_dataset ! "MODIFIED_IGBP_MODIS_NOAH" or "USGS"
     integer :: iswater
     integer :: isurban
     integer :: isice
     real, pointer, dimension(:,:)   :: lat    ! Grid point latitudes
     real, pointer, dimension(:,:)   :: lon    ! Grid point longitudes
     real, pointer, dimension(:,:)   :: ter    ! Terrain field
     real, pointer, dimension(:,:)   :: use    ! Land-use (vegetation) field
     real, pointer, dimension(:,:)   :: tmn    ! Deep soil temperature
     real, pointer, dimension(:,:)   :: soi    ! Soil type
     real, pointer, dimension(:,:)   :: msk    ! Land mask
     real, pointer, dimension(:,:)   :: mmx    ! mapfac_mx
     real, pointer, dimension(:,:)   :: mmy    ! mapfac_my
     real, pointer, dimension(:,:)   :: ice    ! Seaice placeholder
     real, pointer, dimension(:,:,:) :: veg
     real, pointer, dimension(:,:,:) :: lai
  end type geo_em_type

contains

  subroutine read_geo_em_file(flnm, geo_em, ierr)

!---------------------------------------------------------------
!  At the moment, all we need from the geo_em file is the 
!  array of monthly green vegetation fractions.
!
!  We also read projection/grid information, to ensure that
!  the geo_em and wrfinput files are consistent.
!
!  As of v3.7, geo_em is the only WPS/WRF file we read, so it grabs everything
!---------------------------------------------------------------

    implicit none

    character(len=*),   intent(in)  :: flnm
    type (geo_em_type), intent(out) :: geo_em
    integer,            intent(out) :: ierr
    ! Local:
    integer :: ncid
    integer :: iret
    integer :: dimid
    integer :: i,j,l
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
    real, pointer, dimension(:,:,:)   :: soil_top_cat
    integer                           :: dominant_index
    real                              :: dominant_value
    integer                           :: iswater_soil


! Open the NetCDF file.
    print*, 'flnm = ', flnm
    iret = nf90_open(flnm, NF90_NOWRITE, ncid)
    call error_handler(iret, "Problem reading geo_em file: "//flnm)

! Find out about dimensions in the file.

    iret = nf90_inq_dimid(ncid, "west_east", dimid)
    call error_handler(iret, "STOP:  Problem finding NetCDF dimension 'west_east'")
    iret = nf90_inquire_dimension(ncid, dimid, len=geo_em%idim)
    call error_handler(iret, "STOP:  Problem finding NetCDF dimension 'west_east'")

    iret = nf90_inq_dimid(ncid, "south_north", dimid)
    call error_handler(iret, "STOP:  Problem finding NetCDF dimension 'south_north'")
    iret = nf90_inquire_dimension(ncid, dimid, len=geo_em%jdim)
    call error_handler(iret, "STOP:  Problem finding NetCDF dimension 'south_north'")

! Grid id.  Check for the string "GRID_ID" or the string "grid_id"

    iret = nf90_get_att(ncid, NF90_GLOBAL,"grid_id", geo_em%grid_id)
    if (iret /= 0) then
       iret = nf90_get_att(ncid, NF90_GLOBAL,"GRID_ID", geo_em%grid_id)
       call error_handler(iret, "STOP:  nf90_get_att:  'grid_id' or 'GRID_ID' not found.")
    endif

    iret = nf90_get_att(ncid, NF90_GLOBAL, "ISWATER" , geo_em%iswater)
    call error_handler(iret, "STOP:  nf90_get_att:  ISWATER")

    iret = nf90_get_att(ncid, NF90_GLOBAL, "ISURBAN" , geo_em%isurban)
    call error_handler(iret, "STOP:  nf90_get_att:  ISURBAN")

    iret = nf90_get_att(ncid, NF90_GLOBAL, "ISICE" , geo_em%isice)
    call error_handler(iret, "STOP:  nf90_get_att:  ISICE")

    iret = nf90_get_att(ncid, NF90_GLOBAL, "MMINLU" , geo_em%landuse_dataset)
    call error_handler(iret, "STOP:  nf90_get_att:  MMINLU")

! Get Latitude
    allocate(geo_em%lat(geo_em%idim, geo_em%jdim))
    call get_2d("XLAT_M", ncid, geo_em%lat, geo_em%idim, geo_em%jdim)
    la1 = geo_em%lat(1,1)

! Get Longitude
    allocate(geo_em%lon(geo_em%idim, geo_em%jdim))
    call get_2d("XLONG_M", ncid, geo_em%lon, geo_em%idim, geo_em%jdim)
    lo1 = geo_em%lon(1,1)

! Get Terrain
    allocate(geo_em%ter(geo_em%idim, geo_em%jdim))
    call get_2d("HGT_M", ncid, geo_em%ter, geo_em%idim, geo_em%jdim)

! Get Deep Soil Temperature (adjust to elevation)
    allocate(geo_em%tmn(geo_em%idim, geo_em%jdim))
    call get_2d("SOILTEMP", ncid, geo_em%tmn, geo_em%idim, geo_em%jdim)
    geo_em%tmn = geo_em%tmn - 0.0065 * geo_em%ter

! Get Land Use categories 
    allocate(geo_em%use(geo_em%idim, geo_em%jdim))
    call get_2d("LU_INDEX", ncid, geo_em%use, geo_em%idim, geo_em%jdim)

! Create XLAND 
    allocate(geo_em%msk(geo_em%idim, geo_em%jdim))
    where(geo_em%use .eq. geo_em%iswater) geo_em%msk = 2
    where(geo_em%use .ne. geo_em%iswater) geo_em%msk = 1

! Create SEAICE 
    allocate(geo_em%ice(geo_em%idim, geo_em%jdim))
    geo_em%ice = 0.0

! Get Soil categories 
!   soil is a problem since real does some manipulation, so we need to read the % data

    iret = nf90_get_att(ncid, NF90_GLOBAL, "ISOILWATER" , iswater_soil)
    call error_handler(iret, "STOP:  nf90_get_att:  ISOILWATER")

    allocate(soil_top_cat(geo_em%idim, geo_em%jdim, 16))  ! potential problem with hardcoding 16?
    call get_soil_cat(ncid, soil_top_cat, geo_em%idim, geo_em%jdim, 16)
    allocate(geo_em%soi(geo_em%idim, geo_em%jdim))
    
      DO i = 1 , geo_em%idim
         DO j = 1 , geo_em%jdim
            dominant_value = soil_top_cat(i,j,1)
            dominant_index = 1
            IF ( geo_em%msk(i,j) .LT. 1.5 ) THEN
               DO l = 2 , 16
                  IF ( ( l .NE. iswater_soil ) .AND. ( soil_top_cat(i,j,l) .GT. dominant_value ) ) THEN
                     dominant_value = soil_top_cat(i,j,l)
                     dominant_index = l
                  END IF
               END DO
               IF ( dominant_value .LT. 0.01 ) dominant_index = 8
            ELSE
               dominant_index = iswater_soil
            END IF
            geo_em%soi(i,j) = dominant_index
         END DO
      END DO
    
    where(geo_em%use .eq. geo_em%iswater) geo_em%soi = 14
    where(geo_em%use .ne. geo_em%iswater .and. geo_em%soi .eq. 14) geo_em%soi = 8

! Get MAPFAC_MX (for MMF groundwater)
    allocate(geo_em%mmx(geo_em%idim, geo_em%jdim))
    call get_2d("MAPFAC_MX", ncid, geo_em%mmx, geo_em%idim, geo_em%jdim)

! Get MAPFAC_MY (for MMF groundwater)
    allocate(geo_em%mmy(geo_em%idim, geo_em%jdim))
    call get_2d("MAPFAC_MY", ncid, geo_em%mmy, geo_em%idim, geo_em%jdim)

! Get monthly Greenness Fractions
    allocate(geo_em%veg(geo_em%idim, geo_em%jdim, 12))
    call get_monthly(ncid, "GREENFRAC", geo_em%veg, geo_em%idim, geo_em%jdim, 12)
    geo_em%veg = 100.0 * geo_em%veg  ! change to range consistent with wrfinput

! Get monthly leaf area index
    allocate(geo_em%lai(geo_em%idim, geo_em%jdim, 12))
    call get_monthly(ncid, "LAI12M", geo_em%lai, geo_em%idim, geo_em%jdim, 12)

! Projection information:

    call map_init(geo_em%proj)

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

       call map_set(PROJ_LC, geo_em%proj, lat1=la1, lon1=lo1, knowni=1.0, knownj=1.0, &
            truelat1=truelat1, truelat2=truelat2, stdlon=stdlon, dx=dx)

    else if (map_proj == PROJ_PS) then

       iret = nf90_get_att(ncid, NF90_GLOBAL, "STAND_LON", stdlon)
       call error_handler(iret, "STOP:  nf90_get_att:  STAND_LON")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT1" , truelat1)
       call error_handler(iret, "STOP:  nf90_get_att:  TRUELAT1")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "DX" , dx)
       call error_handler(iret, "STOP:  nf90_get_att:  DX")

       call map_set(PROJ_PS, geo_em%proj, lat1=la1, lon1=lo1, truelat1=truelat1, &
            knowni=1.0, knownj=1.0, stdlon=stdlon, dx=dx)

    else if (map_proj == PROJ_MERC) then

       iret = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT1" , truelat1)
       call error_handler(iret, "STOP:  nf90_get_att:  TRUELAT1")

       iret = nf90_get_att(ncid, NF90_GLOBAL, "DX" , dx)
       call error_handler(iret, "STOP:  nf90_get_att:  DX")

       call map_set(PROJ_MERC, geo_em%proj, lat1=la1, lon1=lo1, knowni=1.0, knownj=1.0, &
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

       call map_set(PROJ_CASSINI, geo_em%proj, lat1=la1, lon1=lo1, latinc=latinc, loninc=loninc, &
            knowni=1.0, knownj=1.0, lat0=pole_lat, lon0=pole_lon, stdlon=stdlon)

    else if (map_proj == PROJ_LATLON) then
       stop "FATAL ERROR: In module_geo_em.F -- PROJ_LATLON is not a valid map projection for WRF."

       ! iret = nf90_get_att(ncid, NF90_GLOBAL, "DX" , dx)
       ! call error_handler(iret, "STOP:  nf90_get_att:  DX")

       ! For lat/lon projection, module_llxy assumes a spherical earth
       ! with circumference EARTH_CIRC_M
       ! latinc = 360.0*dx/EARTH_CIRC_M
       ! loninc = 360.0*dx/EARTH_CIRC_M

       ! call map_set(PROJ_LATLON, geo_em%proj, lat1=la1, lon1=lo1, &
       !      latinc=latinc, loninc=loninc, knowni=1.0, knownj=1.0)

    else if (map_proj == PROJ_PS_WGS84) then
       stop "FATAL ERROR: In module_geo_em.F -- PROJ_PS_WGS84 is not a valid map projection for WRF."
       ! call map_set(PROJ_PS_WGS84, geo_em%proj, lat1=geo_em%la1, lon1=geo_em%lo1, knowni=1.0, knownj=1.0, &
       !     stdlon=geo_em%lov, dx=geo_em%dx)
    else if (map_proj == PROJ_GAUSS) then
       stop "FATAL ERROR: In module_geo_em.F -- PROJ_GAUSS is not a valid map projection for WRF."
       ! call map_set(PROJ_GAUSS, geo_em%proj, lat1=, lon1=, nlat=, loninc=)
    else if (map_proj == PROJ_CYL) then
       stop "FATAL ERROR: In module_geo_em.F -- PROJ_CYL is not a valid map projection for WRF."
       ! call map_set(PROJ_CYL, geo_em%proj, latinc=, loninc=, stdlon=)
    else if (map_proj == PROJ_ALBERS_NAD83) then
       stop "FATAL ERROR: In module_geo_em.F -- PROJ_ALBERS_NAD83 is not a valid map projection for WRF."
       !call map_set(PROJ_ALBERS_NAD83, geo_em%proj, lat1=, lon1=, knowni=, knownj=, truelat1=, truelat2=, stdlon=, dx=)
    else if (map_proj == PROJ_ROTLL) then
       stop "FATAL ERROR: In module_geo_em.F -- PROJ_ROTLL is not a valid map projection for WRF."
       ! call map_set(PROJ_ROTLL, geo_em%proj, lat1=geo_em%lat1, lon1=geo_em%lon1, ixdim=, jydim=, phi=, lambda=)
    else
       write(*,'("WARNING: Unknown geo_em map projection:  map_proj = ", I10)') map_proj
       stop "FATAL ERROR: In module_geo_em.F -- Unknown geo_em map projection"
    endif

! Close the file and get out of here

    iret = nf90_close(ncid)
    call error_handler(iret, "STOP:  Problem closing file??")

    ierr = 0
    print*, "Done with subroutine read_geo_em_file"

  end subroutine read_geo_em_file

  subroutine get_monthly(ncid, name, array, idim, jdim, kdim)
    implicit none
    integer, intent(in)                          :: ncid
    character(len=*), intent(in)                 :: name
    integer, intent(in)                          :: idim
    integer, intent(in)                          :: jdim
    integer, intent(in)                          :: kdim
    real, dimension(idim,jdim,kdim), intent(out) :: array
    ! Local:
    integer :: ierr
    integer :: varid

    ierr = nf90_inq_varid(ncid,  trim(name),  varid)
    if(ierr /= 0 .and. trim(name) == "GREENFRAC") call error_handler(ierr, "STOP:  Problem finding GREENFRAC in geo_em file")

    if(ierr /= 0 .and. trim(name) == "LAI12M") then    ! LAI not available in this version
      array = -1.e36
      return
    end if
    
    ierr = nf90_get_var(ncid, varid, array)
    call error_handler(ierr, "STOP:  Problem getting "//trim(name)//" from geo_em file")

  end subroutine get_monthly

  subroutine get_soil_cat(ncid, array, idim, jdim, kdim)
    implicit none
    integer, intent(in)                          :: ncid
    integer, intent(in)                          :: idim
    integer, intent(in)                          :: jdim
    integer, intent(in)                          :: kdim
    real, dimension(idim,jdim,kdim), intent(out) :: array
    ! Local:
    integer :: ierr
    integer :: varid

    ierr = nf90_inq_varid(ncid,  "SOILCTOP",  varid)
    call error_handler(ierr, "STOP:  Problem finding SOILCTOP in geo_em file")

    ierr = nf90_get_var(ncid, varid, array)
    call error_handler(ierr, "STOP:  Problem getting SOILCTOP from geo_em file")

  end subroutine get_soil_cat

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
    call error_handler(ierr, "STOP:  READ_GEO_EM: Problem finding variable '"//trim(name)//"' in geo_em file.")


    ierr = nf90_get_var(ncid, varid, array)
    call error_handler(ierr, "STOP:  READ_GEO_EM:  Problem retrieving variable '"//trim(name)//"' from geo_em file.")

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
          write(*,'(/,"WARNING:  ***** ", A)') failure
       endif
       write(*,'("WARNING:  ***** ",A,/)') nf90_strerror(status)
       stop 'FATAL ERROR: In module_geo_em.F -- Stopped'
    endif

    if (present(success)) then
       write(*,'(A)') success
    endif

  end subroutine error_handler

end module module_geo_em
