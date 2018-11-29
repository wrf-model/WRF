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
!-------------------------------------------------------------
  use module_wrfinputfile
  implicit none
  ! Define a data structure for WRF geo_em file

  type geo_em_type
     integer :: idim
     integer :: jdim
     integer :: map_proj
     real    :: Dx
     real    :: Dy
     real    :: LoV
     real    :: Latin1
     real    :: Latin2
     real    :: La1
     real    :: La2
     real    :: Lo1
     real    :: Lo2
     character(len=4) :: landuse_dataset ! "MODI" or "USGS"
     real, pointer, dimension(:,:,:) :: veg
  end type geo_em_type

contains

  subroutine read_geo_em_file(flnm, geo_em, ierr)

!---------------------------------------------------------------
!  At the moment, all we need from the geo_em file is the 
!  array of monthly green vegetation fractions.
!
!  We also read projection/grid information, to ensure that
!  the geo_em and wrfinput files are consistent.
!---------------------------------------------------------------

    implicit none

    character(len=*),   intent(in)  :: flnm
    type (geo_em_type), intent(out) :: geo_em
    integer,            intent(out) :: ierr
    ! Local:
    integer :: ncid
    integer :: iret
    integer :: dimid
    real, allocatable, dimension(:,:) :: dum2d


! Open the NetCDF file.
    print*, 'flnm = ', flnm
    iret = nf90_open(flnm, NF90_NOWRITE, ncid)
    call error_handler(iret, "Problem reading geo_em file: "//flnm)

!---------------------------------------------------------------
!
! Use the grid and projection information to compare to the
! wrfinput file that we use, to be sure that we have the
! same grid in geo_em and wrfinput files.
!
! We read the data here, but we make the check elsewhere.
!
!---------------------------------------------------------------

! Find out about dimensions in the file.

    iret = nf90_inq_dimid(ncid, "west_east", dimid)
    call error_handler(iret, "STOP:  Problem finding NetCDF dimension 'west_east'")
    iret = nf90_inquire_dimension(ncid, dimid, len=geo_em%idim)
    call error_handler(iret, "STOP:  Problem finding NetCDF dimension 'west_east'")

    iret = nf90_inq_dimid(ncid, "south_north", dimid)
    call error_handler(iret, "STOP:  Problem finding NetCDF dimension 'south_north'")
    iret = nf90_inquire_dimension(ncid, dimid, len=geo_em%jdim)
    call error_handler(iret, "STOP:  Problem finding NetCDF dimension 'south_north'")

    ! Projection information:

    iret = nf90_get_att(ncid, NF90_GLOBAL,"MAP_PROJ", geo_em%map_proj)
    call error_handler(iret, "STOP:  problem finding NetCDF Global attribute MAP_PROJ")

    iret = nf90_get_att(ncid, NF90_GLOBAL, "STAND_LON", geo_em%LoV)
    call error_handler(iret, "STOP:  problem finding NetCDF global attribute STAND_LON")

    iret = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT1" , geo_em%Latin1)
    call error_handler(iret, "STOP:  problem finding NetCDF global attribute TRUELAT1")

    iret = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT2" , geo_em%Latin2)
    call error_handler(iret, "STOP:  problem finding NetCDF global attribute TRUELAT2")

    iret = nf90_get_att(ncid, NF90_GLOBAL, "DX" , geo_em%Dx)
    call error_handler(iret, "STOP:  problem finding NetCDF global attribute DX")

    iret = nf90_get_att(ncid, NF90_GLOBAL, "DY" , geo_em%Dy)
    call error_handler(iret, "STOP:  problem finding NetCDF global attribute DY")

    iret = nf90_get_att(ncid, NF90_GLOBAL, "MMINLU" , geo_em%landuse_dataset)
    call error_handler(iret, "STOP:  problem finding NetCDF global attribute MMINLU")

    allocate(dum2d(geo_em%idim, geo_em%jdim))
    ! Get Latitudes -- upper-left and lower-right corners

    call get_2d("XLAT_M", ncid, dum2d, geo_em%idim, geo_em%jdim)
    geo_em%La1 = dum2d(1,1)
    geo_em%La2 = dum2d(geo_em%idim,geo_em%jdim)

    ! Get Longitude -- upper-left and lower-right corners
    call get_2d("XLONG_M", ncid, dum2d, geo_em%idim, geo_em%jdim)
    geo_em%Lo1 = dum2d(1,1)
    geo_em%Lo2 = dum2d(geo_em%idim,geo_em%jdim)

    deallocate(dum2d)

    !
    ! Get monthly Greenness Fractions
    !
    allocate(geo_em%veg(geo_em%idim, geo_em%jdim, 12))
    call get_greenness(ncid, geo_em%veg, geo_em%idim, geo_em%jdim, 12)

    ! Close the file and get out of here

    iret = nf90_close(ncid)
    call error_handler(iret, "STOP:  Problem closing file??")

    ierr = 0
    print*, "Done with subroutine read_geo_em_file"

  end subroutine read_geo_em_file

  subroutine get_greenness(ncid, array, idim, jdim, kdim)
    implicit none
    integer, intent(in)                          :: ncid
    integer, intent(in)                          :: idim
    integer, intent(in)                          :: jdim
    integer, intent(in)                          :: kdim
    real, dimension(idim,jdim,kdim), intent(out) :: array
    ! Local:
    integer :: ierr
    integer :: varid

    ierr = nf90_inq_varid(ncid,  "GREENFRAC",  varid)
    call error_handler(ierr, "STOP:  Problem finding GREENFRAC in geo_em file")

    ierr = nf90_get_var(ncid, varid, array)
    call error_handler(ierr, "STOP:  Problem getting GREENFRAC from geo_em file")

  end subroutine get_greenness

end module module_geo_em
