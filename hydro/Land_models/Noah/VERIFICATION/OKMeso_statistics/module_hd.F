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

module module_hd
  use netcdf
  type nf_structure
     sequence
     integer :: ncid
     integer :: nvar
     integer :: ndim
     character(len=NF90_MAX_NAME), dimension(NF90_MAX_DIMS) :: dimname
     integer, dimension(NF90_MAX_DIMS) :: dimlen
     integer :: tdim
     integer :: idim
     integer :: jdim
     integer :: kdim
     integer :: map_proj
     real    :: dskm
     real    :: xlonc
     real    :: truelat1
     real    :: truelat2
     real    :: reflat
     real    :: reflon
     real    :: refx
     real    :: refy
  end type nf_structure

  interface netcdf_get_field
     module procedure netcdf_get_field_2d, netcdf_get_field_3d
  end interface

contains

  subroutine netcdf_get_time(nfstruct, name, time_index, time_return)
    implicit none
    type(nf_structure), intent(in) :: nfstruct
    character(len=*),   intent(in) :: name
    integer,            intent(in) :: time_index
    integer :: ncid, varid, dimid
    integer :: iret, DateStrLen
    integer, dimension(2) :: start, nfcount
    character(len=24) :: time_return

    ncid = nfstruct%ncid

    ! We need the varid of this field
    iret = nf90_inq_varid(ncid, name, varid)
    if (iret /= 0) then
       write(*,'("NF90_INQ_VARID:  ",A)') nf90_strerror(iret)
       stop
    endif

    iret = nf90_inq_dimid(ncid, "DateStrLen", dimid)
    if (iret /= 0) then
       write(*,'("NF90_INQ_DIMID (for DateStrLen):  ",A)') nf90_strerror(iret)
       stop
    endif

    iret = nf90_inquire_dimension(ncid, dimid, len=DateStrLen)
    if (iret /= 0) then
       write(*,'("NF90_INQUIRE_DIMENSION:  ",A)') nf90_strerror(iret)
       stop
    endif

    start(1) = 1
    start(2) = time_index
    nfcount(1) = DateStrLen
    nfcount(2) = 1
    
    iret = nf90_get_var(ncid, varid, time_return, start, nfcount)
    if (iret /= 0) then
       write(*,'("netcdf_get_time:  NF90_GET_VAR:  ",A)') nf90_strerror(iret)
       stop
    endif


  end subroutine netcdf_get_time

  subroutine netcdf_get_field_2d(nfstruct, name, time_index, ptr2d)
    implicit none
    type(nf_structure), intent(in) :: nfstruct
    integer, intent(in) :: time_index
    real, pointer, dimension(:,:) :: ptr2d
    character(len=*), intent(in) :: name
    integer :: iret
    integer, dimension(20) :: start, nfcount
    integer :: varid
    integer, dimension(20) :: dimids
    integer :: ndims
    integer :: ncid

    ncid = nfstruct%ncid

    ! We need the varid of this field
    iret = nf90_inq_varid(ncid, name, varid)
    if (iret /= 0) then
       write(*,'("NF90_INQ_VARID:  ",A)') nf90_strerror(iret)
       stop
    endif

    ! We need the dimensions of this variable
    iret = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
    if (iret /= 0) then
       write(*,'("NF90_INQUIRE_VARIABLE:  ",A)') nf90_strerror(iret)
       stop
    endif


    if (ndims == 3) then
       allocate(ptr2d(nfstruct%idim,nfstruct%jdim))
       start(1) = 1
       start(2) = 1
       start(3) = time_index
       nfcount(1) = nfstruct%idim
       nfcount(2) = nfstruct%jdim
       nfcount(3) = 1
    else
       print*, 'ndim = ', nfstruct%ndim
       print*, 'name = ', name
       stop "NETCDF_GET_FIELD: No can do with these dimensions."
    endif

    iret = nf90_get_var(ncid, varid, ptr2d, start, nfcount)
    if (iret /= 0) then
       write(*,'("NF90_GET_VAR:  ",A)') nf90_strerror(iret)
       stop
    endif

  end subroutine netcdf_get_field_2d

  subroutine netcdf_get_field_3d(nfstruct, name, time_index, ptr3d)
    implicit none
    type(nf_structure), intent(in) :: nfstruct
    integer, intent(in) :: time_index
    real, pointer, dimension(:,:,:) :: ptr3d
    character(len=*), intent(in) :: name
    integer :: iret
    integer, dimension(20) :: start, nfcount
    integer :: varid
    integer, dimension(20) :: dimids
    integer :: ndims
    integer :: ncid

    ncid = nfstruct%ncid

    ! We need the varid of this field
    iret = nf90_inq_varid(ncid, name, varid)
    if (iret /= 0) then
       write(*,'("NF90_INQ_VARID:  ",A)') nf90_strerror(iret)
       stop
    endif

    ! We need the dimensions of this variable
    iret = nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids)
    if (iret /= 0) then
       write(*,'("NF90_INQUIRE_VARIABLE:  ",A)') nf90_strerror(iret)
       stop
    endif


    if (ndims == 4) then
       allocate(ptr3d(nfstruct%idim,nfstruct%jdim,nfstruct%kdim))
       start(1) = 1
       start(2) = 1
       start(3) = 1
       start(4) = time_index
       nfcount(1) = nfstruct%idim
       nfcount(2) = nfstruct%jdim
       nfcount(3) = nfstruct%kdim
       nfcount(4) = 1
    else
       print*, 'ndim = ', nfstruct%ndim
       print*, 'name = ', name
       stop "NETCDF_GET_FIELD: No can do with these dimensions."
    endif

    iret = nf90_get_var(ncid, varid, ptr3d, start, nfcount)
    if (iret /= 0) then
       write(*,'("NF90_GET_VAR:  ",A)') nf90_strerror(iret)
       stop
    endif

  end subroutine netcdf_get_field_3d

  subroutine netcdf_open(flnm, nfstruct)
    ! Opens a netcdf file and fills the fields in nfstruct for:
    !      ncid
    !      nvar
    !      ndim
    !      dimname
    !      dimlen
    !      tdim
    !      idim
    !      jdim
    !      kdim
    !   etc.
    ! use netcdf
    ! use module_hd
    implicit none

    character(len=*), intent(in)    :: flnm
    type(nf_structure), intent(out) :: nfstruct

    integer :: iret
    integer :: ncid
    integer :: nvar
    integer :: ndim
    integer :: dimid
    character(len=NF90_MAX_NAME), dimension(NF90_MAX_DIMS) :: dimname
    integer, dimension(NF90_MAX_DIMS) :: dimlen
    integer :: tdim, idim, jdim, kdim, map_proj
    real    :: dskm, truelat1, truelat2, xlonc
    real    :: reflat, reflon
    real    :: dxm

    tdim = 0
    idim = 0
    jdim = 0
    kdim = 1

    iret = nf90_open(trim(flnm), NF90_NOWRITE, ncid)
    if (iret /= 0) then
       write(*,'("NF90_OPEN:  ",A)') nf90_strerror(iret)
       write(*,'(A)') "FILE = '"//trim(flnm)//"'"
       stop
    endif

    iret = nf90_inquire(ncid, nVariables=nvar,  nDimensions=ndim)
    if (iret /= 0) then
       write(*,'("NF90_INQUIRE nVariables:  ",A)') nf90_strerror(iret)
       stop
    endif
!KWM    print*, 'nvar = ', nvar, '  ndim = ', ndim

    iret = nf90_get_att(ncid, NF90_GLOBAL, "MAP_PROJ", map_proj)
    if (iret /= 0) then
       write(*,'("NF90_GET_ATT problem for MAP_PROJ:  ",A)') nf90_strerror(iret)
       stop
    endif

    iret = nf90_get_att(ncid, NF90_GLOBAL, "DX", dxm)
    if (iret /= 0) then
       write(*,'("NF90_GET_ATT problem for DX:  ",A)') nf90_strerror(iret)
       stop
    endif
    dskm = dxm * 1.E-3

    iret = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT1", truelat1)
    if (iret /= 0) then
       write(*,'("NF90_GET_ATT problem for TRUELAT1:  ",A)') nf90_strerror(iret)
       stop
    endif

    iret = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT2", truelat2)
    if (iret /= 0) then
       write(*,'("NF90_GET_ATT problem for TRUELAT2:  ",A)') nf90_strerror(iret)
       stop
    endif

    iret = nf90_get_att(ncid, NF90_GLOBAL, "STAND_LON", xlonc)
    if (iret /= 0) then
       write(*,'("NF90_GET_ATT problem for STAND_LON:  ",A)') nf90_strerror(iret)
       stop
    endif

    iret = nf90_get_att(ncid, NF90_GLOBAL, "LAT1", reflat)
    if (iret /= 0) then
       write(*,'("NF90_GET_ATT problem for LAT1:  ",A)') nf90_strerror(iret)
       stop
    endif

    iret = nf90_get_att(ncid, NF90_GLOBAL, "LON1", reflon)
    if (iret /= 0) then
       write(*,'("NF90_GET_ATT problem for LON1:  ",A)') nf90_strerror(iret)
       stop
    endif

    do dimid = 1, ndim
       iret = nf90_inquire_dimension(ncid, dimid, dimname(dimid), dimlen(dimid))
       if (iret /= 0) then
          write(*,'("NF90_INQUIRE_DIMENSION:  ",A)') nf90_strerror(iret)
          stop
       endif
!KWM       write(*,'("Dimension = ", A20, 2x, i5)') dimname(dimid), dimlen(dimid)
       if (dimname(dimid) == "Times") tdim = dimlen(dimid)
       if (dimname(dimid) == "west_east") idim = dimlen(dimid)
       if (dimname(dimid) == "south_north") jdim = dimlen(dimid)
       if (dimname(dimid) == "soil_layers_stag") kdim = dimlen(dimid)
    enddo

    nfstruct%ncid = ncid
    nfstruct%nvar = nvar
    nfstruct%ndim = ndim
    nfstruct%dimname = dimname
    nfstruct%dimlen = dimlen
    nfstruct%tdim = tdim
    nfstruct%idim = idim
    nfstruct%jdim = jdim
    nfstruct%kdim = kdim
    nfstruct%map_proj = map_proj
    nfstruct%truelat1 = truelat1
    nfstruct%truelat2 = truelat2
    nfstruct%dskm    = dskm
    nfstruct%xlonc    = xlonc
    nfstruct%reflat   = reflat
    nfstruct%reflon   = reflon
    nfstruct%refx     = 1.0
    nfstruct%refy     = 1.0
  end subroutine netcdf_open

  subroutine lltoxy_hd(lat, lon, x, y, nfstruct)
    real, intent(in) :: lat, lon
    real, intent(out) :: x, y
    type (nf_structure), intent(in) :: nfstruct

    select case (nfstruct%map_proj)
    case (1)
       project = "LC"
    case (2)
       project = "ST"
    case (3)
       project = "ME"
    case default
       print*, 'MODULE_HD:  LLTOXY_HD:  Put the map projection in:  proj = ', nfstruct%map_proj
       stop
    end select

    call lltoxy_generic(lat, lon, x, y, &
         project,nfstruct%dskm,nfstruct%reflat,nfstruct%reflon,nfstruct%refx,nfstruct%refy, &
         nfstruct%xlonc,nfstruct%truelat1,nfstruct%truelat2)

  end subroutine lltoxy_hd

#if defined( _NCARGKS_ )

  subroutine wrfmap(nfstruct)
    ! use kwm_plot_utilities
    implicit none
    type(nf_structure), intent(in) :: nfstruct

    integer :: jprj
    real :: plat, plon, rota
    real :: plm1, plm2, plm3, plm4
    integer :: jlts = 2;
    integer :: jgrd = 0;
    integer :: iout = 4;
    integer :: idot = 0;
    integer :: ierr
    character(len=2) :: project

    if (nfstruct%map_proj == 1) then
       jprj = 3
       plat = nfstruct%truelat1
       plon = nfstruct%xlonc
       rota = nfstruct%truelat2
       project = "LC"
       call xytoll_generic(1.0, 1.0, plm1, plm2, &
            project,nfstruct%dskm,nfstruct%reflat,nfstruct%reflon,nfstruct%refx,nfstruct%refy,&
            nfstruct%xlonc,nfstruct%truelat1,nfstruct%truelat2)

       call xytoll_generic(float(nfstruct%idim), float(nfstruct%jdim), plm3, plm4, &
            project,nfstruct%dskm,nfstruct%reflat,nfstruct%reflon,nfstruct%refx,nfstruct%refy,&
            nfstruct%xlonc,nfstruct%truelat1,nfstruct%truelat2)
    else
       print*, 'Put the map projection in:  proj = ', nfstruct%map_proj
       stop
    endif

    print*, 'call supmap:  ', jprj, plat, plon, rota, plm1, plm2, plm3, plm4, jlts, jgrd, iout, idot, ierr
    call supmap(jprj, plat, plon, rota, plm1, plm2, plm3, plm4, jlts, jgrd, iout, idot, ierr)

  end subroutine wrfmap

#endif

end module module_hd
