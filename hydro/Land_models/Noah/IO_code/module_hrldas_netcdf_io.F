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

module module_hrldas_netcdf_io
  use module_date_utilities
  use netcdf
#ifdef _PARALLEL_
  use mpi
#endif
  implicit none

  logical, parameter :: FATAL = .TRUE.
  logical, parameter :: NOT_FATAL = .FALSE.

  type inputstruct
     character(len=19)             :: read_date
     real, pointer, dimension(:,:) :: t
     real, pointer, dimension(:,:) :: q
     real, pointer, dimension(:,:) :: u
     real, pointer, dimension(:,:) :: v
     real, pointer, dimension(:,:) :: p
     real, pointer, dimension(:,:) :: lw
     real, pointer, dimension(:,:) :: sw
     real, pointer, dimension(:,:) :: pcp
     real, pointer, dimension(:,:) :: fpar
     real, pointer, dimension(:,:) :: lai
  end type inputstruct

  character(len=256), private :: restart_filename_remember
  integer, private :: iswater_remember
  integer, private :: xstartpar_remember
  integer, private, allocatable, dimension(:,:) :: vegtyp_remember
  integer, private :: ncid_remember
  integer, private :: output_count_remember = 0
  logical, private :: define_mode_remember
  integer, private :: dimid_ix_remember
  integer, private :: dimid_jx_remember
  integer, private :: dimid_times_remember
  integer, private :: dimid_layers_remember

  interface add_to_restart
     module procedure add_to_restart_2d_float, add_to_restart_2d_integer, add_to_restart_3d
  end interface

  interface get_from_restart
     module procedure get_from_restart_2d_float, get_from_restart_2d_integer, get_from_restart_3d
  end interface

  interface add_to_output
     module procedure add_to_output_2d_float, add_to_output_2d_integer, add_to_output_3d
  end interface

contains

!-------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------

  subroutine check_outdir(rank, outdir)
    implicit none

    ! Check that output directory OUTDIR exists and is writable, by
    ! trying to open a test file in that directory.  Include a random
    ! number in the test file name, to greatly reduce the chance of 
    ! collision with existing file names.  This assumes that the 
    ! intrinsic random_seed routine, called without argument, will seed 
    ! the random number generator based on something like system time 
    ! or hardware noise.

    integer,          intent(in) :: rank
    character(len=*), intent(in) :: outdir

    real                         :: xrand
    character(len=256)           :: testfile
    integer                      :: ierr

    if (rank == 0) then
       call random_seed()
       call random_number(xrand)
       write(testfile, '(A,"/scratch.",I4.4,".",I6.6,".scratch")') trim(outdir), rank, int(xrand*1.E6)
       open(unit=30, file=trim(testfile), status='unknown', iostat=ierr)
       if (ierr /= 0) then
          write(*, '(/)')
          write(*, '(" ***** Namelist error: ******************************************************")')
          write(*, '(" ***** ")')
          write(*, '(" ***** We cannot write a file to the directory specified in namelist option OUTDIR.")')
          write(*, '(" ***** Check namelist option OUTDIR (currently set to ''", A, "'')")') trim(outdir)
          write(*, '(" *****       Check that the directory exists, is a directory, and is writable.")')
          write(*, '(/)')
          stop "OUTDIR Problem"
       endif
       close(unit=30, iostat=ierr, status='delete')
       if (ierr /= 0) then
          print*, "TESTFILE = " // trim(testfile) // '"'
          stop "Much confusion.  Problem closing test file."
       endif
    endif
  end subroutine check_outdir

!-------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------

  subroutine find_restart_file(rank, restart_filename_requested, startdate, khour, olddate, restart_flnm)
    implicit none
    !
    ! If the user has requested the latest restart file, find the latest restart file.
    ! If the user has requested a specific restart file, check that the file exists.
    !
    ! Return the restart file name in string RESTART_FLNM.
    ! Update the OLDDATE string (in case the latest restart file was requested).
    !
    integer,            intent(in)    :: rank
    character(len=*),   intent(in)    :: restart_filename_requested
    character(len=19),  intent(in)    :: startdate
    integer,            intent(in)    :: khour
    character(len=19),  intent(in)    :: olddate
    character(len=256), intent(out)   :: restart_flnm
    character(len=19)                 :: locdate
    integer                           :: ribeg
    integer                           :: riend
    logical                           :: lexist


    ribeg = index(restart_filename_requested, "<LATEST>")-1

    if ( ribeg > 0 ) then

       riend = ribeg + 9
       ! Find the latest RESTART file
       call geth_newdate(locdate(1:13), olddate(1:13), khour+24)
       RLOOP : do
          restart_flnm = restart_filename_requested(1:ribeg) //                 &
               locdate(1:4)//locdate(6:7)//locdate(9:10)//locdate(12:13) //     &
               restart_filename_requested(riend:)
          inquire (file=trim(restart_flnm), exist=lexist)
          if ( .not. lexist ) then
             call geth_newdate(locdate(1:13), locdate(1:13), -1)
             if (locdate(1:13) < startdate(1:13) ) then
                write(*, *)
                write(*, '(" ***** RESTART error: **************************************************")')
                write(*, '(" ***** ")')
                write(*, '(" *****       You have requested to restart from the latest restart file,")')
                write(*, '(" *****       but no restart file was found.")')
                write(*, '(" ***** ")')
                write(*, *)
                stop " ***** ERROR EXIT:  Cannot find a restart file"
             endif
          else
             exit RLOOP
          endif
       enddo RLOOP

    else

       restart_flnm = restart_filename_requested
       inquire (file=trim(restart_flnm), exist=lexist)
       if ( .not. lexist ) then
          write(*, *)
          write(*, '(" ***** RESTART error: **************************************************")')
          write(*, '(" ***** ")')
          write(*, '(" *****       You have requested to restart from a file that cannot be found.")')
          write(*, '(" *****       Specified restart file = ''", A, "''")') trim(restart_flnm)
          write(*, '(" ***** ")')
          write(*, *)
          stop " ***** ERROR EXIT:  Cannot find restart file"
       endif

    endif

    if (rank == 0) then
       write(*, '("Found restart file:  ''", A, "''")') trim(restart_flnm)
    endif

  end subroutine find_restart_file

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine read_hrldas_hdrinfo(wrfinput_flnm, ix, jx, &
       xstart, xend, ystart, yend,                      &
       iswater, isurban, llanduse, dx, dy, truelat1, truelat2, cen_lon, lat1, lon1, &
#ifdef _PARALLEL_
       igrid, mapproj, dum2d_ptr)
#else
       igrid, mapproj)
#endif
    ! Return the dimensions of the grid and some map information.
    implicit none
    character(len=*),   intent(in)    :: wrfinput_flnm
    integer,            intent(out)   :: mapproj
    integer,            intent(out)   :: igrid
    integer,            intent(out)   :: ix, jx    ! dimensions
    integer,            intent(in)    :: xstart, ystart ! Subwindow definition
    integer,            intent(inout) :: xend, yend     ! Subwindow definition
    integer,            intent(out)   :: iswater   ! vegetation category corresponding to water bodies
    integer,            intent(out)   :: isurban   ! vegetation category corresponding to urban areas
    character(len=256), intent(out)   :: llanduse  ! Landuse dataset (USGS or MODI)
    real,               intent(out)   :: dx
    real,               intent(out)   :: dy
    real,               intent(out)   :: truelat1
    real,               intent(out)   :: truelat2
    real,               intent(out)   :: cen_lon
    real,               intent(out)   :: lat1
    real,               intent(out)   :: lon1
#ifdef _PARALLEL_
    real, pointer,   dimension(:,:)   :: dum2d_ptr
#endif
    integer :: ncid, dimid, varid, ierr
    real, allocatable, dimension(:,:) :: dum2d
    character(len=256) :: units
    integer :: i
    integer :: rank

#ifdef _PARALLEL_  
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    if (ierr /= MPI_SUCCESS) stop "MPI_COMM_RANK"
#else
    rank = 0
#endif


    ! Open the NetCDF file.
    if (rank == 0) write(*,'("wrfinput_flnm: ''", A, "''")') trim(wrfinput_flnm)

!KWM#ifdef _PARALLEL_
!KWM    ierr = nf90_open_par(wrfinput_flnm, NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
!KWM#else
    ierr = nf90_open(wrfinput_flnm, NF90_NOWRITE, ncid)
!KWM#endif
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO: Problem opening wrfinput file: "//trim(wrfinput_flnm))

    ierr = nf90_inq_dimid(ncid, "west_east", dimid)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding dimension 'west_east'")

    ierr = nf90_inquire_dimension(ncid, dimid, len=ix)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding dimension length for 'west_east'")

    ierr = nf90_inq_dimid(ncid, "south_north", dimid)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding dimension 'south_north'")

    ierr = nf90_inquire_dimension(ncid, dimid, len=jx)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding dimension length for 'south_north'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "DX", dx)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding global attribute 'DX'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "DY", dy)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding global attribute 'DY'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT1", truelat1)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding global attribute 'TRUELAT1'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "TRUELAT2", truelat2)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding global attribute 'TRUELAT2'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "STAND_LON", cen_lon)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding global attribute 'STAND_LON'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "MAP_PROJ", mapproj)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding global attribute 'MAP_PROJ'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "GRID_ID", igrid)
    if (ierr /= 0) then
       ierr = nf90_get_att(ncid, NF90_GLOBAL, "grid_id", igrid)
       call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding global attribute 'GRID_ID' or 'grid_id'")
    endif

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "ISWATER", iswater)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding global attribute 'ISWATER'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "ISURBAN", isurban)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding global attribute 'ISURBAN'")

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "MMINLU", llanduse)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems finding global attribute 'MMINLU'")
    
    ! IBM XLF seems to need something like this:
    do i = 1, 256
       if (ichar(llanduse(i:i)) == 0) llanduse(i:i) = " "
    enddo

    if (xend == 0) then
       xend = ix
    endif
    if (yend == 0) then
       yend = jx
    endif

!
! This section is for reading the information from the wrfinput file
!

    ! We only need to read the one starting point.
    allocate(dum2d(xstart:xstart,ystart:ystart))
    call get_2d_netcdf("XLAT", ncid, dum2d,  units, xstart, xstart, ystart, ystart, FATAL, ierr)
    lat1 = dum2d(xstart,ystart)

    call get_2d_netcdf("XLONG", ncid, dum2d,  units, xstart, xstart, ystart, ystart, FATAL, ierr)
    lon1 = dum2d(xstart,ystart)
    deallocate (dum2d)

#ifdef _PARALLEL_
    allocate(dum2d_ptr(xstart:xend,ystart:yend))
    call get_2d_netcdf("IVGTYP", ncid, dum2d_ptr,  units, xstart, xend, ystart, yend, FATAL, ierr)
#endif

    ierr = nf90_close(ncid)
    call error_handler(ierr, failure="READ_HRLDAS_HDRINFO:  Problems closing NetCDF file.")

  end subroutine read_hrldas_hdrinfo

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine readland_hrldas(wrfinput_flnm,                            &
       xstart, xend,                                                   &
       ystart, yend,                                                   &
       iswater, vegtyp, soltyp, terrain, tbot_2d, latitude, longitude, &
			    refdk2d, refkdt2d)
    implicit none
    character(len=*),          intent(in)  :: wrfinput_flnm
    integer,                   intent(in)  :: xstart, xend, ystart, yend
    integer,                   intent(in)  :: iswater
    integer, dimension(xstart:xend,ystart:yend), intent(out) :: vegtyp, soltyp
    real,    dimension(xstart:xend,ystart:yend), intent(out) :: terrain
    real,    dimension(xstart:xend,ystart:yend), intent(out) :: tbot_2d
    real,    dimension(xstart:xend,ystart:yend), intent(out) :: latitude
    real,    dimension(xstart:xend,ystart:yend), intent(out) :: longitude
    real,    dimension(xstart:xend,ystart:yend), intent(out) :: refdk2d, refkdt2d

    character(len=256) :: units
    integer :: ierr, varid
    integer :: ncid
    real, dimension(xstart:xend,ystart:yend) :: xdum
    integer :: rank

#ifdef _PARALLEL_  
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    if (ierr /= MPI_SUCCESS) stop "MPI_COMM_RANK"
#else
    rank = 0
#endif


    ! Open the NetCDF file.
    if (rank == 0) write(*,'("wrfinput_flnm: ''", A, "''")') trim(wrfinput_flnm)
#ifdef _PARALLEL_
    ierr = nf90_open_par(wrfinput_flnm, NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
    ierr = nf90_open(wrfinput_flnm, NF90_NOWRITE, ncid)
#endif
    if (ierr /= 0) then
       write(*,'("READLAND_HRLDAS:  Problem opening wrfinput file: ''", A, "''")') trim(wrfinput_flnm)
#ifdef _PARALLEL_
       call mpi_finalize(ierr)
       if (ierr /= 0) write(*, '("Problem with MPI_finalize.")')
#endif
       stop
    endif

    ! Get Latitude (lat)
    call get_2d_netcdf("XLAT", ncid, latitude,  units, xstart, xend, ystart, yend, FATAL, ierr)
    ! print*, 'latitude(xstart,ystart) = ', latitude(xstart,ystart)

    ! Get Longitude (lon)
    call get_2d_netcdf("XLONG", ncid, longitude, units, xstart, xend, ystart, yend, FATAL, ierr)
    ! print*, 'longitude(xstart,ystart) = ', longitude(xstart,ystart)

    ! Get Terrain (avg)
    call get_2d_netcdf("HGT", ncid, terrain,   units, xstart, xend, ystart, yend, FATAL, ierr)
    ! print*, 'terrain(xstart,ystart) = ', terrain(xstart,ystart)

    ! Get Deep layer temperature (TMN)
    call get_2d_netcdf("TMN", ncid, tbot_2d,   units, xstart, xend, ystart, yend, FATAL, ierr)
    ! print*, 'terrain(xstart,ystart) = ', terrain(xstart,ystart)

    ! Get Dominant Land Use categories (use)
    call get_landuse_netcdf(ncid, xdum ,   units, xstart, xend, ystart, yend)
    vegtyp = nint(xdum)
    ! print*, 'vegtyp(xstart,ystart) = ', vegtyp(xstart,ystart)

    ! Get Dominant Soil Type categories in the top layer (stl)
    call get_soilcat_netcdf(ncid, xdum ,   units, xstart, xend, ystart, yend)
    soltyp = nint(xdum)
    ! print*, 'soltyp(xstart,ystart) = ', soltyp(xstart,ystart)

    ! read 2d parameter fields for Noah-LSM
    ierr = nf90_inq_varid(ncid,"REFDK", varid)
    if(ierr .eq. 0) then
     call get_2d_netcdf("REFDK", ncid, refdk2d,   units, xstart, xend, ystart, yend, FATAL, ierr)
    endif
    ierr = nf90_inq_varid(ncid,"REFKDT", varid)
    if(ierr .eq. 0) then
     call get_2d_netcdf("REFKDT", ncid, refkdt2d,   units, xstart, xend, ystart, yend, FATAL, ierr)
   endif

    ! Close the NetCDF file
    ierr = nf90_close(ncid)
    if (ierr /= 0) stop "MODULE_NOAHLSM_HRLDAS_INPUT:  READLAND_HRLDAS:  NF90_CLOSE"

    ! Make sure vegtyp and soltyp are consistent when it comes to water points,
    ! by setting soil category to water when vegetation category is water, and
    ! vice-versa.
    where (vegtyp == ISWATER) soltyp = 14
    where (soltyp == 14) vegtyp = ISWATER

  end subroutine readland_hrldas

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine get_landuse_netcdf(ncid, array, units, xstart, xend, ystart, yend)
    implicit none
    integer, intent(in) :: ncid
    integer, intent(in) :: xstart, xend, ystart, yend
    real, dimension(xstart:xend,ystart:yend), intent(out) :: array
    character(len=256), intent(out) :: units
    integer :: iret, varid
    character(len=24), parameter :: name = "IVGTYP"

    units = " "

    iret = nf90_inq_varid(ncid,  trim(name),  varid)
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_landuse_netcdf:  nf90_inq_varid"
    endif

    iret = nf90_get_var(ncid, varid, array, (/xstart, ystart/), (/xend-xstart+1, yend-ystart+1/))
    if (iret /= 0) then
       print*, 'name = "', trim(name)//'"'
       stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_landuse_netcdf:  nf90_get_var"
    endif

  end subroutine get_landuse_netcdf

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine get_soilcat_netcdf(ncid, array, units, xstart, xend, ystart, yend)
    implicit none
    integer, intent(in) :: ncid
    integer, intent(in) :: xstart, xend, ystart, yend
    real, dimension(xstart:xend,ystart:yend), intent(out) :: array
    character(len=256), intent(out) :: units
    integer :: iret, varid
    character(len=24), parameter :: name = "ISLTYP"

    units = " "

    iret = nf90_inq_varid(ncid,  trim(name),  varid)
    call error_handler(iret, "Problem finding variable '"//trim(name)//"' in the wrfinput file.")

    iret = nf90_get_var(ncid, varid, array, (/xstart, ystart/), (/xend-xstart+1, yend-ystart+1/))
    call error_handler(iret, "Problem retrieving variable "//trim(name)//" from the wrfinput file.")

  end subroutine get_soilcat_netcdf

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine get_2d_netcdf(name, ncid, array, units, xstart, xend, ystart, yend, &
       fatal_if_error, ierr)
    implicit none
    character(len=*), intent(in) :: name
    integer, intent(in) :: ncid
    integer, intent(in) :: xstart, xend, ystart, yend
    real, dimension(xstart:xend,ystart:yend), intent(out) :: array
    character(len=256), intent(out) :: units
    integer :: iret, varid
    ! FATAL_IF_ERROR:  an input code value:
    !      .TRUE. if an error in reading the data should stop the program.
    !      Otherwise the, IERR error flag is set, but the program continues.
    logical, intent(in) :: fatal_if_error 
    integer, intent(out) :: ierr

    units = " "

    iret = nf90_inq_varid(ncid,  name,  varid)
    if (iret /= 0) then
       if (FATAL_IF_ERROR) then
          print*, 'ncid = ', ncid
          call error_handler(iret, "MODULE_HRLDAS_NETCDF_IO:  Problem finding variable '"//trim(name)//"' in NetCDF file.")
       else
          ierr = iret
          return
       endif
    endif

    iret = nf90_get_att(ncid, varid, "units", units)
    if (iret /= 0) units = "units unknown"
!KWM    if (iret /= 0) then
!KWM       if (FATAL_IF_ERROR) then
!KWM          print*, 'name = "', trim(name)//'"'
!KWM          stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_2d_netcdf:  nf90_get_att:  units."
!KWM       else
!KWM          ierr = iret
!KWM          return
!KWM       endif
!KWM    endif

    iret = nf90_get_var(ncid, varid, values=array, start=(/xstart,ystart/), count=(/xend-xstart+1,yend-ystart+1/))
    if (iret /= 0) then
       if (FATAL_IF_ERROR) then
          print*, 'ncid =', ncid
          call error_handler(iret, "MODULE_HRLDAS_NETCDF_IO:  Problem retrieving variable '"//trim(name)//"' from NetCDF file.")
       else
          ierr = iret
          return
       endif
    endif

    ierr = 0;
  end subroutine get_2d_netcdf

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine get_2d_netcdf_soillevel(name, ncid, array, units, xstart, xend, ystart, yend, &
       fatal_if_error, layer_bottom, layer_top, ierr)
    implicit none
    character(len=*), intent(in) :: name
    integer, intent(in) :: ncid
    integer, intent(in) :: xstart, xend, ystart, yend
    real, dimension(xstart:xend,ystart:yend), intent(out) :: array
    character(len=256), intent(out) :: units
    ! FATAL_IF_ERROR:  an input code value:
    !      .TRUE. if an error in reading the data should stop the program.
    !      Otherwise the, IERR error flag is set, but the program continues.
    logical, intent(in) :: fatal_if_error 
    integer, intent(out) :: ierr
    real, intent(out) :: layer_bottom
    real, intent(out) :: layer_top

    integer :: iret, varid

    units = " "

    iret = nf90_inq_varid(ncid,  name,  varid)
    if (iret /= 0) then
       if (FATAL_IF_ERROR) then
          print*, 'name = "', trim(name)//'"'
          stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_2d_netcdf:  nf90_inq_varid"
       else
          ierr = iret
          return
       endif
    endif

    iret = nf90_get_att(ncid, varid, "units", units)
    if (iret /= 0) units = "units unknown"

    iret = nf90_get_var(ncid, varid, values=array, start=(/xstart,ystart/), count=(/xend-xstart+1,yend-ystart+1/))
    if (iret /= 0) then
       if (FATAL_IF_ERROR) then
          print*, 'name = "', trim(name)//'"'
          print*, 'varid =', varid
          print*, trim(nf90_strerror(iret))
          stop "MODULE_NOAHLSM_HRLDAS_INPUT:  get_2d_netcdf:  nf90_get_var"
       else
          ierr = iret
          return
       endif
    endif

    ! Also get some metadata about the soil layers:
    iret = nf90_get_att(ncid, varid, "layer_top", layer_top)
    call error_handler(iret, "Attempted to find 'layer_top' attribute on variable "//trim(name))

    iret = nf90_get_att(ncid, varid, "layer_bottom", layer_bottom)
    call error_handler(iret, "Attempted to find 'layer_bottom' attribute on variable "//trim(name))

    ierr = 0;
  end subroutine get_2d_netcdf_soillevel

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine readinit_hrldas(netcdf_flnm, xstart, xend, ystart, yend, nsoil, sldpth, target_date, &
       ldasin_version, smc, stc, sh2o, cmc, t1, weasd, snodep)
    implicit none
    character(len=*),                                  intent(in)  :: netcdf_flnm
    integer,                                           intent(in)  :: xstart, xend
    integer,                                           intent(in)  :: ystart, yend
    integer,                                           intent(in)  :: nsoil
    real,    dimension(nsoil),                         intent(in)  :: sldpth
    character(len=*),                                  intent(in)  :: target_date
    
    integer,                                           intent(out) :: ldasin_version
    real,    dimension(xstart:xend,ystart:yend,nsoil), intent(out) :: smc
    real,    dimension(xstart:xend,ystart:yend,nsoil), intent(out) :: stc
    real,    dimension(xstart:xend,ystart:yend,nsoil), intent(out) :: sh2o
    real,    dimension(xstart:xend,ystart:yend),       intent(out) :: cmc
    real,    dimension(xstart:xend,ystart:yend),       intent(out) :: t1
    real,    dimension(xstart:xend,ystart:yend),       intent(out) :: weasd
    real,    dimension(xstart:xend,ystart:yend),       intent(out) :: snodep

    character(len=256) :: titlestr
    character(len=256) :: units
    character(len=8)   :: name
    character(len=256) :: ldasin_llanduse

    integer :: ierr, ncid, ierr_snodep
    integer :: idx
    real, dimension(100) :: layer_bottom
    real, dimension(100) :: layer_top

    real, dimension(xstart:xend, ystart:yend, 4) :: soildummy
    integer :: rank

    !
    ! Open the NetCDF LDASIN file.
    !

#ifdef _PARALLEL_

    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    if (ierr /= MPI_SUCCESS) stop "MPI_COMM_RANK"

    ierr = nf90_open_par(netcdf_flnm, NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
    rank = 0
    ierr = nf90_open(netcdf_flnm, NF90_NOWRITE, ncid)
#endif
    if (rank == 0) write(*,'("netcdf_flnm: ''", A, "''")') trim(netcdf_flnm)
    if (ierr /= 0) then
       if (rank == 0) write(*,'("Problem opening netcdf file: ''", A, "''")') trim(netcdf_flnm)
#ifdef _PARALLEL_
       call mpi_finalize(ierr)
#endif
       stop
    endif

    !
    ! Check the NetCDF LDASIN file for a version number.
    !

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "TITLE", titlestr)
    if (ierr /= 0) then
       write(*,'("WARNING:  LDASIN file does not have TITLE attribute.")')
       write(*,'("          This probably means that LDASIN files are from an older release.")')
       write(*,'("          I assume you know what you are doing.")')
       ldasin_version = 0
    else
       write(*,'("LDASIN TITLE attribute: ", A)') trim(titlestr)
       ! Pull out the version number, assuming that the version is identified by vYYYYMMDD, and 
       ! based on a search for the string "v20".
       idx = index(trim(titlestr), "v20")
       if (idx <= 0) then
          write(*,'("FATAL:  LDASIN file has a perverse version identifier")')
          !  write(*,'("          I assume you know what you are doing.")')
          stop
       else
          read(titlestr(idx+1:), '(I8)', iostat=ierr) ldasin_version
          if (ierr /= 0) then
             write(*,'("FATAL:  LDASIN file has a perverse version identifier")')
             !  write(*,'("          I assume you know what you are doing.")')
             stop
          endif
       endif
    endif
    write(*, '("ldasin_version = ", I8)') ldasin_version

    ierr = nf90_get_att(ncid, NF90_GLOBAL, "MMINLU", ldasin_llanduse)
    if (ierr /= 0) then
       write(*,'("WARNING:  LDASIN file does not have MMINLU attribute.")')
       write(*,'("          This probably means that LDASIN files are from an older release.")')
       write(*,'("          I assume you know what you are doing.")')
    else
       write(*,'("LDASIN MMNINLU attribute: ", A)') ldasin_llanduse
    endif

    call get_2d_netcdf("CANWAT",     ncid, cmc,     units, xstart, xend, ystart, yend, FATAL, ierr)
    call get_2d_netcdf("SKINTEMP",   ncid, t1,      units, xstart, xend, ystart, yend, FATAL, ierr)
    call get_2d_netcdf("WEASD",      ncid, weasd,   units, xstart, xend, ystart, yend, FATAL, ierr)
    if (trim(units) == "m") then
       ! No conversion necessary
    else if (trim(units) == "mm") then
       ! convert WEASD from mm to m
       weasd = weasd * 1.E-3
    else if (trim(units) == "kg m{-2}") then
       ! convert WEASD from mm to m
       weasd = weasd * 1.E-3
    else if (trim(units) == "kg/m2") then
       ! convert WEASD from mm to m
       weasd = weasd * 1.E-3
    else
       print*, 'units = "'//trim(units)//'"'
       stop "Unrecognized units on WEASD"
    endif
    call get_2d_netcdf("SNODEP",     ncid, weasd,   units, xstart, xend, ystart, yend, NOT_FATAL, ierr_snodep)

    call get_2d_netcdf_soillevel("STEMP_1",    ncid, soildummy(:,:,1), units,  xstart, xend, ystart, yend, FATAL, &
         layer_bottom(1), layer_top(1), ierr)
    call get_2d_netcdf_soillevel("STEMP_2",    ncid, soildummy(:,:,2), units,  xstart, xend, ystart, yend, FATAL, &
         layer_bottom(2), layer_top(2), ierr)
    call get_2d_netcdf_soillevel("STEMP_3",    ncid, soildummy(:,:,3), units,  xstart, xend, ystart, yend, FATAL, &
         layer_bottom(3), layer_top(3), ierr)
    call get_2d_netcdf_soillevel("STEMP_4",    ncid, soildummy(:,:,4), units,  xstart, xend, ystart, yend, FATAL, &
         layer_bottom(4), layer_top(4), ierr)
    write(*, '("layer_bottom(1:4) = ", 4F9.4)') layer_bottom(1:4)
    write(*, '("layer_top(1:4)    = ", 4F9.4)') layer_top(1:4)
    write(*, '("Soil depth = ", 10F12.6)') sldpth

    call init_interp(xstart, xend, ystart, yend, nsoil, sldpth, stc, 4, soildummy, layer_bottom(1:4), layer_top(1:4))

    call get_2d_netcdf_soillevel("SMOIS_1",    ncid, soildummy(:,:,1), units,  xstart, xend, ystart, yend, FATAL, &
         layer_bottom(1), layer_top(1), ierr)
    call get_2d_netcdf_soillevel("SMOIS_2",    ncid, soildummy(:,:,2), units,  xstart, xend, ystart, yend, FATAL, &
         layer_bottom(2), layer_top(2), ierr)
    call get_2d_netcdf_soillevel("SMOIS_3",    ncid, soildummy(:,:,3), units,  xstart, xend, ystart, yend, FATAL, &
         layer_bottom(3), layer_top(3), ierr)
    call get_2d_netcdf_soillevel("SMOIS_4",    ncid, soildummy(:,:,4), units,  xstart, xend, ystart, yend, FATAL, &
         layer_bottom(4), layer_top(4), ierr)

    call init_interp(xstart, xend, ystart, yend, nsoil, sldpth, smc, 4, soildummy, layer_bottom(1:4), layer_top(1:4))


    if (ierr_snodep /= 0) then
       ! Quick assumption regarding snow depth.
       snodep = weasd * 10.
    endif

    sh2o = smc

    ierr = nf90_close(ncid)
  end subroutine readinit_hrldas

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine init_interp(xstart, xend, ystart, yend, nsoil, sldpth, var, nvar, src, layer_bottom, layer_top)
    implicit none
    integer, intent(in)    :: xstart, xend, ystart, yend, nsoil, nvar
    real, dimension(nsoil) :: sldpth ! the thickness of each layer
    real, dimension(xstart:xend, ystart:yend, nsoil), intent(out) :: var
    real, dimension(xstart:xend, ystart:yend, nvar ), intent(in)  :: src
    real, dimension(nvar),                            intent(in)  :: layer_bottom ! The depth from the surface of each layer bottom.
    real, dimension(nvar),                            intent(in)  :: layer_top    ! The depth from the surface of each layer top.
    integer :: i, j, k, kk, ktop, kbottom
    real, dimension(nsoil) :: dst_centerpoint
    real, dimension(nvar)  :: src_centerpoint
    real :: fraction
    integer :: ierr
    integer :: rank

#ifdef _PARALLEL_    
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    if (ierr /= MPI_SUCCESS) stop "MPI_COMM_RANK"
#else
    rank = 0
#endif

    do k = 1, nsoil
       if (k==1) then
          dst_centerpoint(k) = sldpth(k)/2.
       else
          dst_centerpoint(k) = sldpth(k)/2. + sum(sldpth(1:k-1))
       endif
       if (rank == 0) print*, 'k, dst_centerpoint(k) = ', k, dst_centerpoint(k)
    enddo
    print*

    do k = 1, nvar
       src_centerpoint(k) = 0.5*(layer_bottom(k)+layer_top(k))
       if (rank == 0) print*, 'k, src_centerpoint(k) = ', k, src_centerpoint(k)
    enddo

    KLOOP : do k = 1, nsoil

       if (dst_centerpoint(k) < src_centerpoint(1)) then
          ! If the center of the destination layer is closer to the surface than
          ! the center of the topmost source layer, then simply set the 
          ! value of the destination layer equal to the topmost source layer:
          if (rank == 0) then
             print'("Shallow destination layer:  Taking destination layer at ",F7.4, " from source layer at ", F7.4)', &
                  dst_centerpoint(k), src_centerpoint(1)
          endif
          var(:,:,k) = src(:,:,1)
          cycle KLOOP
       endif

       if (dst_centerpoint(k) > src_centerpoint(nvar)) then
          ! If the center of the destination layer is deeper than
          ! the center of the deepest source layer, then simply set the 
          ! value of the destination layer equal to the deepest source layer:
          if (rank == 0) then
             print'("Deep destination layer:  Taking destination layer at ",F7.4, " from source layer at ", F7.4)', &
                  dst_centerpoint(k), src_centerpoint(nvar)
          endif
          var(:,:,k) = src(:,:,nvar)
          cycle KLOOP
       endif

       ! Check if the center of the destination layer is "close" to the center
       ! of a source layer.  If so, simply set the value of the destination layer
       ! equal to the value of that close soil layer:
       do kk = 1, nvar
          if (abs(dst_centerpoint(k)-src_centerpoint(kk)) < 0.01) then
             if (rank == 0) then
                print'("(Near) match for destination layer:  Taking destination layer at ",F7.4, " from source layer at ", F7.4)', &
                     dst_centerpoint(k), src_centerpoint(kk)
             endif
             var(:,:,k) = src(:,:,kk)
             cycle KLOOP
          endif
       enddo

       ! Otherwise, do a linear interpolation

       ! Get ktop, the index of the top bracketing layer from the source dataset.
       ! Which from the bottom up, will be the first source level that is closer 
       ! to the surface than the destination level
       ktop = -99999
       TOPLOOP : do kk = nvar,1,-1
          if (src_centerpoint(kk) < dst_centerpoint(k)) then
             ktop = kk
             exit TOPLOOP
          endif
       enddo TOPLOOP
       if (ktop < -99998) stop "ktop problem"


       ! Get kbottom, the index of the bottom bracketing layer from the source dataset.
       ! Which, from the top down, will be the first source level that is deeper than
       ! the destination level
       kbottom = -99999
       BOTTOMLOOP : do kk = 1, nvar
          if ( src_centerpoint(kk) > dst_centerpoint(k) ) then
             kbottom = kk
             exit BOTTOMLOOP
          endif
       enddo BOTTOMLOOP
       if (kbottom < -99998) stop "kbottom problem"

       fraction = (src_centerpoint(kbottom)-dst_centerpoint(k)) / (src_centerpoint(kbottom)-src_centerpoint(ktop))

       ! print '(I2, 1x, 3F7.3, F8.5)', k, src_centerpoint(ktop), dst_centerpoint(k), src_centerpoint(kbottom), fraction

       if (rank == 0) then
          print '("dst(",I1,") = src(",I1,")*",F8.5," + src(",I1,")*",F8.5)', k, ktop, fraction, kbottom, (1.0-fraction)
       endif

       var(:,:,k) = (src(:,:,ktop)*fraction) + (src(:,:,kbottom)*(1.0-fraction))

    enddo KLOOP
     
  end subroutine init_interp

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine READVEG_HRLDAS(flnm, xstart, xend, ystart, yend, target_date, vegtyp, fpar, lai, gvfmin, gvfmax)

    implicit none
    character(len=*),                                  intent(in)  :: flnm
    integer,                                           intent(in)  :: xstart, xend
    integer,                                           intent(in)  :: ystart, yend
    character(len=*),                                  intent(in)  :: target_date
    integer, dimension(xstart:xend,ystart:yend),       intent(in)  :: vegtyp
    real,    dimension(xstart:xend,ystart:yend),       intent(out) :: fpar
    real,    dimension(xstart:xend,ystart:yend),       intent(out) :: lai
    real,    dimension(xstart:xend,ystart:yend),       intent(out) :: gvfmin
    real,    dimension(xstart:xend,ystart:yend),       intent(out) :: gvfmax

    character(len=8)   :: name
    character(len=256) :: units
    integer :: ierr

    integer :: ierr_fpar
    integer :: ierr_lai

    integer :: i, j
    integer :: iret, ncid

    ! Open the NetCDF file.
!KWM    write(*,'("flnm: ''", A, "''")') trim(flnm)
#ifdef _PARALLEL_
    iret = nf90_open_par(flnm, NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
    iret = nf90_open(flnm, NF90_NOWRITE, ncid)
#endif
    if (iret /= 0) then
       write(*,'("READVEG_HRLDAS:  Problem opening netcdf file: ''", A, "''")') trim(flnm)
       stop
    endif

    call get_2d_netcdf("VEGFRA",     ncid, fpar,     units, xstart, xend, ystart, yend, NOT_FATAL, ierr_fpar)
    call get_2d_netcdf("LAI",        ncid, lai,      units, xstart, xend, ystart, yend, NOT_FATAL, ierr_lai)

    if (ierr_fpar == 0) then
       ! fpar = fpar * 1.E-2 ! convert from percent to fraction
    else if (ierr_fpar /= 0) then
       ! Get it from tables
       ! print*,' READVEG_HRLDAS:  VEGFRA not found.  Initializing FPAR from table SHDTBL.'
       do i = xstart, xend
          do j = ystart, yend
!KWM             fpar(i,j) = shdtbl(vegtyp(i,j))
          enddo
       enddo
    endif
    if (ierr_lai /= 0) then
       ! Get it from tables
       ! print*,' READVEG_HRLDAS:  LAI not found.  Initializing LAI from table LAITBL.'
       do i = xstart, xend
          do j = ystart, yend
! Fixme for wrfcode input
!             lai(i,j) = laitbl(vegtyp(i,j))
          enddo
       enddo
    endif

    ! Get Minimum Green Vegetation Fraction GVFMIN
    call get_2d_netcdf("GVFMIN", ncid, gvfmin,   units, xstart, xend, ystart, yend, FATAL, ierr)

    ! Get Minimum Green Vegetation Fraction GVFMAX
    call get_2d_netcdf("GVFMAX", ncid, gvfmax,   units, xstart, xend, ystart, yend, FATAL, ierr)

    iret = nf90_close(ncid)
  end subroutine READVEG_HRLDAS

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine READFORC_HRLDAS(flnm_template, forcing_timestep, target_date, xstart, xend, ystart, yend,  &
       t,q,u,v,p,lw,sw,pcp,lai,fpar)
    use kwm_string_utilities
    implicit none

    character(len=*),                   intent(in)  :: flnm_template
    integer,                            intent(in)  :: forcing_timestep
    integer,                            intent(in)  :: xstart, xend
    integer,                            intent(in)  :: ystart, yend
    character(len=19),                  intent(in)  :: target_date ! (YYYY-MM-DD_hh:mm:ss)
    real,             dimension(xstart:xend,ystart:yend), intent(out)   :: t
    real,             dimension(xstart:xend,ystart:yend), intent(out)   :: q
    real,             dimension(xstart:xend,ystart:yend), intent(out)   :: u
    real,             dimension(xstart:xend,ystart:yend), intent(out)   :: v
    real,             dimension(xstart:xend,ystart:yend), intent(out)   :: p
    real,             dimension(xstart:xend,ystart:yend), intent(out)   :: lw
    real,             dimension(xstart:xend,ystart:yend), intent(out)   :: sw
    real,             dimension(xstart:xend,ystart:yend), intent(out)   :: pcp
    real,             dimension(xstart:xend,ystart:yend), intent(out)   :: lai
    real,             dimension(xstart:xend,ystart:yend), intent(inout) :: fpar

    character(len=256) :: flnm
    character(len=256) :: units
    character(len=256) :: nextflnm
    integer :: ierr
    integer :: ncid
    integer :: rank

    type(inputstruct) :: lastread = inputstruct("0000-00-00_00:00:00", &
         null(), null(), null(), null(), null(), null(), null(), null(), null(), null() )
    type(inputstruct) :: nextread= inputstruct("0000-00-00_00:00:00", &
         null(), null(), null(), null(), null(), null(), null(), null(), null(), null() )

!KWM    print*, 'target_date        = ', target_date
!KWM    print*, 'lastread%read_date = ', lastread%read_date
!KWM    print*, 'nextread%read_date = ', nextread%read_date

    if (target_date > nextread%read_date ) then
       !
       ! We've advanced beyond the date of the end-bracketing data in memory.
       ! Read the next (later) forcing data, and put the data into the nextread
       ! structure.
       !

       if (nextread%read_date /= "0000-00-00_00:00:00") then
          ! Clear the old lastread data
          call clear_inputstruct(lastread)

          ! Copy nextread to lastread
          lastread = nextread
       
          ! Clear nextread
          call nullify_inputstruct(nextread)
       endif

       ! Guess the next read date (from the last read date and the forcing timestep).
       ! If there is no last date, assume we're at the beginning of our processing
       ! and take the target_date as the first timestep, for which forcing data
       ! must be available.

       if (lastread%read_date == "0000-00-00_00:00:00") then
          nextread%read_date = target_date
       else
          call geth_newdate(nextread%read_date, lastread%read_date, forcing_timestep)
       endif

       ! Build a file name
       flnm = flnm_template
       call strrep(flnm, "<date>", nextread%read_date(1:4)//nextread%read_date(6:7)//nextread%read_date(9:10)//nextread%read_date(12:13))

!KWM       print*, 'new nextread%read_date = ', nextread%read_date
       !print*, 'read file:  ', trim(flnm)

       ! Open the NetCDF file.
#ifdef _PARALLEL_
       ierr = nf90_open_par(flnm, NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
       ierr = nf90_open(flnm, NF90_NOWRITE, ncid)
#endif
       if (ierr /= 0) then
#ifdef _PARALLEL_
          call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
          if (ierr /= MPI_SUCCESS) stop "MPI_COMM_RANK"
          if (rank == 0) then
#endif
             write(*,'("A)  Problem opening netcdf file: ''", A, "''")') trim(flnm)
#ifdef _PARALLEL_
          endif
          call mpi_finalize(ierr)
#endif
          stop
       endif

       ! Allocate space to hold data
       call allocate_inputstruct(nextread, xstart, xend, ystart, yend)

       ! Read the data
       call get_2d_netcdf("T2D",     ncid, nextread%t,     units, xstart, xend, ystart, yend, FATAL, ierr)
       call get_2d_netcdf("Q2D",     ncid, nextread%q,     units, xstart, xend, ystart, yend, FATAL, ierr)
       call get_2d_netcdf("U2D",     ncid, nextread%u,     units, xstart, xend, ystart, yend, FATAL, ierr)
       call get_2d_netcdf("V2D",     ncid, nextread%v,     units, xstart, xend, ystart, yend, FATAL, ierr)
       call get_2d_netcdf("PSFC",    ncid, nextread%p,     units, xstart, xend, ystart, yend, FATAL, ierr)
       call get_2d_netcdf("LWDOWN",  ncid, nextread%lw,    units, xstart, xend, ystart, yend, FATAL, ierr)
       call get_2d_netcdf("SWDOWN",  ncid, nextread%sw,    units, xstart, xend, ystart, yend, FATAL, ierr)
       call get_2d_netcdf("RAINRATE",ncid, nextread%pcp,   units, xstart, xend, ystart, yend, FATAL, ierr)
       call get_2d_netcdf("VEGFRA",  ncid, nextread%fpar,  units, xstart, xend, ystart, yend, NOT_FATAL, ierr)
       if (ierr /= 0) then
          ! print*, 'VEGFRA not found!'
          ! If we don't find a new VEGFRA, carry over the old one
          if (associated(lastread%fpar)) then
             nextread%fpar = lastread%fpar
          else
             nextread%fpar = fpar
          endif
       endif
       call get_2d_netcdf("LAI",     ncid, nextread%lai,   units, xstart, xend, ystart, yend, NOT_FATAL, ierr)
       if (ierr /= 0) then
          ! print*, 'LAI not found!'
          ! If we don't find a new LAI, carry over the old one
          if (associated(lastread%fpar)) then
             nextread%lai = lastread%lai
          endif
       endif

       ! Close the file
       ierr = nf90_close(ncid)

    endif

    if (target_date == nextread%read_date) then
       !
       ! We have advanced to the later date of our bracketing times for interpolation.
       ! Take that data as is, no interpolation necessary, move that data into the 
       ! lastread structure, and return that data.
       !

       ! Fill the t, q, u, v, ... arrays with data from the nextread structure.
       call copyfrom_inputstruct(nextread, t, q, u, v, p, lw, sw, pcp, fpar, lai, xstart, xend, ystart, yend)

       ! Clear the old lastread data
       call clear_inputstruct(lastread)

       ! Copy nextread to lastread
       lastread = nextread

       ! Set the nextread%read_date field to signal that we need to read
       nextread%read_date = "0000-00-00_00:00:00"

       ! Clear nextread
       call nullify_inputstruct(nextread)

    else if ( ( target_date > lastread%read_date ) .and. ( target_date < nextread%read_date ) ) then

       !
       ! We are at a Noah time step between the lastread data and the available nextread data.
       ! Do temporal interpolation and return the interpolated data.  Keep lastread
       ! and nextread as they were.
       !

       ! Fill the t, q, u, v, ... arrays with data interpolated between lastread and nextread times.
       call interpolate_inputstruct(lastread, nextread, target_date, &
            t, q, u, v, p, lw, sw, pcp, fpar, lai, xstart, xend, ystart, yend)

    else

       print*, 'target_date        = ', target_date
       print*, 'lastread%read_date = ', lastread%read_date
       print*, 'nextread%read_date = ', nextread%read_date

       STOP "We should not be here.  Problem with the logic of READFORC_SHORTER_TIMESTEP"

    endif

  end subroutine READFORC_HRLDAS

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine allocate_inputstruct(instruct, xstart, xend, ystart, yend)
    implicit none
    type(inputstruct)   :: instruct
    integer, intent(in) :: xstart
    integer, intent(in) :: xend
    integer, intent(in) :: ystart
    integer, intent(in) :: yend

    integer :: allostat

    allocate(instruct%t   (xstart:xend,ystart:yend), stat=allostat )
    if (allostat/=0) stop "Problem allocating instruct%t"

    allocate(instruct%q   (xstart:xend,ystart:yend), stat=allostat )
    if (allostat/=0) stop "Problem allocating instruct%q"

    allocate(instruct%u   (xstart:xend,ystart:yend), stat=allostat )
    if (allostat/=0) stop "Problem allocating instruct%u"

    allocate(instruct%v   (xstart:xend,ystart:yend), stat=allostat )
    if (allostat/=0) stop "Problem allocating instruct%v"

    allocate(instruct%p   (xstart:xend,ystart:yend), stat=allostat )
    if (allostat/=0) stop "Problem allocating instruct%p"

    allocate(instruct%lw  (xstart:xend,ystart:yend), stat=allostat )
    if (allostat/=0) stop "Problem allocating instruct%lw"

    allocate(instruct%sw  (xstart:xend,ystart:yend), stat=allostat )
    if (allostat/=0) stop "Problem allocating instruct%sw"

    allocate(instruct%pcp (xstart:xend,ystart:yend), stat=allostat )
    if (allostat/=0) stop "Problem allocating instruct%pcp"

    allocate(instruct%fpar(xstart:xend,ystart:yend), stat=allostat )
    if (allostat/=0) stop "Problem allocating instruct%fpar"

    allocate(instruct%lai (xstart:xend,ystart:yend), stat=allostat )
    if (allostat/=0) stop "Problem allocating instruct%lai"
  end subroutine allocate_inputstruct

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine copyfrom_inputstruct(instruct, t, q, u, v, p, lw, sw, pcp, fpar, lai, xstart, xend, ystart, yend)
    implicit none
    type(inputstruct), intent(in) :: instruct
    integer,           intent(in) :: xstart, xend, ystart, yend
    real, dimension(xstart:xend,ystart:yend), intent(out) :: t, q, u, v, p, lw, sw, pcp, fpar, lai
    t    = instruct%t
    q    = instruct%q
    u    = instruct%u
    v    = instruct%v
    p    = instruct%p
    lw   = instruct%lw
    sw   = instruct%sw
    pcp  = instruct%pcp
    fpar = instruct%fpar
    lai  = instruct%lai
  end subroutine copyfrom_inputstruct

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine interpolate_inputstruct(instructA, instructB, target_date, &
       t, q, u, v, p, lw, sw, pcp, fpar, lai, xstart, xend, ystart, yend)
    implicit none
    type(inputstruct),                        intent(in)  :: instructA, instructB
    character(len=19),                        intent(in)  :: target_date
    integer,                                  intent(in)  :: xstart, xend, ystart, yend
    real, dimension(xstart:xend,ystart:yend), intent(out) :: t, q, u, v, p, lw, sw, pcp, fpar, lai

    integer :: idts, idts2
    real    :: fraction

    call geth_idts(target_date, instructA%read_date, idts)
    call geth_idts(instructB%read_date, instructA%read_date, idts2)

    fraction = real(idts2-idts)/real(idts2)
    t  = ( instructA%t  * fraction ) + ( instructB%t  * (1.0-fraction) )
    q  = ( instructA%q  * fraction ) + ( instructB%q  * (1.0-fraction) )
    u  = ( instructA%u  * fraction ) + ( instructB%u  * (1.0-fraction) )
    v  = ( instructA%v  * fraction ) + ( instructB%v  * (1.0-fraction) )
    p  = ( instructA%p  * fraction ) + ( instructB%p  * (1.0-fraction) )
    lw = ( instructA%lw * fraction ) + ( instructB%lw * (1.0-fraction) )
    sw = ( instructA%sw * fraction ) + ( instructB%sw * (1.0-fraction) )
    pcp = instructA%pcp
    fpar = instructA%fpar
    lai  = instructA%lai
  end subroutine interpolate_inputstruct

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine clear_inputstruct(instruct)
    implicit none
    type(inputstruct) :: instruct

    if (associated(instruct%t)) then
       deallocate(instruct%t)
       nullify(instruct%t)
    endif

    if (associated(instruct%q)) then
       deallocate(instruct%q)
       nullify(instruct%q)
    endif

    if (associated(instruct%u)) then
       deallocate(instruct%u)
       nullify(instruct%u)
    endif

    if (associated(instruct%v)) then
       deallocate(instruct%v)
       nullify(instruct%v)
    endif

    if (associated(instruct%p)) then
       deallocate(instruct%p)
       nullify(instruct%p)
    endif

    if (associated(instruct%lw)) then
       deallocate(instruct%lw)
       nullify(instruct%lw)
    endif

    if (associated(instruct%sw)) then
       deallocate(instruct%sw)
       nullify(instruct%sw)
    endif

    if (associated(instruct%pcp)) then
       deallocate(instruct%pcp)
       nullify(instruct%pcp)
    endif

    if (associated(instruct%fpar)) then
       deallocate(instruct%fpar)
       nullify(instruct%fpar)
    endif

    if (associated(instruct%lai)) then
       deallocate(instruct%lai)
       nullify(instruct%lai)
    endif
  end subroutine clear_inputstruct

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine nullify_inputstruct(instruct)
    implicit none
    type(inputstruct) :: instruct

    nullify(instruct%t)
    nullify(instruct%q)
    nullify(instruct%u)
    nullify(instruct%v)
    nullify(instruct%p)
    nullify(instruct%lw)
    nullify(instruct%sw)
    nullify(instruct%pcp)
    nullify(instruct%fpar)
    nullify(instruct%lai)
  end subroutine nullify_inputstruct

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine READSNOW_HRLDAS(flnm,xstart,xend,ystart,yend,target_date,weasd,snodep)
    implicit none

    character(len=*),                                     intent(in)  :: flnm
    integer,                                              intent(in)  :: xstart, xend
    integer,                                              intent(in)  :: ystart, yend
    character(len=*),                                     intent(in)  :: target_date
    real,             dimension(xstart:xend,ystart:yend), intent(out) :: weasd
    real,             dimension(xstart:xend,ystart:yend), intent(out) :: snodep

    character(len=256) :: units
    integer :: ierr
    integer :: ncid

    ! Open the NetCDF file.

#ifdef _PARALLEL_
    ierr = nf90_open_par(flnm, NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
    ierr = nf90_open(flnm, NF90_NOWRITE, ncid)
#endif
    if (ierr /= 0) then
       write(*,'("READSNOW_HRLDAS:  Problem opening netcdf file: ''", A, "''")') trim(flnm)
       stop
    endif

    call get_2d_netcdf("WEASD",  ncid, weasd,   units, xstart, xend, ystart, yend, FATAL, ierr)

    if (trim(units) == "m") then
       ! No conversion necessary
    else if (trim(units) == "mm") then
       ! convert WEASD from mm to m
       weasd = weasd * 1.E-3
    else if (trim(units) == "kg m{-2}") then
       ! convert WEASD from mm to m
       weasd = weasd * 1.E-3
    else if (trim(units) == "kg/m2") then
       ! convert WEASD from mm to m
       weasd = weasd * 1.E-3
    else
       print*, 'units = "'//trim(units)//'"'
       stop "Unrecognized units on WEASD"
    endif

    call get_2d_netcdf("SNODEP",     ncid, snodep,   units, xstart, xend, ystart, yend, NOT_FATAL, ierr)

    if (ierr /= 0) then
       ! Quick assumption regarding snow depth.
       snodep = weasd * 10.
    endif

    ierr = nf90_close(ncid)

  end subroutine READSNOW_HRLDAS

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine prepare_output_file(outdir, version, iz0tlnd, sfcdif_option, ucmcall, igrid, &
       output_timestep, llanduse, split_output_count, hgrid, &
       ixfull, jxfull, ixpar, jxpar, xstartpar, ystartpar, iswater,  &
       mapproj, lat1, lon1, dx, dy, truelat1, truelat2, cen_lon,     &
       nsoil, sldpth, startdate, date, &
       vegtyp, soltyp)
    ! To prepare the output file, we create the file, write dimensions and attributes, write the time variable.
    ! At the end of this routine, the output file is out of define mode.
    implicit none
#include <netcdf.inc>

    character(len=*),                         intent(in) :: outdir
    character(len=*),                         intent(in) :: version
    integer,                                  intent(in) :: sfcdif_option
    integer,                                  intent(in) :: iz0tlnd
    integer,                                  intent(in) :: ucmcall
    integer,                                  intent(in) :: igrid
    integer,                                  intent(in) :: output_timestep
    character(len=*),                         intent(in) :: llanduse
    integer,                                  intent(in) :: split_output_count
    character,                                intent(in) :: hgrid
    integer,                                  intent(in) :: ixfull
    integer,                                  intent(in) :: jxfull
    integer,                                  intent(in) :: ixpar
    integer,                                  intent(in) :: jxpar
    integer,                                  intent(in) :: xstartpar
    integer,                                  intent(in) :: ystartpar
    integer,                                  intent(in) :: iswater
    integer,                                  intent(in) :: mapproj
    real,                                     intent(in) :: lat1
    real,                                     intent(in) :: lon1
    real,                                     intent(in) :: dx
    real,                                     intent(in) :: dy
    real,                                     intent(in) :: truelat1
    real,                                     intent(in) :: truelat2
    real,                                     intent(in) :: cen_lon
    integer,                                  intent(in) :: nsoil
    real,             dimension(nsoil),       intent(in) :: sldpth
    character(len=19),                        intent(in) :: startdate
    character(len=19),                        intent(in) :: date
    integer,          dimension(ixpar,jxpar), intent(in) :: vegtyp
    integer,          dimension(ixpar,jxpar), intent(in) :: soltyp

    integer :: ncid

    integer :: dimid_ix, dimid_jx, dimid_times, dimid_datelen, varid, n
    integer :: dimid_dum, dimid_layers
    integer :: iret
    character(len=256) :: output_flnm
    character(len=19)  :: date19

    integer :: ierr

    if (output_count_remember == 0) then
       ! If this is a new output file:
       !   We have to create a new file, do dimension initializations, and write global attributes to the file.
       !   Then we get out of define mode.
       if (mod(output_timestep,3600) == 0) then
          write(output_flnm, '(A,"/",A10,".LDASOUT_DOMAIN",I1)') outdir, date(1:4)//date(6:7)//date(9:10)//date(12:13), igrid
       elseif (mod(output_timestep,60) == 0) then
          write(output_flnm, '(A,"/",A12,".LDASOUT_DOMAIN",I1)') outdir, date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
       else
          write(output_flnm, '(A,"/",A14,".LDASOUT_DOMAIN",I1)') outdir, date(1:4)//date(6:7)//date(9:10)//date(12:13)//date(15:16)//date(18:19), igrid
       endif
#ifdef _PARALLEL_
       iret = nf90_create(trim(output_flnm), &
            OR(NF90_CLOBBER, NF90_NETCDF4), ncid, comm=MPI_COMM_WORLD, info=MPI_INFO_NULL)
#else
#ifdef WRFIO_NCD_LARGE_FILE_SUPPORT
       iret = nf90_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#else
       iret = nf90_create(trim(output_flnm), NF90_CLOBBER, ncid)
#endif
#endif
       call error_handler(iret, failure="Problem nf90_create for "//trim(output_flnm))

       ncid_remember = ncid
       define_mode_remember = .TRUE.

       iret = nf90_def_dim(ncid, "Time", NF90_UNLIMITED, dimid_times)
       iret = nf90_def_dim(ncid, "DateStrLen", 19, dimid_datelen)
       ! Dimensions reflect the full size of the subwindow (not the strip known by this particular process).
       iret = nf90_def_dim(ncid, "west_east", ixfull, dimid_ix)
       iret = nf90_def_dim(ncid, "south_north", jxfull, dimid_jx)
       iret = nf90_def_dim(ncid, "west_east_stag", ixfull+1, dimid_dum)
       iret = nf90_def_dim(ncid, "south_north_stag", jxfull+1, dimid_dum)
       iret = nf90_def_dim(ncid, "soil_layers_stag", nsoil, dimid_layers)

       iret = nf90_put_att(ncid, NF90_GLOBAL, "TITLE", "OUTPUT FROM HRLDAS "//version)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "missing_value", -1.E33)

       ! TODO:  Add Grid information   (should look more-or-less like wrfout files)
       ! TODO:  Add Units information  (should look more-or-less like wrfout files)

       date19(1:19) = "0000-00-00_00:00:00"
       date19(1:len_trim(startdate)) = startdate

       iret = nf90_put_att(ncid, NF90_GLOBAL, "START_DATE", date19)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "MAP_PROJ", mapproj)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "LAT1", lat1)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "LON1", lon1)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "DX", dx)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "DY", dy)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT1", truelat1)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT2", truelat2)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "STAND_LON", cen_lon)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "MMINLU", llanduse)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "IZ0TLND", iz0tlnd)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "SFCDIF_OPTION", sfcdif_option)
       iret = nf90_put_att(ncid, NF90_GLOBAL, "UCMCALL", ucmcall)

!
! Done with dimensions and global attributes.
! Now define and describe our "Times" variable.
!

       iret = nf90_def_var(ncid,  "Times",  NF90_CHAR, (/dimid_datelen,dimid_times/), varid)
       call error_handler(iret, failure="Problem nf90_def_var for "//trim(output_flnm))

       iret = nf90_enddef(ncid)
       call error_handler(iret, failure="Problem nf90_enddef")
       define_mode_remember = .FALSE.

    endif
    xstartpar_remember = xstartpar
    dimid_ix_remember = dimid_ix
    dimid_jx_remember = dimid_jx
    dimid_times_remember = dimid_times
    dimid_layers_remember = dimid_layers
    iswater_remember = iswater

    allocate(vegtyp_remember(ixpar,jxpar))
    vegtyp_remember = vegtyp

!
! While we're here, put the data for the "Times" variable to the NetCDF file.
!

    date19(1:19) = "0000-00-00_00:00:00"
    date19(1:len_trim(date)) = date
    iret = nf90_inq_varid(ncid_remember, "Times", varid)
    call error_handler(iret, "OUTPUT_HRLDAS:  Problem inquiring on 'Times'")

    iret = nf90_put_var(ncid_remember, varid, date, (/1,output_count_remember+1/), (/19,1/))
    call error_handler(iret, "OUTPUT_HRLDAS:  Problem writing variable 'Times'")

  end subroutine prepare_output_file

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine set_output_define_mode(imode)
    implicit none
    integer, intent(in) :: imode
    integer :: ierr

    if (imode == 1) then
       ! We need to define things only with a new file, i.e., only when output_count_remember == 0
       if (output_count_remember > 0) return
       ierr = nf90_redef(ncid_remember)
       call error_handler(ierr, failure="Problem nf90_redef")
       define_mode_remember = .TRUE.
    else
       if (define_mode_remember) then
          ierr = nf90_enddef(ncid_remember)
          call error_handler(ierr, failure="Problem nf90_enddef")
          define_mode_remember = .FALSE.
       endif
    endif

  end subroutine set_output_define_mode

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine finalize_output_file(split_output_count)
    implicit none
    integer, intent(in)  :: split_output_count
    integer :: ierr

    output_count_remember = output_count_remember + 1
    if (output_count_remember == split_output_count) then
       output_count_remember = 0 
       ierr = nf90_close(ncid_remember)
       call error_handler(ierr, failure="Problem nf90_close for output file")
    else
       ierr = nf90_sync(ncid_remember)
       call error_handler(ierr, failure="Problem nf90_sync for output file")
    endif

    deallocate(vegtyp_remember)

  end subroutine finalize_output_file

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine add_to_output_2d_float ( array, name, description, units )
    implicit none
    real, dimension(:,:), intent(in) :: array
    character(len=*), intent(in) :: name, description, units
    integer :: ixpar, jxpar

    if (define_mode_remember) then
       call make_var_att_2d ( ncid_remember , dimid_ix_remember , dimid_jx_remember , dimid_times_remember , &
            NF90_FLOAT , trim(name) , trim(description) , trim(units) )
    else 
       ixpar = size(array,1)
       jxpar = size(array,2)
       call put_var_2d (ncid_remember , output_count_remember+1 , vegtyp_remember , iswater_remember , &
            ixpar , jxpar , xstartpar_remember , trim(name) , array )
    endif
  end subroutine add_to_output_2d_float

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine add_to_output_2d_integer ( array, name, description, units )
    implicit none
    integer, dimension(:,:), intent(in) :: array
    character(len=*), intent(in) :: name, description, units
    integer :: ixpar, jxpar

    if (define_mode_remember) then
       call make_var_att_2d ( ncid_remember , dimid_ix_remember , dimid_jx_remember , dimid_times_remember , &
            NF90_INT , trim(name) , trim(description) , trim(units) )
    else
       ixpar = size(array,1)
       jxpar = size(array,2)
       call put_var_int (ncid_remember , output_count_remember+1 , vegtyp_remember , iswater_remember , &
            ixpar , jxpar , xstartpar_remember , trim(name) , array )
    endif
  end subroutine add_to_output_2d_integer

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine add_to_output_3d ( array, name, description, units )
    implicit none
    real, dimension(:,:,:), intent(in) :: array
    character(len=*), intent(in) :: name, description, units
    integer :: ixpar, jxpar, kxpar

    if (define_mode_remember) then
       call make_var_att_3d ( ncid_remember , dimid_ix_remember , dimid_jx_remember , dimid_times_remember , &
            NF90_FLOAT , dimid_layers_remember, trim(name) , trim(description) , trim(units) )
    else 

       ixpar = size(array,1)
       jxpar = size(array,2)
       kxpar = size(array,3)

       call put_var_3d (ncid_remember , output_count_remember+1 , vegtyp_remember , iswater_remember , &
            ixpar , jxpar , xstartpar_remember , kxpar, trim(name) , array )
    endif
  end subroutine add_to_output_3d

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine make_var_att_2d(ncid, dimid_ix, dimid_jx, dimid_times, itype, varname, vardesc, varunits)
    implicit none
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: vardesc
    character(len=*), intent(in) :: varunits
    integer,          intent(in) :: dimid_ix
    integer,          intent(in) :: dimid_jx
    integer,          intent(in) :: dimid_times
    integer,          intent(in) :: itype
    integer :: iret
    integer :: varid

    iret = nf90_def_var(ncid,  varname,   itype, (/dimid_ix,dimid_jx,dimid_times/), varid)
    call error_handler(iret, "MAKE_VAR_ATT_2D: Failure defining variable "//trim(varname))

    iret = nf90_put_att(ncid, varid, "MemoryOrder", "XY ")
    call error_handler(iret, "MAKE_VAR_ATT_2D: Failure adding MemoryOrder attribute to variable "//trim(varname))

    iret = nf90_put_att(ncid, varid, "description", vardesc)
    call error_handler(iret, "MAKE_VAR_ATT_2D: Failure adding description attribute to variable "//trim(varname))

    iret = nf90_put_att(ncid, varid, "units", varunits)
    call error_handler(iret, "MAKE_VAR_ATT_2D: Failure adding units attribute '"//trim(varunits)//"' to variable "//trim(varname))

    iret = nf90_put_att(ncid, varid, "stagger", "-")
    call error_handler(iret, "MAKE_VAR_ATT_2D: Failure adding stagger attribute to variable "//trim(varname))

  end subroutine make_var_att_2d

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine make_var_att_3d(ncid, dimid_ix, dimid_jx, dimid_times, itype, dimid_layers, varname, vardesc, varunits)
    implicit none
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: vardesc
    character(len=*), intent(in) :: varunits
    integer,          intent(in) :: dimid_ix
    integer,          intent(in) :: dimid_jx
    integer,          intent(in) :: dimid_times
    integer,          intent(in) :: dimid_layers
    integer,          intent(in) :: itype
    integer :: iret
    integer :: varid

    iret = nf90_def_var(ncid,  varname, itype, (/dimid_ix,dimid_jx,dimid_layers,dimid_times/), varid)
    call error_handler(iret, "MAKE_VAR_ATT_3D:  Failure defining variable "//trim(varname))

    iret = nf90_put_att(ncid, varid, "MemoryOrder", "XYZ")
    call error_handler(iret, "MAKE_VAR_ATT_3D: Failure adding MemoryOrder attribute for variable "//trim(varname))

    iret = nf90_put_att(ncid, varid, "description", vardesc)
    call error_handler(iret, "MAKE_VAR_ATT_3D: Failure adding description attribute to variable "//trim(varname))

    iret = nf90_put_att(ncid, varid, "units", varunits)
    call error_handler(iret, "MAKE_VAR_ATT_3D: Failure adding units attribute '"//trim(varunits)//"' to variable "//trim(varname))

    iret = nf90_put_att(ncid, varid, "stagger", "Z")
    call error_handler(iret, "MAKE_VAR_ATT_3D: Failure adding stagger attribute to variable "//trim(varname))

  end subroutine make_var_att_3d

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine put_var_2d(ncid, output_count, vegtyp, iswater, ix, jx, xstart, varname, vardata)
    implicit none
    integer,                   intent(in) :: ncid
    integer,                   intent(in) :: output_count
    character(len=*),          intent(in) :: varname
    integer,                   intent(in) :: ix
    integer,                   intent(in) :: jx
    integer,                   intent(in) :: xstart
    integer, dimension(ix,jx), intent(in) :: vegtyp
    integer,                   intent(in) :: iswater
    real,    dimension(ix,jx), intent(in) :: vardata

    real,    dimension(ix,jx)             :: xdum
    integer                               :: iret
    integer                               :: varid

    integer, dimension(3) :: nstart
    integer, dimension(3) :: ncount

    where (vegtyp == ISWATER)
       xdum = -1.E33
    elsewhere
       xdum = vardata
    endwhere

    iret = nf90_inq_varid(ncid,  varname, varid)
    call error_handler(iret, "Subroutine PUT_VAR_2D:  Problem finding variable id for "//trim(varname)//".")

    nstart = (/ xstart ,  1 , output_count /)
    ncount = (/     ix , jx ,            1 /)

    iret = nf90_put_var(ncid, varid, xdum, start=nstart, count=ncount)
    call error_handler(iret, "Subroutine PUT_VAR_2D:  Problem putting variable "//trim(varname)//" to NetCDF file.")

  end subroutine put_var_2d

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine put_var_int(ncid, output_count, vegtyp, iswater, ix, jx, xstart, varname, vardata)
    implicit none
    integer,                                              intent(in) :: ncid
    integer,                                              intent(in) :: output_count
    character(len=*),                                     intent(in) :: varname
    integer,                                              intent(in) :: ix
    integer,                                              intent(in) :: jx
    integer,                                              intent(in) :: xstart
    integer,                                              intent(in) :: iswater
    integer, dimension(ix,jx),                            intent(in) :: vegtyp
    integer, dimension(ix,jx),                            intent(in) :: vardata

    integer                                                          :: iret
    integer                                                          :: varid

    integer, dimension(3)                                            :: nstart
    integer, dimension(3)                                            :: ncount

    nstart = (/ xstart ,  1 , output_count /)
    ncount = (/     ix , jx ,            1 /)

    iret = nf90_inq_varid(ncid,  varname, varid)
    call error_handler(iret, failure="Subroutine PUT_VAR_INT:  Problem finding variable id for variable: "//varname)

    iret = nf90_put_var(ncid, varid, vardata, nstart, ncount)
    call error_handler(iret, failure="Subroutine PUT_VAR_INT:  Problem putting variable '"//varname//"' to NetCDF file.")

  end subroutine put_var_int

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine put_var_3d(ncid, output_count, vegtyp, iswater, ix, jx, xstart, nsoil, varname, vardata)
    implicit none
    integer,                                                    intent(in) :: ncid
    integer,                                                    intent(in) :: output_count
    character(len=*),                                           intent(in) :: varname
    integer,                                                    intent(in) :: ix
    integer,                                                    intent(in) :: jx
    integer,                                                    intent(in) :: xstart
    integer,                                                    intent(in) :: nsoil
    integer,                                                    intent(in) :: iswater
    integer, dimension(ix, jx),                                 intent(in) :: vegtyp
    real,    dimension(ix, jx, nsoil),                          intent(in) :: vardata
    real,    dimension(ix, jx, nsoil)                                      :: xdum
    integer                                                                :: iret
    integer                                                                :: varid
    integer                                                                :: n
    integer, dimension(4)                                                  :: nstart
    integer, dimension(4)                                                  :: ncount

    nstart = (/ xstart ,  1 ,     1 , output_count /)
    ncount = (/     ix , jx , nsoil ,            1 /)

    xdum = vardata
    do n = 1, nsoil
       where (vegtyp(:,:) == ISWATER) xdum(:,:,n) = -1.E33
    enddo

    iret = nf90_inq_varid(ncid,  varname, varid)
    call error_handler(iret, "Subroutine PUT_VAR_3D:  Problem finding variable id for "//trim(varname)//".")

    iret = nf90_put_var(ncid, varid, xdum, start=nstart, count=ncount)
    call error_handler(iret, "Subroutine PUT_VAR_3D:  Problem putting variable "//trim(varname)//" to NetCDF file.")

  end subroutine put_var_3d

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
  subroutine finalize_restart_file()
    implicit none

    deallocate(vegtyp_remember)
    restart_filename_remember = " "
    iswater_remember   = -999999
    xstartpar_remember = -999999
    
  end subroutine finalize_restart_file

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine prepare_restart_file(outdir, version, iz0tlnd, sfcdif_option, ucmcall, igrid, llanduse, olddate, startdate,  &
       ixfull, jxfull, ixpar, jxpar, xstartpar, ystartpar,                                    &
       nsoil, dx, dy, truelat1, truelat2, mapproj, lat1, lon1, cen_lon,                       &
       iswater, vegtyp)

    implicit none
#include <netcdf.inc>

    character(len=*),                      intent(in) :: outdir
    character(len=*),                      intent(in) :: version
    integer,                               intent(in) :: iz0tlnd
    integer,                               intent(in) :: sfcdif_option
    integer,                               intent(in) :: ucmcall
    integer,                               intent(in) :: igrid
    character(len=*),                      intent(in) :: llanduse
    character(len=*),                      intent(in) :: olddate
    character(len=*),                      intent(in) :: startdate
    integer,                               intent(in) :: ixfull
    integer,                               intent(in) :: jxfull
    integer,                               intent(in) :: ixpar
    integer,                               intent(in) :: jxpar
    integer,                               intent(in) :: xstartpar
    integer,                               intent(in) :: ystartpar
    integer,                               intent(in) :: nsoil
    real,                                  intent(in) :: dx, dy
    real,                                  intent(in) :: truelat1, truelat2
    integer,                               intent(in) :: mapproj
    real,                                  intent(in) :: lat1, lon1, cen_lon
    integer,                               intent(in) :: iswater
    integer, dimension(ixpar,jxpar),       intent(in) :: vegtyp

    character(len=1) :: hgrid
    integer :: ncid
    character(len=256) :: output_flnm
    integer :: ierr
    integer :: varid
    integer :: dimid_times, dimid_datelen, dimid_ix, dimid_jx, dimid_dum, dimid_layers
    character(len=19) :: date19
    integer :: rank

#ifdef _PARALLEL_

    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    if (ierr /= MPI_SUCCESS) stop "MPI_COMM_RANK"

#else

    rank = 0

#endif


    write(output_flnm, '(A,"/RESTART.",A10,"_DOMAIN",I1)') trim(outdir), olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13), igrid
    if (rank==0) print*, 'output_flnm = "'//trim(output_flnm)//'"'

    restart_filename_remember = output_flnm
    iswater_remember   = iswater
    xstartpar_remember = xstartpar
    allocate(vegtyp_remember(ixpar,jxpar))
    vegtyp_remember = vegtyp

#ifdef _PARALLEL_

    ierr = nf90_create(trim(output_flnm), &
         OR(NF90_CLOBBER, NF90_NETCDF4), ncid, comm=MPI_COMM_WORLD, info=MPI_INFO_NULL)
#else
#ifdef WRFIO_NCD_LARGE_FILE_SUPPORT
       ierr = nf90_create(trim(output_flnm), IOR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
#else
    ierr = nf90_create(trim(output_flnm), NF90_CLOBBER, ncid)
#endif
#endif

    if (ierr /= 0) stop "Problem nf_create"

    ierr = nf90_def_dim(ncid, "Time", NF90_UNLIMITED, dimid_times)
    ierr = nf90_def_dim(ncid, "DateStrLen", 19, dimid_datelen)
    ierr = nf90_def_dim(ncid, "west_east", ixfull, dimid_ix)
    ierr = nf90_def_dim(ncid, "south_north", jxfull, dimid_jx)
    ierr = nf90_def_dim(ncid, "west_east_stag", ixfull+1, dimid_dum)
    ierr = nf90_def_dim(ncid, "south_north_stag", jxfull+1, dimid_dum)
    ierr = nf90_def_dim(ncid, "soil_layers_stag", nsoil, dimid_layers)

    ierr = nf90_put_att(ncid, NF90_GLOBAL, "TITLE", "RESTART FILE FROM HRLDAS "//version)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "missing_value", -1.E33)

    date19(1:19) = "0000-00-00_00:00:00"
    date19(1:len_trim(startdate)) = startdate

    ierr = nf90_put_att(ncid, NF90_GLOBAL, "START_DATE", date19)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "MAP_PROJ", mapproj)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "LAT1", lat1)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "LON1", lon1)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "DX", dx)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "DY", dy)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT1", truelat1)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT2", truelat2)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "STAND_LON", cen_lon)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "MMINLU", llanduse)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "IZ0TLND", iz0tlnd)
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "SFCDIF_OPTION", sfcdif_option)
#ifdef _HRLDAS_URBAN_
    ierr = nf90_put_att(ncid, NF90_GLOBAL, "UCMCALL", ucmcall)
#endif

!
! Done with dimensions and global attributes.
! Now define and describe all our NetCDF restart variables.
!

    ierr = nf90_def_var(ncid,  "Times",  NF90_CHAR, (/dimid_datelen,dimid_times/), varid)
    ierr = nf90_enddef(ncid)

!
! Done defining and describing all our NetCDF restart variables.
! Now actually put the data for each variable into the NetCDF file.
!

    date19(1:19) = "0000-00-00_00:00:00"
    date19(1:len_trim(olddate)) = olddate

    ierr = nf90_inq_varid(ncid, "Times", varid)
    call error_handler(ierr, "WRITE_RESTART:  Problem inquiring varid for 'Times'")

    ierr = nf90_put_var(ncid, varid, olddate, (/1,1/), (/19,1/))
    call error_handler(ierr, "WRITE_RESTART:  problem putting 'Times' to restart file")

    ierr = nf90_close(ncid)
    call error_handler(ierr, "WRITE_RESTART:  nf90_close")

  end subroutine prepare_restart_file

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------

  subroutine add_to_restart_2d_float(array, name, units, description)
    implicit none
    real,            dimension(:,:),                              intent(in) :: array
    character(len=*),                                             intent(in) :: name
    character(len=*), optional,                                   intent(in) :: units
    character(len=*), optional,                                   intent(in) :: description

    character(len=256) :: output_flnm
    integer :: ncid
    integer :: ierr
    integer :: dimid_ix
    integer :: dimid_jx
    integer :: dimid_times
    integer :: ixout
    integer :: xstartout
    integer :: iswater
    character(len=256) :: local_units
    character(len=256) :: local_description

    integer :: ixpar
    integer :: jxpar

    output_flnm = restart_filename_remember
    iswater     = iswater_remember

    ixpar = size(array,1)
    jxpar = size(array,2)

    if (present(units)) then
       local_units = units
    else
       local_units = "-"
    endif

    if (present(description)) then
       local_description = description
    else
       local_description = "-"
    endif
    
#ifdef _PARALLEL_
    ierr = nf90_open_par(trim(output_flnm), NF90_WRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
    ierr = nf90_open(trim(output_flnm), NF90_WRITE, ncid)
#endif
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_open")

    ierr = nf90_inq_dimid(ncid, "west_east", dimid_ix)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_inq_dimid for 'west_east'")

    ierr = nf90_inq_dimid(ncid, "south_north", dimid_jx)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_inq_dimid for 'south_north'")

    ierr = nf90_inq_dimid(ncid, "Time", dimid_times)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_inq_dimid for 'Time'")

    ierr = nf90_redef(ncid)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_redef")

    call make_var_att_2d(ncid, dimid_ix, dimid_jx, dimid_times, NF90_FLOAT, name, trim(local_description), trim(local_units))

    ierr = nf90_enddef(ncid)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_enddef")

    call put_var_2d(ncid, 1, vegtyp_remember, iswater, ixpar, jxpar, xstartpar_remember, name, array)

    ierr = nf90_close(ncid)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_close")

  end subroutine add_to_restart_2d_float

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------

  subroutine add_to_restart_2d_integer(array, name, units, description)
    implicit none
    integer,         dimension(:,:),                              intent(in) :: array
    character(len=*),                                             intent(in) :: name
    character(len=*), optional,                                   intent(in) :: units
    character(len=*), optional,                                   intent(in) :: description

    character(len=256) :: output_flnm
    integer :: ncid
    integer :: ierr
    integer :: dimid_ix
    integer :: dimid_jx
    integer :: dimid_times
    integer :: ixout
    integer :: xstartout
    integer :: iswater
    character(len=256) :: local_units
    character(len=256) :: local_description

    integer :: ixpar
    integer :: jxpar

    output_flnm = restart_filename_remember
    iswater     = iswater_remember

    ixpar = size(array,1)
    jxpar = size(array,2)

    if (present(units)) then
       local_units = units
    else
       local_units = "-"
    endif

    if (present(description)) then
       local_description = description
    else
       local_description = "-"
    endif
    
#ifdef _PARALLEL_
    ierr = nf90_open_par(trim(output_flnm), NF90_WRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
    ierr = nf90_open(trim(output_flnm), NF90_WRITE, ncid)
#endif
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_open")

    ierr = nf90_inq_dimid(ncid, "west_east", dimid_ix)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_inq_dimid for 'west_east'")

    ierr = nf90_inq_dimid(ncid, "south_north", dimid_jx)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_inq_dimid for 'south_north'")

    ierr = nf90_inq_dimid(ncid, "Time", dimid_times)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_inq_dimid for 'Time'")

    ierr = nf90_redef(ncid)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_redef")

    call make_var_att_2d(ncid, dimid_ix, dimid_jx, dimid_times, NF90_INT, name, trim(local_description), trim(local_units))

    ierr = nf90_enddef(ncid)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_enddef")

    call put_var_int(ncid, 1, vegtyp_remember, iswater, ixpar, jxpar, xstartpar_remember, name, array)

    ierr = nf90_close(ncid)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_close")

  end subroutine add_to_restart_2d_integer

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------

  subroutine add_to_restart_3d(array, name, units, description)
    implicit none
    real,            dimension(:,:,:),                            intent(in) :: array
    character(len=*),                                             intent(in) :: name
    character(len=*), optional,                                   intent(in) :: units
    character(len=*), optional,                                   intent(in) :: description

    character(len=256) :: output_flnm
    integer :: ncid
    integer :: ierr
    integer :: dimid_ix
    integer :: dimid_jx
    integer :: dimid_kx
    integer :: dimid_times
    integer :: ixout
    integer :: xstartout
    integer :: iswater
    character(len=256) :: local_units
    character(len=256) :: local_description

    integer :: ixpar
    integer :: jxpar
    integer :: kxpar

    output_flnm = restart_filename_remember
    iswater     = iswater_remember

    ixpar = size(array,1)
    jxpar = size(array,2)
    kxpar = size(array,3)

    if (present(units)) then
       local_units = units
    else
       local_units = "-"
    endif

    if (present(description)) then
       local_description = description
    else
       local_description = "-"
    endif
    
#ifdef _PARALLEL_
    ierr = nf90_open_par(trim(output_flnm), NF90_WRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
    ierr = nf90_open(trim(output_flnm), NF90_WRITE, ncid)
#endif
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_open")

    ierr = nf90_inq_dimid(ncid, "west_east", dimid_ix)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_inq_dimid for 'west_east'")

    ierr = nf90_inq_dimid(ncid, "south_north", dimid_jx)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_inq_dimid for 'south_north'")

    ierr = nf90_inq_dimid(ncid, "soil_layers_stag", dimid_kx)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_inq_dimid for 'soil_layers_stag'")

    ierr = nf90_inq_dimid(ncid, "Time", dimid_times)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_inq_dimid for 'Time'")

    ierr = nf90_redef(ncid)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_redef")

    call make_var_att_3d(ncid, dimid_ix, dimid_jx, dimid_times, NF90_FLOAT, dimid_kx, name, trim(local_description), trim(local_units))

    ierr = nf90_enddef(ncid)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_enddef")

    call put_var_3d(ncid, 1, vegtyp_remember, iswater, ixpar, jxpar, xstartpar_remember, kxpar, name, array)

    ierr = nf90_close(ncid)
    call error_handler(ierr, "ADD_TO_RESTART:  nf90_close")

  end subroutine add_to_restart_3d

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------

  subroutine read_restart(restart_flnm, ucmcall, iz0tlnd, sfcdif_option,   &
       parallel_xstart, parallel_xend, subwindow_xstart, ix, jx, nsoil,    &
       olddate)

    ! The restart file is dimensioned by our (possibly subwindowed) grid.  Our indices
    ! for the parallel I/O reflect the dimensions of the (possibly subwindowed) grid, 
    ! but not the full domain for which LDAS input files may be available.

    implicit none

    character(len=*),             intent(in)  :: restart_flnm
    integer,                      intent(in)  :: ucmcall
    integer,                      intent(in)  :: iz0tlnd
    integer,                      intent(in)  :: sfcdif_option
    integer,                      intent(in)  :: parallel_xstart
    integer,                      intent(in)  :: parallel_xend
    integer,                      intent(in)  :: subwindow_xstart
    integer,                      intent(in)  :: ix
    integer,                      intent(in)  :: jx
    integer,                      intent(in)  :: nsoil
    character(len=19),            intent(out) :: olddate

    integer :: ierr
    integer :: ncid
    integer :: varid
    character(len=256) :: titlestr
    integer :: restart_version
    integer :: idx
    integer, dimension(4) :: nstart
    integer, dimension(4) :: ncount
    integer :: rank
    integer :: read_iz0tlnd
    integer :: read_sfcdif
    integer :: read_ucmcall

    restart_filename_remember = restart_flnm

#ifdef _PARALLEL_
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    if (ierr /= MPI_SUCCESS) stop "MPI_COMM_RANK"

    ierr = nf90_open_par(trim(restart_flnm), NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
    rank = 0
    ierr = nf90_open(trim(restart_flnm), NF90_NOWRITE, ncid)
#endif

    if (ierr == NF90_ENOTNC) then
       print*, "IERR = NF90_ENOTNC"

    else
       if (ierr /= NF90_NOERR) then
          write(*,*)
          write(*,'(" ***** Restart problem ***************************************")')
          write(*,'(" ***** ")')
          write(*,'(" *****        There was a problem in accessing the file ''", A, "''")') trim(restart_flnm)
          write(*,'(" ***** ")')
       endif
       call error_handler(ierr, " trying to open restart file "//restart_flnm)

       ierr = nf90_get_att(ncid, NF90_GLOBAL, "TITLE", titlestr)
       if (ierr /= 0) then
          write(*,'("WARNING:  RESTART file does not have TITLE attribute.")')
          write(*,'("          This probably means that LDASIN files are from an older release,")')
          write(*,'("          And are very likely incompatible with the current code.")')
          write(*,'("          I assume you know what you are doing.")')
          restart_version = 0
       else
          if (rank == 0) write(*,'("RESTART TITLE attribute: ", A)') trim(titlestr)
          ! Pull out the version number, assuming that the version is identified by vYYYYMMDD, and 
          ! based on a search for the string "v20".
          idx = index(trim(titlestr), "v20")
          if (idx <= 0) then
             write(*,'("FATAL:  RESTART file has a perverse version identifier")')
             !  write(*,'("          I assume you know what you are doing.")')
             stop
          else
             read(titlestr(idx+1:), '(I8)', iostat=ierr) restart_version
             if (ierr /= 0) then
                write(*,'("FATAL:  RESTART file has a perverse version identifier")')
                !  write(*,'("          I assume you know what you are doing.")')
                stop
             endif
          endif
       endif
       if (rank == 0) write(*, '("ldasin_version = ", I8)') restart_version

       ierr = nf90_get_att(ncid, NF90_GLOBAL, "IZ0TLND", read_iz0tlnd)
       call error_handler(ierr, "Problem getting IZ0TLND as attribute of restart file")

       if (read_iz0tlnd /= iz0tlnd) then
          if (rank == 0) then
             write(*,'(/)')
             write(*,'(" ***** Restart problem ***************************************")')
             write(*,'(" ***** Do not change namelist option IZ0TLND on restart.")')
             write(*,'(" *****     IZ0TLND in restart  file is ", I1)') read_iz0tlnd
             write(*,'(" *****     IZ0TLND in namelist file is ", I1)') iz0tlnd
             write(*,'(" ***** Correct the namelist file and resubmit.")')
             write(*,'(/)')
          endif
#ifdef _PARALLEL_
          call mpi_finalize(ierr)
#endif
          stop
       endif

       ierr = nf90_get_att(ncid, NF90_GLOBAL, "SFCDIF_OPTION", read_sfcdif)
       call error_handler(ierr, "Problem getting SFCDIF_OPTION as attribute of restart file")
       if (read_sfcdif /= sfcdif_option) then
          if (rank == 0) then
             write(*,'(/)')
             write(*,'(" ***** Restart problem ***************************************")')
             write(*,'(" ***** Do not change namelist option SFCDIF_OPTION on restart.")')
             write(*,'(" *****     SFCDIF_OPTION in restart  file is ", I1)') read_sfcdif
             write(*,'(" *****     SFCDIF_OPTION in namelist file is ", I1)') sfcdif_option
             write(*,'(" ***** Correct the namelist file and resubmit.")')
             write(*,'(/)')
          endif
#ifdef _PARALLEL_
          call mpi_finalize(ierr)
#endif
          stop
       endif

#ifdef _HRLDAS_URBAN_
       
       ierr = nf90_get_att(ncid, NF90_GLOBAL, "UCMCALL", read_ucmcall)
       call error_handler(ierr, "Problem getting UCMCALL as attribute of restart file")
       if (read_ucmcall /= ucmcall) then
          if (rank == 0) then
             write(*,'(/)')
             write(*,'(" ***** Restart problem ***************************************")')
             write(*,'(" ***** Do not change namelist option UCMCALL on restart.")')
             write(*,'(" *****     UCMCALL in restart  file is ", I1)') read_ucmcall
             write(*,'(" *****     UCMCALL in namelist file is ", I1)') ucmcall
             write(*,'(" ***** Correct the namelist file and resubmit.")')
             write(*,'(/)')
          endif
#ifdef _PARALLEL_
          call mpi_finalize(ierr)
#endif
          stop
       endif

#endif

! Older RESTART files are incompatible if you use the Urban Canopy Model.
#if defined _HRLDAS_URBAN_
       if (ucmcall == 1) then
          if (restart_version < 20070712) then
             write(*,'(/,"  *****************************************************************************  ")')
             write(*,'("  *****  You are using a restart file created prior to version v20070712.")')
             write(*,'("  *****  That version of restart files has problems with Urban Canopy Model.")')
             write(*,'("  *****  Update your code and use newer restart files.")')
             write(*,'("  *****************************************************************************  ",/)')
             stop
          endif
       endif
#endif

       ! Get the time stamp from the restart file.
       ierr = nf90_inq_varid(ncid, "Times", varid)
       call error_handler(ierr, "Problem finding variable in restart file: 'Times'")
       
       ierr = nf90_get_var(ncid, varid, olddate)
       call error_handler(ierr, "Problem finding variable in restart file: 'Times'")

       ierr = nf90_close(ncid)
       call error_handler(ierr, "Problem closing restart file")
    endif

  end subroutine read_restart

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------

  subroutine get_from_restart_2d_float(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, name, array, return_error)
    implicit none
    integer,                            intent(in) :: parallel_xstart
    integer,                            intent(in) :: parallel_xend
    integer,                            intent(in) :: subwindow_xstart
    integer,                            intent(in) :: ixfull
    integer,                            intent(in) :: jxfull
    character(len=*),                   intent(in)  :: name
    real,             dimension(parallel_xstart:parallel_xend,jxfull), intent(out) :: array
    integer,          optional,         intent(out) :: return_error

    integer :: ierr
    integer :: ncid
    integer :: varid
    integer, dimension(4) :: nstart
    integer, dimension(4) :: ncount
    integer :: rank

#ifdef _PARALLEL_

    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    if (ierr /= MPI_SUCCESS) stop "MPI_COMM_RANK"

    ierr = nf90_open_par(trim(restart_filename_remember), NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)

#else
    rank = 0

    ierr = nf90_open(trim(restart_filename_remember), NF90_NOWRITE, ncid)

#endif
    call error_handler(ierr, "GET_FROM_RESTART: Problem opening restart file '"//trim(restart_filename_remember)//"'")

    nstart = (/ parallel_xstart-subwindow_xstart+1, 1,  1, -99999 /)
    ncount = (/ parallel_xend-parallel_xstart+1,   jxfull,  1, -99999 /)

    if (present(return_error)) then
       ierr = nf90_inq_varid(ncid, name, varid)
       if (ierr == NF90_NOERR) then
          return_error = 0
          call error_handler(ierr, "Problem finding variable in restart file '"//trim(name)//"'")

          ierr = nf90_get_var(ncid, varid, array, start=nstart(1:3))
          call error_handler(ierr, "Problem finding variable in restart file: '"//trim(name)//"'")
       else
          return_error = 1
          if (rank == 0) write(*,'("Did not find optional variable ''",A,"'' in restart file ''", A, "''")') trim(name), trim(restart_filename_remember)
       endif
    else
       ierr = nf90_inq_varid(ncid, name, varid)
       call error_handler(ierr, "Problem finding required variable in restart file: '"//trim(name)//"'")

       ierr = nf90_get_var(ncid, varid, array, start=nstart(1:3))
       call error_handler(ierr, "Problem finding variable in restart file: '"//trim(name)//"'")
    endif

    ierr = nf90_close(ncid)
    call error_handler(ierr, "Problem closing restart file")
    
  end subroutine get_from_restart_2d_float

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------

  subroutine get_from_restart_2d_integer(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, name, array, return_error)
    implicit none
    integer,                                                           intent(in) :: parallel_xstart
    integer,                                                           intent(in) :: parallel_xend
    integer,                                                           intent(in) :: subwindow_xstart
    integer,                                                           intent(in) :: ixfull
    integer,                                                           intent(in) :: jxfull
    character(len=*),                                                  intent(in)  :: name
    integer,          dimension(parallel_xstart:parallel_xend,jxfull), intent(out) :: array
    integer,          optional,                                        intent(out) :: return_error

    integer :: ierr
    integer :: ncid
    integer :: varid
    integer, dimension(4) :: nstart
    integer, dimension(4) :: ncount

#ifdef _PARALLEL_
    ierr = nf90_open_par(trim(restart_filename_remember), NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
    ierr = nf90_open(trim(restart_filename_remember), NF90_NOWRITE, ncid)
#endif
    call error_handler(ierr, "GET_FROM_RESTART: Problem opening restart file '"//trim(restart_filename_remember)//"'")

    nstart = (/ parallel_xstart-subwindow_xstart+1, 1,  1, -99999 /)
    ncount = (/ parallel_xend-parallel_xstart+1,   jxfull,  1, -99999 /)

    if (present(return_error)) then
       ierr = nf90_inq_varid(ncid, name, varid)
       if (ierr == NF90_NOERR) then
          return_error = 0
          call error_handler(ierr, "Problem finding variable in restart file '"//trim(name)//"'")

          ierr = nf90_get_var(ncid, varid, array, start=nstart(1:3))
          call error_handler(ierr, "Problem finding variable in restart file: '"//trim(name)//"'")
       else
          return_error = 1
          write(*,'("Did not find optional variable ''",A,"'' in restart file ''", A, "''")') trim(name), trim(restart_filename_remember)
       endif
    else
       ierr = nf90_inq_varid(ncid, name, varid)
       call error_handler(ierr, "Problem finding required variable in restart file: '"//trim(name)//"'")

       ierr = nf90_get_var(ncid, varid, array, start=nstart(1:3))
       call error_handler(ierr, "Problem finding variable in restart file: '"//trim(name)//"'")
    endif

    ierr = nf90_close(ncid)
    call error_handler(ierr, "Problem closing restart file")
    
  end subroutine get_from_restart_2d_integer

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------

  subroutine get_from_restart_3d(parallel_xstart, parallel_xend, subwindow_xstart, ixfull, jxfull, name, array, return_error)
    implicit none
    integer,                            intent(in) :: parallel_xstart
    integer,                            intent(in) :: parallel_xend
    integer,                            intent(in) :: subwindow_xstart
    integer,                            intent(in) :: ixfull
    integer,                            intent(in) :: jxfull
    character(len=*),                   intent(in)  :: name
    real,             dimension(:,:,:), intent(out) :: array
    integer,          optional,         intent(out) :: return_error

    integer :: ierr
    integer :: ncid
    integer :: varid
    integer, dimension(4) :: nstart
    integer, dimension(4) :: ncount

#ifdef _PARALLEL_
    ierr = nf90_open_par(trim(restart_filename_remember), NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
    ierr = nf90_open(trim(restart_filename_remember), NF90_NOWRITE, ncid)
#endif
    call error_handler(ierr, "GET_FROM_RESTART: Problem opening restart file '"//trim(restart_filename_remember)//"'")

    nstart = (/parallel_xstart-subwindow_xstart+1,1, 1, 1/)
    ncount = (/parallel_xend-parallel_xstart+1, size(array,2), size(array,3), 1/)

    if (present(return_error)) then
       ierr = nf90_inq_varid(ncid, name, varid)
       if (ierr == NF90_NOERR) then
          return_error = 0
          call error_handler(ierr, "Problem finding variable in restart file '"//trim(name)//"'")

          ierr = nf90_get_var(ncid, varid, array, start=nstart(1:4))
          call error_handler(ierr, "Problem finding variable in restart file: '"//trim(name)//"'")
       else
          return_error = 1
          write(*,'("Did not find optional variable ''",A,"'' in restart file ''", A, "''")') trim(name), trim(restart_filename_remember)
       endif
    else
       ierr = nf90_inq_varid(ncid, name, varid)
       call error_handler(ierr, "Problem finding required variable in restart file: '"//trim(name)//"'")

       ierr = nf90_get_var(ncid, varid, array, start=nstart(1:4))
       call error_handler(ierr, "Problem finding variable in restart file: '"//trim(name)//"'")
    endif

    ierr = nf90_close(ncid)
    call error_handler(ierr, "Problem closing restart file")
    
  end subroutine get_from_restart_3d

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------

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
       write(*,'(/,A)') nf90_strerror(status)
       if (present(failure)) then
          write(*,'(/," ***** ", A,/)') failure
       endif
       stop 'Stopped'
    endif

    if (present(success)) then
       write(*,'(A)') success
    endif

  end subroutine error_handler

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

  subroutine read_additional(flnm_template, hdate, name, xstart, xend, ystart, yend, array, ierr)
    use kwm_string_utilities
    implicit none
    character(len=*),                         intent(in)  :: flnm_template
    character(len=*),                         intent(in)  :: hdate
    character(len=*),                         intent(in)  :: name
    integer,                                  intent(in)  :: xstart
    integer,                                  intent(in)  :: xend
    integer,                                  intent(in)  :: ystart
    integer,                                  intent(in)  :: yend
    real, dimension(xstart:xend,ystart:yend), intent(out) :: array
    integer,                                  intent(out) :: ierr

    character(len=256) :: flnm
    integer :: jday
    character(len=3) :: hjday
    integer :: ncid
    integer :: varid
    logical :: lexist

    call geth_idts(hdate(1:10), hdate(1:4)//"-01-01", jday)
    jday = jday + 1
    write(hjday,'(I3.3)') jday

    flnm = flnm_template

    call strrep(flnm, "<YYYY>", hdate(1:4))
    call strrep(flnm, "<MM>", hdate(6:7))
    call strrep(flnm, "<DD>", hdate(9:10))
    call strrep(flnm, "<HH>", hdate(12:13))
    call strrep(flnm, "<JDAY>", hjday)

    inquire(file=trim(flnm), exist=lexist)
    if (.not. lexist) then
       ierr = 1
       return
    endif

    write(*, '("Additional flnm = ''",A,"''")') trim(flnm)

#ifdef _PARALLEL_
    ierr = nf90_open_par(trim(flnm), NF90_NOWRITE, MPI_COMM_WORLD, MPI_INFO_NULL, ncid)
#else
    ierr = nf90_open(trim(flnm), NF90_NOWRITE, ncid)
#endif
    call error_handler(ierr, failure="READ_ADDITIONAL: Problem opening additional file: "//trim(flnm))

    ierr = nf90_inq_varid(ncid,  name,  varid)
    call error_handler(ierr, failure="READ_ADDITIONAL: Problem finding variable: "//name)

    ierr = nf90_get_var(ncid, varid, array, start=(/xstart,ystart/), count=(/xend-xstart+1,yend-ystart+1/))
    call error_handler(ierr, failure="READ_ADDITIONAL: Problem getting variable: "//name)

    ierr = nf90_close(ncid)
    call error_handler(ierr, failure="READ_ADDITIONAL:  Problem closing file:  "//trim(flnm))

  end subroutine read_additional

!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

 end module module_hrldas_netcdf_io
