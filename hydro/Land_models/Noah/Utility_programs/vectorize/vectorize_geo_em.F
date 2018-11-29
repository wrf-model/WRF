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

program vectorize_geo_em
  use netcdf
  implicit none

  integer :: ncid_in, ncid_out
  integer :: iret,iloc,ilat,ilon
  integer :: dim_date, dim_we, dim_sn, dim_time, dim_mo, ndims, nvars, natts
  integer :: varid_in(4), varid_out(4)
  character (len = 80) :: attname
  character(len=256) :: input_file
  character(len=256) :: output_file

  real,    allocatable, dimension(:,:,:)   :: lu_index
  real,    allocatable, dimension(:,:,:)   :: xlat_in
  real,    allocatable, dimension(:,:,:)   :: xlon_in
  real,    allocatable, dimension(:,:,:,:) :: green_in
  real,    allocatable, dimension(:,:,:)   :: xlat_out
  real,    allocatable, dimension(:,:,:)   :: xlon_out
  real,    allocatable, dimension(:,:,:,:) :: green_out
  logical, allocatable, dimension(:,:)     :: lumask

  integer :: dimid
  integer :: west_east_old
  integer :: south_north_old
  integer :: west_east_new
  integer :: south_north_new = 1

  integer, dimension(100) :: keep_lu_list
  character(len=256) :: lustring
  integer :: landuse_count
  integer :: k

  character(len=256) :: full_geo_em
  character(len=256) :: vector_directory
  character(len=256) :: dumstr

  namelist/vectorize_namelist/ keep_lu_list, full_geo_em, vector_directory
  
  ! Default values
  keep_lu_list = -99999
  landuse_count = 0
  full_geo_em = " "
  vector_directory = " "
  open(12, file="namelist.vectorize", status='old', form='formatted', action='read')
  read(12, vectorize_namelist)
  close(12)
  do while (keep_lu_list(landuse_count+1) > 0)
     landuse_count = landuse_count + 1
  enddo
  write(*,'("Keep the following ", I4, " land-use categories:")') landuse_count
  write(*,'("         ", I4)') keep_lu_list(1:landuse_count)

!!!!!
! OPEN OLD GEO_EM FILE AND CREATE NEW VECTOR FILE
!!!!!

  iret = nf90_open(trim(full_geo_em), NF90_NOWRITE, ncid_in)
  call error_handler(iret, "Problem opening file '"//trim(input_file)//"'")

  iret = nf90_create(trim(vector_directory)//trim(full_geo_em(index(full_geo_em,"/",.TRUE.):)), NF90_CLOBBER, ncid_out)
  call error_handler(iret, "Problem creating file '"//trim(output_file)//"'")

!!!!!
! DIMENSIONS FROM OLD FILE
!!!!!

  call get_dimlen ( ncid_in, "west_east", west_east_old )
  call get_dimlen ( ncid_in, "south_north", south_north_old )

  allocate ( lu_index ( west_east_old , south_north_old , 1 ) )
  allocate ( xlat_in  ( west_east_old , south_north_old , 1 ) )
  allocate ( xlon_in  ( west_east_old , south_north_old , 1 ) )
  allocate ( green_in ( west_east_old , south_north_old , 12, 1 ) )

!!!!!
! READ IN VARIABLES ONE AT A TIME, EXTRACT AND WRITE TO NEW FILE
!!!!!

!!!!!
! LU_INDEX - only used to extract desired points
!!!!!

  iret = nf90_inq_varid(ncid_in,"LU_INDEX",varid_in(1))
  call error_handler(iret, "Problem getting varid for 'LU_INDEX'")

  iret = nf90_get_var(ncid_in, varid_in(1), lu_index)
  call error_handler(iret, "Problem getting variable 'LU_INDEX'")
  
!!!!!
! XLAT
!!!!!

  iret = nf90_inq_varid(ncid_in,"XLAT_M",varid_in(2))
  call error_handler(iret, "Problem getting varid for 'XLAT_M'")

  iret = nf90_get_var(ncid_in, varid_in(2), xlat_in)
  call error_handler(iret, "Problem getting variable 'XLAT_M'")
  
!!!!!
! XLONG
!!!!!

  iret = nf90_inq_varid(ncid_in,"XLONG_M",varid_in(3))
  call error_handler(iret, "Problem getting varid for 'XLONG_M'")

  iret = nf90_get_var(ncid_in, varid_in(3), xlon_in)
  call error_handler(iret, "Problem getting variable 'XLONG_M'")
  
!!!!!
! GREENFRAC
!!!!!

  iret = nf90_inq_varid(ncid_in,"GREENFRAC",varid_in(4))
  call error_handler(iret, "Problem getting varid for 'GREENFRAC'")

  iret = nf90_get_var(ncid_in, varid_in(4), green_in)
  call error_handler(iret, "Problem getting variable 'GREENFRAC'")

  allocate(lumask(west_east_old, south_north_old))
  lumask = .FALSE.
  do k = 1, landuse_count
     where (nint(lu_index(:,:,1)) == keep_lu_list(k)) lumask = .TRUE.
  enddo
  west_east_new = count(lumask)
  write(*, '("Count of points to keep = ", I20)') west_east_new

  allocate ( xlat_out  ( west_east_new , south_north_new , 1 ) )
  allocate ( xlon_out  ( west_east_new , south_north_new , 1 ) )
  allocate ( green_out ( west_east_new , south_north_new , 12, 1 ) )
  
  iloc = 0
  do ilat = 1,south_north_old
     do ilon = 1,west_east_old
        if(lumask(ilon,ilat)) then
           iloc = iloc + 1
           if (iloc > west_east_new) then
              print*, 'up to ', iloc
              stop "Too many!"
           endif
           ! print*, 'iloc, ilon, ilat = ', iloc, ilon, ilat
           xlat_out(iloc,1,1) = xlat_in(ilon,ilat,1)
           xlon_out(iloc,1,1) = xlon_in(ilon,ilat,1)
           green_out(iloc,1,:,1) = green_in(ilon,ilat,:,1)
        endif
     enddo
  enddo
  if (iloc /= west_east_new) stop "Problem"

!!!!!
! DIMENSIONS IN NEW FILE
!!!!!

  iret = nf90_def_dim(ncid_out,"Time",NF90_UNLIMITED,dim_time)
  call error_handler(iret, "Problem defining dimension 'Time'")

  iret = nf90_def_dim(ncid_out,"DateStrLen",19,dim_date)
  call error_handler(iret, "Problem defining dimension 'DateStrLen'")

  iret = nf90_def_dim(ncid_out,"west_east",west_east_new,dim_we)
  call error_handler(iret, "Problem defining dimension 'west_east'")

  iret = nf90_def_dim(ncid_out,"south_north",south_north_new,dim_sn)
  call error_handler(iret, "Problem defining dimension 'south_north'")

  iret = nf90_def_dim(ncid_out,"month",12,dim_MO)
  call error_handler(iret, "Problem defining dimension 'month'")

!!!!!
! TAKE CARE OF THE PESKY GLOBAL ATTRIBUTES
!!!!!

  iret = nf90_inquire(ncid_in,ndims, nvars, natts)
  call error_handler(iret, "Problem getting number attributes")
  
  do iloc = 1, natts

    iret = nf90_inq_attname(ncid_in,NF90_GLOBAL, iloc, attname)
    call error_handler(iret,"Problem getting attribute name")

    iret = nf90_copy_att(ncid_in, NF90_GLOBAL, trim(attname), ncid_out, NF90_GLOBAL)
    call error_handler(iret,"Problem copying attribute")

  end do

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"WEST-EAST_GRID_DIMENSION", west_east_new+1)
  call error_handler(iret,"Problem putting attribute 'WEST-EAST_GRID_DIMENSION'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"SOUTH-NORTH_GRID_DIMENSION", south_north_new+1)
  call error_handler(iret,"Problem putting attribute 'SOUTH-NORTH_GRID_DIMENSION'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"WEST-EAST_PATCH_END_UNSTAG", west_east_new)
  call error_handler(iret,"Problem putting attribute 'WEST-EAST_PATCH_END_UNSTAG'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"WEST-EAST_PATCH_END_STAG", west_east_new+1)
  call error_handler(iret,"Problem putting attribute 'WEST-EAST_PATCH_END_STAG'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"SOUTH-NORTH_PATCH_END_UNSTAG", south_north_new)
  call error_handler(iret,"Problem putting attribute 'SOUTH-NORTH_PATCH_END_STAG'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"SOUTH-NORTH_PATCH_END_STAG", south_north_new+1)
  call error_handler(iret,"Problem putting attribute 'SOUTH-NORTH-PATCH_END_STAG'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"i_parent_end", west_east_new+1)
  call error_handler(iret,"Problem putting attribute 'i_parent_end'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"j_parent_end", south_north_new+1)
  call error_handler(iret,"Problem putting attribute 'j_parent_end'")

  lustring = " "
  do k = 1, landuse_count
     write(dumstr, *) keep_lu_list(k)
     if (k>1) lustring = trim(lustring)//","
     lustring = trim(lustring) // trim(adjustl(dumstr))
  enddo
  iret = nf90_put_att(ncid_out, NF90_GLOBAL, "vector_landuse_indices", trim(adjustl(lustring)))
  call error_handler(iret,"Problem putting attribute 'vector_landuse_indices'")

!!!!!
! XLAT
!!!!!

  iret = nf90_def_var(ncid_out,"XLAT_M",NF90_FLOAT,(/dim_we,dim_sn,dim_time/),varid_out(1))
  call error_handler(iret, "Problem defining variable 'XLAT_M'")
    iret = nf90_put_att(ncid_out,varid_out(1),"FieldType",104)
    call error_handler(iret, "Problem making attribute 'FieldType' for variable 'XLAT_M'")
    iret = nf90_put_att(ncid_out,varid_out(1),"MemoryOrder","XY ")
    call error_handler(iret, "Problem making attribute 'MemoryOrder' for variable 'XLAT_M'")
    iret = nf90_put_att(ncid_out,varid_out(1),"units","degrees latitude")
    call error_handler(iret, "Problem making attribute 'units' for variable 'XLAT_M'")
    iret = nf90_put_att(ncid_out,varid_out(1),"description","Latitude on mass grid")
    call error_handler(iret, "Problem making attribute 'description' for variable 'XLAT_M'")
    iret = nf90_put_att(ncid_out,varid_out(1),"stagger","M")
    call error_handler(iret, "Problem making attribute 'stagger' for variable 'XLAT_M'")
    iret = nf90_put_att(ncid_out,varid_out(1),"sr_x",1)
    call error_handler(iret, "Problem making attribute 'sr_x' for variable 'XLAT_M'")
    iret = nf90_put_att(ncid_out,varid_out(1),"sr_y",1)
    call error_handler(iret, "Problem making attribute 'sr_y' for variable 'XLAT_M'")

!!!!!
! XLONG
!!!!!

  iret = nf90_def_var(ncid_out,"XLONG_M",NF90_FLOAT,(/dim_we,dim_sn,dim_time/),varid_out(2))
  call error_handler(iret, "Problem defining variable 'XLONG_M'")
    iret = nf90_put_att(ncid_out,varid_out(2),"FieldType",104)
    call error_handler(iret, "Problem defining attribute 'FieldType' for variable 'XLONG_M'")
    iret = nf90_put_att(ncid_out,varid_out(2),"MemoryOrder","XY ")
    call error_handler(iret, "Problem defining attribute 'MemoryOrder' for variable 'XLONG_M'")
    iret = nf90_put_att(ncid_out,varid_out(2),"units","degrees longitude")
    call error_handler(iret, "Problem defining attribute 'units' for variable 'XLONG_M'")
    iret = nf90_put_att(ncid_out,varid_out(2),"description","Longitude on mass grid")
    call error_handler(iret, "Problem defining attribute 'description' for variable 'XLONG_M'")
    iret = nf90_put_att(ncid_out,varid_out(2),"stagger","M")
    call error_handler(iret, "Problem defining attribute 'stagger' for variable 'XLONG_M'")
    iret = nf90_put_att(ncid_out,varid_out(2),"sr_x",1)
    call error_handler(iret, "Problem defining attribute 'sr_x' for variable 'XLONG_M'")
    iret = nf90_put_att(ncid_out,varid_out(2),"sr_y",1)
    call error_handler(iret, "Problem defining attribute 'sr_y' for variable 'XLONG_M'")

!!!!!
! GREENFRAC
!!!!!

  iret = nf90_def_var(ncid_out,"GREENFRAC",NF90_FLOAT,(/dim_we,dim_sn,dim_mo,dim_time/),varid_out(3))
  call error_handler(iret, "Problem defining variable 'GREENFRAC'")
    iret = nf90_put_att(ncid_out,varid_out(3),"FieldType",104)
    call error_handler(iret, "Problem defining attribute 'FieldType' for variable 'GREENFRAC'")
    iret = nf90_put_att(ncid_out,varid_out(3),"MemoryOrder","XYZ")
    call error_handler(iret, "Problem defining attribute 'MemoryOrder' for variable 'GREENFRAC'")
    iret = nf90_put_att(ncid_out,varid_out(3),"units","fraction")
    call error_handler(iret, "Problem defining attribute 'units' for variable 'GREENFRAC'")
    iret = nf90_put_att(ncid_out,varid_out(3),"description","Monthly green fraction")
    call error_handler(iret, "Problem defining attribute 'description' for variable 'GREENFRAC'")
    iret = nf90_put_att(ncid_out,varid_out(3),"stagger","M")
    call error_handler(iret, "Problem defining attribute 'stagger' for variable 'GREENFRAC'")
    iret = nf90_put_att(ncid_out,varid_out(3),"sr_x",1)
    call error_handler(iret, "Problem defining attribute 'sr_x' for variable 'GREENFRAC'")
    iret = nf90_put_att(ncid_out,varid_out(3),"sr_y",1)
    call error_handler(iret, "Problem defining attribute 'sr_y' for variable 'GREENFRAC'")

  iret = nf90_enddef(ncid_out)
  call error_handler(iret, "Problem exiting the define mode")


!!!!!
! WRITE VARIABLES
!!!!!

  iret = nf90_put_var(ncid_out,varid_out(1),xlat_out)
  call error_handler(iret, "Problem writing variable 'XLAT_M'")

  iret = nf90_put_var(ncid_out,varid_out(2),xlon_out)
  call error_handler(iret, "Problem writing variable 'XLONG_M'")

  iret = nf90_put_var(ncid_out,varid_out(3),green_out)
  call error_handler(iret, "Problem writing variable 'GREENFRAC'")

  iret = nf90_close(ncid_out)
  call error_handler(iret, "Problem closing output file")
  
  iret = nf90_close(ncid_in)
  call error_handler(iret, "Problem closing input file")
    
end program

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

subroutine error_handler(ierr, failure)
  use netcdf
  implicit none
  integer, intent(in) :: ierr
  character(len=*), intent(in) :: failure

  if (ierr == NF90_NOERR) return

  write(*, '("----ERROR---------------------------------------------------")')
  write(*, '(5X,A)') trim(nf90_strerror(ierr))
  if (failure /= "") write(*, '(5X,A)') trim(failure)
  write(*, '("------------------------------------------------------------")')
  stop

end subroutine error_handler

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

subroutine get_dimlen(ncid, name, dimlen)
  use netcdf
  implicit none
  integer, intent(in) :: ncid
  character(len=*), intent(in) :: name
  integer, intent(out) :: dimlen

  integer :: dimid
  integer :: ierr

  ierr = nf90_inq_dimid(ncid, name, dimid)
  call error_handler(ierr, "GET_DIMLEN : Problem getting dimension id for '"//name//"'")

  ierr = nf90_inquire_dimension(ncid, dimid, len=dimlen)
  call error_handler(ierr, "GET_DIMLEN : Problem getting dimension length for '"//name//"'")

end subroutine get_dimlen

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
