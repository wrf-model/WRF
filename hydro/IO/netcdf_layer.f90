module netcdf_layer_base
  use netcdf
  use mpi
  implicit none

  type, abstract :: NetCDF_layer_
     procedure (nf90_open), pointer, nopass :: open_file    ! => nf90_open
     procedure (nf90_def_dim), pointer, nopass :: def_dim !=> nf90_def_dim
     procedure (nf90_inq_varid), pointer, nopass :: inq_varid !=> nf90_inq_varid
     procedure (nf90_close), pointer, nopass :: close_file !=> nf90_close

     procedure (integer), pointer, nopass :: put_var !=> nf_put_var
     procedure (integer), pointer, nopass :: get_var !=> nf_get_var
     procedure (integer), pointer, nopass :: put_att !=> nf_put_att
     procedure (integer), pointer, nopass :: def_var !=> nf_def_var
   contains
     procedure (create_file_signature), pass(object), deferred :: create_file
  end type NetCDF_layer_

  integer, external :: nf_put_att
  integer, external :: nf_def_var
  integer, external :: nf_put_var
  integer, external :: nf_get_var

  abstract interface

     function create_file_signature(object, path, cmode, initialsize, chunksize, ncid) result(res)
       import NetCDF_layer_
       class(NetCDF_layer_), intent(in   ) :: object
       character (len = *), intent(in   ) :: path
       integer,             intent(in   ) :: cmode
       integer, optional,   intent(in   ) :: initialsize
       integer, optional,   intent(inout) :: chunksize
       integer,             intent(  out) :: ncid
       integer                            :: res
     end function create_file_signature

  end interface

  type, extends(NetCDF_layer_) :: NetCDF_serial_
   contains
     procedure, pass(object) :: create_file => create_file_serial
  end type NetCDF_serial_

  type, extends(NetCDF_layer_) :: NetCDF_parallel_
     integer :: MPI_communicator
     integer :: default_info = MPI_INFO_NULL
   contains
     procedure, pass(object) :: create_file => create_file_parallel
  end type NetCDF_parallel_

contains

  function create_file_serial (object, path, cmode, initialsize, chunksize, ncid) result(res)
    class(NetCDF_serial_),  intent(in) :: object
    character (len = *), intent(in   ) :: path
    integer,             intent(in   ) :: cmode
    integer, optional,   intent(in   ) :: initialsize
    integer, optional,   intent(inout) :: chunksize
    integer,             intent(  out) :: ncid
    integer                            :: res

    res = nf90_create(path = path, cmode = cmode, ncid = ncid)

  end function create_file_serial

  function create_file_parallel(object, path, cmode, initialsize, chunksize, ncid) result(res)
    class(NetCDF_parallel_),intent(in) :: object
    character (len = *), intent(in   ) :: path
    integer,             intent(in   ) :: cmode
    integer, optional,   intent(in   ) :: initialsize
    integer, optional,   intent(inout) :: chunksize
    integer,             intent(  out) :: ncid
    integer                            :: res

    res = nf90_create(path = path, cmode = cmode, ncid = ncid, &
         &  comm = object%mpi_communicator, info = object%default_info)

  end function create_file_parallel

end module netcdf_layer_base
