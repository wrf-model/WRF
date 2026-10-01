module mo_simple_netcdf
  use mo_rte_kind, only: wp, wl
  use netcdf
  implicit none
  private

  interface read_field
    module procedure read_scalar, read_1d_field, read_2d_field, read_3d_field, read_4d_field
  end interface
  interface write_field
    module procedure write_1d_int_field, write_2d_int_field, &
                     write_1d_field, write_2d_field, write_3d_field, write_4d_field
  end interface

  public :: dim_exists, get_dim_size, create_dim, &
            var_exists, get_var_size, create_var, &
            read_field, read_string, read_char_vec, read_logical_vec, write_field
contains
  !--------------------------------------------------------------------------------------------------------------------
  function read_scalar(ncid, varName)
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    real(wp)                     :: read_scalar

    integer :: varid

    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))
    if(nf90_get_var(ncid, varid, read_scalar)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end function read_scalar
  !--------------------------------------------------------------------------------------------------------------------
  function read_1d_field(ncid, varName, nx)
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer,          intent(in) :: nx
    real(wp), dimension(nx)      :: read_1d_field

    integer :: varid

    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))
    if(nf90_get_var(ncid, varid, read_1d_field)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end function read_1d_field
  !--------------------------------------------------------------------------------------------------------------------
  function read_2d_field(ncid, varName, nx, ny)
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer,          intent(in) :: nx, ny
    real(wp), dimension(nx, ny)  :: read_2d_field

    integer :: varid

    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))
    if(nf90_get_var(ncid, varid, read_2d_field)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end function read_2d_field
  !--------------------------------------------------------------------------------------------------------------------
  function read_3d_field(ncid, varName, nx, ny, nz)
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer,          intent(in) :: nx, ny, nz
    real(wp), dimension(nx, ny, nz)  :: read_3d_field

    integer :: varid

    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))
    if(nf90_get_var(ncid, varid, read_3d_field)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end function read_3d_field
  !--------------------------------------------------------------------------------------------------------------------
  function read_4d_field(ncid, varName, nw, nx, ny, nz)
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer,          intent(in) :: nw, nx, ny, nz
    real(wp), dimension(nw, nx, ny, nz)  :: read_4d_field

    integer :: varid

    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))
    if(nf90_get_var(ncid, varid, read_4d_field)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end function read_4d_field
  !--------------------------------------------------------------------------------------------------------------------
  function read_string(ncid, varName, nc)
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer,          intent(in) :: nc
    character(len=nc)            :: read_string

    integer :: varid

    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) then
      read_string = ""
      return
    end if
    if(nf90_get_var(ncid, varid, read_string)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))
  end function read_string
  !--------------------------------------------------------------------------------------------------------------------
  ! Writing functions
  !--------------------------------------------------------------------------------------------------------------------
  function write_1d_int_field(ncid, varName, var) result(err_msg)
    integer,                intent(in) :: ncid
    character(len=*),       intent(in) :: varName
    integer, dimension(:),  intent(in) :: var
    character(len=128)                 :: err_msg

    integer :: varid

    err_msg = ""
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) then
      err_msg = "write_field: can't find variable " // trim(varName)
      return
    end if
    if(nf90_put_var(ncid, varid, var)  /= NF90_NOERR) &
      err_msg = "write_field: can't write variable " // trim(varName)

  end function write_1d_int_field
  !--------------------------------------------------------------------------------------------------------------------
  function write_2d_int_field(ncid, varName, var) result(err_msg)
    integer,                  intent(in) :: ncid
    character(len=*),         intent(in) :: varName
    integer, dimension(:,:),  intent(in) :: var
    character(len=128)                   :: err_msg

    integer :: varid

    err_msg = ""
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) then
      err_msg = "write_field: can't find variable " // trim(varName)
      return
    end if
    if(nf90_put_var(ncid, varid, var)  /= NF90_NOERR) &
      err_msg = "write_field: can't write variable " // trim(varName)

  end function write_2d_int_field
  !--------------------------------------------------------------------------------------------------------------------
  function write_1d_field(ncid, varName, var) result(err_msg)
    integer,                intent(in) :: ncid
    character(len=*),       intent(in) :: varName
    real(wp), dimension(:), intent(in) :: var
    character(len=128)                 :: err_msg

    integer :: varid

    err_msg = ""
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) then
      err_msg = "write_field: can't find variable " // trim(varName)
      return
    end if
    if(nf90_put_var(ncid, varid, var)  /= NF90_NOERR) &
      err_msg = "write_field: can't write variable " // trim(varName)

  end function write_1d_field
  !--------------------------------------------------------------------------------------------------------------------
  function write_2d_field(ncid, varName, var) result(err_msg)
    integer,                  intent(in) :: ncid
    character(len=*),         intent(in) :: varName
    real(wp), dimension(:,:), intent(in) :: var
    character(len=128)                   :: err_msg

    integer :: varid
    integer :: stat

    err_msg = ""
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) then
      err_msg = "write_field: can't find variable " // trim(varName)
      return
    end if
    stat = nf90_put_var(ncid, varid, var)
    if(stat /= NF90_NOERR) &
      err_msg = "write_field: can't write variable " // trim(varName) // &
      " netcdf err: " // nf90_strerror(stat)

  end function write_2d_field
  !--------------------------------------------------------------------------------------------------------------------
  function write_3d_field(ncid, varName, var) result(err_msg)
    integer,                    intent(in) :: ncid
    character(len=*),           intent(in) :: varName
    real(wp), dimension(:,:,:), intent(in) :: var
    character(len=128)                     :: err_msg

    integer :: varid
    integer :: stat

    err_msg = ""
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) then
      err_msg = "write_field: can't find variable " // trim(varName)
      return
    end if
    stat = nf90_put_var(ncid, varid, var)
    if(stat /= NF90_NOERR) &
      err_msg = "write_field: can't write variable " // trim(varName) // &
      " netcdf err: " // nf90_strerror(stat)

  end function write_3d_field
  !--------------------------------------------------------------------------------------------------------------------
  function write_4d_field(ncid, varName, var) result(err_msg)
    integer,                    intent(in) :: ncid
    character(len=*),           intent(in) :: varName
    real(wp), dimension(:,:,:,:), intent(in) :: var
    character(len=128)                     :: err_msg

    integer :: varid
    integer :: stat

    err_msg = ""
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) then
      err_msg = "write_field: can't find variable " // trim(varName)
      return
    end if
    stat = nf90_put_var(ncid, varid, var)
    if(stat /= NF90_NOERR) &
      err_msg = "write_field: can't write variable " // trim(varName) // &
      " netcdf err: " // nf90_strerror(stat)

  end function write_4d_field
  !--------------------------------------------------------------------------------------------------------------------
  function read_logical_vec(ncid, varName, nx)
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer,          intent(in) :: nx
    integer,      dimension(nx) :: read_logical_tmp
    logical(wl),  dimension(nx) :: read_logical_vec

    integer :: varid
    integer :: ix

    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_logical_vec: can't find variable " // trim(varName))
    if(nf90_get_var(ncid, varid, read_logical_tmp)  /= NF90_NOERR) &
      call stop_on_err("read_logical_vec: can't read variable " // trim(varName))
    do ix = 1, nx
      if (read_logical_tmp(ix) .eq. 0) then
        read_logical_vec(ix) = .false.
      else
        read_logical_vec(ix) = .true.
      endif
    enddo

  end function read_logical_vec
  !--------------------------------------------------------------------------------------------------------------------
  function read_char_vec(ncid, varName, nx)
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer,          intent(in) :: nx
    character(len=32), dimension(nx) :: read_char_vec

    integer :: varid

    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_char_vec: can't find variable " // trim(varName))
    if(nf90_get_var(ncid, varid, read_char_vec)  /= NF90_NOERR) &
      call stop_on_err("read_char_vec: can't read variable " // trim(varName))

  end function read_char_vec
  !--------------------------------------------------------------------------------------------------------------------
  function dim_exists(ncid, dimName)
    !
    ! Does this dimension exist (have a valid dim_id) in the open netCDF file?
    !
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: dimName
    logical                      :: dim_exists

    integer :: dimid
    dim_exists = nf90_inq_dimid(ncid, trim(dimName), dimid) == NF90_NOERR
  end function dim_exists
  !--------------------------------------------------------------------------------------------------------------------
  function var_exists(ncid, varName)
    !
    ! Does this variable exist (have a valid var_id) in the open netCDF file?
    !
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    logical                      :: var_exists

    integer :: varId
    var_exists = nf90_inq_varid(ncid, trim(varName), varid) == NF90_NOERR
  end function var_exists
  !--------------------------------------------------------------------------------------------------------------------
  subroutine create_dim(ncid, dimName, dimLength)
    !
    ! Check to see if a dimiable with this name exists in the file
    !   If so, check against current size
    !   If not, create with specified dimensions
    !
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: dimName
    integer,          intent(in) :: dimLength

    integer                 :: dimid

    if(dim_exists(ncid, dimName)) then
      if (dimLength /= get_dim_size(ncid, trim(dimName))) &
          call stop_on_err("dim " // trim(dimName) // " is present but incorrectly sized.")
    else
      if(nf90_redef(ncid) /= NF90_NOERR) &
        call stop_on_err("create_dim: can't put file into redefine mode")
      if(nf90_def_dim(ncid, dimName, dimLength, dimid) /= NF90_NOERR) &
        call stop_on_err("create_dim: can't define dimension " // trim(dimName))
      if(nf90_enddef(ncid) /= NF90_NOERR) &
        call stop_on_err("create_dim: can't end redefinition??")
    end if
  end subroutine create_dim
  !--------------------------------------------------------------------------------------------------------------------
  subroutine create_var(ncid, varName, dimNames, dimLengths, dataType)
    !
    ! Check to see if a variable with this name exists in the file
    !   If so, check against current size
    !   If not, create with specified dimensions
    ! datatype: NF90_DOUBLE, NF90_FLOAT, NF90_INT, etc.
    !
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    character(len=*), intent(in) :: dimNames(:)
    integer,          intent(in) :: dimLengths(:)
    integer, optional, intent(in) :: dataType

    integer :: i, varid, xtype
    integer :: dimIds(size(dimNames))

    if(var_exists(ncid, varName)) then
      do i = 1, size(dimNames)
        if (dimLengths(i) /= get_dim_size(ncid, trim(dimNames(i)))) &
          call stop_on_err("Variable " // trim(varName) // " is present but incorrectly sized.")
      end do
    else
      do i = 1, size(dimNames)
        if(nf90_inq_dimid(ncid, trim(dimNames(i)), dimIds(i)) /= NF90_NOERR) &
          call stop_on_err("create_var: Can't get id for dimension " // trim(dimnames(i)))
      end do
      if(nf90_redef(ncid) /= NF90_NOERR) &
        call stop_on_err("create_var: can't put file into redefine mode")
      xtype = NF90_DOUBLE
      if(present(dataType)) xtype = dataType
      if(nf90_def_var(ncid, varName, xtype, dimIds, varid) /= NF90_NOERR) &
        call stop_on_err("create_var: can't define variable " // trim(varName))
      if(nf90_enddef(ncid) /= NF90_NOERR) &
        call stop_on_err("create_dim: can't end redefinition??")
    end if
  end subroutine create_var
  !--------------------------------------------------------------------------------------------------------------------
  function get_dim_size(ncid, dimname)
    !
    ! Get the length of a dimension from an open netCDF file
    !  This is unfortunately a two-step process
    !
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: dimname
    integer :: get_dim_size

    integer :: dimid

    if(nf90_inq_dimid(ncid, trim(dimname), dimid) == NF90_NOERR) then
      if(nf90_inquire_dimension(ncid, dimid, len=get_dim_size) /= NF90_NOERR) get_dim_size = 0
    else
      get_dim_size = 0
    end if

  end function get_dim_size
  !--------------------------------------------------------------------------------------------------------------------
  function get_var_size(ncid, varName, n)
    !
    ! Returns the extents of a netcdf variable on disk
    !
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer,          intent(in) :: n
    integer                      :: get_var_size(n)

    integer :: i
    integer :: varid, ndims, dimids(n)

    get_var_size(n) = -1
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("get_var_size: can't find variable " // trim(varName))
    if(nf90_inquire_variable(ncid, varid, ndims = ndims) /= NF90_NOERR) &
      call stop_on_err("get_var_size: can't get information for variable " // trim(varName))
    if(ndims /= n) &
      call stop_on_err("get_var_size:  variable " // trim(varName) // " has the wrong number of dimensions" )
    if(nf90_inquire_variable(ncid, varid, dimids = dimids) /= NF90_NOERR) &
      call stop_on_err("get_var_size: can't read dimension ids for variable " // trim(varName))
    do i = 1, n
      if(nf90_inquire_dimension(ncid, dimids(i), len = get_var_size(i)) /= NF90_NOERR) &
        call stop_on_err("get_var_size: can't get dimension lengths for variable " // trim(varName))
    end do

  end function get_var_size
  !--------------------------------------------------------------------------------------------------------------------
  subroutine stop_on_err(msg)
    !
    ! Print error message and stop
    !
    use iso_fortran_env, only : error_unit
    character(len=*), intent(in) :: msg
    if(len_trim(msg) > 0) then
      write(error_unit,*) trim(msg)
      error stop 1
    end if
  end subroutine
  !--------------------------------------------------------------------------------------------------------------------
end module mo_simple_netcdf
