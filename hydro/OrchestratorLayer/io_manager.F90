module io_manager_base
  use netcdf_layer_base
  implicit none

  type :: IOManager_
     logical :: parallel = .false.
     class(NetCDF_layer_),allocatable :: netcdf_layer
   contains
  end type IOManager_

  interface IOManager_
     module procedure IOManager_init
  end interface IOManager_

contains

  type(IOManager_) function IOManager_init(parallel)
    implicit none

    logical, optional, intent(in) :: parallel

    if(.not.present(parallel)) then
       IOManager_init%parallel = .false.
    else
       IOManager_init%parallel = parallel
    end if

    if( IOManager_init%parallel .eqv. .false.) then
       allocate(NetCDF_serial_ :: IOManager_init%netcdf_layer)
       IOManager_init%netcdf_layer%open_file => nf90_open
    else
       allocate(NetCDF_parallel_ :: IOManager_init%netcdf_layer)
       IOManager_init%netcdf_layer%open_file => nf90_open
    end if

  end function IOManager_init

end module io_manager_base
