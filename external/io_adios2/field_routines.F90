!*----------------------------------------------------------------------------
!*
!*  WRF ADIOS2 I/O
!*  Author: Michael Laufer
!*  	    Toga Networks, a Huawei Company
!*  	    michael.laufer@toganetworks.com
!*  Author: Erick Fredj
!*  	    Computer Science Department, The Jerusalem College of Technology
!*	    fredj@jct.ac.il
!*	    Toga Networks, a Huawei Company
!*	    erick.fredj@toganetworks.com
!*    	        
!*  Date:    November 3, 2021
!*
!*----------------------------------------------------------------------------

subroutine ext_adios2_RealFieldIO(IO,DataHandle,VarID,VStart,VCount,Data,Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  character (*)                       ,intent(in)          :: IO
  integer                             ,intent(in)          :: DataHandle
  type(adios2_variable)               ,intent(in)          :: VarID
  integer(kind=8),dimension(NVarDims) ,intent(in)          :: VStart
  integer(kind=8),dimension(NVarDims) ,intent(in)          :: VCount
  real                                ,intent(inout)       :: Data
  integer                             ,intent(out)         :: Status
  integer                                                  :: stat
  type(wrf_data_handle),pointer                            :: DH
  integer(kind=8),dimension(NVarDims)                      :: VStart_mpi, VCount_mpi

  !start arrays should start at 0 for ADIOS2
  VStart_mpi = VStart - 1
  VCount_mpi = VCount
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_adios2_RealFieldIO ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  !adios2_set_selection to set start dims and count dims
  call adios2_set_selection(VarID, VarID%ndims, VStart_mpi, VCount_mpi, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_RealFieldIO ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(IO == 'write') then
    call adios2_put(DH%adios2Engine, VarID, Data, adios2_mode_sync, stat)
  else
    call adios2_get(DH%adios2Engine, VarID, Data, adios2_mode_sync, stat)
  endif
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_RealFieldIO ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
end subroutine ext_adios2_RealFieldIO

subroutine ext_adios2_DoubleFieldIO(IO,DataHandle,VarID,VStart,VCount,Data,Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  character (*)                        ,intent(in)           :: IO
  integer                              ,intent(in)           :: DataHandle
  type(adios2_variable)                ,intent(in)           :: VarID
  integer(kind=8) ,dimension(NVarDims) ,intent(in)           :: VStart
  integer(kind=8) ,dimension(NVarDims) ,intent(in)           :: VCount
  real*8                               ,intent(inout)        :: Data
  integer                              ,intent(out)          :: Status
  integer                                                    :: stat
  type(wrf_data_handle),pointer                              :: DH
  integer(kind=8),dimension(NVarDims)                        :: VStart_mpi, VCount_mpi

  !start arrays should start at 0 for ADIOS2
  VStart_mpi = VStart - 1
  VCount_mpi = VCount
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_adios2_DoubleFieldIO ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  !adios2_set_selection to set start dims and count dims
  call adios2_set_selection(VarID, VarID%ndims, VStart_mpi, VCount_mpi, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_DoubleFieldIO ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(IO == 'write') then
    call adios2_put(DH%adios2Engine, VarID, Data, adios2_mode_sync, stat)
  else
    call adios2_get(DH%adios2Engine, VarID, Data, adios2_mode_sync, stat)
  endif
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_DoubleFieldIO ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  end subroutine ext_adios2_DoubleFieldIO

subroutine ext_adios2_IntFieldIO(IO,DataHandle,VarID,VStart,VCount,Data,Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  character (*)                        ,intent(in)          :: IO
  integer                              ,intent(in)          :: DataHandle
  type(adios2_variable)                ,intent(in)          :: VarID
  integer(kind=8) ,dimension(NVarDims) ,intent(in)          :: VStart
  integer(kind=8) ,dimension(NVarDims) ,intent(in)          :: VCount
  integer                              ,intent(inout)       :: Data
  integer                              ,intent(out)         :: Status
  integer                                                   :: stat
  type(wrf_data_handle),pointer                             :: DH
  integer(kind=8),dimension(NVarDims)                       :: VStart_mpi, VCount_mpi

  !start arrays should start at 0 for ADIOS2
  VStart_mpi = VStart - 1
  VCount_mpi = VCount
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_adios2_IntFieldIO ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  !adios2_set_selection to set start dims and count dims
  call adios2_set_selection(VarID, VarID%ndims, VStart_mpi, VCount_mpi, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_IntFieldIO ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(IO == 'write') then
    call adios2_put(DH%adios2Engine, VarID, Data, adios2_mode_sync, stat)
  else
    call adios2_get(DH%adios2Engine, VarID, Data, adios2_mode_sync, stat)
  endif
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_IntFieldIO ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
end subroutine ext_adios2_IntFieldIO

subroutine ext_adios2_LogicalFieldIO(IO,DataHandle,VarID,VStart,VCount,Data,Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  character (*)                                    ,intent(in)         :: IO
  integer                                          ,intent(in)         :: DataHandle
  type(adios2_variable)                            ,intent(in)         :: VarID
  integer(kind=8) ,dimension(NVarDims)             ,intent(in)         :: VStart
  integer(kind=8) ,dimension(NVarDims)             ,intent(in)         :: VCount
  logical,dimension(VCount(1),VCount(2),VCount(3)) ,intent(inout)      :: Data
  integer                                          ,intent(out)        :: Status
  integer                                                              :: stat
  type(wrf_data_handle),pointer                                        :: DH
  integer,dimension(:,:,:),allocatable                                 :: Buffer
  integer                                                              :: i,j,k
  integer(kind=8),dimension(NVarDims)                                  :: VStart_mpi, VCount_mpi

  !start arrays should start at 0 for ADIOS2
  VStart_mpi = VStart - 1
  VCount_mpi = VCount
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_adios2_LogicalFieldIO ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  !adios2_set_selection to set start dims and count dims
  call adios2_set_selection(VarID, VarID%ndims, VStart_mpi, VCount_mpi, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_LogicalFieldIO ',__FILE__,', line', __LINE__
  call wrf_debug ( WARN , TRIM(msg))
  return
  endif
  allocate(Buffer(VCount(1),VCount(2),VCount(3)), STAT=stat)
  if(stat/= 0) then
    Status = WRF_ERR_FATAL_ALLOCATION_ERROR
    write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  if(IO == 'write') then
    do k=1,VCount(3)
      do j=1,VCount(2)
        do i=1,VCount(1)
          if(data(i,j,k)) then
            Buffer(i,j,k)=1
          else
            Buffer(i,j,k)=0
          endif
        enddo
      enddo
    enddo
    call adios2_put(DH%adios2Engine, VarID, Buffer, adios2_mode_sync, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ext_adios2_LogicalFieldIO ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  else
    call adios2_get(DH%adios2Engine, VarID, Buffer, adios2_mode_sync, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ext_adios2_LogicalFieldIO ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    Data = Buffer == 1
  endif
  deallocate(Buffer, STAT=stat)
  if(stat/= 0) then
    Status = WRF_ERR_FATAL_DEALLOCATION_ERR
    write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_adios2_LogicalFieldIO
