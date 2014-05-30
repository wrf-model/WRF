subroutine ext_pio_RealFieldIO(Coll,IO,NCID,VarID,VStart,VCount,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use ext_pio_support_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: Coll
  character (*)               ,intent(in)    :: IO
  type (file_desc_t)          ,intent(inout) :: NCID
  integer                     ,intent(in)    :: VarID
  integer ,dimension(NVarDims),intent(in)    :: VStart
  integer ,dimension(NVarDims),intent(in)    :: VCount
  real, dimension(:)          ,intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat

  if(IO == 'write') then
    if(Coll)then
      stat = pio_put_var(NCID,VarID,Data)
    else
      stat = pio_put_var(NCID,VarID,VStart,VCount,Data)
    end if
  else
    if(Coll)then
      stat = pio_get_var(NCID,VarID,Data)
    else
      stat = pio_get_var(NCID,VarID,VStart,VCount,Data)
   end if
  endif
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif
  return
end subroutine ext_pio_RealFieldIO

subroutine ext_pio_DoubleFieldIO(Coll,IO,NCID,VarID,VStart,VCount,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use ext_pio_support_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: Coll
  character (*)               ,intent(in)    :: IO
  type (file_desc_t)          ,intent(inout) :: NCID
  integer                     ,intent(in)    :: VarID
  integer ,dimension(NVarDims),intent(in)    :: VStart
  integer ,dimension(NVarDims),intent(in)    :: VCount
  real*8  ,dimension(:)       ,intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat

  if(IO == 'write') then
    if(Coll)then
      stat = pio_put_var(NCID,VarID,VStart,VCount,Data)
   else
      stat = pio_put_var(NCID,VarID,VStart,VCount,Data)
   endif
  else
    if(Coll)then
      stat = pio_get_var(NCID,VarID,VStart,VCount,Data)
   else
      stat = pio_get_var(NCID,VarID,VStart,VCount,Data)
   endif
  endif
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif
  return
end subroutine ext_pio_DoubleFieldIO

subroutine ext_pio_IntFieldIO(Coll,IO,NCID,VarID,VStart,VCount,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use ext_pio_support_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: Coll
  character (*)               ,intent(in)    :: IO
  type (file_desc_t)          ,intent(inout) :: NCID
  integer                     ,intent(in)    :: VarID
  integer ,dimension(NVarDims),intent(in)    :: VStart
  integer ,dimension(NVarDims),intent(in)    :: VCount
  integer ,dimension(:)       ,intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat

  if(IO == 'write') then
    if(Coll)then
      stat = pio_put_var(NCID,VarID,VStart,VCount,Data)
    else
      stat = pio_put_var(NCID,VarID,VStart,VCount,Data)
    endif
  else
    if(Coll)then
      stat = pio_get_var(NCID,VarID,VStart,VCount,Data)
   else
      stat = pio_get_var(NCID,VarID,VStart,VCount,Data)
   end if
  endif
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif
  return
end subroutine ext_pio_IntFieldIO

subroutine ext_pio_LogicalFieldIO(Coll,IO,NCID,VarID,VStart,VCount,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use ext_pio_support_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                                         ,intent(in)    :: Coll
  character (*)                                   ,intent(in)    :: IO
  type (file_desc_t)                              ,intent(inout) :: NCID
  integer                                         ,intent(in)    :: VarID
  integer,dimension(NVarDims)                     ,intent(in)    :: VStart
  integer,dimension(NVarDims)                     ,intent(in)    :: VCount
  logical,dimension(VCount(1),VCount(2),VCount(3)),intent(inout) :: Data
  integer                                         ,intent(out)   :: Status
  integer,dimension(:,:,:),allocatable                           :: Buffer
  integer                                                        :: stat
  integer                                                        :: i,j,k

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
    if(Coll)then
      stat = pio_put_var(NCID,VarID,VStart,VCount,Buffer)
   else
      stat = pio_put_var(NCID,VarID,VStart,VCount,Buffer)
   end if
  else
    if(Coll)then
      stat = pio_get_var(NCID,VarID,VStart,VCount,Buffer)
    else
      stat = pio_get_var(NCID,VarID,VStart,VCount,Buffer)
    end if
    Data = Buffer == 1
  endif
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  deallocate(Buffer, STAT=stat)
  if(stat/= 0) then
    Status = WRF_ERR_FATAL_DEALLOCATION_ERR
    write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_LogicalFieldIO
