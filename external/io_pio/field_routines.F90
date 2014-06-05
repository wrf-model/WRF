subroutine ext_pio_RealFieldIO(whole,IO,DH,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)                      :: DH
  real, dimension(:)          ,intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  logical                                    :: found
  integer                                    :: stat

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a)') 'IO: <', IO, '>'
  write(unit=0, fmt='(a, l8)') 'whole: ', whole

  call pio_setdebuglevel(1)

  if(IO == 'write') then
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

    if(whole)then
      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data)
    else
      call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                            DH%ioVar(DH%CurrentVariable), Data, stat)
    end if
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  else
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
    write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
    write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable

    if(whole)then
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data)
    else
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      call pio_read_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                           DH%ioVar(DH%CurrentVariable), Data, stat)
    end if
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  endif
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

  return
end subroutine ext_pio_RealFieldIO

subroutine ext_pio_DoubleFieldIO(whole,IO,DH,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)       ,pointer       :: DH
#if 0
  integer                     ,intent(in)    :: VarID
  integer ,dimension(NVarDims),intent(in)    :: VStart
  integer ,dimension(NVarDims),intent(in)    :: VCount
#endif
  real*8  ,dimension(:)       ,intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat

  if(IO == 'write') then
    if(whole)then
      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data)
    else
      call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                            DH%ioVar(DH%CurrentVariable), Data, stat)
    end if
  else
    if(whole)then
      stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data)
    else
      call pio_read_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                           DH%ioVar(DH%CurrentVariable), Data, stat)
    end if
  endif
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif
  return
end subroutine ext_pio_DoubleFieldIO

!subroutine ext_pio_IntFieldIO(Coll,IO,file_handle,H%descVar(DH%CurrentVariable),VStart,VCount,Data,Status)
subroutine ext_pio_IntFieldIO(whole,IO,DH,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)       ,pointer       :: DH
#if 0
  integer                     ,intent(in)    :: VarID
  integer ,dimension(NVarDims),intent(in)    :: VStart
  integer ,dimension(NVarDims),intent(in)    :: VCount
#endif
  integer ,dimension(:)       ,intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat

  if(IO == 'write') then
    if(whole)then
      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data)
    else
      call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                            DH%ioVar(DH%CurrentVariable), Data, stat)
    end if
  else
    if(whole)then
      stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data)
    else
      call pio_read_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                           DH%ioVar(DH%CurrentVariable), Data, stat)
    end if
  endif
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif
  return
end subroutine ext_pio_IntFieldIO

subroutine ext_pio_LogicalFieldIO(whole,IO,DH,VStart,VCount,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)       ,pointer       :: DH
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
    if(whole)then
      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),VStart,VCount,Buffer)
   else
      call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                            DH%ioVar(DH%CurrentVariable), Buffer, stat)
   end if
  else
    if(whole)then
      stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),VStart,VCount,Buffer)
    else
      call pio_read_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                           DH%ioVar(DH%CurrentVariable), Buffer, stat)
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
