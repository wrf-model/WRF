subroutine ext_pio_RealFieldIO(whole,IO,DH,fldsize,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)                      :: DH
  integer                     ,intent(in)    :: fldsize
  real, dimension(1:fldsize)  ,intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  logical                                    :: found
  integer                                    :: stat
  integer                                    :: n, ndims, datasize
  integer, dimension(:), allocatable         :: dimids
  integer, dimension(:), allocatable         :: dimsizes

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

  if(IO == 'write') then
    call pio_setdebuglevel(1)
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    if(whole)then
      stat = pio_inq_varndims(DH%file_handle,DH%descVar(DH%CurrentVariable),ndims)
      allocate(dimids(ndims), stat=stat)
      allocate(dimsizes(ndims), stat=stat)
      stat = pio_inq_vardimid(DH%file_handle,DH%descVar(DH%CurrentVariable),dimids)
      datasize = 1
      do n = 1, ndims - 1
         stat = pio_inq_dimlen(DH%file_handle,dimids(n),dimsizes(n))
         datasize = datasize*dimsizes(n)
         write(unit=0, fmt='(a,i2,a,i6)') 'dimids(', n, ')=', dimids(n)
         write(unit=0, fmt='(a,i2,a,i6)') 'dimsizes(', n, ')=', dimsizes(n)
      end do
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      write(unit=0, fmt='(a,i6)') 'fldsize = ', fldsize
      write(unit=0, fmt='(a,i6)') 'datasize = ', datasize
      if(fldsize < datasize) then
          write(unit=0, fmt='(a)') 'fldsize is too small'
          stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data)
      else if(fldsize > datasize) then
          write(unit=0, fmt='(a)') 'fldsize is too big.'
          stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data(1:datasize))
      end if

      deallocate(dimids)
      deallocate(dimsizes)
    else
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                            DH%ioVar(DH%CurrentVariable), Data, stat)
    end if
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
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
end subroutine ext_pio_RealFieldIO

subroutine ext_pio_DoubleFieldIO(whole,IO,DH,fldsize,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)       ,pointer       :: DH
  integer                     ,intent(in)    :: fldsize
  real*8,dimension(1:fldsize), intent(inout) :: Data
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

subroutine ext_pio_IntFieldIO(whole,IO,DH,fldsize,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)       ,pointer       :: DH
  integer                     ,intent(in)    :: fldsize
  integer,dimension(1:fldsize),intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat
  integer                                    :: Buffer(10)

 !call pio_setdebuglevel(1)

 !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
 !write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
 !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
 !write(unit=0, fmt='(3a)') 'IO: <', IO, '>'
 !write(unit=0, fmt='(a, l8)') 'whole: ', whole

  if(IO == 'write') then
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    if(whole)then
     !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data)
    else
     !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                            DH%ioVar(DH%CurrentVariable), Data, stat)
    end if
   !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  else
    if(whole)then
     !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      if(1 == fldsize) then
       !stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Buffer)
        stat = pio_get_var(DH%file_handle,DH%VarIDs(DH%CurrentVariable),Buffer)
        Data(1) = Buffer(1)
      else
        stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data)
      endif
     !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    else
     !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      call pio_read_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                           DH%ioVar(DH%CurrentVariable), Data, stat)
    end if
   !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  endif
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif
 !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  return
end subroutine ext_pio_IntFieldIO

subroutine ext_pio_LogicalFieldIO(whole,IO,DH,fldsize,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)       ,pointer       :: DH
  integer                     ,intent(in)    :: fldsize
  logical,dimension(1:fldsize),intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer,dimension(1:fldsize)               :: Buffer
  integer                                    :: stat
  integer                                    :: n

  if(IO == 'write') then
    do n=1,fldsize
      if(data(n)) then
        Buffer(n)=1
      else
        Buffer(n)=0
      endif
    enddo
    if(whole)then
      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Buffer)
    else
      call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                            DH%ioVar(DH%CurrentVariable), Buffer, stat)
    end if
  else
    if(whole)then
      stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Buffer)
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
  return
end subroutine ext_pio_LogicalFieldIO

