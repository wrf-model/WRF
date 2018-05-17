!------------------------------------------------------------------
!$Id: field_routines.F90 7668 2014-09-29 16:48:30Z huangwei@ucar.edu $
!------------------------------------------------------------------

subroutine ext_pio_RealFieldIO(whole,IO,DH,Starts,Counts,fldsize,datasize,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)                      :: DH
  integer,dimension(NVarDims) ,intent(in)    :: Starts
  integer,dimension(NVarDims) ,intent(in)    :: Counts
  integer                     ,intent(in)    :: fldsize, datasize
  real, dimension(1:fldsize)  ,intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat
  real, parameter                            :: fillvalue = 9.96921e+36

  write(unit=0, fmt='(3x,3a,i6)') 'Enter ext_pio_RealFieldIO, File: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(6x,3a,l8)') 'IO = ', trim(IO), ', whole = ', whole
  write(unit=0, fmt='(6x,a,4i4)') 'Starts = ', Starts(1:3)
  write(unit=0, fmt='(6x,a,4i4)') 'Counts = ', Counts(1:3)
  write(unit=0, fmt='(6x,a,4i4)') 'fldsize = ', fldsize
  write(unit=0, fmt='(6x,a,4i4)') 'datasize = ', datasize
  write(unit=0, fmt='(6x,a,i4,a,4i4,3x,a)') 'DH%descVar(', DH%CurrentVariable, ') = ', DH%descVar(DH%CurrentVariable)

  if(IO == 'write') then
    if(whole)then
!      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable), &
!                         Starts(1:1),Counts(1:1),Data(1:datasize))
!      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable)%VarID,Data(1:datasize))
       stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data(1:datasize))
!      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable), &
!                         Starts(1:1),Counts(1:1),Data(1:datasize))
    else
      call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                            DH%ioVar(DH%CurrentVariable), Data, stat, fillvalue)
    end if
  else
    if(whole)then
      stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data(1:datasize))
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

  write(unit=0, fmt='(3x,3a,i6)') 'Leave ext_pio_RealFieldIO, File: ', __FILE__, ', line: ', __LINE__

  return
end subroutine ext_pio_RealFieldIO

subroutine ext_pio_DoubleFieldIO(whole,IO,DH,Starts,Counts,fldsize,datasize,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)       ,pointer       :: DH
  integer,dimension(NVarDims) ,intent(in)    :: Starts
  integer,dimension(NVarDims) ,intent(in)    :: Counts
  integer                     ,intent(in)    :: fldsize, datasize
  real*8,dimension(1:fldsize), intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat

  write(unit=0, fmt='(3x,3a,i6)') 'Enter ext_pio_DoubleFieldIO, File: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(6x,3a,l8)') 'IO = ', trim(IO), ', whole = ', whole
  write(unit=0, fmt='(6x,a,i4,a,4i4,3x,a)') 'DH%descVar(', DH%CurrentVariable, ') = ', DH%descVar(DH%CurrentVariable)
  write(unit=0, fmt='(6x,a,4i4)') 'Starts = ', Starts(1:3)
  write(unit=0, fmt='(6x,a,4i4)') 'Counts = ', Counts(1:3)

  if(IO == 'write') then
    if(whole)then
       stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable), &
                          Starts(1:1),Counts(1:1),Data(1:datasize))
!     stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data(1:datasize))
    else
      call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                            DH%ioVar(DH%CurrentVariable), Data, stat)
    end if
  else
    if(whole)then
      stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable), &
                         Starts(1:1),Counts(1:1),Data(1:datasize))
!     stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data)
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

  write(unit=0, fmt='(3x,3a,i6)') 'Leave ext_pio_DoubleFieldIO, File: ', __FILE__, ', line: ', __LINE__

  return
end subroutine ext_pio_DoubleFieldIO

subroutine ext_pio_IntFieldIO(whole,IO,DH,Starts,Counts,fldsize,datasize,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)                      :: DH
  integer,dimension(NVarDims) ,intent(in)    :: Starts
  integer,dimension(NVarDims) ,intent(in)    :: Counts
  integer                     ,intent(in)    :: fldsize, datasize
  integer,dimension(1:fldsize),intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat
  integer, parameter                         :: fillvalue = 20140822

 !write(unit=0, fmt='(3x,3a,i6)') 'Enter ext_pio_IntFieldIO, File: ', __FILE__, ', line: ', __LINE__
 !write(unit=0, fmt='(6x,3a,l8)') 'IO = ', trim(IO), ', whole = ', whole
 !write(unit=0, fmt='(6x,a,4i4)') 'Starts = ', Starts(1:3)
 !write(unit=0, fmt='(6x,a,4i4)') 'Counts = ', Counts(1:3)
 !write(unit=0, fmt='(6x,a,i4,a,4i4,3x,a)') 'DH%descVar(', DH%CurrentVariable, ') = ', DH%descVar(DH%CurrentVariable)

  if(IO == 'write') then
    if(whole)then
       stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable), &
                          Starts(1:1),Counts(1:1),Data(1:datasize))
!      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable)%VarID,Data(1:datasize))
!      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data(1:datasize))
    else
      call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                            DH%ioVar(DH%CurrentVariable), Data, stat, fillvalue)
    end if
  else
    if(whole)then
      stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data(1:datasize))
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

 !write(unit=0, fmt='(3x,3a,i6)') 'Leave ext_pio_IntFieldIO, File: ', __FILE__, ', line: ', __LINE__

  return
end subroutine ext_pio_IntFieldIO

subroutine ext_pio_LogicalFieldIO(whole,IO,DH,Starts,Counts,fldsize,datasize,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  logical                     ,intent(in)    :: whole
  character (*)               ,intent(in)    :: IO
  type(wrf_data_handle)       ,pointer       :: DH
  integer,dimension(NVarDims) ,intent(in)    :: Starts
  integer,dimension(NVarDims) ,intent(in)    :: Counts
  integer                     ,intent(in)    :: fldsize, datasize
  logical,dimension(1:fldsize),intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer,dimension(1:fldsize)               :: Buffer
  integer                                    :: stat
  integer                                    :: n

  write(unit=0, fmt='(3x,3a,i6)') 'Enter ext_pio_LogicalFieldIO, File: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(6x,3a,l8)') 'IO = ', trim(IO), ', whole = ', whole
  write(unit=0, fmt='(6x,a,i4,a,4i4,3x,a)') 'DH%descVar(', DH%CurrentVariable, ') = ', DH%descVar(DH%CurrentVariable)
  write(unit=0, fmt='(6x,a,4i4)') 'Starts = ', Starts(1:3)
  write(unit=0, fmt='(6x,a,4i4)') 'Counts = ', Counts(1:3)

  if(IO == 'write') then
    do n=1,fldsize
      if(data(n)) then
        Buffer(n)=1
      else
        Buffer(n)=0
      endif
    enddo
    if(whole)then
!     stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable), &
!                        Starts,Counts,Buffer(1:datasize))
      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Buffer(1:datasize))
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

  write(unit=0, fmt='(3x,3a,i6)') 'Leave ext_pio_LogicalFieldIO, File: ', __FILE__, ', line: ', __LINE__
  return
end subroutine ext_pio_LogicalFieldIO

