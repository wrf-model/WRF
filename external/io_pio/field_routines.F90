!------------------------------------------------------------------
!$Id$
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

  if(IO == 'write') then
    if(whole)then
       stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable), &
                          Starts,Counts,Data(1:datasize))
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

  if(IO == 'write') then
    if(whole)then
      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable), &
                         Starts,Counts,Data(1:datasize))
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

subroutine ext_pio_IntFieldIO(whole,IO,DH,Starts,Counts,fldsize,datasize,Data,Status)
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
  integer,dimension(1:fldsize),intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat
  integer, parameter                         :: fillvalue = 20140822
  integer                                    :: Buffer(1)

 !call pio_setdebuglevel(1)

  if(IO == 'write') then
    if(whole)then
      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable), &
                         Starts,Counts,Data(1:datasize))
    else
      call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                            DH%ioVar(DH%CurrentVariable), Data, stat, fillvalue)
    end if
  else
    if(whole)then
      if(1 == fldsize) then
        stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Buffer)
        Data(1) = Buffer(1)
      else
        stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Data)
      endif
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

  if(IO == 'write') then
    do n=1,fldsize
      if(data(n)) then
        Buffer(n)=1
      else
        Buffer(n)=0
      endif
    enddo
    if(whole)then
      stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable), &
                         Starts,Counts,Buffer(1:datasize))
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

