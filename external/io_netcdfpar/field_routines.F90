!*------------------------------------------------------------------------------
!*  Standard Disclaimer
!*
!*  Forecast Systems Laboratory
!*  NOAA/OAR/ERL/FSL
!*  325 Broadway
!*  Boulder, CO     80303
!*
!*  AVIATION DIVISION
!*  ADVANCED COMPUTING BRANCH
!*  SMS/NNT Version: 2.0.0 
!*
!*  This software and its documentation are in the public domain and
!*  are furnished "as is".  The United States government, its 
!*  instrumentalities, officers, employees, and agents make no 
!*  warranty, express or implied, as to the usefulness of the software 
!*  and documentation for any purpose.  They assume no 
!*  responsibility (1) for the use of the software and documentation; 
!*  or (2) to provide technical support to users.
!* 
!*  Permission to use, copy, modify, and distribute this software is
!*  hereby granted, provided that this disclaimer notice appears in 
!*  all copies.  All modifications to this software must be clearly
!*  documented, and are solely the responsibility of the agent making
!*  the modification.  If significant modifications or enhancements
!*  are made to this software, the SMS Development team
!*  (sms-info@fsl.noaa.gov) should be notified.
!*
!*----------------------------------------------------------------------------
!*
!*  WRF NetCDF I/O
!   Author:  Jacques Middlecoff jacquesm@fsl.noaa.gov
!*  Date:    October 6, 2000
!*
!*----------------------------------------------------------------------------
subroutine ext_ncdpar_RealFieldIO(IO,NCID,VarID,VStart,VCount,Data,Status)
  use wrf_data_ncpar
  use ext_ncdpar_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character (*)               ,intent(in)    :: IO
  integer                     ,intent(in)    :: NCID
  integer                     ,intent(in)    :: VarID
  integer ,dimension(NVarDims),intent(in)    :: VStart
  integer ,dimension(NVarDims),intent(in)    :: VCount
  real, dimension(*)          ,intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat
  character(len=180)     :: errmsg
  integer i

  if(IO == 'write') then
  !  write(0,*) 'vstart = ',vstart
  !  write(0,*) 'vcount = ',vcount
  !  write(0,*) 'varid = ',varid
  !  i = vcount(1)*vcount(2)
  !  write(0,*) 'data ',data(1),data(i)
    !write(0,*) 'data size = ',size(data)
    stat = NF_VAR_PAR_ACCESS(ncid,VarID,nf_collective)
    call netcdf_err(stat,Status)
    if ( Status /= WRF_NO_ERR) then
      write(0,*) 'error setting par_access'
    ENDIF
  !  write(0,*) 'call put_vara'
    stat = NF_PUT_VARA_REAL(NCID,VarID,VStart,VCount,Data)
  !  write(0,*) 'done put_vara'
  else
    stat = NF_GET_VARA_REAL(NCID,VarID,VStart,VCount,Data)
  endif
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
        errmsg = NF_STRERROR(stat) 
    write(0,*) 'error = ',errmsg
    write(0,*) 'vstart = ',vstart
    write(0,*) 'vcount = ',vcount
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif
  !write(0,*) 'barrier 1'
  !call mpi_barrier(MPI_COMM_WORLD,i)
  !write(0,*) 'barrier 2'
  return
end subroutine ext_ncdpar_RealFieldIO

subroutine ext_ncdpar_DoubleFieldIO(IO,NCID,VarID,VStart,VCount,Data,Status)
  use wrf_data_ncpar
  use ext_ncdpar_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character (*)               ,intent(in)    :: IO
  integer                     ,intent(in)    :: NCID
  integer                     ,intent(in)    :: VarID
  integer ,dimension(NVarDims),intent(in)    :: VStart
  integer ,dimension(NVarDims),intent(in)    :: VCount
  real*8                      ,intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat

  if(IO == 'write') then
    stat = NF_VAR_PAR_ACCESS(ncid,VarID,nf_collective)
    stat = NF_PUT_VARA_DOUBLE(NCID,VarID,VStart,VCount,Data)
  else
    stat = NF_GET_VARA_DOUBLE(NCID,VarID,VStart,VCount,Data)
  endif
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif
  return
end subroutine ext_ncdpar_DoubleFieldIO

subroutine ext_ncdpar_IntFieldIO(IO,NCID,VarID,VStart,VCount,Data,Status)
  use wrf_data_ncpar
  use ext_ncdpar_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character (*)               ,intent(in)    :: IO
  integer                     ,intent(in)    :: NCID
  integer                     ,intent(in)    :: VarID
  integer ,dimension(NVarDims),intent(in)    :: VStart
  integer ,dimension(NVarDims),intent(in)    :: VCount
  integer                     ,intent(inout) :: Data
  integer                     ,intent(out)   :: Status
  integer                                    :: stat

  if(IO == 'write') then
    stat = NF_VAR_PAR_ACCESS(ncid,VarID,nf_collective)
    stat = NF_PUT_VARA_INT(NCID,VarID,VStart,VCount,Data)
  else
    stat = NF_GET_VARA_INT(NCID,VarID,VStart,VCount,Data)
  endif
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif
  return
end subroutine ext_ncdpar_IntFieldIO

subroutine ext_ncdpar_LogicalFieldIO(IO,NCID,VarID,VStart,VCount,Data,Status)
  use wrf_data_ncpar
  use ext_ncdpar_support_routines
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character (*)                                   ,intent(in)    :: IO
  integer                                         ,intent(in)    :: NCID
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
    stat = NF_VAR_PAR_ACCESS(ncid,VarID,nf_collective)
    stat = NF_PUT_VARA_INT(NCID,VarID,VStart,VCount,Buffer)
  else
    stat = NF_GET_VARA_INT(NCID,VarID,VStart,VCount,Buffer)
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
end subroutine ext_ncdpar_LogicalFieldIO
