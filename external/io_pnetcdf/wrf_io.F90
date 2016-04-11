!/*------------------------------------------------------------------------------
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
!*/

module wrf_data_pnc

  integer                , parameter      :: FATAL            = 0
  integer                , parameter      :: WARN             = 0
  integer                , parameter      :: WrfDataHandleMax = 99
  integer                , parameter      :: MaxDims          = 2000 ! = NF_MAX_VARS
  integer                , parameter      :: MaxVars          = 3000
  integer                , parameter      :: MaxTimes         = 10000
  integer                , parameter      :: DateStrLen       = 19
  integer                , parameter      :: VarNameLen       = 31
  integer                , parameter      :: NO_DIM           = 0
  integer                , parameter      :: NVarDims         = 4
  integer                , parameter      :: NMDVarDims       = 2
  character (8)          , parameter      :: NO_NAME          = 'NULL'
  character (DateStrLen) , parameter      :: ZeroDate = '0000-00-00-00:00:00'

#include "wrf_io_flags.h"

  character (256)                         :: msg
  logical                                 :: WrfIOnotInitialized = .true.

  type :: wrf_data_handle
    character (255)                       :: FileName
    integer                               :: FileStatus
    integer                               :: Comm
    integer                               :: NCID
    logical                               :: Free
    logical                               :: Write
    character (5)                         :: TimesName
    integer                               :: TimeIndex
    integer                               :: CurrentTime  !Only used for read
    integer                               :: NumberTimes  !Only used for read
    character (DateStrLen), pointer       :: Times(:)
    integer                               :: TimesVarID
    integer               , pointer       :: DimLengths(:)
    integer               , pointer       :: DimIDs(:)
    character (31)        , pointer       :: DimNames(:)
    integer                               :: DimUnlimID
    character (9)                         :: DimUnlimName
    integer       , dimension(NVarDims)   :: DimID
    integer       , dimension(NVarDims)   :: Dimension
    integer               , pointer       :: MDVarIDs(:)
    integer               , pointer       :: MDVarDimLens(:)
    character (80)        , pointer       :: MDVarNames(:)
    integer               , pointer       :: VarIDs(:)
    integer               , pointer       :: VarDimLens(:,:)
    character (VarNameLen), pointer       :: VarNames(:)
    integer                               :: CurrentVariable  !Only used for read
    integer                               :: NumVars
! first_operation is set to .TRUE. when a new handle is allocated 
! or when open-for-write or open-for-read are committed.  It is set 
! to .FALSE. when the first field is read or written.  
    logical                               :: first_operation
! Whether pnetcdf file is in collective (.true.) or independent mode
! Collective mode is the default.
    logical                               :: Collective
  end type wrf_data_handle
  type(wrf_data_handle),target            :: WrfDataHandles(WrfDataHandleMax)
end module wrf_data_pnc

module ext_pnc_support_routines

  implicit none
  include 'mpif.h'

CONTAINS

integer(KIND=MPI_OFFSET_KIND) function i2offset(i)
  integer i
  i2offset = i
  return
end function i2offset

subroutine allocHandle(DataHandle,DH,Comm,Status)
  use wrf_data_pnc
  include 'wrf_status_codes.h'
  integer              ,intent(out) :: DataHandle
  type(wrf_data_handle),pointer     :: DH
  integer              ,intent(IN)  :: Comm
  integer              ,intent(out) :: Status
  integer                           :: i
  integer                           :: stat

  do i=1,WrfDataHandleMax
    if(WrfDataHandles(i)%Free) then
      DH => WrfDataHandles(i)
      DataHandle = i
      allocate(DH%Times(MaxTimes), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%DimLengths(MaxDims), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%DimIDs(MaxDims), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%DimNames(MaxDims), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%MDVarIDs(MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%MDVarDimLens(MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%MDVarNames(MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%VarIDs(MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%VarDimLens(NVarDims-1,MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      allocate(DH%VarNames(MaxVars), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      exit
    endif
    if(i==WrfDataHandleMax) then
      Status = WRF_WARN_TOO_MANY_FILES
      write(msg,*) 'Warning TOO MANY FILES in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      write(msg,*) 'Did you call ext_pnc_ioinit?'
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  enddo
  DH%Free      =.false.
  DH%Comm      = Comm
  DH%Write     =.false.
  DH%first_operation  = .TRUE.
  DH%Collective = .TRUE.
  Status = WRF_NO_ERR
end subroutine allocHandle

subroutine deallocHandle(DataHandle, Status)
  use wrf_data_pnc
  include 'wrf_status_codes.h'
  integer              ,intent(in) :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: i
  integer                           :: stat

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. WrfDataHandleMax ) THEN
    if(.NOT. WrfDataHandles(DataHandle)%Free) then
      DH => WrfDataHandles(DataHandle)
      deallocate(DH%Times, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%DimLengths, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%DimIDs, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%DimNames, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%MDVarIDs, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%MDVarDimLens, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%MDVarNames, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%VarIDs, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%VarDimLens, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      deallocate(DH%VarNames, STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      DH%Free      =.TRUE.
    endif
  ENDIF
  Status = WRF_NO_ERR
end subroutine deallocHandle

subroutine GetDH(DataHandle,DH,Status)
  use wrf_data_pnc
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  type(wrf_data_handle) ,pointer        :: DH
  integer               ,intent(out)    :: Status

  if(DataHandle < 1 .or. DataHandle > WrfDataHandleMax) then
    Status = WRF_WARN_BAD_DATA_HANDLE
    return
  endif
  DH => WrfDataHandles(DataHandle)
  if(DH%Free) then
    Status = WRF_WARN_BAD_DATA_HANDLE
    return
  endif
  Status = WRF_NO_ERR
  return
end subroutine GetDH

subroutine DateCheck(Date,Status)
  use wrf_data_pnc
  include 'wrf_status_codes.h'
  character*(*) ,intent(in)      :: Date
  integer       ,intent(out)     :: Status
  
  if(len(Date) /= DateStrLen) then
    Status = WRF_WARN_DATESTR_BAD_LENGTH
  else  
    Status = WRF_NO_ERR
  endif
  return
end subroutine DateCheck

subroutine GetName(Element,Var,Name,Status)
  use wrf_data_pnc
  include 'wrf_status_codes.h'
  character*(*) ,intent(in)     :: Element
  character*(*) ,intent(in)     :: Var
  character*(*) ,intent(out)    :: Name
  integer       ,intent(out)    :: Status
  character (VarNameLen)        :: VarName
  character (1)                 :: c
  integer                       :: i
  integer, parameter            ::  upper_to_lower =IACHAR('a')-IACHAR('A')

  VarName = Var
  Name = 'MD___'//trim(Element)//VarName
  do i=1,len(Name)
    c=Name(i:i)
    if('A'<=c .and. c <='Z') Name(i:i)=achar(iachar(c)+upper_to_lower)
    if(c=='-'.or.c==':') Name(i:i)='_'
  enddo
  Status = WRF_NO_ERR
  return
end subroutine GetName

subroutine GetTimeIndex(IO,DataHandle,DateStr,TimeIndex,Status)
  use wrf_data_pnc
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  character (*)         ,intent(in)     :: IO
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(out)    :: TimeIndex
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer(KIND=MPI_OFFSET_KIND)         :: VStart(2)
  integer(KIND=MPI_OFFSET_KIND)         :: VCount(2)
  integer                               :: stat
  integer                               :: i

  DH => WrfDataHandles(DataHandle)
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    Status =  WRF_WARN_DATESTR_ERROR
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(IO == 'write') then
    TimeIndex = DH%TimeIndex
    if(TimeIndex <= 0) then
      TimeIndex = 1
    elseif(DateStr == DH%Times(TimeIndex)) then
      Status = WRF_NO_ERR
      return
    else
      TimeIndex = TimeIndex +1
      if(TimeIndex > MaxTimes) then
        Status = WRF_WARN_TIME_EOF
        write(msg,*) 'Warning TIME EOF in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    endif
    DH%TimeIndex        = TimeIndex
    DH%Times(TimeIndex) = DateStr
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = DateStrLen
    VCount(2) = 1
    stat = NFMPI_PUT_VARA_TEXT_ALL(DH%NCID,DH%TimesVarID,VStart,VCount,DateStr)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  else
    do i=1,MaxTimes
      if(DH%Times(i)==DateStr) then
        Status = WRF_NO_ERR
        TimeIndex = i
        exit
      endif
      if(i==MaxTimes) then
        Status = WRF_WARN_TIME_NF
        write(msg,*) 'Warning TIME ',DateStr,' NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
  endif
  return
end subroutine GetTimeIndex

subroutine GetDim(MemoryOrder,NDim,Status)
  include 'wrf_status_codes.h'
  character*(*) ,intent(in)  :: MemoryOrder
  integer       ,intent(out) :: NDim
  integer       ,intent(out) :: Status
  character*3                :: MemOrd

  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)
    case ('xyz','xzy','yxz','yzx','zxy','zyx','xsz','xez','ysz','yez')
      NDim = 3
    case ('xy','yx','xs','xe','ys','ye')
      NDim = 2
    case ('z','c')
      NDim = 1
    case ('0')  ! NDim=0 for scalars.  TBH:  20060502
      NDim = 0
    case default
      print *, 'memory order = ',MemOrd,'  ',MemoryOrder
      Status = WRF_WARN_BAD_MEMORYORDER
      return
  end select
  Status = WRF_NO_ERR
  return
end subroutine GetDim

subroutine GetIndices(NDim,Start,End,i1,i2,j1,j2,k1,k2)
  integer              ,intent(in)  :: NDim
  integer ,dimension(*),intent(in)  :: Start,End
  integer              ,intent(out) :: i1,i2,j1,j2,k1,k2

  i1=1
  i2=1
  j1=1
  j2=1
  k1=1
  k2=1
  if(NDim == 0) return  ! NDim=0 for scalars.  TBH:  20060502
  i1 = Start(1)
  i2 = End  (1)
  if(NDim == 1) return
  j1 = Start(2)
  j2 = End  (2)
  if(NDim == 2) return
  k1 = Start(3)
  k2 = End  (3)
  return
end subroutine GetIndices

logical function ZeroLengthHorzDim(MemoryOrder,Vector,Status)
  use wrf_data_pnc
  include 'wrf_status_codes.h'
  character*(*)              ,intent(in)    :: MemoryOrder
  integer,dimension(*)       ,intent(in)    :: Vector
  integer                    ,intent(out)   :: Status
  integer                                   :: NDim
  integer,dimension(NVarDims)               :: temp
  character*3                               :: MemOrd
  logical zero_length

  call GetDim(MemoryOrder,NDim,Status)
  temp(1:NDim) = Vector(1:NDim)
  call LowerCase(MemoryOrder,MemOrd)
  zero_length = .false.
  select case (MemOrd)
    case ('xsz','xez','ysz','yez','xs','xe','ys','ye','z','c')
      continue
    case ('0')
      continue  ! NDim=0 for scalars.  TBH:  20060502
    case ('xzy','yzx')
      zero_length = temp(1) .lt. 1 .or. temp(3) .lt. 1
    case ('xy','yx','xyz','yxz')
      zero_length = temp(1) .lt. 1 .or. temp(2) .lt. 1
    case ('zxy','zyx')
      zero_length = temp(2) .lt. 1 .or. temp(3) .lt. 1
    case default
      Status = WRF_WARN_BAD_MEMORYORDER
      ZeroLengthHorzDim = .true.
      return
  end select
  Status = WRF_NO_ERR
  ZeroLengthHorzDim = zero_length
  return
end function ZeroLengthHorzDim

subroutine ExtOrder(MemoryOrder,Vector,Status)
  use wrf_data_pnc
  include 'wrf_status_codes.h'
  character*(*)              ,intent(in)    :: MemoryOrder
  integer,dimension(*)       ,intent(inout) :: Vector
  integer                    ,intent(out)   :: Status
  integer                                   :: NDim
  integer,dimension(NVarDims)               :: temp
  character*3                               :: MemOrd

  call GetDim(MemoryOrder,NDim,Status)
  temp(1:NDim) = Vector(1:NDim)
  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)

    case ('xyz','xsz','xez','ysz','yez','xy','xs','xe','ys','ye','z','c')
      continue
    case ('0')
      continue  ! NDim=0 for scalars.  TBH:  20060502
    case ('xzy')
      Vector(2) = temp(3)
      Vector(3) = temp(2)
    case ('yxz')
      Vector(1) = temp(2)
      Vector(2) = temp(1)
    case ('yzx')
      Vector(1) = temp(3)
      Vector(2) = temp(1)
      Vector(3) = temp(2)
    case ('zxy')
      Vector(1) = temp(2)
      Vector(2) = temp(3)
      Vector(3) = temp(1)
    case ('zyx')
      Vector(1) = temp(3)
      Vector(3) = temp(1)
    case ('yx')
      Vector(1) = temp(2)
      Vector(2) = temp(1)
    case default
      Status = WRF_WARN_BAD_MEMORYORDER
      return
  end select
  Status = WRF_NO_ERR
  return
end subroutine ExtOrder

subroutine ExtOrderStr(MemoryOrder,Vector,ROVector,Status)
  use wrf_data_pnc
  include 'wrf_status_codes.h'
  character*(*)                    ,intent(in)    :: MemoryOrder
  character*(*),dimension(*)       ,intent(in)    :: Vector
  character(80),dimension(NVarDims),intent(out)   :: ROVector
  integer                          ,intent(out)   :: Status
  integer                                         :: NDim
  character*3                                     :: MemOrd

  call GetDim(MemoryOrder,NDim,Status)
  ROVector(1:NDim) = Vector(1:NDim)
  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)

    case ('xyz','xsz','xez','ysz','yez','xy','xs','xe','ys','ye','z','c')
      continue
    case ('0')
      continue  ! NDim=0 for scalars.  TBH:  20060502
    case ('xzy')
      ROVector(2) = Vector(3)
      ROVector(3) = Vector(2)
    case ('yxz')
      ROVector(1) = Vector(2)
      ROVector(2) = Vector(1)
    case ('yzx')
      ROVector(1) = Vector(3)
      ROVector(2) = Vector(1)
      ROVector(3) = Vector(2)
    case ('zxy')
      ROVector(1) = Vector(2)
      ROVector(2) = Vector(3)
      ROVector(3) = Vector(1)
    case ('zyx')
      ROVector(1) = Vector(3)
      ROVector(3) = Vector(1)
    case ('yx')
      ROVector(1) = Vector(2)
      ROVector(2) = Vector(1)
    case default
      Status = WRF_WARN_BAD_MEMORYORDER
      return
  end select
  Status = WRF_NO_ERR
  return
end subroutine ExtOrderStr


subroutine LowerCase(MemoryOrder,MemOrd)
  character*(*) ,intent(in)  :: MemoryOrder
  character*(*) ,intent(out) :: MemOrd
  character*1                :: c
  integer       ,parameter   :: upper_to_lower =IACHAR('a')-IACHAR('A')
  integer                    :: i,N

  MemOrd = ' '
  N = len(MemoryOrder)
  MemOrd(1:N) = MemoryOrder(1:N)
  do i=1,N
    c = MemoryOrder(i:i)
    if('A'<=c .and. c <='Z') MemOrd(i:i)=achar(iachar(c)+upper_to_lower)
  enddo
  return
end subroutine LowerCase

subroutine UpperCase(MemoryOrder,MemOrd)
  character*(*) ,intent(in)  :: MemoryOrder
  character*(*) ,intent(out) :: MemOrd
  character*1                :: c
  integer     ,parameter     :: lower_to_upper =IACHAR('A')-IACHAR('a')
  integer                    :: i,N

  MemOrd = ' '
  N = len(MemoryOrder)
  MemOrd(1:N) = MemoryOrder(1:N)
  do i=1,N
    c = MemoryOrder(i:i)
    if('a'<=c .and. c <='z') MemOrd(i:i)=achar(iachar(c)+lower_to_upper)
  enddo
  return
end subroutine UpperCase

subroutine netcdf_err(err,Status)
  use wrf_data_pnc
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer  ,intent(in)  :: err
  integer  ,intent(out) :: Status
  character(len=80)     :: errmsg
  integer               :: stat

  if( err==NF_NOERR )then
    Status = WRF_NO_ERR
  else
    errmsg = NFMPI_STRERROR(err) 
    write(msg,*) 'NetCDF error: ',errmsg
    call wrf_debug ( WARN , TRIM(msg))
    Status = WRF_WARN_NETCDF
  endif
  return
end subroutine netcdf_err

subroutine FieldIO(IO,DataHandle,DateStr,Starts,Length,MemoryOrder &
                     ,FieldType,NCID,VarID,XField,Status)
  use wrf_data_pnc
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  character (*)              ,intent(in)    :: IO
  integer                    ,intent(in)    :: DataHandle
  character*(*)              ,intent(in)    :: DateStr
  integer,dimension(NVarDims),intent(in)    :: Starts
  integer,dimension(NVarDims),intent(in)    :: Length
  character*(*)              ,intent(in)    :: MemoryOrder
  integer                    ,intent(in)    :: FieldType
  integer                    ,intent(in)    :: NCID
  integer                    ,intent(in)    :: VarID
  integer,dimension(*)       ,intent(inout) :: XField
  integer                    ,intent(out)   :: Status
  integer                                   :: TimeIndex
  integer                                   :: NDim
  integer,dimension(NVarDims)               :: VStart
  integer,dimension(NVarDims)               :: VCount

  call GetTimeIndex(IO,DataHandle,DateStr,TimeIndex,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    write(msg,*) '  Bad time index for DateStr = ',DateStr
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call GetDim(MemoryOrder,NDim,Status)
VStart(:) = 1
VCount(:) = 1
!jm for parallel netcef  VStart(1:NDim) = 1
  VStart(1:NDim) = Starts(1:NDim)
  VCount(1:NDim) = Length(1:NDim)
  VStart(NDim+1) = TimeIndex
  VCount(NDim+1) = 1
  select case (FieldType)
    case (WRF_REAL)
      call ext_pnc_RealFieldIO    (WrfDataHandles(DataHandle)%Collective, &
                                   IO,NCID,VarID,VStart,VCount,XField,Status)
    case (WRF_DOUBLE)
      call ext_pnc_DoubleFieldIO  (WrfDataHandles(DataHandle)%Collective, &
                                   IO,NCID,VarID,VStart,VCount,XField,Status)
    case (WRF_INTEGER)
      call ext_pnc_IntFieldIO     (WrfDataHandles(DataHandle)%Collective, &
                                   IO,NCID,VarID,VStart,VCount,XField,Status)
    case (WRF_LOGICAL)
      call ext_pnc_LogicalFieldIO (WrfDataHandles(DataHandle)%Collective, &
                                   IO,NCID,VarID,VStart,VCount,XField,Status)
      if(Status /= WRF_NO_ERR) return
    case default
!for wrf_complex, double_complex
      Status = WRF_WARN_DATA_TYPE_NOT_FOUND
      write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
  end select
  return
end subroutine FieldIO

subroutine Transpose(IO,MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
                                      ,XField,x1,x2,y1,y2,z1,z2 &
                                             ,i1,i2,j1,j2,k1,k2 )
  character*(*)     ,intent(in)    :: IO
  character*(*)     ,intent(in)    :: MemoryOrder
  integer           ,intent(in)    :: l1,l2,m1,m2,n1,n2
  integer           ,intent(in)    :: di
  integer           ,intent(in)    :: x1,x2,y1,y2,z1,z2
  integer           ,intent(in)    :: i1,i2,j1,j2,k1,k2
  integer           ,intent(inout) ::  Field(di,l1:l2,m1:m2,n1:n2)
!jm 010827  integer           ,intent(inout) :: XField(di,x1:x2,y1:y2,z1:z2)
  integer           ,intent(inout) :: XField(di,(i2-i1+1)*(j2-j1+1)*(k2-k1+1))
  character*3                      :: MemOrd
  character*3                      :: MemO
  integer           ,parameter     :: MaxUpperCase=IACHAR('Z')
  integer                          :: i,j,k,ix,jx,kx

  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)

!#define XDEX(A,B,C) A-A ## 1+1+(A ## 2-A ## 1+1)*((B-B ## 1)+(C-C ## 1)*(B ## 2-B ## 1+1))
! define(`XDEX',($1-``$1''1+1+(``$1''2-``$1''1+1)*(($2-``$2''1)+($3-``$3''1)*(``$2''2-``$2''1+1))))

    case ('xzy')
#undef  DFIELD
#define DFIELD XField(1:di,XDEX(i,k,j))
#include "transpose.code"
    case ('xyz','xsz','xez','ysz','yez','xy','xs','xe','ys','ye','z','c','0')
#undef  DFIELD
#define DFIELD XField(1:di,XDEX(i,j,k))
#include "transpose.code"
    case ('yxz')
#undef  DFIELD
#define DFIELD XField(1:di,XDEX(j,i,k))
#include "transpose.code"
    case ('zxy')
#undef  DFIELD
#define DFIELD XField(1:di,XDEX(k,i,j))
#include "transpose.code"
    case ('yzx')
#undef  DFIELD
#define DFIELD XField(1:di,XDEX(j,k,i))
#include "transpose.code"
    case ('zyx')
#undef  DFIELD
#define DFIELD XField(1:di,XDEX(k,j,i))
#include "transpose.code"
    case ('yx')
#undef  DFIELD
#define DFIELD XField(1:di,XDEX(j,i,k))
#include "transpose.code"
  end select
  return
end subroutine Transpose

subroutine reorder (MemoryOrder,MemO)
  character*(*)     ,intent(in)    :: MemoryOrder
  character*3       ,intent(out)   :: MemO
  character*3                      :: MemOrd
  integer                          :: N,i,i1,i2,i3

  MemO = MemoryOrder
  N = len_trim(MemoryOrder)
  if(N == 1) return
  call lowercase(MemoryOrder,MemOrd)
! never invert the boundary codes
  select case ( MemOrd )
     case ( 'xsz','xez','ysz','yez' )
       return
     case default
       continue
  end select
  i1 = 1
  i3 = 1
  do i=2,N
    if(ichar(MemOrd(i:i)) < ichar(MemOrd(i1:i1))) I1 = i
    if(ichar(MemOrd(i:i)) > ichar(MemOrd(i3:i3))) I3 = i
  enddo
  if(N == 2) then
    i2=i3
  else
    i2 = 6-i1-i3
  endif
  MemO(1:1) = MemoryOrder(i1:i1)
  MemO(2:2) = MemoryOrder(i2:i2)
  if(N == 3) MemO(3:3) = MemoryOrder(i3:i3)
  if(MemOrd(i1:i1) == 's' .or. MemOrd(i1:i1) == 'e') then
    MemO(1:N-1) = MemO(2:N)
    MemO(N:N  ) = MemoryOrder(i1:i1)
  endif
  return
end subroutine reorder
  
! Returns .TRUE. iff it is OK to write time-independent domain metadata to the 
! file referenced by DataHandle.  If DataHandle is invalid, .FALSE. is 
! returned.  
LOGICAL FUNCTION ncd_ok_to_put_dom_ti( DataHandle )
    USE wrf_data_pnc
    include 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle 
    CHARACTER*80 :: fname
    INTEGER :: filestate
    INTEGER :: Status
    LOGICAL :: dryrun, first_output, retval
    call ext_pnc_inquire_filename( DataHandle, fname, filestate, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
                   ', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      dryrun       = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
      first_output = ncd_is_first_operation( DataHandle )
!      retval = .NOT. dryrun .AND. first_output
      retval = dryrun
    ENDIF
    ncd_ok_to_put_dom_ti = retval
    RETURN
END FUNCTION ncd_ok_to_put_dom_ti

! Returns .TRUE. iff it is OK to read time-independent domain metadata from the 
! file referenced by DataHandle.  If DataHandle is invalid, .FALSE. is 
! returned.  
LOGICAL FUNCTION ncd_ok_to_get_dom_ti( DataHandle )
    USE wrf_data_pnc
    include 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle 
    CHARACTER*80 :: fname
    INTEGER :: filestate
    INTEGER :: Status
    LOGICAL :: dryrun, retval
    call ext_pnc_inquire_filename( DataHandle, fname, filestate, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
                   ', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      dryrun       = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
      retval = .NOT. dryrun
    ENDIF
    ncd_ok_to_get_dom_ti = retval
    RETURN
END FUNCTION ncd_ok_to_get_dom_ti

! Returns .TRUE. iff nothing has been read from or written to the file 
! referenced by DataHandle.  If DataHandle is invalid, .FALSE. is returned.  
LOGICAL FUNCTION ncd_is_first_operation( DataHandle )
    USE wrf_data_pnc
    INCLUDE 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle 
    TYPE(wrf_data_handle) ,POINTER :: DH
    INTEGER :: Status
    LOGICAL :: retval
    CALL GetDH( DataHandle, DH, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
                   ', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      retval = DH%first_operation
    ENDIF
    ncd_is_first_operation = retval
    RETURN
END FUNCTION ncd_is_first_operation

end module ext_pnc_support_routines

subroutine ext_pnc_open_for_read(DatasetName, Comm1, Comm2, SysDepInfo, DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  character *(*), INTENT(IN)   :: DatasetName
  integer       , INTENT(IN)   :: Comm1, Comm2
  character *(*), INTENT(IN)   :: SysDepInfo
  integer       , INTENT(OUT)  :: DataHandle
  integer       , INTENT(OUT)  :: Status
  DataHandle = 0   ! dummy setting to quiet warning message
  CALL ext_pnc_open_for_read_begin( DatasetName, Comm1, Comm2, SysDepInfo, DataHandle, Status )
  IF ( Status .EQ. WRF_NO_ERR ) THEN
    CALL ext_pnc_open_for_read_commit( DataHandle, Status )
  ENDIF
  return
end subroutine ext_pnc_open_for_read

!ends training phase; switches internal flag to enable input
!must be paired with call to ext_pnc_open_for_read_begin
subroutine ext_pnc_open_for_read_commit(DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer, intent(in) :: DataHandle
  integer, intent(out) :: Status
  type(wrf_data_handle) ,pointer         :: DH

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED
    write(msg,*) 'ext_pnc_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%FileStatus      = WRF_FILE_OPENED_FOR_READ
  DH%first_operation  = .TRUE.
  Status = WRF_NO_ERR
  return
end subroutine ext_pnc_open_for_read_commit

subroutine ext_pnc_open_for_read_begin( FileName, Comm, IOComm, SysDepInfo, DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  character*(*)         ,intent(IN)      :: FileName
  integer               ,intent(IN)      :: Comm
  integer               ,intent(IN)      :: IOComm
  character*(*)         ,intent(in)      :: SysDepInfo
  integer               ,intent(out)     :: DataHandle
  integer               ,intent(out)     :: Status
  type(wrf_data_handle) ,pointer         :: DH
  integer                                :: XType
  integer                                :: stat
  integer               ,allocatable     :: Buffer(:)
  integer                                :: VarID
  integer                                :: StoredDim
  integer                                :: NAtts
  integer                                :: DimIDs(2)
  integer(KIND=MPI_OFFSET_KIND)          :: VStart(2)
  integer(KIND=MPI_OFFSET_KIND)          :: VLen(2)
  integer                                :: TotalNumVars
  integer                                :: NumVars
  integer                                :: i
  character (NF_MAX_NAME)                :: Name

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_pnc_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Comm,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_OPEN(Comm, FileName, NF_NOWRITE, MPI_INFO_NULL, DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_INQ_VARID(DH%NCID,DH%TimesName,VarID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_INQ_VAR(DH%NCID,VarID,DH%TimesName, XType, StoredDim, DimIDs, NAtts)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(XType/=NF_CHAR) then
    Status = WRF_WARN_TYPE_MISMATCH
    write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_INQ_DIMLEN(DH%NCID,DimIDs(1),VLen(1))  
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(VLen(1) /= DateStrLen) then
    Status = WRF_WARN_DATESTR_BAD_LENGTH
    write(msg,*) 'Warning DATESTR BAD LENGTH in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_INQ_DIMLEN(DH%NCID,DimIDs(2),VLen(2))
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(VLen(2) > MaxTimes) then
    Status = WRF_ERR_FATAL_TOO_MANY_TIMES
    write(msg,*) 'Fatal TOO MANY TIME VALUES in ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  VStart(1) = 1
  VStart(2) = 1
  stat = NFMPI_GET_VARA_TEXT_ALL(DH%NCID,VarID,VStart,VLen,DH%Times)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_INQ_NVARS(DH%NCID,TotalNumVars)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  NumVars = 0
  do i=1,TotalNumVars
    stat = NFMPI_INQ_VARNAME(DH%NCID,i,Name)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    elseif(Name(1:5) /= 'md___' .and. Name /= DH%TimesName) then
      NumVars              = NumVars+1
      DH%VarNames(NumVars) = Name
      DH%VarIDs(NumVars)   = i
    endif      
  enddo
  DH%NumVars         = NumVars
  DH%NumberTimes     = VLen(2)
  DH%FileStatus      = WRF_FILE_OPENED_NOT_COMMITTED
  DH%FileName        = FileName
  DH%CurrentVariable = 0
  DH%CurrentTime     = 0
  DH%TimesVarID      = VarID
  DH%TimeIndex       = 0
  return
end subroutine ext_pnc_open_for_read_begin

subroutine ext_pnc_open_for_update( FileName, Comm, IOComm, SysDepInfo, DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  character*(*)         ,intent(IN)      :: FileName
  integer               ,intent(IN)      :: Comm
  integer               ,intent(IN)      :: IOComm
  character*(*)         ,intent(in)      :: SysDepInfo
  integer               ,intent(out)     :: DataHandle
  integer               ,intent(out)     :: Status
  type(wrf_data_handle) ,pointer         :: DH
  integer                                :: XType
  integer                                :: stat
  integer               ,allocatable     :: Buffer(:)
  integer                                :: VarID
  integer                                :: StoredDim
  integer                                :: NAtts
  integer                                :: DimIDs(2)
  integer                                :: VStart(2)
  integer                                :: VLen(2)
  integer                                :: TotalNumVars
  integer                                :: NumVars
  integer                                :: i
  character (NF_MAX_NAME)                :: Name

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_pnc_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Comm,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_OPEN(Comm, FileName, NF_WRITE, MPI_INFO_NULL, DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_INQ_VARID(DH%NCID,DH%TimesName,VarID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_INQ_VAR(DH%NCID,VarID,DH%TimesName, XType, StoredDim, DimIDs, NAtts)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(XType/=NF_CHAR) then
    Status = WRF_WARN_TYPE_MISMATCH
    write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_INQ_DIMLEN(DH%NCID,DimIDs(1),VLen(1))  
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(VLen(1) /= DateStrLen) then
    Status = WRF_WARN_DATESTR_BAD_LENGTH
    write(msg,*) 'Warning DATESTR BAD LENGTH in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_INQ_DIMLEN(DH%NCID,DimIDs(2),VLen(2))
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(VLen(2) > MaxTimes) then
    Status = WRF_ERR_FATAL_TOO_MANY_TIMES
    write(msg,*) 'Fatal TOO MANY TIME VALUES in ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  VStart(1) = 1
  VStart(2) = 1
  stat = NFMPI_GET_VARA_TEXT_ALL(DH%NCID,VarID,VStart,VLen,DH%Times)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = NFMPI_INQ_NVARS(DH%NCID,TotalNumVars)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  NumVars = 0
  do i=1,TotalNumVars
    stat = NFMPI_INQ_VARNAME(DH%NCID,i,Name)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    elseif(Name(1:5) /= 'md___' .and. Name /= DH%TimesName) then
      NumVars              = NumVars+1
      DH%VarNames(NumVars) = Name
      DH%VarIDs(NumVars)   = i
    endif      
  enddo
  DH%NumVars         = NumVars
  DH%NumberTimes     = VLen(2)
  DH%FileStatus      = WRF_FILE_OPENED_FOR_UPDATE
  DH%FileName        = FileName
  DH%CurrentVariable = 0
  DH%CurrentTime     = 0
  DH%TimesVarID      = VarID
  DH%TimeIndex       = 0
  return
end subroutine ext_pnc_open_for_update


SUBROUTINE ext_pnc_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,DataHandle,Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  character*(*)        ,intent(in)  :: FileName
  integer              ,intent(in)  :: Comm
  integer              ,intent(in)  :: IOComm
  character*(*)        ,intent(in)  :: SysDepInfo
  integer              ,intent(out) :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: i
  integer                           :: stat
  character (7)                     :: Buffer
  integer                           :: VDimIDs(2)
  integer                           :: info, ierr   ! added for Blue Gene (see NF_CREAT below)
  character*1024                    :: newFileName
  integer                           :: gridid
  integer local_communicator_x, ntasks_x

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_pnc_open_for_write_begin: ext_pnc_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Comm,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ext_pnc_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  DH%TimeIndex = 0
  DH%Times     = ZeroDate

#ifndef BLUEGENE
  call mpi_info_create( info, ierr )
# ifdef LUSTRE_FS
  CALL mpi_info_set(info,"romio_ds_write","disable", ierr) ; write(0,*)'mpi_info_set write returns ',ierr
  CALL mpi_info_set(info,"romio_ds_read","disable", ierr) ; write(0,*)'mpi_info_set read returns ',ierr
# endif


! Remove the dash/underscore change to filenames for pnetcdf...
  write(newFileName, fmt="(a)") TRIM(ADJUSTL(FileName))
  do i = 1, len_trim(newFileName)
!     if(newFileName(i:i) == '-') newFileName(i:i) = '_'
     if(newFileName(i:i) == ':') newFileName(i:i) = '_'
  enddo
  stat = NFMPI_CREATE(Comm, newFileName, IOR(NF_CLOBBER, NF_64BIT_OFFSET), info, DH%NCID)
! stat = NFMPI_CREATE(Comm, newFileName, NF_64BIT_OFFSET, info, DH%NCID)
  call mpi_info_free( info, ierr)
#else
!!!!!!!!!!!!!!!
! rob latham suggested hint

  call mpi_info_create( info, ierr )
!  call mpi_info_set(info,'cd_buffer_size','4194304',ierr)
  call mpi_info_set(info,'cd_buffer_size','8388608',ierr)
  stat = NFMPI_CREATE(Comm, FileName, IOR(NF_CLOBBER, NF_64BIT_OFFSET), info, DH%NCID)
  call mpi_info_free( info, ierr)
!
!!!!!!!!!!!!!!! 
#endif

  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_pnc_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  ! JPE added for performance
  stat = NFMPI_SET_FILL(DH%NCID, NF_NOFILL, i)

  DH%FileStatus  = WRF_FILE_OPENED_NOT_COMMITTED
  DH%FileName    = FileName
  stat = NFMPI_DEF_DIM(DH%NCID,DH%DimUnlimName,i2offset(NF_UNLIMITED),DH%DimUnlimID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_pnc_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%VarNames  (1:MaxVars) = NO_NAME
  DH%MDVarNames(1:MaxVars) = NO_NAME
  do i=1,MaxDims
    write(Buffer,FMT="('DIM',i4.4)") i
    DH%DimNames  (i) = Buffer
    DH%DimLengths(i) = NO_DIM
  enddo
  DH%DimNames(1) = 'DateStrLen'
  stat = NFMPI_DEF_DIM(DH%NCID,DH%DimNames(1),i2offset(DateStrLen),DH%DimIDs(1))
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_pnc_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  VDimIDs(1) = DH%DimIDs(1)
  VDimIDs(2) = DH%DimUnlimID
  stat = NFMPI_DEF_VAR(DH%NCID,DH%TimesName,NF_CHAR,2,VDimIDs,DH%TimesVarID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_pnc_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%DimLengths(1) = DateStrLen
  return
end subroutine ext_pnc_open_for_write_begin

!stub
!opens a file for writing or coupler datastream for sending messages.
!no training phase for this version of the open stmt.
subroutine ext_pnc_open_for_write (DatasetName, Comm1, Comm2, &
                                   SysDepInfo, DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  character *(*), intent(in)  ::DatasetName
  integer       , intent(in)  ::Comm1, Comm2
  character *(*), intent(in)  ::SysDepInfo
  integer       , intent(out) :: DataHandle
  integer       , intent(out) :: Status
  Status=WRF_WARN_NOOP
  DataHandle = 0    ! dummy setting to quiet warning message
  return
end subroutine ext_pnc_open_for_write

SUBROUTINE ext_pnc_open_for_write_commit(DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: i
  integer                           :: stat

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_pnc_open_for_write_commit: ext_pnc_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_pnc_open_for_write_commit ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg)) 
    return
  endif
  stat = NFMPI_ENDDEF(DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error (',stat,') from NFMPI_ENDDEF in ext_pnc_open_for_write_commit ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%FileStatus  = WRF_FILE_OPENED_FOR_WRITE
  DH%first_operation  = .TRUE.
  return
end subroutine ext_pnc_open_for_write_commit

subroutine ext_pnc_ioclose(DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: stat

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_pnc_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ext_pnc_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_CLOSE
    write(msg,*) 'Warning TRY TO CLOSE DRYRUN in ext_pnc_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue    
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    continue
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ext_pnc_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif

  stat = NFMPI_CLOSE(DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_pnc_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  CALL deallocHandle( DataHandle, Status )
  DH%Free=.true.
  return
end subroutine ext_pnc_ioclose

subroutine ext_pnc_iosync( DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: stat

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_pnc_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ext_pnc_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_FILE_NOT_COMMITTED
    write(msg,*) 'Warning FILE NOT COMMITTED in ext_pnc_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    continue
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ext_pnc_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  stat = NFMPI_SYNC(DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_pnc_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  return
end subroutine ext_pnc_iosync



subroutine ext_pnc_redef( DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: stat

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_FILE_NOT_COMMITTED
    write(msg,*) 'Warning FILE NOT COMMITTED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_FILE_OPEN_FOR_READ
    write(msg,*) 'Warning FILE OPEN FOR READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  stat = NFMPI_REDEF(DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%FileStatus  = WRF_FILE_OPENED_NOT_COMMITTED
  return
end subroutine ext_pnc_redef

subroutine ext_pnc_enddef( DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: stat

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_FILE_NOT_COMMITTED
    write(msg,*) 'Warning FILE NOT COMMITTED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_FILE_OPEN_FOR_READ
    write(msg,*) 'Warning FILE OPEN FOR READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  stat = NFMPI_ENDDEF(DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%FileStatus  = WRF_FILE_OPENED_FOR_WRITE
  return
end subroutine ext_pnc_enddef

subroutine ext_pnc_ioinit(SysDepInfo, Status)
  use wrf_data_pnc
  implicit none
  include 'wrf_status_codes.h'
  CHARACTER*(*), INTENT(IN) :: SysDepInfo
  INTEGER ,INTENT(INOUT)    :: Status

  WrfIOnotInitialized                             = .false.
  WrfDataHandles(1:WrfDataHandleMax)%Free         = .true.
  WrfDataHandles(1:WrfDataHandleMax)%TimesName    = 'Times'
  WrfDataHandles(1:WrfDataHandleMax)%DimUnlimName = 'Time'
  WrfDataHandles(1:WrfDataHandleMax)%FileStatus   = WRF_FILE_NOT_OPENED
  Status = WRF_NO_ERR
  return
end subroutine ext_pnc_ioinit


subroutine ext_pnc_inquiry (Inquiry, Result, Status)
  use wrf_data_pnc
  implicit none
  include 'wrf_status_codes.h'
  character *(*), INTENT(IN)    :: Inquiry
  character *(*), INTENT(OUT)   :: Result
  integer        ,INTENT(INOUT) :: Status
  SELECT CASE (Inquiry)
  CASE ("RANDOM_WRITE","RANDOM_READ","SEQUENTIAL_WRITE","SEQUENTIAL_READ")
        Result='ALLOW'
  CASE ("OPEN_READ","OPEN_COMMIT_WRITE")
        Result='REQUIRE'
  CASE ("OPEN_WRITE","OPEN_COMMIT_READ","PARALLEL_IO")
        Result='NO'
  CASE ("SELF_DESCRIBING","SUPPORT_METADATA","SUPPORT_3D_FIELDS")
        Result='YES'
  CASE ("MEDIUM")
        Result ='FILE'
  CASE DEFAULT
      Result = 'No Result for that inquiry!'
  END SELECT
  Status=WRF_NO_ERR
  return
end subroutine ext_pnc_inquiry




subroutine ext_pnc_ioexit(Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer       , INTENT(INOUT)     ::Status
  integer                           :: error
  type(wrf_data_handle),pointer     :: DH
  integer                           :: i
  integer                           :: stat
  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_pnc_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  do i=1,WrfDataHandleMax
    CALL deallocHandle( i , stat ) 
  enddo
  return
end subroutine ext_pnc_ioexit

subroutine ext_pnc_get_dom_ti_real(DataHandle,Element,Data,Count,OutCount,Status)
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA real,intent(out) :: Data(*)
#define TYPE_COUNT integer,intent(in) :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCOunt
#define TYPE_BUFFER  real,allocatable :: Buffer(:)
#define NF_TYPE NF_FLOAT
#define NF_ROUTINE NFMPI_GET_ATT_REAL 
#define COPY   Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
#include "ext_pnc_get_dom_ti.code"
end subroutine ext_pnc_get_dom_ti_real

subroutine ext_pnc_get_dom_ti_integer(DataHandle,Element,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA 
#undef TYPE_BUFFER
#undef NF_TYPE
#undef NF_ROUTINE
#undef COPY
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA integer,intent(out) :: Data(*)
#define TYPE_BUFFER  integer,allocatable :: Buffer(:)
#define NF_TYPE NF_INT
#define NF_ROUTINE NFMPI_GET_ATT_INT
#define COPY   Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
#include "ext_pnc_get_dom_ti.code"
end subroutine ext_pnc_get_dom_ti_integer

subroutine ext_pnc_get_dom_ti_double(DataHandle,Element,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA 
#undef TYPE_BUFFER
#undef NF_TYPE
#undef NF_ROUTINE
#undef COPY
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA real*8,intent(out) :: Data(*)
#define TYPE_BUFFER  real*8,allocatable :: Buffer(:)
#define NF_TYPE NF_DOUBLE
#define NF_ROUTINE NFMPI_GET_ATT_DOUBLE
#define COPY   Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
#include "ext_pnc_get_dom_ti.code"
end subroutine ext_pnc_get_dom_ti_double

subroutine ext_pnc_get_dom_ti_logical(DataHandle,Element,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA 
#undef TYPE_BUFFER
#undef NF_TYPE
#undef NF_ROUTINE
#undef COPY
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA logical,intent(out) :: Data(*)
#define TYPE_BUFFER  integer,allocatable :: Buffer(:)
#define NF_TYPE NF_INT
#define NF_ROUTINE NFMPI_GET_ATT_INT
#define COPY   Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))==1
#include "ext_pnc_get_dom_ti.code"
end subroutine ext_pnc_get_dom_ti_logical

subroutine ext_pnc_get_dom_ti_char(DataHandle,Element,Data,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef TYPE_BUFFER
#undef NF_TYPE
#define ROUTINE_TYPE 'CHAR'
#define TYPE_DATA character*(*),intent(out) :: Data
#define TYPE_COUNT
#define TYPE_OUTCOUNT
#define TYPE_BUFFER
#define NF_TYPE NF_CHAR
#define CHAR_TYPE
#include "ext_pnc_get_dom_ti.code"
#undef CHAR_TYPE
end subroutine ext_pnc_get_dom_ti_char

subroutine ext_pnc_put_dom_ti_real(DataHandle,Element,Data,Count,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA 
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef ARGS
#undef LOG
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA  real   ,intent(in) :: Data(*)
#define TYPE_COUNT integer,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_ATT_REAL
#define ARGS NF_FLOAT,i2offset(Count),Data
#include "ext_pnc_put_dom_ti.code"
end subroutine ext_pnc_put_dom_ti_real

subroutine ext_pnc_put_dom_ti_integer(DataHandle,Element,Data,Count,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef ARGS
#undef LOG
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA  integer,intent(in) :: Data(*)
#define TYPE_COUNT integer,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_ATT_INT
#define ARGS NF_INT,i2offset(Count),Data
#include "ext_pnc_put_dom_ti.code"
end subroutine ext_pnc_put_dom_ti_integer

subroutine ext_pnc_put_dom_ti_double(DataHandle,Element,Data,Count,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef ARGS
#undef LOG
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA  real*8 ,intent(in) :: Data(*)
#define TYPE_COUNT integer,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_ATT_DOUBLE
#define ARGS NF_DOUBLE,i2offset(Count),Data
#include "ext_pnc_put_dom_ti.code"
end subroutine ext_pnc_put_dom_ti_double

subroutine ext_pnc_put_dom_ti_logical(DataHandle,Element,Data,Count,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef ARGS
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA  logical,intent(in) :: Data(*)
#define TYPE_COUNT integer,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_ATT_INT
#define ARGS NF_INT,i2offset(Count),Buffer
#define LOG
#include "ext_pnc_put_dom_ti.code"
end subroutine ext_pnc_put_dom_ti_logical

subroutine ext_pnc_put_dom_ti_char(DataHandle,Element,Data,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef ARGS
#undef LOG
#define ROUTINE_TYPE 'CHAR'
#define TYPE_DATA  character*(*),intent(in) :: Data
#define TYPE_COUNT integer,parameter :: Count=1
#define NF_ROUTINE NFMPI_PUT_ATT_TEXT
#define ARGS i2offset(len_trim(Data)),Data
#include "ext_pnc_put_dom_ti.code"
end subroutine ext_pnc_put_dom_ti_char

subroutine ext_pnc_put_var_ti_real(DataHandle,Element,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef ARGS
#undef LOG
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA  real    ,intent(in) :: Data(*)
#define TYPE_COUNT integer ,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_ATT_REAL
#define ARGS NF_FLOAT,i2offset(Count),Data
#include "ext_pnc_put_var_ti.code"
end subroutine ext_pnc_put_var_ti_real

subroutine ext_pnc_put_var_td_real(DataHandle,Element,DateStr,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef NF_TYPE
#undef LENGTH
#undef ARG
#undef LOG
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA  real    ,intent(in) :: Data(*)
#define TYPE_COUNT integer ,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_VARA_REAL_ALL
#define NF_TYPE NF_FLOAT
#define LENGTH Count
#define ARG 
#include "ext_pnc_put_var_td.code"
end subroutine ext_pnc_put_var_td_real

subroutine ext_pnc_put_var_ti_double(DataHandle,Element,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef ARGS
#undef LOG
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA  real*8 ,intent(in) :: Data(*)
#define TYPE_COUNT integer ,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_ATT_DOUBLE
#define ARGS NF_DOUBLE,i2offset(Count),Data
#include "ext_pnc_put_var_ti.code"
end subroutine ext_pnc_put_var_ti_double

subroutine ext_pnc_put_var_td_double(DataHandle,Element,DateStr,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef NF_TYPE
#undef LENGTH
#undef ARG
#undef LOG
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA  real*8,intent(in) :: Data(*)
#define TYPE_COUNT integer ,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_VARA_DOUBLE_ALL
#define NF_TYPE NF_DOUBLE
#define LENGTH Count
#define ARG 
#include "ext_pnc_put_var_td.code"
end subroutine ext_pnc_put_var_td_double

subroutine ext_pnc_put_var_ti_integer(DataHandle,Element,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef ARGS
#undef LOG
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA  integer ,intent(in) :: Data(*)
#define TYPE_COUNT integer ,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_ATT_INT
#define ARGS NF_INT,i2offset(Count),Data 
#include "ext_pnc_put_var_ti.code"
end subroutine ext_pnc_put_var_ti_integer

subroutine ext_pnc_put_var_td_integer(DataHandle,Element,DateStr,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef NF_TYPE
#undef LENGTH
#undef ARG
#undef LOG
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA  integer ,intent(in) :: Data(*)
#define TYPE_COUNT integer ,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_VARA_INT_ALL
#define NF_TYPE NF_INT
#define LENGTH Count
#define ARG 
#include "ext_pnc_put_var_td.code"
end subroutine ext_pnc_put_var_td_integer

subroutine ext_pnc_put_var_ti_logical(DataHandle,Element,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef ARGS 
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA  logical ,intent(in) :: Data(*)
#define TYPE_COUNT integer ,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_ATT_INT
#define LOG
#define ARGS NF_INT,i2offset(Count),Buffer
#include "ext_pnc_put_var_ti.code"
end subroutine ext_pnc_put_var_ti_logical

subroutine ext_pnc_put_var_td_logical(DataHandle,Element,DateStr,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef NF_TYPE
#undef LENGTH
#undef ARG
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA  logical ,intent(in) :: Data(*)
#define TYPE_COUNT integer ,intent(in) :: Count
#define NF_ROUTINE NFMPI_PUT_VARA_INT_ALL
#define NF_TYPE NF_INT
#define LOG
#define LENGTH Count
#define ARG 
#include "ext_pnc_put_var_td.code"
end subroutine ext_pnc_put_var_td_logical

subroutine ext_pnc_put_var_ti_char(DataHandle,Element,Var,Data,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef ARGS
#undef LOG
#define ROUTINE_TYPE 'CHAR'
#define TYPE_DATA  character*(*) ,intent(in) :: Data
#define TYPE_COUNT 
#define NF_ROUTINE NFMPI_PUT_ATT_TEXT
#define ARGS i2offset(len_trim(Data)),trim(Data)
#define CHAR_TYPE
#include "ext_pnc_put_var_ti.code"
#undef CHAR_TYPE
end subroutine ext_pnc_put_var_ti_char

subroutine ext_pnc_put_var_td_char(DataHandle,Element,DateStr,Var,Data,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef NF_ROUTINE
#undef NF_TYPE
#undef LENGTH
#undef ARG
#undef LOG
#define ROUTINE_TYPE 'CHAR'
#define TYPE_DATA  character*(*) ,intent(in) :: Data
#define TYPE_COUNT 
#define NF_ROUTINE NFMPI_PUT_VARA_TEXT_ALL
#define NF_TYPE NF_CHAR
#define LENGTH len(Data)
#include "ext_pnc_put_var_td.code"
end subroutine ext_pnc_put_var_td_char

subroutine ext_pnc_get_var_ti_real(DataHandle,Element,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef NF_TYPE
#undef NF_ROUTINE
#undef COPY
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA     real   ,intent(out) :: Data(*)
#define TYPE_BUFFER   real   ,allocatable :: Buffer(:)
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define NF_TYPE NF_FLOAT
#define NF_ROUTINE NFMPI_GET_ATT_REAL
#define COPY   Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
#include "ext_pnc_get_var_ti.code"
end subroutine ext_pnc_get_var_ti_real

subroutine ext_pnc_get_var_td_real(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef NF_TYPE
#undef NF_ROUTINE
#undef LENGTH
#undef COPY
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA     real   ,intent(out) :: Data(*)
#define TYPE_BUFFER real
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define NF_TYPE NF_FLOAT
#define NF_ROUTINE NFMPI_GET_VARA_REAL_ALL
#define LENGTH min(Count,Len1)
#define COPY   Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
#include "ext_pnc_get_var_td.code"
end subroutine ext_pnc_get_var_td_real

subroutine ext_pnc_get_var_ti_double(DataHandle,Element,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef NF_TYPE
#undef NF_ROUTINE
#undef COPY
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA     real*8 ,intent(out) :: Data(*)
#define TYPE_BUFFER   real*8 ,allocatable :: Buffer(:)
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define NF_TYPE NF_DOUBLE
#define NF_ROUTINE NFMPI_GET_ATT_DOUBLE
#define COPY   Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
#include "ext_pnc_get_var_ti.code"
end subroutine ext_pnc_get_var_ti_double

subroutine ext_pnc_get_var_td_double(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef NF_TYPE
#undef NF_ROUTINE
#undef LENGTH
#undef COPY
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA     real*8 ,intent(out) :: Data(*)
#define TYPE_BUFFER real*8
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define NF_TYPE NF_DOUBLE
#define NF_ROUTINE NFMPI_GET_VARA_DOUBLE_ALL
#define LENGTH min(Count,Len1)
#define COPY   Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
#include "ext_pnc_get_var_td.code"
end subroutine ext_pnc_get_var_td_double

subroutine ext_pnc_get_var_ti_integer(DataHandle,Element,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef NF_TYPE
#undef NF_ROUTINE
#undef COPY
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA     integer,intent(out) :: Data(*)
#define TYPE_BUFFER   integer,allocatable :: Buffer(:)
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define NF_TYPE NF_INT
#define NF_ROUTINE NFMPI_GET_ATT_INT
#define COPY   Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
#include "ext_pnc_get_var_ti.code"
end subroutine ext_pnc_get_var_ti_integer

subroutine ext_pnc_get_var_td_integer(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef NF_TYPE
#undef NF_ROUTINE
#undef LENGTH
#undef COPY
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA     integer,intent(out) :: Data(*)
#define TYPE_BUFFER integer
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define NF_TYPE NF_INT
#define NF_ROUTINE NFMPI_GET_VARA_INT_ALL
#define LENGTH min(Count,Len1)
#define COPY   Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
#include "ext_pnc_get_var_td.code"
end subroutine ext_pnc_get_var_td_integer

subroutine ext_pnc_get_var_ti_logical(DataHandle,Element,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef NF_TYPE
#undef NF_ROUTINE
#undef COPY
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA     logical,intent(out) :: Data(*)
#define TYPE_BUFFER   integer,allocatable :: Buffer(:)
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define NF_TYPE NF_INT
#define NF_ROUTINE NFMPI_GET_ATT_INT
#define COPY   Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))==1
#include "ext_pnc_get_var_ti.code"
end subroutine ext_pnc_get_var_ti_logical

subroutine ext_pnc_get_var_td_logical(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef NF_TYPE
#undef NF_ROUTINE
#undef LENGTH
#undef COPY
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA     logical,intent(out) :: Data(*)
#define TYPE_BUFFER   integer
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define NF_TYPE NF_INT
#define NF_ROUTINE NFMPI_GET_VARA_INT_ALL
#define LENGTH min(Count,Len1)
#define COPY   Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))==1
#include "ext_pnc_get_var_td.code"
end subroutine ext_pnc_get_var_td_logical

subroutine ext_pnc_get_var_ti_char(DataHandle,Element,Var,Data,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef NF_TYPE
#undef NF_ROUTINE
#undef COPY
#define ROUTINE_TYPE 'CHAR'
#define TYPE_DATA   character*(*) ,intent(out) :: Data
#define TYPE_BUFFER
#define TYPE_COUNT integer :: Count = 1
#define TYPE_OUTCOUNT
#define NF_TYPE NF_CHAR
#define NF_ROUTINE NFMPI_GET_ATT_TEXT
#define COPY 
#define CHAR_TYPE
#include "ext_pnc_get_var_ti.code"
#undef CHAR_TYPE
end subroutine ext_pnc_get_var_ti_char

subroutine ext_pnc_get_var_td_char(DataHandle,Element,DateStr,Var,Data,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef NF_TYPE
#undef NF_ROUTINE
#undef LENGTH
#define ROUTINE_TYPE 'CHAR'
#define TYPE_DATA character*(*) ,intent(out)    :: Data
#define TYPE_BUFFER character (80)
#define TYPE_COUNT integer :: Count = 1
#define TYPE_OUTCOUNT
#define NF_TYPE NF_CHAR
#define NF_ROUTINE NFMPI_GET_VARA_TEXT_ALL
#define LENGTH Len1
#define CHAR_TYPE
#include "ext_pnc_get_var_td.code"
#undef CHAR_TYPE
end subroutine ext_pnc_get_var_td_char

subroutine ext_pnc_put_dom_td_real(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real                  ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pnc_put_var_td_real(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,Status)
  return
end subroutine ext_pnc_put_dom_td_real

subroutine ext_pnc_put_dom_td_integer(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pnc_put_var_td_integer(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,Status)
  return
end subroutine ext_pnc_put_dom_td_integer

subroutine ext_pnc_put_dom_td_double(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real*8                ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pnc_put_var_td_double(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'   ,Data,Count,Status)
  return
end subroutine ext_pnc_put_dom_td_double

subroutine ext_pnc_put_dom_td_logical(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  logical               ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pnc_put_var_td_logical(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,Status)
  return
end subroutine ext_pnc_put_dom_td_logical

subroutine ext_pnc_put_dom_td_char(DataHandle,Element,DateStr,Data,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Data
  integer               ,intent(out)    :: Status

  call ext_pnc_put_var_td_char(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Status)
  return
end subroutine ext_pnc_put_dom_td_char

subroutine ext_pnc_get_dom_td_real(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real                  ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pnc_get_var_td_real(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,OutCount,Status)
  return
end subroutine ext_pnc_get_dom_td_real

subroutine ext_pnc_get_dom_td_integer(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pnc_get_var_td_integer(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,OutCount,Status)
  return
end subroutine ext_pnc_get_dom_td_integer

subroutine ext_pnc_get_dom_td_double(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real*8                ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pnc_get_var_td_double(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'   ,Data,Count,OutCount,Status)
  return
end subroutine ext_pnc_get_dom_td_double

subroutine ext_pnc_get_dom_td_logical(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  logical               ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pnc_get_var_td_logical(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,OutCount,Status)
  return
end subroutine ext_pnc_get_dom_td_logical

subroutine ext_pnc_get_dom_td_char(DataHandle,Element,DateStr,Data,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(out)    :: Data
  integer               ,intent(out)    :: Status
  call ext_pnc_get_var_td_char(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Status)
  return
end subroutine ext_pnc_get_dom_td_char


subroutine ext_pnc_write_field(DataHandle,DateStr,Var,Field,FieldType,Comm, &
  IOComm, DomainDesc, MemoryOrdIn, Stagger,  DimNames,                      &
  DomainStart,DomainEnd,MemoryStart,MemoryEnd,PatchStart,PatchEnd,Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer                       ,intent(in)    :: DataHandle
  character*(*)                 ,intent(in)    :: DateStr
  character*(*)                 ,intent(in)    :: Var
  integer                       ,intent(inout) :: Field(*)
  integer                       ,intent(in)    :: FieldType
  integer                       ,intent(inout) :: Comm
  integer                       ,intent(inout) :: IOComm
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrdIn
  character*(*)                 ,intent(in)    :: Stagger ! Dummy for now
  character*(*) ,dimension(*)   ,intent(in)    :: DimNames
  integer       ,dimension(*)   ,intent(in)    :: DomainStart, DomainEnd
  integer       ,dimension(*)   ,intent(in)    :: MemoryStart, MemoryEnd
  integer       ,dimension(*)   ,intent(in)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status
  character (3)                                :: MemoryOrder
  type(wrf_data_handle)         ,pointer       :: DH
  integer                                      :: NCID
  integer                                      :: NDim
  character (VarNameLen)                       :: VarName
  character (3)                                :: MemO
  character (3)                                :: UCMemO
  integer                                      :: VarID
  integer      ,dimension(NVarDims)            :: Length_global, Length_native
  integer      ,dimension(NVarDims)            :: Length
  integer      ,dimension(NVarDims)            :: VDimIDs
  character(80),dimension(NVarDims)            :: RODimNames
  integer      ,dimension(NVarDims)            :: StoredStart
  integer      ,dimension(:,:,:,:),allocatable :: XField
  integer                                      :: stat
  integer                                      :: NVar
  integer                                      :: i,j
  integer                                      :: i1,i2,j1,j2,k1,k2
  integer                                      :: x1,x2,y1,y2,z1,z2
  integer                                      :: p1,p2,q1,q2,r1,r2
  integer                                      :: l1,l2,m1,m2,n1,n2
  integer                                      :: XType
  integer                                      :: di
  character (80)                               :: NullName
  logical                                      :: NotFound
  logical                                      :: quilting
  ! Local, possibly adjusted, copies of MemoryStart and MemoryEnd
  integer       ,dimension(NVarDims)           :: lMemoryStart, lMemoryEnd
  MemoryOrder = trim(adjustl(MemoryOrdIn))
  NullName=char(0)
  call GetDim(MemoryOrder,NDim,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning BAD MEMORY ORDER |',MemoryOrder,'| in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR |',DateStr,'| in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  NCID = DH%NCID

  write(msg,*)'ext_pnc_write_field: called for ',TRIM(Var)
  CALL wrf_debug( 100, msg )

!jm 20061024
  Length(1:NDim) = PatchEnd(1:NDim)-PatchStart(1:NDim)+1
  Length_native(1:NDim) = Length(1:NDim)
  Length_global(1:NDim) = DomainEnd(1:NDim)-DomainStart(1:NDim)+1

  call ExtOrder(MemoryOrder,Length,Status)
  call ExtOrder(MemoryOrder,Length_global,Status)

  call ExtOrderStr(MemoryOrder,DimNames,RODimNames,Status)

  ! Magic number to identify call from IO server when doing quilting
  quilting = (MemoryStart(1) == -998899 .AND. MemoryEnd(1) == -998899)
  IF(quilting)THEN
     lMemoryStart(1:NDim) = 1
     lMemoryEnd(1:NDim) = Length(1:NDim)
  ELSE
     lMemoryStart(1:NDim) = MemoryStart(1:NDim)
     lMemoryEnd(1:NDim) = MemoryEnd(1:NDim)
  END IF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == VarName ) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE
        write(msg,*) 'Warning 2 DRYRUNS 1 VARIABLE (',TRIM(VarName),') in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      elseif(DH%VarNames(NVar) == NO_NAME) then
        DH%VarNames(NVar) = VarName
        DH%NumVars        = NVar
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
    do j = 1,NDim
      if(RODimNames(j) == NullName .or. RODimNames(j) == '') then
        do i=1,MaxDims
          if(DH%DimLengths(i) == Length_global(j)) then
            exit
          elseif(DH%DimLengths(i) == NO_DIM) then
            stat = NFMPI_DEF_DIM(NCID,DH%DimNames(i),i2offset(Length_global(j)),DH%DimIDs(i))
            call netcdf_err(stat,Status)
            if(Status /= WRF_NO_ERR) then
              write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
              call wrf_debug ( WARN , TRIM(msg))
              return
            endif
            DH%DimLengths(i) = Length_global(j)
            exit
          elseif(i == MaxDims) then
            Status = WRF_WARN_TOO_MANY_DIMS
            write(msg,*) 'Warning TOO MANY DIMENSIONS (',i,') in (',TRIM(VarName),') ',__FILE__,', line', __LINE__ 
            call wrf_debug ( WARN , TRIM(msg))
            return
          endif
        enddo
      else !look for input name and check if already defined
        NotFound = .true.
        do i=1,MaxDims
          if (DH%DimNames(i) == RODimNames(j)) then
            if (DH%DimLengths(i) == Length_global(j)) then
              NotFound = .false.
              exit
            else
              Status = WRF_WARN_DIMNAME_REDEFINED
              write(msg,*) 'Warning DIM ',i,', NAME ',TRIM(DH%DimNames(i)),' REDEFINED  by var ', &
                           TRIM(Var),' ',DH%DimLengths(i),Length_global(j) ,' in ', __FILE__ ,' line', __LINE__ 
              call wrf_debug ( WARN , TRIM(msg))
              return
            endif
          endif
        enddo
        if (NotFound) then
          do i=1,MaxDims
            if (DH%DimLengths(i) == NO_DIM) then
              DH%DimNames(i) = RODimNames(j)
              stat = NFMPI_DEF_DIM(NCID,DH%DimNames(i),i2offset(Length_global(j)),DH%DimIDs(i))
              call netcdf_err(stat,Status)
              if(Status /= WRF_NO_ERR) then
                write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
                call wrf_debug ( WARN , TRIM(msg))
                return
              endif
              DH%DimLengths(i) = Length_global(j)
              exit
            elseif(i == MaxDims) then
              Status = WRF_WARN_TOO_MANY_DIMS
              write(msg,*) 'Warning TOO MANY DIMENSIONS in ',__FILE__,', line', __LINE__ 
              call wrf_debug ( WARN , TRIM(msg))
              return
            endif
          enddo
        endif
      endif
      VDimIDs(j) = DH%DimIDs(i)
      DH%VarDimLens(j,NVar) = Length_global(j)
    enddo
    VDimIDs(NDim+1) = DH%DimUnlimID
    select case (FieldType)
      case (WRF_REAL)
        XType = NF_FLOAT
      case (WRF_DOUBLE)
        Xtype = NF_DOUBLE
      case (WRF_INTEGER)
        XType = NF_INT
      case (WRF_LOGICAL)
        XType = NF_INT
      case default
        Status = WRF_WARN_DATA_TYPE_NOT_FOUND
        write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
    end select


    stat = NFMPI_DEF_VAR(NCID,VarName,XType,NDim+1,VDimIDs,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_pnc_write_field: NetCDF error for ',TRIM(VarName),' in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    DH%VarIDs(NVar) = VarID
    stat = NFMPI_PUT_ATT_INT(NCID,VarID,'FieldType',NF_INT,i2offset(1),FieldType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_pnc_write_field: NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call reorder(MemoryOrder,MemO)
    call uppercase(MemO,UCMemO)
    stat = NFMPI_PUT_ATT_TEXT(NCID,VarID,'MemoryOrder',i2offset(3),UCMemO)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_pnc_write_field: NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
    VarID = DH%VarIDs(NVar)
    do j=1,NDim
      if(Length_global(j) /= DH%VarDimLens(j,NVar) .AND. DH%FileStatus /= WRF_FILE_OPENED_FOR_UPDATE ) then
        Status = WRF_WARN_WRTLEN_NE_DRRUNLEN
        write(msg,*) 'Warning LENGTH != DRY RUN LENGTH for |',   &
                     VarName,'| dim ',j,' in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        write(msg,*) '   LENGTH ',Length_global(j),' DRY RUN LENGTH ',DH%VarDimLens(j,NVar)
        call wrf_debug ( WARN , TRIM(msg))
        return
!jm 061024      elseif(PatchStart(j) < MemoryStart(j)) then
!jm      elseif(DomainStart(j) < MemoryStart(j)) then
      elseif(PatchStart(j) < lMemoryStart(j)) then
        Status = WRF_WARN_DIMENSION_ERROR
        write(msg,*) 'Warning DIMENSION ERROR for |',VarName,    &
                     '| in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
    StoredStart = 1
    call GetIndices(NDim,lMemoryStart,lMemoryEnd,l1,l2,m1,m2,n1,n2)
    call GetIndices(NDim,StoredStart,Length   ,x1,x2,y1,y2,z1,z2)
    call GetIndices(NDim,StoredStart,Length_native   ,p1,p2,q1,q2,r1,r2)
    call GetIndices(NDim,PatchStart, PatchEnd ,i1,i2,j1,j2,k1,k2)
    di=1
    if(FieldType == WRF_DOUBLE) di=2
    allocate(XField(di,x1:x2,y1:y2,z1:z2), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , TRIM(msg))
      return
    endif

#if 0
    WRITE(msg,*) 'ARPDBG: MemoryStart = ',lMemoryStart(1:NDim)
    CALL wrf_message(msg)
    WRITE(msg,*) 'ARPDBG:  lMemoryEnd = ',lMemoryEnd(1:NDim)
    CALL wrf_message(msg)
    WRITE(msg,*) 'ARPDBG:      Length = ',Length(1:NDim)
    CALL wrf_message(msg)
#endif

    IF(quilting)THEN
       ! Don't pass in PatchStart and PatchEnd here since we want to
       ! take transpose of whole patch of data which has been sent to
       ! the IO server and passed down to us.
       ! JM: the field and patch dimensions must be reordered or xpose is a noop
       call Transpose('write',MemoryOrder,di, Field,p1,p2,q1,q2,r1,r2 &
                                            ,XField,x1,x2,y1,y2,z1,z2 &
                                                   ,p1,p2,q1,q2,r1,r2 )
    ELSE
       call Transpose('write',MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
                                            ,XField,x1,x2,y1,y2,z1,z2 &
                                                   ,i1,i2,j1,j2,k1,k2 )
    END IF

    StoredStart(1:NDim) = PatchStart(1:NDim)
    call ExtOrder(MemoryOrder,StoredStart,Status)
    call FieldIO('write',DataHandle,DateStr,StoredStart,Length,MemoryOrder, &
                  FieldType,NCID,VarID,XField,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    deallocate(XField, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , TRIM(msg))
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , TRIM(msg))
  endif
  DH%first_operation  = .FALSE.
  return
end subroutine ext_pnc_write_field

subroutine ext_pnc_read_field(DataHandle,DateStr,Var,Field,FieldType,Comm,  &
  IOComm, DomainDesc, MemoryOrdIn, Stagger, DimNames,                       &
  DomainStart,DomainEnd,MemoryStart,MemoryEnd,PatchStart,PatchEnd,Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer                       ,intent(in)    :: DataHandle
  character*(*)                 ,intent(in)    :: DateStr
  character*(*)                 ,intent(in)    :: Var
  integer                       ,intent(out)   :: Field(*)
  integer                       ,intent(in)    :: FieldType
  integer                       ,intent(inout) :: Comm
  integer                       ,intent(inout) :: IOComm
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrdIn
  character*(*)                 ,intent(in)    :: Stagger ! Dummy for now
  character*(*) , dimension (*) ,intent(in)    :: DimNames
  integer ,dimension(*)         ,intent(in)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status
  character (3)                                :: MemoryOrder
  character (NF_MAX_NAME)                      :: dimname
  type(wrf_data_handle)         ,pointer       :: DH
  integer                                      :: NDim
  integer                                      :: NCID
  character (VarNameLen)                       :: VarName
  integer                                      :: VarID
  integer ,dimension(NVarDims)                 :: VCount
  integer ,dimension(NVarDims)                 :: VStart
  integer ,dimension(NVarDims)                 :: Length
  integer ,dimension(NVarDims)                 :: VDimIDs
  integer ,dimension(NVarDims)                 :: MemS
  integer ,dimension(NVarDims)                 :: MemE
  integer ,dimension(NVarDims)                 :: StoredStart
  integer ,dimension(NVarDims)                 :: StoredLen
  integer(KIND=MPI_OFFSET_KIND) ,dimension(NVarDims)                 :: StoredLen_okind
  integer ,dimension(:,:,:,:)   ,allocatable   :: XField
  integer                                      :: NVar
  integer                                      :: j
  integer                                      :: i1,i2,j1,j2,k1,k2
  integer                                      :: x1,x2,y1,y2,z1,z2
  integer                                      :: l1,l2,m1,m2,n1,n2
  character (VarNameLen)                       :: Name
  integer                                      :: XType
  integer                                      :: StoredDim
  integer                                      :: NAtts
  integer(KIND=MPI_OFFSET_KIND)                                      :: Len
  integer                                      :: stat
  integer                                      :: di
  integer                                      :: FType

  MemoryOrder = trim(adjustl(MemoryOrdIn))
  call GetDim(MemoryOrder,NDim,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning BAD MEMORY ORDER |',TRIM(MemoryOrder),'| for |', &
                 TRIM(Var),'| in ext_pnc_read_field ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR |',TRIM(DateStr),'| for |',TRIM(Var), &
                 '| in ext_pnc_read_field ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_pnc_read_field ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
! jm it is okay to have a dry run read. means read is called between ofrb and ofrc. Just return.
!    Status = WRF_WARN_DRYRUN_READ
!    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__ 
!    call wrf_debug ( WARN , TRIM(msg))
    Status = WRF_NO_ERR
    RETURN
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE ) then
    NCID = DH%NCID

    Length(1:NDim) = PatchEnd(1:NDim)-PatchStart(1:NDim)+1
    StoredStart(1:NDim) = PatchStart(1:NDim)

    call ExtOrder(MemoryOrder,Length,Status)

    stat = NFMPI_INQ_VARID(NCID,VarName,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Varname ',Varname
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = NFMPI_INQ_VAR(NCID,VarID,Name,XType,StoredDim,VDimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = NFMPI_GET_ATT_INT(NCID,VarID,'FieldType',FType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
! allow coercion between double and single prec real
!jm    if(FieldType /= Ftype) then
    if( (FieldType == WRF_REAL .OR. FieldType == WRF_DOUBLE) ) then
      if ( .NOT. (Ftype     == WRF_REAL .OR. Ftype     == WRF_DOUBLE ))  then
        Status = WRF_WARN_TYPE_MISMATCH
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    else if(FieldType /= Ftype) then
      Status = WRF_WARN_TYPE_MISMATCH
      write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif      
    select case (FieldType)
      case (WRF_REAL)
! allow coercion between double and single prec real
        if(.NOT. (XType == NF_FLOAT .OR. XType == NF_DOUBLE) )  then
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning REAL TYPE MISMATCH in ',__FILE__,', line', __LINE__
        endif
      case (WRF_DOUBLE)
! allow coercion between double and single prec real
        if(.NOT. (XType == NF_FLOAT .OR. XType == NF_DOUBLE) )  then
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning DOUBLE TYPE MISMATCH in ',__FILE__,', line', __LINE__
        endif
      case (WRF_INTEGER)
        if(XType /= NF_INT)  then 
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning INTEGER TYPE MISMATCH in ',__FILE__,', line', __LINE__
        endif
      case (WRF_LOGICAL)
        if(XType /= NF_INT)  then
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning LOGICAL TYPE MISMATCH in ',__FILE__,', line', __LINE__
        endif
      case default
        Status = WRF_WARN_DATA_TYPE_NOT_FOUND
        write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
    end select
    if(Status /= WRF_NO_ERR) then
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    ! NDim=0 for scalars.  Handle read of old NDim=1 files.  TBH:  20060502
    IF ( ( NDim == 0 ) .AND. ( StoredDim == 2 ) ) THEN
      stat = NFMPI_INQ_DIMNAME(NCID,VDimIDs(1),dimname)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
      IF ( dimname(1:10) == 'ext_scalar' ) THEN
        NDim = 1
        Length(1) = 1
      ENDIF
    ENDIF
    if(StoredDim /= NDim+1) then
      Status = WRF_ERR_FATAL_BAD_VARIABLE_DIM
      write(msg,*) 'Fatal error BAD VARIABLE DIMENSION in ext_pnc_read_field ',TRIM(Var),TRIM(DateStr)
      call wrf_debug ( FATAL , msg)
      write(msg,*) '  StoredDim ', StoredDim, ' .NE. NDim+1 ', NDim+1
      call wrf_debug ( FATAL , msg)
      return
    endif
    do j=1,NDim
      stat = NFMPI_INQ_DIMLEN(NCID,VDimIDs(j),StoredLen_okind(j))
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
      StoredLen(j) = StoredLen_okind(j)
      if(Length(j) > StoredLen(j)) then
        Status = WRF_WARN_READ_PAST_EOF
        write(msg,*) 'Warning READ PAST EOF in ext_pnc_read_field of ',TRIM(Var),Length(j),'>',StoredLen(j)
        call wrf_debug ( WARN , TRIM(msg))
        return
      elseif(Length(j) <= 0) then
        Status = WRF_WARN_ZERO_LENGTH_READ
        write(msg,*) 'Warning ZERO LENGTH READ in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo

    StoredStart = 1
    call GetIndices(NDim,MemoryStart,MemoryEnd,l1,l2,m1,m2,n1,n2)
    call GetIndices(NDim,StoredStart,Length,x1,x2,y1,y2,z1,z2)
!jm    call GetIndices(NDim,DomainStart,DomainEnd,i1,i2,j1,j2,k1,k2)
    call GetIndices(NDim,PatchStart,PatchEnd,i1,i2,j1,j2,k1,k2)
    
    StoredStart(1:NDim) = PatchStart(1:NDim)
    call ExtOrder(MemoryOrder,StoredStart,Status)

    di=1
    if(FieldType == WRF_DOUBLE) di=2
    allocate(XField(di,x1:x2,y1:y2,z1:z2), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    call FieldIO('read',DataHandle,DateStr,StoredStart,Length,MemoryOrder, &
                  FieldType,NCID,VarID,XField,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call Transpose('read',MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
                                        ,XField,x1,x2,y1,y2,z1,z2 &
                                               ,i1,i2,j1,j2,k1,k2 )
    deallocate(XField, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  DH%first_operation  = .FALSE.
  return
end subroutine ext_pnc_read_field

subroutine ext_pnc_inquire_opened( DataHandle, FileName , FileStatus, Status )
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: FileName
  integer               ,intent(out)    :: FileStatus
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    FileStatus = WRF_FILE_NOT_OPENED
    return
  endif
  if(FileName /= DH%FileName) then
    FileStatus = WRF_FILE_NOT_OPENED
  else
    FileStatus = DH%FileStatus
  endif
  Status = WRF_NO_ERR
  return
end subroutine ext_pnc_inquire_opened

subroutine ext_pnc_inquire_filename( Datahandle, FileName,  FileStatus, Status )
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(out)    :: FileName
  integer               ,intent(out)    :: FileStatus
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  FileStatus = WRF_FILE_NOT_OPENED
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  FileName = DH%FileName
  FileStatus = DH%FileStatus
  Status = WRF_NO_ERR
  return
end subroutine ext_pnc_inquire_filename

subroutine ext_pnc_set_time(DataHandle, DateStr, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: i

  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_FILE_NOT_COMMITTED
    write(msg,*) 'Warning FILE NOT COMMITTED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do i=1,MaxTimes
      if(DH%Times(i)==DateStr) then
        DH%CurrentTime = i
        exit
      endif
      if(i==MaxTimes) then
        Status = WRF_WARN_TIME_NF
        return
      endif
    enddo
    DH%CurrentVariable = 0
    Status = WRF_NO_ERR
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pnc_set_time

subroutine ext_pnc_get_next_time(DataHandle, DateStr, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(out)    :: DateStr
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE ) then
    if(DH%CurrentTime >= DH%NumberTimes) then
      write(msg,*) 'Warning ext_pnc_get_next_time: DH%CurrentTime >= DH%NumberTimes ',DH%CurrentTime,DH%NumberTimes
      call wrf_debug ( WARN , TRIM(msg))
      Status = WRF_WARN_TIME_EOF
      return
    endif
    DH%CurrentTime     = DH%CurrentTime +1
    DateStr            = DH%Times(DH%CurrentTime)
    DH%CurrentVariable = 0
    Status = WRF_NO_ERR
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pnc_get_next_time

subroutine ext_pnc_get_previous_time(DataHandle, DateStr, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(out)    :: DateStr
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    if(DH%CurrentTime.GT.0) then
      DH%CurrentTime     = DH%CurrentTime -1
    endif
    DateStr            = DH%Times(DH%CurrentTime)
    DH%CurrentVariable = 0
    Status = WRF_NO_ERR
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pnc_get_previous_time

subroutine ext_pnc_get_next_var(DataHandle, VarName, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(out)    :: VarName
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat
  character (80)                        :: Name

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then

    DH%CurrentVariable = DH%CurrentVariable +1
    if(DH%CurrentVariable > DH%NumVars) then
      Status = WRF_WARN_VAR_EOF
      return
    endif
    VarName = DH%VarNames(DH%CurrentVariable)
    Status  = WRF_NO_ERR
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pnc_get_next_var

subroutine ext_pnc_end_of_frame(DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
#  include "pnetcdf.inc"
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH

  call GetDH(DataHandle,DH,Status)
  return
end subroutine ext_pnc_end_of_frame

! NOTE:  For scalar variables NDim is set to zero and DomainStart and 
! NOTE:  DomainEnd are left unmodified.  
subroutine ext_pnc_get_var_info(DataHandle,Name,NDim,MemoryOrder,Stagger,DomainStart,DomainEnd,WrfType,Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
#  include "pnetcdf.inc"
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Name
  integer               ,intent(out)    :: NDim
  character*(*)         ,intent(out)    :: MemoryOrder
  character*(*)                         :: Stagger ! Dummy for now
  integer ,dimension(*) ,intent(out)    :: DomainStart, DomainEnd
  integer               ,intent(out)    :: WrfType
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: VarID
  integer ,dimension(NVarDims)          :: VDimIDs
  integer                               :: j
  integer                               :: stat
  integer                               :: XType

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    stat = NFMPI_INQ_VARID(DH%NCID,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = NFMPI_INQ_VARTYPE(DH%NCID,VarID,XType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = NFMPI_GET_ATT_INT(DH%NCID,VarID,'FieldType',WrfType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    select case (XType)
      case (NF_BYTE)
        Status = WRF_WARN_BAD_DATA_TYPE
        write(msg,*) 'Warning BYTE IS BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      case (NF_CHAR)
        Status = WRF_WARN_BAD_DATA_TYPE
        write(msg,*) 'Warning CHAR IS BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      case (NF_SHORT)
        Status = WRF_WARN_BAD_DATA_TYPE
        write(msg,*) 'Warning SHORT IS BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      case (NF_INT)
        if(WrfType /= WRF_INTEGER .and. WrfType /= WRF_LOGICAL) then
          Status = WRF_WARN_BAD_DATA_TYPE
          write(msg,*) 'Warning BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
          call wrf_debug ( WARN , TRIM(msg))
          return
        endif
      case (NF_FLOAT)
        if(WrfType /= WRF_REAL) then
          Status = WRF_WARN_BAD_DATA_TYPE
          write(msg,*) 'Warning BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
          call wrf_debug ( WARN , TRIM(msg))
          return
        endif
      case (NF_DOUBLE)
        if(WrfType /= WRF_DOUBLE) then
          Status = WRF_WARN_BAD_DATA_TYPE
          write(msg,*) 'Warning BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
          call wrf_debug ( WARN , TRIM(msg))
          return
        endif
      case default
        Status = WRF_WARN_DATA_TYPE_NOT_FOUND
        write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
    end select

    stat = NFMPI_GET_ATT_TEXT(DH%NCID,VarID,'MemoryOrder',MemoryOrder)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call GetDim(MemoryOrder,NDim,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning BAD MEMORY ORDER ',TRIM(MemoryOrder),' in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = NFMPI_INQ_VARDIMID(DH%NCID,VarID,VDimIDs)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    do j = 1, NDim
      DomainStart(j) = 1
      stat = NFMPI_INQ_DIMLEN(DH%NCID,VDimIDs(j),DomainEnd(j))
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pnc_get_var_info

subroutine ext_pnc_warning_str( Code, ReturnString, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
#  include "pnetcdf.inc"
  include 'wrf_status_codes.h'
  
  integer  , intent(in)  ::Code
  character *(*), intent(out) :: ReturnString
  integer, intent(out) ::Status
  
  SELECT CASE (Code)
  CASE (0)
      ReturnString='No error'
      Status=WRF_NO_ERR
      return
  CASE (-1)
      ReturnString= 'File not found (or file is incomplete)'
      Status=WRF_NO_ERR
      return
  CASE (-2)
      ReturnString='Metadata not found'
      Status=WRF_NO_ERR
      return
  CASE (-3)
      ReturnString= 'Timestamp not found'
      Status=WRF_NO_ERR
      return
  CASE (-4)
      ReturnString= 'No more timestamps'
      Status=WRF_NO_ERR
      return
  CASE (-5)
      ReturnString= 'Variable not found'
      Status=WRF_NO_ERR
      return
  CASE (-6)
      ReturnString= 'No more variables for the current time'
      Status=WRF_NO_ERR
      return
  CASE (-7)
      ReturnString= 'Too many open files'
      Status=WRF_NO_ERR
      return
  CASE (-8)
      ReturnString= 'Data type mismatch'
      Status=WRF_NO_ERR
      return
  CASE (-9)
      ReturnString= 'Attempt to write read-only file'
      Status=WRF_NO_ERR
      return
  CASE (-10)
      ReturnString= 'Attempt to read write-only file'
      Status=WRF_NO_ERR
      return
  CASE (-11)
      ReturnString= 'Attempt to access unopened file'
      Status=WRF_NO_ERR
      return
  CASE (-12)
      ReturnString= 'Attempt to do 2 trainings for 1 variable'
      Status=WRF_NO_ERR
      return
  CASE (-13)
      ReturnString= 'Attempt to read past EOF'
      Status=WRF_NO_ERR
      return
  CASE (-14)
      ReturnString= 'Bad data handle'
      Status=WRF_NO_ERR
      return
  CASE (-15)
      ReturnString= 'Write length not equal to training length'
      Status=WRF_NO_ERR
      return
  CASE (-16)
      ReturnString= 'More dimensions requested than training'
      Status=WRF_NO_ERR
      return
  CASE (-17)
      ReturnString= 'Attempt to read more data than exists'
      Status=WRF_NO_ERR
      return
  CASE (-18)
      ReturnString= 'Input dimensions inconsistent'
      Status=WRF_NO_ERR
      return
  CASE (-19)
      ReturnString= 'Input MemoryOrder not recognized'
      Status=WRF_NO_ERR
      return
  CASE (-20)
      ReturnString= 'A dimension name with 2 different lengths'
      Status=WRF_NO_ERR
      return
  CASE (-21)
      ReturnString= 'String longer than provided storage'
      Status=WRF_NO_ERR
      return
  CASE (-22)
      ReturnString= 'Function not supportable'
      Status=WRF_NO_ERR
      return
  CASE (-23)
      ReturnString= 'Package implements this routine as NOOP'
      Status=WRF_NO_ERR
      return

!netcdf-specific warning messages
  CASE (-1007)
      ReturnString= 'Bad data type'
      Status=WRF_NO_ERR
      return
  CASE (-1008)
      ReturnString= 'File not committed'
      Status=WRF_NO_ERR
      return
  CASE (-1009)
      ReturnString= 'File is opened for reading'
      Status=WRF_NO_ERR
      return
  CASE (-1011)
      ReturnString= 'Attempt to write metadata after open commit'
      Status=WRF_NO_ERR
      return
  CASE (-1010)
      ReturnString= 'I/O not initialized'
      Status=WRF_NO_ERR
      return
  CASE (-1012)
     ReturnString=  'Too many variables requested'
      Status=WRF_NO_ERR
      return
  CASE (-1013)
     ReturnString=  'Attempt to close file during a dry run'
      Status=WRF_NO_ERR
      return
  CASE (-1014)
      ReturnString= 'Date string not 19 characters in length'
      Status=WRF_NO_ERR
      return
  CASE (-1015)
      ReturnString= 'Attempt to read zero length words'
      Status=WRF_NO_ERR
      return
  CASE (-1016)
      ReturnString= 'Data type not found'
      Status=WRF_NO_ERR
      return
  CASE (-1017)
      ReturnString= 'Badly formatted date string'
      Status=WRF_NO_ERR
      return
  CASE (-1018)
      ReturnString= 'Attempt at read during a dry run'
      Status=WRF_NO_ERR
      return
  CASE (-1019)
      ReturnString= 'Attempt to get zero words'
      Status=WRF_NO_ERR
      return
  CASE (-1020)
      ReturnString= 'Attempt to put zero length words'
      Status=WRF_NO_ERR
      return
  CASE (-1021)
      ReturnString= 'NetCDF error'
      Status=WRF_NO_ERR
      return
  CASE (-1022)
      ReturnString= 'Requested length <= 1'
      Status=WRF_NO_ERR
      return
  CASE (-1023)
      ReturnString= 'More data available than requested'
      Status=WRF_NO_ERR
      return
  CASE (-1024)
      ReturnString= 'New date less than previous date'
      Status=WRF_NO_ERR
      return

  CASE DEFAULT
      ReturnString= 'This warning code is not supported or handled directly by WRF and NetCDF. &
      & Might be an erroneous number, or specific to an i/o package other than NetCDF; you may need &
      & to be calling a package-specific routine to return a message for this warning code.'
      Status=WRF_NO_ERR
  END SELECT

  return
end subroutine ext_pnc_warning_str


!returns message string for all WRF and netCDF warning/error status codes
!Other i/o packages must  provide their own routines to return their own status messages
subroutine ext_pnc_error_str( Code, ReturnString, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  implicit none
#  include "pnetcdf.inc"
  include 'wrf_status_codes.h'

  integer  , intent(in)  ::Code
  character *(*), intent(out) :: ReturnString
  integer, intent(out) ::Status

  SELECT CASE (Code)
  CASE (-100)
      ReturnString= 'Allocation Error'
      Status=WRF_NO_ERR
      return
  CASE (-101)
      ReturnString= 'Deallocation Error'
      Status=WRF_NO_ERR
      return
  CASE (-102)
      ReturnString= 'Bad File Status'
      Status=WRF_NO_ERR
      return
  CASE (-1004)
      ReturnString= 'Variable on disk is not 3D'
      Status=WRF_NO_ERR
      return
  CASE (-1005)
      ReturnString= 'Metadata on disk is not 1D'
      Status=WRF_NO_ERR
      return
  CASE (-1006)
      ReturnString= 'Time dimension too small'
      Status=WRF_NO_ERR
      return
  CASE DEFAULT
      ReturnString= 'This error code is not supported or handled directly by WRF and NetCDF. &
      & Might be an erroneous number, or specific to an i/o package other than NetCDF; you may need & 
      & to be calling a package-specific routine to return a message for this error code.'
      Status=WRF_NO_ERR
  END SELECT

  return
end subroutine ext_pnc_error_str


!ARPaddition...
subroutine ext_pnc_end_independent_mode(DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer               ,intent(in)     :: DataHandle
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat

  DH => WrfDataHandles(DataHandle)
  DH%Collective = .TRUE.
  stat = NFMPI_END_INDEP_DATA(DH%NCID)

  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , TRIM(msg))
  endif

  return
end subroutine ext_pnc_end_independent_mode

subroutine ext_pnc_start_independent_mode(DataHandle, Status)
  use wrf_data_pnc
  use ext_pnc_support_routines
  include 'wrf_status_codes.h'
#  include "pnetcdf.inc"
  integer               ,intent(in)     :: DataHandle
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat

  DH => WrfDataHandles(DataHandle)
  DH%Collective = .FALSE.
  stat = NFMPI_BEGIN_INDEP_DATA(DH%NCID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , TRIM(msg))
  endif

  return

end subroutine ext_pnc_start_independent_mode
!ARPaddition end
