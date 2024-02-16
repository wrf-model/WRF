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

module wrf_data_adios2
   use adios2 
   integer                , parameter      :: FATAL            = 0
   integer                , parameter      :: WARN             = 0
   integer                , parameter      :: WrfDataHandleMax = 99
   integer                , parameter      :: MaxDims          = 2000 ! = NF_MAX_VARS
   integer                , parameter      :: MaxVars          = 3000
   integer                , parameter      :: MaxTimes         = 60000
   integer                , parameter      :: DateStrLen       = 19
   integer                , parameter      :: VarNameLen       = 31
   integer                , parameter      :: NO_DIM           = 0
   integer                , parameter      :: NVarDims         = 3 !down from 4, as time is dealt with as "steps" in ADIOS2
   integer                , parameter      :: NMDVarDims       = 2
   character (8)          , parameter      :: NO_NAME          = 'NULL'
   character (DateStrLen) , parameter      :: ZeroDate = '0000-00-00-00:00:00'
#include "wrf_io_flags.h"
   character (256)                         :: msg
   logical                                 :: WrfIOnotInitialized = .true.
   type(adios2_adios)                      :: adios

   type :: wrf_data_handle
     character (255)                       :: FileName
     integer                               :: FileStatus
     logical                               :: Free
     logical                               :: Write
     character (5)                         :: TimesName
     integer                               :: TimeIndex
     integer                               :: CurrentTime  !Only used for read
     integer                               :: NumberTimes  !Only used for read
     character (DateStrLen), pointer       :: Times(:)
     type(adios2_variable)                 :: TimesVarID
     integer               , pointer       :: DimLengths(:)
     type(adios2_attribute), pointer       :: DimIDs(:)
     character (31)        , pointer       :: DimNames(:)
     type(adios2_attribute)                :: DimUnlimID
     character (9)                         :: DimUnlimName
     type(adios2_attribute), dimension(NVarDims) :: DimID
     integer       , dimension(NVarDims)   :: Dimension 
     type(adios2_variable), pointer        :: MDVarIDs(:)
     integer               , pointer       :: MDVarDimLens(:)
     character (80)        , pointer       :: MDVarNames(:)
     type(adios2_variable) , pointer       :: VarIDs(:)
     integer               , pointer       :: VarDimLens(:,:)
     character (VarNameLen), pointer       :: VarNames(:)
     integer                               :: CurrentVariable  !Only used for read
     integer                               :: NumVars
 ! first_operation is set to .TRUE. when a new handle is allocated 
 ! or when open-for-write or open-for-read are committed.  It is set 
 ! to .FALSE. when the first field is read or written.  
     logical                               :: first_operation
     type(adios2_io)                       :: adios2IO
     type(adios2_engine)                   :: adios2Engine
     type(adios2_operator)                 :: compress_operator
     character(32)                         :: blosc_compressor
   end type wrf_data_handle
   type(wrf_data_handle),target            :: WrfDataHandles(WrfDataHandleMax)
 end module wrf_data_adios2
 
module ext_adios2_support_routines
   
   implicit none
   !include 'mpif.h'
 
 CONTAINS
 
 subroutine allocHandle(DataHandle,DH,Status)
   use wrf_data_adios2
   include 'wrf_status_codes.h'
   integer              ,intent(out) :: DataHandle
   type(wrf_data_handle),pointer     :: DH
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
       allocate(DH%VarDimLens(NVarDims,MaxVars), STAT=stat)
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
       write(msg,*) 'Did you call ext_adios2_ioinit?'
       call wrf_debug ( WARN , TRIM(msg))
       return
     endif
   enddo
   DH%Free      =.false.
   DH%Write     =.false.
   DH%first_operation  = .TRUE.
   Status = WRF_NO_ERR
 end subroutine allocHandle
 
 subroutine deallocHandle(DataHandle, Status)
   use wrf_data_adios2
   include 'wrf_status_codes.h'
   integer              ,intent(in)  :: DataHandle
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
   use wrf_data_adios2
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
   use wrf_data_adios2
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
   use wrf_data_adios2
   include 'wrf_status_codes.h'
   character*(*) ,intent(in)     :: Element
   character*(*) ,intent(in)     :: Var
   character*(*) ,intent(out)    :: Name
   integer       ,intent(out)    :: Status
   character (VarNameLen)        :: VarName
   character (1)                 :: c
   integer                       :: i
   integer, parameter            :: upper_to_lower =IACHAR('a')-IACHAR('A')
 
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
   use wrf_data_adios2
   use adios2
   include 'wrf_status_codes.h'
   character (*)         ,intent(in)     :: IO
   integer               ,intent(in)     :: DataHandle
   character*(*)         ,intent(in)     :: DateStr
   integer               ,intent(out)    :: TimeIndex
   integer               ,intent(out)    :: Status
   type(wrf_data_handle) ,pointer        :: DH
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
     CALL adios2_put(DH%adios2Engine, DH%TimesVarID, DateStr, adios2_mode_sync, stat)
     call adios2_err(stat,Status)
     if(Status /= WRF_NO_ERR) then
       write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__ 
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
   use wrf_data_adios2
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
   use wrf_data_adios2
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
   use wrf_data_adios2
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
 
 subroutine adios2_err(err,Status)
   use wrf_data_adios2
   use adios2
   include 'wrf_status_codes.h'
   integer  ,intent(in)  :: err
   integer  ,intent(out) :: Status
   character(len=80)     :: errmsg
   integer               :: stat
 
   if( err == adios2_error_none )then
     Status = WRF_NO_ERR
   else
     write(msg,*) 'adios2 error code: ',err
     call wrf_debug ( WARN , TRIM(msg))
     Status = WRF_WARN_ADIOS2
   endif
   return
 end subroutine adios2_err
 
 subroutine FieldIO(IO,DataHandle,DateStr,Starts,Length,MemoryOrder &
                      ,FieldType,VarID,XField,Status)
   use wrf_data_adios2
   use adios2
   include 'wrf_status_codes.h'
   character (*)              ,intent(in)    :: IO
   integer                    ,intent(in)    :: DataHandle
   character*(*)              ,intent(in)    :: DateStr
   integer,dimension(NVarDims),intent(in)    :: Starts
   integer,dimension(NVarDims),intent(in)    :: Length
   character*(*)              ,intent(in)    :: MemoryOrder
   integer                    ,intent(in)    :: FieldType
   type(adios2_variable)      ,intent(in)    :: VarID
   integer,dimension(*)       ,intent(inout) :: XField
   integer                    ,intent(out)   :: Status
   integer                                   :: TimeIndex
   integer                                   :: NDim
   integer(kind=8),dimension(NVarDims)       :: VStart
   integer(kind=8),dimension(NVarDims)       :: VCount
   integer(kind=8)                           :: TimeIndex_int8
   integer                                   :: stat
 
   call GetTimeIndex(IO,DataHandle,DateStr,TimeIndex,Status)
   if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , TRIM(msg))
     write(msg,*) '  Bad time index for DateStr = ',DateStr
     call wrf_debug ( WARN , TRIM(msg))
     return
   endif
   if(IO == 'write') then
    TimeIndex_int8 = TimeIndex
    call adios2_set_step_selection(VarID, TimeIndex_int8 -1_8, 1_8, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in FieldIO ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
   endif
   call GetDim(MemoryOrder,NDim,Status)
   VStart(:) = 1
   VCount(:) = 1
   VStart(1:NDim) = Starts(1:NDim)
   VCount(1:NDim) = Length(1:NDim)
   select case (FieldType)
     case (WRF_REAL)
       call ext_adios2_RealFieldIO    (IO, DataHandle,VarID,VStart,VCount,XField,Status)
     case (WRF_DOUBLE)
       call ext_adios2_DoubleFieldIO  (IO,DataHandle,VarID,VStart,VCount,XField,Status)
     case (WRF_INTEGER)
       call ext_adios2_IntFieldIO     (IO,DataHandle,VarID,VStart,VCount,XField,Status)
     case (WRF_LOGICAL)
       call ext_adios2_LogicalFieldIO (IO,DataHandle,VarID,VStart,VCount,XField,Status)
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
   integer           ,intent(inout) :: XField(di,(i2-i1+1)*(j2-j1+1)*(k2-k1+1))
   character*3                      :: MemOrd
   character*3                      :: MemO
   integer           ,parameter     :: MaxUpperCase=IACHAR('Z')
   integer                          :: i,j,k,ix,jx,kx
 
   call LowerCase(MemoryOrder,MemOrd)
   select case (MemOrd)
! Cannot use following define due to gfortran cpp traditional mode concatenation limitations 
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
 LOGICAL FUNCTION adios2_ok_to_put_dom_ti( DataHandle )
     USE wrf_data_adios2
     include 'wrf_status_codes.h'
     INTEGER, INTENT(IN) :: DataHandle 
     CHARACTER*80 :: fname
     INTEGER :: filestate
     INTEGER :: Status
     LOGICAL :: dryrun, first_output, retval
     call ext_adios2_inquire_filename( DataHandle, fname, filestate, Status )
     IF ( Status /= WRF_NO_ERR ) THEN
       write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
                    ', line', __LINE__
       call wrf_debug ( WARN , TRIM(msg) )
       retval = .FALSE.
     ELSE
       dryrun       = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
       first_output = adios2_is_first_operation( DataHandle )
 !      retval = .NOT. dryrun .AND. first_output
       retval = dryrun
     ENDIF
     adios2_ok_to_put_dom_ti = retval
     RETURN
 END FUNCTION adios2_ok_to_put_dom_ti
 
 ! Returns .TRUE. iff it is OK to read time-independent domain metadata from the 
 ! file referenced by DataHandle.  If DataHandle is invalid, .FALSE. is 
 ! returned.  
 LOGICAL FUNCTION adios2_ok_to_get_dom_ti( DataHandle )
     USE wrf_data_adios2
     include 'wrf_status_codes.h'
     INTEGER, INTENT(IN) :: DataHandle 
     CHARACTER*80 :: fname
     INTEGER :: filestate
     INTEGER :: Status
     LOGICAL :: dryrun, retval
     call ext_adios2_inquire_filename( DataHandle, fname, filestate, Status )
     IF ( Status /= WRF_NO_ERR ) THEN
       write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
                    ', line', __LINE__
       call wrf_debug ( WARN , TRIM(msg) )
       retval = .FALSE.
     ELSE
       dryrun       = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
       retval = .NOT. dryrun
     ENDIF
     adios2_ok_to_get_dom_ti = retval
     RETURN
 END FUNCTION adios2_ok_to_get_dom_ti
 
 ! Returns .TRUE. iff nothing has been read from or written to the file 
 ! referenced by DataHandle.  If DataHandle is invalid, .FALSE. is returned.  
 LOGICAL FUNCTION adios2_is_first_operation( DataHandle )
     USE wrf_data_adios2
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
     adios2_is_first_operation = retval
     RETURN
 END FUNCTION adios2_is_first_operation
 
 end module ext_adios2_support_routines

subroutine ext_adios2_open_for_read(DatasetName, SysDepInfo, DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  character *(*), INTENT(IN)   :: DatasetName
  character *(*), INTENT(IN)   :: SysDepInfo
  integer       , INTENT(OUT)  :: DataHandle
  integer       , INTENT(OUT)  :: Status
  
  DataHandle = 0   ! dummy setting to quiet warning message
  CALL ext_adios2_open_for_read_begin( DatasetName, SysDepInfo, DataHandle, Status )
  IF ( Status .EQ. WRF_NO_ERR ) THEN
    CALL ext_adios2_open_for_read_commit( DataHandle, Status )
  ENDIF
  return
end subroutine ext_adios2_open_for_read

!ends training phase; switches internal flag to enable input
!must be paired with call to ext_adios2_open_for_read_begin
subroutine ext_adios2_open_for_read_commit(DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  integer, intent(in)              :: DataHandle
  integer, intent(out)             :: Status
  integer                          :: stat
  type(wrf_data_handle) ,pointer   :: DH

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED
    write(msg,*) 'ext_adios2_ioinit was not called ',__FILE__,', line', __LINE__
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
end subroutine ext_adios2_open_for_read_commit

subroutine ext_adios2_open_for_read_begin( FileName, SysDepInfo, DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  character*(*)         ,intent(IN)              :: FileName
  character*(*)         ,intent(in)              :: SysDepInfo
  integer               ,intent(out)             :: DataHandle
  integer               ,intent(out)             :: Status
  type(wrf_data_handle) ,pointer                 :: DH
  integer                                        :: XType
  integer                                        :: stat
  type(adios2_variable)                          :: VarIDTime
  type(adios2_variable)                          :: VarID
  integer                                        :: StoredDim
  integer                                        :: DimIDs(2)
  integer                                        :: TotalNumVars
  integer                                        :: NumVars
  integer                                        :: i
  integer(kind=8)                                :: timestep
  integer(kind=8)                                :: nsteps
  character(len=4096), dimension(:), allocatable :: varnamelist
  type(adios2_namestruct)                        :: namestruct
  character(len=256)                             :: Name

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_adios2_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call adios2_declare_io(DH%adios2IO, adios, FileName, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_open_for_read_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call adios2_open(DH%adios2Engine, DH%adios2IO, FileName, adios2_mode_read, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_open_for_read_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call adios2_inquire_variable(VarIDTime, DH%adios2IO, DH%TimesName, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_open_for_read_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call adios2_steps(nsteps, DH%adios2Engine, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_open_for_read_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(nsteps > MaxTimes) then
    Status = WRF_ERR_FATAL_TOO_MANY_TIMES
    write(msg,*) 'Fatal TOO MANY TIME VALUES in ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  ! Read in times from different time steps
  do timestep=1,nsteps
    call adios2_set_step_selection(VarIDTime, timestep - 1, 1_8, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ext_adios2_open_for_read_begin ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call adios2_get(DH%adios2Engine, VarIDTime, DH%Times(timestep), adios2_mode_sync, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ext_adios2_open_for_read_begin ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  end do
  ! Get variable names
  call adios2_available_variables(DH%adios2IO, namestruct, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  allocate(varnamelist(namestruct%count))
  call adios2_retrieve_names(namestruct, varnamelist, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  TotalNumVars = namestruct%count
  NumVars = 0
  do i=1,TotalNumVars
    Name = varnamelist(i)
    call adios2_inquire_variable(VarID, DH%adios2IO, Name, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    elseif(Name(1:5) /= 'md___' .and. Name /= DH%TimesName) then
      NumVars              = NumVars + 1
      DH%VarNames(NumVars) = Name
      DH%VarIDs(NumVars)   = VarID
    endif      
  enddo
  deallocate(varnamelist)
  DH%NumVars         = NumVars
  DH%NumberTimes     = nsteps
  DH%FileStatus      = WRF_FILE_OPENED_NOT_COMMITTED
  DH%FileName        = FileName
  DH%CurrentVariable = 0
  DH%CurrentTime     = 0
  DH%TimesVarID      = VarIDTime
  DH%TimeIndex       = 0
  return
end subroutine ext_adios2_open_for_read_begin

subroutine ext_adios2_open_for_update( FileName, SysDepInfo, DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  character*(*)         ,intent(IN)              :: FileName
  character*(*)         ,intent(in)              :: SysDepInfo
  integer               ,intent(out)             :: DataHandle
  integer               ,intent(out)             :: Status
  type(wrf_data_handle) ,pointer                 :: DH
  integer                                        :: XType
  integer                                        :: stat
  type(adios2_variable)                          :: VarIDTime
  type(adios2_variable)                          :: VarID
  integer                                        :: StoredDim
  integer                                        :: DimIDs(2)
  integer                                        :: TotalNumVars
  integer                                        :: NumVars
  integer                                        :: i
  integer(kind=8)                                :: timestep
  integer(kind=8)                                :: nsteps
  character(len=4096), dimension(:), allocatable :: varnamelist
  type(adios2_namestruct)                        :: namestruct
  character(len=256)                             :: Name

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_adios2_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call adios2_declare_io(DH%adios2IO, adios, FileName, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_open_for_update ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call adios2_open(DH%adios2Engine, DH%adios2IO, FileName, adios2_mode_read, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error (',stat,') from adios2_open in ext_adios2_open_for_update ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call adios2_inquire_variable(VarIDTime, DH%adios2IO, DH%TimesName, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_open_for_update ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call adios2_steps(nsteps, DH%adios2Engine, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_open_for_update ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(nsteps > MaxTimes) then
    Status = WRF_ERR_FATAL_TOO_MANY_TIMES
    write(msg,*) 'Fatal TOO MANY TIME VALUES in ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  ! Read in times from different time steps
  do timestep=1,nsteps
    call adios2_set_step_selection(VarIDTime, timestep-1, 1_8, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ext_adios2_open_for_update ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call adios2_get(DH%adios2Engine, VarIDTime, DH%Times(timestep), adios2_mode_sync, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ext_adios2_open_for_update ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  end do
  ! Get variable names
  call adios2_available_variables(DH%adios2IO, namestruct, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  allocate(varnamelist(namestruct%count))
  call adios2_retrieve_names(namestruct, varnamelist, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  TotalNumVars = namestruct%count
  NumVars = 0
  do i=1,TotalNumVars
    Name = varnamelist(i)
    call adios2_inquire_variable(VarID, DH%adios2IO, Name, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    elseif(Name(1:5) /= 'md___' .and. Name /= DH%TimesName) then
      NumVars              = NumVars + 1
      DH%VarNames(NumVars) = Name
      DH%VarIDs(NumVars)   = VarID
    endif      
  enddo
  deallocate(varnamelist)
  DH%NumVars         = NumVars
  DH%NumberTimes     = nsteps
  DH%FileStatus      = WRF_FILE_OPENED_FOR_UPDATE
  DH%FileName        = FileName
  DH%CurrentVariable = 0
  DH%CurrentTime     = 0
  DH%TimesVarID      = VarIDTime
  DH%TimeIndex       = 0

  call adios2_close(DH%adios2Engine, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call adios2_open(DH%adios2Engine, DH%adios2IO, FileName, adios2_mode_append, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error (',stat,') from adios2_open in ext_adios2_open_for_update ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  return
end subroutine ext_adios2_open_for_update

SUBROUTINE ext_adios2_open_for_write_begin(FileName,SysDepInfo,Iotype,DataHandle,Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  character*(*)        ,intent(in)  :: FileName
  character*(*)        ,intent(in)  :: SysDepInfo
  character*(*)        ,intent(in)  :: Iotype
  integer              ,intent(out) :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: i
  integer                           :: stat
  character (7)                     :: Buffer
  integer                           :: ierr
  integer                           :: gridid
  type(adios2_variable)             :: var
  type(adios2_attribute)            :: attribute
  type(adios2_attribute)            :: timeAttribute
  logical                           :: compression_enabled
  character*32                      :: compressor
  character(80),dimension(2)        :: DimNamesOut
  logical                           :: in_config
  integer                           :: numaggregators
  character(256)                    :: s_numaggregators

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_adios2_open_for_write_begin: ext_adios2_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ext_adios2_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  DH%TimeIndex = 0
  DH%Times     = ZeroDate
  DH%FileStatus  = WRF_FILE_OPENED_NOT_COMMITTED
  DH%FileName    = FileName
  !ADIOS2 declare i/o
  if(DH%first_operation) then
    call adios2_declare_io(DH%adios2IO, adios, DH%FileName, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ext_adios2_open_for_write_begin ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    DH%first_operation = .false.
 end if
  DH%VarNames  (1:MaxVars) = NO_NAME
  DH%MDVarNames(1:MaxVars) = NO_NAME
  do i=1,MaxDims
    write(Buffer,FMT="('DIM',i4.4)") i
    DH%DimNames  (i) = Buffer
    DH%DimLengths(i) = NO_DIM
  enddo
  DH%DimNames(1) = 'DateStrLen'
  call adios2_define_attribute(DH%DimIDs(1), DH%adios2IO, '_DIM_DateStrLen', &
      DateStrLen, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  !define "Times" variable and dimension attribute
  call adios2_define_variable(DH%TimesVarID, DH%adios2IO, DH%TimesName, adios2_type_character, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DimNamesOut(1) = 'DateStrLen'
  DimNamesOut(2) = 'Time'
  call adios2_define_attribute(timeAttribute,DH%adios2IO, 'Dims', DimNamesOut, 2, DH%TimesVarID%name, '/', stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_open_for_write_begin ',__FILE__,', line', __LINE__
  call wrf_debug ( WARN , TRIM(msg))
  return
  endif
  !ADIOS2 compression
  CALL nl_get_adios2_compression_enable(1,   compression_enabled)
  if (compression_enabled) then
    if (DH%compress_operator%name .ne. 'Compressor') then
      CALL nl_get_adios2_blosc_compressor(1,   compressor)
      DH%blosc_compressor = compressor
      call adios2_define_operator(DH%compress_operator, adios, 'Compressor', 'blosc', stat)
      call adios2_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'adios2 error in ext_adios2_open_for_write_begin ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    endif
  endif
  !ADIOS2 number of aggregators (AKA substreams, subfiles). Overrules setting in adios2.xml.
  !numaggregators = 0 will set a single aggregator per node.
  CALL nl_get_adios2_numaggregators(1,   numaggregators)
  write(s_numaggregators,*) numaggregators
  call adios2_set_parameter(DH%adios2IO, 'NumAggregators', s_numaggregators, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
    
  DH%DimLengths(1) = DateStrLen
  return
end subroutine ext_adios2_open_for_write_begin

!stub
!opens a file for writing or coupler datastream for sending messages.
!no training phase for this version of the open stmt.
subroutine ext_adios2_open_for_write (DatasetName, SysDepInfo, DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  implicit none
  include 'wrf_status_codes.h'
  character *(*), intent(in)  :: DatasetName
  character *(*), intent(in)  :: SysDepInfo
  integer       , intent(out) :: DataHandle
  integer       , intent(out) :: Status
  
  Status=WRF_WARN_NOOP
  DataHandle = 0    ! dummy setting to quiet warning message
  return
end subroutine ext_adios2_open_for_write

SUBROUTINE ext_adios2_start_io_timestep(DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: stat
  
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_adios2_start_io_timestep ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg)) 
    return
  endif
  if (DH%adios2Engine%valid .eqv. .true.) then
    call adios2_begin_step(DH%adios2Engine, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error (',stat,') from adios2_begin_step in ext_adios2_start_io_timestep ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  endif
  return
end SUBROUTINE ext_adios2_start_io_timestep

SUBROUTINE ext_adios2_end_io_timestep(DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: stat
  
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_adios2_end_io_timestep ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg)) 
    return
  endif
  if (DH%adios2Engine%valid .eqv. .true.) then
    call adios2_end_step(DH%adios2Engine, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error (',stat,') from adios2_end_step in ext_adios2_end_io_timestep ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  endif
  return
end SUBROUTINE ext_adios2_end_io_timestep

SUBROUTINE ext_adios2_open_for_write_commit(DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: i
  integer                           :: stat

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_adios2_open_for_write_commit: ext_adios2_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_adios2_open_for_write_commit ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg)) 
    return
  endif
  call adios2_open(DH%adios2Engine, DH%adios2IO, DH%FileName, adios2_mode_write, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error (',stat,') from adios2_open in ext_adios2_open_for_write_commit ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%FileStatus  = WRF_FILE_OPENED_FOR_WRITE
  DH%first_operation  = .TRUE.
  return
end subroutine ext_adios2_open_for_write_commit

subroutine ext_adios2_ioclose(DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: stat

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_adios2_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ext_adios2_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_CLOSE
    write(msg,*) 'Warning TRY TO CLOSE DRYRUN in ext_adios2_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue    
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    continue
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ext_adios2_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  call adios2_close(DH%adios2Engine, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  CALL deallocHandle( DataHandle, Status )
  DH%Free=.true.
  return
end subroutine ext_adios2_ioclose

subroutine ext_adios2_iosync( DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: stat

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_adios2_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ext_adios2_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_FILE_NOT_COMMITTED
    write(msg,*) 'Warning FILE NOT COMMITTED in ext_adios2_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    continue
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ext_adios2_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  return
end subroutine ext_adios2_iosync

subroutine ext_adios2_redef( DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  implicit none
  include 'wrf_status_codes.h'
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
  DH%FileStatus  = WRF_FILE_OPENED_NOT_COMMITTED
  return
end subroutine ext_adios2_redef

subroutine ext_adios2_enddef( DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  implicit none
  include 'wrf_status_codes.h'
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
  DH%FileStatus  = WRF_FILE_OPENED_FOR_WRITE
  return
end subroutine ext_adios2_enddef

subroutine ext_adios2_ioinit(SysDepInfo, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  include 'mpif.h'
  CHARACTER*(*), INTENT(IN) :: SysDepInfo
  integer                   :: stat, rank, ierror
  INTEGER ,INTENT(INOUT)    :: Status
  logical                   :: file_exists=.FALSE.
    
  WrfIOnotInitialized                             = .false.
  WrfDataHandles(1:WrfDataHandleMax)%Free         = .true.
  WrfDataHandles(1:WrfDataHandleMax)%TimesName    = 'Times'
  WrfDataHandles(1:WrfDataHandleMax)%DimUnlimName = 'Time'
  WrfDataHandles(1:WrfDataHandleMax)%FileStatus   = WRF_FILE_NOT_OPENED
  Status = WRF_NO_ERR
  !look for adios2 xml runtime configuration
  INQUIRE(FILE="adios2.xml", EXIST=file_exists)  
  if(file_exists) then
    call adios2_init(adios, 'adios2.xml', MPI_COMM_WORLD, stat)
  else
    call adios2_init(adios, MPI_COMM_WORLD, stat)
  endif
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_ioinit ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  return
end subroutine ext_adios2_ioinit


subroutine ext_adios2_inquiry (Inquiry, Result, Status)
  use wrf_data_adios2
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
end subroutine ext_adios2_inquiry


subroutine ext_adios2_ioexit(Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  integer       , INTENT(INOUT)     :: Status
  integer                           :: error
  type(wrf_data_handle),pointer     :: DH
  integer                           :: i
  integer                           :: stat
  
  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_adios2_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  do i=1,WrfDataHandleMax
    CALL deallocHandle( i , stat ) 
  enddo
  call adios2_finalize(adios, stat)
  call adios2_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'adios2 error in ext_adios2_ioexit ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  return
end subroutine ext_adios2_ioexit

subroutine ext_adios2_get_dom_ti_real(DataHandle,Element,Data,Count,OutCount,Status)
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA real,intent(out) :: Data(*)
#define TYPE_COUNT integer,intent(in) :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define TYPE_BUFFER  real,allocatable :: Buffer(:)
#define COPY   Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
#include "ext_adios2_get_dom_ti.code"
  return
end subroutine ext_adios2_get_dom_ti_real

subroutine ext_adios2_get_dom_ti_integer(DataHandle,Element,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA 
#undef TYPE_BUFFER
#undef COPY
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA integer,intent(out) :: Data(*)
#define TYPE_BUFFER  integer,allocatable :: Buffer(:)
#define COPY   Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
#include "ext_adios2_get_dom_ti.code"
  return
end subroutine ext_adios2_get_dom_ti_integer

subroutine ext_adios2_get_dom_ti_double(DataHandle,Element,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA 
#undef TYPE_BUFFER
#undef COPY
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA real*8,intent(out) :: Data(*)
#define TYPE_BUFFER  real*8,allocatable :: Buffer(:)
#define COPY   Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
#include "ext_adios2_get_dom_ti.code"
  return
end subroutine ext_adios2_get_dom_ti_double

subroutine ext_adios2_get_dom_ti_logical(DataHandle,Element,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA 
#undef TYPE_BUFFER
#undef COPY
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA logical,intent(out) :: Data(*)
#define TYPE_BUFFER  integer,allocatable :: Buffer(:)
#define COPY   Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))==1
#include "ext_adios2_get_dom_ti.code"
  return
end subroutine ext_adios2_get_dom_ti_logical

subroutine ext_adios2_get_dom_ti_char(DataHandle,Element,Data,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef TYPE_BUFFER
#define ROUTINE_TYPE 'CHAR'
#define TYPE_DATA character*(*),intent(out) :: Data
#define TYPE_COUNT
#define TYPE_OUTCOUNT
#define TYPE_BUFFER
#define CHAR_TYPE
#include "ext_adios2_get_dom_ti.code"
#undef CHAR_TYPE
  return
end subroutine ext_adios2_get_dom_ti_char

subroutine ext_adios2_put_dom_ti_real(DataHandle,Element,Data,Count,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA 
#undef TYPE_COUNT
#undef LOG
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA  real   ,intent(in) :: Data(Count)
#define TYPE_COUNT integer,intent(in) :: Count
#include "ext_adios2_put_dom_ti.code"
end subroutine ext_adios2_put_dom_ti_real

subroutine ext_adios2_put_dom_ti_integer(DataHandle,Element,Data,Count,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA
#undef TYPE_COUNT
#undef LOG
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA  integer,intent(in) :: Data(Count)
#define TYPE_COUNT integer,intent(in) :: Count
#include "ext_adios2_put_dom_ti.code"
end subroutine ext_adios2_put_dom_ti_integer

subroutine ext_adios2_put_dom_ti_double(DataHandle,Element,Data,Count,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA
#undef TYPE_COUNT
#undef LOG
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA  real*8 ,intent(in) :: Data(:)
#define TYPE_COUNT integer,intent(in) :: Count
#include "ext_adios2_put_dom_ti.code"
end subroutine ext_adios2_put_dom_ti_double

subroutine ext_adios2_put_dom_ti_logical(DataHandle,Element,Data,Count,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA
#undef TYPE_COUNT
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA  logical,intent(in) :: Data(Count)
#define TYPE_COUNT integer,intent(in) :: Count
#define LOG
#include "ext_adios2_put_dom_ti.code"
end subroutine ext_adios2_put_dom_ti_logical

subroutine ext_adios2_put_dom_ti_char(DataHandle,Element,Data,Status)
#undef ROUTINE_TYPE 
#undef TYPE_DATA
#undef TYPE_COUNT
#undef LOG
#define ROUTINE_TYPE 'CHAR'
#define CHAR_TYPE
#define TYPE_COUNT
#define TYPE_DATA  character*(*), intent(in) :: Data
#include "ext_adios2_put_dom_ti.code"
#undef CHAR_TYPE
end subroutine ext_adios2_put_dom_ti_char

subroutine ext_adios2_put_var_ti_real(DataHandle,Element,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef LOG
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA  real    ,intent(in) :: Data(Count)
#define TYPE_COUNT integer ,intent(in) :: Count
#include "ext_adios2_put_var_ti.code"
end subroutine ext_adios2_put_var_ti_real

subroutine ext_adios2_put_var_td_real(DataHandle,Element,DateStr,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef ADIOS2TYPE
#undef LENGTH
#undef LOG
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA  real    ,intent(in) :: Data(Count)
#define TYPE_COUNT integer ,intent(in) :: Count
#define ADIOS2TYPE adios2_type_real 
#define LENGTH Count
#include "ext_adios2_put_var_td.code"
end subroutine ext_adios2_put_var_td_real

subroutine ext_adios2_put_var_ti_double(DataHandle,Element,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef LOG
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA  real*8 ,intent(in) :: Data(Count)
#define TYPE_COUNT integer ,intent(in) :: Count
#include "ext_adios2_put_var_ti.code"
end subroutine ext_adios2_put_var_ti_double

subroutine ext_adios2_put_var_td_double(DataHandle,Element,DateStr,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef ADIOS2TYPE
#undef LENGTH
#undef LOG
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA  real*8,intent(in)   :: Data(Count)
#define TYPE_COUNT integer ,intent(in) :: Count
#define ADIOS2TYPE adios2_type_dp 
#define LENGTH Count
#include "ext_adios2_put_var_td.code"
end subroutine ext_adios2_put_var_td_double

subroutine ext_adios2_put_var_ti_integer(DataHandle,Element,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef LOG
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA  integer ,intent(in) :: Data(Count)
#define TYPE_COUNT integer ,intent(in) :: Count
#include "ext_adios2_put_var_ti.code"
end subroutine ext_adios2_put_var_ti_integer

subroutine ext_adios2_put_var_td_integer(DataHandle,Element,DateStr,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef ADIOS2TYPE
#undef LENGTH
#undef LOG
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA  integer ,intent(in) :: Data(Count)
#define TYPE_COUNT integer ,intent(in) :: Count
#define ADIOS2TYPE adios2_type_integer4
#define LENGTH Count
#include "ext_adios2_put_var_td.code"
end subroutine ext_adios2_put_var_td_integer

subroutine ext_adios2_put_var_ti_logical(DataHandle,Element,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA  logical ,intent(in) :: Data(Count)
#define TYPE_COUNT integer ,intent(in) :: Count
#define LOG
#include "ext_adios2_put_var_ti.code"
end subroutine ext_adios2_put_var_ti_logical

subroutine ext_adios2_put_var_td_logical(DataHandle,Element,DateStr,Var,Data,Count,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef ADIOS2TYPE
#undef LENGTH
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA  logical ,intent(in) :: Data(Count)
#define TYPE_COUNT integer ,intent(in) :: Count
#define ADIOS2TYPE adios2_type_integer4
#define LOG
#define LENGTH Count
#include "ext_adios2_put_var_td.code"
end subroutine ext_adios2_put_var_td_logical

subroutine ext_adios2_put_var_ti_char(DataHandle,Element,Var,Data,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef LOG
#define ROUTINE_TYPE 'CHAR'
#define TYPE_DATA  character*(*) ,intent(in) :: Data(1)
#define TYPE_COUNT
#define CHAR_TYPE
#include "ext_adios2_put_var_ti.code"
#undef CHAR_TYPE
end subroutine ext_adios2_put_var_ti_char

subroutine ext_adios2_put_var_td_char(DataHandle,Element,DateStr,Var,Data,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_COUNT
#undef ADIOS2TYPE
#undef LENGTH
#undef LOG
#define ROUTINE_TYPE 'CHAR'
#define TYPE_DATA  character*(*) ,intent(in) :: Data
#define TYPE_COUNT
#define ADIOS2TYPE adios2_type_string
#define LENGTH len(Data)
#include "ext_adios2_put_var_td.code"
end subroutine ext_adios2_put_var_td_char

subroutine ext_adios2_get_var_ti_real(DataHandle,Element,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef COPY
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA     real   ,intent(out) :: Data(Count)
#define TYPE_BUFFER   real   ,allocatable :: Buffer(:)
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define COPY   Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
#include "ext_adios2_get_var_ti.code"
  return
end subroutine ext_adios2_get_var_ti_real

subroutine ext_adios2_get_var_td_real(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef LENGTH
#undef COPY
#define ROUTINE_TYPE 'REAL'
#define TYPE_DATA     real   ,intent(out) :: Data(Count)
#define TYPE_BUFFER real
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define LENGTH min(Count,Len1)
#define COPY   Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
#include "ext_adios2_get_var_td.code"
  return
end subroutine ext_adios2_get_var_td_real

subroutine ext_adios2_get_var_ti_double(DataHandle,Element,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef COPY
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA     real*8 ,intent(out) :: Data(Count)
#define TYPE_BUFFER   real*8 ,allocatable :: Buffer(:)
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define COPY   Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
#include "ext_adios2_get_var_ti.code"
  return
end subroutine ext_adios2_get_var_ti_double

subroutine ext_adios2_get_var_td_double(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef LENGTH
#undef COPY
#define ROUTINE_TYPE 'DOUBLE'
#define TYPE_DATA     real*8 ,intent(out) :: Data(Count)
#define TYPE_BUFFER real*8
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define LENGTH min(Count,Len1)
#define COPY   Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
#include "ext_adios2_get_var_td.code"
  return
end subroutine ext_adios2_get_var_td_double

subroutine ext_adios2_get_var_ti_integer(DataHandle,Element,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef COPY
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA     integer,intent(out) :: Data(Count)
#define TYPE_BUFFER   integer,allocatable :: Buffer(:)
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define COPY   Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
#include "ext_adios2_get_var_ti.code"
  return
end subroutine ext_adios2_get_var_ti_integer

subroutine ext_adios2_get_var_td_integer(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef LENGTH
#undef COPY
#define ROUTINE_TYPE 'INTEGER'
#define TYPE_DATA     integer,intent(out) :: Data(Count)
#define TYPE_BUFFER integer
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define LENGTH min(Count,Len1)
#define COPY   Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
#include "ext_adios2_get_var_td.code"
  return
end subroutine ext_adios2_get_var_td_integer

subroutine ext_adios2_get_var_ti_logical(DataHandle,Element,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef COPY
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA     logical,intent(out) :: Data(Count)
#define TYPE_BUFFER   integer,allocatable :: Buffer(:)
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define COPY   Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))==1
#include "ext_adios2_get_var_ti.code"
  return
end subroutine ext_adios2_get_var_ti_logical

subroutine ext_adios2_get_var_td_logical(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef LENGTH
#undef COPY
#define ROUTINE_TYPE 'LOGICAL'
#define TYPE_DATA     logical,intent(out) :: Data(Count)
#define TYPE_BUFFER   integer
#define TYPE_COUNT    integer,intent(in)  :: Count
#define TYPE_OUTCOUNT integer,intent(out) :: OutCount
#define LENGTH min(Count,Len1)
#define COPY   Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))==1
#include "ext_adios2_get_var_td.code"
  return
end subroutine ext_adios2_get_var_td_logical

subroutine ext_adios2_get_var_ti_char(DataHandle,Element,Var,Data,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef COPY
#define ROUTINE_TYPE 'CHAR'
#define TYPE_DATA   character*(*) ,intent(out) :: Data
#define TYPE_BUFFER
#define TYPE_COUNT integer :: Count = 1
#define TYPE_OUTCOUNT
#define COPY 
#define CHAR_TYPE
#include "ext_adios2_get_var_ti.code"
#undef CHAR_TYPE
  return
end subroutine ext_adios2_get_var_ti_char

subroutine ext_adios2_get_var_td_char(DataHandle,Element,DateStr,Var,Data,Status)
#undef ROUTINE_TYPE
#undef TYPE_DATA
#undef TYPE_BUFFER
#undef TYPE_COUNT
#undef TYPE_OUTCOUNT
#undef LENGTH
#define ROUTINE_TYPE 'CHAR'
#define TYPE_DATA character*(*) ,intent(out)    :: Data
#define TYPE_BUFFER character (80)
#define TYPE_COUNT integer :: Count = 1
#define TYPE_OUTCOUNT
#define LENGTH Len1
#define CHAR_TYPE
#include "ext_adios2_get_var_td.code"
#undef CHAR_TYPE
  return
end subroutine ext_adios2_get_var_td_char

subroutine ext_adios2_put_dom_td_real(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real                  ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_adios2_put_var_td_real(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,Status)
  return
end subroutine ext_adios2_put_dom_td_real

subroutine ext_adios2_put_dom_td_integer(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_adios2_put_var_td_integer(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,Status)
  return
end subroutine ext_adios2_put_dom_td_integer

subroutine ext_adios2_put_dom_td_double(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real*8                ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_adios2_put_var_td_double(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'   ,Data,Count,Status)
  return
end subroutine ext_adios2_put_dom_td_double

subroutine ext_adios2_put_dom_td_logical(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  logical               ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_adios2_put_var_td_logical(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,Status)
  return
end subroutine ext_adios2_put_dom_td_logical

subroutine ext_adios2_put_dom_td_char(DataHandle,Element,DateStr,Data,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Data
  integer               ,intent(out)    :: Status

  call ext_adios2_put_var_td_char(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Status)
  return
end subroutine ext_adios2_put_dom_td_char

subroutine ext_adios2_get_dom_td_real(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real                  ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  
  call ext_adios2_get_var_td_real(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,OutCount,Status)
   return
end subroutine ext_adios2_get_dom_td_real

subroutine ext_adios2_get_dom_td_integer(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  
  call ext_adios2_get_var_td_integer(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,OutCount,Status)
   return
end subroutine ext_adios2_get_dom_td_integer

subroutine ext_adios2_get_dom_td_double(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real*8                ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  
  call ext_adios2_get_var_td_double(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'   ,Data,Count,OutCount,Status)
  return
end subroutine ext_adios2_get_dom_td_double

subroutine ext_adios2_get_dom_td_logical(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  logical               ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  
  call ext_adios2_get_var_td_logical(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,OutCount,Status)
  return
end subroutine ext_adios2_get_dom_td_logical

subroutine ext_adios2_get_dom_td_char(DataHandle,Element,DateStr,Data,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(out)    :: Data
  integer               ,intent(out)    :: Status
  
  call ext_adios2_get_var_td_char(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Status)
  return
end subroutine ext_adios2_get_dom_td_char


subroutine ext_adios2_write_field(DataHandle,DateStr,Var,Field,FieldType, &
  DomainDesc, MemoryOrdIn, Stagger,  DimNames,                            &
  DomainStart,DomainEnd,MemoryStart,MemoryEnd,PatchStart,PatchEnd,Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  integer                       ,intent(in)    :: DataHandle
  character*(*)                 ,intent(in)    :: DateStr
  character*(*)                 ,intent(in)    :: Var
  integer                       ,intent(inout) :: Field(*)
  integer                       ,intent(in)    :: FieldType
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
  integer                                      :: NDim
  character (VarNameLen)                       :: VarName
  character (3)                                :: MemO
  character (3)                                :: UCMemO
  type(adios2_variable)                        :: VarID
  type(adios2_attribute)                       :: AttributeID
  integer      ,dimension(NVarDims)            :: Length_global, Length_native
  integer      ,dimension(NVarDims)            :: Length
  integer, dimension(NVarDims)                 :: VDimIDs
  character(80),dimension(NVarDims)            :: RODimNames
  integer      ,dimension(NVarDims)            :: StoredStart
  integer(kind=8)      ,dimension(NVarDims)    :: zero
  integer(kind=8)      ,dimension(NVarDims)    :: shape_dims
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
  ! Local, possibly adjusted, copies of MemoryStart and MemoryEnd
  integer       ,dimension(NVarDims)           :: lMemoryStart, lMemoryEnd
  character(80),dimension(NVarDims+1)          :: DimNamesOut
  integer                                      :: operation_id

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
  write(msg,*)'ext_adios2_write_field: called for ',TRIM(Var)
  CALL wrf_debug( 100, msg )

!jm 20061024
  Length(1:NDim) = PatchEnd(1:NDim)-PatchStart(1:NDim)+1
  Length_native(1:NDim) = Length(1:NDim)
  Length_global(1:NDim) = DomainEnd(1:NDim)-DomainStart(1:NDim)+1

  call ExtOrder(MemoryOrder,Length,Status)
  call ExtOrder(MemoryOrder,Length_global,Status)
  call ExtOrderStr(MemoryOrder,DimNames,RODimNames,Status)
  
  lMemoryStart(1:NDim) = MemoryStart(1:NDim)
  lMemoryEnd(1:NDim) = MemoryEnd(1:NDim)
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
            call adios2_define_attribute(DH%DimIDs(i), DH%adios2IO, '_DIM_'//DH%DimNames(i), &
              Length_global(j), stat)
            call adios2_err(stat,Status)
            if(Status /= WRF_NO_ERR) then
              write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__
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
              call adios2_define_attribute(DH%DimIDs(i), DH%adios2IO, '_DIM_'//DH%DimNames(i), &
              Length_global(j), stat)
              call adios2_err(stat,Status)
              if(Status /= WRF_NO_ERR) then
                write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__
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
      VDimIDs(j) = i
      DH%VarDimLens(j,NVar) = Length_global(j)
    enddo
    select case (FieldType)
      case (WRF_REAL)
        XType = adios2_type_real
      case (WRF_DOUBLE)
        Xtype = adios2_type_dp
      case (WRF_INTEGER)
        XType = adios2_type_integer4
      case (WRF_LOGICAL)
        XType = adios2_type_integer4
      case default
        Status = WRF_WARN_DATA_TYPE_NOT_FOUND
        write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
    end select
    zero(:) = 0
    shape_dims(:) = Length_global(:)
    if(NDim == 0) then
      shape_dims(:) = 1
      call adios2_define_variable(VarID, DH%adios2IO, VarName, XType, &
                               1, shape_dims, zero, zero, &
                               adios2_variable_dims, stat)
    else
      call adios2_define_variable(VarID, DH%adios2IO, VarName, XType, &
                               NDim, shape_dims, zero, zero, &
                              adios2_variable_dims, stat)
    endif
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_adios2_write_field: adios2 error for ',TRIM(VarName),' in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    if (DH%compress_operator%valid .eqv. .true.) then
      if (DH%blosc_compressor == 'blosclz') then
        call adios2_add_operation(operation_id, VarID, DH%compress_operator, 'compressor', 'blosclz', stat)
      elseif (DH%blosc_compressor == 'zlib') then
        call adios2_add_operation(operation_id, VarID, DH%compress_operator, 'compressor', 'zlib', stat)
      elseif (DH%blosc_compressor == 'lz4') then
        call adios2_add_operation(operation_id, VarID, DH%compress_operator, 'compressor', 'lz4', stat)
      elseif (DH%blosc_compressor == 'lz4hc') then
        call adios2_add_operation(operation_id, VarID, DH%compress_operator, 'compressor', 'lz4hc', stat)
      elseif (DH%blosc_compressor == 'zstd') then
          call adios2_add_operation(operation_id, VarID, DH%compress_operator, 'compressor', 'zstd', stat)
      endif
      call adios2_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'ext_adios2_write_field: adios2 error for ',TRIM(VarName),' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    endif
    DH%VarIDs(NVar) = VarID
    ! add attribute of dimension names (for reconstructing NetCDF file with converter)
    do j = 1,NDim
      DimNamesOut(j) = DH%DimNames(VDimIDs(j))
    end do
    DimNamesOut(NDim+1) = DH%DimUnlimName  
    call adios2_define_attribute(AttributeID,DH%adios2IO, 'Dims', &
              DimNamesOut, NDim+1, VarID%name, '/', stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_adios2_write_field: adios2 error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call adios2_define_attribute(AttributeID,DH%adios2IO, 'FieldType', &
              FieldType, VarID%name, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_adios2_write_field: adios2 error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call reorder(MemoryOrder,MemO)
    call uppercase(MemO,UCMemO)
    call adios2_define_attribute(AttributeID, DH%adios2IO, 'MemoryOrder', &
             UCMemO, VarID%name, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_adios2_write_field: adios2 error in ',__FILE__,', line', __LINE__ 
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
    call Transpose('write',MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
                                            ,XField,x1,x2,y1,y2,z1,z2 &
                                                   ,i1,i2,j1,j2,k1,k2 )
    StoredStart(1:NDim) = PatchStart(1:NDim)
    call ExtOrder(MemoryOrder,StoredStart,Status)
    call FieldIO('write',DataHandle,DateStr,StoredStart,Length,MemoryOrder, &
                  FieldType,VarID,XField,Status)
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
end subroutine ext_adios2_write_field

subroutine ext_adios2_read_field(DataHandle,DateStr,Var,Field,FieldType,  &
    DomainDesc, MemoryOrdIn, Stagger, DimNames,                           &
    DomainStart,DomainEnd,MemoryStart,MemoryEnd,PatchStart,PatchEnd,Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  integer                       ,intent(in)    :: DataHandle
  character*(*)                 ,intent(in)    :: DateStr
  character*(*)                 ,intent(in)    :: Var
  integer                       ,intent(out)   :: Field(*)
  integer                       ,intent(in)    :: FieldType
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrdIn
  character*(*)                 ,intent(in)    :: Stagger ! Dummy for now
  character*(*) , dimension (*) ,intent(in)    :: DimNames
  integer ,dimension(*)         ,intent(in)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status
  character (3)                                :: MemoryOrder
  type(wrf_data_handle)         ,pointer       :: DH
  integer                                      :: NDim
  character (VarNameLen)                       :: VarName
  type(adios2_variable)                        :: VarID
  integer ,dimension(NVarDims)                 :: VCount
  integer ,dimension(NVarDims)                 :: VStart
  integer ,dimension(NVarDims)                 :: Length
  integer ,dimension(NVarDims)                 :: VDimIDs
  integer ,dimension(NVarDims)                 :: MemS
  integer ,dimension(NVarDims)                 :: MemE
  integer ,dimension(NVarDims)                 :: StoredStart
  integer ,dimension(NVarDims)                 :: StoredLen
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
  integer                                      :: stat
  integer                                      :: di
  integer                                      :: FType
  type(adios2_attribute)                       :: attribute

  MemoryOrder = trim(adjustl(MemoryOrdIn))
  call GetDim(MemoryOrder,NDim,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning BAD MEMORY ORDER |',TRIM(MemoryOrder),'| for |', &
                 TRIM(Var),'| in ext_adios2_read_field ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR |',TRIM(DateStr),'| for |',TRIM(Var), &
                 '| in ext_adios2_read_field ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_adios2_read_field ',__FILE__,', line', __LINE__
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
    Length(1:NDim) = PatchEnd(1:NDim)-PatchStart(1:NDim)+1
    StoredStart(1:NDim) = PatchStart(1:NDim)
    call ExtOrder(MemoryOrder,Length,Status)
    call adios2_inquire_variable(VarID, DH%adios2IO, VarName, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__,' Varname ',Varname
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call adios2_inquire_variable_attribute(attribute, DH%adios2IO, 'FieldType', VarName, '/', stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call adios2_attribute_data(FType, attribute, stat)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__ 
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
    StoredDim = VarID%ndims
    XType = VarID%type
    select case (FieldType)
      case (WRF_REAL)
! allow coercion between double and single prec real
        if(.NOT. (XType == adios2_type_real .OR. XType == adios2_type_dp ) )  then
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning REAL TYPE MISMATCH in ',__FILE__,', line', __LINE__
        endif
      case (WRF_DOUBLE)
! allow coercion between double and single prec real
        if(.NOT. (XType == adios2_type_real .OR. XType == adios2_type_dp) )  then
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning DOUBLE TYPE MISMATCH in ',__FILE__,', line', __LINE__
        endif
      case (WRF_INTEGER)
        if(XType /= adios2_type_integer4)  then 
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning INTEGER TYPE MISMATCH in ',__FILE__,', line', __LINE__
        endif
      case (WRF_LOGICAL)
        if(XType /= adios2_type_integer4)  then
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
                  FieldType,VarID,XField,Status)
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
end subroutine ext_adios2_read_field

subroutine ext_adios2_inquire_opened( DataHandle, FileName , FileStatus, Status )
  use wrf_data_adios2
  use ext_adios2_support_routines
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
end subroutine ext_adios2_inquire_opened

subroutine ext_adios2_inquire_filename( Datahandle, FileName,  FileStatus, Status )
  use wrf_data_adios2
  use ext_adios2_support_routines
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
end subroutine ext_adios2_inquire_filename

subroutine ext_adios2_set_time(DataHandle, DateStr, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
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
end subroutine ext_adios2_set_time

subroutine ext_adios2_get_next_time(DataHandle, DateStr, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
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
      write(msg,*) 'Warning ext_adios2_get_next_time: DH%CurrentTime >= DH%NumberTimes ',DH%CurrentTime,DH%NumberTimes
      call wrf_debug ( WARN , TRIM(msg))
      Status = WRF_WARN_TIME_EOF
      return
    endif
    DH%CurrentTime     = DH%CurrentTime + 1
    DateStr            = DH%Times(DH%CurrentTime)
    DH%CurrentVariable = 0
    Status = WRF_NO_ERR
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_adios2_get_next_time

subroutine ext_adios2_get_previous_time(DataHandle, DateStr, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
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
end subroutine ext_adios2_get_previous_time

subroutine ext_adios2_get_next_var(DataHandle, VarName, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  implicit none
  include 'wrf_status_codes.h'
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
end subroutine ext_adios2_get_next_var

subroutine ext_adios2_end_of_frame(DataHandle, Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH

  call GetDH(DataHandle,DH,Status)
  return
end subroutine ext_adios2_end_of_frame

subroutine ext_adios2_get_var_info(DataHandle,Name,NDim,MemoryOrder,Stagger,DomainStart,DomainEnd,WrfType,Status)
  use wrf_data_adios2
  use ext_adios2_support_routines
  use adios2
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)          :: DataHandle
  character*(*)         ,intent(in)          :: Name
  integer               ,intent(out)         :: NDim
  character*(*)         ,intent(out)         :: MemoryOrder
  character*(*)                              :: Stagger ! Dummy for now
  integer ,dimension(*) ,intent(out)         :: DomainStart, DomainEnd
  integer               ,intent(out)         :: WrfType
  integer               ,intent(out)         :: Status
  type(wrf_data_handle) ,pointer             :: DH
  type(adios2_variable)                      :: VarID
  integer ,dimension(NVarDims)               :: VDimIDs
  integer                                    :: j
  integer                                    :: stat
  integer                                    :: XType
  type(adios2_attribute)                     :: attribute
  integer(kind=8), dimension(:), allocatable :: shape_dims
  integer                                    :: ndims_adios2

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
    call adios2_inquire_variable(VarID, DH%adios2IO, Name, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    XType = VarID%type
    call adios2_inquire_variable_attribute(attribute, DH%adios2IO, 'FieldType', Name, '/', stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call adios2_attribute_data(WrfType, attribute, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    select case (XType)
      case (adios2_type_character)
        Status = WRF_WARN_BAD_DATA_TYPE
        write(msg,*) 'Warning CHAR IS BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      case (adios2_type_integer4)
        if(WrfType /= WRF_INTEGER .and. WrfType /= WRF_LOGICAL) then
          Status = WRF_WARN_BAD_DATA_TYPE
          write(msg,*) 'Warning BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
          call wrf_debug ( WARN , TRIM(msg))
          return
        endif
      case (adios2_type_real)
        if(WrfType /= WRF_REAL) then
          Status = WRF_WARN_BAD_DATA_TYPE
          write(msg,*) 'Warning BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
          call wrf_debug ( WARN , TRIM(msg))
          return
        endif
      case (adios2_type_dp)
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
    call adios2_inquire_variable_attribute(attribute, DH%adios2IO, 'MemoryOrder', Name, '/', stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call adios2_attribute_data(MemoryOrder, attribute, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call GetDim(MemoryOrder,NDim,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning BAD MEMORY ORDER ',TRIM(MemoryOrder),' in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call adios2_variable_shape(shape_dims, ndims_adios2, VarID, stat)
    call adios2_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'adios2 error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    DomainEnd(1:NDim) = shape_dims(1:NDim)
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_adios2_get_var_info
