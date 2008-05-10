!/***************************************************************************
!* The HDF5 WRF IO module was written by the the HDF Group at NCSA, the     *
!* National Center for Supercomputing Applications.                         *
!*     HDF Group                                                            *
!*     National Center for Supercomputing Applications                      *
!*     University of Illinois at Urbana-Champaign                           *
!*     605 E. Springfield, Champaign IL 61820                               *
!*     http://hdf.ncsa.uiuc.edu/                                            *
!*                                                                          *
!* Copyright 2004 by the Board of Trustees, University of Illinois,         *
!*                                                                          *
!* Redistribution or use of this IO module, with or without modification,   *
!* is permitted for any purpose, including commercial  purposes.            *
!*                                                                          *
!* This software is an unsupported prototype.  Use at your own risk.        *
!*     http://hdf.ncsa.uiuc.edu/apps/WRF-ROMS                               *
!*                                                                          *
!* This work was funded by the MEAD expedition at the National Center       *
!* for Supercomputing Applications, NCSA.  For more information see:        *
!*     http://www.ncsa.uiuc.edu/expeditions/MEAD                            *
!*                                                                          *
!*                                                                          *
!****************************************************************************/

module wrf_phdf5_data

  use HDF5
  integer                , parameter      :: FATAL            = 1
  integer                , parameter      :: WARN             = 1
  integer                , parameter      :: WrfDataHandleMax = 99
  integer                , parameter      :: MaxDims          = 2000 ! = NF_MAX_VARS
  integer                , parameter      :: MaxTabDims       = 100  ! temporary,changable
  integer                , parameter      :: MaxVars          = 2000
  integer                , parameter      :: MaxTimes         = 9999 ! temporary, changable
  integer                , parameter      :: MaxTimeSLen      = 6    ! not exceed 1,000,000 timestamp
  integer                , parameter      :: DateStrLen       = 19
  integer                , parameter      :: VarNameLen       = 31
  integer                , parameter      :: NO_DIM           = 0
  integer                , parameter      :: NVarDims         = 4
  integer                , parameter      :: NMDVarDims       = 2
  integer                , parameter      :: CompDsetSize     = 64256 ! set to 63K
  character (8)          , parameter      :: NO_NAME          = 'NULL'
  character(4)           , parameter      :: hdf5_true        ='TRUE'
  character(5)           , parameter      :: hdf5_false       ='FALSE'
  integer                , parameter      :: MemOrdLen        = 3
  character (DateStrLen) , parameter      :: ZeroDate = '0000-00-00-00:00:00'

#include "wrf_io_flags.h"
! This is a hack.  WRF IOAPI no longer supports WRF_CHARACTER.  Rip this out!  
  integer, parameter  :: WRF_CHARACTER                        = 1080

  character (120)                         :: msg

  ! derived data type for dimensional table
  type :: dim_scale
     character (len = 256) :: dim_name
     integer              :: length
     integer              :: unlimited
  end type dim_scale

  type :: wrf_phdf5_data_handle
     character (256)                        :: FileName
     integer                               :: FileStatus
     integer                               :: Comm
     integer(hid_t)                        :: FileID
     integer(hid_t)                        :: GroupID
     integer(hid_t)                        :: DimGroupID
     integer(hid_t)                        :: EnumID
     character (256)                       :: GroupName 
     character (256)                       :: DimGroupName 
     logical                               :: Free
     logical                               :: Write
     character (5)                         :: TimesName
     integer                               :: TimeIndex
     integer                               :: MaxTimeCount
     integer                               :: CurrentTime  !Only used for read
     integer                               :: NumberTimes  !Only used for read
     character (DateStrLen), pointer       :: Times(:)
     integer(hid_t)                        :: TimesID
     integer(hid_t)                        :: str_id
     integer               , pointer       :: DimLengths(:)
     integer               , pointer       :: DimIDs(:)
     character (31)        , pointer       :: DimNames(:)
     integer                               :: DimUnlimID
     character (9)                         :: DimUnlimName
     type (dim_scale)      , pointer       :: DIMTABLE(:)
     integer       , dimension(NVarDims)   :: DimID
     integer       , dimension(NVarDims)   :: Dimension
     !     integer               , pointer       :: MDDsetIDs(:)
     integer               , pointer       :: MDVarDimLens(:)
     character (256)        , pointer       :: MDVarNames(:)
     integer(hid_t)        , pointer       :: TgroupIDs(:)
     integer(hid_t)        , pointer       :: DsetIDs(:)
     integer(hid_t)        , pointer       :: MDDsetIDs(:)
     !     integer(hid_t)                        :: DimTableID
     integer               , pointer       :: VarDimLens(:,:)
     character (VarNameLen), pointer       :: VarNames(:)
     integer                               :: CurrentVariable  !Only used for read
     integer                               :: NumVars
! first_operation is set to .TRUE. when a new handle is allocated
! or when open-for-write or open-for-read are committed.  It is set
! to .FALSE. when the first field is read or written.
     logical                               :: first_operation
  end type wrf_phdf5_data_handle
  type(wrf_phdf5_data_handle),target        :: WrfDataHandles(WrfDataHandleMax)

end module wrf_phdf5_data


module ext_phdf5_support_routines

  implicit none

CONTAINS

  subroutine allocHandle(DataHandle,DH,Comm,Status)

    use wrf_phdf5_data
    use HDF5
    include 'wrf_status_codes.h'

    integer              ,intent(out) :: DataHandle
    type(wrf_phdf5_data_handle),pointer:: DH
    integer              ,intent(IN)  :: Comm
    integer              ,intent(out) :: Status
    integer                           :: i
    integer                           :: j
    integer                           :: stat
    integer(hid_t)                    :: enum_type  
    !    character (256)                    :: NullName

    !    NullName = char(0)

    do i=1,WrfDataHandleMax
       if(WrfDataHandles(i)%Free) then
          DH => WrfDataHandles(i)
          DataHandle = i
          DH%MaxTimeCount = 1

          DH%FileID = -1
          DH%GroupID = -1
          DH%DimGroupID = -1

          call SetUp_EnumID(enum_type,Status)
          if(Status /= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal enum ALLOCATION ERROR in ',__FILE__,', line',__LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif
          DH%EnumID = enum_type

          allocate(DH%Times(MaxTimes), STAT=stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line',__LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif
          ! wait in the future
          !          DH%Times(1:MaxTimes) = NullName

          allocate(DH%DimLengths(MaxDims), STAT=stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',"__FILE__",', line',__LINE__

             call wrf_debug ( FATAL , msg)
             return
          endif

          allocate(DH%DimIDs(MaxDims), STAT=stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',"__FILE__",', line', __LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif

          allocate(DH%DimNames(MaxDims), STAT=stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',"__FILE__",', line', __LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif

          allocate(DH%DIMTABLE(MaxTabDims), STAT = stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',"__FILE__",', line', __LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif

          do j =1,MaxTabDims
             DH%DIMTABLE(j)%dim_name = NO_NAME
             DH%DIMTABLE(j)%unlimited = -1
          enddo

          allocate(DH%MDDsetIDs(MaxVars), STAT=stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',"__FILE__",', line', __LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif

          allocate(DH%MDVarDimLens(MaxVars), STAT=stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',"__FILE__",', line', __LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif

          allocate(DH%MDVarNames(MaxVars), STAT=stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',"__FILE__",', line', __LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif

          allocate(DH%DsetIDs(MaxVars), STAT=stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',"__FILE__",', line', __LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif
          DH%DsetIDs = -1

          allocate(DH%TgroupIDs(MaxTimes), STAT=stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',"__FILE__",', line', __LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif
          DH%TgroupIDs = -1

          allocate(DH%VarDimLens(NVarDims-1,MaxVars), STAT=stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',"__FILE__",', line', __LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif

          allocate(DH%VarNames(MaxVars), STAT=stat)
          if(stat/= 0) then
             Status = WRF_HDF5_ERR_ALLOCATION
             write(msg,*) 'Fatal ALLOCATION ERROR in ',"__FILE__",', line', __LINE__
             call wrf_debug ( FATAL , msg)
             return
          endif
          exit
       endif

       if(i==WrfDataHandleMax) then
          Status = WRF_HDF5_ERR_TOO_MANY_FILES
          write(msg,*) 'Warning TOO MANY FILES in ',"__FILE__",', line', __LINE__
          call wrf_debug ( WARN , msg)
          return
       endif
    enddo


    DH%Free      =.false.
    DH%Comm      = Comm
    DH%Write     =.false.
    DH%first_operation  = .TRUE.
    Status       = WRF_NO_ERR
  end subroutine allocHandle

  ! Obtain data handler
  subroutine GetDH(DataHandle,DH,Status)

    use wrf_phdf5_data
    include 'wrf_status_codes.h'
    integer               ,intent(in)          :: DataHandle
    type(wrf_phdf5_data_handle) ,pointer        :: DH
    integer               ,intent(out)         :: Status

    if(DataHandle < 1 .or. DataHandle > WrfDataHandleMax) then
       Status = WRF_HDF5_ERR_BAD_DATA_HANDLE
       return
    endif
    DH => WrfDataHandles(DataHandle)
    if(DH%Free) then
       Status = WRF_HDF5_ERR_BAD_DATA_HANDLE
       return
    endif
    Status = WRF_NO_ERR
    return
  end subroutine GetDH

  ! Set up eumerate datatype for possible logical type
  subroutine SetUp_EnumID(enum_type,Status)

    use wrf_phdf5_data
    use HDF5
    implicit none
    include 'wrf_status_codes.h'
    integer(hid_t)              ,intent(out)  :: enum_type
    integer                     ,intent(out)  :: Status
    integer                                   :: hdf5err
    integer, dimension(2)                     :: data

    data(1) = 1
    data(2) = 0

    call h5tenum_create_f(H5T_NATIVE_INTEGER,enum_type,hdf5err)
    if(hdf5err.lt.0) then 
       Status =  WRF_HDF5_ERR_DATATYPE
       write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
       call wrf_debug ( WARN , msg) 
       return
    endif

    call h5tenum_insert_f(enum_type,hdf5_true,data(1),hdf5err)
    if(hdf5err.lt.0) then 
       Status =  WRF_HDF5_ERR_DATATYPE
       write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
       call wrf_debug ( WARN , msg) 
       return
    endif

    call h5tenum_insert_f(enum_type,hdf5_false,data(2),Status)
    if(hdf5err.lt.0) then 
       Status =  WRF_HDF5_ERR_DATATYPE
       write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
       call wrf_debug ( WARN , msg) 
       return
    endif

    Status = WRF_NO_ERR
    return
  end subroutine SetUp_EnumID

! Returns .TRUE. iff it is OK to write time-independent domain metadata to the
! file referenced by DataHandle.  If DataHandle is invalid, .FALSE. is
! returned.
LOGICAL FUNCTION phdf5_ok_to_put_dom_ti( DataHandle )
    use wrf_phdf5_data
    include 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle
    CHARACTER*80 :: fname
    INTEGER :: filestate
    INTEGER :: Status
    LOGICAL :: dryrun, first_output, retval
    call ext_phdf5_inquire_filename( DataHandle, fname, filestate, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
                   ', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      dryrun       = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
      first_output = phdf5_is_first_operation( DataHandle )
      retval = .NOT. dryrun .AND. first_output
    ENDIF
    phdf5_ok_to_put_dom_ti = retval
    RETURN
END FUNCTION phdf5_ok_to_put_dom_ti

! Returns .TRUE. iff it is OK to read time-independent domain metadata from the
! file referenced by DataHandle.  If DataHandle is invalid, .FALSE. is
! returned.
LOGICAL FUNCTION phdf5_ok_to_get_dom_ti( DataHandle )
    use wrf_phdf5_data
    include 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle
    CHARACTER*80 :: fname
    INTEGER :: filestate
    INTEGER :: Status
    LOGICAL :: dryrun, retval
    call ext_phdf5_inquire_filename( DataHandle, fname, filestate, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
                   ', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      dryrun       = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
      retval = .NOT. dryrun
    ENDIF
    phdf5_ok_to_get_dom_ti = retval
    RETURN
END FUNCTION phdf5_ok_to_get_dom_ti

! Returns .TRUE. iff nothing has been read from or written to the file
! referenced by DataHandle.  If DataHandle is invalid, .FALSE. is returned.
LOGICAL FUNCTION phdf5_is_first_operation( DataHandle )
    use wrf_phdf5_data
    INCLUDE 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle
    TYPE(wrf_phdf5_data_handle) ,POINTER :: DH
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
    phdf5_is_first_operation = retval
    RETURN
END FUNCTION phdf5_is_first_operation

end module ext_phdf5_support_routines

!module wrf_phdf5_opt_data
!  integer       ,parameter  :: MaxOptVars = 100
!end module wrf_phdf5_opt_data

!module opt_data_module

!use wrf_phdf5_opt_data
!   type :: field

!	logical         :: Free
!	integer,pointer :: darrays(:)
!	integer         :: index
!   end type field
!   type(field),target :: fieldhandle(MaxOptVars)
!end module opt_data_module

!module opt_support_module
!   implicit none
!contains
!   subroutine alloc_opt_handle(ODH)
!   use opt_data_module
!   type(field),pointer::DH
!   integer            :: i

!   do i =1,MaxOptVars
!	DH=>fieldhandle(i)
!        DH%index = 0
!   enddo
!end module opt_support_module

! check the date, only use the length 
subroutine DateCheck(Date,Status)
  use wrf_phdf5_data
  include 'wrf_status_codes.h'
  character*(*) ,intent(in)      :: Date
  integer       ,intent(out)     :: Status

  if(len(Date) /= DateStrLen) then
     Status = WRF_HDF5_ERR_DATESTR_BAD_LENGTH
  else  
     Status = WRF_NO_ERR
  endif
  return
end subroutine DateCheck

! This routine is for meta-data time dependent varible attribute 
subroutine GetName(Element,Var,Name,Status)

  use wrf_phdf5_data
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

! Obtain TimeIndex 
subroutine GetDataTimeIndex(IO,DataHandle,DateStr,TimeIndex,Status)

  use HDF5
  use wrf_phdf5_data
  use ext_phdf5_support_routines

  implicit none
  include 'wrf_status_codes.h'

  character (*)         ,intent(in)          :: IO
  integer               ,intent(in)          :: DataHandle
  character*(*)         ,intent(in)          :: DateStr
  character (DateStrLen), pointer            :: TempTimes(:)
  integer               ,intent(out)         :: TimeIndex
  integer               ,intent(out)         :: Status

  type(wrf_phdf5_data_handle) ,pointer        :: DH
  integer                                    :: VStart(2)
  integer                                    :: VCount(2)
  integer                                    :: stat
  integer                                    :: i
  integer                                    :: PreTimeCount

  integer                                    :: rank
  integer(hsize_t), dimension(1)             :: chunk_dims =(/1/)
  integer(hsize_t), dimension(1)             :: dims
  integer(hsize_t), dimension(1)             :: hdf5_maxdims
  integer(hsize_t), dimension(1)             :: offset
  integer(hsize_t), dimension(1)             :: count
  integer(hsize_t), dimension(1)             :: sizes

  INTEGER(HID_T)                             :: dset_id   ! Dataset ID 
  INTEGER(HID_T)                             :: dspace_id ! Dataspace ID
  INTEGER(HID_T)                             :: fspace_id ! Dataspace ID
  INTEGER(HID_T)                             :: crp_list  ! chunk ID
  integer(hid_t)                             :: str_id    ! string ID
  integer                                    :: hdf5err

  integer(hid_t)                             :: group_id
  character(Len = 512)                       :: groupname

  ! for debug

  character(len=100)                        :: buf
  integer(size_t)                            :: name_size
  integer(size_t)                            :: datelen_size
  ! suppose the output will not exceed 100,0000 timesteps.
  character(Len = MaxTimeSLen)               :: tname


  !  DH => WrfDataHandles(DataHandle), don't know why NetCDF doesn't use GetDH
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     Status =  WRF_HDF5_ERR_DATESTR_ERROR
     write(msg,*) 'Warning DATE STRING ERROR in ',"__FILE__",', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  if(IO == 'write') then
     TimeIndex = DH%TimeIndex
     if(TimeIndex <= 0) then
        TimeIndex = 1
     elseif(DateStr < DH%Times(TimeIndex)) then
        Status = WRF_HDF5_ERR_DATE_LT_LAST_DATE
        write(msg,*) 'Warning DATE < LAST DATE in ',"__FILE__",', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
     elseif(DateStr == DH%Times(TimeIndex)) then
        Status = WRF_NO_ERR
        return
     else
        TimeIndex = TimeIndex + 1
        !       If exceeding the maximum timestep, updating the maximum timestep
        if(TimeIndex > MaxTimes*(DH%MaxTimeCount)) then
           PreTimeCount = DH%MaxTimeCount
           allocate(TempTimes(PreTimeCount*MaxTimes))
           TempTimes(1:MaxTimes*PreTimeCount)=DH%Times(1:MaxTimes &
                *PreTimeCount)
           DH%MaxTimeCount = DH%MaxTimeCount +1
           deallocate(DH%Times)
           allocate(DH%Times(DH%MaxTimeCount*MaxTimes))
           DH%Times(1:MaxTimes*PreTimeCount)=TempTimes(1:MaxTimes &
                *PreTimeCount)
           deallocate(TempTimes)
        endif
     endif
     DH%TimeIndex        = TimeIndex
     DH%Times(TimeIndex) = DateStr
     !    From NetCDF implementation, keep it in case it can be used.
     !     VStart(1) = 1
     !     VStart(2) = TimeIndex
     !     VCount(1) = DateStrLen
     !     VCount(2) = 1

     ! create memory dataspace id and file dataspace id
     dims(1)   = 1
     count(1)  = 1
     offset(1) = TimeIndex -1
     sizes(1)  = TimeIndex

     ! create group id for different time stamp
     call numtochar(TimeIndex,tname)
     groupname = 'TIME_STAMP_'//tname
!     call h5gn_members_f(DH%GroupID,DH%GroupName,nmembers,hdf5err)
!     do i = 0, nmembers - 1
!        call h5gget_obj_info_idx_f(DH%GroupID,DH%GroupName,i,ObjName, ObjType, &
!                                   hdf5err)
        
!        if(ObjName(1:17) == groupname) then
!          call h5gopen_f(DH%GroupID,groupname,tgroupid,hdf5err)
!          exit
!        endif
!     enddo 
           
     if(DH%Tgroupids(TimeIndex) == -1) then 
       call h5gcreate_f(DH%groupid,groupname,group_id,hdf5err)
       if(hdf5err .lt. 0) then
        Status = WRF_HDF5_ERR_GROUP
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
       endif
       DH%Tgroupids(TimeIndex) = group_id
     else
!        call h5gopen_f(DH%groupid,groupname,group_id,
       group_id = DH%Tgroupids(TimeIndex) 
     endif

     call h5screate_simple_f(1,dims,dspace_id,hdf5err,dims)
     if(hdf5err.lt.0) then
        Status =  WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif


     ! create HDF5 string handler for time 
     if(TimeIndex == 1) then
        call h5tcopy_f(H5T_NATIVE_CHARACTER, str_id, hdf5err)
        if(hdf5err.lt.0) then 
           Status =  WRF_HDF5_ERR_DATATYPE
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif

        datelen_size = DateStrLen
        call h5tset_size_f(str_id,datelen_size,hdf5err)
        if(hdf5err.lt.0) then 
           Status =  WRF_HDF5_ERR_DATATYPE
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif
     else 
        str_id = DH%str_id
     endif

     call h5dcreate_f(group_id,DH%TimesName,str_id,dspace_id,&
          DH%TimesID, hdf5err)
     if(hdf5err.lt.0) then
        Status =  WRF_HDF5_ERR_DATASET_CREATE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif


     ! write the data in memory space to file space
     CALL h5dwrite_f(DH%TimesID,str_id,DateStr,dims,hdf5err,dspace_id,dspace_id)
     if(hdf5err.lt.0) then
        Status =  WRF_HDF5_ERR_DATASET_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     if(TimeIndex == 1) then
        DH%str_id = str_id
     endif


     call h5sclose_f(dspace_id,hdf5err)
     if(hdf5err.lt.0) then
        Status =  WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5dclose_f(DH%TimesID,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_GENERAL
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

  else
     ! This is for IO read
     ! Find the timeIndex(very expensive for large
     ! time stamp, should use hashing table)

     do i=1,MaxTimes*DH%MaxTimeCount

        !       For handling reading maximum timestamp greater than 9000 in the future        
        !        if(DH%Times(i) == NullName) then
        !           Status = WRF_HDF5_ERR_TIME
        !           write(msg,*) 'Warning TIME ',DateStr,' NOT FOUND in ',"__FILE__",&
        !               ', line', __LINE__
        !           call wrf_debug ( WARN , msg)
        !           return
        !        endif

        if(DH%Times(i) == DateStr) then
           Status = WRF_NO_ERR
           TimeIndex = i
           exit
        endif

        ! Need a recursive function to handle this
        ! This is a potential bug 
        if(i == MaxTimes*DH%MaxTimeCount) then
           !           PreTimeCount = DH%MaxTimeCount
           !           allocate(TempTimes(PreTimeCount*MaxTimes))
           !           TempTimes(1:MaxTimes*PreTimeCount)=DH%Times(1:MaxTimes &
           !                *PreTimeCount)
           !           DH%MaxTimeCount = DH%MaxTimeCount +1
           !           deallocate(DH%Times)
           !           allocate(DH%Times(DH%MaxTimeCount*MaxTimes))
           !           DH%Times(1:MaxTimes*PreTimeCount)=TempTimes(1:MaxTimes &
           !                *PreTimeCount)
           !           deallocate(TempTimes)
           Status = WRF_HDF5_ERR_TIME
           write(msg,*) 'Warning TIME ',DateStr,' NOT FOUND in ',"__FILE__",&
                ', line', __LINE__
           call wrf_debug ( WARN , msg)
           return
        endif
     enddo

     ! do the hyperslab selection

  endif
  return
end subroutine GetDataTimeIndex

subroutine GetAttrTimeIndex(IO,DataHandle,DateStr,TimeIndex,Status)

  use HDF5
  use wrf_phdf5_data
  use ext_phdf5_support_routines

  implicit none
  include 'wrf_status_codes.h'

  character (*)         ,intent(in)          :: IO
  integer               ,intent(in)          :: DataHandle
  character*(*)         ,intent(in)          :: DateStr
  character (DateStrLen), pointer            :: TempTimes(:)
  integer               ,intent(out)         :: TimeIndex
  integer               ,intent(out)         :: Status

  type(wrf_phdf5_data_handle) ,pointer        :: DH
  integer                                    :: VStart(2)
  integer                                    :: VCount(2)
  integer                                    :: stat
  integer                                    :: i
  integer                                    :: PreTimeCount

  integer                                    :: rank
  integer(hsize_t), dimension(1)             :: chunk_dims =(/1/)
  integer(hsize_t), dimension(1)             :: dims
  integer(hsize_t), dimension(1)             :: hdf5_maxdims
  integer(hsize_t), dimension(1)             :: offset
  integer(hsize_t), dimension(1)             :: count
  integer(hsize_t), dimension(1)             :: sizes

  INTEGER(HID_T)                             :: dset_id   ! Dataset ID 
  INTEGER(HID_T)                             :: dspace_id ! Dataspace ID
  INTEGER(HID_T)                             :: fspace_id ! Dataspace ID
  INTEGER(HID_T)                             :: crp_list  ! chunk ID
  integer(hid_t)                             :: str_id    ! string ID
  integer                                    :: hdf5err

  integer(size_t)                            :: datelen_size
  integer(hid_t)                             :: group_id
  character(Len = 512)                       :: groupname

  ! suppose the output will not exceed 100,0000 timesteps. 
  character(Len = MaxTimeSLen)               :: tname  

  !  DH => WrfDataHandles(DataHandle), don't know why NetCDF doesn't use GetDH
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     Status =  WRF_HDF5_ERR_DATESTR_ERROR
     write(msg,*) 'Warning DATE STRING ERROR in ',"__FILE__",', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  if(IO == 'write') then
     TimeIndex = DH%TimeIndex
     if(TimeIndex <= 0) then
        TimeIndex = 1
     elseif(DateStr < DH%Times(TimeIndex)) then
        Status = WRF_HDF5_ERR_DATE_LT_LAST_DATE
        write(msg,*) 'Warning DATE < LAST DATE in ',"__FILE__",', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
     elseif(DateStr == DH%Times(TimeIndex)) then
        Status = WRF_NO_ERR
        return
     else
        TimeIndex = TimeIndex + 1
        !       If exceeding the maximum timestep, updating the maximum timestep
        if(TimeIndex > MaxTimes*(DH%MaxTimeCount)) then
           PreTimeCount = DH%MaxTimeCount
           allocate(TempTimes(PreTimeCount*MaxTimes))
           TempTimes(1:MaxTimes*PreTimeCount)=DH%Times(1:MaxTimes &
                *PreTimeCount)
           DH%MaxTimeCount = DH%MaxTimeCount +1
           deallocate(DH%Times)
           allocate(DH%Times(DH%MaxTimeCount*MaxTimes))
           DH%Times(1:MaxTimes*PreTimeCount)=TempTimes(1:MaxTimes &
                *PreTimeCount)
           deallocate(TempTimes)
        endif
     endif
     DH%TimeIndex        = TimeIndex
     DH%Times(TimeIndex) = DateStr

     !    From NetCDF implementation, keep it in case it can be used.
     !     VStart(1) = 1
     !     VStart(2) = TimeIndex
     !     VCount(1) = DateStrLen
     !     VCount(2) = 1

     ! create memory dataspace id and file dataspace id
     dims(1)   = 1
     count(1)  = 1
     offset(1) = TimeIndex -1
     sizes(1)  = TimeIndex

     ! create group id for different time stamp
     call numtochar(TimeIndex,tname)
     groupname = 'TIME_STAMP_'//tname
           
     if(DH%Tgroupids(TimeIndex) == -1) then 
       call h5gcreate_f(DH%groupid,groupname,group_id,hdf5err)
       if(hdf5err .lt. 0) then
        Status = WRF_HDF5_ERR_GROUP
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
       endif
       DH%Tgroupids(TimeIndex) = group_id
     else
!        call h5gopen_f(DH%groupid,groupname,group_id,
       group_id = DH%Tgroupids(TimeIndex) 
     endif

     call h5screate_simple_f(1,dims,dspace_id,hdf5err,dims)
     if(hdf5err.lt.0) then
        Status =  WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif


     ! create HDF5 string handler for time 
     if(TimeIndex == 1) then
        call h5tcopy_f(H5T_NATIVE_CHARACTER, str_id, hdf5err)
        if(hdf5err.lt.0) then 
           Status =  WRF_HDF5_ERR_DATATYPE
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif

        datelen_size = DateStrLen
        call h5tset_size_f(str_id,datelen_size,hdf5err)
        if(hdf5err.lt.0) then 
           Status =  WRF_HDF5_ERR_DATATYPE
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif
     else 
        str_id = DH%str_id
     endif

     call h5dcreate_f(group_id,DH%TimesName,str_id,dspace_id,&
          DH%TimesID, hdf5err)
     if(hdf5err.lt.0) then
        Status =  WRF_HDF5_ERR_DATASET_CREATE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif


     ! write the data in memory space to file space
     CALL h5dwrite_f(DH%TimesID,str_id,DateStr,dims,hdf5err,dspace_id,dspace_id)
     if(hdf5err.lt.0) then
        Status =  WRF_HDF5_ERR_DATASET_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     if(TimeIndex == 1) then
        DH%str_id = str_id
     endif


     call h5sclose_f(dspace_id,hdf5err)
     if(hdf5err.lt.0) then
        Status =  WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5dclose_f(DH%TimesID,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_GENERAL
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

  else
     ! This is for IO read
     ! Find the timeIndex(very expensive for large
     ! time stamp, should use hashing table)

     do i=1,MaxTimes*DH%MaxTimeCount


        if(DH%Times(i) == DateStr) then
           Status = WRF_NO_ERR
           TimeIndex = i
           exit
        endif

        ! Need a recursive function to handle this
        ! This is a potential bug 
        if(i == MaxTimes*DH%MaxTimeCount) then
           Status = WRF_HDF5_ERR_TIME
           write(msg,*) 'Warning TIME ',DateStr,' NOT FOUND in ',"__FILE__",&
                ', line', __LINE__
           call wrf_debug ( WARN , msg)
           return
        endif
     enddo

     ! do the hyperslab selection

  endif
  return
end subroutine GetAttrTimeIndex


! Obtain the rank of the dimension
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
  case ('z','c','0')
     NDim = 1
  case default
     Status = WRF_HDF5_ERR_BAD_MEMORYORDER
     return
  end select
  Status = WRF_NO_ERR
  return
end subroutine GetDim

! Obtain the index for transposing
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

! shuffling the memory order to XYZ order
subroutine ExtOrder(MemoryOrder,Vector,Status)
  use wrf_phdf5_data
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
     Vector(1) = 1
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
     Status = WRF_HDF5_ERR_BAD_MEMORYORDER
     return
  end select
  Status = WRF_NO_ERR
  return
end subroutine ExtOrder

! shuffling the dimensional name order
subroutine ExtOrderStr(MemoryOrder,Vector,ROVector,Status)
  use wrf_phdf5_data
  include 'wrf_status_codes.h'
  character*(*)                    ,intent(in)    :: MemoryOrder
  character*(*),dimension(*)       ,intent(in)    :: Vector
  character(256),dimension(NVarDims),intent(out)   :: ROVector
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
     ROVector(1) = 'ext_scalar'
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
     Status = WRF_HDF5_ERR_BAD_MEMORYORDER
     return
  end select
  Status = WRF_NO_ERR
  return
end subroutine ExtOrderStr


subroutine LowerCase(MemoryOrder,MemOrd)
  character*(*) ,intent(in)  :: MemoryOrder
  character*(*) ,intent(out) :: MemOrd
  character*3                :: c
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
  character*3                :: c
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

! subroutine used in transpose routine
subroutine reorder (MemoryOrder,MemO)
  character*(*)     ,intent(in)    :: MemoryOrder
  character*3       ,intent(out)   :: MemO
  character*3                      :: MemOrd
  integer                          :: N,i,i1,i2,i3

  MemO = MemoryOrder
  N = len_trim(MemoryOrder)
  if(N == 1) return
  call lowercase(MemoryOrder,MemOrd)
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

subroutine Transpose_hdf5(IO,MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
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


  case ('xzy')

     ix=0
     jx=0
     kx=0
     call reorder(MemoryOrder,MemO)
     if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
     if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
     if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
     do k=k1,k2
        do j=j1,j2
           do i=i1,i2
              if(IO == 'write') then
                 XField(1:di,(i-i1+1+(i2-i1+1)*((k-k1)+(j-j1)*(k2-k1+1)))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
              else
                 Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,(i-i1+1+(i2-i1+1)*((k-k1)+(j-j1)*(k2-k1+1))))
              endif
           enddo
        enddo
     enddo
     return

  case ('xyz','xsz','xez','ysz','yez','xy','xs','xe','ys','ye','z','c','0')


     ix=0
     jx=0
     kx=0
     call reorder(MemoryOrder,MemO)
     if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
     if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
     if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
     do k=k1,k2
        do j=j1,j2
           do i=i1,i2
              if(IO == 'write') then
                 XField(1:di,(i-i1+1+(i2-i1+1)*((j-j1)+(k-k1)*(j2-j1+1)))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
              else
                 Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,(i-i1+1+(i2-i1+1)*((j-j1)+(k-k1)*(j2-j1+1))))
              endif
           enddo
        enddo
     enddo
     return

  case ('yxz')


     ix=0
     jx=0
     kx=0
     call reorder(MemoryOrder,MemO)
     if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
     if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
     if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
     do k=k1,k2
        do j=j1,j2
           do i=i1,i2
              if(IO == 'write') then
                 XField(1:di,(j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1)))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
              else
                 Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,(j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1))))
              endif
           enddo
        enddo
     enddo
     return

  case ('zxy')


     ix=0
     jx=0
     kx=0
     call reorder(MemoryOrder,MemO)
     if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
     if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
     if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
     do k=k1,k2
        do j=j1,j2
           do i=i1,i2
              if(IO == 'write') then
                 XField(1:di,(k-k1+1+(k2-k1+1)*((i-i1)+(j-j1)*(i2-i1+1)))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
              else
                 Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,(k-k1+1+(k2-k1+1)*((i-i1)+(j-j1)*(i2-i1+1))))
              endif
           enddo
        enddo
     enddo
     return

  case ('yzx')


     ix=0
     jx=0
     kx=0
     call reorder(MemoryOrder,MemO)
     if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
     if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
     if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
     do k=k1,k2
        do j=j1,j2
           do i=i1,i2
              if(IO == 'write') then
                 XField(1:di,(j-j1+1+(j2-j1+1)*((k-k1)+(i-i1)*(k2-k1+1)))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
              else
                 Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,(j-j1+1+(j2-j1+1)*((k-k1)+(i-i1)*(k2-k1+1))))
              endif
           enddo
        enddo
     enddo
     return

  case ('zyx')


     ix=0
     jx=0
     kx=0
     call reorder(MemoryOrder,MemO)
     if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
     if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
     if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
     do k=k1,k2
        do j=j1,j2
           do i=i1,i2
              if(IO == 'write') then
                 XField(1:di,(k-k1+1+(k2-k1+1)*((j-j1)+(i-i1)*(j2-j1+1)))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
              else
                 Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,(k-k1+1+(k2-k1+1)*((j-j1)+(i-i1)*(j2-j1+1))))
              endif
           enddo
        enddo
     enddo
     return

  case ('yx')


     ix=0
     jx=0
     kx=0
     call reorder(MemoryOrder,MemO)
     if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
     if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
     if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1
     do k=k1,k2
        do j=j1,j2
           do i=i1,i2
              if(IO == 'write') then
                 XField(1:di,(j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1)))) = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
              else
                 Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = XField(1:di,(j-j1+1+(j2-j1+1)*((i-i1)+(k-k1)*(i2-i1+1))))
              endif
           enddo
        enddo
     enddo
     return

  end select
  return
end subroutine Transpose_hdf5

subroutine numtochar(TimeIndex,tname,Status)

  use wrf_phdf5_data
  integer, intent(in) :: TimeIndex
  character(len=MaxTimeSLen),intent(out)::tname
  integer                   ,intent(out)::Status
  integer             :: i,ten_pow,temp
  integer             :: maxtimestep

  maxtimestep =1 
  do i =1,MaxTimeSLen
     maxtimestep = maxtimestep * 10
  enddo
  if(TimeIndex >= maxtimestep) then
     Status = WRF_HDF5_ERR_OTHERS
     write(msg,*) 'Cannot exceed the maximum timestep',maxtimestep,'in',__FILE__,' line',__LINE__
     call wrf_debug(FATAL,msg)
     return
  endif

  ten_pow = 1
  temp =10
  do i =1,MaxTimeSLen
     tname(MaxTimeSLen+1-i:MaxTimeSLen+1-i) = achar(modulo(TimeIndex/ten_pow,temp)+iachar('0'))
     ten_pow  = 10* ten_pow
  enddo

  return
end subroutine numtochar
