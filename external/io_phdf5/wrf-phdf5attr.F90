!***************************************************************************
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

module get_attrid_routine

  Interface get_attrid
     module  procedure get_attrid
  end interface

contains

  subroutine get_attrid(DataHandle,Element,h5_attrid,Status,VAR)

    use wrf_phdf5_data
    use ext_phdf5_support_routines
    USE HDF5 ! This module contains all necessary modules 
    implicit none
    include 'wrf_status_codes.h'

    character*(*)         ,intent(in)             :: Element
    integer               ,intent(in)             :: DataHandle
    integer(hid_t)        ,intent(out)            :: h5_attrid
    integer(hid_t)                                :: dset_id
    integer               ,intent(out)            :: Status
    character*(*)         ,intent(in),optional    :: VAR
    integer(hid_t)                                :: hdf5err
    type(wrf_phdf5_data_handle),pointer           :: DH

    character(Len = MaxTimeSLen)                  :: tname
    character(Len = 512)                          :: tgroupname
    integer(hid_t)                                :: tgroup_id

    call GetDH(DataHandle,DH,Status)
    if(Status /= WRF_NO_ERR) then
       write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
       call wrf_debug ( WARN , msg) 
       return
    endif

    if(present(VAR)) then
       call numtochar(1,tname)
       tgroupname = 'TIME_STAMP_'//tname
       call h5gopen_f(DH%GroupID,tgroupname,tgroup_id,hdf5err)
       if(hdf5err.lt.0) then 
          Status =  WRF_HDF5_ERR_GROUP
          write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , msg) 
          return
       endif
       call h5dopen_f(tgroup_id,VAR,dset_id,hdf5err)
       if(hdf5err.lt.0) then 
          Status =  WRF_HDF5_ERR_DATASET_OPEN
          write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , msg) 
          return
       endif

       call h5aopen_name_f(dset_id,Element,h5_attrid,hdf5err)
       if(hdf5err.lt.0) then 
          Status =  WRF_HDF5_ERR_ATTRIBUTE_OPEN
          write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , msg) 
          return
       endif
       call h5dclose_f(dset_id,hdf5err)
       call h5gclose_f(tgroup_id,hdf5err)
    else
       call h5aopen_name_f(DH%GroupID,Element,h5_attrid,hdf5err)
       write(*,*) "Element ",Element
       if(hdf5err.lt.0) then 
          Status =  WRF_HDF5_ERR_ATTRIBUTE_OPEN
          write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , msg) 
          return
       endif
    endif
    return
  end subroutine get_attrid
end module get_attrid_routine

subroutine create_phdf5_objid(DataHandle,obj_id,routine_type,var,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'wrf_status_codes.h'

  integer                                      :: i
  integer                       ,intent(in)    :: DataHandle
  integer(hid_t)                ,intent(out)   :: obj_id
  character*3                   ,intent(in)    :: routine_type
  character*(*)                 ,intent(in)    :: var
  integer                       ,intent(out)   :: Status
  integer(hid_t)                               :: hdf5err
  type(wrf_phdf5_data_handle),pointer           :: DH


  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ',ROUTINE_TYPE,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  if(routine_type == 'DOM') then

     if(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then
        obj_id = DH%GroupID
     endif

  else if(routine_type == 'VAR') then

     if(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then
        do i = 1, MaxVars
           if(DH%VarNames(i) == var) then
              obj_id = DH%dsetids(i)
              exit
           endif
        enddo
     endif

  else
     Status = WRF_HDF5_ERR_DATA_ID_NOTFOUND
     write(msg,*) 'CANNOT FIND DATASET ID of the attribute in',__FILE__,&
          ', line',__LINE__
  endif

  return 
end subroutine create_phdf5_objid


subroutine create_phdf5_adtypeid(h5_atypeid,routine_datatype,Count,Status,DataHandle)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'wrf_status_codes.h'

  integer                                      :: i
  integer(hid_t)                ,intent(out)   :: h5_atypeid
  integer                       ,intent(in)    :: routine_datatype
  integer                       ,intent(in)    :: Count
  integer                       ,intent(out)   :: Status
  integer(hid_t)                               :: hdf5err
  integer, intent(in), optional                :: DataHandle
  integer(size_t)                              :: count_size

  type(wrf_phdf5_data_handle),pointer           :: DH

  if(routine_datatype == WRF_LOGICAL)then
     call GetDH(DataHandle,DH,Status)
     if(Status /= WRF_NO_ERR) then
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

  endif

  select case(routine_datatype) 
  case (WRF_REAL)
     h5_atypeid = H5T_NATIVE_REAL
  case (WRF_DOUBLE)
     h5_atypeid = H5T_NATIVE_DOUBLE
  case (WRF_INTEGER)
     h5_atypeid = H5T_NATIVE_INTEGER
  case (WRF_LOGICAL)
     h5_atypeid = DH%EnumID
  case (WRF_CHARACTER)

     call h5tcopy_f(H5T_NATIVE_CHARACTER,h5_atypeid,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     count_size = count
     call h5tset_size_f(h5_atypeid,count_size,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5tset_strpad_f(h5_atypeid,H5T_STR_SPACEPAD_F,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

  case default
     Status = WRF_HDF5_ERR_DATATYPE
     return
  end select

  Status = WRF_NO_ERR

  return
end subroutine create_phdf5_adtypeid

subroutine create_phdf5_adspaceid(Count,str_flag,h5_aspaceid,Status)

  use wrf_phdf5_data
  use HDF5
  implicit none
  include 'wrf_status_codes.h'

  integer                                      :: i
  integer                       ,intent(in)    :: Count
  integer                       ,intent(in)    :: str_flag
  integer                       ,intent(out)   :: Status

  integer(hsize_t)              , dimension(1) :: adims
  integer(hid_t)                               :: hdf5err
  integer(hid_t)                ,intent(out)   :: h5_aspaceid
  integer                                      :: arank = 1

  ! if string, count is always 1
  if(str_flag == 1) then 
     adims(1) = 1
     call h5screate_simple_f(arank,adims,h5_aspaceid,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

  else
     adims(1) = Count
     call h5screate_simple_f(arank,adims,h5_aspaceid,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

  endif

  Status = WRF_NO_ERR

  return 
end subroutine create_phdf5_adspaceid


subroutine clean_phdf5_attrids(h5_attr_typeid,h5_space_typeid, &
     h5_attrid,str_flag,Status)

  use wrf_phdf5_data
  use HDF5
  implicit none
  include 'wrf_status_codes.h'
  integer                       ,intent(out)   :: Status
  integer(hid_t)                ,intent(in)    :: h5_attr_typeid
  integer(hid_t)                ,intent(in)    :: h5_space_typeid
  integer(hid_t)                ,intent(in)    :: h5_attrid
  integer                       ,intent(in)    :: str_flag
  integer                                      :: hdf5err

  if(str_flag == 1) then
     call h5tclose_f(h5_attr_typeid,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_CLOSE_GENERAL
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
  endif

  call h5sclose_f(h5_space_typeid,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_CLOSE_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif
  call h5aclose_f(h5_attrid,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CLOSE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  Status = WRF_NO_ERR

  return

end subroutine clean_phdf5_attrids


subroutine create_h5filetype(dtype_id,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use hdf5
  implicit none
  include 'wrf_status_codes.h'   

  integer(hid_t),intent(out)              :: dtype_id
  integer(hid_t)                         :: dtstr_id
  integer(size_t)                        :: type_size
  integer(size_t)                        :: type_sizes
  integer(size_t)                        :: type_sizei
  integer(size_t)                        :: offset
  integer,     intent(out)               :: Status
  integer(hid_t)                         :: hdf5err

  call h5tcopy_f(H5T_NATIVE_CHARACTER,dtstr_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  type_size = 256

  call h5tset_size_f(dtstr_id,type_size,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5tget_size_f(dtstr_id,type_sizes,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5tget_size_f(H5T_NATIVE_INTEGER,type_sizei,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  type_size = type_sizes + 2*type_sizei

  call h5tcreate_f(H5T_COMPOUND_F,type_size,dtype_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  offset = 0

  call h5tinsert_f(dtype_id,"dim_name",offset,dtstr_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  offset = offset + type_sizes
  call h5tinsert_f(dtype_id,"dim_length",offset,H5T_NATIVE_INTEGER,&
       hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  offset = offset + type_sizei

  call h5tinsert_f(dtype_id,"dim_unlimited",offset,H5T_NATIVE_INTEGER,&
       hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  call h5tclose_f(dtstr_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_CLOSE_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  Status = WRF_NO_ERR
  return 
end subroutine  create_h5filetype

! check whether two types are equal, attr_type and h5_attrid
subroutine check_type(DataHandle,attr_type,h5_attrid,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  integer(hid_t)        ,intent(in)     :: attr_type
  integer(hid_t)       ,intent(in)      :: h5_attrid
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_classid
  integer(hid_t)                        :: h5_wrfclassid
  logical                               :: flag
  integer                               :: hdf5err
  type(wrf_phdf5_data_handle),pointer    :: DH

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5aget_type_f(h5_attrid,h5_atypeid,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5tget_class_f(h5_atypeid,h5_classid,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5tget_class_f(attr_type,h5_wrfclassid,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  if((h5_classid==H5T_STRING_F).AND.&
       (attr_type==H5T_NATIVE_CHARACTER)) then 
     flag = .TRUE.
  else 
     if(h5_classid .NE. h5_wrfclassid) then
        flag = .FALSE.
     else
        flag = .TRUE.
     endif
  endif

  if(flag .EQV. .FALSE.) then
     Status = WRF_HDF5_ERR_TYPE_MISMATCH
     return 
  endif

  Status = WRF_NO_ERR
  return
end subroutine check_type


subroutine retrieve_ti_info(DataHandle,h5_attrid,h5_atypeid,Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  integer               ,intent(in)     :: h5_attrid
  integer(hid_t)        ,intent(out)    :: h5_atypeid
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_aspaceid
  integer                               :: typeclass
  integer                               :: hdf5err
  integer                               :: rank
  integer(hsize_t)                      :: npoints

  type(wrf_phdf5_data_handle),pointer    :: DH


  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  if(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then

     call h5aget_type_f(h5_attrid,h5_atypeid,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5aget_space_f(h5_attrid,h5_aspaceid,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5sget_simple_extent_ndims_f(h5_aspaceid,rank,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     if(rank > 1) then
        ! The rank can be either 0 or 1
        Status = WRF_HDF5_ERR_OTHERS
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5sget_simple_extent_npoints_f(h5_aspaceid,npoints,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     OutCount = npoints
     call h5tget_class_f(h5_atypeid,typeclass,hdf5err)
     if(hdf5err.lt.0) then
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__   
        call wrf_debug ( WARN , msg)
        return
     endif
     if((npoints > Count).and.(typeclass.ne.H5T_STRING_F)) then
        OutCount = Count
        Status = WRF_HDF5_ERR_MORE_DATA_IN_FILE
     else
        OutCount = npoints
     endif
  endif
  return
end subroutine retrieve_ti_info

subroutine setup_wrtd_dataset(DataHandle,DataSetName,dtypeid,countmd,&
     dsetid,dspace_id,fspace_id,tgroupid,   &
     TimeIndex,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: DataSetName
  integer(hid_t)        ,intent(in)             :: dtypeid
  integer               ,intent(in)             :: countmd
  integer               ,intent(in)             :: TimeIndex

  integer(hid_t)        ,intent(out)            :: dsetid
  integer(hid_t)        ,intent(out)            :: dspace_id
  integer(hid_t)        ,intent(out)            :: fspace_id
  integer(hid_t)        ,intent(out)            :: tgroupid
  integer(hid_t)                                :: crp_list
  integer               ,intent(out)            :: Status

  integer(hsize_t)      ,dimension(1)           :: sizes
  integer(hsize_t)      ,dimension(1)           :: chunk_dims
  integer(hsize_t)      ,dimension(1)           :: dims
  integer(hsize_t)      ,dimension(1)           :: hdf5_maxdims
  integer(hsize_t)      ,dimension(1)           :: offset
  integer(hsize_t)      ,dimension(1)           :: count
  type(wrf_phdf5_data_handle),pointer           :: DH

  character(Len = MaxTimeSLen)                 :: tname
  character(Len = 512)                         :: tgroupname                           
  integer                                       :: hdf5err


  ! get datahandle
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif


  chunk_dims(1)   = countmd

  dims(1)         = countmd

  count(1)        = countmd

  offset(1)       = 0

  sizes(1)        = countmd

  hdf5_maxdims(1) = countmd

  ! create the memory space id
  call h5screate_simple_f(1,dims,dspace_id,hdf5err,dims)
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! create file space(for parallel module, each dataset per time step)
  call h5screate_simple_f(1,dims,fspace_id,hdf5err,hdf5_maxdims)
  if(hdf5err.lt.0) then        
     Status =  WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! obtain the absolute name of the group where the dataset is located
  call numtochar(TimeIndex,tname)
  tgroupname = 'TIME_STAMP_'//tname
  if(DH%TgroupIDs(TimeIndex) /= -1) then
    tgroupid = DH%TgroupIDs(TimeIndex) 
!  call h5gopen_f(DH%GroupID,tgroupname,tgroupid,hdf5err)
  else 
   call h5gcreate_f(DH%GroupID,tgroupname,tgroupid,hdf5err)
   if(hdf5err.lt.0) then                                            
     Status =  WRF_HDF5_ERR_GROUP                  
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__     
     call wrf_debug(WARN,msg)
     return                                                                               
   endif          
   DH%TgroupIDs(TimeIndex) = tgroupid
  endif

  ! create dataset
  call h5dcreate_f(tgroupid,DatasetName,dtypeid,fspace_id,&
       dsetid,hdf5err)
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_DATASET_CREATE 
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  return
end subroutine setup_wrtd_dataset

subroutine extend_wrtd_dataset(DataHandle,TimeIndex,countmd,dsetid,dspaceid,&
     fspaceid,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  integer               ,intent(in)             :: countmd
  integer               ,intent(in)             :: TimeIndex

  integer(hid_t)        ,intent(out)            :: dsetid
  integer(hid_t)        ,intent(out)            :: dspaceid
  integer(hid_t)        ,intent(out)            :: fspaceid
  integer               ,intent(out)            :: Status

  integer(hsize_t)      ,dimension(2)           :: sizes
  integer(hsize_t)      ,dimension(2)           :: chunk_dims
  integer(hsize_t)      ,dimension(2)           :: dims
  integer(hsize_t)      ,dimension(2)           :: hdf5_maxdims
  integer(hsize_t)      ,dimension(2)           :: offset
  integer(hsize_t)      ,dimension(2)           :: count

  integer                                       :: hdf5err

  sizes(1)    = countmd
  sizes(2)    = TimeIndex
  offset(1)   = 0
  offset(2)   = TimeIndex - 1
  count(1)    = countmd
  count(2)    = 1
  dims(1)     = countmd
  dims(2)     = 1

  ! extend the dataset
  CALL h5dextend_f(dsetid,sizes,hdf5err)
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_DATASET_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! obtain file space id
  CALL h5dget_space_f(dsetid,fspaceid,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  CALL h5sselect_hyperslab_f(fspaceid, H5S_SELECT_SET_F, &
       offset, count, hdf5err) 
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_DATASET_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! create the memory space id
  call h5screate_simple_f(2,dims,dspaceid,hdf5err,dims)
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  return
end subroutine extend_wrtd_dataset

subroutine setup_rdtd_dataset(DataHandle,DataSetName,mtypeid,TimeIndex,&
     countmd,outcountmd,dset_id,memspaceid,    &
     dspace_id,tgroupid,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: DataSetName
  integer               ,intent(in)             :: countmd
  integer               ,intent(out)            :: outcountmd
  integer               ,intent(inout)          :: mtypeid
  integer               ,intent(in)             :: TimeIndex

  integer(hid_t)        ,intent(out)            :: dset_id
  integer(hid_t)        ,intent(out)            :: dspace_id
  integer(hid_t)        ,intent(out)            :: memspaceid
  integer(hid_t)        ,intent(out)            :: tgroupid
  integer               ,intent(out)            :: Status

  integer(hid_t)                                :: dtype_id
  integer(hid_t)                                :: class_type
  integer(hsize_t)      ,dimension(1)           :: sizes
  integer(hsize_t)      ,dimension(1)           :: dims
  integer(hsize_t)      ,dimension(1)           :: h5_dims
  integer(hsize_t)      ,dimension(1)           :: hdf5_maxdims
  integer(hsize_t)      ,dimension(1)           :: offset
  integer(hsize_t)      ,dimension(1)           :: count
  integer                                       :: StoredDim
  type(wrf_phdf5_data_handle),pointer            :: DH

  logical                                       :: flag
  integer                                       :: hdf5err

  character(Len = MaxTimeSLen)                 :: tname
  character(Len = 512)                         :: tgroupname      
  ! get datahandle
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  ! obtain the absolute name of the group where the dataset is located
  call numtochar(TimeIndex,tname)
  tgroupname = 'TIME_STAMP_'//tname
  call h5gopen_f(DH%GroupID,tgroupname,tgroupid,hdf5err)

  ! Obtain HDF5 dataset id  
  call h5dopen_f(tgroupid,DataSetName,dset_id,hdf5err)
  if(hdf5err.lt.0) then
     Status = WRF_HDF5_ERR_DATASET_OPEN
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! Obtain the datatype
  call h5dget_type_f(dset_id,dtype_id,hdf5err)
  if(hdf5err.lt.0) then
     Status = WRF_HDF5_ERR_DATASET_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5tget_class_f(dtype_id,class_type,hdf5err)
  if(hdf5err.lt.0) then
     Status = WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  if(mtypeid == H5T_NATIVE_REAL .or. mtypeid == H5T_NATIVE_DOUBLE) then
     if( class_type /= H5T_FLOAT_F)  then
        Status = WRF_HDF5_ERR_TYPE_MISMATCH
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
     endif
  else if(mtypeid ==H5T_NATIVE_CHARACTER) then
     if(class_type /= H5T_STRING_F) then
        Status = WRF_HDF5_ERR_TYPE_MISMATCH
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
     endif
  else if(mtypeid == H5T_NATIVE_INTEGER) then 
     if(class_type /= H5T_INTEGER_F) then
        Status = WRF_HDF5_ERR_TYPE_MISMATCH
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
     endif
  else if(mtypeid == DH%EnumID) then
     if(class_type /= H5T_ENUM_F) then
        Status = WRF_HDF5_ERR_TYPE_MISMATCH
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
     endif
     call h5tequal_f(dtype_id,DH%EnumID,flag,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_OPEN
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
     if(flag .EQV. .FALSE.) then
        Status = WRF_HDF5_ERR_TYPE_MISMATCH
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
     endif
  else 
     Status = WRF_HDF5_ERR_BAD_DATA_TYPE
     write(msg,*)'Fatal Non-WRF supported TYPE in ',__FILE__,', line',__LINE__
     call wrf_debug(FATAL, msg)
     return
  endif
  ! update string id
  if(mtypeid == H5T_NATIVE_CHARACTER) then
     mtypeid = dtype_id
  endif

  ! Obtain the dataspace
  call h5dget_space_f(dset_id,dspace_id,hdf5err)
  if(hdf5err.lt.0) then
     Status = WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! Obtain the rank of the dimension
  call h5sget_simple_extent_ndims_f(dspace_id,StoredDim,hdf5err)
  if(hdf5err.lt.0) then
     Status = WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  if(StoredDim /=1) then
     Status = WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  call h5sget_simple_extent_dims_f(dspace_id,h5_dims,hdf5_maxdims,hdf5err)
  if(hdf5err.lt.0) then
     Status = WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  if(countmd <= 0) then
     Status = WRF_HDF5_ERR_ZERO_LENGTH_READ
     write(msg,*) 'Warning ZERO LENGTH READ in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  if(countmd .lt. h5_dims(1)) then
     outcountmd = countmd
  else
     outcountmd = h5_dims(1)
  endif

  ! create memspace_id
  dims(1) = outcountmd

  call h5screate_simple_f(1,dims,memspaceid,hdf5err)
  if(hdf5err.lt.0) then
     Status = WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  offset(1) = 0
  count(1)  = outcountmd

  return
end subroutine setup_rdtd_dataset

subroutine make_strid(str_len,str_id,Status) 

  use wrf_phdf5_data
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer      ,intent(in)         :: str_len
  integer(hid_t),intent(out)       :: str_id
  integer       ,intent(out)       :: Status
  integer(size_t)                  :: str_lensize
  integer                          :: hdf5err

  Status = WRF_NO_ERR
  if(str_len <= 0) then
     Status = WRF_HDF5_ERR_ATTRIBUTE_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5tcopy_f(H5T_NATIVE_CHARACTER,str_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  str_lensize = str_len
  call h5tset_size_f(str_id,str_lensize,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5tset_strpad_f(str_id,H5T_STR_SPACEPAD_F,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

end subroutine make_strid
