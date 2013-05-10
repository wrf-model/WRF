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


subroutine HDF5IOWRITE(DataHandle,Comm,DateStr,Length,DomainStart,DomainEnd &
     ,PatchStart,PatchEnd,MemoryOrder &
     ,WrfDType,FieldType,groupID,TimeIndex &
     ,DimRank ,DatasetName,XField,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'mpif.h'
  include 'wrf_status_codes.h'

  integer                     ,intent(in)     :: DataHandle
  integer                     ,intent(inout)  :: Comm
  character*(*)               ,intent(in)     :: DateStr
  integer,dimension(NVarDims) ,intent(in)     :: Length

  integer,dimension(NVarDims) ,intent(in)     :: DomainStart
  integer,dimension(NVarDims) ,intent(in)     :: DomainEnd
  integer,dimension(NVarDims) ,intent(in)     :: PatchStart
  integer,dimension(NVarDims) ,intent(in)     :: PatchEnd

  character*(*)               ,intent(in)     :: MemoryOrder

  integer                     ,intent(in)     :: WrfDType
  integer(hid_t)              ,intent(in)     :: FieldType
  integer(hid_t)              ,intent(in)     :: groupID
  integer                     ,intent(in)     :: TimeIndex

  integer,dimension(*)        ,intent(in)     :: DimRank
  character (*)               ,intent(in)     :: DatasetName
  integer,dimension(*)        ,intent(inout)  :: XField
  integer                     ,intent(out)    :: Status

  integer(hid_t)                              :: dset_id
  integer                                     :: NDim
  integer,dimension(NVarDims)                 :: VStart
  integer,dimension(NVarDims)                 :: VCount
  character (3)                               :: Mem0
  character (3)                               :: UCMem0
  type(wrf_phdf5_data_handle) ,pointer         :: DH

  ! attribute defination
  integer(hid_t)                              :: dimaspace_id  ! DimRank dataspace id
  integer(hid_t)                              :: dimattr_id    ! DimRank attribute id
  integer(hsize_t) ,dimension(1)              :: dim_space
  INTEGER(HID_T)                              :: dspace_id     ! Raw Data memory Dataspace id
  INTEGER(HID_T)                              :: fspace_id     ! Raw Data file Dataspace id
  INTEGER(HID_T)                              :: crp_list      ! chunk  identifier
  integer(hid_t)                              :: h5_atypeid    ! for fieldtype,memorder attribute
  integer(hid_t)                              :: h5_aspaceid   ! for fieldtype,memorder  
  integer(hid_t)                              :: h5_attrid     ! for fieldtype,memorder
  integer(hsize_t), dimension(7)              :: adata_dims
  integer                                     :: routine_atype


  integer,          dimension(:),allocatable  :: dimrank_data

  INTEGER(HSIZE_T), dimension(:),allocatable  :: dims  ! Dataset dimensions
  INTEGER(HSIZE_T), dimension(:),allocatable  :: sizes ! Dataset dimensions
  INTEGER(HSIZE_T), dimension(:),allocatable  :: chunk_dims 
  INTEGER(HSIZE_T), dimension(:),allocatable  :: hdf5_maxdims
  INTEGER(HSIZE_T), dimension(:),allocatable  :: offset 
  INTEGER(HSIZE_T), dimension(:),allocatable  :: count  
  INTEGER(HSIZE_T), DIMENSION(7)              :: dimsfi
  integer                                     :: hdf5err
  integer                                     :: i,j
  integer(size_t)                             :: dsetsize

  ! FOR PARALLEL IO
  integer(hid_t)                              :: xfer_list
  logical                                     :: no_par


  ! get the handle 
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! get the rank of the dimension
  call GetDim(MemoryOrder,NDim,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! If patch is equal to domain, the parallel is not necessary, sequential is used.
  ! In this version, we haven't implemented this yet.
  ! We use no_par to check whether we can use compact data storage.
  no_par = .TRUE.
  do i = 1,NDim
     if((PatchStart(i)/=DomainStart(i)).or.(PatchEnd(i)/=DomainEnd(i))) then
        no_par = .FALSE.
        exit
     endif
  enddo

  ! change the different Memory Order to XYZ for patch and domain
  if(MemoryOrder.NE.'0') then
     call ExtOrder(MemoryOrder,PatchStart,Status)
     call ExtOrder(MemoryOrder,PatchEnd,Status)
     call ExtOrder(MemoryOrder,DomainStart,Status)
     call ExtOrder(MemoryOrder,DomainEnd,Status)
  endif

  ! allocating memory for dynamic arrays; 
  ! since the time step is always 1, we may ignore the fourth
  ! dimension time; now keep it to make it consistent with sequential version
  allocate(dims(NDim+1))
  allocate(count(NDim+1))
  allocate(offset(NDim+1))
  allocate(sizes(NDim+1))


  ! arrange offset, count for each hyperslab
  dims(1:NDim)   = DomainEnd(1:NDim) - DomainStart(1:NDim) + 1
  dims(NDim+1)   = 1

  count(NDim+1)  = 1
  count(1:NDim)  = Length(1:NDim)

  offset(NDim+1) = 0
  offset(1:NDim) = PatchStart(1:NDim) - 1


  ! allocate the dataspace to write hyperslab data

  dimsfi = 0
  do i = 1, NDim + 1
     dimsfi(i) = count(i)
  enddo

  ! create the memory space id
  call h5screate_simple_f(NDim+1,count,dspace_id,hdf5err,count)
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dims)
     deallocate(count)
     deallocate(offset)
     deallocate(sizes)
     return
  endif


  ! create file space
  call h5screate_simple_f(NDim+1,dims,fspace_id,hdf5err,dims)
  if(hdf5err.lt.0) then        
     Status =  WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dims)
     deallocate(count)
     deallocate(offset)
     deallocate(sizes)
     return
  endif

  ! compact storage when the patch is equal to the whole domain
  ! calculate the non-decomposed dataset size

  call h5tget_size_f(FieldType,dsetsize,hdf5err)
  if(hdf5err.lt.0) then
     Status = WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dims)
     deallocate(count)
     deallocate(offset)
     deallocate(sizes)
     return
  endif

  do i =1,NDim
     dsetsize = dsetsize*dims(i)
  enddo
  if(no_par.and.(dsetsize.le.CompDsetSize)) then
     call h5pcreate_f(H5P_DATASET_CREATE_F,crp_list,hdf5err)
     if(hdf5err.lt.0) then
        Status =  WRF_HDF5_ERR_PROPERTY_LIST
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(dims)
        deallocate(count)
        deallocate(offset)
        deallocate(sizes)
        return
     endif
     call h5pset_layout_f(crp_list,0,hdf5err)
     if(hdf5err.lt.0) then
        Status =  WRF_HDF5_ERR_PROPERTY_LIST
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(dims)
        deallocate(count)
        deallocate(offset)
        deallocate(sizes)
        return
     endif
     call h5dcreate_f(DH%TgroupIDs(TimeIndex),DatasetName,FieldType,fspace_id,dset_id,&
          hdf5err,crp_list)
     call h5pclose_f(crp_list,hdf5err)
  else
     call h5dcreate_f(DH%TgroupIDs(TimeIndex),DatasetName,FieldType,fspace_id,dset_id,hdf5err)
  endif

  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_DATASET_CREATE 
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dims)
     deallocate(count)
     deallocate(offset)
     deallocate(sizes)
     return
  endif

  ! select the correct hyperslab for file space id
  CALL h5sselect_hyperslab_f(fspace_id, H5S_SELECT_SET_F, offset, count &
       ,hdf5err) 
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_DATASET_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dims)
     deallocate(count)
     deallocate(offset)
     deallocate(sizes)
     return
  endif

  ! Create property list for collective dataset write
  CALL h5pcreate_f(H5P_DATASET_XFER_F, xfer_list, hdf5err)
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_PROPERTY_LIST
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dims)
     deallocate(count)
     deallocate(offset)
     deallocate(sizes)
     return
  endif

  CALL h5pset_dxpl_mpio_f(xfer_list, H5FD_MPIO_COLLECTIVE_F&
       ,hdf5err)
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_PROPERTY_LIST
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dims)
     deallocate(count)
     deallocate(offset)
     deallocate(sizes)
     return
  endif


  ! write the data in memory space to file space
  CALL h5dwrite_f(dset_id,FieldType,XField,dimsfi,hdf5err,&
       mem_space_id =dspace_id,file_space_id =fspace_id, &
       xfer_prp = xfer_list)
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_DATASET_WRITE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dims)
     deallocate(count)
     deallocate(offset)
     deallocate(sizes)
     return
  endif

  CALL h5pclose_f(xfer_list,hdf5err)
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_PROPERTY_LIST
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dims)
     deallocate(count)
     deallocate(offset)
     deallocate(sizes)
     return
  endif

  if(TimeIndex == 1) then
     do i =1, MaxVars
        if(DH%dsetids(i) == -1) then
           DH%dsetids(i) = dset_id
           DH%VarNames(i) = DataSetName
           exit
        endif
     enddo
     ! Only writing attributes when TimeIndex ==1
     call write_hdf5_attributes(DataHandle,MemoryOrder,WrfDType,DimRank,&
          NDim,dset_id,Status)
  endif

  call h5sclose_f(fspace_id,hdf5err)
  call h5sclose_f(dspace_id,hdf5err)
  if(TimeIndex /= 1) then
     call h5dclose_f(dset_id,hdf5err)  
  endif
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_DATASPACE  
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dims)
     deallocate(count)
     deallocate(offset)
     deallocate(sizes)
     return
  endif
  Status = WRF_NO_ERR
  return
end subroutine  HDF5IOWRITE


subroutine ext_phdf5_ioinit(SysDepInfo, Status)

  use wrf_phdf5_data
  use HDF5
  implicit none

  include 'wrf_status_codes.h'
  include 'mpif.h'

  CHARACTER*(*), INTENT(IN) :: SysDepInfo
  integer, intent(out) :: status
  integer              :: hdf5err

  ! set up some variables inside the derived type
  WrfDataHandles(1:WrfDataHandleMax)%Free = .true.
  ! ?
  WrfDataHandles(1:WrfDataHandleMax)%TimesName    = 'Times' 
  WrfDataHandles(1:WrfDataHandleMax)%DimUnlimName = 'Time'  

  ! set up HDF5 global variables
  call h5open_f(hdf5err)
  if(hdf5err .lt.0) then 
     Status =  WRF_HDF5_ERR_CLOSE_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif
  return
end subroutine ext_phdf5_ioinit


subroutine ext_phdf5_ioclose( DataHandle, Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use hdf5
  implicit none
  include 'wrf_status_codes.h'   
  include 'mpif.h'

  integer              ,intent(in)       :: DataHandle
  integer              ,intent(out)      :: Status
  type(wrf_phdf5_data_handle),pointer     :: DH
  integer                                :: stat
  integer                                :: NVar
  integer                                :: hdferr
  integer                                :: table_length
  integer                                :: i
  integer(hid_t)                         :: dtype_id
  integer                                :: obj_count
  integer(hid_t),allocatable,dimension(:) :: obj_ids
  character(len=100)                       :: buf
  integer(size_t)                        :: name_size

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', 906
     call wrf_debug ( WARN , msg)
     return
  endif

  ! THE FOLLOWING section writes dimscale information to the data set,may be put into a subroutine

  ! check the file status, should be either open_for_read or opened_and_committed
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
     Status = WRF_HDF5_ERR_FILE_OPEN
     write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
     Status = WRF_HDF5_ERR_DRYRUN_CLOSE
     write(msg,*) 'Warning TRY TO CLOSE DRYRUN in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)

  elseif(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then
     ! Handle dim. scale
     ! STORE "Times" as the first element of the dimensional table

     DH%DIMTABLE(1)%dim_name  = 'Time'
     DH%DIMTABLE(1)%Length    = DH%TimeIndex
     DH%DIMTABLE(1)%unlimited = 1

     do i =1,MaxTabDims
        if(DH%DIMTABLE(i)%dim_name== NO_NAME) then
           exit
        endif
     enddo

     table_length = i-1
     call store_table(DataHandle,table_length,Status)
     if(Status.ne.WRF_NO_ERR) then
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
     continue    
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
     !     call h5dclose_f(DH%TimesID,hdferr)
     !     if(hdferr.lt.0) then 
     !       Status =  WRF_HDF5_ERR_DATASET_CLOSE
     !       write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     !       call wrf_debug ( WARN , msg) 
     !       return
     !     endif
     continue
  else
     Status = WRF_HDF5_ERR_BAD_FILE_STATUS
     write(msg,*) 'Fatal hdf5err BAD FILE STATUS in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif

  ! close HDF5 APIs 
  do NVar = 1, MaxVars
     if(DH%DsetIDs(NVar) /= -1) then
        call h5dclose_f(DH%DsetIDs(NVar),hdferr)
        if(hdferr .ne. 0) then
           Status =  WRF_HDF5_ERR_DATASET_CLOSE
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif
     endif
  enddo

  do i = 1, MaxTimes
     if(DH%TgroupIDs(i) /= -1) then
        call h5gclose_f(DH%TgroupIDs(i),hdferr)
        if(hdferr .ne. 0) then
           Status =  WRF_HDF5_ERR_DATASET_CLOSE
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif
     endif
  enddo

  call h5gclose_f(DH%GroupID,hdferr)
  if(hdferr .ne. 0) then
     Status =  WRF_HDF5_ERR_CLOSE_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5gclose_f(DH%DimGroupID,hdferr)
  if(hdferr .ne. 0) then
     Status =  WRF_HDF5_ERR_CLOSE_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'HDF5 IO CLOSE error in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  call h5fclose_f(DH%FileID,hdferr)
  if(hdferr .ne. 0) then
     Status =  WRF_HDF5_ERR_CLOSE_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'HDF5 IO CLOSE error in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  call free_memory(DataHandle,Status)
  if(Status /= WRF_NO_ERR) then
     Status = WRF_HDF5_ERR_OTHERS
     write(msg,*) 'Warning Status = ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  DH%Free=.true.
  return
end subroutine ext_phdf5_ioclose


subroutine ext_phdf5_ioexit(Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'wrf_status_codes.h'
  include 'mpif.h'

  integer              ,intent(out)      :: Status
  integer                                :: hdf5err
  type(wrf_phdf5_data_handle),pointer     :: DH
  integer                                :: i
  integer                                :: stat


  ! free memories 
  do i=1,WrfDataHandleMax
     if(.not.WrfDataHandles(i)%Free) then
        call free_memory(i,Status)
        exit
     endif
  enddo

  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'free resources error in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  CALL h5close_f(hdf5err)

  if(hdf5err.lt.0) then
     Status = WRF_HDF5_ERR_CLOSE_GENERAL
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif

  return
end subroutine ext_phdf5_ioexit



!! This routine will set up everything to read HDF5 files
subroutine ext_phdf5_open_for_read(FileName,Comm,iocomm,SysDepInfo,DataHandle,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'mpif.h'
  include 'wrf_status_codes.h'

  character*(*),intent(in)                     :: FileName
  integer      ,intent(in)                     :: Comm
  integer      ,intent(in)                     :: iocomm
  character*(*),intent(in)                     :: SysDepInfo
  integer      ,intent(out)                    :: DataHandle
  type(wrf_phdf5_data_handle),pointer          :: DH
  integer      ,intent(out)                    :: Status

  integer(hid_t)                               :: Fileid
  integer(hid_t)                               :: tgroupid
  integer(hid_t)                               :: dsetid
  integer(hid_t)                               :: dspaceid
  integer(hid_t)                               :: dtypeid
  integer(hid_t)                               :: acc_plist
  integer                                      :: nmembers
  integer                                      :: submembers
  integer                                      :: tmembers
  integer                                      :: ObjType
  character(len= 256)                           :: ObjName
  character(len= 256)                           :: GroupName

  integer                                      :: i,j
  integer(hsize_t), dimension(7)               :: data_dims
  integer(hsize_t), dimension(1)               :: h5dims
  integer(hsize_t), dimension(1)               :: h5maxdims
  integer                                      :: StoredDim
  integer                                      :: NumVars

  integer                                      :: hdf5err
  integer                                      :: info,mpi_size,mpi_rank  
  character(Len = MaxTimeSLen)                 :: tname
  character(Len = 512)                         :: tgroupname


  ! Allocating the data handle
  call allocHandle(DataHandle,DH,Comm,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call h5pcreate_f(H5P_FILE_ACCESS_F,acc_plist,hdf5err)
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_PROPERTY_LIST
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  info = MPI_INFO_NULL
  CALL h5pset_fapl_mpio_f(acc_plist, comm, info, hdf5err) 
  !   call h5pset_fapl_mpiposix_f(acc_plist,comm,.false.,hdf5err)
  if(hdf5err .lt. 0) then
     Status = WRF_HDF5_ERR_PROPERTY_LIST
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif
  !close every objects when closing HDF5 file.
  call h5pset_fclose_degree_f(acc_plist,H5F_CLOSE_STRONG_F,hdf5err)
  if(hdf5err .lt. 0) then
     Status = WRF_HDF5_ERR_PROPERTY_LIST
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  ! Open the file
  call h5fopen_f(FileName,H5F_ACC_RDWR_F,Fileid,hdf5err &
       ,acc_plist)
  if(hdf5err.lt.0) then
     Status =  WRF_HDF5_ERR_FILE_OPEN
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif
  call h5pclose_f(acc_plist,hdf5err)
  if(hdf5err .lt. 0) then
     Status = WRF_HDF5_ERR_PROPERTY_LIST
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  ! Obtain the number of group
  DH%FileID = Fileid
  call h5gn_members_f(Fileid,"/",nmembers,hdf5err)
  if(hdf5err.lt.0) then
     Status = WRF_HDF5_ERR_GROUP
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! Retrieve group id and dimensional group id, the index must be from 0
  do i = 0, nmembers - 1
     call h5gget_obj_info_idx_f(Fileid,"/",i,ObjName,ObjType,&
          hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_GROUP
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     if(ObjName=='DIM_GROUP') then

        call h5gopen_f(Fileid,"/DIM_GROUP",DH%DimGroupID,hdf5err)
        if(hdf5err.lt.0) then
           Status = WRF_HDF5_ERR_GROUP
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif

        ! For WRF model, the first seven character must be DATASET
     else if(ObjName(1:7)=='DATASET')then

        GroupName="/"//ObjName
        call h5gopen_f(Fileid,GroupName,DH%GroupID,hdf5err)
        if(hdf5err.lt.0) then
           Status = WRF_HDF5_ERR_GROUP
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif

        call h5gn_members_f(FileID,GroupName,submembers,Status)
        if(hdf5err.lt.0) then
           Status = WRF_HDF5_ERR_GROUP
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif

        do j = 0, submembers -1
           call h5gget_obj_info_idx_f(Fileid,GroupName,j,ObjName,ObjType,hdf5err)
           if(hdf5err.lt.0) then
              Status = WRF_HDF5_ERR_GROUP
              write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
              call wrf_debug ( WARN , msg) 
              return
           endif
           call numtochar(j+1,tname)
           tgroupname = 'TIME_STAMP_'//tname

           if(ObjName(1:17)==tgroupname) then
              call h5gopen_f(DH%GroupID,tgroupname,tgroupid,hdf5err)
              if(hdf5err.lt.0) then
                 Status = WRF_HDF5_ERR_GROUP
                 write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
                 call wrf_debug ( WARN , msg) 
                 return
              endif
              call h5gn_members_f(DH%GroupID,tgroupname,tmembers,hdf5err)
              if(hdf5err.lt.0) then
                 Status = WRF_HDF5_ERR_GROUP
                 write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
                 call wrf_debug ( WARN , msg) 
                 return
              endif
              call h5dopen_f(tgroupid,"Times",dsetid,hdf5err)
              if(hdf5err.lt.0) then
                 Status = WRF_HDF5_ERR_DATASET_OPEN
                 write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
                 call wrf_debug ( WARN , msg) 
                 return
              endif
              call h5dget_space_f(dsetid,dspaceid,hdf5err)
              if(hdf5err.lt.0) then
                 Status = WRF_HDF5_ERR_DATASPACE
                 write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
                 call wrf_debug ( WARN , msg) 
                 return
              endif
              call h5sget_simple_extent_ndims_f(dspaceid,StoredDim,hdf5err)
              if(hdf5err.lt.0) then
                 Status = WRF_HDF5_ERR_DATASPACE
                 write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
                 call wrf_debug ( WARN , msg) 
                 return
              endif
              call h5sget_simple_extent_dims_f(dspaceid,h5dims,h5maxdims,hdf5err)  
              if(hdf5err.lt.0) then
                 Status = WRF_HDF5_ERR_DATASPACE
                 write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
                 call wrf_debug ( WARN , msg) 
                 return
              endif
	      data_dims(1) = h5dims(1)
              call h5dget_type_f(dsetid,dtypeid,hdf5err)
              if(hdf5err.lt.0) then
                 Status = WRF_HDF5_ERR_DATATYPE
                 write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
                 call wrf_debug ( WARN , msg) 
                 return
              endif
              call h5dread_f(dsetid,dtypeid,DH%Times(j+1),data_dims,hdf5err)
              if(hdf5err.lt.0) then
                 Status = WRF_HDF5_ERR_DATASET_READ
                 write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
                 call wrf_debug ( WARN , msg) 
                 return
              endif
              DH%CurrentVariable = 0
              DH%CurrentTime     = 0
              DH%TimeIndex       = 0 
              call h5tclose_f(dtypeid,hdf5err)
              call h5sclose_f(dspaceid,hdf5err)
           endif
        enddo
        DH%NumberTimes = submembers

        !       the total member of HDF5 dataset. 
        DH%NumVars = tmembers*submembers
     else
        Status = WRF_HDF5_ERR_OTHERS
     endif
  enddo

  DH%FileStatus      = WRF_FILE_OPENED_FOR_READ
  DH%FileName        = FileName

  ! obtain dimensional scale table
  call retrieve_table(DataHandle,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif
  return

end subroutine ext_phdf5_open_for_read


subroutine ext_phdf5_inquire_opened(DataHandle,FileName,FileStatus,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'wrf_status_codes.h'
  integer                    ,intent(in)     :: DataHandle
  character*(*)              ,intent(in)     :: FileName
  integer                    ,intent(out)    :: FileStatus
  integer                    ,intent(out)    :: Status
  type(wrf_phdf5_data_handle) ,pointer       :: DH


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
end subroutine ext_phdf5_inquire_opened


subroutine ext_phdf5_inquire_filename(DataHandle,FileName,FileStatus,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(out)     :: FileName
  integer               ,intent(out)    :: FileStatus
  integer               ,intent(out)    :: Status
  type(wrf_phdf5_data_handle) ,pointer        :: DH

  ! This line is added to make sure the wrong file will not be opened 
  FileStatus = WRF_FILE_NOT_OPENED

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,',line',__LINE__
     call wrf_debug (WARN, msg)
     return
  endif

  FileName = DH%FileName
  FileStatus = DH%FileStatus
  Status = WRF_NO_ERR

  return 
end subroutine ext_phdf5_inquire_filename


! The real routine to read HDF5 files
subroutine ext_phdf5_read_field(DataHandle,DateStr,Var,Field,FieldType,Comm,  &
     IOComm, DomainDesc, MemoryOrder, Stagger, DimNames, &
     DomainStart,DomainEnd,MemoryStart,MemoryEnd, &
     PatchStart,PatchEnd,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5

  implicit none
  include 'wrf_status_codes.h'
  integer                       ,intent(in)    :: DataHandle
  character*(*)                 ,intent(in)    :: DateStr
  character*(*)                 ,intent(in)    :: Var
  integer                       ,intent(out)   :: Field(*)
  integer                       ,intent(in)    :: FieldType
  integer                       ,intent(inout) :: Comm
  integer                       ,intent(inout) :: IOComm
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrder
  character*(*)                 ,intent(in)    :: Stagger ! Dummy for now
  character*(*) , dimension (*) ,intent(in)    :: DimNames
  integer ,dimension(*)         ,intent(in)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status

  type(wrf_phdf5_data_handle)    ,pointer       :: DH
  integer                                      :: NDim
  integer(hid_t)                               :: GroupID
  character (VarNameLen)                       :: VarName
  integer ,dimension(NVarDims)                 :: Length
  integer ,dimension(NVarDims)                 :: StoredStart
  integer ,dimension(NVarDims)                 :: StoredLen
  integer, dimension(NVarDims)                 :: TemDataStart
  integer ,dimension(:,:,:,:)  ,allocatable    :: XField
  integer                                      :: NVar
  integer                                      :: j
  integer                                      :: i1,i2,j1,j2,k1,k2
  integer                                      :: x1,x2,y1,y2,z1,z2
  integer                                      :: l1,l2,m1,m2,n1,n2
  character (VarNameLen)                       :: Name
  integer                                      :: XType
  integer                                      :: StoredDim
  integer                                      :: NAtts
  integer                                      :: Len
  integer                                      :: stat
  integer                                      :: di
  integer                                      :: FType
  integer(hsize_t),dimension(7)                :: data_dims
  integer(hsize_t),dimension(:) ,allocatable   :: h5_dims
  integer(hsize_t),dimension(:) ,allocatable   :: h5_maxdims
  integer(hsize_t),dimension(:) ,allocatable   :: DataStart
  integer(hsize_t),dimension(:) ,allocatable   :: Datacount
  integer(hid_t)                               :: tgroupid
  integer(hid_t)                               :: dsetid
  integer(hid_t)                               :: dtype_id
  integer(hid_t)                               :: dmemtype_id
  integer(hid_t)                               :: dspace_id
  integer(hid_t)                               :: memspace_id
  integer                                      :: class_type
  integer                                      :: TimeIndex
  logical                                      :: flag
  integer                                      :: hdf5err

  character(Len = MaxTimeSLen)                 :: tname
  character(Len = 512)                         :: tgroupname


  ! FOR PARALLEL IO
  integer                                      :: mpi_rank
  integer(hid_t)                               :: xfer_list


  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
     Status = WRF_HDF5_ERR_FILE_NOT_OPENED
     write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
     Status = WRF_HDF5_ERR_DRYRUN_READ
     write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then
     Status = WRF_HDF5_ERR_READ_WONLY_FILE
     write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then

     ! obtain TimeIndex
     call GetDataTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)

     ! obtain the absolute name of the group where the dataset is located
     call numtochar(TimeIndex,tname)
     tgroupname = 'TIME_STAMP_'//tname

     call h5gopen_f(DH%GroupID,tgroupname,tgroupid,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_GROUP
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5dopen_f(tgroupid,Var,dsetid,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_OPEN
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     ! Obtain the memory datatype
     select case(FieldType)
     case (WRF_REAL)
        dmemtype_id = H5T_NATIVE_REAL
     case (WRF_DOUBLE)
        dmemtype_id = H5T_NATIVE_DOUBLE
     case (WRF_INTEGER)
        dmemtype_id = H5T_NATIVE_INTEGER
     case (WRF_LOGICAL)
        dmemtype_id = DH%EnumID
     case default
        Status = WRF_HDF5_ERR_DATA_TYPE_NOTFOUND
        write(msg,*) 'Warning BAD Memory Data type in ',__FILE__,',line',__LINE__
        call wrf_debug(WARN,msg)
        return
     end select

     ! Obtain the datatype
     call h5dget_type_f(dsetid,dtype_id,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     ! double check whether the Fieldtype is the type of the dataset
     ! we may do the force coercion between real and double
     call h5tget_class_f(dtype_id,class_type,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     if( (FieldType == WRF_REAL .OR. FieldType == WRF_DOUBLE) ) then
        if ( class_type /= H5T_FLOAT_F)  then
           Status = WRF_HDF5_ERR_TYPE_MISMATCH
           write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg)
           return
        endif
     else if(FieldType == WRF_CHARACTER) then
        if(class_type /= H5T_STRING_F) then
           Status = WRF_HDF5_ERR_TYPE_MISMATCH
           write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg)
           return
        endif
     else if(FieldType == WRF_INTEGER) then 
        if(class_type /= H5T_INTEGER_F) then
           Status = WRF_HDF5_ERR_TYPE_MISMATCH
           write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg)
           return
        endif
     else if(FieldType == WRF_LOGICAL) then
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

     ! Obtain the dataspace, check whether the dataspace is within the range
     ! transpose the memory order to the disk order
     call h5dget_space_f(dsetid,dspace_id,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call GetDim(MemoryOrder,NDim,Status)

     Length(1:NDim) = PatchEnd(1:NDim)-PatchStart(1:NDim)+1
     call ExtOrder(MemoryOrder,Length,Status)

     ! Obtain the rank of the dimension
     call h5sget_simple_extent_ndims_f(dspace_id,StoredDim,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     ! From NetCDF implementation, only do error handling
     if((NDim+1) /= StoredDim) then
        Status = WRF_HDF5_ERR_BAD_VARIABLE_DIM
        write(msg,*) 'Fatal error BAD VARIABLE DIMENSION in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
     endif
     allocate(h5_dims(StoredDim))
     allocate(h5_maxdims(StoredDim))
     allocate(DataStart(StoredDim))
     allocate(DataCount(StoredDim))

     call h5sget_simple_extent_dims_f(dspace_id,h5_dims,h5_maxdims,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     ! This part of code needs to be adjusted, currently use NetCDF convention  
     do j = 1, NDim
        if(Length(j) > h5_dims(j)) then
           Status = WRF_HDF5_ERR_READ_PAST_EOF
           write(msg,*) 'Warning READ PAST EOF in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg)
           return
        elseif(Length(j) <= 0) then
           Status = WRF_HDF5_ERR_ZERO_LENGTH_READ
           write(msg,*) 'Warning ZERO LENGTH READ in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg)
           return
        endif
     enddo

     ! create memspace_id 
     data_dims(1:NDim) = Length(1:NDim)
     data_dims(NDim+1) = 1

     call h5screate_simple_f(NDim+1,data_dims,memspace_id,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASPACE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     ! DataStart can start from PatchStart.
     TEMDataStart(1:NDim) = PatchStart(1:NDim)-1

     if(MemoryOrder.NE.'0') then
        call ExtOrder(MemoryOrder,TEMDataStart,Status)
     endif

     DataStart(1:NDim) = TEMDataStart(1:NDim)
     DataStart(NDim+1) = 0
     DataCount(1:NDim) = Length(1:NDim)
     DataCount(NDim+1) = 1

     ! transpose the data XField to Field
     call GetIndices(NDim,MemoryStart,MemoryEnd,l1,l2,m1,m2,n1,n2)
     StoredStart = 1
     StoredLen(1:NDim) = Length(1:NDim)

     ! the dimensional information inside the disk may be greater than
     ! the dimension(PatchEnd-PatchStart); here we can speed up
     ! the performance by using hyperslab selection
     call GetIndices(NDim,StoredStart,StoredLen,x1,x2,y1,y2,z1,z2)
     call GetIndices(NDim,PatchStart,PatchEnd,i1,i2,j1,j2,k1,k2)

     ! di is for double type data
     di = 1 
     if(FieldType == WRF_DOUBLE) di = 2
     allocate(XField(di,x1:x2,y1:y2,z1:z2), STAT=stat)

     ! use hyperslab to only read this current timestamp
     call h5sselect_hyperslab_f(dspace_id,H5S_SELECT_SET_F, &
          DataStart,DataCount,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_GENERAL
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     ! read the data in this time stamp
     call h5dread_f(dsetid,dmemtype_id,XField,data_dims,hdf5err, &
          memspace_id,dspace_id,H5P_DEFAULT_F)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_READ
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call transpose_hdf5('read',MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
          ,XField,x1,x2,y1,y2,z1,z2 &
          ,i1,i2,j1,j2,k1,k2 )

     deallocate(XField, STAT=stat)
     if(stat/= 0) then
        Status = WRF_HDF5_ERR_DEALLOCATION
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
     endif

     call h5dclose_f(dsetid,hdf5err)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_CLOSE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
     deallocate(h5_dims)
     deallocate(h5_maxdims)
     deallocate(DataStart)
     deallocate(DataCount)
  else 
     Status = WRF_HDF5_ERR_BAD_FILE_STATUS
     write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( FATAL , msg)
  endif

  DH%first_operation  = .FALSE.

  return
end subroutine ext_phdf5_read_field

!! This routine essentially sets up everything to write HDF5 files
SUBROUTINE ext_phdf5_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,DataHandle,Status)

  use wrf_phdf5_data
  use HDF5
  use ext_phdf5_support_routines
  implicit none
  include 'mpif.h'
  include 'wrf_status_codes.h'

  character*(*)        ,intent(in)            :: FileName
  integer              ,intent(in)            :: Comm
  integer              ,intent(in)            :: IOComm
  character*(*)        ,intent(in)            :: SysDepInfo
  integer              ,intent(out)           :: DataHandle
  integer              ,intent(out)           :: Status
  type(wrf_phdf5_data_handle),pointer          :: DH
  integer(hid_t)                              :: file5_id
  integer(hid_t)                              :: g_id
  integer(hid_t)                              :: gdim_id
  integer                                     :: hdferr
  integer                                     :: i
  integer                                     :: stat
  character (7)                               :: Buffer
  integer                                     :: VDimIDs(2)
  character(Len = 512)                        :: groupname

  ! For parallel IO
  integer(hid_t)                              :: plist_id
  integer                                     :: hdf5_comm,info,mpi_size,mpi_rank  


  call allocHandle(DataHandle,DH,Comm,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  DH%TimeIndex = 0
  DH%Times     = ZeroDate

  CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, hdferr)
  if(hdferr .lt. 0) then
     Status = WRF_HDF5_ERR_PROPERTY_LIST
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  info      = MPI_INFO_NULL

  CALL h5pset_fapl_mpio_f(plist_id, comm, info, hdferr)

  if(hdferr .lt. 0) then
     Status = WRF_HDF5_ERR_PROPERTY_LIST
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5fcreate_f(FileName,H5F_ACC_TRUNC_F,file5_id,hdferr &
       ,access_prp = plist_id)
  if(hdferr .lt. 0) then
     Status = WRF_HDF5_ERR_FILE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5pclose_f(plist_id,hdferr)
  if(hdferr .lt. 0) then
     Status = WRF_HDF5_ERR_PROPERTY_LIST
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  DH%FileStatus            = WRF_FILE_OPENED_NOT_COMMITTED
  DH%FileName              = FileName
  ! should add a check to see whether the file opened has been used by previous handles
  DH%VarNames  (1:MaxVars) = NO_NAME
  DH%MDVarNames(1:MaxVars) = NO_NAME

  ! group name information is stored at SysDepInfo 
  groupname = "/"//SysDepInfo
!  write(*,*) "groupname ",groupname
  call h5gcreate_f(file5_id,groupname,g_id,hdferr)
  if(hdferr .lt. 0) then
     Status = WRF_HDF5_ERR_GROUP
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! create dimensional group id
  call h5gcreate_f(file5_id,"/DIM_GROUP",gdim_id,hdferr)
  if(hdferr .lt. 0) then
     Status = WRF_HDF5_ERR_GROUP
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  DH%FileID     = file5_id
  DH%GroupID    = g_id
  DH%DIMGroupID = gdim_id

  return

end subroutine ext_phdf5_open_for_write_begin

! HDF5 doesnot need this stage, basically this routine
! just updates the File status.
SUBROUTINE ext_phdf5_open_for_write_commit(DataHandle, Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'wrf_status_codes.h'

  integer              ,intent(in)       :: DataHandle
  integer              ,intent(out)      :: Status
  type(wrf_phdf5_data_handle),pointer     :: DH
  integer(hid_t)                         :: enum_type
  integer                                :: i
  integer                                :: stat


  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  DH%FileStatus  = WRF_FILE_OPENED_AND_COMMITTED
  DH%first_operation  = .TRUE.
  return
end subroutine ext_phdf5_open_for_write_commit

! The real routine to write HDF5 file
subroutine ext_phdf5_write_field(DataHandle,DateStr,Var,Field,FieldType,&
     Comm,IOComm,DomainDesc,MemoryOrder,  &
     Stagger,DimNames,DomainStart,DomainEnd,&
     MemoryStart,MemoryEnd,PatchStart,PatchEnd,&
     Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer                       ,intent(in)      :: DataHandle
  character*(*)                 ,intent(in)      :: DateStr
  character*(*)                 ,intent(in)      :: Var
  integer                       ,intent(inout)   :: Field(*)
  integer                       ,intent(in)      :: FieldType
  integer                       ,intent(inout)   :: Comm
  integer                       ,intent(inout)   :: IOComm
  integer                       ,intent(in)      :: DomainDesc
  character*(*)                 ,intent(in)      :: MemoryOrder
  character*(*)                 ,intent(in)      :: Stagger ! Dummy for now
  character*(*) , dimension (*) ,intent(in)      :: DimNames
  integer ,dimension(*)         ,intent(in)      :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(in)      :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)      :: PatchStart,  PatchEnd
  integer                       ,intent(out)     :: Status

  type(wrf_phdf5_data_handle)    ,pointer        :: DH
  integer(hid_t)                                 :: GroupID
  integer                                        :: NDim
  character (VarNameLen)                         :: VarName
  character (3)                                  :: MemO
  character (3)                                  :: UCMemO
  integer(hid_t)                                 :: DsetID
  integer      ,dimension(NVarDims)              :: Length
  integer      ,dimension(NVarDims)              :: DomLength
  integer      ,dimension(NVarDims+1)            :: DimRank
  character(256),dimension(NVarDims)              :: RODimNames
  integer      ,dimension(NVarDims)              :: StoredStart
  integer      ,dimension(:,:,:,:),allocatable   :: XField
  integer      ,dimension(:,:,:,:),allocatable   :: BUFFER! for logical field
  integer                                        :: stat
  integer                                        :: NVar
  integer                                        :: i,j,k,m,dim_flag
  integer                                        :: i1,i2,j1,j2,k1,k2
  integer                                        :: x1,x2,y1,y2,z1,z2
  integer                                        :: l1,l2,m1,m2,n1,n2
  integer(hid_t)                                 :: XType
  integer                                        :: di
  character (256)                                 :: NullName
  integer                                        :: TimeIndex
  integer ,dimension(NVarDims+1)                 :: temprank
  logical                                        :: NotFound


  NullName = char(0)
  dim_flag = 0

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  ! Examine here, Nov. 7th, 2003
  if(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then 

     ! obtain group id and initialize the rank of dimensional attributes
     GroupID = DH%GroupID
     DimRank = -1

     ! get the rank of the dimension based on MemoryOrder string(cleaver from NetCDF)
     call GetDim(MemoryOrder,NDim,Status)
     if(Status /= WRF_NO_ERR) then
        write(msg,*) 'Warning BAD MEMORY ORDER in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
     endif

     ! check whether the DateStr is the correct length
     call DateCheck(DateStr,Status)
     if(Status /= WRF_NO_ERR) then
        write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
     endif

     ! get the dataset name and dimensional information of the data
     VarName           = Var
     Length(1:NDim)    = PatchEnd(1:NDim) - PatchStart(1:NDim) + 1
     DomLength(1:NDim) = DomainEnd(1:NDim) - DomainStart(1:NDim) + 1

     ! Transposing the data order and dim. string order, store to RODimNames
     call ExtOrder(MemoryOrder,Length,Status)
     call ExtOrder(MemoryOrder,DomLength,Status)
     if(Status /= WRF_NO_ERR) then
        write(msg,*) 'Warning BAD MEMORY ORDER in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
     endif

     ! Map datatype from WRF to HDF5
     select case (FieldType)
     case (WRF_REAL)
        XType = H5T_NATIVE_REAL
     case (WRF_DOUBLE)
        Xtype = H5T_NATIVE_DOUBLE
     case (WRF_INTEGER)
        XType = H5T_NATIVE_INTEGER
     case (WRF_LOGICAL)
        XType = DH%EnumID
     case default
        Status = WRF_HDF5_ERR_DATA_TYPE_NOTFOUND
        return
     end select

     ! HANDLE  with dim. scale 
     ! handle dimensional scale data; search and store them in a table.
     ! The table is one dimensional array of compound data type. One member of
     ! the type is HDF5 string, representing the name of the dim(west_east_stag eg.)
     ! Another number is the length of the dimension(west_east_stag = 31)
     ! In this part, we will not store TIME but leave it at the end since the time
     ! index won't be known until the end of the run; since all fields(HDF5 datasets)
     ! have the same timestamp, writing it once should be fine.

     ! 1) create a loop for dimensions
     call GetDataTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     if(TimeIndex == 1) then

        ! 2) get the dim. name, the first dim. is reserved for time, 
        call ExtOrderStr(MemoryOrder,DimNames,RODimNames,Status)
        if(Status /= WRF_NO_ERR) then
           write(msg,*) 'Warning BAD MEMORY ORDER in ',__FILE__,', line', __LINE__ 
           call wrf_debug ( WARN , msg)
           return
        endif
        ! 3) get the dim. length
        ! 4) inside the loop, search the table for dimensional name( table module)
        !    IF FOUND, go to the next dimension, return the table dimensional rank
        !    (For example, find west_east_stag in the table, the rank of "west_east_stag"
        !     is 3; so return 3 for the array dimrank.)
        !    in the table; so through the table, we can find the information
        !    such as names, length of this dimension
        ! 4.1) save the rank into an array for attribute
        !      if not found,  go to 5)
        ! 4)' the first dimension is reserved for time, so table starts from j = 2 
        !
        ! 5) NOT FOUND, inside the loop add the new dimensional information to the 
        ! table(table module)

        ! The first dimension of the field is always "time" and "time"
        ! is also the first dimension of the "table".
        k = 2
        DimRank(1) = 1

        do i = 1,NDim
           do j = 2,MaxTabDims

              ! Search for the table and see if we are at the end of the table
              if (DH%DIMTABLE(j)%dim_name == NO_NAME) then

                 ! Sometimes the RODimNames is NULLName or ''. If that happens,
                 ! we will search the table from the beginning and see 
                 ! whether the name is FAKEDIM(the default name) and  the 
                 ! current length of the dim. is the same as that of FAKEDIM; 
                 ! if yes, use this FAKEDIM for the current field dim. 

                 if(RODimNames(i) ==''.or. RODimNames(i)==NullName) then
                    do m = 2,j
                       if(DomLength(i)==DH%DIMTABLE(m)%Length.and. &
                            DH%DIMTABLE(m)%dim_name(1:7)=='FAKEDIM')then
                          DimRank(k) = m
                          k = k + 1
                          dim_flag = 1
                          exit
                       endif
                    enddo
                    ! No FAKEDIM and the same length dim. is found,
                    ! Add another dimension "FAKEDIM + j", with the length
                    ! as DomLength(i)
                    if (dim_flag == 1) then 
                       dim_flag = 0
                    else   
                       RODimNames(i) = 'FAKEDIM'//achar(j+iachar('0'))
                       DH%DIMTABLE(j)%dim_name  = RODimNames(i)
                       DH%DIMTABLE(j)%length    = DomLength(i)
                       DimRank(k) = j
                       k          = k + 1
                    endif
                    ! no '' or NULLName is found, then assign this RODimNames
                    ! to the dim. table.
                 else
                    DH%DIMTABLE(j)%dim_name  = RODimNames(i)
                    DH%DIMTABLE(j)%length    = DomLength(i)
                    DimRank(k)               = j
                    k = k + 1
                 endif
                 exit
                 ! If we found the current dim. in the table already,save the rank
              else if(DH%DIMTABLE(j)%dim_name == RODimNames(i)) then
                 ! remember the rank of dimensional scale
                 DimRank(k) = j
                 k = k + 1
                 exit
              else
                 continue
              endif
           enddo
        enddo
     endif ! end of timeindex of 1

     ! 6) create an attribute array called DimRank to store the rank of the attribute.
     !    This will be done in the HDF5IOWRITE routine        

     ! 7) before the end of the run, 1) update time, 2) write the table to HDF5.

     ! get the index of l1,.......for writing HDF5 file.
     StoredStart = 1
     call GetIndices(NDim,MemoryStart,MemoryEnd,l1,l2,m1,m2,n1,n2)
     call GetIndices(NDim,StoredStart,Length   ,x1,x2,y1,y2,z1,z2)
     call GetIndices(NDim,PatchStart, PatchEnd ,i1,i2,j1,j2,k1,k2)
     di=1
     if(FieldType == WRF_DOUBLE) di = 2
     allocate(XField(di,x1:x2,y1:y2,z1:z2), STAT=stat)
     if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line',__LINE__
        call wrf_debug ( FATAL , msg)
        return
     endif

     ! Transpose the real data for tools people
     call Transpose_hdf5('write',MemoryOrder,di, Field,l1,l2,m1,m2,n1,n2 &
          ,XField,x1,x2,y1,y2,z1,z2 &
          ,i1,i2,j1,j2,k1,k2 )

     ! handle with logical data separately,because of not able to 
     ! map Fortran Logical type to C type
     if(FieldType .eq. WRF_LOGICAL) then
        allocate(BUFFER(di,x1:x2,y1:y2,z1:z2), STAT=stat)
        do k =z1,z2
           do j = y1,y2
              do i = x1,x2
                 do m = 1,di
                    if(XField(m,i,j,k)/= 0) then
                       BUFFER(m,i,j,k) = 1
                    else
                       BUFFER(m,i,j,k) = 0
                    endif
                 enddo
              enddo
           enddo
        enddo
        call HDF5IOWRITE(DataHandle,Comm,DateStr,Length,DomainStart, DomainEnd &
             ,PatchStart,PatchEnd, MemoryOrder &
             ,FieldType,XType,groupID,TimeIndex,DimRank &
             ,Var,BUFFER,Status)
        deallocate(BUFFER,STAT=stat)
        if(stat/=0) then
           Status = WRF_ERR_FATAL_ALLOCATION_ERROR
           write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line',__LINE__
           call wrf_debug ( FATAL , msg)
           return
        endif
     else 
        call HDF5IOWRITE(DataHandle,Comm,DateStr,Length, DomainStart, DomainEnd &
             ,PatchStart, PatchEnd, MemoryOrder &
             ,FieldType,XType,groupID,TimeIndex,DimRank &
             ,Var,XField,Status)
     endif

     if (Status /= WRF_NO_ERR) then 
        return
     endif

     deallocate(XField,STAT=stat)
     if(stat/=0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line',__LINE__
        call wrf_debug ( FATAL , msg)
        return
     endif
  endif

  DH%first_operation  = .FALSE.

  return 

end subroutine ext_phdf5_write_field

! set_time routine is only used for open_for_read
subroutine ext_phdf5_set_time(DataHandle, DateStr, Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)          :: DataHandle
  character*(*)         ,intent(in)          :: DateStr
  integer               ,intent(out)         :: Status
  type(wrf_phdf5_data_handle) ,pointer        :: DH
  integer                                    :: i

  ! check whether the Date length is equal to DateStrLen defined at wrf_phdf5_data
  ! sees not enough, leave it for the time being 3/12/2003
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
     return
  endif

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
     Status = WRF_HDF5_ERR_FILE_NOT_OPENED
     write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
     Status = WRF_HDF5_ERR_FILE_NOT_COMMITTED
     write(msg,*) 'Warning FILE NOT COMMITTED in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then
     Status = WRF_HDF5_ERR_READ_WONLY_FILE
     write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
     do i=1,MaxTimes
        if(DH%Times(i)==DateStr) then
           DH%CurrentTime = i
           exit
        endif
        if(i==MaxTimes) then
           Status = WRF_HDF5_ERR_TIME
           return
        endif
     enddo
     DH%CurrentVariable = 0
     Status = WRF_NO_ERR
  else
     Status = WRF_HDF5_ERR_BAD_FILE_STATUS
     write(msg,*) 'FATAL BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_phdf5_set_time

! get_next_time routine is only used for open_for_read
subroutine ext_phdf5_get_next_time(DataHandle, DateStr, Status)
  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)          :: DataHandle
  character*(*)         ,intent(out)         :: DateStr
  integer               ,intent(out)         :: Status
  type(wrf_phdf5_data_handle) ,pointer        :: DH

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
     Status = WRF_HDF5_ERR_FILE_NOT_OPENED
     write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
     Status = WRF_HDF5_ERR_DRYRUN_READ
     write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then
     Status = WRF_HDF5_ERR_READ_WONLY_FILE
     write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
     if(DH%CurrentTime >= DH%NumberTimes) then
        Status = WRF_HDF5_ERR_TIME
        write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
     endif
     DH%CurrentTime     = DH%CurrentTime +1
     DateStr            = DH%Times(DH%CurrentTime)
     DH%CurrentVariable = 0
     Status = WRF_NO_ERR
  else
     Status = WRF_HDF5_ERR_BAD_FILE_STATUS
     write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_phdf5_get_next_time

! get_previous_time routine
subroutine ext_phdf5_get_previous_time(DataHandle, DateStr, Status)
  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)          :: DataHandle
  character*(*)         ,intent(out)         :: DateStr
  integer               ,intent(out)         :: Status
  type(wrf_phdf5_data_handle) ,pointer        :: DH

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
     Status = WRF_HDF5_ERR_FILE_NOT_OPENED
     write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
     Status = WRF_HDF5_ERR_DRYRUN_READ
     write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then
     Status = WRF_HDF5_ERR_READ_WONLY_FILE
     write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
     if(DH%CurrentTime.GT.0) then
       DH%CurrentTime = DH%CurrentTime - 1
     endif
     DateStr            = DH%Times(DH%CurrentTime)
     DH%CurrentVariable = 0
     Status = WRF_NO_ERR
  else
     Status = WRF_HDF5_ERR_BAD_FILE_STATUS
     write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_phdf5_get_previous_time

subroutine ext_phdf5_get_var_info(DataHandle,Name,NDim,MemoryOrder,Stagger,DomainStart,DomainEnd,WrfType,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Name
  integer               ,intent(out)    :: NDim
  character*(*)         ,intent(out)    :: MemoryOrder
  character*(*)         ,intent(out)    :: Stagger ! Dummy for now
  integer ,dimension(*) ,intent(out)    :: DomainStart, DomainEnd
  integer               ,intent(out)    :: WrfType
  integer               ,intent(out)    :: Status
  type(wrf_phdf5_data_handle) ,pointer   :: DH
  integer                               :: VarID
  integer ,dimension(NVarDims)          :: VDimIDs
  integer                               :: j
  integer                               :: hdf5err
  integer                               :: XType

  character(Len =MaxTimeSLen)           :: tname
  character(Len = 512)                  :: tgroupname
  integer(hid_t)                        :: tgroupid
  integer(hid_t)                        :: dsetid
  integer(hid_t)                        :: dspaceid
  integer                               :: HDF5_NDim
  integer(hsize_t),dimension(:),allocatable         :: h5dims
  integer(hsize_t),dimension(:),allocatable         :: h5maxdims

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , TRIM(msg))
     return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
     Status = WRF_HDF5_ERR_FILE_NOT_OPENED
     write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , TRIM(msg))
     return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
     Status = WRF_HDF5_ERR_DRYRUN_READ
     write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , TRIM(msg))
     return
  elseif(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then
     Status = WRF_HDF5_ERR_READ_WONLY_FILE
     write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__    
     call wrf_debug ( WARN , TRIM(msg))
     return
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
     if(Name /= "Times") then
        call numtochar(1,tname)
        tgroupname = 'TIME_STAMP_'//tname
        call h5gopen_f(DH%GroupID,tgroupname,tgroupid,hdf5err)
        if(hdf5err.lt.0) then
           Status = WRF_HDF5_ERR_GROUP
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif
        call h5dopen_f(tgroupid,Name,dsetid,hdf5err)
        if(hdf5err /= 0) then
           STATUS = WRF_HDF5_ERR_DATASET_OPEN
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif
        call h5dget_space_f(dsetid,dspaceid,hdf5err)
        if(hdf5err.lt.0) then
           Status = WRF_HDF5_ERR_DATASPACE
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif

        call h5sget_simple_extent_ndims_f(dspaceid,HDF5_NDim,hdf5err)
        if(hdf5err.lt.0) then
           Status = WRF_HDF5_ERR_DATASPACE
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif

        call ext_phdf5_get_var_ti_char(DataHandle,"MemoryOrder",Name,MemoryOrder,Status)
        if(Status /= WRF_NO_ERR) then
           Status = WRF_HDF5_ERR_ATTRIBUTE_GENERAL
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif

        ! get the rank of the dimension
        call GetDim(MemoryOrder,NDim,Status)
        if(Status /= WRF_NO_ERR) then
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif
        if((NDim+1)/= HDF5_NDim)then
           Status = WRF_HDF5_ERR_DATASPACE
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif
        call ext_phdf5_get_var_ti_char(DataHandle,"Stagger",Name,Stagger,Status)
        if(Status /= WRF_NO_ERR) then
           Status = WRF_HDF5_ERR_ATTRIBUTE_GENERAL
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif
        call ext_phdf5_get_var_ti_integer(DataHandle,"FieldType",Name,WrfType,Status)
        if(Status /= WRF_NO_ERR) then
           Status = WRF_HDF5_ERR_ATTRIBUTE_GENERAL
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif

        ! obtain Domain Start and Domain End.
        allocate(h5dims(NDim+1))
        allocate(h5maxdims(NDim+1))
        call h5sget_simple_extent_dims_f(dspaceid,h5dims,h5maxdims,hdf5err)
        if(hdf5err .lt. 0) then
           Status = WRF_HDF5_ERR_DATASPACE
           write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
           call wrf_debug ( WARN , msg) 
           return
        endif

        do j =1, NDim 
           DomainStart(j) = 1
           DomainEnd(j) = h5dims(j)
        enddo
        deallocate(h5dims)
        deallocate(h5maxdims)
     endif
     return
  endif
  return
end subroutine ext_phdf5_get_var_info

! obtain the domain time independent attribute with REAL type
subroutine ext_phdf5_get_dom_ti_real(DataHandle,Element,Data,Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  use get_attrid_routine
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real                  ,intent(out)    :: Data(*)
  real    ,dimension(:),allocatable     :: buffer
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer                               :: rank
  integer(hid_t)                        :: attr_type 
  integer(hsize_t), dimension(7)        :: h5_dims
  integer                               :: hdf5err

  ! Do nothing unless it is time to read time-independent domain metadata.
  IF ( .NOT. phdf5_ok_to_get_dom_ti( DataHandle ) ) THEN
    Status = WRF_NO_ERR
    return
  ENDIF

  attr_type = H5T_NATIVE_REAL

  call get_attrid(DataHandle,Element,h5_attrid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call check_type(DataHandle,attr_type,h5_attrid,Status)
  if (Status /= WRF_NO_ERR) then
     return
  endif

  call retrieve_ti_info(DataHandle,h5_attrid,h5_atypeid,&
       Count,OutCount,Status)
  if (Status /= WRF_NO_ERR) then
     return
  endif

  allocate(buffer(OutCount))

  h5_dims(1) = OutCount
  call h5aread_f(h5_attrid,attr_type,buffer,h5_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(buffer)
     return
  endif

  data(1:OutCount) = buffer(1:OutCount)

  deallocate(buffer)

  return

end subroutine ext_phdf5_get_dom_ti_real

! obtain the domain time independent attribute with REAL8 type
subroutine ext_phdf5_get_dom_ti_double(DataHandle,Element,Data,Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  use get_attrid_routine
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real*8                ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer                               :: rank
  integer                               :: hdf5err
  integer(hid_t)                        :: attr_type 
  integer(hsize_t), dimension(7)        :: h5_dims

  ! Do nothing unless it is time to read time-independent domain metadata.
  IF ( .NOT. phdf5_ok_to_get_dom_ti( DataHandle ) ) THEN
    Status = WRF_NO_ERR
    return
  ENDIF

  attr_type = H5T_NATIVE_DOUBLE
  call get_attrid(DataHandle,Element,h5_attrid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call check_type(DataHandle,attr_type,h5_attrid,Status)
  if (Status /= WRF_NO_ERR) then
     return
  endif

  call  retrieve_ti_info(DataHandle,h5_attrid,h5_atypeid,&
       Count,OutCount,Status)
  if (Status /= WRF_NO_ERR) then
     return
  endif

  h5_dims(1) = OutCount
  call h5aread_f(h5_attrid,attr_type,data,h5_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  return
end subroutine ext_phdf5_get_dom_ti_double


! obtain the domain time independent attribute with integer type
subroutine ext_phdf5_get_dom_ti_integer(DataHandle,Element,Data,Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules
  use get_attrid_routine
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  integer               ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer                               :: rank
  integer(hid_t)                        :: attr_type 
  integer(hsize_t), dimension(7)        :: h5_dims
  integer                               :: hdf5err

  ! Do nothing unless it is time to read time-independent domain metadata.
  IF ( .NOT. phdf5_ok_to_get_dom_ti( DataHandle ) ) THEN
    Status = WRF_NO_ERR
    return
  ENDIF

  attr_type = H5T_NATIVE_INTEGER

  call get_attrid(DataHandle,Element,h5_attrid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call check_type(DataHandle,attr_type,h5_attrid,Status)
  if (Status /= WRF_NO_ERR) then
     return
  endif

  call  retrieve_ti_info(DataHandle,h5_attrid,h5_atypeid,&
       Count,OutCount,Status)
  if (Status /= WRF_NO_ERR) then
     return
  endif

  h5_dims(1) = OutCount
  call h5aread_f(h5_attrid,attr_type,Data,h5_dims,Status)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  return
end subroutine ext_phdf5_get_dom_ti_integer


subroutine ext_phdf5_get_dom_ti_logical(DataHandle,Element,Data,Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules
  use get_attrid_routine
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)           :: DataHandle
  character*(*)         ,intent(in)           :: Element
  logical               ,intent(out)          :: Data(*)
  integer,       dimension(:),allocatable     :: buffer
  integer               ,intent(in)           :: Count
  integer               ,intent(out)          :: OutCount
  integer               ,intent(out)          :: Status
  integer(hid_t)                              :: h5_atypeid
  integer(hid_t)                              :: h5_aspaceid
  integer(hid_t)                              :: h5_attrid
  integer                                     :: rank
  integer(hid_t)                              :: attr_type 
  type(wrf_phdf5_data_handle),pointer          :: DH
  integer(hsize_t), dimension(7)              :: h5_dims
  integer                                     :: hdf5err


  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! Do nothing unless it is time to read time-independent domain metadata.
  IF ( .NOT. phdf5_ok_to_get_dom_ti( DataHandle ) ) THEN
    Status = WRF_NO_ERR
    return
  ENDIF

  attr_type = DH%EnumID
  call get_attrid(DataHandle,Element,h5_attrid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call check_type(DataHandle,attr_type,h5_attrid,Status)
  if (status /= WRF_NO_ERR) then
     return
  endif

  call  retrieve_ti_info(DataHandle,h5_attrid,h5_atypeid,&
       Count,OutCount,Status)
  if (Status /= WRF_NO_ERR) then
     return
  endif

  h5_dims(1) = OutCount

  allocate(buffer(OutCount))

  call h5aread_f(h5_attrid,attr_type,buffer,h5_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(buffer)
     return
  endif

  Data(1:OutCount) = buffer(1:OutCount)==1
  deallocate(buffer)
  return
end subroutine ext_phdf5_get_dom_ti_logical

! obtain the domain time independent attribute with char type
subroutine ext_phdf5_get_dom_ti_char(DataHandle,Element,Data,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  use get_attrid_routine
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(out)    :: Data
  integer                               :: Count
  integer                               :: OutCount
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer                               :: rank
  integer(hid_t)                        :: attr_type 
  integer(hsize_t), dimension(7)        :: h5_dims
  integer                               :: hdf5err

  ! Do nothing unless it is time to read time-independent domain metadata.
  IF ( .NOT. phdf5_ok_to_get_dom_ti( DataHandle ) ) THEN
    Status = WRF_NO_ERR
    return
  ENDIF

  attr_type = H5T_NATIVE_CHARACTER

  call get_attrid(DataHandle,Element,h5_attrid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call check_type(DataHandle,attr_type,h5_attrid,Status)
  if (status /= WRF_NO_ERR) then
     return
  endif

  call  retrieve_ti_info(DataHandle,h5_attrid,h5_atypeid,&
       Count,OutCount,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  h5_dims(1) = OutCount
  call h5aread_f(h5_attrid,h5_atypeid,data,h5_dims,hdf5err) 
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  return
end subroutine ext_phdf5_get_dom_ti_char

subroutine ext_phdf5_put_dom_td_real(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real                  ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_phdf5_put_var_td_real(DataHandle,Element,DateStr,&
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_',&
       Data,Count,Status)
  return
end subroutine ext_phdf5_put_dom_td_real

subroutine ext_phdf5_put_dom_td_double(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real*8                ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_phdf5_put_var_td_double(DataHandle,Element,DateStr,&
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_',&
       Data,Count,Status)
  return
end subroutine ext_phdf5_put_dom_td_double

subroutine ext_phdf5_put_dom_td_logical(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  logical               ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_phdf5_put_var_td_logical(DataHandle,Element,DateStr,&
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_',&
       Data,Count,Status)
  return

end subroutine ext_phdf5_put_dom_td_logical
subroutine ext_phdf5_put_dom_td_integer(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_phdf5_put_var_td_integer(DataHandle,Element,DateStr,&
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_',&
       Data,Count,Status)
  return
end subroutine ext_phdf5_put_dom_td_integer

subroutine ext_phdf5_put_dom_td_char(DataHandle,Element,DateStr,Data,Status)

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Data
  integer               ,intent(out)    :: Status

  call ext_phdf5_put_var_td_char(DataHandle,Element,DateStr,&
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_',&
       Data,Status)
  return

end subroutine ext_phdf5_put_dom_td_char

subroutine ext_phdf5_get_dom_td_real(DataHandle,Element,DateStr,Data,Count,OutCount,Status)


  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  real                  ,intent(out)            :: Data(*)
  integer               ,intent(in)             :: Count
  integer               ,intent(out)            :: OutCount
  integer               ,intent(out)            :: Status

  call ext_phdf5_get_var_td_real(DataHandle,Element,DateStr,&
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_',Data,Count,OutCount,Status)
  return
end subroutine ext_phdf5_get_dom_td_real

subroutine ext_phdf5_get_dom_td_double(DataHandle,Element,DateStr,Data,Count,OutCount,Status)

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  real*8                ,intent(out)            :: Data(*)
  integer               ,intent(in)             :: Count
  integer               ,intent(out)            :: OutCount
  integer               ,intent(out)            :: Status

  call ext_phdf5_get_var_td_double(DataHandle,Element,DateStr,&
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_',Data,Count,OutCount,Status)
  return
end subroutine ext_phdf5_get_dom_td_double


subroutine ext_phdf5_get_dom_td_integer(DataHandle,Element,DateStr,Data,Count,OutCount,Status)

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  integer               ,intent(out)            :: Data(*)
  integer               ,intent(in)             :: Count
  integer               ,intent(out)            :: OutCount
  integer               ,intent(out)            :: Status

  call ext_phdf5_get_var_td_integer(DataHandle,Element,DateStr,&
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_',Data,Count,OutCount,Status)
  return

end subroutine ext_phdf5_get_dom_td_integer

subroutine ext_phdf5_get_dom_td_logical(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  logical               ,intent(out)            :: Data(*)
  integer               ,intent(in)             :: Count
  integer               ,intent(out)            :: OutCount
  integer               ,intent(out)            :: Status

  call ext_phdf5_get_var_td_logical(DataHandle,Element,DateStr,&
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_',Data,Count,OutCount,Status)
  return

end subroutine ext_phdf5_get_dom_td_logical


subroutine ext_phdf5_get_dom_td_char(DataHandle,Element,DateStr,Data,Status)

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  character*(*)         ,intent(out)            :: Data
  integer               ,intent(out)            :: Status


  call ext_phdf5_get_var_td_char(DataHandle,Element,DateStr,&
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_',Data,Status)
  return


end subroutine ext_phdf5_get_dom_td_char

subroutine ext_phdf5_put_var_td_real(DataHandle,Element,DateStr,Var,Data,Count,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  character*(*)         ,intent(in)             :: Var
  character(len = 256)                           :: DataSetName
  real                  ,intent(in)             :: Data(*)
  integer               ,intent(in)             :: Count
  integer               ,intent(out)            :: Status
  type(wrf_phdf5_data_handle),pointer           :: DH
  integer                                       :: TimeIndex
  integer(hid_t)                                :: dset_id
  integer(hid_t)                                :: dspaceid
  integer(hid_t)                                :: fspaceid
  integer(hid_t)                                :: tgroupid
  integer(hsize_t),dimension(1)                 :: dims              
  integer                                       :: hdf5err
  integer                                       :: i

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! check whether the DateStr is the correct length
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then

     dims(1) = Count

     ! Get the time index
     call GetAttrTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     ! Set up dataspace,property list
     call GetName(Element,Var,DataSetName,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     call setup_wrtd_dataset(DataHandle,DataSetName,H5T_NATIVE_REAL,Count,&
          dset_id,dspaceid,fspaceid,tgroupid,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     call h5dwrite_f(dset_id,H5T_NATIVE_REAL,Data,dims,hdf5err,dspaceid,&
          fspaceid)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASET_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
     call h5dclose_f(dset_id,hdf5err)
     call h5sclose_f(dspaceid,hdf5err)
     call h5sclose_f(fspaceid,hdf5err)
!     call h5gclose_f(tgroupid,hdf5err)
  endif
  return
end subroutine ext_phdf5_put_var_td_real

subroutine ext_phdf5_put_var_td_double(DataHandle,Element,DateStr,Var,Data,Count,Status)
  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  character*(*)         ,intent(in)             :: Var
  character(len = 256)                           :: DataSetName
  real*8                ,intent(in)             :: Data(*)
  integer               ,intent(in)             :: Count
  integer               ,intent(out)            :: Status
  type(wrf_phdf5_data_handle),pointer            :: DH
  integer                                       :: TimeIndex
  integer(hid_t)                                :: dset_id
  integer(hid_t)                                :: dspaceid
  integer(hid_t)                                :: fspaceid
  integer(hid_t)                                :: tgroupid
  integer(hsize_t),dimension(1)                 :: dims              
  integer                                       :: hdf5err
  integer                                       :: i

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! check whether the DateStr is the correct length
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then


     dims(1) = Count
     ! Get the time index
     call GetAttrTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     ! Set up dataspace,property list
     call GetName(Element,Var,DataSetName,Status)
     call setup_wrtd_dataset(DataHandle,DataSetName,H5T_NATIVE_DOUBLE,Count,&
          dset_id,dspaceid,fspaceid,tgroupid,TimeIndex,Status) 

     if(Status /= WRF_NO_ERR) then
        return
     endif

     call h5dwrite_f(dset_id,H5T_NATIVE_DOUBLE,Data,dims,hdf5err,dspaceid,&
          fspaceid)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASET_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5dclose_f(dset_id,hdf5err)
     call h5sclose_f(dspaceid,hdf5err)
     call h5sclose_f(fspaceid,hdf5err)
!     call h5gclose_f(tgroupid,hdf5err)

  endif
  return
end subroutine ext_phdf5_put_var_td_double

subroutine ext_phdf5_put_var_td_integer(DataHandle,Element,DateStr,Var,Data,Count,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  character*(*)         ,intent(in)             :: Var
  character(len = 256)                           :: DataSetName
  integer               ,intent(in)             :: Data(*)
  integer               ,intent(in)             :: Count
  integer               ,intent(out)            :: Status
  type(wrf_phdf5_data_handle),pointer            :: DH
  integer                                       :: TimeIndex
  integer(hid_t)                                :: dset_id
  integer(hid_t)                                :: dspaceid
  integer(hid_t)                                :: fspaceid
  integer(hid_t)                                :: tgroupid
  integer(hsize_t),dimension(1)                 :: dims              
  integer                                       :: hdf5err
  integer                                       :: i

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! check whether the DateStr is the correct length
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then


     dims(1) = Count
     ! Get the time index
     call GetAttrTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     ! Set up dataspace,property list
     call GetName(Element,Var,DataSetName,Status)

     call setup_wrtd_dataset(DataHandle,DataSetName,H5T_NATIVE_INTEGER, &
          Count,dset_id,dspaceid,fspaceid,tgroupid,  &
          TimeIndex, Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     call h5dwrite_f(dset_id,H5T_NATIVE_INTEGER,Data,dims,hdf5err,dspaceid,&
          fspaceid)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASET_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5dclose_f(dset_id,hdf5err)
     call h5sclose_f(dspaceid,hdf5err)
     call h5sclose_f(fspaceid,hdf5err)
!     call h5gclose_f(tgroupid,hdf5err)

  endif
  return

end subroutine ext_phdf5_put_var_td_integer

subroutine ext_phdf5_put_var_td_logical(DataHandle,Element,DateStr,Var,Data,Count,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  character*(*)         ,intent(in)             :: Var
  character(len = 256)                           :: DataSetName
  logical               ,intent(in)             :: Data(*)
  integer ,dimension(:),allocatable             :: Buffer              
  integer               ,intent(in)             :: Count
  integer               ,intent(out)            :: Status
  type(wrf_phdf5_data_handle),pointer            :: DH
  integer                                       :: TimeIndex
  integer(hid_t)                                :: dset_id
  integer(hid_t)                                :: dspaceid
  integer(hid_t)                                :: fspaceid
  integer(hid_t)                                :: tgroupid
  integer(hsize_t),dimension(1)                 :: dims              
  integer                                       :: hdf5err
  integer                                       :: i

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! check whether the DateStr is the correct length
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then

     allocate(buffer(count))
     do i = 1, count
        if(data(i).EQV..TRUE.) then
           buffer(i) = 1
        else
           buffer(i) = 0
        endif
     enddo

     dims(1) = Count
     ! Get the time index
     call GetAttrTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     ! Set up dataspace,property list
     call GetName(Element,Var,DataSetName,Status)

     call setup_wrtd_dataset(DataHandle,DataSetName,DH%EnumID, &
          Count,dset_id,dspaceid,           &
          fspaceid,tgroupid,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     call h5dwrite_f(dset_id,DH%EnumID,Buffer,dims,hdf5err,dspaceid,&
          fspaceid)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASET_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
     call h5dclose_f(dset_id,hdf5err)
     call h5sclose_f(dspaceid,hdf5err)
     call h5sclose_f(fspaceid,hdf5err)
!     call h5gclose_f(tgroupid,hdf5err)
     deallocate(Buffer)
  endif
  return 
end subroutine ext_phdf5_put_var_td_logical

subroutine ext_phdf5_put_var_td_char(DataHandle,Element,DateStr,Var,Data,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  character*(*)         ,intent(in)             :: Var
  character(len = 256)                           :: DataSetName
  character*(*)         ,intent(in)             :: Data
  integer               ,intent(out)            :: Status
  type(wrf_phdf5_data_handle),pointer           :: DH
  integer                                       :: TimeIndex
  integer(hid_t)                                :: dset_id
  integer(hid_t)                                :: dspaceid
  integer(hid_t)                                :: fspaceid
  integer(hid_t)                                :: tgroupid
  integer(hsize_t),dimension(1)                 :: dims              
  integer                                       :: hdf5err
  integer                                       :: i

  integer                                       :: str_id
  integer                                       :: str_len
  integer                                       :: count

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! check whether the DateStr is the correct length
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_OPENED_AND_COMMITTED) then

     dims(1) = 1

     ! Get the time index
     call GetAttrTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     ! make str id
     str_len = len_trim(Data)
     call make_strid(str_len,str_id,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     ! assign count of the string to 1
     count = 1

     ! Set up dataspace,property list
     call GetName(Element,Var,DataSetName,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif
     call setup_wrtd_dataset(DataHandle,DataSetName,str_id, &
          count,dset_id,dspaceid,        &
          fspaceid,tgroupid,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     call h5dwrite_f(dset_id,str_id,Data,dims,hdf5err,dspaceid,&
          fspaceid)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASET_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     ! close the string id
     call h5tclose_f(str_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
     call h5dclose_f(dset_id,hdf5err)
     call h5sclose_f(dspaceid,hdf5err)
     call h5sclose_f(fspaceid,hdf5err)
!     call h5gclose_f(tgroupid,hdf5err)

  endif
  return

end subroutine ext_phdf5_put_var_td_char

subroutine ext_phdf5_get_var_td_real(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  character*(*)         ,intent(in)             :: Var
  character(len =256)                            :: DataSetName
  real                  ,intent(out)            :: Data(*)
  integer               ,intent(in)             :: Count
  integer               ,intent(out)            :: OutCount
  integer               ,intent(out)            :: Status
  type(wrf_phdf5_data_handle),pointer            :: DH
  integer                                       :: TimeIndex
  integer(hid_t)                                :: dset_id
  integer(hid_t)                                :: dspaceid
  integer(hid_t)                                :: memspaceid
  integer(hid_t)                                :: tgroupid
  integer(hsize_t),dimension(7)                 :: data_dims              
  integer                                       :: hdf5err

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! check whether the DateStr is the correct length
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then

     ! get the time-dependent attribute name
     
     call GetName(Element,Var,DataSetName,Status)

     ! get time index of the time-dependent attribute
     call GetAttrTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     ! For parallel, find the group and obtain the attribute.
     ! set up for reading the time-dependent attribute
     call setup_rdtd_dataset(DataHandle,DataSetName,H5T_NATIVE_REAL,TimeIndex,&
          Count,OutCount,dset_id,memspaceid,dspaceid,tgroupid,&
          Status)
     if(Status /= WRF_NO_ERR) then
	return
     endif

     data_dims(1) = OutCount

     ! read the dataset
     call h5dread_f(dset_id,H5T_NATIVE_REAL,data,data_dims,hdf5err, &
          memspaceid,dspaceid,H5P_DEFAULT_F)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_READ
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
     call h5sclose_f(memspaceid,hdf5err)
     call h5sclose_f(dspaceid,hdf5err)
     call h5dclose_f(dset_id,hdf5err)
     call h5gclose_f(tgroupid,hdf5err)
  endif

end subroutine ext_phdf5_get_var_td_real

subroutine ext_phdf5_get_var_td_double(DataHandle,Element,DateStr,Var,Data,&
     Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  character*(*)         ,intent(in)             :: Var
  character(len =256)                            :: DataSetName
  real*8                ,intent(out)            :: Data(*)
  integer               ,intent(in)             :: Count
  integer              ,intent(out)            :: OutCount
  integer               ,intent(out)            :: Status
  type(wrf_phdf5_data_handle),pointer            :: DH
  integer                                       :: TimeIndex
  integer(hid_t)                                :: dset_id
  integer(hid_t)                                :: dspaceid
  integer(hid_t)                                :: memspaceid
  integer(hid_t)                                :: tgroupid
  integer(hsize_t),dimension(7)                 :: data_dims              
  integer                                       :: hdf5err

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! check whether the DateStr is the correct length
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then

     ! get the time-dependent attribute name
     call GetName(Element,Var,DataSetName,Status)

     ! get time index of the time-dependent attribute
     call GetAttrTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     ! set up for reading the time-dependent attribute
     call setup_rdtd_dataset(DataHandle,DataSetName,H5T_NATIVE_DOUBLE,TimeIndex,&
          Count,OutCount,dset_id,memspaceid,dspaceid,tgroupid,&
          Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     data_dims(1) = OutCount

     ! read the dataset
     call h5dread_f(dset_id,H5T_NATIVE_DOUBLE,data,data_dims,hdf5err, &
          memspaceid,dspaceid,H5P_DEFAULT_F)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_READ
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5sclose_f(memspaceid,hdf5err)
     call h5sclose_f(dspaceid,hdf5err)
     call h5dclose_f(dset_id,hdf5err)
     call h5gclose_f(tgroupid,hdf5err)

  endif

end subroutine ext_phdf5_get_var_td_double

subroutine ext_phdf5_get_var_td_integer(DataHandle,Element,DateStr,Var,Data,&
     Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  character*(*)         ,intent(in)             :: Var
  character(len =256)                            :: DataSetName
  integer               ,intent(out)             :: Data(*)
  integer               ,intent(in)             :: Count
  INTEGER    		,intent(out)            :: OutCount
  integer               ,intent(out)            :: Status
  type(wrf_phdf5_data_handle),pointer            :: DH
  integer                                       :: TimeIndex
  integer(hid_t)                                :: dset_id
  integer(hid_t)                                :: dspaceid
  integer(hid_t)                                :: memspaceid
  integer(hid_t)                                :: tgroupid
  integer(hsize_t),dimension(7)                 :: data_dims              
  integer                                       :: hdf5err

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! check whether the DateStr is the correct length
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then

     ! get the time-dependent attribute name
     call GetName(Element,Var,DataSetName,Status)

     ! get time index of the time-dependent attribute
     call GetAttrTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     ! set up for reading the time-dependent attribute
     call setup_rdtd_dataset(DataHandle,DataSetName,H5T_NATIVE_INTEGER,TimeIndex,&
          Count,OutCount,dset_id,memspaceid,dspaceid,tgroupid,&
          Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     data_dims(1) = OutCount

     ! read the dataset
     call h5dread_f(dset_id,H5T_NATIVE_INTEGER,data,data_dims,hdf5err, &
          memspaceid,dspaceid,H5P_DEFAULT_F)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_READ
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5sclose_f(memspaceid,hdf5err)
     call h5sclose_f(dspaceid,hdf5err)
     call h5dclose_f(dset_id,hdf5err)
     call h5gclose_f(tgroupid,hdf5err)
  endif
end subroutine ext_phdf5_get_var_td_integer

subroutine ext_phdf5_get_var_td_logical(DataHandle,Element,DateStr,Var,Data,&
     Count,OutCount,Status)
  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  character*(*)         ,intent(in)             :: Var
  character(len =256)                            :: DataSetName
  logical               ,intent(out)            :: Data(*)
  integer,         dimension(:),allocatable     :: Buffer   
  integer               ,intent(in)             :: Count
  integer               ,intent(out)            :: OutCount
  integer               ,intent(out)            :: Status
  type(wrf_phdf5_data_handle),pointer            :: DH
  integer                                       :: TimeIndex
  integer(hid_t)                                :: dset_id
  integer(hid_t)                                :: dspaceid
  integer(hid_t)                                :: memspaceid
  integer(hid_t)                                :: tgroupid
  integer(hsize_t),dimension(7)                 :: data_dims              
  integer                                       :: hdf5err

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! check whether the DateStr is the correct length
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then

     ! get the time-dependent attribute name
     call GetName(Element,Var,DataSetName,Status)

     ! get time index of the time-dependent attribute
     call GetAttrTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     ! set up for reading the time-dependent attribute
     call setup_rdtd_dataset(DataHandle,DataSetName,DH%EnumID,TimeIndex,&
          Count,OutCount,dset_id,memspaceid,dspaceid,&
          tgroupid,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     data_dims(1) = OutCount
     ! read the dataset

     allocate(Buffer(OutCount))
     call h5dread_f(dset_id,DH%EnumID,buffer,data_dims,hdf5err, &
          memspaceid,dspaceid,H5P_DEFAULT_F)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_READ
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
     data(1:OutCount) = buffer(1:OutCount) == 1
     deallocate(buffer)
     call h5sclose_f(memspaceid,hdf5err)
     call h5sclose_f(dspaceid,hdf5err)
     call h5dclose_f(dset_id,hdf5err)
     call h5gclose_f(tgroupid,hdf5err)
  endif

end subroutine ext_phdf5_get_var_td_logical

subroutine ext_phdf5_get_var_td_char(DataHandle,Element,DateStr,Var,Data,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)             :: DataHandle
  character*(*)         ,intent(in)             :: Element
  character*(*)         ,intent(in)             :: DateStr
  character*(*)         ,intent(in)             :: Var
  character(len =256)                            :: DataSetName
  character*(*)         ,intent(out)             :: Data
  integer                                       :: Count
  integer                                       :: OutCount
  integer               ,intent(out)            :: Status
  type(wrf_phdf5_data_handle),pointer            :: DH
  integer                                       :: TimeIndex
  integer(hid_t)                                :: dset_id
  integer(hid_t)                                :: dspaceid
  integer(hid_t)                                :: memspaceid
  integer(hid_t)                                :: tgroupid
  integer(hsize_t),dimension(7)                 :: data_dims              
  integer                                       :: hdf5err

  integer(hid_t)                                :: str_id

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! check whether the DateStr is the correct length
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then

     ! get the time-dependent attribute name
     call GetName(Element,Var,DataSetName,Status)

     ! get time index of the time-dependent attribute
     call GetAttrTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     ! set up for reading the time-dependent attribute
     str_id = H5T_NATIVE_CHARACTER
     Count  = 1
     call setup_rdtd_dataset(DataHandle,DataSetName,str_id,TimeIndex,&
          Count,OutCount,dset_id,memspaceid,dspaceid,&
          tgroupid,Status)
     if(Status /= WRF_NO_ERR) then
        return
     endif

     data_dims(1) = Count

     ! read the dataset
     call h5dread_f(dset_id,str_id,data,data_dims,hdf5err, &
          memspaceid,dspaceid,H5P_DEFAULT_F)
     if(hdf5err.lt.0) then
        Status = WRF_HDF5_ERR_DATASET_READ
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
     call h5sclose_f(memspaceid,hdf5err)
     call h5sclose_f(dspaceid,hdf5err)
     call h5dclose_f(dset_id,hdf5err)
     call h5gclose_f(tgroupid,hdf5err)
  endif

end subroutine ext_phdf5_get_var_td_char

! obtain the variable time independent attribute with REAL type
subroutine ext_phdf5_get_var_ti_real(DataHandle,Element,Var,Data,Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  use get_attrid_routine
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  real                  ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hid_t)                        :: attr_type 
  integer(hsize_t), dimension(7)        :: h5_dims
  integer                               :: hdf5err

  attr_type = H5T_NATIVE_REAL

  call get_attrid(DataHandle,Element,h5_attrid,Status,Var)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call check_type(DataHandle,attr_type,h5_attrid,Status)
  if (status /= WRF_NO_ERR) then 
     return
  endif

  call  retrieve_ti_info(DataHandle,h5_attrid,h5_atypeid,&
       Count,OutCount,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  h5_dims(1) = OutCount
  call h5aread_f(h5_attrid,attr_type,data,h5_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  return
end subroutine ext_phdf5_get_var_ti_real

! obtain the variable time independent attribute with REAL8 type
subroutine ext_phdf5_get_var_ti_double(DataHandle,Element,Var,Data,Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  use get_attrid_routine
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  real*8                ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hid_t)                        :: attr_type 
  integer(hsize_t), dimension(7)        :: h5_dims
  integer                               :: hdf5err

  attr_type = H5T_NATIVE_DOUBLE

  call get_attrid(DataHandle,Element,h5_attrid,Status,Var)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call check_type(DataHandle,attr_type,h5_attrid,Status)
  if (status /= WRF_NO_ERR) then 
     return
  endif

  call  retrieve_ti_info(DataHandle,h5_attrid,h5_atypeid,&
       Count,OutCount,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  h5_dims(1) = OutCount
  call h5aread_f(h5_attrid,attr_type,data,h5_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

end subroutine ext_phdf5_get_var_ti_double

! obtain the variable time independent attribute with integer type
subroutine ext_phdf5_get_var_ti_integer(DataHandle,Element,Var,Data,Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  use get_attrid_routine
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  integer               ,intent(out)    :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hid_t)                        :: attr_type 
  integer(hsize_t), dimension(7)        :: h5_dims
  integer                               :: hdf5err

  attr_type = H5T_NATIVE_INTEGER

  call get_attrid(DataHandle,Element,h5_attrid,Status,Var)
  if (status /= WRF_NO_ERR) then
     return
  endif

  call check_type(DataHandle,attr_type,h5_attrid,Status)
  if (status /= WRF_NO_ERR) then
     return
  endif

  call  retrieve_ti_info(DataHandle,h5_attrid,h5_atypeid,&
       Count,OutCount,Status)
  if (status /= WRF_NO_ERR) then
     return
  endif

  h5_dims(1) = OutCount
  call h5aread_f(h5_attrid,attr_type,data,h5_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  return

end subroutine ext_phdf5_get_var_ti_integer

! obtain the variable time independent attribute with logical type
subroutine ext_phdf5_get_var_ti_logical(DataHandle,Element,Var,Data,Count,OutCount,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  use get_attrid_routine
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  logical               ,intent(out)    :: Data(*)
  integer, dimension(:),allocatable     :: Buffer
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hid_t)                        :: attr_type 
  type(wrf_phdf5_data_handle),pointer    :: DH
  integer(hsize_t), dimension(7)        :: h5_dims
  integer                               :: hdf5err

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  attr_type = DH%EnumID
  call get_attrid(DataHandle,Element,h5_attrid,Status,Var)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call check_type(DataHandle,attr_type,h5_attrid,Status)
  if (status /= WRF_NO_ERR) then
     return
  endif

  call  retrieve_ti_info(DataHandle,h5_attrid,h5_atypeid,&
       Count,OutCount,Status)
  if (status /= WRF_NO_ERR) then
     return
  endif

  h5_dims(1) = OutCount

  allocate(buffer(OutCount))
  call h5aread_f(h5_attrid,attr_type,buffer,h5_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(buffer)
     return
  endif

  Data(1:OutCount) = buffer(1:OutCount)==1
  deallocate(buffer)
  return

end subroutine ext_phdf5_get_var_ti_logical


! obtain the domain variable independent attribute with Char type
subroutine ext_phdf5_get_var_ti_char(DataHandle,Element,Var,Data,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  use get_attrid_routine
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  character*(*)         ,intent(out)    :: Data
  integer               ,intent(out)    :: Status

  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hid_t)                        :: attr_type 
  integer(hsize_t), dimension(7)        :: h5_dims
  integer                               :: Count
  integer                               :: OutCount
  integer                               :: hdf5err

  attr_type = H5T_NATIVE_CHARACTER
  call get_attrid(DataHandle,Element,h5_attrid,Status,Var)
  if (status /= WRF_NO_ERR) then
     return
  endif

  call check_type(DataHandle,attr_type,h5_attrid,Status)
  if (status /= WRF_NO_ERR) then
     return
  endif

  call  retrieve_ti_info(DataHandle,h5_attrid,h5_atypeid,&
       Count,OutCount,Status)
  if (status /= WRF_NO_ERR) then
     return
  endif

  if(OutCount /= 1) then
     Status = WRF_HDF5_ERR_ATTRIBUTE_OTHERS
  endif
  h5_dims(1) = OutCount
  call h5aread_f(h5_attrid,h5_atypeid,data,h5_dims,hdf5err) 
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  return

end subroutine ext_phdf5_get_var_ti_char


! write the domain time independent attribute with real type
subroutine ext_phdf5_put_dom_ti_real(DataHandle,Element,Data,Count,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real                  ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  integer(hid_t)                        :: h5_objid
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hsize_t), dimension(7)        :: adata_dims
  character*3                           :: routine_type
  integer                               :: routine_atype
  integer                               :: str_flag = 0 ! not a string type
  integer(hid_t)                        :: hdf5err
  character(VarNameLen)                 :: var

  ! Do nothing unless it is time to write time-independent domain metadata.
  IF ( .NOT. phdf5_ok_to_put_dom_ti( DataHandle ) ) THEN
    Status = WRF_NO_ERR
    return
  ENDIF

  var = 'DUMMY'
  routine_type = 'DOM'
  routine_atype = WRF_REAL
  adata_dims(1) = Count

  call create_phdf5_objid(DataHandle,h5_objid,routine_type,var,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adtypeid(h5_atypeid,routine_atype,Count,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adspaceid(Count,str_flag,h5_aspaceid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call h5acreate_f(h5_objid,Element,h5_atypeid,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5awrite_f(h5_attrid,h5_atypeid,Data,adata_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call clean_phdf5_attrids(h5_atypeid,h5_aspaceid,h5_attrid,str_flag,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  return
end subroutine ext_phdf5_put_dom_ti_real

! write the domain time independent attribute with integer type
subroutine ext_phdf5_put_dom_ti_integer(DataHandle,Element,Data,Count,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  integer               ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_objid
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hsize_t), dimension(7)        :: adata_dims
  character*3                           :: routine_type
  integer                               :: routine_atype
  integer                               :: str_flag = 0 ! not a string type
  integer(hid_t)                        :: hdf5err
  character(VarNameLen)                 :: var

  ! Do nothing unless it is time to write time-independent domain metadata.
  IF ( .NOT. phdf5_ok_to_put_dom_ti( DataHandle ) ) THEN
    Status = WRF_NO_ERR
    return
  ENDIF

  var = 'DUMMY'
  routine_type = 'DOM'
  routine_atype = WRF_INTEGER
  adata_dims(1) = Count

  call create_phdf5_objid(DataHandle,h5_objid,routine_type,var,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adtypeid(h5_atypeid,routine_atype,Count,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adspaceid(Count,str_flag,h5_aspaceid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call h5acreate_f(h5_objid,Element,h5_atypeid,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5awrite_f(h5_attrid,h5_atypeid,Data,adata_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call clean_phdf5_attrids(h5_atypeid,h5_aspaceid,h5_attrid,str_flag,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  return
end subroutine ext_phdf5_put_dom_ti_integer

! write the domain time independent attribute with double type
subroutine ext_phdf5_put_dom_ti_double(DataHandle,Element,Data,Count,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real*8                ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_objid
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hsize_t), dimension(7)        :: adata_dims

  character*3                           :: routine_type
  integer                               :: routine_atype
  integer                               :: str_flag = 0 ! not a string type
  integer(hid_t)                        :: hdf5err
  character(VarNameLen)                 :: var

  ! Do nothing unless it is time to write time-independent domain metadata.
  IF ( .NOT. phdf5_ok_to_put_dom_ti( DataHandle ) ) THEN
    Status = WRF_NO_ERR
    return
  ENDIF

  var           = 'DUMMY'
  routine_type  = 'DOM'
  routine_atype = WRF_DOUBLE
  adata_dims(1) = Count

  call create_phdf5_objid(DataHandle,h5_objid,routine_type,var,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adtypeid(h5_atypeid,routine_atype,Count,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adspaceid(Count,str_flag,h5_aspaceid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call h5acreate_f(h5_objid,Element,h5_atypeid,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5awrite_f(h5_attrid,h5_atypeid,Data,adata_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call clean_phdf5_attrids(h5_atypeid,h5_aspaceid,h5_attrid,str_flag,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif
  return

end subroutine ext_phdf5_put_dom_ti_double

! write the domain time independent attribute with logical type
subroutine ext_phdf5_put_dom_ti_logical(DataHandle,Element,Data,Count,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)      :: DataHandle
  character*(*)         ,intent(in)      :: Element
  logical               ,intent(in)      :: Data(*)
  integer     ,dimension(:),allocatable  :: Buffer
  integer               ,intent(in)      :: Count
  integer               ,intent(out)     :: Status

  integer                                :: i
  integer(hid_t)                         :: h5_objid
  integer(hid_t)                         :: h5_atypeid
  integer(hid_t)                         :: h5_aspaceid
  integer(hid_t)                         :: h5_attrid
  integer(hsize_t), dimension(7)         :: adata_dims

  character*3                            :: routine_type
  integer                                :: routine_atype
  integer                                :: str_flag = 0 ! not a string type
  integer(hid_t)                         :: hdf5err
  character(VarNameLen)                  :: var

  ! Do nothing unless it is time to write time-independent domain metadata.
  IF ( .NOT. phdf5_ok_to_put_dom_ti( DataHandle ) ) THEN
    Status = WRF_NO_ERR
    return
  ENDIF

  var           = 'DUMMY'
  routine_type  = 'DOM'
  routine_atype = WRF_LOGICAL
  adata_dims(1) = Count

  allocate(Buffer(Count))

  do i = 1,Count
     if(Data(i) .EQV. .TRUE.) then
        Buffer(i) = 1
     else
        Buffer(i) = 0
     endif
  enddo

  call create_phdf5_objid(DataHandle,h5_objid,routine_type,var,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adtypeid(h5_atypeid,routine_atype,Count,Status,DataHandle)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adspaceid(Count,str_flag,h5_aspaceid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call h5acreate_f(h5_objid,Element,h5_atypeid,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(buffer)
     return
  endif

  call h5awrite_f(h5_attrid,h5_atypeid,Buffer,adata_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(buffer)
     return
  endif

  call clean_phdf5_attrids(h5_atypeid,h5_aspaceid,h5_attrid,str_flag,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  deallocate(Buffer)

end subroutine ext_phdf5_put_dom_ti_logical


! write the domain time independent attribute with char type
subroutine ext_phdf5_put_dom_ti_char(DataHandle,Element,Data,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

!!!! Need more work.
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Data
  integer                               :: Count ! always 1 for char
  integer               ,intent(out)    :: Status

  integer(hid_t)                        :: h5_objid
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hsize_t), dimension(7)        :: adata_dims
  character*3                           :: routine_type
  integer                               :: routine_atype
  integer                               :: str_flag = 1 ! is a string type
  integer(hid_t)                        :: hdf5err
  integer                               :: len_str
  character(VarNameLen)                 :: var
  character(1)                          :: RepData =' '

  ! Do nothing unless it is time to write time-independent domain metadata.
  IF ( .NOT. phdf5_ok_to_put_dom_ti( DataHandle ) ) THEN
    Status = WRF_NO_ERR
    return
  ENDIF

  Count = 1
  var = 'DUMMY'
  routine_type = 'DOM'
  routine_atype = WRF_CHARACTER
  adata_dims(1) = Count

  call create_phdf5_objid(DataHandle,h5_objid,routine_type,var,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  ! This part may need more work, a special case is that the length of the
  ! string may be 0, HDF5 cannot handle 0 length string(?),so set the length
  ! to 1

  len_str = len_trim(Data)
  if(len_str == 0) then
     len_str = 1
  endif

  call create_phdf5_adtypeid(h5_atypeid,routine_atype,len_str,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adspaceid(Count,str_flag,h5_aspaceid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call h5acreate_f(h5_objid,Element,h5_atypeid,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  if(len_trim(Data) == 0) then

     call h5awrite_f(h5_attrid,h5_atypeid,RepData,adata_dims,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
  else

     call h5awrite_f(h5_attrid,h5_atypeid,trim(Data),adata_dims,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
  endif

  call clean_phdf5_attrids(h5_atypeid,h5_aspaceid,h5_attrid,str_flag,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  return
end subroutine ext_phdf5_put_dom_ti_char

! write the variable time independent attribute with real type
subroutine ext_phdf5_put_var_ti_real(DataHandle,Element,Var,Data,Count,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var      
  real                  ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  integer(hid_t)                        :: h5_objid
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hsize_t), dimension(7)        :: adata_dims
  character*3                           :: routine_type
  integer                               :: routine_atype
  integer                               :: str_flag = 0 ! not a string type
  integer(hid_t)                        :: hdf5err
  type(wrf_phdf5_data_handle),pointer    :: DH


  routine_type = 'VAR'
  routine_atype = WRF_REAL
  adata_dims(1) = Count

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  ! The following two checks must be here to avoid duplicating attributes
  if (DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
     Status = WRF_NO_ERR
     return
  endif
  if(DH%TimeIndex > 1) then
     Status = WRF_NO_ERR
     return   
  endif

  call create_phdf5_objid(DataHandle,h5_objid,routine_type,Var,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adtypeid(h5_atypeid,routine_atype,Count,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adspaceid(Count,str_flag,h5_aspaceid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call h5acreate_f(h5_objid,Element,h5_atypeid,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5awrite_f(h5_attrid,h5_atypeid,Data,adata_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call clean_phdf5_attrids(h5_atypeid,h5_aspaceid,h5_attrid,str_flag,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  return
end subroutine ext_phdf5_put_var_ti_real

! write the variable time independent attribute with double type
subroutine ext_phdf5_put_var_ti_double(DataHandle,Element,Var,Data,Count,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real*8                ,intent(in)     :: Data(*)
  character*(*)         ,intent(in)     :: Var      
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  integer(hid_t)                        :: h5_objid
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hsize_t), dimension(7)        :: adata_dims

  character*3                           :: routine_type
  integer                               :: routine_atype
  integer                               :: str_flag = 0 ! not a string type
  integer(hid_t)                        :: hdf5err
  type(wrf_phdf5_data_handle),pointer    :: DH

  routine_type  = 'VAR'
  routine_atype = WRF_DOUBLE
  adata_dims(1) = Count

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  if (DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
     Status = WRF_NO_ERR
     return
  endif
  if(DH%TimeIndex > 1) then
     Status = WRF_NO_ERR
     return   
  endif

  call create_phdf5_objid(DataHandle,h5_objid,routine_type,Var,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adtypeid(h5_atypeid,routine_atype,Count,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adspaceid(Count,str_flag,h5_aspaceid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call h5acreate_f(h5_objid,Element,h5_atypeid,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  call h5awrite_f(h5_attrid,h5_atypeid,Data,adata_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call clean_phdf5_attrids(h5_atypeid,h5_aspaceid,h5_attrid,str_flag,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  return

end subroutine ext_phdf5_put_var_ti_double

! write the variable time independent attribute with integer type
subroutine ext_phdf5_put_var_ti_integer(DataHandle,Element,Var,Data,Count,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var      
  integer               ,intent(in)     :: Data(*)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  integer(hid_t)                        :: h5_objid
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hsize_t), dimension(7)        :: adata_dims

  character*3                           :: routine_type
  integer                               :: routine_atype
  integer                               :: str_flag = 0 ! not a string type
  integer(hid_t)                        :: hdf5err
  type(wrf_phdf5_data_handle),pointer    :: DH

  routine_type = 'VAR'
  routine_atype = WRF_INTEGER
  adata_dims(1) = Count

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  ! The following two checks must be here to avoid duplicating attributes
  if (DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
     Status = WRF_NO_ERR
     return
  endif
  if(DH%TimeIndex > 1) then
     Status = WRF_NO_ERR
     return   
  endif

  call create_phdf5_objid(DataHandle,h5_objid,routine_type,Var,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adtypeid(h5_atypeid,routine_atype,Count,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adspaceid(Count,str_flag,h5_aspaceid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call h5acreate_f(h5_objid,Element,h5_atypeid,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  call h5awrite_f(h5_attrid,h5_atypeid,Data,adata_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif


  call clean_phdf5_attrids(h5_atypeid,h5_aspaceid,h5_attrid,str_flag,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  return
end subroutine ext_phdf5_put_var_ti_integer


! write the variable time independent attribute with logical type
subroutine ext_phdf5_put_var_ti_logical(DataHandle,Element,Var,Data,Count,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var      
  logical               ,intent(in)     :: Data(*)
  integer     ,dimension(:),allocatable :: Buffer
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  integer                                :: i
  integer(hid_t)                        :: h5_objid
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hsize_t), dimension(7)        :: adata_dims

  character*3                           :: routine_type
  integer                               :: routine_atype
  integer                               :: str_flag = 0 ! not a string type
  integer(hid_t)                        :: hdf5err
  type(wrf_phdf5_data_handle),pointer    :: DH

  routine_type = 'VAR'
  routine_atype = WRF_LOGICAL
  adata_dims(1) = Count

  allocate(Buffer(Count))

  do i = 1,Count
     if(Data(i) .EQV. .TRUE.) then
        Buffer(i) = 1
     else
        Buffer(i) = 0
     endif
  enddo

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  ! The following two checks must be here to avoid duplicating attributes
  if (DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
     Status = WRF_NO_ERR
     return
  endif

  if(DH%TimeIndex > 1) then
     Status = WRF_NO_ERR
     return   
  endif

  call create_phdf5_objid(DataHandle,h5_objid,routine_type,var,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adtypeid(h5_atypeid,routine_atype,Count,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adspaceid(Count,str_flag,h5_aspaceid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call h5acreate_f(h5_objid,Element,h5_atypeid,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(buffer)
     return
  endif


  call h5awrite_f(h5_attrid,h5_atypeid,Buffer,adata_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(buffer)
     return
  endif

  call clean_phdf5_attrids(h5_atypeid,h5_aspaceid,h5_attrid,str_flag,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  return
end subroutine ext_phdf5_put_var_ti_logical

! write the variable time independent attribute with char type
subroutine ext_phdf5_put_var_ti_char(DataHandle,Element,Var,Data,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  USE HDF5 ! This module contains all necessary modules 
  implicit none
  include 'wrf_status_codes.h'

  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Data
  character*(*)         ,intent(in)     :: Var      
  integer                               :: Count
  integer               ,intent(out)    :: Status
  integer(hid_t)                        :: h5_objid
  integer(hid_t)                        :: h5_atypeid
  integer(hid_t)                        :: h5_aspaceid
  integer(hid_t)                        :: h5_attrid
  integer(hsize_t), dimension(7)        :: adata_dims

  character*3                           :: routine_type
  integer                               :: routine_atype
  integer                               :: str_flag = 1 ! IS  string type
  integer(hid_t)                        :: hdf5err
  integer                               :: len_str
  character(1)                          :: RepData = ' '
  type(wrf_phdf5_data_handle),pointer    :: DH

  Count         = 1
  routine_type  = 'VAR'
  routine_atype = WRF_CHARACTER
  adata_dims(1) = Count

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
          ', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  ! The following two checks must be here to avoid duplicating attributes
  if (DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
     Status = WRF_NO_ERR
     return
  endif

  if(DH%TimeIndex > 1) then
     Status = WRF_NO_ERR
     return   
  endif

  call create_phdf5_objid(DataHandle,h5_objid,routine_type,Var,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  len_str = len_trim(Data)

  if(len_str .eq. 0) then
     len_str = 1
  endif

  call create_phdf5_adtypeid(h5_atypeid,routine_atype,len_str,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call create_phdf5_adspaceid(Count,str_flag,h5_aspaceid,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  call h5acreate_f(h5_objid,Element,h5_atypeid,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  if(len_trim(Data) == 0) then

     call h5awrite_f(h5_attrid,h5_atypeid,RepData,adata_dims,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
  else
     call h5awrite_f(h5_attrid,h5_atypeid,trim(Data),adata_dims,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif
  endif

  call clean_phdf5_attrids(h5_atypeid,h5_aspaceid,h5_attrid,str_flag,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  return
end subroutine ext_phdf5_put_var_ti_char



! This routine will retrieve the dimensional table, should be useful
! for tool developers.

subroutine retrieve_table(DataHandle,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use hdf5
  implicit none
  include 'wrf_status_codes.h'   

  character*256,dimension(MaxTabDims)    :: dim_name
  integer,dimension(:),allocatable      :: length
  integer,dimension(:),allocatable      :: unlimited
  integer, intent(in)                   :: DataHandle
  integer, intent(out)                  :: Status

  integer(hid_t)                        :: dset_id
  integer(hid_t)                        :: dataspace_id
  integer(hid_t)                        :: dtstr_id
  integer(hid_t)                        :: dt1_id
  integer(hid_t)                        :: dtint1_id
  integer(hid_t)                        :: dtint2_id
  integer(size_t)                       :: type_sizei
  integer(size_t)                       :: offset
  integer                               :: table_length
  integer(size_t)                       :: string_size
  integer(hsize_t),dimension(7)         :: data_dims
  integer(hsize_t)                      :: table_size
  integer                               :: i
  integer                               :: hdf5err

  type(wrf_phdf5_data_handle),pointer    :: DH

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  call h5dopen_f(DH%DimGroupID,"h5dim_table",dset_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATASET_OPEN
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5dget_space_f(dset_id,dataspace_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5sget_simple_extent_npoints_f(dataspace_id,table_size,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  data_dims(1) = table_size
  allocate(length(table_size))
  allocate(unlimited(table_size))


  ! the name of the dimension
  call h5tcopy_f(H5T_NATIVE_CHARACTER,dtstr_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  string_size = 256
  call h5tset_size_f(dtstr_id,string_size,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  call h5tcreate_f(H5T_COMPOUND_F,string_size,dt1_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  offset = 0
  call h5tinsert_f(dt1_id,"dim_name",offset,dtstr_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  call h5dread_f(dset_id,dt1_id,dim_name,data_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATASET_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  ! the length of the dimension
  call h5tget_size_f(H5T_NATIVE_INTEGER,type_sizei,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  call h5tcreate_f(H5T_COMPOUND_F,type_sizei,dtint1_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  offset = 0
  call h5tinsert_f(dtint1_id,"dim_length",offset,H5T_NATIVE_INTEGER,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  call h5dread_f(dset_id,dtint1_id,length,data_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATASET_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif


  ! the unlimited info. of the dimension
  call h5tget_size_f(H5T_NATIVE_INTEGER,type_sizei,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  call h5tcreate_f(H5T_COMPOUND_F,type_sizei,dtint2_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  offset = 0
  call h5tinsert_f(dtint2_id,"dim_unlimited",offset,H5T_NATIVE_INTEGER,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  call h5dread_f(dset_id,dtint2_id,unlimited,data_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATASET_READ
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  ! Store the information to the table array
  do i =1,table_size
     DH%DIMTABLE(i)%dim_name = dim_name(i)
     DH%DIMTABLE(i)%length   = length(i)
     DH%DIMTABLE(i)%unlimited = unlimited(i)
  enddo

  deallocate(length)
  deallocate(unlimited)

  call h5tclose_f(dtint1_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_CLOSE_GENERAL
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

  call h5tclose_f(dtint2_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_CLOSE_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5tclose_f(dt1_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_CLOSE_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5sclose_f(dataspace_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_CLOSE_GENERAL
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5dclose_f(dset_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATASET_CLOSE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  Status = WRF_NO_ERR
  return
end subroutine retrieve_table

! store(write) the dimensional table into the HDF5 file
subroutine store_table(DataHandle,table_length,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use hdf5
  implicit none
  include 'wrf_status_codes.h'   

  integer ,intent(in)                            :: DataHandle
  integer, intent(in)                            :: table_length
  integer, intent(out)                           :: Status

  type(wrf_phdf5_data_handle),pointer             :: DH

  integer(hid_t)                                 :: group_id
  integer(hid_t)                                 :: dset_id
  integer(hid_t)                                 :: dtype_id
  integer(hid_t)                                 :: dtstr_id
  integer(hid_t)                                 :: dtstrm_id
  integer(hid_t)                                 :: dtint1_id
  integer(hid_t)                                 :: dtint2_id
  integer(hid_t)                                 :: plist_id
  integer(size_t)                                :: type_size
  integer(size_t)                                :: type_sizes
  integer(size_t)                                :: type_sizei
  integer(size_t)                                :: offset
  character*256      ,dimension(MaxTabDims)       :: dim_name
  integer           ,dimension(:),allocatable    :: length
  integer           ,dimension(:),allocatable    :: unlimited
  integer(hid_t)                                 :: dspace_id
  integer(hsize_t)  ,dimension(1)                :: table_dims
  integer                                        :: table_rank
  integer(hsize_t) ,dimension(7)                 :: data_dims
  integer                                        :: i,j
  integer                                        :: hdf5err

  data_dims(1) = table_length
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
          ', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  call create_h5filetype(dtype_id,Status)
  if(Status /= WRF_NO_ERR) then
     return
  endif

  ! obtain group id
  group_id = DH%DimGroupID

  ! create data space
  table_rank    = 1
  table_dims(1) = table_length

  call h5screate_simple_f(table_rank,table_dims,dspace_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! obtain the data  
  allocate(length(table_length))
  allocate(unlimited(table_length))

  do i =1, table_length
     length(i)    = DH%DIMTABLE(i)%length
     unlimited(i) = DH%DIMTABLE(i)%unlimited
  enddo

  do i=1,table_length
     do j=1,256
        dim_name(i)(j:j)=DH%DIMTABLE(i)%dim_name(j:j)
     enddo
  enddo

  ! under dimensional group
  call h5dcreate_f(group_id,"h5dim_table",dtype_id,dspace_id,&
       dset_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATASET_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  ! create memory types
  call h5tget_size_f(H5T_NATIVE_INTEGER,type_sizei,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  ! FOR string, it needs extra handling
  call h5tcopy_f(H5T_NATIVE_CHARACTER,dtstr_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATATYPE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(length)
     deallocate(unlimited)
     return
  endif

  type_size = 256

     call h5tset_size_f(dtstr_id, type_size,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     call h5tget_size_f(dtstr_id, type_size,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     call h5tcreate_f(H5T_COMPOUND_F,type_size,dtstrm_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     offset = 0
     call h5tinsert_f(dtstrm_id,"dim_name",offset,dtstr_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     call h5tcreate_f(H5T_COMPOUND_F,type_sizei,dtint1_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     offset = 0
     call h5tinsert_f(dtint1_id,"dim_length",offset,H5T_NATIVE_INTEGER,&
          hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     call h5tcreate_f(H5T_COMPOUND_F,type_sizei,dtint2_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     offset = 0
     call h5tinsert_f(dtint2_id,"dim_unlimited",offset,H5T_NATIVE_INTEGER,&
          hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATATYPE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     ! write data by fields in the datatype,but first create a property list

     call h5pcreate_f(H5P_DATASET_XFER_F,plist_id, hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_PROPERTY_LIST
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     call h5pset_preserve_f(plist_id,.TRUE.,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_PROPERTY_LIST
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     call h5dwrite_f(dset_id,dtstrm_id,dim_name,data_dims,hdf5err,&
          xfer_prp = plist_id)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASET_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     call h5dwrite_f(dset_id,dtint1_id,length,data_dims,hdf5err,&
          xfer_prp = plist_id)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASET_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     call h5dwrite_f(dset_id,dtint2_id,unlimited,data_dims,hdf5err,&
          xfer_prp = plist_id)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASET_WRITE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        deallocate(length)
        deallocate(unlimited)
        return
     endif

     deallocate(length)
     deallocate(unlimited)

     ! release resources

     call h5tclose_f(dtstr_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_CLOSE_GENERAL
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5tclose_f(dtstrm_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_CLOSE_GENERAL
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5tclose_f(dtint1_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_CLOSE_GENERAL
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5tclose_f(dtint2_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_CLOSE_GENERAL
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5tclose_f(dtype_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_CLOSE_GENERAL
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5pclose_f(plist_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_CLOSE_GENERAL
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5dclose_f(dset_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_DATASET_CLOSE
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     call h5sclose_f(dspace_id,hdf5err)
     if(hdf5err.lt.0) then 
        Status =  WRF_HDF5_ERR_CLOSE_GENERAL
        write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg) 
        return
     endif

     return
end subroutine store_table


subroutine free_memory(DataHandle,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'wrf_status_codes.h'
  include 'mpif.h'

  integer              ,intent(in)       :: DataHandle
  integer              ,intent(out)      :: Status
  integer                                :: hdf5err
  type(wrf_phdf5_data_handle),pointer    :: DH
  integer                                :: i
  integer                                :: stat
  real*8                                 :: timeaw,timebw


  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  if(DH%Free) then
     Status = WRF_HDF5_ERR_OTHERS
     write(msg,*) '',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg)
     return
  endif

  deallocate(DH%Times, STAT=stat)
  if(stat/= 0) then
     Status = WRF_HDF5_ERR_DEALLOCATION
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  deallocate(DH%DimLengths, STAT=stat)
  if(stat/= 0) then
     Status = WRF_HDF5_ERR_DEALLOCATION
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  deallocate(DH%DimIDs, STAT=stat)
  if(stat/= 0) then
     Status = WRF_HDF5_ERR_DEALLOCATION
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  deallocate(DH%DimNames, STAT=stat)
  if(stat/= 0) then
     Status = WRF_HDF5_ERR_DEALLOCATION
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  deallocate(DH%DIMTABLE, STAT=stat)
  if(stat/= 0) then
     Status = WRF_HDF5_ERR_DEALLOCATION
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  deallocate(DH%MDDsetIDs, STAT=stat)
  if(stat/= 0) then
     Status = WRF_HDF5_ERR_DEALLOCATION
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  deallocate(DH%MDVarDimLens, STAT=stat)
  if(stat/= 0) then
     Status = WRF_HDF5_ERR_DEALLOCATION
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  deallocate(DH%MDVarNames, STAT=stat)
  if(stat/= 0) then
     Status = WRF_HDF5_ERR_DEALLOCATION
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  deallocate(DH%DsetIDs, STAT=stat)
  if(stat/= 0) then
     Status = WRF_HDF5_ERR_DEALLOCATION
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  deallocate(DH%VarDimLens, STAT=stat)
  if(stat/= 0) then
     Status = WRF_HDF5_ERR_DEALLOCATION
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  deallocate(DH%VarNames, STAT=stat)
  if(stat/= 0) then
     Status = WRF_HDF5_ERR_DEALLOCATION
     write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
     call wrf_debug ( FATAL , msg)
     return
  endif
  return
end subroutine free_memory

subroutine write_hdf5_attributes(DataHandle,MemoryOrder,WrfDType,DimRank,&
     NDim,dset_id,Status)

  use wrf_phdf5_data
  use ext_phdf5_support_routines
  use HDF5
  implicit none
  include 'mpif.h'
  include 'wrf_status_codes.h'


  integer                     ,intent(in)     :: DataHandle
  character*(*)               ,intent(in)     :: MemoryOrder	
  integer                     ,intent(in)     :: WrfDType
  integer,dimension(*)        ,intent(in)     :: DimRank

  integer                     ,intent(in)     :: NDim

  integer(hid_t)              ,intent(in)     :: dset_id
  integer                     ,intent(out)    :: Status

  character (3)                               :: Mem0
  character (3)                               :: UCMem0
  type(wrf_phdf5_data_handle) ,pointer        :: DH

  ! attribute defination
  integer(hid_t)                              :: dimaspace_id  ! DimRank dataspace id
  integer(hid_t)                              :: dimattr_id    ! DimRank attribute id
  integer(hsize_t) ,dimension(1)              :: dim_space

  integer(hid_t)                              :: h5_atypeid    ! for fieldtype,memorder attribute
  integer(hid_t)                              :: h5_aspaceid   ! for fieldtype,memorder  
  integer(hid_t)                              :: h5_attrid     ! for fieldtype,memorder
  integer(hsize_t), dimension(7)              :: adata_dims
  integer                                     :: routine_atype
  integer,          dimension(:),allocatable  :: dimrank_data
  integer                                     :: hdf5err
  integer                                     :: j

  !  For time function
  real*8                                     :: timebw
  real*8                                     :: timeaw
  integer                                    :: total_ele

  ! 
  ! write dimensional rank attribute. This is the temporary fix for dim. scale
  ! the first dimension is always time
  allocate(dimrank_data(NDim+1))
  do j =1, NDim+1
     dimrank_data(j)  = DimRank(j)
  enddo

  dim_space(1)  = NDim+1
  adata_dims(1) = NDim+1
  call h5screate_simple_f(1,dim_space,dimaspace_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_DATASPACE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dimrank_data)
     return
  endif

  call h5acreate_f(dset_id,'H5_DimRank',H5T_NATIVE_INTEGER,dimaspace_id, &
       dimattr_id,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dimrank_data)
     return
  endif

  call h5awrite_f(dimattr_id,H5T_NATIVE_INTEGER,dimrank_data,adata_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     deallocate(dimrank_data)
     return
  endif
  deallocate(dimrank_data)

  ! close space and attribute id
  call clean_phdf5_attrids(H5T_NATIVE_INTEGER,dimaspace_id,dimattr_id,0,Status)
  if(Status.ne.WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif
  ! Write memory order and FieldType attribute, both MemoryOrder and FieldType are 1 element
  adata_dims(1) = 1

  ! output memoryorder attribute
  call reorder(MemoryOrder,Mem0)
  call uppercase(Mem0,UCMem0)

  routine_atype = WRF_CHARACTER

  ! The size of memoryorder string is always MemOrdLen
  call create_phdf5_adtypeid(h5_atypeid,routine_atype,MemOrdLen,Status)
  if(Status.ne.WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! Count for string attribute is always 1 
  call create_phdf5_adspaceid(1,1,h5_aspaceid,Status)
  if(Status.ne.WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif
  call h5acreate_f(dset_id,'MemoryOrder',h5_atypeid,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5awrite_f(h5_attrid,h5_atypeid,UCMem0,adata_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif
  call clean_phdf5_attrids(h5_atypeid,h5_aspaceid,h5_attrid,1,Status)
  if(Status.ne.WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  ! output fieldtype attribute
  call create_phdf5_adspaceid(1,1,h5_aspaceid,Status)
  if(Status.ne.WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5acreate_f(dset_id,'FieldType',H5T_NATIVE_INTEGER,h5_aspaceid, &
       h5_attrid, hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_CREATE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

  call h5awrite_f(h5_attrid,H5T_NATIVE_INTEGER,WrfDType,adata_dims,hdf5err)
  if(hdf5err.lt.0) then 
     Status =  WRF_HDF5_ERR_ATTRIBUTE_WRITE
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif
  call clean_phdf5_attrids(H5T_NATIVE_INTEGER,h5_aspaceid,h5_attrid,0,Status)
  if(Status.ne.WRF_NO_ERR) then
     write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , msg) 
     return
  endif

end subroutine write_hdf5_attributes
