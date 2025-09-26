!------------------------------------------------------------------
!$Id$
!------------------------------------------------------------------

subroutine ext_pio_open_for_read(DatasetName, grid, SysDepInfo, DataHandle, Status)
  use wrf_data_pio
  use pio_routines
  use module_domain
  implicit none
  include 'wrf_status_codes.h'
  character *(*), INTENT(IN)   :: DatasetName
  TYPE(domain)                 :: grid
  character *(*), INTENT(IN)   :: SysDepInfo
  integer       , INTENT(OUT)  :: DataHandle
  integer       , INTENT(OUT)  :: Status
  DataHandle = 0   ! dummy setting to quiet warning message
  CALL ext_pio_open_for_read_begin( DatasetName, grid, SysDepInfo, DataHandle, Status )
  IF ( Status .EQ. WRF_NO_ERR ) THEN
    CALL ext_pio_open_for_read_commit( DataHandle, Status )
  ENDIF
  return
end subroutine ext_pio_open_for_read

!ends training phase; switches internal flag to enable input
!must be paired with call to ext_pio_open_for_read_begin
subroutine ext_pio_open_for_read_commit(DataHandle, Status)
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer, intent(in) :: DataHandle
  integer, intent(out) :: Status
  type(wrf_data_handle) ,pointer         :: DH

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED
    write(msg,*) 'ext_pio_ioinit was not called ',__FILE__,', line', __LINE__
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
  Status = WRF_NO_ERR
  return
end subroutine ext_pio_open_for_read_commit

subroutine upgrade_filename(FileName)
  implicit none

  character*(*), intent(inout) :: FileName
  integer :: i

  do i = 1, len(trim(FileName))
     if(FileName(i:i) == '-') then
        FileName(i:i) = '_'
     else if(FileName(i:i) == ':') then
        FileName(i:i) = '_'
     endif
  enddo

end subroutine upgrade_filename

subroutine ext_pio_open_for_read_begin( FileName, grid, SysDepInfo, DataHandle, Status)
  use wrf_data_pio
  use pio_routines
  use module_domain
  implicit none
  include 'wrf_status_codes.h'
  character*(*)         ,intent(INOUT)   :: FileName
  TYPE(domain)                           :: grid
  character*(*)         ,intent(in)      :: SysDepInfo
  integer               ,intent(out)     :: DataHandle
  integer               ,intent(out)     :: Status
  type(wrf_data_handle) ,pointer         :: DH
  integer                                :: XType
  integer                                :: stat
  integer                                :: StoredDim
  integer                                :: NAtts
  integer                                :: DimIDs(2)
  integer                                :: VStart(2)
  integer                                :: VLen(2)
  integer                                :: TotalNumVars
  integer                                :: NumVars
  integer                                :: i
  integer                                :: ndims, unlimitedDimID
  character(PIO_MAX_NAME)                :: Name

  call upgrade_filename(FileName)

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_pio_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif

  if(DH%first_operation) then
     call initialize_pio(grid, DH)
     call define_pio_iodesc(grid, DH)
     DH%first_operation = .false.
  end if

  stat = pio_openfile(DH%iosystem, DH%file_handle, pio_iotype_pnetcdf, FileName)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , TRIM(msg))
     return
  endif

  stat = pio_inq_varid(DH%file_handle, DH%TimesName, DH%vtime)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
     write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
     call wrf_debug ( WARN , TRIM(msg))
     return
  endif

  stat = pio_inquire_variable(DH%file_handle, DH%vtime, DH%TimesName, XType, StoredDim, DimIDs, NAtts)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(XType/=PIO_CHAR) then
    Status = WRF_WARN_TYPE_MISMATCH
    write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = pio_inq_dimlen(DH%file_handle, DimIDs(1), VLen(1))
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

  stat = pio_inq_dimlen(DH%file_handle, DimIDs(2), VLen(2))
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
  stat = pio_get_var(DH%file_handle, DH%vtime, DH%Times(1:VLen(2)))
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif

  stat = pio_inquire(DH%file_handle, ndims, TotalNumVars, NAtts, unlimitedDimID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif

  NumVars = 0
  do i=1,TotalNumVars
    stat = pio_inq_varname(DH%file_handle,i,Name)
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
  DH%FileName        = trim(FileName)
  DH%CurrentVariable = 0
  DH%CurrentTime     = 0
  DH%TimeIndex       = 0

  do i = 1, ndims
    DH%DimIDs(i) = i
    stat = pio_inq_dimname(DH%file_handle,i,DH%DimNames(i))
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif

    if(unlimitedDimID == i) then
       DH%DimUnlimID = unlimitedDimID
       DH%DimUnlimName = DH%DimNames(i)
    endif
  enddo
  DH%NumDims = ndims
  return
end subroutine ext_pio_open_for_read_begin

subroutine ext_pio_open_for_update( FileName, grid, SysDepInfo, DataHandle, Status)
  use wrf_data_pio
  use pio_routines
  use module_domain
  implicit none
  include 'wrf_status_codes.h'
  character*(*)         ,intent(INOUT)   :: FileName
  TYPE(domain)                           :: grid
  character*(*)         ,intent(in)      :: SysDepInfo
  integer               ,intent(out)     :: DataHandle
  integer               ,intent(out)     :: Status
  type(wrf_data_handle) ,pointer         :: DH
  integer                                :: XType
  integer                                :: stat
  integer                                :: StoredDim
  integer                                :: NAtts
  integer                                :: DimIDs(2)
  integer                                :: VStart(2)
  integer                                :: VLen(2)
  integer                                :: TotalNumVars
  integer                                :: NumVars
  integer                                :: i
  integer                                :: ndims, unlimitedDimID
  character(PIO_MAX_NAME)                :: Name

  call upgrade_filename(FileName)

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_pio_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif

  if(DH%first_operation) then
     call initialize_pio(grid, DH)
     call define_pio_iodesc(grid, DH)
     DH%first_operation = .false.
  end if

  stat = pio_openfile(DH%iosystem, DH%file_handle, pio_iotype_pnetcdf, FileName)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = pio_inq_varid(DH%file_handle, DH%TimesName, DH%vtime)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = pio_inquire_variable(DH%file_handle, DH%vtime, DH%TimesName, &
                              XType, StoredDim, DimIDs, NAtts)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(XType/=PIO_CHAR) then
    Status = WRF_WARN_TYPE_MISMATCH
    write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = pio_inq_dimlen(DH%file_handle, DimIDs(1), VLen(1))
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
  stat = pio_inq_dimlen(DH%file_handle, DimIDs(2), VLen(2))
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
 !stat = pio_get_var(DH%file_handle, DH%vtime, VStart, VLen, DH%Times)
  stat = pio_get_var(DH%file_handle, DH%vtime, DH%Times)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  stat = pio_inquire(DH%file_handle, ndims, TotalNumVars, NAtts, unlimitedDimID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  NumVars = 0
  do i=1,TotalNumVars
    stat = pio_inq_varname(DH%file_handle, i, Name)
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
  DH%FileName        = trim(FileName)
  DH%CurrentVariable = 0
  DH%CurrentTime     = 0
  DH%TimeIndex       = 0
  return
end subroutine ext_pio_open_for_update


SUBROUTINE ext_pio_open_for_write_begin(FileName,grid,SysDepInfo,DataHandle,Status)
  use pio_types
  use pio
  use wrf_data_pio
  use pio_routines
  use module_domain
  implicit none
  include 'wrf_status_codes.h'
  character*(*)        ,intent(inout) :: FileName
  TYPE(domain)                      :: grid
  character*(*)        ,intent(in)  :: SysDepInfo
  integer              ,intent(out) :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: i
  integer                           :: stat
  character (7)                     :: Buffer
  integer                           :: VDimIDs(2)
  integer                           :: info, ierr   ! added for Blue Gene (see PIO_CREAT below)
  character*128                     :: idstr,ntasks_x_str,loccomm_str
  integer                           :: gridid
  integer local_communicator_x, ntasks_x

  call upgrade_filename(FileName)

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_pio_open_for_write_begin: ext_pio_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call allocHandle(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Fatal ALLOCATION ERROR in ext_pio_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  DH%TimeIndex = 0
  DH%Times     = ZeroDate

  if(DH%first_operation) then
     call initialize_pio(grid, DH)
     call define_pio_iodesc(grid, DH)
     DH%first_operation = .false.
  end if

 !call mpi_info_create( info, ierr )
  stat = pio_CreateFile(DH%iosystem, DH%file_handle, &
                        pio_iotype_pnetcdf, FileName, PIO_64BIT_OFFSET)
 !call mpi_info_free( info, ierr)

  call netcdf_err(stat,Status)

  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_pio_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif

 !JPE added for performance
 !stat = nf90_set_fill(DH%file_handle, NF90_NOFILL, i)

  DH%FileStatus  = WRF_FILE_OPENED_NOT_COMMITTED
  DH%FileName    = trim(FileName)
  stat = pio_def_dim(DH%file_handle, DH%DimUnlimName, PIO_UNLIMITED, DH%DimUnlimID)
 !stat = pio_def_dim(DH%file_handle, DH%DimUnlimName, 1, DH%DimUnlimID)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_pio_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif

  DH%VarNames  (1:MaxVars) = NO_NAME
  do i=1,MaxDims
    write(Buffer,FMT="('DIM',i4.4)") i
    DH%DimNames  (i) = Buffer
    DH%DimLengths(i) = NO_DIM
  enddo

  DH%DimNames(1) = 'DateStrLen'
  stat = pio_def_dim(DH%file_handle, DH%DimNames(1), DateStrLen, DH%DimIDs(1))
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_pio_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif

  VDimIDs(1) = DH%DimIDs(1)
  VDimIDs(2) = DH%DimUnlimID
  stat = pio_def_var(DH%file_handle,DH%TimesName,PIO_CHAR,VDimIDs,DH%vtime)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_pio_open_for_write_begin ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%DimLengths(1) = DateStrLen

  return
end subroutine ext_pio_open_for_write_begin

!opens a file for writing or coupler datastream for sending messages.
!no training phase for this version of the open stmt.
subroutine ext_pio_open_for_write (DatasetName, grid, &
                                   SysDepInfo, DataHandle, Status)
  use wrf_data_pio
  use pio_routines
  use module_domain
  implicit none
  include 'wrf_status_codes.h'
  character *(*), intent(in)  :: DatasetName
  type(domain)                :: grid
  character *(*), intent(in)  :: SysDepInfo
  integer       , intent(out) :: DataHandle
  integer       , intent(out) :: Status
  Status=WRF_WARN_NOOP
  DataHandle = 0    ! dummy setting to quiet warning message
  return
end subroutine ext_pio_open_for_write

SUBROUTINE ext_pio_open_for_write_commit(DataHandle, Status)
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: i
  integer                           :: stat

  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_pio_open_for_write_commit: ext_pio_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_pio_open_for_write_commit ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg)) 
    return
  endif
  DH%Write = .true.
  stat = pio_enddef(DH%file_handle)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error (',stat,') from pio_enddef in ext_pio_open_for_write_commit ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  DH%FileStatus = WRF_FILE_OPENED_FOR_WRITE

  return
end subroutine ext_pio_open_for_write_commit

subroutine ext_pio_ioclose(DataHandle, Status)
  use wrf_data_pio
  use pio_routines
  use pio
  use pio_kinds
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: stat

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_pio_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ext_pio_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_CLOSE
    write(msg,*) 'Warning TRY TO CLOSE DRYRUN in ext_pio_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue    
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    continue
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ext_pio_ioclose ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif

  call pio_closefile(DH%file_handle)
  CALL deallocHandle( DataHandle, Status )
  DH%Free=.true.
  return
end subroutine ext_pio_ioclose

subroutine ext_pio_iosync( DataHandle, Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(in)  :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: stat

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_pio_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ext_pio_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_FILE_NOT_COMMITTED
    write(msg,*) 'Warning FILE NOT COMMITTED in ext_pio_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    continue
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    continue
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ext_pio_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , TRIM(msg))
    return
  endif
  call pio_syncfile(DH%file_handle)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ext_pio_iosync ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  return
end subroutine ext_pio_iosync

subroutine ext_pio_ioinit(SysDepInfo, Status)
  use wrf_data_pio
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
end subroutine ext_pio_ioinit

subroutine ext_pio_inquiry (Inquiry, Result, Status)
  use wrf_data_pio
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
end subroutine ext_pio_inquiry

subroutine ext_pio_ioexit(Status)
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer       , INTENT(INOUT)     ::Status
  integer                           :: error
  type(wrf_data_handle),pointer     :: DH
  integer                           :: i
  integer                           :: stat
  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED 
    write(msg,*) 'ext_pio_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  do i=1,WrfDataHandleMax
    CALL deallocHandle( i , stat ) 
  enddo
  return
end subroutine ext_pio_ioexit

subroutine ext_pio_get_dom_ti_real_arr(DataHandle,Element,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines

  implicit none

  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real,                  intent(out)    :: Data(:)
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCOunt
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XType
  integer                               :: Len
  integer                               :: stat
  real,                  allocatable    :: Buffer(:)

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg) 
    return
  endif

! Do nothing unless it is time to read time-independent domain metadata.  
  IF(.not. ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED   
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ   
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE   
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_att(DH%file_handle, PIO_GLOBAL, Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if( XType/=PIO_REAL) then
      Status = WRF_WARN_TYPE_MISMATCH  
      write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1  
      write(msg,*) 'Warning LENGTH < 1 in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR  
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att (DH%file_handle,PIO_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_dom_ti_real_arr

subroutine ext_pio_get_dom_ti_real_sca(DataHandle,Element,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines

  implicit none

  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real,                  intent(out)    :: Data
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCOunt
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XType
  integer                               :: Len
  integer                               :: stat
  real,                  allocatable    :: Buffer(:)

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg) 
    return
  endif

! Do nothing unless it is time to read time-independent domain metadata.  
  IF(.not. ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED   
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ   
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE   
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_att(DH%file_handle, PIO_GLOBAL, Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if( XType/=PIO_REAL) then
      Status = WRF_WARN_TYPE_MISMATCH  
      write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1  
      write(msg,*) 'Warning LENGTH < 1 in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR  
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att (DH%file_handle,PIO_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_dom_ti_real_sca

subroutine ext_pio_get_dom_ti_integer_arr(DataHandle,Element,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines

  implicit none

  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  integer,               intent(out)    :: Data(:)
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCOunt
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XType
  integer                               :: Len
  integer                               :: stat
  integer,               allocatable    :: Buffer(:)

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg) 
    return
  endif

! Do nothing unless it is time to read time-independent domain metadata.  
  IF(.not. ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED   
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ   
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE   
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_att(DH%file_handle, PIO_GLOBAL, Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if( XType/=PIO_INT) then
      Status = WRF_WARN_TYPE_MISMATCH  
      write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1  
      write(msg,*) 'Warning LENGTH < 1 in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR  
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att (DH%file_handle,PIO_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*)  'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_dom_ti_integer_arr

subroutine ext_pio_get_dom_ti_integer_sca(DataHandle,Element,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines

  implicit none

  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  integer,               intent(out)    :: Data
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCOunt
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XType
  integer                               :: Len
  integer                               :: stat
  integer,               allocatable    :: Buffer(:)

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg) 
    return
  endif

! Do nothing unless it is time to read time-independent domain metadata.  
  IF(.not. ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED   
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ   
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE   
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_att(DH%file_handle, PIO_GLOBAL, Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if( XType/=PIO_INT) then
      Status = WRF_WARN_TYPE_MISMATCH  
      write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1  
      write(msg,*) 'Warning LENGTH < 1 in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR  
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att (DH%file_handle,PIO_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*)  'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_dom_ti_integer_sca

subroutine ext_pio_get_dom_ti_double_arr(DataHandle,Element,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines

  implicit none

  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real*8,                intent(out)    :: Data(:)
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCOunt
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XType
  integer                               :: Len
  integer                               :: stat
  real*8,                allocatable    :: Buffer(:)

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg) 
    return
  endif

! Do nothing unless it is time to read time-independent domain metadata.  
  IF(.not. ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
      return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED   
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ   
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE   
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_att(DH%file_handle, PIO_GLOBAL, Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
      Status = WRF_WARN_TYPE_MISMATCH   
      write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1  
      write(msg,*) 'Warning LENGTH < 1 in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR  
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att (DH%file_handle,PIO_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_dom_ti_double_arr

subroutine ext_pio_get_dom_ti_double_sca(DataHandle,Element,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines

  implicit none

  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real*8,                intent(out)    :: Data
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCOunt
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XType
  integer                               :: Len
  integer                               :: stat
  real*8,                allocatable    :: Buffer(:)

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg) 
    return
  endif

! Do nothing unless it is time to read time-independent domain metadata.  
  IF(.not. ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED   
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ   
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE   
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_att(DH%file_handle, PIO_GLOBAL, Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
      Status = WRF_WARN_TYPE_MISMATCH   
      write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1  
      write(msg,*) 'Warning LENGTH < 1 in ',__FILE__,', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR  
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att (DH%file_handle,PIO_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_dom_ti_double_sca

subroutine ext_pio_get_dom_ti_logical_arr(DataHandle,Element,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines

  implicit none

  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  logical,               intent(out)    :: Data(:)
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCOunt
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XType
  integer                               :: Len
  integer                               :: stat
  integer,               allocatable    :: Buffer(:)

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg) 
    return
  endif

! Do nothing unless it is time to read time-independent domain metadata.  
  IF(.not. ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
      return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED   
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ   
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE   
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_att(DH%file_handle, PIO_GLOBAL, Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( PIO_INT == PIO_DOUBLE .OR. PIO_INT == PIO_REAL ) then
      if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH   
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ', trim(Element)
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if( XType/=PIO_INT) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ', trim(Element)
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1  
      write(msg,*) 'Warning LENGTH < 1 in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR  
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att (DH%file_handle,PIO_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len,Count)) = Buffer(1:min(Len,Count))==1
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_dom_ti_logical_arr

subroutine ext_pio_get_dom_ti_logical_sca(DataHandle,Element,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines

  implicit none

  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  logical,               intent(out)    :: Data
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCOunt
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XType
  integer                               :: Len
  integer                               :: stat
  integer,               allocatable    :: Buffer(:)

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg) 
    return
  endif

! Do nothing unless it is time to read time-independent domain metadata.  
  IF(.not. ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
      return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED   
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ   
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE   
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_att(DH%file_handle, PIO_GLOBAL, Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if ( PIO_INT == PIO_DOUBLE .OR. PIO_INT == PIO_REAL ) then
      if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH   
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ', trim(Element)
        call wrf_debug ( WARN , msg)
        return
      endif
    else
      if( XType/=PIO_INT) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ', trim(Element)
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1  
      write(msg,*) 'Warning LENGTH < 1 in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    allocate(Buffer(Len), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR  
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att (DH%file_handle,PIO_GLOBAL,Element,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= WRF_NO_ERR) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len
      Status = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_dom_ti_logical_sca

subroutine ext_pio_get_dom_ti_char_arr(DataHandle,Element,Data,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines

  implicit none

  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*),         intent(out)    :: Data
  
  
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XType
  integer                               :: Len
  integer                               :: stat
  

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg) 
    return
  endif

  ! Do nothing unless it is time to read time-independent domain metadata.  
  IF(.not. ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED   
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ   
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','CHAR',', line', __LINE__
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE   
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_att(DH%file_handle, PIO_GLOBAL, Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1  
      write(msg,*) 'Warning LENGTH < 1 in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = ''
    stat = pio_get_att(DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif

  return
end subroutine ext_pio_get_dom_ti_char_arr

subroutine ext_pio_get_dom_ti_char_sca(DataHandle,Element,Data,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines

  implicit none

  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*),         intent(out)    :: Data
  
  
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XType
  integer                               :: Len
  integer                               :: stat
  

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg) 
    return
  endif

  ! Do nothing unless it is time to read time-independent domain metadata.  
  IF(.not. ncd_ok_to_get_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED   
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ   
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','CHAR',', line', __LINE__
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE   
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_att(DH%file_handle, PIO_GLOBAL, Element, XType, Len)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    if(Len<=0) then
      Status = WRF_WARN_LENGTH_LESS_THAN_1  
      write(msg,*) 'Warning LENGTH < 1 in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ', trim(Element)
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = ''
    stat = pio_get_att(DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif

  return
end subroutine ext_pio_get_dom_ti_char_sca

subroutine ext_pio_put_dom_ti_real_arr(DataHandle,Element,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real,                  intent(in)     :: Data(*)
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat
  integer                               :: i
  real, dimension(1:Count)              :: tmparr

  tmparr(1:Count) = Data(1:Count)

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif

! Do nothing unless it is time to write time-independent domain metadata.  
  IF(.not. ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
   !stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,tmparr)
    if(1 == Count) then
       stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,tmparr(1))
    else
       stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,tmparr(1:Count))
    endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    DH%Write = .false.
    stat = pio_redef(DH%file_handle)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif

    if(1 == Count) then
       stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,tmparr(1))
    else
       stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,tmparr(1:Count))
    endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_put_dom_ti_real_arr

subroutine ext_pio_put_dom_ti_real_sca(DataHandle,Element,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real,                  intent(in)     :: Data
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat
  integer                               :: i

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif

! Do nothing unless it is time to write time-independent domain metadata.  
  IF(.not. ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
      stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    DH%Write = .false.
    stat = pio_redef(DH%file_handle)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_put_dom_ti_real_sca

subroutine ext_pio_put_dom_ti_integer_arr(DataHandle,Element,Data,Count,Status)
  use pio_kinds
  use pio

  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  integer,               intent(in)     :: Data(*)
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat
  integer                               :: i
  integer, dimension(Count)             :: tmparr

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif

  tmparr(1:Count) = Data(1:Count)

!-Do nothing unless it is time to write time-independent domain metadata.  
  IF(.not. ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    stat = pio_put_att (DH%file_handle,PIO_GLOBAL,Element,tmparr)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    DH%Write = .false.
    stat = pio_redef(DH%file_handle)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    tmparr(1:Count) = Data(1:Count)
    if(1 == Count) then
       stat = pio_put_att (DH%file_handle,PIO_GLOBAL,Element,tmparr(1))
    else
       stat = pio_put_att (DH%file_handle,PIO_GLOBAL,Element,tmparr(1:Count))
    endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_put_dom_ti_integer_arr

subroutine ext_pio_put_dom_ti_integer_sca(DataHandle,Element,Data,Count,Status)
  use pio_kinds
  use pio

  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  integer,               intent(in)     :: Data
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat
  integer                               :: i

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif

! Do nothing unless it is time to write time-independent domain metadata.  
  IF(.not. ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
      stat = pio_put_att (DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    if(DH%Write) then
      DH%Write = .false.
      stat = pio_redef(DH%file_handle)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    stat = pio_put_att (DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_put_dom_ti_integer_sca

subroutine ext_pio_put_dom_ti_double_arr(DataHandle,Element,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real*8,                intent(in)     :: Data(:)
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat
  integer                               :: i
  real*8, dimension(1:Count)            :: tmparr

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  tmparr(1:Count) = Data(1:Count)

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif

! Do nothing unless it is time to write time-independent domain metadata.  
  IF(.not. ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    stat = pio_put_att (DH%file_handle,PIO_GLOBAL,Element,tmparr(1:Count))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    if(DH%Write) then
      DH%Write = .false.
      stat = pio_redef(DH%file_handle)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
        call wrf_debug ( WARN , msg)
        return
      endif
    endif

    stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,tmparr(1:Count))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
#endif
  return
end subroutine ext_pio_put_dom_ti_double_arr

subroutine ext_pio_put_dom_ti_double_sca(DataHandle,Element,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  real*8,                intent(in)     :: Data
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat
  integer                               :: i

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif

! Do nothing unless it is time to write time-independent domain metadata.  
  IF(.not. ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    stat = pio_put_att (DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    if(DH%Write) then
      DH%Write = .false.
      stat = pio_redef(DH%file_handle)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
        call wrf_debug ( WARN , msg)
        return
      endif
    endif

    stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
#endif
  return
end subroutine ext_pio_put_dom_ti_double_sca

subroutine ext_pio_put_dom_ti_logical_arr(DataHandle,Element,Data,Count,Status)
  use pio
  use pio_kinds

  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  logical,               intent(in)     :: Data(:)
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat
  integer               ,allocatable    :: Buffer(:)
  integer                               :: i

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif

! Do nothing unless it is time to write time-independent domain metadata.  
  IF(.not. ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
      allocate(Buffer(Count), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      do i=1,Count
        if(data(i)) then
           Buffer(i)=1
        else
           Buffer(i)=0
        endif
      enddo
      stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,Buffer)
      deallocate(Buffer, STAT=stat)
      if(stat /= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    if(DH%Write) then
      DH%Write = .false.
      stat = pio_redef(DH%file_handle)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
        call wrf_debug ( WARN , msg)
        return
      endif
    endif

    allocate(Buffer(Count), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    do i=1,Count
      if(data(i)) then
         Buffer(i)=1
      else
         Buffer(i)=0
      endif
    enddo
    stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,Buffer)
    deallocate(Buffer, STAT=stat)
    if(stat /= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
#endif
  return
end subroutine ext_pio_put_dom_ti_logical_arr

subroutine ext_pio_put_dom_ti_logical_sca(DataHandle,Element,Data,Count,Status)
  use pio
  use pio_kinds

  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  logical,               intent(in)     :: Data
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat
  integer               ,allocatable    :: Buffer(:)
  integer                               :: i

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif

! Do nothing unless it is time to write time-independent domain metadata.  
  IF(.not. ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    allocate(Buffer(Count), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(data) then
       Buffer(1)=1
    else
       Buffer(1)=0
    endif
    stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,Buffer)
    deallocate(Buffer, STAT=stat)
    if(stat /= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    if(DH%Write) then
      DH%Write = .false.
      stat = pio_redef(DH%file_handle)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    allocate(Buffer(Count), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(data) then
       Buffer(1)=1
    else
       Buffer(1)=0
    endif
    stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,Buffer)
    deallocate(Buffer, STAT=stat)
    if(stat /= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
#endif
  return
end subroutine ext_pio_put_dom_ti_logical_sca

subroutine ext_pio_put_dom_ti_char_arr(DataHandle,Element,Data,Status)
  use pio
  use pio_kinds

  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*),         intent(in)     :: Data
  integer,               parameter      :: Count=1
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat
  integer                               :: i

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif

! Do nothing unless it is time to write time-independent domain metadata.  
  IF(.not. ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
      stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    if(DH%Write) then
      DH%Write = .false.
      stat = pio_redef(DH%file_handle)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_put_dom_ti_char_arr

subroutine ext_pio_put_dom_ti_char_sca(DataHandle,Element,Data,Status)
  use pio
  use pio_kinds

  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*),         intent(in)     :: Data
  integer,               parameter      :: Count=1
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat
  integer                               :: i

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif

! Do nothing unless it is time to write time-independent domain metadata.  
  IF(.not. ncd_ok_to_put_dom_ti( DataHandle ) ) THEN
     return
  ENDIF

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    STATUS = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
      stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif (DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    if(DH%Write) then
      DH%Write = .false.
      stat = pio_redef(DH%file_handle)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
        call wrf_debug ( WARN , msg)
        return
      endif
    endif
    stat = pio_put_att(DH%file_handle,PIO_GLOBAL,Element,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
#endif
  return
end subroutine ext_pio_put_dom_ti_char_sca

subroutine ext_pio_put_var_ti_real_arr(DataHandle,Element,Var,Data,Count,Status)
  use pio
  use pio_kinds

  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  real,                  intent(in)     :: Data(:)
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: i
  integer                               :: NVar
  character*1                           :: null

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN  
    write(msg,*) 'Warning WRITE METADATA AFTER OPEN in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF 
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS 
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_ti_real_arr

subroutine ext_pio_put_var_ti_real_sca(DataHandle,Element,Var,Data,Count,Status)
  use pio
  use pio_kinds

  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  real,                  intent(in)     :: Data
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: i
  integer                               :: NVar
  character*1                           :: null

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN  
    write(msg,*) 'Warning WRITE METADATA AFTER OPEN in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF 
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS 
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_ti_real_sca

subroutine ext_pio_put_var_td_real_arr(DataHandle,Element,DateStr,Var,Data,Count,Status)
  use pio
  use pio_kinds

  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  real                  ,intent(in)     :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  integer                               :: stat
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      return
    endif
    do i=1,MaxVars
      if(DH%VarNames(i) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE  
        NVar=i
        return
      elseif(DH%VarNames(i) == NO_NAME) then
        DH%VarNames(i) = Name
        exit
      elseif(i == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = pio_def_dim(DH%file_handle,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS  
        write(msg,*) 'Warning TOO MANY DIMENSIONS in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%VarDimLens(1, NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') '2 Define Var <', trim(Var), '> as NVar:', NVar
    stat = pio_def_var(DH%file_handle,Name,PIO_REAL,DH%descVar(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do i=1,MaxVars
      if(DH%VarNames(i) == Name) then
        NVar=i
        exit
      elseif(DH%VarNames(i) == NO_NAME) then
        Status = WRF_WARN_MD_NF  
        write(msg,*) 'Warning METADATA NOT FOUND in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      elseif(i == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%VarDimLens(1,NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG 
      write(msg,*) 'Warning COUNT TOO LONG in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      write(msg,*) 'Warning ZERO LENGTH PUT in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),VStart,VCount,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_td_real_arr

subroutine ext_pio_put_var_td_real_sca(DataHandle,Element,DateStr,Var,Data,Count,Status)
  use pio
  use pio_kinds

  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  real                  ,intent(in)     :: Data
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  integer                               :: stat
  integer                               :: Buffer(1)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      return
    endif
    do i=1,MaxVars
      if(DH%VarNames(i) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE  
        NVar=i
        return
      elseif(DH%VarNames(i) == NO_NAME) then
        DH%VarNames(i) = Name
        exit
      elseif(i == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = pio_def_dim(DH%file_handle,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS  
        write(msg,*) 'Warning TOO MANY DIMENSIONS in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%VarDimLens(1,NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') '3 Define Var <', trim(Var), '> as NVar:', NVar
    stat = pio_def_var(DH%file_handle,Name,PIO_REAL,DH%descVar(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        exit
      elseif(DH%VarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF  
        write(msg,*) 'Warning METADATA NOT FOUND in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%VarDimLens(1,NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG 
      write(msg,*) 'Warning COUNT TOO LONG in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      write(msg,*) 'Warning ZERO LENGTH PUT in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
    Buffer(1) = Data
   !stat = pio_put_var(DH%file_handle,DH%descMDVar(NVar),VStart,VCount,Buffer)
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_td_real_sca

subroutine ext_pio_put_var_ti_double_arr(DataHandle,Element,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  real*8                ,intent(in)     :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: i
  integer                               :: NVar
  character*1                           :: null

  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN  
    write(msg,*) 'Warning WRITE METADATA AFTER OPEN in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF 
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),Data )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error for Var ',TRIM(Var),&
                   ' Element ',trim(Element),' in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS 
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_put_var_ti_double_arr

subroutine ext_pio_put_var_ti_double_sca(DataHandle,Element,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  real*8                ,intent(in)     :: Data
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  integer                               :: stat
  real*8                                :: Buffer(1)
  integer                               :: i
  integer                               :: NVar
  character*1                           :: null

  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN  
    write(msg,*) 'Warning WRITE METADATA AFTER OPEN in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF 
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    Buffer(1) = Data
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error for Var ',TRIM(Var),&
                   ' Element ',trim(Element),' in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS 
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_put_var_ti_double_sca

subroutine ext_pio_put_var_td_double_arr(DataHandle,Element,DateStr,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  real*8                ,intent(in)     :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  integer                               :: stat
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      return
    endif
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE  
        return
      elseif(DH%VarNames(NVar) == NO_NAME) then
        DH%VarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = pio_def_dim(DH%file_handle,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS  
        write(msg,*) 'Warning TOO MANY DIMENSIONS in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%VarDimLens(1,NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') '4 Define Var <', trim(Var), '> as NVvar:', NVar
    stat = pio_def_var(DH%file_handle,Name,PIO_DOUBLE,DH%descVar(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        exit
      elseif(DH%VarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF  
        write(msg,*) 'Warning METADATA NOT FOUND in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%VarDimLens(1,NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG 
      write(msg,*) 'Warning COUNT TOO LONG in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      write(msg,*) 'Warning ZERO LENGTH PUT in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
   !stat = pio_put_var(DH%file_handle,DH%descMDVar(NVar),VStart,VCount,Data)
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),VStart,VCount,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_td_double_arr

subroutine ext_pio_put_var_td_double_sca(DataHandle,Element,DateStr,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  real*8                ,intent(in)     :: Data
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  integer                               :: stat
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  real*8                                :: Buffer(1)

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      return
    endif
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE  
        return
      elseif(DH%VarNames(NVar) == NO_NAME) then
        DH%VarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = pio_def_dim(DH%file_handle,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS  
        write(msg,*) 'Warning TOO MANY DIMENSIONS in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%VarDimLens(1,NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') '5 Define Var <', trim(Var), '> as NVar:', NVar
    stat = pio_def_var(DH%file_handle,Name,PIO_DOUBLE,DH%descVar(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        exit
      elseif(DH%VarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF  
        write(msg,*) 'Warning METADATA NOT FOUND in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%VarDimLens(1,NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG 
      write(msg,*) 'Warning COUNT TOO LONG in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      write(msg,*) 'Warning ZERO LENGTH PUT in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
    Buffer(1) = Data
   !stat = pio_put_var(DH%file_handle,DH%descMDVar(NVar),VStart,VCount,Buffer)
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_td_double_sca

subroutine ext_pio_put_var_ti_integer_arr(DataHandle,Element,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  integer,               intent(in)     :: Data(:)
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: i
  integer                               :: NVar
  character*1                           :: null

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN  
    write(msg,*) 'Warning WRITE METADATA AFTER OPEN in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF 
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS 
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_ti_integer_arr

subroutine ext_pio_put_var_ti_integer_sca(DataHandle,Element,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  integer,               intent(in)     :: Data
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: Buffer(1)
  integer                               :: i
  integer                               :: NVar
  character*1                           :: null

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN  
    write(msg,*) 'Warning WRITE METADATA AFTER OPEN in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF 
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    Buffer(1) = Data
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS 
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_ti_integer_sca

subroutine ext_pio_put_var_td_integer_arr(DataHandle,Element,DateStr,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  integer,               intent(in)     :: Data(:)
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  integer                               :: stat
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      return
    endif
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE  
        return
      elseif(DH%VarNames(NVar) == NO_NAME) then
        DH%VarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = pio_def_dim(DH%file_handle,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS  
        write(msg,*) 'Warning TOO MANY DIMENSIONS in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%VarDimLens(1,NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') '6 Define Var <', trim(Var), '> as NVar:', NVar
    stat = pio_def_var(DH%file_handle,Name,PIO_INT,DH%descVar(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        exit
      elseif(DH%VarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF  
        write(msg,*) 'Warning METADATA NOT FOUND in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%VarDimLens(1,NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG 
      write(msg,*) 'Warning COUNT TOO LONG in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      write(msg,*) 'Warning ZERO LENGTH PUT in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
   !stat = pio_put_var(DH%file_handle,DH%MDVarIDs(NVar),VStart,VCount,Data)
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),VStart,VCount,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_td_integer_arr

subroutine ext_pio_put_var_td_integer_sca(DataHandle,Element,DateStr,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  integer,               intent(in)     :: Data
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  integer                               :: stat
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  integer                               :: Buffer(1)

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      return
    endif
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE  
        return
      elseif(DH%VarNames(NVar) == NO_NAME) then
        DH%VarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = pio_def_dim(DH%file_handle,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS  
        write(msg,*) 'Warning TOO MANY DIMENSIONS in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%VarDimLens(1,NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') '7 Define Var <', trim(Var), '> as NVar:', NVar
    stat = pio_def_var(DH%file_handle,Name,PIO_INT,DH%descVar(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        exit
      elseif(DH%VarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF  
        write(msg,*) 'Warning METADATA NOT FOUND in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%VarDimLens(1,NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG 
      write(msg,*) 'Warning COUNT TOO LONG in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      write(msg,*) 'Warning ZERO LENGTH PUT in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
    Buffer(1) = Data
   !stat = pio_put_var(DH%file_handle,DH%MDVarIDs(NVar),VStart,VCount,Buffer)
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_td_integer_sca

subroutine ext_pio_put_var_ti_logical_arr(DataHandle,Element,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  logical,               intent(in)     :: Data(:)
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer               ,allocatable    :: Buffer(:)
  integer                               :: i
  integer                               :: NVar
  character*1                           :: null

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN  
    write(msg,*) 'Warning WRITE METADATA AFTER OPEN in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF 
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,' ','LOGICAL',', line', __LINE__ &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    allocate(Buffer(Count), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    do i=1,Count
      if(data(i)) then
         Buffer(i)=1
      else
         Buffer(i)=0
      endif
    enddo
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
    endif
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS 
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_ti_logical_arr

subroutine ext_pio_put_var_ti_logical_sca(DataHandle,Element,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  logical,               intent(in)     :: Data
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: Buffer(1)
  integer                               :: i
  integer                               :: NVar
  character*1                           :: null

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN  
    write(msg,*) 'Warning WRITE METADATA AFTER OPEN in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF 
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,' ','LOGICAL',', line', __LINE__ &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Data) then
       Buffer(1)=1
    else
       Buffer(1)=0
    endif
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
    endif
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS 
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_ti_logical_sca

subroutine ext_pio_put_var_td_logical_arr(DataHandle,Element,DateStr,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  logical,               intent(in)     :: Data(:)
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  integer                               :: stat
  integer               ,allocatable    :: Buffer(:)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      return
    endif
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE  
        return
      elseif(DH%VarNames(NVar) == NO_NAME) then
        DH%VarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = pio_def_dim(DH%file_handle,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS  
        write(msg,*) 'Warning TOO MANY DIMENSIONS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%VarDimLens(1,NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') '8 Define Var <', trim(Var), '> as NVar:', NVar
    stat = pio_def_var(DH%file_handle,Name,PIO_INT,DH%descVar(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        exit
      elseif(DH%VarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF  
        write(msg,*) 'Warning METADATA NOT FOUND in ',__FILE__,' ','LOGICAL',', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%VarDimLens(1,NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG 
      write(msg,*) 'Warning COUNT TOO LONG in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      write(msg,*) 'Warning ZERO LENGTH PUT in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*)  'Warning in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
      allocate(Buffer(Count), STAT=stat)
      if(stat/= 0) then
        Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
        write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
      do i=1,Count
        if(data(i)) then
           Buffer(i)=1
        else
           Buffer(i)=0
        endif
      enddo
     !stat = pio_put_var(DH%file_handle,DH%MDVarIDs(NVar),VStart,VCount,Buffer)
      stat = pio_put_var(DH%file_handle,DH%descVar(NVar),VStart,VCount,Buffer)
      deallocate(Buffer, STAT=stat)
      if(stat /= 0) then
        Status = WRF_ERR_FATAL_DEALLOCATION_ERR 
        write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
        call wrf_debug ( FATAL , msg)
        return
      endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_td_logical_arr

subroutine ext_pio_put_var_td_logical_sca(DataHandle,Element,DateStr,Var,Data,Count,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  logical,               intent(in)     :: Data
  integer,               intent(in)     :: Count
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  integer                               :: stat
  integer                               :: Buffer(1)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      return
    endif
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE  
        return
      elseif(DH%VarNames(NVar) == NO_NAME) then
        DH%VarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == Count) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = pio_def_dim(DH%file_handle,DH%DimNames(i),Count,DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = Count
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS  
        write(msg,*) 'Warning TOO MANY DIMENSIONS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%VarDimLens(1,NVar) = Count
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') '9 Define Var <', trim(Var), '> as NVar:', NVar
    stat = pio_def_var(DH%file_handle,Name,PIO_INT,DH%descVar(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        exit
      elseif(DH%VarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF  
        write(msg,*) 'Warning METADATA NOT FOUND in ',__FILE__,' ','LOGICAL',', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(Count > DH%VarDimLens(1,NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG 
      write(msg,*) 'Warning COUNT TOO LONG in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    elseif(Count < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      write(msg,*) 'Warning ZERO LENGTH PUT in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*)  'Warning in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Count
    VCount(2) = 1
    if(Data) then
       Buffer(1)=1
    else
       Buffer(1)=0
    endif
   !stat = pio_put_var(DH%file_handle,DH%MDVarIDs(NVar),VStart,VCount,Buffer)
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_td_logical_sca

subroutine ext_pio_put_var_ti_char_arr(DataHandle,Element,Var,Data,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  character*(*)         ,intent(in)     :: Data
  
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: i
  integer                               :: NVar
  character(len=1)                      :: null
  character(len=4096)                   :: tmpdata
  integer                               :: length

  length = len(Data)
  if(1 > length) then
     length = 0
     null = char(0)
  else if(4096 < length) then
     length = 4096
     tmpdata = Data(1:4096)
  else
     tmpdata = trim(Data)
  end if

  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN  
    write(msg,*) 'Warning WRITE METADATA AFTER OPEN in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do i=1,MaxVars
      if(TRIM(DH%VarNames(i)) == TRIM(VarName)) then
        NVar = i
        exit
      elseif(i == MaxVars) then
        Status = WRF_WARN_VAR_NF 
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,' ','CHAR',', line', __LINE__ &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo

   !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
   !write(unit=0, fmt='(6a)') 'Var: ', trim(Var), ', Data: ', trim(Data), ', tmpdata: ', trim(tmpdata)
   !write(unit=0, fmt='(3a,i6)') 'Element = ', trim(Element), ', NVar = ', NVar
   !write(unit=0, fmt='(2(a,i6))') 'DH%descVar(NVar)%VarID = ', DH%descVar(NVar)%VarID, &
   !     ', length = ', length

    if(DH%Write) then
      DH%Write = .false.
      stat = pio_redef(DH%file_handle)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
        call wrf_debug ( WARN , msg)
        return
      endif
    endif

    if(1 > length) then
     !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
     !write(unit=0, fmt='(6a)') 'Var: ', trim(Var), ', Element: ', trim(Element), ', tmpdata: ', trim(tmpdata)
     !write(unit=0, fmt='(2(a,i6))') 'DH%descVar(NVar)%VarID = ', DH%descVar(NVar)%VarID, &
     !   ', length = ', length
      stat = pio_put_att(DH%file_handle,DH%descVar(NVar),trim(Element),null)
    else
     !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
     !write(unit=0, fmt='(6a)') 'Var: ', trim(Var), ', Element: ', trim(Element), ', tmpdata: ', trim(tmpdata)
     !write(unit=0, fmt='(2(a,i6))') 'DH%descVar(NVar)%VarID = ', DH%descVar(NVar)%VarID, &
     !   ', length = ', length
      stat = pio_put_att(DH%file_handle,DH%descVar(NVar),trim(Element),tmpdata)
    endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS 
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_put_var_ti_char_arr

subroutine ext_pio_put_var_ti_char_sca(DataHandle,Element,Var,Data,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  character*(*) ,intent(in) :: Data
  
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: i
  integer                               :: NVar
  character*1                           :: null

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  null=char(0)
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_MD_AFTER_OPEN  
    write(msg,*) 'Warning WRITE METADATA AFTER OPEN in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    do NVar=1,MaxVars
      if(TRIM(DH%VarNames(NVar)) == TRIM(VarName)) then
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_VAR_NF 
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,' ','CHAR',', line', __LINE__ &
                        ,NVar,VarName
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(len_trim(Data).le.0) then
      stat = pio_put_var(DH%file_handle,DH%descVar(NVar),null)
    else
      stat = pio_put_var(DH%file_handle,DH%descVar(NVar),trim(Data))
    endif
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error for Var ',TRIM(Var),&
        ' Element ',trim(Element),' in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS 
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_ti_char_sca

subroutine ext_pio_put_var_td_char_arr(DataHandle,Element,DateStr,Var,Data,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  character*(*)         ,intent(in)     :: Data
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  integer                               :: stat
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  character(len=4096)                   :: tmpdata(1)
  integer                               :: length

  length = len(Data)
  if(1 > length) then
     length = 1
     tmpdata(1) = ""
  else if(4096 < length) then
     length = 4096
     tmpdata(1) = Data(1:4096)
  else
     tmpdata(1) = trim(Data)
  end if

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(4a)') 'Var: ', trim(Var), ', Data: ', tmpdata(1)
  write(unit=0, fmt='(4a)') 'Name: ', trim(Name), ', Element = ', trim(Element)

  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(len(Data) < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      return
    endif
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE  
        return
      elseif(DH%VarNames(NVar) == NO_NAME) then
        DH%VarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,' ','CHAR',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == len(Data)) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = pio_def_dim(DH%file_handle,DH%DimNames(i),len(Data),DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = len(Data)
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS  
        write(msg,*) 'Warning TOO MANY DIMENSIONS in ',__FILE__,' ','CHAR',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%VarDimLens(1,NVar) = len(Data)
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') '10 Define Var <', trim(Var), '> as NVar:', NVar
    stat = pio_def_var(DH%file_handle,Name,PIO_CHAR,DH%descVar(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        exit
      elseif(DH%VarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF  
        write(msg,*) 'Warning METADATA NOT FOUND in ',__FILE__,' ','CHAR',', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,' ','CHAR',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(len(Data) > DH%VarDimLens(1,NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG 
      write(msg,*) 'Warning COUNT TOO LONG in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    elseif(len(Data) < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      write(msg,*) 'Warning ZERO LENGTH PUT in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = length
    VCount(2) = 1
    tmpdata = Data
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),VStart,VCount,tmpdata)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line: ', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_put_var_td_char_arr

subroutine ext_pio_put_var_td_char_sca(DataHandle,Element,DateStr,Var,Data,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  character*(*)         ,intent(in)     :: Data
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  integer                               :: stat
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  character(len=DateStrLen)             :: tmpdata(1)
  integer                               :: length

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__

#if 0
  length = len(Data)
  if(1 > length) then
     length = 1
     tmpdata(1) = ""
  else if(4096 < length) then
     length = 4096
     tmpdata(1) = Data(1:4096)
  else
     tmpdata(1) = trim(Data)
  end if

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(4a)') 'Var: ', trim(Var), ', Data: ', tmpdata(1)

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(4a)') 'Var: ', trim(Var), ', Data: ', tmpdata(1)
  write(unit=0, fmt='(4a)') 'Name: ', trim(Name), ', Element = ', trim(Element)

  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    Status = WRF_WARN_WRITE_RONLY_FILE  
    write(msg,*) 'Warning WRITE READ ONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    if(len(Data) < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      return
    endif
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        Status = WRF_WARN_2DRYRUNS_1VARIABLE  
        return
      elseif(DH%VarNames(NVar) == NO_NAME) then
        DH%VarNames(NVar) = Name
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,' ','CHAR',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    do i=1,MaxDims
      if(DH%DimLengths(i) == len(Data)) then
        exit
      elseif(DH%DimLengths(i) == NO_DIM) then
        stat = pio_def_dim(DH%file_handle,DH%DimNames(i),len(Data),DH%DimIDs(i))
        call netcdf_err(stat,Status)
        if(Status /= WRF_NO_ERR) then
          write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
          call wrf_debug ( WARN , msg)
          return
        endif
        DH%DimLengths(i) = len(Data)
        exit
      elseif(i == MaxDims) then
        Status = WRF_WARN_TOO_MANY_DIMS  
        write(msg,*) 'Warning TOO MANY DIMENSIONS in ',__FILE__,' ','CHAR',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    DH%VarDimLens(1,NVar) = len(Data)
    VDims(1) = DH%DimIDs(i)
    VDims(2) = DH%DimUnlimID
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a,i6)') '11 Define Var <', trim(Var), '> as NVar:', NVar
    stat = pio_def_var(DH%file_handle,Name,PIO_CHAR,DH%descVar(NVar))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    do NVar=1,MaxVars
      if(DH%VarNames(NVar) == Name) then
        exit
      elseif(DH%VarNames(NVar) == NO_NAME) then
        Status = WRF_WARN_MD_NF  
        write(msg,*) 'Warning METADATA NOT FOUND in ',__FILE__,' ','CHAR',', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES  
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,' ','CHAR',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    if(len(Data) > DH%VarDimLens(1,NVar)) then
      Status = WRF_WARN_COUNT_TOO_LONG 
      write(msg,*) 'Warning COUNT TOO LONG in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    elseif(len(Data) < 1) then
      Status = WRF_WARN_ZERO_LENGTH_PUT  
      write(msg,*) 'Warning ZERO LENGTH PUT in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('write',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = len(Data)
    VCount(2) = 1
    tmpdata = Data
   !stat = pio_put_var(DH%file_handle,DH%descMDVar(NVar),VStart,VCount,Data)
    stat = pio_put_var(DH%file_handle,DH%descVar(NVar),VStart,VCount,tmpdata)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line: ', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
#endif
  return
end subroutine ext_pio_put_var_td_char_sca

subroutine ext_pio_get_var_ti_real_arr(DataHandle,Element,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  real,                  intent(out)    :: Data(:)
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XLen
  real,                     allocatable :: Buffer(:)
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: NVar
  integer                               :: XType

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line: ', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line: ', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line: ', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE 
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF  
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_inq_att(DH%file_handle,DH%descVar(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
      if(XType /= PIO_REAL) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att(DH%file_handle,DH%descVar(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status   = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = XLen
      Status   = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_get_var_ti_real_arr

subroutine ext_pio_get_var_ti_real_sca(DataHandle,Element,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  real,                  intent(out)    :: Data
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XLen
  real,                     allocatable :: Buffer(:)
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: NVar
  integer                               :: XType

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line: ', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line: ', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line: ', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE 
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF  
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_inq_att(DH%file_handle,DH%descVar(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
      if(XType /= PIO_REAL) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att(DH%file_handle,DH%descVar(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status   = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = XLen
      Status   = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_get_var_ti_real_sca

subroutine ext_pio_get_var_td_real_arr(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character (DateStrLen),intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  real                  ,intent(out)    :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  character (40+len(Element))           :: FName
  integer                               :: stat
  real                  ,allocatable    :: Buffer(:)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  integer                               :: DimIDs(2)
  integer                               :: VarID
  integer                               :: XType
  integer                               :: NDims
  integer                               :: NAtts
  integer                               :: Len1

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE  
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_varid(DH%file_handle,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
   !stat = NFMPI_INQ_VAR(file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    stat = pio_inquire_variable(DH%file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
      Status = WRF_WARN_TYPE_MISMATCH  
      write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D   
      write(msg,*) 'Fatal MDVAR DIM NOT 1D in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_inq_dimlen(DH%file_handle,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR   
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_var(DH%file_handle,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len1
      Status = WRF_NO_ERR   
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_var_td_real_arr

subroutine ext_pio_get_var_td_real_sca(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character (DateStrLen),intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  real                  ,intent(out)    :: Data
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  character (40+len(Element))           :: FName
  integer                               :: stat
  real                  ,allocatable    :: Buffer(:)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  integer                               :: DimIDs(2)
  integer                               :: VarID
  integer                               :: XType
  integer                               :: NDims
  integer                               :: NAtts
  integer                               :: Len1

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE  
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_varid(DH%file_handle,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
   !stat = NFMPI_INQ_VAR(file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    stat = pio_inquire_variable(DH%file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
      Status = WRF_WARN_TYPE_MISMATCH  
      write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D   
      write(msg,*) 'Fatal MDVAR DIM NOT 1D in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_inq_dimlen(DH%file_handle,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR   
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_var(DH%file_handle,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len1
      Status = WRF_NO_ERR   
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_var_td_real_sca

subroutine ext_pio_get_var_ti_double_arr(DataHandle,Element,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  real*8,                intent(out)    :: Data(:)
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XLen
  real*8,                allocatable    :: Buffer(:)
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: NVar
  integer                               :: XType

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE 
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF  
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_inq_att(DH%file_handle,DH%descVar(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
      if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att(DH%file_handle,DH%descVar(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status   = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = XLen
      Status   = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_get_var_ti_double_arr

subroutine ext_pio_get_var_ti_double_sca(DataHandle,Element,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  real*8,                intent(out)    :: Data
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XLen
  real*8,                allocatable    :: Buffer(:)
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: NVar
  integer                               :: XType

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE 
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF  
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_inq_att(DH%file_handle,DH%descVar(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
      if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att(DH%file_handle,DH%descVar(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status   = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = XLen
      Status   = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_get_var_ti_double_sca

subroutine ext_pio_get_var_td_double_arr(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character (DateStrLen),intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  real*8,                intent(out)    :: Data(:)
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  character (40+len(Element))           :: FName
  integer                               :: stat
  real*8,                allocatable    :: Buffer(:)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  integer                               :: DimIDs(2)
  integer                               :: VarID
  integer                               :: XType
  integer                               :: NDims
  integer                               :: NAtts
  integer                               :: Len1

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE  
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_varid(DH%file_handle,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
   !stat = NFMPI_INQ_VAR(file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    stat = pio_inquire_variable(DH%file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D   
      write(msg,*) 'Fatal MDVAR DIM NOT 1D in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_inq_dimlen(DH%file_handle,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR   
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_var(DH%file_handle,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len1
      Status = WRF_NO_ERR   
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_var_td_double_arr

subroutine ext_pio_get_var_td_double_sca(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character (DateStrLen),intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  real*8,                intent(out)    :: Data
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  character (40+len(Element))           :: FName
  integer                               :: stat
  real*8,                allocatable    :: Buffer(:)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  integer                               :: DimIDs(2)
  integer                               :: VarID
  integer                               :: XType
  integer                               :: NDims
  integer                               :: NAtts
  integer                               :: Len1

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE  
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_varid(DH%file_handle,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
   !stat = NFMPI_INQ_VAR(file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    stat = pio_inquire_variable(DH%file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D   
      write(msg,*) 'Fatal MDVAR DIM NOT 1D in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_inq_dimlen(DH%file_handle,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR   
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_var(DH%file_handle,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len1
      Status = WRF_NO_ERR   
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_var_td_double_sca

subroutine ext_pio_get_var_ti_integer_arr(DataHandle,Element,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  integer,               intent(out)    :: Data(:)
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XLen
  integer,               allocatable    :: Buffer(:)
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: NVar
  integer                               :: XType

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE 
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF  
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_inq_att(DH%file_handle,DH%descVar(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
      if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att(DH%file_handle,DH%descVar(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status   = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = XLen
      Status   = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_get_var_ti_integer_arr

subroutine ext_pio_get_var_ti_integer_sca(DataHandle,Element,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  integer,               intent(out)    :: Data
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XLen
  integer,               allocatable    :: Buffer(:)
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: NVar
  integer                               :: XType

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE 
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF  
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_inq_att(DH%file_handle,DH%descVar(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
      if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att(DH%file_handle,DH%descVar(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status   = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = XLen
      Status   = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_get_var_ti_integer_sca

subroutine ext_pio_get_var_td_integer_arr(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character (DateStrLen),intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  integer,               intent(out)    :: Data(:)
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  character (40+len(Element))           :: FName
  integer                               :: stat
  integer               ,allocatable    :: Buffer(:)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  integer                               :: DimIDs(2)
  integer                               :: VarID
  integer                               :: XType
  integer                               :: NDims
  integer                               :: NAtts
  integer                               :: Len1

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE  
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_varid(DH%file_handle,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
   !stat = NFMPI_INQ_VAR(file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    stat = pio_inquire_variable(DH%file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D   
      write(msg,*) 'Fatal MDVAR DIM NOT 1D in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_inq_dimlen(DH%file_handle,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR   
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_var(DH%file_handle,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len1
      Status = WRF_NO_ERR   
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_var_td_integer_arr

subroutine ext_pio_get_var_td_integer_sca(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character (DateStrLen),intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  integer,               intent(out)    :: Data
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  character (40+len(Element))           :: FName
  integer                               :: stat
  integer               ,allocatable    :: Buffer(:)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  integer                               :: DimIDs(2)
  integer                               :: VarID
  integer                               :: XType
  integer                               :: NDims
  integer                               :: NAtts
  integer                               :: Len1

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
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
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE  
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_varid(DH%file_handle,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
   !stat = NFMPI_INQ_VAR(file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    stat = pio_inquire_variable(DH%file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      if( .NOT. ( XType==PIO_REAL .OR. XType==PIO_DOUBLE) ) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D   
      write(msg,*) 'Fatal MDVAR DIM NOT 1D in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_inq_dimlen(DH%file_handle,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR   
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_var(DH%file_handle,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len1
      Status = WRF_NO_ERR   
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_var_td_integer_sca

subroutine ext_pio_get_var_ti_logical_arr(DataHandle,Element,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  logical,               intent(out)    :: Data(:)
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XLen
  integer,               allocatable    :: Buffer(:)
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: NVar
  integer                               :: XType

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE 
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF  
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_inq_att(DH%file_handle,DH%descVar(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
      if(XType /= PIO_INT) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','LOGICAL',', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att(DH%file_handle,DH%descVar(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data(1:min(XLen,Count)) = Buffer(1:min(XLen,Count))==1
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status   = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = XLen
      Status   = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_get_var_ti_logical_arr

subroutine ext_pio_get_var_ti_logical_sca(DataHandle,Element,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  logical,               intent(out)    :: Data
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XLen
  integer,               allocatable    :: Buffer(:)
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: NVar
  integer                               :: XType

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE 
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF  
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_inq_att(DH%file_handle,DH%descVar(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
      if(XType /= PIO_INT) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','LOGICAL',', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    allocate(Buffer(XLen), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR 
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_att(DH%file_handle,DH%descVar(NVar),trim(Element), Buffer )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(XLen > Count) then
      OutCount = Count
      Status   = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = XLen
      Status   = WRF_NO_ERR
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_get_var_ti_logical_sca

subroutine ext_pio_get_var_td_logical_arr(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character (DateStrLen),intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  logical,               intent(out)    :: Data(:)
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  character (40+len(Element))           :: FName
  integer                               :: stat
  integer               ,allocatable    :: Buffer(:)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  integer                               :: DimIDs(2)
  integer                               :: VarID
  integer                               :: XType
  integer                               :: NDims
  integer                               :: NAtts
  integer                               :: Len1

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE  
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_varid(DH%file_handle,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = pio_inquire_variable(DH%file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      if(XType /= PIO_INT) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','LOGICAL',', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D   
      write(msg,*) 'Fatal MDVAR DIM NOT 1D in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_inq_dimlen(DH%file_handle,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR   
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_var(DH%file_handle,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    Data(1:min(Len1,Count)) = Buffer(1:min(Len1,Count))==1
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len1
      Status = WRF_NO_ERR   
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_var_td_logical_arr

subroutine ext_pio_get_var_td_logical_sca(DataHandle,Element,DateStr,Var,Data,Count,OutCount,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character (DateStrLen),intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  logical,               intent(out)    :: Data
  integer,               intent(in)     :: Count
  integer,               intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  character (40+len(Element))           :: FName
  integer                               :: stat
  integer               ,allocatable    :: Buffer(:)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  integer                               :: DimIDs(2)
  integer                               :: VarID
  integer                               :: XType
  integer                               :: NDims
  integer                               :: NAtts
  integer                               :: Len1

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE  
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,' ','LOGICAL',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_varid(DH%file_handle,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = pio_inquire_variable(DH%file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      if(XType /= PIO_INT) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','LOGICAL',', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D   
      write(msg,*) 'Fatal MDVAR DIM NOT 1D in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_inq_dimlen(DH%file_handle,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = min(Count,Len1)
    VCount(2) = 1
    allocate(Buffer(VCount(1)), STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_ALLOCATION_ERROR   
      write(msg,*) 'Fatal ALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_get_var(DH%file_handle,VarID,VStart,VCount,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = Buffer(1)
    deallocate(Buffer, STAT=stat)
    if(stat/= 0) then
      Status = WRF_ERR_FATAL_DEALLOCATION_ERR  
      write(msg,*) 'Fatal DEALLOCATION ERROR in ',__FILE__,' ','LOGICAL',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    if(Len1 > Count) then
      OutCount = Count
      Status = WRF_WARN_MORE_DATA_IN_FILE  
    else
      OutCount = Len1
      Status = WRF_NO_ERR   
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','LOGICAL',', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_get_var_td_logical_sca

subroutine ext_pio_get_var_ti_char_arr(DataHandle,Element,Var,Data,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  character*(*)         ,intent(out)    :: Data
  integer                               :: Count = 1
  
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XLen
  
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: NVar
  integer                               :: XType

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE 
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF  
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,' ','CHAR',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_inq_att(DH%file_handle,DH%descVar(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
      if(XType /= PIO_CHAR) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','CHAR',', line', __LINE__
        return
      endif
    if(XLen > len(Data)) then
      Status = WRF_WARN_CHARSTR_GT_LENDATA   
      write(msg,*) 'Warning LEN CHAR STRING > LEN DATA in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = pio_get_att(DH%file_handle,DH%descVar(NVar),trim(Element), Data )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_get_var_ti_char_arr

subroutine ext_pio_get_var_ti_char_sca(DataHandle,Element,Var,Data,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: Var
  character*(*)         ,intent(out)    :: Data
  integer                               :: Count = 1
  
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: XLen
  
  character (VarNameLen)                :: VarName
  integer                               :: stat
  integer                               :: NVar
  integer                               :: XType

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE 
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF  
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,' ','CHAR',', line', __LINE__ 
        call wrf_debug ( WARN , msg)
        return
      endif
    enddo
    stat = pio_inq_att(DH%file_handle,DH%descVar(NVar),trim(Element),XType,XLen)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
      if(XType /= PIO_CHAR) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','CHAR',', line', __LINE__
        return
      endif
    if(XLen > len(Data)) then
      Status = WRF_WARN_CHARSTR_GT_LENDATA   
      write(msg,*) 'Warning LEN CHAR STRING > LEN DATA in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = pio_get_att(DH%file_handle,DH%descVar(NVar),trim(Element), Data )
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
    endif
    
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  return
end subroutine ext_pio_get_var_ti_char_sca

subroutine ext_pio_get_var_td_char_arr(DataHandle,Element,DateStr,Var,Data,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character (DateStrLen),intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  character*(*)         ,intent(out)    :: Data
  
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  character (40+len(Element))           :: FName
  integer                               :: stat
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  integer                               :: DimIDs(2)
  integer                               :: VarID
  integer                               :: XType
  integer                               :: NDims
  integer                               :: NAtts
  integer                               :: Len1
  integer,               parameter      :: Count = 1
  character(DateStrLen)                 :: Buffer(1)

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE  
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_varid(DH%file_handle,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    stat = pio_inquire_variable(DH%file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
    if(XType /= PIO_CHAR) then
      Status = WRF_WARN_TYPE_MISMATCH  
      write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D   
      write(msg,*) 'Fatal MDVAR DIM NOT 1D in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_inq_dimlen(DH%file_handle,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Len1
    VCount(2) = 1
    if(Len1 > len(Data)) then
      Status = WRF_WARN_CHARSTR_GT_LENDATA  
      write(msg,*) 'Warning LEN CHAR STRING > LEN DATA in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = ''
    stat = pio_get_var(DH%file_handle,VarID,VStart,VCount,Buffer)
   !stat = pio_get_var(DH%file_handle,VarID,Buffer)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = Buffer(1)
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line', __LINE__ 
  endif
  return
end subroutine ext_pio_get_var_td_char_arr

subroutine ext_pio_get_var_td_char_sca(DataHandle,Element,DateStr,Var,Data,Status)
  use pio_kinds
  use pio
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character (DateStrLen),intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Var
  character*(*)         ,intent(out)    :: Data
  
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  character (VarNameLen)                :: VarName
  character (40+len(Element))           :: Name
  character (40+len(Element))           :: FName
  integer                               :: stat
  character (80)        ,allocatable    :: Buffer(:)
  integer                               :: i
  integer                               :: VDims (2)
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: NVar
  integer                               :: TimeIndex
  integer                               :: DimIDs(2)
  integer                               :: VarID
  integer                               :: XType
  integer                               :: NDims
  integer                               :: NAtts
  integer                               :: Len1
  integer,               parameter      :: Count = 1

  if(Count <= 0) then
    Status = WRF_WARN_ZERO_LENGTH_GET  
    write(msg,*) 'Warning ZERO LENGTH GET in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  VarName = Var
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  call GetName(Element, VarName, Name, Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
    return
  endif
  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED  
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    Status = WRF_WARN_DRYRUN_READ  
    write(msg,*) 'Warning DRYRUN READ in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE  
    write(msg,*) 'Warning READ WONLY FILE in ',__FILE__,' ','CHAR',', line', __LINE__
    call wrf_debug ( WARN , msg)
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ) then
    stat = pio_inq_varid(DH%file_handle,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
   !stat = NFMPI_INQ_VAR(file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    stat = pio_inquire_variable(DH%file_handle,VarID,FName,XType,NDims,DimIDs,NAtts)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' Element ',Element
      call wrf_debug ( WARN , msg)
      return
    endif
      if(XType /= PIO_CHAR) then
        Status = WRF_WARN_TYPE_MISMATCH  
        write(msg,*) 'Warning TYPE MISMATCH in ',__FILE__,' ','CHAR',', line', __LINE__
        call wrf_debug ( WARN , msg)
        return
      endif
    if(NDims /= NMDVarDims) then
      Status = WRF_ERR_FATAL_MDVAR_DIM_NOT_1D   
      write(msg,*) 'Fatal MDVAR DIM NOT 1D in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( FATAL , msg)
      return
    endif
    stat = pio_inq_dimlen(DH%file_handle,DimIDs(1),Len1)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__,' DimIDs(1) ',DimIDs(1)
      call wrf_debug ( WARN , msg)
      return
    endif
    call GetTimeIndex('read',DataHandle,DateStr,TimeIndex,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = Len1
    VCount(2) = 1
    if(Len1 > len(Data)) then
      Status = WRF_WARN_CHARSTR_GT_LENDATA  
      write(msg,*) 'Warning LEN CHAR STRING > LEN DATA in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
    Data = ''
   !stat = pio_get_var(DH%file_handle,VarID,VStart,VCount,Data)
    stat = pio_get_var(DH%file_handle,VarID,Data)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS  
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,' ','CHAR',', line', __LINE__ 
  endif
  return
end subroutine ext_pio_get_var_td_char_sca

subroutine ext_pio_put_dom_td_real_arr(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real                  ,intent(in)     :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pio_put_var_td_real_arr(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,Status)
  return
end subroutine ext_pio_put_dom_td_real_arr

subroutine ext_pio_put_dom_td_real_sca(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real                  ,intent(in)     :: Data
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pio_put_var_td_real_sca(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,Status)
  return
end subroutine ext_pio_put_dom_td_real_sca

subroutine ext_pio_put_dom_td_integer_arr(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(in)     :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pio_put_var_td_integer_arr(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,Status)
  return
end subroutine ext_pio_put_dom_td_integer_arr

subroutine ext_pio_put_dom_td_integer_sca(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(in)     :: Data
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pio_put_var_td_integer_sca(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,Status)
  return
end subroutine ext_pio_put_dom_td_integer_sca

subroutine ext_pio_put_dom_td_double_arr(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real*8                ,intent(in)     :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pio_put_var_td_double_arr(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'   ,Data,Count,Status)
  return
end subroutine ext_pio_put_dom_td_double_arr

subroutine ext_pio_put_dom_td_double_sca(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real*8                ,intent(in)     :: Data
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pio_put_var_td_double_sca(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'   ,Data,Count,Status)
  return
end subroutine ext_pio_put_dom_td_double_sca

subroutine ext_pio_put_dom_td_logical_arr(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  logical               ,intent(in)     :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pio_put_var_td_logical_arr(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,Status)
  return
end subroutine ext_pio_put_dom_td_logical_arr

subroutine ext_pio_put_dom_td_logical_sca(DataHandle,Element,DateStr,Data,Count,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  logical               ,intent(in)     :: Data
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: Status

  call ext_pio_put_var_td_logical_sca(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,Status)
  return
end subroutine ext_pio_put_dom_td_logical_sca

subroutine ext_pio_put_dom_td_char_arr(DataHandle,Element,DateStr,Data,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Data
  integer               ,intent(out)    :: Status

  call ext_pio_put_var_td_char_arr(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Status)
  return
end subroutine ext_pio_put_dom_td_char_arr

subroutine ext_pio_put_dom_td_char_sca(DataHandle,Element,DateStr,Data,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(in)     :: Data
  integer               ,intent(out)    :: Status

  call ext_pio_put_var_td_char_sca(DataHandle,Element,DateStr, &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Status)
  return
end subroutine ext_pio_put_dom_td_char_sca

subroutine ext_pio_get_dom_td_real_arr(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real                  ,intent(out)    :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pio_get_var_td_real_arr(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,OutCount,Status)
  return
end subroutine ext_pio_get_dom_td_real_arr

subroutine ext_pio_get_dom_td_real_sca(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real                  ,intent(out)    :: Data
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pio_get_var_td_real_sca(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Count,OutCount,Status)
  return
end subroutine ext_pio_get_dom_td_real_sca

subroutine ext_pio_get_dom_td_integer_arr(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(out)    :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pio_get_var_td_integer_arr(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,OutCount,Status)
  return
end subroutine ext_pio_get_dom_td_integer_arr

subroutine ext_pio_get_dom_td_integer_sca(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(out)    :: Data
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pio_get_var_td_integer_sca(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,OutCount,Status)
  return
end subroutine ext_pio_get_dom_td_integer_sca

subroutine ext_pio_get_dom_td_double_arr(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real*8                ,intent(out)    :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pio_get_var_td_double_arr(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'   ,Data,Count,OutCount,Status)
  return
end subroutine ext_pio_get_dom_td_double_arr

subroutine ext_pio_get_dom_td_double_sca(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  real*8                ,intent(out)    :: Data
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pio_get_var_td_double_sca(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'   ,Data,Count,OutCount,Status)
  return
end subroutine ext_pio_get_dom_td_double_sca

subroutine ext_pio_get_dom_td_logical_arr(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  logical               ,intent(out)    :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pio_get_var_td_logical_arr(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,OutCount,Status)
  return
end subroutine ext_pio_get_dom_td_logical_arr

subroutine ext_pio_get_dom_td_logical_sca(DataHandle,Element,DateStr,Data,Count,OutCount,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  logical               ,intent(out)    :: Data(:)
  integer               ,intent(in)     :: Count
  integer               ,intent(out)    :: OutCount
  integer               ,intent(out)    :: Status
  call ext_pio_get_var_td_logical_sca(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_'    ,Data,Count,OutCount,Status)
  return
end subroutine ext_pio_get_dom_td_logical_sca

subroutine ext_pio_get_dom_td_char_arr(DataHandle,Element,DateStr,Data,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(out)    :: Data
  integer               ,intent(out)    :: Status
  call ext_pio_get_var_td_char_arr(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Status)
  return
end subroutine ext_pio_get_dom_td_char_arr

subroutine ext_pio_get_dom_td_char_sca(DataHandle,Element,DateStr,Data,Status)
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: Element
  character*(*)         ,intent(in)     :: DateStr
  character*(*)         ,intent(out)    :: Data
  integer               ,intent(out)    :: Status
  call ext_pio_get_var_td_char_sca(DataHandle,Element,DateStr,          &
       'E_X_T_D_O_M_A_I_N_M_E_T_A_DATA_' ,Data,Status)
  return
end subroutine ext_pio_get_dom_td_char_sca

subroutine ext_pio_write_field(DataHandle,DateStr,Var,Field,FieldType,grid, &
                               DomainDesc, MemoryOrdIn, Stagger, DimNames,  &
                               DomainStart, DomainEnd, MemoryStart, MemoryEnd, &
                               PatchStart, PatchEnd, Status)
  use wrf_data_pio
  use pio_routines
  use module_domain
  implicit none
  include 'wrf_status_codes.h'
  integer                       ,intent(in)    :: DataHandle
  character*(*)                 ,intent(in)    :: DateStr
  character*(*)                 ,intent(in)    :: Var
  integer                       ,intent(inout) :: Field(*)
  integer                       ,intent(in)    :: FieldType
  type(domain)                                 :: grid
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrdIn
  character*(*)                 ,intent(in)    :: Stagger
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
  integer      ,dimension(NVarDims)            :: Length_global
  integer      ,dimension(NVarDims)            :: Length
  integer      ,dimension(NVarDims)            :: VDimIDs
  character(80),dimension(NVarDims)            :: RODimNames
  integer      ,dimension(NVarDims)            :: VStart
  integer      ,dimension(NVarDims)            :: VCount
  integer                                      :: stat
  integer                                      :: NVar
  integer                                      :: i,j,n,fldsize
  integer                                      :: XType
  character (80)                               :: NullName
  logical                                      :: NotFound
  integer, dimension(1,1)                      :: tmp0dint
  integer, dimension(:,:,:), allocatable       :: tmp2dint

 !Local, possibly adjusted, copies of MemoryStart and MemoryEnd
  MemoryOrder = trim(adjustl(MemoryOrdIn))
  NullName=char(0)
  call GetDim(MemoryOrder,NDim,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning BAD MEMORY ORDER |',MemoryOrder,'| in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif

 !call pio_setdebuglevel(1)

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

  write(msg,*)'ext_pio_write_field: called for ',TRIM(Var)
  CALL wrf_debug( 100, msg )

  VCount(1:NDim) = PatchEnd(1:NDim)-PatchStart(1:NDim)+1
  Length_global(1:NDim) = DomainEnd(1:NDim)-DomainStart(1:NDim)+1

  call ExtOrder(MemoryOrder,VCount,Status)
  call ExtOrder(MemoryOrder,Length_global,Status)

  call ExtOrderStr(MemoryOrder,DimNames,RODimNames,Status)

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
        DH%CurrentVariable= NVar
        exit
      elseif(NVar == MaxVars) then
        Status = WRF_WARN_TOO_MANY_VARIABLES
        write(msg,*) 'Warning TOO MANY VARIABLES in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo

    if(DH%Write)then
      DH%Write = .false.
      stat = pio_redef(DH%file_handle)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    endif

    do j = 1,NDim
      VDimIDs(j) = 0
      if(RODimNames(j) == NullName .or. RODimNames(j) == '') then
        do i=1,MaxDims
          if(DH%DimLengths(i) == Length_global(j)) then
            VDimIDs(j) = DH%DimIDs(i)
            exit
          elseif(DH%DimLengths(i) == NO_DIM) then
            DH%DimLengths(i) = Length_global(j)
            stat = pio_def_dim(DH%file_handle, DH%DimNames(i), DH%DimLengths(i), DH%DimIDs(i))
            call netcdf_err(stat,Status)
            if(Status /= WRF_NO_ERR) then
              write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
              call wrf_debug ( WARN , TRIM(msg))
              return
            endif
            VDimIDs(j) = DH%DimIDs(i)
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
              VDimIDs(j) = DH%DimIDs(i)
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
              DH%DimLengths(i) = Length_global(j)
              stat = pio_def_dim(DH%file_handle, DH%DimNames(i), DH%DimLengths(i), DH%DimIDs(i))
              call netcdf_err(stat,Status)
              if(Status /= WRF_NO_ERR) then
                write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
                call wrf_debug ( WARN , TRIM(msg))
                return
              endif
              VDimIDs(j) = DH%DimIDs(i)
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
      DH%VarDimLens(j,DH%NumVars) = Length_global(j)
    enddo

    select case (FieldType)
      case (WRF_REAL)
        XType = PIO_REAL
      case (WRF_DOUBLE)
        Xtype = PIO_DOUBLE
      case (WRF_INTEGER)
        XType = PIO_INT
      case (WRF_LOGICAL)
        XType = PIO_INT
      case default
        Status = WRF_WARN_DATA_TYPE_NOT_FOUND
        write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
    end select

    VDimIDs(NDim+1) = DH%DimUnlimID
   !write(unit=0, fmt='(/3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
   !write(unit=0, fmt='(3a,i6)') '1 Define Var <', trim(Var), '> as NVar:', DH%NumVars
    stat = pio_def_var(DH%file_handle,VarName,XType,VDimIDs(1:NDim+1),DH%descVar(DH%NumVars))
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_pio_write_field: NetCDF error for ',TRIM(VarName),' in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
   !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
   !write(unit=0, fmt='(a,i6)') 'DH%descVar(DH%NumVars)%VarID = ', DH%descVar(DH%NumVars)%VarID

    stat = pio_put_att(DH%file_handle,DH%descVar(DH%NumVars),'FieldType',FieldType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_pio_write_field: NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    call reorder(MemoryOrder,MemO)
    call uppercase(MemO,UCMemO)
    stat = pio_put_att(DH%file_handle,DH%descVar(DH%NumVars),'MemoryOrder',UCMemO)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'ext_pio_write_field: NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE) then
    if(.not. DH%Write) then
      DH%Write = .true.
      stat = pio_enddef(DH%file_handle)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error (',stat,') in file ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    endif

    do NVar=1,DH%NumVars
      if(DH%VarNames(NVar) == VarName) then
        DH%CurrentVariable = NVar
        exit
      elseif(NVar == DH%NumVars) then
        Status = WRF_WARN_VAR_NF
        write(msg,*) 'Warning VARIABLE NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo

    DH%vartype(DH%CurrentVariable) = NOT_LAND_SOIL_VAR
    fldsize = 1

    do j=1,NDim
      if(Length_global(j) /= DH%VarDimLens(j,DH%CurrentVariable) .AND. DH%FileStatus /= WRF_FILE_OPENED_FOR_UPDATE ) then
        Status = WRF_WARN_WRTLEN_NE_DRRUNLEN
        write(msg,*) 'Warning LENGTH != DRY RUN LENGTH for |',   &
                     VarName,'| dim ',j,' in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        write(msg,*) '   LENGTH ',Length_global(j),' DRY RUN LENGTH ',DH%VarDimLens(j,DH%CurrentVariable)
        call wrf_debug ( WARN , TRIM(msg))
        return
      elseif(PatchStart(j) < MemoryStart(j)) then
        Status = WRF_WARN_DIMENSION_ERROR
        write(msg,*) 'Warning DIMENSION ERROR for |',VarName,    &
                     '| in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo

    VStart = 1
    VStart(1:NDim) = PatchStart(1:NDim)
    call ExtOrder(MemoryOrder,VStart,Status)

    do n = 1, NDim
      VDimIDs(n) = 0
      do i=1,MaxDims
         if(DH%DimLengths(i) == Length_global(n)) then
            VDimIDs(n) = DH%DimIDs(i)
            exit
          end if
       end do

       Length(n) = MemoryEnd(n) - MemoryStart(n) + 1
       fldsize = fldsize * Length(n)

       if("land_cat_stag" == DimNames(n)) then
          DH%vartype(DH%CurrentVariable) = LAND_CAT_VAR
       else if("soil_cat_stag" == DimNames(n)) then
          DH%vartype(DH%CurrentVariable) = SOIL_CAT_VAR
       else if("soil_layers_stag" == DimNames(n)) then
          DH%vartype(DH%CurrentVariable) = SOIL_LAYERS_VAR
       else if("num_ext_model_couple_dom_stag" == DimNames(n)) then
          DH%vartype(DH%CurrentVariable) = MDL_CPL_VAR
       else if("ensemble_stag" == DimNames(n)) then
          DH%vartype(DH%CurrentVariable) = ENSEMBLE_VAR
       endif
    end do

#ifndef INTSPECIAL
    call FieldIO('write',DataHandle,DateStr,Length_global,VStart,VCount,Length,MemoryOrder, &
                  Stagger,FieldType,Field,Status)
#else
    if(WRF_INTEGER == FieldType) then
      if(1 == fldsize) then
         tmp0dint(1,1) = Field(1)
         stat = pio_put_var(DH%file_handle,DH%descVar(DH%CurrentVariable),tmp0dint)
         call netcdf_err(stat,Status)
      else if(2 == Ndim) then
         allocate(tmp2dint(Length(1),Length(2),1), stat=Status)
         n = 0
         do j=1,Length(2)
         do i=1,Length(1)
            n=n+1
            tmp2dint(i,j,1) = Field(n)
         enddo
         enddo
         call pio_write_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                               DH%iodesc2d_m_int, tmp2dint, Status)
         deallocate(tmp2dint)
      else
        call FieldIO('write',DataHandle,DateStr,Length_global,VStart,VCount,Length,MemoryOrder, &
                      Stagger,FieldType,Field,Status)
      endif
    else
       call FieldIO('write',DataHandle,DateStr,Length_global,VStart,VCount,Length,MemoryOrder, &
                     Stagger,FieldType,Field,Status)
    end if
#endif
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , TRIM(msg))
  endif
  return
end subroutine ext_pio_write_field

subroutine ext_pio_read_field(DataHandle,DateStr,Var,Field,FieldType,grid, &
                              DomainDesc, MemoryOrdIn, Stagger, DimNames,  &
                              DomainStart,DomainEnd,MemoryStart,MemoryEnd, &
                              PatchStart,PatchEnd,Status)
  use wrf_data_pio
  use pio_routines
  use module_utility
  use module_domain
  implicit none
  include 'wrf_status_codes.h'
  integer                       ,intent(in)    :: DataHandle
  character*(*)                 ,intent(in)    :: DateStr
  character*(*)                 ,intent(in)    :: Var
  integer                       ,intent(out)   :: Field(*)
  integer                       ,intent(in)    :: FieldType
  type(domain)                                 :: grid
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrdIn
  character*(*)                 ,intent(in)    :: Stagger ! Dummy for now
  character*(*) , dimension (*) ,intent(in)    :: DimNames
  integer ,dimension(*)         ,intent(in)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status
  character (3)                                :: MemoryOrder
  character(PIO_MAX_NAME)                      :: dimname
  type(wrf_data_handle)         ,pointer       :: DH
  integer                                      :: NDim
  character (VarNameLen)                       :: VarName
  integer ,dimension(NVarDims)                 :: VCount
  integer ,dimension(NVarDims)                 :: VStart
  integer ,dimension(NVarDims)                 :: VDimen
  integer ,dimension(NVarDims)                 :: Length
#if 0
  integer ,dimension(NVarDims)                 :: StoredLen
#endif
  integer ,dimension(NVarDims)                 :: VDimIDs
  integer ,dimension(NVarDims)                 :: MemS
  integer ,dimension(NVarDims)                 :: MemE
  integer                                      :: NVar
  character (VarNameLen)                       :: Name
  integer                                      :: XType
  integer                                      :: StoredDim
  integer                                      :: VarID
  integer                                      :: NDims
  integer                                      :: NAtts
  integer(KIND=PIO_OFFSET)                     :: Len
  integer                                      :: stat
  integer                                      :: i, j, n, fldsize
  integer                                      :: FType
  logical                                      :: isbdy
  integer, dimension(:,:,:), allocatable       :: tmp2dint
  character (len=2)                            :: readinStagger

  MemoryOrder = trim(adjustl(MemoryOrdIn))

  call GetDim(MemoryOrder,NDim,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning BAD MEMORY ORDER |',TRIM(MemoryOrder),'| for |', &
                 TRIM(Var),'| in ext_pio_read_field ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning DATE STRING ERROR |',TRIM(DateStr),'| for |',TRIM(Var), &
                 '| in ext_pio_read_field ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning Status = ',Status,' in ext_pio_read_field ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif

  VarName = Var
  DH%CurrentVariable = DH%CurrentVariable + 1
  DH%VarNames(DH%CurrentVariable) = VarName

  if(DH%FileStatus == WRF_FILE_NOT_OPENED) then
    Status = WRF_WARN_FILE_NOT_OPENED
    write(msg,*) 'Warning FILE NOT OPENED in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_NOT_COMMITTED) then
    RETURN
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_WRITE) then
    Status = WRF_WARN_READ_WONLY_FILE
    write(msg,*) 'Warning READ WRITE ONLY FILE in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
  elseif(DH%FileStatus == WRF_FILE_OPENED_FOR_READ .OR. DH%FileStatus == WRF_FILE_OPENED_FOR_UPDATE ) then
   !call pio_seterrorhandling(DH%file_handle, PIO_BCAST_ERROR)
    stat = pio_inq_varid(DH%file_handle,VarName,DH%descVar(DH%CurrentVariable))
   !call pio_seterrorhandling(DH%file_handle, PIO_INTERNAL_ERROR)
   !if(stat /= PIO_NOERR) then
   !   DH%descVar(DH%CurrentVariable)%varID = 0
   !   write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Varname, ',Varname, ' not found in file.'
   !   call wrf_debug ( WARN , TRIM(msg))
   !   return
   !endif

    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__,' Varname ',Varname
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif

    stat = pio_inquire_variable(DH%file_handle,DH%descVar(DH%CurrentVariable), &
                                Name,XType,StoredDim,VDimIDs,NAtts)

    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = pio_get_att(DH%file_handle,DH%descVar(DH%CurrentVariable),'FieldType',FType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif

    readinStagger = ''
    stat = pio_get_att(DH%file_handle,DH%descVar(DH%CurrentVariable),'stagger',readinStagger)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,' ','CHAR',', line', __LINE__
      call wrf_debug ( WARN , msg)
      return
    endif

!---allow coercion between double and single prec real
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
       !allow coercion between double and single prec real
        if(.NOT. (XType == PIO_REAL .OR. XType == PIO_DOUBLE) )  then
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning REAL TYPE MISMATCH in ',__FILE__,', line', __LINE__
        endif
      case (WRF_DOUBLE)
       !allow coercion between double and single prec real
        if(.NOT. (XType == PIO_REAL .OR. XType == PIO_DOUBLE) )  then
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning DOUBLE TYPE MISMATCH in ',__FILE__,', line', __LINE__
        endif
      case (WRF_INTEGER)
        if(XType /= PIO_INT)  then 
          Status = WRF_WARN_TYPE_MISMATCH
          write(msg,*) 'Warning INTEGER TYPE MISMATCH in ',__FILE__,', line', __LINE__
        endif
      case (WRF_LOGICAL)
        if(XType /= PIO_INT)  then
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
      stat = pio_inq_dimname(DH%file_handle,VDimIDs(1),dimname)
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
      IF ( dimname(1:10) == 'ext_scalar' ) THEN
        NDim = 1
        VCount(1) = 1
      ENDIF
    ENDIF
    if(StoredDim /= NDim+1) then
      Status = WRF_ERR_FATAL_BAD_VARIABLE_DIM
      write(msg,*) 'Fatal error BAD VARIABLE DIMENSION in ext_pio_read_field ',TRIM(Var),TRIM(DateStr)
      call wrf_debug ( FATAL , msg)
      write(msg,*) '  StoredDim ', StoredDim, ' .NE. NDim+1 ', NDim+1
      call wrf_debug ( FATAL , msg)
      return
    endif
#if 0
    do n=1,NDim
      stat = pio_inq_dimlen(DH%file_handle,VDimIDs(n),StoredLen(n))
      call netcdf_err(stat,Status)
      if(Status /= WRF_NO_ERR) then
        write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
      if(VCount(n) > StoredLen(n)) then
        Status = WRF_WARN_READ_PAST_EOF
        write(msg,*) 'Warning READ PAST EOF in ext_pio_read_field of ',TRIM(Var),VCount(n),'>',StoredLen(n)
        call wrf_debug ( WARN , TRIM(msg))
        return
      elseif(VCount(n) <= 0) then
        Status = WRF_WARN_ZERO_LENGTH_READ
        write(msg,*) 'Warning ZERO LENGTH READ in ',__FILE__,', line', __LINE__
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
#endif
   !write(unit=0, fmt='(//3a,i6)') 'file: ',__FILE__,', line', __LINE__
   !write(unit=0, fmt='(4x,a,i6,2a)') 'DH%CurrentVariable = ', DH%CurrentVariable, ', name: ', trim(VarName)

    VStart(1:NDim) = PatchStart(1:NDim)
    VCount(1:NDim) = PatchEnd(1:NDim) - PatchStart(1:NDim) + 1
    VDimen(1:NDim) = DomainEnd(1:NDim) - DomainStart(1:NDim) + 1

   !do n = 1, NDim
   !   write(unit=0, fmt='(4x,8(a,i2,a,i6))') &
   !        'DomainStart(', n, ')=', DomainStart(n), ', DomainEnd(', n, ')=', DomainEnd(n), &
   !        ', MemoryStart(', n, ')=', MemoryStart(n), ', MemoryEnd(', n, ')=', MemoryEnd(n), &
   !        ', PatchStart(', n, ')=', PatchStart(n), ', PatchEnd(', n, ')=', PatchEnd(n), &
   !        ', VStart(', n, ')=', VStart(n), ', VCount(', n, ')=', VCount(n)
   !end do

    call ExtOrder(MemoryOrder,VStart,Status)
    call ExtOrder(MemoryOrder,VCount,Status)
    call ExtOrder(MemoryOrder,VDimen,Status)

    DH%vartype(DH%CurrentVariable) = NOT_LAND_SOIL_VAR
    fldsize = 1
    do n = 1, NDim
       Length(n) = MemoryEnd(n) - MemoryStart(n) + 1
       fldsize = fldsize * Length(n)

      !write(unit=0, fmt='(4x,2(a,i2,a,i6))') &
      !     'VStart(', n, ')=', VStart(n), ', VCount(', n, ')=', VCount(n)

       if("land_cat_stag" == DH%DimNames(VDimIDs(n))) then
          DH%vartype(DH%CurrentVariable) = LAND_CAT_VAR
       else if("soil_cat_stag" == DH%DimNames(VDimIDs(n))) then
          DH%vartype(DH%CurrentVariable) = SOIL_CAT_VAR
       else if("soil_layers_stag" == DH%DimNames(VDimIDs(n))) then
          DH%vartype(DH%CurrentVariable) = SOIL_LAYERS_VAR
       else if("num_ext_model_couple_dom_stag" == DH%DimNames(VDimIDs(n))) then
          DH%vartype(DH%CurrentVariable) = MDL_CPL_VAR
       else if("ensemble_stag" == DH%DimNames(VDimIDs(n))) then
          DH%vartype(DH%CurrentVariable) = ENSEMBLE_VAR
       endif
    end do
   
#ifndef INTSPECIAL
    isbdy = is_boundary(MemoryOrder)
    if(isbdy) then
     !write(unit=0, fmt='(//3a,i6)') 'file: ',__FILE__,', line', __LINE__
     !write(unit=0, fmt='(4x,a,i6,2a)') 'DH%CurrentVariable = ',
     !DH%CurrentVariable, ', name: ', trim(VarName)

      call FieldBDY('read',DataHandle,DateStr,NDim,VDimen, &
                    MemoryStart,MemoryEnd,PatchStart,PatchEnd, &
                    FieldType,Field,Status)
    else
     !if((WRF_INTEGER == FieldType) .and. (1 == fldsize)) then
     !  Status = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),VCount(1:1))
     !  Field(1) = VCount(1)
     !else
        call FieldIO('read',DataHandle,DateStr,VDimen,VStart,VCount,Length,MemoryOrder, &
                      readinStagger,FieldType,Field,Status)
     !endif
    endif
#else
    if(WRF_INTEGER == FieldType) then
      if(1 == fldsize) then
         Status = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),VCount(1:1))
         Field(1) = VCount(1)
      else if(2 == Ndim) then
         allocate(tmp2dint(Length(1),Length(2),1), stat=Status)
         call pio_read_darray(DH%file_handle, DH%descVar(DH%CurrentVariable), &
                              DH%iodesc2d_m_int, tmp2dint, Status)
!                             DH%ioVar(DH%CurrentVariable), tmp2dint, Status)
         n = 0
         do j=1,Length(2)
         do i=1,Length(1)
            n=n+1
            Field(n) = tmp2dint(i,j,1)
         enddo
         enddo
         deallocate(tmp2dint)
      else
        call FieldIO('read',DataHandle,DateStr,VDimen,VStart,VCount,Length,MemoryOrder, &
                      readinStagger,FieldType,Field,Status)
      endif
    else
      isbdy = is_boundary(MemoryOrder)
      if(isbdy) then
       !write(unit=0, fmt='(//3a,i6)') 'file: ',__FILE__,', line', __LINE__
       !write(unit=0, fmt='(4x,a,i6,2a)') 'DH%CurrentVariable = ', DH%CurrentVariable, ', name: ', trim(VarName)

        call FieldBDY('read',DataHandle,DateStr,NDim,VDimen, &
                      MemoryStart,MemoryEnd,PatchStart,PatchEnd, &
                      FieldType,Field,Status)
      else
        call FieldIO('read',DataHandle,DateStr,VDimen,VStart,VCount,Length,MemoryOrder, &
                      readinStagger,FieldType,Field,Status)
      endif
    endif
#endif
  else
    Status = WRF_ERR_FATAL_BAD_FILE_STATUS
    write(msg,*) 'Fatal error BAD FILE STATUS in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( FATAL , msg)
  endif
  return
end subroutine ext_pio_read_field

subroutine ext_pio_inquire_opened( DataHandle, FileName , FileStatus, Status )
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(inout)  :: FileName
  integer               ,intent(out)    :: FileStatus
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH

  call upgrade_filename(FileName)
 !call upgrade_filename(DH%FileName)

  call GetDH(DataHandle,DH,Status)
  if(Status /= WRF_NO_ERR) then
    FileStatus = WRF_FILE_NOT_OPENED
    return
  endif
  if(trim(FileName) /= trim(DH%FileName)) then
    FileStatus = WRF_FILE_NOT_OPENED
  else
    FileStatus = DH%FileStatus
  endif
  Status = WRF_NO_ERR
  return
end subroutine ext_pio_inquire_opened

subroutine ext_pio_inquire_filename( Datahandle, FileName,  FileStatus, Status )
  use wrf_data_pio
  use pio_routines
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
  FileName = trim(DH%FileName)
 !call upgrade_filename(FileName)
  FileStatus = DH%FileStatus
  Status = WRF_NO_ERR
  return
end subroutine ext_pio_inquire_filename

subroutine ext_pio_set_time(DataHandle, DateStr, Status)
  use wrf_data_pio
  use pio_routines
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
end subroutine ext_pio_set_time

subroutine ext_pio_get_next_time(DataHandle, DateStr, Status)
  use wrf_data_pio
  use pio_routines
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
      write(msg,*) 'Warning ext_pio_get_next_time: DH%CurrentTime >= DH%NumberTimes ',DH%CurrentTime,DH%NumberTimes
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
end subroutine ext_pio_get_next_time

subroutine ext_pio_get_previous_time(DataHandle, DateStr, Status)
  use wrf_data_pio
  use pio_routines
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
end subroutine ext_pio_get_previous_time

subroutine ext_pio_get_next_var(DataHandle, VarName, Status)
  use wrf_data_pio
  use pio_routines
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
end subroutine ext_pio_get_next_var

subroutine ext_pio_end_of_frame(DataHandle, Status)
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH

  call GetDH(DataHandle,DH,Status)
  return
end subroutine ext_pio_end_of_frame

! NOTE:  For scalar variables NDim is set to zero and DomainStart and 
! NOTE:  DomainEnd are left unmodified.  
subroutine ext_pio_get_var_info(DataHandle,Name,NDim,MemoryOrder,Stagger,DomainStart,DomainEnd,WrfType,Status)
  use wrf_data_pio
  use pio_routines
  implicit none
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
    stat = pio_inq_varid(DH%file_handle,Name,VarID)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = pio_inq_vartype(DH%file_handle,VarID,XType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    stat = pio_get_att(DH%file_handle,VarID,'FieldType',WrfType)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    select case (XType)
     !case (PIO_BYTE)
     !  Status = WRF_WARN_BAD_DATA_TYPE
     !  write(msg,*) 'Warning BYTE IS BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
     !  call wrf_debug ( WARN , TRIM(msg))
     !  return
      case (PIO_CHAR)
        Status = WRF_WARN_BAD_DATA_TYPE
        write(msg,*) 'Warning CHAR IS BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
     !case (PIO_SHORT)
     !  Status = WRF_WARN_BAD_DATA_TYPE
     !  write(msg,*) 'Warning SHORT IS BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
     !  call wrf_debug ( WARN , TRIM(msg))
     !  return
      case (PIO_INT)
        if(WrfType /= WRF_INTEGER .and. WrfType /= WRF_LOGICAL) then
          Status = WRF_WARN_BAD_DATA_TYPE
          write(msg,*) 'Warning BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
          call wrf_debug ( WARN , TRIM(msg))
          return
        endif
      case (PIO_REAL)
        if(WrfType /= WRF_REAL) then
          Status = WRF_WARN_BAD_DATA_TYPE
          write(msg,*) 'Warning BAD DATA TYPE in ',__FILE__,', line', __LINE__ 
          call wrf_debug ( WARN , TRIM(msg))
          return
        endif
      case (PIO_DOUBLE)
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

    stat = pio_get_att(DH%file_handle,VarID,'MemoryOrder',MemoryOrder)
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
    stat = pio_inq_vardimid(DH%file_handle,VarID,VDimIDs)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
    do j = 1, NDim
      DomainStart(j) = 1
      stat = pio_inq_dimlen(DH%file_handle,VDimIDs(j),DomainEnd(j))
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
end subroutine ext_pio_get_var_info

subroutine ext_pio_warning_str( Code, ReturnString, Status)
  use wrf_data_pio
  use pio_routines
  implicit none
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
end subroutine ext_pio_warning_str


!returns message string for all WRF and netCDF warning/error status codes
!Other i/o packages must  provide their own routines to return their own status messages
subroutine ext_pio_error_str( Code, ReturnString, Status)
  use wrf_data_pio
  use pio_routines
  implicit none
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
end subroutine ext_pio_error_str


subroutine ext_pio_end_independent_mode(DataHandle, Status)
  use wrf_data_pio
  use pio_routines
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat

  DH => WrfDataHandles(DataHandle)
  return
end subroutine ext_pio_end_independent_mode

subroutine ext_pio_start_independent_mode(DataHandle, Status)
  use wrf_data_pio
  use pio_routines
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer                               :: stat

  DH => WrfDataHandles(DataHandle)
  return
end subroutine ext_pio_start_independent_mode

