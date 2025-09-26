!*-----------------------------------------------------------------------------
!*
!*  Todd Hutchinson
!*  WSI
!*  400 Minuteman Road
!*  Andover, MA     01810
!*  thutchinson@wsi.com
!*
!*  August, 2005
!*-----------------------------------------------------------------------------

!*
!* This io_grib2 API is designed to read WRF input and write WRF output data
!*   in grib version 2 format.  
!*


#include "wrf_projection.h"

module gr2_data_info

!*
!* This module will hold data internal to this I/O implementation.
!*   The variables will be accessible by all functions (provided they have a
!*   "USE gr2_data_info" line).
!*

  USE grib2tbls_types

  integer                , parameter       :: FATAL            = 1
  integer                , parameter       :: DEBUG            = 100
  integer                , parameter       :: DateStrLen       = 19
  integer                , parameter       :: maxMsgSize       = 300
  integer                , parameter       :: firstFileHandle  = 8
  integer                , parameter       :: maxFileHandles   = 200
  integer                , parameter       :: maxLevels        = 1000
  integer                , parameter       :: maxSoilLevels    = 100
  integer                , parameter       :: maxDomains       = 500
  character(200)                           :: mapfilename = 'grib2map.tbl'

  integer                , parameter       :: JIDSSIZE = 13
  integer                , parameter       :: JPDTSIZE = 15
  integer                , parameter       :: JGDTSIZE = 30

  logical                                  :: grib2map_table_filled = .FALSE.

  logical                                  :: WrfIOnotInitialized = .true.

  integer, dimension(maxDomains)           :: domains
  integer                                  :: max_domain = 0

  character*24                             :: StartDate = ''
  character*24                             :: InputProgramName = ''
  real                                     :: timestep
  integer                                  :: full_xsize, full_ysize
  REAL,          dimension(maxSoilLevels)  :: soil_depth, soil_thickness
  REAL,          dimension(maxLevels)      :: half_eta, full_eta

  integer                                  :: wrf_projection
  integer                                  :: background_proc_id
  integer                                  :: forecast_proc_id
  integer                                  :: production_status
  integer                                  :: compression
  real                                     :: center_lat, center_lon
  real                                     :: dx,dy
  real                                     :: truelat1, truelat2
  real                                     :: proj_central_lon

  TYPE :: HandleVar
     character, dimension(:), pointer      :: fileindex(:)
     integer                               :: CurrentTime
     integer                               :: NumberTimes
     integer                               :: sizeAllocated = 0
     logical                               :: write = .FALSE.
     character (DateStrLen), dimension(:),allocatable  :: Times(:)
     logical                               :: committed, opened, used
     character*128                         :: DataFile
     integer                               :: FileFd
     integer                               :: FileStatus
     integer                               :: recnum
     real                                  :: last_scalar_time_written
  ENDTYPE
  TYPE (HandleVar), dimension(maxFileHandles),SAVE :: fileinfo

  character(len=30000), dimension(maxFileHandles) :: td_output
  character(len=30000), dimension(maxFileHandles) :: ti_output
  character(len=30000), dimension(maxFileHandles) :: scalar_output
  character(len=30000), dimension(maxFileHandles) :: global_input = ''
  character(len=30000), dimension(maxFileHandles) :: scalar_input = ''

  real                                     :: last_fcst_secs
  real                                     :: fcst_secs

  logical                                  :: half_eta_init       = .FALSE.
  logical                                  :: full_eta_init       = .FALSE.
  logical                                  :: soil_thickness_init = .FALSE.
  logical                                  :: soil_depth_init     = .FALSE.

end module gr2_data_info


!*****************************************************************************

subroutine ext_gr2_ioinit(SysDepInfo,Status)

  USE gr2_data_info
  implicit none
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  CHARACTER*(*), INTENT(IN) :: SysDepInfo
  integer ,intent(out) :: Status
  integer :: i
  CHARACTER (LEN=300) :: wrf_err_message

  call wrf_debug ( DEBUG , 'Entering ext_gr2_ioinit')

  do i=firstFileHandle, maxFileHandles
        fileinfo(i)%used = .false.
        fileinfo(i)%committed = .false.
        fileinfo(i)%opened = .false.
        td_output(i) = ''
        ti_output(i) = ''
        scalar_output(i) = ''
  enddo
  domains(:) = -1
  last_fcst_secs = -1.0

  fileinfo(1:maxFileHandles)%FileStatus = WRF_FILE_NOT_OPENED
  WrfIOnotInitialized = .false.

  Status = WRF_NO_ERR

  return
end subroutine ext_gr2_ioinit

!*****************************************************************************

subroutine ext_gr2_ioexit(Status)

  USE gr2_data_info
  implicit none
#include "wrf_status_codes.h"
  integer ,intent(out) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr2_ioexit')

  Status = WRF_NO_ERR

  if (grib2map_table_filled) then
     call free_grib2map()
     grib2map_table_filled = .FALSE.
  endif

  return
end subroutine ext_gr2_ioexit

!*****************************************************************************

SUBROUTINE ext_gr2_open_for_read_begin ( FileName , Comm_compute, Comm_io, &
     SysDepInfo, DataHandle , Status )

  USE gr2_data_info
  USE grib2tbls_types
  USE grib_mod
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER (LEN=maxMsgSize) :: msg

  integer :: center, subcenter, MasterTblV, &
       LocalTblV, Disc, Category, ParmNum, DecScl, BinScl

  integer :: fields_to_skip
  integer :: JIDS(JIDSSIZE), JPDTN, JPDT(JPDTSIZE), JGDTN, &
       JGDT(JGDTSIZE)
  logical :: UNPACK
  character*(100) :: VarName
  type(gribfield) :: gfld
  integer         :: idx
  character(len=DateStrLen) :: theTime,refTime
  integer         :: time_range_convert(13)
  integer         :: fcstsecs
  integer         :: endchar
  integer         :: ierr

  INTERFACE
     Subroutine load_grib2map (filename, message, status)
       USE grib2tbls_types
       character*(*), intent(in)                   :: filename
       character*(*), intent(inout)                :: message
       integer      , intent(out)                  :: status
     END subroutine load_grib2map
  END INTERFACE

  call wrf_debug ( DEBUG , &
       'Entering ext_gr2_open_for_read_begin, opening '//trim(FileName))

  CALL gr2_get_new_handle(DataHandle)

  !
  ! Open grib file
  !
  if (DataHandle .GT. 0) then
     
     call baopenr(DataHandle,trim(FileName),status)

     if (status .ne. 0) then
        Status = WRF_ERR_FATAL_BAD_FILE_STATUS
     else
        fileinfo(DataHandle)%opened = .true.
        fileinfo(DataHandle)%DataFile = TRIM(FileName)
        fileinfo(DataHandle)%FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
!        fileinfo(DataHandle)%CurrentTime = 1
     endif
  else
     Status = WRF_WARN_TOO_MANY_FILES
     return
  endif
 
  fileinfo(DataHandle)%recnum = -1

  !
  ! Fill up the grib2tbls structure from data in the grib2map file.
  !
  if (.NOT. grib2map_table_filled) then
     grib2map_table_filled = .TRUE.
     CALL load_grib2map(mapfilename, msg, status)
     if (status .ne. 0) then
        call wrf_message(trim(msg))
        Status = WRF_ERR_FATAL_BAD_FILE_STATUS
        return
     endif
  endif


  !
  ! Get the parameter info for metadata
  !
  VarName = "WRF_GLOBAL"
  CALL get_parminfo(VarName, center, subcenter, MasterTblV, &
       LocalTblV, Disc, Category, ParmNum, DecScl, BinScl, status)
  if (status .ne. 0) then
     write(msg,*) 'Could not find parameter for '//   &
          trim(VarName)//'   Skipping output of '//trim(VarName)
     call wrf_message(trim(msg))
     Status =  WRF_GRIB2_ERR_GRIB2MAP
     return
  endif

  !
  ! Read the metadata
  !
  fields_to_skip = 0
  
  !
  ! First, set all values to the wildcard, then reset values that we wish
  !    to specify.
  !
  call gr2_g2lib_wildcard(JIDS, JPDT, JGDT)
  
  JIDS(1) = center
  JIDS(2) = subcenter
  JIDS(3) = MasterTblV
  JIDS(4) = LocalTblV
  JIDS(5) = 1           ! Indicates that time is "Start of Forecast"
  JIDS(13) = 1          ! Type of processed data (1 for forecast products)
  
  JPDTN = 0             ! Product definition template number
  JPDT(1) = Category
  JPDT(2) = ParmNum
  JPDT(3) = 2           ! Generating process id
  JPDT(9) = 0           ! Forecast time 

  JGDTN    = -1         ! Indicates that any Grid Display Template is a match
  
  UNPACK   = .FALSE.    ! Dont unpack bitmap and data values

  CALL GETGB2(DataHandle, DataHandle, fields_to_skip, -1, Disc, JIDS, JPDTN, &
       JPDT, JGDTN, JGDT, UNPACK, fileinfo(DataHandle)%recnum, gfld, status)
  if (status .ne. 0) then
     if (status .eq. 99) then
        write(msg,*)'Could not find metadata field named '//trim(VarName)
     else
        write(msg,*)'Retrieving grib field '//trim(VarName)//' failed, ',status
     endif
     call wrf_message(trim(msg))
     status = WRF_GRIB2_ERR_GETGB2
     return
  endif

  global_input(DataHandle) = transfer(gfld%local,global_input(DataHandle))
  global_input(DataHandle)(gfld%locallen+1:30000) = ' '

  call gf_free(gfld)

  !
  ! Read and index all scalar data
  !
  VarName = "WRF_SCALAR"
  CALL get_parminfo(VarName, center, subcenter, MasterTblV, &
       LocalTblV, Disc, Category, ParmNum, DecScl, BinScl, status)
  if (status .ne. 0) then
     write(msg,*) 'Could not find parameter for '//   &
          trim(VarName)//'   Skipping reading of '//trim(VarName)
     call wrf_message(trim(msg))
     Status =  WRF_GRIB2_ERR_GRIB2MAP
     return
  endif

  !
  ! Read the metadata
  !
  ! First, set all values to wild, then specify necessary values
  !
  call gr2_g2lib_wildcard(JIDS, JPDT, JGDT)

  JIDS(1) = center
  JIDS(2) = subcenter
  JIDS(3) = MasterTblV
  JIDS(4) = LocalTblV

  JIDS(5) = 1           ! Indicates that time is "Start of Forecast"
  JIDS(13) = 1          ! Type of processed data (1 for forecast products)
  
  JPDTN = 0             ! Product definition template number
  JPDT(1) = Category
  JPDT(2) = ParmNum
  JPDT(3) = 2           ! Generating process id

  JGDTN    = -1         ! Indicates that any Grid Display Template is a match
  
  UNPACK   = .FALSE.    ! Dont unpack bitmap and data values

  fields_to_skip = 0
  do while (status .eq. 0) 
     CALL GETGB2(DataHandle, 0, fields_to_skip, -1, -1, JIDS, JPDTN, &
          JPDT, JGDTN, JGDT, UNPACK, fileinfo(DataHandle)%recnum, &
          gfld, status)
     if (status .eq. 99) then
        exit
     else if (status .ne. 0) then
        write(msg,*)'Finding data field '//trim(VarName)//' failed 1.'
        call wrf_message(trim(msg))
        Status = WRF_GRIB2_ERR_READ
        return
     endif
     
     ! Build times list here
     write(refTime,'(I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')      &
          gfld%idsect(6),'-',gfld%idsect(7),'-',gfld%idsect(8),'_',&
          gfld%idsect(9),':',gfld%idsect(10),':',gfld%idsect(11)

     time_range_convert(:) = -1
     time_range_convert(1) = 60
     time_range_convert(2) = 60*60
     time_range_convert(3) = 24*60*60
     time_range_convert(10) = 3*60*60
     time_range_convert(11) = 6*60*60
     time_range_convert(12) = 12*60*60
     time_range_convert(13) = 1
     
     if (time_range_convert(gfld%ipdtmpl(8)) .gt. 0) then
        fcstsecs = gfld%ipdtmpl(9)*time_range_convert(gfld%ipdtmpl(8))
     else 
        write(msg,*)'Invalid time range in input data: ',gfld%ipdtmpl(8),&
             ' Skipping'
        call wrf_message(trim(msg))
        call gf_free(gfld)
        cycle
     endif
     call advance_wrf_time(refTime,fcstsecs,theTime)

     call gr2_add_time(DataHandle,theTime)

     fields_to_skip = fields_to_skip + fileinfo(DataHandle)%recnum

     scalar_input(DataHandle) = transfer(gfld%local,scalar_input(DataHandle))
     scalar_input(DataHandle)(gfld%locallen+1:30000) = ' '
     
     call gf_free(gfld)
  enddo

  !
  ! Fill up the eta levels variables
  !

  if (.not. full_eta_init) then
     CALL gr2_fill_levels(DataHandle, "ZNW", full_eta, ierr)
     if (ierr .eq. 0) then
        full_eta_init = .TRUE.
     endif
  endif
  if (.not. half_eta_init) then
     CALL gr2_fill_levels(DataHandle, "ZNU", half_eta, ierr)
     if (ierr .eq. 0) then 
        half_eta_init = .TRUE.
     endif
  endif
  !
  ! Fill up the soil levels
  !
  if (.not. soil_depth_init) then
     call gr2_fill_levels(DataHandle,"ZS",soil_depth, ierr)
     if (ierr .eq. 0) then
        soil_depth_init = .TRUE.
     endif
  endif
  if (.not. soil_thickness_init) then
     call gr2_fill_levels(DataHandle,"DZS",soil_thickness, ierr)
     if (ierr .eq. 0) then
        soil_thickness_init = .TRUE.
     endif
  endif

  ! 
  ! Fill up any variables from the global metadata
  !

  CALL gr2_get_metadata_value(global_input(DataHandle), &
       'START_DATE', StartDate, status)
  if (status .ne. 0) then
     write(msg,*)'Could not find metadata value for START_DATE, continuing'
     call wrf_message(trim(msg))
  endif
 
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       'PROGRAM_NAME', InputProgramName, status)
  if (status .ne. 0) then
     write(msg,*)'Could not find metadata value for PROGRAM_NAME, continuing'
     call wrf_message(trim(msg))
  else
     endchar = SCAN(InputProgramName," ")
     InputProgramName = InputProgramName(1:endchar)
  endif


  Status = WRF_NO_ERR

  call wrf_debug ( DEBUG , 'Exiting ext_gr2_open_for_read_begin')

  RETURN
END SUBROUTINE ext_gr2_open_for_read_begin

!*****************************************************************************

SUBROUTINE ext_gr2_open_for_read_commit( DataHandle , Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  character(len=maxMsgSize) :: msg
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr2_open_for_read_commit')

  Status = WRF_NO_ERR
  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED
    write(msg,*) 'ext_gr2_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  fileinfo(DataHandle)%committed = .true.
  fileinfo(DataHandle)%FileStatus = WRF_FILE_OPENED_FOR_READ

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_gr2_open_for_read_commit

!*****************************************************************************

SUBROUTINE ext_gr2_open_for_read ( FileName , Comm_compute, Comm_io, &
     SysDepInfo, DataHandle , Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status


  call wrf_debug ( DEBUG , 'Entering ext_gr2_open_for_read')

  DataHandle = 0   ! dummy setting to quiet warning message
  CALL ext_gr2_open_for_read_begin( FileName, Comm_compute, Comm_io, &
       SysDepInfo, DataHandle, Status )
  IF ( Status .EQ. WRF_NO_ERR ) THEN
    CALL ext_gr2_open_for_read_commit( DataHandle, Status )
  ENDIF
  return

  RETURN  
END SUBROUTINE ext_gr2_open_for_read

!*****************************************************************************

SUBROUTINE ext_gr2_open_for_write_begin(FileName, Comm, IOComm, SysDepInfo, &
     DataHandle, Status)
  
  USE gr2_data_info
  implicit none
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"

  character*(*)        ,intent(in)  :: FileName
  integer              ,intent(in)  :: Comm
  integer              ,intent(in)  :: IOComm
  character*(*)        ,intent(in)  :: SysDepInfo
  integer              ,intent(out) :: DataHandle
  integer              ,intent(out) :: Status
  integer :: ierr
  CHARACTER (LEN=maxMsgSize) :: msg

  INTERFACE
     Subroutine load_grib2map (filename, message, status)
       USE grib2tbls_types
       character*(*), intent(in)                   :: filename
       character*(*), intent(inout)                :: message
       integer      , intent(out)                  :: status
     END subroutine load_grib2map
  END INTERFACE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_open_for_write_begin')

  Status = WRF_NO_ERR

  if (.NOT. grib2map_table_filled) then
     grib2map_table_filled = .TRUE.
     CALL load_grib2map(mapfilename, msg, status)
     if (status .ne. 0) then
        call wrf_message(trim(msg))
        Status = WRF_ERR_FATAL_BAD_FILE_STATUS
        return
     endif
  endif

  CALL gr2_get_new_handle(DataHandle)

  if (DataHandle .GT. 0) then

     call baopenw(DataHandle,trim(FileName),ierr)

     if (ierr .ne. 0) then
        Status = WRF_ERR_FATAL_BAD_FILE_STATUS
     else
        fileinfo(DataHandle)%opened = .true.
        fileinfo(DataHandle)%DataFile = TRIM(FileName)
        fileinfo(DataHandle)%FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
     endif
     fileinfo(DataHandle)%last_scalar_time_written = -1
     fileinfo(DataHandle)%committed = .false.
     td_output(DataHandle) = ''
     ti_output(DataHandle) = ''
     scalar_output(DataHandle) = ''
     fileinfo(DataHandle)%write = .true.
  else
     Status = WRF_WARN_TOO_MANY_FILES
  endif

  RETURN  
END SUBROUTINE ext_gr2_open_for_write_begin

!*****************************************************************************

SUBROUTINE ext_gr2_open_for_write_commit( DataHandle , Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr2_open_for_write_commit')

  IF ( fileinfo(DataHandle)%opened ) THEN
    IF ( fileinfo(DataHandle)%used ) THEN
      fileinfo(DataHandle)%committed = .true.
      fileinfo(DataHandle)%FileStatus = WRF_FILE_OPENED_FOR_WRITE
    ENDIF
  ENDIF

  Status = WRF_NO_ERR

  RETURN  
END SUBROUTINE ext_gr2_open_for_write_commit

!*****************************************************************************

subroutine ext_gr2_inquiry (Inquiry, Result, Status)
  use gr2_data_info
  implicit none
#include "wrf_status_codes.h"
  character *(*), INTENT(IN)    :: Inquiry
  character *(*), INTENT(OUT)   :: Result
  integer        ,INTENT(INOUT) :: Status
  SELECT CASE (Inquiry)
  CASE ("RANDOM_WRITE","RANDOM_READ")
     Result='ALLOW'
  CASE ("SEQUENTIAL_WRITE","SEQUENTIAL_READ")
     Result='NO'
  CASE ("OPEN_READ", "OPEN_WRITE", "OPEN_COMMIT_WRITE")
     Result='REQUIRE'
  CASE ("OPEN_COMMIT_READ","PARALLEL_IO")
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
end subroutine ext_gr2_inquiry

!*****************************************************************************

SUBROUTINE ext_gr2_inquire_opened ( DataHandle, FileName , FileStat, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStat
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr2_inquire_opened')

  FileStat = WRF_NO_ERR
  if ((DataHandle .ge. firstFileHandle) .and. &
       (DataHandle .le. maxFileHandles)) then
     FileStat = fileinfo(DataHandle)%FileStatus
  else
     FileStat = WRF_FILE_NOT_OPENED
  endif
  
  Status = FileStat

  RETURN
END SUBROUTINE ext_gr2_inquire_opened

!*****************************************************************************

SUBROUTINE ext_gr2_ioclose ( DataHandle, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER DataHandle, Status
  INTEGER istat
  character(len=1000) :: outstring
  character :: lf
  character*(maxMsgSize) :: msg
  integer   :: idx

  lf=char(10)
  call wrf_debug ( DEBUG , 'Entering ext_gr2_ioclose')

  Status = WRF_NO_ERR

  if (fileinfo(DataHandle)%write .eqv. .TRUE.) then
     call gr2_fill_local_use(DataHandle,scalar_output(DataHandle),&
          "WRF_SCALAR",fcst_secs,msg,status)
     if (status .ne. 0) then
        call wrf_message(trim(msg))
        return
     endif
     fileinfo(DataHandle)%last_scalar_time_written = fcst_secs
     scalar_output(DataHandle) = ''
     
     call gr2_fill_local_use(DataHandle,&
          trim(ti_output(DataHandle))//trim(td_output(DataHandle)),&
          "WRF_GLOBAL",0,msg,status)
     if (status .ne. 0) then
        call wrf_message(trim(msg))
        return
     endif
     ti_output(DataHandle) = ''
     td_output(DataHandle) = ''
  endif

  do idx = 1,fileinfo(DataHandle)%NumberTimes 
     if (allocated(fileinfo(DataHandle)%Times)) then
        deallocate(fileinfo(DataHandle)%Times)
     endif
  enddo
  fileinfo(DataHandle)%NumberTimes = 0
  fileinfo(DataHandle)%sizeAllocated = 0
  fileinfo(DataHandle)%CurrentTime = 0
  fileinfo(DataHandle)%write = .FALSE.

  call baclose(DataHandle,status)
  if (status .ne. 0) then
     call wrf_message("Closing file failed, continuing")
  else
     fileinfo(DataHandle)%opened = .true.
     fileinfo(DataHandle)%DataFile = ''
     fileinfo(DataHandle)%FileStatus = WRF_FILE_NOT_OPENED
  endif

  fileinfo(DataHandle)%used = .false.

  RETURN
END SUBROUTINE ext_gr2_ioclose

!*****************************************************************************

SUBROUTINE ext_gr2_write_field( DataHandle , DateStrIn , VarName , &
     Field , FieldType , Comm , IOComm, &
     DomainDesc , MemoryOrder , Stagger , &
     DimNames , &
     DomainStart , DomainEnd , &
     MemoryStart , MemoryEnd , &
     PatchStart , PatchEnd , &
     Status )

  USE gr2_data_info
  USE grib2tbls_types
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  integer                       ,intent(in)    :: DataHandle 
  character*(*)                 ,intent(in)    :: DateStrIn
  character*(*)                 ,intent(in)    :: VarName
  integer                       ,intent(in)    :: FieldType
  integer                       ,intent(inout) :: Comm
  integer                       ,intent(inout) :: IOComm
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrder
  character*(*)                 ,intent(in)    :: Stagger
  character*(*) , dimension (*) ,intent(in)    :: DimNames
  integer ,dimension(*)         ,intent(in)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status

  real                          , intent(in), &
       dimension( 1:1,MemoryStart(1):MemoryEnd(1), &
       MemoryStart(2):MemoryEnd(2), &
       MemoryStart(3):MemoryEnd(3) )           :: Field


  character (120)                              :: DateStr

  character (maxMsgSize)                       :: msg
  integer                                      :: xsize, ysize, zsize
  integer                                      :: x, y, z
  integer                                      :: &
       x_start,x_end,y_start,y_end,z_start,z_end
  integer                                      :: idx
  integer                                      :: proj_center_flag
  logical                                      :: vert_stag = .false.
  real,    dimension(:,:), pointer             :: data
  integer                                      :: istat
  integer                                      :: accum_period
  integer, dimension(maxLevels)                :: level1, level2
  integer, dimension(maxLevels)                :: grib_levels
  logical                                      :: soil_layers, fraction
  integer                                      :: vert_unit1, vert_unit2
  integer                                      :: vert_sclFctr1, vert_sclFctr2
  integer                                      :: this_domain
  logical                                      :: new_domain
  real                                         :: &
       region_center_lat, region_center_lon
  integer                                      :: dom_xsize, dom_ysize;
  integer , parameter                          :: lcgrib = 2000000
  character (lcgrib)                           :: cgrib
  integer                                      :: ierr
  integer                                      :: lengrib

  integer                                     :: center, subcenter, &
       MasterTblV, LocalTblV, Disc, Category, ParmNum, DecScl, BinScl
  CHARACTER(len=100)  :: tmpstr
  integer             :: ndims
  integer             :: dim1size, dim2size, dim3size, dim3
  integer             :: numlevels
  integer             :: ngrdpts
  integer             :: bytes_written
  
  call wrf_debug ( DEBUG , 'Entering ext_gr2_write_field for parameter '//&
       VarName)

  !
  ! If DateStr is all 0s, we reset it to StartDate.  For some reason, 
  !   in idealized simulations, StartDate is 0001-01-01_00:00:00 while
  !   the first DateStr is 0000-00-00_00:00:00.  
  !
  if (DateStrIn .eq. '0000-00-00_00:00:00') then
     DateStr = TRIM(StartDate)
  else
     DateStr = DateStrIn
  endif

  !
  ! Check if this is a domain that we haven t seen yet.  If so, add it to 
  !   the list of domains.
  !
  this_domain = 0
  new_domain = .false.
  do idx = 1, max_domain
     if (DomainDesc .eq. domains(idx)) then
        this_domain = idx
     endif
  enddo
  if (this_domain .eq. 0) then
     max_domain = max_domain + 1
     domains(max_domain) = DomainDesc
     this_domain = max_domain
     new_domain = .true.
  endif

  zsize = 1
  xsize = 1
  ysize = 1
  soil_layers = .false.
  fraction = .false.

  ! First, handle then special cases for the boundary data.

  CALL get_dims(MemoryOrder, PatchStart, PatchEnd, ndims, x_start, x_end, &
       y_start, y_end,z_start,z_end)
  xsize = x_end - x_start + 1
  ysize = y_end - y_start + 1
  zsize = z_end - z_start + 1

  do idx = 1, len(MemoryOrder)
     if ((MemoryOrder(idx:idx) .eq. 'Z') .and. &
          (DimNames(idx) .eq. 'soil_layers_stag')) then
        soil_layers = .true.
     else if ((VarName .eq. 'LANDUSEF') .or. (VarName .eq. 'SOILCBOT') .or. &
          (VarName .eq. 'SOILCTOP')) then
        fraction = .true.
     endif
  enddo

  if (zsize .eq. 0) then 
     zsize = 1
  endif

  !
  ! Fill up the variables that hold the vertical coordinate data
  !

  if (VarName .eq. 'ZNU') then
     do idx = 1, zsize
        half_eta(idx) = Field(1,idx,1,1)
     enddo
     half_eta_init = .TRUE.
  endif

  if (VarName .eq. 'ZNW') then
     do idx = 1, zsize
        full_eta(idx) = Field(1,idx,1,1)
     enddo
     full_eta_init = .TRUE.
  endif
  
  if (VarName .eq. 'ZS') then
     do idx = 1, zsize
        soil_depth(idx) = Field(1,idx,1,1)
     enddo
     soil_depth_init = .TRUE.
  endif

  if (VarName .eq. 'DZS') then
     do idx = 1, zsize
        soil_thickness(idx) = Field(1,idx,1,1)
     enddo
     soil_thickness_init = .TRUE.
  endif

  ! 
  ! Check to assure that dimensions are valid
  !

  if ((xsize .lt. 1) .or. (ysize .lt. 1) .or. (zsize .lt. 1)) then
     write(msg,*) 'Cannot output field with memory order: ', &
          MemoryOrder,Varname
     call wrf_message(trim(msg))
     return
  endif
     

  if (fileinfo(DataHandle)%opened .and. fileinfo(DataHandle)%committed) then

     if (StartDate == '') then
        StartDate = DateStr
     endif
     
     CALL geth_idts(DateStr,StartDate,fcst_secs)

     !
     ! If this is a new forecast time, and we have not written the 
     !   last_fcst_secs scalar output yet, then write it here.
     !

     if ((abs(fcst_secs - 0.0) .gt. 0.01) .and. &
          (last_fcst_secs .ge. 0) .and. &
          (abs(fcst_secs - last_fcst_secs) .gt. 0.01) .and. &
          (abs(last_fcst_secs - fileinfo(DataHandle)%last_scalar_time_written) .gt. 0.01) ) then
        call gr2_fill_local_use(DataHandle,scalar_output(DataHandle),&
             "WRF_SCALAR",last_fcst_secs,msg,status)
        if (status .ne. 0) then
           call wrf_message(trim(msg))
           return
        endif
        fileinfo(DataHandle)%last_scalar_time_written = last_fcst_secs
        scalar_output(DataHandle) = ''
     endif

     call get_vert_stag(VarName,Stagger,vert_stag)
     
     do idx = 1, zsize
        call gr2_get_levels(VarName, idx, zsize, soil_layers, vert_stag, &
             fraction, vert_unit1, vert_unit2, vert_sclFctr1, &
             vert_sclFctr2, level1(idx), level2(idx))
     enddo
     
     ! 
     ! Get the center lat/lon for the area being output.  For some cases (such
     !    as for boundary areas, the center of the area is different from the
     !    center of the model grid.
     !
     if (index(Stagger,'X') .le. 0) then
        dom_xsize = full_xsize - 1
     else
        dom_xsize = full_xsize
     endif
     if (index(Stagger,'Y') .le. 0) then
        dom_ysize = full_ysize - 1
     else
        dom_ysize = full_ysize
     endif
     

     CALL get_region_center(MemoryOrder, wrf_projection, center_lat, &
          center_lon, dom_xsize, dom_ysize, dx, dy, proj_central_lon, &
          proj_center_flag, truelat1, truelat2, xsize, ysize, &
          region_center_lat, region_center_lon)
     

     if (ndims .eq. 0) then        ! Scalar quantity

        ALLOCATE(data(1:1,1:1), STAT=istat)

        call gr2_retrieve_data(MemoryOrder, MemoryStart, MemoryEnd, &
             xsize, ysize, zsize, z, FieldType, Field, data)
        write(tmpstr,'(G17.10)')data(1,1)
        CALL gr2_build_string (scalar_output(DataHandle), &
             trim(adjustl(VarName)), tmpstr, 1, Status)

        DEALLOCATE(data)

     else if (ndims .ge. 1) then   ! Vector (1-D) and 2/3 D quantities

        if (ndims .eq. 1) then     ! Handle Vector (1-D) parameters
           dim1size = zsize
           dim2size = 1
           dim3size = 1
        else                       ! Handle 2/3 D parameters
           dim1size = xsize
           dim2size = ysize
           dim3size = zsize
        endif
        
        ALLOCATE(data(1:dim1size,1:dim2size), STAT=istat)

        CALL get_parminfo(VarName, center, subcenter, MasterTblV, &
             LocalTblV, Disc, Category, ParmNum, DecScl, BinScl, status)
        if (status .ne. 0) then
           write(msg,*) 'Could not find parameter for '//   &
                trim(VarName)//'   Skipping output of '//trim(VarName)
           call wrf_message(trim(msg))
           Status =  WRF_GRIB2_ERR_GRIB2MAP
           return
        endif

        VERTDIM : do dim3 = 1, dim3size

           call gr2_retrieve_data(MemoryOrder, MemoryStart, MemoryEnd, xsize, &
                ysize, zsize, dim3, FieldType, Field, data)
        
           ! 
           ! Here, we do any necessary conversions to the data.
           !
           
           ! Potential temperature is sometimes passed in as perturbation 
           !   potential temperature (i.e., POT-300).  Other times (i.e., from 
           !   WRF SI), it is passed in as full potential temperature.
           ! Here, we convert to full potential temperature by adding 300
           !   only if POT < 200 K.
           !
           if (VarName == 'T') then
              if ((data(1,1) < 200) .and. (data(1,1) .ne. 0)) then
                 data = data + 300
              endif
           endif
           
           ! 
           ! For precip, we setup the accumulation period, and output a precip
           !    rate for time-step precip.
           !
           if ((VarName .eq. 'RAINCV') .or. (VarName .eq. 'RAINNCV')) then
              ! Convert time-step precip to precip rate.
              data = data/timestep
              accum_period = 0
           else
              accum_period = 0
           endif
           
           !
           ! Create indicator and identification sections (sections 0 and 1)
           !
           CALL gr2_create_w(StartDate, cgrib, lcgrib, production_status, &
                Disc, center, subcenter, MasterTblV, LocalTblV, ierr, msg)
           if (ierr .ne. 0) then
              call wrf_message(trim(msg))
              Status = WRF_GRIB2_ERR_GRIBCREATE
              return
           endif

           !
           ! Add the grid definition section (section 3) using a 1x1 grid
           !
           call gr2_addgrid_w(cgrib, lcgrib, center_lat, proj_central_lon,  &
                wrf_projection, truelat1, truelat2, xsize, ysize, dx, dy, &
                region_center_lat, region_center_lon, ierr, msg)
           if (ierr .ne. 0) then
              call wrf_message(trim(msg))
              Status = WRF_GRIB2_ERR_ADDGRIB
              return
           endif

           if (ndims .eq. 1) then
              numlevels = zsize
              grib_levels(:) = level1(:)
              ngrdpts = zsize
           else
              numlevels = 2
              grib_levels(1) = level1(dim3)
              grib_levels(2) = level2(dim3)
              ngrdpts = xsize*ysize
           endif
           
           !
           ! Add the Product Definition, Data representation, bitmap 
           !      and data sections (sections 4-7)
           !
           
           call gr2_addfield_w(cgrib, lcgrib, VarName, Category, ParmNum, &
                DecScl, BinScl, fcst_secs, vert_unit1, vert_unit2, &
                vert_sclFctr1, vert_sclFctr2, numlevels, &
                grib_levels, ngrdpts,  background_proc_id, forecast_proc_id, &
                compression, data, ierr, msg)
           if (ierr .eq. 11) then
              write(msg,'(A,I7,A)') 'WARNING: decimal scale for field '//&
                   trim(VarName)//' at level ',grib_levels(1),&
                   ' was reduced to fit field into 24 bits.  '//&
                   ' Some precision may be lost!'//&
                   '     To prevent this message, reduce decimal scale '//&
                   'factor in '//trim(mapfilename)
              call wrf_message(trim(msg))
           else if (ierr .eq. 12) then
              write(msg,'(A,I7,A)') 'WARNING: binary scale for field '//&
                   trim(VarName)//' at level ',grib_levels(1), &
                   ' was reduced to fit field into 24 bits.  '//&
                   ' Some precision may be lost!'//&
                   '     To prevent this message, reduce binary scale '//&
                   'factor in '//trim(mapfilename)
              call wrf_message(trim(msg))
           else if (ierr .ne. 0) then
              call wrf_message(trim(msg))
              Status = WRF_GRIB2_ERR_ADDFIELD
              return
           endif

           !
           ! Close out the message
           !
           
           call gribend(cgrib,lcgrib,lengrib,ierr)
           if (ierr .ne. 0) then
              write(msg,*) 'gribend failed with ierr: ',ierr     
              call wrf_message(trim(msg))
              Status = WRF_GRIB2_ERR_GRIBEND
              return
           endif

           ! 
           ! Write the data to the file
           !
           
!           call write_file_n(fileinfo(DataHandle)%FileFd, cgrib, lengrib, ierr)
           call bawrite(DataHandle, -1, lengrib, bytes_written, cgrib)
           if (bytes_written .ne. lengrib) then
              write(msg,*) '1 Error writing cgrib to file, wrote: ', &
                   bytes_written, ' bytes.  Tried to write ', lengrib, ' bytes'
              call wrf_message(trim(msg))
              Status = WRF_GRIB2_ERR_WRITE
              return
           endif

        ENDDO VERTDIM
        
        DEALLOCATE(data)

     endif

     last_fcst_secs = fcst_secs

  endif

  deallocate(data, STAT = istat)

  Status = WRF_NO_ERR

  call wrf_debug ( DEBUG , 'Leaving ext_gr2_write_field')

  RETURN
END SUBROUTINE ext_gr2_write_field

!*****************************************************************************

SUBROUTINE ext_gr2_read_field ( DataHandle , DateStr , VarName , Field , &
     FieldType , Comm , IOComm, DomainDesc , MemoryOrder , Stagger ,     &
     DimNames , DomainStart , DomainEnd , MemoryStart , MemoryEnd ,      &
     PatchStart , PatchEnd ,  Status )

  USE gr2_data_info
  USE grib_mod
  IMPLICIT NONE  
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER                       ,intent(in)       :: DataHandle 
  CHARACTER*(*)                 ,intent(in)       :: DateStr
  CHARACTER*(*)                 ,intent(in)       :: VarName
  integer                       ,intent(inout)    :: FieldType
  integer                       ,intent(inout)    :: Comm
  integer                       ,intent(inout)    :: IOComm
  integer                       ,intent(inout)    :: DomainDesc
  character*(*)                 ,intent(inout)    :: MemoryOrder
  character*(*)                 ,intent(inout)    :: Stagger
  character*(*) , dimension (*) ,intent(inout)    :: DimNames
  integer ,dimension(*)         ,intent(inout)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(inout)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(inout)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)      :: Status
  INTEGER                       ,intent(out)      :: Field(*)
  integer                       :: xsize,ysize,zsize
  integer                       :: x_start,x_end,y_start,y_end,z_start,z_end
  integer                       :: ndims
  character (len=1000)          :: Value
  character (maxMsgSize)        :: msg
  integer                       :: ierr
  real                          :: Data
  integer                       :: center, subcenter, MasterTblV, &
       LocalTblV, Disc, Category, ParmNum, DecScl, BinScl
  integer                       :: dim1size,dim2size,dim3size,dim3

  integer :: idx
  integer :: fields_to_skip
  integer :: JIDS(JIDSSIZE), JPDTN, JPDT(JPDTSIZE), JGDTN, &
       JGDT(JGDTSIZE)
  logical :: UNPACK
  type(gribfield) :: gfld
  logical                                      :: soil_layers, fraction
  logical                                      :: vert_stag = .false.
  integer                                      :: vert_unit1, vert_unit2
  integer                                      :: vert_sclFctr1, vert_sclFctr2
  integer                                      :: level1, level2
  integer                                      :: di
  real                                         :: tmpreal

  call wrf_debug ( DEBUG , 'Entering ext_gr2_read_field'//fileinfo(DataHandle)%DataFile)
  
  CALL get_dims(MemoryOrder, PatchStart, PatchEnd, ndims, x_start, x_end, &
       y_start, y_end,z_start,z_end)
  xsize = x_end - x_start + 1
  ysize = y_end - y_start + 1
  zsize = z_end - z_start + 1

  ! 
  ! Check to assure that dimensions are valid
  !

  if ((xsize .lt. 1) .or. (ysize .lt. 1) .or. (zsize .lt. 1)) then
     write(msg,*) 'Cannot retrieve field with memory order: ', &
          MemoryOrder,Varname
     Status = WRF_GRIB2_ERR_READ
     call wrf_message(trim(msg))
     return
  endif
     

  if (ndims .eq. 0) then    ! Scalar quantity

     call gr2_get_metadata_value(scalar_input(DataHandle),trim(VarName),&
          Value,ierr)
     if (ierr /= 0) then
        Status = WRF_GRIB2_ERR_READ
        CALL wrf_message ( &
             "gr2_get_metadata_value failed for Scalar variable "//&
             trim(VarName))
        return
     endif

     READ(Value,*,IOSTAT=ierr)Data
     if (ierr .ne. 0) then
        CALL wrf_message("Reading data from "//trim(VarName)//" failed")
        Status = WRF_GRIB2_ERR_READ
        return
     endif

     if (FieldType .eq. WRF_INTEGER) then
        Field(1:1) = data
     else if ((FieldType .eq. WRF_REAL) .or. (FieldType .eq. WRF_DOUBLE)) then
        Field(1:1) = TRANSFER(data,Field(1),1)
     else
        write (msg,*)'Reading of type ',FieldType,'from grib data not supported, not reading ',VarName
        call wrf_message(msg)
     endif

  else if (ndims .ge. 1) then   ! Vector (1-D) and 2/3 D quantities
     
     if (ndims .eq. 1) then     ! Handle Vector (1-D) parameters
        dim1size = zsize
        dim2size = 1
        dim3size = 1
     else                       ! Handle 2/3 D parameters
        dim1size = xsize
        dim2size = ysize
        dim3size = zsize
     endif
     
     CALL get_parminfo(VarName, center, subcenter, MasterTblV, &
          LocalTblV, Disc, Category, ParmNum, DecScl, BinScl, status)
     if (status .ne. 0) then
        write(msg,*) 'Could not find parameter for '//   &
             trim(VarName)//'   Skipping output of '//trim(VarName)
        call wrf_message(trim(msg))
        Status =  WRF_GRIB2_ERR_GRIB2MAP
        return
     endif
     
     CALL get_vert_stag(VarName,Stagger,vert_stag)
     CALL get_soil_layers(VarName,soil_layers)

     VERTDIM : do dim3 = 1, dim3size

        fields_to_skip = 0

        !
        ! First, set all values to wild, then specify necessary values
        !
        call gr2_g2lib_wildcard(JIDS, JPDT, JGDT)

        JIDS(1) = center
        JIDS(2) = subcenter
        JIDS(3) = MasterTblV
        JIDS(4) = LocalTblV
        JIDS(5) = 1           ! Indicates that time is "Start of Forecast"
        
        READ (StartDate,'(I4.4,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2)') &
             (JIDS(idx),idx=6,11)
        JIDS(13) = 1          ! Type of processed data(1 for forecast products)
        
        JPDT(1) = Category
        JPDT(2) = ParmNum
        JPDT(3) = 2           ! Generating process id

        CALL geth_idts(DateStr,StartDate,tmpreal)  ! Forecast time 
        
        JPDT(9) = NINT(tmpreal)

        if (ndims .eq. 1) then
           jpdtn = 1000       ! Product definition tmplate (1000 for cross-sxn)
        else
           call gr2_get_levels(VarName, dim3, dim3size, soil_layers, &
                vert_stag, .false., vert_unit1, vert_unit2, vert_sclFctr1, &
                vert_sclFctr2, level1, level2)
           
           jpdtn = 0          ! Product definition template (0 for horiz grid)
           JPDT(10) = vert_unit1     ! Type of first surface
           JPDT(11) = vert_sclFctr1  ! Scale factor first surface
           JPDT(12) = level1         ! First surface
           JPDT(13) = vert_unit2     ! Type of second surface
           JPDT(14) = vert_sclFctr2  ! Scale factor second surface
           JPDT(15) = level2         ! Second fixed surface
        endif

        JGDTN    = -1    ! Indicates that any Grid Display Template is a match
        
        UNPACK   = .TRUE.! Unpack bitmap and data values
        
        fields_to_skip = 0
        CALL GETGB2(DataHandle, 0, fields_to_skip, &
             fileinfo(DataHandle)%recnum+1, &
             Disc, JIDS, JPDTN, JPDT, JGDTN, JGDT, UNPACK, &
             fileinfo(DataHandle)%recnum, gfld, status)
        if (status .eq. 99) then
           write(msg,*)'Could not find data for field '//trim(VarName)//&
                ' in file '//trim(fileinfo(DataHandle)%DataFile)
           call wrf_message(trim(msg))
           Status = WRF_GRIB2_ERR_READ
           return
        else if (status .ne. 0) then
           write(msg,*)'Retrieving data field '//trim(VarName)//' failed 2.',status,dim3,DataHandle
           call wrf_message(trim(msg))
           Status = WRF_GRIB2_ERR_READ
           return
        endif

        if(FieldType == WRF_DOUBLE) then
           di = 2
        else 
           di = 1
        endif

        ! 
        ! Here, we do any necessary conversions to the data.
        !
        ! The WRF executable (wrf.exe) expects perturbation potential
        !   temperature.  However, real.exe expects full potential T.
        ! So, if the program is WRF, subtract 300 from Potential Temperature 
        !   to get perturbation potential temperature.
        !
        if (VarName == 'T') then
           if ( &
                (InputProgramName .eq. 'REAL_EM') .or. &
                (InputProgramName .eq. 'IDEAL') .or. &
                (InputProgramName .eq. 'NDOWN_EM')) then
              gfld%fld = gfld%fld - 300
           endif
        endif


        if (ndims .eq. 1) then
           CALL Transpose1D_grib(MemoryOrder, di, FieldType, Field, &
                MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), &
                MemoryStart(3), MemoryEnd(3), &
                gfld%fld, zsize)
        else
           CALL Transpose_grib(MemoryOrder, di, FieldType, Field, &
                MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), &
                MemoryStart(3), MemoryEnd(3), &
                gfld%fld, dim3, ysize,xsize)
        endif

        call gf_free(gfld)
        
     enddo VERTDIM
  endif

  Status = WRF_NO_ERR


  call wrf_debug ( DEBUG , 'Leaving ext_gr2_read_field')

  RETURN
END SUBROUTINE ext_gr2_read_field

!*****************************************************************************

SUBROUTINE ext_gr2_get_next_var ( DataHandle, VarName, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_next_var')

  Status = WRF_WARN_NOOP

  RETURN
END SUBROUTINE ext_gr2_get_next_var

!*****************************************************************************

subroutine ext_gr2_end_of_frame(DataHandle, Status)

  USE gr2_data_info
  implicit none
#include "wrf_status_codes.h"
  integer               ,intent(in)     :: DataHandle
  integer               ,intent(out)    :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr2_end_of_frame')

  Status = WRF_WARN_NOOP

  return
end subroutine ext_gr2_end_of_frame

!*****************************************************************************

SUBROUTINE ext_gr2_iosync ( DataHandle, Status )

  USE gr2_data_info  
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  integer                     :: ierror

  call wrf_debug ( DEBUG , 'Entering ext_gr2_iosync')

  Status = WRF_NO_ERR
  if (DataHandle .GT. 0) then
     CALL flush_file(fileinfo(DataHandle)%FileFd)
  else
     Status = WRF_WARN_TOO_MANY_FILES
  endif

  RETURN
END SUBROUTINE ext_gr2_iosync

!*****************************************************************************

SUBROUTINE ext_gr2_inquire_filename ( DataHandle, FileName , FileStat, &
     Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStat
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER *80   SysDepInfo

  call wrf_debug ( DEBUG , 'Entering ext_gr2_inquire_filename')

  FileName = fileinfo(DataHandle)%DataFile 

  if ((DataHandle .ge. firstFileHandle) .and. &
       (DataHandle .le. maxFileHandles)) then
     FileStat = fileinfo(DataHandle)%FileStatus
  else
     FileStat = WRF_FILE_NOT_OPENED
  endif
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_gr2_inquire_filename

!*****************************************************************************

SUBROUTINE ext_gr2_get_var_info ( DataHandle , VarName , NDim , &
     MemoryOrder , Stagger , DomainStart , DomainEnd , WrfType, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: VarName
  integer               ,intent(out)    :: NDim
  character*(*)         ,intent(out)    :: MemoryOrder
  character*(*)         ,intent(out)    :: Stagger
  integer ,dimension(*) ,intent(out)    :: DomainStart, DomainEnd
  integer               ,intent(out)    :: WrfType
  integer               ,intent(out)    :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_info')

  MemoryOrder = ""
  Stagger = ""
  DomainStart(1) = 0
  DomainEnd(1) = 0
  WrfType = 0
  NDim = 0

  CALL wrf_message('ext_gr2_get_var_info not supported for grib version2 data')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_gr2_get_var_info

!*****************************************************************************

SUBROUTINE ext_gr2_set_time ( DataHandle, DateStr, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  integer       :: found_time
  integer       :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_set_time')

  found_time = 0
  do idx = 1,fileinfo(DataHandle)%NumberTimes
     if (fileinfo(DataHandle)%Times(idx) == DateStr) then
        found_time = 1
        fileinfo(DataHandle)%CurrentTime = idx
     endif
  enddo
  if (found_time == 0) then 
     Status = WRF_WARN_TIME_NF
  else
     Status = WRF_NO_ERR
  endif

  RETURN
END SUBROUTINE ext_gr2_set_time

!*****************************************************************************

SUBROUTINE ext_gr2_get_next_time ( DataHandle, DateStr, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(OUT) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_next_time')

  if (fileinfo(DataHandle)%CurrentTime == fileinfo(DataHandle)%NumberTimes) then
     Status = WRF_WARN_TIME_EOF
  else
     fileinfo(DataHandle)%CurrentTime = fileinfo(DataHandle)%CurrentTime + 1
     DateStr = fileinfo(DataHandle)%Times(fileinfo(DataHandle)%CurrentTime)
     Status = WRF_NO_ERR
  endif

  call wrf_debug ( DEBUG , 'Leaving ext_gr2_get_next_time, got time '//DateStr)

  RETURN
END SUBROUTINE ext_gr2_get_next_time

!*****************************************************************************

SUBROUTINE ext_gr2_get_previous_time ( DataHandle, DateStr, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_previous_time')

  if (fileinfo(DataHandle)%CurrentTime <= 0) then
     Status = WRF_WARN_TIME_EOF
  else
     fileinfo(DataHandle)%CurrentTime = fileinfo(DataHandle)%CurrentTime - 1
     DateStr = fileinfo(DataHandle)%Times(fileinfo(DataHandle)%CurrentTime)
     Status = WRF_NO_ERR
  endif

  RETURN
END SUBROUTINE ext_gr2_get_previous_time

!******************************************************************************
!* Start of get_var_ti_* routines
!******************************************************************************

SUBROUTINE ext_gr2_get_var_ti_real ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)    :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real ,          INTENT(OUT)   :: Data(*)
  INTEGER ,       INTENT(IN)    :: Count
  INTEGER ,       INTENT(OUT)   :: OutCount
  INTEGER ,       INTENT(OUT)   :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER(len=100)  :: Value

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_ti_real')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx
 
  RETURN
END SUBROUTINE ext_gr2_get_var_ti_real 

!*****************************************************************************

SUBROUTINE ext_gr2_get_var_ti_real8 ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)      :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real*8 ,        INTENT(OUT)     :: Data(*)
  INTEGER ,       INTENT(IN)      :: Count
  INTEGER ,       INTENT(OUT)     :: OutCount
  INTEGER ,       INTENT(OUT)     :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(100)  :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_ti_real8')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx
 
  RETURN
END SUBROUTINE ext_gr2_get_var_ti_real8 

!*****************************************************************************

SUBROUTINE ext_gr2_get_var_ti_double ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )
  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(100)  :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_ti_double')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_var_ti_double

!*****************************************************************************

SUBROUTINE ext_gr2_get_var_ti_integer ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)       :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  integer ,       INTENT(OUT)      :: Data(*)
  INTEGER ,       INTENT(IN)       :: Count
  INTEGER ,       INTENT(OUT)      :: OutCount
  INTEGER ,       INTENT(OUT)      :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_ti_integer')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_var_ti_integer 

!*****************************************************************************

SUBROUTINE ext_gr2_get_var_ti_logical ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)       :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  logical ,       INTENT(OUT)      :: Data(*)
  INTEGER ,       INTENT(IN)       :: Count
  INTEGER ,       INTENT(OUT)      :: OutCount
  INTEGER ,       INTENT(OUT)      :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(100) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_ti_logical')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_var_ti_logical 

!*****************************************************************************

SUBROUTINE ext_gr2_get_var_ti_char ( DataHandle,Element,  Varname, Data,  &
     Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER       :: stat

  Status = WRF_NO_ERR
  
  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_ti_char')

  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(Element), Data, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  RETURN
END SUBROUTINE ext_gr2_get_var_ti_char 

!******************************************************************************
!* End of get_var_ti_* routines
!******************************************************************************


!******************************************************************************
!* Start of put_var_ti_* routines
!******************************************************************************

SUBROUTINE ext_gr2_put_var_ti_real ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real ,          INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_ti_real')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr2_build_string (ti_output(DataHandle), &
          trim(VarName)//';'//trim(Element), tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_var_ti_real 

!*****************************************************************************

SUBROUTINE ext_gr2_put_var_ti_double ( DataHandle,Element,  Varname, Data, &
     Count,  Status )
  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_ti_double')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (ti_output(DataHandle), &
          trim(VarName)//';'//trim(Element), tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr2_put_var_ti_double

!*****************************************************************************

SUBROUTINE ext_gr2_put_var_ti_real8 ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real*8 ,        INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_ti_real8')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (ti_output(DataHandle), &
          trim(VarName)//';'//trim(Element), tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr2_put_var_ti_real8 

!*****************************************************************************

SUBROUTINE ext_gr2_put_var_ti_integer ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  integer ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_ti_integer')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (ti_output(DataHandle), &
          trim(VarName)//';'//trim(Element), tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr2_put_var_ti_integer 

!*****************************************************************************

SUBROUTINE ext_gr2_put_var_ti_logical ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  logical ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_ti_logical')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (ti_output(DataHandle), &
          trim(Varname)//';'//trim(Element), tmpstr, Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr2_put_var_ti_logical 

!*****************************************************************************

SUBROUTINE ext_gr2_put_var_ti_char ( DataHandle,Element,  Varname, Data,  &
     Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER(len=*) :: Element
  CHARACTER(len=*) :: VarName 
  CHARACTER(len=*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy
  INTEGER                     :: Count
  CHARACTER(len=1000) :: tmpstr(1)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_ti_char')

  if (fileinfo(DataHandle)%committed) then

     write(tmpstr(1),*)trim(Data)

     CALL gr2_build_string (ti_output(DataHandle), &
          trim(VarName)//';'//trim(Element), tmpstr, 1, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_var_ti_char 

!******************************************************************************
!* End of put_var_ti_* routines
!******************************************************************************

!******************************************************************************
!* Start of get_var_td_* routines
!******************************************************************************

SUBROUTINE ext_gr2_get_var_td_double ( DataHandle,Element,  DateStr, &
     Varname, Data, Count, Outcount, Status )
  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_td_double')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(DateStr)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

RETURN
END SUBROUTINE ext_gr2_get_var_td_double

!*****************************************************************************

SUBROUTINE ext_gr2_get_var_td_real ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real ,          INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_td_real')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(DateStr)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_var_td_real 

!*****************************************************************************

SUBROUTINE ext_gr2_get_var_td_real8 ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real*8 ,        INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_td_real8')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(DateStr)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_var_td_real8 

!*****************************************************************************

SUBROUTINE ext_gr2_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  integer ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_td_integer')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(DateStr)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_var_td_integer 

!*****************************************************************************

SUBROUTINE ext_gr2_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )
  
  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  logical ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_td_logical')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(DateStr)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_var_td_logical 

!*****************************************************************************

SUBROUTINE ext_gr2_get_var_td_char ( DataHandle,Element,  DateStr,Varname, &
     Data,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER       :: stat

  Status = WRF_NO_ERR
  
  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_var_td_char')

  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(VarName)//';'//trim(DateStr)//';'//trim(Element), Data, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(VarName)//';'//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  RETURN
END SUBROUTINE ext_gr2_get_var_td_char 

!******************************************************************************
!* End of get_var_td_* routines
!******************************************************************************

!******************************************************************************
!* Start of put_var_td_* routines
!******************************************************************************

SUBROUTINE ext_gr2_put_var_td_double ( DataHandle, Element, DateStr, Varname, &
     Data, Count,  Status )
  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_td_double')


  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr2_build_string (td_output(DataHandle), &
          trim(Varname)//';'//trim(DateStr)//';'//trim(Element), &
          tmpstr, Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr2_put_var_td_double

!*****************************************************************************

SUBROUTINE ext_gr2_put_var_td_integer ( DataHandle,Element,  DateStr, &
     Varname, Data, Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  integer ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_td_integer')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (td_output(DataHandle), &
          trim(Varname)//';'//trim(DateStr)//';'//trim(Element), &
          tmpstr, Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr2_put_var_td_integer 

!*****************************************************************************

SUBROUTINE ext_gr2_put_var_td_real ( DataHandle,Element,  DateStr,Varname, &
     Data, Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real ,          INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_td_real')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (td_output(DataHandle), &
          trim(Varname)//';'//trim(DateStr)//';'//trim(Element), &
          tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_var_td_real 

!*****************************************************************************

SUBROUTINE ext_gr2_put_var_td_real8 ( DataHandle,Element,  DateStr,Varname, &
     Data, Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real*8 ,        INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_td_real8')

  if (fileinfo(DataHandle)%committed) then
     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (td_output(DataHandle), &
          trim(Varname)//';'//trim(DateStr)//';'//trim(Element), &
          tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr2_put_var_td_real8 

!*****************************************************************************

SUBROUTINE ext_gr2_put_var_td_logical ( DataHandle,Element,  DateStr, &
     Varname, Data, Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  logical ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_td_logical')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr2_build_string (td_output(DataHandle), &
          trim(Varname)//';'//trim(DateStr)//';'//trim(Element), &
          tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_var_td_logical 

!*****************************************************************************

SUBROUTINE ext_gr2_put_var_td_char ( DataHandle,Element,  DateStr,Varname, &
     Data,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_var_td_char')

  if (fileinfo(DataHandle)%committed) then

     write(tmpstr(idx),*)Data

     CALL gr2_build_string (td_output(DataHandle), &
          trim(Varname)//';'//trim(DateStr)//';'//trim(Element), &
          tmpstr, 1, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_var_td_char 

!******************************************************************************
!* End of put_var_td_* routines
!******************************************************************************


!******************************************************************************
!* Start of get_dom_ti_* routines
!******************************************************************************

SUBROUTINE ext_gr2_get_dom_ti_real ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real ,          INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Outcount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_ti_real')

  Status = WRF_NO_ERR

  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_dom_ti_real 

!*****************************************************************************

SUBROUTINE ext_gr2_get_dom_ti_real8 ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real*8 ,        INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_ti_real8')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx
 
  RETURN
END SUBROUTINE ext_gr2_get_dom_ti_real8 

!*****************************************************************************

SUBROUTINE ext_gr2_get_dom_ti_integer ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  integer ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE
  
  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_ti_integer Element: '//Element)

  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = Count
 
  RETURN
END SUBROUTINE ext_gr2_get_dom_ti_integer 

!*****************************************************************************

SUBROUTINE ext_gr2_get_dom_ti_logical ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  logical ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_ti_logical')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx
 
  RETURN
END SUBROUTINE ext_gr2_get_dom_ti_logical 

!*****************************************************************************

SUBROUTINE ext_gr2_get_dom_ti_char ( DataHandle,Element,   Data,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER       :: stat
  INTEGER       :: endchar

  Status = WRF_NO_ERR
  
  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_ti_char')

  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(Element), Data, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  RETURN
END SUBROUTINE ext_gr2_get_dom_ti_char 

!*****************************************************************************

SUBROUTINE ext_gr2_get_dom_ti_double ( DataHandle,Element,   Data, Count, &
     Outcount, Status )
  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_ti_double')

  Status = WRF_NO_ERR
   
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx
 
RETURN
END SUBROUTINE ext_gr2_get_dom_ti_double

!******************************************************************************
!* End of get_dom_ti_* routines
!******************************************************************************


!******************************************************************************
!* Start of put_dom_ti_* routines
!******************************************************************************

SUBROUTINE ext_gr2_put_dom_ti_real ( DataHandle,Element,   Data, Count,  &
     Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real ,          INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy
  CHARACTER(len=1000) :: tmpstr(1000)
  character(len=2)    :: lf
  integer             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_ti_real')

  if (Element .eq. 'DX') then
     dx = Data(1)/1000.
  endif
  if (Element .eq. 'DY') then
     dy = Data(1)/1000.
  endif
  if (Element .eq. 'CEN_LAT') then
     center_lat = Data(1)
  endif
  if (Element .eq. 'CEN_LON') then
     center_lon = Data(1)
  endif  
  if (Element .eq. 'TRUELAT1') then
     truelat1 = Data(1)
  endif
  if (Element .eq. 'TRUELAT2') then
     truelat2 = Data(1)
  endif
  if (Element == 'STAND_LON') then
     proj_central_lon = Data(1)
  endif
  if (Element == 'DT') then
     timestep = Data(1)
  endif

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (ti_output(DataHandle), Element, &
          tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_dom_ti_real 

!*****************************************************************************

SUBROUTINE ext_gr2_put_dom_ti_real8 ( DataHandle,Element,   Data, Count,  &
     Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real*8 ,        INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_ti_real8')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (ti_output(DataHandle), Element, &
          tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_dom_ti_real8 

!*****************************************************************************

SUBROUTINE ext_gr2_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  &
     Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  INTEGER ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx


  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_ti_integer')

  if (Element == 'WEST-EAST_GRID_DIMENSION') then
     full_xsize = Data(1)
  else if (Element == 'SOUTH-NORTH_GRID_DIMENSION') then
     full_ysize = Data(1)
  else if (Element == 'MAP_PROJ') then
     wrf_projection = Data(1)
  else if (Element == 'BACKGROUND_PROC_ID') then
     background_proc_id = Data(1)
  else if (Element == 'FORECAST_PROC_ID') then
     forecast_proc_id = Data(1)
  else if (Element == 'PRODUCTION_STATUS') then
     production_status = Data(1)
  else if (Element == 'COMPRESSION') then
     compression = Data(1)
  endif

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (ti_output(DataHandle), Element, &
          tmpstr, Count, Status)

  endif

  call wrf_debug ( DEBUG , 'Leaving ext_gr2_put_dom_ti_integer')

  RETURN
END SUBROUTINE ext_gr2_put_dom_ti_integer 

!*****************************************************************************

SUBROUTINE ext_gr2_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  &
     Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  logical ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_ti_logical')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (ti_output(DataHandle), Element, &
          tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_dom_ti_logical 

!*****************************************************************************

SUBROUTINE ext_gr2_put_dom_ti_char ( DataHandle,Element,   Data,  &
     Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*),     INTENT(IN)  :: Data
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy
  CHARACTER(len=1000) :: tmpstr

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_ti_char')

  if (Element .eq. 'START_DATE') then

     !
     ! This is just a hack to fix a problem when outputting restart.  WRF
     !   outputs both the initialization time and the time of the restart 
     !   as the StartDate.  So, we ll just take the earliest.
     !
     if ((StartDate .eq. '') .or. (Data .le. StartDate)) then
        StartDate = Data
     endif

  endif

  if (fileinfo(DataHandle)%committed) then

     write(tmpstr,*)trim(Data)
     
     CALL gr2_build_string (ti_output(DataHandle), Element, &
          tmpstr, 1, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_dom_ti_char

!*****************************************************************************

SUBROUTINE ext_gr2_put_dom_ti_double ( DataHandle,Element, Data, Count, &
     Status )
  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_ti_double')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr2_build_string (ti_output(DataHandle), Element, &
          tmpstr, Count, Status)

  endif
  
  RETURN
END SUBROUTINE ext_gr2_put_dom_ti_double

!******************************************************************************
!* End of put_dom_ti_* routines
!******************************************************************************


!******************************************************************************
!* Start of get_dom_td_* routines
!******************************************************************************

SUBROUTINE ext_gr2_get_dom_td_real ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real ,          INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_td_real')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(DateStr)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_dom_td_real 

!*****************************************************************************

SUBROUTINE ext_gr2_get_dom_td_real8 ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real*8 ,        INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_td_real8')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(DateStr)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_dom_td_real8 

!*****************************************************************************

SUBROUTINE ext_gr2_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  integer ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_td_integer')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(DateStr)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_dom_td_integer 

!*****************************************************************************

SUBROUTINE ext_gr2_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  logical ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_td_logical')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(DateStr)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

  RETURN
END SUBROUTINE ext_gr2_get_dom_td_logical 

!*****************************************************************************

SUBROUTINE ext_gr2_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  &
     Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER       :: stat

  Status = WRF_NO_ERR
  
  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_td_char')

  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(DateStr)//';'//trim(Element), Data, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  RETURN
END SUBROUTINE ext_gr2_get_dom_td_char 

!*****************************************************************************

SUBROUTINE ext_gr2_get_dom_td_double ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )
  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER          :: idx
  INTEGER          :: stat
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr2_get_dom_td_double')

  Status = WRF_NO_ERR
  
  CALL gr2_get_metadata_value(global_input(DataHandle), &
       trim(DateStr)//';'//trim(Element), Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "gr2_get_metadata_value failed for "//trim(DateStr)//';'//trim(Element))
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  READ(Value,*,IOSTAT=stat)(Data(idx),idx=1,Count)
  if (stat .ne. 0) then
     CALL wrf_message("Reading data from"//Value//"failed")
     Status = WRF_WARN_COUNT_TOO_LONG
     RETURN
  endif
  Outcount = idx

RETURN
END SUBROUTINE ext_gr2_get_dom_td_double

!******************************************************************************
!* End of get_dom_td_* routines
!******************************************************************************


!******************************************************************************
!* Start of put_dom_td_* routines
!******************************************************************************


SUBROUTINE ext_gr2_put_dom_td_real8 ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real*8 ,        INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_td_real8')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr2_build_string (td_output(DataHandle), &
          trim(DateStr)//';'//trim(Element), tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_dom_td_real8 

!*****************************************************************************

SUBROUTINE ext_gr2_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  integer ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_td_integer')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (td_output(DataHandle), &
          trim(DateStr)//';'//trim(Element), tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_dom_td_integer

!*****************************************************************************

SUBROUTINE ext_gr2_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  logical ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_td_logical')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (td_output(DataHandle), &
          trim(DateStr)//';'//trim(Element), tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_dom_td_logical

!*****************************************************************************

SUBROUTINE ext_gr2_put_dom_td_char ( DataHandle,Element, DateStr,  Data, &
     Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER(len=*), INTENT(IN)  :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1)

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_td_char')

  if (fileinfo(DataHandle)%committed) then

     write(tmpstr(1),*)Data

     CALL gr2_build_string (td_output(DataHandle), &
          trim(DateStr)//';'//trim(Element), tmpstr, &
          1, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_dom_td_char 

!*****************************************************************************

SUBROUTINE ext_gr2_put_dom_td_double ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )
  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_td_double')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr2_build_string (td_output(DataHandle), &
          trim(DateStr)//';'//trim(Element), tmpstr, &
          Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr2_put_dom_td_double

!*****************************************************************************

SUBROUTINE ext_gr2_put_dom_td_real ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real ,          INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr2_put_dom_td_real')

  if (fileinfo(DataHandle)%committed) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr2_build_string (td_output(DataHandle), &
          trim(DateStr)//';'//trim(Element), tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr2_put_dom_td_real 


!******************************************************************************
!* End of put_dom_td_* routines
!******************************************************************************


SUBROUTINE gr2_get_new_handle(DataHandle)
  USE gr2_data_info
  IMPLICIT NONE
  
  INTEGER ,       INTENT(OUT)  :: DataHandle
  INTEGER :: i

  DataHandle = -1
  do i=firstFileHandle, maxFileHandles
     if (.NOT. fileinfo(i)%used) then
        DataHandle = i
        fileinfo(i)%used = .true.
        exit
     endif
  enddo

  RETURN
END SUBROUTINE gr2_get_new_handle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!*****************************************************************************

SUBROUTINE gr2_retrieve_data (MemoryOrder, MemoryStart, MemoryEnd, xsize, ysize, &
     zsize, z, FieldType, Field, data)
  
  IMPLICIT NONE

#include "wrf_io_flags.h"

  character*(*)                 ,intent(in)    :: MemoryOrder
  integer                       ,intent(in)    :: xsize, ysize, zsize
  integer                       ,intent(in)    :: z
  integer,dimension(*)          ,intent(in)    :: MemoryStart, MemoryEnd
  integer                       ,intent(in)    :: FieldType
  real                          ,intent(in),       &
       dimension( 1:1,MemoryStart(1):MemoryEnd(1), &
       MemoryStart(2):MemoryEnd(2), &
       MemoryStart(3):MemoryEnd(3) )           :: Field
  real   ,dimension(1:xsize,1:ysize),intent(inout) :: data

  integer                                      :: x, y, idx
  integer, dimension(:,:),   pointer           :: mold
  integer                                      :: istat
  integer                                      :: dim1
  
  ALLOCATE(mold(1:xsize,1:ysize), STAT=istat)
  if (istat .ne. 0) then
     print *,'Could not allocate space for mold, returning'
     return
  endif

  !
  ! Set the size of the first dimension of the data array (dim1) to xsize.  
  !    If the MemoryOrder is Z or z, dim1 is overridden below.
  !
  dim1 = xsize

  SELECT CASE (MemoryOrder)
  CASE ('XYZ')
     data = Field(1,1:xsize,1:ysize,z)
  CASE ('C')
     data = Field(1,1:xsize,1:ysize,z)
  CASE ('XZY')
     data = Field(1,1:xsize,z,1:ysize)
  CASE ('YXZ')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,y,x,z)
        enddo
     enddo
  CASE ('YZX')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,y,z,x)
        enddo
     enddo
  CASE ('ZXY')
     data = Field(1,z,1:xsize,1:ysize)
  CASE ('ZYX')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,z,y,x)
        enddo
     enddo
  CASE ('XY')
     data = Field(1,1:xsize,1:ysize,1)
  CASE ('YX')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,y,x,1)
        enddo
     enddo
     
  CASE ('XSZ')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,y,z,x)
        enddo
     enddo
  CASE ('XEZ')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,y,z,x)
        enddo
     enddo
  CASE ('YSZ')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,x,z,y)
        enddo
     enddo
  CASE ('YEZ')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,x,z,y)
        enddo
     enddo
     
  CASE ('XS')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,y,x,1)
        enddo
     enddo
  CASE ('XE')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,y,x,1)
        enddo
     enddo
  CASE ('YS')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,x,y,1)
        enddo
     enddo
  CASE ('YE')
     do x = 1,xsize
        do y = 1,ysize
           data(x,y) = Field(1,x,y,1)
        enddo
     enddo
  CASE ('Z')
     data(1:zsize,1) = Field(1,1:zsize,1,1)
     dim1 = zsize
  CASE ('z')
     data(1:zsize,1) = Field(1,zsize:1,1,1)
     dim1 = zsize
  CASE ('0')
     data(1,1) = Field(1,1,1,1)
  END SELECT
  
  ! 
  ! Here, we convert any integer fields to real
  !
  if (FieldType == WRF_INTEGER) then
     mold = 0
     do idx=1,dim1
        !
        ! The parentheses around data(idx,:) are needed in order
        !   to fix a bug with transfer with the xlf compiler on NCARs
        !   IBM (bluesky).
        !
        data(idx,:)=transfer((data(idx,:)),mold)
     enddo
  endif

  deallocate(mold)
  
  return

end subroutine gr2_retrieve_data

!*****************************************************************************

SUBROUTINE gr2_get_levels(VarName, zidx, zsize, soil_layers, vert_stag, &
     fraction, vert_unit1, vert_unit2, vert_sclFctr1, vert_sclFctr2, &
     level1, level2)

  use gr2_data_info
  IMPLICIT NONE

  integer :: zidx
  integer :: zsize
  logical :: soil_layers
  logical :: vert_stag
  logical :: fraction
  integer :: vert_unit1, vert_unit2
  integer :: vert_sclFctr1, vert_sclFctr2
  integer :: level1
  integer :: level2
  character (LEN=*) :: VarName

  ! Setup vert_unit, and vertical levels in grib units

  if ((VarName .eq. 'LANDUSEF') .or. (VarName .eq. 'SOILCTOP') &
       .or. (VarName .eq. 'SOILCBOT')) then
     vert_unit1 = 105;
     vert_unit2 = 255;
     vert_sclFctr1 = 0
     vert_sclFctr2 = 0
     level1 = zidx
     level2 = 0
  else if ((zsize .gt. 1) .and. (.not. soil_layers) .and. (.not. fraction)) &
       then
     vert_unit1 = 111;
     vert_unit2 = 255;
     vert_sclFctr1 = 4
     vert_sclFctr2 = 4
     if (vert_stag) then
        level1 = (10000*full_eta(zidx)+0.5)
     else
        level1 = (10000*half_eta(zidx)+0.5)
     endif
     level2 = 0
  else
     ! Set the vertical coordinate and level for soil and 2D fields
     if (fraction) then
        vert_unit1 = 105
        vert_unit2 = 255
        level1 = zidx
        level2 = 0
        vert_sclFctr1 = 0
        vert_sclFctr2 = 0
     else if (soil_layers) then
        vert_unit1 = 106
        vert_unit2 = 106
        level1 = 100*(soil_depth(zidx) - 0.5*soil_thickness(zidx))+0.5
        level2 = 100*(soil_depth(zidx) + 0.5*soil_thickness(zidx))+0.5
        vert_sclFctr1 = 2
        vert_sclFctr2 = 2
     else if (VarName .eq. 'mu') then
        vert_unit1 = 105
        vert_unit2 = 255
        level1 = 0
        level2 = 0
        vert_sclFctr1 = 0
        vert_sclFctr2 = 0
     else if ((VarName .eq. 'Q2') .or. (VarName .eq. 'TH2') .or. &
        (VarName .eq. 'T2')) then
        vert_unit1 = 103
        vert_unit2 = 255
        level1 = 2
        level2 = 0
        vert_sclFctr1 = 0
        vert_sclFctr2 = 0
     else if ((VarName .eq. 'Q10') .or. (VarName .eq. 'TH10') .or. &
          (VarName .eq. 'U10') .or. (VarName .eq. 'V10')) then
        vert_unit1 = 103
        vert_unit2 = 255
        level1 = 10
        level2 = 0
        vert_sclFctr1 = 0
        vert_sclFctr2 = 0
     else 
        vert_unit1 = 1
        vert_unit2 = 255
        level1 = 0
        level2 = 0
        vert_sclFctr1 = 0
        vert_sclFctr2 = 0
     endif
  endif

end SUBROUTINE gr2_get_levels

!*****************************************************************************

subroutine gr2_create_w(StartDate, cgrib, lcgrib, production_status, Disc, &
     center, subcenter, MasterTblV, LocalTblV, ierr, msg)

  implicit none

  character*24 ,intent(in)     :: StartDate
  character*(*),intent(inout)  :: cgrib
  integer      ,intent(in)     :: lcgrib
  integer      ,intent(in)     :: production_status
  integer      ,intent(out)    :: ierr
  character*(*),intent(out)    :: msg
  integer , dimension(13)      :: listsec1
  integer , dimension(2)       :: listsec0
  integer                      :: slen
  integer , intent(in)         :: Disc, center, subcenter, MasterTblV, LocalTblV

  ! 
  ! Create the grib message
  !
  listsec0(1) = Disc       ! Discipline (Table 0.0)
  listsec0(2) = 2          ! Grib edition number

  listsec1(1) = center     ! Id of Originating Center (255 for missing)
  listsec1(2) = subcenter  ! Id of originating sub-center (255 for missing)
  listsec1(3) = MasterTblV ! Master Table Version #
  listsec1(4) = LocalTblV  ! Local table version #
  listsec1(5) = 1          ! Significance of reference time, 1 indicates start of forecast

  READ(StartDate(1:4),  '(I4)') listsec1(6) ! Year of reference

  READ(StartDate(6:7),  '(I2)') listsec1(7) ! Month of reference

  READ(StartDate(9:10), '(I2)') listsec1(8) ! Day of reference

  slen = LEN(StartDate)

  if (slen.GE.13) then
     read(StartDate(12:13),'(I2)') listsec1(9)
  else
     listsec1(9) = 0
  endif

  if (slen.GE.16) then
     read(StartDate(15:16),'(I2)') listsec1(10)
  else
     listsec1(10) = 0
  endif

  if (slen.GE.19) then
     read(StartDate(18:19),'(I2)') listsec1(11)
  else
     listsec1(11) = 0
  end if

  listsec1(12) = production_status  ! Production status of data
  listsec1(13) = 1     ! Type of data (1 indicates forecast products)

  call gribcreate(cgrib,lcgrib,listsec0,listsec1,ierr)

  if (ierr .ne. 0) then
     write(msg,*) 'gribcreate failed with ierr: ',ierr
  else
     msg = ''
  endif
  
end SUBROUTINE gr2_create_w


!*****************************************************************************
subroutine gr2_addgrid_w(cgrib, lcgrib, central_lat, central_lon, wrf_projection, &
     latin1, latin2, nx, ny, dx, dy, center_lat, center_lon, ierr,msg)
  
  implicit none

  character*(*)            ,intent(inout)   :: cgrib
  integer                  ,intent(in)      :: lcgrib
  real                     ,intent(in)      :: central_lat
  real                     ,intent(in)      :: central_lon
  integer                  ,intent(in)      :: wrf_projection
  real                     ,intent(in)      :: latin1
  real                     ,intent(in)      :: latin2
  integer                  ,intent(in)      :: nx
  integer                  ,intent(in)      :: ny
  real                     ,intent(in)      :: dx
  real                     ,intent(in)      :: dy
  real                     ,intent(in)      :: center_lat
  real                     ,intent(in)      :: center_lon
  integer                  ,intent(out)     :: ierr
  character*(*)            ,intent(out)     :: msg
  integer, dimension(5)                     :: igds
  integer, parameter                        :: igdstmplen = 25
  integer, dimension(igdstmplen)            :: igdstmpl
  integer, parameter                        :: idefnum = 0
  integer, dimension(idefnum)               :: ideflist
  real                                      :: LLLa, LLLo, URLa, URLo
  real                                      :: incrx, incry
  real, parameter                           :: deg_to_microdeg = 1e6
  real, parameter                           :: km_to_mm = 1e6
  real, parameter                           :: km_to_m = 1e3
  real, parameter                           :: DEG_TO_RAD = PI/180
  real, parameter                           :: RAD_TO_DEG = 180/PI
  real, parameter                           :: ERADIUS = 6370.0

  igds(1) = 0      ! Source of grid definition
  igds(2) = nx*ny  ! Number of points in grid
  igds(3) = 0      ! 
  igds(4) = 0

  ! Here, setup the parameters that are common to all WRF projections

  igdstmpl(1) = 1       ! Shape of earth (1 for spherical with specified radius)
  igdstmpl(2) = 0       ! Scale factor for earth radius
  igdstmpl(3) = ERADIUS*km_to_m ! Radius of earth
  igdstmpl(4) = 0       ! Scale factor for major axis
  igdstmpl(5) = 0       ! Major axis
  igdstmpl(6) = 0       ! Scale factor for minor axis
  igdstmpl(7) = 0       ! Minor axis
  igdstmpl(8) = nx      ! Number of points along x axis
  igdstmpl(9) = ny      ! Number of points along y axis
  
  !
  ! Setup increments in "x" and "y" direction.  For LATLON projection
  !   increments need to be in degrees.  For all other projections, 
  !   increments are in km.
  !
  if ((wrf_projection .eq. WRF_LATLON) &
       .or. (wrf_projection .eq. WRF_CASSINI)) then
     incrx = (dx/ERADIUS) * RAD_TO_DEG
     incry = (dy/ERADIUS) * RAD_TO_DEG
  else
     incrx = dx
     incry = dy
  endif

  ! Latitude and longitude of first (i.e., lower left) grid point
  call get_ll_latlon(central_lat, central_lon, wrf_projection, &
       latin1, latin2, nx, ny, incrx, incry, center_lat, center_lon, &
       LLLa, LLLo, URLa, URLo, ierr);

  select case (wrf_projection)

  case(WRF_LATLON,WRF_CASSINI)
     igds(5) = 0
     igdstmpl(10) = 0    ! Basic Angle of init projection (not important to us)
     igdstmpl(11) = 0    ! Subdivision of basic angle
     igdstmpl(12) = LLLa*deg_to_microdeg
     igdstmpl(13) = LLLo*deg_to_microdeg
     call gr2_convert_lon(igdstmpl(13))
     igdstmpl(14) = 128  ! Resolution and component flags
     igdstmpl(15) = URLa*deg_to_microdeg
     igdstmpl(16) = URLo*deg_to_microdeg
     call gr2_convert_lon(igdstmpl(16))

     ! Warning, the following assumes that dx and dy are valid at the equator.
     !    It is not clear in WRF where dx and dy are valid for latlon projections
     igdstmpl(17) = incrx*deg_to_microdeg ! i-direction increment in micro degs
     igdstmpl(18) = incry*deg_to_microdeg ! j-direction increment in micro degs

     igdstmpl(19) = 64   ! Scanning mode
  case(WRF_MERCATOR)
     igds(5) = 10
     igdstmpl(10) = LLLa*deg_to_microdeg
     igdstmpl(11) = LLLo*deg_to_microdeg
     call gr2_convert_lon(igdstmpl(11))
     igdstmpl(12) = 128  ! Resolution and component flags
     igdstmpl(13) = latin1*deg_to_microdeg  ! "True" latitude
     igdstmpl(14) = URLa*deg_to_microdeg
     igdstmpl(15) = URLo*deg_to_microdeg
     call gr2_convert_lon(igdstmpl(15))
     igdstmpl(16) = 64   ! Scanning mode
     igdstmpl(17) = 0    ! Orientation of grid between i-direction and equator
     igdstmpl(18) = dx*km_to_mm   ! i-direction increment
     igdstmpl(19) = dy*km_to_mm   ! j-direction increment
  case(WRF_LAMBERT)
     igds(5) = 30
     
     igdstmpl(10) = LLLa*deg_to_microdeg
     igdstmpl(11) = LLLo*deg_to_microdeg
     call gr2_convert_lon(igdstmpl(11))
     igdstmpl(12) = 128 ! Resolution and component flag
     igdstmpl(13) = latin1*deg_to_microdeg ! Latitude where Dx and Dy are specified
     igdstmpl(14) = central_lon*deg_to_microdeg
     call gr2_convert_lon(igdstmpl(14))
     igdstmpl(15) = dx*km_to_mm  ! x-dimension grid-spacing  in units of m^-3
     igdstmpl(16) = dy*km_to_mm
     if (center_lat .lt. 0) then
        igdstmpl(17) = 1
     else
        igdstmpl(17) = 0
     endif
     igdstmpl(18) = 64   ! Scanning mode
     igdstmpl(19) = latin1*deg_to_microdeg
     igdstmpl(20) = latin2*deg_to_microdeg
     igdstmpl(21) = -90*deg_to_microdeg
     igdstmpl(22) = central_lon*deg_to_microdeg
     call gr2_convert_lon(igdstmpl(22))

  case(WRF_POLAR_STEREO)
     igds(5) = 20
     igdstmpl(10) = LLLa*deg_to_microdeg
     igdstmpl(11) = LLLo*deg_to_microdeg
     call gr2_convert_lon(igdstmpl(11))
     igdstmpl(12) = 128 ! Resolution and component flag
     igdstmpl(13) = latin1*deg_to_microdeg ! Latitude where Dx and Dy are specified
     igdstmpl(14) = central_lon*deg_to_microdeg
     call gr2_convert_lon(igdstmpl(14))
     igdstmpl(15) = dx*km_to_mm  ! x-dimension grid-spacing  in units of m^-3
     igdstmpl(16) = dy*km_to_mm
     if (center_lat .lt. 0) then
        igdstmpl(17) = 1
     else
        igdstmpl(17) = 0
     endif
     igdstmpl(18) = 64   ! Scanning mode

  case default
     write(msg,*) 'invalid WRF projection: ',wrf_projection
     ierr = -1
     return
  end select


  call addgrid(cgrib,lcgrib,igds,igdstmpl,igdstmplen,ideflist,idefnum,ierr)
  if (ierr .ne. 0) then
     write(msg,*) 'addgrid failed with ierr: ',ierr
  else
     msg = ''
  endif

end subroutine gr2_addgrid_w

!*****************************************************************************

subroutine gr2_addfield_w(cgrib, lcgrib, VarName, parmcat, parmnum, DecScl, &
     BinScl, fcst_secs, vert_unit1, vert_unit2, vert_sclFctr1, vert_sclFctr2, &
     numlevels, levels, ngrdpts, background_proc_id, forecast_proc_id, &
     compression, fld, ierr, msg)
  
  implicit none

  character*(*)            ,intent(inout)   :: cgrib
  integer                  ,intent(in)      :: lcgrib
  character (LEN=*)        ,intent(in)      :: VarName
  integer                  ,intent(in)      :: parmcat,parmnum,DecScl,BinScl
  real                     ,intent(in)      :: fcst_secs
  integer                  ,intent(in)      :: vert_unit1, vert_unit2
  integer                  ,intent(in)      :: vert_sclFctr1, vert_sclFctr2
  integer                  ,intent(in)      :: numlevels
  integer, dimension(*)    ,intent(in)      :: levels
  integer                  ,intent(in)      :: ngrdpts
  real                     ,intent(in)      :: fld(ngrdpts)
  integer                  ,intent(in)      :: background_proc_id
  integer                  ,intent(in)      :: forecast_proc_id
  integer                  ,intent(in)      :: compression
  integer                  ,intent(out)     :: ierr
  character*(*)            ,intent(out)     :: msg
  integer                                   :: ipdsnum
  integer, parameter                        :: ipdstmplen = 15
  integer, dimension(ipdstmplen)            :: ipdstmpl
  integer                                   :: numcoord
  integer, dimension(numlevels)             :: coordlist
  integer                                   :: idrsnum
  integer, parameter                        :: idrstmplen = 7
  integer, dimension(idrstmplen)            :: idrstmpl
  integer                                   :: ibmap
  integer, dimension(1)                     :: bmap

  if (numlevels .gt. 2) then
     ipdsnum = 1000           ! Product definition tmplate (1000 for cross-sxn)
  else
     ipdsnum = 0              ! Product definition template (0 for horiz grid)
  endif

  ipdstmpl(1) = parmcat    ! Parameter category
  ipdstmpl(2) = parmnum    ! Parameter number
  ipdstmpl(3) = 2          ! Type of generating process (2 for forecast)
  ipdstmpl(4) = background_proc_id ! Background generating process id
  ipdstmpl(5) = forecast_proc_id   ! Analysis or forecast generating process id
  ipdstmpl(6) = 0          ! Data cutoff period (Hours)
  ipdstmpl(7) = 0          ! Data cutoff period (minutes)
  ipdstmpl(8) = 13         ! Time range indicator (13 for seconds)
  ipdstmpl(9) = NINT(fcst_secs) ! Forecast time

  if (ipdsnum .eq. 1000) then
     numcoord = numlevels
     coordlist = levels(1:numlevels)

     !
     ! Set Data Representation templ (Use 0 for vertical cross sections,
     !    since there seems to be a bug in g2lib for JPEG2000 and PNG)
     !
     idrsnum = 0

  else if (ipdsnum .eq. 0) then
     ipdstmpl(10) = vert_unit1    ! Type of first surface (111 for Eta level)
     ipdstmpl(11) = vert_sclFctr1 ! Scale factor for 1st surface
     ipdstmpl(12) = levels(1)     ! First fixed surface
     ipdstmpl(13) = vert_unit2    ! Type of second fixed surface
     ipdstmpl(14) = vert_sclFctr2 ! Scale factor for 2nd surface
     if (numlevels .eq. 2) then 
        ipdstmpl(15) = levels(2)
     else
        ipdstmpl(15) = 0
     endif
     numcoord = 0
     coordlist(1) = 0

     ! Set Data Representation templ (40 for JPEG2000, 41 for PNG)  
     idrsnum = compression

  endif


  if (idrsnum == 40) then    ! JPEG 2000

     idrstmpl(1) = 255       ! Reference value - ignored on input
     idrstmpl(2) = BinScl    ! Binary scale factor
     idrstmpl(3) = DecScl    ! Decimal scale factor 
     idrstmpl(4) = 0         ! number of bits for each data value - ignored on input
     idrstmpl(5) = 0         ! Original field type - ignored on input
     idrstmpl(6) = 0         ! 0 for lossless compression
     idrstmpl(7) = 255       ! Desired compression ratio if idrstmpl(6) != 0

  else if (idrsnum == 41) then ! PNG

     idrstmpl(1) = 255       ! Reference value - ignored on input
     idrstmpl(2) = BinScl    ! Binary scale factor
     idrstmpl(3) = DecScl    ! Decimal scale factor 
     idrstmpl(4) = 0         ! number of bits for each data value - ignored on input
     idrstmpl(5) = 0         ! Original field type - ignored on input

  else if (idrsnum == 0) then! Simple packing 

     idrstmpl(1) = 255       ! Reference value - ignored on input
     idrstmpl(2) = BinScl    ! Binary scale factor
     idrstmpl(3) = DecScl    ! Decimal scale factor 
     idrstmpl(4) = 0         ! number of bits for each data value - ignored on input
     idrstmpl(5) = 0         ! Original field type - ignored on input
     
  else
     
     write (msg,*) 'addfield failed because Data Representation template',&
          idrsnum,' is invalid'
     ierr = 1
     return

  endif

  ibmap = 255                ! Flag for bitmap
  
  call addfield(cgrib, lcgrib, ipdsnum, ipdstmpl, ipdstmplen, coordlist,      &
       numcoord, idrsnum, idrstmpl, idrstmplen, fld, ngrdpts, ibmap,          &
       bmap, ierr)

  if (ierr .ne. 0) then
     write(msg,*) 'addfield failed with ierr: ',ierr
  else
     msg = ''
  endif

end subroutine gr2_addfield_w

!*****************************************************************************

subroutine gr2_fill_local_use(DataHandle,string,VarName,fcsts,msg,status)

  use gr2_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"

  integer,         intent(in)    :: DataHandle
  character*(*)   ,intent(inout) :: string
  character*(*)   ,intent(in)    :: VarName
  integer                        :: center, subcenter, MasterTblV, LocalTblV, &
       Disc, Category, ParmNum, DecScl, BinScl
  integer         ,intent(out)   :: status
  character*(*)   ,intent(out)   :: msg
  integer , parameter            :: lcgrib = 1000000
  character (lcgrib)             :: cgrib
  real, dimension(1,1)           :: data
  integer                        :: lengrib
  integer                        :: lcsec2
  integer                        :: fcsts
  integer                        :: bytes_written
  
  ! 
  ! Set data to a default dummy value.
  !
  data = 1.0

  !
  ! This statement prevents problems when calling addlocal in the grib2
  !   library.  Basically, if addlocal is called with an empty string, it
  !   will be encoded correctly by the grib2 routine, but the grib2 routines
  !   that read the data (i.e., getgb2) will segfault.  This prevents that 
  !   segfault.
  !

  if (string .eq. '') string = 'none'

  CALL get_parminfo(VarName, center, subcenter, MasterTblV, &
       LocalTblV, Disc, Category, ParmNum, DecScl, BinScl, status)
  if (status .ne. 0) then
     write(msg,*) 'Could not find parameter for '//   &
          trim(VarName)//'   Skipping output of '//trim(VarName)
     call wrf_message(trim(msg))
     Status =  WRF_GRIB2_ERR_GRIB2MAP
     return
  endif

  !
  ! Create the indicator and identification sections (sections 0 and 1)
  !
  CALL gr2_create_w(StartDate, cgrib, lcgrib, production_status, Disc, &
             center, subcenter, MasterTblV, LocalTblV, status, msg)
  if (status .ne. 0) then
     call wrf_message(trim(msg))
     Status = WRF_GRIB2_ERR_GRIBCREATE
     return
  endif

  ! 
  ! Add the local use section
  !
  lcsec2 = len_trim(string)
  call addlocal(cgrib,lcgrib,string,lcsec2,status)
  if (status .ne. 0) then
     call wrf_message(trim(msg))
     Status = WRF_GRIB2_ERR_ADDLOCAL
     return
  endif

  !
  ! Add the grid definition section (section 3) using a 1x1 grid
  !
  call gr2_addgrid_w(cgrib, lcgrib, center_lat, proj_central_lon,  &
       wrf_projection, truelat1, truelat2, 1, 1, dx, dy,       &
       center_lat, center_lon, status, msg)
  if (status .ne. 0) then
     call wrf_message(trim(msg))
     Status = WRF_GRIB2_ERR_ADDGRIB
     return
  endif

  !
  ! Add the Product Definition, Data representation, bitmap 
  !      and data sections (sections 4-7)
  !
  call gr2_addfield_w(cgrib, lcgrib, VarName, Category, ParmNum, DecScl, &
       BinScl, fcsts, 1, 255, 0, 0, 1, 0, 1, &
       background_proc_id, forecast_proc_id, compression, data, status, msg)
  if (status .ne. 0) then
     call wrf_message(trim(msg))
     Status = WRF_GRIB2_ERR_ADDFIELD
     return
  endif

  !
  ! Close out the message
  !
  
  call gribend(cgrib,lcgrib,lengrib,status)
  if (status .ne. 0) then
     write(msg,*) 'gribend failed with status: ',status     
     call wrf_message(trim(msg))
     Status = WRF_GRIB2_ERR_GRIBEND
     return
  endif

  ! 
  ! Write the data to the file
  !
  
  call bawrite(DataHandle, -1, lengrib, bytes_written, cgrib)
!!  call write_file_n(fileinfo(DataHandle)%FileFd, cgrib, lengrib, status)
  if (bytes_written .ne. lengrib) then
     write(msg,*) '2 Error writing cgrib to file, wrote: ', &
          bytes_written, ' bytes.  Tried to write ', lengrib, ' bytes'
     call wrf_message(trim(msg))
     Status = WRF_GRIB2_ERR_WRITE
     return
  endif

  ! Set string back to the original blank value
  if (string .eq. '') string = ''

  return

end subroutine gr2_fill_local_use

!*****************************************************************************
!
! Set longitude to be in the range of 0-360 degrees.
!
!*****************************************************************************

subroutine gr2_convert_lon(value)

  IMPLICIT NONE

  integer, intent(inout) :: value
  real, parameter                           :: deg_to_microdeg = 1e6

  do while (value .lt. 0) 
     value = value + 360*deg_to_microdeg
  enddo

  do while (value .gt. 360*deg_to_microdeg) 
     value = value - 360*deg_to_microdeg
  enddo

end subroutine gr2_convert_lon


!*****************************************************************************
!
! Add a time to the list of times
!
!*****************************************************************************

subroutine gr2_add_time(DataHandle,addTime)

  USE gr2_data_info
  IMPLICIT NONE

  integer           :: DataHandle
  character (len=*) :: addTime
  integer           :: idx
  logical           :: already_have = .false.
  logical           :: swap
  character (len=len(addTime)) :: tmp
  character (DateStrLen), dimension(:),pointer  :: tmpTimes(:)
  integer,parameter :: allsize = 50
  integer           :: ierr
  
  already_have = .false.
  do idx = 1,fileinfo(DataHandle)%NumberTimes 
     if (addTime .eq. fileinfo(DataHandle)%Times(idx)) then
        already_have = .true.
     endif
  enddo
  
  if (.not. already_have) then
     fileinfo(DataHandle)%NumberTimes = fileinfo(DataHandle)%NumberTimes + 1

     if (fileinfo(DataHandle)%NumberTimes .gt. &
          fileinfo(DataHandle)%sizeAllocated) then

        if (fileinfo(DataHandle)%NumberTimes .eq. 1) then

           if (allocated(fileinfo(DataHandle)%Times)) &
                deallocate(fileinfo(DataHandle)%Times)

           allocate(fileinfo(DataHandle)%Times(allsize), stat = ierr)
           if (ierr .ne. 0) then
              call wrf_message('Could not allocate space for Times 1, exiting')
              stop
           endif

           fileinfo(DataHandle)%sizeAllocated = allsize

        else

           allocate(tmpTimes(fileinfo(DataHandle)%NumberTimes), stat=ierr)

           tmpTimes = &
                fileinfo(DataHandle)%Times(1:fileinfo(DataHandle)%NumberTimes)

           deallocate(fileinfo(DataHandle)%Times)

           allocate(&
                fileinfo(DataHandle)%Times(fileinfo(DataHandle)%sizeAllocated+allsize), stat=ierr)

           if (ierr .ne. 0) then
              call wrf_message('Could not allocate space for Times 2, exiting')
              stop
           endif

           fileinfo(DataHandle)%Times(1:fileinfo(DataHandle)%NumberTimes) = &
                tmpTimes

           deallocate(tmpTimes)
           
        endif
        
     endif

     fileinfo(DataHandle)%Times(fileinfo(DataHandle)%NumberTimes) = addTime
  
     ! Sort the Times array

     swap = .true.
     do while (swap)
        swap = .false.
        do idx = 1,fileinfo(DataHandle)%NumberTimes - 1
           if (fileinfo(DataHandle)%Times(idx) .gt. fileinfo(DataHandle)%Times(idx+1)) then
              tmp = fileinfo(DataHandle)%Times(idx)
              fileinfo(DataHandle)%Times(idx) = fileinfo(DataHandle)%Times(idx+1)
              fileinfo(DataHandle)%Times(idx+1) = tmp
              swap = .true.
           endif
        enddo
     enddo

  endif

  return

end subroutine gr2_add_time


!*****************************************************************************
!
! Fill an array of levels
!
!*****************************************************************************

subroutine gr2_fill_levels(DataHandle,VarName,levels,ierr)

  USE gr2_data_info
  USE grib_mod
  IMPLICIT NONE

#include "wrf_status_codes.h"


  integer            :: DataHandle
  character (len=*)  :: VarName
  REAL,DIMENSION(*)  :: levels
  integer            :: ierr
  integer :: JIDS(JIDSSIZE), JPDTN, JPDT(JPDTSIZE), JGDTN, &
       JGDT(JGDTSIZE)
  type(gribfield)    :: gfld
  integer            :: status, fields_to_skip
  logical            :: unpack
  integer            :: center, subcenter, MasterTblV, LocalTblV, &
       Disc, Category, ParmNum, DecScl, BinScl
  CHARACTER (LEN=maxMsgSize) :: msg


  CALL get_parminfo(VarName, center, subcenter, MasterTblV, &
       LocalTblV, Disc, Category, ParmNum, DecScl, BinScl, status)
  if (status .ne. 0) then
     write(msg,*) 'Could not find parameter for '//   &
          trim(VarName)//'   Skipping output of '//trim(VarName)
     call wrf_message(trim(msg))
     ierr = -1
     return
  endif


  !
  ! First, set all values to wild, then specify necessary values
  !
  call gr2_g2lib_wildcard(JIDS, JPDT, JGDT)

  JIDS(1) = center
  JIDS(2) = subcenter
  JIDS(3) = MasterTblV
  JIDS(4) = LocalTblV
  JIDS(5) = 1           ! Indicates that time is "Start of Forecast"
  JIDS(13) = 1          ! Type of processed data (1 for forecast products)
  
  JPDTN = 1000          ! Product definition template number
  JPDT(1) = Category
  JPDT(2) = ParmNum
  JPDT(3) = 2           ! Generating process id

  JGDTN    = -1         ! Indicates that any Grid Display Template is a match
  
  UNPACK   = .TRUE.     ! Unpack bitmap and data values


  fields_to_skip = 0

  CALL GETGB2(DataHandle, 0, fields_to_skip, -1, Disc, JIDS, JPDTN, &
       JPDT, JGDTN, JGDT, UNPACK, fileinfo(DataHandle)%recnum, &
       gfld, status)
  if (status .eq. 99) then
     write(msg,*)'Could not find field '//trim(VarName)//&
          ' continuing.'
     call wrf_message(trim(msg))
     ierr = -1
     return
  else if (status .ne. 0) then
     write(msg,*)'Retrieving scalar data field '//trim(VarName)//&
          ' failed, continuing.'
     call wrf_message(trim(msg))
     ierr = -1
     return
  endif
  
  levels(1:gfld%ndpts) = gfld%fld(1:gfld%ndpts)
  ierr = 0
  
end subroutine gr2_fill_levels


!*****************************************************************************
!
! Set values for search array arguments for getgb2 to missing.
!
!*****************************************************************************

subroutine gr2_g2lib_wildcard(JIDS, JPDT, JGDT)

  USE gr2_data_info
  integer :: JIDS(*), JPDT(*), JGDT(*)

  do idx = 1,JIDSSIZE
     JIDS(idx) = -9999
  enddo
  
  do idx=1,JPDTSIZE
     JPDT(idx) = -9999
  enddo
  
  do idx = 1,JGDTSIZE
     JGDT(idx) = -9999
  enddo

  return

end subroutine gr2_g2lib_wildcard
!*****************************************************************************
!
! Retrieve a metadata value from the input string
!
!*****************************************************************************

subroutine gr2_get_metadata_value(instring, Key, Value, stat)
  character(len=*),intent(in)  :: instring
  character(len=*),intent(in)  :: Key
  character(len=*),intent(out) :: Value
  integer         ,intent(out) :: stat
  integer :: Key_pos, equals_pos, line_end
  character :: lf

  lf=char(10)

  Value = 'abc'

  !
  ! Find Starting position of Key
  !
  Key_pos = index(instring, lf//' '//Key//' =')
  if (Key_pos .eq. 0) then
     stat = -1
     return
  endif

  !
  ! Find position of the "=" after the Key
  !
  equals_pos = index(instring(Key_pos:len(instring)), "=") + Key_pos
  if (equals_pos .eq. Key_pos) then
     stat = -1
     return
  endif

  !
  ! Find end of line
  !
  line_end = index(instring(equals_pos:len(instring)), lf) + equals_pos

  !
  ! Handle the case for the last line in the string
  !
  if (line_end .eq. equals_pos) then
     line_end = len(trim(instring))
  endif

  !
  ! Set value
  !
  if ( (equals_pos + 1) .le. (line_end - 2) ) then
     Value = trim(adjustl(instring(equals_pos+1:line_end-2)))
  else
     Value = ""
  endif
  
  stat = 0
  

end subroutine gr2_get_metadata_value

!*****************************************************************************
!
! Build onto a metadata string with the input value
!
!*****************************************************************************

SUBROUTINE gr2_build_string (string, Element, Value, Count, Status)

  IMPLICIT NONE
#include "wrf_status_codes.h"

  CHARACTER (LEN=*) , INTENT(INOUT) :: string
  CHARACTER (LEN=*) , INTENT(IN)    :: Element
  CHARACTER (LEN=*) , INTENT(IN)    :: Value(*)
  INTEGER ,           INTENT(IN)    :: Count
  INTEGER ,           INTENT(OUT)   :: Status

  CHARACTER (LEN=2)                 :: lf
  INTEGER                           :: IDX

  lf=char(10)//' '

  if (index(string,lf//Element//' =') .gt. 0) then
     ! We do nothing, since we dont want to add the same variable twice.
  else 
     if (len_trim(string) == 0) then
        string = lf//Element//' = '
     else
        string = trim(string)//lf//Element//' = '
     endif
     do idx = 1,Count
        if (idx > 1) then
           string = trim(string)//','
        endif
        string = trim(string)//' '//trim(adjustl(Value(idx)))
     enddo
  endif

  Status = WRF_NO_ERR

END SUBROUTINE gr2_build_string

