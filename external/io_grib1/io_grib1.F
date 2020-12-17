!*-----------------------------------------------------------------------------
!*
!*  Todd Hutchinson
!*  WSI
!*  400 Minuteman Road
!*  Andover, MA     01810
!*  thutchinson@wsi.com
!*
!*-----------------------------------------------------------------------------

!*
!* This io_grib1 API is designed to read WRF input and write WRF output data
!*   in grib version 1 format.  
!*


module gr1_data_info

!*
!* This module will hold data internal to this I/O implementation.  
!*   The variables will be accessible by all functions (provided they have a 
!*   "USE gr1_data_info" line).
!*

  integer                , parameter       :: FATAL            = 1
  integer                , parameter       :: DEBUG            = 100
  integer                , parameter       :: DateStrLen       = 19

  integer                , parameter       :: firstFileHandle  = 8
  integer                , parameter       :: maxFileHandles   = 30 
  integer                , parameter       :: maxLevels        = 1000
  integer                , parameter       :: maxSoilLevels    = 100
  integer                , parameter       :: maxPressLevels   = 100
  integer                , parameter       :: maxTurbLayers    = 100
  integer                , parameter       :: maxDomains       = 500

  logical ,      dimension(maxFileHandles) :: committed, opened, used
  character*128, dimension(maxFileHandles) :: DataFile
  integer,       dimension(maxFileHandles) :: FileFd
  integer,       dimension(maxFileHandles) :: FileStatus
  REAL,          dimension(maxLevels)      :: half_eta, full_eta
  REAL,          dimension(maxSoilLevels)  :: soil_depth, soil_thickness
  REAL,          dimension(maxPressLevels) :: press_levels
  REAL,          dimension(maxTurbLayers)  :: turb_layer_bot
  REAL,          dimension(maxTurbLayers)  :: turb_layer_top
  character*24                             :: StartDate = ''
  character*24                             :: InputProgramName = ''
  integer                                  :: projection
  integer                                  :: wg_grid_id
  real                                     :: dx,dy
  real                                     :: truelat1, truelat2
  real                                     :: center_lat, center_lon
  real                                     :: proj_central_lon
  real                                     :: timestep
  character,     dimension(:), pointer     :: grib_tables
  logical                                  :: table_filled = .FALSE.
  character,     dimension(:), pointer     :: grid_info
  integer                                  :: full_xsize, full_ysize
  integer, dimension(maxDomains)           :: domains = -1
  integer                                  :: this_domain = 0
  integer                                  :: max_domain = 0
  
  TYPE :: HandleVar
     character, dimension(:), pointer      :: fileindex(:)
     integer                               :: CurrentTime
     integer                               :: NumberTimes
     character (DateStrLen), dimension(:),pointer  :: Times(:)
  ENDTYPE
  TYPE (HandleVar), dimension(maxFileHandles) :: fileinfo

  TYPE :: prevdata
     integer :: fcst_secs_rainc
     integer :: fcst_secs_rainnc
     real, dimension(:,:), pointer         :: rainc, rainnc
  END TYPE prevdata
  TYPE (prevdata), DIMENSION(500)          :: lastdata

  TYPE :: initdata
     real,         dimension(:,:), pointer :: snod
  END TYPE initdata

  TYPE (initdata), dimension(maxDomains)   :: firstdata

  TYPE :: prestype
     real,         dimension(:,:,:), pointer :: vals
     logical                                :: newtime
     character*120                          :: lastDateStr
  END TYPE prestype

  character*120, dimension(maxDomains)     :: lastDateStr

  TYPE (prestype), dimension(maxDomains)   :: pressure
  TYPE (prestype), dimension(maxDomains)   :: geopotential

  integer                                  :: center, subcenter, parmtbl

  character(len=15000), dimension(firstFileHandle:maxFileHandles) :: td_output
  character(len=15000), dimension(firstFileHandle:maxFileHandles) :: ti_output

  logical                                  :: WrfIOnotInitialized = .true.

end module gr1_data_info


subroutine ext_gr1_ioinit(SysDepInfo,Status)

  USE gr1_data_info
  implicit none
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  CHARACTER*(*), INTENT(IN) :: SysDepInfo
  integer ,intent(out) :: Status
  integer :: i
  integer :: size, istat
  CHARACTER (LEN=300) :: wrf_err_message

  call wrf_debug ( DEBUG , 'Entering ext_gr1_ioinit')

  do i=firstFileHandle, maxFileHandles
        used(i) = .false.
        committed(i) = .false.
        opened(i) = .false.
        td_output(i) = ''
        ti_output(i) = ''
  enddo
  domains(:) = -1

  do i = 1, maxDomains
    pressure(i)%newtime = .false.
    pressure(i)%lastDateStr = ''
    geopotential(i)%newtime = .false.
    geopotential(i)%lastDateStr = ''
    lastDateStr(i) = ''
  enddo

  lastdata%fcst_secs_rainc = 0
  lastdata%fcst_secs_rainnc = 0
  FileStatus(1:maxFileHandles) = WRF_FILE_NOT_OPENED
  WrfIOnotInitialized = .false.

  Status = WRF_NO_ERR

  return
end subroutine ext_gr1_ioinit

!*****************************************************************************

subroutine ext_gr1_ioexit(Status)

  USE gr1_data_info
  implicit none
#include "wrf_status_codes.h"
  integer istat
  integer ,intent(out) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_ioexit')

  if (table_filled) then
     CALL free_gribmap(grib_tables)
     DEALLOCATE(grib_tables, stat=istat)
     table_filled = .FALSE.
  endif
  IF ( ASSOCIATED ( grid_info ) ) THEN
    DEALLOCATE(grid_info, stat=istat)
  ENDIF
  NULLIFY(grid_info)

  Status = WRF_NO_ERR

  return
end subroutine ext_gr1_ioexit

!*****************************************************************************

SUBROUTINE ext_gr1_open_for_read_begin ( FileName , Comm_compute, Comm_io, &
     SysDepInfo, DataHandle , Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  integer                     :: ierr
  integer                     :: size
  integer                     :: idx
  integer                     :: parmid
  integer                     :: dpth_parmid
  integer                     :: thk_parmid
  integer                     :: leveltype
  integer , DIMENSION(1000)   :: indices
  integer                     :: numindices
  real , DIMENSION(1000)      :: levels
  real                        :: tmp
  integer                     :: swapped
  integer                     :: etaidx
  integer                     :: grb_index
  integer                     :: level1, level2
  integer   :: tablenum
  integer   :: stat
  integer   :: endchar
  integer   :: last_grb_index
  CHARACTER (LEN=300) :: wrf_err_message

  call wrf_debug ( DEBUG , 'Entering ext_gr1_open_for_read_begin')

  CALL gr1_get_new_handle(DataHandle)

  if (DataHandle .GT. 0) then
     CALL open_file(TRIM(FileName), 'r', FileFd(DataHandle), ierr)
     if (ierr .ne. 0) then
        Status = WRF_ERR_FATAL_BAD_FILE_STATUS
     else
        opened(DataHandle) = .true.
        DataFile(DataHandle) = TRIM(FileName)
        FileStatus(DataHandle) = WRF_FILE_OPENED_NOT_COMMITTED
     endif
  else
     Status = WRF_WARN_TOO_MANY_FILES
     return
  endif
 
  ! Read the grib index file first
  if (.NOT. table_filled) then
     table_filled = .TRUE.
     CALL GET_GRIB1_TABLES_SIZE(size)
     ALLOCATE(grib_tables(1:size), STAT=ierr)
     CALL LOAD_GRIB1_TABLES ("gribmap.txt", grib_tables, ierr)
     if (ierr .ne. 0) then
        DEALLOCATE(grib_tables)
        WRITE( wrf_err_message , * ) &
             'Could not open file gribmap.txt '
        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
        Status = WRF_ERR_FATAL_BAD_FILE_STATUS
        return
     endif
  endif

  ! Begin by indexing file and reading metadata into structure.
  CALL GET_FILEINDEX_SIZE(size)
  ALLOCATE(fileinfo(DataHandle)%fileindex(1:size), STAT=ierr)

  CALL ALLOC_INDEX_FILE(fileinfo(DataHandle)%fileindex(:))
  CALL INDEX_FILE(FileFd(DataHandle),fileinfo(DataHandle)%fileindex(:))

  ! Get times into Times variable
  CALL GET_NUM_TIMES(fileinfo(DataHandle)%fileindex(:), &
       fileinfo(DataHandle)%NumberTimes);

  ALLOCATE(fileinfo(DataHandle)%Times(1:fileinfo(DataHandle)%NumberTimes), STAT=ierr)
  do idx = 1,fileinfo(DataHandle)%NumberTimes
     CALL GET_TIME(fileinfo(DataHandle)%fileindex(:),idx, &
          fileinfo(DataHandle)%Times(idx))
  enddo

  ! CurrentTime starts as 0.  The first time in the file is 1.  So,
  !   until set_time or get_next_time is called, the current time
  !   is not set.
  fileinfo(DataHandle)%CurrentTime = 0

  CALL gr1_fill_eta_levels(fileinfo(DataHandle)%fileindex(:), &
       FileFd(DataHandle), & 
       grib_tables, "ZNW", full_eta)
  CALL gr1_fill_eta_levels(fileinfo(DataHandle)%fileindex(:), FileFd(DataHandle), &
       grib_tables, "ZNU", half_eta)

  ! 
  ! Now, get the soil levels
  !
  CALL GET_GRIB_PARAM(grib_tables, "ZS", center, subcenter, parmtbl, &
       tablenum, dpth_parmid)
  CALL GET_GRIB_PARAM(grib_tables,"DZS", center, subcenter, parmtbl, &
       tablenum, thk_parmid)
  if (dpth_parmid == -1) then
     call wrf_message ('Error getting grib parameter')
  endif

  leveltype = 112

  CALL GET_GRIB_INDICES(fileinfo(DataHandle)%fileindex(:),center, subcenter, parmtbl, &
       dpth_parmid,"*",leveltype, &
       -HUGE(1),-HUGE(1), -HUGE(1),-HUGE(1),indices,numindices)

  last_grb_index = -1;
  do idx = 1,numindices
     CALL READ_GRIB(fileinfo(DataHandle)%fileindex(:), FileFd(DataHandle), &
          indices(idx), soil_depth(idx))
     !
     ! Now read the soil thickenesses
     !
     CALL GET_LEVEL1(fileinfo(DataHandle)%fileindex(:),indices(idx),level1)
     CALL GET_LEVEL2(fileinfo(DataHandle)%fileindex(:),indices(idx),level2)
     CALL GET_GRIB_INDEX_GUESS(fileinfo(DataHandle)%fileindex(:), &
          center, subcenter, parmtbl, thk_parmid,"*",leveltype, &
          level1,level2,-HUGE(1),-HUGE(1), last_grb_index+1, grb_index)
     CALL READ_GRIB(fileinfo(DataHandle)%fileindex(:),FileFd(DataHandle),grb_index, &
          soil_thickness(idx))

     last_grb_index = grb_index
  enddo
  


  !
  ! Fill up any variables that need to be retrieved from Metadata
  !
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), 'PROGRAM_NAME', "none", &
       "none", InputProgramName, stat)
  if (stat /= 0) then
     CALL wrf_debug (DEBUG , "PROGRAM_NAME not found in input METADATA")
  else 
     endchar = SCAN(InputProgramName," ")
     InputProgramName = InputProgramName(1:endchar)
  endif

  call wrf_debug ( DEBUG , 'Exiting ext_gr1_open_for_read_begin')

  RETURN
END SUBROUTINE ext_gr1_open_for_read_begin

!*****************************************************************************

SUBROUTINE ext_gr1_open_for_read_commit( DataHandle , Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  character(len=1000) :: msg
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_open_for_read_commit')

  Status = WRF_NO_ERR
  if(WrfIOnotInitialized) then
    Status = WRF_IO_NOT_INITIALIZED
    write(msg,*) 'ext_gr1_ioinit was not called ',__FILE__,', line', __LINE__
    call wrf_debug ( FATAL , msg)
    return
  endif
  committed(DataHandle) = .true.
  FileStatus(DataHandle) = WRF_FILE_OPENED_FOR_READ

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_gr1_open_for_read_commit

!*****************************************************************************

SUBROUTINE ext_gr1_open_for_read ( FileName , Comm_compute, Comm_io, &
     SysDepInfo, DataHandle , Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status


  call wrf_debug ( DEBUG , 'Entering ext_gr1_open_for_read')

  DataHandle = 0   ! dummy setting to quiet warning message
  CALL ext_gr1_open_for_read_begin( FileName, Comm_compute, Comm_io, &
       SysDepInfo, DataHandle, Status )
  IF ( Status .EQ. WRF_NO_ERR ) THEN
     FileStatus(DataHandle) = WRF_FILE_OPENED_NOT_COMMITTED
     CALL ext_gr1_open_for_read_commit( DataHandle, Status )
  ENDIF
  return

  RETURN  
END SUBROUTINE ext_gr1_open_for_read

!*****************************************************************************

SUBROUTINE ext_gr1_open_for_write_begin(FileName, Comm, IOComm, SysDepInfo, &
     DataHandle, Status)
  
  USE gr1_data_info
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
  CHARACTER (LEN=300) :: wrf_err_message
  integer             :: size

  call wrf_debug ( DEBUG , 'Entering ext_gr1_open_for_write_begin')

  if (.NOT. table_filled) then
     table_filled = .TRUE.
     CALL GET_GRIB1_TABLES_SIZE(size)
     ALLOCATE(grib_tables(1:size), STAT=ierr)
     CALL LOAD_GRIB1_TABLES ("gribmap.txt", grib_tables, ierr)
     if (ierr .ne. 0) then
        DEALLOCATE(grib_tables)
        WRITE( wrf_err_message , * ) &
             'Could not open file gribmap.txt '
        CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
        Status = WRF_ERR_FATAL_BAD_FILE_STATUS
        return
     endif
  endif

  Status = WRF_NO_ERR
  CALL gr1_get_new_handle(DataHandle)
  if (DataHandle .GT. 0) then
     CALL open_file(TRIM(FileName), 'w', FileFd(DataHandle), ierr)
     if (ierr .ne. 0) then
        Status = WRF_WARN_WRITE_RONLY_FILE
     else
        opened(DataHandle) = .true.
        DataFile(DataHandle) = TRIM(FileName)
        FileStatus(DataHandle) = WRF_FILE_OPENED_NOT_COMMITTED
     endif
     committed(DataHandle) = .false.
     td_output(DataHandle) = ''
  else
     Status = WRF_WARN_TOO_MANY_FILES
  endif

  RETURN  
END SUBROUTINE ext_gr1_open_for_write_begin

!*****************************************************************************

SUBROUTINE ext_gr1_open_for_write_commit( DataHandle , Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_open_for_write_commit')

  IF ( opened( DataHandle ) ) THEN
    IF ( used( DataHandle ) ) THEN
      committed(DataHandle) = .true.
      FileStatus(DataHandle) = WRF_FILE_OPENED_FOR_WRITE
    ENDIF
  ENDIF

  Status = WRF_NO_ERR

  RETURN  
END SUBROUTINE ext_gr1_open_for_write_commit

!*****************************************************************************

subroutine ext_gr1_inquiry (Inquiry, Result, Status)
  use gr1_data_info
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
end subroutine ext_gr1_inquiry

!*****************************************************************************

SUBROUTINE ext_gr1_inquire_opened ( DataHandle, FileName , FileStat, Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStat
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_inquire_opened')

  FileStat = WRF_NO_ERR
  if ((DataHandle .ge. firstFileHandle) .and. &
       (DataHandle .le. maxFileHandles)) then
     FileStat = FileStatus(DataHandle)
  else
     FileStat = WRF_FILE_NOT_OPENED
  endif
  
  Status = FileStat

  RETURN
END SUBROUTINE ext_gr1_inquire_opened

!*****************************************************************************

SUBROUTINE ext_gr1_ioclose ( DataHandle, Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER DataHandle, Status
  INTEGER istat
  INTEGER ierr
  character(len=1000) :: outstring
  character :: lf
  lf=char(10)
     
  call wrf_debug ( DEBUG , 'Entering ext_gr1_ioclose')

  Status = WRF_NO_ERR

  CALL write_file(FileFd(DataHandle), lf//'<METADATA>'//lf,ierr)
  outstring = &
       '<!-- The following are fields that were supplied to the WRF I/O API.'//lf//&
       'Many variables (but not all) are redundant with the variables within '//lf//&
       'the grib headers.  They are stored here, as METADATA, so that the '//lf//&
       'WRF I/O API has simple access to these variables.-->'
  CALL write_file(FileFd(DataHandle), trim(outstring), ierr)
  if (trim(ti_output(DataHandle)) /= '') then
     CALL write_file(FileFd(DataHandle), trim(ti_output(DataHandle)), ierr)
     CALL write_file(FileFd(DataHandle), lf, ierr)
  endif
  if (trim(td_output(DataHandle)) /= '') then
     CALL write_file(FileFd(DataHandle), trim(td_output(DataHandle)), ierr)
     CALL write_file(FileFd(DataHandle), lf, ierr)
  endif
  CALL write_file(FileFd(DataHandle), '</METADATA>'//lf,ierr)
  ti_output(DataHandle) = ''
  td_output(DataHandle) = ''
  if (ierr .ne. 0) then
     Status = WRF_WARN_WRITE_RONLY_FILE
  endif
  CALL close_file(FileFd(DataHandle))

  used(DataHandle) = .false.

  RETURN
END SUBROUTINE ext_gr1_ioclose

!*****************************************************************************

SUBROUTINE ext_gr1_write_field( DataHandle , DateStrIn , VarName , &
     Field , FieldType , Comm , IOComm, &
     DomainDesc , MemoryOrder , Stagger , &
     DimNames , &
     DomainStart , DomainEnd , &
     MemoryStart , MemoryEnd , &
     PatchStart , PatchEnd , &
     Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
#include "wrf_projection.h"
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStrIn
  CHARACTER(DateStrLen) :: DateStr
  CHARACTER*(*) :: VarName
  CHARACTER*120 :: OutName
  CHARACTER(120) :: TmpVarName
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
  integer                                      :: ierror
  character (120)                         :: msg
  integer :: xsize, ysize, zsize
  integer :: x, y, z
  integer :: x_start,x_end,y_start,y_end,z_start,z_end,ndim
  integer :: idx
  integer :: proj_center_flag
  logical :: vert_stag = .false.
  integer :: levelnum
  real, DIMENSION(:,:), POINTER :: data,tmpdata
  integer, DIMENSION(:), POINTER :: mold
  integer :: istat
  integer :: accum_period
  integer :: size
  integer, dimension(1000) :: level1, level2
  real, DIMENSION( 1:1,MemoryStart(1):MemoryEnd(1), &
                   MemoryStart(2):MemoryEnd(2), &
                   MemoryStart(3):MemoryEnd(3) ) :: Field
  real    :: fcst_secs
  logical :: soil_layers, fraction, is_press_levels,is_turb_layers
  integer :: vert_unit
  integer :: abc(2,2,2)
  integer :: def(8)
  logical :: output = .true.
  integer :: idx1, idx2, idx3
  logical :: new_domain
  real    :: region_center_lat, region_center_lon
  integer :: dom_xsize, dom_ysize;
  integer :: ierr
  logical :: already_have_domain

  call wrf_debug ( DEBUG , 'Entering ext_gr1_write_field for parameter'//VarName)

  !
  ! If DateStr is all 0's, we reset it to StartDate (if StartDate exists).  
  !   For some reason, 
  !   in idealized simulations, StartDate is 0001-01-01_00:00:00 while
  !   the first DateStr is 0000-00-00_00:00:00.  
  !
  if (DateStrIn .eq. '0000-00-00_00:00:00') then
     if (StartDate .ne. '') then
        DateStr = TRIM(StartDate)
     else
        DateStr = '0001-01-01_00:00:00'
     endif
  else
     DateStr = DateStrIn
  endif

  !
  ! Check if this is a domain that we haven't seen yet.  If so, add it to 
  !   the list of domains.
  !
  new_domain = .false.
  already_have_domain = .false.
  do idx = 1, max_domain
     if (this_domain .eq. domains(idx)) then
        already_have_domain = .true.
     endif
  enddo
  if (.NOT. already_have_domain) then
     max_domain = max_domain + 1
     domains(max_domain) = this_domain
     new_domain = .true.
  endif

  !
  ! If the time has changed, we open a new file.  This is a kludge to get
  !   around slowness in WRF that occurs when opening a new data file the
  !   standard way.
  !
#ifdef GRIB_ONE_TIME_PER_FILE
  if (lastDateStr(this_domain) .ne. DateStr) then
     write(DataFile(DataHandle),'(A8,i2.2,A1,A19)') 'wrfout_d',this_domain,'_',DateStr
     call ext_gr1_ioclose ( DataHandle, Status )
     CALL open_file(TRIM(DataFile(DataHandle)), 'w', FileFd(DataHandle), ierr)
     if (ierr .ne. 0) then
        print *,'Could not open new file: ',DataFile(DataHandle)
        print *,'  Appending to old file.'
     else
        ! Just set used back to .true. here, since ioclose set it to false.
        used(DataHandle) = .true.
     endif
     td_output(DataHandle) = ''
  endif
  lastDateStr(this_domain) = DateStr
#endif

  output = .true.
  zsize = 1
  xsize = 1
  ysize = 1
  OutName = VarName
  is_press_levels = .false.
  is_turb_layers = .false.
  soil_layers = .false.
  fraction = .false.

  ! First, handle then special cases for the boundary data.

  CALL get_dims(MemoryOrder, PatchStart, PatchEnd, ndim, x_start, x_end, &
       y_start, y_end,z_start,z_end)
  xsize = x_end - x_start + 1
  ysize = y_end - y_start + 1
  zsize = z_end - z_start + 1

  do idx = 1, len(MemoryOrder)
     if ((MemoryOrder(idx:idx) .eq. 'Z') .and. &
          (DimNames(idx) .eq. 'soil_layers_stag')) then
        soil_layers = .true.
     else if ((MemoryOrder(idx:idx) .eq. 'Z') .and. &
          (DimNames(idx) .eq. 'num_press_levels_stag')) then
        is_press_levels = .true.
     else if ((MemoryOrder(idx:idx) .eq. 'Z') .and. &
          (DimNames(idx) .eq. 'num_turb_layers')) then
        is_turb_layers = .true.
     else if ((OutName .eq. 'LANDUSEF') .or. (OutName .eq. 'SOILCBOT') .or. &
          (OutName .eq. 'SOILCTOP')) then
        fraction = .true.
     endif
  enddo

  if (.not. ASSOCIATED(grid_info)) then
     CALL get_grid_info_size(size)
     ALLOCATE(grid_info(1:size), STAT=istat)
     if (istat .eq. -1) then
        DEALLOCATE(grid_info)
        Status = WRF_ERR_FATAL_BAD_FILE_STATUS
        return
     endif
  endif
     

  if (new_domain) then
     ALLOCATE(firstdata(this_domain)%snod(xsize,ysize))
     firstdata(this_domain)%snod(:,:) = 0.0
     ALLOCATE(lastdata(this_domain)%rainc(xsize,ysize))
     lastdata(this_domain)%rainc(:,:) = 0.0
     ALLOCATE(lastdata(this_domain)%rainnc(xsize,ysize))
     lastdata(this_domain)%rainnc(:,:) = 0.0
  endif

  if (zsize .eq. 0) then 
     zsize = 1
  endif

  ALLOCATE(data(1:xsize,1:ysize), STAT=istat)
  ALLOCATE(mold(1:ysize), STAT=istat)
  ALLOCATE(tmpdata(1:xsize,1:ysize), STAT=istat)

  if (OutName .eq. 'ZNU') then
     do idx = 1, zsize
        half_eta(idx) = Field(1,idx,1,1)
     enddo
  endif

  if (OutName .eq. 'ZNW') then
     do idx = 1, zsize
        full_eta(idx) = Field(1,idx,1,1)
     enddo
  endif

  if (OutName .eq. 'ZS') then
     do idx = 1, zsize
        soil_depth(idx) = Field(1,idx,1,1)
     enddo
  endif

  if (OutName .eq. 'DZS') then
     do idx = 1, zsize
        soil_thickness(idx) = Field(1,idx,1,1)
     enddo
  endif

  if (OutName .eq. 'P_PL') then
     do idx = 1, zsize
        press_levels(idx) = Field(1,idx,1,1)
     enddo
  endif

  if (OutName .eq. 'AFWA_TLYRBOT') then
     do idx = 1, zsize
        turb_layer_bot(idx) = Field(1,idx,1,1)
     enddo
  endif

  if (OutName .eq. 'AFWA_TLYRTOP') then
     do idx = 1, zsize
        turb_layer_top(idx) = Field(1,idx,1,1)
     enddo
  endif
  if ((xsize .lt. 1) .or. (ysize .lt. 1)) then
     write(msg,*) 'Cannot output field with memory order: ', &
          MemoryOrder,Varname
     call wrf_message(msg)
     return
  endif
     
  call get_vert_stag(OutName,Stagger,vert_stag)

  do idx = 1, zsize
     call gr1_get_levels(OutName, idx, zsize, soil_layers, vert_stag, fraction, &
          is_press_levels, is_turb_layers, vert_unit, level1(idx), level2(idx))
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

  !
  ! Handle case of polare stereographic centered on pole.  In that case,
  !   always set center lon to be the projection central longitude.
  !
  if ((projection .eq. WRF_POLAR_STEREO) .AND. &
       (abs(center_lat - 90.0) < 0.01)) then
     center_lon = proj_central_lon
  endif

  CALL get_region_center(MemoryOrder, projection, center_lat, center_lon, &
       dom_xsize, dom_ysize, dx, dy, proj_central_lon, proj_center_flag, &
       truelat1, truelat2, xsize, ysize, region_center_lat, region_center_lon)

  if ( .not. opened(DataHandle)) then
     Status = WRF_WARN_FILE_NOT_OPENED
     return
  endif


  if (opened(DataHandle) .and. committed(DataHandle)) then


#ifdef OUTPUT_FULL_PRESSURE

     ! 
     ! The following is a kludge to output full pressure instead of the two 
     !  fields of base-state pressure and pressure perturbation.
     !
     ! This code can be turned on by adding -DOUTPUT_FULL_PRESSURE to the 
     !  compile line
     !
     
     if ((OutName .eq. 'P') .or. (OutName.eq.'PB')) then
        do idx = 1, len(MemoryOrder)
            if (MemoryOrder(idx:idx) .eq. 'X') then
              idx1=idx
           endif
           if (MemoryOrder(idx:idx) .eq. 'Y') then
              idx2=idx
           endif
           if (MemoryOrder(idx:idx) .eq. 'Z') then
              idx3=idx
           endif
        enddo

        ! 
        ! Allocate space for pressure values (this variable holds 
        !   base-state pressure or pressure perturbation to be used 
        !   later to sum base-state and perturbation pressure to get full 
        !   pressure).
        !

        if (.not. ASSOCIATED(pressure(this_domain)%vals)) then
           ALLOCATE(pressure(this_domain)%vals(MemoryStart(1):MemoryEnd(1), &
                MemoryStart(2):MemoryEnd(2),MemoryStart(3):MemoryEnd(3)))
        endif
        if (DateStr .NE. &
             pressure(this_domain)%lastDateStr) then
           pressure(this_domain)%newtime = .true.
        endif
        if (pressure(this_domain)%newtime) then
           pressure(this_domain)%vals = Field(1,:,:,:)
           pressure(this_domain)%newtime = .false.
           output = .false.
        else 
           output = .true.
        endif
        pressure(this_domain)%lastDateStr=DateStr
     endif
#endif

#ifdef OUTPUT_FULL_GEOPOTENTIAL

     ! 
     ! The following is a kludge to output full geopotential height instead 
     !  of the two fields of base-state geopotential and perturbation 
     !  geopotential.
     !
     ! This code can be turned on by adding -DOUTPUT_FULL_GEOPOTENTIAL to the 
     !  compile line
     !
     
     if ((OutName .eq. 'PHB') .or. (OutName.eq.'PH')) then
        do idx = 1, len(MemoryOrder)
            if (MemoryOrder(idx:idx) .eq. 'X') then
              idx1=idx
           endif
           if (MemoryOrder(idx:idx) .eq. 'Y') then
              idx2=idx
           endif
           if (MemoryOrder(idx:idx) .eq. 'Z') then
              idx3=idx
           endif
        enddo

        ! 
        ! Allocate space for geopotential values (this variable holds 
        !   geopotential to be used 
        !   later to sum base-state and perturbation to get full 
        !   geopotential).
        !

        if (.not. ASSOCIATED(geopotential(this_domain)%vals)) then
           ALLOCATE(geopotential(this_domain)%vals(MemoryStart(1):MemoryEnd(1), &
                MemoryStart(2):MemoryEnd(2),MemoryStart(3):MemoryEnd(3)))
        endif
        if (DateStr .NE. &
             geopotential(this_domain)%lastDateStr) then
           geopotential(this_domain)%newtime = .true.
        endif
        if (geopotential(this_domain)%newtime) then
           geopotential(this_domain)%vals = Field(1,:,:,:)
           geopotential(this_domain)%newtime = .false.
           output = .false.
        else 
           output = .true.
        endif
        geopotential(this_domain)%lastDateStr=DateStr
     endif
#endif

     if (output) then 
        if (StartDate == '') then
           StartDate = DateStr
        endif
        CALL geth_idts(DateStr,StartDate,fcst_secs)
        
        if (center_lat .lt. 0) then
           proj_center_flag = 2
        else
           proj_center_flag = 1
        endif
         
        do z = 1, zsize
           SELECT CASE (MemoryOrder)
           CASE ('XYZ')
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
              data(1,1) = Field(1,z,1,1)
           CASE ('z')
              data(1,1) = Field(1,z,1,1)
           CASE ('C')
              data = Field(1,1:xsize,1:ysize,z)
           CASE ('c')
              data = Field(1,1:xsize,1:ysize,z)
           CASE ('0')
              data(1,1) = Field(1,1,1,1)
           END SELECT

           ! 
           ! Here, we convert any integer fields to real
           !
           if (FieldType == WRF_INTEGER) then
              mold = 0
              do idx=1,xsize
                 !
                 ! The parentheses around data(idx,:) are needed in order
                 !   to fix a bug with transfer with the xlf compiler on NCAR's
                 !   IBM (bluesky).
                 !
                 data(idx,:)=transfer((data(idx,:)),mold)
              enddo
           endif
           ! 
           ! Here, we do any necessary conversions to the data.
           !
           
           ! Potential temperature is sometimes passed in as perturbation 
           !   potential temperature (i.e., POT-300).
           !
           if (OutName == 'T') then
              data = data + 300
           endif

           ! 
           ! For precip, we setup the accumulation period, and output a precip
           !    rate for time-step precip.
           !
           if (OutName .eq. 'RAINNCV') then
              ! Convert time-step precip to precip rate.
              data = data/timestep
              accum_period = 0
           else
              accum_period = 0
           endif
 
           if ((OutName .eq. 'AFWA_TOTPRECIP') .or. &
               (OutName .eq. 'AFWA_RAIN') .or. &
               (OutName .eq. 'AFWA_FZRA') .or. &
               (OutName .eq. 'AFWA_SNOW') .or. &
               (OutName .eq. 'AFWA_SNOWFALL') .or. &
               (OutName .eq. 'RAINC') .or. &
               (OutName .eq. 'AFWA_ICE')) then
               accum_period = fcst_secs
           endif

#ifdef OUTPUT_FULL_PRESSURE
           !
           ! Computation of full-pressure off by default since there are 
           !  uses for base-state and perturbation (i.e., restarts
           !
            if ((OutName .eq. 'P') .or. (OutName.eq.'PB')) then
               if (idx3 .eq. 1) then
                  data = data + &
                       pressure(this_domain)%vals(z, &
                       patchstart(2):patchend(2),patchstart(3):patchend(3))
               elseif (idx3 .eq. 2) then
                  data = data + &
                       pressure(this_domain)%vals(patchstart(1):patchend(1), &
                       z,patchstart(3):patchend(3))
               elseif (idx3 .eq. 3) then
                  data = data + &
                       pressure(this_domain)%vals(patchstart(1):patchend(1), &
                       patchstart(2):patchend(2),z)
               else
                  call wrf_message ('error in idx3, continuing')
               endif

               OutName = 'P'
            endif
#endif

#ifdef OUTPUT_FULL_GEOPOTENTIAL
           !
           ! Computation of full-geopotential off by default since there are 
           !  uses for base-state and perturbation (i.e., restarts
           !
            if ((OutName .eq. 'PHB') .or. (OutName.eq.'PH')) then
               if (idx3 .eq. 1) then
                  data = data + &
                       geopotential(this_domain)%vals(z, &
                       patchstart(2):patchend(2),patchstart(3):patchend(3))
               elseif (idx3 .eq. 2) then
                  data = data + &
                       geopotential(this_domain)%vals(patchstart(1):patchend(1), &
                       z,patchstart(3):patchend(3))
               elseif (idx3 .eq. 3) then
                  data = data + &
                       geopotential(this_domain)%vals(patchstart(1):patchend(1), &
                       patchstart(2):patchend(2),z)
               else
                  call wrf_message ('error in idx3, continuing')
               endif

               OutName = 'PHP'
            endif
#endif

           !
           !    Output current level
           !
           CALL load_grid_info(OutName, StartDate, vert_unit, level1(z), &
                level2(z), fcst_secs, accum_period, wg_grid_id, projection, &
                xsize, ysize, region_center_lat, region_center_lon, dx, dy, &
                proj_central_lon, proj_center_flag, truelat1, truelat2, &
                grib_tables, grid_info)
           
           !
           ! Here, we copy data to a temporary array.  After write_grib,
           !    we copy back from the temporary array to the permanent
           !    array.  write_grib modifies data.  For certain fields that
           !    we use below, we want the original (unmodified) data 
           !    values.  This kludge assures that we have the original
           !    values.
           !

           if ((OutName .eq. 'RAINC') .or. (OutName .eq. 'RAINNC') .or. &
                (OutName .eq. 'SNOWH')) then
              tmpdata(:,:) = data(:,:)
           endif

           CALL write_grib(grid_info, FileFd(DataHandle), data)

           if ((OutName .eq. 'RAINC') .or. (OutName .eq. 'RAINNC') .or. &
                (OutName .eq. 'SNOWH')) then
              data(:,:) = tmpdata(:,:)
           endif

           CALL free_grid_info(grid_info)
           
           !
           ! If this is the total accumulated rain, call write_grib again 
           !   to output the accumulation since the last output time as well.
           !   This is somewhat of a kludge to meet the requirements of PF.
           !
           if ((OutName .eq. 'RAINC') .or. (OutName .eq. 'RAINNC') .or. &
                (OutName .eq. 'SNOWH')) then
              if (OutName .eq. 'RAINC') then
                 data(:,:) = data(:,:) - lastdata(this_domain)%rainc(:,:)
                 lastdata(this_domain)%rainc(:,:) = tmpdata(:,:)
                 accum_period = fcst_secs - &
                      lastdata(this_domain)%fcst_secs_rainc
                 lastdata(this_domain)%fcst_secs_rainc = fcst_secs
                 TmpVarName = 'ACPCP'
              else if (OutName .eq. 'RAINNC') then
                 tmpdata(:,:) = data(:,:)
                 data(:,:) = data(:,:) - lastdata(this_domain)%rainnc(:,:)
                 lastdata(this_domain)%rainnc(:,:) = tmpdata(:,:)
                 accum_period = fcst_secs - &
                      lastdata(this_domain)%fcst_secs_rainnc
                 lastdata(this_domain)%fcst_secs_rainnc = fcst_secs
                 TmpVarName = 'NCPCP'
              else if (OutName .eq. 'SNOWH') then
                 if (fcst_secs .eq. 0) then
                    firstdata(this_domain)%snod(:,:) = data(:,:)
                 endif
                 data(:,:) = data(:,:) - firstdata(this_domain)%snod(:,:)
                 TmpVarName = 'SNOWCU'
              endif

              CALL load_grid_info(TmpVarName, StartDate, vert_unit, level1(z),&
                   level2(z), fcst_secs, accum_period, wg_grid_id, &
                   projection, xsize, ysize, region_center_lat, &
                   region_center_lon, dx, dy, proj_central_lon, &
                   proj_center_flag, truelat1, truelat2, grib_tables, &
                   grid_info)
           
              CALL write_grib(grid_info, FileFd(DataHandle), data)
              CALL free_grid_info(grid_info)
           endif

        enddo
     endif
  endif

  deallocate(data, STAT = istat)
  deallocate(mold, STAT = istat)
  deallocate(tmpdata, STAT = istat)

  Status = WRF_NO_ERR

  call wrf_debug ( DEBUG , 'Leaving ext_gr1_write_field')

  RETURN
END SUBROUTINE ext_gr1_write_field

!*****************************************************************************

SUBROUTINE ext_gr1_read_field ( DataHandle , DateStr , VarName , Field , &
     FieldType , Comm , IOComm, DomainDesc , MemoryOrder , Stagger ,     &
     DimNames , DomainStart , DomainEnd , MemoryStart , MemoryEnd ,      &
     PatchStart , PatchEnd ,  Status )

  USE gr1_data_info
  IMPLICIT NONE  
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  CHARACTER (len=400) :: msg
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
  integer   :: ndim,x_start,x_end,y_start,y_end,z_start,z_end
  integer   :: zidx
  REAL, DIMENSION(:,:), POINTER :: data
  logical                     :: vert_stag
  logical                     :: soil_layers
  integer                     :: level1,level2

  integer                     :: parmid
  integer                     :: vert_unit
  integer                     :: grb_index
  integer                     :: numcols, numrows
  integer                     :: data_allocated
  integer                     :: istat
  integer                     :: tablenum
  integer                     :: di
  integer                     :: last_grb_index
  logical                     :: fraction

  call wrf_debug ( DEBUG , 'Entering ext_gr1_read_field')

  !
  ! Get dimensions of data.  
  ! Assume that the domain size in the input data is the same as the Domain 
  !     Size from the input arguments.
  !
  
  CALL get_dims(MemoryOrder,DomainStart,DomainEnd,ndim,x_start,x_end,y_start, &
       y_end,z_start,z_end) 

  !
  ! Get grib parameter id
  !
  CALL GET_GRIB_PARAM(grib_tables, VarName, center, subcenter, parmtbl, &
       tablenum, parmid)

  !
  ! Setup the vertical unit and levels
  !
  CALL get_vert_stag(VarName,Stagger,vert_stag)
  CALL get_soil_layers(VarName,soil_layers)

  !
  ! Loop over levels, grabbing data from each level, then assembling into a 
  !   3D array.
  !
  data_allocated = 0
  last_grb_index = -1
  do zidx = z_start,z_end
     
     IF ((VarName .eq. 'LANDUSEF') .or. (VarName .eq. 'SOILCBOT') .or. &
          (VarName .eq. 'SOILCTOP')) then
        fraction = .true.
     ELSE
        fraction = .false.
     END IF
     CALL gr1_get_levels(VarName,zidx,z_end-z_start,soil_layers,vert_stag,fraction, &
          .false., .false., vert_unit,level1,level2)
     
     CALL GET_GRIB_INDEX_VALIDTIME_GUESS(fileinfo(DataHandle)%fileindex(:), center, &
          subcenter, parmtbl, parmid,DateStr,vert_unit,level1, &
          level2, last_grb_index + 1, grb_index)
     if (grb_index < 0) then
        write(msg,*)'Field not found: parmid: ',VarName,parmid,DateStr, &
             vert_unit,level1,level2
        call wrf_debug (DEBUG , msg)
        cycle
     endif

     if (data_allocated .eq. 0) then
        CALL GET_SIZEOF_GRID(fileinfo(DataHandle)%fileindex(:),grb_index,numcols,numrows)
        allocate(data(z_start:z_end,1:numcols*numrows),stat=istat)
        data_allocated = 1
     endif

     CALL READ_GRIB(fileinfo(DataHandle)%fileindex(:), FileFd(DataHandle), grb_index, &
          data(zidx,:))

     !
     ! Transpose data into the order specified by MemoryOrder, setting only 
     !   entries within the memory dimensions
     !
     CALL get_dims(MemoryOrder, MemoryStart, MemoryEnd, ndim, x_start, x_end, &
          y_start, y_end,z_start,z_end)

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
           data(zidx,:) = data(zidx,:) - 300
        endif
     endif

     CALL Transpose_grib(MemoryOrder, di, FieldType, Field, &
          MemoryStart(1), MemoryEnd(1), MemoryStart(2), MemoryEnd(2), &
          MemoryStart(3), MemoryEnd(3), &
          data(zidx,:), zidx, numrows, numcols)

     if (zidx .eq. z_end) then
        data_allocated = 0
        deallocate(data)
     endif

     last_grb_index = grb_index

  enddo

  Status = WRF_NO_ERR
  if (grb_index < 0) Status = WRF_WARN_VAR_NF
  call wrf_debug ( DEBUG , 'Leaving ext_gr1_read_field')

  RETURN
END SUBROUTINE ext_gr1_read_field

!*****************************************************************************

SUBROUTINE ext_gr1_get_next_var ( DataHandle, VarName, Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_next_var')

  call wrf_message ( 'WARNING: ext_gr1_get_next_var is not supported.')

  Status = WRF_WARN_NOOP

  RETURN
END SUBROUTINE ext_gr1_get_next_var

!*****************************************************************************

subroutine ext_gr1_end_of_frame(DataHandle, Status)

  USE gr1_data_info
  implicit none
#include "wrf_status_codes.h"
  integer               ,intent(in)     :: DataHandle
  integer               ,intent(out)    :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_end_of_frame')

  Status = WRF_WARN_NOOP

  return
end subroutine ext_gr1_end_of_frame

!*****************************************************************************

SUBROUTINE ext_gr1_iosync ( DataHandle, Status )

  USE gr1_data_info  
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_iosync')

  Status = WRF_NO_ERR
  if (DataHandle .GT. 0) then
     CALL flush_file(FileFd(DataHandle))
  else
     Status = WRF_WARN_TOO_MANY_FILES
  endif

  RETURN
END SUBROUTINE ext_gr1_iosync

!*****************************************************************************

SUBROUTINE ext_gr1_inquire_filename ( DataHandle, FileName , FileStat, &
     Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStat
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER *80   SysDepInfo

  call wrf_debug ( DEBUG , 'Entering ext_gr1_inquire_filename')

  FileName = DataFile(DataHandle) 

  if ((DataHandle .ge. firstFileHandle) .and. &
       (DataHandle .le. maxFileHandles)) then
     FileStat = FileStatus(DataHandle)
  else
     FileStat = WRF_FILE_NOT_OPENED
  endif
  
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_gr1_inquire_filename

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_info ( DataHandle , VarName , NDim , &
     MemoryOrder , Stagger , DomainStart , DomainEnd , Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: VarName
  integer               ,intent(out)    :: NDim
  character*(*)         ,intent(out)    :: MemoryOrder
  character*(*)         ,intent(out)    :: Stagger
  integer ,dimension(*) ,intent(out)    :: DomainStart, DomainEnd
  integer               ,intent(out)    :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_info')

  CALL wrf_message('ext_gr1_get_var_info not supported for grib version1 data')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_gr1_get_var_info

!*****************************************************************************

SUBROUTINE ext_gr1_set_time ( DataHandle, DateStr, Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  integer       :: found_time
  integer       :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr1_set_time')

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
END SUBROUTINE ext_gr1_set_time

!*****************************************************************************

SUBROUTINE ext_gr1_get_next_time ( DataHandle, DateStr, Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(OUT) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_next_time')

  if (fileinfo(DataHandle)%CurrentTime == fileinfo(DataHandle)%NumberTimes) then
     Status = WRF_WARN_TIME_EOF
  else
     fileinfo(DataHandle)%CurrentTime = fileinfo(DataHandle)%CurrentTime + 1
     DateStr = fileinfo(DataHandle)%Times(fileinfo(DataHandle)%CurrentTime)
     Status = WRF_NO_ERR
  endif

  RETURN
END SUBROUTINE ext_gr1_get_next_time

!*****************************************************************************

SUBROUTINE ext_gr1_get_previous_time ( DataHandle, DateStr, Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_previous_time')

  if (fileinfo(DataHandle)%CurrentTime <= 0) then
     Status = WRF_WARN_TIME_EOF
  else
     fileinfo(DataHandle)%CurrentTime = fileinfo(DataHandle)%CurrentTime - 1
     DateStr = fileinfo(DataHandle)%Times(fileinfo(DataHandle)%CurrentTime)
     Status = WRF_NO_ERR
  endif

  RETURN
END SUBROUTINE ext_gr1_get_previous_time

!******************************************************************************
!* Start of get_var_ti_* routines
!******************************************************************************

SUBROUTINE ext_gr1_get_var_ti_real ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE gr1_data_info
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
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_ti_real')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), "none", &
       Varname, Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_var_ti_real 

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_ti_real8 ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE gr1_data_info
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
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_ti_real8')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:),TRIM(Element),&
       "none",Varname,Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_var_ti_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_ti_double ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )
  USE gr1_data_info
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
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_ti_double')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), &
       "none", Varname, &
       Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_var_ti_double

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_ti_integer ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_ti_integer')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), &
       "none", Varname, Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_var_ti_integer 

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_ti_logical ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE gr1_data_info
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
  CHARACTER*(1000) :: VALUE

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_ti_logical')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), &
       "none", Varname, Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_var_ti_logical 

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_ti_char ( DataHandle,Element,  Varname, Data,  &
     Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER       :: stat

  Status = WRF_NO_ERR
  
  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_ti_char')

  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), &
       "none", Varname, Data,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  RETURN
END SUBROUTINE ext_gr1_get_var_ti_char 

!******************************************************************************
!* End of get_var_ti_* routines
!******************************************************************************


!******************************************************************************
!* Start of put_var_ti_* routines
!******************************************************************************

SUBROUTINE ext_gr1_put_var_ti_real ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_ti_real')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_ti_real 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_ti_double ( DataHandle,Element,  Varname, Data, &
     Count,  Status )
  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_ti_double')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_ti_double

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_ti_real8 ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_ti_real8')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_ti_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_ti_integer ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_ti_integer')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_ti_integer 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_ti_logical ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_ti_logical')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr1_put_var_ti_logical 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_ti_char ( DataHandle,Element,  Varname, Data,  &
     Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_ti_char')

  if (committed(DataHandle)) then

     write(tmpstr(1),*)trim(Data)

     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, 1, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_ti_char 

!******************************************************************************
!* End of put_var_ti_* routines
!******************************************************************************

!******************************************************************************
!* Start of get_var_td_* routines
!******************************************************************************

SUBROUTINE ext_gr1_get_var_td_double ( DataHandle,Element,  DateStr, &
     Varname, Data, Count, Outcount, Status )
  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_td_double')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:),TRIM(Element),DateStr,&
       Varname,Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_var_td_double

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_td_real ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_td_real')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), DateStr, &
       Varname, Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_var_td_real 

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_td_real8 ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_td_real8')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:),TRIM(Element),DateStr,&
       Varname,Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_var_td_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_td_integer')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), DateStr, &
       Varname, Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_var_td_integer 

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )
  
  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_td_logical')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), DateStr, &
       Varname, Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_var_td_logical 

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_td_char ( DataHandle,Element,  DateStr,Varname, &
     Data,  Status )

  USE gr1_data_info
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
  
  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_td_char')

  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), DateStr, &
       Varname, Data,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  RETURN
END SUBROUTINE ext_gr1_get_var_td_char 

!******************************************************************************
!* End of get_var_td_* routines
!******************************************************************************

!******************************************************************************
!* Start of put_var_td_* routines
!******************************************************************************

SUBROUTINE ext_gr1_put_var_td_double ( DataHandle, Element, DateStr, Varname, &
     Data, Count,  Status )
  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_td_double')


  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr1_build_string (td_output(DataHandle), &
          Varname//';'//DateStr//';'//Element, tmpstr, Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr1_put_var_td_double

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_td_integer ( DataHandle,Element,  DateStr, &
     Varname, Data, Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_td_integer')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (td_output(DataHandle), &
          Varname//';'//DateStr//';'//Element, tmpstr, Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr1_put_var_td_integer 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_td_real ( DataHandle,Element,  DateStr,Varname, &
     Data, Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_td_real')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (td_output(DataHandle), &
          Varname//';'//DateStr//';'//Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_td_real 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_td_real8 ( DataHandle,Element,  DateStr,Varname, &
     Data, Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_td_real8')

  if (committed(DataHandle)) then
     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (td_output(DataHandle), &
          Varname//';'//DateStr//';'//Element, tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_td_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_td_logical ( DataHandle,Element,  DateStr, &
     Varname, Data, Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_td_logical')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr1_build_string (td_output(DataHandle), &
          Varname//';'//DateStr//';'//Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_td_logical 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_td_char ( DataHandle,Element,  DateStr,Varname, &
     Data,  Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_td_char')

  if (committed(DataHandle)) then

    
     DO idx=1,LEN(Data)
        tmpstr(idx:idx)=Data(idx:idx)
     END DO
     DO idx=LEN(Data)+1,1000
        tmpstr(idx:idx)=' '
     END DO

     CALL gr1_build_string (td_output(DataHandle), &
          Varname//';'//DateStr//';'//Element, tmpstr, 1, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_td_char 

!******************************************************************************
!* End of put_var_td_* routines
!******************************************************************************


!******************************************************************************
!* Start of get_dom_ti_* routines
!******************************************************************************

SUBROUTINE ext_gr1_get_dom_ti_real ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_ti_real')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), "none", &
       "none", Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_dom_ti_real 

!*****************************************************************************

SUBROUTINE ext_gr1_get_dom_ti_real8 ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_ti_real8')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), "none", &
       "none", Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_dom_ti_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_get_dom_ti_integer ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE gr1_data_info
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
  
  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_ti_integer Element: '//Element)

  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), "none", &
       "none", Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_dom_ti_integer 

!*****************************************************************************

SUBROUTINE ext_gr1_get_dom_ti_logical ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_ti_logical')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), "none", &
       "none", Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_dom_ti_logical 

!*****************************************************************************

SUBROUTINE ext_gr1_get_dom_ti_char ( DataHandle,Element,   Data,  Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER       :: stat
  INTEGER       :: endchar

  Status = WRF_NO_ERR
  
  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_ti_char')

  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), "none", &
       "none", Data, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  RETURN
END SUBROUTINE ext_gr1_get_dom_ti_char 

!*****************************************************************************

SUBROUTINE ext_gr1_get_dom_ti_double ( DataHandle,Element,   Data, Count, &
     Outcount, Status )
  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_ti_double')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), "none", &
       "none", Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_dom_ti_double

!******************************************************************************
!* End of get_dom_ti_* routines
!******************************************************************************


!******************************************************************************
!* Start of put_dom_ti_* routines
!******************************************************************************

SUBROUTINE ext_gr1_put_dom_ti_real ( DataHandle,Element,   Data, Count,  &
     Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_ti_real')

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

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_ti_real 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_ti_real8 ( DataHandle,Element,   Data, Count,  &
     Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real*8 ,        INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_ti_real8')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_ti_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  &
     Status )

  USE gr1_data_info
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


  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_ti_integer')

  if (Element == 'WEST-EAST_GRID_DIMENSION') then
     full_xsize = Data(1)
  else if (Element == 'SOUTH-NORTH_GRID_DIMENSION') then
     full_ysize = Data(1)
  else if (Element == 'MAP_PROJ') then
     projection = Data(1)
  else if (Element == 'WG_GRID_ID') then
     wg_grid_id = Data(1)
  else if (Element == 'GRID_ID') then
     this_domain = Data(1)
  endif

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

  call wrf_debug ( DEBUG , 'Leaving ext_gr1_put_dom_ti_integer')

  RETURN
END SUBROUTINE ext_gr1_put_dom_ti_integer 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  &
     Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  logical ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_ti_logical')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_ti_logical 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_ti_char ( DataHandle,Element,   Data,  &
     Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*),     INTENT(IN)  :: Data
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy
  CHARACTER(len=1000) :: tmpstr(1000)

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_ti_char')

  if (Element .eq. 'START_DATE') then
     StartDate = Data
  endif

  if (committed(DataHandle)) then

     write(tmpstr(1),*)trim(Data)
     
     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, 1, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_ti_char

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_ti_double ( DataHandle,Element, Data, Count, &
     Status )
  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1000)
  INTEGER             :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_ti_double')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr1_build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif
  
  RETURN
END SUBROUTINE ext_gr1_put_dom_ti_double

!******************************************************************************
!* End of put_dom_ti_* routines
!******************************************************************************


!******************************************************************************
!* Start of get_dom_td_* routines
!******************************************************************************

SUBROUTINE ext_gr1_get_dom_td_real ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_td_real')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), DateStr, &
       "none", Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_dom_td_real 

!*****************************************************************************

SUBROUTINE ext_gr1_get_dom_td_real8 ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_td_real8')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), DateStr, &
       "none", Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_dom_td_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_td_integer')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), DateStr, &
       "none", Value,stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_dom_td_integer 

!*****************************************************************************

SUBROUTINE ext_gr1_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_td_logical')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), DateStr, &
       "none", Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_dom_td_logical 

!*****************************************************************************

SUBROUTINE ext_gr1_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  &
     Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER       :: stat

  Status = WRF_NO_ERR
  
  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_td_char')

  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), DateStr, &
       "none", Data, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
     Status = WRF_WARN_VAR_NF
     RETURN
  endif

  RETURN
END SUBROUTINE ext_gr1_get_dom_td_char 

!*****************************************************************************

SUBROUTINE ext_gr1_get_dom_td_double ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )
  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_dom_td_double')

  Status = WRF_NO_ERR
  
  CALL GET_METADATA_VALUE(fileinfo(DataHandle)%fileindex(:), TRIM(Element), DateStr, &
       "none", Value, stat)
  if (stat /= 0) then
     CALL wrf_debug ( DEBUG , "GET_METADATA_VALUE failed for "//Element)
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
END SUBROUTINE ext_gr1_get_dom_td_double

!******************************************************************************
!* End of get_dom_td_* routines
!******************************************************************************


!******************************************************************************
!* Start of put_dom_td_* routines
!******************************************************************************


SUBROUTINE ext_gr1_put_dom_td_real8 ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_td_real8')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr1_build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_td_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_td_integer')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_td_integer

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_td_logical')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_td_logical

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_td_char ( DataHandle,Element, DateStr,  Data, &
     Status )

  USE gr1_data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER(len=*), INTENT(IN)  :: Data
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER(len=1000) :: tmpstr(1)

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_td_char')

  if (committed(DataHandle)) then

     write(tmpstr(1),*)Data

     CALL gr1_build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          1, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_td_char 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_td_double ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )
  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_td_double')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo

     CALL gr1_build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr1_put_dom_td_double

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_td_real ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE gr1_data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_dom_td_real')

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL gr1_build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_td_real 


!******************************************************************************
!* End of put_dom_td_* routines
!******************************************************************************


!*****************************************************************************

SUBROUTINE gr1_build_string (string, Element, Value, Count, Status)

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

  Status = WRF_NO_ERR

END SUBROUTINE gr1_build_string

!*****************************************************************************

SUBROUTINE gr1_get_new_handle(DataHandle)
  USE gr1_data_info
  IMPLICIT NONE
  
  INTEGER ,       INTENT(OUT)  :: DataHandle
  INTEGER :: i

  DataHandle = -1
  do i=firstFileHandle, maxFileHandles
     if (.NOT. used(i)) then
        DataHandle = i
        used(i) = .true.
        exit
     endif
  enddo

  RETURN
END SUBROUTINE gr1_get_new_handle


!******************************************************************************


SUBROUTINE gr1_get_levels(VarName, zidx, zsize, soil_layers, vert_stag, fraction, &
     is_press_levels, is_turb_layers, vert_unit, level1, level2)

  use gr1_data_info
  IMPLICIT NONE

  integer :: zidx
  integer :: zsize
  logical :: soil_layers
  logical :: vert_stag
  logical :: fraction
  logical :: is_press_levels
  logical :: is_turb_layers 
  integer :: vert_unit
  integer :: level1
  integer :: level2
  character (LEN=*) :: VarName

  ! Setup vert_unit, and vertical levels in grib units

  if ((VarName .eq. 'LANDUSEF') .or. (VarName .eq. 'SOILCTOP') &
       .or. (VarName .eq. 'SOILCBOT')) then
     vert_unit = 109;
     level1 = zidx
     level2 = 0
  else if ((zsize .gt. 1) .and. (.not. soil_layers) .and. (.not. fraction) &
            .and. (.not. is_press_levels) .and. (.not. is_turb_layers)) then
     vert_unit = 119;
     if (vert_stag) then
        level1 = (10000*full_eta(zidx)+0.5)
     else
        level1 = (10000*half_eta(zidx)+0.5)
     endif
     level2 = 0
  else
     ! Set the vertical coordinate and level for soil and 2D fields
     if (fraction) then
        vert_unit = 109
        level1 = zidx
        level2 = 0           
     else if (soil_layers) then
        vert_unit = 112
        level1 = 100*(soil_depth(zidx) - 0.5*soil_thickness(zidx))+0.5
        level2 = 100*(soil_depth(zidx) + 0.5*soil_thickness(zidx))+0.5
     ! Added pressure level as vertical unit - GAC 20140402
     else if (is_press_levels) then
        vert_unit = 100
        level1 = press_levels(zidx)/100.
        level2 = 0
     else if (is_turb_layers) then
        vert_unit = 106
        level1 = turb_layer_top(zidx)/100.
        level2 = turb_layer_bot(zidx)/100.
     else if (VarName .eq. 'mu') then
        vert_unit = 200
        level1 = 0
        level2 = 0
     else if ((VarName .eq. 'Q2') .or. (VarName .eq. 'TH2') .or. &
        (VarName .eq. 'T2')) then
        vert_unit = 105
        level1 = 2
        level2 = 0
     else if ((VarName .eq. 'Q10') .or. (VarName .eq. 'TH10') .or. &
          (VarName .eq. 'U10') .or. (VarName .eq. 'V10')) then
        vert_unit = 105
        level1 = 10
        level2 = 0
     ! Until a more sophisticated way to do this becomes clear,
     ! adding AFWA diagnostic variable vertical unit and level info
     ! here.  GAC 20140402
     else if ((VarName .eq. 'TCOLI_MAX') .or.                      &
             (Varname .eq. 'GRPL_FLX_MAX') .or.                    &
             (Varname .eq. 'VIL') .or.                             &
             (Varname .eq. 'RADARVIL') .or.                        &
             (Varname .eq. 'FZLEV') .or.                           &
             (Varname .eq. 'REFD_COM') .or.                        &
             (Varname .eq. 'ICINGTOP') .or.                        &
             (Varname .eq. 'ICINGBOT') .or.                        &
             (Varname .eq. 'ICING_LG') .or.                        &
             (Varname .eq. 'ICING_SM') .or.                        &
             (Varname .eq. 'AFWA_CLOUD') .or.                      &
             (Varname .eq. 'AFWA_CLOUD_CEIL') .or.                 &
             (Varname .eq. 'AFWA_HAIL') .or.                       &
             (Varname .eq. 'AFWA_TORNADO') .or.                    &
             (Varname .eq. 'AFWA_PWAT') .or.                       &
             (Varname .eq. 'QICING_LG_MAX') .or.                   &
             (Varname .eq. 'QICING_SM_MAX')) then
        vert_unit = 200
        level1 = 0
        level2 = 0
     else if (VarName .eq. 'REFD_MAX') then
        vert_unit = 105
        level1 = 1000
        level2 = 0
     else if (VarName .eq. 'WSPD10MAX') then
        vert_unit = 105
        level1 = 10
        level2 = 0
     else if (VarName .eq. 'UP_HELI_MAX') then
        vert_unit = 106
        level1 = 50 !5000 m
        level2 = 20 !2000 m
     else if (Varname .eq. 'AFWA_LLWS') then
        vert_unit = 106
        level1 = 20 !2000 m
        level2 = 0  !0 m
     else if ((Varname .eq. 'AFWA_LLTURB') .or.                   &
              (Varname .eq. 'AFWA_LLTURBLGT') .or.                &
              (Varname .eq. 'AFWA_LLTURBMDT') .or.                &
              (Varname .eq. 'AFWA_LLTURBSVR')) then
        vert_unit = 106
        level1 = 15 !1500 m
        level2 = 0  !0 m
     else if ((VarName .eq. 'W_UP_MAX') .or. (VarName .eq. 'W_DN_MAX')) then
        vert_unit = 101
        level1 = 40  !400 mb
        level2 = 100 !1000 mb
     else if (VarName .eq. 'AFWA_LIDX') then
        vert_unit = 101
        level1 = 50  !500 mb
        level2 = 100 !1000 mb
     else if (VarName .eq. 'AFWA_MSLP') then
        vert_unit = 102  ! Mean sea level
        level1 = 0
        level2 = 0
     else if ((VarName .eq. 'AFWA_CAPE_MU') .or.                  &
              (VarName .eq. 'AFWA_CIN_MU')) then
        vert_unit = 116  ! Pressure above ground
        !level1 = 180     ! 180 mb AGL
        !level2 = 0
        level1 = 0       ! 180 mb AGL
        level2 = 180
     else 
        vert_unit = 1
        level1 = 0
        level2 = 0
     endif
  endif

end SUBROUTINE gr1_get_levels

!*****************************************************************************


SUBROUTINE gr1_fill_eta_levels(fileindex, FileFd, grib_tables, VarName, eta_levels)
  IMPLICIT NONE

  CHARACTER (len=*) :: fileindex
  INTEGER   :: FileFd
  CHARACTER (len=*) :: grib_tables
  character (len=*) :: VarName
  REAL,DIMENSION(*) :: eta_levels

  INTEGER   :: center, subcenter, parmtbl
  INTEGER   :: swapped
  INTEGER   :: leveltype
  INTEGER   :: idx
  INTEGER   :: parmid
  INTEGER   :: tablenum
  REAL      :: tmp
  INTEGER   :: numindices
  integer , DIMENSION(1000)   :: indices

  !
  ! Read the levels from the grib file
  !
  CALL GET_GRIB_PARAM(grib_tables, VarName, center, subcenter, parmtbl, &
       tablenum, parmid)

  if (parmid == -1) then
     call wrf_message ('Error getting grib parameter')
  endif

  leveltype = 119

  CALL GET_GRIB_INDICES(fileindex(:), center, subcenter, parmtbl, &
       parmid, "*", leveltype, &
       -HUGE(1), -HUGE(1), -HUGE(1), -HUGE(1), indices, numindices)

  do idx = 1,numindices
     CALL READ_GRIB(fileindex(:),FileFd,indices(idx),eta_levels(idx))
  enddo

  !
  ! Sort the levels--from highest (bottom) to lowest (top)
  !
  swapped = 1
  sortloop : do
     if (swapped /= 1) exit sortloop
     swapped = 0
     do idx=2, numindices
        !
        ! Remove duplicate levels, caused by multiple time periods in a 
        ! single file.
        !
        if (eta_levels(idx) == eta_levels(idx-1)) eta_levels(idx) = 0.0
        if (eta_levels(idx) > eta_levels(idx-1)) then
          tmp = eta_levels(idx)
          eta_levels(idx) = eta_levels(idx - 1)
          eta_levels(idx - 1) = tmp
          swapped = 1
        endif
     enddo
  enddo sortloop

end subroutine gr1_fill_eta_levels

