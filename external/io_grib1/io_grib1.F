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


module data_info

!*
!* This module will hold data internal to this I/O implementation.  
!*   The variables will be accessible by all functions (provided they have a 
!*   "USE data_info" line).
!*

  integer                , parameter       :: FATAL            = 1
  integer                , parameter       :: DEBUG            = 100
  integer                , parameter       :: DateStrLen       = 19

  integer                , parameter       :: firstFileHandle  = 8
  integer                , parameter       :: maxFileHandles   = 200
  integer                , parameter       :: maxLevels        = 1000
  integer                , parameter       :: maxSoilLevels    = 100
  integer                , parameter       :: maxDomains       = 500

  logical ,      dimension(maxFileHandles) :: committed, opened, used
  character*128, dimension(maxFileHandles) :: DataFile
  integer,       dimension(maxFileHandles) :: FileFd
  integer,       dimension(maxFileHandles) :: FileStatus
  REAL,          dimension(maxLevels)      :: half_eta, full_eta
  REAL,          dimension(maxSoilLevels)  :: soil_depth, soil_thickness
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
  character,     dimension(:), pointer     :: grid_info
  character, dimension(:,:), pointer :: fileindex
  integer,   dimension(maxFileHandles)     :: CurrentTime
  integer,   dimension(maxFileHandles)     :: NumberTimes
  character (DateStrLen), dimension(:,:), pointer :: Times
  integer                                  :: full_xsize, full_ysize
  integer, dimension(maxDomains)           :: domains
  integer                                  :: max_domain = 0

  TYPE :: prevdata
     integer :: fcst_secs_rainc
     integer :: fcst_secs_rainnc
     real, dimension(:,:), pointer         :: rainc, rainnc
  END TYPE prevdata

  TYPE :: initdata
     real,         dimension(:,:), pointer :: snod
  END TYPE initdata

  TYPE (initdata), dimension(maxDomains)   :: firstdata

  TYPE :: prestype
     real,         dimension(:,:,:), pointer :: vals
     logical                                :: newtime
     character*120                          :: lastDateStr
  END TYPE prestype

  TYPE (prestype), dimension(maxDomains)   :: pressure

  integer                                  :: center, subcenter, parmtbl

  character(len=30000), dimension(maxFileHandles) :: td_output
  character(len=30000), dimension(maxFileHandles) :: ti_output

  logical                                  :: WrfIOnotInitialized = .true.

end module data_info


subroutine ext_gr1_ioinit(SysDepInfo,Status)

  USE data_info
  implicit none
#include "wrf_status_codes.h"
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
  enddo

  CALL GET_GRIB1_TABLES_SIZE(size)
  ALLOCATE(grib_tables(1:size), STAT=istat)
  CALL LOAD_GRIB1_TABLES ("gribmap.txt"//ACHAR(0), grib_tables, istat)
  if (istat .ne. 0) then
     DEALLOCATE(grib_tables)
     WRITE( wrf_err_message , * ) &
          'Could not open file gribmap.txt '
     CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
     Status = WRF_ERR_FATAL_BAD_FILE_STATUS
     return
  endif

  WrfIOnotInitialized = .false.

  Status = WRF_NO_ERR

  return
end subroutine ext_gr1_ioinit

!*****************************************************************************

subroutine ext_gr1_ioexit(Status)

  USE data_info
  implicit none
#include "wrf_status_codes.h"
  integer istat
  integer ,intent(out) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_ioexit')

  CALL free_gribmap(grib_tables)
  DEALLOCATE(grib_tables, stat=istat)
  DEALLOCATE(grid_info, stat=istat)

  Status = WRF_NO_ERR

  return
end subroutine ext_gr1_ioexit

!*****************************************************************************

SUBROUTINE ext_gr1_open_for_read_begin ( FileName , Comm_compute, Comm_io, &
     SysDepInfo, DataHandle , Status )

  USE data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_open_for_read_begin')

  CALL get_new_handle(DataHandle)

  if (DataHandle .GT. 0) then
     CALL open_file(TRIM(FileName), 'r', FileFd(DataHandle), ierr)
     if (ierr .ne. 0) then
        Status = WRF_ERR_FATAL_BAD_FILE_STATUS
     else
        opened(DataHandle) = .true.
        DataFile(DataHandle) = TRIM(FileName)
        FileStatus(DataHandle) = WRF_FILE_OPENED_FOR_READ
     endif
  else
     Status = WRF_WARN_TOO_MANY_FILES
     return
  endif
 
  ! Begin by indexing file and reading metadata into structure.
  CALL GET_FILEINDEX_SIZE(size)
  ALLOCATE(fileindex(DataHandle,1:size), STAT=ierr)

  CALL ALLOC_INDEX_FILE(fileindex(DataHandle,:))
  CALL INDEX_FILE(FileFd(DataHandle),fileindex(DataHandle,:))

  ! Get times into Times variable
  CALL GET_NUM_TIMES(fileindex(DataHandle,:),NumberTimes(DataHandle));

  ALLOCATE(Times(DataHandle,1:NumberTimes(DataHandle)), STAT=ierr)
  do idx = 1,NumberTimes(DataHandle)
     CALL GET_TIME(fileindex(DataHandle,:),idx,Times(DataHandle,idx))
  enddo

  ! CurrentTime starts as 0.  The first time in the file is 1.  So,
  !   until set_time or get_next_time is called, the current time
  !   is not set.
  CurrentTime(DataHandle) = 0

  CALL FILL_ETA_LEVELS(fileindex(DataHandle,:), FileFd(DataHandle), & 
       grib_tables, "ZNW", full_eta)
  CALL FILL_ETA_LEVELS(fileindex(DataHandle,:), FileFd(DataHandle), &
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

  CALL GET_GRIB_INDICES(fileindex(DataHandle,:),center, subcenter, parmtbl, &
       dpth_parmid,"*",leveltype, &
       -HUGE(1),-HUGE(1), -HUGE(1),-HUGE(1),indices,numindices)

  last_grb_index = -1;
  do idx = 1,numindices
     CALL READ_GRIB(fileindex(DataHandle,:), FileFd(DataHandle), &
          indices(idx), soil_depth(idx))
     !
     ! Now read the soil thickenesses
     !
     CALL GET_LEVEL1(fileindex(DataHandle,:),indices(idx),level1)
     CALL GET_LEVEL2(fileindex(DataHandle,:),indices(idx),level2)
     CALL GET_GRIB_INDEX_GUESS(fileindex(DataHandle,:), &
          center, subcenter, parmtbl, thk_parmid,"*",leveltype, &
          level1,level2,-HUGE(1),-HUGE(1), last_grb_index+1, grb_index)
     CALL READ_GRIB(fileindex(DataHandle,:),FileFd(DataHandle),grb_index, &
          soil_thickness(idx))

     last_grb_index = grb_index
  enddo
  


  !
  ! Fill up any variables that need to be retrieved from Metadata
  !
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), 'PROGRAM_NAME', "none", &
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

  USE data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
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
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_gr1_open_for_read_commit

!*****************************************************************************

SUBROUTINE ext_gr1_open_for_read ( FileName , Comm_compute, Comm_io, &
     SysDepInfo, DataHandle , Status )

  USE data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
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
    CALL ext_gr1_open_for_read_commit( DataHandle, Status )
  ENDIF
  return

  RETURN  
END SUBROUTINE ext_gr1_open_for_read

!*****************************************************************************

SUBROUTINE ext_gr1_open_for_write_begin(FileName, Comm, IOComm, SysDepInfo, &
     DataHandle, Status)
  
  USE data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_open_for_write_begin')

  Status = WRF_NO_ERR
  CALL get_new_handle(DataHandle)
  if (DataHandle .GT. 0) then
     CALL open_file(TRIM(FileName), 'w', FileFd(DataHandle), ierr)
     if (ierr .ne. 0) then
        Status = WRF_WARN_WRITE_RONLY_FILE
     else
        opened(DataHandle) = .true.
        DataFile(DataHandle) = TRIM(FileName)
        FileStatus(DataHandle) = WRF_FILE_OPENED_FOR_WRITE
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

  USE data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_open_for_write_commit')

  IF ( opened( DataHandle ) ) THEN
    IF ( used( DataHandle ) ) THEN
      committed(DataHandle) = .true.
    ENDIF
  ENDIF

  Status = WRF_NO_ERR

  RETURN  
END SUBROUTINE ext_gr1_open_for_write_commit

!*****************************************************************************

subroutine ext_gr1_inquiry (Inquiry, Result, Status)
  use data_info
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

  USE data_info
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

  USE data_info
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

SUBROUTINE ext_gr1_write_field( DataHandle , DateStr , VarName , &
     Field , FieldType , Comm , IOComm, &
     DomainDesc , MemoryOrder , Stagger , &
     DimNames , &
     DomainStart , DomainEnd , &
     MemoryStart , MemoryEnd , &
     PatchStart , PatchEnd , &
     Status )

  USE data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
#include "wrf_io_flags.h"
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
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
  logical :: soil_layers, fraction
  integer :: vert_unit
  integer :: abc(2,2,2)
  integer :: def(8)
  logical :: output = .true.
  integer :: idx1, idx2, idx3
  integer :: this_domain
  logical :: new_domain
  real    :: region_center_lat, region_center_lon
  integer :: dom_xsize, dom_ysize;

  call wrf_debug ( DEBUG , 'Entering ext_gr1_write_field')

  !
  ! If DateStr is all 0's, we reset it to StartDate.  For some reason, 
  !   in idealized simulations, StartDate is 0001-01-01_00:00:00 while
  !   the first DateStr is 0000-00-00_00:00:00.  
  !
  if (DateStr .eq. '0000-00-00_00:00:00') then
     DateStr = TRIM(StartDate)
  endif

  !
  ! Check if this is a domain that we haven't seen yet.  If so, add it to 
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

  output = .true.
  zsize = 1
  xsize = 1
  ysize = 1
  OutName = VarName
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


  if ((xsize .lt. 1) .or. (ysize .lt. 1)) then
     write(msg,*) 'Cannot output field with memory order: ', &
          MemoryOrder,Varname
     call wrf_message(msg)
     return
  endif
     
  call get_vert_stag(OutName,Stagger,vert_stag)

  do idx = 1, zsize
     call get_levels(OutName, idx, zsize, soil_layers, vert_stag, fraction, &
          vert_unit, level1(idx), level2(idx))
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

  CALL get_region_center(MemoryOrder, projection, center_lat, center_lon, &
       dom_xsize, dom_ysize, dx, dy, proj_central_lon, proj_center_flag, &
       truelat1, truelat2, xsize, ysize, region_center_lat, region_center_lon)

  if ( .not. opened(DataHandle)) then
     Status = WRF_WARN_FILE_NOT_OPENED
     return
  endif


  if (opened(DataHandle) .and. committed(DataHandle)) then


     !
     ! The following code to compute full pressure was removed by 
     !  Todd Hutchinson since there are times when base-state and 
     !  perturbation are required (i.e., for a restart)
     !

     ! 
     ! The following is a kludge to output full pressure instead of the two 
     !  fields of base-state pressure and pressure perturbation.
     !
     
!     if ((OutName .eq. 'P') .or. (OutName.eq.'PB')) then
!        do idx = 1, len(MemoryOrder)
!           if (MemoryOrder(idx:idx) .eq. 'X') then
!              idx1=idx
!           endif
!           if (MemoryOrder(idx:idx) .eq. 'Y') then
!              idx2=idx
!           endif
!           if (MemoryOrder(idx:idx) .eq. 'Z') then
!              idx3=idx
!           endif
!        enddo

        ! 
        ! Allocate space for pressure values (this variable holds 
        !   base-state pressure or pressure perturbation to be used 
        !   later to sum base-state and perturbation pressure to get full 
        !   pressure).
        !

!        if (.not. ASSOCIATED(pressure(this_domain)%vals)) then
!           ALLOCATE(pressure(this_domain)%vals(MemoryStart(1):MemoryEnd(1), &
!                MemoryStart(2):MemoryEnd(2),MemoryStart(3):MemoryEnd(3)))
!        endif
!        if (DateStr .NE. &
!             pressure(this_domain)%lastDateStr) then
!           pressure(this_domain)%newtime = .true.
!        endif
!        if (pressure(this_domain)%newtime) then
!           pressure(this_domain)%vals = Field(1,:,:,:)
!           pressure(this_domain)%newtime = .false.
!           output = .false.
!        else 
!           output = .true.
!        endif
!        pressure(this_domain)%lastDateStr=DateStr
!     endif

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
           CASE ('0')
              data(1,1) = Field(1,1,1,1)
           END SELECT

           ! 
           ! Here, we convert any integer fields to real
           !
           if (FieldType == WRF_INTEGER) then
              mold = 0
              do idx=1,xsize
                 data(idx,:)=transfer(data(idx,:),mold)
              enddo
           endif
           
           
           ! 
           ! Here, we do any necessary conversions to the data.
           !
           
           ! Potential temperature is sometimes passed in as perturbation 
           !   potential temperature (i.e., POT-300).  Other times (i.e., from 
           !   WRF SI), it is passed in as full potential temperature.
           ! Here, we convert to full potential temperature by adding 300
           !   only if POT < 200 K.
           !
           if (OutName == 'T') then
              if (data(1,1) < 200) then
                 data = data + 300
              endif
           endif

           ! 
           ! For precip, we setup the accumulation period, and output a precip
           !    rate for time-step precip.
           !
           if ((OutName .eq. 'RAINCV') .or. (OutName .eq. 'RAINNCV')) then
              ! Convert time-step precip to precip rate.
              data = data/timestep
              accum_period = 0
           else
              accum_period = 0
           endif

           !
           ! Computation of full-pressure removed since there are 
           !  uses for base-state and perturbation (i.e., restarts
           !
!           if ((OutName .eq. 'P') .or. (OutName.eq.'PB')) then
!              if (idx3 .eq. 1) then
!                 data = data + &
!                      pressure(this_domain)%vals(z, &
!                      patchstart(2):patchend(2),patchstart(3):patchend(3))
!              elseif (idx3 .eq. 2) then
!                 data = data + &
!                      pressure(this_domain)%vals(patchstart(1):patchend(1), &
!                      z,patchstart(3):patchend(3))
!              elseif (idx3 .eq. 3) then
!                 data = data + &
!                      pressure(this_domain)%vals(patchstart(1):patchend(1), &
!                      patchstart(2):patchend(2),z)
!              else
!                 call wrf_message ('error in idx3, continuing')
!              endif
!
!              OutName = 'P'
!           endif

           !
           !    Output current level
           !
           CALL load_grid_info(OutName, StartDate, vert_unit, level1(z), &
                level2(z), fcst_secs, accum_period, wg_grid_id, projection, &
                xsize, ysize, region_center_lat, region_center_lon, dx, dy, &
                proj_central_lon, proj_center_flag, truelat1, truelat2, &
                grib_tables, grid_info)
           
           CALL write_grib(grid_info, FileFd(DataHandle), data)

           CALL free_grid_info(grid_info)
           
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

  USE data_info
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
     
     CALL get_levels(VarName,zidx,z_end-z_start,soil_layers,vert_stag, &
          .false., vert_unit,level1,level2)
     
     CALL GET_GRIB_INDEX_VALIDTIME_GUESS(fileindex(DataHandle,:), center, &
          subcenter, parmtbl, parmid,DateStr,vert_unit,level1, &
          level2, last_grb_index + 1, grb_index)
     if (grb_index < 0) then
        write(msg,*)'Field not found: parmid: ',VarName,parmid,DateStr, &
             vert_unit,level1,level2
        call wrf_debug (DEBUG , msg)
        cycle
     endif

     if (data_allocated .eq. 0) then
        CALL GET_SIZEOF_GRID(fileindex(DataHandle,:),grb_index,numcols,numrows)
        allocate(data(z_start:z_end,1:numcols*numrows),stat=istat)
        data_allocated = 1
     endif

     CALL READ_GRIB(fileindex(DataHandle,:), FileFd(DataHandle), grb_index, &
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

     CALL Transpose(MemoryOrder, di, FieldType, Field, &
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
  call wrf_debug ( DEBUG , 'Leaving ext_gr1_read_field')

  RETURN
END SUBROUTINE ext_gr1_read_field

!*****************************************************************************

SUBROUTINE ext_gr1_get_next_var ( DataHandle, VarName, Status )

  USE data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_next_var')

  Status = WRF_WARN_NOOP

  RETURN
END SUBROUTINE ext_gr1_get_next_var

!*****************************************************************************

subroutine ext_gr1_end_of_frame(DataHandle, Status)

  USE data_info
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

  USE data_info  
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  integer                     :: ierror

  call wrf_debug ( DEBUG , 'Entering ext_gr1_iosync')

  Status = WRF_NO_ERR
  if (DataHandle .GT. 0) then
     CALL flush_file(FileFd(DataHandle), ierror)
     if (ierror .ne. 0) then
        Status = WRF_WARN_WRITE_RONLY_FILE
     endif
  else
     Status = WRF_WARN_TOO_MANY_FILES
  endif

  RETURN
END SUBROUTINE ext_gr1_iosync

!*****************************************************************************

SUBROUTINE ext_gr1_inquire_filename ( DataHandle, FileName , FileStat, &
     Status )

  USE data_info
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

  FileStat = WRF_NO_ERR
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_gr1_inquire_filename

!*****************************************************************************

SUBROUTINE ext_gr1_get_var_info ( DataHandle , VarName , NDim , &
     MemoryOrder , Stagger , DomainStart , DomainEnd , WrfType, Status )

  USE data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_var_info')

  CALL wrf_message('ext_gr1_get_var_info not supported for grib version1 data')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_gr1_get_var_info

!*****************************************************************************

SUBROUTINE ext_gr1_set_time ( DataHandle, DateStr, Status )

  USE data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  integer       :: found_time
  integer       :: idx

  call wrf_debug ( DEBUG , 'Entering ext_gr1_set_time')

  found_time = 0
  do idx = 1,NumberTimes(DataHandle)
     if (Times(DataHandle,idx) == DateStr) then
        found_time = 1
        CurrentTime(DataHandle) = idx
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

  USE data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(OUT) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_next_time')

  if (CurrentTime(DataHandle) == NumberTimes(DataHandle)) then
     Status = WRF_WARN_TIME_EOF
  else
     CurrentTime(DataHandle) = CurrentTime(DataHandle) + 1
     DateStr = Times(DataHandle,CurrentTime(DataHandle))
     Status = WRF_NO_ERR
  endif

  RETURN
END SUBROUTINE ext_gr1_get_next_time

!*****************************************************************************

SUBROUTINE ext_gr1_get_previous_time ( DataHandle, DateStr, Status )

  USE data_info
  IMPLICIT NONE
#include "wrf_status_codes.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status

  call wrf_debug ( DEBUG , 'Entering ext_gr1_get_previous_time')

  if (CurrentTime(DataHandle) <= 1) then
     Status = WRF_WARN_TIME_EOF
  else
     CurrentTime(DataHandle) = CurrentTime(DataHandle) - 1
     DateStr = Times(DataHandle,CurrentTime(DataHandle))
     Status = WRF_NO_ERR
  endif

  RETURN
END SUBROUTINE ext_gr1_get_previous_time

!******************************************************************************
!* Start of get_var_ti_* routines
!******************************************************************************

SUBROUTINE ext_gr1_get_var_ti_real ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), "none", &
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:),TRIM(Element),&
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
  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), &
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), &
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), &
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

  USE data_info
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

  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), &
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

  USE data_info
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

     CALL build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_ti_real 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_ti_double ( DataHandle,Element,  Varname, Data, &
     Count,  Status )
  USE data_info
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
     
     CALL build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_ti_double

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_ti_real8 ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE data_info
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
     
     CALL build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_ti_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_ti_integer ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE data_info
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
     
     CALL build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_ti_integer 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_ti_logical ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE data_info
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
     
     CALL build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr1_put_var_ti_logical 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_ti_char ( DataHandle,Element,  Varname, Data,  &
     Status )

  USE data_info
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

     CALL build_string (ti_output(DataHandle), Element, tmpstr, 1, Status)

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
  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:),TRIM(Element),DateStr,&
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), DateStr, &
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:),TRIM(Element),DateStr,&
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), DateStr, &
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
  
  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), DateStr, &
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

  USE data_info
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

  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), DateStr, &
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
  USE data_info
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

     CALL build_string (td_output(DataHandle), &
          Varname//';'//DateStr//';'//Element, tmpstr, Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr1_put_var_td_double

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_td_integer ( DataHandle,Element,  DateStr, &
     Varname, Data, Count,  Status )

  USE data_info
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
     
     CALL build_string (td_output(DataHandle), &
          Varname//';'//DateStr//';'//Element, tmpstr, Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr1_put_var_td_integer 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_td_real ( DataHandle,Element,  DateStr,Varname, &
     Data, Count,  Status )

  USE data_info
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
     
     CALL build_string (td_output(DataHandle), &
          Varname//';'//DateStr//';'//Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_td_real 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_td_real8 ( DataHandle,Element,  DateStr,Varname, &
     Data, Count,  Status )

  USE data_info
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
     
     CALL build_string (td_output(DataHandle), &
          Varname//';'//DateStr//';'//Element, tmpstr, Count, Status)
  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_td_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_td_logical ( DataHandle,Element,  DateStr, &
     Varname, Data, Count,  Status )

  USE data_info
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

     CALL build_string (td_output(DataHandle), &
          Varname//';'//DateStr//';'//Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_var_td_logical 

!*****************************************************************************

SUBROUTINE ext_gr1_put_var_td_char ( DataHandle,Element,  DateStr,Varname, &
     Data,  Status )

  USE data_info
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

  call wrf_debug ( DEBUG , 'Entering ext_gr1_put_var_td_char')

  if (committed(DataHandle)) then

     write(tmpstr(idx),*)Data

     CALL build_string (td_output(DataHandle), &
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), "none", &
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), "none", &
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

  USE data_info
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

  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), "none", &
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), "none", &
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

  USE data_info
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

  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), "none", &
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
  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), "none", &
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

  USE data_info
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
     
     CALL build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_ti_real 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_ti_real8 ( DataHandle,Element,   Data, Count,  &
     Status )

  USE data_info
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
     
     CALL build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_ti_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  &
     Status )

  USE data_info
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
  endif

  if (committed(DataHandle)) then

     do idx = 1,Count
        write(tmpstr(idx),'(G17.10)')Data(idx)
     enddo
     
     CALL build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

  call wrf_debug ( DEBUG , 'Leaving ext_gr1_put_dom_ti_integer')

  RETURN
END SUBROUTINE ext_gr1_put_dom_ti_integer 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  &
     Status )

  USE data_info
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
     
     CALL build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_ti_logical 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_ti_char ( DataHandle,Element,   Data,  &
     Status )

  USE data_info
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
     
     CALL build_string (ti_output(DataHandle), Element, tmpstr, 1, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_ti_char

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_ti_double ( DataHandle,Element, Data, Count, &
     Status )
  USE data_info
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

     CALL build_string (ti_output(DataHandle), Element, tmpstr, Count, Status)

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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), DateStr, &
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), DateStr, &
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), DateStr, &
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

  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), DateStr, &
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

  USE data_info
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

  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), DateStr, &
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
  USE data_info
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
  
  CALL GET_METADATA_VALUE(fileindex(DataHandle,:), TRIM(Element), DateStr, &
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

  USE data_info
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

     CALL build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_td_real8 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE data_info
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
     
     CALL build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_td_integer

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE data_info
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
     
     CALL build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_td_logical

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_td_char ( DataHandle,Element, DateStr,  Data, &
     Status )

  USE data_info
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

     CALL build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          1, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_td_char 

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_td_double ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )
  USE data_info
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

     CALL build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          Count, Status)

  endif

RETURN
END SUBROUTINE ext_gr1_put_dom_td_double

!*****************************************************************************

SUBROUTINE ext_gr1_put_dom_td_real ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE data_info
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
     
     CALL build_string (td_output(DataHandle), DateStr//';'//Element, tmpstr, &
          Count, Status)

  endif

  RETURN
END SUBROUTINE ext_gr1_put_dom_td_real 


!******************************************************************************
!* End of put_dom_td_* routines
!******************************************************************************


SUBROUTINE get_new_handle(DataHandle)
  USE data_info
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
END SUBROUTINE get_new_handle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION ndfeb ( year ) RESULT (num_days)
  
  ! Compute the number of days in February for the given year
  
  IMPLICIT NONE
  
  INTEGER :: year
  INTEGER :: num_days
  
  num_days = 28 ! By default, February has 28 days ...
  IF (MOD(year,4).eq.0) THEN  
     num_days = 29  ! But every four years, it has 29 days ...
     IF (MOD(year,100).eq.0) THEN
        num_days = 28  ! Except every 100 years, when it has 28 days ...
        IF (MOD(year,400).eq.0) THEN
           num_days = 29  ! Except every 400 years, when it has 29 days.
        END IF
     END IF
  END IF
  
END FUNCTION ndfeb

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE geth_idts (ndate, odate, idts)

  IMPLICIT NONE

  !  From 2 input mdates ('YYYY-MM-DD HH:MM:SS.ffff'), 
  !  compute the time difference.

  !  on entry     -  ndate  -  the new hdate.
  !                  odate  -  the old hdate.

  !  on exit      -  idts    -  the change in time in seconds.

  CHARACTER (LEN=*) , INTENT(INOUT) :: ndate, odate
  REAL              , INTENT(OUT)   :: idts

  !  Local Variables

  !  yrnew    -  indicates the year associated with "ndate"
  !  yrold    -  indicates the year associated with "odate"
  !  monew    -  indicates the month associated with "ndate"
  !  moold    -  indicates the month associated with "odate"
  !  dynew    -  indicates the day associated with "ndate"
  !  dyold    -  indicates the day associated with "odate"
  !  hrnew    -  indicates the hour associated with "ndate"
  !  hrold    -  indicates the hour associated with "odate"
  !  minew    -  indicates the minute associated with "ndate"
  !  miold    -  indicates the minute associated with "odate"
  !  scnew    -  indicates the second associated with "ndate"
  !  scold    -  indicates the second associated with "odate"
  !  i        -  loop counter
  !  mday     -  a list assigning the number of days in each month

  CHARACTER (LEN=24) :: tdate
  INTEGER :: olen, nlen
  INTEGER :: yrnew, monew, dynew, hrnew, minew, scnew
  INTEGER :: yrold, moold, dyold, hrold, miold, scold
  INTEGER :: mday(12), i, newdys, olddys
  LOGICAL :: npass, opass
  INTEGER :: isign
  CHARACTER (LEN=300) :: wrf_err_message
  INTEGER :: ndfeb

  IF (odate.GT.ndate) THEN
     isign = -1
     tdate=ndate
     ndate=odate
     odate=tdate
  ELSE
     isign = 1
  END IF

  !  Assign the number of days in a months

  mday( 1) = 31
  mday( 2) = 28
  mday( 3) = 31
  mday( 4) = 30
  mday( 5) = 31
  mday( 6) = 30
  mday( 7) = 31
  mday( 8) = 31
  mday( 9) = 30
  mday(10) = 31
  mday(11) = 30
  mday(12) = 31

  !  Break down old hdate into parts

  hrold = 0
  miold = 0
  scold = 0
  olen = LEN(odate)

  READ(odate(1:4),  '(I4)') yrold
  READ(odate(6:7),  '(I2)') moold
  READ(odate(9:10), '(I2)') dyold
  IF (olen.GE.13) THEN
     READ(odate(12:13),'(I2)') hrold
     IF (olen.GE.16) THEN
        READ(odate(15:16),'(I2)') miold
        IF (olen.GE.19) THEN
           READ(odate(18:19),'(I2)') scold
        END IF
     END IF
  END IF

  !  Break down new hdate into parts

  hrnew = 0
  minew = 0
  scnew = 0
  nlen = LEN(ndate)

  READ(ndate(1:4),  '(I4)') yrnew
  READ(ndate(6:7),  '(I2)') monew
  READ(ndate(9:10), '(I2)') dynew
  IF (nlen.GE.13) THEN
     READ(ndate(12:13),'(I2)') hrnew
     IF (nlen.GE.16) THEN
        READ(ndate(15:16),'(I2)') minew
        IF (nlen.GE.19) THEN
           READ(ndate(18:19),'(I2)') scnew
        END IF
     END IF
  END IF

  !  Check that the dates make sense.

  npass = .true.
  opass = .true.

  !  Check that the month of NDATE makes sense.

  IF ((monew.GT.12).or.(monew.LT.1)) THEN
     PRINT*, 'GETH_IDTS:  Month of NDATE = ', monew
     npass = .false.
  END IF

  !  Check that the month of ODATE makes sense.

  IF ((moold.GT.12).or.(moold.LT.1)) THEN
     PRINT*, 'GETH_IDTS:  Month of ODATE = ', moold
     opass = .false.
  END IF

  !  Check that the day of NDATE makes sense.

  IF (monew.ne.2) THEN
     ! ...... For all months but February
     IF ((dynew.GT.mday(monew)).or.(dynew.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of NDATE = ', dynew
        npass = .false.
     END IF
  ELSE IF (monew.eq.2) THEN
     ! ...... For February
     IF ((dynew.GT.ndfeb(yrnew)).OR.(dynew.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of NDATE = ', dynew
        npass = .false.
     END IF
  END IF

  !  Check that the day of ODATE makes sense.

  IF (moold.ne.2) THEN
     ! ...... For all months but February
     IF ((dyold.GT.mday(moold)).or.(dyold.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of ODATE = ', dyold
        opass = .false.
     END IF
  ELSE IF (moold.eq.2) THEN
     ! ....... For February
     IF ((dyold.GT.ndfeb(yrold)).or.(dyold.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of ODATE = ', dyold
        opass = .false.
     END IF
  END IF

  !  Check that the hour of NDATE makes sense.

  IF ((hrnew.GT.23).or.(hrnew.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Hour of NDATE = ', hrnew
     npass = .false.
  END IF

  !  Check that the hour of ODATE makes sense.

  IF ((hrold.GT.23).or.(hrold.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Hour of ODATE = ', hrold
     opass = .false.
  END IF

  !  Check that the minute of NDATE makes sense.

  IF ((minew.GT.59).or.(minew.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Minute of NDATE = ', minew
     npass = .false.
  END IF

  !  Check that the minute of ODATE makes sense.

  IF ((miold.GT.59).or.(miold.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Minute of ODATE = ', miold
     opass = .false.
  END IF

  !  Check that the second of NDATE makes sense.

  IF ((scnew.GT.59).or.(scnew.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  SECOND of NDATE = ', scnew
     npass = .false.
  END IF

  !  Check that the second of ODATE makes sense.

  IF ((scold.GT.59).or.(scold.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Second of ODATE = ', scold
     opass = .false.
  END IF

  IF (.not. npass) THEN
     WRITE( wrf_err_message , * ) &
          'module_date_time: geth_idts: Bad NDATE: ', ndate(1:nlen)
     CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
  END IF

  IF (.not. opass) THEN
     WRITE( wrf_err_message , * ) &
          'module_date_time: geth_idts: Bad ODATE: ', odate(1:olen)
     CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
  END IF

  !  Date Checks are completed.  Continue.

  !  Compute number of days from 1 January ODATE, 00:00:00 until ndate
  !  Compute number of hours from 1 January ODATE, 00:00:00 until ndate
  !  Compute number of minutes from 1 January ODATE, 00:00:00 until ndate

  newdys = 0
  DO i = yrold, yrnew - 1
     newdys = newdys + (365 + (ndfeb(i)-28))
  END DO

  IF (monew .GT. 1) THEN
     mday(2) = ndfeb(yrnew)
     DO i = 1, monew - 1
        newdys = newdys + mday(i)
     END DO
     mday(2) = 28
  END IF

  newdys = newdys + dynew-1

  !  Compute number of hours from 1 January ODATE, 00:00:00 until odate
  !  Compute number of minutes from 1 January ODATE, 00:00:00 until odate

  olddys = 0

  IF (moold .GT. 1) THEN
     mday(2) = ndfeb(yrold)
     DO i = 1, moold - 1
        olddys = olddys + mday(i)
     END DO
     mday(2) = 28
  END IF

  olddys = olddys + dyold-1

  !  Determine the time difference in seconds

  idts = (newdys - olddys) * 86400
  idts = idts + (hrnew - hrold) * 3600
  idts = idts + (minew - miold) * 60
  idts = idts + (scnew - scold)

  IF (isign .eq. -1) THEN
     tdate=ndate
     ndate=odate
     odate=tdate
     idts = idts * isign
  END IF

END SUBROUTINE geth_idts

!*****************************************************************************

SUBROUTINE build_string (string, Element, Value, Count, Status)

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

END SUBROUTINE build_string

!*****************************************************************************

SUBROUTINE get_dims(MemoryOrder, Start, End, ndim, x_start, x_end, y_start, &
     y_end, z_start, z_end)
  IMPLICIT NONE
  CHARACTER (LEN=*)    ,INTENT(IN)    :: MemoryOrder
  INTEGER              ,INTENT(OUT)   :: ndim,x_start,x_end,y_start
  INTEGER              ,INTENT(OUT)   :: y_end,z_start,z_end
  integer ,dimension(*),intent(in)    :: Start, End
  CHARACTER (LEN=1) :: char
  INTEGER           :: idx

  x_start = 1
  x_end   = 1
  y_start = 1
  y_end   = 1
  z_start = 1
  z_end   = 1

  !
  ! Note: Need to add "char == 'S'" for boundary conditions
  !

  ndim = 0

  !
  ! First, do the special boundary cases.  These do not seem to 
  !    
  if ((MemoryOrder(1:3) .eq. 'XSZ') &
       .or. (MemoryOrder(1:3) .eq. 'XEZ')) then
     x_start = Start(3)
     x_end = End(3)
     y_start = Start(1)
     y_end = End(1)
     z_start = Start(2)
     z_end = End(2)
  else if ((MemoryOrder(1:3) .eq. 'YSZ') .or. &
       (MemoryOrder(1:3) .eq. 'YEZ')) then
     x_start = Start(1)
     x_end = End(1)
     y_start = Start(3)
     y_end = End(3)
     z_start = Start(2)
     z_end = End(2)
  else if ((MemoryOrder(1:2) .eq. 'YS') .or. &
       (MemoryOrder(1:2) .eq. 'YE')) then
     x_start = Start(1)
     x_end = End(1)
     y_start = Start(2)
     y_end = End(2)
  else if ((MemoryOrder(1:2) .eq. 'XS') .or. &
       (MemoryOrder(1:2) .eq. 'XE')) then
     x_start = Start(2)
     x_end = End(2)
     y_start = Start(1)
     y_end = End(1)
  else
     do idx=1,len_trim(MemoryOrder)
        ndim = ndim + 1
        char = MemoryOrder(idx:idx)
        if ((char == 'X') .or. (char == 'x')) then
           x_start = Start(idx)
           x_end   = End(idx)
        else if ((char == 'Y') .or. (char == 'y')) then
           y_start = Start(idx)
           y_end   = End(idx)
        else if ((char == 'Z') .or. (char == 'z')) then
           z_start = Start(idx)
           z_end   = End(idx)
        else if (char == '0') then
           ! Do nothing, this indicates field is a scalar.
        else if ((char == 'C') .or. (char == 'c')) then
           ! Do nothing, this is a "non-decomposed" field.
        else
           call wrf_message('Invalid Dimension in get_dims: '//char)
        endif
     enddo
  endif

END SUBROUTINE get_dims

!*****************************************************************************

SUBROUTINE get_dimvals(MemoryOrder,x,y,z,dims)

  IMPLICIT NONE
  CHARACTER (LEN=*)    ,INTENT(IN)    :: MemoryOrder
  INTEGER              ,INTENT(IN)    :: x,y,z
  INTEGER, DIMENSION(*),INTENT(OUT)   :: dims
  INTEGER                             :: idx
  CHARACTER (LEN=1) :: char

  dims(1) = 1
  dims(2) = 1
  dims(3) = 1
  !
  ! Note: Need to add "char == 'S'" for boundary conditions
  !

  if ((MemoryOrder(1:3) .eq. 'XSZ') &
       .or. (MemoryOrder(1:3) .eq. 'XEZ')) then
     dims(1) = y
     dims(2) = z
     dims(3) = x
  else if ((MemoryOrder(1:3) .eq. 'YSZ') .or. &
       (MemoryOrder(1:3) .eq. 'YEZ')) then
     dims(1) = x
     dims(2) = z
     dims(3) = y
  else if ((MemoryOrder(1:2) .eq. 'YS') .or. &
       (MemoryOrder(1:2) .eq. 'YE')) then
     dims(1) = x
     dims(2) = y
     dims(3) = z
  else if ((MemoryOrder(1:2) .eq. 'XS') .or. &
       (MemoryOrder(1:2) .eq. 'XE')) then
     dims(1) = y
     dims(2) = x
     dims(3) = z
  else 
     do idx=1,len_trim(MemoryOrder)
        char = MemoryOrder(idx:idx)
        if ((char == 'X') .or. (char == 'x')) then
           dims(idx) = x
        else if ((char == 'Y') .or. (char == 'y')) then
           dims(idx) = y
        else if ((char == 'Z') .or. (char == 'z')) then
           dims(idx) = z
        else if (char == '0') then
           ! This is a scalar, do nothing.
        else if ((char == 'C') .or. (char == 'c')) then
           ! Do nothing, this is a "non-decomposed" field.
        else
           call wrf_message ('Invalid Dimension in get_dimvals: '//char)
        endif
     enddo
  endif

END SUBROUTINE get_dimvals

!*****************************************************************************

SUBROUTINE get_vert_stag(VarName,Stagger,vert_stag)
  
  character (LEN=*) :: VarName
  character (LEN=*) :: Stagger
  logical           :: vert_stag

  if ((index(Stagger,'Z') > 0) .or. (VarName .eq. 'DNW') &
       .or.(VarName .eq. 'RDNW')) then
     vert_stag = .true.
  else
     vert_stag = .false.
  endif
end SUBROUTINE

!*****************************************************************************

SUBROUTINE get_soil_layers(VarName,soil_layers)
  
  character (LEN=*) :: VarName
  logical           :: soil_layers

  if ((VarName .eq. 'ZS') .or. (VarName .eq. 'DZS') &
       .or.(VarName .eq. 'TSLB') .or. (VarName .eq. 'SMOIS') &
       .or. (VarName .eq. 'SH2O') .or. (VarName .eq. 'SMSTOT')) then
     soil_layers = .true.
  else
     soil_layers = .false.
  endif
end SUBROUTINE

!*****************************************************************************

SUBROUTINE get_levels(VarName, zidx, zsize, soil_layers, vert_stag, fraction, &
     vert_unit, level1, level2)

  use data_info
  IMPLICIT NONE

  integer :: zidx
  integer :: zsize
  logical :: soil_layers
  logical :: vert_stag
  logical :: fraction
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
  else if ((zsize .gt. 1) .and. (.not. soil_layers) .and. (.not. fraction)) &
       then
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
     else 
        vert_unit = 1
        level1 = 0
        level2 = 0
     endif
  endif

end SUBROUTINE get_levels

!*****************************************************************************


SUBROUTINE FILL_ETA_LEVELS(fileindex, FileFd, grib_tables, VarName, eta_levels)
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
        if (eta_levels(idx) > eta_levels(idx-1)) then
          tmp = eta_levels(idx)
          eta_levels(idx) = eta_levels(idx - 1)
          eta_levels(idx - 1) = tmp
          swapped = 1
        endif
     enddo
  enddo sortloop

end subroutine FILL_ETA_LEVELS

!*****************************************************************************

SUBROUTINE Transpose(MemoryOrder, di, FieldType, Field, &
     Start1, End1, Start2, End2, Start3, End3, data, zidx, numrows, numcols)

  IMPLICIT NONE

#include "wrf_io_flags.h"

  CHARACTER (LEN=*),INTENT(IN)    :: MemoryOrder
  INTEGER          ,INTENT(IN)    :: Start1,End1,Start2,End2,Start3,End3
  INTEGER          ,INTENT(IN)    :: di
  integer          ,intent(inout) :: &
       Field(di,Start1:End1,Start2:End2,Start3:End3)
  INTEGER          ,intent(in)    :: FieldType
  real             ,intent(in)    :: data(*)
  INTEGER          ,INTENT(IN)    :: zidx, numcols, numrows
  INTEGER, DIMENSION(3)           :: dims
  INTEGER                         :: col, row
  LOGICAL                         :: logicaltype
  CHARACTER (LEN=1000)            :: msg
     
!        Field(1:di,dims(1),dims(2),dims(3)) = data((row-1)*numcols+col)
! if ((FieldType == WRF_REAL) .or. (FieldType == WRF_DOUBLE)) then
     do col=1,numcols
        do row=1,numrows
           call get_dimvals(MemoryOrder,col,row,zidx,dims)
           Field(1:di,dims(1),dims(2),dims(3)) = &
                TRANSFER(data((row-1)*numcols+col),Field,1)
        enddo
     enddo
! else if (FieldType == WRF_INTEGER) then
!    do col=1,numcols
!       do row=1,numrows
!          call get_dimvals(MemoryOrder,col,row,zidx,dims)
!          Field(1:di,dims(1),dims(2),dims(3)) = data((row-1)*numcols+col)
!       enddo
!    enddo
! else if (FieldType == WRF_LOGICAL) then
!    do col=1,numcols
!       do row=1,numrows
!          call get_dimvals(MemoryOrder,col,row,zidx,dims)
!          Field(1:di,dims(1),dims(2),dims(3)) = &
!               TRANSFER(data((row-1)*numcols+col),logicaltype,1)
!       enddo
!    enddo
! else 
!    write (msg,*)'Reading of type ',FieldType,'from grib1 data not supported'
!    call wrf_message(msg)
! endif
  

end SUBROUTINE

