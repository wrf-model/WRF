!*-----------------------------------------------------------------------------
!*
!*  Todd Hutchinson
!*  WSI
!*  4 Federal Street
!*  Billerica, MA     01821
!*  thutchinson@wsi.com
!*
!*-----------------------------------------------------------------------------

!*
!* This module will hold data internal to this I/O implementation.  
!*   The variables will be accessible by all functions (provided they have a 
!*   "USE data_info" line).
!*

module data_info
  INTEGER :: FileHandles = 0
  INTEGER :: firstFileHandle = 8
  INTEGER,PARAMETER :: maxFileHandles = 200
  LOGICAL, DIMENSION(maxFileHandles) :: committed, opened, used
  CHARACTER*128, DIMENSION(maxFileHandles) :: DataFile
  CHARACTER*128, DIMENSION(maxFileHandles) :: FileFd
  CHARACTER*24 :: StartDate
  REAL, DIMENSION(1000) :: half_eta, full_eta
  REAL, DIMENSION(100) :: soil_depth, soil_thickness
  INTEGER :: projection
  REAL :: dx,dy
  REAL :: truelat1, truelat2
  REAL :: center_lat, center_lon
  REAL :: proj_central_lon
  REAL :: timestep
  CHARACTER, DIMENSION(:), POINTER :: grib_table_info
  INTEGER, DIMENSION(500) :: domains
  INTEGER :: max_domain = 0
  TYPE :: prevdata
     INTEGER :: fcst_secs_rainc
     INTEGER :: fcst_secs_rainnc
     REAL, DIMENSION(:,:), POINTER :: rainc, rainnc
  END TYPE prevdata
  TYPE (prevdata), DIMENSION(500) :: lastdata
  TYPE :: prestype
     REAL, DIMENSION(:,:,:), POINTER :: vals
     LOGICAL :: newtime
     CHARACTER*120 :: lastDateStr
  END TYPE prestype
  TYPE (prestype), DIMENSION(500) :: pressure
end module data_info


subroutine ext_yyy_ioinit(SysDepInfo,Status)

  USE data_info
  implicit none
  include 'wrf_status_codes.h'
  CHARACTER*(*), INTENT(IN) :: SysDepInfo
  integer ,intent(out) :: Status
  integer :: i

  do i=firstFileHandle, maxFileHandles
        used(i) = .false.
        committed(i) = .false.
        opened(i) = .false.
  enddo
  domains(:) = -1

  do i = 1, 500
    lastdata(i)%fcst_secs_rainc = 0
    lastdata(i)%fcst_secs_rainnc = 0
    pressure(i)%newtime = .false.
    pressure(i)%lastDateStr = ''
  enddo

  Status = WRF_NO_ERR

  return
end subroutine ext_yyy_ioinit


subroutine ext_yyy_ioexit(Status)

  USE data_info
  implicit none
  include 'wrf_status_codes.h'
  integer ,intent(out) :: Status

  Status = WRF_NO_ERR

  return
end subroutine ext_yyy_ioexit

! added stub, Aug 2004, JM
SUBROUTINE ext_yyy_open_for_read_begin ( FileName , Comm_compute, Comm_io, &
     SysDepInfo, DataHandle , Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_open_for_read_begin not supported yet')

  DataHandle = 1
  DataFile(DataHandle) = TRIM(FileName)
  committed(DataHandle) = .false.
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_open_for_read_begin

! added stub, Aug 2004, JM
SUBROUTINE ext_yyy_open_for_read_commit( DataHandle , Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_open_for_read_commit

SUBROUTINE ext_yyy_open_for_read ( FileName , Comm_compute, Comm_io, &
     SysDepInfo, DataHandle , Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_open_for_read not supported yet')
  
  DataHandle = 1
  DataFile(DataHandle) = TRIM(FileName)
  committed(DataHandle) = .false.
  Status = WRF_NO_ERR

  RETURN  
END SUBROUTINE ext_yyy_open_for_read


SUBROUTINE ext_yyy_open_for_write_begin(FileName, Comm, IOComm, SysDepInfo, &
     DataHandle, Status)
  
  USE data_info
  implicit none
  include 'wrf_status_codes.h'

  character*(*)        ,intent(in)  :: FileName
  integer              ,intent(in)  :: Comm
  integer              ,intent(in)  :: IOComm
  character*(*)        ,intent(in)  :: SysDepInfo
  integer              ,intent(out) :: DataHandle
  integer              ,intent(out) :: Status
  integer :: ierr

  Status = WRF_NO_ERR
  CALL get_new_handle(DataHandle)
  if (DataHandle .GT. 0) then
     CALL open_file(TRIM(FileName), FileFd(DataHandle), ierr)
     if (ierr .ne. 0) then
        Status = WRF_ERR_WARN_WRITE_RONLY_FILE
     else
        opened(DataHandle) = .true.
        DataFile(DataHandle) = TRIM(FileName)
     endif
     committed(DataHandle) = .false.
  else
     Status = WRF_ERR_WARN_TOO_MANY_FILES
  endif

  RETURN  
END SUBROUTINE ext_yyy_open_for_write_begin


SUBROUTINE ext_yyy_open_for_write_commit( DataHandle , Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  IF ( opened( DataHandle ) ) THEN
    IF ( used( DataHandle ) ) THEN
      committed(DataHandle) = .true.
    ENDIF
  ENDIF

  Status = WRF_NO_ERR

  RETURN  
END SUBROUTINE ext_yyy_open_for_write_commit


SUBROUTINE ext_yyy_inquire_opened ( DataHandle, FileName , FileStatus, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  include 'wrf_io_flags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status

  FileStatus = WRF_ERR_WARN_FILE_NOT_OPENED
  if ((DataHandle .ge. firstFileHandle) .and. &
       (DataHandle .le. maxFileHandles)) then
     if (used(DataHandle) .and. committed(DataHandle)) then
        FileStatus = WRF_FILE_OPENED_AND_COMMITTED
     else if (used(DataHandle) .and. .not. committed(DataHandle)) then
        FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
     endif
  endif
  
  Status = WRF_NO_ERR 

  RETURN
END SUBROUTINE ext_yyy_inquire_opened


SUBROUTINE ext_yyy_ioclose ( DataHandle, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER DataHandle, Status
  INTEGER istat

  Status = WRF_NO_ERR

  CALL close_file(FileFd(DataHandle))
  used(DataHandle) = .false.
  DEALLOCATE(grib_table_info, stat=istat)

  RETURN
END SUBROUTINE ext_yyy_ioclose

SUBROUTINE ext_yyy_write_field( DataHandle , DateStr , VarName , &
     Field , FieldType , Comm , IOComm, &
     DomainDesc , MemoryOrder , Stagger , &
     DimNames , &
     DomainStart , DomainEnd , &
     MemoryStart , MemoryEnd , &
     PatchStart , PatchEnd , &
     Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  include 'wrf_io_flags.h'
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  CHARACTER*120 :: OutName
  CHARACTER(120) :: varname2
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
  character, pointer :: pds
  integer :: xsize, ysize, zsize
  integer :: x, y, z
  integer :: idx
  integer :: south
  logical :: vert_stag = .false.
  integer :: levelnum
  real, DIMENSION(:,:), POINTER :: data,tmpdata
  integer, DIMENSION(:), POINTER :: mold
  integer :: istat
  integer :: accum_period
  integer :: size
  real, dimension(1000) :: level1, level2
  real, DIMENSION( MemoryStart(1):MemoryEnd(1), &
                   MemoryStart(2):MemoryEnd(2), &
                   MemoryStart(3):MemoryEnd(3) ) :: Field
  integer :: fcst_secs
  logical :: soil_layers
  integer :: vert_unit
  integer :: abc(2,2,2)
  integer :: def(8)
  logical :: output = .true.
  integer :: idx1, idx2, idx3
  integer :: this_domain
  logical :: new_domain

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
  do idx = 1, len(MemoryOrder)
     if (MemoryOrder(idx:idx) .eq. 'X') then
        xsize = PatchEnd(idx) - PatchStart(idx) + 1
     endif
     if (MemoryOrder(idx:idx) .eq. 'Y') then
        ysize = PatchEnd(idx) - PatchStart(idx) + 1
     endif
     if (MemoryOrder(idx:idx) .eq. 'Z') then
        zsize = PatchEnd(idx) - PatchStart(idx) + 1
        if (DimNames(idx) .eq. 'soil_layers_stag') then
           soil_layers = .true.
        endif
     endif
  enddo

  if (.not. ASSOCIATED(grib_table_info)) then
     CALL get_grib_struct_size(size)
     ALLOCATE(grib_table_info(1:size), STAT=istat)
     CALL read_gribmap("gribmap.txt",grib_table_info,istat)
     if (istat .eq. -1) then
        DEALLOCATE(grib_table_info)
        Status = WRF_ERR_FATAL_BAD_FILE_STATUS
        return
     endif
  endif

  if (new_domain) then
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
        half_eta(idx) = Field(idx,1,1)
     enddo
  endif

  if (OutName .eq. 'ZNW') then
     do idx = 1, zsize
        full_eta(idx) = Field(idx,1,1)
     enddo
  endif

  if (OutName .eq. 'ZS') then
     do idx = 1, zsize
        soil_depth(idx) = Field(idx,1,1)
     enddo
  endif

  if (OutName .eq. 'DZS') then
     do idx = 1, zsize
        soil_thickness(idx) = Field(idx,1,1)
     enddo
  endif


  if ((xsize .lt. 1) .or. (ysize .lt. 1)) then
     write(msg,*) 'Cannot output field with memory order: ', &
          MemoryOrder,Varname
     call wrf_message(msg)
     return
  endif
     
  if ((index(Stagger,'Z') > 0) .or. (OutName .eq. 'DNW') .or. &
      (OutName .eq. 'RDNW')) then
     vert_stag = .true.
  else
     vert_stag = .false.
  endif

  if ((zsize .gt. 1) .and. (.not. soil_layers)) then
     do idx = 1, zsize     
        if (vert_stag) then
           level1(idx) = full_eta(idx)
        else
           level1(idx) = half_eta(idx)
        endif
        level2(idx) = 0;
     enddo
     vert_unit = 119;
  else
     ! Set the vertical coordinate and level for soil and 2D fields
     if (len(OutName) .gt. 1) then
         varname2=OutName(1:2)
     else
         varname2=OutName
     endif
     if (soil_layers) then
        do idx = 1, zsize
           vert_unit = 112;
           level1(idx) = (soil_depth(idx) - 0.5*soil_thickness(idx))*100
           level2(idx) = (soil_depth(idx) + 0.5*soil_thickness(idx))*100
        enddo
     else if (OutName .eq. 'mu') then
        vert_unit = 200;
        level1(1) = 0;
        level2(1) = 0;
     else if ((OutName .eq. 'Q2') .or. (OutName .eq. 'TH2') .or. &
        (OutName .eq. 'T2')) then
        vert_unit = 105;
        level1(1) = 2;
        level2(1) = 0;
     else if ((OutName .eq. 'Q10') .or. (OutName .eq. 'TH10') .or. &
          (OutName .eq. 'U10') .or. (OutName .eq. 'V10')) then
        vert_unit = 105;
        level1(1) = 10;
        level2(10) = 0;
     else 
        vert_unit = 105;
        level1(1) = 0;
        level2(1) = 0;
     endif
  endif

  if ( .not. opened(DataHandle)) then
     Status = WRF_ERR_WARN_FILE_NOT_OPENED
     return
  endif


  if (opened(DataHandle) .and. committed(DataHandle)) then
     ! 
     ! The following is a kludge to output full pressure instead of the two 
     !  fields of base-state pressure and pressure perturbation.
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
           pressure(this_domain)%vals = Field(:,:,:)
           pressure(this_domain)%newtime = .false.
           output = .false.
        else 
           output = .true.
        endif
        pressure(this_domain)%lastDateStr=DateStr
     endif

     if (output) then 
        CALL geth_idts(DateStr,StartDate,fcst_secs)
        
        nullify(pds)
        if (center_lat .lt. 0) then
           south = 1
        else
           south = 0
        endif
         
       do z = 1, zsize
           SELECT CASE (MemoryOrder)
           CASE ('XYZ')
              data = Field(1:xsize,1:ysize,z)
           CASE ('XZY')
              data = Field(1:xsize,z,1:ysize)
           CASE ('YXZ')
              do x = 1,xsize
                 do y = 1,ysize
                    data(x,y) = Field(y,x,z)
                 enddo
              enddo
           CASE ('YZX')
              do x = 1,xsize
                 do y = 1,ysize
                    data(x,y) = Field(y,z,x)
                 enddo
              enddo
           CASE ('ZXY')
              data = Field(z,1:xsize,1:ysize)
           CASE ('ZYX')
              do x = 1,xsize
                 do y = 1,ysize
                    data(x,y) = Field(z,y,x)
                 enddo
              enddo
           CASE ('XY')
              data = Field(1:xsize,1:ysize,1)
           CASE ('YX')
              do x = 1,xsize
                 do y = 1,ysize
                    data(x,y) = Field(y,x,1)
                 enddo
              enddo
           CASE ('Z')
              data(1,1) = Field(z,1,1)
           CASE ('0')
              data(1,1) = Field(1,1,1)
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
           !
           ! Just add 300 to Potential Temperature perturbation to get full 
           !   potential temperature.
           !
           if (OutName == 'T') then
              data = data + 300
           endif
           ! 
           ! For precip, we setup the accumulation period, and output a precip
           !    rate for time-step precip.
           !
           if ((OutName .eq. 'RAINC') .or. (OutName .eq. 'RAINNC')) then
              accum_period = fcst_secs
           else if ((OutName .eq. 'RAINCV') .or. (OutName .eq. 'RAINNCV')) then
              ! Convert time-step precip to precip rate.
              data = data/timestep
              accum_period = 0
           else
              accum_period = 0
           endif

           if ((OutName .eq. 'P') .or. (OutName.eq.'PB')) then
              if (idx3 .eq. 1) then
                 data = data + pressure(this_domain)%vals(z,patchstart(2):patchend(2),patchstart(3):patchend(3))
              elseif (idx3 .eq. 2) then
                 data = data + pressure(this_domain)%vals(patchstart(1):patchend(1),z,patchstart(3):patchend(3))
              elseif (idx3 .eq. 3) then
                 data = data + pressure(this_domain)%vals(patchstart(1):patchend(1),patchstart(2):patchend(2),z)
              else
                 print *,'error in idx3, continuing'
              endif

              OutName = 'P'
           endif

           !    Output each level
           
           call write_grib(OutName, level1(z), level2(z), &
                vert_unit, StartDate, &
                fcst_secs, accum_period, projection, xsize, ysize, &
                center_lat, center_lon, proj_central_lon, &
                dx, dy, south, truelat1, truelat2, &
                data, grib_table_info, FileFd(DataHandle))
           
           !
           ! If this is the total accumulated rain, call write_grib again 
           !   to output the accumulation since the last output time as well.
           !   This is somewhat of a kludge to meet the requirements of PF.
           !
           if ((OutName .eq. 'RAINC') .or. (OutName .eq. 'RAINNC')) then
              if (OutName .eq. 'RAINC') then
                 tmpdata(:,:) = data(:,:)
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
              endif
              call write_grib(TmpVarName, level1(z), level2(z), &
                   vert_unit, StartDate, &
                   fcst_secs, accum_period, projection, xsize, ysize, &
                   center_lat, center_lon, proj_central_lon, &
                   dx, dy, south, truelat1, truelat2, &
                   data, grib_table_info, FileFd(DataHandle))
           endif
        enddo
     endif
  endif

  deallocate(data, STAT = istat)
  deallocate(mold, STAT = istat)
  deallocate(tmpdata, STAT = istat)

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_write_field


SUBROUTINE ext_yyy_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
  USE data_info
  IMPLICIT NONE  
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
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
  REAL, DIMENSION(*)    :: Field

  CALL wrf_message('ext_yyy_read_field not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_read_field


SUBROUTINE ext_yyy_get_next_var ( DataHandle, VarName, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_next_var not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_next_var


subroutine ext_yyy_end_of_frame(DataHandle, Status)

  USE data_info
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  integer               ,intent(out)    :: Status

  Status = WRF_NO_ERR

  return
end subroutine ext_yyy_end_of_frame


SUBROUTINE ext_yyy_iosync ( DataHandle, Status )

  USE data_info  
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  integer                     :: ierror

  Status = WRF_NO_ERR
  if (DataHandle .GT. 0) then
     CALL flush_file(FileFd(DataHandle), ierror)
     if (ierror .ne. 0) then
        Status = WRF_ERR_WARN_WRITE_RONLY_FILE
     endif
  else
     Status = WRF_ERR_WARN_TOO_MANY_FILES
  endif

  RETURN
END SUBROUTINE ext_yyy_iosync


SUBROUTINE ext_yyy_inquire_filename ( DataHandle, FileName , FileStatus, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  include 'wrf_io_flags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER *80   SysDepInfo

  FileStatus = WRF_ERR_WARN_FILE_NOT_OPENED
  if ((DataHandle .ge. firstFileHandle) .and. &
       (DataHandle .le. maxFileHandles)) then
     if (used(DataHandle) .and. committed(DataHandle)) then
        FileStatus = WRF_FILE_OPENED_AND_COMMITTED
     else if (used(DataHandle) .and. .not. committed(DataHandle)) then
        FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
     endif
  endif
  
  Status = WRF_NO_ERR 
  
  FileName = DataFile(DataHandle) 

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_inquire_filename


SUBROUTINE ext_yyy_get_var_info ( DataHandle , VarName , NDim , &
     MemoryOrder , Stagger , DomainStart , DomainEnd , WrfType, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: VarName
  integer               ,intent(out)    :: NDim
  character*(*)         ,intent(out)    :: MemoryOrder
  character*(*)         ,intent(out)    :: Stagger
  integer ,dimension(*) ,intent(out)    :: DomainStart, DomainEnd
  integer               ,intent(out)    :: WrfType
  integer               ,intent(out)    :: Status

  CALL wrf_message('ext_yyy_get_var_info not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_var_info


SUBROUTINE ext_yyy_set_time ( DataHandle, DateStr, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_set_time not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_set_time


SUBROUTINE ext_yyy_get_next_time ( DataHandle, DateStr, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_next_time not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_next_time


SUBROUTINE ext_yyy_get_previous_time ( DataHandle, DateStr, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_previous_time not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_previous_time


SUBROUTINE ext_yyy_get_var_ti_real ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)    :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real ,          INTENT(OUT)   :: Data(*)
  INTEGER ,       INTENT(IN)    :: Count
  INTEGER ,       INTENT(OUT)   :: OutCount
  INTEGER ,       INTENT(OUT)   :: Status

  CALL wrf_message('ext_yyy_get_var_ti_real not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_var_ti_real 



SUBROUTINE ext_yyy_get_var_ti_real8 ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)      :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real*8 ,        INTENT(OUT)     :: Data(*)
  INTEGER ,       INTENT(IN)      :: Count
  INTEGER ,       INTENT(OUT)     :: OutCount
  INTEGER ,       INTENT(OUT)     :: Status

  CALL wrf_message('ext_yyy_get_var_ti_real8 not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_var_ti_real8 



SUBROUTINE ext_yyy_get_var_ti_integer ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)       :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  integer ,       INTENT(OUT)      :: Data(*)
  INTEGER ,       INTENT(IN)       :: Count
  INTEGER ,       INTENT(OUT)      :: OutCount
  INTEGER ,       INTENT(OUT)      :: Status

  CALL wrf_message('ext_yyy_get_var_ti_integer not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_var_ti_integer 

SUBROUTINE ext_yyy_get_var_ti_logical ( DataHandle,Element,  Varname, Data, &
     Count, Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)       :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  logical ,       INTENT(OUT)      :: Data(*)
  INTEGER ,       INTENT(IN)       :: Count
  INTEGER ,       INTENT(OUT)      :: OutCount
  INTEGER ,       INTENT(OUT)      :: Status

  CALL wrf_message('ext_yyy_get_var_ti_logical not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_var_ti_logical 


SUBROUTINE ext_yyy_get_var_ti_char ( DataHandle,Element,  Varname, Data,  &
     Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_var_ti_char not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_var_ti_char 


SUBROUTINE ext_yyy_put_var_ti_real ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real ,          INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_var_ti_real 

SUBROUTINE ext_yyy_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_yyy_get_var_ti_double

SUBROUTINE ext_yyy_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )
  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_yyy_put_var_ti_double

SUBROUTINE ext_yyy_put_var_ti_real8 ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real*8 ,        INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_var_ti_real8 

SUBROUTINE ext_yyy_put_var_ti_integer ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  integer ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_var_ti_integer 


SUBROUTINE ext_yyy_put_var_ti_logical ( DataHandle,Element,  Varname, Data, &
     Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  logical ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

RETURN
END SUBROUTINE ext_yyy_put_var_ti_logical 


SUBROUTINE ext_yyy_put_var_ti_char ( DataHandle,Element,  Varname, Data,  &
     Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy
  INTEGER                     :: Count

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_var_ti_char 


SUBROUTINE ext_yyy_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_yyy_get_var_td_double

SUBROUTINE ext_yyy_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_yyy_put_var_td_double

SUBROUTINE ext_yyy_get_var_td_real ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real ,          INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_var_td_real not supported yet')
  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_get_var_td_real 

SUBROUTINE ext_yyy_get_var_td_real8 ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real*8 ,        INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_var_td_real8 not supported yet')
  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_get_var_td_real8 


SUBROUTINE ext_yyy_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  integer ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_var_td_integer not supported yet')
  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_get_var_td_integer 


SUBROUTINE ext_yyy_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, &
     Data, Count, Outcount, Status )
  
  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  logical ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_var_td_logical not supported yet')
  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_get_var_td_logical 


SUBROUTINE ext_yyy_get_var_td_char ( DataHandle,Element,  DateStr,Varname, &
     Data,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_var_td_char not supported yet')
  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_get_var_td_char 


SUBROUTINE ext_yyy_put_var_td_integer ( DataHandle,Element,  DateStr, &
     Varname, Data, Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  integer ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR  

RETURN
END SUBROUTINE ext_yyy_put_var_td_integer 


SUBROUTINE ext_yyy_put_var_td_real ( DataHandle,Element,  DateStr,Varname, &
     Data, Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real ,          INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_put_var_td_real 


SUBROUTINE ext_yyy_put_var_td_real8 ( DataHandle,Element,  DateStr,Varname, &
     Data, Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real*8 ,        INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_put_var_td_real8 


SUBROUTINE ext_yyy_put_var_td_logical ( DataHandle,Element,  DateStr, &
     Varname, Data, Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  logical ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_put_var_td_logical 


SUBROUTINE ext_yyy_put_var_td_char ( DataHandle,Element,  DateStr,Varname, &
     Data,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_put_var_td_char 


SUBROUTINE ext_yyy_get_dom_ti_real ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE data_info
  IMPLICIT NONE
  INCLUDE 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real ,          INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Outcount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_dom_ti_real not supported yet')
  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_get_dom_ti_real 


SUBROUTINE ext_yyy_get_dom_ti_real8 ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real*8 ,        INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_dom_ti_real8 not supported yet')
  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_get_dom_ti_real8 


SUBROUTINE ext_yyy_get_dom_ti_integer ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  integer ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_dom_ti_integer not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_dom_ti_integer 


SUBROUTINE ext_yyy_get_dom_ti_logical ( DataHandle,Element,   Data, Count, &
     Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  logical ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_dom_ti_logical not supported yet')
  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_get_dom_ti_logical 


SUBROUTINE ext_yyy_get_dom_ti_char ( DataHandle,Element,   Data,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_dom_ti_char not supported yet')
  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_get_dom_ti_char 

SUBROUTINE ext_yyy_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )
  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_dom_ti_double not supported yet')
  Status = WRF_NO_ERR  

RETURN
END SUBROUTINE ext_yyy_get_dom_ti_double


SUBROUTINE ext_yyy_put_dom_ti_real ( DataHandle,Element,   Data, Count,  &
     Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real ,          INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy

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

  Status = WRF_NO_ERR  

  RETURN
END SUBROUTINE ext_yyy_put_dom_ti_real 


SUBROUTINE ext_yyy_put_dom_ti_real8 ( DataHandle,Element,   Data, Count,  &
     Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real*8 ,        INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_dom_ti_real8 


SUBROUTINE ext_yyy_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  &
     Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  INTEGER ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy

  if (Element == 'MAP_PROJ') then
     projection = Data(1)
  endif

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_dom_ti_integer 

SUBROUTINE ext_yyy_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  &
     Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  logical ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_dom_ti_logical 


SUBROUTINE ext_yyy_put_dom_ti_char ( DataHandle,Element,   Data,  &
     Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*),     INTENT(IN)  :: Data
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy

  if (Element .eq. 'START_DATE') then
     StartDate = Data
  endif
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_dom_ti_char

SUBROUTINE ext_yyy_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )
  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_put_dom_ti_double not supported yet')
  Status = WRF_NO_ERR  

RETURN
END SUBROUTINE ext_yyy_put_dom_ti_double

SUBROUTINE ext_yyy_get_dom_td_real ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real ,          INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_dom_td_real not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_dom_td_real 


SUBROUTINE ext_yyy_get_dom_td_real8 ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real*8 ,        INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_dom_td_real8 not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_dom_td_real8 


SUBROUTINE ext_yyy_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  integer ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_dom_td_integer not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_dom_td_integer 


SUBROUTINE ext_yyy_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, &
     Count, Outcount, Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  logical ,       INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: OutCount
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_dom_td_logical not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_dom_td_logical 


SUBROUTINE ext_yyy_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  &
     Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_message('ext_yyy_get_dom_td_char not supported yet')
  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_get_dom_td_char 


SUBROUTINE ext_yyy_put_dom_td_real ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real ,          INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_dom_td_real 

SUBROUTINE ext_yyy_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_yyy_get_dom_td_double

SUBROUTINE ext_yyy_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )
  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_yyy_put_dom_td_double

SUBROUTINE ext_yyy_put_dom_td_real8 ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real*8 ,        INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_dom_td_real8 


SUBROUTINE ext_yyy_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  integer ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_dom_td_integer


SUBROUTINE ext_yyy_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, &
     Count,  Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  logical ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_dom_td_logical


SUBROUTINE ext_yyy_put_dom_td_char ( DataHandle,Element, DateStr,  Data, &
     Status )

  USE data_info
  IMPLICIT NONE
  include 'wrf_status_codes.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER ,     INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_NO_ERR

  RETURN
END SUBROUTINE ext_yyy_put_dom_td_char 

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
      INTEGER           , INTENT(OUT)   :: idts
      
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
         WRITE( wrf_err_message , * ) 'module_date_time: geth_idts: Bad NDATE: ', ndate(1:nlen)
         CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
      END IF
      
      IF (.not. opass) THEN
         WRITE( wrf_err_message , * ) 'module_date_time: geth_idts: Bad ODATE: ', odate(1:olen)
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

