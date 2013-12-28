subroutine ext_out4wave_fail(cstatus)
  implicit none
  integer, intent(in) :: cstatus
  character*255 :: message
  select case(cstatus)
  case(2000) 
     call wrf_error_fatal('out4wave failed: too many files opened')
  case(2001)
     call wrf_error_fatal('out4wave failed: cannot open file')
  case(2002)
     call wrf_error_fatal('out4wave failed: cannot seek')
  case(2003)
     call wrf_error_fatal('out4wave failed: cannot lock file')
  case(2004)
     call wrf_error_fatal('out4wave failed: dimensions too small (smaller than 2x2)')
  case(2005)
     call wrf_error_fatal('out4wave failed: cannot allocate memory')
  case(2006)
     call wrf_error_fatal('out4wave failed: cannot write to file')
  case(2007)
     call wrf_error_fatal('out4wave failed: cannot unlock file')
  case(2008)
     call wrf_error_fatal('out4wave failed: error closing file (some data may not have been written)')
  case default
     write(message,"('out4wave failed: unknown status ',I0)") cstatus
     call wrf_error_fatal(trim(message))
  end select
end subroutine ext_out4wave_fail

subroutine ext_out4wave_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )
  implicit none
#include "wrf_io_flags.h"
  integer ,       intent(in)    :: DataHandle 
  character*(*) :: DateStr
  character*(*) :: VarName
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
  real, dimension(*)    :: Field
  integer :: flen

  Status=0
  if(FieldType /= WRF_REAL) return ! ignore all but REAL variables
  flen=len_trim(VarName)
  call c_out4wave_field(DataHandle,Status,Field,MemoryStart,MemoryEnd, &
       DomainStart,DomainEnd,PatchStart,PatchEnd,VarName,flen)
  if(Status/=0) call ext_out4wave_fail(Status)
end subroutine ext_out4wave_write_field

subroutine ext_out4wave_ioinit(SysDepInfo, Status)
  implicit none
  character*(*), intent(in) :: SysDepInfo
  integer Status
  call c_out4wave_init(Status)
end subroutine ext_out4wave_ioinit

subroutine ext_out4wave_open_for_read_begin( & ! OUT4WAVE CANNOT READ!
     FileName, Comm_compute, Comm_io, SysDepInfo, DataHandle, Status)
  implicit none
  character*(*) :: FileName
  integer ,       intent(in)  :: Comm_compute , Comm_io
  character*(*) :: SysDepInfo
  integer ,       intent(out) :: DataHandle
  integer ,       intent(out) :: Status
  DataHandle=-1
  Status=-1
  call wrf_error_fatal('out4wave does not support reading')
end subroutine ext_out4wave_open_for_read_begin

subroutine ext_out4wave_open_for_write_begin( &
     FileName , Comm_compute, Comm_io, SysDepInfo, &
     DataHandle , Status )
  implicit none
  character*(*) :: FileName
  integer ,       intent(in)  :: Comm_compute , Comm_io
  character*(*) :: SysDepInfo
  integer ,       intent(out) :: DataHandle
  integer ,       intent(inout) :: Status
  integer :: flen
  flen=len_trim(FileName)
  call c_out4wave_open(FileName,flen,DataHandle,Status)
  if(Status/=0) call ext_out4wave_fail(Status)
end subroutine ext_out4wave_open_for_write_begin

subroutine ext_out4wave_inquire_opened(DataHandle,FileName,FileStatus,Status)
  implicit none
#include "wrf_io_flags.h"
  integer ,       intent(in)  :: DataHandle
  character*(*) :: FileName
  integer ,       intent(out) :: FileStatus
  integer ,       intent(out) :: Status
  integer :: opened, dryrun
  Status=0
  call c_out4wave_inquire(DataHandle,FileName,0,opened,dryrun)
  if(Status/=0) call ext_out4wave_fail(Status)
  if(opened) then
     if(dryrun/=0) then
        FileStatus=WRF_FILE_OPENED_NOT_COMMITTED
     else
        FileStatus=WRF_FILE_OPENED_FOR_WRITE
     endif
  else
     FileStatus=WRF_FILE_NOT_OPENED
  endif
end subroutine ext_out4wave_inquire_opened

subroutine ext_out4wave_inquire_filename(DataHandle,FileName,FileStatus,Status)
  implicit none
#include "wrf_io_flags.h"
  integer ,       intent(in)  :: DataHandle
  character*(*) :: FileName
  integer ,       intent(out) :: FileStatus
  integer ,       intent(out) :: Status
  integer :: opened, flen, dryrun
  Status=0
  flen=len(filename)
  call c_out4wave_inquire(DataHandle,FileName,flen,opened,dryrun)
  if(Status/=0) call ext_out4wave_fail(Status)
  if(opened/=0) then
     if(dryrun/=0) then
        FileStatus=WRF_FILE_OPENED_NOT_COMMITTED
     else
        FileStatus=WRF_FILE_OPENED_FOR_WRITE
     endif
  else
     FileStatus=WRF_FILE_NOT_OPENED
  endif
end subroutine ext_out4wave_inquire_filename

subroutine ext_out4wave_iosync ( DataHandle, Status )
  implicit none
  integer, intent(in) :: DataHandle
  integer, intent(out) :: Status
  call c_out4wave_flush(DataHandle,Status)
  if(Status/=0) call ext_out4wave_fail(Status)
end subroutine ext_out4wave_iosync

subroutine ext_out4wave_ioclose ( DataHandle, Status )
  implicit none
  integer, intent(in) :: DataHandle
  integer, intent(out) :: Status
  Status=0
  call c_out4wave_close(DataHandle,Status)
  if(Status/=0) call ext_out4wave_fail(Status)
end subroutine ext_out4wave_ioclose

subroutine ext_out4wave_ioexit( Status )
  implicit none
  integer ,       intent(out) :: Status
  call c_out4wave_ioexit(Status)
end subroutine ext_out4wave_ioexit

subroutine ext_out4wave_open_for_write_commit( DataHandle , Status )
 implicit none
  integer ,       intent(in ) :: DataHandle
  integer ,       intent(out) :: Status
  Status=0
  call c_out4wave_commit(DataHandle,Status)
  if(Status/=0) call ext_out4wave_fail(Status)
end subroutine ext_out4wave_open_for_write_commit
