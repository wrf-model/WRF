PROGRAM test_io_grib2
  IMPLICIT NONE
#include "wrf_io_flags.h"

  integer            :: Status
  character(len=80)  :: SysDepInfo
  character(len=80)  :: FileName = 'test.gr2'
  integer            :: Comm
  integer            :: IOComm
  integer            :: DataHandle
  integer            :: FileStat
  real, DIMENSION( 1:1, 1:4, 1:4, 1:4) :: Field
  integer            :: DomainDesc
  character(len=3)   :: MemoryOrder = 'XZY'
  character(len=3)   :: Stagger = ''
  character(len=100) :: DimNames = ''
  integer, dimension(3) :: DomainStart, DomainEnd, MemoryStart, &
       MemoryEnd, PatchStart, PatchEnd

  DomainStart(1) = 1
  DomainStart(2) = 1
  DomainStart(3) = 1
  DomainEnd(1) = 4
  DomainEnd(2) = 4
  DomainEnd(3) = 4

  MemoryStart(1) = 1
  MemoryStart(2) = 1
  MemoryStart(3) = 1
  MemoryEnd(1) = 4
  MemoryEnd(2) = 4
  MemoryEnd(3) = 4

  PatchStart(1) = 1
  PatchStart(2) = 1
  PatchStart(3) = 1
  PatchEnd(1) = 4
  PatchEnd(2) = 4
  PatchEnd(3) = 4

  call ext_gr1_ioinit(SysDepInfo,Status)

  call ext_gr1_ioexit(Status)

  call ext_gr1_open_for_write_begin(FileName, Comm, IOComm, SysDepInfo, &
     DataHandle, Status)

  call ext_gr1_open_for_write_commit( DataHandle , Status )

  call ext_gr1_inquire_opened ( DataHandle, FileName , FileStat, Status )

  Field(1,1,1,1) = 2.1
  call ext_gr1_write_field ( DataHandle, "2005-07-27_12:00:00","POT",Field, &
       WRF_REAL, Comm, IOComm, DomainDesc, MemoryOrder, Stagger, &
       DimNames, DomainStart, DomainEnd, MemoryStart, MemoryEnd, &
       PatchStart, PatchEnd, Status)


  call ext_gr1_ioclose ( DataHandle, Status )

END PROGRAM
