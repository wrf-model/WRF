MODULE module_internal_header_util

!<DESCRIPTION>
!<PRE>
! Subroutines defined in this module are used to generate (put together) and get (take apart) 
! data headers stored in the form of integer vectors.
! 
! Data headers serve two purposes:  
!   - Provide a package-independent metadata storage and retrieval mechanism 
!     for I/O packages that do not support native metadata.  
!   - Provide a mechanism for communicating I/O commands from compute 
!     tasks to quilt tasks when I/O quilt servers are enabled.  
! 
! Within a data header, character strings are stored one character per integer.  
! The number of characters is stored immediately before the first character of 
! each string.
!
! In an I/O package that does not support native metadata, routines 
! int_gen_*_header() are called to pack information into data headers that 
! are then written to files.  Routines int_get_*_header() are called to 
! extract information from a data headers after they have been read from a 
! file.  
!
! When I/O quilt server tasks are used, routines int_gen_*_header() 
! are called by compute tasks to pack information into data headers 
! (commands) that are then sent to the I/O quilt servers.  Routines 
! int_get_*_header() are called by I/O quilt servers to extract 
! information from data headers (commands) received from the compute 
! tasks.  
!
!</PRE>
!</DESCRIPTION>

INTERFACE int_get_ti_header
   MODULE PROCEDURE int_get_ti_header_integer, int_get_ti_header_real
END INTERFACE
INTERFACE int_gen_ti_header
   MODULE PROCEDURE int_gen_ti_header_integer, int_gen_ti_header_real
END INTERFACE
INTERFACE int_get_td_header
   MODULE PROCEDURE int_get_td_header_integer, int_get_td_header_real
END INTERFACE
INTERFACE int_gen_td_header
   MODULE PROCEDURE int_gen_td_header_integer, int_gen_td_header_real
END INTERFACE

PRIVATE :: int_pack_string, int_unpack_string

CONTAINS
!!!!!!!!!!!!! header manipulation routines !!!!!!!!!!!!!!!

INTEGER FUNCTION get_hdr_tag( hdrbuf )
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: hdrbuf(*)
  get_hdr_tag = hdrbuf(2)
  RETURN
END FUNCTION get_hdr_tag

INTEGER FUNCTION get_hdr_rec_size( hdrbuf )
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: hdrbuf(*)
  get_hdr_rec_size = hdrbuf(1)
  RETURN
END FUNCTION get_hdr_rec_size

SUBROUTINE int_gen_write_field_header ( hdrbuf, hdrbufsize, itypesize, ftypesize, &
                                        DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm, &
                                        DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                        DomainStart , DomainEnd ,                                    &
                                        MemoryStart , MemoryEnd ,                                    &
                                        PatchStart , PatchEnd )
!<DESCRIPTION>
!<PRE>
! Items and their starting locations within a "write field" data header.  
! Assume that the data header is stored in integer vector "hdrbuf":  
!  hdrbuf(1) = hdrbufsize
!  hdrbuf(2) = headerTag
!  hdrbuf(3) = ftypesize
!  hdrbuf(4) = DataHandle
!  hdrbuf(5) = LEN(TRIM(DateStr))
!  hdrbuf(6:5+n1) = DateStr                                          ! n1 = LEN(TRIM(DateStr)) + 1
!  hdrbuf(6+n1) = LEN(TRIM(VarName))
!  hdrbuf(7+n1:6+n1+n2) = VarName                                    ! n2 = LEN(TRIM(VarName)) + 1
!  hdrbuf(7+n1+n2) = FieldType
!  hdrbuf(8+n1+n2) = LEN(TRIM(MemoryOrder))
!  hdrbuf(9+n1+n2:8+n1+n2+n3) = MemoryOrder                          ! n3 = LEN(TRIM(MemoryOrder)) + 1
!  hdrbuf(9+n1+n2+n3) = LEN(TRIM(Stagger))
!  hdrbuf(9+n1+n2+n3:8+n1+n2+n3+n4) = Stagger                        ! n4 = LEN(TRIM(Stagger)) + 1
!  hdrbuf(9+n1+n2+n3+n4) = LEN(TRIM(DimNames(1)))
!  hdrbuf(9+n1+n2+n3+n4:8+n1+n2+n3+n4+n5) = DimNames(1)              ! n5 = LEN(TRIM(DimNames(1))) + 1
!  hdrbuf(9+n1+n2+n3+n4+n5) = LEN(TRIM(DimNames(2)))
!  hdrbuf(9+n1+n2+n3+n4+n5:8+n1+n2+n3+n4+n5+n6) = DimNames(2)        ! n6 = LEN(TRIM(DimNames(2))) + 1
!  hdrbuf(9+n1+n2+n3+n4+n5+n6) = LEN(TRIM(DimNames(3)))
!  hdrbuf(9+n1+n2+n3+n4+n5+n6:8+n1+n2+n3+n4+n5+n6+n7) = DimNames(3)  ! n7 = LEN(TRIM(DimNames(3))) + 1
!  hdrbuf(9+n1+n2+n3+n4+n5+n6+n7) = DomainStart(1)
!  hdrbuf(10+n1+n2+n3+n4+n5+n6+n7) = DomainStart(2)
!  hdrbuf(11+n1+n2+n3+n4+n5+n6+n7) = DomainStart(3)
!  hdrbuf(12+n1+n2+n3+n4+n5+n6+n7) = DomainEnd(1)
!  hdrbuf(13+n1+n2+n3+n4+n5+n6+n7) = DomainEnd(2)
!  hdrbuf(14+n1+n2+n3+n4+n5+n6+n7) = DomainEnd(3)
!  hdrbuf(15+n1+n2+n3+n4+n5+n6+n7) = PatchStart(1)
!  hdrbuf(16+n1+n2+n3+n4+n5+n6+n7) = PatchStart(2)
!  hdrbuf(17+n1+n2+n3+n4+n5+n6+n7) = PatchStart(3)
!  hdrbuf(18+n1+n2+n3+n4+n5+n6+n7) = PatchEnd(1)
!  hdrbuf(19+n1+n2+n3+n4+n5+n6+n7) = PatchEnd(2)
!  hdrbuf(20+n1+n2+n3+n4+n5+n6+n7) = PatchEnd(3)
!  hdrbuf(21+n1+n2+n3+n4+n5+n6+n7) = DomainDesc
!
! Further details for some items:  
!  hdrbufsize:  Size of this data header in bytes.  
!  headerTag:   "Header tag" that tells the I/O quilt servers what kind of 
!               header this is.  For a "write field" header it must be set to 
!               int_field.  See file intio_tags.h for a complete list of 
!               these tags.  
!  ftypesize:   Size of field data type in bytes.  
!  DataHandle:  Descriptor for an open data set.  
!  DomainDesc:  Additional argument that may be used by some packages as a 
!               package-specific domain descriptor.  
!  Other items are described in detail in the "WRF I/O and Model Coupling API 
!  Specification".  
!
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER,       INTENT(INOUT)  ::  hdrbuf(*)
  INTEGER,       INTENT(INOUT)  ::  hdrbufsize
  INTEGER,       INTENT(INOUT)  ::  itypesize, ftypesize
  INTEGER ,      INTENT(IN)     :: DataHandle
  CHARACTER*(*), INTENT(IN)  :: DateStr
  CHARACTER*(*), INTENT(IN)  :: VarName
  REAL, DIMENSION(*)            :: Dummy
  INTEGER                       ,intent(in)    :: FieldType
  INTEGER                       ,intent(inout) :: Comm
  INTEGER                       ,intent(inout) :: IOComm
  INTEGER                       ,intent(in)    :: DomainDesc
  CHARACTER*(*)                 ,intent(in)    :: MemoryOrder
  CHARACTER*(*)                 ,intent(in)    :: Stagger
  CHARACTER*(*) , dimension (*) ,intent(in)    :: DimNames
  INTEGER ,dimension(*)         ,intent(in)    :: DomainStart, DomainEnd
  INTEGER ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd

  INTEGER i, n


  hdrbuf(1) = 0 ! deferred -- this will be length of header
  hdrbuf(2) = int_field
  hdrbuf(3) = ftypesize

  i = 4
  hdrbuf(i) = DataHandle      ; i = i+1
  call int_pack_string( DateStr, hdrbuf(i), n ) ; i = i + n
  call int_pack_string( VarName, hdrbuf(i), n ) ; i = i + n
  hdrbuf(i) = FieldType       ; i = i+1
  call int_pack_string( MemoryOrder, hdrbuf(i), n ) ; i = i + n
  call int_pack_string( Stagger,     hdrbuf(i), n ) ; i = i + n
  call int_pack_string( DimNames(1), hdrbuf(i), n ) ; i = i + n
  call int_pack_string( DimNames(2), hdrbuf(i), n ) ; i = i + n
  call int_pack_string( DimNames(3), hdrbuf(i), n ) ; i = i + n
  hdrbuf(i) = DomainStart(1)     ; i = i+1
  hdrbuf(i) = DomainStart(2)     ; i = i+1
  hdrbuf(i) = DomainStart(3)     ; i = i+1
  hdrbuf(i) = DomainEnd(1)       ; i = i+1
  hdrbuf(i) = DomainEnd(2)       ; i = i+1
  hdrbuf(i) = DomainEnd(3)       ; i = i+1
  hdrbuf(i) = PatchStart(1)     ; i = i+1
  hdrbuf(i) = PatchStart(2)     ; i = i+1
  hdrbuf(i) = PatchStart(3)     ; i = i+1
  hdrbuf(i) = PatchEnd(1)       ; i = i+1
  hdrbuf(i) = PatchEnd(2)       ; i = i+1
  hdrbuf(i) = PatchEnd(3)       ; i = i+1
  hdrbuf(i) = DomainDesc        ; i = i+1

  hdrbufsize = (i-1) * itypesize  ! return the number in bytes
  hdrbuf(1) = hdrbufsize

  RETURN
END SUBROUTINE int_gen_write_field_header


SUBROUTINE int_get_write_field_header ( hdrbuf, hdrbufsize, itypesize, ftypesize, &
                                        DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm,  &
                                        DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                        DomainStart , DomainEnd ,                                    &
                                        MemoryStart , MemoryEnd ,                                    &
                                        PatchStart , PatchEnd )
!<DESCRIPTION>
!<PRE>
! See documentation block in int_gen_write_field_header() for 
! a description of a "write field" header.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER,       INTENT(INOUT)  ::  hdrbuf(*)
  INTEGER,       INTENT(OUT)    ::  hdrbufsize
  INTEGER,       INTENT(INOUT)  ::  itypesize, ftypesize
  INTEGER ,      INTENT(OUT)    :: DataHandle
  CHARACTER*(*), INTENT(INOUT)  :: DateStr
  CHARACTER*(*), INTENT(INOUT)  :: VarName
  REAL, DIMENSION(*)            :: Dummy
  INTEGER                                       :: FieldType
  INTEGER                                       :: Comm
  INTEGER                                       :: IOComm
  INTEGER                                       :: DomainDesc
  CHARACTER*(*)                                 :: MemoryOrder
  CHARACTER*(*)                                 :: Stagger
  CHARACTER*(*) , dimension (*)                 :: DimNames
  INTEGER ,dimension(*)                         :: DomainStart, DomainEnd
  INTEGER ,dimension(*)                         :: MemoryStart, MemoryEnd
  INTEGER ,dimension(*)                         :: PatchStart,  PatchEnd
!Local
  CHARACTER*132 mess
  INTEGER i, n

  hdrbufsize = hdrbuf(1)
  IF ( hdrbuf(2) .NE. int_field ) THEN
    write(mess,*)'int_get_write_field_header: hdrbuf(2) ne int_field ',hdrbuf(2),int_field
    CALL wrf_error_fatal ( mess )
  ENDIF
  ftypesize = hdrbuf(3)

   i = 4
   DataHandle = hdrbuf(i)     ; i = i+1
  call int_unpack_string( DateStr, hdrbuf(i), n )     ; i = i+n
  call int_unpack_string( VarName, hdrbuf(i), n )     ; i = i+n
   FieldType = hdrbuf(i)      ; i = i+1
  call int_unpack_string( MemoryOrder, hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( Stagger, hdrbuf(i), n )     ; i = i+n
  call int_unpack_string( DimNames(1), hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( DimNames(2), hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( DimNames(3), hdrbuf(i), n ) ; i = i+n
   DomainStart(1) = hdrbuf(i)    ; i = i+1
   DomainStart(2) = hdrbuf(i)    ; i = i+1
   DomainStart(3) = hdrbuf(i)    ; i = i+1
   DomainEnd(1) = hdrbuf(i)       ; i = i+1
   DomainEnd(2) = hdrbuf(i)       ; i = i+1
   DomainEnd(3) = hdrbuf(i)       ; i = i+1
   PatchStart(1) = hdrbuf(i)     ; i = i+1
   PatchStart(2) = hdrbuf(i)     ; i = i+1
   PatchStart(3) = hdrbuf(i)     ; i = i+1
   PatchEnd(1) = hdrbuf(i)       ; i = i+1
   PatchEnd(2) = hdrbuf(i)       ; i = i+1
   PatchEnd(3) = hdrbuf(i)       ; i = i+1
   DomainDesc = hdrbuf(i)       ; i = i+1

  RETURN
END SUBROUTINE int_get_write_field_header

!!!!!!!!

!generate open for read header
SUBROUTINE int_gen_ofr_header( hdrbuf, hdrbufsize, itypesize, &
                                FileName, SysDepInfo, DataHandle )
!<DESCRIPTION>
!<PRE>
! Items and their starting locations within a "open for read" data header.  
! Assume that the data header is stored in integer vector "hdrbuf":  
!  hdrbuf(1) = hdrbufsize
!  hdrbuf(2) = headerTag
!  hdrbuf(3) = DataHandle
!  hdrbuf(4) = LEN(TRIM(FileName))
!  hdrbuf(5:4+n1) = FileName             ! n1 = LEN(TRIM(FileName)) + 1
!  hdrbuf(5+n1) = LEN(TRIM(SysDepInfo))
!  hdrbuf(6+n1:5+n1+n2) = SysDepInfo     ! n2 = LEN(TRIM(SysDepInfo)) + 1
!
! Further details for some items:  
!  hdrbufsize:  Size of this data header in bytes.  
!  headerTag:   "Header tag" that tells the I/O quilt servers what kind of 
!               header this is.  For an "open for read" header it must be set to 
!               int_open_for_read.  See file intio_tags.h for a complete list of 
!               these tags.  
!  DataHandle:  Descriptor for an open data set.  
!  FileName:    File name.  
!  SysDepInfo:  System dependent information used for optional additional 
!               I/O control information.  
!  Other items are described in detail in the "WRF I/O and Model Coupling API 
!  Specification".  
!
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER,       INTENT(INOUT) ::  hdrbuf(*)
  INTEGER,       INTENT(OUT)   ::  hdrbufsize
  INTEGER,       INTENT(INOUT) ::  itypesize
  INTEGER ,      INTENT(IN)    :: DataHandle
  CHARACTER*(*), INTENT(INOUT) :: FileName
  CHARACTER*(*), INTENT(INOUT) :: SysDepInfo
!Local
  INTEGER i, n, i1
!
  hdrbuf(1) = 0  !deferred
  hdrbuf(2) = int_open_for_read
  i = 3
  hdrbuf(i) = DataHandle     ; i = i+1

  call int_pack_string( TRIM(FileName), hdrbuf(i), n )   ; i = i + n
  call int_pack_string( TRIM(SysDepInfo), hdrbuf(i), n ) ; i = i + n
  hdrbufsize = (i-1) * itypesize  ! return the number in bytes
  hdrbuf(1) = hdrbufsize
  RETURN
END SUBROUTINE int_gen_ofr_header

!get open for read header
SUBROUTINE int_get_ofr_header( hdrbuf, hdrbufsize, itypesize, &
                                FileName, SysDepInfo, DataHandle )
!<DESCRIPTION>
!<PRE>
! See documentation block in int_gen_ofr_header() for 
! a description of a "open for read" header.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER,       INTENT(INOUT) ::  hdrbuf(*)
  INTEGER,       INTENT(OUT)   ::  hdrbufsize
  INTEGER,       INTENT(INOUT) ::  itypesize
  INTEGER ,      INTENT(OUT)   :: DataHandle
  CHARACTER*(*), INTENT(INOUT) :: FileName
  CHARACTER*(*), INTENT(INOUT) :: SysDepInfo
!Local
  INTEGER i, n
!
  hdrbufsize = hdrbuf(1)
!  IF ( hdrbuf(2) .NE. int_open_for_read ) THEN
!    CALL wrf_error_fatal ( "int_get_ofr_header: hdrbuf ne int_open_for_read")
!  ENDIF
  i = 3
  DataHandle = hdrbuf(i)    ; i = i+1
  call int_unpack_string( FileName, hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( SysDepInfo, hdrbuf(i), n ) ; i = i+n
  RETURN
END SUBROUTINE int_get_ofr_header

!!!!!!!!

!generate open for write begin header
SUBROUTINE int_gen_ofwb_header( hdrbuf, hdrbufsize, itypesize, &
                                FileName, SysDepInfo, io_form, DataHandle )
!<DESCRIPTION>
!<PRE>
! Items and their starting locations within a "open for write begin" data 
! header.  Assume that the data header is stored in integer vector "hdrbuf":  
!  hdrbuf(1) = hdrbufsize
!  hdrbuf(2) = headerTag
!  hdrbuf(3) = DataHandle
!  hdrbuf(4) = io_form
!  hdrbuf(5) = LEN(TRIM(FileName))
!  hdrbuf(6:5+n1) = FileName             ! n1 = LEN(TRIM(FileName)) + 1
!  hdrbuf(6+n1) = LEN(TRIM(SysDepInfo))
!  hdrbuf(7+n1:6+n1+n2) = SysDepInfo     ! n2 = LEN(TRIM(SysDepInfo)) + 1
!
! Further details for some items:  
!  hdrbufsize:  Size of this data header in bytes.  
!  headerTag:   "Header tag" that tells the I/O quilt servers what kind of 
!               header this is.  For an "open for write begin" header it must be set to 
!               int_open_for_write_begin.  See file intio_tags.h for a complete list of 
!               these tags.  
!  DataHandle:  Descriptor for an open data set.  
!  io_form:     I/O format for this file (netCDF, etc.).  
!  FileName:    File name.  
!  SysDepInfo:  System dependent information used for optional additional 
!               I/O control information.  
!  Other items are described in detail in the "WRF I/O and Model Coupling API 
!  Specification".  
!
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER,       INTENT(INOUT) :: hdrbuf(*)
  INTEGER,       INTENT(OUT)   :: hdrbufsize
  INTEGER,       INTENT(INOUT) :: itypesize
  INTEGER ,      INTENT(IN)    :: io_form
  INTEGER ,      INTENT(IN)    :: DataHandle
  CHARACTER*(*), INTENT(INOUT) :: FileName
  CHARACTER*(*), INTENT(INOUT) :: SysDepInfo
!Local
  INTEGER i, n, j
!
  hdrbuf(1) = 0  !deferred
  hdrbuf(2) = int_open_for_write_begin
  i = 3
  hdrbuf(i) = DataHandle     ; i = i+1
  hdrbuf(i) = io_form        ; i = i+1
!j = i
  call int_pack_string( FileName, hdrbuf(i), n )   ; i = i + n
!write(0,*)'int_gen_ofwb_header FileName ',TRIM(FileName),hdrbuf(j),n
!j = i
  call int_pack_string( SysDepInfo, hdrbuf(i), n ) ; i = i + n
!write(0,*)'int_gen_ofwb_header SysDepInfo ',TRIM(SysDepInfo),hdrbuf(j),n
  hdrbufsize = (i-1) * itypesize  ! return the number in bytes
  hdrbuf(1) = hdrbufsize
!write(0,*)'int_gen_ofwb_header hdrbuf(1) ',hdrbuf(1)
  RETURN
END SUBROUTINE int_gen_ofwb_header

!get open for write begin header
SUBROUTINE int_get_ofwb_header( hdrbuf, hdrbufsize, itypesize, &
                                FileName, SysDepInfo, io_form, DataHandle )
!<DESCRIPTION>
!<PRE>
! See documentation block in int_gen_ofwb_header() for 
! a description of a "open for write begin" header.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER,       INTENT(INOUT)  :: hdrbuf(*)
  INTEGER,       INTENT(OUT)    :: hdrbufsize
  INTEGER,       INTENT(INOUT)  :: itypesize
  INTEGER ,      INTENT(OUT)    :: DataHandle
  INTEGER ,      INTENT(OUT)    :: io_form
  CHARACTER*(*), INTENT (INOUT) :: FileName
  CHARACTER*(*), INTENT (INOUT) :: SysDepInfo
!Local
  INTEGER i, n, j
!
  hdrbufsize = hdrbuf(1)
!write(0,*)' int_get_ofwb_header next rec start ',hdrbuf(hdrbufsize+1)
!  IF ( hdrbuf(2) .NE. int_open_for_write_begin ) THEN
!    CALL wrf_error_fatal ( "int_get_ofwb_header: hdrbuf ne int_open_for_write_begin") 
!  ENDIF
  i = 3
  DataHandle = hdrbuf(i)    ; i = i+1
!write(0,*)' int_get_ofwb_header next rec',i, hdrbuf(hdrbufsize+1)
  io_form    = hdrbuf(i)    ; i = i+1
!write(0,*)' int_get_ofwb_header next rec',i, hdrbuf(hdrbufsize+1)

!j = i
  call int_unpack_string( FileName, hdrbuf(i), n ) ; i = i+n
!write(0,*)'int_get_ofwb_header FileName ',TRIM(FileName),hdrbuf(j),n
!write(0,*)' int_get_ofwb_header next rec',i, hdrbuf(hdrbufsize+1)
!j = i
  call int_unpack_string( SysDepInfo, hdrbuf(i), n ) ; i = i+n
!write(0,*)'int_get_ofwb_header SysDepInfo ',TRIM(SysDepInfo),hdrbuf(j),n
!write(0,*)' int_get_ofwb_header next rec',i, hdrbuf(hdrbufsize+1)
!write(0,*)'int_get_ofwb_header hdrbufsize ',hdrbufsize
  RETURN
END SUBROUTINE int_get_ofwb_header

!!!!!!!!!!

SUBROUTINE int_gen_handle_header( hdrbuf, hdrbufsize, itypesize, &
                                DataHandle , code )
!<DESCRIPTION>
!<PRE>
! Items and their starting locations within a "generic handle" data header.  
! Several types of data headers contain only a DataHandle and a header tag 
! (I/O command).  This routine is used for all of them.  Assume that 
! the data header is stored in integer vector "hdrbuf":  
!  hdrbuf(1) = hdrbufsize
!  hdrbuf(2) = headerTag
!  hdrbuf(3) = DataHandle
!
! Further details for some items:  
!  hdrbufsize:  Size of this data header in bytes.  
!  headerTag:   "Header tag" that tells the I/O quilt servers what kind of 
!               header this is.  For a "generic handle" header there are 
!               several possible values.  In this routine, dummy argument 
!               "code" is used as headerTag.  
!  DataHandle:  Descriptor for an open data set.  
!
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT) ::  hdrbuf(*)
  INTEGER, INTENT(OUT)   ::  hdrbufsize
  INTEGER, INTENT(INOUT) ::  itypesize
  INTEGER ,INTENT(IN)    :: DataHandle, code
!Local
  INTEGER i
!
  hdrbuf(1) = 0  !deferred
  hdrbuf(2) = code
  i = 3
  hdrbuf(i) = DataHandle     ; i = i+1
  hdrbufsize = (i-1) * itypesize  ! return the number in bytes
  hdrbuf(1) = hdrbufsize
  RETURN
END SUBROUTINE int_gen_handle_header

SUBROUTINE int_get_handle_header( hdrbuf, hdrbufsize, itypesize, &
                                DataHandle , code )
!<DESCRIPTION>
!<PRE>
! See documentation block in int_gen_handle_header() for 
! a description of a "generic handle" header.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT) ::  hdrbuf(*)
  INTEGER, INTENT(OUT)   ::  hdrbufsize
  INTEGER, INTENT(INOUT) ::  itypesize
  INTEGER ,INTENT(OUT)   :: DataHandle, code
!Local
  INTEGER i
!
  hdrbufsize = hdrbuf(1)
  code       = hdrbuf(2)
  i = 3
  DataHandle = hdrbuf(i)    ; i = i+1
  RETURN
END SUBROUTINE int_get_handle_header

!!!!!!!!!!!!

SUBROUTINE int_gen_ti_header_integer( hdrbuf, hdrbufsize, itypesize, typesize, &
                                      DataHandle, Element, Data, Count, code )
!<DESCRIPTION>
!<PRE>
! Items and their starting locations within a "time-independent integer" 
! data header.  Assume that the data header is stored in integer vector 
! "hdrbuf":  
!  hdrbuf(1) = hdrbufsize
!  hdrbuf(2) = headerTag
!  hdrbuf(3) = DataHandle
!  hdrbuf(4) = typesize
!  hdrbuf(5) = Count
!  hdrbuf(6:6+n1) = Data              ! n1 = (Count * typesize / itypesize) + 1
!  hdrbuf(7+n1) = LEN(TRIM(Element))
!  hdrbuf(8+n1:7+n1+n2) = Element     ! n2 = LEN(TRIM(Element)) + 1
!
! Further details for some items:  
!  hdrbufsize:  Size of this data header in bytes.  
!  headerTag:   "Header tag" that tells the I/O quilt servers what kind of 
!               header this is.  For an "time-independent integer" header it must be 
!               set to int_dom_ti_integer.  See file intio_tags.h for a complete 
!               list of these tags.  
!  DataHandle:  Descriptor for an open data set.  
!  typesize:    Size in bytes of each element of Data.  
!  Count:       Number of elements in Data.  
!  Data:        Data to write to file.  
!  Element:     Name of the data.  
!  Other items are described in detail in the "WRF I/O and Model Coupling API 
!  Specification".  
!
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(INOUT) ::  Element
  INTEGER, INTENT(IN)          ::  Data(*)
  INTEGER, INTENT(IN)          ::  DataHandle, Count, code
!Local
  INTEGER i, n
!
  CALL int_gen_ti_header_c ( hdrbuf, hdrbufsize, itypesize, typesize, &
                             DataHandle, Data, Count, code )
  i = hdrbufsize/itypesize + 1 ;
!write(0,*)'int_gen_ti_header_integer ',TRIM(Element)
  CALL int_pack_string ( Element, hdrbuf( i ), n ) ; i = i + n
  hdrbufsize = n * itypesize + hdrbufsize ! return the number in bytes
  hdrbuf(1) = hdrbufsize
  RETURN
END SUBROUTINE int_gen_ti_header_integer

SUBROUTINE int_gen_ti_header_integer_varna( hdrbuf, hdrbufsize, itypesize, typesize, &
                                      DataHandle, Element, VarName, Data, Count, code )
!<DESCRIPTION>
!<PRE>
! Items and their starting locations within a "time-independent integer"
! data header.  Assume that the data header is stored in integer vector
! "hdrbuf":
!  hdrbuf(1) = hdrbufsize
!  hdrbuf(2) = headerTag
!  hdrbuf(3) = DataHandle
!  hdrbuf(4) = typesize
!  hdrbuf(5) = Count
!  hdrbuf(6:6+n1) = Data              ! n1 = (Count * typesize / itypesize) + 1
!  hdrbuf(7+n1) = LEN(TRIM(Element))
!  hdrbuf(8+n1:7+n1+n2) = Element     ! n2 = LEN(TRIM(Element)) + 1
!  hdrbuf(8+n1+n2) = LEN(TRIM(VarName)) = n3
!  hderbuf(9+n1+n2:8+n1+n2+n3) = TRIM(VarName)
!
! Further details for some items:
!  hdrbufsize:  Size of this data header in bytes.
!  headerTag:   "Header tag" that tells the I/O quilt servers what kind of
!               header this is.  For an "time-independent integer" header it must be
!               set to int_dom_ti_integer.  See file intio_tags.h for a complete
!               list of these tags.
!  DataHandle:  Descriptor for an open data set.
!  typesize:    Size in bytes of each element of Data.
!  Count:       Number of elements in Data.
!  Data:        Data to write to file.
!  Element:     Name of the data.
!  VarName:     Variable name.  Used for *_<get|put>_var_ti_char but not for
!               *_<get|put>_dom_ti_char.
!  Other items are described in detail in the "WRF I/O and Model Coupling API
!  Specification".
!
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(IN)    ::  Element, VarName
  INTEGER, INTENT(IN)          ::  Data(*)
  INTEGER, INTENT(IN)          ::  DataHandle, Count, code
!Local
  INTEGER i, n
!
  CALL int_gen_ti_header_c ( hdrbuf, hdrbufsize, itypesize, typesize, &
                             DataHandle, Data, Count, code )
  i = hdrbufsize/itypesize + 1 ;
!write(0,*)'int_gen_ti_header_integer ',TRIM(Element)
  CALL int_pack_string ( Element, hdrbuf( i ), n ) ; i = i + n
  CALL int_pack_string ( VarName, hdrbuf( i ), n ) ; i = i + n
  hdrbufsize = i * itypesize + hdrbufsize ! return the number in bytes
  hdrbuf(1) = hdrbufsize
  RETURN
END SUBROUTINE int_gen_ti_header_integer_varna

SUBROUTINE int_gen_ti_header_real( hdrbuf, hdrbufsize, itypesize, typesize, &
                                   DataHandle, Element, Data, Count, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_ti_header_integer except that Data has type REAL.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(INOUT) ::  Element
  REAL, INTENT(IN)             ::  Data(*)
  INTEGER, INTENT(IN)          ::  DataHandle, Count, code
!Local
  INTEGER i, n
!
  CALL int_gen_ti_header_c ( hdrbuf, hdrbufsize, itypesize, typesize, &
                             DataHandle, Data, Count, code )
  i = hdrbufsize/itypesize + 1 ;
!write(0,*)'int_gen_ti_header_real ',TRIM(Element)
  CALL int_pack_string ( Element, hdrbuf( i ), n ) ; i = i + n
  hdrbufsize = n * itypesize + hdrbufsize ! return the number in bytes
  hdrbuf(1) = hdrbufsize
  RETURN
END SUBROUTINE int_gen_ti_header_real

SUBROUTINE int_get_ti_header_integer_varna( hdrbuf, hdrbufsize, itypesize, typesize, &
                              DataHandle, Element, VarName, Data, Count, code)
!<DESCRIPTION>
!<PRE>
! Same as int_gen_ti_header_integer except that Data is read from
! the file.
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(INOUT) ::  Element, VarName
  INTEGER, INTENT(OUT)         ::  Data(*)
  INTEGER, INTENT(OUT)         ::  DataHandle, Count, code
!Local
  INTEGER i, n
!

  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, Data, Count, code )
  i = n/itypesize + 1
  CALL int_unpack_string ( Element, hdrbuf( i ), n ) ; i=i+n;
  CALL int_unpack_string ( VarName, hdrbuf( i ), n ) ; i = i + n
! write(0,*)'int_get_ti_header_integer_varna "', &
!      TRIM(Element),'" "', TRIM(VarName),'" data(1)=',Data(1)
  hdrbufsize = hdrbuf(1)
  RETURN
END SUBROUTINE int_get_ti_header_integer_varna

SUBROUTINE int_get_ti_header_integer( hdrbuf, hdrbufsize, itypesize, typesize, &
                              DataHandle, Element, Data, Count, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_ti_header_integer except that Data is read from 
! the file.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(INOUT) ::  Element
  INTEGER, INTENT(OUT)         ::  Data(*)
  INTEGER, INTENT(OUT)         ::  DataHandle, Count, code
!Local
  INTEGER i, n
!

  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, Data, Count, code )
  i = 1 
  CALL int_unpack_string ( Element, hdrbuf( n/itypesize + 1 ), n ) ;
!write(0,*)'int_get_ti_header_integer ',TRIM(Element), Data(1)
  hdrbufsize = hdrbuf(1)
  RETURN
END SUBROUTINE int_get_ti_header_integer

SUBROUTINE int_get_ti_header_real( hdrbuf, hdrbufsize, itypesize, typesize, &
                              DataHandle, Element, Data, Count, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_ti_header_real except that Data is read from 
! the file.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(INOUT) ::  Element
  REAL, INTENT(OUT)            ::  Data(*)
  INTEGER, INTENT(OUT)         ::  DataHandle, Count, code
!Local
  INTEGER i, n
!

  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, Data, Count, code )
  i = 1
  CALL int_unpack_string ( Element, hdrbuf( n/itypesize + 1 ), n ) ;
!write(0,*)'int_get_ti_header_real ',TRIM(Element), Data(1)
  hdrbufsize = hdrbuf(1)
  RETURN
END SUBROUTINE int_get_ti_header_real

!!!!!!!!!!!!

SUBROUTINE int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, Element, VarName, Data, code )
!<DESCRIPTION>
!<PRE>
! Items and their starting locations within a "time-independent string" 
! data header.  Assume that the data header is stored in integer vector 
! "hdrbuf":  
!  hdrbuf(1) = hdrbufsize
!  hdrbuf(2) = headerTag
!  hdrbuf(3) = DataHandle
!  hdrbuf(4) = typesize
!  hdrbuf(5) = LEN(TRIM(Element))
!  hdrbuf(6:5+n1) = Element                ! n1 = LEN(TRIM(Element)) + 1
!  hdrbuf(6+n1) = LEN(TRIM(Data))
!  hdrbuf(7+n1:6+n1+n2) = Data             ! n2 = LEN(TRIM(Data)) + 1
!  hdrbuf(7+n1+n2) = LEN(TRIM(VarName))
!  hdrbuf(8+n1+n2:7+n1+n2+n3) = VarName    ! n3 = LEN(TRIM(VarName)) + 1
!
! Further details for some items:  
!  hdrbufsize:  Size of this data header in bytes.  
!  headerTag:   "Header tag" that tells the I/O quilt servers what kind of 
!               header this is.  For an "time-independent string" header it must be 
!               set to int_dom_ti_char.  See file intio_tags.h for a complete 
!               list of these tags.  
!  DataHandle:  Descriptor for an open data set.  
!  typesize:    1 (size in bytes of a single CHARACTER).  
!  Element:     Name of the data.  
!  Data:        Data to write to file.  
!  VarName:     Variable name.  Used for *_<get|put>_var_ti_char but not for 
!               *_<get|put>_dom_ti_char.  
!  Other items are described in detail in the "WRF I/O and Model Coupling API 
!  Specification".  
!
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize
  CHARACTER*(*), INTENT(IN)    :: Element, Data, VarName
  INTEGER, INTENT(IN)          ::  DataHandle, code
!Local
  INTEGER                      ::  DummyData
  INTEGER i, n, Count, DummyCount
!
  DummyCount = 0
  CALL int_gen_ti_header_c ( hdrbuf, hdrbufsize, itypesize, 1, &
                             DataHandle, DummyData, DummyCount, code )
  i = hdrbufsize/itypesize+1 ;
  CALL int_pack_string ( Element, hdrbuf( i ), n ) ; i = i + n
  CALL int_pack_string ( Data   , hdrbuf( i ), n ) ; i = i + n
  CALL int_pack_string ( VarName   , hdrbuf( i ), n ) ; i = i + n
  hdrbufsize = (i-1) * itypesize + hdrbufsize ! return the number in bytes
  hdrbuf(1) = hdrbufsize
  RETURN
END SUBROUTINE int_gen_ti_header_char

SUBROUTINE int_get_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, Element, VarName, Data, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_ti_header_char except that Data is read from 
! the file.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize
  CHARACTER*(*), INTENT(INOUT) ::  Element, Data, VarName
  INTEGER, INTENT(OUT)         ::  DataHandle, code
!Local
  INTEGER i, n, DummyCount, typesize
  CHARACTER * 132  dummyData
!
  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, dummyData, DummyCount, code )
  i = n/itypesize+1 ;
  CALL int_unpack_string ( Element, hdrbuf( i ), n ) ; i = i + n
  CALL int_unpack_string ( Data   , hdrbuf( i ), n ) ; i = i + n
  CALL int_unpack_string ( VarName  , hdrbuf( i ), n ) ; i = i + n
  hdrbufsize = hdrbuf(1)

  RETURN
END SUBROUTINE int_get_ti_header_char


!!!!!!!!!!!!

SUBROUTINE int_gen_td_header_char( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, DateStr, Element, Data, code )
!<DESCRIPTION>
!<PRE>
! Items and their starting locations within a "time-dependent string" 
! data header.  Assume that the data header is stored in integer vector 
! "hdrbuf":  
!  hdrbuf(1) = hdrbufsize
!  hdrbuf(2) = headerTag
!  hdrbuf(3) = DataHandle
!  hdrbuf(4) = typesize
!  hdrbuf(5) = LEN(TRIM(Element))
!  hdrbuf(6:5+n1) = Element            ! n1 = LEN(TRIM(Element)) + 1
!  hdrbuf(6+n1) = LEN(TRIM(DateStr))
!  hdrbuf(7+n1:6+n1+n2) = DateStr      ! n2 = LEN(TRIM(DateStr)) + 1
!  hdrbuf(7+n1+n2) = LEN(TRIM(Data))
!  hdrbuf(8+n1+n2:7+n1+n2+n3) = Data   ! n3 = LEN(TRIM(Data)) + 1
!
! Further details for some items:  
!  hdrbufsize:  Size of this data header in bytes.  
!  headerTag:   "Header tag" that tells the I/O quilt servers what kind of 
!               header this is.  For an "time-dependent string" header it must be 
!               set to int_dom_td_char.  See file intio_tags.h for a complete 
!               list of these tags.  
!  DataHandle:  Descriptor for an open data set.  
!  typesize:    1 (size in bytes of a single CHARACTER).  
!  Element:     Name of the data.  
!  Data:        Data to write to file.  
!  Other items are described in detail in the "WRF I/O and Model Coupling API 
!  Specification".  
!
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize
  CHARACTER*(*), INTENT(INOUT) ::  DateStr, Element, Data
  INTEGER, INTENT(IN)          ::  DataHandle, code
!Local
  INTEGER i, n, DummyCount, DummyData
!
  DummyCount = 0

  CALL int_gen_ti_header_c ( hdrbuf, hdrbufsize, itypesize, 1, &
                           DataHandle, DummyData, DummyCount, code )
  i = hdrbufsize/itypesize + 1 ;
  CALL int_pack_string ( Element, hdrbuf( i ), n ) ; i = i + n
  CALL int_pack_string ( DateStr, hdrbuf( i ), n ) ; i = i + n
  CALL int_pack_string ( Data   , hdrbuf( i ), n ) ; i = i + n
  hdrbufsize = (i-1) * itypesize + hdrbufsize ! return the number in bytes
  hdrbuf(1) = hdrbufsize
  RETURN
END SUBROUTINE int_gen_td_header_char

SUBROUTINE int_get_td_header_char( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, DateStr, Element, Data, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_td_header_char except that Data is read from 
! the file.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize
  CHARACTER*(*), INTENT(INOUT) ::  DateStr, Element, Data
  INTEGER, INTENT(OUT)         ::  DataHandle, code
!Local
  INTEGER i, n, Count, typesize
!

  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, Data, Count, code )
  i = n/itypesize + 1 ;
  CALL int_unpack_string ( Element, hdrbuf( i ), n ) ; i = i + n ;
  CALL int_unpack_string ( DateStr, hdrbuf( i ), n ) ; i = i + n ;
  CALL int_unpack_string ( Data   , hdrbuf( i ), n ) ; i = i + n ;
  hdrbufsize = hdrbuf(1)
  RETURN
END SUBROUTINE int_get_td_header_char

SUBROUTINE int_gen_td_header_integer( hdrbuf, hdrbufsize, itypesize, typesize, &
                                      DataHandle, DateStr, Element, Data, Count, code )
!<DESCRIPTION>
!<PRE>
! Items and their starting locations within a "time-dependent integer" 
! data header.  Assume that the data header is stored in integer vector 
! "hdrbuf":  
!  hdrbuf(1) = hdrbufsize
!  hdrbuf(2) = headerTag
!  hdrbuf(3) = DataHandle
!  hdrbuf(4) = typesize
!  hdrbuf(5) = Count
!  hdrbuf(6:6+n1) = Data              ! n1 = (Count * typesize / itypesize) + 1
!  hdrbuf(7+n1) = LEN(TRIM(DateStr))
!  hdrbuf(8+n1:7+n1+n2) = DateStr      ! n2 = LEN(TRIM(DateStr)) + 1
!  hdrbuf(8+n1+n2) = LEN(TRIM(Element))
!  hdrbuf(9+n1+n2:8+n1+n2+n3) = Element   ! n3 = LEN(TRIM(Element)) + 1
!
! Further details for some items:  
!  hdrbufsize:  Size of this data header in bytes.  
!  headerTag:   "Header tag" that tells the I/O quilt servers what kind of 
!               header this is.  For an "time-dependent integer" header it must be 
!               set to int_dom_td_integer.  See file intio_tags.h for a complete 
!               list of these tags.  
!  DataHandle:  Descriptor for an open data set.  
!  typesize:    1 (size in bytes of a single CHARACTER).  
!  Element:     Name of the data.  
!  Count:       Number of elements in Data.  
!  Data:        Data to write to file.  
!  Other items are described in detail in the "WRF I/O and Model Coupling API 
!  Specification".  
!
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(INOUT) ::  DateStr, Element
  INTEGER, INTENT(IN)          ::  Data(*)
  INTEGER, INTENT(IN)          ::  DataHandle, Count, code
!Local
  INTEGER i, n
!

  CALL int_gen_ti_header_c ( hdrbuf, hdrbufsize, itypesize, typesize, &
                           DataHandle, Data, Count, code )
  i = hdrbufsize/itypesize + 1 ;
  CALL int_pack_string ( DateStr, hdrbuf( i ), n ) ; i = i + n
  CALL int_pack_string ( Element, hdrbuf( i ), n ) ; i = i + n
  hdrbufsize = (i-1) * itypesize + hdrbufsize ! return the number in bytes
  hdrbuf(1) = hdrbufsize
  RETURN
END SUBROUTINE int_gen_td_header_integer

SUBROUTINE int_gen_td_header_real( hdrbuf, hdrbufsize, itypesize, typesize, &
                                   DataHandle, DateStr, Element, Data, Count, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_td_header_integer except that Data has type REAL.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(INOUT) ::  DateStr, Element
  REAL, INTENT(IN)             ::  Data(*)
  INTEGER, INTENT(IN)          ::  DataHandle, Count, code
!Local
  INTEGER i, n
!

  CALL int_gen_ti_header_c ( hdrbuf, hdrbufsize, itypesize, typesize, &
                           DataHandle, Data, Count, code )
  i = hdrbufsize/itypesize + 1 ;
  CALL int_pack_string ( DateStr, hdrbuf( i ), n ) ; i = i + n
  CALL int_pack_string ( Element, hdrbuf( i ), n ) ; i = i + n
  hdrbufsize = (i-1) * itypesize + hdrbufsize ! return the number in bytes
  hdrbuf(1) = hdrbufsize
  RETURN
END SUBROUTINE int_gen_td_header_real

SUBROUTINE int_get_td_header_integer( hdrbuf, hdrbufsize, itypesize, typesize, &
                              DataHandle, DateStr, Element, Data, Count, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_td_header_integer except that Data is read from 
! the file.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(INOUT) ::  DateStr, Element
  INTEGER, INTENT(OUT)         ::  Data(*)
  INTEGER, INTENT(OUT)         ::  DataHandle, Count, code
!Local
  INTEGER i, n
!

  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, Data, Count, code )
  i = n/itypesize + 1 ;
  CALL int_unpack_string ( DateStr, hdrbuf( i ), n ) ; i = i + n ;
  CALL int_unpack_string ( Element, hdrbuf( i ), n ) ; i = i + n ;
  hdrbufsize = hdrbuf(1)
  RETURN
END SUBROUTINE int_get_td_header_integer

SUBROUTINE int_get_td_header_real( hdrbuf, hdrbufsize, itypesize, typesize, &
                              DataHandle, DateStr, Element, Data, Count, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_td_header_real except that Data is read from 
! the file.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(INOUT) ::  DateStr, Element
  REAL , INTENT(OUT)           ::  Data(*)
  INTEGER, INTENT(OUT)         ::  DataHandle, Count, code
!Local
  INTEGER i, n
!

  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, Data, Count, code )
  i = n/itypesize + 1 ;
  CALL int_unpack_string ( DateStr, hdrbuf( i ), n ) ; i = i + n ;
  CALL int_unpack_string ( Element, hdrbuf( i ), n ) ; i = i + n ;
  hdrbufsize = hdrbuf(1)
  RETURN
END SUBROUTINE int_get_td_header_real

!!!!!!!!!!!!!!

SUBROUTINE int_gen_noop_header ( hdrbuf, hdrbufsize, itypesize )
  IMPLICIT NONE
!<DESCRIPTION>
!<PRE>
! Items and their starting locations within a "no-operation" 
! data header.  Assume that the data header is stored in integer vector 
! "hdrbuf":  
!  hdrbuf(1) = hdrbufsize
!  hdrbuf(2) = headerTag
!
! Further details for some items:  
!  hdrbufsize:  Size of this data header in bytes.  
!  headerTag:   "Header tag" that tells the I/O quilt servers what kind of 
!               header this is.  For an "no-operation" header it must be 
!               set to int_noop.  See file intio_tags.h for a complete 
!               list of these tags.  
!
!</PRE>
!</DESCRIPTION>
#include "intio_tags.h"
  INTEGER, INTENT(INOUT) ::  hdrbuf(*)
  INTEGER, INTENT(OUT)   ::  hdrbufsize
  INTEGER, INTENT(INOUT) ::  itypesize
!Local
  INTEGER i
!
  hdrbuf(1) = 0  !deferred
  hdrbuf(2) = int_noop
  i = 3
  hdrbufsize = (i-1) * itypesize  ! return the number in bytes
  hdrbuf(1) = hdrbufsize
  RETURN
END SUBROUTINE int_gen_noop_header

SUBROUTINE int_get_noop_header( hdrbuf, hdrbufsize, itypesize )
!<DESCRIPTION>
!<PRE>
! See documentation block in int_gen_noop_header() for 
! a description of a "no-operation" header.  
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
#include "intio_tags.h"
  INTEGER, INTENT(INOUT) ::  hdrbuf(*)
  INTEGER, INTENT(OUT)   ::  hdrbufsize
  INTEGER, INTENT(INOUT) ::  itypesize
!Local
  INTEGER i
!
  hdrbufsize = hdrbuf(1)
  IF ( hdrbuf(2) .NE. int_noop ) THEN
    CALL wrf_error_fatal ( "int_get_noop_header: hdrbuf ne int_noop")
  ENDIF
  i = 3
  RETURN
END SUBROUTINE int_get_noop_header


! first int is length of string to follow then string encodes as ints
SUBROUTINE int_pack_string ( str, buf, n )
  IMPLICIT NONE
!<DESCRIPTION>
!<PRE>
! This routine is used to store a string as a sequence of integers.  
! The first integer is the string length.  
!</PRE>
!</DESCRIPTION>
  CHARACTER*(*), INTENT(IN)          :: str
  INTEGER, INTENT(OUT)               :: n    ! on return, N is the number of ints stored in buf
  INTEGER, INTENT(OUT), DIMENSION(*) :: buf
!Local
  INTEGER i
!
  n = 1
  buf(n) = LEN(TRIM(str))
  n = n+1
  DO i = 1, LEN(TRIM(str))
    buf(n) = ichar(str(i:i))
    n = n+1
  ENDDO
  n = n - 1
END SUBROUTINE int_pack_string

SUBROUTINE int_unpack_string ( str, buf, n )
  IMPLICIT NONE
!<DESCRIPTION>
!<PRE>
! This routine is used to extract a string from a sequence of integers.  
! The first integer is the string length.  
!</PRE>
!</DESCRIPTION>
  CHARACTER*(*), INTENT(OUT)        :: str
  INTEGER, INTENT(OUT)              :: n       ! on return, N is the number of ints copied from buf
  INTEGER, INTENT(IN), DIMENSION(*) :: buf
!Local
  INTEGER i
  INTEGER strlen

  strlen = buf(1)
  str = ""
  DO i = 1, strlen
    str(i:i) = char(buf(i+1))
  ENDDO
  n = strlen + 1
END SUBROUTINE int_unpack_string

END MODULE module_internal_header_util

