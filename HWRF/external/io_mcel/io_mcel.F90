MODULE module_ext_mcel

  INTEGER, PARAMETER :: int_num_handles = 99
  LOGICAL, DIMENSION(int_num_handles) :: okay_to_write, okay_to_read,                     &
                                         opened_for_write, opened_for_update,             &
                                         opened_for_read,                                 &
                                         int_handle_in_use, okay_to_commit
  LOGICAL, DIMENSION(int_num_handles) :: mcel_grid_defined, mcel_finalized
  INTEGER, DIMENSION(int_num_handles) :: int_num_bytes_to_write
  INTEGER, DIMENSION(int_num_handles) :: usemask
  CHARACTER*256, DIMENSION(int_num_handles) :: CurrentDateInFile
  CHARACTER*8092, DIMENSION(int_num_handles) :: ListOfFields
  REAL, POINTER    :: int_local_output_buffer(:)
  INTEGER          :: int_local_output_cursor
  INTEGER          :: mcel_npglobal, mcel_mystart, mcel_mnproc, mcel_myproc

  INTEGER, PARAMETER           :: onebyte = 1
  INTEGER comm_io_servers, iserver, hdrbufsize, obufsize
  INTEGER itypesize, rtypesize, typesize
  INTEGER, DIMENSION(512)     :: hdrbuf
  INTEGER, DIMENSION(int_num_handles)       :: handle
  INTEGER, DIMENSION(512, int_num_handles)  :: open_file_descriptors
  INCLUDE "MCEL.inc"
#include "intio_tags.h"
#include "wrf_io_flags.h"
#include "wrf_status_codes.h"
  CHARACTER*80  LAT_R(int_num_handles), LON_R(int_num_handles), LANDMASK_I(int_num_handles)

  REAL*8, ALLOCATABLE :: xlat(:,:), xlong(:,:)
  REAL*8              :: deltax, deltay, dxm(2)
  REAL*8              :: originx, originy, origin(2)
  INTEGER, ALLOCATABLE :: mask(:,:)
  REAL, ALLOCATABLE :: rmask(:,:)
  DOUBLEPRECISION, ALLOCATABLE :: dmask(:,:)

  CHARACTER*132 last_next_var 

  CONTAINS

    LOGICAL FUNCTION int_valid_handle( handle )
      IMPLICIT NONE
      INTEGER, INTENT(IN) ::  handle
      int_valid_handle = ( handle .ge. 8 .and. handle .le. int_num_handles ) 
    END FUNCTION int_valid_handle

    SUBROUTINE int_get_fresh_handle( retval )
!      USE wrf_data, ONLY : wrf_data_handle
!      USE ext_ncd_support_routines, ONLY : allocHandle
!      type(wrf_data_handle),pointer     :: DH
!      INTEGER i, retval, comm, Status
      INTEGER i, retval

#if 0
      CALL allocHandle(retval,DH,Comm,Status)
#endif

      retval = -1
! dont use first 8 handles
      DO i = 8, int_num_handles
        IF ( .NOT. int_handle_in_use(i) )  THEN
          retval = i
          GOTO 33
        ENDIF
      ENDDO
33    CONTINUE
      IF ( retval < 0 )  THEN
        CALL wrf_error_fatal("external/io_quilt/io_int.F90: int_get_fresh_handle() can not")
      ENDIF
      int_handle_in_use(retval) = .TRUE.
      NULLIFY ( int_local_output_buffer )
    END SUBROUTINE int_get_fresh_handle

! parse comma separated list of VARIABLE=VALUE strings and return the
! value for the matching variable if such exists, otherwise return
! the empty string
SUBROUTINE get_value ( varname , str , retval )
  IMPLICIT NONE
  CHARACTER*(*) ::    varname
  CHARACTER*(*) ::    str
  CHARACTER*(*) ::    retval

  CHARACTER (128) varstr, tstr
  INTEGER i,j,n,varstrn
  LOGICAL nobreak, nobreakouter

  varstr = TRIM(varname)//"="
  varstrn = len(TRIM(varstr))
  n = len(TRIM(str))
  retval = ""
  i = 1
  nobreakouter = .TRUE.
  DO WHILE ( nobreakouter )
    j = 1
    nobreak = .TRUE.
    tstr = ""
    DO WHILE ( nobreak )
      nobreak = .FALSE.
      IF ( i .LE. n ) THEN
        IF (str(i:i) .NE. ',' ) THEN
           tstr(j:j) = str(i:i)
           nobreak = .TRUE.
        ENDIF
      ENDIF
      j = j + 1
      i = i + 1
    ENDDO
    IF ( i .GT. n ) nobreakouter = .FALSE.
    IF ( varstr(1:varstrn) .EQ. tstr(1:varstrn) ) THEN
      retval(1:) = TRIM(tstr(varstrn+1:))
      nobreakouter = .FALSE.
    ENDIF
  ENDDO
  RETURN
END SUBROUTINE get_value


    !--- ioinit
    SUBROUTINE init_module_ext_mcel
      IMPLICIT NONE
      CALL wrf_sizeof_integer( itypesize )
      CALL wrf_sizeof_real   ( rtypesize )
    END SUBROUTINE init_module_ext_mcel

END MODULE module_ext_mcel

 SUBROUTINE copy_field_to_cache_r2r ( Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   REAL             Field(*)
   REAL             cache(ips:ipe,jps:jpe)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        cache(i,j) = Field( idex )
     ENDDO
   ENDDO
 END SUBROUTINE copy_field_to_cache_r2r

 SUBROUTINE copy_field_to_cache_r2d ( Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   REAL             Field(*)
   DOUBLE PRECISION cache(ips:ipe,jps:jpe)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        cache(i,j) = Field( idex )
     ENDDO
   ENDDO
 END SUBROUTINE copy_field_to_cache_r2d

 SUBROUTINE copy_field_to_cache_d2r ( Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   DOUBLE PRECISION Field(*) 
   REAL             cache(ips:ipe,jps:jpe)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        cache(i,j) = Field( idex )
     ENDDO
   ENDDO
 END SUBROUTINE copy_field_to_cache_d2r

 SUBROUTINE copy_field_to_cache_d2d ( Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   DOUBLE PRECISION Field(*) 
   DOUBLE PRECISION cache(ips:ipe,jps:jpe)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        cache(i,j) = Field( idex )
     ENDDO
   ENDDO
 END SUBROUTINE copy_field_to_cache_d2d

 SUBROUTINE copy_field_to_cache_int ( Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   INTEGER Field(*)
   INTEGER cache(ips:ipe,jps:jpe)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        cache(i,j) = Field( idex )
     ENDDO
   ENDDO
 END SUBROUTINE copy_field_to_cache_int

 SUBROUTINE copy_cache_to_field_r2r ( cache, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   REAL            cache(ips:ipe,jps:jpe)
   REAL            Field(*)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        Field( idex ) = cache(i,j)
     ENDDO
   ENDDO
 END SUBROUTINE copy_cache_to_field_r2r

 SUBROUTINE copy_cache_to_field_r2d ( cache, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   REAL             cache(ips:ipe,jps:jpe)
   DOUBLEPRECISION  Field(*)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        Field( idex ) = cache(i,j)
     ENDDO
   ENDDO
 END SUBROUTINE copy_cache_to_field_r2d

 SUBROUTINE copy_cache_to_field_d2r ( cache, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   DOUBLEPRECISION  cache(ips:ipe,jps:jpe)
   REAL             Field(*)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        Field( idex ) = cache(i,j)
     ENDDO
   ENDDO
 END SUBROUTINE copy_cache_to_field_d2r

 SUBROUTINE copy_cache_to_field_d2d ( cache, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   DOUBLEPRECISION  cache(ips:ipe,jps:jpe)
   DOUBLEPRECISION  Field(*)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        Field( idex ) = cache(i,j)
     ENDDO
   ENDDO
 END SUBROUTINE copy_cache_to_field_d2d

!--------------

SUBROUTINE ext_mcel_ioinit( SysDepInfo, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  CHARACTER*(*), INTENT(IN) :: SysDepInfo
  INTEGER Status
  CALL init_module_ext_mcel
  Status = 0 
END SUBROUTINE ext_mcel_ioinit

!--- open_for_read 
SUBROUTINE ext_mcel_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )
  USE module_ext_mcel
  IMPLICIT NONE
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i

  CALL int_get_fresh_handle(i)
  okay_to_write(i) = .false.
  DataHandle = i
  CurrentDateInFile(i) = ""
  Status = WRF_WARN_NOTSUPPORTED

  RETURN  
END SUBROUTINE ext_mcel_open_for_read


!--- inquire_opened
SUBROUTINE ext_mcel_inquire_opened ( DataHandle, FileName , FileStatus, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status

  Status = 0

  FileStatus = WRF_FILE_NOT_OPENED
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF      ( int_handle_in_use( DataHandle ) .AND. opened_for_read ( DataHandle ) ) THEN
      IF ( okay_to_read ( DataHandle ) ) THEN
        FileStatus = WRF_FILE_OPENED_FOR_READ
      ELSE
        FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
      ENDIF
    ELSE IF ( int_handle_in_use( DataHandle ) .AND. opened_for_write ( DataHandle ) ) THEN
      IF ( okay_to_write ( DataHandle ) ) THEN
        FileStatus = WRF_FILE_OPENED_FOR_WRITE
      ELSE
        FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
      ENDIF
    ENDIF
  ENDIF
  Status = 0
  
  RETURN
END SUBROUTINE ext_mcel_inquire_opened

!--- inquire_filename
SUBROUTINE ext_mcel_inquire_filename ( DataHandle, FileName , FileStatus, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER *80   SysDepInfo
  Status = 0
  FileStatus = WRF_FILE_NOT_OPENED
  IF ( int_valid_handle( DataHandle ) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      IF ( opened_for_read ( DataHandle ) ) THEN
        IF ( okay_to_read( DataHandle ) ) THEN
           FileStatus = WRF_FILE_OPENED_FOR_READ
        ELSE
           FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
        ENDIF
      ELSE IF ( opened_for_write( DataHandle ) ) THEN
        IF ( okay_to_write( DataHandle ) ) THEN
           FileStatus = WRF_FILE_OPENED_FOR_WRITE
        ELSE
           FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
        ENDIF
      ELSE
        FileStatus = WRF_FILE_NOT_OPENED
      ENDIF
    ENDIF
  ENDIF
  Status = 0
END SUBROUTINE ext_mcel_inquire_filename

!--- sync
SUBROUTINE ext_mcel_iosync ( DataHandle, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  Status = 0
  RETURN
END SUBROUTINE ext_mcel_iosync

!--- close
SUBROUTINE ext_mcel_ioclose ( DataHandle, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER DataHandle, Status

  IF ( int_valid_handle (DataHandle) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CLOSE ( DataHandle ) 
    ENDIF
  ENDIF

  Status = 0

  RETURN
END SUBROUTINE ext_mcel_ioclose

!--- ioexit
SUBROUTINE ext_mcel_ioexit( Status )

  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER                     :: DataHandle
  INTEGER i,ierr
  REAL dummy

  RETURN  
END SUBROUTINE ext_mcel_ioexit

!--- get_next_time
SUBROUTINE ext_mcel_get_next_time ( DataHandle, DateStr, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER         code
  CHARACTER*132   locElement, dummyvar
  INTEGER istat

!local
  INTEGER                        :: locDataHandle
  CHARACTER*132                  :: locDateStr
  CHARACTER*132                  :: locVarName
  integer                        :: locFieldType
  integer                        :: locComm
  integer                        :: locIOComm
  integer                        :: locDomainDesc
  character*132                  :: locMemoryOrder
  character*132                  :: locStagger
  character*132 , dimension (3)  :: locDimNames
  integer ,dimension(3)          :: locDomainStart, locDomainEnd
  integer ,dimension(3)          :: locMemoryStart, locMemoryEnd
  integer ,dimension(3)          :: locPatchStart,  locPatchEnd

  character*132 mess
  integer ii,jj,kk,myrank
  INTEGER inttypesize, realtypesize
  REAL, DIMENSION( 1 ) :: Field

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_quilt/io_int.F90: ext_mcel_get_next_time: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_quilt/io_int.F90: ext_mcel_get_next_time: DataHandle not opened" )
  ENDIF
  inttypesize = itypesize
  realtypesize = rtypesize

  Status = WRF_WARN_NOTSUPPORTED

  RETURN
END SUBROUTINE ext_mcel_get_next_time

!--- set_time
SUBROUTINE ext_mcel_set_time ( DataHandle, DateStr, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status

  Status = WRF_WARN_NOTSUPPORTED
  RETURN
END SUBROUTINE ext_mcel_set_time

!--- get_var_info
SUBROUTINE ext_mcel_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , WrfType, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: VarName
  integer               ,intent(out)    :: NDim
  character*(*)         ,intent(out)    :: MemoryOrder
  character*(*)         ,intent(out)    :: Stagger
  integer ,dimension(*) ,intent(out)    :: DomainStart, DomainEnd
  integer               ,intent(out)    :: WrfType
  integer               ,intent(out)    :: Status

!local
  INTEGER                        :: locDataHandle
  CHARACTER*132                  :: locDateStr
  CHARACTER*132                  :: locVarName
  integer                        :: locFieldType
  integer                        :: locComm
  integer                        :: locIOComm
  integer                        :: locDomainDesc
  character*132                  :: locMemoryOrder
  character*132                  :: locStagger
  character*132 , dimension (3)  :: locDimNames
  integer ,dimension(3)          :: locDomainStart, locDomainEnd
  integer ,dimension(3)          :: locMemoryStart, locMemoryEnd
  integer ,dimension(3)          :: locPatchStart,  locPatchEnd

  character*132 mess
  integer ii,jj,kk,myrank
  INTEGER inttypesize, realtypesize, istat, code
  REAL, DIMENSION( 1 ) :: Field

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_quilt/io_int.F90: ext_mcel_get_var_info: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_quilt/io_int.F90: ext_mcel_get_var_info: DataHandle not opened" )
  ENDIF
  inttypesize = itypesize
  realtypesize = rtypesize
  Status = 0

RETURN
END SUBROUTINE ext_mcel_get_var_info

!--- get_next_var  (not defined for IntIO)
SUBROUTINE ext_mcel_get_next_var ( DataHandle, VarName, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: VarName
  INTEGER ,       INTENT(OUT) :: Status

!local
  INTEGER                        :: locDataHandle
  CHARACTER*132                  :: locDateStr
  CHARACTER*132                  :: locVarName
  integer                        :: locFieldType
  integer                        :: locComm
  integer                        :: locIOComm
  integer                        :: locDomainDesc
  character*132                  :: locMemoryOrder
  character*132                  :: locStagger
  character*132 , dimension (3)  :: locDimNames
  integer ,dimension(3)          :: locDomainStart, locDomainEnd
  integer ,dimension(3)          :: locMemoryStart, locMemoryEnd
  integer ,dimension(3)          :: locPatchStart,  locPatchEnd

character*128 locElement, strData, dumstr
integer loccode, loccount
integer idata(128)
real    rdata(128)

  character*132 mess
  integer ii,jj,kk,myrank
  INTEGER inttypesize, realtypesize, istat, code
  REAL, DIMENSION( 1 ) :: Field

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_quilt/io_int.F90: ext_mcel_get_next_var: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_quilt/io_int.F90: ext_mcel_get_next_var: DataHandle not opened" )
  ENDIF
  inttypesize = itypesize
  realtypesize = rtypesize

  Status = 0

  RETURN
END SUBROUTINE ext_mcel_get_next_var

!--- get_dom_ti_real
SUBROUTINE ext_mcel_get_dom_ti_real ( DataHandle,Element,   Data, Count, Outcount, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Outcount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER loccount, code, istat, locDataHandle
  CHARACTER*132                :: locElement, mess
  LOGICAL keepgoing

  Status = 0

RETURN
END SUBROUTINE ext_mcel_get_dom_ti_real 

!--- put_dom_ti_real
SUBROUTINE ext_mcel_put_dom_ti_real ( DataHandle,Element,   Data, Count,  Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy
!

  Status = 0
RETURN
END SUBROUTINE ext_mcel_put_dom_ti_real 

!--- get_dom_ti_double
SUBROUTINE ext_mcel_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_mcel_get_dom_ti_double not supported yet')
RETURN
END SUBROUTINE ext_mcel_get_dom_ti_double 

!--- put_dom_ti_double
SUBROUTINE ext_mcel_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_mcel_put_dom_ti_double not supported yet')
RETURN
END SUBROUTINE ext_mcel_put_dom_ti_double 

!--- get_dom_ti_integer
SUBROUTINE ext_mcel_get_dom_ti_integer ( DataHandle,Element,   Data, Count, Outcount, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER loccount, code, istat, locDataHandle
  CHARACTER*132   locElement, mess
  LOGICAL keepgoing

  Status = 0
RETURN
END SUBROUTINE ext_mcel_get_dom_ti_integer 

!--- put_dom_ti_integer
SUBROUTINE ext_mcel_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  INTEGER ,       INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy
!
  Status = 0
RETURN
END SUBROUTINE ext_mcel_put_dom_ti_integer 

!--- get_dom_ti_logical
SUBROUTINE ext_mcel_get_dom_ti_logical ( DataHandle,Element,   Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_mcel_get_dom_ti_logical not supported yet')
RETURN
END SUBROUTINE ext_mcel_get_dom_ti_logical 

!--- put_dom_ti_logical
SUBROUTINE ext_mcel_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_message('ext_mcel_put_dom_ti_logical not supported yet')
RETURN
END SUBROUTINE ext_mcel_put_dom_ti_logical 

!--- get_dom_ti_char
SUBROUTINE ext_mcel_get_dom_ti_char ( DataHandle,Element,   Data,  Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER istat, code, i
  CHARACTER*79 dumstr, locElement
  INTEGER locDataHandle
  LOGICAL keepgoing

  Status = 0
RETURN
END SUBROUTINE ext_mcel_get_dom_ti_char 

!--- put_dom_ti_char
SUBROUTINE ext_mcel_put_dom_ti_char ( DataHandle, Element,  Data,  Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i
  REAL dummy
  INTEGER                 :: Count

! TBH:  Not sure what this is doing here.  2004_11_15
! JGM:  You are right. It does not belong here.  2006_09_28
!  IF ( int_valid_handle ( Datahandle ) ) THEN
!    IF ( int_handle_in_use( DataHandle ) ) THEN
!      CALL int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize,  &
!                                   DataHandle, Element, "", Data, int_dom_ti_char )
!      WRITE( unit=DataHandle ) hdrbuf 
!    ENDIF
!  ENDIF
  Status = 0
RETURN
END SUBROUTINE ext_mcel_put_dom_ti_char 

!--- get_dom_td_real
SUBROUTINE ext_mcel_get_dom_td_real ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_dom_td_real 

!--- put_dom_td_real
SUBROUTINE ext_mcel_put_dom_td_real ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_dom_td_real 

!--- get_dom_td_double
SUBROUTINE ext_mcel_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_dom_td_double 

!--- put_dom_td_double
SUBROUTINE ext_mcel_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_dom_td_double 

!--- get_dom_td_integer
SUBROUTINE ext_mcel_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_dom_td_integer 

!--- put_dom_td_integer
SUBROUTINE ext_mcel_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_dom_td_integer 

!--- get_dom_td_logical
SUBROUTINE ext_mcel_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_dom_td_logical 

!--- put_dom_td_logical
SUBROUTINE ext_mcel_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_dom_td_logical 

!--- get_dom_td_char
SUBROUTINE ext_mcel_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_dom_td_char 

!--- put_dom_td_char
SUBROUTINE ext_mcel_put_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_dom_td_char 

!--- get_var_ti_real
SUBROUTINE ext_mcel_get_var_ti_real ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_var_ti_real 

!--- put_var_ti_real
SUBROUTINE ext_mcel_put_var_ti_real ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_var_ti_real 

!--- get_var_ti_double
SUBROUTINE ext_mcel_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_var_ti_double 

!--- put_var_ti_double
SUBROUTINE ext_mcel_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_var_ti_double 

!--- get_var_ti_integer
SUBROUTINE ext_mcel_get_var_ti_integer ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_var_ti_integer 

!--- put_var_ti_integer
SUBROUTINE ext_mcel_put_var_ti_integer ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_var_ti_integer 

!--- get_var_ti_logical
SUBROUTINE ext_mcel_get_var_ti_logical ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_var_ti_logical 

!--- put_var_ti_logical
SUBROUTINE ext_mcel_put_var_ti_logical ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_var_ti_logical 

!--- get_var_ti_char
SUBROUTINE ext_mcel_get_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER locDataHandle, code
  CHARACTER*132 locElement, locVarName
  Status = 0
RETURN
END SUBROUTINE ext_mcel_get_var_ti_char 

!--- put_var_ti_char
SUBROUTINE ext_mcel_put_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
  REAL dummy
  INTEGER                 :: Count
  Status = 0
RETURN
END SUBROUTINE ext_mcel_put_var_ti_char 

!--- get_var_td_real
SUBROUTINE ext_mcel_get_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_var_td_real 

!--- put_var_td_real
SUBROUTINE ext_mcel_put_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_var_td_real 

!--- get_var_td_double
SUBROUTINE ext_mcel_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_var_td_double 

!--- put_var_td_double
SUBROUTINE ext_mcel_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_var_td_double 

!--- get_var_td_integer
SUBROUTINE ext_mcel_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_var_td_integer 

!--- put_var_td_integer
SUBROUTINE ext_mcel_put_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_var_td_integer 

!--- get_var_td_logical
SUBROUTINE ext_mcel_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_var_td_logical 

!--- put_var_td_logical
SUBROUTINE ext_mcel_put_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_var_td_logical 

!--- get_var_td_char
SUBROUTINE ext_mcel_get_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_get_var_td_char 

!--- put_var_td_char
SUBROUTINE ext_mcel_put_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName 
  CHARACTER*(*) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_mcel_put_var_td_char 

SUBROUTINE ext_mcel_georegister( DataHandle, inlon, inlat,                                    &
                                 MemoryStart , MemoryEnd ,                                    &
                                 PatchStart , PatchEnd ,                                      &
                                 Status )
  USE module_ext_mcel
  IMPLICIT NONE
  integer                       ,intent(in)    :: DataHandle
  integer                       ,intent(inout) :: Status
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  REAL , DIMENSION(MemoryStart(1):MemoryEnd(1),MemoryStart(2):MemoryEnd(2)), INTENT(IN) :: inlon, inlat
  integer ips,ipe,jps,jpe
  integer ims,ime,jms,jme
  integer idex,ierr,i,j

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_mcel_georegister: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_mcel_georegister: DataHandle not opened" )
  ENDIF
  IF ( mcel_finalized( DataHandle ) ) THEN
    CALL wrf_error_fatal( "ext_mcel_georegister: called after first read/write operation" ) ;
  ENDIF

  ips = PatchStart(1) ; ipe = PatchEnd(1)
  jps = PatchStart(2) ; jpe = PatchEnd(2)
  ims = MemoryStart(1) ; ime = MemoryEnd(1)
  jms = MemoryStart(2) ; jme = MemoryEnd(2)

  IF ( ALLOCATED(xlat) ) THEN
    DEALLOCATE(xlat)
  ENDIF
  IF ( ALLOCATED(xlong) ) THEN
    DEALLOCATE(xlong)
  ENDIF
  ALLOCATE(xlat(ips:ipe,jps:jpe))
  DO j = jps, jpe
    DO i = ips, ipe
      idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
      xlat(i,j) = inlat( i,j)  ! idex )
    ENDDO
  ENDDO
  ALLOCATE(xlong(ips:ipe,jps:jpe))
  DO j = jps, jpe
    DO i = ips, ipe
      idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
      xlong(i,j) = inlon( i,j ) ! idex )
    ENDDO
  ENDDO
  RETURN
END SUBROUTINE ext_mcel_georegister

SUBROUTINE ext_mcel_mask ( DataHandle, inmask,                                          &
                           MemoryStart , MemoryEnd ,                                    &
                           PatchStart , PatchEnd ,                                      &
                           Status )
  USE module_ext_mcel
  IMPLICIT NONE
  integer                       ,intent(in)    :: DataHandle
  integer                       ,intent(inout) :: Status
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  INTEGER , DIMENSION(MemoryStart(1):MemoryEnd(1),MemoryStart(2):MemoryEnd(2)), INTENT(IN) :: inmask
  integer ips,ipe,jps,jpe
  integer ims,ime,jms,jme
  integer idex,ierr,i,j

  ips = PatchStart(1) ; ipe = PatchEnd(1)
  jps = PatchStart(2) ; jpe = PatchEnd(2)
  ims = MemoryStart(1) ; ime = MemoryEnd(1)
  jms = MemoryStart(2) ; jme = MemoryEnd(2)

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_mcel_mask: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_mcel_mask: DataHandle not opened" )
  ENDIF
  IF ( mcel_finalized( DataHandle ) ) THEN
    CALL wrf_error_fatal( "ext_mcel_mask: called after first read/write operation" ) ;
  ENDIF

  IF ( ALLOCATED(mask) ) THEN
    DEALLOCATE(mask)
  ENDIF
  ALLOCATE(mask(ips:ipe,jps:jpe))
  DO j = jps, jpe
    DO i = ips, ipe
      idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
      mask(i,j) = inmask( i,j ) ! idex )
    ENDDO
  ENDDO
  RETURN
END SUBROUTINE ext_mcel_mask

INTEGER FUNCTION cast_to_int( a )
  INTEGER a
  cast_to_int = a
  RETURN
END FUNCTION cast_to_int

