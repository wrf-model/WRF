! (old comment from when this file was a template)
! This is a template for adding a package-dependent implemetnation of
! the I/O API.  You can use the name xxx since that is already set up
! as a placeholder in module_io.F, md_calls.m4, and the Registry, or
! you can change the name here and in those other places.  For additional
! information on adding a package to WRF, see the latest version of the
! WRF Design and Implementation Document 1.1 (Draft). June 21, 2001
!
! Uses header manipulation routines in module_io_quilt.F
!

MODULE module_ext_mcel

  INTEGER, PARAMETER :: int_num_handles = 99
  LOGICAL, DIMENSION(int_num_handles) :: okay_to_write, okay_to_read, opened_for_update , &
                                         opened_for_read ,                                &
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
  INCLUDE 'intio_tags.h'
  INCLUDE 'wrf_io_flags.h'
  INCLUDE 'wrf_status_codes.h'
  CHARACTER*80  LAT_R, LON_R, LANDMASK_I

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

SUBROUTINE ext_mcel_ioinit( Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER Status
  CALL init_module_ext_mcel
  Status = 0 
END SUBROUTINE ext_mcel_ioinit


!--- open_for_write_begin
SUBROUTINE ext_mcel_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                         DataHandle , Status )
  USE module_ext_mcel
  IMPLICIT NONE
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i, tasks_in_group, ierr, comm_io_group
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  REAL dummy
  INTEGER io_form
  CHARACTER*256   fname
  CHARACTER*256   grid_type
  CHARACTER*256   tstr

  fname = FileName

  CALL int_get_fresh_handle(i)
  okay_to_write(i) = .false.
  okay_to_read(i) = .false.
  opened_for_update(i) = .false.
  opened_for_read(i) = .false.
  mcel_grid_defined(i) = .false.
  mcel_finalized(i) = .false.
  DataHandle = i

! right now only 2d -- extend later to 3d, 1d, 0d

  CALL get_value ( 'MCEL_GRIDTYPE', SysDepInfo, grid_type )

  LAT_R = ""
  LON_R = ""
  LANDMASK_I = ""
  CALL get_value( "LAT_R", SysDepInfo, LAT_R )
  CALL get_value( "LON_R", SysDepInfo, LON_R )
  CALL get_value( "LANDMASK_I", SysDepInfo, LANDMASK_I )

  mcel_npglobal=-1 ; mcel_mystart=-1 ; mcel_mnproc=-1 ; mcel_myproc=-1  

  CALL get_value( "MCEL_NPGLOBAL", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_npglobal
  CALL get_value( "MCEL_MYSTART", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_mystart
  CALL get_value( "MCEL_MNPROC", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_mnproc
  CALL get_value( "MCEL_MYPROC", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_myproc

  deltax = -1
  CALL get_value( "MCEL_DELTA_X", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) deltax
  deltay = -1
  CALL get_value( "MCEL_DELTA_Y", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) deltay
  originx = -1
  CALL get_value( "MCEL_ORIGIN_X", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) originx
  originy = -1
  CALL get_value( "MCEL_ORIGIN_Y", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) originy


  IF ( TRIM(grid_type) .EQ. 'UNSTRUCTURED' ) THEN
     CALL newGrid( open_file_descriptors(2,i), 2, MCEL_GRIDTYPE_UNSTRUCTURED, &
                   MCEL_GRIDCENT_NODAL, MCEL_GRIDCOORD_LATLONG, &
                   ierr )
  ELSE

     CALL newGrid( open_file_descriptors(2,i), 2, MCEL_GRIDTYPE_STRUCTURED, &
                   MCEL_GRIDCENT_NODAL, MCEL_GRIDCOORD_LATLONG, &
                   ierr )
  ENDIF

  IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_for_write_begin: newGrid" )

  CALL newProgram ( open_file_descriptors(1,i), TRIM(fname), ierr )

  IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_for_write_begin: newProgram" )

  Status = 0
  RETURN  
END SUBROUTINE ext_mcel_open_for_write_begin

!--- open_for_write_commit
SUBROUTINE ext_mcel_open_for_write_commit( DataHandle , Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER*80   FileName,SysDepInfo, mess
  REAL dummy
  INTEGER ierr

  IF ( int_valid_handle ( DataHandle ) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      okay_to_write( DataHandle ) = .true.
      okay_to_read( DataHandle ) = .true.
      opened_for_update( DataHandle ) = .false.
    ENDIF
  ENDIF

! rest of this is handled lazy on first write

  Status = 0

  RETURN  
END SUBROUTINE ext_mcel_open_for_write_commit

!--- open_for_read_begin
SUBROUTINE ext_mcel_open_for_read_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                         DataHandle , Status )
  USE module_ext_mcel
  IMPLICIT NONE
  CHARACTER*(*) :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER*(*) :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i, tasks_in_group, ierr, comm_io_group
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  REAL dummy
  INTEGER io_form
  CHARACTER*80 read_mode, grid_type, filter_handle, use_mask
  CHARACTER*256   tstr

  CALL int_get_fresh_handle(i)
  okay_to_write(i) = .false.
  okay_to_read(i) = .false.
  opened_for_read(i) = .true.
  mcel_grid_defined(i) = .false.
  DataHandle = i
  ListOfFields(i) = " "

! recover the names of the strings that contain georeference and mask data, if avail
  lat_r = ""
  lon_r = ""
  landmask_i = ""
  read_mode = ""
  grid_type = "STRUCTURED"
  filter_handle = "Filter"
  use_mask = ""
  CALL get_value( "LAT_R", SysDepInfo, lat_r )
  CALL get_value( "LON_R", SysDepInfo, lon_r )
  CALL get_value( "LANDMASK_I", SysDepInfo, landmask_i )
  CALL get_value( "READ_MODE", SysDepInfo, read_mode )
  CALL get_value( "MCEL_GRIDTYPE", SysDepInfo, grid_type )
  CALL get_value( "FILTER_HANDLE", SysDepInfo, filter_handle )
  CALL get_value( "USE_MASK", SysDepInfo, use_mask )
  IF ( TRIM(read_mode) .EQ. 'UPDATE' ) THEN
    opened_for_update( i ) = .true.
  ELSE
    opened_for_update( i ) = .false.
  ENDIF

  usemask(i) = MCEL_MASK_ALLVALID
  IF ( trim(use_mask) .NE. "" ) read( use_mask , '(I)' ) usemask(i)
write(0,*)'ofrb use_mask ',trim(use_mask)
write(0,*)'ofrb filter_handle ',trim(filter_handle)
write(0,*)'ofrb usemask(',i,') = ',usemask(i)

  mcel_npglobal=-1 ; mcel_mystart=-1 ; mcel_mnproc=-1 ; mcel_myproc=-1

  CALL get_value( "MCEL_NPGLOBAL", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_npglobal
  CALL get_value( "MCEL_MYSTART", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_mystart
  CALL get_value( "MCEL_MNPROC", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_mnproc
  CALL get_value( "MCEL_MYPROC", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(I)' ) mcel_myproc
  deltax = -1
  CALL get_value( "MCEL_DELTA_X", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) deltax
  deltay = -1
  CALL get_value( "MCEL_DELTA_Y", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) deltay
  originx = -1
  CALL get_value( "MCEL_ORIGIN_X", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) originx
  originy = -1
  CALL get_value( "MCEL_ORIGIN_Y", SysDepInfo, tstr )
  IF ( tstr .NE. "" ) READ( tstr, '(f15.8)' ) originy

  IF ( trim(grid_type) .EQ. 'UNSTRUCTURED' ) THEN
    CALL newGrid( open_file_descriptors(2,i), 2, MCEL_GRIDTYPE_UNSTRUCTURED, &
                  MCEL_GRIDCENT_NODAL, MCEL_GRIDCOORD_LATLONG, &
                  ierr )
  ELSE
    CALL newGrid( open_file_descriptors(2,i), 2, MCEL_GRIDTYPE_STRUCTURED, &
                  MCEL_GRIDCENT_NODAL, MCEL_GRIDCOORD_LATLONG, &
                  ierr )
  ENDIF
  IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_for_read_begin: newGrid" )

!  IF ( opened_for_update( i ) ) THEN
    ! right now the name of the ior file is coming in as the file name; whereis for writing the
    ! file name is the program name.  Needs to be resolved for this call to newfilter. 
    ! Make the FileName the program name and find a different way to bring in the ior file; sysdepinfo? environ?
    CALL newfilter ( open_file_descriptors(1,i), 'relfile:/' // TRIM(FileName), filter_handle, ierr )
    IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_for_read_begin: newfilter" )
!  ELSE
!    CALL newProgram ( open_file_descriptors(1,i), TRIM(FileName), ierr )
!    IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_for_read_begin: newProgram" )
!  ENDIF

  Status = 0
  RETURN  
END SUBROUTINE ext_mcel_open_for_read_begin

!--- open_for_read_commit
SUBROUTINE ext_mcel_open_for_read_commit( DataHandle , Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  CHARACTER*80   FileName,SysDepInfo, mess
  REAL dummy
  INTEGER ierr

  IF ( int_valid_handle ( DataHandle ) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      if ( opened_for_update( DataHandle ) ) okay_to_write( DataHandle ) = .true.
      okay_to_read( DataHandle ) = .true.
    ENDIF
  ENDIF

! everything else will be handled in lazy fashion by first operation

  Status = 0

  RETURN  
END SUBROUTINE ext_mcel_open_for_read_commit

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
  Status = 0 

  RETURN  
END SUBROUTINE ext_mcel_open_for_read


!--- intio_nextrec  (INT_IO only)
SUBROUTINE ext_mcel_intio_nextrec ( DataHandle , NextRec , Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER , INTENT(IN)  :: DataHandle
  INTEGER               :: NextRec
  INTEGER               :: Status

  Status = 0

  RETURN  
END SUBROUTINE ext_mcel_intio_nextrec

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
    IF      ( int_handle_in_use( DataHandle ) .AND. okay_to_write( DataHandle ) ) THEN
      FileStatus = WRF_FILE_OPENED_AND_COMMITTED
    ELSE IF ( int_handle_in_use( DataHandle ) .AND. .NOT. okay_to_write( DataHandle ) ) THEN
      FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
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
      IF ( okay_to_write( DataHandle ) ) THEN
        FileStatus = WRF_FILE_OPENED_AND_COMMITTED
      ELSE
        FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
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

  Status = 0

  RETURN
END SUBROUTINE ext_mcel_get_next_time

!--- set_time
SUBROUTINE ext_mcel_set_time ( DataHandle, DateStr, Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status

  Status = 0
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

  IF ( int_valid_handle ( Datahandle ) ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize,  &
                                   DataHandle, Element, "", Data, int_dom_ti_char )
      WRITE( unit=DataHandle ) hdrbuf 
    ENDIF
  ENDIF
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

!--- read_field
SUBROUTINE ext_mcel_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
  USE module_ext_mcel
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
  integer                       ,intent(inout)    :: FieldType
  integer                       ,intent(inout) :: Comm
  integer                       ,intent(inout) :: IOComm
  integer                       ,intent(inout)    :: DomainDesc
  character*(*)                 ,intent(inout)    :: MemoryOrder
  character*(*)                 ,intent(inout)    :: Stagger
  character*(*) , dimension (*) ,intent(inout)    :: DimNames
  integer ,dimension(*)         ,intent(inout)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(inout)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(inout)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status

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
  real, allocatable, dimension(:,:) :: temp
  doubleprecision, allocatable, dimension(:,:) :: dtemp
  integer gSize(2)
  INTEGER, EXTERNAL :: cast_to_int

  character*132 mess
  integer ips,ipe,jps,jpe
  integer ims,ime,jms,jme
  integer idex,ierr,i,j

  integer ii,jj,kk,myrank,ierr, mcel_type
  real*8 data_time
  character*14 timestr
  character*80 mess
  


!  REAL, DIMENSION( MemoryStart(1):MemoryEnd(1), &
!                   MemoryStart(2):MemoryEnd(2), &
!                   MemoryStart(3):MemoryEnd(3) ) :: Field
  REAL, DIMENSION(*)    :: Field

  INTEGER inttypesize, realtypesize, istat, code

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_quilt/io_int.F90: ext_mcel_read_field: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("external/io_quilt/io_int.F90: ext_mcel_read_field: DataHandle not opened" )
  ENDIF


  ips = PatchStart(1) ; ipe = PatchEnd(1) 
  jps = PatchStart(2) ; jpe = PatchEnd(2) 
  ims = MemoryStart(1) ; ime = MemoryEnd(1)
  jms = MemoryStart(2) ; jme = MemoryEnd(2)

write(0,*)'ext_mcel_read_field ',DataHandle, TRIM(DateStr), TRIM(VarName)

  inttypesize = itypesize
  realtypesize = rtypesize
  IF      ( FieldType .EQ. WRF_REAL ) THEN
    typesize = rtypesize
    mcel_type = MCEL_DATATYPE_REAL
  ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
    mcel_type = MCEL_DATATYPE_DOUBLE
  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
    typesize = itypesize
    mcel_type = MCEL_DATATYPE_INT32
  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    CALL wrf_error_fatal( 'io_int.F90: ext_mcel_write_field, WRF_LOGICAL not yet supported')
  ENDIF

  ! case 1: the file is opened but not commited for update 
!  if ( .not. okay_to_write( DataHandle ) .and. opened_for_update( DataHandle) )  then
  if ( .not. okay_to_read( DataHandle ) )  then
    IF ( opened_for_update( DataHandle) ) THEN
write(0,*)'ext_mcel_read_field tr calling ext_mcel_write_field ', TRIM(DateStr), TRIM(VarName)
      CALL ext_mcel_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                                  DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                  DomainStart , DomainEnd ,                                    &
                                  MemoryStart , MemoryEnd ,                                    &
                                  PatchStart , PatchEnd ,                                      &
                                  ierr )
write(0,*)'ext_mcel_read_field tr back from ext_mcel_write_field ', TRIM(DateStr), TRIM(VarName), ierr
    ELSE

! these will have been set in the call to open_for_read_begin from sysdepinfo
      IF ( mcel_npglobal .NE. -1 .AND. mcel_mystart .NE. -1 .AND.  &
           mcel_mnproc   .NE. -1 .AND. mcel_myproc  .NE. -1     ) THEN
write(0,*)'ext_mcel_read_field tr setglobalsize ', TRIM(VarName), mcel_npglobal
        call setglobalsize(open_file_descriptors(2,DataHandle),mcel_npglobal,ierr)
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_read_field: setglobalsize")
write(0,*)'ext_mcel_read_field tr setglobalstart ', TRIM(VarName), mcel_mystart
        call setglobalstart(open_file_descriptors(2,DataHandle),mcel_mystart,ierr)
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_read_field: setglobalstart")
#if 0
        call setprocinfo(open_file_descriptors(1,DataHandle),mcel_mnproc,mcel_myproc,ierr)
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_read_field: setprocinfo")
#endif
      ENDIF
      mcel_npglobal=-1 ; mcel_mystart=-1 ; mcel_mnproc=-1 ; mcel_myproc=-1

      ! sieve the fields coming in and grab the ones we need for geo registration
      IF      ( TRIM(VarName) .EQ. TRIM(LAT_R) ) THEN
        IF ( ALLOCATED(xlat) ) THEN
          DEALLOCATE(xlat)
        ENDIF
        ALLOCATE(xlat(ips:ipe,jps:jpe))
        CALL copy_field_to_cache ( FieldType , Field, xlat, ips, ipe, jps, jpe, ims, ime, jms, jme )
      ELSE IF ( TRIM(VarName) .EQ. TRIM(LON_R) ) THEN
        IF ( ALLOCATED(xlong) ) THEN
          DEALLOCATE(xlong)
        ENDIF
        ALLOCATE(xlong(ips:ipe,jps:jpe))
        CALL copy_field_to_cache ( FieldType , Field, xlong, ips, ipe, jps, jpe, ims, ime, jms, jme )
      ELSE IF ( TRIM(VarName) .EQ. TRIM(LANDMASK_I) ) THEN
        IF ( ALLOCATED(mask) ) THEN
          DEALLOCATE(mask)
        ENDIF
        ALLOCATE(mask(ips:ipe,jps:jpe))
        IF ( FieldType .EQ. WRF_INTEGER ) THEN
          CALL copy_field_to_cache ( FieldType , Field, mask, ips, ipe, jps, jpe, ims, ime, jms, jme )
        ELSE IF ( FieldType .EQ. WRF_REAL ) THEN
          ALLOCATE(rmask(ips:ipe,jps:jpe))
          CALL copy_field_to_cache ( FieldType , Field, rmask, ips, ipe, jps, jpe, ims, ime, jms, jme )
          mask = NINT( rmask )
          DEALLOCATE(rmask)
        ELSE IF (FieldType .EQ. WRF_DOUBLE ) THEN
          ALLOCATE(dmask(ips:ipe,jps:jpe))
          CALL copy_field_to_cache ( FieldType , Field, dmask, ips, ipe, jps, jpe, ims, ime, jms, jme )
          mask = NINT( dmask )
          DEALLOCATE(dmask)
        ENDIF
      ELSE
        IF ( .NOT. mcel_grid_defined( DataHandle ) ) THEN
          mcel_grid_defined( DataHandle ) = .true.
          gSize(1) = ipe-ips+1
          gSize(2) = jpe-jps+1
write(0,*)'ext_mcel_read_field tr setSize ', TRIM(VarName), gSize
          CALL setSize ( open_file_descriptors(2,DataHandle), gSize, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: setSize")
        ENDIF
write(0,*)'ext_mcel_read_field tr addSources ', TRIM(VarName)
        CALL addSources ( open_file_descriptors(1,DataHandle), MCEL_SERVER,  &
  &       TRIM(VarName),1, mcel_type, ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: addSources")
write(0,*)'ext_mcel_read_field tr addOutputs ', TRIM(VarName)
        CALL addOutputs ( open_file_descriptors(1,DataHandle),   &
  &       TRIM(VarName),1, mcel_type, ierr )
! add this filed to the list that we know something about
        ListOfFields(DataHandle) = TRIM(ListOfFields(DataHandle)) // ',' // TRIM(VarName)
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: addOutputs")
      ENDIF
    ENDIF

  ! case 2: opened for update and committed
!  else if ( okay_to_write( DataHandle ) .and. opened_for_update( DataHandle) )  then
  else if ( okay_to_read( DataHandle ) )  then

write(0,*)'ext_mcel_read_field ok ', Trim(VarName)
    IF ( TRIM(VarName) .NE. TRIM(LAT_R) .AND. TRIM(VarName) .NE. TRIM(LON_R) .AND. &
         TRIM(VarName) .NE. TRIM(LANDMASK_I) ) THEN
      IF ( .NOT. mcel_finalized( DataHandle ) ) THEN
        IF ( ALLOCATED( xlat ) .AND. ALLOCATED( xlong ) ) THEN
write(0,*)'ext_mcel_read_field ok setlocationsXY ', Trim(VarName)
          CALL setLocationsXY( open_file_descriptors(2,DataHandle), xlong, xlat, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_open_read_field: setLocationsXY" )
        ELSE IF ( deltax .gt. 0. .and. deltay .gt. 0. .and. originx .gt. 0. .and. originy .gt. 0. ) THEN
          dxm(1) = deltax
          dxm(2) = deltay
          call SetDX ( open_file_descriptors(2,DataHandle), dxm, ierr)
          origin(1) = originx
          origin(2) = originy
          call SetOrigin ( open_file_descriptors(2,DataHandle), origin, ierr)
        ELSE
          CALL wrf_error_fatal( "ext_mcel_open_for_write_commit:noLocationsXY or dx/dy")
        ENDIF
        IF ( ALLOCATED(mask) ) THEN
write(0,*)'ext_mcel_read_field ok setMask ', Trim(VarName)
          CALL setMask ( open_file_descriptors(2,DataHandle) , mask, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_read_field: setMask")
        ENDIF
write(0,*)'ext_mcel_read_field ok setoutputgrid ', Trim(VarName)
        CALL setoutputgrid ( open_file_descriptors(1,DataHandle), open_file_descriptors(2,DataHandle), ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_read_field: setoutputgrid")
write(0,*)'ext_mcel_read_field ok finalizefilters ', Trim(VarName)
        CALL finalizefilters ( open_file_descriptors(1,DataHandle), ierr )
        IF ( ierr .GT. 0 .and. ierr .ne. 3 ) THEN
           write(mess,*)'ext_mcel_open_for_read_field: finalizefilters ierr=',ierr
           CALL wrf_error_fatal( TRIM(mess) )
        ENDIF
        mcel_finalized( DataHandle ) = .TRUE.
      ENDIF

      ! a little string munging, assumes that we're getting an ISO compliant date string
      ! basically removing the delimeters

      timestr = "              "
      timestr(1:4)   = DateStr(1:4)    ! YYYY
      timestr(5:6)   = DateStr(6:7)    ! MM
      timestr(7:8)   = DateStr(9:10)   ! DD
      timestr(9:10)  = DateStr(12:13)  ! HH
      timestr(11:12) = DateStr(15:16)  ! MM
      timestr(13:14) = DateStr(18:19)  ! SS

      CALL YYYYMMDDHHMMSS2SECS( timestr, data_time )

      IF ( INDEX( TRIM( ListOfFields(DataHandle) ), TRIM( VarName ) ) .EQ. 0 ) THEN
        write(mess,*)'ext_mcel_open_for_read_field: ',TRIM( VarName ),' is not a field set up for DataHandle ', DataHandle
        CALL wrf_error_fatal( TRIM(mess) )
      ENDIF

      IF ( FieldType .EQ. WRF_REAL ) THEN
        ALLOCATE(temp(ips:ipe,jps:jpe))
write(0,*)'ext_mcel_read_field opened_for_update(DataHandle) ',opened_for_update(DataHandle)
        IF ( opened_for_update(DataHandle) ) THEN
          CALL copy_field_to_temp ( Field, temp, ips, ipe, jps, jpe, ims, ime, jms, jme )
write(0,*)'ext_mcel_read_field ok getData returns ',ierr, Trim(VarName)
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),temp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                       &
            MCEL_FETCHPOLICY_KEEPBLOCK,ierr)
        ELSE
! the difference is there is no KEEP in the FETCHPOLICY
write(0,*)'ext_mcel_read_field ok getData ', Trim(VarName)
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),temp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                      &
            MCEL_FETCHPOLICY_BLOCK,ierr)
write(0,*)'ext_mcel_read_field ok getData returns ',ierr, Trim(VarName)
        ENDIF
        CALL copy_field_to_temp ( Field, temp, ips, ipe, jps, jpe, ims, ime, jms, jme )
        DEALLOCATE(temp)
      ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN

        ALLOCATE(dtemp(ips:ipe,jps:jpe))
write(0,*)'ext_mcel_read_field opened_for_update(DataHandle) ',opened_for_update(DataHandle)
        IF ( opened_for_update(DataHandle) ) THEN
          CALL copy_field_to_dtemp ( Field, dtemp, ips, ipe, jps, jpe, ims, ime, jms, jme )
write(0,*)'ext_mcel_read_field ok getData returns ',ierr, Trim(VarName)
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),dtemp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                       &
            MCEL_FETCHPOLICY_KEEPBLOCK,ierr)
        ELSE
! the difference is there is no KEEP in the FETCHPOLICY
write(0,*)'ext_mcel_read_field ok getData ', Trim(VarName)
          call getData(open_file_descriptors(1,DataHandle),TRIM(VarName),dtemp,                 &
            data_time,data_time,MCEL_TIMECENT_POINT,usemask(DataHandle),                      &
            MCEL_FETCHPOLICY_BLOCK,ierr)
write(0,*)'ext_mcel_read_field ok getData returns ',ierr, Trim(VarName)
        ENDIF
        CALL copy_dtemp_to_field ( dtemp, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )

        DEALLOCATE(dtemp)

      ENDIF
    ENDIF
  endif

  Status = 0

  RETURN

END SUBROUTINE ext_mcel_read_field

!--- write_field
SUBROUTINE ext_mcel_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )
  USE module_ext_mcel
  USE module_date_time   ! defined in share
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) :: DateStr
  CHARACTER*(*) :: VarName
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

  integer ips,ipe,jps,jpe
  integer ims,ime,jms,jme
  integer idex,ierr,i,j

  integer ii,jj,kk,myrank,mcel_type
  integer gSize(2)
  integer idts
  real*8 data_time
  CHARACTER*256 RollOverDeathDate
  CHARACTER*80 mess, timestr
  INTEGER, EXTERNAL :: cast_to_int

!  REAL, DIMENSION( MemoryStart(1):MemoryEnd(1), &
!                   MemoryStart(2):MemoryEnd(2), &
!                   MemoryStart(3):MemoryEnd(3) ) :: Field

  REAL, DIMENSION(*)    :: Field

  real, allocatable, dimension(:,:) :: temp
  integer, allocatable, dimension(:,:) :: itemp
  doubleprecision, allocatable, dimension(:,:) :: dtemp

  INTEGER inttypesize, realtypesize

  IF ( .NOT. int_valid_handle( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_mcel_write_field: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("ext_mcel_write_field: DataHandle not opened" )
  ENDIF

  inttypesize = itypesize
  realtypesize = rtypesize
  IF      ( FieldType .EQ. WRF_REAL ) THEN
    typesize = rtypesize
    mcel_type = MCEL_DATATYPE_REAL
  ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
    mcel_type = MCEL_DATATYPE_DOUBLE
  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
    typesize = itypesize
    mcel_type = MCEL_DATATYPE_INT32
  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    CALL wrf_error_fatal( 'io_int.F90: ext_mcel_write_field, WRF_LOGICAL not yet supported')
  ENDIF

  ips = PatchStart(1) ; ipe = PatchEnd(1) 
  jps = PatchStart(2) ; jpe = PatchEnd(2) 
  ims = MemoryStart(1) ; ime = MemoryEnd(1)
  jms = MemoryStart(2) ; jme = MemoryEnd(2)

  IF ( okay_to_write( DataHandle ) ) THEN
    IF ( TRIM(VarName) .NE. TRIM(LAT_R) .AND. TRIM(VarName) .NE. TRIM(LON_R) .AND. &
         TRIM(VarName) .NE. TRIM(LANDMASK_I) ) THEN
      IF ( .NOT. mcel_finalized( DataHandle ) ) THEN
        IF ( ALLOCATED( xlat ) .AND. ALLOCATED( xlong ) ) THEN
          CALL setLocationsXY( open_file_descriptors(2,DataHandle), xlong, xlat, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal( "ext_mcel_write_field: setLocationsXY" )
        ELSE IF ( deltax .gt. 0. .and. deltay .gt. 0. .and. originx .gt. 0. .and. originy .gt. 0. ) THEN
          dxm(1) = deltax
          dxm(2) = deltay
          call SetDX ( open_file_descriptors(2,DataHandle), dxm, ierr)
          origin(1) = originx
          origin(2) = originy
          call SetOrigin ( open_file_descriptors(2,DataHandle), origin, ierr)
        ELSE
          CALL wrf_error_fatal( "ext_mcel_write_field:noLocationsXY")
        ENDIF
        IF ( ALLOCATED(mask) ) THEN
          CALL setMask ( open_file_descriptors(2,DataHandle) , mask, ierr )
          IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: setMask")
        ENDIF
        CALL setGrid ( open_file_descriptors(1,DataHandle), open_file_descriptors(2,DataHandle), ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: setGrid")
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: setoutputgrid")
        CALL finalize ( open_file_descriptors(1,DataHandle), ierr )
        IF ( ierr .GT. 0 ) THEN
           write(mess,*)'ext_mcel_write_field: finalize ierr=',ierr
           CALL wrf_error_fatal( TRIM(mess) )
        ENDIF
        mcel_finalized( DataHandle ) = .TRUE.
      ENDIF

      timestr(1:4)   = DateStr(1:4)    ! YYYY
      timestr(5:6)   = DateStr(6:7)    ! MM
      timestr(7:8)   = DateStr(9:10)   ! DD
      timestr(9:10)  = DateStr(12:13)  ! HH
      timestr(11:12) = DateStr(15:16)  ! MM
      timestr(13:14) = DateStr(18:19)  ! SS
      CALL YYYYMMDDHHMMSS2SECS( timestr, data_time )

      IF ( FieldType .EQ. WRF_INTEGER ) THEN
        ALLOCATE(itemp(ips:ipe,jps:jpe))
        DO j = jps, jpe
          DO i = ips, ipe
            idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
            itemp(i,j) = cast_to_int(Field( idex ))
          ENDDO
        ENDDO
        CALL storeData( open_file_descriptors(1,DataHandle), TRIM(Varname), &
                        itemp, &
                        data_time, data_time,  &
                        MCEL_TIMECENT_POINT, ierr )
        DEALLOCATE(itemp)
      ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
        ALLOCATE(dtemp(ips:ipe,jps:jpe))
        CALL copy_field_to_dtemp ( Field, dtemp, ips, ipe, jps, jpe, ims, ime, jms, jme )
        CALL storeData( open_file_descriptors(1,DataHandle), TRIM(Varname), &
                        dtemp, &
                        data_time, data_time,  &
                        MCEL_TIMECENT_POINT, ierr )
        DEALLOCATE(dtemp)
      ELSE IF ( FieldType .EQ. WRF_REAL ) THEN
        ALLOCATE(temp(ips:ipe,jps:jpe))
        CALL copy_field_to_temp ( Field, temp, ips, ipe, jps, jpe, ims, ime, jms, jme )
        CALL storeData( open_file_descriptors(1,DataHandle), TRIM(Varname), &
                        temp, &
                        data_time, data_time,  &
                        MCEL_TIMECENT_POINT, ierr )
        DEALLOCATE(temp)
      ENDIF

      IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: storeData")

    ENDIF
  ELSE   ! opened for training

    ! sieve the fields coming in and grab the ones we need for geo registration
    IF      ( TRIM(VarName) .EQ. TRIM(LAT_R) ) THEN
      IF ( ALLOCATED(xlat) ) THEN
        DEALLOCATE(xlat)
      ENDIF
      ALLOCATE(xlat(ips:ipe,jps:jpe))
      CALL copy_field_to_cache ( FieldType , Field, xlat, ips, ipe, jps, jpe, ims, ime, jms, jme )
    ELSE IF ( TRIM(VarName) .EQ. TRIM(LON_R) ) THEN
      IF ( ALLOCATED(xlong) ) THEN
        DEALLOCATE(xlong)
      ENDIF
      ALLOCATE(xlong(ips:ipe,jps:jpe))
      CALL copy_field_to_cache ( FieldType , Field, xlong, ips, ipe, jps, jpe, ims, ime, jms, jme )
    ELSE IF ( TRIM(VarName) .EQ. TRIM(LANDMASK_I) ) THEN
      IF ( ALLOCATED(mask) ) THEN
        DEALLOCATE(mask)
      ENDIF
      ALLOCATE(mask(ips:ipe,jps:jpe))
      IF ( FieldType .EQ. WRF_INTEGER ) THEN
        CALL copy_field_to_cache ( FieldType , Field, mask, ips, ipe, jps, jpe, ims, ime, jms, jme )
      ELSE IF ( FieldType .EQ. WRF_REAL ) THEN
        ALLOCATE(rmask(ips:ipe,jps:jpe))
        CALL copy_field_to_cache ( FieldType , Field, rmask, ips, ipe, jps, jpe, ims, ime, jms, jme )
        mask = NINT( rmask )
        DEALLOCATE(dmask)
      ELSE IF (FieldType .EQ. WRF_DOUBLE ) THEN
        ALLOCATE(dmask(ips:ipe,jps:jpe))
        CALL copy_field_to_cache ( FieldType , Field, dmask, ips, ipe, jps, jpe, ims, ime, jms, jme )
        mask = NINT( dmask )
        DEALLOCATE(dmask)
      ENDIF
    ELSE
      IF ( .NOT. mcel_grid_defined( DataHandle ) ) THEN
        mcel_grid_defined( DataHandle ) = .true.

        gSize(1) = ipe-ips+1
        gSize(2) = jpe-jps+1
write(0,*)'ext_mcel_write_field setSize ',gSize
        CALL setSize ( open_file_descriptors(2,DataHandle), gSize, ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: setSize")

! these will have been set in the call to open_for_write_begin from sysdepinfo
        IF ( mcel_npglobal .NE. -1 .AND. mcel_mystart .NE. -1 .AND.  &
             mcel_mnproc   .NE. -1 .AND. mcel_myproc  .NE. -1     ) THEN
          call setglobalsize(open_file_descriptors(2,DataHandle),mcel_npglobal,ierr)
          call setglobalstart(open_file_descriptors(2,DataHandle),mcel_mystart,ierr)
          call setprocinfo(open_file_descriptors(1,DataHandle),mcel_mnproc,mcel_myproc,ierr)
        ENDIF
        mcel_npglobal=-1 ; mcel_mystart=-1 ; mcel_mnproc=-1 ; mcel_myproc=-1

      ENDIF
      IF ( opened_for_read( DataHandle) ) THEN
        CALL addSources ( open_file_descriptors(1,DataHandle), MCEL_SERVER,  &
  &       TRIM(VarName),1, mcel_type, ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: addSources")
        CALL addOutputs ( open_file_descriptors(1,DataHandle),   &
  &       TRIM(VarName),1, mcel_type, ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: addOutputs")
      ELSE
        CALL addVar ( open_file_descriptors(1,DataHandle), TRIM(VarName), mcel_type, ierr )
        IF ( ierr .NE. 0 ) CALL wrf_error_fatal("ext_mcel_write_field: addVar")
      ENDIF
    ENDIF
  ENDIF
  Status = 0
  RETURN
END SUBROUTINE ext_mcel_write_field

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

 SUBROUTINE copy_field_to_cache ( FieldType , Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   REAL Field(*)
   REAL cache(*)
   IF ( FieldType .EQ. WRF_REAL ) THEN
     CALL copy_field_to_cache_real( Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
   ENDIF
   IF ( FieldType .EQ. WRF_DOUBLE ) THEN
     CALL copy_field_to_cache_double( Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
   ENDIF
   IF ( FieldType .EQ. WRF_INTEGER ) THEN
     CALL copy_field_to_cache_integer( Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
   ENDIF
 END SUBROUTINE copy_field_to_cache

 SUBROUTINE copy_field_to_cache_real ( Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   REAL Field(*)
   DOUBLEPRECISION cache(ips:ipe,jps:jpe)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        cache(i,j) = Field( idex )
     ENDDO
   ENDDO
 END SUBROUTINE copy_field_to_cache_real

 SUBROUTINE copy_field_to_cache_double ( Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   DOUBLEPRECISION Field(*)
   DOUBLEPRECISION cache(ips:ipe,jps:jpe)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        cache(i,j) = Field( idex )
     ENDDO
   ENDDO
 END SUBROUTINE copy_field_to_cache_double

 SUBROUTINE copy_field_to_cache_integer ( Field, cache, ips, ipe, jps, jpe, ims, ime, jms, jme )
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
 END SUBROUTINE copy_field_to_cache_integer

 SUBROUTINE copy_cache_to_field ( cache, FieldType , Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   REAL Field(*)
   REAL cache(*)
   IF ( FieldType .EQ. WRF_REAL ) THEN
     CALL copy_cache_to_field_real( cache, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
   ENDIF
   IF ( FieldType .EQ. WRF_DOUBLE ) THEN
     CALL copy_cache_to_field_double( cache, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
   ENDIF
   IF ( FieldType .EQ. WRF_INTEGER ) THEN
     CALL copy_cache_to_field_integer( cache, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
   ENDIF
 END SUBROUTINE copy_cache_to_field

 SUBROUTINE copy_cache_to_field_real ( cache, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   REAL Field(*)
   DOUBLEPRECISION cache(ips:ipe,jps:jpe)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        Field( idex ) = cache(i,j)
     ENDDO
   ENDDO
 END SUBROUTINE copy_cache_to_field_real
 SUBROUTINE copy_cache_to_field_double ( cache, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   DOUBLEPRECISION Field(*)
   DOUBLEPRECISION cache(ips:ipe,jps:jpe)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        Field( idex ) = cache(i,j)
     ENDDO
   ENDDO
 END SUBROUTINE copy_cache_to_field_double

 SUBROUTINE copy_cache_to_field_integer ( cache, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
   USE module_ext_mcel
   INTEGER FieldType, ips, ipe, jps, jpe, ims, ime, jms, jme
   INTEGER idex, i, j
   INTEGER Field(*)
   INTEGER cache(ips:ipe,jps:jpe)
   DO j = jps, jpe
     DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        Field( idex ) = cache(i,j)
     ENDDO
   ENDDO
 END SUBROUTINE copy_cache_to_field_integer

 SUBROUTINE copy_dtemp_to_field  ( dtemp, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
    IMPLICIT NONE
    INTEGER ips, ipe, jps, jpe, ims, ime, jms, jme
    INTEGER idex, i, j
    DOUBLE PRECISION Field(*)
    DOUBLE PRECISION dtemp( ips:ipe, jps:jpe)
    DO j = jps, jpe
      DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        Field( idex ) = dtemp(i,j) 
      ENDDO
    ENDDO
 END SUBROUTINE copy_dtemp_to_field

 SUBROUTINE copy_field_to_dtemp  ( Field, dtemp, ips, ipe, jps, jpe, ims, ime, jms, jme )
    IMPLICIT NONE
    INTEGER ips, ipe, jps, jpe, ims, ime, jms, jme
    INTEGER idex, i, j
    DOUBLE PRECISION Field(*)
    DOUBLE PRECISION dtemp( ips:ipe, jps:jpe)
    DO j = jps, jpe
      DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        dtemp(i,j) = Field( idex )
      ENDDO
    ENDDO
 END SUBROUTINE copy_field_to_dtemp

 SUBROUTINE copy_temp_to_field  ( temp, Field, ips, ipe, jps, jpe, ims, ime, jms, jme )
    IMPLICIT NONE
    INTEGER ips, ipe, jps, jpe, ims, ime, jms, jme
    INTEGER idex, i, j
    REAL Field(*)
    REAL temp( ips:ipe, jps:jpe)
    DO j = jps, jpe
      DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        Field( idex ) = temp(i,j) 
      ENDDO
    ENDDO
 END SUBROUTINE copy_temp_to_field

 SUBROUTINE copy_field_to_temp  ( Field, temp, ips, ipe, jps, jpe, ims, ime, jms, jme )
    IMPLICIT NONE
    INTEGER ips, ipe, jps, jpe, ims, ime, jms, jme
    INTEGER idex, i, j
    REAL Field(*)
    REAL temp( ips:ipe, jps:jpe)
    DO j = jps, jpe
      DO i = ips, ipe
        idex = i+ips-ims + (j+jps-jms-1)*(ime-ims+1)
        temp(i,j) = Field( idex )
      ENDDO
    ENDDO
 END SUBROUTINE copy_field_to_temp
