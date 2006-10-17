MODULE module_quilt_outbuf_ops
!<DESCRIPTION>
!<PRE>
! This module contains routines and data structures used by the I/O quilt 
! servers to assemble fields ("quilting") and write them to disk.  
!</PRE>
!</DESCRIPTION>
  INTEGER, PARAMETER :: tabsize = 1000
  INTEGER            :: num_entries

  TYPE outrec
    CHARACTER*80                       :: VarName, DateStr, MemoryOrder, Stagger, DimNames(3)
    INTEGER                            :: ndim
    INTEGER, DIMENSION(3)              :: DomainStart, DomainEnd
    INTEGER                            :: FieldType
    REAL,    POINTER, DIMENSION(:,:,:) :: rptr 
    INTEGER, POINTER, DIMENSION(:,:,:) :: iptr
  END TYPE outrec

  TYPE(outrec), DIMENSION(tabsize) :: outbuf_table

CONTAINS

  SUBROUTINE init_outbuf
!<DESCRIPTION>
!<PRE>
! This routine re-initializes module data structures.  
!</PRE>
!</DESCRIPTION>
    IMPLICIT NONE
    INTEGER i
    DO i = 1, tabsize
      outbuf_table(i)%VarName = ""
      outbuf_table(i)%DateStr = ""
      outbuf_table(i)%MemoryOrder = ""
      outbuf_table(i)%Stagger = ""
      outbuf_table(i)%DimNames(1) = ""
      outbuf_table(i)%DimNames(2) = ""
      outbuf_table(i)%DimNames(3) = ""
      outbuf_table(i)%ndim = 0
      NULLIFY( outbuf_table(i)%rptr )
      NULLIFY( outbuf_table(i)%iptr )
    ENDDO
    num_entries = 0
  END SUBROUTINE init_outbuf


  SUBROUTINE write_outbuf ( DataHandle , io_form_arg )
!<DESCRIPTION>
!<PRE>
! This routine writes all of the records stored in outbuf_table to the 
! file referenced by DataHandle using format specified by io_form_arg.  
! This routine calls the package-specific I/O routines to accomplish 
! the write.  
! It then re-initializes module data structures.  
!</PRE>
!</DESCRIPTION>
    USE module_state_description
    IMPLICIT NONE
#include "wrf_io_flags.h"
    INTEGER , INTENT(IN)  :: DataHandle, io_form_arg
    INTEGER               :: ii,ds1,de1,ds2,de2,ds3,de3
    INTEGER               :: Comm, IOComm, DomainDesc ! dummy
    INTEGER               :: Status
    CHARACTER*80          :: mess
    Comm = 0 ; IOComm = 0 ; DomainDesc = 0 
    DO ii = 1, num_entries
      WRITE(mess,*)'writing ', &
                    TRIM(outbuf_table(ii)%DateStr)," ",                                   &
                    TRIM(outbuf_table(ii)%VarName)," ",                                   &
                    TRIM(outbuf_table(ii)%MemoryOrder)
      ds1 = outbuf_table(ii)%DomainStart(1) ; de1 = outbuf_table(ii)%DomainEnd(1)
      ds2 = outbuf_table(ii)%DomainStart(2) ; de2 = outbuf_table(ii)%DomainEnd(2)
      ds3 = outbuf_table(ii)%DomainStart(3) ; de3 = outbuf_table(ii)%DomainEnd(3)

      SELECT CASE ( io_form_arg )

#ifdef NETCDF
        CASE ( IO_NETCDF   )

          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_ncd_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  !*
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  !*
                                 outbuf_table(ii)%DimNames ,                          &  !*
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN
          CALL ext_ncd_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  !*
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  !*
                                 outbuf_table(ii)%DimNames ,                          &  !*
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )
          ENDIF
#endif
#ifdef YYY
      CASE ( IO_YYY   )

          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_yyy_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  !*
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  !*
                                 outbuf_table(ii)%DimNames ,                          &  !*
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN
          CALL ext_yyy_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  !*
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  !*
                                 outbuf_table(ii)%DimNames ,                          &  !*
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )
          ENDIF
#endif
#ifdef GRIB1
      CASE ( IO_GRIB1   )

          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_gr1_write_field ( DataHandle ,                                   &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  !*
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  !*
                                 outbuf_table(ii)%DimNames ,                          &  !*
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN
          CALL ext_gr1_write_field ( DataHandle ,                                   &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  !*
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  !*
                                 outbuf_table(ii)%DimNames ,                          &  !*
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )
          ENDIF
#endif
#ifdef GRIB2
      CASE ( IO_GRIB2   )

          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_gr2_write_field ( DataHandle ,                                   &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  !*
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  !*
                                 outbuf_table(ii)%DimNames ,                          &  !*
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN
          CALL ext_gr2_write_field ( DataHandle ,                                   &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  !*
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  !*
                                 outbuf_table(ii)%DimNames ,                          &  !*
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )
          ENDIF
#endif
#ifdef INTIO
        CASE ( IO_INTIO  )
          IF ( outbuf_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN

          CALL ext_int_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%rptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  !*
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  !*
                                 outbuf_table(ii)%DimNames ,                          &  !*
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ELSE IF ( outbuf_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN

          CALL ext_int_write_field ( DataHandle ,                                     &
                                 TRIM(outbuf_table(ii)%DateStr),                      &
                                 TRIM(outbuf_table(ii)%VarName),                      &
                                 outbuf_table(ii)%iptr(ds1:de1,ds2:de2,ds3:de3),      &
                                 outbuf_table(ii)%FieldType,                          &  !*
                                 Comm, IOComm, DomainDesc ,                           &
                                 TRIM(outbuf_table(ii)%MemoryOrder),                  &
                                 TRIM(outbuf_table(ii)%Stagger),                      &  !*
                                 outbuf_table(ii)%DimNames ,                          &  !*
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 outbuf_table(ii)%DomainStart,                        &
                                 outbuf_table(ii)%DomainEnd,                          &
                                 Status )

          ENDIF
#endif
        CASE DEFAULT
      END SELECT


      IF ( ASSOCIATED( outbuf_table(ii)%rptr) ) DEALLOCATE(outbuf_table(ii)%rptr)
      IF ( ASSOCIATED( outbuf_table(ii)%iptr) ) DEALLOCATE(outbuf_table(ii)%iptr)
      NULLIFY( outbuf_table(ii)%rptr )
      NULLIFY( outbuf_table(ii)%iptr )
    ENDDO
    CALL init_outbuf
  END SUBROUTINE write_outbuf

END MODULE module_quilt_outbuf_ops

! don't let other programs see the definition of this; type mismatches
! on inbuf will result;  may want to make a module program at some point 
  SUBROUTINE store_patch_in_outbuf( inbuf_r, inbuf_i, DateStr, VarName , FieldType, MemoryOrder, Stagger, DimNames, &
                                    DomainStart , DomainEnd , &
                                    MemoryStart , MemoryEnd , &
                                    PatchStart , PatchEnd )
!<DESCRIPTION>
!<PRE>
! This routine does the "output quilting".  
!
! It stores a patch in the appropriate location in a domain-sized array 
! within an element of the outbuf_table data structure.  DateStr, VarName, and 
! MemoryOrder are used to uniquely identify which element of outbuf_table is 
! associated with this array.  If no element is associated, then this routine 
! first assigns an unused element and allocates space within that element for 
! the globally-sized array.  This routine also stores DateStr, VarName, 
! FieldType, MemoryOrder, Stagger, DimNames, DomainStart, and DomainEnd within 
! the same element of outbuf.  
!</PRE>
!</DESCRIPTION>
    USE module_quilt_outbuf_ops
    IMPLICIT NONE
#include "wrf_io_flags.h"
    INTEGER ,                INTENT(IN) :: FieldType
    REAL    , DIMENSION(*) , INTENT(IN) :: inbuf_r
    INTEGER , DIMENSION(*) , INTENT(IN) :: inbuf_i
    INTEGER , DIMENSION(3) , INTENT(IN) :: DomainStart , DomainEnd , MemoryStart , MemoryEnd , PatchStart , PatchEnd
    CHARACTER*(*)    , INTENT(IN) :: DateStr , VarName, MemoryOrder , Stagger, DimNames(3)
! Local
    CHARACTER*120 mess
    INTEGER               :: l,m,n,ii,jj
    LOGICAL               :: found

    ! Find the VarName if it's in the buffer already
    ii = 1
    found = .false.
    DO WHILE ( .NOT. found .AND. ii .LE. num_entries )
      !TBH:  need to test other attributes too!  
      IF ( TRIM(VarName) .EQ. TRIM(outbuf_table(ii)%VarName) ) THEN
        IF ( TRIM(DateStr) .EQ. TRIM(outbuf_table(ii)%DateStr) ) THEN
          IF ( TRIM(MemoryOrder) .EQ. TRIM(outbuf_table(ii)%MemoryOrder) ) THEN
            found = .true.
          ELSE
            CALL wrf_error_fatal("store_patch_in_outbuf: memory order disagreement")
          ENDIF
        ELSE
          CALL wrf_error_fatal("store_patch_in_outbuf: multiple dates in buffer")
        ENDIF
      ELSE
        ii = ii + 1
      ENDIF
    ENDDO
    IF ( .NOT. found ) THEN
      num_entries = num_entries + 1
      IF      ( FieldType .EQ. WRF_FLOAT ) THEN
        ALLOCATE( outbuf_table(num_entries)%rptr(DomainStart(1):DomainEnd(1), &
                                                 DomainStart(2):DomainEnd(2),DomainStart(3):DomainEnd(3)) )
      ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
        ALLOCATE( outbuf_table(num_entries)%iptr(DomainStart(1):DomainEnd(1), &
                                                 DomainStart(2):DomainEnd(2),DomainStart(3):DomainEnd(3)) )
      ELSE
        write(mess,*)"store_patch_in_outbuf: unsupported type ", FieldType
        CALL wrf_error_fatal(mess)
      ENDIF
      outbuf_table(num_entries)%VarName = TRIM(VarName)
      outbuf_table(num_entries)%DateStr = TRIM(DateStr)
      outbuf_table(num_entries)%MemoryOrder = TRIM(MemoryOrder)
      outbuf_table(num_entries)%Stagger = TRIM(Stagger)
      outbuf_table(num_entries)%DimNames(1) = TRIM(DimNames(1))
      outbuf_table(num_entries)%DimNames(2) = TRIM(DimNames(2))
      outbuf_table(num_entries)%DimNames(3) = TRIM(DimNames(3))
      outbuf_table(num_entries)%DomainStart = DomainStart
      outbuf_table(num_entries)%DomainEnd = DomainEnd
      outbuf_table(num_entries)%FieldType = FieldType
      ii = num_entries
    ENDIF
    jj = 1
    IF (  FieldType .EQ. WRF_FLOAT ) THEN
      DO n = PatchStart(3),PatchEnd(3)
        DO m = PatchStart(2),PatchEnd(2)
          DO l = PatchStart(1),PatchEnd(1)
            outbuf_table(ii)%rptr(l,m,n) = inbuf_r(jj)
            jj = jj + 1
          ENDDO
        ENDDO
      ENDDO
    ENDIF
    IF (  FieldType .EQ. WRF_INTEGER ) THEN
      DO n = PatchStart(3),PatchEnd(3)
        DO m = PatchStart(2),PatchEnd(2)
          DO l = PatchStart(1),PatchEnd(1)
            outbuf_table(ii)%iptr(l,m,n) = inbuf_i(jj)
            jj = jj + 1
          ENDDO
        ENDDO
      ENDDO
    ENDIF

    RETURN

  END SUBROUTINE store_patch_in_outbuf

!call add_to_bufsize_for_field( VarName, hdrbufsize+chunksize )

  SUBROUTINE add_to_bufsize_for_field( VarName, Nbytes )
!<DESCRIPTION>
!<PRE>
! This routine is a wrapper for C routine add_to_bufsize_for_field_c() that 
! is used to accumulate buffer sizes.  Buffer size Nbytes is added to the 
! curent buffer size for the buffer named VarName.  Any buffer space 
! associated with VarName is freed.  If a buffer named VarName does not exist, 
! a new one is assigned and its size is set to Nbytes.  
!</PRE>
!</DESCRIPTION>
    USE module_quilt_outbuf_ops
    IMPLICIT NONE
    CHARACTER*(*)    , INTENT(IN) :: VarName
    INTEGER          , INTENT(IN) :: Nbytes
! Local
    CHARACTER*120 mess
    INTEGER               :: i, ierr
    INTEGER               :: VarNameAsInts( 256 )
    VarNameAsInts( 1 ) = len(trim(VarName))
    DO i = 2, len(trim(VarName)) + 1
      VarNameAsInts( i ) = ICHAR( VarName(i-1:i-1) )
    ENDDO
    CALL add_to_bufsize_for_field_c ( VarNameAsInts, Nbytes )
    RETURN
  END SUBROUTINE add_to_bufsize_for_field
  
  SUBROUTINE store_piece_of_field( inbuf, VarName, Nbytes )
!<DESCRIPTION>
!<PRE>
! This routine is a wrapper for C routine store_piece_of_field_c() that 
! is used to store pieces of a field in an internal buffer.  Nbytes bytes of 
! buffer inbuf are appended to the end of the internal buffer named VarName.  
! An error occurs if either an internal buffer named VarName does not exist or 
! if there are fewer than Nbytes bytes left in the internal buffer.  
!</PRE>
!</DESCRIPTION>
    USE module_quilt_outbuf_ops
    IMPLICIT NONE
    INTEGER ,                INTENT(IN) :: Nbytes
    INTEGER , DIMENSION(*) , INTENT(IN) :: inbuf
    CHARACTER*(*)    , INTENT(IN) :: VarName
! Local
    CHARACTER*120 mess
    INTEGER               :: i, ierr
    INTEGER               :: VarNameAsInts( 256 )

    VarNameAsInts( 1 ) = len(trim(VarName))
    DO i = 2, len(trim(VarName)) + 1
      VarNameAsInts( i ) = ICHAR( VarName(i-1:i-1) )
    ENDDO
    CALL store_piece_of_field_c ( inbuf, VarNameAsInts, Nbytes, ierr )
    IF ( ierr .NE. 0 ) CALL wrf_error_fatal ( "store_piece_of_field" )
    RETURN
  END SUBROUTINE store_piece_of_field

  SUBROUTINE retrieve_pieces_of_field( outbuf, VarName, obufsz, Nbytes_tot, lret )
!<DESCRIPTION>
!<PRE>
! This routine is a wrapper for C routine retrieve_pieces_of_field_c() that 
! is used to extract the entire contents (i.e. all previously stored pieces of 
! fields) of the next internal buffer.  The name associated with this internal 
! buffer is returned in VarName.  The number of bytes read is returned in 
! Nbytes_tot.  Bytes are stored in outbuf whose size (in bytes) is obufsz.  
! If there are more than obufsz bytes left in the next internal buffer, then 
! only obufsz bytes are returned and the rest are discarded (probably an error 
! in the making!).  The internal buffer is then freed.  Flag lret is set to 
! .TRUE. iff there are more fields left to extract.  
!</PRE>
!</DESCRIPTION>
    USE module_quilt_outbuf_ops
    IMPLICIT NONE
    INTEGER ,                INTENT(IN) :: obufsz
    INTEGER ,                INTENT(OUT) :: Nbytes_tot
    INTEGER , DIMENSION(*) , INTENT(OUT) :: outbuf
    CHARACTER*(*)    , INTENT(OUT) :: VarName
    LOGICAL                       :: lret   ! true if more, false if not
! Local
    CHARACTER*120 mess
    INTEGER               :: i, iret
    INTEGER               :: VarNameAsInts( 256 )

    CALL retrieve_pieces_of_field_c ( outbuf, VarNameAsInts, obufsz, Nbytes_tot, iret )
    IF ( iret .NE.  0 ) THEN
       lret = .FALSE.
    ELSE
       lret = .TRUE.
       VarName = ' '
       DO i = 2, VarNameAsInts(1) + 1
         VarName(i-1:i-1) = CHAR(VarNameAsInts( i ))
       ENDDO
    ENDIF
    RETURN
  END SUBROUTINE retrieve_pieces_of_field

