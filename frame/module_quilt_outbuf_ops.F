MODULE module_quilt_outbuf_ops
!<DESCRIPTION>
!<PRE>
! This module contains routines and data structures used by the I/O quilt 
! servers to assemble fields ("quilting") and write them to disk.  
!</PRE>
!</DESCRIPTION>
  INTEGER, PARAMETER :: tabsize = 5
  ! The number of entries in outpatch_table (up to a maximum of tabsize)
  INTEGER, SAVE      :: num_entries

! ARP, for PNC-enabled quilting, 02/06/2010
  TYPE varpatch
    LOGICAL                            :: forDeletion ! TRUE if patch to be 
                                                      ! deleted
    INTEGER, DIMENSION(3)              :: PatchStart, PatchEnd, PatchExtent
    REAL,    POINTER, DIMENSION(:,:,:) :: rptr 
    INTEGER, POINTER, DIMENSION(:,:,:) :: iptr
  END TYPE varpatch

  ! With PNC-enabled quilting, each table entry consists of a series of
  ! 'npatch' patches (one for each of the compute PEs that this IOServer has
  ! as clients). We attempt to stitch these together before finally 
  ! writing the data to disk.
  TYPE outpatchlist
    CHARACTER*80                       :: VarName, DateStr, MemoryOrder, &
                                          Stagger, DimNames(3)
    INTEGER, DIMENSION(3)              :: DomainStart, DomainEnd
    INTEGER                            :: FieldType
    ! Total no. of patches in the list PatchList
    INTEGER                            :: nPatch
    ! How many of the patches remain active in PatchList
    INTEGER                            :: nActivePatch
    TYPE(varpatch), ALLOCATABLE, DIMENSION(:) :: PatchList
!    TYPE(varpatch), DIMENSION(tabsize) :: PatchList
  END TYPE outpatchlist

  TYPE(outpatchlist), DIMENSION(tabsize), SAVE :: outpatch_table

  ! List of which of the initial set of patches saved by the IOServer have 
  ! been successfully stitched together. Without any stitching, each patch's 
  ! entry contains just itself:
  !   JoinedPatches(1,ipatch) = ipatch
  ! If jpatch is then stitched to ipatch then we do:
  !   JoinedPatches(2,ipatch) = jpatch
  ! and so on.
  INTEGER, ALLOCATABLE, DIMENSION(:,:), SAVE :: JoinedPatches 
 
  ! The no. of original patches to be stitched together to make each new patch
  ! i.e. if the 2nd new patch consists of 4 of the original patches stitched
  ! together then:
  !   PatchCount(2) = 4
  INTEGER, ALLOCATABLE, DIMENSION(:), SAVE   :: PatchCount

! endARP, for PNC-enabled quilting, 02/06/2010

  TYPE outrec
    CHARACTER*80                       :: VarName, DateStr, MemoryOrder, &
                                          Stagger, DimNames(3)
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
    INTEGER :: i, j
    DO i = 1, tabsize

#ifdef PNETCDF_QUILT
      ! This section for PNC-enabled IO quilting
      outpatch_table(i)%VarName = ""
      outpatch_table(i)%DateStr = ""
      outpatch_table(i)%MemoryOrder = ""
      outpatch_table(i)%Stagger = ""
      outpatch_table(i)%DimNames(1:3) = ""
      outpatch_table(i)%DomainStart(1:3) = 0
      outpatch_table(i)%DomainEnd(1:3)   = 0
      ! We don't free any memory here - that is done immediately after the
      ! write of each patch is completed
      DO j = 1, outpatch_table(i)%npatch
         IF (ALLOCATED(outpatch_table(i)%PatchList)) THEN
            outpatch_table(i)%PatchList(j)%forDeletion   = .FALSE.
            outpatch_table(i)%PatchList(j)%PatchStart(:) = 0
            outpatch_table(i)%PatchList(j)%PatchEnd(:)   = 0
            outpatch_table(i)%PatchList(j)%PatchExtent(:)= 0
            IF (ASSOCIATED(outpatch_table(i)%PatchList(j)%rptr)) &
               NULLIFY( outpatch_table(i)%PatchList(j)%rptr )
            IF (ASSOCIATED(outpatch_table(i)%PatchList(j)%iptr)) &
               NULLIFY( outpatch_table(i)%PatchList(j)%iptr )
            DEALLOCATE(outpatch_table(i)%PatchList)
         ENDIF
      END DO
      outpatch_table(i)%npatch           = 0
      outpatch_table(i)%nActivePatch     = 0
#else
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
#endif

    ENDDO
!write(0,*)'initializing num_entries to 0 '
    num_entries = 0
  END SUBROUTINE init_outbuf

#ifdef PNETCDF_QUILT
  SUBROUTINE write_outbuf_pnc ( DataHandle, io_form_arg, local_comm, &
                                mytask, ntasks )
!<DESCRIPTION>
!<PRE>
! This routine writes all of the records stored in outpatch_table to the 
! file referenced by DataHandle using pNetCDF. The patches constituting
! each record are stitched together as far as is possible before
! the pNetCDF I/O routines are called to accomplish the write.
! 
! It then re-initializes module data structures.  
!</PRE>
!</DESCRIPTION>
    USE module_state_description
    IMPLICIT NONE
    INCLUDE 'mpif.h'
#include "wrf_io_flags.h"
    INTEGER , INTENT(IN)  :: DataHandle, io_form_arg, &
                             local_comm, mytask, ntasks
    INTEGER               :: ii, jj
    INTEGER               :: DomainDesc ! dummy
    INTEGER               :: Status
    INTEGER               :: ipatch, icnt
    INTEGER, ALLOCATABLE, DIMENSION(:) :: count_buf
    INTEGER               :: min_count
    LOGICAL               :: do_indep_write ! If no. of patches differs between
                                            ! IO Servers then we will have to 
                                            ! switch pnetcdf into 
                                            ! independent-writes mode for some 
                                            ! of them
    CHARACTER*256         :: mess

    DomainDesc = 0 

    ALLOCATE(count_buf(ntasks), Stat=Status)
    IF(Status /= 0)THEN
       CALL wrf_error_fatal("write_outbuf_pnc: allocate failed")
    END IF

    WRITE(mess,"('write_outbuf_pnc: table has ', I3,' entries')") num_entries
    CALL wrf_debug ( 200, TRIM( mess ) )

    DO ii = 1, num_entries

      WRITE(mess,*)'write_outbuf_pnc: writing ', &
                    TRIM(outpatch_table(ii)%DateStr)," ",                    &
                    TRIM(outpatch_table(ii)%VarName)," ",                    &
                    TRIM(outpatch_table(ii)%MemoryOrder)
      CALL wrf_debug ( 200, TRIM( mess ) )

      SELECT CASE ( io_form_arg )

        CASE ( IO_PNETCDF   )

          ! Situation is more complicated in this case since field data stored 
          ! as a list of patches rather than in one array of global-domain 
          ! extent.
          ! PatchStart(1) - PatchEnd(1) is dimension with unit stride.

          ! Quilt patches back together where possible in order to minimise 
          ! number of individual writes
          CALL stitch_outbuf_patches(ii)

          ! Check how many patches each of the other IO servers has - we can
          ! only use pNetCDF in collective mode for the same no. of writes
          ! on each IO server. Any other patches will have to be written in
          ! independent mode.
          do_indep_write = .FALSE.
          count_buf(:) = 0
          min_count = outpatch_table(ii)%nActivePatch

          CALL MPI_AllGather(min_count, 1, MPI_INTEGER,      &
	                     count_buf, 1, MPI_INTEGER, &
	                     local_comm, Status)

          ! Work out the minimum no. of patches on any IO Server and whether 
          ! or not we will have to enter independent IO mode.
          min_count = outpatch_table(ii)%nActivePatch
          DO jj=1,ntasks, 1
             IF(count_buf(jj) < min_count) min_count = count_buf(jj)
             IF(outpatch_table(ii)%nActivePatch /= count_buf(jj)) do_indep_write = .TRUE.

          END DO

!          WRITE(mess,*) 'ARPDBG: Min. no. of patches is ', min_count
!          CALL wrf_message(mess)
!          WRITE(mess,*) 'ARPDBG: I have ',count_buf(mytask+1),' patches.'
!          CALL wrf_message(mess)

          IF ( outpatch_table(ii)%FieldType .EQ. WRF_FLOAT ) THEN
             
             ! Loop over the patches in this field up to the number that 
             ! every IO Server has. This is slightly tricky now
             ! that some of them may be 'deleted.' 

             ipatch = 0
             icnt = 0
             DO WHILE ( icnt < min_count )

                ipatch = ipatch + 1

                IF(outpatch_table(ii)%PatchList(ipatch)%forDeletion) CYCLE

                icnt = icnt + 1

                WRITE (mess, "('Calling write for patch: ',I3, ' Start = ',3I4)") ipatch, outpatch_table(ii)%PatchList(ipatch)%PatchStart(1:3)
                CALL wrf_debug ( 200, TRIM( mess ) )
                WRITE (mess,"(29x,'End = ',3I4)") outpatch_table(ii)%PatchList(ipatch)%PatchEnd(1:3)
                CALL wrf_debug ( 200, TRIM( mess ) )

                CALL ext_pnc_write_field ( DataHandle ,                          &
                                 TRIM(outpatch_table(ii)%DateStr),               &
                                 TRIM(outpatch_table(ii)%VarName),               &
                                 outpatch_table(ii)%PatchList(ipatch)%rptr,      &
                                 outpatch_table(ii)%FieldType,                   &!*
                                 local_comm, local_comm, DomainDesc ,            &
                                 TRIM(outpatch_table(ii)%MemoryOrder),           &
                                 TRIM(outpatch_table(ii)%Stagger),               &!*
                                 outpatch_table(ii)%DimNames ,                   &!*
                                 outpatch_table(ii)%DomainStart,                 &
                                 outpatch_table(ii)%DomainEnd,                   &
                                 ! ARP supply magic number as MemoryStart and 
                                 ! MemoryEnd to signal that this routine is 
                                 ! being called from quilting.
                                 -998899,                                        &
                                 -998899,                                        &
                                 outpatch_table(ii)%PatchList(ipatch)%PatchStart,&
                                 outpatch_table(ii)%PatchList(ipatch)%PatchEnd,  &
                                 Status )

                ! Free memory associated with this patch
                DEALLOCATE(outpatch_table(ii)%PatchList(ipatch)%rptr)

             END DO

             IF( do_indep_write )THEN
                ! We must do the next few patches (if any) in independent IO 
                ! mode as not all of the IO Servers have the same no. of 
                ! patches. 
                ! outpatch_table(ii)%nActivePatch holds the no. of live patches
                ! for this IO Server

                CALL ext_pnc_start_independent_mode(DataHandle, Status)

                DO WHILE ( icnt<outpatch_table(ii)%nActivePatch )

                   ipatch = ipatch + 1

                   IF(outpatch_table(ii)%PatchList(ipatch)%forDeletion) CYCLE

                   icnt = icnt + 1

                   CALL ext_pnc_write_field ( DataHandle ,                          &
                                 TRIM(outpatch_table(ii)%DateStr),               &
                                 TRIM(outpatch_table(ii)%VarName),               &
                                 outpatch_table(ii)%PatchList(ipatch)%rptr,      &
                                 outpatch_table(ii)%FieldType,                   &!*
                                 local_comm, local_comm, DomainDesc ,                      &
                                 TRIM(outpatch_table(ii)%MemoryOrder),           &
                                 TRIM(outpatch_table(ii)%Stagger),               &!*
                                 outpatch_table(ii)%DimNames ,                   &!*
                                 outpatch_table(ii)%DomainStart,                 &
                                 outpatch_table(ii)%DomainEnd,                   &
                                 ! ARP supply magic number as MemoryStart and 
                                 ! MemoryEnd to signal that this routine is 
                                 ! being called from quilting.
                                 -998899,                                        &
                                 -998899,                                        &
                                 outpatch_table(ii)%PatchList(ipatch)%PatchStart,&
                                 outpatch_table(ii)%PatchList(ipatch)%PatchEnd,  &
                                 Status )

                   ! Free memory associated with this patch
                   DEALLOCATE(outpatch_table(ii)%PatchList(ipatch)%rptr)

                END DO

                ! End of patches that not every IO Server has so can switch
                ! back to collective mode.
                CALL ext_pnc_end_independent_mode(DataHandle, Status)


             END IF ! Additional patches

          ELSE IF ( outpatch_table(ii)%FieldType .EQ. WRF_INTEGER ) THEN

             ! Loop over the patches in this field up to the number that 
             ! every IO Server has. This is slightly tricky now
             ! that some of them may be 'deleted.' 
             ipatch = 0
             icnt = 0
             DO WHILE ( icnt < min_count )

                ipatch = ipatch + 1

                IF(outpatch_table(ii)%PatchList(ipatch)%forDeletion) CYCLE

                icnt = icnt + 1

                CALL ext_pnc_write_field ( DataHandle ,                           &
                                 TRIM(outpatch_table(ii)%DateStr),                &
                                 TRIM(outpatch_table(ii)%VarName),                &
                                 outpatch_table(ii)%PatchList(ipatch)%iptr,       &
                                 outpatch_table(ii)%FieldType,                    &!*
                                 local_comm, local_comm, DomainDesc,              &
                                 TRIM(outpatch_table(ii)%MemoryOrder),            &
                                 TRIM(outpatch_table(ii)%Stagger),                &!*
                                 outpatch_table(ii)%DimNames ,                    &!*
                                 outpatch_table(ii)%DomainStart,                  &
                                 outpatch_table(ii)%DomainEnd,                    &
                                 ! ARP supply magic number as MemoryStart and 
                                 ! MemoryEnd to signal that this routine is 
                                 ! being called from quilting.
                                 -998899,                                         &
                                 -998899,                                         &
                                 outpatch_table(ii)%PatchList(ipatch)%PatchStart, &
                                 outpatch_table(ii)%PatchList(ipatch)%PatchEnd,   &
                                 Status )

                ! Free memory associated with this patch
                DEALLOCATE(outpatch_table(ii)%PatchList(ipatch)%iptr)

             END DO

             IF( do_indep_write )THEN

                ! We have to do the next few patches in independent IO mode as 
                ! not all of the IO Servers have this many patches. 
                ! outpatch_table(ii)%npatch holds the no. of live patches for 
                ! this IO Server
                CALL ext_pnc_start_independent_mode(DataHandle, Status)

                DO WHILE ( icnt<outpatch_table(ii)%nActivePatch )

                   ipatch = ipatch + 1

                   IF(outpatch_table(ii)%PatchList(ipatch)%forDeletion) CYCLE

                   icnt = icnt + 1

                   CALL ext_pnc_write_field ( DataHandle ,                          &
                                 TRIM(outpatch_table(ii)%DateStr),               &
                                 TRIM(outpatch_table(ii)%VarName),               &
                                 outpatch_table(ii)%PatchList(ipatch)%iptr,      &
                                 outpatch_table(ii)%FieldType,                   &!*
                                 local_comm, local_comm, DomainDesc ,                      &
                                 TRIM(outpatch_table(ii)%MemoryOrder),           &
                                 TRIM(outpatch_table(ii)%Stagger),               &!*
                                 outpatch_table(ii)%DimNames ,                   &!*
                                 outpatch_table(ii)%DomainStart,                 &
                                 outpatch_table(ii)%DomainEnd,                   &
                                 ! ARP supply magic number as MemoryStart and 
                                 ! MemoryEnd to signal that this routine is 
                                 ! being called from quilting.
                                 -998899,                                        &
                                 -998899,                                        &
                                 outpatch_table(ii)%PatchList(ipatch)%PatchStart,&
                                 outpatch_table(ii)%PatchList(ipatch)%PatchEnd,  &
                                 Status )

                   ! Free memory associated with this patch
                   DEALLOCATE(outpatch_table(ii)%PatchList(ipatch)%iptr)

                END DO

                ! End of patches that not every IO Server has so can switch
                ! back to collective mode.
                CALL ext_pnc_end_independent_mode(DataHandle, Status)

             ENDIF ! Have additional patches
          ENDIF

        CASE DEFAULT
      END SELECT

    ENDDO ! Loop over output buffers

    ! Reset the table of output buffers
    CALL init_outbuf()

    DEALLOCATE(count_buf)

  END SUBROUTINE write_outbuf_pnc
#endif

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
    CHARACTER*256         :: mess
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


  SUBROUTINE stitch_outbuf_patches(ibuf)
    USE module_timing
    IMPLICIT none
    INTEGER, INTENT(in) :: ibuf
!<DESCRIPTION>
!<PRE>
! This routine does the "output quilting" for the case where quilting has been
! built to use Parallel NetCDF. Unlike store_patch_in_outbuf() we do not have
! data for the whole domain --- instead we aim to quilt as much of the data as
! possible in order to reduce the number of separate writes that we must do.
!</PRE>
!</DESCRIPTION>
#include "wrf_io_flags.h"
    INTEGER                              :: ipatch, jpatch, ii
    INTEGER                              :: ierr
    INTEGER                              :: npatches
    INTEGER,              DIMENSION(3)   :: newExtent, pos
    INTEGER, ALLOCATABLE, DIMENSION(:,:) :: OldPatchStart
    INTEGER, POINTER,   DIMENSION(:,:,:) :: ibuffer
    REAL,    POINTER,   DIMENSION(:,:,:) :: rbuffer
    CHARACTER*256                        :: mess
integer i,j

!    CALL start_timing()

    IF(LEN_TRIM(outpatch_table(ibuf)%MemoryOrder) < 2)THEN
       ! This field is a scalar or 1D array. Such quantities are replicated
       ! across compute nodes and therefore we need only keep a single
       ! patch - delete all but the first in the list
      IF ( outpatch_table(ibuf)%FieldType .EQ. WRF_FLOAT ) THEN

          DO jpatch=2,outpatch_table(ibuf)%npatch,1
             outpatch_table(ibuf)%PatchList(jpatch)%forDeletion = .TRUE.
             outpatch_table(ibuf)%nActivePatch = &
                                 outpatch_table(ibuf)%nActivePatch - 1
             DEALLOCATE(outpatch_table(ibuf)%PatchList(jpatch)%rptr)
          END DO

      ELSE IF ( outpatch_table(ibuf)%FieldType .EQ. WRF_INTEGER ) THEN

          DO jpatch=2,outpatch_table(ibuf)%npatch,1
             outpatch_table(ibuf)%PatchList(jpatch)%forDeletion = .TRUE.
             outpatch_table(ibuf)%nActivePatch = &
                                 outpatch_table(ibuf)%nActivePatch - 1
             DEALLOCATE(outpatch_table(ibuf)%PatchList(jpatch)%iptr)
          END DO

      ELSE
         CALL wrf_error_fatal("stitch_outbuf_patches: unrecognised Field Type")
      END IF

!     CALL end_timing("stitch_outbuf_patches: deleting replicated patches")

      RETURN

    END IF ! Field is scalar or 1D

    ! Otherwise, this field _is_ distributed across compute PEs and therefore
    ! it's worth trying to stitch patches together...
    ALLOCATE(OldPatchStart(3,outpatch_table(ibuf)%npatch), &
             JoinedPatches(outpatch_table(ibuf)%npatch,    &
                           outpatch_table(ibuf)%npatch),   &
             PatchCount(outpatch_table(ibuf)%npatch),      &
             Stat=ierr)
    IF(ierr /= 0)THEN
       CALL wrf_message('stitch_outbuf_patches: unable to stitch patches as allocate failed.')
       RETURN
    END IF

    JoinedPatches(:,:) = -1
    ! Initialise these arrays to catch failures in the above allocate on
    ! linux-based systems (e.g. Cray XE) where allocation only actually 
    ! performed when requested memory is touched.
    PatchCount(:) = 0
    OldPatchStart(:,:) = 0

    NULLIFY(ibuffer)
    NULLIFY(rbuffer)

    DO jpatch=1,outpatch_table(ibuf)%npatch,1

       ! Each patch consists of just itself initially
       JoinedPatches(1,jpatch) = jpatch
       PatchCount(jpatch) = 1

       ! Store the location of each patch for use after we've decided how to 
       ! stitch them together
       OldPatchStart(:,jpatch) = outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(:)
    END DO

    ! Search through patches to find pairs that we can stitch together
    ipatch = 1
    OUTER: DO WHILE(ipatch < outpatch_table(ibuf)%npatch)

       IF( outpatch_table(ibuf)%PatchList(ipatch)%forDeletion )THEN
          ipatch = ipatch + 1
          CYCLE OUTER
       END IF

       INNER: DO jpatch=ipatch+1,outpatch_table(ibuf)%npatch,1

          IF(outpatch_table(ibuf)%PatchList(jpatch)%forDeletion )THEN
             CYCLE INNER
          END IF

          ! Look for patches that can be concatenated with ipatch in the first
          ! dimension (preferred since that is contiguous in memory in F90)
          !  ________________         ____________  
          !  |               |        |           |
          ! Startx(j)     Endx(j) Startx(i)   Endx(i)
          !
          IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1) == &
              (outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) - 1) )THEN

             ! Patches contiguous in first dimension - do they have the same
             ! extents in the other two dimensions?
             IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3)) )THEN
		
                ! We can concatenate these two patches in first dimension
!                WRITE(mess,"('Can concatenate patches ',I3,' and ',I3,' in dim 1')") ipatch, jpatch
!                CALL wrf_message(mess)

                ! Grow patch ipatch to include jpatch
                outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) = &
                         outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1)
                CALL merge_patches(ibuf, ipatch, jpatch)

                ! Go again...
                ipatch = 1
                CYCLE OUTER
             END IF
          END IF
          !  ______________         ____________  
          !  |             |        |           |
          ! Startx(i)    Endx(i) Startx(j)   Endx(j)
          !
          IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1) == &
             (outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1) + 1))THEN

             ! Patches contiguous in first dimension - do they have the same
             ! extents in the other two dimensions?
             IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3)) )THEN

                 ! We can concatenate these two patches in first dimension
!                 WRITE(mess,"('Can concatenate patches ',I3,' and ',I3,' in dim 1')") ipatch, jpatch
!                 CALL wrf_message(mess)

		! Grow patch ipatch to include jpatch
                outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1) = &
                        outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1)
                CALL merge_patches(ibuf, ipatch, jpatch)

                ! Go again...
		ipatch = 1
		CYCLE OUTER
              END IF
           END IF

           ! Try the second dimension
           IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2) == &
                (outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) - 1))THEN

              ! Patches contiguous in second dimension - do they have the same
              ! extents in the other two dimensions?
              IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1)==     &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3)) )THEN

                 ! We can concatenate these two patches in second dimension
!                 WRITE(mess,"('Can concatenate patches ',I3,' and ',I3,' in dim 2')") ipatch, jpatch
!                 CALL wrf_message(mess)

                 ! Grow patch ipatch to include jpatch
                 outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) = &
                         outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2)
                 CALL merge_patches(ibuf, ipatch, jpatch)

                 ! Go again...
                 ipatch = 1
                 CYCLE OUTER
              END IF
           END IF

           IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2) == &
                (outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2) + 1) )THEN

              ! Patches contiguous in second dimension - do they have the same
              ! extents in the other two dimensions?
              IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1)==     &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3)) )THEN

                 ! We can concatenate these two patches in second dimension
!                 WRITE(mess,"('Can concatenate patches ',I3,' and ',I3,' in dim 2')") ipatch, jpatch
!                 CALL wrf_message(mess)

                 ! Grow patch ipatch to include jpatch
                 outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2) = &
                         outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2)
                 CALL merge_patches(ibuf, ipatch, jpatch)

                 ! Go again...
                 ipatch = 1
                 CYCLE OUTER                 
              END IF
           END IF

           ! Try the third dimension
           IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3) == &
               (outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) - 1) )THEN

              ! Patches contiguous in second dimension - do they have the same
              ! extents in the other two dimensions?
              IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1)==     &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2)) )THEN

                 ! We can concatenate these two patches in the third dimension
!                 WRITE(mess,"('Can concatenate patches ',I3,' and ',I3,' in dim 3')") ipatch, jpatch
!                 CALL wrf_message(mess)

                 ! Grow patch ipatch to include jpatch
                 outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(3) = &
                         outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3)
                 CALL merge_patches(ibuf, ipatch, jpatch)

                 ! Go again...
                 ipatch = 1
                 CYCLE OUTER                 
              END IF
           END IF

           IF(outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3) == &
                (outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3) + 1))THEN

              ! Patches contiguous in second dimension - do they have the same
              ! extents in the other two dimensions?
              IF( (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1)==     &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(1) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(1)   ) .AND.&
                 (outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2)==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchStart(2) ) .AND.&
		 (outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2)  ==      &
                  outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(2)) )THEN

                 ! We can concatenate these two patches in the third dimension
!                 WRITE(mess,"('Can concatenate patches ',I3,' and ',I3,' in dim 3')") ipatch, jpatch
!                 CALL wrf_message(mess)

                 ! Grow patch ipatch to include jpatch
                 outpatch_table(ibuf)%PatchList(ipatch)%PatchEnd(3) = &
                         outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3)
                 CALL merge_patches(ibuf, ipatch, jpatch)

                 ! Go again...
                 ipatch = 1
                 CYCLE OUTER                 
              END IF
           END IF

       END DO INNER

       ipatch = ipatch + 1

    END DO OUTER

    npatches = 0

    DO jpatch=1,outpatch_table(ibuf)%npatch,1

       IF ( outpatch_table(ibuf)%PatchList(jpatch)%forDeletion ) CYCLE

!       WRITE(mess,"('Patch ',I3,': [',I3,': ',I3,'],[',I3,':',I3,'],[',I3,':',I3,']')") jpatch, outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(1), &
!             outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(1),   &
!             outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(2), &
!             outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(2),   &
!             outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(3), &
!             outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(3)
!       CALL wrf_message(mess)

       ! Count how many patches we're left with
       npatches = npatches + 1

       ! If no patches have been merged together to make this patch then we 
       ! don't have to do any more with it
       IF(PatchCount(jpatch) == 1) CYCLE

       ! Get the extent of this patch
       newExtent(:) = outpatch_table(ibuf)%PatchList(jpatch)%PatchEnd(:) - &
                      outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(:) + 1
       ! Allocate a buffer to hold all of its data
       IF ( outpatch_table(ibuf)%FieldType .EQ. WRF_FLOAT ) THEN
          ALLOCATE(rbuffer(newExtent(1), newExtent(2), newExtent(3)), &
                   Stat=ierr)
       ELSE IF ( outpatch_table(ibuf)%FieldType .EQ. WRF_INTEGER ) THEN
          ALLOCATE(ibuffer(newExtent(1), newExtent(2), newExtent(3)), &
                   Stat=ierr)
       END IF
       IF(ierr /= 0)THEN
          CALL wrf_error_fatal('stitch_outbuf_patches: unable to stitch patches as allocate for merge buffer failed.')
          RETURN
       END IF

       ! Copy data into this buffer from each of the patches that are being
       ! stitched together
       IF( ASSOCIATED(rbuffer) )THEN

!         CALL start_timing()

          DO ipatch=1,PatchCount(jpatch),1

             ii = JoinedPatches(ipatch, jpatch)

             ! Work out where to put it - the PatchList(i)%PatchStart() has been
             ! updated to hold the start of the newly quilted patch i. It will
             ! therefore be less than or equal to the starts of each of the 
             ! constituent patches.
             pos(:) = OldPatchStart(:,ii) - &
                      outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(:) + 1
             ! Do the copy - can use the PatchExtent data here because that
             ! wasn't modified during the stitching of the patches.

             rbuffer(pos(1): pos(1)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(1)-1, &
                     pos(2): pos(2)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(2)-1, &
                     pos(3): pos(3)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(3)-1 ) &
                             = &
                      outpatch_table(ibuf)%PatchList(ii)%rptr(:, :, :)

             ! Having copied the data from this patch, we can free-up the 
             ! associated buffer
             DEALLOCATE(outpatch_table(ibuf)%PatchList(ii)%rptr)
          END DO

!         CALL end_timing("Data copy into new real patch")

          ! Re-assign the pointer associated with this patch to the new,
          ! larger, buffer containing the quilted patches
          outpatch_table(ibuf)%PatchList(jpatch)%rptr => rbuffer

          ! Unset the original pointer to this buffer
          NULLIFY(rbuffer)

       ELSE IF( ASSOCIATED(ibuffer) )THEN

!         CALL start_timing()

          DO ipatch=1,PatchCount(jpatch),1

             ii = JoinedPatches(ipatch, jpatch)

             ! Work out where to put it
             pos(:) = OldPatchStart(:,ii) - &
                      outpatch_table(ibuf)%PatchList(jpatch)%PatchStart(:) + 1
             ! Do the copy - can use the PatchExtent data here because that
             ! wasn't modified during the stitching of the patches.
             ibuffer(pos(1): &
                 pos(1)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(1)-1, &
                 pos(2): &
                 pos(2)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(2)-1, &
                 pos(3): &
                 pos(3)+outpatch_table(ibuf)%PatchList(ii)%PatchExtent(3)-1 ) = &
                      outpatch_table(ibuf)%PatchList(ii)%iptr(:, :, :)

             DEALLOCATE(outpatch_table(ibuf)%PatchList(ii)%iptr)
          END DO

!         CALL end_timing("Data copy into new integer patch")

          ! Re-assign the pointer associated with this patch to the new,
          ! larger, buffer containing the quilted patches
          outpatch_table(ibuf)%PatchList(jpatch)%iptr => ibuffer
          NULLIFY(ibuffer)

       END IF

    END DO

    WRITE(mess,*) "--------------------------"
    CALL wrf_debug ( 200, TRIM( mess ) )

    ! Record how many patches we're left with
    outpatch_table(ibuf)%nPatch = npatches

    DEALLOCATE(OldPatchStart, JoinedPatches, PatchCount)

!   CALL end_timing("stitch patches")

  END SUBROUTINE stitch_outbuf_patches

  !-------------------------------------------------------------------------
  SUBROUTINE merge_patches(itab, ipatch, jpatch)
    INTEGER, INTENT(in) :: itab, ipatch, jpatch
    ! Merge patch jpatch into patch ipatch and then 'delete' jpatch
    INTEGER :: ii

    ! Keep track of which patches we've merged: ipatch takes
    ! on all of the original patches which currently make up 
    ! jpatch.
    DO ii=1,PatchCount(jpatch),1
       PatchCount(ipatch) = PatchCount(ipatch) + 1
       JoinedPatches(PatchCount(ipatch),ipatch) = JoinedPatches(ii,jpatch)
    END DO
    ! and mark patch jpatch for deletion
    outpatch_table(itab)%PatchList(jpatch)%forDeletion = .TRUE.
    ! decrement the count of active patches
    outpatch_table(itab)%nActivePatch = outpatch_table(itab)%nActivePatch - 1

  END SUBROUTINE merge_patches

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
    CHARACTER*(*)          , INTENT(IN) :: DateStr , VarName, MemoryOrder , Stagger, DimNames(3)
! Local
    CHARACTER*256         ::  mess
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

! don't let other programs see the definition of this; type mismatches
! on inbuf will result;  may want to make a module program at some point 
  SUBROUTINE store_patch_in_outbuf_pnc( inbuf_r, inbuf_i, DateStr, VarName , &
                                        FieldType, MemoryOrder, Stagger,     &
                                        DimNames ,                &
                                        DomainStart , DomainEnd , &
                                        MemoryStart , MemoryEnd , &
                                        PatchStart  , PatchEnd  , &
                                        ntasks )
!<DESCRIPTION>
!<PRE>
! This routine stores a patch in an array within an element of the 
! outpatch_table%PatchList data structure.  DateStr, VarName, and 
! MemoryOrder are used to uniquely identify which element of outpatch_table is 
! associated with this array.  If no element is associated, then this routine 
! first assigns an unused element and allocates space within that element.  
! This routine also stores DateStr, VarName, 
! FieldType, MemoryOrder, Stagger, DimNames, DomainStart, and DomainEnd within 
! the same element of outpatch.  
!</PRE>
!</DESCRIPTION>
    USE module_quilt_outbuf_ops, Only: outpatch_table, tabsize, num_entries
    USE module_timing
    IMPLICIT NONE
#include "wrf_io_flags.h"
    INTEGER ,               INTENT(IN) :: FieldType
    REAL    , DIMENSION(*), INTENT(IN) :: inbuf_r
    INTEGER , DIMENSION(*), INTENT(IN) :: inbuf_i
    INTEGER , DIMENSION(3), INTENT(IN) :: DomainStart, DomainEnd, MemoryStart,&
                                          MemoryEnd , PatchStart , PatchEnd
    CHARACTER*(*)         , INTENT(IN) :: DateStr , VarName, MemoryOrder , &
                                          Stagger, DimNames(3)
    INTEGER, INTENT(IN) :: ntasks ! Number of compute tasks associated with 
                                  ! this IO server
! Local
    CHARACTER*256         :: mess
    INTEGER               :: l,m,n,ii,jj,ipatch,ierr
    LOGICAL               :: found

!   CALL start_timing()

    ! Find the VarName if it's in the buffer already
    ii = 1
    found = .false.
    DO WHILE ( .NOT. found .AND. ii .LE. num_entries )
      !TBH:  need to test other attributes too!  
      IF ( TRIM(VarName) .EQ. TRIM(outpatch_table(ii)%VarName) ) THEN
        IF ( TRIM(DateStr) .EQ. TRIM(outpatch_table(ii)%DateStr) ) THEN
          IF ( TRIM(MemoryOrder) .EQ. TRIM(outpatch_table(ii)%MemoryOrder) ) THEN
            found = .true.
          ELSE
            CALL wrf_error_fatal("store_patch_in_outbuf_pnc: memory order disagreement")
          ENDIF
        ELSE
          CALL wrf_error_fatal("store_patch_in_outbuf_pnc: multiple dates in buffer")
        ENDIF
      ELSE
        ii = ii + 1
      ENDIF
    ENDDO
    IF ( .NOT. found ) THEN
      num_entries = num_entries + 1
      IF(num_entries > tabsize)THEN
         WRITE(mess,*) 'Number of entries in outpatch_table has exceeded tabsize (',&
         tabsize,') in module_quilt_outbuf_ops::store_patch_in_outbuf_pnc'
         CALL wrf_error_fatal(mess)
      END IF
      outpatch_table(num_entries)%npatch = 0

      outpatch_table(num_entries)%VarName     = TRIM(VarName)
      outpatch_table(num_entries)%DateStr     = TRIM(DateStr)
      outpatch_table(num_entries)%MemoryOrder = TRIM(MemoryOrder)
      outpatch_table(num_entries)%Stagger     = TRIM(Stagger)
      outpatch_table(num_entries)%DimNames(1) = TRIM(DimNames(1))
      outpatch_table(num_entries)%DimNames(2) = TRIM(DimNames(2))
      outpatch_table(num_entries)%DimNames(3) = TRIM(DimNames(3))
      outpatch_table(num_entries)%DomainStart = DomainStart
      outpatch_table(num_entries)%DomainEnd   = DomainEnd
      outpatch_table(num_entries)%FieldType   = FieldType
      ! Allocate the table for the list of patches for this output - it
      ! will have as many entries as there are compute tasks associated with
      ! this IO server.
      IF ( ALLOCATED(outpatch_table(num_entries)%PatchList) ) &
         DEALLOCATE(outpatch_table(num_entries)%PatchList)
      ALLOCATE(outpatch_table(num_entries)%PatchList(ntasks), Stat=ierr)
      IF(ierr /= 0)THEN
         WRITE(mess,*)'num_entries ',num_entries,' ntasks ',ntasks,' ierr ',ierr
         CALL wrf_message(mess)
         WRITE(mess,*)'Allocation for ',ntasks, &
                      ' patches in store_patch_in_outbuf_pnc() failed.'
         CALL wrf_error_fatal( mess )
      ENDIF
      ! Initialise the list of patches
      DO ii=1, ntasks, 1
         outpatch_table(num_entries)%PatchList(ii)%forDeletion = .FALSE.
         NULLIFY(outpatch_table(num_entries)%PatchList(ii)%rptr)
         NULLIFY(outpatch_table(num_entries)%PatchList(ii)%iptr)
         outpatch_table(num_entries)%PatchList(ii)%PatchStart(:) = 0
         outpatch_table(num_entries)%PatchList(ii)%PatchEnd(:) = 0
         outpatch_table(num_entries)%PatchList(ii)%PatchExtent(:) = 0
      END DO 

      ii = num_entries

      WRITE(mess,*)'Adding field entry no. ',num_entries
      CALL wrf_debug ( 200, TRIM( mess ) )
      WRITE(mess,*)'Variable = ',TRIM(VarName)
      CALL wrf_debug ( 200, TRIM( mess ) )
      WRITE(mess,*)'Domain start = ',DomainStart(:)
      CALL wrf_debug ( 200, TRIM( mess ) )
      WRITE(mess,*)'Domain end   = ',DomainEnd(:)
      CALL wrf_debug ( 200, TRIM( mess ) )
    ENDIF

    ! We only store > 1 patch if the field has two or more dimensions. Scalars
    ! and 1D arrays are replicated across compute nodes and therefore we only
    ! need keep a single patch.
    IF(LEN_TRIM(outpatch_table(ii)%MemoryOrder) >= 2 .OR. &
       outpatch_table(ii)%npatch < 1)THEN

       ! Add another patch
       outpatch_table(ii)%npatch = outpatch_table(ii)%npatch + 1
       outpatch_table(ii)%nActivePatch = outpatch_table(ii)%npatch

       ipatch = outpatch_table(ii)%npatch

       outpatch_table(ii)%PatchList(ipatch)%PatchStart(:) = PatchStart(:)
       outpatch_table(ii)%PatchList(ipatch)%PatchEnd(:)   = PatchEnd(:)
       outpatch_table(ii)%PatchList(ipatch)%PatchExtent(:)= PatchEnd(:) - PatchStart(:) + 1

       ierr = 0

       IF      ( FieldType .EQ. WRF_FLOAT ) THEN
          ALLOCATE( outpatch_table(ii)%PatchList(ipatch)%rptr( &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(1), &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(2), &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(3)),&
                                                 Stat=ierr)
       ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
          ALLOCATE( outpatch_table(ii)%PatchList(ipatch)%iptr( &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(1), &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(2), &
                                                 outpatch_table(ii)%PatchList(ipatch)%PatchExtent(3)),&
                                                 Stat=ierr)
       ELSE
          WRITE(mess,*)"store_patch_in_outbuf_pnc: unsupported type ", FieldType
          CALL wrf_error_fatal(mess)
       ENDIF

       IF(ierr /= 0)THEN
          WRITE(mess,*)"store_patch_in_outbuf_pnc: failed to allocate memory to hold patch for var. ", TRIM(VarName)
          CALL wrf_error_fatal(mess)
       END IF

       jj = 1

       WRITE(mess,"('Variable ',(A),', patch ',I3,': (',I3,':',I3,',',I3,':',I3,',',I3,':',I3,')')")&
                TRIM(outpatch_table(ii)%VarName),  &
                ipatch, &
                PatchStart(1),PatchEnd(1), &
                PatchStart(2),PatchEnd(2), &
                PatchStart(3),PatchEnd(3)
       CALL wrf_debug ( 200, TRIM( mess ) )

       IF (  FieldType .EQ. WRF_FLOAT ) THEN
          DO n = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(3),1
             DO m = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(2),1
                DO l = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(1),1
                   outpatch_table(ii)%PatchList(ipatch)%rptr(l,m,n) = inbuf_r(jj)
                   jj = jj + 1
                ENDDO
             ENDDO
          ENDDO
       ENDIF
       IF (  FieldType .EQ. WRF_INTEGER ) THEN
          DO n = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(3),1
             DO m = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(2),1
                DO l = 1,outpatch_table(ii)%PatchList(ipatch)%PatchExtent(1),1
                   outpatch_table(ii)%PatchList(ipatch)%iptr(l,m,n) = inbuf_i(jj)
                   jj = jj + 1
                ENDDO
             ENDDO
          ENDDO
       ENDIF

    END IF ! We need to add another patch

!   CALL end_timing("store patch in outbuf")

    RETURN

  END SUBROUTINE store_patch_in_outbuf_pnc

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
    CHARACTER*256         :: mess
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
    CHARACTER*(*)          , INTENT(IN) :: VarName
! Local
    CHARACTER*256         :: mess
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
    CHARACTER*256         :: mess
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

