!WRF:DRIVER_LAYER:IO
!
#define DEBUG_LVL 50
!#define mpi_x_comm_size(i,j,k)  Mpi_Comm_Size ( i,j,k ) ; write(0,*) __LINE__
#define mpi_x_comm_size(i,j,k)  Mpi_Comm_Size ( i,j,k )

MODULE module_ext_quilt
  INTEGER, PARAMETER :: int_num_handles = 99
  LOGICAL, DIMENSION(int_num_handles) :: okay_to_write, int_handle_in_use, okay_to_commit
  INTEGER, DIMENSION(int_num_handles) :: int_num_bytes_to_write, io_form
  REAL, POINTER    :: int_local_output_buffer(:)
  INTEGER          :: int_local_output_cursor
  LOGICAL          :: quilting_enabled
  LOGICAL          :: disable_quilt = .FALSE.

#ifdef DM_PARALLEL
  INTEGER mpi_comm_local
  INTEGER mpi_comm_io_groups(100)
  INTEGER nio_tasks_in_group
  INTEGER nio_groups
  INTEGER nio_tasks_per_group
  INTEGER ncompute_tasks
  INTEGER ntasks
  INTEGER mytask

  INTEGER, PARAMETER           :: onebyte = 1
  INTEGER comm_io_servers, iserver, hdrbufsize, obufsize
  INTEGER, DIMENSION(4096)     :: hdrbuf
  INTEGER, DIMENSION(int_num_handles)     :: handle
#endif

  CONTAINS

#if defined( DM_PARALLEL ) && !defined( STUBMPI )
    SUBROUTINE int_get_fresh_handle( retval )
      INTEGER i, retval
      retval = -1
      DO i = 1, int_num_handles
        IF ( .NOT. int_handle_in_use(i) )  THEN
          retval = i
          GOTO 33
        ENDIF
      ENDDO
33    CONTINUE
      IF ( retval < 0 )  THEN
        CALL wrf_error_fatal("frame/module_io_quilt.F: int_get_fresh_handle() can not")
      ENDIF
      int_handle_in_use(i) = .TRUE.
      NULLIFY ( int_local_output_buffer )
    END SUBROUTINE int_get_fresh_handle

    SUBROUTINE setup_quilt_servers ( nio_tasks_per_group,     &
                                     mytask,                  &
                                     ntasks,                  &
                                     n_groups_arg,            &
                                     nio,                     &
                                     mpi_comm_wrld,           &
                                     mpi_comm_local,          &
                                     mpi_comm_io_groups)
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INTEGER,                      INTENT(IN)  :: nio_tasks_per_group, mytask, ntasks, &
                                                   n_groups_arg, mpi_comm_wrld
!
! MPI_COMM_LOCAL is the communicator for the local groups of tasks. For the compute 
!   tasks it is the group of compute tasks; for a server group it the communicator of
!   tasks in the server group.
!
! MPI_COMM_IO_GROUPS is the communicator that is a subset of the compute tasks that
! are associated with a task in each of the server groups. On a compute task, which has
! an associate in each of the server groups, this is treated as an array; each element
! corresponds to a different server group. On the server group this is set up so that
! only element 1 is the communicator (because each server task is part of only one
! io_group) and it is always the nio'th (the last) task in the io_group.
!
! When the total number of extra I/O tasks does not divide evenly by
! the number of io server groups request, the remainder tasks are not used (wasted)
!
      INTEGER,  INTENT(OUT)                     :: mpi_comm_local, nio
      INTEGER, DIMENSION(100),      INTENT(OUT) :: mpi_comm_io_groups
! Local
      INTEGER                     :: i, j, ii, comdup, ierr, niotasks, n_groups
      INTEGER, DIMENSION(ntasks)  :: icolor
      CHARACTER*128 mess

#ifndef STUBMPI

      n_groups = n_groups_arg
      IF ( n_groups .LT. 1 ) n_groups = 1

      ! nio is number of io tasks per group.  If there arent enough tasks to satisfy
      ! the requirement that there be at least as many compute tasks as io tasks in
      ! each group, then just print a warning and dump out of quilting

      nio = nio_tasks_per_group
      ncompute_tasks = ntasks - (nio * n_groups)
      IF ( ncompute_tasks .LT. nio ) THEN 
        WRITE(mess,'("Not enough tasks to have ",I3," groups of ",I3," I/O tasks. No quilting.")')n_groups,nio
        nio            = 0
        ncompute_tasks = ntasks
      ELSE                                   
        WRITE(mess,'("Quilting with ",I3," groups of ",I3," I/O tasks.")')n_groups,nio
      ENDIF                                   
      CALL wrf_message(mess)
    
      IF ( nio .LT. 0 ) THEN
        nio = 0
      ENDIF
      IF ( nio .EQ. 0 ) THEN
        quilting_enabled = .FALSE.
        mpi_comm_local = MPI_COMM_WORLD
        mpi_comm_io_groups = MPI_COMM_WORLD
        RETURN
      ENDIF
      quilting_enabled = .TRUE.

! First construct the local communicators
! prepare to split the communicator by designating compute-only tasks
      DO i = 1, ncompute_tasks
        icolor(i) = 0
      ENDDO
      ii = 1
! and designating the groups of i/o tasks
      DO i = ncompute_tasks+1, ntasks, nio
        DO j = i, i+nio-1
          icolor(j) = ii
        ENDDO
        ii = ii+1
      ENDDO
      CALL MPI_Comm_dup(mpi_comm_wrld,comdup,ierr)
      CALL MPI_Comm_split(comdup,icolor(mytask+1),mytask,mpi_comm_local,ierr)

! Now construct the communicators for the io_groups; round-robining the compute tasks
      DO i = 1, ncompute_tasks
        icolor(i) = mod(i-1,nio)
      ENDDO
! ... and add the io servers as the last task in each group
      DO j = 1, n_groups
        ii = 0
        DO i = ncompute_tasks+(j-1)*nio+1,ncompute_tasks+j*nio
          icolor(i) = ii
          ii = ii+1
        ENDDO
        CALL MPI_Comm_dup(mpi_comm_wrld,comdup,ierr)
        CALL MPI_Comm_split(comdup,icolor(mytask+1),mytask,mpi_comm_io_groups(j),ierr)
      ENDDO
! If I am an I/O server, figure out which group I'm in and make that group's
! communicator the first element in the mpi_comm_io_groups array
      IF ( mytask+1 .GT. ncompute_tasks ) THEN
        niotasks = ntasks - ncompute_tasks
        i = mytask - ncompute_tasks
        j = i / nio + 1
        mpi_comm_io_groups(1) = mpi_comm_io_groups(j)
      ENDIF
#endif

    END SUBROUTINE setup_quilt_servers

    SUBROUTINE quilt
      USE module_state_description
      USE module_quilt_outbuf_ops
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INCLUDE 'intio_tags.h'
      INCLUDE 'wrf_io_flags.h'
      INTEGER itag, ninbuf, ntasks_io_group, ntasks_local_group, mytask_local, ierr
      INTEGER istat, ishutdown, ishuttag, ishuthand
      LOGICAL ishutflag
      INTEGER mytask_io_group
      INTEGER   :: nout_set = 0
      INTEGER   :: obufsize, bigbufsize, inttypesize, chunksize, sz
      REAL      :: dummy
      INTEGER, ALLOCATABLE, DIMENSION(:) :: obuf, bigbuf
      REAL,    ALLOCATABLE, DIMENSION(:) :: RDATA
      INTEGER, ALLOCATABLE, DIMENSION(:) :: IDATA
      CHARACTER (LEN=512) :: CDATA
      CHARACTER (LEN=80) :: fname
      INTEGER icurs, hdrbufsize, itypesize, ftypesize, Status, fstat, io_form_arg
      INTEGER :: DataHandle, FieldType, Comm, IOComm, DomainDesc, code, Count
      INTEGER, DIMENSION(3) :: DomainStart , DomainEnd , MemoryStart , MemoryEnd , PatchStart , PatchEnd
      INTEGER :: dummybuf(1)
      CHARACTER (len=80) :: DateStr , Element, VarName, MemoryOrder , Stagger , DimNames(3), FileName, SysDepInfo, mess
      INTEGER, EXTERNAL :: use_package
      INTEGER, EXTERNAL :: get_hdr_tag
      LOGICAL           :: stored_write_record, retval
      INTEGER iii, vid

!

#ifdef NETCDF
      CALL ext_ncd_ioinit( SysDepInfo, ierr)
#endif

#ifndef STUBMPI

      okay_to_commit = .false.
      ishuttag = 202020
      ninbuf = 0
      CALL mpi_x_comm_size( mpi_comm_io_groups(1), ntasks_io_group,    ierr )
      CALL MPI_COMM_RANK( mpi_comm_io_groups(1), mytask_io_group,    ierr )
      CALL mpi_x_comm_size( mpi_comm_local,        ntasks_local_group, ierr )
      CALL MPI_COMM_RANK( mpi_comm_local,        mytask_local,       ierr )

      CALL MPI_TYPE_SIZE( MPI_INTEGER, inttypesize, ierr )
      IF ( inttypesize <= 0 ) THEN
        CALL wrf_error_fatal("external/RSL/module_dm.F: quilt: type size <= 0 invalid")
      ENDIF
! infinite loop until shutdown message received
      CALL mpi_irecv( ishutdown, 1, MPI_INTEGER, 0, ishuttag, MPI_COMM_WORLD, ishuthand, ierr )
      DO WHILE (.TRUE.)

        ! wait for info from compute tasks in the I/O group that we're ready to rock
        ! obufsize will contain number of *bytes*

        CALL MPI_Reduce( ninbuf, obufsize, 1, MPI_INTEGER,  &
                         MPI_SUM, mytask_io_group,          &
                         mpi_comm_io_groups(1), ierr )
        IF ( obufsize .LT. 0 ) THEN
#ifdef NETCDF
          CALL ext_ncd_ioexit( Status )
#endif
          CALL mpi_finalize(ierr)
          CALL wrf_error_fatal ( 'obufsize .LT. 0' )
        ENDIF

        ! Allocate the buffer that's big enough -- note: obuf is size in *bytes*
        ! so we need to pare this down, since the buffer is "real" (but not
        ! necessarily)

        ALLOCATE( obuf( (obufsize+1)/inttypesize ) )

        ! let's roll; get the data from the compute procs and put in obuf
        CALL collect_on_comm( mpi_comm_io_groups(1),        &
                              onebyte,                      &
                              dummy, 0,                     &
                              obuf, obufsize )

        CALL init_store_piece_of_field
        CALL mpi_type_size ( MPI_INTEGER , itypesize , ierr )
! calculate the size of the buffer required for each field
        vid = 0
        icurs = inttypesize
        DO WHILE ( icurs .lt. obufsize )
          SELECT CASE ( get_hdr_tag( obuf ( icurs / inttypesize ) ) )
            CASE ( int_field )
              CALL int_get_write_field_header ( obuf(icurs/inttypesize), hdrbufsize, itypesize, ftypesize,  &
                                                DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm, &
                                                DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                                DomainStart , DomainEnd ,                                    &
                                                MemoryStart , MemoryEnd ,                                    &
                                                PatchStart , PatchEnd )
              chunksize = (PatchEnd(1)-PatchStart(1)+1)*(PatchEnd(2)-PatchStart(2)+1)* &
                          (PatchEnd(3)-PatchStart(3)+1)*ftypesize

              call add_to_bufsize_for_field( VarName, hdrbufsize )
              icurs = icurs + hdrbufsize
              IF ( DomainDesc .NE. 333933 ) THEN   ! magic number
                call add_to_bufsize_for_field( VarName, chunksize )
                icurs = icurs + chunksize
              ENDIF
            CASE DEFAULT
              hdrbufsize = obuf(icurs/inttypesize)
              write(VarName,'(I5.5)')vid 
              call add_to_bufsize_for_field( VarName, hdrbufsize )
              icurs = icurs + hdrbufsize
              vid = vid+1
          END SELECT
        ENDDO
! store the fields
        vid = 0
        icurs = inttypesize
        DO WHILE ( icurs .lt. obufsize )
          SELECT CASE ( get_hdr_tag( obuf ( icurs / inttypesize ) ) )
            CASE ( int_field )
              CALL int_get_write_field_header ( obuf(icurs/inttypesize), hdrbufsize, itypesize, ftypesize,  &
                                                DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm, &
                                                DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                                DomainStart , DomainEnd ,                                    &
                                                MemoryStart , MemoryEnd ,                                    &
                                                PatchStart , PatchEnd )
              chunksize = (PatchEnd(1)-PatchStart(1)+1)*(PatchEnd(2)-PatchStart(2)+1)* &
                          (PatchEnd(3)-PatchStart(3)+1)*ftypesize

              call store_piece_of_field( obuf(icurs/inttypesize), VarName, hdrbufsize )
              icurs = icurs + hdrbufsize
              IF ( DomainDesc .NE. 333933 ) THEN   ! magic number
                call store_piece_of_field( obuf(icurs/inttypesize), VarName, chunksize )
                icurs = icurs + chunksize
              ENDIF
            CASE DEFAULT
              hdrbufsize = obuf(icurs/inttypesize)
              write(VarName,'(I5.5)')vid 
              call store_piece_of_field( obuf(icurs/inttypesize), VarName, hdrbufsize )
              icurs = icurs + hdrbufsize
              vid = vid+1
          END SELECT
        ENDDO

        CALL init_retrieve_pieces_of_field
        CALL retrieve_pieces_of_field ( obuf , VarName, obufsize, sz, retval )
        CALL MPI_Reduce( sz, bigbufsize, 1, MPI_INTEGER,  &
                         MPI_SUM, ntasks_local_group-1,         &
                         mpi_comm_local, ierr )

        DO WHILE ( retval )

          IF ( mytask_local .EQ. ntasks_local_group-1 ) THEN
            ALLOCATE( bigbuf( (bigbufsize+1)/inttypesize ) )
          ENDIF

          CALL collect_on_comm( mpi_comm_local,                    &
                                onebyte,                           &
                                obuf, sz,  &
                                bigbuf, bigbufsize )

          IF ( mytask_local .EQ. ntasks_local_group-1 ) THEN

            icurs = inttypesize  ! icurs is a byte counter, but buffer is integer

            stored_write_record = .false.

            DO WHILE ( icurs .lt. bigbufsize )
              CALL mpi_type_size ( MPI_INTEGER , itypesize , ierr )

              SELECT CASE ( get_hdr_tag( bigbuf(icurs/inttypesize) ) )
                CASE ( int_noop )
                  CALL int_get_noop_header( bigbuf(icurs/inttypesize), hdrbufsize, itypesize )
                  icurs = icurs + hdrbufsize

                CASE ( int_dom_td_real )
                  CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                  ALLOCATE( RData( bigbuf(icurs/inttypesize + 4 ) ) )      ! 5 is the count of data items for this record ; defined in collect_on_comm.c
                  CALL int_get_td_header( bigbuf(icurs/inttypesize), hdrbufsize, inttypesize, ftypesize, &
                                          DataHandle, DateStr, Element, RData, Count, code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( RData )
                CASE ( int_dom_ti_real )
                  CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                  ALLOCATE( RData( bigbuf(icurs/inttypesize + 4 ) ) )      ! 5 is the count of data items for this record ; defined in collect_on_comm.c
                  CALL int_get_ti_header( bigbuf(icurs/inttypesize), hdrbufsize, inttypesize, ftypesize, &
                                          DataHandle, Element, RData, Count, code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( RData )

                CASE ( int_dom_td_integer )
                  CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                  ALLOCATE( RData( bigbuf(icurs/inttypesize + 4 ) ) )      ! 5 is the count of data items for this record ; defined in collect_on_comm.c
                  CALL int_get_td_header( bigbuf(icurs/inttypesize), hdrbufsize, inttypesize, ftypesize, &
                                          DataHandle, DateStr, Element, IData, Count, code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( RData )

                CASE ( int_dom_ti_integer )

                  CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                  ALLOCATE( IData( bigbuf(icurs/inttypesize + 4 ) ) )      ! 5 is the count of data items for this record ; defined in collect_on_comm.c
                  CALL int_get_ti_header( bigbuf(icurs/inttypesize), hdrbufsize, inttypesize, ftypesize, &
                                          DataHandle, Element, IData, Count, code )
                  icurs = icurs + hdrbufsize
                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( IData)
 
                CASE ( int_set_time )
                  CALL int_get_ti_header_char( bigbuf(icurs/inttypesize), hdrbufsize, inttypesize, &
                                               DataHandle, Element, VarName, CData, code )
                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_set_time ( handle(DataHandle), TRIM(CData), Status)
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  icurs = icurs + hdrbufsize

                CASE ( int_dom_ti_char )
                  CALL int_get_ti_header_char( bigbuf(icurs/inttypesize), hdrbufsize, inttypesize, &
                                               DataHandle, Element, VarName, CData, code )

                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  icurs = icurs + hdrbufsize

                CASE ( int_var_ti_char )
                  CALL int_get_ti_header_char( bigbuf(icurs/inttypesize), hdrbufsize, inttypesize, &
                                               DataHandle, Element, VarName, CData, code )

                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  icurs = icurs + hdrbufsize

                CASE ( int_ioexit )

                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_ioexit( Status )
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  CALL server_io_exit( Status )
                  CALL mpi_finalize(ierr)
                  CALL wrf_error_fatal ( 'exiting IO' )
                CASE ( int_ioclose )
                  CALL int_get_handle_header( bigbuf(icurs/inttypesize), hdrbufsize, itypesize, &
                                              DataHandle , code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_inquire_filename( handle(DataHandle), fname, fstat, Status )
                      IF ( fstat .EQ. WRF_FILE_OPENED_AND_COMMITTED .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                        CALL ext_ncd_ioclose(handle(DataHandle),Status)
                      ENDIF
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_inquire_filename( handle(DataHandle), fname, fstat, Status )
                      IF ( fstat .EQ. WRF_FILE_OPENED_AND_COMMITTED .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                        CALL ext_int_ioclose(handle(DataHandle),Status)
                      ENDIF
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                CASE ( int_open_for_write_begin )

                  CALL int_get_ofwb_header( bigbuf(icurs/inttypesize), hdrbufsize, itypesize, &
                                            FileName,SysDepInfo,io_form_arg,DataHandle )
                  icurs = icurs + hdrbufsize
                
                  io_form(DataHandle) = io_form_arg

                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT
                
                  okay_to_write(DataHandle) = .false.

                CASE ( int_open_for_write_commit )

                  CALL int_get_handle_header( bigbuf(icurs/inttypesize), hdrbufsize, itypesize, &
                                              DataHandle , code )
                  icurs = icurs + hdrbufsize
                  okay_to_commit(DataHandle) = .true.

                CASE ( int_field )
                  CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                  CALL int_get_write_field_header ( bigbuf(icurs/inttypesize), hdrbufsize, itypesize, ftypesize,  &
                                                    DataHandle , DateStr , VarName , Dummy , FieldType , Comm , IOComm, &
                                                    DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                                    DomainStart , DomainEnd ,                                    &
                                                    MemoryStart , MemoryEnd ,                                    &
                                                    PatchStart , PatchEnd )
                  icurs = icurs + hdrbufsize

                  IF ( okay_to_write(DataHandle) ) THEN

                    WRITE(*,*)'>>> ',TRIM(DateStr), ' ', TRIM(VarName), ' ', TRIM(MemoryOrder), ' ', &
                         (PatchEnd(1)-PatchStart(1)+1)*(PatchEnd(2)-PatchStart(2)+1)*(PatchEnd(3)-PatchStart(3)+1)

                    IF ( FieldType .EQ. WRF_REAL .OR. FieldType .EQ. WRF_DOUBLE)  THEN
                      CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                      stored_write_record = .true.
                      CALL store_patch_in_outbuf ( bigbuf(icurs/inttypesize), dummybuf, TRIM(DateStr), TRIM(VarName) , &
                                                   FieldType, TRIM(MemoryOrder), TRIM(Stagger), DimNames, &
                                                   DomainStart , DomainEnd , &
                                                   MemoryStart , MemoryEnd , &
                                                   PatchStart , PatchEnd )

                    ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
                      CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                      stored_write_record = .true.
                      CALL store_patch_in_outbuf ( dummybuf, bigbuf(icurs/inttypesize), TRIM(DateStr), TRIM(VarName) , &
                                                   FieldType, TRIM(MemoryOrder), TRIM(Stagger), DimNames, &
                                                   DomainStart , DomainEnd , &
                                                   MemoryStart , MemoryEnd , &
                                                   PatchStart , PatchEnd )
                    ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
                      ftypesize = LWORDSIZE
                    ENDIF
                    icurs = icurs + (PatchEnd(1)-PatchStart(1)+1)*(PatchEnd(2)-PatchStart(2)+1)* &
                                    (PatchEnd(3)-PatchStart(3)+1)*ftypesize
                  ELSE
                    SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                      CASE ( IO_NETCDF   )
                        CALL ext_ncd_write_field ( handle(DataHandle) , TRIM(DateStr) ,         &
                                   TRIM(VarName) , dummy , FieldType , Comm , IOComm,           &
                                   DomainDesc , TRIM(MemoryOrder) , TRIM(Stagger) , DimNames ,  &
                                   DomainStart , DomainEnd ,                                    &
                                   DomainStart , DomainEnd ,                                    &
                                   DomainStart , DomainEnd ,                                    &
                                   Status )
#endif
                      CASE DEFAULT
                        Status = 0
                    END SELECT
                  ENDIF
                CASE ( int_iosync )
                  CALL int_get_handle_header( bigbuf(icurs/inttypesize), hdrbufsize, itypesize, &
                                            DataHandle , code )
                  icurs = icurs + hdrbufsize
                CASE DEFAULT
                  WRITE(mess,*)'quilt: bad tag: ',get_hdr_tag( bigbuf(icurs/inttypesize) ),' icurs ',icurs/inttypesize
                  CALL wrf_error_fatal( mess )
              END SELECT

            ENDDO

            IF (stored_write_record) THEN
              CALL write_outbuf ( handle(DataHandle), use_package(io_form(DataHandle))) 
            ENDIF
            IF (okay_to_commit(DataHandle)) THEN

              SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                CASE ( IO_NETCDF   )
                  CALL ext_ncd_inquire_filename( handle(DataHandle), fname, fstat, Status )
                  IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                    CALL ext_ncd_open_for_write_commit(handle(DataHandle),Status)
                    okay_to_write(DataHandle) = .true.
                  ENDIF
#endif
#ifdef INTIO
                CASE ( IO_INTIO   )
                  CALL ext_int_inquire_filename( handle(DataHandle), fname, fstat, Status )
                  IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                    CALL ext_int_open_for_write_commit(handle(DataHandle),Status)
                    okay_to_write(DataHandle) = .true.
                  ENDIF
#endif
                CASE DEFAULT
                  Status = 0
              END SELECT

            okay_to_commit(DataHandle) = .false.
          ENDIF
          DEALLOCATE( bigbuf )
        ENDIF

        CALL retrieve_pieces_of_field ( obuf , VarName, obufsize, sz, retval )
        CALL MPI_Reduce( sz, bigbufsize, 1, MPI_INTEGER,  &
                         MPI_SUM, ntasks_local_group-1,         &
                         mpi_comm_local, ierr )
      END DO

      DEALLOCATE( obuf )

      END DO
#endif

    END SUBROUTINE quilt

! end of #endif of DM_PARALLEL
#endif

    !--- ioinit
    SUBROUTINE init_module_ext_quilt
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INTEGER i
      NAMELIST /namelist_quilt/ nio_tasks_per_group, nio_groups
      INTEGER ntasks, mytask, ierr
      LOGICAL mpi_inited

      quilting_enabled = .FALSE.
      IF ( disable_quilt ) RETURN

      DO i = 1,int_num_handles
        okay_to_write(i) = .FALSE.
        int_handle_in_use(i) = .FALSE.
        int_num_bytes_to_write(i) = 0
      ENDDO

      CALL MPI_INITIALIZED( mpi_inited, ierr )
      IF ( mpi_inited ) THEN
        CALL wrf_error_fatal("frame/module_io_quilt.F: quilt initialization must be called before MPI_Init") ;
      ENDIF

      CALL mpi_init ( ierr )
      CALL wrf_set_dm_communicator( MPI_COMM_WORLD )
      CALL wrf_termio_dup
      CALL MPI_Comm_rank ( MPI_COMM_WORLD, mytask, ierr ) ;
      CALL mpi_x_comm_size ( MPI_COMM_WORLD, ntasks, ierr ) ;

      IF ( mytask .EQ. 0 ) THEN
        OPEN ( unit=27, file="namelist.input", form="formatted", status="old" )
        nio_groups = 1
        nio_tasks_per_group  = 0
        READ ( 27 , namelist_quilt )
        CLOSE ( 27 )
      ENDIF
      CALL mpi_bcast( nio_tasks_per_group  , 1 , MPI_INTEGER , 0 , MPI_COMM_WORLD, ierr )
      CALL mpi_bcast( nio_groups , 1 , MPI_INTEGER , 0 , MPI_COMM_WORLD, ierr )

      CALL setup_quilt_servers( nio_tasks_per_group,            &
                                mytask,               &
                                ntasks,               &
                                nio_groups,           &
                                nio_tasks_in_group,   &
                                MPI_COMM_WORLD,       &
                                mpi_comm_local,       &
                                mpi_comm_io_groups)

       ! provide the communicator for the integration tasks to RSL
       IF ( mytask .lt. ncompute_tasks ) THEN
          CALL wrf_set_dm_communicator( mpi_comm_local )
       ELSE
          CALL quilt    ! will not return on io server tasks
       ENDIF
#endif
      RETURN
    END SUBROUTINE init_module_ext_quilt
END MODULE module_ext_quilt

! Call this in programs that you never want to be quilting (e.g. real)
! Must call before call to init_module_ext_quilt
!
SUBROUTINE disable_quilting
  USE module_ext_quilt
  disable_quilt = .TRUE.
  RETURN
END SUBROUTINE disable_quilting

LOGICAL FUNCTION  use_output_servers()
  USE module_ext_quilt
  use_output_servers = quilting_enabled
  RETURN
END FUNCTION use_output_servers

LOGICAL FUNCTION  use_input_servers()
  USE module_ext_quilt
  use_input_servers = .FALSE.
  RETURN
END FUNCTION use_input_servers

!--- open_for_write_begin
SUBROUTINE ext_quilt_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                     DataHandle , io_form_arg, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_ext_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER *(*), INTENT(IN)  :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(IN)  :: io_form_arg
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i, typesize, itypesize, tasks_in_group, ierr, comm_io_group
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  REAL dummy

  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_open_for_write_begin' ) 
  CALL int_get_fresh_handle(i)
  okay_to_write(i) = .false.
  DataHandle = i

  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
  IF ( wrf_dm_on_monitor() ) THEN
    CALL int_gen_ofwb_header( hdrbuf, hdrbufsize, itypesize, &
                            FileName,SysDepInfo,io_form_arg,DataHandle )
  ELSE
    CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
  ENDIF

  iserver = 1 ! only one server group for now
  CALL get_mpi_comm_io_groups( comm_io_group , iserver )
  CALL MPI_TYPE_SIZE( MPI_REAL, typesize, ierr )

  CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )

  ! send the size of my local buffer to the i/o task (obufsize doesnt mean anything here)
  CALL MPI_Reduce( hdrbufsize, obufsize, 1, MPI_INTEGER,  &
                   MPI_SUM, tasks_in_group-1,          &   ! root = nio_tasks_in_group-1 is me
                   comm_io_group, ierr )

  ! send data to the i/o processor
  CALL collect_on_comm( comm_io_group,            &
                        onebyte,                       &
                        hdrbuf, hdrbufsize , &
                        dummy, 0 )

  Status = 0


#endif
  RETURN  
END SUBROUTINE ext_quilt_open_for_write_begin

!--- open_for_write_commit
SUBROUTINE ext_quilt_open_for_write_commit( DataHandle , Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_ext_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i, typesize, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy

  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_open_for_write_commit' ) 
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      okay_to_write( DataHandle ) = .true.
    ENDIF
  ENDIF

  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
  CALL int_gen_handle_header( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, int_open_for_write_commit )

  iserver = 1 ! only one server group for now
  CALL get_mpi_comm_io_groups( comm_io_group , iserver )
  CALL MPI_TYPE_SIZE( MPI_REAL, typesize, ierr )

  CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )

  ! send the size of my local buffer to the i/o task (obufsize doesnt mean anything here)
  CALL MPI_Reduce( hdrbufsize, obufsize, 1, MPI_INTEGER,  &
                   MPI_SUM, tasks_in_group-1,          &   ! root = nio_tasks_in_group-1 is me
                   comm_io_group, ierr )

  ! send data to the i/o processor
  CALL collect_on_comm( comm_io_group,            &
                        onebyte,                       &
                        hdrbuf, hdrbufsize , &
                        dummy, 0 )

  Status = 0

#endif
  RETURN  
END SUBROUTINE ext_quilt_open_for_write_commit

!--- open_for_read 
SUBROUTINE ext_quilt_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER *(*), INTENT(IN)  :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_open_for_read' ) 
  DataHandle = -1
  Status = -1
  CALL wrf_error_fatal ( "frame/module_io_quilt.F: ext_quilt_open_for_read not yet supported" )
#endif
  RETURN  
END SUBROUTINE ext_quilt_open_for_read

!--- intio_nextrec  (INT_IO only)
SUBROUTINE ext_quilt_intio_nextrec ( DataHandle , NextRec , Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER , INTENT(IN)  :: DataHandle
  INTEGER               :: NextRec
  INTEGER               :: Status
#endif
  RETURN  
END SUBROUTINE ext_quilt_intio_nextrec

!--- inquire_opened
SUBROUTINE ext_quilt_inquire_opened ( DataHandle, FileName , FileStatus, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_ext_quilt
  IMPLICIT NONE
  include 'wrf_io_flags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status

  Status = 0

  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_inquire_opened' ) 
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      IF ( okay_to_write( DataHandle ) ) THEN
        FileStatus = WRF_FILE_OPENED_AND_COMMITTED
      ENDIF
    ENDIF
  ENDIF
  Status = 0
  
#endif
  RETURN
END SUBROUTINE ext_quilt_inquire_opened

!--- inquire_filename
SUBROUTINE ext_quilt_inquire_filename ( DataHandle, FileName , FileStatus, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_ext_quilt
  IMPLICIT NONE
  include 'wrf_io_flags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER *(*), INTENT(OUT) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_inquire_filename' ) 
  Status = 0
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      IF ( okay_to_write( DataHandle ) ) THEN
        FileStatus = WRF_FILE_OPENED_AND_COMMITTED
      ELSE
        FileStatus = WRF_FILE_OPENED_NOT_COMMITTED
      ENDIF
    ELSE
        FileStatus = WRF_FILE_NOT_OPENED
    ENDIF
    Status = 0
    FileName = "bogusfornow"
  ELSE
    Status = -1
  ENDIF
#endif
  RETURN
END SUBROUTINE ext_quilt_inquire_filename

!--- sync
SUBROUTINE ext_quilt_iosync ( DataHandle, Status )
#if  defined( DM_PARALLEL ) && ! defined (STUBMPI) 
  USE module_ext_quilt
  IMPLICIT NONE
  include "mpif.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  INTEGER locsize , typesize, inttypesize
  INTEGER ierr, tasks_in_group, comm_io_group, dummy, i

  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_iosync' ) 

  IF ( associated ( int_local_output_buffer ) ) THEN

    iserver = 1 ! only one server group for now
    CALL get_mpi_comm_io_groups( comm_io_group , iserver )
    CALL MPI_TYPE_SIZE( MPI_REAL, typesize, ierr )
    IF ( typesize <= 0 ) THEN
      CALL wrf_error_fatal("frame/module_io_quilt.F: ext_quilt_iosync : type size <= 0 invalid")
    ENDIF

    CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )

    locsize = int_num_bytes_to_write(DataHandle)

    ! send the size of my local buffer to the i/o task (obufsize doesnt mean anything here)
    CALL MPI_Reduce( locsize, obufsize, 1, MPI_INTEGER,  &
                     MPI_SUM, tasks_in_group-1,          &   ! root = nio_tasks_in_group-1 is me
                     comm_io_group, ierr )


    ! send data to the i/o processor
    CALL collect_on_comm( comm_io_group,            &
                          onebyte,                       &
                          int_local_output_buffer, locsize , &
                          dummy, 0 )


    int_local_output_cursor = 1
!    int_num_bytes_to_write(DataHandle) = 0
    DEALLOCATE ( int_local_output_buffer )
    NULLIFY ( int_local_output_buffer )
  ELSE
    CALL wrf_message ("frame/module_io_quilt.F: ext_quilt_iosync: no buffer allocated")
  ENDIF
  Status = 0
#endif
  RETURN
END SUBROUTINE ext_quilt_iosync

!--- close
SUBROUTINE ext_quilt_ioclose ( DataHandle, Status )
#if defined( DM_PARALLEL ) && ! defined( STUBMPI) 
  USE module_ext_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i, typesize, itypesize, tasks_in_group, comm_io_group, ierr
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  REAL dummy

  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_ioclose' ) 
  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )

  IF ( wrf_dm_on_monitor() ) THEN
    CALL int_gen_handle_header( hdrbuf, hdrbufsize, itypesize, &
                                DataHandle , int_ioclose )
  ELSE
    CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
  ENDIF

  iserver = 1 ! only one server group for now
  CALL get_mpi_comm_io_groups( comm_io_group , iserver )
  CALL MPI_TYPE_SIZE( MPI_REAL, typesize, ierr )

  CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )

  ! send the size of my local buffer to the i/o task (obufsize doesnt mean anything here)
  CALL MPI_Reduce( hdrbufsize, obufsize, 1, MPI_INTEGER,  &
                   MPI_SUM, tasks_in_group-1,          &   ! root = nio_tasks_in_group-1 is me
                   comm_io_group, ierr )

  ! send data to the i/o processor
  CALL collect_on_comm( comm_io_group,            &
                        onebyte,                       &
                        hdrbuf, hdrbufsize , &
                        dummy, 0 )

  int_handle_in_use(DataHandle) = .false.
  okay_to_write(DataHandle) = .false.
  okay_to_commit(DataHandle) = .false.
  int_local_output_cursor = 1
  int_num_bytes_to_write(DataHandle) = 0
  IF ( associated ( int_local_output_buffer ) ) THEN
    DEALLOCATE ( int_local_output_buffer )
    NULLIFY ( int_local_output_buffer )
  ENDIF

  Status = 0

#endif
  RETURN
END SUBROUTINE ext_quilt_ioclose

!--- ioexit
SUBROUTINE ext_quilt_ioexit( Status )
#if defined( DM_PARALLEL ) && ! defined (STUBMPI ) 
  USE module_ext_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER                     :: DataHandle
  INTEGER i, typesize, itypesize, tasks_in_group, comm_io_group, ierr
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  REAL dummy

  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_ioexit' ) 
  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )

  IF ( wrf_dm_on_monitor() ) THEN
    CALL int_gen_handle_header( hdrbuf, hdrbufsize, itypesize, &
                                DataHandle , int_ioexit )  ! Handle is dummy
  ELSE
    CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
  ENDIF

  iserver = 1 ! only one server group for now
  CALL get_mpi_comm_io_groups( comm_io_group , iserver )
  CALL MPI_TYPE_SIZE( MPI_REAL, typesize, ierr )

  CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )

! BY SENDING A NEGATIVE SIZE WE GET THE SERVERS TO SHUT DOWN

  hdrbufsize = -100 
  CALL MPI_Reduce( hdrbufsize, obufsize, 1, MPI_INTEGER,  &
                   MPI_SUM, tasks_in_group-1,          &   ! root = nio_tasks_in_group-1 is me
                   comm_io_group, ierr )
  Status = 0

#endif
  RETURN  
END SUBROUTINE

SUBROUTINE server_io_exit( Status )
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
  USE module_ext_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
!
  INTEGER, INTENT(INOUT) :: Status
  INTEGER irank, isize, ierr, i, itag

  CALL wrf_debug ( DEBUG_LVL, 'in server_io_exit' ) 
  CALL mpi_comm_rank ( mpi_comm_local, irank, ierr )
  CALL mpi_x_comm_size ( mpi_comm_local, isize, ierr )
!
  ! send out a message to all the I/O servers that we're shutting down
  ! otherwise, they will just spin and never call finalize.
  itag = 202020
  IF ( irank .EQ. 0 ) THEN
    DO i = 1, isize-1
      CALL mpi_send( itag, 1, MPI_INTEGER, i, itag, mpi_comm_local, ierr )
    ENDDO
  ENDIF
#endif
  RETURN  
END SUBROUTINE

!--- get_next_time (not defined for IntIO )
SUBROUTINE ext_quilt_get_next_time ( DataHandle, DateStr, Status )
#if defined( DM_PARALLEL ) && ! defined (STUBMPI) 
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*)               :: DateStr
  INTEGER                     :: Status
#endif
  RETURN
END SUBROUTINE ext_quilt_get_next_time

!--- get_previous_time (not defined for IntIO )
SUBROUTINE ext_quilt_get_previous_time ( DataHandle, DateStr, Status )
#if defined( DM_PARALLEL ) && ! defined (STUBMPI)
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*)               :: DateStr
  INTEGER                     :: Status
#endif
  RETURN
END SUBROUTINE ext_quilt_get_previous_time

!--- put_dom_ti_char
SUBROUTINE ext_quilt_set_time ( DataHandle, Data,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_ext_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Data
  INTEGER                     :: Status
  INTEGER i, typesize, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
  INTEGER                 :: Count
!
  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_set_time' )

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      Count = 0   ! there is no count for character strings
      CALL int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, "TIMESTAMP", "", Data, int_set_time )
      iserver = 1 ! only one server group for now
      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )
      ! send the size of my local buffer to the i/o task (obufsize doesnt mean anything here)
      CALL MPI_Reduce( hdrbufsize, obufsize, 1, MPI_INTEGER,  &
                       MPI_SUM, tasks_in_group-1,          &   ! root = nio_tasks_in_group-1 is me
                       comm_io_group, ierr )
      ! send data to the i/o processor
      CALL collect_on_comm( comm_io_group,            &
                            onebyte,                       &
                            hdrbuf, hdrbufsize , &
                            dummy, 0 )
    ENDIF
  ENDIF

#endif
RETURN
END SUBROUTINE ext_quilt_set_time

!--- get_next_var  (not defined for IntIO)
SUBROUTINE ext_quilt_get_next_var ( DataHandle, VarName, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*)               :: VarName
  INTEGER                     :: Status
#endif
  RETURN
END SUBROUTINE ext_quilt_get_next_var

!--- get_dom_ti_real
SUBROUTINE ext_quilt_get_dom_ti_real ( DataHandle,Element,   Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  REAL,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Outcount
  INTEGER                     :: Status
  CALL wrf_message('ext_quilt_get_dom_ti_real not supported yet')
#endif
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_real 

!--- put_dom_ti_real
SUBROUTINE ext_quilt_put_dom_ti_real ( DataHandle,Element,   Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_ext_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
  INTEGER i, typesize, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
!
  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_put_dom_ti_real' ) 
  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      CALL MPI_TYPE_SIZE( MPI_REAL, typesize, ierr )
      CALL int_gen_ti_header( hdrbuf, hdrbufsize, itypesize, typesize, &
                              DataHandle, Element, Data, Count, int_dom_ti_real )
      iserver = 1 ! only one server group for now
      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )
      ! send the size of my local buffer to the i/o task (obufsize doesnt mean anything here)
      CALL MPI_Reduce( hdrbufsize, obufsize, 1, MPI_INTEGER,  &
                       MPI_SUM, tasks_in_group-1,          &   ! root = nio_tasks_in_group-1 is me
                       comm_io_group, ierr )
      ! send data to the i/o processor
      CALL collect_on_comm( comm_io_group,            &
                            onebyte,                       &
                            hdrbuf, hdrbufsize , &
                            dummy, 0 )
    ENDIF
  ENDIF

  Status = 0
#endif
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_real 

!--- get_dom_ti_double
SUBROUTINE ext_quilt_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8                      :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
  CALL wrf_message('ext_quilt_get_dom_ti_double not supported yet')
#endif
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_double 

!--- put_dom_ti_double
SUBROUTINE ext_quilt_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
  CALL wrf_message('ext_quilt_put_dom_ti_double not supported yet')
#endif
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_double 

!--- get_dom_ti_integer
SUBROUTINE ext_quilt_get_dom_ti_integer ( DataHandle,Element,   Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  integer                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
  CALL wrf_message('ext_quilt_get_dom_ti_integer not supported yet')
#endif
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_integer 

!--- put_dom_ti_integer
SUBROUTINE ext_quilt_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_ext_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  INTEGER ,       INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
  INTEGER i, typesize, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
!

  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_put_dom_ti_integer' ) 

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      CALL MPI_TYPE_SIZE( MPI_INTEGER, typesize, ierr )
      CALL int_gen_ti_header( hdrbuf, hdrbufsize, itypesize, typesize, &
                              DataHandle, Element, Data, Count, int_dom_ti_integer )
      iserver = 1 ! only one server group for now
      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )
      ! send the size of my local buffer to the i/o task (obufsize doesnt mean anything here)
      CALL MPI_Reduce( hdrbufsize, obufsize, 1, MPI_INTEGER,  &
                       MPI_SUM, tasks_in_group-1,          &   ! root = nio_tasks_in_group-1 is me
                       comm_io_group, ierr )
      ! send data to the i/o processor
      CALL collect_on_comm( comm_io_group,            &
                            onebyte,                       &
                            hdrbuf, hdrbufsize , &
                            dummy, 0 )
    ENDIF
  ENDIF
  CALL wrf_debug ( DEBUG_LVL, 'returning from ext_quilt_put_dom_ti_integer' ) 

#endif
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_integer 

!--- get_dom_ti_logical
SUBROUTINE ext_quilt_get_dom_ti_logical ( DataHandle,Element,   Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  logical                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
  CALL wrf_message('ext_quilt_get_dom_ti_logical not supported yet')
#endif
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_logical 

!--- put_dom_ti_logical
SUBROUTINE ext_quilt_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
  CALL wrf_message('ext_quilt_put_dom_ti_logical not supported yet')
#endif
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_logical 

!--- get_dom_ti_char
SUBROUTINE ext_quilt_get_dom_ti_char ( DataHandle,Element,   Data,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*)               :: Data
  INTEGER                     :: Status
  CALL wrf_message('ext_quilt_get_dom_ti_char not supported yet')
#endif
RETURN
END SUBROUTINE ext_quilt_get_dom_ti_char 

!--- put_dom_ti_char
SUBROUTINE ext_quilt_put_dom_ti_char ( DataHandle, Element,  Data,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_ext_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: Data
  INTEGER                     :: Status
  INTEGER i, typesize, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
  INTEGER                 :: Count
!
  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_put_dom_ti_char' ) 

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      Count = 0   ! there is no count for character strings
      CALL int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, Element, "", Data, int_dom_ti_char )
      iserver = 1 ! only one server group for now
      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )
      ! send the size of my local buffer to the i/o task (obufsize doesnt mean anything here)
      CALL MPI_Reduce( hdrbufsize, obufsize, 1, MPI_INTEGER,  &
                       MPI_SUM, tasks_in_group-1,          &   ! root = nio_tasks_in_group-1 is me
                       comm_io_group, ierr )
      ! send data to the i/o processor
      CALL collect_on_comm( comm_io_group,            &
                            onebyte,                       &
                            hdrbuf, hdrbufsize , &
                            dummy, 0 )
    ENDIF
  ENDIF

#endif
RETURN
END SUBROUTINE ext_quilt_put_dom_ti_char 

!--- get_dom_td_real
SUBROUTINE ext_quilt_get_dom_td_real ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real                        :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_dom_td_real 

!--- put_dom_td_real
SUBROUTINE ext_quilt_put_dom_td_real ( DataHandle,Element, DateStr,  Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_dom_td_real 

!--- get_dom_td_double
SUBROUTINE ext_quilt_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real*8                          :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_dom_td_double 

!--- put_dom_td_double
SUBROUTINE ext_quilt_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_dom_td_double 

!--- get_dom_td_integer
SUBROUTINE ext_quilt_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  integer                          :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_dom_td_integer 

!--- put_dom_td_integer
SUBROUTINE ext_quilt_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_dom_td_integer 

!--- get_dom_td_logical
SUBROUTINE ext_quilt_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  logical                          :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_dom_td_logical 

!--- put_dom_td_logical
SUBROUTINE ext_quilt_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_dom_td_logical 

!--- get_dom_td_char
SUBROUTINE ext_quilt_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*)               :: Data
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_dom_td_char 

!--- put_dom_td_char
SUBROUTINE ext_quilt_put_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN) :: Data
  INTEGER                          :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_dom_td_char 

!--- get_var_ti_real
SUBROUTINE ext_quilt_get_var_ti_real ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real                          :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_var_ti_real 

!--- put_var_ti_real
SUBROUTINE ext_quilt_put_var_ti_real ( DataHandle,Element,  Varname, Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_var_ti_real 

!--- get_var_ti_double
SUBROUTINE ext_quilt_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8                      :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_var_ti_double 

!--- put_var_ti_double
SUBROUTINE ext_quilt_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8 ,        INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_var_ti_double 

!--- get_var_ti_integer
SUBROUTINE ext_quilt_get_var_ti_integer ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_var_ti_integer 

!--- put_var_ti_integer
SUBROUTINE ext_quilt_put_var_ti_integer ( DataHandle,Element,  Varname, Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_var_ti_integer 

!--- get_var_ti_logical
SUBROUTINE ext_quilt_get_var_ti_logical ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_var_ti_logical 

!--- put_var_ti_logical
SUBROUTINE ext_quilt_put_var_ti_logical ( DataHandle,Element,  Varname, Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_var_ti_logical 

!--- get_var_ti_char
SUBROUTINE ext_quilt_get_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*)               :: Data
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_var_ti_char 

!--- put_var_ti_char
SUBROUTINE ext_quilt_put_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )

#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_ext_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*) , INTENT(IN)  :: Data
  INTEGER                     :: Status
  INTEGER i, typesize, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
  INTEGER                 :: Count
!

  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_put_var_ti_char' ) 

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      Count = 0   ! there is no count for character strings
      CALL int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, TRIM(Element), TRIM(VarName), TRIM(Data), int_var_ti_char )
      iserver = 1 ! only one server group for now
      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )
      ! send the size of my local buffer to the i/o task (obufsize doesnt mean anything here)
      CALL MPI_Reduce( hdrbufsize, obufsize, 1, MPI_INTEGER,  &
                       MPI_SUM, tasks_in_group-1,          &   ! root = nio_tasks_in_group-1 is me
                       comm_io_group, ierr )
      ! send data to the i/o processor
      CALL collect_on_comm( comm_io_group,            &
                            onebyte,                       &
                            hdrbuf, hdrbufsize , &
                            dummy, 0 )
    ENDIF
  ENDIF

#endif
RETURN
END SUBROUTINE ext_quilt_put_var_ti_char 

!--- get_var_td_real
SUBROUTINE ext_quilt_get_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real                        :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_var_td_real 

!--- put_var_td_real
SUBROUTINE ext_quilt_put_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_var_td_real 

!--- get_var_td_double
SUBROUTINE ext_quilt_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8                      :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_var_td_double 

!--- put_var_td_double
SUBROUTINE ext_quilt_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_var_td_double 

!--- get_var_td_integer
SUBROUTINE ext_quilt_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount,Status)
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_var_td_integer 

!--- put_var_td_integer
SUBROUTINE ext_quilt_put_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer ,       INTENT(IN)  :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_var_td_integer 

!--- get_var_td_logical
SUBROUTINE ext_quilt_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical                          :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_var_td_logical 

!--- put_var_td_logical
SUBROUTINE ext_quilt_put_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_var_td_logical 

!--- get_var_td_char
SUBROUTINE ext_quilt_get_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*)               :: Data
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_var_td_char 

!--- put_var_td_char
SUBROUTINE ext_quilt_put_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*) , INTENT(IN) :: Data
  INTEGER                    :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_put_var_td_char 

!--- read_field
SUBROUTINE ext_quilt_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) , INTENT(INOUT) :: DateStr
  CHARACTER*(*) , INTENT(INOUT) :: VarName
  INTEGER ,       INTENT(INOUT) :: Field(*)
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
  Status = 0
#endif
RETURN
END SUBROUTINE ext_quilt_read_field

!--- write_field
SUBROUTINE ext_quilt_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_state_description
  USE module_ext_quilt
  IMPLICIT NONE
  include 'mpif.h'
  include 'wrf_io_flags.h'
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) , INTENT(IN)    :: DateStr
  CHARACTER*(*) , INTENT(IN)    :: VarName
!  INTEGER ,       INTENT(IN)    :: Field(*)
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

  integer ii,jj,kk,myrank

  REAL, DIMENSION( MemoryStart(1):MemoryEnd(1), &
                   MemoryStart(2):MemoryEnd(2), &
                   MemoryStart(3):MemoryEnd(3) ) :: Field
  INTEGER locsize , typesize, inttypesize, realtypesize
  INTEGER ierr, tasks_in_group, comm_io_group, dummy, i
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  INTEGER, EXTERNAL :: use_package

  CALL wrf_debug ( DEBUG_LVL, 'in ext_quilt_write_field' ) 

  IF ( .NOT. (DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles) ) THEN
    CALL wrf_error_fatal("frame/module_io_quilt.F: ext_quilt_write_field: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("frame/module_io_quilt.F: ext_quilt_write_field: DataHandle not opened" )
  ENDIF

  locsize = (PatchEnd(1)-PatchStart(1)+1)* &
            (PatchEnd(2)-PatchStart(2)+1)* &
            (PatchEnd(3)-PatchStart(3)+1)

  CALL mpi_type_size( MPI_INTEGER, inttypesize, ierr )
  CALL mpi_type_size( MPI_REAL, realtypesize, ierr )
  IF      ( FieldType .EQ. WRF_REAL ) THEN
    CALL mpi_type_size( MPI_REAL, typesize, ierr )
  ELSE IF ( FieldType .EQ. WRF_DOUBLE ) THEN
    CALL mpi_type_size( MPI_DOUBLE_PRECISION, typesize, ierr )
  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
    CALL mpi_type_size( MPI_INTEGER, typesize, ierr )
  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    CALL mpi_type_size( MPI_LOGICAL, typesize, ierr )
  ENDIF

  IF ( .NOT. okay_to_write( DataHandle ) ) THEN

    IF ( use_package(io_form(handle(DataHandle))) .NE. IO_INTIO ) THEN
      ! it is not okay to actually write; what we do here is just "bookkeep": count up
      ! the number and size of messages that we will output to io server associated with
      ! this task

      CALL int_gen_write_field_header ( hdrbuf, hdrbufsize, inttypesize, typesize,           &
                               DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                               333933         , MemoryOrder , Stagger , DimNames ,              &   ! 333933 means training; magic number
                               DomainStart , DomainEnd ,                                    &
                               MemoryStart , MemoryEnd ,                                    &
                               PatchStart , PatchEnd )

      int_num_bytes_to_write(DataHandle) = int_num_bytes_to_write(DataHandle) + locsize * typesize + hdrbufsize

      ! Send the hdr for the write in case the interface is calling the I/O API in "learn" mode

      iserver = 1
      CALL get_mpi_comm_io_groups( comm_io_group , iserver )
      ! send the size of my local buffer to the i/o task (obufsize doesnt mean anything here)

      CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )

      IF ( .NOT. wrf_dm_on_monitor() ) THEN     ! only one task in compute grid sends this message; send noops on others
        CALL int_gen_noop_header( hdrbuf, hdrbufsize, inttypesize )
      ENDIF

      CALL MPI_Reduce( hdrbufsize, obufsize, 1, MPI_INTEGER,  &
                       MPI_SUM, tasks_in_group-1,             &   ! root = nio_tasks_in_group-1 is me
                       comm_io_group, ierr )
      ! send data to the i/o processor

      CALL collect_on_comm( comm_io_group,                   &
                            onebyte,                          &
                            hdrbuf, hdrbufsize ,                 &
                            dummy, 0 )
    ENDIF

  ELSE

    IF ( .NOT. associated( int_local_output_buffer ) ) THEN
      ALLOCATE ( int_local_output_buffer( (int_num_bytes_to_write( DataHandle )+1)/inttypesize ) )
      int_local_output_cursor = 1
    ENDIF

    CALL int_gen_write_field_header ( hdrbuf, hdrbufsize, inttypesize, typesize,           &
                             DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             0          , MemoryOrder , Stagger , DimNames ,              &   ! non-333933 means okay to write; magic number
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd )

    CALL int_pack_data ( hdrbuf , hdrbufsize , int_local_output_buffer, int_local_output_cursor )

    CALL int_pack_data ( Field(PatchStart(1):PatchEnd(1),PatchStart(2):PatchEnd(2),PatchStart(3):PatchEnd(3) ), &
                                  locsize * typesize , int_local_output_buffer, int_local_output_cursor )

  ENDIF
  Status = 0

#endif
  RETURN
END SUBROUTINE ext_quilt_write_field

!--- get_var_info  (not implemented for IntIO)
SUBROUTINE ext_quilt_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , Status )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: VarName
  integer                               :: NDim
  character*(*)                         :: MemoryOrder
  character*(*)                         :: Stagger
  integer ,dimension(*)                 :: DomainStart, DomainEnd
  integer                               :: Status
#endif
RETURN
END SUBROUTINE ext_quilt_get_var_info

SUBROUTINE get_mpi_comm_io_groups( retval, i )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
      USE module_ext_quilt
      IMPLICIT NONE
      INTEGER, INTENT(IN ) :: i
      INTEGER, INTENT(OUT) :: retval
      retval = mpi_comm_io_groups(i)
#endif
      RETURN
END SUBROUTINE get_mpi_comm_io_groups

SUBROUTINE get_nio_tasks_in_group( retval )
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
      USE module_ext_quilt
      IMPLICIT NONE
      INTEGER, INTENT(OUT) :: retval
      retval = nio_tasks_in_group
#endif
      RETURN
END SUBROUTINE get_nio_tasks_in_group


