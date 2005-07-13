!WRF:DRIVER_LAYER:IO
!
#define DEBUG_LVL 50
!#define mpi_x_comm_size(i,j,k)  Mpi_Comm_Size ( i,j,k ) ; write(0,*) __LINE__
#define mpi_x_comm_size(i,j,k)  Mpi_Comm_Size ( i,j,k )

MODULE module_wrf_quilt
!<DESCRIPTION>
!<PRE>
! This module contains WRF-specific I/O quilt routines called by both 
! client (compute) and server (I/O quilt) tasks.  I/O quilt servers are 
! a run-time optimization that allow I/O operations, executed on the I/O 
! quilt server tasks, to be overlapped with useful computation, executed on 
! the compute tasks.  Since I/O operations are often quite slow compared to 
! computation, this performance optimization can increase parallel 
! efficiency.  
!
! Currently, one group of I/O servers can be specified at run-time.  Namelist 
! variable "nio_tasks_per_group" is used to specify the number of I/O server 
! tasks in this group.  In most cases, parallel efficiency is optimized when 
! the minimum number of I/O server tasks are used.  If memory needed to cache 
! I/O operations fits on a single processor, then set nio_tasks_per_group=1.  
! If not, increase the number of I/O server tasks until I/O operations fit in 
! memory.  In the future, multiple groups of I/O server tasks will be 
! supported.  The number of groups will be specified by namelist variable 
! "nio_groups".  For now, nio_groups must be set to 1.  Currently, I/O servers 
! only support overlap of output operations with computation.  Also, only I/O 
! packages that do no support native parallel I/O may be used with I/O server 
! tasks.  This excludes PHDF5 and MCEL.  
!
! In this module, the I/O quilt server tasks call package-dependent 
! WRF-specific I/O interfaces to perform I/O operations requested by the 
! client (compute) tasks.  All of these calls occur inside subroutine 
! quilt().  
! 
! The client (compute) tasks call package-independent WRF-specific "quilt I/O" 
! interfaces that send requests to the I/O quilt servers.  All of these calls 
! are made from module_io.F.  
!
! These routines have the same names and (roughly) the same arguments as those 
! specified in the WRF I/O API except that:
! - "Quilt I/O" routines defined in this file and called by routines in 
!   module_io.F have the "wrf_quilt_" prefix.
! - Package-dependent routines called from routines in this file are defined 
!   in the external I/O packages and have the "ext_" prefix.
!
! Both client (compute) and server tasks call routine init_module_wrf_quilt() 
! which then calls setup_quilt_servers() determine which tasks are compute 
! tasks and which are server tasks.  Before the end of init_module_wrf_quilt() 
! server tasks call routine quilt() and remain there for the rest of the model 
! run.  Compute tasks return from init_module_wrf_quilt() to perform model 
! computations.  
!
! See http://www.mmm.ucar.edu/wrf/WG2/software_2.0/IOAPI.doc for the latest
! version of the WRF I/O API.  This document includes detailed descriptions
! of subroutines and their arguments that are not duplicated here.
!</PRE>
!</DESCRIPTION>
  USE module_internal_header_util

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
!<DESCRIPTION>
!<PRE>
! Find an unused "client file handle" and return it in retval.
! The "client file handle" is used to remember how a file was opened
! so clients do not need to ask the I/O quilt servers for this information.
! It is also used as a file identifier in communications with the I/O
! server task.
!
! Note that client tasks know nothing about package-specific handles.
! Only the I/O quilt servers know about them.
!</PRE>
!</DESCRIPTION>
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
!<DESCRIPTION>
!<PRE>
! Both client (compute) and server tasks call this routine to 
! determine which tasks are compute tasks and which are I/O server tasks.  
!
! Module variables MPI_COMM_LOCAL and MPI_COMM_IO_GROUPS(:) are set up to 
! contain MPI communicators as follows:  
!
! MPI_COMM_LOCAL is the communicator for the local groups of tasks. For the 
! compute tasks it is the group of compute tasks; for a server group it the 
! communicator of tasks in the server group.
!
! Elements of MPI_COMM_IO_GROUPS are communicators that each contain one or 
! more compute tasks and a single I/O server assigned to those compute tasks.  
! The I/O server tasks is always the last task in these communicators.  
! On a compute task, which has a single associate in each of the server 
! groups, MPI_COMM_IO_GROUPS is treated as an array; each element corresponds 
! to a different server group. 
! On a server task only the first element of MPI_COMM_IO_GROUPS is used 
! because each server task is part of only one io_group.  
!
! I/O server tasks in each I/O server group are divided among compute tasks as 
! evenly as possible.  
!
! When multiple I/O server groups are used, each must have the same number of 
! tasks.  When the total number of extra I/O tasks does not divide evenly by 
! the number of io server groups requested, the remainder tasks are not used 
! (wasted).  
!
! For example, communicator membership for 18 tasks with nio_groups=2 and 
! nio_tasks_per_group=3 is shown below:  
!
! Membership for MPI_COMM_LOCAL communicators:
!   COMPUTE TASKS:          0   1   2   3   4   5   6   7   8   9  10  11
!   1ST I/O SERVER GROUP:  12  13  14
!   2ND I/O SERVER GROUP:  15  16  17
!
! Membership for MPI_COMM_IO_GROUPS(1):  
!   COMPUTE TASKS 0, 3, 6, 9:   0   3   6   9  12
!   COMPUTE TASKS 1, 4, 7,10:   1   4   7  10  13
!   COMPUTE TASKS 2, 5, 8,11:   2   5   8  11  14
!   I/O SERVER TASK       12:   0   3   6   9  12
!   I/O SERVER TASK       13:   1   4   7  10  13
!   I/O SERVER TASK       14:   2   5   8  11  14
!   I/O SERVER TASK       15:   0   3   6   9  15
!   I/O SERVER TASK       16:   1   4   7  10  16
!   I/O SERVER TASK       17:   2   5   8  11  17
!
! Membership for MPI_COMM_IO_GROUPS(2):  
!   COMPUTE TASKS 0, 3, 6, 9:   0   3   6   9  15
!   COMPUTE TASKS 1, 4, 7,10:   1   4   7  10  16
!   COMPUTE TASKS 2, 5, 8,11:   2   5   8  11  17
!   I/O SERVER TASK       12:  ** not used **
!   I/O SERVER TASK       13:  ** not used **
!   I/O SERVER TASK       14:  ** not used **
!   I/O SERVER TASK       15:  ** not used **
!   I/O SERVER TASK       16:  ** not used **
!   I/O SERVER TASK       17:  ** not used **
!
!</PRE>
!</DESCRIPTION>
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INTEGER,                      INTENT(IN)  :: nio_tasks_per_group, mytask, ntasks, &
                                                   n_groups_arg, mpi_comm_wrld
      INTEGER,  INTENT(OUT)                     :: mpi_comm_local, nio
      INTEGER, DIMENSION(100),      INTENT(OUT) :: mpi_comm_io_groups
! Local
      INTEGER                     :: i, j, ii, comdup, ierr, niotasks, n_groups
      INTEGER, DIMENSION(ntasks)  :: icolor
      CHARACTER*128 mess

      n_groups = n_groups_arg
      IF ( n_groups .LT. 1 ) n_groups = 1

!<DESCRIPTION>
!<PRE>
! nio is number of io tasks per group.  If there arent enough tasks to satisfy
! the requirement that there be at least as many compute tasks as io tasks in
! each group, then just print a warning and dump out of quilting
!</PRE>
!</DESCRIPTION>

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
        ! TBH:  each I/O group will contain only one I/O server
        DO i = ncompute_tasks+1,ntasks
          icolor(i) = MPI_UNDEFINED
        ENDDO
        ii = 0
        DO i = ncompute_tasks+(j-1)*nio+1,ncompute_tasks+j*nio
          icolor(i) = ii
          ii = ii+1
        ENDDO
        CALL MPI_Comm_dup(mpi_comm_wrld,comdup,ierr)
        CALL MPI_Comm_split(comdup,icolor(mytask+1),mytask,mpi_comm_io_groups(j),ierr)
      ENDDO
! If I am an I/O server, figure out which group I'm in and make that group's
! communicator the first element in the mpi_comm_io_groups array (I will ignore
! all of the other elements).
      IF ( mytask+1 .GT. ncompute_tasks ) THEN
        niotasks = ntasks - ncompute_tasks
        i = mytask - ncompute_tasks
        j = i / nio + 1
        mpi_comm_io_groups(1) = mpi_comm_io_groups(j)
      ENDIF

    END SUBROUTINE setup_quilt_servers

    SUBROUTINE quilt
!<DESCRIPTION>
!<PRE>
! I/O server tasks call this routine and remain in it for the rest of the 
! model run.  I/O servers receive I/O requests from compute tasks and 
! perform requested I/O operations by calling package-dependent WRF-specific 
! I/O interfaces.  Requests are sent in the form of "data headers".  Each 
! request has a unique "header" message associated with it.  For requests that 
! contain large amounts of data, the data is appended to the header.  See 
! file module_internal_header_util.F for detailed descriptions of all 
! headers.  
!
! We wish to be able to link to different packages depending on whether
! the I/O is restart, initial, history, or boundary.
!</PRE>
!</DESCRIPTION>
      USE module_state_description
      USE module_quilt_outbuf_ops
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INCLUDE 'intio_tags.h'
      INCLUDE 'wrf_io_flags.h'
      INTEGER itag, ninbuf, ntasks_io_group, ntasks_local_group, mytask_local, ierr
      INTEGER istat
      INTEGER mytask_io_group
      INTEGER   :: nout_set = 0
      INTEGER   :: obufsize, bigbufsize, inttypesize, chunksize, sz
      REAL, DIMENSION(1)      :: dummy
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
      LOGICAL           :: stored_write_record, retval
      INTEGER iii, jjj, vid

!

!<DESCRIPTION>
!<PRE>
! Call ext_pkg_ioinit() routines to initialize I/O packages.  
!</PRE>
!</DESCRIPTION>
      SysDepInfo = " "
#ifdef NETCDF
      CALL ext_ncd_ioinit( SysDepInfo, ierr)
#endif
#ifdef INTIO
      CALL ext_int_ioinit( SysDepInfo, ierr )
#endif
#ifdef XXX
      CALL ext_xxx_ioinit( SysDepInfo, ierr)
#endif
#ifdef YYY
      CALL ext_yyy_ioinit( SysDepInfo, ierr)
#endif
#ifdef ZZZ
      CALL ext_zzz_ioinit( SysDepInfo, ierr)
#endif
#ifdef GRIB1
      CALL ext_gr1_ioinit( SysDepInfo, ierr)
#endif
#ifdef GRIB2
      CALL ext_gr2_ioinit( SysDepInfo, ierr)
#endif

      okay_to_commit = .false.
      stored_write_record = .false.
      ninbuf = 0
      ! get info. about the I/O server group that this I/O server task
      ! belongs to
      ! Last task in this I/O server group is the I/O server "root"
      ! The I/O server "root" actually writes data to disk
      ! TBH:  WARNING:  This is also implicit in the call to collect_on_comm().
      CALL mpi_x_comm_size( mpi_comm_io_groups(1), ntasks_io_group,    ierr )
      CALL MPI_COMM_RANK( mpi_comm_io_groups(1), mytask_io_group,    ierr )
      CALL mpi_x_comm_size( mpi_comm_local,        ntasks_local_group, ierr )
      CALL MPI_COMM_RANK( mpi_comm_local,        mytask_local,       ierr )

      CALL MPI_TYPE_SIZE( MPI_INTEGER, inttypesize, ierr )
      IF ( inttypesize <= 0 ) THEN
        CALL wrf_error_fatal("external/RSL/module_dm.F: quilt: type size <= 0 invalid")
      ENDIF
! infinite loop until shutdown message received
!<DESCRIPTION>
!<PRE>
! This is the main request-handling loop.  I/O quilt servers stay in this loop 
! until the model run ends.  
!</PRE>
!</DESCRIPTION>
      DO WHILE (.TRUE.)

!<DESCRIPTION>
!<PRE>
! Each I/O server receives requests from its compute tasks.  Each request
! is contained in a data header (see module_internal_header_util.F for
! detailed descriptions of data headers).
! Each request is sent in two phases.  First, sizes of all messages that 
! will be sent from the compute tasks to this I/O server are summed on the 
! I/O server via MPI_reduce().  The I/O server then allocates buffer "obuf" 
! and receives concatenated messages from the compute tasks in it via the 
! call to collect_on_comm().  Note that "sizes" are generally expressed in 
! *bytes* in this code so conversion to "count" (number of Fortran words) is 
! required for Fortran indexing and MPI calls.  
!</PRE>
!</DESCRIPTION>
        ! wait for info from compute tasks in the I/O group that we're ready to rock
        ! obufsize will contain number of *bytes*
        CALL MPI_Reduce( ninbuf, obufsize, 1, MPI_INTEGER,  &
                         MPI_SUM, mytask_io_group,          &
                         mpi_comm_io_groups(1), ierr )
!write(0,*)'ninbuf ',ninbuf,' obufsize ', obufsize
!<DESCRIPTION>
!<PRE>
! Negative obufsize will trigger I/O server exit.  
!</PRE>
!</DESCRIPTION>
        IF ( obufsize .LT. 0 ) THEN
#ifdef NETCDF
          CALL ext_ncd_ioexit( Status )
#endif
#ifdef INTIO
          CALL ext_int_ioexit( Status )
#endif
#ifdef XXX
          CALL ext_xxx_ioexit( Status )
#endif
#ifdef YYY
          CALL ext_yyy_ioexit( Status )
#endif
#ifdef ZZZ
          CALL ext_zzz_ioexit( Status )
#endif
#ifdef GRIB1
          CALL ext_gr1_ioexit( Status )
#endif
#ifdef GRIB2
          CALL ext_gr2_ioexit( Status )
#endif
          CALL wrf_message ( 'I/O QUILT SERVERS DONE' )
          CALL mpi_finalize(ierr)
          STOP
        ENDIF

!<DESCRIPTION>
!<PRE>
! Allocate buffer obuf to be big enough for the data the compute tasks
! will send.  Note: obuf is size in *bytes* so we need to pare this 
! down, since the buffer is INTEGER.  
!</PRE>
!</DESCRIPTION>
!write(0,*)'allocating obuf: ',(obufsize+1)/inttypesize
        ALLOCATE( obuf( (obufsize+1)/inttypesize ) )

!call mpi_comm_size( mpi_comm_io_groups(1), iii, ierr )
!write(0,*)'mpi_comm_size mpi_comm_io_groups() ',iii
!call mpi_comm_rank( mpi_comm_io_groups(1), iii, ierr )
!write(0,*)'mpi_comm_rank mpi_comm_io_groups() ',iii

!<DESCRIPTION>
!<PRE>
! let's roll; get the data from the compute procs and put in obuf
!</PRE>
!</DESCRIPTION>
        CALL collect_on_comm( mpi_comm_io_groups(1),        &
                              onebyte,                      &
                              dummy, 0,                     &
                              obuf, obufsize )

!write(0,*)'calling init_store_piece_of_field'
!<DESCRIPTION>
!<PRE>
! Now all messages received from the compute clients are stored in 
! obuf.  Scan through obuf and extract headers and field data and store in 
! internal buffers.  The scan is done twice, first to determine sizes of 
! internal buffers required for storage of headers and fields and second to 
! actually store the headers and fields.  This bit of code does not do the 
! "quilting" (assembly of patches into full domains).  For each field, it 
! simply concatenates all received patches for the field into a separate 
! internal buffer (i.e. one buffer per field).  Quilting is done later by 
! routine store_patch_in_outbuf().  
!</PRE>
!</DESCRIPTION>
        CALL init_store_piece_of_field
        CALL mpi_type_size ( MPI_INTEGER , itypesize , ierr )
!write(0,*)'mpi_type_size returns ', itypesize
!<DESCRIPTION>
!<PRE>
! Scan obuf the first time to calculate the size of the buffer required for 
! each field.  Calls to add_to_bufsize_for_field() accumulate sizes.  
!</PRE>
!</DESCRIPTION>
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

!write(0,*) 'X-1', hdrbufsize, get_hdr_tag( obuf ( icurs / inttypesize ) ) , get_hdr_rec_size( obuf ( icurs / inttypesize ) ), TRIM(VarName)
              call add_to_bufsize_for_field( VarName, hdrbufsize )
              icurs = icurs + hdrbufsize
              ! If this is a real write (i.e. not a training write), accumulate
              ! buffersize for this field.
              IF ( DomainDesc .NE. 333933 ) THEN   ! magic number
!write(0,*) 'X-1a', chunksize, TRIM(VarName)
                call add_to_bufsize_for_field( VarName, chunksize )
                icurs = icurs + chunksize
              ENDIF
            CASE DEFAULT
              hdrbufsize = obuf(icurs/inttypesize)
              write(VarName,'(I5.5)')vid 
!write(0,*) 'X-2', hdrbufsize, get_hdr_tag( obuf ( icurs / inttypesize ) ) , get_hdr_rec_size( obuf ( icurs / inttypesize ) ), TRIM(VarName)
              call add_to_bufsize_for_field( VarName, hdrbufsize )
              icurs = icurs + hdrbufsize
              vid = vid+1
          END SELECT
        ENDDO
!<DESCRIPTION>
!<PRE>
! Store the headers and field data in internal buffers.  The first call to 
! store_piece_of_field() allocates internal buffers using sizes computed by 
! calls to add_to_bufsize_for_field().  
!</PRE>
!</DESCRIPTION>
        vid = 0
        icurs = inttypesize
        DO WHILE ( icurs .lt. obufsize )
!write(0,*) 'A icurs ', icurs, ' obufsize ', obufsize
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
!write(0,*) 'A-1', hdrbufsize, get_hdr_tag( obuf ( icurs / inttypesize ) ) , get_hdr_rec_size( obuf ( icurs / inttypesize ) ), TRIM(VarName)
              icurs = icurs + hdrbufsize
              ! If this is a real write (i.e. not a training write), store
              ! this piece of this field.
              IF ( DomainDesc .NE. 333933 ) THEN   ! magic number
!write(0,*) 'A-1a', chunksize, TRIM(VarName)
                call store_piece_of_field( obuf(icurs/inttypesize), VarName, chunksize )
                icurs = icurs + chunksize
              ENDIF
            CASE DEFAULT
              hdrbufsize = obuf(icurs/inttypesize)
              write(VarName,'(I5.5)')vid 
              call store_piece_of_field( obuf(icurs/inttypesize), VarName, hdrbufsize )
!write(0,*) 'A-2', hdrbufsize, get_hdr_tag( obuf ( icurs / inttypesize ) ) , get_hdr_rec_size( obuf ( icurs / inttypesize ) ), TRIM(VarName)
              icurs = icurs + hdrbufsize
              vid = vid+1
          END SELECT
        ENDDO

!call mpi_comm_size( mpi_comm_local, iii, ierr )
!write(0,*)'mpi_comm_size mpi_comm_local ',iii
!call mpi_comm_rank( mpi_comm_local, iii, ierr )
!write(0,*)'mpi_comm_rank mpi_comm_local ',iii

!write(0,*)'calling init_retrieve_pieces_of_field '
!<DESCRIPTION>
!<PRE>
! Now, for each field, retrieve headers and patches (data) from the internal 
! buffers and collect them all on the I/O quilt server "root" task.
!</PRE>
!</DESCRIPTION>
        CALL init_retrieve_pieces_of_field
!<DESCRIPTION>
!<PRE>
! Retrieve header and all patches for the first field from the internal 
! buffers.  
!</PRE>
!</DESCRIPTION>
        CALL retrieve_pieces_of_field ( obuf , VarName, obufsize, sz, retval )
!write(0,*)'calling first retrieve_pieces_of_field ',TRIM(VarName),obufsize,sz,retval
!<DESCRIPTION>
!<PRE>
! Sum sizes of all headers and patches (data) for this field from all I/O 
! servers in this I/O server group onto the I/O server "root".
!</PRE>
!</DESCRIPTION>
        CALL MPI_Reduce( sz, bigbufsize, 1, MPI_INTEGER,  &
                         MPI_SUM, ntasks_local_group-1,         &
                         mpi_comm_local, ierr )
!write(0,*)'after MPI_Reduce ',sz, bigbufsize

!<DESCRIPTION>
!<PRE>
! Loop until there are no more fields to retrieve from the internal buffers.
!</PRE>
!</DESCRIPTION>
        DO WHILE ( retval )

!write(0,*)' VarName ',TRIM(VarName),' sz ',sz,' bigbufsize ',bigbufsize

!<DESCRIPTION>
!<PRE>
! I/O server "root" allocates space to collect headers and fields from all
! other servers in this I/O server group.
!</PRE>
!</DESCRIPTION>
          IF ( mytask_local .EQ. ntasks_local_group-1 ) THEN
!write(0,*)'allocating bufbuf ', (bigbufsize+1)/inttypesize
            ALLOCATE( bigbuf( (bigbufsize+1)/inttypesize ) )
          ENDIF

!write(0,*)'before collect_on_comm tag,size ',Trim(VarName),get_hdr_tag(obuf),get_hdr_rec_size(obuf)
!<DESCRIPTION>
!<PRE>
! Collect buffers and fields from all I/O servers in this I/O server group
! onto the I/O server "root"
!</PRE>
!</DESCRIPTION>
          CALL collect_on_comm( mpi_comm_local,                    &
                                onebyte,                           &
                                obuf, sz,  &
                                bigbuf, bigbufsize )
!write(0,*)'after collect_on_comm ', sz, bigbufsize
!<DESCRIPTION>
!<PRE>
! The I/O server "root" now handles collected requests from all compute 
! tasks served by this I/O server group (i.e. all compute tasks).  
!</PRE>
!</DESCRIPTION>
          IF ( mytask_local .EQ. ntasks_local_group-1 ) THEN
!jjj = 4
!do iii = 1, ntasks_local_group
!  write(0,*)'i,j,tag,size ', iii, jjj, get_hdr_tag(bigbuf(jjj/4)),get_hdr_rec_size(bigbuf(jjj/4))
!  jjj = jjj + get_hdr_rec_size(bigbuf(jjj/4))
!enddo

            icurs = inttypesize  ! icurs is a byte counter, but buffer is integer

            stored_write_record = .false.

!<DESCRIPTION>
!<PRE>
! The I/O server "root" loops over the collected requests.  
!</PRE>
!</DESCRIPTION>
            DO WHILE ( icurs .lt. bigbufsize )
              CALL mpi_type_size ( MPI_INTEGER , itypesize , ierr )

!write(0,*)'B tag,size ',icurs,get_hdr_tag( bigbuf(icurs/inttypesize) ),get_hdr_rec_size( bigbuf(icurs/inttypesize) )
!write(0,*)'   inttypesize ',inttypesize,' itypesize ',itypesize,' icurs/inttypesize ',icurs/inttypesize
!<DESCRIPTION>
!<PRE>
! The I/O server "root" gets the request out of the next header and
! handles it by, in most cases, calling the appropriate external I/O package
! interface.
!</PRE>
!</DESCRIPTION>
              SELECT CASE ( get_hdr_tag( bigbuf(icurs/inttypesize) ) )
!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "noop" (do nothing) request.  This is 
! actually quite easy.  "Noop" requests exist to help avoid race conditions.  
! In some cases, only one compute task will everything about a request so 
! other compute tasks send "noop" requests.  
!</PRE>
!</DESCRIPTION>
                CASE ( int_noop )
!write(0,*)' int_noop '
                  CALL int_get_noop_header( bigbuf(icurs/inttypesize), hdrbufsize, itypesize )
                  icurs = icurs + hdrbufsize

!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "put_dom_td_real" request.
!</PRE>
!</DESCRIPTION>
                CASE ( int_dom_td_real )
!write(0,*)' int_dom_td_real '
                  CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                  ALLOCATE( RData( bigbuf(icurs/inttypesize + 4 ) ) )      ! 5 is the count of data items for this record ; defined in collect_on_comm.c
                  CALL int_get_td_header( bigbuf(icurs/inttypesize:), hdrbufsize, inttypesize, ftypesize, &
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
#ifdef YYY
                 CASE ( IO_YYY )
                    CALL ext_yyy_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
#endif
#ifdef GRIB1
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
#endif
#ifdef GRIB2
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_td_real( handle(DataHandle),TRIM(Element),TRIM(DateStr),RData, Count, Status )
#endif
                     CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( RData )
!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "put_dom_ti_real" request.
!</PRE>
!</DESCRIPTION>
                CASE ( int_dom_ti_real )
!write(0,*)' int_dom_ti_real '
                  CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                  ALLOCATE( RData( bigbuf(icurs/inttypesize + 4 ) ) )      ! 5 is the count of data items for this record ; defined in collect_on_comm.c
                  CALL int_get_ti_header( bigbuf(icurs/inttypesize:), hdrbufsize, inttypesize, ftypesize, &
                                          DataHandle, Element, RData, Count, code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
!write(0,*)'ext_ncd_put_dom_ti_real ',handle(DataHandle),TRIM(Element),RData,Status
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
#endif
#ifdef YYY
                 CASE ( IO_YYY )
                    CALL ext_yyy_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
#endif
#ifdef GRIB1
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
#endif
#ifdef GRIB2
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_ti_real( handle(DataHandle),TRIM(Element), RData, Count, Status )
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( RData )

!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "put_dom_td_integer" request.
!</PRE>
!</DESCRIPTION>
                CASE ( int_dom_td_integer )
!write(0,*)' int_dom_td_integer '
                  CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                  ALLOCATE( IData( bigbuf(icurs/inttypesize + 4 ) ) )      ! 5 is the count of data items for this record ; defined in collect_on_comm.c
                  CALL int_get_td_header( bigbuf(icurs/inttypesize:), hdrbufsize, inttypesize, ftypesize, &
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
#ifdef YYY
                 CASE ( IO_YYY )
                    CALL ext_yyy_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
#endif
#ifdef GRIB1
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
#endif
#ifdef GRIB2
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_td_integer( handle(DataHandle),TRIM(Element), Trim(DateStr), IData, Count, Status )
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( IData )

!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "put_dom_ti_integer" request.
!</PRE>
!</DESCRIPTION>
                CASE ( int_dom_ti_integer )
!write(0,*)' int_dom_ti_integer '

                  CALL mpi_type_size( MPI_INTEGER, ftypesize, ierr )
                  ALLOCATE( IData( bigbuf(icurs/inttypesize + 4 ) ) )      ! 5 is the count of data items for this record ; defined in collect_on_comm.c
                  CALL int_get_ti_header( bigbuf(icurs/inttypesize:), hdrbufsize, inttypesize, ftypesize, &
                                          DataHandle, Element, IData, Count, code )
                  icurs = icurs + hdrbufsize
                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
!write(0,*)'ext_ncd_put_dom_ti_integer ',handle(DataHandle),TRIM(Element),IData,Status
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
#endif
#ifdef YYY
                 CASE ( IO_YYY )
                    CALL ext_yyy_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
#endif
#ifdef GRIB1
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
#endif
#ifdef GRIB2
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_ti_integer( handle(DataHandle),TRIM(Element), IData, Count, Status )
#endif

                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  DEALLOCATE( IData)
 
!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "set_time" request.
!</PRE>
!</DESCRIPTION>
                CASE ( int_set_time )
!write(0,*)' int_set_time '
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

!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "put_dom_ti_char" request.
!</PRE>
!</DESCRIPTION>
                CASE ( int_dom_ti_char )
!write(0,*)' int_dom_ti_char '
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
#ifdef YYY
                 CASE ( IO_YYY )
                    CALL ext_yyy_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
#endif
#ifdef GRIB1
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
#endif
#ifdef GRIB2
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_dom_ti_char ( handle(DataHandle), TRIM(Element), TRIM(CData), Status)
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  icurs = icurs + hdrbufsize

!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "put_var_ti_char" request.
!</PRE>
!</DESCRIPTION>
                CASE ( int_var_ti_char )
!write(0,*)' int_var_ti_char '
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
#ifdef YYY
                 CASE ( IO_YYY )
                    CALL ext_yyy_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
#endif
#ifdef GRIB1
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
#endif
#ifdef GRIB2
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_put_var_ti_char ( handle(DataHandle), TRIM(Element), TRIM(VarName), TRIM(CData), Status)
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

                  icurs = icurs + hdrbufsize

                CASE ( int_ioexit )
! ioexit is now handled by sending negative message length to server
                  CALL wrf_error_fatal( &
                         "quilt: should have handled int_ioexit already")
!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "ioclose" request.
!</PRE>
!</DESCRIPTION>
                CASE ( int_ioclose )
!write(0,*)' int_ioclose '
                  CALL int_get_handle_header( bigbuf(icurs/inttypesize), hdrbufsize, itypesize, &
                                              DataHandle , code )
                  icurs = icurs + hdrbufsize

                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_inquire_filename( handle(DataHandle), fname, fstat, Status )
                      IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                        CALL ext_ncd_ioclose(handle(DataHandle),Status)
                      ENDIF
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_inquire_filename( handle(DataHandle), fname, fstat, Status )
                      IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                        CALL ext_int_ioclose(handle(DataHandle),Status)
                      ENDIF
#endif
#ifdef YYY
                 CASE ( IO_YYY )
                    CALL ext_yyy_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                      CALL ext_yyy_ioclose(handle(DataHandle),Status)
                    ENDIF
#endif
#ifdef GRIB1
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                      CALL ext_gr1_ioclose(handle(DataHandle),Status)
                    ENDIF
#endif
#ifdef GRIB2
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_FOR_WRITE .OR. fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                      CALL ext_gr2_ioclose(handle(DataHandle),Status)
                    ENDIF
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT

!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "open_for_write_begin" request.
!</PRE>
!</DESCRIPTION>
                CASE ( int_open_for_write_begin )

                  CALL int_get_ofwb_header( bigbuf(icurs/inttypesize), hdrbufsize, itypesize, &
                                            FileName,SysDepInfo,io_form_arg,DataHandle )

!write(0,*)' int_open_for_write_begin inttypesize ',inttypesize,' itypesize ',itypesize
!write(0,*)' int_open_for_write_begin icurs ', icurs, hdrbufsize
!write(0,*)' int_open_for_write_begin FileName ',TRIM(FileName) 
!write(0,*)' int_open_for_write_begin SysDepInfo ',TRIM(SysDepInfo) 
                  icurs = icurs + hdrbufsize
!write(0,*)' int_open_for_write_begin new icurs,tag,size ', icurs, get_hdr_tag( bigbuf(icurs/inttypesize) ),get_hdr_rec_size( bigbuf(icurs/inttypesize) )
                
                  io_form(DataHandle) = io_form_arg

                  SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                    CASE ( IO_NETCDF   )
                      CALL ext_ncd_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
!write(0,*)'ext_ncd_open_for_write_begin ',Trim(FileName),DataHandle,handle(DataHandle),Status
#endif
#ifdef INTIO
                    CASE ( IO_INTIO   )
                      CALL ext_int_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
#endif
#ifdef YYY
                    CASE ( IO_YYY )
                       CALL ext_yyy_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
#endif
#ifdef GRIB1
                    CASE ( IO_GRIB1 )
                       CALL ext_gr1_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
#endif
#ifdef GRIB2
                    CASE ( IO_GRIB2 )
                       CALL ext_gr2_open_for_write_begin(FileName,Comm,IOComm,SysDepInfo,handle(DataHandle),Status)
#endif
                    CASE DEFAULT
                      Status = 0
                  END SELECT
                
                  okay_to_write(DataHandle) = .false.

!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "open_for_write_commit" request.
! In this case, the "okay_to_commit" is simply set to .true. so "write_field"
! requests will initiate writes to disk.  Actual commit will be done after
! all requests in this batch have been handled.
!</PRE>
!</DESCRIPTION>
                CASE ( int_open_for_write_commit )

                  CALL int_get_handle_header( bigbuf(icurs/inttypesize), hdrbufsize, itypesize, &
                                              DataHandle , code )
!write(0,*)' int_open_for_write_commit icurs ', icurs, hdrbufsize
                  icurs = icurs + hdrbufsize
                  okay_to_commit(DataHandle) = .true.

!<DESCRIPTION>
!<PRE>
! The I/O server "root" handles the "write_field" (int_field) request.
! If okay_to_write(DataHandle) is .true. then the patch in the
! header (bigbuf) is written to a globally-sized internal output buffer via
! the call to store_patch_in_outbuf().  Note that this is where the actual
! "quilting" (reassembly of patches onto a full-size domain) is done.  If
! okay_to_write(DataHandle) is .false. then external I/O package interfaces
! are called to write metadata for I/O formats that support native metadata.
!
! NOTE that the I/O server "root" will only see write_field (int_field)
! requests AFTER an "iosync" request.
!</PRE>
!</DESCRIPTION>
                CASE ( int_field )
!write(0,*)' int_field '
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
                      ! Note that the WRF_DOUBLE branch of this IF statement must come first since 
                      ! WRF_REAL is set equal to WRF_DOUBLE during autopromotion builds.  
                      IF ( FieldType .EQ. WRF_DOUBLE)  THEN
! this branch has not been tested TBH: 20050406
                        CALL mpi_type_size( MPI_DOUBLE_PRECISION, ftypesize, ierr )
                      ELSE
                        CALL mpi_type_size( MPI_REAL, ftypesize, ierr )
                      ENDIF
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
#if 0
! since this is training and the grib output doesn't need training, disable this branch.
#ifdef YYY
                 CASE ( IO_YYY )
                      CALL ext_YYY_write_field ( handle(DataHandle) , TRIM(DateStr) ,         &
                                 TRIM(VarName) , dummy , FieldType , Comm , IOComm,           &
                                 DomainDesc , TRIM(MemoryOrder) , TRIM(Stagger) , DimNames ,  &
                                 DomainStart , DomainEnd ,                                    &
                                 DomainStart , DomainEnd ,                                    &
                                 DomainStart , DomainEnd ,                                    &
                                 Status )
#endif
#endif
                      CASE DEFAULT
                        Status = 0
                    END SELECT
                  ENDIF
                CASE ( int_iosync )
!write(0,*)' int_iosync '
                  CALL int_get_handle_header( bigbuf(icurs/inttypesize), hdrbufsize, itypesize, &
                                            DataHandle , code )
                  icurs = icurs + hdrbufsize
                CASE DEFAULT
                  WRITE(mess,*)'quilt: bad tag: ',get_hdr_tag( bigbuf(icurs/inttypesize) ),' icurs ',icurs/inttypesize
                  CALL wrf_error_fatal( mess )
              END SELECT

            ENDDO
!<DESCRIPTION>
!<PRE>
! Now, the I/O server "root" has finshed handling all commands from the latest
! call to retrieve_pieces_of_field().
!</PRE>
!</DESCRIPTION>

            IF (stored_write_record) THEN
!write(0,*)'calling write_outbuf ',DataHandle
!<DESCRIPTION>
!<PRE>
! If any fields have been stored in a globally-sized internal output buffer
! (via a call to store_patch_in_outbuf()) then call write_outbuf() to write
! them to disk now.
! NOTE that the I/O server "root" will only have called
! store_patch_in_outbuf() when handling write_field (int_field)
! commands which only arrive AFTER an "iosync" command.
!</PRE>
!</DESCRIPTION>
              CALL write_outbuf ( handle(DataHandle), use_package(io_form(DataHandle))) 
!write(0,*)'back from write_outbuf ',DataHandle
            ENDIF

!<DESCRIPTION>
!<PRE>
! If one or more "open_for_write_commit" commands were encountered from the
! latest call to retrieve_pieces_of_field() then call the package-specific
! routine to do the commit.
!</PRE>
!</DESCRIPTION>
            IF (okay_to_commit(DataHandle)) THEN

              SELECT CASE (use_package(io_form(DataHandle)))
#ifdef NETCDF
                CASE ( IO_NETCDF   )
                  CALL ext_ncd_inquire_filename( handle(DataHandle), fname, fstat, Status )
!write(0,*)'preparing to commit ', DataHandle, fstat, fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED
                  IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
!write(0,*)'calling ext_ncd_open_for_write_commit ', handle(DataHandle), DataHandle
                    CALL ext_ncd_open_for_write_commit(handle(DataHandle),Status)
!write(0,*)'back from ext_ncd_open_for_write_commit ', Status
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
#ifdef YYY
                 CASE ( IO_YYY )
                    CALL ext_yyy_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                       CALL ext_yyy_open_for_write_commit(handle(DataHandle),Status)
                       okay_to_write(DataHandle) = .true.
                    ENDIF
#endif
#ifdef GRIB1
                 CASE ( IO_GRIB1 )
                    CALL ext_gr1_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                       CALL ext_gr1_open_for_write_commit(handle(DataHandle),Status)
                       okay_to_write(DataHandle) = .true.
                    ENDIF
#endif
#ifdef GRIB2
                 CASE ( IO_GRIB2 )
                    CALL ext_gr2_inquire_filename( handle(DataHandle), fname, fstat, Status )
                    IF ( fstat .EQ. WRF_FILE_OPENED_NOT_COMMITTED ) THEN
                       CALL ext_gr2_open_for_write_commit(handle(DataHandle),Status)
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

!<DESCRIPTION>
!<PRE>
! Retrieve header and all patches for the next field from the internal 
! buffers.  
!</PRE>
!</DESCRIPTION>
        CALL retrieve_pieces_of_field ( obuf , VarName, obufsize, sz, retval )
!write(0,*)'calling next retrieve_pieces_of_field ',trim(VarName),obufsize,sz,retval
!<DESCRIPTION>
!<PRE>
! Sum sizes of all headers and patches (data) for this field from all I/O 
! servers in this I/O server group onto the I/O server "root".
!</PRE>
!</DESCRIPTION>
        CALL MPI_Reduce( sz, bigbufsize, 1, MPI_INTEGER,  &
                         MPI_SUM, ntasks_local_group-1,         &
                         mpi_comm_local, ierr )
!<DESCRIPTION>
!<PRE>
! Then, return to the top of the loop to collect headers and data from all 
! I/O servers in this I/O server group onto the I/O server "root" and handle 
! the next batch of commands.  
!</PRE>
!</DESCRIPTION>
!write(0,*)'after MPI_Reduce ',sz, bigbufsize
      END DO

      DEALLOCATE( obuf )

      ! flush output files if needed
      IF (stored_write_record) THEN
        SELECT CASE ( use_package(io_form) )
#ifdef NETCDF
          CASE ( IO_NETCDF   )
            CALL ext_ncd_iosync( handle(DataHandle), Status )
#endif
#ifdef XXX
          CASE ( IO_XXX   )
            CALL ext_xxx_iosync( handle(DataHandle), Status )
#endif
#ifdef YYY
          CASE ( IO_YYY   )
            CALL ext_yyy_iosync( handle(DataHandle), Status )
#endif
#ifdef ZZZ
          CASE ( IO_ZZZ   )
            CALL ext_zzz_iosync( handle(DataHandle), Status )
#endif
#ifdef GRIB1
          CASE ( IO_GRIB1   )
            CALL ext_gr1_iosync( handle(DataHandle), Status )
#endif
#ifdef GRIB2
          CASE ( IO_GRIB2   )
            CALL ext_gr2_iosync( handle(DataHandle), Status )
#endif
#ifdef INTIO
          CASE ( IO_INTIO   )
            CALL ext_int_iosync( handle(DataHandle), Status )
#endif
          CASE DEFAULT
            Status = 0
        END SELECT
      ENDIF

      END DO

    END SUBROUTINE quilt

! end of #endif of DM_PARALLEL
#endif

    SUBROUTINE init_module_wrf_quilt
!<DESCRIPTION>
!<PRE>
! Both client (compute) and server tasks call this routine to initialize the 
! module.  Routine setup_quilt_servers() is called from this routine to 
! determine which tasks are compute tasks and which are server tasks.  Server 
! tasks then call routine quilt() and remain there for the rest of the model 
! run.  Compute tasks return from init_module_wrf_quilt() to perform model 
! computations.  
!</PRE>
!</DESCRIPTION>
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
    END SUBROUTINE init_module_wrf_quilt
END MODULE module_wrf_quilt

!<DESCRIPTION>
!<PRE>
! Remaining routines in this file are defined outside of the module
! either to defeat arg/param type checking or to avoid an explicit use
! dependence.
!</PRE>
!</DESCRIPTION>

SUBROUTINE disable_quilting
!<DESCRIPTION>
!<PRE>
! Call this in programs that you never want to be quilting (e.g. real)
! Must call before call to init_module_wrf_quilt().  
!</PRE>
!</DESCRIPTION>
  USE module_wrf_quilt
  disable_quilt = .TRUE.
  RETURN
END SUBROUTINE disable_quilting

LOGICAL FUNCTION  use_output_servers()
!<DESCRIPTION>
!<PRE>
! Returns .TRUE. if I/O quilt servers are in-use for write operations.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  USE module_wrf_quilt
  use_output_servers = quilting_enabled
  RETURN
END FUNCTION use_output_servers

LOGICAL FUNCTION  use_input_servers()
!<DESCRIPTION>
!<PRE>
! Returns .TRUE. if I/O quilt servers are in-use for read operations.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
  USE module_wrf_quilt
  use_input_servers = .FALSE.
  RETURN
END FUNCTION use_input_servers

SUBROUTINE wrf_quilt_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                     DataHandle , io_form_arg, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to begin data definition ("training") phase
! for writing to WRF dataset FileName.  io_form_arg indicates file format.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER *(*), INTENT(IN)  :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(IN)  :: io_form_arg
  INTEGER ,       INTENT(OUT) :: Status
! Local
  CHARACTER*132   :: locFileName, locSysDepInfo
  INTEGER i, itypesize, tasks_in_group, ierr, comm_io_group
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  REAL dummy

  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_open_for_write_begin' ) 
  CALL int_get_fresh_handle(i)
  okay_to_write(i) = .false.
  DataHandle = i

  locFileName = FileName
  locSysDepInfo = SysDepInfo

  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
  IF ( wrf_dm_on_monitor() ) THEN
    CALL int_gen_ofwb_header( hdrbuf, hdrbufsize, itypesize, &
                            locFileName,locSysDepInfo,io_form_arg,DataHandle )
  ELSE
    CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
  ENDIF

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

  Status = 0


#endif
  RETURN  
END SUBROUTINE wrf_quilt_open_for_write_begin

SUBROUTINE wrf_quilt_open_for_write_commit( DataHandle , Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to switch an internal flag to enable output
! for the dataset referenced by DataHandle.  The call to
! wrf_quilt_open_for_write_commit() must be paired with a call to
! wrf_quilt_open_for_write_begin().
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy

  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_open_for_write_commit' ) 
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
END SUBROUTINE wrf_quilt_open_for_write_commit

SUBROUTINE wrf_quilt_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to open WRF dataset FileName for reading.
! This routine is called only by client (compute) tasks.  
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER *(*), INTENT(IN)  :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_open_for_read' ) 
  DataHandle = -1
  Status = -1
  CALL wrf_error_fatal ( "frame/module_io_quilt.F: wrf_quilt_open_for_read not yet supported" )
#endif
  RETURN  
END SUBROUTINE wrf_quilt_open_for_read

SUBROUTINE wrf_quilt_inquire_opened ( DataHandle, FileName , FileStatus, Status )
!<DESCRIPTION>
!<PRE>
! Inquire if the dataset referenced by DataHandle is open.
! Does not require communication with I/O servers.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_wrf_quilt
  IMPLICIT NONE
  include 'wrf_io_flags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status

  Status = 0

  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_inquire_opened' ) 
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      IF ( okay_to_write( DataHandle ) ) THEN
        FileStatus = WRF_FILE_OPENED_FOR_WRITE
      ENDIF
    ENDIF
  ENDIF
  Status = 0
  
#endif
  RETURN
END SUBROUTINE wrf_quilt_inquire_opened

SUBROUTINE wrf_quilt_inquire_filename ( DataHandle, FileName , FileStatus, Status )
!<DESCRIPTION>
!<PRE>
! Return the Filename and FileStatus associated with DataHandle.
! Does not require communication with I/O servers.
!
! Note that the current implementation does not actually return FileName.
! Currenlty, WRF does not use this returned value.  Fixing this would simply
! require saving the file names on the client tasks in an array similar to
! okay_to_write().
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_wrf_quilt
  IMPLICIT NONE
  include 'wrf_io_flags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER *(*), INTENT(OUT) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_inquire_filename' ) 
  Status = 0
  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      IF ( okay_to_write( DataHandle ) ) THEN
        FileStatus = WRF_FILE_OPENED_FOR_WRITE
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
END SUBROUTINE wrf_quilt_inquire_filename

SUBROUTINE wrf_quilt_iosync ( DataHandle, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to synchronize the disk copy of a dataset
! with memory buffers.
!
! After the "iosync" header (request) is sent to the I/O quilt server,
! the compute tasks will then send the entire contents (headers and data) of
! int_local_output_buffer to their I/O quilt server.  This communication is
! done in subroutine send_to_io_quilt_servers().  After the I/O quilt servers
! receive this data, they will write all accumulated fields to disk.
!
! Significant time may be required for the I/O quilt servers to organize
! fields and write them to disk.  Therefore, the "iosync" request should be
! sent only when the compute tasks are ready to run for a while without
! needing to communicate with the servers.  Otherwise, the compute tasks
! will end up waiting for the servers to finish writing to disk, thus wasting
! any performance benefits of having servers at all.
!
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if  defined( DM_PARALLEL ) && ! defined (STUBMPI) 
  USE module_wrf_quilt
  IMPLICIT NONE
  include "mpif.h"
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status

  INTEGER locsize , inttypesize
  INTEGER ierr, tasks_in_group, comm_io_group, dummy, i

  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_iosync' ) 

  IF ( associated ( int_local_output_buffer ) ) THEN

    iserver = 1 ! only one server group for now
    CALL get_mpi_comm_io_groups( comm_io_group , iserver )

    CALL mpi_x_comm_size( comm_io_group , tasks_in_group , ierr )

    locsize = int_num_bytes_to_write(DataHandle)

    ! send the size of my local buffer to the i/o task (obufsize doesnt mean anything here)
    CALL MPI_Reduce( locsize, obufsize, 1, MPI_INTEGER,  &
                     MPI_SUM, tasks_in_group-1,          &   ! root = nio_tasks_in_group-1 is me
                     comm_io_group, ierr )


    ! send data to the i/o processor
#ifdef DEREF_KLUDGE
    CALL collect_on_comm( comm_io_group,            &
                          onebyte,                       &
                          int_local_output_buffer(1), locsize , &
                          dummy, 0 )
#else
    CALL collect_on_comm( comm_io_group,            &
                          onebyte,                       &
                          int_local_output_buffer, locsize , &
                          dummy, 0 )
#endif


    int_local_output_cursor = 1
!    int_num_bytes_to_write(DataHandle) = 0
    DEALLOCATE ( int_local_output_buffer )
    NULLIFY ( int_local_output_buffer )
  ELSE
    CALL wrf_message ("frame/module_io_quilt.F: wrf_quilt_iosync: no buffer allocated")
  ENDIF
  Status = 0
#endif
  RETURN
END SUBROUTINE wrf_quilt_iosync

SUBROUTINE wrf_quilt_ioclose ( DataHandle, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to close the dataset referenced by
! DataHandle.
! This routine also clears the client file handle and, if needed, deallocates
! int_local_output_buffer.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && ! defined( STUBMPI) 
  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER i, itypesize, tasks_in_group, comm_io_group, ierr
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  REAL dummy

  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_ioclose' ) 
  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )

  IF ( wrf_dm_on_monitor() ) THEN
    CALL int_gen_handle_header( hdrbuf, hdrbufsize, itypesize, &
                                DataHandle , int_ioclose )
  ELSE
    CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
  ENDIF

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
END SUBROUTINE wrf_quilt_ioclose

SUBROUTINE wrf_quilt_ioexit( Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to shut down the WRF I/O system.
! Do not call any wrf_quilt_*() routines after this routine has been called.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && ! defined (STUBMPI ) 
  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(OUT) :: Status
  INTEGER                     :: DataHandle
  INTEGER i, itypesize, tasks_in_group, comm_io_group, ierr
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  REAL dummy

  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_ioexit' ) 
  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )

  IF ( wrf_dm_on_monitor() ) THEN
    CALL int_gen_handle_header( hdrbuf, hdrbufsize, itypesize, &
                                DataHandle , int_ioexit )  ! Handle is dummy
  ELSE
    CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
  ENDIF

  iserver = 1 ! only one server group for now
  CALL get_mpi_comm_io_groups( comm_io_group , iserver )

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

SUBROUTINE wrf_quilt_get_next_time ( DataHandle, DateStr, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to return the next time stamp.
! This is not yet supported.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && ! defined (STUBMPI) 
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*)               :: DateStr
  INTEGER                     :: Status
#endif
  RETURN
END SUBROUTINE wrf_quilt_get_next_time

SUBROUTINE wrf_quilt_get_previous_time ( DataHandle, DateStr, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to return the previous time stamp.
! This is not yet supported.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && ! defined (STUBMPI)
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*)               :: DateStr
  INTEGER                     :: Status
#endif
  RETURN
END SUBROUTINE wrf_quilt_get_previous_time

SUBROUTINE wrf_quilt_set_time ( DataHandle, Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to set the time stamp in the dataset
! referenced by DataHandle.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Data
  INTEGER                     :: Status
  INTEGER i, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
  INTEGER                 :: Count
!
  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_set_time' )

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
END SUBROUTINE wrf_quilt_set_time

SUBROUTINE wrf_quilt_get_next_var ( DataHandle, VarName, Status )
!<DESCRIPTION>
!<PRE>
! When reading, instruct the I/O quilt servers to return the name of the next
! variable in the current time frame.
! This is not yet supported.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*)               :: VarName
  INTEGER                     :: Status
#endif
  RETURN
END SUBROUTINE wrf_quilt_get_next_var

SUBROUTINE wrf_quilt_get_dom_ti_real ( DataHandle,Element,   Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent domain metadata named "Element"
! from the open dataset described by DataHandle.
! Metadata of type real are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  

! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  REAL,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Outcount
  INTEGER                     :: Status
  CALL wrf_message('wrf_quilt_get_dom_ti_real not supported yet')
#endif
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_real 

SUBROUTINE wrf_quilt_put_dom_ti_real ( DataHandle,Element,   Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! domain metadata named "Element"
! to the open dataset described by DataHandle.
! Metadata of type real are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
!Local
  CHARACTER*132   :: locElement
  INTEGER i, typesize, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
!
  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_put_dom_ti_real' ) 
  CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
  locElement = Element

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      CALL MPI_TYPE_SIZE( MPI_REAL, typesize, ierr )
      IF ( wrf_dm_on_monitor() ) THEN
        CALL int_gen_ti_header( hdrbuf, hdrbufsize, itypesize, typesize, &
                                DataHandle, locElement, Data, Count, int_dom_ti_real )
      ELSE
        CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
      ENDIF
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
END SUBROUTINE wrf_quilt_put_dom_ti_real 

SUBROUTINE wrf_quilt_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent domain metadata named "Element"
! from the open dataset described by DataHandle.
! Metadata of type double are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8                      :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: OutCount
  INTEGER                     :: Status
  CALL wrf_error_fatal('wrf_quilt_get_dom_ti_double not supported yet')
#endif
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_double 

SUBROUTINE wrf_quilt_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! domain metadata named "Element"
! to the open dataset described by DataHandle.
! Metadata of type double are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
  CALL wrf_error_fatal('wrf_quilt_put_dom_ti_double not supported yet')
#endif
RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_double 

SUBROUTINE wrf_quilt_get_dom_ti_integer ( DataHandle,Element,   Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent domain metadata named "Element"
! from the open dataset described by DataHandle.
! Metadata of type integer are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  integer                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
  CALL wrf_message('wrf_quilt_get_dom_ti_integer not supported yet')
#endif
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_integer 

SUBROUTINE wrf_quilt_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! domain metadata named "Element"
! to the open dataset described by DataHandle.
! Metadata of type integer are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  INTEGER ,       INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
! Local
  CHARACTER*132   :: locElement
  INTEGER i, typesize, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
!

  locElement = Element

  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_put_dom_ti_integer' ) 

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      CALL MPI_TYPE_SIZE( MPI_INTEGER, typesize, ierr )
      IF ( wrf_dm_on_monitor() ) THEN
        CALL int_gen_ti_header( hdrbuf, hdrbufsize, itypesize, typesize, &
                                DataHandle, locElement, Data, Count, int_dom_ti_integer )
      ELSE
        CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
      ENDIF
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
  CALL wrf_debug ( DEBUG_LVL, 'returning from wrf_quilt_put_dom_ti_integer' ) 

#endif
RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_integer 

SUBROUTINE wrf_quilt_get_dom_ti_logical ( DataHandle,Element,   Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent domain metadata named "Element"
! from the open dataset described by DataHandle.
! Metadata of type logical are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  logical                     :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                      :: OutCount
  INTEGER                     :: Status
  CALL wrf_message('wrf_quilt_get_dom_ti_logical not supported yet')
#endif
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_logical 

SUBROUTINE wrf_quilt_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! domain metadata named "Element"
! to the open dataset described by DataHandle.
! Metadata of type logical are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
  CALL wrf_message('wrf_quilt_put_dom_ti_logical not supported yet')
#endif
RETURN
END SUBROUTINE wrf_quilt_put_dom_ti_logical 

SUBROUTINE wrf_quilt_get_dom_ti_char ( DataHandle,Element,   Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read time independent
! domain metadata named "Element"
! from the open dataset described by DataHandle.
! Metadata of type char are
! stored in string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*)               :: Data
  INTEGER                     :: Status
  CALL wrf_message('wrf_quilt_get_dom_ti_char not supported yet')
#endif
RETURN
END SUBROUTINE wrf_quilt_get_dom_ti_char 

SUBROUTINE wrf_quilt_put_dom_ti_char ( DataHandle, Element,  Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write time independent
! domain metadata named "Element"
! to the open dataset described by DataHandle.
! Metadata of type char are
! copied from string Data.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: Data
  INTEGER                     :: Status
  INTEGER i, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
!
  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_put_dom_ti_char' ) 

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      IF ( wrf_dm_on_monitor() ) THEN
        CALL int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                                DataHandle, Element, "", Data, int_dom_ti_char )
      ELSE
        CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
      ENDIF
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
END SUBROUTINE wrf_quilt_put_dom_ti_char 

SUBROUTINE wrf_quilt_get_dom_td_real ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent domain metadata named "Element" valid at time DateStr
! from the open dataset described by DataHandle.
! Metadata of type real are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_get_dom_td_real 

SUBROUTINE wrf_quilt_put_dom_td_real ( DataHandle,Element, DateStr,  Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! domain metadata named "Element" valid at time DateStr
! to the open dataset described by DataHandle.
! Metadata of type real are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_put_dom_td_real 

SUBROUTINE wrf_quilt_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent domain metadata named "Element" valid at time DateStr
! from the open dataset described by DataHandle.
! Metadata of type double are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
  CALL wrf_error_fatal('wrf_quilt_get_dom_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_double 

SUBROUTINE wrf_quilt_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! domain metadata named "Element" valid at time DateStr
! to the open dataset described by DataHandle.
! Metadata of type double are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
  CALL wrf_error_fatal('wrf_quilt_put_dom_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_double 

SUBROUTINE wrf_quilt_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent domain metadata named "Element" valid at time DateStr
! from the open dataset described by DataHandle.
! Metadata of type integer are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_get_dom_td_integer 

SUBROUTINE wrf_quilt_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! domain metadata named "Element" valid at time DateStr
! to the open dataset described by DataHandle.
! Metadata of type integer are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_put_dom_td_integer 

SUBROUTINE wrf_quilt_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent domain metadata named "Element" valid at time DateStr
! from the open dataset described by DataHandle.
! Metadata of type logical are
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_get_dom_td_logical 

SUBROUTINE wrf_quilt_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! domain metadata named "Element" valid at time DateStr
! to the open dataset described by DataHandle.
! Metadata of type logical are
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_put_dom_td_logical 

SUBROUTINE wrf_quilt_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read time dependent
! domain metadata named "Element" valid at time DateStr
! from the open dataset described by DataHandle.
! Metadata of type char are
! stored in string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*)               :: Data
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE wrf_quilt_get_dom_td_char 

SUBROUTINE wrf_quilt_put_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct $he I/O quilt servers to write time dependent
! domain metadata named "Element" valid at time DateStr
! to the open dataset described by DataHandle.
! Metadata of type char are
! copied from string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN) :: Data
  INTEGER                          :: Status
#endif
RETURN
END SUBROUTINE wrf_quilt_put_dom_td_char 

SUBROUTINE wrf_quilt_get_var_ti_real ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent attribute "Element" of variable "Varname"
! from the open dataset described by DataHandle.
! Attribute of type real is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_get_var_ti_real 

SUBROUTINE wrf_quilt_put_var_ti_real ( DataHandle,Element,  Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! attribute "Element" of variable "Varname"
! to the open dataset described by DataHandle.
! Attribute of type real is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_put_var_ti_real 

SUBROUTINE wrf_quilt_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent attribute "Element" of variable "Varname"
! from the open dataset described by DataHandle.
! Attribute of type double is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
  CALL wrf_error_fatal('wrf_quilt_get_var_ti_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_double 

SUBROUTINE wrf_quilt_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! attribute "Element" of variable "Varname"
! to the open dataset described by DataHandle.
! Attribute of type double is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8 ,        INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER                     :: Status
#endif
  CALL wrf_error_fatal('wrf_quilt_put_var_ti_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_put_var_ti_double 

SUBROUTINE wrf_quilt_get_var_ti_integer ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent attribute "Element" of variable "Varname"
! from the open dataset described by DataHandle.
! Attribute of type integer is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_get_var_ti_integer 

SUBROUTINE wrf_quilt_put_var_ti_integer ( DataHandle,Element,  Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! attribute "Element" of variable "Varname"
! to the open dataset described by DataHandle.
! Attribute of type integer is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_put_var_ti_integer 

SUBROUTINE wrf_quilt_get_var_ti_logical ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! independent attribute "Element" of variable "Varname"
! from the open dataset described by DataHandle.
! Attribute of type logical is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_get_var_ti_logical 

SUBROUTINE wrf_quilt_put_var_ti_logical ( DataHandle,Element,  Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time independent
! attribute "Element" of variable "Varname"
! to the open dataset described by DataHandle.
! Attribute of type logical is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_put_var_ti_logical 

SUBROUTINE wrf_quilt_get_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read time independent
! attribute "Element" of variable "Varname"
! from the open dataset described by DataHandle.
! Attribute of type char is
! stored in string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*)               :: Data
  INTEGER                     :: Status
#endif
RETURN
END SUBROUTINE wrf_quilt_get_var_ti_char 

SUBROUTINE wrf_quilt_put_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write time independent
! attribute "Element" of variable "Varname"
! to the open dataset described by DataHandle.
! Attribute of type char is
! copied from string Data.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>

#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_wrf_quilt
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INCLUDE 'intio_tags.h'
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*) , INTENT(IN)  :: Data
  INTEGER                     :: Status
  INTEGER i, itypesize, tasks_in_group, ierr, comm_io_group
  REAL dummy
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
!

  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_put_var_ti_char' ) 

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles ) THEN
    IF ( int_handle_in_use( DataHandle ) ) THEN
      CALL MPI_TYPE_SIZE( MPI_INTEGER, itypesize, ierr )
      IF ( wrf_dm_on_monitor() ) THEN
        CALL int_gen_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                                DataHandle, TRIM(Element), TRIM(VarName), TRIM(Data), int_var_ti_char )
      ELSE
        CALL int_gen_noop_header( hdrbuf, hdrbufsize, itypesize )
      ENDIF
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
END SUBROUTINE wrf_quilt_put_var_ti_char 

SUBROUTINE wrf_quilt_get_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent attribute "Element" of variable "Varname" valid at time DateStr
! from the open dataset described by DataHandle.
! Attribute of type real is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_get_var_td_real 

SUBROUTINE wrf_quilt_put_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! to the open dataset described by DataHandle.
! Attribute of type real is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_put_var_td_real 

SUBROUTINE wrf_quilt_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent attribute "Element" of variable "Varname" valid at time DateStr
! from the open dataset described by DataHandle.
! Attribute of type double is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
  CALL wrf_error_fatal('wrf_quilt_get_var_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_get_var_td_double 

SUBROUTINE wrf_quilt_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! to the open dataset described by DataHandle.
! Attribute of type double is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
  CALL wrf_error_fatal('wrf_quilt_put_var_td_double not supported yet')
RETURN
END SUBROUTINE wrf_quilt_put_var_td_double 

SUBROUTINE wrf_quilt_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount,Status)
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent attribute "Element" of variable "Varname" valid at time DateStr
! from the open dataset described by DataHandle.
! Attribute of type integer is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_get_var_td_integer 

SUBROUTINE wrf_quilt_put_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! to the open dataset described by DataHandle.
! Attribute of type integer is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_put_var_td_integer 

SUBROUTINE wrf_quilt_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read Count words of time
! dependent attribute "Element" of variable "Varname" valid at time DateStr
! from the open dataset described by DataHandle.
! Attribute of type logical is
! stored in array Data.
! Actual number of words read is returned in OutCount.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_get_var_td_logical 

SUBROUTINE wrf_quilt_put_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write Count words of time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! to the open dataset described by DataHandle.
! Attribute of type logical is
! copied from array Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_put_var_td_logical 

SUBROUTINE wrf_quilt_get_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to attempt to read time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! from the open dataset described by DataHandle.
! Attribute of type char is
! stored in string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_get_var_td_char 

SUBROUTINE wrf_quilt_put_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to write time dependent
! attribute "Element" of variable "Varname" valid at time DateStr
! to the open dataset described by DataHandle.
! Attribute of type char is
! copied from string Data.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_put_var_td_char 

SUBROUTINE wrf_quilt_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
!<DESCRIPTION>
!<PRE>
! Instruct the I/O quilt servers to read the variable named VarName from the
! dataset pointed to by DataHandle.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_read_field

SUBROUTINE wrf_quilt_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )
!<DESCRIPTION>
!<PRE>
! Prepare instructions for the I/O quilt servers to write the variable named
! VarName to the dataset pointed to by DataHandle.
!
! During a "training" write this routine accumulates number and sizes of
! messages that will be sent to the I/O server associated with this compute
! (client) task.
!
! During a "real" write, this routine begins by allocating
! int_local_output_buffer if it has not already been allocated.  Sizes
! accumulated during "training" are used to determine how big
! int_local_output_buffer must be.  This routine then stores "int_field"
! headers and associated field data in int_local_output_buffer.  The contents
! of int_local_output_buffer are actually sent to the I/O quilt server in
! routine wrf_quilt_iosync().  This scheme allows output of multiple variables
! to be aggregated into a single "iosync" operation.
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
  USE module_state_description
  USE module_wrf_quilt
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
  INTEGER locsize , typesize, inttypesize
  INTEGER ierr, tasks_in_group, comm_io_group, dummy, i
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  INTEGER, EXTERNAL :: use_package

  CALL wrf_debug ( DEBUG_LVL, 'in wrf_quilt_write_field' ) 

  IF ( .NOT. (DataHandle .GE. 1 .AND. DataHandle .LE. int_num_handles) ) THEN
    CALL wrf_error_fatal("frame/module_io_quilt.F: wrf_quilt_write_field: invalid data handle" )
  ENDIF
  IF ( .NOT. int_handle_in_use( DataHandle ) ) THEN
    CALL wrf_error_fatal("frame/module_io_quilt.F: wrf_quilt_write_field: DataHandle not opened" )
  ENDIF

  locsize = (PatchEnd(1)-PatchStart(1)+1)* &
            (PatchEnd(2)-PatchStart(2)+1)* &
            (PatchEnd(3)-PatchStart(3)+1)

  CALL mpi_type_size( MPI_INTEGER, inttypesize, ierr )
  ! Note that the WRF_DOUBLE branch of this IF statement must come first since 
  ! WRF_REAL is set equal to WRF_DOUBLE during autopromotion builds.  
  IF ( FieldType .EQ. WRF_DOUBLE ) THEN
    CALL mpi_type_size( MPI_DOUBLE_PRECISION, typesize, ierr )
  ELSE IF ( FieldType .EQ. WRF_REAL ) THEN
    CALL mpi_type_size( MPI_REAL, typesize, ierr )
  ELSE IF ( FieldType .EQ. WRF_INTEGER ) THEN
    CALL mpi_type_size( MPI_INTEGER, typesize, ierr )
  ELSE IF ( FieldType .EQ. WRF_LOGICAL ) THEN
    CALL mpi_type_size( MPI_LOGICAL, typesize, ierr )
  ENDIF

  IF ( .NOT. okay_to_write( DataHandle ) ) THEN

      ! This is a "training" write.
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

  ELSE

    IF ( .NOT. associated( int_local_output_buffer ) ) THEN
      ALLOCATE ( int_local_output_buffer( (int_num_bytes_to_write( DataHandle )+1)/inttypesize ) )
      int_local_output_cursor = 1
    ENDIF

    ! This is NOT a "training" write.  It is OK to write now.
    CALL int_gen_write_field_header ( hdrbuf, hdrbufsize, inttypesize, typesize,           &
                             DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm,  &
                             0          , MemoryOrder , Stagger , DimNames ,              &   ! non-333933 means okay to write; magic number
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd )

    ! Pack header into int_local_output_buffer.  It will be sent to the 
    ! I/O servers during the next "iosync" operation.  
#ifdef DEREF_KLUDGE
    CALL int_pack_data ( hdrbuf , hdrbufsize , int_local_output_buffer(1), int_local_output_cursor )
#else
    CALL int_pack_data ( hdrbuf , hdrbufsize , int_local_output_buffer, int_local_output_cursor )
#endif

    ! Pack field data into int_local_output_buffer.  It will be sent to the 
    ! I/O servers during the next "iosync" operation.  
#ifdef DEREF_KLUDGE
    CALL int_pack_data ( Field(PatchStart(1):PatchEnd(1),PatchStart(2):PatchEnd(2),PatchStart(3):PatchEnd(3) ), &
                                  locsize * typesize , int_local_output_buffer(1), int_local_output_cursor )
#else
    CALL int_pack_data ( Field(PatchStart(1):PatchEnd(1),PatchStart(2):PatchEnd(2),PatchStart(3):PatchEnd(3) ), &
                                  locsize * typesize , int_local_output_buffer, int_local_output_cursor )
#endif

  ENDIF
  Status = 0

#endif
  RETURN
END SUBROUTINE wrf_quilt_write_field

SUBROUTINE wrf_quilt_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , Status )
!<DESCRIPTION>
!<PRE>
! This routine applies only to a dataset that is open for read.  It instructs
! the I/O quilt servers to return information about variable VarName.
! This routine is called only by client (compute) tasks.  
!
! This is not yet supported.
!</PRE>
!</DESCRIPTION>
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
END SUBROUTINE wrf_quilt_get_var_info

SUBROUTINE get_mpi_comm_io_groups( retval, isrvr )
!<DESCRIPTION>
!<PRE>
! This routine returns the compute+io communicator to which this
! compute task belongs for I/O server group "isrvr".
! This routine is called only by client (compute) tasks.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
      USE module_wrf_quilt
      IMPLICIT NONE
      INTEGER, INTENT(IN ) :: isrvr
      INTEGER, INTENT(OUT) :: retval
      retval = mpi_comm_io_groups(isrvr)
#endif
      RETURN
END SUBROUTINE get_mpi_comm_io_groups

SUBROUTINE get_nio_tasks_in_group( retval )
!<DESCRIPTION>
!<PRE>
! This routine returns the number of I/O server tasks in each 
! I/O server group.  It can be called by both clients and 
! servers.  
!</PRE>
!</DESCRIPTION>
#if defined( DM_PARALLEL ) && !defined( STUBMPI )
      USE module_wrf_quilt
      IMPLICIT NONE
      INTEGER, INTENT(OUT) :: retval
      retval = nio_tasks_in_group
#endif
      RETURN
END SUBROUTINE get_nio_tasks_in_group


