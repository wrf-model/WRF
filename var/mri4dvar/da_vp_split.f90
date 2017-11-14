program da_vp_split

!----------------------------------------------------------------------
! Purpose: Scatter global hires. control variables to different PEs
!
! Input     : vp_hires.bin   -- high resolution global control variables
!
! Output    : vp_XXXX        --  high resolution local control variables
!
! In order to keep the domain size, it needs to match ( n - 1 )*ratio + 1 
!
! where n  is the grid number in x or y
!       ratio is the refinement ratio between two resulotions
!
! liuz@ucar.edu , 2016-05, NCAR/MMM
!----------------------------------------------------------------------

  implicit none

  include 'mpif.h'

  integer :: i, j, k, n, status

  INTEGER :: ntasks_x, ntasks_y, mytask, mytask_x, mytask_y
  INTEGER :: new_local_comm, local_communicator
  INTEGER, DIMENSION(2) :: dims, coords
  LOGICAL, DIMENSION(2) :: isperiodic
  INTEGER ::   ids,  ide,  jds,  jde,  kds,  kde, &
               ips,  ipe,  jps,  jpe,  kps,  kpe
  INTEGER :: minx, miny
  integer :: ratio = 3

  integer :: io_status

  character (len = 255)  :: vp_hires
  character (len = 255)  :: arg       = ""
  integer, parameter :: vp_unit = 8

  integer :: ix, jy, kz

  real, dimension(:,:,:), allocatable :: v1, v2, v3, v4, v5
  real, dimension(:,:,:), allocatable :: v6, v7, v8, v9, v10, v11
  real, dimension(:,:,:), allocatable :: v1l, v2l, v3l, v4l, v5l
  real, dimension(:,:,:), allocatable :: v6l, v7l, v8l, v9l, v10l, v11l

  integer size, ierror
  integer :: cloud_cv_options ! 2 or 3 with cloud cv variables
  integer :: use_cv_w         ! =1 for w control variable

  LOGICAL :: file_exists


  !------------------------------
  ! read program arguments
  !------------------------------
  call getarg(1, arg)
  call getarg(2, arg)
  read(arg, '(i3)') cloud_cv_options

  call getarg(3, arg)
  call getarg(4, arg)
  read(arg, '(i3)') use_cv_w

  write (*, *) 'cloud_cv_options = ', cloud_cv_options, &
               'use_cv_w = ', use_cv_w

  !---------------------------------------------------------------------
  ! MPI initialization
  !---------------------------------------------------------------------
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, mytask, ierror)

  call MPASPECT( size, ntasks_x, ntasks_y, 1, 1 )
  if ( mytask == 0 ) WRITE( * , * )'Ntasks in X ',ntasks_x,', ntasks in Y ',ntasks_y

  new_local_comm = MPI_COMM_WORLD
  dims(1) = ntasks_y  ! rows
  dims(2) = ntasks_x  ! columns
  isperiodic(1) = .false.
  isperiodic(2) = .false.
  CALL mpi_cart_create( new_local_comm, 2, dims, isperiodic, .false., local_communicator, ierror )
  CALL mpi_comm_rank( local_communicator, mytask, ierror )
  CALL mpi_cart_coords( local_communicator, mytask, 2, coords, ierror )
  mytask_x = coords(2)   ! col task (x)
  mytask_y = coords(1)   ! row task (y)
  !write (*,*) "The coords of task ",mytask, " is ",mytask_x,mytask_y

  io_status = 0

  vp_hires='vp_output.global_hires'
  inquire(FILE=trim(vp_hires), EXIST=file_exists)

  if ( .not. file_exists ) then
    Write(*,*) "\nError: "//trim(vp_hires)//" not exists\n"
    call exit(-1)
  endif

  open(unit=vp_unit,file=trim(vp_hires),iostat=io_status,form='UNFORMATTED',status='OLD')
  if (io_status /= 0) then
     write(*,*) "Error ",io_status," opening vp file "//trim(vp_hires)
     call exit(-1)
  end if
  if ( mytask == 0 ) write(*,*) 'Reading vp from : '//trim(vp_hires)
  read(vp_unit) ide, jde, kde ! domain dimension (unstagered)
  ide = ide + 1 ! WRF parallel decomposition is based on stagered grid
  jde = jde + 1
  kde = kde + 1
  if ( mytask == 0 ) write(*,*) 'ide, jde, kde = ', ide, jde, kde
  ids = 1
  jds = 1
  kds = 1

  !---------------------------------------------------------------------
  ! Calculate the domain decomposition
  !---------------------------------------------------------------------
  CALL compute_memory_dims_rsl_lite ( 0 ,   &
               ids,  ide,  jds,  jde,  kds,  kde, &
               ips,  ipe,  jps,  jpe,  kps,  kpe  )
  ! convert to A-grid and middle levels on which control variables sit 
  if ( ipe == ide ) ipe = ipe - 1
  if ( jpe == jde ) jpe = jpe - 1
  if ( kpe == kde ) kpe = kpe - 1
  !WRITE(*,*)'*************************************'
  !WRITE(90,*)'local ',ips,ipe,jps,jpe,kps,kpe
  WRITE(*,*)'local ',ips,ipe,jps,jpe,kps,kpe
  !WRITE(*,*)'*************************************'

  !---------------------------------------------------------------------
  ! allocate global vp variables (unstagered)
  !---------------------------------------------------------------------
  allocate ( v1(ids:ide-1,jds:jde-1,kds:kde-1) )
  allocate ( v2(ids:ide-1,jds:jde-1,kds:kde-1) )
  allocate ( v3(ids:ide-1,jds:jde-1,kds:kde-1) )
  allocate ( v4(ids:ide-1,jds:jde-1,kds:kde-1) )
  allocate ( v5(ids:ide-1,jds:jde-1,kds:kde-1) )

 if ( cloud_cv_options >= 2 ) then
  allocate ( v6(ids:ide-1,jds:jde-1,kds:kde-1) )
  allocate ( v7(ids:ide-1,jds:jde-1,kds:kde-1) )
  allocate ( v8(ids:ide-1,jds:jde-1,kds:kde-1) )
  allocate ( v9(ids:ide-1,jds:jde-1,kds:kde-1) )
  allocate ( v10(ids:ide-1,jds:jde-1,kds:kde-1) )
 end if

 if ( use_cv_w == 1 ) allocate ( v11(ids:ide-1,jds:jde-1,kds:kde-1) )

  read(vp_unit) v1, v2, v3, v4, v5
  if ( cloud_cv_options >= 2 )read(vp_unit) v6, v7, v8, v9, v10
  if ( use_cv_w == 1 )read(vp_unit) v11
  close(vp_unit)

  call MPI_BARRIER(MPI_COMM_WORLD,ierror)
  if ( mytask == 0 ) write(*,*) 'Reading vp from : '//trim(vp_hires)//' is completeed'

  !---------------------------------------------------------------------
  ! allocate local vp variables (unstagered)
  !---------------------------------------------------------------------
  ix = ipe-ips+1
  jy = jpe-jps+1
  kz = kpe-kps+1

  allocate ( v1l(1:ix,1:jy,1:kz) )
  allocate ( v2l(1:ix,1:jy,1:kz) )
  allocate ( v3l(1:ix,1:jy,1:kz) )
  allocate ( v4l(1:ix,1:jy,1:kz) )
  allocate ( v5l(1:ix,1:jy,1:kz) )

 if ( cloud_cv_options >= 2 ) then
  allocate ( v6l(1:ix,1:jy,1:kz) )
  allocate ( v7l(1:ix,1:jy,1:kz) )
  allocate ( v8l(1:ix,1:jy,1:kz) )
  allocate ( v9l(1:ix,1:jy,1:kz) )
  allocate ( v10l(1:ix,1:jy,1:kz) )
 end if

 if ( use_cv_w == 1 ) allocate ( v11l(1:ix,1:jy,1:kz) )

  !---------------------------------------------------------------------
  ! Scatter vp to PEs
  !---------------------------------------------------------------------

  v1l(1:ix,1:jy,1:kz) = v1(ips:ipe,jps:jpe,kps:kpe)
  v2l(1:ix,1:jy,1:kz) = v2(ips:ipe,jps:jpe,kps:kpe)
  v3l(1:ix,1:jy,1:kz) = v3(ips:ipe,jps:jpe,kps:kpe)
  v4l(1:ix,1:jy,1:kz) = v4(ips:ipe,jps:jpe,kps:kpe)
  v5l(1:ix,1:jy,1:kz) = v5(ips:ipe,jps:jpe,kps:kpe)
 
 if ( cloud_cv_options >= 2 ) then
  v6l(1:ix,1:jy,1:kz) = v6(ips:ipe,jps:jpe,kps:kpe)
  v7l(1:ix,1:jy,1:kz) = v7(ips:ipe,jps:jpe,kps:kpe)
  v8l(1:ix,1:jy,1:kz) = v8(ips:ipe,jps:jpe,kps:kpe)
  v9l(1:ix,1:jy,1:kz) = v9(ips:ipe,jps:jpe,kps:kpe)
  v10l(1:ix,1:jy,1:kz) = v10(ips:ipe,jps:jpe,kps:kpe)
 end if

 if ( use_cv_w == 1 ) v11l(1:ix,1:jy,1:kz) = v11(ips:ipe,jps:jpe,kps:kpe)

  write (vp_hires,'(A,i4.4)') "vp_input.",mytask

  open(unit=vp_unit,file=trim(vp_hires),iostat=io_status,form='UNFORMATTED',status='UNKNOWN')
  if (io_status /= 0) then
     write(*,*) "Error ",io_status," opening vp file "//trim(vp_hires)
     call exit(-1)
  end if
  write(*,*) 'Writting vp on hires to : '//trim(vp_hires)
  write(vp_unit) ips, ipe, jps, jpe, kps, kpe
  write(vp_unit) v1l, v2l, v3l, v4l, v5l
  if ( cloud_cv_options >= 2 )write(vp_unit) v6l, v7l, v8l, v9l, v10l
  if ( use_cv_w == 1 )write(vp_unit) v11l
  !write(*,*) 'Sample of cvt :',mytask, maxval(cvt), minval(cvt)
  close(vp_unit)

  !---------------------------------------------------------------------
  ! The end
  !---------------------------------------------------------------------
  !if ( mytask == 0 ) then
     deallocate (v1)
     deallocate (v2)
     deallocate (v3)
     deallocate (v4)
     deallocate (v5)
     deallocate (v1l)
     deallocate (v2l)
     deallocate (v3l)
     deallocate (v4l)
     deallocate (v5l)

   if ( cloud_cv_options >= 2 ) then
     deallocate (v6)
     deallocate (v7)
     deallocate (v8)
     deallocate (v9)
     deallocate (v10)
     deallocate (v6l)
     deallocate (v7l)
     deallocate (v8l)
     deallocate (v9l)
     deallocate (v10l)
   end if

   if ( use_cv_w == 1 ) then
     deallocate (v11)
     deallocate (v11l)
   end if
  !endif

  call MPI_BARRIER(MPI_COMM_WORLD,ierror)
  if ( mytask == 0 ) Write(*,*) "Distributting control variables completed successfully"
  call MPI_FINALIZE(ierror)

contains

   SUBROUTINE MPASPECT( P, MINM, MINN, PROCMIN_M, PROCMIN_N )
      IMPLICIT NONE
      INTEGER P, M, N, MINI, MINM, MINN, PROCMIN_M, PROCMIN_N, ierror
      MINI = 2*P
      MINM = 1
      MINN = P
      DO M = 1, P
        IF ( MOD( P, M ) .EQ. 0 ) THEN
          N = P / M
          IF ( ABS(M-N) .LT. MINI                &
               .AND. M .GE. PROCMIN_M            &
               .AND. N .GE. PROCMIN_N            &
             ) THEN
            MINI = ABS(M-N)
            MINM = M
            MINN = N
          ENDIF
        ENDIF
      ENDDO
      IF ( MINM .LT. PROCMIN_M .OR. MINN .LT. PROCMIN_N ) THEN
        WRITE( * , * )'MPASPECT: UNABLE TO GENERATE PROCESSOR MESH.  STOPPING.'
        WRITE( * , * )' PROCMIN_M ', PROCMIN_M
        WRITE( * , * )' PROCMIN_N ', PROCMIN_N
        WRITE( * , * )' P         ', P
        WRITE( * , * )' MINM      ', MINM
        WRITE( * , * )' MINN      ', MINN
        call MPI_FINALIZE(ierror)
        stop
      ENDIF
   RETURN
   END SUBROUTINE MPASPECT

  SUBROUTINE compute_memory_dims_rsl_lite  (      &
                   shw ,                          &
                   ids,  ide,  jds,  jde,  kds,  kde, &
                   ips,  ipe,  jps,  jpe,  kps,  kpe )

    IMPLICIT NONE
    INTEGER, INTENT(IN)               ::  shw
    INTEGER, INTENT(IN)     ::  ids, ide, jds, jde, kds, kde
    INTEGER, INTENT(OUT)    ::  ips, ipe, jps, jpe, kps, kpe

    INTEGER Px, Py, P, i, j, k, ierr

! xy decomposition

    ips = -1
    j = jds
    ierr = 0
    DO i = ids, ide
       CALL task_for_point ( i, j, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py, &
                             minx, miny, ierr )
       IF ( ierr .NE. 0 ) stop 'error code returned by task_for_point '
       IF ( Px .EQ. mytask_x ) THEN
          ipe = i
          IF ( ips .EQ. -1 ) ips = i
       ENDIF
    ENDDO
    ! handle setting the memory dimensions where there are no X elements assigned to this proc
    IF (ips .EQ. -1 ) THEN
       ipe = -1
       ips = 0
    ENDIF
    jps = -1
    i = ids
    ierr = 0
    DO j = jds, jde
       CALL task_for_point ( i, j, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py, &
                             minx, miny, ierr )
       IF ( ierr .NE. 0 ) stop 'error code returned by task_for_point '
       IF ( Py .EQ. mytask_y ) THEN
          jpe = j
          IF ( jps .EQ. -1 ) jps = j
       ENDIF
    ENDDO
    ! handle setting the memory dimensions where there are no Y elements assigned to this proc
    IF (jps .EQ. -1 ) THEN
       jpe = -1
       jps = 0
    ENDIF

!begin: wig; 12-Mar-2008
! This appears redundant with the conditionals above, but we get cases with only
! one of the directions being set to "missing" when turning off extra processors.
! This may break the handling of setting only one of nproc_x or nproc_y via the namelist.
    IF (ipe .EQ. -1 .or. jpe .EQ. -1) THEN
       ipe = -1
       ips = 0
       jpe = -1
       jps = 0
    ENDIF
!end: wig; 12-Mar-2008

! extend the patch dimensions out shw along edges of domain
    IF ( ips < ipe .and. jps < jpe ) THEN           !wig; 11-Mar-2008
       IF ( mytask_x .EQ. 0 ) THEN
          ips = ips - shw
       ENDIF
       IF ( mytask_x .EQ. ntasks_x-1 ) THEN
          ipe = ipe + shw
       ENDIF
       IF ( mytask_y .EQ. 0 ) THEN
          jps = jps - shw
       ENDIF
       IF ( mytask_y .EQ. ntasks_y-1 ) THEN
          jpe = jpe + shw
       ENDIF
    ENDIF                                           !wig; 11-Mar-2008

    kps = 1
    kpe = kde-kds+1

  END SUBROUTINE compute_memory_dims_rsl_lite

end program da_vp_split
