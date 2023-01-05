! MPI functions for lagrangian transport in Module Firebrand Spotting
! 
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
! ATTENTION TO:
! Capitals vs. lower-case: In FORTRAN words use CAPITAL case
! Indent by 4 spaces 
! Operators: use >, <, >=, <=, == (instead of .GT., .EQ., .GE. etc)
! Ordering of DO loops for fast memory access: 
!     The innermost loop goes over the 1st array dimension (fastest changing dimension)
!     do j = 1, Jend; do k = 1, Kend; do i = 1, Iend; A(i, k, j); end do; end do; end do
!-------------------------------------------------------------------------------

MODULE module_firebrand_spotting_mpi

    USE module_domain,       ONLY : get_ijk_from_grid, domain               ! grid
    USE module_configure,    ONLY : grid_config_rec_type                    ! config_flags
    !USE module_symbols_util, ONLY : WRFU_TimeInterval, WRFU_TimeIntervalGet, WRFU_TimeIntervalSet
#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
    USE MPI 
#endif
    IMPLICIT NONE

    PRIVATE

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
    PUBLIC &
        ! Module Variables:
        neighbors, my_id, task_id, mpiprocs, &
        left_id, right_id, up_id, down_id, &
        upleft_id, upright_id, downleft_id, downright_id, &
        ! Functions:
        fs_mpi_recvfrompatch_real, &
        fs_mpi_recvfrompatch_int, &
        fs_mpi_recvbuffsize, &
        fs_mpi_recvbuff1_real, &
        fs_mpi_recvbuff1_int, &
        fs_mpi_checkreceive, &
        ! Subroutines:
        fs_mpi_send2neighbors, & ! (task_id, mask, p_x, p_y, p_z, fs_p_m, fs_p_d, fs_p_e, fs_p_t, fs_p_v, p_id, p_dt)
        fs_mpi_init, &
        fs_mpi_nothing2send, &
        fs_mpi_recv, & ! (np_id, task_id, r_x, r_y, r_z, r_p_m, r_p_d, r_p_e, r_p_t, r_p_v, r_id, r_dt)
        fs_mpi_sendbuff_real, &
        fs_mpi_sendbuff_int, &
        fs_mpi_sendbuffsize, &
        fs_mpi_sendbuff1_real, &
        fs_mpi_sendbuff1_int
#endif

    ! THESE VARIABLES ARE IN MODULE SCOPE ! Careful with reassignments - don't reassign
    ! SAVE attribute is default

    !-------------------------------------------------------------------------------
    ! variables in module scope: private, only available module-wide (host association)
    !-------------------------------------------------------------------------------
    ! They should not be declared again in suboutines (may not compile)
    !  and must not be among routine dummy arguments. Consequently, cannot be IO variables
    !
    ! Runtime variables are not available at module level (e.g., namelist, tile dependent variables).
    ! Include here only what can be set during compilation:
    ! fixed parameters, allocatables, declarions (without initialization)

    !-------------------------------------------------------------------------------
    ! Fixed parameters ***MODULE SCOPE***
    !-------------------------------------------------------------------------------

#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
    INTEGER, PARAMETER :: wrfdbg = 0    

    INTEGER, PARAMETER :: dp = KIND(0.d0)      ! double precision
    REAL,    PARAMETER :: ZERO_dp = 0.0_dp ! this is a real type variable, not a double precision type
    REAL,    PARAMETER :: dp05 = 0.5_dp
    REAL,    PARAMETER :: dp1 = 1.0_dp

    !-------------------------------------------------------------------------------
    ! Generic variables for multiple use within module ***MODULE SCOPE***
    !-------------------------------------------------------------------------------

    CHARACTER (LEN=200), SAVE     :: msg
    CHARACTER (LEN=256), SAVE     :: fmt
    CHARACTER (LEN=200), DIMENSION(10) :: amsg
    INTEGER, SAVE :: imsg ! loop counters

    !-------------------------------------------------------------------------------
    ! MPI variables - Move particles between tiles - ***MODULE SCOPE***
    !-------------------------------------------------------------------------------

    INTEGER, PARAMETER :: neighbors = 8 ! number of neighbor tasks - includes diagonals
    INTEGER, PARAMETER :: mpi_datapack_nreal = 8 ! number of real type arrays packed together: 3 (xyz) + 5 (p_properties)
    INTEGER, PARAMETER :: mpi_datapack_nint = 3  ! number of integer type arrays packed together 2 (id, src, dt)

    INTEGER, SAVE :: my_id, left_id, right_id, up_id, down_id
    INTEGER, SAVE :: upleft_id, upright_id, downleft_id, downright_id
    INTEGER, SAVE, DIMENSION(neighbors) :: task_id

    INTEGER, SAVE :: mpiprocs = 0
    !-------------------------------------------------------------------------------
    ! grid and cf are not here because dimensions are given at runtime (derived types)
    ! grid values change at every interaction,
    ! therefore, it needs to be a dummy argument
    !-------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------
    ! Variable bounds - Initialized in init, used in dummy arguments in driver 
    ! ***MODULE SCOPE***
    !-------------------------------------------------------------------------------
    INTEGER, SAVE :: ids, jds, ide, jde, kde      ! domain bounds
    INTEGER, SAVE :: ims, jms, ime, jme, kms, kme ! memory bounds
    INTEGER, SAVE :: is, ie, js, je, ks, ke       ! patch start/end 
    INTEGER, SAVE :: ifps, jfps, ifpe, jfpe       ! refined fire grid bounds
#endif

CONTAINS
#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
!=============================================================
!=============================================================


!******************************************************************
!******************************************************************
!*                                                                *
!*                         MPI Routines                           *
!*                                                                *
!******************************************************************
!******************************************************************

!=============================================================
    FUNCTION fs_mpi_recvfrompatch_real(bsz, fromid) RESULT (buff)
!=============================================================

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: bsz, fromid
        REAL,    DIMENSION(bsz) :: buff ! p_x, p_y, p_z  
        INTEGER :: ierr, recvtag, ii
        INTEGER :: stat(MPI_STATUS_SIZE)

        !-------------------------------------------------------------------------------
        ! Receive a real type array of size bsz
        !-------------------------------------------------------------------------------

        buff = ZERO_dp
        recvtag = 2000 + fromid ! 2000: tag real type data
        CALL mpi_recv(buff, bsz, MPI_FLOAT, fromid, recvtag, MPI_COMM_WORLD, stat, ierr)

    END FUNCTION fs_mpi_recvfrompatch_real
!=============================================================
!=============================================================


!=============================================================
    FUNCTION fs_mpi_recvfrompatch_int(bsz, fromid) RESULT (buff)
!=============================================================

        IMPLICIT NONE
        INTEGER, INTENT(IN) :: bsz, fromid
        INTEGER, DIMENSION(bsz) :: buff  
        INTEGER :: ierr, recvtag, ii
        INTEGER :: stat(MPI_STATUS_SIZE)

        !-------------------------------------------------------------------------------
        ! Receive an int type array of size bsz
        !-------------------------------------------------------------------------------

        buff = 0
        recvtag = 3000 + fromid ! 3000: tag int type data
        CALL mpi_recv(buff, bsz, MPI_INTEGER, fromid, recvtag, MPI_COMM_WORLD, stat, ierr)

    END FUNCTION fs_mpi_recvfrompatch_int
!=============================================================
!=============================================================


!=============================================================
    FUNCTION fs_mpi_recvbuffsize(fromid) RESULT(recvbuffsz)
!=============================================================

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: fromid
        INTEGER :: recvbuffsz
        INTEGER :: ierr, recvtag, sz , tag
        INTEGER :: stat(MPI_STATUS_SIZE)

        !-------------------------------------------------------------------------------
        ! Receive the buffer size (zero or nbr)
        !-------------------------------------------------------------------------------

        recvbuffsz = 0
        sz = 1  ! one value corresponding to nbr (must send a number, receive is blocking)
        tag = 1000 ! tag for communicating nbr 
        recvtag = tag + fromid
        ierr = 0

        !-------------------------------------------------------------------------------
        IF (fromid > -1) THEN 
            CALL mpi_recv(recvbuffsz, sz, MPI_INTEGER, fromid, recvtag, MPI_COMM_WORLD, stat, ierr)
        ENDIF

        END FUNCTION fs_mpi_recvbuffsize
!=============================================================
!=============================================================

!=============================================================
    FUNCTION fs_mpi_recvbuff1_real(fromid) RESULT(recvbuffsz)
!=============================================================

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: fromid
        REAL    :: recvbuffsz
        INTEGER :: ierr, recvtag, sz , tag
        INTEGER :: stat(MPI_STATUS_SIZE)

        !-------------------------------------------------------------------------------
        ! Receive a real type scalar
        !-------------------------------------------------------------------------------

        recvbuffsz = 0
        sz = 1  ! one value corresponding to nbr (must send a number, receive is blocking)
        tag = 4000 ! tag for communicating nbr 
        recvtag = tag + fromid
        ierr = 0

        !-------------------------------------------------------------------------------
        !IF (fromid > -1) THEN 
            CALL mpi_recv(recvbuffsz, sz, MPI_FLOAT, fromid, recvtag, MPI_COMM_WORLD, stat, ierr)           
        !ENDIF

        END FUNCTION fs_mpi_recvbuff1_real
!=============================================================
!=============================================================


!=============================================================
    FUNCTION fs_mpi_recvbuff1_int(fromid) RESULT(recvbuffsz)
!=============================================================

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: fromid
        INTEGER :: recvbuffsz
        INTEGER :: ierr, recvtag, sz , tag
        INTEGER :: stat(MPI_STATUS_SIZE)

        !-------------------------------------------------------------------------------
        ! Receive a real type scalar
        !-------------------------------------------------------------------------------

        recvbuffsz = 0
        sz = 1  ! one value corresponding to nbr (must send a number, receive is blocking)
        tag = 5000 ! tag for communicating nbr 
        recvtag = tag + fromid
        ierr = 0

        !-------------------------------------------------------------------------------
        !IF (fromid > -1) THEN 
            CALL mpi_recv(recvbuffsz, sz, MPI_INTEGER, fromid, recvtag, MPI_COMM_WORLD, stat, ierr)           
        !ENDIF

        END FUNCTION fs_mpi_recvbuff1_int
!=============================================================
!=============================================================



!=============================================================
    SUBROUTINE fs_mpi_send2neighbors(task_id, mask, p_x, p_y, p_z, p_id, p_src, p_dt, fs_p_m, fs_p_d, fs_p_e, fs_p_t, fs_p_v)
!=============================================================

        IMPLICIT NONE

        INTEGER, PARAMETER :: np = neighbors ! number or neighbor tasks
        INTEGER, PARAMETER :: nreal = mpi_datapack_nreal ! number of real type arrays packed together
        INTEGER, PARAMETER :: nint  = mpi_datapack_nint  ! number of integer type arrays packed together

        INTEGER, INTENT(IN), DIMENSION(:) :: task_id
        LOGICAL, INTENT(IN), DIMENSION(:) :: mask
        INTEGER, INTENT(IN), DIMENSION(:) :: p_id, p_dt, p_src
        REAL,    INTENT(IN), DIMENSION(:) :: p_x, p_y, p_z, fs_p_m, fs_p_d, fs_p_e, fs_p_t, fs_p_v

        LOGICAL, ALLOCATABLE, DIMENSION(:) :: masksendto
        LOGICAL, ALLOCATABLE, DIMENSION(:) :: ml, mr, mu, md 
        INTEGER, ALLOCATABLE, DIMENSION(:) :: p_int
        REAL,    ALLOCATABLE, DIMENSION(:) :: p_real

        INTEGER :: ierr, nbr, ii, sendto, k

        !-------------------------------------------------------------------------------

        !task_id = [left_id, right_id, up_id, down_id, upleft_id, upright_id, downleft_id, downright_id]
        ALLOCATE(masksendto(SIZE(mask)), ml(SIZE(mask)), mr(SIZE(mask)), mu(SIZE(mask)), md(SIZE(mask)))

        ml = .FALSE.
        mr = .FALSE.
        mu = .FALSE.
        md = .FALSE.

        ml = (FLOOR(p_x) < is) ! MASK LEFT
        mr = (FLOOR(p_x) > ie) ! MASK RIGHT
        mu = (FLOOR(p_y) > je) ! MASK UP  
        md = (FLOOR(p_y) < js) ! MASK DONW
        
        !-------------------------------------------------------------------------------
        ! Send to adjacent patch boundaries
        !-------------------------------------------------------------------------------

        DO ii=1,np
            sendto = task_id(ii)

            masksendto = .FALSE.

            IF (sendto > -1) THEN 
                IF (sendto == left_id)  masksendto = ((mask .AND. ml) .AND. (.NOT. ( md .OR. mu) )) ! LEFT
                IF (sendto == right_id) masksendto = ((mask .AND. mr) .AND. (.NOT. ( md .OR. mu) )) ! RIGHT
                IF (sendto == up_id)    masksendto = ((mask .AND. mu) .AND. (.NOT. ( ml .OR. mr) )) ! UP
                IF (sendto == down_id)  masksendto = ((mask .AND. md) .AND. (.NOT. ( ml .OR. mr) )) ! DONW

                IF (sendto == upleft_id)    masksendto = (mask .AND. (mu .AND. ml) ) ! UPLEFT
                IF (sendto == upright_id)   masksendto = (mask .AND. (mu .AND. mr) ) ! UPRIGHT
                IF (sendto == downleft_id)  masksendto = (mask .AND. (md .AND. ml) ) ! DOWNLEFT
                IF (sendto == downright_id) masksendto = (mask .AND. (md .AND. mr) ) ! DOWNRIGHT

                nbr = COUNT(masksendto)

                IF (nbr == 0) &
                    CALL fs_mpi_nothing2send(sendto=sendto)

                IF (nbr > 0) THEN

                    ! WRITE (msg,'(2(i6,1x))') sendto, nbr
                    ! CALL wrf_debug (wrfdbg, 'SPFire_mpi_sendaway sendto_id nbr:  '//msg)

                    ALLOCATE(p_real(nreal*nbr), p_int(nint*nbr)) 
                    
                    p_real = [PACK(p_x,masksendto),&
                               PACK(p_y,masksendto),&
                               PACK(p_z,masksendto),&
                               PACK(fs_p_m,masksendto),&
                               PACK(fs_p_d,masksendto),&
                               PACK(fs_p_e,masksendto),&
                               PACK(fs_p_t,masksendto),&
                               PACK(fs_p_v,masksendto)]
                    p_int  = [PACK(p_id,masksendto),&
                              PACK(p_src,masksendto),&
                               PACK(p_dt,masksendto)]

                    CALL fs_mpi_sendbuffsize(sendto=sendto, nbr=nbr)
                    CALL fs_mpi_sendbuff_real(sendto=sendto, bsz=nbr*nreal, buff=p_real)
                    CALL fs_mpi_sendbuff_int(sendto=sendto, bsz=nbr*nint, buff=p_int)

                    ! DO k=1,MIN(nbr,5)
                    !     WRITE(msg,'(3(i6,1x),6(f12.6,1x))') sendto, p_int(k), p_int(k+nbr), p_real(k), p_real(k+nbr), p_real(k+2*nbr), p_real(k+4*nbr), p_real(k+6*nbr), p_real(k+7*nbr)
                    !     CALL wrf_debug (wrfdbg, 'SPFire_mpi send  >>> '// msg)
                    ! ENDDO


                    DEALLOCATE(p_real, p_int) 
                ENDIF
            ENDIF
        ENDDO

        DEALLOCATE(masksendto, ml, mr, mu, md)

!=============================================================
    END SUBROUTINE fs_mpi_send2neighbors
!=============================================================




!=============================================================
    SUBROUTINE fs_mpi_init(grid)
!=============================================================

        USE module_dm, ONLY : ntasks_x, ntasks_y, mytask_x, mytask_y ! total tasks in x,y dimensions, this task i,j 

        IMPLICIT NONE
        INCLUDE "mpif.h"

        TYPE(domain), INTENT(IN)    :: grid ! input data **Note: Intent IN**

        INTEGER :: ierr, numprocs
        LOGICAL :: mpi_inited
        CHARACTER (LEN=10) :: envval

        my_id    = -1 
        left_id  = -1 
        right_id = -1 
        up_id    = -1 
        down_id  = -1
        upleft_id   = -1
        upright_id  = -1
        downleft_id = -1
        downright_id = -1

        CALL MPI_INITIALIZED( mpi_inited, ierr)

        IF ( .NOT. mpi_inited ) &
            CALL wrf_error_fatal( "failed to initialize MPI")

        !-------------------------------------------------------------------------------
        ! Who am I ?
        !-------------------------------------------------------------------------------

        CALL MPI_COMM_RANK( MPI_COMM_WORLD, my_id, ierr)
        CALL MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr)
        mpiprocs = numprocs ! mpiprocs and my_id are in module scope

        WRITE (msg,'(2(i6,1x))') my_id, numprocs
        CALL wrf_debug (wrfdbg, 'SPFire_mpi mpi initialized. myid, nproc: '//msg)

        ! WRITE (msg,'(4(i6,1x))') ntasks_x, ntasks_y, mytask_x, mytask_y
        ! CALL wrf_debug (wrfdbg, 'SPFire_mpi tasks: '//msg)

        ! WRITE (msg,'(5(i9,1x))') my_id, is, ie, js, je
        ! CALL wrf_debug (wrfdbg, 'SPFire_mpi my_id, tile bounds: '//msg)

        ! CALL get_environment_variable ("WRF_NUM_TILES_X",envval, status=ierr)
        ! WRITE (msg,'(1(a10,1x))') envval
        ! CALL wrf_debug (wrfdbg, 'SPFire_mpi: '//msg)

        !-------------------------------------------------------------------------------
        ! Who are my neighbors? all neighbors *_id are declared in module scope
        !-------------------------------------------------------------------------------

        ! get neighbors  (-1 at domain bounds)
        down_id = my_id - ntasks_x
        up_id =   my_id + ntasks_x 
        IF( mytask_y == 0) down_id = -1
        IF( mytask_y == (ntasks_y-1) ) up_id = -1

        downleft_id  = down_id - 1
        downright_id = down_id + 1
        upleft_id    = up_id - 1
        upright_id   = up_id + 1

        IF (down_id == -1) downleft_id  = -1
        IF (down_id == -1) downright_id = -1
        IF (up_id == -1)   upleft_id    = -1
        IF (up_id == -1)   upright_id   = -1

        left_id = my_id - 1 
        right_id = my_id + 1 
        IF( mytask_x == 0) left_id = -1
        IF( mytask_x == (ntasks_x-1) ) right_id =-1

        IF (left_id == -1)  downleft_id  = -1
        IF (left_id == -1)  upleft_id    = -1
        IF (right_id == -1) downright_id = -1
        IF (right_id == -1) upright_id   = -1
        
        ! WRITE (msg,'(5(i6,1x))') my_id, left_id, right_id, up_id, down_id
        ! CALL wrf_debug (wrfdbg, 'SPFire_mpi   id, L, R, U, D:     '//msg)

        ! WRITE (msg,'(4(i6,1x))') downleft_id, downright_id, upleft_id, upright_id
        ! CALL wrf_debug (wrfdbg, 'SPFire_mpi diag, DL, DR, UL, UR: '//msg)

        ! task_id is in module scope
        task_id = [left_id, right_id, up_id, down_id, upleft_id, upright_id, downleft_id, downright_id]


        ! row and column of the current process within the domain:
        ! left_right ( 0 : ntasks_x -1 )
        ! up_down    ( 0 : ntasks_y    -1 )

        ! my_task_i = MOD(my_id , ntasks_x)
        ! my_task_j = my_id / ntasks_x

        !-------------------------------------------------------------------------------
        ! Set bounds for finding the tiles to send and receive 
        ! *** variables declared in MODULE SCOPE ***
        !-------------------------------------------------------------------------------

        CALL get_local_ijk(grid, & 
                           ips=is,  jps=js,  ipe=ie,  jpe=je,  kps=ks,  kpe=ke)        

        ! WRITE (msg,'(6(i6,1x))') is, ie, js, je, ks, ke
        ! CALL wrf_debug (wrfdbg, 'SPFire_mpi_init tile bounds: '//msg)

    END SUBROUTINE fs_mpi_init
!=============================================================
!=============================================================



!=============================================================
    SUBROUTINE fs_mpi_nothing2send(sendto)
!=============================================================

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: sendto 
        INTEGER :: ierr, tag, nbr, sz

        !-------------------------------------------------------------------------------
        ! Send a signal with value zero
        !-------------------------------------------------------------------------------

        sz = 1 ! one value corresponding to nbr (must send a number, receive is blocking)
        nbr = 0
        tag = 1000 + my_id ! tag for communicating nbr

        IF (sendto > -1) THEN             
            CALL mpi_send(nbr, sz, MPI_INTEGER, sendto, tag, MPI_COMM_WORLD, ierr)
        ENDIF

    END SUBROUTINE fs_mpi_nothing2send

!=============================================================
!=============================================================


!=============================================================
    FUNCTION fs_mpi_checkreceive(task_list, np) RESULT(buffsz)
!=============================================================

        IMPLICIT NONE

        INTEGER  :: np 
        INTEGER, DIMENSION(np) :: buffsz
        INTEGER, INTENT(IN), DIMENSION(:) :: task_list
        INTEGER :: ii, tmp2

        buffsz(:) = 0

        !-------------------------------------------------------------------------------
        ! Anything to receive from any tile?
        !-------------------------------------------------------------------------------

        DO ii=1,np
            tmp2 = 0
            tmp2 = fs_mpi_recvbuffsize(fromid=task_list(ii))
            buffsz(ii) = tmp2
        ENDDO

    END FUNCTION fs_mpi_checkreceive

!=============================================================
!=============================================================


!=============================================================
        SUBROUTINE fs_mpi_recv(np_id, task_id, r_x, r_y, r_z, r_p_m, r_p_d, r_p_e, r_p_t, r_p_v, r_id, r_src, r_dt)
!=============================================================

        IMPLICIT NONE

        INTEGER, PARAMETER :: nreal = mpi_datapack_nreal ! number of real type arrays packed together
        INTEGER, PARAMETER :: nint  = mpi_datapack_nint  ! number of integer type arrays packed together
        INTEGER, PARAMETER :: np = neighbors

        INTEGER, INTENT(IN), DIMENSION(:) :: task_id, np_id
        REAL,    INTENT(OUT),DIMENSION(:) :: r_x, r_y, r_z
        INTEGER, INTENT(OUT),DIMENSION(:) :: r_id, r_dt, r_src
        REAL,    INTENT(OUT),DIMENSION(:) :: r_p_m, r_p_d, r_p_e, r_p_t, r_p_v

        INTEGER :: np_sum, istart, iend, ii
        INTEGER, ALLOCATABLE, DIMENSION(:,:) :: arr_int
        REAL,    ALLOCATABLE, DIMENSION(:,:) :: arr_real

        !-------------------------------------------------------------------------------

        np_sum = SUM(np_id)
        ALLOCATE(arr_real(np_sum,nreal))
        ALLOCATE(arr_int(np_sum, nint))
        
        arr_real = ZERO_dp
        arr_int  = 0

        ! WRITE (msg,'(8(i4,1x))') (np_id(ii), ii=1,np)
        ! CALL wrf_debug (wrfdbg, 'SPFire_mpi_recv_all np_id  >>> '// msg)
        ! WRITE (msg,'(8(i4,1x))') (task_id(ii), ii=1,np)
        ! CALL wrf_debug (wrfdbg, 'SPFire_mpi_recv_all task_id >>> '// msg)

        istart = 1
        DO ii=1,np

            IF (np_id(ii) > 0) THEN 

                iend = istart + np_id(ii) -1

                ! WRITE (msg,'(4(i4,1x))') ii, np_id(ii), istart, iend
                ! CALL wrf_debug (wrfdbg, 'SPFire_mpi recv all id, nbr, istart:iend >>> '// msg)
!                 WRITE (msg,*) SHAPE(arr_real)
!                 CALL wrf_debug (wrfdbg, 'SPFire_mpi_recv_all shape arr_real >>> '// msg)

                arr_real(istart:iend,:) = RESHAPE( &
                    fs_mpi_recvfrompatch_real(bsz=np_id(ii)*nreal, fromid=task_id(ii)), &
                                                         [np_id(ii),nreal])

                arr_int(istart:iend,:)  = RESHAPE( &
                    fs_mpi_recvfrompatch_int(bsz=np_id(ii)*nint, fromid=task_id(ii)), &
                                                         [np_id(ii), nint])

                istart = istart + np_id(ii)

            ENDIF
        ENDDO

        r_x = arr_real(:,1)
        r_y = arr_real(:,2)
        r_z = arr_real(:,3)

        r_p_m = arr_real(:,4)
        r_p_d = arr_real(:,5)
        r_p_e = arr_real(:,6)
        r_p_t = arr_real(:,7)
        r_p_v = arr_real(:,8)

        r_id = arr_int(:,1)
        r_src = arr_int(:,2)
        r_dt = arr_int(:,3)

    END SUBROUTINE fs_mpi_recv

!=============================================================
!=============================================================



!=============================================================
    SUBROUTINE fs_mpi_sendbuff_real(bsz, sendto, buff)
!=============================================================

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: bsz, sendto 
        REAL,    INTENT(IN), DIMENSION(bsz) :: buff ! p_x, p_y, p_z
        INTEGER :: ierr, tag

        !-------------------------------------------------------------------------------
        ! Send the type-real buffer:
        ! a 1-d packed array composed of "nreal" flattened arrays, each of size "nbr"
        !-------------------------------------------------------------------------------

        ierr = 0
        tag = 2000 + my_id ! 2000: tag real type data
        CALL mpi_send(buff, bsz, MPI_FLOAT, sendto, tag, MPI_COMM_WORLD, ierr)

    END SUBROUTINE fs_mpi_sendbuff_real
!=============================================================

!=============================================================



!=============================================================
    SUBROUTINE fs_mpi_sendbuff_int(bsz, sendto, buff)
!=============================================================

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: bsz, sendto 
        INTEGER, INTENT(IN), DIMENSION(bsz) :: buff  ! p_id, p_dt
        INTEGER :: ierr, tag

        !-------------------------------------------------------------------------------
        ! Send the integer buffer:
        ! a 1-d packed array composed of "nint" flattened arrays, each of size "nbr"
        !-------------------------------------------------------------------------------

        ierr = 0
        tag = 3000 + my_id ! 3000: tag int type data
        CALL mpi_send(buff, bsz, MPI_INTEGER, sendto, tag, MPI_COMM_WORLD, ierr)

    END SUBROUTINE fs_mpi_sendbuff_int
!=============================================================
!=============================================================



!=============================================================
    SUBROUTINE fs_mpi_sendbuffsize(nbr, sendto)
!=============================================================

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: nbr
        INTEGER, INTENT(IN) :: sendto 
        INTEGER :: ierr, tag, sz

        !-------------------------------------------------------------------------------
        ! Send an integer scalar or 
        ! Send the buffer size for an incoming array:
        !    the number of elements in each array (real or int) that will be packed into one data buffer
        !    and sent over by fs_mpi_sendbuff_real/int
        !-------------------------------------------------------------------------------

        ierr = 0
        sz = 1 ! one value corresponding to nbr (must send a number, receive is blocking)
        tag = 1000+my_id ! tag for communicating nbr 

        CALL mpi_send(nbr, sz, MPI_INTEGER, sendto, tag, MPI_COMM_WORLD, ierr)

    END SUBROUTINE fs_mpi_sendbuffsize
!=============================================================
!=============================================================



!=============================================================
    SUBROUTINE fs_mpi_sendbuff1_real(nbr, sendto)
!=============================================================

        IMPLICIT NONE

        REAL,    INTENT(IN) :: nbr
        INTEGER, INTENT(IN) :: sendto 
        INTEGER :: ierr, tag, sz

        !-------------------------------------------------------------------------------
        ! Send a real type scalar
        !-------------------------------------------------------------------------------

        ierr = 0
        sz = 1 ! one value corresponding to nbr (must send a number, receive is blocking)
        tag = 4000+my_id ! tag for communicating nbr 

        CALL mpi_send(nbr, sz, MPI_FLOAT, sendto, tag, MPI_COMM_WORLD, ierr)

    END SUBROUTINE fs_mpi_sendbuff1_real
!=============================================================
!=============================================================


!=============================================================
    SUBROUTINE fs_mpi_sendbuff1_int(nbr, sendto)
!=============================================================

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: nbr
        INTEGER, INTENT(IN) :: sendto 
        INTEGER :: ierr, tag, sz

        !-------------------------------------------------------------------------------
        ! Send an integer scalar or 
        ! Send the buffer size for an incoming array:
        !    the number of elements in each array (real or int) that will be packed into one data buffer
        !    and sent over by fs_mpi_sendbuff_real/int
        !-------------------------------------------------------------------------------

        ierr = 0
        sz = 1 ! one value corresponding to nbr (must send a number, receive is blocking)
        tag = 5000+my_id ! tag for communicating nbr 

        CALL mpi_send(nbr, sz, MPI_INTEGER, sendto, tag, MPI_COMM_WORLD, ierr)

    END SUBROUTINE fs_mpi_sendbuff1_int

!=============================================================
!=============================================================

!=============================================================
    SUBROUTINE get_local_ijk(grid, ips, ipe, jps, jpe, kps, kpe)
!=============================================================

        USE module_domain, ONLY: get_ijk_from_grid

        IMPLICIT NONE

        TYPE(DOMAIN), INTENT(IN) :: grid 
        INTEGER,      INTENT(OUT), OPTIONAL :: ips, ipe, jps, jpe, kps, kpe

        INTEGER :: iips, iipe, jjps, jjpe, kkps, kkpe
        INTEGER :: iims, iime, jjms, jjme, kkms, kkme
        INTEGER :: iids, iide, jjds, jjde, kkds, kkde


        IF (&
            (.NOT. PRESENT(ips)) .AND. &
            (.NOT. PRESENT(jps)) .AND. &
            (.NOT. PRESENT(kps)) .AND. &
            (.NOT. PRESENT(ipe)) .AND. &
            (.NOT. PRESENT(jpe)) .AND. &
            (.NOT. PRESENT(kpe))) &
            CALL wrf_debug ( 1, 'get_local_ijk function is NOT requesting a result' )

        CALL get_ijk_from_grid (  grid ,        &
            iids, iide, jjds, jjde, kkds, kkde, &
            iims, iime, jjms, jjme, kkms, kkme, &
            iips, iipe, jjps, jjpe, kkps, kkpe)

        IF (PRESENT(ips)) ips = iips
        IF (PRESENT(jps)) jps = jjps
        IF (PRESENT(kps)) kps = kkps
        IF (PRESENT(ipe)) ipe = iipe
        IF (PRESENT(jpe)) jpe = jjpe
        IF (PRESENT(kpe)) kpe = kkpe

    END SUBROUTINE get_local_ijk

!=============================================================
!=============================================================
#endif
END MODULE module_firebrand_spotting_mpi

