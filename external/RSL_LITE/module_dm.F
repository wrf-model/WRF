#if NMM_CORE==1
#define copy_fcnm UpNear
#define copy_fcn UpCopy
#define interp_fcn DownCopy
#define copy_fcni UpINear
#endif

#define NEST_FULL_INFLUENCE(A,B) A=B
MODULE module_dm

   USE module_machine
   USE module_wrf_error
   USE module_driver_constants
!   USE module_comm_dm
#if ( DA_CORE != 1 )
   USE module_cpl, ONLY : coupler_on, cpl_init
#endif

   IMPLICIT NONE
#ifndef STUBMPI
   INCLUDE 'mpif.h'
#else
   INTEGER, PARAMETER :: MPI_UNDEFINED = -1
#endif

#if ( NMM_CORE == 1 ) || ( WRF_CHEM == 1 )
   INTEGER, PARAMETER :: max_halo_width = 6
#else
   INTEGER, PARAMETER :: max_halo_width = 6 ! 5
#endif

   INTEGER :: ips_save, ipe_save, jps_save, jpe_save, itrace
   INTEGER :: lats_to_mic, minx, miny

   INTEGER :: communicator_stack_cursor = 0
   INTEGER :: current_id  = 1
   INTEGER, DIMENSION(max_domains) ::  ntasks_stack, ntasks_y_stack          &
                                     , ntasks_x_stack, mytask_stack          &
                                     , mytask_x_stack, mytask_y_stack        &
                                     , id_stack                            
   INTEGER, DIMENSION(max_domains) ::  ntasks_store, ntasks_y_store          &
                                     , ntasks_x_store, mytask_store          &
                                     , mytask_x_store, mytask_y_store      
   INTEGER ntasks, ntasks_y, ntasks_x, mytask, mytask_x, mytask_y

   INTEGER, DIMENSION(max_domains) :: local_communicator_stack, local_communicator_periodic_stack &
                                     ,local_iocommunicator_stack                                  &
                                     ,local_communicator_x_stack, local_communicator_y_stack
   INTEGER, DIMENSION(max_domains) :: local_communicator_store, local_communicator_periodic_store &
                                     ,local_iocommunicator_store                                  &
                                     ,local_communicator_x_store, local_communicator_y_store

   INTEGER :: mpi_comm_allcompute         = MPI_UNDEFINED
   INTEGER :: local_communicator          = MPI_UNDEFINED
   INTEGER :: local_communicator_periodic = MPI_UNDEFINED
   INTEGER :: local_iocommunicator        = MPI_UNDEFINED
   INTEGER :: local_communicator_x        = MPI_UNDEFINED
   INTEGER :: local_communicator_y        = MPI_UNDEFINED ! subcommunicators for rows and cols of mesh
   INTEGER :: local_quilt_comm            = MPI_UNDEFINED ! added 20151212 jm
   LOGICAL :: dm_debug_flag = .FALSE.
! for parallel nesting, 201408, jm
   INTEGER intercomm_to_mom( max_domains ), intercomm_to_kid( max_nests, max_domains )
   INTEGER mpi_comm_to_mom( max_domains ), mpi_comm_to_kid( max_nests, max_domains )
   INTEGER which_kid(max_domains), nkids(max_domains)
   INTEGER nest_task_offsets(max_domains)
   LOGICAL intercomm_active( max_domains )
   LOGICAL domain_active_this_task( max_domains )
! see comments below (search for "Communicator definition")
   INTEGER tasks_per_split
   INTEGER comm_start(max_domains)   ! set in dm_task_split
!   INTEGER comm_pes  (max_domains)   ! either this may be set in dm_task_split
!   INTEGER comm_pes_x(max_domains)   ! or these may be set in dm_task_split
!   INTEGER comm_pes_y(max_domains)   ! "    "   may be set in dm_task_split
!   INTEGER comm_domain(max_domains)  ! set in dm_task_split
   INTEGER nest_pes_x(max_domains)   ! set in dm_task_split
   INTEGER nest_pes_y(max_domains)   ! set in dm_task_split
   INTEGER comms_i_am_in (max_domains)  ! list of local communicators this task is a member of
   INTEGER loc_comm(max_domains)
   LOGICAL poll_servers
   INTEGER nio_tasks_per_group(max_domains), nio_groups, num_io_tasks
   NAMELIST /dm_task_split/ tasks_per_split, comm_start, nest_pes_x, nest_pes_y
   NAMELIST /namelist_quilt/ nio_tasks_per_group, nio_groups, poll_servers


#if (DA_CORE == 1)
   integer :: c_ipsy, c_ipey, c_kpsy, c_kpey, c_kpsx, c_kpex, c_ipex, c_ipsx, c_jpex, c_jpsx, c_jpey, c_jpsy
   integer :: c_imsy, c_imey, c_kmsy, c_kmey, c_kmsx, c_kmex, c_imex, c_imsx, c_jmex, c_jmsx, c_jmey, c_jmsy
   integer :: k
#endif

   INTERFACE wrf_dm_maxval
#if ( defined(PROMOTE_FLOAT) || ( RWORDSIZE == DWORDSIZE ) )
     MODULE PROCEDURE wrf_dm_maxval_real , wrf_dm_maxval_integer
#else
     MODULE PROCEDURE wrf_dm_maxval_real , wrf_dm_maxval_integer, wrf_dm_maxval_doubleprecision
#endif
   END INTERFACE

   INTERFACE wrf_dm_minval                       ! gopal's doing
#if ( defined(PROMOTE_FLOAT) || ( RWORDSIZE == DWORDSIZE ) )
     MODULE PROCEDURE wrf_dm_minval_real , wrf_dm_minval_integer
#else
     MODULE PROCEDURE wrf_dm_minval_real , wrf_dm_minval_integer, wrf_dm_minval_doubleprecision
#endif
   END INTERFACE

CONTAINS

   SUBROUTINE MPASPECT( P, MINM, MINN, PROCMIN_M, PROCMIN_N )
      IMPLICIT NONE
      INTEGER P, M, N, MINI, MINM, MINN, PROCMIN_M, PROCMIN_N
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
          END IF
        END IF
      END DO
      IF ( MINM .LT. PROCMIN_M .OR. MINN .LT. PROCMIN_N ) THEN
        WRITE( wrf_err_message , * )'MPASPECT: UNABLE TO GENERATE PROCESSOR MESH.  STOPPING.'
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        WRITE( wrf_err_message , * )' PROCMIN_M ', PROCMIN_M
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        WRITE( wrf_err_message , * )' PROCMIN_N ', PROCMIN_N
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        WRITE( wrf_err_message , * )' P         ', P
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        WRITE( wrf_err_message , * )' MINM      ', MINM
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        WRITE( wrf_err_message , * )' MINN      ', MINN
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        CALL wrf_error_fatal ( 'module_dm: mpaspect' )
      END IF
   RETURN
   END SUBROUTINE MPASPECT

   SUBROUTINE compute_mesh( ntasks , ntasks_x, ntasks_y )
     IMPLICIT NONE
     INTEGER, INTENT(IN)  :: ntasks
     INTEGER, INTENT(OUT) :: ntasks_x, ntasks_y
     INTEGER lats_to_mic
     CALL nl_get_nproc_x ( 1, ntasks_x )
     CALL nl_get_nproc_y ( 1, ntasks_y )
#ifndef NMM_CORE
     CALL nl_get_lats_to_mic ( 1, lats_to_mic )
#endif
! check if user has specified in the namelist
     IF ( ntasks_x .GT. 0 .OR. ntasks_y .GT. 0 ) THEN
       ! if only ntasks_x is specified then make it 1-d decomp in i
       IF      ( ntasks_x .GT. 0 .AND. ntasks_y .EQ. -1 ) THEN
         ntasks_y = ntasks / ntasks_x
       ! if only ntasks_y is specified then make it 1-d decomp in j
       ELSE IF ( ntasks_x .EQ. -1 .AND. ntasks_y .GT. 0 ) THEN
         ntasks_x = ntasks / ntasks_y
       END IF
       ! make sure user knows what they're doing
       IF ( ntasks_x * ntasks_y .NE. ntasks ) THEN
         WRITE( wrf_err_message , * )'WRF_DM_INITIALIZE (RSL_LITE): nproc_x * nproc_y in namelist ne ',ntasks
         CALL wrf_error_fatal ( wrf_err_message )
       END IF
#ifndef NMM_CORE
     ELSE IF ( lats_to_mic .GT. 0 ) THEN
       ntasks_x = ntasks / 2
       ntasks_y = 2
       IF ( ntasks_x * ntasks_y .NE. ntasks ) THEN
         WRITE( wrf_err_message , * )&
           'WRF_DM_INITIALIZE (lats_to_mic > 0) nproc_x (',ntasks_x,')* nproc_y (',ntasks_y,&
           ') in namelist ne ',ntasks
         CALL wrf_error_fatal ( wrf_err_message )
       END IF
#endif
     ELSE
       ! When neither is specified, work out mesh with MPASPECT
       ! Pass nproc_ln and nproc_nt so that number of procs in
       ! i-dim (nproc_ln) is equal or lesser.
       CALL mpaspect ( ntasks, ntasks_x, ntasks_y, 1, 1 )
     END IF
     ntasks_store(1) = ntasks
     ntasks_x_store(1) = ntasks_x
     ntasks_y_store(1) = ntasks_y
   END SUBROUTINE compute_mesh

   SUBROUTINE wrf_dm_initialize
      IMPLICIT NONE
#ifndef STUBMPI
      INTEGER :: local_comm_per, local_comm_x, local_comm_y, local_comm2, new_local_comm, group, newgroup, p, p1, ierr,itmp
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ranks
      INTEGER comdup
      INTEGER, DIMENSION(2) :: dims, coords
      LOGICAL, DIMENSION(2) :: isperiodic
      LOGICAL :: reorder_mesh

      CALL instate_communicators_for_domain(1)

      CALL wrf_get_dm_communicator ( new_local_comm )
      dims(1) = nest_pes_y(1)  ! rows
      dims(2) = nest_pes_x(1)  ! columns
      isperiodic(1) = .true.
      isperiodic(2) = .true.
      CALL mpi_cart_create( new_local_comm, 2, dims, isperiodic, .false., local_comm_per, ierr )
      local_communicator_periodic_store(1) = local_comm_per
! set all the domains' periodic communicators to this one <- kludge, 20151223, splitting domains won't work for period bc's
      local_communicator_periodic_store = local_comm_per
      local_communicator_periodic = local_comm_per

#else
      ntasks = 1
      ntasks_x = 1
      ntasks_y = 1
      mytask = 0
      mytask_x = 0
      mytask_y = 0
      nest_pes_x = 1
      nest_pes_y = 1
      intercomm_active = .TRUE.
      domain_active_this_task = .TRUE.
#endif
      CALL nl_set_nproc_x ( 1, ntasks_x )
      CALL nl_set_nproc_y ( 1, ntasks_y )
      WRITE( wrf_err_message , * )'Ntasks in X ',ntasks_x,', ntasks in Y ',ntasks_y
      CALL wrf_message( wrf_err_message )
      RETURN
   END SUBROUTINE wrf_dm_initialize

   SUBROUTINE get_dm_max_halo_width( id, width )
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: id
     INTEGER, INTENT(OUT) :: width
     IF ( id .EQ. 1 ) THEN   ! this is coarse domain
       width = max_halo_width
     ELSE
       width = max_halo_width + 3
     END IF
     RETURN
   END SUBROUTINE get_dm_max_halo_width

   SUBROUTINE patch_domain_rsl_lite( id  , parent, parent_id, &
                                sd1 , ed1 , sp1 , ep1 , sm1 , em1 ,        &
                                sd2 , ed2 , sp2 , ep2 , sm2 , em2 ,        &
                                sd3 , ed3 , sp3 , ep3 , sm3 , em3 ,        &
                                      sp1x , ep1x , sm1x , em1x , &
                                      sp2x , ep2x , sm2x , em2x , &
                                      sp3x , ep3x , sm3x , em3x , &
                                      sp1y , ep1y , sm1y , em1y , &
                                      sp2y , ep2y , sm2y , em2y , &
                                      sp3y , ep3y , sm3y , em3y , &
                                bdx , bdy )

#if ( ( defined(SGIALTIX) || defined(FUJITSU_FX10) || defined(KEEP_INT_AROUND) ) && (! defined(MOVE_NESTS) ) )
      USE module_domain, ONLY : domain, head_grid, find_grid_by_id, alloc_space_field
#else
      USE module_domain, ONLY : domain, head_grid, find_grid_by_id
#endif

      IMPLICIT NONE
      INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
      INTEGER, INTENT(OUT)  :: sp1 , ep1 , sp2 , ep2 , sp3 , ep3 , &
                               sm1 , em1 , sm2 , em2 , sm3 , em3
      INTEGER, INTENT(OUT)  :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &
                               sm1x , em1x , sm2x , em2x , sm3x , em3x
      INTEGER, INTENT(OUT)  :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &
                               sm1y , em1y , sm2y , em2y , sm3y , em3y
      INTEGER, INTENT(IN)   :: id, parent_id
      TYPE(domain),POINTER  :: parent

! Local variables
      INTEGER               :: ids, ide, jds, jde, kds, kde
      INTEGER               :: ims, ime, jms, jme, kms, kme
      INTEGER               :: ips, ipe, jps, jpe, kps, kpe
      INTEGER               :: imsx, imex, jmsx, jmex, kmsx, kmex
      INTEGER               :: ipsx, ipex, jpsx, jpex, kpsx, kpex
      INTEGER               :: imsy, imey, jmsy, jmey, kmsy, kmey
      INTEGER               :: ipsy, ipey, jpsy, jpey, kpsy, kpey

      INTEGER               :: c_sd1 , c_ed1 , c_sd2 , c_ed2 , c_sd3 , c_ed3
      INTEGER               :: c_sp1 , c_ep1 , c_sp2 , c_ep2 , c_sp3 , c_ep3 , &
                               c_sm1 , c_em1 , c_sm2 , c_em2 , c_sm3 , c_em3
      INTEGER               :: c_sp1x , c_ep1x , c_sp2x , c_ep2x , c_sp3x , c_ep3x , &
                               c_sm1x , c_em1x , c_sm2x , c_em2x , c_sm3x , c_em3x
      INTEGER               :: c_sp1y , c_ep1y , c_sp2y , c_ep2y , c_sp3y , c_ep3y , &
                               c_sm1y , c_em1y , c_sm2y , c_em2y , c_sm3y , c_em3y

      INTEGER               :: c_ids, c_ide, c_jds, c_jde, c_kds, c_kde
      INTEGER               :: c_ims, c_ime, c_jms, c_jme, c_kms, c_kme
      INTEGER               :: c_ips, c_ipe, c_jps, c_jpe, c_kps, c_kpe

      INTEGER               :: idim , jdim , kdim , rem , a, b
      INTEGER               :: i, j, ni, nj, Px, Py, P

      INTEGER               :: parent_grid_ratio, i_parent_start, j_parent_start
      INTEGER               :: shw
      INTEGER               :: idim_cd, jdim_cd, ierr
      INTEGER               :: max_dom

#if (DA_CORE == 1)
      INTEGER               :: e_we, e_sn
#endif

      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER  :: nest_grid
      CHARACTER*256   :: mess

      INTEGER parent_max_halo_width
      INTEGER thisdomain_max_halo_width
      INTEGER lats_to_mic

     lats_to_mic=0
#ifndef NMM_CORE
     CALL nl_get_lats_to_mic( 1, lats_to_mic )
#endif
      IF ( lats_to_mic .GT. 0 ) THEN
        minx = -99  ! code to task_for_point to do split decomposition over MIC and host
        miny = lats_to_mic  ! number of latitudes that should be assigned to MIC
      ELSE
        minx = 1   ! normal
        miny = 1   ! normal
      END IF



      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_ZXY )
            ids = sd2 ; ide = ed2
            jds = sd3 ; jde = ed3
            kds = sd1 ; kde = ed1
         CASE ( DATA_ORDER_XYZ )
            ids = sd1 ; ide = ed1
            jds = sd2 ; jde = ed2
            kds = sd3 ; kde = ed3
         CASE ( DATA_ORDER_XZY )
            ids = sd1 ; ide = ed1
            jds = sd3 ; jde = ed3
            kds = sd2 ; kde = ed2
         CASE ( DATA_ORDER_YXZ)
            ids = sd2 ; ide = ed2
            jds = sd1 ; jde = ed1
            kds = sd3 ; kde = ed3
      END SELECT

      CALL nl_get_max_dom( 1 , max_dom )

      CALL get_dm_max_halo_width( id , thisdomain_max_halo_width )
      IF ( id .GT. 1 ) THEN
        CALL get_dm_max_halo_width( parent%id , parent_max_halo_width )
      END IF

      CALL compute_memory_dims_rsl_lite ( id, thisdomain_max_halo_width, 0 , bdx, bdy,   &
                   ids,  ide,  jds,  jde,  kds,  kde, &
                   ims,  ime,  jms,  jme,  kms,  kme, &
                   imsx, imex, jmsx, jmex, kmsx, kmex, &
                   imsy, imey, jmsy, jmey, kmsy, kmey, &
                   ips,  ipe,  jps,  jpe,  kps,  kpe, &
                   ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                   ipsy, ipey, jpsy, jpey, kpsy, kpey )

     ! ensure that the every parent domain point has a full set of nested points under it
     ! even at the borders. Do this by making sure the number of nest points is a multiple of
     ! the nesting ratio. Note that this is important mostly to the intermediate domain, which
     ! is the subject of the scatter gather comms with the parent

      IF ( id .GT. 1 ) THEN
         CALL nl_get_parent_grid_ratio( id, parent_grid_ratio )
         if ( mod(ime,parent_grid_ratio) .NE. 0 ) ime = ime + parent_grid_ratio - mod(ime,parent_grid_ratio)
         if ( mod(jme,parent_grid_ratio) .NE. 0 ) jme = jme + parent_grid_ratio - mod(jme,parent_grid_ratio)
      END IF

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            sp2 = ips ; ep2 = ipe ; sm2 = ims ; em2 = ime
            sp3 = jps ; ep3 = jpe ; sm3 = jms ; em3 = jme
            sp1 = kps ; ep1 = kpe ; sm1 = kms ; em1 = kme
            sp2x = ipsx ; ep2x = ipex ; sm2x = imsx ; em2x = imex
            sp3x = jpsx ; ep3x = jpex ; sm3x = jmsx ; em3x = jmex
            sp1x = kpsx ; ep1x = kpex ; sm1x = kmsx ; em1x = kmex
            sp2y = ipsy ; ep2y = ipey ; sm2y = imsy ; em2y = imey
            sp3y = jpsy ; ep3y = jpey ; sm3y = jmsy ; em3y = jmey
            sp1y = kpsy ; ep1y = kpey ; sm1y = kmsy ; em1y = kmey
         CASE ( DATA_ORDER_ZYX )
            sp3 = ips ; ep3 = ipe ; sm3 = ims ; em3 = ime
            sp2 = jps ; ep2 = jpe ; sm2 = jms ; em2 = jme
            sp1 = kps ; ep1 = kpe ; sm1 = kms ; em1 = kme
            sp3x = ipsx ; ep3x = ipex ; sm3x = imsx ; em3x = imex
            sp2x = jpsx ; ep2x = jpex ; sm2x = jmsx ; em2x = jmex
            sp1x = kpsx ; ep1x = kpex ; sm1x = kmsx ; em1x = kmex
            sp3y = ipsy ; ep3y = ipey ; sm3y = imsy ; em3y = imey
            sp2y = jpsy ; ep2y = jpey ; sm2y = jmsy ; em2y = jmey
            sp1y = kpsy ; ep1y = kpey ; sm1y = kmsy ; em1y = kmey
         CASE ( DATA_ORDER_XYZ )
            sp1 = ips ; ep1 = ipe ; sm1 = ims ; em1 = ime
            sp2 = jps ; ep2 = jpe ; sm2 = jms ; em2 = jme
            sp3 = kps ; ep3 = kpe ; sm3 = kms ; em3 = kme
            sp1x = ipsx ; ep1x = ipex ; sm1x = imsx ; em1x = imex
            sp2x = jpsx ; ep2x = jpex ; sm2x = jmsx ; em2x = jmex
            sp3x = kpsx ; ep3x = kpex ; sm3x = kmsx ; em3x = kmex
            sp1y = ipsy ; ep1y = ipey ; sm1y = imsy ; em1y = imey
            sp2y = jpsy ; ep2y = jpey ; sm2y = jmsy ; em2y = jmey
            sp3y = kpsy ; ep3y = kpey ; sm3y = kmsy ; em3y = kmey
         CASE ( DATA_ORDER_YXZ)
            sp2 = ips ; ep2 = ipe ; sm2 = ims ; em2 = ime
            sp1 = jps ; ep1 = jpe ; sm1 = jms ; em1 = jme
            sp3 = kps ; ep3 = kpe ; sm3 = kms ; em3 = kme
            sp2x = ipsx ; ep2x = ipex ; sm2x = imsx ; em2x = imex
            sp1x = jpsx ; ep1x = jpex ; sm1x = jmsx ; em1x = jmex
            sp3x = kpsx ; ep3x = kpex ; sm3x = kmsx ; em3x = kmex
            sp2y = ipsy ; ep2y = ipey ; sm2y = imsy ; em2y = imey
            sp1y = jpsy ; ep1y = jpey ; sm1y = jmsy ; em1y = jmey
            sp3y = kpsy ; ep3y = kpey ; sm3y = kmsy ; em3y = kmey
         CASE ( DATA_ORDER_XZY )
            sp1 = ips ; ep1 = ipe ; sm1 = ims ; em1 = ime
            sp3 = jps ; ep3 = jpe ; sm3 = jms ; em3 = jme
            sp2 = kps ; ep2 = kpe ; sm2 = kms ; em2 = kme
            sp1x = ipsx ; ep1x = ipex ; sm1x = imsx ; em1x = imex
            sp3x = jpsx ; ep3x = jpex ; sm3x = jmsx ; em3x = jmex
            sp2x = kpsx ; ep2x = kpex ; sm2x = kmsx ; em2x = kmex
            sp1y = ipsy ; ep1y = ipey ; sm1y = imsy ; em1y = imey
            sp3y = jpsy ; ep3y = jpey ; sm3y = jmsy ; em3y = jmey
            sp2y = kpsy ; ep2y = kpey ; sm2y = kmsy ; em2y = kmey
         CASE ( DATA_ORDER_YZX )
            sp3 = ips ; ep3 = ipe ; sm3 = ims ; em3 = ime
            sp1 = jps ; ep1 = jpe ; sm1 = jms ; em1 = jme
            sp2 = kps ; ep2 = kpe ; sm2 = kms ; em2 = kme
            sp3x = ipsx ; ep3x = ipex ; sm3x = imsx ; em3x = imex
            sp1x = jpsx ; ep1x = jpex ; sm1x = jmsx ; em1x = jmex
            sp2x = kpsx ; ep2x = kpex ; sm2x = kmsx ; em2x = kmex
            sp3y = ipsy ; ep3y = ipey ; sm3y = imsy ; em3y = imey
            sp1y = jpsy ; ep1y = jpey ; sm1y = jmsy ; em1y = jmey
            sp2y = kpsy ; ep2y = kpey ; sm2y = kmsy ; em2y = kmey
      END SELECT

      IF ( id.EQ.1 ) THEN
         WRITE(wrf_err_message,*)'*************************************'
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'Parent domain'
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'ids,ide,jds,jde ',ids,ide,jds,jde
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'ims,ime,jms,jme ',ims,ime,jms,jme
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'ips,ipe,jps,jpe ',ips,ipe,jps,jpe
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'*************************************'
         CALL wrf_message( TRIM(wrf_err_message) )
      END IF

      IF ( id .GT. 1 ) THEN

         CALL nl_get_shw( id, shw )
         CALL nl_get_i_parent_start( id , i_parent_start )
         CALL nl_get_j_parent_start( id , j_parent_start )
         CALL nl_get_parent_grid_ratio( id, parent_grid_ratio )

         SELECT CASE ( model_data_order )
            CASE ( DATA_ORDER_ZXY )
               idim = ed2-sd2+1
               jdim = ed3-sd3+1
               kdim = ed1-sd1+1
               c_kds = sd1                ; c_kde = ed1
            CASE ( DATA_ORDER_ZYX )
               idim = ed3-sd3+1
               jdim = ed2-sd2+1
               kdim = ed1-sd1+1
               c_kds = sd1                ; c_kde = ed1
            CASE ( DATA_ORDER_XYZ )
               idim = ed1-sd1+1
               jdim = ed2-sd2+1
               kdim = ed3-sd3+1
               c_kds = sd3                ; c_kde = ed3
            CASE ( DATA_ORDER_YXZ)
               idim = ed2-sd2+1
               jdim = ed1-sd1+1
               kdim = ed3-sd3+1
               c_kds = sd3                ; c_kde = ed3
            CASE ( DATA_ORDER_XZY )
               idim = ed1-sd1+1
               jdim = ed3-sd3+1
               kdim = ed2-sd2+1
               c_kds = sd2                ; c_kde = ed2
            CASE ( DATA_ORDER_YZX )
               idim = ed3-sd3+1
               jdim = ed1-sd1+1
               kdim = ed2-sd2+1
               c_kds = sd2                ; c_kde = ed2
         END SELECT

         idim_cd = idim / parent_grid_ratio + 1 + 2*shw + 1
         jdim_cd = jdim / parent_grid_ratio + 1 + 2*shw + 1

         c_ids = i_parent_start-shw ; c_ide = c_ids + idim_cd - 1
         c_jds = j_parent_start-shw ; c_jde = c_jds + jdim_cd - 1

#if (DA_CORE == 1)
          call nl_get_e_we( id -1, e_we )
          call nl_get_e_sn( id -1, e_sn )

         if ( c_ids .le. 0   ) c_ids = 1
         if ( c_ide .gt. e_we) c_ide = e_we
         if ( c_jds .le. 0   ) c_jds = 1
         if ( c_jde .gt. e_sn) c_jde = e_sn
#endif
         ! we want the intermediate domain to be decomposed the
         ! the same as the underlying nest. So try this:

         c_ips = -1
         nj = ( c_jds - j_parent_start ) * parent_grid_ratio + 1 + 1 ;
         ierr = 0
         DO i = c_ids, c_ide
            ni = ( i - i_parent_start ) * parent_grid_ratio + 1 + 1 ;
!jm            CALL task_for_point ( ni, nj, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py, &
            CALL task_for_point ( ni, nj, ids, ide, jds, jde, nest_pes_x(id), nest_pes_y(id),Px,Py, &
                                  minx, miny,  ierr )
            IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (a)')
            IF ( Px .EQ. mytask_x ) THEN
               c_ipe = i
               IF ( c_ips .EQ. -1 ) c_ips = i
            END IF
         END DO
         IF ( ierr .NE. 0 ) THEN
            CALL tfp_message("<stdin>",__LINE__)
         END IF
         IF (c_ips .EQ. -1 ) THEN
            c_ipe = -1
            c_ips = 0
         END IF

         c_jps = -1
         ni = ( c_ids - i_parent_start ) * parent_grid_ratio + 1 + 1 ;
         ierr = 0
         DO j = c_jds, c_jde
            nj = ( j - j_parent_start ) * parent_grid_ratio + 1 + 1 ;
!            CALL task_for_point ( ni, nj, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py, &
            CALL task_for_point ( ni, nj, ids, ide, jds, jde, nest_pes_x(id), nest_pes_y(id), Px, Py, &
                                  minx, miny, ierr )
            IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (b)')


            IF ( Py .EQ. mytask_y ) THEN
               c_jpe = j
               IF ( c_jps .EQ. -1 ) c_jps = j
            END IF
         END DO
         IF ( ierr .NE. 0 ) THEN
            CALL tfp_message("<stdin>",__LINE__)
         END IF
         IF (c_jps .EQ. -1 ) THEN
            c_jpe = -1
            c_jps = 0
         END IF

#if (DA_CORE == 1)
         IF (c_ipe .EQ. -1 .or. c_jpe .EQ. -1) THEN
            c_ipe = -1
            c_ips = 0
            c_jpe = -1
            c_jps = 0
         END IF


          c_kpsx = -1
          nj = ( c_jds - j_parent_start ) * parent_grid_ratio + 1 + 1 ;
          ierr = 0
          DO k = c_kds, c_kde
!             CALL task_for_point ( k, nj, kds, kde, jds, jde, ntasks_x, ntasks_y, Px, Py, &
             CALL task_for_point ( k, nj, kds, kde, jds, jde, nest_pes_x(id), nest_pes_y(id), Px, Py, &
                                   1, 1, ierr )
             IF ( Px .EQ. mytask_x ) THEN
                c_kpex = k
                IF ( c_kpsx .EQ. -1 ) c_kpsx = k
             END IF
          END DO
          IF ( ierr .NE. 0 ) THEN
             CALL tfp_message("<stdin>",__LINE__)
          END IF
          IF (c_kpsx .EQ. -1 ) THEN
             c_kpex = -1
             c_kpsx = 0
          END IF

          c_jpsx = -1
          k = c_kds ;
          ierr = 0
          DO j = c_jds, c_jde
             nj = ( j - j_parent_start ) * parent_grid_ratio + 1 + 1 ;
!             CALL task_for_point ( k, nj, kds, kde, jds, jde, ntasks_x, ntasks_y, Px, Py, &
             CALL task_for_point ( k, nj, kds, kde, jds, jde, nest_pes_x(id), nest_pes_y(id), Px, Py, &
                                   1, 1, ierr )
             IF ( Py .EQ. mytask_y ) THEN
                c_jpex = j
                IF ( c_jpsx .EQ. -1 ) c_jpsx = j
             END IF
          END DO
          IF ( ierr .NE. 0 ) THEN
             CALL tfp_message("<stdin>",__LINE__)
          END IF
          IF (c_jpsx .EQ. -1 ) THEN
             c_jpex = -1
             c_jpsx = 0
          END IF

          IF (c_ipex .EQ. -1 .or. c_jpex .EQ. -1) THEN
             c_ipex = -1
             c_ipsx = 0
             c_jpex = -1
             c_jpsx = 0
          END IF

          c_kpsy = c_kpsx   ! same as above
          c_kpey = c_kpex   ! same as above

          c_ipsy = -1
          k = c_kds ;
          ierr = 0
          DO i = c_ids, c_ide
             ni = ( i - i_parent_start ) * parent_grid_ratio + 1 + 1 ;
!             CALL task_for_point ( ni, k, ids, ide, kds, kde, ntasks_y, ntasks_x, Py, Px, &
             CALL task_for_point ( ni, k, ids, ide, kds, kde, nest_pes_y(id), nest_pes_x(id), Py, Px, &
                                   1, 1, ierr ) ! x and y for proc mesh reversed
             IF ( Py .EQ. mytask_y ) THEN
                c_ipey = i
                IF ( c_ipsy .EQ. -1 ) c_ipsy = i
             END IF
          END DO
          IF ( ierr .NE. 0 ) THEN
             CALL tfp_message("<stdin>",__LINE__)
          END IF
          IF (c_ipsy .EQ. -1 ) THEN
             c_ipey = -1
             c_ipsy = 0
          END IF
#endif


         IF ( c_ips <= c_ipe ) THEN
! extend the patch dimensions out shw along edges of domain
           IF ( mytask_x .EQ. 0 ) THEN
             c_ips = c_ips - shw
#if (DA_CORE == 1)
             c_ipsy = c_ipsy - shw  
#endif
           END IF
!           IF ( mytask_x .EQ. ntasks_x-1 ) THEN
           IF ( mytask_x .EQ. nest_pes_x(id)-1 ) THEN
             c_ipe = c_ipe + shw
#if (DA_CORE == 1)
             c_ipey = c_ipey + shw  
#endif
           END IF
           c_ims = max( c_ips - max(shw,thisdomain_max_halo_width), c_ids - bdx ) - 1
           c_ime = min( c_ipe + max(shw,thisdomain_max_halo_width), c_ide + bdx ) + 1
         ELSE
           c_ims = 0
           c_ime = 0
         END IF


! handle j dims
         IF ( c_jps <= c_jpe ) THEN
! extend the patch dimensions out shw along edges of domain
           IF ( mytask_y .EQ. 0 ) THEN
              c_jps = c_jps - shw
#if (DA_CORE == 1)
              c_jpsx = c_jpsx - shw  
#endif
           END IF
!           IF ( mytask_y .EQ. ntasks_y-1 ) THEN
           IF ( mytask_y .EQ. nest_pes_y(id)-1 ) THEN
              c_jpe = c_jpe + shw
#if (DA_CORE == 1)
              c_jpex = c_jpex + shw  
#endif
           END IF
           c_jms = max( c_jps - max(shw,thisdomain_max_halo_width), c_jds - bdx ) - 1
           c_jme = min( c_jpe + max(shw,thisdomain_max_halo_width), c_jde + bdx ) + 1
! handle k dims
         ELSE
           c_jms = 0
           c_jme = 0
         END IF
         c_kps = 1
         c_kpe = c_kde
         c_kms = 1
         c_kme = c_kde

! Default initializations
         c_sm1x = 1 ; c_em1x = 1 ; c_sm2x = 1 ; c_em2x = 1 ; c_sm3x = 1 ; c_em3x = 1
         c_sm1y = 1 ; c_em1y = 1 ; c_sm2y = 1 ; c_em2y = 1 ; c_sm3y = 1 ; c_em3y = 1

#if (DA_CORE == 1)
         c_kmsx = c_kpsx
         c_kmex = c_kpex
         c_kmsy = c_kpsy
         c_kmey = c_kpey

         IF ( c_kpsx .EQ. 0 .AND. c_kpex .EQ. -1 ) THEN  
            c_kmsx = 0
            c_kmex = 0
         END IF
         IF ( c_kpsy .EQ. 0 .AND. c_kpey .EQ. -1 ) THEN
            c_kmsy = 0
            c_kmey = 0
         END IF
         c_imsx = c_ids
         c_imex = c_ide
         c_ipsx = c_imsx
         c_ipex = c_imex

         IF ( c_ipsy .EQ. 0 .AND. c_ipey .EQ. -1 ) THEN
            c_imsy = 0
            c_imey = 0
         ELSE
            c_imsy = c_ipsy
            c_imey = c_ipey
         END IF

         c_jmsx = c_jpsx
         c_jmex = c_jpex
         c_jmsy = c_jds
         c_jmey = c_jde

         IF ( c_jpsx .EQ. 0 .AND. c_jpex .EQ. -1 ) THEN
            c_jmsx = 0
            c_jmex = 0
         ELSE
            c_jpsy = c_jmsy
            c_jpey = c_jmey
         END IF

         c_sm1x = c_imsx
         c_em1x = c_imex
         c_sm2x = c_jmsx
         c_em2x = c_jmex
         c_sm3x = c_kmsx
         c_em3x = c_kmex

         c_sm1y = c_imsy
         c_em1y = c_imey
         c_sm2y = c_jmsy
         c_em2y = c_jmey
         c_sm3y = c_kmsy
         c_em3y = c_kmey

         c_sp1x = c_ipsx
         c_ep1x = c_ipex
         c_sp2x = c_jpsx
         c_ep2x = c_jpex
         c_sp3x = c_kpsx
         c_ep3x = c_kpex

         c_sp1y = c_ipsy
         c_ep1y = c_ipey
         c_sp2y = c_jpsy
         c_ep2y = c_jpey
         c_sp3y = c_kpsy
         c_ep3y = c_kpey
#endif

         WRITE(wrf_err_message,*)'*************************************'
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'Nesting domain'
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'ids,ide,jds,jde ',ids,ide,jds,jde
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'ims,ime,jms,jme ',ims,ime,jms,jme
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'ips,ipe,jps,jpe ',ips,ipe,jps,jpe
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'INTERMEDIATE domain'
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'ids,ide,jds,jde ',c_ids,c_ide,c_jds,c_jde
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'ims,ime,jms,jme ',c_ims,c_ime,c_jms,c_jme
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'ips,ipe,jps,jpe ',c_ips,c_ipe,c_jps,c_jpe
         CALL wrf_message( TRIM(wrf_err_message) )
         WRITE(wrf_err_message,*)'*************************************'
         CALL wrf_message( TRIM(wrf_err_message) )

         SELECT CASE ( model_data_order )
            CASE ( DATA_ORDER_ZXY )
               c_sd2 = c_ids ; c_ed2 = c_ide ; c_sp2 = c_ips ; c_ep2 = c_ipe ; c_sm2 = c_ims ; c_em2 = c_ime
               c_sd3 = c_jds ; c_ed3 = c_jde ; c_sp3 = c_jps ; c_ep3 = c_jpe ; c_sm3 = c_jms ; c_em3 = c_jme
               c_sd1 = c_kds ; c_ed1 = c_kde ; c_sp1 = c_kps ; c_ep1 = c_kpe ; c_sm1 = c_kms ; c_em1 = c_kme
            CASE ( DATA_ORDER_ZYX )
               c_sd3 = c_ids ; c_ed3 = c_ide ; c_sp3 = c_ips ; c_ep3 = c_ipe ; c_sm3 = c_ims ; c_em3 = c_ime
               c_sd2 = c_jds ; c_ed2 = c_jde ; c_sp2 = c_jps ; c_ep2 = c_jpe ; c_sm2 = c_jms ; c_em2 = c_jme
               c_sd1 = c_kds ; c_ed1 = c_kde ; c_sp1 = c_kps ; c_ep1 = c_kpe ; c_sm1 = c_kms ; c_em1 = c_kme
            CASE ( DATA_ORDER_XYZ )
               c_sd1 = c_ids ; c_ed1 = c_ide ; c_sp1 = c_ips ; c_ep1 = c_ipe ; c_sm1 = c_ims ; c_em1 = c_ime
               c_sd2 = c_jds ; c_ed2 = c_jde ; c_sp2 = c_jps ; c_ep2 = c_jpe ; c_sm2 = c_jms ; c_em2 = c_jme
               c_sd3 = c_kds ; c_ed3 = c_kde ; c_sp3 = c_kps ; c_ep3 = c_kpe ; c_sm3 = c_kms ; c_em3 = c_kme
            CASE ( DATA_ORDER_YXZ)
               c_sd2 = c_ids ; c_ed2 = c_ide ; c_sp2 = c_ips ; c_ep2 = c_ipe ; c_sm2 = c_ims ; c_em2 = c_ime
               c_sd1 = c_jds ; c_ed1 = c_jde ; c_sp1 = c_jps ; c_ep1 = c_jpe ; c_sm1 = c_jms ; c_em1 = c_jme
               c_sd3 = c_kds ; c_ed3 = c_kde ; c_sp3 = c_kps ; c_ep3 = c_kpe ; c_sm3 = c_kms ; c_em3 = c_kme
            CASE ( DATA_ORDER_XZY )
               c_sd1 = c_ids ; c_ed1 = c_ide ; c_sp1 = c_ips ; c_ep1 = c_ipe ; c_sm1 = c_ims ; c_em1 = c_ime
               c_sd3 = c_jds ; c_ed3 = c_jde ; c_sp3 = c_jps ; c_ep3 = c_jpe ; c_sm3 = c_jms ; c_em3 = c_jme
               c_sd2 = c_kds ; c_ed2 = c_kde ; c_sp2 = c_kps ; c_ep2 = c_kpe ; c_sm2 = c_kms ; c_em2 = c_kme
            CASE ( DATA_ORDER_YZX )
               c_sd3 = c_ids ; c_ed3 = c_ide ; c_sp3 = c_ips ; c_ep3 = c_ipe ; c_sm3 = c_ims ; c_em3 = c_ime
               c_sd1 = c_jds ; c_ed1 = c_jde ; c_sp1 = c_jps ; c_ep1 = c_jpe ; c_sm1 = c_jms ; c_em1 = c_jme
               c_sd2 = c_kds ; c_ed2 = c_kde ; c_sp2 = c_kps ; c_ep2 = c_kpe ; c_sm2 = c_kms ; c_em2 = c_kme
         END SELECT

         ALLOCATE ( intermediate_grid )
         ALLOCATE ( intermediate_grid%parents( max_parents ) )
         ALLOCATE ( intermediate_grid%nests( max_nests ) )
         intermediate_grid%allocated=.false.
         NULLIFY( intermediate_grid%sibling )
         DO i = 1, max_nests
            NULLIFY( intermediate_grid%nests(i)%ptr )
         END DO
         NULLIFY  (intermediate_grid%next)
         NULLIFY  (intermediate_grid%same_level)
         NULLIFY  (intermediate_grid%i_start)
         NULLIFY  (intermediate_grid%j_start)
         NULLIFY  (intermediate_grid%i_end)
         NULLIFY  (intermediate_grid%j_end)
         intermediate_grid%id = id   ! these must be the same. Other parts of code depend on it (see gen_comms.c)
         intermediate_grid%num_nests = 0
         intermediate_grid%num_siblings = 0
         intermediate_grid%num_parents = 1
         intermediate_grid%max_tiles   = 0
         intermediate_grid%num_tiles_spec   = 0
#if ( EM_CORE == 1 && DA_CORE != 1 )
         intermediate_grid%active_this_task = .true.
#endif
         CALL find_grid_by_id ( id, head_grid, nest_grid )

         nest_grid%intermediate_grid => intermediate_grid  ! nest grid now has a pointer to this baby
         intermediate_grid%parents(1)%ptr => nest_grid     ! the intermediate grid considers nest its parent
         intermediate_grid%num_parents = 1

         intermediate_grid%is_intermediate = .TRUE.
         SELECT CASE ( model_data_order )
            CASE ( DATA_ORDER_ZXY )
               intermediate_grid%nids = nest_grid%sd32 ; intermediate_grid%njds = nest_grid%sd33
               intermediate_grid%nide = nest_grid%ed32 ; intermediate_grid%njde = nest_grid%sd33
            CASE ( DATA_ORDER_ZYX )
               intermediate_grid%nids = nest_grid%sd33 ; intermediate_grid%njds = nest_grid%sd32
               intermediate_grid%nide = nest_grid%ed33 ; intermediate_grid%njde = nest_grid%sd32
            CASE ( DATA_ORDER_XYZ )
               intermediate_grid%nids = nest_grid%sd31 ; intermediate_grid%njds = nest_grid%sd32
               intermediate_grid%nide = nest_grid%ed31 ; intermediate_grid%njde = nest_grid%sd32
            CASE ( DATA_ORDER_YXZ)
               intermediate_grid%nids = nest_grid%sd32 ; intermediate_grid%njds = nest_grid%sd31
               intermediate_grid%nide = nest_grid%ed32 ; intermediate_grid%njde = nest_grid%sd31
            CASE ( DATA_ORDER_XZY )
               intermediate_grid%nids = nest_grid%sd31 ; intermediate_grid%njds = nest_grid%sd33
               intermediate_grid%nide = nest_grid%ed31 ; intermediate_grid%njde = nest_grid%sd33
            CASE ( DATA_ORDER_YZX )
               intermediate_grid%nids = nest_grid%sd33 ; intermediate_grid%njds = nest_grid%sd31
               intermediate_grid%nide = nest_grid%ed33 ; intermediate_grid%njde = nest_grid%sd31
         END SELECT
         intermediate_grid%nids = ids
         intermediate_grid%nide = ide
         intermediate_grid%njds = jds
         intermediate_grid%njde = jde

         intermediate_grid%sm31x                           = c_sm1x
         intermediate_grid%em31x                           = c_em1x
         intermediate_grid%sm32x                           = c_sm2x
         intermediate_grid%em32x                           = c_em2x
         intermediate_grid%sm33x                           = c_sm3x
         intermediate_grid%em33x                           = c_em3x
         intermediate_grid%sm31y                           = c_sm1y
         intermediate_grid%em31y                           = c_em1y
         intermediate_grid%sm32y                           = c_sm2y
         intermediate_grid%em32y                           = c_em2y
         intermediate_grid%sm33y                           = c_sm3y
         intermediate_grid%em33y                           = c_em3y

#if (DA_CORE == 1)
         intermediate_grid%sp31x                           = c_sp1x
         intermediate_grid%ep31x                           = c_ep1x
         intermediate_grid%sp32x                           = c_sp2x
         intermediate_grid%ep32x                           = c_ep2x
         intermediate_grid%sp33x                           = c_sp3x
         intermediate_grid%ep33x                           = c_ep3x
         intermediate_grid%sp31y                           = c_sp1y
         intermediate_grid%ep31y                           = c_ep1y
         intermediate_grid%sp32y                           = c_sp2y
         intermediate_grid%ep32y                           = c_ep2y
         intermediate_grid%sp33y                           = c_sp3y
         intermediate_grid%ep33y                           = c_ep3y
#endif

#if ( ( defined(SGIALTIX) || defined(FUJITSU_FX10) || defined(KEEP_INT_AROUND) ) && (! defined(MOVE_NESTS) ) )
         ! allocate space for the intermediate domain
!         CALL alloc_space_field ( intermediate_grid, intermediate_grid%id , 1, 2 , .TRUE., intercomm_active( intermediate_grid%id ), &   ! use same id as nest
         CALL alloc_space_field ( intermediate_grid, intermediate_grid%id , 1, 2 , .TRUE., nest_grid%active_this_task, &   ! use same id as nest
                               c_sd1, c_ed1, c_sd2, c_ed2, c_sd3, c_ed3,       &
                               c_sm1,  c_em1,  c_sm2,  c_em2,  c_sm3,  c_em3,  &
                               c_sp1,  c_ep1,  c_sp2,  c_ep2,  c_sp3,  c_ep3,  &
                               c_sm1x, c_em1x, c_sm2x, c_em2x, c_sm3x, c_em3x, &
                               c_sm1y, c_em1y, c_sm2y, c_em2y, c_sm3y, c_em3y, &
                               c_sm1x, c_em1x, c_sm2x, c_em2x, c_sm3x, c_em3x, &   ! x-xpose
                               c_sm1y, c_em1y, c_sm2y, c_em2y, c_sm3y, c_em3y  )   ! y-xpose
#endif
         intermediate_grid%sd31                            =   c_sd1
         intermediate_grid%ed31                            =   c_ed1
         intermediate_grid%sp31                            = c_sp1
         intermediate_grid%ep31                            = c_ep1
         intermediate_grid%sm31                            = c_sm1
         intermediate_grid%em31                            = c_em1
         intermediate_grid%sd32                            =   c_sd2
         intermediate_grid%ed32                            =   c_ed2
         intermediate_grid%sp32                            = c_sp2
         intermediate_grid%ep32                            = c_ep2
         intermediate_grid%sm32                            = c_sm2
         intermediate_grid%em32                            = c_em2
         intermediate_grid%sd33                            =   c_sd3
         intermediate_grid%ed33                            =   c_ed3
         intermediate_grid%sp33                            = c_sp3
         intermediate_grid%ep33                            = c_ep3
         intermediate_grid%sm33                            = c_sm3
         intermediate_grid%em33                            = c_em3

         CALL med_add_config_info_to_grid ( intermediate_grid )

         intermediate_grid%dx = parent%dx
         intermediate_grid%dy = parent%dy
         intermediate_grid%dt = parent%dt
      END IF

      RETURN
  END SUBROUTINE patch_domain_rsl_lite

  SUBROUTINE compute_memory_dims_rsl_lite  (      &
                   id , maxhalowidth ,            &
                   shw , bdx,  bdy ,              &
                   ids,  ide,  jds,  jde,  kds,  kde, &
                   ims,  ime,  jms,  jme,  kms,  kme, &
                   imsx, imex, jmsx, jmex, kmsx, kmex, &
                   imsy, imey, jmsy, jmey, kmsy, kmey, &
                   ips,  ipe,  jps,  jpe,  kps,  kpe, &
                   ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                   ipsy, ipey, jpsy, jpey, kpsy, kpey )

    IMPLICIT NONE
    INTEGER, INTENT(IN)               ::  id , maxhalowidth
    INTEGER, INTENT(IN)               ::  shw, bdx, bdy
    INTEGER, INTENT(IN)     ::  ids, ide, jds, jde, kds, kde
    INTEGER, INTENT(OUT)    ::  ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(OUT)    ::  imsx, imex, jmsx, jmex, kmsx, kmex
    INTEGER, INTENT(OUT)    ::  imsy, imey, jmsy, jmey, kmsy, kmey
    INTEGER, INTENT(OUT)    ::  ips, ipe, jps, jpe, kps, kpe
    INTEGER, INTENT(OUT)    ::  ipsx, ipex, jpsx, jpex, kpsx, kpex
    INTEGER, INTENT(OUT)    ::  ipsy, ipey, jpsy, jpey, kpsy, kpey

    INTEGER Px, Py, P, i, j, k, ierr

#if ( ! NMM_CORE == 1 )

! xy decomposition

    ips = -1
    j = jds
    ierr = 0
    DO i = ids, ide
!       CALL task_for_point ( i, j, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py, &
       CALL task_for_point ( i, j, ids, ide, jds, jde, nest_pes_x(id), nest_pes_y(id), Px, Py, &
                             minx, miny, ierr )
       IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (c)')
       IF ( Px .EQ. mytask_x ) THEN
          ipe = i
          IF ( ips .EQ. -1 ) ips = i
       END IF
    END DO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message("<stdin>",__LINE__)
    END IF
    ! handle setting the memory dimensions where there are no X elements assigned to this proc
    IF (ips .EQ. -1 ) THEN
       ipe = -1
       ips = 0
    END IF

    jps = -1
    i = ids
    ierr = 0
    DO j = jds, jde
!       CALL task_for_point ( i, j, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py, &
       CALL task_for_point ( i, j, ids, ide, jds, jde, nest_pes_x(id), nest_pes_y(id), Px, Py, &
                             minx, miny, ierr )
       IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (d)')
       IF ( Py .EQ. mytask_y ) THEN
          jpe = j
          IF ( jps .EQ. -1 ) jps = j
       END IF
    END DO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message("<stdin>",__LINE__)
    END IF
    ! handle setting the memory dimensions where there are no Y elements assigned to this proc
    IF (jps .EQ. -1 ) THEN
       jpe = -1
       jps = 0
    END IF

!begin: wig; 12-Mar-2008
! This appears redundant with the conditionals above, but we get cases with only
! one of the directions being set to "missing" when turning off extra processors.
! This may break the handling of setting only one of nproc_x or nproc_y via the namelist.
    IF (ipe .EQ. -1 .or. jpe .EQ. -1) THEN
       ipe = -1
       ips = 0
       jpe = -1
       jps = 0
    END IF
!end: wig; 12-Mar-2008

!
! description of transpose decomposition strategy for RSL LITE. 20061231jm
!
! Here is the tranpose scheme that is implemented for RSL_LITE. Upper-case
! XY corresponds to the dimension of the processor mesh, lower-case xyz
! corresponds to grid dimension.
!
!      xy        zy        zx
!
!     XxYy <--> XzYy <--> XzYx <- note x decomposed over Y procs
!       ^                  ^
!       |                  |
!       +------------------+  <- this edge is costly; see below
!
! The aim is to avoid all-to-all communication over whole
! communicator. Instead, when possible, use a transpose scheme that requires
! all-to-all within dimensional communicators; that is, communicators
! defined for the processes in a rank or column of the processor mesh. Note,
! however, it is not possible to create a ring of transposes between
! xy-yz-xz decompositions without at least one of the edges in the ring
! being fully all-to-all (in other words, one of the tranpose edges must
! rotate and not just transpose a plane of the model grid within the
! processor mesh). The issue is then, where should we put this costly edge
! in the tranpose scheme we chose? To avoid being completely arbitrary,
! we chose a scheme most natural for models that use parallel spectral
! transforms, where the costly edge is the one that goes from the xz to
! the xy decomposition.  (May be implemented as just a two step transpose
! back through yz).
!
! Additional notational convention, below. The 'x' or 'y' appended to the
! dimension start or end variable refers to which grid dimension is all
! on-processor in the given decomposition. That is ipsx and ipex are the
! start and end for the i-dimension in the zy decomposition where x is
! on-processor. ('z' is assumed for xy decomposition and not appended to
! the ips, ipe, etc. variable names).
!

! XzYy decomposition

    kpsx = -1
    j = jds ;
    ierr = 0
    DO k = kds, kde
!       CALL task_for_point ( k, j, kds, kde, jds, jde, ntasks_x, ntasks_y, Px, Py, &
       CALL task_for_point ( k, j, kds, kde, jds, jde, nest_pes_x(id), nest_pes_y(id), Px, Py, &
                             minx, miny, ierr )
       IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (e)')
       IF ( Px .EQ. mytask_x ) THEN
          kpex = k
          IF ( kpsx .EQ. -1 ) kpsx = k
       END IF
    END DO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message("<stdin>",__LINE__)
    END IF
    
! handle case where no levels are assigned to this process
! no iterations.  Do same for I and J. Need to handle memory alloc below.
    IF (kpsx .EQ. -1 ) THEN
       kpex = -1
       kpsx = 0
    END IF

    jpsx = -1
    k = kds ;
    ierr = 0
    DO j = jds, jde
!       CALL task_for_point ( k, j, kds, kde, jds, jde, ntasks_x, ntasks_y, Px, Py, &
       CALL task_for_point ( k, j, kds, kde, jds, jde, nest_pes_x(id), nest_pes_y(id), Px, Py, &
                             minx, miny, ierr )
       IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (f)')
       IF ( Py .EQ. mytask_y ) THEN
          jpex = j
          IF ( jpsx .EQ. -1 ) jpsx = j
       END IF
    END DO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message("<stdin>",__LINE__)
    END IF
    IF (jpsx .EQ. -1 ) THEN
       jpex = -1
       jpsx = 0
    END IF

!begin: wig; 12-Mar-2008
! This appears redundant with the conditionals above, but we get cases with only
! one of the directions being set to "missing" when turning off extra processors.
! This may break the handling of setting only one of nproc_x or nproc_y via the namelist.
    IF (jpex .EQ. -1) THEN
       ipex = -1
       ipsx = 0
       jpex = -1
       jpsx = 0
    END IF
!end: wig; 12-Mar-2008

! XzYx decomposition  (note, x grid dim is decomposed over Y processor dim)

    kpsy = kpsx   ! same as above
    kpey = kpex   ! same as above

    ipsy = -1
    k = kds ;
    ierr = 0
    DO i = ids, ide
!       CALL task_for_point ( i, k, ids, ide, kds, kde, ntasks_y, ntasks_x, Py, Px, &
       CALL task_for_point ( i, k, ids, ide, kds, kde, nest_pes_y(id), nest_pes_x(id), Py, Px, &
                             miny, minx, ierr )
       IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (g)')
       IF ( Py .EQ. mytask_y ) THEN
          ipey = i
          IF ( ipsy .EQ. -1 ) ipsy = i
       END IF
    END DO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message("<stdin>",__LINE__)
    END IF
    IF (ipsy .EQ. -1 ) THEN
       ipey = -1
       ipsy = 0
    END IF


#else

! In case of NMM CORE, the domain only ever runs from ids..ide-1 and jds..jde-1 so
! adjust decomposition to reflect.  20051020 JM
    ips = -1
    j = jds
    ierr = 0
    DO i = ids, ide-1
!jm       CALL task_for_point ( i, j, ids, ide-1, jds, jde-1, ntasks_x, ntasks_y, Px, Py, &
       CALL task_for_point ( i, j, ids, ide-1, jds, jde-1, nest_pes_x(id), nest_pes_y(id), Px, Py, &
                             minx, miny, ierr )
       IF ( Px .EQ. mytask_x ) THEN
          ipe = i
!          IF ( Px .EQ. ntasks_x-1 ) ipe = ipe + 1
          IF ( Px .EQ. nest_pes_x(id)-1 ) ipe = ipe + 1
          IF ( ips .EQ. -1 ) ips = i
       END IF
    END DO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message("<stdin>",__LINE__)
    END IF
    jps = -1
    i = ids ;
    ierr = 0
    DO j = jds, jde-1
!jm       CALL task_for_point ( i, j, ids, ide-1, jds, jde-1, ntasks_x, ntasks_y, Px, Py, &
       CALL task_for_point ( i, j, ids, ide-1, jds, jde-1, nest_pes_x(id), nest_pes_y(id), Px, Py, &
                             minx, miny, ierr )
       IF ( Py .EQ. mytask_y ) THEN
          jpe = j
!          IF ( Py .EQ. ntasks_y-1 ) jpe = jpe + 1
          IF ( Py .EQ. nest_pes_y(id)-1 ) jpe = jpe + 1
          IF ( jps .EQ. -1 ) jps = j
       END IF
    END DO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message("<stdin>",__LINE__)
    END IF
#endif

! extend the patch dimensions out shw along edges of domain
    IF ( ips < ipe .and. jps < jpe ) THEN           !wig; 11-Mar-2008
       IF ( mytask_x .EQ. 0 ) THEN
          ips = ips - shw
          ipsy = ipsy - shw
       END IF
!       IF ( mytask_x .EQ. ntasks_x-1 ) THEN
       IF ( mytask_x .EQ. nest_pes_x(id)-1 ) THEN
          ipe = ipe + shw
          ipey = ipey + shw
       END IF
       IF ( mytask_y .EQ. 0 ) THEN
          jps = jps - shw
          jpsx = jpsx - shw
       END IF
!       IF ( mytask_y .EQ. ntasks_y-1 ) THEN
       IF ( mytask_y .EQ. nest_pes_y(id)-1 ) THEN
          jpe = jpe + shw
          jpex = jpex + shw
       END IF
    END IF                                           !wig; 11-Mar-2008

    kps = 1
    kpe = kde-kds+1

    kms = 1
    kme = kpe
    kmsx = kpsx
    kmex = kpex
    kmsy = kpsy
    kmey = kpey

    ! handle setting the memory dimensions where there are no levels assigned to this proc
    IF ( kpsx .EQ. 0 .AND. kpex .EQ. -1 ) THEN
      kmsx = 0
      kmex = 0
    END IF
    IF ( kpsy .EQ. 0 .AND. kpey .EQ. -1 ) THEN
      kmsy = 0
      kmey = 0
    END IF

    IF ( (jps .EQ. 0 .AND. jpe .EQ. -1) .OR. (ips .EQ. 0 .AND. ipe .EQ. -1) ) THEN
      ims = 0
      ime = 0
    ELSE
      ims = max( ips - max(shw,maxhalowidth), ids - bdx ) - 1
      ime = min( ipe + max(shw,maxhalowidth), ide + bdx ) + 1
#ifdef INTEL_ALIGN64
! align on 64 byte boundaries if -align array64byte
      ims = ips-CHUNK
      ime = ime + (CHUNK-mod(ime-ims+1,CHUNK))
#endif
    END IF
    imsx = ids
    imex = ide
    ipsx = imsx
    ipex = imex
    ! handle setting the memory dimensions where there are no Y elements assigned to this proc
    IF ( ipsy .EQ. 0 .AND. ipey .EQ. -1 ) THEN
      imsy = 0
      imey = 0
    ELSE
      imsy = ipsy
      imey = ipey
    END IF

    IF ( (jps .EQ. 0 .AND. jpe .EQ. -1) .OR. (ips .EQ. 0 .AND. ipe .EQ. -1) ) THEN
      jms = 0
      jme = 0
    ELSE
      jms = max( jps - max(shw,maxhalowidth), jds - bdy ) - 1
      jme = min( jpe + max(shw,maxhalowidth), jde + bdy ) + 1
    END IF
    jmsx = jpsx
    jmex = jpex
    jmsy = jds
    jmey = jde
    ! handle setting the memory dimensions where there are no X elements assigned to this proc
    IF ( jpsx .EQ. 0 .AND. jpex .EQ. -1 ) THEN
      jmsx = 0
      jmex = 0
      jpsy = 0
      jpey = -1
    ELSE
      jpsy = jmsy
      jpey = jmey
    END IF

  END SUBROUTINE compute_memory_dims_rsl_lite

! internal, used below for switching the argument to MPI calls
! if reals are being autopromoted to doubles in the build of WRF
   INTEGER function getrealmpitype()
#ifndef STUBMPI
      IMPLICIT NONE
      INTEGER rtypesize, dtypesize, ierr
      CALL mpi_type_size ( MPI_REAL, rtypesize, ierr )
      CALL mpi_type_size ( MPI_DOUBLE_PRECISION, dtypesize, ierr )
      IF ( RWORDSIZE .EQ. rtypesize ) THEN
        getrealmpitype = MPI_REAL
      ELSE IF ( RWORDSIZE .EQ. dtypesize ) THEN
        getrealmpitype = MPI_DOUBLE_PRECISION
      ELSE
        CALL wrf_error_fatal ( 'RWORDSIZE or DWORDSIZE does not match any MPI type' )
      END IF
#else
! required dummy initialization for function that is never called
      getrealmpitype = 1
#endif
      RETURN
   END FUNCTION getrealmpitype

   REAL FUNCTION wrf_dm_max_int ( inval )
      IMPLICIT NONE
#ifndef STUBMPI
      INTEGER, intent(in) :: inval
      INTEGER :: ierr, retval
      CALL mpi_allreduce ( inval, retval , 1, MPI_INTEGER, MPI_MAX, local_communicator, ierr )
      wrf_dm_max_int = retval
#else
      INTEGER, intent(in) :: inval
      wrf_dm_max_int = inval
#endif
   END FUNCTION wrf_dm_max_int

   REAL FUNCTION wrf_dm_max_real ( inval )
      IMPLICIT NONE
#ifndef STUBMPI
      REAL inval, retval
      INTEGER comm,ierr
      CALL wrf_get_dm_communicator(comm)
      CALL mpi_allreduce ( inval, retval , 1, getrealmpitype(), MPI_MAX, comm, ierr )
      wrf_dm_max_real = retval
#else
      REAL inval
      wrf_dm_max_real = inval
#endif
   END FUNCTION wrf_dm_max_real

   REAL FUNCTION wrf_dm_min_real ( inval )
      IMPLICIT NONE
#ifndef STUBMPI
      REAL inval, retval
      INTEGER comm,ierr
      CALL wrf_get_dm_communicator(comm)
      CALL mpi_allreduce ( inval, retval , 1, getrealmpitype(), MPI_MIN, comm, ierr )
      wrf_dm_min_real = retval
#else
      REAL inval
      wrf_dm_min_real = inval
#endif
   END FUNCTION wrf_dm_min_real

   SUBROUTINE wrf_dm_min_reals ( inval, retval, n )
      IMPLICIT NONE
      INTEGER n
      REAL inval(*)
      REAL retval(*)
#ifndef STUBMPI
      INTEGER comm,ierr
      CALL wrf_get_dm_communicator(comm)
      CALL mpi_allreduce ( inval, retval , n, getrealmpitype(), MPI_MIN, comm, ierr )
#else
      retval(1:n) = inval(1:n)
#endif
   END SUBROUTINE wrf_dm_min_reals

   FUNCTION wrf_dm_sum_real8 ( inval )
     ! Forced eight byte real sum needed for calculating an accurate
     ! mean motion in HWRF moduel_tracker.
      IMPLICIT NONE
#ifndef STUBMPI
      REAL*8 inval, retval, wrf_dm_sum_real8
      INTEGER comm,ierr
      CALL wrf_get_dm_communicator(comm)
      CALL mpi_allreduce ( inval, retval , 1, MPI_REAL8, MPI_SUM, comm, ierr )
      wrf_dm_sum_real8 = retval
#else
      REAL*8 wrf_dm_sum_real8,inval
      wrf_dm_sum_real8 = inval
#endif
   END FUNCTION wrf_dm_sum_real8

   REAL FUNCTION wrf_dm_sum_real ( inval )
      IMPLICIT NONE
#ifndef STUBMPI
      REAL inval, retval
      INTEGER comm,ierr
      CALL wrf_get_dm_communicator(comm)
      CALL mpi_allreduce ( inval, retval , 1, getrealmpitype(), MPI_SUM, comm, ierr )
      wrf_dm_sum_real = retval
#else
      REAL inval
      wrf_dm_sum_real = inval
#endif
   END FUNCTION wrf_dm_sum_real

   SUBROUTINE wrf_dm_sum_reals (inval, retval)
      IMPLICIT NONE
      REAL, INTENT(IN)  :: inval(:)
      REAL, INTENT(OUT) :: retval(:)
#ifndef STUBMPI
      INTEGER comm,ierr
      CALL wrf_get_dm_communicator(comm)
      CALL mpi_allreduce ( inval, retval, SIZE(inval), getrealmpitype(), MPI_SUM, comm, ierr )
#else
      retval = inval
#endif
   END SUBROUTINE wrf_dm_sum_reals

   INTEGER FUNCTION wrf_dm_sum_integer ( inval )
      IMPLICIT NONE
#ifndef STUBMPI
      INTEGER inval, retval
      INTEGER comm,ierr
      CALL wrf_get_dm_communicator(comm)
      CALL mpi_allreduce ( inval, retval , 1, MPI_INTEGER, MPI_SUM, comm, ierr )
      wrf_dm_sum_integer = retval
#else
      INTEGER inval
      wrf_dm_sum_integer = inval
#endif
   END FUNCTION wrf_dm_sum_integer

   SUBROUTINE wrf_dm_sum_integers (inval, retval)
      IMPLICIT NONE
      INTEGER, INTENT(IN)  :: inval(:)
      INTEGER, INTENT(OUT) :: retval(:)
#ifndef STUBMPI
      INTEGER comm,ierr
      CALL wrf_get_dm_communicator(comm)
      CALL mpi_allreduce ( inval, retval, SIZE(inval), MPI_INTEGER, MPI_SUM, comm, ierr )
#else
      retval = inval
#endif
   END SUBROUTINE wrf_dm_sum_integers

#if ( HWRF == 1 )
   SUBROUTINE wrf_dm_minloc_real ( val, lat, lon, z, idex, jdex )
#ifndef STUBMPI
      use mpi
      IMPLICIT NONE
      REAL val, lat, lon, z
      INTEGER idex, jdex, ierr, mrank, comm
      REAL inreduce(2), outreduce(2), bcast(5)

      inreduce=(/ val, real(mytask) /)
      CALL wrf_get_dm_communicator(comm)
      call MPI_Allreduce(inreduce,outreduce,1,MPI_2REAL,MPI_MINLOC,&
           comm,ierr)
      val=outreduce(1)
      mrank=outreduce(2)
      bcast=(/ lat,lon,z,real(idex),real(jdex) /)
      call MPI_Bcast(bcast,5,MPI_REAL,mrank,comm,ierr)
      lat=bcast(1)
      lon=bcast(2)
      z=bcast(3)
      idex=bcast(4)
      jdex=bcast(5)
#else
      IMPLICIT NONE
      REAL val,lat,lon,z
      INTEGER idex, jdex
#endif
   END SUBROUTINE wrf_dm_minloc_real
   SUBROUTINE wrf_dm_maxloc_real ( val, lat, lon, z, idex, jdex )
#ifndef STUBMPI
      use mpi
      IMPLICIT NONE
      REAL val, lat, lon, z
      INTEGER idex, jdex, ierr, mrank, comm
      REAL inreduce(2), outreduce(2), bcast(5)

      inreduce=(/ val, real(mytask) /)
      CALL wrf_get_dm_communicator(comm)
      call MPI_Allreduce(inreduce,outreduce,1,MPI_2REAL,MPI_MAXLOC,&
           comm,ierr)
      val=outreduce(1)
      mrank=outreduce(2)
      bcast=(/ lat,lon,z,real(idex),real(jdex) /)
      call MPI_Bcast(bcast,5,MPI_REAL,mrank,comm,ierr)
      lat=bcast(1)
      lon=bcast(2)
      z=bcast(3)
      idex=bcast(4)
      jdex=bcast(5)
#else
      IMPLICIT NONE
      REAL val,lat,lon,z
      INTEGER idex, jdex
#endif
   END SUBROUTINE wrf_dm_maxloc_real
#endif

   INTEGER FUNCTION wrf_dm_bxor_integer ( inval )
      IMPLICIT NONE
#ifndef STUBMPI
      INTEGER inval, retval
      INTEGER comm, ierr
      CALL wrf_get_dm_communicator(comm)
      CALL mpi_allreduce ( inval, retval , 1, MPI_INTEGER, MPI_BXOR, comm, ierr )
      wrf_dm_bxor_integer = retval
#else
      INTEGER inval
      wrf_dm_bxor_integer = inval
#endif
   END FUNCTION wrf_dm_bxor_integer


LOGICAL FUNCTION wrf_dm_lor_logical ( inval )
      IMPLICIT NONE
#ifndef STUBMPI
      LOGICAL inval, retval
      INTEGER comm, ierr
      CALL wrf_get_dm_communicator(comm)
      CALL mpi_allreduce ( inval, retval , 1, MPI_LOGICAL, MPI_LOR, comm, ierr )
      wrf_dm_lor_logical = retval
#else
      LOGICAL inval
      wrf_dm_lor_logical = inval
#endif
   END FUNCTION wrf_dm_lor_logical


LOGICAL FUNCTION wrf_dm_land_logical ( inval )
      IMPLICIT NONE
#ifndef STUBMPI
      LOGICAL inval, retval
      INTEGER comm, ierr
      CALL wrf_get_dm_communicator(comm)
      CALL mpi_allreduce ( inval, retval , 1, MPI_LOGICAL, MPI_LAND, comm, ierr )
      wrf_dm_land_logical = retval
#else
      LOGICAL inval
      wrf_dm_land_logical = inval
#endif
   END FUNCTION wrf_dm_land_logical


   SUBROUTINE wrf_dm_maxval_real ( val, idex, jdex )
# ifndef STUBMPI
      use mpi
      IMPLICIT NONE
      REAL val
      INTEGER :: idex, jdex, i, comm
      INTEGER :: bcast(2),mrank
      REAL :: inreduce(2),outreduce(2)

      inreduce=(/ val, real(mytask) /)
      bcast=(/ idex,jdex /)
      CALL wrf_get_dm_communicator(comm)
      call MPI_Allreduce(inreduce,outreduce,1,MPI_2REAL,&
                         MPI_MAXLOC,comm,i)
      mrank=outreduce(2)
      val=outreduce(1)
      call MPI_Bcast(bcast,2,MPI_REAL,mrank,comm,i)
      idex=bcast(1)
      jdex=bcast(2)
# else
      IMPLICIT NONE
      REAL val
      INTEGER idex, jdex, ierr
# endif
    END SUBROUTINE wrf_dm_maxval_real

   SUBROUTINE wrf_dm_minval_real ( val, idex, jdex )
# ifndef STUBMPI
      use mpi
      IMPLICIT NONE
      REAL val
      INTEGER :: idex, jdex, i, comm
      INTEGER :: bcast(2),mrank
      REAL :: inreduce(2),outreduce(2)

      inreduce=(/ val, real(mytask) /)
      bcast=(/ idex,jdex /)
      CALL wrf_get_dm_communicator(comm)
      call MPI_Allreduce(inreduce,outreduce,1,MPI_2REAL,&
                         MPI_MINLOC,comm,i)
      mrank=outreduce(2)
      val=outreduce(1)
      call MPI_Bcast(bcast,2,MPI_REAL,mrank,comm,i)
      idex=bcast(1)
      jdex=bcast(2)
# else
      IMPLICIT NONE
      REAL val
      INTEGER idex, jdex
# endif
    END SUBROUTINE wrf_dm_minval_real

#ifndef PROMOTE_FLOAT
   SUBROUTINE wrf_dm_maxval_doubleprecision ( val, idex, jdex )
# ifndef STUBMPI
      use mpi
      IMPLICIT NONE
      DOUBLE PRECISION val
      INTEGER :: idex, jdex, i, comm
      INTEGER :: bcast(2),mrank
      DOUBLE PRECISION :: inreduce(2),outreduce(2)

      inreduce=(/ val, dble(mytask) /)
      bcast=(/ idex,jdex /)
      CALL wrf_get_dm_communicator(comm)
      call MPI_Allreduce(inreduce,outreduce,1,MPI_2DOUBLE_PRECISION,&
                         MPI_MAXLOC,comm,i)
      mrank=outreduce(2)
      val=outreduce(1)
      call MPI_Bcast(bcast,2,MPI_DOUBLE_PRECISION,mrank,comm,i)
      idex=bcast(1)
      jdex=bcast(2)
# else
      IMPLICIT NONE
      DOUBLE PRECISION val
      INTEGER idex, jdex, ierr
# endif
   END SUBROUTINE wrf_dm_maxval_doubleprecision

   SUBROUTINE wrf_dm_minval_doubleprecision ( val, idex, jdex )
# ifndef STUBMPI
      use mpi
      IMPLICIT NONE
      DOUBLE PRECISION val
      INTEGER :: idex, jdex, i, comm
      INTEGER :: bcast(2),mrank
      DOUBLE PRECISION :: inreduce(2),outreduce(2)

      inreduce=(/ val, dble(mytask) /)
      bcast=(/ idex,jdex /)
      CALL wrf_get_dm_communicator(comm)
      call MPI_Allreduce(inreduce,outreduce,1,MPI_2DOUBLE_PRECISION,&
                         MPI_MINLOC,comm,i)
      mrank=outreduce(2)
      val=outreduce(1)
      call MPI_Bcast(bcast,2,MPI_DOUBLE_PRECISION,mrank,comm,i)
      idex=bcast(1)
      jdex=bcast(2)
# else
      IMPLICIT NONE
      DOUBLE PRECISION val
      INTEGER idex, jdex, ierr
# endif
   END SUBROUTINE wrf_dm_minval_doubleprecision
#endif

   SUBROUTINE wrf_dm_maxval_integer ( val, idex, jdex )
# ifndef STUBMPI
      use mpi
      IMPLICIT NONE
      INTEGER val
      INTEGER :: idex, jdex, i, comm
      INTEGER :: bcast(2),mrank
      INTEGER :: inreduce(2),outreduce(2)

      inreduce=(/ val, mytask /)
      bcast=(/ idex,jdex /)
      CALL wrf_get_dm_communicator(comm)
      call MPI_Allreduce(inreduce,outreduce,1,MPI_2INTEGER,&
                         MPI_MAXLOC,comm,i)
      mrank=outreduce(2)
      val=outreduce(1)
      call MPI_Bcast(bcast,2,MPI_INTEGER,mrank,comm,i)
      idex=bcast(1)
      jdex=bcast(2)
# else
      IMPLICIT NONE
      INTEGER val
      INTEGER idex, jdex, ierr
# endif
    END SUBROUTINE wrf_dm_maxval_integer

   SUBROUTINE wrf_dm_minval_integer ( val, idex, jdex )
# ifndef STUBMPI
      use mpi
      IMPLICIT NONE
      INTEGER val
      INTEGER :: idex, jdex, i, comm
      INTEGER :: bcast(2),mrank
      INTEGER :: inreduce(2),outreduce(2)

      inreduce=(/ val, mytask /)
      bcast=(/ idex,jdex /)
      CALL wrf_get_dm_communicator(comm)
      call MPI_Allreduce(inreduce,outreduce,1,MPI_2INTEGER,&
                         MPI_MINLOC,comm,i)
      mrank=outreduce(2)
      val=outreduce(1)
      call MPI_Bcast(bcast,2,MPI_INTEGER,mrank,comm,i)
      idex=bcast(1)
      jdex=bcast(2)
# else
      IMPLICIT NONE
      INTEGER val
      INTEGER idex, jdex, ierr
# endif
    END SUBROUTINE wrf_dm_minval_integer

   SUBROUTINE hwrf_coupler_init
#if ( HWRF == 1 )
# ifndef STUBMPI
      IMPLICIT NONE
      LOGICAL mpi_inited
      INTEGER mpi_comm_here,ierr
      CALL MPI_INITIALIZED( mpi_inited, ierr )
      IF ( .NOT. mpi_inited ) THEN
        IF ( coupler_on ) THEN
           CALL cpl_init( mpi_comm_here )
        ELSE
           CALL mpi_init ( ierr )
           mpi_comm_here = MPI_COMM_WORLD
        END IF
        CALL atm_cmp_start( mpi_comm_here )
        CALL wrf_set_dm_communicator( mpi_comm_here )
        CALL wrf_termio_dup( mpi_comm_here )
      END IF
      RETURN
# endif
#endif
   END SUBROUTINE hwrf_coupler_init

   SUBROUTINE split_communicator
#ifndef STUBMPI
      IMPLICIT NONE
      LOGICAL mpi_inited
!      INTEGER mpi_comm_here, mpi_comm_local, comdup, comdup2, origmytask,  mytask, ntasks, ierr, io_status
      INTEGER mpi_comm_here, mpi_comm_local, comdup, comdup2, origmytask,  ierr, io_status
      INTEGER mpi_comm_me_and_mom
      INTEGER coords(3)
      INTEGER mytask_local,ntasks_local,num_compute_tasks
# if defined(_OPENMP) && defined(MPI2_THREAD_SUPPORT)
      INTEGER thread_support_provided, thread_support_requested
# endif
      INTEGER i, j, k, x, y, n_x, n_y
      INTEGER iii
      INTEGER, ALLOCATABLE :: icolor(:),icolor2(:),idomain(:)
      INTEGER comm_id
!
! Communicator definition                       Domains
!                                                       
!  6 pe Example Comm   PEs                        (1)
!        COMM_WORLD  0 1 2 3 4 5                  / !               1    0 1 2 3 4 5                (2) (3)
!               2    0 1                         |
!               3        0 1 2 3                (4)
!               4    0 1
!
!    Notes: 1. No requirement that any communicator be all tasks
!           2. A task may be a member of an arbitrary number
!              of local communicators (But you may not want to do this)
!
!
!  Namelist Split Settings (for 3 comms, 4 domains)
!  Revised namelist semantics -- no need for binding nests to separately defined communicators
!
!   (domain_id)     1    2    3    4
!   parent_id       -    1    1    2
!   comm_start      0    0    2    0
!   nest_pes_x      2    1    2    1
!   nest_pes_y      3    2    2    2
!   
!! superceded
!!  Namelist Split Settings (for 3 comms, 4 domains)
!!   (comm_id)       1    2    3    ...
!!   comm_start      0    0    2
!!   comm_pes_x      2    1    2
!!   comm_pes_y      3    2    2
!!
!!  Domain definitions
!!   (domain_id)     1    2    3    4
!!   parent_id       -    1    1    2
!!   comm_domain     1    2    3    2
!! * nest_pes_x      2    1    2    1
!! * nest_pes_y      3    2    2    2
!!
!!   [* nest_pes_x is comm_pes_x(comm_domain(domain_id))]
!

      INTEGER dims(3)
! for parallel nesting, 201408, jm
      INTEGER :: id
      INTEGER :: intercomm
      INTEGER :: domain_id,par_id,nest_id,kid_id
      INTEGER :: mytask_me_and_mom, ntasks_me_and_mom, remote_leader
      LOGICAL :: inthisone
      LOGICAL :: mytask_is_nest, mytask_is_par,isperiodic(3)
! for new quilting
      LOGICAL :: quilting_is_turned_off

!!!!! needed to sneak-peek the registry to get parent_id
! define as temporaries
#include "namelist_defines.inc"

! Statements that specify the namelists
#include "namelist_statements.inc"

      CALL MPI_INITIALIZED( mpi_inited, ierr )
      IF ( .NOT. mpi_inited ) THEN
# if defined(_OPENMP) && defined(MPI2_THREAD_SUPPORT)
        thread_support_requested = MPI_THREAD_FUNNELED
        CALL mpi_init_thread ( thread_support_requested, thread_support_provided, ierr )
        IF ( thread_support_provided .lt. thread_support_requested ) THEN
           CALL WRF_ERROR_FATAL( "failed to initialize MPI thread support")
        END IF
        mpi_comm_here = MPI_COMM_WORLD
# else
#if ( DA_CORE != 1 )
        IF ( coupler_on ) THEN
           CALL cpl_init( mpi_comm_here )
        ELSE
#endif
           CALL mpi_init ( ierr )
           mpi_comm_here = MPI_COMM_WORLD
#if ( DA_CORE != 1 )
        END IF
#endif
# endif
#if ( HWRF == 1 )
!!!!! jm 20150807 note that for HWRF, this will not be called here because of the call to hwrf_coupler_init (defined above) in init_modules
!!!!        CALL atm_cmp_start( mpi_comm_here )   ! atmospheric side of HWRF coupler will split MPI_COMM_WORLD and return communicator as argument
#endif
        CALL wrf_set_dm_communicator( mpi_comm_here )
        CALL wrf_termio_dup( mpi_comm_here )
#if (WRFPLUS == 1)
      ELSE
        CALL wrf_set_dm_communicator( local_communicator )
#endif
      END IF
! this should have been reset by init_module_wrf_quilt to be just the compute tasks
      CALL wrf_get_dm_communicator( mpi_comm_here )

      CALL MPI_Comm_rank ( mpi_comm_here, mytask_local, ierr ) ;
      CALL MPI_Comm_size ( mpi_comm_here, ntasks_local, ierr ) ;
      mpi_comm_allcompute = mpi_comm_here

      IF ( mytask_local .EQ. 0 ) THEN
        max_dom = 1
        OPEN ( unit=27, file="namelist.input", form="formatted", status="old" )
        READ ( UNIT = 27 , NML = domains , IOSTAT=io_status )
        REWIND(27)
        nio_groups = 1
        nio_tasks_per_group  = 0
        poll_servers = .false.
        READ ( 27 , NML = namelist_quilt, IOSTAT=io_status )
        CLOSE(27)
      END IF
      CALL mpi_bcast( nio_tasks_per_group  , max_domains , MPI_INTEGER , 0 , mpi_comm_here, ierr )
      CALL mpi_bcast( nio_groups , 1 , MPI_INTEGER , 0 , mpi_comm_here, ierr )
      CALL mpi_bcast( max_dom, 1 , MPI_INTEGER , 0 , mpi_comm_here, ierr )
      CALL mpi_bcast( parent_id, max_domains , MPI_INTEGER , 0 , mpi_comm_here, ierr )
#if ( HWRF == 1 )
! check to make sure that if nio_tasks_per_group is non-zero for any domain it has to be non-zero for all of them
      i = MAXVAL(nio_tasks_per_group(1:max_dom))
      IF ( i .GT. 0 .AND. nio_groups .GT. 0 ) THEN
        DO id = 1, max_dom
          IF ( nio_tasks_per_group(id) .LE. 0 ) THEN
             CALL wrf_error_fatal( &
'If nio_tasks_per_group in namelist.input is non-zero for any domain, every active domain must have a non-zero value in nio_tasks_per_group')
          END IF
        END DO
      END IF

      num_io_tasks = 0
      DO id = 1, max_dom
        num_io_tasks = num_io_tasks + nio_tasks_per_group(id)*nio_groups
      END DO
#else
      CALL quilting_disabled( quilting_is_turned_off )
      IF ( quilting_is_turned_off ) THEN
        num_io_tasks = 0
        nio_tasks_per_group  = 0
        nio_groups = 1
      ELSE
        num_io_tasks = nio_tasks_per_group(1)*nio_groups
      END IF
#endif
      CALL nl_set_max_dom(1,max_dom)  ! quilting wants to see this too

      IF ( mytask_local .EQ. 0 ) THEN
        OPEN ( unit=27, file="namelist.input", form="formatted", status="old" )
! get a sneak peek an nproc_x and nproc_y
        nproc_x = -1
        nproc_y = -1
        READ ( 27 , NML = domains, IOSTAT=io_status )
        CLOSE ( 27 )
        OPEN ( unit=27, file="namelist.input", form="formatted", status="old" )
        tasks_per_split = ntasks_local
! we need to sneak-peek the parent_id namelist setting, ,which is in the "domains" section
! of the namelist.  That namelist is registry generated, so the registry-generated information
! is #included above.
        nest_pes_x = 0    ! dimensions of communicator in X and y
        nest_pes_y = 0
        IF ( nproc_x .EQ. -1 .OR. nproc_y .EQ. -1 ) THEN
#if ( HWRF == 1 )
          CALL compute_mesh( ntasks_local, n_x, n_y )
#else
          CALL compute_mesh( ntasks_local-num_io_tasks, n_x, n_y )
#endif
        ELSE
          n_x = nproc_x
          n_y = nproc_y
        END IF
        comm_start = 0   ! make it so everyone will use same communicator if the dm_task_split namelist is not specified or is empty
        nest_pes_x(1:max_dom) = n_x
        nest_pes_y(1:max_dom) = n_y
        READ ( 27 , NML = dm_task_split, IOSTAT=io_status )
        CLOSE ( 27 )
      END IF
      CALL mpi_bcast( io_status, 1 , MPI_INTEGER , 0 , mpi_comm_here, ierr )
      IF ( io_status .NE. 0 ) THEN
! or if  dm_task_split was read but was emptly, do nothing: dm_task_split not specified, everyone uses same communicator (see above)
      END IF
      CALL mpi_bcast( tasks_per_split, 1 , MPI_INTEGER , 0 , mpi_comm_here, ierr )
      CALL mpi_bcast( nproc_x, 1 , MPI_INTEGER , 0 , mpi_comm_here, ierr )
      CALL mpi_bcast( nproc_y, 1 , MPI_INTEGER , 0 , mpi_comm_here, ierr )
      CALL mpi_bcast( comm_start, max_domains , MPI_INTEGER , 0 , mpi_comm_here, ierr )
      CALL mpi_bcast( nest_pes_x, max_domains , MPI_INTEGER , 0 , mpi_comm_here, ierr )
      CALL mpi_bcast( nest_pes_y, max_domains , MPI_INTEGER , 0 , mpi_comm_here, ierr )

      nkids = 1
      which_kid = 0
      DO i = 2, max_dom
        IF ( 1 .le. parent_id(i) .AND. parent_id(i) .LE. max_domains ) THEN
          which_kid(i) = nkids(parent_id(i))
          nkids(parent_id(i)) = nkids(parent_id(i)) + 1
        ELSE
          WRITE(wrf_err_message,*)'invalid parent id for domain ',i
          CALL wrf_error_fatal(TRIM(wrf_err_message))
        END IF
      END DO

      num_compute_tasks = -99
      DO nest_id = 1,max_dom
        IF ( nest_id .EQ. 1 ) THEN
          nest_task_offsets(nest_id) = comm_start(nest_id)
        ELSE
          IF ( comm_start(nest_id) .LT. comm_start(parent_id(nest_id)) ) THEN
            WRITE(wrf_err_message,&
        "('nest domain ',i3,'comm_start (',i3,') lt parent ',i3,' comm_start (',i3,')')") &
                   nest_id,comm_start,parent_id(nest_id),comm_start(parent_id(nest_id))
            CALL wrf_error_fatal(TRIM(wrf_err_message))
          ELSE IF ( comm_start(nest_id) .LT. &
                    comm_start(parent_id(nest_id)) &
                   +nest_pes_x(parent_id(nest_id))*nest_pes_y(parent_id(nest_id))) THEN
            nest_task_offsets(nest_id) = comm_start(nest_id)-comm_start(parent_id(nest_id))
          ELSE
            nest_task_offsets(nest_id) = nest_pes_x(parent_id(nest_id))*nest_pes_y(parent_id(nest_id))
          END IF
        END IF
        IF ((comm_start(nest_id)+nest_pes_x(nest_id)*nest_pes_y(nest_id)) .GT. num_compute_tasks ) THEN
          num_compute_tasks = (comm_start(nest_id)+nest_pes_x(nest_id)*nest_pes_y(nest_id))
        END IF
      END DO

      IF ( .TRUE. ) THEN
!jm Additional code here to set up communicator for this domain and tables
!jm mapping individual domain task IDs to the original local communicator
!jm that is unsplit over nest domains.  from now on what we are calling
!jm local_communicator will be the communicator that is used by the local
!jm nests. The communicator that spans all the nests will be renamed to
!jm intercomm_communicator.  
!jm Design note: exploring the idea of using MPI intercommunicators.  They
!jm only work in pairs so we'd have a lot of intercommunicators to set up 
!jm and keep around. We'd also have to have additional communicator arguments 
!jm to all the nesting routines in and around the RSL nesting parts.
        CALL MPI_Comm_rank ( mpi_comm_here, mytask_local, ierr ) ;
        CALL MPI_Comm_rank ( mpi_comm_here, origmytask, ierr ) ;
        CALL mpi_comm_size ( mpi_comm_here, ntasks_local, ierr ) ;
        ALLOCATE( icolor(ntasks_local) )
        ALLOCATE( icolor2(ntasks_local) )
        ALLOCATE( idomain(ntasks_local) )
        k = 0
! split off the separate local communicators

! construct list of local communicators my task is in
        comms_i_am_in = MPI_UNDEFINED
        DO i = 1, max_dom
          inthisone = .FALSE.
          icolor = 0
          DO j = comm_start(i), comm_start(i)+nest_pes_x(i)*nest_pes_y(i)-1
            IF ( j+1 .GT. ntasks_local ) THEN
              WRITE(wrf_err_message,*)"check comm_start, nest_pes_x, nest_pes_y settings in namelist for comm ",i
              CALL wrf_error_fatal(wrf_err_message)
            END IF
            icolor(j+1) = 1
          END DO
          IF ( icolor(mytask_local+1) .EQ. 1 ) inthisone = .TRUE.
          CALL MPI_Comm_dup(mpi_comm_here,comdup,ierr)
          CALL MPI_Comm_split(comdup,icolor(mytask_local+1),mytask_local,mpi_comm_local,ierr)
          IF ( inthisone ) THEN
            dims(1) = nest_pes_y(i) ! rows
            dims(2) = nest_pes_x(i)  ! columns
            isperiodic(1) = .false.
            isperiodic(2) = .false.
            CALL mpi_cart_create( mpi_comm_local, 2, dims, isperiodic, .false., comms_i_am_in(i), ierr )
          END IF
        END DO

! assign domains to communicators
        local_communicator = MPI_UNDEFINED
#if ( HWRF != 1 )
        CALL wrf_set_dm_quilt_comm( mpi_comm_here )   ! used by module_io_quilt_old.F
#endif
        DO i = 1, max_dom
          local_communicator_store(i) = comms_i_am_in(i)
          domain_active_this_task(i) = ( local_communicator_store(i) .NE. MPI_UNDEFINED )
          IF ( local_communicator_store(i) .NE. MPI_UNDEFINED ) THEN
             CALL MPI_Comm_size( local_communicator_store(i), ntasks_store(i), ierr )
             CALL MPI_Comm_rank( local_communicator_store(i), mytask_store(i), ierr )
             CALL mpi_cart_coords( local_communicator_store(i), mytask_store(i), 2, coords, ierr )
             IF ( ierr .NE. 0 ) CALL wrf_error_fatal('MPI_cart_coords fails ')
             mytask_y_store(i) = coords(1)   ! col task (1)
             mytask_x_store(i) = coords(2)   ! col task (x)
             CALL MPI_Comm_dup( local_communicator_store(i), comdup2, ierr )
             IF ( ierr .NE. 0 ) CALL wrf_error_fatal('MPI_Comm_dup fails ')

             CALL MPI_Comm_split(comdup2,mytask_y_store(i),mytask_store(i),local_communicator_x_store(i),ierr)
             IF ( ierr .NE. 0 ) CALL wrf_error_fatal('MPI_Comm_split fails for y ')

             CALL MPI_Comm_split(comdup2,mytask_x_store(i),mytask_store(i),local_communicator_y_store(i),ierr)
             IF ( ierr .NE. 0 ) CALL wrf_error_fatal('MPI_Comm_split fails for x ')

             CALL MPI_Comm_size( local_communicator_x_store(i), ntasks_x_store(i), ierr )
             CALL MPI_Comm_rank( local_communicator_x_store(i), mytask_x_store(i), ierr )
             CALL MPI_Comm_size( local_communicator_y_store(i), ntasks_y_store(i), ierr )
             CALL MPI_Comm_rank( local_communicator_y_store(i), mytask_y_store(i), ierr )
          END IF
        END DO

        intercomm_active  = .FALSE.
        ! iterate over parent-nest pairs
        ! split off a new communicator from the big one that includes the tasks from the parent and nest communicators
        ! starting with the parent tasks followed by the nest tasks
        ! if a task is in both (ie. the communicators overlap) set the offset at the start of the first nest task
        ! in this way, we will handle cases where the parent and nest are decomposed over the same set of tasks
        ! (in that case, the offset would be the first task of the parent-nest communicator and that communicator)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ntasks_local = num_compute_tasks
        DO nest_id = 2, max_dom
           par_id  = parent_id(nest_id)
           icolor2 = 0
           DO j = 1,ntasks_local !iterate over all the tasks in the "big" communicator
             IF ( local_communicator_store( par_id ) .NE. MPI_UNDEFINED .OR. local_communicator_store( nest_id ) .NE. MPI_UNDEFINED ) icolor2(j)=1
           END DO
        ! set mpi_comm_me_and_mom to be a communicator that has my parents tasks and mine
           icolor2 = 0
           mytask_is_nest = .FALSE.
           mytask_is_par = .FALSE.
           DO j = 1,ntasks_local

             IF ( comm_start(nest_id) .LE. j-1 .AND. j-1 .LT. comm_start(nest_id) + nest_pes_x(nest_id)*nest_pes_y(nest_id) )  THEN
               icolor2(j)=1
               if ( j-1 .EQ. mytask_local ) mytask_is_nest=.TRUE.
             END IF
             IF ( comm_start(par_id ) .LE. j-1 .AND. j-1 .LT. comm_start(par_id ) + nest_pes_x(par_id )*nest_pes_y(par_id ) )  THEN
               icolor2(j)=1
               if ( j-1 .EQ. mytask_local ) mytask_is_par=.TRUE.
             END IF
           END DO

           i = icolor2(mytask_local+1)
           CALL MPI_Comm_dup(mpi_comm_here,comdup,ierr)
           CALL MPI_Comm_split(comdup,i,origmytask,mpi_comm_me_and_mom,ierr)

           IF ( mytask_is_nest  ) THEN
              intercomm_active(nest_id)  = .TRUE.
              mpi_comm_to_mom(nest_id)   =  mpi_comm_me_and_mom
           END IF
           IF ( mytask_is_par ) THEN
              intercomm_active(par_id)              = .TRUE.
              mpi_comm_to_kid(which_kid(nest_id),par_id)  =  mpi_comm_me_and_mom
           END IF
        END DO
        DEALLOCATE( icolor )
        DEALLOCATE( icolor2 )
        DEALLOCATE( idomain )

      ELSE IF ( ( tasks_per_split .LE. ntasks_local .AND. tasks_per_split .LE. 0 ) ) THEN
        domain_active_this_task(1) = .TRUE.
        IF ( mod( ntasks_local, tasks_per_split ) .NE. 0 ) THEN
          CALL wrf_message( 'WARNING: tasks_per_split does not evenly divide ntasks. Some tasks will be wasted.' )
        END IF

        ALLOCATE( icolor(ntasks_local) )
        j = 0
        DO WHILE ( j .LT. ntasks_local / tasks_per_split )
          DO i = 1, tasks_per_split
            icolor( i + j * tasks_per_split ) = j
          END DO
          j = j + 1
        END DO

        CALL MPI_Comm_dup(mpi_comm_here,comdup,ierr)
        CALL MPI_Comm_split(comdup,icolor(mytask_local+1),mytask_local,mpi_comm_local,ierr)
        CALL wrf_set_dm_communicator( mpi_comm_local )
        CALL store_communicators_for_domain(1)
        DEALLOCATE( icolor )
      ELSE
        domain_active_this_task(1) = .TRUE.
        mpi_comm_local = mpi_comm_here
        CALL wrf_set_dm_communicator( mpi_comm_local )
        CALL store_communicators_for_domain(1)
      END IF

      CALL instate_communicators_for_domain(1)

#else
! for serial (non-MPI) builds
      IMPLICIT NONE
# if defined(_OPENMP) && defined(MPI2_THREAD_SUPPORT)
      INTEGER thread_support_provided, thread_support_requested
# endif
      INTEGER i, j, k, x, y, n_x, n_y
      INTEGER iii
      INTEGER dims(3)
! for parallel nesting, 201408, jm
      INTEGER :: id
      INTEGER :: io_status
      INTEGER :: domain_id,par_id,nest_id,kid_id

!!!!! needed to sneak-peek the registry to get parent_id
! define as temporaries
#include "namelist_defines.inc"

! Statements that specify the namelists
#include "namelist_statements.inc"

      max_dom = 1
      OPEN ( unit=27, file="namelist.input", form="formatted", status="old" )
      READ ( UNIT = 27 , NML = domains , IOSTAT=io_status )
      CLOSE(27)

      nkids = 1
      which_kid = 0
      DO i = 2, max_dom
        IF ( 1 .le. parent_id(i) .AND. parent_id(i) .LE. max_domains ) THEN
          which_kid(i) = nkids(parent_id(i))
          nkids(parent_id(i)) = nkids(parent_id(i)) + 1
        ELSE
          WRITE(wrf_err_message,*)'invalid parent id for domain ',i
          CALL wrf_error_fatal(TRIM(wrf_err_message))
        END IF
      END DO

      intercomm_active = .TRUE.
      domain_active_this_task = .TRUE.
      ntasks_stack = 1
      ntasks_y_stack = 1
      ntasks_x_stack = 1
      mytask_stack = 0
      mytask_x_stack = 0
      mytask_y_stack = 0
      ntasks_store = 1
      ntasks_y_store = 1
      ntasks_x_store = 1
      mytask_store = 0
      mytask_x_store = 0
      mytask_y_store = 0
      ntasks = 1
      ntasks_y = 1
      ntasks_x = 1
      mytask = 0
      mytask_x = 0
      mytask_y = 0
      nest_pes_x = 1
      nest_pes_y = 1
      CALL instate_communicators_for_domain(1)
#endif
   END SUBROUTINE split_communicator

   SUBROUTINE init_module_dm
#ifndef STUBMPI
      IMPLICIT NONE
      INTEGER mpi_comm_local, mpi_comm_here, ierr, mytask, nproc
      LOGICAL mpi_inited
      CALL mpi_initialized( mpi_inited, ierr )
      IF ( .NOT. mpi_inited ) THEN
        ! If MPI has not been initialized then initialize it and
        ! make comm_world the communicator
        ! Otherwise, something else (e.g. split_communicator) has already
        ! initialized MPI, so just grab the communicator that
        ! should already be stored and use that.
        CALL mpi_init ( ierr )
        mpi_comm_here = MPI_COMM_WORLD
        CALL wrf_set_dm_communicator ( mpi_comm_here )
      END IF
      CALL wrf_get_dm_communicator( mpi_comm_local )
#endif
   END SUBROUTINE init_module_dm

! stub
   SUBROUTINE wrf_dm_move_nest ( parent, nest, dx, dy )
      USE module_domain, ONLY : domain
      IMPLICIT NONE
      TYPE (domain), INTENT(INOUT) :: parent, nest
      INTEGER, INTENT(IN)          :: dx,dy
      RETURN
   END SUBROUTINE wrf_dm_move_nest

!------------------------------------------------------------------------------
   SUBROUTINE get_full_obs_vector( nsta, nerrf, niobf,          &
                                   mp_local_uobmask,            &
                                   mp_local_vobmask,            &
                                   mp_local_cobmask, errf )
      
!------------------------------------------------------------------------------
!  PURPOSE: Do MPI allgatherv operation across processors to get the
!           errors at each observation point on all processors.
!       
!------------------------------------------------------------------------------
        
    INTEGER, INTENT(IN)   :: nsta                ! Observation index.
    INTEGER, INTENT(IN)   :: nerrf               ! Number of error fields.
    INTEGER, INTENT(IN)   :: niobf               ! Number of observations.
    LOGICAL, INTENT(IN)   :: MP_LOCAL_UOBMASK(NIOBF)
    LOGICAL, INTENT(IN)   :: MP_LOCAL_VOBMASK(NIOBF)
    LOGICAL, INTENT(IN)   :: MP_LOCAL_COBMASK(NIOBF)
    REAL, INTENT(INOUT)   :: errf(nerrf, niobf)

#ifndef STUBMPI
        
! Local declarations
    integer i, n, nlocal_dot, nlocal_crs
    REAL UVT_BUFFER(NIOBF)    ! Buffer for holding U, V, or T
    REAL QRK_BUFFER(NIOBF)    ! Buffer for holding Q or RKO
    REAL SFP_BUFFER(NIOBF)    ! Buffer for holding Surface pressure
    REAL PBL_BUFFER(NIOBF)    ! Buffer for holding (real) KPBL index
    REAL QATOB_BUFFER(NIOBF)  ! Buffer for holding QV at the ob location
    INTEGER N_BUFFER(NIOBF)
    REAL FULL_BUFFER(NIOBF)
    INTEGER IFULL_BUFFER(NIOBF)
    INTEGER IDISPLACEMENT(1024)   ! HARD CODED MAX NUMBER OF PROCESSORS
    INTEGER ICOUNT(1024)          ! HARD CODED MAX NUMBER OF PROCESSORS

    INTEGER :: MPI_COMM_COMP      ! MPI group communicator
    INTEGER :: NPROCS             ! Number of processors
    INTEGER :: IERR               ! Error code from MPI routines

! Get communicator for MPI operations.
    CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)

! Get rank of monitor processor and broadcast to others.
    CALL MPI_COMM_SIZE( MPI_COMM_COMP, NPROCS, IERR )

! DO THE U FIELD
   NLOCAL_DOT = 0
   DO N = 1, NSTA
     IF ( MP_LOCAL_UOBMASK(N) ) THEN      ! USE U-POINT MASK
       NLOCAL_DOT = NLOCAL_DOT + 1
       UVT_BUFFER(NLOCAL_DOT) = ERRF(1,N)        ! U WIND COMPONENT
       SFP_BUFFER(NLOCAL_DOT) = ERRF(7,N)        ! SURFACE PRESSURE
       QRK_BUFFER(NLOCAL_DOT) = ERRF(9,N)        ! RKO
       N_BUFFER(NLOCAL_DOT) = N
     END IF
   END DO
   CALL MPI_ALLGATHER(NLOCAL_DOT,1,MPI_INTEGER, &
                      ICOUNT,1,MPI_INTEGER,     &
                      MPI_COMM_COMP,IERR)
   I = 1

   IDISPLACEMENT(1) = 0
   DO I = 2, NPROCS
     IDISPLACEMENT(I) = IDISPLACEMENT(I-1) + ICOUNT(I-1)
   END DO
   CALL MPI_ALLGATHERV( N_BUFFER, NLOCAL_DOT, MPI_INTEGER,    &
                        IFULL_BUFFER, ICOUNT, IDISPLACEMENT,  &
                        MPI_INTEGER, MPI_COMM_COMP, IERR)
! U
   CALL MPI_ALLGATHERV( UVT_BUFFER, NLOCAL_DOT, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(1,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   END DO
! SURF PRESS AT U-POINTS
   CALL MPI_ALLGATHERV( SFP_BUFFER, NLOCAL_DOT, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(7,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   END DO
! RKO
   CALL MPI_ALLGATHERV( QRK_BUFFER, NLOCAL_DOT, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(9,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   END DO

! DO THE V FIELD
   NLOCAL_DOT = 0
   DO N = 1, NSTA
     IF ( MP_LOCAL_VOBMASK(N) ) THEN         ! USE V-POINT MASK
       NLOCAL_DOT = NLOCAL_DOT + 1
       UVT_BUFFER(NLOCAL_DOT) = ERRF(2,N)    ! V WIND COMPONENT
       SFP_BUFFER(NLOCAL_DOT) = ERRF(8,N)    ! SURFACE PRESSURE
       N_BUFFER(NLOCAL_DOT) = N
     END IF
   END DO
   CALL MPI_ALLGATHER(NLOCAL_DOT,1,MPI_INTEGER, &
                      ICOUNT,1,MPI_INTEGER,     &
                      MPI_COMM_COMP,IERR)
   I = 1

   IDISPLACEMENT(1) = 0
   DO I = 2, NPROCS
     IDISPLACEMENT(I) = IDISPLACEMENT(I-1) + ICOUNT(I-1)
   END DO
   CALL MPI_ALLGATHERV( N_BUFFER, NLOCAL_DOT, MPI_INTEGER,    &
                        IFULL_BUFFER, ICOUNT, IDISPLACEMENT,  &
                        MPI_INTEGER, MPI_COMM_COMP, IERR)
! V
   CALL MPI_ALLGATHERV( UVT_BUFFER, NLOCAL_DOT, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(2,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   END DO
! SURF PRESS AT V-POINTS
   CALL MPI_ALLGATHERV( SFP_BUFFER, NLOCAL_DOT, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(8,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   END DO

! DO THE CROSS FIELDS, T AND Q
   NLOCAL_CRS = 0
   DO N = 1, NSTA
     IF ( MP_LOCAL_COBMASK(N) ) THEN       ! USE MASS-POINT MASK
       NLOCAL_CRS = NLOCAL_CRS + 1
       UVT_BUFFER(NLOCAL_CRS) = ERRF(3,N)     ! TEMPERATURE
       QRK_BUFFER(NLOCAL_CRS) = ERRF(4,N)     ! MOISTURE
       PBL_BUFFER(NLOCAL_CRS) = ERRF(5,N)     ! KPBL
       SFP_BUFFER(NLOCAL_CRS) = ERRF(6,N)     ! SURFACE PRESSURE
       QATOB_BUFFER(NLOCAL_CRS) = ERRF(10,N)     ! Model Mixing ratio itself (NOT ERROR)
       N_BUFFER(NLOCAL_CRS) = N
     END IF
   END DO
   CALL MPI_ALLGATHER(NLOCAL_CRS,1,MPI_INTEGER, &
                      ICOUNT,1,MPI_INTEGER,     &
                      MPI_COMM_COMP,IERR)
   IDISPLACEMENT(1) = 0
   DO I = 2, NPROCS
     IDISPLACEMENT(I) = IDISPLACEMENT(I-1) + ICOUNT(I-1)
   END DO
   CALL MPI_ALLGATHERV( N_BUFFER, NLOCAL_CRS, MPI_INTEGER,    &
                        IFULL_BUFFER, ICOUNT, IDISPLACEMENT,  &
                        MPI_INTEGER, MPI_COMM_COMP, IERR)
! T
   CALL MPI_ALLGATHERV( UVT_BUFFER, NLOCAL_CRS, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)

   DO N = 1, NSTA
     ERRF(3,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   END DO
! Q
   CALL MPI_ALLGATHERV( QRK_BUFFER, NLOCAL_CRS, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(4,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   END DO
! KPBL
   CALL MPI_ALLGATHERV( PBL_BUFFER, NLOCAL_CRS, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(5,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   END DO
! SURF PRESS AT MASS POINTS
   CALL MPI_ALLGATHERV( SFP_BUFFER, NLOCAL_CRS, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(6,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   END DO

! Water vapor mixing ratio at the mass points (NOT THE ERROR)
   CALL MPI_ALLGATHERV( QATOB_BUFFER, NLOCAL_CRS, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(10,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   END DO

#endif
   END SUBROUTINE get_full_obs_vector



   SUBROUTINE wrf_dm_maxtile_real ( val , tile)
      IMPLICIT NONE
      REAL val, val_all( ntasks )
      INTEGER tile
      INTEGER ierr

! <DESCRIPTION>
! Collective operation. Each processor calls passing a local value and its index; on return
! all processors are passed back the maximum of all values passed and its tile number.
!
! </DESCRIPTION>
      INTEGER i, comm
#ifndef STUBMPI

      CALL wrf_get_dm_communicator ( comm )
      CALL mpi_allgather ( val, 1, getrealmpitype(), val_all , 1, getrealmpitype(), comm, ierr )
      val = val_all(1)
      tile = 1
      DO i = 2, ntasks
        IF ( val_all(i) .GT. val ) THEN
           tile = i
           val = val_all(i)
        END IF
      END DO
#endif
   END SUBROUTINE wrf_dm_maxtile_real


   SUBROUTINE wrf_dm_mintile_real ( val , tile)
      IMPLICIT NONE
      REAL val, val_all( ntasks )
      INTEGER tile
      INTEGER ierr

! <DESCRIPTION>
! Collective operation. Each processor calls passing a local value and its index; on return
! all processors are passed back the minimum of all values passed and its tile number.
!
! </DESCRIPTION>
      INTEGER i, comm
#ifndef STUBMPI

      CALL wrf_get_dm_communicator ( comm )
      CALL mpi_allgather ( val, 1, getrealmpitype(), val_all , 1, getrealmpitype(), comm, ierr )
      val = val_all(1)
      tile = 1
      DO i = 2, ntasks
        IF ( val_all(i) .LT. val ) THEN
           tile = i
           val = val_all(i)
        END IF
      END DO
#endif
   END SUBROUTINE wrf_dm_mintile_real


   SUBROUTINE wrf_dm_mintile_double ( val , tile)
      IMPLICIT NONE
      DOUBLE PRECISION val, val_all( ntasks )
      INTEGER tile
      INTEGER ierr

! <DESCRIPTION>
! Collective operation. Each processor calls passing a local value and its index; on return
! all processors are passed back the minimum of all values passed and its tile number.
!
! </DESCRIPTION>
      INTEGER i, comm
#ifndef STUBMPI

      CALL wrf_get_dm_communicator ( comm )
      CALL mpi_allgather ( val, 1, MPI_DOUBLE_PRECISION, val_all , 1, MPI_DOUBLE_PRECISION, comm, ierr )
      val = val_all(1)
      tile = 1
      DO i = 2, ntasks
        IF ( val_all(i) .LT. val ) THEN
           tile = i
           val = val_all(i)
        END IF
      END DO
#endif
   END SUBROUTINE wrf_dm_mintile_double


   SUBROUTINE wrf_dm_tile_val_int ( val , tile)
      IMPLICIT NONE
      INTEGER val, val_all( ntasks )
      INTEGER tile
      INTEGER ierr

! <DESCRIPTION>
! Collective operation. Get value from input tile.
!
! </DESCRIPTION>
      INTEGER i, comm
#ifndef STUBMPI

      CALL wrf_get_dm_communicator ( comm )
      CALL mpi_allgather ( val, 1, MPI_INTEGER, val_all , 1, MPI_INTEGER, comm, ierr )
      val = val_all(tile)
#endif
   END SUBROUTINE wrf_dm_tile_val_int

   SUBROUTINE wrf_get_hostname  ( str )
      CHARACTER*(*) str
      CHARACTER tmp(512)
      INTEGER i , n, cs
      CALL rsl_lite_get_hostname( tmp, 512, n, cs )
      DO i = 1, n
        str(i:i) = tmp(i)
      END DO
      RETURN
   END SUBROUTINE wrf_get_hostname

   SUBROUTINE wrf_get_hostid  ( hostid )
      INTEGER hostid
      CHARACTER tmp(512)
      INTEGER i, sz, n, cs
      CALL rsl_lite_get_hostname( tmp, 512, n, cs )
      hostid = cs
      RETURN
   END SUBROUTINE wrf_get_hostid

END MODULE module_dm


   SUBROUTINE push_communicators_for_domain( id )
      USE module_dm
      INTEGER, INTENT(IN) :: id   ! if specified also does an instate for grid id
      IF ( communicator_stack_cursor .GE. max_domains ) CALL wrf_error_fatal("push_communicators_for_domain would excede stacksize")
      communicator_stack_cursor = communicator_stack_cursor + 1

      id_stack(communicator_stack_cursor) = current_id
      local_communicator_stack( communicator_stack_cursor )    =    local_communicator
      local_communicator_periodic_stack( communicator_stack_cursor )  =    local_communicator_periodic
      local_iocommunicator_stack( communicator_stack_cursor )  =    local_iocommunicator
      local_communicator_x_stack( communicator_stack_cursor )  =    local_communicator_x
      local_communicator_y_stack( communicator_stack_cursor )  =    local_communicator_y
      ntasks_stack( communicator_stack_cursor )        =    ntasks
      ntasks_y_stack( communicator_stack_cursor )      =    ntasks_y
      ntasks_x_stack( communicator_stack_cursor )      =    ntasks_x
      mytask_stack( communicator_stack_cursor )        =    mytask
      mytask_x_stack( communicator_stack_cursor )       =    mytask_x
      mytask_y_stack( communicator_stack_cursor )       =    mytask_y

      CALL instate_communicators_for_domain( id )
   END SUBROUTINE push_communicators_for_domain
   SUBROUTINE pop_communicators_for_domain
      USE module_dm
      IMPLICIT NONE
      IF ( communicator_stack_cursor .LT. 1 ) CALL wrf_error_fatal("pop_communicators_for_domain on empty stack")
      current_id = id_stack(communicator_stack_cursor)
      local_communicator = local_communicator_stack( communicator_stack_cursor )
      local_communicator_periodic = local_communicator_periodic_stack( communicator_stack_cursor )
      local_iocommunicator = local_iocommunicator_stack( communicator_stack_cursor )
      local_communicator_x = local_communicator_x_stack( communicator_stack_cursor )
      local_communicator_y = local_communicator_y_stack( communicator_stack_cursor )
      ntasks = ntasks_stack( communicator_stack_cursor )
      ntasks_y = ntasks_y_stack( communicator_stack_cursor )
      ntasks_x = ntasks_x_stack( communicator_stack_cursor )
      mytask = mytask_stack( communicator_stack_cursor )
      mytask_x = mytask_x_stack( communicator_stack_cursor )
      mytask_y = mytask_y_stack( communicator_stack_cursor )
      communicator_stack_cursor = communicator_stack_cursor - 1
   END SUBROUTINE pop_communicators_for_domain
   SUBROUTINE instate_communicators_for_domain( id )
      USE module_dm
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
      INTEGER ierr
      current_id = id
      local_communicator          = local_communicator_store( id )
      local_communicator_periodic = local_communicator_periodic_store( id )
      local_iocommunicator        = local_iocommunicator_store( id )
      local_communicator_x        = local_communicator_x_store( id )
      local_communicator_y        = local_communicator_y_store( id )
      ntasks         = ntasks_store( id )
      mytask         = mytask_store( id )
      ntasks_x       = ntasks_x_store( id )
      ntasks_y       = ntasks_y_store( id )
      mytask_x       = mytask_x_store( id )
      mytask_y       = mytask_y_store( id )
   END SUBROUTINE instate_communicators_for_domain
   SUBROUTINE store_communicators_for_domain( id )
      USE module_dm
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
      local_communicator_store( id )    =    local_communicator
      local_communicator_periodic_store( id )  =    local_communicator_periodic
      local_iocommunicator_store( id )  =    local_iocommunicator
      local_communicator_x_store( id )  =    local_communicator_x
      local_communicator_y_store( id )  =    local_communicator_y
      ntasks_store( id )        =    ntasks
      ntasks_x_store( id )      =    ntasks_x
      ntasks_y_store( id )      =    ntasks_y
      mytask_store( id )        =    mytask
      mytask_x_store( id )      =    mytask_x
      mytask_y_store( id )      =    mytask_y
   END SUBROUTINE store_communicators_for_domain

!=========================================================================
! wrf_dm_patch_domain has to be outside the module because it is called
! by a routine in module_domain but depends on module domain

SUBROUTINE wrf_dm_patch_domain ( id  , domdesc , parent_id , parent_domdesc , &
                          sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                          sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                          sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                      sp1x , ep1x , sm1x , em1x , &
                                      sp2x , ep2x , sm2x , em2x , &
                                      sp3x , ep3x , sm3x , em3x , &
                                      sp1y , ep1y , sm1y , em1y , &
                                      sp2y , ep2y , sm2y , em2y , &
                                      sp3y , ep3y , sm3y , em3y , &
                          bdx , bdy )
   USE module_domain, ONLY : domain, head_grid, find_grid_by_id
   USE module_dm, ONLY : patch_domain_rsl_lite  !, push_communicators_for_domain, pop_communicators_for_domain
   IMPLICIT NONE

   INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
   INTEGER, INTENT(OUT)  :: sp1 , ep1 , sp2 , ep2 , sp3 , ep3 , &
                            sm1 , em1 , sm2 , em2 , sm3 , em3
   INTEGER               :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &
                            sm1x , em1x , sm2x , em2x , sm3x , em3x
   INTEGER               :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &
                            sm1y , em1y , sm2y , em2y , sm3y , em3y
   INTEGER, INTENT(INOUT):: id  , domdesc , parent_id , parent_domdesc

   TYPE(domain), POINTER :: parent
   TYPE(domain), POINTER :: grid_ptr

   ! this is necessary because we cannot pass parent directly into
   ! wrf_dm_patch_domain because creating the correct interface definitions
   ! would generate a circular USE reference between module_domain and module_dm
   ! see comment this date in module_domain for more information. JM 20020416

   NULLIFY( parent )
   grid_ptr => head_grid
   CALL find_grid_by_id( parent_id , grid_ptr , parent )

   CALL push_communicators_for_domain(id)

   CALL patch_domain_rsl_lite ( id  , parent, parent_id , &
                           sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                           sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                           sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                      sp1x , ep1x , sm1x , em1x , &
                                      sp2x , ep2x , sm2x , em2x , &
                                      sp3x , ep3x , sm3x , em3x , &
                                      sp1y , ep1y , sm1y , em1y , &
                                      sp2y , ep2y , sm2y , em2y , &
                                      sp3y , ep3y , sm3y , em3y , &
                           bdx , bdy )

   CALL pop_communicators_for_domain

   RETURN
END SUBROUTINE wrf_dm_patch_domain

SUBROUTINE wrf_termio_dup( comm )
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: comm
  INTEGER mytask, ntasks
#ifndef STUBMPI
  INTEGER ierr
  INCLUDE 'mpif.h'
  CALL mpi_comm_size(comm, ntasks, ierr )
  CALL mpi_comm_rank(comm, mytask, ierr )
  write(0,*)'starting wrf task ',mytask,' of ',ntasks
  CALL rsl_error_dup1( mytask )
#else
  mytask = 0
  ntasks = 1
#endif
END SUBROUTINE wrf_termio_dup

SUBROUTINE wrf_get_myproc( myproc )
  USE module_dm , ONLY : mytask
  IMPLICIT NONE
  INTEGER myproc
  myproc = mytask
  RETURN
END SUBROUTINE wrf_get_myproc

SUBROUTINE wrf_get_nproc( nproc )
  USE module_dm , ONLY : ntasks
  IMPLICIT NONE
  INTEGER nproc
  nproc = ntasks
  RETURN
END SUBROUTINE wrf_get_nproc

SUBROUTINE wrf_get_nprocx( nprocx )
  USE module_dm , ONLY : ntasks_x
  IMPLICIT NONE
  INTEGER nprocx
  nprocx = ntasks_x
  RETURN
END SUBROUTINE wrf_get_nprocx

SUBROUTINE wrf_get_nprocy( nprocy )
  USE module_dm , ONLY : ntasks_y
  IMPLICIT NONE
  INTEGER nprocy
  nprocy = ntasks_y
  RETURN
END SUBROUTINE wrf_get_nprocy

SUBROUTINE wrf_dm_bcast_bytes ( buf , size )
   USE module_dm , ONLY : local_communicator
   IMPLICIT NONE
#ifndef STUBMPI
   INCLUDE 'mpif.h'
#endif
   INTEGER size
#ifndef NEC
   INTEGER*1 BUF(size)
#else
   CHARACTER*1 BUF(size)
#endif
#ifndef STUBMPI
   CALL BYTE_BCAST ( buf , size, local_communicator )
#endif
   RETURN
END SUBROUTINE wrf_dm_bcast_bytes

SUBROUTINE wrf_dm_bcast_string( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
! <DESCRIPTION>
! Collective operation. Given a string and a size in characters on task zero, broadcast and return that buffer on all tasks.
!
! </DESCRIPTION>
   CHARACTER*(*) buf
#ifndef STUBMPI
   INTEGER ibuf(256),i,n
   CHARACTER*256 tstr
   n = n1
   ! Root task is required to have the correct value of N1, other tasks
   ! might not have the correct value.  
   CALL wrf_dm_bcast_integer( n , 1 )
   IF (n .GT. 256) n = 256
   IF (n .GT. 0 ) then
     DO i = 1, n
       ibuf(I) = ichar(buf(I:I))
     END DO
     CALL wrf_dm_bcast_integer( ibuf, n )
     buf = ''
     DO i = 1, n
       buf(i:i) = char(ibuf(i))
     END DO
   END IF
#endif
   RETURN
END SUBROUTINE wrf_dm_bcast_string

SUBROUTINE wrf_dm_bcast_string_comm( BUF, N1, COMM )
   IMPLICIT NONE
   INTEGER n1
   INTEGER COMM
! <DESCRIPTION>
! Collective operation. Given a string and a size in characters on task zero, broadcast and return that buffer on all tasks.
!
! </DESCRIPTION>
   CHARACTER*(*) buf
#ifndef STUBMPI
   INTEGER ibuf(256),i,n
   CHARACTER*256 tstr
   n = n1
   ! Root task is required to have the correct value of N1, other tasks
   ! might not have the correct value.
   CALL BYTE_BCAST( n, IWORDSIZE, COMM )
   IF (n .GT. 256) n = 256
   IF (n .GT. 0 ) then
     DO i = 1, n
       ibuf(I) = ichar(buf(I:I))
     END DO
     CALL BYTE_BCAST( ibuf, N*IWORDSIZE, COMM )
     buf = ''
     DO i = 1, n
       buf(i:i) = char(ibuf(i))
     END DO
   END IF
#endif
   RETURN
END SUBROUTINE wrf_dm_bcast_string_comm

SUBROUTINE wrf_dm_bcast_integer( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   INTEGER  buf(*)
   CALL wrf_dm_bcast_bytes ( BUF , N1 * IWORDSIZE )
   RETURN
END SUBROUTINE wrf_dm_bcast_integer

SUBROUTINE wrf_dm_bcast_double( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
! this next declaration is REAL, not DOUBLE PRECISION because it will be autopromoted
! to double precision by the compiler when WRF is compiled for 8 byte reals. Only reason
! for having this separate routine is so we pass the correct MPI type to mpi_scatterv
! since we were not indexing the globbuf and Field arrays it does not matter
   REAL  buf(*)
   CALL wrf_dm_bcast_bytes ( BUF , N1 * DWORDSIZE )
   RETURN
END SUBROUTINE wrf_dm_bcast_double

SUBROUTINE wrf_dm_bcast_real( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   REAL  buf(*)
   CALL wrf_dm_bcast_bytes ( BUF , N1 * RWORDSIZE )
   RETURN
END SUBROUTINE wrf_dm_bcast_real

SUBROUTINE wrf_dm_bcast_logical( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   LOGICAL  buf(*)
   CALL wrf_dm_bcast_bytes ( BUF , N1 * LWORDSIZE )
   RETURN
END SUBROUTINE wrf_dm_bcast_logical

SUBROUTINE write_68( grid, v , s , &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   its, ite, jts, jte, kts, kte )
  USE module_domain, ONLY : domain
  IMPLICIT NONE
  TYPE(domain) , INTENT (INOUT) :: grid
  CHARACTER *(*) s
  INTEGER ids, ide, jds, jde, kds, kde, &
          ims, ime, jms, jme, kms, kme, &
          its, ite, jts, jte, kts, kte
  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ) :: v

  INTEGER i,j,k,ierr

  logical, external :: wrf_dm_on_monitor
  real globbuf( ids:ide, kds:kde, jds:jde )
  character*3 ord, stag

  if ( kds == kde ) then
    ord = 'xy'
    stag = 'xy'
  CALL wrf_patch_to_global_real ( v, globbuf, grid%domdesc, stag, ord, &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )
  else

    stag = 'xyz'
    ord = 'xzy'
  CALL wrf_patch_to_global_real ( v, globbuf, grid%domdesc, stag, ord, &
                     ids, ide, kds, kde, jds, jde, &
                     ims, ime, kms, kme, jms, jme, &
                     its, ite, kts, kte, jts, jte )
  endif


  if ( wrf_dm_on_monitor() ) THEN
    WRITE(68,*) ide-ids+1, jde-jds+1 , s
    DO j = jds, jde
    DO i = ids, ide
       WRITE(68,*) globbuf(i,1,j)
    END DO
    END DO
  endif

  RETURN
END

   SUBROUTINE wrf_abort

#if ( DA_CORE != 1 )
      USE module_cpl, ONLY : coupler_on, cpl_abort
#endif

      IMPLICIT NONE
#ifndef STUBMPI
      INCLUDE 'mpif.h'
      INTEGER ierr
#if ( DA_CORE != 1 )
      IF ( coupler_on ) THEN
         CALL cpl_abort( 'wrf_abort', 'look for abort message in rsl* files' )
      ELSE
#endif
         CALL mpi_abort(MPI_COMM_WORLD,1,ierr)
#if ( DA_CORE != 1 )
      END IF
#endif
#else
      STOP
#endif
   END SUBROUTINE wrf_abort

   SUBROUTINE wrf_dm_shutdown
      IMPLICIT NONE
#ifndef STUBMPI
      INTEGER ierr
      CALL MPI_FINALIZE( ierr )
#endif
      RETURN
   END SUBROUTINE wrf_dm_shutdown

   LOGICAL FUNCTION wrf_dm_on_monitor()
      IMPLICIT NONE
#ifndef STUBMPI
      INCLUDE 'mpif.h'
      INTEGER tsk, ierr, mpi_comm_local
      CALL wrf_get_dm_communicator( mpi_comm_local )
      IF ( mpi_comm_local .NE. MPI_UNDEFINED ) THEN
        CALL mpi_comm_rank ( mpi_comm_local, tsk , ierr )
        wrf_dm_on_monitor = tsk .EQ. 0
      ELSE
        wrf_dm_on_monitor = .FALSE.
      END IF
#else
      wrf_dm_on_monitor = .TRUE.
#endif
      RETURN
   END FUNCTION wrf_dm_on_monitor

   SUBROUTINE rsl_comm_iter_init(shw,ps,pe)
      INTEGER shw, ps, pe
      INTEGER iter, plus_send_start, plus_recv_start, &
                    minus_send_start, minus_recv_start
      COMMON /rcii/ iter, plus_send_start, plus_recv_start, &
                          minus_send_start, minus_recv_start
      iter = 0
      minus_send_start = ps
      minus_recv_start = ps-1
      plus_send_start = pe
      plus_recv_start = pe+1
   END SUBROUTINE rsl_comm_iter_init

   LOGICAL FUNCTION rsl_comm_iter ( id , is_intermediate,                     &
                                    shw ,  xy , ds, de_in, ps, pe, nds,nde, &
                                    sendbeg_m, sendw_m, sendbeg_p, sendw_p,   &
                                    recvbeg_m, recvw_m, recvbeg_p, recvw_p    )
      USE module_dm, ONLY : ntasks_x, ntasks_y, mytask_x, mytask_y, minx, miny, &
                            nest_pes_x, nest_pes_y
      IMPLICIT NONE
      INTEGER, INTENT(IN)  :: id,shw,xy,ds,de_in,ps,pe,nds,nde
      LOGICAL, INTENT(IN)  :: is_intermediate  ! treated differently, coarse but with same decomp as nest
      INTEGER, INTENT(OUT) :: sendbeg_m, sendw_m, sendbeg_p, sendw_p
      INTEGER, INTENT(OUT) :: recvbeg_m, recvw_m, recvbeg_p, recvw_p
      INTEGER k, kn, ni, nj, de, Px, Py, nt, ntx, nty, me, lb, ub, ierr
      INTEGER dum
      LOGICAL went
      INTEGER iter, plus_send_start, plus_recv_start, &
                    minus_send_start, minus_recv_start
      INTEGER parent_grid_ratio, parent_start
      COMMON /rcii/ iter, plus_send_start, plus_recv_start, &
                          minus_send_start, minus_recv_start

#if (NMM_CORE == 1 )
! In case of NMM CORE, the domain only ever runs from ids..ide-1 and jds..jde-1 so
! adjust decomposition to reflect.  20081206 JM
      de = de_in - 1
#else
      de = de_in
#endif
      ntx = nest_pes_x(id)
      nty = nest_pes_y(id)
      IF ( xy .EQ. 1 ) THEN  ! X/I axis
        nt = ntasks_x
        me = mytask_x
        dum = 2 * nty  ! dummy dimension length for tfp to decompose without getting div 0
        IF ( is_intermediate ) THEN
           CALL nl_get_i_parent_start(id,parent_start)
           CALL nl_get_parent_grid_ratio(id,parent_grid_ratio)
        END IF
      ELSE
        nt = ntasks_y
        me = mytask_y
        dum = 2 * ntx  ! dummy dimension length for tfp to decompose without getting div 0
        IF ( is_intermediate ) THEN
           CALL nl_get_j_parent_start(id,parent_start)
           CALL nl_get_parent_grid_ratio(id,parent_grid_ratio)
        END IF
      END IF
      iter = iter + 1

#if (DA_CORE == 0)
      went = .FALSE.
      ! send to minus
      sendw_m = 0
      sendbeg_m = 1
      IF ( me .GT. 0 ) THEN
        lb = minus_send_start
        sendbeg_m = lb-ps+1
        DO k = lb,ps+shw-1
          went = .TRUE.
          IF ( xy .eq. 1 ) THEN
            IF ( is_intermediate ) THEN
              kn =  ( k - parent_start ) * parent_grid_ratio + 1 + 1 ;
              CALL task_for_point (kn,1,nds,nde,1,dum,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (h)')
            ELSE
              CALL task_for_point (k,1,ds,de,1,dum,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (i)')
            END IF
            IF ( Px .NE. me+(iter-1) ) THEN
              exit
            END IF
          ELSE
            IF ( is_intermediate ) THEN
              kn =  ( k - parent_start ) * parent_grid_ratio + 1 + 1 ;
              CALL task_for_point (1,kn,1,dum,nds,nde,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (h)')
            ELSE
              CALL task_for_point (1,k,1,dum,ds,de,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (i)')
            END IF
            IF ( Py .NE. me+(iter-1) ) THEN
              exit
            END IF
          END IF
          minus_send_start = minus_send_start+1
          sendw_m = sendw_m + 1
        END DO
      END IF
      ! recv from minus
      recvw_m = 0
      recvbeg_m = 1
      IF ( me .GT. 0 ) THEN
        ub = minus_recv_start
        recvbeg_m = ps - ub
        DO k = minus_recv_start,ps-shw,-1
          went = .TRUE.
          IF ( xy .eq. 1 ) THEN
          IF ( is_intermediate ) THEN
            kn =  ( k - parent_start ) * parent_grid_ratio + 1 + 1 ;
              CALL task_for_point (kn,1,nds,nde,1,dum,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (j)')
          ELSE
              CALL task_for_point (k,1,ds,de,1,dum,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (k)')
          END IF
          IF ( Px .NE. me-iter ) THEN
            exit
          END IF
          ELSE
            IF ( is_intermediate ) THEN
              kn =  ( k - parent_start ) * parent_grid_ratio + 1 + 1 ;
              CALL task_for_point (1,kn,1,dum,nds,nde,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (j)')
            ELSE
              CALL task_for_point (1,k,1,dum,ds,de,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (k)')
            END IF
            IF ( Py .NE. me-iter ) THEN
              exit
            END IF
          END IF
          minus_recv_start = minus_recv_start-1
          recvw_m = recvw_m + 1
        END DO
      END IF

      ! send to plus
      sendw_p = 0
      sendbeg_p = 1
      IF ( ( xy .eq. 1 .and. me .LT. ntx-1 ) .OR. ( xy .eq. 0 .and. me .LT. nty-1 ) ) THEN
        ub = plus_send_start
        sendbeg_p = pe - ub + 1
        DO k = ub,pe-shw+1,-1
          went = .TRUE.
          IF ( xy .eq. 1 ) THEN
          IF ( is_intermediate ) THEN
            kn =  ( k - parent_start ) * parent_grid_ratio + 1 + 1 ;
              CALL task_for_point (kn,1,nds,nde,1,dum,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (l)')
          ELSE
              CALL task_for_point (k,1,ds,de,1,dum,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (m)')
          END IF
          IF ( Px .NE. me-(iter-1) ) THEN
            exit
          END IF
          ELSE
            IF ( is_intermediate ) THEN
              kn =  ( k - parent_start ) * parent_grid_ratio + 1 + 1 ;
              CALL task_for_point (1,kn,1,dum,nds,nde,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (l)')
            ELSE
              CALL task_for_point (1,k,1,dum,ds,de,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (m)')
            END IF
            IF ( Py .NE. me-(iter-1) ) THEN
              exit
            END IF
          END IF
          plus_send_start = plus_send_start - 1
          sendw_p = sendw_p + 1
        END DO
      END IF
      ! recv from plus
      recvw_p = 0
      recvbeg_p = 1
      IF ( ( xy .eq. 1 .and. me .LT. ntx-1 ) .OR. ( xy .eq. 0 .and. me .LT. nty-1 ) ) THEN
        lb = plus_recv_start
        recvbeg_p = lb - pe
        DO k = lb,pe+shw
          went = .TRUE.
          IF ( xy .eq. 1 ) THEN
          IF ( is_intermediate ) THEN
            kn =  ( k - parent_start ) * parent_grid_ratio + 1 + 1 ;
              CALL task_for_point (kn,1,nds,nde,1,dum,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (n)')
          ELSE

              CALL task_for_point (k,1,ds,de,1,dum,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately

              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (o)')
          END IF
          IF ( Px .NE. me+iter ) THEN
            exit
          END IF
          ELSE
            IF ( is_intermediate ) THEN
              kn =  ( k - parent_start ) * parent_grid_ratio + 1 + 1 ;
              CALL task_for_point (1,kn,1,dum,nds,nde,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (n)')
            ELSE
              CALL task_for_point (1,k,1,dum,ds,de,ntx,nty,Px,Py,minx,miny,ierr) ! modified to treat x and y separately
              IF ( ierr .NE. 0 ) CALL wrf_error_fatal('error code returned by task_for_point in module_dm.F (o)')
            END IF
            IF ( Py .NE. me+iter ) THEN
              exit
            END IF
          END IF
          plus_recv_start = plus_recv_start + 1
          recvw_p = recvw_p + 1
        END DO
      END IF
#else
      if ( iter .eq. 1 ) then
        went = .true.
      else
        went = .false.
      endif
      sendw_m = 0 ; sendw_p = 0 ; recvw_m = 0 ; recvw_p = 0
      sendbeg_m = 1 ; if ( me .GT. 0 ) sendw_m = shw ;
      sendbeg_p = 1 ; if ( me .LT. nt-1 ) sendw_p = shw
      recvbeg_m = 1 ; if ( me .GT. 0 ) recvw_m = shw ;
      recvbeg_p = 1 ; if ( me .LT. nt-1 ) recvw_p = shw ;

      ! write(0,*)'shw  ', shw , ' xy ',xy
      ! write(0,*)' ds, de, ps, pe, nds,nde ',ds, de, ps, pe, nds,nde
      ! write(0,*)'sendbeg_m, sendw_m, sendbeg_p, sendw_p, recvbeg_m, recvw_m, recvbeg_p, recvw_p '
      ! write(0,*)sendbeg_m, sendw_m, sendbeg_p, sendw_p, recvbeg_m, recvw_m, recvbeg_p, recvw_p
#endif
      !if ( went ) then
      !  write(0,*)'shw  ', shw , ' xy ',xy,' plus_send_start ',plus_send_start,' minus_send_start ', minus_send_start
      !  write(0,*)' ds, de, ps, pe, nds,nde ',ds, de, ps, pe, nds,nde
      !  write(0,*)'sendbeg_m, sendw_m, sendbeg_p, sendw_p, recvbeg_m, recvw_m, recvbeg_p, recvw_p '
      !  write(0,*)sendbeg_m, sendw_m, sendbeg_p, sendw_p, recvbeg_m, recvw_m, recvbeg_p, recvw_p
      !endif
      rsl_comm_iter = went
   END FUNCTION rsl_comm_iter

   INTEGER FUNCTION wrf_dm_monitor_rank()
      IMPLICIT NONE
      wrf_dm_monitor_rank = 0
      RETURN
   END FUNCTION wrf_dm_monitor_rank

! return the global communicator if id <= 0
   SUBROUTINE wrf_get_dm_communicator_for_id ( id, communicator )
      USE module_dm , ONLY : local_communicator_store, mpi_comm_allcompute
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: id
      INTEGER , INTENT(OUT) :: communicator
      IF ( id .le. 0 ) THEN
        communicator = mpi_comm_allcompute
      ELSE
        communicator = local_communicator_store(id)
      END IF
      RETURN
   END SUBROUTINE wrf_get_dm_communicator_for_id

   SUBROUTINE wrf_get_dm_communicator ( communicator )
      USE module_dm , ONLY : local_communicator
      IMPLICIT NONE
      INTEGER , INTENT(OUT) :: communicator
      communicator = local_communicator
      RETURN
   END SUBROUTINE wrf_get_dm_communicator

   SUBROUTINE wrf_get_dm_communicator_x ( communicator )
      USE module_dm , ONLY : local_communicator_x
      IMPLICIT NONE
      INTEGER , INTENT(OUT) :: communicator
      communicator = local_communicator_x
      RETURN
   END SUBROUTINE wrf_get_dm_communicator_x

   SUBROUTINE wrf_get_dm_communicator_y ( communicator )
      USE module_dm , ONLY : local_communicator_y
      IMPLICIT NONE
      INTEGER , INTENT(OUT) :: communicator
      communicator = local_communicator_y
      RETURN
   END SUBROUTINE wrf_get_dm_communicator_y

   SUBROUTINE wrf_get_dm_iocommunicator ( iocommunicator )
      USE module_dm , ONLY : local_iocommunicator
      IMPLICIT NONE
      INTEGER , INTENT(OUT) :: iocommunicator
      iocommunicator = local_iocommunicator
      RETURN
   END SUBROUTINE wrf_get_dm_iocommunicator

   SUBROUTINE wrf_set_dm_communicator ( communicator )
      USE module_dm , ONLY : local_communicator
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: communicator
      local_communicator = communicator
      RETURN
   END SUBROUTINE wrf_set_dm_communicator

   SUBROUTINE wrf_set_dm_iocommunicator ( iocommunicator )
      USE module_dm , ONLY : local_iocommunicator
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: iocommunicator
      local_iocommunicator = iocommunicator
      RETURN
   END SUBROUTINE wrf_set_dm_iocommunicator

   SUBROUTINE wrf_get_dm_ntasks_x ( retval )
      USE module_dm , ONLY : ntasks_x
      IMPLICIT NONE
      INTEGER , INTENT(OUT) :: retval
      retval = ntasks_x
      RETURN
   END SUBROUTINE wrf_get_dm_ntasks_x

   SUBROUTINE wrf_get_dm_ntasks_y ( retval )
      USE module_dm , ONLY : ntasks_y
      IMPLICIT NONE
      INTEGER , INTENT(OUT) :: retval
      retval = ntasks_y
      RETURN
   END SUBROUTINE wrf_get_dm_ntasks_y

! added 20151212
   SUBROUTINE wrf_set_dm_quilt_comm ( communicator )
      USE module_dm , ONLY : local_quilt_comm
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: communicator
      local_quilt_comm = communicator
      RETURN
   END SUBROUTINE wrf_set_dm_quilt_comm

   SUBROUTINE wrf_get_dm_quilt_comm ( communicator )
      USE module_dm , ONLY : local_quilt_comm
      IMPLICIT NONE
      INTEGER , INTENT(OUT) :: communicator
      communicator = local_quilt_comm
      RETURN
   END SUBROUTINE wrf_get_dm_quilt_comm


!!!!!!!!!!!!!!!!!!!!!!! PATCH TO GLOBAL !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE wrf_patch_to_global_real (buf,globbuf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       REAL globbuf(*)
       REAL buf(*)

       CALL wrf_patch_to_global_generic (buf,globbuf,domdesc,stagger,ordering,RWORDSIZE,&
                                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                         MS1,ME1,MS2,ME2,MS3,ME3,&
                                         PS1,PE1,PS2,PE2,PS3,PE3 )

       RETURN
   END SUBROUTINE wrf_patch_to_global_real

   SUBROUTINE wrf_patch_to_global_double (buf,globbuf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
! this next declaration is REAL, not DOUBLE PRECISION because it will be autopromoted
! to double precision by the compiler when WRF is compiled for 8 byte reals. Only reason
! for having this separate routine is so we pass the correct MPI type to mpi_scatterv
! since we were not indexing the globbuf and Field arrays it does not matter
       REAL globbuf(*)
       REAL buf(*)

       CALL wrf_patch_to_global_generic (buf,globbuf,domdesc,stagger,ordering,DWORDSIZE,&
                                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                         MS1,ME1,MS2,ME2,MS3,ME3,&
                                         PS1,PE1,PS2,PE2,PS3,PE3 )

       RETURN
   END SUBROUTINE wrf_patch_to_global_double


   SUBROUTINE wrf_patch_to_global_integer (buf,globbuf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       INTEGER globbuf(*)
       INTEGER buf(*)

       CALL wrf_patch_to_global_generic (buf,globbuf,domdesc,stagger,ordering,IWORDSIZE,&
                                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                         MS1,ME1,MS2,ME2,MS3,ME3,&
                                         PS1,PE1,PS2,PE2,PS3,PE3 )

       RETURN
   END SUBROUTINE wrf_patch_to_global_integer


   SUBROUTINE wrf_patch_to_global_logical (buf,globbuf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       LOGICAL globbuf(*)
       LOGICAL buf(*)

       CALL wrf_patch_to_global_generic (buf,globbuf,domdesc,stagger,ordering,LWORDSIZE,&
                                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                         MS1,ME1,MS2,ME2,MS3,ME3,&
                                         PS1,PE1,PS2,PE2,PS3,PE3 )

       RETURN
   END SUBROUTINE wrf_patch_to_global_logical

#ifdef DEREF_KLUDGE
# define FRSTELEM (1)
#else
# define FRSTELEM
#endif

   SUBROUTINE wrf_patch_to_global_generic (buf,globbuf,domdesc,stagger,ordering,typesize,&
                                       DS1a,DE1a,DS2a,DE2a,DS3a,DE3a,&
                                       MS1a,ME1a,MS2a,ME2a,MS3a,ME3a,&
                                       PS1a,PE1a,PS2a,PE2a,PS3a,PE3a )
       USE module_driver_constants
       USE module_timing
       USE module_wrf_error, ONLY : wrf_at_debug_level
       USE module_dm, ONLY : local_communicator, ntasks

       IMPLICIT NONE
       INTEGER                         DS1a,DE1a,DS2a,DE2a,DS3a,DE3a,&
                                       MS1a,ME1a,MS2a,ME2a,MS3a,ME3a,&
                                       PS1a,PE1a,PS2a,PE2a,PS3a,PE3A
       CHARACTER *(*) stagger,ordering
       INTEGER domdesc,typesize,ierr
       REAL globbuf(*)
       REAL buf(*)
#ifndef STUBMPI
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       INTEGER                         ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe
       LOGICAL, EXTERNAL :: wrf_dm_on_monitor, has_char

       INTEGER i, j, k,  ndim
       INTEGER  Patch(3,2), Gpatch(3,2,ntasks)
    ! allocated further down, after the D indices are potentially recalculated for staggering
       REAL, ALLOCATABLE :: tmpbuf( : )
       REAL locbuf( (PE1a-PS1a+1)*(PE2a-PS2a+1)*(PE3a-PS3a+1)/RWORDSIZE*typesize+32 )

       DS1 = DS1a ; DE1 = DE1a ; DS2=DS2a ; DE2 = DE2a ; DS3 = DS3a ; DE3 = DE3a
       MS1 = MS1a ; ME1 = ME1a ; MS2=MS2a ; ME2 = ME2a ; MS3 = MS3a ; ME3 = ME3a
       PS1 = PS1a ; PE1 = PE1a ; PS2=PS2a ; PE2 = PE2a ; PS3 = PS3a ; PE3 = PE3a

       SELECT CASE ( TRIM(ordering) )
         CASE ( 'xy', 'yx' )
           ndim = 2
         CASE DEFAULT
           ndim = 3   ! where appropriate
       END SELECT

       SELECT CASE ( TRIM(ordering) )
         CASE ( 'xyz','xy' )
            ! the non-staggered variables come in at one-less than
            ! domain dimensions, but code wants full domain spec, so
            ! adjust if not staggered
           IF ( .NOT. has_char( stagger, 'x' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE2 = DE2+1
           IF ( ndim .EQ. 3 .AND. .NOT. has_char( stagger, 'z' ) ) DE3 = DE3+1
         CASE ( 'yxz','yx' )
           IF ( .NOT. has_char( stagger, 'x' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE1 = DE1+1
           IF ( ndim .EQ. 3 .AND. .NOT. has_char( stagger, 'z' ) ) DE3 = DE3+1
         CASE ( 'zxy' )
           IF ( .NOT. has_char( stagger, 'x' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE3 = DE3+1
           IF ( ndim .EQ. 3 .AND. .NOT. has_char( stagger, 'z' ) ) DE1 = DE1+1
         CASE ( 'xzy' )
           IF ( .NOT. has_char( stagger, 'x' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE3 = DE3+1
           IF ( ndim .EQ. 3 .AND. .NOT. has_char( stagger, 'z' ) ) DE2 = DE2+1
         CASE DEFAULT
       END SELECT

     ! moved to here to be after the potential recalculations of D dims
       IF ( wrf_dm_on_monitor() ) THEN
         ALLOCATE ( tmpbuf ( (DE1-DS1+1)*(DE2-DS2+1)*(DE3-DS3+1)/RWORDSIZE*typesize+32 ), STAT=ierr )
       ELSE
         ALLOCATE ( tmpbuf ( 1 ), STAT=ierr )
       END IF
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating tmpbuf in wrf_patch_to_global_generic')

       Patch(1,1) = ps1 ; Patch(1,2) = pe1    ! use patch dims
       Patch(2,1) = ps2 ; Patch(2,2) = pe2
       Patch(3,1) = ps3 ; Patch(3,2) = pe3

       IF      ( typesize .EQ. RWORDSIZE ) THEN
         CALL just_patch_r ( buf , locbuf , size(locbuf)*RWORDSIZE/typesize, &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )
       ELSE IF ( typesize .EQ. IWORDSIZE ) THEN
         CALL just_patch_i ( buf , locbuf , size(locbuf)*RWORDSIZE/typesize, &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )
       ELSE IF ( typesize .EQ. DWORDSIZE ) THEN
         CALL just_patch_d ( buf , locbuf , size(locbuf)*RWORDSIZE/typesize, &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )
       ELSE IF ( typesize .EQ. LWORDSIZE ) THEN
         CALL just_patch_l ( buf , locbuf , size(locbuf)*RWORDSIZE/typesize, &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )
       END IF

! defined in external/io_quilt
       CALL collect_on_comm0 (  local_communicator , IWORDSIZE ,  &
                                Patch , 6 ,                       &
                                GPatch , 6*ntasks                 )

       CALL collect_on_comm0 (  local_communicator , typesize ,  &
                                locbuf , (pe1-ps1+1)*(pe2-ps2+1)*(pe3-ps3+1),   &
                                tmpbuf FRSTELEM , (de1-ds1+1)*(de2-ds2+1)*(de3-ds3+1) )

       ndim = len(TRIM(ordering))

       IF ( wrf_at_debug_level(500) ) THEN
         CALL start_timing
       END IF

       IF ( ndim .GE. 2 .AND. wrf_dm_on_monitor() ) THEN

         IF      ( typesize .EQ. RWORDSIZE ) THEN
           CALL patch_2_outbuf_r ( tmpbuf FRSTELEM , globbuf ,             &
                                   DS1, DE1, DS2, DE2, DS3, DE3 , &
                                   GPATCH                         )
         ELSE IF ( typesize .EQ. IWORDSIZE ) THEN
           CALL patch_2_outbuf_i ( tmpbuf FRSTELEM , globbuf ,             &
                                   DS1, DE1, DS2, DE2, DS3, DE3 , &
                                   GPATCH                         )
         ELSE IF ( typesize .EQ. DWORDSIZE ) THEN
           CALL patch_2_outbuf_d ( tmpbuf FRSTELEM , globbuf ,             &
                                   DS1, DE1, DS2, DE2, DS3, DE3 , &
                                   GPATCH                         )
         ELSE IF ( typesize .EQ. LWORDSIZE ) THEN
           CALL patch_2_outbuf_l ( tmpbuf FRSTELEM , globbuf ,             &
                                   DS1, DE1, DS2, DE2, DS3, DE3 , &
                                   GPATCH                         )
         END IF

       END IF

       IF ( wrf_at_debug_level(500) ) THEN
         CALL end_timing('wrf_patch_to_global_generic')
       END IF
       DEALLOCATE( tmpbuf )
#endif
       RETURN
    END SUBROUTINE wrf_patch_to_global_generic

  SUBROUTINE just_patch_i ( inbuf , outbuf, noutbuf,     &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    IMPLICIT NONE
    INTEGER                         , INTENT(IN)  :: noutbuf
    INTEGER    , DIMENSION(noutbuf) , INTENT(OUT) :: outbuf
    INTEGER   MS1,ME1,MS2,ME2,MS3,ME3
    INTEGER   PS1,PE1,PS2,PE2,PS3,PE3
    INTEGER    , DIMENSION( MS1:ME1,MS2:ME2,MS3:ME3 ) , INTENT(IN) :: inbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
      DO k = PS3, PE3
        DO j = PS2, PE2
          DO i = PS1, PE1
            outbuf( icurs )  = inbuf( i, j, k )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    RETURN
  END SUBROUTINE just_patch_i

  SUBROUTINE just_patch_r ( inbuf , outbuf, noutbuf,     &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    IMPLICIT NONE
    INTEGER                      , INTENT(IN)  :: noutbuf
    REAL    , DIMENSION(noutbuf) , INTENT(OUT) :: outbuf
    INTEGER   MS1,ME1,MS2,ME2,MS3,ME3
    INTEGER   PS1,PE1,PS2,PE2,PS3,PE3
    REAL    , DIMENSION( MS1:ME1,MS2:ME2,MS3:ME3 ) , INTENT(in) :: inbuf
! Local
    INTEGER               :: i,j,k   ,  icurs
    icurs = 1
      DO k = PS3, PE3
        DO j = PS2, PE2
          DO i = PS1, PE1
            outbuf( icurs )  = inbuf( i, j, k )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    RETURN
  END SUBROUTINE just_patch_r

  SUBROUTINE just_patch_d ( inbuf , outbuf, noutbuf,     &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    IMPLICIT NONE
    INTEGER                                  , INTENT(IN)  :: noutbuf
    DOUBLE PRECISION    , DIMENSION(noutbuf) , INTENT(OUT) :: outbuf
    INTEGER   MS1,ME1,MS2,ME2,MS3,ME3
    INTEGER   PS1,PE1,PS2,PE2,PS3,PE3
    DOUBLE PRECISION    , DIMENSION( MS1:ME1,MS2:ME2,MS3:ME3 ) , INTENT(in) :: inbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
      DO k = PS3, PE3
        DO j = PS2, PE2
          DO i = PS1, PE1
            outbuf( icurs )  = inbuf( i, j, k )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    RETURN
  END SUBROUTINE just_patch_d

  SUBROUTINE just_patch_l ( inbuf , outbuf, noutbuf,     &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    IMPLICIT NONE
    INTEGER                         , INTENT(IN)  :: noutbuf
    LOGICAL    , DIMENSION(noutbuf) , INTENT(OUT) :: outbuf
    INTEGER   MS1,ME1,MS2,ME2,MS3,ME3
    INTEGER   PS1,PE1,PS2,PE2,PS3,PE3
    LOGICAL    , DIMENSION( MS1:ME1,MS2:ME2,MS3:ME3 ) , INTENT(in) :: inbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
      DO k = PS3, PE3
        DO j = PS2, PE2
          DO i = PS1, PE1
            outbuf( icurs )  = inbuf( i, j, k )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    RETURN
  END SUBROUTINE just_patch_l


  SUBROUTINE patch_2_outbuf_r( inbuf, outbuf,            &
                               DS1,DE1,DS2,DE2,DS3,DE3,  &
                               GPATCH )
    USE module_dm, ONLY : ntasks
    IMPLICIT NONE
    REAL    , DIMENSION(*) , INTENT(IN) :: inbuf
    INTEGER   DS1,DE1,DS2,DE2,DS3,DE3,GPATCH(3,2,ntasks)
    REAL    , DIMENSION( DS1:DE1,DS2:DE2,DS3:DE3 ) , INTENT(out) :: outbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
    DO n = 1, ntasks
      DO k = GPATCH( 3,1,n ), GPATCH( 3,2,n )
        DO j = GPATCH( 2,1,n ), GPATCH( 2,2,n )
          DO i = GPATCH( 1,1,n ), GPATCH( 1,2,n )
            outbuf( i, j, k ) = inbuf( icurs )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    END DO

    RETURN
  END SUBROUTINE patch_2_outbuf_r

  SUBROUTINE patch_2_outbuf_i( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm, ONLY : ntasks
    IMPLICIT NONE
    INTEGER    , DIMENSION(*) , INTENT(IN) :: inbuf
    INTEGER   DS1,DE1,DS2,DE2,DS3,DE3,GPATCH(3,2,ntasks)
    INTEGER    , DIMENSION( DS1:DE1,DS2:DE2,DS3:DE3 ) , INTENT(out) :: outbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
    DO n = 1, ntasks
      DO k = GPATCH( 3,1,n ), GPATCH( 3,2,n )
        DO j = GPATCH( 2,1,n ), GPATCH( 2,2,n )
          DO i = GPATCH( 1,1,n ), GPATCH( 1,2,n )
            outbuf( i, j, k ) = inbuf( icurs )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    END DO
    RETURN
  END SUBROUTINE patch_2_outbuf_i

  SUBROUTINE patch_2_outbuf_d( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm, ONLY : ntasks
    IMPLICIT NONE
    DOUBLE PRECISION    , DIMENSION(*) , INTENT(IN) :: inbuf
    INTEGER   DS1,DE1,DS2,DE2,DS3,DE3,GPATCH(3,2,ntasks)
    DOUBLE PRECISION    , DIMENSION( DS1:DE1,DS2:DE2,DS3:DE3 ) , INTENT(out) :: outbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
    DO n = 1, ntasks
      DO k = GPATCH( 3,1,n ), GPATCH( 3,2,n )
        DO j = GPATCH( 2,1,n ), GPATCH( 2,2,n )
          DO i = GPATCH( 1,1,n ), GPATCH( 1,2,n )
            outbuf( i, j, k ) = inbuf( icurs )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    END DO
    RETURN
  END SUBROUTINE patch_2_outbuf_d

  SUBROUTINE patch_2_outbuf_l( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm, ONLY : ntasks
    IMPLICIT NONE
    LOGICAL    , DIMENSION(*) , INTENT(IN) :: inbuf
    INTEGER   DS1,DE1,DS2,DE2,DS3,DE3,GPATCH(3,2,ntasks)
    LOGICAL    , DIMENSION( DS1:DE1,DS2:DE2,DS3:DE3 ) , INTENT(out) :: outbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
    DO n = 1, ntasks
      DO k = GPATCH( 3,1,n ), GPATCH( 3,2,n )
        DO j = GPATCH( 2,1,n ), GPATCH( 2,2,n )
          DO i = GPATCH( 1,1,n ), GPATCH( 1,2,n )
            outbuf( i, j, k ) = inbuf( icurs )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    END DO
    RETURN
  END SUBROUTINE patch_2_outbuf_l

!!!!!!!!!!!!!!!!!!!!!!! GLOBAL TO PATCH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE wrf_global_to_patch_real (globbuf,buf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       REAL globbuf(*)
       REAL buf(*)

       CALL wrf_global_to_patch_generic (globbuf,buf,domdesc,stagger,ordering,RWORDSIZE,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       RETURN
    END SUBROUTINE wrf_global_to_patch_real

    SUBROUTINE wrf_global_to_patch_double (globbuf,buf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
! this next declaration is REAL, not DOUBLE PRECISION because it will be autopromoted
! to double precision by the compiler when WRF is compiled for 8 byte reals. Only reason
! for having this separate routine is so we pass the correct MPI type to mpi_scatterv
! since we were not indexing the globbuf and Field arrays it does not matter
       REAL globbuf(*)
       REAL buf(*)

       CALL wrf_global_to_patch_generic (globbuf,buf,domdesc,stagger,ordering,DWORDSIZE,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       RETURN
    END SUBROUTINE wrf_global_to_patch_double


    SUBROUTINE wrf_global_to_patch_integer (globbuf,buf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       INTEGER globbuf(*)
       INTEGER buf(*)

       CALL wrf_global_to_patch_generic (globbuf,buf,domdesc,stagger,ordering,IWORDSIZE,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       RETURN
    END SUBROUTINE wrf_global_to_patch_integer

    SUBROUTINE wrf_global_to_patch_logical (globbuf,buf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       LOGICAL globbuf(*)
       LOGICAL buf(*)

       CALL wrf_global_to_patch_generic (globbuf,buf,domdesc,stagger,ordering,LWORDSIZE,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       RETURN
    END SUBROUTINE wrf_global_to_patch_logical

    SUBROUTINE wrf_global_to_patch_generic (globbuf,buf,domdesc,stagger,ordering,typesize,&
                                       DS1a,DE1a,DS2a,DE2a,DS3a,DE3a,&
                                       MS1a,ME1a,MS2a,ME2a,MS3a,ME3a,&
                                       PS1a,PE1a,PS2a,PE2a,PS3a,PE3a )
       USE module_dm, ONLY : local_communicator, ntasks
       USE module_driver_constants
       IMPLICIT NONE
       INTEGER                         DS1a,DE1a,DS2a,DE2a,DS3a,DE3a,&
                                       MS1a,ME1a,MS2a,ME2a,MS3a,ME3a,&
                                       PS1a,PE1a,PS2a,PE2a,PS3a,PE3A
       CHARACTER *(*) stagger,ordering
       INTEGER domdesc,typesize,ierr
       REAL globbuf(*)
       REAL buf(*)
#ifndef STUBMPI
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       LOGICAL, EXTERNAL :: wrf_dm_on_monitor, has_char

       INTEGER i,j,k,ord,ord2d,ndim
       INTEGER  Patch(3,2), Gpatch(3,2,ntasks)
       REAL, ALLOCATABLE :: tmpbuf( : )
       REAL locbuf( (PE1a-PS1a+1)*(PE2a-PS2a+1)*(PE3a-PS3a+1)/RWORDSIZE*typesize+32 )

       DS1 = DS1a ; DE1 = DE1a ; DS2=DS2a ; DE2 = DE2a ; DS3 = DS3a ; DE3 = DE3a
       MS1 = MS1a ; ME1 = ME1a ; MS2=MS2a ; ME2 = ME2a ; MS3 = MS3a ; ME3 = ME3a
       PS1 = PS1a ; PE1 = PE1a ; PS2=PS2a ; PE2 = PE2a ; PS3 = PS3a ; PE3 = PE3a

       SELECT CASE ( TRIM(ordering) )
         CASE ( 'xy', 'yx' )
           ndim = 2
         CASE DEFAULT
           ndim = 3   ! where appropriate
       END SELECT

       SELECT CASE ( TRIM(ordering) )
         CASE ( 'xyz','xy' )
            ! the non-staggered variables come in at one-less than
            ! domain dimensions, but code wants full domain spec, so
            ! adjust if not staggered
           IF ( .NOT. has_char( stagger, 'x' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE2 = DE2+1
           IF ( ndim .EQ. 3 .AND. .NOT. has_char( stagger, 'z' ) ) DE3 = DE3+1
         CASE ( 'yxz','yx' )
           IF ( .NOT. has_char( stagger, 'x' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE1 = DE1+1
           IF ( ndim .EQ. 3 .AND. .NOT. has_char( stagger, 'z' ) ) DE3 = DE3+1
         CASE ( 'zxy' )
           IF ( .NOT. has_char( stagger, 'x' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE3 = DE3+1
           IF ( ndim .EQ. 3 .AND. .NOT. has_char( stagger, 'z' ) ) DE1 = DE1+1
         CASE ( 'xzy' )
           IF ( .NOT. has_char( stagger, 'x' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE3 = DE3+1
           IF ( ndim .EQ. 3 .AND. .NOT. has_char( stagger, 'z' ) ) DE2 = DE2+1
         CASE DEFAULT
       END SELECT

     ! moved to here to be after the potential recalculations of D dims
       IF ( wrf_dm_on_monitor() ) THEN
         ALLOCATE ( tmpbuf ( (DE1-DS1+1)*(DE2-DS2+1)*(DE3-DS3+1)/RWORDSIZE*typesize+32 ), STAT=ierr )
       ELSE
         ALLOCATE ( tmpbuf ( 1 ), STAT=ierr )
       END IF
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating tmpbuf in wrf_global_to_patch_generic')

       Patch(1,1) = ps1 ; Patch(1,2) = pe1    ! use patch dims
       Patch(2,1) = ps2 ; Patch(2,2) = pe2
       Patch(3,1) = ps3 ; Patch(3,2) = pe3

! defined in external/io_quilt
       CALL collect_on_comm0 (  local_communicator , IWORDSIZE ,  &
                                Patch , 6 ,                       &
                                GPatch , 6*ntasks                 )
       ndim = len(TRIM(ordering))

       IF ( wrf_dm_on_monitor() .AND. ndim .GE. 2 ) THEN
         IF      ( typesize .EQ. RWORDSIZE ) THEN
           CALL outbuf_2_patch_r ( globbuf , tmpbuf FRSTELEM ,    &
                                   DS1, DE1, DS2, DE2, DS3, DE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3 , &
                                   GPATCH                         )
         ELSE IF ( typesize .EQ. IWORDSIZE ) THEN
           CALL outbuf_2_patch_i ( globbuf , tmpbuf FRSTELEM ,    &
                                   DS1, DE1, DS2, DE2, DS3, DE3 , &
                                   GPATCH                         )
         ELSE IF ( typesize .EQ. DWORDSIZE ) THEN
           CALL outbuf_2_patch_d ( globbuf , tmpbuf FRSTELEM ,    &
                                   DS1, DE1, DS2, DE2, DS3, DE3 , &
                                   GPATCH                         )
         ELSE IF ( typesize .EQ. LWORDSIZE ) THEN
           CALL outbuf_2_patch_l ( globbuf , tmpbuf FRSTELEM ,    &
                                   DS1, DE1, DS2, DE2, DS3, DE3 , &
                                   GPATCH                         )
         END IF
       END IF

       CALL dist_on_comm0 (  local_communicator , typesize ,  &
                             tmpbuf FRSTELEM , (de1-ds1+1)*(de2-ds2+1)*(de3-ds3+1) , &
                             locbuf    , (pe1-ps1+1)*(pe2-ps2+1)*(pe3-ps3+1) )

       IF      ( typesize .EQ. RWORDSIZE ) THEN
         CALL all_sub_r ( locbuf , buf ,             &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )

       ELSE IF ( typesize .EQ. IWORDSIZE ) THEN
         CALL all_sub_i ( locbuf , buf ,             &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )
       ELSE IF ( typesize .EQ. DWORDSIZE ) THEN
         CALL all_sub_d ( locbuf , buf ,             &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )
       ELSE IF ( typesize .EQ. LWORDSIZE ) THEN
         CALL all_sub_l ( locbuf , buf ,             &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )
       END IF


       DEALLOCATE ( tmpbuf )
#endif
       RETURN
    END SUBROUTINE wrf_global_to_patch_generic

  SUBROUTINE all_sub_i ( inbuf , outbuf,              &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    IMPLICIT NONE
    INTEGER    , DIMENSION(*) , INTENT(IN) :: inbuf
    INTEGER   MS1,ME1,MS2,ME2,MS3,ME3
    INTEGER   PS1,PE1,PS2,PE2,PS3,PE3
    INTEGER    , DIMENSION( MS1:ME1,MS2:ME2,MS3:ME3 ) , INTENT(OUT) :: outbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
      DO k = PS3, PE3
        DO j = PS2, PE2
          DO i = PS1, PE1
            outbuf( i, j, k )  = inbuf ( icurs )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    RETURN
  END SUBROUTINE all_sub_i

  SUBROUTINE all_sub_r ( inbuf , outbuf,              &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    IMPLICIT NONE
    REAL       , DIMENSION(*) , INTENT(IN) :: inbuf
    INTEGER   MS1,ME1,MS2,ME2,MS3,ME3
    INTEGER   PS1,PE1,PS2,PE2,PS3,PE3
    REAL       , DIMENSION( MS1:ME1,MS2:ME2,MS3:ME3 ) , INTENT(OUT) :: outbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
      DO k = PS3, PE3
        DO j = PS2, PE2
          DO i = PS1, PE1
            outbuf( i, j, k )  = inbuf ( icurs )
            icurs = icurs + 1
          END DO
        END DO
      END DO

    RETURN
  END SUBROUTINE all_sub_r

  SUBROUTINE all_sub_d ( inbuf , outbuf,              &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    IMPLICIT NONE
    DOUBLE PRECISION    , DIMENSION(*) , INTENT(IN) :: inbuf
    INTEGER   MS1,ME1,MS2,ME2,MS3,ME3
    INTEGER   PS1,PE1,PS2,PE2,PS3,PE3
    DOUBLE PRECISION    , DIMENSION( MS1:ME1,MS2:ME2,MS3:ME3 ) , INTENT(OUT) :: outbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
      DO k = PS3, PE3
        DO j = PS2, PE2
          DO i = PS1, PE1
            outbuf( i, j, k )  = inbuf ( icurs )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    RETURN
  END SUBROUTINE all_sub_d

  SUBROUTINE all_sub_l ( inbuf , outbuf,              &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    IMPLICIT NONE
    LOGICAL    , DIMENSION(*) , INTENT(IN) :: inbuf
    INTEGER   MS1,ME1,MS2,ME2,MS3,ME3
    INTEGER   PS1,PE1,PS2,PE2,PS3,PE3
    LOGICAL    , DIMENSION( MS1:ME1,MS2:ME2,MS3:ME3 ) , INTENT(OUT) :: outbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
      DO k = PS3, PE3
        DO j = PS2, PE2
          DO i = PS1, PE1
            outbuf( i, j, k )  = inbuf ( icurs )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    RETURN
  END SUBROUTINE all_sub_l

  SUBROUTINE outbuf_2_patch_r( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3, &
                               MS1, ME1, MS2, ME2, MS3, ME3 , &
                               GPATCH )
    USE module_dm, ONLY : ntasks
    IMPLICIT NONE
    REAL    , DIMENSION(*) , INTENT(OUT) :: outbuf
    INTEGER   DS1,DE1,DS2,DE2,DS3,DE3,GPATCH(3,2,ntasks)
    INTEGER   MS1,ME1,MS2,ME2,MS3,ME3
    REAL    , DIMENSION( DS1:DE1,DS2:DE2,DS3:DE3 ) , INTENT(IN) :: inbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs

    icurs = 1
    DO n = 1, ntasks
      DO k = GPATCH( 3,1,n ), GPATCH( 3,2,n )
        DO j = GPATCH( 2,1,n ), GPATCH( 2,2,n )
          DO i = GPATCH( 1,1,n ), GPATCH( 1,2,n )
            outbuf( icurs ) = inbuf( i,j,k )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    END DO
    RETURN
  END SUBROUTINE outbuf_2_patch_r

  SUBROUTINE outbuf_2_patch_i( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm, ONLY : ntasks
    IMPLICIT NONE
    INTEGER    , DIMENSION(*) , INTENT(OUT) :: outbuf
    INTEGER   DS1,DE1,DS2,DE2,DS3,DE3,GPATCH(3,2,ntasks)
    INTEGER    , DIMENSION( DS1:DE1,DS2:DE2,DS3:DE3 ) , INTENT(IN) :: inbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
    DO n = 1, ntasks
      DO k = GPATCH( 3,1,n ), GPATCH( 3,2,n )
        DO j = GPATCH( 2,1,n ), GPATCH( 2,2,n )
          DO i = GPATCH( 1,1,n ), GPATCH( 1,2,n )
            outbuf( icurs ) = inbuf( i,j,k )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    END DO
    RETURN
  END SUBROUTINE outbuf_2_patch_i

  SUBROUTINE outbuf_2_patch_d( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm, ONLY : ntasks
    IMPLICIT NONE
    DOUBLE PRECISION    , DIMENSION(*) , INTENT(OUT) :: outbuf
    INTEGER   DS1,DE1,DS2,DE2,DS3,DE3,GPATCH(3,2,ntasks)
    DOUBLE PRECISION    , DIMENSION( DS1:DE1,DS2:DE2,DS3:DE3 ) , INTENT(IN) :: inbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
    DO n = 1, ntasks
      DO k = GPATCH( 3,1,n ), GPATCH( 3,2,n )
        DO j = GPATCH( 2,1,n ), GPATCH( 2,2,n )
          DO i = GPATCH( 1,1,n ), GPATCH( 1,2,n )
            outbuf( icurs ) = inbuf( i,j,k )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    END DO
    RETURN
  END SUBROUTINE outbuf_2_patch_d

  SUBROUTINE outbuf_2_patch_l( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm, ONLY : ntasks
    IMPLICIT NONE
    LOGICAL    , DIMENSION(*) , INTENT(OUT) :: outbuf
    INTEGER   DS1,DE1,DS2,DE2,DS3,DE3,GPATCH(3,2,ntasks)
    LOGICAL    , DIMENSION( DS1:DE1,DS2:DE2,DS3:DE3 ) , INTENT(IN) :: inbuf
! Local
    INTEGER               :: i,j,k,n   ,  icurs
    icurs = 1
    DO n = 1, ntasks
      DO k = GPATCH( 3,1,n ), GPATCH( 3,2,n )
        DO j = GPATCH( 2,1,n ), GPATCH( 2,2,n )
          DO i = GPATCH( 1,1,n ), GPATCH( 1,2,n )
            outbuf( icurs ) = inbuf( i,j,k )
            icurs = icurs + 1
          END DO
        END DO
      END DO
    END DO
    RETURN
  END SUBROUTINE outbuf_2_patch_l


  SUBROUTINE wrf_dm_nestexchange_init
      CALL rsl_lite_nesting_reset
  END SUBROUTINE wrf_dm_nestexchange_init


!------------------------------------------------------------------

#if ( EM_CORE == 1 && DA_CORE != 1 )

!------------------------------------------------------------------

   SUBROUTINE force_domain_em_part2 ( grid, ngrid, pgrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, local_communicator, mytask, &
                            nest_pes_x, nest_pes_y ! ,                                 &
                            !push_communicators_for_domain,pop_communicators_for_domain
      USE module_comm_nesting_dm, ONLY : halo_force_down_sub
      USE module_model_constants
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
      TYPE(domain), POINTER :: pgrid         !KAL added for vertical nesting
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k,kk
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe
      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7,itrace
      REAL  dummy_xs, dummy_xe, dummy_ys, dummy_ye

      !KAL variables for vertical nesting
      REAL :: p_top_m  , p_surf_m , mu_m , hsca_m , pre_c ,pre_n
      REAL, DIMENSION(pgrid%s_vert:pgrid%e_vert) :: alt_w_c
      REAL, DIMENSION(pgrid%s_vert:pgrid%e_vert+1) :: alt_u_c
      REAL, DIMENSION(ngrid%s_vert:ngrid%e_vert) :: alt_w_n
      REAL, DIMENSION(ngrid%s_vert:ngrid%e_vert+1) :: alt_u_n      
      
      REAL, DIMENSION(:,:,:), ALLOCATABLE :: p, al
      REAL :: pfu, pfd, phm, temp, qvf, qvf1, qvf2    

      !KAL change this for vertical nesting
      ! force_domain_em_part1 packs up the interpolation onto the coarse (vertical) grid
      ! therefore the message size is based on the coarse grid number of levels
      ! here it is unpacked onto the intermediate grid
      CALL get_ijk_from_grid (  pgrid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
                                
      !KAL this is the original WRF code                                
      !CALL get_ijk_from_grid (  grid ,                   &
      !                          cids, cide, cjds, cjde, ckds, ckde,    &
      !                          cims, cime, cjms, cjme, ckms, ckme,    &
      !                          cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

#include "nest_interpdown_unpack.inc"

if (ngrid%vert_refine_method .NE. 0) then

      !KAL calculating the vertical coordinate for parent and nest grid (code from ndown)
      ! assume that the parent and nest have the same p_top value (as in ndown)
      
!KAL ckde is equal to e_vert of the coarse grid. There are e_vert-1 u points.  The coarse 1D grid here is e_vert+1,
!    so it is the e_vert-1 points from the coarse grid, plus a surface point plus a top point.  Extrapolation coefficients
!    are used to get the surface and top points to fill out the pro_u_c 1D array of u values from the coarse grid.     
                
      hsca_m = 6.7 !KAL scale height of the atmosphere
      p_top_m = ngrid%p_top
      p_surf_m = 1.e5
      mu_m = p_surf_m - p_top_m
!    parent
      do  k = 1,ckde
      pre_c = mu_m * pgrid%c3f(k) + p_top_m + pgrid%c4f(k)
      alt_w_c(k) =  -hsca_m * alog(pre_c/p_surf_m)
      enddo   
      do  k = 1,ckde-1
      pre_c = mu_m * pgrid%c3h(k) + p_top_m + pgrid%c4h(k)
      alt_u_c(k+1) =  -hsca_m * alog(pre_c/p_surf_m)
      enddo
      alt_u_c(1) =  alt_w_c(1)
      alt_u_c(ckde+1) =  alt_w_c(ckde)       
!    nest
      do  k = 1,nkde
      pre_n = mu_m * ngrid%c3f(k) + p_top_m + ngrid%c4f(k)
      alt_w_n(k) =  -hsca_m * alog(pre_n/p_surf_m)
      enddo
      do  k = 1,nkde-1
      pre_n = mu_m * ngrid%c3h(k) + p_top_m + ngrid%c4h(k)
      alt_u_n(k+1) =  -hsca_m * alog(pre_n/p_surf_m)
      enddo
      alt_u_n(1) =  alt_w_n(1)
      alt_u_n(nkde+1) =  alt_w_n(nkde)
        
endif   

      !KAL added this call for vertical nesting (return coarse grid dimensions to intended values)
      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
                                                      
      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

      !  Vertical refinement is turned on.

      IF (ngrid%vert_refine_method .NE. 0) THEN
      
#include "nest_forcedown_interp_vert.inc"

         IF ( ngrid%this_is_an_ideal_run ) THEN
            IF ( SIZE( grid%t_init, 1 ) * SIZE( grid%t_init, 3 ) .GT. 1 ) THEN
               CALL vert_interp_vert_nesting( grid%t_init, & !CD field
                                              ids, ide, kds, kde, jds, jde, & !CD dims
                                              ims, ime, kms, kme, jms, jme, & !CD dims
                                              ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                              pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                              pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                              alt_u_c, alt_u_n ) !coordinates for parent and nest
            END IF  ! Check t_init is a fully allocated 3d array.
         END IF ! only for ideal runs


         !  Rebalance the grid on the intermediate grid.  The intermediate grid has the horizontal
         !  resolution of the parent grid, but at this point has been interpolated in the vertical
         !  to the resolution of the nest.  The base state (phb, pb, etc) from the parent grid is
         !  unpacked onto the intermediate grid every time this subroutine is called.  We need the
         !  base state of the nest, so it is recalculated here.  

         !  Additionally, we do not need to vertically interpolate the entire intermediate grid
         !  above, just the points that contribute to the boundary forcing.

         !  Base state potential temperature and inverse density (alpha = 1/rho) from
         !  the half eta levels and the base-profile surface pressure.  Compute 1/rho
         !  from equation of state.  The potential temperature is a perturbation from t0.
      
         !  Uncouple the variables moist and t_2 that are used to calculate ph_2

         DO j = MAX(jds,jps),MIN(jde-1,jpe)
            DO i = MAX(ids,ips),MIN(ide-1,ipe)
               DO k=kds,kde-1
                  grid%t_2(i,k,j) = grid%t_2(i,k,j)/((ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k)) + (ngrid%c1h(k)*grid%mu_2(i,j)))
                  moist(i,k,j,P_QV) = moist(i,k,j,P_QV)/((ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k)) + (ngrid%c1h(k)*grid%mu_2(i,j)))
               END DO
            END DO
         END DO
    
         DO j = MAX(jds,jps),MIN(jde-1,jpe)
            DO i = MAX(ids,ips),MIN(ide-1,ipe)

               DO k = 1, kpe-1
                  grid%pb(i,k,j) = ngrid%c3h(k)*(ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k)) + ngrid%c4h(k) + ngrid%p_top
             
                  !  If this is a real run, recalc t_init.
   
                  IF ( .NOT. ngrid%this_is_an_ideal_run ) THEN
                     temp = MAX ( ngrid%tiso, ngrid%t00 + ngrid%tlp*LOG(grid%pb(i,k,j)/ngrid%p00) )
                     IF ( grid%pb(i,k,j) .LT. ngrid%p_strat ) THEN
                        temp = ngrid%tiso + ngrid%tlp_strat * LOG ( grid%pb(i,k,j)/ngrid%p_strat )
                     END IF
                     grid%t_init(i,k,j) = temp*(ngrid%p00/grid%pb(i,k,j))**(r_d/cp) - t0
                  END IF
                  grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
               END DO
   
               !  Integrate base geopotential, starting at terrain elevation.  This assures that
               !  the base state is in exact hydrostatic balance with respect to the model equations.
               !  This field is on full levels.
   
               grid%phb(i,1,j) = grid%ht(i,j) * g
               IF (grid%hypsometric_opt == 1) THEN
                  DO kk = 2,kpe
                     k = kk - 1
                     grid%phb(i,kk,j) = grid%phb(i,k,j) - ngrid%dnw(k)*(ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k))*grid%alb(i,k,j)
                  END DO
               ELSE IF (grid%hypsometric_opt == 2) THEN
                  DO k = 2,kpe
                     pfu = ngrid%c3f(k  )*grid%MUB(i,j) + ngrid%c4f(k  ) + ngrid%p_top
                     pfd = ngrid%c3f(k-1)*grid%MUB(i,j) + ngrid%c4f(k-1) + ngrid%p_top
                     phm = ngrid%c3h(k-1)*grid%MUB(i,j) + ngrid%c4h(k-1) + ngrid%p_top
                     grid%phb(i,k,j) = grid%phb(i,k-1,j) + grid%alb(i,k-1,j)*phm*LOG(pfd/pfu)
                  END DO
               ELSE
                  CALL wrf_error_fatal( 'module_dm: hypsometric_opt should be 1 or 2' )
               END IF  ! which hypsometric option
            END DO  ! i loop
         END DO  ! j loop
         !  Perturbation fields
         ALLOCATE( p (ips:ipe, kps:kpe, jps:jpe) )
         ALLOCATE( al(ips:ipe, kps:kpe, jps:jpe) )
         DO j = MAX(jds,jps),MIN(jde-1,jpe)
            DO i = MAX(ids,ips),MIN(ide-1,ipe)
               !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum
               !  equation) down from the top to get the pressure perturbation.  First get the pressure
               !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.
      
               kk = kpe-1
               k = kk+1
      
               qvf1 = 0.5*(moist(i,kk,j,P_QV)+moist(i,kk,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2
      
               p(i,kk,j) = - 0.5*((ngrid%c1f(k)*grid%Mu_2(i,j))+qvf1*(ngrid%c1f(k)*grid%Mub(i,j)+ngrid%c2f(k)))/ngrid%rdnw(kk)/qvf2
               qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
               al(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf* &
                           (((p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm) - grid%alb(i,kk,j)
      
               !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
               !  inverse density fields (total and perturbation).
      
               DO kk=kpe-2,1,-1
                  k = kk + 1
                  qvf1 = 0.5*(moist(i,kk,j,P_QV)+moist(i,kk+1,j,P_QV))
                  qvf2 = 1./(1.+qvf1)
                  qvf1 = qvf1*qvf2
                  p(i,kk,j) = p(i,kk+1,j) - ((ngrid%c1f(k)*grid%Mu_2(i,j)) + qvf1*(ngrid%c1f(k)*grid%Mub(i,j)+ngrid%c2f(k)))/qvf2/ngrid%rdn(kk+1)
                  qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
                  al(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf* &
                              (((p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm) - grid%alb(i,kk,j)
               END DO
      
               !  This is the hydrostatic equation used in the model after the small timesteps.  In
               !  the model, grid%al (inverse density) is computed from the geopotential.
      
               IF (grid%hypsometric_opt == 1) THEN
                  DO kk = 2,kpe
                     k = kk - 1
                     grid%ph_2(i,kk,j) = grid%ph_2(i,kk-1,j) - &
                                        ngrid%dnw(kk-1) * ( ((ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k))+(ngrid%c1h(k)*grid%mu_2(i,j)))*al(i,kk-1,j) &
                                        + (ngrid%c1h(k)*grid%mu_2(i,j))*grid%alb(i,kk-1,j) )
                  END DO
      
               ! Alternative hydrostatic eq.: dZ = -al*p*dLOG(p), where p is dry pressure.
               ! Note that al*p approximates Rd*T and dLOG(p) does z.
               ! Here T varies mostly linear with z, the first-order integration produces better result.
      
               ELSE IF (grid%hypsometric_opt == 2) THEN
      
                  grid%ph_2(i,1,j) = grid%phb(i,1,j)
                  DO k = 2,kpe
                     pfu = ngrid%c3f(k  )*( grid%MUB(i,j)+grid%MU_2(i,j) ) + ngrid%c4f(k  ) + ngrid%p_top
                     pfd = ngrid%c3f(k-1)*( grid%MUB(i,j)+grid%MU_2(i,j) ) + ngrid%c4f(k-1) + ngrid%p_top
                     phm = ngrid%c3h(k-1)*( grid%MUB(i,j)+grid%MU_2(i,j) ) + ngrid%c4h(k-1) + ngrid%p_top
                     grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + (grid%alb(i,k-1,j)+al(i,k-1,j))*phm*LOG(pfd/pfu)
                  END DO
      
                  DO k = 1,kpe
                     grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
                  END DO
      
               END IF

            END DO ! i loop
         END DO ! j loop

         DEALLOCATE(p)
         DEALLOCATE(al)
      
         ! Couple the variables moist and t_2, and the newly calculated ph_2
         DO j = MAX(jds,jps),MIN(jde-1,jpe)
            DO i = MAX(ids,ips),MIN(ide-1,ipe)
               DO k=kps,kpe
               grid%ph_2(i,k,j) = grid%ph_2(i,k,j)*((ngrid%c1f(k)*grid%Mub(i,j)+ngrid%c2f(k)) + (ngrid%c1f(k)*grid%Mu_2(i,j)))
               END DO
            END DO
         END DO
         DO j = MAX(jds,jps),MIN(jde-1,jpe)
            DO i = MAX(ids,ips),MIN(ide-1,ipe)
               DO k=kps,kpe-1
               grid%t_2(i,k,j) = grid%t_2(i,k,j)*((ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k)) + (ngrid%c1h(k)*grid%mu_2(i,j)))
               moist(i,k,j,P_QV) = moist(i,k,j,P_QV)*((ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k)) + (ngrid%c1h(k)*grid%mu_2(i,j)))
               END DO
            END DO
         END DO


      END IF
                               

#include "HALO_FORCE_DOWN.inc"

      ! code here to interpolate the data into the nested domain
# include "nest_forcedown_interp.inc"

      RETURN
   END SUBROUTINE force_domain_em_part2

!------------------------------------------------------------------

   SUBROUTINE interp_domain_em_part1 ( grid, intermediate_grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, &
                            nest_task_offsets, nest_pes_x, nest_pes_y, which_kid,   &
                            intercomm_active, mpi_comm_to_kid, mpi_comm_to_mom,     &
                            mytask, get_dm_max_halo_width
      USE module_timing
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      INTEGER iparstrt,jparstrt,sw
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER icoord, jcoord, idim_cd, jdim_cd, pgr
      INTEGER thisdomain_max_halo_width
      INTEGER local_comm, myproc, nproc
      INTEGER ioffset, ierr

      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_myproc( myproc )
      CALL wrf_get_nproc( nproc )

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  intermediate_grid ,              &
                                iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      CALL nl_get_parent_grid_ratio ( ngrid%id, pgr )
      CALL nl_get_i_parent_start ( intermediate_grid%id, iparstrt )
      CALL nl_get_j_parent_start ( intermediate_grid%id, jparstrt )
      CALL nl_get_shw            ( intermediate_grid%id, sw )
      icoord =    iparstrt - sw
      jcoord =    jparstrt - sw
      idim_cd = iide - iids + 1
      jdim_cd = ijde - ijds + 1

      nlev  = ckde - ckds + 1

      ! get max_halo_width for parent. It may be smaller if it is moad
      CALL get_dm_max_halo_width ( grid%id , thisdomain_max_halo_width )

      IF ( grid%active_this_task ) THEN
#include "nest_interpdown_pack.inc"
      END IF

      ! determine which communicator and offset to use
      IF ( intercomm_active( grid%id ) ) THEN        ! I am parent
        local_comm = mpi_comm_to_kid( which_kid(ngrid%id), grid%id )
        ioffset = nest_task_offsets(ngrid%id)
      ELSE IF ( intercomm_active( ngrid%id ) ) THEN  ! I am nest
        local_comm = mpi_comm_to_mom( ngrid%id )
        ioffset = nest_task_offsets(ngrid%id)
      END IF

      IF ( grid%active_this_task .OR. ngrid%active_this_task ) THEN
#ifndef STUBMPI
        CALL mpi_comm_rank(local_comm,myproc,ierr)
        CALL mpi_comm_size(local_comm,nproc,ierr)
#endif
        CALL rsl_lite_bcast_msgs( myproc, nest_pes_x(grid%id)*nest_pes_y(grid%id),         &
                                          nest_pes_x(ngrid%id)*nest_pes_y(ngrid%id),       &
                                          ioffset, local_comm )
      END IF

      RETURN
   END SUBROUTINE interp_domain_em_part1

!------------------------------------------------------------------

   SUBROUTINE interp_domain_em_part2 ( grid, ngrid, pgrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, &
                            mytask, get_dm_max_halo_width, which_kid
                            ! push_communicators_for_domain,pop_communicators_for_domain
      USE module_comm_nesting_dm, ONLY : halo_interp_down_sub
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
      TYPE(domain), POINTER :: pgrid         !KAL added for vertical nesting
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER myproc
      INTEGER ierr
      INTEGER thisdomain_max_halo_width

      !KAL variables for vertical nesting
      REAL :: p_top_m  , p_surf_m , mu_m , hsca_m , pre_c ,pre_n
      REAL, DIMENSION(pgrid%s_vert:pgrid%e_vert) :: alt_w_c
      REAL, DIMENSION(pgrid%s_vert:pgrid%e_vert+1) :: alt_u_c
      REAL, DIMENSION(ngrid%s_vert:ngrid%e_vert) :: alt_w_n
      REAL, DIMENSION(ngrid%s_vert:ngrid%e_vert+1) :: alt_u_n


      !KAL change this for vertical nesting
      ! interp_domain_em_part1 packs up the interpolation onto the coarse (vertical) grid
      ! therefore the message size is based on the coarse grid number of levels
      ! here it is unpacked onto the intermediate grid
       CALL get_ijk_from_grid ( pgrid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      !KAL this is the original WRF code
      !CALL get_ijk_from_grid (  grid ,                   &
      !                          cids, cide, cjds, cjde, ckds, ckde,    &
      !                          cims, cime, cjms, cjme, ckms, ckme,    &
      !                          cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

      CALL get_dm_max_halo_width ( ngrid%id , thisdomain_max_halo_width )

#include "nest_interpdown_unpack.inc"


if (ngrid%vert_refine_method .NE. 0) then

      !KAL calculating the vertical coordinate for parent and nest grid (code from ndown)
      ! assume that the parent and nest have the same p_top value (as in ndown)
      
!KAL ckde is equal to e_vert of the coarse grid. There are e_vert-1 u points.  The coarse 1D grid here is e_vert+1,
!    so it is the e_vert-1 points from the coarse grid, plus a surface point plus a top point.  Extrapolation coefficients
!    are used to get the surface and top points to fill out the pro_u_c 1D array of u values from the coarse grid.     
                
      hsca_m = 6.7 !KAL scale height of the atmosphere
      p_top_m = ngrid%p_top
      p_surf_m = 1.e5
      mu_m = p_surf_m - p_top_m
!    parent
      do  k = 1,ckde
      pre_c = mu_m * pgrid%c3f(k) + p_top_m + pgrid%c4f(k)
      alt_w_c(k) =  -hsca_m * alog(pre_c/p_surf_m)
      enddo   
      do  k = 1,ckde-1
      pre_c = mu_m * pgrid%c3h(k) + p_top_m + pgrid%c4h(k)
      alt_u_c(k+1) =  -hsca_m * alog(pre_c/p_surf_m)
      enddo
      alt_u_c(1) =  alt_w_c(1)
      alt_u_c(ckde+1) =  alt_w_c(ckde)       
!    nest
      do  k = 1,nkde
      pre_n = mu_m * ngrid%c3f(k) + p_top_m + ngrid%c4f(k)
      alt_w_n(k) =  -hsca_m * alog(pre_n/p_surf_m)
      enddo
      do  k = 1,nkde-1
      pre_n = mu_m * ngrid%c3h(k) + p_top_m + ngrid%c4h(k)
      alt_u_n(k+1) =  -hsca_m * alog(pre_n/p_surf_m)
      enddo
      alt_u_n(1) =  alt_w_n(1)
      alt_u_n(nkde+1) =  alt_w_n(nkde)
endif   



      !KAL added this call for vertical nesting (return coarse grid dimensions to intended values)
      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )


if (ngrid%vert_refine_method .NE. 0) then
      
!KAL added this code (the include file) for the vertical nesting
#include "nest_interpdown_interp_vert.inc"


      !KAL finish off the 1-D variables (t_base, u_base, v_base, qv_base, and z_base) (move this out of here if alt_u_c and alt_u_n are calculated elsewhere)
      CALL vert_interp_vert_nesting_1d ( &         
                                        ngrid%t_base,                                           &    ! CD field
                                        ids, ide, kds, kde, jds, jde,                           &    ! CD dims
                                        ims, ime, kms, kme, jms, jme,                           &    ! CD dims
                                        ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe,           &    ! CD dims
                                        pgrid%s_vert, pgrid%e_vert,                             &    ! vertical dimension of the parent grid
                                        pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, &    ! coarse grid extrapolation constants
                                        alt_u_c, alt_u_n)                                            ! coordinates for parent and nest
      CALL vert_interp_vert_nesting_1d ( &         
                                        ngrid%u_base,                                           &    ! CD field
                                        ids, ide, kds, kde, jds, jde,                           &    ! CD dims
                                        ims, ime, kms, kme, jms, jme,                           &    ! CD dims
                                        ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe,           &    ! CD dims
                                        pgrid%s_vert, pgrid%e_vert,                             &    ! vertical dimension of the parent grid
                                        pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, &    ! coarse grid extrapolation constants
                                        alt_u_c, alt_u_n)                                            ! coordinates for parent and nest
      CALL vert_interp_vert_nesting_1d ( &         
                                        ngrid%v_base,                                           &    ! CD field
                                        ids, ide, kds, kde, jds, jde,                           &    ! CD dims
                                        ims, ime, kms, kme, jms, jme,                           &    ! CD dims
                                        ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe,           &    ! CD dims
                                        pgrid%s_vert, pgrid%e_vert,                             &    ! vertical dimension of the parent grid
                                        pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, &    ! coarse grid extrapolation constants
                                        alt_u_c, alt_u_n)                                            ! coordinates for parent and nest
      CALL vert_interp_vert_nesting_1d ( &         
                                        ngrid%qv_base,                                          &    ! CD field
                                        ids, ide, kds, kde, jds, jde,                           &    ! CD dims
                                        ims, ime, kms, kme, jms, jme,                           &    ! CD dims
                                        ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe,           &    ! CD dims
                                        pgrid%s_vert, pgrid%e_vert,                             &    ! vertical dimension of the parent grid
                                        pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, &    ! coarse grid extrapolation constants
                                        alt_u_c, alt_u_n)                                            ! coordinates for parent and nest
      CALL vert_interp_vert_nesting_1d ( &         
                                        ngrid%z_base,                                           &    ! CD field
                                        ids, ide, kds, kde, jds, jde,                           &    ! CD dims
                                        ims, ime, kms, kme, jms, jme,                           &    ! CD dims
                                        ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe,           &    ! CD dims
                                        pgrid%s_vert, pgrid%e_vert,                             &    ! vertical dimension of the parent grid
                                        pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, &    ! coarse grid extrapolation constants
                                        alt_u_c, alt_u_n)                                            ! coordinates for parent and nest

endif
        
        CALL push_communicators_for_domain( grid%id )

#include "HALO_INTERP_DOWN.inc"

        CALL pop_communicators_for_domain

# include "nest_interpdown_interp.inc"

      RETURN
   END SUBROUTINE interp_domain_em_part2

!------------------------------------------------------------------

   SUBROUTINE interp_domain_em_small_part1 ( grid, intermediate_grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_comm_dm, ONLY: halo_em_horiz_interp_sub
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, &
                            mytask, get_dm_max_halo_width,                          &
                            nest_task_offsets, mpi_comm_to_kid, mpi_comm_to_mom,    &
                            which_kid, nest_pes_x, nest_pes_y, intercomm_active
      USE module_timing
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      INTEGER iparstrt,jparstrt,sw
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::           ids,  ide,  jds,  jde,  kds,  kde,    &
                                 ims,  ime,  jms,  jme,  kms,  kme,    &
                                 ips,  ipe,  jps,  jpe,  kps,  kpe

      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER icoord, jcoord, idim_cd, jdim_cd, pgr
      INTEGER thisdomain_max_halo_width
      INTEGER local_comm, myproc, nproc
      INTEGER ioffset

      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_myproc( myproc )
      CALL wrf_get_nproc( nproc )

      CALL get_ijk_from_grid (  grid ,                           &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe     )
#ifdef DM_PARALLEL
# include "HALO_EM_HORIZ_INTERP.inc"
#endif

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  intermediate_grid ,              &
                                iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      CALL nl_get_parent_grid_ratio ( ngrid%id, pgr )
      CALL nl_get_i_parent_start ( intermediate_grid%id, iparstrt )
      CALL nl_get_j_parent_start ( intermediate_grid%id, jparstrt )
      CALL nl_get_shw            ( intermediate_grid%id, sw )
      icoord =    iparstrt - sw
      jcoord =    jparstrt - sw
      idim_cd = iide - iids + 1
      jdim_cd = ijde - ijds + 1

      nlev  = ckde - ckds + 1

      ! get max_halo_width for parent. It may be smaller if it is moad
      CALL get_dm_max_halo_width ( grid%id , thisdomain_max_halo_width )

      !  How many 3d arrays, so far just 3d theta-300 and geopotential perturbation,
      !  and the 2d topo elevation, three max press/temp/height fields, and three
      !  min press/temp/height fields.
   
      msize = ( 2 )* nlev + 7
   
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, calling rsl_lite_to_child')
      CALL rsl_lite_to_child_info( local_communicator, msize*RWORDSIZE     &
                              ,cips,cipe,cjps,cjpe                         &
                              ,iids,iide,ijds,ijde                         &
                              ,nids,nide,njds,njde                         &
                              ,pgr , sw                                    &
                              ,ntasks_x,ntasks_y                           &
                              ,thisdomain_max_halo_width                   &
                              ,icoord,jcoord                               &
                              ,idim_cd,jdim_cd                             &
                              ,pig,pjg,retval )
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, back from rsl_lite_to_child')
      DO while ( retval .eq. 1 )
         IF ( SIZE(grid%ph_2) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, ph_2')
            DO k = ckds,ckde
               xv(k)= grid%ph_2(pig,k,pjg)
            END DO
            CALL rsl_lite_to_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%t_2) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, t_2')
            DO k = ckds,(ckde-1)
               xv(k)= grid%t_2(pig,k,pjg)
            END DO
            CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%ht) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, ht')
            xv(1)= grid%ht(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%t_max_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, t_max_p')
            xv(1)= grid%t_max_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%ght_max_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, ght_max_p')
            xv(1)= grid%ght_max_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%max_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, max_p')
            xv(1)= grid%max_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%t_min_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, t_min_p')
            xv(1)= grid%t_min_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%ght_min_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, ght_min_p')
            xv(1)= grid%ght_min_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%min_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, min_p')
            xv(1)= grid%min_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, calling rsl_lite_to_child_info')
         CALL rsl_lite_to_child_info( local_communicator, msize*RWORDSIZE  &
                                     ,cips,cipe,cjps,cjpe                  &
                                     ,iids,iide,ijds,ijde                  &
                                     ,nids,nide,njds,njde                  &
                                     ,pgr , sw                             &
                                     ,ntasks_x,ntasks_y                    &
                                     ,thisdomain_max_halo_width            &
                                     ,icoord,jcoord                        &
                                     ,idim_cd,jdim_cd                      &
                                     ,pig,pjg,retval )
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, back from rsl_lite_to_child_info')
      END DO

      ! determine which communicator and offset to use
      IF ( intercomm_active( grid%id ) ) THEN        ! I am parent
        local_comm = mpi_comm_to_kid( which_kid(ngrid%id), grid%id )
        ioffset = nest_task_offsets(ngrid%id)
      ELSE IF ( intercomm_active( ngrid%id ) ) THEN  ! I am nest
        local_comm = mpi_comm_to_mom( ngrid%id )
        ioffset = nest_task_offsets(ngrid%id)
      END IF

!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, calling rsl_lite_bcast')
      CALL rsl_lite_bcast_msgs( myproc, nest_pes_x(grid%id)*nest_pes_y(grid%id),         &
                                        nest_pes_x(ngrid%id)*nest_pes_y(ngrid%id),       &
                                        ioffset, local_comm )
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, back from rsl_lite_bcast')

      RETURN
   END SUBROUTINE interp_domain_em_small_part1

!------------------------------------------------------------------

   SUBROUTINE interp_domain_em_small_part2 ( grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, &
                            mytask, get_dm_max_halo_width
      USE module_comm_nesting_dm, ONLY : halo_interp_down_sub
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER myproc
      INTEGER ierr
      INTEGER thisdomain_max_halo_width

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

      CALL get_dm_max_halo_width ( ngrid%id , thisdomain_max_halo_width )

      CALL rsl_lite_from_parent_info(pig,pjg,retval)
      
      DO while ( retval .eq. 1 )
      
         IF ( SIZE(grid%ph_2) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
            DO k = ckds,ckde
               grid%ph_2(pig,k,pjg) = xv(k)
            END DO
         END IF
   
         IF ( SIZE(grid%t_2) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
            DO k = ckds,(ckde-1)
               grid%t_2(pig,k,pjg) = xv(k)
            END DO
         END IF
   
         IF ( SIZE(grid%ht) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%ht(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%t_max_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%t_max_p(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%ght_max_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%ght_max_p(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%max_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%max_p(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%t_min_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%t_min_p(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%ght_min_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%ght_min_p(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%min_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%min_p(pig,pjg) = xv(1)
         END IF
      
         CALL rsl_lite_from_parent_info(pig,pjg,retval)
         
      END DO

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#include "HALO_INTERP_DOWN.inc"

      CALL interp_fcn_bl ( grid%ph_2,                                           &       
                           cids, cide, ckds, ckde, cjds, cjde,                  &         
                           cims, cime, ckms, ckme, cjms, cjme,                  &         
                           cips, cipe, ckps, MIN( ckde, ckpe ), cjps, cjpe,     &         
                           ngrid%ph_2,                                          &   
                           nids, nide, nkds, nkde, njds, njde,                  &         
                           nims, nime, nkms, nkme, njms, njme,                  &         
                           nips, nipe, nkps, MIN( nkde, nkpe ), njps, njpe,     &         
                           config_flags%shw, ngrid%imask_nostag,                &         
                           .FALSE., .FALSE.,                                    &         
                           ngrid%i_parent_start, ngrid%j_parent_start,          &
                           ngrid%parent_grid_ratio, ngrid%parent_grid_ratio,    &
                           grid%ht, ngrid%ht,                                   &
                           grid%t_max_p, ngrid%t_max_p,                         &
                           grid%ght_max_p, ngrid%ght_max_p,                     &
                           grid%max_p, ngrid%max_p,                             &
                           grid%t_min_p, ngrid%t_min_p,                         &
                           grid%ght_min_p, ngrid%ght_min_p,                     &
                           grid%min_p, ngrid%min_p,                             &
                           ngrid%znw, ngrid%p_top                               )
      
      CALL interp_fcn_bl ( grid%t_2,                                            &       
                           cids, cide, ckds, ckde, cjds, cjde,                  &         
                           cims, cime, ckms, ckme, cjms, cjme,                  &         
                           cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe, &         
                           ngrid%t_2,                                           &   
                           nids, nide, nkds, nkde, njds, njde,                  &         
                           nims, nime, nkms, nkme, njms, njme,                  &         
                           nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe, &         
                           config_flags%shw, ngrid%imask_nostag,                &         
                           .FALSE., .FALSE.,                                    &         
                           ngrid%i_parent_start, ngrid%j_parent_start,          &
                           ngrid%parent_grid_ratio, ngrid%parent_grid_ratio,    &
                           grid%ht, ngrid%ht,                                   &
                           grid%t_max_p, ngrid%t_max_p,                         &
                           grid%ght_max_p, ngrid%ght_max_p,                     &
                           grid%max_p, ngrid%max_p,                             &
                           grid%t_min_p, ngrid%t_min_p,                         &
                           grid%ght_min_p, ngrid%ght_min_p,                     &
                           grid%min_p, ngrid%min_p,                             &
                           ngrid%znu, ngrid%p_top                               )

      RETURN
   END SUBROUTINE interp_domain_em_small_part2

!------------------------------------------------------------------

   SUBROUTINE feedback_nest_prep ( grid, config_flags    &
!
#include "dummy_new_args.inc"
!
)
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask !, &
                             !push_communicators_for_domain, pop_communicators_for_domain
      USE module_comm_nesting_dm, ONLY : halo_interp_up_sub
      IMPLICIT NONE
!
      TYPE(domain), TARGET :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE (grid_config_rec_type) :: config_flags ! configureation flags, has vertical dim of
                                                  ! soil temp, moisture, etc., has vertical dim
                                                  ! of soil categories
#include "dummy_new_decl.inc"

      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER       :: idum1, idum2


      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

    IF ( grid%active_this_task ) THEN
      CALL push_communicators_for_domain( grid%id )

#ifdef DM_PARALLEL
#include "HALO_INTERP_UP.inc"
#endif

      CALL pop_communicators_for_domain
    END IF

   END SUBROUTINE feedback_nest_prep

!------------------------------------------------------------------

   SUBROUTINE feedback_domain_em_part1 ( grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
                            ipe_save, jpe_save, ips_save, jps_save,                         &
                            nest_pes_x, nest_pes_y

      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE(domain), POINTER :: xgrid
      TYPE (grid_config_rec_type)            :: config_flags, nconfig_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER local_comm, myproc, nproc, idum1, idum2
      INTEGER thisdomain_max_halo_width

!cyl: add variables for trajectory
      integer tjk

      INTERFACE
          SUBROUTINE feedback_nest_prep ( grid, config_flags    &
!
#include "dummy_new_args.inc"
!
)
             USE module_state_description
             USE module_domain, ONLY : domain
             USE module_configure, ONLY : grid_config_rec_type
!
             TYPE (grid_config_rec_type)            :: config_flags
             TYPE(domain), TARGET                   :: grid
#include "dummy_new_decl.inc"
          END SUBROUTINE feedback_nest_prep
      END INTERFACE
!

      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_myproc( myproc )
      CALL wrf_get_nproc( nproc )

!
! intermediate grid
      CALL get_ijk_from_grid (  grid ,                                 &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
! nest grid
      CALL get_ijk_from_grid (  ngrid ,                                &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

      ips_save = ngrid%i_parent_start   ! used in feedback_domain_em_part2 below
      jps_save = ngrid%j_parent_start
      ipe_save = ngrid%i_parent_start + (nide-nids+1) / ngrid%parent_grid_ratio - 1
      jpe_save = ngrid%j_parent_start + (njde-njds+1) / ngrid%parent_grid_ratio - 1

! feedback_nest_prep invokes a halo exchange on the ngrid. It is done this way
! in a separate routine because the HALOs need the data to be dereference from the
! grid data structure and, in this routine, the dereferenced fields are related to
! the intermediate domain, not the nest itself. Save the current grid pointer to intermediate
! domain, switch grid to point to ngrid, invoke feedback_nest_prep,  then restore grid
! to point to intermediate domain.

      CALL model_to_grid_config_rec ( ngrid%id , model_config_rec , nconfig_flags )
      CALL set_scalar_indices_from_config ( ngrid%id , idum1 , idum2 )
      xgrid => grid
      grid => ngrid

      CALL feedback_nest_prep ( grid, nconfig_flags    &
!
#include "actual_new_args.inc"
!
)

! put things back so grid is intermediate grid

      grid => xgrid
      CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

! "interp" (basically copy) ngrid onto intermediate grid

#include "nest_feedbackup_interp.inc"

      RETURN
   END SUBROUTINE feedback_domain_em_part1

!------------------------------------------------------------------

   SUBROUTINE feedback_domain_em_part2 ( grid, intermediate_grid, ngrid , config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, domain_clock_get, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type, model_config_rec
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
                            ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width,  &
                            nest_pes_x, nest_pes_y,                                         &
                            intercomm_active, nest_task_offsets,                    &
                            mpi_comm_to_mom, mpi_comm_to_kid, which_kid !,            &
                             !push_communicators_for_domain, pop_communicators_for_domain

      USE module_comm_nesting_dm, ONLY : halo_interp_up_sub
      USE module_utility
      IMPLICIT NONE

!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid
      TYPE(domain), POINTER :: parent_grid

#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          xids, xide, xjds, xjde, xkds, xkde,    &
                                xims, xime, xjms, xjme, xkms, xkme,    &
                                xips, xipe, xjps, xjpe, xkps, xkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER icoord, jcoord, idim_cd, jdim_cd
      INTEGER local_comm, myproc, nproc, ioffset
      INTEGER iparstrt, jparstrt, sw, thisdomain_max_halo_width
      REAL    nest_influence

      character*256 :: timestr
      integer ierr

      LOGICAL, EXTERNAL  :: cd_feedback_mask

!cyl: add variables for trajectory
      integer tjk

! On entry to this routine,
!  "grid" refers to the parent domain
!  "intermediate_grid" refers to local copy of parent domain that overlies this patch of nest
!  "ngrid" refers to the nest, which is only needed for smoothing on the parent because
!          the nest feedback data has already been transferred during em_nest_feedbackup_interp
!          in part1, above.
! The way these settings c and n dimensions are set, below, looks backwards but from the point
! of view of the RSL routine rsl_lite_to_parent_info(), call to which is included by
! em_nest_feedbackup_pack, the "n" domain represents the parent domain and the "c" domain
! represents the intermediate domain. The backwards lookingness should be fixed in the gen_comms.c
! registry routine that accompanies RSL_LITE but, just as it's sometimes easier to put up a road
! sign that says "DIP" than fix the dip,  at this point it was easier just to write this comment. JM
!
      nest_influence = 1.

      CALL domain_clock_get( grid, current_timestr=timestr )

      CALL get_ijk_from_grid (  intermediate_grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  grid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                xids, xide, xjds, xjde, xkds, xkde,    &
                                xims, xime, xjms, xjme, xkms, xkme,    &
                                xips, xipe, xjps, xjpe, xkps, xkpe    )

      ips_save = ngrid%i_parent_start   ! used in feedback_domain_em_part2 below
      jps_save = ngrid%j_parent_start
      ipe_save = ngrid%i_parent_start + (xide-xids+1) / ngrid%parent_grid_ratio - 1
      jpe_save = ngrid%j_parent_start + (xjde-xjds+1) / ngrid%parent_grid_ratio - 1




IF ( ngrid%active_this_task ) THEN
!cyl add this for trajectory
    CALL push_communicators_for_domain( ngrid%id )

    do tjk = 1,config_flags%num_traj
     if (ngrid%traj_long(tjk) .eq. -9999.0) then
!       print*,'n=-9999',tjk
        ngrid%traj_long(tjk)=grid%traj_long(tjk)
        ngrid%traj_k(tjk)=grid%traj_k(tjk)
     else
!       print*,'p!=-9999',tjk
        grid%traj_long(tjk)=ngrid%traj_long(tjk)
        grid%traj_k(tjk)=ngrid%traj_k(tjk)
     endif
     if (ngrid%traj_lat(tjk) .eq. -9999.0) then
         ngrid%traj_lat(tjk)=grid%traj_lat(tjk)
         ngrid%traj_k(tjk)=grid%traj_k(tjk)
     else
         grid%traj_lat(tjk)=ngrid%traj_lat(tjk)
         grid%traj_k(tjk)=ngrid%traj_k(tjk)
     endif
    enddo
!endcyl

      CALL nl_get_i_parent_start ( intermediate_grid%id, iparstrt )
      CALL nl_get_j_parent_start ( intermediate_grid%id, jparstrt )
      CALL nl_get_shw            ( intermediate_grid%id, sw )
      icoord =    iparstrt - sw
      jcoord =    jparstrt - sw
      idim_cd = cide - cids + 1
      jdim_cd = cjde - cjds + 1

      nlev  = ckde - ckds + 1

      CALL get_dm_max_halo_width ( grid%id , thisdomain_max_halo_width )

      parent_grid => grid
      grid => ngrid
#include "nest_feedbackup_pack.inc"
      grid => parent_grid
    CALL pop_communicators_for_domain

END IF

!      CALL wrf_get_dm_communicator ( local_comm )
!      CALL wrf_get_myproc( myproc )
!      CALL wrf_get_nproc( nproc )

      ! determine which communicator and offset to use
      IF ( intercomm_active( grid%id ) ) THEN        ! I am parent
        local_comm = mpi_comm_to_kid( which_kid(ngrid%id), grid%id )
        ioffset = nest_task_offsets(ngrid%id)
      ELSE IF ( intercomm_active( ngrid%id ) ) THEN  ! I am nest
        local_comm = mpi_comm_to_mom( ngrid%id )
        ioffset = nest_task_offsets(ngrid%id)
      END IF

      IF ( grid%active_this_task .OR. ngrid%active_this_task ) THEN
#ifndef STUBMPI
        CALL mpi_comm_rank(local_comm,myproc,ierr)
        CALL mpi_comm_size(local_comm,nproc,ierr)
#endif
!call tracebackqq()
        CALL rsl_lite_merge_msgs( myproc, nest_pes_x(grid%id)*nest_pes_y(grid%id),         &
                                          nest_pes_x(ngrid%id)*nest_pes_y(ngrid%id),       &
                                          ioffset, local_comm )
      END IF

IF ( grid%active_this_task ) THEN
    CALL push_communicators_for_domain( grid%id )


#define NEST_INFLUENCE(A,B) A = B
#include "nest_feedbackup_unpack.inc"

      ! smooth coarse grid
      CALL get_ijk_from_grid (  ngrid,                           &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )
      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#include "HALO_INTERP_UP.inc"

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )

#include "nest_feedbackup_smooth.inc"

    CALL pop_communicators_for_domain
END IF

      RETURN
   END SUBROUTINE feedback_domain_em_part2
#endif

#if ( NMM_CORE == 1 && NMM_NEST == 1 )
!==============================================================================
! NMM nesting infrastructure extended from EM core. This is gopal's doing.
!==============================================================================

   SUBROUTINE before_interp_halos_nmm(grid,config_flags &
!
#include "dummy_new_args.inc"
!
      )
     ! This is called before interp_domain_nmm_part1 to do
     ! pre-interpolation halo communication on the nest.
     ! Author: Sam Trahan, February 2011
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
                            ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width,  &
                            nest_pes_x, nest_pes_y !,                                         &
                             !push_communicators_for_domain, pop_communicators_for_domain
      USE module_comm_dm, ONLY : HALO_NMM_WEIGHTS_sub

      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE (grid_config_rec_type)            :: config_flags
#include "dummy_new_decl.inc"
     INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE, &
                IMS,IME,JMS,JME,KMS,KME, &
                IPS,IPE,JPS,JPE,KPS,KPE

!#ifdef DEREF_KLUDGE
!!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
!   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
!   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
!   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
!#endif
#include "deref_kludge.h"

!#define COPY_IN

     ! FIXME: Don't initialize these to -1; it is a waste.
     ! Initialization is only for debugging purposes.
      IDS=-1; IDE=-1; JDS=-1; JDE=-1; KDS=-1; KDE=-1
      IMS=-1; IME=-1; JMS=-1; JME=-1; KMS=-1; KME=-1
      IPS=-1; IPE=-1; JPS=-1; JPE=-1; KPS=-1; KPE=-1
      CALL GET_IJK_FROM_GRID(GRID                                       &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                      ,IMS,IME,JMS,JME,KMS,KME                    &
     &                      ,IPS,IPE,JPS,JPE,KPS,KPE )

     CALL push_communicators_for_domain(grid%id)
#include "HALO_NMM_WEIGHTS.inc"
     CALL pop_communicators_for_domain

   END SUBROUTINE before_interp_halos_nmm

   SUBROUTINE interp_domain_nmm_part1 ( grid, intermediate_grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
                            ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width,  &
                            nest_pes_x, nest_pes_y
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, local_communicator, mytask, &
                            nest_pes_x, nest_pes_y,                                 &
                            intercomm_active, nest_task_offsets,                    &
                            mpi_comm_to_mom, mpi_comm_to_kid, which_kid !,            &
                             !push_communicators_for_domain,pop_communicators_for_domain

      USE module_timing
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      INTEGER iparstrt,jparstrt,sw
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7
      LOGICAL feedback_flag, feedback_flag_v
      INTEGER icoord, jcoord, idim_cd, jdim_cd, pgr
      INTEGER local_comm, ioffset, myproc, nproc, ierr
      INTEGER thisdomain_max_halo_width

      LOGICAL interp_mp
      interp_mp=grid%interp_mp .or. ngrid%interp_mp

      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_myproc( myproc )
      CALL wrf_get_nproc( nproc )

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  intermediate_grid ,              &
                                iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      CALL nl_get_parent_grid_ratio ( ngrid%id, pgr )
      CALL nl_get_i_parent_start ( intermediate_grid%id, iparstrt )
      CALL nl_get_j_parent_start ( intermediate_grid%id, jparstrt )
      CALL nl_get_shw            ( intermediate_grid%id, sw )
      icoord =    iparstrt - sw
      jcoord =    jparstrt - sw
      idim_cd = iide - iids + 1
      jdim_cd = ijde - ijds + 1

      nlev  = ckde - ckds + 1

      ! get max_halo_width for parent. It may be smaller if it is moad
      CALL get_dm_max_halo_width ( ngrid%id , thisdomain_max_halo_width )

      IF ( grid%active_this_task ) THEN
#include "nest_interpdown_pack.inc"
      END IF

      ! determine which communicator and offset to use
      IF ( intercomm_active( grid%id ) ) THEN        ! I am parent
        local_comm = mpi_comm_to_kid( which_kid(ngrid%id), grid%id )
        ioffset = nest_task_offsets(ngrid%id)
      ELSE IF ( intercomm_active( ngrid%id ) ) THEN  ! I am nest
        local_comm = mpi_comm_to_mom( ngrid%id )
        ioffset = nest_task_offsets(ngrid%id)
      END IF

      IF ( grid%active_this_task .OR. ngrid%active_this_task ) THEN
#ifndef STUBMPI
        CALL mpi_comm_rank(local_comm,myproc,ierr)
        CALL mpi_comm_size(local_comm,nproc,ierr)
#endif
!CALL tracebackqq()
        CALL rsl_lite_bcast_msgs( myproc, nest_pes_x(grid%id)*nest_pes_y(grid%id),         &
                                        nest_pes_x(ngrid%id)*nest_pes_y(ngrid%id),       &
                                        ioffset, local_comm )
      END IF

      RETURN
   END SUBROUTINE interp_domain_nmm_part1

!------------------------------------------------------------------

   SUBROUTINE interp_domain_nmm_part2 ( grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
                            ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width ,  &
                            nest_task_offsets
                             !push_communicators_for_domain,pop_communicators_for_domain,     &
      USE module_comm_nesting_dm, ONLY : halo_interp_down_sub
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7
      LOGICAL feedback_flag, feedback_flag_v
      INTEGER myproc
      INTEGER ierr

      integer, parameter :: EConst=0, ECopy=1, EExtrap=2 ! MUST match module_interp_nmm
      LOGICAL interp_mp
#include "deref_kludge.h"

! interp_mp is set unconditionally in alloc_and_configure_domain (module_domain.F),
! regardless of active_this_task
      interp_mp=grid%interp_mp .or. ngrid%interp_mp

      IF ( ngrid%active_this_task ) THEN

        CALL get_ijk_from_grid (  grid ,                   &
                                  cids, cide, cjds, cjde, ckds, ckde,    &
                                  cims, cime, cjms, cjme, ckms, ckme,    &
                                  cips, cipe, cjps, cjpe, ckps, ckpe    )
        CALL get_ijk_from_grid (  ngrid ,              &
                                  nids, nide, njds, njde, nkds, nkde,    &
                                  nims, nime, njms, njme, nkms, nkme,    &
                                  nips, nipe, njps, njpe, nkps, nkpe    )


        nlev  = ckde - ckds + 1

#include "nest_interpdown_unpack.inc"

        CALL get_ijk_from_grid (  grid ,              &
                                  ids, ide, jds, jde, kds, kde,    &
                                  ims, ime, jms, jme, kms, kme,    &
                                  ips, ipe, jps, jpe, kps, kpe    )

        CALL push_communicators_for_domain( grid%id )
#include "HALO_INTERP_DOWN.inc"

      ! Generate interpolation information and interpolate Q, T and
      ! possibly PD while we're at it:

! Grid is set to ngrid%intermediate_grid in the call from med_interp_domain
! (share/mediation_interp_domain.F) so if one is active_this_task, so is the other
        call store_interp_info(ngrid,grid)
        call ext_c2n_fulldom(ngrid%IIH,ngrid%JJH,ngrid%HBWGT1, &
             ngrid%HBWGT2,ngrid%HBWGT3,ngrid%HBWGT4,         &
             ngrid%deta1,ngrid%deta2,ngrid%eta1,             &
             ngrid%eta2,ngrid%pt,ngrid%pdtop,                &
             grid%pint,grid%t,grid%pd,grid%q,       &
             cims, cime, cjms, cjme, ckms, ckme,             &
             ngrid%pint,ngrid%t,ngrid%pd,ngrid%q,&
             ngrid%iinfo,ngrid%winfo,ngrid%imask_nostag, &
             nids, nide, njds, njde, nkds, nkde,             &
             nims, nime, njms, njme, nkms, nkme,             &
             nips, nipe, njps, njpe, nkps, nkpe)

#include "nest_interpdown_interp.inc"


        CALL pop_communicators_for_domain

      END IF

      RETURN
   END SUBROUTINE interp_domain_nmm_part2

!------------------------------------------------------------------

   SUBROUTINE force_domain_nmm_part1 ( grid, intermediate_grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
                            ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width,  &
                            nest_pes_x, nest_pes_y,                                 &
                            intercomm_active, nest_task_offsets,                    &
                            mpi_comm_to_mom, mpi_comm_to_kid, which_kid !,            &
                            !push_communicators_for_domain,pop_communicators_for_domain

      USE module_timing
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      INTEGER iparstrt,jparstrt,sw
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7
      LOGICAL feedback_flag, feedback_flag_v
      INTEGER icoord, jcoord, idim_cd, jdim_cd, pgr
      INTEGER local_comm, ioffset, myproc, nproc, ierr
      INTEGER thisdomain_max_halo_width
      LOGICAL interp_mp

      interp_mp=grid%interp_mp .or. ngrid%interp_mp

      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_myproc( myproc )
      CALL wrf_get_nproc( nproc )

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  intermediate_grid ,              &
                                iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      CALL nl_get_parent_grid_ratio ( ngrid%id, pgr )
      CALL nl_get_i_parent_start ( intermediate_grid%id, iparstrt )
      CALL nl_get_j_parent_start ( intermediate_grid%id, jparstrt )
      CALL nl_get_shw            ( intermediate_grid%id, sw )
      icoord =    iparstrt - sw
      jcoord =    jparstrt - sw
      idim_cd = iide - iids + 1
      jdim_cd = ijde - ijds + 1

      nlev  = ckde - ckds + 1

      CALL get_dm_max_halo_width ( ngrid%id , thisdomain_max_halo_width )

      IF ( grid%active_this_task ) THEN
#include "nest_forcedown_pack.inc"
      END IF

      ! determine which communicator and offset to use
      IF ( intercomm_active( grid%id ) ) THEN        ! I am parent
        local_comm = mpi_comm_to_kid( which_kid(ngrid%id), grid%id )
        ioffset = nest_task_offsets(ngrid%id)
      ELSE IF ( intercomm_active( ngrid%id ) ) THEN  ! I am nest
        local_comm = mpi_comm_to_mom( ngrid%id )
        ioffset = nest_task_offsets(ngrid%id)
      END IF

      IF ( grid%active_this_task .OR. ngrid%active_this_task ) THEN
#ifndef STUBMPI
        CALL mpi_comm_rank(local_comm,myproc,ierr)
        CALL mpi_comm_size(local_comm,nproc,ierr)
#endif
        CALL rsl_lite_bcast_msgs( myproc, nest_pes_x(grid%id)*nest_pes_y(grid%id),         &
                                        nest_pes_x(ngrid%id)*nest_pes_y(ngrid%id),       &
                                        ioffset, local_comm )
      END IF

      RETURN
      END SUBROUTINE force_domain_nmm_part1

!==============================================================================================

   SUBROUTINE force_domain_nmm_part2 ( grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
                            ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width,  &
                            nest_pes_x, nest_pes_y !,                                         &
                             !push_communicators_for_domain,pop_communicators_for_domain

#if (NMM_NEST == 1)
      USE module_comm_nesting_dm, ONLY : halo_force_down_sub
      use module_comm_dm, only: HALO_NMM_INTERP_INFO_sub
# if ( HWRF == 1 )
      use module_comm_dm, only: HALO_NMM_FORCE_DOWN_SST_sub
# endif
#endif
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid,cgrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7
      REAL  dummy_xs, dummy_xe, dummy_ys, dummy_ye
      LOGICAL feedback_flag, feedback_flag_v

      LOGICAL interp_mp
      integer, parameter :: EConst=0, ECopy=1, EExtrap=2 ! MUST match module_interp_nmm

#include "deref_kludge.h"
      interp_mp=grid%interp_mp .or. ngrid%interp_mp

IF ( ngrid%active_this_task ) THEN
      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

!jm as far as I can tell, grid is ngrid%intermediate_domain, so they
!jm should both have the same id, both be active_this_task (if one is)
!jm and use the same communicator.  But just to be safe, some extra
!jm pushes and pops of domain communicators littered here.

      cgrid=>grid
      nlev  = ckde - ckds + 1

#include "nest_forcedown_unpack.inc"

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

      CALL push_communicators_for_domain( grid%id )

#if ( HWRF == 1 )
        IF(ngrid%force_sst(1) == 1) then
# include "HALO_NMM_FORCE_DOWN_SST.inc"
        END IF
#endif

#include "HALO_FORCE_DOWN.inc"
      CALL pop_communicators_for_domain

      call store_interp_info(ngrid,grid)


      call ext_c2b_fulldom(ngrid%IIH,ngrid%JJH,ngrid%HBWGT1, &
           ngrid%HBWGT2,ngrid%HBWGT3,ngrid%HBWGT4,         &
           ngrid%deta1,ngrid%deta2,ngrid%eta1,             &
           ngrid%eta2,ngrid%pt,ngrid%pdtop,                &
           grid%pint,grid%t,grid%pd,grid%q,                &
           cims, cime, cjms, cjme, ckms, ckme,             &
           nids, nide, njds, njde, nkds, nkde,             &
           nims, nime, njms, njme, nkms, nkme,             &
           nips, nipe, njps, njpe, nkps, nkpe,             &
           ngrid%iinfo_bxs, ngrid%iinfo_bxe,               &
           ngrid%iinfo_bys, ngrid%iinfo_bye,               &
           ngrid%winfo_bxs, ngrid%winfo_bxe,               &
           ngrid%winfo_bys, ngrid%winfo_bye,               &
           ngrid%pd_bxs, ngrid%pd_bxe,             &
           ngrid%pd_bys, ngrid%pd_bye,             &
           ngrid%t_bxs, ngrid%t_bxe,               &
           ngrid%t_bys, ngrid%t_bye,               &
           ngrid%q_bxs, ngrid%q_bxe,               &
           ngrid%q_bys, ngrid%q_bye)

      ! Need a halo for interpolation information due to how V grid
      ! interpolation works:
      grid=>ngrid
      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )
      CALL push_communicators_for_domain( grid%id )
#include "HALO_NMM_INTERP_INFO.inc"
      CALL pop_communicators_for_domain

      grid=>cgrid
      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )


      ! code here to interpolate the data into the nested domain
      CALL push_communicators_for_domain( grid%id )
#include "nest_forcedown_interp.inc"
      CALL pop_communicators_for_domain

END IF

      RETURN
   END SUBROUTINE force_domain_nmm_part2

!================================================================================
!
! This routine exists only to call a halo on a domain (the nest)
! gets called from feedback_domain_em_part1, below.  This is needed
! because the halo code expects the fields being exchanged to have
! been dereferenced from the grid data structure, but in feedback_domain_em_part1
! the grid data structure points to the coarse domain, not the nest.
! And we want the halo exchange on the nest, so that the code in
! em_nest_feedbackup_interp.inc will work correctly on multi-p. JM 20040308
!

   SUBROUTINE feedback_nest_prep_nmm ( grid, config_flags    &
!
#include "dummy_new_args.inc"
!
)
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
                            ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width,  &
                            nest_pes_x, nest_pes_y
                            !push_communicators_for_domain, pop_communicators_for_domain,    &
      USE module_comm_dm, ONLY : HALO_NMM_WEIGHTS_sub
      USE module_comm_nesting_dm, ONLY : HALO_INTERP_UP_sub

      IMPLICIT NONE
!
      TYPE(domain), TARGET :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE (grid_config_rec_type) :: config_flags ! configureation flags, has vertical dim of
                                                  ! soil temp, moisture, etc., has vertical dim
                                                  ! of soil categories
#include "dummy_new_decl.inc"

      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER       :: idum1, idum2
      LOGICAL :: interp_mp
      interp_mp=.true.

#include "deref_kludge.h"

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )
    IF ( grid%active_this_task ) THEN
      CALL push_communicators_for_domain( grid%id )
#ifdef DM_PARALLEL
#include "HALO_INTERP_UP.inc"
#include "HALO_NMM_WEIGHTS.inc"
#endif
      CALL pop_communicators_for_domain
    END IF

   END SUBROUTINE feedback_nest_prep_nmm

!------------------------------------------------------------------


!==============================================================================================

   SUBROUTINE force_intermediate_nmm ( grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
                            ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width
#if ( NMM_NEST == 1 )
      USE module_comm_nesting_dm, ONLY : halo_force_down_sub
#endif
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: cgrid
      TYPE(domain), POINTER :: ngrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7
      REAL  dummy_xs, dummy_xe, dummy_ys, dummy_ye
      LOGICAL feedback_flag, feedback_flag_v

integer myproc
      LOGICAL interp_mp

!#ifdef DEREF_KLUDGE
!!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
!   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
!   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
!   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
!#endif
#include "deref_kludge.h"

      interp_mp=grid%interp_mp .or. ngrid%interp_mp

!#define COPY_IN
      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      cgrid=>grid
      nlev  = ckde - ckds + 1

#include "nest_interpdown_unpack.inc"

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#include "HALO_FORCE_DOWN.inc"

      RETURN
    END SUBROUTINE force_intermediate_nmm

! ----------------------------------------------------------------------

   SUBROUTINE feedback_domain_nmm_part1 ( grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
                            ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width,  &
                            nest_pes_x, nest_pes_y
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize, i_parent_start, j_parent_start
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE(domain), POINTER :: xgrid
      TYPE (grid_config_rec_type)            :: config_flags, nconfig_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER local_comm, myproc, nproc, idum1, idum2

      integer, parameter :: EConst=0, ECopy=1, EExtrap=2 ! MUST match module_interp_nmm

      LOGICAL interp_mp

      INTERFACE
          SUBROUTINE feedback_nest_prep_nmm ( grid, config_flags    &
!
#include "dummy_new_args.inc"
!
)
             USE module_state_description
             USE module_domain, ONLY : domain
             USE module_configure, ONLY : grid_config_rec_type
!
             TYPE (grid_config_rec_type)            :: config_flags
             TYPE(domain), TARGET                   :: grid
#include "dummy_new_decl.inc"
          END SUBROUTINE feedback_nest_prep_nmm
      END INTERFACE
!

      interp_mp=grid%interp_mp .or. ngrid%interp_mp
      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_myproc( myproc )
      CALL wrf_get_nproc( nproc )


!
! intermediate grid
      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
! nest grid
      CALL get_ijk_from_grid (  ngrid ,                  &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

      ips_save = ngrid%i_parent_start  ! +1 not used in ipe_save & jpe_save
      jps_save = ngrid%j_parent_start  !  because of one extra namelist point
      ipe_save = ngrid%i_parent_start + (nide-nids) / ngrid%parent_grid_ratio - 1
      jpe_save = ngrid%j_parent_start + (njde-njds) / ngrid%parent_grid_ratio - 1

! feedback_nest_prep invokes a halo exchange on the ngrid. It is done this way
! in a separate routine because the HALOs need the data to be dereference from the
! grid data structure and, in this routine, the dereferenced fields are related to
! the intermediate domain, not the nest itself. Save the current grid pointer to intermediate
! domain, switch grid to point to ngrid, invoke feedback_nest_prep,  then restore grid
! to point to intermediate domain.

      CALL model_to_grid_config_rec ( ngrid%id , model_config_rec , nconfig_flags )
      CALL set_scalar_indices_from_config ( ngrid%id , idum1 , idum2 )
      xgrid => grid
      grid => ngrid
#include "deref_kludge.h"
      CALL feedback_nest_prep_nmm ( grid, config_flags    &
!
#include "actual_new_args.inc"
!
)

! put things back so grid is intermediate grid

      grid => xgrid
      CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

! "interp" (basically copy) ngrid onto intermediate grid

      ! Generate interpolation information and interpolate Q, T and
      ! possibly PD while we're at it:

      call store_interp_info(ngrid,grid)
      call ext_n2c_fulldom(&
           ngrid%deta1,ngrid%deta2,ngrid%eta1,             &
           ngrid%eta2,ngrid%pt,ngrid%pdtop,                &
           grid%pint,grid%t,grid%pd,grid%q,                &
           cids, cide, cjds, cjde, ckds, ckde,             &
           cims, cime, cjms, cjme, ckms, ckme,             &
           cips, cipe, cjps, cjpe, ckps, ckpe,             &
           ngrid%pint,ngrid%t,                             &
           ngrid%pd,ngrid%q,                               &
           ngrid%i_parent_start, ngrid%j_parent_start,     &
           grid%iinfo,grid%winfo,                          &
           nids, nide, njds, njde, nkds, nkde,             &
           nims, nime, njms, njme, nkms, nkme,             &
           nips, nipe, njps, njpe, nkps, nkpe)


      ! "interp" ngrid onto intermediate grid
#include "nest_feedbackup_interp.inc"

      RETURN
   END SUBROUTINE feedback_domain_nmm_part1

!------------------------------------------------------------------

   SUBROUTINE feedback_domain_nmm_part2 ( grid, intermediate_grid, ngrid , config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, domain_clock_get, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : get_dm_max_halo_width, ips_save, ipe_save,              &
                            jps_save, jpe_save, ntasks, mytask, ntasks_x, ntasks_y, &
                            local_communicator, itrace,                             &
                            nest_pes_x, nest_pes_y,                                 &
                            intercomm_active, nest_task_offsets,                    &
                            mpi_comm_to_mom, mpi_comm_to_kid, which_kid !  ,            &
                            ! push_communicators_for_domain, pop_communicators_for_domain

      USE module_comm_nesting_dm, ONLY : halo_interp_up_sub
      USE module_utility
      IMPLICIT NONE

!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid
      TYPE(domain), POINTER :: parent_grid

#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          xids, xide, xjds, xjde, xkds, xkde,    &
                                xims, xime, xjms, xjme, xkms, xkme,    &
                                xips, xipe, xjps, xjpe, xkps, xkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER icoord, jcoord, idim_cd, jdim_cd
      INTEGER local_comm, myproc, nproc
      INTEGER iparstrt, jparstrt, sw
      INTEGER thisdomain_max_halo_width

      character*256 :: timestr
      integer ioffset, ierr

      REAL    nest_influence
      LOGICAL feedback_flag, feedback_flag_v
      LOGICAL, EXTERNAL  :: cd_feedback_mask
      LOGICAL, EXTERNAL  :: cd_feedback_mask_v

      LOGICAL interp_mp

! On entry to this routine,
!  "grid" refers to the parent domain
!  "intermediate_grid" refers to local copy of parent domain that overlies this patch of nest
!  "ngrid" refers to the nest, which is only needed for smoothing on the parent because
!          the nest feedback data has already been transferred during em_nest_feedbackup_interp
!          in part1, above.
! The way these settings c and n dimensions are set, below, looks backwards but from the point
! of view of the RSL routine rsl_lite_to_parent_info(), call to which is included by
! em_nest_feedbackup_pack, the "n" domain represents the parent domain and the "c" domain
! represents the intermediate domain. The backwards lookingness should be fixed in the gen_comms.c
! registry routine that accompanies RSL_LITE but, just as it's sometimes easier to put up a road
! sign that says "DIP" than fix the dip,  at this point it was easier just to write this comment. JM
!

      interp_mp=grid%interp_mp .or. ngrid%interp_mp
      nest_influence = 0.5
#define NEST_INFLUENCE(A,B) A = nest_influence*(B) + (1.0-nest_influence)*(A)


      CALL domain_clock_get( grid, current_timestr=timestr )

      CALL get_ijk_from_grid (  intermediate_grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  grid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                xids, xide, xjds, xjde, xkds, xkde,    &
                                xims, xime, xjms, xjme, xkms, xkme,    &
                                xips, xipe, xjps, xjpe, xkps, xkpe    )

      ips_save = ngrid%i_parent_start
      jps_save = ngrid%j_parent_start
      ipe_save = ngrid%i_parent_start + (xide-xids) / ngrid%parent_grid_ratio - 1
      jpe_save = ngrid%j_parent_start + (xjde-xjds) / ngrid%parent_grid_ratio - 1


      nide = nide - 1   !dusan
      njde = njde - 1   !dusan

IF ( ngrid%active_this_task ) THEN
    CALL push_communicators_for_domain( ngrid%id )
      CALL nl_get_i_parent_start ( intermediate_grid%id, iparstrt )
      CALL nl_get_j_parent_start ( intermediate_grid%id, jparstrt )
      CALL nl_get_shw            ( intermediate_grid%id, sw )
      icoord =    iparstrt  - sw
      jcoord =    jparstrt  - sw
      idim_cd = cide - cids + 1
      jdim_cd = cjde - cjds + 1

      nlev  = ckde - ckds + 1

      CALL get_dm_max_halo_width ( ngrid%id , thisdomain_max_halo_width )
      parent_grid => grid
      grid => ngrid
#include "nest_feedbackup_pack.inc"
      grid => parent_grid
    CALL pop_communicators_for_domain
END IF

!      CALL wrf_get_dm_communicator ( local_comm )
!      CALL wrf_get_myproc( myproc )
!      CALL wrf_get_nproc( nproc )

      ! determine which communicator and offset to use
      IF ( intercomm_active( grid%id ) ) THEN        ! I am parent
        local_comm = mpi_comm_to_kid( which_kid(ngrid%id), grid%id )
        ioffset = nest_task_offsets(ngrid%id)
      ELSE IF ( intercomm_active( ngrid%id ) ) THEN  ! I am nest
        local_comm = mpi_comm_to_mom( ngrid%id )
        ioffset = nest_task_offsets(ngrid%id)
      END IF

      IF ( grid%active_this_task .OR. ngrid%active_this_task ) THEN
#ifndef STUBMPI
        CALL mpi_comm_rank(local_comm,myproc,ierr)
        CALL mpi_comm_size(local_comm,nproc,ierr)
#endif
        CALL rsl_lite_merge_msgs( myproc, nest_pes_x(grid%id)*nest_pes_y(grid%id),         &
                                          nest_pes_x(ngrid%id)*nest_pes_y(ngrid%id),       &
                                          ioffset, local_comm )
      END IF

IF ( grid%active_this_task ) THEN
    CALL push_communicators_for_domain( grid%id )

#include "nest_feedbackup_unpack.inc"


      ! smooth coarse grid

      CALL get_ijk_from_grid (  ngrid,                                 &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe     )
      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

     before_smooth_halo: if(config_flags%smooth_option/=0) then
#include "HALO_INTERP_UP.inc"
     endif before_smooth_halo

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )

     smoother: if(config_flags%smooth_option/=0) then
#include "nest_feedbackup_smooth.inc"
     endif smoother

    CALL pop_communicators_for_domain
END IF


      RETURN
   END SUBROUTINE feedback_domain_nmm_part2

!=================================================================================
!   End of gopal's doing
!=================================================================================
#endif

!------------------------------------------------------------------

   SUBROUTINE wrf_gatherv_real (Field, field_ofst,            &
                                my_count ,                    &    ! sendcount
                                globbuf, glob_ofst ,          &    ! recvbuf
                                counts                      , &    ! recvcounts
                                displs                      , &    ! displs
                                root                        , &    ! root
                                communicator                , &    ! communicator
                                ierr )
   USE module_dm, ONLY : getrealmpitype
   IMPLICIT NONE
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
   REAL, DIMENSION(*) :: Field, globbuf
#ifndef STUBMPI
   INCLUDE 'mpif.h'

           CALL mpi_gatherv( Field( field_ofst ),      &    ! sendbuf
                            my_count ,                       &    ! sendcount
                            getrealmpitype() ,               &    ! sendtype
                            globbuf( glob_ofst ) ,                 &    ! recvbuf
                            counts                         , &    ! recvcounts
                            displs                         , &    ! displs
                            getrealmpitype()               , &    ! recvtype
                            root                           , &    ! root
                            communicator                   , &    ! communicator
                            ierr )
#endif

   END SUBROUTINE wrf_gatherv_real

   SUBROUTINE wrf_gatherv_double (Field, field_ofst,            &
                                my_count ,                    &    ! sendcount
                                globbuf, glob_ofst ,          &    ! recvbuf
                                counts                      , &    ! recvcounts
                                displs                      , &    ! displs
                                root                        , &    ! root
                                communicator                , &    ! communicator
                                ierr )
!   USE module_dm
   IMPLICIT NONE
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
! this next declaration is REAL, not DOUBLE PRECISION because it will be autopromoted
! to double precision by the compiler when WRF is compiled for 8 byte reals. Only reason
! for having this separate routine is so we pass the correct MPI type to mpi_scatterv
! if we were not indexing the globbuf and Field arrays it would not even matter
   REAL, DIMENSION(*) :: Field, globbuf
#ifndef STUBMPI
   INCLUDE 'mpif.h'

           CALL mpi_gatherv( Field( field_ofst ),      &    ! sendbuf
                            my_count ,                       &    ! sendcount
                            MPI_DOUBLE_PRECISION         ,               &    ! sendtype
                            globbuf( glob_ofst ) ,                 &    ! recvbuf
                            counts                         , &    ! recvcounts
                            displs                         , &    ! displs
                            MPI_DOUBLE_PRECISION                       , &    ! recvtype
                            root                           , &    ! root
                            communicator                   , &    ! communicator
                            ierr )
#endif

   END SUBROUTINE wrf_gatherv_double

   SUBROUTINE wrf_gatherv_integer (Field, field_ofst,            &
                                my_count ,                    &    ! sendcount
                                globbuf, glob_ofst ,          &    ! recvbuf
                                counts                      , &    ! recvcounts
                                displs                      , &    ! displs
                                root                        , &    ! root
                                communicator                , &    ! communicator
                                ierr )
   IMPLICIT NONE
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
   INTEGER, DIMENSION(*) :: Field, globbuf
#ifndef STUBMPI
   INCLUDE 'mpif.h'

           CALL mpi_gatherv( Field( field_ofst ),      &    ! sendbuf
                            my_count ,                       &    ! sendcount
                            MPI_INTEGER         ,               &    ! sendtype
                            globbuf( glob_ofst ) ,                 &    ! recvbuf
                            counts                         , &    ! recvcounts
                            displs                         , &    ! displs
                            MPI_INTEGER                       , &    ! recvtype
                            root                           , &    ! root
                            communicator                   , &    ! communicator
                            ierr )
#endif

   END SUBROUTINE wrf_gatherv_integer

!new stuff 20070124
   SUBROUTINE wrf_scatterv_real (                             &
                                globbuf, glob_ofst ,          &    ! recvbuf
                                counts                      , &    ! recvcounts
                                Field, field_ofst,            &
                                my_count ,                    &    ! sendcount
                                displs                      , &    ! displs
                                root                        , &    ! root
                                communicator                , &    ! communicator
                                ierr )
   USE module_dm, ONLY : getrealmpitype
   IMPLICIT NONE
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
   REAL, DIMENSION(*) :: Field, globbuf
#ifndef STUBMPI
   INCLUDE 'mpif.h'

           CALL mpi_scatterv(                                &
                            globbuf( glob_ofst ) ,           &    ! recvbuf
                            counts                         , &    ! recvcounts
                            displs                         , &    ! displs
                            getrealmpitype()               , &    ! recvtype
                            Field( field_ofst ),             &    ! sendbuf
                            my_count ,                       &    ! sendcount
                            getrealmpitype() ,               &    ! sendtype
                            root                           , &    ! root
                            communicator                   , &    ! communicator
                            ierr )
#endif

   END SUBROUTINE wrf_scatterv_real

   SUBROUTINE wrf_scatterv_double (                           &
                                globbuf, glob_ofst ,          &    ! recvbuf
                                counts                      , &    ! recvcounts
                                Field, field_ofst,            &
                                my_count ,                    &    ! sendcount
                                displs                      , &    ! displs
                                root                        , &    ! root
                                communicator                , &    ! communicator
                                ierr )
   IMPLICIT NONE
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
   REAL, DIMENSION(*) :: Field, globbuf
#ifndef STUBMPI
   INCLUDE 'mpif.h'
! this next declaration is REAL, not DOUBLE PRECISION because it will be autopromoted
! to double precision by the compiler when WRF is compiled for 8 byte reals. Only reason
! for having this separate routine is so we pass the correct MPI type to mpi_scatterv
! if we were not indexing the globbuf and Field arrays it would not even matter

           CALL mpi_scatterv(                                &
                            globbuf( glob_ofst ) ,           &    ! recvbuf
                            counts                         , &    ! recvcounts
                            displs                         , &    ! displs
                            MPI_DOUBLE_PRECISION           , &    ! recvtype
                            Field( field_ofst ),             &    ! sendbuf
                            my_count ,                       &    ! sendcount
                            MPI_DOUBLE_PRECISION         ,   &    ! sendtype
                            root                           , &    ! root
                            communicator                   , &    ! communicator
                            ierr )
#endif

   END SUBROUTINE wrf_scatterv_double

   SUBROUTINE wrf_scatterv_integer (                          &
                                globbuf, glob_ofst ,          &    ! recvbuf
                                counts                      , &    ! recvcounts
                                Field, field_ofst,            &
                                my_count ,                    &    ! sendcount
                                displs                      , &    ! displs
                                root                        , &    ! root
                                communicator                , &    ! communicator
                                ierr )
   IMPLICIT NONE
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
   INTEGER, DIMENSION(*) :: Field, globbuf
#ifndef STUBMPI
   INCLUDE 'mpif.h'

           CALL mpi_scatterv(                                &
                            globbuf( glob_ofst ) ,           &    ! recvbuf
                            counts                         , &    ! recvcounts
                            displs                         , &    ! displs
                            MPI_INTEGER                    , &    ! recvtype
                            Field( field_ofst ),             &    ! sendbuf
                            my_count ,                       &    ! sendcount
                            MPI_INTEGER         ,            &    ! sendtype
                            root                           , &    ! root
                            communicator                   , &    ! communicator
                            ierr )
#endif

   END SUBROUTINE wrf_scatterv_integer
! end new stuff 20070124

     SUBROUTINE wrf_dm_gatherv ( v, elemsize , km_s, km_e, wordsz )
      IMPLICIT NONE
      INTEGER  elemsize, km_s, km_e, wordsz
      REAL v(*)
      IF ( wordsz .EQ. DWORDSIZE ) THEN
         CALL wrf_dm_gatherv_double(v, elemsize , km_s, km_e)
      ELSE
         CALL wrf_dm_gatherv_single(v, elemsize , km_s, km_e)
      END IF
     END SUBROUTINE wrf_dm_gatherv

     SUBROUTINE wrf_dm_gatherv_double ( v, elemsize , km_s, km_e )
      IMPLICIT NONE
      INTEGER  elemsize, km_s, km_e
      REAL*8 v(0:*)
#ifndef STUBMPI
# ifndef USE_MPI_IN_PLACE
      REAL*8 v_local((km_e-km_s+1)*elemsize)
# endif
      INTEGER, DIMENSION(:), ALLOCATABLE :: recvcounts, displs
      INTEGER send_type, myproc, nproc, local_comm, ierr, i
   INCLUDE 'mpif.h'
      send_type = MPI_DOUBLE_PRECISION
      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_nproc( nproc )
      CALL wrf_get_myproc( myproc )
      ALLOCATE( recvcounts(nproc), displs(nproc) )
      i = (km_e-km_s+1)*elemsize
      CALL mpi_allgather( i,1,MPI_INTEGER,recvcounts,1,MPI_INTEGER,local_comm,ierr) ;
      i = (km_s)*elemsize
      CALL mpi_allgather( i,1,MPI_INTEGER,displs,1,MPI_INTEGER,local_comm,ierr) ;
# ifdef USE_MPI_IN_PLACE
      CALL mpi_allgatherv( MPI_IN_PLACE,                                  &
# else
      DO i = 1,elemsize*(km_e-km_s+1)
        v_local(i) = v(i+elemsize*km_s-1)
      END DO
      CALL mpi_allgatherv( v_local,                                       &
# endif
                           (km_e-km_s+1)*elemsize,                        &
                           send_type,                                     &
                           v,                                             &
                           recvcounts,                                    &
                           displs,                                        &
                           send_type,                                     &
                           local_comm,                                    &
                           ierr )
      DEALLOCATE(recvcounts)
      DEALLOCATE(displs)
#endif
      return
     END SUBROUTINE wrf_dm_gatherv_double

     SUBROUTINE wrf_dm_gatherv_single ( v, elemsize , km_s, km_e )
      IMPLICIT NONE
      INTEGER  elemsize, km_s, km_e
      REAL*4 v(0:*)
#ifndef STUBMPI
# ifndef USE_MPI_IN_PLACE
      REAL*4 v_local((km_e-km_s+1)*elemsize)
# endif
      INTEGER, DIMENSION(:), ALLOCATABLE :: recvcounts, displs
      INTEGER send_type, myproc, nproc, local_comm, ierr, i
   INCLUDE 'mpif.h'
      send_type = MPI_REAL
      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_nproc( nproc )
      CALL wrf_get_myproc( myproc )
      ALLOCATE( recvcounts(nproc), displs(nproc) )
      i = (km_e-km_s+1)*elemsize
      CALL mpi_allgather( i,1,MPI_INTEGER,recvcounts,1,MPI_INTEGER,local_comm,ierr) ;
      i = (km_s)*elemsize
      CALL mpi_allgather( i,1,MPI_INTEGER,displs,1,MPI_INTEGER,local_comm,ierr) ;
# ifdef USE_MPI_IN_PLACE
      CALL mpi_allgatherv( MPI_IN_PLACE,                                  &
# else
      DO i = 1,elemsize*(km_e-km_s+1)
        v_local(i) = v(i+elemsize*km_s-1)
      END DO
      CALL mpi_allgatherv( v_local,                                       &
# endif
                           (km_e-km_s+1)*elemsize,                        &
                           send_type,                                     &
                           v,                                             &
                           recvcounts,                                    &
                           displs,                                        &
                           send_type,                                     &
                           local_comm,                                    &
                           ierr )
      DEALLOCATE(recvcounts)
      DEALLOCATE(displs)
#endif
      return
     END SUBROUTINE wrf_dm_gatherv_single

      SUBROUTINE wrf_dm_decomp1d( nt, km_s, km_e )
       IMPLICIT NONE
       INTEGER, INTENT(IN)  :: nt
       INTEGER, INTENT(OUT) :: km_s, km_e
     ! local
       INTEGER nn, nnp,  na, nb
       INTEGER myproc, nproc

       CALL wrf_get_myproc(myproc)
       CALL wrf_get_nproc(nproc)
       nn = nt / nproc           ! min number done by this task
       nnp = nn
       if ( myproc .lt. mod( nt, nproc ) )   nnp = nnp + 1 ! distribute remainder

       na = min( myproc, mod(nt,nproc) ) ! Number of blocks with remainder that precede this one
       nb = max( 0, myproc - na )        ! number of blocks without a remainder that precede this one
       km_s = na * ( nn+1) + nb * nn     ! starting iteration for this task
       km_e = km_s + nnp - 1             ! ending iteration for this task
      END SUBROUTINE wrf_dm_decomp1d


SUBROUTINE wrf_dm_define_comms ( grid )
   USE module_domain, ONLY : domain
   IMPLICIT NONE
   TYPE(domain) , INTENT (INOUT) :: grid
   RETURN
END SUBROUTINE wrf_dm_define_comms

SUBROUTINE tfp_message( fname, lno )
   CHARACTER*(*) fname
   INTEGER lno
   CHARACTER*1024 mess
#ifndef STUBMPI
   WRITE(mess,*)'tfp_message: ',trim(fname),lno
   CALL wrf_message(mess)
# ifdef ALLOW_OVERDECOMP
     CALL task_for_point_message  ! defined in RSL_LITE/task_for_point.c
# else
     CALL wrf_error_fatal(mess)
# endif
#endif
END SUBROUTINE tfp_message

   SUBROUTINE set_dm_debug
    USE module_dm, ONLY : dm_debug_flag
    IMPLICIT NONE
    dm_debug_flag = .TRUE.
   END SUBROUTINE set_dm_debug
   SUBROUTINE reset_dm_debug
    USE module_dm, ONLY : dm_debug_flag
    IMPLICIT NONE
    dm_debug_flag = .FALSE.
   END SUBROUTINE reset_dm_debug
   SUBROUTINE get_dm_debug ( arg )
    USE module_dm, ONLY : dm_debug_flag
    IMPLICIT NONE
    LOGICAL arg
    arg = dm_debug_flag
   END SUBROUTINE get_dm_debug

