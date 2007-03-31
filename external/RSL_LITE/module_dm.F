!WRF:PACKAGE:RSL 
!
MODULE module_dm

   USE module_machine
   USE module_configure
   USE module_state_description
   USE module_wrf_error
   USE module_driver_constants
   IMPLICIT NONE

#if ( NMM_CORE == 1 ) || defined( WRF_CHEM ) 
   INTEGER, PARAMETER :: max_halo_width = 6
#else
   INTEGER, PARAMETER :: max_halo_width = 5
#endif

   INTEGER :: ips_save, ipe_save, jps_save, jpe_save, itrace

   INTEGER ntasks, ntasks_y, ntasks_x, mytask, mytask_x, mytask_y
   INTEGER local_communicator, local_communicator_periodic, local_iocommunicator
   INTEGER local_communicator_x, local_communicator_y ! subcommunicators for rows and cols of mesh
   LOGICAL :: dm_debug_flag = .FALSE.

   INTERFACE wrf_dm_maxval
     MODULE PROCEDURE wrf_dm_maxval_real , wrf_dm_maxval_integer, wrf_dm_maxval_doubleprecision
   END INTERFACE

   INTERFACE wrf_dm_minval                       ! gopal's doing
     MODULE PROCEDURE wrf_dm_minval_real , wrf_dm_minval_integer, wrf_dm_minval_doubleprecision
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
          ENDIF
        ENDIF
      ENDDO
      IF ( MINM .LT. PROCMIN_M .OR. MINN .LT. PROCMIN_N ) THEN
        WRITE( wrf_err_message , * )'MPASPECT: UNABLE TO GENERATE PROCESSOR MESH.  STOPPING.'
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        WRITE(0,*)' PROCMIN_M ', PROCMIN_M
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
      ENDIF
   RETURN
   END SUBROUTINE MPASPECT

   SUBROUTINE wrf_dm_initialize
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INTEGER :: local_comm, local_comm2, new_local_comm, group, newgroup, p, p1, ierr
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ranks
      INTEGER comdup
      INTEGER, DIMENSION(2) :: dims, coords
      LOGICAL, DIMENSION(2) :: isperiodic
      LOGICAL :: reorder_mesh

      CALL wrf_get_dm_communicator ( local_comm )
      CALL mpi_comm_size( local_comm, ntasks, ierr )
      CALL nl_get_nproc_x ( 1, ntasks_x )
      CALL nl_get_nproc_y ( 1, ntasks_y )
      CALL nl_get_reorder_mesh( 1, reorder_mesh )

! check if user has specified in the namelist
      IF ( ntasks_x .GT. 0 .OR. ntasks_y .GT. 0 ) THEN
        ! if only ntasks_x is specified then make it 1-d decomp in i
        IF      ( ntasks_x .GT. 0 .AND. ntasks_y .EQ. -1 ) THEN
          ntasks_y = ntasks / ntasks_x
        ! if only ntasks_y is specified then make it 1-d decomp in j
        ELSE IF ( ntasks_x .EQ. -1 .AND. ntasks_y .GT. 0 ) THEN
          ntasks_x = ntasks / ntasks_y
        ENDIF
        ! make sure user knows what they're doing
        IF ( ntasks_x * ntasks_y .NE. ntasks ) THEN
          WRITE( wrf_err_message , * )'WRF_DM_INITIALIZE (RSL_LITE): nproc_x * nproc_y in namelist ne ',ntasks
          CALL wrf_error_fatal ( wrf_err_message )
        ENDIF
      ELSE
        ! When neither is specified, work out mesh with MPASPECT
        ! Pass nproc_ln and nproc_nt so that number of procs in
        ! i-dim (nproc_ln) is equal or lesser.
        CALL mpaspect ( ntasks, ntasks_x, ntasks_y, 1, 1 )
      ENDIF
      WRITE( wrf_err_message , * )'Ntasks in X ',ntasks_x,', ntasks in Y ',ntasks_y
      CALL wrf_message( wrf_err_message )

      CALL mpi_comm_rank( local_comm, mytask, ierr )
! extra code to reorder the communicator 20051212jm
      IF ( reorder_mesh ) THEN
write(0,*)'reordering mesh'
        ALLOCATE (ranks(ntasks))
        CALL mpi_comm_dup ( local_comm , local_comm2, ierr )
        CALL mpi_comm_group ( local_comm2, group, ierr )
        DO p1=1,ntasks
          p = p1 - 1
          ranks(p1) = mod( p , ntasks_x ) * ntasks_y + p / ntasks_x  
        ENDDO
        CALL mpi_group_incl( group, ntasks, ranks, newgroup, ierr )
        DEALLOCATE (ranks)
        CALL mpi_comm_create( local_comm2, newgroup, new_local_comm , ierr )
      ELSE
        new_local_comm = local_comm
      ENDIF
! end extra code to reorder the communicator 20051212jm
      dims(1) = ntasks_y  ! rows
      dims(2) = ntasks_x  ! columns
      isperiodic(1) = .false.
      isperiodic(2) = .false.
      CALL mpi_cart_create( new_local_comm, 2, dims, isperiodic, .false., local_communicator, ierr )
      dims(1) = ntasks_y  ! rows
      dims(2) = ntasks_x  ! columns
      isperiodic(1) = .true.
      isperiodic(2) = .true.
      CALL mpi_cart_create( new_local_comm, 2, dims, isperiodic, .false., local_communicator_periodic, ierr )
! debug
      CALL mpi_comm_rank( local_communicator_periodic, mytask, ierr )
      CALL mpi_cart_coords( local_communicator_periodic, mytask, 2, coords, ierr )
        write(0,*)'periodic coords ',mytask, coords

      CALL mpi_comm_rank( local_communicator, mytask, ierr )
      CALL mpi_cart_coords( local_communicator, mytask, 2, coords, ierr )
        write(0,*)'non periodic coords ',mytask, coords
      mytask_x = coords(2)   ! col task (x)
      mytask_y = coords(1)   ! row task (y)
      CALL nl_set_nproc_x ( 1, ntasks_x )
      CALL nl_set_nproc_y ( 1, ntasks_y )

! 20061228 set up subcommunicators for processors in X, Y coords of mesh
! note that local_comm_x has all the processors in a row (X=0:nproc_x-1);
! in other words, local_comm_x has all the processes with the same rank in Y
      CALL MPI_Comm_dup( new_local_comm, comdup, ierr )
      IF ( ierr .NE. 0 ) CALL wrf_error_fatal('MPI_Comm_dup fails in 20061228 mod')
      CALL MPI_Comm_split(comdup,mytask_y,mytask,local_communicator_x,ierr)
      IF ( ierr .NE. 0 ) CALL wrf_error_fatal('MPI_Comm_split fails for x in 20061228 mod')
      CALL MPI_Comm_split(comdup,mytask_x,mytask,local_communicator_y,ierr)
      IF ( ierr .NE. 0 ) CALL wrf_error_fatal('MPI_Comm_split fails for y in 20061228 mod')
! end 20061228

      CALL wrf_set_dm_communicator ( local_communicator )
      RETURN
   END SUBROUTINE wrf_dm_initialize

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

      USE module_domain
      USE module_machine

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
      INTEGER               :: idim_cd, jdim_cd

      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER  :: nest_grid


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

      CALL compute_memory_dims_rsl_lite ( 0 , bdx, bdy,   &
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
      ENDIF

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
      ENDIF

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

         ! we want the intermediate domain to be decomposed the
         ! the same as the underlying nest. So try this:

! At such time as NMM nesting is able to use RSL_LITE (would require
! a number of other mods to this file for that to happen), this should
! be updated along the lines of what's done in compute_memory_dims_rsl_lite
! below. See note dated 20051020.  JM

         c_ips = -1
         nj = ( c_jds - j_parent_start ) * parent_grid_ratio + 1 + 1 ;
         DO i = c_ids, c_ide
            ni = ( i - i_parent_start ) * parent_grid_ratio + 1 + 1 ;
            CALL task_for_point ( ni, nj, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py )
            IF ( Px .EQ. mytask_x ) THEN
               c_ipe = i
               IF ( c_ips .EQ. -1 ) c_ips = i
            ENDIF
         ENDDO

         c_jps = -1
         ni = ( c_ids - i_parent_start ) * parent_grid_ratio + 1 + 1 ;
         DO j = c_jds, c_jde
            nj = ( j - j_parent_start ) * parent_grid_ratio + 1 + 1 ;
            CALL task_for_point ( ni, nj, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py )
            IF ( Py .EQ. mytask_y ) THEN
               c_jpe = j
               IF ( c_jps .EQ. -1 ) c_jps = j
            ENDIF
         ENDDO

! extend the patch dimensions out shw along edges of domain
         IF ( mytask_x .EQ. 0 ) THEN
           c_ips = c_ips - shw
         ENDIF
         IF ( mytask_x .EQ. ntasks_x-1 ) THEN
           c_ipe = c_ipe + shw
         ENDIF
         c_ims = max( c_ips - max(shw,max_halo_width), c_ids - bdx ) - 1
         c_ime = min( c_ipe + max(shw,max_halo_width), c_ide + bdx ) + 1

! handle j dims
! extend the patch dimensions out shw along edges of domain
         IF ( mytask_y .EQ. 0 ) THEN
            c_jps = c_jps - shw
         ENDIF
         IF ( mytask_y .EQ. ntasks_y-1 ) THEN
            c_jpe = c_jpe + shw
         ENDIF
         c_jms = max( c_jps - max(shw,max_halo_width), c_jds - bdx ) - 1
         c_jme = min( c_jpe + max(shw,max_halo_width), c_jde + bdx ) + 1
! handle k dims
         c_kps = 1
         c_kpe = c_kde
         c_kms = 1
         c_kme = c_kde

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

         NULLIFY( intermediate_grid%sibling )
         DO i = 1, max_nests
            NULLIFY( intermediate_grid%nests(i)%ptr )
         ENDDO
         NULLIFY  (intermediate_grid%next)
         NULLIFY  (intermediate_grid%same_level)
         NULLIFY  (intermediate_grid%i_start)
         NULLIFY  (intermediate_grid%j_start)
         NULLIFY  (intermediate_grid%i_end)
         NULLIFY  (intermediate_grid%j_end)
         intermediate_grid%id = id
         intermediate_grid%num_nests = 0
         intermediate_grid%num_siblings = 0
         intermediate_grid%num_parents = 1
         intermediate_grid%max_tiles   = 0
         intermediate_grid%num_tiles_spec   = 0
         CALL find_grid_by_id ( id, head_grid, nest_grid )

         nest_grid%intermediate_grid => intermediate_grid  ! nest grid now has a pointer to this baby
         intermediate_grid%parents(1)%ptr => nest_grid     ! the intermediate grid considers nest its parent
         intermediate_grid%num_parents = 1

         c_sm1x = 1 ; c_em1x = 1 ; c_sm2x = 1 ; c_em2x = 1 ; c_sm3x = 1 ; c_em3x = 1
         c_sm1y = 1 ; c_em1y = 1 ; c_sm2y = 1 ; c_em2y = 1 ; c_sm3y = 1 ; c_em3y = 1

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

#ifdef SGIALTIX
         ! allocate space for the intermediate domain
         CALL alloc_space_field ( intermediate_grid, intermediate_grid%id , 1, 2 , .TRUE., &   ! use same id as nest
                               c_sd1, c_ed1, c_sd2, c_ed2, c_sd3, c_ed3,       &
                               c_sm1,  c_em1,  c_sm2,  c_em2,  c_sm3,  c_em3,  &
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
      ENDIF

      RETURN
  END SUBROUTINE patch_domain_rsl_lite

  SUBROUTINE compute_memory_dims_rsl_lite  (      &
                   shw , bdx,  bdy ,              &
                   ids,  ide,  jds,  jde,  kds,  kde, &
                   ims,  ime,  jms,  jme,  kms,  kme, &
                   imsx, imex, jmsx, jmex, kmsx, kmex, &
                   imsy, imey, jmsy, jmey, kmsy, kmey, &
                   ips,  ipe,  jps,  jpe,  kps,  kpe, &
                   ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                   ipsy, ipey, jpsy, jpey, kpsy, kpey )

    USE module_machine
    IMPLICIT NONE
    INTEGER, INTENT(IN)               ::  shw, bdx, bdy
    INTEGER, INTENT(IN)     ::  ids, ide, jds, jde, kds, kde
    INTEGER, INTENT(OUT)    ::  ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(OUT)    ::  imsx, imex, jmsx, jmex, kmsx, kmex
    INTEGER, INTENT(OUT)    ::  imsy, imey, jmsy, jmey, kmsy, kmey
    INTEGER, INTENT(OUT)    ::  ips, ipe, jps, jpe, kps, kpe
    INTEGER, INTENT(OUT)    ::  ipsx, ipex, jpsx, jpex, kpsx, kpex
    INTEGER, INTENT(OUT)    ::  ipsy, ipey, jpsy, jpey, kpsy, kpey

    INTEGER Px, Py, P, i, j, k

#if ( ! NMM_CORE == 1 )

! xy decomposition

    ips = -1
    j = jds ;
    DO i = ids, ide
       CALL task_for_point ( i, j, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py )
       IF ( Px .EQ. mytask_x ) THEN
          ipe = i
          IF ( ips .EQ. -1 ) ips = i
       ENDIF
    ENDDO
    jps = -1
    i = ids ;
    DO j = jds, jde
       CALL task_for_point ( i, j, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py )
       IF ( Py .EQ. mytask_y ) THEN
          jpe = j
          IF ( jps .EQ. -1 ) jps = j
       ENDIF
    ENDDO

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
    DO k = kds, kde
       CALL task_for_point ( k, j, kds, kde, jds, jde, ntasks_x, ntasks_y, Px, Py )
       IF ( Px .EQ. mytask_x ) THEN
          kpex = k
          IF ( kpsx .EQ. -1 ) kpsx = k
       ENDIF
    ENDDO

    jpsx = -1
    k = kds ;
    DO j = jds, jde
       CALL task_for_point ( k, j, kds, kde, jds, jde, ntasks_x, ntasks_y, Px, Py )
       IF ( Py .EQ. mytask_y ) THEN
          jpex = j
          IF ( jpsx .EQ. -1 ) jpsx = j
       ENDIF
    ENDDO

! XzYx decomposition  (note, x grid dim is decomposed over Y processor dim)

    kpsy = kpsx   ! same as above
    kpey = kpex   ! same as above

    ipsy = -1
    k = kds ;
    DO i = ids, ide
       CALL task_for_point ( i, k, ids, ide, kds, kde, ntasks_y, ntasks_x, Py, Px ) ! x and y for proc mesh reversed
       IF ( Py .EQ. mytask_y ) THEN
          ipey = i
          IF ( ipsy .EQ. -1 ) ipsy = i
       ENDIF
    ENDDO


#else

! In case of NMM CORE, the domain only ever runs from ids..ide-1 and jds..jde-1 so
! adjust decomposition to reflect.  20051020 JM
    ips = -1
    j = jds ;
    DO i = ids, ide-1
       CALL task_for_point ( i, j, ids, ide-1, jds, jde-1, ntasks_x, ntasks_y, Px, Py )
       IF ( Px .EQ. mytask_x ) THEN
          ipe = i
          IF ( Px .EQ. ntasks_x-1 ) ipe = ipe + 1
          IF ( ips .EQ. -1 ) ips = i
       ENDIF
    ENDDO
    jps = -1
    i = ids ;
    DO j = jds, jde-1
       CALL task_for_point ( i, j, ids, ide-1, jds, jde-1, ntasks_x, ntasks_y, Px, Py )
       IF ( Py .EQ. mytask_y ) THEN
          jpe = j
          IF ( Py .EQ. ntasks_y-1 ) jpe = jpe + 1
          IF ( jps .EQ. -1 ) jps = j
       ENDIF
    ENDDO
#endif

! extend the patch dimensions out shw along edges of domain
    IF ( mytask_x .EQ. 0 ) THEN
       ips = ips - shw
       ipsy = ipsy - shw
    ENDIF
    IF ( mytask_x .EQ. ntasks_x-1 ) THEN
       ipe = ipe + shw
       ipey = ipey + shw
    ENDIF
    IF ( mytask_y .EQ. 0 ) THEN
       jps = jps - shw
       jpsx = jpsx - shw
    ENDIF
    IF ( mytask_y .EQ. ntasks_y-1 ) THEN
       jpe = jpe + shw
       jpex = jpex + shw
    ENDIF

    kps = 1
    kpe = kde-kds+1

    kms = 1
    kme = kpe
    kmsx = kpsx
    kmex = kpex
    kmsy = kpsy
    kmey = kpey

    ims = max( ips - max(shw,max_halo_width), ids - bdx ) - 1
    ime = min( ipe + max(shw,max_halo_width), ide + bdx ) + 1
    imsx = ids
    imex = ide
    ipsx = imsx
    ipex = imex
    imsy = ipsy
    imey = ipey

    jms = max( jps - max(shw,max_halo_width), jds - bdy ) - 1
    jme = min( jpe + max(shw,max_halo_width), jde + bdy ) + 1
    jmsx = jpsx
    jmex = jpex
    jmsy = jds
    jmey = jde
    jpsy = jmsy
    jpey = jmey

  END SUBROUTINE compute_memory_dims_rsl_lite

! internal, used below for switching the argument to MPI calls
! if reals are being autopromoted to doubles in the build of WRF
   INTEGER function getrealmpitype()
#ifndef STUBMPI
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INTEGER rtypesize, dtypesize, ierr
      CALL mpi_type_size ( MPI_REAL, rtypesize, ierr )
      CALL mpi_type_size ( MPI_DOUBLE_PRECISION, dtypesize, ierr )
      IF ( RWORDSIZE .EQ. rtypesize ) THEN
        getrealmpitype = MPI_REAL
      ELSE IF ( RWORDSIZE .EQ. dtypesize ) THEN
        getrealmpitype = MPI_DOUBLE_PRECISION
      ELSE
        CALL wrf_error_fatal ( 'RWORDSIZE or DWORDSIZE does not match any MPI type' )
      ENDIF
#else
! required dummy initialization for function that is never called
      getrealmpitype = 1
#endif
      RETURN
   END FUNCTION getrealmpitype

   REAL FUNCTION wrf_dm_max_real ( inval )
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      REAL inval, retval
      INTEGER ierr
      CALL mpi_allreduce ( inval, retval , 1, getrealmpitype(), MPI_MAX, local_communicator, ierr )
      wrf_dm_max_real = retval
   END FUNCTION wrf_dm_max_real

   REAL FUNCTION wrf_dm_min_real ( inval )
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      REAL inval, retval
      INTEGER ierr
      CALL mpi_allreduce ( inval, retval , 1, getrealmpitype(), MPI_MIN, local_communicator, ierr )
      wrf_dm_min_real = retval
   END FUNCTION wrf_dm_min_real

   REAL FUNCTION wrf_dm_sum_real ( inval )
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      REAL inval, retval
      INTEGER ierr
      CALL mpi_allreduce ( inval, retval , 1, getrealmpitype(), MPI_SUM, local_communicator, ierr )
      wrf_dm_sum_real = retval
   END FUNCTION wrf_dm_sum_real

   INTEGER FUNCTION wrf_dm_sum_integer ( inval )
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INTEGER inval, retval
      INTEGER ierr
      CALL mpi_allreduce ( inval, retval , 1, MPI_INTEGER, MPI_SUM, local_communicator, ierr )
      wrf_dm_sum_integer = retval
   END FUNCTION wrf_dm_sum_integer

   SUBROUTINE wrf_dm_maxval_real ( val, idex, jdex )
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      REAL val, val_all( ntasks )
      INTEGER idex, jdex, ierr
      INTEGER dex(2)
      INTEGER dex_all (2,ntasks)
      INTEGER i

      dex(1) = idex ; dex(2) = jdex
      CALL mpi_allgather ( dex, 2, MPI_INTEGER, dex_all , 2, MPI_INTEGER, local_communicator, ierr )
      CALL mpi_allgather ( val, 1, getrealmpitype(), val_all , 1, getrealmpitype(), local_communicator, ierr )
      val = val_all(1)
      idex = dex_all(1,1) ; jdex = dex_all(2,1)
      DO i = 2, ntasks
        IF ( val_all(i) .GT. val ) THEN
           val = val_all(i)
           idex = dex_all(1,i)
           jdex = dex_all(2,i)
        ENDIF
      ENDDO
   END SUBROUTINE wrf_dm_maxval_real

   SUBROUTINE wrf_dm_maxval_doubleprecision ( val, idex, jdex )
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      DOUBLE PRECISION val, val_all( ntasks )
      INTEGER idex, jdex, ierr
      INTEGER dex(2)
      INTEGER dex_all (2,ntasks)
      INTEGER i

      dex(1) = idex ; dex(2) = jdex
      CALL mpi_allgather ( dex, 2, MPI_INTEGER, dex_all , 2, MPI_INTEGER, local_communicator, ierr )
      CALL mpi_allgather ( val, 1, MPI_DOUBLE_PRECISION, val_all , 1, MPI_DOUBLE_PRECISION, local_communicator, ierr )
      val = val_all(1)
      idex = dex_all(1,1) ; jdex = dex_all(2,1)
      DO i = 2, ntasks
        IF ( val_all(i) .GT. val ) THEN
           val = val_all(i)
           idex = dex_all(1,i)
           jdex = dex_all(2,i)
        ENDIF
      ENDDO
   END SUBROUTINE wrf_dm_maxval_doubleprecision

   SUBROUTINE wrf_dm_maxval_integer ( val, idex, jdex )
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INTEGER val, val_all( ntasks )
      INTEGER idex, jdex, ierr
      INTEGER dex(2)
      INTEGER dex_all (2,ntasks)
      INTEGER i

      dex(1) = idex ; dex(2) = jdex
      CALL mpi_allgather ( dex, 2, MPI_INTEGER, dex_all , 2, MPI_INTEGER, local_communicator, ierr )
      CALL mpi_allgather ( val, 1, MPI_INTEGER, val_all , 1, MPI_INTEGER, local_communicator, ierr )
      val = val_all(1)
      idex = dex_all(1,1) ; jdex = dex_all(2,1)
      DO i = 2, ntasks
        IF ( val_all(i) .GT. val ) THEN
           val = val_all(i)
           idex = dex_all(1,i)
           jdex = dex_all(2,i)
        ENDIF
      ENDDO
   END SUBROUTINE wrf_dm_maxval_integer

!  For HWRF some additional computation is required. This is gopal's doing

   SUBROUTINE wrf_dm_minval_real ( val, idex, jdex )
      IMPLICIT NONE
      REAL val, val_all( ntasks )
      INTEGER idex, jdex, ierr
      INTEGER dex(2)
      INTEGER dex_all (2,ntasks)
! <DESCRIPTION>
! Collective operation. Each processor calls passing a local value and its index; on return
! all processors are passed back the maximum of all values passed and its index.
!
! </DESCRIPTION>
      INTEGER i, comm
#ifndef STUBMPI
      INCLUDE 'mpif.h'

      CALL wrf_get_dm_communicator ( comm )
      dex(1) = idex ; dex(2) = jdex
      CALL mpi_allgather ( dex, 2, MPI_INTEGER, dex_all , 2, MPI_INTEGER, comm, ierr )
      CALL mpi_allgather ( val, 1, MPI_REAL, val_all , 1, MPI_REAL, comm, ierr )
      val = val_all(1)
      idex = dex_all(1,1) ; jdex = dex_all(2,1)
      DO i = 2, ntasks
        IF ( val_all(i) .LT. val ) THEN
           val = val_all(i)
           idex = dex_all(1,i)
           jdex = dex_all(2,i)
        ENDIF
      ENDDO
#endif
   END SUBROUTINE wrf_dm_minval_real

   SUBROUTINE wrf_dm_minval_doubleprecision ( val, idex, jdex )
      IMPLICIT NONE
      DOUBLE PRECISION val, val_all( ntasks )
      INTEGER idex, jdex, ierr
      INTEGER dex(2)
      INTEGER dex_all (2,ntasks)
! <DESCRIPTION>
! Collective operation. Each processor calls passing a local value and its index; on return
! all processors are passed back the maximum of all values passed and its index.
!
! </DESCRIPTION>
      INTEGER i, comm
#ifndef STUBMPI
      INCLUDE 'mpif.h'

      CALL wrf_get_dm_communicator ( comm )
      dex(1) = idex ; dex(2) = jdex
      CALL mpi_allgather ( dex, 2, MPI_INTEGER, dex_all , 2, MPI_INTEGER, comm, ierr )
      CALL mpi_allgather ( val, 1, MPI_DOUBLE_PRECISION, val_all , 1, MPI_DOUBLE_PRECISION, comm, ierr )
      val = val_all(1)
      idex = dex_all(1,1) ; jdex = dex_all(2,1)
      DO i = 2, ntasks
        IF ( val_all(i) .LT. val ) THEN
           val = val_all(i)
           idex = dex_all(1,i)
           jdex = dex_all(2,i)
        ENDIF
      ENDDO
#endif
   END SUBROUTINE wrf_dm_minval_doubleprecision

   SUBROUTINE wrf_dm_minval_integer ( val, idex, jdex )
      IMPLICIT NONE
      INTEGER val, val_all( ntasks )
      INTEGER idex, jdex, ierr
      INTEGER dex(2)
      INTEGER dex_all (2,ntasks)
! <DESCRIPTION>
! Collective operation. Each processor calls passing a local value and its index; on return
! all processors are passed back the maximum of all values passed and its index.
!
! </DESCRIPTION>
      INTEGER i, comm
#ifndef STUBMPI
      INCLUDE 'mpif.h'

      CALL wrf_get_dm_communicator ( comm )
      dex(1) = idex ; dex(2) = jdex
      CALL mpi_allgather ( dex, 2, MPI_INTEGER, dex_all , 2, MPI_INTEGER, comm, ierr )
      CALL mpi_allgather ( val, 1, MPI_INTEGER, val_all , 1, MPI_INTEGER, comm, ierr )
      val = val_all(1)
      idex = dex_all(1,1) ; jdex = dex_all(2,1)
      DO i = 2, ntasks
        IF ( val_all(i) .LT. val ) THEN
           val = val_all(i)
           idex = dex_all(1,i)
           jdex = dex_all(2,i)
        ENDIF
      ENDDO
#endif
   END SUBROUTINE wrf_dm_minval_integer     ! End of gopal's doing

   SUBROUTINE init_module_dm
      IMPLICIT NONE
      INTEGER mpi_comm_local, ierr, mytask, nproc
      INCLUDE 'mpif.h'
      LOGICAL mpi_inited
      CALL mpi_initialized( mpi_inited, ierr )
      IF ( .NOT. mpi_inited ) THEN
	! If MPI has not been initialized then initialize it and 
	! make comm_world the communicator
	! Otherwise, something else (e.g. quilt-io) has already 
	! initialized MPI, so just grab the communicator that
	! should already be stored and use that.
	CALL mpi_init ( ierr )
        CALL wrf_termio_dup
	CALL wrf_set_dm_communicator ( MPI_COMM_WORLD )
      ENDIF
      CALL wrf_get_dm_communicator( mpi_comm_local )
   END SUBROUTINE init_module_dm

! stub
   SUBROUTINE wrf_dm_move_nest ( parent, nest, dx, dy )
      USE module_domain
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
#ifndef STUBMPI
    INCLUDE 'mpif.h'
        
    INTEGER, INTENT(IN)   :: nsta                ! Observation index.
    INTEGER, INTENT(IN)   :: nerrf               ! Number of error fields.
    INTEGER, INTENT(IN)   :: niobf               ! Number of observations.
    LOGICAL, INTENT(IN)   :: MP_LOCAL_UOBMASK(NIOBF)
    LOGICAL, INTENT(IN)   :: MP_LOCAL_VOBMASK(NIOBF)
    LOGICAL, INTENT(IN)   :: MP_LOCAL_COBMASK(NIOBF)
    REAL, INTENT(INOUT)   :: errf(nerrf, niobf)
        
! Local declarations
    integer i, n, nlocal_dot, nlocal_crs
    REAL UVT_BUFFER(NIOBF)    ! Buffer for holding U, V, or T
    REAL QRK_BUFFER(NIOBF)    ! Buffer for holding Q or RKO
    REAL SFP_BUFFER(NIOBF)    ! Buffer for holding Surface pressure
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
     ENDIF
   ENDDO
   CALL MPI_ALLGATHER(NLOCAL_DOT,1,MPI_INTEGER, &
                      ICOUNT,1,MPI_INTEGER,     &
                      MPI_COMM_COMP,IERR)
   I = 1

   IDISPLACEMENT(1) = 0
   DO I = 2, NPROCS
     IDISPLACEMENT(I) = IDISPLACEMENT(I-1) + ICOUNT(I-1)
   ENDDO
   CALL MPI_ALLGATHERV( N_BUFFER, NLOCAL_DOT, MPI_INTEGER,    &
                        IFULL_BUFFER, ICOUNT, IDISPLACEMENT,  &
                        MPI_INTEGER, MPI_COMM_COMP, IERR)
! U
   CALL MPI_ALLGATHERV( UVT_BUFFER, NLOCAL_DOT, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(1,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   ENDDO
! SURF PRESS AT U-POINTS
   CALL MPI_ALLGATHERV( SFP_BUFFER, NLOCAL_DOT, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(7,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   ENDDO
! RKO
   CALL MPI_ALLGATHERV( QRK_BUFFER, NLOCAL_DOT, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(9,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   ENDDO

! DO THE V FIELD
   NLOCAL_DOT = 0
   DO N = 1, NSTA
     IF ( MP_LOCAL_VOBMASK(N) ) THEN         ! USE V-POINT MASK
       NLOCAL_DOT = NLOCAL_DOT + 1
       UVT_BUFFER(NLOCAL_DOT) = ERRF(2,N)    ! V WIND COMPONENT
       SFP_BUFFER(NLOCAL_DOT) = ERRF(8,N)    ! SURFACE PRESSURE
       N_BUFFER(NLOCAL_DOT) = N
     ENDIF
   ENDDO
   CALL MPI_ALLGATHER(NLOCAL_DOT,1,MPI_INTEGER, &
                      ICOUNT,1,MPI_INTEGER,     &
                      MPI_COMM_COMP,IERR)
   I = 1

   IDISPLACEMENT(1) = 0
   DO I = 2, NPROCS
     IDISPLACEMENT(I) = IDISPLACEMENT(I-1) + ICOUNT(I-1)
   ENDDO
   CALL MPI_ALLGATHERV( N_BUFFER, NLOCAL_DOT, MPI_INTEGER,    &
                        IFULL_BUFFER, ICOUNT, IDISPLACEMENT,  &
                        MPI_INTEGER, MPI_COMM_COMP, IERR)
! V
   CALL MPI_ALLGATHERV( UVT_BUFFER, NLOCAL_DOT, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(2,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   ENDDO
! SURF PRESS AT V-POINTS
   CALL MPI_ALLGATHERV( SFP_BUFFER, NLOCAL_DOT, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(8,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   ENDDO

! DO THE CROSS FIELDS, T AND Q
   NLOCAL_CRS = 0
   DO N = 1, NSTA
     IF ( MP_LOCAL_COBMASK(N) ) THEN       ! USE MASS-POINT MASK
       NLOCAL_CRS = NLOCAL_CRS + 1
       UVT_BUFFER(NLOCAL_CRS) = ERRF(3,N)     ! TEMPERATURE
       QRK_BUFFER(NLOCAL_CRS) = ERRF(4,N)     ! MOISTURE
       SFP_BUFFER(NLOCAL_CRS) = ERRF(6,N)     ! SURFACE PRESSURE
       N_BUFFER(NLOCAL_CRS) = N
     ENDIF
   ENDDO
   CALL MPI_ALLGATHER(NLOCAL_CRS,1,MPI_INTEGER, &
                      ICOUNT,1,MPI_INTEGER,     &
                      MPI_COMM_COMP,IERR)
   IDISPLACEMENT(1) = 0
   DO I = 2, NPROCS
     IDISPLACEMENT(I) = IDISPLACEMENT(I-1) + ICOUNT(I-1)
   ENDDO
   CALL MPI_ALLGATHERV( N_BUFFER, NLOCAL_CRS, MPI_INTEGER,    &
                        IFULL_BUFFER, ICOUNT, IDISPLACEMENT,  &
                        MPI_INTEGER, MPI_COMM_COMP, IERR)
! T
   CALL MPI_ALLGATHERV( UVT_BUFFER, NLOCAL_CRS, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)

   DO N = 1, NSTA
     ERRF(3,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   ENDDO
! Q
   CALL MPI_ALLGATHERV( QRK_BUFFER, NLOCAL_CRS, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(4,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   ENDDO
! SURF PRESS AT MASS POINTS
   CALL MPI_ALLGATHERV( SFP_BUFFER, NLOCAL_CRS, MPI_REAL,     &
                        FULL_BUFFER, ICOUNT, IDISPLACEMENT,   &
                        MPI_REAL, MPI_COMM_COMP, IERR)
   DO N = 1, NSTA
     ERRF(6,IFULL_BUFFER(N)) = FULL_BUFFER(N)
   ENDDO
#endif
   END SUBROUTINE get_full_obs_vector

END MODULE module_dm

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
   USE module_domain
   USE module_dm
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

   RETURN
END SUBROUTINE wrf_dm_patch_domain

SUBROUTINE wrf_termio_dup
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER mytask, ntasks, ierr
  CALL mpi_comm_size(MPI_COMM_WORLD, ntasks, ierr )
  CALL mpi_comm_rank(MPI_COMM_WORLD, mytask, ierr )
  write(0,*)'starting wrf task ',mytask,' of ',ntasks
  CALL rsl_error_dup1( mytask )
END SUBROUTINE wrf_termio_dup

SUBROUTINE wrf_get_myproc( myproc )
  USE module_dm
  IMPLICIT NONE
  INTEGER myproc
  myproc = mytask
  RETURN
END SUBROUTINE wrf_get_myproc

SUBROUTINE wrf_get_nproc( nproc )
  USE module_dm
  IMPLICIT NONE
  INTEGER nproc
  nproc = ntasks
  RETURN
END SUBROUTINE wrf_get_nproc

SUBROUTINE wrf_get_nprocx( nprocx )
  USE module_dm
  IMPLICIT NONE
  INTEGER nprocx
  nprocx = ntasks_x
  RETURN
END SUBROUTINE wrf_get_nprocx

SUBROUTINE wrf_get_nprocy( nprocy )
  USE module_dm
  IMPLICIT NONE
  INTEGER nprocy
  nprocy = ntasks_y
  RETURN
END SUBROUTINE wrf_get_nprocy

SUBROUTINE wrf_dm_bcast_bytes ( buf , size )
   USE module_dm
   IMPLICIT NONE
   INCLUDE 'mpif.h'
   INTEGER size
#ifndef NEC
   INTEGER*1 BUF(size)
#else
   CHARACTER*1 BUF(size)
#endif
   CALL BYTE_BCAST ( buf , size, local_communicator )
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
     ENDDO
     CALL wrf_dm_bcast_integer( ibuf, n )
     buf = ''
     DO i = 1, n
       buf(i:i) = char(ibuf(i))
     ENDDO
   ENDIF
   RETURN
END SUBROUTINE wrf_dm_bcast_string

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
  USE module_domain
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
    ENDDO
    ENDDO
  endif

  RETURN
END

   SUBROUTINE wrf_abort
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INTEGER ierr
      CALL mpi_abort(MPI_COMM_WORLD,1,ierr)
   END SUBROUTINE wrf_abort

   SUBROUTINE wrf_dm_shutdown
      IMPLICIT NONE
      INTEGER ierr
      CALL MPI_FINALIZE( ierr )
      RETURN
   END SUBROUTINE wrf_dm_shutdown

   LOGICAL FUNCTION wrf_dm_on_monitor()
      USE module_dm
      IMPLICIT NONE
      INCLUDE 'mpif.h'
      INTEGER tsk, ierr, mpi_comm_local
      CALL wrf_get_dm_communicator( mpi_comm_local )
      CALL mpi_comm_rank ( mpi_comm_local, tsk , ierr )
      wrf_dm_on_monitor = tsk .EQ. 0
      RETURN
   END FUNCTION wrf_dm_on_monitor

   INTEGER FUNCTION wrf_dm_monitor_rank()
      USE module_dm
      IMPLICIT NONE
      wrf_dm_monitor_rank = 0
      RETURN
   END FUNCTION wrf_dm_monitor_rank

   SUBROUTINE wrf_get_dm_communicator ( communicator )
      USE module_dm
      IMPLICIT NONE
      INTEGER , INTENT(OUT) :: communicator
      communicator = local_communicator
      RETURN
   END SUBROUTINE wrf_get_dm_communicator

   SUBROUTINE wrf_get_dm_iocommunicator ( iocommunicator )
      USE module_dm
      IMPLICIT NONE
      INTEGER , INTENT(OUT) :: iocommunicator
      iocommunicator = local_iocommunicator
      RETURN
   END SUBROUTINE wrf_get_dm_iocommunicator

   SUBROUTINE wrf_set_dm_communicator ( communicator )
      USE module_dm
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: communicator
      local_communicator = communicator
      RETURN
   END SUBROUTINE wrf_set_dm_communicator

   SUBROUTINE wrf_set_dm_iocommunicator ( iocommunicator )
      USE module_dm
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: iocommunicator
      local_iocommunicator = iocommunicator
      RETURN
   END SUBROUTINE wrf_set_dm_iocommunicator


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
#  define FRSTELEM (1)
#else
#  define FRSTELEM
#endif

   SUBROUTINE wrf_patch_to_global_generic (buf,globbuf,domdesc,stagger,ordering,typesize,&
                                       DS1a,DE1a,DS2a,DE2a,DS3a,DE3a,&
                                       MS1a,ME1a,MS2a,ME2a,MS3a,ME3a,&
                                       PS1a,PE1a,PS2a,PE2a,PS3a,PE3a )
       USE module_driver_constants
       USE module_timing
       USE module_wrf_error
       USE module_dm
       IMPLICIT NONE
       INTEGER                         DS1a,DE1a,DS2a,DE2a,DS3a,DE3a,&
                                       MS1a,ME1a,MS2a,ME2a,MS3a,ME3a,&
                                       PS1a,PE1a,PS2a,PE2a,PS3a,PE3A 
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       INTEGER                         ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe
       CHARACTER *(*) stagger,ordering
       LOGICAL, EXTERNAL :: wrf_dm_on_monitor, has_char
       INTEGER fid,domdesc,typesize,ierr
       REAL globbuf(*)
       REAL buf(*)

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
       ENDIF
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating tmpbuf in wrf_patch_to_global_generic')
 
       Patch(1,1) = ps1 ; Patch(1,2) = pe1    ! use patch dims
       Patch(2,1) = ps2 ; Patch(2,2) = pe2
       Patch(3,1) = ps3 ; Patch(3,2) = pe3

       IF      ( typesize .EQ. RWORDSIZE ) THEN
         CALL just_patch_r ( buf , locbuf , size(locbuf), &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )
       ELSE IF ( typesize .EQ. IWORDSIZE ) THEN
         CALL just_patch_i ( buf , locbuf , size(locbuf), &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )
       ELSE IF ( typesize .EQ. DWORDSIZE ) THEN
         CALL just_patch_d ( buf , locbuf , size(locbuf), &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )
       ELSE IF ( typesize .EQ. LWORDSIZE ) THEN
         CALL just_patch_l ( buf , locbuf , size(locbuf), &
                                   PS1, PE1, PS2, PE2, PS3, PE3 , &
                                   MS1, ME1, MS2, ME2, MS3, ME3   )
       ENDIF

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
       ENDIF

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
	 ENDIF

       ENDIF

       IF ( wrf_at_debug_level(500) ) THEN
         CALL end_timing('wrf_patch_to_global_generic')
       ENDIF
       DEALLOCATE( tmpbuf )
       RETURN
    END SUBROUTINE wrf_patch_to_global_generic

  SUBROUTINE just_patch_i ( inbuf , outbuf, noutbuf,     &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    RETURN
  END SUBROUTINE just_patch_i

  SUBROUTINE just_patch_r ( inbuf , outbuf, noutbuf,     &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    RETURN
  END SUBROUTINE just_patch_r

  SUBROUTINE just_patch_d ( inbuf , outbuf, noutbuf,     &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    RETURN
  END SUBROUTINE just_patch_d

  SUBROUTINE just_patch_l ( inbuf , outbuf, noutbuf,     &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    RETURN
  END SUBROUTINE just_patch_l


  SUBROUTINE patch_2_outbuf_r( inbuf, outbuf,            &
                               DS1,DE1,DS2,DE2,DS3,DE3,  &
                               GPATCH ) 
    USE module_dm
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
	  ENDDO
	ENDDO
      ENDDO
    ENDDO

    RETURN
  END SUBROUTINE patch_2_outbuf_r

  SUBROUTINE patch_2_outbuf_i( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    RETURN
  END SUBROUTINE patch_2_outbuf_i

  SUBROUTINE patch_2_outbuf_d( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    RETURN
  END SUBROUTINE patch_2_outbuf_d

  SUBROUTINE patch_2_outbuf_l( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    ENDDO
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
       USE module_dm
       USE module_driver_constants
       IMPLICIT NONE
       INTEGER                         DS1a,DE1a,DS2a,DE2a,DS3a,DE3a,&
                                       MS1a,ME1a,MS2a,ME2a,MS3a,ME3a,&
                                       PS1a,PE1a,PS2a,PE2a,PS3a,PE3A 
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc,typesize,ierr
       REAL globbuf(*)
       REAL buf(*)
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
       ENDIF
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
         ENDIF
       ENDIF

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
       ENDIF


       DEALLOCATE ( tmpbuf )
       RETURN
    END SUBROUTINE wrf_global_to_patch_generic

  SUBROUTINE all_sub_i ( inbuf , outbuf,              &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    RETURN
  END SUBROUTINE all_sub_i

  SUBROUTINE all_sub_r ( inbuf , outbuf,              &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO

    RETURN
  END SUBROUTINE all_sub_r

  SUBROUTINE all_sub_d ( inbuf , outbuf,              &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    RETURN
  END SUBROUTINE all_sub_d

  SUBROUTINE all_sub_l ( inbuf , outbuf,              &
                               PS1,PE1,PS2,PE2,PS3,PE3,  &
                               MS1,ME1,MS2,ME2,MS3,ME3   )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    RETURN
  END SUBROUTINE all_sub_l

  SUBROUTINE outbuf_2_patch_r( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3, &
                               MS1, ME1, MS2, ME2, MS3, ME3 , &
                               GPATCH )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    RETURN
  END SUBROUTINE outbuf_2_patch_r

  SUBROUTINE outbuf_2_patch_i( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    RETURN
  END SUBROUTINE outbuf_2_patch_i

  SUBROUTINE outbuf_2_patch_d( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    RETURN
  END SUBROUTINE outbuf_2_patch_d

  SUBROUTINE outbuf_2_patch_l( inbuf, outbuf,         &
                               DS1,DE1,DS2,DE2,DS3,DE3,&
                               GPATCH )
    USE module_dm
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
          ENDDO
        ENDDO
      ENDDO
    ENDDO
    RETURN
  END SUBROUTINE outbuf_2_patch_l



!------------------------------------------------------------------

#if ( EM_CORE == 1 )

!------------------------------------------------------------------

   SUBROUTINE force_domain_em_part2 ( grid, ngrid, config_flags    &
!
#include "em_dummy_new_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include <em_dummy_new_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

#include "em_nest_interpdown_unpack.inc"

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#include "HALO_EM_FORCE_DOWN.inc"

      ! code here to interpolate the data into the nested domain
#  include "em_nest_forcedown_interp.inc"

      RETURN
   END SUBROUTINE force_domain_em_part2

!------------------------------------------------------------------

   SUBROUTINE interp_domain_em_part1 ( grid, intermediate_grid, ngrid, config_flags    &
!
#include "em_dummy_new_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      USE module_timing
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid
#include <em_dummy_new_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      INTEGER iparstrt,jparstrt,sw
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      INTEGER icoord, jcoord, idim_cd, jdim_cd, pgr
      INTEGER local_comm, myproc, nproc

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

#include "em_nest_interpdown_pack.inc"

      CALL rsl_lite_bcast_msgs( myproc, nproc, local_comm )

      RETURN
   END SUBROUTINE interp_domain_em_part1

!------------------------------------------------------------------

   SUBROUTINE interp_domain_em_part2 ( grid, ngrid, config_flags    &
!
#include "em_dummy_new_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include <em_dummy_new_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe
      INTEGER myproc
      INTEGER ierr

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1 

#include "em_nest_interpdown_unpack.inc"

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#include "HALO_EM_INTERP_DOWN.inc"

#  include "em_nest_interpdown_interp.inc"

      RETURN
   END SUBROUTINE interp_domain_em_part2

!------------------------------------------------------------------

   SUBROUTINE feedback_nest_prep ( grid, config_flags    &
!
#include "em_dummy_new_args.inc"
!
)
      USE module_domain
      USE module_configure
      USE module_dm
      USE module_state_description
      IMPLICIT NONE
!
      TYPE(domain), TARGET :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE (grid_config_rec_type) :: config_flags ! configureation flags, has vertical dim of 
                                                  ! soil temp, moisture, etc., has vertical dim
                                                  ! of soil categories
#include <em_dummy_new_decl.inc>

      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER       :: idum1, idum2


      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#ifdef DM_PARALLEL
#include "HALO_EM_INTERP_UP.inc"
#endif

   END SUBROUTINE feedback_nest_prep

!------------------------------------------------------------------

   SUBROUTINE feedback_domain_em_part1 ( grid, ngrid, config_flags    &
!
#include "em_dummy_new_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include <em_dummy_new_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE(domain), POINTER :: xgrid
      TYPE (grid_config_rec_type)            :: config_flags, nconfig_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER local_comm, myproc, nproc, idum1, idum2

      INTERFACE
          SUBROUTINE feedback_nest_prep ( grid, config_flags    &
!
#include "em_dummy_new_args.inc"
!
)
             USE module_domain
             USE module_configure
             USE module_dm
             USE module_state_description
!
             TYPE (grid_config_rec_type)            :: config_flags
             TYPE(domain), TARGET                   :: grid
#include <em_dummy_new_decl.inc>
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
#include "em_actual_new_args.inc"
!
)

! put things back so grid is intermediate grid

      grid => xgrid
      CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

! "interp" (basically copy) ngrid onto intermediate grid

#include "em_nest_feedbackup_interp.inc"

      RETURN
   END SUBROUTINE feedback_domain_em_part1

!------------------------------------------------------------------

   SUBROUTINE feedback_domain_em_part2 ( grid, intermediate_grid, ngrid , config_flags    &
!
#include "em_dummy_new_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      USE module_utility
      IMPLICIT NONE

!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid

#include <em_dummy_new_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe
      INTEGER icoord, jcoord, idim_cd, jdim_cd
      INTEGER local_comm, myproc, nproc
      INTEGER iparstrt, jparstrt, sw
      REAL    nest_influence

      character*256 :: timestr
      integer ierr

      LOGICAL, EXTERNAL  :: em_cd_feedback_mask

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

      CALL nl_get_i_parent_start ( intermediate_grid%id, iparstrt )
      CALL nl_get_j_parent_start ( intermediate_grid%id, jparstrt )
      CALL nl_get_shw            ( intermediate_grid%id, sw )
      icoord =    iparstrt - sw
      jcoord =    jparstrt - sw
      idim_cd = cide - cids + 1
      jdim_cd = cjde - cjds + 1

      nlev  = ckde - ckds + 1

#include "em_nest_feedbackup_pack.inc"

      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_myproc( myproc )
      CALL wrf_get_nproc( nproc )

      CALL rsl_lite_merge_msgs( myproc, nproc, local_comm )

#define NEST_INFLUENCE(A,B) A = B
#include "em_nest_feedbackup_unpack.inc"

      ! smooth coarse grid
      CALL get_ijk_from_grid (  ngrid,                           &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )
      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#include "HALO_EM_INTERP_UP.inc"

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )

#include "em_nest_feedbackup_smooth.inc"

      RETURN
   END SUBROUTINE feedback_domain_em_part2
#endif

#if ( NMM_CORE == 1 && NMM_NEST == 1 )
!==============================================================================
! NMM nesting infrastructure extended from EM core. This is gopal's doing.
!==============================================================================

   SUBROUTINE interp_domain_nmm_part1 ( grid, intermediate_grid, ngrid, config_flags    &
!
#include "nmm_dummy_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      USE module_timing
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid
#include <nmm_dummy_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      INTEGER iparstrt,jparstrt,sw
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      INTEGER icoord, jcoord, idim_cd, jdim_cd, pgr
      INTEGER local_comm, myproc, nproc

      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_myproc( myproc )
      CALL wrf_get_nproc( nproc )

#define COPY_IN
#include <nmm_scalar_derefs.inc>

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

#include "nmm_nest_interpdown_pack.inc"

      CALL rsl_lite_bcast_msgs( myproc, nproc, local_comm )

#define COPY_OUT
#include <nmm_scalar_derefs.inc>
      RETURN
   END SUBROUTINE interp_domain_nmm_part1

!------------------------------------------------------------------

   SUBROUTINE interp_domain_nmm_part2 ( grid, ngrid, config_flags    &
!
#include "nmm_dummy_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include <nmm_dummy_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe
      INTEGER myproc
      INTEGER ierr

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif
#include "deref_kludge.h"

#define COPY_IN
#include <nmm_scalar_derefs.inc>
      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1 

#include "nmm_nest_interpdown_unpack.inc"

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#include "HALO_NMM_INTERP_DOWN1.inc"

#include "nmm_nest_interpdown_interp.inc"

#define COPY_OUT
#include <nmm_scalar_derefs.inc>

      RETURN
   END SUBROUTINE interp_domain_nmm_part2

!------------------------------------------------------------------

   SUBROUTINE force_domain_nmm_part1 ( grid, intermediate_grid, config_flags    &
!
#include "nmm_dummy_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      USE module_timing
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
#include <nmm_dummy_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
#define COPY_IN
#include <nmm_scalar_derefs.inc>
!
      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )

      CALL get_ijk_from_grid (  intermediate_grid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

#include "nmm_nest_forcedown_pack.inc"

!   WRITE(0,*)'I have completed PACKING of BCs data successfully'

#define COPY_OUT
#include <nmm_scalar_derefs.inc>
      RETURN
   END SUBROUTINE force_domain_nmm_part1

!==============================================================================================

   SUBROUTINE force_domain_nmm_part2 ( grid, ngrid, config_flags    &
!
#include "nmm_dummy_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include <nmm_dummy_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe
integer myproc

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif
#include "deref_kludge.h"

#define COPY_IN
#include <nmm_scalar_derefs.inc>

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

#include "nmm_nest_interpdown_unpack.inc"

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#include "HALO_NMM_FORCE_DOWN1.inc"

      ! code here to interpolate the data into the nested domain
#include "nmm_nest_forcedown_interp.inc"

#define COPY_OUT
#include <nmm_scalar_derefs.inc>

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
#include "nmm_dummy_args.inc"
!
)
      USE module_domain
      USE module_configure
      USE module_dm
      USE module_state_description
      IMPLICIT NONE
!
      TYPE(domain), TARGET :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE (grid_config_rec_type) :: config_flags ! configureation flags, has vertical dim of
                                                  ! soil temp, moisture, etc., has vertical dim
                                                  ! of soil categories
#include <nmm_dummy_decl.inc>

      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER       :: idum1, idum2


#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif
#include "deref_kludge.h"

#define COPY_IN
#include <nmm_scalar_derefs.inc>

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#ifdef DM_PARALLEL
#include "HALO_NMM_WEIGHTS.inc"
#endif

#define COPY_OUT
#include <nmm_scalar_derefs.inc>

   END SUBROUTINE feedback_nest_prep_nmm

!------------------------------------------------------------------

   SUBROUTINE feedback_domain_nmm_part1 ( grid, ngrid, config_flags    &
!
#include "nmm_dummy_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include <nmm_dummy_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE(domain), POINTER :: xgrid
      TYPE (grid_config_rec_type)            :: config_flags, nconfig_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER local_comm, myproc, nproc, idum1, idum2

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

      INTERFACE
          SUBROUTINE feedback_nest_prep_nmm ( grid, config_flags    &
!
#include "nmm_dummy_args.inc"
!
)
             USE module_domain
             USE module_configure
             USE module_dm
             USE module_state_description
!
             TYPE (grid_config_rec_type)            :: config_flags
             TYPE(domain), TARGET                   :: grid
#include <nmm_dummy_decl.inc>
          END SUBROUTINE feedback_nest_prep_nmm
      END INTERFACE
!
#define COPY_IN
#include <nmm_scalar_derefs.inc>

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
      ipe_save = ngrid%i_parent_start + (nide-nids) / ngrid%parent_grid_ratio
      jpe_save = ngrid%j_parent_start + (njde-njds) / ngrid%parent_grid_ratio

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
#include "nmm_actual_args.inc"
!
)

! put things back so grid is intermediate grid

      grid => xgrid
      CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

! "interp" (basically copy) ngrid onto intermediate grid

#include "nmm_nest_feedbackup_interp.inc"

#define COPY_OUT
#include <nmm_scalar_derefs.inc>
      RETURN
   END SUBROUTINE feedback_domain_nmm_part1

!------------------------------------------------------------------

   SUBROUTINE feedback_domain_nmm_part2 ( grid, intermediate_grid, ngrid , config_flags    &
!
#include "nmm_dummy_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      USE module_utility
      IMPLICIT NONE

!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid

#include <nmm_dummy_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe
      INTEGER icoord, jcoord, idim_cd, jdim_cd
      INTEGER local_comm, myproc, nproc
      INTEGER iparstrt, jparstrt, sw

      character*256 :: timestr
      integer ierr

      REAL    nest_influence
      LOGICAL, EXTERNAL  :: nmm_cd_feedback_mask
#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif
#include "deref_kludge.h"

#define COPY_IN
#include <nmm_scalar_derefs.inc>

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

      nide = nide - 1   !dusan
      njde = njde - 1   !dusan

      CALL nl_get_i_parent_start ( intermediate_grid%id, iparstrt )
      CALL nl_get_j_parent_start ( intermediate_grid%id, jparstrt )
      CALL nl_get_shw            ( intermediate_grid%id, sw )
      icoord =    iparstrt  - sw
      jcoord =    jparstrt  - sw
      idim_cd = cide - cids + 1
      jdim_cd = cjde - cjds + 1

      nlev  = ckde - ckds + 1

#include "nmm_nest_feedbackup_pack.inc"

      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_myproc( myproc )
      CALL wrf_get_nproc( nproc )

      CALL rsl_lite_merge_msgs( myproc, nproc, local_comm )

#include "nmm_nest_feedbackup_unpack.inc"


      ! smooth coarse grid

      CALL get_ijk_from_grid (  ngrid,                                 &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe     )
      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#include "HALO_NMM_INTERP_UP.inc"

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )

#include "nmm_nest_feedbackup_smooth.inc"

#define COPY_OUT
#include <nmm_scalar_derefs.inc>
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
   USE module_dm
   IMPLICIT NONE
   INCLUDE 'mpif.h'
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
   REAL, DIMENSION(*) :: Field, globbuf

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

   END SUBROUTINE wrf_gatherv_real

   SUBROUTINE wrf_gatherv_double (Field, field_ofst,            &
                                my_count ,                    &    ! sendcount
                                globbuf, glob_ofst ,          &    ! recvbuf
                                counts                      , &    ! recvcounts
                                displs                      , &    ! displs
                                root                        , &    ! root
                                communicator                , &    ! communicator
                                ierr )
   USE module_dm
   IMPLICIT NONE
   INCLUDE 'mpif.h'
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
! this next declaration is REAL, not DOUBLE PRECISION because it will be autopromoted
! to double precision by the compiler when WRF is compiled for 8 byte reals. Only reason
! for having this separate routine is so we pass the correct MPI type to mpi_scatterv
! if we were not indexing the globbuf and Field arrays it would not even matter
   REAL, DIMENSION(*) :: Field, globbuf

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
   INCLUDE 'mpif.h'
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
   INTEGER, DIMENSION(*) :: Field, globbuf

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
   USE module_dm
   IMPLICIT NONE
   INCLUDE 'mpif.h'
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
   REAL, DIMENSION(*) :: Field, globbuf

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
   USE module_dm
   IMPLICIT NONE
   INCLUDE 'mpif.h'
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
! this next declaration is REAL, not DOUBLE PRECISION because it will be autopromoted
! to double precision by the compiler when WRF is compiled for 8 byte reals. Only reason
! for having this separate routine is so we pass the correct MPI type to mpi_scatterv
! if we were not indexing the globbuf and Field arrays it would not even matter
   REAL, DIMENSION(*) :: Field, globbuf

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
   INCLUDE 'mpif.h'
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
   INTEGER, DIMENSION(*) :: Field, globbuf

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

   END SUBROUTINE wrf_scatterv_integer
! end new stuff 20070124

SUBROUTINE wrf_dm_define_comms ( grid )
   USE module_domain
   IMPLICIT NONE
   TYPE(domain) , INTENT (INOUT) :: grid
   RETURN
END SUBROUTINE wrf_dm_define_comms

   SUBROUTINE set_dm_debug 
    USE module_dm
    IMPLICIT NONE
    dm_debug_flag = .TRUE.
   END SUBROUTINE set_dm_debug
   SUBROUTINE reset_dm_debug 
    USE module_dm
    IMPLICIT NONE
    dm_debug_flag = .FALSE.
   END SUBROUTINE reset_dm_debug
   SUBROUTINE get_dm_debug ( arg )
    USE module_dm
    IMPLICIT NONE
    LOGICAL arg
    arg = dm_debug_flag
   END SUBROUTINE get_dm_debug
