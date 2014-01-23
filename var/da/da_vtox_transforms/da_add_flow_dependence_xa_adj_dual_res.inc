subroutine da_add_flow_dependence_xa_adj_dual_res (ne, ep, grid, vp)

   !-----------------------------------------------------------------------
   ! Purpose: Add flow-dependent increments in model space (xa).
   !-----------------------------------------------------------------------
                                      
   implicit none
   
   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (domain), intent(in)            :: grid  ! Analysis increments.
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.
   
   integer                              :: i, j, k, n ! Loop counters.
   real                                 :: alpha       ! Local alpha copy.

   real, allocatable, dimension(:,:,:)  :: output_u,output_v,output_t,output_q, output_p
   real, allocatable, dimension(:,:,:)  :: input_u,input_v,input_t,input_q,input_p
   
   integer  :: cids, cide, ckds, ckde, cjds, cjde, &
               cims, cime, ckms, ckme, cjms, cjme, &
               cips, cipe, ckps, ckpe, cjps, cjpe, &
               nids, nide, nkds, nkde, njds, njde, &
               nims, nime, nkms, nkme, njms, njme, &
               nips, nipe, nkps, nkpe, njps, njpe

   ! HALO STUFF
   integer :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
   integer :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
   logical, external :: rsl_comm_iter


   ! Get coarse (ensemble) grid dimensions ( grid%intermediate_gridd)
   CALL get_ijk_from_grid (  grid%intermediate_grid ,               &
                             cids, cide, cjds, cjde, ckds, ckde,    &
                             cims, cime, cjms, cjme, ckms, ckme,    &
                             cips, cipe, cjps, cjpe, ckps, ckpe    )

   ! Get fine (analysis) grid dimensions (grid)
   CALL get_ijk_from_grid (  grid,                                  &
                             nids, nide, njds, njde, nkds, nkde,    &
                             nims, nime, njms, njme, nkms, nkme,    &
                             nips, nipe, njps, njpe, nkps, nkpe   )
 
   !
   ! Allocate and initialize arrays
   !

   ! Input is hi-res domain
   allocate( input_u(nims:nime,nkms:nkme,njms:njme) )
   allocate( input_v(nims:nime,nkms:nkme,njms:njme) )
   allocate( input_t(nims:nime,nkms:nkme,njms:njme) )
   allocate( input_q(nims:nime,nkms:nkme,njms:njme) )
   allocate( input_p(nims:nime,1:1,njms:njme) )

   ! Output is low-res domain
   allocate( output_u(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( output_v(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( output_t(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( output_q(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( output_p(cims:cime,1:1,cjms:cjme) )

   output_u = 0. ; input_u = 0.
   output_v = 0. ; input_v = 0.
   output_t = 0. ; input_t = 0.
   output_q = 0. ; input_q = 0.
   output_p = 0. ; input_p = 0.


   !
   ! Get input (hi-res) data into (x,z,y) order for interpolation
   !

   do j = jte, jts, -1
      do k = kte, kts, -1
         do i = ite, its, -1
            input_u(i,k,j) = grid%xa%u(i,j,k)
            input_v(i,k,j) = grid%xa%v(i,j,k)
            input_t(i,k,j) = grid%xa%t(i,j,k)
            input_q(i,k,j) = grid%xa%q(i,j,k)
         end do
      end do
   end do

   input_p(:,1,:) = grid%xa%psfc(:,:)

   !

!  if ( num_procs .eq. 1) then
!     if ( grid%parent_grid_ratio.eq.3 ) then
!        write(*,*)' CSS xa%t(107,98,21)= ',grid%xa%t(107,98,21)
!     else
!        write(*,*)' CSS xa%t(123,53,21)= ',grid%xa%t(123,53,21)
!     endif
!     write(*,*)' CSS min, max xa%u= ',minval(grid%xa%u),maxval(grid%xa%u) !, minval(input_u), maxval(input_u)
!     write(*,*)' CSS min, max xa%v= ',minval(grid%xa%v),maxval(grid%xa%v) !, minval(input_v), maxval(input_v)
!     write(*,*)' CSS min, max xa%t= ',minval(grid%xa%t),maxval(grid%xa%t) !, minval(input_t), maxval(input_t)
!     write(*,*)' CSS min, max xa%q= ',minval(grid%xa%q),maxval(grid%xa%q) !, minval(input_q), maxval(input_q)
!     write(*,*)' CSS number non zero = ',count( grid%xa%t .ne. 0 )
!     write(*,*)' CSS number zero = ',count( grid%xa%t .eq. 0 )
!  endif


   print *,' CSS istart, jstart = ',grid%i_parent_start, grid%j_parent_start
   print *,'CSS grid%j_start(1), grid%j_end(1), jts_int, jte_int', grid%j_start(1), grid%j_end(1),jts_int,jte_int

   print *, ' CSS, min/max Input U = ',minval(input_u),maxval(input_u)


#ifdef DM_PARALLEL
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!! DO HALO STUFF !!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     print *, 'CSS  calling inc/HALO_INTERP_UP_inline.inc'

   CALL rsl_comm_iter_init(4,njps,njpe)
   DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         0 , njds,njde,njps,njpe, grid%njds, grid%njde, &
                         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   &
                         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))

      CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 0, &
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   &
	   25  &
! + num_moist   &
! + num_dfi_moist   &
! + num_scalar   &
! + num_dfi_scalar   &
! + num_tracer   &
	   , 175, RWORDSIZE, &
	   0, 22, IWORDSIZE, &
	   0, 0, DWORDSIZE, &
	    0,  0, LWORDSIZE, &
	    myproc, ntasks, ntasks_x, ntasks_y,   &
	    nips, nipe, njps, njpe, nkps, MAX(1,1&
      ,nkpe&
!     ,config_flags%num_soil_layers &
!     ,config_flags%num_snso_layers &
!     ,config_flags%num_snow_layers &
!     ,config_flags%lagday &
      ))

	IF ( SIZE(input_u,1)*SIZE(input_u,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_u, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_v,1)*SIZE(input_v,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_v, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_t,1)*SIZE(input_t,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_t, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_q,1)*SIZE(input_q,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_q, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_p,1)*SIZE(input_p,2) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_p(:,1,:), 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 0, 0, DATA_ORDER_XY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, 1, 1,    &
           nims, nime, njms, njme, 1, 1,    &
           nips, nipe, njps, njpe, 1, 1   )
	ENDIF

        CALL RSL_LITE_EXCH_Y ( local_communicator , myproc, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )

	IF ( SIZE(input_u,1)*SIZE(input_u,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_u, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_v,1)*SIZE(input_v,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_v, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_t,1)*SIZE(input_t,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_t, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_q,1)*SIZE(input_q,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_q, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_p,1)*SIZE(input_p,2) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_p(:,1,:), 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 0, 1, DATA_ORDER_XY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, 1, 1,    &
           nims, nime, njms, njme, 1, 1,    &
           nips, nipe, njps, njpe, 1, 1   )
	ENDIF

      ENDDO

 
   CALL rsl_comm_iter_init(4,nips,nipe)
   DO WHILE ( rsl_comm_iter( grid%id , grid%is_intermediate, 4 , &
                         1 , nids,nide,nips,nipe, grid%nids, grid%nide, &
                         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   &
                         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))

      CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 1, &
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   &
	   25  &
! + num_moist   &
! + num_dfi_moist   &
! + num_scalar   &
! + num_dfi_scalar   &
! + num_tracer   &
	   , 175, RWORDSIZE, &
	   0, 22, IWORDSIZE, &
	   0, 0, DWORDSIZE, &
	    0,  0, LWORDSIZE, &
	    myproc, ntasks, ntasks_x, ntasks_y,   &
	    nips, nipe, njps, njpe, nkps, MAX(1,1&
      ,nkpe&
!     ,config_flags%num_soil_layers &
!     ,config_flags%num_snso_layers &
!     ,config_flags%num_snow_layers &
!     ,config_flags%lagday &
      ))

	IF ( SIZE(input_u,1)*SIZE(input_u,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_u, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_v,1)*SIZE(input_v,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_v, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_t,1)*SIZE(input_t,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_t, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_q,1)*SIZE(input_q,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_q, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_p,1)*SIZE(input_p,2) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_p(:,1,:), 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 1, 0, DATA_ORDER_XY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, 1, 1,    &
           nims, nime, njms, njme, 1, 1,    &
           nips, nipe, njps, njpe, 1, 1   )
	ENDIF


      CALL RSL_LITE_EXCH_X ( local_communicator , myproc, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )


	IF ( SIZE(input_u,1)*SIZE(input_u,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_u, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_v,1)*SIZE(input_v,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_v, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_t,1)*SIZE(input_t,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_t, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_q,1)*SIZE(input_q,3) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_q, 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, nkds, nkde,    &
           nims, nime, njms, njme, nkms, nkme,    &
           nips, nipe, njps, njpe, nkps, nkpe   )
	ENDIF
	IF ( SIZE(input_p,1)*SIZE(input_p,2) .GT. 1 ) THEN
	   CALL RSL_LITE_PACK ( local_communicator,&
	   input_p(:,1,:), 4,&
	   rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
	   rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
	   RWORDSIZE, 1, 1, DATA_ORDER_XY, 0, &
	   myproc, ntasks, ntasks_x, ntasks_y,       &
           nids, nide, njds, njde, 1, 1,    &
           nims, nime, njms, njme, 1, 1,    &
           nips, nipe, njps, njpe, 1, 1   )
	ENDIF

   ENDDO

   !!!!!!!!! END HALO STUFF !!!!!!!!!!!!!!!!!!!!!!!!!!!
#endif


   !
   ! Now interpolate grid%xa from  hi-res to low-res
   !  Output is first entry in copy_fcn/copy_fcnm subroutine, and this is the low-resolution grid%xa in (x,z,y) order
   !

   if ( dual_res_upscale_opt .eq. 1 ) then

      ! U 
      CALL copy_fcnm(  &
                  output_u,   &       ! CD field ... intermediate grid
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  input_u,  &   ! ND field ... fine grid
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  shw(0), grid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  grid%i_parent_start, grid%j_parent_start,                     &
                  grid%parent_grid_ratio, grid%parent_grid_ratio                &
                  )


      ! V 
      CALL copy_fcnm (  &
                  output_v,   &       ! CD field ... intermediate grid
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  input_v,  &   ! ND field ... fine grid
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  shw(0), grid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  grid%i_parent_start, grid%j_parent_start,                     &
                  grid%parent_grid_ratio, grid%parent_grid_ratio                &
                  )


      ! T 
      CALL copy_fcnm (  &
                  output_t,   &       ! CD field ... intermediate grid
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  input_t,  &   ! ND field ... fine grid
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  shw(0), grid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  grid%i_parent_start, grid%j_parent_start,                     &
                  grid%parent_grid_ratio, grid%parent_grid_ratio                &
                  )


      ! Q 
      CALL copy_fcnm (  &
                  output_q,   &       ! CD field ... intermediate grid
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  input_q,  &   ! ND field ... fine grid
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  shw(0), grid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  grid%i_parent_start, grid%j_parent_start,                     &
                  grid%parent_grid_ratio, grid%parent_grid_ratio                &
                  )



      ! P 
      CALL copy_fcnm (  &
                  output_p,   &       ! CD field ... intermediate grid
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  input_p,  &   ! ND field ... fine grid
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         
                  shw(0), grid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  grid%i_parent_start, grid%j_parent_start,                     &
                  grid%parent_grid_ratio, grid%parent_grid_ratio                &
                  )
 
   else if ( dual_res_upscale_opt .eq. 2 ) then
      ! U 

      CALL copy_fcn(  &
                  output_u,   &       ! CD field ... intermediate grid
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  input_u,  &   ! ND field ... fine grid
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  shw(0), grid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  grid%i_parent_start, grid%j_parent_start,                     &
                  grid%parent_grid_ratio, grid%parent_grid_ratio                &
                  )


      ! V 
      CALL copy_fcn (  &
                  output_v,   &       ! CD field ... intermediate grid
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  input_v,  &   ! ND field ... fine grid
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  shw(0), grid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  grid%i_parent_start, grid%j_parent_start,                     &
                  grid%parent_grid_ratio, grid%parent_grid_ratio                &
                  )


      ! T 
      CALL copy_fcn (  &
                  output_t,   &       ! CD field ... intermediate grid
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  input_t,  &   ! ND field ... fine grid
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  shw(0), grid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  grid%i_parent_start, grid%j_parent_start,                     &
                  grid%parent_grid_ratio, grid%parent_grid_ratio                &
                  )


      ! Q 
      CALL copy_fcn (  &
                  output_q,   &       ! CD field ... intermediate grid
                 cids, cide, ckds, ckde, cjds, cjde,   &         ! CD dims
                 cims, cime, ckms, ckme, cjms, cjme,   &         ! CD dims
                 cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe,   &         
                  input_q,  &   ! ND field ... fine grid
                 nids, nide, nkds, nkde, njds, njde,   &         ! ND dims
                 nims, nime, nkms, nkme, njms, njme,   &         ! ND dims
                 nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe,   &         
                  shw(0), grid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  grid%i_parent_start, grid%j_parent_start,                     &
                  grid%parent_grid_ratio, grid%parent_grid_ratio                &
                  )



      ! P 
      CALL copy_fcn (  &
                  output_p,   &       ! CD field ... intermediate grid
                 cids, cide, 1, 1, cjds, cjde,   &         ! CD dims
                 cims, cime, 1, 1, cjms, cjme,   &         ! CD dims
                 cips, cipe, 1, 1, cjps, cjpe,   &         
                  input_p,  &   ! ND field ... fine grid
                 nids, nide, 1, 1, njds, njde,   &         ! ND dims
                 nims, nime, 1, 1, njms, njme,   &         ! ND dims
                 nips, nipe, 1, 1, njps, njpe,   &         
                  shw(0), grid%imask_nostag,         &         ! stencil half width
                  .FALSE., .FALSE.,                                                &         ! xstag, ystag
                  grid%i_parent_start, grid%j_parent_start,                     &
                  grid%parent_grid_ratio, grid%parent_grid_ratio                &
                  )
   else if ( dual_res_upscale_opt .eq. 3 ) then

      write(*,*) ' CSS using adjoint of bilinear interpolation'
      do n=1,total_here
         do k = kts,kte
	    output_u(ob_locs(n)%i  ,k,ob_locs(n)%j)   = ob_locs(n)%dym * ob_locs(n)%dxm  * input_u( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_u(ob_locs(n)%i  ,k,ob_locs(n)%j)   
	    output_u(ob_locs(n)%i+1,k,ob_locs(n)%j)   = ob_locs(n)%dym * ob_locs(n)%dx   * input_u( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_u(ob_locs(n)%i+1,k,ob_locs(n)%j)   
	    output_u(ob_locs(n)%i  ,k,ob_locs(n)%j+1) = ob_locs(n)%dy  * ob_locs(n)%dxm  * input_u( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_u(ob_locs(n)%i  ,k,ob_locs(n)%j+1) 
	    output_u(ob_locs(n)%i+1,k,ob_locs(n)%j+1) = ob_locs(n)%dy  * ob_locs(n)%dx   * input_u( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_u(ob_locs(n)%i+1,k,ob_locs(n)%j+1) 

	    output_v(ob_locs(n)%i  ,k,ob_locs(n)%j)   = ob_locs(n)%dym * ob_locs(n)%dxm  * input_v( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_v(ob_locs(n)%i  ,k,ob_locs(n)%j)   
	    output_v(ob_locs(n)%i+1,k,ob_locs(n)%j)   = ob_locs(n)%dym * ob_locs(n)%dx   * input_v( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_v(ob_locs(n)%i+1,k,ob_locs(n)%j)   
	    output_v(ob_locs(n)%i  ,k,ob_locs(n)%j+1) = ob_locs(n)%dy  * ob_locs(n)%dxm  * input_v( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_v(ob_locs(n)%i  ,k,ob_locs(n)%j+1) 
	    output_v(ob_locs(n)%i+1,k,ob_locs(n)%j+1) = ob_locs(n)%dy  * ob_locs(n)%dx   * input_v( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_v(ob_locs(n)%i+1,k,ob_locs(n)%j+1) 
 
	    output_t(ob_locs(n)%i  ,k,ob_locs(n)%j)   = ob_locs(n)%dym * ob_locs(n)%dxm  * input_t( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_t(ob_locs(n)%i  ,k,ob_locs(n)%j)   
	    output_t(ob_locs(n)%i+1,k,ob_locs(n)%j)   = ob_locs(n)%dym * ob_locs(n)%dx   * input_t( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_t(ob_locs(n)%i+1,k,ob_locs(n)%j)   
	    output_t(ob_locs(n)%i  ,k,ob_locs(n)%j+1) = ob_locs(n)%dy  * ob_locs(n)%dxm  * input_t( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_t(ob_locs(n)%i  ,k,ob_locs(n)%j+1) 
	    output_t(ob_locs(n)%i+1,k,ob_locs(n)%j+1) = ob_locs(n)%dy  * ob_locs(n)%dx   * input_t( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_t(ob_locs(n)%i+1,k,ob_locs(n)%j+1) 

	    output_q(ob_locs(n)%i  ,k,ob_locs(n)%j)   = ob_locs(n)%dym * ob_locs(n)%dxm  * input_q( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_q(ob_locs(n)%i  ,k,ob_locs(n)%j)   
	    output_q(ob_locs(n)%i+1,k,ob_locs(n)%j)   = ob_locs(n)%dym * ob_locs(n)%dx   * input_q( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_q(ob_locs(n)%i+1,k,ob_locs(n)%j)   
	    output_q(ob_locs(n)%i  ,k,ob_locs(n)%j+1) = ob_locs(n)%dy  * ob_locs(n)%dxm  * input_q( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_q(ob_locs(n)%i  ,k,ob_locs(n)%j+1) 
	    output_q(ob_locs(n)%i+1,k,ob_locs(n)%j+1) = ob_locs(n)%dy  * ob_locs(n)%dx   * input_q( ob_locs(n)%xx,k,ob_locs(n)%yy) + output_q(ob_locs(n)%i+1,k,ob_locs(n)%j+1) 
	 end do

	 output_p(ob_locs(n)%i  ,1,ob_locs(n)%j)   = ob_locs(n)%dym * ob_locs(n)%dxm  * input_p( ob_locs(n)%xx,1,ob_locs(n)%yy) + output_p(ob_locs(n)%i  ,1,ob_locs(n)%j)   
	 output_p(ob_locs(n)%i+1,1,ob_locs(n)%j)   = ob_locs(n)%dym * ob_locs(n)%dx   * input_p( ob_locs(n)%xx,1,ob_locs(n)%yy) + output_p(ob_locs(n)%i+1,1,ob_locs(n)%j)   
	 output_p(ob_locs(n)%i  ,1,ob_locs(n)%j+1) = ob_locs(n)%dy  * ob_locs(n)%dxm  * input_p( ob_locs(n)%xx,1,ob_locs(n)%yy) + output_p(ob_locs(n)%i  ,1,ob_locs(n)%j+1) 
	 output_p(ob_locs(n)%i+1,1,ob_locs(n)%j+1) = ob_locs(n)%dy  * ob_locs(n)%dx   * input_p( ob_locs(n)%xx,1,ob_locs(n)%yy) + output_p(ob_locs(n)%i+1,1,ob_locs(n)%j+1) 

      end do

   else 

      print *, ' CSS invalid value of  dual_res_upscale_opt'
      print *, ' Should be 1, 2, or 3. Stop'
      stop

   endif

   print *, ' CSS, min/max Input U = ',minval(input_u),maxval(input_u)
   print *, ' CSS, min/max Output U = ',minval(output_u),maxval(output_u)
   print *, ' CSS, min/max V Input = ',minval(input_v),maxval(input_v)
   print *, ' CSS, min/max V Output = ',minval(output_v),maxval(output_v)
   print *, ' CSS, min/max T Input = ',minval(input_t),maxval(input_t)
   print *, ' CSS, min/max T Output = ',minval(output_t),maxval(output_t)
   print *, ' CSS, min/max Q Input = ',minval(input_q),maxval(input_q)
   print *, ' CSS, min/max Q Output = ',minval(output_q),maxval(output_q)
   print *, ' CSS, min/max P Input = ',minval(input_p),maxval(input_p)
   print *, ' CSS, min/max P Output = ',minval(output_p),maxval(output_p)

    !
    ! Smooth things here if you want...call sm121 from interp_fcn.F. Input is similar to copy_fcn
    !    For dual-res application, smoothing is probably not necessary
    !
    !Input and output is first entry

! SUBROUTINE sm121 ( output_p , &
!                     ids_int, ide_int + 1, kds_int, kde_int + 1, jds_int, jde_int + 1,   &         ! CD dims
!                     ims_int, ime_int, kms_int, kme_int, jms_int, jme_int,   &         ! CD dims
!                     its_int, ite_int, kts_int, kte_int, jts_int, jte_int,   &         ! CD dims
!                     .FALSE., .FALSE.,                     &  ! staggering of field
!                    ids, ide, kds, kde, jds, jde,   &
!                     ims, ime, kms, kme, jms, jme,   &
!                     its, ite, kts, kte, jts, jte,   &
!                     grid%i_parent_start, grid%j_parent_start, &
!                     grid%parent_grid_ratio, grid%parent_grid_ratio &  ! Position of lower left of nest in 
!                     )



   ! 
   ! Now compute alpha on the low-res domain
   !

   vp % alpha = 0.0

   do n = ne, 1, -1
      ! ps:
      vp % alpha(its_int:ite_int,jts_int:jte_int,1,n) = vp % alpha(its_int:ite_int,jts_int:jte_int,1,n) + &
                                        ep % v5(its_int:ite_int,jts_int:jte_int,1,n) * & ! v5 = ps
                                       output_p(its_int:ite_int,1,jts_int:jte_int)
   end do


   do j = jte_int, jts_int, -1
      do k = kte_int, kts_int, -1
         do i = ite_int, its_int, -1

            do n = ne, 1, -1
               alpha = 0.0
               alpha = alpha + ep % v4(i,j,k,n) * output_q(i,k,j)
               alpha = alpha + ep % v3(i,j,k,n) * output_t(i,k,j)
               alpha = alpha + ep % v2(i,j,k,n) * output_v(i,k,j)
               alpha = alpha + ep % v1(i,j,k,n) * output_u(i,k,j)
               vp % alpha(i,j,k,n) = vp % alpha(i,j,k,n) + alpha
            end do
         end do
      end do
   end do

!  if ( num_procs .eq. 1) then
!     write(*,*)' CSS vp%alpha(i,j,k,n) = ',vp%alpha(123,53,21,2),vp%alpha(123,53,21,22)
!     write(*,*)' CSS min, max vp%alpha(i,j,k,n) = ',minval(vp%alpha),maxval(vp%alpha)
!  endif

   ! Clean-up
   deallocate(output_u,output_v,output_t,output_q,output_p)
   deallocate(input_u,input_v,input_t,input_q,input_p)

   if (trace_use) call da_trace_exit("da_add_flow_dependence_xa_adj_dual_res")

end subroutine da_add_flow_dependence_xa_adj_dual_res

