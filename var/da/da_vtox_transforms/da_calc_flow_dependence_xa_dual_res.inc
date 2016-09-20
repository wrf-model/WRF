subroutine da_calc_flow_dependence_xa_dual_res (grid, xa_ens, ne,  ep, vp, nobwin)

   !-----------------------------------------------------------------------
   ! Purpose: calculate flow-dependent increments in model space (xa)
   !          for a certain sub-window for dual-resolution hybrid
   !-----------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)         :: grid
   type (x_subtype), intent(inout)      :: xa_ens
!   type (x_type), intent(inout)         :: xa_ens
   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (vp_type), intent(in)           :: vp  ! CV on grid structure.

   integer                              :: i, j, k, n  ! Loop counters.
   real                                 :: alpha       ! Local alpha copy.
   integer, intent(in), optional        :: nobwin
   integer                              :: iobwin, ii
   real, allocatable, dimension(:,:,:)  :: ens_contrib_u, ens_contrib_v, ens_contrib_t, ens_contrib_q, ens_contrib_p
   real, allocatable, dimension(:,:,:)  :: output_u,output_v,output_t,output_q, output_p
   real, allocatable, dimension(:,:,:)  :: ens_contrib_qcw, ens_contrib_qrn, ens_contrib_qci, ens_contrib_qsn, ens_contrib_qgr
   real, allocatable, dimension(:,:,:)  :: output_qcw, output_qrn, output_qci, output_qsn, output_qgr

   integer  :: thisdomain_max_halo_width, ierr

   integer  :: cids, cide, ckds, ckde, cjds, cjde, &
               cims, cime, ckms, ckme, cjms, cjme, &
               cips, cipe, ckps, ckpe, cjps, cjpe, &
               nids, nide, nkds, nkde, njds, njde, &
               nims, nime, nkms, nkme, njms, njme, &
               nips, nipe, nkps, nkpe, njps, njpe

   integer :: nj, cj, nk, ck, ni, ci ! for testing

#ifdef DM_PARALLEL
   ! HALO STUFF
   integer :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p
   integer :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m
   logical, external :: rsl_comm_iter
#endif

   if (trace_use) call da_trace_entry("da_calc_flow_dependence_xa_dual_res")

   iobwin = 1
   if ( present(nobwin) ) then
      iobwin = nobwin
   end if

   ! Get coarse (ensemble) grid dimensions ( grid%intermediate_grid)
   CALL get_ijk_from_grid (  grid%intermediate_grid ,               &
                             cids, cide, cjds, cjde, ckds, ckde,    &
                             cims, cime, cjms, cjme, ckms, ckme,    &
                             cips, cipe, cjps, cjpe, ckps, ckpe    )

   ! Get fine (analysis) grid dimensions ( grid)
   CALL get_ijk_from_grid (  grid,                                  &
                             nids, nide, njds, njde, nkds, nkde,    &
                             nims, nime, njms, njme, nkms, nkme,    &
                             nips, nipe, njps, njpe, nkps, nkpe )
#ifdef DM_PARALLEL
   CALL get_dm_max_halo_width ( grid%id , thisdomain_max_halo_width ) ! Can omit?
#endif

   ! Input: Ensemble contribution to increment -- low-res domain (x,z,y) order
   allocate( ens_contrib_u(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( ens_contrib_v(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( ens_contrib_t(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( ens_contrib_q(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( ens_contrib_p(cims:cime,1:1,cjms:cjme) )
#ifdef CLOUD_CV
   allocate( ens_contrib_qcw(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( ens_contrib_qrn(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( ens_contrib_qci(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( ens_contrib_qsn(cims:cime,ckms:ckme,cjms:cjme) )
   allocate( ens_contrib_qgr(cims:cime,ckms:ckme,cjms:cjme) )
#endif

   ! Output: Ensemble contribution to increment interpolated to hi-res domain, (x,z,y) order
   allocate( output_u(nims:nime,nkms:nkme,njms:njme) )
   allocate( output_v(nims:nime,nkms:nkme,njms:njme) )
   allocate( output_t(nims:nime,nkms:nkme,njms:njme) )
   allocate( output_q(nims:nime,nkms:nkme,njms:njme) )
   allocate( output_p(nims:nime,1:1,njms:njme) )
#ifdef CLOUD_CV
   allocate( output_qcw(nims:nime,nkms:nkme,njms:njme) )
   allocate( output_qrn(nims:nime,nkms:nkme,njms:njme) )
   allocate( output_qci(nims:nime,nkms:nkme,njms:njme) )
   allocate( output_qsn(nims:nime,nkms:nkme,njms:njme) )
   allocate( output_qgr(nims:nime,nkms:nkme,njms:njme) )
#endif

   ens_contrib_u = 0.
   ens_contrib_v = 0.
   ens_contrib_t = 0.
   ens_contrib_q = 0.
   ens_contrib_p = 0.
#ifdef CLOUD_CV
   ens_contrib_qcw = 0.
   ens_contrib_qrn = 0.
   ens_contrib_qci = 0.
   ens_contrib_qsn = 0.
   ens_contrib_qgr = 0.
#endif

   output_u = 0.
   output_v = 0.
   output_t = 0.
   output_q = 0.
   output_p = 0.
#ifdef CLOUD_CV
   output_qcw = 0.
   output_qrn = 0.
   output_qci = 0.
   output_qsn = 0.
   output_qgr = 0.
#endif

   !
   ! Determine the ensemble contribution to the increment (low-res) and put in (x,z,y) order for interpolation
   !
   do n = 1, ne
      ii = (iobwin-1)*ensdim_alpha + n
      do j = jts_int, jte_int
         do k = kts_int, kte_int
            do i = its_int, ite_int

               alpha = vp % alpha(i,j,k,n)

               ens_contrib_u(i,k,j) = ens_contrib_u(i,k,j) + alpha * ep % v1(i,j,k,ii) ! v1 = u
               ens_contrib_v(i,k,j) = ens_contrib_v(i,k,j) + alpha * ep % v2(i,j,k,ii) ! v2 = v
               ens_contrib_t(i,k,j) = ens_contrib_t(i,k,j) + alpha * ep % v3(i,j,k,ii) ! v3 = t
               ens_contrib_q(i,k,j) = ens_contrib_q(i,k,j) + alpha * ep % v4(i,j,k,ii) ! v4 = q
#ifdef CLOUD_CV
               ens_contrib_qcw(i,k,j) = ens_contrib_qcw(i,k,j) + alpha * ep%cw(i,j,k,ii)
               ens_contrib_qrn(i,k,j) = ens_contrib_qrn(i,k,j) + alpha * ep%rn(i,j,k,ii)
               ens_contrib_qci(i,k,j) = ens_contrib_qci(i,k,j) + alpha * ep%ci(i,j,k,ii)
               ens_contrib_qsn(i,k,j) = ens_contrib_qsn(i,k,j) + alpha * ep%sn(i,j,k,ii)
               ens_contrib_qgr(i,k,j) = ens_contrib_qgr(i,k,j) + alpha * ep%gr(i,j,k,ii)
#endif

            end do !i loop
          end do !k loop
      end do !j loop

      ens_contrib_p(its_int:ite_int,1,jts_int:jte_int) = ens_contrib_p(its_int:ite_int,1,jts_int:jte_int) + &
                                                  vp % alpha(its_int:ite_int,jts_int:jte_int,1,n)  *        &
                                                  ep % v5   (its_int:ite_int,jts_int:jte_int,1,ii)  ! v5 = ps
   end do !n loop

#ifdef DM_PARALLEL
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!! DO HALO STUFF !!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   CALL rsl_comm_iter_init(4,cjps,cjpe)
   DO WHILE ( rsl_comm_iter( grid%intermediate_grid%id , grid%intermediate_grid%is_intermediate, 4 ,            &
                             0 , cjds,cjde,cjps,cjpe, grid%intermediate_grid%njds, grid%intermediate_grid%njde, &
                             rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,                            &
                             rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))

      CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 0,            &
           rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   &
           rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   &
#ifdef CLOUD_CV
           9, 1, RWORDSIZE, &
#else
           4, 1, RWORDSIZE, &
#endif
           0, 0, IWORDSIZE, &
           0, 0, DWORDSIZE, &
           0, 0, LWORDSIZE, &
           myproc, ntasks, ntasks_x, ntasks_y,   &
           cips, cipe, cjps, cjpe, ckps, MAX(1,1,ckpe) )

      IF ( SIZE(ens_contrib_u,1)*SIZE(ens_contrib_u,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_u, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_v,1)*SIZE(ens_contrib_v,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_v, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_t,1)*SIZE(ens_contrib_t,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_t, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_q,1)*SIZE(ens_contrib_q,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_q, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_p,1)*SIZE(ens_contrib_p,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_p(:,1,:), 4,                                &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XY, 0,                      &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, 1, 1,                           &
         cims, cime, cjms, cjme, 1, 1,                           &
         cips, cipe, cjps, cjpe, 1, 1    )
      ENDIF
#ifdef CLOUD_CV
      IF ( SIZE(ens_contrib_qcw,1)*SIZE(ens_contrib_qcw,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qcw, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qrn,1)*SIZE(ens_contrib_qrn,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qrn, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qci,1)*SIZE(ens_contrib_qci,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qci, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qsn,1)*SIZE(ens_contrib_qsn,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qsn, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qgr,1)*SIZE(ens_contrib_qgr,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qgr, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
#endif

     CALL RSL_LITE_EXCH_Y ( local_communicator , myproc, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )

      IF ( SIZE(ens_contrib_u,1)*SIZE(ens_contrib_u,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_u, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_v,1)*SIZE(ens_contrib_v,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_v, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_t,1)*SIZE(ens_contrib_t,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_t, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_q,1)*SIZE(ens_contrib_q,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_q, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_p,1)*SIZE(ens_contrib_p,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_p(:,1,:), 4,                                &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XY, 0,                      &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, 1, 1,                           &
         cims, cime, cjms, cjme, 1, 1,                           &
         cips, cipe, cjps, cjpe, 1, 1    )
      ENDIF
#ifdef CLOUD_CV
      IF ( SIZE(ens_contrib_qcw,1)*SIZE(ens_contrib_qcw,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qcw, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qrn,1)*SIZE(ens_contrib_qrn,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qrn, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qci,1)*SIZE(ens_contrib_qci,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qci, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qsn,1)*SIZE(ens_contrib_qsn,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qsn, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qgr,1)*SIZE(ens_contrib_qgr,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qgr, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
#endif
   END DO


   CALL rsl_comm_iter_init(4,cips,cipe)
   DO WHILE ( rsl_comm_iter( grid%intermediate_grid%id , grid%intermediate_grid%is_intermediate, 4 ,           &
                            1 , cids,cide,cips,cipe, grid%intermediate_grid%nids, grid%intermediate_grid%nide, &
                            rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   &
                            rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))

      CALL RSL_LITE_INIT_EXCH ( local_communicator, 4, 1,            &
           rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   &
           rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   &
#ifdef CLOUD_CV
           9, 1, RWORDSIZE, &
#else
           4, 1, RWORDSIZE, &
#endif
           0, 0, IWORDSIZE, &
           0, 0, DWORDSIZE, &
           0, 0, LWORDSIZE, &
           myproc, ntasks, ntasks_x, ntasks_y,   &
           cips, cipe, cjps, cjpe, ckps, MAX(1,1,ckpe) )

      IF ( SIZE(ens_contrib_u,1)*SIZE(ens_contrib_u,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_u, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_v,1)*SIZE(ens_contrib_v,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_v, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_t,1)*SIZE(ens_contrib_t,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_t, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_q,1)*SIZE(ens_contrib_q,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_q, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_p,1)*SIZE(ens_contrib_p,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_p(:,1,:), 4,                                &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XY, 0,                      &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, 1, 1,                           &
         cims, cime, cjms, cjme, 1, 1,                           &
         cips, cipe, cjps, cjpe, 1, 1    )
      ENDIF
#ifdef CLOUD_CV
      IF ( SIZE(ens_contrib_qcw,1)*SIZE(ens_contrib_qcw,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qcw, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qrn,1)*SIZE(ens_contrib_qrn,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qrn, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qci,1)*SIZE(ens_contrib_qci,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qci, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qsn,1)*SIZE(ens_contrib_qsn,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qsn, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qgr,1)*SIZE(ens_contrib_qgr,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qgr, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
#endif

     CALL RSL_LITE_EXCH_X ( local_communicator , myproc, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )

      IF ( SIZE(ens_contrib_u,1)*SIZE(ens_contrib_u,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_u, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_v,1)*SIZE(ens_contrib_v,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_v, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_t,1)*SIZE(ens_contrib_t,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_t, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_q,1)*SIZE(ens_contrib_q,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_q, 4,                                       &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_p,1)*SIZE(ens_contrib_p,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_p(:,1,:), 4,                                &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XY, 0,                      &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, 1, 1,                           &
         cims, cime, cjms, cjme, 1, 1,                           &
         cips, cipe, cjps, cjpe, 1, 1    )
      ENDIF
   ENDDO
#ifdef CLOUD_CV
      IF ( SIZE(ens_contrib_qcw,1)*SIZE(ens_contrib_qcw,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qcw, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qrn,1)*SIZE(ens_contrib_qrn,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qrn, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qci,1)*SIZE(ens_contrib_qci,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qci, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qsn,1)*SIZE(ens_contrib_qsn,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qsn, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qgr,1)*SIZE(ens_contrib_qgr,3) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qgr, 4,                                     &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XZY, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
#endif

   !!!!!! END HALO STUFF !!!!!!!!!!!!!
#endif

   !------------------------------------------------------------------------------
   ! Now, interpolate the ensemble contributions increment to the high-res grid
   !------------------------------------------------------------------------------

   ! Input is the low-res ensemble contribution to increment in (x,z,y) order
   ! Output is the ensemble contribution to the increment on the hi-res grid in (x,z,y) order

   !call da_message((/"Using adjoint-based interpolation"/))

   !$OMP PARALLEL DO &
   !$OMP PRIVATE (n)
   do n=1,total_here
      do k = kts, kte

         output_u(ob_locs(n)%xx,k,ob_locs(n)%yy) =                                                &
                     ob_locs(n)%dym*(ob_locs(n)%dxm*ens_contrib_u(ob_locs(n)%i,k,ob_locs(n)%j)    &
                   + ob_locs(n)%dx*ens_contrib_u(ob_locs(n)%i+1,k,ob_locs(n)%j))                  &
                   + ob_locs(n)%dy *(ob_locs(n)%dxm*ens_contrib_u(ob_locs(n)%i,k,ob_locs(n)%j+1)  &
                   + ob_locs(n)%dx*ens_contrib_u(ob_locs(n)%i+1,k,ob_locs(n)%j+1))

         output_v(ob_locs(n)%xx,k,ob_locs(n)%yy) =                                                &
                     ob_locs(n)%dym*(ob_locs(n)%dxm*ens_contrib_v(ob_locs(n)%i,k,ob_locs(n)%j)    &
                   + ob_locs(n)%dx*ens_contrib_v(ob_locs(n)%i+1,k,ob_locs(n)%j))                  &
                   + ob_locs(n)%dy *(ob_locs(n)%dxm*ens_contrib_v(ob_locs(n)%i,k,ob_locs(n)%j+1)  &
                   + ob_locs(n)%dx*ens_contrib_v(ob_locs(n)%i+1,k,ob_locs(n)%j+1))

         output_t(ob_locs(n)%xx,k,ob_locs(n)%yy) =                                                &
                     ob_locs(n)%dym*(ob_locs(n)%dxm*ens_contrib_t(ob_locs(n)%i,k,ob_locs(n)%j)    &
                   + ob_locs(n)%dx*ens_contrib_t(ob_locs(n)%i+1,k,ob_locs(n)%j))                  &
                   + ob_locs(n)%dy *(ob_locs(n)%dxm*ens_contrib_t(ob_locs(n)%i,k,ob_locs(n)%j+1)  &
                   + ob_locs(n)%dx*ens_contrib_t(ob_locs(n)%i+1,k,ob_locs(n)%j+1))

         output_q(ob_locs(n)%xx,k,ob_locs(n)%yy) =                                                &
                     ob_locs(n)%dym*(ob_locs(n)%dxm*ens_contrib_q(ob_locs(n)%i,k,ob_locs(n)%j)    &
                   + ob_locs(n)%dx*ens_contrib_q(ob_locs(n)%i+1,k,ob_locs(n)%j))                  &
                   + ob_locs(n)%dy *(ob_locs(n)%dxm*ens_contrib_q(ob_locs(n)%i,k,ob_locs(n)%j+1)  &
                   + ob_locs(n)%dx*ens_contrib_q(ob_locs(n)%i+1,k,ob_locs(n)%j+1))

#ifdef CLOUD_CV
         output_qcw(ob_locs(n)%xx,k,ob_locs(n)%yy) =                                               &
                     ob_locs(n)%dym*(ob_locs(n)%dxm*ens_contrib_qcw(ob_locs(n)%i,k,ob_locs(n)%j)   &
                   + ob_locs(n)%dx*ens_contrib_qcw(ob_locs(n)%i+1,k,ob_locs(n)%j))                 &
                   + ob_locs(n)%dy *(ob_locs(n)%dxm*ens_contrib_qcw(ob_locs(n)%i,k,ob_locs(n)%j+1) &
                   + ob_locs(n)%dx*ens_contrib_qcw(ob_locs(n)%i+1,k,ob_locs(n)%j+1))
         output_qrn(ob_locs(n)%xx,k,ob_locs(n)%yy) =                                               &
                     ob_locs(n)%dym*(ob_locs(n)%dxm*ens_contrib_qrn(ob_locs(n)%i,k,ob_locs(n)%j)   &
                   + ob_locs(n)%dx*ens_contrib_qrn(ob_locs(n)%i+1,k,ob_locs(n)%j))                 &
                   + ob_locs(n)%dy *(ob_locs(n)%dxm*ens_contrib_qrn(ob_locs(n)%i,k,ob_locs(n)%j+1) &
                   + ob_locs(n)%dx*ens_contrib_qrn(ob_locs(n)%i+1,k,ob_locs(n)%j+1))
         output_qci(ob_locs(n)%xx,k,ob_locs(n)%yy) =                                               &
                     ob_locs(n)%dym*(ob_locs(n)%dxm*ens_contrib_qci(ob_locs(n)%i,k,ob_locs(n)%j)   &
                   + ob_locs(n)%dx*ens_contrib_qci(ob_locs(n)%i+1,k,ob_locs(n)%j))                 &
                   + ob_locs(n)%dy *(ob_locs(n)%dxm*ens_contrib_qci(ob_locs(n)%i,k,ob_locs(n)%j+1) &
                   + ob_locs(n)%dx*ens_contrib_qci(ob_locs(n)%i+1,k,ob_locs(n)%j+1))
         output_qsn(ob_locs(n)%xx,k,ob_locs(n)%yy) =                                               &
                     ob_locs(n)%dym*(ob_locs(n)%dxm*ens_contrib_qsn(ob_locs(n)%i,k,ob_locs(n)%j)   &
                   + ob_locs(n)%dx*ens_contrib_qsn(ob_locs(n)%i+1,k,ob_locs(n)%j))                 &
                   + ob_locs(n)%dy *(ob_locs(n)%dxm*ens_contrib_qsn(ob_locs(n)%i,k,ob_locs(n)%j+1) &
                   + ob_locs(n)%dx*ens_contrib_qsn(ob_locs(n)%i+1,k,ob_locs(n)%j+1))
         output_qgr(ob_locs(n)%xx,k,ob_locs(n)%yy) =                                               &
                     ob_locs(n)%dym*(ob_locs(n)%dxm*ens_contrib_qgr(ob_locs(n)%i,k,ob_locs(n)%j)   &
                   + ob_locs(n)%dx*ens_contrib_qgr(ob_locs(n)%i+1,k,ob_locs(n)%j))                 &
                   + ob_locs(n)%dy *(ob_locs(n)%dxm*ens_contrib_qgr(ob_locs(n)%i,k,ob_locs(n)%j+1) &
                   + ob_locs(n)%dx*ens_contrib_qgr(ob_locs(n)%i+1,k,ob_locs(n)%j+1))
#endif

      end do

      output_p(ob_locs(n)%xx,1,ob_locs(n)%yy) = &
                  ob_locs(n)%dym*(ob_locs(n)%dxm*ens_contrib_p(ob_locs(n)%i,1,ob_locs(n)%j)    &
                + ob_locs(n)%dx*ens_contrib_p(ob_locs(n)%i+1,1,ob_locs(n)%j))                  &
                + ob_locs(n)%dy *(ob_locs(n)%dxm*ens_contrib_p(ob_locs(n)%i,1,ob_locs(n)%j+1)  &
                + ob_locs(n)%dx*ens_contrib_p(ob_locs(n)%i+1,1,ob_locs(n)%j+1))

   end do
   !$OMP END PARALLEL DO

   !
   ! Now add the hi-res ensemble contribution to the increment to the static increment.
   !  This forms the total hi-res increment
   !

   do k = kts, kte
      do j = jts, jte
         do i = its, ite

            ! u:
            xa_ens % u(i,j,k) = xa_ens % u(i,j,k) + output_u(i,k,j) ! u

            ! v:
            xa_ens % v(i,j,k) = xa_ens % v(i,j,k) + output_v(i,k,j) ! v

            ! t:
            xa_ens % t(i,j,k) = xa_ens % t(i,j,k) + output_t(i,k,j) ! t

            ! q:
            xa_ens % q(i,j,k) = xa_ens % q(i,j,k) + output_q(i,k,j) ! q

#ifdef CLOUD_CV
            xa_ens%qcw(i,j,k) = xa_ens%qcw(i,j,k) + output_qcw(i,k,j)
            xa_ens%qrn(i,j,k) = xa_ens%qrn(i,j,k) + output_qrn(i,k,j)
            xa_ens%qci(i,j,k) = xa_ens%qci(i,j,k) + output_qci(i,k,j)
            xa_ens%qsn(i,j,k) = xa_ens%qsn(i,j,k) + output_qsn(i,k,j)
            xa_ens%qgr(i,j,k) = xa_ens%qgr(i,j,k) + output_qgr(i,k,j)
#endif

         end do
      end do
   end do

   ! ps:
   xa_ens % psfc(its:ite,jts:jte) = xa_ens % psfc(its:ite,jts:jte) + &
                                    output_p(its:ite,1,jts:jte) ! ps

   !
   ! Clean-up
   !

   deallocate(ens_contrib_u,ens_contrib_v,ens_contrib_t,ens_contrib_q,ens_contrib_p)
   deallocate(output_u,output_v,output_t,output_q,output_p)
#ifdef CLOUD_CV
   deallocate(ens_contrib_qcw,ens_contrib_qrn,ens_contrib_qci,ens_contrib_qsn,ens_contrib_qgr)
   deallocate(output_qcw,output_qrn,output_qci,output_qsn,output_qgr)
#endif

   if (trace_use) call da_trace_exit("da_calc_flow_dependence_xa_dual_res")

end subroutine da_calc_flow_dependence_xa_dual_res

