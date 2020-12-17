subroutine da_calc_flow_dependence_xa_dual_res (grid, xa_ens, ne,  ep, vp, nobwin)

   !-----------------------------------------------------------------------
   ! Purpose: calculate flow-dependent increments in model space (xa)
   !          for a certain sub-window for dual-resolution hybrid
   !-----------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)         :: grid
   type (x_subtype), intent(inout)      :: xa_ens
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

   integer  :: cids, cide, ckds, ckde, cjds, cjde, &
               cims, cime, ckms, ckme, cjms, cjme, &
               cips, cipe, ckps, ckpe, cjps, cjpe, &
               nids, nide, nkds, nkde, njds, njde, &
               nims, nime, nkms, nkme, njms, njme, &
               nips, nipe, nkps, nkpe, njps, njpe

   integer :: nvar
   integer :: stenwidth

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

   ! Input: Ensemble contribution to increment -- low-res domain
   allocate( ens_contrib_u(cims:cime,cjms:cjme,ckms:ckme) )
   allocate( ens_contrib_v(cims:cime,cjms:cjme,ckms:ckme) )
   allocate( ens_contrib_t(cims:cime,cjms:cjme,ckms:ckme) )
   allocate( ens_contrib_q(cims:cime,cjms:cjme,ckms:ckme) )
   allocate( ens_contrib_p(cims:cime,cjms:cjme,1:1) )

   if ( alpha_hydrometeors ) then
      allocate( ens_contrib_qcw(cims:cime,cjms:cjme,ckms:ckme) )
      allocate( ens_contrib_qrn(cims:cime,cjms:cjme,ckms:ckme) )
      allocate( ens_contrib_qci(cims:cime,cjms:cjme,ckms:ckme) )
      allocate( ens_contrib_qsn(cims:cime,cjms:cjme,ckms:ckme) )
      allocate( ens_contrib_qgr(cims:cime,cjms:cjme,ckms:ckme) )
   end if

   ! Output: Ensemble contribution to increment interpolated to hi-res domain
   allocate( output_u(nims:nime,njms:njme,nkms:nkme) )
   allocate( output_v(nims:nime,njms:njme,nkms:nkme) )
   allocate( output_t(nims:nime,njms:njme,nkms:nkme) )
   allocate( output_q(nims:nime,njms:njme,nkms:nkme) )
   allocate( output_p(nims:nime,njms:njme,1:1) )

   if ( alpha_hydrometeors ) then
      allocate( output_qcw(nims:nime,njms:njme,nkms:nkme) )
      allocate( output_qrn(nims:nime,njms:njme,nkms:nkme) )
      allocate( output_qci(nims:nime,njms:njme,nkms:nkme) )
      allocate( output_qsn(nims:nime,njms:njme,nkms:nkme) )
      allocate( output_qgr(nims:nime,njms:njme,nkms:nkme) )
   end if

   ens_contrib_u = 0.
   ens_contrib_v = 0.
   ens_contrib_t = 0.
   ens_contrib_q = 0.
   ens_contrib_p = 0.

   if ( alpha_hydrometeors ) then
      ens_contrib_qcw = 0.
      ens_contrib_qrn = 0.
      ens_contrib_qci = 0.
      ens_contrib_qsn = 0.
      ens_contrib_qgr = 0.
   end if

   output_u = 0.
   output_v = 0.
   output_t = 0.
   output_q = 0.
   output_p = 0.

   if ( alpha_hydrometeors ) then
      output_qcw = 0.
      output_qrn = 0.
      output_qci = 0.
      output_qsn = 0.
      output_qgr = 0.
   end if

   !
   ! Determine the ensemble contribution to the increment (low-res) for interpolation
   !
   do n = 1, ne
      ii = (iobwin-1)*ensdim_alpha + n
      do k = kts_int, kte_int
         do j = jts_int, jte_int
            do i = its_int, ite_int

               alpha = vp % alpha(i,j,k,n)

               ens_contrib_u(i,j,k) = ens_contrib_u(i,j,k) + alpha * ep % v1(i,j,k,ii) ! v1 = u
               ens_contrib_v(i,j,k) = ens_contrib_v(i,j,k) + alpha * ep % v2(i,j,k,ii) ! v2 = v
               ens_contrib_t(i,j,k) = ens_contrib_t(i,j,k) + alpha * ep % v3(i,j,k,ii) ! v3 = t
               ens_contrib_q(i,j,k) = ens_contrib_q(i,j,k) + alpha * ep % v4(i,j,k,ii) ! v4 = q

            end do !i loop
         end do !j loop
      end do !k loop

      ens_contrib_p(its_int:ite_int,jts_int:jte_int,1) = ens_contrib_p(its_int:ite_int,jts_int:jte_int,1) + &
                                                  vp % alpha(its_int:ite_int,jts_int:jte_int,1,n)  *        &
                                                  ep % v5   (its_int:ite_int,jts_int:jte_int,1,ii)  ! v5 = ps
      if ( alpha_hydrometeors ) then
         do k = kts_int, kte_int
            do j = jts_int, jte_int
               do i = its_int, ite_int
                  alpha = vp % alpha(i,j,k,n)
                  ens_contrib_qcw(i,j,k) = ens_contrib_qcw(i,j,k) + alpha * ep%cw(i,j,k,ii)
                  ens_contrib_qrn(i,j,k) = ens_contrib_qrn(i,j,k) + alpha * ep%rn(i,j,k,ii)
                  ens_contrib_qci(i,j,k) = ens_contrib_qci(i,j,k) + alpha * ep%ci(i,j,k,ii)
                  ens_contrib_qsn(i,j,k) = ens_contrib_qsn(i,j,k) + alpha * ep%sn(i,j,k,ii)
                  ens_contrib_qgr(i,j,k) = ens_contrib_qgr(i,j,k) + alpha * ep%gr(i,j,k,ii)
               end do !i loop
            end do !j loop
         end do !k loop
      end if ! alpha_hydrometeors

   end do !n loop

#ifdef DM_PARALLEL
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!! DO HALO STUFF !!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   nvar = 4
   if ( alpha_hydrometeors ) then
      nvar = 9
   end if

   stenwidth = 4

   CALL rsl_comm_iter_init(stenwidth,cjps,cjpe)
   DO WHILE ( rsl_comm_iter( grid%intermediate_grid%id , grid%intermediate_grid%is_intermediate, stenwidth ,    &
                             0 , cjds,cjde,cjps,cjpe, grid%intermediate_grid%njds, grid%intermediate_grid%njde, &
                             rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,                            &
                             rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))

      CALL RSL_LITE_INIT_EXCH ( local_communicator, stenwidth, 0,    &
           rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   &
           rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   &
           nvar, 1, RWORDSIZE, &
           0, 0, IWORDSIZE, &
           0, 0, DWORDSIZE, &
           0, 0, LWORDSIZE, &
           myproc, ntasks, ntasks_x, ntasks_y,   &
           cips, cipe, cjps, cjpe, ckps, ckpe )

      IF ( SIZE(ens_contrib_u,1)*SIZE(ens_contrib_u,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_u, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_v,1)*SIZE(ens_contrib_v,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_v, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_t,1)*SIZE(ens_contrib_t,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_t, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_q,1)*SIZE(ens_contrib_q,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_q, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_p,1)*SIZE(ens_contrib_p,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_p(:,:,1), stenwidth,                        &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XY, 0,                      &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, 1, 1,                           &
         cims, cime, cjms, cjme, 1, 1,                           &
         cips, cipe, cjps, cjpe, 1, 1    )
      ENDIF
if ( alpha_hydrometeors ) then
      IF ( SIZE(ens_contrib_qcw,1)*SIZE(ens_contrib_qcw,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qcw, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qrn,1)*SIZE(ens_contrib_qrn,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qrn, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qci,1)*SIZE(ens_contrib_qci,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qci, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qsn,1)*SIZE(ens_contrib_qsn,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qsn, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qgr,1)*SIZE(ens_contrib_qgr,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qgr, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
end if ! alpha_hydrometeors

     CALL RSL_LITE_EXCH_Y ( local_communicator , myproc, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )

      IF ( SIZE(ens_contrib_u,1)*SIZE(ens_contrib_u,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_u, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_v,1)*SIZE(ens_contrib_v,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_v, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_t,1)*SIZE(ens_contrib_t,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_t, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_q,1)*SIZE(ens_contrib_q,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_q, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_p,1)*SIZE(ens_contrib_p,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_p(:,:,1), stenwidth,                        &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XY, 0,                      &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, 1, 1,                           &
         cims, cime, cjms, cjme, 1, 1,                           &
         cips, cipe, cjps, cjpe, 1, 1    )
      ENDIF
if ( alpha_hydrometeors ) then
      IF ( SIZE(ens_contrib_qcw,1)*SIZE(ens_contrib_qcw,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qcw, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qrn,1)*SIZE(ens_contrib_qrn,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qrn, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qci,1)*SIZE(ens_contrib_qci,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qci, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qsn,1)*SIZE(ens_contrib_qsn,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qsn, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qgr,1)*SIZE(ens_contrib_qgr,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qgr, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 0, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
end if ! alpha_hydrometeors
   END DO

   CALL rsl_comm_iter_init(stenwidth,cips,cipe)
   DO WHILE ( rsl_comm_iter( grid%intermediate_grid%id , grid%intermediate_grid%is_intermediate, stenwidth ,   &
                            1 , cids,cide,cips,cipe, grid%intermediate_grid%nids, grid%intermediate_grid%nide, &
                            rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   &
                            rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))

      CALL RSL_LITE_INIT_EXCH ( local_communicator, stenwidth, 1,    &
           rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   &
           rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   &
           nvar, 1, RWORDSIZE, &
           0, 0, IWORDSIZE, &
           0, 0, DWORDSIZE, &
           0, 0, LWORDSIZE, &
           myproc, ntasks, ntasks_x, ntasks_y,   &
           cips, cipe, cjps, cjpe, ckps, ckpe )

      IF ( SIZE(ens_contrib_u,1)*SIZE(ens_contrib_u,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_u, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_v,1)*SIZE(ens_contrib_v,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_v, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_t,1)*SIZE(ens_contrib_t,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_t, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_q,1)*SIZE(ens_contrib_q,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_q, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_p,1)*SIZE(ens_contrib_p,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_p(:,:,1), stenwidth,                        &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XY, 0,                      &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, 1, 1,                           &
         cims, cime, cjms, cjme, 1, 1,                           &
         cips, cipe, cjps, cjpe, 1, 1    )
      ENDIF
if ( alpha_hydrometeors ) then
      IF ( SIZE(ens_contrib_qcw,1)*SIZE(ens_contrib_qcw,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qcw, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qrn,1)*SIZE(ens_contrib_qrn,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qrn, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qci,1)*SIZE(ens_contrib_qci,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qci, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qsn,1)*SIZE(ens_contrib_qsn,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qsn, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qgr,1)*SIZE(ens_contrib_qgr,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qgr, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 0, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
end if ! alpha_hydrometeors

     CALL RSL_LITE_EXCH_X ( local_communicator , myproc, ntasks, ntasks_x, ntasks_y, &
                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )

      IF ( SIZE(ens_contrib_u,1)*SIZE(ens_contrib_u,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_u, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_v,1)*SIZE(ens_contrib_v,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_v, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_t,1)*SIZE(ens_contrib_t,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_t, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_q,1)*SIZE(ens_contrib_q,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_q, stenwidth,                               &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_p,1)*SIZE(ens_contrib_p,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_p(:,:,1), stenwidth,                        &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XY, 0,                      &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, 1, 1,                           &
         cims, cime, cjms, cjme, 1, 1,                           &
         cips, cipe, cjps, cjpe, 1, 1    )
      ENDIF
if ( alpha_hydrometeors ) then
      IF ( SIZE(ens_contrib_qcw,1)*SIZE(ens_contrib_qcw,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qcw, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qrn,1)*SIZE(ens_contrib_qrn,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qrn, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qci,1)*SIZE(ens_contrib_qci,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qci, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qsn,1)*SIZE(ens_contrib_qsn,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qsn, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
      IF ( SIZE(ens_contrib_qgr,1)*SIZE(ens_contrib_qgr,2) .GT. 1 ) THEN
         CALL RSL_LITE_PACK ( local_communicator,                &
         ens_contrib_qgr, stenwidth,                             &
         rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &
         rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &
         RWORDSIZE, 1, 1, DATA_ORDER_XYZ, 0,                     &
         myproc, ntasks, ntasks_x, ntasks_y,                     &
         cids, cide, cjds, cjde, ckds, ckde,                     &
         cims, cime, cjms, cjme, ckms, ckme,                     &
         cips, cipe, cjps, cjpe, ckps, ckpe    )
      ENDIF
end if ! alpha_hydrometeors
   END DO

   !!!!!! END HALO STUFF !!!!!!!!!!!!!
#endif

   !------------------------------------------------------------------------------
   ! Now, interpolate the ensemble contributions increment to the high-res grid
   !------------------------------------------------------------------------------

   ! Input is the low-res ensemble contribution to increment
   ! Output is the ensemble contribution to the increment on the hi-res grid

   !call da_message((/"Using adjoint-based interpolation"/))

   do k = kts, kte
      do j = jts, jte
         do i = its, ite

            output_u(i,j,k) =                                                                                    &
                     aens_locs(i,j)%dym*(aens_locs(i,j)%dxm*ens_contrib_u(aens_locs(i,j)%i,aens_locs(i,j)%j,k)   &
                   + aens_locs(i,j)%dx*ens_contrib_u(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k))                     &
                   + aens_locs(i,j)%dy *(aens_locs(i,j)%dxm*ens_contrib_u(aens_locs(i,j)%i,aens_locs(i,j)%j+1,k) &
                   + aens_locs(i,j)%dx*ens_contrib_u(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k))

            output_v(i,j,k) =                                                                                    &
                     aens_locs(i,j)%dym*(aens_locs(i,j)%dxm*ens_contrib_v(aens_locs(i,j)%i,aens_locs(i,j)%j,k)   &
                   + aens_locs(i,j)%dx*ens_contrib_v(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k))                     &
                   + aens_locs(i,j)%dy *(aens_locs(i,j)%dxm*ens_contrib_v(aens_locs(i,j)%i,aens_locs(i,j)%j+1,k) &
                   + aens_locs(i,j)%dx*ens_contrib_v(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k))

            output_t(i,j,k) =                                                                                    &
                     aens_locs(i,j)%dym*(aens_locs(i,j)%dxm*ens_contrib_t(aens_locs(i,j)%i,aens_locs(i,j)%j,k)   &
                   + aens_locs(i,j)%dx*ens_contrib_t(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k))                     &
                   + aens_locs(i,j)%dy *(aens_locs(i,j)%dxm*ens_contrib_t(aens_locs(i,j)%i,aens_locs(i,j)%j+1,k) &
                   + aens_locs(i,j)%dx*ens_contrib_t(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k))

            output_q(i,j,k) =                                                                                    &
                     aens_locs(i,j)%dym*(aens_locs(i,j)%dxm*ens_contrib_q(aens_locs(i,j)%i,aens_locs(i,j)%j,k)   &
                   + aens_locs(i,j)%dx*ens_contrib_q(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k))                     &
                   + aens_locs(i,j)%dy *(aens_locs(i,j)%dxm*ens_contrib_q(aens_locs(i,j)%i,aens_locs(i,j)%j+1,k) &
                   + aens_locs(i,j)%dx*ens_contrib_q(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k))

         end do
      end do
   end do

   do j = jts, jte
      do i = its, ite
         output_p(i,j,1) =                                                                                     &
                  aens_locs(i,j)%dym*(aens_locs(i,j)%dxm*ens_contrib_p(aens_locs(i,j)%i,aens_locs(i,j)%j,1)    &
                + aens_locs(i,j)%dx*ens_contrib_p(aens_locs(i,j)%i+1,aens_locs(i,j)%j,1))                      &
                + aens_locs(i,j)%dy *(aens_locs(i,j)%dxm*ens_contrib_p(aens_locs(i,j)%i,aens_locs(i,j)%j+1,1)  &
                + aens_locs(i,j)%dx*ens_contrib_p(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,1))

      end do
   end do

   if ( alpha_hydrometeors ) then
      do k = kts, kte
         do j = jts, jte
            do i = its, ite
               output_qcw(i,j,k) =                                                                                 &
                     aens_locs(i,j)%dym*(aens_locs(i,j)%dxm*ens_contrib_qcw(aens_locs(i,j)%i,aens_locs(i,j)%j,k)   &
                   + aens_locs(i,j)%dx*ens_contrib_qcw(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k))                     &
                   + aens_locs(i,j)%dy *(aens_locs(i,j)%dxm*ens_contrib_qcw(aens_locs(i,j)%i,aens_locs(i,j)%j+1,k) &
                   + aens_locs(i,j)%dx*ens_contrib_qcw(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k))
               output_qrn(i,j,k) =                                                                                 &
                     aens_locs(i,j)%dym*(aens_locs(i,j)%dxm*ens_contrib_qrn(aens_locs(i,j)%i,aens_locs(i,j)%j,k)   &
                   + aens_locs(i,j)%dx*ens_contrib_qrn(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k))                     &
                   + aens_locs(i,j)%dy *(aens_locs(i,j)%dxm*ens_contrib_qrn(aens_locs(i,j)%i,aens_locs(i,j)%j+1,k) &
                   + aens_locs(i,j)%dx*ens_contrib_qrn(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k))
               output_qci(i,j,k) =                                                                                 &
                     aens_locs(i,j)%dym*(aens_locs(i,j)%dxm*ens_contrib_qci(aens_locs(i,j)%i,aens_locs(i,j)%j,k)   &
                   + aens_locs(i,j)%dx*ens_contrib_qci(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k))                     &
                   + aens_locs(i,j)%dy *(aens_locs(i,j)%dxm*ens_contrib_qci(aens_locs(i,j)%i,aens_locs(i,j)%j+1,k) &
                   + aens_locs(i,j)%dx*ens_contrib_qci(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k))
               output_qsn(i,j,k) =                                                                                 &
                     aens_locs(i,j)%dym*(aens_locs(i,j)%dxm*ens_contrib_qsn(aens_locs(i,j)%i,aens_locs(i,j)%j,k)   &
                   + aens_locs(i,j)%dx*ens_contrib_qsn(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k))                     &
                   + aens_locs(i,j)%dy *(aens_locs(i,j)%dxm*ens_contrib_qsn(aens_locs(i,j)%i,aens_locs(i,j)%j+1,k) &
                   + aens_locs(i,j)%dx*ens_contrib_qsn(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k))
               output_qgr(i,j,k) =                                                                                 &
                     aens_locs(i,j)%dym*(aens_locs(i,j)%dxm*ens_contrib_qgr(aens_locs(i,j)%i,aens_locs(i,j)%j,k)   &
                   + aens_locs(i,j)%dx*ens_contrib_qgr(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k))                     &
                   + aens_locs(i,j)%dy *(aens_locs(i,j)%dxm*ens_contrib_qgr(aens_locs(i,j)%i,aens_locs(i,j)%j+1,k) &
                   + aens_locs(i,j)%dx*ens_contrib_qgr(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k))
            end do
         end do
      end do
   end if ! alpha_hydrometeors

   !
   ! Now add the hi-res ensemble contribution to the increment to the static increment.
   !  This forms the total hi-res increment
   !

   do k = kts, kte
      do j = jts, jte
         do i = its, ite

            ! u:
            xa_ens % u(i,j,k) = xa_ens % u(i,j,k) + output_u(i,j,k) ! u

            ! v:
            xa_ens % v(i,j,k) = xa_ens % v(i,j,k) + output_v(i,j,k) ! v

            ! t:
            xa_ens % t(i,j,k) = xa_ens % t(i,j,k) + output_t(i,j,k) ! t

            ! q:
            xa_ens % q(i,j,k) = xa_ens % q(i,j,k) + output_q(i,j,k) ! q

         end do
      end do
   end do

   ! ps:
   xa_ens % psfc(its:ite,jts:jte) = xa_ens % psfc(its:ite,jts:jte) + &
                                    output_p(its:ite,jts:jte,1) ! ps

   if ( alpha_hydrometeors ) then
      do k = kts, kte
         do j = jts, jte
            do i = its, ite
               xa_ens%qcw(i,j,k) = xa_ens%qcw(i,j,k) + output_qcw(i,j,k)
               xa_ens%qrn(i,j,k) = xa_ens%qrn(i,j,k) + output_qrn(i,j,k)
               xa_ens%qci(i,j,k) = xa_ens%qci(i,j,k) + output_qci(i,j,k)
               xa_ens%qsn(i,j,k) = xa_ens%qsn(i,j,k) + output_qsn(i,j,k)
               xa_ens%qgr(i,j,k) = xa_ens%qgr(i,j,k) + output_qgr(i,j,k)
            end do
         end do
      end do
   end if ! alpha_hydrometeors

   !
   ! Clean-up
   !

   deallocate(ens_contrib_u,ens_contrib_v,ens_contrib_t,ens_contrib_q,ens_contrib_p)
   deallocate(output_u,output_v,output_t,output_q,output_p)
   if ( alpha_hydrometeors ) then
      deallocate(ens_contrib_qcw,ens_contrib_qrn,ens_contrib_qci,ens_contrib_qsn,ens_contrib_qgr)
      deallocate(output_qcw,output_qrn,output_qci,output_qsn,output_qgr)
   end if

   if (trace_use) call da_trace_exit("da_calc_flow_dependence_xa_dual_res")

end subroutine da_calc_flow_dependence_xa_dual_res

