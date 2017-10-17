subroutine da_calc_flow_dependence_xa_adj_dual_res (ne, ep, grid, xa, vp, nobwin)

   !-----------------------------------------------------------------------
   ! Purpose: adjoint of calculating flow-dependent increments in model space (xa)
   !          for a certain sub-window for dual-resolution hybrid
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)                  :: ne  ! Ensemble size.
   type (ep_type), intent(in)           :: ep  ! Ensemble perturbations.
   type (domain), intent(in)            :: grid  ! for getting grid dimensions
   type (x_type), intent(inout)         :: xa  ! Analysis increments
   type (vp_type), intent(inout)        :: vp  ! CV on grid structure.

   integer                              :: i, j, k, n ! Loop counters.
   real                                 :: alpha       ! Local alpha copy.
   integer, intent(in), optional        :: nobwin
   integer                              :: iobwin, ii
   real, allocatable, dimension(:,:,:)  :: output_u,output_v,output_t,output_q, output_p
   real, allocatable, dimension(:,:,:)  :: input_u,input_v,input_t,input_q,input_p
   real, allocatable, dimension(:,:,:)  :: output_qcw,output_qrn,output_qci,output_qsn, output_qgr
   real, allocatable, dimension(:,:,:)  :: input_qcw,input_qrn,input_qci,input_qsn,input_qgr

   integer  :: cims, cime, ckms, ckme, cjms, cjme, &
               nims, nime, nkms, nkme, njms, njme
   integer  :: nvar

   if (trace_use) call da_trace_entry("da_calc_flow_dependence_xa_adj_dual_res")

   iobwin = 1
   if ( present(nobwin) ) then
      iobwin = nobwin
   end if

   ! Get coarse (ensemble) grid dimensions ( grid%intermediate_grid)
   cims = ims_int; cime = ime_int
   cjms = jms_int; cjme = jme_int
   ckms = kms_int; ckme = kme_int

   ! Get fine (analysis) grid dimensions (grid)
   nims = ims;     nime = ime
   njms = jms;     njme = jme
   nkms = kms;     nkme = kme

   !
   ! Allocate and initialize arrays
   !

   ! Input is hi-res domain
   allocate( input_u(nims:nime,njms:njme,nkms:nkme) )
   allocate( input_v(nims:nime,njms:njme,nkms:nkme) )
   allocate( input_t(nims:nime,njms:njme,nkms:nkme) )
   allocate( input_q(nims:nime,njms:njme,nkms:nkme) )
   allocate( input_p(nims:nime,njms:njme,1:1) )
   if ( alpha_hydrometeors ) then
      allocate( input_qcw(nims:nime,njms:njme,nkms:nkme) )
      allocate( input_qrn(nims:nime,njms:njme,nkms:nkme) )
      allocate( input_qci(nims:nime,njms:njme,nkms:nkme) )
      allocate( input_qsn(nims:nime,njms:njme,nkms:nkme) )
      allocate( input_qgr(nims:nime,njms:njme,nkms:nkme) )
   end if

   ! Output is low-res domain
   allocate( output_u(cims:cime,cjms:cjme,ckms:ckme) )
   allocate( output_v(cims:cime,cjms:cjme,ckms:ckme) )
   allocate( output_t(cims:cime,cjms:cjme,ckms:ckme) )
   allocate( output_q(cims:cime,cjms:cjme,ckms:ckme) )
   allocate( output_p(cims:cime,cjms:cjme,1:1) )
   if ( alpha_hydrometeors ) then
      allocate( output_qcw(cims:cime,cjms:cjme,ckms:ckme) )
      allocate( output_qrn(cims:cime,cjms:cjme,ckms:ckme) )
      allocate( output_qci(cims:cime,cjms:cjme,ckms:ckme) )
      allocate( output_qsn(cims:cime,cjms:cjme,ckms:ckme) )
      allocate( output_qgr(cims:cime,cjms:cjme,ckms:ckme) )
   end if

   output_u = 0. ; input_u = 0.
   output_v = 0. ; input_v = 0.
   output_t = 0. ; input_t = 0.
   output_q = 0. ; input_q = 0.
   output_p = 0. ; input_p = 0.
   if ( alpha_hydrometeors ) then
      output_qcw = 0. ; input_qcw = 0.
      output_qrn = 0. ; input_qrn = 0.
      output_qci = 0. ; input_qci = 0.
      output_qsn = 0. ; input_qsn = 0.
      output_qgr = 0. ; input_qgr = 0.
   end if

   !
   ! Get input (hi-res) data for interpolation
   !

   do k = kte, kts, -1
      do j = jte, jts, -1
         do i = ite, its, -1
            input_u(i,j,k) = xa%u(i,j,k)
            input_v(i,j,k) = xa%v(i,j,k)
            input_t(i,j,k) = xa%t(i,j,k)
            input_q(i,j,k) = xa%q(i,j,k)
         end do
      end do
   end do

   input_p(:,:,1) = xa%psfc(:,:)

   if ( alpha_hydrometeors ) then
      do k = kte, kts, -1
         do j = jte, jts, -1
            do i = ite, its, -1
               input_qcw(i,j,k) = xa%qcw(i,j,k)
               input_qrn(i,j,k) = xa%qrn(i,j,k)
               input_qci(i,j,k) = xa%qci(i,j,k)
               input_qsn(i,j,k) = xa%qsn(i,j,k)
               input_qgr(i,j,k) = xa%qgr(i,j,k)
            end do
         end do
      end do
   end if ! alpha_hydrometeors

   write (unit=message(1),fmt='(A,2I8)')' istart, jstart = ',grid%i_parent_start, grid%j_parent_start
   call wrf_debug(2, message(1))
   write (unit=message(2),fmt='(A,4I8)')' grid%j_start(1), grid%j_end(1), jts_int, jte_int', grid%j_start(1), grid%j_end(1),jts_int,jte_int
   call wrf_debug(2, message(2))
   write (unit=message(3),fmt='(A,2F12.5)')' min/max  Input U = ',minval(input_u),maxval(input_u)
   call wrf_debug(2, message(3))
   !call da_message(message(1:3))

   !
   ! Now interpolate grid%xa from hi-res to low-res
   !
      !call da_message((/'Using adjoint of bilinear interpolation'/))
   do k = kts,kte
      do j = jts,jte
         do i = its,ite
            output_u(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dxm  * input_u(i,j,k) + output_u(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)
            output_u(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dx   * input_u(i,j,k) + output_u(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)
            output_u(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dxm  * input_u(i,j,k) + output_u(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k)
            output_u(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dx   * input_u(i,j,k) + output_u(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k)

            output_v(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dxm  * input_v(i,j,k) + output_v(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)
            output_v(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dx   * input_v(i,j,k) + output_v(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)
            output_v(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dxm  * input_v(i,j,k) + output_v(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k)
            output_v(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dx   * input_v(i,j,k) + output_v(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k)

            output_t(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dxm  * input_t(i,j,k) + output_t(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)
            output_t(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dx   * input_t(i,j,k) + output_t(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)
            output_t(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dxm  * input_t(i,j,k) + output_t(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k)
            output_t(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dx   * input_t(i,j,k) + output_t(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k)

            output_q(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dxm  * input_q(i,j,k) + output_q(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)
            output_q(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dx   * input_q(i,j,k) + output_q(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)
            output_q(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dxm  * input_q(i,j,k) + output_q(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k)
            output_q(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dx   * input_q(i,j,k) + output_q(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k)
         end do
      end do
   end do

   do j = jts,jte
      do i = its,ite
         output_p(aens_locs(i,j)%i  ,aens_locs(i,j)%j,1)   = aens_locs(i,j)%dym * aens_locs(i,j)%dxm  * input_p(i,j,1) + output_p(aens_locs(i,j)%i  ,aens_locs(i,j)%j,1)
         output_p(aens_locs(i,j)%i+1,aens_locs(i,j)%j,1)   = aens_locs(i,j)%dym * aens_locs(i,j)%dx   * input_p(i,j,1) + output_p(aens_locs(i,j)%i+1,aens_locs(i,j)%j,1)
         output_p(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,1) = aens_locs(i,j)%dy  * aens_locs(i,j)%dxm  * input_p(i,j,1) + output_p(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,1)
         output_p(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,1) = aens_locs(i,j)%dy  * aens_locs(i,j)%dx   * input_p(i,j,1) + output_p(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,1)

      end do
   end do

   if ( alpha_hydrometeors ) then
      do k = kts,kte
         do j = jts,jte
            do i = its,ite
               output_qcw(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dxm  * input_qcw(i,j,k) + output_qcw(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)
               output_qcw(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dx   * input_qcw(i,j,k) + output_qcw(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)
               output_qcw(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dxm  * input_qcw(i,j,k) + output_qcw(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k)
               output_qcw(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dx   * input_qcw(i,j,k) + output_qcw(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k)

               output_qrn(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dxm  * input_qrn(i,j,k) + output_qrn(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)
               output_qrn(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dx   * input_qrn(i,j,k) + output_qrn(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)
               output_qrn(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dxm  * input_qrn(i,j,k) + output_qrn(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k)
               output_qrn(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dx   * input_qrn(i,j,k) + output_qrn(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k)

               output_qci(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dxm  * input_qci(i,j,k) + output_qci(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)
               output_qci(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dx   * input_qci(i,j,k) + output_qci(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)
               output_qci(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dxm  * input_qci(i,j,k) + output_qci(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k)
               output_qci(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dx   * input_qci(i,j,k) + output_qci(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k)

               output_qsn(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dxm  * input_qsn(i,j,k) + output_qsn(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)
               output_qsn(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dx   * input_qsn(i,j,k) + output_qsn(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)
               output_qsn(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dxm  * input_qsn(i,j,k) + output_qsn(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k)
               output_qsn(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dx   * input_qsn(i,j,k) + output_qsn(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k)

               output_qgr(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dxm  * input_qgr(i,j,k) + output_qgr(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)
               output_qgr(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)   = aens_locs(i,j)%dym * aens_locs(i,j)%dx   * input_qgr(i,j,k) + output_qgr(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)
               output_qgr(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dxm  * input_qgr(i,j,k) + output_qgr(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k)
               output_qgr(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k) = aens_locs(i,j)%dy  * aens_locs(i,j)%dx   * input_qgr(i,j,k) + output_qgr(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k)
            end do
         end do
      end do
   end if ! alpha_hydrometeors

   write (unit=message(1), fmt='(A,2F12.5)') ' min/max U  Input = ',minval(input_u),maxval(input_u)
   call wrf_debug(2, message(1))
   write (unit=message(2), fmt='(A,2F12.5)') ' min/max U Output = ',minval(output_u),maxval(output_u)
   call wrf_debug(2, message(2))
   write (unit=message(3), fmt='(A,2F12.5)') ' min/max V  Input = ',minval(input_v),maxval(input_v)
   call wrf_debug(2, message(3))
   write (unit=message(4), fmt='(A,2F12.5)') ' min/max V Output = ',minval(output_v),maxval(output_v)
   call wrf_debug(2, message(4))
   write (unit=message(5), fmt='(A,2F12.5)') ' min/max T  Input = ',minval(input_t),maxval(input_t)
   call wrf_debug(2, message(5))
   write (unit=message(6), fmt='(A,2F12.5)') ' min/max T Output = ',minval(output_t),maxval(output_t)
   call wrf_debug(2, message(6))
   write (unit=message(7), fmt='(A,2F12.5)') ' min/max Q  Input = ',minval(input_q),maxval(input_q)
   call wrf_debug(2, message(7))
   write (unit=message(8), fmt='(A,2F12.5)') ' min/max Q Output = ',minval(output_q),maxval(output_q)
   call wrf_debug(2, message(8))
   write (unit=message(9), fmt='(A,2F12.5)') ' min/max P  Input = ',minval(input_p),maxval(input_p)
   call wrf_debug(2, message(9))
   write (unit=message(10),fmt='(A,2F12.5)') ' min/max P Output = ',minval(output_p),maxval(output_p)
   call wrf_debug(2, message(10))
   !call da_message(message(1:10))

   !
   ! Now compute alpha on the low-res domain
   !

   !done outside this subroutine
   !vp % alpha = 0.0

   do n = ne, 1, -1
      ii = (iobwin-1)*ensdim_alpha + n

      if ( alpha_hydrometeors ) then
         do k = kte_int, kts_int, -1
            do j = jte_int, jts_int, -1
               do i = ite_int, its_int, -1
                  alpha = 0.0
                  alpha = alpha + ep % gr(i,j,k,ii) * output_qgr(i,j,k)
                  alpha = alpha + ep % sn(i,j,k,ii) * output_qsn(i,j,k)
                  alpha = alpha + ep % ci(i,j,k,ii) * output_qci(i,j,k)
                  alpha = alpha + ep % rn(i,j,k,ii) * output_qrn(i,j,k)
                  alpha = alpha + ep % cw(i,j,k,ii) * output_qcw(i,j,k)
                  vp % alpha(i,j,k,n) = vp % alpha(i,j,k,n) + alpha
               end do
            end do
         end do
      end if ! alpha_hydrometeors

      ! ps:
      vp % alpha(its_int:ite_int,jts_int:jte_int,1,n) =      &
           vp % alpha(its_int:ite_int,jts_int:jte_int,1,n) + &
           ep % v5(its_int:ite_int,jts_int:jte_int,1,ii) *   & ! v5 = ps
           output_p(its_int:ite_int,jts_int:jte_int,1)

      do k = kte_int, kts_int, -1
         do j = jte_int, jts_int, -1
            do i = ite_int, its_int, -1

                  alpha = 0.0
                  alpha = alpha + ep % v4(i,j,k,ii) * output_q(i,j,k)
                  alpha = alpha + ep % v3(i,j,k,ii) * output_t(i,j,k)
                  alpha = alpha + ep % v2(i,j,k,ii) * output_v(i,j,k)
                  alpha = alpha + ep % v1(i,j,k,ii) * output_u(i,j,k)

                  vp % alpha(i,j,k,n) = vp % alpha(i,j,k,n) + alpha

            end do
         end do
      end do
   end do !n loop

   ! Clean-up
   deallocate(output_u,output_v,output_t,output_q,output_p)
   deallocate(input_u,input_v,input_t,input_q,input_p)
   if ( alpha_hydrometeors ) then
      deallocate(output_qcw,output_qrn,output_qci,output_qsn,output_qgr)
      deallocate(input_qcw,input_qrn,input_qci,input_qsn,input_qgr)
   end if

   if (trace_use) call da_trace_exit("da_calc_flow_dependence_xa_adj_dual_res")

end subroutine da_calc_flow_dependence_xa_adj_dual_res

