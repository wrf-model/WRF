subroutine da_get_innov_vector_synop( it,num_qcstat_conv, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   integer,       intent(in)    :: it      ! External iteration.
   type(domain),  intent(in)    :: grid    ! model state
   type(y_type),  intent(inout) :: ob      ! Observation structure.
   type(iv_type), intent(inout) :: iv      ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)


   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.

   real    :: v_h(kms:kme)      ! Model value h at ob hor. loc
   real    :: v_p(kms:kme)      ! Model value p at ob hor. loc

   real    :: hd, psfcm

   real    :: ho, to, qo
   real    :: speed, direction
   real    :: rh_error, es, qs, rho
   real    :: xcorr
   real    :: err_inflate_fac

   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_t(:,:)
   real, allocatable :: model_q(:,:)
   real, allocatable :: model_p(:,:)
   real, allocatable :: model_psfc(:,:)
   real, allocatable :: model_hsm(:,:)
   real, allocatable :: model_qs(:)

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_synop")

   allocate (model_u  (1,iv%info(synop)%n1:iv%info(synop)%n2))
   allocate (model_v  (1,iv%info(synop)%n1:iv%info(synop)%n2))
   allocate (model_t  (1,iv%info(synop)%n1:iv%info(synop)%n2))
   allocate (model_q  (1,iv%info(synop)%n1:iv%info(synop)%n2))
   allocate (model_p  (1,iv%info(synop)%n1:iv%info(synop)%n2))
   allocate (model_psfc(1,iv%info(synop)%n1:iv%info(synop)%n2))
   allocate (model_hsm(1,iv%info(synop)%n1:iv%info(synop)%n2))

   if ( it > 1 ) then
      do n=iv%info(synop)%n1,iv%info(synop)%n2
         if (iv%synop(n)%u%qc == fails_error_max) iv%synop(n)%u%qc = 0
         if (iv%synop(n)%v%qc == fails_error_max) iv%synop(n)%v%qc = 0
         if (iv%synop(n)%t%qc == fails_error_max) iv%synop(n)%t%qc = 0
         if (iv%synop(n)%p%qc == fails_error_max) iv%synop(n)%p%qc = 0
         if (iv%synop(n)%q%qc == fails_error_max) iv%synop(n)%q%qc = 0
      end do
   end if

   if (sfc_assi_options == sfc_assi_options_1) then

      do n=iv%info(synop)%n1,iv%info(synop)%n2

         if ( it == 1 .and. sfc_hori_intp_options == 2 ) then
            ! choose the nearest model point with matching land/ocean type and
            ! smallest height difference with the ob
            ! dx, dy, dxm, dym are reset in da_sfc_hori_interp_weights
            call da_sfc_hori_interp_weights(n, iv%info(synop), iv%synop(n), grid%xb, 1)
         end if

         ! [1.1] Get horizontal interpolation weights:
         i   = iv%info(synop)%i(1,n)
         j   = iv%info(synop)%j(1,n)
         dx  = iv%info(synop)%dx(1,n)
         dy  = iv%info(synop)%dy(1,n)
         dxm = iv%info(synop)%dxm(1,n)
         dym = iv%info(synop)%dym(1,n)

         ! Surface correction

         iv%synop(n)%p%inv = ob%synop(n)%p
         iv%synop(n)%t%inv = ob%synop(n)%t
         iv%synop(n)%q%inv = ob%synop(n)%q
         iv%synop(n)%u%inv = ob%synop(n)%u
         iv%synop(n)%v%inv = ob%synop(n)%v

         if ( it == 1 .and. sfc_hori_intp_options == 2 ) then
            ! some obs might have been rejected in da_sfc_hori_interp_weights
            ! due to mismatched land/ocean type
            if ( iv%synop(n)%p%qc < 0 ) iv%synop(n)%p%inv = missing_r
            if ( iv%synop(n)%t%qc < 0 ) iv%synop(n)%t%inv = missing_r
            if ( iv%synop(n)%q%qc < 0 ) iv%synop(n)%q%inv = missing_r
            if ( iv%synop(n)%u%qc < 0 ) iv%synop(n)%u%inv = missing_r
            if ( iv%synop(n)%v%qc < 0 ) iv%synop(n)%v%inv = missing_r
         end if

         if (iv % synop(n) % h > missing_r) then
            do k=kts,kte
               v_h(k) = dym*(dxm*grid%xb%h(i,j  ,k) + dx*grid%xb%h(i+1,j  ,k)) &
                       + dy *(dxm*grid%xb%h(i,j+1,k) + dx*grid%xb%h(i+1,j+1,k))
            end do

            hd = v_h(kts) - iv % synop(n) % h
            if (abs(hd) <= Max_StHeight_Diff .or.  anal_type_verify ) then
               if (iv % synop(n) % h < v_h(kts)) then
                  iv%info(synop)%zk(:,n) = 1.0+1.0e-6
                  call da_obs_sfc_correction(iv%info(synop), iv%synop(n), n, grid%xb)

                  if ( sfcht_adjust_q ) then
                     ! calculate RH with original t, p, q
                     if ( abs(ob%synop(n)%q-missing_r) > 1.0 ) then
                        if ( abs(ob%synop(n)%p-missing_r) > 1.0 .and. &
                             abs(ob%synop(n)%t-missing_r) > 1.0 ) then
                           call da_tpq_to_rh(ob%synop(n)%t, ob%synop(n)%p, ob%synop(n)%q, es, qs, rho)
                        end if
                     end if
                     ! calculate corrected q with corrected t, p and original RH
                     if ( abs(ob%synop(n)%q-missing_r) > 1.0 ) then
                        if ( abs(iv%synop(n)%p%inv-missing_r) > 1.0 .and. &
                             abs(iv%synop(n)%t%inv-missing_r) > 1.0 ) then
                           call da_tp_to_qs(iv%synop(n)%t%inv, iv%synop(n)%p%inv, es, qs)
                           iv%synop(n)%q%inv = qs * rho * 0.01
                           iv%synop(n)%q%qc  = surface_correction
                        end if
                    end if
                  end if !sfcht_adjust_q

               else
                  call da_to_zk(iv % synop(n) % h, v_h, v_interp_h, iv%info(synop)%zk(1,n))
               end if
            end if

            if ( (it == 1) .and. (obs_err_inflate) .and. (abs(hd) <= Max_StHeight_Diff) ) then
               err_inflate_fac = MIN(exp(abs(hd)/stn_ht_diff_scale),100.)
               !iv%synop(n)%u%error = iv%synop(n)%u%error * err_inflate_fac
               !iv%synop(n)%v%error = iv%synop(n)%v%error * err_inflate_fac
               iv%synop(n)%t%error = iv%synop(n)%t%error * err_inflate_fac
               iv%synop(n)%p%error = iv%synop(n)%p%error * err_inflate_fac
               iv%synop(n)%q%error = iv%synop(n)%q%error * err_inflate_fac
            end if

         else if (ob % synop(n) % p > 1.0) then
            do k=kts,kte
              v_p(k) = dym*(dxm*grid%xb%p(i,j  ,k) + dx*grid%xb%p(i+1,j  ,k)) &
                       + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
            end do

            call da_to_zk(ob % synop(n) % p, v_p, v_interp_p, iv%info(synop)%zk(1,n))
            if (iv%info(synop)%zk(1,n) < 0.0 .and. .not.anal_type_verify ) then
               iv % synop(n) % p % inv = missing_r
               iv % synop(n) % p % qc  = missing_data
               iv%info(synop)%zk(:,n) = 1.0+1.0e-6
            end if
         end if
      end do

      call da_convert_zk (iv%info(synop))

      if (.not.anal_type_verify ) then
         do n=iv%info(synop)%n1,iv%info(synop)%n2
            if (iv%info(synop)%zk(1,n) < 0.0) then
               iv % synop(n) % u % qc = missing_data
               iv % synop(n) % v % qc = missing_data
               iv % synop(n) % t % qc = missing_data
               iv % synop(n) % q % qc = missing_data
               iv % synop(n) % p % qc = missing_data
            end if
         end do
      end if

      ! [1.2] Interpolate horizontally:
#ifdef A2C
      call da_interp_lin_3d (grid%xb%u, iv%info(synop), model_u,'u')
      call da_interp_lin_3d (grid%xb%v, iv%info(synop), model_v,'v')
#else
      call da_interp_lin_3d (grid%xb%u, iv%info(synop), model_u)
      call da_interp_lin_3d (grid%xb%v, iv%info(synop), model_v)
#endif
      call da_interp_lin_3d (grid%xb%t, iv%info(synop), model_t)
      call da_interp_lin_3d (grid%xb%q, iv%info(synop), model_q)
      call da_interp_lin_3d (grid%xb%p, iv%info(synop), model_p)
      call da_interp_lin_2d (grid%xb%psfc, iv%info(synop), 1, model_psfc(1,:))
      call da_interp_lin_2d (grid%xb%terr, iv%info(synop), 1, model_hsm(1,:))

      if ( it == 1 .and. q_error_options == 2 ) then
         allocate (model_qs(iv%info(synop)%n1:iv%info(synop)%n2))
         do n=iv%info(synop)%n1,iv%info(synop)%n2
           if ( abs(ob%synop(n)%q-missing_r) > 1.0 ) then
               call da_tp_to_qs(model_t(1,n), model_p(1,n), es, model_qs(n))
               rh_error = iv%synop(n)%q%error !q error is rh at this stage
               iv%synop(n)%q%error = model_qs(n)*rh_error*0.01
           else
               iv%synop(n)%q%error = missing_r
               iv%synop(n)%q%qc    = missing_data
           end if
         end do
         deallocate (model_qs)
      end if

   else if (sfc_assi_options == sfc_assi_options_2) then
      ! Surface data assimilation approach 2
      !------------------------------------

      ! 1.2.1 Surface assimilation approach 2(10-m u, v, 2-m t, q, and sfc_p)

      call da_interp_lin_2d (grid%xb%u10,  iv%info(synop), 1, model_u(1,:))
      call da_interp_lin_2d (grid%xb%v10,  iv%info(synop), 1, model_v(1,:))
      call da_interp_lin_2d (grid%xb%t2,   iv%info(synop), 1, model_t(1,:))
      call da_interp_lin_2d (grid%xb%q2,   iv%info(synop), 1, model_q(1,:))
      call da_interp_lin_2d (grid%xb%psfc, iv%info(synop), 1, model_p(1,:))
      call da_interp_lin_2d (grid%xb%terr, iv%info(synop), 1, model_hsm(1,:))

      do n=iv%info(synop)%n1,iv%info(synop)%n2

         iv%synop(n)%p%inv = ob%synop(n)%p
         iv%synop(n)%t%inv = ob%synop(n)%t
         iv%synop(n)%q%inv = ob%synop(n)%q
         iv%synop(n)%u%inv = ob%synop(n)%u
         iv%synop(n)%v%inv = ob%synop(n)%v

         if (iv%synop(n)%p%qc >= 0) then
            ho = iv%synop(n)%h
            to = -888888.0
            qo = -888888.0

            if (iv%synop(n)%t%qc >= 0 .and. iv%synop(n)%q%qc >= 0) then
               to = ob%synop(n)%t
               qo = ob%synop(n)%q
               call da_sfc_pre (psfcm, model_p(1,n), model_t(1,n), model_q(1,n), model_hsm(1,n), ho, to, qo)
            else if (iv%synop(n)%t%qc >= 0 .and. iv%synop(n)%q%qc < 0) then
               to = ob%synop(n)%t
               call da_sfc_pre (psfcm, model_p(1,n), model_t(1,n), model_q(1,n), model_hsm(1,n), ho, to)
            else
               call da_sfc_pre (psfcm, model_p(1,n), model_t(1,n), model_q(1,n), model_hsm(1,n), ho)
            end if

            ! Pressure at the observed height:
            model_p(1,n) = psfcm
         end if
      end do
   end if

   do n=iv%info(synop)%n1,iv%info(synop)%n2

      !--------------------------------------------------------------------
      !     [3.0] Fast interpolation:
      !--------------------------------------------------------------------
      if(wind_sd_synop)then
         call da_ffdduv_model (speed,direction,model_u(1,n), model_v(1,n), convert_uv2fd)

         if (ob%synop(n)%u > missing_r .AND. iv%synop(n)%u%qc >= obs_qc_pointer) then
             iv%synop(n)%u%inv = iv%synop(n)%u%inv - speed
         else
             iv % synop(n) % u % inv = 0.0
         end if

         if (ob%synop(n)%v > missing_r .AND. iv%synop(n)%v%qc >= obs_qc_pointer) then
             iv%synop(n)%v%inv = iv%synop(n)%v%inv - direction
             if (iv%synop(n)%v%inv > 180.0 ) iv%synop(n)%v%inv = iv%synop(n)%v%inv - 360.0
             if (iv%synop(n)%v%inv < -180.0 ) iv%synop(n)%v%inv = iv%synop(n)%v%inv + 360.0
         else
             iv % synop(n) % v % inv = 0.0
         end if
      else
         if (ob % synop(n) % u > missing_r .AND. iv % synop(n) % u % qc >= obs_qc_pointer) then
             iv % synop(n) % u % inv = iv%synop(n)%u%inv - model_u(1,n)
         else
             iv % synop(n) % u % inv = 0.0
         end if

         if (ob % synop(n) % v > missing_r .AND. iv % synop(n) % v % qc >= obs_qc_pointer) then
             iv % synop(n) % v % inv = iv%synop(n)%v%inv - model_v(1,n)
         else
             iv % synop(n) % v % inv = 0.0
         end if
      end if

      !if (ob % synop(n) % p > 0.0 .AND. iv % synop(n) % p % qc >= obs_qc_pointer) then
      if ( iv % synop(n) % p % qc >= obs_qc_pointer ) then
         iv % synop(n) % p % inv = iv%synop(n)%p%inv - model_p(1,n)
      else
         iv % synop(n) % p % inv = 0.0
      end if

      if (ob % synop(n) % t > 0.0 .AND. iv % synop(n) % t % qc >= obs_qc_pointer) then
         iv % synop(n) % t % inv = iv%synop(n)%t%inv - model_t(1,n)
      else
         iv % synop(n) % t % inv = 0.0
      end if

      if (ob % synop(n) % q > 0.0 .AND. iv % synop(n) % q % qc >= obs_qc_pointer) then
         if ( sfcht_adjust_q ) then
            iv % synop(n) % q % inv = iv%synop(n)%q%inv - model_q(1,n)
         else
            iv % synop(n) % q % inv = ob%synop(n)%q - model_q(1,n)
         end if
      else
         iv % synop(n) % q % inv = 0.0
      end if
   end do

   !--------------------------------------------------------------------
   ! [5.0] Perform optional maximum error check:
   !--------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_synop(iv,ob, it,num_qcstat_conv)

   if (check_buddy) call da_check_buddy_synop(iv, ob, grid%dx, it)
 !
   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_t)
   deallocate (model_q)
   deallocate (model_p)
   deallocate (model_psfc)
   deallocate (model_hsm)

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_synop")

end subroutine da_get_innov_vector_synop


