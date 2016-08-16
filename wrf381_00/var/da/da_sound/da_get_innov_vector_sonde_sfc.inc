subroutine da_get_innov_vector_sonde_sfc( it, num_qcstat_conv,grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD    
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(domain),     intent(in)    :: grid      ! first guess state.
   type(y_type),     intent(inout) :: ob      ! Observation structure.
   type(iv_type),    intent(inout) :: iv      ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)


   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real, allocatable :: model_u(:,:)  ! Model value u at oblocation.
   real, allocatable :: model_v(:,:)  ! Model value v at oblocation.
   real, allocatable :: model_t(:,:)  ! Model value t at oblocation.
   real, allocatable :: model_p(:,:)  ! Model value p at oblocation.
   real, allocatable :: model_q(:,:)  ! Model value q at oblocation.
   real, allocatable :: model_hsm(:,:)

   real  :: speed, direction

   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.
   real    :: v_p(kms:kme)      ! Model value p at ob hor. location.

   real    :: hd, psfcm
   real    :: ho, to, qo
   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_sonde_sfc")

   allocate (model_u(1,iv%info(sonde_sfc)%n1:iv%info(sonde_sfc)%n2))
   allocate (model_v(1,iv%info(sonde_sfc)%n1:iv%info(sonde_sfc)%n2))
   allocate (model_t(1,iv%info(sonde_sfc)%n1:iv%info(sonde_sfc)%n2))
   allocate (model_p(1,iv%info(sonde_sfc)%n1:iv%info(sonde_sfc)%n2))
   allocate (model_q(1,iv%info(sonde_sfc)%n1:iv%info(sonde_sfc)%n2))
   allocate (model_hsm(1,iv%info(sonde_sfc)%n1:iv%info(sonde_sfc)%n2))

   if ( it > 1 ) then
      do n=iv%info(sonde_sfc)%n1,iv%info(sonde_sfc)%n2
         if (iv%sonde_sfc(n)%u%qc == fails_error_max) iv%sonde_sfc(n)%u%qc = 0
         if (iv%sonde_sfc(n)%v%qc == fails_error_max) iv%sonde_sfc(n)%v%qc = 0
         if (iv%sonde_sfc(n)%t%qc == fails_error_max) iv%sonde_sfc(n)%t%qc = 0
         if (iv%sonde_sfc(n)%p%qc == fails_error_max) iv%sonde_sfc(n)%p%qc = 0
         if (iv%sonde_sfc(n)%q%qc == fails_error_max) iv%sonde_sfc(n)%q%qc = 0
      end do
   end if

   if (sfc_assi_options == sfc_assi_options_1) then
      do n=iv%info(sonde_sfc)%n1,iv%info(sonde_sfc)%n2
         ! [1.1] Get horizontal interpolation weights:

         i   = iv%info(sonde_sfc)%i(1,n)
         j   = iv%info(sonde_sfc)%j(1,n)
         dx  = iv%info(sonde_sfc)%dx(1,n)
         dy  = iv%info(sonde_sfc)%dy(1,n)
         dxm = iv%info(sonde_sfc)%dxm(1,n)
         dym = iv%info(sonde_sfc)%dym(1,n)

         ! Surface correction

         iv%sonde_sfc(n)%p%inv = ob%sonde_sfc(n)%p
         iv%sonde_sfc(n)%t%inv = ob%sonde_sfc(n)%t
         iv%sonde_sfc(n)%q%inv = ob%sonde_sfc(n)%q
         iv%sonde_sfc(n)%u%inv = ob%sonde_sfc(n)%u
         iv%sonde_sfc(n)%v%inv = ob%sonde_sfc(n)%v

         if (iv % sonde_sfc(n) % h > missing_r) then
            do k=kts,kte
               v_h(k) = dym*(dxm*grid%xb%h(i,j  ,k) + dx*grid%xb%h(i+1,j  ,k)) &
                  + dy *(dxm*grid%xb%h(i,j+1,k) + dx*grid%xb%h(i+1,j+1,k))
            end do

            hd = v_h(kts) - iv % sonde_sfc(n) % h

            if (abs(hd) <= Max_StHeight_Diff .or. anal_type_verify ) then
               if (iv % sonde_sfc(n) % h < v_h(kts)) then
                  iv%info(sonde_sfc)%zk(:,n) = 1.0+1.0e-6
                  call da_obs_sfc_correction(iv%info(sonde_sfc), iv%sonde_sfc(n), n, grid%xb)

               else
                  call da_to_zk(iv % sonde_sfc(n) % h, v_h, v_interp_h, iv%info(sonde_sfc)%zk(1,n))
               end if
            end if
         else if (ob % sonde_sfc(n) % p > 1.0) then
            do k=kts,kte
               v_p(k) = dym*(dxm*grid%xb%p(i,j  ,k) + dx*grid%xb%p(i+1,j  ,k)) &
                      + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
            end do

            call da_to_zk(ob % sonde_sfc(n) % p, v_p, v_interp_p, iv%info(sonde_sfc)%zk(1,n))

            if (iv%info(sonde_sfc)%zk(1,n) < 0.0 .and. .not. anal_type_verify) then
               iv % sonde_sfc(n) % p % inv = missing_r
               iv % sonde_sfc(n) % p % qc  = missing_data
               iv%info(sonde_sfc)%zk(:,n) = 1.0+1.0e-6
            end if
         end if
      end do

      call da_convert_zk (iv%info(sonde_sfc))

      if (.not.anal_type_verify ) then
         do n=iv%info(sonde_sfc)%n1,iv%info(sonde_sfc)%n2
            if (iv%info(sonde_sfc)%zk(1,n) < 0.0) then
               iv % sonde_sfc(n) % u % qc = missing_data
               iv % sonde_sfc(n) % v % qc = missing_data
               iv % sonde_sfc(n) % t % qc = missing_data
               iv % sonde_sfc(n) % q % qc = missing_data
               iv % sonde_sfc(n) % p % qc = missing_data
            end if
         end do
      end if

      ! [1.2] Interpolate horizontally:
#ifdef A2C
      call da_interp_lin_3d (grid%xb%u, iv%info(sonde_sfc), model_u,'u')
      call da_interp_lin_3d (grid%xb%v, iv%info(sonde_sfc), model_v,'v')
#else
      call da_interp_lin_3d (grid%xb%u, iv%info(sonde_sfc), model_u)
      call da_interp_lin_3d (grid%xb%v, iv%info(sonde_sfc), model_v)
#endif
      call da_interp_lin_3d (grid%xb%t, iv%info(sonde_sfc), model_t)
      call da_interp_lin_3d (grid%xb%q, iv%info(sonde_sfc), model_q)
      call da_interp_lin_3d (grid%xb%p, iv%info(sonde_sfc), model_p)

   else if (sfc_assi_options == sfc_assi_options_2) then
      ! 1.2.1 Surface assimilation approach 2(10-m u, v, 2-m t, q, and sfc_p)

      call da_interp_lin_2d (grid%xb%u10,  iv%info(sonde_sfc), 1,model_u)
      call da_interp_lin_2d (grid%xb%v10,  iv%info(sonde_sfc), 1,model_v)
      call da_interp_lin_2d (grid%xb%t2,   iv%info(sonde_sfc), 1,model_t)
      call da_interp_lin_2d (grid%xb%q2,   iv%info(sonde_sfc), 1,model_q)
      call da_interp_lin_2d (grid%xb%psfc, iv%info(sonde_sfc), 1,model_p)

      do n=iv%info(sonde_sfc)%n1,iv%info(sonde_sfc)%n2

         iv%sonde_sfc(n)%p%inv = ob%sonde_sfc(n)%p
         iv%sonde_sfc(n)%t%inv = ob%sonde_sfc(n)%t
         iv%sonde_sfc(n)%q%inv = ob%sonde_sfc(n)%q
         iv%sonde_sfc(n)%u%inv = ob%sonde_sfc(n)%u
         iv%sonde_sfc(n)%v%inv = ob%sonde_sfc(n)%v

         if (iv%sonde_sfc(n)%p%qc >= 0) then
            ! model surface p, t, q, h at observed site:

            call da_interp_lin_2d_partial (grid%xb%terr, iv%info(sonde_sfc), 1, n, n, model_hsm(:,n))

            ho = iv%sonde_sfc(n)%h
            to = -888888.0
            qo = -888888.0

            if (iv%sonde_sfc(n)%t%qc >= 0 .and. iv%sonde_sfc(n)%q%qc >= 0) then
               to = ob%sonde_sfc(n)%t
               qo = ob%sonde_sfc(n)%q
               call da_sfc_pre(psfcm, model_p(1,n), model_t(1,n), model_q(1,n), model_hsm(1,n), ho, to, qo)
            else if (iv%sonde_sfc(n)%t%qc >= 0 .and. iv%sonde_sfc(n)%q%qc < 0) then
               to = ob%sonde_sfc(n)%t
               call da_sfc_pre(psfcm, model_p(1,n), model_t(1,n), model_q(1,n), model_hsm(1,n), ho,to)
            else
               call da_sfc_pre(psfcm, model_p(1,n), model_t(1,n), model_q(1,n), model_hsm(1,n), ho)
            end if

            ! Pressure at the observed height:
            model_p(1,n) = psfcm
         end if
      end do
   end if

   do n=iv%info(sonde_sfc)%n1,iv%info(sonde_sfc)%n2
      !-----------------------------------------------------------------------
      ! [3.0] Fast interpolation:
      !-----------------------------------------------------------------------
      if(wind_sd_sound)then
         call da_ffdduv_model (speed,direction,model_u(1,n), model_v(1,n), convert_uv2fd)

         if (ob%sonde_sfc(n)%u > missing_r .AND. iv%sonde_sfc(n)%u%qc >= obs_qc_pointer) then
             iv%sonde_sfc(n)%u%inv = iv%sonde_sfc(n)%u%inv - speed
         else
             iv % sonde_sfc(n) % u % inv = 0.0
         end if

         if (ob%sonde_sfc(n)%v > missing_r .AND. iv%sonde_sfc(n)%v%qc >= obs_qc_pointer) then
             iv%sonde_sfc(n)%v%inv = iv%sonde_sfc(n)%v%inv - direction
             if (iv%sonde_sfc(n)%v%inv > 180.0 ) iv%sonde_sfc(n)%v%inv = iv%sonde_sfc(n)%v%inv - 360.0
             if (iv%sonde_sfc(n)%v%inv < -180.0 ) iv%sonde_sfc(n)%v%inv = iv%sonde_sfc(n)%v%inv + 360.0
         else
             iv % sonde_sfc(n) % v % inv = 0.0
         end if
      else
         if (ob % sonde_sfc(n) % u > missing_r .AND. iv % sonde_sfc(n) % u % qc >= obs_qc_pointer) then
             iv % sonde_sfc(n) % u % inv = iv%sonde_sfc(n)%u%inv - model_u(1,n)
         else
             iv % sonde_sfc(n) % u % inv = 0.0
         end if

         if (ob % sonde_sfc(n) % v > missing_r .AND. iv % sonde_sfc(n) % v % qc >= obs_qc_pointer) then
             iv % sonde_sfc(n) % v % inv = iv%sonde_sfc(n)%v%inv - model_v(1,n)
         else
             iv % sonde_sfc(n) % v % inv = 0.0
         end if
      end if

      !if (ob % sonde_sfc(n) % p > 0.0 .AND. iv % sonde_sfc(n) % p % qc >= obs_qc_pointer) then
      if ( iv % sonde_sfc(n) % p % qc >= obs_qc_pointer ) then
         iv % sonde_sfc(n) % p % inv = iv%sonde_sfc(n)%p%inv - model_p(1,n)
      else
         iv % sonde_sfc(n) % p % inv = 0.0
      end if

      if (ob % sonde_sfc(n) % t > 0.0 .AND. iv % sonde_sfc(n) % t % qc >= obs_qc_pointer) then
         iv % sonde_sfc(n) % t % inv = iv%sonde_sfc(n)%t%inv - model_t(1,n)
      else
         iv % sonde_sfc(n) % t % inv = 0.0
      end if

      if (ob % sonde_sfc(n) % q > 0.0 .AND. iv % sonde_sfc(n) % q % qc >= obs_qc_pointer) then
         iv % sonde_sfc(n) % q % inv = iv%sonde_sfc(n)%q%inv - model_q(1,n)
      else
         iv % sonde_sfc(n) % q % inv = 0.0
      end if
   end do

   !-----------------------------------------------------------------------
   !     [5.0] Perform optional maximum error check:
   !-----------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_sonde_sfc(iv,ob, it, num_qcstat_conv)
 
   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_t)
   deallocate (model_p)
   deallocate (model_q)
   deallocate (model_hsm)
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_sonde_sfc")

end subroutine da_get_innov_vector_sonde_sfc


