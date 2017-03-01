subroutine da_get_innov_vector_ships( it,num_qcstat_conv, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD     
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(domain),     intent(in)    :: grid    ! first guess state.
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

   real    :: v_h(kms:kme)   ! Model value h at ob hor. location.
   real    :: v_p(kms:kme)   ! Model value p at ob hor. location.

   real    :: hd, psfcm
   real    :: ho, to, qo
   real  :: speed, direction
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_ships")

   allocate (model_u(1,iv%info(ships)%n1:iv%info(ships)%n2))
   allocate (model_v(1,iv%info(ships)%n1:iv%info(ships)%n2))
   allocate (model_t(1,iv%info(ships)%n1:iv%info(ships)%n2))
   allocate (model_p(1,iv%info(ships)%n1:iv%info(ships)%n2))
   allocate (model_q(1,iv%info(ships)%n1:iv%info(ships)%n2))
   allocate (model_hsm(1,iv%info(ships)%n1:iv%info(ships)%n2))

   if ( it > 1 ) then
      do n=iv%info(ships)%n1,iv%info(ships)%n2
         if (iv%ships(n)%u%qc == fails_error_max) iv%ships(n)%u%qc = 0
         if (iv%ships(n)%v%qc == fails_error_max) iv%ships(n)%v%qc = 0
         if (iv%ships(n)%t%qc == fails_error_max) iv%ships(n)%t%qc = 0
         if (iv%ships(n)%p%qc == fails_error_max) iv%ships(n)%p%qc = 0
         if (iv%ships(n)%q%qc == fails_error_max) iv%ships(n)%q%qc = 0
      end do
   end if

   if (sfc_assi_options == sfc_assi_options_1) then
      do n=iv%info(ships)%n1,iv%info(ships)%n2

         ! [1.1] Get horizontal interpolation weights:

         i   = iv%info(ships)%i(1,n)
         j   = iv%info(ships)%j(1,n)
         dx  = iv%info(ships)%dx(1,n)
         dy  = iv%info(ships)%dy(1,n)
         dxm = iv%info(ships)%dxm(1,n)
         dym = iv%info(ships)%dym(1,n)

         ! Surface correction

         iv%ships(n)%p%inv = ob%ships(n)%p
         iv%ships(n)%t%inv = ob%ships(n)%t
         iv%ships(n)%q%inv = ob%ships(n)%q
         iv%ships(n)%u%inv = ob%ships(n)%u
         iv%ships(n)%v%inv = ob%ships(n)%v

         if (iv % ships(n) % h > missing_r) then
            do k=kts,kte
              v_h(k) = dym*(dxm*grid%xb%h(i,j  ,k) + dx*grid%xb%h(i+1,j  ,k)) &
                 + dy *(dxm*grid%xb%h(i,j+1,k) + dx*grid%xb%h(i+1,j+1,k))
            end do

            hd = v_h(kts) - iv % ships(n) % h

            if (abs(hd) <= Max_StHeight_Diff .or. anal_type_verify) then
               if (iv % ships(n) % h < v_h(kts)) then
                  iv%info(ships)%zk(:,n) = 1.0+1.0e-6
                  call da_obs_sfc_correction(iv%info(ships), iv%ships(n), n, grid%xb)
               else
                  call da_to_zk(iv % ships(n) % h, v_h, v_interp_h, iv%info(ships)%zk(1,n))
               end if
            end if
         else if (ob % ships(n) % p > 1.0) then
            do k=kts,kte
              v_p(k) = dym*(dxm*grid%xb%p(i,j  ,k) + dx*grid%xb%p(i+1,j  ,k)) &
                       + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
            end do

            call da_to_zk(ob % ships(n) % p, v_p, v_interp_p, iv%info(ships)%zk(1,n))

            if (iv%info(ships)%zk(1,n) < 0.0 .and.  .not.anal_type_verify) then
               iv % ships(n) % p % inv = missing_r
               iv % ships(n) % p % qc  = missing_data
               iv%info(ships)%zk(:,n) = 1.0+1.0e-6
            end if
         end if
      end do

      call da_convert_zk (iv%info(ships))

      if (.not.anal_type_verify) then
         do n=iv%info(ships)%n1,iv%info(ships)%n2
            if (iv%info(ships)%zk(1,n) < 0.0) then
               iv % ships(n) % u % qc = missing_data
               iv % ships(n) % v % qc = missing_data
               iv % ships(n) % t % qc = missing_data
               iv % ships(n) % q % qc = missing_data
               iv % ships(n) % p % qc = missing_data
            end if
         end do
      end if

      ! Interpolate horizontally:
#ifdef A2C
      call da_interp_lin_3d (grid%xb%u, iv%info(ships), model_u,'u')
      call da_interp_lin_3d (grid%xb%v, iv%info(ships), model_v,'v')
#else
      call da_interp_lin_3d (grid%xb%u, iv%info(ships), model_u)
      call da_interp_lin_3d (grid%xb%v, iv%info(ships), model_v)
#endif
      call da_interp_lin_3d (grid%xb%t, iv%info(ships), model_t)
      call da_interp_lin_3d (grid%xb%q, iv%info(ships), model_q)
      call da_interp_lin_3d (grid%xb%p, iv%info(ships), model_p)

   else if (sfc_assi_options == sfc_assi_options_2) then
      ! Surface data assimilation approach 2

      ! 1.2.1 Surface assimilation approach 2(10-m u, v, 2-m t, q, and 
      ! sfc_p)

      call da_interp_lin_2d (grid%xb%u10,  iv%info(ships), 1, model_u)
      call da_interp_lin_2d (grid%xb%v10,  iv%info(ships), 1, model_v)
      call da_interp_lin_2d (grid%xb%t2,   iv%info(ships), 1, model_t)
      call da_interp_lin_2d (grid%xb%q2,   iv%info(ships), 1, model_q)
      call da_interp_lin_2d (grid%xb%psfc, iv%info(ships), 1, model_p)

      do n=iv%info(ships)%n1,iv%info(ships)%n2
         iv%ships(n)%p%inv = ob%ships(n)%p
         iv%ships(n)%t%inv = ob%ships(n)%t
         iv%ships(n)%q%inv = ob%ships(n)%q
         iv%ships(n)%u%inv = ob%ships(n)%u
         iv%ships(n)%v%inv = ob%ships(n)%v

         if (iv%ships(n)%p%qc >= 0) then

            ! model surface p, t, q, h at observed site:

            call da_interp_lin_2d_partial (grid%xb%terr, iv%info(ships), 1, n, n, model_hsm(:,n))

            ho = iv%ships(n)%h
            to = -888888.0
            qo = -888888.0

            if (iv%ships(n)%t%qc >= 0 .and. iv%ships(n)%q%qc >= 0) then
               to = ob%ships(n)%t
               qo = ob%ships(n)%q
               call da_sfc_pre(psfcm, model_p(1,n), model_t(1,n), model_q(1,n), model_hsm(1,n), ho, to, qo)
            else if (iv%ships(n)%t%qc >= 0 .and. iv%ships(n)%q%qc < 0) then
                to = ob%ships(n)%t
                call da_sfc_pre(psfcm, model_p(1,n), model_t(1,n), model_q(1,n), model_hsm(1,n), ho, to)
            else
                call da_sfc_pre(psfcm, model_p(1,n), model_t(1,n), model_q(1,n), model_hsm(1,n), ho)
            end if

            ! Pressure at the observed height:
            model_p(1,n) = psfcm
         end if
      end do
   end if

   do n=iv%info(ships)%n1,iv%info(ships)%n2

      !-----------------------------------------------------------------------
      ! [3.0] Fast interpolation:
      !-----------------------------------------------------------------------

      if(wind_sd_ships)then
         call da_ffdduv_model (speed,direction,model_u(1,n), model_v(1,n), convert_uv2fd)

         if (ob%ships(n)%u > missing_r .AND. iv%ships(n)%u%qc >= obs_qc_pointer) then
             iv%ships(n)%u%inv = iv%ships(n)%u%inv - speed
         else
             iv % ships(n) % u % inv = 0.0
         end if

         if (ob%ships(n)%v > missing_r .AND. iv%ships(n)%v%qc >= obs_qc_pointer) then
             iv%ships(n)%v%inv = iv%ships(n)%v%inv - direction
             if (iv%ships(n)%v%inv > 180.0 ) iv%ships(n)%v%inv = iv%ships(n)%v%inv - 360.0
             if (iv%ships(n)%v%inv < -180.0 ) iv%ships(n)%v%inv = iv%ships(n)%v%inv + 360.0
         else
             iv % ships(n) % v % inv = 0.0
         end if
      else
         if (ob % ships(n) % u > missing_r .AND. iv % ships(n) % u % qc >= obs_qc_pointer) then
             iv % ships(n) % u % inv = iv%ships(n)%u%inv - model_u(1,n)
         else
             iv % ships(n) % u % inv = 0.0
         end if

         if (ob % ships(n) % v > missing_r .AND. iv % ships(n) % v % qc >= obs_qc_pointer) then
             iv % ships(n) % v % inv = iv%ships(n)%v%inv - model_v(1,n)
         else
             iv % ships(n) % v % inv = 0.0
         end if
      end if

      !if (ob % ships(n) % p > 0.0 .AND. iv % ships(n) % p % qc >= obs_qc_pointer) then
      if ( iv % ships(n) % p % qc >= obs_qc_pointer ) then
         iv % ships(n) % p % inv = iv%ships(n)%p%inv - model_p(1,n)
      else
         iv % ships(n) % p % inv = 0.0
      end if

      if (ob % ships(n) % t > 0.0 .AND. iv % ships(n) % t % qc >= obs_qc_pointer) then
         iv % ships(n) % t % inv = iv%ships(n)%t%inv - model_t(1,n)
      else
         iv % ships(n) % t % inv = 0.0
      end if

      if (ob % ships(n) % q > 0.0 .AND. iv % ships(n) % q % qc >= obs_qc_pointer) then
         iv % ships(n) % q % inv = iv%ships(n)%q%inv - model_q(1,n)
      else
         iv % ships(n) % q % inv = 0.0
      end if
   end do

   !---------------------------------------------------------------------
   ! [5.0] Perform optional maximum error check:
   !---------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_ships(iv,ob, it, num_qcstat_conv)
   
   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_t)
   deallocate (model_p)
   deallocate (model_q)
   deallocate (model_hsm)
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_ships")

end subroutine da_get_innov_vector_ships


