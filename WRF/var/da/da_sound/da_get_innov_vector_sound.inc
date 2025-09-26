subroutine da_get_innov_vector_sound (it,num_qcstat_conv, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD  
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid     ! first guess state.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer :: n, k        ! Loop counter.
   integer :: i  (kms:kme)
   integer :: j  (kms:kme)
   real    :: dx (kms:kme)
   real    :: dxm(kms:kme)  
   real    :: dy (kms:kme)
   real    :: dym(kms:kme)  

   real, allocatable :: model_u(:,:)  ! Model value u at ob location.
   real, allocatable :: model_v(:,:)  ! Model value v at ob location.
   real, allocatable :: model_t(:,:)  ! Model value t at ob location.
   real, allocatable :: model_q(:,:)  ! Model value q at ob location.

   real    :: speed, direction

   real    :: v_h(kts:kte)      ! Model value h at ob hor. location.
   real    :: v_p(kts:kte)      ! Model value p at ob hor. location.

   if (trace_use_dull) call da_trace_entry ("da_get_innov_vector_sound")

   allocate (model_u(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (model_v(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (model_t(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (model_q(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))

   model_u(:,:) = 0.0
   model_v(:,:) = 0.0
   model_t(:,:) = 0.0
   model_q(:,:) = 0.0

   if ( it > 1 ) then
     do n=iv%info(sound)%n1,iv%info(sound)%n2
        do k=1, iv%info(sound)%levels(n)
           if (iv%sound(n)%u(k)%qc == fails_error_max) iv%sound(n)%u(k)%qc = 0
           if (iv%sound(n)%v(k)%qc == fails_error_max) iv%sound(n)%v(k)%qc = 0
           if (iv%sound(n)%t(k)%qc == fails_error_max) iv%sound(n)%t(k)%qc = 0
           if (iv%sound(n)%q(k)%qc == fails_error_max) iv%sound(n)%q(k)%qc = 0
        end do
     end do
   end if

   do n=iv%info(sound)%n1, iv%info(sound)%n2
      if (iv%info(sound)%levels(n) < 1) cycle

      ! [1.1] Get horizontal interpolation weights:

      if (position_lev_dependant) then
         i(:)   = iv%info(sound)%i(:,n)
         j(:)   = iv%info(sound)%j(:,n)
         dx(:)  = iv%info(sound)%dx(:,n)
         dy(:)  = iv%info(sound)%dy(:,n)
         dxm(:) = iv%info(sound)%dxm(:,n)
         dym(:) = iv%info(sound)%dym(:,n)
         do k=kts,kte
            v_h(k) = dym(k)*(dxm(k)*grid%xb%h(i(k),j(k),k) + dx(k)*grid%xb%h(i(k)+1,j(k),k)) &
               + dy(k) *(dxm(k)*grid%xb%h(i(k),j(k)+1,k) + dx(k)*grid%xb%h(i(k)+1,j(k)+1,k))
            v_p(k) = dym(k)*(dxm(k)*grid%xb%p(i(k),j(k),k) + dx(k)*grid%xb%p(i(k)+1,j(k),k)) &
               + dy(k) *(dxm(k)*grid%xb%p(i(k),j(k)+1,k) + dx(k)*grid%xb%p(i(k)+1,j(k)+1,k))
         end do
      else
         i(1)   = iv%info(sound)%i(1,n)
         j(1)   = iv%info(sound)%j(1,n)
         dx(1)  = iv%info(sound)%dx(1,n)
         dy(1)  = iv%info(sound)%dy(1,n)
         dxm(1) = iv%info(sound)%dxm(1,n)
         dym(1) = iv%info(sound)%dym(1,n)

         v_h(kts:kte) = dym(1) * (dxm(1)*grid%xb%h(i(1),j(1),kts:kte)   + dx(1)*grid%xb%h(i(1)+1,j(1),kts:kte)) &
                       + dy(1) * (dxm(1)*grid%xb%h(i(1),j(1)+1,kts:kte) + dx(1)*grid%xb%h(i(1)+1,j(1)+1,kts:kte))
         v_p(kts:kte) = dym(1) * (dxm(1)*grid%xb%p(i(1),j(1),kts:kte)   + dx(1)*grid%xb%p(i(1)+1,j(1),kts:kte)) &
                       + dy(1) * (dxm(1)*grid%xb%p(i(1),j(1)+1,kts:kte) + dx(1)*grid%xb%p(i(1)+1,j(1)+1,kts:kte))
      end if

      do k=1, iv%info(sound)%levels(n)
         if (iv%sound(n)%p(k) > 1.0) then
            call da_to_zk (iv%sound(n)%p(k), v_p, v_interp_p, iv%info(sound)%zk(k,n))
         else if (iv%sound(n)%h(k) > 0.0) then
            call da_to_zk (iv%sound(n)%h(k), v_h, v_interp_h, iv%info(sound)%zk(k,n))
         end if

      end do

   end do

   call da_convert_zk (iv%info(sound))

   if (.not. anal_type_verify) then
      do n=iv%info(sound)%n1,iv%info(sound)%n2
         do k=1, iv%info(sound)%levels(n)
            if (iv%info(sound)%zk(k,n) < 0.0) then
               iv%sound(n)%u(k)%qc = missing_data
               iv%sound(n)%v(k)%qc = missing_data
               iv%sound(n)%t(k)%qc = missing_data
               iv%sound(n)%q(k)%qc = missing_data
            end if
         end do
      end do
   end if

   ! [1.2] Interpolate horizontally to ob:

#ifdef A2C
   call da_interp_lin_3d (grid%xb%u, iv%info(sound), model_u,'u')
   call da_interp_lin_3d (grid%xb%v, iv%info(sound), model_v,'v')
#else
   call da_interp_lin_3d (grid%xb%u, iv%info(sound), model_u)
   call da_interp_lin_3d (grid%xb%v, iv%info(sound), model_v)
#endif
   call da_interp_lin_3d (grid%xb%t, iv%info(sound), model_t)
   call da_interp_lin_3d (grid%xb%q, iv%info(sound), model_q)

   do n=iv%info(sound)%n1, iv%info(sound)%n2
      !----------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !----------------------------------------------------------------------

      do k = 1, iv%info(sound)%levels(n)
         iv%sound(n)%u(k)%inv = 0.0
         iv%sound(n)%v(k)%inv = 0.0
         iv%sound(n)%t(k)%inv = 0.0
         iv%sound(n)%q(k)%inv = 0.0
         !-------------------------------------------------------------------
         ! [3.0] Interpolation:
         !-------------------------------------------------------------------

          if (wind_sd_sound) then
              call da_ffdduv_model (speed,direction,model_u(k,n), model_v(k,n), convert_uv2fd)

              if (ob%sound(n)%u(k) > missing_r .AND. iv%sound(n)%u(k)%qc >= obs_qc_pointer) then
                  iv%sound(n)%u(k)%inv = ob%sound(n)%u(k) - speed
              end if

              if (ob%sound(n)%v(k) > missing_r .AND. iv%sound(n)%v(k)%qc >= obs_qc_pointer) then
                  iv%sound(n)%v(k)%inv = ob%sound(n)%v(k) - direction
                  if (iv%sound(n)%v(k)%inv > 180.0 ) iv%sound(n)%v(k)%inv = iv%sound(n)%v(k)%inv - 360.0
                  if (iv%sound(n)%v(k)%inv < -180.0 ) iv%sound(n)%v(k)%inv = iv%sound(n)%v(k)%inv + 360.0
              end if
           else
             if (ob%sound(n)%u(k) > missing_r .AND. iv%sound(n)%u(k)%qc >= obs_qc_pointer) then
                 iv%sound(n)%u(k)%inv = ob%sound(n)%u(k) - model_u(k,n)
             end if

             if (ob%sound(n)%v(k) > missing_r .AND. iv%sound(n)%v(k)%qc >= obs_qc_pointer) then
                 iv%sound(n)%v(k)%inv = ob%sound(n)%v(k) - model_v(k,n)
             end if
          end if

         if (ob%sound(n)%t(k) > missing_r .AND. iv%sound(n)%t(k)%qc >= obs_qc_pointer) then
            iv%sound(n)%t(k)%inv = ob%sound(n)%t(k) - model_t(k,n)
         end if

         if (ob%sound(n)%q(k) > missing_r .AND. iv%sound(n)%q(k)%qc >= obs_qc_pointer) then
            iv%sound(n)%q(k)%inv = ob%sound(n)%q(k) - model_q(k,n)
         end if
      end do
   end do

   !----------------------------------------------------------------------
   ! [5.0] Perform optional maximum error check:
   !----------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_sound (iv, it, num_qcstat_conv)

   if (check_buddy) call da_check_buddy_sound(iv, grid%dx, it)
!
   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_t)
   deallocate (model_q)
   
   if (trace_use_dull) call da_trace_exit ("da_get_innov_vector_sound")

end subroutine da_get_innov_vector_sound


