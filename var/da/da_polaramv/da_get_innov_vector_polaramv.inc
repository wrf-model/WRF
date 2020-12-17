subroutine da_get_innov_vector_polaramv( it, num_qcstat_conv, grid, ob, iv)

   !----------------------------------------------------------------------
   ! Purpose: Calculates innovation vector, does QC for polaramv
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(domain),     intent(in)    :: grid      ! first guess state.
   type(y_type),     intent(in)    :: ob      ! Observation structure.
   type(iv_type),    intent(inout) :: iv      ! O-B structure.
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

   real    :: v_p(kms:kme)      ! Model value p at ob hor. location.
   real    :: speed, direction
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_polaramv")

   allocate (model_u(iv%info(polaramv)%max_lev,iv%info(polaramv)%n1:iv%info(polaramv)%n2))
   allocate (model_v(iv%info(polaramv)%max_lev,iv%info(polaramv)%n1:iv%info(polaramv)%n2))

   model_u(:,:) = 0.0
   model_v(:,:) = 0.0

   if( it > 1 ) then
      do n=iv%info(polaramv)%n1, iv%info(polaramv)%n2
         do k=1, iv%info(polaramv)%levels(n)
            if (iv%polaramv(n)%u(k)%qc == fails_error_max) iv%polaramv(n)%u(k)%qc = 0
            if (iv%polaramv(n)%v(k)%qc == fails_error_max) iv%polaramv(n)%v(k)%qc = 0
         end do
      end do
   end if

   do n=iv%info(polaramv)%n1, iv%info(polaramv)%n2
      if (iv%info(polaramv)%levels(n) < 1) cycle

      ! [1.1] Get horizontal interpolation weights:

      if (position_lev_dependant) then
         i(:)   = iv%info(polaramv)%i(:,n)
         j(:)   = iv%info(polaramv)%j(:,n)
         dx(:)  = iv%info(polaramv)%dx(:,n)
         dy(:)  = iv%info(polaramv)%dy(:,n)
         dxm(:) = iv%info(polaramv)%dxm(:,n)
         dym(:) = iv%info(polaramv)%dym(:,n)
         do k=kts,kte
            v_p(k) = dym(k)*(dxm(k)*grid%xb%p(i(k),j(k),k) + dx(k)*grid%xb%p(i(k)+1,j(k),k)) &
               + dy(k) *(dxm(k)*grid%xb%p(i(k),j(k)+1,k) + dx(k)*grid%xb%p(i(k)+1,j(k)+1,k))
         end do
      else
         i(1)   = iv%info(polaramv)%i(1,n)
         j(1)   = iv%info(polaramv)%j(1,n)
         dx(1)  = iv%info(polaramv)%dx(1,n)
         dy(1)  = iv%info(polaramv)%dy(1,n)
         dxm(1) = iv%info(polaramv)%dxm(1,n)
         dym(1) = iv%info(polaramv)%dym(1,n)

         v_p(kts:kte) = dym(1) * (dxm(1)*grid%xb%p(i(1),j(1),kts:kte)   + dx(1)*grid%xb%p(i(1)+1,j(1),kts:kte)) &
                       + dy(1) * (dxm(1)*grid%xb%p(i(1),j(1)+1,kts:kte) + dx(1)*grid%xb%p(i(1)+1,j(1)+1,kts:kte))
      end if

      do k=1, iv%info(polaramv)%levels(n)
         if (iv%polaramv(n)%p(k) > 1.0) then
            call da_to_zk (iv%polaramv(n)%p(k), v_p,v_interp_p, iv%info(polaramv)%zk(k,n))
         end if
      end do
   end do

   call da_convert_zk(iv%info(polaramv))

   if (.not. anal_type_verify) then
      do n=iv%info(polaramv)%n1,iv%info(polaramv)%n2
         do k=1, iv%info(polaramv)%levels(n)
            if (iv%info(polaramv)%zk(k,n) < 0.0) then
               iv%polaramv(n)%u(k)%qc = missing_data
               iv%polaramv(n)%v(k)%qc = missing_data
            end if
         end do
      end do
   end if

   ! [1.2] Interpolate horizontally to ob:

#ifdef A2C
   call da_interp_lin_3d (grid%xb%u, iv%info(polaramv), model_u,'u')
   call da_interp_lin_3d (grid%xb%v, iv%info(polaramv), model_v,'v')
#else
   call da_interp_lin_3d (grid%xb%u, iv%info(polaramv), model_u)
   call da_interp_lin_3d (grid%xb%v, iv%info(polaramv), model_v)
#endif

   do n=iv%info(polaramv)%n1,iv%info(polaramv)%n2
      do k = 1, iv%info(polaramv)%levels(n)

          if (wind_sd_polaramv) then
              call da_ffdduv_model (speed,direction,model_u(k,n), model_v(k,n), convert_uv2fd)

              if (ob%polaramv(n)%u(k) > missing_r .AND. iv%polaramv(n)%u(k)%qc >= obs_qc_pointer) then
                  iv%polaramv(n)%u(k)%inv = ob%polaramv(n)%u(k) - speed
              end if

              if (ob%polaramv(n)%v(k) > missing_r .AND. iv%polaramv(n)%v(k)%qc >= obs_qc_pointer) then
                  iv%polaramv(n)%v(k)%inv = ob%polaramv(n)%v(k) - direction
                  if (iv%polaramv(n)%v(k)%inv > 180.0 ) iv%polaramv(n)%v(k)%inv = iv%polaramv(n)%v(k)%inv - 360.0
                  if (iv%polaramv(n)%v(k)%inv < -180.0 ) iv%polaramv(n)%v(k)%inv = iv%polaramv(n)%v(k)%inv + 360.0
              end if
          else

             if (ob%polaramv(n)%u(k) > missing_r .AND. iv%polaramv(n)%u(k)%qc >= obs_qc_pointer) then
                 iv%polaramv(n)%u(k)%inv = ob%polaramv(n)%u(k) - model_u(k,n)
             end if

             if (ob%polaramv(n)%v(k) > missing_r .AND. iv%polaramv(n)%v(k)%qc >= obs_qc_pointer) then
                 iv%polaramv(n)%v(k)%inv = ob%polaramv(n)%v(k) - model_v(k,n)
             end if
          end if

      end do
   end do
   
   !------------------------------------------------------------------------
   ! Perform optional maximum error check:
   !------------------------------------------------------------------------

    if ( check_max_iv ) &
       call da_check_max_iv_polaramv(iv, it, num_qcstat_conv)     

   deallocate (model_u)
   deallocate (model_v)
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_polaramv")

end subroutine da_get_innov_vector_polaramv


