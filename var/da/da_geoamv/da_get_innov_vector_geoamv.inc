subroutine da_get_innov_vector_geoamv( it,num_qcstat_conv,  grid, ob, iv)

   !-------------------------------------------------------------------------
   ! Purpose: Calculates innovation vector does QC for geoamv
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-------------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(domain),     intent(in)    :: grid    ! first guess state.
   type(y_type),     intent(in)    :: ob      ! Observation structure.
   type(iv_type),    intent(inout) :: iv      ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer :: n              ! Loop counter.
   integer :: k              ! Index dimension.
   integer :: num_levs       ! Number of obs levels.
   real    :: speed, direction

   integer :: i  (kms:kme)
   integer :: j  (kms:kme)
   real    :: dx (kms:kme)
   real    :: dxm(kms:kme)  
   real    :: dy (kms:kme)
   real    :: dym(kms:kme) 
   real,allocatable :: model_u(:,:)
   real,allocatable :: model_v(:,:)

   real    :: v_p(kts:kte)      ! Model value p at ob hor. location.

   integer :: itu,ituf,itvv,itvvf

   if (trace_use_dull) call da_trace_entry ("da_get_innov_vector_geoamv")

   allocate (model_u(iv%info(geoamv)%max_lev,iv%info(geoamv)%n1:iv%info(geoamv)%n2))
   allocate (model_v(iv%info(geoamv)%max_lev,iv%info(geoamv)%n1:iv%info(geoamv)%n2))

   model_u(:,:) = 0.0
   model_v(:,:) = 0.0
   
   if ( it > 1 ) then
      do n = iv%info(geoamv)%n1, iv%info(geoamv)%n2
         do k = 1, iv%info(geoamv)%levels(n)
            if (iv%geoamv(n)%u(k)%qc == fails_error_max) iv%geoamv(n)%u(k)%qc = 0
            if (iv%geoamv(n)%v(k)%qc == fails_error_max) iv%geoamv(n)%v(k)%qc = 0
         end do
      end do
   end if

   do n = iv%info(geoamv)%n1, iv%info(geoamv)%n2
      ! [1.3] Get horizontal interpolation weights:

      num_levs = iv%info(geoamv)%levels(n)
      if (num_levs < 1) cycle

      ! slower
      ! i(:)   = iv%info(geoamv)%i(:,n)
      ! j(:)   = iv%info(geoamv)%j(:,n)
      ! dx(:)  = iv%info(geoamv)%dx(:,n)
      ! dy(:)  = iv%info(geoamv)%dy(:,n)
      ! dxm(:) = iv%info(geoamv)%dxm(:,n)
      ! dym(:) = iv%info(geoamv)%dym(:,n)

      ! faster
      i(1)   = iv%info(geoamv)%i(1,n)
      j(1)   = iv%info(geoamv)%j(1,n)
      dx(1)  = iv%info(geoamv)%dx(1,n)
      dy(1)  = iv%info(geoamv)%dy(1,n)
      dxm(1) = iv%info(geoamv)%dxm(1,n)
      dym(1) = iv%info(geoamv)%dym(1,n)

      ! if position varies with height, slower
      ! do k=kts,kte
      !    v_p(k) = dym(k)*(dxm(k)*grid%xb%p(i(k),j(k),k)+dx(k)*grid%xb%p(i(k)+1,j(k),k)) &
      !       + dy(k)*(dxm(k)*grid%xb%p(i(k),j(k)+1,k)+dx(k)*grid%xb%p(i(k)+1,j(k)+1,k))
      ! end do
 
      ! If position does not, faster
      v_p(kts:kte) = dym(1)*(dxm(1)*grid%xb%p(i(1),j(1),kts:kte) + dx(1)*grid%xb%p(i(1)+1,j(1),kts:kte)) &
         + dy(1)*(dxm(1)*grid%xb%p(i(1),j(1)+1,kts:kte) + dx(1)*grid%xb%p(i(1)+1,j(1)+1,kts:kte))

      do k=1, iv%info(geoamv)%levels(n)
         if (iv%geoamv(n)%p(k) > 1.0) then
            call da_to_zk (iv%geoamv(n)%p(k), v_p, v_interp_p, iv%info(geoamv)%zk(k,n))
         end if
      end do

   end do

   call da_convert_zk (iv%info(geoamv))

   if (.not. anal_type_verify) then
      do n = iv%info(geoamv)%n1, iv%info(geoamv)%n2
         do k=1, iv%info(geoamv)%levels(n)
            if (iv%info(geoamv)%zk(k,n) < 0.0) then
               iv%geoamv(n)%u(k)% qc = missing_data
               iv%geoamv(n)%v(k)% qc = missing_data
            end if
         end do
      end do
   end if

#ifdef A2C
   call da_interp_lin_3d (grid%xb%u, iv%info(geoamv), model_u,'u')
   call da_interp_lin_3d (grid%xb%v, iv%info(geoamv), model_v,'v')
#else
   call da_interp_lin_3d (grid%xb%u, iv%info(geoamv), model_u)
   call da_interp_lin_3d (grid%xb%v, iv%info(geoamv), model_v)
#endif

   do n = iv%info(geoamv)%n1, iv%info(geoamv)%n2
      do k = 1, iv%info(geoamv)%levels(n)
         iv%geoamv(n)%u(k)%inv = 0.0
         iv%geoamv(n)%v(k)%inv = 0.0

         if (wind_sd_geoamv) then
             call da_ffdduv_model (speed,direction,model_u(k,n), model_v(k,n), convert_uv2fd)

             if (ob%geoamv(n)%u(k) > missing_r .AND. iv%geoamv(n)%u(k)%qc >= obs_qc_pointer) then
                 iv%geoamv(n)%u(k)%inv = ob%geoamv(n)%u(k) - speed
             end if

             if (ob%geoamv(n)%v(k) > missing_r .AND. iv%geoamv(n)%v(k)%qc >= obs_qc_pointer) then
                 iv%geoamv(n)%v(k)%inv = ob%geoamv(n)%v(k) - direction
                 if (iv%geoamv(n)%v(k)%inv > 180.0 ) iv%geoamv(n)%v(k)%inv = iv%geoamv(n)%v(k)%inv - 360.0
                 if (iv%geoamv(n)%v(k)%inv < -180.0 ) iv%geoamv(n)%v(k)%inv = iv%geoamv(n)%v(k)%inv + 360.0
             end if
         else
             if (ob%geoamv(n)%u(k) > missing_r .AND. iv%geoamv(n)%u(k)%qc >= obs_qc_pointer) then
                 iv%geoamv(n)%u(k)%inv = ob%geoamv(n)%u(k) - model_u(k,n)
             end if

             if (ob%geoamv(n)%v(k) > missing_r .AND. iv%geoamv(n)%v(k)%qc >= obs_qc_pointer) then
                 iv%geoamv(n)%v(k)%inv = ob%geoamv(n)%v(k) - model_v(k,n)
             end if
         end if

      end do
   end do

   !------------------------------------------------------------------------
   ! Perform optional maximum error check:
   !------------------------------------------------------------------------
   if ( check_max_iv ) &
      call da_check_max_iv_geoamv(iv,it,num_qcstat_conv)     

   deallocate (model_u)
   deallocate (model_v)

   if (trace_use_dull) call da_trace_exit ("da_get_innov_vector_geoamv")
   
end subroutine da_get_innov_vector_geoamv
