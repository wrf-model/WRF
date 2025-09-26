subroutine da_get_innov_vector_profiler( it,num_qcstat_conv, grid, ob, iv)

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

   integer :: n, k  ! Loop counter.
   integer :: i, j  ! Index dimension.
   real    :: speed, direction
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.

   real, allocatable :: model_u(:,:)  ! Model value u at ob location.
   real, allocatable :: model_v(:,:)  ! Model value v at ob location.

   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.
   real    :: v_p(kms:kme)      ! Model value p at ob hor. location.
   integer :: itu,ituf,itvv,itvvf
   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_profiler")
   
   itu   = 0; itvv    = 0;
   ituf  = 0; itvvf   = 0;

   allocate (model_u(iv%info(profiler)%max_lev,iv%info(profiler)%n1:iv%info(profiler)%n2))
   allocate (model_v(iv%info(profiler)%max_lev,iv%info(profiler)%n1:iv%info(profiler)%n2))

   model_u(:,:) = 0.0
   model_v(:,:) = 0.0

   if ( it > 1 ) then
      do n=iv%info(profiler)%n1,iv%info(profiler)%n2
         do k = 1, iv%info(profiler)%levels(n)
            if (iv%profiler(n)%u(k)%qc == fails_error_max) iv%profiler(n)%u(k)%qc = 0
            if (iv%profiler(n)%v(k)%qc == fails_error_max) iv%profiler(n)%v(k)%qc = 0
         end do
      end do
   end if

   do n=iv%info(profiler)%n1,iv%info(profiler)%n2

      ! [1.3] Get horizontal interpolation weights:

      i   = iv%info(profiler)%i(1,n)
      j   = iv%info(profiler)%j(1,n)
      dx  = iv%info(profiler)%dx(1,n)
      dy  = iv%info(profiler)%dy(1,n)
      dxm = iv%info(profiler)%dxm(1,n)
      dym = iv%info(profiler)%dym(1,n)

      do k=kts,kte
         v_h(k) = dym*(dxm*grid%xb%h(i,j,k) + dx*grid%xb%h(i+1,j,k)) + dy *(dxm*grid%xb%h(i,j+1,k) + dx*grid%xb%h(i+1,j+1,k))
         v_p(k) = dym*(dxm*grid%xb%p(i,j,k) + dx*grid%xb%p(i+1,j,k)) + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
      end do

      do k=1, iv%info(profiler)%levels(n)
         if (iv % profiler(n) % p(k) > 1.0) then
            call da_to_zk(iv % profiler(n) % p(k), v_p, v_interp_p, iv%info(profiler)%zk(k,n))
         else if (iv % profiler(n) % h(k) > missing_r) then
            call da_to_zk(iv % profiler(n) % h(k), v_h, v_interp_h, iv%info(profiler)%zk(k,n))
         end if

         if (iv%info(profiler)%zk(k,n) < 0.0 .and.  .not.anal_type_verify) then
            iv % profiler(n) % u(k) % qc = missing_data
            iv % profiler(n) % v(k) % qc = missing_data
         end if
      end do
   end do

   call da_convert_zk (iv%info(profiler))

   ! [1.4] Interpolate horizontally:
#ifdef A2C
   call da_interp_lin_3d (grid%xb%u, iv%info(profiler), model_u,'u')
   call da_interp_lin_3d (grid%xb%v, iv%info(profiler), model_v,'v')
#else
   call da_interp_lin_3d (grid%xb%u, iv%info(profiler), model_u)
   call da_interp_lin_3d (grid%xb%v, iv%info(profiler), model_v)
#endif


   do n=iv%info(profiler)%n1,iv%info(profiler)%n2
      !------------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !------------------------------------------------------------------------

      do k = 1, iv%info(profiler)%levels(n)
         !----------------------------------------------------------------
         ! [4.0] Fast interpolation:
         !----------------------------------------------------------------

          if (wind_sd_profiler) then
              call da_ffdduv_model (speed,direction,model_u(k,n), model_v(k,n), convert_uv2fd)

              if (ob%profiler(n)%u(k) > missing_r .AND. iv%profiler(n)%u(k)%qc >= obs_qc_pointer) then
                  iv%profiler(n)%u(k)%inv = ob%profiler(n)%u(k) - speed
              end if

              if (ob%profiler(n)%v(k) > missing_r .AND. iv%profiler(n)%v(k)%qc >= obs_qc_pointer) then
                  iv%profiler(n)%v(k)%inv = ob%profiler(n)%v(k) - direction
                  if (iv%profiler(n)%v(k)%inv > 180.0 ) iv%profiler(n)%v(k)%inv = iv%profiler(n)%v(k)%inv - 360.0
                  if (iv%profiler(n)%v(k)%inv < -180.0 ) iv%profiler(n)%v(k)%inv = iv%profiler(n)%v(k)%inv + 360.0
              end if
          else
              if (ob%profiler(n)%u(k) > missing_r .AND. iv%profiler(n)%u(k)%qc >= obs_qc_pointer) then
                  iv%profiler(n)%u(k)%inv = ob%profiler(n)%u(k) - model_u(k,n)
              end if

              if (ob%profiler(n)%v(k) > missing_r .AND. iv%profiler(n)%v(k)%qc >= obs_qc_pointer) then
                  iv%profiler(n)%v(k)%inv = ob%profiler(n)%v(k) - model_v(k,n)
              end if
          end if
      end do

      !------------------------------------------------------------------
      ! [5.0] Perform optional maximum error check:
      !------------------------------------------------------------------

   end do

   if ( check_max_iv ) &
      call da_check_max_iv_profiler(iv, it,num_qcstat_conv)      

   deallocate (model_u)
   deallocate (model_v)
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_profiler")

end subroutine da_get_innov_vector_profiler


