subroutine da_get_innov_vector_qscat (it,num_qcstat_conv, grid, ob, iv)

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
   real    :: speed, direction

   real, allocatable :: model_u(:,:)  ! Model value u at ob location.
   real, allocatable :: model_v(:,:)  ! Model value v at ob location.

   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_qscat")

   allocate (model_u(iv%info(qscat)%max_lev,iv%info(qscat)%n1:iv%info(qscat)%n2))
   allocate (model_v(iv%info(qscat)%max_lev,iv%info(qscat)%n1:iv%info(qscat)%n2))

   if ( it > 1 ) then
      do n=iv%info(qscat)%n1,iv%info(qscat)%n2
         if (iv%qscat(n)%u%qc == fails_error_max) iv%qscat(n)%u%qc = 0
         if (iv%qscat(n)%v%qc == fails_error_max) iv%qscat(n)%v%qc = 0
      end do
   end if

   do n=iv%info(qscat)%n1,iv%info(qscat)%n2

      ! [1.1] Get horizontal interpolation weights:

      i   = iv%info(qscat)%i(1,n)
      j   = iv%info(qscat)%j(1,n)
      dx  = iv%info(qscat)%dx(1,n)
      dy  = iv%info(qscat)%dy(1,n)
      dxm = iv%info(qscat)%dxm(1,n)
      dym = iv%info(qscat)%dym(1,n)

      do k=kts,kte
         v_h(k) = dym*(dxm*grid%xb%h(i,j,k)+dx*grid%xb%h(i+1,j,k)) + dy*(dxm*grid%xb%h(i,j+1,k)+dx*grid%xb%h(i+1,j+1,k))
      end do

      if (iv % qscat(n) % h > missing_r) then
         call da_to_zk(iv % qscat(n) % h, v_h, v_interp_h, iv%info(qscat)%zk(1,n))
         if (iv%info(qscat)%zk(1,n) < 1.0) then
            iv%info(qscat)%zk(1,n) = 1.0
         end if
      end if
   end do

   call da_convert_zk (iv%info(qscat))

   if (.not. anal_type_verify) then
      do n=iv%info(qscat)%n1,iv%info(qscat)%n2
         if (iv%info(qscat)%zk(1,n) < 0.0) then
            iv%qscat(n)%u%qc = missing_data
            iv%qscat(n)%v%qc = missing_data
         end if
      end do
   end if

#ifdef A2C
   call da_interp_lin_3d (grid%xb%u, iv%info(qscat), model_u,'u')
   call da_interp_lin_3d (grid%xb%v, iv%info(qscat), model_v,'v')
#else
   call da_interp_lin_3d (grid%xb%u, iv%info(qscat), model_u)
   call da_interp_lin_3d (grid%xb%v, iv%info(qscat), model_v)
#endif

   do n=iv%info(qscat)%n1,iv%info(qscat)%n2

      !------------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      ! [3.0] Fast interpolation:
      !------------------------------------------------------------------------
          if (wind_sd_qscat) then
              call da_ffdduv_model (speed,direction,model_u(1,n), model_v(1,n), convert_uv2fd)

              if (ob%qscat(n)%u > missing_r .AND. iv%qscat(n)%u%qc >= obs_qc_pointer) then
                  iv%qscat(n)%u%inv = ob%qscat(n)%u - speed
              end if

              if (ob%qscat(n)%v > missing_r .AND. iv%qscat(n)%v%qc >= obs_qc_pointer) then
                  iv%qscat(n)%v%inv = ob%qscat(n)%v - direction
                  if (iv%qscat(n)%v%inv > 180.0 ) iv%qscat(n)%v%inv = iv%qscat(n)%v%inv - 360.0
                  if (iv%qscat(n)%v%inv < -180.0 ) iv%qscat(n)%v%inv = iv%qscat(n)%v%inv + 360.0
              end if
          else
               if (ob % qscat(n) % u > missing_r .AND. &
                   iv % qscat(n) % u % qc >= obs_qc_pointer) then
                   iv % qscat(n) % u % inv = ob % qscat(n) % u - model_u(1,n)
               end if

               if (ob % qscat(n) % v > missing_r .AND. &
                   iv % qscat(n) % v % qc >= obs_qc_pointer) then
                   iv % qscat(n) % v % inv = ob % qscat(n) % v - model_v(1,n)
               end if
          end if
   end do

   !------------------------------------------------------------------------
   ! [5.0] Perform optional maximum error check:
   !------------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_qscat(iv, it, num_qcstat_conv)       

   deallocate (model_u)
   deallocate (model_v)

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_qscat")

end subroutine da_get_innov_vector_qscat


