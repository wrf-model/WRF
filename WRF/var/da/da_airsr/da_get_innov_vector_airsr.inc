subroutine da_get_innov_vector_airsr( it,num_qcstat_conv, grid, ob, iv)

   !----------------------------------------------------------------------
   ! Purpose:   a) Rcomputes innovation vecrot at
   !                   AIRS retrieval locations 
   !                b) Does quality control check on innovation vector
   !                   if required.
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid       ! first guess state.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)


   integer :: n  ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.

   ! real    :: model_h(1:max_ob_levels)  ! Model value h at ob location.

   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.
   real    :: v_p(kms:kme)      ! Model value p at ob hor. location.

   real, allocatable :: model_q(:,:)  ! Model value q at ob location.
   real, allocatable :: model_t(:,:)  ! Model value t at ob location.

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_airsr")

   allocate (model_t(iv%info(airsr)%max_lev,iv%info(airsr)%n1:iv%info(airsr)%n2))
   allocate (model_q(iv%info(airsr)%max_lev,iv%info(airsr)%n1:iv%info(airsr)%n2))

   model_t(:,:) = 0.0
   model_q(:,:) = 0.0

   if ( it > 1 ) then
      do n = iv%info(airsr)%n1, iv%info(airsr)%n2
         do k = 1, iv%info(airsr)%levels(n)
            if (iv%airsr(n)%t(k)%qc == fails_error_max) iv%airsr(n)%t(k)%qc = 0
            if (iv%airsr(n)%q(k)%qc == fails_error_max) iv%airsr(n)%q(k)%qc = 0
         end do
      end do
   end if

   do n=iv%info(airsr)%n1, iv%info(airsr)%n2
      num_levs = iv%info(airsr)%levels(n)

      if (num_levs < 1) cycle

      ! [1.1] Get horizontal interpolation weights:

      i   = iv%info(airsr)%i(1,n)
      j   = iv%info(airsr)%j(1,n)
      dx  = iv%info(airsr)%dx(1,n)
      dy  = iv%info(airsr)%dy(1,n)
      dxm = iv%info(airsr)%dxm(1,n)
      dym = iv%info(airsr)%dym(1,n)

      do k=kts,kte
         v_h(k) = dym*(dxm*grid%xb%h(i,j,k) + dx*grid%xb%h(i+1,j,k)) + dy *(dxm*grid%xb%h(i,j+1,k) + dx*grid%xb%h(i+1,j+1,k))
         v_p(k) = dym*(dxm*grid%xb%p(i,j,k) + dx*grid%xb%p(i+1,j,k)) + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
      end do

      do k=1, num_levs
         if (iv%airsr(n)%p(k) > 1.0) then
            call da_to_zk(iv%airsr(n)%p(k), v_p, v_interp_p, iv%info(airsr)%zk(k,n))
         else if (iv%airsr(n)%h(k) > 0.0) then
            call da_to_zk(iv%airsr(n)%h(k), v_h, v_interp_h, iv%info(airsr)%zk(k,n))
         end if

         if (iv%info(airsr)%zk(k,n) < 0.0 .and. .not. anal_type_verify) then
            iv%airsr(n)%t(k)%qc = missing_data
            iv%airsr(n)%q(k)%qc = missing_data
         end if
      end do
   end do

   call da_convert_zk (iv%info(airsr))

   ! [1.2] Interpolate horizontally to ob:
   call da_interp_lin_3d (grid%xb%t, iv%info(airsr), model_t)
   call da_interp_lin_3d (grid%xb%q, iv%info(airsr), model_q)

   do n=iv%info(airsr)%n1,iv%info(airsr)%n2
      !------------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !------------------------------------------------------------------------

      do k = 1, iv%info(airsr)%levels(n)
         iv%airsr(n)%t(k)%inv = 0.0
         iv%airsr(n)%q(k)%inv = 0.0

         !------------------------------------------------------------------------
         ! [3.0] Interpolation:
         !------------------------------------------------------------------------

         if (ob%airsr(n)%t(k) > missing_r .AND. iv%airsr(n)%t(k)%qc >= obs_qc_pointer) then
           iv%airsr(n)%t(k)%inv = ob%airsr(n)%t(k) - model_t(k,n)
         end if

         if (ob%airsr(n)%q(k) > missing_r .AND. iv%airsr(n)%q(k)%qc >= obs_qc_pointer) then
            iv%airsr(n)%q(k)%inv = ob%airsr(n)%q(k) - model_q(k,n)
         end if
      end do
   end do

   !------------------------------------------------------------------------
   ! [5.0] Perform optional maximum error check:
   !------------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_airsr(iv, it, num_qcstat_conv)     
 
   deallocate (model_t)
   deallocate (model_q)
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_airsr")

end subroutine da_get_innov_vector_airsr


