subroutine da_get_innov_vector_bogus(it,num_qcstat_conv, grid, ob, iv)

   !------------------------------------------------------------------------------
   ! Purpose: calculate the innovations for the bogus data.
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !------------------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid     ! first guess state.
   type(y_type),     intent(in)    :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)


   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.

   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.
   real    :: v_p(kms:kme)      ! Model value p at ob hor. location.

   real, allocatable :: model_u(:,:)  ! Model value u at ob location.
   real, allocatable :: model_v(:,:)  ! Model value v at ob location.
   real, allocatable :: model_t(:,:)  ! Model value t at ob location.
   real, allocatable :: model_q(:,:)  ! Model value t at ob location.
   real    :: model_slp                  ! Model value slp at ob location.

   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_bogus")

   allocate (model_u(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))
   allocate (model_v(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))
   allocate (model_t(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))
   allocate (model_q(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))
   model_u(:,:) = 0.0
   model_v(:,:) = 0.0
   model_t(:,:) = 0.0
   model_q(:,:) = 0.0

   if ( it > 1 ) then
      do n=iv%info(bogus)%n1,iv%info(bogus)%n2
         do k=1, iv%info(bogus)%levels(n)
            if (iv%bogus(n)%u(k)%qc == fails_error_max) iv%bogus(n)%u(k)%qc = 0
            if (iv%bogus(n)%v(k)%qc == fails_error_max) iv%bogus(n)%v(k)%qc = 0
            if (iv%bogus(n)%t(k)%qc == fails_error_max) iv%bogus(n)%t(k)%qc = 0
            if (iv%bogus(n)%q(k)%qc == fails_error_max) iv%bogus(n)%q(k)%qc = 0
         end do
      end do
   end if

   do n=iv%info(bogus)%n1,iv%info(bogus)%n2
      num_levs = iv%info(bogus)%levels(n)

      if (num_levs < 1) cycle

      i   = iv%info(bogus)%i(1,n)
      j   = iv%info(bogus)%j(1,n)
      dx  = iv%info(bogus)%dx(1,n)
      dy  = iv%info(bogus)%dy(1,n)
      dxm = iv%info(bogus)%dxm(1,n)
      dym = iv%info(bogus)%dym(1,n)

      do k=kts,kte
         v_h(k) = dym*(dxm*grid%xb%h(i,j,k) + dx*grid%xb%h(i+1,j,k)) + dy *(dxm*grid%xb%h(i,j+1,k) + dx*grid%xb%h(i+1,j+1,k))
         v_p(k) = dym*(dxm*grid%xb%p(i,j,k) + dx*grid%xb%p(i+1,j,k)) + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
      end do

      do k=1, iv%info(bogus)%levels(n)
         if (iv % bogus(n) % p(k) > 1.0) then
            call da_to_zk(iv % bogus(n) % p(k), v_p, v_interp_p, iv%info(bogus)%zk(k,n))
         else if (iv % bogus(n) % h(k) > 0.0) then
            call da_to_zk(iv % bogus(n) % h(k), v_h, v_interp_h, iv%info(bogus)%zk(k,n))
         end if

         if (iv%info(bogus)%zk(k,n) < 0.0 .and.  .not.anal_type_verify) then
            iv % bogus(n) % u(k) % qc = missing_data
            iv % bogus(n) % v(k) % qc = missing_data
            iv % bogus(n) % t(k) % qc = missing_data
            iv % bogus(n) % q(k) % qc = missing_data
         end if
      end do
   end do

   call da_convert_zk (iv%info(bogus))

   ! [1.4] Interpolate horizontally:

#ifdef A2C
   call da_interp_lin_3d (grid%xb%u, iv%info(bogus), model_u,'u')
   call da_interp_lin_3d (grid%xb%v, iv%info(bogus), model_v,'v')
#else
   call da_interp_lin_3d (grid%xb%u, iv%info(bogus), model_u)
   call da_interp_lin_3d (grid%xb%v, iv%info(bogus), model_v)
#endif
   call da_interp_lin_3d (grid%xb%t, iv%info(bogus), model_t)
   call da_interp_lin_3d (grid%xb%q, iv%info(bogus), model_q)

   do n=iv%info(bogus)%n1,iv%info(bogus)%n2
      num_levs = iv%info(bogus)%levels(n)

      if (num_levs < 1) cycle

      i   = iv%info(bogus)%i(1,n)
      j   = iv%info(bogus)%j(1,n)
      dx  = iv%info(bogus)%dx(1,n)
      dy  = iv%info(bogus)%dy(1,n)
      dxm = iv%info(bogus)%dxm(1,n)
      dym = iv%info(bogus)%dym(1,n)

      model_slp = dym*(dxm*grid%xb%slp(i,j)   + dx*grid%xb%slp(i+1,j)) &
         + dy *(dxm*grid%xb%slp(i,j+1) + dx*grid%xb%slp(i+1,j+1))

      !------------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !------------------------------------------------------------------------

      iv % bogus(n) % slp % inv = 0.0

      if (ABS(ob % bogus(n) % slp - missing_r) > 1.0 .AND. &
           iv % bogus(n) % slp % qc >= obs_qc_pointer) then
        iv % bogus(n) % slp % inv = ob % bogus(n) % slp - model_slp
      end if

      do k = 1, iv%info(bogus)%levels(n)
         iv % bogus(n) % u(k) % inv = 0.0
         iv % bogus(n) % v(k) % inv = 0.0
         iv % bogus(n) % t(k) % inv = 0.0
         iv % bogus(n) % q(k) % inv = 0.0

         !------------------------------------------------------------------------
         ! [4.0] Fast interpolation:
         !------------------------------------------------------------------------

         if (ob % bogus(n) % u(k) > missing_r .AND. iv % bogus(n) % u(k) % qc >= obs_qc_pointer) then
           iv % bogus(n) % u(k) % inv = ob % bogus(n) % u(k) - model_u(k,n)
         end if

         if (ob % bogus(n) % v(k) > missing_r .AND. iv % bogus(n) % v(k) % qc >= obs_qc_pointer) then
           iv % bogus(n) % v(k) % inv = ob % bogus(n) % v(k) - model_v(k,n)
         end if

         if (ob % bogus(n) % t(k) > missing_r .AND. iv % bogus(n) % t(k) % qc >= obs_qc_pointer) then
            ! only for global Bogus(YRG 07/15/2005):
            if (iv%info(bogus)%platform(n)(8:12) /= 'TCBOG') then
               iv % bogus(n) % t(k) % inv = ob % bogus(n) % t(k) - model_t(k,n)
            else
               iv % bogus(n) % t(k) % inv = missing_r 
               iv % bogus(n) % t(k) % qc  = missing_data
            end if
         end if

         if (ob % bogus(n) % q(k) > missing_r .AND. iv % bogus(n) % q(k) % qc >= obs_qc_pointer) then
            ! only for global Bogus(YRG 07/15/2005):
            if (iv%info(bogus)%platform(n)(8:12) /= 'TCBOG') then
               iv % bogus(n) % q(k) % inv = ob % bogus(n) % q(k) - model_q(k,n)
            else
              iv % bogus(n) % q(k) % inv = missing_r 
              iv % bogus(n) % q(k) % qc  = missing_data
            end if
         end if
      end do
   end do

   !------------------------------------------------------------------------
   ! [5.0] Perform optional maximum error check:
   !------------------------------------------------------------------------

   if ( check_max_iv ) &
      call da_check_max_iv_bogus(iv,ob, it, num_qcstat_conv)

    deallocate (model_u)
    deallocate (model_v)
    deallocate (model_t)
    deallocate (model_q)

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_bogus")

end subroutine da_get_innov_vector_bogus


