subroutine da_get_innov_vector_ssmt1(it,num_qcstat_conv,grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid       ! first guess state.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)


   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real, allocatable :: model_t(:,:)  ! Model value t at ob location.

   real   :: v_h(kms:kme)      ! Model value h at ob hor. location.
   real   :: v_p(kms:kme)      ! Model value p at ob hor. location.

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_ssmt1")

   allocate (model_t(1:iv%info(ssmt1)%max_lev,iv%info(ssmt1)%n1:iv%info(ssmt1)%n2))
   model_t(:,:) = 0.0

   if ( it > 1 )then
      do n=iv%info(ssmt1)%n1,iv%info(ssmt1)%n2
         do k = 1, iv%info(ssmt1)%levels(n)
            if(iv % ssmt1(n) % t(k) % qc == fails_error_max)iv % ssmt1(n) % t(k) % qc = 0
         end do
      end do
   end if

   do n=iv%info(ssmt1)%n1,iv%info(ssmt1)%n2

      num_levs = iv%info(ssmt1)%levels(n)

      if (num_levs < 1) cycle

      ! [1.1] Get horizontal interpolation weights:

      i   = iv%info(ssmt1)%i(1,n)
      j   = iv%info(ssmt1)%j(1,n)
      dx  = iv%info(ssmt1)%dx(1,n)
      dy  = iv%info(ssmt1)%dy(1,n)
      dxm = iv%info(ssmt1)%dxm(1,n)
      dym = iv%info(ssmt1)%dym(1,n)

      do k=kts,kte
         v_h(k) = dym*(dxm*grid%xb%h(i,j,k)+dx*grid%xb%h(i+1,j,k)) + dy*(dxm*grid%xb%h(i,j+1,k)+dx*grid%xb%h(i+1,j+1,k))
         v_p(k) = dym*(dxm*grid%xb%p(i,j,k)+dx*grid%xb%p(i+1,j,k)) + dy*(dxm*grid%xb%p(i,j+1,k)+dx*grid%xb%p(i+1,j+1,k))
      end do

      num_levs=0

      do k=1, iv%info(ssmt1)%levels(n)
         if (iv % ssmt1(n) % h(k) > 0.0) then
            call da_to_zk(iv % ssmt1(n) % h(k), v_h, v_interp_h, iv%info(ssmt1)%zk(k,n))
         else if (iv % ssmt1(n) % p(k) > 1.0) then
            call da_to_zk(iv % ssmt1(n) % p(k), v_p, v_interp_p, iv%info(ssmt1)%zk(k,n))
         end if

         if ( iv%info(ssmt1)%zk(k,n) > 0.0) then
            num_levs=num_levs+1
            iv%info(ssmt1)%zk(num_levs,n)=iv%info(ssmt1)%zk(k,n)

            ob % ssmt1(n) % t(num_levs)         = ob % ssmt1(n) % t(k)
            iv % ssmt1(n) % t(num_levs) % qc    = iv % ssmt1(n) % t(k) % qc
            iv % ssmt1(n) % t(num_levs) % error = iv % ssmt1(n) % t(k) % error
         end if
      end do

      iv%info(ssmt1)%levels(n) = num_levs
   end do

   call da_convert_zk (iv%info(ssmt1))

   ! [1.2] Interpolate horizontally to ob:

   call da_interp_lin_3d (grid%xb%t, iv%info(ssmt1), model_t)

   do n=iv%info(ssmt1)%n1,iv%info(ssmt1)%n2

      !---------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !---------------------------------------------------------------------

      do k = 1, iv%info(ssmt1)%levels(n)
         iv % ssmt1(n) % t(k) % inv = 0.0

         !----------------------------------------------------------------
         ! [3.0] Interpolation:
         !----------------------------------------------------------------

         if (ob % ssmt1(n) % t(k) > missing_r .AND. &
             iv % ssmt1(n) % t(k) % qc >= obs_qc_pointer) then
            iv % ssmt1(n) % t(k) % inv = ob % ssmt1(n) % t(k) - model_t(k,n)
         end if
      end do
   end do

   !------------------------------------------------------------------
   ! [5.0] Perform optional maximum error check:
   !------------------------------------------------------------------

   if (check_max_iv) call da_check_max_iv_ssmt1(iv, it, num_qcstat_conv)

   deallocate (model_t)
    
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_ssmt1")

end subroutine da_get_innov_vector_ssmt1


