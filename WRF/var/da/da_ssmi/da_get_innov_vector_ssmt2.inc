subroutine da_get_innov_vector_ssmt2 (it, num_qcstat_conv,grid, ob, iv)

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
   real, allocatable :: model_rh(:,:) ! Model value rh at ob location.

   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.
   real    :: v_p(kms:kme)      ! Model value p at ob hor. location.

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_ssmt2")

   allocate (model_rh(1:iv%info(ssmt2)%max_lev,iv%info(ssmt2)%n1:iv%info(ssmt2)%n2))
   model_rh(:,:) = 0.0

   if ( it > 1 )then
      do n=iv%info(ssmt2)%n1,iv%info(ssmt2)%n2
         do k = 1, iv%info(ssmt1)%levels(n)
            if(iv % ssmt2(n) % rh(k) % qc == fails_error_max)iv % ssmt2(n) % rh(k) % qc = 0
         end do
      end do
   end if

   do n=iv%info(ssmt2)%n1,iv%info(ssmt2)%n2
      num_levs = iv%info(ssmt2)%levels(n)

      if (num_levs < 1) cycle

      ! [1.1] Get horizontal interpolation weights:

      i   = iv%info(ssmt2)%i(1,n)
      j   = iv%info(ssmt2)%j(1,n)
      dx  = iv%info(ssmt2)%dx(1,n)
      dy  = iv%info(ssmt2)%dy(1,n)
      dxm = iv%info(ssmt2)%dxm(1,n)
      dym = iv%info(ssmt2)%dym(1,n)

      do k=kts,kte
         v_h(k) = dym*(dxm*grid%xb%h(i,j  ,k) + dx*grid%xb%h(i+1,j  ,k)) &
                + dy *(dxm*grid%xb%h(i,j+1,k) + dx*grid%xb%h(i+1,j+1,k))
         v_p(k) = dym*(dxm*grid%xb%p(i,j  ,k) + dx*grid%xb%p(i+1,j  ,k)) &
                + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
      end do

      num_levs=0
      do k=1, iv%info(ssmt2)%levels(n)
         if (iv % ssmt2(n) % h(k) > 0.0) then
            call da_to_zk(iv % ssmt2(n) % h(k), v_h, v_interp_h, iv%info(ssmt2)%zk(k,n))
         else if (iv % ssmt2(n) % p(k) > 1.0) then
            call da_to_zk(iv % ssmt2(n) % p(k), v_p, v_interp_p, iv%info(ssmt2)%zk(k,n))
         end if

         if (iv%info(ssmt2)%zk(k,n) > 0.0) then
            num_levs=num_levs+1
            iv%info(ssmt2)%zk(num_levs,n)=iv%info(ssmt2)%zk(k,n)

            ob % ssmt2(n) % rh(num_levs) = ob % ssmt2(n) % rh(k)

            iv % ssmt2(n) % rh(num_levs) % qc = iv % ssmt2(n) % rh(k) % qc

            iv % ssmt2(n) % rh(num_levs) % error = iv % ssmt2(n) % rh(k) % error
         end if
      end do

      iv%info(ssmt2)%levels(n) = num_levs
   end do

   call da_convert_zk (iv%info(ssmt2))

   call da_interp_lin_3d (grid%xb%rh, iv%info(ssmt2), model_rh)

   do n=iv%info(ssmt2)%n1, iv%info(ssmt2)%n2
      do k = 1, iv%info(ssmt2)%levels(n)
         iv % ssmt2(n) % rh(k) % inv = 0.0

         !-----------------------------------------------------------------
         ! [3.0] Interpolation:
         !-----------------------------------------------------------------

         if (ob % ssmt2(n) % rh(k) > missing_r .AND. &
             iv % ssmt2(n) % rh(k) % qc >= obs_qc_pointer) then
            iv % ssmt2(n) % rh(k) % inv = ob % ssmt2(n) % rh(k) - model_rh(k,n)
         end if

         ! write(122,'(2i4,i8,5f15.5)')n, k, iv%ssmt2(n)%height_qc(k), &
         ! iv%ssmt2(n)%info%lat, iv%ssmt2(n)%info%lon, &
         ! iv%ssmt2(n)%h(k), &
         ! ob % ssmt2(n) % rh(k), model_rh(k,n)

      end do
   end do

   !------------------------------------------------------------------------
   ! [5.0] Perform optional maximum error check:
   !------------------------------------------------------------------------

   if (check_max_iv) call da_check_max_iv_ssmt2(iv, it, num_qcstat_conv)           

   deallocate (model_rh)
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_ssmt2")

end subroutine da_get_innov_vector_ssmt2


