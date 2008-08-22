subroutine da_get_innov_vector_pilot( it,num_qcstat_conv, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD     
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid     ! first guess state.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.

   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.

   real, allocatable :: model_u(:,:)  ! Model value u at ob location.
   real, allocatable :: model_v(:,:)  ! Model value v at ob location.

   real    :: v_p(kms:kme)      ! Model value p at ob hor. location.
   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_pilot")

   allocate (model_u(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))
   allocate (model_v(iv%info(pilot)%max_lev,iv%info(pilot)%n1:iv%info(pilot)%n2))

   model_u(:,:) = 0.0
   model_v(:,:) = 0.0

   do n=iv%info(pilot)%n1,iv%info(pilot)%n2
      ! [1.3] Get horizontal interpolation weights:

      i   = iv%info(pilot)%i(1,n)
      j   = iv%info(pilot)%j(1,n)
      dx  = iv%info(pilot)%dx(1,n)
      dy  = iv%info(pilot)%dy(1,n)
      dxm = iv%info(pilot)%dxm(1,n)
      dym = iv%info(pilot)%dym(1,n)

      do k=kts,kte
         v_p(k) = dym*(dxm*grid%xb%p(i,j,k) + dx*grid%xb%p(i+1,j,k)) + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
      end do

      do k=1, iv%info(pilot)%levels(n)
         if (iv % pilot(n) % p(k) > 1.0) then
            call da_to_zk(iv % pilot(n) % p(k), v_p, v_interp_p, iv%info(pilot)%zk(k,n))
         end if

         if (iv%info(pilot)%zk(k,n) < 0.0 .and.  .not.anal_type_verify) then
            iv % pilot(n) % u(k) % qc = missing
            iv % pilot(n) % v(k) % qc = missing
         end if
      end do
   end do

   call da_convert_zk (iv%info(pilot))

   ! [1.4] Interpolate horizontally:
   call da_interp_lin_3d (grid%xb%u, iv%info(pilot), model_u)
   call da_interp_lin_3d (grid%xb%v, iv%info(pilot), model_v)

   do n=iv%info(pilot)%n1,iv%info(pilot)%n2
      !------------------------------------------------------------------------
      ! [2.0] Initialise components of innovation vector:
      !------------------------------------------------------------------------

      do k = 1, iv%info(pilot)%levels(n)
         iv % pilot(n) % u(k) % inv = 0.0
         iv % pilot(n) % v(k) % inv = 0.0

         !------------------------------------------------------------------------
         ! [4.0] Fast interpolation:
         !------------------------------------------------------------------------

         if (ob % pilot(n) % u(k) > missing_r .AND. iv % pilot(n) % u(k) % qc >= obs_qc_pointer) then
            iv % pilot(n) % u(k) % inv = ob % pilot(n) % u(k) - model_u(k,n)
         end if

         if (ob % pilot(n) % v(k) > missing_r .AND. iv % pilot(n) % v(k) % qc >= obs_qc_pointer) then
            iv % pilot(n) % v(k) % inv = ob % pilot(n) % v(k) - model_v(k,n)
         end if
      end do
   end do

   !------------------------------------------------------------------------
   ! [5.0] Perform optional maximum error check:
   !------------------------------------------------------------------------

   if (check_max_iv) call da_check_max_iv_pilot(iv, it, num_qcstat_conv)    

   deallocate (model_u)
   deallocate (model_v)
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_pilot")

end subroutine da_get_innov_vector_pilot


