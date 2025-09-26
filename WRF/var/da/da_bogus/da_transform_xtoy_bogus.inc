subroutine da_transform_xtoy_bogus (grid, iv, y)

   !---------------------------------------------------------------------
   ! Purpose: the linearized bogus observation operators.
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !---------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs ! Number of obs levels.
   real    :: dx, dxm 
   real    :: dy, dym
   real    :: model_t(kts:kte)
   real    :: model_t9(kts:kte)
   real    :: model_q(kts:kte)
   real    :: model_q9(kts:kte)
   real    :: model_p_c(kts:kte)
   real    :: model_p_c9(kts:kte)
   real    :: psfc_loc,terr_loc,psfc_loc9

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: t(:,:)
   real, allocatable :: q(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_bogus")      

   do n = iv%info(bogus)%n1,iv%info(bogus)%n2
      num_levs = iv%info(bogus)%levels(n)

      if (num_levs < 1) cycle

      ! [1.1] Get cross pt. horizontal interpolation weights:

      i   = iv%info(bogus)%i(1,n)
      dy  = iv%info(bogus)%dy(1,n)
      dym = iv%info(bogus)%dym(1,n)
      j   = iv%info(bogus)%j(1,n)
      dx  = iv%info(bogus)%dx(1,n)
      dxm = iv%info(bogus)%dxm(1,n)

      ! [1.2] Interpolate horizontally from cross points:

      do k = kts, kte
         model_t9(k) = dym*(dxm*grid%xa%t(i,j,k) + dx*grid%xa%t(i+1,j,k)) &
            + dy *(dxm*grid%xa%t(i,j+1,k) + dx*grid%xa%t(i+1,j+1,k))
         model_t(k) = dym*(dxm*grid%xb%t(i,j,k) + dx*grid%xb%t(i+1,j,k)) &
            + dy *(dxm*grid%xb%t(i,j+1,k) + dx*grid%xb%t(i+1,j+1,k))
         model_q9(k) = dym*(dxm*grid%xa%q(i,j,k) + dx*grid%xa%q(i+1,j,k)) &
            + dy *(dxm*grid%xa%q(i,j+1,k) + dx*grid%xa%q(i+1,j+1,k))
         model_q(k) = dym*(dxm*grid%xb%q(i,j,k) + dx*grid%xb%q(i+1,j,k)) &
            + dy *(dxm*grid%xb%q(i,j+1,k) + dx*grid%xb%q(i+1,j+1,k))
         model_p_c9(k) = dym*(dxm*grid%xa%p(i,j,k) + dx*grid%xa%p(i+1,j,k)) &
            + dy *(dxm*grid%xa%p(i,j+1,k) + dx*grid%xa%p(i+1,j+1,k))
         model_p_c(k) = dym*(dxm*grid%xb%p(i,j,k) + dx*grid%xb%p(i+1,j,k)) &
            + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
      end do

      terr_loc = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) &
         + dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))
      psfc_loc = dym*(dxm*grid%xb%psfc(i,j)   + dx*grid%xb%psfc(i+1,j)) &
         + dy *(dxm*grid%xb%psfc(i,j+1) + dx*grid%xb%psfc(i+1,j+1))
      psfc_loc9 = dym*(dxm*grid%xa%psfc(i,j)   + dx*grid%xa%psfc(i+1,j)) &
         + dy *(dxm*grid%xa%psfc(i,j+1) + dx*grid%xa%psfc(i+1,j+1))

      call da_tpq_to_slp_lin (model_t, model_q, model_p_c, terr_loc, psfc_loc,   &
         model_t9, model_q9, model_p_c9, psfc_loc9, y%bogus(n)%slp) 
   end do

   allocate (u(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))
   allocate (v(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))
   allocate (t(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))
   allocate (q(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))
   u(:,:)=0.0
   v(:,:)=0.0
   t(:,:)=0.0
   q(:,:)=0.0
  
#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(bogus), u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(bogus), v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(bogus), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(bogus), v)
#endif
   call da_interp_lin_3d (grid%xa%t, iv%info(bogus), t)
   call da_interp_lin_3d (grid%xa%q, iv%info(bogus), q)

   do n=iv%info(bogus)%n1,iv%info(bogus)%n2
      y%bogus(n)%u(:) = u(1:size(y%bogus(n)%u),n)
      y%bogus(n)%v(:) = v(1:size(y%bogus(n)%v),n)
      y%bogus(n)%t(:) = t(1:size(y%bogus(n)%t),n)
      y%bogus(n)%q(:) = q(1:size(y%bogus(n)%q),n)
   end do

   deallocate (u)
   deallocate (v)
   deallocate (t)
   deallocate (q)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_bogus") 

end subroutine da_transform_xtoy_bogus


