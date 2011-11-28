subroutine da_transform_xtoy_bogus_adj(grid, iv, jo_grad_y, jo_grad_x)

   !---------------------------------------------------------------------
   ! Purpose: the adjoint of bogus observation operators.
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !---------------------------------------------------------------------

   implicit none

   type (domain),     intent(in)     :: grid
   type (iv_type),    intent(in)     :: iv          ! obs. inc vector (o-b).
   type (y_type) ,    intent(inout)  :: jo_grad_y   ! grad_y(jo)
   type (x_type) ,    intent(inout)  :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: num_levs
   real    :: dx, dxm
   real    :: dy, dym

   real    :: model_t(kms:kme)
   real    :: tt(kms:kme)
   real    :: model_q(kms:kme)
   real    :: qq(kms:kme)
   real    :: model_p_c(kms:kme)
   real    :: pp(kms:kme)
   real    :: psfc_loc,terr_loc,ppsfc   

   real, allocatable :: temp_u(:,:)
   real, allocatable :: temp_v(:,:)
   real, allocatable :: temp_t(:,:)
   real, allocatable :: temp_q(:,:)           

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_bogus_adj")

   allocate (temp_u(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))
   allocate (temp_v(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))
   allocate (temp_t(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))
   allocate (temp_q(iv%info(bogus)%max_lev,iv%info(bogus)%n1:iv%info(bogus)%n2))

   do n=iv%info(bogus)%n1,iv%info(bogus)%n2
      temp_u(1:size(jo_grad_y%bogus(n)%u),n)  = jo_grad_y%bogus(n)%u
      temp_v(1:size(jo_grad_y%bogus(n)%u),n)  = jo_grad_y%bogus(n)%v
      temp_t(1:size(jo_grad_y%bogus(n)%u),n)  = jo_grad_y%bogus(n)%t
      temp_q(1:size(jo_grad_y%bogus(n)%u),n)  = jo_grad_y%bogus(n)%q
   end do

   ! [1.1] Adjoint feedback from Y to X for u and v:

#ifdef A2C
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(bogus), temp_u,'u')
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(bogus), temp_v,'v')
#else
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(bogus), temp_u)
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(bogus), temp_v)
#endif
   call da_interp_lin_3d_adj (jo_grad_x%t, iv%info(bogus), temp_t)
   call da_interp_lin_3d_adj (jo_grad_x%q, iv%info(bogus), temp_q)
   deallocate (temp_u)
   deallocate (temp_v)
   deallocate (temp_t)
   deallocate (temp_q)

   do n= iv%info(bogus)%n1, iv%info(bogus)%n2
      num_levs = iv%info(bogus)%levels(n)
      if (num_levs < 1) cycle

      ! [1.1] Get cross pt. horizontal interpolation weights:

      i   = iv%info(bogus)%i(1,n)
      dy  = iv%info(bogus)%dy(1,n)
      dym = iv%info(bogus)%dym(1,n)
      j   = iv%info(bogus)%j(1,n)
      dx  = iv%info(bogus)%dx(1,n)
      dxm = iv%info(bogus)%dxm(1,n)

      ! [1.2] Compute the feedback from SLP to t and q:

      ! 1.2.1 Background at the observation location:

      do k = kts, kte
         model_t(k) = dym*(dxm*grid%xb%t(i,j,k) + dx*grid%xb%t(i+1,j,k)) &
            + dy *(dxm*grid%xb%t(i,j+1,k) + dx*grid%xb%t(i+1,j+1,k))
         model_q(k) = dym*(dxm*grid%xb%q(i,j,k) + dx*grid%xb%q(i+1,j,k)) &
            + dy *(dxm*grid%xb%q(i,j+1,k) + dx*grid%xb%q(i+1,j+1,k))
         model_p_c(k) = dym*(dxm*grid%xb%p(i,j,k) + dx*grid%xb%p(i+1,j,k)) &
            + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
      end do

      terr_loc = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) &
         + dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))
      psfc_loc = dym*(dxm*grid%xb%psfc(i,j)   + dx*grid%xb%psfc(i+1,j)) &
         + dy *(dxm*grid%xb%psfc(i,j+1) + dx*grid%xb%psfc(i+1,j+1))

      ! 1.2.2 Compute the feedback from SLP to p, t, q, and psfc 
      !       at the observed location:

      call da_tpq_to_slp_adj(model_t, model_q ,model_p_c, terr_loc, psfc_loc, &
         tt, qq, pp, ppsfc, jo_grad_y%bogus(n)%slp)  

      ! 1.2.3 feedback from the observed location to grid space:

      ! 1.2.3.1 for Psfc

      jo_grad_x % psfc(i,j)     = jo_grad_x % psfc(i,j) + dym*dxm*ppsfc
      jo_grad_x % psfc(i+1,j)   = jo_grad_x % psfc(i+1,j)+ dym*dx*ppsfc
      jo_grad_x % psfc(i,j+1)   = jo_grad_x % psfc(i,j+1)+ dy*dxm*ppsfc
      jo_grad_x % psfc(i+1,j+1) = jo_grad_x % psfc(i+1,j+1)+dy*dx*ppsfc

      ! 1.2.3.2 for t, q, p

      do k = kts, kte
         jo_grad_x % t(i,j,k)     = jo_grad_x % t(i,j,k)+dym*dxm*tt(k)
         jo_grad_x % t(i+1,j,k)   = jo_grad_x % t(i+1,j,k)+ dym*dx*tt(k)
         jo_grad_x % t(i,j+1,k)   = jo_grad_x % t(i,j+1,k)+ dy*dxm*tt(k)
         jo_grad_x % t(i+1,j+1,k) = jo_grad_x % t(i+1,j+1,k)+dy*dx*tt(k)
         jo_grad_x % q(i,j,k)     = jo_grad_x % q(i,j,k)+dym*dxm*qq(k)
         jo_grad_x % q(i+1,j,k)   = jo_grad_x % q(i+1,j,k)+ dym*dx*qq(k)
         jo_grad_x % q(i,j+1,k)   = jo_grad_x % q(i,j+1,k)+ dy*dxm*qq(k)
         jo_grad_x % q(i+1,j+1,k) = jo_grad_x % q(i+1,j+1,k)+dy*dx*qq(k)
         jo_grad_x % p(i,j,k)     = jo_grad_x % p(i,j,k)+dym*dxm*pp(k)
         jo_grad_x % p(i+1,j,k)   = jo_grad_x % p(i+1,j,k)+ dym*dx*pp(k)
         jo_grad_x % p(i,j+1,k)   = jo_grad_x % p(i,j+1,k)+ dy*dxm*pp(k)
         jo_grad_x % p(i+1,j+1,k) = jo_grad_x % p(i+1,j+1,k)+dy*dx*pp(k)
      end do
   end do                  

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_bogus_adj")

end subroutine da_transform_xtoy_bogus_adj


