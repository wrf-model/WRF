subroutine da_transform_xtoy_airep (grid, iv, y)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !--------------------------------------------------------------------------

   implicit none

   type(domain),  intent(in)    :: grid
   type(iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type(y_type),  intent(inout) :: y        ! y = h (grid%grid%xa) (linear)

   integer :: n,k        ! Loop counter.

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: t(:,:)
   real, allocatable :: q(:,:)

   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_airep")

   allocate (u(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))
   allocate (v(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))
   allocate (t(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))
   allocate (q(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))
  
   allocate (ub(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))
   allocate (vb(iv%info(airep)%max_lev,iv%info(airep)%n1:iv%info(airep)%n2))

#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(airep), u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(airep), v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(airep), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(airep), v)
#endif
   call da_interp_lin_3d (grid%xa%t, iv%info(airep), t)
   call da_interp_lin_3d (grid%xa%q, iv%info(airep), q)

   call da_interp_lin_3d (grid%xb%u, iv%info(airep), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(airep), vb)

   do n=iv%info(airep)%n1,iv%info(airep)%n2
      do k = 1, iv%info(airep)%levels(n)
         if(wind_sd_airep) then
             call da_uv_to_sd_lin(y%airep(n)%u(k),y%airep(n)%v(k),u(k,n),v(k,n),ub(k,n),vb(k,n))
         else
             y%airep(n)%u(k) = u(k,n)
             y%airep(n)%v(k) = v(k,n)
         endif
         y%airep(n)%t(:) = t(1:size(y%airep(n)%t),n)
         y%airep(n)%q(:) = q(1:size(y%airep(n)%q),n)
      end do
   end do
  
   deallocate (u)
   deallocate (v)
   deallocate (t)
   deallocate (q)
   deallocate (ub)
   deallocate (vb)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_airep")

end subroutine da_transform_xtoy_airep


