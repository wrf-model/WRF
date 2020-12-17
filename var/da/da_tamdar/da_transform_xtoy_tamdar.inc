subroutine da_transform_xtoy_tamdar (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),     intent(in)    :: grid
   type (iv_type),    intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),     intent(inout) :: y        ! y = h (grid%xa) (linear)

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: t(:,:)
   real, allocatable :: q(:,:)

   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   integer :: n, k

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_tamdar")

   allocate (u(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))
   allocate (v(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))
   allocate (t(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))
   allocate (q(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))

   allocate (ub(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))
   allocate (vb(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))

#ifdef A2C  
   call da_interp_lin_3d (grid%xa%u, iv%info(tamdar), u, 'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(tamdar), v, 'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(tamdar), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(tamdar), v)
#endif
   call da_interp_lin_3d (grid%xa%t, iv%info(tamdar), t)
   call da_interp_lin_3d (grid%xa%q, iv%info(tamdar), q)

   call da_interp_lin_3d (grid%xb%u, iv%info(tamdar), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(tamdar), vb)

   do n=iv%info(tamdar)%n1,iv%info(tamdar)%n2
      do k = 1, iv%info(tamdar)%levels(n)
         if(wind_sd_tamdar) then
            call da_uv_to_sd_lin(y%tamdar(n)%u(k),y%tamdar(n)%v(k),u(k,n),v(k,n),ub(k,n),vb(k,n))
         else
            y%tamdar(n)%u(k) = u(k,n)
            y%tamdar(n)%v(k) = v(k,n)
         end if
      end do
      y%tamdar(n)%t(:) = t(1:size(y%tamdar(n)%t),n)
      y%tamdar(n)%q(:) = q(1:size(y%tamdar(n)%q),n)
   end do

   deallocate (u)
   deallocate (v)
   deallocate (t)
   deallocate (q)
   deallocate (ub)
   deallocate (vb)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_tamdar")

end subroutine da_transform_xtoy_tamdar


