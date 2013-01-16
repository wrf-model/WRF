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

   integer :: n

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_tamdar")

   allocate (u(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))
   allocate (v(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))
   allocate (t(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))
   allocate (q(iv%info(tamdar)%max_lev,iv%info(tamdar)%n1:iv%info(tamdar)%n2))

#ifdef A2C  
   call da_interp_lin_3d (grid%xa%u, iv%info(tamdar), u, 'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(tamdar), v, 'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(tamdar), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(tamdar), v)
#endif
   call da_interp_lin_3d (grid%xa%t, iv%info(tamdar), t)
   call da_interp_lin_3d (grid%xa%q, iv%info(tamdar), q)

   do n=iv%info(tamdar)%n1,iv%info(tamdar)%n2
      y%tamdar(n)%u(:) = u(1:size(y%tamdar(n)%u),n)
      y%tamdar(n)%v(:) = v(1:size(y%tamdar(n)%v),n)
      y%tamdar(n)%t(:) = t(1:size(y%tamdar(n)%t),n)
      y%tamdar(n)%q(:) = q(1:size(y%tamdar(n)%q),n)
   end do

   deallocate (u)
   deallocate (v)
   deallocate (t)
   deallocate (q)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_tamdar")

end subroutine da_transform_xtoy_tamdar


