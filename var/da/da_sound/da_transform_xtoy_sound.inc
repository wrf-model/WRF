subroutine da_transform_xtoy_sound (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
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

   integer :: n,k

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_sound")

   allocate (u(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (v(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (t(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (q(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))

   allocate (ub(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
   allocate (vb(iv%info(sound)%max_lev,iv%info(sound)%n1:iv%info(sound)%n2))
  
#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(sound), u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(sound), v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(sound), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(sound), v)
#endif
   call da_interp_lin_3d (grid%xa%t, iv%info(sound), t)
   call da_interp_lin_3d (grid%xa%q, iv%info(sound), q)

   call da_interp_lin_3d (grid%xb%u, iv%info(sound), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(sound), vb)


   do n=iv%info(sound)%n1,iv%info(sound)%n2
      do k = 1, iv%info(sound)%levels(n)
         if(wind_sd_sound) then
            call da_uv_to_sd_lin(y%sound(n)%u(k),y%sound(n)%v(k),u(k,n),v(k,n),ub(k,n),vb(k,n))
         else
            y%sound(n)%u(k) = u(k,n)
            y%sound(n)%v(k) = v(k,n)
         end if
      end do
      y%sound(n)%t(:) = t(1:size(y%sound(n)%t),n)
      y%sound(n)%q(:) = q(1:size(y%sound(n)%q),n)
   end do

   deallocate (u)
   deallocate (v)
   deallocate (t)
   deallocate (q)
   deallocate (ub)
   deallocate (vb)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_sound")

end subroutine da_transform_xtoy_sound


