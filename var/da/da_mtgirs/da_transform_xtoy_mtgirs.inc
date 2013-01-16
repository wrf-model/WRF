subroutine da_transform_xtoy_mtgirs (grid, iv, y)

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

   integer :: n

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_mtgirs")

   allocate (u(iv%info(mtgirs)%max_lev,iv%info(mtgirs)%n1:iv%info(mtgirs)%n2))
   allocate (v(iv%info(mtgirs)%max_lev,iv%info(mtgirs)%n1:iv%info(mtgirs)%n2))
   allocate (t(iv%info(mtgirs)%max_lev,iv%info(mtgirs)%n1:iv%info(mtgirs)%n2))
   allocate (q(iv%info(mtgirs)%max_lev,iv%info(mtgirs)%n1:iv%info(mtgirs)%n2))
  
#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(mtgirs), u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(mtgirs), v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(mtgirs), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(mtgirs), v)
#endif
   call da_interp_lin_3d (grid%xa%t, iv%info(mtgirs), t)
   call da_interp_lin_3d (grid%xa%q, iv%info(mtgirs), q)

   do n=iv%info(mtgirs)%n1,iv%info(mtgirs)%n2
      y%mtgirs(n)%u(:) = u(1:size(y%mtgirs(n)%u),n)
      y%mtgirs(n)%v(:) = v(1:size(y%mtgirs(n)%v),n)
      y%mtgirs(n)%t(:) = t(1:size(y%mtgirs(n)%t),n)
      y%mtgirs(n)%q(:) = q(1:size(y%mtgirs(n)%q),n)
   end do

   deallocate (u)
   deallocate (v)
   deallocate (t)
   deallocate (q)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_mtgirs")

end subroutine da_transform_xtoy_mtgirs


