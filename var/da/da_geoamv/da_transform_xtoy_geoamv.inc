subroutine da_transform_xtoy_geoamv (grid, iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: X to Y Transform operator for Geo. AMVs
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer           :: n
   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_geoamv")

   allocate (u(iv%info(geoamv)%max_lev,iv%info(geoamv)%n1:iv%info(geoamv)%n2))
   allocate (v(iv%info(geoamv)%max_lev,iv%info(geoamv)%n1:iv%info(geoamv)%n2))

#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(geoamv), u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(geoamv), v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(geoamv), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(geoamv), v)
#endif

   do n=iv%info(geoamv)%n1,iv%info(geoamv)%n2
      y%geoamv(n)%u(:) = u(1:size(y%geoamv(n)%u),n)
      y%geoamv(n)%v(:) = v(1:size(y%geoamv(n)%v),n)
   end do

   deallocate (u)
   deallocate (v)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_geoamv")

end subroutine da_transform_xtoy_geoamv


