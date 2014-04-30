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

   integer           :: n,k
   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)

   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_geoamv")

   allocate (u(iv%info(geoamv)%max_lev,iv%info(geoamv)%n1:iv%info(geoamv)%n2))
   allocate (v(iv%info(geoamv)%max_lev,iv%info(geoamv)%n1:iv%info(geoamv)%n2))

   allocate (ub(iv%info(geoamv)%max_lev,iv%info(geoamv)%n1:iv%info(geoamv)%n2))
   allocate (vb(iv%info(geoamv)%max_lev,iv%info(geoamv)%n1:iv%info(geoamv)%n2))

#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(geoamv), u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(geoamv), v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(geoamv), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(geoamv), v)
#endif

   call da_interp_lin_3d (grid%xb%u, iv%info(geoamv), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(geoamv), vb)

   do n=iv%info(geoamv)%n1,iv%info(geoamv)%n2
      do k = 1, iv%info(geoamv)%levels(n)
         if(wind_sd_geoamv) then
            call da_uv_to_sd_lin(y%geoamv(n)%u(k),y%geoamv(n)%v(k),u(k,n),v(k,n),ub(k,n),vb(k,n))
         else
            y%geoamv(n)%u(k) = u(k,n)
            y%geoamv(n)%v(k) = v(k,n)
         end if
       end do
   end do

   deallocate (u)
   deallocate (v)
   deallocate (vb)
   deallocate (ub)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_geoamv")

end subroutine da_transform_xtoy_geoamv


