subroutine da_transform_xtoy_polaramv (grid, iv, y)

   !----------------------------------------------------------------------
   ! Purpose: X to Y Transform operator for Polar AMV's               
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !----------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer           :: n, k ! Loop counter.
   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_polaramv")

   allocate (u(iv%info(polaramv)%max_lev,iv%info(polaramv)%n1:iv%info(polaramv)%n2))
   allocate (v(iv%info(polaramv)%max_lev,iv%info(polaramv)%n1:iv%info(polaramv)%n2))
   allocate (ub(iv%info(polaramv)%max_lev,iv%info(polaramv)%n1:iv%info(polaramv)%n2))
   allocate (vb(iv%info(polaramv)%max_lev,iv%info(polaramv)%n1:iv%info(polaramv)%n2))

#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(polaramv), u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(polaramv), v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(polaramv), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(polaramv), v)
#endif
   call da_interp_lin_3d (grid%xb%u, iv%info(polaramv), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(polaramv), vb)

   do n=iv%info(polaramv)%n1,iv%info(polaramv)%n2
      do k = 1, iv%info(polaramv)%levels(n)

         if(wind_sd_polaramv) then
            call da_uv_to_sd_lin(y%polaramv(n)%u(k),y%polaramv(n)%v(k),u(k,n),v(k,n),ub(k,n),vb(k,n))
         else
            y%polaramv(n)%u(k) = u(k,n)
            y%polaramv(n)%v(k) = v(k,n)
         end if

      end do
   end do

   deallocate (u)
   deallocate (v)
   deallocate (ub)
   deallocate (vb)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_polaramv")

end subroutine da_transform_xtoy_polaramv


