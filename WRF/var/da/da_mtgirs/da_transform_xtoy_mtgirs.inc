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
   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   integer :: n,k

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_mtgirs")

   allocate (u(iv%info(mtgirs)%max_lev,iv%info(mtgirs)%n1:iv%info(mtgirs)%n2))
   allocate (v(iv%info(mtgirs)%max_lev,iv%info(mtgirs)%n1:iv%info(mtgirs)%n2))
   allocate (t(iv%info(mtgirs)%max_lev,iv%info(mtgirs)%n1:iv%info(mtgirs)%n2))
   allocate (q(iv%info(mtgirs)%max_lev,iv%info(mtgirs)%n1:iv%info(mtgirs)%n2))
  
   allocate (ub(iv%info(mtgirs)%max_lev,iv%info(mtgirs)%n1:iv%info(mtgirs)%n2))
   allocate (vb(iv%info(mtgirs)%max_lev,iv%info(mtgirs)%n1:iv%info(mtgirs)%n2))

#ifdef A2C
   call da_interp_lin_3d (grid%xa%u, iv%info(mtgirs), u,'u')
   call da_interp_lin_3d (grid%xa%v, iv%info(mtgirs), v,'v')
#else
   call da_interp_lin_3d (grid%xa%u, iv%info(mtgirs), u)
   call da_interp_lin_3d (grid%xa%v, iv%info(mtgirs), v)
#endif
   call da_interp_lin_3d (grid%xa%t, iv%info(mtgirs), t)
   call da_interp_lin_3d (grid%xa%q, iv%info(mtgirs), q)

   call da_interp_lin_3d (grid%xb%u, iv%info(mtgirs), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(mtgirs), vb)

   do n=iv%info(mtgirs)%n1,iv%info(mtgirs)%n2
      do k = 1, iv%info(mtgirs)%levels(n)
         if(wind_sd_mtgirs) then
            call da_uv_to_sd_lin(y%mtgirs(n)%u(k),y%mtgirs(n)%v(k),u(k,n),v(k,n),ub(k,n),vb(k,n))
         else
            y%mtgirs(n)%u(k) = u(k,n)
            y%mtgirs(n)%v(k) = v(k,n)
         end if
      end do
      y%mtgirs(n)%t(:) = t(1:size(y%mtgirs(n)%t),n)
      y%mtgirs(n)%q(:) = q(1:size(y%mtgirs(n)%q),n)
   end do

   deallocate (u)
   deallocate (v)
   deallocate (t)
   deallocate (q)
   deallocate (ub)
   deallocate (vb)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_mtgirs")

end subroutine da_transform_xtoy_mtgirs


