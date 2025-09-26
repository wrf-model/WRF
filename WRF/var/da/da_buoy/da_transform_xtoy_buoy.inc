subroutine da_transform_xtoy_buoy (grid, iv, y)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !--------------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n        ! Loop counter.

   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_t(:,:)
   real, allocatable :: model_q(:,:)
   real, allocatable :: model_psfc(:)

   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_buoy")

   if (sfc_assi_options == sfc_assi_options_1) then
      allocate (model_u(1,iv%info(buoy)%n1:iv%info(buoy)%n2))
      allocate (model_v(1,iv%info(buoy)%n1:iv%info(buoy)%n2))
      allocate (model_t(1,iv%info(buoy)%n1:iv%info(buoy)%n2))
      allocate (model_q(1,iv%info(buoy)%n1:iv%info(buoy)%n2))
      allocate (model_psfc(iv%info(buoy)%n1:iv%info(buoy)%n2))

      allocate (ub(1,iv%info(buoy)%n1:iv%info(buoy)%n2))
      allocate (vb(1,iv%info(buoy)%n1:iv%info(buoy)%n2))

      ! [1.2] Interpolate horizontally:
#ifdef A2C
      call da_interp_lin_3d (grid%xa%u, iv%info(buoy), model_u,'u')
      call da_interp_lin_3d (grid%xa%v, iv%info(buoy), model_v,'v')
#else
      call da_interp_lin_3d (grid%xa%u, iv%info(buoy), model_u)
      call da_interp_lin_3d (grid%xa%v, iv%info(buoy), model_v)
#endif
      call da_interp_lin_3d (grid%xa%t, iv%info(buoy), model_t)
      call da_interp_lin_3d (grid%xa%q, iv%info(buoy), model_q)

      call da_interp_lin_2d (grid%xa%psfc, iv%info(buoy), 1, model_psfc)

      call da_interp_lin_3d (grid%xb%u, iv%info(buoy), ub)
      call da_interp_lin_3d (grid%xb%v, iv%info(buoy), vb)

      do n=iv%info(buoy)%n1,iv%info(buoy)%n2
         if(wind_sd_buoy)then
            call da_uv_to_sd_lin(y%buoy(n)%u,y%buoy(n)%v,model_u(1,n),model_v(1,n),ub(1,n),vb(1,n))
         else
            y%buoy(n)%u = model_u(1,n)
            y%buoy(n)%v = model_v(1,n)
         end if
         y%buoy(n)%t = model_t(1,n)
         y%buoy(n)%q = model_q(1,n)
         y%buoy(n)%p = model_psfc(n)
      end do
      deallocate (model_u)
      deallocate (model_v)
      deallocate (model_t)
      deallocate (model_q)
      deallocate (model_psfc)
      deallocate (ub)
      deallocate (vb)

   else if (sfc_assi_options == sfc_assi_options_2) then
      ! [2.0] Surface assimilation approach 2
      call da_transform_xtopsfc(grid,iv,buoy,iv%buoy(:),y%buoy(:))
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_buoy")

end subroutine da_transform_xtoy_buoy


