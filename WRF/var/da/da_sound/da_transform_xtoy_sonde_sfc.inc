subroutine da_transform_xtoy_sonde_sfc (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   type (domain),     intent(inout) :: grid
   type (iv_type),    intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),     intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n        ! Loop counter.

   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_t(:,:)
   real, allocatable :: model_q(:,:)
   real, allocatable :: model_psfc(:)

   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_sonde_sfc")

   if (sfc_assi_options == sfc_assi_options_1) then
      allocate (model_u(1,iv%info(sound)%n1:iv%info(sound)%n2))
      allocate (model_v(1,iv%info(sound)%n1:iv%info(sound)%n2))
      allocate (model_t(1,iv%info(sound)%n1:iv%info(sound)%n2))
      allocate (model_q(1,iv%info(sound)%n1:iv%info(sound)%n2))
      allocate (model_psfc(iv%info(sound)%n1:iv%info(sound)%n2))

      allocate (ub(1,iv%info(sound)%n1:iv%info(sound)%n2))
      allocate (vb(1,iv%info(sound)%n1:iv%info(sound)%n2))

      ! [1.2] Interpolate horizontally:
#ifdef A2C
      call da_interp_lin_3d (grid%xa%u, iv%info(sonde_sfc), model_u,'u')
      call da_interp_lin_3d (grid%xa%v, iv%info(sonde_sfc), model_v,'v')
#else
      call da_interp_lin_3d (grid%xa%u, iv%info(sonde_sfc), model_u)
      call da_interp_lin_3d (grid%xa%v, iv%info(sonde_sfc), model_v)
#endif
      call da_interp_lin_3d (grid%xa%t, iv%info(sonde_sfc), model_t)
      call da_interp_lin_3d (grid%xa%q, iv%info(sonde_sfc), model_q)

      call da_interp_lin_2d (grid%xa%psfc, iv%info(sonde_sfc), 1, model_psfc)

      call da_interp_lin_3d (grid%xb%u, iv%info(sonde_sfc), ub)
      call da_interp_lin_3d (grid%xb%v, iv%info(sonde_sfc), vb)

      do n=iv%info(sound)%n1,iv%info(sound)%n2
         if(wind_sd_sound)then
            call da_uv_to_sd_lin(y%sonde_sfc(n)%u,y%sonde_sfc(n)%v,model_u(1,n),model_v(1,n),ub(1,n),vb(1,n))
         else
            y%sonde_sfc(n)%u = model_u(1,n)
            y%sonde_sfc(n)%v = model_v(1,n)
         end if
         y%sonde_sfc(n)%t = model_t(1,n)
         y%sonde_sfc(n)%q = model_q(1,n)
         y%sonde_sfc(n)%p = model_psfc(n)
      end do

      deallocate (model_u)
      deallocate (model_v)
      deallocate (model_t)
      deallocate (model_q)
      deallocate (model_psfc)
      deallocate (ub)
      deallocate (vb)

   else if (sfc_assi_options == sfc_assi_options_2) then
      call da_transform_xtopsfc(grid, iv, sonde_sfc, iv%sonde_sfc(:), y%sonde_sfc(:))
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_sonde_sfc")

end subroutine da_transform_xtoy_sonde_sfc


