subroutine da_transform_xtoy_radar (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: calculate the Doppler radial velocity and 
   ! reflectivity at the observation location from the first guess.
   ! It is linearized. 
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !---------------------------------------------------------------------
 
   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n                   ! Loop counter.      
   integer :: k                         ! Index dimension.   

   real, allocatable :: model_p(:,:)
   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_w(:,:)
   real, allocatable :: model_qrn(:,:)
   real, allocatable :: model_qrnb(:,:)
   real, allocatable :: model_ps(:)
   real    :: xr,yr,zr

   real    :: alog_10

   if (trace_use) call da_trace_entry("da_transform_xtoy_radar")

   alog_10 = alog(10.0)

   allocate (model_p(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_u(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_v(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_w(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qrn(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qrnb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_ps(iv%info(radar)%n1:iv%info(radar)%n2))

   do n=iv%info(radar)%n1,iv%info(radar)%n2
      do k = 1, iv%info(radar)%levels(n)
         model_qrnb(k,n) = iv%radar(n)%model_qrn(k)
         model_p(k,n)    = iv%radar(n)%model_p(k)
      end do

      model_ps(n) = iv%radar(n)%model_ps
   end do

   ! [1.4] Interpolate horizontally from dot points:
#ifdef A2C
   call da_interp_lin_3d (grid%xa%u,   iv%info(radar), model_u,'u')
   call da_interp_lin_3d (grid%xa%v,   iv%info(radar), model_v,'v')
#else
   call da_interp_lin_3d (grid%xa%u,   iv%info(radar), model_u)
   call da_interp_lin_3d (grid%xa%v,   iv%info(radar), model_v)
#endif
   call da_interp_lin_3d (grid%xa%qrn, iv%info(radar), model_qrn)
   call da_interp_lin_3d (grid%xa%wh,  iv%info(radar), model_w)

   do n=iv%info(radar)%n1,iv%info(radar)%n2

      ! [1.7] Calculate rv and rf at OBS location

      xr = grid%xb%ds * (iv%info(radar)%x(1,n) - iv%radar(n)%stn_loc%x)
      yr = grid%xb%ds * (iv%info(radar)%y(1,n) - iv%radar(n)%stn_loc%y)

      do k = 1, iv%info(radar)%levels(n)
         if (iv % radar(n) % height_qc(k) /= below_model_surface .and.  &
              iv % radar(n) % height_qc(k) /= above_model_lid) then
            if (use_radar_rv) then
               if (iv % radar(n) % rv(k) % qc >= obs_qc_pointer) then
                  zr=iv%radar(n)%height(k) - iv%radar(n)%stn_loc%elv

                  call da_radial_velocity_lin(y%radar(n)%rv(k), &
                     model_p(k,n), &
                     model_u(k,n), model_v(k,n), model_w(k,n), model_qrn(k,n),    &
                     model_ps(n), xr, yr, zr, model_qrnb(k,n))
               end if
            end if

            if (use_radar_rf) then
               if (iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then
                  y%radar(n)%rf(k) = leh2 * model_qrn(k,n) /(model_qrnb(k,n)*alog_10) 
               end if
            end if
         end if
      end do
   end do

   deallocate (model_p)
   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_w)
   deallocate (model_qrn)
   deallocate (model_qrnb)
   deallocate (model_ps)

   if (trace_use) call da_trace_exit("da_transform_xtoy_radar")

end subroutine da_transform_xtoy_radar 


