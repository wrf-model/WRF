subroutine da_transform_xtoy_radar_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: Adjoint of da_transform_xtoy_radar
   ! History:
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !    08/2017 - bug fix for Vr operator (Siou-Ying Jiang, CWB, Taiwan)
   !-----------------------------------------------------------------------

   !------------------------------------------------------------------------
   ! This subroutine is the adjoint of Doppler radar observation operators.
   !------------------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(inout) :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: k  ! Index dimension.

   integer :: n

   real, allocatable :: model_p(:,:)
   real, allocatable :: model_rho(:,:)
   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_w(:,:)
   real, allocatable :: model_qrn(:,:)
   real, allocatable :: model_qrnb(:,:)
   real, allocatable :: model_ps(:)

   real, allocatable :: model_qsn(:,:)
   real, allocatable :: model_qgr(:,:)
   real, allocatable :: model_qv(:,:)
   real, allocatable :: model_qvb(:,:)
   real, allocatable :: model_t(:,:)
   real, allocatable :: model_tb(:,:)

   real    :: xr,yr,zr

   real    :: alog10,qrn1,qsn1,qgr1

   if (trace_use) call da_trace_entry("da_transform_xtoy_radar_adj")

   alog10= alog(10.0)

   allocate (model_p(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_rho(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_u(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_v(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_w(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qrn(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qrnb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_ps(iv%info(radar)%n1:iv%info(radar)%n2))

   allocate (model_qsn(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qgr(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qv(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_t(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))

   if ( use_radar_rqv ) then
      !basic states
      allocate (model_qvb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
      allocate (model_tb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
      call da_interp_lin_3d (grid%xb%t,   iv%info(radar), model_tb)
      call da_interp_lin_3d (grid%xb%q,   iv%info(radar), model_qvb)
   end if

   ! Needed
   model_u = 0.0
   model_v = 0.0
   model_w = 0.0
   model_qrn = 0.0
   model_qsn = 0.0
   model_qgr = 0.0
   model_qv  = 0.0
   model_t   = 0.0


   ! W_HALF is vertical velocity at half-sigma levels.

   model_ps(iv%info(radar)%n1:iv%info(radar)%n2) = iv%radar(iv%info(radar)%n1:iv%info(radar)%n2)%model_ps 

   do n=iv%info(radar)%n1,iv%info(radar)%n2

      ! [1.7] Calculate rv and rf at OBS location

      xr = grid%xb%ds * (iv%info(radar)%x(1,n) - iv%radar(n)%stn_loc%x)
      yr = grid%xb%ds * (iv%info(radar)%y(1,n) - iv%radar(n)%stn_loc%y)

      model_qrnb(1:iv%info(radar)%levels(n),n) = iv%radar(n)%model_qrn(1:iv%info(radar)%levels(n))
      model_p   (1:iv%info(radar)%levels(n),n) = iv%radar(n)%model_p(1:iv%info(radar)%levels(n))
      model_rho (1:iv%info(radar)%levels(n),n) = iv%radar(n)%model_rho(1:iv%info(radar)%levels(n))

      do k = 1,iv%info(radar)%levels(n)
         if (iv % radar(n) % height_qc(k) /= below_model_surface .and.  &
              iv % radar(n) % height_qc(k) /= above_model_lid) then

            if (use_radar_rf .and. .not.use_radar_rhv) then
               if (iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then
                  model_qrn(k,n) = model_qrn(k,n) + leh2/(model_qrnb(k,n)*alog10) * jo_grad_y%radar(n)%rf(k)
               end if
            end if

            if (.not.use_radar_rf .and. use_radar_rhv) then
               if (iv % radar(n) % rrn(k) % qc >= obs_qc_pointer) then
                  model_qrn(k,n) = model_qrn(k,n) + jo_grad_y%radar(n)%rrn(k)
               end if
               if (iv % radar(n) % rsn(k) % qc >= obs_qc_pointer) then
                  model_qsn(k,n) = model_qsn(k,n) + jo_grad_y%radar(n)%rsn(k)
               end if
               if (iv % radar(n) % rgr(k) % qc >= obs_qc_pointer) then
                  model_qgr(k,n) = model_qgr(k,n) + jo_grad_y%radar(n)%rgr(k)
               end if
            end if

            if (use_radar_rqv) then
               if (iv % radar(n) % rqv(k) % qc >= obs_qc_pointer) then
               !TL  y%radar(n)%rqv(k) = y%radar(n)%rqv(k) +( es_beta*es_gamma/(model_tb(k,n)+es_gamma)**2.0 )*model_qvb(k,n)*model_t(k,n)
                  model_qv(k,n) = model_qv(k,n) + jo_grad_y%radar(n)%rqv(k)
                  model_t(k,n)  = model_t(k,n)  + (es_beta*es_gamma/(model_tb(k,n)+es_gamma)**2.0)*model_qvb(k,n)*jo_grad_y%radar(n)%rqv(k)
               end if
            end if


            if (use_radar_rv) then
               if (iv % radar(n) % rv(k) % qc >= obs_qc_pointer) then
                  zr=iv%radar(n)%height(k) - iv%radar(n)%stn_loc%elv

                  call da_radial_velocity_adj(jo_grad_y%radar(n)%rv(k), &
                     model_p(k,n), model_u(k,n), model_v(k,n), model_w(k,n),  &
                     model_qrn(k,n), model_ps(n), xr, yr, zr, model_qrnb(k,n),&
                     model_rho(k,n))

               end if
            end if
         end if
      end do
      jo_grad_y%radar(n)%rv(:) = 0.0
      jo_grad_y%radar(n)%rf(:) = 0.0
      if (use_radar_rhv) then
         jo_grad_y%radar(n)%rrn(:)= 0.0
         jo_grad_y%radar(n)%rsn(:)= 0.0
         jo_grad_y%radar(n)%rgr(:)= 0.0
      end if
      if (use_radar_rqv) then
         jo_grad_y%radar(n)%rqv(:)= 0.0
      end if
   end do ! n

   ! [1.6] Interpolate horizontally from crs points:

   call da_interp_lin_3d_adj (jo_grad_x % wh,  iv%info(radar), model_w)
   if ( cloud_cv_options >= 1 ) then
      call da_interp_lin_3d_adj (jo_grad_x % qrn, iv%info(radar), model_qrn)
      if ( cloud_cv_options >= 2 ) then
         call da_interp_lin_3d_adj (jo_grad_x % qsn, iv%info(radar), model_qsn)
         call da_interp_lin_3d_adj (jo_grad_x % qgr, iv%info(radar), model_qgr)
      end if
   end if
   call da_interp_lin_3d_adj (jo_grad_x % q,   iv%info(radar), model_qv)
   call da_interp_lin_3d_adj (jo_grad_x % t,   iv%info(radar), model_t)
#ifdef A2C
   call da_interp_lin_3d_adj (jo_grad_x % v,   iv%info(radar), model_v,'u')
   call da_interp_lin_3d_adj (jo_grad_x % u,   iv%info(radar), model_u,'v')
#else
   call da_interp_lin_3d_adj (jo_grad_x % v,   iv%info(radar), model_v)
   call da_interp_lin_3d_adj (jo_grad_x % u,   iv%info(radar), model_u)
#endif

   deallocate (model_p)
   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_w)
   deallocate (model_qrn)
   deallocate (model_qrnb)
   deallocate (model_ps)
   deallocate (model_qv)
   deallocate (model_t)
   deallocate (model_qsn)
   deallocate (model_qgr)
   deallocate (model_rho)
   if ( use_radar_rqv ) then
      deallocate (model_qvb)
      deallocate (model_tb)
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoy_radar_adj")

end subroutine da_transform_xtoy_radar_adj


