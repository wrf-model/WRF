subroutine da_transform_xtoy_radar (grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: calculate the Doppler radial velocity and 
   ! reflectivity at the observation location from the first guess.
   ! It is linearized. 
   ! History:
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !    08/2017 - bug fix for Vr operator (Siou-Ying Jiang, CWB, Taiwan)
   !---------------------------------------------------------------------
 
   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa) (linear)

   integer :: n, k

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
   !------------------------
   !  for jung et al 2008
   !------------------------
   real, allocatable :: model_qsnb(:,:)
   real, allocatable :: model_qgrb(:,:)
   real, allocatable :: model_qnrb(:,:)
   real, allocatable :: model_qnsb(:,:)
   real, allocatable :: model_qngb(:,:)
   real, allocatable :: model_qnr(:,:)
   real, allocatable :: model_qns(:,:)
   real, allocatable :: model_qng(:,:)
   !------------------------ 

   real    :: xr,yr,zr

   real    :: alog_10

   !------------------------
   !  for jung et al 2008
   !------------------------
   real    :: qvp,qra,qsn,qgr ! mixing ratio
                              ! to match the type of argument in subroutine
   real    :: dqra,dqsn,dqgr,dtmk,dqvp
   real    :: dqnr,dqns,dqng
   real    :: zmm,zmm_ref
   real    :: qnr,qns,qng            ! number concentration of rain snow and graupel
   real    :: tmk,prs                ! temperature and pressure
   real    :: dbz                    ! reflectivity in dBZ
   real    :: rn0_r,rn0_s,rn0_g      ! intercept parameter of rain snow and graupel
   real    :: rhos,rhog              ! density of snow and graupel
   !------------------------

   if (trace_use) call da_trace_entry("da_transform_xtoy_radar")

   alog_10 = alog(10.0)

   !------------------------
   !  for jung et al 2008
   !------------------------
   qnr=0
   qns=0
   qng=0
   rn0_r=8e6
   rn0_s=3e6
   rn0_g=4e5
   rhos=100.0
   rhog=400.0
   !------------------------

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

   !------------------------
   !  for jung et al 2008
   !------------------------
   if (use_radar_rf .and. radar_rf_opt==2) then
     allocate (model_qsnb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
     allocate (model_qgrb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
     allocate (model_qnrb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
     allocate (model_qnsb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
     allocate (model_qngb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
     allocate (model_qnr(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
     allocate (model_qns(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
     allocate (model_qng(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   end if
   !------------------------

   do n=iv%info(radar)%n1,iv%info(radar)%n2
      do k = 1, iv%info(radar)%levels(n)
         model_qrnb(k,n) = iv%radar(n)%model_qrn(k)
         model_p(k,n)    = iv%radar(n)%model_p(k)
         model_rho(k,n)  = iv%radar(n)%model_rho(k)
         !------------------------
         !  for jung et al 2008
         !------------------------
        if (use_radar_rf .and. radar_rf_opt==2) then
         model_qsnb(k,n) = iv%radar(n)%model_qsn(k)
         model_qgrb(k,n) = iv%radar(n)%model_qgr(k)
         model_qnrb(k,n) = 0
         model_qnsb(k,n) = 0
         model_qngb(k,n) = 0
        end if
         !------------------------
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
   call da_interp_lin_3d (grid%xa%wh,  iv%info(radar), model_w)
   model_qsn = 0.0
   model_qgr = 0.0
   model_qrn = 0.0
   if ( cloud_cv_options >= 1 ) then
      call da_interp_lin_3d (grid%xa%qrn, iv%info(radar), model_qrn)
      if ( cloud_cv_options >= 2 ) then
         call da_interp_lin_3d (grid%xa%qsn, iv%info(radar), model_qsn)
         call da_interp_lin_3d (grid%xa%qgr, iv%info(radar), model_qgr)
      end if
   end if
   call da_interp_lin_3d (grid%xa%q,   iv%info(radar), model_qv)
   call da_interp_lin_3d (grid%xa%t,   iv%info(radar), model_t)

   !if ( use_radar_rqv ) then
      !basic states
      allocate (model_tb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
      allocate (model_qvb(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
      call da_interp_lin_3d (grid%xb%t,   iv%info(radar), model_tb)
      call da_interp_lin_3d (grid%xb%q,   iv%info(radar), model_qvb)
   !end if

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
                     model_ps(n), xr, yr, zr, model_qrnb(k,n), model_rho(k,n))
                  y%radar(n)%rv(k)=y%radar(n)%rv(k)*radar_rv_rscl
               end if
            end if

            if (use_radar_rf .and. radar_rf_opt==2) then
              if (iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then
                   tmk=model_tb(k,n)
                   prs=model_p(k,n)
                   zmm=iv % radar(n) % zmm(k) % inv
                   zmm_ref=0
                   call da_radzicevar_tl(model_qvb(k,n),model_qrnb(k,n),model_qsnb(k,n),model_qgrb(k,n),qnr,qns,qng,tmk,prs,dbz,                   &
                                      0,0,0,rn0_r,rn0_s,rn0_g,                                                                                  &
                                      rhos,rhog,model_t(k,n),model_qv(k,n),model_qrn(k,n),model_qsn(k,n),model_qgr(k,n),dqnr,dqns,dqng,zmm,2,   &
                                      0,zmm_ref)
                   y%radar(n)%rf(k) =radar_rf_rscl*dbz
              end if
            end if

            if (use_radar_rf .and. radar_rf_opt==1) then
               if (iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then
                  y%radar(n)%rf(k) = leh2 * model_qrn(k,n) /(model_qrnb(k,n)*alog_10) 
               end if
            end if

            if (.not.use_radar_rf .and. use_radar_rhv) then
               if (iv % radar(n) % rrn(k) % qc >= obs_qc_pointer) then
                  y%radar(n)%rrn(k) = model_qrn(k,n) 
               end if
               if (iv % radar(n) % rsn(k) % qc >= obs_qc_pointer) then
                  y%radar(n)%rsn(k) = model_qsn(k,n) 
               end if
               if (iv % radar(n) % rgr(k) % qc >= obs_qc_pointer) then
                  y%radar(n)%rgr(k) = model_qgr(k,n)
               end if
            end if

            if (use_radar_rqv) then
               !dqv=qs*drh+(c2*c3*/(T+c3)**2.0-c4/T)*qv**dT
               !c2=17.67 is es_beta
               !c3=243.5 is es_gamma
               !c4=0.622 is a_ew
               ! use qc from get_inv.
               if (iv % radar(n) % rqv(k) % qc >= obs_qc_pointer) then
                  y%radar(n)%rqv(k) = model_qv(k,n)
                  ! Wang JAMC
                  y%radar(n)%rqv(k) = y%radar(n)%rqv(k) + ( es_beta*es_gamma/(model_tb(k,n)+es_gamma)**2.0 )*model_qvb(k,n)*model_t(k,n)
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
   deallocate (model_qsn)
   deallocate (model_qgr)
   deallocate (model_qv)
   deallocate (model_t)
   deallocate (model_rho)
   !if ( use_radar_rqv ) then
      deallocate (model_tb)
      deallocate (model_qvb)
   !end if

   if (use_radar_rf .and. radar_rf_opt==2) then
     deallocate (model_qsnb)
     deallocate (model_qgrb)
     deallocate (model_qnr)
     deallocate (model_qns)
     deallocate (model_qng)
     deallocate (model_qnrb)
     deallocate (model_qnsb)
     deallocate (model_qngb)
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoy_radar")

end subroutine da_transform_xtoy_radar
