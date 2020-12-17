subroutine da_transform_xtoy_gpspw (grid, iv, y)

   !----------------------------------------------------------------
   ! Purpose: Calculate the difference in the elevation between model surface and GPS site
   !----------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid 
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n        ! Loop counter.
   integer :: i, j     ! Index dimension.
   real    :: dx, dxm  ! 
   real    :: dy, dym  !

   integer :: k          ! Index dimension.
   real    :: dpw, ddpw  ! adjustment pw [mm]
   real    :: obs_terr   ! real terrain height at GPS site [m]
   real    :: model_terr ! model terrain height at GPS site[m]
   real    :: model_q(kts:kte)    ! model q at GPS site [kg/kg]
   real    :: model_z(kts:kte)    ! model z at GPS site [m]
   real    :: model_rho(kts:kte)  ! model rho at GPS site [kg/m^3]
   real    :: model_dq(kts:kte)   ! model dq at GPS site [kg/kg]
   real    :: model_drho(kts:kte) ! model drho at GPS site [kg/m^3]

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_gpspw")

   do n=iv%info(gpspw)%n1,iv%info(gpspw)%n2
      i   = iv%info(gpspw)%i(1,n)
      dy  = iv%info(gpspw)%dy(1,n)
      dym = iv%info(gpspw)%dym(1,n)
      j   = iv%info(gpspw)%j(1,n)
      dx  = iv%info(gpspw)%dx(1,n)
      dxm = iv%info(gpspw)%dxm(1,n)

      ! Mathematical transformation:

      ! kusaka 2004-04-08
      ! y % gpspw(n)% tpw = dym* (dxm * grid%xa%tpw(i,j) + &
      !    dx  * grid%xa%tpw(i+1,j)) + &
      !    dy * (dxm * grid%xa%tpw(i,j+1) + &
      !    dx  * grid%xa%tpw(i+1,j+1))
      dpw = 0.0
      obs_terr   = iv%info(gpspw)%elv(n)
      model_terr = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) + &
                   dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))

      if (obs_terr <= model_terr) then 
         model_q(1) = dym*(dxm*grid%xb%q(i,j,1)   + dx*grid%xb%q(i+1,j,1)) + &
             dy *(dxm*grid%xb%q(i,j+1,1) + dx*grid%xb%q(i+1,j+1,1))
         model_rho(1) = dym*(dxm*grid%xb%rho(i,j,1)   + dx*grid%xb%rho(i+1,j,1)) + &
            dy *(dxm*grid%xb%rho(i,j+1,1) + dx*grid%xb%rho(i+1,j+1,1))

         model_dq(1) = dym*(dxm*grid%xa%q(i,j,1)   + dx*grid%xa%q(i+1,j,1)) + &
            dy *(dxm*grid%xa%q(i,j+1,1) + dx*grid%xa%q(i+1,j+1,1))
         model_drho(1) = dym*(dxm*grid%xa%rho(i,j,1)   + dx*grid%xa%rho(i+1,j,1)) + &
            dy *(dxm*grid%xa%rho(i,j+1,1) + dx*grid%xa%rho(i+1,j+1,1))

         dpw = (model_rho(1)*model_dq(1) + model_drho(1)*model_q(1)) &
            * (obs_terr - model_terr)
      else 
         model_z(1) = dym*(dxm*grid%xb%hf(i,j,1)   + dx*grid%xb%hf(i+1,j,1)) + &
            dy *(dxm*grid%xb%hf(i,j+1,1) + dx*grid%xb%hf(i+1,j+1,1))
         do k = kts, kte
            if (model_z(k) >= obs_terr) exit

            model_z(k+1) = dym*(dxm*grid%xb%hf(i,j,k+1)   + dx*grid%xb%hf(i+1,j,k+1)) + &
               dy *(dxm*grid%xb%hf(i,j+1,k+1) + dx*grid%xb%hf(i+1,j+1,k+1))

            model_q(k) = dym*(dxm*grid%xb%q(i,j,k)   + dx*grid%xb%q(i+1,j,k)) + &
               dy *(dxm*grid%xb%q(i,j+1,k) + dx*grid%xb%q(i+1,j+1,k))
            model_rho(k) = dym*(dxm*grid%xb%rho(i,j,k)   + dx*grid%xb%rho(i+1,j,k)) + &
               dy *(dxm*grid%xb%rho(i,j+1,k) + dx*grid%xb%rho(i+1,j+1,k))

            model_dq(k) = dym*(dxm*grid%xa%q(i,j,k)   + dx*grid%xa%q(i+1,j,k)) + &
               dy *(dxm*grid%xa%q(i,j+1,k) + dx*grid%xa%q(i+1,j+1,k))
            model_drho(k) = dym*(dxm*grid%xa%rho(i,j,k)   + dx*grid%xa%rho(i+1,j,k)) + &
               dy *(dxm*grid%xa%rho(i,j+1,k) + dx*grid%xa%rho(i+1,j+1,k))

            ! Here assumed that "model_z" is constant, i.e. grid%xa%hf=0.0. With MM5, 
            ! this is true, but with WRF, grid%xa%hf may not be zero (?). In the WRF 
            ! model space (x,y,znu), only the "znu" is constant, but all variables 
            ! including height could be changed at the "znu" levels. So here is only 
            ! an approximation for WRF. The following few lines of code is written
            ! by Y.-R. Guo 07/16/2004.

            if (model_z(k+1) <= obs_terr) then
               ddpw = (model_rho(k)*model_dq(k) + model_drho(k)*model_q(k)) * (model_z(k+1)-model_z(k))
            else 
               ddpw = (model_rho(k)*model_dq(k) + model_drho(k)*model_q(k)) * (obs_terr-model_z(k))
            end if

            dpw = dpw + ddpw
         end do 
      end if 
      y % gpspw(n)% tpw = dym* (dxm * grid%xa%tpw(i,j) + &
                                dx  * grid%xa%tpw(i+1,j)) + &
                         dy * (dxm * grid%xa%tpw(i,j+1) + &
                                dx  * grid%xa%tpw(i+1,j+1)) &
                         + 0.1*dpw
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_gpspw")

end subroutine da_transform_xtoy_gpspw


