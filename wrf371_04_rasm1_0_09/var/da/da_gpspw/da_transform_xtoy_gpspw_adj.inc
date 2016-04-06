subroutine da_transform_xtoy_gpspw_adj(grid, iv, jo_grad_y, jo_grad_x)

   !----------------------------------------------------------------
   ! Purpose: Calculate the difference in the elevation between model surface and GPS site
   !----------------------------------------------------------------

   implicit none

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   integer :: n        ! Loop counter.
   integer :: i, j     ! Index dimension.
   real    :: dx, dxm  !
   real    :: dy, dym  !

   integer :: k        ! Index dimension.
   real    :: dpw,ddpw     ! adjustment pw [mm]
   real    :: obs_terr     ! real terrain height at GPS site [m]
   real    :: model_terr   ! model terrain height at GPS site[m]
   real    :: model_q(kts:kte)   ! model q at GPS site [kg/kg]
   real    :: model_z(kts:kte)   ! model z at GPS site [m]
   real    :: model_rho(kts:kte) ! model rho at GPS site [kg/m^3]
   real    :: model_dq(kts:kte)   ! model dq at GPS site [kg/kg]
   real    :: model_drho(kts:kte) ! model drho at GPS site [kg/m^3]

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_gpspw_adj")

   do n=iv%info(gpspw)%n1,iv%info(gpspw)%n2
      i   = iv%info(gpspw)%i(1,n)
      dy  = iv%info(gpspw)%dy(1,n)
      dym = iv%info(gpspw)%dym(1,n)
      j   = iv%info(gpspw)%j(1,n)
      dx  = iv%info(gpspw)%dx(1,n)
      dxm = iv%info(gpspw)%dxm(1,n)

      !  Initialise the varibles
      dpw           = 0.0
      model_q(:)    = 0.0
      model_dq(:)   = 0.0
      model_rho(:)  = 0.0
      model_drho(:) = 0.0

      obs_terr   = iv%info(gpspw)%elv(n)
      model_terr = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) + &
         dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))

      dpw = 0.1 *jo_grad_y%gpspw(n)%tpw

      jo_grad_x%tpw(i  ,j )  = jo_grad_x%tpw(i  ,j )  + dxm*dym*jo_grad_y%gpspw(n)%tpw
      jo_grad_x%tpw(i+1,j )  = jo_grad_x%tpw(i+1,j )  + dym*dx *jo_grad_y%gpspw(n)%tpw
      jo_grad_x%tpw(i  ,j+1) = jo_grad_x%tpw(i,j+1)   + dxm *dy*jo_grad_y%gpspw(n)%tpw
      jo_grad_x%tpw(i+1,j+1) = jo_grad_x%tpw(i+1,j+1) + dx *dy *jo_grad_y%gpspw(n)%tpw

      if (obs_terr <= model_terr) then 
         model_q(1)   = dym*(dxm*grid%xb%q(i,j,1)   + dx*grid%xb%q(i+1,j,1)) + &
            dy *(dxm*grid%xb%q(i,j+1,1) + dx*grid%xb%q(i+1,j+1,1))
         model_rho(1) = dym*(dxm*grid%xb%rho(i,j,1)   + dx*grid%xb%rho(i+1,j,1)) + &
            dy *(dxm*grid%xb%rho(i,j+1,1) + dx*grid%xb%rho(i+1,j+1,1))

         model_dq(1)   =  model_rho(1)*(obs_terr - model_terr)*dpw
         model_drho(1) =  model_q(1)  *(obs_terr - model_terr)*dpw

         jo_grad_x%q(i,j,1)       = jo_grad_x%q(i,j,1)       + dym*dxm*model_dq(1)
         jo_grad_x%q(i+1,j,1)     = jo_grad_x%q(i+1,j,1)     + dym*dx*model_dq(1)
         jo_grad_x%q(i,j+1,1)     = jo_grad_x%q(i,j+1,1)     + dy*dxm*model_dq(1)
         jo_grad_x%q(i+1,j+1,1)   = jo_grad_x%q(i+1,j+1,1)   + dy*dx*model_dq(1)

         jo_grad_x%rho(i,j,1)     = jo_grad_x%rho(i,j,1)     + dym*dxm*model_drho(1)
         jo_grad_x%rho(i+1,j,1)   = jo_grad_x%rho(i+1,j,1)   + dym*dx*model_drho(1)
         jo_grad_x%rho(i,j+1,1)   = jo_grad_x%rho(i,j+1,1)   + dy*dxm*model_drho(1)
         jo_grad_x%rho(i+1,j+1,1) = jo_grad_x%rho(i+1,j+1,1) + dy*dx*model_drho(1)
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

           ddpw = dpw

           if (model_z(k+1) <= obs_terr) then
             model_dq  (k) = model_rho(k) *(model_z(k+1) - model_z(k))*ddpw 
             model_drho(k) = model_q(k)   *(model_z(k+1) - model_z(k))*ddpw 
           else
             model_dq  (k) = model_rho(k) *(obs_terr-model_z(k))*ddpw 
             model_drho(k) = model_q(k)   *(obs_terr-model_z(k))*ddpw 
           end if 

           ! No feedback to x%hf was considered here (Refer to comments in
           ! da_transform_xtoy_gpspw.inc).       Y.-R. Guo  07/15/2002
           !......................................................................... 

           jo_grad_x%q(i,j,k)       = jo_grad_x%q(i,j,k)       + dym*dxm*model_dq(k)
           jo_grad_x%q(i+1,j,k)     = jo_grad_x%q(i+1,j,k)     + dym*dx*model_dq(k)
           jo_grad_x%q(i,j+1,k)     = jo_grad_x%q(i,j+1,k)     + dy*dxm*model_dq(k)
           jo_grad_x%q(i+1,j+1,k)   = jo_grad_x%q(i+1,j+1,k)   + dy*dx*model_dq(k)

           jo_grad_x%rho(i,j,k)     = jo_grad_x%rho(i,j,k)     + dym*dxm*model_drho(k)
           jo_grad_x%rho(i+1,j,k)   = jo_grad_x%rho(i+1,j,k)   + dym*dx*model_drho(k)
           jo_grad_x%rho(i,j+1,k)   = jo_grad_x%rho(i,j+1,k)   + dy*dxm*model_drho(k)
           jo_grad_x%rho(i+1,j+1,k) = jo_grad_x%rho(i+1,j+1,k) + dy*dx*model_drho(k)
        end do
      end if 
   end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_gpspw_adj")

end subroutine da_transform_xtoy_gpspw_adj


