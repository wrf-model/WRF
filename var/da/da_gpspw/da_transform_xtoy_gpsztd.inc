subroutine da_transform_xtoy_gpsztd ( grid, iv, y )
!----------------------------------------------------------------
! Purpose: To calculate the observed ZTD with the height
!          correction in considering the differexbnce between
!          the model terrain height and receiver height.
!
!    The logic is similar to the Gpspw: 
!
!      when the receiver below the model surface, a correction,
!      "dzd" should be subtructed from the observed ZTD;
!      when the receiver higher than the model suface, a
!      correction "ztd" should be added to the observed ZTD.
!          
!----------------------------------------------------------------

   IMPLICIT NONE

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(INOUT) :: y        ! y = h (xa)

   INTEGER                      :: n        ! Loop counter.
   INTEGER                      :: i, j     ! Index dimension.
   REAL                         :: dx, dxm  ! 
   REAL                         :: dy, dym  !
!--   
   INTEGER                :: k          ! Index dimension.
   REAL                   :: dzd, ddzd  ! adjustment pw [mm]
   REAL                   :: obs_terr   ! real terrain height at GPS site [m]
   REAL                   :: model_terr ! model terrain height at GPS site[m]
   REAL,DIMENSION(kts:kte):: model_ztd   ! model ztd at GPS site [m]
   REAL,DIMENSION(kts:kte):: model_z     ! model height at GPS site [m]
!--
   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_gpsztd")

      !bugfix-for-4dvar y % gpspw(:)% tpw = 0.0

      do n=iv%info(gpspw)%n1,iv%info(gpspw)%n2

         i   = iv%info(gpspw)%i(1,n)
         dy  = iv%info(gpspw)%dy(1,n)
         dym = iv%info(gpspw)%dym(1,n)
         j   = iv%info(gpspw)%j(1,n)
         dx  = iv%info(gpspw)%dx(1,n)
         dxm = iv%info(gpspw)%dxm(1,n)

! Mathematical transformation:

         dzd = 0.0
         obs_terr   = iv%info(gpspw)%elv(n)
         model_terr = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) + &
                      dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))

         if ( obs_terr <= model_terr ) then 

            model_ztd(1) = dym*(dxm*grid%xa%ref(i,j,1)   + dx*grid%xa%ref(i+1,j,1)) + &
                           dy *(dxm*grid%xa%ref(i,j+1,1) + dx*grid%xa%ref(i+1,j+1,1))

            dzd =  model_ztd(1) * ( obs_terr - model_terr )

         else 

            model_z(1) = dym*(dxm*grid%xb%hf(i,j,1)   + dx*grid%xb%hf(i+1,j,1)) + &
                         dy *(dxm*grid%xb%hf(i,j+1,1) + dx*grid%xb%hf(i+1,j+1,1))

            do k = kts, kte

              if (model_z(k) >= obs_terr ) exit

              model_z(k+1) = dym*(dxm*grid%xb%hf(i,j,k+1)   + dx*grid%xb%hf(i+1,j,k+1)) + &
                             dy *(dxm*grid%xb%hf(i,j+1,k+1) + dx*grid%xb%hf(i+1,j+1,k+1))

              model_ztd(k) = dym*(dxm*grid%xa%ref(i,j,k)   + dx*grid%xa%ref(i+1,j,k)) + &
                             dy *(dxm*grid%xa%ref(i,j+1,k) + dx*grid%xa%ref(i+1,j+1,k))
!
! Here assumed that "model_z" is constant, i.e. grid%xa%hf=0.0. With MM5, 
! this is true, but with WRF, grid%xa%hf may not be zero (?). In the WRF 
! model space (x,y,znu), only the "znu" is constant, but all variables 
! including height could be changed at the "znu" levels. So here is only 
! an approximation for WRF. The following few lines of code is written
! by Y.-R. Guo 07/16/2004.
!
              if ( model_z(k+1) <= obs_terr ) then
                 ddzd = model_ztd(k) * ( model_z(k+1) - model_z(k) )
              else 
                 ddzd = model_ztd(k) * ( obs_terr -  model_z(k) )
              endif

              dzd = dzd + ddzd

            end do 
         end if 

         y % gpspw(n)% tpw = dym* ( dxm * grid%xa%ztd(i,j) + &
                                    dx  * grid%xa%ztd(i+1,j) ) + &
                             dy * ( dxm * grid%xa%ztd(i,j+1) + &
                                    dx  * grid%xa%ztd(i+1,j+1) ) &
                             + 1.e-4 * dzd

      end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_gpsztd")

end subroutine da_transform_xtoy_gpsztd

