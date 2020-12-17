subroutine da_transform_xtoy_gpsztd_adj( grid, iv, jo_grad_y, jo_grad_x )
!----------------------------------------------------------------
!  HISTORY
!
!    Purpose:  Considering the difference in the elevation 
!              between model surface and GPS ZTD site
!
!                                   Y.-R. Guo 05/21/2008
!----------------------------------------------------------------

   IMPLICIT NONE

   type (domain),  intent(in)    :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   INTEGER                       :: n        ! Loop counter.
   INTEGER                       :: i, j     ! Index dimension.
   REAL                          :: dx, dxm  !
   REAL                          :: dy, dym  !

!-- 2004-04-08
   INTEGER                :: k        ! Index dimension.
   REAL                   :: dzd,ddzd     ! adjustment pw [mm]
   REAL                   :: obs_terr     ! real terrain height at GPS site [m]
   REAL                   :: model_terr   ! model terrain height at GPS site[m]
   REAL,DIMENSION(kts:kte):: model_z     ! model z at GPS site [m]
   REAL,DIMENSION(kts:kte):: model_dztd  ! model dq at GPS site [kg/kg]
!--
   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_gpspw_adj")

   do n=iv%info(gpspw)%n1,iv%info(gpspw)%n2

         i   = iv%info(gpspw)%i(1,n)
         dy  = iv%info(gpspw)%dy(1,n)
         dym = iv%info(gpspw)%dym(1,n)
         j   = iv%info(gpspw)%j(1,n)
         dx  = iv%info(gpspw)%dx(1,n)
         dxm = iv%info(gpspw)%dxm(1,n)

!  Initialise the varibles
         dzd             = 0.
         model_dztd(:)   = 0.

         obs_terr   = iv%info(gpspw)%elv(n)
         model_terr = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) + &
                      dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))
         
         dzd =       1.e-4 * jo_grad_y%gpspw(n)%tpw

         jo_grad_x%ztd(i  ,j  ) = jo_grad_x%ztd(i  ,j  ) &
                                + dxm*dym*jo_grad_y%gpspw(n)%tpw
         jo_grad_x%ztd(i+1,j  ) = jo_grad_x%ztd(i+1,j  ) &
                                + dym*dx *jo_grad_y%gpspw(n)%tpw
         jo_grad_x%ztd(i  ,j+1) = jo_grad_x%ztd(i  ,j+1) &
                                + dxm *dy*jo_grad_y%gpspw(n)%tpw
         jo_grad_x%ztd(i+1,j+1) = jo_grad_x%ztd(i+1,j+1) &
                                + dx *dy *jo_grad_y%gpspw(n)%tpw

         IF ( obs_terr <= model_terr ) THEN 

            model_dztd(1)  =  (obs_terr - model_terr) * dzd

            jo_grad_x%ref(i,j,1)     = jo_grad_x%ref(i,j,1)     + dym*dxm*model_dztd(1)
            jo_grad_x%ref(i+1,j,1)   = jo_grad_x%ref(i+1,j,1)   + dym*dx *model_dztd(1)
            jo_grad_x%ref(i,j+1,1)   = jo_grad_x%ref(i,j+1,1)   + dy *dxm*model_dztd(1)
            jo_grad_x%ref(i+1,j+1,1) = jo_grad_x%ref(i+1,j+1,1) + dy *dx *model_dztd(1)

         ELSE 

            model_z(1) = dym*(dxm*grid%xb%hf(i,j,1)   + dx*grid%xb%hf(i+1,j,1)) + &
                         dy *(dxm*grid%xb%hf(i,j+1,1) + dx*grid%xb%hf(i+1,j+1,1))

! ..............................................................................
! The following part of code is written by Y.-R. Guo             07/16/2004

            do k = kts, kte

               if ( model_z(k) >= obs_terr ) exit

               model_z(k+1) = dym*(dxm*grid%xb%hf(i,j,k+1)   + dx*grid%xb%hf(i+1,j,k+1)) + &
                              dy *(dxm*grid%xb%hf(i,j+1,k+1) + dx*grid%xb%hf(i+1,j+1,k+1))
               
               ddzd = dzd

               if ( model_z(k+1) <= obs_terr ) then
                 model_dztd(k) = (model_z(k+1) - model_z(k))*ddzd 
               else
                 model_dztd(k) = (obs_terr - model_z(k))*ddzd 
               end if 
!
! No feedback to x%hf was considered here (Refer to comments in
! DA_Transform_XToY_Gpspw.inc).       Y.-R. Guo  04/06/2005
! ..................................................................................... 
               jo_grad_x%ref(i,j,k)     = jo_grad_x%ref(i,j,k)     + dym*dxm*model_dztd(k)
               jo_grad_x%ref(i+1,j,k)   = jo_grad_x%ref(i+1,j,k)   + dym*dx *model_dztd(k)
               jo_grad_x%ref(i,j+1,k)   = jo_grad_x%ref(i,j+1,k)   + dy *dxm*model_dztd(k)
               jo_grad_x%ref(i+1,j+1,k) = jo_grad_x%ref(i+1,j+1,k) + dy *dx *model_dztd(k)

             end do
         end if
      end do

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_gpsztd_adj")

end subroutine da_transform_xtoy_gpsztd_adj
