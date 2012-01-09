subroutine da_transfer_xatowrftl_lbc(grid, config_flags, filnam)

   !---------------------------------------------------------------------------
   !  Purpose: Convert analysis increments into WRFTL increments 
   !           (following xatowrf, but only keep the increments)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   character*4, intent(in) :: filnam

#ifdef VAR4D

   integer :: i, j, k, ii, jj
   integer :: is, ie, js, je, ks, ke, spec_bdy_width
   integer :: ids0, ide0, jds0, jde0, kds0, kde0
   integer :: ims0, ime0, jms0, jme0, kms0, kme0
   integer :: ips0, ipe0, jps0, jpe0, kps0, kpe0
   real    :: sdmd, s1md
   real    :: rho_cgrid

#ifdef A2C
   real, allocatable, dimension(:,:,:) :: g_press
#else
   real, allocatable, dimension(:,:,:) :: utmp, vtmp, g_press
   real, allocatable, dimension(:,:,:) :: g_u, g_v, g_ph, g_w
   real, allocatable, dimension(:,:) ::   g_mu
   real, allocatable, dimension(:,:,:) :: g_moist, g_t
#endif

   integer ndynopt

   if (trace_use) call da_trace_entry("da_transfer_xatowrftl_lbc")

   IF ( ALLOCATED ( ubdy3dtemp2 ) ) DEALLOCATE ( ubdy3dtemp2 )
   IF ( ALLOCATED ( vbdy3dtemp2 ) ) DEALLOCATE ( vbdy3dtemp2 )
   IF ( ALLOCATED ( tbdy3dtemp2 ) ) DEALLOCATE ( tbdy3dtemp2 )
   IF ( ALLOCATED ( pbdy3dtemp2 ) ) DEALLOCATE ( pbdy3dtemp2 )
   IF ( ALLOCATED ( qbdy3dtemp2 ) ) DEALLOCATE ( qbdy3dtemp2 )
   IF ( ALLOCATED ( mbdy2dtemp2 ) ) DEALLOCATE ( mbdy2dtemp2 )
!  IF ( ALLOCATED ( wbdy3dtemp2 ) ) DEALLOCATE ( wbdy3dtemp2 )

   ALLOCATE ( ubdy3dtemp2(ims:ime,jms:jme,kms:kme) )
   ALLOCATE ( vbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
   ALLOCATE ( tbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
   ALLOCATE ( pbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
   ALLOCATE ( qbdy3dtemp2(ims:ime,jms:jme,kms:kme) )
   ALLOCATE ( mbdy2dtemp2(ims:ime,1:1,    jms:jme) )
!  ALLOCATE ( wbdy3dtemp2(ims:ime,jms:jme,kms:kme) )

   ubdy3dtemp2 = 0.0
   vbdy3dtemp2 = 0.0
   tbdy3dtemp2 = 0.0
   pbdy3dtemp2 = 0.0
   qbdy3dtemp2 = 0.0
   mbdy2dtemp2 = 0.0
!  wbdy3dtemp2 = 0.0

   spec_bdy_width =  grid%spec_bdy_width

   IF ( var4d_lbc ) THEN 

   is=grid%xp%its
   ie=grid%xp%ite
   js=grid%xp%jts
   je=grid%xp%jte
   ks=grid%xp%kts
   ke=grid%xp%kte

   allocate (g_press(ims:ime,jms:jme,kms:kme))
   allocate (g_moist(ims:ime,jms:jme,kms:kme))
   allocate (g_t(ims:ime,jms:jme,kms:kme))
   allocate (g_mu(ims:ime,jms:jme))

   !---------------------------------------------------------------------------
   ! [1.0] Get the mixing ratio of moisture first, as it is easy.
   !---------------------------------------------------------------------------

   do k = ks , ke
      do j = js, je
         do i = is, ie
            g_moist(i,j,k) = grid%x6a%q(i,j,k)/(1.0-grid%xb%q(i,j,k))**2 
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] COMPUTE increments of dry-column air mass per unit area
   !---------------------------------------------------------------------------

   do j = js, je
      do i = is, ie
         sdmd=0.0
         s1md=0.0
         do k = ks , ke
            sdmd=sdmd + g_moist(i,j,k) * grid%dnw(k)
            s1md=s1md+(1.0+grid%moist(i,j,k,P_QV))*grid%dnw(k)
         end do
         g_mu(i,j) = -(grid%x6a%psfc(i,j)+grid%xb%psac(i,j)*sdmd)/s1md
      end do
   end do

#ifdef DM_PARALLEL
   grid%g_mu_2 = g_mu
#include "HALO_EM_E_TL.inc"
   g_mu = grid%g_mu_2
#endif

   !---------------------------------------------------------------------------
   ! [3.0] compute moisture tendency perturbation
   !---------------------------------------------------------------------------

   CALL g_couple ( model_config_flags, grid%mu_2 , g_mu, grid%mub , qbdy3dtemp2 , grid%moist(:,:,:,P_G_QV), g_moist,  't' , grid%msfty , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   ENDIF

   CALL g_stuff_bdytend ( qbdy3dtemp2 , qbdy3dtemp1 , REAL(interval_seconds) ,                 &
                                        model_grid%g_moist_btxs(:,:,:,P_G_QV), model_grid%g_moist_btxe(:,:,:,P_G_QV), &
                                        model_grid%g_moist_btys(:,:,:,P_G_QV), model_grid%g_moist_btye(:,:,:,P_G_QV), &
                                        'T' , &
                                        spec_bdy_width      , &
                                        grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                        grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                        grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   !---------------------------------------------------------------------------
   ! [2.0] COMPUTE tendency of increments of dry-column air mass
   !---------------------------------------------------------------------------

   IF ( var4d_lbc ) THEN

   DO j = grid%sp32 , MIN(grid%ed32-1,grid%ep32)
      DO i = grid%sp31 , MIN(grid%ed31-1,grid%ep31)
          mbdy2dtemp2(i,1,j) = g_mu(i,j)
      END DO
   END DO

   ENDIF

   CALL g_stuff_bdytend ( mbdy2dtemp2 , mbdy2dtemp1 , REAL(interval_seconds) ,                 &
                                   model_grid%g_mu_btxs, model_grid%g_mu_btxe,   &
                                   model_grid%g_mu_btys, model_grid%g_mu_btye,   &
                                   'M' , &
                                   spec_bdy_width      , &
                                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, 1, 1, &
                                   grid%sm31, grid%em31, grid%sm32, grid%em32, 1, 1, &
                                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, 1, 1 )

   !---------------------------------------------------------------------------
   ! [3.0] compute pressure increments (for computing theta increments)
   !---------------------------------------------------------------------------

   IF ( var4d_lbc ) THEN

   do j=js,je
      do i=is,ie
         g_press(i,j,ke+1)=0.0
         do k=ke,ks,-1
            g_press(i,j,k)=g_press(i,j,k+1) &
               -(g_mu(i,j)*(1.0+grid%moist(i,j,k,P_QV)) &
               +(grid%mu_2(i,j)+grid%mub(i,j))*g_moist(i,j,k))* &
               grid%dn(k)
            grid%x6a%p(i,j,k)=0.5*(g_press(i,j,k)+g_press(i,j,k+1))
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [4.0] convert temperature increments into theta increments
   !       evaluate also the increments of (1/rho) and geopotential
   !---------------------------------------------------------------------------

   do k = ks, ke
      do j = js, je
         do i = is, ie
            g_t(i,j,k) = (t0+grid%t_2(i,j,k)) * (grid%x6a%t(i,j,k)/grid%xb%t(i,j,k) &
                        -kappa*grid%x6a%p(i,j,k)/grid%xb%p(i,j,k))
         end do
      end do
   end do

   CALL g_couple ( model_config_flags, grid%mu_2 , g_mu, grid%mub , tbdy3dtemp2 , grid%t_2 , g_t , 't' , grid%msfty , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   ENDIF

   CALL g_stuff_bdytend ( tbdy3dtemp2 , tbdy3dtemp1 , REAL(interval_seconds) ,                 &
                                      model_grid%g_t_btxs, model_grid%g_t_btxe,     &
                                      model_grid%g_t_btys, model_grid%g_t_btye,     &
                                      'T' , &
                                      spec_bdy_width      , &
                                      grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                      grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                      grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   IF ( var4d_lbc ) THEN

   allocate ( g_ph(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, grid%xp%kms:grid%xp%kme) )

!  do j=js,je
!     do i=is,ie
!        g_ph(i,j,ks)=0.0
!        do k=ks,ke
!           rho_cgrid=grid%xb%rho(i,j,k) &
!                     *(grid%x6a%p(i,j,k)/grid%xb%p(i,j,k) &
!                     -grid%x6a%t(i,j,k)/grid%xb%t(i,j,k) &
!                     -0.61*grid%x6a%q(i,j,k)/(1.0+0.61*grid%xb%q(i,j,k)))
!           g_ph(i,j,k+1)=g_ph(i,j,k) &
!              -(g_press(i,j,k+1)-g_press(i,j,k) &
!              +(grid%ph_2(i,j,k+1)-grid%ph_2(i,j,k))*rho_cgrid) &
!              /grid%xb%rho(i,j,k)
!        end do
!     end do
!  end do

   g_ph = 0.0

   CALL g_couple ( model_config_flags, grid%mu_2 , g_mu, grid%mub , pbdy3dtemp2 , grid%ph_2, g_ph, 'h' , grid%msfty , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )
   ENDIF

   CALL g_stuff_bdytend ( pbdy3dtemp2 , pbdy3dtemp1 , REAL(interval_seconds) ,                 &
                                      model_grid%g_ph_btxs, model_grid%g_ph_btxe,   &
                                      model_grid%g_ph_btys, model_grid%g_ph_btye,   &
                                      'W' , &
                                      spec_bdy_width      , &
                                      grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                      grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                      grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   IF ( var4d_lbc ) THEN

   deallocate (g_press)
   deallocate (g_ph)

   !---------------------------------------------------------------------------
   ! [5.0] convert from a-grid to c-grid
   !---------------------------------------------------------------------------

   allocate ( g_u(ims:ime,jms:jme, kms:kme) )
   allocate ( g_v(ims:ime,jms:jme, kms:kme) )

#ifdef DM_PARALLEL
#include "HALO_X6A_A.inc"

   allocate ( utmp(ims:ime,jms:jme, kms:kme) )
   allocate ( vtmp(ims:ime,jms:jme, kms:kme) )

   utmp = grid%x6a%u
   vtmp = grid%x6a%v

   ! The southern boundary (fill A-GRID boundaries)
   ! To keep the gradient, A(0) = 2A(1)-A(2)
   if (js == grid%xp%jds) then
      vtmp(is:ie,js-1,ks:ke)=2.0*grid%x6a%v(is:ie,js  ,ks:ke) &
                            -    grid%x6a%v(is:ie,js+1,ks:ke)
   end if

   ! The northern boundary
   if (je == grid%xp%jde) then
      vtmp(is:ie,je+1,ks:ke)=2.0*grid%x6a%v(is:ie,je  ,ks:ke) &
                            -    grid%x6a%v(is:ie,je-1,ks:ke)
   end if

   ! The western boundary (fill A-GRID boundaries)
   ! To keep the gradient, A(0) = 2A(1)-A(2)
   if (is == grid%xp%ids) then
      utmp(is-1,js:je,ks:ke)=2.0*grid%x6a%u(is  ,js:je,ks:ke) &
                            -    grid%x6a%u(is+1,js:je,ks:ke)
   end if

   ! The eastern boundary
   if (ie == grid%xp%ide) then
      utmp(ie+1,js:je,ks:ke)=2.0*grid%x6a%u(ie  ,js:je,ks:ke) &
                            -    grid%x6a%u(ie-1,js:je,ks:ke)
   end if

   do k=ks,ke
      do j=js,je
         do i=is,ie+1
            g_u(i,j,k)=0.5*(utmp(i-1,j  ,k)+utmp(i,j,k))
         end do
      end do
      do j=js,je+1
         do i=is,ie
            g_v(i,j,k)=0.5*(vtmp(i  ,j-1,k)+vtmp(i,j,k))
         end do
      end do
   end do


   deallocate (utmp)
   deallocate (vtmp)
#else

   do k=ks,ke
      do j=js,je
         do i=is+1,ie
            g_u(i,j,k)=0.5*(grid%x6a%u(i-1,j,k)+grid%x6a%u(i,j,k))
         end do
      end do
      do j=js+1,je
         do i=is,ie
            g_v(i,j,k)=0.5*(grid%x6a%v(i,j-1,k)+grid%x6a%v(i,j,k))
         end do
      end do
   end do

   ! To keep the gradient, A(N+1) = 2A(N)-A(N-1)
   ! and on C-Grid, this will lead to C(N+1)=(A(N)+A(N+1))/2=(3A(N)-A(N-1))/2

   ! The eastern boundary
   g_u(ie+1,js:je,ks:ke)=(3.0*grid%x6a%u(ie,js:je,ks:ke)-grid%x6a%u(ie-1,js:je,ks:ke))/2.0

   ! The northern boundary
   g_v(is:ie,je+1,ks:ke)=(3.0*grid%x6a%v(is:ie,je,ks:ke)-grid%x6a%v(is:ie,je-1,ks:ke))/2.0

   ! To keep the gradient, A(0) = 2A(1)-A(2)
   ! and on C-Grid, this will lead to C(1)=(A(0)+A(1))/2=(3A(1)-A(2))/2

   ! The western boundary
   g_u(is,js:je,ks:ke)=(3.0*grid%x6a%u(is,js:je,ks:ke)-grid%x6a%u(is+1,js:je,ks:ke))/2.0

   ! The southern boundary
   g_v(is:ie,js,ks:ke)=(3.0*grid%x6a%v(is:ie,js,ks:ke)-grid%x6a%v(is:ie,js+1,ks:ke))/2.0

#endif

   CALL g_couple ( model_config_flags, grid%mu_2 , g_mu, grid%mub , ubdy3dtemp2 , grid%u_2 , g_u , 'u' , grid%msfuy , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL g_couple ( model_config_flags, grid%mu_2 , g_mu, grid%mub , vbdy3dtemp2 , grid%v_2 , g_v , 'v' , grid%msfvx , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   ENDIF

   CALL g_stuff_bdytend ( ubdy3dtemp2 , ubdy3dtemp1 , REAL(interval_seconds) ,                 &
                                      model_grid%g_u_btxs, model_grid%g_u_btxe,     &
                                      model_grid%g_u_btys, model_grid%g_u_btye,     &
                                      'U' , &
                                      spec_bdy_width      , &
                                      grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                      grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                      grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL g_stuff_bdytend ( vbdy3dtemp2 , vbdy3dtemp1 , REAL(interval_seconds) ,                 &
                                      model_grid%g_v_btxs, model_grid%g_v_btxe,     &
                                      model_grid%g_v_btys, model_grid%g_v_btye,     &
                                      'V' , &
                                      spec_bdy_width      , &
                                      grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                      grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                      grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

!  IF ( var4d_lbc ) THEN

!  allocate ( g_w(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, grid%xp%kms:grid%xp%kme) )

!  g_w = 0.0

!  CALL g_couple ( model_config_flags, grid%mu_2 , g_mu, grid%mub , wbdy3dtemp2 , grid%w_2, g_w, 'h' , grid%msfty , &
!                  grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
!                  grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
!                  grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )
!  ENDIF

!  CALL g_stuff_bdytend ( wbdy3dtemp2 , wbdy3dtemp1 , REAL(interval_seconds) ,                 &
!                                     model_grid%g_w_btxs, model_grid%g_w_btxe,   &
!                                     model_grid%g_w_btys, model_grid%g_w_btye,   &
!                                     'W' , &
!                                     spec_bdy_width      , &
!                                     grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
!                                     grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
!                                     grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   IF ( var4d_lbc ) THEN

   !---------------------------------------------------------------------------
   ! [7.0] tidy up
   !---------------------------------------------------------------------------
   deallocate (g_u)
   deallocate (g_v)
   deallocate (g_mu)
   deallocate (g_moist)
!  deallocate (g_w)
   deallocate (g_t)

   ENDIF

   DEALLOCATE ( ubdy3dtemp2 )
   DEALLOCATE ( vbdy3dtemp2 )
   DEALLOCATE ( tbdy3dtemp2 )
   DEALLOCATE ( pbdy3dtemp2 )
!  DEALLOCATE ( wbdy3dtemp2 )
   DEALLOCATE ( qbdy3dtemp2 )
   DEALLOCATE ( mbdy2dtemp2 )

   DEALLOCATE ( ubdy3dtemp1 )
   DEALLOCATE ( vbdy3dtemp1 )
   DEALLOCATE ( tbdy3dtemp1 )
   DEALLOCATE ( pbdy3dtemp1 )
!  DEALLOCATE ( wbdy3dtemp1 )
   DEALLOCATE ( qbdy3dtemp1 )
   DEALLOCATE ( mbdy2dtemp1 )


#ifdef DM_PARALLEL

   ids0=ids; ide0=ide; jds0=jds; jde0=jde; kds0=kds; kde0=kde
   ims0=ims; ime0=ime; jms0=jms; jme0=jme; kms0=kms; kme0=kme
   ips0=ips; ipe0=ipe; jps0=jps; jpe0=jpe; kps0=kps; kpe0=kpe

   ids=grid%sd31; ide=grid%ed31; jds=grid%sd32; jde=grid%ed32; kds=grid%sd33; kde=grid%ed33
   ims=grid%sm31; ime=grid%em31; jms=grid%sm32; jme=grid%em32; kms=grid%sm33; kme=grid%em33
   ips=grid%sp31; ipe=grid%ep31; jps=grid%sp32; jpe=grid%ep32; kps=grid%sp33; kpe=grid%ep33

!   Use a_u_1, a_v_1 etc as temporary buffers to do the halo exchange for bdy fields

!  X-direction pack

   CALL da_bdy_fields_halo ( grid%a_u_1 , grid%a_v_1, grid%a_t_1, grid%a_ph_1, grid%a_mu_1, &
                          grid%a_w_1, 0, 0, spec_bdy_width      , &
                          model_grid%g_u_bxs, model_grid%g_u_bxe, model_grid%g_u_bys, model_grid%g_u_bye, &
                          model_grid%g_v_bxs, model_grid%g_v_bxe, model_grid%g_v_bys, model_grid%g_v_bye, &
                          model_grid%g_t_bxs, model_grid%g_t_bxe, model_grid%g_t_bys, model_grid%g_t_bye, &
                          model_grid%g_ph_bxs, model_grid%g_ph_bxe, model_grid%g_ph_bys, model_grid%g_ph_bye, &
                          model_grid%g_mu_bxs, model_grid%g_mu_bxe, model_grid%g_mu_bys, model_grid%g_mu_bye, &
                          model_grid%g_moist_bxs(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_bxe(:,:,:,PARAM_FIRST_SCALAR), &
                          model_grid%g_moist_bys(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_bye(:,:,:,PARAM_FIRST_SCALAR), &
                          grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                          grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                          grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

#    include "HALO_EM_BDY.inc"

!  X-direction unpack

   CALL da_bdy_fields_halo ( grid%a_u_1 , grid%a_v_1, grid%a_t_1, grid%a_ph_1, grid%a_mu_1, &
                          grid%a_w_1, 1, 0, spec_bdy_width      , &
                          model_grid%g_u_bxs, model_grid%g_u_bxe, model_grid%g_u_bys, model_grid%g_u_bye, &
                          model_grid%g_v_bxs, model_grid%g_v_bxe, model_grid%g_v_bys, model_grid%g_v_bye, &
                          model_grid%g_t_bxs, model_grid%g_t_bxe, model_grid%g_t_bys, model_grid%g_t_bye, &
                          model_grid%g_ph_bxs, model_grid%g_ph_bxe, model_grid%g_ph_bys, model_grid%g_ph_bye, &
                          model_grid%g_mu_bxs, model_grid%g_mu_bxe, model_grid%g_mu_bys, model_grid%g_mu_bye, &
                          model_grid%g_moist_bxs(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_bxe(:,:,:,PARAM_FIRST_SCALAR), &
                          model_grid%g_moist_bys(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_bye(:,:,:,PARAM_FIRST_SCALAR), &
                          grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                          grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                          grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   grid%a_u_1 = 0.0
   grid%a_v_1 = 0.0
   grid%a_t_1 = 0.0
   grid%a_ph_1 = 0.0
   grid%a_mu_1 = 0.0
   grid%a_w_1 = 0.0

!  X-direction pack

   CALL da_bdy_fields_halo ( grid%a_u_1 , grid%a_v_1, grid%a_t_1, grid%a_ph_1, grid%a_mu_1, &
                          grid%a_w_1, 0, 0, spec_bdy_width      , &         
                          model_grid%g_u_btxs, model_grid%g_u_btxe, model_grid%g_u_btys, model_grid%g_u_btye, &
                          model_grid%g_v_btxs, model_grid%g_v_btxe, model_grid%g_v_btys, model_grid%g_v_btye, &
                          model_grid%g_t_btxs, model_grid%g_t_btxe, model_grid%g_t_btys, model_grid%g_t_btye, &
                          model_grid%g_ph_btxs, model_grid%g_ph_btxe, model_grid%g_ph_btys, model_grid%g_ph_btye, &
                          model_grid%g_mu_btxs, model_grid%g_mu_btxe, model_grid%g_mu_btys, model_grid%g_mu_btye, &
                          model_grid%g_moist_btxs(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_btxe(:,:,:,PARAM_FIRST_SCALAR), &
                          model_grid%g_moist_btys(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_btye(:,:,:,PARAM_FIRST_SCALAR), &
                          grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                          grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                          grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

#    include "HALO_EM_BDY.inc"

!  X-direction unpack

   CALL da_bdy_fields_halo ( grid%a_u_1 , grid%a_v_1, grid%a_t_1, grid%a_ph_1, grid%a_mu_1, &
                          grid%a_w_1, 1, 0, spec_bdy_width      , &         
                          model_grid%g_u_btxs, model_grid%g_u_btxe, model_grid%g_u_btys, model_grid%g_u_btye, &
                          model_grid%g_v_btxs, model_grid%g_v_btxe, model_grid%g_v_btys, model_grid%g_v_btye, &
                          model_grid%g_t_btxs, model_grid%g_t_btxe, model_grid%g_t_btys, model_grid%g_t_btye, &
                          model_grid%g_ph_btxs, model_grid%g_ph_btxe, model_grid%g_ph_btys, model_grid%g_ph_btye, &
                          model_grid%g_mu_btxs, model_grid%g_mu_btxe, model_grid%g_mu_btys, model_grid%g_mu_btye, &
                          model_grid%g_moist_btxs(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_btxe(:,:,:,PARAM_FIRST_SCALAR), &
                          model_grid%g_moist_btys(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_btye(:,:,:,PARAM_FIRST_SCALAR), &
                          grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                          grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                          grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   grid%a_u_1 = 0.0
   grid%a_v_1 = 0.0
   grid%a_t_1 = 0.0
   grid%a_ph_1 = 0.0
   grid%a_mu_1 = 0.0
   grid%a_w_1 = 0.0

!  Y-direction pack

   CALL da_bdy_fields_halo ( grid%a_u_1 , grid%a_v_1, grid%a_t_1, grid%a_ph_1, grid%a_mu_1, &
                          grid%a_w_1, 0, 1, spec_bdy_width      , &
                          model_grid%g_u_bxs, model_grid%g_u_bxe, model_grid%g_u_bys, model_grid%g_u_bye, &
                          model_grid%g_v_bxs, model_grid%g_v_bxe, model_grid%g_v_bys, model_grid%g_v_bye, &
                          model_grid%g_t_bxs, model_grid%g_t_bxe, model_grid%g_t_bys, model_grid%g_t_bye, &
                          model_grid%g_ph_bxs, model_grid%g_ph_bxe, model_grid%g_ph_bys, model_grid%g_ph_bye, &
                          model_grid%g_mu_bxs, model_grid%g_mu_bxe, model_grid%g_mu_bys, model_grid%g_mu_bye, &
                          model_grid%g_moist_bxs(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_bxe(:,:,:,PARAM_FIRST_SCALAR), &
                          model_grid%g_moist_bys(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_bye(:,:,:,PARAM_FIRST_SCALAR), &
                          grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                          grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                          grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

#    include "HALO_EM_BDY.inc"

!  Y-direction pack

   CALL da_bdy_fields_halo ( grid%a_u_1 , grid%a_v_1, grid%a_t_1, grid%a_ph_1, grid%a_mu_1, &
                          grid%a_w_1, 1, 1, spec_bdy_width      , &
                          model_grid%g_u_bxs, model_grid%g_u_bxe, model_grid%g_u_bys, model_grid%g_u_bye, &
                          model_grid%g_v_bxs, model_grid%g_v_bxe, model_grid%g_v_bys, model_grid%g_v_bye, &
                          model_grid%g_t_bxs, model_grid%g_t_bxe, model_grid%g_t_bys, model_grid%g_t_bye, &
                          model_grid%g_ph_bxs, model_grid%g_ph_bxe, model_grid%g_ph_bys, model_grid%g_ph_bye, &
                          model_grid%g_mu_bxs, model_grid%g_mu_bxe, model_grid%g_mu_bys, model_grid%g_mu_bye, &
                          model_grid%g_moist_bxs(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_bxe(:,:,:,PARAM_FIRST_SCALAR), &
                          model_grid%g_moist_bys(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_bye(:,:,:,PARAM_FIRST_SCALAR), &
                          grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                          grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                          grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   grid%a_u_1 = 0.0
   grid%a_v_1 = 0.0
   grid%a_t_1 = 0.0
   grid%a_ph_1 = 0.0
   grid%a_mu_1 = 0.0
   grid%a_w_1 = 0.0

!  Y-direction pack

   CALL da_bdy_fields_halo ( grid%a_u_1 , grid%a_v_1, grid%a_t_1, grid%a_ph_1, grid%a_mu_1, &
                          grid%a_w_1, 0, 1, spec_bdy_width      , &
                          model_grid%g_u_btxs, model_grid%g_u_btxe, model_grid%g_u_btys, model_grid%g_u_btye, &
                          model_grid%g_v_btxs, model_grid%g_v_btxe, model_grid%g_v_btys, model_grid%g_v_btye, &
                          model_grid%g_t_btxs, model_grid%g_t_btxe, model_grid%g_t_btys, model_grid%g_t_btye, &
                          model_grid%g_ph_btxs, model_grid%g_ph_btxe, model_grid%g_ph_btys, model_grid%g_ph_btye, &
                          model_grid%g_mu_btxs, model_grid%g_mu_btxe, model_grid%g_mu_btys, model_grid%g_mu_btye, &
                          model_grid%g_moist_btxs(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_btxe(:,:,:,PARAM_FIRST_SCALAR), &
                          model_grid%g_moist_btys(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_btye(:,:,:,PARAM_FIRST_SCALAR), &
                          grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                          grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                          grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

#    include "HALO_EM_BDY.inc"

!  Y-direction unpack

   CALL da_bdy_fields_halo ( grid%a_u_1 , grid%a_v_1, grid%a_t_1, grid%a_ph_1, grid%a_mu_1, &
                          grid%a_w_1, 1, 1, spec_bdy_width      , &
                          model_grid%g_u_btxs, model_grid%g_u_btxe, model_grid%g_u_btys, model_grid%g_u_btye, &
                          model_grid%g_v_btxs, model_grid%g_v_btxe, model_grid%g_v_btys, model_grid%g_v_btye, &
                          model_grid%g_t_btxs, model_grid%g_t_btxe, model_grid%g_t_btys, model_grid%g_t_btye, &
                          model_grid%g_ph_btxs, model_grid%g_ph_btxe, model_grid%g_ph_btys, model_grid%g_ph_btye, &
                          model_grid%g_mu_btxs, model_grid%g_mu_btxe, model_grid%g_mu_btys, model_grid%g_mu_btye, &
                          model_grid%g_moist_btxs(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_btxe(:,:,:,PARAM_FIRST_SCALAR), &
                          model_grid%g_moist_btys(:,:,:,PARAM_FIRST_SCALAR), model_grid%g_moist_btye(:,:,:,PARAM_FIRST_SCALAR), &
                          grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                          grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                          grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   grid%a_u_1 = 0.0
   grid%a_v_1 = 0.0
   grid%a_t_1 = 0.0
   grid%a_ph_1 = 0.0
   grid%a_mu_1 = 0.0
   grid%a_w_1 = 0.0

   ids=ids0; ide=ide0; jds=jds0; jde=jde0; kds=kds0; kde=kde0
   ims=ims0; ime=ime0; jms=jms0; jme=jme0; kms=kms0; kme=kme0
   ips=ips0; ipe=ipe0; jps=jps0; jpe=jpe0; kps=kps0; kpe=kpe0
#endif

   if (trace_use) call da_trace_exit("da_transfer_xatowrftl_lbc")

#endif
end subroutine da_transfer_xatowrftl_lbc


