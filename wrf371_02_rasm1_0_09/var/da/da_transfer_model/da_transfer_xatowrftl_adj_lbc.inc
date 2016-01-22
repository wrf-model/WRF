subroutine da_transfer_xatowrftl_adj_lbc(grid, config_flags, filnam)

   !---------------------------------------------------------------------------
   ! Purpose: Convert WRFTL variables to analysis increments
   !           (inverse of the incremental part of xatowrf)
   !---------------------------------------------------------------------------

   implicit none
   

   type(domain), intent(inout)               :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4, intent(in) :: filnam

#ifdef VAR4D

   ! Local variables

   integer :: i, j, k, ii, jj, spec_bdy_width
   real    :: sdmd, s1md
   real    :: rho_cgrid

   real, dimension(ims:ime,jms:jme,kms:kme) :: a_press
#ifndef A2C
   real, dimension(ims:ime,jms:jme,kms:kme) :: utmp
   real, dimension(ims:ime,jms:jme,kms:kme) :: vtmp
#endif

   integer ndynopt

   if (trace_use) call da_trace_entry("da_transfer_xatowrftl_adj_lbc")

   spec_bdy_width = grid%spec_bdy_width

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

   !---------------------------------------------------------------------------
   ! [6.5] Add adjoint from LBC
   !---------------------------------------------------------------------------

!  CALL a_stuff_bdytend_new ( wbdy3dtemp2 , REAL(interval_seconds) ,                 &
!                             model_grid%a_w_btxs, model_grid%a_w_btxe,     &
!                             model_grid%a_w_btys, model_grid%a_w_btye,     &
!                             'W' , &
!                             spec_bdy_width      , &
!                             grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
!                             grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
!                             grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_new ( ubdy3dtemp2 , REAL(interval_seconds) ,                 &
                              model_grid%a_u_btxs, model_grid%a_u_btxe,     &
                              model_grid%a_u_btys, model_grid%a_u_btye,     &
                              'U' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_new ( vbdy3dtemp2 , REAL(interval_seconds) ,                 &
                              model_grid%a_v_btxs, model_grid%a_v_btxe,     &
                              model_grid%a_v_btys, model_grid%a_v_btye,     &
                              'V' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_new ( tbdy3dtemp2 , REAL(interval_seconds) ,                 &
                              model_grid%a_t_btxs, model_grid%a_t_btxe,     &
                              model_grid%a_t_btys, model_grid%a_t_btye,     &
                              'T' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_new ( pbdy3dtemp2 , REAL(interval_seconds) ,                 &
                              model_grid%a_ph_btxs, model_grid%a_ph_btxe,     &
                              model_grid%a_ph_btys, model_grid%a_ph_btye,     &
                              'W' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_new ( qbdy3dtemp2 , REAL(interval_seconds) ,                 &
                              model_grid%a_moist_btxs(:,:,:,P_A_QV), model_grid%a_moist_btxe(:,:,:,P_A_QV),     &
                              model_grid%a_moist_btys(:,:,:,P_A_QV), model_grid%a_moist_btye(:,:,:,P_A_QV),     &
                              'T' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_stuff_bdytend_new ( mbdy2dtemp2 , REAL(interval_seconds) ,                 &
                              model_grid%a_mu_btxs, model_grid%a_mu_btxe,     &
                              model_grid%a_mu_btys, model_grid%a_mu_btye,     &
                              'M' , &
                              spec_bdy_width      , &
                              grid%sd31, grid%ed31, grid%sd32, grid%ed32, 1, 1, &
                              grid%sm31, grid%em31, grid%sm32, grid%em32, 1, 1, &
                              grid%sp31, grid%ep31, grid%sp32, grid%ep32, 1, 1 )

#ifdef DM_PARALLEL
#include "HALO_EM_E.inc"
#endif
   
!  CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , wbdy3dtemp2 , grid%w_2, grid%a_w_2, 'h' , grid%msfty , &
!                  grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
!                  grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
!                  grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , ubdy3dtemp2 , grid%u_2 , grid%a_u_2 , 'u' , grid%msfuy , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , vbdy3dtemp2 , grid%v_2 , grid%a_v_2 , 'v' , grid%msfvx , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , tbdy3dtemp2 , grid%t_2 , grid%a_t_2 , 't' , grid%msfty , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , pbdy3dtemp2 , grid%ph_2, grid%a_ph_2, 'h' , grid%msfty , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL a_couple ( model_config_flags, grid%mu_2 , grid%a_mu_2, grid%mub , qbdy3dtemp2 , grid%moist(:,:,:,P_A_QV), grid%a_moist(:,:,:,P_A_QV),  't' , grid%msfty , &
                   grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                   grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                   grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   DO j = grid%sp32 , MIN(grid%ed32-1,grid%ep32)
      DO i = grid%sp31 , MIN(grid%ed31-1,grid%ep31)
         grid%a_mu_2(i,j) = grid%a_mu_2(i,j) + mbdy2dtemp2(i,1,j)
         mbdy2dtemp2(i,1,j) = 0.0
      END DO
   END DO

#ifdef DM_PARALLEL
   call da_halo_em_e_ad ( grid%a_mu_2 )
#endif  

   model_grid%a_w_btxs = 0.0
   model_grid%a_w_btxe = 0.0
   model_grid%a_w_btys = 0.0
   model_grid%a_w_btye = 0.0
   model_grid%a_w_bxs = 0.0
   model_grid%a_w_bxe = 0.0
   model_grid%a_w_bys = 0.0
   model_grid%a_w_bye = 0.0

   DEALLOCATE ( ubdy3dtemp2 )
   DEALLOCATE ( vbdy3dtemp2 )
   DEALLOCATE ( tbdy3dtemp2 )
   DEALLOCATE ( pbdy3dtemp2 )
   DEALLOCATE ( qbdy3dtemp2 )
   DEALLOCATE ( mbdy2dtemp2 )
!  DEALLOCATE ( wbdy3dtemp2 )

#ifndef A2C
   !---------------------------------------------------------------------------
   ! [5.0] Adjoint of CONVERT FROM A-GRID TO C-GRID
   !---------------------------------------------------------------------------

   ! Fill the halo region for a_u and a_v.
   utmp=grid%x6a%u
   vtmp=grid%x6a%v
#endif
   grid%x6a%u=grid%a_u_2
   grid%x6a%v=grid%a_v_2

#ifdef A2C
  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
     ipe = ipe + 1
     ide = ide + 1
  end if

  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
     jpe = jpe + 1
     jde = jde + 1
  end if
#endif

#ifdef DM_PARALLEL
#include "HALO_X6A_A.inc"
#endif

#ifdef A2C
  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
     ipe = ipe - 1
     ide = ide - 1
  end if

  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
     jpe = jpe - 1
     jde = jde - 1
  end if
#else
   grid%a_u_2=grid%x6a%u
   grid%a_v_2=grid%x6a%v
   grid%x6a%u=utmp
   grid%x6a%v=vtmp
   utmp=0.0
   vtmp=0.0

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            utmp(i,j,k)=utmp(i,j,k)+0.5*(grid%a_u_2(i+1,j  ,k)+grid%a_u_2(i,j,k))
            vtmp(i,j,k)=vtmp(i,j,k)+0.5*(grid%a_v_2(i  ,j+1,k)+grid%a_v_2(i,j,k))
         end do
      end do
   end do

   utmp(its-1,jts:jte,kts:kte)=utmp(its-1,jts:jte,kts:kte)+0.5*grid%a_u_2(its,jts:jte,kts:kte)
   utmp(ite+1,jts:jte,kts:kte)=utmp(ite+1,jts:jte,kts:kte)+0.5*grid%a_u_2(ite+1,jts:jte,kts:kte)
   vtmp(its:ite,jts-1,kts:kte)=vtmp(its:ite,jts-1,kts:kte)+0.5*grid%a_v_2(its:ite,jts,kts:kte)
   vtmp(its:ite,jte+1,kts:kte)=vtmp(its:ite,jte+1,kts:kte)+0.5*grid%a_v_2(its:ite,jte+1,kts:kte)

   ! The western boundary
   if (its == grid%xp%ids) then
      grid%x6a%u(its  ,jts:jte,kts:kte)=grid%x6a%u(its  ,jts:jte,kts:kte)+2.0*utmp(its-1,jts:jte,kts:kte)
      grid%x6a%u(its+1,jts:jte,kts:kte)=grid%x6a%u(its+1,jts:jte,kts:kte)-utmp(its-1,jts:jte,kts:kte)
   end if

   ! The eastern boundary
   if (ite == grid%xp%ide) then
      grid%x6a%u(ite  ,jts:jte,kts:kte)=grid%x6a%u(ite  ,jts:jte,kts:kte)+2.0*utmp(ite+1,jts:jte,kts:kte)
      grid%x6a%u(ite-1,jts:jte,kts:kte)=grid%x6a%u(ite-1,jts:jte,kts:kte)-utmp(ite+1,jts:jte,kts:kte)
   end if

   grid%x6a%u=grid%x6a%u+utmp

   ! The southern boundary
   if (jts == grid%xp%jds) then
      grid%x6a%v(its:ite,jts  ,kts:kte)=grid%x6a%v(its:ite,jts  ,kts:kte)+2.0*vtmp(its:ite,jts-1,kts:kte)
      grid%x6a%v(its:ite,jts+1,kts:kte)=grid%x6a%v(its:ite,jts+1,kts:kte)-vtmp(its:ite,jts-1,kts:kte)
   end if

   ! The northern boundary
   if (jte == grid%xp%jde) then
      grid%x6a%v(its:ite,jte  ,kts:kte)=grid%x6a%v(its:ite,jte  ,kts:kte)+2.0*vtmp(its:ite,jte+1,kts:kte)
      grid%x6a%v(its:ite,jte-1,kts:kte)=grid%x6a%v(its:ite,jte-1,kts:kte)-vtmp(its:ite,jte+1,kts:kte)
   end if

   grid%x6a%v=grid%x6a%v+vtmp
#endif

   grid%a_u_2 = 0.0
   grid%a_v_2 = 0.0

   !---------------------------------------------------------------------------
   ! [4.0] Adjoint of CONVERT TEMPERATURE inCREMENTS inTO THETA inCREMENTS
   !       EVALUATE ALSO THE inCREMENTS OF (1/rho) AND GEOPOTENTIAL
   !---------------------------------------------------------------------------

   a_press(its:ite,jts:jte,kts:kte+1)=0.0
!  do k=kte,kts,-1
!     do j=jts,jte
!        do i=its,ite
!           rho_cgrid=-(grid%ph_2(i,j,k+1)-grid%ph_2(i,j,k))*grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
!           a_press(i,j,k )=a_press(i,j,k )+grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
!           a_press(i,j,k+1)=a_press(i,j,k+1)-grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
!           grid%a_ph_2(i,j,k ) =grid%a_ph_2(i,j,k)   +grid%a_ph_2(i,j,k+1)
!           grid%x6a%q(i,j,k)=grid%x6a%q(i,j,k)-grid%xb%rho(i,j,k)*0.61*rho_cgrid/(1.0+0.61*grid%xb%q(i,j,k))
!           grid%x6a%t(i,j,k)=grid%x6a%t(i,j,k)-grid%xb%rho(i,j,k)*rho_cgrid/grid%xb%t(i,j,k)
!           grid%x6a%p(i,j,k)= grid%x6a%p(i,j,k)+grid%xb%rho(i,j,k)*rho_cgrid/grid%xb%p(i,j,k)
!        end do
!     end do
!  end do

   do k=kts,kte
      do j=jts,jte
         do i=its,ite 
            grid%x6a%p(i,j,k)=grid%x6a%p(i,j,k)-(t0+grid%t_2(i,j,k))*kappa*grid%a_t_2(i,j,k)/grid%xb%p(i,j,k)
            grid%x6a%t(i,j,k)=grid%x6a%t(i,j,k)+(t0+grid%t_2(i,j,k))*grid%a_t_2(i,j,k)/grid%xb%t(i,j,k)
         end do
      end do
   end do

   grid%a_t_2 = 0.0
   grid%a_ph_2 = 0.0
 
   !---------------------------------------------------------------------------
   ! [3.0] Adjoint of COMPUTE pressure increments (for computing theta increments)
   !---------------------------------------------------------------------------

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            a_press(i,j,k+1)=a_press(i,j,k+1)+0.5*grid%x6a%p(i,j,k)
            a_press(i,j,k )=a_press(i,j,k )+0.5*grid%x6a%p(i,j,k)
            grid%x6a%p(i,j,k)=0.0
            grid%a_moist(i,j,k,P_A_QV)=grid%a_moist(i,j,k,P_A_QV)-(grid%mu_2(i,j)+grid%mub(i,j))*a_press(i,j,k)*grid%dn(k)
            grid%a_mu_2(i,j)=grid%a_mu_2(i,j)-a_press(i,j,k)*(1.0+grid%moist(i,j,k,P_QV))*grid%dn(k)
            a_press(i,j,k+1)=a_press(i,j,k+1)+a_press(i,j,k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] Adjoint of COMPUTE increments of dry-column air mass per unit area
   !---------------------------------------------------------------------------

   do j=jts,jte
      do i=its,ite
         sdmd=0.0
         s1md=0.0
         do k=kts,kte
            s1md=s1md+(1.0+grid%moist(i,j,k,P_QV))*grid%dnw(k)
         end do
         sdmd=sdmd-grid%xb%psac(i,j)*grid%a_mu_2(i,j)/s1md
         grid%x6a%psfc(i,j)=grid%x6a%psfc(i,j)-grid%a_mu_2(i,j)/s1md
         do k=kts,kte
            grid%a_moist(i,j,k,P_A_QV)=grid%a_moist(i,j,k,P_A_QV)+sdmd*grid%dnw(k)
         end do
      end do
   end do

   grid%a_mu_2 = 0.0
   !---------------------------------------------------------------------------
   ! [1.0] Adjoint of Get the mixing ratio of moisture 
   !---------------------------------------------------------------------------
   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            grid%x6a%q(i,j,k)=grid%x6a%q(i,j,k)+grid%a_moist(i,j,k,P_A_QV)/(1.0-grid%xb%q(i,j,k))**2
         end do
      end do
   end do

   grid%a_moist = 0.0

   do k=kts,kte+1
      do j=jts,jte
         do i=its,ite
            grid%x6a%w(i,j,k)=grid%x6a%w(i,j,k)+grid%a_w_2(i,j,k)
         end do
      end do
   end do

   grid%a_w_2 = 0.0

   if (trace_use) call da_trace_exit("da_transfer_xatowrftl_adj_lbc")

#endif
end subroutine da_transfer_xatowrftl_adj_lbc


