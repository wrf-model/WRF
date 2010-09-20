subroutine da_transfer_xatowrftl_adj_lbc(grid, config_flags, filnam)

   !---------------------------------------------------------------------------
   ! Purpose: Convert WRFTL variables to analysis increments
   !           (inverse of the incremental part of xatowrf)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4, intent(in) :: filnam

   ! Local variables

   integer :: i, j, k, ii, jj, spec_bdy_width
   real    :: sdmd, s1md
   real    :: rho_cgrid

   real, dimension(ims:ime,jms:jme,kms:kme) :: a_press
#ifndef A2C
   real, dimension(ims:ime,jms:jme,kms:kme) :: utmp
   real, dimension(ims:ime,jms:jme,kms:kme) :: vtmp
   real, dimension(ims:ime,jms:jme) :: mut
   real, dimension(ims:ime,jms:jme) :: muu
   real, dimension(ims:ime,jms:jme) :: muv
#endif

   integer ndynopt

   if (trace_use) call da_trace_entry("da_transfer_xatowrftl_adj_lbc")

   spec_bdy_width = grid%spec_bdy_width

   !---------------------------------------------------------------------------
   ! [7.0] Adjoint of outPUT (inPUT)
   !---------------------------------------------------------------------------

   call da_med_boundary_input(grid , config_flags, "wrfbdy_ad01")

   !---------------------------------------------------------------------------
   ! [6.5] Add adjoint from LBC
   !---------------------------------------------------------------------------

   do j=jts,jte
      do i=its,ite
         mut(i,j) = grid%mu_2(i,j)+grid%mub(i,j)
      end do

      mut(its-1,j) = mut(its,j)
      mut(ite+1,j) = mut(ite,j)
   end do

   do i=its-1,ite+1
      mut(i,jts-1)=mut(i,jts)
      mut(i,jte+1)=mut(i,jte)
   end do

   do j=jts,jte
      do i=its,ite+1
         muu(i,j) = 0.5*(mut(i,j)+mut(i-1,j))
      end do
   end do

   do j=jts,jte+1
      do i=its,ite
         muv(i,j) = 0.5*(mut(i,j)+mut(i,j-1))
      end do
   end do


   ! A_U

   do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jde-1,jte)
         do i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
            grid%a_u_2(i,j,k) = grid%a_u_btxs(j,k,i)/REAL(interval_seconds)*muu(i,j)/grid%msfuy(i,j)
         end do
      end do
   end do
   do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jde-1,jte)
         do i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
            ii = ide - i + 1
            grid%a_u_2(i,j,k) = grid%a_u_btxe(j,k,ii)/REAL(interval_seconds)*muu(i,j)/grid%msfuy(i,j)
         end do
      end do
   end do
   grid%a_u_btxs = 0.0
   grid%a_u_bxs = 0.0
   grid%a_u_btxe = 0.0
   grid%a_u_bxe = 0.0
   do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
         do i = MAX(ids,its) , MIN(ide,ite)
            grid%a_u_2(i,j,k) = grid%a_u_btys(i,k,j)/REAL(interval_seconds)*muu(i,j)/grid%msfuy(i,j)
         end do
      end do
   end do
   do k = kds , kde - 1
      do j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
         jj = jde - j
         do i = MAX(ids,its) , MIN(ide,ite)
            grid%a_u_2(i,j,k) = grid%a_u_btye(i,k,jj)/REAL(interval_seconds)*muu(i,j)/grid%msfuy(i,j)
         end do
      end do
   end do
   grid%a_u_btys = 0.0
   grid%a_u_bys = 0.0
   grid%a_u_btye = 0.0
   grid%a_u_bye = 0.0

   ! A_V

   do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jde,jte)
         do i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
            grid%a_v_2(i,j,k) = grid%a_v_btxs(j,k,i)/REAL(interval_seconds)*muv(i,j)/grid%msfvx(i,j)
         end do
      end do
   end do
   do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jde,jte)
         do i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
            ii = ide - i
            grid%a_v_2(i,j,k) = grid%a_v_btxe(j,k,ii)/REAL(interval_seconds)*muv(i,j)/grid%msfvx(i,j)
         end do
      end do
   end do
   grid%a_v_btxs = 0.0
   grid%a_v_bxs = 0.0
   grid%a_v_btxe = 0.0
   grid%a_v_bxe = 0.0
   do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
         do i = MAX(ids,its) , MIN(ide-1,ite)
            grid%a_v_2(i,j,k) = grid%a_v_btys(i,k,j)/REAL(interval_seconds)*muv(i,j)/grid%msfvx(i,j)
         end do
      end do
   end do
   do k = kds , kde - 1
      do j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
         jj = jde - j + 1
         do i = MAX(ids,its) , MIN(ide-1,ite)
            grid%a_v_2(i,j,k) = grid%a_v_btye(i,k,jj)/REAL(interval_seconds)*muv(i,j)/grid%msfvx(i,j)
         end do
      end do
   end do
   grid%a_v_btys = 0.0
   grid%a_v_bys = 0.0
   grid%a_v_btye = 0.0
   grid%a_v_bye = 0.0

   ! A_T

   do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jde-1,jte)
         do i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
            grid%a_t_2(i,j,k) = grid%a_t_btxs(j,k,i)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
  do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jde-1,jte)
         do i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
            ii = ide - i
            grid%a_t_2(i,j,k) = grid%a_t_btxe(j,k,ii)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
   grid%a_t_btxs = 0.0
   grid%a_t_bxs = 0.0
   grid%a_t_btxe = 0.0
   grid%a_t_bxe = 0.0
   do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
         do i = MAX(ids,its) , MIN(ide-1,ite)
            grid%a_t_2(i,j,k) = grid%a_t_btys(i,k,j)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
   do k = kds , kde - 1
      do j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
         jj = jde - j
         do i = MAX(ids,its) , MIN(ide-1,ite)
            grid%a_t_2(i,j,k) = grid%a_t_btye(i,k,jj)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
   grid%a_t_btys = 0.0
   grid%a_t_bys = 0.0
   grid%a_t_btye = 0.0
   grid%a_t_bye = 0.0

   ! A_PH

   do k = kds , kde
      do j = MAX(jds,jts) , MIN(jde-1,jte)
         do i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
            grid%a_ph_2(i,j,k) = grid%a_ph_btxs(j,k,i)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
   do k = kds , kde
      do j = MAX(jds,jts) , MIN(jde-1,jte)
         do i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
            ii = ide - i
            grid%a_ph_2(i,j,k) = grid%a_ph_btxe(j,k,ii)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
   grid%a_ph_btxs = 0.0
   grid%a_ph_bxs = 0.0
   grid%a_ph_btxe = 0.0
   grid%a_ph_bxe = 0.0
   do k = kds , kde
      do j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
         do i = MAX(ids,its) , MIN(ide-1,ite)
            grid%a_ph_2(i,j,k) = grid%a_ph_btys(i,k,j)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
   do k = kds , kde
      do j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
         jj = jde - j
         do i = MAX(ids,its) , MIN(ide-1,ite)
            grid%a_ph_2(i,j,k) = grid%a_ph_btye(i,k,jj)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
   grid%a_ph_btys = 0.0
   grid%a_ph_bys = 0.0
   grid%a_ph_btye = 0.0
   grid%a_ph_bye = 0.0

   ! A_MU

   do j = MAX(jds,jts) , MIN(jde-1,jte)
      do i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
         grid%a_mu_2(i,j) = grid%a_mu_btxs(j,1,i)/REAL(interval_seconds)
      end do
   end do
   do j = MAX(jds,jts) , MIN(jde-1,jte)
      do i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
         ii = ide - i
         grid%a_mu_2(i,j) = grid%a_mu_btxe(j,1,ii)/REAL(interval_seconds)
      end do
   end do
   grid%a_mu_btxs = 0.0
   grid%a_mu_bxs = 0.0
   grid%a_mu_btxe = 0.0
   grid%a_mu_bxe = 0.0
   do j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
      do i = MAX(ids,its) , MIN(ide-1,ite)
         grid%a_mu_2(i,j) = grid%a_mu_btys(i,1,j)/REAL(interval_seconds)
      end do
   end do
   do j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
      jj = jde - j
      do i = MAX(ids,its) , MIN(ide-1,ite)
         grid%a_mu_2(i,j) = grid%a_mu_btye(i,1,jj)/REAL(interval_seconds)
      end do
   end do
   grid%a_mu_btys = 0.0
   grid%a_mu_bys = 0.0
   grid%a_mu_btye = 0.0
   grid%a_mu_bye = 0.0

   ! A_MOIST

   do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jde-1,jte)
         do i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
            grid%a_moist(i,j,k,P_A_QV) = grid%a_moist_btxs(j,k,i,P_A_QV)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
   do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jde-1,jte)
         do i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
            ii = ide -i
            grid%a_moist(i,j,k,P_A_QV) = grid%a_moist_btxe(j,k,ii,P_A_QV)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
   grid%a_moist_btxs = 0.0
   grid%a_moist_bxs = 0.0
   grid%a_moist_btxe = 0.0
   grid%a_moist_bxe = 0.0
   do k = kds , kde - 1
      do j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
         do i = MAX(ids,its) , MIN(ide-1,ite)
            grid%a_moist(i,j,k,P_A_QV) = grid%a_moist_btys(i,k,j,P_A_QV)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
   do k = kds , kde - 1
      do j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
         jj = jde - j
         do i = MAX(ids,its) , MIN(ide-1,ite)
            grid%a_moist(i,j,k,P_A_QV) = grid%a_moist_btye(i,k,jj,P_A_QV)/REAL(interval_seconds)*(grid%mu_2(i,j)+grid%mub(i,j))
         end do
      end do
   end do
   grid%a_moist_btys = 0.0
   grid%a_moist_bys = 0.0
   grid%a_moist_btye = 0.0
   grid%a_moist_bye = 0.0

   grid%a_w_2 = 0.0

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
#include "HALO_XA_A.inc"
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
   do k=kte,kts,-1
      do j=jts,jte
         do i=its,ite
            rho_cgrid=-(grid%ph_2(i,j,k+1)-grid%ph_2(i,j,k))*grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
            a_press(i,j,k )=a_press(i,j,k )+grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
            a_press(i,j,k+1)=a_press(i,j,k+1)-grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
            grid%a_ph_2(i,j,k ) =grid%a_ph_2(i,j,k)   +grid%a_ph_2(i,j,k+1)
            grid%x6a%q(i,j,k)=grid%x6a%q(i,j,k)-grid%xb%rho(i,j,k)*0.61*rho_cgrid/(1.0+0.61*grid%xb%q(i,j,k))
            grid%x6a%t(i,j,k)=grid%x6a%t(i,j,k)-grid%xb%rho(i,j,k)*rho_cgrid/grid%xb%t(i,j,k)
            grid%x6a%p(i,j,k)= grid%x6a%p(i,j,k)+grid%xb%rho(i,j,k)*rho_cgrid/grid%xb%p(i,j,k)
         end do
      end do
   end do

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

   if (trace_use) call da_trace_exit("da_transfer_xatowrftl_adj_lbc")

end subroutine da_transfer_xatowrftl_adj_lbc


