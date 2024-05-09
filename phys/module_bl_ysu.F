#define NEED_B4B_DURING_CCPP_TESTING 1
!=================================================================================================================
 module module_bl_ysu
 use ccpp_kind_types,only: kind_phys
 use bl_ysu


 implicit none
 private
 public:: ysu


 contains


!=================================================================================================================
   subroutine ysu(u3d,v3d,t3d,qv3d,qc3d,qi3d,p3d,p3di,pi3d,                    &
                  rublten,rvblten,rthblten,                                    &
                  rqvblten,rqcblten,rqiblten,flag_qc,flag_qi,                  &
                  cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,                    &
                  dz8w,psfc,                                                   &
                  znt,ust,hpbl,psim,psih,                                      &
                  xland,hfx,qfx,wspd,br,                                       &
                  dt,kpbl2d,                                                   &
                  exch_h,exch_m,                                               &
                  wstar,delta,                                                 &
                  u10,v10,                                                     &
                  uoce,voce,                                                   &
                  rthraten,ysu_topdown_pblmix,                                 &
                  ctopo,ctopo2,                                                &
                  idiff,flag_bep,frc_urb2d,                                    &
                  a_u_bep,a_v_bep,a_t_bep,                                     &
                  a_q_bep,                                                     &
                  a_e_bep,b_u_bep,b_v_bep,                                     &
                  b_t_bep,b_q_bep,                                             &
                  b_e_bep,dlg_bep,                                             &
                  dl_u_bep,sf_bep,vl_bep,                                      &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte,                                   &
                  errmsg,errflg                                                &
                 )
!-------------------------------------------------------------------------------
  implicit none
!-------------------------------------------------------------------------------
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- th3d        3d potential temperature (k)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- qc3d        3d cloud mixing ratio (kg/kg)
!-- qi3d        3d ice mixing ratio (kg/kg)
!               (note: if P_QI<PARAM_FIRST_SCALAR this should be zero filled)
!-- p3d         3d pressure (pa)
!-- p3di        3d pressure (pa) at interface level
!-- pi3d        3d exner function (dimensionless)
!-- rr3d        3d dry air density (kg/m^3)
!-- rublten     u tendency due to
!               pbl parameterization (m/s/s)
!-- rvblten     v tendency due to
!               pbl parameterization (m/s/s)
!-- rthblten    theta tendency due to
!               pbl parameterization (K/s)
!-- rqvblten    qv tendency due to
!               pbl parameterization (kg/kg/s)
!-- rqcblten    qc tendency due to
!               pbl parameterization (kg/kg/s)
!-- rqiblten    qi tendency due to
!               pbl parameterization (kg/kg/s)
!-- cp          heat capacity at constant pressure for dry air (j/kg/k)
!-- g           acceleration due to gravity (m/s^2)
!-- rovcp       r/cp
!-- rd          gas constant for dry air (j/kg/k)
!-- rovg        r/g
!-- dz8w        dz between full levels (m)
!-- xlv         latent heat of vaporization (j/kg)
!-- rv          gas constant for water vapor (j/kg/k)
!-- psfc        pressure at the surface (pa)
!-- znt         roughness length (m)
!-- ust         u* in similarity theory (m/s)
!-- hpbl        pbl height (m)
!-- psim        similarity stability function for momentum
!-- psih        similarity stability function for heat
!-- xland       land mask (1 for land, 2 for water)
!-- hfx         upward heat flux at the surface (w/m^2)
!-- qfx         upward moisture flux at the surface (kg/m^2/s)
!-- wspd        wind speed at lowest model level (m/s)
!-- u10         u-wind speed at 10 m (m/s)
!-- v10         v-wind speed at 10 m (m/s)
!-- uoce        sea surface zonal currents (m s-1)
!-- voce        sea surface meridional currents (m s-1)
!-- br          bulk richardson number in surface layer
!-- dt          time step (s)
!-- rvovrd      r_v divided by r_d (dimensionless)
!-- ep1         constant for virtual temperature (r_v/r_d - 1)
!-- ep2         constant for specific humidity calculation
!-- karman      von karman constant
!-- idiff       diff3d BEP/BEM+BEM diffusion flag
!-- flag_bep    flag to use BEP/BEP+BEM
!-- frc_urb2d   urban fraction
!-- a_u_bep     BEP/BEP+BEM implicit component u-mom
!-- a_v_bep     BEP/BEP+BEM implicit component v-mom
!-- a_t_bep     BEP/BEP+BEM implicit component pot. temp.
!-- a_q_bep     BEP/BEP+BEM implicit component vapor mixing ratio
!-- a_e_bep     BEP/BEP+BEM implicit component TKE
!-- b_u_bep     BEP/BEP+BEM explicit component u-mom
!-- b_v_bep     BEP/BEP+BEM explicit component v-mom
!-- b_t_bep     BEP/BEP+BEM explicit component pot.temp.
!-- b_q_bep     BEP/BEP+BEM explicit component vapor mixing ratio
!-- b_e_bep     BEP/BEP+BEM explicit component TKE
!-- dlg_bep     Height above ground Martilli et al. (2002) Eq. 24
!-- dl_u_bep    modified length scale Martilli et al. (2002) Eq. 22
!-- sf_bep      fraction of vertical surface not occupied by buildings
!-- vl_bep      volume fraction of grid cell not occupied by buildings
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!-------------------------------------------------------------------------------
!
!
   integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte

   integer,  intent(in)      ::      ysu_topdown_pblmix
!
   real(kind=kind_phys),     intent(in   )   ::      dt,cp,g,rovcp,rovg,rd,xlv,rv
!
   real(kind=kind_phys),     intent(in )     ::      ep1,ep2,karman
!
   real(kind=kind_phys),     dimension( ims:ime, kms:kme, jms:jme )          , &
             intent(in   )   ::                                          qv3d, &
                                                                         qc3d, &
                                                                         qi3d, &
                                                                          p3d, &
                                                                         pi3d, &
                                                                          t3d, &
                                                                         dz8w, &
                                                                     rthraten
   real(kind=kind_phys),     dimension( ims:ime, kms:kme, jms:jme )          , &
             intent(in   )   ::                                          p3di
!
   real(kind=kind_phys),     dimension( ims:ime, kms:kme, jms:jme )          , &
             intent(out  )   ::                                       rublten, &
                                                                      rvblten, &
                                                                     rthblten, &
                                                                     rqvblten, &
                                                                     rqcblten, &
                                                                     rqiblten
!
   real(kind=kind_phys),     dimension( ims:ime, kms:kme, jms:jme )          , &
             intent(out  )   ::                                        exch_h, &
                                                                       exch_m
   real(kind=kind_phys),     dimension( ims:ime, jms:jme )                   , &
             intent(out  )   ::                                         wstar
   real(kind=kind_phys),     dimension( ims:ime, jms:jme )                   , &
             intent(out  )   ::                                         delta
   real(kind=kind_phys),     dimension( ims:ime, jms:jme )                   , &
             intent(inout)   ::                                           u10, &
                                                                          v10
   real(kind=kind_phys),     dimension( ims:ime, jms:jme )                   , &
             intent(in   )   ::                                          uoce, &
                                                                         voce
!
   real(kind=kind_phys),     dimension( ims:ime, jms:jme )                   , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx, &
                                                                           br, &
                                                                         psfc
   real(kind=kind_phys),     dimension( ims:ime, jms:jme )                   , &
             intent(in   )   ::                                                &
                                                                         psim, &
                                                                         psih
   real(kind=kind_phys),     dimension( ims:ime, jms:jme )                   , &
             intent(in   )   ::                                           znt, &
                                                                          ust, &
                                                                          wspd
   real(kind=kind_phys),     dimension( ims:ime, jms:jme )                   , &
             intent(out  )   ::                                          hpbl
!
   real(kind=kind_phys),     dimension( ims:ime, kms:kme, jms:jme )          , &
             intent(in   )   ::                                           u3d, &
                                                                          v3d
!
   integer,  dimension( ims:ime, jms:jme )                                   , &
             intent(out  )   ::                                        kpbl2d
!
   logical,  intent(in)      ::                                       flag_qc, &
                                                                      flag_qi
!
   integer,  intent(in)      ::                                          idiff
   logical,  intent(in)      ::                                       flag_bep
   real(kind=kind_phys),     dimension( ims:ime, kms:kme, jms:jme )          , &
             optional                                                        , &
             intent(in)      ::                                       a_u_bep, &
                                                              a_v_bep,a_t_bep, &
                                                              a_e_bep,b_u_bep, &
                                                              a_q_bep,b_q_bep, &
                                                              b_v_bep,b_t_bep, &
                                                              b_e_bep,dlg_bep, &
                                                                     dl_u_bep, &
                                                                vl_bep,sf_bep
   real(kind=kind_phys),     dimension(ims:ime,jms:jme)                      , &
             optional                                                        , &
             intent(in)      ::                                     frc_urb2d
!
   real(kind=kind_phys),     dimension( ims:ime, jms:jme )                   , &
             optional                                                        , &
             intent(in   )   ::                                         ctopo, &
                                                                       ctopo2
!
   character(len=*), intent(out)   ::                                  errmsg
   integer,          intent(out)   ::                                  errflg
!local
   integer ::  i,j,k

!temporary allocation of local chemical species and/or passive tracers that are vertically-
!mixed in subroutine bl_ysu_run:
   logical:: l_topdown_pblmix

   integer,  parameter :: nmix = 0
   integer :: n

   real(kind=kind_phys),   dimension(ims:ime,kms:kme,jms:jme,nmix)::       qmix
   real(kind=kind_phys),   dimension(ims:ime,kms:kme,jms:jme,nmix):: rqmixblten

   !  Local tile-sized arrays for contiguous data for bl_ysu_run call.

   real(kind=kind_phys),   dimension(its:ite,kts:kte,nmix) ::              &
                                                             qmix_hv     , &
                                                             rqmixblten_hv

   real(kind=kind_phys),   dimension(its:ite,kts:kte)      ::              &
                                                             u3d_hv      , &
                                                             v3d_hv      , &
                                                             t3d_hv      , &
                                                             qv3d_hv     , &
                                                             qc3d_hv     , &
                                                             qi3d_hv     , &
                                                             p3d_hv      , &
                                                             pi3d_hv     , &
                                                             rublten_hv  , &
                                                             rvblten_hv  , &
                                                             rthblten_hv , &
                                                             rqvblten_hv , &
                                                             rqcblten_hv , &
                                                             rqiblten_hv , &
                                                             dz8w_hv     , &
                                                             exch_h_hv   , &
                                                             exch_m_hv   , &
                                                             rthraten_hv

   real(kind=kind_phys),   dimension(its:ite,kts:kte)      ::              &
                                                             a_u_hv      , &
                                                             a_v_hv      , &
                                                             a_t_hv      , &
                                                             a_e_hv      , &
                                                             b_u_hv      , &
                                                             a_q_hv      , &
                                                             b_q_hv      , &
                                                             b_v_hv      , &
                                                             b_t_hv      , &
                                                             b_e_hv      , &
                                                             dlg_hv      , &
                                                             dl_u_hv     , &
                                                             vlk_hv      , &
                                                             sfk_hv
   real(kind=kind_phys),   dimension(its:ite,kts:kte+1)    ::              &
                                                             p3di_hv

   real(kind=kind_phys),   dimension(its:ite)              ::              &
                                                             psfc_hv     , &
                                                             znt_hv      , &
                                                             ust_hv      , &
                                                             hpbl_hv     , &
                                                             psim_hv     , &
                                                             psih_hv     , &
                                                             xland_hv    , &
                                                             hfx_hv      , &
                                                             qfx_hv      , &
                                                             wspd_hv     , &
                                                             br_hv       , &
                                                             wstar_hv    , &
                                                             delta_hv    , &
                                                             u10_hv      , &
                                                             v10_hv      , &
                                                             uoce_hv     , &
                                                             voce_hv     , &
                                                             ctopo_hv    , &
                                                             ctopo2_hv

   integer,                dimension(its:ite)              ::              &
                                                             kpbl2d_hv
   real,                   dimension(its:ite)              ::              &
                                                             frcurb_hv

!-----------------------------------------------------------------------------------------------------------------

   l_topdown_pblmix = .false.
   if(ysu_topdown_pblmix .eq. 1) l_topdown_pblmix = .true.

   do j = jts,jte
!
      !  Assign input data to local tile-sized arrays.

      do n = 1, nmix
         do k = kts, kte
            do i = its, ite
               qmix_hv(i,k,n) = qmix(i,k,j,n)
            end do
         end do
      end do

      do k = kts, kte+1
         do i = its, ite
            p3di_hv(i,k) = p3di(i,k,j)
         end do
      end do

      do k = kts, kte
         do i = its, ite
            u3d_hv(i,k) = u3d(i,k,j)
            v3d_hv(i,k) = v3d(i,k,j)
            t3d_hv(i,k) = t3d(i,k,j)
            qv3d_hv(i,k) = qv3d(i,k,j)
            qc3d_hv(i,k) = qc3d(i,k,j)
            qi3d_hv(i,k) = qi3d(i,k,j)
            p3d_hv(i,k) = p3d(i,k,j)
            pi3d_hv(i,k) = pi3d(i,k,j)
            dz8w_hv(i,k) = dz8w(i,k,j)
            rthraten_hv(i,k) = rthraten(i,k,j)
         end do
      end do

      if(present(a_u_bep) .and. present(a_v_bep) .and. present(a_t_bep) .and.  &
         present(a_q_bep) .and. present(a_e_bep) .and. present(b_u_bep) .and.  &
         present(b_v_bep) .and. present(b_t_bep) .and. present(b_q_bep) .and.  &
         present(b_e_bep) .and. present(dlg_bep) .and. present(dl_u_bep) .and. &
         present(sf_bep)  .and. present(vl_bep)  .and. present(frc_urb2d)) then
         do k = kts, kte
            do i = its,ite
               a_u_hv(i,k)  = a_u_bep(i,k,j)
               a_v_hv(i,k)  = a_v_bep(i,k,j)
               a_t_hv(i,k)  = a_t_bep(i,k,j)
               a_q_hv(i,k)  = a_q_bep(i,k,j)
               a_e_hv(i,k)  = a_e_bep(i,k,j)
               b_u_hv(i,k)  = b_u_bep(i,k,j)
               b_v_hv(i,k)  = b_v_bep(i,k,j)
               b_t_hv(i,k)  = b_t_bep(i,k,j)
               b_q_hv(i,k)  = b_q_bep(i,k,j)
               b_e_hv(i,k)  = b_e_bep(i,k,j)
               dlg_hv(i,k)  = dlg_bep(i,k,j)
               dl_u_hv(i,k) = dl_u_bep(i,k,j)
               vlk_hv(i,k) = vl_bep(i,k,j)
               sfk_hv(i,k)  = sf_bep(i,k,j)
            enddo
         enddo
         do i = its, ite
            frcurb_hv(i) = frc_urb2d(i,j)
         enddo
      endif

      do i = its, ite
         psfc_hv(i) = psfc(i,j)
         znt_hv(i) = znt(i,j)
         ust_hv(i) = ust(i,j)
         wspd_hv(i) = wspd(i,j)
         psim_hv(i) = psim(i,j)
         psih_hv(i) = psih(i,j)
         xland_hv(i) = xland(i,j)
         hfx_hv(i) = hfx(i,j)
         qfx_hv(i) = qfx(i,j)
         br_hv(i) = br(i,j)
         u10_hv(i) = u10(i,j)
         v10_hv(i) = v10(i,j)
         uoce_hv(i) = uoce(i,j)
         voce_hv(i) = voce(i,j)
         ctopo_hv(i) = ctopo(i,j)
         ctopo2_hv(i) = ctopo2(i,j)
      end do
!
      call bl_ysu_run(ux=u3d_hv,vx=v3d_hv                                      &
              ,tx=t3d_hv                                                       &
              ,qvx=qv3d_hv,qcx=qc3d_hv,qix=qi3d_hv                             &
              ,f_qc=flag_qc,f_qi=flag_qi                                       &
              ,nmix=nmix,qmix=qmix_hv                                          &
              ,p2d=p3d_hv,p2di=p3di_hv                                         &
              ,pi2d=pi3d_hv                                                    &
              ,utnp=rublten_hv,vtnp=rvblten_hv                                 &
              ,ttnp=rthblten_hv,qvtnp=rqvblten_hv                              &
              ,qctnp=rqcblten_hv,qitnp=rqiblten_hv                             &
              ,qmixtnp=rqmixblten_hv                                           &
              ,cp=cp,g=g,rovcp=rovcp,rd=rd,rovg=rovg                           &    
              ,xlv=xlv,rv=rv                                                   &
              ,ep1=ep1,ep2=ep2,karman=karman                                   &
              ,dz8w2d=dz8w_hv                                                  &
              ,psfcpa=psfc_hv,znt=znt_hv,ust=ust_hv                            &
              ,hpbl=hpbl_hv                                                    &
              ,psim=psim_hv                                                    &
              ,psih=psih_hv,xland=xland_hv                                     &
              ,hfx=hfx_hv,qfx=qfx_hv                                           &
              ,wspd=wspd_hv,br=br_hv                                           &
              ,dt=dt,kpbl1d=kpbl2d_hv                                          &
              ,exch_hx=exch_h_hv                                               &
              ,exch_mx=exch_m_hv                                               &
              ,wstar=wstar_hv                                                  &
              ,delta=delta_hv                                                  &
              ,u10=u10_hv,v10=v10_hv                                           &
              ,uox=uoce_hv,vox=voce_hv                                         &
              ,rthraten=rthraten_hv                                            &
              ,ysu_topdown_pblmix=l_topdown_pblmix                             &
              ,ctopo=ctopo_hv,ctopo2=ctopo2_hv                                 &
              ,a_u=a_u_hv,a_v=a_v_hv,a_t=a_t_hv,a_q=a_q_hv,a_e=a_e_hv          &
              ,b_u=b_u_hv,b_v=b_v_hv,b_t=b_t_hv,b_q=b_q_hv,b_e=b_e_hv          &
              ,sfk=sfk_hv,vlk=vlk_hv,dlu=dl_u_hv,dlg=dlg_hv,frcurb=frcurb_hv   &
              ,flag_bep=flag_bep                                               &
              ,its=its,ite=ite,kte=kte,kme=kme                                 &
              ,errmsg=errmsg,errflg=errflg                                     )
!
      !  Assign local data back to full-sized arrays.
      !  Only required for the INTENT(OUT) or INTENT(INOUT) arrays.

      do n = 1, nmix
         do k = kts, kte
            do i = its, ite
               rqmixblten(i,k,j,n) = rqmixblten_hv(i,k,n)
            end do
         end do
      end do

      do k = kts, kte
         do i = its, ite
            rublten(i,k,j) = rublten_hv(i,k)
            rvblten(i,k,j) = rvblten_hv(i,k)
#if (NEED_B4B_DURING_CCPP_TESTING == 1)
            rthblten(i,k,j) = rthblten_hv(i,k)/pi3d_hv(i,k)
#elif (NEED_B4B_DURING_CCPP_TESTING != 1)
            rthblten(i,k,j) = rthblten_hv(i,k)
#endif
            rqvblten(i,k,j) = rqvblten_hv(i,k)
            rqcblten(i,k,j) = rqcblten_hv(i,k)
            rqiblten(i,k,j) = rqiblten_hv(i,k)
            exch_h(i,k,j) = exch_h_hv(i,k)
            exch_m(i,k,j) = exch_m_hv(i,k)
         end do
      end do

      do i = its, ite
         u10(i,j) = u10_hv(i)
         v10(i,j) = v10_hv(i)
         hpbl(i,j) = hpbl_hv(i)
         kpbl2d(i,j) = kpbl2d_hv(i)
         wstar(i,j) = wstar_hv(i)
         delta(i,j) = delta_hv(i)
      end do
   enddo

 end subroutine ysu

!=================================================================================================================
 end module module_bl_ysu
!=================================================================================================================
