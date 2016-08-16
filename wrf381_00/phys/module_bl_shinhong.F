!WRF:model_layer:physics
!
module module_bl_shinhong
!
contains
!
!-------------------------------------------------------------------------------
!
   subroutine shinhong(u3d,v3d,th3d,t3d,qv3d,qc3d,qi3d,p3d,p3di,pi3d,          &
                  rublten,rvblten,rthblten,                                    &
                  rqvblten,rqcblten,rqiblten,flag_qi,                          &
                  cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,                    &
                  dz8w,psfc,                                                   &
                  znu,znw,mut,p_top,                                           &
                  znt,ust,hpbl,psim,psih,                                      &
                  xland,hfx,qfx,wspd,br,                                       &
                  dt,kpbl2d,                                                   &
                  exch_h,                                                      &
                  u10,v10,                                                     &
                  shinhong_tke_diag,tke_pbl,el_pbl,corf,                       &
                  dx,dy,                                                       &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte,                                   &
                !optional
                  ctopo,ctopo2,                                                &
                  wstar,delta,                                                 &
                  regime                                           )
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
!-- ep1         constant for virtual temperature (r_v/r_d - 1)
!-- ep2         constant for specific humidity calculation
!-- karman      von karman constant
!-- xlv         latent heat of vaporization (j/kg)
!-- rv          gas constant for water vapor (j/kg/k)
!-- dz8w        dz between full levels (m)
!-- psfc        pressure at the surface (pa)
!-- znu         eta values on half (mass) levels  
!-- znw         eta values on full (w) levels
!-- mut         mass in column (pa)
!-- p_top       pressure top of the model (pa)
!-- znt         roughness length (m)
!-- ust	        u* in similarity theory (m/s)
!-- hpbl        pbl height (m)
!-- psim        similarity stability function for momentum
!-- psih        similarity stability function for heat
!-- xland       land mask (1 for land, 2 for water)
!-- hfx	        upward heat flux at the surface (w/m^2)
!-- qfx	        upward moisture flux at the surface (kg/m^2/s)
!-- wspd        wind speed at lowest model level (m/s)
!-- br          bulk richardson number in surface layer
!-- u10         u-wind speed at 10 m (m/s)
!-- v10         v-wind speed at 10 m (m/s)
!-- dt          time step (s)
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
   integer,parameter ::  ndiff = 3
   real,parameter    ::  rcl = 1.0
!
   integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte
   integer,  intent(in   )   ::      shinhong_tke_diag
!
   real,     intent(in   )   ::      dt,cp,g,rovcp,rovg,rd,xlv,rv
   real,     intent(in   )   ::      ep1,ep2,karman
   real,     intent(in   )   ::      dx,dy
!
   integer,  dimension( ims:ime, jms:jme )                                   , &
             intent(out  )   ::                                        kpbl2d
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                           u3d, &
                                                                          v3d
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          qv3d, &
                                                                         qc3d, &
                                                                         qi3d, &
                                                                          p3d, &
                                                                         pi3d, &
                                                                         th3d, &
                                                                          t3d, &
                                                                         dz8w
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          p3di
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                       rublten, &
                                                                      rvblten, &
                                                                     rthblten, &
                                                                     rqvblten, &
                                                                     rqcblten
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                        exch_h
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                       tke_pbl, &
                                                                       el_pbl
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx, &
                                                                         corf, &
                                                                           br, &
                                                                         psfc
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                                &
                                                                         psim, &
                                                                         psih
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                           u10, &
                                                                          v10
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                           znt, &
                                                                          ust, &
                                                                         hpbl, &
                                                                         wspd
!
   logical,  intent(in)      ::                                       flag_qi
!
! optional
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout), optional    ::                           rqiblten
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout), optional    ::                              wstar, &
                                                                        delta
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout), optional    ::                             regime
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   ), optional    ::                                mut
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   ), optional    ::                              ctopo, &
                                                                       ctopo2
!
   real,     dimension( kms:kme )                                            , &
             intent(in   ), optional    ::                                znu, &
                                                                          znw
!
   real,     optional, intent(in   )    ::                              p_top
!
! local
!
   integer ::  i,j,k
   real,     dimension( its:ite, kts:kte*ndiff )  ::                 rqvbl2dt, &
                                                                         qv2d
   real,     dimension( its:ite, kts:kte )        ::                      pdh
   real,     dimension( its:ite, kts:kte+1 )      ::                     pdhi
   real,     dimension( its:ite )                 ::                           &
                                                                        dusfc, &
                                                                        dvsfc, &
                                                                        dtsfc, &
                                                                        dqsfc
!
   qv2d(its:ite,:) = 0.0
!
   do j = jts,jte
     if(present(mut))then
!
! For ARW we will replace p and p8w with dry hydrostatic pressure
!
       do k = kts,kte+1
         do i = its,ite
           if(k.le.kte)pdh(i,k) = mut(i,j)*znu(k) + p_top
           pdhi(i,k) = mut(i,j)*znw(k) + p_top
         enddo
       enddo
     else
       do k = kts,kte+1
         do i = its,ite
           if(k.le.kte)pdh(i,k) = p3d(i,k,j)
           pdhi(i,k) = p3di(i,k,j)
         enddo
       enddo
     endif
     do k = kts,kte
       do i = its,ite
         qv2d(i,k) = qv3d(i,k,j)
         qv2d(i,k+kte) = qc3d(i,k,j)
         if(present(rqiblten)) qv2d(i,k+kte+kte) = qi3d(i,k,j)
       enddo
     enddo
!
     call shinhong2d(J=j,ux=u3d(ims,kms,j),vx=v3d(ims,kms,j)                   &
             ,tx=t3d(ims,kms,j)                                                &
             ,qx=qv2d(its,kts)                                                 &
             ,p2d=pdh(its,kts),p2di=pdhi(its,kts)                              &
             ,pi2d=pi3d(ims,kms,j)                                             &
             ,utnp=rublten(ims,kms,j),vtnp=rvblten(ims,kms,j)                  &
             ,ttnp=rthblten(ims,kms,j),qtnp=rqvbl2dt(its,kts),ndiff=ndiff      &
             ,cp=cp,g=g,rovcp=rovcp,rd=rd,rovg=rovg                            &
             ,xlv=xlv,rv=rv                                                    &
             ,ep1=ep1,ep2=ep2,karman=karman                                    &
             ,dz8w2d=dz8w(ims,kms,j)                                           &
             ,psfcpa=psfc(ims,j),znt=znt(ims,j),ust=ust(ims,j)                 &
             ,hpbl=hpbl(ims,j)                                                 &
             ,regime=regime(ims,j),psim=psim(ims,j)                            &
             ,psih=psih(ims,j),xland=xland(ims,j)                              &
             ,hfx=hfx(ims,j),qfx=qfx(ims,j)                                    &
             ,wspd=wspd(ims,j),br=br(ims,j)                                    &
             ,dusfc=dusfc,dvsfc=dvsfc,dtsfc=dtsfc,dqsfc=dqsfc                  &
             ,dt=dt,rcl=1.0,kpbl1d=kpbl2d(ims,j)                               &
             ,exch_hx=exch_h(ims,kms,j)                                        &
             ,wstar=wstar(ims,j)                                               &
             ,delta=delta(ims,j)                                               &
             ,u10=u10(ims,j),v10=v10(ims,j)                                    &
             ,ctopo=ctopo(ims,j),ctopo2=ctopo2(ims,j)                          &
             ,shinhong_tke_diag=shinhong_tke_diag                              &
             ,tke=tke_pbl(ims,kms,j),el_pbl=el_pbl(ims,kms,j)                  &
             ,corf=corf(ims,j)                                                 &
             ,dx=dx,dy=dy                                                      &
             ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde                &
             ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme                &
             ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )
!
     do k = kts,kte
       do i = its,ite
         rthblten(i,k,j) = rthblten(i,k,j)/pi3d(i,k,j)
         rqvblten(i,k,j) = rqvbl2dt(i,k)
         rqcblten(i,k,j) = rqvbl2dt(i,k+kte)
         if(present(rqiblten)) rqiblten(i,k,j) = rqvbl2dt(i,k+kte+kte)
       enddo
     enddo
!
   enddo
!
   end subroutine shinhong
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine shinhong2d(j,ux,vx,tx,qx,p2d,p2di,pi2d,                          &
                  utnp,vtnp,ttnp,qtnp,ndiff,                                   &
                  cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,                    &
                  dz8w2d,psfcpa,                                               &
                  znt,ust,hpbl,psim,psih,                                      &
                  xland,hfx,qfx,wspd,br,                                       &
                  dusfc,dvsfc,dtsfc,dqsfc,                                     &
                  dt,rcl,kpbl1d,                                               &
                  exch_hx,                                                     &
                  wstar,delta,                                                 &
                  shinhong_tke_diag,                                           &
                  tke,el_pbl,corf,                                             &
                  u10,v10,                                                     &
                  ctopo,ctopo2,                                                &
                  dx,dy,                                                       &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte,                                   &
                !optional
                  regime )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
!     the shinhongpbl (shin and hong 2015) is based on the les study of shin
!     and hong (2013). the major ingredients of the shinhongpbl are
!       1) the prescribed nonlocal heat transport profile fit to the les and 
!       2) inclusion of explicit scale dependency functions for vertical 
!          transport in convective pbl. 
!     so, the shinhongpbl works at the gray zone resolution of convective pbl.
!     note that honnert et al. (2011) first suggested explicit scale dependency
!     function, and shin and hong (2013) further classified the function by
!     stability (u*/w*) in convective pbl and calculated the function for 
!     nonlocal and local transport separately.
!     vertical mixing in the stable boundary layer and free atmosphere follows
!     hong (2010) and hong et al. (2006), same as the ysupbl scheme.
!
!     shinhongpbl:
!     coded and implemented by hyeyum hailey shin (ncar)
!              summer 2014
!
!     ysupbl:
!     coded by song-you hong (yonsei university) and implemented by
!              song-you hong (yonsei university) and jimy dudhia (ncar)
!              summer 2002
!
!     references:
!        shin and hong (2015) mon. wea. rev.
!        shin and hong (2013) j. atmos. sci.
!        honnert, masson, and couvreux (2011) j. atmos. sci.
!        hong (2010) quart. j. roy. met. soc
!        hong, noh, and dudhia (2006), mon. wea. rev. 
!
!-------------------------------------------------------------------------------
!
   real,parameter    ::  xkzminm = 0.1,xkzminh = 0.01
   real,parameter    ::  xkzmax = 1000.,rimin = -100.
   real,parameter    ::  rlam = 30.,prmin = 0.25,prmax = 4.
   real,parameter    ::  brcr_ub = 0.0,brcr_sb = 0.25,cori = 1.e-4
   real,parameter    ::  afac = 6.8,bfac = 6.8,pfac = 2.0,pfac_q = 2.0
   real,parameter    ::  phifac = 8.,sfcfrac = 0.1
   real,parameter    ::  d1 = 0.02, d2 = 0.05, d3 = 0.001
   real,parameter    ::  h1 = 0.33333335, h2 = 0.6666667
   real,parameter    ::  ckz = 0.001,zfmin = 1.e-8,aphi5 = 5.,aphi16 = 16.
   real,parameter    ::  tmin=1.e-2
   real,parameter    ::  gamcrt = 3.,gamcrq = 2.e-3
   real,parameter    ::  xka = 2.4e-5
   integer,parameter ::  imvdif = 1
!
! tunable parameters for tke
!
   real,parameter    ::  epsq2l = 0.01,c_1 = 1.0,gamcre = 0.224
!
! tunable parameters for prescribed nonlocal transport profile
!
   real,parameter    ::  mltop = 1.0,sfcfracn1 = 0.075
   real,parameter    ::  nlfrac = 0.7,enlfrac = -0.4
   real,parameter    ::  a11 = 1.0,a12 = -1.15
   real,parameter    ::  ezfac = 1.5
   real,parameter    ::  cpent = -0.4,rigsmax = 100.
   real,parameter    ::  entfmin = 1.0, entfmax = 5.0
!
   integer,  intent(in   )   ::     ids,ide, jds,jde, kds,kde,                 &
                                    ims,ime, jms,jme, kms,kme,                 &
                                    its,ite, jts,jte, kts,kte,                 &
                                    j,ndiff
   integer,  intent(in   )   ::     shinhong_tke_diag
!
   real,     intent(in   )   ::     dt,rcl,cp,g,rovcp,rovg,rd,xlv,rv
   real,     intent(in   )   ::     ep1,ep2,karman
   real,     intent(in   )   ::     dx,dy
!
   integer,  dimension( ims:ime )                                            , &
             intent(out  )   ::                                        kpbl1d
!
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(in   )   ::                                        dz8w2d, &
                                                                         pi2d
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(in   )   ::                                            ux, &
                                                                           vx
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(in   )   ::                                            tx
!
   real,     dimension( its:ite, kts:kte*ndiff )                             , &
             intent(in   )   ::                                            qx
   real,     dimension( its:ite, kts:kte+1 )                                 , &
             intent(in   )   ::                                          p2di
   real,     dimension( its:ite, kts:kte )                                   , &
             intent(in   )   ::                                           p2d
!
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(inout)   ::                                          utnp, &
                                                                         vtnp, &
                                                                         ttnp
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(inout)   ::                                       exch_hx
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(inout)   ::                                           tke, &
                                                                       el_pbl
   real,     dimension( its:ite, kts:kte*ndiff )                             , &
             intent(inout)   ::                                          qtnp
!
   real,     dimension( ims:ime )                                            , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx
   real,     dimension( ims:ime )                                            , &
             intent(in   )   ::                                            br, &
                                                                         psim, &
                                                                         psih, &
                                                                       psfcpa
   real,     dimension( ims:ime )                                            , &
             intent(in   )   ::                                          corf
!
   real,     dimension( ims:ime )                                            , &
             intent(inout)   ::                                           ust, &
                                                                         hpbl, &
                                                                          znt
   real,     dimension( ims:ime )                                            , &
             intent(inout)   ::                                          wspd
   real,     dimension( ims:ime )                                            , &
             intent(inout)    ::                                          u10, &
                                                                          v10
!
   real,     dimension( ims:ime )                                            , &
             optional                                                        , &
             intent(in   )   ::                                         ctopo, &
                                                                       ctopo2
   real,     dimension( ims:ime )                                            , &
             optional                                                        , &
             intent(inout)   ::                                        regime
   real,     dimension( its:ite )                                            , &
             intent(out  )   ::                                         wstar, &
                                                                        delta
!
! local vars
!
   integer ::  n,i,k,l,ic,is,nwmass
   integer ::  klpbl, kqc, kqi
   integer ::  lmh,lmxl
!
   real    ::  dt2,rdt,spdk2,fm,fh,hol1,gamfac,vpert,prnum,prnum0
   real    ::  ss,ri,qmean,tmean,alpha,chi,zk,rl2,dk,sri
   real    ::  brint,dtodsd,dtodsu,rdz,dsdzt,dsdzq,dsdz2,rlamdz
   real    ::  utend,vtend,ttend,qtend
   real    ::  dtstep,govrthv
   real    ::  cont, conq, conw, conwrc
   real    ::  delxy,pu1,pth1,pq1
   real    ::  dex,hgame_c
   real    ::  zfacdx
   real    ::  amf1,amf2,bmf2,amf3,bmf3,amf4,bmf4,sflux0,snlflux0
   real    ::  mlfrac,ezfrac,sfcfracn
   real    ::  uwst,uwstx,csfac
   real    ::  prnumfac,bfx0,hfx0,qfx0,delb,dux,dvx,                           &
               dsdzu,dsdzv,wm3,dthx,dqx,wspd10,ross,tem1,dsig,tvcon,conpr,     &
               prfac,prfac2,phim8z
!
   integer,  dimension( its:ite )            ::                          kpbl
   real,     dimension( its:ite )            ::                           hol
   real,     dimension( its:ite )            ::                       deltaoh
   real,     dimension( its:ite )            ::                          rigs, &
                                                                     enlfrac2, &
                                                                        cslen
   real,     dimension( its:ite )            ::                                &
                                                                         rhox, &
                                                                       govrth, &
                                                                  zl1,thermal, &
                                                                       wscale, &
                                                                  hgamt,hgamq, &
                                                                    brdn,brup, &
                                                                    phim,phih, &
                                                                  dusfc,dvsfc, &
                                                                  dtsfc,dqsfc, &
                                                                        prpbl, &
                                                                        wspd1
   real,     dimension( its:ite )            ::                                &
                                                                         ust3, &
                                                                       wstar3, &
                                                                  hgamu,hgamv, &
                                                                      wm2, we, &
                                                                       bfxpbl, &
                                                                hfxpbl,qfxpbl, &
                                                                ufxpbl,vfxpbl, &
                                                                        dthvx
   real,     dimension( its:ite )            ::                                &
                                                                         brcr, &
                                                                        sflux, &
                                                                         zol1, &
                                                                    brcr_sbro
   real,     dimension( its:ite )            ::                                &
                                                                       efxpbl, &
                                                                     hpbl_cbl, &
                                                                       epshol, &
                                                                           ct
!
   real,     dimension( its:ite, kts:kte )   ::                     xkzm,xkzh, &
                                                                        f1,f2, &
                                                                        r1,r2, &
                                                                        ad,au, &
                                                                           cu, &
                                                                           al, &
                                                                         xkzq, &
                                                                         zfac
   real,     dimension( its:ite, kts:kte )   ::                                &
                                                                     thx,thvx, &
                                                                          del, &
                                                                          dza, &
                                                                          dzq, &
                                                                        xkzom, &
                                                                        xkzoh, &
                                                                           za
   real,     dimension( its:ite, kts:kte )   ::                                &
                                                                      wscalek
   real,     dimension( its:ite, kts:kte )   ::                                &
                                                                  xkzml,xkzhl, &
                                                               zfacent,entfac
   real,     dimension( its:ite, kts:kte )   ::                                &
                                                                           mf, &
                                                                       zfacmf, &
                                                                     entfacmf
   real,     dimension( its:ite, kts:kte )   ::                                &
                                                                          q2x, &
                                                                      hgame2d, &
                                                                      tflux_e, &
                                                                      qflux_e, &
                                                                     tvflux_e
   real,     dimension( its:ite, kts:kte+1 ) ::                            zq
   real,     dimension( its:ite, kts:kte, ndiff ) ::                    r3,f3
!
   real,     dimension( kts:kte )            ::                                &
                                                                      uxk,vxk, &
                                                               txk,thxk,thvxk, &
                                                                         q2xk, &
                                                                        hgame
   real,     dimension( kts:kte )            ::                                &
                                                         ps1d,pb1d,eps1d,pt1d, &
                                                    xkze1d,eflx_l1d,eflx_nl1d, &
                                                                        ptke1
   real,     dimension( kts+1:kte )          ::                                &
                                                                 s2,gh,rig,el, &
                                                                    akmk,akhk, &
                                                  mfk,ufxpblk,vfxpblk,qfxpblk
   real,     dimension( kts:kte+1 )          ::                           zqk
   real,     dimension( kts:kte*ndiff )      ::                           qxk
!
   logical,  dimension( its:ite )            ::                        pblflg, &
                                                                       sfcflg, &
                                                                       stable
   logical,  dimension( ndiff )              ::                        ifvmix
!
!-------------------------------------------------------------------------------
!
   klpbl = kte
   lmh = 1
   lmxl = 1
!
   cont=cp/g
   conq=xlv/g
   conw=1./g
   conwrc = conw*sqrt(rcl)
   conpr = bfac*karman*sfcfrac
!
!  k-start index for cloud and rain 
!
   kqc = 1 + kte
   kqi = 1 + kte*2
   nwmass = 3
   ifvmix(:) = .true.
!
   do k = kts,kte
     do i = its,ite
       thx(i,k) = tx(i,k)/pi2d(i,k)
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       tvcon = (1.+ep1*qx(i,k))
       thvx(i,k) = thx(i,k)*tvcon
     enddo
   enddo
!
   do i = its,ite
     tvcon = (1.+ep1*qx(i,1))
     rhox(i) = psfcpa(i)/(rd*tx(i,1)*tvcon)
     govrth(i) = g/thx(i,1)
   enddo
!
!-----compute the height of full- and half-sigma levels above ground
!     level, and the layer thicknesses.
!
   do i = its,ite
     zq(i,1) = 0.
   enddo
!
   do k = kts,kte
     do i = its,ite
       zq(i,k+1) = dz8w2d(i,k)+zq(i,k)
     enddo
   enddo
! 
   do k = kts,kte
     do i = its,ite
       za(i,k) = 0.5*(zq(i,k)+zq(i,k+1))
       dzq(i,k) = zq(i,k+1)-zq(i,k)
       del(i,k) = p2di(i,k)-p2di(i,k+1)
     enddo
   enddo
!
   do i = its,ite
     dza(i,1) = za(i,1)
   enddo
!
   do k = kts+1,kte
     do i = its,ite
       dza(i,k) = za(i,k)-za(i,k-1)
     enddo
   enddo
!
!
!-----initialize vertical tendencies and
!
   utnp(its:ite,:) = 0.
   vtnp(its:ite,:) = 0.
   ttnp(its:ite,:) = 0.
   qtnp(its:ite,:) = 0.
!
   do i = its,ite
     wspd1(i) = sqrt(ux(i,1)*ux(i,1)+vx(i,1)*vx(i,1))+1.e-9
   enddo
!
!---- compute vertical diffusion
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     compute preliminary variables
!
   dtstep = dt
   dt2 = 2.*dtstep
   rdt = 1./dt2
!
   do i = its,ite
     bfxpbl(i) = 0.0
     hfxpbl(i) = 0.0
     qfxpbl(i) = 0.0
     ufxpbl(i) = 0.0
     vfxpbl(i) = 0.0
     hgamu(i)  = 0.0
     hgamv(i)  = 0.0
     delta(i)  = 0.0
   enddo
!
   do i = its,ite
     efxpbl(i)   = 0.0
     hpbl_cbl(i) = 0.0
     epshol(i)   = 0.0
     ct(i)       = 0.0
   enddo
!
   do i = its,ite
     deltaoh(i)  = 0.0
     rigs(i)     = 0.0
     enlfrac2(i) = 0.0
     cslen(i)    = 0.0
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       wscalek(i,k) = 0.0
     enddo
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       zfac(i,k) = 0.0
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       q2x(i,k) = 2.*tke(i,k)
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       el_pbl(i,k)   = 0.0
       hgame2d(i,k)  = 0.0
       tflux_e(i,k)  = 0.0
       qflux_e(i,k)  = 0.0
       tvflux_e(i,k) = 0.0
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       mf(i,k)     = 0.0
       zfacmf(i,k) = 0.0
     enddo
   enddo
!
   do k = kts,klpbl-1
     do i = its,ite
       xkzom(i,k) = xkzminm
       xkzoh(i,k) = xkzminh
     enddo
   enddo
!
   do i = its,ite
     dusfc(i) = 0.
     dvsfc(i) = 0.
     dtsfc(i) = 0.
     dqsfc(i) = 0.
   enddo
!
   do i = its,ite
     hgamt(i)  = 0.
     hgamq(i)  = 0.
     wscale(i) = 0.
     kpbl(i)   = 1
     hpbl(i)   = zq(i,1)
     hpbl_cbl(i) = zq(i,1)
     zl1(i)    = za(i,1)
     thermal(i)= thvx(i,1)
     pblflg(i) = .true.
     sfcflg(i) = .true.
     sflux(i) = hfx(i)/rhox(i)/cp + qfx(i)/rhox(i)*ep1*thx(i,1)
     if(br(i).gt.0.0) sfcflg(i) = .false.
   enddo
!
!     compute the first guess of pbl height
!
   do i = its,ite
     stable(i) = .false.
     brup(i) = br(i)
     brcr(i) = brcr_ub
   enddo
!
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo
!
   do i = its,ite
     k = kpbl(i)
     if(brdn(i).ge.brcr(i))then
       brint = 0.
     elseif(brup(i).le.brcr(i))then
       brint = 1.
     else
       brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
     endif
     hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
     if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
     if(kpbl(i).le.1) pblflg(i) = .false.
   enddo
!
   do i = its,ite
     fm = psim(i)
     fh = psih(i)
     zol1(i) = max(br(i)*fm*fm/fh,rimin)
     if(sfcflg(i))then
       zol1(i) = min(zol1(i),-zfmin)
     else
       zol1(i) = max(zol1(i),zfmin)
     endif
     hol1 = zol1(i)*hpbl(i)/zl1(i)*sfcfrac
     epshol(i) = hol1
     if(sfcflg(i))then
       phim(i) = (1.-aphi16*hol1)**(-1./4.)
       phih(i) = (1.-aphi16*hol1)**(-1./2.)
       bfx0  = max(sflux(i),0.)
       hfx0 = max(hfx(i)/rhox(i)/cp,0.)
       qfx0 = max(ep1*thx(i,1)*qfx(i)/rhox(i),0.)
       wstar3(i) = (govrth(i)*bfx0*hpbl(i))
       wstar(i) = (wstar3(i))**h1
     else
       phim(i) = (1.+aphi5*hol1)
       phih(i) = phim(i)
       wstar(i)  = 0.
       wstar3(i) = 0.
     endif
     ust3(i)   = ust(i)**3.
     wscale(i) = (ust3(i)+phifac*karman*wstar3(i)*0.5)**h1
     wscale(i) = min(wscale(i),ust(i)*aphi16)
     wscale(i) = max(wscale(i),ust(i)/aphi5)
   enddo
!
!     compute the surface variables for pbl height estimation
!     under unstable conditions
!
   do i = its,ite
     if(sfcflg(i).and.sflux(i).gt.0.0)then
       gamfac   = bfac/rhox(i)/wscale(i)
       hgamt(i) = min(gamfac*hfx(i)/cp,gamcrt)
       hgamq(i) = min(gamfac*qfx(i),gamcrq)
       vpert = (hgamt(i)+ep1*thx(i,1)*hgamq(i))/bfac*afac
       thermal(i) = thermal(i)+max(vpert,0.)*min(za(i,1)/(sfcfrac*hpbl(i)),1.0)
       hgamt(i) = max(hgamt(i),0.0)
       hgamq(i) = max(hgamq(i),0.0)
       brint    = -15.9*ust(i)*ust(i)/wspd(i)*wstar3(i)/(wscale(i)**4.)
       hgamu(i) = brint*ux(i,1)
       hgamv(i) = brint*vx(i,1)
     else
       pblflg(i) = .false.
     endif
   enddo
!
!     enhance the pbl height by considering the thermal
!
   do i = its,ite
     if(pblflg(i))then
       kpbl(i) = 1
       hpbl(i) = zq(i,1)
     endif
   enddo
!
   do i = its,ite
     if(pblflg(i))then
       stable(i) = .false.
       brup(i) = br(i)
       brcr(i) = brcr_ub
     endif
   enddo
!
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i).and.pblflg(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo
!
   do i = its,ite
     if(pblflg(i)) then
       k = kpbl(i)
       if(brdn(i).ge.brcr(i))then
         brint = 0.
       elseif(brup(i).le.brcr(i))then
         brint = 1.
       else
         brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
       endif
       hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
       if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
       if(kpbl(i).le.1) pblflg(i) = .false.
       uwst  = abs(ust(i)/wstar(i)-0.5)
       uwstx = -80.*uwst+14.
       csfac = 0.5*(tanh(uwstx)+3.)
       cslen(i) = csfac*hpbl(i)
     endif
   enddo
!
!     stable boundary layer
!
   do i = its,ite
     hpbl_cbl(i) = hpbl(i)
     if((.not.sfcflg(i)).and.hpbl(i).lt.zq(i,2)) then
       brup(i) = br(i)
       stable(i) = .false.
     else
       stable(i) = .true.
     endif
   enddo
!
   do i = its,ite
     if((.not.stable(i)).and.((xland(i)-1.5).ge.0))then
       wspd10 = u10(i)*u10(i) + v10(i)*v10(i)
       wspd10 = sqrt(wspd10)
       ross = wspd10 / (cori*znt(i))
       brcr_sbro(i) = min(0.16*(1.e-7*ross)**(-0.18),.3)
     endif
   enddo
!
   do i = its,ite
     if(.not.stable(i))then
       if((xland(i)-1.5).ge.0)then
         brcr(i) = brcr_sbro(i)
       else
         brcr(i) = brcr_sb
       endif
     endif
   enddo
!
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo
!
   do i = its,ite
     if((.not.sfcflg(i)).and.hpbl(i).lt.zq(i,2)) then
       k = kpbl(i)
       if(brdn(i).ge.brcr(i))then
         brint = 0.
       elseif(brup(i).le.brcr(i))then
         brint = 1.
       else
         brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
       endif
       hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
       if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
       if(kpbl(i).le.1) pblflg(i) = .false.
     endif
   enddo
!
!     scale dependency for nonlocal momentum and moisture transport
!
   delxy=sqrt(dx*dy)
!
   do i = its,ite
     pu1=pu(delxy,cslen(i))
     pq1=pq(delxy,cslen(i))
     if(pblflg(i)) then
       hgamu(i) = hgamu(i)*pu1
       hgamv(i) = hgamv(i)*pu1
       hgamq(i) = hgamq(i)*pq1
     endif
   enddo
!
!     estimate the entrainment parameters
!
   delxy=sqrt(dx*dy)
!
   do i = its,ite
     if(pblflg(i)) then
       k = kpbl(i) - 1
       prpbl(i) = 1.0
       wm3       = wstar3(i) + 5. * ust3(i)
       wm2(i)    = wm3**h2
       bfxpbl(i) = -0.15*thvx(i,1)/g*wm3/hpbl(i)
       dthvx(i)  = max(thvx(i,k+1)-thvx(i,k),tmin)
       dthx  = max(thx(i,k+1)-thx(i,k),tmin)
       dqx   = min(qx(i,k+1)-qx(i,k),0.0)
       we(i) = max(bfxpbl(i)/dthvx(i),-sqrt(wm2(i)))
       hfxpbl(i) = we(i)*dthx
       pq1=pq(delxy,cslen(i))
       qfxpbl(i) = we(i)*dqx*pq1
!
       pu1=pu(delxy,cslen(i))
       dux = ux(i,k+1)-ux(i,k)
       dvx = vx(i,k+1)-vx(i,k)
       if(dux.gt.tmin) then
         ufxpbl(i) = max(prpbl(i)*we(i)*dux*pu1,-ust(i)*ust(i))
       elseif(dux.lt.-tmin) then
         ufxpbl(i) = min(prpbl(i)*we(i)*dux*pu1,ust(i)*ust(i))
       else
         ufxpbl(i) = 0.0
       endif
       if(dvx.gt.tmin) then
         vfxpbl(i) = max(prpbl(i)*we(i)*dvx*pu1,-ust(i)*ust(i))
       elseif(dvx.lt.-tmin) then
         vfxpbl(i) = min(prpbl(i)*we(i)*dvx*pu1,ust(i)*ust(i))
       else
         vfxpbl(i) = 0.0
       endif
       delb  = govrth(i)*d3*hpbl(i)
       delta(i) = min(d1*hpbl(i) + d2*wm2(i)/delb,100.)
       delb  = govrth(i)*dthvx(i)
       deltaoh(i) = d1*hpbl(i) + d2*wm2(i)/delb
       deltaoh(i) = max(ezfac*deltaoh(i),hpbl(i)-za(i,kpbl(i)-1)-1.)
       deltaoh(i) = min(deltaoh(i)      ,hpbl(i))
       rigs(i)     = govrth(i)*dthvx(i)*deltaoh(i)/(dux**2.+dvx**2.)
       rigs(i)     = max(min(rigs(i), rigsmax),rimin)
       enlfrac2(i) = max(min(wm3/wstar3(i)/(1.+cpent/rigs(i)),entfmax), entfmin)
       enlfrac2(i) = enlfrac2(i)*enlfrac
     endif
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       if(pblflg(i))then
         entfacmf(i,k) = sqrt(((zq(i,k+1)-hpbl(i))/deltaoh(i))**2.)
       endif
       if(pblflg(i).and.k.ge.kpbl(i))then
         entfac(i,k) = ((zq(i,k+1)-hpbl(i))/deltaoh(i))**2.
       else
         entfac(i,k) = 1.e30
       endif
     enddo
   enddo
!
!     compute diffusion coefficients below pbl
!
   do k = kts,klpbl
     do i = its,ite
       if(k.lt.kpbl(i)) then
         zfac(i,k) = min(max((1.-(zq(i,k+1)-zl1(i))/(hpbl(i)-zl1(i))),zfmin),1.)
         zfacent(i,k) = (1.-zfac(i,k))**3.
         wscalek(i,k) = (ust3(i)+phifac*karman*wstar3(i)*(1.-zfac(i,k)))**h1
         if(sfcflg(i)) then 
           prfac = conpr
           prfac2 = 15.9*wstar3(i)/ust3(i)/(1.+4.*karman*wstar3(i)/ust3(i))
           prnumfac = -3.*(max(zq(i,k+1)-sfcfrac*hpbl(i),0.))**2./hpbl(i)**2.
         else
           prfac = 0.
           prfac2 = 0.
           prnumfac = 0.
           phim8z = 1.+aphi5*zol1(i)*zq(i,k+1)/zl1(i)
           wscalek(i,k) = ust(i)/phim8z
           wscalek(i,k) = max(wscalek(i,k),0.001)
         endif
         prnum0 = (phih(i)/phim(i)+prfac)
         prnum0 = max(min(prnum0,prmax),prmin)
         xkzm(i,k) = wscalek(i,k)*karman*zq(i,k+1)*zfac(i,k)**pfac
         prnum =  1. + (prnum0-1.)*exp(prnumfac)
         xkzq(i,k) = xkzm(i,k)/prnum*zfac(i,k)**(pfac_q-pfac)
         prnum0 = prnum0/(1.+prfac2*karman*sfcfrac)
         prnum =  1. + (prnum0-1.)*exp(prnumfac)
         xkzh(i,k) = xkzm(i,k)/prnum
         xkzm(i,k) = xkzm(i,k)+xkzom(i,k)
         xkzh(i,k) = xkzh(i,k)+xkzoh(i,k)
         xkzq(i,k) = xkzq(i,k)+xkzoh(i,k)
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         xkzq(i,k) = min(xkzq(i,k),xkzmax)
       endif
     enddo
   enddo
!
!     compute diffusion coefficients over pbl (free atmosphere)
!
   do k = kts,kte-1
     do i = its,ite
       if(k.ge.kpbl(i)) then
         ss = ((ux(i,k+1)-ux(i,k))*(ux(i,k+1)-ux(i,k))                         &
              +(vx(i,k+1)-vx(i,k))*(vx(i,k+1)-vx(i,k)))                        &
              /(dza(i,k+1)*dza(i,k+1))+1.e-9
         govrthv = g/(0.5*(thvx(i,k+1)+thvx(i,k)))
         ri = govrthv*(thvx(i,k+1)-thvx(i,k))/(ss*dza(i,k+1))
!      in cloud
         if(imvdif.eq.1.and.nwmass.ge.3)then
           if((qx(i,kqc+k-1)+qx(i,kqi+k-1)).gt.0.01e-3                         &
             .and.(qx(i,kqc+k)+qx(i,kqi+k)).gt.0.01e-3) then
             qmean = 0.5*(qx(i,k)+qx(i,k+1))
             tmean = 0.5*(tx(i,k)+tx(i,k+1))
             alpha = xlv*qmean/rd/tmean
             chi   = xlv*xlv*qmean/cp/rv/tmean/tmean
             ri    = (1.+alpha)*(ri-g*g/ss/tmean/cp*((chi-alpha)/(1.+chi)))
           endif
         endif
         zk = karman*zq(i,k+1)
         rlamdz = min(max(0.1*dza(i,k+1),rlam),300.)
         rlamdz = min(dza(i,k+1),rlamdz)
         rl2 = (zk*rlamdz/(rlamdz+zk))**2
         dk = rl2*sqrt(ss)
         if(ri.lt.0.)then
! unstable regime
           ri = max(ri, rimin)
           sri = sqrt(-ri)
           xkzm(i,k) = dk*(1+8.*(-ri)/(1+1.746*sri))
           xkzh(i,k) = dk*(1+8.*(-ri)/(1+1.286*sri))
         else
! stable regime
           xkzh(i,k) = dk/(1+5.*ri)**2
           prnum = 1.0+2.1*ri
           prnum = min(prnum,prmax)
           xkzm(i,k) = xkzh(i,k)*prnum
         endif
!
         xkzm(i,k) = xkzm(i,k)+xkzom(i,k)
         xkzh(i,k) = xkzh(i,k)+xkzoh(i,k)
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         xkzml(i,k) = xkzm(i,k)
         xkzhl(i,k) = xkzh(i,k)
       endif
     enddo
   enddo
!
!     prescribe nonlocal heat transport below pbl
!
   do i = its,ite
     deltaoh(i) = deltaoh(i)/hpbl(i)
   enddo
!
   delxy=sqrt(dx*dy)
   do i = its,ite
     mlfrac      = mltop-deltaoh(i)
     ezfrac      = mltop+deltaoh(i)
     zfacmf(i,1) = min(max((zq(i,2)/hpbl(i)),zfmin),1.)
     sfcfracn    = max(sfcfracn1,zfacmf(i,1))
!
     sflux0      = (a11+a12*sfcfracn)*sflux(i)
     snlflux0    = nlfrac*sflux0
     amf1        = snlflux0/sfcfracn
     amf2        = -snlflux0/(mlfrac-sfcfracn)
     bmf2        = -mlfrac*amf2
     amf3        = snlflux0*enlfrac2(i)/deltaoh(i)
     bmf3        = -amf3*mlfrac
     hfxpbl(i)   = amf3+bmf3
     pth1=pthnl(delxy,cslen(i))
     hfxpbl(i)   = hfxpbl(i)*pth1
!
     do k = kts,klpbl
       zfacmf(i,k) = max((zq(i,k+1)/hpbl(i)),zfmin)
       if(pblflg(i).and.k.lt.kpbl(i)) then
         if(zfacmf(i,k).le.sfcfracn) then
           mf(i,k) = amf1*zfacmf(i,k)
         else if (zfacmf(i,k).le.mlfrac) then
           mf(i,k) = amf2*zfacmf(i,k)+bmf2
         endif
         mf(i,k) = mf(i,k)+hfxpbl(i)*exp(-entfacmf(i,k))
         mf(i,k) = mf(i,k)*pth1
       endif
     enddo
   enddo
!
!     compute tridiagonal matrix elements for heat
!
   do k = kts,kte
     do i = its,ite
       au(i,k) = 0.
       al(i,k) = 0.
       ad(i,k) = 0.
       f1(i,k) = 0.
     enddo
   enddo
!
   do i = its,ite
     ad(i,1) = 1.
     f1(i,1) = thx(i,1)-300.+hfx(i)/cont/del(i,1)*dt2
   enddo
!
   delxy=sqrt(dx*dy)
   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k)
       dtodsu = dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzh(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i)) then
         dsdzt = tem1*(-mf(i,k)/xkzh(i,k))
         f1(i,k)   = f1(i,k)+dtodsd*dsdzt
         f1(i,k+1) = thx(i,k+1)-300.-dtodsu*dsdzt
       elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
         xkzh(i,k) = -we(i)*dza(i,kpbl(i))*exp(-entfac(i,k))
         xkzh(i,k) = sqrt(xkzh(i,k)*xkzhl(i,k))
         xkzh(i,k) = max(xkzh(i,k),xkzoh(i,k))
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         f1(i,k+1) = thx(i,k+1)-300.
       else
         f1(i,k+1) = thx(i,k+1)-300.
       endif
       tem1   = dsig*xkzh(i,k)*rdz
       dsdz2     = tem1*rdz
       au(i,k)   = -dtodsd*dsdz2
       al(i,k)   = -dtodsu*dsdz2
!
!     scale dependency for local heat transport
!
       zfacdx=0.2*hpbl(i)/zq(i,k+1)
       delxy=sqrt(dx*dy)*max(zfacdx,1.0)
       pth1=pthl(delxy,hpbl(i))
       if(pblflg(i).and.k.lt.kpbl(i)) then
         au(i,k) = au(i,k)*pth1
         al(i,k) = al(i,k)*pth1
       endif
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
       exch_hx(i,k+1) = xkzh(i,k)
     enddo
   enddo
!
! copies here to avoid duplicate input args for tridin
!
   do k = kts,kte
     do i = its,ite
       cu(i,k) = au(i,k)
       r1(i,k) = f1(i,k)
     enddo
   enddo
!
   call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1)
!
!     recover tendencies of heat
!
   do k = kte,kts,-1
     do i = its,ite
       ttend = (f1(i,k)-thx(i,k)+300.)*rdt*pi2d(i,k)
       ttnp(i,k) = ttnp(i,k)+ttend
       dtsfc(i) = dtsfc(i)+ttend*cont*del(i,k)/pi2d(i,k)
       if(k.eq.kte) then
         tflux_e(i,k) = ttend*dz8w2d(i,k)
       else
         tflux_e(i,k) = tflux_e(i,k+1) + ttend*dz8w2d(i,k)
       endif
     enddo
   enddo
!
!     compute tridiagonal matrix elements for moisture, clouds, and gases
!
   do k = kts,kte
     do i = its,ite
       au(i,k) = 0.
       al(i,k) = 0.
       ad(i,k) = 0.
     enddo
   enddo
!
   do ic = 1,ndiff
     do i = its,ite
       do k = kts,kte
         f3(i,k,ic) = 0.
       enddo
     enddo
   enddo
!
   do i = its,ite
     ad(i,1) = 1.
     f3(i,1,1) = qx(i,1)+qfx(i)*g/del(i,1)*dt2
   enddo
!
   if(ndiff.ge.2) then
     do ic = 2,ndiff
       is = (ic-1) * kte
       do i = its,ite
         f3(i,1,ic) = qx(i,1+is)
       enddo
     enddo
   endif
!
   do k = kts,kte-1
     do i = its,ite
       if(k.ge.kpbl(i)) then
         xkzq(i,k) = xkzh(i,k)
       endif
     enddo
   enddo
!
   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k)
       dtodsu = dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzq(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i)) then
         dsdzq = tem1*(-qfxpbl(i)*zfacent(i,k)/xkzq(i,k))
         f3(i,k,1) = f3(i,k,1)+dtodsd*dsdzq
         f3(i,k+1,1) = qx(i,k+1)-dtodsu*dsdzq
       elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
         xkzq(i,k) = -we(i)*dza(i,kpbl(i))*exp(-entfac(i,k))
         xkzq(i,k) = sqrt(xkzq(i,k)*xkzhl(i,k))
         xkzq(i,k) = max(xkzq(i,k),xkzoh(i,k))
         xkzq(i,k) = min(xkzq(i,k),xkzmax)
         f3(i,k+1,1) = qx(i,k+1)
       else
         f3(i,k+1,1) = qx(i,k+1)
       endif
       tem1   = dsig*xkzq(i,k)*rdz
       dsdz2     = tem1*rdz
       au(i,k)   = -dtodsd*dsdz2
       al(i,k)   = -dtodsu*dsdz2
!
!     scale dependency for local moisture transport
!
       zfacdx=0.2*hpbl(i)/zq(i,k+1)
       delxy=sqrt(dx*dy)*max(zfacdx,1.0)
       pq1=pq(delxy,hpbl(i))
       if(pblflg(i).and.k.lt.kpbl(i)) then
         au(i,k) = au(i,k)*pq1
         al(i,k) = al(i,k)*pq1
       endif
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
!      exch_hx(i,k+1) = xkzh(i,k)
     enddo
   enddo
!
   if(ndiff.ge.2) then
     do ic = 2,ndiff
       is = (ic-1) * kte
       do k = kts,kte-1
         do i = its,ite
           f3(i,k+1,ic) = qx(i,k+1+is)
         enddo
       enddo
     enddo
   endif
!
! copies here to avoid duplicate input args for tridin
!
   do k = kts,kte
     do i = its,ite
       cu(i,k) = au(i,k)
     enddo
   enddo
!
   do ic = 1,ndiff
     do k = kts,kte
       do i = its,ite
         r3(i,k,ic) = f3(i,k,ic)
       enddo
     enddo
   enddo
!
!     solve tridiagonal problem for moisture, clouds, and gases
!
   call tridin_ysu(al,ad,cu,r3,au,f3,its,ite,kts,kte,ndiff)
!
!     recover tendencies of heat and moisture
!
   do k = kte,kts,-1
     do i = its,ite
       qtend = (f3(i,k,1)-qx(i,k))*rdt
       qtnp(i,k) = qtnp(i,k)+qtend
       dqsfc(i) = dqsfc(i)+qtend*conq*del(i,k)
       if(k.eq.kte) then
         qflux_e(i,k) = qtend*dz8w2d(i,k)
       else
         qflux_e(i,k) = qflux_e(i,k+1) + qtend*dz8w2d(i,k)
       endif
       tvflux_e(i,k) = tflux_e(i,k) + qflux_e(i,k)*ep1*thx(i,k)
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       if(pblflg(i).and.k.lt.kpbl(i)) then
         hgame_c=c_1*0.2*2.5*(g/thvx(i,k))*wstar(i)/(0.25*(q2x(i,k+1)+q2x(i,k)))
         hgame_c=min(hgame_c,gamcre)
         if(k.eq.kte)then
           hgame2d(i,k)=hgame_c*0.5*tvflux_e(i,k)*hpbl(i)
           hgame2d(i,k)=max(hgame2d(i,k),0.0)
         else
           hgame2d(i,k)=hgame_c*0.5*(tvflux_e(i,k)+tvflux_e(i,k+1))*hpbl(i)
           hgame2d(i,k)=max(hgame2d(i,k),0.0)
         endif
       endif
     enddo
   enddo
!
   if(ndiff.ge.2) then
     do ic = 2,ndiff
       if(ifvmix(ic)) then
         is = (ic-1) * kte
         do k = kte,kts,-1
           do i = its,ite
             qtend = (f3(i,k,ic)-qx(i,k+is))*rdt
             qtnp(i,k+is) = qtnp(i,k+is)+qtend
           enddo
         enddo
       endif
     enddo
   endif
!
!     compute tridiagonal matrix elements for momentum
!
   do i = its,ite
     do k = kts,kte
       au(i,k) = 0.
       al(i,k) = 0.
       ad(i,k) = 0.
       f1(i,k) = 0.
       f2(i,k) = 0.
     enddo
   enddo
!
   do i = its,ite
! paj: ctopo=1 if topo_wind=0 (default)
! mchen add this line to make sure NMM can still work with YSU PBL
     ad(i,1) = 1.+ctopo(i)*ust(i)**2/wspd1(i)*rhox(i)*g/del(i,1)*dt2           &
          *(wspd1(i)/wspd(i))**2
     f1(i,1) = ux(i,1)
     f2(i,1) = vx(i,1)
   enddo
!
   delxy=sqrt(dx*dy)
   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k)
       dtodsu = dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzm(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i))then
         dsdzu     = tem1*(-hgamu(i)/hpbl(i)-ufxpbl(i)*zfacent(i,k)/xkzm(i,k))
         dsdzv     = tem1*(-hgamv(i)/hpbl(i)-vfxpbl(i)*zfacent(i,k)/xkzm(i,k))
         f1(i,k)   = f1(i,k)+dtodsd*dsdzu
         f1(i,k+1) = ux(i,k+1)-dtodsu*dsdzu
         f2(i,k)   = f2(i,k)+dtodsd*dsdzv
         f2(i,k+1) = vx(i,k+1)-dtodsu*dsdzv
       elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
         xkzm(i,k) = prpbl(i)*xkzh(i,k)
         xkzm(i,k) = sqrt(xkzm(i,k)*xkzml(i,k))
         xkzm(i,k) = max(xkzm(i,k),xkzom(i,k))
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         f1(i,k+1) = ux(i,k+1)
         f2(i,k+1) = vx(i,k+1)
       else
         f1(i,k+1) = ux(i,k+1)
         f2(i,k+1) = vx(i,k+1)
       endif
       tem1   = dsig*xkzm(i,k)*rdz
       dsdz2     = tem1*rdz
       au(i,k)   = -dtodsd*dsdz2
       al(i,k)   = -dtodsu*dsdz2
!
!     scale dependency for local momentum transport
!
       zfacdx=0.2*hpbl(i)/zq(i,k+1)
       delxy=sqrt(dx*dy)*max(zfacdx,1.0)
       pu1=pu(delxy,hpbl(i))
       if(pblflg(i).and.k.lt.kpbl(i)) then
         au(i,k) = au(i,k)*pu1
         al(i,k) = al(i,k)*pu1
       endif
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
     enddo
   enddo
!
! copies here to avoid duplicate input args for tridin
!
   do k = kts,kte
     do i = its,ite
       cu(i,k) = au(i,k)
       r1(i,k) = f1(i,k)
       r2(i,k) = f2(i,k)
     enddo
   enddo
!
!     solve tridiagonal problem for momentum
!
   call tridi1n(al,ad,cu,r1,r2,au,f1,f2,its,ite,kts,kte,1)
!
!     recover tendencies of momentum
!
   do k = kte,kts,-1
     do i = its,ite
       utend = (f1(i,k)-ux(i,k))*rdt
       vtend = (f2(i,k)-vx(i,k))*rdt
       utnp(i,k) = utnp(i,k)+utend
       vtnp(i,k) = vtnp(i,k)+vtend
       dusfc(i) = dusfc(i) + utend*conwrc*del(i,k)
       dvsfc(i) = dvsfc(i) + vtend*conwrc*del(i,k)
     enddo
   enddo
!
   do i = its,ite
     kpbl1d(i) = kpbl(i)
   enddo
!
! paj: ctopo2=1 if topo_wind=0 (default)
!
   do i = its,ite
     u10(i) = ctopo2(i)*u10(i)+(1-ctopo2(i))*ux(i,1)
     v10(i) = ctopo2(i)*v10(i)+(1-ctopo2(i))*vx(i,1)
   enddo
!
!---- calculate sgs tke which is consistent with shinhongpbl algorithm
!
   if (shinhong_tke_diag.eq.1) then
!
   tke_calculation: do i = its,ite
     do k = kts+1,kte
       s2(k)   = 0.0
       gh(k)   = 0.0
       rig(k)  = 0.0
       el(k)   = 0.0
       akmk(k) = 0.0
       akhk(k) = 0.0
       mfk(k)      = 0.0
       ufxpblk(k)  = 0.0
       vfxpblk(k)  = 0.0
       qfxpblk(k)  = 0.0
     enddo
!
     do k = kts,kte
       uxk(k)   = 0.0
       vxk(k)   = 0.0
       txk(k)   = 0.0
       thxk(k)  = 0.0
       thvxk(k) = 0.0
       q2xk(k)  = 0.0
       hgame(k) = 0.0
       ps1d(k)  = 0.0
       pb1d(k)  = 0.0
       eps1d(k) = 0.0
       pt1d(k)  = 0.0
       xkze1d(k)    = 0.0
       eflx_l1d(k)  = 0.0
       eflx_nl1d(k) = 0.0
       ptke1(k)     = 1.0
     enddo
!
     do k = kts,kte+1
       zqk(k)   = 0.0
     enddo
!
     do k = kts,kte*ndiff
       qxk(k) = 0.0
     enddo
!
     do k = kts,kte
       uxk(k)   = ux(i,k)
       vxk(k)   = vx(i,k)
       txk(k)   = tx(i,k)
       thxk(k)  = thx(i,k)
       thvxk(k) = thvx(i,k)
       q2xk(k)  = q2x(i,k)
       hgame(k) = hgame2d(i,k)
     enddo
!
     do k = kts,kte-1
       if(pblflg(i).and.k.le.kpbl(i)) then
         zfacdx      = 0.2*hpbl(i)/za(i,k)
         delxy       = sqrt(dx*dy)*max(zfacdx,1.0)
         ptke1(k+1)  = ptke(delxy,hpbl(i))
       endif
     enddo
!
     do k = kts,kte+1
       zqk(k) = zq(i,k)
     enddo
!
     do k = kts,kte*ndiff
       qxk(k) = qx(i,k)
     enddo
!
     do k = kts+1,kte
       akmk(k) = xkzm(i,k-1)
       akhk(k) = xkzh(i,k-1)
       mfk(k)      = mf(i,k-1)/xkzh(i,k-1)
       ufxpblk(k)  = ufxpbl(i)*zfacent(i,k-1)/xkzm(i,k-1)
       vfxpblk(k)  = vfxpbl(i)*zfacent(i,k-1)/xkzm(i,k-1)
       qfxpblk(k)  = qfxpbl(i)*zfacent(i,k-1)/xkzq(i,k-1)
     enddo
!
     if(pblflg(i)) then
       k = kpbl(i) - 1
       dex = 0.25*(q2xk(k+2)-q2xk(k))
       efxpbl(i) = we(i)*dex
     endif
!
     delxy=sqrt(dx*dy)
!
!---- find the mixing length
!
     call mixlen(lmh,uxk,vxk,txk,thxk,qxk(kts),qxk(kte+1)                      &
                     ,q2xk,zqk,ust(i),corf(i),epshol(i)                        &
                     ,s2,gh,rig,el                                             &
                     ,hpbl(i),kpbl(i),lmxl,ct(i)                               &
                     ,hgamu(i),hgamv(i),hgamq(i),pblflg(i)                     &
                     ,mfk,ufxpblk,vfxpblk,qfxpblk                              &
                     ,ep1,karman,cp                                            &
                     ,ids,ide,jds,jde,kds,kde                                  &
                     ,ims,ime,jms,jme,kms,kme                                  &
                     ,its,ite,jts,jte,kts,kte   )
!
!---- solve for the production/dissipation of the turbulent kinetic energy
!
     call prodq2(lmh,dt,ust(i),s2,rig,q2xk,el,zqk,akmk,akhk                    &
                     ,uxk,vxk,thxk,thvxk                                       &
                     ,hgamu(i),hgamv(i),hgamq(i),delxy                         &
                     ,hpbl(i),pblflg(i),kpbl(i)                                &
                     ,mfk,ufxpblk,vfxpblk,qfxpblk                              &
                     ,ep1                                                      &
                     ,ids,ide,jds,jde,kds,kde                                  &
                     ,ims,ime,jms,jme,kms,kme                                  &
                     ,its,ite,jts,jte,kts,kte   )
!
!
!---- carry out the vertical diffusion of turbulent kinetic energy
!
     call vdifq(lmh,dt,q2xk,el,zqk                                             &
                    ,akhk,ptke1                                                &
                    ,hgame,hpbl(i),pblflg(i),kpbl(i)                           &
                    ,efxpbl(i)                                                 &
                    ,ids,ide,jds,jde,kds,kde                                   &
                    ,ims,ime,jms,jme,kms,kme                                   &
                    ,its,ite,jts,jte,kts,kte   )
!
!---- save the new tke and mixing length.
!
     do k = kts,kte
       q2x(i,k) = amax1(q2xk(k),epsq2l)
       tke(i,k) = 0.5*q2x(i,k)
       if(k/=kts) el_pbl(i,k) = el(k) ! el is not defined at kte
     enddo
!
   enddo tke_calculation
   endif
!
!---- end of tke calculation
!
!
!---- end of vertical diffusion
!
   end subroutine shinhong2d
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine tridi1n(cl,cm,cu,r1,r2,au,f1,f2,its,ite,kts,kte,nt)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer, intent(in )      ::     its,ite, kts,kte, nt
!
   real, dimension( its:ite, kts+1:kte+1 )                                   , &
         intent(in   )  ::                                                 cl
!
   real, dimension( its:ite, kts:kte )                                       , &
         intent(in   )  ::                                                 cm, &
                                                                           r1
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(in   )  ::                                                 r2
!
   real, dimension( its:ite, kts:kte )                                       , &
         intent(inout)  ::                                                 au, &
                                                                           cu, &
                                                                           f1
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(inout)  ::                                                 f2
!
   real    :: fk
   integer :: i,k,l,n,it
!
!-------------------------------------------------------------------------------
!
   l = ite
   n = kte
!
   do i = its,l
     fk = 1./cm(i,1)
     au(i,1) = fk*cu(i,1)
     f1(i,1) = fk*r1(i,1)
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./cm(i,1)
       f2(i,1,it) = fk*r2(i,1,it)
     enddo
   enddo
!
   do k = kts+1,n-1
     do i = its,l
       fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
       au(i,k) = fk*cu(i,k)
       f1(i,k) = fk*(r1(i,k)-cl(i,k)*f1(i,k-1))
     enddo
   enddo
!
   do it = 1,nt
     do k = kts+1,n-1
       do i = its,l
         fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
         f2(i,k,it) = fk*(r2(i,k,it)-cl(i,k)*f2(i,k-1,it))
       enddo
     enddo
   enddo
!
   do i = its,l
     fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
     f1(i,n) = fk*(r1(i,n)-cl(i,n)*f1(i,n-1))
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
       f2(i,n,it) = fk*(r2(i,n,it)-cl(i,n)*f2(i,n-1,it))
     enddo
   enddo
!
   do k = n-1,kts,-1
     do i = its,l
       f1(i,k) = f1(i,k)-au(i,k)*f1(i,k+1)
     enddo
   enddo
!
   do it = 1,nt
     do k = n-1,kts,-1
       do i = its,l
         f2(i,k,it) = f2(i,k,it)-au(i,k)*f2(i,k+1,it)
       enddo
     enddo
   enddo
!
   end subroutine tridi1n
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine tridin_ysu(cl,cm,cu,r2,au,f2,its,ite,kts,kte,nt)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer, intent(in )      ::     its,ite, kts,kte, nt
!
   real, dimension( its:ite, kts+1:kte+1 )                                   , &
         intent(in   )  ::                                                 cl
!
   real, dimension( its:ite, kts:kte )                                       , &
         intent(in   )  ::                                                 cm
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(in   )  ::                                                 r2
!
   real, dimension( its:ite, kts:kte )                                       , &
         intent(inout)  ::                                                 au, &
                                                                           cu
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(inout)  ::                                                 f2
!
   real    :: fk
   integer :: i,k,l,n,it
!
!-------------------------------------------------------------------------------
!
   l = ite
   n = kte
!
   do it = 1,nt
     do i = its,l
       fk = 1./cm(i,1)
       au(i,1) = fk*cu(i,1)
       f2(i,1,it) = fk*r2(i,1,it)
     enddo
   enddo
!
   do it = 1,nt
     do k = kts+1,n-1
       do i = its,l
         fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
         au(i,k) = fk*cu(i,k)
         f2(i,k,it) = fk*(r2(i,k,it)-cl(i,k)*f2(i,k-1,it))
       enddo
     enddo
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
       f2(i,n,it) = fk*(r2(i,n,it)-cl(i,n)*f2(i,n-1,it))
     enddo
   enddo
!
   do it = 1,nt
     do k = n-1,kts,-1
       do i = its,l
         f2(i,k,it) = f2(i,k,it)-au(i,k)*f2(i,k+1,it)
       enddo
     enddo
   enddo
!
   end subroutine tridin_ysu
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine mixlen(lmh,u,v,t,the,q,cwm,q2,z,ustar,corf,epshol,               &
                     s2,gh,ri,el,hpbl,lpbl,lmxl,ct,                            &
                     hgamu,hgamv,hgamq,pblflg,                                 &
                     mf,ufxpbl,vfxpbl,qfxpbl,                                  &
                     p608,vkarman,cp,                                          &
                     ids,ide,jds,jde,kds,kde,                                  &
                     ims,ime,jms,jme,kms,kme,                                  &
                     its,ite,jts,jte,kts,kte)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!  qnse model constants
!-------------------------------------------------------------------------------
   real,parameter :: blckdr=0.0063,cn=0.75
   real,parameter :: eps1=1.e-12,epsl=0.32,epsru=1.e-7,epsrs=1.e-7
   real,parameter :: el0max=1000.,el0min=1.,elfc=0.23*0.5
   real,parameter :: alph=0.30,beta=1./273.,g=9.81,btg=beta*g
   real,parameter :: a1=0.659888514560862645,a2x=0.6574209922667784586
   real,parameter :: b1=11.87799326209552761,b2=7.226971804046074028
   real,parameter :: c1=0.000830955950095854396
   real,parameter :: adnh= 9.*a1*a2x*a2x*(12.*a1+3.*b2)*btg*btg
   real,parameter :: adnm=18.*a1*a1*a2x*(b2-3.*a2x)*btg
   real,parameter :: bdnh= 3.*a2x*(7.*a1+b2)*btg,bdnm= 6.*a1*a1
!-------------------------------------------------------------------------------
!  free term in the equilibrium equation for (l/q)**2
!-------------------------------------------------------------------------------
   real,parameter :: aeqh=9.*a1*a2x*a2x*b1*btg*btg &
                         +9.*a1*a2x*a2x*(12.*a1+3.*b2)*btg*btg
   real,parameter :: aeqm=3.*a1*a2x*b1*(3.*a2x+3.*b2*c1+18.*a1*c1-b2) &
                         *btg+18.*a1*a1*a2x*(b2-3.*a2x)*btg
!-------------------------------------------------------------------------------
!  forbidden turbulence area
!-------------------------------------------------------------------------------
   real,parameter :: requ=-aeqh/aeqm
   real,parameter :: epsgh=1.e-9,epsgm=requ*epsgh
!-------------------------------------------------------------------------------
!  near isotropy for shear turbulence, ww/q2 lower limit
!-------------------------------------------------------------------------------
   real,parameter :: ubryl=(18.*requ*a1*a1*a2x*b2*c1*btg &
                            +9.*a1*a2x*a2x*b2*btg*btg)   &
                           /(requ*adnm+adnh)
   real,parameter :: ubry=(1.+epsrs)*ubryl,ubry3=3.*ubry
   real,parameter :: aubh=27.*a1*a2x*a2x*b2*btg*btg-adnh*ubry3
   real,parameter :: aubm=54.*a1*a1*a2x*b2*c1*btg  -adnm*ubry3
   real,parameter :: bubh=(9.*a1*a2x+3.*a2x*b2)*btg-bdnh*ubry3
   real,parameter :: bubm=18.*a1*a1*c1             -bdnm*ubry3
   real,parameter :: cubr=1.-ubry3,rcubr=1./cubr
!-------------------------------------------------------------------------------
!  k profile constants
!-------------------------------------------------------------------------------
   real,parameter :: elcbl=0.77
!-------------------------------------------------------------------------------
!
   integer,  intent(in   )   ::     ids,ide, jds,jde, kds,kde,                 &
                                    ims,ime, jms,jme, kms,kme,                 &
                                    its,ite, jts,jte, kts,kte
   integer,  intent(in   )   ::     lmh,lmxl,lpbl
!
   real,     intent(in   )   ::     p608,vkarman,cp
   real,     intent(in   )   ::     hpbl,corf,ustar,hgamu,hgamv,hgamq
   real,     intent(inout)   ::     ct,epshol
!
   real,     dimension( kts:kte )                                            , &
             intent(in   )   ::                                           cwm, &
                                                                            q, &
                                                                           q2, &
                                                                            t, &
                                                                          the, &
                                                                            u, &
                                                                            v
!
   real,     dimension( kts+1:kte )                                          , &
             intent(in   )   ::                                            mf, &
                                                                       ufxpbl, &
                                                                       vfxpbl, &
                                                                       qfxpbl
!
   real,     dimension( kts:kte+1 )                                          , &
             intent(in   )   ::                                             z
!
   real,     dimension( kts+1:kte )                                          , &
             intent(out  )   ::                                            el, &
                                                                           ri, &
                                                                           gh, &
                                                                           s2
!
   logical,intent(in) :: pblflg
!
!  local vars
!
   integer :: k,lpblm
   real    :: suk,svk,elocp
   real    :: a,aden,b,bden,aubr,bubr,blmx,el0,eloq2x,ghl,s2l,                 &
              qol2st,qol2un,qdzl,rdz,sq,srel,szq,tem,thm,vkrmz,rlambda,        &
              rlb,rln,f
   real    :: ckp
   real,     dimension( kts:kte )   ::                                     q1, &
                                                                          en2
   real,     dimension( kts+1:kte ) ::                                    dth, &
                                                                          elm, &
                                                                          rel
!
!-------------------------------------------------------------------------------
!
   elocp=2.72e6/cp
   ct=0.
!
   do k = kts,kte
     q1(k) = 0.
   enddo
!
   do k = kts+1,kte
     dth(k) = the(k)-the(k-1)
   enddo
!
   do k = kts+2,kte
     if(dth(k)>0..and.dth(k-1)<=0.)then
       dth(k)=dth(k)+ct
       exit
     endif
   enddo
!
!  compute local gradient richardson number
!
   do k = kte,kts+1,-1
     rdz=2./(z(k+1)-z(k-1))
     s2l=((u(k)-u(k-1))**2+(v(k)-v(k-1))**2)*rdz*rdz ! s**2
     if(pblflg.and.k.le.lpbl)then
       suk=(u(k)-u(k-1))*rdz
       svk=(v(k)-v(k-1))*rdz
       s2l=(suk-hgamu/hpbl-ufxpbl(k))*suk+(svk-hgamv/hpbl-vfxpbl(k))*svk
     endif
     s2l=max(s2l,epsgm)
     s2(k)=s2l
!
     tem=(t(k)+t(k-1))*0.5
     thm=(the(k)+the(k-1))*0.5
     a=thm*p608
     b=(elocp/tem-1.-p608)*thm
     ghl=(dth(k)*((q(k)+q(k-1)+cwm(k)+cwm(k-1))*(0.5*p608)+1.)                 &
        +(q(k)-q(k-1)+cwm(k)-cwm(k-1))*a                                       &
        +(cwm(k)-cwm(k-1))*b)*rdz                                  ! dtheta/dz
     if(pblflg.and.k.le.lpbl)then
       ghl=ghl-mf(k)-(hgamq/hpbl+qfxpbl(k))*a
     endif
     if(abs(ghl)<=epsgh)ghl=epsgh
!
     en2(k)=ghl*g/thm                                   ! n**2
     gh(k)=ghl
     ri(k)=en2(k)/s2l
   enddo
!
!  find maximum mixing lengths and the level of the pbl top
!
   do k = kte,kts+1,-1
     s2l=s2(k)
     ghl=gh(k)
     if(ghl>=epsgh)then
       if(s2l/ghl<=requ)then
         elm(k)=epsl
       else
         aubr=(aubm*s2l+aubh*ghl)*ghl
         bubr= bubm*s2l+bubh*ghl
         qol2st=(-0.5*bubr+sqrt(bubr*bubr*0.25-aubr*cubr))*rcubr
         eloq2x=1./qol2st
         elm(k)=max(sqrt(eloq2x*q2(k)),epsl)
       endif
     else
       aden=(adnm*s2l+adnh*ghl)*ghl
       bden= bdnm*s2l+bdnh*ghl
       qol2un=-0.5*bden+sqrt(bden*bden*0.25-aden)
       eloq2x=1./(qol2un+epsru)       ! repsr1/qol2un
       elm(k)=max(sqrt(eloq2x*q2(k)),epsl)
     endif
   enddo
!
   do k = lpbl,lmh,-1
     q1(k)=sqrt(q2(k))
   enddo
!
   szq=0.
   sq =0.
   do k = kte,kts+1,-1
     qdzl=(q1(k)+q1(k-1))*(z(k)-z(k-1))
     szq=(z(k)+z(k-1)-z(lmh)-z(lmh))*qdzl+szq
     sq=qdzl+sq
   enddo
!
!  computation of asymptotic l in blackadar formula
!
   el0=min(alph*szq*0.5/sq,el0max)
   el0=max(el0            ,el0min)
!
!  above the pbl top
!
   lpblm=min(lpbl+1,kte)
   do k = kte,lpblm,-1
     el(k)=(z(k+1)-z(k-1))*elfc
     rel(k)=el(k)/elm(k)
   enddo
!
!  inside the pbl
!
   epshol=min(epshol,0.0)
   ckp=elcbl*((1.0-8.0*epshol)**(1./3.))
   if(lpbl>lmh)then
     do k = lpbl,lmh+1,-1
       vkrmz=(z(k)-z(lmh))*vkarman
       if(pblflg) then
         vkrmz=ckp*(z(k)-z(lmh))*vkarman
         el(k)=vkrmz/(vkrmz/el0+1.)
       else
         el(k)=vkrmz/(vkrmz/el0+1.)
       endif
       rel(k)=el(k)/elm(k)
     enddo
   endif
!
   do k = lpbl-1,lmh+2,-1
     srel=min(((rel(k-1)+rel(k+1))*0.5+rel(k))*0.5,rel(k))
     el(k)=max(srel*elm(k),epsl)
   enddo
!
!  mixing length for the qnse model in stable case
!
   f=max(corf,eps1)
   rlambda=f/(blckdr*ustar)
   do k = kte,kts+1,-1
     if(en2(k)>=0.0)then ! stable case
       vkrmz=(z(k)-z(lmh))*vkarman
       rlb=rlambda+1./vkrmz
       rln=sqrt(2.*en2(k)/q2(k))/cn
       el(k)=1./(rlb+rln)
     endif
   enddo
!
   end subroutine mixlen
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine prodq2(lmh,dtturbl,ustar,s2,ri,q2,el,z,akm,akh,                  &
                     uxk,vxk,thxk,thvxk,                                       &
                     hgamu,hgamv,hgamq,delxy,                                  &
                     hpbl,pblflg,kpbl,                                         &
                     mf,ufxpbl,vfxpbl,qfxpbl,                                  &
                     p608,                                                     &
                     ids,ide,jds,jde,kds,kde,                                  &
                     ims,ime,jms,jme,kms,kme,                                  &
                     its,ite,jts,jte,kts,kte)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   real,parameter :: epsq2l = 0.01,c0 = 0.55,ceps = 16.6,g = 9.81
!
   integer,  intent(in   )   ::     ids,ide, jds,jde, kds,kde,                 &
                                    ims,ime, jms,jme, kms,kme,                 &
                                    its,ite, jts,jte, kts,kte
   integer,  intent(in   )   ::     lmh,kpbl
!
   real,     intent(in   )   ::     p608,dtturbl,ustar
   real,     intent(in   )   ::     hgamu,hgamv,hgamq,delxy,hpbl
!
   logical,  intent(in   )   ::     pblflg
!
   real,     dimension( kts:kte )                                            , &
             intent(in   )   ::                                           uxk, &
                                                                          vxk, &
                                                                         thxk, &
                                                                        thvxk
   real,     dimension( kts+1:kte )                                          , &
             intent(in   )   ::                                            s2, &
                                                                           ri, &
                                                                          akm, &
                                                                          akh, &
                                                                           el, &
                                                                           mf, &
                                                                       ufxpbl, &
                                                                       vfxpbl, &
                                                                       qfxpbl
!
   real,     dimension( kts:kte+1 )                                          , &
             intent(in   )   ::                                             z
!
   real,     dimension( kts:kte )                                            , &
             intent(inout)   ::                                            q2
!
!  local vars
!
   integer :: k
!
   real    :: s2l,q2l,deltaz,akml,akhl,en2,pr,bpr,dis,rc02
   real    :: suk,svk,gthvk,govrthvk,pru,prv
   real    :: thm,disel
!
!-------------------------------------------------------------------------------
!
   rc02=2.0/(c0*c0)
!
!  start of production/dissipation loop
!
   main_integration: do k = kts+1,kte
     deltaz=0.5*(z(k+1)-z(k-1))
     s2l=s2(k)
     q2l=q2(k)
     suk=(uxk(k)-uxk(k-1))/deltaz
     svk=(vxk(k)-vxk(k-1))/deltaz
     gthvk=(thvxk(k)-thvxk(k-1))/deltaz
     govrthvk=g/(0.5*(thvxk(k)+thvxk(k-1)))
     akml=akm(k)
     akhl=akh(k)
     en2=ri(k)*s2l !n**2
     thm=(thxk(k)+thxk(k-1))*0.5
!
!  turbulence production term
!
     if(pblflg.and.k.le.kpbl)then
       pru=(akml*(suk-hgamu/hpbl-ufxpbl(k)))*suk
       prv=(akml*(svk-hgamv/hpbl-vfxpbl(k)))*svk
     else
       pru=akml*suk*suk
       prv=akml*svk*svk
     endif
     pr=pru+prv
!
!  buoyancy production
!
     if(pblflg.and.k.le.kpbl)then
       bpr=(akhl*(gthvk-mf(k)-(hgamq/hpbl+qfxpbl(k))*p608*thm))*govrthvk
     else
       bpr=akhl*gthvk*govrthvk
     endif
!
!  dissipation
!
     disel=min(delxy,ceps*el(k))
     dis=(q2l)**1.5/disel
!
     q2l=q2l+2.0*(pr-bpr-dis)*dtturbl
     q2(k)=amax1(q2l,epsq2l)
!
!  end of production/dissipation loop
!
   enddo main_integration
!
!  lower boundary condition for q2
!
   q2(kts)=amax1(rc02*ustar*ustar,epsq2l)
!
   end subroutine prodq2
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine vdifq(lmh,dtdif,q2,el,z,                                         &
                    akhk,ptke1,                                                &
                    hgame,hpbl,pblflg,kpbl,                                    &
                    efxpbl,                                                    &
                    ids,ide,jds,jde,kds,kde,                                   &
                    ims,ime,jms,jme,kms,kme,                                   &
                    its,ite,jts,jte,kts,kte)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   real,parameter     :: c_k=1.0,esq=5.0
!
   integer,  intent(in   )   ::     ids,ide, jds,jde, kds,kde,                 &
                                    ims,ime, jms,jme, kms,kme,                 &
                                    its,ite, jts,jte, kts,kte
   integer,  intent(in   )   ::     lmh,kpbl
!
   real,     intent(in   )   ::     dtdif,hpbl,efxpbl
!
   logical,  intent(in   )   ::     pblflg
!
   real,     dimension( kts:kte )                                            , &
             intent(in   )   ::                                         hgame, &
                                                                        ptke1
   real,     dimension( kts+1:kte )                                          , &
             intent(in   )   ::                                            el, &
                                                                         akhk
   real,     dimension( kts:kte+1 )                                          , &
             intent(in   )   ::                                             z
!
   real,     dimension( kts:kte )                                            , &
             intent(inout)   ::                                            q2
!
!  local vars
!
   integer :: k
!
   real    :: aden,akqs,bden,besh,besm,cden,cf,dtozs,ell,eloq2,eloq4
   real    :: elqdz,esh,esm,esqhf,ghl,gml,q1l,rden,rdz
   real    :: zak
!
   real,     dimension( kts+1:kte ) ::                               zfacentk
   real,     dimension( kts+2:kte ) ::                                    akq, &
                                                                           cm, &
                                                                           cr, &
                                                                         dtoz, &
                                                                         rsq2
!
!-------------------------------------------------------------------------------
!
!  vertical turbulent diffusion
!
   esqhf=0.5*esq
   do k = kts+1,kte
     zak=0.5*(z(k)+z(k-1)) !zak of vdifq = za(k-1) of shinhong2d
     zfacentk(k)=(zak/hpbl)**3.0
   enddo
!
   do k = kte,kts+2,-1
     dtoz(k)=(dtdif+dtdif)/(z(k+1)-z(k-1))
     akq(k)=c_k*(akhk(k)/(z(k+1)-z(k-1))+akhk(k-1)/(z(k)-z(k-2)))
     akq(k)=akq(k)*ptke1(k)
     cr(k)=-dtoz(k)*akq(k)
   enddo
!
   akqs=c_k*akhk(kts+1)/(z(kts+2)-z(kts))
   akqs=akqs*ptke1(kts+1)
   cm(kte)=dtoz(kte)*akq(kte)+1.
   rsq2(kte)=q2(kte)
!
   do k = kte-1,kts+2,-1
     cf=-dtoz(k)*akq(k+1)/cm(k+1)
     cm(k)=-cr(k+1)*cf+(akq(k+1)+akq(k))*dtoz(k)+1.
     rsq2(k)=-rsq2(k+1)*cf+q2(k)
     if(pblflg.and.k.lt.kpbl) then
       rsq2(k)=rsq2(k)-dtoz(k)*(2.0*hgame(k)/hpbl)*akq(k+1)*(z(k+1)-z(k))      &
                      +dtoz(k)*(2.0*hgame(k-1)/hpbl)*akq(k)*(z(k)-z(k-1))
       rsq2(k)=rsq2(k)-dtoz(k)*2.0*efxpbl*zfacentk(k+1)                        &
                      +dtoz(k)*2.0*efxpbl*zfacentk(k)
     endif
   enddo
!
   dtozs=(dtdif+dtdif)/(z(kts+2)-z(kts))
   cf=-dtozs*akq(lmh+2)/cm(lmh+2)
!
   if(pblflg.and.((lmh+1).lt.kpbl)) then
     q2(lmh+1)=(dtozs*akqs*q2(lmh)-rsq2(lmh+2)*cf+q2(lmh+1)                    &
               -dtozs*(2.0*hgame(lmh+1)/hpbl)*akq(lmh+2)*(z(lmh+2)-z(lmh+1))   &
               +dtozs*(2.0*hgame(lmh)/hpbl)*akqs*(z(lmh+1)-z(lmh)))
     q2(lmh+1)=q2(lmh+1)-dtozs*2.0*efxpbl*zfacentk(lmh+2)                      &
                        +dtozs*2.0*efxpbl*zfacentk(lmh+1)
     q2(lmh+1)=q2(lmh+1)/((akq(lmh+2)+akqs)*dtozs-cr(lmh+2)*cf+1.)
   else
     q2(lmh+1)=(dtozs*akqs*q2(lmh)-rsq2(lmh+2)*cf+q2(lmh+1))                   &
              /((akq(lmh+2)+akqs)*dtozs-cr(lmh+2)*cf+1.)
   endif
!
   do k = lmh+2,kte
     q2(k)=(-cr(k)*q2(k-1)+rsq2(k))/cm(k)
   enddo
!
   end subroutine vdifq
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine shinhonginit(rublten,rvblten,rthblten,rqvblten,                  &
                      rqcblten,rqiblten,                                       &
                      tke_pbl,                                                 &
                      p_qi,p_first_scalar,                                     &
                      restart, allowed_to_read,                                &
                      ids, ide, jds, jde, kds, kde,                            &
                      ims, ime, jms, jme, kms, kme,                            &
                      its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   real,parameter                ::  epsq2l = 0.01
   logical , intent(in)          ::  restart, allowed_to_read
   integer , intent(in)          ::  ids, ide, jds, jde, kds, kde,             &
                                     ims, ime, jms, jme, kms, kme,             &
                                     its, ite, jts, jte, kts, kte
   integer , intent(in)          ::  p_qi,p_first_scalar
   real , dimension( ims:ime , kms:kme , jms:jme ), intent(out) ::             &
                                                                      rublten, &
                                                                      rvblten, &
                                                                     rthblten, &
                                                                     rqvblten, &
                                                                     rqcblten, &
                                                                     rqiblten
   real , dimension( ims:ime , kms:kme , jms:jme ), intent(out) ::             &
                                                                      tke_pbl
   integer :: i, j, k, itf, jtf, ktf
!
   jtf = min0(jte,jde-1)
   ktf = min0(kte,kde-1)
   itf = min0(ite,ide-1)
!
   if(.not.restart)then
     do j = jts,jtf
       do k = kts,ktf
         do i = its,itf
            rublten(i,k,j) = 0.
            rvblten(i,k,j) = 0.
            rthblten(i,k,j) = 0.
            rqvblten(i,k,j) = 0.
            rqcblten(i,k,j) = 0.
            tke_pbl(i,k,j) = epsq2l/2.
         enddo
       enddo
     enddo
   endif
!
   if (p_qi .ge. p_first_scalar .and. .not.restart) then
     do j = jts,jtf
       do k = kts,ktf
         do i = its,itf
           rqiblten(i,k,j) = 0.
         enddo
       enddo
     enddo
   endif
!
   end subroutine shinhonginit
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function pu(d,h)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real :: pu
   real,parameter :: pmin = 0.0,pmax = 1.0
   real,parameter :: a1 = 1.0, a2 = 0.070, a3 = 1.0, a4 = 0.142, a5 = 0.071
   real,parameter :: b1 = 2.0, b2 = 0.6666667
   real :: d,h,doh,num,den
!
   doh=d/h
   num=a1*(doh)**b1+a2*(doh)**b2
   den=a3*(doh)**b1+a4*(doh)**b2+a5
   pu=num/den
   pu=max(pu,pmin)
   pu=min(pu,pmax)
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function pq(d,h)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real :: pq
   real,parameter :: pmin = 0.0,pmax = 1.0
   real,parameter :: a1 = 1.0, a2 = -0.098, a3 = 1.0, a4 = 0.106, a5 = 0.5
   real,parameter :: b1 = 2.0
   real :: d,h,doh,num,den
!
   doh=d/h
   num=a1*(doh)**b1+a2
   den=a3*(doh)**b1+a4
   pq=a5*num/den+(1.-a5)
   pq=max(pq,pmin)
   pq=min(pq,pmax)
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function pthnl(d,h)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real :: pthnl
   real,parameter :: pmin = 0.0,pmax = 1.0
   real,parameter :: a1 = 1.000, a2 = 0.936, a3 = -1.110,                      &
                     a4 = 1.000, a5 = 0.312, a6 = 0.329, a7 = 0.243
   real,parameter :: b1 = 2.0, b2 = 0.875
   real :: d,h,doh,num,den
!
   doh=d/h
   num=a1*(doh)**b1+a2*(doh)**b2+a3
   den=a4*(doh)**b1+a5*(doh)**b2+a6
   pthnl=a7*num/den+(1.-a7)
   pthnl=max(pthnl,pmin)
   pthnl=min(pthnl,pmax)
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function pthl(d,h)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real :: pthl
   real,parameter :: pmin = 0.0,pmax = 1.0
   real,parameter :: a1 = 1.000, a2 = 0.870, a3 = -0.913,                      &
                     a4 = 1.000, a5 = 0.153, a6 = 0.278, a7 = 0.280
   real,parameter :: b1 = 2.0, b2 = 0.5
   real :: d,h,doh,num,den
!
   doh=d/h
   num=a1*(doh)**b1+a2*(doh)**b2+a3
   den=a4*(doh)**b1+a5*(doh)**b2+a6
   pthl=a7*num/den+(1.-a7)
   pthl=max(pthl,pmin)
   pthl=min(pthl,pmax)
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function ptke(d,h)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real :: ptke
   real,parameter :: pmin = 0.0,pmax = 1.0
   real,parameter :: a1 = 1.000, a2 = 0.070,                                   &
                     a3 = 1.000, a4 = 0.142, a5 = 0.071
   real,parameter :: b1 = 2.0, b2 = 0.6666667
   real :: d,h,doh,num,den
!
   doh=d/h
   num=a1*(doh)**b1+a2*(doh)**b2
   den=a3*(doh)**b1+a4*(doh)**b2+a5
   ptke=num/den
   ptke=max(ptke,pmin)
   ptke=min(ptke,pmax)
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
end module module_bl_shinhong
!-------------------------------------------------------------------------------
