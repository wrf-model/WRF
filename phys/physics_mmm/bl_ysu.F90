#define  NEED_B4B_DURING_CCPP_TESTING 1
!=================================================================================================================
 module bl_ysu
 use ccpp_kind_types,only: kind_phys

 implicit none
 private
 public:: bl_ysu_run,     &
          bl_ysu_init,    &
          bl_ysu_finalize


 contains


!=================================================================================================================
!>\section arg_table_bl_ysu_init
!!\html\include bl_ysu_init.html
!!
 subroutine bl_ysu_init(errmsg,errflg)
!=================================================================================================================

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------

 errmsg = 'bl_ysu_init OK'
 errflg = 0

 end subroutine bl_ysu_init

!=================================================================================================================
!>\section arg_table_bl_ysu_finalize
!!\html\include bl_ysu_finalize.html
!!
 subroutine bl_ysu_finalize(errmsg,errflg)
!=================================================================================================================

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------

 errmsg = 'bl_ysu_finalize OK'
 errflg = 0

 end subroutine bl_ysu_finalize

!=================================================================================================================
!>\section arg_table_bl_ysu_run
!!\html\include bl_ysu_run.html
!!
   subroutine bl_ysu_run(ux,vx,tx,qvx,qcx,qix,nmix,qmix,p2d,p2di,pi2d,     &
                         f_qc,f_qi,                                        &
                         utnp,vtnp,ttnp,qvtnp,qctnp,qitnp,qmixtnp,         &
                         cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,         &
                         dz8w2d,psfcpa,                                    &
                         znt,ust,hpbl,dusfc,dvsfc,dtsfc,dqsfc,psim,psih,   &
                         xland,hfx,qfx,wspd,br,                            &
                         dt,kpbl1d,                                        &
                         exch_hx,exch_mx,                                  &
                         wstar,delta,                                      &
                         u10,v10,                                          &
                         uox,vox,                                          &
                         rthraten,                                         &
                         ysu_topdown_pblmix,                               &
                         ctopo,ctopo2,                                     &
                         a_u,a_v,a_t,a_q,a_e,                              &
                         b_u,b_v,b_t,b_q,b_e,                              &
                         sfk,vlk,dlu,dlg,frcurb,                           &
                         flag_bep,                                         &
                         its,ite,kte,kme,                                  &
                         errmsg,errflg                                     &
                        )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
!     this code is a revised vertical diffusion package ("ysupbl")
!     with a nonlocal turbulent mixing in the pbl after "mrfpbl".
!     the ysupbl (hong et al. 2006) is based on the study of noh
!     et al.(2003) and accumulated realism of the behavior of the
!     troen and mahrt (1986) concept implemented by hong and pan(1996).
!     the major ingredient of the ysupbl is the inclusion of an explicit
!     treatment of the entrainment processes at the entrainment layer.
!     this routine uses an implicit approach for vertical flux
!     divergence and does not require "miter" timesteps.
!     it includes vertical diffusion in the stable atmosphere
!     and moist vertical diffusion in clouds.
!
!     mrfpbl:
!     coded by song-you hong (ncep), implemented by jimy dudhia (ncar)
!              fall 1996
!
!     ysupbl:
!     coded by song-you hong (yonsei university) and implemented by
!              song-you hong (yonsei university) and jimy dudhia (ncar)
!              summer 2002
!
!     further modifications :
!              an enhanced stable layer mixing, april 2008
!              ==> increase pbl height when sfc is stable (hong 2010)
!              pressure-level diffusion, april 2009
!              ==> negligible differences
!              implicit forcing for momentum with clean up, july 2009
!              ==> prevents model blowup when sfc layer is too low
!              incresea of lamda, maximum (30, 0.1 x del z) feb 2010
!              ==> prevents model blowup when delz is extremely large
!              revised prandtl number at surface, peggy lemone, feb 2010
!              ==> increase kh, decrease mixing due to counter-gradient term
!              revised thermal, shin et al. mon. wea. rev. , songyou hong, aug 2011
!              ==> reduce the thermal strength when z1 < 0.1 h
!              revised prandtl number for free convection, dudhia, mar 2012
!              ==> pr0 = 1 + bke (=0.272) when neutral, kh is reduced
!              minimum kzo = 0.01, lo = min (30m,delz), hong, mar 2012
!              ==> weaker mixing when stable, and les resolution in vertical
!              gz1oz0 is removed, and psim psih are ln(z1/z0)-psim,h, hong, mar 2012
!              ==> consider thermal z0 when differs from mechanical z0
!              a bug fix in wscale computation in stable bl, sukanta basu, jun 2012
!              ==> wscale becomes small with height, and less mixing in stable bl
!              revision in background diffusion (kzo), jan 2016
!              ==> kzo = 0.1 for momentum and = 0.01 for mass to account for
!                  internal wave mixing of large et al. (1994), songyou hong, feb 2016
!              ==> alleviate superious excessive mixing when delz is large
!              add multilayer urban canopy models of BEP and BEP+BEM, jan 2021
!
!     references:
!
!        hendricks, knievel, and wang (2020), j. appl. meteor. clim.
!        hong (2010) quart. j. roy. met. soc
!        hong, noh, and dudhia (2006), mon. wea. rev.
!        hong and pan (1996), mon. wea. rev.
!        noh, chun, hong, and raasch (2003), boundary layer met.
!        troen and mahrt (1986), boundary layer met.
!
!-------------------------------------------------------------------------------
!
   real(kind=kind_phys),parameter    ::  xkzminm = 0.1,xkzminh = 0.01
   real(kind=kind_phys),parameter    ::  xkzmin = 0.01,xkzmax = 1000.,rimin = -100.
   real(kind=kind_phys),parameter    ::  rlam = 30.,prmin = 0.25,prmax = 4.
   real(kind=kind_phys),parameter    ::  brcr_ub = 0.0,brcr_sb = 0.25,cori = 1.e-4
   real(kind=kind_phys),parameter    ::  afac = 6.8,bfac = 6.8,pfac = 2.0,pfac_q = 2.0
   real(kind=kind_phys),parameter    ::  phifac = 8.,sfcfrac = 0.1
   real(kind=kind_phys),parameter    ::  d1 = 0.02, d2 = 0.05, d3 = 0.001
   real(kind=kind_phys),parameter    ::  h1 = 0.33333335, h2 = 0.6666667
   real(kind=kind_phys),parameter    ::  zfmin = 1.e-8,aphi5 = 5.,aphi16 = 16.
   real(kind=kind_phys),parameter    ::  tmin=1.e-2
   real(kind=kind_phys),parameter    ::  gamcrt = 3.,gamcrq = 2.e-3
   real(kind=kind_phys),parameter    ::  xka = 2.4e-5
   integer,parameter ::  imvdif = 1
   real(kind=kind_phys),parameter    ::  rcl = 1.0
   integer,parameter ::  kts=1, kms=1
!
   integer,  intent(in   )   ::     its,ite,kte,kme

   logical,  intent(in)      ::     ysu_topdown_pblmix
!
   integer,  intent(in)      ::     nmix
!
   real(kind=kind_phys),     intent(in   )   ::     dt,cp,g,rovcp,rovg,rd,xlv,rv
!
   real(kind=kind_phys),     intent(in )     ::     ep1,ep2,karman
!
   logical,  intent(in )     ::     f_qc, f_qi
!
   real(kind=kind_phys),     dimension( its:,: )                             , &
             intent(in)      ::                                        dz8w2d, &
                                                                         pi2d
!
   real(kind=kind_phys),     dimension( its:,: )                             , &
             intent(in   )   ::                                            tx, &
                                                                          qvx, &
                                                                          qcx, &
                                                                          qix
!
   real(kind=kind_phys),     dimension( its:,:,: )                           , &
             intent(in   )   ::                                          qmix
!
   real(kind=kind_phys),     dimension( its:,: )                             , &
             intent(out  )   ::                                          utnp, &
                                                                         vtnp, &
                                                                         ttnp, &
                                                                        qvtnp, &
                                                                        qctnp, &
                                                                        qitnp
!
   real(kind=kind_phys),     dimension( its:,:,: )                           , &
             intent(out  )   ::                                       qmixtnp
!
   real(kind=kind_phys),     dimension( its:,: )                             , &
             intent(in   )   ::                                          p2di
!
   real(kind=kind_phys),     dimension( its:,: )                             , &
             intent(in   )   ::                                           p2d
!
   real(kind=kind_phys),     dimension( its: )                               , &
             intent(out  )   ::                                          hpbl
!
   real(kind=kind_phys),     dimension( its: )                               , &
             intent(out  ), optional ::                                 dusfc, &
                                                                        dvsfc, &
                                                                        dtsfc, &
                                                                        dqsfc
!
   real(kind=kind_phys),     dimension( its: )                               , &
             intent(in   )   ::                                           ust, &
                                                                          znt
   real(kind=kind_phys),     dimension( its: )                               , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx
!
   real(kind=kind_phys),     dimension( its: ), intent(in   )    ::      wspd
   real(kind=kind_phys),     dimension( its: ), intent(in   )    ::        br
!
   real(kind=kind_phys),     dimension( its: ), intent(in   )    ::      psim, &
                                                                         psih
!
   real(kind=kind_phys),     dimension( its: ), intent(in   )    ::    psfcpa
   integer,  dimension( its: ), intent(out  )   ::                     kpbl1d
!
   real(kind=kind_phys),     dimension( its:,: )                             , &
             intent(in   )   ::                                            ux, &
                                                                           vx, &
                                                                      rthraten
   real(kind=kind_phys),     dimension( its: )                               , &
             optional                                                        , &
             intent(in   )   ::                                         ctopo, &
                                                                       ctopo2
!
   logical,  intent(in   )   ::                                      flag_bep
   real(kind=kind_phys),     dimension( its:,: )                             , &
             optional                                                        , &
             intent(in   )   ::                                           a_u, &
                                                                          a_v, &
                                                                          a_t, &
                                                                          a_q, &
                                                                          a_e, &
                                                                          b_u, &
                                                                          b_v, &
                                                                          b_t, &
                                                                          b_q, &
                                                                          b_e, &
                                                                          sfk, &
                                                                          vlk, &
                                                                          dlu, &
                                                                          dlg
   real(kind=kind_phys),     dimension( its: )                               , &
             optional                                                        , &
             intent(in   )   ::                                        frcurb
!
   character(len=*), intent(out)   ::                                  errmsg
   integer,          intent(out)   ::                                  errflg

!
! local vars
!
   real(kind=kind_phys),     dimension( its:ite )            ::           hol
   real(kind=kind_phys),     dimension( its:ite, kms:kme )   ::            zq
!
   real(kind=kind_phys),     dimension( its:ite, kts:kte )   ::                &
                                                               thx,thvx,thlix, &
                                                                          del, &
                                                                          dza, &
                                                                          dzq, &
                                                                        xkzom, &
                                                                        xkzoh, &
                                                                           za
!
   real(kind=kind_phys),    dimension( its:ite )             ::                &
                                                                         rhox, &
                                                                       govrth, &
                                                                  zl1,thermal, &
                                                                       wscale, &
                                                                  hgamt,hgamq, &
                                                                    brdn,brup, &
                                                                    phim,phih, &
                                                                        prpbl, &
                                                              wspd1,thermalli
!
   real(kind=kind_phys),    dimension( its:ite, kts:kte )   :: xkzh,xkzm,xkzq, &
                                                                        f1,f2, &
                                                                        r1,r2, &
                                                                        ad,au, &
                                                                           cu, &
                                                                           al, &
                                                                         zfac, &
                                                                        rhox2, &
                                                                       hgamt2, &
                                                                  ad1,adm,adv
!
!jdf added exch_hx
!
   real(kind=kind_phys),    dimension( its:ite, kts:kte )                    , &
            intent(out  )   ::                                        exch_hx, &
                                                                      exch_mx
!
   real(kind=kind_phys),    dimension( its:ite )                             , &
            intent(inout)    ::                                           u10, &
                                                                          v10
   real(kind=kind_phys),    dimension( its:ite ), optional                   , &
            intent(in  )    ::                                            uox, &
                                                                          vox
   real(kind=kind_phys),    dimension( its:ite )    ::                   uoxl, &
                                                                         voxl
   real(kind=kind_phys),    dimension( its:ite )    ::                         &
                                                                         brcr, &
                                                                        sflux, &
                                                                         zol1, &
                                                                    brcr_sbro
!
   real(kind=kind_phys),    dimension( its:ite, kts:kte)     ::         r3,f3
   integer, dimension( its:ite )             ::                  kpbl,kpblold
!
   logical, dimension( its:ite )             ::                        pblflg, &
                                                                       sfcflg, &
                                                                       stable, &
                                                                     cloudflg

   logical                                   ::                     definebrup
!
   integer ::  n,i,k,l,ic,is,kk
   integer ::  klpbl
!
!
   real(kind=kind_phys)    ::  dt2,rdt,spdk2,fm,fh,hol1,gamfac,vpert,prnum,prnum0
   real(kind=kind_phys)    ::  ss,ri,qmean,tmean,alph,chi,zk,rl2,dk,sri
   real(kind=kind_phys)    ::  brint,dtodsd,dtodsu,rdz,dsdzt,dsdzq,dsdz2,rlamdz
   real(kind=kind_phys)    ::  utend,vtend,ttend,qtend
   real(kind=kind_phys)    ::  dtstep,govrthv
   real(kind=kind_phys)    ::  cont, conq, conw, conwrc
!

   real(kind=kind_phys), dimension( its:ite, kts:kte )    :: wscalek,wscalek2
   real(kind=kind_phys), dimension( its:ite ), intent(out) ::           wstar, &
                                                                        delta
   real(kind=kind_phys), dimension( its:ite, kts:kte )     ::     xkzml,xkzhl, &
                                                               zfacent,entfac
   real(kind=kind_phys), dimension( its:ite, kts:kte )     ::            qcxl, &
                                                                         qixl
   real(kind=kind_phys), dimension( its:ite )              ::            ust3, &
                                                                       wstar3, &
                                                                     wstar3_2, &
                                                                  hgamu,hgamv, &
                                                                      wm2, we, &
                                                                       bfxpbl, &
                                                                hfxpbl,qfxpbl, &
                                                                ufxpbl,vfxpbl, &
                                                                        dthvx
   real(kind=kind_phys)    ::  prnumfac,bfx0,hfx0,qfx0,delb,dux,dvx,           &
               dsdzu,dsdzv,wm3,dthx,dqx,wspd10,ross,tem1,dsig,tvcon,conpr,     &
               prfac,prfac2,phim8z,radsum,tmp1,templ,rvls,temps,ent_eff,       &
               rcldb,bruptmp,radflux,vconvlim,vconvnew,fluxc,vconvc,vconv
!topo-corr
   real(kind=kind_phys),    dimension( its:ite, kts:kte )    ::          fric, &
                                                                       tke_ysu,&
                                                                        el_ysu,&
                                                                     shear_ysu,&
                                                                      buoy_ysu
   real(kind=kind_phys),    dimension( its:ite) ::                    pblh_ysu,&
                                                                       vconvfx
!
   real(kind=kind_phys)      ::                                      bepswitch

   real(kind=kind_phys),    dimension( its:ite, kts:kte )    ::                &
               a_u2d,a_v2d,a_t2d,a_q2d,a_e2d,b_u2d,b_v2d,b_t2d,b_q2d,b_e2d,    &
               sfk2d,vlk2d,dlu2d,dlg2d
   real(kind=kind_phys),    dimension( its:ite )             ::                &
               frc_urb1d

   real(kind=kind_phys),     dimension( kts:kte  ) :: thvx_1d,tke_1d,dzq_1d
   real(kind=kind_phys),     dimension( kts:kte+1) :: zq_1d

!
!-------------------------------------------------------------------------------
!
   klpbl = kte
!
   cont=cp/g
   conq=xlv/g
   conw=1./g
   conwrc = conw*sqrt(rcl)
   conpr = bfac*karman*sfcfrac
!
!  k-start index for tracer diffusion
!
   if(f_qc) then
      do k = kts,kte
        do i = its,ite
           qcxl(i,k)  = qcx(i,k)
        enddo
      enddo
   else
      do k = kts,kte
        do i = its,ite
           qcxl(i,k) = 0.
        enddo
      enddo
   endif
!
   if(f_qi) then
      do k = kts,kte
        do i = its,ite
           qixl(i,k)  = qix(i,k)
        enddo
      enddo
   else
      do k = kts,kte
        do i = its,ite
           qixl(i,k) = 0.
        enddo
      enddo
   endif
!
   do k = kts,kte
     do i = its,ite
       thx(i,k) = tx(i,k)/pi2d(i,k)
       thlix(i,k) = (tx(i,k)-xlv*qcxl(i,k)/cp-2.834E6*qixl(i,k)/cp)/pi2d(i,k)
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       tvcon = (1.+ep1*qvx(i,k))
       thvx(i,k) = thx(i,k)*tvcon
     enddo
   enddo
!
   if ( present(uox) .and. present(vox) ) then
      do i =its,ite
         uoxl(i) = uox(i)
         voxl(i) = vox(i)
      enddo
   else
      do i =its,ite
         uoxl(i) = 0
         voxl(i) = 0
      enddo
   endif
!
   do i = its,ite
     tvcon = (1.+ep1*qvx(i,1))
     rhox(i) = psfcpa(i)/(rd*tx(i,1)*tvcon)
     govrth(i) = g/thx(i,1)
   enddo
!
   if(present(a_u) .and. present(a_v) .and. present(a_t) .and. &
      present(a_q) .and. present(a_t) .and. present(a_e) .and. &
      present(b_u) .and. present(b_v) .and. present(b_t) .and. &
      present(b_q) .and. present(b_e) .and. present(dlg) .and. &
      present(dlu) .and. present(sfk) .and. present(vlk) .and. &
      present(frcurb) .and. flag_bep) then

      bepswitch=1.0
      do k = kts, kte
         do i = its,ite
            a_u2d(i,k) = a_u(i,k)
            a_v2d(i,k) = a_v(i,k)
            a_t2d(i,k) = a_t(i,k)
            a_q2d(i,k) = a_q(i,k)
            a_e2d(i,k) = a_e(i,k)
            b_u2d(i,k) = b_u(i,k)
            b_v2d(i,k) = b_v(i,k)
            b_t2d(i,k) = b_t(i,k)
            b_q2d(i,k) = b_q(i,k)
            b_e2d(i,k) = b_e(i,k)
            dlg2d(i,k) = dlg(i,k)
            dlu2d(i,k) = dlu(i,k)
            vlk2d(i,k) = vlk(i,k)
            sfk2d(i,k) = sfk(i,k)
         enddo
      enddo
      do i = its, ite
         frc_urb1d(i) = frcurb(i)
      enddo
   else
      bepswitch=0.0
      do k = kts, kte
         do i = its,ite
            a_u2d(i,k) = 0.0
            a_v2d(i,k) = 0.0
            a_t2d(i,k) = 0.0
            a_q2d(i,k) = 0.0
            a_e2d(i,k) = 0.0
            b_u2d(i,k) = 0.0
            b_v2d(i,k) = 0.0
            b_t2d(i,k) = 0.0
            b_q2d(i,k) = 0.0
            b_e2d(i,k) = 0.0
            dlg2d(i,k) = 0.0
            dlu2d(i,k) = 0.0
            vlk2d(i,k) = 1.0
            sfk2d(i,k) = 1.0
         enddo
      enddo
      do i = its, ite
         frc_urb1d(i) = 0.0
      enddo
   endif
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
       tvcon = (1.+ep1*qvx(i,k))
       rhox2(i,k) = p2d(i,k)/(rd*tx(i,k)*tvcon)
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
!-----initialize output and local exchange coefficents:
   do k = kts,kte
     do i = its,ite
       exch_hx(i,k) = 0.
       exch_mx(i,k) = 0.
       xkzh(i,k)    = 0.
       xkzhl(i,k)   = 0.
       xkzm(i,k)    = 0.
       xkzml(i,k)   = 0.
       xkzq(i,k)    = 0.
     enddo
   enddo
!
   do i = its,ite
     wspd1(i) = sqrt( (ux(i,1)-uoxl(i))*(ux(i,1)-uoxl(i)) + (vx(i,1)-voxl(i))*(vx(i,1)-voxl(i)) )+1.e-9
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
     wstar3_2(i) =  0.0
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       wscalek(i,k) = 0.0
       wscalek2(i,k) = 0.0
     enddo
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       zfac(i,k) = 0.0
     enddo
   enddo
   do k = kts,klpbl-1
     do i = its,ite
       xkzom(i,k) = xkzminm
       xkzoh(i,k) = xkzminh
     enddo
   enddo
!
   do i = its,ite
      if(present(dusfc)) dusfc(i) = 0.
      if(present(dvsfc)) dvsfc(i) = 0.
      if(present(dtsfc)) dtsfc(i) = 0.
      if(present(dqsfc)) dqsfc(i) = 0.
   enddo
!
   do i = its,ite
     hgamt(i)  = 0.
     hgamq(i)  = 0.
     wscale(i) = 0.
     kpbl(i)   = 1
     hpbl(i)   = zq(i,1)
     zl1(i)    = za(i,1)
     thermal(i)= thvx(i,1)
     thermalli(i) = thlix(i,1)
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
     if(sfcflg(i))then
       phim(i) = (1.-aphi16*hol1)**(-1./4.)
       phih(i) = (1.-aphi16*hol1)**(-1./2.)
       bfx0 = max(sflux(i),0.)
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
       thermalli(i)= thermalli(i)+max(vpert,0.)*min(za(i,1)/(sfcfrac*hpbl(i)),1.0)
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
!     enhance pbl by theta-li
!
   if (ysu_topdown_pblmix)then
     do i = its,ite
        kpblold(i) = kpbl(i)
        definebrup=.false.
        do k = kpblold(i), kte-1
           spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
           bruptmp = (thlix(i,k)-thermalli(i))*(g*za(i,k)/thlix(i,1))/spdk2
           stable(i) = bruptmp.ge.brcr(i)
           if (definebrup) then
           kpbl(i) = k
           brup(i) = bruptmp
           definebrup=.false.
           endif
           if (.not.stable(i)) then !overwrite brup brdn values
           brdn(i)=bruptmp
           definebrup=.true.
           pblflg(i)=.true.
           endif
        enddo
     enddo
   endif

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
     endif
   enddo
!
!     stable boundary layer
!
   do i = its,ite
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
!     estimate the entrainment parameters
!
   do i = its,ite
     cloudflg(i)=.false. 
     if(pblflg(i)) then
       k = kpbl(i) - 1
       wm3       = wstar3(i) + 5. * ust3(i)
       wm2(i)    = wm3**h2
       bfxpbl(i) = -0.15*thvx(i,1)/g*wm3/hpbl(i)
       dthvx(i)  = max(thvx(i,k+1)-thvx(i,k),tmin)
       we(i) = max(bfxpbl(i)/dthvx(i),-sqrt(wm2(i)))
       if((qcxl(i,k)+qixl(i,k)).gt.0.01e-3.and.ysu_topdown_pblmix)then
           if ( kpbl(i) .ge. 2) then
                cloudflg(i)=.true. 
                templ=thlix(i,k)*(p2di(i,k+1)/100000)**rovcp
                !rvls is ws at full level
                rvls=100.*6.112*EXP(17.67*(templ-273.16)/(templ-29.65))*(ep2/p2di(i,k+1))
                temps=templ + ((qvx(i,k)+qcxl(i,k))-rvls)/(cp/xlv  + &
                ep2*xlv*rvls/(rd*templ**2))
                rvls=100.*6.112*EXP(17.67*(temps-273.15)/(temps-29.65))*(ep2/p2di(i,k+1))
                rcldb=max((qvx(i,k)+qcxl(i,k))-rvls,0.)
                !entrainment efficiency
                dthvx(i)  = (thlix(i,k+2)+thx(i,k+2)*ep1*(qvx(i,k+2)+qcxl(i,k+2))) &
                          - (thlix(i,k) + thx(i,k)  *ep1*(qvx(i,k)  +qcxl(i,k)))
                dthvx(i)  = max(dthvx(i),0.1)
                tmp1      = xlv/cp * rcldb/(pi2d(i,k)*dthvx(i))
                ent_eff   = 0.2 * 8. * tmp1 +0.2

                radsum=0.
                do kk = 1,kpbl(i)-1
                   radflux=rthraten(i,kk)*pi2d(i,kk) !converts theta/s to temp/s
                   radflux=radflux*cp/g*(p2di(i,kk)-p2di(i,kk+1)) ! converts temp/s to W/m^2
                   if (radflux < 0.0 ) radsum=abs(radflux)+radsum
                enddo
                radsum=max(radsum,0.0)

                !recompute entrainment from sfc thermals
                bfx0 = max(max(sflux(i),0.0)-radsum/rhox2(i,k)/cp,0.)
                bfx0 = max(sflux(i),0.0)
                wm3 = (govrth(i)*bfx0*hpbl(i))+5. * ust3(i)
                wm2(i)    = wm3**h2
                bfxpbl(i) = -0.15*thvx(i,1)/g*wm3/hpbl(i)
                dthvx(i)  = max(thvx(i,k+1)-thvx(i,k),tmin)
                we(i) = max(bfxpbl(i)/dthvx(i),-sqrt(wm2(i)))

                !entrainment from PBL top thermals
                bfx0 = max(radsum/rhox2(i,k)/cp-max(sflux(i),0.0),0.)
                bfx0 = max(radsum/rhox2(i,k)/cp,0.)
                wm3       = (g/thvx(i,k)*bfx0*hpbl(i)) ! this is wstar3(i)
                wm2(i)    = wm2(i)+wm3**h2
                bfxpbl(i) = - ent_eff * bfx0
                dthvx(i)  = max(thvx(i,k+1)-thvx(i,k),0.1)
                we(i) = we(i) + max(bfxpbl(i)/dthvx(i),-sqrt(wm3**h2))

                !wstar3_2
                bfx0 = max(radsum/rhox2(i,k)/cp,0.)
                wstar3_2(i) =  (g/thvx(i,k)*bfx0*hpbl(i))
                !recompute hgamt 
                wscale(i) = (ust3(i)+phifac*karman*(wstar3(i)+wstar3_2(i))*0.5)**h1
                wscale(i) = min(wscale(i),ust(i)*aphi16)
                wscale(i) = max(wscale(i),ust(i)/aphi5)
                gamfac   = bfac/rhox(i)/wscale(i)
                hgamt(i) = min(gamfac*hfx(i)/cp,gamcrt)
                hgamq(i) = min(gamfac*qfx(i),gamcrq)
                gamfac   = bfac/rhox2(i,k)/wscale(i)
                hgamt2(i,k) = min(gamfac*radsum/cp,gamcrt)
                hgamt(i) = max(hgamt(i),0.0) + max(hgamt2(i,k),0.0)
                brint    = -15.9*ust(i)*ust(i)/wspd(i)*(wstar3(i)+wstar3_2(i))/(wscale(i)**4.)
                hgamu(i) = brint*ux(i,1)
                hgamv(i) = brint*vx(i,1)
           endif
       endif
       prpbl(i) = 1.0
       dthx  = max(thx(i,k+1)-thx(i,k),tmin)
       dqx   = min(qvx(i,k+1)-qvx(i,k),0.0)
       hfxpbl(i) = we(i)*dthx
       qfxpbl(i) = we(i)*dqx
!
       dux = ux(i,k+1)-ux(i,k)
       dvx = vx(i,k+1)-vx(i,k)
       if(dux.gt.tmin) then
         ufxpbl(i) = max(prpbl(i)*we(i)*dux,-ust(i)*ust(i))
       elseif(dux.lt.-tmin) then
         ufxpbl(i) = min(prpbl(i)*we(i)*dux,ust(i)*ust(i))
       else
         ufxpbl(i) = 0.0
       endif
       if(dvx.gt.tmin) then
         vfxpbl(i) = max(prpbl(i)*we(i)*dvx,-ust(i)*ust(i))
       elseif(dvx.lt.-tmin) then
         vfxpbl(i) = min(prpbl(i)*we(i)*dvx,ust(i)*ust(i))
       else
         vfxpbl(i) = 0.0
       endif
       delb  = govrth(i)*d3*hpbl(i)
       delta(i) = min(d1*hpbl(i) + d2*wm2(i)/delb,100.)
     endif
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       if(pblflg(i).and.k.ge.kpbl(i))then
         entfac(i,k) = ((zq(i,k+1)-hpbl(i))/delta(i))**2.
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
         wscalek2(i,k) = (phifac*karman*wstar3_2(i)*(zfac(i,k)))**h1
         if(sfcflg(i)) then
           prfac = conpr
           prfac2 = 15.9*(wstar3(i)+wstar3_2(i))/ust3(i)/(1.+4.*karman*(wstar3(i)+wstar3_2(i))/ust3(i))
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
           xkzm(i,k) = wscalek(i,k) *karman*    zq(i,k+1)      *    zfac(i,k)**pfac+ &
                       wscalek2(i,k)*karman*(hpbl(i)-zq(i,k+1))*(1-zfac(i,k))**pfac
         !Do not include xkzm at kpbl-1 since it changes entrainment
         if (k.eq.kpbl(i)-1.and.cloudflg(i).and.we(i).lt.0.0) then
           xkzm(i,k) = 0.0
         endif
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
         if(imvdif.eq.1)then
           if((qcxl(i,k)+qixl(i,k)).gt.0.01e-3.and. &
              (qcxl(i,k+1)+qixl(i,k+1)).gt.0.01e-3)then
!      in cloud
             qmean = 0.5*(qvx(i,k)+qvx(i,k+1))
             tmean = 0.5*(tx(i,k)+tx(i,k+1))
             alph  = xlv*qmean/rd/tmean
             chi   = xlv*xlv*qmean/cp/rv/tmean/tmean
             ri    = (1.+alph)*(ri-g*g/ss/tmean/cp*((chi-alph)/(1.+chi)))
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
     f1(i,1) = thx(i,1)-300.+(1.0-bepswitch)*hfx(i)/cont/del(i,1)*dt2
   enddo
!
   do k = kts,kte-1
     do i = its,ite
       dtodsd = sfk2d(i,k)*dt2/del(i,k)
       dtodsu = sfk2d(i,k)*dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzh(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i)) then
         dsdzt = tem1*(-hgamt(i)/hpbl(i)-hfxpbl(i)*zfacent(i,k)/xkzh(i,k))
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
       au(i,k)   = -dtodsd*dsdz2/vlk2d(i,k)
       al(i,k)   = -dtodsu*dsdz2/vlk2d(i,k)
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
       exch_hx(i,k+1) = xkzh(i,k)
     enddo
   enddo
!
! add bep/bep+bem forcing for heat if flag_bep=.true.
!
   do k = kts,kte
     do i = its,ite
      ad(i,k) = ad(i,k) - a_t2d(i,k)*dt2
      f1(i,k) = f1(i,k) + b_t2d(i,k)*dt2
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
#if (NEED_B4B_DURING_CCPP_TESTING == 1)
       ttend = (f1(i,k)-thx(i,k)+300.)*rdt*pi2d(i,k)
       ttnp(i,k) = ttend
       if(present(dtsfc)) dtsfc(i) = dtsfc(i)+ttend*cont*del(i,k)/pi2d(i,k)
#elif (NEED_B4B_DURING_CCPP_TESTING != 1)
       ttend = (f1(i,k)-thx(i,k)+300.)*rdt
       ttnp(i,k) = ttend
       if(present(dtsfc)) dtsfc(i) = dtsfc(i)+ttend*cont*del(i,k)
#endif
     enddo
   enddo
!

!--- compute tridiagonal matrix elements for water vapor, cloud water, and cloud ice:
   !--- initialization of k-coefficient above the PBL.
   do i = its,ite
      do k = kts,kte-1
         if(k .ge. kpbl(i)) xkzq(i,k) = xkzh(i,k)
      enddo
   enddo

   !--- water vapor:
   do i = its,ite
      do k = kts,kte
         au(i,k) = 0.
         al(i,k) = 0.
         ad(i,k) = 0.
         f1(i,k) = 0.
         r1(i,k) = 0.
      enddo

      k = 1
      ad(i,1) = 1.
      f1(i,1) = qvx(i,1)+(1.0-bepswitch)*qfx(i)*g/del(i,1)*dt2

      do k = kts,kte-1
         dtodsd = sfk2d(i,k)*dt2/del(i,k)
         dtodsu = sfk2d(i,k)*dt2/del(i,k+1)
         dsig   = p2d(i,k)-p2d(i,k+1)
         rdz    = 1./dza(i,k+1)
         tem1   = dsig*xkzq(i,k)*rdz
         if(pblflg(i).and.k.lt.kpbl(i)) then
            dsdzq = tem1*(-qfxpbl(i)*zfacent(i,k)/xkzq(i,k))
            f1(i,k) = f1(i,k)+dtodsd*dsdzq
            f1(i,k+1) = qvx(i,k+1)-dtodsu*dsdzq
         elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
            xkzq(i,k) = -we(i)*dza(i,kpbl(i))*exp(-entfac(i,k))
            xkzq(i,k) = sqrt(xkzq(i,k)*xkzhl(i,k))
            xkzq(i,k) = max(xkzq(i,k),xkzoh(i,k))
            xkzq(i,k) = min(xkzq(i,k),xkzmax)
            f1(i,k+1) = qvx(i,k+1)
         else
            f1(i,k+1) = qvx(i,k+1)
         endif
         tem1      = dsig*xkzq(i,k)*rdz
         dsdz2     = tem1*rdz
         au(i,k)   = -dtodsd*dsdz2/vlk2d(i,k)
         al(i,k)   = -dtodsu*dsdz2/vlk2d(i,k)
         ad(i,k)   = ad(i,k)-au(i,k)
         ad(i,k+1) = 1.-al(i,k)
      enddo
!
! add bep/bep+bem forcing for water vapor if flag_bep=.true.
!
      do k = kts,kte
         adv(i,k) = ad(i,k) - a_q2d(i,k)*dt2
         f1(i,k)  = f1(i,k) + b_q2d(i,k)*dt2
      enddo

      do k = kts,kte
         cu(i,k) = au(i,k)
         r1(i,k) = f1(i,k)
      enddo
   enddo
   call tridin_ysu(al,adv,cu,r1,au,f1,its,ite,kts,kte,1)

   do i = its,ite
      do k = kte,kts,-1
         qtend = (f1(i,k)-qvx(i,k))*rdt
         qvtnp(i,k) = qtend
         if(present(dqsfc)) dqsfc(i)   = dqsfc(i)+qtend*conq*del(i,k)
      enddo
   enddo

   !--- cloud water:
   if(f_qc) then
      do i = its,ite
         do k = kts,kte
            f1(i,k) = qcxl(i,k)
            r1(i,k) = f1(i,k)
         enddo
      enddo
      call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1)

      do i = its,ite
         do k = kte,kts,-1
            qtend = (f1(i,k)-qcxl(i,k))*rdt
            qctnp(i,k) = qtend
         enddo
      enddo
   endif

   !--- cloud ice:
   if(f_qi) then
      do i = its,ite
         do k = kts,kte
            f1(i,k) = qixl(i,k)
            r1(i,k) = f1(i,k)
         enddo
      enddo
      call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1)

      do i = its,ite
         do k = kte,kts,-1
            qtend = (f1(i,k)-qixl(i,k))*rdt
            qitnp(i,k) = qtend
         enddo
      enddo
   endif

   !--- chemical species and/or passive tracers, meaning all variables that we want to
   !    be vertically-mixed, if nmix=0 (number of tracers) then the loop is skipped
   do n = 1, nmix
      do i = its,ite
         do k = kts,kte
            f1(i,k) = qmix(i,k,n)
            r1(i,k) = f1(i,k)
         enddo
      enddo
      call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1)

      do i = its,ite
         do k = kte,kts,-1
            qtend = (f1(i,k)-qmix(i,k,n))*rdt
            qmixtnp(i,k,n) = qtend
         enddo
      enddo
   enddo

!
!  compute tridiagonal matrix elements for momentum
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
! paj: ctopo=1 if topo_wind=0 (default)
!raquel---paj tke code (could be replaced with shin-hong tke in future
   do i = its,ite
      do k= kts, kte-1
        shear_ysu(i,k)=xkzm(i,k)*((-hgamu(i)/hpbl(i)+(ux(i,k+1)-ux(i,k))/dza(i,k+1))*(ux(i,k+1)-ux(i,k))/dza(i,k+1) &
        + (-hgamv(i)/hpbl(i)+(vx(i,k+1)-vx(i,k))/dza(i,k+1))*(vx(i,k+1)-vx(i,k))/dza(i,k+1))
         buoy_ysu(i,k)=xkzh(i,k)*g*(1.0/thx(i,k))*(-hgamt(i)/hpbl(i)+(thx(i,k+1)-thx(i,k))/dza(i,k+1))

       zk = karman*zq(i,k+1)
 !over pbl
       if (k.ge.kpbl(i)) then
        rlamdz = min(max(0.1*dza(i,k+1),rlam),300.)
        rlamdz = min(dza(i,k+1),rlamdz)
       else
 !in pbl
        rlamdz = 150.0
       endif
       el_ysu(i,k) = zk*rlamdz/(rlamdz+zk)
       tke_ysu(i,k)=16.6*el_ysu(i,k)*(shear_ysu(i,k)-buoy_ysu(i,k))
 !q2 when q3 positive
       if(tke_ysu(i,k).le.0) then
        tke_ysu(i,k)=0.0
       else
        tke_ysu(i,k)=(tke_ysu(i,k))**0.66
       endif
      enddo
 !Hybrid pblh of MYNN
 !tke is q2
!     CALL GET_PBLH(KTS,KTE,pblh_ysu(i),thvx(i,kts:kte),&
!     &    tke_ysu(i,kts:kte),zq(i,kts:kte+1),dzq(i,kts:kte),xland(i))
      do k = kts,kte
         thvx_1d(k) = thvx(i,k)
         tke_1d(k)  = tke_ysu(i,k)
         zq_1d(k)   = zq(i,k)
         dzq_1d(k)  = dzq(i,k)
      enddo
      zq_1d(kte+1) = zq(i,kte+1)
      call get_pblh(kts,kte,pblh_ysu(i),thvx_1d,tke_1d,zq_1d,dzq_1d,xland(i))

!--- end of paj tke
! compute vconv
!      Use Beljaars over land
        if (xland(i).lt.1.5) then
        fluxc = max(sflux(i),0.0)
        vconvc=1.
        VCONV = vconvc*(g/thvx(i,1)*pblh_ysu(i)*fluxc)**.33
        else
! for water there is no topo effect so vconv not needed
        VCONV = 0.
        endif
        vconvfx(i) = vconv
!raquel
!ctopo stability correction
      fric(i,1)=ust(i)**2/wspd1(i)*rhox(i)*g/del(i,1)*dt2         &
        *(wspd1(i)/wspd(i))**2
      if(present(ctopo)) then
        vconvnew=0.9*vconvfx(i)+1.5*(max((pblh_ysu(i)-500)/1000.0,0.0))
        vconvlim = min(vconvnew,1.0)
        ad(i,1) = 1.+fric(i,1)*vconvlim+ctopo(i)*fric(i,1)*(1-vconvlim)
        ad(i,1) = ad(i,1) - bepswitch*frc_urb1d(i)* &
                     (fric(i,1)*vconvlim+ctopo(i)*fric(i,1)*(1-vconvlim))
!       ad(i,1) = 1.+(1.-bepswitch*frc_urb1d(i))* &
!                    (fric(i,1)*vconvlim+ctopo(i)*fric(i,1)*(1-vconvlim))
      else
        ad(i,1) = 1.+fric(i,1)
      endif
     f1(i,1) = ux(i,1)+uoxl(i)*ust(i)**2*rhox(i)*g/del(i,1)*dt2/wspd1(i)*(wspd1(i)/wspd(i))**2
     f2(i,1) = vx(i,1)+voxl(i)*ust(i)**2*rhox(i)*g/del(i,1)*dt2/wspd1(i)*(wspd1(i)/wspd(i))**2
   enddo
!
   do k = kts,kte-1
     do i = its,ite
       dtodsd = sfk2d(i,k)*dt2/del(i,k)
       dtodsu = sfk2d(i,k)*dt2/del(i,k+1)
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
       au(i,k)   = -dtodsd*dsdz2/vlk2d(i,k)
       al(i,k)   = -dtodsu*dsdz2/vlk2d(i,k)
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
       exch_mx(i,k+1) = xkzm(i,k)
     enddo
   enddo
!
! add bep/bep+bem forcing for momentum if flag_bep=.true.
!
   do k = kts,kte
     do i = its,ite
       ad1(i,k) = ad(i,k)
     end do
   end do
   do k = kts,kte
     do i = its,ite
       ad(i,k) = ad(i,k) - a_u2d(i,k)*dt2
       ad1(i,k) = ad1(i,k) - a_v2d(i,k)*dt2
       f1(i,k) = f1(i,k) + b_u2d(i,k)*dt2
       f2(i,k) = f2(i,k) + b_v2d(i,k)*dt2
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
   call tridi2n(al,ad,ad1,cu,r1,r2,au,f1,f2,its,ite,kts,kte,1)
!
!     recover tendencies of momentum
!
   do k = kte,kts,-1
     do i = its,ite
       utend = (f1(i,k)-ux(i,k))*rdt
       vtend = (f2(i,k)-vx(i,k))*rdt
       utnp(i,k) = utend
       vtnp(i,k) = vtend
       if(present(dusfc)) dusfc(i) = dusfc(i) + utend*conwrc*del(i,k)
       if(present(dvsfc)) dvsfc(i) = dvsfc(i) + vtend*conwrc*del(i,k)
     enddo
   enddo
!
! paj: ctopo2=1 if topo_wind=0 (default)
!
   do i = its,ite
     if(present(ctopo).and.present(ctopo2)) then ! mchen for NMM
       u10(i) = ctopo2(i)*u10(i)+(1-ctopo2(i))*ux(i,1)
       v10(i) = ctopo2(i)*v10(i)+(1-ctopo2(i))*vx(i,1)
     endif !mchen
   enddo
!
!---- end of vertical diffusion
!
   do i = its,ite
     kpbl1d(i) = kpbl(i)
   enddo
!
   errmsg = 'bl_ysu_run OK'
   errflg = 0
!
   end subroutine bl_ysu_run

!=================================================================================================================
   subroutine tridi2n(cl,cm,cm1,cu,r1,r2,au,f1,f2,its,ite,kts,kte,nt)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer, intent(in )      ::     its,ite, kts,kte, nt
!
   real(kind=kind_phys), dimension( its:ite, kts+1:kte+1 )                   , &
         intent(in   )  ::                                                 cl
!
   real(kind=kind_phys), dimension( its:ite, kts:kte )                       , &
         intent(in   )  ::                                                 cm, &
                                                                          cm1, &
                                                                           r1
   real(kind=kind_phys), dimension( its:ite, kts:kte,nt )                    , &
         intent(in   )  ::                                                 r2
!
   real(kind=kind_phys), dimension( its:ite, kts:kte )                       , &
         intent(inout)  ::                                                 au, &
                                                                           cu, &
                                                                           f1
   real(kind=kind_phys), dimension( its:ite, kts:kte,nt )                    , &
         intent(inout)  ::                                                 f2
!
   real(kind=kind_phys) :: fk
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
       fk = 1./cm1(i,1)
       f2(i,1,it) = fk*r2(i,1,it)
     enddo
   enddo

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
         fk = 1./(cm1(i,k)-cl(i,k)*au(i,k-1))
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
       fk = 1./(cm1(i,n)-cl(i,n)*au(i,n-1))
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
   end subroutine tridi2n
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
   real(kind=kind_phys), dimension( its:ite, kts+1:kte+1 )                   , &
         intent(in   )  ::                                                 cl
!
   real(kind=kind_phys), dimension( its:ite, kts:kte )                       , &
         intent(in   )  ::                                                 au, &
                                                                           cm, &
                                                                           cu
   real(kind=kind_phys), dimension( its:ite, kts:kte,nt )                    , &
         intent(in   )  ::                                                 r2

   real(kind=kind_phys), dimension( its:ite, kts:kte,nt )                    , &
         intent(inout)  ::                                                 f2
!
   real(kind=kind_phys) :: fk
   real(kind=kind_phys), dimension( its:ite, kts:kte ) ::                 aul
   integer :: i,k,l,n,it
!
!-------------------------------------------------------------------------------
!
   l = ite
   n = kte
!
   do  i = its,ite
     do k = kts,kte
       aul(i,k) = 0.
     enddo
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./cm(i,1)
       aul(i,1) = fk*cu(i,1)
       f2(i,1,it) = fk*r2(i,1,it)
     enddo
   enddo
!
   do it = 1,nt
     do k = kts+1,n-1
       do i = its,l
         fk = 1./(cm(i,k)-cl(i,k)*aul(i,k-1))
         aul(i,k) = fk*cu(i,k)
         f2(i,k,it) = fk*(r2(i,k,it)-cl(i,k)*f2(i,k-1,it))
       enddo
     enddo
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./(cm(i,n)-cl(i,n)*aul(i,n-1))
       f2(i,n,it) = fk*(r2(i,n,it)-cl(i,n)*f2(i,n-1,it))
     enddo
   enddo
!
   do it = 1,nt
     do k = n-1,kts,-1
       do i = its,l
         f2(i,k,it) = f2(i,k,it)-aul(i,k)*f2(i,k+1,it)
       enddo
     enddo
   enddo
!
   end subroutine tridin_ysu

!=================================================================================================================
      subroutine get_pblh(kts,kte,zi,thetav1d,qke1d,zw1d,dz1d,landsea)
! Copied from MYNN PBL

      !---------------------------------------------------------------
      !             NOTES ON THE PBLH FORMULATION
      !
      !The 1.5-theta-increase method defines PBL heights as the level at
      !which the potential temperature first exceeds the minimum potential
      !temperature within the boundary layer by 1.5 K. When applied to
      !observed temperatures, this method has been shown to produce PBL-
      !height estimates that are unbiased relative to profiler-based
      !estimates (Nielsen-Gammon et al. 2008). However, their study did not
      !include LLJs. Banta and Pichugina (2008) show that a TKE-based
      !threshold is a good estimate of the PBL height in LLJs. Therefore,
      !a hybrid definition is implemented that uses both methods, weighting
      !the TKE-method more during stable conditions (PBLH < 400 m).
      !A variable tke threshold (TKEeps) is used since no hard-wired
      !value could be found to work best in all conditions.
      !---------------------------------------------------------------

      integer,intent(in) :: kts,kte
      real(kind=kind_phys), intent(out) :: zi
      real(kind=kind_phys), intent(in) :: landsea
      real(kind=kind_phys), dimension(kts:kte), intent(in) :: thetav1d, qke1d, dz1d
      real(kind=kind_phys), dimension(kts:kte+1), intent(in) :: zw1d
      !local vars
      real(kind=kind_phys) ::  pblh_tke,qtke,qtkem1,wt,maxqke,tkeeps,minthv
      real(kind=kind_phys) :: delt_thv   !delta theta-v; dependent on land/sea point
      real(kind=kind_phys), parameter :: sbl_lim  = 200. !theta-v pbl lower limit of trust (m).
      real(kind=kind_phys), parameter :: sbl_damp = 400. !damping range for averaging with tke-based pblh (m).
      integer :: i,j,k,kthv,ktke

      !find max tke and min thetav in the lowest 500 m
      k = kts+1
      kthv = 1
      ktke = 1
      maxqke = 0.
      minthv = 9.e9

      do while (zw1d(k) .le. 500.)
        qtke  =max(qke1d(k),0.)   ! maximum qke
         if (maxqke < qtke) then
            maxqke = qtke
            ktke = k
         endif
         if (minthv > thetav1d(k)) then
             minthv = thetav1d(k)
             kthv = k
         endif
         k = k+1
      enddo
      !tkeeps = maxtke/20. = maxqke/40.
      tkeeps = maxqke/40.
      tkeeps = max(tkeeps,0.025)
      tkeeps = min(tkeeps,0.25)

      !find thetav-based pblh (best for daytime).
      zi=0.
      k = kthv+1
      if((landsea-1.5).ge.0)then
      ! water
          delt_thv = 0.75
      else
      ! land
          delt_thv = 1.5
      endif

      zi=0.
      k = kthv+1
      do while (zi .eq. 0.)
         if (thetav1d(k) .ge. (minthv + delt_thv))then
            zi = zw1d(k) - dz1d(k-1)* &
              & min((thetav1d(k)-(minthv + delt_thv))/max(thetav1d(k)-thetav1d(k-1),1e-6),1.0)
        endif
        k = k+1
         if (k .eq. kte-1) zi = zw1d(kts+1) !exit safeguard
      enddo

      !print*,"in get_pblh:",thsfc,zi
      !for stable boundary layers, use tke method to complement the
      !thetav-based definition (when the theta-v based pblh is below ~0.5 km).
      !the tanh weighting function will make the tke-based definition negligible
      !when the theta-v-based definition is above ~1 km.
      !find tke-based pblh (best for nocturnal/stable conditions).

      pblh_tke=0.
      k = ktke+1
     do while (pblh_tke .eq. 0.)
        !qke can be negative (if ckmod == 0)... make tke non-negative.
         qtke  =max(qke1d(k)/2.,0.)      ! maximum tke
         qtkem1=max(qke1d(k-1)/2.,0.)
         if (qtke .le. tkeeps) then
               pblh_tke = zw1d(k) - dz1d(k-1)* &
               & min((tkeeps-qtke)/max(qtkem1-qtke, 1e-6), 1.0)
             !in case of near zero tke, set pblh = lowest level.
             pblh_tke = max(pblh_tke,zw1d(kts+1))
             !print *,"pblh_tke:",i,j,pblh_tke, qke1d(k)/2., zw1d(kts+1)
         endif
         k = k+1
         if (k .eq. kte-1) pblh_tke = zw1d(kts+1) !exit safeguard
      enddo

    !blend the two pblh types here:

      wt=.5*tanh((zi - sbl_lim)/sbl_damp) + .5
      zi=pblh_tke*(1.-wt) + zi*wt

   end subroutine get_pblh

!=================================================================================================================
 end module bl_ysu
!=================================================================================================================
