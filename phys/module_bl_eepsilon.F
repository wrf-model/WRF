!WRF:model_layer:physics
!
!####################e-epsilon scheme##################################
!   taken from the IPRC IRAM - Yuqing Wang, university of hawaii
!   added and improved by Chunxi Zhang and Yuqing Wang to wrf4.2, 2020
!   refenrences: Langland and Liou (1996, mwr, 124, 905-918)
!                Zhang et al.      (2020, mwr, 148, 1121-1145)
!######################################################################
module module_bl_eeps

   use module_model_constants, only:rgas=>r_d,   &
   &       t13=>Prandtl,ep1=>EP_1,ep2=>EP_2,grv=>g, &
   &       akv=>KARMAN,cp,rcp,xlv,xls,xlf,p1000mb
     
   real, private, parameter :: cs2=17.67,cs3=273.15,cs4=29.65
   real, private, parameter :: ci2=22.514,ci3=273.16,ci4=0.0
   real, private, parameter :: hgfr=233.15,t0=273.15
   real, private, parameter :: epsl=1.e-20
   real, private, parameter :: us2min=0.1
   real, private, parameter :: akvmx = 1600.
   real, private, parameter :: ricr=0.25,bt=5.0
   real, private, parameter :: minpek = 1.0e-4
   real, private, parameter :: minpep = 1.0e-6
   real, private, parameter :: maxpek = 80.
   real, private, parameter :: maxpep = 2.0
   integer,private,parameter:: bl_eeps_tkeadv = 1
!---------------------------------------------------------------
! ci--- five constants used in TKE tubrbulence closure scheme
!---------------------------------------------------------------
! Detering and Etling (1986); Langland and Liou (1996)
!   real,parameter :: c1=1.35,c2=0.026,c3=1.13,c4=1.90,c5=0.77
! Duynkerke and Driedonks (1987); Stubley and Rooney (1986)
   real,parameter :: c1=1.35,c2=0.09,c3=1.44,c4=1.92,c5=0.77
! Beljaars et al. (1987)
!   real,parameter :: c1=1.35,c2=0.032,c3=1.44,c4=1.92,c5=0.54
!
contains
!-------------------------------------------------------------------------------
!
   subroutine eeps(u3d,v3d,t3d,qv3d,qc3d,qi3d,qr3d,qs3d,qg3d,p3d,pi3d,    &
                  rublten,rvblten,rthblten,                               &
                  rqvblten,rqcblten,rqiblten,                             &
                  pek,pep,                                                &
                  dz8w,psfc,qsfc,tsk,ust,rmol,wspd,                       &
                  xland,hfx,qfx,                                          &
                  dt,dx,itimestep,exch_h,exch_m,pblh,kpbl,                &
                  pek_adv,pep_adv,                                        &
                  ids,ide, jds,jde, kds,kde,                              &
                  ims,ime, jms,jme, kms,kme,                              &
                  its,ite, jts,jte, kts,kte                               &
                                                      )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- qc3d        3d cloud mixing ratio (kg/kg)
!-- qi3d        3d ice mixing ratio (kg/kg)
!-- qr3d        3d rain mixing ratio (kg/kg)
!-- qs3d        3d snow mixing ratio (kg/kg)
!-- qg3d        3d graupel mixing ratio (kg/kg)
!               (note: if P_QI<PARAM_FIRST_SCALAR this should be zero filled)
!-- p3d         3d pressure (pa)
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
!-- dz8w        dz between full levels (m)
!-- pek         3d turbulent kinetic energy (TKE, m2/s2)
!-- pep         3d dissipation rate of TKE 
!-- psfc        pressure at the surface (pa)
!-- qsfc        ground saturated mixing ratio
!-- ust         u* in similarity theory (m/s)
!-- rmol        inverse (1/L) of Monin-Obukhov length (m)
!-- xland       land mask (1 for land, 2 for water)
!-- tsk         surface skin temperature
!-- hfx         upward heat flux at the surface (w/m^2)
!-- qfx         upward moisture flux at the surface (kg/m^2/s)
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
   integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte

   integer,  intent(in   )   ::      itimestep

   real,     intent(in   )   ::      dt, dx
!
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          qv3d, &
                                                                         qc3d, &
                                                                         qi3d, &
                                                                          p3d, &
                                                                         pi3d, &
                                                                          t3d, &
                                                                         dz8w, &
                                                                          u3d, &
                                                                          v3d
   
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             optional                                                        , &
             intent(in   )   ::                                          qr3d, &
                                                                         qs3d, &
                                                                         qg3d

   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                       rublten, &
                                                                      rvblten, &
                                                                     rthblten, &
                                                                     rqvblten, &
                                                                     rqcblten, &
                                                                     rqiblten
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx, &
                                                                         psfc, &
                                                                         qsfc, &
                                                                          tsk, &
                                                                          ust, &
                                                                          rmol,&
                                                                          wspd
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                       pek, pep

   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(inout)   ::                                 pek_adv,pep_adv

   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(out)   ::                                    exch_h,exch_m

   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(out   )   ::                                          pblh

   integer,  dimension( ims:ime, jms:jme )                                   , &
             intent(out   )   ::                                          kpbl
!
!local
      real,     dimension( its:ite, kts:kte+1)::              zi,ghti  
      real,     dimension( its:ite, kts:kte ) ::              zl                                                   
      
      real,     dimension( its:ite, kts:kte ) ::              u1, &
                                                              v1, &
                                                              q1, &
                                                              q2, &
                                                              q3, &
                                                              q4, &
                                                              q5, &
                                                              q6, &
                                                              t1, &
                                                              ghtl, &
                                                              prsl
      real,     dimension( its:ite, kts:kte ) ::          pek2d,pep2d
      real,     dimension( its:ite, kts:kte+1 ) ::        exh2d,exm2d
      real,     dimension( its:ite) ::               psfc1,ust1,rmol1,  &
                                                     xland1,qsfc1,hfx1, &
                                                     qfx1,tsk1,pblh1,wspd1
      integer,  dimension( its:ite) ::               kpbl1

      real    :: rdelt
      integer ::  i,j,k,zz,im,kx,pp

      im=ite-its+1
      kx=kte-kts+1
      rdelt = 1./dt

      if(itimestep .eq. 1) then
         pek_adv = pek
         pep_adv = pep
      end if

      do j=jts,jte
! --------------- compute zi and zl -----------------------------------------
      do i=its,ite
        zi(i,kts)=0.0
      enddo
!
      do k=kts,kte
        do i=its,ite
          zi(i,k+1)=zi(i,k)+dz8w(i,k,j)
        enddo
      enddo
!
      do k=kts,kte
        do i=its,ite
          zl(i,k)=0.5*(zi(i,k)+zi(i,k+1))
        enddo
      enddo

! reverse the vertical levels
      pp = 0
      do k=kts,kte
        zz = kte-pp
        do i=its,ite
          u1(i,zz)=u3d(i,k,j)
          v1(i,zz)=v3d(i,k,j)
          t1(i,zz)=t3d(i,k,j)
          q1(i,zz)=qv3d(i,k,j)/(1.+qv3d(i,k,j))
          q2(i,zz)=qc3d(i,k,j)/(1.+qc3d(i,k,j))
          q3(i,zz)=qi3d(i,k,j)/(1.+qi3d(i,k,j))
          if(present(qr3d)) then
            q4(i,zz)=qr3d(i,k,j)/(1.+qr3d(i,k,j))
          else
            q4(i,zz)=0.
          end if
          if(present(qs3d)) then
            q5(i,zz)=qs3d(i,k,j)/(1.+qs3d(i,k,j))
          else
            q5(i,zz)=0.
          end if
          if(present(qg3d)) then
            q6(i,zz)=qg3d(i,k,j)/(1.+qg3d(i,k,j))
          else
            q6(i,zz)=0.
          end if
          ghtl(i,zz)=zl(i,k)
          prsl(i,zz) = p3d(i,k,j)
          if(bl_eeps_tkeadv==1)then
            if(k.ne.1) then 
              pek(i,k,j) = 0.5*(pek_adv(i,k,j)+pek_adv(i,k-1,j))
              pep(i,k,j) = 0.5*(pep_adv(i,k,j)+pep_adv(i,k-1,j))
            else
              pek(i,k,j) = minpek
              pep(i,k,j) = minpep
            end if
          end if
          pek2d(i,zz)= min(maxpek,max(minpek,pek(i,k,j)))
          pep2d(i,zz)= min(maxpep,max(minpep,pep(i,k,j)))
        end do
        pp = pp + 1
      end do

       pp = 0
       do k=kts,kte+1
        zz = kte+1-pp
        do i=its,ite
          ghti(i,zz) = zi(i,k)
        enddo
        pp = pp + 1
      enddo

      do i=its,ite
        psfc1(i) = psfc(i,j)
        ust1(i)  = ust(i,j)
        rmol1(i) = rmol(i,j)
        wspd1(i) = wspd(i,j)
        xland1(i)= xland(i,j)
        qsfc1(i) = qsfc(i,j)
        hfx1(i)  = hfx(i,j)
        qfx1(i)  = qfx(i,j)
        tsk1(i)  = tsk(i,j)
      end do
! 
      call eeps2d(u1,v1,t1,q1,q2,q3,q4,q5,q6,prsl,ghtl,ghti,pek2d,pep2d &
              ,psfc1,ust1,rmol1,wspd1,xland1,qsfc1,hfx1,qfx1,tsk1       &
              ,dt,dx,itimestep,exh2d,exm2d,pblh1,kpbl1,im,kx)
!
      pp = 0
      do k=kts,kte
        zz = kte-pp
        do i=its,ite
          rthblten(i,k,j)=(t1(i,zz)-t3d(i,k,j))/pi3d(i,k,j)*rdelt
          rqvblten(i,k,j)=(q1(i,zz)/(1.-q1(i,zz))-qv3d(i,k,j))*rdelt
          rqcblten(i,k,j)=(q2(i,zz)/(1.-q2(i,zz))-qc3d(i,k,j))*rdelt
          rqiblten(i,k,j)=(q3(i,zz)/(1.-q3(i,zz))-qi3d(i,k,j))*rdelt
          rublten(i,k,j) =(u1(i,zz)-u3d(i,k,j))*rdelt
          rvblten(i,k,j) =(v1(i,zz)-v3d(i,k,j))*rdelt

          pek(i,k,j)     = pek2d(i,zz)
          pep(i,k,j)     = pep2d(i,zz)
        enddo
        pp = pp + 1
      enddo

      pp = 0
      do k=kts,kte+1
        zz = kte+1-pp
        do i=its,ite
          exch_h(i,k,j)  = exh2d(i,zz)
          exch_m(i,k,j)  = exm2d(i,zz)
        enddo
        pp = pp + 1
      enddo

      do i=its,ite
        pblh(i,j) = pblh1(i)
        kpbl(i,j) = kpbl1(i)
      end do

      if(bl_eeps_tkeadv==1)then
        do k=kts,kte
          do i=its,ite
            if(k.ne.kte) then
              pek_adv(i,k,j) = 0.5*(pek(i,k,j)+pek(i,k+1,j))
              pep_adv(i,k,j) = 0.5*(pep(i,k,j)+pep(i,k+1,j))
            else
              pek_adv(i,k,j) = 0.5*pek(i,k,j)
              pep_adv(i,k,j) = 0.5*pep(i,k,j)
            end if
          end do
        end do
      end if
!
   enddo

!
   end subroutine eeps
!
!-------------------------------------------------------------------------------
!
   subroutine eeps2d(pu,pv,tz,pqv,pqc,pqi,pqr,pqs,pqg,prs,poz,zz,pek,pep,       &
                  psfcpa,ust,rmol,wspd,xland,qsfc,hfx,qfx,tsk,                  &
                  dt,dx,itimestep,k_h,k_m,pblh,kpbl,lq,km)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer,  intent(in   )   ::     itimestep,lq,km
   real,     intent(in   )   ::     dt, dx
!
   real,     dimension( lq )                                            , &
             intent(in)   ::                                              ust, &
                                                                        xland, &
                                                                          hfx, &
                                                                          qfx, &
                                                                          rmol,&
                                                                          wspd,&
                                                                   psfcpa,qsfc,&
                                                                           tsk
!
   real,     dimension( lq,km ),intent(inout)      ::          pu, &
                                                               pv, &
                                                               tz, &
                                                              pqv, &
                                                              pqc, &
                                                              pqi, &
                                                              pek, &
                                                              pep
  
   real,     dimension( lq,km ),intent(in)      ::            prs, &
                                                              poz, &
                                                              pqr, &
                                                              pqs, &
                                                              pqg
   real,     dimension( lq,km+1 ),intent(in)               ::  zz

   real,     dimension( lq,km+1 ),intent(out)              :: k_h,k_m
   real,     dimension( lq ),intent(out)                   ::  pblh    
   integer,  dimension( lq ),intent(out)                   ::  kpbl

! Local Vars
   real,     dimension( lq,km ) :: fac,psp,pt,ztv,szv,disht,up2
   real,     dimension( lq,km ) :: qvs,st,sh,rich,richb,dum2,dzz,doz
   real,     dimension( lq,km ) :: am,src,src1,ax,bx,cx,ac,ab,ba,alt
   real,     dimension( lq,km-1 ) :: ax1,bx1,cx1,yy1,amt
   real,     dimension( lq,km+1 ) :: akm,akh,pnt

   real,     dimension( lq)    :: tkm,zkm,qkm,us2,us,dens,wstar2,sflux
   real,     dimension( lq)    :: rm2,sfa,hs,hq
   real,     dimension( lq)    :: gamat,gamaq
   real,     dimension( lq,km) :: sgk1,sgk2 
   integer ::  j,k,mdt,lvl
   real    ::  dtt,dum,dum0,alh,eqp,qvsw,qvsi,faf,rdz,du,dv,dss
   real    ::  tk,qvk,alk,af,ag,aks,aco
   real    ::  duj,alv0,richf,rch,alv,ak,cpm
   real    ::  govrth,bfx0,wstar3,coef
   
   mdt = min(6,max(3,int(dt/10.0+0.1)))
   dtt = dt/mdt

!  dzz for dz between full levels, doz for dz between half levels
   do k=1,km
     do j=1,lq
       dzz(j,k) = zz(j,k)-zz(j,k+1)
       alt(j,k) = dt/dzz(j,k)
       up2(j,k)=grv/max(us2min,pu(j,k)*pu(j,k)+pv(j,k)*pv(j,k))
     enddo
   enddo

   do k=1,km-1
     do j=1,lq
       doz(j,k)  = poz(j,k)-poz(j,k+1)
     end do
   end do
!
   do j=1,lq
     doz(j,km)= 2.0*poz(j,km) ! not used
!     wstar2(j)= 0.0          ! it will be assigned values later
   enddo
!
   do k=1,km-1
     do j=1,lq
       amt(j,k) = dtt/doz(j,k)
     enddo
   enddo

   do k=1,km
     do j=1,lq
       sgk1(j,k) = 0.1
       sgk2(j,k) = 0.01
     end do
   end do
!
   do k=1,km
   do j=1,lq
      psp(j,k)=(prs(j,k)/p1000mb)**rcp
      pt(j,k)=tz(j,k)/psp(j,k)
      dum=1.0+ep1*pqv(j,k)
      ztv(j,k)=tz(j,k)*dum
      szv(j,k)=pt(j,k)*dum
      fac(j,k) = pqc(j,k) + pqi(j,k) + pqr(j,k) + pqs(j,k) + pqg(j,k)
      cx(j,k)=1.0+pqv(j,k)+fac(j,k)

      alh=3.1484e6-2.37e3*tz(j,k)
      eqp=611.2*exp(cs2*(tz(j,k)-cs3)/(tz(j,k)-cs4))
      eqp=min(0.5*prs(j,k),eqp)
      qvsw=0.622*eqp/(prs(j,k)-eqp)
      eqp=611.0*exp(ci2*(tz(j,k)-ci3)/(tz(j,k)-ci4))
      eqp=min(0.5*prs(j,k),eqp)
      qvsi=0.622*eqp/(prs(j,k)-eqp)
      if(tz(j,k).ge.t0) then
        faf=0.0
      else if(tz(j,k).le.hgfr) then
        faf=1.0
      else
        if(pqi(j,k).le.epsl) then
        faf=0.0
        else if(pqc(j,k).le.epsl) then
        faf=1.0
        else
        faf=pqi(j,k)/(pqc(j,k)+pqi(j,k))
        endif
      endif
      qvs(j,k)=(1.0-faf)*qvsw+faf*qvsi
      ax(j,k)=(1.0-faf)*alh+faf*xls
   end do
   end do

!
!  to calculate the Richardson number and vertical shear of
!  the horizontal wind above the surface layer
!
      do k=1,km-1
      do j=1,lq
        rdz=1.0/(poz(j,k)-poz(j,k+1))
        du=pu(j,k)-pu(j,k+1)
        dv=pv(j,k)-pv(j,k+1)
        sh(j,k)=(du*du+dv*dv)*(rdz*rdz)
        if((pqv(j,k).lt.qvs(j,k)).or.(pqv(j,k+1).lt.qvs(j,k+1))) then
          dss=log(szv(j,k)/szv(j,k+1))
          st(j,k)=grv*dss*rdz
        else
          dss=log(pt(j,k)/pt(j,k+1))
          tk=0.5*(tz(j,k+1)+tz(j,k))
          qvk=0.5*(pqv(j,k+1)+pqv(j,k))
          alk=0.5*(ax(j,k+1)+ax(j,k))
          af=alk*qvk/(rgas*tk)
          ag=alk/(cp*tk)
          aks=ep2*af*ag
          aco=(1.0+af)/(1.0+aks)
          st(j,k)=(aco*(dss+ag*(pqv(j,k)-pqv(j,k+1))) &
               -(cx(j,k)-cx(j,k+1)))*grv*rdz
        endif
        rich(j,k)=st(j,k)/max(1.0e-7,sh(j,k))
      end do
      end do

! calculate first guess pblh
      do k=1,km
      do j=1,lq
        richb(j,k)=poz(j,k)*(szv(j,k)/szv(j,km)-1.0)*up2(j,k)
      enddo
      enddo
!
      do j=1,lq
        pblh(j)=poz(j,km)
        kpbl(j)=1
        lvl=km
        do k=km-1,5,-1
          if(richb(j,k).ge.ricr) then
            lvl=k
            exit
          endif
        enddo

        if(lvl.lt.km) then
          dum=(poz(j,lvl)-poz(j,lvl+1))/(richb(j,lvl)-richb(j,lvl+1))
          pblh(j)=max(poz(j,lvl+1),poz(j,lvl)-(richb(j,lvl)-ricr)*dum)
          pblh(j)=min(pblh(j),poz(j,lvl))
        endif
      end do

!  preparing for tke/ep calculations
!
      do j=1,lq
        tkm(j) = tz(j,km)
        zkm(j) = poz(j,km)
        qkm(j) = pqv(j,km)
        sfa(j) = tsk(j)*(p1000mb/psfcpa(j))**rcp
        dens(j) = psfcpa(j)/(rgas*tkm(j)*(1.+ep1*qkm(j)))
        cpm = cp * (1.+0.81*qkm(j))

        rm2(j)=ust(j)*ust(j)/wspd(j)
        hs(j)=hfx(j)/(dens(j)*cpm)
        hq(j)=qfx(j)/dens(j)
!
        sh(j,km)=0.0
        st(j,km)=0.0
        duj=ust(j)*ust(j)
        pep(j,km)=duj*ust(j)/(akv*zkm(j))
        pep(j,km) = min(maxpep,max(minpep,pep(j,km)))
        govrth = grv/pt(j,km)
        sflux(j) = hfx(j)/dens(j)/cpm + &
                   qfx(j)/dens(j)*ep1*pt(j,km)
        bfx0    = max(sflux(j),0.)
        wstar3  = govrth*bfx0*pblh(j)
        wstar2(j)  = (wstar3**2)**t13

        rich(j,km) = grv*zkm(j)*  &
            log(szv(j,km)/(sfa(j)*(1.+ep1*qsfc(j))))/(wspd(j)**2.)

        if(rmol(j).lt.0. .and. sflux(j).gt.0) then
          pek(j,km)=3.75*duj+0.2*wstar2(j)+duj*((-zkm(j)*rmol(j))**2.)**t13
        else
          pek(j,km)=3.75*duj
        endif
        pek(j,km) = min(maxpek,max(minpek,pek(j,km)))

      end do

! to recalculate pblh here
      do j=1,lq
        if(rmol(j).lt.0. .and. sflux(j).gt.0) then
          dss=szv(j,km)+min(5.0,bt*sflux(j)/(max(0.1,sqrt(wstar2(j)))))
          do k=1,km
            richb(j,k)=poz(j,k)*(szv(j,k)/dss-1.0)*up2(j,k)
          end do

          pblh(j)=poz(j,km)
          lvl=km
          do k=km-1,5,-1
            if(richb(j,k).ge.ricr) then
              lvl=k
              exit
            end if
          enddo
 
          if(lvl.lt.km) then
            dum=(poz(j,lvl)-poz(j,lvl+1))/(richb(j,lvl)-richb(j,lvl+1))
            pblh(j)=max(poz(j,lvl+1),poz(j,lvl)-(richb(j,lvl)-ricr)*dum)
            pblh(j)=min(pblh(j),poz(j,lvl))
            kpbl(j)=km-lvl+1
          end if
        end if
      end do

! to add the countergradient term
      do j=1,lq
        if(rmol(j).lt.0.0 .and. sflux(j).gt.0) then
          du=max(0.1,sqrt(wstar2(j)))
          gamat(j)=max(bt*hs(j)/(pblh(j)*du),0.)
          gamaq(j)=max(bt*hq(j)/(pblh(j)*du),0.)
        else
          gamat(j)=0.0
          gamaq(j)=0.0
        endif
      end do
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  time integration of tke and ep equations
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      do k=1,km
      do j=1,lq
        richf=0.1912408
        if(rich(j,k).lt.0.195) then
          richf=0.6588*(rich(j,k)+0.1776-sqrt(rich(j,k)*rich(j,k)- &
              0.3221*rich(j,k)+0.03156))
          richf=max(-1000.0,min(0.1912408,richf))
        endif
        dum2(j,k)=1.122346
        if(richf.lt.0.16) dum2(j,k)=1.318*(0.2231-richf) &
              /(0.2341-richf)
      end do
      end do
!
      lvl=1
      do while(lvl .le. mdt)

      do k=1,km-1
        do j=1,lq
          akm(j,k)=c2*pek(j,k)*pek(j,k)/pep(j,k)
          akh(j,k)=dum2(j,k)*akm(j,k)
          akm(j,k)=sgk1(j,k)+akm(j,k)
          akh(j,k)=sgk2(j,k)+akh(j,k)
          akm(j,k)=min(akvmx,akm(j,k))
          akh(j,k)=min(akvmx,akh(j,k))
          if(zz(j,k).gt.pblh(j))then
            src(j,k)=akm(j,k)*sh(j,k)-akh(j,k)*st(j,k)
            src1(j,k)=max(akm(j,k)*sh(j,k),akm(j,k)*sh(j,k)-akh(j,k)*st(j,k))
          else
            src(j,k)=akm(j,k)*sh(j,k)-akh(j,k)*(st(j,k) &
                   -2.0*grv*gamat(j)/(szv(j,k+1)+szv(j,k)))
            src1(j,k)=max(akm(j,k)*sh(j,k),akm(j,k)*sh(j,k)-akh(j,k)*(st(j,k) &
                   -2.0*grv*gamat(j)/(szv(j,k+1)+szv(j,k))))
          end if
        end do
      end do
!
      do j=1,lq
        akm(j,km)=c2*pek(j,km)*pek(j,km)/pep(j,km)
        akh(j,km)=dum2(j,km)*akm(j,km)
      end do
!
!  to calculate the vertical diffusion coefficients at u,v,levels
!
      do k=2,km
      do j=1,lq
        am(j,k)=0.5*(akm(j,k-1)+akm(j,k))/dzz(j,k)
      end do
      end do

      do j=1,lq
        am(j,1)=0.5*akm(j,1)/dzz(j,1)
      end do
!
!  to calculate the cofficients for the triangle matrix
!  for tke dissipation and solve the matrix
!
      do k=1,km-1
        do j=1,lq
          dum=(c3*src1(j,k)-c4*pep(j,k))*dtt/pek(j,k)
          ax1(j,k)=-c5*amt(j,k)*am(j,k+1)
          cx1(j,k)=-c5*amt(j,k)*am(j,k)
          if(dum.le.0.5) then
            bx1(j,k)=1.0+c5*amt(j,k)*(am(j,k)+am(j,k+1))-dum
            yy1(j,k)=pep(j,k)
          else
            bx1(j,k)=1.0+c5*amt(j,k)*(am(j,k)+am(j,k+1))
            yy1(j,k)=pep(j,k)+dum*pep(j,k)
          endif
        end do
      end do
!
      do j=1,lq
        cx1(j,1)=0.0
        ax1(j,km-1)=0.0
        yy1(j,km-1)=yy1(j,km-1)+c5*amt(j,km-1)*am(j,km)*pep(j,km)
      end do
!
      call tridiag(ax1,bx1,cx1,yy1,lq,km-1)
!
!  to calculate the cofficients for the triangle matrix
!  for tke and solve the matrix
!
      do k=1,km-1
        do j=1,lq
          pep(j,k)=min(maxpep,max(minpep, yy1(j,k)))
          dum=(src(j,k)-pep(j,k))*dtt
          ax1(j,k)=-c1*amt(j,k)*am(j,k+1)
          bx1(j,k)=1.0+c1*amt(j,k)*(am(j,k)+am(j,k+1))
          cx1(j,k)=-c1*amt(j,k)*am(j,k)
          yy1(j,k)=pek(j,k)+dum
        end do
      end do

      do j=1,lq
        cx1(j,1)=0.0
        ax1(j,km-1)=0.0
        yy1(j,km-1)=yy1(j,km-1)+c1*amt(j,km-1)*am(j,km)*pek(j,km)
      end do
!
      call tridiag(ax1,bx1,cx1,yy1,lq,km-1)
!
      do k=1,km-1
      do j=1,lq
        pek(j,k)=min(maxpek,max(minpek, yy1(j,k)))
      end do
      end do

      lvl = lvl + 1
      end do ! end while
!
!-------------------------------------------------------------
!  to calculate the vertical diffusion coeficient for momentum 
!  heat, moisture
!-------------------------------------------------------------
!  note: akm and akh at the first level (km) will not be used 
      do k=2,km
        do j=1,lq
          akm(j,k)=c2*pek(j,k-1)*pek(j,k-1)/pep(j,k-1)
          akh(j,k)=dum2(j,k)*akm(j,k)
          akm(j,k)=sgk1(j,k)+akm(j,k)
          akh(j,k)=sgk2(j,k)+akh(j,k)
          akm(j,k)=min(akvmx,akm(j,k))
          akh(j,k)=min(akvmx,akh(j,k))
          pnt(j,k)=akh(j,k)
          if(zz(j,k).gt.pblh(j))pnt(j,k)=0.
          akm(j,k)=akm(j,k)/doz(j,k-1)
          akh(j,k)=akh(j,k)/doz(j,k-1)
        end do
      end do
!
      do j=1,lq
        akm(j,1)=0.0
        akh(j,1)=0.0
        pnt(j,1)=0.0
        akm(j,km+1)=0.0
        akh(j,km+1)=0.0
        pnt(j,km+1)=0.0
      end do
!
! Momentum fluxes calculation
!
      do k=1,km
      do j=1,lq
        ax(j,k)=-alt(j,k)*akm(j,k+1)
        bx(j,k)=1.0+alt(j,k)*(akm(j,k)+akm(j,k+1))
        cx(j,k)=-alt(j,k)*akm(j,k)
        ba(j,k)=ax(j,k)
        ab(j,k)=bx(j,k)
        ac(j,k)=cx(j,k)
      end do
      end do

      do j=1,lq
        bx(j,km) = bx(j,km)+alt(j,km)*rm2(j)
        ab(j,km)=bx(j,km)
      end do

!
      call tridiag(ba,ab,ac,pu,lq,km)
!
      call tridiag(ax,bx,cx,pv,lq,km)
!
! Heat and moisture fluxes calculation
!
      do k=1,km
      do j=1,lq
        ax(j,k)=-alt(j,k)*akh(j,k+1)
        bx(j,k)=1.0+alt(j,k)*(akh(j,k)+akh(j,k+1))
        cx(j,k)=-alt(j,k)*akh(j,k)
        ba(j,k)=ax(j,k)
        ab(j,k)=bx(j,k)
        ac(j,k)=cx(j,k)
        dum0=0.0       
        if(k.gt.1) dum0=pep(j,k-1)
        dum=pep(j,k)
        if(dum0.le.1.e-6) dum0=0.0
        if(dum.le.1.e-6) dum=0.0
        disht(j,k)=0.5*(dum0+dum)/(cp*(1.+.81*pqv(j,k)))
        pt(j,k)=pt(j,k)+disht(j,k)*dt/psp(j,k)+alt(j,k)* &
                gamat(j)*(pnt(j,k+1)-pnt(j,k))
      end do
      end do
!
      do j=1,lq
        pt(j,km)=pt(j,km)+alt(j,km)*hs(j)
      end do
!
      call tridiag(ba,ab,ac,pt,lq,km)
!
      do k=1,km
      do j=1,lq
        ba(j,k)=ax(j,k)
        ab(j,k)=bx(j,k)
        ac(j,k)=cx(j,k)
      end do
      end do

      do k=1,km
      do j=1,lq
         pqv(j,k)=pqv(j,k)+alt(j,k)*gamaq(j)* &
                 (pnt(j,k+1)-pnt(j,k))
      end do
      end do

      do j=1,lq
        pqv(j,km)=pqv(j,km)+alt(j,km)*hq(j)
      end do
!
      call tridiag(ba,ab,ac,pqv,lq,km)
!
      do k=1,km
      do j=1,lq
        ba(j,k)=ax(j,k)
        ab(j,k)=bx(j,k)
        ac(j,k)=cx(j,k)
      end do
      end do

      call tridiag(ba,ab,ac,pqc,lq,km)
!
      call tridiag(ax,bx,cx,pqi,lq,km)
!
      do k=1,km
      do j=1,lq
        tz(j,k)=pt(j,k)*psp(j,k)
        pqv(j,k)=max(epsl,pqv(j,k))
        pqc(j,k)=max(epsl,pqc(j,k))
        pqi(j,k)=max(epsl,pqi(j,k))
!-----------------------------------------------------
!  immediate melting of cloud ice below freezing level
!-----------------------------------------------------
        if(tz(j,k).ge.t0.and.pqi(j,k).gt.epsl) then
          if(pqc(j,k).le.epsl) then
            pqc(j,k)=pqi(j,k)
          else
            pqc(j,k)=pqc(j,k)+pqi(j,k)
          endif
          cpm=cp*(1.0+0.81*pqv(j,k))
          alh=3.1484e6-2.37e3*tz(j,k)
          tz(j,k)=tz(j,k)-(xls-alh)*pqi(j,k)/cpm
          pqi(j,k)=epsl
        endif
      end do
      end do

      do k=2,km
      do j=1,lq
        k_h(j,k)=akh(j,k)*doz(j,k-1)
        k_m(j,k)=akm(j,k)*doz(j,k-1)
      end do
      end do

      do j=1,lq
        k_h(j,1)=0.
        k_m(j,1)=0.
        k_m(j,km+1)= 0. !c2*pek(j,km)*pek(j,km)/pep(j,km) ! we don't need it
        k_h(j,km+1)= 0. !dum2(j,km)*k_m(j,km+1)           ! we don't need it
      end do

   end subroutine eeps2d

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ==================================================================
   subroutine tridiag(a,b,c,d,lq,km)
!! to solve system of linear eqs on tridiagonal matrix n times n
!! after Peaceman and Rachford, 1955
!! a,b,c,d - are vectors of order n 
!! a,b,c - are coefficients on the LHS
!! d - is initially RHS on the output becomes a solution vector

!-------------------------------------------------------------------
    implicit none
    INTEGER, INTENT(in):: lq,km
    REAL, DIMENSION(lq,km), INTENT(in) :: a,b
    REAL, DIMENSION(lq,km), INTENT(inout) :: c,d

    INTEGER :: j,k
    REAL :: p
    REAL, DIMENSION(lq,km) :: q

    do j=1,lq
      c(j,1)=0.
      q(j,km)=-c(j,km)/b(j,km)
      d(j,km)=d(j,km)/b(j,km)
    end do

    DO k=km-1,1,-1
    do j=1,lq
       p=1./(b(j,k)+a(j,k)*q(j,k+1))
       q(j,k)=-c(j,k)*p
       d(j,k)=(d(j,k)-a(j,k)*d(j,k+1))*p
    end do
    ENDDO

    DO k=2,km
    do j=1,lq
       d(j,k)=d(j,k)+q(j,k)*d(j,k-1)
    end do
    ENDDO

   end subroutine tridiag

!-------------------------------------------------------------------------------
   subroutine eepsinit(rublten,rvblten,rthblten,rqvblten,                       &
                      rqcblten,rqiblten,p_qi,p_first_scalar,pek,pep,            &
                      restart, allowed_to_read,                                &
                      ids, ide, jds, jde, kds, kde,                            &
                      ims, ime, jms, jme, kms, kme,                            &
                      its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   logical , intent(in)          :: restart, allowed_to_read
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
   real, dimension( ims:ime, kms:kme, jms:jme ),intent(out)   ::     pek, pep
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
            pek(i,k,j) = minpek
            pep(i,k,j) = minpep
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
   end subroutine eepsinit
!-------------------------------------------------------------------------------  
end module module_bl_eeps
