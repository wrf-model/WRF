!
!
!
!
MODULE module_shcu_nscv
CONTAINS
!-------------------------------------------------------------------------------
   subroutine shcu_nscv(dt,p3di,p3d,pi3d,qc3d,qi3d,rho3d,                      &
                     qv3d,t3d,raincv,xland,dz8w,w,u3d,v3d,                     &
                     hpbl,hfx,qfx,                                             &
                     mp_physics,                                               &
                     pgcon,                                                    &
                     cp,cliq,cpv,g,xlv,r_d,r_v,ep_1,ep_2,                      &
                     cice,xls,psat,f_qi,f_qc,                                  &
                     rthshten,rqvshten,rqcshten,rqishten,                      &
                     rushten,rvshten,                                          &
                     pratesh,hbot,htop,                                        &
                     ids,ide, jds,jde, kds,kde,                                &
                     ims,ime, jms,jme, kms,kme,                                &
                     its,ite, jts,jte, kts,kte)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!-- dt          time step (s)
!-- p3di        3d pressure (pa) at interface level
!-- p3d         3d pressure (pa)
!-- pi3d        3d exner function (dimensionless)
!-- z           height above sea level (m)
!-- qc3d        cloud water mixing ratio (kg/kg)
!-- qi3d        cloud ice mixing ratio (kg/kg)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- t3d         temperature (k)
!-- w           vertical velocity (m/s)
!-- dz8w        dz between full levels (m)
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
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
   integer,  intent(in   )   ::       ids,ide, jds,jde, kds,kde,               &
                                      ims,ime, jms,jme, kms,kme,               &
                                      its,ite, jts,jte, kts,kte
   real,     intent(in   )   ::      cp,cliq,cpv,g,xlv,r_d,r_v,ep_1,ep_2,      &
                                     cice,xls,psat
   real,     intent(in   )   ::      dt
   real,     optional, intent(in ) :: pgcon
   real,     dimension( ims:ime, kms:kme, jms:jme ),optional                  ,&
             intent(inout)   ::                                       rthshten,&
                                                                       rushten,&
                                                                       rvshten,&
                                                                      rqcshten,&
                                                                      rqishten,&
                                                                      rqvshten
   logical, optional ::                                              F_QC,F_QI
   real,     dimension( ims:ime, kms:kme, jms:jme )                           ,&
             intent(in   )   ::                                           qv3d,&
                                                                          qc3d,&
                                                                          qi3d,&
                                                                         rho3d,&
                                                                           p3d,&
                                                                          pi3d,&
                                                                           t3d
   real,     dimension( ims:ime, kms:kme, jms:jme )                           ,&
             intent(in   )   ::                                           p3di
   real,     dimension( ims:ime, kms:kme, jms:jme )                           ,&
             intent(in   )   ::                                           dz8w,&  
                                                                             w
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                         raincv
   real,     dimension( ims:ime, jms:jme )                                    ,&
             intent(inout) ::                                          pratesh
   real,     dimension( ims:ime, jms:jme )                                    ,&
             intent(out) ::                                               hbot,&
                                                                          htop
!
   real,     dimension( ims:ime, jms:jme )                                    ,&
             intent(in   ) ::                                            xland
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                           ,&
              intent(in   )   ::                                           u3d,&
                                                                           v3d
!
   real,     dimension( ims:ime, jms:jme )                                    ,&
              intent(in   )   ::                                          hpbl,&
                                                                           hfx,&
                                                                           qfx
   integer,   intent(in   )   ::                                    mp_physics
   integer :: ncloud
!
!  local
!
   real,   dimension( its:ite, kts:kte )  ::                               del,&
                                                                         prsll,&
                                                                           dot,&
                                                                            u1,&
                                                                            v1,&
                                                                            t1,&
                                                                           q1, &
                                                                           qc2,&
                                                                           qi2
   real,   dimension( its:ite, kts:kte+1 )  ::                           prsii,&
                                                                           zii
   real,   dimension( its:ite, kts:kte )  ::                               zll 
   real,   dimension( its:ite)  ::                                         rain
   real ::                                                          delt,rdelt
   integer, dimension (its:ite)  ::                                       kbot,&
                                                                          ktop,&
                                                                          icps
   real :: pgcon_use
   integer ::  i,j,k,kp
!
! microphysics scheme --> ncloud 
!
   if (mp_physics .eq. 0) then
     ncloud = 0
   elseif ( mp_physics .eq. 1 .or. mp_physics .eq. 3 ) then
     ncloud = 1
   else
     ncloud = 2
   endif
!
   if(present(pgcon)) then
     pgcon_use = pgcon
   else
!    pgcon_use  = 0.7     ! Gregory et al. (1997, QJRMS)
     pgcon_use  = 0.55    ! Zhang & Wu (2003,JAS)
     ! 0.55 is a physically-based value used by GFS
     ! HWRF uses 0.2, for model tuning purposes
   endif
!
   delt=dt
   rdelt=1./delt
!
! outer most J_loop
!
   do j = jts,jte
     do k = kts,kte
       kp = k+1
       do i = its,ite
         dot(i,k) = -5.0e-4*g*rho3d(i,k,j)*(w(i,k,j)+w(i,kp,j))
         prsll(i,k)=p3d(i,k,j)*0.001
         prsii(i,k)=p3di(i,k,j)*0.001
       enddo
     enddo
!
     do i = its,ite
       prsii(i,kte+1)=p3di(i,kte+1,j)*0.001
     enddo
!
     do i = its,ite
       zii(i,1)=0.0
     enddo     
!
     do k = kts,kte                                            
       do i = its,ite
         zii(i,k+1)=zii(i,k)+dz8w(i,k,j)
       enddo
     enddo
!
     do k = kts,kte                
       do i = its,ite                                                  
         zll(i,k)=0.5*(zii(i,k)+zii(i,k+1))
       enddo                                                         
     enddo
!
     do k = kts,kte
       do i = its,ite
         del(i,k)=prsll(i,k)*g/r_d*dz8w(i,k,j)/t3d(i,k,j)
         u1(i,k)=u3d(i,k,j)
         v1(i,k)=v3d(i,k,j)
         q1(i,k)=qv3d(i,k,j)
!        q1(i,k)=qv3d(i,k,j)/(1.+qv3d(i,k,j))
         t1(i,k)=t3d(i,k,j)
         qi2(i,k) = qi3d(i,k,j)
         qc2(i,k) = qc3d(i,k,j)
       enddo
     enddo
!
     icps(:) = 0
     do i = its,ite
       if(raincv(i,j) .gt. 1.e-30) icps(i)=1
     enddo
!
! NCEP SCV
!
     call nscv2d(delt=delt,del=del(its,kts),prsl=prsll(its,kts),               &
              prsi=prsii(its,kts),prslk=pi3d(ims,kms,j),zl=zll(its,kts),       &
              ncloud=ncloud,qc2=qc2(its,kts),qi2=qi2(its,kts),                 &
              q1=q1(its,kts),t1=t1(its,kts),rain=rain(its),                    &
              kbot=kbot(its),ktop=ktop(its),                                   &
              icps=icps(its),                                                  &
              slimsk=xland(ims,j),dot=dot(its,kts),                            &
              u1=u1(its,kts), v1=v1(its,kts),                                  &
              cp_=cp,cliq_=cliq,cvap_=cpv,g_=g,hvap_=xlv,                      &
              rd_=r_d,rv_=r_v,fv_=ep_1,ep2=ep_2,                               &
              cice=cice,xls=xls,psat=psat,                                     &
              hpbl=hpbl(ims,j),hfx=hfx(ims,j),qfx=qfx(ims,j),                  &
              pgcon=pgcon_use,                                                 &
              ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde,               &
              ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme,               &
              its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )
!
     do i = its,ite
       pratesh(i,j) = rain(i)*1000./dt
       hbot(i,j) = kbot(i)
       htop(i,j) = ktop(i)
     enddo
!
     IF(PRESENT(rthshten).AND.PRESENT(rqvshten)) THEN
       do k = kts,kte
         do i = its,ite
           rthshten(i,k,j)=(t1(i,k)-t3d(i,k,j))/pi3d(i,k,j)*rdelt
           rqvshten(i,k,j)=(q1(i,k)-qv3d(i,k,j))*rdelt
         enddo
       enddo
     ENDIF
!
     IF(PRESENT(rushten).AND.PRESENT(rvshten)) THEN
       do k = kts,kte
         do i = its,ite
           rushten(i,k,j)=(u1(i,k)-u3d(i,k,j))*rdelt
           rvshten(i,k,j)=(v1(i,k)-v3d(i,k,j))*rdelt
         enddo
       enddo
     ENDIF
!
     IF(PRESENT( rqishten )) THEN
       IF ( F_QI ) THEN
         do k = kts,kte
           do i = its,ite
             rqishten(i,k,j)=(qi2(i,k)-qi3d(i,k,j))*rdelt
           enddo
         enddo
       ENDIF
     ENDIF
!
     IF(PRESENT( rqcshten )) THEN
       IF ( F_QC ) THEN
         do k = kts,kte
           do i = its,ite
             rqcshten(i,k,j)=(qc2(i,k)-qc3d(i,k,j))*rdelt
           enddo
         enddo
       ENDIF
     ENDIF
!
   enddo ! outer most J_loop
!
   return
   end subroutine shcu_nscv
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
! NCEP SCV (Shallow Convection Scheme)
!-------------------------------------------------------------------------------
   subroutine nscv2d(delt,del,prsl,prsi,prslk,zl,                              &
                 ncloud,qc2,qi2,q1,t1,rain,kbot,ktop,                          &
                 icps,                                                         &
                 slimsk,dot,u1,v1,                                             &
                 cp_,cliq_,cvap_,g_,hvap_,rd_,rv_,fv_,ep2,                     &
                 cice,xls,psat,                                                &
                 hpbl,hfx,qfx,                                                 &
                 pgcon,                                                        &
                 ids,ide, jds,jde, kds,kde,                                    &
                 ims,ime, jms,jme, kms,kme,                                    &
                 its,ite, jts,jte, kts,kte)
!-------------------------------------------------------------------------------
!
! subprogram:    nscv2d    computes shallow-convective heating and moistening
!
! abstract: computes non-precipitating convective heating and moistening 
!   using a one cloud type arakawa-schubert convection scheme as in the ncep
!   sas scheme. the scheme has been operational at ncep gfs model since july 2010
!   the scheme includes updraft and downdraft effects, but the cloud depth is 
!   limited less than 150 hpa. 
!
! developed by jong-il han and hua-lu pan 
!   implemented into wrf by jihyeon jang and songyou hong
!   module with cpp-based options is available in grims
!
! program history log:
!   10-07-01 jong-il han  initial operational implementation at ncep gfs
!   10-12-01 jihyeon jang implemented into wrf
!   18-05-04 jihyeon jang seperated the shallow convection module from nsas
!
! subprograms called:
!   fpvs     - function to compute saturation vapor pressure
!
! references:
!   han and pan (2011, wea. forecasting)
!   
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
!  in/out variables
!
   integer         ::  ids,ide, jds,jde, kds,kde,                              &
                       ims,ime, jms,jme, kms,kme,                              &
                       its,ite, jts,jte, kts,kte
   real            ::  cp_,cliq_,cvap_,g_,hvap_,rd_,rv_,fv_,ep2
   real            ::  pi_,qmin_,t0c_
   real            ::  cice,xlv0,xls,psat
!
   real            ::  delt
   real            ::  del(its:ite,kts:kte),                                   &
                       prsl(its:ite,kts:kte),prslk(ims:ime,kms:kme),           &
                       prsi(its:ite,kts:kte+1),zl(its:ite,kts:kte)
   integer         ::  ncloud
   real            ::  slimsk(ims:ime)
   real            ::  dot(its:ite,kts:kte)
   real            ::  hpbl(ims:ime)
   real            ::  rcs
   real            ::  hfx(ims:ime),qfx(ims:ime)
!
   real            ::  qi2(its:ite,kts:kte),qc2(its:ite,kts:kte)
   real            ::  q1(its:ite,kts:kte),                                    &
                       t1(its:ite,kts:kte),                                    &
                       u1(its:ite,kts:kte),                                    &
                       v1(its:ite,kts:kte)
   integer         ::  icps(its:ite)
!
   real            ::  rain(its:ite)
   integer         ::  kbot(its:ite),ktop(its:ite)
!
!  local variables and arrays
!
   integer         ::  i,j,indx, jmn, k, kk, km1
   integer         ::  kpbl(its:ite)
!
   real            ::  dellat,                                                 &
                       desdt,   deta,    detad,   dg,                          &
                       dh,      dhh,     dlnsig,  dp,                          &
                       dq,      dqsdp,   dqsdt,   dt,                          &
                       dt2,     dtmax,   dtmin,                                &
                       dv1h,    dv2h,    dv3h,                                 &
                       dv1q,    dv2q,    dv3q,                                 &
                       dv1u,    dv2u,    dv3u,                                 &
                       dv1v,    dv2v,    dv3v,                                 &
                       dz,      dz1,     e1,      clam,                        &
                       aafac,                                                  &
                       es,      etah,                                          &
                       evef,    evfact,  evfactl,                              &
                       factor,  fjcap,                                         &
                       gamma,   pprime,  betaw,                                &
                       qlk,     qrch,    qs,                                   &
                       rfact,   shear,   tem1,                                 &
                       tem2,    val,     val1,                                 &
                       val2,    w1,      w1l,     w1s,                         &
                       w2,      w2l,     w2s,     w3,                          &
                       w3l,     w3s,     w4,      w4l,                         &
                       w4s,     tem,     ptem,    ptem1,                       &
                       pgcon
!
   integer         ::  kb(its:ite), kbcon(its:ite), kbcon1(its:ite),           &
                       ktcon(its:ite), ktcon1(its:ite),                        &
                       kbm(its:ite), kmax(its:ite)
!
   real            ::  aa1(its:ite),                                           &
                       delhbar(its:ite), delq(its:ite),                        &
                       delq2(its:ite),   delqev(its:ite), rntot(its:ite),      &
                       delqbar(its:ite), deltbar(its:ite),                     &
                       deltv(its:ite),   edt(its:ite),                         &
                       wstar(its:ite),   sflx(its:ite),                        &
                       pdot(its:ite),    po(its:ite,kts:kte),                  &
                       qcond(its:ite),   qevap(its:ite),  hmax(its:ite),       &
                       vshear(its:ite),                                        &
                       xlamud(its:ite),  xmb(its:ite),    xmbmax(its:ite)
   real            ::  delubar(its:ite), delvbar(its:ite)
!
   real            ::  cincr
!
   real            ::  thx(its:ite, kts:kte)
   real            ::  rhox(its:ite)
   real            ::  tvcon
!
   real            ::  p(its:ite,kts:kte),       to(its:ite,kts:kte),          &
                       qo(its:ite,kts:kte),      qeso(its:ite,kts:kte),        &
                       uo(its:ite,kts:kte),      vo(its:ite,kts:kte)
!
!  cloud water
!
   real            ::  qlko_ktcon(its:ite),     dellal(its:ite,kts:kte),       &
                       dbyo(its:ite,kts:kte),                                  &
                       xlamue(its:ite,kts:kte),                                &
                       heo(its:ite,kts:kte),    heso(its:ite,kts:kte),         &
                       dellah(its:ite,kts:kte), dellaq(its:ite,kts:kte),       &
                       dellau(its:ite,kts:kte), dellav(its:ite,kts:kte),       &
                       ucko(its:ite,kts:kte),   vcko(its:ite,kts:kte),         &
                       hcko(its:ite,kts:kte),   qcko(its:ite,kts:kte),         &
                       eta(its:ite,kts:kte),    zi(its:ite,kts:kte+1),         &
                       pwo(its:ite,kts:kte)
!
   logical         ::  totflg, cnvflg(its:ite), flg(its:ite)
!
!  physical parameters
!
   real,parameter  ::  c0=.002,c1=5.e-4
   real,parameter  ::  cincrmax=180.,cincrmin=120.,dthk=25.
   real            ::  el2orc,fact1,fact2,eps
   real,parameter  ::  h1=0.33333333
   real,parameter  ::  tf=233.16, tcr=263.16, tcrf=1.0/(tcr-tf)
!-------------------------------------------------------------------------------
   pi_ = 3.14159
   qmin_ = 1.0e-30
   t0c_ = 273.15
   xlv0 = hvap_
   km1 = kte - 1
!
!  compute surface buoyancy flux
!
   do k = kts,kte
     do i = its,ite
       thx(i,k) = t1(i,k)/prslk(i,k)
     enddo
   enddo
!
   do i = its,ite
     tvcon = (1.+fv_*q1(i,1))
     rhox(i) = prsl(i,1)*1.e3/(rd_*t1(i,1)*tvcon)
   enddo
!
   do i = its,ite
!    sflx(i) = heat(i)+fv_*t1(i,1)*evap(i)
     sflx(i) = hfx(i)/rhox(i)/cp_ + qfx(i)/rhox(i)*fv_*thx(i,1)
   enddo
!
!  initialize arrays
!
   do i = its,ite
     cnvflg(i) = .true.
     if(icps(i).eq.1) cnvflg(i) = .false.
     if(sflx(i).le.0.) cnvflg(i) = .false.
     if(cnvflg(i)) then
       kbot(i)=kte+1
       ktop(i)=0
     endif
     rain(i)=0.
     kbcon(i)=kte
     ktcon(i)=1
     kb(i)=kte
     pdot(i) = 0.
     qlko_ktcon(i) = 0.
     edt(i)  = 0.
     aa1(i)  = 0.
     vshear(i) = 0.
   enddo
!
   totflg = .true.
   do i = its,ite
     totflg = totflg .and. (.not. cnvflg(i))
   enddo
   if(totflg) return
!
   dt2   =  delt
   val   =         1200.
   dtmin = max(dt2, val )
   val   =         3600.
   dtmax = max(dt2, val )
!
!  model tunable parameters are all here
!
   clam    = .3
   aafac   = .1
   betaw   = .03
   evfact  = 0.3
   evfactl = 0.3
   val     = 1.
!
! define miscellaneous values
!
   el2orc = hvap_*hvap_/(rv_*cp_)
   eps    = rd_/rv_ 
   fact1  = (cvap_-cliq_)/rv_
   fact2  = hvap_/rv_-fact1*t0c_
!
   w1l     = -8.e-3
   w2l     = -4.e-2
   w3l     = -5.e-3
   w4l     = -5.e-4
   w1s     = -2.e-4
   w2s     = -2.e-3
   w3s     = -1.e-3
   w4s     = -2.e-5
!
!  define top layer for search of the downdraft originating layer
!  and the maximum thetae for updraft
!
   do i = its,ite
     kbm(i)   = kte
     kmax(i)  = kte
   enddo
!     
   do k = kts,kte
     do i = its,ite
       if (prsl(i,k).gt.prsi(i,1)*0.70) kbm(i) = k + 1
       if (prsl(i,k).gt.prsi(i,1)*0.60) kmax(i) = k + 1
     enddo
   enddo
!
   do i = its,ite
     kbm(i)   = min(kbm(i),kmax(i))
   enddo
!
!  hydrostatic height assume zero terr and compute
!  updraft entrainment rate as an inverse function of height
!
   do k = kts+1,kte
     do i = its,ite
       zi(i,k) = 0.5*(zl(i,k-1)+zl(i,k))
     enddo
   enddo
!
   do k = kts,km1
     do i = its,ite
       xlamue(i,k) = clam / zi(i,k+1)
     enddo
   enddo
!
   do i = its,ite
     xlamue(i,kte) = xlamue(i,km1)
   enddo
!
!  pbl height
!
   do i = its,ite
     flg(i) = cnvflg(i)
     kpbl(i)= 1
   enddo
!
   do k = kts+1,km1
     do i = its,ite
       if (flg(i).and.zl(i,k).le.hpbl(i)) then 
         kpbl(i) = k
       else
         flg(i) = .false.
       endif
     enddo
   enddo
!
   do i = its,ite
     kpbl(i)= min(kpbl(i),kbm(i))
   enddo
!
!   convert surface pressure to mb from cb
!
   rcs = 1.
   do k = kts,kte
     do i = its,ite
       if (cnvflg(i) .and. k .le. kmax(i)) then
         p(i,k) = prsl(i,k) * 10.0
         eta(i,k)  = 1.
         hcko(i,k) = 0.
         qcko(i,k) = 0.
         ucko(i,k) = 0.
         vcko(i,k) = 0.
         dbyo(i,k) = 0.
         pwo(i,k)  = 0.
         dellal(i,k) = 0.
         to(i,k)   = t1(i,k)
         qo(i,k)   = q1(i,k)
         uo(i,k)   = u1(i,k) * rcs
         vo(i,k)   = v1(i,k) * rcs
       endif
     enddo
   enddo
!
!
!  column variables
!  p is pressure of the layer (mb)
!  t is temperature at t-dt (k)..tn
!  q is mixing ratio at t-dt (kg/kg)..qn
!  to is temperature at t+dt (k)... this is after advection and turbulan
!  qo is mixing ratio at t+dt (kg/kg)..q1
!
   do k = kts, kte
     do i=its,ite
       if (cnvflg(i) .and. k .le. kmax(i)) then
         qeso(i,k) = 0.01 * fpvs(to(i,k),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
         qeso(i,k) = eps * qeso(i,k) / (p(i,k) + (eps-1.)*qeso(i,k))
         val1      =             1.e-8
         qeso(i,k) = max(qeso(i,k), val1)
         val2      =           1.e-10
         qo(i,k)   = max(qo(i,k), val2 )
       endif
     enddo
   enddo
!
!  compute moist static energy
!
   do k = kts,kte
     do i=its,ite
       if (cnvflg(i) .and. k .le. kmax(i)) then
         tem       = g_ * zl(i,k) + cp_ * to(i,k)
         heo(i,k)  = tem  + hvap_ * qo(i,k)
         heso(i,k) = tem  + hvap_ * qeso(i,k)
       endif
     enddo
   enddo
!
!  determine level with largest moist static energy within pbl
!  this is the level where updraft starts
!
   do i=its,ite
     if (cnvflg(i)) then
       hmax(i) = heo(i,1)
       kb(i) = 1
     endif
   enddo
!
   do k = kts+1, kte
     do i=its,ite
       if (cnvflg(i).and.k.le.kpbl(i)) then
         if(heo(i,k).gt.hmax(i)) then
           kb(i)   = k
           hmax(i) = heo(i,k)
         endif
       endif
     enddo
   enddo
!
   do k = kts, km1
     do i=its,ite
       if (cnvflg(i) .and. k .le. kmax(i)-1) then
         dz      = .5 * (zl(i,k+1) - zl(i,k))
         dp      = .5 * (p(i,k+1) - p(i,k))
         es = 0.01*fpvs(to(i,k+1),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
         pprime  = p(i,k+1) + (eps-1.) * es
         qs      = eps * es / pprime
         dqsdp   = - qs / pprime
         desdt   = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
         dqsdt   = qs * p(i,k+1) * desdt / (es * pprime)
         gamma   = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
         dt      = (g_ * dz + hvap_ * dqsdp * dp) / (cp_ * (1. + gamma))
         dq      = dqsdt * dt + dqsdp * dp
         to(i,k) = to(i,k+1) + dt
         qo(i,k) = qo(i,k+1) + dq
         po(i,k) = .5 * (p(i,k) + p(i,k+1))
       endif
     enddo
   enddo
!
   do k = kts, km1
     do i=its,ite
       if (cnvflg(i) .and. k .le. kmax(i)-1) then
         qeso(i,k)=0.01*fpvs(to(i,k),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
         qeso(i,k) = eps * qeso(i,k) / (po(i,k) + (eps-1.) * qeso(i,k))
         val1      =             1.e-8
         qeso(i,k) = max(qeso(i,k), val1)
         val2      =           1.e-10
         qo(i,k)   = max(qo(i,k), val2 )
         heo(i,k)  = .5 * g_ * (zl(i,k) + zl(i,k+1)) +                         &
                        cp_ * to(i,k) + hvap_ * qo(i,k)
         heso(i,k) = .5 * g_ * (zl(i,k) + zl(i,k+1)) +                         &
                        cp_ * to(i,k) + hvap_ * qeso(i,k)
         uo(i,k)   = .5 * (uo(i,k) + uo(i,k+1))
         vo(i,k)   = .5 * (vo(i,k) + vo(i,k+1))
       endif
     enddo
   enddo
!
!  look for the level of free convection as cloud base
!
   do i=its,ite
     flg(i)   = cnvflg(i)
     if(flg(i)) kbcon(i) = kmax(i)
   enddo
!
   do k = kts+1, km1
     do i=its,ite
       if (flg(i).and.k.lt.kbm(i)) then
         if(k.gt.kb(i).and.heo(i,kb(i)).gt.heso(i,k)) then
           kbcon(i) = k
           flg(i)   = .false.
         endif
       endif
     enddo
   enddo
!
   do i=its,ite
     if(cnvflg(i)) then
       if(kbcon(i).eq.kmax(i)) cnvflg(i) = .false.
     endif
   enddo
!
   totflg = .true.
   do i=its,ite
     totflg = totflg .and. (.not. cnvflg(i))
   enddo
   if(totflg) return
!
!  determine critical convective inhibition
!  as a function of vertical velocity at cloud base.
!
   do i=its,ite
     if(cnvflg(i)) then
       pdot(i)  = 10.* dot(i,kbcon(i))
     endif
   enddo
!
   do i=its,ite
     if(cnvflg(i)) then
       if(slimsk(i).eq.1.) then
         w1 = w1l
         w2 = w2l
         w3 = w3l
         w4 = w4l
       else
         w1 = w1s
         w2 = w2s
         w3 = w3s
         w4 = w4s
       endif
       if(pdot(i).le.w4) then
         ptem = (pdot(i) - w4) / (w3 - w4)
       elseif(pdot(i).ge.-w4) then
         ptem = - (pdot(i) + w4) / (w4 - w3)
       else
         ptem = 0.
       endif
       val1    =             -1.
       ptem = max(ptem,val1)
       val2    =             1.
       ptem = min(ptem,val2)
       ptem = 1. - ptem
       ptem1= .5*(cincrmax-cincrmin)
       cincr = cincrmax - ptem * ptem1
       tem1 = p(i,kb(i)) - p(i,kbcon(i))
       if(tem1.gt.cincr) then
         cnvflg(i) = .false.
       endif
     endif
   enddo
!
   totflg = .true.
   do i=its,ite
     totflg = totflg .and. (.not. cnvflg(i))
   enddo
   if(totflg) return
!
!  assume the detrainment rate for the updrafts to be same as 
!  the entrainment rate at cloud base
!
   do i = its,ite
     if(cnvflg(i)) then
       xlamud(i) = xlamue(i,kbcon(i))
     endif
   enddo
!
!  determine updraft mass flux for the subcloud layers
!
   do k = km1, kts, -1
     do i = its,ite
       if (cnvflg(i)) then
         if(k.lt.kbcon(i).and.k.ge.kb(i)) then
           dz       = zi(i,k+2) - zi(i,k+1)
           ptem     = 0.5*(xlamue(i,k)+xlamue(i,k+1))-xlamud(i)
           eta(i,k) = eta(i,k+1) / (1. + ptem * dz)
         endif
       endif
     enddo
   enddo
!
!  compute mass flux above cloud base
!
   do k = kts+1, km1
     do i = its,ite
       if(cnvflg(i))then
         if(k.gt.kbcon(i).and.k.lt.kmax(i)) then
           dz       = zi(i,k+1) - zi(i,k)
           ptem     = 0.5*(xlamue(i,k)+xlamue(i,k-1))-xlamud(i)
           eta(i,k) = eta(i,k-1) * (1 + ptem * dz)
         endif
       endif
     enddo
   enddo
!
!  compute updraft cloud property
!
   do i = its,ite
     if(cnvflg(i)) then
       indx         = kb(i)
       hcko(i,indx) = heo(i,indx)
       ucko(i,indx) = uo(i,indx)
       vcko(i,indx) = vo(i,indx)
     endif
   enddo
!
   do k = kts+1, km1
     do i = its,ite
       if (cnvflg(i)) then
         if(k.gt.kb(i).and.k.lt.kmax(i)) then
           dz   = zi(i,k+1) - zi(i,k)
           tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
           tem1 = 0.5 * xlamud(i) * dz
           factor = 1. + tem - tem1
           ptem = 0.5 * tem + pgcon
           ptem1= 0.5 * tem - pgcon
           hcko(i,k) = ((1.-tem1)*hcko(i,k-1)+tem*0.5*                         &
                       (heo(i,k)+heo(i,k-1)))/factor
           ucko(i,k) = ((1.-tem1)*ucko(i,k-1)+ptem*uo(i,k)                     &
                       +ptem1*uo(i,k-1))/factor
           vcko(i,k) = ((1.-tem1)*vcko(i,k-1)+ptem*vo(i,k)                     &
                       +ptem1*vo(i,k-1))/factor
           dbyo(i,k) = hcko(i,k) - heso(i,k)
         endif
       endif
     enddo
   enddo
!
!   taking account into convection inhibition due to existence of
!    dry layers below cloud base
!
   do i=its,ite
     flg(i) = cnvflg(i)
     kbcon1(i) = kmax(i)
   enddo
!
   do k = kts+1, km1
     do i=its,ite
       if (flg(i).and.k.lt.kbm(i)) then
         if(k.ge.kbcon(i).and.dbyo(i,k).gt.0.) then
           kbcon1(i) = k
           flg(i)    = .false.
         endif
       endif
     enddo
   enddo
!
   do i=its,ite
     if(cnvflg(i)) then
       if(kbcon1(i).eq.kmax(i)) cnvflg(i) = .false.
     endif
   enddo
!
   do i=its,ite
     if(cnvflg(i)) then
       tem = p(i,kbcon(i)) - p(i,kbcon1(i))
       if(tem.gt.dthk) then
         cnvflg(i) = .false.
       endif
     endif
   enddo
!
   totflg = .true.
   do i = its,ite
     totflg = totflg .and. (.not. cnvflg(i))
   enddo
   if(totflg) return
!
!  determine first guess cloud top as the level of zero buoyancy
!    limited to the level of sigma=0.7
!
   do i = its,ite
     flg(i) = cnvflg(i)
     if(flg(i)) ktcon(i) = kbm(i)
   enddo
!
   do k = kts+1, km1
     do i=its,ite
       if (flg(i).and.k .lt. kbm(i)) then
         if(k.gt.kbcon1(i).and.dbyo(i,k).lt.0.) then
           ktcon(i) = k
           flg(i)   = .false.
         endif
       endif
     enddo
   enddo
!
!  specify upper limit of mass flux at cloud base
!
   do i = its,ite
     if(cnvflg(i)) then
       k = kbcon(i)
       dp = 1000. * del(i,k)
       xmbmax(i) = dp / (g_ * dt2)
     endif
   enddo
!
!  compute cloud moisture property and precipitation
!
   do i = its,ite
     if (cnvflg(i)) then
       aa1(i) = 0.
       qcko(i,kb(i)) = qo(i,kb(i))
     endif
   enddo
!
   do k = kts+1, km1
     do i = its,ite
       if (cnvflg(i)) then
         if(k.gt.kb(i).and.k.lt.ktcon(i)) then
           dz    = zi(i,k+1) - zi(i,k)
           gamma = el2orc * qeso(i,k) / (to(i,k)**2)
           qrch = qeso(i,k) + gamma * dbyo(i,k) / (hvap_ * (1. + gamma))
           tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
           tem1 = 0.5 * xlamud(i) * dz
           factor = 1. + tem - tem1
           qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*                         &
                       (qo(i,k)+qo(i,k-1)))/factor
           dq = eta(i,k) * (qcko(i,k) - qrch)
!
!          rhbar(i) = rhbar(i) + qo(i,k) / qeso(i,k)
!
!  below lfc check if there is excess moisture to release latent heat
!
           if(k.ge.kbcon(i).and.dq.gt.0.) then
             etah = .5 * (eta(i,k) + eta(i,k-1))
             if(ncloud.gt.0) then
               dp = 1000. * del(i,k)
               qlk = dq / (eta(i,k) + etah * (c0 + c1) * dz)
               dellal(i,k) = etah * c1 * dz * qlk * g_ / dp
             else
               qlk = dq / (eta(i,k) + etah * c0 * dz)
             endif
             aa1(i) = aa1(i) - dz * g_ * qlk
             qcko(i,k)= qlk + qrch
             pwo(i,k) = etah * c0 * dz * qlk
           endif
         endif
       endif
     enddo
   enddo
!
!  calculate cloud work function
!
   do k = kts+1, km1
     do i = its,ite
       if (cnvflg(i)) then
         if(k.ge.kbcon(i).and.k.lt.ktcon(i)) then
           dz1 = zl(i,k+1) - zl(i,k)        
           gamma = el2orc * qeso(i,k) / (to(i,k)**2)
           rfact =  1. + fv_ * cp_ * gamma * to(i,k) / hvap_
           aa1(i) = aa1(i) + dz1 * (g_ / (cp_ * to(i,k)))                      &
                  * dbyo(i,k) / (1. + gamma) * rfact
           val = 0.
           aa1(i)=aa1(i)+ dz1 * g_ * fv_ * max(val,(qeso(i,k) - qo(i,k)))
         endif
       endif
     enddo
   enddo
!
   do i = its,ite
     if(cnvflg(i).and.aa1(i).le.0.) cnvflg(i) = .false.
   enddo
!
   totflg = .true.
   do i=its,ite
     totflg = totflg .and. (.not. cnvflg(i))
   enddo
   if(totflg) return
!
!  estimate the convective overshooting as the level
!    where the [aafac * cloud work function] becomes zero,
!    which is the final cloud top limited to the level of sigma=0.7
!
   do i = its,ite
     if (cnvflg(i)) then
       aa1(i) = aafac * aa1(i)
     endif
   enddo
!
   do i = its,ite
     flg(i) = cnvflg(i)
     ktcon1(i) = kbm(i)
   enddo
!
   do k = kts+1,km1
     do i = its,ite
       if (flg(i)) then
         if(k.ge.ktcon(i).and.k.lt.kbm(i)) then
           dz1 = zl(i,k+1) - zl(i,k)
           gamma = el2orc * qeso(i,k) / (to(i,k)**2)
           rfact =  1. + fv_ * cp_ * gamma                                     &
                   * to(i,k) / hvap_
           aa1(i) = aa1(i) +                                                   &
                   dz1 * (g_ / (cp_ * to(i,k)))                                &
                   * dbyo(i,k) / (1. + gamma) * rfact
           if(aa1(i).lt.0.) then
             ktcon1(i) = k
             flg(i) = .false.
           endif
         endif
       endif
     enddo
   enddo
!
!  compute cloud moisture property, detraining cloud water
!    and precipitation in overshooting layers
!
   do k = kts+1,km1
     do i = its,ite
       if (cnvflg(i)) then
         if(k.ge.ktcon(i).and.k.lt.ktcon1(i)) then
           dz    = zi(i,k+1) - zi(i,k)
           gamma = el2orc * qeso(i,k) / (to(i,k)**2)
           qrch = qeso(i,k)                                                    &
                + gamma * dbyo(i,k) / (hvap_ * (1. + gamma))
           tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1)) * dz
           tem1 = 0.5 * xlamud(i) * dz
           factor = 1. + tem - tem1
           qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*                         &
                       (qo(i,k)+qo(i,k-1)))/factor
           dq = eta(i,k) * (qcko(i,k) - qrch)
!
!  check if there is excess moisture to release latent heat
!
           if(dq.gt.0.) then
             etah = .5 * (eta(i,k) + eta(i,k-1))
             if(ncloud.gt.0) then
               dp = 1000. * del(i,k)
               qlk = dq / (eta(i,k) + etah * (c0 + c1) * dz)
               dellal(i,k) = etah * c1 * dz * qlk * g_ / dp
             else
               qlk = dq / (eta(i,k) + etah * c0 * dz)
             endif
             qcko(i,k) = qlk + qrch
             pwo(i,k) = etah * c0 * dz * qlk
           endif
         endif
       endif
     enddo
   enddo
!
! exchange ktcon with ktcon1
!
   do i = its,ite
     if(cnvflg(i)) then
       kk = ktcon(i)
       ktcon(i) = ktcon1(i)
       ktcon1(i) = kk
     endif
   enddo
!
!  this section is ready for cloud water
!
   if(ncloud.gt.0) then
!
!  compute liquid and vapor separation at cloud top
!
     do i = its,ite
       if(cnvflg(i)) then
         k = ktcon(i) - 1
         gamma = el2orc * qeso(i,k) / (to(i,k)**2)
         qrch = qeso(i,k)                                                      &
              + gamma * dbyo(i,k) / (hvap_ * (1. + gamma))
         dq = qcko(i,k) - qrch
!
!  check if there is excess moisture to release latent heat
!
         if(dq.gt.0.) then
           qlko_ktcon(i) = dq
           qcko(i,k) = qrch
         endif
       endif
     enddo
!
   endif
!
!--- compute precipitation efficiency in terms of windshear
!
   do i = its,ite
     if(cnvflg(i)) then
       vshear(i) = 0.
     endif
   enddo
!
   do k = kts+1,kte
     do i = its,ite
       if (cnvflg(i)) then
         if(k.gt.kb(i).and.k.le.ktcon(i)) then
           shear= sqrt((uo(i,k)-uo(i,k-1)) ** 2 + (vo(i,k)-vo(i,k-1)) ** 2)
           vshear(i) = vshear(i) + shear
         endif
       endif
     enddo
   enddo
!
   do i = its,ite
     if(cnvflg(i)) then
       vshear(i) = 1.e3 * vshear(i) / (zi(i,ktcon(i)+1)-zi(i,kb(i)+1))
       e1=1.591-.639*vshear(i)                                                 &
             +.0953*(vshear(i)**2)-.00496*(vshear(i)**3)
       edt(i)=1.-e1
       val =         .9
       edt(i) = min(edt(i),val)
       val =         .0
       edt(i) = max(edt(i),val)
     endif
   enddo
!
!--- what would the change be, that a cloud with unit mass
!--- will do to the environment?
!
   do k = kts,kte
     do i = its,ite
       if(cnvflg(i) .and. k .le. kmax(i)) then
         dellah(i,k) = 0.
         dellaq(i,k) = 0.
         dellau(i,k) = 0.
         dellav(i,k) = 0.
       endif
     enddo
   enddo
!
!--- changed due to subsidence and entrainment
!
   do k = kts+1,km1
     do i = its,ite
       if (cnvflg(i)) then
         if(k.gt.kb(i).and.k.lt.ktcon(i)) then
           dp = 1000. * del(i,k)
           dz = zi(i,k+1) - zi(i,k)
!
           dv1h = heo(i,k)
           dv2h = .5 * (heo(i,k) + heo(i,k-1))
           dv3h = heo(i,k-1)
           dv1q = qo(i,k)
           dv2q = .5 * (qo(i,k) + qo(i,k-1))
           dv3q = qo(i,k-1)
           dv1u = uo(i,k)
           dv2u = .5 * (uo(i,k) + uo(i,k-1))
           dv3u = uo(i,k-1)
           dv1v = vo(i,k)
           dv2v = .5 * (vo(i,k) + vo(i,k-1))
           dv3v = vo(i,k-1)
!
           tem  = 0.5 * (xlamue(i,k)+xlamue(i,k-1))
           tem1 = xlamud(i)
!
           dellah(i,k) = dellah(i,k) +                                         &
          ( eta(i,k)*dv1h - eta(i,k-1)*dv3h                                    &
         -  tem*eta(i,k-1)*dv2h*dz                                             &
         +  tem1*eta(i,k-1)*.5*(hcko(i,k)+hcko(i,k-1))*dz ) *g_/dp
!
           dellaq(i,k) = dellaq(i,k) +                                         &
          ( eta(i,k)*dv1q - eta(i,k-1)*dv3q                                    &
         -  tem*eta(i,k-1)*dv2q*dz                                             &
         +  tem1*eta(i,k-1)*.5*(qcko(i,k)+qcko(i,k-1))*dz ) *g_/dp
!
           dellau(i,k) = dellau(i,k) +                                         &
          ( eta(i,k)*dv1u - eta(i,k-1)*dv3u                                    &
         -  tem*eta(i,k-1)*dv2u*dz                                             &
         +  tem1*eta(i,k-1)*.5*(ucko(i,k)+ucko(i,k-1))*dz                      &
         -  pgcon*eta(i,k-1)*(dv1u-dv3u) ) *g_/dp
!
           dellav(i,k) = dellav(i,k) +                                         &
          ( eta(i,k)*dv1v - eta(i,k-1)*dv3v                                    &
         -  tem*eta(i,k-1)*dv2v*dz                                             &
         +  tem1*eta(i,k-1)*.5*(vcko(i,k)+vcko(i,k-1))*dz                      &
         -  pgcon*eta(i,k-1)*(dv1v-dv3v) ) *g_/dp
!
         endif
       endif
     enddo
   enddo
!
!------- cloud top
!
   do i = its,ite
     if(cnvflg(i)) then
       indx = ktcon(i)
       dp = 1000. * del(i,indx)
       dv1h = heo(i,indx-1)
       dellah(i,indx) = eta(i,indx-1) *                                        &
                       (hcko(i,indx-1) - dv1h) * g_ / dp
       dv1q = qo(i,indx-1)
       dellaq(i,indx) = eta(i,indx-1) *                                        &
                       (qcko(i,indx-1) - dv1q) * g_ / dp
       dv1u = uo(i,indx-1)
       dellau(i,indx) = eta(i,indx-1) *                                        &
                       (ucko(i,indx-1) - dv1u) * g_ / dp
       dv1v = vo(i,indx-1)
       dellav(i,indx) = eta(i,indx-1) *                                        &
                       (vcko(i,indx-1) - dv1v) * g_ / dp
!
!  cloud water
!
       dellal(i,indx) = eta(i,indx-1) *                                        &
                       qlko_ktcon(i) * g_ / dp
     endif
   enddo
!
!  mass flux at cloud base for shallow convection
!  (Grant, 2001)
!
   do i= its,ite
     if(cnvflg(i)) then
       k = kbcon(i)
       ptem = g_*sflx(i)*hpbl(i)/t1(i,1)
       wstar(i) = ptem**h1
       tem = po(i,k)*100. / (rd_*t1(i,k))
       xmb(i) = betaw*tem*wstar(i)
       xmb(i) = min(xmb(i),xmbmax(i))
     endif
   enddo
!
   do k = kts,kte
     do i = its,ite
       if (cnvflg(i) .and. k .le. kmax(i)) then
         qeso(i,k)=0.01* fpvs(t1(i,k),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
         qeso(i,k) = eps * qeso(i,k) / (p(i,k) + (eps-1.)*qeso(i,k))
         val     =             1.e-8
         qeso(i,k) = max(qeso(i,k), val )
       endif
     enddo
   enddo
!
   do i = its,ite
     delhbar(i) = 0.
     delqbar(i) = 0.
     deltbar(i) = 0.
     delubar(i) = 0.
     delvbar(i) = 0.
     qcond(i) = 0.
   enddo
!
   do k = kts,kte
     do i = its,ite
       if (cnvflg(i)) then
         if(k.gt.kb(i).and.k.le.ktcon(i)) then
           dellat = (dellah(i,k) - hvap_ * dellaq(i,k)) / cp_
           t1(i,k) = t1(i,k) + dellat * xmb(i) * dt2
           q1(i,k) = q1(i,k) + dellaq(i,k) * xmb(i) * dt2
           tem = 1./rcs
           u1(i,k) = u1(i,k) + dellau(i,k) * xmb(i) * dt2 * tem
           v1(i,k) = v1(i,k) + dellav(i,k) * xmb(i) * dt2 * tem
           dp = 1000. * del(i,k)
           delhbar(i) = delhbar(i) + dellah(i,k)*xmb(i)*dp/g_
           delqbar(i) = delqbar(i) + dellaq(i,k)*xmb(i)*dp/g_
           deltbar(i) = deltbar(i) + dellat*xmb(i)*dp/g_
           delubar(i) = delubar(i) + dellau(i,k)*xmb(i)*dp/g_
           delvbar(i) = delvbar(i) + dellav(i,k)*xmb(i)*dp/g_
         endif
       endif
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       if (cnvflg(i)) then
         if(k.gt.kb(i).and.k.le.ktcon(i)) then
           qeso(i,k)=0.01* fpvs(t1(i,k),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls    &
                     ,psat,t0c_)
           qeso(i,k) = eps * qeso(i,k)/(p(i,k) + (eps-1.)*qeso(i,k))
           val     =             1.e-8
           qeso(i,k) = max(qeso(i,k), val )
         endif
       endif
     enddo
   enddo
!
   do i = its,ite
     rntot(i) = 0.
     delqev(i) = 0.
     delq2(i) = 0.
     flg(i) = cnvflg(i)
   enddo
!
   do k = kte,kts,-1
     do i = its,ite
       if (cnvflg(i)) then
         if(k.lt.ktcon(i).and.k.gt.kb(i)) then
           rntot(i) = rntot(i) + pwo(i,k) * xmb(i) * .001 * dt2
         endif
       endif
     enddo
   enddo
!
! evaporating rain
!
   do k = kte,kts,-1
     do i = its,ite
       if (k .le. kmax(i)) then
         deltv(i) = 0.
         delq(i) = 0.
         qevap(i) = 0.
         if(cnvflg(i)) then
           if(k.lt.ktcon(i).and.k.gt.kb(i)) then
             rain(i) = rain(i) + pwo(i,k) * xmb(i) * .001 * dt2
           endif
         endif
         if(flg(i).and.k.lt.ktcon(i)) then
           evef = edt(i) * evfact
           if(slimsk(i).eq.1.) evef=edt(i) * evfactl
           qcond(i) = evef * (q1(i,k) - qeso(i,k))                             &
                    / (1. + el2orc * qeso(i,k) / t1(i,k)**2)
           dp = 1000. * del(i,k)
           if(rain(i).gt.0..and.qcond(i).lt.0.) then
             qevap(i) = -qcond(i) * (1.-exp(-.32*sqrt(dt2*rain(i))))
             qevap(i) = min(qevap(i), rain(i)*1000.*g_/dp)
             delq2(i) = delqev(i) + .001 * qevap(i) * dp / g_
           endif
           if(rain(i).gt.0..and.qcond(i).lt.0..and.delq2(i).gt.rntot(i)) then
             qevap(i) = 1000.* g_ * (rntot(i) - delqev(i)) / dp
             flg(i) = .false.
           endif
           if(rain(i).gt.0..and.qevap(i).gt.0.) then
             tem  = .001 * dp / g_
             tem1 = qevap(i) * tem
             if(tem1.gt.rain(i)) then
               qevap(i) = rain(i) / tem
               rain(i) = 0.
             else
               rain(i) = rain(i) - tem1
             endif
             q1(i,k) = q1(i,k) + qevap(i)
             t1(i,k) = t1(i,k) - (hvap_/cp_) * qevap(i)
             deltv(i) = - (hvap_/cp_)*qevap(i)/dt2
             delq(i) =  + qevap(i)/dt2
             delqev(i) = delqev(i) + .001*dp*qevap(i)/g_
           endif
           dellaq(i,k) = dellaq(i,k) + delq(i) / xmb(i)
           delqbar(i) = delqbar(i) + delq(i)*dp/g_
           deltbar(i) = deltbar(i) + deltv(i)*dp/g_
         endif
       endif
     enddo
   enddo
!
   do i = its,ite
     if(cnvflg(i)) then
       if(rain(i).lt.0..or..not.flg(i)) rain(i) = 0.
       ktop(i) = ktcon(i)
       kbot(i) = kbcon(i)
       icps(i) = 0
     endif
   enddo
!
! cloud water
!
   if (ncloud.gt.0) then
!
     do k = kts,km1
       do i = its,ite
         if (cnvflg(i)) then
           if (k.ge.kbcon(i).and.k.le.ktcon(i)) then
             tem  = dellal(i,k) * xmb(i) * dt2
             tem1 = max(0.0, min(1.0, (tcr-t1(i,k))*tcrf))
             if (ncloud.ge.2) then
               qi2(i,k) = qi2(i,k) + tem * tem1            ! ice
               qc2(i,k) = qc2(i,k) + tem *(1.0-tem1)       ! water
             else
               qc2(i,k) = qc2(i,k) + tem
             endif
           endif
         endif
       enddo
     enddo
!
   endif
!
      end subroutine nscv2d
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   REAL FUNCTION fpvs(t,ice,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c)
!-------------------------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------------------------
   REAL :: t,rd,rv,cvap,cliq,cice,hvap,hsub,psat,t0c,dldt,xa,xb,dldti,         &
           xai,xbi,ttp,tr
   INTEGER :: ice
!
   ttp=t0c+0.01
   dldt=cvap-cliq
   xa=-dldt/rv
   xb=xa+hvap/(rv*ttp)
   dldti=cvap-cice
   xai=-dldti/rv
   xbi=xai+hsub/(rv*ttp)
   tr=ttp/t
   if(t.lt.ttp.and.ice.eq.1) then
     fpvs=psat*(tr**xai)*exp(xbi*(1.-tr))
   else
     fpvs=psat*(tr**xa)*exp(xb*(1.-tr))
   endif
!
   if (t.lt.180.) then
     tr=ttp/180.
     if(t.lt.ttp.and.ice.eq.1) then
       fpvs=psat*(tr**xai)*exp(xbi*(1.-tr))
     else
       fpvs=psat*(tr**xa)*exp(xb*(1.-tr))
     endif
   endif
!
   if (t.ge.330.) then
     tr=ttp/330
     if(t.lt.ttp.and.ice.eq.1) then
       fpvs=psat*(tr**xai)*exp(xbi*(1.-tr))
     else
       fpvs=psat*(tr**xa)*exp(xb*(1.-tr))
     endif
   endif
!
   END FUNCTION fpvs
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine nscvinit(rthshten,rqvshten,rqcshten,rqishten,                    &
                      rushten,rvshten,                                         &
                      restart,p_qc,p_qi,p_first_scalar,                        &
                      allowed_to_read,                                         &
                      ids, ide, jds, jde, kds, kde,                            &
                      ims, ime, jms, jme, kms, kme,                            &
                      its, ite, jts, jte, kts, kte                  )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   logical , intent(in)           ::  allowed_to_read,restart
   integer , intent(in)           ::  ids, ide, jds, jde, kds, kde,            &
                                      ims, ime, jms, jme, kms, kme,            &
                                      its, ite, jts, jte, kts, kte
   integer , intent(in)           ::  p_first_scalar, p_qi, p_qc
   real,     dimension( ims:ime , kms:kme , jms:jme ) , intent(out) ::         &
                                                              rthshten,        &
                                                              rqvshten,        &
                                                               rushten,        &
                                                               rvshten,        &
                                                              rqcshten,        &
                                                              rqishten
   integer :: i, j, k, itf, jtf, ktf
!
   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)
!
   if(.not.restart)then
     do j = jts,jtf
       do k = kts,ktf
         do i = its,itf
           rthshten(i,k,j)=0.
           rqvshten(i,k,j)=0.
           rushten(i,k,j)=0.
           rvshten(i,k,j)=0.
         enddo
       enddo
     enddo
!
     if (p_qc .ge. p_first_scalar) then
       do j = jts,jtf
         do k = kts,ktf
           do i = its,itf
             rqcshten(i,k,j)=0.
           enddo
         enddo
       enddo
     endif
!
     if (p_qi .ge. p_first_scalar) then
       do j = jts,jtf
         do k = kts,ktf
           do i = its,itf
             rqishten(i,k,j)=0.
           enddo
         enddo
       enddo
     endif
   endif
!
   end subroutine nscvinit
!-------------------------------------------------------------------------------
!
END MODULE module_shcu_nscv
!
