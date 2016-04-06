!
!
!
!
MODULE module_cu_nsas
CONTAINS
!-------------------------------------------------------------------------------
   subroutine cu_nsas(dt,dx,p3di,p3d,pi3d,qc3d,qi3d,rho3d,itimestep,stepcu,    &
                     hbot,htop,cu_act_flag,                                    &
                     rthcuten,rqvcuten,rqccuten,rqicuten,                      &
                     rucuten,rvcuten,                                          &
                     qv3d,t3d,raincv,pratec,xland,dz8w,w,u3d,v3d,              &
                     hpbl,hfx,qfx,                                             &
                     mp_physics,dx_factor_nsas,                                &
                     p_qc,p_qi,p_first_scalar,                                 &
                     pgcon,                                                    &
                     cp,cliq,cpv,g,xlv,r_d,r_v,ep_1,ep_2,                      &
                     cice,xls,psat,f_qi,f_qc,                                  &
                     ids,ide, jds,jde, kds,kde,                                &
                     ims,ime, jms,jme, kms,kme,                                &
                     its,ite, jts,jte, kts,kte)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!-- dt          time step (s)
!-- dx          grid interval (m)
!-- p3di        3d pressure (pa) at interface level
!-- p3d         3d pressure (pa)
!-- pi3d        3d exner function (dimensionless)
!-- z           height above sea level (m)
!-- qc3d        cloud water mixing ratio (kg/kg)
!-- qi3d        cloud ice mixing ratio (kg/kg)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- t3d         temperature (k)
!-- raincv      cumulus scheme precipitation (mm)
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
                                      its,ite, jts,jte, kts,kte,               &
                                      itimestep, stepcu,                       &
                                      p_qc,p_qi,p_first_scalar
   real,     intent(in   )   ::      cp,cliq,cpv,g,xlv,r_d,r_v,ep_1,ep_2,      &
                                     cice,xls,psat
   real,     intent(in   )   ::      dt,dx
   real,     optional, intent(in ) :: pgcon
   real,     dimension( ims:ime, kms:kme, jms:jme ),optional                  ,&
             intent(inout)   ::                                       rthcuten,&
                                                                       rucuten,&
                                                                       rvcuten,&
                                                                      rqccuten,&
                                                                      rqicuten,&
                                                                      rqvcuten
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
   real,     dimension( ims:ime, jms:jme )                                    ,&
             intent(inout) ::                                           raincv,&
                                                                        pratec
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
   logical,  dimension( ims:ime, jms:jme )                                    ,&
             intent(inout) ::                                      cu_act_flag
!
   real,     dimension( ims:ime, jms:jme )                                    ,&
              intent(in   )   ::                                          hpbl,&
                                                                           hfx,&
                                                                           qfx
   integer,   intent(in   )   ::                                    mp_physics
   integer,   intent(in   )   ::                                dx_factor_nsas 
   integer :: ncloud
!
!local
!
   real,  dimension( its:ite, jts:jte )  ::                            raincv1,&
                                                                       raincv2,&
                                                                       pratec1,&
                                                                       pratec2
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
   do j = jts,jte
     do i = its,ite
       cu_act_flag(i,j)=.TRUE.
     enddo
   enddo
   delt=dt*stepcu
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
! NCEP SAS 
!
     call nsas2d(delt=dt,delx=dx,del=del(its,kts),                             &
              prsl=prsll(its,kts),prsi=prsii(its,kts),prslk=pi3d(ims,kms,j),   &
              zl=zll(its,kts),zi=zii(its,kts),                                 &
              ncloud=ncloud,qc2=qc2(its,kts),qi2=qi2(its,kts),                 &
              q1=q1(its,kts),t1=t1(its,kts),rain=rain(its),                    &
              kbot=kbot(its),ktop=ktop(its),                                   &
              icps=icps(its),                                                  &
              lat=j,slimsk=xland(ims,j),dot=dot(its,kts),                      &
              u1=u1(its,kts), v1=v1(its,kts),                                  &
              cp_=cp,cliq_=cliq,cvap_=cpv,g_=g,hvap_=xlv,                      &
              rd_=r_d,rv_=r_v,fv_=ep_1,ep2=ep_2,                               &
              cice=cice,xls=xls,psat=psat,                                     &
              pgcon=pgcon_use,                                                 &
              dx_factor_nsas=dx_factor_nsas,                                   &
              ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde,               &
              ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme,               &
              its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )
!
     do i = its,ite
       pratec1(i,j)=rain(i)*1000./(stepcu*dt)
       raincv1(i,j)=rain(i)*1000./(stepcu)
     enddo
!
! NCEP SCV
!
     call nscv2d(delt=dt,del=del(its,kts),prsl=prsll(its,kts),                 &
              prsi=prsii(its,kts),prslk=pi3d(ims,kms,j),zl=zll(its,kts),       &
              zi=zii(its,kts),ncloud=ncloud,qc2=qc2(its,kts),qi2=qi2(its,kts), &
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
       pratec2(i,j)=rain(i)*1000./(stepcu*dt)
       raincv2(i,j)=rain(i)*1000./(stepcu)
     enddo
!
     do i = its,ite
       raincv(i,j) = raincv1(i,j) + raincv2(i,j)
       pratec(i,j) = pratec1(i,j) + pratec2(i,j)
       hbot(i,j) = kbot(i)
       htop(i,j) = ktop(i)
     enddo
!
     IF(PRESENT(rthcuten).AND.PRESENT(rqvcuten)) THEN
       do k = kts,kte
         do i = its,ite
           rthcuten(i,k,j)=(t1(i,k)-t3d(i,k,j))/pi3d(i,k,j)*rdelt
           rqvcuten(i,k,j)=(q1(i,k)-qv3d(i,k,j))*rdelt
         enddo
       enddo
     ENDIF
!
     IF(PRESENT(rucuten).AND.PRESENT(rvcuten)) THEN
       do k = kts,kte
         do i = its,ite
           rucuten(i,k,j)=(u1(i,k)-u3d(i,k,j))*rdelt
           rvcuten(i,k,j)=(v1(i,k)-v3d(i,k,j))*rdelt
         enddo
       enddo
     ENDIF
!
     IF(PRESENT( rqicuten )) THEN
       IF ( F_QI ) THEN
         do k = kts,kte
           do i = its,ite
             rqicuten(i,k,j)=(qi2(i,k)-qi3d(i,k,j))*rdelt
           enddo
         enddo
       ENDIF
     ENDIF
!
     IF(PRESENT( rqccuten )) THEN
       IF ( F_QC ) THEN
         do k = kts,kte
           do i = its,ite
             rqccuten(i,k,j)=(qc2(i,k)-qc3d(i,k,j))*rdelt
           enddo
         enddo
       ENDIF
     ENDIF
!
   enddo ! outer most J_loop
!
   return
   end subroutine cu_nsas
!
!-------------------------------------------------------------------------------
! NCEP SAS (Deep Convection Scheme)
!-------------------------------------------------------------------------------
   subroutine nsas2d(delt,delx,del,prsl,prsi,prslk,zl,zi,                      &
            ncloud,                                                            & 
            qc2,qi2,                                                           & 
            q1,t1,rain,kbot,ktop,                                              &
            icps,                                                              &
            lat,slimsk,dot,u1,v1,cp_,cliq_,cvap_,g_,hvap_,rd_,rv_,fv_,ep2,     &
            cice,xls,psat,                                                     &
            pgcon,                                                             &
            dx_factor_nsas,                                                    &
            ids,ide, jds,jde, kds,kde,                                         &
            ims,ime, jms,jme, kms,kme,                                         &
            its,ite, jts,jte, kts,kte)
!-------------------------------------------------------------------------------
!
! subprogram:    phys_cps_sas      computes convective heating and moistening
!                                                      and momentum transport
!
! abstract: computes convective heating and moistening using a one
!   cloud type arakawa-schubert convection scheme originally developed
!   by georg grell. the scheme has been revised at ncep since initial 
!   implementation in 1993. it includes updraft and downdraft effects.
!   the closure is the cloud work function. both updraft and downdraft
!   are assumed to be saturated and the heating and moistening are
!   accomplished by the compensating environment. convective momentum transport
!   is taken into account. the name comes from "simplified arakawa-schubert
!   convection parameterization".
!
! developed by hua-lu pan, wan-shu wu, songyou hong, and jongil han
!   implemented into wrf by kyosun sunny lim and songyou hong
!   module with cpp-based options is available in grims 
!
! program history log:
!   92-03-01  hua-lu pan       operational development
!   96-03-01  song-you hong    revised closure, and trigger 
!   99-03-01  hua-lu pan       multiple clouds
!   06-03-01  young-hwa byun   closure based on moisture convergence (optional)
!   09-10-01  jung-eun kim     f90 format with standard physics modules
!   10-07-01  jong-il han      revised cloud model,trigger, as in gfs july 2010
!   10-12-01  kyosun sunny lim wrf compatible version
!   14-01-09  song-you hong    dx dependent trigger, closure, and mass flux
!
!
! usage:    call phys_cps_sas(delt,delx,del,prsl,prsi,prslk,prsik,zl,zi,       &
!                             q2,q1,t1,u1,v1,rcs,slimsk,dot,cldwrk,rain,       &
!                             jcap,ncloud,lat,kbot,ktop,icps,                  &
!                             ids,ide, jds,jde, kds,kde,                       &
!                             ims,ime, jms,jme, kms,kme,                       &
!                             its,ite, jts,jte, kts,kte)
!
!   delt     - real model integration time step
!   delx     - real model grid interval
!   del      - real (kms:kme) sigma layer thickness
!   prsl     - real (ims:ime,kms:kme) pressure values
!   prsi     - real (ims:ime,kms:kme) pressure values at interface level
!   prslk    - real (ims:ime,kms:kme) pressure values to the kappa
!   prsik    - real (ims:ime,kms:kme) pressure values to the kappa at interface lev.
!   zl       - real (ims:ime,kms:kme) height above sea level
!   zi       - real (ims:ime,kms:kme) height above sea level at interface level
!   rcs      - real
!   slimsk   - real (ims:ime) land(1),sea(0), ice(2) flag
!   dot      - real (ims:ime,kms:kme) vertical velocity
!   jcap     - integer wave number 
!   ncloud   - integer no_cloud(0),no_ice(1),cloud+ice(2) 
!   lat      - integer  current latitude index
!
! output argument list:
!   q2       - real (ims:ime,kms:kme) detrained hydrometeors in kg/kg
!            - in case of the  --> qc2(cloud), qi2(ice)
!   q1       - real (ims:ime,kms:kme) adjusted specific humidity in kg/kg
!   t1       - real (ims:ime,kms:kme) adjusted temperature in kelvin
!   u1       - real (ims:ime,kms:kme) adjusted zonal wind in m/s
!   v1       - real (ims:ime,kms:kme) adjusted meridional wind in m/s
!   cldwrk   - real (ims:ime) cloud work function
!   rain     - real (ims:ime) convective rain in meters
!   kbot     - integer (ims:ime) cloud bottom level
!   ktop     - integer (ims:ime) cloud top level
!   icps     - integer (ims:ime) bit flag indicating deep convection
!
! subprograms called:
!   fpvs     - function to compute saturation vapor pressure
!
! remarks: function fpvs is inlined by fpp.
!          nonstandard automatic arrays are used.
!
! references :
!   pan and wu    (1995, ncep office note)
!   hong and pan  (1998, mon wea rev)
!   park and hong (2007,jmsj)
!   byun and hong (2007, mon wea rev)
!   han and pan   (2011, wea. forecasting)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
! model tunable parameters 
!
   real,parameter  ::  alphal = 0.5,    alphas = 0.5
   real,parameter  ::  betal  = 0.05,   betas  = 0.05
   real,parameter  ::  pdpdwn = 0.0,    pdetrn = 200.0
   real,parameter  ::  c0     = 0.002,  c1     = 0.002
   real,parameter  ::  xlamdd = 1.0e-4, xlamde = 1.0e-4
   real,parameter  ::  clam   = 0.1,    cxlamu = 1.0e-4
   real,parameter  ::  aafac  = 0.1
   real,parameter  ::  dthk=25.
   real,parameter  ::  cincrmax = 180.,cincrmin = 120.
   real,parameter  ::  mbdt = 10., edtmaxl = 0.3, edtmaxs = 0.3
   real,parameter  ::  evfacts = 0.3, evfactl = 0.3
!
   real,parameter  ::  tf=233.16,tcr=263.16,tcrf=1.0/(tcr-tf)
   real,parameter  ::  xk1=2.e-5,xlhor=3.e4,xhver=5000.,theimax=1.
   real,parameter  ::  xc1=1.e-7,xc2=1.e4,xc3=3.e3,ecesscr=3.0,edtk1=3.e4
!
!  passing variables
!
   real            ::  cp_,cliq_,cvap_,g_,hvap_,rd_,rv_,fv_,ep2
   real            ::  pi_,qmin_,t0c_,cice,xlv0,xls,psat
   real            ::  pgcon
   integer         ::  dx_factor_nsas
   integer         ::  lat,                                                    &
                       ncloud,                                                 &
                       ids,ide, jds,jde, kds,kde,                              &
                       ims,ime, jms,jme, kms,kme,                              &
                       its,ite, jts,jte, kts,kte
!
   real            ::  delt,rcs
   real            ::  del(its:ite,kts:kte),                                   &
                       prsl(its:ite,kts:kte),prslk(ims:ime,kms:kme),           &
                       prsi(its:ite,kts:kte+1),                                &
                       zl(its:ite,kts:kte),zi(its:ite,kts:kte+1),              &
                       q1(its:ite,kts:kte),t1(its:ite,kts:kte),                &
                       u1(its:ite,kts:kte),v1(its:ite,kts:kte),                &
                       dot(its:ite,kts:kte)
   real            ::  qi2(its:ite,kts:kte)
   real            ::  qc2(its:ite,kts:kte)
!
   real            ::  rain(its:ite)
   integer         ::  kbot(its:ite),ktop(its:ite),icps(its:ite)
   real            ::  slimsk(ims:ime)
!
!
!  local variables and arrays
!
   integer         ::  i,k,kmax,kbmax,kbm,jmn,indx,indp,kts1,kte1,kmax1,kk
   real            ::  p(its:ite,kts:kte),pdot(its:ite),acrtfct(its:ite)
   real            ::  uo(its:ite,kts:kte),vo(its:ite,kts:kte)
   real            ::  to(its:ite,kts:kte),qo(its:ite,kts:kte)
   real            ::  hcko(its:ite,kts:kte)
   real            ::  qcko(its:ite,kts:kte),eta(its:ite,kts:kte)
   real            ::  etad(its:ite,kts:kte)
   real            ::  qrcdo(its:ite,kts:kte)
   real            ::  pwo(its:ite,kts:kte),pwdo(its:ite,kts:kte)
   real            ::  dtconv(its:ite)
   real            ::  deltv(its:ite),acrt(its:ite)
   real            ::  qeso(its:ite,kts:kte)
   real            ::  tvo(its:ite,kts:kte),dbyo(its:ite,kts:kte)
   real            ::  heo(its:ite,kts:kte),heso(its:ite,kts:kte)
   real            ::  qrcd(its:ite,kts:kte)
   real            ::  dellah(its:ite,kts:kte),dellaq(its:ite,kts:kte)
!
   integer         ::  kb(its:ite),kbcon(its:ite)
   integer         ::  kbcon1(its:ite)
   real            ::  hmax(its:ite),delq(its:ite)
   real            ::  hkbo(its:ite),qkbo(its:ite),pbcdif(its:ite)
   integer         ::  kbds(its:ite),lmin(its:ite),jmin(its:ite)
   integer         ::  ktcon(its:ite)
   integer         ::  ktcon1(its:ite)
   integer         ::  kbdtr(its:ite)
   integer         ::  klcl(its:ite),ktdown(its:ite)
   real            ::  vmax(its:ite)
   real            ::  hmin(its:ite),pwavo(its:ite)
   real            ::  aa1(its:ite),vshear(its:ite)
   real            ::  qevap(its:ite)
   real            ::  edt(its:ite)
   real            ::  edto(its:ite),pwevo(its:ite)
   real            ::  qcond(its:ite)
   real            ::  hcdo(its:ite,kts:kte)
   real            ::  ddp(its:ite),pp2(its:ite)
   real            ::  qcdo(its:ite,kts:kte)
   real            ::  adet(its:ite),aatmp(its:ite)
   real            ::  xhkb(its:ite),xqkb(its:ite)
   real            ::  xpwav(its:ite),xpwev(its:ite),xhcd(its:ite,kts:kte)
   real            ::  xaa0(its:ite),f(its:ite),xk(its:ite)
   real            ::  xmb(its:ite)
   real            ::  edtx(its:ite),xqcd(its:ite,kts:kte)
   real            ::  hsbar(its:ite),xmbmax(its:ite)
   real            ::  xlamb(its:ite,kts:kte),xlamd(its:ite)
   real            ::  excess(its:ite)
   real            ::  plcl(its:ite)
   real            ::  delhbar(its:ite),delqbar(its:ite),deltbar(its:ite)
   real,save       ::  pcrit(15), acritt(15)
   real            ::  acrit(15)
   real            ::  qcirs(its:ite,kts:kte),qrski(its:ite)
   real            ::  dellal(its:ite,kts:kte)
   real            ::  rntot(its:ite),delqev(its:ite),delq2(its:ite) 
!
   real            ::  fent1(its:ite,kts:kte),fent2(its:ite,kts:kte)
   real            ::  frh(its:ite,kts:kte)
   real            ::  xlamud(its:ite),sumx(its:ite)
   real            ::  aa2(its:ite)
   real            ::  ucko(its:ite,kts:kte),vcko(its:ite,kts:kte)
   real            ::  ucdo(its:ite,kts:kte),vcdo(its:ite,kts:kte)
   real            ::  dellau(its:ite,kts:kte),dellav(its:ite,kts:kte)
   real            ::  delubar(its:ite),delvbar(its:ite)
   real            ::  qlko_ktcon(its:ite)
!
   real            ::  alpha,beta,                                             &
                       dt2,dtmin,dtmax,dtmaxl,dtmaxs,                          &
                       el2orc,eps,fact1,fact2,                                 &
                       tem,tem1,cincr
   real            ::  dz,dp,es,pprime,qs,                                     &
                       dqsdp,desdt,dqsdt,gamma,                                &
                       dt,dq,po,thei,delx,delza,dzfac,                         &
                       thec,theb,thekb,thekh,theavg,thedif,                    &
                       omgkb,omgkbp1,omgdif,omgfac,heom,rh,thermal,chi,        &
                       factor,onemf,dz1,qrch,etah,qlk,qc,rfact,shear,          &
                       e1,dh,deta,detad,theom,edtmax,dhh,dg,aup,adw,           &
                       dv1,dv2,dv3,dv1q,dv2q,dv3q,dvq1,                        &
                       dv1u,dv2u,dv3u,dv1v,dv2v,dv3v,                          &
                       dellat,xdby,xqrch,xqc,xpw,xpwd,                         &
                       W1l,W2l,W3l,W4l,W1s,W2s,W3s,W4s,                        & 
                       w1,w2,w3,w4,qrsk(its:ite,kts:kte),evef,ptem,ptem1
!
   logical         ::  totflg, cnvflg(its:ite),flg(its:ite),lclflg
   real            ::  dx_factor
!
!  climatological critical cloud work functions for closure
!
   data pcrit/850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,               &
              350.,300.,250.,200.,150./
   data acritt/.0633,.0445,.0553,.0664,.075,.1082,.1521,.2216,                 &
              .3151,.3677,.41,.5255,.7663,1.1686,1.6851/
!
!-----------------------------------------------------------------------
!
! define miscellaneous values
!
   pi_   = 3.14159
   qmin_ = 1.0e-30
   t0c_ = 273.15
   xlv0 = hvap_
   rcs  = 1.
   el2orc = hvap_*hvap_/(rv_*cp_)
   eps    = rd_/rv_
   fact1  = (cvap_-cliq_)/rv_
   fact2  = hvap_/rv_-fact1*t0c_
   kts1 = kts + 1
   kte1 = kte - 1
   dt2    = delt
   dtmin  = max(dt2,1200.)
   dtmax  = max(dt2,3600.)
!
   if (dx_factor_nsas == 1) then
   dx_factor = 250. / delx ! assume 2.5 ms-1 (1km) and 1.125 cms-1 (200km)
   W1l = dx_factor * 0.1 * (-1.)
   W2l = dx_factor * (-1.)
   W3l = dx_factor * (-1.)
   W4l = dx_factor * 0.1 * (-1.)
   W1s = W1l
   W2s = W2l
   W3s = W3l
   W4s = W4l
   else 
   W1l = -8.E-3
   W2l = -4.E-2
   W3l = -5.E-3
   W4l = -5.E-4
   W1s = -2.E-4
   W2s = -2.E-3
   W3s = -1.E-3
   W4s = -2.E-5
   endif
!
!  initialize arrays
!
   lclflg = .true.
   do i = its,ite
     rain(i)     = 0.0
     kbot(i)   = kte+1
     ktop(i)   = 0
     icps(i)   = 0
     cnvflg(i) = .true.
     dtconv(i) = 3600.
     pdot(i)   = 0.0
     edto(i)   = 0.0
     edtx(i)   = 0.0
     xmbmax(i) = 0.3
     excess(i) = 0.0
     plcl(i)   = 0.0
     aa2(i) = 0.0
     qlko_ktcon(i) = 0.0
     pbcdif(i)= 0.0
     lmin(i) = 1
     jmin(i) = 1
     edt(i) = 0.0
   enddo
!
   do k = 1,15
     acrit(k) = acritt(k) * (975. - pcrit(k))
   enddo
!
! Define top layer for search of the downdraft originating layer
! and the maximum thetae for updraft
!
   kbmax = kte 
   kbm   = kte 
   kmax  = kte 
   do k = kts,kte 
     do i = its,ite 
       if(prsl(i,k).gt.prsi(i,1)*0.45) kbmax = k + 1 
       if(prsl(i,k).gt.prsi(i,1)*0.70) kbm   = k + 1 
       if(prsl(i,k).gt.prsi(i,1)*0.04) kmax  = k + 1 
     enddo 
   enddo 
   kmax = min(kmax,kte)
   kmax1 = kmax - 1
   kbm = min(kbm,kte)
!
! convert surface pressure to mb from cb
!
   do k = kts,kte
     do i = its,ite
       pwo(i,k)  = 0.0
       pwdo(i,k) = 0.0
       dellal(i,k) = 0.0
       hcko(i,k) = 0.0
       qcko(i,k) = 0.0
       hcdo(i,k) = 0.0
       qcdo(i,k) = 0.0
     enddo
   enddo
!
   do k = kts,kmax
     do i = its,ite
       p(i,k) = prsl(i,k) * 10.
       pwo(i,k) = 0.0
       pwdo(i,k) = 0.0
       to(i,k) = t1(i,k)
       qo(i,k) = q1(i,k)
       dbyo(i,k) = 0.0
       fent1(i,k) = 1.0
       fent2(i,k) = 1.0
       frh(i,k) = 0.0
       ucko(i,k) = 0.0
       vcko(i,k) = 0.0
       ucdo(i,k) = 0.0
       vcdo(i,k) = 0.0
       uo(i,k) = u1(i,k) * rcs
       vo(i,k) = v1(i,k) * rcs
     enddo
   enddo
!
! column variables
!
!  p is pressure of the layer (mb)
!  t is temperature at t-dt (k)..tn
!  q is mixing ratio at t-dt (kg/kg)..qn
!  to is temperature at t+dt (k)... this is after advection and turbulan
!  qo is mixing ratio at t+dt (kg/kg)..q1
!
   do k = kts,kmax
     do i = its,ite
       qeso(i,k)=0.01*fpvs(to(i,k),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
       qeso(i,k) = eps * qeso(i,k) / (p(i,k) + (eps-1.) * qeso(i,k))
       qeso(i,k) = max(qeso(i,k),qmin_)
       qo(i,k)   = max(qo(i,k), 1.e-10 )
       tvo(i,k)  = to(i,k) + fv_ * to(i,k) * max(qo(i,k),qmin_)
     enddo
   enddo
!
! compute moist static energy
!
   do k = kts,kmax
     do i = its,ite
       heo(i,k)  = g_ * zl(i,k) + cp_* to(i,k) + hvap_ * qo(i,k)
       heso(i,k) = g_ * zl(i,k) + cp_* to(i,k) + hvap_ * qeso(i,k)
     enddo
   enddo
!
! Determine level with largest moist static energy
! This is the level where updraft starts
!
   do i = its,ite
     hmax(i) = heo(i,1)
     kb(i) = 1
   enddo
!
   do k = kts1,kbm
     do i = its,ite
       if(heo(i,k).gt.hmax(i)) then
         kb(i) = k
         hmax(i) = heo(i,k)
       endif
     enddo
   enddo
!
   do k = kts,kmax1
     do i = its,ite
       if(cnvflg(i)) then
         dz = .5 * (zl(i,k+1) - zl(i,k))
         dp = .5 * (p(i,k+1) - p(i,k))
         es = 0.01*fpvs(to(i,k+1),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
         pprime = p(i,k+1) + (eps-1.) * es
         qs = eps * es / pprime
         dqsdp = - qs / pprime
         desdt = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
         dqsdt = qs * p(i,k+1) * desdt / (es * pprime)
         gamma = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
         dt = (g_ * dz + hvap_ * dqsdp * dp) / (cp_ * (1. + gamma))
         dq = dqsdt * dt + dqsdp * dp
         to(i,k) = to(i,k+1) + dt
         qo(i,k) = qo(i,k+1) + dq
         po = .5 * (p(i,k) + p(i,k+1))
         qeso(i,k)=0.01*fpvs(to(i,k),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
         qeso(i,k) = eps * qeso(i,k) / (po + (eps-1.) * qeso(i,k))
         qeso(i,k) = max(qeso(i,k),qmin_)
         qo(i,k)   = max(qo(i,k), 1.e-10)
         frh(i,k)  = 1. - min(qo(i,k)/qeso(i,k), 1.)
         heo(i,k)  = .5 * g_ * (zl(i,k) + zl(i,k+1)) +                          &
                cp_ * to(i,k) + hvap_ * qo(i,k)
         heso(i,k) = .5 * g_ * (zl(i,k) + zl(i,k+1)) +                          &
                cp_ * to(i,k) + hvap_ * qeso(i,k)
         uo(i,k)   = .5 * (uo(i,k) + uo(i,k+1))
         vo(i,k)   = .5 * (vo(i,k) + vo(i,k+1))
       endif
     enddo
   enddo
!
! look for convective cloud base as the level of free convection
!
   do i = its,ite
     if(cnvflg(i)) then
       indx = kb(i)
       hkbo(i) = heo(i,indx)
       qkbo(i) = qo(i,indx)
     endif
   enddo
!
   do i = its,ite
     flg(i) = cnvflg(i)
     kbcon(i) = kmax
   enddo
!
   do k = kts,kbmax
     do i = its,ite
       if(flg(i).and.k.gt.kb(i)) then
         hsbar(i) = heso(i,k)
         if(hkbo(i).gt.hsbar(i)) then
           flg(i) = .false.
           kbcon(i) = k
         endif
       endif
     enddo
   enddo
   do i = its,ite
     if(kbcon(i).eq.kmax) cnvflg(i) = .false.
   enddo
!
   totflg = .true.
   do i = its,ite
     totflg = totflg .and. (.not. cnvflg(i))
   enddo
   if(totflg) return
!
   do i = its,ite
     if(cnvflg(i)) then
!
!  determine critical convective inhibition
!  as a function of vertical velocity at cloud base.
!
       pdot(i)  = 10.* dot(i,kbcon(i))
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
         tem = (pdot(i) - w4) / (w3 - w4)
       elseif(pdot(i).ge.-w4) then
         tem = - (pdot(i) + w4) / (w4 - w3)
       else
         tem = 0.
       endif
       tem = max(tem,-1.)
       tem = min(tem,1.)
       tem = 1. - tem
       tem1= .5*(cincrmax-cincrmin)
       cincr = cincrmax - tem * tem1
       pbcdif(i) = -p(i,kbcon(i)) + p(i,kb(i))
       if(pbcdif(i).gt.cincr) cnvflg(i) = .false.
     endif
   enddo
!
!
   totflg = .true.
   do i = its,ite
     totflg = totflg .and. (.not. cnvflg(i))
   enddo
   if(totflg) return
!
   do k = kts,kte1
     do i = its,ite
       xlamb(i,k) = clam / zi(i,k+1) 
     enddo
   enddo
!
!  assume that updraft entrainment rate above cloud base is
!  same as that at cloud base
!
   do k = kts1,kmax1
     do i = its,ite
       if(cnvflg(i).and.(k.gt.kbcon(i))) then
         xlamb(i,k) = xlamb(i,kbcon(i))
       endif
     enddo
   enddo
!
!   assume the detrainment rate for the updrafts to be same as
!   the entrainment rate at cloud base
!
   do i = its,ite
     if(cnvflg(i)) then
       xlamud(i) = xlamb(i,kbcon(i))
     endif
   enddo
!
!  functions rapidly decreasing with height, mimicking a cloud ensemble
!    (Bechtold et al., 2008)
!
   do k = kts1,kmax1
     do i = its,ite
       if(cnvflg(i).and.(k.gt.kbcon(i))) then
         tem = qeso(i,k)/qeso(i,kbcon(i))
         fent1(i,k) = tem**2
         fent2(i,k) = tem**3
       endif
     enddo
   enddo
!
!  final entrainment rate as the sum of turbulent part and organized entrainment
!    depending on the environmental relative humidity
!    (Bechtold et al., 2008)
!
   do k = kts1,kmax1
     do i = its,ite
       if(cnvflg(i).and.(k.ge.kbcon(i))) then
          tem = cxlamu * frh(i,k) * fent2(i,k)
          xlamb(i,k) = xlamb(i,k)*fent1(i,k) + tem
       endif
     enddo
   enddo
!
! determine updraft mass flux
!
   do k = kts,kte
     do i = its,ite
      if(cnvflg(i)) then
         eta(i,k) = 1.
       endif
     enddo
   enddo
!
   do k = kbmax,kts1,-1
     do i = its,ite
       if(cnvflg(i).and.k.lt.kbcon(i).and.k.ge.kb(i)) then
         dz = zi(i,k+2) - zi(i,k+1)
         ptem     = 0.5*(xlamb(i,k)+xlamb(i,k+1))-xlamud(i)
         eta(i,k) = eta(i,k+1) / (1. + ptem * dz)
       endif
     enddo
   enddo
  do k = kts1,kmax1
     do i = its,ite
       if(cnvflg(i).and.k.gt.kbcon(i)) then
         dz  = zi(i,k+1) - zi(i,k)
         ptem = 0.5*(xlamb(i,k)+xlamb(i,k-1))-xlamud(i)
         eta(i,k) = eta(i,k-1) * (1 + ptem * dz)
       endif
     enddo
   enddo
   do i = its,ite
     if(cnvflg(i)) then
       dz = zi(i,3) - zi(i,2)
       ptem     = 0.5*(xlamb(i,1)+xlamb(i,2))-xlamud(i)
       eta(i,1) = eta(i,2) / (1. + ptem * dz)
     endif
   enddo
!
! work up updraft cloud properties
!
   do i = its,ite
     if(cnvflg(i)) then
       indx = kb(i)
       hcko(i,indx) = hkbo(i)
       qcko(i,indx) = qkbo(i)
       ucko(i,indx) = uo(i,indx)
       vcko(i,indx) = vo(i,indx)
       pwavo(i) = 0.
     endif
   enddo
!
! cloud property below cloud base is modified by the entrainment proces
!
   do k = kts1,kmax1
     do i = its,ite
       if(cnvflg(i).and.k.gt.kb(i)) then
         dz   = zi(i,k+1) - zi(i,k)
         tem  = 0.5 * (xlamb(i,k)+xlamb(i,k-1)) * dz
         tem1 = 0.5 * xlamud(i) * dz
         factor = 1. + tem - tem1
         ptem = 0.5 * tem + pgcon
         ptem1= 0.5 * tem - pgcon
         hcko(i,k) = ((1.-tem1)*hcko(i,k-1)+tem*0.5*                          &
                     (heo(i,k)+heo(i,k-1)))/factor
         ucko(i,k) = ((1.-tem1)*ucko(i,k-1)+ptem*uo(i,k)                      &
                     +ptem1*uo(i,k-1))/factor
         vcko(i,k) = ((1.-tem1)*vcko(i,k-1)+ptem*vo(i,k)                      &
                     +ptem1*vo(i,k-1))/factor
         dbyo(i,k) = hcko(i,k) - heso(i,k)
       endif
     enddo
   enddo
!
!   taking account into convection inhibition due to existence of
!    dry layers below cloud base
!
   do i = its,ite
     flg(i) = cnvflg(i)
     kbcon1(i) = kmax
   enddo
!
   do k = kts1,kmax
     do i = its,ite
       if(flg(i).and.k.ge.kbcon(i).and.dbyo(i,k).gt.0.) then
         kbcon1(i) = k
         flg(i) = .false.
       endif
     enddo
   enddo
!
   do i = its,ite
     if(cnvflg(i)) then
       if(kbcon1(i).eq.kmax) cnvflg(i) = .false.
     endif
   enddo
!
   do i = its,ite
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
!
!  determine cloud top
!
!
   do i = its,ite
     flg(i) = cnvflg(i)
     ktcon(i) = 1
   enddo
!
!   check inversion
!
   do k = kts1,kmax1
     do i = its,ite
       if(dbyo(i,k).lt.0..and.flg(i).and.k.gt. kbcon1(i)) then
         ktcon(i) = k
         flg(i)   = .false.
       endif
     enddo
   enddo
!
!
! check cloud depth
!
   do i = its,ite
     if(cnvflg(i).and.(p(i,kbcon(i)) - p(i,ktcon(i))).lt.150.)                 &
            cnvflg(i) = .false.
   enddo
!
   totflg = .true.
   do i = its,ite
     totflg = totflg .and. (.not. cnvflg(i))
   enddo
   if(totflg) return
!
!
!  search for downdraft originating level above theta-e minimum
!
   do i = its,ite 
     if(cnvflg(i)) then
       hmin(i) = heo(i,kbcon1(i))
       lmin(i) = kbmax
       jmin(i) = kbmax
    endif
   enddo
!
   do k = kts1,kbmax 
     do i = its,ite 
       if(cnvflg(i).and.k.gt.kbcon1(i).and.heo(i,k).lt.hmin(i)) then
         lmin(i) = k + 1
         hmin(i) = heo(i,k)
       endif
     enddo
   enddo
!
! make sure that jmin is within the cloud
!
   do i = its,ite
     if(cnvflg(i)) then
       jmin(i) = min(lmin(i),ktcon(i)-1)
       jmin(i) = max(jmin(i),kbcon1(i)+1)
       if(jmin(i).ge.ktcon(i)) cnvflg(i) = .false.
     endif
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
!
! compute cloud moisture property and precipitation
!
   do i = its,ite
     aa1(i) = 0.
   enddo
!
   do k = kts1,kmax
     do i = its,ite
       if(cnvflg(i).and.k.gt.kb(i).and.k.lt.ktcon(i)) then
         dz = .5 * (zl(i,k+1) - zl(i,k-1))
         dz1 = (zi(i,k+1) - zi(i,k))
         gamma = el2orc * qeso(i,k) / (to(i,k)**2)
         qrch = qeso(i,k)                                                      &
              + gamma * dbyo(i,k) / (hvap_ * (1. + gamma))
         tem  = 0.5 * (xlamb(i,k)+xlamb(i,k-1)) * dz1
         tem1 = 0.5 * xlamud(i) * dz1
         factor = 1. + tem - tem1
         qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*                           & 
                    (qo(i,k)+qo(i,k-1)))/factor
         qcirs(i,k) = eta(i,k) * qcko(i,k) - eta(i,k) * qrch
!
! check if there is excess moisture to release latent heat
!
         if(qcirs(i,k).gt.0. .and. k.ge.kbcon(i)) then
           etah = .5 * (eta(i,k) + eta(i,k-1))
           if(ncloud.gt.0..and.k.gt.jmin(i)) then
             dp = 1000. * del(i,k)
             qlk = qcirs(i,k) / (eta(i,k) + etah * (c0 + c1) * dz1)
             dellal(i,k) = etah * c1 * dz1 * qlk * g_ / dp
           else
             qlk = qcirs(i,k) / (eta(i,k) + etah * c0 * dz1)
           endif
           aa1(i) = aa1(i) - dz1 * g_ * qlk
           qc = qlk + qrch
           pwo(i,k) = etah * c0 * dz1 * qlk
           qcko(i,k) = qc
           pwavo(i) = pwavo(i) + pwo(i,k)
         endif
       endif
     enddo
   enddo
!
! calculate cloud work function at t+dt
!
   do k = kts1,kmax 
     do i = its,ite 
       if(cnvflg(i).and.k.ge.kbcon(i).and.k.lt.ktcon(i)) then
         dz1 = zl(i,k+1) - zl(i,k)
         gamma = el2orc * qeso(i,k) / (to(i,k)**2)
         rfact =  1. + fv_ * cp_ * gamma* to(i,k) / hvap_
         aa1(i) = aa1(i) +dz1 * (g_ / (cp_ * to(i,k)))                         &
                  * dbyo(i,k) / (1. + gamma)* rfact
         aa1(i) = aa1(i)+dz1 * g_ * fv_ *                                      &
                  max(0.,(qeso(i,k) - qo(i,k)))
       endif
     enddo
   enddo
!
   do i = its,ite
     if(cnvflg(i).and.aa1(i).le.0.) cnvflg(i) = .false.
   enddo
!
   totflg = .true.
   do i = its,ite
     totflg = totflg .and. (.not. cnvflg(i))
   enddo
   if(totflg) return
!
!    estimate the convective overshooting as the level
!    where the [aafac * cloud work function] becomes zero,
!    which is the final cloud top
!
   do i = its,ite
     if (cnvflg(i)) then
       aa2(i) = aafac * aa1(i)
     endif
   enddo
!
   do i = its,ite
     flg(i) = cnvflg(i)
     ktcon1(i) = kmax1
   enddo
!
   do k = kts1,kmax
     do i = its, ite
       if (flg(i)) then
         if(k.ge.ktcon(i).and.k.lt.kmax) then
           dz1 = zl(i,k+1) - zl(i,k)
           gamma = el2orc * qeso(i,k) / (to(i,k)**2)
           rfact =  1. + fv_ * cp_ * gamma* to(i,k) / hvap_
           aa2(i) = aa2(i) +dz1 * (g_ / (cp_ * to(i,k)))                    &
                       * dbyo(i,k) / (1. + gamma)* rfact
           if(aa2(i).lt.0.) then
             ktcon1(i) = k
             flg(i) = .false.
           endif
         endif
       endif
     enddo
   enddo
!
!  compute cloud moisture property, detraining cloud water
!  and precipitation in overshooting layers
!
   do k = kts1,kmax
     do i = its,ite
       if (cnvflg(i)) then
         if(k.ge.ktcon(i).and.k.lt.ktcon1(i)) then
           dz = (zi(i,k+1) - zi(i,k))
           gamma = el2orc * qeso(i,k) / (to(i,k)**2)
           qrch = qeso(i,k)+ gamma * dbyo(i,k) / (hvap_ * (1. + gamma))
           tem  = 0.5 * (xlamb(i,k)+xlamb(i,k-1)) * dz
           tem1 = 0.5 * xlamud(i) * dz
           factor = 1. + tem - tem1
           qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*                         & 
                      (qo(i,k)+qo(i,k-1)))/factor
           qcirs(i,k) = eta(i,k) * qcko(i,k) - eta(i,k) * qrch
!
!  check if there is excess moisture to release latent heat
!
           if(qcirs(i,k).gt.0.) then
             etah = .5 * (eta(i,k) + eta(i,k-1))
             if(ncloud.gt.0.) then
               dp = 1000. * del(i,k)
               qlk = qcirs(i,k) / (eta(i,k) + etah * (c0 + c1) * dz)
               dellal(i,k) = etah * c1 * dz * qlk * g_ / dp
             else
               qlk = qcirs(i,k) / (eta(i,k) + etah * c0 * dz)
             endif
             qc = qlk + qrch
             pwo(i,k) = etah * c0 * dz * qlk
             qcko(i,k) = qc
             pwavo(i) = pwavo(i) + pwo(i,k)
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
! this section is ready for cloud water
!
   if (ncloud.gt.0) then
!
!  compute liquid and vapor separation at cloud top
! 
     do i = its,ite
       if(cnvflg(i)) then
         k = ktcon(i)-1
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
   endif
!
! ..... downdraft calculations .....
!
! determine downdraft strength in terms of wind shear
!
   do i = its,ite
     if(cnvflg(i)) then
       vshear(i) = 0.
     endif
   enddo
!
   do k = kts1,kmax
     do i = its,ite
       if(k.gt.kb(i).and.k.le.ktcon(i).and.cnvflg(i)) then
         shear= sqrt((uo(i,k)-uo(i,k-1)) ** 2                                  & 
                       + (vo(i,k)-vo(i,k-1)) ** 2)
         vshear(i) = vshear(i) + shear
       endif
     enddo
   enddo
!
   do i = its,ite
     if(cnvflg(i)) then
       vshear(i) = 1.e3 * vshear(i) / (zi(i,ktcon(i)+1)-zi(i,kb(i)+1))
       e1 = 1.591-.639*vshear(i)                                               &
           +.0953*(vshear(i)**2)-.00496*(vshear(i)**3)
       edt(i)  = 1.-e1
!
       edt(i)  = min(edt(i),.9)
       edt(i)  = max(edt(i),.0)
       edto(i) = edt(i)
       edtx(i) = edt(i)
     endif
   enddo
!
! determine detrainment rate between 1 and kbdtr
!
   do i = its,ite
     if(cnvflg(i)) then
       sumx(i) = 0.
     endif
   enddo
!
   do k = kts,kmax1
     do i = its,ite
       if(cnvflg(i).and.k.ge.1.and.k.lt.kbcon(i)) then
         dz = zi(i,k+2) - zi(i,k+1)
         sumx(i) = sumx(i) + dz
       endif
     enddo
   enddo
!
   do i = its,ite
     kbdtr(i) = kbcon(i)
     beta = betas
     if(slimsk(i).eq.1.) beta = betal
     if(cnvflg(i)) then
       kbdtr(i) = kbcon(i)
       kbdtr(i) = max(kbdtr(i),1)
       dz =(sumx(i)+zi(i,2))/float(kbcon(i))
       tem = 1./float(kbcon(i))
       xlamd(i) = (1.-beta**tem)/dz
     endif
   enddo
!
! determine downdraft mass flux
!
   do k = kts,kmax
     do i = its,ite
       if(cnvflg(i)) then
         etad(i,k) = 1.
       endif
       qrcdo(i,k) = 0.
       qrcd(i,k) = 0.
     enddo
   enddo
!
   do k = kmax1,kts,-1
     do i = its,ite
       if(cnvflg(i)) then
         if(k.lt.jmin(i).and.k.ge.kbcon(i))then
           dz = (zi(i,k+2) - zi(i,k+1))
           ptem = xlamdd-xlamde
           etad(i,k) = etad(i,k+1) * (1.-ptem * dz)
         elseif(k.lt.kbcon(i))then
           dz = (zi(i,k+2) - zi(i,k+1))
           ptem = xlamd(i)+xlamdd-xlamde
           etad(i,k) = etad(i,k+1) * (1.-ptem * dz)
         endif
       endif
     enddo
   enddo
!
!
! downdraft moisture properties
!
   do i = its,ite
     if(cnvflg(i)) then
      pwevo(i) = 0.
     endif
   enddo
!
   do i = its,ite
     if(cnvflg(i))  then 
       jmn = jmin(i)
       hcdo(i,jmn) = heo(i,jmn)
       qcdo(i,jmn) = qo(i,jmn)
       qrcdo(i,jmn) = qeso(i,jmn)
       ucdo(i,jmn) = uo(i,jmn)
       vcdo(i,jmn) = vo(i,jmn)
     endif
   enddo
!
   do k = kmax1,kts,-1 
     do i = its,ite 
       if (cnvflg(i) .and. k.lt.jmin(i)) then
         dz = zi(i,k+2) - zi(i,k+1)
         if(k.ge.kbcon(i)) then
           tem  = xlamde * dz
           tem1 = 0.5 * xlamdd * dz
         else
           tem  = xlamde * dz
           tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
          endif
          factor = 1. + tem - tem1
          ptem = 0.5 * tem - pgcon
          ptem1= 0.5 * tem + pgcon
          hcdo(i,k) = ((1.-tem1)*hcdo(i,k+1)+tem*0.5*                     & 
                      (heo(i,k)+heo(i,k+1)))/factor
          ucdo(i,k) = ((1.-tem1)*ucdo(i,k+1)+ptem*uo(i,k+1)               & 
                     +ptem1*uo(i,k))/factor
          vcdo(i,k) = ((1.-tem1)*vcdo(i,k+1)+ptem*vo(i,k+1)               & 
                     +ptem1*vo(i,k))/factor
          dbyo(i,k) = hcdo(i,k) - heso(i,k)
       endif
     enddo
   enddo
!
   do k = kmax1,kts,-1
     do i = its,ite
       if(cnvflg(i).and.k.lt.jmin(i)) then
         dq = qeso(i,k)
         dt = to(i,k)
         gamma = el2orc * dq / dt**2
         qrcdo(i,k)=dq+(1./hvap_)*(gamma/(1.+gamma))*dbyo(i,k)
         detad = etad(i,k+1) - etad(i,k)
         dz = zi(i,k+2) - zi(i,k+1)
         if(k.ge.kbcon(i)) then
            tem  = xlamde * dz
            tem1 = 0.5 * xlamdd * dz
         else
            tem  = xlamde * dz
            tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
         endif
         factor = 1. + tem - tem1
         qcdo(i,k) = ((1.-tem1)*qcdo(i,k+1)+tem*0.5*                      & 
                     (qo(i,k)+qo(i,k+1)))/factor
         pwdo(i,k) = etad(i,k+1) * qcdo(i,k) -etad(i,k+1) * qrcdo(i,k)
         qcdo(i,k) = qrcdo(i,k)
         pwevo(i) = pwevo(i) + pwdo(i,k)
       endif
     enddo
   enddo
!
! final downdraft strength dependent on precip
! efficiency (edt), normalized condensate (pwav), and
! evaporate (pwev)
!
   do i = its,ite
     edtmax = edtmaxl
     if(slimsk(i).eq.2.) edtmax = edtmaxs
     if(cnvflg(i)) then
       if(pwevo(i).lt.0.) then
         edto(i) = -edto(i) * pwavo(i) / pwevo(i)
         edto(i) = min(edto(i),edtmax)
       else
         edto(i) = 0.
       endif
     endif
   enddo
!
! downdraft cloudwork functions
!
   do k = kmax1,kts,-1
     do i = its,ite
       if(cnvflg(i).and.k.lt.jmin(i)) then
         gamma = el2orc * qeso(i,k) / to(i,k)**2
         dhh=hcdo(i,k)
         dt=to(i,k)
         dg=gamma
         dh=heso(i,k)
         dz=-1.*(zl(i,k+1)-zl(i,k))
         aa1(i)=aa1(i)+edto(i)*dz*(g_/(cp_*dt))*((dhh-dh)/(1.+dg))             &
                *(1.+fv_*cp_*dg*dt/hvap_)
         aa1(i)=aa1(i)+edto(i)*dz*g_*fv_*max(0.,(qeso(i,k)-qo(i,k)))
       endif
     enddo
   enddo
!
   do i = its,ite
     if(cnvflg(i).and.aa1(i).le.0.) cnvflg(i) = .false.
   enddo
!
   totflg = .true.
   do i = its,ite
     totflg = totflg .and. (.not. cnvflg(i))
   enddo
   if(totflg) return
!
! what would the change be, that a cloud with unit mass
! will do to the environment?
!
   do k = kts,kmax
     do i = its,ite
       if(cnvflg(i)) then
         dellah(i,k) = 0.
         dellaq(i,k) = 0.
         dellau(i,k) = 0.
         dellav(i,k) = 0.
       endif
     enddo
   enddo
!
   do i = its,ite
     if(cnvflg(i)) then
       dp = 1000. * del(i,1)
       dellah(i,1) = edto(i) * etad(i,1) * (hcdo(i,1)                          &
                   - heo(i,1)) * g_ / dp
       dellaq(i,1) = edto(i) * etad(i,1) * (qcdo(i,1)                          &
                   - qo(i,1)) * g_ / dp
       dellau(i,1) = edto(i) * etad(i,1) * (ucdo(i,1)                          &
                   - uo(i,1)) * g_ / dp
       dellav(i,1) = edto(i) * etad(i,1) * (vcdo(i,1)                          &
                   - vo(i,1)) * g_ / dp
     endif
   enddo
!
! changed due to subsidence and entrainment
!
   do k = kts1,kmax1
     do i = its,ite
       if(cnvflg(i).and.k.lt.ktcon(i)) then
         aup = 1.
         if(k.le.kb(i)) aup = 0.
         adw = 1.
         if(k.gt.jmin(i)) adw = 0.
         dv1= heo(i,k)
         dv2 = .5 * (heo(i,k) + heo(i,k-1))
         dv3= heo(i,k-1)
         dv1q= qo(i,k)
         dv2q = .5 * (qo(i,k) + qo(i,k-1))
         dv3q= qo(i,k-1)
         dv1u = uo(i,k)
         dv2u = .5 * (uo(i,k) + uo(i,k-1))
         dv3u = uo(i,k-1)
         dv1v = vo(i,k)
         dv2v = .5 * (vo(i,k) + vo(i,k-1))
         dv3v = vo(i,k-1)
         dp = 1000. * del(i,k)
         dz = zi(i,k+1) - zi(i,k)
         tem  = 0.5 * (xlamb(i,k)+xlamb(i,k-1))
         tem1 = xlamud(i)
         if(k.le.kbcon(i)) then
           ptem  = xlamde
           ptem1 = xlamd(i)+xlamdd
         else
           ptem  = xlamde
           ptem1 = xlamdd
         endif
         deta = eta(i,k) - eta(i,k-1)
         detad = etad(i,k) - etad(i,k-1)
         dellah(i,k) = dellah(i,k) +                                           &
             ((aup * eta(i,k) - adw * edto(i) * etad(i,k)) * dv1               &
         - (aup * eta(i,k-1) - adw * edto(i) * etad(i,k-1))* dv3               &
         - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2*dz              & 
         +  aup*tem1*eta(i,k-1)*.5*(hcko(i,k)+hcko(i,k-1))*dz                  & 
         +  adw*edto(i)*ptem1*etad(i,k)*.5*(hcdo(i,k)+hcdo(i,k-1))*dz) *g_/dp
         dellaq(i,k) = dellaq(i,k) +                                           &
             ((aup * eta(i,k) - adw * edto(i) * etad(i,k)) * dv1q              &
         - (aup * eta(i,k-1) - adw * edto(i) * etad(i,k-1))* dv3q              &
         - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2q*dz             & 
         +  aup*tem1*eta(i,k-1)*.5*(qcko(i,k)+qcko(i,k-1))*dz                  & 
         +  adw*edto(i)*ptem1*etad(i,k)*.5*(qrcdo(i,k)+qrcdo(i,k-1))*dz) *g_/dp
         dellau(i,k) = dellau(i,k) +                                           &
             ((aup * eta(i,k) - adw * edto(i) * etad(i,k)) * dv1u              &
         - (aup * eta(i,k-1) - adw * edto(i) * etad(i,k-1))* dv3u              &
         - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2u*dz             & 
         +  aup*tem1*eta(i,k-1)*.5*(ucko(i,k)+ucko(i,k-1))*dz                  & 
         +  adw*edto(i)*ptem1*etad(i,k)*.5*(ucdo(i,k)+ucdo(i,k-1))*dz          & 
         -  pgcon*(aup*eta(i,k-1)-adw*edto(i)*etad(i,k))*(dv1u-dv3u))*g_/dp
!
         dellav(i,k) = dellav(i,k) +                                           &
             ((aup * eta(i,k) - adw * edto(i) * etad(i,k)) * dv1v              &
         - (aup * eta(i,k-1) - adw * edto(i) * etad(i,k-1))* dv3v              &
         - (aup*tem*eta(i,k-1)+adw*edto(i)*ptem*etad(i,k))*dv2v*dz             & 
         +  aup*tem1*eta(i,k-1)*.5*(vcko(i,k)+vcko(i,k-1))*dz                  & 
         +  adw*edto(i)*ptem1*etad(i,k)*.5*(vcdo(i,k)+vcdo(i,k-1))*dz          & 
         -  pgcon*(aup*eta(i,k-1)-adw*edto(i)*etad(i,k))*(dv1v-dv3v))*g_/dp
       endif
     enddo
   enddo
!
! cloud top
!
   do i = its,ite
     if(cnvflg(i)) then
       indx = ktcon(i)
       dp = 1000. * del(i,indx)
       dv1 = heo(i,indx-1)
       dellah(i,indx) = eta(i,indx-1) *                                        &
                        (hcko(i,indx-1) - dv1) * g_ / dp
       dvq1 = qo(i,indx-1)
       dellaq(i,indx) = eta(i,indx-1) *                                        &
                        (qcko(i,indx-1) - dvq1) * g_ / dp
       dv1u = uo(i,indx-1)
       dellau(i,indx) = eta(i,indx-1) *                                        &
                        (ucko(i,indx-1) - dv1u) * g_ / dp
       dv1v = vo(i,indx-1)
       dellav(i,indx) = eta(i,indx-1) *                                        &
                        (vcko(i,indx-1) - dv1v) * g_ / dp
!
!  cloud water
!
       dellal(i,indx) = eta(i,indx-1) * qlko_ktcon(i) * g_ / dp
     endif
   enddo
!
! final changed variable per unit mass flux
!
   do k = kts,kmax
     do i = its,ite
       if(cnvflg(i).and.k.gt.ktcon(i)) then
         qo(i,k) = q1(i,k)
         to(i,k) = t1(i,k)
       endif
       if(cnvflg(i).and.k.le.ktcon(i)) then
         qo(i,k) = dellaq(i,k) * mbdt + q1(i,k)
         dellat  = (dellah(i,k) - hvap_ * dellaq(i,k)) / cp_
         to(i,k) = dellat * mbdt + t1(i,k)
         qo(i,k) = max(qo(i,k),1.0e-10)
       endif
     enddo
   enddo
!
!------------------------------------------------------------------------------
!
! the above changed environment is now used to calulate the
! effect the arbitrary cloud (with unit mass flux)
! which then is used to calculate the real mass flux,
! necessary to keep this change in balance with the large-scale
! destabilization.
!
! environmental conditions again
!
!------------------------------------------------------------------------------
!
   do k = kts,kmax
     do i = its,ite
       if(cnvflg(i)) then
         qeso(i,k)=0.01* fpvs(to(i,k),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
         qeso(i,k) = eps * qeso(i,k) / (p(i,k) + (eps-1.) * qeso(i,k))
         qeso(i,k) = max(qeso(i,k),qmin_)
         tvo(i,k)  = to(i,k) + fv_ * to(i,k) * max(qo(i,k),qmin_)
       endif
     enddo
   enddo
!
   do i = its,ite
     if(cnvflg(i)) then
       xaa0(i) = 0.
       xpwav(i) = 0.
     endif
   enddo
!
! moist static energy
!
   do k = kts,kmax1
     do i = its,ite
       if(cnvflg(i)) then
         dz = .5 * (zl(i,k+1) - zl(i,k))
         dp = .5 * (p(i,k+1) - p(i,k))
         es =0.01*fpvs(to(i,k+1),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
         pprime = p(i,k+1) + (eps-1.) * es
         qs = eps * es / pprime
         dqsdp = - qs / pprime
         desdt = es * (fact1 / to(i,k+1) + fact2 / (to(i,k+1)**2))
         dqsdt = qs * p(i,k+1) * desdt / (es * pprime)
         gamma = el2orc * qeso(i,k+1) / (to(i,k+1)**2)
         dt = (g_ * dz + hvap_ * dqsdp * dp) / (cp_ * (1. + gamma))
         dq = dqsdt * dt + dqsdp * dp
         to(i,k) = to(i,k+1) + dt
         qo(i,k) = qo(i,k+1) + dq
         po = .5 * (p(i,k) + p(i,k+1))
         qeso(i,k) =0.01* fpvs(to(i,k),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
         qeso(i,k) = eps * qeso(i,k) / (po + (eps-1.) * qeso(i,k))
         qeso(i,k) = max(qeso(i,k),qmin_)
         qo(i,k)   = max(qo(i,k), 1.0e-10)
         heo(i,k) = .5 * g_ * (zl(i,k) + zl(i,k+1)) +                          &
                     cp_ * to(i,k) + hvap_ * qo(i,k)
         heso(i,k) = .5 * g_ * (zl(i,k) + zl(i,k+1)) +                         &
                     cp_ * to(i,k) + hvap_ * qeso(i,k)
       endif
     enddo
   enddo
!
   k = kmax
   do i = its,ite
     if(cnvflg(i)) then
       heo(i,k)  = g_ * zl(i,k) + cp_ * to(i,k) + hvap_ * qo(i,k)
       heso(i,k) = g_ * zl(i,k) + cp_ * to(i,k) + hvap_ * qeso(i,k)
     endif
   enddo
!
   do i = its,ite
     if(cnvflg(i)) then
       xaa0(i) = 0.
       xpwav(i) = 0.
       indx = kb(i)
       xhkb(i) = heo(i,indx)
       xqkb(i) = qo(i,indx)
       hcko(i,indx) = xhkb(i)
       qcko(i,indx) = xqkb(i)
     endif
   enddo
!
! ..... static control .....
!
! moisture and cloud work functions
!
   do k = kts1,kmax1
     do i = its,ite
       if(cnvflg(i).and.k.gt.kb(i).and.k.le.ktcon(i)) then
         dz = zi(i,k+1) - zi(i,k)
         tem  = 0.5 * (xlamb(i,k)+xlamb(i,k-1)) * dz
         tem1 = 0.5 * xlamud(i) * dz
         factor = 1. + tem - tem1
         hcko(i,k) = ((1.-tem1)*hcko(i,k-1)+tem*0.5*                         & 
                    (heo(i,k)+heo(i,k-1)))/factor
       endif
     enddo
   enddo
!
   do k = kts1,kmax1
     do i = its,ite
       if(cnvflg(i).and.k.gt.kb(i).and.k.lt.ktcon(i)) then
         dz = zi(i,k+1) - zi(i,k)
         gamma = el2orc * qeso(i,k) / (to(i,k)**2)
         xdby = hcko(i,k) - heso(i,k)
         xqrch = qeso(i,k)                                                     &
              + gamma * xdby / (hvap_ * (1. + gamma))
         tem  = 0.5 * (xlamb(i,k)+xlamb(i,k-1)) * dz
         tem1 = 0.5 * xlamud(i) * dz
         factor = 1. + tem - tem1
         qcko(i,k) = ((1.-tem1)*qcko(i,k-1)+tem*0.5*(qo(i,k)+qo(i,k-1)))/factor
         dq = eta(i,k) * qcko(i,k) - eta(i,k) * xqrch
         if(k.ge.kbcon(i).and.dq.gt.0.) then
           etah = .5 * (eta(i,k) + eta(i,k-1))
           if(ncloud.gt.0..and.k.gt.jmin(i)) then
             qlk = dq / (eta(i,k) + etah * (c0 + c1) * dz)
           else
             qlk = dq / (eta(i,k) + etah * c0 * dz)
           endif
           if(k.lt.ktcon1(i)) then
             xaa0(i) = xaa0(i) - dz * g_ * qlk
           endif
           qcko(i,k) = qlk + xqrch
           xpw = etah * c0 * dz * qlk
           xpwav(i) = xpwav(i) + xpw
         endif
       endif
       if(cnvflg(i).and.k.ge.kbcon(i).and.k.lt.ktcon1(i)) then
         dz1 = zl(i,k+1) - zl(i,k)
         gamma = el2orc * qeso(i,k) / (to(i,k)**2)
         rfact =  1. + fv_ * cp_ * gamma                                       &
                  * to(i,k) / hvap_
         xdby = hcko(i,k) - heso(i,k)
         xaa0(i) = xaa0(i)                                                     &
                 + dz1 * (g_ / (cp_ * to(i,k)))                                &
                 * xdby / (1. + gamma)                                         &
                 * rfact
         xaa0(i)=xaa0(i)+                                                      &
                  dz1 * g_ * fv_ *                                             &
                  max(0.,(qeso(i,k) - qo(i,k)))
       endif
     enddo
   enddo
!
! ..... downdraft calculations .....
!
!
! downdraft moisture properties
!
   do i = its,ite
     xpwev(i) = 0.
   enddo
!
   do i = its,ite
     if(cnvflg(i)) then
       jmn = jmin(i)
       xhcd(i,jmn) = heo(i,jmn)
       xqcd(i,jmn) = qo(i,jmn)
       qrcd(i,jmn) = qeso(i,jmn)
     endif
   enddo
!
   do k = kmax1,kts,-1
     do i = its,ite
       if(cnvflg(i).and.k.lt.jmin(i)) then
         dz = zi(i,k+2) - zi(i,k+1)
         if(k.ge.kbcon(i)) then
            tem  = xlamde * dz
            tem1 = 0.5 * xlamdd * dz
         else
            tem  = xlamde * dz
            tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
         endif
         factor = 1. + tem - tem1
         xhcd(i,k) = ((1.-tem1)*xhcd(i,k+1)+tem*0.5*                        & 
                    (heo(i,k)+heo(i,k+1)))/factor
       endif
     enddo
   enddo
!
   do k = kmax1,kts,-1
     do i = its,ite
       if(cnvflg(i).and.k.lt.jmin(i)) then
         dq = qeso(i,k)
         dt = to(i,k)
         gamma = el2orc * dq / dt**2
         dh = xhcd(i,k) - heso(i,k)
         qrcd(i,k)=dq+(1./hvap_)*(gamma/(1.+gamma))*dh
         dz = zi(i,k+2) - zi(i,k+1)
         if(k.ge.kbcon(i)) then
           tem  = xlamde * dz
           tem1 = 0.5 * xlamdd * dz
         else
           tem  = xlamde * dz
           tem1 = 0.5 * (xlamd(i)+xlamdd) * dz
         endif
         factor = 1. + tem - tem1
         xqcd(i,k) = ((1.-tem1)*xqcd(i,k+1)+tem*0.5*                           & 
                   (qo(i,k)+qo(i,k+1)))/factor
         xpwd     = etad(i,k+1) * (xqcd(i,k) - qrcd(i,k))
         xqcd(i,k)= qrcd(i,k)
         xpwev(i) = xpwev(i) + xpwd
       endif
     enddo
   enddo
!
   do i = its,ite
     edtmax = edtmaxl
     if(slimsk(i).eq.2.) edtmax = edtmaxs
     if(cnvflg(i)) then
       if(xpwev(i).ge.0.) then
         edtx(i) = 0.
       else
         edtx(i) = -edtx(i) * xpwav(i) / xpwev(i)
         edtx(i) = min(edtx(i),edtmax)
       endif
     endif
   enddo
!
! downdraft cloudwork functions
!
   do k = kmax1,kts,-1
     do i = its,ite
       if(cnvflg(i).and.k.lt.jmin(i)) then
         gamma = el2orc * qeso(i,k) / to(i,k)**2
         dhh=xhcd(i,k)
         dt= to(i,k)
         dg= gamma
         dh= heso(i,k)
         dz=-1.*(zl(i,k+1)-zl(i,k))
         xaa0(i)=xaa0(i)+edtx(i)*dz*(g_/(cp_*dt))*((dhh-dh)/(1.+dg))           &
                 *(1.+fv_*cp_*dg*dt/hvap_)
         xaa0(i)=xaa0(i)+edtx(i)*                                              &
                  dz*g_*fv_*max(0.,(qeso(i,k)-qo(i,k)))
       endif
     enddo
   enddo
!
! calculate critical cloud work function
!
   do i = its,ite
     acrt(i) = 0.
     if(cnvflg(i)) then
       if(p(i,ktcon(i)).lt.pcrit(15))then
         acrt(i)=acrit(15)*(975.-p(i,ktcon(i)))/(975.-pcrit(15))
       else if(p(i,ktcon(i)).gt.pcrit(1))then
         acrt(i)=acrit(1)
       else
         k = int((850. - p(i,ktcon(i)))/50.) + 2
         k = min(k,15)
         k = max(k,2)
         acrt(i)=acrit(k)+(acrit(k-1)-acrit(k))*                               &
              (p(i,ktcon(i))-pcrit(k))/(pcrit(k-1)-pcrit(k))
        endif
      endif
    enddo
!
   do i = its,ite
     acrtfct(i) = 1.
     w1 = w1s
     w2 = w2s
     w3 = w3s
     w4 = w4s
     if(slimsk(i).eq.1.) then
       w1 = w1l
       w2 = w2l
       w3 = w3l
       w4 = w4l
     endif
     if(cnvflg(i)) then
       if(pdot(i).le.w4) then
         acrtfct(i) = (pdot(i) - w4) / (w3 - w4)
       elseif(pdot(i).ge.-w4) then
       acrtfct(i) = - (pdot(i) + w4) / (w4 - w3)
       else
         acrtfct(i) = 0.
       endif
       acrtfct(i) = max(acrtfct(i),-1.)
       acrtfct(i) = min(acrtfct(i),1.)
       acrtfct(i) = 1. - acrtfct(i)
       dtconv(i) = dt2 + max((1800. - dt2),0.) * (pdot(i) - w2) / (w1 - w2)   
       dtconv(i) = max(dtconv(i),dtmin)
       dtconv(i) = min(dtconv(i),dtmax)
!
     endif
   enddo
!
! large scale forcing
!
   do i = its,ite
     if(cnvflg(i)) then
       f(i) = (aa1(i) - acrt(i) * acrtfct(i)) / dtconv(i)
       if(f(i).le.0.) cnvflg(i) = .false.
     endif
     if(cnvflg(i)) then
       xk(i) = (xaa0(i) - aa1(i)) / mbdt
       if(xk(i).ge.0.) cnvflg(i) = .false.
     endif
!
! kernel, cloud base mass flux
!
     if(cnvflg(i)) then
       xmb(i) = -f(i) / xk(i)
       xmb(i) = min(xmb(i),xmbmax(i))
     endif
!
     if(cnvflg(i)) then
     endif
!
   enddo
   totflg = .true.
   do i = its,ite
     totflg = totflg .and. (.not. cnvflg(i))
   enddo
   if(totflg) return
!
! restore t0 and qo to t1 and q1 in case convection stops
!
   do k = kts,kmax
     do i = its,ite
       if (cnvflg(i)) then
       to(i,k) = t1(i,k)
       qo(i,k) = q1(i,k)
       uo(i,k) = u1(i,k)
       vo(i,k) = v1(i,k)
       qeso(i,k) = 0.01*fpvs(t1(i,k),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
       qeso(i,k) = eps * qeso(i,k) / (p(i,k) + (eps-1.) * qeso(i,k))
       qeso(i,k) = max(qeso(i,k),qmin_)
       endif
     enddo
   enddo
!
! feedback: simply the changes from the cloud with unit mass flux
!           multiplied by  the mass flux necessary to keep the
!           equilibrium with the larger-scale.
!
   do i = its,ite
     delhbar(i) = 0.
     delqbar(i) = 0.
     deltbar(i) = 0.
     qcond(i) = 0.
     qrski(i) = 0.
     delubar(i) = 0.
     delvbar(i) = 0.
   enddo
!
   do k = kts,kmax
     do i = its,ite
       if(cnvflg(i).and.k.le.ktcon(i)) then
         aup = 1.
         if(k.le.kb(i)) aup = 0.
         adw = 1.
         if(k.gt.jmin(i)) adw = 0.
         dellat = (dellah(i,k) - hvap_ * dellaq(i,k)) / cp_
         t1(i,k) = t1(i,k) + dellat * xmb(i) * dt2
         q1(i,k) = q1(i,k) + dellaq(i,k) * xmb(i) * dt2
         tem=1./rcs
         u1(i,k) = u1(i,k) + dellau(i,k) * xmb(i) * dt2 * tem
         v1(i,k) = v1(i,k) + dellav(i,k) * xmb(i) * dt2 * tem 
         dp = 1000. * del(i,k)
         delhbar(i) = delhbar(i) + dellah(i,k)*xmb(i)*dp/g_
         delqbar(i) = delqbar(i) + dellaq(i,k)*xmb(i)*dp/g_
         deltbar(i) = deltbar(i) + dellat*xmb(i)*dp/g_
         delubar(i) = delubar(i) + dellau(i,k)*xmb(i)*dp/g_
         delvbar(i) = delvbar(i) + dellav(i,k)*xmb(i)*dp/g_
       endif
     enddo
   enddo
!
   do i = its,ite
     if(cnvflg(i)) then
     endif
   enddo
!
   do k = kts,kmax 
     do i = its,ite 
       if (cnvflg(i) .and. k.le.ktcon(i)) then
         qeso(i,k)=0.01* fpvs(t1(i,k),1,rd_,rv_,cvap_,cliq_,cice,xlv0,xls,psat,t0c_)
         qeso(i,k) = eps * qeso(i,k)/(p(i,k) + (eps-1.)*qeso(i,k))
         qeso(i,k) = max(qeso(i,k), qmin_ )
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
!  comptute rainfall  
!
   do k = kmax,kts,-1
     do i = its,ite
       if(cnvflg(i).and.k.lt.ktcon(i)) then
         aup = 1.
         if(k.le.kb(i)) aup = 0.
         adw = 1.
         if(k.ge.jmin(i)) adw = 0.
         rntot(i) = rntot(i)                                                   &
               + (aup * pwo(i,k) + adw * edto(i) * pwdo(i,k))                  &
               * xmb(i) * .001 * dt2
       endif
     enddo
   enddo
!
!  conversion rainfall (m) and compute the evaporation of falling raindrops 
!
   do k = kmax,kts,-1
     do i = its,ite
       delq(i) = 0.0
       deltv(i) = 0.0
       qevap(i) = 0.0
       if(cnvflg(i).and.k.lt.ktcon(i)) then
         aup = 1.
         if(k.le.kb(i)) aup = 0.
         adw = 1.
         if(k.ge.jmin(i)) adw = 0.
         rain(i) = rain(i)                                                     &
               + (aup * pwo(i,k) + adw * edto(i) * pwdo(i,k))                  &
               * xmb(i) * .001 * dt2
       endif
       if(cnvflg(i).and.flg(i).and.k.lt.ktcon(i)) then
!
         evef = edt(i) * evfacts
         if(slimsk(i).eq.1.) evef = edt(i) * evfactl
         qcond(i) = evef * (q1(i,k) - qeso(i,k)) / (1. + el2orc *              &
                  qeso(i,k) / t1(i,k)**2)
         dp = 1000. * del(i,k)
         if(rain(i).gt.0..and.qcond(i).lt.0.) then
           qevap(i) = -qcond(i) * (1. - exp(-.32 * sqrt(dt2 * rain(i))))
           qevap(i) = min(qevap(i), rain(i)*1000.*g_/dp)
           delq2(i) = delqev(i) + .001 * qevap(i) * dp / g_
           if (delq2(i).gt.rntot(i)) then
             qevap(i) = 1000.* g_ * (rntot(i) - delqev(i)) / dp
             flg(i) = .false.
           endif 
         endif
         if(rain(i).gt.0..and.qevap(i).gt.0.) then
           q1(i,k) = q1(i,k) + qevap(i)
           t1(i,k) = t1(i,k) - (hvap_/cp_) * qevap(i)
           rain(i) = rain(i) - .001 * qevap(i) * dp / g_
           delqev(i) = delqev(i) + .001*dp*qevap(i)/g_
           deltv(i) =  - (hvap_/cp_)*qevap(i)/dt2
           delq(i) =  + qevap(i)/dt2
         endif
         dellaq(i,k) = dellaq(i,k) + delq(i)/xmb(i)
         delqbar(i)  = delqbar(i) + delq(i)*dp/g_
         deltbar(i)  = deltbar(i) + deltv(i)*dp/g_
       endif
     enddo
   enddo
!
!
! consider the negative rain in the event of rain evaporation and downdrafts
!
   do i = its,ite
     if(cnvflg(i)) then
       if(rain(i).lt.0..and..not.flg(i)) rain(i) = 0.
       if(rain(i).le.0.) then
         rain(i) = 0.
       else
         ktop(i) = ktcon(i)
         kbot(i) = kbcon(i)
         icps(i) = 1
       endif
     endif
   enddo
!
   do k = kts,kmax
     do i = its,ite
       if(cnvflg(i).and.rain(i).le.0.) then
          t1(i,k) = to(i,k)
          q1(i,k) = qo(i,k)
          u1(i,k) = uo(i,k)
          v1(i,k) = vo(i,k)
       endif
     enddo
   enddo
!
!  detrainment of cloud water and ice
!
   if (ncloud.gt.0) then
     do k = kts,kmax 
       do i = its,ite 
         if (cnvflg(i) .and. rain(i).gt.0.) then
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
   endif
!
   end subroutine nsas2d
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
   subroutine nsasinit(rthcuten,rqvcuten,rqccuten,rqicuten,                    &
                      rucuten,rvcuten,                                         &  
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
                                                              rthcuten,        &
                                                              rqvcuten,        &
                                                               rucuten,        &
                                                               rvcuten,        &
                                                              rqccuten,        &
                                                              rqicuten
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
           rthcuten(i,k,j)=0.
           rqvcuten(i,k,j)=0.
           rucuten(i,k,j)=0.   
           rvcuten(i,k,j)=0.   
         enddo
       enddo
     enddo
!
     if (p_qc .ge. p_first_scalar) then
       do j = jts,jtf
         do k = kts,ktf
           do i = its,itf
             rqccuten(i,k,j)=0.
           enddo
         enddo
       enddo
     endif
!
     if (p_qi .ge. p_first_scalar) then
       do j = jts,jtf
         do k = kts,ktf
           do i = its,itf
             rqicuten(i,k,j)=0.
           enddo
         enddo
       enddo
     endif
   endif
!
   end subroutine nsasinit
!
!-------------------------------------------------------------------------------
! NCEP SCV (Shallow Convection Scheme)
!-------------------------------------------------------------------------------
   subroutine nscv2d(delt,del,prsl,prsi,prslk,zl,zi,                           &
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
! subprogram:    nscv2d           computes shallow-convective heating and moisng
!
! abstract: computes non-precipitating convective heating and moistening 
!   using a one cloud type arakawa-schubert convection scheme as in the ncep
!   sas scheme. the scheme has been operational at ncep gfs model since july 2010
!   the scheme includes updraft and downdraft effects, but the cloud depth is 
!   limited less than 150 hpa. 
!
! developed by jong-il han and hua-lu pan 
!   implemented into wrf by jiheon jang and songyou hong
!   module with cpp-based options is available in grims
!
! program history log:
!   10-07-01 jong-il han  initial operational implementation at ncep gfs
!   10-12-01 jihyeon jang implemented into wrf
!
! subprograms called:
!   fpvs     - function to compute saturation vapor pressure
!
! references:
!   han and pan (2010, wea. forecasting)
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
           dz       = zi(i,k+1) - zi(i,k)
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
           dz       = zi(i,k) - zi(i,k-1)
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
           dz   = zi(i,k) - zi(i,k-1)
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
           dz    = zi(i,k) - zi(i,k-1)
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
           dz    = zi(i,k) - zi(i,k-1)
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
       vshear(i) = 1.e3 * vshear(i) / (zi(i,ktcon(i))-zi(i,kb(i)))
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
           dz = zi(i,k) - zi(i,k-1)
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
END MODULE module_cu_nsas
!
