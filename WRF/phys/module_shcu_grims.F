!WRF:model_layer:physics
!
!
!
!
module module_shcu_grims
!
   integer,parameter :: nxtdp = 5001
   integer,parameter :: nxthe = 241,nythe = 151
   integer,parameter :: nxma = 151,nyma = 121
   integer,parameter :: nxsvp = 7501
!
   real,parameter    :: t0c = 2.7315e+2
   real,parameter    :: psat = 6.1078e+2
   real,parameter    :: rd = 2.8705e+2
   real,parameter    :: rv = 4.6150e+2
   real,parameter    :: cp = 1.0046e+3
   real,parameter    :: hvap = 2.5000e+6
   real,parameter    :: cvap = 1.8460e+3
   real,parameter    :: cliq = 4.1855e+3
   real,parameter    :: cice = 2.1060E+3
   real,parameter    :: hsub = 2.8340E+6
   real,parameter    :: terrm = 1.e-4
!
   real,parameter    :: rocp=rd/cp
   real,parameter    :: cpor=cp/rd
   real,parameter    :: eps=rd/rv
   real,parameter    :: ttp=t0c+0.01
   real,parameter    :: psatk=psat*1.e-3
   real,parameter    :: psatb=psatk*1.e-2
   real,parameter    :: dldt=cvap-cliq,xa=-dldt/rv,xb=xa+hvap/(rv*ttp)
   real,parameter    :: dldti=cvap-cice,xai=-dldti/rv,xbi=xai+hsub/(rv*ttp)
!
   real,save         :: c1xma,c2xma,c1yma,c2yma,c1xpvs,c2xpvs
   real,save         :: c1xtdp,c2xtdp,c1xthe,c2xthe,c1ythe,c2ythe
   real,save         :: tbtdp(nxtdp)
   real,save         :: tbthe(nxthe,nythe)
   real,save         :: tbtma(nxma,nyma), tbqma(nxma,nyma)
   real,save         :: tbpvs(nxsvp)
contains
!
!-------------------------------------------------------------------------------
   subroutine grims(qv3d,t3d,p3di,p3d,pi3d,z3di,                               &
                    wstar,hpbl,delta,                                          &
                    rthshten,rqvshten,                                         &
                    dt,g,xlv,rd,rv,rcp,p1000mb,                                &
                    kpbl2d,znu,raincv,                                         &
                    ids,ide, jds,jde, kds,kde,                                 &
                    ims,ime, jms,jme, kms,kme,                                 &
                    its,ite, jts,jte, kts,kte)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
! input argument
!
!-- qv3d        3d specific humidity (kgkg-1)
!-- t3d         3d temperature (k)
!-- p3di        3d pressure (pa) at interface level
!-- p3d         3d pressure (pa)
!-- pi3d        3d exner function (dimensionless)
!-- z3di        3d z at interface level (m)
!-- wstar       convective velocity scale (ms-1) from pbl
!-- hpbl        pbl height (m)
!-- delta       entrainment layer depth (m)
!-- rthshten    computed theta tendency due to shallow convection scheme
!-- rqvshten    computed q_v tendency due to shallow convection scheme
!-- dt          time step (s)
!-- g           acceleration due to gravity (m/s^2)
!-- xlv         latent heat of vaporization (j/kg)
!-- rd          gas constant for dry air (j/kg/k)
!-- rv          gas constant for water vapor (j/kg/k)
!-- kpbl2d      k-index for pbl top
!-- raincv      time-step precipitation from cumulus convection scheme
!-- znu         eta values (sigma values)
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
!
! output argument
!-- rthshten    computed theta tendency due to shallow convection scheme
!-- rqvshten    computed q_v tendency due to shallow convection scheme
!-------------------------------------------------------------------------------
!
! local 
!
!-- icps        cps index, =1 for deep convection
!-- pi2di       2d exner function at interface level (dimensionless)
!-- delp2di     2d pressuer depth (pa) between interface levels
!-- zl          2d z (m)
!-- t1          2d temperature (k) will be changed by shallow convection
!-- q1          2d specific humidity (kgkg-1) will be changed by shallow convection
!-- levshc      maximum k-level for shallow convection
!
   integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte
!
   real,     intent(in   )   ::      dt,g,xlv,rd,rv,rcp,p1000mb
!
   integer,  dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                        kpbl2d
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          qv3d, &
                                                                          t3d, &
                                                                         p3di, &
                                                                          p3d, &
                                                                         pi3d, &
                                                                         z3di
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                          hpbl, &
                                                                        wstar, &
                                                                        delta, &
                                                                       raincv
!
   real,     dimension( kms:kme )                                            , &
             intent(in   )   ::                                           znu
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             optional                                                        , &
             intent(inout)   ::                                      rthshten, &
                                                                     rqvshten
!
!  local variables
!
   integer         ::  i,j,k,levshc
   real            ::  sigshc,rdelt
!
   integer,  dimension( its:ite )            ::                          icps
   real,     dimension( its:ite, kts:kte+1 ) ::                         pi2di
   real,     dimension( its:ite, kts:kte )   ::                       delp2di, &
                                                                           zl, &
                                                                           t1, &
                                                                           q1
!
   rdelt = 1.0/dt
   sigshc = 0.6
   levshc = 0
!
   do k = kts,kte
     if(znu(k).gt.sigshc) levshc = k + 1
   enddo
!
   pi2di(:,:) = 0.0
   delp2di(:,:) = 0.0
   zl(:,:) = 0.0
   t1(:,:) = 0.0
   q1(:,:) = 0.0
!
   do j = jts,jte ! outmost j loop
     icps(:) = 0
     do k = kts,kte
       do i = its,ite
         pi2di(i,k) = (p3di(i,k,j)/p1000mb)**rcp
       enddo
     enddo
     do i = its,ite
       pi2di(i,kte+1) = (p3di(i,kte+1,j)/p1000mb)**rcp
     enddo
!
     do k = kts,kte
       do i = its,ite
         delp2di(i,k) = p3di(i,k,j)-p3di(i,k+1,j)
         zl(i,k) = 0.5*(z3di(i,k,j)+z3di(i,k+1,j))
         t1(i,k) = t3d(i,k,j)
         q1(i,k) = qv3d(i,k,j)
       enddo
     enddo
!
     do i = its,ite
       if(raincv(i,j) .gt. 1.e-30) icps(i)=1
     enddo
!
     call grims2d(q=q1(its,kts),t=t1(its,kts),prsi=p3di(ims,kms,j),            &
              prsik=pi2di(its,kts),delprsi=delp2di(its,kts),                   &
              prsl=p3d(ims,kms,j),prslk=pi3d(ims,kms,j),zl=zl(its,kts),        &
              wstar=wstar(ims,j),hpbl=hpbl(ims,j),delta=delta(ims,j),          &
              dt=dt,cp=cp,g=g,xlv=xlv,rd=rd,rv=rv,                             &
              icps=icps(its),kpbl=kpbl2d(ims,j),levshc=levshc,                 &
              ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde,               &
              ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme,               &
              its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )
!
     if(present(rthshten).and.present(rqvshten)) then
       do k = kts,kte
         do i = its,ite
           rthshten(i,k,j)=(t1(i,k)-t3d(i,k,j))/pi3d(i,k,j)*rdelt
           rqvshten(i,k,j)=(q1(i,k)-qv3d(i,k,j))*rdelt
         enddo
       enddo
     endif
!
   enddo ! end outmost j loop
!
   end subroutine grims
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine grims2d(q,t,prsi,prsik,delprsi,prsl,prslk,zl,                    &
                       wstar,hpbl,delta,                                       &
                       dt,cp,g,xlv,rd,rv,                                      &
                       icps,kpbl,levshc,                                       &
                       ids,ide, jds,jde, kds,kde,                              &
                       ims,ime, jms,jme, kms,kme,                              &
                       its,ite, jts,jte, kts,kte)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
!
!   this scheme applies an eddy-diffusion approach within the shallow convective 
!   layer defined by the moist static energy profile and is coupled to the 
!   ysu pbl properties. this scheme names after the grims
!   shallow convection scheme since it was developed/evaluated in grims.
!
!   coded by song-you hong (yonsei university; ysu) and
!   implemented into wrf by jihyeon jang, hyeyum hailey shin, junhong lee (ysu), 
!       and wei wang (ncar) winter 2012
!
! references:
!   hong et al. (2013, manuscript in preparation)
!   hong et al. (2013, asia-pacific j. atmos. sci.) the global/regional 
!       integrated model system (grims) 
!
!-------------------------------------------------------------------------------
!
   integer,  intent(in   ) ::     levshc,                                      &
                                  ids,ide, jds,jde, kds,kde,                   &
                                  ims,ime, jms,jme, kms,kme,                   &
                                  its,ite, jts,jte, kts,kte
!
   real,     intent(in   ) ::     dt,cp,g,xlv,rd,rv
!
   integer,  dimension( ims:ime )                                            , &
             intent(in   ) ::                                            kpbl
!
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(in   ) ::                                            prsi
!
   real,     dimension( its:ite, kts:kte+1 )                                 , &
             intent(in   ) ::                                           prsik 
!
   real,     dimension( its:ite, kts:kte )                                   , &
             intent(in   ) ::                                         delprsi, &
                                                                           zl
!
   real,     dimension( ims:ime, kms:kme )                                   , &
             intent(in   ) ::                                            prsl, &
                                                                        prslk
!
   integer,     dimension( its:ite )                                         , &
             intent(in   ) ::                                            icps
!
   real,     dimension( its:ite, kts:kte )                                   , &
             intent(inout) ::                                               q, &
                                                                            t
!
   real,     dimension( ims:ime )                                            , &
             intent(in   ) ::                                            hpbl, &
                                                                        wstar, &
                                                                        delta
!
!  profile shape parameter
!
   real,parameter    ::  pfac = 3.
!
!  maximum and minimum diffusivity 
!
   real,parameter    ::  xkzmax = 50., xkzmin = 0.001
!
!  maxium distance of a parcel to lcl (m)
!
   real,parameter    ::  zdiffcr1 = 1000., zdiffcr2 = 1000.
!
!  bounds of parcel origin
!
   integer,parameter    ::  kliftl=2,kliftu=2
!
!  scale factor for wstar
!
   real,parameter       ::  wsfac = 1.47
!
!  local variables and arrays
!
   logical              ::  lshc(its:ite),flg(ite-its+1)
   integer              ::  i,ik,ik1,iku,k,k1,k2,kt,n2
   integer              ::  index2(ite-its+1)
   integer              ::  klcl(ite-its+1),kbot(ite-its+1)
   integer              ::  ktop(ite-its+1)
   integer              ::  lmin(ite-its+1)
   integer              ::  kb(ite-its+1),kbcon(ite-its+1)
   real                 ::  eps,epsm1
   real                 ::  eldq,xkzh,cpdt,rtdls
   real                 ::  dmse,dtodsu,dtodsl,dsig,dsdz1,dsdz2
   real                 ::  q2((ite-its+1)*kte)
   real                 ::  t2((ite-its+1)*kte)
   real                 ::  al((ite-its+1)*(kte-1))
   real                 ::  ad((ite-its+1)*kte)
   real                 ::  au((ite-its+1)*(kte-1))
   real                 ::  delprsi2((ite-its+1)*kte)
   real                 ::  prsi2((ite-its+1)*kte),prsik2((ite-its+1)*kte)
   real                 ::  prsl2((ite-its+1)*kte),prslk2((ite-its+1)*kte)
   real                 ::  qeso2((ite-its+1)*kte),rh2(ite-its+1)
   real                 ::  depth(ite-its+1),zdiff1(ite-its+1),zdiff2(ite-its+1)
   real                 ::  hmin(ite-its+1),hmax(ite-its+1)
   real                 ::  z(1:(ite-its+1),kts:kte)
   real                 ::  heo(1:(ite-its+1),kts:kte)
   real                 ::  heso(1:(ite-its+1),kts:kte)
   real                 ::  pik,height,xkzfac
!-------------------------------------------------------------------------------
!
   eps   = rd/rv
   epsm1 = eps-1
!
   do i = its,ite
     lshc(i)=.false.
   enddo
!
!  check for moist static instability to trigger convection
!
   do k = kts,levshc-1
     do i = its,ite
       if(icps(i).eq.0.and.kpbl(i).ge.2) then
         eldq = xlv*(q(i,k)-q(i,k+1))
         cpdt = cp*(t(i,k)-t(i,k+1))
         rtdls = (prsl(i,k)-prsl(i,k+1))/prsi(i,k+1)*rd*0.5*(t(i,k)+t(i,k+1))
         dmse = eldq+cpdt-rtdls
         lshc(i) = lshc(i).or.dmse.gt.0.
       endif
     enddo
   enddo
   do i = its,ite
     if(wstar(i).lt.0.001) lshc(i)=.false.
   enddo
!
!  reset i-dimension for active clouds
!
   n2 = 0
   do i = its,ite
     if(lshc(i)) then
       n2 = n2+1
       index2(n2) = i
     endif
   enddo
!
   if(n2.eq.0) return
!
!  prepare the variables 
!
   do k = kts,levshc
     do i = 1,n2
       if(lshc(index2(i))) then
         ik = (k-1)*n2+i
         pik = prsl(index2(i),k) 
         q2(ik) = q(index2(i),k)
         t2(ik) = t(index2(i),k)
         delprsi2(ik) = delprsi(index2(i),k)
         prsi2(ik) = prsi(index2(i),k)
         prsl2(ik) = prsl(index2(i),k)
         prsik2(ik)= prsik(index2(i),k)
         prslk2(ik)= prslk(index2(i),k)
         z(i,k) = zl(index2(i),k)
         qeso2(ik) = fpvs_pa(t2(ik)) 
         qeso2(ik) = eps * qeso2(ik) / (pik + epsm1 * qeso2(ik))
         qeso2(ik) = max(qeso2(ik),1.E-8)
         heo(i,k)  = g * z(i,k) + cp* t2(ik) + xlv * q2(ik)
         heso(i,k) = g * z(i,k) + cp* t2(ik) + xlv * qeso2(ik)
       endif
     enddo
   enddo
!
! determine largest moist static energy for updraft originating level
!
   do i = 1,n2
     if(lshc(index2(i))) then
       hmax(i) = heo(i,1)
       kb(i) = 1
       kbcon(i) = levshc
       flg(i) = .true.
       zdiff1(i) = -1.0
       zdiff2(i) = -1.0
     endif
   enddo
!
   do k = kts+1,levshc-1
     do i = 1,n2
       if(lshc(index2(i))) then
         if(heo(i,k).gt.hmax(i).and.k.le.kpbl(index2(i))) then
           kb(i) = k
           hmax(i) = heo(i,k)
         endif
       endif
     enddo
   enddo
!
! check the buoyancy of a parcel by the distance to lcl and lfc
!
   do k = kts+1,levshc-1
     do i = 1,n2
       if(lshc(index2(i)).and.flg(i)) then
         if(k.gt.kb(i).and.heo(i,kb(i)).gt.heso(i,k)) then
           flg(i) = .false.
           kbcon(i) = k
         endif
       endif
     enddo
   enddo
!
   do i = 1,n2
     if(lshc(index2(i))) then
       zdiff1(i) = z(i,kpbl(index2(i)))-z(i,kb(i))
       zdiff2(i) = z(i,kbcon(i))-z(i,kb(I))
       if(zdiff1(i).gt.zdiffcr1.or.zdiff1(i).lt.0.) lshc(index2(i)) = .false.
       if(zdiff2(i).gt.zdiffcr2) lshc(index2(i)) = .false.
     endif
   enddo
!
!  compute moist adiabat and determine cloud top
!
   call phys_moist_adiabat_pa(n2,levshc-1,kliftl,kliftu,                       &
                           prsl2,prsik2,prslk2,t2,q2,                          &
                           klcl,kbot,ktop,al,au,rd,rv,                         &
                            ids,ide, jds,jde, kds,kde,                         &
                            ims,ime, jms,jme, kms,kme,                         &
                            its,ite, jts,jte, kts,kte)
!
   do i = 1,n2
     if(lshc(index2(i))) then
       kbot(i) = max(kpbl(index2(i)),2)
       kbot(i) = min(kbot(i),levshc)
       ktop(i) = ktop(i)+1
     endif
   enddo
!
!  revise the cloud top below minimum moist static energy
!
   do i = 1,n2
     if(lshc(index2(i))) then
       hmin(i) = heo(i,kbot(i))
       lmin(i) = levshc
     endif
   enddo
!
   do k = kts,levshc
     do i = 1,n2
       if(lshc(index2(i))) then
         if(heo(i,k).lt.hmin(i).and.k.ge.kbot(i).and.k.le.ktop(i)) then
           lmin(i) = k + 1
           hmin(i) = heo(i,k)
         endif
       endif
     enddo
   enddo
!
   do i = 1,n2
     if(lshc(index2(i))) then
       ktop(i) = min(ktop(i),lmin(i))
     endif
   enddo
!
   do i = 1,n2
     if(lshc(index2(i))) then
       if((ktop(i)-kbot(i)).le.1) then
         lshc(index2(i)) = .false.
       endif
     endif
   enddo
!
!  compute diffusion properties
!
   do i = 1,n2
     if(lshc(index2(i))) then
       k = kbot(i)-1
       ik=(k-1)*n2+i
       rh2(i) = q2(ik)/qeso2(ik)
     endif
   enddo
!
   k1 = levshc+1
   k2 = 0
   do i = 1,n2
     if(.not.lshc(index2(i))) then
       kbot(i) = levshc+1
       ktop(i) = 0
     else
       depth(i) = z(i,ktop(i))-z(i,kbot(i))
     endif
     k1 = min(k1,kbot(i))
     k2 = max(k2,ktop(i))
   enddo
   kt = k2-k1+1
   if(kt.lt.2) return
!
!  set eddy viscosity coefficient xkzh at sigma interfaces
!
   do i = 1,n2
     ik = (k1-1)*n2+i
     ad(ik) = 1.
   enddo
!
   do k = kts,levshc-1
     do i = 1,n2
       if(k.ge.kbot(i).and.k.lt.ktop(i)) then
         ik = (k-1)*n2+i
         iku = k*n2+i
         rtdls = (prsl2(ik)-prsl2(iku))/prsi2(iku)*rd*0.5*(t2(ik)+t2(iku))
         au(ik) = g/rtdls
       endif
     enddo
   enddo
!
   do k = k1,k2-1
     do i = 1,n2
       ik = (k-1)*n2+i
       iku = k*n2+i
       dtodsl = 2.*dt/delprsi2(ik)
       dtodsu = 2.*dt/delprsi2(iku)
       dsig = prsl2(ik)-prsl2(iku)
       if(k.ge.kbot(i).and.k.lt.ktop(i)) then
         height = z(i,k)-z(i,kbot(i))
         xkzfac = rh2(i)*wsfac*wstar(index2(i))*delta(index2(i))
         xkzh = min(max(xkzfac*(1.-(height+hpbl(index2(i)))                    &
                /(depth(i)+hpbl(index2(i))))**pfac,xkzmin),xkzmax)
       else
         xkzh = 0.
       endif
       dsdz1 = xkzh*dsig*au(ik)*g/cp
       dsdz2 = xkzh*dsig*au(ik)*au(ik)
       au(ik) = -dtodsl*dsdz2
       al(ik) = -dtodsu*dsdz2
       ad(ik) = ad(ik)-au(ik)
       ad(iku) = 1.-al(ik)
       t2(ik) = t2(ik)+dtodsl*dsdz1
       t2(iku) = t2(iku)-dtodsu*dsdz1
     enddo
   enddo
!
!  solve tri-diagonal matrix
!
   ik1 = (k1-1)*n2+1
   call scv_tri_diagonal_grims(n2,n2,kt,al(ik1),ad(ik1),au(ik1),               &
                                q2(ik1),t2(ik1),au(ik1),q2(ik1),t2(ik1))
!
!  feedback to large-scale variables  
!
   do k = k1,k2
     do i = 1,n2
       if(lshc(index2(i))) then
         ik = (k-1)*n2+i
         q(index2(i),k) = q2(ik)
         t(index2(i),k) = t2(ik)
       endif
     enddo
   enddo
!
   return
   end subroutine grims2d
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine scv_tri_diagonal_grims(lons2,l,n,cl,cm,cu,r1,r2,au,a1,a2)
!-------------------------------------------------------------------------------
!
! subprogram:  scv_tri_diagonal    
!                                                                               
! abstract: this routine solves multiple tridiagonal matrix problems            
!   with 2 right-hand-side and solution vectors for every matrix.               
!   the solutions are found by eliminating off-diagonal coefficients,           
!   marching first foreward then backward along the matrix diagonal.            
!   the computations are vectorized around the number of matrices.              
!   no checks are made for zeroes on the diagonal or singularity.               
!                                                                               
! program history log:                                                          
!   1991-05-07  iredell                                                           
!   2009-03-01  jung-eun kim         fortran 90 and modules
!                                                                               
! usage:    call scv_tri_diagonal(l,n,cl,cm,cu,r1,r2,au,a1,a2)      
!                                                                               
!   input argument list:                                                        
!     l        - integer number of tridiagonal matrices                         
!     n        - integer order of the matrices                                  
!     cl       - real (l,2:n) lower diagonal matrix elements                    
!     cm       - real (l,n) main diagonal matrix elements                       
!     cu       - real (l,n-1) upper diagonal matrix elements                    
!                (may be equivalent to au if no longer needed)                  
!     r1       - real (l,n) 1st right-hand-side vector elements                 
!                (may be equivalent to a1 if no longer needed)                  
!     r2       - real (l,n) 2nd right-hand-side vector elements                 
!                (may be equivalent to a2 if no longer needed)                  
!                                                                               
!   output argument list:                                                       
!     au       - real (l,n-1) work array                                        
!     a1       - real (l,n) 1st solution vector elements                        
!     a2       - real (l,n) 2nd solution vector elements                        
!                                                                               
! remarks: this routine can be easily modified to solve a different             
!   number of right-hand-sides and solutions per matrix besides 2.              
!                                                                               
!-------------------------------------------------------------------------------
   implicit none
   integer              ::  lons2,l,n,i,k
   real                 ::  fk
   real                 ::  cl(l,2:n),cm(l,n),cu(l,n-1),r1(l,n),r2(l,n),       &
                            au(l,n-1),a1(l,n),a2(l,n)
!-------------------------------------------------------------------------------
   do i = 1,lons2
     fk = 1./cm(i,1)                                                           
     au(i,1)= fk*cu(i,1)                                                      
     a1(i,1)= fk*r1(i,1)                                                      
     a2(i,1)= fk*r2(i,1)                                                      
   enddo                                                                     
!
   do k = 2,n-1                                                                
     do i = 1,lons2
       fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))                                     
       au(i,k) = fk*cu(i,k)                                                    
       a1(i,k) = fk*(r1(i,k)-cl(i,k)*a1(i,k-1))                                
       a2(i,k) = fk*(r2(i,k)-cl(i,k)*a2(i,k-1))                                
     enddo                                                                   
   enddo                                                                     
!
   do i = 1,lons2
     fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))                                       
     a1(i,n) = fk*(r1(i,n)-cl(i,n)*a1(i,n-1))                                  
     a2(i,n) = fk*(r2(i,n)-cl(i,n)*a2(i,n-1))                                  
   enddo                                                                     
   do k = n-1,1,-1                                                             
     do i = 1,lons2
       a1(i,k) = a1(i,k)-au(i,k)*a1(i,k+1)                                     
       a2(i,k) = a2(i,k)-au(i,k)*a2(i,k+1)                                     
     enddo                                                                   
   enddo                                                                     
!
   return                                                                    
   end subroutine scv_tri_diagonal_grims
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine phys_moist_adiabat_pa(ilev,klev,k1,k2,                           &
                                 prsl,prsik,prslk,tenv,qenv,                   &
                                 klcl,kbot,ktop,tcld,qcld,rd,rv,               &
                                      ids,ide, jds,jde, kds,kde,               &
                                      ims,ime, jms,jme, kms,kme,               &
                                      its,ite, jts,jte, kts,kte)
!-------------------------------------------------------------------------------
!
! subprogram: phys_moist_adiabat_pa
!
! abstract: 
! - compute moist adiabatic cloud soundings
! - atmospheric columns of temperature and specific humidity
!   are examined by this routine for conditional instability.
!   the test parcel is chosen from the layer between layers k1 and k2
!   that has the warmest potential wet-bulb temperature.
!   excess cloud temperatures and specific humidities are returned
!   where the lifted parcel is found to be buoyant.
!   fast inlinable functions are invoked to compute
!   dewpoint and lifting condensation level temperatures,
!   equivalent potential temperature at the lcl, and
!   temperature and specific humidity of the ascending parcel.
!
! program history log:
!   1983-11-01  phillips
!   1991-05-07  iredell                arguments changed, code tidied
!   2000-01-01  song-you hong          physcis options
!   2009-10-01  jung-eun kim           f90 format with standard physics modules
!   2010-07-01  myung-seo koo          dimension allocatable with namelist input
!
! usage:  call phys_moist_adiabat_pa(ilev,klev,k1,k2,                          &
!                                 prsl,prslk,prsik,tenv,qenv,                  &
!                                 klcl,kbot,ktop,tcld,qcld,rd,rv,              &
!                                      ids,ide, jds,jde, kds,kde,              &
!                                      ims,ime, jms,jme, kms,kme,              &
!                                      its,ite, jts,jte, kts,kte)
!
!   input argument list:
!     ilev         - integer number of atmospheric columns
!     klev         - integer number of sigma levels in a column
!     k1           - integer lowest level from which a parcel can originate
!     k2           - integer highest level from which a parcel can originate
!     prsl         - real (ilev,klev) pressure values
!     prslk,prsik  - real (ilev,klev) pressure values to the kappa
!     tenv         - real (ilev,klev) environment temperatures
!     qenv         - real (ilev,klev) environment specific humidities
!
!   output argument list:
!     klcl     - integer (ilev) level just above lcl (klev+1 if no lcl)
!     kbot     - integer (ilev) level just above cloud bottom
!     ktop     - integer (ilev) level just below cloud top
!              - note that kbot(i) gt ktop(i) if no cloud.
!     tcld     - real (ilev,klev) of excess cloud temperatures.
!                (parcel t minus environ t, or 0. where no cloud)
!     qcld     - real (ilev,klev) of excess cloud specific humidities.
!                (parcel q minus environ q, or 0. where no cloud)
!
! subprograms called:
!     ftdp     - function to compute dewpoint temperature
!     ftlcl    - function to compute lcl temperature
!     fthe     - function to compute equivalent potential temperature
!     ftma     - function to compute parcel temperature and humidity
!
! remarks: all functions are inlined by fpp.
!          nonstandard automatic arrays are used.
!
!-------------------------------------------------------------------------------
   implicit none
!
   integer,parameter    ::  nx=151,ny=121
   integer              ::  ilev,klev,k1,k2
   real                 ::  prsl(ilev,klev),prslk(ilev,klev),prsik(ilev,klev)
   real                 ::  tenv(ilev,klev),qenv(ilev,klev)
   integer              ::  klcl(ilev),kbot(ilev),ktop(ilev)
   real                 ::  tcld(ilev,klev),qcld(ilev,klev)
   real                 ::  rd,rv
   integer              ::  ids,ide, jds,jde, kds,kde,                         &
                            ims,ime, jms,jme, kms,kme,                         &
                            its,ite, jts,jte, kts,kte
!
!  local arrays
!
   real                 ::  slkma(ilev)
   real                 ::  thema(ilev)
   real                 ::  pv,tdpd
   real                 ::  slklcl,thelcl,tlcl
   real                 ::  xj,yj
   real                 ::  ftx1,ftx2,ftma1,qx1,qx2,qma,tma,tvcld,tvenv
   real                 ::  eps,epsm1,ftv
   integer              ::  i,k,jx,jy
!-------------------------------------------------------------------------------
!
!  compute parameters
!   
   eps=rd/rv
   epsm1=rd/rv-1.
   ftv=rv/rd-1.
!
!  determine warmest potential wet-bulb temperature between k1 and k2.
!  compute its lifting condensation level.
!  
   do i = 1,ilev
     slkma(i)=0.
     thema(i)=0.
   enddo
!
   do k = k1,k2
     do i = 1,ilev
       pv=prsl(i,k)*1.e-3*qenv(i,k)/(eps-epsm1*qenv(i,k))
       tdpd=tenv(i,k)-ftdp(pv)
       if(tdpd.gt.0.) then
         tlcl=ftlcl(tenv(i,k),tdpd)
         slklcl=prslk(i,k)/prsik(i,1)*tlcl/tenv(i,k)
       else
         tlcl=tenv(i,k)
         slklcl=prslk(i,k)/prsik(i,1)
       endif
       thelcl=fthe(tlcl,slklcl*prsik(i,1))
       if(thelcl.gt.thema(i)) then
         slkma(i)=slklcl
         thema(i)=thelcl
       endif
     enddo
   enddo
!
!  set cloud temperatures and humidities wherever the parcel lifted up
!  the moist adiabat is buoyant with respect to the environment.
!  
   do i = 1,ilev
     klcl(i)=klev+1
     kbot(i)=klev+1
     ktop(i)=0
   enddo
!
   do k = kts,klev
     do i = 1,ilev
       tcld(i,k)=0.
       qcld(i,k)=0.
     enddo
   enddo
!
   do k = k1,klev
     do i = 1,ilev
       if(prslk(i,k)/prsik(i,1).le.slkma(i)) then
         klcl(i)=min(klcl(i),k)
!
! insert ftma   tma=ftma(thema(i),prslk(i,k),qma)
!
         xj=min(max(c1xma+c2xma*thema(i),1.),float(nx))
         yj=min(max(c1yma+c2yma*prslk(i,k),1.),float(ny))
         jx=min(xj,nx-1.)
         jy=min(yj,ny-1.)
         ftx1=tbtma(jx,jy)+(xj-jx)*(tbtma(jx+1,jy)-tbtma(jx,jy))
         ftx2=tbtma(jx,jy+1)+(xj-jx)*(tbtma(jx+1,jy+1)-tbtma(jx,jy+1))
         ftma1=ftx1+(yj-jy)*(ftx2-ftx1)
         qx1=tbqma(jx,jy)+(xj-jx)*(tbqma(jx+1,jy)-tbqma(jx,jy))
         qx2=tbqma(jx,jy+1)+(xj-jx)*(tbqma(jx+1,jy+1)-tbqma(jx,jy+1))
         qma=qx1+(yj-jy)*(qx2-qx1)
         tma=ftma1
!
         tvcld=tma*(1.+ftv*qma)
         tvenv=tenv(i,k)*(1.+ftv*qenv(i,k))
         if(tvcld.gt.tvenv) then
           kbot(i)=min(kbot(i),k)
           ktop(i)=max(ktop(i),k)
           tcld(i,k)=tma-tenv(i,k)
           qcld(i,k)=qma-qenv(i,k)
         endif
       endif
     enddo
   enddo
!
   return
   end subroutine phys_moist_adiabat_pa
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function ftdp(pv)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real             :: ftdp
!
   integer          :: jx
   real             :: xj
   real             :: xmax,xmin,xinc
   real             :: pv
!
   xmin= 0.001
   xmax=10.001
   xinc=(xmax-xmin)/(nxtdp-1)
   c1xtdp=1.-xmin/xinc
   c2xtdp=1./xinc
!
   xj=min(max(c1xtdp+c2xtdp*pv,1.),float(nxtdp))
   jx=min(xj,nxtdp-1.)
   ftdp=tbtdp(jx)+(xj-jx)*(tbtdp(jx+1)-tbtdp(jx))
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function ftlcl(t,tdpd)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real             :: ftlcl
!
   real,parameter   :: clcl1=0.954442e+0, clcl2=0.967772e-3,           &
                       clcl3=-0.710321e-3,clcl4=-0.270742e-5
   real             ::  t,tdpd
!
   ftlcl=t-tdpd*(clcl1+clcl2*t+tdpd*(clcl3+clcl4*t))
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function fthe(t,pk)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real             :: fthe
!
   integer          :: jx,jy
   real             :: xmin,xmax,xinc,ymin,ymax,yinc
   real             :: xj,yj
   real             :: ftx1,ftx2
   real             :: t,pk
!
   xmin=ttp-90.
   xmax=ttp+30.
   xinc=(xmax-xmin)/(nxthe-1)
   c1xthe=1.-xmin/xinc
   c2xthe=1./xinc
   ymin=0.04**rocp
   ymax=1.10**rocp
   yinc=(ymax-ymin)/(nythe-1)
   c1ythe=1.-ymin/yinc
   c2ythe=1./yinc
!
   xj=min(c1xthe+c2xthe*t,float(nxthe))
   yj=min(c1ythe+c2ythe*pk,float(nythe))
   if(xj.ge.1..and.yj.ge.1.) then
     jx=min(xj,nxthe-1.)
     jy=min(yj,nythe-1.)
     ftx1=tbthe(jx,jy)+(xj-jx)*(tbthe(jx+1,jy)-tbthe(jx,jy))
     ftx2=tbthe(jx,jy+1)+(xj-jx)*(tbthe(jx+1,jy+1)-tbthe(jx,jy+1))
     fthe=ftx1+(yj-jy)*(ftx2-ftx1)
   else
     fthe=0.
   endif
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function fpvs_pa(t)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real                 :: fpvs_pa
!
   integer              :: jx
   real                 :: xmax,xmin,xinc
   real                 :: xj
   real                 :: t
!
   xmin=180.0
   xmax=330.0
!
   xj=min(max(c1xpvs+c2xpvs*t,1.),float(nxsvp))
   jx=min(xj,nxsvp-1.)
   fpvs_pa=tbpvs(jx)+(xj-jx)*(tbpvs(jx+1)-tbpvs(jx))
   fpvs_pa=fpvs_pa * 1.e3
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine grimsinit(rthshten,rqvshten,                                     &
                        restart,                                               &
                        ids,ide, jds,jde, kds,kde,                             &
                        ims,ime, jms,jme, kms,kme,                             &
                        its,ite, jts,jte, kts,kte                  )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   logical , intent(in)           ::  restart
   integer , intent(in)           ::  ids, ide, jds, jde, kds, kde,            &
                                      ims, ime, jms, jme, kms, kme,            &
                                      its, ite, jts, jte, kts, kte
   real,     dimension( ims:ime , kms:kme , jms:jme ) , intent(out) ::         &
                                                                     rthshten, &
                                                                     rqvshten
   integer :: i, j, k, itf, jtf, ktf
!
   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)
   if(.not.restart)then
     do j = jts,jtf
       do k = kts,ktf
         do i = its,itf
           rthshten(i,k,j)=0.
           rqvshten(i,k,j)=0.
         enddo
       enddo
     enddo
   endif
!
   call funct_dew_point_temp_init
   call funct_pot_temp_init
   call funct_moist_adiabat_init
   call funct_svp_init
!
   end subroutine grimsinit
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine funct_dew_point_temp_init
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer          :: jx
   real             :: xmax,xmin,xinc,pv,x,t
!
   xmin= 0.001
   xmax=10.001
   xinc=(xmax-xmin)/(nxtdp-1)
   c1xtdp=1.-xmin/xinc
   c2xtdp=1./xinc
   t=208.0
   do jx=1,nxtdp
     x=xmin+(jx-1)*xinc
     pv=x
     t=ftdpxg(t,pv)
     tbtdp(jx)=t
   enddo
!
   return
   end subroutine funct_dew_point_temp_init
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine funct_pot_temp_init
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer          :: jx,jy
   real             :: xmin,xmax,xinc,ymin,ymax,yinc
   real             :: x,y,t,pk
!
   xmin=ttp-90.
   xmax=ttp+30.
   xinc=(xmax-xmin)/(nxthe-1)
   c1xthe=1.-xmin/xinc
   c2xthe=1./xinc
   ymin=0.04**rocp
   ymax=1.10**rocp
   yinc=(ymax-ymin)/(nythe-1)
   c1ythe=1.-ymin/yinc
   c2ythe=1./yinc
!
   do jy = 1,nythe
     y=ymin+(jy-1)*yinc
     pk=y
     do jx = 1,nxthe
       x=xmin+(jx-1)*xinc
       t=x
       tbthe(jx,jy)=fthex(t,pk)
     enddo
   enddo
!
   return
   end subroutine funct_pot_temp_init
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine funct_moist_adiabat_init
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer          :: jx,jy
   real             :: xmin,xmax,xinc,ymin,ymax,yinc
   real             :: y,pk,t,x,the,q
!
   xmin=200.
   xmax=500.
   xinc=(xmax-xmin)/(nxma-1)
   c1xma=1.-xmin/xinc
   c2xma=1./xinc
   ymin=0.01**rocp
   ymax=1.10**rocp
   yinc=(ymax-ymin)/(nyma-1)
   c1yma=1.-ymin/yinc
   c2yma=1./yinc
!
   do jy = 1,nyma
     y=ymin+(jy-1)*yinc
     pk=y
     t=xmin*y
     do jx = 1,nxma
       x=xmin+(jx-1)*xinc
       the=x
       t=ftmaxg(t,the,pk,q)
       tbtma(jx,jy)=t
       tbqma(jx,jy)=q
     enddo
   enddo
!
   return
   end subroutine funct_moist_adiabat_init
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine funct_svp_init
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer          :: jx
   real             :: xmin,xmax,xinc
   real             :: t,x
!
   xmin=180.0
   xmax=330.0
   xinc=(xmax-xmin)/(nxsvp-1)
   c1xpvs=1.-xmin/xinc
   c2xpvs=1./xinc
   do jx = 1,nxsvp
     x=xmin+(jx-1)*xinc
     t=x
     tbpvs(jx)=fpvsx(t)
   enddo
!
   return
   end subroutine funct_svp_init
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function ftdpxg(tg,pv)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real             :: ftdpxg
!
   real             :: tg,pv
   real             :: t,tr,pvt,el,dpvt,terr
!
   t=tg
   tr=ttp/t
   pvt=psatk*(tr**xa)*exp(xb*(1.-tr))
   el=hvap+dldt*(t-ttp)
   dpvt=el*pvt/(rv*t**2)
   terr=(pvt-pv)/dpvt
   t=t-terr
!
   do while(abs(terr).gt.terrm)
     tr=ttp/t
     pvt=psatk*(tr**xa)*exp(xb*(1.-tr))
     el=hvap+dldt*(t-ttp)
     dpvt=el*pvt/(rv*t**2)
     terr=(pvt-pv)/dpvt
     t=t-terr
   enddo
   ftdpxg=t
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function fthex(t,pk)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real             :: fthex
!
   real             :: t,pk,p
   real             :: tr, pv, pd, el, expo
!
   p=pk**cpor
   tr=ttp/t
   pv=psatb*(tr**xa)*exp(xb*(1.-tr))
   pd=p-pv
   if(pd.gt.0.) then
     el=hvap+dldt*(t-ttp)
     expo=el*eps*pv/(cp*t*pd)
     expo = min(expo,100.0)
     fthex=t*pd**(-rocp)*exp(expo)
   else
     fthex=0.
   endif
!
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function ftmaxg(tg,the,pk,qma)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real             :: ftmaxg
   real             :: tg,the,pk,t,p,tr,pv,pd,el,expo,thet,dthet,terr,qma
!
   t=tg
   p=pk**cpor
   tr=ttp/t
   pv=psatb*(tr**xa)*exp(xb*(1.-tr))
   pd=p-pv
   el=hvap+dldt*(t-ttp)
   expo=el*eps*pv/(cp*t*pd)
   thet=t*pd**(-rocp)*exp(expo)
   dthet=thet/t*(1.+expo*(dldt*t/el+el*p/(rv*t*pd)))
   terr=(thet-the)/dthet
   t=t-terr
!
   do while(abs(terr).gt.terrm)
     tr=ttp/t
     pv=psatb*(tr**xa)*exp(xb*(1.-tr))
     pd=p-pv
     el=hvap+dldt*(t-ttp)
     expo=el*eps*pv/(cp*t*pd)
     thet=t*pd**(-rocp)*exp(expo)
     dthet=thet/t*(1.+expo*(dldt*t/el+el*p/(rv*t*pd)))
     terr=(thet-the)/dthet
     t=t-terr
   enddo
   ftmaxg=t
   tr=ttp/t
   pv=psatb*(tr**xa)*exp(xb*(1.-tr))
   pd=p-pv
   qma=eps*pv/(pd+eps*pv)
!  
   return
   end function
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   function fpvsx(t)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   real             :: fpvsx
!
   real             :: t
   real             :: tr
!
   tr=ttp/t
   if(t.ge.ttp) then
     fpvsx=psatk*(tr**xa)*exp(xb*(1.-tr))
   else
     fpvsx=psatk*(tr**xai)*exp(xbi*(1.-tr))
   endif
!
   return
   end function
!-------------------------------------------------------------------------------
end module module_shcu_grims
