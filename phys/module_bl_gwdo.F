!WRF:model_layer:physics
!
!
!
!
module module_bl_gwdo
contains
!-------------------------------------------------------------------------------
   subroutine gwdo(u3d,v3d,t3d,qv3d,p3d,p3di,pi3d,z,                           &
                  rublten,rvblten,                                             &
                  dtaux3d,dtauy3d,dusfcg,dvsfcg,                               &
                  var2d,oc12d,oa2d1,oa2d2,oa2d3,oa2d4,ol2d1,ol2d2,ol2d3,ol2d4, &
                  znu,znw,mut,p_top,                                           &
                  cp,g,rd,rv,ep1,pi,                                           &
                  dt,dx,kpbl2d,itimestep,                                      &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!                                                                       
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- p3d         3d pressure (pa)
!-- p3di        3d pressure (pa) at interface level
!-- pi3d        3d exner function (dimensionless)
!-- rublten     u tendency due to pbl parameterization (m/s/s) 
!-- rvblten     v tendency due to pbl parameterization (m/s/s)
!-- znu         eta values (sigma values)
!-- cp          heat capacity at constant pressure for dry air (j/kg/k)
!-- g           acceleration due to gravity (m/s^2)
!-- rd          gas constant for dry air (j/kg/k)
!-- z           height above sea level (m)
!-- rv          gas constant for water vapor (j/kg/k)
!-- dt          time step (s)
!-- dx          model grid interval (m)
!-- ep1         constant for virtual temperature (r_v/r_d - 1) (dimensionless)
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
!-------------------------------------------------------------------------------
  integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                 &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte
  integer,  intent(in   )   ::      itimestep
!
  real,     intent(in   )   ::      dt,dx,cp,g,rd,rv,ep1,pi
!
  real,     dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(in   )   ::                                           qv3d, &
                                                                          p3d, &
                                                                         pi3d, &
                                                                          t3d, &
                                                                             z
  real,     dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(in   )   ::                                           p3di
!
  real,     dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(inout)   ::                                        rublten, &
                                                                      rvblten
  real,     dimension( ims:ime, kms:kme, jms:jme )                           , &
            intent(inout)   ::                                        dtaux3d, &
                                                                      dtauy3d
!
  real,      dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                           u3d, &
                                                                          v3d
!
  integer,   dimension( ims:ime, jms:jme )                                   , &
             intent(in  )   ::                                         kpbl2d
  real,   dimension( ims:ime, jms:jme )                                      , &
             intent(inout  )   ::                                      dusfcg, &
                                                                       dvsfcg
!
  real,   dimension( ims:ime, jms:jme )                                      , &
             intent(in  )   ::                                          var2d, &
                                                                        oc12d, &
                                                      oa2d1,oa2d2,oa2d3,oa2d4, &
                                                      ol2d1,ol2d2,ol2d3,ol2d4
  real,     dimension( ims:ime, jms:jme )                                    , &
            optional                                                         , &
            intent(in  )   ::                                             mut
!
  real,     dimension( kms:kme )                                             , &
            optional                                                         , &
            intent(in  )   ::                                             znu, &
                                                                          znw
!
  real,     optional, intent(in  )   ::                                 p_top
!
!local
!
  real,   dimension( its:ite, kts:kte )  ::                           delprsi, &
                                                                          pdh
  real,     dimension( its:ite, kts:kte+1 )   ::                         pdhi
  real,   dimension( its:ite, 4 )        ::                               oa4, &
                                                                          ol4
  integer ::  i,j,k,kdt,kpblmax
!
   do k = kts,kte
     if(znu(k).gt.0.6) kpblmax = k + 1
   enddo
!
   do j = jts,jte
      if(present(mut))then
! For ARW we will replace p and p8w with dry hydrostatic pressure
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
!
      do k = kts,kte
        do i = its,ite
          delprsi(i,k) = pdhi(i,k)-pdhi(i,k+1)
        enddo
      enddo
        do i = its,ite
            oa4(i,1) = oa2d1(i,j)
            oa4(i,2) = oa2d2(i,j)
            oa4(i,3) = oa2d3(i,j)
            oa4(i,4) = oa2d4(i,j)
            ol4(i,1) = ol2d1(i,j)
            ol4(i,2) = ol2d2(i,j)
            ol4(i,3) = ol2d3(i,j)
            ol4(i,4) = ol2d4(i,j)
        enddo
      call gwdo2d(dudt=rublten(ims,kms,j),dvdt=rvblten(ims,kms,j)              &
              ,dtaux2d=dtaux3d(ims,kms,j),dtauy2d=dtauy3d(ims,kms,j)           &
              ,u1=u3d(ims,kms,j),v1=v3d(ims,kms,j)                             &
              ,t1=t3d(ims,kms,j),q1=qv3d(ims,kms,j)                            &
              ,del=delprsi(its,kts)                                            &
              ,prsi=pdhi(its,kts)                                              &
              ,prsl=pdh(its,kts),prslk=pi3d(ims,kms,j)                         &
              ,zl=z(ims,kms,j),rcl=1.0                                         &
              ,kpblmax=kpblmax                                                 &
              ,dusfc=dusfcg(ims,j),dvsfc=dvsfcg(ims,j)                         &
              ,var=var2d(ims,j),oc1=oc12d(ims,j)                               &
              ,oa4=oa4,ol4=ol4                                                 &
              ,g=g,cp=cp,rd=rd,rv=rv,fv=ep1,pi=pi                              &
              ,dxmeter=dx,deltim=dt                                            &
              ,kpbl=kpbl2d(ims,j),kdt=itimestep,lat=j                          &
              ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde               &
              ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme               &
              ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte   )
   enddo
!
   end subroutine gwdo
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine gwdo2d(dudt,dvdt,dtaux2d,dtauy2d,                                &
                    u1,v1,t1,q1,                                               &
                    del,                                                       &
                    prsi,prsl,prslk,zl,rcl,kpblmax,                            &
                    var,oc1,oa4,ol4,dusfc,dvsfc,                               &
                    g,cp,rd,rv,fv,pi,dxmeter,deltim,kpbl,kdt,lat,              &
                    ids,ide, jds,jde, kds,kde,                                 &
                    ims,ime, jms,jme, kms,kme,                                 &
                    its,ite, jts,jte, kts,kte)
!-------------------------------------------------------------------------------
!  
!  this code handles the time tendencies of u v due to the effect of  mountain 
!  induced gravity wave drag from sub-grid scale orography. this routine 
!  not only treats the traditional upper-level wave breaking due to mountain 
!  variance (alpert 1988), but also the enhanced lower-tropospheric wave 
!  breaking due to mountain convexity and asymmetry (kim and arakawa 1995). 
!  thus, in addition to the terrain height data in a model grid gox, 
!  additional 10-2d topographic statistics files are needed, including 
!  orographic standard  deviation (var), convexity (oc1), asymmetry (oa4) 
!  and ol (ol4). these data sets are prepared based on the 30 sec usgs orography
!  hong (1999). the current scheme was implmented as in hong et al.(2008)
!
!  coded by song-you hong and young-joon kim and implemented by song-you hong
!
!  program history log:
!    2014-10-01  Hyun-Joo Choi (from KIAPS)  flow-blocking drag of kim and doyle
!                              with blocked height by dividing streamline theory
!
!  references:
!        hong et al. (2008), wea. and forecasting
!        kim and doyle (2005), Q. J. R. Meteor. Soc.
!        kim and arakawa (1995), j. atmos. sci.
!        alpet et al. (1988), NWP conference.
!        hong (1999), NCEP office note 424.
!
!  notice : comparible or lower resolution orography files than model resolution
!           are desirable in preprocess (wps) to prevent weakening of the drag
!-------------------------------------------------------------------------------
!
!  input                                                                
!        dudt (ims:ime,kms:kme)  non-lin tendency for u wind component
!        dvdt (ims:ime,kms:kme)  non-lin tendency for v wind component
!        u1(ims:ime,kms:kme) zonal wind / sqrt(rcl)  m/sec  at t0-dt
!        v1(ims:ime,kms:kme) meridional wind / sqrt(rcl) m/sec at t0-dt
!        t1(ims:ime,kms:kme) temperature deg k at t0-dt
!        q1(ims:ime,kms:kme) specific humidity at t0-dt
!
!        rcl     a scaling factor = reciprocal of square of cos(lat)
!                for gmp.  rcl=1 if u1 and v1 are wind components.
!        deltim  time step    secs                                       
!        del(kts:kte)  positive increment of pressure across layer (pa)
!                                                                       
!  output
!        dudt, dvdt    wind tendency due to gwdo
!
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
   integer              ::  kdt,lat,latd,lond,kpblmax,                         &
                            ids,ide, jds,jde, kds,kde,                         &
                            ims,ime, jms,jme, kms,kme,                         &
                            its,ite, jts,jte, kts,kte
!
   real                 ::  g,rd,rv,fv,cp,pi,dxmeter,deltim,rcl
   real                 ::  dudt(ims:ime,kms:kme),dvdt(ims:ime,kms:kme),       &
                            dtaux2d(ims:ime,kms:kme),dtauy2d(ims:ime,kms:kme), &
                            u1(ims:ime,kms:kme),v1(ims:ime,kms:kme),           & 
                            t1(ims:ime,kms:kme),q1(ims:ime,kms:kme),           &
                            zl(ims:ime,kms:kme),prsl(its:ite,kts:kte),         &
                            prslk(ims:ime,kms:kme)
   real                 ::  prsi(its:ite,kts:kte+1),del(its:ite,kts:kte)
   real                 ::  oa4(its:ite,4),ol4(its:ite,4)
!
   integer              ::  kpbl(ims:ime)
   real                 ::  var(ims:ime),oc1(ims:ime),                         &
                            dusfc(ims:ime),dvsfc(ims:ime)
!
! critical richardson number for wave breaking : ! larger drag with larger value
!
   real,parameter       ::  ric     = 0.25  
!
   real,parameter       ::  dw2min  = 1.
   real,parameter       ::  rimin   = -100.
   real,parameter       ::  bnv2min = 1.0e-5
   real,parameter       ::  efmin   = 0.0
   real,parameter       ::  efmax   = 10.0
   real,parameter       ::  xl      = 4.0e4  
   real,parameter       ::  critac  = 1.0e-5
   real,parameter       ::  gmax    = 1.    
   real,parameter       ::  veleps  = 1.0                                                 
   real,parameter       ::  factop  = 0.5                                                  
   real,parameter       ::  frc     = 1.0      
   real,parameter       ::  ce      = 0.8     
   real,parameter       ::  cg      = 0.5    
   integer,parameter    ::  kpblmin = 2
!
!  local variables
!
   integer              ::  i,k,lcap,lcapp1,nwd,idir,                          &
                            klcap,kp1,ikount,kk
!
   real                 ::  rcs,rclcs,csg,fdir,cleff,cs,rcsks,                 &
                            wdir,ti,rdz,temp,tem2,dw2,shr2,bvf2,rdelks,        &
                            wtkbj,tem,gfobnv,hd,fro,rim,temc,tem1,efact,       &
                            temv,dtaux,dtauy
!
   logical              ::  ldrag(its:ite),icrilv(its:ite),                    &
                            flag(its:ite),kloop1(its:ite)
!                                                                       
   real                 ::  taub(its:ite),taup(its:ite,kts:kte+1),             &
                            xn(its:ite),yn(its:ite),                           &
                            ubar(its:ite),vbar(its:ite),                       &
                            fr(its:ite),ulow(its:ite),                         &
                            rulow(its:ite),bnv(its:ite),                       &
                            oa(its:ite),ol(its:ite),                           &
                            roll(its:ite),dtfac(its:ite),                      &
                            brvf(its:ite),xlinv(its:ite),                      &
                            delks(its:ite),delks1(its:ite),                    &
                            bnv2(its:ite,kts:kte),usqj(its:ite,kts:kte),       &
                            taud(its:ite,kts:kte),ro(its:ite,kts:kte),         &
                            vtk(its:ite,kts:kte),vtj(its:ite,kts:kte),         &
                            zlowtop(its:ite),velco(its:ite,kts:kte-1),         &
                            coefm(its:ite)
!
   integer              ::  kbl(its:ite),klowtop(its:ite)
!
   logical :: iope
   integer,parameter    ::  mdir=8
   integer              ::  nwdir(mdir)
   data nwdir/6,7,5,8,2,3,1,4/
!
!  variables for flow-blocking drag
!
   real,parameter       :: frmax  = 10.
   real,parameter       :: olmin  = 1.0e-5
   real,parameter       :: odmin  = 0.1 
   real,parameter       :: odmax  = 10. 
   real,parameter       :: erad   = 6371.315e+3
   integer              :: komax(its:ite)
   integer              :: kblk
   real                 :: cd
   real                 :: zblk,tautem
   real                 :: pe,ke 
   real                 :: delx,dely,dxy4(4),dxy4p(4)
   real                 :: dxy(its:ite),dxyp(its:ite)
   real                 :: ol4p(4),olp(its:ite),od(its:ite)
   real                 :: taufb(its:ite,kts:kte+1)
!
!---- constants                                                         
!                                                                       
   rcs    = sqrt(rcl)                                                   
   cs     = 1. / sqrt(rcl)                                                     
   csg    = cs * g                                                      
   lcap   = kte                                                         
   lcapp1 = lcap + 1                                                 
   fdir   = mdir / (2.0*pi)
!
!--- calculate length of grid for flow-blocking drag
!
   delx   = dxmeter 
   dely   = dxmeter
   dxy4(1)  = delx
   dxy4(2)  = dely
   dxy4(3)  = sqrt(delx*delx + dely*dely)
   dxy4(4)  = dxy4(3)
   dxy4p(1) = dxy4(2)
   dxy4p(2) = dxy4(1)
   dxy4p(3) = dxy4(4)
   dxy4p(4) = dxy4(3)
!
!
!-----initialize arrays                                                 
!                                                                       
   dtaux = 0.0
   dtauy = 0.0
   do i = its,ite                                                       
     klowtop(i)    = 0
     kbl(i)        = 0
   enddo                                                             
!
   do i = its,ite                                                       
     xn(i)         = 0.0
     yn(i)         = 0.0
     ubar (i)      = 0.0
     vbar (i)      = 0.0
     roll (i)      = 0.0
     taub (i)      = 0.0
     taup(i,1)     = 0.0
     oa(i)         = 0.0
     ol(i)         = 0.0
     ulow (i)      = 0.0
     dtfac(i)      = 1.0
     ldrag(i)      = .false.
     icrilv(i)     = .false. 
     flag(i)       = .true.
   enddo                                                             
!
   do k = kts,kte
     do i = its,ite
       usqj(i,k) = 0.0
       bnv2(i,k) = 0.0
       vtj(i,k)  = 0.0
       vtk(i,k)  = 0.0
       taup(i,k) = 0.0
       taud(i,k) = 0.0
       dtaux2d(i,k)= 0.0
       dtauy2d(i,k)= 0.0
     enddo
   enddo
!
   do i = its,ite
     taup(i,kte+1) = 0.0
     xlinv(i)     = 1.0/xl                                                   
   enddo
!
!  initialize array for flow-blocking drag
!
   taufb(its:ite,kts:kte+1) = 0.0
   komax(its:ite) = 0
!
   do k = kts,kte
     do i = its,ite
       vtj(i,k)  = t1(i,k)  * (1.+fv*q1(i,k))
       vtk(i,k)  = vtj(i,k) / prslk(i,k)
       ro(i,k)   = 1./rd * prsl(i,k) / vtj(i,k) ! density kg/m**3
     enddo
   enddo
!
!  determine reference level: maximum of 2*var and pbl heights
!
   do i = its,ite
     zlowtop(i) = 2. * var(i)
   enddo
!
   do i = its,ite
     kloop1(i) = .true.
   enddo
!
   do k = kts+1,kte
     do i = its,ite
       if(kloop1(i).and.zl(i,k)-zl(i,1).ge.zlowtop(i)) then
         klowtop(i) = k+1
         kloop1(i)  = .false.
       endif
     enddo
   enddo
!
   do i = its,ite
     kbl(i)   = max(kpbl(i), klowtop(i))
     kbl(i)   = max(min(kbl(i),kpblmax),kpblmin)
   enddo
!
!  determine the level of maximum orographic height
!
   komax(:) = kbl(:)
!
   do i = its,ite
     delks(i)  = 1.0 / (prsi(i,1) - prsi(i,kbl(i)))
     delks1(i) = 1.0 / (prsl(i,1) - prsl(i,kbl(i)))
   enddo
!
!  compute low level averages within pbl
!
   do k = kts,kpblmax
     do i = its,ite
       if (k.lt.kbl(i)) then
         rcsks   = rcs     * del(i,k) * delks(i)
         rdelks  = del(i,k)  * delks(i)
         ubar(i) = ubar(i) + rcsks  * u1(i,k)      ! pbl u  mean
         vbar(i) = vbar(i) + rcsks  * v1(i,k)      ! pbl v  mean
         roll(i) = roll(i) + rdelks * ro(i,k)      ! ro mean
       endif
     enddo
   enddo
!
!     figure out low-level horizontal wind direction 
!
!             nwd  1   2   3   4   5   6   7   8
!              wd  w   s  sw  nw   e   n  ne  se
!
   do i = its,ite                                                       
     wdir   = atan2(ubar(i),vbar(i)) + pi
     idir   = mod(nint(fdir*wdir),mdir) + 1
     nwd    = nwdir(idir)
     oa(i)  = (1-2*int( (nwd-1)/4 )) * oa4(i,mod(nwd-1,4)+1)
     ol(i) = ol4(i,mod(nwd-1,4)+1) 
!
!----- compute orographic width along (ol) and perpendicular (olp)
!----- the direction of wind
!
     ol4p(1) = ol4(i,2)
     ol4p(2) = ol4(i,1)
     ol4p(3) = ol4(i,4)
     ol4p(4) = ol4(i,3)
     olp(i)  = ol4p(mod(nwd-1,4)+1) 
!
!----- compute orographic direction (horizontal orographic aspect ratio)
!
     od(i) = olp(i)/max(ol(i),olmin)
     od(i) = min(od(i),odmax)
     od(i) = max(od(i),odmin)
!
!----- compute length of grid in the along(dxy) and cross(dxyp) wind directions
!
     dxy(i)  = dxy4(MOD(nwd-1,4)+1)
     dxyp(i) = dxy4p(MOD(nwd-1,4)+1)
   enddo
!                                                                       
!---  saving richardson number in usqj for migwdi                       
!
   do k = kts,kte-1                                                     
     do i = its,ite                                                     
       ti        = 2.0 / (t1(i,k)+t1(i,k+1))                                
       rdz       = 1./(zl(i,k+1) - zl(i,k))
       tem1      = u1(i,k) - u1(i,k+1)
       tem2      = v1(i,k) - v1(i,k+1)   
       dw2       = rcl*(tem1*tem1 + tem2*tem2)
       shr2      = max(dw2,dw2min) * rdz * rdz
       bvf2      = g*(g/cp+rdz*(vtj(i,k+1)-vtj(i,k))) * ti                
       usqj(i,k) = max(bvf2/shr2,rimin)                            
       bnv2(i,k) = 2.0*g*rdz*(vtk(i,k+1)-vtk(i,k))/(vtk(i,k+1)+vtk(i,k))
       bnv2(i,k) = max( bnv2(i,k), bnv2min )
     enddo                                                          
   enddo                                                             
!
!----compute the "low level" or 1/3 wind magnitude (m/s)                
!                                                                       
   do i = its,ite                                                       
     ulow(i) = max(sqrt(ubar(i)*ubar(i) + vbar(i)*vbar(i)), 1.0)
     rulow(i) = 1./ulow(i)
   enddo                                                             
!
   do k = kts,kte-1                                                    
     do i = its,ite                                                   
       velco(i,k)  = (0.5*rcs) * ((u1(i,k)+u1(i,k+1)) * ubar(i)                &
                                + (v1(i,k)+v1(i,k+1)) * vbar(i))                 
       velco(i,k)  = velco(i,k) * rulow(i)                               
       if ((velco(i,k).lt.veleps) .and. (velco(i,k).gt.0.)) then
         velco(i,k) = veleps                                      
       endif
     enddo                                                          
   enddo                                                             
!                                                                       
!  no drag when critical level in the base layer                        
!                                                                       
   do i = its,ite                                                       
     ldrag(i) = velco(i,1).le.0.                                    
   enddo                                                             
!
!  no drag when velco.lt.0                                               
!                                                                       
   do k = kpblmin,kpblmax
     do i = its,ite                                                    
       if (k .lt. kbl(i)) ldrag(i) = ldrag(i).or. velco(i,k).le.0.
     enddo                                                          
   enddo                                                             
!                                                                       
!  no drag when bnv2.lt.0                                               
!                                                                       
   do k = kts,kpblmax
     do i = its,ite                                                    
       if (k .lt. kbl(i)) ldrag(i) = ldrag(i).or. bnv2(i,k).lt.0.
     enddo                                                          
   enddo                                                             
!                                                                       
!-----the low level weighted average ri is stored in usqj(1,1; im)      
!-----the low level weighted average n**2 is stored in bnv2(1,1; im)    
!---- this is called bnvl2 in phys_gwd_alpert_sub not bnv2                           
!---- rdelks (del(k)/delks) vert ave factor so we can * instead of /    
!                                                                       
   do i = its,ite                                                       
     wtkbj     = (prsl(i,1)-prsl(i,2)) * delks1(i)
     bnv2(i,1) = wtkbj * bnv2(i,1)                                
     usqj(i,1) = wtkbj * usqj(i,1)                                
   enddo                                                             
!
   do k = kpblmin,kpblmax                                                
     do i = its,ite                                                    
       if (k .lt. kbl(i)) then
         rdelks    = (prsl(i,k)-prsl(i,k+1)) * delks1(i)
         bnv2(i,1) = bnv2(i,1) + bnv2(i,k) * rdelks
         usqj(i,1) = usqj(i,1) + usqj(i,k) * rdelks
       endif
     enddo                                                          
   enddo                                                             
!                                                                       
   do i = its,ite                                                       
     ldrag(i) = ldrag(i) .or. bnv2(i,1).le.0.0                         
     ldrag(i) = ldrag(i) .or. ulow(i).eq.1.0                           
     ldrag(i) = ldrag(i) .or. var(i) .le. 0.0
   enddo                                                             
!                                                                       
!  set all ri low level values to the low level value          
!                                                                       
   do k = kpblmin,kpblmax
     do i = its,ite                                                    
       if (k .lt. kbl(i)) usqj(i,k) = usqj(i,1)
     enddo                                                          
   enddo                                                             
!
   do i = its,ite 
     if (.not.ldrag(i))   then   
       bnv(i) = sqrt( bnv2(i,1) )                                  
       fr(i) = bnv(i)  * rulow(i) * 2. * var(i) * od(i)
       fr(i) = min(fr(i),frmax)
       xn(i)  = ubar(i) * rulow(i)
       yn(i)  = vbar(i) * rulow(i)
     endif
   enddo
!
!  compute the base level stress and store it in taub
!  calculate enhancement factor, number of mountains & aspect        
!  ratio const. use simplified relationship between standard            
!  deviation & critical hgt                                          
!
   do i = its,ite                                                       
     if (.not. ldrag(i))   then   
       efact    = (oa(i) + 2.) ** (ce*fr(i)/frc)                         
       efact    = min( max(efact,efmin), efmax )                            
!!!!!!! cleff (effective grid length) is highly tunable parameter
!!!!!!! the bigger (smaller) value produce weaker (stronger) wave drag
       cleff    = sqrt(dxy(i)**2. + dxyp(i)**2.)
       cleff    = 3. * max(dxmeter,cleff)
       coefm(i) = (1. + ol(i)) ** (oa(i)+1.)                   
       xlinv(i) = coefm(i) / cleff                                             
       tem      = fr(i) * fr(i) * oc1(i)
       gfobnv   = gmax * tem / ((tem + cg)*bnv(i))   
       taub(i)  = xlinv(i) * roll(i) * ulow(i) * ulow(i)                       &
                * ulow(i) * gfobnv * efact          
     else                                                          
       taub(i) = 0.0                                             
       xn(i)   = 0.0                                             
       yn(i)   = 0.0                                             
     endif                                                         
   enddo                                                             
!                                                                       
!   now compute vertical structure of the stress.
!
   do k = kts,kpblmax
     do i = its,ite
       if (k .le. kbl(i)) taup(i,k) = taub(i)
     enddo
   enddo
!
   do k = kpblmin, kte-1                   ! vertical level k loop!
     kp1 = k + 1
     do i = its,ite
!
!   unstablelayer if ri < ric
!   unstable layer if upper air vel comp along surf vel <=0 (crit lay)
!   at (u-c)=0. crit layer exists and bit vector should be set (.le.)
!
       if (k .ge. kbl(i)) then
         icrilv(i) = icrilv(i) .or. ( usqj(i,k) .lt. ric)                      &
                               .or. (velco(i,k) .le. 0.0)
         brvf(i)  = max(bnv2(i,k),bnv2min) ! brunt-vaisala frequency squared
         brvf(i)  = sqrt(brvf(i))          ! brunt-vaisala frequency
       endif
     enddo
!
     do i = its,ite
       if (k .ge. kbl(i) .and. (.not. ldrag(i)))   then   
         if (.not.icrilv(i) .and. taup(i,k) .gt. 0.0 ) then
           temv = 1.0 / velco(i,k)
           tem1 = coefm(i)/dxy(i)*(ro(i,kp1)+ro(i,k))*brvf(i)*velco(i,k)*0.5
           hd   = sqrt(taup(i,k) / tem1)
           fro  = brvf(i) * hd * temv
!
!  rim is the  minimum-richardson number by shutts (1985)
!
           tem2   = sqrt(usqj(i,k))
           tem    = 1. + tem2 * fro
           rim    = usqj(i,k) * (1.-fro) / (tem * tem)
!
!  check stability to employ the 'saturation hypothesis'
!  of lindzen (1981) except at tropospheric downstream regions
!
           if (rim .le. ric) then  ! saturation hypothesis!
             if ((oa(i) .le. 0.).or.(kp1 .ge. kpblmin )) then
               temc = 2.0 + 1.0 / tem2
               hd   = velco(i,k) * (2.*sqrt(temc)-temc) / brvf(i)
               taup(i,kp1) = tem1 * hd * hd
             endif
           else                    ! no wavebreaking!
             taup(i,kp1) = taup(i,k)
           endif
         endif
       endif
     enddo      
   enddo
!
   if(lcap.lt.kte) then                                               
     do klcap = lcapp1,kte                                          
       do i = its,ite                                                 
         taup(i,klcap) = prsi(i,klcap) / prsi(i,lcap) * taup(i,lcap)      
       enddo                                                       
     enddo                                                          
   endif                                                             
   do i = its,ite
     if(.not.ldrag(i)) then
!
!------- determine the height of flow-blocking layer
!
        kblk = 0
        pe = 0.0
        do k = kte, kpblmin, -1
          if(kblk.eq.0 .and. k.le.komax(i)) then
            pe = pe + bnv2(i,k)*(zl(i,komax(i))-zl(i,k))*del(i,k)/g/ro(i,k)
            ke = 0.5*((rcs*u1(i,k))**2.+(rcs*v1(i,k))**2.)
!
!---------- apply flow-blocking drag when pe >= ke 
!
            if(pe.ge.ke) then
              kblk = k
              kblk = min(kblk,kbl(i))
              zblk = zl(i,kblk)-zl(i,kts)
            endif
          endif
        enddo
        if(kblk.ne.0) then
!
!--------- compute flow-blocking stress
!
          cd = max(2.0-1.0/od(i),0.0)
          taufb(i,kts) = 0.5 * roll(i) * coefm(i) / dxy(i)**2 * cd * dxyp(i)   &
                         * olp(i) * zblk * ulow(i)**2
          tautem = taufb(i,kts)/float(kblk-kts)
          do k = kts+1, kblk
            taufb(i,k) = taufb(i,k-1) - tautem
          enddo
!
!----------sum orographic GW stress and flow-blocking stress
!
          taup(i,:) = taup(i,:) + taufb(i,:)
        endif
     endif
   enddo 
!                                                                       
!  calculate - (g)*d(tau)/d(pressure) and deceleration terms dtaux, dtauy
!
   do k = kts,kte                                                       
     do i = its,ite                                                       
       taud(i,k) = 1. * (taup(i,k+1) - taup(i,k)) * csg / del(i,k)
     enddo                                                             
   enddo                                                             
!                                                                       
!  limit de-acceleration (momentum deposition ) at top to 1/2 value 
!  the idea is some stuff must go out the 'top'                     
!                                                                       
   do klcap = lcap,kte                                               
     do i = its,ite                                                    
       taud(i,klcap) = taud(i,klcap) * factop
     enddo                                                          
   enddo                                                             
!                                                                       
!  if the gravity wave drag would force a critical line             
!  in the lower ksmm1 layers during the next deltim timestep,     
!  then only apply drag until that critical line is reached.        
!                                                                       
   do k = kts,kpblmax-1                                                    
     do i = its,ite                                                    
       if (k .le. kbl(i)) then
         if(taud(i,k).ne.0.)                                                   &
         dtfac(i) = min(dtfac(i),abs(velco(i,k)                                &
                   /(deltim*rcs*taud(i,k))))
       endif
     enddo                                                          
   enddo                                                             
!
   do i = its,ite
     dusfc(i) = 0.
     dvsfc(i) = 0.
   enddo
!
   do k = kts,kte                                                       
     do i = its,ite 
       taud(i,k)  = taud(i,k) * dtfac(i)                              
       dtaux = taud(i,k) * xn(i)
       dtauy = taud(i,k) * yn(i)
       dtaux2d(i,k) = dtaux
       dtauy2d(i,k) = dtauy
       dudt(i,k)  = dtaux + dudt(i,k)
       dvdt(i,k)  = dtauy + dvdt(i,k)
       dusfc(i)   = dusfc(i) + dtaux * del(i,k)
       dvsfc(i)   = dvsfc(i) + dtauy * del(i,k)
     enddo                                                          
   enddo                                                             
!
   do i = its,ite
     dusfc(i) = (-1./g*rcs) * dusfc(i)
     dvsfc(i) = (-1./g*rcs) * dvsfc(i)
   enddo
!
   return                                                            
   end subroutine gwdo2d
!-------------------------------------------------------------------
end module module_bl_gwdo
