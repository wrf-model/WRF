!=================================================================================================================
 module bl_gwdo
 use ccpp_kind_types,only: kind_phys

 implicit none
 private
 public:: bl_gwdo_run,     &
          bl_gwdo_init,    &
          bl_gwdo_finalize


 contains


!=================================================================================================================
!>\section arg_table_bl_gwdo_init
!!\html\include bl_gwdo_init.html
!!
 subroutine bl_gwdo_init(errmsg,errflg)
!=================================================================================================================

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------

 errmsg = 'bl_gwdo_init OK'
 errflg = 0

 end subroutine bl_gwdo_init

!=================================================================================================================
!>\section arg_table_bl_gwdo_finalize
!!\html\include bl_gwdo_finalize.html
!!
 subroutine bl_gwdo_finalize(errmsg,errflg)
!=================================================================================================================

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------

 errmsg = 'bl_gwdo_finalize OK'
 errflg = 0

 end subroutine bl_gwdo_finalize

!=================================================================================================================
!>\section arg_table_bl_gwdo_run
!!\html\include bl_gwdo_run.html
!!
   subroutine bl_gwdo_run(sina, cosa,                                          &
                     rublten,rvblten,                                          &
                     dtaux3d,dtauy3d,                                          &
                     dusfcg,dvsfcg,                                            &
                     uproj, vproj,                                             &
                     t1, q1,                                                   &
                     prsi, prsl, prslk, zl,                                    &
                     var, oc1,                                                 &
                     oa2d1, oa2d2,                                             &
                     oa2d3, oa2d4,                                             &
                     ol2d1, ol2d2,                                             &
                     ol2d3, ol2d4,                                             &
                     g_, cp_, rd_, rv_, fv_, pi_,                              &
                     dxmeter, deltim,                                          &
                     its, ite, kte, kme,                                       &
                     errmsg, errflg                                            )
!-------------------------------------------------------------------------------
!  
!  abstract : 
!    this code handles the time tendencies of u v due to the effect of 
!    mountain induced gravity wave drag from sub-grid scale orography. 
!    this routine not only treats the traditional upper-level wave breaking due 
!    to mountain variance (alpert 1988), but also the enhanced 
!    lower-tropospheric wave breaking due to mountain convexity and asymmetry
!    (kim and arakawa 1995). thus, in addition to the terrain height data 
!    in a model grid gox, additional 10-2d topographic statistics files are 
!    needed, including orographic standard  deviation (var), convexity (oc1), 
!    asymmetry (oa4) and ol (ol4). these data sets are prepared based on the 
!    30 sec usgs orography (hong 1999). the current scheme was implmented as in 
!    choi and hong (2015), which names kim gwdo since it was developed by 
!    kiaps staffs for kiaps integrated model system (kim). the scheme 
!    additionally includes the effects of orographic anisotropy and 
!    flow-blocking drag. 
!    coded by song-you hong and young-joon kim and implemented by song-you hong
!
!  history log :
!    2015-07-01  hyun-joo choi add flow-blocking drag and orographic anisotropy 
!
!  references :
!    choi and hong (2015), j. geophys. res.
!    hong et al. (2008), wea. forecasting
!    kim and doyle (2005), q. j. r. meteor. soc.
!    kim and arakawa (1995), j. atmos. sci.
!    alpet et al. (1988), NWP conference
!    hong (1999), NCEP office note 424
!
!  input :                                                                
!    dudt, dvdt       - non-lin tendency for u and v wind component
!    uproj, vproj     - projection-relative U and V m/sec
!    u1, v1           - zonal and meridional wind m/sec  at t0-dt
!    t1               - temperature deg k at t0-dt
!    q1               - mixing ratio at t0-dt
!    deltim           - time step (s)                                      
!    del              - positive increment of pressure across layer (pa)
!    prslk, zl, prsl, prsi    - pressure and height variables
!    oa4, ol4, omax, var, oc1 - orographic statistics 
!                                                                       
!  output :
!    dudt, dvdt - wind tendency due to gwdo
!    dtaux2d, dtauy2d - diagnoised orographic gwd
!    dusfc, dvsfc     - gw stress
!
!-------------------------------------------------------------------------------
   implicit none
!
   integer, parameter                                                :: kts = 1
   integer                                           , intent(in   ) :: its, ite, kte, kme
   real(kind=kind_phys)                              , intent(in   ) :: g_, pi_, rd_, rv_, fv_,&
                                                                        cp_, deltim
   real(kind=kind_phys), dimension(its:)             , intent(in   ) :: dxmeter
   real(kind=kind_phys), dimension(its:,:)           , intent(inout) :: rublten, rvblten
   real(kind=kind_phys), dimension(its:,:)           , intent(  out) :: dtaux3d, dtauy3d
   real(kind=kind_phys), dimension(its:)             , intent(  out) :: dusfcg, dvsfcg
   real(kind=kind_phys), dimension(its:)             , intent(in   ) :: sina, cosa
   real(kind=kind_phys), dimension(its:,:)           , intent(in   ) :: uproj, vproj
   real(kind=kind_phys), dimension(its:,:)           , intent(in   ) :: t1, q1, prslk, zl
!
   real(kind=kind_phys), dimension(its:,:)           , intent(in   ) :: prsl
   real(kind=kind_phys), dimension(its:,:)           , intent(in   ) :: prsi
!
   real(kind=kind_phys), dimension(its:)             , intent(in   ) :: var, oc1, &
                                                                        oa2d1, oa2d2, oa2d3, oa2d4, &
                                                                        ol2d1, ol2d2, ol2d3, ol2d4
   character(len=*)                                  , intent(  out) :: errmsg
   integer                                           , intent(  out) :: errflg
!
   real(kind=kind_phys), parameter      ::  ric     = 0.25    ! critical richardson number 
   real(kind=kind_phys), parameter      ::  dw2min  = 1.
   real(kind=kind_phys), parameter      ::  rimin   = -100.
   real(kind=kind_phys), parameter      ::  bnv2min = 1.0e-5
   real(kind=kind_phys), parameter      ::  efmin   = 0.0
   real(kind=kind_phys), parameter      ::  efmax   = 10.0
   real(kind=kind_phys), parameter      ::  xl      = 4.0e4  
   real(kind=kind_phys), parameter      ::  critac  = 1.0e-5
   real(kind=kind_phys), parameter      ::  gmax    = 1.    
   real(kind=kind_phys), parameter      ::  veleps  = 1.0                                                 
   real(kind=kind_phys), parameter      ::  frc     = 1.0      
   real(kind=kind_phys), parameter      ::  ce      = 0.8     
   real(kind=kind_phys), parameter      ::  cg      = 0.5    
   integer,parameter                    ::  kpblmin = 2
!
! local variables
!
   integer                              ::  kpblmax
   integer                              ::  latd,lond
   integer                              ::  i,k,lcap,lcapp1,nwd,idir,                          &
                                            klcap,kp1,ikount,kk
!
   real(kind=kind_phys)                 ::  fdir,cs,rcsks,                                     &
                                            wdir,ti,rdz,temp,tem2,dw2,shr2,bvf2,rdelks,        &
                                            wtkbj,tem,gfobnv,hd,fro,rim,temc,tem1,efact,       &
                                            temv,dtaux,dtauy
!
   real(kind=kind_phys), dimension(its:ite,kts:kte)   :: dudt, dvdt        
   real(kind=kind_phys), dimension(its:ite,kts:kte)   :: dtaux2d, dtauy2d  
   real(kind=kind_phys), dimension(its:ite)           :: dusfc, dvsfc
   logical, dimension(its:ite)                        :: ldrag, icrilv, flag,kloop1
   real(kind=kind_phys), dimension(its:ite)           :: coefm
!                                                                       
   real(kind=kind_phys), dimension(its:ite)           :: taub, xn, yn, ubar, vbar, fr,         &
                                                         ulow, rulow, bnv, oa, ol, rhobar,     &
                                                         dtfac, brvf, xlinv, delks,delks1,     &
                                                         zlowtop,cleff
   real(kind=kind_phys), dimension(its:ite,kts:kte+1) :: taup
   real(kind=kind_phys), dimension(its:ite,kts:kte-1) :: velco
   real(kind=kind_phys), dimension(its:ite,kts:kte)   :: bnv2, usqj, taud, rho, vtk, vtj
   real(kind=kind_phys), dimension(its:ite,kts:kte)   :: del 
   real(kind=kind_phys), dimension(its:ite,kts:kte)   :: u1, v1
   real(kind=kind_phys), dimension(its:ite,4)         :: oa4, ol4
!
   integer, dimension(its:ite)                        :: kbl, klowtop
   integer, parameter                                 :: mdir=8
   integer, dimension(mdir)                           :: nwdir
   data nwdir/6,7,5,8,2,3,1,4/
!
! variables for flow-blocking drag
!
   real(kind=kind_phys), parameter                    :: frmax  = 10.
   real(kind=kind_phys), parameter                    :: olmin  = 1.0e-5
   real(kind=kind_phys), parameter                    :: odmin  = 0.1 
   real(kind=kind_phys), parameter                    :: odmax  = 10. 
!
   real(kind=kind_phys)                               :: fbdcd
   real(kind=kind_phys)                               :: zblk, tautem
   real(kind=kind_phys)                               :: fbdpe, fbdke 
   real(kind=kind_phys), dimension(its:ite)           :: delx, dely
   real(kind=kind_phys), dimension(its:ite,4)         :: dxy4, dxy4p
   real(kind=kind_phys), dimension(4)                 :: ol4p
   real(kind=kind_phys), dimension(its:ite)           :: dxy, dxyp, olp, od
   real(kind=kind_phys), dimension(its:ite,kts:kte+1) :: taufb
!
   integer, dimension(its:ite)                        :: komax
   integer                                            :: kblk
!-------------------------------------------------------------------------------
!
! constants                                                         
!                                                                       
   lcap   = kte                                                         
   lcapp1 = lcap + 1                                                 
   fdir   = mdir / (2.0*pi_)
!
! initialize CCPP error flag and message
!
    errmsg = ''
    errflg = 0
!
! calculate length of grid for flow-blocking drag
!
   delx(its:ite) = dxmeter(its:ite)
   dely(its:ite) = dxmeter(its:ite)
   dxy4(its:ite,1)  = delx(its:ite)
   dxy4(its:ite,2)  = dely(its:ite)
   dxy4(its:ite,3)  = sqrt(delx(its:ite)**2. + dely(its:ite)**2.)
   dxy4(its:ite,4)  = dxy4(its:ite,3)
   dxy4p(its:ite,1) = dxy4(its:ite,2)
   dxy4p(its:ite,2) = dxy4(its:ite,1)
   dxy4p(its:ite,3) = dxy4(its:ite,4)
   dxy4p(its:ite,4) = dxy4(its:ite,3)
!
   cleff(its:ite) = dxmeter(its:ite)
!
! initialize arrays, array syntax is OK for OpenMP since these are local
!                                                                       
   ldrag   = .false. ; icrilv = .false. ; flag    = .true.
!
   klowtop = 0       ; kbl    = 0
!
   dtaux   = 0.      ; dtauy  = 0.      ; xn      = 0.     ; yn      = 0.  
   ubar    = 0.      ; vbar   = 0.      ; rhobar  = 0.     ; ulow    = 0.
   oa      = 0.      ; ol     = 0.      ; taub    = 0.  
!
   usqj    = 0.      ; bnv2   = 0.      ; vtj     = 0.     ; vtk     = 0. 
   taup    = 0.      ; taud   = 0.      ; dtaux2d = 0.     ; dtauy2d = 0.
!
   dtfac   = 1.0     ; xlinv  = 1.0/xl
!
   komax = 0
   taufb = 0.0
!
   do k = kts,kte
     do i = its,ite
       vtj(i,k) = t1(i,k)  * (1.+fv_*q1(i,k))
       vtk(i,k) = vtj(i,k) / prslk(i,k)

       !  Density (kg/m^3)

       rho(i,k) = 1./rd_ * prsl(i,k) / vtj(i,k)

       !  Delta p (positive) between interfaces levels (Pa)

       del(i,k) = prsi(i,k) - prsi(i,k+1)
       
       !  Earth-relative zonal and meridional winds (m/s)

       u1(i,k) = uproj(i,k)*cosa(i) - vproj(i,k)*sina(i)
       v1(i,k) = uproj(i,k)*sina(i) + vproj(i,k)*cosa(i)

     enddo
   enddo

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
       if(zlowtop(i) .gt. 0.) then
       if (kloop1(i).and.zl(i,k)-zl(i,1).ge.zlowtop(i)) then
         klowtop(i) = k+1
         kloop1(i) = .false.
       endif
       endif
     enddo
   enddo
!
   kpblmax = kte
   do i = its,ite
     kbl(i) = klowtop(i)
     kbl(i) = max(min(kbl(i),kpblmax),kpblmin)
   enddo
!
! determine the level of maximum orographic height
!
   komax(:) = kbl(:)
!
   do i = its,ite
     delks(i)  = 1.0 / (prsi(i,1) - prsi(i,kbl(i)))
     delks1(i) = 1.0 / (prsl(i,1) - prsl(i,kbl(i)))
   enddo
!
! compute low level averages within pbl
!
   do k = kts,kpblmax
     do i = its,ite
       if (k.lt.kbl(i)) then
         rcsks     = del(i,k) * delks(i)
         rdelks    = del(i,k)  * delks(i)
         ubar(i)   = ubar(i) + rcsks  * u1(i,k)      ! pbl  u  mean
         vbar(i)   = vbar(i) + rcsks  * v1(i,k)      ! pbl  v  mean
         rhobar(i) = rhobar(i) + rdelks * rho(i,k)   ! pbl rho mean
       endif
     enddo
   enddo
!
! figure out low-level horizontal wind direction 
!
! nwd  1   2   3   4   5   6   7   8
! wd   w   s  sw  nw   e   n  ne  se
!
   do i = its,ite                      
     oa4(i,1) = oa2d1(i)
     oa4(i,2) = oa2d2(i)
     oa4(i,3) = oa2d3(i)
     oa4(i,4) = oa2d4(i)
     ol4(i,1) = ol2d1(i)
     ol4(i,2) = ol2d2(i)
     ol4(i,3) = ol2d3(i)
     ol4(i,4) = ol2d4(i)
     wdir  = atan2(ubar(i),vbar(i)) + pi_
     idir  = mod(nint(fdir*wdir),mdir) + 1
     nwd   = nwdir(idir)
     oa(i) = (1-2*int( (nwd-1)/4 )) * oa4(i,mod(nwd-1,4)+1)
     ol(i) = ol4(i,mod(nwd-1,4)+1) 
!
! compute orographic width along (ol) and perpendicular (olp) the wind direction
!
     ol4p(1) = ol4(i,2)
     ol4p(2) = ol4(i,1)
     ol4p(3) = ol4(i,4)
     ol4p(4) = ol4(i,3)
     olp(i)  = ol4p(mod(nwd-1,4)+1) 
!
! compute orographic direction (horizontal orographic aspect ratio)
!
     od(i) = olp(i)/max(ol(i),olmin)
     od(i) = min(od(i),odmax)
     od(i) = max(od(i),odmin)
!
! compute length of grid in the along(dxy) and cross(dxyp) wind directions
!
     dxy(i)  = dxy4(i,MOD(nwd-1,4)+1)
     dxyp(i) = dxy4p(i,MOD(nwd-1,4)+1)
   enddo
!                                                                       
! saving richardson number in usqj for migwdi                       
!
   do k = kts,kte-1                                                     
     do i = its,ite                                                     
       ti        = 2.0 / (t1(i,k)+t1(i,k+1))                                
       rdz       = 1./(zl(i,k+1) - zl(i,k))
       tem1      = u1(i,k) - u1(i,k+1)
       tem2      = v1(i,k) - v1(i,k+1)   
       dw2       = tem1*tem1 + tem2*tem2
       shr2      = max(dw2,dw2min) * rdz * rdz
       bvf2      = g_*(g_/cp_+rdz*(vtj(i,k+1)-vtj(i,k))) * ti                
       usqj(i,k) = max(bvf2/shr2,rimin)                            
       bnv2(i,k) = 2.0*g_*rdz*(vtk(i,k+1)-vtk(i,k))/(vtk(i,k+1)+vtk(i,k))
     enddo                                                          
   enddo                                                             
!
! compute the "low level" or 1/3 wind magnitude (m/s)                
!                                                                       
   do i = its,ite                                                       
     ulow(i) = max(sqrt(ubar(i)*ubar(i) + vbar(i)*vbar(i)), 1.0)
     rulow(i) = 1./ulow(i)
   enddo                                                             
!
   do k = kts,kte-1                                                    
     do i = its,ite                                                   
       velco(i,k) = 0.5 * ((u1(i,k)+u1(i,k+1)) * ubar(i)                       &
                          + (v1(i,k)+v1(i,k+1)) * vbar(i))                 
       velco(i,k) = velco(i,k) * rulow(i)                               
       if ((velco(i,k).lt.veleps) .and. (velco(i,k).gt.0.)) then
         velco(i,k) = veleps                                      
       endif
     enddo                                                          
   enddo                                                             
!                                                                       
! no drag when critical level in the base layer                        
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
! the low level weighted average ri is stored in usqj(1,1; im)      
! the low level weighted average n**2 is stored in bnv2(1,1; im)    
! this is called bnvl2 in phy_gwd_alpert_sub not bnv2                           
! rdelks (del(k)/delks) vert ave factor so we can * instead of /    
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
! set all ri low level values to the low level value          
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
       fr(i) = bnv(i)  * rulow(i) * var(i) * od(i)
       fr(i) = min(fr(i),frmax)
       xn(i)  = ubar(i) * rulow(i)
       yn(i)  = vbar(i) * rulow(i)
     endif
   enddo
!
! compute the base level stress and store it in taub
! calculate enhancement factor, number of mountains & aspect        
! ratio const. use simplified relationship between standard            
! deviation & critical hgt                                          
!
   do i = its,ite                                                       
     if (.not. ldrag(i))   then   
       efact    = (oa(i) + 2.) ** (ce*fr(i)/frc)                         
       efact    = min( max(efact,efmin), efmax )                            
       coefm(i) = (1. + ol(i)) ** (oa(i)+1.)                   
       xlinv(i) = coefm(i) / cleff(i)
       tem      = fr(i) * fr(i) * oc1(i)
       gfobnv   = gmax * tem / ((tem + cg)*bnv(i))   
       taub(i)  = xlinv(i) * rhobar(i) * ulow(i) * ulow(i)                     &
                * ulow(i) * gfobnv * efact          
     else                                                          
       taub(i) = 0.0                                             
       xn(i)   = 0.0                                             
       yn(i)   = 0.0                                             
     endif                                                         
   enddo                                                             
!                                                                       
! now compute vertical structure of the stress.
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
! unstablelayer if ri < ric
! unstable layer if upper air vel comp along surf vel <=0 (crit lay)
! at (u-c)=0. crit layer exists and bit vector should be set (.le.)
!
       if (k .ge. kbl(i)) then
         icrilv(i) = icrilv(i) .or. ( usqj(i,k) .lt. ric)                      &
                               .or. (velco(i,k) .le. 0.0)
         brvf(i) = max(bnv2(i,k),bnv2min) ! brunt-vaisala frequency squared
         brvf(i) = sqrt(brvf(i))          ! brunt-vaisala frequency
       endif
     enddo
!
     do i = its,ite
       if (k .ge. kbl(i) .and. (.not. ldrag(i)))   then   
         if (.not.icrilv(i) .and. taup(i,k) .gt. 0.0 ) then
           temv = 1.0 / velco(i,k)
           tem1 = coefm(i)/dxy(i)*(rho(i,kp1)+rho(i,k))*brvf(i)*velco(i,k)*0.5
           hd   = sqrt(taup(i,k) / tem1)
           fro  = brvf(i) * hd * temv
!
! rim is the  minimum-richardson number by shutts (1985)
!
           tem2 = sqrt(usqj(i,k))
           tem  = 1. + tem2 * fro
           rim  = usqj(i,k) * (1.-fro) / (tem * tem)
!
! check stability to employ the 'saturation hypothesis'
! of lindzen (1981) except at tropospheric downstream regions
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
   if (lcap.lt.kte) then                                               
     do klcap = lcapp1,kte                                          
       do i = its,ite                                                 
         taup(i,klcap) = prsi(i,klcap) / prsi(i,lcap) * taup(i,lcap)      
       enddo                                                       
     enddo                                                          
   endif                                                             
   do i = its,ite
     if (.not.ldrag(i)) then
!
! determine the height of flow-blocking layer
!
       kblk = 0
       fbdpe = 0.0
       fbdke = 0.0
       do k = kte, kpblmin, -1
         if (kblk.eq.0 .and. k.le.kbl(i)) then
           fbdpe = fbdpe + bnv2(i,k)*(zl(i,kbl(i))-zl(i,k))                    &
                   *del(i,k)/g_/rho(i,k)
           fbdke = 0.5*(u1(i,k)**2.+v1(i,k)**2.)
!
! apply flow-blocking drag when fbdpe >= fbdke 
!
           if (fbdpe.ge.fbdke) then
             kblk = k
             kblk = min(kblk,kbl(i))
             zblk = zl(i,kblk)-zl(i,kts)
           endif
         endif
       enddo
       if (kblk.ne.0) then
!
! compute flow-blocking stress
!
         fbdcd = max(2.0-1.0/od(i),0.0)
         taufb(i,kts) = 0.5*rhobar(i)*coefm(i)/dxmeter(i)**2*fbdcd*dxyp(i)     &
                        *olp(i)*zblk*ulow(i)**2
         tautem = taufb(i,kts)/real(kblk-kts)
         do k = kts+1, kblk
           taufb(i,k) = taufb(i,k-1) - tautem
         enddo
!
! sum orographic GW stress and flow-blocking stress
!
         taup(i,:) = taup(i,:) + taufb(i,:)
       endif
     endif
   enddo 
!                                                                       
! calculate - (g)*d(tau)/d(pressure) and deceleration terms dtaux, dtauy
!
   do k = kts,kte                                                       
     do i = its,ite                                                       
       taud(i,k) = 1. * (taup(i,k+1) - taup(i,k)) * g_ / del(i,k)
     enddo                                                             
   enddo                                                             
!                                                                       
! if the gravity wave drag would force a critical line             
! in the lower ksmm1 layers during the next deltim timestep,     
! then only apply drag until that critical line is reached.        
!                                                                       
   do k = kts,kpblmax-1                                                    
     do i = its,ite                                                    
       if (k .le. kbl(i)) then
         if (taud(i,k).ne.0.)                                                   &
           dtfac(i) = min(dtfac(i),abs(velco(i,k)/(deltim*taud(i,k))))  
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
       taud(i,k) = taud(i,k) * dtfac(i)                              
       dtaux = taud(i,k) * xn(i)
       dtauy = taud(i,k) * yn(i)
       dtaux2d(i,k) = dtaux
       dtauy2d(i,k) = dtauy
       dudt(i,k) = dtaux
       dvdt(i,k) = dtauy
       dusfc(i)  = dusfc(i) + dtaux * del(i,k)
       dvsfc(i)  = dvsfc(i) + dtauy * del(i,k)
     enddo                                                          
   enddo                                                             
!
   do i = its,ite
     dusfc(i) = (-1./g_) * dusfc(i)
     dvsfc(i) = (-1./g_) * dvsfc(i)
   enddo
!
! rotate tendencies from zonal/meridional back to model grid
!
   do k = kts,kte
      do i = its,ite
         rublten(i,k) = rublten(i,k)+dudt(i,k)*cosa(i) + dvdt(i,k)*sina(i)
         rvblten(i,k) = rvblten(i,k)-dudt(i,k)*sina(i) + dvdt(i,k)*cosa(i)
         dtaux3d(i,k) = dtaux2d(i,k)*cosa(i) + dtauy2d(i,k)*sina(i)
         dtauy3d(i,k) =-dtaux2d(i,k)*sina(i) + dtauy2d(i,k)*cosa(i)
     enddo
   enddo
   do i = its,ite
      dusfcg(i) = dusfc(i)*cosa(i) + dvsfc(i)*sina(i)
      dvsfcg(i) =-dusfc(i)*sina(i) + dvsfc(i)*cosa(i)
   enddo
   return                                                            
   end subroutine bl_gwdo_run


!=================================================================================================================
 end module bl_gwdo
!=================================================================================================================

