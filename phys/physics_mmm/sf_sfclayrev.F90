!=================================================================================================================
 module sf_sfclayrev
 use ccpp_kind_types,only: kind_phys

 implicit none
 private
 public:: sf_sfclayrev_run,     &
          sf_sfclayrev_init,    &
          sf_sfclayrev_finalize


 real(kind=kind_phys),parameter:: vconvc= 1.
 real(kind=kind_phys),parameter:: czo   = 0.0185
 real(kind=kind_phys),parameter:: ozo   = 1.59e-5

 real(kind=kind_phys),dimension(0:1000 ),save:: psim_stab,psim_unstab,psih_stab,psih_unstab


 contains


!=================================================================================================================
!>\section arg_table_sf_sfclayrev_init
!!\html\include sf_sfclayrev_init.html
!!
 subroutine sf_sfclayrev_init(errmsg,errflg)
!=================================================================================================================

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!local variables:
 integer:: n
 real(kind=kind_phys):: zolf

!-----------------------------------------------------------------------------------------------------------------

 do n = 0,1000
! stable function tables
    zolf = float(n)*0.01
    psim_stab(n)=psim_stable_full(zolf)
    psih_stab(n)=psih_stable_full(zolf)

! unstable function tables
    zolf = -float(n)*0.01
    psim_unstab(n)=psim_unstable_full(zolf)
    psih_unstab(n)=psih_unstable_full(zolf)
 enddo

 errmsg = 'sf_sfclayrev_init OK'
 errflg = 0

 end subroutine sf_sfclayrev_init

!=================================================================================================================
!>\section arg_table_sf_sfclayrev_finalize
!!\html\include sf_sfclayrev_finalize.html
!!
 subroutine sf_sfclayrev_finalize(errmsg,errflg)
!=================================================================================================================

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------

 errmsg = 'sf_sfclayrev_finalize OK'
 errflg = 0

 end subroutine sf_sfclayrev_finalize

!=================================================================================================================
!>\section arg_table_sf_sfclayrev_run
!!\html\include sf_sfclayrev_run.html
!!
 subroutine sf_sfclayrev_run(ux,vx,t1d,qv1d,p1d,dz8w1d,                &
                             cp,g,rovcp,r,xlv,psfcpa,chs,chs2,cqs2,    &
                             cpm,pblh,rmol,znt,ust,mavail,zol,mol,     &
                             regime,psim,psih,fm,fh,                   &
                             xland,hfx,qfx,tsk,                        &
                             u10,v10,th2,t2,q2,flhc,flqc,qgh,          &
                             qsfc,lh,gz1oz0,wspd,br,isfflx,dx,         &
                             svp1,svp2,svp3,svpt0,ep1,ep2,             &
                             karman,p1000mb,lakemask,                  &
                             shalwater_z0,water_depth,                 &
                             isftcflx,iz0tlnd,scm_force_flux,          &
                             ustm,ck,cka,cd,cda,                       &
                             its,ite,errmsg,errflg                     &
                            )
!=================================================================================================================

!--- input arguments:
 logical,intent(in):: isfflx
 logical,intent(in):: shalwater_z0
 logical,intent(in),optional:: scm_force_flux

 integer,intent(in):: its,ite
 integer,intent(in),optional:: isftcflx, iz0tlnd

 real(kind=kind_phys),intent(in):: svp1,svp2,svp3,svpt0
 real(kind=kind_phys),intent(in):: ep1,ep2,karman
 real(kind=kind_phys),intent(in):: p1000mb
 real(kind=kind_phys),intent(in):: cp,g,rovcp,r,xlv

 real(kind=kind_phys),intent(in),dimension(its:):: &
    mavail,     &
    pblh,       &
    psfcpa,     &
    tsk,        &
    xland,      &
    lakemask,   &
    water_depth

 real(kind=kind_phys),intent(in),dimension(its:):: &
    dx,         &
    dz8w1d,     &    
    ux,         &
    vx,         &
    qv1d,       &
    p1d,        &
    t1d

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

 real(kind=kind_phys),intent(out),dimension(its:):: &
    lh,         &
    u10,        &
    v10,        &
    th2,        &
    t2,         &
    q2

 real(kind=kind_phys),intent(out),dimension(its:),optional:: &
    ck,         &
    cka,        &
    cd,         &
    cda

!--- inout arguments:
 real(kind=kind_phys),intent(inout),dimension(its:):: &
    regime,     &
    hfx,        &
    qfx,        &
    qsfc,       &
    mol,        &
    rmol,       &
    gz1oz0,     &
    wspd,       &
    br,         &
    psim,       &
    psih,       &
    fm,         &
    fh,         &
    znt,        &
    zol,        &
    ust,        &
    cpm,        &
    chs2,       &
    cqs2,       &
    chs,        &
    flhc,       &
    flqc,       &
    qgh

 real(kind=kind_phys),intent(inout),dimension(its:),optional:: &
    ustm

!--- local variables:
 integer:: n,i,k,kk,l,nzol,nk,nzol2,nzol10

 real(kind=kind_phys),parameter:: xka = 2.4e-5
 real(kind=kind_phys),parameter:: prt = 1.
 real(kind=kind_phys),parameter:: salinity_factor = 0.98

 real(kind=kind_phys):: pl,thcon,tvcon,e1
 real(kind=kind_phys):: zl,tskv,dthvdz,dthvm,vconv,rzol,rzol2,rzol10,zol2,zol10
 real(kind=kind_phys):: dtg,psix,dtthx,psix10,psit,psit2,psiq,psiq2,psiq10
 real(kind=kind_phys):: fluxc,vsgd,z0q,visc,restar,czil,gz0ozq,gz0ozt
 real(kind=kind_phys):: zw,zn1,zn2
 real(kind=kind_phys):: zolzz,zol0
 real(kind=kind_phys):: zl2,zl10,z0t

 real(kind=kind_phys),dimension(its:ite):: &
    za,         &
    thvx,       &
    zqkl,       &
    zqklp1,     &
    thx,        &
    qx,         &
    psih2,      &
    psim2,      &
    psih10,     &
    psim10,     &
    denomq,     &
    denomq2,    &
    denomt2,    &
    wspdi,      &
    gz2oz0,     &
    gz10oz0,    &
    rhox,       &
    govrth,     &
    tgdsa,      &
    scr3,       &
    scr4,       &
    thgb,       &
    psfc

 real(kind=kind_phys),dimension(its:ite):: &
    pq,         &
    pq2,        &
    pq10

!-----------------------------------------------------------------------------------------------------------------

 do i = its,ite
!PSFC cb
    psfc(i)=psfcpa(i)/1000.
 enddo
!                                                      
!----CONVERT GROUND TEMPERATURE TO POTENTIAL TEMPERATURE:  
!                                                            
 do 5 i = its,ite                                   
    tgdsa(i)=tsk(i)                                    
!PSFC cb
!   thgb(i)=tsk(i)*(100./psfc(i))**rovcp                
    thgb(i)=tsk(i)*(p1000mb/psfcpa(i))**rovcp   
 5 continue                                               
!                                                            
!-----DECOUPLE FLUX-FORM VARIABLES TO GIVE U,V,T,THETA,THETA-VIR.,
!     T-VIR., QV, AND QC AT CROSS POINTS AND AT KTAU-1.  
!                                                                 
!     *** NOTE ***                                           
!         THE BOUNDARY WINDS MAY NOT BE ADEQUATELY AFFECTED BY FRICTION,         
!         SO USE ONLY INTERIOR VALUES OF UX AND VX TO CALCULATE 
!         TENDENCIES.                             
!                                                           
 10 continue                                                     

!do 24 i = its,ite
!   ux(i)=u1d(i)
!   vx(i)=v1d(i)
!24 continue                                             
                                                             
 26 continue                                               

!.....SCR3(I,K) STORE TEMPERATURE,                           
!     SCR4(I,K) STORE VIRTUAL TEMPERATURE.                                       
                                                                                 
 do 30 i = its,ite
!PL cb
    pl=p1d(i)/1000.
    scr3(i)=t1d(i)                                                   
!   thcon=(100./pl)**rovcp                                                 
    thcon=(p1000mb*0.001/pl)**rovcp
    thx(i)=scr3(i)*thcon                                               
    scr4(i)=scr3(i)                                                    
    thvx(i)=thx(i)                                                     
    qx(i)=0.                                                             
 30 continue                                                                 
!                                                                                
 do i = its,ite
    qgh(i)=0.                                                                
    flhc(i)=0.                                                               
    flqc(i)=0.                                                               
    cpm(i)=cp                                                                
 enddo
!                                                                                
!if(idry.eq.1)goto 80                                                   
 do 50 i = its,ite
    qx(i)=qv1d(i)                                                    
    tvcon=(1.+ep1*qx(i))                                      
    thvx(i)=thx(i)*tvcon                                               
    scr4(i)=scr3(i)*tvcon                                              
 50 continue                                                                 
!
 do 60 i=its,ite
    e1=svp1*exp(svp2*(tgdsa(i)-svpt0)/(tgdsa(i)-svp3))                       
    !the saturation vapor pressure for salty water is on average 2% lower
    if(xland(i).gt.1.5 .and. lakemask(i).eq.0.) e1=e1*salinity_factor
    !for land points qsfc can come from previous time step
    if(xland(i).gt.1.5.or.qsfc(i).le.0.0)qsfc(i)=ep2*e1/(psfc(i)-e1)                                                 
!QGH CHANGED TO USE LOWEST-LEVEL AIR TEMP CONSISTENT WITH MYJSFC CHANGE
!Q2SAT = QGH IN LSM
    e1=svp1*exp(svp2*(t1d(i)-svpt0)/(t1d(i)-svp3))                       
    pl=p1d(i)/1000.
    qgh(i)=ep2*e1/(pl-e1)                                                 
    cpm(i)=cp*(1.+0.8*qx(i))                                   
 60 continue                                                                   
 80 continue

!-----COMPUTE THE HEIGHT OF FULL- AND HALF-SIGMA LEVELS ABOVE GROUND             
!     LEVEL, AND THE LAYER THICKNESSES.                                          
                                                                                 
 do 90 i = its,ite
    zqklp1(i)=0.
    rhox(i)=psfc(i)*1000./(r*scr4(i))                                       
 90 continue                                                                   
!                                                                                
 do 110 i = its,ite                                                   
    zqkl(i)=dz8w1d(i)+zqklp1(i)
 110 continue                                                                 
!                                                                                
 do 120 i = its,ite
    za(i)=0.5*(zqkl(i)+zqklp1(i))                                        
 120 continue                                                                 
!                                                                                
 do 160 i=its,ite
    govrth(i)=g/thx(i)                                                    
 160 continue                                                                   
                                                                                 
!-----CALCULATE BULK RICHARDSON NO. OF SURFACE LAYER, ACCORDING TO               
!     AKB(1976), EQ(12).                                                                            
 do 260 i = its,ite
    gz1oz0(i)=alog((za(i)+znt(i))/znt(i))   ! log((z+z0)/z0)                                     
    gz2oz0(i)=alog((2.+znt(i))/znt(i))      ! log((2+z0)/z0)                           
    gz10oz0(i)=alog((10.+znt(i))/znt(i))    ! log((10+z0)z0)                    
    if((xland(i)-1.5).ge.0)then                                            
       zl=znt(i)                                                            
    else                                                                     
       zl=0.01                                                                
    endif                                                                    
    wspd(i)=sqrt(ux(i)*ux(i)+vx(i)*vx(i))                        

    tskv=thgb(i)*(1.+ep1*qsfc(i))                     
    dthvdz=(thvx(i)-tskv)                                                 
!-----CONVECTIVE VELOCITY SCALE VC AND SUBGRID-SCALE VELOCITY VSG
!     FOLLOWING BELJAARS (1994, QJRMS) AND MAHRT AND SUN (1995, MWR)
!                         ... HONG AUG. 2001
!
!   vconv = 0.25*sqrt(g/tskv*pblh(i)*dthvm)
!   USE BELJAARS OVER LAND, OLD MM5 (WYNGAARD) FORMULA OVER WATER
    if(xland(i).lt.1.5) then
       fluxc = max(hfx(i)/rhox(i)/cp                    &
             + ep1*tskv*qfx(i)/rhox(i),0.)
       vconv = vconvc*(g/tgdsa(i)*pblh(i)*fluxc)**.33
    else
       if(-dthvdz.ge.0)then
          dthvm=-dthvdz
       else
          dthvm=0.
       endif
!      vconv = 2.*sqrt(dthvm)
! V3.7: REDUCING CONTRIBUTION IN CALM CONDITIONS
       vconv = sqrt(dthvm)
    endif
! MAHRT AND SUN LOW-RES CORRECTION
    vsgd = 0.32 * (max(dx(i)/5000.-1.,0.))**.33
    wspd(i)=sqrt(wspd(i)*wspd(i)+vconv*vconv+vsgd*vsgd)
    wspd(i)=amax1(wspd(i),0.1)
    br(i)=govrth(i)*za(i)*dthvdz/(wspd(i)*wspd(i))                        
!-----IF PREVIOUSLY UNSTABLE, DO NOT LET INTO REGIMES 1 AND 2
    if(mol(i).lt.0.)br(i)=amin1(br(i),0.0)
    rmol(i)=-govrth(i)*dthvdz*za(i)*karman
 260 continue

!                                                                                
!-----DIAGNOSE BASIC PARAMETERS FOR THE APPROPRIATED STABILITY CLASS:            
!                                                                                
!                                                                                
!     THE STABILITY CLASSES ARE DETERMINED BY BR (BULK RICHARDSON NO.)           
!     AND HOL (HEIGHT OF PBL/MONIN-OBUKHOV LENGTH).                              
!                                                                                
!     CRITERIA FOR THE CLASSES ARE AS FOLLOWS:                                   
!                                                                                
!        1. BR .GE. 0.0;                                                         
!               REPRESENTS NIGHTTIME STABLE CONDITIONS (REGIME=1),               
!                                                                                
!        3. BR .EQ. 0.0                                                          
!               REPRESENTS FORCED CONVECTION CONDITIONS (REGIME=3),              
!                                                                                
!        4. BR .LT. 0.0                                                          
!               REPRESENTS FREE CONVECTION CONDITIONS (REGIME=4).                
!                                                                                

 do 320 i = its,ite
!                                                                           
    if(br(i).gt.0) then
       if(br(i).gt.250.0) then
          zol(i)=zolri(250.0,za(i),znt(i))
       else
          zol(i)=zolri(br(i),za(i),znt(i))
       endif
    endif
!
    if(br(i).lt.0) then
       if(ust(i).lt.0.001)then
          zol(i)=br(i)*gz1oz0(i)
       else
          if(br(i).lt.-250.0) then
             zol(i)=zolri(-250.0,za(i),znt(i))
           else
              zol(i)=zolri(br(i),za(i),znt(i))
           endif
       endif
    endif
!
! ... paj: compute integrated similarity functions.
!
    zolzz=zol(i)*(za(i)+znt(i))/za(i) ! (z+z0/L
    zol10=zol(i)*(10.+znt(i))/za(i)   ! (10+z0)/L
    zol2=zol(i)*(2.+znt(i))/za(i)     ! (2+z0)/L
    zol0=zol(i)*znt(i)/za(i)          ! z0/L
    zl2=(2.)/za(i)*zol(i)             ! 2/L     
    zl10=(10.)/za(i)*zol(i)           ! 10/L

    if((xland(i)-1.5).lt.0.)then
       zl=(0.01)/za(i)*zol(i)         ! (0.01)/L     
    else
       zl=zol0                        ! z0/L
    endif

    if(br(i).lt.0.)goto 310  ! go to unstable regime (class 4)
    if(br(i).eq.0.)goto 280  ! go to neutral regime (class 3)
!                                                                                
!-----CLASS 1; STABLE (NIGHTTIME) CONDITIONS:                                    
!
    regime(i)=1.
!
! ... paj: psim and psih. follows cheng and brutsaert 2005 (cb05).
!
    psim(i)=psim_stable(zolzz)-psim_stable(zol0)
    psih(i)=psih_stable(zolzz)-psih_stable(zol0)
!
    psim10(i)=psim_stable(zol10)-psim_stable(zol0)
    psih10(i)=psih_stable(zol10)-psih_stable(zol0)
!
    psim2(i)=psim_stable(zol2)-psim_stable(zol0)
    psih2(i)=psih_stable(zol2)-psih_stable(zol0)
!
! ... paj: preparations to compute psiq. follows cb05+carlson boland jam 1978.
!
    pq(i)=psih_stable(zol(i))-psih_stable(zl)
    pq2(i)=psih_stable(zl2)-psih_stable(zl)
    pq10(i)=psih_stable(zl10)-psih_stable(zl)
!
!   1.0 over monin-obukhov length
    rmol(i)=zol(i)/za(i) 
!                                                                                
    goto 320                                                                 
!                                                                                
!-----CLASS 3; FORCED CONVECTION:                                                
!                                                                                
 280 regime(i)=3.                                                           
     psim(i)=0.0                                                              
     psih(i)=psim(i)                                                          
     psim10(i)=0.                                                   
     psih10(i)=psim10(i)                                           
     psim2(i)=0.                                                  
     psih2(i)=psim2(i)                                           
!
! paj: preparations to compute PSIQ.
!
     pq(i)=psih(i)
     pq2(i)=psih2(i)
     pq10(i)=0.
!
     zol(i)=0.                                             
     rmol(i) = zol(i)/za(i)  

     goto 320                                                                 
!                                                                                
!-----CLASS 4; FREE CONVECTION:                                                  
!                                                                                
 310 continue                                                                 
     regime(i)=4.                                                           
!
! ... paj: PSIM and PSIH ...
!
     psim(i)=psim_unstable(zolzz)-psim_unstable(zol0)
     psih(i)=psih_unstable(zolzz)-psih_unstable(zol0)
!
     psim10(i)=psim_unstable(zol10)-psim_unstable(zol0)
     psih10(i)=psih_unstable(zol10)-psih_unstable(zol0)
!
     psim2(i)=psim_unstable(zol2)-psim_unstable(zol0)
     psih2(i)=psih_unstable(zol2)-psih_unstable(zol0)
!
! ... paj: preparations to compute PSIQ 
!
     pq(i)=psih_unstable(zol(i))-psih_unstable(zl)
     pq2(i)=psih_unstable(zl2)-psih_unstable(zl)
     pq10(i)=psih_unstable(zl10)-psih_unstable(zl)
!
!-----LIMIT PSIH AND PSIM IN THE CASE OF THIN LAYERS AND HIGH ROUGHNESS            
!-----THIS PREVENTS DENOMINATOR IN FLUXES FROM GETTING TOO SMALL                 
     psih(i)=amin1(psih(i),0.9*gz1oz0(i))
     psim(i)=amin1(psim(i),0.9*gz1oz0(i))
     psih2(i)=amin1(psih2(i),0.9*gz2oz0(i))
     psim10(i)=amin1(psim10(i),0.9*gz10oz0(i))
!
! AHW: mods to compute ck, cd
     psih10(i)=amin1(psih10(i),0.9*gz10oz0(i))
     rmol(i) = zol(i)/za(i)  

 320 continue                                                                   
!                                                                                
!-----COMPUTE THE FRICTIONAL VELOCITY:                                           
!     ZA(1982) EQS(2.60),(2.61).                                                 
!                                                                                
 do 330 i = its,ite
    dtg=thx(i)-thgb(i)                                                   
    psix=gz1oz0(i)-psim(i)                                                   
    psix10=gz10oz0(i)-psim10(i)

!   LOWER LIMIT ADDED TO PREVENT LARGE FLHC IN SOIL MODEL
!   ACTIVATES IN UNSTABLE CONDITIONS WITH THIN LAYERS OR HIGH Z0
!   PSIT=AMAX1(GZ1OZ0(I)-PSIH(I),2.)
    psit=gz1oz0(i)-psih(i)
    psit2=gz2oz0(i)-psih2(i)
!
    if((xland(i)-1.5).ge.0)then                                            
       zl=znt(i)                                                            
    else                                                                     
       zl=0.01                                                                
    endif                                                                    
!
    psiq=alog(karman*ust(i)*za(i)/xka+za(i)/zl)-pq(i)
    psiq2=alog(karman*ust(i)*2./xka+2./zl)-pq2(i)

! AHW: mods to compute ck, cd
    psiq10=alog(karman*ust(i)*10./xka+10./zl)-pq10(i)

! v3.7: using fairall 2003 to compute z0q and z0t over water:
!       adapted from module_sf_mynn.f
    if((xland(i)-1.5).ge.0.) then
       visc=(1.32+0.009*(scr3(i)-273.15))*1.e-5
       restar=ust(i)*znt(i)/visc
       z0t = (5.5e-5)*(restar**(-0.60))
       z0t = min(z0t,1.0e-4)
       z0t = max(z0t,2.0e-9)
       z0q = z0t

! following paj:
       zolzz=zol(i)*(za(i)+z0t)/za(i) ! (z+z0t)/L
       zol10=zol(i)*(10.+z0t)/za(i)   ! (10+z0t)/L
       zol2=zol(i)*(2.+z0t)/za(i)     ! (2+z0t)/L
       zol0=zol(i)*z0t/za(i)          ! z0t/L
!
       if(zol(i).gt.0.) then
          psih(i)=psih_stable(zolzz)-psih_stable(zol0)
          psih10(i)=psih_stable(zol10)-psih_stable(zol0)
          psih2(i)=psih_stable(zol2)-psih_stable(zol0)
       else
          if(zol(i).eq.0) then
             psih(i)=0.
             psih10(i)=0.
             psih2(i)=0.
          else
             psih(i)=psih_unstable(zolzz)-psih_unstable(zol0)
             psih10(i)=psih_unstable(zol10)-psih_unstable(zol0)
             psih2(i)=psih_unstable(zol2)-psih_unstable(zol0)
          endif
       endif
       psit=alog((za(i)+z0t)/z0t)-psih(i)
       psit2=alog((2.+z0t)/z0t)-psih2(i)

       zolzz=zol(i)*(za(i)+z0q)/za(i)    ! (z+z0q)/L
       zol10=zol(i)*(10.+z0q)/za(i)   ! (10+z0q)/L
       zol2=zol(i)*(2.+z0q)/za(i)     ! (2+z0q)/L
       zol0=zol(i)*z0q/za(i)          ! z0q/L
!
       if(zol(i).gt.0.) then
          psih(i)=psih_stable(zolzz)-psih_stable(zol0)
          psih10(i)=psih_stable(zol10)-psih_stable(zol0)
          psih2(i)=psih_stable(zol2)-psih_stable(zol0)
       else
          if(zol(i).eq.0) then
             psih(i)=0.
             psih10(i)=0.
             psih2(i)=0.
          else
             psih(i)=psih_unstable(zolzz)-psih_unstable(zol0)
             psih10(i)=psih_unstable(zol10)-psih_unstable(zol0)
             psih2(i)=psih_unstable(zol2)-psih_unstable(zol0)
             endif
          endif
!
          psiq=alog((za(i)+z0q)/z0q)-psih(i)
          psiq2=alog((2.+z0q)/z0q)-psih2(i)
          psiq10=alog((10.+z0q)/z0q)-psih10(i)
       endif

       if(present(isftcflx)) then
          if(isftcflx.eq.1 .and. (xland(i)-1.5).ge.0.) then
! v3.1
!            z0q = 1.e-4 + 1.e-3*(max(0.,ust(i)-1.))**2
! hfip1
!            z0q = 0.62*2.0e-5/ust(i) + 1.e-3*(max(0.,ust(i)-1.5))**2
! v3.2
             z0q = 1.e-4
!
! ... paj: recompute psih for z0q
!
          zolzz=zol(i)*(za(i)+z0q)/za(i) ! (z+z0q)/L
          zol10=zol(i)*(10.+z0q)/za(i)   ! (10+z0q)/L
          zol2=zol(i)*(2.+z0q)/za(i)     ! (2+z0q)/L
          zol0=zol(i)*z0q/za(i)          ! z0q/L
!
          if(zol(i).gt.0.) then
             psih(i)=psih_stable(zolzz)-psih_stable(zol0)
             psih10(i)=psih_stable(zol10)-psih_stable(zol0)
             psih2(i)=psih_stable(zol2)-psih_stable(zol0)
          else
             if(zol(i).eq.0) then
                psih(i)=0.
                psih10(i)=0.
                psih2(i)=0.
             else
                psih(i)=psih_unstable(zolzz)-psih_unstable(zol0)
                psih10(i)=psih_unstable(zol10)-psih_unstable(zol0)
                psih2(i)=psih_unstable(zol2)-psih_unstable(zol0)
             endif
          endif
!
          psiq=alog((za(i)+z0q)/z0q)-psih(i)
          psit=psiq
          psiq2=alog((2.+z0q)/z0q)-psih2(i)
          psiq10=alog((10.+z0q)/z0q)-psih10(i)
          psit2=psiq2
       endif
       if(isftcflx.eq.2 .and. (xland(i)-1.5).ge.0.) then
! AHW: Garratt formula: Calculate roughness Reynolds number
!        Kinematic viscosity of air (linear approc to
!        temp dependence at sea level)
! GZ0OZT and GZ0OZQ are based off formulas from Brutsaert (1975), which
! Garratt (1992) used with values of k = 0.40, Pr = 0.71, and Sc = 0.60
          visc=(1.32+0.009*(scr3(i)-273.15))*1.e-5
!         visc=1.5e-5
          restar=ust(i)*znt(i)/visc
          gz0ozt=0.40*(7.3*sqrt(sqrt(restar))*sqrt(0.71)-5.)
!
! ... paj: compute psih for z0t for temperature ...
!
          z0t=znt(i)/exp(gz0ozt)
!
          zolzz=zol(i)*(za(i)+z0t)/za(i) ! (z+z0t)/L
          zol10=zol(i)*(10.+z0t)/za(i)   ! (10+z0t)/L
          zol2=zol(i)*(2.+z0t)/za(i)     ! (2+z0t)/L
          zol0=zol(i)*z0t/za(i)          ! z0t/L
!
          if(zol(i).gt.0.) then
             psih(i)=psih_stable(zolzz)-psih_stable(zol0)
             psih10(i)=psih_stable(zol10)-psih_stable(zol0)
             psih2(i)=psih_stable(zol2)-psih_stable(zol0)
          else
             if(zol(i).eq.0) then
                psih(i)=0.
                psih10(i)=0.
                psih2(i)=0.
             else
                psih(i)=psih_unstable(zolzz)-psih_unstable(zol0)
                psih10(i)=psih_unstable(zol10)-psih_unstable(zol0)
                psih2(i)=psih_unstable(zol2)-psih_unstable(zol0)
             endif
          endif
!
!         psit=gz1oz0(i)-psih(i)+restar2
!         psit2=gz2oz0(i)-psih2(i)+restar2
          psit=alog((za(i)+z0t)/z0t)-psih(i)
          psit2=alog((2.+z0t)/z0t)-psih2(i)
!
          gz0ozq=0.40*(7.3*sqrt(sqrt(restar))*sqrt(0.60)-5.)
          z0q=znt(i)/exp(gz0ozq)
!
          zolzz=zol(i)*(za(i)+z0q)/za(i) ! (z+z0q)/L
          zol10=zol(i)*(10.+z0q)/za(i)   ! (10+z0q)/L
          zol2=zol(i)*(2.+z0q)/za(i)     ! (2+z0q)/L
          zol0=zol(i)*z0q/za(i)          ! z0q/L
!
          if(zol(i).gt.0.) then
             psih(i)=psih_stable(zolzz)-psih_stable(zol0)
             psih10(i)=psih_stable(zol10)-psih_stable(zol0)
             psih2(i)=psih_stable(zol2)-psih_stable(zol0)
          else
             if(zol(i).eq.0) then
                psih(i)=0.
                psih10(i)=0.
                psih2(i)=0.
             else
                psih(i)=psih_unstable(zolzz)-psih_unstable(zol0)
                psih10(i)=psih_unstable(zol10)-psih_unstable(zol0)
                psih2(i)=psih_unstable(zol2)-psih_unstable(zol0)
             endif
          endif
!
          psiq=alog((za(i)+z0q)/z0q)-psih(i)
          psiq2=alog((2.+z0q)/z0q)-psih2(i)
          psiq10=alog((10.+z0q)/z0q)-psih10(i)
!         psiq=gz1oz0(i)-psih(i)+2.28*sqrt(sqrt(restar))-2.
!         psiq2=gz2oz0(i)-psih2(i)+2.28*sqrt(sqrt(restar))-2.
!         psiq10=gz10oz0(i)-psih(i)+2.28*sqrt(sqrt(restar))-2.
       endif
    endif
    if(present(ck) .and. present(cd) .and. present(cka) .and. present(cda)) then
       ck(i)=(karman/psix10)*(karman/psiq10)
       cd(i)=(karman/psix10)*(karman/psix10)
       cka(i)=(karman/psix)*(karman/psiq)
       cda(i)=(karman/psix)*(karman/psix)
    endif
    if(present(iz0tlnd)) then
       if(iz0tlnd.ge.1 .and. (xland(i)-1.5).le.0.) then
          zl=znt(i)
!         CZIL RELATED CHANGES FOR LAND
          visc=(1.32+0.009*(scr3(i)-273.15))*1.e-5
          restar=ust(i)*zl/visc
!         Modify CZIL according to Chen & Zhang, 2009 if iz0tlnd = 1
!         If iz0tlnd = 2, use traditional value

          if(iz0tlnd.eq.1) then
             czil = 10.0 ** ( -0.40 * ( zl / 0.07 ) )
          elseif(iz0tlnd.eq.2) then
             czil = 0.1 
          endif
!
! ... paj: compute phish for z0t over land
!
          z0t=znt(i)/exp(czil*karman*sqrt(restar))
!
          zolzz=zol(i)*(za(i)+z0t)/za(i) ! (z+z0t)/L
          zol10=zol(i)*(10.+z0t)/za(i)   ! (10+z0t)/L
          zol2=zol(i)*(2.+z0t)/za(i)     ! (2+z0t)/L
          zol0=zol(i)*z0t/za(i)          ! z0t/L
!
          if(zol(i).gt.0.) then
             psih(i)=psih_stable(zolzz)-psih_stable(zol0)
             psih10(i)=psih_stable(zol10)-psih_stable(zol0)
             psih2(i)=psih_stable(zol2)-psih_stable(zol0)
          else
             if(zol(i).eq.0) then
                psih(i)=0.
                psih10(i)=0.
                psih2(i)=0.
             else
                psih(i)=psih_unstable(zolzz)-psih_unstable(zol0)
                psih10(i)=psih_unstable(zol10)-psih_unstable(zol0)
                psih2(i)=psih_unstable(zol2)-psih_unstable(zol0)
             endif
          endif
!
          psiq=alog((za(i)+z0t)/z0t)-psih(i)
          psiq2=alog((2.+z0t)/z0t)-psih2(i)
          psit=psiq
          psit2=psiq2
!
!         psit=gz1oz0(i)-psih(i)+czil*karman*sqrt(restar)
!         psiq=gz1oz0(i)-psih(i)+czil*karman*sqrt(restar)
!         psit2=gz2oz0(i)-psih2(i)+czil*karman*sqrt(restar)
!         psiq2=gz2oz0(i)-psih2(i)+czil*karman*sqrt(restar)
       endif
    endif
! TO PREVENT OSCILLATIONS AVERAGE WITH OLD VALUE 
    ust(i)=0.5*ust(i)+0.5*karman*wspd(i)/psix                                             
! TKE coupling: compute ust without vconv for use in tke scheme
    wspdi(i)=sqrt(ux(i)*ux(i)+vx(i)*vx(i))
    if(present(ustm)) then
       ustm(i)=0.5*ustm(i)+0.5*karman*wspdi(i)/psix
    endif

    u10(i)=ux(i)*psix10/psix                                    
    v10(i)=vx(i)*psix10/psix                                   
    th2(i)=thgb(i)+dtg*psit2/psit                                
    q2(i)=qsfc(i)+(qx(i)-qsfc(i))*psiq2/psiq                   
    t2(i) = th2(i)*(psfcpa(i)/p1000mb)**rovcp                     
!                                                                                
    if((xland(i)-1.5).lt.0.)then                                            
       ust(i)=amax1(ust(i),0.001)
    endif                                                                    
    mol(i)=karman*dtg/psit/prt                              
    denomq(i)=psiq
    denomq2(i)=psiq2
    denomt2(i)=psit2
    fm(i)=psix
    fh(i)=psit
 330 continue                                                                   
!                                                                                
 335 continue                                                                   
                                                                                  
!-----COMPUTE THE SURFACE SENSIBLE AND LATENT HEAT FLUXES:                       
 if(present(scm_force_flux) ) then
    if(scm_force_flux) goto 350
    endif
    do i = its,ite
       qfx(i)=0.                                                              
       hfx(i)=0.                                                              
    enddo
 350 continue                                                                   

 if(.not. isfflx) goto 410

!-----OVER WATER, ALTER ROUGHNESS LENGTH (ZNT) ACCORDING TO WIND (UST).          
 do 360 i = its,ite
    if((xland(i)-1.5).ge.0)then                                            
!      znt(i)=czo*ust(i)*ust(i)/g+ozo                                   
       ! PSH - formulation for depth-dependent roughness from
       ! ... Jimenez and Dudhia, 2018
       if(shalwater_z0) then
          znt(i) = depth_dependent_z0(water_depth(i),znt(i),ust(i))
       else
          !Since V3.7 (ref: EC Physics document for Cy36r1)
          znt(i)=czo*ust(i)*ust(i)/g+0.11*1.5e-5/ust(i)
          ! v3.9: add limit as in isftcflx = 1,2
          znt(i)=min(znt(i),2.85e-3)
       endif
! COARE 3.5 (Edson et al. 2013)
!      czc = 0.0017*wspd(i)-0.005
!      czc = min(czc,0.028)
!      znt(i)=czc*ust(i)*ust(i)/g+0.11*1.5e-5/ust(i)
! AHW: change roughness length, and hence the drag coefficients Ck and Cd
       if(present(isftcflx)) then
          if(isftcflx.ne.0) then
!            znt(i)=10.*exp(-9.*ust(i)**(-.3333))
!            znt(i)=10.*exp(-9.5*ust(i)**(-.3333))
!            znt(i)=znt(i) + 0.11*1.5e-5/amax1(ust(i),0.01)
!            znt(i)=0.011*ust(i)*ust(i)/g+ozo
!            znt(i)=max(znt(i),3.50e-5)
! AHW 2012:
             zw  = min((ust(i)/1.06)**(0.3),1.0)
             zn1 = 0.011*ust(i)*ust(i)/g + ozo
             zn2 = 10.*exp(-9.5*ust(i)**(-.3333)) + &
                   0.11*1.5e-5/amax1(ust(i),0.01)
             znt(i)=(1.0-zw) * zn1 + zw * zn2
             znt(i)=min(znt(i),2.85e-3)
             znt(i)=max(znt(i),1.27e-7)
          endif
       endif
       zl = znt(i)
    else
       zl = 0.01
    endif                                                                    
    flqc(i)=rhox(i)*mavail(i)*ust(i)*karman/denomq(i)
!   flqc(i)=rhox(i)*mavail(i)*ust(i)*karman/(   &
!           alog(karman*ust(i)*za(i)/xka+za(i)/zl)-psih(i))
    dtthx=abs(thx(i)-thgb(i))                                            
    if(dtthx.gt.1.e-5)then                                                   
       flhc(i)=cpm(i)*rhox(i)*ust(i)*mol(i)/(thx(i)-thgb(i))          
!      write(*,1001)flhc(i),cpm(i),rhox(i),ust(i),mol(i),thx(i),thgb(i),i
 1001  format(f8.5,2x,f12.7,2x,f12.10,2x,f12.10,2x,f13.10,2x,f12.8,f12.8,2x,i3)
    else                                                                     
       flhc(i)=0.                                                             
    endif                                                                    
 360 continue                                                                   

!                                                                                
!-----COMPUTE SURFACE MOIST FLUX:                                                
!                                                                                
!IF(IDRY.EQ.1)GOTO 390                                                
!                                                                                
 if(present(scm_force_flux)) then
    if(scm_force_flux) goto 405
    endif

    do 370 i = its,ite
       qfx(i)=flqc(i)*(qsfc(i)-qx(i))                                     
!      qfx(i)=amax1(qfx(i),0.)                                            
       lh(i)=xlv*qfx(i)
    370 continue                                                                 

!-----COMPUTE SURFACE HEAT FLUX:                                                 
!                                                                                
    390 continue                                                                 
    do 400 i = its,ite
       if(xland(i)-1.5.gt.0.)then                                           
          hfx(i)=flhc(i)*(thgb(i)-thx(i)) 
!         if(present(isftcflx)) then
!            if(isftcflx.ne.0) then
! AHW: add dissipative heating term (commented out in 3.6.1)
!               hfx(i)=hfx(i)+rhox(i)*ustm(i)*ustm(i)*wspdi(i)
!            endif
!         endif 
       elseif(xland(i)-1.5.lt.0.)then                                       
          hfx(i)=flhc(i)*(thgb(i)-thx(i))                                
!         hfx(i)=amax1(hfx(i),-250.)                                       
       endif                                                                  
   400 continue                                                                 

   405 continue                                                                 
         
   do i = its,ite
      if((xland(i)-1.5).ge.0)then
         zl=znt(i)
      else
         zl=0.01
      endif
!v3.1.1
!     chs(i)=ust(i)*karman/(alog(karman*ust(i)*za(i) &
!           /xka+za(i)/zl)-psih(i))
      chs(i)=ust(i)*karman/denomq(i)
!     gz2oz0(i)=alog(2./znt(i))
!     psim2(i)=-10.*gz2oz0(i)
!     psim2(i)=amax1(psim2(i),-10.)
!     psih2(i)=psim2(i)
! v3.1.1
!     cqs2(i)=ust(i)*karman/(alog(karman*ust(i)*2.0  &
!            /xka+2.0/zl)-psih2(i))
!     chs2(i)=ust(i)*karman/(gz2oz0(i)-psih2(i))
      cqs2(i)=ust(i)*karman/denomq2(i)
         chs2(i)=ust(i)*karman/denomt2(i)
   enddo

   410 continue                                                                   

!jdf
!  do i = its,ite
!     if(ust(i).ge.0.1) then
!        rmol(i)=rmol(i)*(-flhc(i))/(ust(i)*ust(i)*ust(i))
!     else
!        rmol(i)=rmol(i)*(-flhc(i))/(0.1*0.1*0.1)
!     endif
!  enddo
!jdf

 errmsg = 'sf_sfclayrev_run OK'
 errflg = 0

 end subroutine sf_sfclayrev_run

!=================================================================================================================
 real(kind=kind_phys) function zolri(ri,z,z0)
 real(kind=kind_phys),intent(in):: ri,z,z0

 integer:: iter
 real(kind=kind_phys):: fx1,fx2,x1,x2


 if(ri.lt.0.)then
    x1=-5.
    x2=0.
 else
    x1=0.
    x2=5.
 endif

 fx1=zolri2(x1,ri,z,z0)
 fx2=zolri2(x2,ri,z,z0)
 iter = 0
 do while (abs(x1 - x2) > 0.01)
 if (iter .eq. 10) return
!check added for potential divide by zero (2019/11)
    if(fx1.eq.fx2)return
    if(abs(fx2).lt.abs(fx1))then
       x1=x1-fx1/(fx2-fx1)*(x2-x1)
       fx1=zolri2(x1,ri,z,z0)
       zolri=x1
    else
       x2=x2-fx2/(fx2-fx1)*(x2-x1)
       fx2=zolri2(x2,ri,z,z0)
       zolri=x2
    endif
    iter = iter + 1
 enddo

 return
 end function zolri

!=================================================================================================================
 real(kind=kind_phys) function zolri2(zol2,ri2,z,z0)
 real(kind=kind_phys),intent(in):: ri2,z,z0
 real(kind=kind_phys),intent(inout):: zol2
 real(kind=kind_phys):: psih2,psix2,zol20,zol3

 if(zol2*ri2 .lt. 0.)zol2=0.  ! limit zol2 - must be same sign as ri2

 zol20=zol2*z0/z ! z0/L
 zol3=zol2+zol20 ! (z+z0)/L

 if(ri2.lt.0) then
    psix2=log((z+z0)/z0)-(psim_unstable(zol3)-psim_unstable(zol20))
    psih2=log((z+z0)/z0)-(psih_unstable(zol3)-psih_unstable(zol20))
 else
    psix2=log((z+z0)/z0)-(psim_stable(zol3)-psim_stable(zol20))
    psih2=log((z+z0)/z0)-(psih_stable(zol3)-psih_stable(zol20))
 endif

 zolri2=zol2*psih2/psix2**2-ri2

 return
 end function zolri2

!=================================================================================================================
!
! ... integrated similarity functions ...
!
 real(kind=kind_phys) function psim_stable_full(zolf)
 real(kind=kind_phys),intent(in):: zolf
 psim_stable_full=-6.1*log(zolf+(1+zolf**2.5)**(1./2.5))

 return
 end function psim_stable_full

!=================================================================================================================
 real(kind=kind_phys) function psih_stable_full(zolf)
 real(kind=kind_phys),intent(in):: zolf
 psih_stable_full=-5.3*log(zolf+(1+zolf**1.1)**(1./1.1))

 return
 end function psih_stable_full

!=================================================================================================================
 real(kind=kind_phys) function psim_unstable_full(zolf)
 real(kind=kind_phys),intent(in):: zolf
 real(kind=kind_phys):: psimc,psimk,x,y,ym
 x=(1.-16.*zolf)**.25
 psimk=2*ALOG(0.5*(1+X))+ALOG(0.5*(1+X*X))-2.*ATAN(X)+2.*ATAN(1.)

 ym=(1.-10.*zolf)**0.33
 psimc=(3./2.)*log((ym**2.+ym+1.)/3.)-sqrt(3.)*ATAN((2.*ym+1)/sqrt(3.))+4.*ATAN(1.)/sqrt(3.)

 psim_unstable_full=(psimk+zolf**2*(psimc))/(1+zolf**2.)

 return
 end function psim_unstable_full

!=================================================================================================================
 real(kind=kind_phys) function psih_unstable_full(zolf)
 real(kind=kind_phys),intent(in):: zolf
 real(kind=kind_phys):: psihc,psihk,y,yh
 y=(1.-16.*zolf)**.5
 psihk=2.*log((1+y)/2.)

 yh=(1.-34.*zolf)**0.33
 psihc=(3./2.)*log((yh**2.+yh+1.)/3.)-sqrt(3.)*ATAN((2.*yh+1)/sqrt(3.))+4.*ATAN(1.)/sqrt(3.)

 psih_unstable_full=(psihk+zolf**2*(psihc))/(1+zolf**2.)

 return
 end function psih_unstable_full

!=================================================================================================================
! ... look-up table functions ...
 real(kind=kind_phys) function psim_stable(zolf)
 real(kind=kind_phys),intent(in):: zolf
 integer:: nzol
 real(kind=kind_phys):: rzol
 nzol = int(zolf*100.)
 rzol = zolf*100. - nzol
 if(nzol+1 .lt. 1000)then
    psim_stable = psim_stab(nzol) + rzol*(psim_stab(nzol+1)-psim_stab(nzol))
 else
    psim_stable = psim_stable_full(zolf)
 endif

 return
 end function psim_stable

!=================================================================================================================
 real(kind=kind_phys) function psih_stable(zolf)
 real(kind=kind_phys),intent(in):: zolf
 integer:: nzol
 real(kind=kind_phys):: rzol
 nzol = int(zolf*100.)
 rzol = zolf*100. - nzol
 if(nzol+1 .lt. 1000)then
    psih_stable = psih_stab(nzol) + rzol*(psih_stab(nzol+1)-psih_stab(nzol))
 else
    psih_stable = psih_stable_full(zolf)
 endif

 return
 end function psih_stable

!=================================================================================================================
 real(kind=kind_phys) function psim_unstable(zolf)
 real(kind=kind_phys),intent(in):: zolf
 integer:: nzol
 real(kind=kind_phys):: rzol
 nzol = int(-zolf*100.)
 rzol = -zolf*100. - nzol
 if(nzol+1 .lt. 1000)then
    psim_unstable = psim_unstab(nzol) + rzol*(psim_unstab(nzol+1)-psim_unstab(nzol))
 else
    psim_unstable = psim_unstable_full(zolf)
 endif

 return
 end function psim_unstable

!=================================================================================================================
 real(kind=kind_phys) function psih_unstable(zolf)
 real(kind=kind_phys),intent(in):: zolf
 integer:: nzol
 real(kind=kind_phys):: rzol
 nzol = int(-zolf*100.)
 rzol = -zolf*100. - nzol
 if(nzol+1 .lt. 1000)then
    psih_unstable = psih_unstab(nzol) + rzol*(psih_unstab(nzol+1)-psih_unstab(nzol))
 else
    psih_unstable = psih_unstable_full(zolf)
 endif

 return
 end function psih_unstable

!=================================================================================================================
 real(kind=kind_phys) function depth_dependent_z0(water_depth,z0,ust)
 real(kind=kind_phys),intent(in):: water_depth,z0,ust
 real(kind=kind_phys):: depth_b
 real(kind=kind_phys):: effective_depth
 if(water_depth .lt. 10.0) then
    effective_depth = 10.0
 elseif(water_depth .gt. 100.0) then
    effective_depth = 100.0
 else
    effective_depth = water_depth
 endif
 
 depth_b = 1 / 30.0 * log (1260.0 / effective_depth)
 depth_dependent_z0 = exp((2.7 * ust - 1.8 / depth_b) / (ust + 0.17 / depth_b) )
 depth_dependent_z0 = MIN(depth_dependent_z0,0.1)

 return
 end function depth_dependent_z0

!=================================================================================================================
 end module sf_sfclayrev
!=================================================================================================================
