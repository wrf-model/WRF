! module cup_gf_sh will call shallow convection as described in Grell and
! Freitas (2016). Input variables are:
!    zo               Height at model levels
!    t,tn             Temperature without and with forcing at model levels
!    q,qo             mixing ratio without and with forcing at model levels
!    po               pressure at model levels (mb)
!    psur             surface pressure (mb)
!    z1               surface height
!    dhdt             forcing for boundary layer equilibrium   
!    hfx,qfx          in w/m2 (positive, if upward from sfc)
!    kpbl             level of boundaty layer height
!    xland            land mask (1. for land)
!    ichoice          which closure to choose 
!                     1: old g
!                     2: zws
!                     3: dhdt
!                     0: average
!    tcrit            parameter for water/ice conversion (258)
!
!!!!!!!!!!!! Variables that are diagnostic
!
!    zuo               normalized mass flux profile
!    xmb_out           base mass flux
!    kbcon             convective cloud base
!    ktop              cloud top
!    k22               level of updraft originating air
!    ierr              error flag
!    ierrc             error description
!
!!!!!!!!!!!! Variables that are on output
!    outt               temperature tendency (K/s)
!    outq               mixing ratio tendency (kg/kg/s)
!    outqc              cloud water/ice tendency (kg/kg/s)
!    pre                precip rate (mm/s)
!    cupclw             incloud mixing ratio of cloudwater/ice (for radiation)
!                       this needs heavy tuning factors, since cloud fraction is
!                       not included (kg/kg)
!    cnvwt              required for GFS physics
!
!    itf,ktf,its,ite, kts,kte are dimensions
!    ztexec,zqexec    excess temperature and moisture for updraft
MODULE module_cu_gf_sh
    real, parameter:: c1_shal=0.! .0005
    real, parameter:: g  =9.81
    real, parameter:: cp =1004.
    real, parameter:: xlv=2.5e6
    real, parameter:: r_v=461.
    real, parameter:: c0_shal=.001
    real, parameter:: fluxtune=1.5


contains
  SUBROUTINE CUP_gf_sh (                                              &
! input variables, must be supplied
                         zo,T,Q,Z1,TN,QO,PO,PSUR,dhdt,kpbl,rho,     &
                         hfx,qfx,xland,ichoice,tcrit,dtime, &
! input variables. Ierr should be initialized to zero or larger than zero for
! turning off shallow convection for grid points
                         zuo,xmb_out,kbcon,ktop,k22,ierr,ierrc,    &
! output tendencies
                         OUTT,OUTQ,OUTQC,cnvwt,pre,cupclw,             &
! dimesnional variables
                         itf,ktf,its,ite, kts,kte,ipr)
!
! this module needs some subroutines from gf_deep
!
  use module_cu_gf_deep,only:cup_env,cup_env_clev,get_cloud_bc,cup_minimi,  &
                      get_inversion_layers,rates_up_pdf,get_cloud_bc,     &
                      cup_up_aa0,cup_kbcon,get_lateral_massflux
     implicit none
     integer                                                           &
        ,intent (in   )                   ::                           &
        itf,ktf,        &
        its,ite, kts,kte,ipr
     logical :: MAKE_CALC_FOR_XK = .true.
     integer, intent (in   )              ::                           &
        ichoice
  !
  ! 
  !
  ! outtem = output temp tendency (per s)
  ! outq   = output q tendency (per s)
  ! outqc  = output qc tendency (per s)
  ! pre    = output precip
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout  )                   ::                           &
        cnvwt,OUTT,OUTQ,OUTQC,cupclw,zuo
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        xmb_out
     integer,    dimension (its:ite)                                   &
        ,intent (inout  )                   ::                           &
        ierr
     integer,    dimension (its:ite)                                   &
        ,intent (out  )                   ::                           &
        kbcon,ktop,k22
     integer,    dimension (its:ite)                                   &
        ,intent (in  )                   ::                           &
        kpbl
  !
  ! basic environmental input includes a flag (ierr) to turn off
  ! convection for this call only and at that particular gridpoint
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        T,PO,tn,dhdt,rho
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
         Q,QO
     real, dimension (its:ite)                                         &
        ,intent (in   )                   ::                           &
        xland,Z1,PSUR,hfx,qfx
       
       real                                                            &
        ,intent (in   )                   ::                           &
        dtime,tcrit
  !
  !***************** the following are your basic environmental
  !                  variables. They carry a "_cup" if they are
  !                  on model cloud levels (staggered). They carry
  !                  an "o"-ending (z becomes zo), if they are the forced
  !                  variables. 
  !
  ! z           = heights of model levels
  ! q           = environmental mixing ratio
  ! qes         = environmental saturation mixing ratio
  ! t           = environmental temp
  ! p           = environmental pressure
  ! he          = environmental moist static energy
  ! hes         = environmental saturation moist static energy
  ! z_cup       = heights of model cloud levels
  ! q_cup       = environmental q on model cloud levels
  ! qes_cup     = saturation q on model cloud levels
  ! t_cup       = temperature (Kelvin) on model cloud levels
  ! p_cup       = environmental pressure
  ! he_cup = moist static energy on model cloud levels
  ! hes_cup = saturation moist static energy on model cloud levels
  ! gamma_cup = gamma on model cloud levels
  ! dby = buoancy term
  ! entr = entrainment rate
  ! bu = buoancy term
  ! gamma_cup = gamma on model cloud levels
  ! qrch = saturation q in cloud
  ! pwev = total normalized integrated evaoprate (I2)
  ! z1 = terrain elevation
  ! psur        = surface pressure
  ! zu      = updraft normalized mass flux
  ! kbcon       = LFC of parcel from k22
  ! k22         = updraft originating level
  ! ichoice       = flag if only want one closure (usually set to zero!)
  ! dby = buoancy term
  ! ktop = cloud top (output)
  ! xmb    = total base mass flux
  ! hc = cloud moist static energy
  ! hkb = moist static energy at originating level

     real,    dimension (its:ite,kts:kte) ::                           &
        entr_rate_2d,he,hes,qes,z,                      &
        heo,heso,qeso,zo,                                              &
        xhe,xhes,xqes,xz,xt,xq,                                        &
        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,      &
        qeso_cup,qo_cup,heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,     &
        tn_cup,                                                        &
        xqes_cup,xq_cup,xhe_cup,xhes_cup,xz_cup,     &
        xt_cup,dby,hc,zu,   &
        dbyo,qco,pwo,hco,qrco,     &
        dbyt,xdby,xhc,xzu,            &

  ! cd  = detrainment function for updraft
  ! dellat = change of temperature per unit mass flux of cloud ensemble
  ! dellaq = change of q per unit mass flux of cloud ensemble
  ! dellaqc = change of qc per unit mass flux of cloud ensemble

        cd,DELLAH,DELLAQ,DELLAT,DELLAQC

  ! aa0 cloud work function for downdraft
  ! aa0     = cloud work function without forcing effects
  ! aa1     = cloud work function with forcing effects
  ! xaa0    = cloud work function with cloud effects (ensemble dependent)

     real,    dimension (its:ite) ::                                   &
       zws,ztexec,zqexec,pre,AA1,AA0,XAA0,HKB,                          &
       flux_tun,HKBO,XHKB,                                    &
       rand_vmas,xmbmax,XMB,                         &
       cap_max,entr_rate,                                    &
       cap_max_increment
     integer,    dimension (its:ite) ::                                &
       kstabi,xland1,KBMAX,ktopx

     integer                              ::                           &
       I,K,ki
     real                                 ::                           &
      dz,mbdt,zkbmax,      &
      cap_maxs,trash,trash2,frh
      
      real buo_flux,pgeoh,dp,entup,detup,totmas

     real xff_shal(3),blqe,xkshal
     character*50 :: ierrc(its:ite)
     real,    dimension (its:ite,kts:kte) ::                           &
       up_massentr,up_massdetr,up_massentro,up_massdetro
     real :: C_up,x_add,qaver
     real,    dimension (its:ite,kts:kte) :: dtempdz
     integer, dimension (its:ite,kts:kte) ::  k_inv_layers 
     integer, dimension (its:ite) ::  start_level
     start_level(:)=0
     rand_vmas(:)=0.
     flux_tun=fluxtune
      do i=its,itf
        xland1(i)=int(xland(i)+.001) ! 1.
        ktopx(i)=0
        if(xland(i).gt.1.5 .or. xland(i).lt.0.5)then
            xland1(i)=0
!            ierr(i)=100
        endif
        pre(i)=0.
        xmb_out(i)=0.
        cap_max_increment(i)=25.
        ierrc(i)=" "
        entr_rate(i) = 9.e-5 ! 1.75e-3 ! 1.2e-3 ! .2/50.
      enddo
!
!--- initial entrainment rate (these may be changed later on in the
!--- program
!
      
!
!--- initial detrainmentrates
!
      do k=kts,ktf
      do i=its,itf
        up_massentro(i,k)=0.
        up_massdetro(i,k)=0.
        z(i,k)=zo(i,k)
        xz(i,k)=zo(i,k)
        qrco(i,k)=0.
        pwo(i,k)=0.
        cd(i,k)=1.*entr_rate(i)
        dellaqc(i,k)=0.
        cupclw(i,k)=0.
      enddo
      enddo
!
!--- max/min allowed value for epsilon (ratio downdraft base mass flux/updraft
!
!--- minimum depth (m), clouds must have
!
!
!--- maximum depth (mb) of capping 
!--- inversion (larger cap = no convection)
!
      cap_maxs=125.
      DO i=its,itf
        kbmax(i)=1
        aa0(i)=0.
        aa1(i)=0.
      enddo
      do i=its,itf
          cap_max(i)=cap_maxs
          ztexec(i)  = 0.
          zqexec(i)  = 0.
          zws(i)     = 0.
      enddo
      do i=its,itf
         !- buoyancy flux (H+LE)
         buo_flux= (hfx(i)/cp+0.608*t(i,1)*qfx(i)/xlv)/rho(i,1)
         pgeoh = zo(i,2)*g
         !-convective-scale velocity w*
         zws(i) = max(0.,flux_tun(i)*0.41*buo_flux*zo(i,2)*g/t(i,1))
         if(zws(i) > TINY(pgeoh)) then
          !-convective-scale velocity w*
          zws(i) = 1.2*zws(i)**.3333
          !- temperature excess 
          ztexec(i)     = MAX(flux_tun(i)*hfx(i)/(rho(i,1)*zws(i)*cp),0.0)
          !- moisture  excess
          zqexec(i)     = MAX(flux_tun(i)*qfx(i)/xlv/(rho(i,1)*zws(i)),0.)
         endif
       !- zws for shallow convection closure (Grant 2001)
       !- height of the pbl
       zws(i) = max(0.,flux_tun(i)*0.41*buo_flux*zo(i,kpbl(i))*g/t(i,kpbl(i)))
       zws(i) = 1.2*zws(i)**.3333
       zws(i) = zws(i)*rho(i,kpbl(i)) !check if zrho is correct

      enddo

!
!--- max height(m) above ground where updraft air can originate
!
      zkbmax=3000.
!
!--- calculate moist static energy, heights, qes
!
      call cup_env(z,qes,he,hes,t,q,po,z1, &
           psur,ierr,tcrit,-1,   &
           itf,ktf, &
           its,ite, kts,kte)
      call cup_env(zo,qeso,heo,heso,tn,qo,po,z1, &
           psur,ierr,tcrit,-1,   &
           itf,ktf, &
           its,ite, kts,kte)

!
!--- environmental values on cloud levels
!
      call cup_env_clev(t,qes,q,he,hes,z,po,qes_cup,q_cup,he_cup, &
           hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
           ierr,z1,          &
           itf,ktf, &
           its,ite, kts,kte)
      call cup_env_clev(tn,qeso,qo,heo,heso,zo,po,qeso_cup,qo_cup, &
           heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,tn_cup,psur,  &
           ierr,z1,          &
           itf,ktf, &
           its,ite, kts,kte)
      do i=its,itf
        if(ierr(i).eq.0)then
!
      do k=kts,ktf
        if(zo_cup(i,k).gt.zkbmax+z1(i))then
          kbmax(i)=k
          go to 25
        endif
      enddo
 25   continue
!
      kbmax(i)=min(kbmax(i),ktf/2)
      endif
      enddo

!
!
!
!------- DETERMINE LEVEL WITH HIGHEST MOIST STATIC ENERGY CONTENT - K22
!
       DO 36 i=its,itf
         if(kpbl(i).gt.3)cap_max(i)=po_cup(i,kpbl(i))
         IF(ierr(I) == 0)THEN
          k22(i)=maxloc(HEO_CUP(i,2:kbmax(i)),1)
          k22(i)=max(2,k22(i))
          IF(K22(I).GT.KBMAX(i))then
           ierr(i)=2
           ierrc(i)="could not find k22"
           ktop(i)=0
           k22(i)=0
           kbcon(i)=0
         endif
         endif
 36   CONTINUE
!
!--- DETERMINE THE LEVEL OF CONVECTIVE CLOUD BASE  - KBCON
!
      do i=its,itf
       if(ierr(I).eq.0)then
             x_add = xlv*zqexec(i)+cp*ztexec(i)
             call get_cloud_bc(kte,he_cup (i,1:kte),hkb (i),k22(i),x_add)
             call get_cloud_bc(kte,heo_cup(i,1:kte),hkbo(i),k22(i),x_add)
       endif ! ierr
      enddo

!JOE-Georg and Saulo's new idea:
      do i=its,itf
      do k=kts,ktf
          dbyo(i,k)= 0. !hkbo(i)-heso_cup(i,k)
      enddo
      enddo

      call cup_kbcon(ierrc,cap_max_increment,5,k22,kbcon,heo_cup,heso_cup, &
           hkbo,ierr,kbmax,po_cup,cap_max, &
           ztexec,zqexec, &
           0,itf,ktf, &
           its,ite, kts,kte, &
           z_cup,entr_rate,heo,0)
!--- get inversion layers for cloud tops
      call cup_minimi(HEso_cup,Kbcon,kbmax,kstabi,ierr,  &
           itf,ktf, &
           its,ite, kts,kte)
!
      call get_inversion_layers(ierr,p_cup,t_cup,z_cup,q_cup,qes_cup,k_inv_layers,&
                           kbcon,kstabi,dtempdz,itf,ktf,its,ite, kts,kte)
!
!
      DO i=its,itf
         entr_rate_2d(i,:)=entr_rate(i)
         IF(ierr(I) == 0)THEN
            start_level(i)=k22(i)
            x_add = xlv*zqexec(i)+cp*ztexec(i)
            call get_cloud_bc(kte,he_cup (i,1:kte),hkb (i),k22(i),x_add)
            if(kbcon(i).gt.ktf-4)then
                ierr(i)=231
            endif
            do k=kts,ktf
               frh = 2.*min(qo_cup(i,k)/qeso_cup(i,k),1.)
               entr_rate_2d(i,k)=entr_rate(i)*(2.3-frh)
               cd(i,k)=entr_rate_2d(i,k)
            enddo
!
! first estimate for shallow convection
!
            ktop(i)=1
!            if(k_inv_layers(i,1).gt.0)then
!!               ktop(i)=min(k_inv_layers(i,1),k_inv_layers(i,2))
            if(k_inv_layers(i,1).gt.0 .and.   &
               (po_cup(i,kbcon(i))-po_cup(i,k_inv_layers(i,1))).lt.200.)then
               ktop(i)=k_inv_layers(i,1)
            else
               do k=kbcon(i)+1,ktf
                  if((po_cup(i,kbcon(i))-po_cup(i,k)).gt.200.)then
                    ktop(i)=k
                    exit
                  endif
               enddo
            endif
         endif
      enddo
! get normalized mass flux profile
      call rates_up_pdf(rand_vmas,ipr,'shallow',ktop,ierr,po_cup,entr_rate_2d,hkbo,heo,heso_cup,zo_cup, &
           xland1,kstabi,k22,kbcon,its,ite,itf,kts,kte,ktf,zuo,kpbl,ktopx,ktopx,kbcon)
      do i=its,itf
        if(ierr(i).eq.0)then
!           do k=maxloc(zuo(i,:),1),1,-1 ! ktop(i)-1,1,-1
!             if(zuo(i,k).lt.1.e-6)then
!               k22(i)=k+1
!               start_level(i)=k22(i)
!               exit
!             endif
!           enddo
           if(k22(i).gt.1)then
             do k=1,k22(i)-1
              zuo(i,k)=0.
              zu (i,k)=0.
              xzu(i,k)=0.
             enddo
           endif
           do k=maxloc(zuo(i,:),1),ktop(i)
             if(zuo(i,k).lt.1.e-6)then
               ktop(i)=k-1
               exit
             endif
           enddo
           do k=k22(i),ktop(i)
             xzu(i,k)= zuo(i,k)
              zu(i,k)= zuo(i,k)
           enddo
           do k=ktop(i)+1,ktf
             zuo(i,k)=0.
             zu (i,k)=0.
             xzu(i,k)=0.
           enddo
           k22(i)=max(2,k22(i))
        endif
      enddo
!
! calculate mass entrainment and detrainment
!
      CALL get_lateral_massflux(itf,ktf, its,ite, kts,kte &
                                ,ierr,ktop,zo_cup,zuo,cd,entr_rate_2d        &
                                ,up_massentro, up_massdetro ,up_massentr, up_massdetr &
                                ,'shallow',kbcon,k22)

      do k=kts,ktf
      do i=its,itf
         hc(i,k)=0.
         qco(i,k)=0.
         qrco(i,k)=0.
         DBY(I,K)=0.
         hco(i,k)=0.
         DBYo(I,K)=0.
      enddo
      enddo
      do i=its,itf
       IF(ierr(I) /= 0) cycle
         do k=1,start_level(i)-1
            hc(i,k)=he_cup(i,k)
            hco(i,k)=heo_cup(i,k)
         enddo
         k=start_level(i)
         hc(i,k)=hkb(i)
         hco(i,k)=hkbo(i)
      enddo
!
!
      do 42 i=its,itf
        dbyt(i,:)=0.
        IF(ierr(I) /= 0) cycle
         do k=start_level(i)+1,ktop(i)
          hc(i,k)=(hc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)*hc(i,k-1)+ &
                         up_massentr(i,k-1)*he(i,k-1))   /            &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
          dby(i,k)=max(0.,hc(i,k)-hes_cup(i,k))
          hco(i,k)=(hco(i,k-1)*zuo(i,k-1)-.5*up_massdetro(i,k-1)*hco(i,k-1)+ &
                         up_massentro(i,k-1)*heo(i,k-1))   /            &
                         (zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
          dbyo(i,k)=hco(i,k)-heso_cup(i,k)
          DZ=Zo_cup(i,K+1)-Zo_cup(i,K)
          dbyt(i,k)=dbyt(i,k-1)+dbyo(i,k)*dz
         enddo
       ki=maxloc(dbyt(i,:),1)
       if(ktop(i).gt.ki+1)then
         ktop(i)=ki+1
         zuo(i,ktop(i)+1:ktf)=0.
         zu(i,ktop(i)+1:ktf)=0.
         cd(i,ktop(i)+1:ktf)=0.
         up_massdetro(i,ktop(i))=zuo(i,ktop(i))
!         up_massentro(i,ktop(i))=0.
         up_massentro(i,ktop(i):ktf)=0.
         up_massdetro(i,ktop(i)+1:ktf)=0.
         entr_rate_2d(i,ktop(i)+1:ktf)=0.

!         ierr(i)=423
       endif

         if(ktop(i).lt.kbcon(i)+1)then
            ierr(i)=5
            ierrc(i)='ktop is less than kbcon+1'
             go to 42
         endif
         if(ktop(i).gt.ktf-2)then
             ierr(i)=5
             ierrc(i)="ktop is larger than ktf-2"
             go to 42
         endif
!
         call get_cloud_bc(kte,qo_cup (i,1:kte),qaver,k22(i))
         qaver = qaver + zqexec(i)
         do k=1,start_level(i)-1
           qco (i,k)= qo_cup(i,k)
         enddo
         k=start_level(i)
         qco (i,k)= qaver 
!
         do k=start_level(i)+1,ktop(i)
          trash=QESo_cup(I,K)+(1./XLV)*(GAMMAo_cup(i,k) &
                /(1.+GAMMAo_cup(i,k)))*DBYo(I,K)
          !- total water liq+vapour
          trash2  = qco(i,k-1) ! +qrco(i,k-1)
          qco (i,k)=   (trash2* ( zuo(i,k-1)-0.5*up_massdetr(i,k-1)) + &
                       up_massentr(i,k-1)*qo(i,k-1))   /            &
                       (zuo(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))

          if(qco(i,k)>=trash ) then 
              DZ=Z_cup(i,K)-Z_cup(i,K-1)
              ! cloud liquid water
              qrco(i,k)= (qco(i,k)-trash)/(1.+(c0_shal+c1_shal)*dz)
!              qrco(i,k)= (qco(i,k)-trash)/(1.+c0_shal*dz)
              pwo(i,k)=c0_shal*dz*qrco(i,k)*zuo(i,k)
              ! cloud water vapor 
              qco (i,k)= trash+qrco(i,k)
        
          else
              qrco(i,k)= 0.0
          endif 
          cupclw(i,k)=qrco(i,k)
         enddo
         trash=0.
         trash2=0.
         do k=k22(i)+1,ktop(i)
          dp=100.*(po_cup(i,k)-po_cup(i,k+1))
          cnvwt(i,k)=zuo(i,k)*cupclw(i,k)*g/dp
          trash2=trash2+entr_rate_2d(i,k)
          qco(i,k)=qco(i,k)-qrco(i,k)
         enddo
         do k=k22(i)+1,max(kbcon(i),k22(i)+1)
          trash=trash+entr_rate_2d(i,k)
         enddo
         do k=ktop(i)+1,ktf-1
           hc  (i,k)=hes_cup (i,k)
           hco (i,k)=heso_cup(i,k)
           qco (i,k)=qeso_cup(i,k)
           qrco(i,k)=0.
           dby (i,k)=0.
           dbyo(i,k)=0.
           zu  (i,k)=0.
           xzu (i,k)=0.
           zuo (i,k)=0.
         enddo
 42 continue
!
!--- calculate workfunctions for updrafts
!
      IF(MAKE_CALC_FOR_XK) THEN
        call cup_up_aa0(aa0,z,zu,dby,GAMMA_CUP,t_cup, &
            kbcon,ktop,ierr,           &
            itf,ktf, its,ite, kts,kte)
        call cup_up_aa0(aa1,zo,zuo,dbyo,GAMMAo_CUP,tn_cup, &
            kbcon,ktop,ierr,           &
            itf,ktf, its,ite, kts,kte)
        do i=its,itf
          if(ierr(i) == 0)then
           if(aa1(i) <= 0.)then
               ierr(i)=17
               ierrc(i)="cloud work function zero"
           endif
         endif
       enddo
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
!--- change per unit mass that a model cloud would modify the environment
!
!--- 1. in bottom layer
!
      do k=kts,kte
       do i=its,itf
        dellah(i,k)=0.
        dellaq(i,k)=0.
       enddo
      enddo
!
!----------------------------------------------  cloud level ktop
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level ktop-1
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!
!----------------------------------------------  cloud level k+2
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level k+1
!
!----------------------------------------------  cloud level k+1
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level k
!
!----------------------------------------------  cloud level k
!
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!
!----------------------------------------------  cloud level 3
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level 2
!
!----------------------------------------------  cloud level 2
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level 1
      trash2=0.
      do i=its,itf
        if(ierr(i).eq.0)then
         do k=k22(i),ktop(i)
            ! entrainment/detrainment for updraft
            entup=up_massentro(i,k)
            detup=up_massdetro(i,k)
            totmas=detup-entup+zuo(i,k+1)-zuo(i,k)
            if(abs(totmas).gt.1.e-6)then
               write(0,*)'*********************',i,k,totmas
               write(0,*)k22(i),kbcon(i),ktop(i)
            endif
            dp=100.*(po_cup(i,k)-po_cup(i,k+1))
            dellah(i,k) =-(zuo(i,k+1)*(hco(i,k+1)-heo_cup(i,k+1) )-     &
                           zuo(i,k  )*(hco(i,k  )-heo_cup(i,k  ) ))*g/dp

            !-- take out cloud liquid water for detrainment
            dz=zo_cup(i,k+1)-zo_cup(i,k)
            if(k.lt.ktop(i))then
             dellaqc(i,k)= zuo(i,k)*c1_shal*qrco(i,k)*dz/dp*g !  detup*0.5*(qrco(i,k+1)+qrco(i,k)) *g/dp
            else
             dellaqc(i,k)=   detup*qrco(i,k) *g/dp
            endif

            !-- condensation source term = detrained + flux divergence of 
            !-- cloud liquid water (qrco)
            C_up = dellaqc(i,k)+(zuo(i,k+1)* qrco(i,k+1) -       &
                                  zuo(i,k  )* qrco(i,k  )  )*g/dp
!            C_up = dellaqc(i,k)
            !-- water vapor budget (flux divergence of Q_up-Q_env - condensation
            !term)
            dellaq(i,k) =-(zuo(i,k+1)*(qco(i,k+1)-qo_cup(i,k+1) ) -      &
                           zuo(i,k  )*(qco(i,k  )-qo_cup(i,k  ) ) )*g/dp &
                           - C_up - 0.5*(pwo (i,k)+pwo (i,k+1))*g/dp
          enddo
        endif
      enddo

!
!--- using dellas, calculate changed environmental profiles
!
      mbdt=.5 !3.e-4

      do k=kts,ktf
       do i=its,itf
         dellat(i,k)=0.
         if(ierr(i)/=0)cycle
         xhe(i,k)=dellah(i,k)*mbdt+heo(i,k)
         xq (i,k)=max(1.e-16,(dellaq(i,k)+dellaqc(i,k))*mbdt+qo(i,k))
         dellat(i,k)=(1./cp)*(dellah(i,k)-xlv*(dellaq(i,k)))
         xt (i,k)= (-dellaqc(i,k)*xlv/cp+dellat(i,k))*mbdt+tn(i,k)
         xt (i,k)=  max(190.,xt(i,k))
         
       enddo
      enddo
      do i=its,itf
       if(ierr(i).eq.0)then
!        xhkb(i)=hkbo(i)+(dellah(i,k22(i)))*mbdt
        xhe(i,ktf)=heo(i,ktf)
        xq(i,ktf)=qo(i,ktf)
        xt(i,ktf)=tn(i,ktf)
       endif
      enddo
!
!
     IF(MAKE_CALC_FOR_XK) THEN
!
!--- calculate moist static energy, heights, qes
!
      call cup_env(xz,xqes,xhe,xhes,xt,xq,po,z1, &
           psur,ierr,tcrit,-1,   &
           itf,ktf, &
           its,ite, kts,kte)
!
!--- environmental values on cloud levels
!
      call cup_env_clev(xt,xqes,xq,xhe,xhes,xz,po,xqes_cup,xq_cup, &
           xhe_cup,xhes_cup,xz_cup,po_cup,gamma_cup,xt_cup,psur,   &
           ierr,z1,          &
           itf,ktf, &
           its,ite, kts,kte)
!
!
!**************************** static control
      do k=kts,ktf
      do i=its,itf
         xhc(i,k)=0.
         xDBY(I,K)=0.
      enddo
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
         x_add = xlv*zqexec(i)+cp*ztexec(i)
         call get_cloud_bc(kte,xhe_cup (i,1:kte),xhkb (i),k22(i),x_add)
         do k=1,start_level(i)-1
            xhc(i,k)=xhe_cup(i,k)
         enddo
         k=start_level(i)
         xhc(i,k)=xhkb(i)
        endif !ierr
      enddo
!
!
      do i=its,itf
       if(ierr(i).eq.0)then
        xzu(i,1:ktf)=zuo(i,1:ktf)	
        do k=start_level(i)+1,ktop(i)
         xhc(i,k)=(xhc(i,k-1)*xzu(i,k-1)-.5*up_massdetro(i,k-1)*xhc(i,k-1)+ &
                          up_massentro(i,k-1)*xhe(i,k-1))   /            &
                          (xzu(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
         xdby(i,k)=xhc(i,k)-xhes_cup(i,k)
        enddo
        do k=ktop(i)+1,ktf
           xHC (i,K)=xhes_cup(i,k)
           xDBY(I,K)=0.
           xzu (i,k)=0.
        enddo
       endif
      enddo

!
!--- workfunctions for updraft
!
      call cup_up_aa0(xaa0,xz,xzu,xdby,GAMMA_CUP,xt_cup, &
           kbcon,ktop,ierr,           &
           itf,ktf, &
           its,ite, kts,kte)
!
     ENDIF
!
!
! now for shallow forcing
!
       do i=its,itf
        xmb(i)=0.
        xff_shal(1:3)=0.
        if(ierr(i).eq.0)then
          xmbmax(i)=1.0  
!         xmbmax(i)=100.*(p(i,kbcon(i))-p(i,kbcon(i)+1))/(g*dtime)
!
!-stabilization closure
          xkshal=(xaa0(i)-aa1(i))/mbdt
             if(xkshal.le.0.and.xkshal.gt.-.01*mbdt) &
                           xkshal=-.01*mbdt
             if(xkshal.gt.0.and.xkshal.lt.1.e-2) &
                           xkshal=1.e-2

          xff_shal(1)=max(0.,-(aa1(i)-aa0(i))/(xkshal*dtime))
!
!- closure from Grant (2001)
          xff_shal(2)=.03*zws(i)
!- boundary layer QE closure
          blqe=0.
          trash=0.
          do k=1,kpbl(i)
                blqe=blqe+100.*dhdt(i,k)*(po_cup(i,k)-po_cup(i,k+1))/g
          enddo
          trash=max((hc(i,kbcon(i))-he_cup(i,kbcon(i))),1.e1)
          xff_shal(3)=max(0.,blqe/trash)
          xff_shal(3)=min(xmbmax(i),xff_shal(3))
!- average 
          xmb(i)=(xff_shal(1)+xff_shal(2)+xff_shal(3))/3.
          xmb(i)=min(xmbmax(i),xmb(i))
          if(ichoice > 0)xmb(i)=min(xmbmax(i),xff_shal(ichoice))
          if(xmb(i) <= 0.)then
             ierr(i)=21
             ierrc(i)="21"
          endif
        endif
        if(ierr(i).ne.0)then
           k22  (i)=0
           kbcon(i)=0
           ktop (i)=0
           xmb  (i)=0.
           outt (i,:)=0.
           outq (i,:)=0.
           outqc(i,:)=0.
        else if(ierr(i).eq.0)then
          xmb_out(i)=xmb(i)
! 
! final tendencies
!
          pre(i)=0.
          do k=2,ktop(i)
           outt (i,k)= dellat (i,k)*xmb(i)
           outq (i,k)= dellaq (i,k)*xmb(i)
           outqc(i,k)= dellaqc(i,k)*xmb(i)
           pre  (i)  = pre(i)+pwo(i,k)*xmb(i)
          enddo
        endif
       enddo
!      
! done shallow
!--------------------------done------------------------------
!

   END SUBROUTINE CUP_gf_sh
END MODULE module_cu_gf_sh
