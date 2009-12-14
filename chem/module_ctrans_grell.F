!WRF:MODEL_LAYER:PHYSICS
!

MODULE module_ctrans_grell
USE module_cu_gd
USE module_dep_simple
USE module_state_description, only:p_co,p_qv,p_so2,p_o3,p_sulf,p_nh3,p_h2o2, &
                              p_paa,p_hno3,p_n2o5,p_so4aj,p_nh4aj,p_no3aj

CONTAINS

!-------------------------------------------------------------
   SUBROUTINE GRELLDRVCT(DT,itimestep,DX,                       &
              rho_phy,RAINCV,chem,              &
              U,V,t_phy,moist,dz8w,p_phy,                       &
              XLV,CP,G,r_v,z,cu_co_ten,                         &
              num_moist,numgas,num_chem,chemopt,scalaropt,      &
              ids,ide, jds,jde, kds,kde,                        &
              ims,ime, jms,jme, kms,kme,                        &
              its,ite, jts,jte, kts,kte                         )
! USE module_configure
! USE module_state_description
!-------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------
   INTEGER,      INTENT(IN   ) ::                               &
                                  numgas,chemopt,scalaropt, &
                                  ids,ide, jds,jde, kds,kde,    & 
                                  ims,ime, jms,jme, kms,kme,    & 
                                  its,ite, jts,jte, kts,kte,    &
                                  num_chem,num_moist

  
   INTEGER,      INTENT(IN   ) :: ITIMESTEP

   REAL,         INTENT(IN   ) :: XLV, R_v
   REAL,         INTENT(IN   ) :: CP,G

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme,num_moist )         ,    &
          INTENT(IN   ) ::                              moist 
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                          U,    &
                                                          V,    &
                                                      t_phy,    &
                                                      z,        &
                                                      p_phy,    &
                                                       dz8w,    &
                                                    rho_phy
!
! on output for control only, purely diagnostic
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(INOUT   ) ::                                   &
                                                    cu_co_ten


!
   REAL, INTENT(IN   ) :: DT, DX
!
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme, num_chem ),    &
         INTENT(INOUT) ::                                       &
                                   chem
                           

   REAL, DIMENSION( ims:ime , jms:jme ),                        &
         INTENT(IN) ::                 RAINCV

! LOCAL VARS
     real,    dimension (its:ite,kts:kte) ::                    &
        OUTT,OUTQ,OUTQC
     real,    dimension (its:ite)         ::                    &
        pret, ter11

!
! basic environmental input includes moisture convergence (mconv)
! omega (omeg), windspeed (us,vs), and a flag (aaeq) to turn off
! convection for this call only and at that particular gridpoint
!
     real,    dimension (its:ite,kts:kte) ::                    &
        T,TN,q,qo,PO,P,US,VS,hstary
     real,    dimension (its:ite,kts:kte,num_chem) ::                    &
           tracer,tracert
     real, dimension (its:ite)            ::                    &
        Z1,PSUR,AAEQ
     integer, dimension (its:ite)            ::                    &
        ktop

   INTEGER :: nv,i,j,k,ICLDCK,ipr,jpr,npr
   REAL    :: tcrit,dp,dq,epsilc
   INTEGER :: itf,jtf,ktf,iopt
   epsilc=1.e-30
!  return
!  ipr=111
!  jpr=40
!  if(itimestep.lt.34.or.itimestep.gt.36)ipr=0
!  if(itimestep.lt.34.or.itimestep.gt.36)jpr=0
!  ipr=61
!  jpr=60
   ipr=0
   jpr=0
   npr=1
   if(p_co.gt.1)npr=p_co
   tcrit=258.
   iopt=0
   itf=MIN(ite,ide-1)
   ktf=MIN(kte,kde-1)
   jtf=MIN(jte,jde-1)
!                                                                      
!                                                                      
123  continue
     DO 100 J = jts,jtf  
     if(j.eq.jpr)print *,'dt = ',dt
     DO I=ITS,ITF
         ktop(i)=0
         PSUR(I)=p_phy(I,kts,J)*.01
         TER11(I)=z(i,kts,j)
         aaeq(i)=0.
!
!   rainrate is input for this transport/wet-deposition routine
!
         pret(i)=raincv(i,j)/dt
         if(pret(i).le.0.)aaeq(i)=20.
     ENDDO
     DO K=kts,ktf
     DO I=ITS,ITF
         po(i,k)=p_phy(i,k,j)*.01
         P(I,K)=PO(i,k)
         US(I,K) =u(i,k,j)
         VS(I,K) =v(i,k,j)
         T(I,K)=t_phy(i,k,j)
         q(I,K)=moist(i,k,j,p_qv)
         IF(Q(I,K).LT.1.E-08)Q(I,K)=1.E-08
     ENDDO
     ENDDO
     do nv=2,num_chem
     DO K=kts,ktf
     DO I=ITS,ITF
         tracer(i,k,nv)=max(epsilc,chem(i,k,j,nv))
         tracert(i,k,nv)=0.
     ENDDO
     ENDDO
     ENDDO
     DO K=kts,ktf
     DO I=ITS,ITF
         cu_co_ten(i,k,j)=0.
!        hstary(i,k)=hstar4(nv)*exp(dhr(nv)*(1./t(i,k)-1./298.))
         if(i.eq.ipr.and.j.eq.jpr)then
          print *,k,pret(i),tracer(i,k,npr),p(i,k),z(i,k,j)
         endif
     ENDDO
     ENDDO
!    ENDDO
!
!---- CALL NON_RESOLVED CONVECTIVE TRANSPORT
!
      CALL CUP_ct(ktop,tracer,j,AAEQ,T,Q,TER11,PRET,P,tracert, &
           hstary,DT,PSUR,US,VS,tcrit,                         &
           xlv,r_v,cp,g,ipr,jpr,npr,num_chem,chemopt,scalaropt,&
           numgas,ids,ide, jds,jde, kds,kde,                   &
           ims,ime, jms,jme, kms,kme,                          &
           its,ite, jts,jte, kts,kte                           )

            do nv=2,num_chem
            DO I=its,itf
              if(pret(i).le.0.)then
                 DO K=kts,ktf
                   tracert(i,k,nv)=0.
                 ENDDO
              endif
             enddo
             enddo
      CALL neg_check_ct(pret,ktop,epsilc,dt,tracer,tracert,iopt,num_chem,   &
                        its,ite,kts,kte,itf,ktf,ipr,jpr,npr,j)
       do nv=2,num_chem
            DO I=its,itf
              if(pret(i).gt.0.)then
                 DO K=kts,ktf
                   chem(i,k,j,nv)=max(epsilc,chem(i,k,j,nv)+tracert(i,k,nv)*dt)
                   if(nv.eq.npr)then
                        cu_co_ten(i,k,j)=tracert(i,k,npr)*dt
                        if(i.eq.ipr.and.j.eq.jpr)print *,k,chem(i,k,j,nv),cu_co_ten(i,k,j)
                   endif
                 ENDDO
              else
                 DO K=kts,ktf
                   tracert(i,k,nv)=0.
                   if(nv.eq.npr)cu_co_ten(i,k,j)=0.
                 enddo
              endif
            ENDDO
       ENDDO


 100    continue

   END SUBROUTINE GRELLDRVCT


   SUBROUTINE CUP_ct(ktop,tracer,J,AAEQ,T,Q,Z1,                        &
              PRE,P,tracert,hstary,DTIME,PSUR,US,VS,TCRIT,             &
              xl,rv,cp,g,ipr,jpr,npr,num_chem,chemopt,scalaropt,       &
              numgas,ids,ide, jds,jde, kds,kde,                        &
              ims,ime, jms,jme, kms,kme,                               &
              its,ite, jts,jte, kts,kte                                )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        num_chem,ids,ide, jds,jde, kds,kde,                            &
        ims,ime, jms,jme, kms,kme,scalaropt,                           &
        its,ite, jts,jte, kts,kte,ipr,jpr,npr,chemopt,numgas
     integer, intent (in   )              ::                           &
        j
  !
  ! 
  !
  !tracert = output temp tendency (per s)
  ! pre    = input precip
     real,    dimension (its:ite,kts:kte,num_chem)                              &
        ,intent (inout  )                   ::                           &
        tracert,tracer
     real,    dimension (its:ite)                                      &
        ,intent (inout  )                   ::                           &
        pre
     integer,    dimension (its:ite)                                   &
         ,intent (inout  )                   ::                        &
          ktop
     integer,    dimension (its:ite)     ::                            &
        kbcon
  !
  ! basic environmental input includes moisture convergence (mconv)
  ! omega (omeg), windspeed (us,vs), and a flag (aaeq) to turn off
  ! convection for this call only and at that particular gridpoint
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        T,P,US,VS,HSTARY
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
         Q
     real, dimension (its:ite)                                         &
        ,intent (in   )                   ::                           &
        Z1,PSUR,AAEQ

       
       real                                                            &
        ,intent (in   )                   ::                           &
        dtime,tcrit,xl,cp,rv,g


     real,    dimension (its:ite,1:3) ::                         &
        edtc
!
!
!
!***************** the following are your basic environmental
!                  variables. They carry a "_cup" if they are
!                  on model cloud levels (staggered). They carry
!                  an "o"-ending (z becomes zo), if they are the forced
!                  variables. They are preceded by x (z becomes xz)
!                  to indicate modification by some typ of cloud
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
!
!
  ! hcd = moist static energy in downdraft
  ! zd normalized downdraft mass flux
  ! dby = buoancy term
  ! entr = entrainment rate
  ! zd   = downdraft normalized mass flux
  ! entr= entrainment rate
  ! hcd = h in model cloud
  ! bu = buoancy term
  ! zd = normalized downdraft mass flux
  ! gamma_cup = gamma on model cloud levels
  ! mentr_rate = entrainment rate
  ! qcd = cloud q (including liquid water) after entrainment
  ! qrch = saturation q in cloud
  ! pwd = evaporate at that level
  ! pwev = total normalized integrated evaoprate (I2)
  ! entr= entrainment rate
  ! z1 = terrain elevation
  ! entr = downdraft entrainment rate
  ! jmin = downdraft originating level
  ! kdet = level above ground where downdraft start detraining
  ! psur        = surface pressure
  ! z1          = terrain elevation
  ! zd      = downdraft normalized mass flux
  ! zu      = updraft normalized mass flux
  ! mbdt    = arbitrary numerical parameter
  ! dtime   = dt over which forcing is applied
  ! kbcon       = LFC of parcel from k22
  ! k22         = updraft originating level
  ! dby = buoancy term
  ! ktop = cloud top (output)
  ! xmb    = total base mass flux
  ! hc = cloud moist static energy
  ! hkb = moist static energy at originating level
  ! mentr_rate = entrainment rate

     real,    dimension (its:ite,kts:kte) ::                           &
        he,hes,qes,z,pwdper,                                           &

        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,      &

        dby,qc,qrcd,pwd,pw,hcd,qcd,dbyd,hc,qrc,zu,zd,clw_all,          &

  ! cd  = detrainment function for updraft
  ! cdd = detrainment function for downdraft

        cd,cdd,scr1,DELLAH,DELLAQ,DELLAT,DELLAQC

  ! edt = epsilon
  ! edt     = epsilon
     real,    dimension (its:ite) ::                                   &
       edt,HKB,QKB,          &
       XMB,PWAV,PWEV,BU,cap_max,cap_max_increment
     real,    dimension (its:ite,kts:kte,num_chem)       ::             &
        tr_c,tr_up,tr_dd,tre_cup,tr_pw,tr_pwd
     real,    dimension (its:ite,num_chem)         ::                   &
        trkb
     integer,    dimension (its:ite) ::                                &
       kzdown,KDET,K22,KB,JMIN,kstabi,kstabm,                     &   !-lxz
       ierr,KBMAX 

     integer                              ::                           &
       ki,I,K,KK
     real                                 ::                           &
      day,dz,mbdt,entr_rate,radius,entrd_rate,mentr_rate,mentrd_rate,  &
      zcutdown,edtmax,edtmin,depth_min,zkbmax,z_detr,zktop,            &
      dh,cap_maxs

     integer :: itf,jtf,ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)
     jtf=MIN(jte,jde-1)

!sms$distribute end
      day=86400.
!
!--- specify entrainmentrate and detrainmentrate
!
      radius=12000.
!
!--- gross entrainment rate (these may be changed later on in the
!--- program, depending what your detrainment is!!)
!
      entr_rate=.2/radius
!
!--- entrainment of mass
!
      mentrd_rate=0.
      mentr_rate=entr_rate
!
!--- initial detrainmentrates
!
      do k=kts,ktf
      do i=its,itf
        cd(i,k)=0.1*entr_rate
        cdd(i,k)=0.
        clw_all(i,k)=0.
      enddo
      enddo
!
!--- max/min allowed value for epsilon (ratio downdraft base mass flux/updraft
!    base mass flux
!
      edtmax=.8
      edtmin=.2
!
!--- minimum depth (m), clouds must have
!
      depth_min=500.
!
!--- maximum depth (mb) of capping 
!--- inversion (larger cap = no convection)
!
      cap_maxs=175.
!sms$to_local(grid_dh: <1, mix :size>, <2, mjx :size>) begin
      DO 7 i=its,itf
        kbmax(i)=1
        cap_max_increment(i)=0.
        edt(i)=0.
        kstabm(i)=ktf-1
        IERR(i)=0
        if(aaeq(i).ne.0.)then
           ierr(i)=20
        endif
 7    CONTINUE
      do i=its,itf
          cap_max(i)=cap_maxs
      enddo
!
!--- max height(m) above ground where updraft air can originate
!
      zkbmax=4000.
!
!--- height(m) above which no downdrafts are allowed to originate
!
      zcutdown=3000.
!
!--- depth(m) over which downdraft detrains all its mass
!
      z_detr=1250.
!
      mbdt=dtime*4.E-03
!
!--- calculate moist static energy, heights, qes
!
      call cup_env(z,qes,he,hes,t,q,p,z1, &
           psur,ierr,tcrit,0,xl,cp,   &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- environmental values on cloud levels
!
      call cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,q_cup,he_cup, &
           hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
           ierr,z1,xl,rv,cp,          &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_env_clev_tr(tracer,tre_cup,num_chem,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
      if(ierr(i).eq.0)then
!
      do k=kts,ktf-2
        if(z_cup(i,k).gt.zkbmax+z1(i))then
          kbmax(i)=k
          go to 25
        endif
      enddo
 25   continue
!
!
!--- level where detrainment for downdraft starts
!
      do k=kts,ktf
        if(z_cup(i,k).gt.z_detr+z1(i))then
          kdet(i)=k
          go to 26
        endif
      enddo
 26   continue
!
      endif
      enddo
!
!
!
!------- DETERMINE LEVEL WITH HIGHEST MOIST STATIC ENERGY CONTENT - K22
!
      CALL cup_MAXIMI(HE_CUP,3,KBMAX,K22,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
       DO 36 i=its,itf
         IF(ierr(I).eq.0.)THEN
         IF(K22(I).GE.KBMAX(i))ierr(i)=2
         endif
 36   CONTINUE
!
!--- DETERMINE THE LEVEL OF CONVECTIVE CLOUD BASE  - KBCON
!
      call cup_kbcon(cap_max_increment,1,k22,kbcon,he_cup,hes_cup, &
           ierr,kbmax,p_cup,cap_max, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- increase detrainment in stable layers
!
      CALL cup_minimi(HEs_cup,Kbcon,kstabm,kstabi,ierr,  &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
      IF(ierr(I).eq.0.)THEN
        if(kstabm(i)-1.gt.kstabi(i))then
           do k=kstabi(i),kstabm(i)-1
             cd(i,k)=cd(i,k-1)+1.5*entr_rate
             if(cd(i,k).gt.10.0*entr_rate)cd(i,k)=10.0*entr_rate
           enddo
        ENDIF
      ENDIF
      ENDDO
!
!--- calculate incloud moist static energy
!
      call cup_up_he(k22,hkb,z_cup,cd,mentr_rate,he_cup,hc, &
           kbcon,ierr,dby,he,hes_cup, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)

!--- DETERMINE CLOUD TOP - KTOP
!
      call cup_ktop(1,dby,kbcon,ktop,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      DO 37 i=its,itf
         kzdown(i)=0
         if(ierr(i).eq.0)then
            zktop=(z_cup(i,ktop(i))-z1(i))*.6
            zktop=min(zktop+z1(i),zcutdown+z1(i))
            do k=kts,ktf
              if(z_cup(i,k).gt.zktop)then
                 kzdown(i)=k
                 go to 37
              endif
              enddo
         endif
 37   CONTINUE
!
!--- DOWNDRAFT ORIGINATING LEVEL - JMIN
!
      call cup_minimi(HEs_cup,K22,kzdown,JMIN,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      DO 100 i=its,ite
      IF(ierr(I).eq.0.)THEN
           if(jmin(i).le.3)then
             ierr(i)=9
             go to 100
           endif
!
!--- check whether it would have buoyancy, if there where
!--- no entrainment/detrainment
!
101   continue
      if(jmin(i)-1.lt.KDET(I))kdet(i)=jmin(i)-1
      if(jmin(i).ge.Ktop(I)-1)jmin(i)=ktop(i)-2
      ki=jmin(i)
      hcd(i,ki)=hes_cup(i,ki)
      DZ=Z_cup(i,Ki+1)-Z_cup(i,Ki)
      dh=dz*(HCD(i,Ki)-hes_cup(i,ki))
      dh=0.
!
      do k=ki-1,1,-1
         hcd(i,k)=hes_cup(i,jmin(i))
         DZ=Z_cup(i,K+1)-Z_cup(i,K)
         dh=dh+dz*(HCD(i,K)-hes_cup(i,k))
         if(dh.gt.0.)then
           jmin(i)=jmin(i)-1
           if(jmin(i).gt.3)then
             go to 101
           else if(jmin(i).le.3)then
             ierr(i)=9
             go to 100
           endif
         endif
       enddo

         IF(JMIN(I).LE.3)then
            ierr(i)=4
         endif

      ENDIF
100   continue
!
! - Must have at least depth_min m between cloud convective base
!     and cloud top.
!
      do i=its,itf
      IF(ierr(I).eq.0.)THEN
           if(jmin(i).le.3)then
             ierr(i)=9
           endif
      IF(-z_cup(I,KBCON(I))+z_cup(I,KTOP(I)).LT.depth_min)then
            ierr(i)=6
      endif
      endif
      enddo

!
!c--- normalized updraft mass flux profile
!
      call cup_up_nms(zu,z_cup,mentr_rate,cd,kbcon,ktop,ierr,k22, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!c--- normalized downdraft mass flux profile,also work on bottom detrainment
!--- in this routine
!
      call cup_dd_nms(zd,z_cup,cdd,mentrd_rate,jmin,ierr, &
           0,kdet,z1,                 &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- downdraft moist static energy
!
      call cup_dd_he(hes_cup,zd,hcd,z_cup,cdd,mentrd_rate, &
           jmin,ierr,he,dbyd,he_cup,  &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- calculate moisture properties of downdraft
!
      call cup_dd_moisture(zd,hcd,hes_cup,qcd,qes_cup, &
           pwd,q_cup,z_cup,cdd,mentrd_rate,jmin,ierr,gamma_cup, &
           pwev,bu,qrcd,q,he,t_cup,2,xl, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- calculate moisture properties of updraft
!
      call cup_up_moisture(ierr,z_cup,qc,qrc,pw,pwav, &
           kbcon,ktop,cd,dby,mentr_rate,clw_all,      &
           q,GAMMA_cup,zu,qes_cup,k22,q_cup,xl, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- DETERMINE DOWNDRAFT STRENGTH IN TERMS OF WINDSHEAR
!
      call cup_dd_edt(ierr,us,vs,z,ktop,kbcon,edt,p,pwav, &
           pwev,edtmax,edtmin,3,edtc, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
        do i=its,itf
         if(ierr(i).eq.0)then
         edt(i)=edtc(i,2)
         endif
        enddo
!
! massflux from precip and normalized cloud properties
!
        pwdper=0.
        do i=its,itf
 
          if(ierr(i).gt.0)pre(i)=0.
          if(ierr(i).eq.0)then
          xmb(i)=pre(i)/(pwav(i)+edt(i)*pwev(i))
!
!--- percent of that that is evaporated (pwd is negative)
!
          if(i.eq.ipr.and.j.eq.jpr)then
            print *,'xmb,edt,pwav = ',xmb(i),edt(i),pwav(i)
            print *,'k,pwdper(i,k),pw,pwd(i,k)',z1(i)
          endif
          do k=1,ktop(i)
          pwdper(i,k)=-edt(i)*pwd(i,k)/pwav(i)
          if(i.eq.ipr.and.j.eq.jpr)then
            print *,k,pwdper(i,k),pw(i,k),pwd(i,k)
          endif
          enddo
          endif
        enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!!!!!   NOW WE HAVE EVREYTHING TO CALCULATE TRACER TRANSPORT AND WET DEPOSITION !!!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!--- calculate incloud tracer distribution
!
       if(j.eq.jpr)print *,'calling up_tracer'
       call cup_up_tracer(ierr,tcrit,t,pre,z_cup,p,tracer,tre_cup,tr_up,tr_pw, &
                   tr_c,hstary,pw,clw_all,kbcon,ktop,cd,mentr_rate,zu,k22,&
                   numgas,chemopt,scalaropt,ids,ide, jds,jde, kds,kde, &
                   ims,ime, jms,jme, kms,kme, &
                   its,ite, jts,jte, kts,kte,ipr,jpr,j,npr,num_chem)
       if(j.eq.jpr)print *,'called up_tracer'
       call cup_dd_tracer(ierr,z_cup,qrcd,tracer,tre_cup,tr_up,tr_dd, &
                    tr_pw,tr_pwd,jmin,cdd,mentrd_rate,zd,pwdper,k22,        &
                    num_chem,ids,ide, jds,jde, kds,kde, &
                    ims,ime, jms,jme, kms,kme, &
                    its,ite, jts,jte, kts,kte)
       if(j.eq.jpr)print *,'called dd_tracer'


!
      if(j.eq.jpr)then
        i=ipr
        print *,'in 250 loop ',edt(ipr),ierr(ipr)
!       if(ierr(i).eq.0.or.ierr(i).eq.3)then
        print *,k22(I),kbcon(i),ktop(i),jmin(i)
        print *,edt(i)
        do k=kts,ktf
          print *,k,z(i,k),he(i,k),hes(i,k)
        enddo
        do k=1,ktop(i)+1
          print *,zu(i,k),zd(i,k),pw(i,k),pwd(i,k)
        enddo
        print *,'tr_up(i,k,6),tr_dd(i,k,6),tr_pw(i,k,6),tr_pwd(i,k,6)'
        do k=1,ktop(i)+1
          print *,tr_up(i,k,npr),tr_dd(i,k,npr),tr_pw(i,k,npr),tr_pwd(i,k,npr)
        enddo
        endif
!     endif
!
!--- calculate transport tendencies
!
!--- 1. in bottom layer
!
      call cup_dellabot_tr(ipr,jpr,tre_cup,ierr,z_cup,p,tr_dd,edt, &
           zd,cdd,tracer,tracert,j,mentrd_rate,z,g,xmb, &
           num_chem,ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- 2. everywhere else
!

      call cup_dellas_tr(ierr,z_cup,p_cup,tr_dd,edt,zd,cdd,      &
           tracer,tracert,j,mentrd_rate,zu,g,xmb,                &
           cd,tr_up,ktop,k22,kbcon,mentr_rate,jmin,tre_cup,kdet, &
           k22,ipr,jpr,npr,'deep',num_chem,                      &
           ids,ide, jds,jde, kds,kde,                            &
           ims,ime, jms,jme, kms,kme,                            &
           its,ite, jts,jte, kts,kte                             )
       if(j.eq.jpr)then
        i=ipr
        do k=kts,ktf
          print *,k,tracer(i,k,npr),tracert(i,k,npr)
        enddo
       endif
!
! may need more below for wet deposition......
!
!
!     call cup_output_wd (   &
!          ids,ide, jds,jde, kds,kde, &
!          ims,ime, jms,jme, kms,kme, &
!          its,ite, jts,jte, kts,kte)

   END SUBROUTINE CUP_CT

   SUBROUTINE cup_dellabot_tr(ipr,jpr,tre_cup,ierr,z_cup,p_cup,  &
              tr_dd,edt,zd,cdd,tracer,tracert,j,mentrd_rate,z,g,xmb,     &
              num_chem,ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        num_chem,ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        j,ipr,jpr
  !
  ! ierr error value, maybe modified in this routine
  !
     real,    dimension (its:ite,kts:kte,1:num_chem)                              &
        ,intent (out  )                   ::                           &
        tracert
     real,    dimension (its:ite,kts:kte,1:num_chem)                              &
        ,intent (in   )                   ::                           &
        tre_cup,tracer,tr_dd
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                   ::                           &
        z_cup,p_cup,zd,cdd,z
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        edt,xmb
     real                                                              &
        ,intent (in   )                   ::                           &
        g,mentrd_rate
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
!
!  local variables in this routine
!

      integer i
      real detdo,detdo1,detdo2,entdo,dp,dz,subin,                      &
      totmas
!
     integer :: itf, ktf, nv, npr
     npr=24
     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)
!
!
      if(j.eq.jpr)print *,'in cup dellabot '
      tracert=0.
      do 100 i=its,itf
      if(ierr(i).ne.0)go to 100
      dz=z_cup(i,2)-z_cup(i,1)
      DP=100.*(p_cup(i,1)-P_cup(i,2))
      detdo1=edt(i)*zd(i,2)*CDD(i,1)*DZ
      detdo2=edt(i)*zd(i,1)
      entdo=edt(i)*zd(i,2)*mentrd_rate*dz
      subin=-EDT(I)*zd(i,2)
      detdo=detdo1+detdo2-entdo+subin
      do nv=2,num_chem
      tracert(I,1,nv)=(detdo1*.5*(tr_dd(i,1,nv)+tr_dd(i,2,nv)) &
                 +detdo2*tr_dd(i,1,nv) &
                 +subin*tre_cup(i,2,nv) &
                 -entdo*tracer(i,1,nv))*g/dp*xmb(i)
      enddo
      if(j.eq.jpr.and.i.eq.ipr)print *,'in cup dellabot ',tracert(I,1,npr), &
        detdo1,detdo2,subin,entdo,tr_dd(i,1,npr),tr_dd(i,2,npr),tracer(i,1,npr)
 100  CONTINUE

   END SUBROUTINE cup_dellabot_tr


   SUBROUTINE cup_dellas_tr(ierr,z_cup,p_cup,tr_dd,edt,zd,cdd,             &
              tracer,tracert,j,mentrd_rate,zu,g,xmb,                       &
              cd,tr_up,ktop,k22,kbcon,mentr_rate,jmin,tre_cup,kdet,kpbl,   &
              ipr,jpr,npr,name,num_chem,                                   &
              ids,ide, jds,jde, kds,kde,                                   &
              ims,ime, jms,jme, kms,kme,                                   &
              its,ite, jts,jte, kts,kte                                    )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        num_chem,ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        j,ipr,jpr,npr
  !
  ! ierr error value, maybe modified in this routine
  !
     real,    dimension (its:ite,kts:kte,1:num_chem)                              &
        ,intent (inout  )                   ::                           &
        tracert
     real,    dimension (its:ite,kts:kte,1:num_chem)                              &
        ,intent (in  )                   ::                           &
        tr_up,tr_dd,tre_cup,tracer
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                   ::                           &
        z_cup,p_cup,zd,cdd,cd,zu
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        edt,xmb
     real                                                              &
        ,intent (in   )                   ::                           &
        g,mentrd_rate,mentr_rate
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,ktop,k22,jmin,kdet,kpbl
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
      character *(*), intent (in)        ::                           &
       name
!
!  local variables in this routine
!

      integer i,k,nv
      real detdo1,detdo2,entdo,dp,dz,subin,detdo,entup,                &
      detup,subdown,entdoj,entupk,detupk,totmas
!
     integer :: itf, ktf
!    npr=24
     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)
!
!
      i=ipr
      if(j.eq.jpr)then
        print *,'in dellas kpbl(i),k22(i),kbcon(i),ktop(i),jmin(i)'
        print *,kpbl(i),k22(i),kbcon(i),ktop(i),jmin(i)
      endif
       do nv=2,num_chem
       DO K=kts+1,kte
       do i=its,itf
          tracert(i,k,nv)=0.
       enddo
       enddo
       enddo
!
       DO 100 k=kts+1,ktf-1
       DO 100 i=its,ite
         IF(ierr(i).ne.0)GO TO 100
         IF(K.Gt.KTOP(I))GO TO 100
!
!--- SPECIFY DETRAINMENT OF DOWNDRAFT, HAS TO BE CONSISTENT
!--- WITH ZD CALCULATIONS IN SOUNDD.
!
         DZ=Z_cup(I,K+1)-Z_cup(I,K)
         detdo=edt(i)*CDD(i,K)*DZ*ZD(i,k+1)
         entdo=edt(i)*mentrd_rate*dz*zd(i,k+1)
         subin=zu(i,k+1)-zd(i,k+1)*edt(i)
         entup=0.
         detup=0.
         if(k.ge.kbcon(i).and.k.lt.ktop(i))then
            entup=mentr_rate*dz*zu(i,k)
            detup=CD(i,K+1)*DZ*ZU(i,k)
         endif
         subdown=(zu(i,k)-zd(i,k)*edt(i))
         entdoj=0.
         entupk=0.
         detupk=0.
!
         if(k.eq.jmin(i))then
         entdoj=edt(i)*zd(i,k)
         endif

         if(k.eq.k22(i)-1)then
         entupk=zu(i,kpbl(i))
         endif

         if(k.gt.kdet(i))then
            detdo=0.
         endif

         if(k.eq.ktop(i)-0)then
         detupk=zu(i,ktop(i))
         subin=0.
         endif
         if(k.lt.kbcon(i))then
            detup=0.
         endif
!C
!C--- CHANGED DUE TO SUBSIDENCE AND ENTRAINMENT
!C
         totmas=subin-subdown+detup-entup-entdo+ &
                 detdo-entupk-entdoj+detupk
          if(j.eq.jpr.and.i.eq.ipr)print *,'k,totmas,sui,sud = ',k, &
          totmas,subin,subdown
!         if(j.eq.jpr.and.i.eq.ipr)print *,'updr stuff = ',detup,
!     1      entup,entupk,detupk
!         if(j.eq.jpr.and.i.eq.ipr)print *,'dddr stuff = ',entdo,
!     1      detdo,entdoj
         if(abs(totmas).gt.1.e-6)then
           print *,'*********************',i,j,k,totmas,name
           print *,kpbl(i),k22(i),kbcon(i),ktop(i)
!c          print *,'updr stuff = ',subin,
!c    1      subdown,detup,entup,entupk,detupk
!c          print *,'dddr stuff = ',entdo,
!c    1      detdo,entdoj
            CALL wrf_error_fatal ( 'cup_dellas_tr: TOTMAS > CRITICAL VALUE')
         endif
         dp=100.*(p_cup(i,k-1)-p_cup(i,k))
         do nv=2,num_chem
!        tracert(i,k,nv)=(subin*tre_cup(i,k+1,nv) &
!                   -subdown*tre_cup(i,k,nv) &
         tracert(i,k,nv)=(subin*tracer(i,k+1,nv) &
                    -subdown*tracer(i,k,nv) &
                    +detup*.5*(tr_up(i,K+1,nv)+tr_up(i,K,nv)) &
                    +detdo*.5*(tr_dd(i,K+1,nv)+tr_dd(i,K,nv)) &
                    -entup*tracer(i,k,nv) &
                    -entdo*tracer(i,k,nv) &
                    -entupk*tre_cup(i,k22(i),nv) &
                    -entdoj*tre_cup(i,jmin(i),nv) &
                    +detupk*tr_up(i,ktop(i),nv) &
                     )*g/dp*xmb(i)
         enddo
       if(i.eq.ipr.and.j.eq.jpr)then
         print *,k,tracert(i,k,npr),subin*tre_cup(i,k+1,npr),subdown*tre_cup(i,k,npr), &
                   detdo*.5*(tr_dd(i,K+1,npr)+tr_dd(i,K,npr))
         print *,k,detup*.5*(tr_up(i,K+1,npr)+tr_up(i,K,npr)),detupk*tr_up(i,ktop(i),npr), &
                entup*tracer(i,k,npr),entdo*tracer(i,k,npr)
         print *,k,entupk*tre_cup(i,k,npr),detupk,tr_up(i,ktop(i),npr)
       endif

 100  CONTINUE

   END SUBROUTINE cup_dellas_tr
   SUBROUTINE cup_env_clev_tr(tracer,tre_cup,num_chem,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      implicit none
     integer                                                           &
        ,intent (in   )                   ::                           &
        num_chem,ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, dimension (its:ite)                                      &
        ,intent (in)                      ::                           &
        ierr

     real,    dimension (its:ite,kts:kte,1:num_chem)                   &
        ,intent (in   )                   ::                           &
        tracer
     real,    dimension (its:ite,kts:kte,1:num_chem)                   &
        ,intent (out  )                   ::                           &
        tre_cup
! 
!  local variables in this routine
! 
  
     integer                              ::                           &
       i,k,nv,itf,ktf
     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)
      do nv=2,num_chem
      do k=kts+1,ktf
      do i=its,ite
        if(ierr(i).eq.0)then
        tre_cup(i,k,nv)=.5*(tracer(i,k-1,nv)+tracer(i,k,nv))
        endif
      enddo
      enddo
      enddo
      do nv=2,num_chem
      do i=its,ite
        if(ierr(i).eq.0)then
        tre_cup(i,kts,nv)=tracer(i,kts,nv)
        endif
      enddo
      enddo


END subroutine cup_env_clev_tr


   SUBROUTINE  cup_up_tracer(ierr,tcrit,t,pre,z_cup,p,tracer,tre_cup,tr_up, &
                tr_pw,tr_c,hstary,cupclw,clw_all,kbcon,ktop,cd,mentr_rate,zu,k22,  &
                          numgas,chemopt,scalaropt,ids,ide, jds,jde, kds,kde, &
                          ims,ime, jms,jme, kms,kme, &
                          its,ite, jts,jte, kts,kte,ipr,jpr,j,npr,num_chem)
! USE module_configure
! USE module_state_description
  USE module_ctrans_aqchem
        implicit none
! Aqeuous species pointers INCLUDE File

!...........PARAMETERS and their descriptions:

      INTEGER      NGAS            ! number of gas phase species for AQCHEM
      PARAMETER  ( NGAS  = 11 )

      INTEGER      NAER            ! number of aerosol species for AQCHEM
      PARAMETER  ( NAER  = 23 )

!...pointers for the AQCHEM array GAS

      INTEGER      LSO2            ! local pointer to SO2
      PARAMETER  ( LSO2   =  1 )

      INTEGER      LHNO3           ! local pointer to HNO3
      PARAMETER  ( LHNO3  =  2 )

      INTEGER      LN2O5           ! local pointer to N2O5
      PARAMETER  ( LN2O5  =  3 )
      INTEGER      LCO2            ! local pointer to CO2
      PARAMETER  ( LCO2   =  4 )

      INTEGER      LNH3            ! local pointer to NH3
      PARAMETER  ( LNH3   =  5 )

      INTEGER      LH2O2           ! local pointer to H2O2
      PARAMETER  ( LH2O2  =  6 )

      INTEGER      LO3             ! local pointer to O3
      PARAMETER  ( LO3    =  7 )

      INTEGER      LFOA            ! local pointer to FOA
      PARAMETER  ( LFOA   =  8 )

      INTEGER      LMHP            ! local pointer to MHP
      PARAMETER  ( LMHP   =  9 )

      INTEGER      LPAA            ! local pointer to PAA
      PARAMETER  ( LPAA   = 10 )

      INTEGER      LH2SO4          ! local pointer to H2SO4
      PARAMETER  ( LH2SO4 = 11 )

!...pointers for the AQCHEM array AEROSOL

      INTEGER      LSO4AKN         ! local pointer to SO4I aerosol
      PARAMETER  ( LSO4AKN =  1 )

      INTEGER      LSO4ACC         ! local pointer to SO4 aerosol
      PARAMETER  ( LSO4ACC =  2 )

      INTEGER      LNH4AKN         ! local pointer to NH4I aerosol
      PARAMETER  ( LNH4AKN =  3 )

      INTEGER      LNH4ACC         ! local pointer to NH4 aerosol
      PARAMETER  ( LNH4ACC =  4 )

      INTEGER      LNO3AKN         ! local pointer to NO3I aerosol
      PARAMETER  ( LNO3AKN =  5 )

      INTEGER      LNO3ACC         ! local pointer to NO3 aerosol
      PARAMETER  ( LNO3ACC =  6 )

      INTEGER      LNO3COR         ! local pointer to course aerosol nitrate
      PARAMETER  ( LNO3COR =  7 )

      INTEGER      LORGAKN         ! local pointer to organic I aerosol
      PARAMETER  ( LORGAKN =  8 )

      INTEGER      LORGACC         ! local pointer to organic aerosol
      PARAMETER  ( LORGACC =  9 )

      INTEGER      LPRIAKN         ! local pointer to primary I aerosol
      PARAMETER  ( LPRIAKN = 10 )

      INTEGER      LPRIACC         ! local pointer to primary aerosol
      PARAMETER  ( LPRIACC = 11 )

      INTEGER      LPRICOR         ! local pointer to primary I aerosol
      PARAMETER  ( LPRICOR = 12 )

      INTEGER      LCACO3          ! local pointer to CaCO3 aerosol
      PARAMETER  ( LCACO3  = 13 )

      INTEGER      LMGCO3          ! local pointer to MgCO3 aerosol
      PARAMETER  ( LMGCO3  = 14 )

      INTEGER      LNACL           ! local pointer to NaCl aerosol
      PARAMETER  ( LNACL   = 15 )

      INTEGER      LA3FE           ! local pointer to Fe+++ aerosol
      PARAMETER  ( LA3FE   = 16 )

      INTEGER      LB2MN           ! local pointer to Mn++ aerosol
      PARAMETER  ( LB2MN   = 17 )

      INTEGER      LKCL            ! local pointer to NaCl aerosol
      PARAMETER  ( LKCL    = 18 )

      INTEGER      LNUMAKN         ! local pointer to # Aitken aerosol
      PARAMETER  ( LNUMAKN = 19 )

      INTEGER      LNUMACC         ! local pointer to # accumulation aerosol
      PARAMETER  ( LNUMACC = 20 )

      INTEGER      LNUMCOR         ! local pointer to # coarse aerosol
      PARAMETER  ( LNUMCOR = 21 )

      INTEGER      LSRFAKN         ! local pointer to sfc area Aitken aerosol
      PARAMETER  ( LSRFAKN = 22 )

      INTEGER      LSRFACC         ! local pntr to sfc area accumulation aerosol
      PARAMETER  ( LSRFACC = 23 )


!
!  on input
!
  
   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
                         numgas,ids,ide, jds,jde, kds,kde,scalaropt,   &
                         num_chem,ims,ime, jms,jme, kms,kme,chemopt,   &
                            its,ite, jts,jte, kts,kte,ipr,jpr,j,npr
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                   ::                           &
        z_cup,cd,zu,p,hstary,t
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout  )                   ::                           &
        cupclw,clw_all
     real,    dimension (its:ite,kts:kte,1:num_chem)                              &
        ,intent (inout  )                   ::                           &
        tr_up,tr_c,tr_pw
     real,    dimension (its:ite,kts:kte,1:num_chem)                              &
        ,intent (in  )                   ::                           &
        tre_cup,tracer
     real,    dimension (its:ite)                              &
        ,intent (in  )                   ::                           &
        pre

  ! entr= entrainment rate
     real                                                              &
        ,intent (in   )                   ::                           &
        mentr_rate,tcrit
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,ktop,k22
   ! ierr error value, maybe modified in this routine
  
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
!  local variables in this routine
!
      real :: conc_equi,conc_mxr,partialp,taucld

     integer                              ::                           &
        iall,i,k,iwd,nv
     real                                 ::                           &
        trcc,trch,dh,qrch,c0,dz,radius,airm,dens
     integer                              ::                           &
       itf,ktf,iaer,igas
!
! aerosol scavenging coeffs for aitken mode
!
           real alfa0,alfa2,alfa3
! output variables
! hpwdep h+ deposition
      real, dimension (ngas) :: gas,gaswdep
      real, dimension (naer) :: aerosol,aerwdep
          real hpwdep
     alfa0=0.
         alfa2=0.
         alfa3=0.
       gas(lco2)=340.
         taucld=1800.
         qrch=0.
     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)

!
        iall=0
        c0=.002
        iwd=0
!
!--- no precip for small clouds
!
        if(mentr_rate.gt.0.)then
          radius=.2/mentr_rate
          if(radius.lt.900.)c0=0.
!         if(radius.lt.900.)iall=0
        endif
        do nv=2,num_chem
        do k=kts,ktf
        do i=its,itf
          tr_pw(i,k,nv)=0.
          if(ierr(i).eq.0)tr_up(i,k,nv)=tre_cup(i,k,nv)
          tr_c(i,k,nv)=0.
        enddo
        enddo
        enddo
      do nv=2,num_chem
      do i=its,itf
      if(ierr(i).eq.0.)then
      do k=k22(i),kbcon(i)-1
        tr_up(i,k,nv)=tre_cup(i,k22(i),nv)
      enddo
      endif
      enddo
      enddo
      if(j.eq.jpr)print *,'p_so2,o_o3 = ',p_so2,p_o3
        DO 100 k=kts+1,ktf-1
        DO 100 i=its,itf
         AEROSOL=0.
         GAS=0.
         IF(ierr(i).ne.0)GO TO 100
         IF(K.Lt.KBCON(I))GO TO 100
         IF(K.Gt.KTOP(I)+1)GO TO 100
         DZ=Z_cup(i,K)-Z_cup(i,K-1)
         if(cupclw(i,k).le.0.)cupclw(i,k)=0.
         if(clw_all(i,k).le.0.)clw_all(i,k)=0.
!
!------    1. steady state plume equation, for what could
!------       be in cloud before anything happens (kg/kg)
!------       tr_up would be the concentration if tr would be conserved
!
!
        do nv=2,num_chem
        if(i.eq.ipr.and.j.eq.jpr.and.nv.eq.npr)print *,k,tr_up(i,K-1,nv),tr_up(i,K,nv),tr_pw(i,k-1,nv),clw_all(i,k),cupclw(i,k)
        tr_up(i,K,nv)=(tr_up(i,K-1,nv)*(1.-.5*CD(i,K)*DZ)+mentr_rate* &
                DZ*tracer(i,K-1,nv))/(1.+mentr_rate*DZ-.5*cd(i,k)*dz)
        tr_up(i,k,nv)=max(1.e-16,tr_up(i,K,nv))
        enddo
!
! sources or sinks due to aq chem
!

!!!!!!!! the following only for made/sorgam !!!!!!!!!!
        if(chemopt.eq.2)then
           dens=1000.*p(i,k)*100./t(i,k)/287./28.9628
           airm=dens*dz
!...gas concentrations (ppm)

      GAS( LCO2 )  = 370.0
       GAS( LFOA )  = 0.0 ! ???
       GAS( LMHP )  = 0.0 ! ???

      GAS( LSO2 )  = tr_up(i,k,p_so2)
      GAS( LH2SO4 )  = tr_up(i,k,p_sulf)
      GAS( LNH3 )  = tr_up(i,k,p_nh3)
      GAS( LH2O2 ) = tr_up(i,k,p_h2o2)

      GAS( LO3 )   = tr_up(i,k,p_o3)
      GAS( LPAA )  = tr_up(i,k,p_paa)
      GAS( LHNO3 ) = tr_up(i,k,p_hno3)
      GAS( LN2O5 ) = tr_up(i,k,p_n2o5)
!...convert to mol/mol  

      DO IGAS=1,NGAS
        GAS( IGAS ) = GAS( IGAS ) * 1.0E-6
      END DO

!...aerosol concentrations (ug/m3)

!      AEROSOL( LSO4ACC ) = 20.0
!      AEROSOL( LNH4ACC ) = 6.65
!      AEROSOL( LNO3ACC ) = 10.0
!      AEROSOL( LNACL )   = 1.71
!!      AEROSOL( LA3FE )   = 0.5
!      AEROSOL( LB2MN )   = 0.02
!      AEROSOL( LNO3COR ) = 0.0
       AEROSOL( LORGACC ) = 0.0
       AEROSOL( LPRIACC ) = 0.0
!      AEROSOL( LCACO3 )  = 3.05
!      AEROSOL( LMGCO3 )  = 0.0

      AEROSOL( LSO4ACC ) = tr_up(i,k,p_so4aj)
      AEROSOL( LNH4ACC ) = tr_up(i,k,p_nh4aj)
      AEROSOL( LNO3ACC ) = tr_up(i,k,p_no3aj)
      AEROSOL( LNACL )   = 0.
      AEROSOL( LA3FE )   = .5
      AEROSOL( LB2MN )   = .02
      AEROSOL( LNO3COR ) = 0.
!     AEROSOL( LORGACC ) = tr_up(i,k,) + tr_up(i,k,) + tr_up(i,k,)
!     AEROSOL( LPRIACC ) = tr_up(i,k,) + tr_up(i,k,)
      AEROSOL( LCACO3 )  = 0.
      AEROSOL( LMGCO3 )  = 0.


!...convert to mol/mol
!      

!      DO IAER=1,NAER
!        AEROSOL( IAER ) = AEROSOL( IAER ) * 1.0E-6 * CTHK1
!     &                  / ( SGRAERMW( IAER ) * AIRM )
!      END DO
      DO IAER=1,NAER
        AEROSOL( IAER ) = AEROSOL( IAER ) * 1.0E-6
      END DO
! first clw is water, second is total

     GASWDEP=0.
     AERWDEP=0.
     HPWDEP=0.
!    if(clw_all(i,k).gt.1.e-12)then 
!    if(cupclw(i,k).gt.1.e-12)then 
!     CALL AQCHEM (t(i,k),p(i,k)*100.,taucld,cupclw(i,k)/3600.,           &
!        clw_all(i,k)*dens,clw_all(i,k)*dens,airm,ALFA0,ALFA2,ALFA3,GAS,  &
!                  AEROSOL, GASWDEP, AERWDEP, HPWDEP )
!    endif
!    endif





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!            FOLLOWING FOR WET DEPOSITION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
       do nv=2,num_chem
          tr_c(i,k,nv)=0.
                  tr_pw(i,k,nv)=c0*dz*tr_C(I,K,nv)*zu(i,k)
        if(tr_c(i,k,nv).le.0.)then
          tr_c(i,k,nv)=0.
        endif
           enddo
!
!--- iall.eq.1, if all cloudwater goes to rain
!
        if(iall.eq.1)then
          tr_c(i,k,nv)=0.
          tr_pw(i,k,nv)=(tr_c(I,K,nv)-QRCH)*zu(i,k)
          if(tr_pw(i,k,nv).lt.0.)tr_pw(i,k,nv)=0.
        endif

!
!----- set next level
!        tr_up(I,K,nv)=tr_c(I,K,nv)+qrch
      tr_up(i,k,p_so2)=gas(lso2)*1.e6
      tr_up(i,k,p_sulf)=gas(lh2so4)*1.e6
      tr_up(i,k,p_nh3)=gas(lnh3)*1.e6 
      tr_up(i,k,p_h2o2)=gas(lh2o2)*1.e6

      tr_up(i,k,p_o3)=gas(lo3)*1.e6
      tr_up(i,k,p_paa)=gas(lpaa)*1.e6
      tr_up(i,k,p_hno3)=gas(lhno3)*1.e6 
      tr_up(i,k,p_n2o5)=gas(ln2o5)*1.e6
      tr_up(i,k,p_so4aj)=AEROSOL( LSO4ACC )*1.e6
      tr_up(i,k,p_nh4aj)=AEROSOL( LNH4ACC )*1.e6
      tr_up(i,k,p_no3aj)=AEROSOL( LNO3ACC ) *1.e6

      tr_pw(i,k,p_so2)=gaswdep(lso2)*1.e6 
      tr_pw(i,k,p_sulf)=gaswdep(lh2so4)*1.e6
      tr_pw(i,k,p_nh3)=gaswdep(lnh3)*1.e6
      tr_pw(i,k,p_h2o2)=gaswdep(lh2o2)*1.e6
 
      tr_pw(i,k,p_o3)=gaswdep(lo3)*1.e6
      tr_pw(i,k,p_paa)=gaswdep(lpaa)*1.e6
      tr_pw(i,k,p_hno3)=gaswdep(lhno3)*1.e6
      tr_pw(i,k,p_n2o5)=gaswdep(ln2o5)*1.e6
      tr_pw(i,k,p_so4aj)=AERwdep( LSO4ACC )*1.e6
      tr_pw(i,k,p_nh4aj)=AERwdep( LNH4ACC )*1.e6
      tr_pw(i,k,p_no3aj)=AERwdep( LNO3ACC ) *1.e6
      if(i.eq.ipr.and.j.eq.jpr)then
          write(6,*)'a',tr_up(i,k,npr),tracer(i,K-1,npr),tr_pw(i,k,npr)
      endif
      else    ! NOT MADE SORGAM
       do nv=2,num_chem
! we definitely need wet deposition for sulf
! tr_c would be conc_mxr for other tracers, like:
!              partialp=1.e-6*qc(i,k)*29./wtm(name)*p(i,k)/1013.
!              conc_equi=partialp*hstary(i,k)
!
!--- conc_mxr would be "my" qc-qrch (Kg/Kg)
!
!              conc_mxr=conc_equi*cupclw(i,j,k)*wtm(name)

! setting it to zero, takes away wet deposition for now
          tr_c(i,k,nv)=0.
          tr_pw(i,k,nv)=c0*dz*tr_C(I,K,nv)*zu(i,k)
        if(tr_c(i,k,nv).le.0.)then
          tr_c(i,k,nv)=0.
        endif
!
! here comes nonzero stuff
!
        if(nv.eq.p_sulf.and.p_sulf.gt.1.and.chemopt.ne.300.and.chemopt.ne.301)then
               tr_c(i,k,nv)=tr_up(i,k,nv)
               trch=tr_up(i,k,nv)-tr_c(i,k,nv)
               trcc=(tr_up(i,k,nv)-trch)/(1.+c0*dz*zu(i,k))
               tr_pw(i,k,nv)=c0*dz*trcc*zu(i,k)
               tr_up(i,k,nv)=trcc
!
! for gocart assuming scavenging factor of .6
!
        else if((chemopt.ge.300.and.nv.gt.numgas).or.(chemopt.ge.300.and.nv.eq.p_sulf))then
               tr_c(i,k,nv)=.6*tr_up(i,k,nv)
               trch=tr_up(i,k,nv)-tr_c(i,k,nv)
               trcc=(tr_up(i,k,nv)-trch)/(1.+c0*dz*zu(i,k))
               tr_pw(i,k,nv)=c0*dz*trcc*zu(i,k)
               tr_up(i,k,nv)=trcc+trch  !conc total = conc in liq water      (= trcc )
                                        !            + conc in air            (= trch    )

        endif
        enddo   ! enddo nv
!
!--- iall.eq.1, if all cloudwater goes to rain
!
!       if(iall.eq.1)then
!         tr_c(i,k,nv)=0.
!         tr_pw(i,k,nv)=(tr_c(I,K,nv)-QRCH)*zu(i,k)
!         if(tr_pw(i,k,nv).lt.0.)tr_pw(i,k,nv)=0.
!       endif
      endif ! END CHEMOPT

 100     CONTINUE


END subroutine cup_up_tracer



   SUBROUTINE  cup_dd_tracer(ierr,z_cup,qrcd,tracer,tre_cup,tr_up,tr_dd, &
                          tr_pw,tr_pwd,jmin,cdd,entr,zd,pwdper,k22,      &
                          num_chem,ids,ide, jds,jde, kds,kde, &
                          ims,ime, jms,jme, kms,kme, &
                          its,ite, jts,jte, kts,kte)
! USE module_configure
! USE module_state_description
        implicit none
!
!  on input
!
  
   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
                         num_chem,ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                   ::                           &
       pwdper,zd,cdd,qrcd,z_cup 
     real,    dimension (its:ite,kts:kte,1:num_chem)                              &
        ,intent (inout  )                   ::                           &
        tr_dd,tr_pwd,tr_up
     real,    dimension (its:ite,kts:kte,1:num_chem)                              &
        ,intent (in  )                   ::                           &
        tre_cup,tracer,tr_pw
     real,    dimension (its:ite,1:num_chem) :: pwav

  ! entr= entrainment rate
     real                                                              &
        ,intent (in   )                   ::                           &
        entr
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        jmin
   ! ierr error value, maybe modified in this routine
  
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr,k22
!  local variables in this routine
!

     integer                              ::                           &
        iall,i,k,nv,ki
     real                                 ::                           &
        dh,qrch,c0,dz,radius
     integer                              ::                           &
       itf,ktf
         logical iaer (num_chem)
         iaer = .false.

        if(p_so4aj.gt.1)iaer(p_so4aj) = .true.
        if(p_nh4aj.gt.1)iaer(p_nh4aj) = .true.
        if(p_no3aj.gt.1)iaer(p_no3aj) = .true.

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)
!
      qrch=0.
      do nv=2,num_chem
      do k=kts+0,kte
      do i=its,ite
         tr_dd(i,k,nv)=0.
         tr_pwd(i,k,nv)=0.
      enddo
      enddo
      do i=its,ite
      pwav(i,nv)=0.
      IF(ierr(I).eq.0)then
      do k=kts,ktf
        pwav(i,nv)=pwav(i,nv)+tr_pw(i,k,nv)
      enddo
      endif
      enddo
      enddo
!
!--- in downdraft, do only transport of tracers, other
!--- than evaporation of part of the rainwater (see below)
!
!
      do 100 i=its,ite
      IF(ierr(I).eq.0)then
!
!--- assume no gas takeup by rain during falling
!--- for now
!
!
      if(jmin(i).lt.3)write(0,*)i,jmin(i)
      do nv=2,num_chem
         tr_dd(i,jmin(i),nv)=tre_cup(i,jmin(i),nv)
      enddo
      do ki=jmin(i)-1,1,-1
         DZ=Z_cup(i,Ki+1)-Z_cup(i,Ki)
         do nv=2,num_chem
         tr_pwd(i,jmin(i),nv)=0.
         tr_dd(i,Ki,nv)=(tr_dd(i,Ki+1,nv)*(1.-.5*CDD(i,Ki)*DZ) &
                  +entr*DZ*tracer(i,Ki,nv)                     &
                  )/(1.+entr*DZ-.5*CDD(i,Ki)*DZ)
!
!--- if tracer conserved
!
         qrch=tr_dd(i,Ki,nv)
!
!--- part of dissolved liquid phase material that is being evaporated
!     need percentage of rainwater that evaporates at level
!     pwdper
!     qcd=qcd+pwdper
!
!        tr_pwd(i,ki,nv)=pwdper(i,ki)*pwav(i,nv)
         if(iaer(nv))then
             tr_pwd(i,ki,nv)=0.
         else
             tr_pwd(i,ki,nv)=pwdper(i,ki)*pwav(i,nv)
         endif
         tr_dd(i,ki,nv)=qrch+tr_pwd(i,ki,nv)
      enddo
!
!--- end loop over nv
      enddo
      endif
100    continue

END subroutine cup_dd_tracer




   SUBROUTINE neg_check_ct(pret,ktop,epsilc,dt,q,outq,iopt,num_chem,    &
                           its,ite,kts,kte,itf,ktf,ipr,jpr,npr,j)

   INTEGER,      INTENT(IN   ) ::   iopt,num_chem,its,ite,kts,kte,itf,ktf,ipr,jpr,npr,j

     real, dimension (its:ite,kts:kte,num_chem  )          ,                  &
      intent(inout   ) ::                                                     &
       q,outq
     real, dimension (its:ite  )          ,                                   &
      intent(in      ) ::                                                     &
       pret
     integer, dimension (its:ite  )          ,                                &
      intent(in   ) ::                                                        &
      ktop
     real                                                                     &
        ,intent (in  )                   ::                                   &
        dt,epsilc
     real :: tracermin,tracermax,thresh,qmem,qmemf,qmem2,qtest,qmem1
!
! check whether routine produces negative q's. This can happen, since 
! tendencies are calculated based on forced q's. This should have no
! influence on conservation properties, it scales linear through all
! tendencies. Use iopt=0 to test for each tracer seperately, iopt=1
! for a more severe limitation...
!
      thresh=epsilc
!     thresh=1.e-30
      if(iopt.eq.0)then
      do nv=2,num_chem
      do 100 i=its,itf
         if(pret(i).le.0.)go to 100
         tracermin=q(i,kts,nv)
         tracermax=q(i,kts,nv)
         do k=kts+1,kte-1
           tracermin=min(tracermin,q(i,k,nv))
           tracermax=max(tracermax,q(i,k,nv))
         enddo
         tracermin=max(tracermin,thresh)
         qmemf=1.
!
! first check for minimum restriction
!
         do k=kts,ktop(i)
!
! tracer tendency
!
            qmem=outq(i,k,nv)
!
! only necessary if there is a tendency
!
            if(qmem.lt.0.)then
               qtest=q(i,k,nv)+outq(i,k,nv)*dt
               if(qtest.lt.tracermin)then
!
! qmem2 would be the maximum allowable tendency
!
                    qmem1=outq(i,k,nv)
                    qmem2=(tracermin-q(i,k,nv))/dt
                    qmemf=min(qmemf,qmem2/qmem1)
                    if(qmemf.gt.1.)print *,'something wrong in negct_1',qmem2,qmem1
                    if(i.eq.ipr.and.j.eq.jpr.and.nv.eq.npr)then
                      print *,k,qtest,qmem2,qmem1,qmemf
                    endif
                    qmemf=max(qmemf,0.)
               endif
            endif
         enddo
         do k=kts,ktop(i)
            outq(i,k,nv)=outq(i,k,nv)*qmemf
         enddo
!
! now check max
!
         qmemf=1.
         do k=kts,ktop(i)
!
! tracer tendency
!
            qmem=outq(i,k,nv)
!
! only necessary if there is a tendency
!
            if(qmem.gt.0.)then
               qtest=q(i,k,nv)+outq(i,k,nv)*dt
               if(qtest.gt.tracermax)then
!
! qmem2 would be the maximum allowable tendency
!
                    qmem1=outq(i,k,nv)
                    qmem2=(tracermax-q(i,k,nv))/dt
                    qmemf=min(qmemf,qmem2/qmem1)
                    if(qmemf.gt.1.)print *,'something wrong in negct_2',qmem2,qmem1
                    if(i.eq.ipr.and.j.eq.jpr.and.nv.eq.npr)then
                      print *,'2',k,qtest,qmem2,qmem1,qmemf
                    endif
                    qmemf=max(qmemf,0.)
               endif
            endif
         enddo
         do k=kts,ktop(i)
            outq(i,k,nv)=outq(i,k,nv)*qmemf
         enddo
 100  continue
      enddo
!
! ELSE
!
      elseif(iopt.eq.1)then
      do i=its,itf
      qmemf=1.
      do k=kts,ktop(i)
      do nv=2,num_chem
!
! tracer tendency
!
         qmem=outq(i,k,nv)
!
! only necessary if tendency is larger than zero
!
         if(qmem.lt.0.)then
         qtest=q(i,k,nv)+outq(i,k,nv)*dt
         if(qtest.lt.thresh)then
!
! qmem2 would be the maximum allowable tendency
!
           qmem1=outq(i,k,nv)
           qmem2=(thresh-q(i,k,nv))/dt
           qmemf=min(qmemf,qmem2/qmem1)
           qmemf=max(0.,qmemf)
         endif
         endif
      enddo
      enddo
      do nv=2,num_chem
      do k=kts,ktop(i)
         outq(i,k,nv)=outq(i,k,nv)*qmemf
      enddo
      enddo
      enddo
      endif

   END SUBROUTINE neg_check_ct


!-------------------------------------------------------
END MODULE module_ctrans_grell
