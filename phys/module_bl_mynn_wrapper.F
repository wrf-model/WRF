!> \file module_bl_mynn_wrapper.F90
!!  This serves as the interface between the WRF PBL driver and the MYNN 
!!  eddy-diffusivity mass-flux scheme in module_bl_mynn.F. 

!>\ingroup gsd_mynn_edmf
!> The following references best describe the code within
!!    Olson et al. (2019, NOAA Technical Memorandum)
!!    Nakanishi and Niino (2009) \cite NAKANISHI_2009
      MODULE module_bl_mynn_wrapper

      use module_bl_mynn_common

      contains

!> \section arg_table_mynnedmf_wrapper_init Argument Table
!! \htmlinclude mynnedmf_wrapper_init.html
!!
      subroutine mynnedmf_wrapper_init (              &
        &  RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,&
        &  RQIBLTEN,QKE,                              &
        &  restart,allowed_to_read,                   &
        &  P_QC,P_QI,PARAM_FIRST_SCALAR,              &
        &  IDS,IDE,JDS,JDE,KDS,KDE,                   &
        &  IMS,IME,JMS,JME,KMS,KME,                   &
        &  ITS,ITE,JTS,JTE,KTS,KTE                    )

        implicit none
        
        LOGICAL,INTENT(IN) :: ALLOWED_TO_READ,RESTART

        INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,  &
             &                IMS,IME,JMS,JME,KMS,KME,  &
             &                ITS,ITE,JTS,JTE,KTS,KTE


        REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: &
             &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,                 &
             &RQCBLTEN,RQIBLTEN,QKE

        INTEGER,  intent(in) :: P_QC,P_QI,PARAM_FIRST_SCALAR

        INTEGER :: I,J,K,ITF,JTF,KTF

        JTF=MIN0(JTE,JDE-1)
        KTF=MIN0(KTE,KDE-1)
        ITF=MIN0(ITE,IDE-1)

        IF (.NOT.RESTART) THEN
         DO J=JTS,JTF
          DO K=KTS,KTF
           DO I=ITS,ITF
              RUBLTEN(i,k,j)=0.
              RVBLTEN(i,k,j)=0.
              RTHBLTEN(i,k,j)=0.
              RQVBLTEN(i,k,j)=0.
              if( p_qc >= param_first_scalar ) RQCBLTEN(i,k,j)=0.
              if( p_qi >= param_first_scalar ) RQIBLTEN(i,k,j)=0.
           ENDDO
          ENDDO
         ENDDO
        ENDIF

      end subroutine mynnedmf_wrapper_init

      subroutine mynnedmf_wrapper_finalize ()
      end subroutine mynnedmf_wrapper_finalize

! \brief This scheme (1) performs pre-mynnedmf work, (2) runs the mynnedmf, and (3) performs post-mynnedmf work
!> \section arg_table_mynnedmf_wrapper_run Argument Table
!! \htmlinclude mynnedmf_wrapper_run.html
!!
SUBROUTINE mynnedmf_wrapper_run(                 &
     &  initflag,restart,cycling,                &
     &  delt,dz,dxc,znt,                         &
     &  u,v,w,th,                                &
     &  qv,qc,qi,qnc,qni,qnwfa,qnifa,qnbca,      &
!     &  ozone,                                   &
     &  p,exner,rho,t3d,                         &
     &  xland,ts,qsfc,ps,                        &
     &  ust,ch,hfx,qfx,rmol,wspd,                &
     &  uoce,voce,                               &
     &  qke,qke_adv,sh3d,sm3d,                   &
!--- chem/smoke
#if (WRF_CHEM == 1)
     &  mix_chem,chem3d,vd3d,nchem,kdvel,        &
     &  ndvel,num_vert_mix,                      &
!     &  frp_mean,emis_ant_no,enh_mix,            & !to be included soon
#endif
!--- end chem/smoke 
     &  Tsq,Qsq,Cov,                             &
     &  rublten,rvblten,rthblten,                &
     &  rqvblten,rqcblten,rqiblten,              &
     &  rqncblten,rqniblten,                     &
     &  rqnwfablten,rqnifablten,rqnbcablten,     &
!     &  ro3blten,                                &
     &  exch_h,exch_m,pblh,kpbl,el_pbl,          &
     &  dqke,qwt,qshear,qbuoy,qdiss,             &
     &  qc_bl,qi_bl,cldfra_bl,                   &
     &  edmf_a,edmf_w,edmf_qt,                   &
     &  edmf_thl,edmf_ent,edmf_qc,               &
     &  sub_thl3d,sub_sqv3d,                     &
     &  det_thl3d,det_sqv3d,                     &
     &  nupdraft,maxMF,ktop_plume,               &
     &  rthraten,                                &
     &  tke_budget,         bl_mynn_tkeadvect,   &
     &  bl_mynn_cloudpdf,   bl_mynn_mixlength,   &
     &  icloud_bl,          bl_mynn_edmf,        &
     &  bl_mynn_edmf_mom,   bl_mynn_edmf_tke,    &
     &  bl_mynn_cloudmix,   bl_mynn_mixqt,       &
     &  bl_mynn_output,     bl_mynn_closure,     &
     &  bl_mynn_mixscalars,                      &
     &  spp_pbl,pattern_spp_pbl,                 &
     &  flag_qc,flag_qi,                         &
     &  flag_qnc,flag_qni,                       &
     &  flag_qnwfa,flag_qnifa,flag_qnbca,        &
     &  ids,ide,jds,jde,kds,kde,                 &
     &  ims,ime,jms,jme,kms,kme,                 &
     &  its,ite,jts,jte,kts,kte                  )

!     use module_gfs_machine,  only : kind_phys
     use module_bl_mynn, only: mynn_bl_driver

!------------------------------------------------------------------- 
     implicit none
!------------------------------------------------------------------- 

     !smoke/chem: disclaimer: all smoke-related variables are still
     !considered under development in CCPP. Until that work is
     !completed, these flags/arrays must be kept hard-coded as is.
#if (WRF_CHEM == 1)
     logical, intent(in) :: mix_chem
     integer, intent(in) :: nchem, ndvel, kdvel, num_vert_mix
     logical, parameter ::                                  &
     &       rrfs_sd    =.false.,                           &
     &       smoke_dbg  =.false.,                           &
     &       enh_mix    =.false.
#else
     logical, parameter ::                                  &
     &       mix_chem   =.false.,                           &
     &       enh_mix    =.false.,                           &
     &       rrfs_sd    =.false.,                           &
     &       smoke_dbg  =.false.
     integer, parameter :: nchem=2, ndvel=2, kdvel=1,       &
             num_vert_mix = 1
#endif

! NAMELIST OPTIONS (INPUT):
     logical, intent(in) ::                                 &
     &       bl_mynn_tkeadvect,                             &
     &       cycling
      integer, intent(in) ::                                &
     &       bl_mynn_cloudpdf,                              &
     &       bl_mynn_mixlength,                             &
     &       icloud_bl,                                     &
     &       bl_mynn_edmf,                                  &
     &       bl_mynn_edmf_mom,                              &
     &       bl_mynn_edmf_tke,                              &
     &       bl_mynn_cloudmix,                              &
     &       bl_mynn_mixqt,                                 &
     &       bl_mynn_output,                                &
     &       bl_mynn_mixscalars,                            &
     &       spp_pbl,                                       &
     &       tke_budget
      real, intent(in) ::                                   &
     &       bl_mynn_closure

      logical, intent(in) ::                                &
     &       FLAG_QI, FLAG_QNI, FLAG_QC, FLAG_QNC,          &
     &       FLAG_QNWFA, FLAG_QNIFA, FLAG_QNBCA
      logical, parameter :: FLAG_OZONE = .false.

!MYNN-1D
      REAL,    intent(in) :: delt, dxc
      LOGICAL, intent(in) :: restart
      INTEGER :: i, j, k, itf, jtf, ktf, n
      INTEGER, intent(in) :: initflag,                      &
     &           IDS,IDE,JDS,JDE,KDS,KDE,                   &
     &           IMS,IME,JMS,JME,KMS,KME,                   &
     &           ITS,ITE,JTS,JTE,KTS,KTE

!MYNN-3D
      real, dimension(ims:ime,kms:kme,jms:jme), intent(in) ::          &
     &       u,v,w,t3d,th,rho,exner,p,dz
      real, dimension(ims:ime,kms:kme,jms:jme), intent(inout) ::       &
     &       rublten,rvblten,rthblten,                                 &
     &       rqvblten,rqcblten,rqiblten,                               &
     &       rqncblten,rqniblten,                                      &
     &       rqnwfablten,rqnifablten,rqnbcablten !,ro3blten
      real, dimension(ims:ime,kms:kme,jms:jme), intent(inout) ::       &
     &       qke, qke_adv, el_pbl, sh3d, sm3d
      real, dimension(ims:ime,kms:kme,jms:jme), intent(inout) ::       &
     &       Tsq, Qsq, Cov, exch_h, exch_m
      real, dimension(ims:ime,kms:kme,jms:jme), intent(in) :: rthraten

!optional 3D arrays
      real, dimension(ims:ime,kms:kme,jms:jme), optional, intent(in)  ::    &
     &       pattern_spp_pbl
      real, dimension(ims:ime,kms:kme,jms:jme), optional, intent(inout) ::  &
     &       qc_bl, qi_bl, cldfra_bl
      real, dimension(ims:ime,kms:kme,jms:jme), optional, intent(inout) ::  &
     &       edmf_a,edmf_w,edmf_qt,                                    &
     &       edmf_thl,edmf_ent,edmf_qc,                                &
     &       sub_thl3d,sub_sqv3d,det_thl3d,det_sqv3d
      real, dimension(ims:ime,kms:kme,jms:jme), optional, intent(inout) ::  &
     &       dqke,qWT,qSHEAR,qBUOY,qDISS
      real, dimension(ims:ime,kms:kme,jms:jme), optional, intent(inout) ::  &
     &       qv,qc,qi,qnc,qni,qnwfa,qnifa,qnbca!,o3

!optional 2D arrays for passing into module_bl_myn.F
      real, allocatable, dimension(:,:) ::                             &
     &       qc_bl2d, qi_bl2d, cldfra_bl2d, pattern_spp_pbl2d
      real, allocatable, dimension(:,:) ::                             &
     &       edmf_a2d,edmf_w2d,edmf_qt2d,                              &
     &       edmf_thl2d,edmf_ent2d,edmf_qc2d,                          &
     &       sub_thl2d,sub_sqv2d,det_thl2d,det_sqv2d
      real, allocatable, dimension(:,:) ::                             &
     &       dqke2d,qWT2d,qSHEAR2d,qBUOY2d,qDISS2d
      real, allocatable, dimension(:,:) ::                             &
     &       qc2d,qi2d,qnc2d,qni2d,qnwfa2d,qnifa2d,qnbca2d!,o32d

!smoke/chem arrays - no if-defs in module_bl_mynn.F, so must define arrays
#if (WRF_CHEM == 1)
      real, dimension(ims:ime,kms:kme,jms:jme,nchem), intent(in) :: chem3d
      real, dimension(ims:ime,kdvel,jms:jme, ndvel), intent(in)  :: vd3d
      real, dimension(ims:ime,kms:kme,nchem) :: chem
      real, dimension(ims:ime,ndvel)         :: vd
      real, dimension(ims:ime)               :: frp_mean, emis_ant_no
#else
      real, dimension(ims:ime,kms:kme,nchem) :: chem
      real, dimension(ims:ime,ndvel)         :: vd
      real, dimension(ims:ime) :: frp_mean, emis_ant_no
#endif

!MYNN-2D
      real, dimension(ims:ime,jms:jme), intent(in) ::                  &
     &        xland,ts,qsfc,ps,ch
      real, dimension(ims:ime,jms:jme), intent(inout) ::               &
     &        znt,pblh,maxmf,rmol,hfx,qfx,ust,wspd,                    &
     &        uoce,voce
      integer, dimension(ims:ime,jms:jme), intent(inout) ::            &
     &        kpbl,nupdraft,ktop_plume

!Local
      real, dimension(ims:ime,kms:kme)         :: delp,sqv,sqc,sqi
      real, dimension(ims:ime)                 :: dx
      logical, parameter                       :: debug = .false.
      real, dimension(ims:ime,kms:kme,jms:jme) :: ozone,r03blten

   !write(0,*)"=============================================="
   !write(0,*)"in mynn wrapper..."
   !write(0,*)"initflag=",initflag
   !write(0,*)"restart =",restart

   jtf=MIN0(JTE,JDE-1)
   ktf=MIN0(KTE,KDE-1)
   itf=MIN0(ITE,IDE-1)

   !For now, initialized bogus array
   ozone=0.0
   r03blten=0.0

   !Allocate any arrays being used
   if (icloud_bl > 0) then
      allocate(qc_bl2d(ims:ime,kms:kme))
      allocate(qi_bl2d(ims:ime,kms:kme))
      allocate(cldfra_bl2d(ims:ime,kms:kme))
      qc_bl2d=0.0
      qi_bl2d=0.0
      cldfra_bl2d=0.0
   endif
   if (spp_pbl > 0) then
      allocate(pattern_spp_pbl2d(ims:ime,kms:kme))
   endif
   if (bl_mynn_output > 0) then
      allocate(edmf_a2d(ims:ime,kms:kme))
      allocate(edmf_w2d(ims:ime,kms:kme))
      allocate(edmf_qt2d(ims:ime,kms:kme))
      allocate(edmf_thl2d(ims:ime,kms:kme))
      allocate(edmf_ent2d(ims:ime,kms:kme))
      allocate(edmf_qc2d(ims:ime,kms:kme))
      allocate(sub_thl2d(ims:ime,kms:kme))
      allocate(sub_sqv2d(ims:ime,kms:kme))
      allocate(det_thl2d(ims:ime,kms:kme))
      allocate(det_sqv2d(ims:ime,kms:kme))
   endif
   if (tke_budget .eq. 1) then
      allocate(dqke2d(ims:ime,kms:kme))
      allocate(qWT2d(ims:ime,kms:kme))
      allocate(qSHEAR2d(ims:ime,kms:kme))
      allocate(qBUOY2d(ims:ime,kms:kme))
      allocate(qDISS2d(ims:ime,kms:kme))
      dqke2d  =0.0
      qWT2d   =0.0
      qSHEAR2d=0.0
      qBUOY2d =0.0
      qDISS2d =0.0
   endif
   if (flag_qc) then
      allocate(qc2d(ims:ime,kms:kme))
      qc2d=0.0
   endif
   if (flag_qi) then
      allocate(qi2d(ims:ime,kms:kme))
      qi2d=0.0
   endif
   if (flag_qnc) then
      allocate(qnc2d(ims:ime,kms:kme))
      qnc2d=0.0
   endif
   if (flag_qni) then
      allocate(qni2d(ims:ime,kms:kme))
      qni2d=0.0
   endif
   if (flag_qnwfa) then
      allocate(qnwfa2d(ims:ime,kms:kme))
      qnwfa2d=0.0
   endif
   if (flag_qnifa) then
      allocate(qnifa2d(ims:ime,kms:kme))
      qnifa2d=0.0
   endif
   if (flag_qnbca) then
      allocate(qnbca2d(ims:ime,kms:kme))
      qnbca2d=0.0
   endif
   !---------------------------------
   !Begin looping in the j-direction
   !---------------------------------
   do j = jts,jtf

      !need sgs cloud info input for diagnostic-decay
      if (icloud_bl > 0) then
         do k=kts,ktf
         do i=its,itf
            qc_bl2d(i,k)     = qc_bl(i,k,j)
            qi_bl2d(i,k)     = qi_bl(i,k,j)
            cldfra_bl2d(i,k) = cldfra_bl(i,k,j)
         enddo
         enddo
      endif

      !spp input
      if (spp_pbl > 0) then
         do k=kts,ktf
         do i=its,itf
            pattern_spp_pbl2d(i,k) = pattern_spp_pbl(i,k,j)
         enddo
         enddo
      endif

      !intialize moist species
      if (flag_qc) then
         do k=kts,ktf
         do i=its,itf
            qc2d(i,k) = qc(i,k,j)
         enddo
         enddo
      endif
      if (flag_qi) then
         do k=kts,ktf
         do i=its,itf
            qi2d(i,k) = qi(i,k,j)
         enddo
         enddo
      endif
      if (flag_qnc) then
         do k=kts,ktf
         do i=its,itf
            qnc2d(i,k) = qnc(i,k,j)
         enddo
         enddo
      endif
      if (flag_qni) then
         do k=kts,ktf
         do i=its,itf
            qni2d(i,k) = qni(i,k,j)
         enddo
         enddo
      endif
      if (flag_qnwfa) then
         do k=kts,ktf
         do i=its,itf
            qnwfa2d(i,k) = qnwfa(i,k,j)
         enddo
         enddo
      endif
      if (flag_qnifa) then
         do k=kts,ktf
         do i=its,itf
            qnifa2d(i,k) = qnifa(i,k,j)
         enddo
         enddo
      endif
      if (flag_qnbca) then
         do k=kts,ktf
         do i=its,itf
            qnbca2d(i,k) = qnbca(i,k,j)
         enddo
         enddo
      endif

#if (WRF_CHEM == 1)
      if (mix_chem) then
         do n=1,nchem
         do k=kts,ktf
         do i=its,itf
            chem(i,k,n)=chem3d(i,k,j,n)
         enddo
         enddo
         enddo

         !set kdvel =1
         do n=1,ndvel
         do i=its,itf
            vd(i,n)  =vd3d(i,1,j,n)
         enddo
         enddo
      endif
      frp_mean = 0.0
      emis_ant_no = 0.0
#else
      chem     = 0.0
      vd       = 0.0
      frp_mean = 0.0
      emis_ant_no = 0.0
#endif

      ! Check incoming moist species to ensure non-negative values
      ! First, create pressure differences (delp) across model layers
      do i=its,itf
         dx(i)=dxc
!         delp(i,1)  = ps(i,j) - (p(i,2,j)*dz(i,1,j) + p(i,1,j)*dz(i,2,j))/(dz(i,1,j)+dz(i,2,j))
!         do k=2,kte-1
!            delp(i,k) = (p(i,k,j)*dz(i,k-1,j) + p(i,k-1,j)*dz(i,k,j))/(dz(i,k,j)+dz(i,k-1,j)) - &
!                        (p(i,k+1,j)*dz(i,k,j) + p(i,k,j)*dz(i,k+1,j))/(dz(i,k,j)+dz(i,k+1,j))
!         enddo
!         delp(i,kte) = delp(i,kte-1)
      enddo

!      do i=its,itf
!         call moisture_check2(kte, delt,                 &
!                              delp(i,:), exner(i,:,j),   &
!                              qv(i,:,j), qc(i,:,j),      &
!                              qi(i,:,j), t3d(i,:,j)      )
!      enddo

      !In WRF, mixing ratio is incoming. Convert to specific humidity:
       do k=kts,ktf
          do i=its,itf
             sqv(i,k)=qv(i,k,j)/(1.0 + qv(i,k,j))
             sqc(i,k)=qc2d(i,k)/(1.0 + qv(i,k,j))
             sqi(i,k)=qi2d(i,k)/(1.0 + qv(i,k,j))
         enddo
      enddo

!      do i=its,ite
!         ts(i,j)=tsurf(i,j)/exner(i,1,j)  !theta
!      enddo

      if (debug) then
         print*
         write(0,*)"===CALLING mynn_bl_driver; input:"
         print*,"tke_budget=",tke_budget
         print*,"bl_mynn_tkeadvect=",bl_mynn_tkeadvect
         print*,"bl_mynn_cloudpdf=",bl_mynn_cloudpdf
         print*,"bl_mynn_mixlength=",bl_mynn_mixlength
         print*,"bl_mynn_edmf=",bl_mynn_edmf
         print*,"bl_mynn_edmf_mom=",bl_mynn_edmf_mom
         print*,"bl_mynn_edmf_tke=",bl_mynn_edmf_tke
         print*,"bl_mynn_cloudmix=",bl_mynn_cloudmix
         print*,"bl_mynn_mixqt=",bl_mynn_mixqt
         print*,"icloud_bl=",icloud_bl
         print*,"T:",t3d(its,1,j),t3d(its,2,j),t3d(its,kte,j)
         print*,"TH:",th(its,1,j),th(its,2,j),th(its,kte,j)
         print*,"rho:",rho(its,1,j),rho(its,2,j),rho(its,kte,j)
         print*,"exner:",exner(its,1,j),exner(its,2,j),exner(its,kte,j)
         print*,"p:",p(its,1,j),p(its,2,j),p(its,kte,j)
         print*,"dz:",dz(its,1,j),dz(its,2,j),dz(its,kte,j)
         print*,"u:",u(its,1,j),u(its,2,j),u(its,kte,j)
         print*,"v:",v(its,1,j),v(its,2,j),v(its,kte,j)
         print*,"sqv:",sqv(its,1),sqv(its,2),sqv(its,kte)
         print*,"sqc:",sqc(its,1),sqc(its,2),sqc(its,kte)
         print*,"sqi:",sqi(its,1),sqi(its,2),sqi(its,kte)
         print*,"rmol:",rmol(its,j)," ust:",ust(its,j)
         print*,"dx=",dx(its),"initflag=",initflag
         print*,"Thetasurf:",ts(its,j)
         print*,"HFX:",hfx(its,j)," qfx",qfx(its,j)
         print*,"qsfc:",qsfc(its,j)," ps:",ps(its,j)
         print*,"wspd:",wspd(its,j)
         print*,"znt:",znt(its,j)," delt=",delt
         print*,"ite=",ite," kte=",kte
         print*,"PBLH=",pblh(its,j)," KPBL=",KPBL(its,j)," xland=",xland(its,j)
         print*," ch=",ch(its,j)
         print*,"qke:",qke(its,1,j),qke(its,2,j),qke(its,kte,j)
         print*,"el_pbl:",el_pbl(its,1,j),el_pbl(its,2,j),el_pbl(its,kte,j)
         print*,"Sh3d:",Sh3d(its,1,j),sh3d(its,2,j),sh3d(its,kte,j)
         print*,"max cf_bl:",maxval(cldfra_bl(its,:,j))
      endif

!print*,"In mynn wrapper, calling mynn_bl_driver"
              CALL  mynn_bl_driver(                                    &
     &        initflag=initflag,restart=restart,cycling=cycling,       &
     &        delt=delt,dz=dz(:,:,j),dx=dx,znt=znt(:,j),               &
     &        u=u(:,:,j),v=v(:,:,j),w=w(:,:,j),                        &
     &        th=th(:,:,j),sqv3D=sqv,sqc3D=sqc,                        &
     &        sqi3D=sqi,qnc=qnc2d,qni=qni2d,                           &
     &        qnwfa=qnwfa2d,qnifa=qnifa2d,                             &
     &        ozone=ozone(:,:,j),                                      &
     &        p=p(:,:,j),exner=exner(:,:,j),rho=rho(:,:,j),            &
     &        T3D=t3d(:,:,j),xland=xland(:,j),                         &
     &        ts=ts(:,j),qsfc=qsfc(:,j),ps=ps(:,j),                    &
     &        ust=ust(:,j),ch=ch(:,j),hfx=hfx(:,j),qfx=qfx(:,j),       &
     &        rmol=rmol(:,j),wspd=wspd(:,j),                           &
     &        uoce=uoce(:,j),voce=voce(:,j),                           & !input
     &        qke=QKE(:,:,j),qke_adv=qke_adv(:,:,j),                   & !output
     &        sh3d=Sh3d(:,:,j),sm3d=Sm3d(:,:,j),                       & !output
     &        nchem=nchem,kdvel=kdvel,ndvel=ndvel,                     & !chem/smoke
     &        Chem3d=chem,Vdep=vd,                                     &
     &        FRP=frp_mean,EMIS_ANT_NO=emis_ant_no,                    &
     &        mix_chem=mix_chem,enh_mix=enh_mix,                       &
     &        rrfs_sd=rrfs_sd,smoke_dbg=smoke_dbg,                     & !end chem/smoke
     &        tsq=tsq(:,:,j),qsq=qsq(:,:,j),cov=cov(:,:,j),            & !output
     &        RUBLTEN=RUBLTEN(:,:,j),RVBLTEN=RVBLTEN(:,:,j),           & !output
     &        RTHBLTEN=RTHBLTEN(:,:,j),RQVBLTEN=RQVBLTEN(:,:,j),       & !output
     &        RQCBLTEN=rqcblten(:,:,j),RQIBLTEN=rqiblten(:,:,j),       & !output
     &        RQNCBLTEN=rqncblten(:,:,j),RQNIBLTEN=rqniblten(:,:,j),   & !output
     &        RQNWFABLTEN=RQNWFABLTEN(:,:,j),                          & !output
     &        RQNIFABLTEN=RQNIFABLTEN(:,:,j),                          & !output
     &        RQNBCABLTEN=RQNBCABLTEN(:,:,j),                          & !output
     &        dozone=r03blten(:,:,j),                                  & !output
     &        EXCH_H=exch_h(:,:,j),EXCH_M=exch_m(:,:,j),               & !output
     &        pblh=pblh(:,j),KPBL=KPBL(:,j),                           & !output
     &        el_pbl=el_pbl(:,:,j),                                    & !output
     &        dqke=dqke2d,qWT=qWT2d,qSHEAR=qSHEAR2d,                   & !output
     &        qBUOY=qBUOY2d,qDISS=qDISS2d,                             & !output
     &        qc_bl=qc_bl2d,qi_bl=qi_bl2d,cldfra_bl=cldfra_bl2d,       & !output
     &        bl_mynn_tkeadvect=bl_mynn_tkeadvect,                     & !input parameter
     &        tke_budget=tke_budget,                                   & !input parameter
     &        bl_mynn_cloudpdf=bl_mynn_cloudpdf,                       & !input parameter
     &        bl_mynn_mixlength=bl_mynn_mixlength,                     & !input parameter
     &        icloud_bl=icloud_bl,                                     & !input parameter
     &        closure=bl_mynn_closure,bl_mynn_edmf=bl_mynn_edmf,       & !input parameter
     &        bl_mynn_edmf_mom=bl_mynn_edmf_mom,                       & !input parameter
     &        bl_mynn_edmf_tke=bl_mynn_edmf_tke,                       & !input parameter
     &        bl_mynn_mixscalars=bl_mynn_mixscalars,                   & !input parameter
     &        bl_mynn_output=bl_mynn_output,                           & !input parameter
     &        bl_mynn_cloudmix=bl_mynn_cloudmix,                       & !input parameter
     &        bl_mynn_mixqt=bl_mynn_mixqt,                             & !input parameter
     &        edmf_a=edmf_a2d,edmf_w=edmf_w2d,                         & !output
     &        edmf_qt=edmf_qt2d,edmf_thl=edmf_thl2d,                   & !output
     &        edmf_ent=edmf_ent2d,edmf_qc=edmf_qc2d,                   & !output
     &        sub_thl3D=sub_thl2d,sub_sqv3D=sub_sqv2d,                 & !output
     &        det_thl3D=det_thl2d,det_sqv3D=det_sqv2d,                 & !output
     &        nupdraft=nupdraft(:,j),maxMF=maxMF(:,j),                 & !output
     &        ktop_plume=ktop_plume(:,j),                              & !output
     &        spp_pbl=spp_pbl,pattern_spp_pbl=pattern_spp_pbl2d,       & !input
     &        RTHRATEN=rthraten(:,:,j),                                & !input
     &        FLAG_QI=flag_qi,FLAG_QNI=flag_qni,                       & !input
     &        FLAG_QC=flag_qc,FLAG_QNC=flag_qnc,                       & !input
     &        FLAG_QNWFA=FLAG_QNWFA,FLAG_QNIFA=FLAG_QNIFA,             & !input
     &        FLAG_QNBCA=FLAG_QNBCA,                                   & !input
     &        IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde,         & !input
     &        IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme,         & !input
     &        ITS=its,ITE=itf,JTS=jts,JTE=jtf,KTS=kts,KTE=kte)           !input
!print*,"In mynn wrapper, after bl_mynn_driver"

     !- Convert spec hum to mixing ratio:
      do k=kts,ktf
        do i=its,itf
           RQVBLTEN(i,k,j) = RQVBLTEN(i,k,j)/(1.0 - sqv(i,k))
           RQCBLTEN(i,k,j) = RQCBLTEN(i,k,j)/(1.0 - sqv(i,k))
           RQIBLTEN(i,k,j) = RQIBLTEN(i,k,j)/(1.0 - sqv(i,k))
        enddo
      enddo

     !- Collect 3D ouput:
      if (icloud_bl > 0) then
         do k=kts,ktf
         do i=its,itf
            qc_bl(i,k,j)     = qc_bl2d(i,k)
            qi_bl(i,k,j)     = qi_bl2d(i,k)
            cldfra_bl(i,k,j) = cldfra_bl2d(i,k)
         enddo
         enddo
      endif

      if (tke_budget .eq. 1) then
         do k=kts,ktf
         do i=its,itf
            dqke(i,k,j)     = dqke2d(i,k)
            qwt(i,k,j)      = qwt2d(i,k)
            qshear(i,k,j)   = qshear2d(i,k)
            qbuoy(i,k,j)    = qbuoy2d(i,k)
            qdiss(i,k,j)    = qdiss2d(i,k)
         enddo
         enddo
      endif

      if (bl_mynn_output > 0) then
         do k=kts,ktf
         do i=its,itf
            edmf_a(i,k,j)     = edmf_a2d(i,k)
            edmf_w(i,k,j)     = edmf_w2d(i,k)
            edmf_qt(i,k,j)    = edmf_qt2d(i,k)
            edmf_thl(i,k,j)   = edmf_thl2d(i,k)
            edmf_ent(i,k,j)   = edmf_ent2d(i,k)
            edmf_qc(i,k,j)    = edmf_qc2d(i,k)
            sub_thl3d(i,k,j)  = sub_thl2d(i,k)
            sub_sqv3d(i,k,j)  = sub_sqv2d(i,k)
            det_thl3d(i,k,j)  = det_thl2d(i,k)
            det_sqv3d(i,k,j)  = det_sqv2d(i,k)
         enddo
         enddo
      endif

      if (debug) then
          print*
          print*,"===Finished with mynn_bl_driver; output:"
          print*,"T:",t3d(its,1,j),t3d(its,2,j),t3d(its,kte,j)
          print*,"TH:",th(its,1,j),th(its,2,j),th(its,kte,j)
          print*,"rho:",rho(its,1,j),rho(its,2,j),rho(its,kte,j)
          print*,"exner:",exner(its,1,j),exner(its,2,j),exner(its,kte,j)
          print*,"p:",p(its,1,j),p(its,2,j),p(its,kte,j)
          print*,"dz:",dz(its,1,j),dz(its,2,j),dz(its,kte,j)
          print*,"u:",u(its,1,j),u(its,2,j),u(its,kte,j)
          print*,"v:",v(its,1,j),v(its,2,j),v(its,kte,j)
          print*,"sqv:",sqv(its,1),sqv(its,2),sqv(its,kte)
          print*,"sqc:",sqc(its,1),sqc(its,2),sqc(its,kte)
          print*,"sqi:",sqi(its,1),sqi(its,2),sqi(its,kte)
          print*,"rmol:",rmol(its,j)," ust:",ust(its,j)
          print*,"dx(its,j)=",dx(its),"initflag=",initflag
          print*,"Thetasurf:",ts(its,j)
          print*,"HFX:",hfx(its,j)," qfx",qfx(its,j)
          print*,"qsfc:",qsfc(its,j)," ps:",ps(its,j)
          print*,"wspd:",wspd(its,j)
          print*,"znt:",znt(its,j)," delt=",delt
          print*,"im=",ite," kte=",kte
          print*,"PBLH=",pblh(its,j)," KPBL=",KPBL(its,j)," xland=",xland(its,j)
          print*,"ch=",ch(its,j)
          print*,"qke:",qke(its,1,j),qke(its,2,j),qke(its,kte,j)
          print*,"el_pbl:",el_pbl(its,1,j),el_pbl(its,2,j),el_pbl(its,kte,j)
          print*,"Sh3d:",Sh3d(its,1,j),sh3d(its,2,j),sh3d(its,kte,j)
          print*,"exch_h:",exch_h(its,1,j),exch_h(its,2,j),exch_h(its,kte,j)
          print*,"exch_m:",exch_m(its,1,j),exch_m(its,2,j),exch_m(its,kte,j)
          print*,"max cf_bl:",maxval(cldfra_bl(its,:,j))
          print*,"max qc_bl:",maxval(qc_bl(its,:,j))
          print*,"dtdt:",rthblten(its,1,j),rthblten(its,2,j),rthblten(its,kte,j)
          print*,"dudt:",rublten(its,1,j),rublten(its,2,j),rublten(its,kte,j)
          print*,"dvdt:",rvblten(its,1,j),rvblten(its,2,j),rvblten(its,kte,j)
          print*,"dqdt:",rqvblten(its,1,j),rqvblten(its,2,j),rqvblten(its,kte,j)
          print*,"ktop_plume:",ktop_plume(its,j)," maxmf:",maxmf(its,j)
          print*,"nup:",nupdraft(its,j)
          print*
       endif

   enddo  !end j-loop 

   !Deallocate all temporary interface arrays
   if (bl_mynn_output > 0) then
      deallocate(edmf_a2d)
      deallocate(edmf_w2d)
      deallocate(edmf_qt2d)
      deallocate(edmf_thl2d)
      deallocate(edmf_ent2d)
      deallocate(edmf_qc2d)
      deallocate(sub_thl2d)
      deallocate(sub_sqv2d)
      deallocate(det_thl2d)
      deallocate(det_sqv2d)
   endif
   if (tke_budget .eq. 1) then
      deallocate(dqke2d)
      deallocate(qwt2d)
      deallocate(qshear2d)
      deallocate(qbuoy2d)
      deallocate(qdiss2d)
   endif
   if (icloud_bl > 0) then
      deallocate(qc_bl2d)
      deallocate(qi_bl2d)
      deallocate(cldfra_bl2d)
   endif
   if (flag_qc)   deallocate(qc2d)
   if (flag_qi)   deallocate(qi2d)
   if (flag_qnc)  deallocate(qnc2d)
   if (flag_qni)  deallocate(qni2d)
   if (flag_qnwfa)deallocate(qnwfa2d)
   if (flag_qnifa)deallocate(qnifa2d)
   if (flag_qnbca)deallocate(qnbca2d)
   if (spp_pbl > 0) then
      deallocate(pattern_spp_pbl2d)
   endif

!print*,"In mynn wrapper, at end"

  CONTAINS

! ==================================================================
  SUBROUTINE moisture_check2(kte, delt, dp, exner, &
                             qv, qc, qi, th        )
  !
  ! If qc < qcmin, qi < qimin, or qv < qvmin happens in any layer,
  ! force them to be larger than minimum value by (1) condensating 
  ! water vapor into liquid or ice, and (2) by transporting water vapor 
  ! from the very lower layer.
  ! 
  ! We then update the final state variables and tendencies associated
  ! with this correction. If any condensation happens, update theta/temperature too.
  ! Note that (qv,qc,qi,th) are the final state variables after
  ! applying corresponding input tendencies and corrective tendencies.

    implicit none
    integer,  intent(in)     :: kte
    real, intent(in)     :: delt
    real, dimension(kte), intent(in)     :: dp
    real, dimension(kte), intent(in)     :: exner
    real, dimension(kte), intent(inout)  :: qv, qc, qi, th
    integer   k
    real ::  dqc2, dqi2, dqv2, sum, aa, dum
    real, parameter :: qvmin1= 1e-8,    & !min at k=1
                       qvmin = 1e-20,   & !min above k=1
                       qcmin = 0.0,     &
                       qimin = 0.0

    do k = kte, 1, -1  ! From the top to the surface
       dqc2 = max(0.0, qcmin-qc(k)) !qc deficit (>=0)
       dqi2 = max(0.0, qimin-qi(k)) !qi deficit (>=0)

       !update species
       qc(k)  = qc(k)  +  dqc2
       qi(k)  = qi(k)  +  dqi2
       qv(k)  = qv(k)  -  dqc2 - dqi2
       !for theta
       !th(k)  = th(k)  +  xlvcp/exner(k)*dqc2 + &
       !                   xlscp/exner(k)*dqi2
       !for temperature
       th(k)  = th(k)  +  xlvcp*dqc2 + &
                          xlscp*dqi2

       !then fix qv if lending qv made it negative
       if (k .eq. 1) then
          dqv2   = max(0.0, qvmin1-qv(k)) !qv deficit (>=0)
          qv(k)  = qv(k)  + dqv2
          qv(k)  = max(qv(k),qvmin1)
          dqv2   = 0.0
       else
          dqv2   = max(0.0, qvmin-qv(k)) !qv deficit (>=0)
          qv(k)  = qv(k)  + dqv2
          qv(k-1)= qv(k-1)  - dqv2*dp(k)/dp(k-1)
          qv(k)  = max(qv(k),qvmin)
       endif
       qc(k) = max(qc(k),qcmin)
       qi(k) = max(qi(k),qimin)
    end do

    ! Extra moisture used to satisfy 'qv(1)>=qvmin' is proportionally
    ! extracted from all the layers that has 'qv > 2*qvmin'. This fully
    ! preserves column moisture.
    if( dqv2 .gt. 1.e-20 ) then
        sum = 0.0
        do k = 1, kte
           if( qv(k) .gt. 2.0*qvmin ) sum = sum + qv(k)*dp(k)
        enddo
        aa = dqv2*dp(1)/max(1.e-20,sum)
        if( aa .lt. 0.5 ) then
            do k = 1, kte
               if( qv(k) .gt. 2.0*qvmin ) then
                   dum    = aa*qv(k)
                   qv(k)  = qv(k) - dum
               endif
            enddo
        else
        ! For testing purposes only (not yet found in any output):
        !    write(*,*) 'Full moisture conservation is impossible'
        endif
    endif

    return

  END SUBROUTINE moisture_check2

  END SUBROUTINE mynnedmf_wrapper_run

!###=================================================================

END MODULE module_bl_mynn_wrapper
