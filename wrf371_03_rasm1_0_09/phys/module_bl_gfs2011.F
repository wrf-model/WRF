!LWRF:MODEL_LAYER:PHYSICS
!
MODULE module_bl_gfs2011

CONTAINS

!-------------------------------------------------------------------          
   SUBROUTINE BL_GFS2011(U3D,V3D,TH3D,T3D,QV3D,QC3D,QI3D,P3D,PI3D, &
                  RUBLTEN,RVBLTEN,RTHBLTEN,                        &
                  RQVBLTEN,RQCBLTEN,RQIBLTEN,          	           & 
                  CP,G,ROVCP,R,ROVG,P_QI,P_FIRST_SCALAR,           &
                  dz8w,z,PSFC,                                     &
                  UST,PBL,PSIM,PSIH,                               &
                  HFX,QFX,TSK,GZ1OZ0,WSPD,BR,                      &
                  DT,KPBL2D,EP1,KARMAN,                            &
#if (NMM_CORE==1)
                  DISHEAT,                                         &
#endif
                  RTHRATEN,                                        &    !Kwon add RTHRATEN 
                  HPBL2D, EVAP2D, HEAT2D,                          &    !Kwon add FOR SHAL. CON.
                  ids,ide, jds,jde, kds,kde,                       &
                  ims,ime, jms,jme, kms,kme,                       &
                  its,ite, jts,jte, kts,kte                        )
!--------------------------------------------------------------------
      USE MODULE_GFS_MACHINE , ONLY : kind_phys
!-------------------------------------------------------------------
      IMPLICIT NONE
!-------------------------------------------------------------------
!-- U3D         3D u-velocity interpolated to theta points (m/s)
!-- V3D         3D v-velocity interpolated to theta points (m/s)
!-- TH3D	3D potential temperature (K)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- QC3D        3D cloud mixing ratio (Kg/Kg)
!-- QI3D        3D ice mixing ratio (Kg/Kg)
!-- P3D         3D pressure (Pa)
!-- PI3D	3D exner function (dimensionless)
!-- rr3D	3D dry air density (kg/m^3)
!-- RUBLTEN     U tendency due to
!               PBL parameterization (m/s^2)
!-- RVBLTEN     V tendency due to
!               PBL parameterization (m/s^2)
!-- RTHBLTEN    Theta tendency due to
!               PBL parameterization (K/s)
!-- RQVBLTEN    Qv tendency due to
!               PBL parameterization (kg/kg/s)
!-- RQCBLTEN    Qc tendency due to
!               PBL parameterization (kg/kg/s)
!-- RQIBLTEN    Qi tendency due to
!               PBL parameterization (kg/kg/s)
!-- CP          heat capacity at constant pressure for dry air (J/kg/K)
!-- G           acceleration due to gravity (m/s^2)
!-- ROVCP       R/CP
!-- R           gas constant for dry air (J/kg/K)
!-- ROVG 	R/G
!-- P_QI	species index for cloud ice
!-- dz8w	dz between full levels (m)
!-- z		height above sea level (m)
!-- PSFC        pressure at the surface (Pa)
!-- UST		u* in similarity theory (m/s)
!-- PBL		PBL height (m)
!-- PSIM        similarity stability function for momentum
!-- PSIH        similarity stability function for heat
!-- HFX		upward heat flux at the surface (W/m^2)
!-- QFX		upward moisture flux at the surface (kg/m^2/s)
!-- TSK		surface temperature (K)
!-- GZ1OZ0      log(z/z0) where z0 is roughness length
!-- WSPD        wind speed at lowest model level (m/s)
!-- BR          bulk Richardson number in surface layer
!-- DT		time step (s)
!-- rvovrd      R_v divided by R_d (dimensionless)
!-- EP1         constant for virtual temperature (R_v/R_d - 1) (dimensionless)
!-- KARMAN      Von Karman constant
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
!-------------------------------------------------------------------

      INTEGER, INTENT(IN) ::            ids,ide, jds,jde, kds,kde,      &
                                        ims,ime, jms,jme, kms,kme,      &
                                        its,ite, jts,jte, kts,kte,      &
                                        P_QI,P_FIRST_SCALAR

#if (NMM_CORE==1)
      LOGICAL , INTENT(IN)::            DISHEAT                                    !gopal's doing
#endif
      REAL,  DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) ::  RTHRATEN         !Kwon
      REAL,  DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::              &
                                        HPBL2D,                         &    !ADDED BY KWON FOR SHALLOW CONV.
                                        EVAP2D,                         &    !ADDED BY KWON FOR SHALLOW CONV.
                                        HEAT2D                               !ADDED BY KWON FOR SHALLOW CONV.

      REAL,    INTENT(IN) ::                                            &
                                        CP,                             &
                                        DT,                             &
                                        EP1,                            &
                                        G,                              &
                                        KARMAN,                         &
                                        R,                              & 
                                        ROVCP,                          &
                                        ROVG 

      REAL,    DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) ::      & 
                                        DZ8W,                           &
                                        P3D,                            &
                                        PI3D,                           &
                                        QC3D,                           &
                                        QI3D,                           &
                                        QV3D,                           &
                                        T3D,                            &
                                        TH3D,                           &
                                        U3D,                            &
                                        V3D,                            &
                                        Z   


      REAL,    DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::   &
                                        RTHBLTEN,                       &
                                        RQCBLTEN,                       &
                                        RQIBLTEN,                       &
                                        RQVBLTEN,                       &
                                        RUBLTEN,                        &
                                        RVBLTEN                        

      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(IN) ::               &
                                        BR,                             &
                                        GZ1OZ0,                         &
                                        HFX,                            &
                                        PSFC,                           &
                                        PSIM,                           &
                                        PSIH,                           &
                                        QFX,                            &
                                        TSK
 
      REAL,    DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::            & 
                                        PBL,                            &
                                        UST,                            &
                                        WSPD

      INTEGER, DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::              &
                                        KPBL2D                         


!--------------------------- LOCAL VARS ------------------------------


      REAL     (kind=kind_phys), DIMENSION(its:ite, kts:kte) ::         &
                                        DEL,                            &
                                        DU,                             &
                                        DV,                             &
                                        PHIL,                           &
                                        PRSL,                           &
                                        PRSLK,                          &
                                        T1,                             &
                                        TAU,                            &
                                        dishx,                          &
                                        THRATEN,                        & !Kwon
                                        U1,                             &
                                        V1

      REAL     (kind=kind_phys), DIMENSION(its:ite, kts:kte+1) ::       &
                                        PHII,                           & 
                                        PRSI

      REAL     (kind=kind_phys), DIMENSION(its:ite, kts:kte, 3) ::      &
                                        Q1,                             &
                                        RTG

      REAL     (kind=kind_phys), DIMENSION(its:ite) ::                  &
                                        DQSFC,                          &
                                        DTSFC,                          &
                                        DUSFC,                          &
                                        DVSFC,                          &
                                        EVAP,                           &
                                        FH,                             &
                                        FM,                             &
                                        HEAT,                           &
                                        HGAMQ,                          &
                                        HGAMT,                          &
                                        HPBL,                           &
                                        PSK,                            &
                                        QSS,                            &
                                        RBSOIL,                         &
                                        RCL,                            &
                                        SPD1,                           &
                                        STRESS,                         &
                                        TSEA

      REAL     (kind=kind_phys) ::                                      &
                                        CPM,                            &
                                        cpmikj,                         &
                                        DELTIM,                         &
                                        FMTMP,                          &
                                        RRHOX

      INTEGER, DIMENSION( its:ite ) ::                                  &
                                        KPBL 

      INTEGER ::                                                        &
                                        I,                              &
                                        IM,                             &
                                        J,                              &
                                        K,                              &
                                        KM,                             &
                                        KTEM,                           &
                                        KTEP,                           &
                                        KX,                             &
                                        L,                              & 
                                        NTRAC
 
   IM=ITE-ITS+1
   KX=KTE-KTS+1
   KTEM=KTE-1
   KTEP=KTE+1
   NTRAC=2
   DELTIM=DT
   IF (P_QI.ge.P_FIRST_SCALAR) NTRAC=3


   DO J=jts,jte

      DO i=its,ite
        RRHOX=(R*T3D(I,KTS,J)*(1.+EP1*QV3D(I,KTS,J)))/PSFC(I,J)
        CPM=CP*(1.+0.8*QV3D(i,kts,j))
        FMTMP=GZ1OZ0(i,j)-PSIM(i,j)
        PSK(i)=(PSFC(i,j)*.00001)**ROVCP
        FM(i)=FMTMP
        FH(i)=GZ1OZ0(i,j)-PSIH(i,j)
        TSEA(i)=TSK(i,j)
        QSS(i)=QV3D(i,kts,j)               ! not used in moninq so set to qv3d for now
        HEAT(i)=HFX(i,j)/CPM*RRHOX
        EVAP(i)=QFX(i,j)*RRHOX
! Kwon FOR NEW SHALLOW CONVECTION 
        HEAT2D(i,j)=HFX(i,j)/CPM*RRHOX
        EVAP2D(i,j)=QFX(i,j)*RRHOX
!
        STRESS(i)=KARMAN*KARMAN*WSPD(i,j)*WSPD(i,j)/(FMTMP*FMTMP)
        SPD1(i)=WSPD(i,j)
        PRSI(i,kts)=PSFC(i,j)*.001
        PHII(I,kts)=0.
        RCL(i)=1.
        RBSOIL(I)=BR(i,j)
      ENDDO

      DO k=kts,kte
        DO i=its,ite 
          DV(I,K) = 0.
          DU(I,K) = 0.
          TAU(I,K) = 0.
          U1(I,K) = U3D(i,k,j)
          V1(I,K) = V3D(i,k,j)
          T1(I,K) = T3D(i,k,j)
#ifdef NMM_CORE
          THRATEN(I,K) = RTHRATEN(I,K,J)
#else
          THRATEN(I,K) = 0.0
#endif
          Q1(I,K,1) = QV3D(i,k,j)/(1.+QV3D(i,k,j))
          Q1(I,K,2) = QC3D(i,k,j)/(1.+QC3D(i,k,j))
          PRSL(I,K)=P3D(i,k,j)*.001
        ENDDO
      ENDDO

      DO k=kts,kte
        DO i=its,ite 
          PRSLK(I,K)=(PRSL(i,k)*.01)**ROVCP
        ENDDO
      ENDDO


      DO k=kts+1,kte
        km=k-1
        DO i=its,ite 
          DEL(i,km)=PRSL(i,km)/ROVG*dz8w(i,km,j)/T3D(i,km,j)
          PRSI(i,k)=PRSI(i,km)-DEL(i,km)
          PHII(I,K)=(Z(i,k,j)-Z(i,kts,j))*G
          PHIL(I,KM)=0.5*(Z(i,k,j)+Z(i,km,j)-2.*Z(i,kts,j))*G
        ENDDO
      ENDDO

      DO i=its,ite 
        DEL(i,kte)=DEL(i,ktem)
        PRSI(i,ktep)=PRSI(i,kte)-DEL(i,ktem)
        PHII(I,KTEP)=PHII(I,KTE)+dz8w(i,kte,j)*G
        PHIL(I,KTE)=PHII(I,KTE)-PHIL(I,KTEM)+PHII(I,KTE)
      ENDDO

      IF (P_QI.ge.P_FIRST_SCALAR) THEN
        DO k=kts,kte
          DO i=its,ite 
            Q1(I,K,3) = QI3D(i,k,j)/(1.+QI3D(i,k,j))
          ENDDO
        ENDDO
      ENDIF

      DO l=1,ntrac
        DO k=kts,kte
          DO i=its,ite
            RTG(I,K,L) = 0.
          ENDDO
        ENDDO
      ENDDO
!
!  2010 new gfs pbl
!
      call moninq(im,im,km,ntrac,dv,du,tau,rtg,                       &
     &     u1,v1,t1,q1,thraten,                                       &  !kwon
     &     psk,rbsoil,fm,fh,tsea,qss,heat,evap,stress,spd1,kpbl,      &
     &     prsi,del,prsl,prslk,phii,phil,rcl,deltim,                  &
     &     dusfc,dvsfc,dtsfc,dqsfc,hpbl,hgamt,hgamq)

!============================================================================
!    ADD  IN  DISSIPATIVE HEATING .... v*dv. This is Bob's doing
!============================================================================

#if (NMM_CORE==1)

      IF(DISHEAT)THEN
       DO k=kts,kte
         DO i=its,ite
          dishx(i,k)=u1(i,k)*du(i,k) + v1(i,k)*dv(i,k)
          cpmikj=CP*(1.+0.8*QV3D(i,k,j))
          dishx(i,k)=-dishx(i,k)/cpmikj
!         IF(k==1)WRITE(0,*)'ADDITIONAL DISSIPATIVE HEATING',tau(i,k),dishx(i,k)
          tau(i,k)=tau(i,k)+dishx(i,k)
         ENDDO 
       ENDDO 
      ENDIF
#endif

!=============================================================================


      DO k=kts,kte
        DO i=its,ite
          RVBLTEN(I,K,J)=DV(I,K)
          RUBLTEN(I,K,J)=DU(I,K)
          RTHBLTEN(I,K,J)=TAU(I,K)/PI3D(I,K,J)
          RQVBLTEN(I,K,J)=RTG(I,K,1)/(1.-Q1(I,K,1))**2
          RQCBLTEN(I,K,J)=RTG(I,K,2)/(1.-Q1(I,K,2))**2
        ENDDO
      ENDDO

      IF (P_QI.ge.P_FIRST_SCALAR) THEN
        DO k=kts,kte
          DO i=its,ite
            RQIBLTEN(I,K,J)=RTG(I,K,3)/(1.-Q1(I,K,3))**2
          ENDDO
        ENDDO
      ENDIF

      DO i=its,ite
        UST(i,j)=SQRT(STRESS(i))
        WSPD(i,j)=SQRT(U3D(I,KTS,J)*U3D(I,KTS,J)+                       &
                       V3D(I,KTS,J)*V3D(I,KTS,J))+1.E-9
        PBL(i,j)=HPBL(i)
!Kwon For new shallow convection
        HPBL2D(i,j)=HPBL(i)
!
        KPBL2D(i,j)=kpbl(i)
      ENDDO

    ENDDO


   END SUBROUTINE BL_GFS2011

!===================================================================
   SUBROUTINE gfs2011init(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,       &
                      RQCBLTEN,RQIBLTEN,P_QI,P_FIRST_SCALAR,       &
                      restart,                                     &
                      allowed_to_read,                             &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------          
   IMPLICIT NONE
!-------------------------------------------------------------------          
   LOGICAL , INTENT(IN)          ::  allowed_to_read,restart
   INTEGER , INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)          ::  P_QI,P_FIRST_SCALAR

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::         &
                                                         RUBLTEN, &
                                                         RVBLTEN, &
                                                         RTHBLTEN, &
                                                         RQVBLTEN, &
                                                         RQCBLTEN, & 
                                                         RQIBLTEN
   INTEGER :: i, j, k, itf, jtf, ktf

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RUBLTEN(i,k,j)=0.
        RVBLTEN(i,k,j)=0.
        RTHBLTEN(i,k,j)=0.
        RQVBLTEN(i,k,j)=0.
        RQCBLTEN(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO
   ENDIF

   IF (P_QI .ge. P_FIRST_SCALAR .and. .not.restart) THEN
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         RQIBLTEN(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO
   ENDIF

   IF (P_QI .ge. P_FIRST_SCALAR) THEN
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         RQIBLTEN(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO
   ENDIF


   END SUBROUTINE gfs2011init

! --------------------------------------------------------------
!========================================================  2010 NEW GFS PBL
!FPP$ NOCONCUR R
!-----------------------------------------------------------------------
      subroutine moninq(ix,im,km,ntrac,dv,du,tau,rtg,              &
     &     uo,vo,t1,q1,thraten,                                    &     !kwon
     &     psk,rbsoil,fm,fh,tsea,qss,heat,evap,stress,spd1,kpbl,   &
     &     prsi,del,prsl,prslk,phii,phil,rcs,deltim,               &
     &     dusfc,dvsfc,dtsfc,dqsfc,hpbl,hgamt,hgamq)                     !kwon
!     &     dusfc,dvsfc,dtsfc,dqsfc,hpbl,hgamt,hgamq,dkt)
!
!      use machine     , only : kind_phys
!      use funcphys , only : fpvs
!      use physcons, grav => con_g, rd => con_rd, cp => con_cp      &
!     &,             hvap => con_hvap, fv => con_fvirt
!
      USE MODULE_GFS_MACHINE, ONLY : kind_phys                               !kwon
!      USE MODULE_GFS_FUNCPHYS, ONLY : fpvs                                  !kwon
      USE MODULE_GFS_PHYSCONS, grav => con_g, rd => con_rd,           &      !kwon
     &           cp => con_cp,  hvap => con_hvap, fv => con_fvirt            !kwon
!
      implicit none
!
!     include 'constant.h'
!
!
!     arguments
!
      integer ix, im, km, ntrac, kpbl(im), kpblx(im)
!
      real(kind=kind_phys) deltim
      real(kind=kind_phys) dv(im,km),     du(im,km),          &
     &                     tau(im,km),    rtg(im,km,ntrac),          &
     &                     uo(ix,km),     vo(ix,km),          &
     &                     t1(ix,km),     q1(ix,km,ntrac),          &
     &                     swh(ix,km),    hlw(ix,km),          &
     &                     xmu(im),          &
     &                     psk(im),       rbsoil(im),          &
!    &                     cd(im),        ch(im),          &
     &                     fm(im),        fh(im),          &
     &                     tsea(im),      qss(im),          &
     &                                    spd1(im),          &
!    &                     dphi(im),      spd1(im),          &
     &                     prsi(ix,km+1), del(ix,km),          &
     &                     prsl(ix,km),   prslk(ix,km),    &
     &                     phii(ix,km+1), phil(ix,km),          &
     &                     rcs(im),       dusfc(im),          &
     &                     dvsfc(im),     dtsfc(im),          &
     &                     dqsfc(im),     hpbl(im),      hpblx(im),          &
     &                     hgamt(im),     hgamq(im)
!    &,                    hgamu(im),     hgamv(im),     hgams(im)
!
!    locals
!
      integer i,iprt,is,iun,k,kk,km1,kmpbl,latd,lond
      integer lcld(im),icld(im),kcld(im),krad(im)
      integer kemx(im)
!
!     real(kind=kind_phys) betaq(im), betat(im),   betaw(im),
      real(kind=kind_phys) evap(im),  heat(im),    phih(im),    &
     &                     phim(im),  rbdn(im),    rbup(im),    &
     &                     stress(im),beta(im),    &
     &                     ustar(im), wscale(im),  thermal(im),    &
     &                     wstar3(im)
!
      real(kind=kind_phys) thvx(im,km), thlvx(im,km),thraten(im,km),   & !Kwon
     &                     qlx(im,km),  thetae(im,km),    &
     &                     qtx(im,km),  bf(im,km-1),    &
     &                     u1(im,km),   v1(im,km),    radx(im,km-1),    &
     &                     govrth(im),  hrad(im),     cteit(im),    &
!    &                     hradm(im),   radmin(im),   vrad(im),    &
     &                     radmin(im),  vrad(im),    &
     &                     zd(im),      zdd(im),      thlvx1(im)
!
      real(kind=kind_phys) rdzt(im,km-1),dktx(im,km-1),dkux(im,km-1),    &
     &                     zi(im,km+1),  zl(im,km),    xkzo(im,km-1),    &
     &                     dku(im,km-1), dkt(im,km-1),xkzmo(im,km-1),    &
     &                     cku(im,km-1), ckt(im,km-1),    &
     &                     al(im,km-1),  ad(im,km),    &
     &                     au(im,km-1),  a1(im,km),     &
     &                     a2(im,km*ntrac), theta(im,km)
!
!     real(kind=kind_phys) prinv(im), hpbl01(im), rent(im)
      real(kind=kind_phys) prinv(im), rent(im)
!
      logical  pblflg(im), sfcflg(im), scuflg(im), flg(im)
!
      real(kind=kind_phys) aphi16,  aphi5,  bvf2,   wfac,    &
     &                     cfac,    conq,   cont,   conw,    &
     &                     dk,      dkmax,  dkmin,    &
     &                     dq1,     dsdz2,  dsdzq,  dsdzt,    &
     &                     dsdzu,   dsdzv,  sfac,    &
     &                     dsig,    dt,     dthe1,  dtodsd,    &
     &                     dtodsu,  dw2,    dw2min, g,    &
     &                     gamcrq,  gamcrt, gocp,   gor, gravi,    &
     &                     hol,     hol1,   pfac,   prmax, prmin,    &
     &                     prnum,   qmin,   tdzmin, qtend, rbcr,    &
     &                     rbint,   rdt,    rdz,    qlmin,     &
!    &                     rbint,   rdt,    rdz,    rdzt1,    &
     &                     ri,      rimin,  rl2,    rlam,  rlamun,    &
     &                     rone,    rzero,  sfcfrac,sflux,    &
     &                     shr2,    spdk2,  sri,    &
     &                     tem,     ti,     ttend,  tvd,    &
     &                     tvu,     utend,  vk,     vk2,    &
     &                     vtend,   zfac,   vpert,  cpert,    &
     &                     rentf1,  rentf2, radfac,    &
     &                     zfmin,   zk,     tem1,   tem2,    &
     &                     xkzm,    xkzmu,  xkzminv,    &
     &                     ptem,    ptem1,  ptem2
!
      real(kind=kind_phys) zstblmax,h1,     h2,     qlcr,  actei,    &
     &                     cldtime, u01,    v01,    delu,  delv
!
      parameter(gravi=1.0/grav)
      parameter(g=grav)
      parameter(gor=g/rd,gocp=g/cp)
      parameter(cont=1000.*cp/g,conq=1000.*hvap/g,conw=1000./g)
      parameter(rlam=30.0,vk=0.4,vk2=vk*vk)
      parameter(prmin=0.25,prmax=4.)
      parameter(dw2min=0.0001,dkmin=0.0,dkmax=1000.,rimin=-100.)
      parameter(rbcr=0.25,wfac=7.0,cfac=6.5,pfac=2.0,sfcfrac=0.1)
      parameter(qmin=1.e-8,xkzm=1.0,zfmin=1.e-8,aphi5=5.,aphi16=16.)
      parameter(tdzmin=1.e-3,qlmin=1.e-12,cpert=0.25,sfac=5.4)
      parameter(h1=0.33333333,h2=0.66666667)
      parameter(cldtime=500.,xkzmu=3.0,xkzminv=0.3)
!     parameter(gamcrt=3.,gamcrq=2.e-3,rlamun=150.0)
      parameter(gamcrt=3.,gamcrq=0.,rlamun=150.0)
      parameter(rentf1=0.2,rentf2=1.0,radfac=0.85)
      parameter(iun=84)
!
!     parameter (zstblmax = 2500., qlcr=1.0e-5)
!     parameter (zstblmax = 2500., qlcr=3.0e-5)
!     parameter (zstblmax = 2500., qlcr=3.5e-5)
!     parameter (zstblmax = 2500., qlcr=1.0e-4)
      parameter (zstblmax = 2500., qlcr=3.5e-5)
!     parameter (actei = 0.23)
      parameter (actei = 0.7)
!
!-----------------------------------------------------------------------
!
 601  format(1x,' moninp lat lon step hour ',3i6,f6.1)
 602      format(1x,'    k','        z','        t','       th',    &
     &     '      tvh','        q','        u','        v',         &
     &     '       sp')
 603      format(1x,i5,8f9.1)
 604      format(1x,'  sfc',9x,f9.1,18x,f9.1)
 605      format(1x,'    k      zl    spd2   thekv   the1v'         &
     &         ,' thermal    rbup')
 606      format(1x,i5,6f8.2)
 607      format(1x,' kpbl    hpbl      fm      fh   hgamt',        &
     &         '   hgamq      ws   ustar      cd      ch')
 608      format(1x,i5,9f8.2)
 609      format(1x,' k pr dkt dku ',i5,3f8.2)
 610      format(1x,' k pr dkt dku ',i5,3f8.2,' l2 ri t2',          &
     &         ' sr2  ',2f8.2,2e10.2)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     compute preliminary variables
!
      if (ix .lt. im) stop
!
!     iprt = 0
!     if(iprt.eq.1) then
!ccc   latd = 0
!     lond = 0
!     else
!ccc   latd = 0
!     lond = 0
!     endif
!c
      dt    = 2. * deltim
      rdt   = 1. / dt
      km1   = km - 1
      kmpbl = km / 2
!
      do k=1,km
        do i=1,im
          zi(i,k) = phii(i,k) * gravi
          zl(i,k) = phil(i,k) * gravi
          u1(i,k) = uo(i,k) * rcs(i)
          v1(i,k) = vo(i,k) * rcs(i)
        enddo
      enddo
      do i=1,im
         zi(i,km+1) = phii(i,km+1) * gravi
      enddo
!c
      do k = 1,km1
        do i=1,im
          rdzt(i,k) = 1.0 / (zl(i,k+1) - zl(i,k))
        enddo
      enddo
!c
!c  vertical background diffusivity
!c
      do k = 1,km1
        do i=1,im
          tem1      = 1.0 - prsi(i,k+1) / prsi(i,1)
          tem1      = tem1 * tem1 * 10.0
          xkzo(i,k) = xkzm * min(real(1.0,kind=kind_phys), exp(-tem1))
        enddo
      enddo
!c
!c  vertical background diffusivity for momentum
!c
      do k = 1,km1
        do i=1,im
          ptem = prsi(i,k+1) / prsi(i,1)
          if(ptem.ge.0.2) then
            xkzmo(i,k) = xkzmu
            ptem1 = prsi(i,k+1)
          else
            tem1 = 1.0 - prsi(i,k+1) / ptem1
            tem1 = tem1 * tem1 * 5.0
            xkzmo(i,k) = xkzmu * min(real(1.0,kind=kind_phys), exp(-tem1))
          endif
        enddo
      enddo
!c
!c  diffusivity in the inversion layer is set to be xkzminv (m^2/s)
!c
      do k = 1,kmpbl
        do i=1,im
!         if(zi(i,k+1).gt.200..and.zi(i,k+1).lt.zstblmax) then
          if(zi(i,k+1).gt.250.) then
            tem1 = (t1(i,k+1)-t1(i,k)) * rdzt(i,k)
            if(tem1 .gt. 1.e-5) then
               xkzo(i,k) = min(xkzo(i,k),xkzminv)
            endif
          endif
        enddo
      enddo
!c
      do i = 1,im
         dusfc(i) = 0.
         dvsfc(i) = 0.
         dtsfc(i) = 0.
         dqsfc(i) = 0.
         hgamt(i) = 0.
         hgamq(i) = 0.
!        hgamu(i) = 0.
!        hgamv(i) = 0.
!        hgams(i) = 0.
         wscale(i)= 0.
         kpbl(i)  = 1
         kpblx(i) = 1
         hpbl(i)  = zi(i,1)
         hpblx(i) = zi(i,1)
         pblflg(i)= .true.
         sfcflg(i)= .true.
         if(rbsoil(i).gt.0.0) sfcflg(i) = .false.
         scuflg(i)= .true.
         if(scuflg(i)) then
           radmin(i)= 0.
           cteit(i) = 0.
           rent(i)  = rentf1
           hrad(i)  = zi(i,1)
!          hradm(i) = zi(i,1)
           krad(i)  = 1
           icld(i)  = 0
           lcld(i)  = km1
           kcld(i)  = km1
           zd(i)    = 0.
        endif
      enddo
!
      do k = 1,km
        do i = 1,im
          theta(i,k) = t1(i,k) * psk(i) / prslk(i,k)
          qlx(i,k)   = max(q1(i,k,ntrac),qlmin)
          qtx(i,k)   = max(q1(i,k,1),qmin)+qlx(i,k)
          ptem       = qlx(i,k)
          ptem1      = hvap*max(q1(i,k,1),qmin)/(cp*t1(i,k))
          thetae(i,k)= theta(i,k)*(1.+ptem1)
          thvx(i,k)  = theta(i,k)*(1.+fv*max(q1(i,k,1),qmin)-ptem)
          ptem2      = theta(i,k)-(hvap/cp)*ptem
          thlvx(i,k) = ptem2*(1.+fv*qtx(i,k))
        enddo
      enddo
      do k = 1,km1
        do i = 1,im
          dku(i,k)  = 0.
          dkt(i,k)  = 0.
          dktx(i,k) = 0.
          dkux(i,k) = 0.
          cku(i,k)  = 0.
          ckt(i,k)  = 0.
          tem       = zi(i,k+1)-zi(i,k)
!          radx(i,k) = tem*(swh(i,k)*xmu(i)+hlw(i,k))
          radx(i,k) = tem*thraten(i,k)                   !Kwon
        enddo
      enddo
!c
      do i=1,im
         flg(i)  = scuflg(i)
      enddo
      do k = 1, km1
        do i=1,im
          if(flg(i).and.zl(i,k).ge.zstblmax) then
             lcld(i)=k
             flg(i)=.false.
          endif
      enddo
      enddo
!c
!c  compute buoyancy flux
!c
      do k = 1, km1
      do i = 1, im
         bf(i,k) = (thvx(i,k+1)-thvx(i,k))*rdzt(i,k)
      enddo
      enddo
!c
      do i = 1,im
        govrth(i) = g/theta(i,1)
      enddo
!c
      do i=1,im
         beta(i)  = dt / (zi(i,2)-zi(i,1))
      enddo
!c
      do i=1,im
         ustar(i) = sqrt(stress(i))
         thermal(i) = thvx(i,1)
      enddo
!c
!c  compute the first guess pbl height
!c
      do i=1,im
         flg(i) = .false.
         rbup(i) = rbsoil(i)
      enddo
      do k = 2, kmpbl
      do i = 1, im
        if(.not.flg(i)) then
          rbdn(i) = rbup(i)
          spdk2   = max((u1(i,k)**2+v1(i,k)**2),real(1.0,kind=kind_phys))
          rbup(i) = (thvx(i,k)-thermal(i))*        &
     &              (g*zl(i,k)/thvx(i,1))/spdk2
          kpbl(i) = k
          flg(i)  = rbup(i).gt.rbcr
        endif
      enddo
      enddo
      do i = 1,im
         k = kpbl(i)
         if(rbdn(i).ge.rbcr) then
           rbint = 0.
         elseif(rbup(i).le.rbcr) then
           rbint = 1.
         else
           rbint = (rbcr-rbdn(i))/(rbup(i)-rbdn(i))
         endif
         hpbl(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
         if(hpbl(i).lt.zi(i,kpbl(i))) kpbl(i) = kpbl(i) - 1
         hpblx(i) = hpbl(i)
         kpblx(i) = kpbl(i)
      enddo
!c
!c     compute similarity parameters 
!c
      do i=1,im
         sflux = heat(i) + evap(i)*fv*theta(i,1)
         if(sfcflg(i).and.sflux.gt.0.) then
           hol = max(rbsoil(i)*fm(i)*fm(i)/fh(i),rimin)
           hol = min(hol,-zfmin)
!c
           hol1 = hol*hpbl(i)/zl(i,1)*sfcfrac
!          phim(i) = (1.-aphi16*hol1)**(-1./4.)
!          phih(i) = (1.-aphi16*hol1)**(-1./2.)
           tem     = 1.0 / (1. - aphi16*hol1)
           phih(i) = sqrt(tem)
           phim(i) = sqrt(phih(i))
           wstar3(i) = govrth(i)*sflux*hpbl(i)
           tem1      = ustar(i)**3.
           wscale(i) = (tem1+wfac*vk*wstar3(i)*sfcfrac)**h1
!          wscale(i) = ustar(i)/phim(i)
           wscale(i) = min(wscale(i),ustar(i)*aphi16)
           wscale(i) = max(wscale(i),ustar(i)/aphi5)
         else
           pblflg(i)=.false.
         endif
      enddo
!c
!c compute counter-gradient mixing term for heat and moisture
!c
      do i = 1,im
         if(pblflg(i)) then
           hgamt(i)  = min(cfac*heat(i)/wscale(i),gamcrt)
           hgamq(i)  = min(cfac*evap(i)/wscale(i),gamcrq)
           vpert     = hgamt(i) + hgamq(i)*fv*theta(i,1)
           vpert     = min(vpert,gamcrt)
           thermal(i)= thermal(i)+max(vpert,real(0.0,kind=kind_phys))
           hgamt(i)  = max(hgamt(i),real(0.0,kind=kind_phys))
           hgamq(i)  = max(hgamq(i),real(0.0,kind=kind_phys))
         endif
      enddo
!c
!c compute large-scale mixing term for momentum
!c
!     do i = 1,im
!       flg(i) = pblflg(i)
!       kemx(i)= 1
!       hpbl01(i)= sfcfrac*hpbl(i)
!     enddo
!     do k = 1, kmpbl
!     do i = 1, im
!       if(flg(i).and.zl(i,k).gt.hpbl01(i)) then
!         kemx(i) = k
!         flg(i)  = .false.
!       endif
!     enddo
!     enddo
!     do i = 1, im
!       if(pblflg(i)) then
!         kk = kpbl(i)
!         if(kemx(i).le.1) then
!           ptem  = u1(i,1)/zl(i,1)
!           ptem1 = v1(i,1)/zl(i,1)
!           u01   = ptem*hpbl01(i)
!           v01   = ptem1*hpbl01(i)
!         else
!           tem   = zl(i,kemx(i))-zl(i,kemx(i)-1)
!           ptem  = (u1(i,kemx(i))-u1(i,kemx(i)-1))/tem
!           ptem1 = (v1(i,kemx(i))-v1(i,kemx(i)-1))/tem
!           tem1  = hpbl01(i)-zl(i,kemx(i)-1)
!           u01   = u1(i,kemx(i)-1)+ptem*tem1
!           v01   = v1(i,kemx(i)-1)+ptem1*tem1
!         endif
!         if(kk.gt.kemx(i)) then
!           delu  = u1(i,kk)-u01
!           delv  = v1(i,kk)-v01
!           tem2  = sqrt(delu**2+delv**2)
!           tem2  = max(tem2,0.1)
!           ptem2 = -sfac*ustar(i)*ustar(i)*wstar3(i)
!    1                /(wscale(i)**4.)
!           hgamu(i) = ptem2*delu/tem2
!           hgamv(i) = ptem2*delv/tem2
!           tem  = sqrt(u1(i,kk)**2+v1(i,kk)**2)
!           tem1 = sqrt(u01**2+v01**2)
!           ptem = tem - tem1
!           if(ptem.gt.0.) then
!             hgams(i)=-sfac*vk*sfcfrac*wstar3(i)/(wscale(i)**3.)
!           else
!             hgams(i)=sfac*vk*sfcfrac*wstar3(i)/(wscale(i)**3.)
!           endif
!         else
!           hgams(i) = 0.
!         endif
!       endif
!     enddo
!c
!c  enhance the pbl height by considering the thermal excess
!c
      do i=1,im
         flg(i)  = .true.
         if(pblflg(i)) then
           flg(i)  = .false.
           rbup(i) = rbsoil(i)
         endif
      enddo
      do k = 2, kmpbl
      do i = 1, im
        if(.not.flg(i)) then
          rbdn(i) = rbup(i)
          spdk2   = max((u1(i,k)**2+v1(i,k)**2),real(1.0,kind=kind_phys))
          rbup(i) = (thvx(i,k)-thermal(i))*        &
     &              (g*zl(i,k)/thvx(i,1))/spdk2
          kpbl(i) = k
          flg(i)  = rbup(i).gt.rbcr
        endif
      enddo
      enddo
      do i = 1,im
        if(pblflg(i)) then
           k = kpbl(i)
           if(rbdn(i).ge.rbcr) then
              rbint = 0.
           elseif(rbup(i).le.rbcr) then
              rbint = 1.
           else
              rbint = (rbcr-rbdn(i))/(rbup(i)-rbdn(i))
           endif
           hpbl(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
           if(hpbl(i).lt.zi(i,kpbl(i))) kpbl(i) = kpbl(i) - 1
           if(kpbl(i).le.1) pblflg(i) = .false.
        endif
      enddo
!c
!c  look for stratocumulus
!c
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i).and.k.le.lcld(i)) then
          if(qlx(i,k).ge.qlcr) then
             kcld(i)=k
             flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i).and.kcld(i).eq.km1) scuflg(i)=.false.
      enddo
!c
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i).and.k.le.kcld(i)) then
          if(qlx(i,k).ge.qlcr) then
            if(radx(i,k).lt.radmin(i)) then
              radmin(i)=radx(i,k)
              krad(i)=k
            endif
          else
            flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i).and.krad(i).le.1) scuflg(i)=.false.
        if(scuflg(i).and.radmin(i).ge.0.) scuflg(i)=.false.
      enddo
!c
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,2,-1
      do i = 1, im
        if(flg(i).and.k.le.krad(i)) then
          if(qlx(i,k).ge.qlcr) then
            icld(i)=icld(i)+1
          else
            flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i).and.icld(i).lt.1) scuflg(i)=.false.
      enddo
!c
      do i = 1, im
        if(scuflg(i)) then
           hrad(i) = zi(i,krad(i)+1)
!          hradm(i)= zl(i,krad(i))
        endif
      enddo
!c
      do i = 1, im
        if(scuflg(i).and.hrad(i).lt.zi(i,2)) scuflg(i)=.false.
      enddo
!c
      do i = 1, im
        if(scuflg(i)) then
          k    = krad(i)
          tem  = zi(i,k+1)-zi(i,k)
          tem1 = cldtime*radmin(i)/tem
          thlvx1(i) = thlvx(i,k)+tem1
!         if(thlvx1(i).gt.thlvx(i,k-1)) scuflg(i)=.false.
        endif
      enddo
!c 
      do i = 1, im
         flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i).and.k.le.krad(i))then
          if(thlvx1(i).le.thlvx(i,k))then
             tem=zi(i,k+1)-zi(i,k)
             zd(i)=zd(i)+tem
          else
             flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i))then
          kk = max(1, krad(i)+1-icld(i))
          zdd(i) = hrad(i)-zi(i,kk)
        endif
      enddo
      do i = 1, im
        if(scuflg(i))then
          zd(i) = max(zd(i),zdd(i))
          zd(i) = min(zd(i),hrad(i))
          tem   = govrth(i)*zd(i)*(-radmin(i))
          vrad(i)= tem**h1
        endif
      enddo
!c
!c     compute inverse Prandtl number
!c
      do i = 1, im
        if(pblflg(i)) then
          tem = phih(i)/phim(i)+cfac*vk*sfcfrac
!         prinv(i) = (1.0-hgams(i))/tem
          prinv(i) = 1.0 / tem
          prinv(i) = min(prinv(i),prmax)
          prinv(i) = max(prinv(i),prmin)
        endif
      enddo
!c
!c     compute diffusion coefficients below pbl
!c
      do k = 1, kmpbl
      do i=1,im
         if(pblflg(i).and.k.lt.kpbl(i)) then
!           zfac = max((1.-(zi(i,k+1)-zl(i,1))/
!    1             (hpbl(i)-zl(i,1))), zfmin)
            zfac = max((1.-zi(i,k+1)/hpbl(i)), zfmin)
            tem = wscale(i)*vk*zi(i,k+1)*zfac**pfac
!           dku(i,k) = xkzo(i,k)+wscale(i)*vk*zi(i,k+1)
!    1                 *zfac**pfac
            dku(i,k) = xkzmo(i,k) + tem
            dkt(i,k) = xkzo(i,k) + tem * prinv(i)
            dku(i,k) = min(dku(i,k),dkmax)
!           dku(i,k) = max(dku(i,k),xkzmo(i,k))
            dkt(i,k) = min(dkt(i,k),dkmax)
!           dkt(i,k) = max(dkt(i,k),xkzo(i,k))
            dktx(i,k)= dkt(i,k)
            dkux(i,k)= dku(i,k)
         endif
      enddo
      enddo
!c
!c compute diffusion coefficients based on local scheme
!c
      do i = 1, im
        if(.not.pblflg(i)) then
          kpbl(i) = 1
        endif
      enddo
      do k = 1, km1
         do i=1,im
            if(k.ge.kpbl(i)) then
               rdz  = rdzt(i,k)
               ti   = 2./(t1(i,k)+t1(i,k+1))
               dw2  = (u1(i,k)-u1(i,k+1))**2        &
     &               +(v1(i,k)-v1(i,k+1))**2
               shr2 = max(dw2,dw2min)*rdz*rdz
               bvf2 = g*bf(i,k)*ti
               ri   = max(bvf2/shr2,rimin)
               zk   = vk*zi(i,k+1)
               if(ri.lt.0.) then ! unstable regime
                  rl2      = zk*rlamun/(rlamun+zk)
                  dk       = rl2*rl2*sqrt(shr2)
                  sri      = sqrt(-ri)
                  dku(i,k) = xkzmo(i,k) + dk*(1+8.*(-ri)/(1+1.746*sri))
                  dkt(i,k) = xkzo(i,k) + dk*(1+8.*(-ri)/(1+1.286*sri))
               else             ! stable regime
                  rl2      = zk*rlam/(rlam+zk)
!!                tem      = rlam * sqrt(0.01*prsi(i,k))
!!                rl2      = zk*tem/(tem+zk)
                  dk       = rl2*rl2*sqrt(shr2)
                  tem1     = dk/(1+5.*ri)**2
                  if(k.ge.kpblx(i)) then
                    prnum = 1.0 + 2.1*ri
                    prnum = min(prnum,prmax)
                  else
                    prnum = 1.0
                  endif
                  dkt(i,k) = xkzo(i,k) + tem1
                  dku(i,k) = xkzmo(i,k) + tem1 * prnum
               endif
!c
               dku(i,k) = min(dku(i,k),dkmax)
!              dku(i,k) = max(dku(i,k),xkzmo(i,k))
               dkt(i,k) = min(dkt(i,k),dkmax)
!              dkt(i,k) = max(dkt(i,k),xkzo(i,k))
!c
            endif
!c
         enddo
      enddo
!c
!c  compute diffusion coefficients for cloud-top driven diffusion
!c  if the condition for cloud-top instability is met,
!c    increase entrainment flux at cloud top
!c
      do i = 1, im
        if(scuflg(i)) then
           k = krad(i)
           tem = thetae(i,k) - thetae(i,k+1)
           tem1 = qtx(i,k) - qtx(i,k+1)
           if (tem.gt.0..and.tem1.gt.0.) then
             cteit(i)= cp*tem/(hvap*tem1)
             if(cteit(i).gt.actei) rent(i) = rentf2
           endif
        endif
      enddo
      do i = 1, im
        if(scuflg(i)) then
           k = krad(i)
           tem1  = max(bf(i,k),tdzmin)
           ckt(i,k) = -rent(i)*radmin(i)/tem1
           cku(i,k) = ckt(i,k)
        endif
      enddo
!c
      do k = 1, kmpbl
         do i=1,im
            if(scuflg(i).and.k.lt.krad(i)) then
               tem1=hrad(i)-zd(i)
               tem2=zi(i,k+1)-tem1
               if(tem2.gt.0.) then
                  ptem= tem2/zd(i)
                  if(ptem.ge.1.) ptem= 1.
                  ptem= tem2*ptem*sqrt(1.-ptem)
                  ckt(i,k) = radfac*vk*vrad(i)*ptem
                  cku(i,k) = 0.75*ckt(i,k)
                  ckt(i,k) = max(ckt(i,k),dkmin)
                  ckt(i,k) = min(ckt(i,k),dkmax)
                  cku(i,k) = max(cku(i,k),dkmin)
                  cku(i,k) = min(cku(i,k),dkmax)
               endif
            endif
         enddo
      enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      do k = 1, kmpbl
        do i=1,im
          if(scuflg(i)) then
             dkt(i,k) = dkt(i,k)+ckt(i,k)
             dku(i,k) = dku(i,k)+cku(i,k)
             dkt(i,k) = min(dkt(i,k),dkmax)
             dku(i,k) = min(dku(i,k),dkmax)
          endif
        enddo
      enddo
!c
!c     compute tridiagonal matrix elements for heat and moisture
!c
      do i=1,im
         ad(i,1) = 1.
         a1(i,1) = t1(i,1)   + beta(i) * heat(i)
         a2(i,1) = q1(i,1,1) + beta(i) * evap(i)
      enddo
      if(ntrac.ge.2) then
        do k = 2, ntrac
          is = (k-1) * km
          do i = 1, im
            a2(i,1+is) = q1(i,1,k)
          enddo
        enddo
      endif
!c
      do k = 1,km1
        do i = 1,im
          dtodsd = dt/del(i,k)
          dtodsu = dt/del(i,k+1)
          dsig   = prsl(i,k)-prsl(i,k+1)
!         rdz    = rdzt(i,k)*2./(t1(i,k)+t1(i,k+1))
          rdz    = rdzt(i,k)
          tem1   = dsig * dkt(i,k) * rdz
          if(pblflg(i).and.k.lt.kpbl(i)) then
!            dsdzt = dsig*dkt(i,k)*rdz*(gocp-hgamt(i)/hpbl(i))
!            dsdzq = dsig*dkt(i,k)*rdz*(-hgamq(i)/hpbl(i))
             ptem1 = dsig * dktx(i,k) * rdz
             tem   = 1.0 / hpbl(i)
             dsdzt = tem1 * gocp - ptem1*hgamt(i)*tem
             dsdzq = ptem1 * (-hgamq(i)*tem)
             a2(i,k)   = a2(i,k)+dtodsd*dsdzq
             a2(i,k+1) = q1(i,k+1,1)-dtodsu*dsdzq
          else
!            dsdzt = dsig*dkt(i,k)*rdz*(gocp)
             dsdzt = tem1 * gocp
             a2(i,k+1) = q1(i,k+1,1)
          endif
!         dsdz2 = dsig*dkt(i,k)*rdz*rdz
          dsdz2     = tem1 * rdz
          au(i,k)   = -dtodsd*dsdz2
          al(i,k)   = -dtodsu*dsdz2
          ad(i,k)   = ad(i,k)-au(i,k)
          ad(i,k+1) = 1.-al(i,k)
          a1(i,k)   = a1(i,k)+dtodsd*dsdzt
          a1(i,k+1) = t1(i,k+1)-dtodsu*dsdzt
        enddo
      enddo
      if(ntrac.ge.2) then
        do kk = 2, ntrac
          is = (kk-1) * km
          do k = 1, km1
            do i = 1, im
              a2(i,k+1+is) = q1(i,k+1,kk)
            enddo
          enddo
        enddo
      endif
!c
!c     solve tridiagonal problem for heat and moisture
!c
      call tridin(im,km,ntrac,al,ad,au,a1,a2,au,a1,a2)
!c
!c     recover tendencies of heat and moisture
!c
      do  k = 1,km
         do i = 1,im
            ttend      = (a1(i,k)-t1(i,k))*rdt
            qtend      = (a2(i,k)-q1(i,k,1))*rdt
            tau(i,k)   = tau(i,k)+ttend
            rtg(i,k,1) = rtg(i,k,1)+qtend
            dtsfc(i)   = dtsfc(i)+cont*del(i,k)*ttend
            dqsfc(i)   = dqsfc(i)+conq*del(i,k)*qtend
         enddo
      enddo
      if(ntrac.ge.2) then
        do kk = 2, ntrac
          is = (kk-1) * km
          do k = 1, km 
            do i = 1, im
              qtend = (a2(i,k+is)-q1(i,k,kk))*rdt
              rtg(i,k,kk) = rtg(i,k,kk)+qtend
            enddo
          enddo
        enddo
      endif
!c
!c     compute tridiagonal matrix elements for momentum
!c
      do i=1,im
         ad(i,1) = 1.0 + beta(i) * stress(i) / spd1(i)
         a1(i,1) = u1(i,1)
         a2(i,1) = v1(i,1)
      enddo
!c
      do k = 1,km1
        do i=1,im
          dtodsd = dt/del(i,k)
          dtodsu = dt/del(i,k+1)
          dsig   = prsl(i,k)-prsl(i,k+1)
          rdz    = rdzt(i,k)
          tem1   = dsig*dku(i,k)*rdz
!         if(pblflg(i).and.k.lt.kpbl(i))then
!           ptem1 = dsig*dkux(i,k)*rdz
!           dsdzu = ptem1*(-hgamu(i)/hpbl(i))
!           dsdzv = ptem1*(-hgamv(i)/hpbl(i))
!           a1(i,k)   = a1(i,k)+dtodsd*dsdzu
!           a1(i,k+1) = u1(i,k+1)-dtodsu*dsdzu
!           a2(i,k)   = a2(i,k)+dtodsd*dsdzv
!           a2(i,k+1) = v1(i,k+1)-dtodsu*dsdzv
!         else
            a1(i,k+1) = u1(i,k+1)
            a2(i,k+1) = v1(i,k+1)
!         endif
!         dsdz2     = dsig*dku(i,k)*rdz*rdz
          dsdz2     = tem1*rdz
          au(i,k)   = -dtodsd*dsdz2
          al(i,k)   = -dtodsu*dsdz2
          ad(i,k)   = ad(i,k)-au(i,k)
          ad(i,k+1) = 1.-al(i,k)
        enddo
      enddo
!c
!c     solve tridiagonal problem for momentum
!c
      call tridi2(im,km,al,ad,au,a1,a2,au,a1,a2)
!c
!c     recover tendencies of momentum
!c
      do k = 1,km
         do i = 1,im
            ptem = 1./rcs(i) 
            utend = (a1(i,k)-u1(i,k))*rdt
            vtend = (a2(i,k)-v1(i,k))*rdt
            du(i,k)  = du(i,k)+utend*ptem
            dv(i,k)  = dv(i,k)+vtend*ptem
            dusfc(i) = dusfc(i)+conw*del(i,k)*utend
            dvsfc(i) = dvsfc(i)+conw*del(i,k)*vtend
         enddo
      enddo
!c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c  pbl height for diagnostic purpose
!c
      do i = 1, im
         hpbl(i) = hpblx(i)
         kpbl(i) = kpblx(i)
      enddo
!c
!c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      return
      end subroutine moninq
!FPP$ NOCONCUR R
!-----------------------------------------------------------------------
      SUBROUTINE TRIDI2(L,N,CL,CM,CU,R1,R2,AU,A1,A2)
!sela %INCLUDE DBTRIDI2;
!
      USE MODULE_GFS_MACHINE, ONLY : kind_phys
      implicit none
      integer             k,n,l,i
      real(kind=kind_phys) fk
!
      real(kind=kind_phys) CL(L,2:N),CM(L,N),CU(L,N-1),R1(L,N),R2(L,N), &
     &          AU(L,N-1),A1(L,N),A2(L,N)
!-----------------------------------------------------------------------
      DO I=1,L
        FK      = 1./CM(I,1)
        AU(I,1) = FK*CU(I,1)
        A1(I,1) = FK*R1(I,1)
        A2(I,1) = FK*R2(I,1)
      ENDDO
      DO K=2,N-1
        DO I=1,L
          FK      = 1./(CM(I,K)-CL(I,K)*AU(I,K-1))
          AU(I,K) = FK*CU(I,K)
          A1(I,K) = FK*(R1(I,K)-CL(I,K)*A1(I,K-1))
          A2(I,K) = FK*(R2(I,K)-CL(I,K)*A2(I,K-1))
        ENDDO
      ENDDO
      DO I=1,L
        FK      = 1./(CM(I,N)-CL(I,N)*AU(I,N-1))
        A1(I,N) = FK*(R1(I,N)-CL(I,N)*A1(I,N-1))
        A2(I,N) = FK*(R2(I,N)-CL(I,N)*A2(I,N-1))
      ENDDO
      DO K=N-1,1,-1
        DO I=1,L
          A1(I,K) = A1(I,K)-AU(I,K)*A1(I,K+1)
          A2(I,K) = A2(I,K)-AU(I,K)*A2(I,K+1)
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE TRIDI2
!FPP$ NOCONCUR R
!-----------------------------------------------------------------------
      SUBROUTINE TRIDIN(L,N,nt,CL,CM,CU,R1,R2,AU,A1,A2)
!sela %INCLUDE DBTRIDI2;
!
      USE MODULE_GFS_MACHINE, ONLY : kind_phys
      implicit none
      integer             is,k,kk,n,nt,l,i
      real(kind=kind_phys) fk(L)
!
      real(kind=kind_phys) CL(L,2:N), CM(L,N), CU(L,N-1),               &
     &                     R1(L,N),   R2(L,N*nt),                       &
     &                     AU(L,N-1), A1(L,N), A2(L,N*nt),              &
     &                     FKK(L,2:N-1)
!-----------------------------------------------------------------------
      DO I=1,L
        FK(I)   = 1./CM(I,1)
        AU(I,1) = FK(I)*CU(I,1)
        A1(I,1) = FK(I)*R1(I,1)
      ENDDO
      do k = 1, nt
        is = (k-1) * n
        do i = 1, l
          a2(i,1+is) = fk(I) * r2(i,1+is)
        enddo
      enddo
      DO K=2,N-1
        DO I=1,L
          FKK(I,K) = 1./(CM(I,K)-CL(I,K)*AU(I,K-1))
          AU(I,K)  = FKK(I,K)*CU(I,K)
          A1(I,K)  = FKK(I,K)*(R1(I,K)-CL(I,K)*A1(I,K-1))
        ENDDO
      ENDDO
      do kk = 1, nt
        is = (kk-1) * n
        DO K=2,N-1
          DO I=1,L
            A2(I,K+is) = FKK(I,K)*(R2(I,K+is)-CL(I,K)*A2(I,K+is-1))
          ENDDO
        ENDDO
      ENDDO
      DO I=1,L
        FK(I)   = 1./(CM(I,N)-CL(I,N)*AU(I,N-1))
        A1(I,N) = FK(I)*(R1(I,N)-CL(I,N)*A1(I,N-1))
      ENDDO
      do k = 1, nt
        is = (k-1) * n
        do i = 1, l
          A2(I,N+is) = FK(I)*(R2(I,N+is)-CL(I,N)*A2(I,N+is-1))
        enddo
      enddo
      DO K=N-1,1,-1
        DO I=1,L
          A1(I,K) = A1(I,K) - AU(I,K)*A1(I,K+1)
        ENDDO
      ENDDO
      do kk = 1, nt
        is = (kk-1) * n
        DO K=n-1,1,-1
          DO I=1,L
            A2(I,K+is) = A2(I,K+is) - AU(I,K)*A2(I,K+is+1)
          ENDDO
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE TRIDIN
      SUBROUTINE TRIDIT(L,N,nt,CL,CM,CU,RT,AU,AT)
!sela %INCLUDE DBTRIDI2;
!
      USE MODULE_GFS_MACHINE, ONLY : kind_phys
      implicit none
      integer             is,k,kk,n,nt,l,i
      real(kind=kind_phys) fk(L)
!
      real(kind=kind_phys) CL(L,2:N), CM(L,N), CU(L,N-1),               &
     &                     RT(L,N*nt),                                  &
     &                     AU(L,N-1), AT(L,N*nt),                       &
     &                     FKK(L,2:N-1)                  
!-----------------------------------------------------------------------
      DO I=1,L
        FK(I)   = 1./CM(I,1)
        AU(I,1) = FK(I)*CU(I,1)
      ENDDO
      do k = 1, nt
        is = (k-1) * n
        do i = 1, l
          at(i,1+is) = fk(I) * rt(i,1+is)
        enddo
      enddo
      DO K=2,N-1
        DO I=1,L
          FKK(I,K) = 1./(CM(I,K)-CL(I,K)*AU(I,K-1))
          AU(I,K)  = FKK(I,K)*CU(I,K)
        ENDDO
      ENDDO
      do kk = 1, nt
        is = (kk-1) * n
        DO K=2,N-1
          DO I=1,L
            AT(I,K+is) = FKK(I,K)*(RT(I,K+is)-CL(I,K)*AT(I,K+is-1))
          ENDDO
        ENDDO
      ENDDO
      DO I=1,L
        FK(I)   = 1./(CM(I,N)-CL(I,N)*AU(I,N-1))
      ENDDO
      do k = 1, nt
        is = (k-1) * n
        do i = 1, l
          AT(I,N+is) = FK(I)*(RT(I,N+is)-CL(I,N)*AT(I,N+is-1))
        enddo
      enddo
      do kk = 1, nt
        is = (kk-1) * n
        DO K=n-1,1,-1
          DO I=1,L
            AT(I,K+is) = AT(I,K+is) - AU(I,K)*AT(I,K+is+1)
          ENDDO
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE TRIDIT
                                                                                 
!-----------------------------------------------------------------------

      END MODULE module_bl_gfs2011
