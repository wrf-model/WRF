!LWRF:MODEL_LAYER:PHYSICS
!
MODULE module_bl_gfsedmf

#if (HWRF==1)

CONTAINS

!-------------------------------------------------------------------          
   SUBROUTINE BL_GFSEDMF(U3D,V3D,TH3D,T3D,QV3D,QC3D,QI3D,P3D,PI3D, &
                  RUBLTEN,RVBLTEN,RTHBLTEN,                        &
                  RQVBLTEN,RQCBLTEN,RQIBLTEN,          	           & 
                  CP,G,ROVCP,R,ROVG,P_QI,P_FIRST_SCALAR,           &
                  dz8w,z,PSFC,                                     &
                  UST,PBL,PSIM,PSIH,                               &
                  HFX,QFX,TSK,GZ1OZ0,WSPD,BR,                      &
                  DT,KPBL2D,EP1,KARMAN,                            &
                  DISHEAT,                                         &
                  RTHRATEN,                                        &    !Kwon add RTHRATEN 
                  HPBL2D, EVAP2D, HEAT2D,                          &    !Kwon add FOR SHAL. CON.

                  U10,V10,ZNT,                                    &
                  DKU3D,DKT3D,                                    & 
                  VAR_RIC,coef_ric_l,coef_ric_s,alpha,xland,        &
#if (HWRF==1)
                  pert_pbl, ens_random_seed, ens_pblamp,           &
#endif
                  ids,ide, jds,jde, kds,kde,                       &
                  ims,ime, jms,jme, kms,kme,                       &
                  its,ite, jts,jte, kts,kte                        )
!--------------------------------------------------------------------
      USE MODULE_GFS_MACHINE , ONLY : kind_phys
!      USE MODULE_GFS_FUNCPHYS, only : fpvs
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


#if (HWRF==1)
      integer,intent(in) :: ens_random_seed
      real,intent(in) :: ens_pblamp
      logical,intent(in) :: pert_pbl
#endif
!wang
       REAL,  DIMENSION(ims:ime, jms:jme), INTENT(IN) ::              &
                                         U10,                         &
                                         V10,                   &
                                         ZNT, xland

       REAL,  DIMENSION(ims:ime, jms:jme, kms:kme), INTENT(OUT) :: DKU3D,DKT3D
!wang

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
      REAL     (kind=kind_phys), DIMENSION(its:ite, kts:kte-1) ::DKU,DKT

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
                                        TSEA,      &
                                        zorl,u10m,v10m,zol, xland1

      REAL     (kind=kind_phys) ::                                      &
                                        CPM,                            &
                                        cpmikj,                         &
                                        DELTIM,                         &
                                        FMTMP,                          &
                                        RRHOX

      INTEGER, DIMENSION( its:ite ) ::                                  &
                                        KPBL , kinver

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
                                        NTRAC, ntcw

      real(kind=kind_phys)xkzm_m, xkzm_h, xkzm_s   !! wang, background diff 
      real :: VAR_RIC,coef_ric_l,coef_ric_s,alpha
      logical lprnt
      integer ipr

 
   IM=ITE-ITS+1
   KX=KTE-KTS+1
   KTEM=KTE-1
   KTEP=KTE+1
   NTRAC=2
   DELTIM=DT

   xkzm_m=1.0
   xkzm_h=1.0
   xkzm_s=1.0   
   lprnt=.false.
   ipr=1
   ntcw=2

!    write(0,*)'in gfsedmf PBL'

   IF (P_QI.ge.P_FIRST_SCALAR) NTRAC=3

 !! note 2015,08-19, if we consider rain water, then ntrac=4, and set q1(i,k,4)=QR
 !! here, we do not consider rain water

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
        zorl(i)=znt(i,j) * 100.0  ! m to cm
        u10m(i)=u10(i,j)
        v10m(i)=v10(i,j)
        xland1(i)=xland(i,j)
        kinver(i)=kx
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
          THRATEN(I,K) = RTHRATEN(I,K,J)  !! * 0.0  !!! test , removing additional diffusion
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
!      call moninq(im,im,km,ntrac,dv,du,tau,rtg,                       &
!     &     u1,v1,t1,q1,thraten,                                       &  !kwon
!     &     psk,rbsoil,fm,fh,tsea,qss,heat,evap,stress,spd1,kpbl,      &
!     &     prsi,del,prsl,prslk,phii,phil,rcl,deltim,                  &
!     &     dusfc,dvsfc,dtsfc,dqsfc,hpbl,hgamt,hgamq)


      call moninedmf(im,im,kx,ntrac,ntcw,dv,du,tau,rtg,        &
!     &   u1,v1,t1,q1,swh,hlw,xmu,                             &
     &   u1,v1,t1,q1,thraten,                                  &
     &   psk,rbsoil,zorl,u10m,v10m,fm,fh,                      &
     &   tsea,qss,heat,evap,stress,spd1,kpbl,                  &
     &   prsi,del,prsl,prslk,phii,phil,deltim,disheat,         &  
     &   dusfc,dvsfc,dtsfc,dqsfc,hpbl,hgamt,hgamq,dkt,dku,     &
     &   kinver,xkzm_m,xkzm_h,xkzm_s,lprnt,ipr,zol,            &
#if (HWRF==1)
         pert_pbl, ens_random_seed, ens_pblamp,             &
#endif
     &   VAR_RIC,coef_ric_l,coef_ric_s,alpha,xland1)


!============================================================================
!    ADD  IN  DISSIPATIVE HEATING .... v*dv. This is Bob's doing
!============================================================================

!#if (NMM_CORE==1)
!!! already considered in edmfpbl
!!      IF(DISHEAT)THEN
!!       DO k=kts,kte
!!         DO i=its,ite
!!          dishx(i,k)=u1(i,k)*du(i,k) + v1(i,k)*dv(i,k)
!!          cpmikj=CP*(1.+0.8*QV3D(i,k,j))
!!          dishx(i,k)=-dishx(i,k)/cpmikj
!         IF(k==1)WRITE(0,*)'ADDITIONAL DISSIPATIVE HEATING',tau(i,k),dishx(i,k)
!!          tau(i,k)=tau(i,k)+dishx(i,k)
!!         ENDDO 
!!       ENDDO 
!!      ENDIF
!#endif

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

#if (HWRF==1)
     DO i=its,ite
     DO k=kts,kte
      DKU3D(I,J,K) = 0.
      DKT3D(I,J,K) = 0.
     ENDDO
     ENDDO

     DO i=its,ite
     DO k=kts,kte-1
      DKU3D(I,J,K) = DKU(I,K)
      DKT3D(I,J,K) = DKT(I,K)
     ENDDO

     ENDDO
#endif


    ENDDO


   END SUBROUTINE BL_GFSEDMF

!===================================================================
   SUBROUTINE gfsedmfinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,       &
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


   END SUBROUTINE gfsedmfinit

!----------------------------------------------------------------------

!!!!!  ==========================================================  !!!!!
! subroutine 'moninedmf' computes subgrid vertical mixing by turbulence
! 
! for the convective boundary layer, the scheme adopts eddy-diffusion
!  mass-flux (edmf) parameterization (siebesma et al., 2007) to take into 
!  account nonlocal transport by large eddies. to reduce the tropical wind rmse, 
!  a hybrid scheme is used, in which the edmf scheme is used only for strongly 
!  unstable pbl while the current operational vertical diffusion scheme is called 
!  for the weakly unstable pbl.  
!
      subroutine moninedmf(ix,im,km,ntrac,ntcw,dv,du,tau,rtg,           &
!     &   u1,v1,t1,q1,swh,hlw,xmu,                                      &
     &   u1,v1,t1,q1,thraten,                                           &
     &   psk,rbsoil,zorl,u10m,v10m,fm,fh,                               &
     &   tsea,qss,heat,evap,stress,spd1,kpbl,                           &
     &   prsi,del,prsl,prslk,phii,phil,delt,dspheat,                    &
     &   dusfc,dvsfc,dtsfc,dqsfc,hpbl,hgamt,hgamq,dkt,dku,              &
     &   kinver,xkzm_m,xkzm_h,xkzm_s,lprnt,ipr,zol,                     &
#if (HWRF==1)
           pert_pbl, ens_random_seed, ens_pblamp,             &
#endif
     &   VAR_RIC,coef_ric_l,coef_ric_s,alpha,xland1)
!
      USE MODULE_GFS_MACHINE, only : kind_phys
!      USE MODULE_GFS_FUNCPHYS, only : fpvs
      USE MODULE_GFS_PHYSCONS, grav => con_g, rd => con_rd, cp => con_cp &
     &,             hvap => con_hvap, fv => con_fvirt
      implicit none
!
!     arguments
!
      logical lprnt
      integer ipr
      integer ix, im, km, ntrac, ntcw, kpbl(im), kinver(im)
!
#if (HWRF==1)
      integer,intent(in) :: ens_random_seed
      real,intent(in) :: ens_pblamp
      logical,intent(in) :: pert_pbl
#endif
      real(kind=kind_phys) delt, xkzm_m, xkzm_h, xkzm_s
      real(kind=kind_phys) dv(im,km),     du(im,km),                 &
     &                     tau(im,km),    rtg(im,km,ntrac),          &
     &                     u1(ix,km),     v1(ix,km),                 &
     &                     t1(ix,km),     q1(ix,km,ntrac),           &
     &                     swh(ix,km),    hlw(ix,km),                &
     &                     xmu(im),       psk(im),                   &
     &                     rbsoil(im),    zorl(im),                  &
     &                     u10m(im),      v10m(im),                  &
     &                     fm(im),        fh(im),                    & 
     &                     tsea(im),      qss(im),                   &
     &                                    spd1(im),                  &
     &                     prsi(ix,km+1), del(ix,km),                &
     &                     prsl(ix,km),   prslk(ix,km),              &
     &                     phii(ix,km+1), phil(ix,km),               &
     &                     dusfc(im),     dvsfc(im),                 &
     &                     dtsfc(im),     dqsfc(im),                 &
     &                     hpbl(im),      hpblx(im),                 &
     &                     hgamt(im),     hgamq(im)
!
      logical dspheat
!          flag for tke dissipative heating
!
!    locals
!
      integer i,iprt,is,iun,k,kk,km1,kmpbl,latd,lond
      integer lcld(im),icld(im),kcld(im),krad(im)
      integer kx1(im), kpblx(im)
!
!     real(kind=kind_phys) betaq(im), betat(im),   betaw(im),
      real(kind=kind_phys) evap(im),  heat(im),    phih(im),           &
     &                     phim(im),  rbdn(im),    rbup(im),           &
     &                     stress(im),beta(im),    sflux(im),          &
     &                     z0(im),    crb(im),     wstar(im),          &
     &                     zol(im),   ustmin(im),  ustar(im),          &
     &                     thermal(im),wscale(im), wscaleu(im)
!
      real(kind=kind_phys) theta(im,km),thvx(im,km),  thlvx(im,km),    &
     &                     thraten(im,km),                             & ! wang
     &                     qlx(im,km),  thetae(im,km),                 &
     &                     qtx(im,km),  bf(im,km-1),  diss(im,km),     &
     &                     radx(im,km-1),                              &
     &                     govrth(im),  hrad(im),                      &
!    &                     hradm(im),   radmin(im),   vrad(im),        &
     &                     radmin(im),  vrad(im),                      &
     &                     zd(im),      zdd(im),      thlvx1(im)       
!
      real(kind=kind_phys) rdzt(im,km-1),dktx(im,km-1),                &
     &                     zi(im,km+1),  zl(im,km),    xkzo(im,km-1),  &
     &                     dku(im,km-1), dkt(im,km-1), xkzmo(im,km-1), &
     &                     cku(im,km-1), ckt(im,km-1),                 &
     &                     ti(im,km-1),  shr2(im,km-1),                &
     &                     al(im,km-1),  ad(im,km),                    &  
     &                     au(im,km-1),  a1(im,km),                    &
     &                     a2(im,km*ntrac)


!
      real(kind=kind_phys) tcko(im,km),  qcko(im,km,ntrac),            &
     &                     ucko(im,km),  vcko(im,km),  xmf(im,km)
!
      real(kind=kind_phys) prinv(im), rent(im)
!
      logical  pblflg(im), sfcflg(im), scuflg(im), flg(im)
      logical  ublflg(im), pcnvflg(im)
!
!  pcnvflg: true for convective(strongly unstable) pbl
!  ublflg: true for unstable but not convective(strongly unstable) pbl
!
      real(kind=kind_phys) aphi16,  aphi5,  bvf2,   wfac,            &
     &                     cfac,    conq,   cont,   conw,            &
     &                     dk,      dkmax,  dkmin,                   &
     &                     dq1,     dsdz2,  dsdzq,  dsdzt,           &
     &                     dsdzu,   dsdzv,                           &
     &                     dsig,    dt2,    dthe1,  dtodsd,          &
     &                     dtodsu,  dw2,    dw2min, g,               &
     &                     gamcrq,  gamcrt, gocp,                    &
     &                     gravi,   f0,                              &
     &                     prnum,   prmax,  prmin,  pfac,  crbcon,   &
     &                     qmin,    tdzmin, qtend,  crbmin,crbmax,   &
     &                     rbint,   rdt,    rdz,    qlmin,           &
     &                     ri,      rimin,  rl2,    rlam,  rlamun,   &
     &                     rone,    rzero,  sfcfrac,                 &
     &                     spdk2,   sri,    zol1,   zolcr, zolcru,   &
     &                     robn,    ttend,                           &
     &                     utend,   vk,     vk2,                     &
     &                     ust3,    wst3,                            &    
     &                     vtend,   zfac,   vpert,  cteit,           &
     &                     rentf1,  rentf2, radfac,                  & 
     &                     zfmin,   zk,     tem,    tem1,  tem2,     &
     &                     xkzm,    xkzmu,  xkzminv,                 &
     &                     ptem,    ptem1,  ptem2, tx1(im), tx2(im)  
!
      real(kind=kind_phys) zstblmax,h1,     h2,     qlcr,  actei,   &
     &                     cldtime

!! for aplha
     real(kind=kind_phys) WSPM(IM,KM-1), xland1(IM)
     integer kLOC ! RGF
     real :: xDKU, ALPHA    ! RGF
     real :: VAR_RIC,coef_ric_l,coef_ric_s
     
     logical:: outp
     integer :: stype, useshape
     real :: smax,ashape,sz2h, sksfc,skmax,ashape1,skminusk0, hmax

!!

!cc
      parameter(gravi=1.0/grav)
      parameter(g=grav)
      parameter(gocp=g/cp)
      parameter(cont=cp/g,conq=hvap/g,conw=1.0/g)               ! for del in pa
!     parameter(cont=1000.*cp/g,conq=1000.*hvap/g,conw=1000./g) ! for del in kpa
      parameter(rlam=30.0,vk=0.4,vk2=vk*vk)
      parameter(prmin=0.25,prmax=4.,zolcr=0.2,zolcru=-0.5)
      parameter(dw2min=0.0001,dkmin=0.0,dkmax=1000.,rimin=-100.)
      parameter(crbcon=0.25,crbmin=0.15,crbmax=0.35)
      parameter(wfac=7.0,cfac=6.5,pfac=2.0,sfcfrac=0.1)
!     parameter(qmin=1.e-8,xkzm=1.0,zfmin=1.e-8,aphi5=5.,aphi16=16.)
      parameter(qmin=1.e-8,         zfmin=1.e-8,aphi5=5.,aphi16=16.)
      parameter(tdzmin=1.e-3,qlmin=1.e-12,f0=1.e-4)
      parameter(h1=0.33333333,h2=0.66666667)
      parameter(cldtime=500.,xkzminv=0.3)
!     parameter(cldtime=500.,xkzmu=3.0,xkzminv=0.3)
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

#if HWRF==1
      real*8 :: ran1           !zhang
      real :: rr
      logical,save :: pert_pbl_local !zhang
      integer,save :: ens_random_seed_local,env_pp_local!zhang
      integer :: ensda_physics_pert !zhang
      real,save :: ens_pblamp_local   !zhang
      CHARACTER(len=3) :: env_memb,env_pp
      data ens_random_seed_local/0/
      data env_pp_local/0/
      if ( ens_random_seed_local .eq. 0 ) then
         CALL nl_get_ensda_physics_pert(1,ensda_physics_pert)
         ens_random_seed_local=ens_random_seed
         env_pp_local=ensda_physics_pert
         pert_pbl_local=.false.
         ens_pblamp_local=0.0
! env_pp=1: do physics perturbations for ensda members, ens_random_seed must be 99
         if ( env_pp_local .eq. 1 ) then
            if ( ens_random_seed .ne. 99 ) then
               pert_pbl_local=.true.
               ens_pblamp_local=ens_pblamp
            else
! ens_random_seed=99 do physics perturbation for ensemble forecasts, env_pp must be zero
               ens_random_seed_local=ens_random_seed
               pert_pbl_local=pert_pbl
               ens_pblamp_local=ens_pblamp
            endif
         else
            ens_random_seed_local=ens_random_seed
            pert_pbl_local=pert_pbl
            ens_pblamp_local=ens_pblamp
         endif
       print*,"PBL==",ens_random_seed_local,pert_pbl_local,ens_pblamp_local,ensda_physics_pert
!      print*, "zhang in pbl= one time ", pert_pbl_local,ens_random_seed_local, ens_pblamp_local
      endif
!      print*, "zhang in pbl=",pert_pbl_local, ens_random_seed_local, ens_pblamp_local
#endif

! Weiguo Wang added, height-dependent ALPHA
!       smax=0.148
!       stype=1
      useshape=2 !0-- no change, origincal ALPHA adjustment,1-- shape1, 2-- shape2(adjust above sfc)
  !     useshape=1 
!c
!c-----------------------------------------------------------------------
!c
!c-----------------------------------------------------------------------
!c
 601  format(1x,' moninp lat lon step hour ',3i6,f6.1)
 602      format(1x,'    k','        z','        t','       th',   &
     &     '      tvh','        q','        u','        v',        &
     &     '       sp')
 603      format(1x,i5,8f9.1)
 604      format(1x,'  sfc',9x,f9.1,18x,f9.1)
 605      format(1x,'    k      zl    spd2   thekv   the1v'        &
     &         ,' thermal    rbup')
 606      format(1x,i5,6f8.2)
 607      format(1x,' kpbl    hpbl      fm      fh   hgamt',       &
     &         '   hgamq      ws   ustar      cd      ch')
 608      format(1x,i5,9f8.2)
 609      format(1x,' k pr dkt dku ',i5,3f8.2)
 610      format(1x,' k pr dkt dku ',i5,3f8.2,' l2 ri t2',         &
     &         ' sr2  ',2f8.2,2e10.2)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     compute preliminary variables
!
      if (ix .lt. im) stop
!
!     iprt = 0
!     if(iprt.eq.1) then
!cc   latd = 0
!     lond = 0
!     else
!cc   latd = 0
!     lond = 0
!     endif
!
      dt2   = delt
      rdt   = 1. / dt2
      km1   = km - 1
      kmpbl = km / 2
!
      do k=1,km
        do i=1,im
          zi(i,k) = phii(i,k) * gravi
          zl(i,k) = phil(i,k) * gravi
        enddo
      enddo
      do i=1,im
         zi(i,km+1) = phii(i,km+1) * gravi
      enddo
!
      do k = 1,km1
        do i=1,im
          rdzt(i,k) = 1.0 / (zl(i,k+1) - zl(i,k))
        enddo
      enddo
!
      do i=1,im
        kx1(i) = 1
        tx1(i) = 1.0 / prsi(i,1)
        tx2(i) = tx1(i)
      enddo
      do k = 1,km1
        do i=1,im
          xkzo(i,k)  = 0.0
          xkzmo(i,k) = 0.0
          if (k < kinver(i)) then
!                                  vertical background diffusivity
            ptem      = prsi(i,k+1) * tx1(i)
            tem1      = 1.0 - ptem
            tem1      = tem1 * tem1 * 10.0
            xkzo(i,k) = xkzm_h * min(1.0, exp(-tem1))

!                                  vertical background diffusivity for momentum
            if (ptem >= xkzm_s) then
              xkzmo(i,k) = xkzm_m
              kx1(i)     = k + 1
            else
              if (k == kx1(i) .and. k > 1) tx2(i) = 1.0 / prsi(i,k)
              tem1 = 1.0 - prsi(i,k+1) * tx2(i)
              tem1 = tem1 * tem1 * 5.0
              xkzmo(i,k) = xkzm_m * min(1.0, exp(-tem1))
            endif
          endif
        enddo
      enddo
!     if (lprnt) then
!       print *,' xkzo=',(xkzo(ipr,k),k=1,km1)
!       print *,' xkzmo=',(xkzmo(ipr,k),k=1,km1)
!     endif
!
!  diffusivity in the inversion layer is set to be xkzminv (m^2/s)
!
      do k = 1,kmpbl
        do i=1,im
!         if(zi(i,k+1) > 200..and.zi(i,k+1) < zstblmax) then
          if(zi(i,k+1) > 250.) then
            tem1 = (t1(i,k+1)-t1(i,k)) * rdzt(i,k)
            if(tem1 > 1.e-5) then
               xkzo(i,k)  = min(xkzo(i,k),xkzminv)
            endif
          endif
        enddo
      enddo
!
      do i = 1,im
         z0(i)    = 0.01 * zorl(i)
         dusfc(i) = 0.
         dvsfc(i) = 0.
         dtsfc(i) = 0.
         dqsfc(i) = 0.
         wscale(i)= 0.
         wscaleu(i)= 0.
         kpbl(i)  = 1
         hpbl(i)  = zi(i,1)
         hpblx(i) = zi(i,1)
         pblflg(i)= .true.
         sfcflg(i)= .true.
         if(rbsoil(i) > 0.) sfcflg(i) = .false.
         ublflg(i)= .false.
         pcnvflg(i)= .false.
         scuflg(i)= .true.
         if(scuflg(i)) then
           radmin(i)= 0.
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
          qlx(i,k)   = max(q1(i,k,ntcw),qlmin)
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
          cku(i,k)  = 0.
          ckt(i,k)  = 0.
          tem       = zi(i,k+1)-zi(i,k)
!          radx(i,k) = tem*(swh(i,k)*xmu(i)+hlw(i,k))
          radx(i,k) = tem*thraten(i,k)
        enddo
      enddo
!
      do i=1,im
         flg(i)  = scuflg(i)
      enddo
      do k = 1, km1
        do i=1,im
          if(flg(i).and.zl(i,k) >= zstblmax) then
             lcld(i)=k
             flg(i)=.false.
          endif
      enddo
      enddo
!
!  compute virtual potential temp gradient (bf) and winshear square
!
      do k = 1, km1
      do i = 1, im
         rdz  = rdzt(i,k)
         bf(i,k) = (thvx(i,k+1)-thvx(i,k))*rdz
         ti(i,k) = 2./(t1(i,k)+t1(i,k+1))
         dw2  = (u1(i,k)-u1(i,k+1))**2                        &
     &        + (v1(i,k)-v1(i,k+1))**2
         shr2(i,k) = max(dw2,dw2min)*rdz*rdz
      enddo
      enddo
!
      do i = 1,im
        govrth(i) = g/theta(i,1)
      enddo
!
      do i=1,im
         beta(i)  = dt2 / (zi(i,2)-zi(i,1))
      enddo
!
      do i=1,im
         ustar(i) = sqrt(stress(i))
      enddo
!
      do i = 1,im
         sflux(i)  = heat(i) + evap(i)*fv*theta(i,1)
         if(.not.sfcflg(i) .or. sflux(i) <= 0.) pblflg(i)=.false.
      enddo
!
!  compute the pbl height
!
      do i=1,im
         flg(i) = .false.
         rbup(i) = rbsoil(i)
!
!!         if(pblflg(i)) then
!!           thermal(i) = thvx(i,1)
!!           crb(i) = crbcon
!!         else
!!           thermal(i) = tsea(i)*(1.+fv*max(q1(i,1,1),qmin))
!!           tem = sqrt(u10m(i)**2+v10m(i)**2)
!!           tem = max(tem, 1.)
!!           robn = tem / (f0 * z0(i))
!!           tem1 = 1.e-7 * robn
!!           crb(i) = 0.16 * (tem1 ** (-0.18))
!!           crb(i) = max(min(crb(i), crbmax), crbmin)
!!         endif
!
! use variable Ri for all conditions
         if(pblflg(i)) then
           thermal(i) = thvx(i,1)
         else
           thermal(i) = tsea(i)*(1.+fv*max(q1(i,1,1),qmin))
         endif
           tem = sqrt(u10m(i)**2+v10m(i)**2)
           tem = max(tem, 1.)
           robn = tem / (f0 * z0(i))
           tem1 = 1.e-7 * robn
!           crb(i) = 0.16 * (tem1 ** (-0.18))
          crb(i) = crbcon
        IF(var_ric.eq.1.) THEN
         IF(xland1(i).eq.1)  crb(I) = coef_ric_l*(tem1)**(-0.18)
         IF(xland1(i).eq.2)  crb(I) = coef_ric_s*(tem1)**(-0.18)
        ENDIF
           crb(i) = max(min(crb(i), crbmax), crbmin)
      enddo

         outp=.false.
         if(outp) then
          write(*,*)'var_ric,coef_ric_l,coef_ric_s,alpha'
          write(*,*)var_ric,coef_ric_l,coef_ric_s,alpha
          outp=.false.
         endif
      do k = 1, kmpbl
      do i = 1, im
        if(.not.flg(i)) then
          rbdn(i) = rbup(i)
          spdk2   = max((u1(i,k)**2+v1(i,k)**2),1.)
          rbup(i) = (thvx(i,k)-thermal(i))*                        &
     &              (g*zl(i,k)/thvx(i,1))/spdk2
          kpbl(i) = k
          flg(i)  = rbup(i) > crb(i)
        endif
      enddo
      enddo
      do i = 1,im
        if(kpbl(i) > 1) then
          k = kpbl(i)
          if(rbdn(i) >= crb(i)) then
            rbint = 0.
          elseif(rbup(i) <= crb(i)) then
            rbint = 1.
          else
            rbint = (crb(i)-rbdn(i))/(rbup(i)-rbdn(i))
          endif
          hpbl(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
          if(hpbl(i) < zi(i,kpbl(i))) kpbl(i) = kpbl(i) - 1
        else
          hpbl(i) = zl(i,1)
          kpbl(i) = 1
        endif
        kpblx(i) = kpbl(i)
        hpblx(i) = hpbl(i)
      enddo
!
!  compute similarity parameters 
!
      do i=1,im
         zol(i) = max(rbsoil(i)*fm(i)*fm(i)/fh(i),rimin)
         if(sfcflg(i)) then
           zol(i) = min(zol(i),-zfmin)
         else
           zol(i) = max(zol(i),zfmin)
         endif
         zol1 = zol(i)*sfcfrac*hpbl(i)/zl(i,1)
         if(sfcflg(i)) then
!          phim(i) = (1.-aphi16*zol1)**(-1./4.)
!          phih(i) = (1.-aphi16*zol1)**(-1./2.)
           tem     = 1.0 / (1. - aphi16*zol1)
           phih(i) = sqrt(tem)
           phim(i) = sqrt(phih(i))
         else
           phim(i) = 1. + aphi5*zol1
           phih(i) = phim(i)
         endif
         wscale(i) = ustar(i)/phim(i)
         ustmin(i) = ustar(i)/aphi5
         wscale(i) = max(wscale(i),ustmin(i))
      enddo
      do i=1,im
        if(pblflg(i)) then
          if(zol(i) < zolcru .and. kpbl(i) > 1) then
            pcnvflg(i) = .true.
          else
            ublflg(i) = .true.
          endif
          wst3 = govrth(i)*sflux(i)*hpbl(i)
          wstar(i)= wst3**h1
          ust3 = ustar(i)**3.
          wscaleu(i) = (ust3+wfac*vk*wst3*sfcfrac)**h1
          wscaleu(i) = max(wscaleu(i),ustmin(i))
        endif
      enddo
!
! compute counter-gradient mixing term for heat and moisture
!
      do i = 1,im
         if(ublflg(i)) then
           hgamt(i)  = min(cfac*heat(i)/wscaleu(i),gamcrt)
           hgamq(i)  = min(cfac*evap(i)/wscaleu(i),gamcrq)
           vpert     = hgamt(i) + hgamq(i)*fv*theta(i,1)
           vpert     = min(vpert,gamcrt)
           thermal(i)= thermal(i)+max(vpert,0.)
           hgamt(i)  = max(hgamt(i),0.0)
           hgamq(i)  = max(hgamq(i),0.0)
         endif
      enddo
!
!  enhance the pbl height by considering the thermal excess
!
      do i=1,im
         flg(i)  = .true.
         if(ublflg(i)) then
           flg(i)  = .false.
           rbup(i) = rbsoil(i)
         endif
      enddo
      do k = 2, kmpbl
      do i = 1, im
        if(.not.flg(i)) then
          rbdn(i) = rbup(i)
          spdk2   = max((u1(i,k)**2+v1(i,k)**2),1.)
          rbup(i) = (thvx(i,k)-thermal(i))*                       &
     &              (g*zl(i,k)/thvx(i,1))/spdk2
          kpbl(i) = k
          flg(i)  = rbup(i) > crb(i)
        endif
      enddo
      enddo
      do i = 1,im
        if(ublflg(i)) then
           k = kpbl(i)
           if(rbdn(i) >= crb(i)) then
              rbint = 0.
           elseif(rbup(i) <= crb(i)) then
              rbint = 1.
           else
              rbint = (crb(i)-rbdn(i))/(rbup(i)-rbdn(i))
           endif
           hpbl(i) = zl(i,k-1) + rbint*(zl(i,k)-zl(i,k-1))
#if (HWRF==1)
!zhang adding PBL perturtion
!zzz            if ( pert_pbl_local .and. ens_random_seed_local .gt. 0 ) then
            if ( pert_pbl_local ) then
!            print*, "zhang PBL ens_random_seed==", ens_random_seed,ens_random_seed_local
            ens_random_seed_local=ran1(-ens_random_seed_local)*1000
            rr=(2.0*ens_pblamp_local*ran1(-ens_random_seed_local)-ens_pblamp_local)
!            print*, "zhang PBL aa", rr, ens_pblamp_local,ens_random_seed_local,HPBL(I)
            HPBL(I) = HPBL(I)*(1.0+rr)
!            print*, "zhang PBL bb", rr, ens_pblamp_local,ens_random_seed_local,HPBL(I)
            endif
#endif

           if(hpbl(i) < zi(i,kpbl(i))) kpbl(i) = kpbl(i) - 1
           if(kpbl(i) <= 1) then
              ublflg(i) = .false.
              pblflg(i) = .false.
           endif
        endif
      enddo
!
!  look for stratocumulus
!
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i) .and. k <= lcld(i)) then
          if(qlx(i,k).ge.qlcr) then
             kcld(i)=k
             flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i) .and. kcld(i)==km1) scuflg(i)=.false.
      enddo
!
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i) .and. k <= kcld(i)) then
          if(qlx(i,k) >= qlcr) then
            if(radx(i,k) < radmin(i)) then
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
        if(scuflg(i) .and. krad(i) <= 1) scuflg(i)=.false.
        if(scuflg(i) .and. radmin(i)>=0.) scuflg(i)=.false.
      enddo
!
      do i = 1, im
        flg(i)=scuflg(i)
      enddo
      do k = kmpbl,2,-1
      do i = 1, im
        if(flg(i) .and. k <= krad(i)) then
          if(qlx(i,k) >= qlcr) then
            icld(i)=icld(i)+1
          else
            flg(i)=.false.
          endif
        endif
      enddo
      enddo
      do i = 1, im
        if(scuflg(i) .and. icld(i) < 1) scuflg(i)=.false.
      enddo
!
      do i = 1, im
        if(scuflg(i)) then
           hrad(i) = zi(i,krad(i)+1)
!          hradm(i)= zl(i,krad(i))
        endif
      enddo
!
      do i = 1, im
        if(scuflg(i) .and. hrad(i)<zi(i,2)) scuflg(i)=.false.
      enddo
!
      do i = 1, im
        if(scuflg(i)) then
          k    = krad(i)
          tem  = zi(i,k+1)-zi(i,k)
          tem1 = cldtime*radmin(i)/tem
          thlvx1(i) = thlvx(i,k)+tem1
!         if(thlvx1(i) > thlvx(i,k-1)) scuflg(i)=.false.
        endif
      enddo
! 
      do i = 1, im
         flg(i)=scuflg(i)
      enddo
      do k = kmpbl,1,-1
      do i = 1, im
        if(flg(i) .and. k <= krad(i))then
          if(thlvx1(i) <= thlvx(i,k))then
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
!
!     compute inverse prandtl number
!
      do i = 1, im
        if(ublflg(i)) then
          tem = phih(i)/phim(i)+cfac*vk*sfcfrac
        else
          tem = phih(i)/phim(i)
        endif
        prinv(i) =  1.0 / tem
        prinv(i) = min(prinv(i),prmax)
        prinv(i) = max(prinv(i),prmin)
      enddo
      do i = 1, im
        if(zol(i) > zolcr) then
          kpbl(i) = 1
        endif
      enddo

!!! 20150915 WeiguoWang added alpha and wind-dependent modification of K by RGF
#if (HWRF==1)
! -------------------------------------------------------------------------------------
! begin RGF modifications
! this is version MOD05


! RGF determine wspd at roughly 500 m above surface, or as close as possible,
! reuse SPDK2
!  zi(i,k) is AGL, right?  May not matter if applied only to water grid points
      if(ALPHA.lt.0)then

       DO I=1,IM
         SPDK2 = 0.
         WSPM(i,1) = 0.
         DO K = 1, KMPBL ! kmpbl is like a max possible pbl height
          if(zi(i,k).le.500.and.zi(i,k+1).gt.500.)then ! find level bracketing 500 m
           SPDK2 = SQRT(U1(i,k)*U1(i,k)+V1(i,k)*V1(i,k)) ! wspd near 500 m
           WSPM(i,1) = SPDK2/0.6  ! now the Km limit for 500 m.  just store in K=1
            !wang test , limit Kmax<100
           !  WSPM(i,1)=amin1(SPDK2/0.6, 100.0)
            !
           WSPM(i,2) = float(k)  ! height of level at gridpoint i. store in K=2
!           if(i.eq.25) print *,' IK ',i,k,' ZI ',zi(i,k), ' WSPM1 ',wspm(i,1),'
!           KMPBL ',kmpbl,' KPBL ',kpbl(i)
          endif
         ENDDO
       ENDDO ! i

      endif ! ALPHA < 0
#endif



!
!     compute diffusion coefficients below pbl
!
      do i=1,im
      do k = 1, kmpbl
         if(k < kpbl(i)) then
!           zfac = max((1.-(zi(i,k+1)-zl(i,1))/
!    1             (hpbl(i)-zl(i,1))), zfmin)
            zfac = max((1.-zi(i,k+1)/hpbl(i)), zfmin)
         !   tem = zi(i,k+1) * (zfac**pfac) 
            tem = zi(i,k+1) * (zfac**pfac) * ABS(ALPHA)

!!!! CHANGES FOR HEIGHT-DEPENDENT K ADJUSTMENT, WANG W
             if(useshape .ge. 1) then
                sz2h=(ZI(I,K+1)-ZL(I,1))/(HPBL(I)-ZL(I,1))
                sz2h=max(sz2h,zfmin)
                sz2h=min(sz2h,1.0)
                    zfac=(1.0-sz2h)**pfac
!                    smax=0.148  !! max value of this shape function
                     smax=0.148  !! max value of this shape function
                     hmax=0.333  !! roughly height if max K
                     skmax=hmax*(1.0-hmax)**pfac
                     sksfc=min(ZI(I,2)/HPBL(I),0.05)  ! surface layer top, 0.05H or ZI(2) (Zi(1)=0)
                     sksfc=sksfc*(1-sksfc)**pfac

                zfac=max(zfac,zfmin)
                ashape=max(ABS(ALPHA),0.2)  ! should not be smaller than 0.2, otherwise too much adjustment(?)
                if(useshape ==1) then 
                 ashape=( 1.0 - ((sz2h*zfac/smax)**0.25) *( 1.0 - ashape )  )
                 tem = zi(i,k+1) * (zfac) * ashape
                endif

                if (useshape == 2) then   !only adjus K that is > K_surface_top
                  ashape1=1.0
                 if (skmax > sksfc)  ashape1=(skmax*ashape-sksfc)/(skmax-sksfc)
                  skminusk0=ZI(I,K+1)*zfac - HPBL(i)*sksfc
                   tem = zi(i,k+1) * (zfac) ! no adjustment
                  if (skminusk0 > 0) then   ! only adjust K which is > surface top K
                   tem = skminusk0*ashape1 + HPBL(i)*sksfc
                  endif
                endif
             endif  ! endif useshape>1
!!!! END OF CHAGES , WANG W

!!If alpha >= 0, this is the only modification of K
! if alpha = -1, the above provides the first guess for DKU, based on assumption
! alpha = +1
!               (other values of alpha < 0 can also be applied)
! if alpha > 0, the above applies the alpha suppression factor and we are
! finished

            if(pblflg(i)) then
              tem1 = vk * wscaleu(i) * tem
!             dku(i,k) = xkzmo(i,k) + tem1
!             dkt(i,k) = xkzo(i,k)  + tem1 * prinv(i)
              dku(i,k) = tem1
              dkt(i,k) = tem1 * prinv(i)
            else
              tem1 = vk * wscale(i) * tem
!             dku(i,k) = xkzmo(i,k) + tem1
!             dkt(i,k) = xkzo(i,k)  + tem1 * prinv(i)
              dku(i,k) = tem1
              dkt(i,k) = tem1 * prinv(i)
            endif
            dku(i,k) = min(dku(i,k),dkmax)
            dku(i,k) = max(dku(i,k),xkzmo(i,k))
            dkt(i,k) = min(dkt(i,k),dkmax)
            dkt(i,k) = max(dkt(i,k),xkzo(i,k))
            dktx(i,k)= dkt(i,k)
         endif
      enddo     !K loop

#if (HWRF==1)
! possible modification of first guess DKU, under certain conditions
! (1) this applies only to columns over water

        IF(xland1(i).eq.2)then ! sea only

! (2) alpha test
! if alpha < 0, find alpha for each column and do the loop again
! if alpha > 0, we are finished


        if(alpha.lt.0)then      ! variable alpha test

! k-level of layer around 500 m
            kLOC = INT(WSPM(i,2))
!            print *,' kLOC ',kLOC,' KPBL ',KPBL(I)

! (3) only do  this IF KPBL(I) >= kLOC.  Otherwise, we are finished, with DKU as
! if alpha = +1

          if(KPBL(I).gt.kLOC)then

            xDKU = DKU(i,kLOC)     ! Km at k-level
! (4) DKU check.
! WSPM(i,1) is the KM cap for the 500-m level.
!  if DKU at 500-m level < WSPM(i,1), do not limit Km ANYWHERE.  Alpha =
!  abs(alpha).  No need to recalc.
!  if DKU at 500-m level > WSPM(i,1), then alpha = WSPM(i,1)/xDKU for entire
!  column
            if(xDKU.ge.WSPM(i,1)) then ! ONLY if DKU at 500-m exceeds cap, otherwise already done

            WSPM(i,3) = WSPM(i,1)/xDKU  ! ratio of cap to Km at k-level, store in WSPM(i,3)
            !WSPM(i,4) = amin1(WSPM(I,3),1.0) ! this is new column alpha. cap at 1. ! should never be needed
            WSPM(i,4) = min(WSPM(I,3),1.0) ! this is new column alpha. cap at 1. ! should never be needed
 !! recalculate K capped by WSPM(i,1)           
      do k = 1, kmpbl
         if(k < kpbl(i)) then
!           zfac = max((1.-(zi(i,k+1)-zl(i,1))/
!    1             (hpbl(i)-zl(i,1))), zfmin)
            zfac = max((1.-zi(i,k+1)/hpbl(i)), zfmin)
         !   tem = zi(i,k+1) * (zfac**pfac) 
            tem = zi(i,k+1) * (zfac**pfac) * WSPM(i,4)

!!! wang use different K shape, options!!!!!!!!!!!!!!!!!!!!!!!!!

!!!! CHANGES FOR HEIGHT-DEPENDENT K ADJUSTMENT, WANG W
             if(useshape .ge. 1) then
                sz2h=(ZI(I,K+1)-ZL(I,1))/(HPBL(I)-ZL(I,1))
                sz2h=max(sz2h,zfmin)
                sz2h=min(sz2h,1.0)
                    zfac=(1.0-sz2h)**pfac
                     smax=0.148  !! max value of this shape function
                     hmax=0.333  !! roughly height if max K
                     skmax=hmax*(1.0-hmax)**pfac
                     sksfc=min(ZI(I,2)/HPBL(I),0.05)  ! surface layer top, 0.05H or ZI(2) (Zi(1)=0)
                     sksfc=sksfc*(1-sksfc)**pfac

                zfac=max(zfac,zfmin)
                ashape=max(WSPM(i,4),0.2)  !! adjustment coef should not smaller than 0.2
                if(useshape ==1) then 
                 ashape=( 1.0 - ((sz2h*zfac/smax)**0.25) *( 1.0 - ashape )  )
                 tem = zi(i,k+1) * (zfac) * ashape
!                 if(k ==5) write(0,*)'min alf, height-depend alf',WSPM(i,4),ashape
                endif  ! endif useshape=1

                if (useshape == 2) then   !only adjus K that is > K_surface_top
                  ashape1=1.0
                 if (skmax > sksfc)  ashape1=(skmax*ashape-sksfc)/(skmax-sksfc)

                  skminusk0=ZI(I,K+1)*zfac - HPBL(i)*sksfc
                 tem = zi(i,k+1) * (zfac) ! no adjustment
!             if(k ==5) write(0,*)'before, dku,ashape,ashpe1',tem*wscaleu(i)*vk,ashape,ashape1
                  if (skminusk0 > 0) then   ! only adjust K which is > surface top K
                   tem = skminusk0*ashape1 + HPBL(i)*sksfc
                  endif
!            if(k ==5) write(0,*)'after, dku,k_sfc,skmax,sksfc,zi(2),hpbl',tem*wscaleu(i)*vk,WSCALEU(I)*VK*HPBL(i)*sksfc, skmax,sksfc,ZI(I,2),HPBL(I)

                endif  ! endif useshape=2
             endif  ! endif useshape>1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            if(pblflg(i)) then
              tem1 = vk * wscaleu(i) * tem
!             dku(i,k) = xkzmo(i,k) + tem1
!             dkt(i,k) = xkzo(i,k)  + tem1 * prinv(i)
              dku(i,k) = tem1
              dkt(i,k) = tem1 * prinv(i)
            else
              tem1 = vk * wscale(i) * tem
!             dku(i,k) = xkzmo(i,k) + tem1
!             dkt(i,k) = xkzo(i,k)  + tem1 * prinv(i)
              dku(i,k) = tem1
              dkt(i,k) = tem1 * prinv(i)
            endif
            dku(i,k) = min(dku(i,k),dkmax)
            dku(i,k) = max(dku(i,k),xkzmo(i,k))
            dkt(i,k) = min(dkt(i,k),dkmax)
            dkt(i,k) = max(dkt(i,k),xkzo(i,k))
            dktx(i,k)= dkt(i,k)
         endif
      enddo     !K loop
            endif ! xDKU.ge.WSPM(i,1)
          endif ! KPBL(I).ge.kLOC
         endif ! alpha < 0
         endif ! xland1 = 2

#endif
      enddo     ! I loop


!
! compute diffusion coefficients based on local scheme above pbl
!
      do k = 1, km1
         do i=1,im
            if(k >= kpbl(i)) then
               bvf2 = g*bf(i,k)*ti(i,k)
               ri   = max(bvf2/shr2(i,k),rimin)
               zk   = vk*zi(i,k+1)
               if(ri < 0.) then ! unstable regime
                  rl2      = zk*rlamun/(rlamun+zk)
                  dk       = rl2*rl2*sqrt(shr2(i,k))
                  sri      = sqrt(-ri)
!                 dku(i,k) = xkzmo(i,k) + dk*(1+8.*(-ri)/(1+1.746*sri))
!                 dkt(i,k) = xkzo(i,k)  + dk*(1+8.*(-ri)/(1+1.286*sri))
                  dku(i,k) = dk*(1+8.*(-ri)/(1+1.746*sri))
                  dkt(i,k) = dk*(1+8.*(-ri)/(1+1.286*sri))
               else             ! stable regime
                  rl2      = zk*rlam/(rlam+zk)
!!                tem      = rlam * sqrt(0.01*prsi(i,k))
!!                rl2      = zk*tem/(tem+zk)
                  dk       = rl2*rl2*sqrt(shr2(i,k))
                  tem1     = dk/(1+5.*ri)**2
!
                  if(k >= kpblx(i)) then
                    prnum = 1.0 + 2.1*ri
                    prnum = min(prnum,prmax)
                  else
                    prnum = 1.0
                  endif
!                 dku(i,k) = xkzmo(i,k) + tem1 * prnum
!                 dkt(i,k) = xkzo(i,k)  + tem1
                  dku(i,k) = tem1 * prnum
                  dkt(i,k) = tem1
               endif
!
               dku(i,k) = min(dku(i,k),dkmax)
               dku(i,k) = max(dku(i,k),xkzmo(i,k))
               dkt(i,k) = min(dkt(i,k),dkmax)
               dkt(i,k) = max(dkt(i,k),xkzo(i,k))
!
            endif
!
         enddo
      enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  compute components for mass flux mixing by large thermals
!
      do k = 1, km
        do i = 1, im
          if(pcnvflg(i)) then
            tcko(i,k) = t1(i,k)
            ucko(i,k) = u1(i,k)
            vcko(i,k) = v1(i,k)
            xmf(i,k) = 0.
          endif
        enddo
      enddo
      do kk = 1, ntrac
      do k = 1, km
        do i = 1, im
          if(pcnvflg(i)) then
            qcko(i,k,kk) = q1(i,k,kk)
          endif
        enddo
      enddo
      enddo
!
      call mfpbl(im,ix,km,ntrac,dt2,pcnvflg,                  &
     &       zl,zi,thvx,q1,t1,u1,v1,hpbl,kpbl,                &
     &       sflux,ustar,wstar,xmf,tcko,qcko,ucko,vcko)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  compute diffusion coefficients for cloud-top driven diffusion
!  if the condition for cloud-top instability is met,
!    increase entrainment flux at cloud top
!
      do i = 1, im
        if(scuflg(i)) then
           k = krad(i)
           tem = thetae(i,k) - thetae(i,k+1)
           tem1 = qtx(i,k) - qtx(i,k+1)
           if (tem > 0. .and. tem1 > 0.) then
             cteit= cp*tem/(hvap*tem1)
             if(cteit > actei) rent(i) = rentf2
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
!
      do k = 1, kmpbl
         do i=1,im
            if(scuflg(i) .and. k < krad(i)) then
               tem1=hrad(i)-zd(i)
               tem2=zi(i,k+1)-tem1
               if(tem2 > 0.) then
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
            ! dkt(i,k) = dkt(i,k)+ckt(i,k)
            ! dku(i,k) = dku(i,k)+cku(i,k)
         !! if K needs to be adjusted by alpha, then no need to add this term
            if(alpha == 1.0)  dkt(i,k) = dkt(i,k)+ckt(i,k)
            if(alpha == 1.0)  dku(i,k) = dku(i,k)+cku(i,k)
             dkt(i,k) = min(dkt(i,k),dkmax)
             dku(i,k) = min(dku(i,k),dkmax)
          endif
        enddo
      enddo
!
!     compute tridiagonal matrix elements for heat and moisture
!
      do i=1,im
         ad(i,1) = 1.
         a1(i,1) = t1(i,1)   + beta(i) * heat(i)
         a2(i,1) = q1(i,1,1) + beta(i) * evap(i)
      enddo

      if(ntrac >= 2) then
        do k = 2, ntrac
          is = (k-1) * km
          do i = 1, im
            a2(i,1+is) = q1(i,1,k)
          enddo
        enddo
      endif
!
      do k = 1,km1
        do i = 1,im
          dtodsd = dt2/del(i,k)
          dtodsu = dt2/del(i,k+1)
          dsig   = prsl(i,k)-prsl(i,k+1)
          rdz    = rdzt(i,k)
          tem1   = dsig * dkt(i,k) * rdz
          dsdz2     = tem1 * rdz
          au(i,k)   = -dtodsd*dsdz2
          al(i,k)   = -dtodsu*dsdz2
!
          if(pcnvflg(i) .and. k < kpbl(i)) then
             tem2      = dsig * rdz
             ptem      = 0.5 * tem2 * xmf(i,k)
             ptem1     = dtodsd * ptem
             ptem2     = dtodsu * ptem
             ad(i,k)   = ad(i,k)-au(i,k)-ptem1
             ad(i,k+1) = 1.-al(i,k)+ptem2
             au(i,k)   = au(i,k)-ptem1
             al(i,k)   = al(i,k)+ptem2
             ptem      = tcko(i,k) + tcko(i,k+1)
             dsdzt     = tem1 * gocp
             a1(i,k)   = a1(i,k)+dtodsd*dsdzt-ptem1*ptem
             a1(i,k+1) = t1(i,k+1)-dtodsu*dsdzt+ptem2*ptem
             ptem      = qcko(i,k,1) + qcko(i,k+1,1)
             a2(i,k)   = a2(i,k) - ptem1 * ptem
             a2(i,k+1) = q1(i,k+1,1) + ptem2 * ptem
          elseif(ublflg(i) .and. k < kpbl(i)) then
             ptem1 = dsig * dktx(i,k) * rdz
             tem   = 1.0 / hpbl(i)
             dsdzt = tem1 * gocp - ptem1 * hgamt(i) * tem
             dsdzq = - ptem1 * hgamq(i) * tem
             ad(i,k)   = ad(i,k)-au(i,k)
             ad(i,k+1) = 1.-al(i,k)
             a1(i,k)   = a1(i,k)+dtodsd*dsdzt
             a1(i,k+1) = t1(i,k+1)-dtodsu*dsdzt
             a2(i,k)   = a2(i,k)+dtodsd*dsdzq
             a2(i,k+1) = q1(i,k+1,1)-dtodsu*dsdzq
          else
             ad(i,k)   = ad(i,k)-au(i,k)
             ad(i,k+1) = 1.-al(i,k)
             dsdzt     = tem1 * gocp
             a1(i,k)   = a1(i,k)+dtodsd*dsdzt
             a1(i,k+1) = t1(i,k+1)-dtodsu*dsdzt
             a2(i,k+1) = q1(i,k+1,1)
          endif
!
        enddo
      enddo
!
      if(ntrac >= 2) then
        do kk = 2, ntrac
          is = (kk-1) * km
          do k = 1, km1
            do i = 1, im
              if(pcnvflg(i) .and. k < kpbl(i)) then
                dtodsd = dt2/del(i,k)
                dtodsu = dt2/del(i,k+1)
                dsig  = prsl(i,k)-prsl(i,k+1)
                tem   = dsig * rdzt(i,k)
                ptem  = 0.5 * tem * xmf(i,k)
                ptem1 = dtodsd * ptem
                ptem2 = dtodsu * ptem
                tem1  = qcko(i,k,kk) + qcko(i,k+1,kk)
                a2(i,k+is) = a2(i,k+is) - ptem1*tem1
                a2(i,k+1+is)= q1(i,k+1,kk) + ptem2*tem1
              else
                a2(i,k+1+is) = q1(i,k+1,kk)
              endif
            enddo
          enddo
        enddo
      endif
!
!     solve tridiagonal problem for heat and moisture
!
      call tridin(im,km,ntrac,al,ad,au,a1,a2,au,a1,a2)

!
!     recover tendencies of heat and moisture
!
      do  k = 1,km
         do i = 1,im
            ttend      = (a1(i,k)-t1(i,k)) * rdt
            qtend      = (a2(i,k)-q1(i,k,1))*rdt
            tau(i,k)   = tau(i,k)+ttend
            rtg(i,k,1) = rtg(i,k,1)+qtend
            dtsfc(i)   = dtsfc(i)+cont*del(i,k)*ttend
            dqsfc(i)   = dqsfc(i)+conq*del(i,k)*qtend
         enddo
      enddo
      if(ntrac >= 2) then
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
!
!   compute tke dissipation rate
!
      if(dspheat) then
!
      do k = 1,km1
        do i = 1,im
          diss(i,k) = dku(i,k)*shr2(i,k)-g*ti(i,k)*dkt(i,k)*bf(i,k)
!         diss(i,k) = dku(i,k)*shr2(i,k)
        enddo
      enddo
!
!     add dissipative heating at the first model layer
!
      do i = 1,im
         tem   = govrth(i)*sflux(i)
         tem1  = tem + stress(i)*spd1(i)/zl(i,1)
         tem2  = 0.5 * (tem1+diss(i,1)) 
         tem2  = max(tem2, 0.)
         ttend = tem2 / cp
!         tau(i,1) = tau(i,1)+0.5*ttend
         tau(i,1) = tau(i,1)+0.7*ttend
      enddo
!
!     add dissipative heating above the first model layer
!
      do k = 2,km1
        do i = 1,im
          tem = 0.5 * (diss(i,k-1)+diss(i,k))
          tem  = max(tem, 0.)
          ttend = tem / cp
!          tau(i,k) = tau(i,k) + 0.5*ttend
          tau(i,k) = tau(i,k) + 0.7*ttend
        enddo
      enddo
!
      endif
!
!     compute tridiagonal matrix elements for momentum
!
      do i=1,im
         ad(i,1) = 1.0 + beta(i) * stress(i) / spd1(i)
         a1(i,1) = u1(i,1)
         a2(i,1) = v1(i,1)
      enddo
!
      do k = 1,km1
        do i=1,im
          dtodsd  = dt2/del(i,k)
          dtodsu  = dt2/del(i,k+1)
          dsig    = prsl(i,k)-prsl(i,k+1)
          rdz     = rdzt(i,k)
          tem1    = dsig*dku(i,k)*rdz
          dsdz2   = tem1 * rdz
          au(i,k) = -dtodsd*dsdz2
          al(i,k) = -dtodsu*dsdz2
!
          if(pcnvflg(i) .and. k < kpbl(i)) then
             tem2      = dsig * rdz
             ptem      = 0.5 * tem2 * xmf(i,k)
             ptem1     = dtodsd * ptem
             ptem2     = dtodsu * ptem
             ad(i,k)   = ad(i,k)-au(i,k)-ptem1
             ad(i,k+1) = 1.-al(i,k)+ptem2
             au(i,k)   = au(i,k)-ptem1
             al(i,k)   = al(i,k)+ptem2
             ptem      = ucko(i,k) + ucko(i,k+1)
             a1(i,k)   = a1(i,k) - ptem1 * ptem
             a1(i,k+1) = u1(i,k+1) + ptem2 * ptem
             ptem      = vcko(i,k) + vcko(i,k+1)
             a2(i,k)   = a2(i,k) - ptem1 * ptem
             a2(i,k+1) = v1(i,k+1) + ptem2 * ptem
          else
             ad(i,k)   = ad(i,k)-au(i,k)
             ad(i,k+1) = 1.-al(i,k)
             a1(i,k+1) = u1(i,k+1)
             a2(i,k+1) = v1(i,k+1)
          endif
!
        enddo
      enddo
!
!     solve tridiagonal problem for momentum
!
      call tridi2(im,km,al,ad,au,a1,a2,au,a1,a2)
!
!     recover tendencies of momentum
!
      do k = 1,km
         do i = 1,im
            utend = (a1(i,k)-u1(i,k))*rdt
            vtend = (a2(i,k)-v1(i,k))*rdt
            du(i,k)  = du(i,k)  + utend
            dv(i,k)  = dv(i,k)  + vtend
            dusfc(i) = dusfc(i) + conw*del(i,k)*utend
            dvsfc(i) = dvsfc(i) + conw*del(i,k)*vtend
!
!  for dissipative heating for ecmwf model
!
!           tem1 = 0.5*(a1(i,k)+u1(i,k))
!           tem2 = 0.5*(a2(i,k)+v1(i,k))
!           diss(i,k) = -(tem1*utend+tem2*vtend)
!           diss(i,k) = max(diss(i,k),0.)
!           ttend = diss(i,k) / cp
!           tau(i,k) = tau(i,k) + ttend
!
         enddo
      enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      do i = 1, im
         hpbl(i) = hpblx(i)
         kpbl(i) = kpblx(i)
      enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      return
      end subroutine moninedmf
!c-----------------------------------------------------------------------
      subroutine tridi2(l,n,cl,cm,cu,r1,r2,au,a1,a2)
!cc
      USE MODULE_GFS_MACHINE, only : kind_phys
      implicit none
      integer             k,n,l,i
      real(kind=kind_phys) fk
!cc
      real(kind=kind_phys) cl(l,2:n),cm(l,n),cu(l,n-1),r1(l,n),r2(l,n), &
     &          au(l,n-1),a1(l,n),a2(l,n)
!c-----------------------------------------------------------------------
      do i=1,l
        fk      = 1./cm(i,1)
        au(i,1) = fk*cu(i,1)
        a1(i,1) = fk*r1(i,1)
        a2(i,1) = fk*r2(i,1)
      enddo
      do k=2,n-1
        do i=1,l
          fk      = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
          au(i,k) = fk*cu(i,k)
          a1(i,k) = fk*(r1(i,k)-cl(i,k)*a1(i,k-1))
          a2(i,k) = fk*(r2(i,k)-cl(i,k)*a2(i,k-1))
        enddo
      enddo
      do i=1,l
        fk      = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
        a1(i,n) = fk*(r1(i,n)-cl(i,n)*a1(i,n-1))
        a2(i,n) = fk*(r2(i,n)-cl(i,n)*a2(i,n-1))
      enddo
      do k=n-1,1,-1
        do i=1,l
          a1(i,k) = a1(i,k)-au(i,k)*a1(i,k+1)
          a2(i,k) = a2(i,k)-au(i,k)*a2(i,k+1)
        enddo
      enddo
!-----------------------------------------------------------------------
      return
      end subroutine tridi2
!-----------------------------------------------------------------------
      subroutine tridin(l,n,nt,cl,cm,cu,r1,r2,au,a1,a2)
!!
      USE MODULE_GFS_MACHINE     , only : kind_phys
      implicit none
      integer             is,k,kk,n,nt,l,i
      real(kind=kind_phys) fk(l)
!!
      real(kind=kind_phys) cl(l,2:n), cm(l,n), cu(l,n-1),     &
     &                     r1(l,n),   r2(l,n*nt),             &
     &                     au(l,n-1), a1(l,n), a2(l,n*nt),    &  
     &                     fkk(l,2:n-1)
!-----------------------------------------------------------------------
      do i=1,l
        fk(i)   = 1./cm(i,1)
        au(i,1) = fk(i)*cu(i,1)
        a1(i,1) = fk(i)*r1(i,1)
      enddo
      do k = 1, nt
        is = (k-1) * n
        do i = 1, l
          a2(i,1+is) = fk(i) * r2(i,1+is)
        enddo
      enddo
      do k=2,n-1
        do i=1,l
          fkk(i,k) = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
          au(i,k)  = fkk(i,k)*cu(i,k)
          a1(i,k)  = fkk(i,k)*(r1(i,k)-cl(i,k)*a1(i,k-1))
        enddo
      enddo
      do kk = 1, nt
        is = (kk-1) * n
        do k=2,n-1
          do i=1,l
            a2(i,k+is) = fkk(i,k)*(r2(i,k+is)-cl(i,k)*a2(i,k+is-1))
          enddo
        enddo
      enddo
      do i=1,l
        fk(i)   = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
        a1(i,n) = fk(i)*(r1(i,n)-cl(i,n)*a1(i,n-1))
      enddo
      do k = 1, nt
        is = (k-1) * n
        do i = 1, l
          a2(i,n+is) = fk(i)*(r2(i,n+is)-cl(i,n)*a2(i,n+is-1))
        enddo
      enddo
      do k=n-1,1,-1
        do i=1,l
          a1(i,k) = a1(i,k) - au(i,k)*a1(i,k+1)
        enddo
      enddo
      do kk = 1, nt
        is = (kk-1) * n
        do k=n-1,1,-1
          do i=1,l
            a2(i,k+is) = a2(i,k+is) - au(i,k)*a2(i,k+is+1)
          enddo
        enddo
      enddo
!-----------------------------------------------------------------------
      return
      end subroutine tridin
!----------------------------------------------------------------------

!!!!!  ==========================================================  !!!!!
! subroutine 'mfpbl' computes mass-flux components, called by 
!  subroutine 'moninedmf'.
!
      subroutine mfpbl(im,ix,km,ntrac,delt,cnvflg,          &
     &   zl,zm,thvx,q1,t1,u1,v1,hpbl,kpbl,                  &
     &   sflx,ustar,wstar,xmf,tcko,qcko,ucko,vcko)
!
      USE MODULE_GFS_MACHINE, only : kind_phys
      USE MODULE_GFS_PHYSCONS, grav => con_g, cp => con_cp
!
      implicit none
!
      integer              im, ix, km, ntrac
!    &,                    me
      integer              kpbl(im)
      logical              cnvflg(im)
      real(kind=kind_phys) delt
      real(kind=kind_phys) q1(ix,km,ntrac), t1(ix,km),             &
     &                     u1(ix,km),  v1(ix,km),                  &
     &                     thvx(im,km),                            &
     &                     zl(im,km),  zm(im,km+1),                &
     &                     hpbl(im),   sflx(im),    ustar(im),     &
     &                     wstar(im),  xmf(im,km),                 &
     &                     tcko(im,km),qcko(im,km,ntrac),          & 
     &                     ucko(im,km),vcko(im,km)                 
!
!c  local variables and arrays
!
      integer   i, j, k, n, kmpbl
!
      real(kind=kind_phys) dt2,     dz,      ce0,                 &
     &                     h1,      factor,  gocp,                &
     &                     g,       c1,      d1,                  &
     &                     b1,      f1,      bb1,     bb2,        & 
     &                     alp,     a1,      qmin,    zfmin,      &
     &                     xmmx,    rbint,   tau,                 &
!    &                     rbint,   tau,                          &
     &                     tem,     tem1,    tem2,                &
     &                     ptem,    ptem1,   ptem2,               &  
     &                     pgcon
!
      real(kind=kind_phys) sigw1(im),   usws3(im),  xlamax(im),   &
     &                     rbdn(im),    rbup(im),   delz(im)
!
      real(kind=kind_phys) wu2(im,km),     xlamue(im,km),         &
     &                     thvu(im,km),    zi(im,km),             &
     &                     buo(im,km)
!
      logical totflg, flg(im)
!
!c  physical parameters
      parameter(g=grav)
      parameter(gocp=g/cp)
!     parameter(ce0=0.37,qmin=1.e-8,alp=1.0,pgcon=0.55)
      parameter(ce0=0.38,qmin=1.e-8,alp=1.0,pgcon=0.55)
      parameter(a1=0.08,b1=0.5,f1=0.15,c1=0.3,d1=2.58,tau=500.)
      parameter(zfmin=1.e-8,h1=0.33333333)
!
!c-----------------------------------------------------------------------
!
!************************************************************************
!
      kmpbl = km/2 + 1
      dt2 = delt
!!
      totflg = .true.
      do i=1,im
        totflg = totflg .and. (.not. cnvflg(i))
      enddo
      if(totflg) return
!!
      do k = 1, km
        do i=1,im
          if (cnvflg(i)) then
            zi(i,k) = zm(i,k+1)
          endif
        enddo
      enddo
!
      do i=1,im
        if(cnvflg(i)) then 
          k = kpbl(i) / 2
          k = max(k, 1) 
          delz(i) = zl(i,k+1) - zl(i,k)
          xlamax(i) = ce0 / delz(i)
        endif
      enddo
      do k = 1, kmpbl
        do i=1,im
          if(cnvflg(i)) then
            if(k < kpbl(i)) then
              ptem = 1./(zi(i,k)+delz(i))
              tem = max((hpbl(i)-zi(i,k)+delz(i)) ,delz(i))
              ptem1 = 1./tem
              xlamue(i,k) = ce0 * (ptem+ptem1)
            else
              xlamue(i,k) = xlamax(i)
            endif
          endif
        enddo
      enddo
!
!  compute thermal excess
!
      do i=1,im
        if(cnvflg(i)) then
          tem = zl(i,1)/hpbl(i)
          usws3(i) = (ustar(i)/wstar(i))**3.
          tem1 = usws3(i) + 0.6*tem
          tem2 = max((1.-tem), zfmin)
          ptem = (tem1**h1) * sqrt(tem2)
          sigw1(i) = 1.3 * ptem * wstar(i)
          ptem1 = alp * sflx(i) / sigw1(i)
          thvu(i,1) = thvx(i,1) + ptem1
          buo(i,1) = g * (thvu(i,1)/thvx(i,1)-1.)
        endif
      enddo
!
!  compute potential temperature and buoyancy for updraft air parcel
!
      do k = 2, kmpbl
        do i=1,im
          if(cnvflg(i)) then
            dz = zl(i,k) - zl(i,k-1)
            tem = xlamue(i,k-1) * dz
            ptem = 2. + tem
            ptem1 = (2. - tem) / ptem
            tem1 = tem  * (thvx(i,k)+thvx(i,k-1)) / ptem
            thvu(i,k) = ptem1 * thvu(i,k-1) + tem1
            buo(i,k) = g * (thvu(i,k)/thvx(i,k)-1.)
          endif
        enddo
      enddo
!
!  compute updraft velocity square(wu2)
!
!     tem = 1.-2.*f1
!     bb1 = 2. * b1 / tem
!     bb2 = 2. / tem
!  from soares et al. (2004,qjrms)
!     bb1 = 2.
!     bb2 = 4.
!
!  from bretherton et al. (2004, mwr)
!     bb1 = 4.
!     bb2 = 2.
!
!  from our tuning
      bb1 = 1.8
      bb2 = 3.5 
!
      do i = 1, im
        if(cnvflg(i)) then
!
!         tem = zi(i,1)/hpbl(i)
!         tem1 = usws3(i) + 0.6*tem
!         tem2 = max((1.-tem), zfmin)
!         ptem = (tem1**h1) * sqrt(tem2)
!         ptem1 = 1.3 * ptem * wstar(i)
!         wu2(i,1) = d1*d1*ptem1*ptem1
!
          dz   = zi(i,1)
          tem  = 0.5*bb1*xlamue(i,1)*dz
          tem1 = bb2 * buo(i,1) * dz
          ptem1 = 1. + tem
          wu2(i,1) = tem1 / ptem1
!
        endif
      enddo
      do k = 2, kmpbl
        do i = 1, im
          if(cnvflg(i)) then
            dz    = zi(i,k) - zi(i,k-1)
            tem  = 0.25*bb1*(xlamue(i,k)+xlamue(i,k-1))*dz
            tem1 = bb2 * buo(i,k) * dz
            ptem = (1. - tem) * wu2(i,k-1)
            ptem1 = 1. + tem
            wu2(i,k) = (ptem + tem1) / ptem1
          endif
        enddo
      enddo
!
!  update pbl height as the height where updraft velocity vanishes
!
      do i=1,im
         flg(i)  = .true.
         if(cnvflg(i)) then
           flg(i)  = .false.
           rbup(i) = wu2(i,1)
         endif
      enddo
      do k = 2, kmpbl
      do i = 1, im
        if(.not.flg(i)) then
          rbdn(i) = rbup(i)
          rbup(i) = wu2(i,k)
          kpbl(i) = k
          flg(i)  = rbup(i).le.0.
        endif
      enddo
      enddo
      do i = 1,im
        if(cnvflg(i)) then
           k = kpbl(i)
           if(rbdn(i) <= 0.) then
              rbint = 0.
           elseif(rbup(i) >= 0.) then
              rbint = 1.
           else
              rbint = rbdn(i)/(rbdn(i)-rbup(i))
           endif
           hpbl(i) = zi(i,k-1) + rbint*(zi(i,k)-zi(i,k-1))
        endif
      enddo
!c
      do i=1,im
        if(cnvflg(i)) then
          k = kpbl(i) / 2
          k = max(k, 1)
          delz(i) = zl(i,k+1) - zl(i,k)
          xlamax(i) = ce0 / delz(i)
        endif
      enddo
!
!  update entrainment rate
!
!     do k = 1, kmpbl
!       do i=1,im
!         if(cnvflg(i)) then
!           if(k < kpbl(i)) then
!             tem = tau * sqrt(wu2(i,k))
!             tem1 = 1. / tem
!             ptem = ce0 / zi(i,k)
!             xlamue(i,k) = max(tem1, ptem)
!           else
!             xlamue(i,k) = xlamax(i)
!           endif
!         endif
!       enddo
!     enddo
!
      do k = 1, kmpbl
        do i=1,im
          if(cnvflg(i)) then
            if(k < kpbl(i)) then
              ptem = 1./(zi(i,k)+delz(i))
              tem = max((hpbl(i)-zi(i,k)+delz(i)) ,delz(i))
              ptem1 = 1./tem
              xlamue(i,k) = ce0 * (ptem+ptem1)
            else
              xlamue(i,k) = xlamax(i)
            endif
          endif
        enddo
      enddo
!
!  updraft mass flux as a function of sigmaw
!   (0.3*sigmaw[square root of vertical turbulence variance])
!
!     do k = 1, kmpbl
!       do i=1,im
!         if(cnvflg(i) .and. k < kpbl(i)) then
!           tem = zi(i,k)/hpbl(i)
!           tem1 = usws3(i) + 0.6*tem
!           tem2 = max((1.-tem), zfmin)
!           ptem = (tem1**h1) * sqrt(tem2)
!           ptem1 = 1.3 * ptem * wstar(i)
!           xmf(i,k) = c1 * ptem1
!         endif
!       enddo
!     enddo
!
!  updraft mass flux as a function of updraft velocity profile
!
      do k = 1, kmpbl
        do i = 1, im
          if (cnvflg(i) .and. k < kpbl(i)) then
             xmf(i,k) = a1 * sqrt(wu2(i,k))
             dz   = zl(i,k+1) - zl(i,k)
             xmmx = dz / dt2
             xmf(i,k) = min(xmf(i,k),xmmx)
          endif
        enddo
      enddo
!c
!c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c  compute updraft property
!c
      do k = 2, kmpbl
        do i = 1, im
          if (cnvflg(i) .and. k <= kpbl(i)) then
             dz   = zl(i,k) - zl(i,k-1)
             tem  = 0.5 * xlamue(i,k-1) * dz
             factor = 1. + tem
             ptem = tem + pgcon
             ptem1= tem - pgcon
!
             tcko(i,k) = ((1.-tem)*tcko(i,k-1)+tem*               &
     &                    (t1(i,k)+t1(i,k-1))-gocp*dz)/factor
             ucko(i,k) = ((1.-tem)*ucko(i,k-1)+ptem*u1(i,k)       & 
     &                    +ptem1*u1(i,k-1))/factor                 
             vcko(i,k) = ((1.-tem)*vcko(i,k-1)+ptem*v1(i,k)       &
     &                    +ptem1*v1(i,k-1))/factor
          endif
        enddo
      enddo
      do n = 1, ntrac
      do k = 2, kmpbl
        do i = 1, im
          if (cnvflg(i) .and. k <= kpbl(i)) then
             dz   = zl(i,k) - zl(i,k-1)
             tem  = 0.5 * xlamue(i,k-1) * dz
             factor = 1. + tem
 
             qcko(i,k,n) = ((1.-tem)*qcko(i,k-1,n)+tem*           &
     &                    (q1(i,k,n)+q1(i,k-1,n)))/factor
          endif
        enddo
      enddo
      enddo
!
      return
      end subroutine mfpbl
!----------------------------------------------------------------------
#endif
      END MODULE module_bl_gfsedmf
