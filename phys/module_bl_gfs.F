!LWRF:MODEL_LAYER:PHYSICS
!
MODULE module_bl_gfs

CONTAINS

!-------------------------------------------------------------------          
   SUBROUTINE BL_GFS(U3D,V3D,TH3D,T3D,QV3D,QC3D,QI3D,P3D,PI3D,     &
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
#if (HWRF==1)
                  ALPHA,                                           &
                  HPBL2D, EVAP2D, HEAT2D,                          &    !Kwon add FOR SHAL. CON.
                  VAR_RIC,                                         &    !Kwon for variable Ric
                  U10,V10,ZNT,MZNT,rc2d,                           &    !Kwon for variable Ric
                  DKU3D,DKT3D,coef_ric_l,coef_ric_s,xland,         &    !Kwon for variable Ric
                  msang,scurx,scury,iwavecpl,lcurr_sf,                      &
                  pert_pbl, ens_random_seed, ens_pblamp,           &
#endif
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
!-- ALPHA       boundary depth scaling factor
!-- VAR_RIC     Flag for using variable Ric or not (=1: variable Ric, =0: constant Ric)
!-- RO          Surface Rossby number
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

#if (NMM_CORE==1)
      LOGICAL , INTENT(IN)::            DISHEAT                         !  gopal's doing
#endif

#if (HWRF==1)
      INTEGER , INTENT(IN)          ::  iwavecpl
      LOGICAL , INTENT(IN)          ::  lcurr_sf
      REAL,  DIMENSION(ims:ime, jms:jme), INTENT(OUT) ::              &
                                        HPBL2D,                         &    !ADDED BY KWON FOR SHALLOW CONV.
                                        EVAP2D,                         &    !ADDED BY KWON FOR SHALLOW CONV.
                                        HEAT2D,RC2D,MZNT                     !ADDED BY KWON FOR SHALLOW CONV.
      REAL,  DIMENSION(ims:ime, jms:jme), INTENT(IN) ::              &
                                        U10,                         &    !ADDED BY KWON FOR VARIABLE Ric
                                        V10,XLAND,                   &    !ADDED BY KWON FOR VARIABLE Ric
                                        ZNT                               !ADDED BY KWON FOR VARIABLE Ric
      REAL,  DIMENSION(ims:ime, jms:jme, kms:kme), INTENT(OUT) :: DKU3D,DKT3D  
      REAL,    INTENT(IN) :: VAR_RIC,coef_ric_l,coef_ric_s                   !ADDED BY KWON
      REAL,  DIMENSION(ims:ime, jms:jme), INTENT(IN) ::                 &
                                        SCURX,                          &
                                        SCURY,                          &
                                        MSANG
      integer,intent(in) :: ens_random_seed
      real,intent(in) :: ens_pblamp
      logical,intent(in) :: pert_pbl
#endif



      INTEGER, INTENT(IN) ::            ids,ide, jds,jde, kds,kde,      &
                                        ims,ime, jms,jme, kms,kme,      &
                                        its,ite, jts,jte, kts,kte,      &
                                        P_QI,P_FIRST_SCALAR

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
#if (HWRF==1)
                                        dku,dkt,   &       !Kwon for diffusivity
#endif
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
                                        XLAND1,                         &
                                        SPD1,                           &
                                        STRESS,                         &
                                        RO,rbcr,                        &  !Kwon for variablr Ric(surface Rossby number)
                                        TSEA

      REAL     (kind=kind_phys) ::                                      &
                                        CPM,                            &
                                        cpmikj,                         &
                                        DELTIM,                         &
                                        FMTMP,                          &
                                        RRHOX

#if (HWRF == 1)
      REAL      ::      ALPHA
#else
      REAL, PARAMETER:: ALPHA=1.0
#endif

#if (HWRF == 1)
      REAL :: UBOT, VBOT, UBOT1, VBOT1
#endif
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
        QSS(i)=QV3D(i,kts,j)               ! not used in moninp so set to qv3d for now
        HEAT(i)=HFX(i,j)/CPM*RRHOX
        EVAP(i)=QFX(i,j)*RRHOX
        XLAND1(i) = 0.0        
#if (HWRF==1)
! Kwon FOR NEW SHALLOW CONVECTION 
        HEAT2D(i,j)=HFX(i,j)/CPM*RRHOX
        EVAP2D(i,j)=QFX(i,j)*RRHOX
        XLAND1(i) = XLAND(I,J)
#endif
!
#if (HWRF==1)
        IF ( XLAND(I,J)  > 1.99 ) THEN
          IF ( LCURR_SF ) THEN
            UBOT = U3D(I,KTS,J)-SCURX(I,J)
            VBOT = V3D(I,KTS,J)-SCURY(I,J)
          ELSE
            UBOT = U3D(I,KTS,J)
            VBOT = V3D(I,KTS,J)
          ENDIF
          IF ( IWAVECPL .eq. 1 ) THEN
            UBOT1 = ( UBOT * COS(MSANG(I,J))  -               &
                     VBOT * SIN(MSANG(I,J)) )                 &
                                  * COS(MSANG(I,J))
            VBOT1 = ( VBOT * COS(MSANG(I,J))  -               &
                     UBOT * SIN(MSANG(I,J)) )                &
                                  * COS(MSANG(I,J))

            WSPD(i,j) = SQRT(UBOT1*UBOT1+VBOT1*VBOT1) + 1.E-9
          ELSE
            WSPD(i,j) = SQRT(UBOT*UBOT+VBOT*VBOT) + 1.E-9
          ENDIF
        ENDIF
#endif
        STRESS(i)=KARMAN*KARMAN*WSPD(i,j)*WSPD(i,j)/(FMTMP*FMTMP)
        SPD1(i)=WSPD(i,j)
        PRSI(i,kts)=PSFC(i,j)*.001
        PHII(I,kts)=0.
        RCL(i)=1.
        RBSOIL(I)=BR(i,j)
#if (HWRF==1)
! Kwon for variable Ric   : Ro=W10/(f*zo): surface Rossby number
       Ro(I)=SQRT(U10(I,J)**2 + V10(I,J)**2) / (1.E-4 * MZNT(I,J))
#endif
      ENDDO

      DO k=kts,kte
        DO i=its,ite 
          DV(I,K) = 0.
          DU(I,K) = 0.
          TAU(I,K) = 0.
#if (HWRF==1)
          IF ( XLAND(I,J) > 1.99  .AND. k == KTS ) THEN
            IF ( LCURR_SF ) THEN
              UBOT = U3D(i,k,j) - SCURX(I,J)
              VBOT = V3D(i,k,j) - SCURY(I,J)
            ELSE
              UBOT = U3D(i,k,j)
              VBOT = V3D(i,k,j)
            ENDIF
            IF ( IWAVECPL .eq. 1 ) THEN
              U1(I,K) = ( UBOT * COS(MSANG(I,J))  -            &
                          VBOT * SIN(MSANG(I,J)) )             &
                                  * COS(MSANG(I,J))
              V1(I,K) = ( VBOT * COS(MSANG(I,J))  -            &
                          UBOT * SIN(MSANG(I,J)) )             &
                                  * COS(MSANG(I,J))
            ELSE
              U1(I,K) =  UBOT
              V1(I,K) =  VBOT
            ENDIF
          ELSE
            U1(I,K) = U3D(i,k,j)
            V1(I,K) = V3D(i,k,j)
          ENDIF
#else
          U1(I,K) = U3D(i,k,j)
          V1(I,K) = V3D(i,k,j)
#endif
          T1(I,K) = T3D(i,k,j)
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
      CALL MONINP(IM,IM,KX,NTRAC,DV,DU,TAU,RTG,U1,V1,T1,Q1,             &
                  PSK,RBSOIL,FM,FH,TSEA,QSS,HEAT,EVAP,STRESS,           &
                  SPD1,KPBL,PRSI,DEL,PRSL,PRSLK,PHII,PHIL,RCL,          &
                  DELTIM,DUSFC,DVSFC,DTSFC,DQSFC,HPBL,HGAMT,            &
#if (HWRF==1)
               VAR_RIC,Ro,DKU,DKT,coef_ric_l,coef_ric_s,xland1,  &
              pert_pbl, ens_random_seed, ens_pblamp,             &
#endif
                RBCR,HGAMQ,ALPHA)

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
#if (HWRF==1)
!Kwon For new shallow convection
        HPBL2D(i,j)=HPBL(i)
        rc2D(i,j)=rbcr(i)
#endif
!
        KPBL2D(i,j)=kpbl(i)
      ENDDO
! INITIALIZE DKU3D and DKT3D  (3D momentum and thermal diffusivity for
! diagnostics)
!
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


   END SUBROUTINE BL_GFS

!===================================================================
   SUBROUTINE gfsinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,           &
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


   END SUBROUTINE gfsinit

! --------------------------------------------------------------
!FPP$ NOCONCUR R
      SUBROUTINE MONINP(IX,IM,KM,ntrac,DV,DU,TAU,RTG,                   &
     &     U1,V1,T1,Q1,                                                 &
     &     PSK,RBSOIL,FM,FH,TSEA,QSS,HEAT,EVAP,STRESS,SPD1,KPBL,        &
     &     PRSI,DEL,PRSL,PRSLK,PHII,PHIL,RCL,DELTIM,                    &
     &     DUSFC,DVSFC,DTSFC,DQSFC,HPBL,HGAMT,                     &
#if (HWRF==1)
           VAR_RIC,Ro,DKU,DKT,coef_ric_l,coef_ric_s,xland1,   &
           pert_pbl, ens_random_seed, ens_pblamp,             &
#endif
           RBCR,HGAMQ,ALPHA)
!
      USE MODULE_GFS_MACHINE, ONLY : kind_phys
      USE MODULE_GFS_PHYSCONS, grav => con_g, RD => con_RD, CP => con_CP &
     &,             HVAP => con_HVAP, ROG => con_ROG, FV => con_FVirt

      implicit none
!
!     include 'constant.h'
!
!
!     Arguments
!
      integer IX, IM, KM, ntrac, KPBL(IM)
!
      real(kind=kind_phys) DELTIM
      real :: ALPHA

#if (HWRF==1)
      real :: VAR_RIC,coef_ric_l,coef_ric_s
      integer,intent(in) :: ens_random_seed
      real,intent(in) :: ens_pblamp
      logical,intent(in) :: pert_pbl
#endif
      real(kind=kind_phys) DV(IM,KM),     DU(IM,KM),                    &
     &                     TAU(IM,KM),    RTG(IM,KM,ntrac),             &
     &                     U1(IX,KM),     V1(IX,KM),                    &
     &                     T1(IX,KM),     Q1(IX,KM,ntrac),              &
     &                     PSK(IM),       RBSOIL(IM),                   &
!    &                     CD(IM),        CH(IM),                       &
     &                     FM(IM),        FH(IM),                       &
     &                     TSEA(IM),      QSS(IM),                      &
     &                                    SPD1(IM),                     &
!    &                     DPHI(IM),      SPD1(IM),                     &
     &                     PRSI(IX,KM+1), DEL(IX,KM),                   &
     &                     PRSL(IX,KM),   PRSLK(IX,KM),                 &
     &                     PHII(IX,KM+1), PHIL(IX,KM),                  & 
     &                     RCL(IM),       DUSFC(IM),                    &
     &                     dvsfc(IM),     dtsfc(IM),                    & 
     &                     DQSFC(IM),     HPBL(IM),                     &
     &                     HGAMT(IM),     hgamq(IM), RBCR(IM)
#if (HWRF==1)
real(kind=kind_phys) RO(IM),xland1(IM)
#endif
!
!    Locals
!
      integer i,iprt,is,iun,k,kk,kmpbl,lond
!     real(kind=kind_phys) betaq(IM), betat(IM),   betaw(IM),           &
      real(kind=kind_phys) evap(IM),  heat(IM),    phih(IM),            &
     &                     phim(IM),  rbdn(IM),    rbup(IM),            &
     &                     the1(IM),  stress(im),  beta(im),            &
     &                     the1v(IM), thekv(IM),   thermal(IM),         &
     &                     thesv(IM), ustar(IM),   wscale(IM)            
!    &                     thesv(IM), ustar(IM),   wscale(IM),  zl1(IM)
!
      real(kind=kind_phys) RDZT(IM,KM-1),                               &
     &                     ZI(IM,KM+1),     ZL(IM,KM),                  &
     &                     DKO(IM,KM-1),                                &
     &                     AL(IM,KM-1),     AD(IM,KM),                  &
     &                     AU(IM,KM-1),     A1(IM,KM),                  &
     &                     A2(IM,KM),       THETA(IM,KM),               &
     &                     AT(IM,KM*(ntrac-1)),DKU(IM,KM-1),DKT(IM,KM-1),WSPM(IM,KM-1) ! RGF added WSPM
      logical              pblflg(IM),   sfcflg(IM), stable(IM)
!
      real(kind=kind_phys) aphi16,  aphi5,  bet1,   bvf2,               &
     &                     cfac,    conq,   cont,   conw,               &
     &                     conwrc,  dk,     dkmax,  dkmin,              &
     &                     dq1,     dsdz2,  dsdzq,  dsdzt,              &
     &                     dsig,    dt,     dthe1,  dtodsd,             &
     &                     dtodsu,  dw2,    dw2min, g,                  &
     &                     gamcrq,  gamcrt, gocp,   gor, gravi,         &
     &                     hol,     pfac,   prmax,  prmin, prinv,       &
     &                     prnum,   qmin,   qtend,                      & 
     &                     rbint,   rdt,    rdz,    rdzt1,              &
     &                     ri,      rimin,  rl2,    rlam,               &
     &                     rone,   rzero,  sfcfrac,                     &
     &                     sflux,   shr2,   spdk2,  sri,                &
     &                     tem,     ti,     ttend,  tvd,                &
     &                     tvu,     utend,  vk,     vk2,                &
     &                     vpert,   vtend,  xkzo,   zfac,               &
     &                     zfmin,   zk,     tem1
      integer kLOC ! RGF
      real xDKU    ! RGF
!
      PARAMETER(g=grav)
      PARAMETER(GOR=G/RD,GOCP=G/CP)
      PARAMETER(CONT=1000.*CP/G,CONQ=1000.*HVAP/G,CONW=1000./G)
      PARAMETER(RLAM=150.,VK=0.4,VK2=VK*VK,PRMIN=1.0,PRMAX=4.)
      PARAMETER(DW2MIN=0.0001,DKMIN=1.0,DKMAX=1000.,RIMIN=-100.)
      PARAMETER(CFAC=7.8,PFAC=2.0,SFCFRAC=0.1)
      PARAMETER(QMIN=1.E-8,XKZO=1.0,ZFMIN=1.E-8,APHI5=5.,APHI16=16.)
!     PARAMETER(GAMCRT=3.,GAMCRQ=2.E-3)
      PARAMETER(GAMCRT=3.,GAMCRQ=0.)
      PARAMETER(RZERO=0.,RONE=1.)
      PARAMETER(IUN=84)
#if HWRF==1
      real*8 :: ran1           !zhang
      real :: rr 
      logical,save :: pert_pbl_local !zhang
      integer,save :: ens_random_seed_local !zhang
      real,save :: ens_pblamp_local   !zhang
      data ens_random_seed_local/0/
!zz      print*, 'zhang in pbl==========='
      if ( ens_random_seed_local .eq. 0 ) then
         pert_pbl_local=pert_pbl
         ens_random_seed_local=ens_random_seed
         ens_pblamp_local=ens_pblamp
!zz      print*, "zhang in pbl= one time ", pert_pbl_local, ens_random_seed_local, ens_pblamp_local
      endif
!zz      print*, "zhang in pbl=",pert_pbl_local, ens_random_seed_local, ens_pblamp_local
#endif
!
!
!-----------------------------------------------------------------------
!
 601  FORMAT(1X,' MONINP LAT LON STEP HOUR ',3I6,F6.1)
 602      FORMAT(1X,'    K','        Z','        T','       TH',        &
     &     '      TVH','        Q','        U','        V',             &
     &     '       SP')
 603      FORMAT(1X,I5,8F9.1)
 604      FORMAT(1X,'  SFC',9X,F9.1,18X,F9.1)
 605      FORMAT(1X,'    K      ZL    SPD2   THEKV   THE1V'             &
     &         ,' THERMAL    RBUP')
 606      FORMAT(1X,I5,6F8.2)
 607      FORMAT(1X,' KPBL    HPBL      FM      FH   HGAMT',            &
     &         '   HGAMQ      WS   USTAR      CD      CH')
 608      FORMAT(1X,I5,9F8.2)
 609      FORMAT(1X,' K PR DKT DKU ',I5,3F8.2)
 610      FORMAT(1X,' K PR DKT DKU ',I5,3F8.2,' L2 RI T2',              & 
     &         ' SR2  ',2F8.2,2E10.2)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     COMPUTE PRELIMINARY VARIABLES
!

      if (IX .lt. im) stop
!
      IPRT = 0
      IF(IPRT.EQ.1) THEN
!!!   LATD = 0
      LOND = 0
      ELSE
!!!   LATD = 0
      LOND = 0
      ENDIF
!
! define critical Richardson number    by KWON: Vickers and Mahart(2004) J. Appl. Meteo.
!  coef_ric=0.16 originally but it may too small: controled by namelist=0.25
!  Land and Ocean points are treated differently
!  by Kwon
!
     do i=1,im
     RBCR(I) = 0.25
#if (HWRF==1)
     IF(var_ric.eq.1.) THEN             
      IF(xland1(i).eq.1)  RBCR(I) = coef_ric_l*(1.E-7*Ro(I))**(-0.18)
      IF(xland1(i).eq.2)  RBCR(I) = coef_ric_s*(1.E-7*Ro(I))**(-0.18)
!      write(0,*) 'xland1 coef_ric_l coef_ric_s ',xland1(i),coef_ric_l,coef_ric_s
     ENDIF
     IF(RBCR(I).GT.0.5) RBCR(I)=0.5    !set upper limit Suggsted by Han
#endif
     enddo
!
      gravi = 1.0 / grav
      DT    = 2. * DELTIM
      RDT   = 1. / DT
      KMPBL = KM / 2
!
      do k=1,km
        do i=1,im
          zi(i,k) = phii(i,k) * gravi
          zl(i,k) = phil(i,k) * gravi
        enddo
      enddo
!
      do k=1,kmpbl
        do i=1,im
          theta(i,k) = t1(i,k) * psk(i) / prslk(i,k)
        enddo
      enddo
!
      DO K = 1,KM-1
        DO I=1,IM
          RDZT(I,K) = GOR * PRSI(I,K+1) / (PRSL(I,K) - PRSL(I,K+1))
        ENDDO
      ENDDO
!
      DO I = 1,IM
         DUSFC(I) = 0.
         DVSFC(I) = 0.
         DTSFC(I) = 0.
         DQSFC(I) = 0.
         HGAMT(I) = 0.
         HGAMQ(I) = 0.
         WSCALE(I) = 0.
         KPBL(I) = 1
         HPBL(I) = ZI(I,2)
         PBLFLG(I) = .TRUE.
         SFCFLG(I) = .TRUE.
         IF(RBSOIL(I).GT.0.0) SFCFLG(I) = .FALSE.
      ENDDO
!!
      DO I=1,IM
         RDZT1    = GOR * prSL(i,1) / DEL(i,1)
!        BET1     = DT*RDZT1*SPD1(I)/T1(I,1)
         BETA(I)  = DT*RDZT1/T1(I,1)
!        BETAW(I) = BET1*CD(I)
!        BETAT(I) = BET1*CH(I)
!        BETAQ(I) = DPHI(I)*BETAT(I)
      ENDDO
!
      DO I=1,IM
!        ZL1(i) = 0.-(T1(I,1)+TSEA(I))/2.*LOG(PRSL(I,1)/PRSI(I,1))*ROG
!        USTAR(I) = SQRT(CD(I)*SPD1(I)**2)
         USTAR(I) = SQRT(STRESS(I))
      ENDDO
!
      DO I=1,IM
         THESV(I)   = TSEA(I)*(1.+FV*MAX(QSS(I),QMIN))
         THE1(I)    = THETA(I,1)
         THE1V(I)   = THE1(I)*(1.+FV*MAX(Q1(I,1,1),QMIN))
         THERMAL(I) = THE1V(I)
!        DTHE1      = (THE1(I)-TSEA(I))
!        DQ1        = (MAX(Q1(I,1,1),QMIN) - MAX(QSS(I),QMIN))
!        HEAT(I)    = -CH(I)*SPD1(I)*DTHE1
!        EVAP(I)    = -CH(I)*SPD1(I)*DQ1
      ENDDO
!
!
!     COMPUTE THE FIRST GUESS OF PBL HEIGHT
!
      DO I=1,IM
         STABLE(I) = .FALSE.
!        ZL(i,1) = ZL1(i)
         RBUP(I) = RBSOIL(I)
      ENDDO
      DO K = 2, KMPBL
        DO I = 1, IM
          IF(.NOT.STABLE(I)) THEN
             RBDN(I)   = RBUP(I)
!            ZL(I,k)   = ZL(I,K-1) - (T1(i,k)+T1(i,K-1))/2 *
!    &                   LOG(PRSL(I,K)/PRSL(I,K-1)) * ROG
             THEKV(I)  = THETA(i,k)*(1.+FV*MAX(Q1(i,k,1),QMIN))
             SPDK2 = MAX(RCL(i)*(U1(i,k)*U1(i,k)+V1(i,k)*V1(i,k)),RONE)
             RBUP(I)   = (THEKV(I)-THE1V(I))*(G*ZL(I,k)/THE1V(I))/SPDK2
             KPBL(I)   = K
             STABLE(I) = RBUP(I).GT.RBCR(I)
          ENDIF
        ENDDO
      ENDDO
!
      DO I = 1,IM
         K = KPBL(I)
         IF(RBDN(I).GE.RBCR(I)) THEN
            RBINT = 0.
         ELSEIF(RBUP(I).LE.RBCR(I)) THEN
            RBINT = 1.
         ELSE
            RBINT = (RBCR(I)-RBDN(I))/(RBUP(I)-RBDN(I))
         ENDIF
         HPBL(I) = ZL(I,K-1) + RBINT*(ZL(I,K)-ZL(I,K-1))
         IF(HPBL(I).LT.ZI(I,KPBL(I))) KPBL(I) = KPBL(I) - 1
      ENDDO
!!
      DO I=1,IM
           HOL = MAX(RBSOIL(I)*FM(I)*FM(I)/FH(I),RIMIN)
           IF(SFCFLG(I)) THEN
              HOL = MIN(HOL,-ZFMIN)
           ELSE
              HOL = MAX(HOL,ZFMIN)
           ENDIF
!
!          HOL = HOL*HPBL(I)/ZL1(I)*SFCFRAC
           HOL = HOL*HPBL(I)/ZL(I,1)*SFCFRAC
           IF(SFCFLG(I)) THEN
!             PHIM = (1.-APHI16*HOL)**(-1./4.)
!             PHIH = (1.-APHI16*HOL)**(-1./2.)
              TEM  = 1.0 / (1. - APHI16*HOL)
              PHIH(I) = SQRT(TEM)
              PHIM(I) = SQRT(PHIH(I))
           ELSE
              PHIM(I) = (1.+APHI5*HOL)
              PHIH(I) = PHIM(I)
           ENDIF
           WSCALE(I) = USTAR(I)/PHIM(I)
           WSCALE(I) = MIN(WSCALE(I),USTAR(I)*APHI16)
           WSCALE(I) = MAX(WSCALE(I),USTAR(I)/APHI5)
      ENDDO
!
!     COMPUTE THE SURFACE VARIABLES FOR PBL HEIGHT ESTIMATION
!     UNDER UNSTABLE CONDITIONS
!
      DO I = 1,IM
         SFLUX  = HEAT(I) + EVAP(I)*FV*THE1(I)
         IF(SFCFLG(I).AND.SFLUX.GT.0.0) THEN
           HGAMT(I)   = MIN(CFAC*HEAT(I)/WSCALE(I),GAMCRT)
           HGAMQ(I)   = MIN(CFAC*EVAP(I)/WSCALE(I),GAMCRQ)
           VPERT      = HGAMT(I) + FV*THE1(I)*HGAMQ(I)
           VPERT      = MIN(VPERT,GAMCRT)
           THERMAL(I) = THERMAL(I) + MAX(VPERT,RZERO)
           HGAMT(I)   = MAX(HGAMT(I),RZERO)
           HGAMQ(I)   = MAX(HGAMQ(I),RZERO)
         ELSE
           PBLFLG(I) = .FALSE.
         ENDIF
      ENDDO
!
      DO I = 1,IM
         IF(PBLFLG(I)) THEN
            KPBL(I) = 1
            HPBL(I) = ZI(I,2)
         ENDIF
      ENDDO
!
!     ENHANCE THE PBL HEIGHT BY CONSIDERING THE THERMAL
!
      DO I = 1, IM
         IF(PBLFLG(I)) THEN
            STABLE(I) = .FALSE.
            RBUP(I) = RBSOIL(I)
         ENDIF
      ENDDO
      DO K = 2, KMPBL
        DO I = 1, IM
          IF(.NOT.STABLE(I).AND.PBLFLG(I)) THEN
            RBDN(I)   = RBUP(I)
!           ZL(I,k)   = ZL(I,K-1) - (T1(i,k)+T1(i,K-1))/2 *
!    &                  LOG(PRSL(I,K)/PRSL(I,K-1)) * ROG
            THEKV(I)  = THETA(i,k)*(1.+FV*MAX(Q1(i,k,1),QMIN))
            SPDK2   = MAX(RCL(i)*(U1(i,k)*U1(i,k)+V1(i,k)*V1(i,k)),RONE)
            RBUP(I)   = (THEKV(I)-THERMAL(I))*(G*ZL(I,k)/THE1V(I))/SPDK2
            KPBL(I)   = K
            STABLE(I) = RBUP(I).GT.RBCR(I)
          ENDIF
        ENDDO
      ENDDO
!
      DO I = 1,IM
         IF(PBLFLG(I)) THEN
            K = KPBL(I)
            IF(RBDN(I).GE.RBCR(I)) THEN
               RBINT = 0.
            ELSEIF(RBUP(I).LE.RBCR(I)) THEN
               RBINT = 1.
            ELSE
               RBINT = (RBCR(I)-RBDN(I))/(RBUP(I)-RBDN(I))
            ENDIF
            HPBL(I) = ZL(I,K-1) + RBINT*(ZL(I,k)-ZL(I,K-1))
#if (HWRF==1)
!zhang adding PBL perturtion
            if ( pert_pbl_local ) then
            rr=(2.0*ens_pblamp_local*ran1(ens_random_seed_local)-ens_pblamp_local)
            print*, "zhang inside the loop", rr, ens_pblamp_local,ens_random_seed_local 
            HPBL(I) = HPBL(I)*(1.0+rr)
            endif
#endif
            IF(HPBL(I).LT.ZI(I,KPBL(I))) KPBL(I) = KPBL(I) - 1
            IF(KPBL(I).LE.1) PBLFLG(I) = .FALSE.
         ENDIF
      ENDDO
!!
!
!     COMPUTE DIFFUSION COEFFICIENTS BELOW PBL
!

#if (HWRF==1)
! -------------------------------------------------------------------------------------
! begin RGF modifications
! this is version MOD05


! RGF determine wspd at roughly 500 m above surface, or as close as possible, reuse SPDK2
!  zi(i,k) is AGL, right?  May not matter if applied only to water grid points
      if(ALPHA.lt.0)then

       DO I=1,IM
         SPDK2 = 0.
         WSPM(i,1) = 0.
         DO K = 1, KMPBL ! kmpbl is like a max possible pbl height
          if(zi(i,k).le.500.and.zi(i,k+1).gt.500.)then ! find level bracketing 500 m
           SPDK2 = SQRT(U1(i,k)*U1(i,k)+V1(i,k)*V1(i,k)) ! wspd near 500 m
           WSPM(i,1) = SPDK2/0.6  ! now the Km limit for 500 m.  just store in K=1
           WSPM(i,2) = float(k)  ! height of level at gridpoint i. store in K=2
!           if(i.eq.25) print *,' IK ',i,k,' ZI ',zi(i,k), ' WSPM1 ',wspm(i,1),' KMPBL ',kmpbl,' KPBL ',kpbl(i)
          endif
         ENDDO
       ENDDO ! i

      endif ! ALPHA < 0
#endif


      DO I=1,IM			! RGF SWAPPED ORDER.  TESTED - NO IMPACT
         DO K = 1, KMPBL



! First guess at DKU.  If alpha >= 0, this is the only loop executed

            IF(KPBL(I).GT.K) THEN ! first guess DKU, this is original loop
               PRINV = 1.0 / (PHIH(I)/PHIM(I)+CFAC*VK*.1)
               PRINV = MIN(PRINV,PRMAX)
               PRINV = MAX(PRINV,PRMIN)
!              ZFAC = MAX((1.-(ZI(I,K+1)-ZL1(I))/                       &
!    &                (HPBL(I)-ZL1(I))), ZFMIN)
               ZFAC = MAX((1.-(ZI(I,K+1)-ZL(I,1))/                      &
     &                (HPBL(I)-ZL(I,1))), ZFMIN)
!
!              alpha factor (0-1.0) is multiplied to profile function to reduce the effect
!              height of the Hurricane Boundary Layer. This is gopal's doing
!


               DKU(i,k) = XKZO + WSCALE(I)*VK*ZI(I,K+1)                 &
     &                         *ABS(ALPHA)* ZFAC**PFAC 
! if alpha = -1, the above provides the first guess for DKU, based on assumption alpha = +1
! 		(other values of alpha < 0 can also be applied)
! if alpha > 0, the above applies the alpha suppression factor and we are finished
!               if(i.eq.25) print *,' I25 K ',k,' ORIG DKU ',dku(i,k)
               DKT(i,k) = DKU(i,k)*PRINV
               DKO(i,k) = (DKU(i,k)-XKZO)*PRINV
               DKU(i,k) = MIN(DKU(i,k),DKMAX)
               DKU(i,k) = MAX(DKU(i,k),DKMIN)
               DKT(i,k) = MIN(DKT(i,k),DKMAX)
               DKT(i,k) = MAX(DKT(i,k),DKMIN)
               DKO(i,k) = MAX(RZERO, MIN(DKMAX, DKO(i,k)))
              endif ! KPBL
        ENDDO ! K


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

! (3) only do  this IF KPBL(I) >= kLOC.  Otherwise, we are finished, with DKU as if alpha = +1

          if(KPBL(I).gt.kLOC)then

            xDKU = DKU(i,kLOC)     ! Km at k-level

! (4) DKU check.
! WSPM(i,1) is the KM cap for the 500-m level. 
!  if DKU at 500-m level < WSPM(i,1), do not limit Km ANYWHERE.  Alpha = abs(alpha).  No need to recalc.
!  if DKU at 500-m level > WSPM(i,1), then alpha = WSPM(i,1)/xDKU for entire column
            if(xDKU.ge.WSPM(i,1)) then ! ONLY if DKU at 500-m exceeds cap, otherwise already done


            WSPM(i,3) = WSPM(i,1)/xDKU  ! ratio of cap to Km at k-level, store in WSPM(i,3)
            WSPM(i,4) = amin1(WSPM(I,3),1.0) ! this is new column alpha. cap at 1. ! should never be needed

!    if(i.eq.25) print *,' I25 kLOC ',kLOC,' xDKU ',xDKU,' WSPM1 ',WSPM(i,1),' WSPM3 ',WSPM(i,3),' WSPM4 ',WSPM(i,4)

            DO K = 1, KMPBL ! now go through K loop again
             IF(KPBL(I).GT.K) THEN
               PRINV = 1.0 / (PHIH(I)/PHIM(I)+CFAC*VK*.1)
               PRINV = MIN(PRINV,PRMAX)
               PRINV = MAX(PRINV,PRMIN)
!              ZFAC = MAX((1.-(ZI(I,K+1)-ZL1(I))/                       &
!    &                (HPBL(I)-ZL1(I))), ZFMIN)
               ZFAC = MAX((1.-(ZI(I,K+1)-ZL(I,1))/                      &
     &                (HPBL(I)-ZL(I,1))), ZFMIN)

               DKU(i,k) = XKZO + WSCALE(I)*VK*ZI(I,K+1)                 &
     &                         *WSPM(i,4)* ZFAC**PFAC ! recalculated DKU using column alpha
!               if(i.eq.25) print *,' I25 K ',k,' DKU AFTER ',dku(i,k)

            
               DKT(i,k) = DKU(i,k)*PRINV
               DKO(i,k) = (DKU(i,k)-XKZO)*PRINV
               DKU(i,k) = MIN(DKU(i,k),DKMAX)
               DKU(i,k) = MAX(DKU(i,k),DKMIN)
               DKT(i,k) = MIN(DKT(i,k),DKMAX)
               DKT(i,k) = MAX(DKT(i,k),DKMIN)
               DKO(i,k) = MAX(RZERO, MIN(DKMAX, DKO(i,k)))
            ENDIF ! KPBL
          ENDDO ! DO K (RGF ALTERED)
         endif ! xDKU.ge.WSPM(i,1)
         endif ! KPBL(I).ge.kLOC
        endif ! alpha < 0
        endif ! xland1 = 2
#endif
      ENDDO ! DO I

! end RGF modifications
! -------------------------------------------------------------------------------------

!
!     COMPUTE DIFFUSION COEFFICIENTS OVER PBL (FREE ATMOSPHERE)
!
      DO K = 1, KM-1
         DO I=1,IM
            IF(K.GE.KPBL(I)) THEN
!              TI   = 0.5*(T1(i,k)+T1(i,K+1))
               TI   = 2.0 / (T1(i,k)+T1(i,K+1))
!              RDZ  = RDZT(I,K)/TI
               RDZ  = RDZT(I,K) * TI
!              RDZ  = RDZT(I,K)
               DW2  = RCL(i)*((U1(i,k)-U1(i,K+1))**2                    &
     &                      + (V1(i,k)-V1(i,K+1))**2)
               SHR2 = MAX(DW2,DW2MIN)*RDZ**2
               TVD  = T1(i,k)*(1.+FV*MAX(Q1(i,k,1),QMIN))
               TVU  = T1(i,K+1)*(1.+FV*MAX(Q1(i,K+1,1),QMIN))
!              BVF2 = G*(GOCP+RDZ*(TVU-TVD))/TI
               BVF2 = G*(GOCP+RDZ*(TVU-TVD)) * TI
               RI   = MAX(BVF2/SHR2,RIMIN)
               ZK   = VK*ZI(I,K+1)
!              RL2  = (ZK*RLAM/(RLAM+ZK))**2
!              DK   = RL2*SQRT(SHR2)
               RL2  = ZK*RLAM/(RLAM+ZK)
               DK   = RL2*RL2*SQRT(SHR2)
               IF(RI.LT.0.) THEN ! UNSTABLE REGIME
                  SRI = SQRT(-RI)
                  DKU(i,k) = XKZO + DK*(1+8.*(-RI)/(1+1.746*SRI))
!                 DKT(i,k) = XKZO + DK*(1+8.*(-RI)/(1+1.286*SRI))
                  tem      =        DK*(1+8.*(-RI)/(1+1.286*SRI))
                  DKT(i,k) = XKZO + tem
                  DKO(i,k) =        tem
               ELSE             ! STABLE REGIME
!                 DKT(i,k)  = XKZO + DK/(1+5.*RI)**2
                  tem       =        DK/(1+5.*RI)**2
                  DKT(i,k)  = XKZO + tem
                  DKO(i,k)  =        tem
                  PRNUM     = 1.0 + 2.1*RI
                  PRNUM     = MIN(PRNUM,PRMAX)
                  DKU(i,k)  = (DKT(i,k)-XKZO)*PRNUM + XKZO
               ENDIF
!
               DKU(i,k) = MIN(DKU(i,k),DKMAX)
               DKU(i,k) = MAX(DKU(i,k),DKMIN)
               DKT(i,k) = MIN(DKT(i,k),DKMAX)
               DKT(i,k) = MAX(DKT(i,k),DKMIN)
               DKO(i,k) = MAX(RZERO, MIN(DKMAX, DKO(i,k)))
!
!!!   IF(I.EQ.LOND.AND.LAT.EQ.LATD) THEN
!!!   PRNUM = DKU(k)/DKT(k)
!!!   WRITE(IUN,610) K,PRNUM,DKT(k),DKU(k),RL2,RI,
!!!   1              BVF2,SHR2
!!!   ENDIF
!
            ENDIF
         ENDDO
      ENDDO

!
!     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR HEAT AND MOISTURE
!
      DO I=1,IM
         AD(I,1) = 1.
         A1(I,1) = T1(i,1)   + BETA(i) * HEAT(I)
         A2(I,1) = Q1(i,1,1) + BETA(i) * EVAP(I)
!        A1(I,1) = T1(i,1)-BETAT(I)*(THETA(i,1)-TSEA(I))
!        A2(I,1) = Q1(i,1,1)-BETAQ(I)*
!    &           (MAX(Q1(i,1,1),QMIN)-MAX(QSS(I),QMIN))
      ENDDO
!
      DO K = 1,KM-1
        DO I = 1,IM
          DTODSD = DT/DEL(I,K)
          DTODSU = DT/DEL(I,K+1)
          DSIG   = PRSL(I,K)-PRSL(I,K+1)
          RDZ    = RDZT(I,K)*2./(T1(i,k)+T1(i,K+1))
!         RDZ    = RDZT(I,K)
          tem1   = DSIG * DKT(i,k) * RDZ
          IF(PBLFLG(I).AND.K.LT.KPBL(I)) THEN
!            DSDZT = DSIG*DKT(i,k)*RDZ*(GOCP-HGAMT(I)/HPBL(I))
!            DSDZQ = DSIG*DKT(i,k)*RDZ*(-HGAMQ(I)/HPBL(I))
             tem   = 1.0 / HPBL(I)
             DSDZT = tem1 * (GOCP-HGAMT(I)*tem)
             DSDZQ = tem1 * (-HGAMQ(I)*tem)
             A2(I,k)   = A2(I,k)+DTODSD*DSDZQ
             A2(I,k+1) = Q1(i,k+1,1)-DTODSU*DSDZQ
          ELSE
!            DSDZT = DSIG*DKT(i,k)*RDZ*(GOCP)
             DSDZT = tem1 * GOCP
             A2(I,k+1) = Q1(i,k+1,1)
          ENDIF
!         DSDZ2 = DSIG*DKT(i,k)*RDZ*RDZ
          DSDZ2     = tem1 * RDZ
          AU(I,k)   = -DTODSD*DSDZ2
          AL(I,k)   = -DTODSU*DSDZ2
          AD(I,k)   = AD(I,k)-AU(I,k)
          AD(I,k+1) = 1.-AL(I,k)
          A1(I,k)   = A1(I,k)+DTODSD*DSDZT
          A1(I,k+1) = T1(i,k+1)-DTODSU*DSDZT
        ENDDO
      ENDDO
!
!     SOLVE TRIDIAGONAL PROBLEM FOR HEAT AND MOISTURE
!
      CALL TRIDIN(IM,KM,1,AL,AD,AU,A1,A2,AU,A1,A2)
!
!     RECOVER TENDENCIES OF HEAT AND MOISTURE
!
      DO  K = 1,KM
         DO I = 1,IM
            TTEND      = (A1(I,k)-T1(i,k))*RDT
            QTEND      = (A2(I,k)-Q1(i,k,1))*RDT
            TAU(i,k)   = TAU(i,k)+TTEND
            RTG(I,k,1) = RTG(i,k,1)+QTEND
            DTSFC(I)   = DTSFC(I)+CONT*DEL(I,K)*TTEND
            DQSFC(I)   = DQSFC(I)+CONQ*DEL(I,K)*QTEND
         ENDDO
      ENDDO
!
!     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR MOMENTUM
!
      DO I=1,IM
!        AD(I,1) = 1.+BETAW(I)
         AD(I,1) = 1.0 + BETA(i) * STRESS(I) / SPD1(I)
         A1(I,1) = U1(i,1)
         A2(I,1) = V1(i,1)
!        AD(I,1) = 1.0
!        tem     = 1.0 + BETA(I) * STRESS(I) / SPD1(I)
!        A1(I,1) = U1(i,1) * tem
!        A2(I,1) = V1(i,1) * tem
      ENDDO
!
      DO K = 1,KM-1
        DO I=1,IM
          DTODSD    = DT/DEL(I,K)
          DTODSU    = DT/DEL(I,K+1)
          DSIG      = PRSL(I,K)-PRSL(I,K+1)
          RDZ       = RDZT(I,K)*2./(T1(i,k)+T1(i,k+1))
!         RDZ       = RDZT(I,K)
          DSDZ2     = DSIG*DKU(i,k)*RDZ*RDZ
          AU(I,k)   = -DTODSD*DSDZ2
          AL(I,k)   = -DTODSU*DSDZ2
          AD(I,k)   = AD(I,k)-AU(I,k)
          AD(I,k+1) = 1.-AL(I,k)
          A1(I,k+1) = U1(i,k+1)
          A2(I,k+1) = V1(i,k+1)
        ENDDO
      ENDDO
!
!     SOLVE TRIDIAGONAL PROBLEM FOR MOMENTUM
!
      CALL TRIDI2(IM,KM,AL,AD,AU,A1,A2,AU,A1,A2)
!
!     RECOVER TENDENCIES OF MOMENTUM
!
      DO K = 1,KM
         DO I = 1,IM
            CONWRC = CONW*SQRT(RCL(i))
            UTEND = (A1(I,k)-U1(i,k))*RDT
            VTEND = (A2(I,k)-V1(i,k))*RDT
            DU(i,k)  = DU(i,k)+UTEND
            DV(i,k)  = DV(i,k)+VTEND
            DUSFC(I) = DUSFC(I)+CONWRC*DEL(I,K)*UTEND
            DVSFC(I) = DVSFC(I)+CONWRC*DEL(I,K)*VTEND
         ENDDO
      ENDDO
!!
!
!     COMPUTE TRIDIAGONAL MATRIX ELEMENTS FOR TRACERS
!
      if (ntrac .ge. 2) then
        DO I=1,IM
         AD(I,1) = 1.
        ENDDO
        do k = 2, ntrac
          is = (k-2) * km
          do i = 1, im
            AT(I,1+is) = Q1(i,1,k)
          enddo
        enddo
!
        DO K = 1,KM-1
          DO I = 1,IM
            DTODSD = DT/DEL(I,K)
            DTODSU = DT/DEL(I,K+1)
            DSIG   = PRSL(I,K)-PRSL(I,K+1)
            RDZ    = RDZT(I,K)*2./(T1(i,k)+T1(i,K+1))
            tem1   = DSIG * DKT(i,k) * RDZ
            DSDZ2     = tem1 * RDZ
            AU(I,k)   = -DTODSD*DSDZ2
            AL(I,k)   = -DTODSU*DSDZ2
            AD(I,k)   = AD(I,k)-AU(I,k)
            AD(I,k+1) = 1.-AL(I,k)
          ENDDO
        ENDDO
        do kk = 2, ntrac
          is = (kk-2) * km
          do k = 1, km - 1
            do i = 1, im
              AT(I,k+1+is) = Q1(i,k+1,kk)
            enddo
          enddo
        enddo
!
!     SOLVE TRIDIAGONAL PROBLEM FOR TRACERS
!
        CALL TRIDIT(IM,KM,ntrac-1,AL,AD,AU,AT,AU,AT)
!
!     RECOVER TENDENCIES OF TRACERS
!
        do kk = 2, ntrac
          is = (kk-2) * km
          do k = 1, km 
            do i = 1, im
              QTEND = (AT(I,K+is)-Q1(i,K,kk))*RDT
              RTG(i,K,kk) = RTG(i,K,kk) + QTEND
            enddo
          enddo
        enddo
      endif
!!
      RETURN
      END SUBROUTINE MONINP
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

      END MODULE module_bl_gfs
