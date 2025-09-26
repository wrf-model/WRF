!-----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: HORIZONTAL DIFFUSION
!
!-----------------------------------------------------------------------
!
#include "nmm_loop_basemacros.h"
#include "nmm_loop_macros.h"
!
!-----------------------------------------------------------------------
!
      MODULE MODULE_DIFFUSION_NMM
!
!-----------------------------------------------------------------------
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_CONFIGURE,             ONLY : GRID_CONFIG_REC_TYPE
      USE MODULE_STATE_DESCRIPTION
!-----------------------------------------------------------------------
!
      LOGICAL :: SECOND=.TRUE.
      INTEGER :: KSMUD=1
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE HDIFF(NTSD,DT,FIS,DY,HDAC,HDACV                        &
     &                ,HBM2,DETA1,SIGMA                                 &
#if ( HWRF == 1 )
     &                ,T,Q,U,V,Q2,Z,W,SM,SICE,h_diff                    &
#else
     &                ,T,Q,U,V,Q2,Z,W,SM,SICE                           &
#endif
     &                ,DEF3D                                            &
     &                ,IHE,IHW,IVE,IVW                                  &
     &                ,CONFIG_FLAGS                                     &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    HDIFF       HORIZONTAL DIFFUSION
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-11-17
!     
! ABSTRACT:
!     HDIFF CALCULATES THE CONTRIBUTION OF THE HORIZONTAL DIFFUSION
!     TO THE TENDENCIES OF TEMPERATURE, SPECIFIC HUMIDITY, WIND
!     COMPONENTS, AND TURBULENT KINETIC ENERGY AND THEN UPDATES THOSE
!     VARIABLES.  A SECOND-ORDER NONLINEAR SCHEME SIMILAR TO
!     SMAGORINSKY'S IS USED WHERE THE DIFFUSION COEFFICIENT IS
!     A FUNCTION OF THE DEFORMATION FIELD AND OF THE TURBULENT
!     KINETIC ENERGY.
!     
! PROGRAM HISTORY LOG:
!   87-06-??  JANJIC     - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   96-03-28  BLACK      - ADDED EXTERNAL EDGE
!   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
!   02-02-07  BLACK      - CONVERTED TO WRF STRUCTURE
!   02-08-29  MICHALAKES -
!   02-09-06  WOLFE      -
!   03-05-27  JANJIC     - ADDED SLOPE ADJUSTMENT
!   04-11-18  BLACK      - THREADED
!   05-12-12  BLACK      - CONVERTED FROM IKJ TO IJK
!   06-08-15  JANJIC     - ENHANCEMENT AT SLOPING SEA COAST
!     
! USAGE: CALL HDIFF FROM SUBROUTINE SOLVE_RUNSTREAM
!
!   INPUT ARGUMENT LIST:
!  
!   OUTPUT ARGUMENT LIST: 
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!  
!     UNIQUE: NONE
!  
!     LIBRARY: NONE
!  
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$  
!***********************************************************************
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!***  STRUCTURE THAT CONTAINS RUN-TIME CONFIGURATION (NAMELIST) DATA FOR DOMAIN
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: NTSD
!
      REAL,INTENT(IN) :: DT,DY
#if ( HWRF == 1 )
      REAL,INTENT(IN) :: H_DIFF  
#endif
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: DETA1
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,HBM2            &
     &                                             ,HDAC,HDACV          &
     &                                             ,SM,SICE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(OUT) :: DEF3D          
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: W,Z
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: T,Q,Q2   &
     &                                                        ,U,V
!
      INTEGER, DIMENSION(JMS:JME), INTENT(IN) :: IHE,IHW,IVE,IVW
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: SIGMA
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,J,K,KS
!
      REAL :: DEF_IJ,DEFSK,DEFTK,HKNE_IJ,HKSE_IJ,Q2L,RDY,SLOP,SLOPHC    &
     &       ,UTK,VKNE_IJ,VKSE_IJ,VTK,DEF1,DEF2,DEF3,DEF4
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: DEF,HKNE,HKSE          &
     &                                          ,Q2DIF,Q2NE,Q2SE        &
     &                                          ,QDIF,QNE,QSE,SNE,SSE   &
     &                                          ,TDIF,TNE,TSE           &
     &                                          ,UDIF,UNE,USE           &
     &                                          ,VDIF,VKNE,VKSE,VNE,VSE
!
      LOGICAL :: CILINE,WATSLOP
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
#if ( HWRF == 1 )
      SLOPHC=SLOPHT*SQRT(2.)*0.5*9.
#else
      SLOPHC=config_flags%slophc
#endif
      RDY=1./DY
!
      DO J=JTS-5,JTE+5
      DO I=ITS-5,ITE+5
        DEF(I,J)=0.
        TNE(I,J)=0.
        QNE(I,J)=0.
        Q2NE(I,J)=0.
        HKNE(I,J)=0.
        UNE(I,J)=0.
        VNE(I,J)=0.
        VKNE(I,J)=0.
        TSE(I,J)=0.
        QSE(I,J)=0.
        Q2SE(I,J)=0.
        HKSE(I,J)=0.
        USE(I,J)=0.
        VSE(I,J)=0.
        VKSE(I,J)=0.
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***
!***  DIFFUSING Q2 AT GROUND LEVEL DOES NOT MATTER
!***  BECAUSE USTAR2 IS RECALCULATED.
!***
!-----------------------------------------------------------------------
!***  ITERATION LOOP
!-----------------------------------------------------------------------
!
      DO 600 KS=1,KSMUD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  MAIN INTEGRATION LOOP
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(def1,def2,def3,def4,def_ij,defsk,deftk,hkne_ij,hkse_ij   &
!$omp&        ,i,j,k,q2dif,q2ne,q2se,qdif,qne,qse,slop,sne,sse          &
!$omp&        ,tdif,tne,tse,udif,une,use,vdif,vkne,vkne_ij              &
!$omp&        ,vkse,vkse_ij,vne,vse)
!-----------------------------------------------------------------------
!
      main_integration : DO K=KTS,KTE

      DO J=JMS,JME
      DO I=IMS,IME
        DEF3D(I,J,K)=0.
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  SLOPE SWITCHES FOR MOISTURE
!-----------------------------------------------------------------------
!
        IF(SIGMA==1)THEN
!
!-----------------------------------------------------------------------
!***  PRESSURE DOMAIN
!-----------------------------------------------------------------------
!
          IF(DETA1(K)>0.)THEN
            DO J=MYJS_P1,MYJE1_P2
            DO I=MYIS_P1,MYIE1_P1
              SNE(I,J)=1.
            ENDDO
            ENDDO
!
            DO J=MYJS1_P1,MYJE_P2
            DO I=MYIS_P1,MYIE1_P1
              SSE(I,J)=1.
            ENDDO
            ENDDO
!
!-----------------------------------------------------------------------
!***  SIGMA DOMAIN
!-----------------------------------------------------------------------
!
          ELSE
            DO J=MYJS_P1,MYJE1_P1
            DO I=MYIS_P1,MYIE1_P1
              SLOP=ABS((Z(I+IHE(J),J+1,K)-Z(I,J,K))*RDY)
!
              CILINE=((SM(I+IHE(J),J+1)/=SM(I,J)).OR.                   &
                      (SICE(I+IHE(J),J+1)/=SICE(I,J)))
!
              WATSLOP=(SM(I+IHE(J),J+1)==1.0.AND.                       &
                       SM(I,J)==1.0.AND.SLOP/=0.)
!
              IF(SLOP<SLOPHC.OR.CILINE.OR.WATSLOP)THEN
                SNE(I,J)=1.
              ELSE
                SNE(I,J)=0.
              ENDIF
            ENDDO
            ENDDO
!
            DO J=MYJS1_P1,MYJE_P1
            DO I=MYIS_P1,MYIE1_P1
              SLOP=ABS((Z(I+IHE(J),J-1,K)-Z(I,J,K))*RDY)
!
              CILINE=((SM(I+IHE(J),J-1)/=SM(I,J)).OR.                   &
                      (SICE(I+IHE(J),J-1)/=SICE(I,J)))
!
              WATSLOP=(SM(I+IHE(J),J-1)==1.0.AND.                       &
                       SM(I,J)==1.0.AND.SLOP/=0.)
!
              IF(SLOP<SLOPHC.OR.CILINE.OR.WATSLOP)THEN
                SSE(I,J)=1.
              ELSE
                SSE(I,J)=0.
              ENDIF
            ENDDO
            ENDDO
          ENDIF
!
        ENDIF
!-----------------------------------------------------------------------
!***  DEFORMATIONS
!-----------------------------------------------------------------------
!
        DO J=MYJS_P1,MYJE_P1
        DO I=MYIS_P1,MYIE_P1
!
          DEFTK=U(I+IHE(J),J,K)-U(I+IHW(J),J,K)                         &
     &         -V(I,J+1,K)+V(I,J-1,K)
          DEFSK=U(I,J+1,K)-U(I,J-1,K)                                   &
     &         +V(I+IHE(J),J,K)-V(I+IHW(J),J,K)
          DEF1=(W(I+IHW(J),J-1,K)-W(I,J,K))*0.5
          DEF2=(W(I+IHE(J),J-1,K)-W(I,J,K))*0.5
          DEF3=(W(I+IHW(J),J+1,K)-W(I,J,K))*0.5
          DEF4=(W(I+IHE(J),J+1,K)-W(I,J,K))*0.5
          Q2L=Q2(I,J,K)
          IF(Q2L<=EPSQ2)Q2L=0.
          IF ( CONFIG_FLAGS%BL_PBL_PHYSICS  == MYJPBLSCHEME) then
                DEF_IJ=DEFTK*DEFTK+DEFSK*DEFSK+DEF1*DEF1+DEF2*DEF2 &
    &             +DEF3*DEF3+DEF4*DEF4+SCQ2*Q2L
          else
                DEF_IJ=DEFTK*DEFTK+DEFSK*DEFSK+DEF1*DEF1+DEF2*DEF2 &
    &            +DEF3*DEF3+DEF4*DEF4
          ENDIF

          DEF_IJ=SQRT(DEF_IJ+DEF_IJ)*HBM2(I,J)
          DEF_IJ=MAX(DEF_IJ,DEFC)
          DEF_IJ=MIN(DEF_IJ,DEFM)
          DEF_IJ=DEF_IJ*0.1
          DEF(I,J)=DEF_IJ
          DEF3D(I,J,K)=DEF_IJ
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  DIAGONAL CONTRIBUTIONS
!-----------------------------------------------------------------------
!
        DO J=MYJS_P1,MYJE1_P1
        DO I=MYIS_P1,MYIE1_P1
          HKNE_IJ=(DEF(I,J)+DEF(I+IHE(J),J+1))*SNE(I,J)
          TNE (I,J)=(T (I+IHE(J),J+1,K)-T (I,J,K))*HKNE_IJ
          QNE (I,J)=(Q (I+IHE(J),J+1,K)-Q (I,J,K))*HKNE_IJ
          Q2NE(I,J)=(Q2(I+IHE(J),J+1,K)-Q2(I,J,K))*HKNE_IJ
          HKNE(I,J)=HKNE_IJ
!
          VKNE_IJ=DEF(I+IVE(J),J)+DEF(I,J+1)
          UNE(I,J)=(U(I+IVE(J),J+1,K)-U(I,J,K))*VKNE_IJ
          VNE(I,J)=(V(I+IVE(J),J+1,K)-V(I,J,K))*VKNE_IJ
          VKNE(I,J)=VKNE_IJ
        ENDDO
        ENDDO
!
        DO J=MYJS1_P1,MYJE_P1
        DO I=MYIS_P1,MYIE1_P1
          HKSE_IJ=(DEF(I+IHE(J),J-1)+DEF(I,J))*SSE(I,J)
          TSE (I,J)=(T (I+IHE(J),J-1,K)-T (I,J,K))*HKSE_IJ
          QSE (I,J)=(Q (I+IHE(J),J-1,K)-Q (I,J,K))*HKSE_IJ
          Q2SE(I,J)=(Q2(I+IHE(J),J-1,K)-Q2(I,J,K))*HKSE_IJ
          HKSE(I,J)=HKSE_IJ
!
          VKSE_IJ=DEF(I,J-1)+DEF(I+IVE(J),J)
          USE(I,J)=(U(I+IVE(J),J-1,K)-U(I,J,K))*VKSE_IJ
          VSE(I,J)=(V(I+IVE(J),J-1,K)-V(I,J,K))*VKSE_IJ
          VKSE(I,J)=VKSE_IJ
        ENDDO
        ENDDO
!-----------------------------------------------------------------------
!
        DO J=MYJS1,MYJE1
        DO I=MYIS1,MYIE
          TDIF (I,J)=(TNE (I,J)-TNE (I+IHW(J),J-1)                      &
     &               +TSE (I,J)-TSE (I+IHW(J),J+1))*HDAC(I,J)
          QDIF (I,J)=(QNE (I,J)-QNE (I+IHW(J),J-1)                      &
     &               +QSE (I,J)-QSE (I+IHW(J),J+1))*HDAC(I,J)*FCDIF
          Q2DIF(I,J)=(Q2NE(I,J)-Q2NE(I+IHW(J),J-1)                      &
     &               +Q2SE(I,J)-Q2SE(I+IHW(J),J+1))*HDAC(I,J)
!
          UDIF(I,J)=(UNE(I,J)-UNE(I+IVW(J),J-1)                         &
     &              +USE(I,J)-USE(I+IVW(J),J+1))*HDACV(I,J)
          VDIF(I,J)=(VNE(I,J)-VNE(I+IVW(J),J-1)                         &
     &              +VSE(I,J)-VSE(I+IVW(J),J+1))*HDACV(I,J)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  2ND ORDER DIFFUSION
!-----------------------------------------------------------------------
!
        IF(SECOND)THEN
          DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            T (I,J,K)=T (I,J,K)+TDIF (I,J)
            Q (I,J,K)=Q (I,J,K)+QDIF (I,J)
!
#if ( HWRF == 1 )
            U(I,J,K)=U(I,J,K)+UDIF(I,J)*h_diff
            V(I,J,K)=V(I,J,K)+VDIF(I,J)*h_diff
#else
            U(I,J,K)=U(I,J,K)+UDIF(I,J)
            V(I,J,K)=V(I,J,K)+VDIF(I,J)
#endif
          ENDDO
          ENDDO
!
!-----------------------------------------------------------------------
          IF(K>=KTS+1)THEN
            DO J=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              Q2(I,J,K)=Q2(I,J,K)+Q2DIF(I,J)
            ENDDO
            ENDDO
          ENDIF
!
!-----------------------------------------------------------------------
!***  4TH ORDER DIAGONAL CONTRIBUTIONS
!-----------------------------------------------------------------------
!
        ELSE
!
          DO J=MYJS,MYJE1
          DO I=MYIS,MYIE1
            HKNE_IJ=HKNE(I,J)
            TNE (I,J)=(TDIF (I+IHE(J),J+1)-TDIF (I,J))*HKNE_IJ
            QNE (I,J)=(QDIF (I+IHE(J),J+1)-QDIF (I,J))*HKNE_IJ
            Q2NE(I,J)=(Q2DIF(I+IHE(J),J+1)-Q2DIF(I,J))*HKNE_IJ
          ENDDO
          ENDDO
!
          DO J=MYJS1,MYJE
          DO I=MYIS,MYIE1
            HKSE_IJ=HKSE(I,J)
            TSE (I,J)=(TDIF (I+IHE(J),J-1)-TDIF (I,J))*HKSE_IJ
            QSE (I,J)=(QDIF (I+IHE(J),J-1)-QDIF (I,J))*HKSE_IJ
            Q2SE(I,J)=(Q2DIF(I+IHE(J),J-1)-Q2DIF(I,J))*HKSE_IJ
          ENDDO
          ENDDO
!
          DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            T(I,J,K)=T(I,J,K)-(TNE(I,J)-TNE(I+IHW(J),J-1)               &
     &                        +TSE(I,J)-TSE(I+IHW(J),J+1))*HDAC(I,J)
            Q(I,J,K)=Q(I,J,K)-(QNE(I,J)-QNE(I+IHW(J),J-1)               &
     &                        +QSE(I,J)-QSE(I+IHW(J),J+1))*HDAC(I,J)    &
     &                        *FCDIF
          ENDDO
          ENDDO
          
!
          IF(K>=KTS+1)THEN
            DO J=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              Q2(I,J,K)=Q2(I,J,K)-(Q2NE(I,J)-Q2NE(I+IHW(J),J-1)         &
     &                            +Q2SE(I,J)-Q2SE(I+IHW(J),J+1))        &
     &                            *HDAC(I,J)
            ENDDO
            ENDDO
          ENDIF
!
!-----------------------------------------------------------------------
!
          DO J=MYJS,MYJE1
          DO I=MYIS,MYIE1
            UNE(I,J)=(UDIF(I+IVE(J),J+1)-UDIF(I,J))*VKNE(I,J)
            VNE(I,J)=(VDIF(I+IVE(J),J+1)-VDIF(I,J))*VKNE(I,J)
          ENDDO
          ENDDO
!
          DO J=MYJS1,MYJE
          DO I=MYIS,MYIE1
            USE(I,J)=(UDIF(I+IVE(J),J-1)-UDIF(I,J))*VKSE(I,J)
            VSE(I,J)=(VDIF(I+IVE(J),J-1)-VDIF(I,J))*VKSE(I,J)
          ENDDO
          ENDDO
!
          DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
#if ( HWRF == 1 )
            U(I,J,K)=U(I,J,K)-(UNE(I,J)-UNE(I+IVW(J),J-1)               &
     &                        +USE(I,J)-USE(I+IVW(J),J+1))*HDACV(I,J)*h_diff
            V(I,J,K)=V(I,J,K)-(VNE(I,J)-VNE(I+IVW(J),J-1)               &
     &                        +VSE(I,J)-VSE(I+IVW(J),J+1))*HDACV(I,J)*h_diff
#else
            U(I,J,K)=U(I,J,K)-(UNE(I,J)-UNE(I+IVW(J),J-1)               &
     &                        +USE(I,J)-USE(I+IVW(J),J+1))*HDACV(I,J)
            V(I,J,K)=V(I,J,K)-(VNE(I,J)-VNE(I+IVW(J),J-1)               &
     &                        +VSE(I,J)-VSE(I+IVW(J),J+1))*HDACV(I,J)
#endif
          ENDDO
          ENDDO
!
!-----------------------------------------------------------------------
        ENDIF  ! End 4th order diffusion
!-----------------------------------------------------------------------
!
      ENDDO main_integration
!
!-----------------------------------------------------------------------
!
  600 CONTINUE
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE HDIFF
!
!-----------------------------------------------------------------------
!
      END MODULE MODULE_DIFFUSION_NMM
!
!-----------------------------------------------------------------------
