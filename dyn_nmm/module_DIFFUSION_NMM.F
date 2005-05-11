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
     &                ,HTM,HBM2,VTM,DETA1,SIGMA                         &
     &                ,T,Q,U,V,Q2,Z,W                                   &
     &                ,IHE,IHW,IVE,IVW,INDX3_WRK                        &
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
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,INTENT(IN) :: NTSD
!
      REAL,INTENT(IN) :: DT,DY
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: DETA1
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,HBM2            &
     &                                             ,HDAC,HDACV
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: HTM,VTM,Z,W
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: T,Q,Q2   &
     &                                                        ,U,V
!
      INTEGER, DIMENSION(JMS:JME), INTENT(IN) :: IHE,IHW,IVE,IVW
!
!-----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!***  NMM_MAX_DIM is set in configure.wrf and must agree with
!***  the value of dimspec q in the Registry/Registry.
!!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
!
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: SIGMA
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,J,J1_P1,J1_P2,J2_00,J2_M1,J2_P1,J3_00,J3_P1,J3_P2    &
     &          ,J4_00,J4_M1,J4_M2,J4_P1,J4_P2,JJ,JKNT,JSTART,K,KS
!
      REAL :: DEF_J,DEFSK,DEFTK,HKNE_J,HKSE_J,Q2L,RDY,SLOP,SLOPHC       &
     &       ,UTK,VKNE_J,VKSE_J,VTK,DEF1,DEF2,DEF3,DEF4
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE) :: SNE,SSE
!
!***  TYPE 1 WORKING ARRAY (SEE PFDHT)
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-2:2) :: DEF
!
!***  TYPE 2 WORKING ARRAY (SEE PFDHT)
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-2:1) :: HKNE,QNE,Q2NE,TNE     &
     &                                           ,UNE,VKNE,VNE
!
!***  TYPE 3 WORKING ARRAY (SEE PFDHT)
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-1:2) :: HKSE,QSE,Q2SE,TSE     &
     &                                           ,USE,VKSE,VSE
!
!***  TYPE 4 WORKING ARRAY (SEE PFDHT)
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-1:1) :: CKE,QDIF,Q2DIF        &
     &                                           ,TDIF,UDIF,VDIF
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      JSTART=MYJS2
!-----------------------------------------------------------------------
!
      SLOPHC=SLOPHT*SQRT(2.)*0.5
      RDY=1./DY
!
!-----------------------------------------------------------------------
!***
!***  DIFFUSING Q2 AT GROUND LEVEL DOES NOT MATTER
!***  BECAUSE USTAR2 IS RECALCULATED
!***
!-----------------------------------------------------------------------
!***  MARCH NORTHWARD THROUGH THE SOUTHERNMOST SLABS TO BEGIN
!***  FILLING THE MAIN WORKING ARRAYS WHICH ARE MULTI-DIMENSIONED
!***  IN J BECAUSE THEY ARE DIFFERENCED OR AVERAGED IN J.
!-----------------------------------------------------------------------
!
      DO J=-2,2
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        DEF(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=-2,1
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        TNE(I,K,J)=0.
        QNE(I,K,J)=0.
        Q2NE(I,K,J)=0.
        HKNE(I,K,J)=0.
        UNE(I,K,J)=0.
        VNE(I,K,J)=0.
        VKNE(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=-1,2
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        TSE(I,K,J)=0.
        QSE(I,K,J)=0.
        Q2SE(I,K,J)=0.
        HKSE(I,K,J)=0.
        USE(I,K,J)=0.
        VSE(I,K,J)=0.
        VKSE(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(def_j,def1,def2,def3,def4,defsk,deftk,i,j,jj,k,q2l)
      DO J=-2,1
        JJ=JSTART+J
!
        DO K=KTS,KTE

        DO I=MYIS_P1,MYIE_P1
          DEFTK=U(I+IHE(JJ),K,JJ)-U(I+IHW(JJ),K,JJ)                     &
     &         -V(I,K,JJ+1)+V(I,K,JJ-1)
          DEFSK=U(I,K,JJ+1)-U(I,K,JJ-1)                                 &
     &         +V(I+IHE(JJ),K,JJ)-V(I+IHW(JJ),K,JJ)
          Q2L=MAX(Q2(I,K,JJ),EPSQ2)
!
          DEF1=W(I+IHW(JJ),K,JJ-1)-W(I,K,JJ)
          DEF2=W(I+IHE(JJ),K,JJ-1)-W(I,K,JJ)
          DEF3=W(I+IHW(JJ),K,JJ+1)-W(I,K,JJ)
          DEF4=W(I+IHE(JJ),K,JJ+1)-W(I,K,JJ)
!
          DEF_J=DEFTK*DEFTK+DEFSK*DEFSK+DEF1*DEF1+DEF2*DEF2+            &
     &          DEF3*DEF3+DEF4*DEF4+SCQ2*Q2L
          DEF_J=SQRT(DEF_J+DEF_J)*HBM2(I,JJ)
          DEF_J=MAX(DEF_J,DEFC)
          DEF_J=MIN(DEF_J,DEFM)
          DEF_J=DEF_J*0.1
          DEF(I,K,J)=DEF_J
        ENDDO
        ENDDO
!
      ENDDO
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(hkne_j,i,j,jj,k,slop,sne,vkne_j)
      DO J=-2,0
        JJ=JSTART+J
!
!-----------------------------------------------------------------------
!***  SLOPE SWITCHES FOR MOISTURE
!-----------------------------------------------------------------------
!
        IF(SIGMA==1)THEN
          DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!***  PRESSURE DOMAIN
!-----------------------------------------------------------------------
!
            IF(DETA1(K)>0.)THEN
              DO I=MYIS_P1,MYIE1_P1
                SNE(I,K)=1.
              ENDDO
!
!-----------------------------------------------------------------------
!***  SIGMA DOMAIN
!-----------------------------------------------------------------------
!
            ELSE
              DO I=MYIS_P1,MYIE1_P1
                SLOP=ABS((Z(I+IHE(JJ),K,JJ+1)-Z(I,K,JJ))*RDY)
                IF(SLOP<SLOPHC)THEN
                  SNE(I,K)=1.
                ELSE
                  SNE(I,K)=0.
                ENDIF
!
              ENDDO
            ENDIF
!
          ENDDO
        ENDIF
!
        DO K=KTS,KTE
        DO I=MYIS_P1,MYIE1_P1
          HKNE_J=(DEF(I,K,J)+DEF(I+IHE(JJ),K,J+1))                      &
     &           *HTM(I,K,JJ)*HTM(I+IHE(JJ),K,JJ+1)*SNE(I,K)
          TNE (I,K,J)=(T (I+IHE(JJ),K,JJ+1)-T (I,K,JJ))*HKNE_J
          QNE (I,K,J)=(Q (I+IHE(JJ),K,JJ+1)-Q (I,K,JJ))*HKNE_J
          Q2NE(I,K,J)=(Q2(I+IHE(JJ),K,JJ+1)-Q2(I,K,JJ))*HKNE_J
          HKNE(I,K,J)=HKNE_J
!
          VKNE_J=(DEF(I+IVE(JJ),K,J)+DEF(I,K,J+1))                      &
     &           *VTM(I,K,JJ)*VTM(I+IVE(JJ),K,JJ+1)
          UNE(I,K,J)=(U(I+IVE(JJ),K,JJ+1)-U(I,K,JJ))*VKNE_J
          VNE(I,K,J)=(V(I+IVE(JJ),K,JJ+1)-V(I,K,JJ))*VKNE_J
          VKNE(I,K,J)=VKNE_J
        ENDDO
        ENDDO
!
      ENDDO
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(hkse_j,i,j,jj,k,slop,sse,vkse_j)
      DO J=-1,1
        JJ=JSTART+J
!
!-----------------------------------------------------------------------
!***  SLOPE SWITCHES FOR MOISTURE
!-----------------------------------------------------------------------
!
        IF(SIGMA==1)THEN
          DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!***  PRESSURE DOMAIN
!-----------------------------------------------------------------------
!
            IF(DETA1(K)>0.)THEN
              DO I=MYIS_P1,MYIE1_P1
                SSE(I,K)=1.
              ENDDO
!
!-----------------------------------------------------------------------
!***  SIGMA DOMAIN
!-----------------------------------------------------------------------
!
            ELSE
              DO I=MYIS_P1,MYIE1_P1
                SLOP=ABS((Z(I+IHE(JJ),K,JJ-1)-Z(I,K,JJ))*RDY)
                IF(SLOP<SLOPHC)THEN
                  SSE(I,K)=1.
                ELSE
                  SSE(I,K)=0.
                ENDIF
              ENDDO
            ENDIF
!
          ENDDO
        ENDIF
!
        DO K=KTS,KTE
        DO I=MYIS_P1,MYIE1_P1
          HKSE_J=(DEF(I+IHE(JJ),K,J-1)+DEF(I,K,J))                      &
     &           *HTM(I+IHE(JJ),K,JJ-1)*HTM(I,K,JJ)*SSE(I,K)
          TSE (I,K,J)=(T (I+IHE(JJ),K,JJ-1)-T (I,K,JJ))*HKSE_J
          QSE (I,K,J)=(Q (I+IHE(JJ),K,JJ-1)-Q (I,K,JJ))*HKSE_J
          Q2SE(I,K,J)=(Q2(I+IHE(JJ),K,JJ-1)-Q2(I,K,JJ))*HKSE_J
          HKSE(I,K,J)=HKSE_J
!
          VKSE_J=(DEF(I,K,J-1)+DEF(I+IVE(JJ),K,J))                      &
     &           *VTM(I+IVE(JJ),K,JJ-1)*VTM(I,K,JJ)
          USE(I,K,J)=(U(I+IVE(JJ),K,JJ-1)-U(I,K,JJ))*VKSE_J
          VSE(I,K,J)=(V(I+IVE(JJ),K,JJ-1)-V(I,K,JJ))*VKSE_J
          VKSE(I,K,J)=VKSE_J
        ENDDO
        ENDDO
!
      ENDDO
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,jj,k)
      DO J=-1,0
        JJ=JSTART+J
!
        DO K=KTS,KTE
        DO I=MYIS1_P1,MYIE1
          TDIF (I,K,J)=(TNE (I,K,J)-TNE (I+IHW(JJ),K,J-1)               &
     &                 +TSE (I,K,J)-TSE (I+IHW(JJ),K,J+1))              &
     &                 *HDAC(I,JJ)
          QDIF (I,K,J)=(QNE (I,K,J)-QNE (I+IHW(JJ),K,J-1)               &
     &                 +QSE (I,K,J)-QSE (I+IHW(JJ),K,J+1))              &
     &                 *HDAC(I,JJ)*FCDIF
          Q2DIF(I,K,J)=(Q2NE(I,K,J)-Q2NE(I+IHW(JJ),K,J-1)               &
     &                 +Q2SE(I,K,J)-Q2SE(I+IHW(JJ),K,J+1))              &
     &                 *HDAC(I,JJ)
!
          UDIF (I,K,J)=(UNE (I,K,J)-UNE (I+IVW(JJ),K,J-1)               &
     &                 +USE (I,K,J)-USE (I+IVW(JJ),K,J+1))              &
     &                 *HDACV(I,JJ)
          VDIF (I,K,J)=(VNE (I,K,J)-VNE (I+IVW(JJ),K,J-1)               &
     &                 +VSE (I,K,J)-VSE (I+IVW(JJ),K,J+1))              &
     &                 *HDACV(I,JJ)
        ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!***  ITERATION LOOP
!-----------------------------------------------------------------------
!
      DO 600 KS=1,KSMUD
!
      JKNT=0
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  MAIN VERTICAL INTEGRATION LOOP
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      main_integration : DO J=MYJS2,MYJE2
!-----------------------------------------------------------------------
!
!***
!***  SET THE 3RD INDEX IN THE WORKING ARRAYS (SEE SUBROUTINE INIT
!***                                           AND DIAGRAMS IN PFDHT)
!***
!***  J[TYPE]_NN WHERE "TYPE" IS THE WORKING ARRAY TYPE SEEN IN THE
!***  LOCAL DECLARATION ABOVE (DEPENDENT UPON THE J EXTENT) AND
!***  NN IS THE NUMBER OF ROWS NORTH OF THE CENTRAL ROW WHOSE J IS
!***  THE CURRENT VALUE OF THE main_integration LOOP.
!***  (P2 denotes +2, etc.)
!***
      JKNT=JKNT+1
!
      J1_P2=INDX3_WRK(2,JKNT,1)
      J1_P1=INDX3_WRK(1,JKNT,1)
!
      J2_P1=INDX3_WRK(1,JKNT,2)
      J2_00=INDX3_WRK(0,JKNT,2)
      J2_M1=INDX3_WRK(-1,JKNT,2)
!
      J3_P2=INDX3_WRK(2,JKNT,3)
      J3_P1=INDX3_WRK(1,JKNT,3)
      J3_00=INDX3_WRK(0,JKNT,3)
!
      J4_P2=INDX3_WRK(2,JKNT,4)
      J4_P1=INDX3_WRK(1,JKNT,4)
      J4_00=INDX3_WRK(0,JKNT,4)
      J4_M1=INDX3_WRK(-1,JKNT,4)
!
!-----------------------------------------------------------------------
!***  SLOPE SWITCHES FOR MOISTURE
!-----------------------------------------------------------------------
      IF(SIGMA==1)THEN
!
!$omp parallel do                                                       &
!$omp& private(i,k,slop)
        DO K=KTS,KTE
!
!***  PRESSURE DOMAIN
!
          IF(DETA1(K)>0.)THEN
            DO I=MYIS_P1,MYIE1_P1
              SNE(I,K)=1.
              SSE(I,K)=1.
            ENDDO
!
!***  SIGMA DOMAIN
!
          ELSE
            DO I=MYIS_P1,MYIE1_P1
              SLOP=ABS((Z(I+IHE(J+1),K,J+2)-Z(I,K,J+1))*RDY)
              IF(SLOP<SLOPHC)THEN
                SNE(I,K)=1.
              ELSE
                SNE(I,K)=0.
              ENDIF
!
              SLOP=ABS((Z(I+IHE(J+2),K,J+1)-Z(I,K,J+2))*RDY)
              IF(SLOP<SLOPHC)THEN
                SSE(I,K)=1.
              ELSE
                SSE(I,K)=0.
              ENDIF
            ENDDO
          ENDIF
!
        ENDDO
      ENDIF
!-----------------------------------------------------------------------
!***  DEFORMATIONS
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(def_j,def1,def2,def3,def4,defsk,deftk,i,k,q2l,slop)
      DO K=KTS,KTE
      DO I=MYIS_P1,MYIE_P1
!
        DEFTK=U(I+IHE(J+2),K,J+2)-U(I+IHW(J+2),K,J+2)                   &
     &       -V(I,K,J+3)+V(I,K,J+1)
        DEFSK=U(I,K,J+3)-U(I,K,J+1)                                     &
     &       +V(I+IHE(J+2),K,J+2)-V(I+IHW(J+2),K,J+2)
        DEF1=W(I+IHW(J+2),K,J+1)-W(I,K,J+2)
        DEF2=W(I+IHE(J+2),K,J+1)-W(I,K,J+2)
        DEF3=W(I+IHW(J+2),K,J+3)-W(I,K,J+2)
        DEF4=W(I+IHE(J+2),K,J+3)-W(I,K,J+2)
        Q2L=MAX(Q2(I,K,J+2),EPSQ2)
        DEF_J=DEFTK*DEFTK+DEFSK*DEFSK+DEF1*DEF1+DEF2*DEF2               & 
     &       +DEF3*DEF3+DEF4*DEF4+SCQ2*Q2L
        DEF_J=SQRT(DEF_J+DEF_J)*HBM2(I,J+2)
        DEF_J=MAX(DEF_J,DEFC)
        DEF_J=MIN(DEF_J,DEFM)
        DEF_J=DEF_J*0.1
        DEF(I,K,J1_P2)=DEF_J
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  DIAGONAL CONTRIBUTIONS
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(hkne_j,hkse_j,i,k,vkne_j,vkse_j)
      DO K=KTS,KTE
      DO I=MYIS_P1,MYIE1_P1
        HKNE_J=(DEF(I,K,J1_P1)+DEF(I+IHE(J+1),K,J1_P2))                 &
     &         *HTM(I,K,J+1)*HTM(I+IHE(J+1),K,J+2)*SNE(I,K)
        TNE (I,K,J2_P1)=(T (I+IHE(J+1),K,J+2)-T (I,K,J+1))*HKNE_J
        QNE (I,K,J2_P1)=(Q (I+IHE(J+1),K,J+2)-Q (I,K,J+1))*HKNE_J
        Q2NE(I,K,J2_P1)=(Q2(I+IHE(J+1),K,J+2)-Q2(I,K,J+1))*HKNE_J
        HKNE(I,K,J2_P1)=HKNE_J
!
        VKNE_J=(DEF(I+IVE(J+1),K,J1_P1)+DEF(I,K,J1_P2))                 &
     &         *VTM(I,K,J+1)*VTM(I+IVE(J+1),K,J+2)
        UNE(I,K,J2_P1)=(U(I+IVE(J+1),K,J+2)-U(I,K,J+1))*VKNE_J
        VNE(I,K,J2_P1)=(V(I+IVE(J+1),K,J+2)-V(I,K,J+1))*VKNE_J
        VKNE(I,K,J2_P1)=VKNE_J
!
        HKSE_J=(DEF(I+IHE(J+2),K,J1_P1)+DEF(I,K,J1_P2))                 &
     &         *HTM(I+IHE(J+2),K,J+1)*HTM(I,K,J+2)*SSE(I,K)
        TSE (I,K,J3_P2)=(T (I+IHE(J+2),K,J+1)-T (I,K,J+2))*HKSE_J
        QSE (I,K,J3_P2)=(Q (I+IHE(J+2),K,J+1)-Q (I,K,J+2))*HKSE_J
        Q2SE(I,K,J3_P2)=(Q2(I+IHE(J+2),K,J+1)-Q2(I,K,J+2))*HKSE_J
        HKSE(I,K,J3_P2)=HKSE_J
!
        VKSE_J=(DEF(I,K,J1_P1)+DEF(I+IVE(J+2),K,J1_P2))                 &
     &         *VTM(I+IVE(J+2),K,J+1)*VTM(I,K,J+2)
        USE (I,K,J3_P2)=(U (I+IVE(J+2),K,J+1)-U (I,K,J+2))*VKSE_J
        VSE (I,K,J3_P2)=(V (I+IVE(J+2),K,J+1)-V (I,K,J+2))*VKSE_J
        VKSE(I,K,J3_P2)=VKSE_J
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,k)
      DO K=KTS,KTE
      DO I=MYIS_P1,MYIE
        TDIF (I,K,J4_P1)=(TNE (I,K,J2_P1)-TNE (I+IHW(J+1),K,J2_00)      &
     &                   +TSE (I,K,J3_P1)-TSE (I+IHW(J+1),K,J3_P2))     &
     &                   *HDAC(I,J+1)
        QDIF (I,K,J4_P1)=(QNE (I,K,J2_P1)-QNE (I+IHW(J+1),K,J2_00)      &
     &                   +QSE (I,K,J3_P1)-QSE (I+IHW(J+1),K,J3_P2))     &
     &                   *HDAC(I,J+1)*FCDIF
        Q2DIF(I,K,J4_P1)=(Q2NE(I,K,J2_P1)-Q2NE(I+IHW(J+1),K,J2_00)      &
     &                   +Q2SE(I,K,J3_P1)-Q2SE(I+IHW(J+1),K,J3_P2))     &
     &                   *HDAC(I,J+1)
!
        UDIF (I,K,J4_P1)=(UNE (I,K,J2_P1)-UNE (I+IVW(J+1),K,J2_00)      &
     &                   +USE (I,K,J3_P1)-USE (I+IVW(J+1),K,J3_P2))     &
     &                   *HDACV(I,J+1)
        VDIF (I,K,J4_P1)=(VNE (I,K,J2_P1)-VNE (I+IVW(J+1),K,J2_00)      &
     &                   +VSE (I,K,J3_P1)-VSE (I+IVW(J+1),K,J3_P2))     &
     &                   *HDACV(I,J+1)
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  2ND ORDER DIFFUSION
!-----------------------------------------------------------------------
!
      IF(SECOND)THEN
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE
        DO I=MYIS1,MYIE1
          T(I,K,J)=T(I,K,J)+TDIF(I,K,J4_00)
          Q(I,K,J)=Q(I,K,J)+QDIF(I,K,J4_00)
!
          U(I,K,J)=U(I,K,J)+UDIF(I,K,J4_00)
          V(I,K,J)=V(I,K,J)+VDIF(I,K,J4_00)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS+1,KTE
        DO I=MYIS1,MYIE1
          Q2(I,K,J)=Q2(I,K,J)+Q2DIF(I,K,J4_00)*HTM(I,K-1,J)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  4TH ORDER DIAGONAL CONTRIBUTIONS
!-----------------------------------------------------------------------
!
      ELSE
!
!$omp parallel do                                                       &
!$omp& private(hkne_j,hkse_j,i,k,vkne_j,vkse_j)
        DO K=KTS,KTE
        DO I=MYIS_P1,MYIE1
          HKNE_J=HKNE(I,K,J2_00)
          TNE (I,K,J2_00)=(TDIF (I+IHE(J),K,J4_P1)-TDIF (I,K,J4_00))    &
     &                    *HKNE_J
          QNE (I,K,J2_00)=(QDIF (I+IHE(J),K,J4_P1)-QDIF (I,K,J4_00))    &
     &                    *HKNE_J
          Q2NE(I,K,J2_00)=(Q2DIF(I+IHE(J),K,J4_P1)-Q2DIF(I,K,J4_00))    &
     &                    *HKNE_J
!
          VKNE_J=VKNE(I,K,J2_00)
          UNE (I,K,J2_00)=(UDIF (I+IVE(J),K,J4_P1)-UDIF (I,K,J4_00))    &
     &                    *VKNE_J
          VNE (I,K,J2_00)=(VDIF (I+IVE(J),K,J4_P1)-VDIF (I,K,J4_00))    &
     &                    *VKNE_J
!
          HKSE_J=HKSE(I,K,J3_P1)
          TSE (I,K,J3_P1)=(TDIF (I+IHE(J+1),K,J4_00)                    &
     &                    -TDIF (I         ,K,J4_P1))*HKSE_J
          QSE (I,K,J3_P1)=(QDIF (I+IHE(J+1),K,J4_00)                    &
     &                    -QDIF (I         ,K,J4_P1))*HKSE_J
          Q2SE(I,K,J3_P1)=(Q2DIF(I+IHE(J+1),K,J4_00)                    &
     &                    -Q2DIF(I         ,K,J4_P1))*HKSE_J

!
          VKSE_J=VKSE(I,K,J3_P1)
          USE (I,K,J3_P1)=(UDIF (I+IVE(J+1),K,J4_00)                    &
     &                    -UDIF (I         ,K,J4_P1))*VKSE_J
          VSE (I,K,J3_P1)=(VDIF (I+IVE(J+1),K,J4_00)                    &
     &                    -VDIF (I         ,K,J4_P1))*VKSE_J
        ENDDO
        ENDDO
!
        IF(J==MYJS2)THEN
!$omp parallel do                                                       &
!$omp& private(hkne_j,hkse_j,i,k,vkne_j,vkse_j)
          DO K=KTS,KTE
          DO I=MYIS_P1,MYIE1
            HKNE_J=HKNE(I,K,J2_M1)
            TNE (I,K,J2_M1)=(TDIF (I+IHE(J-1),K,J4_00)                  &
     &                      -TDIF (I         ,K,J4_M1))*HKNE_J
            QNE (I,K,J2_M1)=(QDIF (I+IHE(J-1),K,J4_00)                  &
     &                      -QDIF (I         ,K,J4_M1))*HKNE_J
            Q2NE(I,K,J2_M1)=(Q2DIF(I+IHE(J-1),K,J4_00)                  &
     &                      -Q2DIF(I         ,K,J4_M1))*HKNE_J
!
            VKNE_J=VKNE(I,K,J2_M1)
            UNE (I,K,J2_M1)=(UDIF (I+IVE(J-1),K,J4_00)                  &
     &                      -UDIF (I         ,K,J4_M1))*VKNE_J
            VNE (I,K,J2_M1)=(VDIF (I+IVE(J-1),K,J4_00)                  &
     &                      -VDIF (I         ,K,J4_M1))*VKNE_J
!
            HKSE_J=HKSE(I,K,J3_00)
            TSE (I,K,J3_00)=(TDIF (I+IHE(J),K,J4_M1)                    &
     &                      -TDIF (I       ,K,J4_00))*HKSE_J
            QSE (I,K,J3_00)=(QDIF (I+IHE(J),K,J4_M1)                    &
     &                      -QDIF (I       ,K,J4_00))*HKSE_J
            Q2SE(I,K,J3_00)=(Q2DIF(I+IHE(J),K,J4_M1)                    &
     &                      -Q2DIF(I       ,K,J4_00))*HKSE_J

!
            VKSE_J=VKSE(I,K,J3_00)
            USE (I,K,J3_00)=(UDIF (I+IVE(J),K,J4_M1)                    &
     &                      -UDIF (I       ,K,J4_00))*VKSE_J
            VSE (I,K,J3_00)=(VDIF (I+IVE(J),K,J4_M1)                    &
     &                      -VDIF (I       ,K,J4_00))*VKSE_J
          ENDDO
          ENDDO
        ENDIF
!
        IF(J==MYJE2)THEN
!
          DO K=KTS,KTE
          DO I=MYIS_P1,MYIE1
            TNE (I,K,J2_P1)=0.
            QNE (I,K,J2_P1)=0.
            Q2NE(I,K,J2_P1)=0.
            UNE (I,K,J2_P1)=0.
            VNE (I,K,J2_P1)=0.
          ENDDO
          ENDDO
!
        ENDIF
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,k,utk,vtk)
        DO K=KTS,KTE
        DO I=MYIS1,MYIE1
          T(I,K,J)=T(I,K,J)-(TNE (I,K,J2_00)-TNE (I+IHW(J),K,J2_M1)     &
     &                      +TSE (I,K,J3_00)-TSE (I+IHW(J),K,J3_P1))    &
     &                      *HDAC(I,J)
          Q(I,K,J)=Q(I,K,J)-(QNE (I,K,J2_00)-QNE (I+IHW(J),K,J2_M1)     &
     &                      +QSE (I,K,J3_00)-QSE (I+IHW(J),K,J3_P1))    &
     &                      *HDAC(I,J)*FCDIF
!
          UTK=U(I,K,J)
          VTK=V(I,K,J)
          U(I,K,J)=U(I,K,J)-(UNE (I,K,J2_00)-UNE (I+IVW(J),K,J2_M1)     &
     &                      +USE (I,K,J3_00)-USE (I+IVW(J),K,J3_P1))    &
     &                      *HDACV(I,J)
          V(I,K,J)=V(I,K,J)-(VNE (I,K,J2_00)-VNE (I+IVW(J),K,J2_M1)     &
     &                      +VSE (I,K,J3_00)-VSE (I+IVW(J),K,J3_P1))    &
     &                      *HDACV(I,J)
          CKE(I,K,J4_00)=0.5*(U(I,K,J)*U(I,K,J)-UTK*UTK                 &
     &                       +V(I,K,J)*V(I,K,J)-VTK*VTK)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE-1
        DO I=MYIS1,MYIE1
          Q2(I,K,J)=Q2(I,K,J)-(Q2NE(I,K,J2_00)-Q2NE(I+IHW(J),K,J2_M1)   &
     &                        +Q2SE(I,K,J3_00)-Q2SE(I+IHW(J),K,J3_P1))  &
     &                        *HDAC(I,J)*HTM(I,K+1,J)
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
      END SUBROUTINE HDIFF
!-----------------------------------------------------------------------
      END MODULE MODULE_DIFFUSION_NMM
