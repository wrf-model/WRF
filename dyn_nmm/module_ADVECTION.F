!#define BIT_FOR_BIT
!
!NCEP_MESO:MODEL_LAYER: HORIZONTAL AND VERTICAL ADVECTION
!
!----------------------------------------------------------------------
!
      MODULE MODULE_ADVECTION
!
!----------------------------------------------------------------------

#define OPTIM_ADVE

#ifdef OPTIM_ADVE
# include "nmm_loop_basemacros.h"
# include "nmm_loop_macros.h"
#else
#endif
      USE MODULE_MPP
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_EXT_INTERNAL
!----------------------------------------------------------------------
#ifdef DM_PARALLEL
      INCLUDE "mpif.h"
#endif
!----------------------------------------------------------------------
!
      REAL,PARAMETER :: FF2=-0.64813,FF3=0.24520,FF4=-0.12189
      REAL,PARAMETER :: FFC=1.533,FBC=1.-FFC
      REAL :: CFL_MAX=0.97
      REAL :: CONSERVE_MIN=0.9,CONSERVE_MAX=1.1
!
!----------------------------------------------------------------------
!
!***  For Crank_Nicholson check only.
!
      INTEGER :: ITEST=10,JTEST=10
      REAL :: ADTP,ADUP,ADVP,TTLO,TTUP,TULO,TUUP,TVLO,TVUP
!
!----------------------------------------------------------------------
      CONTAINS
!
#ifdef OPTIM_ADVE
# include "adve_optim.h"
#else
# include "adve_orig.h"
#endif
!
!***********************************************************************
      SUBROUTINE VAD2(NTSD,DT,IDTAD,DX,DY                               &
     &               ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                &
     &               ,HBM2,LMH                                          &
     &               ,Q,Q2,CWM,PETDT                                    &
     &               ,N_IUP_H,N_IUP_V                                   &
     &               ,N_IUP_ADH,N_IUP_ADV                               &
     &               ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                       &
     &               ,IHE,IHW,IVE,IVW,INDX3_WRK                         &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &               ,IMS,IME,JMS,JME,KMS,KME                           &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    VAD2        VERTICAL ADVECTION OF H2O SUBSTANCE AND TKE
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 96-07-19
!
! ABSTRACT:
!     VAD2 CALCULATES THE CONTRIBUTION OF THE HORIZONTAL AND VERTICAL
!     ADVECTION TO THE TENDENCIES OF WATER SUBSTANCE AND TKE AND THEN
!     UPDATES THOSE VARIABLES.  AN ANTI-FILTERING TECHNIQUE IS USED.
!
! PROGRAM HISTORY LOG:
!   96-07-19  JANJIC   - ORIGINATOR
!   98-11-02  BLACK    - MODIFIED FOR DISTRIBUTED MEMORY
!   99-03-17  TUCCILLO - INCORPORATED MPI_ALLREDUCE FOR GLOBAL SUM
!   02-02-06  BLACK    - CONVERTED TO WRF FORMAT
!   02-09-06  WOLFE    - MORE CONVERSION TO GLOBAL INDEXING
!
! USAGE: CALL VAD2 FROM SUBROUTINE SOLVE_RUNSTREAM
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST
!
!   OUTPUT FILES:
!       NONE
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
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V          &
     &                                        ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V      &
     &                                                ,IUP_ADH,IUP_ADV
! NMM_MAX_DIM is set in configure.wrf and must agree with
! the value of dimspec q in the Registry/Registry
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: IDTAD,NTSD
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: DT,DY,PDTOP
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX,HBM2,PDSL
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PETDT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: CWM,Q,Q2
!
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      REAL,PARAMETER :: FF1=0.525
!
      LOGICAL :: BOT,TOP
!
      INTEGER :: I,IRECV,J,JFP,JFQ,K,KOFF,LAP,LLAP
!
      INTEGER,DIMENSION(KTS:KTE) :: LA
!
      REAL*8 :: ADDT,AFRP,D2PQE,D2PQQ,D2PQW,DEP,DETAP,DPDN,DPUP,DQP     &
     &       ,DWP,E00,E4P,EP,EP0,HADDT,HBM2IJ                           &
     &       ,Q00,Q4P,QP,QP0                                            &
     &       ,RFACEK,RFACQK,RFACWK,RFC,RR                               &
     &       ,SUMNE,SUMNQ,SUMNW,SUMPE,SUMPQ,SUMPW                       &
     &       ,W00,W4P,WP,WP0
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,DQL,DWL,E3,E4,PETDTK           &
     &                          ,RFACE,RFACQ,RFACW,Q3,Q4,W3,W4
!
!**********************************************************************
!
!----------------------------------------------------------------------
!
      ADDT=REAL(IDTAD)*DT
!
!----------------------------------------------------------------------
!
      main_integration : DO J=MYJS2,MYJE2
!
      DO I=MYIS1_P1,MYIE1_P1
!----------------------------------------------------------------------
        KOFF=KTE-LMH(I,J)
!
        E3(KTE)=Q2(I,KTE,J)*0.5
!
        DO K=KTE-1,KOFF+1,-1
          E3(K)=MAX((Q2(I,K+1,J)+Q2(I,K,J))*0.5,EPSQ2)
        ENDDO
!
        DO K=KOFF+1,KTE
          Q3(K)=MAX(Q(I,K,J),EPSQ)
          W3(K)=MAX(CWM(I,K,J),CLIMIT)
          E4(K)=E3(K)
          Q4(K)=Q3(K)
          W4(K)=W3(K)
        ENDDO
!
        PETDTK(KTE)=PETDT(I,KTE-1,J)*0.5
!
        DO K=KTE-1,KOFF+2,-1
          PETDTK(K)=(PETDT(I,K,J)+PETDT(I,K-1,J))*0.5
        ENDDO
!
        PETDTK(KOFF+1)=PETDT(I,KOFF+1,J)*0.5
!----------------------------------------------------------------------
        HADDT=-ADDT*HBM2(I,J)
!
        DO K=KTE,KOFF+1,-1
          RR=PETDTK(K)*HADDT
!
          IF(RR.LT.0.)THEN
            LAP=1
          ELSE
            LAP=-1
          ENDIF
!
          LA(K)=LAP
          LLAP=K+LAP
!
          TOP=.FALSE.
          BOT=.FALSE.
!
          IF(LLAP.GT.KOFF.AND.LLAP.LT.KTE+1.AND.LAP.NE.0)THEN
            RR=ABS(RR/((AETA1(LLAP)-AETA1(K))*PDTOP                     &
     &                +(AETA2(LLAP)-AETA2(K))*PDSL(I,J)))
!
            AFR(K)=(((FF4*RR+FF3)*RR+FF2)*RR+FF1)*RR
            DQP=(Q3(LLAP)-Q3(K))*RR
            DWP=(W3(LLAP)-W3(K))*RR
            DEP=(E3(LLAP)-E3(K))*RR
            DQL(K)=DQP
            DWL(K)=DWP
            DEL(K)=DEP
          ELSE
            TOP=LLAP.EQ.KTE+1
            BOT=LLAP.EQ.KOFF
!
            RR=0.
            AFR(K)=0.
            DQL(K)=0.
            DWL(K)=0.
            DEL(K)=0.
          ENDIF
        ENDDO
!----------------------------------------------------------------------
        IF(TOP)THEN
          IF(LA(KTE-1).GT.0)THEN
            RFC=(DETA1(KTE-1)*PDTOP+DETA2(KTE-1)*PDSL(I,J))             &
     &         /(DETA1(KTE  )*PDTOP+DETA2(KTE  )*PDSL(I,J))
            DQL(KTE)=-DQL(KTE+1)*RFC
            DWL(KTE)=-DWL(KTE+1)*RFC
            DEL(KTE)=-DEL(KTE+1)*RFC
          ENDIF
        ENDIF
!
        IF(BOT)THEN
          IF(LA(KOFF+2).LT.0)THEN
            RFC=(DETA1(KOFF+2)*PDTOP+DETA2(KOFF+2)*PDSL(I,J))           &
     &         /(DETA1(KOFF+1)*PDTOP+DETA2(KOFF+1)*PDSL(I,J))
            DQL(KOFF+1)=-DQL(KOFF+2)*RFC
            DWL(KOFF+1)=-DWL(KOFF+2)*RFC
            DEL(KOFF+1)=-DEL(KOFF+2)*RFC
          ENDIF
        ENDIF
!
        DO K=KOFF+1,KTE
          Q4(K)=Q3(K)+DQL(K)
          W4(K)=W3(K)+DWL(K)
          E4(K)=E3(K)+DEL(K)
        ENDDO
!----------------------------------------------------------------------
!***  ANTI-FILTERING STEP
!----------------------------------------------------------------------
        SUMPQ=0.
        SUMNQ=0.
        SUMPW=0.
        SUMNW=0.
        SUMPE=0.
        SUMNE=0.
!
!***  ANTI-FILTERING LIMITERS
!
        DO 50 K=KTE-1,KOFF+2,-1
!
        DETAP=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
!
        Q4P=Q4(K)
        W4P=W4(K)
        E4P=E4(K)
!
        LAP=LA(K)
!
        IF(LAP.NE.0)THEN
          DPDN=(AETA1(K+LAP)-AETA1(K))*PDTOP                            &
     &        +(AETA2(K+LAP)-AETA2(K))*PDSL(I,J)
          DPUP=(AETA1(K)-AETA1(K-LAP))*PDTOP                            &
     &        +(AETA2(K)-AETA2(K-LAP))*PDSL(I,J)
!
          AFRP=2.*AFR(K)*DPDN*DPDN/(DPDN+DPUP)
          D2PQQ=((Q4(K+LAP)-Q4P)/DPDN                                   &
     &          -(Q4P-Q4(K-LAP))/DPUP)*AFRP
          D2PQW=((W4(K+LAP)-W4P)/DPDN                                   &
     &          -(W4P-W4(K-LAP))/DPUP)*AFRP
          D2PQE=((E4(K+LAP)-E4P)/DPDN                                   &
     &          -(E4P-E4(K-LAP))/DPUP)*AFRP
        ELSE
          D2PQQ=0.
          D2PQW=0.
          D2PQE=0.
        ENDIF
!
        QP=Q4P-D2PQQ
        WP=W4P-D2PQW
        EP=E4P-D2PQE
!
        Q00=Q3(K)
        QP0=Q3(K+LAP)
!
        W00=W3(K)
        WP0=W3(K+LAP)
!
        E00=E3(K)
        EP0=E3(K+LAP)
!
        IF(LAP.NE.0)THEN
          QP=MAX(QP,MIN(Q00,QP0))
          QP=MIN(QP,MAX(Q00,QP0))
          WP=MAX(WP,MIN(W00,WP0))
          WP=MIN(WP,MAX(W00,WP0))
          EP=MAX(EP,MIN(E00,EP0))
          EP=MIN(EP,MAX(E00,EP0))
        ENDIF
!
        DQP=QP-Q00
        DWP=WP-W00
        DEP=EP-E00
!
        DQL(K)=DQP
        DWL(K)=DWP
        DEL(K)=DEP
!
        DQP=DQP*DETAP
        DWP=DWP*DETAP
        DEP=DEP*DETAP
!
        IF(DQP.GT.0.)THEN
          SUMPQ=SUMPQ+DQP
        ELSE
          SUMNQ=SUMNQ+DQP
        ENDIF
!
        IF(DWP.GT.0.)THEN
          SUMPW=SUMPW+DWP
        ELSE
          SUMNW=SUMNW+DWP
        ENDIF
!
        IF(DEP.GT.0.)THEN
          SUMPE=SUMPE+DEP
        ELSE
          SUMNE=SUMNE+DEP
        ENDIF
!
   50   CONTINUE
!----------------------------------------------------------------------
        DQL(KOFF+1)=0.
        DWL(KOFF+1)=0.
        DEL(KOFF+1)=0.
!
        DQL(KTE)=0.
        DWL(KTE)=0.
        DEL(KTE)=0.
!----------------------------------------------------------------------
!***  FIRST MOMENT CONSERVING FACTOR
!----------------------------------------------------------------------
        IF(SUMPQ.GT.1.E-9)THEN
          RFACQK=-SUMNQ/SUMPQ
        ELSE
          RFACQK=1.
        ENDIF
!
        IF(SUMPW.GT.1.E-9)THEN
          RFACWK=-SUMNW/SUMPW
        ELSE
          RFACWK=1.
        ENDIF
!
        IF(SUMPE.GT.1.E-9)THEN
          RFACEK=-SUMNE/SUMPE
        ELSE
          RFACEK=1.
        ENDIF
!
        IF(RFACQK.LT.CONSERVE_MIN.OR.RFACQK.GT.CONSERVE_MAX)RFACQK=1.
        IF(RFACWK.LT.CONSERVE_MIN.OR.RFACWK.GT.CONSERVE_MAX)RFACWK=1.
        IF(RFACEK.LT.CONSERVE_MIN.OR.RFACEK.GT.CONSERVE_MAX)RFACEK=1.
!----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!----------------------------------------------------------------------
        DO K=KTE,KOFF+1,-1
          DQP=DQL(K)
          IF(DQP.GE.0.)DQP=DQP*RFACQK
          Q(I,K,J)=Q3(K)+DQP
        ENDDO
!----------------------------------------------------------------------
        DO K=KTE,KOFF+1,-1
          DWP=DWL(K)
          IF(DWP.GE.0.)DWP=DWP*RFACWK
          CWM(I,K,J)=W3(K)+DWP
        ENDDO
!----------------------------------------------------------------------
        DO K=KTE,KOFF+1,-1
          DEP=DEL(K)
          IF(DEP.GE.0.)DEP=DEP*RFACEK
          E3(K)=E3(K)+DEP
        ENDDO
!
        HBM2IJ=HBM2(I,J)
        Q2(I,KTE,J)=MAX(E3(KTE)+E3(KTE)-EPSQ2,EPSQ2)*HBM2IJ             &
     &             +Q2(I,KTE,J)*(1.-HBM2IJ)
        DO K=KTE-1,KOFF+2,-1
          Q2(I,K,J)=MAX(E3(K)+E3(K)-Q2(I,K+1,J),EPSQ2)*HBM2IJ           &
     &             +Q2(I,K,J)*(1.-HBM2IJ)
        ENDDO
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      ENDDO 
!
      ENDDO main_integration
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      END SUBROUTINE VAD2
!----------------------------------------------------------------------
!
!***********************************************************************
      SUBROUTINE HAD2(                                                  &
#if defined(DM_PARALLEL)
     &                domdesc ,                                         &
#endif
     &                NTSD,DT,IDTAD,DX,DY                               &
     &               ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                &
     &               ,HTM,HBM2,HBM3,LMH                                 &
     &               ,Q,Q2,CWM,U,V,Z,HYDRO                              &
     &               ,N_IUP_H,N_IUP_V                                   &
     &               ,N_IUP_ADH,N_IUP_ADV                               &
     &               ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                       &
     &               ,IHE,IHW,IVE,IVW,INDX3_WRK                         &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &               ,IMS,IME,JMS,JME,KMS,KME                           &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    HAD2        HORIZONTAL ADVECTION OF H2O AND TKE
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 96-07-19
!
! ABSTRACT:
!     HAD2 CALCULATES THE CONTRIBUTION OF THE HORIZONTAL ADVECTION
!     TO THE TENDENCIES OF WATER SUBSTANCE AND TKE AND THEN
!     UPDATES THOSE VARIABLES.  AN ANTI-FILTERING TECHNIQUE IS USED.
!
! PROGRAM HISTORY LOG:
!   96-07-19  JANJIC   - ORIGINATOR
!   98-11-02  BLACK    - MODIFIED FOR DISTRIBUTED MEMORY
!   99-03-17  TUCCILLO - INCORPORATED MPI_ALLREDUCE FOR GLOBAL SUM
!   02-02-06  BLACK    - CONVERTED TO WRF FORMAT
!   02-09-06  WOLFE    - MORE CONVERSION TO GLOBAL INDEXING
!   03-05-23  JANJIC   - ADDED SLOPE FACTOR
!
! USAGE: CALL ADV2 FROM SUBROUTINE SOLVE_RUNSTREAM
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST
!
!   OUTPUT FILES:
!       NONE
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
!**********************************************************************
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V          &
     &                                        ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V      &
     &                                                ,IUP_ADH,IUP_ADV
! NMM_MAX_DIM is set in configure.wrf and must agree with
! the value of dimspec q in the Registry/Registry
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: IDTAD,NTSD
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: DT,DY,PDTOP
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX,HBM2,HBM3,PDSL
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: HTM,U,V,Z
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: CWM,Q,Q2
!
      LOGICAL,INTENT(IN) :: HYDRO
!
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      REAL,PARAMETER :: FF1=0.530
#ifdef DM_PARALLEL
      INTEGER domdesc
#endif
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL)
      LOGICAL, EXTERNAL :: wrf_dm_on_monitor
      INTEGER ii
      REAL, DIMENSION(ims:ime,kms:kme,jms:jme,6) :: xsums_l
      REAL, DIMENSION(ids:ide,kds:kde,jds:jde,6) :: xsums_g
#endif
!
      LOGICAL :: BOT,TOP
!
      INTEGER :: I,IRECV,J,JFP,JFQ,K,KOFF,LAP,LLAP
!
      INTEGER,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: IFPA,IFPF           &
     &                                             ,IFQA,IFQF           &
     &                                             ,JFPA,JFPF           &
     &                                             ,JFQA,JFQF
!
      REAL :: ADDT,AFRP,CRIT,D2PQE,D2PQQ,D2PQW,DEP,DESTIJ,DQP,DQSTIJ    &
     &       ,DVOLP,DWP,DWSTIJ,DZA,DZB,E00,E0Q,E1X,E2IJ,E4P,ENH,EP,EP0  &
     &       ,ESTIJ,FPQ,HAFP,HAFQ,HBM2IJ,HM,HTMIKJ,PP,PPQ00,Q00,Q0Q     &
     &       ,Q1IJ,Q4P,QP,QP0,QSTIJ,RDY,RFACEK,RFACQK,RFACWK,RFC        &
     &       ,RFEIJ,RFQIJ,RFWIJ,RR,SLOPAC,SPP,SQP,SSA,SSB,SUMNE,SUMNQ   &
     &       ,SUMNW,SUMPE,SUMPQ,SUMPW,TTA,TTB,W00,W0Q,W1IJ,W4P,WP,WP0   &
     &       ,WSTIJ
!
      DOUBLE PRECISION,DIMENSION(6,KTE-KTS+1) :: GSUMS,XSUMS
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,DQL,DWL,E3,E4                  &
     &                          ,RFACE,RFACQ,RFACW,Q3,Q4,W3,W4
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: DARE,EMH
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,JTS-5:JTE+5) :: AFP,AFQ,DEST   &
     &                                                  ,DQST,DVOL,DWST &
     &                                                  ,E1,E2,Q1,W1
      integer :: nunit,ier
      save nunit
!**********************************************************************
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!
      RDY=1./DY
      SLOPAC=SLOPHT*SQRT(2.)*0.5*50.
      CRIT=SLOPAC*REAL(IDTAD)*DT*RDY*1000.
!
      ADDT=REAL(IDTAD)*DT
      ENH=ADDT/(08.*DY)
!
!----------------------------------------------------------------------
      DO J=MYJS_P3,MYJE_P3
      DO I=MYIS_P2,MYIE_P2
        EMH (I,J)=ADDT/(08.*DX(I,J))
        DARE(I,J)=HBM3(I,J)*DX(I,J)*DY
        E1(I,KTE,J)=MAX(Q2(I,KTE,J)*0.5,EPSQ2)
        E2(I,KTE,J)=E1(I,KTE,J)
      ENDDO
      ENDDO
!----------------------------------------------------------------------
!
      DO J=MYJS_P3,MYJE_P3
        DO K=KTS,KTE
        DO I=MYIS_P2,MYIE_P2
          DVOL(I,K,J)=DARE(I,J)*(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))
          HTMIKJ=HTM(I,K,J)
          Q  (I,K,J)=MAX(Q  (I,K,J),EPSQ)*HTMIKJ
          CWM(I,K,J)=MAX(CWM(I,K,J),CLIMIT)*HTMIKJ
          Q1  (I,K,J)=Q  (I,K,J)
          W1  (I,K,J)=CWM(I,K,J)
        ENDDO
        ENDDO
!
        DO K=KTE-1,KTS,-1
        DO I=MYIS_P2,MYIE_P2
          E1X=(Q2(I,K+1,J)+Q2(I,K,J))*0.5
          E1(I,K,J)=MAX(E1X,EPSQ2)
          E2(I,K,J)=E1(I,K,J)
        ENDDO
        ENDDO
!
      ENDDO
!----------------------------------------------------------------------
      DO J=MYJS2_P1,MYJE2_P1
      DO K=KTS,KTE
      DO I=MYIS1_P1,MYIE1_P1
!
        TTA=(U(I,K,J-1)+U(I+IHW(J),K,J)+U(I+IHE(J),K,J)+U(I,K,J+1))     &
     &      *EMH(I,J)*HBM2(I,J)
        TTB=(V(I,K,J-1)+V(I+IHW(J),K,J)+V(I+IHE(J),K,J)+V(I,K,J+1))     &
     &      *ENH*HBM2(I,J)
!
        SPP=-TTA-TTB
        SQP= TTA-TTB
!
        IF(SPP.LT.0.)THEN
          JFP=-1
        ELSE
          JFP=1
        ENDIF
        IF(SQP.LT.0.)THEN
          JFQ=-1
        ELSE
          JFQ=1
        ENDIF
!
        IFPA(I,K,J)=IHE(J)+I+( JFP-1)/2
        IFQA(I,K,J)=IHE(J)+I+(-JFQ-1)/2
!
        JFPA(I,K,J)=J+JFP
        JFQA(I,K,J)=J+JFQ
!
        IFPF(I,K,J)=IHE(J)+I+(-JFP-1)/2
        IFQF(I,K,J)=IHE(J)+I+( JFQ-1)/2
!
        JFPF(I,K,J)=J-JFP
        JFQF(I,K,J)=J-JFQ
!
!-----------------------------------------------------------------------
        IF(.NOT.HYDRO)THEN ! z currently not available for hydro=.true.
          DZA=(Z(IFPA(I,K,J),K,JFPA(I,K,J))-Z(I,K,J))*RDY
          DZB=(Z(IFQA(I,K,J),K,JFQA(I,K,J))-Z(I,K,J))*RDY
!
          IF(ABS(DZA).GT.SLOPAC)THEN
            SSA=DZA*SPP
            IF(SSA.GT.CRIT)THEN
              SPP=0. !spp*.1
            ENDIF
          ENDIF
!
          IF(ABS(DZB).GT.SLOPAC)THEN
            SSB=DZB*SQP
            IF(SSB.GT.CRIT)THEN
              SQP=0. !sqp*.1
            ENDIF
          ENDIF
!
        ENDIF
!-----------------------------------------------------------------------
        SPP=SPP*HTM(IFPA(I,K,J),K,JFPA(I,K,J))
        SQP=SQP*HTM(IFQA(I,K,J),K,JFQA(I,K,J))
        FPQ=SPP*SQP*HTM(I,K,J-2)*HTM(I-1,K,J)                           &
     &             *HTM(I+1,K,J)*HTM(I,K,J+2)*0.25
        PP=ABS(SPP)
        QP=ABS(SQP)
!
        AFP(I,K,J)=(((FF4*PP+FF3)*PP+FF2)*PP+FF1)*PP
        AFQ(I,K,J)=(((FF4*QP+FF3)*QP+FF2)*QP+FF1)*QP
!
        Q1(I,K,J)=(Q  (IFPA(I,K,J),K,JFPA(I,K,J))-Q  (I,K,J))*PP        &
     &           +(Q  (IFQA(I,K,J),K,JFQA(I,K,J))-Q  (I,K,J))*QP        &
     &           +(Q  (I,K,J-2)+Q  (I,K,J+2)                            &
     &            -Q  (I-1,K,J)-Q  (I+1,K,J))*FPQ                       &
     &           +Q(I,K,J)
!
        W1(I,K,J)=(CWM(IFPA(I,K,J),K,JFPA(I,K,J))-CWM(I,K,J))*PP        &
     &           +(CWM(IFQA(I,K,J),K,JFQA(I,K,J))-CWM(I,K,J))*QP        &
     &           +(CWM(I,K,J-2)+CWM(I,K,J+2)                            &
     &            -CWM(I-1,K,J)-CWM(I+1,K,J))*FPQ                       &
     &           +CWM(I,K,J)
!
        E2(I,K,J)=(E1 (IFPA(I,K,J),K,JFPA(I,K,J))-E1 (I,K,J))*PP        &
     &           +(E1 (IFQA(I,K,J),K,JFQA(I,K,J))-E1 (I,K,J))*QP        &
     &           +(E1 (I,K,J-2)+E1 (I,K,J+2)                            &
     &            -E1 (I-1,K,J)-E1 (I+1,K,J))*FPQ                       &
     &           +E1(I,K,J)
!
      ENDDO
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!***  ANTI-FILTERING STEP
!----------------------------------------------------------------------
!
      DO K=KTS,KTE
        XSUMS(1,K)=0.
        XSUMS(2,K)=0.
        XSUMS(3,K)=0.
        XSUMS(4,K)=0.
        XSUMS(5,K)=0.
        XSUMS(6,K)=0.
      ENDDO
!
!-------------ANTI-FILTERING LIMITERS----------------------------------
!
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL)
      xsums_l = 0
      xsums_g = 0
#endif
      DO 150 J=MYJS2,MYJE2
      DO 150 K=KTS,KTE
      DO 150 I=MYIS1,MYIE1
!
      DVOLP=DVOL(I,K,J)
      Q1IJ =Q1(I,K,J)
      W1IJ =W1(I,K,J)
      E2IJ =E2(I,K,J)
!
      HAFP=HTM(IFPF(I,K,J),K,JFPF(I,K,J))*AFP(I,K,J)
      HAFQ=HTM(IFQF(I,K,J),K,JFQF(I,K,J))*AFQ(I,K,J)
!
      D2PQQ=(Q1(IFPA(I,K,J),K,JFPA(I,K,J))-Q1IJ                         &
     &      -Q1IJ+Q1(IFPF(I,K,J),K,JFPF(I,K,J)))                        &
     &      *HAFP                                                       &
     &     +(Q1(IFQA(I,K,J),K,JFQA(I,K,J))-Q1IJ                         &
     &      -Q1IJ+Q1(IFQF(I,K,J),K,JFQF(I,K,J)))                        &
     &      *HAFQ
!
      D2PQW=(W1(IFPA(I,K,J),K,JFPA(I,K,J))-W1IJ                         &
     &      -W1IJ+W1(IFPF(I,K,J),K,JFPF(I,K,J)))                        &
     &      *HAFP                                                       &
     &     +(W1(IFQA(I,K,J),K,JFQA(I,K,J))-W1IJ                         &
     &      -W1IJ+W1(IFQF(I,K,J),K,JFQF(I,K,J)))                        &
     &      *HAFQ
!
      D2PQE=(E2(IFPA(I,K,J),K,JFPA(I,K,J))-E2IJ                         &
     &      -E2IJ+E2(IFPF(I,K,J),K,JFPF(I,K,J)))                        &
     &      *HAFP                                                       &
     &     +(E2(IFQA(I,K,J),K,JFQA(I,K,J))-E2IJ                         &
     &      -E2IJ+E2(IFQF(I,K,J),K,JFQF(I,K,J)))                        &
     &      *HAFQ
!
      QSTIJ=Q1IJ-D2PQQ
      WSTIJ=W1IJ-D2PQW
      ESTIJ=E2IJ-D2PQE
!
      Q00=Q  (I          ,K          ,J)
      QP0=Q  (IFPA(I,K,J),K,JFPA(I,K,J))
      Q0Q=Q  (IFQA(I,K,J),K,JFQA(I,K,J))
!
      W00=CWM(I          ,K          ,J)
      WP0=CWM(IFPA(I,K,J),K,JFPA(I,K,J))
      W0Q=CWM(IFQA(I,K,J),K,JFQA(I,K,J))
!
      E00=E1 (I          ,K          ,J)
      EP0=E1 (IFPA(I,K,J),K,JFPA(I,K,J))
      E0Q=E1 (IFQA(I,K,J),K,JFQA(I,K,J))
!
      QSTIJ=MAX(QSTIJ,MIN(Q00,QP0,Q0Q))
      QSTIJ=MIN(QSTIJ,MAX(Q00,QP0,Q0Q))
      WSTIJ=MAX(WSTIJ,MIN(W00,WP0,W0Q))
      WSTIJ=MIN(WSTIJ,MAX(W00,WP0,W0Q))
      ESTIJ=MAX(ESTIJ,MIN(E00,EP0,E0Q))
      ESTIJ=MIN(ESTIJ,MAX(E00,EP0,E0Q))
!
      DQSTIJ=QSTIJ-Q(I,K,J)
      DWSTIJ=WSTIJ-CWM(I,K,J)
      DESTIJ=ESTIJ-E1(I,K,J)
!
      DQST(I,K,J)=DQSTIJ
      DWST(I,K,J)=DWSTIJ
      DEST(I,K,J)=DESTIJ
!
      DQSTIJ=DQSTIJ*DVOLP
      DWSTIJ=DWSTIJ*DVOLP
      DESTIJ=DESTIJ*DVOLP
!
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL)
      do ii = 1,6
      xsums_l(i,k,j,ii) = 0
      enddo
      IF(DQSTIJ.GT.0.)THEN
        XSUMS_l(i,K,j,1)=DQSTIJ
      ELSE
        XSUMS_l(i,K,j,2)=DQSTIJ
      ENDIF
!
      IF(DWSTIJ.GT.0.)THEN
        XSUMS_l(i,K,j,3)=DWSTIJ
      ELSE
        XSUMS_l(i,K,j,4)=DWSTIJ
      ENDIF
!
      IF(DESTIJ.GT.0.)THEN
        XSUMS_l(i,K,j,5)=DESTIJ
      ELSE
        XSUMS_l(i,K,j,6)=DESTIJ
      ENDIF
#else
      IF(DQSTIJ.GT.0.)THEN
        XSUMS(1,K)=XSUMS(1,K)+DQSTIJ
      ELSE
        XSUMS(2,K)=XSUMS(2,K)+DQSTIJ
      ENDIF
!
      IF(DWSTIJ.GT.0.)THEN
        XSUMS(3,K)=XSUMS(3,K)+DWSTIJ
      ELSE
        XSUMS(4,K)=XSUMS(4,K)+DWSTIJ
      ENDIF
!
      IF(DESTIJ.GT.0.)THEN
        XSUMS(5,K)=XSUMS(5,K)+DESTIJ
      ELSE
        XSUMS(6,K)=XSUMS(6,K)+DESTIJ
      ENDIF
#endif
!
  150 CONTINUE
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL)
      DO II=1,6
        CALL WRF_PATCH_TO_GLOBAL_REAL( XSUMS_L(IMS,KMS,JMS,II),         &
     &                                 XSUMS_G(1,1,1,ii), domdesc,      & 
     &                                'xyz','xzy',                      &
     &                                 IDS,IDE,KDS,KDE,JDS,JDE,         &    
     &                                 IMS,IME,KMS,KME,JMS,JME,         &
     &                                 ITS,ITE,KTS,KTE,JTS,JTE )
      ENDDO
!
      GSUMS=0.
!
      IF(WRF_DM_ON_MONITOR())THEN
        DO II=1,6
          DO J =JDS,JDE
          DO K =KDS,KDE
          DO I =IDS,IDE
            GSUMS(II,K)=GSUMS(II,K)+XSUMS_G(I,K,J,II)
          ENDDO
          ENDDO
          ENDDO
        ENDDO
      ENDIF

      CALL WRF_DM_BCAST_BYTES(GSUMS,2*RWORDSIZE*6*(KDE-KDS+1) )

#else
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  GLOBAL REDUCTION
!----------------------------------------------------------------------
!
# ifdef DM_PARALLEL
      CALL WRF_GET_DM_COMMUNICATOR ( MPI_COMM_COMP )
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,6*(KTE-KTS+1)                      &
     &                  ,MPI_DOUBLE_PRECISION,MPI_SUM                   &
     &                  ,MPI_COMM_COMP,IRECV)
# else
      GSUMS=XSUMS
# endif
#endif
!
!----------------------------------------------------------------------
!***  END OF GLOBAL REDUCTION
!----------------------------------------------------------------------
!
!     if(mype.eq.0)then
!       if(ntsd.eq.0)then
!!        call int_get_fresh_handle(nunit)
!!        close(nunit)
!         nunit=56
!         open(unit=nunit,file='gsums',form='unformatted',iostat=ier)
!       endif
!     endif
      DO K=KTS,KTE
!       if(mype.eq.0)then
!         write(nunit)(gsums(i,k),i=1,6)
!       endif
!
!----------------------------------------------------------------------
        SUMPQ=GSUMS(1,K)
        SUMNQ=GSUMS(2,K)
        SUMPW=GSUMS(3,K)
        SUMNW=GSUMS(4,K)
        SUMPE=GSUMS(5,K)
        SUMNE=GSUMS(6,K)
!
!----------------------------------------------------------------------
!***  FIRST MOMENT CONSERVING FACTOR
!----------------------------------------------------------------------
!
        IF(SUMPQ.GT.1.)THEN
          RFACQK=-SUMNQ/SUMPQ
        ELSE
          RFACQK=1.
        ENDIF
!
        IF(SUMPW.GT.1.)THEN
          RFACWK=-SUMNW/SUMPW
        ELSE
          RFACWK=1.
        ENDIF
!
        IF(SUMPE.GT.1.)THEN
          RFACEK=-SUMNE/SUMPE
        ELSE
          RFACEK=1.
        ENDIF
!
        IF(RFACQK.LT.CONSERVE_MIN.OR.RFACQK.GT.CONSERVE_MAX)RFACQK=1.
        IF(RFACWK.LT.CONSERVE_MIN.OR.RFACWK.GT.CONSERVE_MAX)RFACWK=1.
        IF(RFACEK.LT.CONSERVE_MIN.OR.RFACEK.GT.CONSERVE_MAX)RFACEK=1.
!
        RFACQ(K)=RFACQK
        RFACW(K)=RFACWK
        RFACE(K)=RFACEK
!
      ENDDO
!     if(mype.eq.0.and.ntsd.eq.181)close(nunit)
!
!----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!----------------------------------------------------------------------
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
          RFACQK=RFACQ(K)
          IF(RFACQK.LT.1.)THEN
            DO I=MYIS1,MYIE1
              DQSTIJ=DQST(I,K,J)
              RFQIJ=HBM2(I,J)*(RFACQK-1.)+1.
              IF(DQSTIJ.GE.0.)DQSTIJ=DQSTIJ*RFQIJ
              Q  (I,K,J)=Q(I,K,J)+DQSTIJ
            ENDDO
          ELSE
            DO I=MYIS1,MYIE1
              DQSTIJ=DQST(I,K,J)
              RFQIJ=HBM2(I,J)*(RFACQK-1.)+1.
              IF(DQSTIJ.LT.0.)DQSTIJ=DQSTIJ/RFQIJ
              Q  (I,K,J)=Q(I,K,J)+DQSTIJ
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!----------------------------------------------------------------------
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
          RFACWK=RFACW(K)
          IF(RFACWK.LT.1.)THEN
            DO I=MYIS1,MYIE1
              DWSTIJ=DWST(I,K,J)
              RFWIJ=HBM2(I,J)*(RFACWK-1.)+1.
              IF(DWSTIJ.GE.0.)DWSTIJ=DWSTIJ*RFWIJ
              CWM(I,K,J)=CWM(I,K,J)+DWSTIJ
            ENDDO
          ELSE
            DO I=MYIS1,MYIE1
              DWSTIJ=DWST(I,K,J)
              RFWIJ=HBM2(I,J)*(RFACWK-1.)+1.
              IF(DWSTIJ.LT.0.)DWSTIJ=DWSTIJ/RFWIJ
              CWM(I,K,J)=CWM(I,K,J)+DWSTIJ
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!----------------------------------------------------------------------
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
          RFACEK=RFACE(K)
          IF(RFACEK.LT.1.)THEN
            DO I=MYIS1,MYIE1
              DESTIJ=DEST(I,K,J)
              RFEIJ=HBM2(I,J)*(RFACEK-1.)+1.
              IF(DESTIJ.GE.0.)DESTIJ=DESTIJ*RFEIJ
              E1 (I,K,J)=E1 (I,K,J)+DESTIJ
            ENDDO
          ELSE
            DO I=MYIS1,MYIE1
              DESTIJ=DEST(I,K,J)
              RFEIJ=HBM2(I,J)*(RFACEK-1.)+1.
              IF(DESTIJ.LT.0.)DESTIJ=DESTIJ/RFEIJ
              E1 (I,K,J)=E1 (I,K,J)+DESTIJ
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!----------------------------------------------------------------------
      DO J=MYJS,MYJE
      DO K=KTS,KTE
      DO I=MYIS,MYIE
        Q  (I,K,J)=MAX(Q  (I,K,J),EPSQ)*HTM(I,K,J)
        CWM(I,K,J)=MAX(CWM(I,K,J),CLIMIT)*HTM(I,K,J)
      ENDDO
      ENDDO
      ENDDO
!
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        Q2(I,KTE,J)=MAX(E1(I,KTE,J)+E1(I,KTE,J)-EPSQ2,EPSQ2)            &
     &             *HTM(I,KTE,J)
      ENDDO
      ENDDO
!
      DO J=MYJS,MYJE
      DO K=KTE-1,KTS+1,-1
      DO I=MYIS,MYIE
        KOFF=KTE-LMH(I,J)
        IF(K.GT.KOFF+1)THEN
          Q2(I,K,J)=MAX(E1(I,K,J)+E1(I,K,J)-Q2(I,K+1,J),EPSQ2)          &
     &             *HTM(I,K,J)
        ELSE
          Q2(I,K,J)=Q2(I,K+1,J)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      END SUBROUTINE HAD2
!----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE VAD2_DRY(NTSD,DT,IDTAD,DX,DY                           &
     &                   ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP            &
     &                   ,HBM2,LMH                                      &
     &                   ,Q2,PETDT                                      &
     &                   ,N_IUP_H,N_IUP_V                               &
     &                   ,N_IUP_ADH,N_IUP_ADV                           &
     &                   ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                   &
     &                   ,IHE,IHW,IVE,IVW,INDX3_WRK                     &
     &                   ,IDS,IDE,JDS,JDE,KDS,KDE                       &
     &                   ,IMS,IME,JMS,JME,KMS,KME                       &
     &                   ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    VAD2_DRY    VERTICAL ADVECTION OF TKE
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 96-07-19
!
! ABSTRACT:
!     VAD2 CALCULATES THE CONTRIBUTION OF THE HORIZONTAL AND VERTICAL
!     ADVECTION TO THE TENDENCY OF TKE AND THEN UPDATES IT.
!     AN ANTI-FILTERING TECHNIQUE IS USED.
!
! PROGRAM HISTORY LOG:
!   96-07-19  JANJIC   - ORIGINATOR
!   98-11-02  BLACK    - MODIFIED FOR DISTRIBUTED MEMORY
!   99-03-17  TUCCILLO - INCORPORATED MPI_ALLREDUCE FOR GLOBAL SUM
!   02-02-06  BLACK    - CONVERTED TO WRF FORMAT
!   02-09-06  WOLFE    - MORE CONVERSION TO GLOBAL INDEXING
!
! USAGE: CALL VAD2_DRY FROM SUBROUTINE DIGITAL_FILTER
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST
!
!   OUTPUT FILES:
!       NONE
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
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V          &
     &                                        ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V      &
     &                                                ,IUP_ADH,IUP_ADV
! NMM_MAX_DIM is set in configure.wrf and must agree with
! the value of dimspec q in the Registry/Registry
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: IDTAD,NTSD
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: DT,DY,PDTOP
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX,HBM2,PDSL
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PETDT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: Q2
!
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      REAL,PARAMETER :: FF1=0.525
!
      LOGICAL :: BOT,TOP
!
      INTEGER :: I,IRECV,J,JFP,JFQ,K,KOFF,LAP,LLAP
!
      INTEGER,DIMENSION(KTS:KTE) :: LA
!
      REAL :: ADDT,AFRP,D2PQE,DEP,DETAP,DPDN,DPUP,DQP                   &
     &       ,DWP,E00,E4P,EP,EP0,HADDT,HBM2IJ                           &
     &       ,RFACEK,RFC,RR,SUMNE,SUMPE
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,E3,E4,PETDTK,RFACE
!
!**********************************************************************
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!
      ADDT=REAL(IDTAD)*DT
!
!----------------------------------------------------------------------
!
      main_integration : DO J=MYJS2,MYJE2
!
      DO I=MYIS1_P1,MYIE1_P1
!----------------------------------------------------------------------
        KOFF=KTE-LMH(I,J)
!
        E3(KTE)=Q2(I,KTE,J)*0.5
!
        DO K=KTE-1,KOFF+1,-1
          E3(K)=MAX((Q2(I,K+1,J)+Q2(I,K,J))*0.5,EPSQ2)
        ENDDO
!
        DO K=KOFF+1,KTE
          E4(K)=E3(K)
        ENDDO
!
        PETDTK(KTE)=PETDT(I,KTE-1,J)*0.5
!
        DO K=KTE-1,KOFF+2,-1
          PETDTK(K)=(PETDT(I,K+1,J)+PETDT(I,K,J))*0.5
        ENDDO
!
        PETDTK(KOFF+1)=PETDT(I,KOFF+1,J)*0.5
!----------------------------------------------------------------------
        HADDT=-ADDT*HBM2(I,J)
!
        DO K=KTE,KOFF+1,-1
          RR=PETDTK(K)*HADDT
!
          IF(RR.LT.0.)THEN
            LAP=1
          ELSE
            LAP=-1
          ENDIF
!
          LA(K)=LAP
          LLAP=K+LAP
!
          TOP=.FALSE.
          BOT=.FALSE.
!
          IF(LLAP.GT.0.AND.LLAP.LT.KTE+1.AND.LAP.NE.0)THEN
            RR=ABS(RR/((AETA1(LLAP)-AETA1(K))*PDTOP                     &
     &                +(AETA2(LLAP)-AETA2(K))*PDSL(I,J)))
!
            AFR(K)=(((FF4*RR+FF3)*RR+FF2)*RR+FF1)*RR
            DEP=(E3(LLAP)-E3(K))*RR
            DEL(K)=DEP
          ELSE
            TOP=LLAP.EQ.KTE+1
            BOT=LLAP.EQ.KOFF
!
            RR=0.
            AFR(K)=0.
            DEL(K)=0.
          ENDIF
        ENDDO
!----------------------------------------------------------------------
        IF(TOP)THEN
          IF(LA(KTE-1).LT.0)THEN
            RFC=(DETA1(KTE-1)*PDTOP+DETA2(KTE-1)*PDSL(I,J))             &
     &         /(DETA1(KTE  )*PDTOP+DETA2(KTE  )*PDSL(I,J))
            DEL(KTE)=-DEL(KTE+1)*RFC
          ENDIF
        ENDIF
!
        IF(BOT)THEN
          IF(LA(KOFF+2).LT.0)THEN
            RFC=(DETA1(KOFF+2)*PDTOP+DETA2(KOFF+2)*PDSL(I,J))           &
     &         /(DETA1(KOFF+1)*PDTOP+DETA2(KOFF+1)*PDSL(I,J))
            DEL(KOFF+1)=-DEL(KOFF+2)*RFC
          ENDIF
        ENDIF
!
        DO K=KOFF+1,KTE
          E4(K)=E3(K)+DEL(K)
        ENDDO
!----------------------------------------------------------------------
!***  ANTI-FILTERING STEP
!----------------------------------------------------------------------
        SUMPE=0.
        SUMNE=0.
!
!***  ANTI-FILTERING LIMITERS
!
        DO 50 K=KTE-1,KOFF+2,-1
!
        DETAP=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
!
        E4P=E4(K)
!
        LAP=LA(K)
!
        IF(LAP.NE.0)THEN
          DPDN=(AETA1(K+LAP)-AETA1(K))*PDTOP                            &
     &        +(AETA2(K+LAP)-AETA2(K))*PDSL(I,J)
          DPUP=(AETA1(K)-AETA1(K-LAP))*PDTOP                            &
     &        +(AETA2(K)-AETA2(K-LAP))*PDSL(I,J)
!
          AFRP=2.*AFR(K)*DPDN*DPDN/(DPDN+DPUP)
          D2PQE=((E4(K+LAP)-E4P)/DPDN                                   &
     &          -(E4P-E4(K-LAP))/DPUP)*AFRP
        ELSE
          D2PQE=0.
        ENDIF
!
        EP=E4P-D2PQE
!
        E00=E3(K)
        EP0=E3(K+LAP)
!
        IF(LAP.NE.0)THEN
          EP=MAX(EP,MIN(E00,EP0))
          EP=MIN(EP,MAX(E00,EP0))
        ENDIF
!
        DEP=EP-E00
!
        DEL(K)=DEP
!
        DEP=DEP*DETAP
!
        IF(DEP.GT.0.)THEN
          SUMPE=SUMPE+DEP
        ELSE
          SUMNE=SUMNE+DEP
        ENDIF
!
   50   CONTINUE
!----------------------------------------------------------------------
        DEL(KTE)=0.
!
        DEL(KOFF+1)=0.
!----------------------------------------------------------------------
!***  FIRST MOMENT CONSERVING FACTOR
!----------------------------------------------------------------------
        IF(SUMPE.GT.1.E-9)THEN
          RFACEK=-SUMNE/SUMPE
        ELSE
          RFACEK=1.
        ENDIF
!
        IF(RFACEK.LT.CONSERVE_MIN.OR.RFACEK.GT.CONSERVE_MAX)RFACEK=1.
!----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!----------------------------------------------------------------------
        DO K=KOFF+1,KTE
          DEP=DEL(K)
          IF(DEP.GE.0.)DEP=DEP*RFACEK
          E3(K)=E3(K)+DEP
        ENDDO
!
        HBM2IJ=HBM2(I,J)
        Q2(I,KTE,J)=MAX(E3(KTE)+E3(KTE)-EPSQ2,EPSQ2)*HBM2IJ             &
     &             +Q2(I,KTE,J)*(1.-HBM2IJ)
        DO K=KTE-1,KOFF+2
          Q2(I,K,J)=MAX(E3(K)+E3(K)-Q2(I,K+1,J),EPSQ2)*HBM2IJ           &
     &             +Q2(I,K,J)*(1.-HBM2IJ)
        ENDDO
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      ENDDO 
!
      ENDDO main_integration
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      END SUBROUTINE VAD2_DRY
!----------------------------------------------------------------------
!
!***********************************************************************
      SUBROUTINE HAD2_DRY(NTSD,DT,IDTAD,DX,DY                           &
     &                   ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP            &
     &                   ,HTM,HBM2,HBM3,LMH                             &
     &                   ,Q2,U,V,Z,HYDRO                                &
     &                   ,N_IUP_H,N_IUP_V                               &
     &                   ,N_IUP_ADH,N_IUP_ADV                           &
     &                   ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                   &
     &                   ,IHE,IHW,IVE,IVW,INDX3_WRK                     &
     &                   ,IDS,IDE,JDS,JDE,KDS,KDE                       &
     &                   ,IMS,IME,JMS,JME,KMS,KME                       &
     &                   ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    HAD2_DRY    HORIZONTAL ADVECTION OF TKE
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 96-07-19
!
! ABSTRACT:
!     HAD2 CALCULATES THE CONTRIBUTION OF THE HORIZONTAL ADVECTION
!     TO THE TENDENCIES OF TKE AND UPDATES IT.
!     AN ANTI-FILTERING TECHNIQUE IS USED.
!
! PROGRAM HISTORY LOG:
!   96-07-19  JANJIC   - ORIGINATOR
!   98-11-02  BLACK    - MODIFIED FOR DISTRIBUTED MEMORY
!   99-03-17  TUCCILLO - INCORPORATED MPI_ALLREDUCE FOR GLOBAL SUM
!   02-02-06  BLACK    - CONVERTED TO WRF FORMAT
!   02-09-06  WOLFE    - MORE CONVERSION TO GLOBAL INDEXING
!   03-05-23  JANJIC   - ADDED SLOPE FACTOR
!
! USAGE: CALL HAD2_DRY FROM SUBROUTINE DIGITAL_FILTER
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST
!
!   OUTPUT FILES:
!       NONE
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
!**********************************************************************
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V          &
     &                                        ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V      &
     &                                                ,IUP_ADH,IUP_ADV
! NMM_MAX_DIM is set in configure.wrf and must agree with
! the value of dimspec q in the Registry/Registry
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: IDTAD,NTSD
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: DT,DY,PDTOP
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX,HBM2,HBM3,PDSL
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: HTM,U,V,Z
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: Q2
!
      LOGICAL,INTENT(IN) :: HYDRO
!
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      REAL,PARAMETER :: FF1=0.530
!
      LOGICAL :: BOT,TOP
!
      INTEGER :: I,IRECV,J,JFP,JFQ,K,KOFF,LAP,LLAP
!
      INTEGER,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: IFPA,IFPF           &
     &                                             ,IFQA,IFQF           &
     &                                             ,JFPA,JFPF           &
     &                                             ,JFQA,JFQF
!
      REAL :: ADDT,AFRP,CRIT,D2PQE,DEP,DESTIJ,DVOLP,DZA,DZB             &
     &       ,E00,E0Q,E2IJ,E4P,ENH,EP,EP0,ESTIJ,FPQ                     &
     &       ,HAFP,HAFQ,HBM2IJ,HM,HTMIKJ,PP,PPQ00                       &
     &       ,QP,RDY,RFACEK,RFC,RFEIJ,RR                                &
     &       ,SLOPAC,SPP,SQP,SSA,SSB,SUMNE,SUMPE,TTA,TTB
!
      REAL,DIMENSION(2,KTE-KTS+1) :: GSUMS,XSUMS
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,E3,E4,RFACE
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: DARE,EMH
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: AFP,AFQ,DEST,DVOL      &
     &                                          ,E1,E2  
!
!**********************************************************************
!
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
      RDY=1./DY
      SLOPAC=SLOPHT*SQRT(2.)*0.5*50.
      CRIT=SLOPAC*REAL(IDTAD)*DT*RDY*1000.
!
      ADDT=REAL(IDTAD)*DT
      ENH=ADDT/(08.*DY)
!
!----------------------------------------------------------------------
      DO J=MYJS_P3,MYJE_P3
      DO I=MYIS_P2,MYIE_P2
        EMH (I,J)=ADDT/(08.*DX(I,J))
        DARE(I,J)=HBM3(I,J)*DX(I,J)*DY
        E1(I,KTE,J)=MAX(Q2(I,KTE,J)*0.5,EPSQ2)
        E2(I,KTE,J)=E1(I,KTE,J)
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      DO J=MYJS_P3,MYJE_P3
!
        DO K=KTS,KTE
        DO I=MYIS_P2,MYIE_P2
          DVOL(I,K,J)=DARE(I,J)*(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))
        ENDDO
        ENDDO
!
        DO K=KTE-1,KTS,-1
        DO I=MYIS_P2,MYIE_P2
          E1(I,K,J)=MAX((Q2(I,K+1,J)+Q2(I,K,J))*0.5,EPSQ2)
          E2(I,K,J)=E1(I,K,J)
        ENDDO
        ENDDO
!
      ENDDO
!----------------------------------------------------------------------
      DO J=MYJS2_P1,MYJE2_P1
      DO K=KTS,KTE
      DO I=MYIS1_P1,MYIE1_P1
!
        TTA=(U(I,K,J-1)+U(I+IHW(J),K,J)+U(I+IHE(J),K,J)+U(I,K,J+1))     &
     &      *EMH(I,J)*HBM2(I,J)
        TTB=(V(I,K,J-1)+V(I+IHW(J),K,J)+V(I+IHE(J),K,J)+V(I,K,J+1))     &
     &      *ENH*HBM2(I,J)
!
        SPP=-TTA-TTB
        SQP= TTA-TTB
!
        IF(SPP.LT.0.)THEN
          JFP=-1
        ELSE
          JFP=1
        ENDIF
        IF(SQP.LT.0.)THEN
          JFQ=-1
        ELSE
          JFQ=1
        ENDIF
!
        IFPA(I,K,J)=IHE(J)+I+( JFP-1)/2
        IFQA(I,K,J)=IHE(J)+I+(-JFQ-1)/2
!
        JFPA(I,K,J)=J+JFP
        JFQA(I,K,J)=J+JFQ
!
        IFPF(I,K,J)=IHE(J)+I+(-JFP-1)/2
        IFQF(I,K,J)=IHE(J)+I+( JFQ-1)/2
!
        JFPF(I,K,J)=J-JFP
        JFQF(I,K,J)=J-JFQ
!
!-----------------------------------------------------------------------
        IF(.NOT.HYDRO)THEN ! z currently not available for hydro=.true.
          DZA=(Z(IFPA(I,K,J),K,JFPA(I,K,J))-Z(I,K,J))*RDY
          DZB=(Z(IFQA(I,K,J),K,JFQA(I,K,J))-Z(I,K,J))*RDY
!
          IF(ABS(DZA).GT.SLOPAC)THEN
            SSA=DZA*SPP
            IF(SSA.GT.CRIT)THEN
              SPP=0. !spp*.1
            ENDIF
          ENDIF
!
          IF(ABS(DZB).GT.SLOPAC)THEN
            SSB=DZB*SQP
            IF(SSB.GT.CRIT)THEN
              SQP=0. !sqp*.1
            ENDIF
          ENDIF
!
        ENDIF
!-----------------------------------------------------------------------
        SPP=SPP*HTM(IFPA(I,K,J),K,JFPA(I,K,J))
        SQP=SQP*HTM(IFQA(I,K,J),K,JFQA(I,K,J))
        FPQ=SPP*SQP*HTM(I,K,J-2)*HTM(I-1,K,J)                           &
     &             *HTM(I+1,K,J)*HTM(I,K,J+2)*0.25
        PP=ABS(SPP)
        QP=ABS(SQP)
!
        AFP(I,K,J)=(((FF4*PP+FF3)*PP+FF2)*PP+FF1)*PP
        AFQ(I,K,J)=(((FF4*QP+FF3)*QP+FF2)*QP+FF1)*QP
!
        E2(I,K,J)=(E1 (IFPA(I,K,J),K,JFPA(I,K,J))-E1 (I,K,J))*PP        &
     &           +(E1 (IFQA(I,K,J),K,JFQA(I,K,J))-E1 (I,K,J))*QP        &
     &           +(E1 (I,K,J-2)+E1 (I,K,J+2)                            &
     &            -E1 (I-1,K,J)-E1 (I+1,K,J))*FPQ                       &
     &           +E1(I,K,J)
!
      ENDDO
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!***  ANTI-FILTERING STEP
!----------------------------------------------------------------------
!
      DO K=KTS,KTE
        XSUMS(1,K)=0.
        XSUMS(2,K)=0.
      ENDDO
!
!-------------ANTI-FILTERING LIMITERS----------------------------------
!
      DO 150 J=MYJS2,MYJE2
      DO 150 K=KTS,KTE
      DO 150 I=MYIS1,MYIE1
!
      DVOLP=DVOL(I,K,J)
      E2IJ =E2(I,K,J)
!
      HAFP=HTM(IFPF(I,K,J),K,JFPF(I,K,J))*AFP(I,K,J)
      HAFQ=HTM(IFQF(I,K,J),K,JFQF(I,K,J))*AFQ(I,K,J)
!
      D2PQE=(E2(IFPA(I,K,J),K,JFPA(I,K,J))-E2IJ                         &
     &      -E2IJ+E2(IFPF(I,K,J),K,JFPF(I,K,J)))                        &
     &      *HAFP                                                       &
     &     +(E2(IFQA(I,K,J),K,JFQA(I,K,J))-E2IJ                         &
     &      -E2IJ+E2(IFQF(I,K,J),K,JFQF(I,K,J)))                        &
     &      *HAFQ
!
      ESTIJ=E2IJ-D2PQE
!
      E00=E1 (I          ,K          ,J)
      EP0=E1 (IFPA(I,K,J),K,JFPA(I,K,J))
      E0Q=E1 (IFQA(I,K,J),K,JFQA(I,K,J))
!
      ESTIJ=MAX(ESTIJ,MIN(E00,EP0,E0Q))
      ESTIJ=MIN(ESTIJ,MAX(E00,EP0,E0Q))
!
      DESTIJ=ESTIJ-E1(I,K,J)
      DEST(I,K,J)=DESTIJ
!
      DESTIJ=DESTIJ*DVOLP
!
      IF(DESTIJ.GT.0.)THEN
        XSUMS(1,K)=XSUMS(1,K)+DESTIJ
      ELSE
        XSUMS(2,K)=XSUMS(2,K)+DESTIJ
      ENDIF
!
  150 CONTINUE
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  GLOBAL REDUCTION
!----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
      CALL WRF_GET_DM_COMMUNICATOR ( MPI_COMM_COMP )
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,2*(KTE-KTS+1),MPI_REAL,MPI_SUM     &
     &                  ,MPI_COMM_COMP,IRECV)
#else
      GSUMS=XSUMS
#endif
!
!----------------------------------------------------------------------
!***  END OF GLOBAL REDUCTION
!----------------------------------------------------------------------
!
      DO K=KTS,KTE
!
!----------------------------------------------------------------------
        SUMPE=GSUMS(1,K)
        SUMNE=GSUMS(2,K)
!
!----------------------------------------------------------------------
!***  FIRST MOMENT CONSERVING FACTOR
!----------------------------------------------------------------------
!
        IF(SUMPE.GT.1.)THEN
          RFACEK=-SUMNE/SUMPE
        ELSE
          RFACEK=1.
        ENDIF
!
        IF(RFACEK.LT.CONSERVE_MIN.OR.RFACEK.GT.CONSERVE_MAX)RFACEK=1.
!
        RFACE(K)=RFACEK
!
      ENDDO
!
!----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!----------------------------------------------------------------------
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
          RFACEK=RFACE(K)
          IF(RFACEK.LT.1.)THEN
            DO I=MYIS1,MYIE1
              DESTIJ=DEST(I,K,J)
              RFEIJ=HBM2(I,J)*(RFACEK-1.)+1.
              IF(DESTIJ.GE.0.)DESTIJ=DESTIJ*RFEIJ
              E1 (I,K,J)=E1 (I,K,J)+DESTIJ
            ENDDO
          ELSE
            DO I=MYIS1,MYIE1
              DESTIJ=DEST(I,K,J)
              RFEIJ=HBM2(I,J)*(RFACEK-1.)+1.
              IF(DESTIJ.LT.0.)DESTIJ=DESTIJ/RFEIJ
              E1 (I,K,J)=E1 (I,K,J)+DESTIJ
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!----------------------------------------------------------------------
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        Q2(I,KTE,J)=MAX(E1(I,KTE,J)+E1(I,KTE,J)-EPSQ2,EPSQ2)            &
     &             *HTM(I,KTE,J)
      ENDDO
      ENDDO
!
      DO J=MYJS,MYJE
      DO K=KTE-1,KTS+1,-1
      DO I=MYIS,MYIE
        KOFF=KTE-LMH(I,J)
        IF(K.GT.KOFF+1)THEN
          Q2(I,K,J)=MAX(E1(I,K,J)+E1(I,K,J)-Q2(I,K+1,J),EPSQ2)          &
     &             *HTM(I,K,J)
        ELSE
          Q2(I,K,J)=Q2(I,K+1,J)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      END SUBROUTINE HAD2_DRY
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      END MODULE MODULE_ADVECTION
