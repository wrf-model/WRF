!----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: NONHYDROSTATIC DYNAMICS ROUTINES
!
!----------------------------------------------------------------------
!
#include "nmm_loop_basemacros.h"
#include "nmm_loop_macros.h"
!
!----------------------------------------------------------------------
!
      MODULE MODULE_NONHY_DYNAM
!
!----------------------------------------------------------------------
      USE MODULE_MODEL_CONSTANTS
!     USE MODULE_INDX
!----------------------------------------------------------------------
!
      REAL :: CAPA=R_D/CP,RG=1./G,TRG=2.*R_D/G
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE EPS(NTSD,DT,HYDRO,DX,DY,FAD                            &
                    ,DETA1,DETA2,PDTOP,PT                               &
                    ,HTM,HBM2,HBM3,LMH                                  &
                    ,PDSL,PDSLO,PINT,RTOP,PETDT,PDWDT                   &
                    ,DWDT,DWDTMN,DWDTMX                                 &
                    ,FNS,FEW,FNE,FSE                                    &
                    ,T,U,V,W,Q,CWM                                      &
                    ,IHE,IHW,IVE,IVW,INDX3_WRK                          &
                    ,IDS,IDE,JDS,JDE,KDS,KDE                            &
                    ,IMS,IME,JMS,JME,KMS,KME                            &
                    ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    EPS
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 9?-??-??
!
! ABSTRACT:
!     EPS COMPUTES THE VERTICAL AND HORIZONTAL ADVECTION OF DZ/DT
!
! PROGRAM HISTORY LOG:
!   9?-??-??  JANJIC     - ORIGINATOR
!   00-01-05  BLACK      - DISTRIBUTED MEMORY AND THREADS
!   02-02-07  BLACK      - CONVERTED TO WRF STRUCTURE
!   04-11-22  BLACK      - THREADED
!
! USAGE: CALL EPS FROM SUBROUTINE SOLVE_RUNSTREAM
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
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!-----------------------------------------------------------------------
#ifdef DM_PARALLEL
      INCLUDE "mpif.h"
#endif
!
!-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE 
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
!-----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!***  NMM_MAX_DIM is set in configure.wrf and must agree with
!***  the value of dimspec q in the Registry/Registry
!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
!
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: NTSD
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: LMH
!
      REAL,INTENT(IN) :: DT,DY,PDTOP,PT
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DWDTMN,DWDTMX,DX    &
                                                   ,FAD,HBM2,HBM3       &
                                                   ,PDSL,PDSLO
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PETDT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: CWM         &
                                                           ,FEW,FNE     &
                                                           ,FNS,FSE     &
                                                           ,HTM,Q       &
                                                           ,RTOP        &
                                                           ,U,V
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: DWDT     &
                                                              ,PDWDT    &
                                                              ,T
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: PINT,W
!
      LOGICAL,INTENT(IN) :: HYDRO
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
!
      INTEGER,PARAMETER :: NTSHY=2
!
      REAL,PARAMETER :: WGHT=0.35,WP=0.
!
      INTEGER,DIMENSION(KTS:KTE) :: LA
!
      INTEGER :: I,J,J4_00,J4_M1,J4_P1,J5_00,J5_M1,J6_00,J6_P1          &
                ,JEND,JJ,JKNT,JSTART,K,KOFF,LMP
!
      REAL,DIMENSION(KTS:KTE) :: B1,B2,B3,C0,CWM_K,DWDT_K,Q_K,RDPP      &
                                ,RTOP_K,T_K
!
      REAL,DIMENSION(KTS:KTE+1) :: CHI,COFF,PINT_K,PNP1,PONE,PSTR,W_K
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: TTB
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE) :: WEW
!
      REAL :: ADDT,DELP,DETAL,DP,DPDE,DPPL,DPSTR,DPTL,DPTU              &
             ,DWDTT,EPSN,FCT,FFC,GDT,GDT2                               &
             ,HBM3IJ,HM,PP1,PSTRDN,PSTRUP,RDP,RDPDN,RDPUP,RDT           &
             ,TFC,TMP,TTAL,TTFC
!
      LOGICAL :: BOT,TOP
!
!***  TYPE 4 WORKING ARRAY (SEE PFDHT)
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-1:1) :: WNS
!
!***  TYPE 5 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-1:0) :: WNE
!
!***  TYPE 6 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE, 0:1) :: WSE
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      IF(NTSD<=NTSHY.OR.HYDRO)THEN
!***
        DO J=MYJS_P2,MYJE_P2
        DO I=MYIS_P1,MYIE_P1
          PINT(I,KTE+1,J)=PT
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=MYJS_P2,MYJE_P2
          DO K=KTS,KTE
          DO I=MYIS_P1,MYIE_P1
            DWDT(I,K,J)=1.
            PDWDT(I,K,J)=1.
          ENDDO
          ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=MYJS_P2,MYJE_P2
          DO K=KTE,KTS,-1
          DO I=MYIS_P1,MYIE_P1
            PINT(I,K,J)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)+PINT(I,K+1,J)
          ENDDO
          ENDDO
        ENDDO
!***
        RETURN
!***
      ENDIF
!-----------------------------------------------------------------------
      ADDT=DT
      RDT=1./ADDT
!-----------------------------------------------------------------------
!
!***  TIME TENDENCY
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=MYJS_P1,MYJE_P1
        DO K=KTS,KTE
        DO I=MYIS_P1,MYIE_P1
          DWDT(I,K,J)=(W(I,K,J)-DWDT(I,K,J))*HTM(I,K,J)*HBM2(I,J)*RDT
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***
!***  VERTICAL ADVECTION
!***
!-----------------------------------------------------------------------
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        TTB(I,J)=0.
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,ttal)
      DO J=MYJS2,MYJE2
      DO K=KTE,KTS+1,-1
      DO I=MYIS,MYIE
        TTAL=(W(I,K-1,J)-W(I,K,J))*PETDT(I,K-1,J)*0.5
        DWDT(I,K,J)=(TTAL+TTB(I,J))                                     &
                   /(DETA1(K)*PDTOP+DETA2(K)*PDSLO(I,J))                &
                    +DWDT(I,K,J)
        TTB(I,J)=TTAL
      ENDDO
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        TTB(I,J)=(W(I,KTS,J)-W(I,KTS+1,J))*PETDT(I,KTS,J)*0.5
        DWDT(I,KTS,J)=TTB(I,J)/(DETA1(KTS)*PDTOP+DETA2(KTS)*PDSLO(I,J)) &
                     +DWDT(I,KTS,J)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!***
!***  END OF VERTICAL ADVECTION
!***
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***
!***  HORIZONTAL ADVECTION
!***
!-----------------------------------------------------------------------
!***  MARCH NORTHWARD THROUGH THE SOUTHERNMOST SLABS TO BEGIN
!***  FILLING THE MAIN WORKING ARRAYS WHICH ARE MULTI-DIMENSIONED
!***  IN J BECAUSE THEY ARE DIFFERENCED OR AVERAGED IN J
!-----------------------------------------------------------------------
!
      JSTART=MYJS3
!
      DO J=-1,0
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE
        DO I=MYIS_P3,MYIE_P3
          WNS(I,K,J)=FNS(I,K,JJ)*(W(I,K,JJ+1)-W(I,K,JJ-1))
        ENDDO
        ENDDO
!
      ENDDO
!
      J=-1
      JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(i,k)
      DO K=KTS,KTE
      DO I=MYIS_P2,MYIE1_P2
        WNE(I,K,J)=FNE(I,K,JJ)*(W(I+IHE(JJ),K,JJ+1)-W(I,K,JJ))
      ENDDO
      ENDDO
!
      J=0
      JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(i,k)
      DO K=KTS,KTE
      DO I=MYIS_P2,MYIE1_P2
        WSE(I,K,J)=FSE(I,K,JJ)*(W(I+IHE(JJ),K,JJ-1)-W(I,K,JJ))
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      JKNT=0
      JSTART=MYJS3
      JEND  =MYJE3
!
      main_horizontal:  DO J=JSTART,JEND
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***
!***  SET THE 3RD INDEX IN THE WORKING ARRAYS (SEE SUBROUTINE INIT
!***                                           AND PFDHT DIAGRAMS)
!***
!***  J[TYPE]_NN WHERE "TYPE" IS THE WORKING ARRAY TYPE SEEN IN THE
!***  LOCAL DECLARATION ABOVE (DEPENDENT UPON THE J EXTENT) AND
!***  NN IS THE NUMBER OF ROWS NORTH OF THE CENTRAL ROW WHOSE J IS
!***  THE CURRENT VALUE OF THE main_integration LOOP.
!***  (P3 denotes +3, M1 denotes -1, etc.)
!***
      JKNT=JKNT+1
!
      J4_P1=INDX3_WRK(1,JKNT,4)
      J4_00=INDX3_WRK(0,JKNT,4)
      J4_M1=INDX3_WRK(-1,JKNT,4)
!
      J5_00=INDX3_WRK(0,JKNT,5)
      J5_M1=INDX3_WRK(-1,JKNT,5)
!
      J6_P1=INDX3_WRK(1,JKNT,6)
      J6_00=INDX3_WRK(0,JKNT,6)
!
!-----------------------------------------------------------------------
!***  THE WORKING ARRAYS FOR THE PRIMARY VARIABLES
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dpde,i,k)
      DO K=KTS,KTE
!
      DO I=MYIS_P3,MYIE_P3
        WEW(I,K)=FEW(I,K,J)*(W(I+IVE(J),K,J)-W(I+IVW(J),K,J))
        WNS(I,K,J4_P1)=FNS(I,K,J+1)*(W(I,K,J+2)-W(I,K,J))
      ENDDO
!
!***  DIAGONAL FLUXES AND DIAGONALLY AVERAGED WIND
!
      DO I=MYIS_P2,MYIE1_P2
        WNE(I,K,J5_00)=FNE(I,K,J)*(W(I+IHE(J),K,J+1)-W(I,K,J))
        WSE(I,K,J6_P1)=FSE(I,K,J+1)*(W(I+IHE(J+1),K,J)-W(I,K,J+1))
      ENDDO
!-----------------------------------------------------------------------
!
      DO I=MYIS2,MYIE2
        DPDE=DETA1(K)*PDTOP+DETA2(K)*PDSLO(I,J)
        DWDT(I,K,J)=-(WEW(I+IHW(J),K)      +WEW(I+IHE(J),K)             &
                     +WNS(I,K,J4_M1)       +WNS(I,K,J4_P1)              &
                     +WNE(I+IHW(J),K,J5_M1)+WNE(I,K,J5_00)              &
                     +WSE(I,K,J6_00)       +WSE(I+IHW(J),K,J6_P1))      &
                     *FAD(I,J)*HTM(I,K,J)*HBM3(I,J)/(DPDE*DT)           &
                     +DWDT(I,K,J)
      ENDDO
!
      ENDDO
!-----------------------------------------------------------------------
!
      ENDDO main_horizontal
!
!-----------------------------------------------------------------------
!***
!***  END OF HORIZONTAL ADVECTION
!***
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(dwdtt,i,j,k)
      DO J=MYJS,MYJE
      DO K=KTS,KTE
      DO I=MYIS,MYIE
        DWDTT=DWDT(I,K,J)*HTM(I,K,J)
        DWDTT=MAX(DWDTT,DWDTMN(I,J))
        DWDTT=MIN(DWDTT,DWDTMX(I,J))
!
        DWDT(I,K,J)=(DWDTT*RG+1.)*(1.-WP)+PDWDT(I,K,J)*WP
      ENDDO
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
      GDT=G*DT
      GDT2=GDT*GDT
      FFC=-R_D/GDT2
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(b1,b2,b3,c0,chi,coff,cwm_k,delp,dppl,dpstr,dptl,dptu,    &
!$omp&         dwdt_k,fct,hbm3ij,i,j,k,koff,pint_k,pnp1,pone,pp1,pstr,  &
!$omp&         pstrdn,pstrup,q_k,rdpdn,rdpp,rdpup,rtop_k,t_k,tfc,       &
!$omp&         tmp,ttfc,w_k)
      final_update:  DO J=MYJS3,MYJE3
!
      PONE(KTE+1)=PT
      PSTR(KTE+1)=PT
      PNP1(KTE+1)=PT
      CHI(KTE+1)=0.
!
      DO I=MYIS2,MYIE2
!
!-----------------------------------------------------------------------
!
!***  EXTRACT COLUMNS FROM 3-D ARRAYS
!
        DO K=KTS,KTE
          CWM_K(K)=CWM(I,K,J)
          DWDT_K(K)=DWDT(I,K,J)
          Q_K(K)=Q(I,K,J)
          RTOP_K(K)=RTOP(I,K,J)
          T_K(K)=T(I,K,J)
        ENDDO
!
        DO K=KTS,KTE+1
          PINT_K(K)=PINT(I,K,J)
          W_K(K)=W(I,K,J)
        ENDDO
!-----------------------------------------------------------------------
!
        KOFF=KTE-LMH(I,J)
!
        DO K=KTE,KOFF+1,-1
          CHI(K)=0.
          DPPL=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
          RDPP(K)=1./DPPL
          PONE(K)=PINT_K(K)
          DPSTR=DWDT_K(K)*DPPL
          PSTR(K)=PSTR(K+1)+DPSTR
          PP1=PNP1(K+1)+DPSTR
          PNP1(K)=(PP1-PONE(K))*WGHT+PONE(K)
          TFC=Q_K(K)*P608+(1.-CWM_K(K))
          TTFC=-CAPA*TFC+1.
          COFF(K)=T_K(K)*TTFC*TFC*DPPL*FFC                              &
                   /((PNP1(K+1)+PNP1(K))*(PNP1(K+1)+PNP1(K)))
        ENDDO
!-----------------------------------------------------------------------
!
        PSTRUP=-(PSTR(KTE+1)+PSTR(KTE)-PONE(KTE+1)-PONE(KTE))*COFF(KTE)
!
!-----------------------------------------------------------------------
        DO K=KTE-1,KOFF+1,-1
          RDPDN=RDPP(K)
          RDPUP=RDPP(K+1)
!
          PSTRDN=-(PSTR(K+1)+PSTR(K)-PONE(K+1)-PONE(K))*COFF(K)
!
          B1(K)=COFF(K+1)+RDPUP
          B2(K)=(COFF(K+1)+COFF(K))-(RDPUP+RDPDN)
          B3(K)=COFF(K)+RDPDN
          C0(K)=PSTRUP+PSTRDN
!
          PSTRUP=PSTRDN
        ENDDO
!-----------------------------------------------------------------------
        B1(KTE-1)=0.
        B2(KOFF+1)=B2(KOFF+1)+B3(KOFF+1)
!-----------------------------------------------------------------------
!
!***  ELIMINATION
!
        DO K=KTE-2,KOFF+1,-1
          TMP=-B1(K)/B2(K+1)
          B2(K)=B3(K+1)*TMP+B2(K)
          C0(K)=C0(K+1)*TMP+C0(K)
        ENDDO
!
        CHI(KTE+1)=0.
!-----------------------------------------------------------------------
!
!***  BACK SUBSTITUTION
!
        CHI(KOFF+2)=C0(KOFF+1)/B2(KOFF+1)
        CHI(KOFF+1)=CHI(KOFF+2)
!
        DO K=KOFF+3,KTE
          CHI(K)=(-B3(K-1)*CHI(K-1)+C0(K-1))/B2(K-1)
        ENDDO
!-----------------------------------------------------------------------
        HBM3IJ=HBM3(I,J)
        DPTU=0.
        FCT=0.5/CP*HBM3IJ
!
        DO K=KTE,KOFF+1,-1
          DPTL=(CHI(K)+PSTR(K)-PINT_K(K))*HBM3IJ
          PINT_K(K)=PINT_K(K)+DPTL
          T_K(K)=(DPTU+DPTL)*RTOP_K(K)*FCT+T_K(K)
          DELP=(PINT_K(K)-PINT_K(K+1))*RDPP(K)
          W_K(K)=((DELP-DWDT_K(K))*GDT+W_K(K))*HBM3IJ
          DWDT_K(K)=(DELP-1.)*HBM3IJ+1.
!
          DPTU=DPTL
        ENDDO
!-----------------------------------------------------------------------
        DO K=KOFF+1,KTE
          PINT(I,K,J)=PINT_K(K)
          T(I,K,J)=T_K(K)
          W(I,K,J)=W_K(K)
          DWDT(I,K,J)=DWDT_K(K)
        ENDDO
!-----------------------------------------------------------------------
!
      ENDDO
!
      ENDDO final_update
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE EPS
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE VADZ(NTSD,DT,FIS,SIGMA,DFL,HTM,HBM2                    &
                     ,DETA1,DETA2,PDTOP                                 &
                     ,PINT,PDSL,PDSLO,PETDT                             &
                     ,RTOP,T,Q,CWM,Z,W,DWDT,PDWDT                       &
                     ,IHE,IHW,IVE,IVW,INDX3_WRK                         &
                     ,IDS,IDE,JDS,JDE,KDS,KDE                           &
                     ,IMS,IME,JMS,JME,KMS,KME                           &
                     ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    VADZ        VERTICAL ADVECTION OF HEIGHT
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-11-17
!     
! ABSTRACT:
!     VADV CALCULATES THE CONTRIBUTION OF THE VERTICAL ADVECTION
!     OF HEIGHT IN ORDER TO COMPUTE W=DZ/DT DIAGNOSTICALLY
!     
! PROGRAM HISTORY LOG:
!   96-05-??  JANJIC     - ORIGINATOR
!   00-01-04  BLACK      - DISTRIBUTED MEMORY AND THREADS
!   01-03-26  BLACK      - CONVERTED TO WRF STRUCTURE
!   02-02-19  BLACK      - CONVERSION UPDATED
!   04-11-22  BLACK      - THREADED
!     
! USAGE: CALL VADZ FROM MAIN PROGRAM
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
#ifdef  AS_RECEIVED
      LOGICAL,INTENT(IN) :: SIGMA
#else
      INTEGER,INTENT(IN) :: SIGMA
#endif
!
      INTEGER,INTENT(IN) :: NTSD
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
!-----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!***  NMM_MAX_DIM is set in configure.wrf and must agree with
!***  the value of dimspec q in the Registry/Registry
!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
!
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      REAL,INTENT(IN) :: DT,PDTOP
!
      REAL,DIMENSION(KTS:KTE),INTENT(IN) :: DETA1,DETA2
!
      REAL,DIMENSION(KTS:KTE+1),INTENT(IN) :: DFL
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: FIS,HBM2,PDSL,PDSLO
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PETDT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: CWM,HTM     &
                                                           ,Q,RTOP,T
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: PDWDT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: DWDT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PINT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: W,Z
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
      INTEGER :: I,J,K
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: FNE,FSE,TTB
!
      REAL :: DZ,RDT,TTAL,ZETA
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      RDT=1./DT
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dz,i,j,k,zeta)
      DO J=MYJS,MYJE
!
        DO K=KTS,KTE
        DO I=MYIS,MYIE
          PDWDT(I,K,J)=DWDT(I,K,J)
          DWDT(I,K,J)=W(I,K,J)
        ENDDO
        ENDDO
!
        DO I=MYIS,MYIE
          W(I,KTS,J)=0.
#ifdef AS_RECEIVED
          IF(SIGMA)THEN
#else
          IF(SIGMA==1)THEN
#endif
            Z(I,KTS,J)=FIS(I,J)*RG
          ELSE
            Z(I,KTS,J)=0.
          ENDIF
        ENDDO
!
        DO K=KTS,KTE
!
          ZETA=DFL(K+1)*RG
!
          DO I=MYIS,MYIE
!
            DZ=(Q(I,K,J)*P608-CWM(I,K,J)+1.)*T(I,K,J)                   &
              /(PINT(I,K+1,J)+PINT(I,K,J))                              &
              *(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))*TRG
            Z(I,K+1,J)=(Z(I,K,J)+DZ-ZETA)*HTM(I,K,J)+ZETA
            W(I,K+1,J)=(DZ-RTOP(I,K,J)                                  &
                      *(DETA1(K)*PDTOP+DETA2(K)*PDSLO(I,J))*RG)         &
                      *HTM(I,K,J)*HBM2(I,J)                             &
                      +W(I,K,J)
!
          ENDDO
        ENDDO
!
      ENDDO
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=MYJS,MYJE
!
        DO K=KTS,KTE
        DO I=MYIS,MYIE
          Z(I,K,J)=(Z(I,K+1,J)+Z(I,K,J))*0.5
          W(I,K,J)=(W(I,K+1,J)+W(I,K,J))*HTM(I,K,J)*HBM2(I,J)*0.5*RDT
        ENDDO
        ENDDO
!
      ENDDO
!-----------------------------------------------------------------------
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        TTB(I,J)=0.
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,ttal)
      DO J=MYJS2,MYJE2
        DO K=KTE,KTS+1,-1
        DO I=MYIS1,MYIE1
          TTAL=(Z(I,K-1,J)-Z(I,K,J))*PETDT(I,K-1,J)*0.5
          W(I,K,J)=(TTAL+TTB(I,J))/(DETA1(K)*PDTOP+DETA2(K)*PDSLO(I,J)) &
                  +W(I,K,J)
          TTB(I,J)=TTAL
        ENDDO
        ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        W(I,KTS,J)=TTB(I,J)/(DETA1(KTS)*PDTOP+DETA2(KTS)*PDSLO(I,J))    &
                  +W(I,KTS,J)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
      END SUBROUTINE VADZ
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE HADZ(NTSD,DT,HYDRO,HTM,HBM2,DETA1,DETA2,PDTOP          &
                     ,DX,DY,FAD                                         &
                     ,FEW,FNS,FNE,FSE                                   &
                     ,PDSL,U,V,W,Z                                      &
                     ,IHE,IHW,IVE,IVW,INDX3_WRK                         &
                     ,IDS,IDE,JDS,JDE,KDS,KDE                           &
                     ,IMS,IME,JMS,JME,KMS,KME                           &
                     ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    HADZ        HORIZONTAL ADVECTION OF HEIGHT
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 96-05-??       
!     
! ABSTRACT:
!     HADZ CALCULATES DIAGNOSTICALLY THE CONTRIBUTION OF
!     THE HORIZONTAL ADVECTION OF HEIGHT
!     
! PROGRAM HISTORY LOG:
!   96-05-??  JANJIC     - ORIGINATOR
!   00-01-04  BLACK      - DISTRIBUTED MEMORY AND THREADS
!   01-03-26  BLACK      - CONVERTED TO WRF STRUCTURE
!   04-11-22  BLACK      - THREADED
!
! USAGE: CALL HADZ FROM MAIN PROGRAM
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT FILES:
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
      LOGICAL,INTENT(IN) :: HYDRO
!
      INTEGER,INTENT(IN) :: NTSD
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
!-----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!***  NMM_MAX_DIM is set in configure.wrf and must agree with
!***  the value of dimspec q in the Registry/Registry
!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
!
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      REAL,INTENT(IN) :: DT,DY,PDTOP
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX,FAD,HBM2,PDSL
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: HTM,U,V
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: FEW,FNE    &
                                                            ,FNS,FSE
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: Z
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: W
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
      INTEGER,PARAMETER :: NTSHY=2
!
      INTEGER :: I,J,J1_00,J1_P1,J1_P2,J4_00,J4_M1,J4_P1,J5_00,J5_M1    &
                ,J6_00,J6_P1,JJ,JKNT,JSTART,K
!
      REAL :: FEWP,FNEP,FNSP,FSEP,UDY,VDX
!
      REAL,DIMENSION(IMS:IME,KTS:KTE) :: UDY_00,ZEW
!
!***  TYPE 1 WORKING ARRAY (SEE PFDHT)
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-2:2) :: DPDE
!
!***  TYPE 4 WORKING ARRAY 
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-1:1) :: UNED,USED,ZNS
!
!***  TYPE 5 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-1:0) :: ZNE
!
!***  TYPE 6 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE, 0:1) :: ZSE
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      IF(NTSD+1<=NTSHY.OR.HYDRO)THEN
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=MYJS,MYJE
          DO K=KTS,KTE
          DO I=MYIS,MYIE
            W(I,K,J)=0.
          ENDDO
          ENDDO
        ENDDO
!***
        RETURN
!***
      ENDIF
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!***  FIRST ZERO OUT SOME WORKING ARRAYS
!
      DO J=-2,2
!$omp parallel do                                                       &
!$omp& private(i,k)
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        DPDE(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=-1,1
!$omp parallel do                                                       &
!$omp& private(i,k)
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        UNED(I,K,J)=0.
        USED(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  MARCH NORTHWARD THROUGH THE SOUTHERNMOST SLABS TO BEGIN
!***  FILLING THE MAIN WORKING ARRAYS WHICH ARE MULTI-DIMENSIONED
!***  IN J BECAUSE THEY ARE DIFFERENCED OR AVERAGED IN J
!-----------------------------------------------------------------------
!
      JSTART=MYJS2_P1
!
      DO J=-2,1
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE
        DO I=MYIS_P4,MYIE_P4
          DPDE(I,K,J)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,JJ)
        ENDDO
        ENDDO
!
      ENDDO
!
      DO J=-1,0
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(fnsp,i,k,udy,vdx)
        DO K=KTS,KTE
        DO I=MYIS_P3,MYIE_P3
          UDY=U(I,K,JJ)*DY
          VDX=V(I,K,JJ)*DX(I,JJ)
          UNED(I,K,J)=UDY+VDX
          USED(I,K,J)=UDY-VDX
          FNSP=VDX*(DPDE(I,K,J-1)+DPDE(I,K,J+1))
          ZNS(I,K,J)=FNSP*(Z(I,K,JJ+1)-Z(I,K,JJ-1))
          FNS(I,K,JJ)=FNSP
          UDY_00(I,K)=UDY
        ENDDO
        ENDDO
!
      ENDDO
!
      J=-1
      JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(fnep,i,k)
      DO K=KTS,KTE
      DO I=MYIS_P2,MYIE_P2
        FNEP=(UNED(I+IHE(JJ),K,J)+UNED(I,K,J+1))                        &
            *(DPDE(I,K,J)+DPDE(I+IHE(JJ),K,J+1))
        ZNE(I,K,J)=FNEP*(Z(I+IHE(JJ),K,JJ+1)-Z(I,K,JJ))
      ENDDO
      ENDDO
!
      J=0
      JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(fsep,i,k)
      DO K=KTS,KTE
      DO I=MYIS_P2,MYIE_P2
        FSEP=(USED(I+IHE(JJ),K,J)+USED(I,K,J-1))                        &
            *(DPDE(I,K,J)+DPDE(I+IHE(JJ),K,J-1))
        ZSE(I,K,J)=FSEP*(Z(I+IHE(JJ),K,JJ-1)-Z(I,K,JJ))
        FSE(I,K,JJ)=FSEP
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
      JKNT=0
!
      main_integration:  DO J=MYJS2_P1,MYJE2_P1
!
!-----------------------------------------------------------------------
!***
!***  SET THE 3RD INDEX IN THE WORKING ARRAYS (SEE SUBROUTINE INIT
!***                                           AND ABOVE DIAGRAMS)
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
        J1_00=INDX3_WRK(0,JKNT,1)
!
        J4_P1=INDX3_WRK(1,JKNT,4)
        J4_00=INDX3_WRK(0,JKNT,4)
        J4_M1=INDX3_WRK(-1,JKNT,4)
!
        J5_00=INDX3_WRK(0,JKNT,5)
        J5_M1=INDX3_WRK(-1,JKNT,5)
!
        J6_P1=INDX3_WRK(1,JKNT,6)
        J6_00=INDX3_WRK(0,JKNT,6)
!-----------------------------------------------------------------------
!
!***  MASS FLUXES AND MASS POINT ADVECTION COMPONENTS
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(fewp,fnep,fnsp,fsep,i,k,udy,vdx)
        DO K=KTS,KTE
!
        DO I=MYIS_P4,MYIE_P4
          DPDE(I,K,J1_P2)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J+2)
        ENDDO
!
        DO I=MYIS_P3,MYIE_P3
          UDY=U(I,K,J+1)*DY
          VDX=V(I,K,J+1)*DX(I,J+1)
!
          FEWP=UDY_00(I,K)                                              &
             *(DPDE(I+IVW(J),K,J1_00)+DPDE(I+IVE(J),K,J1_00))
          FNSP=VDX*(DPDE(I,K,J1_00)+DPDE(I,K,J1_P2))
!
          FEW(I,K,J)=FEWP
          FNS(I,K,J+1)=FNSP
!
          ZEW(I,K)=FEWP*(Z(I+IVE(J),K,J)-Z(I+IVW(J),K,J))
          ZNS(I,K,J4_P1)=FNSP*(Z(I,K,J+2)-Z(I,K,J))
!
          UNED(I,K,J4_P1)=UDY+VDX
          USED(I,K,J4_P1)=UDY-VDX
!
          UDY_00(I,K)=UDY
        ENDDO
!
!-----------------------------------------------------------------------
!
!***  DIAGONAL FLUXES AND DIAGONALLY AVERAGED WIND
!
!-----------------------------------------------------------------------
        DO I=MYIS_P2,MYIE1_P2
          FNEP=(UNED(I+IHE(J),K,J4_00)+UNED(I,K,J4_P1))                 &
              *(DPDE(I,K,J1_00)+DPDE(I+IHE(J),K,J1_P1))
          FNE(I,K,J)=FNEP
          ZNE(I,K,J5_00)=FNEP*(Z(I+IHE(J),K,J+1)-Z(I,K,J))
!
          FSEP=(USED(I+IHE(J+1),K,J4_P1)+USED(I,K,J4_00))               &
              *(DPDE(I,K,J1_P1)+DPDE(I+IHE(J+1),K,J1_00))
          FSE(I,K,J+1)=FSEP
          ZSE(I,K,J6_P1)=FSEP*(Z(I+IHE(J+1),K,J)-Z(I,K,J+1))
        ENDDO
!
!-----------------------------------------------------------------------
!
!***  ADVECTION OF Z
!
!-----------------------------------------------------------------------
        DO I=MYIS1_P1,MYIE1_P1
          W(I,K,J)=-(ZEW(I+IHW(J),K)+ZEW(I+IHE(J),K)                    &
                    +ZNS(I,K,J4_M1)+ZNS(I,K,J4_P1)                      &
                    +ZNE(I+IHW(J),K,J5_M1)+ZNE(I,K,J5_00)               &
                    +ZSE(I,K,J6_00)+ZSE(I+IHW(J),K,J6_P1))              &
                    *FAD(I,J)*HTM(I,K,J)*HBM2(I,J)/(DPDE(I,K,J1_00)*DT) &
                    +W(I,K,J)
        ENDDO
!
        ENDDO   ! End K loop
!-----------------------------------------------------------------------
!
      ENDDO main_integration
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE HADZ
!
!-----------------------------------------------------------------------
      END MODULE MODULE_NONHY_DYNAM
!-----------------------------------------------------------------------
