!----------------------------------------------------------------------
!#define BIT_FOR_BIT
!----------------------------------------------------------------------
#include "nmm_loop_basemacros.h"
#include "nmm_loop_macros.h"
!----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: HORIZONTAL AND VERTICAL ADVECTION
!
!----------------------------------------------------------------------
!
      MODULE MODULE_ADVECTION
!
!----------------------------------------------------------------------
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
      REAL :: CONSERVE_MIN=0.9,CONSERVE_MAX=1.1
!
!----------------------------------------------------------------------
!***  CRANK-NICHOLSON OFF-CENTER WEIGHTS FOR CURRENT AND FUTURE
!***  TIME LEVELS.
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: WGT1=0.90
      REAL,PARAMETER :: WGT2=2.-WGT1
!
!***  FOR CRANK_NICHOLSON CHECK ONLY.
!
      INTEGER :: ITEST=47,JTEST=70
      REAL :: ADTP,ADUP,ADVP,TTLO,TTUP,TULO,TUUP,TVLO,TVUP
!
!----------------------------------------------------------------------
      CONTAINS
!
!*********************************************************************** 
      SUBROUTINE ADVE(NTSD,DT,DETA1,DETA2,PDTOP                         &
     &               ,CURV,F,FAD,F4D,EM_LOC,EMT_LOC,EN,ENT,DX,DY        &
     &               ,HTM,HBM2,VTM,VBM2,LMH,LMV                         &
     &               ,T,U,V,PDSLO,TOLD,UOLD,VOLD                        &
     &               ,PETDT,UPSTRM                                      &
     &               ,FEW,FNS,FNE,FSE                                   &
     &               ,ADT,ADU,ADV                                       &
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
! SUBPROGRAM:    ADVE        HORIZONTAL AND VERTICAL ADVECTION
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-10-28       
!     
! ABSTRACT:
!     ADVE CALCULATES THE CONTRIBUTION OF THE HORIZONTAL AND VERTICAL
!     ADVECTION TO THE TENDENCIES OF TEMPERATURE AND WIND AND THEN
!     UPDATES THOSE VARIABLES.
!     THE JANJIC ADVECTION SCHEME FOR THE ARAKAWA E GRID IS USED
!     FOR ALL VARIABLES INSIDE THE FIFTH ROW.  AN UPSTREAM SCHEME
!     IS USED ON ALL VARIABLES IN THE THIRD, FOURTH, AND FIFTH
!     OUTERMOST ROWS.  THE ADAMS-BASHFORTH TIME SCHEME IS USED.
!     
! PROGRAM HISTORY LOG:
!   87-06-??  JANJIC       - ORIGINATOR
!   95-03-25  BLACK        - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   96-03-28  BLACK        - ADDED EXTERNAL EDGE
!   98-10-30  BLACK        - MODIFIED FOR DISTRIBUTED MEMORY
!   99-07-    JANJIC       - CONVERTED TO ADAMS-BASHFORTH SCHEME
!                            COMBINING HORIZONTAL AND VERTICAL ADVECTION
!   02-02-04  BLACK        - ADDED VERTICAL CFL CHECK
!   02-02-05  BLACK        - CONVERTED TO WRF FORMAT
!   02-08-29  MICHALAKES   - CONDITIONAL COMPILATION OF MPI
!                            CONVERT TO GLOBAL INDEXING
!   02-09-06  WOLFE        - MORE CONVERSION TO GLOBAL INDEXING
!   04-05-29  JANJIC,BLACK - CRANK-NICHOLSON VERTICAL ADVECTION
!   04-11-23  BLACK        - THREADED 
!     
! USAGE: CALL ADVE FROM SUBROUTINE SOLVE_NMM
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
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V         &
     &                                         ,N_IUP_ADH,N_IUP_ADV
      INTEGER, DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V     &
     &                                                 ,IUP_ADH,IUP_ADV &
     &                                                 ,LMH,LMV
!
!***  NMM_MAX_DIM is set in configure.wrf and must agree with
!***  the value of dimspec q in the Registry/Registry
!
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: NTSD
!
      REAL,INTENT(IN) :: DT,DY,EN,ENT,F4D,PDTOP
!
      REAL,DIMENSION(NMM_MAX_DIM),INTENT(IN) :: EM_LOC,EMT_LOC
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CURV,DX,F,FAD,HBM2  &
     &                                             ,PDSLO,VBM2
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PETDT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: HTM,VTM
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: T,TOLD   &
     &                                                        ,U,UOLD   &
     &                                                        ,V,VOLD
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: ADT,ADU    &
     &                                                      ,ADV        &
     &                                                      ,FEW,FNE    &
     &                                                      ,FNS,FSE
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      LOGICAL :: UPSTRM
!
      INTEGER :: I,IEND,IFP,IFQ,II,IPQ,ISP,ISQ,ISTART                   &
     &          ,IUP_ADH_J,IVH,IVL                                      &
     &          ,J,J1,JA,JAK,JEND,JGLOBAL,JJ,JKNT,JP2,JSTART            &
     &          ,K,KNTI_ADH,KSTART,KSTOP,LMHK,LMVK                      &
     &          ,N,N_IUPH_J,N_IUPADH_J,N_IUPADV_J
!
      INTEGER :: MY_IS_GLB,MY_IE_GLB,MY_JS_GLB,MY_JE_GLB
!
      INTEGER :: J0_P3,J0_P2,J0_P1,J0_00,J0_M1,J1_P2,J1_P1,J1_00,J1_M1  &
     &          ,J2_P1,J2_00,J2_M1,J3_P2,J3_P1,J3_00                    &
     &          ,J4_P1,J4_00,J4_M1,J5_00,J5_M1,J6_P1,J6_00
!
      INTEGER,DIMENSION(ITS-5:ITE+5) :: KBOT_CFL_T,KTOP_CFL_T           &
     &                                 ,KBOT_CFL_U,KTOP_CFL_U           &
     &                                 ,KBOT_CFL_V,KTOP_CFL_V
!
      INTEGER,DIMENSION(ITS-5:ITE+5,KTS:KTE) :: ISPA,ISQA
!
      REAL :: ARRAY3_X,CFL,CFT,CFU,CFV,CMT,CMU,CMV                      &
     &       ,DPDE_P3,DTE,DTQ                                           &
     &       ,F0,F1,F2,F3,FEW_00,FEW_P1,FNE_X,FNS_P1,FNS_X,FPP,FSE_X    &
     &       ,HM,PDOP,PDOPU,PDOPV,PP                                    &
     &       ,PVVLO,PVVLOU,PVVLOV,PVVUP,PVVUPU,PVVUPV                   &
     &       ,QP,RDP,RDPD,RDPDX,RDPDY,RDPU,RDPV                         &
     &       ,T_UP,TEMPA,TEMPB,TTA,TTB,U_UP,UDY_P1,UDY_X                &
     &       ,VXD_X,VDX_P2,V_UP,VDX_X,VM,VTA,VUA,VVA                    &
     &       ,VVLO,VVLOU,VVLOV,VVUP,VVUPU,VVUPV
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE) :: ARRAY0,ARRAY1              &
     &                                      ,ARRAY2,ARRAY3              &
     &                                      ,VAD_TEND_T,VAD_TEND_U      &
     &                                      ,VAD_TEND_V
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE) :: TEW,UEW,VEW
!
      REAL,DIMENSION(KTS:KTE) :: CRT,CRU,CRV,DETA1_PDTOP                &
     &                          ,RCMT,RCMU,RCMV,RSTT,RSTU,RSTV,TN,UN    &
     &                          ,VAD_TNDX_T,VAD_TNDX_U,VAD_TNDX_V,VN
!
      REAL,DIMENSION(ITS-5:ITE+5,-1:1) :: PETDTK
!
      REAL,DIMENSION(ITS-5:ITE+5) :: TDN,UDN,VDN
!
!-----------------------------------------------------------------------
!
!***  TYPE 0 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KMS:KME,-3:3) :: DPDE
!
!***  TYPE 1 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KMS:KME,-2:2) :: TST,UDY,UST,VDX,VST
!
!***  TYPE 4 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KMS:KME,-1:1) :: TNS,UNS,VNS
!
!***  TYPE 5 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KMS:KME,-1:0) :: TNE,UNE,VNE
!
!***  TYPE 6 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KMS:KME, 0:1) :: TSE,USE,VSE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***********************************************************************
!
!                         DPDE      -----  3
!                          |                      J Increasing
!                          |                        
!                          |                            ^
!                         FNS       -----  2            |
!                          |                            |
!                          |                            |
!                          |                            |
!                         VNS       -----  1            |
!                          |
!                          |
!                          |
!                         ADV       -----  0  ------> Current J
!                          |
!                          |
!                          |
!                         VNS       ----- -1
!                          |
!                          |
!                          |
!                         FNS       ----- -2
!                          |
!                          |
!                          |
!                         DPDE      ----- -3
!
!***********************************************************************
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ISTART=MYIS_P2
      IEND=MYIE_P2 
      IF(ITE==IDE)IEND=MYIE-3 
!
      DTQ=DT*0.25
      DTE=DT*(0.5*0.25)
!***
!***  INITIALIZE SOME WORKING ARRAYS TO ZERO
!***
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        TEW(I,K)=0.
        UEW(I,K)=0.
        VEW(I,K)=0.
      ENDDO
      ENDDO
!
!***  TYPE 0
!
      DO N=-3,3
        DO K=KTS,KTE
        DO I=ITS-5,ITE+5
          DPDE(I,K,N)=0.
        ENDDO
        ENDDO
      ENDDO
!
!***  TYPE 1
!
      DO N=-2,2
        DO K=KTS,KTE
        DO I=ITS-5,ITE+5
          TST(I,K,N)=0.
          UST(I,K,N)=0.
          VST(I,K,N)=0.
          UDY(I,K,N)=0.
          VDX(I,K,N)=0.
        ENDDO
        ENDDO
      ENDDO
!
!***  TYPES 5 AND 6
!
      DO N=-1,0
        DO K=KTS,KTE
        DO I=ITS-5,ITE+5
          TNE(I,K,N)=0.
          TSE(I,K,N+1)=0.
          UNE(I,K,N)=0.
          USE(I,K,N+1)=0.
          VNE(I,K,N)=0.
          VSE(I,K,N+1)=0.
        ENDDO
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!***
!***  PRECOMPUTE DETA1 TIMES PDTOP.
!***
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
        DETA1_PDTOP(K)=DETA1(K)*PDTOP
      ENDDO
!-----------------------------------------------------------------------
!***
!***  WE NEED THE STARTING AND ENDING J FOR THIS TASK'S INTEGRATION
!***
!-----------------------------------------------------------------------
!
      JSTART=MYJS2
      JEND=MYJE2
!
!-----------------------------------------------------------------------
!
!***  START THE HORIZONTAL ADVECTION IN THE INITIAL SOUTHERN SLABS.
!
!-----------------------------------------------------------------------
!
      DO J=-2,1
        JJ=JSTART+J
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE
        DO I=MYIS_P4,MYIE_P4
          TST(I,K,J)=T(I,K,JJ)*FFC+TOLD(I,K,JJ)*FBC
          UST(I,K,J)=U(I,K,JJ)*FFC+UOLD(I,K,JJ)*FBC
          VST(I,K,J)=V(I,K,JJ)*FFC+VOLD(I,K,JJ)*FBC
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  MARCH NORTHWARD THROUGH THE SOUTHERNMOST SLABS TO BEGIN
!***  FILLING THE MAIN WORKING ARRAYS WHICH ARE MULTI-DIMENSIONED
!***  IN J BECAUSE THEY ARE DIFFERENCED OR AVERAGED IN J.
!***  ONLY THE NORTHERNMOST OF EACH OF THE WORKING ARRAYS WILL BE
!***  FILLED IN THE PRIMARY INTEGRATION SECTION.
!-----------------------------------------------------------------------
!
      J1=-3
      IF(JTS==JDS)J1=-2  ! Cannot go 3 south from J=2 for south tasks
!
      DO J=J1,2
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE
        DO I=MYIS_P4,MYIE_P4
          DPDE(I,K,J)=DETA1_PDTOP(K)+DETA2(K)*PDSLO(I,JJ)
        ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
      DO J=-2,1
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE
        DO I=MYIS_P4,MYIE_P4
          UDY(I,K,J)=U(I,K,JJ)*DY
          VDX_X=V(I,K,JJ)*DX(I,JJ)
          FNS(I,K,JJ)=VDX_X*(DPDE(I,K,J-1)+DPDE(I,K,J+1))
          VDX(I,K,J)=VDX_X
        ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
      DO J=-2,0
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(i,k,tempa)
        DO K=KTS,KTE
        DO I=MYIS_P3,MYIE_P3
          TEMPA=(UDY(I+IHE(JJ),K,J)+VDX(I+IHE(JJ),K,J))                 &
     &         +(UDY(I,K,J+1)      +VDX(I,K,J+1))
          FNE(I,K,JJ)=TEMPA*(DPDE(I,K,J)+DPDE(I+IHE(JJ),K,J+1))
        ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
      DO J=-1,1
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(i,k,tempb)
        DO K=KTS,KTE
        DO I=MYIS_P3,MYIE_P3
          TEMPB=(UDY(I+IHE(JJ),K,J)-VDX(I+IHE(JJ),K,J))                 &
     &         +(UDY(I,K,J-1)      -VDX(I,K,J-1))
          FSE(I,K,JJ)=TEMPB*(DPDE(I,K,J)+DPDE(I+IHE(JJ),K,J-1))
        ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
      DO J=-1,0
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(fns_x,i,k,udy_x)
        DO K=KTS,KTE
        DO I=MYIS1_P3,MYIE1_P3
          FNS_X=FNS(I,K,JJ)
          TNS(I,K,J)=FNS_X*(TST(I,K,J+1)-TST(I,K,J-1))
!
          UDY_X=U(I,K,JJ)*DY
          FEW(I,K,JJ)=UDY_X*(DPDE(I+IVW(JJ),K,J)+DPDE(I+IVE(JJ),K,J))   
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE
        DO I=MYIS1_P4,MYIE1_P4
          UNS(I,K,J)=(FNS(I+IHW(JJ),K,JJ)+FNS(I+IHE(JJ),K,JJ))          &
     &              *(UST(I,K,J+1)-UST(I,K,J-1))
          VNS(I,K,J)=(FNS(I,K,JJ-1)+FNS(I,K,JJ+1))                      &
     &              *(VST(I,K,J+1)-VST(I,K,J-1))
        ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
      JJ=JSTART-1
!
!$omp parallel do                                                       &
!$omp& private(fne_x,fse_x,i,k)
      DO K=KTS,KTE
      DO I=MYIS1_P2,MYIE1_P2
        FNE_X=FNE(I,K,JJ)
        TNE(I,K,-1)=FNE_X*(TST(I+IHE(JJ),K,0)-TST(I,K,-1))
!
        FSE_X=FSE(I,K,JJ+1)
        TSE(I,K,0)=FSE_X*(TST(I+IHE(JJ+1),K,-1)-TST(I,K,0))
!
        UNE(I,K,-1)=(FNE(I+IVW(JJ),K,JJ)+FNE(I+IVE(JJ),K,JJ))           &
     &             *(UST(I+IVE(JJ),K,0)-UST(I,K,-1))
        USE(I,K,0)=(FSE(I+IVW(JJ+1),K,JJ+1)+FSE(I+IVE(JJ+1),K,JJ+1))    &
     &            *(UST(I+IVE(JJ+1),K,-1)-UST(I,K,0))
        VNE(I,K,-1)=(FNE(I,K,JJ-1)+FNE(I,K,JJ+1))                       &
     &             *(VST(I+IVE(JJ),K,0)-VST(I,K,-1))
        VSE(I,K,0)=(FSE(I,K,JJ)+FSE(I,K,JJ+2))                          &
     &            *(VST(I+IVE(JJ+1),K,-1)-VST(I,K,0))
      ENDDO
      ENDDO
!
      JKNT=0
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      main_integration : DO J=JSTART,JEND
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
!-----------------------------------------------------------------------
!
      JKNT=JKNT+1
!
      J0_P3=INDX3_WRK(3,JKNT,0)
      J0_P2=INDX3_WRK(2,JKNT,0)
      J0_P1=INDX3_WRK(1,JKNT,0)
      J0_00=INDX3_WRK(0,JKNT,0)
      J0_M1=INDX3_WRK(-1,JKNT,0)
!
      J1_P2=INDX3_WRK(2,JKNT,1)
      J1_P1=INDX3_WRK(1,JKNT,1)
      J1_00=INDX3_WRK(0,JKNT,1)
      J1_M1=INDX3_WRK(-1,JKNT,1)
!
      J2_P1=INDX3_WRK(1,JKNT,2)
      J2_00=INDX3_WRK(0,JKNT,2)
      J2_M1=INDX3_WRK(-1,JKNT,2)
!
      J3_P2=INDX3_WRK(2,JKNT,3)
      J3_P1=INDX3_WRK(1,JKNT,3)
      J3_00=INDX3_WRK(0,JKNT,3)
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
      MY_IS_GLB=1  ! make this a noop for global indexing
      MY_IE_GLB=1  ! make this a noop for global indexing
      MY_JS_GLB=1  ! make this a noop for global indexing
      MY_JE_GLB=1  ! make this a noop for global indexing
  
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(dpde_p3,few_00,fne_x,fns_p1,fse_x,i,k,tempa,tempb        &
!$omp&        ,udy_p1,vdx_p2)
      vertical_loop_1 : DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!***  EXECUTE HORIZONTAL ADVECTION.
!-----------------------------------------------------------------------
!
      DO I=MYIS_P4,MYIE_P4
        TST(I,K,J1_P2)=T(I,K,J+2)*FFC+TOLD(I,K,J+2)*FBC
        UST(I,K,J1_P2)=U(I,K,J+2)*FFC+UOLD(I,K,J+2)*FBC
        VST(I,K,J1_P2)=V(I,K,J+2)*FFC+VOLD(I,K,J+2)*FBC
      ENDDO
!
!-----------------------------------------------------------------------
!***  MASS FLUXES AND MASS POINT ADVECTION COMPONENTS
!-----------------------------------------------------------------------
!
      DO I=MYIS_P4,MYIE_P4
!
!-----------------------------------------------------------------------
!***  THE NS AND EW FLUXES IN THE FOLLOWING LOOP ARE ON V POINTS
!***  FOR T.
!-----------------------------------------------------------------------
!
        DPDE_P3=DETA1_PDTOP(K)+DETA2(K)*PDSLO(I,J+3)
        DPDE(I,K,J0_P3)=DPDE_P3
!
!-----------------------------------------------------------------------
        UDY(I,K,J1_P2)=U(I,K,J+2)*DY
        VDX_P2=V(I,K,J+2)*DX(I,J+2)
        VDX(I,K,J1_P2)=VDX_P2
        FNS(I,K,J+2)=VDX_P2*(DPDE(I,K,J0_P1)+DPDE_P3)
      ENDDO
!
!-----------------------------------------------------------------------
      DO I=MYIS_P3,MYIE_P3
        TEMPA=(UDY(I+IHE(J+1),K,J1_P1)+VDX(I+IHE(J+1),K,J1_P1))         &
     &       +(UDY(I,K,J1_P2)         +VDX(I,K,J1_P2))
        FNE(I,K,J+1)=TEMPA*(DPDE(I,K,J0_P1)+DPDE(I+IHE(J+1),K,J0_P2))
!
!-----------------------------------------------------------------------
        TEMPB=(UDY(I+IHE(J+2),K,J1_P2)-VDX(I+IHE(J+2),K,J1_P2))         &
     &       +(UDY(I,K,J1_P1)         -VDX(I,K,J1_P1))
        FSE(I,K,J+2)=TEMPB*(DPDE(I,K,J0_P2)+DPDE(I+IHE(J),K,J0_P1))
!
!-----------------------------------------------------------------------
        FNS_P1=FNS(I,K,J+1)
        TNS(I,K,J4_P1)=FNS_P1*(TST(I,K,J1_P2)-TST(I,K,J1_00))
!
!-----------------------------------------------------------------------
        UDY_P1=U(I,K,J+1)*DY
        FEW(I,K,J+1)=UDY_P1*(DPDE(I+IVW(J+1),K,J0_P1)                   &
     &                        +DPDE(I+IVE(J+1),K,J0_P1))
        FEW_00=FEW(I,K,J)
        TEW(I,K)=FEW_00*(TST(I+IVE(J),K,J1_00)-TST(I+IVW(J),K,J1_00))
!
!-----------------------------------------------------------------------
!***  THE NE AND SE FLUXES ARE ASSOCIATED WITH H POINTS
!***  (ACTUALLY JUST TO THE NE AND SE OF EACH H POINT).
!-----------------------------------------------------------------------
!
        FNE_X=FNE(I,K,J)
        TNE(I,K,J5_00)=FNE_X*(TST(I+IHE(J),K,J1_P1)-TST(I,K,J1_00))
!
        FSE_X=FSE(I,K,J+1)
        TSE(I,K,J6_P1)=FSE_X*(TST(I+IHE(J+1),K,J1_00)-TST(I,K,J1_P1))
      ENDDO
!
!-----------------------------------------------------------------------
!***  CALCULATION OF MOMENTUM ADVECTION COMPONENTS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  THE NS AND EW FLUXES ARE ON H POINTS FOR U AND V.
!-----------------------------------------------------------------------
!
      DO I=MYIS_P2,MYIE_P2
        UEW(I,K)=(FEW(I+IHW(J),K,J)+FEW(I+IHE(J),K,J))                  &
     &          *(UST(I+IHE(J),K,J1_00)-UST(I+IHW(J),K,J1_00))
        UNS(I,K,J4_P1)=(FNS(I+IHW(J+1),K,J+1)                           &
     &                 +FNS(I+IHE(J+1),K,J+1))                          &
     &                *(UST(I,K,J1_P2)-UST(I,K,J1_00))
        VEW(I,K)=(FEW(I,K,J-1)+FEW(I,K,J+1))                            &
     &          *(VST(I+IHE(J),K,J1_00)-VST(I+IHW(J),K,J1_00))
        VNS(I,K,J4_P1)=(FNS(I,K,J)+FNS(I,K,J+2))                        &
     &                *(VST(I,K,J1_P2)-VST(I,K,J1_00))
!
!-----------------------------------------------------------------------
!***  THE FOLLOWING NE AND SE FLUXES ARE TIED TO V POINTS AND ARE
!***  LOCATED JUST TO THE NE AND SE OF THE GIVEN I,J.
!-----------------------------------------------------------------------
!
        UNE(I,K,J5_00)=(FNE(I+IVW(J),K,J)+FNE(I+IVE(J),K,J))            &
     &                *(UST(I+IVE(J),K,J1_P1)-UST(I,K,J1_00))
        USE(I,K,J6_P1)=(FSE(I+IVW(J+1),K,J+1)                           &
     &                 +FSE(I+IVE(J+1),K,J+1))                          &
     &                *(UST(I+IVE(J+1),K,J1_00)-UST(I,K,J1_P1))
        VNE(I,K,J5_00)=(FNE(I,K,J-1)+FNE(I,K,J+1))                      &
     &                *(VST(I+IVE(J),K,J1_P1)-VST(I,K,J1_00))
        VSE(I,K,J6_P1)=(FSE(I,K,J)+FSE(I,K,J+2))                        &
     &                *(VST(I+IVE(J+1),K,J1_00)-VST(I,K,J1_P1))
      ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO vertical_loop_1
!
!-----------------------------------------------------------------------
!***  COMPUTE THE ADVECTION TENDENCIES FOR T.
!***  THE AD ARRAYS ARE ON H POINTS.
!***  SKIP TO UPSTREAM IF THESE ROWS HAVE ONLY UPSTREAM POINTS.
!-----------------------------------------------------------------------
!
      
      JGLOBAL=J+MY_JS_GLB-1
      IF(JGLOBAL>=6.AND.JGLOBAL<=JDE-5)THEN
!
        JJ=J+MY_JS_GLB-1   ! okay because MY_JS_GLB is 1
        IF(ITS==IDS)ISTART=3+MOD(JJ,2)  ! need to think about this
                                        ! more in terms of how to 
                                        ! convert to global indexing
!
!$omp parallel do                                                       &
!$omp& private(i,k,rdpd)
        DO K=KTS,KTE
        DO I=ISTART,IEND
          RDPD=1./DPDE(I,K,J0_00)
!
          ADT(I,K,J)=(TEW(I+IHW(J),K)+TEW(I+IHE(J),K)                   &
     &               +TNS(I,K,J4_M1)+TNS(I,K,J4_P1)                     &
     &               +TNE(I+IHW(J),K,J5_M1)+TNE(I,K,J5_00)              &
     &               +TSE(I,K,J6_00)+TSE(I+IHW(J),K,J6_P1))             &
     &               *RDPD*FAD(I,J)
!
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  COMPUTE THE ADVECTION TENDENCIES FOR U AND V.
!***  THE AD ARRAYS ARE ON VELOCITY POINTS.
!-----------------------------------------------------------------------
!
        IF(ITS==IDS)ISTART=3+MOD(JJ+1,2)
!
!$omp parallel do                                                       &
!$omp& private(i,k,rdpdx,rdpdy)
        DO K=KTS,KTE
        DO I=ISTART,IEND
          RDPDX=1./(DPDE(I+IVW(J),K,J0_00)+DPDE(I+IVE(J),K,J0_00))
          RDPDY=1./(DPDE(I,K,J0_M1)+DPDE(I,K,J0_P1))
!
          ADU(I,K,J)=(UEW(I+IVW(J),K)+UEW(I+IVE(J),K)                   &
     &               +UNS(I,K,J4_M1)+UNS(I,K,J4_P1)                     &
     &               +UNE(I+IVW(J),K,J5_M1)+UNE(I,K,J5_00)              &
     &               +USE(I,K,J6_00)+USE(I+IVW(J),K,J6_P1))             &
     &               *RDPDX*FAD(I+IVW(J),J)
!
          ADV(I,K,J)=(VEW(I+IVW(J),K)+VEW(I+IVE(J),K)                   &
     &               +VNS(I,K,J4_M1)+VNS(I,K,J4_P1)                     &
     &               +VNE(I+IVW(J),K,J5_M1)+VNE(I,K,J5_00)              &
     &               +VSE(I,K,J6_00)+VSE(I+IVW(J),K,J6_P1))             &
     &               *RDPDY*FAD(I+IVW(J),J)
!
        ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!***  END OF JANJIC HORIZONTAL ADVECTION 
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  UPSTREAM ADVECTION OF T, U, AND V
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      upstream : IF(UPSTRM)THEN
!
!-----------------------------------------------------------------------
!***
!***  COMPUTE UPSTREAM COMPUTATIONS ON THIS TASK'S ROWS.
!***
!-----------------------------------------------------------------------
!
          N_IUPH_J=N_IUP_H(J)   ! See explanation in INIT
!
!$omp parallel do                                                       &
!$omp& private(array3_x,i,k,pp,qp,tta,ttb)
          DO K=KTS,KTE
!
            DO II=0,N_IUPH_J-1
              I=IUP_H(IMS+II,J)
              TTA=EMT_LOC(J)*(UST(I,K,J1_M1)+UST(I+IHW(J),K,J1_00)      &
     &                       +UST(I+IHE(J),K,J1_00)+UST(I,K,J1_P1))
              TTB=ENT       *(VST(I,K,J1_M1)+VST(I+IHW(J),K,J1_00)      &
     &                       +VST(I+IHE(J),K,J1_00)+VST(I,K,J1_P1))
              PP=-TTA-TTB
              QP= TTA-TTB
!
              IF(PP<0.)THEN
                ISPA(I,K)=-1
              ELSE
                ISPA(I,K)= 1
              ENDIF
!
              IF(QP<0.)THEN
                ISQA(I,K)=-1
              ELSE
                ISQA(I,K)= 1
              ENDIF
!
              PP=ABS(PP)
              QP=ABS(QP)
              ARRAY3_X=PP*QP
              ARRAY0(I,K)=ARRAY3_X-PP-QP
              ARRAY1(I,K)=PP-ARRAY3_X
              ARRAY2(I,K)=QP-ARRAY3_X
              ARRAY3(I,K)=ARRAY3_X
            ENDDO
!
          ENDDO
!-----------------------------------------------------------------------
!
          N_IUPADH_J=N_IUP_ADH(J) 
!
!$omp parallel do                                                       &
!$omp& private(f0,f1,f2,f3,i,ifp,ifq,ipq,isp,isq,iup_adh_j,k,knti_adh)
          DO K=KTS,KTE
!
            KNTI_ADH=1
            IUP_ADH_J=IUP_ADH(IMS,J)
!
            DO II=0,N_IUPH_J-1
              I=IUP_H(IMS+II,J)
!
              ISP=ISPA(I,K)
              ISQ=ISQA(I,K)
              IFP=(ISP-1)/2
              IFQ=(-ISQ-1)/2
              IPQ=(ISP-ISQ)/2
!
              IF(HTM(I+IHE(J)+IFP,K,J+ISP)                              &
     &          *HTM(I+IHE(J)+IFQ,K,J+ISQ)                              &
     &          *HTM(I+IPQ,K,J+ISP+ISQ)>0.1)THEN
                 GO TO 150
              ENDIF
!
              IF(HTM(I+IHE(J)+IFP,K,J+ISP)                              &
     &          +HTM(I+IHE(J)+IFQ,K,J+ISQ)                              &
     &          +HTM(I+IPQ,K,J+ISP+ISQ)<0.1)THEN 
!
                T(I+IHE(J)+IFP,K,J+ISP)=T(I,K,J)
                T(I+IHE(J)+IFQ,K,J+ISQ)=T(I,K,J)
                T(I+IPQ,K,J+ISP+ISQ)=T(I,K,J)
!
              ELSEIF                                                    &
     &        (HTM(I+IHE(J)+IFP,K,J+ISP)+HTM(I+IPQ,K,J+ISP+ISQ)         &
     &         <0.99)THEN
!
                T(I+IHE(J)+IFP,K,J+ISP)=T(I,K,J)
                T(I+IPQ,K,J+ISP+ISQ)=T(I+IHE(J)+IFQ,K,J+ISQ)
!
              ELSEIF                                                    &
     &        (HTM(I+IHE(J)+IFQ,K,J+ISQ)+HTM(I+IPQ,K,J+ISP+ISQ)         &
               <0.99)THEN
!
                T(I+IHE(J)+IFQ,K,J+ISQ)=T(I,K,J)
                T(I+IPQ,K,J+ISP+ISQ)=T(I+IHE(J)+IFP,K,J+ISP)
!
              ELSEIF                                                    &
     &        (HTM(I+IHE(J)+IFP,K,J+ISP)                                &
     &        +HTM(I+IHE(J)+IFQ,K,J+ISQ)<0.99)THEN
                T(I+IHE(J)+IFP,K,J+ISP)=0.5*(T(I,K,J)                   &
     &                                      +T(I+IPQ,K,J+ISP+ISQ))
                T(I+IHE(J)+IFQ,K,J+ISQ)=T(I+IHE(J)+IFP,K,J+ISP)
!
              ELSEIF(HTM(I+IHE(J)+IFP,K,J+ISP)<0.99)THEN
                T(I+IHE(J)+IFP,K,J+ISP)=T(I,K,J)                        &
     &                                 +T(I+IPQ,K,J+ISP+ISQ)            &
     &                                 -T(I+IHE(J)+IFQ,K,J+ISQ)
!
              ELSEIF(HTM(I+IHE(J)+IFQ,K,J+ISQ)<0.99)THEN
                T(I+IHE(J)+IFQ,K,J+ISQ)=T(I,K,J)                        &
     &                                 +T(I+IPQ,K,J+ISP+ISQ)            &
     &                                 -T(I+IHE(J)+IFP,K,J+ISP)
!
              ELSE
                T(I+IPQ,K,J+ISP+ISQ)=T(I+IHE(J)+IFP,K,J+ISP)            &
     &                              +T(I+IHE(J)+IFQ,K,J+ISQ)            &
     &                              -T(I,K,J)
!
              ENDIF
!
  150         CONTINUE
!
!-----------------------------------------------------------------------
!
              IF(I==IUP_ADH_J)THEN  ! Update advection H tendencies
!
                ISP=ISPA(I,K)
                ISQ=ISQA(I,K)
                IFP=(ISP-1)/2
                IFQ=(-ISQ-1)/2
                IPQ=(ISP-ISQ)/2
!
                F0=ARRAY0(I,K)
                F1=ARRAY1(I,K)
                F2=ARRAY2(I,K)
                F3=ARRAY3(I,K)
!
                ADT(I,K,J)=F0*T(I,K,J)                                  &
     &                    +F1*T(I+IHE(J)+IFP,K,J+ISP)                   &
     &                    +F2*T(I+IHE(J)+IFQ,K,J+ISQ)                   &
                          +F3*T(I+IPQ,K,J+ISP+ISQ)
!
!-----------------------------------------------------------------------
!
                IF(KNTI_ADH<N_IUPADH_J)THEN
                  IUP_ADH_J=IUP_ADH(IMS+KNTI_ADH,J)
                  KNTI_ADH=KNTI_ADH+1
                ENDIF
!
              ENDIF  ! End of advection H tendency IF block
!
            ENDDO  ! End of II loop
!
          ENDDO  ! End of K loop
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  UPSTREAM ADVECTION OF VELOCITY COMPONENTS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
          N_IUPADV_J=N_IUP_ADV(J)
!
!$omp parallel do                                                       &
!$omp& private(f0,f1,f2,f3,i,ifp,ifq,ipq,isp,isq,k,pp,qp,tta,ttb)
          DO K=KTS,KTE
!
            DO II=0,N_IUPADV_J-1
              I=IUP_ADV(IMS+II,J)
!
              TTA=EM_LOC(J)*UST(I,K,J1_00)
              TTB=EN       *VST(I,K,J1_00)
              PP=-TTA-TTB
              QP=TTA-TTB
!
              IF(PP<0.)THEN
                ISP=-1
              ELSE
                ISP= 1
              ENDIF
!
              IF(QP<0.)THEN
                ISQ=-1
              ELSE
                ISQ= 1
              ENDIF
!
              IFP=(ISP-1)/2
              IFQ=(-ISQ-1)/2
              IPQ=(ISP-ISQ)/2
              PP=ABS(PP)
              QP=ABS(QP)
              F3=PP*QP
              F0=F3-PP-QP
              F1=PP-F3
              F2=QP-F3
!
              ADU(I,K,J)=F0*U(I,K,J)                                    &
     &                  +F1*U(I+IVE(J)+IFP,K,J+ISP)                     &
     &                  +F2*U(I+IVE(J)+IFQ,K,J+ISQ)                     &
     &                  +F3*U(I+IPQ,K,J+ISP+ISQ)
! 
              ADV(I,K,J)=F0*V(I,K,J)                                    &
     &                  +F1*V(I+IVE(J)+IFP,K,J+ISP)                     &
     &                  +F2*V(I+IVE(J)+IFQ,K,J+ISQ)                     &
     &                  +F3*V(I+IPQ,K,J+ISP+ISQ)
!
            ENDDO
!
          ENDDO  !  End of K loop
!
!-----------------------------------------------------------------------
!
        ENDIF upstream
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  END OF THIS UPSTREAM REGION
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!***  COMPUTE VERTICAL ADVECTION TENDENCIES USING CRANK-NICHOLSON.
!
!-----------------------------------------------------------------------
!***  FIRST THE TEMPERATURE
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(cft,cmt,crt,i,k,lmhk,pdop,pvvlo,pvvup,rcmt,rdp,rstt,tn   &
!$omp&        ,vvlo,vvup                                                &
!!!$omp&        ,adtp,ttlo,ttup                                           &
!$omp&        )
      iloop_for_t:  DO I=MYIS1,MYIE1
!
        PDOP=PDSLO(I,J)
        PVVLO=PETDT(I,KTE-1,J)*DTQ
        VVLO=PVVLO/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOP)
        CMT=-VVLO*WGT2+1.
        RCMT(KTE)=1./CMT
        CRT(KTE)=VVLO*WGT2
        RSTT(KTE)=-VVLO*WGT1*(T(I,KTE-1,J)-T(I,KTE,J))+T(I,KTE,J)
!
        LMHK=KTE-LMH(I,J)+1
        DO K=KTE-1,LMHK+1,-1
          RDP=1./(DETA1_PDTOP(K)+DETA2(K)*PDOP)
          PVVUP=PVVLO
          PVVLO=PETDT(I,K-1,J)*DTQ
          VVUP=PVVUP*RDP
          VVLO=PVVLO*RDP
          CFT=-VVUP*WGT2*RCMT(K+1)
          CMT=-CRT(K+1)*CFT+((VVUP-VVLO)*WGT2+1.)
          RCMT(K)=1./CMT
          CRT(K)=VVLO*WGT2
          RSTT(K)=-RSTT(K+1)*CFT+T(I,K,J)                               &
     &            -(T(I,K,J)-T(I,K+1,J))*VVUP*WGT1                      &
     &            -(T(I,K-1,J)-T(I,K,J))*VVLO*WGT1
        ENDDO
!
        PVVUP=PVVLO
        VVUP=PVVUP/(DETA1_PDTOP(LMHK)+DETA2(LMHK)*PDOP)
        CFT=-VVUP*WGT2*RCMT(LMHK+1)
        CMT=-CRT(LMHK+1)*CFT+VVUP*WGT2+1.
        CRT(LMHK)=0.
        RSTT(LMHK)=-(T(I,LMHK,J)-T(I,LMHK+1,J))*VVUP*WGT1               &
     &               -RSTT(LMHK+1)*CFT+T(I,LMHK,J)
        TN(LMHK)=RSTT(LMHK)/CMT
        VAD_TEND_T(I,LMHK)=TN(LMHK)-T(I,LMHK,J)
!
        DO K=LMHK+1,KTE
          TN(K)=(-CRT(K)*TN(K-1)+RSTT(K))*RCMT(K)
          VAD_TEND_T(I,K)=TN(K)-T(I,K,J)
        ENDDO
!
!-----------------------------------------------------------------------
!***  The following section is only for checking the implicit solution
!***  using back-substitution.  Remove this section otherwise.
!-----------------------------------------------------------------------
!       if(ntsd<=10.or.ntsd>=6000)then
!       IF(I==ITEST.AND.J==JTEST)THEN
!!
!         PVVLO=PETDT(I,KTE-1,J)*DT*0.25
!         VVLO=PVVLO/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOP)
!         TTLO=VVLO*(T(I,KTE-1,J)-T(I,KTE,J)                            &
!    &              +TN(KTE-1)-TN(KTE))
!         ADTP=TTLO+TN(KTE)-T(I,KTE,J)
!         WRITE(0,*)' NTSD=',NTSD,' I=',ITEST,' J=',JTEST,' K=',KTE     &
!    &,             ' ADTP=',ADTP
!         WRITE(0,*)' T=',T(I,KTE,J),' TN=',TN(KTE)                     &
!    &,               ' VAD_TEND_T=',VAD_TEND_T(I,KTE)
!         WRITE(0,*)' '
!!
!         DO K=KTE-1,LMHK+1,-1
!           RDP=1./(DETA1_PDTOP(K)+DETA2(K)*PDOP)
!           PVVUP=PVVLO
!           PVVLO=PETDT(I,K-1,J)*DT*0.25
!           VVUP=PVVUP*RDP
!           VVLO=PVVLO*RDP
!           TTUP=VVUP*(T(I,K,J)-T(I,K+1,J)+TN(K)-TN(K+1))
!           TTLO=VVLO*(T(I,K-1,J)-T(I,K,J)+TN(K-1)-TN(K))
!           ADTP=TTLO+TTUP+TN(K)-T(I,K,J)
!           WRITE(0,*)' NTSD=',NTSD,' I=',I,' J=',J,' K=',K             &
!    &,               ' ADTP=',ADTP
!           WRITE(0,*)' T=',T(I,K,J),' TN=',TN(K)                       &
!    &,               ' VAD_TEND_T=',VAD_TEND_T(I,K)
!           WRITE(0,*)' '
!         ENDDO
!!
!         IF(LMHK==KTS)THEN
!           PVVUP=PVVLO
!           VVUP=PVVUP/(DETA1_PDTOP(KTS)+DETA2(KTS)*PDOP)
!           TTUP=VVUP*(T(I,KTS,J)-T(I,KTS+1,J)+TN(KTS)-TN(KTS+1))
!           ADTP=TTUP+TN(KTS)-T(I,KTS,J)
!           WRITE(0,*)' NTSD=',NTSD,' I=',I,' J=',J,' K=',KTS           &
!    &,               ' ADTP=',ADTP
!           WRITE(0,*)' T=',T(I,KTS,J),' TN=',TN(KTS)                   &
!    &,               ' VAD_TEND_T=',VAD_TEND_T(I,KTS)
!           WRITE(0,*)' '
!         ENDIF
!       ENDIF
!       endif
!
!-----------------------------------------------------------------------
!***  End of check.
!-----------------------------------------------------------------------
!
      ENDDO iloop_for_t
!
!-----------------------------------------------------------------------
!***  NOW VERTICAL ADVECTION OF WIND COMPONENTS
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(cfu,cfv,cmu,cmv,cru,crv,i,k,lmvk,pdopu,pdopv             &
!$omp&        ,pvvlou,pvvlov,pvvupu,pvvupv,rcmu,rcmv,rdpu,rdpv          &
!$omp&        ,rstu,rstv,un,vn,vvlou,vvlov,vvupu,vvupv                  &
!!!$omp&        ,adup,advp,tulo,tuup,tvlo,tvup                            &
!$omp&         ) 
      iloop_for_uv:  DO I=MYIS1,MYIE1
!
        PDOPU=(PDSLO(I+IVW(J),J)+PDSLO(I+IVE(J),J))*0.5
        PDOPV=(PDSLO(I,J-1)+PDSLO(I,J+1))*0.5
        PVVLOU=(PETDT(I+IVW(J),KTE-1,J)+PETDT(I+IVE(J),KTE-1,J))*DTE
        PVVLOV=(PETDT(I,KTE-1,J-1)+PETDT(I,KTE-1,J+1))*DTE
        VVLOU=PVVLOU/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOPU)
        VVLOV=PVVLOV/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOPV)
        CMU=-VVLOU*WGT2+1.
        CMV=-VVLOV*WGT2+1.
        RCMU(KTE)=1./CMU
        RCMV(KTE)=1./CMV
        CRU(KTE)=VVLOU*WGT2
        CRV(KTE)=VVLOV*WGT2
        RSTU(KTE)=-VVLOU*WGT1*(U(I,KTE-1,J)-U(I,KTE,J))+U(I,KTE,J)
        RSTV(KTE)=-VVLOV*WGT1*(V(I,KTE-1,J)-V(I,KTE,J))+V(I,KTE,J)
!
        LMVK=KTE-LMV(I,J)+1
        DO K=KTE-1,LMVK+1,-1
          RDPU=1./(DETA1_PDTOP(K)+DETA2(K)*PDOPU)
          RDPV=1./(DETA1_PDTOP(K)+DETA2(K)*PDOPV)
          PVVUPU=PVVLOU
          PVVUPV=PVVLOV
          PVVLOU=(PETDT(I+IVW(J),K-1,J)+PETDT(I+IVE(J),K-1,J))*DTE
          PVVLOV=(PETDT(I,K-1,J-1)+PETDT(I,K-1,J+1))*DTE
          VVUPU=PVVUPU*RDPU
          VVUPV=PVVUPV*RDPV
          VVLOU=PVVLOU*RDPU
          VVLOV=PVVLOV*RDPV
          CFU=-VVUPU*WGT2*RCMU(K+1)
          CFV=-VVUPV*WGT2*RCMV(K+1)
          CMU=-CRU(K+1)*CFU+(VVUPU-VVLOU)*WGT2+1.
          CMV=-CRV(K+1)*CFV+(VVUPV-VVLOV)*WGT2+1.
          RCMU(K)=1./CMU
          RCMV(K)=1./CMV
          CRU(K)=VVLOU*WGT2
          CRV(K)=VVLOV*WGT2
          RSTU(K)=-RSTU(K+1)*CFU+U(I,K,J)                               &
     &            -(U(I,K,J)-U(I,K+1,J))*VVUPU*WGT1                     &
     &            -(U(I,K-1,J)-U(I,K,J))*VVLOU*WGT1
          RSTV(K)=-RSTV(K+1)*CFV+V(I,K,J)                               &
     &            -(V(I,K,J)-V(I,K+1,J))*VVUPV*WGT1                     &
     &            -(V(I,K-1,J)-V(I,K,J))*VVLOV*WGT1
        ENDDO
!
        RDPU=1./(DETA1_PDTOP(LMVK)+DETA2(LMVK)*PDOPU)
        RDPV=1./(DETA1_PDTOP(LMVK)+DETA2(LMVK)*PDOPV)
        PVVUPU=PVVLOU
        PVVUPV=PVVLOV
        VVUPU=PVVUPU*RDPU
        VVUPV=PVVUPV*RDPV
        CFU=-VVUPU*WGT2*RCMU(LMVK+1)
        CFV=-VVUPV*WGT2*RCMV(LMVK+1)
        CMU=-CRU(LMVK+1)*CFU+VVUPU*WGT2+1.
        CMV=-CRV(LMVK+1)*CFV+VVUPV*WGT2+1.
        CRU(LMVK)=0.
        CRV(LMVK)=0.
        RSTU(LMVK)=-(U(I,LMVK,J)-U(I,LMVK+1,J))*VVUPU*WGT1              &
     &               -RSTU(LMVK+1)*CFU+U(I,LMVK,J)
        RSTV(LMVK)=-(V(I,LMVK,J)-V(I,LMVK+1,J))*VVUPV*WGT1              &
     &               -RSTV(LMVK+1)*CFV+V(I,LMVK,J)
        UN(LMVK)=RSTU(LMVK)/CMU
        VN(LMVK)=RSTV(LMVK)/CMV
        VAD_TEND_U(I,LMVK)=UN(LMVK)-U(I,LMVK,J)
        VAD_TEND_V(I,LMVK)=VN(LMVK)-V(I,LMVK,J)
!
        DO K=LMVK+1,KTE
          UN(K)=(-CRU(K)*UN(K-1)+RSTU(K))*RCMU(K)
          VN(K)=(-CRV(K)*VN(K-1)+RSTV(K))*RCMV(K)
          VAD_TEND_U(I,K)=UN(K)-U(I,K,J)
          VAD_TEND_V(I,K)=VN(K)-V(I,K,J)
        ENDDO
!
!-----------------------------------------------------------------------
!***  The following section is only for checking the implicit solution
!***  using back-substitution.  Remove this section otherwise.
!-----------------------------------------------------------------------
!
!       if(ntsd<=10.or.ntsd>=6000)then
!       IF(I==ITEST.AND.J==JTEST)THEN
!!
!         PDOPU=(PDSLO(I+IVW(J),J)+PDSLO(I+IVE(J),J))*0.5
!         PDOPV=(PDSLO(I,J-1)+PDSLO(I,J+1))*0.5
!         PVVLOU=(PETDT(I+IVW(J),KTE-1,J)                               &
!    &           +PETDT(I+IVE(J),KTE-1,J))*DTE
!         PVVLOV=(PETDT(I,KTE-1,J-1)                                    &
!    &           +PETDT(I,KTE-1,J+1))*DTE
!         VVLOU=PVVLOU/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOPU)
!         VVLOV=PVVLOV/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOPV)
!         TULO=VVLOU*(U(I,KTE-1,J)-U(I,KTE,J)+UN(KTE-1)-UN(KTE))
!         TVLO=VVLOV*(V(I,KTE-1,J)-V(I,KTE,J)+VN(KTE-1)-VN(KTE))
!         ADUP=TULO+UN(KTE)-U(I,KTE,J)
!         ADVP=TVLO+VN(KTE)-V(I,KTE,J)
!         WRITE(0,*)' NTSD=',NTSD,' I=',I,' J=',J,' K=',KTE             &
!    &,             ' ADUP=',ADUP,' ADVP=',ADVP
!         WRITE(0,*)' U=',U(I,KTE,J),' UN=',UN(KTE)                     &
!    &,             ' VAD_TEND_U=',VAD_TEND_U(I,KTE)                    &
!    &,             ' V=',V(I,KTE,J),' VN=',VN(KTE)                     &
!    &,             ' VAD_TEND_V=',VAD_TEND_V(I,KTE)
!         WRITE(0,*)' '
!!
!         DO K=KTE-1,LMVK+1,-1
!           RDPU=1./(DETA1_PDTOP(K)+DETA2(K)*PDOPU)
!           RDPV=1./(DETA1_PDTOP(K)+DETA2(K)*PDOPV)
!           PVVUPU=PVVLOU
!           PVVUPV=PVVLOV
!           PVVLOU=(PETDT(I+IVW(J),K-1,J)                               &
!    &            +PETDT(I+IVE(J),K-1,J))*DTE
!           PVVLOV=(PETDT(I,K-1,J-1)+PETDT(I,K-1,J+1))*DTE
!           VVUPU=PVVUPU*RDPU
!           VVUPV=PVVUPV*RDPV
!           VVLOU=PVVLOU*RDPU
!           VVLOV=PVVLOV*RDPV
!           TUUP=VVUPU*(U(I,K,J)-U(I,K+1,J)+UN(K)-UN(K+1))
!           TVUP=VVUPV*(V(I,K,J)-V(I,K+1,J)+VN(K)-VN(K+1))
!           TULO=VVLOU*(U(I,K-1,J)-U(I,K,J)+UN(K-1)-UN(K))
!           TVLO=VVLOV*(V(I,K-1,J)-V(I,K,J)+VN(K-1)-VN(K))
!           ADUP=TUUP+TULO+UN(K)-U(I,K,J)
!           ADVP=TVUP+TVLO+VN(K)-V(I,K,J)
!           WRITE(0,*)' NTSD=',NTSD,' I=',ITEST,' J=',JTEST,' K=',K     &
!    &,               ' ADUP=',ADUP,' ADVP=',ADVP
!           WRITE(0,*)' U=',U(I,K,J),' UN=',UN(K)                       &
!    &,               ' VAD_TEND_U=',VAD_TEND_U(I,K)                    &
!    &,               ' V=',V(I,K,J),' VN=',VN(K)                       &
!    &,               ' VAD_TEND_V=',VAD_TEND_V(I,K)
!           WRITE(0,*)' '
!         ENDDO
!!
!         IF(LMVK==KTS)THEN
!           PVVUPU=PVVLOU
!           PVVUPV=PVVLOV
!           VVUPU=PVVUPU/(DETA1_PDTOP(KTS)+DETA2(KTS)*PDOPU)
!           VVUPV=PVVUPV/(DETA1_PDTOP(KTS)+DETA2(KTS)*PDOPV)
!           TUUP=VVUPU*(U(I,KTS,J)-U(I,KTS+1,J)+UN(KTS)-UN(KTS+1))
!           TVUP=VVUPV*(V(I,KTS,J)-V(I,KTS+1,J)+VN(KTS)-VN(KTS+1))
!           ADUP=TUUP+UN(KTS)-U(I,KTS,J)
!           ADVP=TVUP+VN(KTS)-V(I,KTS,J)
!           WRITE(0,*)' NTSD=',NTSD,' I=',ITEST,' J=',JTEST,' K=',KTS   &
!    &,               ' ADUP=',ADUP,' ADVP=',ADVP
!           WRITE(0,*)' U=',U(I,KTS,J),' UN=',UN(KTS)                   &
!    &,               ' VAD_TEND_U=',VAD_TEND_U(I,KTS)                  &
!    &,               ' V=',V(I,KTS,J),' VN=',VN(KTS)                   &
!    &,               ' VAD_TEND_V=',VAD_TEND_V(I,KTS)
!           WRITE(0,*)' '
!         ENDIF
!       ENDIF
!     endif
!
!-----------------------------------------------------------------------
!***  End of check.
!-----------------------------------------------------------------------
!
      ENDDO iloop_for_uv
!
!-----------------------------------------------------------------------
!
!***  NOW SUM THE VERTICAL AND HORIZONTAL TENDENCIES,
!***  CURVATURE AND CORIOLIS TERMS
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(fpp,hm,i,k,vm)
      DO K=KTS,KTE
      DO I=MYIS1,MYIE1
        HM=HTM(I,K,J)*HBM2(I,J)
        VM=VTM(I,K,J)*VBM2(I,J)
        ADT(I,K,J)=(VAD_TEND_T(I,K)+2.*ADT(I,K,J))*HM
!
        FPP=CURV(I,J)*2.*UST(I,K,J1_00)+F(I,J)*2.
        ADU(I,K,J)=(VAD_TEND_U(I,K)+2.*ADU(I,K,J)+VST(I,K,J1_00)*FPP)   &
     &             *VM
        ADV(I,K,J)=(VAD_TEND_V(I,K)+2.*ADV(I,K,J)-UST(I,K,J1_00)*FPP)   &
     &             *VM
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      ENDDO main_integration
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  SAVE THE OLD VALUES FOR TIMESTEPPING
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=MYJS_P4,MYJE_P4
        DO K=KTS,KTE
        DO I=MYIS_P4,MYIE_P4
          TOLD(I,K,J)=T(I,K,J)
          UOLD(I,K,J)=U(I,K,J)
          VOLD(I,K,J)=V(I,K,J)
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  FINALLY UPDATE THE PROGNOSTIC VARIABLES
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
        DO I=MYIS1,MYIE1
          T(I,K,J)=ADT(I,K,J)+T(I,K,J)
          U(I,K,J)=ADU(I,K,J)+U(I,K,J)
          V(I,K,J)=ADV(I,K,J)+V(I,K,J)
        ENDDO
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
      END SUBROUTINE ADVE
!-----------------------------------------------------------------------
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
!     VAD2 CALCULATES THE CONTRIBUTION OF THE VERTICAL ADVECTION
!     TO THE TENDENCIES OF WATER SUBSTANCE AND TKE AND THEN UPDATES
!     THOSE VARIABLES.  AN ANTI-FILTERING TECHNIQUE IS USED.
!
! PROGRAM HISTORY LOG:
!   96-07-19  JANJIC   - ORIGINATOR
!   98-11-02  BLACK    - MODIFIED FOR DISTRIBUTED MEMORY
!   99-03-17  TUCCILLO - INCORPORATED MPI_ALLREDUCE FOR GLOBAL SUM
!   02-02-06  BLACK    - CONVERTED TO WRF FORMAT
!   02-09-06  WOLFE    - MORE CONVERSION TO GLOBAL INDEXING
!   04-11-23  BLACK    - THREADED
!
! USAGE: CALL VAD2 FROM SUBROUTINE SOLVE_NMM
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
!***********************************************************************
!-----------------------------------------------------------------------
!
      ADDT=REAL(IDTAD)*DT
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(afr,afrp,bot,d2pqe,d2pqq,d2pqw,del,dep,detap,dpdn,dpup   &
!$omp&        ,dql,dqp,dwl,dwp,e00,e3,e4,e4p,ep,ep0,haddt,i,j,k,koff    &
!$omp&        ,la,lap,llap,petdtk,q00,q3,q4,q4p,qp,qp0,rfacek,rfacqk    &
!$omp&        ,rfacwk,rfc,rr,sumne,sumnq,sumnw,sumpe,sumpq,sumpw,top    &
!$omp&        ,w00,w3,w4,w4p,wp,wp0)
      main_integration : DO J=MYJS2,MYJE2
!
      DO I=MYIS1_P1,MYIE1_P1
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
        HADDT=-ADDT*HBM2(I,J)
!
        DO K=KTE,KOFF+1,-1
          RR=PETDTK(K)*HADDT
!
          IF(RR<0.)THEN
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
          IF(LLAP>KOFF.AND.LLAP<KTE+1.AND.LAP/=0)THEN
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
            TOP=LLAP==KTE+1
            BOT=LLAP==KOFF
!
            RR=0.
            AFR(K)=0.
            DQL(K)=0.
            DWL(K)=0.
            DEL(K)=0.
          ENDIF
        ENDDO
!-----------------------------------------------------------------------
        IF(TOP)THEN
          IF(LA(KTE-1)>0)THEN
            RFC=(DETA1(KTE-1)*PDTOP+DETA2(KTE-1)*PDSL(I,J))             &
     &         /(DETA1(KTE  )*PDTOP+DETA2(KTE  )*PDSL(I,J))
            DQL(KTE)=-DQL(KTE+1)*RFC
            DWL(KTE)=-DWL(KTE+1)*RFC
            DEL(KTE)=-DEL(KTE+1)*RFC
          ENDIF
        ENDIF
!
        IF(BOT)THEN
          IF(LA(KOFF+2)<0)THEN
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
!-----------------------------------------------------------------------
!***  ANTI-FILTERING STEP
!-----------------------------------------------------------------------
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
        IF(LAP/=0)THEN
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
        IF(DQP>0.)THEN
          SUMPQ=SUMPQ+DQP
        ELSE
          SUMNQ=SUMNQ+DQP
        ENDIF
!
        IF(DWP>0.)THEN
          SUMPW=SUMPW+DWP
        ELSE
          SUMNW=SUMNW+DWP
        ENDIF
!
        IF(DEP>0.)THEN
          SUMPE=SUMPE+DEP
        ELSE
          SUMNE=SUMNE+DEP
        ENDIF
!
   50   CONTINUE
!-----------------------------------------------------------------------
        DQL(KOFF+1)=0.
        DWL(KOFF+1)=0.
        DEL(KOFF+1)=0.
!
        DQL(KTE)=0.
        DWL(KTE)=0.
        DEL(KTE)=0.
!-----------------------------------------------------------------------
!***  FIRST MOMENT CONSERVING FACTOR
!-----------------------------------------------------------------------
        IF(SUMPQ>1.E-9)THEN
          RFACQK=-SUMNQ/SUMPQ
        ELSE
          RFACQK=1.
        ENDIF
!
        IF(SUMPW>1.E-9)THEN
          RFACWK=-SUMNW/SUMPW
        ELSE
          RFACWK=1.
        ENDIF
!
        IF(SUMPE>1.E-9)THEN
          RFACEK=-SUMNE/SUMPE
        ELSE
          RFACEK=1.
        ENDIF
!
        IF(RFACQK<CONSERVE_MIN.OR.RFACQK>CONSERVE_MAX)RFACQK=1.
        IF(RFACWK<CONSERVE_MIN.OR.RFACWK>CONSERVE_MAX)RFACWK=1.
        IF(RFACEK<CONSERVE_MIN.OR.RFACEK>CONSERVE_MAX)RFACEK=1.
!-----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!-----------------------------------------------------------------------
        DO K=KTE,KOFF+1,-1
          DQP=DQL(K)
          IF(DQP>=0.)DQP=DQP*RFACQK
          Q(I,K,J)=Q3(K)+DQP
        ENDDO
!-----------------------------------------------------------------------
        DO K=KTE,KOFF+1,-1
          DWP=DWL(K)
          IF(DWP>=0.)DWP=DWP*RFACWK
          CWM(I,K,J)=W3(K)+DWP
        ENDDO
!-----------------------------------------------------------------------
        DO K=KTE,KOFF+1,-1
          DEP=DEL(K)
          IF(DEP>=0.)DEP=DEP*RFACEK
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
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      ENDDO 
!
      ENDDO main_integration
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      END SUBROUTINE VAD2
!-----------------------------------------------------------------------
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
!   04-11-23  BLACK    - THREADED
!
! USAGE: CALL HAD2 FROM SUBROUTINE SOLVE_NMM
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
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V          &
     &                                        ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V      &
     &                                                ,IUP_ADH,IUP_ADV
!-----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! NMM_MAX_DIM is set in configure.wrf and must agree with the value of
! dimspec q in Registry/Registry.
!!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      REAL,PARAMETER :: FF1=0.530
!
#ifdef DM_PARALLEL
      INTEGER :: DOMDESC
#endif
!
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL)
      LOGICAL,EXTERNAL :: WRF_DM_ON_MONITOR
      INTEGER :: N
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,6) :: XSUMS_L
      REAL,DIMENSION(IDS:IDE,KDS:KDE,JDS:JDE,6) :: XSUMS_G
#endif
!
      LOGICAL :: BOT,TOP
!
      INTEGER :: I,IRECV,J,JFP,JFQ,K,KOFF,LAP,LLAP,MPI_COMM_COMP
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
!***********************************************************************
!-----------------------------------------------------------------------
!
      RDY=1./DY
      SLOPAC=SLOPHT*SQRT(2.)*0.5*50.
      CRIT=SLOPAC*REAL(IDTAD)*DT*RDY*1000.
!
      ADDT=REAL(IDTAD)*DT
      ENH=ADDT/(08.*DY)
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS_P3,MYJE_P3
      DO I=MYIS_P2,MYIE_P2
        EMH (I,J)=ADDT/(08.*DX(I,J))
        DARE(I,J)=HBM3(I,J)*DX(I,J)*DY
        E1(I,KTE,J)=MAX(Q2(I,KTE,J)*0.5,EPSQ2)
        E2(I,KTE,J)=E1(I,KTE,J)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(e1x,htmikj,i,j,k)
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
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dza,dzb,fpq,i,j,jfp,jfq,k,pp,qp,ssa,ssb,spp,sqp,tta,ttb)
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
        IF(SPP<0.)THEN
          JFP=-1
        ELSE
          JFP=1
        ENDIF
        IF(SQP<0.)THEN
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
          IF(ABS(DZA)>SLOPAC)THEN
            SSA=DZA*SPP
            IF(SSA>CRIT)THEN
              SPP=0. !spp*.1
            ENDIF
          ENDIF
!
          IF(ABS(DZB)>SLOPAC)THEN
            SSB=DZB*SQP
            IF(SSB>CRIT)THEN
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
!-----------------------------------------------------------------------
!***  ANTI-FILTERING STEP
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
        XSUMS(1,K)=0.
        XSUMS(2,K)=0.
        XSUMS(3,K)=0.
        XSUMS(4,K)=0.
        XSUMS(5,K)=0.
        XSUMS(6,K)=0.
      ENDDO
!-----------------------------------------------------------------------
!
!***  ANTI-FILTERING LIMITERS
!
!-----------------------------------------------------------------------
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL)
      DO N=1,6
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=JMS,JME 
        DO K=KMS,KME 
        DO I=IMS,IME 
          XSUMS_L(I,K,J,N)=0.
        ENDDO
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO J=JDS,JDE 
        DO K=KDS,KDE 
        DO I=IDS,IDE 
          XSUMS_G(I,K,J,N)=0.
        ENDDO
        ENDDO
        ENDDO
!
      ENDDO
!
#endif
!-----------------------------------------------------------------------
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
      DO N=1,6
        XSUMS_L(I,K,J,N)=0.
      ENDDO
!
      IF(DQSTIJ>0.)THEN
        XSUMS_L(I,K,J,1)=DQSTIJ
      ELSE
        XSUMS_L(I,K,J,2)=DQSTIJ
      ENDIF
!
      IF(DWSTIJ>0.)THEN
        XSUMS_L(I,K,J,3)=DWSTIJ
      ELSE
        XSUMS_L(I,K,J,4)=DWSTIJ
      ENDIF
!
      IF(DESTIJ>0.)THEN
        XSUMS_L(I,K,J,5)=DESTIJ
      ELSE
        XSUMS_L(I,K,J,6)=DESTIJ
      ENDIF
#else
      IF(DQSTIJ>0.)THEN
        XSUMS(1,K)=XSUMS(1,K)+DQSTIJ
      ELSE
        XSUMS(2,K)=XSUMS(2,K)+DQSTIJ
      ENDIF
!
      IF(DWSTIJ>0.)THEN
        XSUMS(3,K)=XSUMS(3,K)+DWSTIJ
      ELSE
        XSUMS(4,K)=XSUMS(4,K)+DWSTIJ
      ENDIF
!
      IF(DESTIJ>0.)THEN
        XSUMS(5,K)=XSUMS(5,K)+DESTIJ
      ELSE
        XSUMS(6,K)=XSUMS(6,K)+DESTIJ
      ENDIF
#endif
!
  150 CONTINUE
!
!-----------------------------------------------------------------------
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL)
      DO N=1,6
        CALL WRF_PATCH_TO_GLOBAL_REAL( XSUMS_L(IMS,KMS,JMS,N)           &
     &,                                XSUMS_G(1,1,1,N),DOMDESC         & 
     &,                               'xyz','xzy'                       &
     &,                                IDS,IDE,KDS,KDE,JDS,JDE          &    
     &,                                IMS,IME,KMS,KME,JMS,JME          &
     &,                                ITS,ITE,KTS,KTE,JTS,JTE )
      ENDDO
!
      GSUMS=0.
!
      IF(WRF_DM_ON_MONITOR())THEN
        DO N=1,6
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO J=JDS,JDE
          DO K=KDS,KDE
          DO I=IDS,IDE
            GSUMS(N,K)=GSUMS(N,K)+XSUMS_G(I,K,J,N)
          ENDDO
          ENDDO
          ENDDO
        ENDDO
      ENDIF

      CALL WRF_DM_BCAST_BYTES(GSUMS,2*RWORDSIZE*6*(KDE-KDS+1) )

#else
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
# ifdef DM_PARALLEL
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,6*(KTE-KTS+1)                      &
     &                  ,MPI_DOUBLE_PRECISION,MPI_SUM                   &
     &                  ,MPI_COMM_COMP,IRECV)
# else
      GSUMS=XSUMS
# endif
#endif
!
!-----------------------------------------------------------------------
!***  END OF GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
!     if(mype==0)then
!       if(ntsd==0)then
!!        call int_get_fresh_handle(nunit)
!!        close(nunit)
!         nunit=56
!         open(unit=nunit,file='gsums',form='unformatted',iostat=ier)
!       endif
!     endif
      DO K=KTS,KTE
!       if(mype==0)then
!         write(nunit)(gsums(i,k),i=1,6)
!       endif
!
!-----------------------------------------------------------------------
        SUMPQ=GSUMS(1,K)
        SUMNQ=GSUMS(2,K)
        SUMPW=GSUMS(3,K)
        SUMNW=GSUMS(4,K)
        SUMPE=GSUMS(5,K)
        SUMNE=GSUMS(6,K)
!
!-----------------------------------------------------------------------
!***  FIRST MOMENT CONSERVING FACTOR
!-----------------------------------------------------------------------
!
        IF(SUMPQ>1.)THEN
          RFACQK=-SUMNQ/SUMPQ
        ELSE
          RFACQK=1.
        ENDIF
!
        IF(SUMPW>1.)THEN
          RFACWK=-SUMNW/SUMPW
        ELSE
          RFACWK=1.
        ENDIF
!
        IF(SUMPE>1.)THEN
          RFACEK=-SUMNE/SUMPE
        ELSE
          RFACEK=1.
        ENDIF
!
        IF(RFACQK<CONSERVE_MIN.OR.RFACQK>CONSERVE_MAX)RFACQK=1.
        IF(RFACWK<CONSERVE_MIN.OR.RFACWK>CONSERVE_MAX)RFACWK=1.
        IF(RFACEK<CONSERVE_MIN.OR.RFACEK>CONSERVE_MAX)RFACEK=1.
!
        RFACQ(K)=RFACQK
        RFACW(K)=RFACWK
        RFACE(K)=RFACEK
!
      ENDDO
!     if(mype==0.and.ntsd==181)close(nunit)
!
!-----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dqstij,i,j,k,rfacqk,rfqij)
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
          RFACQK=RFACQ(K)
          IF(RFACQK<1.)THEN
            DO I=MYIS1,MYIE1
              DQSTIJ=DQST(I,K,J)
              RFQIJ=HBM2(I,J)*(RFACQK-1.)+1.
              IF(DQSTIJ>=0.)DQSTIJ=DQSTIJ*RFQIJ
              Q(I,K,J)=Q(I,K,J)+DQSTIJ
            ENDDO
          ELSE
            DO I=MYIS1,MYIE1
              DQSTIJ=DQST(I,K,J)
              RFQIJ=HBM2(I,J)*(RFACQK-1.)+1.
              IF(DQSTIJ<0.)DQSTIJ=DQSTIJ/RFQIJ
              Q(I,K,J)=Q(I,K,J)+DQSTIJ
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dwstij,i,j,k,rfacwk,rfwij)
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
          RFACWK=RFACW(K)
          IF(RFACWK<1.)THEN
            DO I=MYIS1,MYIE1
              DWSTIJ=DWST(I,K,J)
              RFWIJ=HBM2(I,J)*(RFACWK-1.)+1.
              IF(DWSTIJ>=0.)DWSTIJ=DWSTIJ*RFWIJ
              CWM(I,K,J)=CWM(I,K,J)+DWSTIJ
            ENDDO
          ELSE
            DO I=MYIS1,MYIE1
              DWSTIJ=DWST(I,K,J)
              RFWIJ=HBM2(I,J)*(RFACWK-1.)+1.
              IF(DWSTIJ<0.)DWSTIJ=DWSTIJ/RFWIJ
              CWM(I,K,J)=CWM(I,K,J)+DWSTIJ
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(destij,i,j,k,rfacek,rfeij)
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
          RFACEK=RFACE(K)
          IF(RFACEK<1.)THEN
            DO I=MYIS1,MYIE1
              DESTIJ=DEST(I,K,J)
              RFEIJ=HBM2(I,J)*(RFACEK-1.)+1.
              IF(DESTIJ>=0.)DESTIJ=DESTIJ*RFEIJ
              E1(I,K,J)=E1(I,K,J)+DESTIJ
            ENDDO
          ELSE
            DO I=MYIS1,MYIE1
              DESTIJ=DEST(I,K,J)
              RFEIJ=HBM2(I,J)*(RFACEK-1.)+1.
              IF(DESTIJ<0.)DESTIJ=DESTIJ/RFEIJ
              E1(I,K,J)=E1(I,K,J)+DESTIJ
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=MYJS,MYJE
      DO K=KTS,KTE
      DO I=MYIS,MYIE
        Q  (I,K,J)=MAX(Q  (I,K,J),EPSQ)*HTM(I,K,J)
        CWM(I,K,J)=MAX(CWM(I,K,J),CLIMIT)*HTM(I,K,J)
      ENDDO
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        Q2(I,KTE,J)=MAX(E1(I,KTE,J)+E1(I,KTE,J)-EPSQ2,EPSQ2)            &
     &             *HTM(I,KTE,J)
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,koff)
      DO J=MYJS,MYJE
      DO K=KTE-1,KTS+1,-1
      DO I=MYIS,MYIE
        KOFF=KTE-LMH(I,J)
        IF(K>KOFF+1)THEN
          Q2(I,K,J)=MAX(E1(I,K,J)+E1(I,K,J)-Q2(I,K+1,J),EPSQ2)          &
     &             *HTM(I,K,J)
        ELSE
          Q2(I,K,J)=Q2(I,K+1,J)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
      END SUBROUTINE HAD2
!-----------------------------------------------------------------------
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
!   04-11-23  BLACK    - THREADED
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
!-----------------------------------------------------------------------
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
!***********************************************************************
!-----------------------------------------------------------------------
!
      ADDT=REAL(IDTAD)*DT
!
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(afr,afrp,bot,d2pqe,del,dep,detap,dpdn,dpup,e00,e3        &
!$omp&        ,e4,e4p,ep,ep0,hbm2ij,i,j,k,koff,la,lap,llap,petdtk       &
!$omp&        ,rfacek,rfc,rr,sumne,sumpe,top)
      main_integration : DO J=MYJS2,MYJE2
!
      DO I=MYIS1_P1,MYIE1_P1
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
        HADDT=-ADDT*HBM2(I,J)
!
        DO K=KTE,KOFF+1,-1
          RR=PETDTK(K)*HADDT
!
          IF(RR<0.)THEN
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
          IF(LLAP>0.AND.LLAP<KTE+1.AND.LAP/=0)THEN
            RR=ABS(RR/((AETA1(LLAP)-AETA1(K))*PDTOP                     &
     &                +(AETA2(LLAP)-AETA2(K))*PDSL(I,J)))
!
            AFR(K)=(((FF4*RR+FF3)*RR+FF2)*RR+FF1)*RR
            DEP=(E3(LLAP)-E3(K))*RR
            DEL(K)=DEP
          ELSE
            TOP=LLAP==KTE+1
            BOT=LLAP==KOFF
!
            RR=0.
            AFR(K)=0.
            DEL(K)=0.
          ENDIF
        ENDDO
!-----------------------------------------------------------------------
        IF(TOP)THEN
          IF(LA(KTE-1)<0)THEN
            RFC=(DETA1(KTE-1)*PDTOP+DETA2(KTE-1)*PDSL(I,J))             &
     &         /(DETA1(KTE  )*PDTOP+DETA2(KTE  )*PDSL(I,J))
            DEL(KTE)=-DEL(KTE+1)*RFC
          ENDIF
        ENDIF
!
        IF(BOT)THEN
          IF(LA(KOFF+2)<0)THEN
            RFC=(DETA1(KOFF+2)*PDTOP+DETA2(KOFF+2)*PDSL(I,J))           &
     &         /(DETA1(KOFF+1)*PDTOP+DETA2(KOFF+1)*PDSL(I,J))
            DEL(KOFF+1)=-DEL(KOFF+2)*RFC
          ENDIF
        ENDIF
!
        DO K=KOFF+1,KTE
          E4(K)=E3(K)+DEL(K)
        ENDDO
!-----------------------------------------------------------------------
!***  ANTI-FILTERING STEP
!-----------------------------------------------------------------------
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
        IF(LAP/=0)THEN
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
        IF(LAP/=0)THEN
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
        IF(DEP>0.)THEN
          SUMPE=SUMPE+DEP
        ELSE
          SUMNE=SUMNE+DEP
        ENDIF
!
   50   CONTINUE
!-----------------------------------------------------------------------
        DEL(KTE)=0.
!
        DEL(KOFF+1)=0.
!-----------------------------------------------------------------------
!***  FIRST MOMENT CONSERVING FACTOR
!-----------------------------------------------------------------------
        IF(SUMPE>1.E-9)THEN
          RFACEK=-SUMNE/SUMPE
        ELSE
          RFACEK=1.
        ENDIF
!
        IF(RFACEK<CONSERVE_MIN.OR.RFACEK>CONSERVE_MAX)RFACEK=1.
!-----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!-----------------------------------------------------------------------
        DO K=KOFF+1,KTE
          DEP=DEL(K)
          IF(DEP>=0.)DEP=DEP*RFACEK
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
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      ENDDO 
!
      ENDDO main_integration
!-----------------------------------------------------------------------
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
!   04-11-23  BLACK    - THREADED
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
      INTEGER :: I,IRECV,J,JFP,JFQ,K,KOFF,LAP,LLAP,MPI_COMM_COMP
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
!***********************************************************************
!-----------------------------------------------------------------------
      RDY=1./DY
      SLOPAC=SLOPHT*SQRT(2.)*0.5*50.
      CRIT=SLOPAC*REAL(IDTAD)*DT*RDY*1000.
!
      ADDT=REAL(IDTAD)*DT
      ENH=ADDT/(08.*DY)
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS_P3,MYJE_P3
      DO I=MYIS_P2,MYIE_P2
        EMH (I,J)=ADDT/(08.*DX(I,J))
        DARE(I,J)=HBM3(I,J)*DX(I,J)*DY
        E1(I,KTE,J)=MAX(Q2(I,KTE,J)*0.5,EPSQ2)
        E2(I,KTE,J)=E1(I,KTE,J)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,k)
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
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dza,dzb,fpq,i,j,jfp,jfq,k,pp,qp,spp,sqp,ssa,ssb,tta,ttb)
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
        IF(SPP<0.)THEN
          JFP=-1
        ELSE
          JFP=1
        ENDIF
        IF(SQP<0.)THEN
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
!------------------------------------------------------------------------
        IF(.NOT.HYDRO)THEN ! z currently not available for hydro=.true.
          DZA=(Z(IFPA(I,K,J),K,JFPA(I,K,J))-Z(I,K,J))*RDY
          DZB=(Z(IFQA(I,K,J),K,JFQA(I,K,J))-Z(I,K,J))*RDY
!
          IF(ABS(DZA)>SLOPAC)THEN
            SSA=DZA*SPP
            IF(SSA>CRIT)THEN
              SPP=0. !spp*.1
            ENDIF
          ENDIF
!
          IF(ABS(DZB)>SLOPAC)THEN
            SSB=DZB*SQP
            IF(SSB>CRIT)THEN
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
!-----------------------------------------------------------------------
!***  ANTI-FILTERING STEP
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
        XSUMS(1,K)=0.
        XSUMS(2,K)=0.
      ENDDO
!
!--------------ANTI-FILTERING LIMITERS----------------------------------
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
      IF(DESTIJ>0.)THEN
        XSUMS(1,K)=XSUMS(1,K)+DESTIJ
      ELSE
        XSUMS(2,K)=XSUMS(2,K)+DESTIJ
      ENDIF
!
  150 CONTINUE
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,2*(KTE-KTS+1),MPI_REAL,MPI_SUM     &
     &                  ,MPI_COMM_COMP,IRECV)
#else
      GSUMS=XSUMS
#endif
!
!-----------------------------------------------------------------------
!***  END OF GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
!
!-----------------------------------------------------------------------
        SUMPE=GSUMS(1,K)
        SUMNE=GSUMS(2,K)
!
!-----------------------------------------------------------------------
!***  FIRST MOMENT CONSERVING FACTOR
!-----------------------------------------------------------------------
!
        IF(SUMPE>1.)THEN
          RFACEK=-SUMNE/SUMPE
        ELSE
          RFACEK=1.
        ENDIF
!
        IF(RFACEK<CONSERVE_MIN.OR.RFACEK>CONSERVE_MAX)RFACEK=1.
!
        RFACE(K)=RFACEK
!
      ENDDO
!
!-----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(destij,i,j,k,rfacek,rfeij)
      DO J=MYJS2,MYJE2
        DO K=KTS,KTE
          RFACEK=RFACE(K)
          IF(RFACEK<1.)THEN
            DO I=MYIS1,MYIE1
              DESTIJ=DEST(I,K,J)
              RFEIJ=HBM2(I,J)*(RFACEK-1.)+1.
              IF(DESTIJ>=0.)DESTIJ=DESTIJ*RFEIJ
              E1(I,K,J)=E1(I,K,J)+DESTIJ
            ENDDO
          ELSE
            DO I=MYIS1,MYIE1
              DESTIJ=DEST(I,K,J)
              RFEIJ=HBM2(I,J)*(RFACEK-1.)+1.
              IF(DESTIJ<0.)DESTIJ=DESTIJ/RFEIJ
              E1(I,K,J)=E1(I,K,J)+DESTIJ
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        Q2(I,KTE,J)=MAX(E1(I,KTE,J)+E1(I,KTE,J)-EPSQ2,EPSQ2)            &
     &             *HTM(I,KTE,J)
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,koff)
      DO J=MYJS,MYJE
      DO K=KTE-1,KTS+1,-1
      DO I=MYIS,MYIE
        KOFF=KTE-LMH(I,J)
        IF(K>KOFF+1)THEN
          Q2(I,K,J)=MAX(E1(I,K,J)+E1(I,K,J)-Q2(I,K+1,J),EPSQ2)          &
     &             *HTM(I,K,J)
        ELSE
          Q2(I,K,J)=Q2(I,K+1,J)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
      END SUBROUTINE HAD2_DRY
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      END MODULE MODULE_ADVECTION
!-----------------------------------------------------------------------
