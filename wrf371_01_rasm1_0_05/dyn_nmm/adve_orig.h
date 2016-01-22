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
!   87-06-??  JANJIC     - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   96-03-28  BLACK      - ADDED EXTERNAL EDGE
!   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
!   99-07-    JANJIC     - CONVERTED TO ADAMS-BASHFORTH SCHEME
!                          COMBINING HORIZONTAL AND VERTICAL ADVECTION
!   02-02-04  BLACK      - ADDED VERTICAL CFL CHECK
!   02-02-05  BLACK      - CONVERTED TO WRF FORMAT
!   02-08-29  MICHALAKES - CONDITIONAL COMPILATION OF MPI
!                          CONVERT TO GLOBAL INDEXING
!   02-09-06  WOLFE      - MORE CONVERSION TO GLOBAL INDEXING
!   04-05-29  JANJIC,BLACK - CRANK-NICHOLSON VERTICAL ADVECTION
!     
! USAGE: CALL ADVE FROM SUBROUTINE SOLVE_RUNSTREAM
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
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V          &
     &                                        ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V      &
     &                                                ,IUP_ADH,IUP_ADV  &
     &                                                ,LMH,LMV
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
      INTEGER,DIMENSION(ITS-5:ITE+5,KTS:KTE) :: ISPA,ISQA
!
      REAL :: ARRAY3_X,CFT,CFU,CFV,CMT,CMU,CMV                          &
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
      JSTART=MYJS2
      JEND=MYJE2
!
!
!-----------------------------------------------------------------------
!
!***  START THE HORIZONTAL ADVECTION IN THE INITIAL SOUTHERN SLABS.
!
!-----------------------------------------------------------------------
!
      DO J=-2,1
        JJ=JSTART+J
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

!
! John and Tom both think this is all right, even for tiles,
! as long as the slab arrays being indexed by these things
! are locally defined.
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
!  
!-----------------------------------------------------------------------
!***  THE WORKING ARRAYS FOR THE PRIMARY VARIABLES
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
      DO I=MYIS_P4,MYIE_P4
        TST(I,K,J1_P2)=T(I,K,J+2)*FFC+TOLD(I,K,J+2)*FBC
        UST(I,K,J1_P2)=U(I,K,J+2)*FFC+UOLD(I,K,J+2)*FBC
        VST(I,K,J1_P2)=V(I,K,J+2)*FFC+VOLD(I,K,J+2)*FBC
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  MASS FLUXES AND MASS POINT ADVECTION COMPONENTS
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
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
      ENDDO
!
!-----------------------------------------------------------------------
      DO K=KTS,KTE
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
      ENDDO
!
!-----------------------------------------------------------------------
!***  CALCULATION OF MOMENTUM ADVECTION COMPONENTS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  THE NS AND EW FLUXES ARE ON H POINTS FOR U AND V.
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
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
      ENDDO
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
      iloop_for_t:  DO I=MYIS1,MYIE1
!
        PDOP=PDSLO(I,J)
        PVVLO=PETDT(I,KTE-1,J)*DTQ
        VVLO=PVVLO/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOP)
        CMT=-VVLO+1.
        RCMT(KTE)=1./CMT
        CRT(KTE)=VVLO
        RSTT(KTE)=-VVLO*(T(I,KTE-1,J)-T(I,KTE,J))+T(I,KTE,J)
!
        LMHK=KTE-LMH(I,J)+1
        DO K=KTE-1,LMHK+1,-1
          RDP=1./(DETA1_PDTOP(K)+DETA2(K)*PDOP)
          PVVUP=PVVLO
          PVVLO=PETDT(I,K-1,J)*DTQ
          VVUP=PVVUP*RDP
          VVLO=PVVLO*RDP
          CFT=-VVUP*RCMT(K+1)
          CMT=-CRT(K+1)*CFT+(VVUP-VVLO+1.)
          RCMT(K)=1./CMT
          CRT(K)=VVLO
          RSTT(K)=-RSTT(K+1)*CFT+T(I,K,J)                               &
     &            -(T(I,K,J)-T(I,K+1,J))*VVUP                           &
     &            -(T(I,K-1,J)-T(I,K,J))*VVLO
        ENDDO
!
        PVVUP=PVVLO
        VVUP=PVVUP/(DETA1_PDTOP(LMHK)+DETA2(LMHK)*PDOP)
        CFT=-VVUP*RCMT(LMHK+1)
        CMT=-CRT(LMHK+1)*CFT+VVUP+1.
        CRT(LMHK)=0.
        RSTT(LMHK)=-(T(I,LMHK,J)-T(I,LMHK+1,J))*VVUP                    &
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
!
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
      iloop_for_uv:  DO I=MYIS1,MYIE1
!
        PDOPU=(PDSLO(I+IVW(J),J)+PDSLO(I+IVE(J),J))*0.5
        PDOPV=(PDSLO(I,J-1)+PDSLO(I,J+1))*0.5
        PVVLOU=(PETDT(I+IVW(J),KTE-1,J)+PETDT(I+IVE(J),KTE-1,J))*DTE
        PVVLOV=(PETDT(I,KTE-1,J-1)+PETDT(I,KTE-1,J+1))*DTE
        VVLOU=PVVLOU/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOPU)
        VVLOV=PVVLOV/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOPV)
        CMU=-VVLOU+1.
        CMV=-VVLOV+1.
        RCMU(KTE)=1./CMU
        RCMV(KTE)=1./CMV
        CRU(KTE)=VVLOU
        CRV(KTE)=VVLOV
        RSTU(KTE)=-VVLOU*(U(I,KTE-1,J)-U(I,KTE,J))+U(I,KTE,J)
        RSTV(KTE)=-VVLOV*(V(I,KTE-1,J)-V(I,KTE,J))+V(I,KTE,J)
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
          CFU=-VVUPU*RCMU(K+1)
          CFV=-VVUPV*RCMV(K+1)
          CMU=-CRU(K+1)*CFU+VVUPU-VVLOU+1.
          CMV=-CRV(K+1)*CFV+VVUPV-VVLOV+1.
          RCMU(K)=1./CMU
          RCMV(K)=1./CMV
          CRU(K)=VVLOU
          CRV(K)=VVLOV
          RSTU(K)=-RSTU(K+1)*CFU+U(I,K,J)                               &
     &            -(U(I,K,J)-U(I,K+1,J))*VVUPU                          &
     &            -(U(I,K-1,J)-U(I,K,J))*VVLOU
          RSTV(K)=-RSTV(K+1)*CFV+V(I,K,J)                               &
     &            -(V(I,K,J)-V(I,K+1,J))*VVUPV                          &
     &            -(V(I,K-1,J)-V(I,K,J))*VVLOV
        ENDDO
!
        RDPU=1./(DETA1_PDTOP(LMVK)+DETA2(LMVK)*PDOPU)
        RDPV=1./(DETA1_PDTOP(LMVK)+DETA2(LMVK)*PDOPV)
        PVVUPU=PVVLOU
        PVVUPV=PVVLOV
        VVUPU=PVVUPU*RDPU
        VVUPV=PVVUPV*RDPV
        CFU=-VVUPU*RCMU(LMVK+1)
        CFV=-VVUPV*RCMV(LMVK+1)
        CMU=-CRU(LMVK+1)*CFU+VVUPU+1.
        CMV=-CRV(LMVK+1)*CFV+VVUPV+1.
        CRU(LMVK)=0.
        CRV(LMVK)=0.
        RSTU(LMVK)=-(U(I,LMVK,J)-U(I,LMVK+1,J))*VVUPU                   &
     &               -RSTU(LMVK+1)*CFU+U(I,LMVK,J)
        RSTV(LMVK)=-(V(I,LMVK,J)-V(I,LMVK+1,J))*VVUPV                   &
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
!
!-----------------------------------------------------------------------
!***  End of check.
!-----------------------------------------------------------------------
!
      ENDDO iloop_for_uv
!
!
!-----------------------------------------------------------------------
!
!***  NOW SUM THE VERTICAL AND HORIZONTAL TENDENCIES,
!***  CURVATURE AND CORIOLIS TERMS
!
!-----------------------------------------------------------------------
!
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
