!*********************************************************************** 
      SUBROUTINE ADVE(NTSD,DT,DETA1,DETA2,PDTOP                         &
     &               ,CURV,F,FAD,F4D,EM_LOC,EMT_LOC,EN,ENT,DX,DY        &
     &               ,HTM,HBM2,VTM,VBM2                                 &
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
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V         &
     &                                         ,N_IUP_ADH,N_IUP_ADV
      INTEGER, DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V     &
     &                                                 ,IUP_ADH,IUP_ADV
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
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      LOGICAL :: UPSTRM
!
      INTEGER :: I,IEND,IFP,IFQ,II,IPQ,ISP,ISQ,ISTART                   &
     &          ,IUP_ADH_J,IVH,IVL                                      &
     &          ,J,J1,JA,JAK,JEND,JGLOBAL,JJ,JKNT,JP2,JSTART            &
     &          ,K,KNTI_ADH,KSTART,KSTOP                                &
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
      REAL :: ARRAY3_X,CFL,DPDE_P3,F0,F1,F2,F3                          &
     &       ,FEW_00,FEW_P1,FNE_X,FNS_P1,FNS_X,FPP,FSE_X                &
     &       ,HM,PP,QP,RDPD,RDPDX,RDPDY,T_UP,TEMPA,TEMPB,TTA,TTB        &
     &       ,U_UP,UDY_P1,UDY_X,VXD_X,VDX_P2,V_UP,VDX_X,VM,VTA,VUA,VVA
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE) :: ARRAY0,ARRAY1              &
     &                                      ,ARRAY2,ARRAY3              &
     &                                      ,VAD_TEND_T,VAD_TEND_U      &
     &                                      ,VAD_TEND_V
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE) :: TEW,UEW,VEW
!
      REAL,DIMENSION(ITS-5:ITE+5) :: VTB,VUB,VVB
!
      REAL,DIMENSION(KTS:KTE) :: VAD_TNDX_T,VAD_TNDX_U,VAD_TNDX_V
!
      REAL,DIMENSION(ITS-5:ITE+5,-1:1) :: PETDTK
!
      REAL,DIMENSION(ITS-5:ITE+5) :: TDN,UDN,VDN
!
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!**********************************************************************
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
!**********************************************************************
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! I should have done all this but right now the loops are still
! using the non-thread safe versions in module_MPP.F  JM
      MYIS    =MAX(IDS  ,ITS  )
      MYIE    =MIN(IDE  ,ITE  )
!
      MYIS1   =MAX(IDS+1,ITS  )
      MYIE1   =MIN(IDE-1,ITE  )
      MYJS2   =MAX(JDS+2,JTS  )
      MYJE2   =MIN(JDE-2,JTE  )
!
      MYIS_P2 =MAX(IDS  ,ITS-2)
      MYIE_P2 =MIN(IDE  ,ITE+2)
      MYIS_P3 =MAX(IDS  ,ITS-3)
      MYIE_P3 =MIN(IDE  ,ITE+3)
      MYIS_P4 =MAX(IDS  ,ITS-4)
      MYIE_P4 =MIN(IDE  ,ITE+4)
      MYJS_P2 =MAX(JDS  ,JTS-2)
      MYJE_P2 =MIN(JDE  ,JTE+2)
      MYJS_P4 =MAX(JDS  ,JTS-4)
      MYJE_P4 =MIN(JDE  ,JTE+4)
!
      MYIS1_P1=MAX(IDS+1,ITS-1)
      MYIE1_P1=MIN(IDE-1,ITE+1)
      MYIS1_P2=MAX(IDS+1,ITS-2)
      MYIE1_P2=MIN(IDE-1,ITE+2)
      MYIS1_P3=MAX(IDS+1,ITS-3)
      MYIE1_P3=MIN(IDE-1,ITE+3)
      MYIS1_P4=MAX(IDS+1,ITS-4)
      MYIE1_P4=MIN(IDE-1,ITE+4)

      ISTART=MYIS_P2
      IEND=MYIE_P2 
      IF( ITE == IDE )IEND=MYIE-3 

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
!----------------------------------------------------------------------
!***
!***  WE NEED THE STARTING AND ENDING J FOR THIS TASK'S INTEGRATION
!***
      JSTART=MYJS2
      JEND=MYJE2
!
!
!----------------------------------------------------------------------
!
!***  FILL WORKING ARRAYS FOR THE INTEGRATION
!
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!***  MARCH NORTHWARD THROUGH THE SOUTHERNMOST SLABS TO BEGIN
!***  FILLING THE MAIN WORKING ARRAYS WHICH ARE MULTI-DIMENSIONED
!***  IN J BECAUSE THEY ARE DIFFERENCED OR AVERAGED IN J.
!***  THE NORTHERNMOST OF EACH OF THE WORKING ARRAYS WILL BE
!***  FILLED IN THE PRIMARY INTEGRATION SECTONS.
!----------------------------------------------------------------------
!
      J1=-3
      IF(JTS==JDS)J1=-2  ! Cannot go 3 south from J=2 for south tasks
!
      DO J=J1,2
        JJ=JSTART+J
!
        DO K=KTS,KTE
        DO I=MYIS_P4,MYIE_P4
          DPDE(I,K,J)=DETA1(K)*PDTOP+DETA2(K)*PDSLO(I,JJ)
        ENDDO
        ENDDO
!
      ENDDO
!
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      main_integration : DO J=JSTART,JEND
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
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
  
!----------------------------------------------------------------------
!***  THE WORKING ARRAYS FOR THE PRIMARY VARIABLES
!----------------------------------------------------------------------
!
      DO K=KTS,KTE
      DO I=MYIS_P4,MYIE_P4
        TST(I,K,J1_P2)=T(I,K,J+2)*FFC+TOLD(I,K,J+2)*FBC
        UST(I,K,J1_P2)=U(I,K,J+2)*FFC+UOLD(I,K,J+2)*FBC
        VST(I,K,J1_P2)=V(I,K,J+2)*FFC+VOLD(I,K,J+2)*FBC
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!***  MASS FLUXES AND MASS POINT ADVECTION COMPONENTS
!----------------------------------------------------------------------
!
      DO K=KTS,KTE
      DO I=MYIS_P4,MYIE_P4
!
!----------------------------------------------------------------------
!***  THE NS AND EW FLUXES IN THE FOLLOWING LOOP ARE ON V POINTS
!***  FOR T.
!----------------------------------------------------------------------
!
        DPDE_P3=DETA1(K)*PDTOP+DETA2(K)*PDSLO(I,J+3)
        DPDE(I,K,J0_P3)=DPDE_P3
!
!----------------------------------------------------------------------
        UDY(I,K,J1_P2)=U(I,K,J+2)*DY
        VDX_P2=V(I,K,J+2)*DX(I,J+2)
        VDX(I,K,J1_P2)=VDX_P2
        FNS(I,K,J+2)=VDX_P2*(DPDE(I,K,J0_P1)+DPDE_P3)
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
      DO K=KTS,KTE
      DO I=MYIS_P3,MYIE_P3
        TEMPA=(UDY(I+IHE(J+1),K,J1_P1)+VDX(I+IHE(J+1),K,J1_P1))         &
     &       +(UDY(I,K,J1_P2)         +VDX(I,K,J1_P2))
        FNE(I,K,J+1)=TEMPA*(DPDE(I,K,J0_P1)+DPDE(I+IHE(J+1),K,J0_P2))
!
!----------------------------------------------------------------------
        TEMPB=(UDY(I+IHE(J+2),K,J1_P2)-VDX(I+IHE(J+2),K,J1_P2))         &
     &       +(UDY(I,K,J1_P1)         -VDX(I,K,J1_P1))
        FSE(I,K,J+2)=TEMPB*(DPDE(I,K,J0_P2)+DPDE(I+IHE(J),K,J0_P1))
!
!----------------------------------------------------------------------
        FNS_P1=FNS(I,K,J+1)
        TNS(I,K,J4_P1)=FNS_P1*(TST(I,K,J1_P2)-TST(I,K,J1_00))
!
!----------------------------------------------------------------------
        UDY_P1=U(I,K,J+1)*DY
        FEW(I,K,J+1)=UDY_P1*(DPDE(I+IVW(J+1),K,J0_P1)                   &
     &                        +DPDE(I+IVE(J+1),K,J0_P1))
        FEW_00=FEW(I,K,J)
        TEW(I,K)=FEW_00*(TST(I+IVE(J),K,J1_00)-TST(I+IVW(J),K,J1_00))
!
!----------------------------------------------------------------------
!***  THE NE AND SE FLUXES ARE ASSOCIATED WITH H POINTS
!***  (ACTUALLY JUST TO THE NE AND SE OF EACH H POINT).
!----------------------------------------------------------------------
!
        FNE_X=FNE(I,K,J)
        TNE(I,K,J5_00)=FNE_X*(TST(I+IHE(J),K,J1_P1)-TST(I,K,J1_00))
!
        FSE_X=FSE(I,K,J+1)
        TSE(I,K,J6_P1)=FSE_X*(TST(I+IHE(J+1),K,J1_00)-TST(I,K,J1_P1))
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!***  CALCULATION OF MOMENTUM ADVECTION COMPONENTS
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!***  THE NS AND EW FLUXES ARE ON H POINTS FOR U AND V.
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!***  THE FOLLOWING NE AND SE FLUXES ARE TIED TO V POINTS AND ARE
!***  LOCATED JUST TO THE NE AND SE OF THE GIVEN I,J.
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!***  COMPUTE THE ADVECTION TENDENCIES FOR T.
!***  THE AD ARRAYS ARE ON H POINTS.
!***  SKIP TO UPSTREAM IF THESE ROWS HAVE ONLY UPSTREAM POINTS.
!----------------------------------------------------------------------
!
      
      JGLOBAL=J+MY_JS_GLB-1
      IF(JGLOBAL.GE.6.AND.JGLOBAL.LE.JDE-5)THEN
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
!----------------------------------------------------------------------
!***  COMPUTE THE ADVECTION TENDENCIES FOR U AND V.
!***  THE AD ARRAYS ARE ON VELOCITY POINTS.
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!***  END OF JANJIC ADVECTION 
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!***  UPSTREAM ADVECTION OF T, U, AND V
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      upstream : IF(UPSTRM)THEN
!
!----------------------------------------------------------------------
!***
!***  COMPUTE UPSTREAM COMPUTATIONS ON THIS TASK'S ROWS.
!***
!----------------------------------------------------------------------
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
              IF(PP.LT.0.)THEN
                ISPA(I,K)=-1
              ELSE
                ISPA(I,K)= 1
              ENDIF
!
              IF(QP.LT.0.)THEN
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
!----------------------------------------------------------------------
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
     &          *HTM(I+IPQ,K,J+ISP+ISQ).GT.0.1)THEN
                 GO TO 150
              ENDIF
!
              IF(HTM(I+IHE(J)+IFP,K,J+ISP)                              &
     &          +HTM(I+IHE(J)+IFQ,K,J+ISQ)                              &
     &          +HTM(I+IPQ,K,J+ISP+ISQ).LT.0.1)THEN 
!
                T(I+IHE(J)+IFP,K,J+ISP)=T(I,K,J)
                T(I+IHE(J)+IFQ,K,J+ISQ)=T(I,K,J)
                T(I+IPQ,K,J+ISP+ISQ)=T(I,K,J)
!
              ELSEIF                                                    &
     &        (HTM(I+IHE(J)+IFP,K,J+ISP)+HTM(I+IPQ,K,J+ISP+ISQ)         &
     &        .LT.0.99)THEN
!
                T(I+IHE(J)+IFP,K,J+ISP)=T(I,K,J)
                T(I+IPQ,K,J+ISP+ISQ)=T(I+IHE(J)+IFQ,K,J+ISQ)
!
              ELSEIF                                                    &
     &        (HTM(I+IHE(J)+IFQ,K,J+ISQ)+HTM(I+IPQ,K,J+ISP+ISQ)         &
              .LT.0.99)THEN
!
                T(I+IHE(J)+IFQ,K,J+ISQ)=T(I,K,J)
                T(I+IPQ,K,J+ISP+ISQ)=T(I+IHE(J)+IFP,K,J+ISP)
!
              ELSEIF                                                    &
     &        (HTM(I+IHE(J)+IFP,K,J+ISP)                                &
     &        +HTM(I+IHE(J)+IFQ,K,J+ISQ).LT.0.99)THEN
                T(I+IHE(J)+IFP,K,J+ISP)=0.5*(T(I,K,J)                   &
     &                                      +T(I+IPQ,K,J+ISP+ISQ))
                T(I+IHE(J)+IFQ,K,J+ISQ)=T(I+IHE(J)+IFP,K,J+ISP)
!
              ELSEIF(HTM(I+IHE(J)+IFP,K,J+ISP).LT.0.99)THEN
                T(I+IHE(J)+IFP,K,J+ISP)=T(I,K,J)                        &
     &                                 +T(I+IPQ,K,J+ISP+ISQ)            &
     &                                 -T(I+IHE(J)+IFQ,K,J+ISQ)
!
              ELSEIF(HTM(I+IHE(J)+IFQ,K,J+ISQ).LT.0.99)THEN
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
!----------------------------------------------------------------------
!
              IF(I.EQ.IUP_ADH_J)THEN  ! Update advection H tendencies
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
!----------------------------------------------------------------------
!
                IF(KNTI_ADH.LT.N_IUPADH_J)THEN
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
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!***  UPSTREAM ADVECTION OF VELOCITY COMPONENTS
!----------------------------------------------------------------------
!----------------------------------------------------------------------
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
              IF(PP.LT.0.)THEN
                ISP=-1
              ELSE
                ISP= 1
              ENDIF
!
              IF(QP.LT.0.)THEN
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
!----------------------------------------------------------------------
!
        ENDIF upstream
!
!----------------------------------------------------------------------
!***
!***  END OF THIS UPSTREAM REGION
!***
!----------------------------------------------------------------------
!
!***  IF THE VERTICAL CFL CRITERION IS VIOLATED THEN LOCATE LIMITS
!***  BETWEEN WHICH TO SMOOTH THE TENDENCIES.
!
!-----------------------------------------------------------------------
      DO I=MYIS1,MYIE1
        KTOP_CFL_T(I)=0
        KBOT_CFL_T(I)=0
        KTOP_CFL_U(I)=0
        KBOT_CFL_U(I)=0
        KTOP_CFL_V(I)=0
        KBOT_CFL_V(I)=0
      ENDDO
!
      DO I=MYIS1,MYIE1
        VTB(I)=0.
        VUB(I)=0.
        VVB(I)=0.
      ENDDO
!
      DO K=KTE-1,KTS,-1
      DO I=MYIS1,MYIE1
!
!***  MASS POINTS
!
        CFL=PETDT(I,K,J)*DT*HBM2(I,J)                                   &
     &     /(0.5*(DPDE(I,K+1,J0_00)+DPDE(I,K,J0_00)))
!
        IF(ABS(CFL).GT.CFL_MAX)THEN
        write(0,*) 'mass CFL violated...I,K,J,CFL: ', I,K,J,CFL
          IF(KTOP_CFL_T(I).EQ.0)KTOP_CFL_T(I)=MIN(K,KTE-2)
          IF(KBOT_CFL_T(I).LT.K)KBOT_CFL_T(I)=MIN(K,KTE-2)
        ENDIF
!
!***  U COMPONENT
!
        CFL=(PETDT(I+IVW(J),K,J)+PETDT(I+IVE(J),K,J))*DT*VBM2(I,J)      &
     &     /((DPDE(I+IVW(J),K+1,J0_00)+DPDE(I+IVE(J),K+1,J0_00)         &
     &       +DPDE(I+IVW(J),K  ,J0_00)+DPDE(I+IVE(J),K  ,J0_00))*0.5)
!
        IF(ABS(CFL).GT.CFL_MAX)THEN
        write(0,*) 'U-comp CFL violated...I,K,J,CFL: ', I,K,J,CFL
          IF(KTOP_CFL_U(I).EQ.0)KTOP_CFL_U(I)=MIN(K,KTE-2)
          IF(KBOT_CFL_U(I).LT.K)KBOT_CFL_U(I)=MIN(K,KTE-2)
        ENDIF
!
!***  V COMPONENT
!
        CFL=(PETDT(I,K,J-1)+PETDT(I,K,J+1))*DT*VBM2(I,J)                &
     &     /((DPDE(I,K+1,J0_M1)+DPDE(I,K+1,J0_P1)                       &
     &       +DPDE(I,K  ,J0_M1)+DPDE(I,K  ,J0_P1))*0.5)
!
        IF(ABS(CFL).GT.CFL_MAX)THEN
        write(0,*) 'V-comp CFL violated...I,K,J,CFL: ', I,K,J,CFL
          IF(KTOP_CFL_V(I).EQ.0)KTOP_CFL_V(I)=MIN(K,KTE-2)
          IF(KBOT_CFL_V(I).LT.K)KBOT_CFL_V(I)=MIN(K,KTE-2)
        ENDIF
!
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!
!***  COMPUTE VERTICAL ADVECTION TENDENCIES
!
!----------------------------------------------------------------------
!
      DO K=KTE,KTS,-1
!
        DO I=MYIS,MYIE
          PETDTK(I,0)=0.
        ENDDO
!
        IF(K.GT.KTS)THEN
!
          DO I=MYIS1_P1,MYIE1_P1
            PETDTK(I,1)=PETDT(I,K-1,J+1)
            PETDTK(I,0)=PETDT(I,K-1,J)
            PETDTK(I,-1)=PETDT(I,K-1,J-1)
            TDN(I)=T(I,K-1,J)
            UDN(I)=U(I,K-1,J)
            VDN(I)=V(I,K-1,J)
          ENDDO
        ELSE
          DO I=MYIS,MYIE
            PETDTK(I,1)=0.
            PETDTK(I,0)=0.
            PETDTK(I,-1)=0.
          ENDDO
        ENDIF
!
      DO I=MYIS1,MYIE1
!
!***  TEMPERATURE
!
        T_UP=T(I,K,J)
        VTA=(TDN(I)-T_UP)*PETDTK(I,0)*F4D
        VAD_TEND_T(I,K)=(VTA+VTB(I))/DPDE(I,K,J0_00)+T_UP
        VTB(I)=VTA
!
!***  U COMPONENT
!
        U_UP=U(I,K,J)
        VUA=(UDN(I)-U_UP)                                               &
     &     *(PETDTK(I+IVW(J),0)+PETDTK(I+IVE(J),0))*F4D                   
        VAD_TEND_U(I,K)=(VUA+VUB(I))                                    &
     &                 /(DPDE(I+IVW(J),K,J0_00)                         &
     &                  +DPDE(I+IVE(J),K,J0_00))+U_UP
        VUB(I)=VUA
!
!***  V COMPONENT
!
        V_UP=V(I,K,J)
        VVA=(VDN(I)-V_UP)                                               &
     &     *(PETDTK(I,-1)+PETDTK(I,1))*F4D  
        VAD_TEND_V(I,K)=(VVA+VVB(I))                                    &
     &                 /(DPDE(I,K,J0_M1)+DPDE(I,K,J0_P1))+V_UP
        VVB(I)=VVA
!
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!***  SECOND (BACKWARD) STEP
!----------------------------------------------------------------------
!
      DO K=KTE,KTS,-1
        IF(K.GT.KTS)THEN
          DO I=MYIS1_P1,MYIE1_P1
            PETDTK(I,1)=PETDT(I,K-1,J+1)
            PETDTK(I,0)=PETDT(I,K-1,J)
            PETDTK(I,-1)=PETDT(I,K-1,J-1)
            TDN(I)=VAD_TEND_T(I,K-1)
            UDN(I)=VAD_TEND_U(I,K-1)
            VDN(I)=VAD_TEND_V(I,K-1)
          ENDDO
        ELSE
          DO I=MYIS,MYIE
            PETDTK(I,1)=0.
            PETDTK(I,0)=0.
            PETDTK(I,-1)=0.
          ENDDO
        ENDIF
!
        DO I=MYIS1,MYIE1
!
!***  TEMPERATURE
!
          T_UP=VAD_TEND_T(I,K)
          VTA=(TDN(I)-T_UP)*PETDTK(I,0)*F4D
          VAD_TEND_T(I,K)=(VTA+VTB(I))/DPDE(I,K,J0_00)
          VTB(I)=VTA
!
!***  U COMPONENT
!
          U_UP=VAD_TEND_U(I,K)
          VUA=(UDN(I)-U_UP)                                             &
     &       *(PETDTK(I+IVW(J),0)+PETDTK(I+IVE(J),0))*F4D
          VAD_TEND_U(I,K)=(VUA+VUB(I))                                  &
     &                   /(DPDE(I+IVW(J),K,J0_00)                       &
     &                    +DPDE(I+IVE(J),K,J0_00))
          VUB(I)=VUA
!
!***  V COMPONENT
!
          V_UP=VAD_TEND_V(I,K)
          VVA=(VDN(I)-V_UP)                                             &
     &       *(PETDTK(I,-1)+PETDTK(I,1))*F4D
          VAD_TEND_V(I,K)=(VVA+VVB(I))                                  &
     &                   /(DPDE(I,K,J0_M1)+DPDE(I,K,J0_P1))
          VVB(I)=VVA
!
        ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!
!***  IF THE CFL CRITERION IS VIOLATED THEN VERTICALLY SMOOTH
!***  THE TENDENCIES
!
!-----------------------------------------------------------------------
!
!***  TEMPERATURE
!
      DO I=MYIS1,MYIE1
!
        IF(KTOP_CFL_T(I).GT.0)THEN
          KSTART=KTOP_CFL_T(I)
          KSTOP =MIN(KBOT_CFL_T(I),KTE-1)
!
          DO K=KSTOP,KSTART,-1
            VAD_TNDX_T(K)=(VAD_TEND_T(I,K+1)+VAD_TEND_T(I,K-1)          &
     &                    +2.*VAD_TEND_T(I,K))*0.25
          ENDDO
!
          DO K=KSTART,KSTOP
            VAD_TEND_T(I,K)=VAD_TNDX_T(K)
          ENDDO
!
        ENDIF
!
      ENDDO
!
!***  U COMPONENT
!
      DO I=MYIS1,MYIE1
!
        IF(KTOP_CFL_U(I).GT.0)THEN
          KSTART=KTOP_CFL_U(I)
          KSTOP =MIN(KBOT_CFL_U(I),KTE-1)
!
          DO K=KSTOP,KSTART,-1
            VAD_TNDX_U(K)=(VAD_TEND_U(I,K+1)+VAD_TEND_U(I,K-1)          &
     &                    +2.*VAD_TEND_U(I,K))*0.25
          ENDDO
          DO K=KSTART,KSTOP
            VAD_TEND_U(I,K)=VAD_TNDX_U(K)
          ENDDO
!
        ENDIF
      ENDDO
!
!***  V COMPONENT
!
      DO I=MYIS1,MYIE1
!
        IF(KTOP_CFL_V(I).GT.0)THEN
          KSTART=KTOP_CFL_V(I)
          KSTOP =MIN(KBOT_CFL_V(I),KTE-1)
!
          DO K=KSTOP,KSTART,-1
            VAD_TNDX_V(K)=(VAD_TEND_V(I,K+1)+VAD_TEND_V(I,K-1)          &
     &                    +2.*VAD_TEND_V(I,K))*0.25
          ENDDO
          DO K=KSTART,KSTOP
            VAD_TEND_V(I,K)=VAD_TNDX_V(K)
          ENDDO
!
        ENDIF
      ENDDO
!
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      ENDDO main_integration
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  SAVE THE OLD VALUES FOR TIMESTEPPING
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!***  FINALLY UPDATE THE PROGNOSTIC VARIABLES
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
      END SUBROUTINE ADVE
!----------------------------------------------------------------------
