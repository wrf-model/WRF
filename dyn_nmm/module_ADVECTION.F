!#define BIT_FOR_BIT
!
!NCEP_MESO:MODEL_LAYER: HORIZONTAL AND VERTICAL ADVECTION
!
!----------------------------------------------------------------------
!
      MODULE MODULE_ADVECTION
!
!----------------------------------------------------------------------
      USE MODULE_MPP
      USE module_model_constants
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
      CONTAINS
!
!********************************************************************** 
      SUBROUTINE ADVE(NTSD,DT,DETA1,DETA2,PDTOP                        &
                     ,CURV,F,FAD,F4D,EM_LOC,EMT_LOC,EN,ENT,DX,DY       &
                     ,HTM,HBM2,VTM,VBM2                                &
                     ,T,U,V,PDSLO,TOLD,UOLD,VOLD                       &
                     ,PETDT,UPSTRM                                     &
                     ,FEW,FNS,FNE,FSE                                  &
                     ,ADT,ADU,ADV                                      &
                     ,N_IUP_H,N_IUP_V                                  &
                     ,N_IUP_ADH,N_IUP_ADV                              &
                     ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                      &
                     ,IHE,IHW,IVE,IVW,INDX3_WRK                        &
                     ,IDS,IDE,JDS,JDE,KDS,KDE                          &
                     ,IMS,IME,JMS,JME,KMS,KME                          &
                     ,ITS,ITE,JTS,JTE,KTS,KTE)
!**********************************************************************
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
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V        &
                                               ,N_IUP_ADH,N_IUP_ADV
      INTEGER, DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V    &
                                                       ,IUP_ADH,IUP_ADV
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
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CURV,DX,F,FAD,HBM2 &
                                                   ,PDSLO,VBM2
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PETDT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: HTM,VTM
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: T,TOLD  &
                                                              ,U,UOLD  &
                                                              ,V,VOLD
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: ADT,ADU   &
                                                            ,ADV       &
                                                            ,FEW,FNE   &
                                                            ,FNS,FSE
!
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      LOGICAL :: UPSTRM
!
      INTEGER :: I,IEND,IFP,IFQ,II,IPQ,ISP,ISQ,ISTART                  &
                ,IUP_ADH_J,IVH,IVL                                     &
                ,J,J1,JA,JAK,JEND,JGLOBAL,JJ,JKNT,JP2,JSTART           &
                ,K,KNTI_ADH,KSTART,KSTOP                               &
                ,N,N_IUPH_J,N_IUPADH_J,N_IUPADV_J
!
      INTEGER :: MY_IS_GLB,MY_IE_GLB,MY_JS_GLB,MY_JE_GLB
!
      INTEGER :: J0_P3,J0_P2,J0_P1,J0_00,J0_M1,J1_P2,J1_P1,J1_00,J1_M1 &
                ,J2_P1,J2_00,J2_M1,J3_P2,J3_P1,J3_00                   &
                ,J4_P1,J4_00,J4_M1,J5_00,J5_M1,J6_P1,J6_00
!
      INTEGER,DIMENSION(ITS-5:ITE+5) :: KBOT_CFL_T,KTOP_CFL_T              &
                                   ,KBOT_CFL_U,KTOP_CFL_U              &
                                   ,KBOT_CFL_V,KTOP_CFL_V
!
      INTEGER,DIMENSION(ITS-5:ITE+5,KTS:KTE) :: ISPA,ISQA
!
      REAL :: ARRAY3_X,CFL,DPDE_P3,F0,F1,F2,F3                         &
             ,FEW_00,FEW_P1,FNE_X,FNS_P1,FNS_X,FPP,FSE_X               &
             ,HM,PP,QP,RDPD,RDPDX,RDPDY,T_UP,TEMPA,TEMPB,TTA,TTB       &
             ,U_UP,UDY_P1,UDY_X,VXD_X,VDX_P2,V_UP,VDX_X,VM,VTA,VUA,VVA
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE) :: ARRAY0,ARRAY1             &
                                            ,ARRAY2,ARRAY3             &
                                        ,VAD_TEND_T,VAD_TEND_U         &
                                        ,VAD_TEND_V
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
! initialize the temporaries to avoid floating point exceptions on 
! machines that care

#ifdef FLOATSAFE
      ARRAY0          = 0.
      ARRAY1          = 0.
      ARRAY2          = 0.
      ARRAY3          = 0.
      VAD_TEND_T      = 0.
      VAD_TEND_U      = 0.
      VAD_TEND_V      = 0.
      TEW             = 0.
      UEW             = 0.
      VEW             = 0.
      VTB             = 0.
      VUB             = 0.
      VVB             = 0.
      VAD_TNDX_T      = 0.
      VAD_TNDX_U      = 0.
      VAD_TNDX_V      = 0.
      PETDTK          = 0.
      TDN             = 0.
      UDN             = 0.
      VDN             = 0.
      DPDE            = 0.
      TST             = 0.
      UDY             = 0.
      UST             = 0.
      VDX             = 0.
      VST             = 0.
      TNS             = 0.
      UNS             = 0.
      VNS             = 0.
      TNE             = 0.
      UNE             = 0.
      VNE             = 0.
      TSE             = 0.
      USE             = 0.
      VSE             = 0.
#endif

!----------------------------------------------------------------------
! I should have done all this but right now the loops are still
! using the non-thread safe versions in module_MPP.F  JM
! what about myie and myje?  jw - not stored myis_p2,myie_p2...
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
      DO I=ITS,ITE
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
        DO I=ITS,ITE
          DPDE(I,K,N)=0.
        ENDDO
        ENDDO
      ENDDO
!
!***  TYPE 1
!
      DO N=-2,2
        DO K=KTS,KTE
        DO I=ITS,ITE
          TST(I,K,N)=0.
          UST(I,K,N)=0.
          VST(I,K,N)=0.
          VDX(I,K,N)=0.
        ENDDO
        ENDDO
      ENDDO
!
!***  TYPES 5 AND 6
!
      DO N=-1,0
        DO K=KTS,KTE
        DO I=ITS,ITE
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
!!!   JSTART=MYJS_P2
!!!   JEND=MYJE_P2
!
!!!   IF(IBROW==1)THEN
!!!     JSTART=MYJS2
!!!   ELSEIF(ITROW==1)THEN
!!!     JEND=MYJE2
!!!   ENDIF
!
!!!   call f_hpmstart(10,"ADVE_main")
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
          TEMPA=(UDY(I+IHE(JJ),K,J)+VDX(I+IHE(JJ),K,J))                &
               +(UDY(I,K,J+1)      +VDX(I,K,J+1))
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
          TEMPB=(UDY(I+IHE(JJ),K,J)-VDX(I+IHE(JJ),K,J))                &
               +(UDY(I,K,J-1)      -VDX(I,K,J-1))
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
          UNS(I,K,J)=(FNS(I+IHW(JJ),K,JJ)+FNS(I+IHE(JJ),K,JJ))         &
                    *(UST(I,K,J+1)-UST(I,K,J-1))
          VNS(I,K,J)=(FNS(I,K,JJ-1)+FNS(I,K,JJ+1))                     &
                    *(VST(I,K,J+1)-VST(I,K,J-1))
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
        UNE(I,K,-1)=(FNE(I+IVW(JJ),K,JJ)+FNE(I+IVE(JJ),K,JJ))          &
                   *(UST(I+IVE(JJ),K,0)-UST(I,K,-1))
        USE(I,K,0)=(FSE(I+IVW(JJ+1),K,JJ+1)+FSE(I+IVE(JJ+1),K,JJ+1))   &
                  *(UST(I+IVE(JJ+1),K,-1)-UST(I,K,0))
        VNE(I,K,-1)=(FNE(I,K,JJ-1)+FNE(I,K,JJ+1))                      &
                   *(VST(I+IVE(JJ),K,0)-VST(I,K,-1))
        VSE(I,K,0)=(FSE(I,K,JJ)+FSE(I,K,JJ+2))                         &
                  *(VST(I+IVE(JJ+1),K,-1)-VST(I,K,0))
      ENDDO
      ENDDO
!
      JKNT=0
!!!   call f_hpmstop(10)
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      main_integration : DO J=JSTART,JEND
!!!   call f_hpmstart(10,"ADVE_main")
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
!
!jm
! only the J ones are used here
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
!!!   call f_hpmstart(2,"BIGLOOP")
      DO K=KTS,KTE
      DO I=MYIS_P3,MYIE_P3


        TEMPA=(UDY(I+IHE(J+1),K,J1_P1)+VDX(I+IHE(J+1),K,J1_P1))       &
             +(UDY(I,K,J1_P2)         +VDX(I,K,J1_P2))
        FNE(I,K,J+1)=TEMPA*(DPDE(I,K,J0_P1)+DPDE(I+IHE(J+1),K,J0_P2))
!
!----------------------------------------------------------------------
        TEMPB=(UDY(I+IHE(J+2),K,J1_P2)-VDX(I+IHE(J+2),K,J1_P2))        &
             +(UDY(I,K,J1_P1)         -VDX(I,K,J1_P1))
        FSE(I,K,J+2)=TEMPB*(DPDE(I,K,J0_P2)+DPDE(I+IHE(J),K,J0_P1))
!
!----------------------------------------------------------------------
        FNS_P1=FNS(I,K,J+1)
        TNS(I,K,J4_P1)=FNS_P1*(TST(I,K,J1_P2)-TST(I,K,J1_00))
!
!----------------------------------------------------------------------
        UDY_P1=U(I,K,J+1)*DY
!write(0,*)'i,j,k ',i,j,k
!write(0,*)'U(I,K,J+1) ',U(I,K,J+1)
!write(0,*)'UDY_P1 ',UDY_P1
!write(0,*)'DPDE(I+IVW(J+1),K,J0_P1) ',DPDE(I+IVW(J+1),K,J0_P1)
!write(0,*)'DPDE(I+IVE(J+1),K,J0_P1) ',DPDE(I+IVE(J+1),K,J0_P1)
!write(0,*)'IVW(J+1) ',IVW(J+1),' IVE(J+1) ',IVE(J+1)
!write(0,*)'J0_P1  ',J0_P1
        FEW(I,K,J+1)=UDY_P1*(DPDE(I+IVW(J+1),K,J0_P1)                  &
                              +DPDE(I+IVE(J+1),K,J0_P1))
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
!!!!  call f_hpmstop(2)
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
        UEW(I,K)=(FEW(I+IHW(J),K,J)+FEW(I+IHE(J),K,J))                 &
                *(UST(I+IHE(J),K,J1_00)-UST(I+IHW(J),K,J1_00))
        UNS(I,K,J4_P1)=(FNS(I+IHW(J+1),K,J+1)                          &
                       +FNS(I+IHE(J+1),K,J+1))                         &
                      *(UST(I,K,J1_P2)-UST(I,K,J1_00))
        VEW(I,K)=(FEW(I,K,J-1)+FEW(I,K,J+1))                           &
                *(VST(I+IHE(J),K,J1_00)-VST(I+IHW(J),K,J1_00))
        VNS(I,K,J4_P1)=(FNS(I,K,J)+FNS(I,K,J+2))                       &
                      *(VST(I,K,J1_P2)-VST(I,K,J1_00))
!
!----------------------------------------------------------------------
!***  THE FOLLOWING NE AND SE FLUXES ARE TIED TO V POINTS AND ARE
!***  LOCATED JUST TO THE NE AND SE OF THE GIVEN I,J.
!----------------------------------------------------------------------
!
        UNE(I,K,J5_00)=(FNE(I+IVW(J),K,J)+FNE(I+IVE(J),K,J))           &
                      *(UST(I+IVE(J),K,J1_P1)-UST(I,K,J1_00))
        USE(I,K,J6_P1)=(FSE(I+IVW(J+1),K,J+1)                          &
                       +FSE(I+IVE(J+1),K,J+1))                         &
                      *(UST(I+IVE(J+1),K,J1_00)-UST(I,K,J1_P1))
        VNE(I,K,J5_00)=(FNE(I,K,J-1)+FNE(I,K,J+1))                     &
                      *(VST(I+IVE(J),K,J1_P1)-VST(I,K,J1_00))
        VSE(I,K,J6_P1)=(FSE(I,K,J)+FSE(I,K,J+2))                       &
                      *(VST(I+IVE(J+1),K,J1_00)-VST(I,K,J1_P1))
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
          ADT(I,K,J)=(TEW(I+IHW(J),K)+TEW(I+IHE(J),K)                  &
                     +TNS(I,K,J4_M1)+TNS(I,K,J4_P1)                    &
                     +TNE(I+IHW(J),K,J5_M1)+TNE(I,K,J5_00)             &
                     +TSE(I,K,J6_00)+TSE(I+IHW(J),K,J6_P1))            &
                     *RDPD*FAD(I,J)
!
        ENDDO
        ENDDO
!
!----------------------------------------------------------------------
!***  COMPUTE THE ADVECTION TENDENCIES FOR U AND V.
!***  THE AD ARRAYS ARE ON VELOCITY POINTS.
!----------------------------------------------------------------------
!
!jw        ISTART=MYIS_P2
        IF(ITS==IDS)ISTART=3+MOD(JJ+1,2)
!jw        IEND=MYIE_P2
!jw        IF( ITE == IDE ) IEND=MYIE-3
!
        DO K=KTS,KTE
        DO I=ISTART,IEND
          RDPDX=1./(DPDE(I+IVW(J),K,J0_00)+DPDE(I+IVE(J),K,J0_00))
          RDPDY=1./(DPDE(I,K,J0_M1)+DPDE(I,K,J0_P1))
!
          ADU(I,K,J)=(UEW(I+IVW(J),K)+UEW(I+IVE(J),K)                  &
                     +UNS(I,K,J4_M1)+UNS(I,K,J4_P1)                    &
                     +UNE(I+IVW(J),K,J5_M1)+UNE(I,K,J5_00)             &
                     +USE(I,K,J6_00)+USE(I+IVW(J),K,J6_P1))            &
                     *RDPDX*FAD(I+IVW(J),J)
!
          ADV(I,K,J)=(VEW(I+IVW(J),K)+VEW(I+IVE(J),K)                  &
                     +VNS(I,K,J4_M1)+VNS(I,K,J4_P1)                    &
                     +VNE(I+IVW(J),K,J5_M1)+VNE(I,K,J5_00)             &
                     +VSE(I,K,J6_00)+VSE(I+IVW(J),K,J6_P1))            &
                     *RDPDY*FAD(I+IVW(J),J)
!
        ENDDO
        ENDDO
!
      ENDIF
!!!   call f_hpmstop(10)
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
!!!   call f_hpmstart(11,"ADVE_upstr")
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
            DO II=1,N_IUPH_J
              I=IUP_H(II,J)
              TTA=EMT_LOC(J)*(UST(I,K,J1_M1)+UST(I+IHW(J),K,J1_00)     &
                             +UST(I+IHE(J),K,J1_00)+UST(I,K,J1_P1))
              TTB=ENT       *(VST(I,K,J1_M1)+VST(I+IHW(J),K,J1_00)     &
                             +VST(I+IHE(J),K,J1_00)+VST(I,K,J1_P1))
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
            IUP_ADH_J=IUP_ADH(1,J)
!
            DO II=1,N_IUPH_J
              I=IUP_H(II,J)
!
              ISP=ISPA(I,K)
              ISQ=ISQA(I,K)
              IFP=(ISP-1)/2
              IFQ=(-ISQ-1)/2
              IPQ=(ISP-ISQ)/2
!
              IF(HTM(I+IHE(J)+IFP,K,J+ISP)                             &
                *HTM(I+IHE(J)+IFQ,K,J+ISQ)                             &
                *HTM(I+IPQ,K,J+ISP+ISQ).GT.0.1)THEN
                 GO TO 150
              ENDIF
!
              IF(HTM(I+IHE(J)+IFP,K,J+ISP)                             &
                +HTM(I+IHE(J)+IFQ,K,J+ISQ)                             &
                +HTM(I+IPQ,K,J+ISP+ISQ).LT.0.1)THEN 
!
                T(I+IHE(J)+IFP,K,J+ISP)=T(I,K,J)
                T(I+IHE(J)+IFQ,K,J+ISQ)=T(I,K,J)
                T(I+IPQ,K,J+ISP+ISQ)=T(I,K,J)
!
              ELSEIF                                                   &
              (HTM(I+IHE(J)+IFP,K,J+ISP)+HTM(I+IPQ,K,J+ISP+ISQ)        &
              .LT.0.99)THEN
!
                T(I+IHE(J)+IFP,K,J+ISP)=T(I,K,J)
                T(I+IPQ,K,J+ISP+ISQ)=T(I+IHE(J)+IFQ,K,J+ISQ)
!
              ELSEIF                                                   &
              (HTM(I+IHE(J)+IFQ,K,J+ISQ)+HTM(I+IPQ,K,J+ISP+ISQ)        &
              .LT.0.99)THEN
!
                T(I+IHE(J)+IFQ,K,J+ISQ)=T(I,K,J)
                T(I+IPQ,K,J+ISP+ISQ)=T(I+IHE(J)+IFP,K,J+ISP)
!
              ELSEIF                                                   &
              (HTM(I+IHE(J)+IFP,K,J+ISP)                               &
              +HTM(I+IHE(J)+IFQ,K,J+ISQ).LT.0.99)THEN
                T(I+IHE(J)+IFP,K,J+ISP)=0.5*(T(I,K,J)                  &
                                            +T(I+IPQ,K,J+ISP+ISQ))
                T(I+IHE(J)+IFQ,K,J+ISQ)=T(I+IHE(J)+IFP,K,J+ISP)
!
              ELSEIF(HTM(I+IHE(J)+IFP,K,J+ISP).LT.0.99)THEN
                T(I+IHE(J)+IFP,K,J+ISP)=T(I,K,J)                       &
                                       +T(I+IPQ,K,J+ISP+ISQ)           &
                                       -T(I+IHE(J)+IFQ,K,J+ISQ)
!
              ELSEIF(HTM(I+IHE(J)+IFQ,K,J+ISQ).LT.0.99)THEN
                T(I+IHE(J)+IFQ,K,J+ISQ)=T(I,K,J)                       &
                                       +T(I+IPQ,K,J+ISP+ISQ)           &
                                       -T(I+IHE(J)+IFP,K,J+ISP)
!
              ELSE
                T(I+IPQ,K,J+ISP+ISQ)=T(I+IHE(J)+IFP,K,J+ISP)           &
                                    +T(I+IHE(J)+IFQ,K,J+ISQ)           &
                                    -T(I,K,J)
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
                ADT(I,K,J)=F0*T(I,K,J)                                 &
                          +F1*T(I+IHE(J)+IFP,K,J+ISP)                  &
                          +F2*T(I+IHE(J)+IFQ,K,J+ISQ)                  &
                          +F3*T(I+IPQ,K,J+ISP+ISQ)
!
!----------------------------------------------------------------------
!
                IF(KNTI_ADH.LT.N_IUPADH_J)THEN
                  KNTI_ADH=KNTI_ADH+1
                  IUP_ADH_J=IUP_ADH(KNTI_ADH,J)
                ENDIF
!
              ENDIF  ! End of advection H tendency IF block
!
            ENDDO  ! End of I loop
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
            DO II=1,N_IUPADV_J
              I=IUP_ADV(II,J)
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
              ADU(I,K,J)=F0*U(I,K,J)                                   &
                        +F1*U(I+IVE(J)+IFP,K,J+ISP)                    &
                        +F2*U(I+IVE(J)+IFQ,K,J+ISQ)                    &
                        +F3*U(I+IPQ,K,J+ISP+ISQ)
! 
              ADV(I,K,J)=F0*V(I,K,J)                                   &
                        +F1*V(I+IVE(J)+IFP,K,J+ISP)                    &
                        +F2*V(I+IVE(J)+IFQ,K,J+ISQ)                    &
                        +F3*V(I+IPQ,K,J+ISP+ISQ)
!
            ENDDO
!
          ENDDO  !  End of K loop
!
!----------------------------------------------------------------------
!
        ENDIF upstream
!!!   call f_hpmstop(11)
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
           /(0.5*(DPDE(I,K+1,J0_00)+DPDE(I,K,J0_00)))
!
        IF(ABS(CFL).GT.CFL_MAX)THEN
          IF(KTOP_CFL_T(I).EQ.0)KTOP_CFL_T(I)=MIN(K,KTE-2)
          IF(KBOT_CFL_T(I).LT.K)KBOT_CFL_T(I)=MIN(K,KTE-2)
        ENDIF
!
!***  U COMPONENT
!
        CFL=(PETDT(I+IVW(J),K,J)+PETDT(I+IVE(J),K,J))*DT*VBM2(I,J)     &
           /((DPDE(I+IVW(J),K+1,J0_00)+DPDE(I+IVE(J),K+1,J0_00)        &
             +DPDE(I+IVW(J),K  ,J0_00)+DPDE(I+IVE(J),K  ,J0_00))*0.5)
!
        IF(ABS(CFL).GT.CFL_MAX)THEN
          IF(KTOP_CFL_U(I).EQ.0)KTOP_CFL_U(I)=MIN(K,KTE-2)
          IF(KBOT_CFL_U(I).LT.K)KBOT_CFL_U(I)=MIN(K,KTE-2)
        ENDIF
!
!***  V COMPONENT
!
        CFL=(PETDT(I,K,J-1)+PETDT(I,K,J+1))*DT*VBM2(I,J)               &
           /((DPDE(I,K+1,J0_M1)+DPDE(I,K+1,J0_P1)                      &
             +DPDE(I,K  ,J0_M1)+DPDE(I,K  ,J0_P1))*0.5)
!
        IF(ABS(CFL).GT.CFL_MAX)THEN
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
        VUA=(UDN(I)-U_UP)                                              &
           *(PETDTK(I+IVW(J),0)+PETDTK(I+IVE(J),0))*F4D                   
        VAD_TEND_U(I,K)=(VUA+VUB(I))                                   &
                       /(DPDE(I+IVW(J),K,J0_00)                        &
                        +DPDE(I+IVE(J),K,J0_00))+U_UP
        VUB(I)=VUA
!
!***  V COMPONENT
!
        V_UP=V(I,K,J)
        VVA=(VDN(I)-V_UP)                                              &
           *(PETDTK(I,-1)+PETDTK(I,1))*F4D  
        VAD_TEND_V(I,K)=(VVA+VVB(I))                                   &
                       /(DPDE(I,K,J0_M1)+DPDE(I,K,J0_P1))+V_UP
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
          VUA=(UDN(I)-U_UP)                                            &
             *(PETDTK(I+IVW(J),0)+PETDTK(I+IVE(J),0))*F4D
          VAD_TEND_U(I,K)=(VUA+VUB(I))                                 &
                         /(DPDE(I+IVW(J),K,J0_00)                      &
                          +DPDE(I+IVE(J),K,J0_00))
          VUB(I)=VUA
!
!***  V COMPONENT
!
          V_UP=VAD_TEND_V(I,K)
          VVA=(VDN(I)-V_UP)                                            &
             *(PETDTK(I,-1)+PETDTK(I,1))*F4D
          VAD_TEND_V(I,K)=(VVA+VVB(I))                                 &
                         /(DPDE(I,K,J0_M1)+DPDE(I,K,J0_P1))
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
                          +2.*VAD_TEND_T(I,K))*0.25
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
            VAD_TNDX_U(K)=(VAD_TEND_U(I,K+1)+VAD_TEND_U(I,K-1)         &
                          +2.*VAD_TEND_U(I,K))*0.25
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
            VAD_TNDX_V(K)=(VAD_TEND_V(I,K+1)+VAD_TEND_V(I,K-1)         &
                          +2.*VAD_TEND_V(I,K))*0.25
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
        ADU(I,K,J)=(VAD_TEND_U(I,K)+2.*ADU(I,K,J)+VST(I,K,J1_00)*FPP)  &
                   *VM
        ADV(I,K,J)=(VAD_TEND_V(I,K)+2.*ADV(I,K,J)-UST(I,K,J1_00)*FPP)  &
                   *VM
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
!
!**********************************************************************
      SUBROUTINE VAD2(NTSD,DT,IDTAD,DX,DY                              &
                     ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP               &
                     ,HBM2,LMH                                         &
                     ,Q,Q2,CWM,PETDT                                   &
                     ,N_IUP_H,N_IUP_V                                  &
                     ,N_IUP_ADH,N_IUP_ADV                              &
                     ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                      &
                     ,IHE,IHW,IVE,IVW,INDX3_WRK                        &
                     ,IDS,IDE,JDS,JDE,KDS,KDE                          &
                     ,IMS,IME,JMS,JME,KMS,KME                          &
                     ,ITS,ITE,JTS,JTE,KTS,KTE)
!**********************************************************************
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
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V         &
                                              ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V     &
                                                      ,IUP_ADH,IUP_ADV
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
      REAL*8 :: ADDT,AFRP,D2PQE,D2PQQ,D2PQW,DEP,DETAP,DPDN,DPUP,DQP    &
             ,DWP,E00,E4P,EP,EP0,HADDT,HBM2IJ                          &
             ,Q00,Q4P,QP,QP0                                           &
             ,RFACEK,RFACQK,RFACWK,RFC,RR                              &
             ,SUMNE,SUMNQ,SUMNW,SUMPE,SUMPQ,SUMPW                      &
             ,W00,W4P,WP,WP0
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,DQL,DWL,E3,E4,PETDTK          &
                                ,RFACE,RFACQ,RFACW,Q3,Q4,W3,W4
!
!**********************************************************************
!
      MYJS2   =MAX(JDS+2,JTS  )
      MYJE2   =MIN(JDE-2,JTE  )
      MYIS1_P1=MAX(IDS+1,ITS-1)
      MYIE1_P1=MIN(IDE-1,ITE+1)
!
!----------------------------------------------------------------------
!
      ADDT=REAL(IDTAD)*DT
!
!----------------------------------------------------------------------
!
!jm still needs to be converted
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
            RR=ABS(RR/((AETA1(LLAP)-AETA1(K))*PDTOP                    &
                      +(AETA2(LLAP)-AETA2(K))*PDSL(I,J)))
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
            RFC=(DETA1(KTE-1)*PDTOP+DETA2(KTE-1)*PDSL(I,J))            &
               /(DETA1(KTE  )*PDTOP+DETA2(KTE  )*PDSL(I,J))
            DQL(KTE)=-DQL(KTE+1)*RFC
            DWL(KTE)=-DWL(KTE+1)*RFC
            DEL(KTE)=-DEL(KTE+1)*RFC
          ENDIF
        ENDIF
!
        IF(BOT)THEN
          IF(LA(KOFF+2).LT.0)THEN
            RFC=(DETA1(KOFF+2)*PDTOP+DETA2(KOFF+2)*PDSL(I,J))          &
               /(DETA1(KOFF+1)*PDTOP+DETA2(KOFF+1)*PDSL(I,J))
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
          DPDN=(AETA1(K+LAP)-AETA1(K))*PDTOP                           &
              +(AETA2(K+LAP)-AETA2(K))*PDSL(I,J)
          DPUP=(AETA1(K)-AETA1(K-LAP))*PDTOP                           &
              +(AETA2(K)-AETA2(K-LAP))*PDSL(I,J)
!
          AFRP=2.*AFR(K)*DPDN*DPDN/(DPDN+DPUP)
          D2PQQ=((Q4(K+LAP)-Q4P)/DPDN                                  &
                -(Q4P-Q4(K-LAP))/DPUP)*AFRP
          D2PQW=((W4(K+LAP)-W4P)/DPDN                                  &
                -(W4P-W4(K-LAP))/DPUP)*AFRP
          D2PQE=((E4(K+LAP)-E4P)/DPDN                                  &
                -(E4P-E4(K-LAP))/DPUP)*AFRP
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
        Q2(I,KTE,J)=MAX(E3(KTE)+E3(KTE)-EPSQ2,EPSQ2)*HBM2IJ            &
                   +Q2(I,KTE,J)*(1.-HBM2IJ)
        DO K=KTE-1,KOFF+2,-1
          Q2(I,K,J)=MAX(E3(K)+E3(K)-Q2(I,K+1,J),EPSQ2)*HBM2IJ          &
                   +Q2(I,K,J)*(1.-HBM2IJ)
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
!**********************************************************************
      SUBROUTINE HAD2(                                                 &
#if defined(DM_PARALLEL)
                      domdesc ,                                        &
#endif
                      NTSD,DT,IDTAD,DX,DY                              &
                     ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP               &
                     ,HTM,HBM2,HBM3,LMH                                &
                     ,Q,Q2,CWM,U,V,Z,HYDRO                             &
                     ,N_IUP_H,N_IUP_V                                  &
                     ,N_IUP_ADH,N_IUP_ADV                              &
                     ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                      &
                     ,IHE,IHW,IVE,IVW,INDX3_WRK                        &
                     ,IDS,IDE,JDS,JDE,KDS,KDE                          &
                     ,IMS,IME,JMS,JME,KMS,KME                          &
                     ,ITS,ITE,JTS,JTE,KTS,KTE)
!**********************************************************************
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
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V         &
                                              ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V     &
                                                      ,IUP_ADH,IUP_ADV
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
      integer domdesc
#endif
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL)
      logical, external :: wrf_dm_on_monitor
      integer ii
      real, dimension(ims:ime,kms:kme,jms:jme,6) :: xsums_l
      real, dimension(ids:ide,kds:kde,jds:jde,6) :: xsums_g
#endif
!
      LOGICAL :: BOT,TOP
!
      INTEGER :: I,IRECV,J,JFP,JFQ,K,KOFF,LAP,LLAP
!
      INTEGER,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: IFPA,IFPF          &
                                                   ,IFQA,IFQF          &
                                                   ,JFPA,JFPF          &
                                                   ,JFQA,JFQF
!
      REAL :: ADDT,AFRP,CRIT,D2PQE,D2PQQ,D2PQW,DEP,DESTIJ,DQP,DQSTIJ   &
             ,DVOLP,DWP,DWSTIJ,DZA,DZB,E00,E0Q,E1X,E2IJ,E4P,ENH,EP,EP0 &
             ,ESTIJ,FPQ,HAFP,HAFQ,HBM2IJ,HM,HTMIKJ,PP,PPQ00,Q00,Q0Q    &
             ,Q1IJ,Q4P,QP,QP0,QSTIJ,RDY,RFACEK,RFACQK,RFACWK,RFC       &
             ,RFEIJ,RFQIJ,RFWIJ,RR,SLOPAC,SPP,SQP,SSA,SSB,SUMNE,SUMNQ  &
             ,SUMNW,SUMPE,SUMPQ,SUMPW,TTA,TTB,W00,W0Q,W1IJ,W4P,WP,WP0  &
             ,WSTIJ
!
      DOUBLE PRECISION,DIMENSION(6,KTE-KTS+1) :: GSUMS,XSUMS
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,DQL,DWL,E3,E4                 &
                                ,RFACE,RFACQ,RFACW,Q3,Q4,W3,W4
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: DARE,EMH
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,JTS-5:JTE+5) :: AFP,AFQ,DEST,DQST     &
                                                ,DVOL,DWST,E1,E2       &
                                                ,Q1,W1
!
!**********************************************************************
!
!----------------------------------------------------------------------
      MYIS    =MAX(IDS  ,ITS  )
      MYIE    =MIN(IDE  ,ITE  )
      MYJS    =MAX(JDS  ,JTS  )
      MYJE    =MIN(JDE  ,JTE  )
!
      MYIS1   =MAX(IDS+1,ITS  )
      MYIE1   =MIN(IDE-1,ITE  )
      MYJS2   =MAX(JDS+2,JTS  )
      MYJE2   =MIN(JDE-2,JTE  )
!
      MYIS_P2 =MAX(IDS  ,ITS-2)
      MYIE_P2 =MIN(IDE  ,ITE+2)
      MYJS_P3 =MAX(JDS  ,JTS-3)
      MYJE_P3 =MIN(JDE  ,JTE+3)
!
      MYIS1_P1=MAX(IDS+1,ITS-1)
      MYIE1_P1=MIN(IDE-1,ITE+1)
      MYJS2_P1=MAX(JDS+2,JTS-1)
      MYJE2_P1=MIN(JDE-2,JTE+1)
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
        TTA=(U(I,K,J-1)+U(I+IHW(J),K,J)+U(I+IHE(J),K,J)+U(I,K,J+1))    &
            *EMH(I,J)*HBM2(I,J)
        TTB=(V(I,K,J-1)+V(I+IHW(J),K,J)+V(I+IHE(J),K,J)+V(I,K,J+1))    &
            *ENH*HBM2(I,J)
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
                   *HTM(I+1,K,J)*HTM(I,K,J+2)*0.25
        PP=ABS(SPP)
        QP=ABS(SQP)
!
        AFP(I,K,J)=(((FF4*PP+FF3)*PP+FF2)*PP+FF1)*PP
        AFQ(I,K,J)=(((FF4*QP+FF3)*QP+FF2)*QP+FF1)*QP
!
        Q1(I,K,J)=(Q  (IFPA(I,K,J),K,JFPA(I,K,J))-Q  (I,K,J))*PP       &
                 +(Q  (IFQA(I,K,J),K,JFQA(I,K,J))-Q  (I,K,J))*QP       &
                 +(Q  (I,K,J-2)+Q  (I,K,J+2)                           &
                  -Q  (I-1,K,J)-Q  (I+1,K,J))*FPQ                      &
                 +Q(I,K,J)
!
        W1(I,K,J)=(CWM(IFPA(I,K,J),K,JFPA(I,K,J))-CWM(I,K,J))*PP       &
                 +(CWM(IFQA(I,K,J),K,JFQA(I,K,J))-CWM(I,K,J))*QP       &
                 +(CWM(I,K,J-2)+CWM(I,K,J+2)                           &
                  -CWM(I-1,K,J)-CWM(I+1,K,J))*FPQ                      &
                 +CWM(I,K,J)
!
        E2(I,K,J)=(E1 (IFPA(I,K,J),K,JFPA(I,K,J))-E1 (I,K,J))*PP       &
                 +(E1 (IFQA(I,K,J),K,JFQA(I,K,J))-E1 (I,K,J))*QP       &
                 +(E1 (I,K,J-2)+E1 (I,K,J+2)                           &
                  -E1 (I-1,K,J)-E1 (I+1,K,J))*FPQ                      &
                 +E1(I,K,J)
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
      D2PQQ=(Q1(IFPA(I,K,J),K,JFPA(I,K,J))-Q1IJ                        &
            -Q1IJ+Q1(IFPF(I,K,J),K,JFPF(I,K,J)))                       &
            *HAFP                                                      &
           +(Q1(IFQA(I,K,J),K,JFQA(I,K,J))-Q1IJ                        &
            -Q1IJ+Q1(IFQF(I,K,J),K,JFQF(I,K,J)))                       &
            *HAFQ
!
      D2PQW=(W1(IFPA(I,K,J),K,JFPA(I,K,J))-W1IJ                        &
            -W1IJ+W1(IFPF(I,K,J),K,JFPF(I,K,J)))                       &
            *HAFP                                                      &
           +(W1(IFQA(I,K,J),K,JFQA(I,K,J))-W1IJ                        &
            -W1IJ+W1(IFQF(I,K,J),K,JFQF(I,K,J)))                       &
            *HAFQ
!
      D2PQE=(E2(IFPA(I,K,J),K,JFPA(I,K,J))-E2IJ                        &
            -E2IJ+E2(IFPF(I,K,J),K,JFPF(I,K,J)))                       &
            *HAFP                                                      &
           +(E2(IFQA(I,K,J),K,JFQA(I,K,J))-E2IJ                        &
            -E2IJ+E2(IFQF(I,K,J),K,JFQF(I,K,J)))                       &
            *HAFQ
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
      do ii = 1,6
        CALL WRF_PATCH_TO_GLOBAL_REAL( XSUMS_L(IMS,KMS,JMS,II),        &
                                       XSUMS_G(1,1,1,ii), domdesc,     &
                                       'xyz','xzy', &
                                       ids,ide,kds,kde,jds,jde, &
                                       ims,ime,kms,kme,jms,jme, &
                                       its,ite,kts,kte,jts,jte )
      enddo
      gsums = 0.
      if ( wrf_dm_on_monitor() ) then
        do ii = 1,6
        do j = jds,jde
        do k = kds,kde
        do i = ids,ide
          gsums(ii,k) = gsums(ii,k) + XSUMS_G(i,k,j,ii)
        enddo
        enddo
        enddo
        enddo
      endif
      call wrf_dm_bcast_bytes( gsums, 2*RWORDSIZE*6*(kde-kds+1) )

#else
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!***  GLOBAL REDUCTION
!----------------------------------------------------------------------
!
# ifdef DM_PARALLEL
      CALL wrf_get_dm_communicator ( mpi_comm_comp )
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,6*(KTE-KTS+1)                     &
                        ,MPI_DOUBLE_PRECISION,MPI_SUM                  &
                        ,MPI_COMM_COMP,IRECV)
# else
      GSUMS = XSUMS
# endif
#endif
!
!----------------------------------------------------------------------
!***  END OF GLOBAL REDUCTION
!----------------------------------------------------------------------
!
      DO K=KTS,KTE
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
        Q2(I,KTE,J)=MAX(E1(I,KTE,J)+E1(I,KTE,J)-EPSQ2,EPSQ2)           &
                   *HTM(I,KTE,J)
      ENDDO
      ENDDO
!
      DO J=MYJS,MYJE
      DO K=KTE-1,KTS+1,-1
      DO I=MYIS,MYIE
        KOFF=KTE-LMH(I,J)
        IF(K.GT.KOFF+1)THEN
          Q2(I,K,J)=MAX(E1(I,K,J)+E1(I,K,J)-Q2(I,K+1,J),EPSQ2)         &
                   *HTM(I,K,J)
        ELSE
          Q2(I,K,J)=Q2(I,K+1,J)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      END SUBROUTINE HAD2
!----------------------------------------------------------------------
!**********************************************************************
      SUBROUTINE VAD2_DRY(NTSD,DT,IDTAD,DX,DY                          &
                         ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP           &
                         ,HBM2,LMH                                     &
                         ,Q2,PETDT                                     &
                         ,N_IUP_H,N_IUP_V                              &
                         ,N_IUP_ADH,N_IUP_ADV                          &
                         ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                  &
                         ,IHE,IHW,IVE,IVW,INDX3_WRK                    &
                         ,IDS,IDE,JDS,JDE,KDS,KDE                      &
                         ,IMS,IME,JMS,JME,KMS,KME                      &
                         ,ITS,ITE,JTS,JTE,KTS,KTE)
!**********************************************************************
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
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V         &
                                              ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V     &
                                                      ,IUP_ADH,IUP_ADV
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
      REAL :: ADDT,AFRP,D2PQE,DEP,DETAP,DPDN,DPUP,DQP                  &
             ,DWP,E00,E4P,EP,EP0,HADDT,HBM2IJ                          &
             ,RFACEK,RFC,RR,SUMNE,SUMPE
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,E3,E4,PETDTK,RFACE
!
!**********************************************************************
!
!----------------------------------------------------------------------
      MYJS2   =MAX(JDS+2,JTS  )     !jw
      MYJE2   =MIN(JDE-2,JTE  )
      MYIS1_P1=MAX(IDS+1,ITS-1)
      MYIE1_P1=MIN(IDE-1,ITE+1)
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
            RR=ABS(RR/((AETA1(LLAP)-AETA1(K))*PDTOP                    &
                      +(AETA2(LLAP)-AETA2(K))*PDSL(I,J)))
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
            RFC=(DETA1(KTE-1)*PDTOP+DETA2(KTE-1)*PDSL(I,J))            &
               /(DETA1(KTE  )*PDTOP+DETA2(KTE  )*PDSL(I,J))
            DEL(KTE)=-DEL(KTE+1)*RFC
          ENDIF
        ENDIF
!
        IF(BOT)THEN
          IF(LA(KOFF+2).LT.0)THEN
            RFC=(DETA1(KOFF+2)*PDTOP+DETA2(KOFF+2)*PDSL(I,J))          &
               /(DETA1(KOFF+1)*PDTOP+DETA2(KOFF+1)*PDSL(I,J))
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
          DPDN=(AETA1(K+LAP)-AETA1(K))*PDTOP                           &
              +(AETA2(K+LAP)-AETA2(K))*PDSL(I,J)
          DPUP=(AETA1(K)-AETA1(K-LAP))*PDTOP                           &
              +(AETA2(K)-AETA2(K-LAP))*PDSL(I,J)
!
          AFRP=2.*AFR(K)*DPDN*DPDN/(DPDN+DPUP)
          D2PQE=((E4(K+LAP)-E4P)/DPDN                                  &
                -(E4P-E4(K-LAP))/DPUP)*AFRP
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
        Q2(I,KTE,J)=MAX(E3(KTE)+E3(KTE)-EPSQ2,EPSQ2)*HBM2IJ            &
                   +Q2(I,KTE,J)*(1.-HBM2IJ)
        DO K=KTE-1,KOFF+2
          Q2(I,K,J)=MAX(E3(K)+E3(K)-Q2(I,K+1,J),EPSQ2)*HBM2IJ          &
                   +Q2(I,K,J)*(1.-HBM2IJ)
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
!**********************************************************************
      SUBROUTINE HAD2_DRY(NTSD,DT,IDTAD,DX,DY                          &
                         ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP           &
                         ,HTM,HBM2,HBM3,LMH                            &
                         ,Q2,U,V,Z,HYDRO                               &
                         ,N_IUP_H,N_IUP_V                              &
                         ,N_IUP_ADH,N_IUP_ADV                          &
                         ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                  &
                         ,IHE,IHW,IVE,IVW,INDX3_WRK                    &
                         ,IDS,IDE,JDS,JDE,KDS,KDE                      &
                         ,IMS,IME,JMS,JME,KMS,KME                      &
                         ,ITS,ITE,JTS,JTE,KTS,KTE)
!**********************************************************************
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
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
                           ,IMS,IME,JMS,JME,KMS,KME                    &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V         &
                                              ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V     &
                                                      ,IUP_ADH,IUP_ADV
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
      INTEGER,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: IFPA,IFPF          &
                                                   ,IFQA,IFQF          &
                                                   ,JFPA,JFPF          &
                                                   ,JFQA,JFQF
!
      REAL :: ADDT,AFRP,CRIT,D2PQE,DEP,DESTIJ,DVOLP,DZA,DZB            &
             ,E00,E0Q,E2IJ,E4P,ENH,EP,EP0,ESTIJ,FPQ                    &
             ,HAFP,HAFQ,HBM2IJ,HM,HTMIKJ,PP,PPQ00                      &
             ,QP,RDY,RFACEK,RFC,RFEIJ,RR                               &
             ,SLOPAC,SPP,SQP,SSA,SSB,SUMNE,SUMPE,TTA,TTB
!
      REAL,DIMENSION(2,KTE-KTS+1) :: GSUMS,XSUMS
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,E3,E4,RFACE
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: DARE,EMH
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: AFP,AFQ,DEST,DVOL     &
                                                ,E1,E2  
!
!**********************************************************************
!
!----------------------------------------------------------------------
      MYIS    =MAX(IDS  ,ITS  )     !jw
      MYIE    =MIN(IDE  ,ITE  )
      MYJS    =MAX(JDS  ,JTS  )
      MYJE    =MIN(JDE  ,JTE  )
!
      MYIS1   =MAX(IDS+1,ITS  )
      MYIE1   =MIN(IDE-1,ITE  )
      MYJS2   =MAX(JDS+2,JTS  )
      MYJE2   =MIN(JDE-2,JTE  )
!
      MYIS_P2 =MAX(IDS  ,ITS-2)
      MYIE_P2 =MIN(IDE  ,ITE+2)
      MYJS_P3 =MAX(JDS  ,JTS-3)
      MYJE_P3 =MIN(JDE  ,JTE+3)
!
      MYIS1_P1=MAX(IDS+1,ITS-1)
      MYIE1_P1=MIN(IDE-1,ITE+1)
      MYJS2_P1=MAX(JDS+2,JTS-1)
      MYJE2_P1=MIN(JDE-2,JTE+1)
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
        TTA=(U(I,K,J-1)+U(I+IHW(J),K,J)+U(I+IHE(J),K,J)+U(I,K,J+1))    &
            *EMH(I,J)*HBM2(I,J)
        TTB=(V(I,K,J-1)+V(I+IHW(J),K,J)+V(I+IHE(J),K,J)+V(I,K,J+1))    &
            *ENH*HBM2(I,J)
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
        FPQ=SPP*SQP*HTM(I,K,J-2)*HTM(I-1,K,J)                &
                   *HTM(I+1,K,J)*HTM(I,K,J+2)*0.25
        PP=ABS(SPP)
        QP=ABS(SQP)
!
        AFP(I,K,J)=(((FF4*PP+FF3)*PP+FF2)*PP+FF1)*PP
        AFQ(I,K,J)=(((FF4*QP+FF3)*QP+FF2)*QP+FF1)*QP
!
        E2(I,K,J)=(E1 (IFPA(I,K,J),K,JFPA(I,K,J))-E1 (I,K,J))*PP       &
                 +(E1 (IFQA(I,K,J),K,JFQA(I,K,J))-E1 (I,K,J))*QP       &
                 +(E1 (I,K,J-2)+E1 (I,K,J+2)                           &
                  -E1 (I-1,K,J)-E1 (I+1,K,J))*FPQ                      &
                 +E1(I,K,J)
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
      D2PQE=(E2(IFPA(I,K,J),K,JFPA(I,K,J))-E2IJ                        &
            -E2IJ+E2(IFPF(I,K,J),K,JFPF(I,K,J)))                       &
            *HAFP                                                      &
           +(E2(IFQA(I,K,J),K,JFQA(I,K,J))-E2IJ                        &
            -E2IJ+E2(IFQF(I,K,J),K,JFQF(I,K,J)))                       &
            *HAFQ
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
      CALL wrf_get_dm_communicator ( mpi_comm_comp )
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,2*(KTE-KTS+1),MPI_REAL,MPI_SUM    &
                        ,MPI_COMM_COMP,IRECV)
#else
      GSUMS = XSUMS
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
        Q2(I,KTE,J)=MAX(E1(I,KTE,J)+E1(I,KTE,J)-EPSQ2,EPSQ2)           &
                   *HTM(I,KTE,J)
      ENDDO
      ENDDO
!
      DO J=MYJS,MYJE
      DO K=KTE-1,KTS+1,-1
      DO I=MYIS,MYIE
        KOFF=KTE-LMH(I,J)
        IF(K.GT.KOFF+1)THEN
          Q2(I,K,J)=MAX(E1(I,K,J)+E1(I,K,J)-Q2(I,K+1,J),EPSQ2)         &
                   *HTM(I,K,J)
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
