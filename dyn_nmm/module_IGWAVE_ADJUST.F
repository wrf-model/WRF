!-----------------------------------------------------------------------
!
!NCEP_MESO:MODEL_LAYER: INERTIAL GRAVITY WAVE ADJUSTMENT
!
!-----------------------------------------------------------------------
#include "nmm_loop_basemacros.h"
#include "nmm_loop_macros.h"
#define  DATA_CALLS_INCLUDED
!-----------------------------------------------------------------------
!
      MODULE MODULE_IGWAVE_ADJUST
!
!-----------------------------------------------------------------------
      USE MODULE_MODEL_CONSTANTS
!     USE MODULE_TIMERS  ! this one creates a name conflict at compile time
!-----------------------------------------------------------------------
!***
!***  SPECIFY THE NUMBER OF TIMES TO SMOOTH THE VERTICAL VELOCITY
!***  AND THE NUMBER OF ROWS FROM THE NORTHERN AND SOUTHERN EDGES
!***  OF THE GLOBAL DOMAIN BEYOND WHICH THE SMOOTHING DOES NOT GO
!***  FOR SUBROUTINE PDTE
!
      INTEGER :: KSMUD=0,LNSDT=7
!
!-----------------------------------------------------------------------
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE PFDHT(NTSD,LAST_TIME,PT,DETA1,DETA2,PDTOP,RES,FIS      &
     &                ,HYDRO,SIGMA,FIRST,DX,DY                          &
     &                ,HTM,HBM2,VTM,VBM2,VBM3                           &
     &                ,FDIV,FCP,WPDAR,DFL,CPGFU,CPGFV                   &
     &                ,PD,PDSL,T,Q,U,V,CWM,OMGALF,PINT,DWDT             &
     &                ,RTOP,DIV,FEW,FNS,FNE,FSE                         &
     &                ,IHE,IHW,IVE,IVW,INDX3_WRK                        &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    PFDHT       DIVERGENCE/HORIZONTAL OMEGA-ALPHA
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-10-28
!
! ABSTRACT:
!     PFDHT CALCULATES THE PRESSURE GRADIENT FORCE, UPDATES THE
!     VELOCITY COMPONENTS DUE TO THE EFFECT OF THE PRESSURE GRADIENT
!     AND CORIOILS FORCES, COMPUTES THE DIVERGENCE INCLUDING THE
!     MODIFICATION PREVENTING GRAVITY WAVE GRID SEPARATION, AND
!     CALCULATES THE HORIZONTAL PART OF THE OMEGA-ALPHA TERM.
!     (THE PART PROPORTIONAL TO THE ADVECTION OF MASS ALONG
!      COORDINATE SURFACES).
!
! PROGRAM HISTORY LOG:
!   87-06-??  JANJIC     - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   96-03-29  BLACK      - ADDED EXTERNAL EDGE
!   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
!   02-02-01  BLACK      - REWRITTEN FOR WRF CODING STANDARDS
!   04-02-17  JANJIC     - REMOVED UPDATE OF TEMPERATURE
!   04-11-23  BLACK      - THREADED
!
! USAGE: CALL PFDHT FROM MAIN PROGRAM SOLVE_RUNSTREAM
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
!***********************************************************************
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
!#ifdef DM_PARALLEL
!      INCLUDE "mpif.h"
!#endif
!-----------------------------------------------------------------------
      LOGICAL,INTENT(IN) :: FIRST,HYDRO
      INTEGER,INTENT(IN) :: SIGMA
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
!***  NMM_MAX_DIM is set in configure.wrf and must agree with
!***  the value of dimspec q in the Registry/Registry
!
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: NTSD
      LOGICAL,INTENT(IN) :: LAST_TIME
!
      REAL,INTENT(IN) :: CPGFV,DY,PDTOP,PT
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: DETA1,DETA2
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: DFL
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CPGFU,DX,FCP,FDIV   &
     &                                             ,PD,FIS,RES,WPDAR    &
     &                                             ,HBM2,VBM2,VBM3
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: CWM,DWDT    &
     &                                                     ,Q,T,HTM,VTM
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PINT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: DIV      &
     &                                                        ,OMGALF   &
     &                                                        ,RTOP,U,V
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: FEW,FNS    &
     &                                                      ,FNE,FSE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: PDSL
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,J,JJ,JKNT,JSTART,K
      INTEGER :: J1_00,J1_M1,J1_P1,J1_P2
      INTEGER :: J2_00,J2_M1,J2_P1
      INTEGER :: J3_00,J3_P1,J3_P2
      INTEGER :: J4_00,J4_M1,J4_P1
      INTEGER :: J5_00,J5_M1
      INTEGER :: J6_00,J6_P1
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: ALP1,FILO
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE+1,JTS-5:JTE+5) :: PINTLG
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,JTS-5:JTE+5) :: FIM
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE) :: DIVL,TEW
!
      REAL :: ADPDNE,ADPDSE,ADPDX,ADPDY,APELP,DFI,DCNEK,DCSEK           &
     &       ,DPFEW,DPFNS,DPFNEK,DPFSEK,DPNEK,DPSEK,EDIV,FIUP           &
     &       ,HM,PCEW,PCNS,PEW,PNS,PRSFRC,PVNEK,PVSEK,RTOPP,VM
!
      REAL :: SLP_STD=101300.0
!
!***  TYPE 1 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-2:2) :: APEL,DFDZ,DPDE
!
!***  TYPE 2 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-2:1) :: CNE,PCNE,PNE,PPNE
!
!***  TYPE 3 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-1:2) :: CSE,PCSE,PPSE,PSE
!
!***  TYPE 4 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-1:1) :: PCXC,TNS,UDY,VDX
!
!***  TYPE 5 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-1:0) :: TNE
!
!***  TYPE 6 WORKING ARRAY
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE, 0:1) :: TSE
!-----------------------------------------------------------------------
!***********************************************************************
!
!                                       
!                CSE                          CSE            -------  1
!                 *                            *  
!                 *                            *    
!       *******   *                  *******   *   
!      *       *  *                 *       *  *  
!   CNE         * *              CNE         * *       
!               TEW----------OMGALF----------TEW             -------  0
!   CSE         * *              CSE         * *         
!      *       *  *                 *       *  *       
!       *******   *                  *******   *     
!                 *                            *   
!                 *                            * 
!                CNE                          CNE            ------- -1
!                                        
!
!
! 
!***********************************************************************
! 
!                              CSE                           -------  2
!                               *
!                               *
!                               *
!                               *
!                      CNE*****TNS                           -------  1
!                      CSE     | *
!                              | *
!                              | *
!                              | *
!                              | CNE
!                            OMGALF                          -------  0
!                              | CSE
!                              | *
!                              | *
!                              | *
!                      CNE     | *
!                      CSE*****TNS                           ------- -1
!                               *
!                               *
!                               *
!                               *
!                              CNE                           ------- -2
! 
!***********************************************************************
!-----------------------------------------------------------------------
!***  PREPARATORY CALCULATIONS
!-----------------------------------------------------------------------
!     call hpm_start('PFDHT')
!
      DO J=JMS,JME
      DO I=IMS,IME
        PDSL(I,J)=0.
      ENDDO
      ENDDO
!
      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        OMGALF(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
!***  ZERO OUT TEMPORARIES.
!
      DO J=JTS-5,JTE+5
      DO I=ITS-5,ITE+5
        ALP1(I,J)=0.
        FILO(I,J)=0.
      ENDDO
      ENDDO
!
      DO J=JTS-5,JTE+5
      DO K=KTS,KTE+1
      DO I=ITS-5,ITE+5
        PINTLG(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=JTS-5,JTE+5
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        FIM(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        DIVL(I,K)=0.
        TEW(I,K)=0.
      ENDDO
      ENDDO
!
      DO J=-2,2
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        APEL(I,K,J)=0.
        DFDZ(I,K,J)=0.
        DPDE(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=-2,1
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        CNE(I,K,J)=0.
        PCNE(I,K,J)=0.
        PNE(I,K,J)=0.
        PPNE(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=-1,2
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        CSE(I,K,J)=0.
        PCSE(I,K,J)=0.
        PSE(I,K,J)=0.
        PPSE(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=-1,1
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        PCXC(I,K,J)=0.
        TNS(I,K,J)=0.
        UDY(I,K,J)=0.
        VDX(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=-1,0
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        TNE(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      DO J=0,1
      DO K=KTS,KTE
      DO I=ITS-5,ITE+5
        TSE(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!
      IF(SIGMA.EQ.1)THEN
        DO J=MYJS_P4,MYJE_P4
        DO I=MYIS_P4,MYIE_P4
          FILO(I,J)=FIS(I,J)
          PDSL(I,J)=PD(I,J)
        ENDDO
        ENDDO
      ELSE
        DO J=MYJS_P4,MYJE_P4
        DO I=MYIS_P4,MYIE_P4
          FILO(I,J)=0.0
          PDSL(I,J)=RES(I,J)*PD(I,J)
        ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!***
!***  INTEGRATE THE GEOPOTENTIAL
!***
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(apelp,dfi,fiup,i,j,k,rtopp)
      DO J=MYJS_P4,MYJE_P4
!
        DO K=KTS,KTE
        DO I=MYIS_P4,MYIE_P4
!
          APELP=(PINT(I,K+1,J)+PINT(I,K,J))*0.5
          RTOPP=(Q(I,K,J)*P608-CWM(I,K,J)+1.)*T(I,K,J)*R_D/APELP

          DFI=RTOPP*(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))
!
          RTOP(I,K,J)=RTOPP
          FIUP=FILO(I,J)+DFI
          FIM(I,K,J)=FILO(I,J)+FIUP
          FILO(I,J)=(FIUP-DFL(K+1))*HTM(I,K,J)+DFL(K+1)
        ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  MARCH NORTHWARD THROUGH THE SOUTHERNMOST SLABS TO BEGIN
!***  FILLING THE MAIN WORKING ARRAYS WHICH ARE MULTI-DIMENSIONED
!***  IN J BECAUSE THEY ARE DIFFERENCED OR AVERAGED IN J
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      JSTART=MYJS2_P2
!
      DO J=-2,1
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(apelp,i,k)
        DO K=KTS,KTE
        DO I=MYIS_P4,MYIE_P4
          APELP=0.5*(PINT(I,K+1,JJ)+PINT(I,K,JJ))
          APEL(I,K,J)=APELP
          DFDZ(I,K,J)=RTOP(I,K,JJ)
          DPDE(I,K,J)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,JJ)
        ENDDO
        ENDDO
!
      ENDDO
!
      DO J=-2,0
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE
        DO I=MYIS_P3,MYIE_P3
          CNE(I,K,J)=(DFDZ(I+IHE(JJ),K,J+1)+DFDZ(I,K,J))*2.             &
     &              *(APEL(I+IHE(JJ),K,J+1)-APEL(I,K,J))
          PNE(I,K,J)=(FIM(I+IHE(JJ),K,JJ+1)-FIM(I,K,JJ))                &
     &              *(DWDT(I+IHE(JJ),K,JJ+1)+DWDT(I,K,JJ))
          PCNE(I,K,J)=CNE(I,K,J)*(DPDE(I+IHE(JJ),K,J+1)+DPDE(I,K,J))
          PPNE(I,K,J)=PNE(I,K,J)*(DPDE(I+IHE(JJ),K,J+1)+DPDE(I,K,J))
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE
        DO I=MYIS_P3,MYIE_P3
          CSE(I,K,J+1)=(DFDZ(I+IHE(JJ+1),K,J)+DFDZ(I,K,J+1))*2.         &
     &                *(APEL(I+IHE(JJ+1),K,J)-APEL(I,K,J+1))
          PSE(I,K,J+1)=(FIM(I+IHE(JJ+1),K,JJ)-FIM(I,K,JJ+1))            &                
     &                *(DWDT(I+IHE(JJ+1),K,JJ)+DWDT(I,K,JJ+1))
          PCSE(I,K,J+1)=CSE(I,K,J+1)                                    &
     &                 *(DPDE(I+IHE(JJ+1),K,J)+DPDE(I,K,J+1))
          PPSE(I,K,J+1)=PSE(I,K,J+1)                                    &
     &                 *(DPDE(I+IHE(JJ+1),K,J)+DPDE(I,K,J+1))
        ENDDO
        ENDDO
      ENDDO
!
      IF(.NOT.FIRST)THEN   ! Skip at timestep 0
        J=0
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(adpdx,adpdy,dcnek,dcsek,dpfew,dpfnek,dpfns,dpfsek,       &
!$omp&         dpnek,dpsek,i,k,pcew,pcns,pew,pns,vm)
        DO K=KTS,KTE
        DO I=MYIS_P2,MYIE1_P2
          DPFNEK=((PPNE(I+IVW(JJ),K,J)+PPNE(I,K,J-1))                   &
     &           +(PCNE(I+IVW(JJ),K,J)+PCNE(I,K,J-1)))*2.
          DPFSEK=((PPSE(I+IVW(JJ),K,J)+PPSE(I,K,J+1))                   &
     &           +(PCSE(I+IVW(JJ),K,J)+PCSE(I,K,J+1)))*2.
          DPFEW=DPFNEK+DPFSEK
          DPFNS=DPFNEK-DPFSEK
          ADPDX=DPDE(I+IVW(JJ),K,J)+DPDE(I+IVE(JJ),K,J)
          ADPDY=DPDE(I,K,J-1)+DPDE(I,K,J+1)
          DPNEK=PNE(I+IVW(JJ),K,J)+PNE(I,K,J-1)
          DPSEK=PSE(I+IVW(JJ),K,J)+PSE(I,K,J+1)
          PEW=DPNEK+DPSEK
          PNS=DPNEK-DPSEK
          DCNEK=CNE(I+IVW(JJ),K,J)+CNE(I,K,J-1)
          DCSEK=CSE(I+IVW(JJ),K,J)+CSE(I,K,J+1)
          PCEW=(DCNEK+DCSEK)*ADPDX
          PCNS=(DCNEK-DCSEK)*ADPDY
          VM=VTM(I,K,JJ)*VBM2(I,JJ)
          U(I,K,JJ)=(((DPFEW+PCEW)/ADPDX+PEW)*CPGFU(I,JJ))*VM+U(I,K,JJ)
          V(I,K,JJ)=(((DPFNS+PCNS)/ADPDY+PNS)*CPGFV      )*VM+V(I,K,JJ)
        ENDDO
        ENDDO
      ENDIF
!
      DO J=-1,0
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(adpdy,dcnek,dcsek,i,k)
        DO K=KTS,KTE
        DO I=MYIS_P3,MYIE_P3
          UDY(I,K,J)=DY*U(I,K,JJ)
          VDX(I,K,J)=DX(I,JJ)*V(I,K,JJ)
          DCNEK=CNE(I+IVW(JJ),K,J)+CNE(I,K,J-1)
          DCSEK=CSE(I+IVW(JJ),K,J)+CSE(I,K,J+1)
          ADPDY=DPDE(I,K,J-1)+DPDE(I,K,J+1)
          TNS(I,K,J)=VDX(I,K,J)*((DCNEK-DCSEK)*ADPDY)
          FNS(I,K,JJ)=VDX(I,K,J)*ADPDY
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE
        DO I=MYIS_P1,MYIE_P1
          PCXC(I,K,J)=(PNE(I+IVW(JJ),K,J)-PNE(I,K,J-1)                  &
     &                +CNE(I+IVW(JJ),K,J)-CNE(I,K,J-1)                  &
     &                +PSE(I+IVW(JJ),K,J)-PSE(I,K,J+1)                  &
     &                +CSE(I+IVW(JJ),K,J)-CSE(I,K,J+1))                 &
     &                *VBM3(I,JJ)*VTM(I,K,JJ)
        ENDDO
        ENDDO
!
      ENDDO
!
      JJ=JSTART
!$omp parallel do                                                       &
!$omp& private(adpdne,i,k,pvnek)
      DO K=KTS,KTE
      DO I=MYIS_P2,MYIE1_P2
        ADPDNE=DPDE(I+IHE(JJ-1),K,0)+DPDE(I,K,-1)
        PVNEK=(UDY(I+IHE(JJ-1),K,-1)+VDX(I+IHE(JJ-1),K,-1))             &
     &       +(UDY(I,K,0)          +VDX(I,K,0))
        PCNE(I,K,-1)=CNE(I,K,-1)*ADPDNE
        PPNE(I,K,-1)=PNE(I,K,-1)*ADPDNE
        TNE(I,K,-1)=PVNEK*PCNE(I,K,-1)*2.
        FNE(I,K,JJ-1)=PVNEK*ADPDNE
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(adpdse,i,k,pvsek)
      DO K=KTS,KTE
      DO I=MYIS_P2,MYIE1_P2
        ADPDSE=DPDE(I+IHE(JJ),K,-1)+DPDE(I,K,0)
        PVSEK=(UDY(I+IHE(JJ),K,0)-VDX(I+IHE(JJ),K,0))                   &
     &       +(UDY(I,K,-1)      -VDX(I,K,-1))
        PCSE(I,K,0)=CSE(I,K,0)*ADPDSE
        PPSE(I,K,0)=PSE(I,K,0)*ADPDSE
        TSE(I,K,0)=PVSEK*PCSE(I,K,0)*2.
        FSE(I,K,JJ)=PVSEK*ADPDSE
      ENDDO
      ENDDO
!
      JKNT=0
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  MAIN INTEGRATION LOOP
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      main_integration : DO J=MYJS2_P2,MYJE2_P2
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
!-----------------------------------------------------------------------
      PRSFRC=PDTOP/(SLP_STD-PT)
!$omp parallel do                                                       &
!$omp& private(apelp,i,k)
      DO K=KTS,KTE
!
      DO I=MYIS_P4,MYIE_P4
        APELP=0.5*(PINT(I,K+1,J+2)+PINT(I,K,J+2))
        APEL(I,K,J1_P2)=APELP
        DFDZ(I,K,J1_P2)=RTOP(I,K,J+2)
        DPDE(I,K,J1_P2)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J+2)
      ENDDO
!
!-----------------------------------------------------------------------
!***  DIAGONAL CONTRIBUTIONS TO PRESSURE GRADIENT FORCE
!-----------------------------------------------------------------------
!
!     call hpm_start('block1')
      DO I=MYIS_P3,MYIE_P3
        CNE(I,K,J2_P1)=(DFDZ(I+IHE(J+1),K,J1_P2)+DFDZ(I,K,J1_P1))*2.    &
     &                *(APEL(I+IHE(J+1),K,J1_P2)-APEL(I,K,J1_P1))
        PNE(I,K,J2_P1)=(FIM(I+IHE(J+1),K,J+2)-FIM(I,K,J+1))             &
     &                *(DWDT(I+IHE(J+1),K,J+2)+DWDT(I,K,J+1))
        PCNE(I,K,J2_P1)=CNE(I,K,J2_P1)                                  &
     &                 *(DPDE(I+IHE(J+1),K,J1_P2)+DPDE(I,K,J1_P1))
        PPNE(I,K,J2_P1)=PNE(I,K,J2_P1)                                  &
     &                 *(DPDE(I+IHE(J+1),K,J1_P2)+DPDE(I,K,J1_P1))
      ENDDO
!
      DO I=MYIS_P3,MYIE_P3
        CSE(I,K,J3_P2)=(DFDZ(I+IHE(J+2),K,J1_P1)+DFDZ(I,K,J1_P2))*2.    &
     &                *(APEL(I+IHE(J+2),K,J1_P1)-APEL(I,K,J1_P2))
        PSE(I,K,J3_P2)=(FIM(I+IHE(J+2),K,J+1)-FIM(I,K,J+2))             &
     &                *(DWDT(I+IHE(J+2),K,J+1)+DWDT(I,K,J+2))
        PCSE(I,K,J3_P2)=CSE(I,K,J3_P2)                                  &
     &                 *(DPDE(I+IHE(J+2),K,J1_P1)+DPDE(I,K,J1_P2))
        PPSE(I,K,J3_P2)=PSE(I,K,J3_P2)                                  &
     &                 *(DPDE(I+IHE(J+2),K,J1_P1)+DPDE(I,K,J1_P2))
      ENDDO
!
!-----------------------------------------------------------------------
!***  CONTINUITY EQUATION MODIFICATION
!-----------------------------------------------------------------------
!
      DO I=MYIS_P1,MYIE_P1
        PCXC(I,K,J4_P1)=(PNE(I+IVW(J+1),K,J2_P1)                        &
     &                  +CNE(I+IVW(J+1),K,J2_P1)                        &
     &                  +PSE(I+IVW(J+1),K,J3_P1)                        &
     &                  +CSE(I+IVW(J+1),K,J3_P1)                        &
     &                  -PNE(I,K,J2_00)                                 &
     &                  -CNE(I,K,J2_00)                                 &
     &                  -PSE(I,K,J3_P2)                                 &
     &                  -CSE(I,K,J3_P2))                                &
     &                  *VBM3(I,J+1)*VTM(I,K,J+1)
      ENDDO
!
!-----------------------------------------------------------------------
!
      DO I=MYIS1,MYIE1
        DIVL(I,K)=(DETA1(K)*PRSFRC                                      &
     &            +DETA2(K)*(1.-PRSFRC))*WPDAR(I,J)                     &
     &           *(PCXC(I+IHE(J),K,J4_00)-PCXC(I,K,J4_P1)               &
                  +PCXC(I+IHW(J),K,J4_00)-PCXC(I,K,J4_M1))
      ENDDO
      ENDDO
!     call hpm_stop('block1')
!
!-----------------------------------------------------------------------
!
      IF(.NOT.FIRST)THEN     ! Skip at timestep 0
!
!-----------------------------------------------------------------------
!***  LAT & LONG PRESSURE FORCE COMPONENTS
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(adpdx,adpdy,dcnek,dcsek,dpfew,dpfnek,dpfns,dpfsek,       &
!$omp&         dpnek,dpsek,i,k,pcew,pcns,pew,pns,vm)
        DO K=KTS,KTE
        DO I=MYIS_P2,MYIE1_P2
          DPNEK=PNE(I+IVW(J+1),K,J2_P1)+PNE(I,K,J2_00)
          DPSEK=PSE(I+IVW(J+1),K,J3_P1)+PSE(I,K,J3_P2)
          PEW=DPNEK+DPSEK
          PNS=DPNEK-DPSEK
!
          ADPDX=DPDE(I+IVW(J+1),K,J1_P1)+DPDE(I+IVE(J+1),K,J1_P1)
          ADPDY=DPDE(I,K,J1_00)+DPDE(I,K,J1_P2)
          DCNEK=CNE(I+IVW(J+1),K,J2_P1)+CNE(I,K,J2_00)
          DCSEK=CSE(I+IVW(J+1),K,J3_P1)+CSE(I,K,J3_P2)
          PCEW=(DCNEK+DCSEK)*ADPDX
          PCNS=(DCNEK-DCSEK)*ADPDY
!
          DPFNEK=((PPNE(I+IVW(J+1),K,J2_P1)+PPNE(I,K,J2_00))            &
     &           +(PCNE(I+IVW(J+1),K,J2_P1)+PCNE(I,K,J2_00)))*2.
          DPFSEK=((PPSE(I+IVW(J+1),K,J3_P1)+PPSE(I,K,J3_P2))            &
     &           +(PCSE(I+IVW(J+1),K,J3_P1)+PCSE(I,K,J3_P2)))*2.
          DPFEW=DPFNEK+DPFSEK
          DPFNS=DPFNEK-DPFSEK
!
!-----------------------------------------------------------------------
!***  UPDATE U AND V FOR PRESSURE GRADIENT FORCE
!-----------------------------------------------------------------------
!
          VM=VTM(I,K,J+1)*VBM2(I,J+1)
          U(I,K,J+1)=(((DPFEW+PCEW)/ADPDX+PEW)*CPGFU(I,J+1))*VM         &
     &              +U(I,K,J+1) 
          V(I,K,J+1)=(((DPFNS+PCNS)/ADPDY+PNS)*CPGFV       )*VM         &
     &              +V(I,K,J+1)
        ENDDO
        ENDDO
!-----------------------------------------------------------------------
!
      ENDIF    !End of IF block executed for FIRST equal to .FALSE.
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      IF(.NOT.LAST_TIME)THEN    !Do not execute block at last timestep
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(adpdx,adpdy,dcnek,dcsek,ediv,hm,i,k,pvnek,pvsek)
        DO K=KTS,KTE
        DO I=MYIS_P2,MYIE_P3
          UDY(I,K,J4_P1)=DY*U(I,K,J+1)
          VDX(I,K,J4_P1)=DX(I,J+1)*V(I,K,J+1)
        ENDDO
!
!-----------------------------------------------------------------------
!***  LAT & LON FLUXES & OMEGA-ALPHA COMPONENTS
!-----------------------------------------------------------------------
!
        DO I=MYIS_P2,MYIE_P3
          ADPDX=DPDE(I+IVW(J),K,J1_00)+DPDE(I+IVE(J),K,J1_00)
          DCNEK=CNE(I+IVW(J),K,J2_00)+CNE(I,K,J2_M1)
          DCSEK=CSE(I+IVW(J),K,J3_00)+CSE(I,K,J3_P1)
          TEW(I,K)=UDY(I,K,J4_00)*((DCNEK+DCSEK)*ADPDX)
          FEW(I,K,J)=UDY(I,K,J4_00)*ADPDX
!
          ADPDY=DPDE(I,K,J1_P2)+DPDE(I,K,J1_00)
          DCNEK=CNE(I+IVW(J+1),K,J2_P1)+CNE(I,K,J2_00)
          DCSEK=CSE(I+IVW(J+1),K,J3_P1)+CSE(I,K,J3_P2)
          TNS(I,K,J4_P1)=VDX(I,K,J4_P1)*((DCNEK-DCSEK)*ADPDY)
          FNS(I,K,J+1)=VDX(I,K,J4_P1)*ADPDY
        ENDDO
!
!-----------------------------------------------------------------------
!***  DIAGONAL FLUXES AND DIAGONALLY AVERAGED WIND
!-----------------------------------------------------------------------
!
        DO I=MYIS_P1,MYIE1_P1
          PVNEK=(UDY(I+IHE(J),K,J4_00)+VDX(I+IHE(J),K,J4_00))           &
     &         +(UDY(I,K,J4_P1)       +VDX(I,K,J4_P1))
          TNE(I,K,J5_00)=PVNEK*PCNE(I,K,J2_00)*2.
          FNE(I,K,J)=PVNEK*(DPDE(I+IHE(J),K,J1_P1)+DPDE(I,K,J1_00))
        ENDDO
!
        DO I=MYIS_P1,MYIE1_P1
          PVSEK=(UDY(I+IHE(J+1),K,J4_P1)-VDX(I+IHE(J+1),K,J4_P1))       &
     &         +(UDY(I,K,J4_00)         -VDX(I,K,J4_00))
          TSE(I,K,J6_P1)=PVSEK*PCSE(I,K,J3_P1)*2.
          FSE(I,K,J+1)=PVSEK*(DPDE(I+IHE(J+1),K,J1_00)+DPDE(I,K,J1_P1))
        ENDDO
!
!-----------------------------------------------------------------------
!***  HORIZONTAL PART OF OMEGA-ALPHA & DIVERGENCE
!-----------------------------------------------------------------------
!
        DO I=MYIS1,MYIE1
          HM=HTM(I,K,J)*HBM2(I,J)
          OMGALF(I,K,J)=(TEW(I+IHE(J),K)+TEW(I+IHW(J),K)                &
     &                  +TNS(I,K,J4_P1) +TNS(I,K,J4_M1)                 &
     &                  +TNE(I,K,J5_00) +TNE(I+IHW(J),K,J5_M1)          &
     &                  +TSE(I,K,J6_00)+TSE(I+IHW(J),K,J6_P1))          &
     &                  /DPDE(I,K,J1_00)*FCP(I,J)*HM
          EDIV=(FEW(I+IHE(J),K,J)+FNS(I,K,J+1)                          &
     &         +FNE(I,K,J)+FSE(I,K,J)                                   &
     &        -(FEW(I+IHW(J),K,J)+FNS(I,K,J-1)                          &
     &         +FNE(I+IHW(J),K,J-1)+FSE(I+IHW(J),K,J+1)))*FDIV(I,J)
          DIV(I,K,J)=(EDIV+DIVL(I,K))*HM
        ENDDO
        ENDDO
!-----------------------------------------------------------------------
!
      ENDIF   !End block to skip execution at last timestep
!
!-----------------------------------------------------------------------
!
      ENDDO main_integration
!     call hpm_stop('PFDHT')
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE PFDHT
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE PDTE(                                                  &
#ifdef DM_PARALLEL
     &                GRID,                                             &
#endif
     &                NTSD,DT,PT,ETA2,RES,HYDRO                         &
     &               ,HTM,HBM2                                          &
     &               ,PD,PDSL,PDSLO                                     &
     &               ,PETDT,DIV,PSDT                                    &
     &               ,IHE,IHW,IVE,IVW,INDX3_WRK                         &                 
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &               ,IMS,IME,JMS,JME,KMS,KME                           &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    PDTE        SURFACE PRESSURE TENDENCY CALC
!   PRGRMMR: JANJIC          ORG: W/NP2      DATE: 96-07-??      
!     
! ABSTRACT:
!     PDTE VERTICALLY INTEGRATES THE MASS FLUX DIVERGENCE TO
!     OBTAIN THE SURFACE PRESSURE TENDENCY AND VERTICAL VELOCITY ON
!     THE LAYER INTERFACES.  THEN IT UPDATES THE HYDROSTATIC SURFACE
!     PRESSURE AND THE NONHYDROSTATIC PRESSURE.
!     
! PROGRAM HISTORY LOG:
!   87-06-??  JANJIC     - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   96-05-??  JANJIC     - ADDED NONHYDROSTATIC EFFECTS & MERGED THE
!                          PREVIOUS SUBROUTINES PDTE & PDNEW
!   00-01-03  BLACK      - DISTRIBUTED MEMORY AND THREADS
!   01-02-23  BLACK      - CONVERTED TO WRF FORMAT
!   01-04-11  BLACK      - REWRITTEN FOR WRF CODING STANDARDS
!   04-02-17  JANJIC     - MOVED UPDATE OF T DUE TO OMEGA-ALPHA TERM
!                          AND UPDATE OF PINT TO NEW ROUTINE VTOA
!   04-11-23  BLACK      - THREADED
!     
! USAGE: CALL PDTE FROM SUBROUTINE SOLVE_RUNSTREAM
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
#ifdef DM_PARALLEL
      USE module_domain
      USE module_dm
#endif
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
#ifdef DM_PARALLEL
!     INCLUDE "mpif.h"
      TYPE (DOMAIN) :: GRID
#endif
!-----------------------------------------------------------------------
      LOGICAL,INTENT(IN) :: HYDRO
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
!***  NMM_MAX_DIM is set in configure.wrf and must agree with
!***  the value of dimspec q in the Registry/Registry
!
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: NTSD
!
      REAL,INTENT(IN) :: DT,PT
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: RES,HBM2   
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: HTM
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: DIV
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: PD,PDSL
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: PETDT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: PDSLO,PSDT
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,IHH,IHL,IX,J,JHH,JHL,JJ,JX,K,KNT,KS,NSMUD
      INTEGER :: J1_00,J1_M1,J2_00,J2_P1
      INTEGER :: MY_IS_GLB,MY_IE_GLB,MY_JS_GLB,MY_JE_GLB
#ifdef DM_PARALLEL
      INTEGER :: IPS,IPE,JPS,JPE,KPS,KPE
#endif
#ifdef DEREF_KLUDGE
! SEE http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
      INTEGER :: SM31,EM31,SM32,EM32,SM33,EM33
      INTEGER :: SM31X,EM31X,SM32X,EM32X,SM33X,EM33X
      INTEGER :: SM31Y,EM31Y,SM32Y,EM32Y,SM33Y,EM33Y
#endif
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: APDT,HBMS,PRET
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-1:0) :: PNE
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE, 0:1) :: PSE
!
      REAL :: PETDTL
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
#include "deref_kludge.h"
!
      DO J=JMS,JME
      DO I=IMS,IME
        PDSLO(I,J)=0.
      ENDDO
      ENDDO
!
      MY_IS_GLB=ITS
      MY_IE_GLB=ITE
      MY_JS_GLB=JTS
      MY_JE_GLB=JTE
!-----------------------------------------------------------------------
!***  COMPUTATION OF PRESSURE TENDENCY & PREPARATIONS
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=MYJS_P2,MYJE_P2
        DO K=KTE-1,KTS,-1
        DO I=MYIS_P2,MYIE_P2
          DIV(I,K,J)=DIV(I,K+1,J)+DIV(I,K,J)
        ENDDO
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        PSDT(I,J)=-DIV(I,KTS,J)
        APDT(I,J)=PSDT(I,J)
        PDSLO(I,J)=PDSL(I,J)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
      DO J=JMS,JME
      DO I=IMS,IME
        PDSL(I,J)=0.
      ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        PD(I,J)=PSDT(I,J)*DT+PD(I,J)
        PRET(I,J)=PSDT(I,J)*RES(I,J)
        PDSL(I,J)=PD(I,J)*RES(I,J)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!***  COMPUTATION OF PETDT
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO J=MYJS_P2,MYJE_P2
        DO K=KTE-1,KTS,-1
        DO I=MYIS_P2,MYIE_P2
          PETDT(I,K,J)=-(PRET(I,J)*ETA2(K+1)+DIV(I,K+1,J))              &
     &                  *HTM(I,K,J)*HBM2(I,J)
        ENDDO
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!***  SMOOTHING VERTICAL VELOCITY ALONG BOUNDARIES
!-----------------------------------------------------------------------
      nonhydrostatic_smoothing: IF(.NOT.HYDRO.AND.KSMUD.GT.0)THEN
!
        NSMUD=KSMUD
!
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          HBMS(I,J)=HBM2(I,J)
        ENDDO
        ENDDO
!
        JHL=LNSDT
        JHH=JDE-JHL+1
!
!$omp parallel do                                                       &
!$omp& private(i,ihh,ihl,ix,j,jx)
        DO J=JHL,JHH
          IF(J.GE.MY_JS_GLB.AND.J.LE.MY_JE_GLB)THEN
            IHL=JHL/2+1
            IHH=IDE-IHL+MOD(J,2)
!
            DO I=IHL,IHH
              IF(I.GE.MY_IS_GLB.AND.I.LE.MY_IE_GLB)THEN
                IX=I    ! -MY_IS_GLB+1
                JX=J    ! -MY_JS_GLB+1
                HBMS(IX,JX)=0.
              ENDIF
            ENDDO
!
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!***
!***  SMOOTH THE VERTICAL VELOCITY
!***
!-----------------------------------------------------------------------
!
        DO KS=1,NSMUD
!
!-----------------------------------------------------------------------
!
!***  FILL SOUTHERNMOST SLABS OF THE PNE AND PSE WORKING ARRAYS
!
          JJ=MYJS2-1
!$omp parallel do                                                       &
!$omp& private(i,k)
          DO K=KTS,KTE-1
!
          DO I=MYIS_P1,MYIE1_P1
            PNE(I,K,-1)=(PETDT(I+IHE(JJ),K,JJ+1)-PETDT(I,K,JJ))         &
     &                  *HTM(I,K,JJ)*HTM(I+IHE(JJ),K,JJ+1)
          ENDDO
!
          DO I=MYIS_P1,MYIE1_P1
            PSE(I,K,0)=(PETDT(I+IHE(JJ+1),K,JJ)-PETDT(I,K,JJ+1))        &
     &                 *HTM(I+IHE(JJ+1),K,JJ)*HTM(I,K,JJ+1)
          ENDDO
!
          ENDDO
!
          KNT=0
!
!-----------------------------------------------------------------------
!
!***  PROCEED NORTHWARD WITH THE SMOOTHING.
!***  PNE AT H(I,J) LIES BETWEEN (I,J) AND THE H POINT TO THE NE.
!***  PSE AT H(I,J) LIES BETWEEN (I,J) AND THE H POINT TO THE SE.
!
          DO J=MYJS2,MYJE2
!
            KNT=KNT+1
            J1_00=-MOD(KNT+1,2)
            J1_M1=-MOD(KNT,2)
            J2_P1=MOD(KNT,2)
            J2_00=MOD(KNT+1,2)
!
!$omp parallel do                                                       &
!$omp& private(i,k,petdtl)
            DO K=KTS,KTE-1
!
            DO I=MYIS_P1,MYIE1_P1
              PNE(I,K,J1_00)=(PETDT(I+IHE(J),K,J+1)-PETDT(I,K,J))       &
     &                       *HTM(I,K+1,J)*HTM(I+IHE(J),K+1,J+1)
            ENDDO
!
            DO I=MYIS_P1,MYIE1_P1
              PSE(I,K,J2_P1)=(PETDT(I+IHE(J+1),K,J)-PETDT(I,K,J+1))     &
     &                       *HTM(I+IHE(J+1),K+1,J)*HTM(I,K+1,J+1)
            ENDDO
!
            DO I=MYIS1,MYIE1
              PETDTL=(PNE(I,K,J1_00)-PNE(I+IHW(J),K,J1_M1)              &
     &               +PSE(I,K,J2_00)-PSE(I+IHW(J),K,J2_P1))*HBM2(I,J)
              PETDT(I,K,J)=PETDTL*HBMS(I,J)*0.125+PETDT(I,K,J) 
            ENDDO
!
            ENDDO
!
          ENDDO
!
#ifdef DM_PARALLEL
          IPS=ITS;IPE=ITE;JPS=JTS;JPE=JTE;KPS=KTS;KPE=KTE
# include <HALO_NMM_E.inc>
#endif
!-----------------------------------------------------------------------
!
        ENDDO  ! End of smoothing loop
!
!-----------------------------------------------------------------------
      ENDIF nonhydrostatic_smoothing
!-----------------------------------------------------------------------
      END SUBROUTINE PDTE
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE VTOA(                                                  &
#ifdef DM_PARALLEL
     &                grid,                                             &
#endif
     &                NTSD,DT,PT,ETA2                                   &
     &               ,HTM,HBM2,EF4T                                     &
     &               ,T,DWDT,RTOP,OMGALF                                &
     &               ,PINT,DIV,PSDT,RES                                 &
     &               ,IHE,IHW,IVE,IVW,INDX3_WRK                         &                 
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &               ,IMS,IME,JMS,JME,KMS,KME                           &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    VTOA        OMEGA-ALPHA
!   PRGRMMR: JANJIC          ORG: W/NP2      DATE: 04-02-17      
!     
! ABSTRACT:
!     VTOA UPDATES THE NONHYDROSTATIC PRESSURE AND ADDS THE
!     CONTRIBUTION OF THE OMEGA-ALPHA TERM OF THE THERMODYNAMIC
!     EQUATION.  ALSO, THE OMEGA-ALPHA TERM IS COMPUTED FOR DIAGNOSTICS.
!     
! PROGRAM HISTORY LOG:
!   04-02-17  JANJIC     - SEPARATED FROM ORIGINAL PDTEDT ROUTINE
!   04-11-23  BLACK      - THREADED
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
#ifdef DM_PARALLEL
      USE MODULE_DOMAIN
      USE MODULE_DM
#endif
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
#ifdef DM_PARALLEL
!     INCLUDE "mpif.h"
      TYPE (DOMAIN) :: GRID
#endif
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
!***  NMM_MAX_DIM is set in configure.wrf and must agree with
!***  the value of dimspec q in the Registry/Registry
!
      INTEGER,DIMENSION(-3:3,NMM_MAX_DIM,0:6),INTENT(IN) :: INDX3_WRK
!
      INTEGER,INTENT(IN) :: NTSD
!
      REAL,INTENT(IN) :: DT,EF4T,PT
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HBM2,PSDT,RES
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DIV,DWDT    &
     &                                                     ,HTM,RTOP
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: OMGALF,T  
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: PINT
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,IHH,IHL,IX,J,JHH,JHL,JJ,JX,K,KNT,KS,NSMUD
      INTEGER :: J1_00,J1_M1,J2_00,J2_P1
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: PRET,TPM
!
      REAL :: DWDTP,RHS,TPMP
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!***  PREPARATIONS
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        PINT(I,KTE+1,J)=PT
        TPM(I,J)=PT+PINT(I,KTE,J)
        PRET(I,J)=PSDT(I,J)*RES(I,J)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!***  KINETIC ENERGY GENERATION TERMS IN T EQUATION
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dwdtp,i,j,rhs,tpmp)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        DWDTP=DWDT(I,KTE,J)
        TPMP=PINT(I,KTE,J)+PINT(I,KTE-1,J)
!
        RHS=-DIV(I,KTE,J)*RTOP(I,KTE,J)*HTM(I,KTE,J)*DWDTP*EF4T
        OMGALF(I,KTE,J)=OMGALF(I,KTE,J)+RHS
        T(I,KTE,J)=OMGALF(I,KTE,J)*HBM2(I,J)+T(I,KTE,J)
        PINT(I,KTE,J)=PRET(I,J)*(ETA2(KTE+1)+ETA2(KTE))*DWDTP*DT        &
     &             +TPM(I,J)-PINT(I,KTE+1,J)
!
        TPM(I,J)=TPMP
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dwdtp,i,j,k,rhs,tpmp)
      DO J=MYJS,MYJE
        DO K=KTE-1,KTS+1,-1
        DO I=MYIS,MYIE
          DWDTP=DWDT(I,K,J)
          TPMP=PINT(I,K,J)+PINT(I,K-1,J)
!
          RHS=-(DIV(I,K+1,J)+DIV(I,K,J))*RTOP(I,K,J)*HTM(I,K,J)*DWDTP   &
     &         *EF4T
          OMGALF(I,K,J)=OMGALF(I,K,J)+RHS
          T(I,K,J)=OMGALF(I,K,J)*HBM2(I,J)+T(I,K,J)
          PINT(I,K,J)=PRET(I,J)*(ETA2(K+1)+ETA2(K))*DWDTP*DT            &
     &               +TPM(I,J)-PINT(I,K+1,J)
!
          TPM(I,J)=TPMP
        ENDDO
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dwdtp,i,j,rhs)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
!
        DWDTP=DWDT(I,KTS,J)
!
        RHS=-(DIV(I,KTS+1,J)+DIV(I,KTS,J))*RTOP(I,KTS,J)*HTM(I,KTS,J)   &
     &       *DWDTP*EF4T
        OMGALF(I,KTS,J)=OMGALF(I,KTS,J)+RHS
        T(I,KTS,J)=OMGALF(I,KTS,J)*HBM2(I,J)+T(I,KTS,J)
        PINT(I,KTS,J)=PRET(I,J)*(ETA2(KTS+1)+ETA2(KTS))*DWDTP*DT        &
     &                 +TPM(I,J)-PINT(I,KTS+1,J)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
      END SUBROUTINE VTOA
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE DDAMP(NTSD,DT,DETA1,DETA2,PDSL,PDTOP,DIV,HBM2,VTM      &
     &                ,T,U,V,DDMPU,DDMPV                                &
     &                ,IHE,IHW,IVE,IVW,INDX3_WRK                        &              
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    DDAMP       DIVERGENCE DAMPING
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08       
!     
! ABSTRACT:
!     DDAMP MODIFIES THE WIND COMPONENTS SO AS TO REDUCE THE
!     HORIZONTAL DIVERGENCE.
!     
! PROGRAM HISTORY LOG:
!   87-08-??  JANJIC     - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   95-03-28  BLACK      - ADDED EXTERNAL EDGE
!   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
!   01-03-12  BLACK      - CONVERTED TO WRF STRUCTURE
!   04-11-18  BLACK      - THREADED
!     
! USAGE: CALL DDAMP FROM SUBROUTINE SOLVE_RUNSTREAM
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
      IMPLICIT NONE
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
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
      INTEGER,INTENT(IN) :: NTSD
!
      REAL,INTENT(IN) :: DT,PDTOP
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DDMPU,DDMPV         &
     &                                             ,HBM2,PDSL
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: VTM
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: DIV,T    &
     &                                                        ,U,V
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
      INTEGER :: I,IER,J,J4_00,J4_M1,J4_P1,JJ,JKNT,JSTART,K,STAT
!
      REAL :: RDPDX,RDPDY
!
!***  TYPE 4 WORKING ARRAY   ! See PFDHT
!
      REAL,DIMENSION(ITS-5:ITE+5,KTS:KTE,-1:1) :: CKE,DPDE
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!***  MARCH NORTHWARD THROUGH THE SOUTHERNMOST SLABS TO BEGIN
!***  FILLING THE WORKING ARRAY NEEDED FOR AVERAGING AND
!***  DIFFERENCING IN J
!
!-----------------------------------------------------------------------
      JSTART=MYJS2
!
      DO J=-1,0
        JJ=JSTART+J
!
!$omp parallel do                                                       &
!$omp& private(i,k)
        DO K=KTS,KTE
        DO I=MYIS_P2,MYIE_P2
          DPDE(I,K,J)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,JJ)
          DIV(I,K,JJ)=DIV(I,K,JJ)*HBM2(I,JJ)
        ENDDO
        ENDDO
!
      ENDDO
!
      JKNT=0
!-----------------------------------------------------------------------
!
      main_integration : DO J=MYJS2,MYJE2
!
!-----------------------------------------------------------------------
!***
!***  SET THE 3RD INDEX OF THE WORKING ARRAYS (SEE SUBROUTINE INIT
!***                                           AND PFDHT DIAGRAMS)
!***
!***  J[TYPE]_NN WHERE "TYPE" IS THE WORKING ARRAY TYPE SEEN IN THE
!***  LOCAL DECLARATION ABOVE (DEPENDENT UPON THE J EXTENT) AND
!***  NN IS THE NUMBER OF ROWS NORTH OF THE CENTRAL ROW WHOSE J IS
!***  THE CURRENT VALUE OF THE main_integration LOOP.
!***  (P2 denotes +2, etc.)
!***
      JKNT=JKNT+1
!
      J4_P1=INDX3_WRK(1,JKNT,4)
      J4_00=INDX3_WRK(0,JKNT,4)
      J4_M1=INDX3_WRK(-1,JKNT,4)
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,k,rdpdx,rdpdy)
      DO K=KTS,KTE
!
      DO I=MYIS_P2,MYIE_P2
        DPDE(I,K,J4_P1)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J+1)
        DIV(I,K,J+1)=DIV(I,K,J+1)*HBM2(I,J+1)
      ENDDO
!
      DO I=MYIS1_P1,MYIE1_P1
        RDPDX=VTM(I,K,J)/(DPDE(I+IVW(J),K,J4_00)                        &
     &                   +DPDE(I+IVE(J),K,J4_00))
        U(I,K,J)=U(I,K,J)+(DIV(I+IVE(J),K,J)-DIV(I+IVW(J),K,J))         &
     &                    *RDPDX*DDMPU(I,J)
!
        RDPDY=VTM(I,K,J)/(DPDE(I,K,J4_M1)+DPDE(I,K,J4_P1))
        V(I,K,J)=V(I,K,J)+(DIV(I,K,J+1)-DIV(I,K,J-1))                   &
     &                    *RDPDY*DDMPV(I,J)
      ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO main_integration
!
!-----------------------------------------------------------------------
      END SUBROUTINE DDAMP
!-----------------------------------------------------------------------
      END MODULE MODULE_IGWAVE_ADJUST
!-----------------------------------------------------------------------
