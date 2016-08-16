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
      USE MODULE_MODEL_CONSTANTS, only: R_d, p608
!     USE MODULE_EXCHANGE
      USE MODULE_MPP,ONLY: MYPE
      USE MODULE_WRF_ERROR     
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
     &                ,HBM2,VBM2,VBM3                                   &
     &                ,FDIV,FCP,WPDAR,DFL,CPGFU,CPGFV                   &
     &                ,PD,PDSL,T,Q,U,V,CWM,OMGALF,PINT,DWDT             &
     &                ,RTOP,DIV,FEW,FNS,FNE,FSE                         &
     &                ,IHE,IHW,IVE,IVW                                  &
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
!   05-12-09  BLACK      - CONVERTED FROM IKJ TO IJK
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
      LOGICAL,INTENT(IN) :: FIRST,HYDRO
      INTEGER,INTENT(IN) :: SIGMA
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
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
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: CWM,DWDT    &
     &                                                     ,Q,T
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PINT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: DIV      &
     &                                                        ,OMGALF   &
     &                                                        ,RTOP,U,V
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(OUT) :: FEW,FNS    &
     &                                                      ,FNE,FSE
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: PDSL
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,J,K
!
      REAL :: SLP_STD=101300.0
!
      REAL :: APELP,DFI,DCNEK,DCSEK,DPFNEK,DPFSEK,DPNEK,DPSEK           &
     &       ,EDIV,FIUP,PRSFRC,PVNEK,PVSEK,RTOPP,VM
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: ADPDNE,ADPDSE          &
     &                                          ,ADPDX,ADPDY,APEL       &
     &                                          ,CNE,CSE,DFDZ,DPDE      &
     &                                          ,DPFEW,DPFNS            &
     &                                          ,FILO,FIM,HM            &
     &                                          ,PCEW,PCNE,PCNS,PCSE    &
     &                                          ,PCXC,PEW,PNE,PNS       &
     &                                          ,PPNE,PPSE,PSE          &
     &                                          ,RDPD,RDPDX,RDPDY       &
     &                                          ,TEW,TNE,TNS,TSE        &
     &                                          ,UDY,VDX
!
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
!$omp parallel do
      DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          OMGALF(I,J,K)=0.
          DIV(I,J,K)=0.
        ENDDO
        ENDDO
      ENDDO
!
!$omp parallel do
      DO J=JMS,JME
      DO I=IMS,IME
        PDSL(I,J)=0.
      ENDDO
      ENDDO
!
!$omp parallel do
      DO J=JTS-5,JTE+5
      DO I=ITS-5,ITE+5
        ADPDNE(I,J)=0.
        ADPDSE(I,J)=0.
        ADPDX(I,J)=0.
        ADPDY(I,J)=0.
        APEL(I,J)=0.
        CNE (I,J)=0.
        CSE (I,J)=0.
        DFDZ(I,J)=0.
        DPDE(I,J)=0.
        DPFEW(I,J)=0.
        DPFNS(I,J)=0.
        FILO(I,J)=0.
        FIM (I,J)=0.
        HM (I,J)=0.
        PCEW(I,J)=0.
        PCNE(I,J)=0.
        PCNS(I,J)=0.
        PCSE(I,J)=0.
        PCXC(I,J)=0.
        PEW (I,J)=0.
        PNE (I,J)=0.
        PNS (I,J)=0.
        PPNE(I,J)=0.
        PPSE(I,J)=0.
        PSE (I,J)=0.
        RDPD(I,J)=0.
        RDPDX(I,J)=0.
        RDPDY(I,J)=0.
        TEW (I,J)=0.
        TNE (I,J)=0.
        TNS (I,J)=0.
        TSE (I,J)=0.
        UDY (I,J)=0.
        VDX (I,J)=0.
      ENDDO
      ENDDO
!
      IF(SIGMA==1)THEN
!$omp parallel do
        DO J=MYJS_P4,MYJE_P4
        DO I=MYIS_P4,MYIE_P4
          FILO(I,J)=FIS(I,J)
          PDSL(I,J)=PD(I,J)
        ENDDO
        ENDDO
      ELSE
!$omp parallel do
        DO J=MYJS_P4,MYJE_P4
        DO I=MYIS_P4,MYIE_P4
          FILO(I,J)=0.0
          PDSL(I,J)=RES(I,J)*PD(I,J)
        ENDDO
        ENDDO
      ENDIF
!
      PRSFRC=PDTOP/(SLP_STD-PT)
!
!-----------------------------------------------------------------------
!
!***  MAIN VERTICAL INTEGRATION LOOP
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(adpdne,adpdse,adpdx,adpdy,                               &
!$omp&         apel,cne,cse,dcnek,dcsek,dfdz,dpde,dpfew,dpfnek,         &
!$omp&         dpfns,dpfsek,dpnek,ediv,few,fne,fns,fse,hm,              &
!$omp&         pcew,pcne,pcns,pcse,pcxc,pew,pne,pns,ppne,ppse,          &
!$omp&         pse,pvnek,pvsek,rdpd,rdpdx,rdpdy,tew,tne,tns,tse,        &
!$omp&         udy,vdx,vm)
!-----------------------------------------------------------------------
!
       main_integration : DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  INTEGRATE THE GEOPOTENTIAL
!-----------------------------------------------------------------------
!
        DO J=MYJS_P4,MYJE_P4
        DO I=MYIS_P4,MYIE_P4
!
          HM(I,J)=HBM2(I,J)
!
          APELP=(PINT(I,J,K+1)+PINT(I,J,K))*0.5
          RTOPP=(Q(I,J,K)*P608-CWM(I,J,K)+1.)*T(I,J,K)*R_D/APELP
          DFI=RTOPP*(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))
!
          APEL(I,J)=APELP
          RTOP(I,J,K)=RTOPP
          DFDZ(I,J)=RTOPP
!
          FIUP=FILO(I,J)+DFI
          FIM(I,J)=FILO(I,J)+FIUP
!     if(i==154.and.j==096)then
!       write(0,10281)k,q(i,j,k),cwm(i,j,k),t(i,j,k),apelp,pdsl(i,j)
10281   format(' k=',i2,' q=',z8,' cwm=',z8,' t=',z8,' apelp=',z8,' pdsl=',z8)
!     endif
          FILO(I,J)=FIUP
!
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
        DO J=MYJS_P4,MYJE_P4
        DO I=MYIS_P4,MYIE_P4
          DPDE(I,J)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
        ENDDO
        ENDDO
!
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          RDPD(I,J)=1./DPDE(I,J)
        ENDDO
        ENDDO
!
        DO J=MYJS1_P3,MYJE1_P3
        DO I=MYIS_P3,MYIE_P3
          ADPDX(I,J)=DPDE(I+IVW(J),J)+DPDE(I+IVE(J),J)
          ADPDY(I,J)=DPDE(I,J+1)+DPDE(I,J-1)
          RDPDX(I,J)=1./ADPDX(I,J)
          RDPDY(I,J)=1./ADPDY(I,J)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  DIAGONAL CONTRIBUTIONS TO PRESSURE GRADIENT FORCE
!-----------------------------------------------------------------------
!
        DO J=MYJS_P3,MYJE1_P3
        DO I=MYIS_P3,MYIE_P3
          ADPDNE(I,J)=DPDE(I+IHE(J),J+1)+DPDE(I,J)
          PNE(I,J)=(FIM (I+IHE(J),J+1)-FIM (I,J))                       &
     &            *(DWDT(I+IHE(J),J+1,K)+DWDT(I,J,K))
          PPNE(I,J)=PNE(I,J)*ADPDNE(I,J)
          CNE(I,J)=(DFDZ(I+IHE(J),J+1)+DFDZ(I,J))*2.                    &
     &            *(APEL(I+IHE(J),J+1)-APEL(I,J))
          PCNE(I,J)=CNE(I,J)*ADPDNE(I,J)
        ENDDO
        ENDDO
!
        DO J=MYJS1_P3,MYJE_P3
        DO I=MYIS_P3,MYIE_P3
          ADPDSE(I,J)=DPDE(I+IHE(J),J-1)+DPDE(I,J)
          PSE(I,J)=(FIM (I+IHE(J),J-1)-FIM (I,J))                       &
     &            *(DWDT(I+IHE(J),J-1,K)+DWDT(I,J,K))
!     if(i==154.and.j==096.and.k==kte)then
!       write(0,58391)PSE(I,J),FIM(I+IHE(J),J-1),FIM(I,J),DWDT(I+IHE(J),J-1,K),DWDT(I,J,K),ihe(j)
58391   format(' pse=',z8,' fim=',2(1x,z8),' dwdt=',2(1x,z8),' ihe=',i2)
!     endif
          PPSE(I,J)=PSE(I,J)*ADPDSE(I,J)
          CSE(I,J)=(DFDZ(I+IHE(J),J-1)+DFDZ(I,J))*2.                    &
     &            *(APEL(I+IHE(J),J-1)-APEL(I,J))
          PCSE(I,J)=CSE(I,J)*ADPDSE(I,J)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  CONTINUITY EQUATION MODIFICATION
!-----------------------------------------------------------------------
!
        DO J=MYJS1_P1,MYJE1_P1
        DO I=MYIS_P1,MYIE_P1
!     if(i==155.and.j==096.and.k==kte)then
!       write(0,72451)PNE(I+IVW(J),J),PNE(I,J-1),PSE(I+IVW(J),J),PSE(I,J+1),ivw(j)
!       write(0,72452)CNE(I+IVW(J),J),CNE(I,J-1),CSE(I+IVW(J),J),CSE(I,J+1)
72451   format(' pne=',2(1x,z8),' pse=',2(1x,z8),' ivw=',i2)
72452   format(' cne=',2(1x,z8),' cse=',2(1x,z8))
!     endif
          PCXC(I,J)=VBM3(I,J)*                                          &
     &             (PNE(I+IVW(J),J)+CNE(I+IVW(J),J)                     &
     &             +PSE(I+IVW(J),J)+CSE(I+IVW(J),J)                     &
     &             -PNE(I,J-1)-CNE(I,J-1)                               &
     &             -PSE(I,J+1)-CSE(I,J+1))
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
!     if(i==155.and.j==095.and.k==kte)then
!       write(0,76501)deta1(k),deta2(k),prsfrc,wpdar(i,j),ihe(j),ihw(j)
!       write(0,76502)PCXC(I+IHE(J),J),PCXC(I,J+1),PCXC(I+IHW(J),J),PCXC(I,J-1)
76501   format(' deta1=',z8,' deta2=',z8,' prsfrc=',z8,' wpdar=',z8,' ihe=',i2,' ihw=',i2)
76502   format(' pcxc=',4(1x,z8))
!     endif
          DIV(I,J,K)=(DETA1(K)*PRSFRC                                   &   
     &               +DETA2(K)*(1.-PRSFRC))*WPDAR(I,J)                  &
     &              *(PCXC(I+IHE(J),J)-PCXC(I,J+1)                      &
     &               +PCXC(I+IHW(J),J)-PCXC(I,J-1))
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  LATITUDINAL AND LONGITUDINAL PRESSURE FORCE COMPONENTS
!-----------------------------------------------------------------------
!
        DO J=MYJS1_P2,MYJE1_P2
        DO I=MYIS_P2,MYIE_P3
          DPNEK=PNE(I+IVW(J),J)+PNE(I,J-1)
          DPSEK=PSE(I+IVW(J),J)+PSE(I,J+1)
          PEW(I,J)=DPNEK+DPSEK
          PNS(I,J)=DPNEK-DPSEK
          DCNEK=CNE(I+IVW(J),J)+CNE(I,J-1)
          DCSEK=CSE(I+IVW(J),J)+CSE(I,J+1)
          PCEW(I,J)=(DCNEK+DCSEK)*ADPDX(I,J)
          PCNS(I,J)=(DCNEK-DCSEK)*ADPDY(I,J)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
        IF(.NOT.FIRST)THEN     ! Skip at timestep 0
!
!-----------------------------------------------------------------------
!***  UPDATE U AND V FOR PRESSURE GRADIENT FORCE
!-----------------------------------------------------------------------
!
          DO J=MYJS2_P2,MYJE2_P2
          DO I=MYIS_P2,MYIE1_P2
            DPFNEK=((PPNE(I+IVW(J),J)+PPNE(I,J-1))                      &
     &             +(PCNE(I+IVW(J),J)+PCNE(I,J-1)))
            DPFNEK=DPFNEK+DPFNEK
            DPFSEK=((PPSE(I+IVW(J),J)+PPSE(I,J+1))                      &
     &             +(PCSE(I+IVW(J),J)+PCSE(I,J+1)))
            DPFSEK=DPFSEK+DPFSEK
            DPFEW(I,J)=DPFNEK+DPFSEK
            DPFNS(I,J)=DPFNEK-DPFSEK
          ENDDO
          ENDDO
!
!-----------------------------------------------------------------------
!
          DO J=MYJS2_P3,MYJE2_P3
          DO I=MYIS_P2,MYIE1_P2
            VM=VBM2(I,J)
            U(I,J,K)=(((DPFEW(I,J)+PCEW(I,J))*RDPDX(I,J)                &
     &                 +PEW(I,J))*CPGFU(I,J))*VM+U(I,J,K)
            V(I,J,K)=(((DPFNS(I,J)+PCNS(I,J))*RDPDY(I,J)                &
     &                 +PNS(I,J))*CPGFV)*VM+V(I,J,K)
          ENDDO
          ENDDO
!
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
!***  LATITUDINAL AND LONGITUDINAL FLUXES AND OMEGA-ALPHA COMPONENTS
!-----------------------------------------------------------------------
!
          DO J=MYJS1_P2,MYJE1_P2
          DO I=MYIS_P2,MYIE_P3
            UDY(I,J)=DY*U(I,J,K)
            FEW(I,J,K)=UDY(I,J)*ADPDX(I,J)
            TEW(I,J)=UDY(I,J)*PCEW(I,J)
            VDX(I,J)=DX(I,J)*V(I,J,K)
!     if(i==178.and.j==003.and.k==53)then
!       write(0,77601)udy(i,j),dy,u(i,j,k)
77601   format(' udy=',z8,' dy=',z8,' u=',z8)
!     endif
            FNS(I,J,K)=VDX(I,J)*ADPDY(I,J)
            TNS(I,J)=VDX(I,J)*PCNS(I,J)
          ENDDO
          ENDDO
!
!-----------------------------------------------------------------------
!***  DIAGONAL FLUXES AND DIAGONALLY AVERAGED WIND
!-----------------------------------------------------------------------
!
          DO J=MYJS1_P1,MYJE2_P1
          DO I=MYIS_P1,MYIE1_P1
            PVNEK=(UDY(I+IHE(J),J)+VDX(I+IHE(J),J))                     &
     &           +(UDY(I,J+1)+VDX(I,J+1))
            FNE(I,J,K)=PVNEK*ADPDNE(I,J)
!     if(i==178.and.j==003.and.k==53)then
!       write(0,33781)fne(i,j,k),dpde(i+ihe(j),j+1),dpde(i,j),ihe(j)
!       write(0,33782)udy(i+ihe(j),j),udy(i,j+1),vdx(i+ihe(j),j),vdx(i,j+1)
33781   format(' fne=',z8,' dpdne=',2(1x,z8),' ihe=',i2)
33782   format(' udy=',2(1x,z8),' vdx=',2(1x,z8))
!     endif
            TNE(I,J)=PVNEK*PCNE(I,J)*2.
          ENDDO
          ENDDO
!
          DO J=MYJS2_P1,MYJE1_P1
          DO I=MYIS_P1,MYIE1_P1
            PVSEK=(UDY(I+IHE(J),J)-VDX(I+IHE(J),J))                     &
     &           +(UDY(I,J-1)-VDX(I,J-1))
            FSE(I,J,K)=PVSEK*ADPDSE(I,J)
            TSE(I,J)=PVSEK*PCSE(I,J)*2.
          ENDDO
          ENDDO
!
!-----------------------------------------------------------------------
!***  HORIZONTAL PART OF OMEGA-ALPHA & DIVERGENCE
!-----------------------------------------------------------------------
!
          DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            OMGALF(I,J,K)=(TEW(I+IHE(J),J)+TEW(I+IHW(J),J)              &
     &                    +TNS(I,J+1)     +TNS(I,J-1)                   &
     &                    +TNE(I,J)       +TNE(I+IHW(J),J-1)            &
     &                    +TSE(I,J)       +TSE(I+IHW(J),J+1))           &
     &                    *RDPD(I,J)*FCP(I,J)*HM(I,J)
! 
!     if(i==178.and.j==003.and.k==53)then
!       write(0,36311)div(i,j,k),fdiv(i,j),ihe(j),ihw(j)
!       write(0,36312)FEW(I+IHE(J),J,K),FEW(I+IHW(J),J,K),FNS(I,J+1,K),FNS(I,J-1,K)
!       write(0,36313)FNE(I,J,K),FNE(I+IHW(J),J-1,K),FSE(I,J,K),FSE(I+IHW(J),J+1,K)
36311   format(' PFDHT div=',z8,' fdiv=',z8,' ihe=',i2,' ihw=',i2)
36312   format(' few=',2(1x,z8),' fns=',2(1x,z8))
36313   format(' fne=',2(1x,z8),' fse=',2(1x,z8))
!     endif
            EDIV=(FEW(I+IHE(J),J,K)  +FNS(I,J+1,K)                      &
                 +FNE(I,J,K)         +FSE(I,J,K)                        &
                -(FEW(I+IHW(J),J,K)  +FNS(I,J-1,K)                      &
                 +FNE(I+IHW(J),J-1,K)+FSE(I+IHW(J),J+1,K)))*FDIV(I,J)
!
            DIV(I,J,K)=(EDIV+DIV(I,J,K))*HM(I,J)
          ENDDO
          ENDDO
!
!-----------------------------------------------------------------------
!
        ENDIF   !End block to skip execution at last timestep
!
!-----------------------------------------------------------------------
!
      ENDDO main_integration
!
!-----------------------------------------------------------------------
!     call hpm_stop('PFDHT')
!-----------------------------------------------------------------------
!
      END SUBROUTINE PFDHT
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE PDTE(                                                  &
#ifdef DM_PARALLEL
     &                GRID,MYPE,MPI_COMM_COMP,                          &
#endif
     &                NTSD,DT,PT,ETA2,RES,HYDRO,HBM2                    &
     &               ,PD,PDSL,PDSLO                                     &
     &               ,PETDT,DIV,PSDT                                    &
     &               ,IHE,IHW,IVE,IVW                                   &                 
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
!   05-12-09  BLACK      - CONVERTED FROM IKJ TO IJK
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
      USE module_domain, ONLY: DOMAIN
      USE MODULE_DM,                    ONLY : LOCAL_COMMUNICATOR       &
                                              ,MYTASK,NTASKS,NTASKS_X   &
                                              ,NTASKS_Y                 &
                                              ,wrf_dm_sum_real          &
                                              ,wrf_dm_sum_integer          
      USE MODULE_COMM_DM, only: HALO_NMM_E_sub
#endif
!-----------------------------------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------------------------------
#ifdef DM_PARALLEL
      TYPE (DOMAIN) :: GRID
      INTEGER,INTENT(IN) :: MYPE,MPI_COMM_COMP
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
      INTEGER,INTENT(IN) :: NTSD
!
      REAL,INTENT(IN) :: DT,PT
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: RES,HBM2   
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: DIV
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: PD,PDSL
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: PETDT
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: PDSLO,PSDT
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,IHH,IHL,IX,J,JHH,JHL,JX,K,KS,NSMUD
      INTEGER :: MY_IS_GLB,MY_IE_GLB,MY_JS_GLB,MY_JE_GLB
      INTEGER :: LOC_NPTS, GLB_NPTS
#ifdef DM_PARALLEL
      INTEGER :: IPS,IPE,JPS,JPE,KPS,KPE,IRET
#endif
!#ifdef DEREF_KLUDGE
!! SEE http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
!      INTEGER :: SM31,EM31,SM32,EM32,SM33,EM33
!      INTEGER :: SM31X,EM31X,SM32X,EM32X,SM33X,EM33X
!      INTEGER :: SM31Y,EM31Y,SM32Y,EM32Y,SM33Y,EM33Y
!#endif
!
      REAL :: PETDTL, TASK_CHANGE, GLOBAL_CHANGE, GLOBAL_CHANGE_WRF
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: HBMS,PNE,PRET,PSE
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!#include "deref_kludge.h"
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
!
!-----------------------------------------------------------------------
!***  VERTICALLY INTEGRATE THE HORIZONTAL DIVERGENCE
!-----------------------------------------------------------------------
!

      LOC_NPTS=0

!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO K=KTE-1,KTS,-1
        DO J=MYJS_P2,MYJE_P2
        DO I=MYIS_P2,MYIE_P2
          DIV(I,J,K)=DIV(I,J,K+1)+DIV(I,J,K)
        if (K .eq. KTS) then
         LOC_NPTS=LOC_NPTS+1
        endif

        ENDDO
        ENDDO
      ENDDO

#ifdef DM_PARALLEL
      GLB_NPTS=wrf_dm_sum_integer(LOC_NPTS)
#else
      GLB_NPTS=LOC_NPTS
#endif

!
!-----------------------------------------------------------------------
!***  COMPUTATION OF PRESSURE TENDENCY
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        PSDT(I,J)=-DIV(I,J,KTS)
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

      TASK_CHANGE=0.

      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        PD(I,J)=PSDT(I,J)*DT+PD(I,J)
        PRET(I,J)=PSDT(I,J)*RES(I,J)
        PDSL(I,J)=PD(I,J)*RES(I,J)
        TASK_CHANGE=TASK_CHANGE+abs(PSDT(I,J)*108./DT)  ! .01*10800/dt (hPa/3 h)
      ENDDO
      ENDDO

#ifdef DM_PARALLEL
      GLOBAL_CHANGE_WRF=wrf_dm_sum_real(TASK_CHANGE)/GLB_NPTS
#else
      GLOBAL_CHANGE_WRF=TASK_CHANGE/GLB_NPTS
#endif

#ifdef DM_PARALLEL
      if ( MYPE == 0 ) then
        write(wrf_err_message,*) 'avg global change (hPa/3h): ', GLOBAL_CHANGE_WRF
        call wrf_debug(1,wrf_err_message)
      endif
#else
        write(wrf_err_message,*) 'avg global change (hPa/3h): ', GLOBAL_CHANGE_WRF
        call wrf_debug(1,wrf_err_message)
#endif

!
!-----------------------------------------------------------------------
!***  COMPUTATION OF PETDT
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
      DO K=KTE-1,KTS,-1
        DO J=MYJS_P2,MYJE_P2
        DO I=MYIS_P2,MYIE_P2
          PETDT(I,J,K)=-(PRET(I,J)*ETA2(K+1)+DIV(I,J,K+1))              &
     &                  *HBM2(I,J)
        ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  SMOOTHING VERTICAL VELOCITY ALONG BOUNDARIES
!-----------------------------------------------------------------------
!
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
!***  PNE AT H(I,J) LIES BETWEEN (I,J) AND THE H POINT TO THE NE.
!***  PSE AT H(I,J) LIES BETWEEN (I,J) AND THE H POINT TO THE SE.
!
!$omp parallel do                                                       &
!$omp& private(i,j,k,petdtl,pne,pse)
!
          DO K=KTS+1,KTE
!
            DO J=MYJS_P1,MYJE1_P1
            DO I=MYIS_P1,MYIE1_P1
              PNE(I,J)=PETDT(I+IHE(J),J+1,K)-PETDT(I,J,K)
            ENDDO
            ENDDO
!
            DO J=MYJS1_P1,MYJE_P1
            DO I=MYIS_P1,MYIE1_P1
              PSE(I,J)=PETDT(I+IHE(J),J-1,K)-PETDT(I,J,K)
            ENDDO
            ENDDO
!
            DO J=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              PETDTL=(PNE(I,J)-PNE(I+IHW(J),J-1)                        &
     &               +PSE(I,J)-PSE(I+IHW(J),J+1))*HBM2(I,J)
              PETDT(I,J,K)=PETDTL*HBMS(I,J)*0.125+PETDT(I,J,K)
            ENDDO
            ENDDO
!
          ENDDO
!
#ifdef DM_PARALLEL
!          IPS=ITS;IPE=ITE;JPS=JTS;JPE=JTE;KPS=KTS;KPE=KTE
# include "HALO_NMM_E.inc"
#endif
!-----------------------------------------------------------------------
!
        ENDDO  ! End of smoothing loop
!
!-----------------------------------------------------------------------
!
      ENDIF nonhydrostatic_smoothing
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE PDTE
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      SUBROUTINE VTOA(                                                  &
     &                NTSD,DT,PT,ETA2                                   &
     &               ,HBM2,EF4T                                         &
     &               ,T,DWDT,RTOP,OMGALF                                &
     &               ,PINT,DIV,PSDT,RES                                 &
     &               ,IHE,IHW,IVE,IVW                                   &                 
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
!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
                           ,IMS,IME,JMS,JME,KMS,KME                     &
                           ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
      INTEGER,INTENT(IN) :: NTSD
!
      REAL,INTENT(IN) :: DT,EF4T,PT
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: HBM2,PSDT,RES
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: DIV,DWDT    &
     &                                                     ,RTOP
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: OMGALF,T  
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: PINT
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,J,K
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
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        PINT(I,J,KTE+1)=PT
        TPM(I,J)=PT+PINT(I,J,KTE)
        PRET(I,J)=PSDT(I,J)*RES(I,J)
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  KINETIC ENERGY GENERATION TERMS IN T EQUATION
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(dwdtp,i,j,rhs,tpmp)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        DWDTP=DWDT(I,J,KTE)
        TPMP=PINT(I,J,KTE)+PINT(I,J,KTE-1)
!
        RHS=-DIV(I,J,KTE)*RTOP(I,J,KTE)*DWDTP*EF4T
        OMGALF(I,J,KTE)=OMGALF(I,J,KTE)+RHS
        T(I,J,KTE)=OMGALF(I,J,KTE)*HBM2(I,J)+T(I,J,KTE)
        PINT(I,J,KTE)=PRET(I,J)*(ETA2(KTE+1)+ETA2(KTE))*DWDTP*DT        &
     &             +TPM(I,J)-PINT(I,J,KTE+1)
!
        TPM(I,J)=TPMP
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dwdtp,i,j,k,rhs,tpmp)
      DO K=KTE-1,KTS+1,-1
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          DWDTP=DWDT(I,J,K)
          TPMP=PINT(I,J,K)+PINT(I,J,K-1)
!
          RHS=-(DIV(I,J,K+1)+DIV(I,J,K))*RTOP(I,J,K)*DWDTP*EF4T
          OMGALF(I,J,K)=OMGALF(I,J,K)+RHS
          T(I,J,K)=OMGALF(I,J,K)*HBM2(I,J)+T(I,J,K)
          PINT(I,J,K)=PRET(I,J)*(ETA2(K+1)+ETA2(K))*DWDTP*DT            &
     &               +TPM(I,J)-PINT(I,J,K+1)
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
        DWDTP=DWDT(I,J,KTS)
!
!     if(i==77.and.j==53)then
!       write(0,28361)t(i,j,kts),omgalf(i,j,kts),rtop(i,j,kts),dwdtp
!       write(0,28362)div(i,j,kts),div(i,j,kts+1),ef4t
28361   format(' t=',z8,' omgalf=',z8,' rtop=',z8,' dwdtp=',z8)
28362   format(' div=',2(1x,z8),' ef4t=',z8)
!     endif
        RHS=-(DIV(I,J,KTS+1)+DIV(I,J,KTS))*RTOP(I,J,KTS)*DWDTP*EF4T
        OMGALF(I,J,KTS)=OMGALF(I,J,KTS)+RHS
        T(I,J,KTS)=OMGALF(I,J,KTS)*HBM2(I,J)+T(I,J,KTS)
        PINT(I,J,KTS)=PRET(I,J)*(ETA2(KTS+1)+ETA2(KTS))*DWDTP*DT        &
     &                 +TPM(I,J)-PINT(I,J,KTS+1)
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
      END SUBROUTINE VTOA
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE DDAMP(NTSD,DT,DETA1,DETA2,PDSL,PDTOP,DIV,HBM2          &
     &                ,T,U,V,DDMPU,DDMPV                                &
     &                ,IHE,IHW,IVE,IVW                                  &              
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
!   05-12-09  BLACK      - CONVERTED FROM IKJ TO IJK
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
      INTEGER,INTENT(IN) :: NTSD
!
      REAL,INTENT(IN) :: DT,PDTOP
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DDMPU,DDMPV         &
     &                                             ,HBM2,PDSL
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: DIV,T    &
     &                                                        ,U,V
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER :: I,J,K
!
      REAL :: FCIM,FCXM,RDPDX,RDPDY
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: DIVE,DPDE,PDE          &
     &                                          ,XDIVX,XDIVY
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=JTS-5,JTE+5
      DO I=ITS-5,ITE+5
        PDE(I,J)=0.
        DPDE(I,J)=0.
        XDIVX(I,J)=0.
        XDIVY(I,J)=0.
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      FCXM=1.
!
!$omp parallel do
!$omp& private(i,j)
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        PDE (I,J)=PDSL(I,J)+PDTOP
        DIVE(I,J)=0.
      ENDDO
      ENDDO
!
      DO K=KTS,KTE
!$omp parallel do                                                       &
!$omp& private(i,j)
        DO J=MYJS_P2,MYJE_P2
        DO I=MYIS_P2,MYIE_P2
          DIVE(I,J)=DIV(I,J,K)*HBM2(I,J)+DIVE(I,J)
        ENDDO
        ENDDO
      ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,rdpdx,rdpdy,fcxm)
      DO J=MYJS2,MYJE2
      DO I=MYIS1_P1,MYIE1_P1
        RDPDX=DDMPU(I,J)*FCXM                                           &
     &       /(PDE(I+IVW(J),J)  +PDE(I+IVE(J),J))    
        RDPDY=DDMPV(I,J)*FCXM                                           &
     &       /(PDE(I       ,J-1)+PDE(I     ,J+1))
!
        XDIVX(I,J)=(DIVE(I+IVE(J),J  )-DIVE(I+IVW(J),J  ))*RDPDX
        XDIVY(I,J)=(DIVE(I       ,J+1)-DIVE(I       ,J-1))*RDPDY
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      FCIM=1.
!
!$omp parallel do                                                       &
!$omp& private(dpde,i,j,k,rdpdx,rdpdy,fcim)
!
      fcim_loop: DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!
        DO J=MYJS_P2,MYJE_P2
        DO I=MYIS_P1,MYIE_P1
          DPDE(I,J)=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
          DIV(I,J,K)=DIV(I,J,K)*HBM2(I,J)
        ENDDO
        ENDDO
!
        DO J=MYJS2,MYJE2
        DO I=MYIS1_P1,MYIE1_P1
          RDPDX=DDMPU(I,J)*FCIM                                        &
     &         /(DPDE(I+IVW(J),J)  +DPDE(I+IVE(J),J))
          RDPDY=DDMPV(I,J)*FCIM                                        &
     &         /(DPDE(I       ,J-1)+DPDE(I       ,J+1))
          U(I,J,K)=((DIV(I+IVE(J),J,K  )-DIV(I+IVW(J),J,K  ))*RDPDX    &
     &             +XDIVX(I,J))+U(I,J,K)
          V(I,J,K)=((DIV(I       ,J+1,K)-DIV(I       ,J-1,K))*RDPDY    &
     &             +XDIVY(I,J))+V(I,J,K)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO fcim_loop
!
!-----------------------------------------------------------------------
!

      END SUBROUTINE DDAMP
!
!-----------------------------------------------------------------------
!
      END MODULE MODULE_IGWAVE_ADJUST
!
!-----------------------------------------------------------------------
