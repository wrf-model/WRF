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
#if defined(DM_PARALLEL) && !defined(STUBMPI)
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
     &               ,HBM2,VBM2                                         &
     &               ,T,U,V,PDSLO,TOLD,UOLD,VOLD                        &
     &               ,PETDT,UPSTRM                                      &
     &               ,FEW,FNS,FNE,FSE                                   &
     &               ,ADT,ADU,ADV                                       &
     &               ,N_IUP_H,N_IUP_V                                   &
     &               ,N_IUP_ADH,N_IUP_ADV                               &
     &               ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                       &
     &               ,IHE,IHW,IVE,IVW                                   &
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
!   05-12-14  BLACK        - CONVERTED FROM IKJ TO IJK
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
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW         &
                                               ,N_IUP_H,N_IUP_V         &
     &                                         ,N_IUP_ADH,N_IUP_ADV
!
      INTEGER, DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V     &
     &                                                 ,IUP_ADH,IUP_ADV
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
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: ADT,ADU,ADV
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PETDT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: T,TOLD   &
     &                                                        ,U,UOLD   &
     &                                                        ,V,VOLD
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(OUT) :: FEW,FNE    &
     &                                                      ,FNS,FSE
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
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
      INTEGER,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: ISPA,ISQA
!
      REAL :: ADPDX,ADPDY,ARRAY3_X,CFL,CFT,CFU,CFV,CMT,CMU,CMV          &
     &       ,DTE,DTQ,F0,F1,F2,F3,FEWP,FNEP,FNSP,FPP,FSEP,HM            &
     &       ,PDOP,PDOPU,PDOPV,PP                                       &
     &       ,PVVLO,PVVLOU,PVVLOV,PVVUP,PVVUPU,PVVUPV                   &
     &       ,QP,RDP,RDPU,RDPV                                          &
     &       ,TEMPA,TEMPB,TTA,TTB,UDY                                   &
     &       ,VDX,VM,VVLO,VVLOU,VVLOV,VVUP,VVUPU,VVUPV
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: ARRAY0,ARRAY1          &
     &                                          ,ARRAY2,ARRAY3          &
     &                                          ,DPDE,RDPD,RDPDX,RDPDY  &
     &                                          ,TEW,TNE,TNS,TSE,TST    &
     &                                          ,UNE,UNED,UEW,UNS,USE   &
     &                                          ,USED,UST               &
     &                                          ,VEW,VNE,VNS,VSE        &
     &                                          ,VST
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5,KTS:KTE) :: VAD_TEND_T     &
     &                                                  ,VAD_TEND_U     &
     &                                                  ,VAD_TEND_V
!
      REAL,DIMENSION(KTS:KTE) :: CRT,CRU,CRV,DETA1_PDTOP                &
     &                          ,RCMT,RCMU,RCMV,RSTT,RSTU,RSTV          &
     &                          ,T_K,TN,U_K,UN,V_K,VN
!
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
     DO J=JTS-5,JTE+5
     DO I=ITS-5,ITE+5
       ARRAY0(I,J)=0.0
       ARRAY1(I,J)=0.0
       ARRAY2(I,J)=0.0
       ARRAY3(I,J)=0.0
       DPDE(I,J)=0.0
       RDPD(I,J)=0.0
       RDPDX(I,J)=0.0
       RDPDY(I,J)=0.0
       TEW(I,J)=0.0
       TNE(I,J)=0.0
       TNS(I,J)=0.0
       TSE(I,J)=0.0
       TST(I,J)=0.0
       UNE(I,J)=0.0
       UNED(I,J)=0.0
       UEW(I,J)=0.0
       UNS(I,J)=0.0
       USE(I,J)=0.0
       USED(I,J)=0.0
       UST(I,J)=0.0
       VEW(I,J)=0.0
       VNE(I,J)=0.0
       VNS(I,J)=0.0
       VSE(I,J)=0.0
       VST(I,J)=0.0
     ENDDO
     ENDDO
!-----------------------------------------------------------------------
!
      DTQ=DT*0.25
      DTE=DT*(0.5*0.25)
!
!-----------------------------------------------------------------------
!***
!***  PRECOMPUTE DETA1 TIMES PDTOP.
!***
!-----------------------------------------------------------------------
!
      DO K=KTS,KTE
        DETA1_PDTOP(K)=DETA1(K)*PDTOP
      ENDDO
!
!-----------------------------------------------------------------------
!***
!***  INITIALIZE SOME WORKING ARRAYS TO ZERO
!***
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!***  COMPUTE VERTICAL ADVECTION TENDENCIES USING CRANK-NICHOLSON.
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  FIRST THE TEMPERATURE
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(cft,cfu,cfv,cmt,cmu,cmv,crt,cru,crv,i,k                  &
!$omp&        ,pdop,pdopu,pdopv,pvvlo,pvvlou,pvvlov,pvvup,pvvupu,pvvupv &
!$omp&        ,rcmt,rcmu,rcmv,rdp,rdpu,rdpv,rstt,rstu,rstv,t_k,tn       &
!$omp&        ,u_k,un,v_k,vn,vvlo,vvlou,vvlov,vvup,vvupu,vvupv)
!!$omp& private(adtp,adup,advp,ttlo,ttup,tulo,tuup,tvlo,tvup)
!-----------------------------------------------------------------------
!
      main_vertical: DO J=MYJS2,MYJE2
!
!-----------------------------------------------------------------------
!
        iloop_for_t: DO I=MYIS1,MYIE1
!
!-----------------------------------------------------------------------
!***  EXTRACT T FROM THE COLUMN
!-----------------------------------------------------------------------
!
          DO K=KTS,KTE
            T_K(K)=T(I,J,K)
          ENDDO
!
!-----------------------------------------------------------------------
!
          PDOP=PDSLO(I,J)
          PVVLO=PETDT(I,J,KTE-1)*DTQ
          VVLO=PVVLO/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOP)
          CMT=-VVLO*WGT2+1.
          RCMT(KTE)=1./CMT
          CRT(KTE)=VVLO*WGT2
          RSTT(KTE)=-VVLO*WGT1*(T_K(KTE-1)-T_K(KTE))+T_K(KTE)
!
!-----------------------------------------------------------------------
!
          DO K=KTE-1,KTS+1,-1
            RDP=1./(DETA1_PDTOP(K)+DETA2(K)*PDOP)
            PVVUP=PVVLO
            PVVLO=PETDT(I,J,K-1)*DTQ
            VVUP=PVVUP*RDP
            VVLO=PVVLO*RDP
            CFT=-VVUP*WGT2*RCMT(K+1)
            CMT=-CRT(K+1)*CFT+((VVUP-VVLO)*WGT2+1.)
            RCMT(K)=1./CMT
            CRT(K)=VVLO*WGT2
            RSTT(K)=-RSTT(K+1)*CFT+T_K(K)                               &
       &            -(T_K(K)-T_K(K+1))*VVUP*WGT1                        &
       &            -(T_K(K-1)-T_K(K))*VVLO*WGT1
          ENDDO
!
!-----------------------------------------------------------------------
!
          PVVUP=PVVLO
          VVUP=PVVUP/(DETA1_PDTOP(KTS)+DETA2(KTS)*PDOP)
          CFT=-VVUP*WGT2*RCMT(KTS+1)
          CMT=-CRT(KTS+1)*CFT+VVUP*WGT2+1.
          CRT(KTS)=0.
          RSTT(KTS)=-(T_K(KTS)-T_K(KTS+1))*VVUP*WGT1                    &
      &              -RSTT(KTS+1)*CFT+T_K(KTS)
          TN(KTS)=RSTT(KTS)/CMT
          VAD_TEND_T(I,J,KTS)=TN(KTS)-T_K(KTS)
!
          DO K=KTS+1,KTE
            TN(K)=(-CRT(K)*TN(K-1)+RSTT(K))*RCMT(K)
            VAD_TEND_T(I,J,K)=TN(K)-T_K(K)
          ENDDO
!
!-----------------------------------------------------------------------
!***  The following section is only for checking the implicit solution
!***  using back-substitution.  Remove this section otherwise.
!-----------------------------------------------------------------------
!       if(ntsd<=10.or.ntsd>=6000)then
!       IF(I==ITEST.AND.J==JTEST)THEN
!!
!         PVVLO=PETDT(I,J,KTE-1)*DT*0.25
!         VVLO=PVVLO/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOP)
!         TTLO=VVLO*(T(I,J,KTE-1)-T(I,J,KTE)                            &
!    &              +TN(KTE-1)-TN(KTE))
!         ADTP=TTLO+TN(KTE)-T(I,J,KTE)
!         WRITE(0,*)' NTSD=',NTSD,' I=',ITEST,' J=',JTEST,' K=',KTE     &
!    &,             ' ADTP=',ADTP
!         WRITE(0,*)' T=',T(I,J,KTE),' TN=',TN(KTE)                     &
!    &,               ' VAD_TEND_T=',VAD_TEND_T(I,J,KTE)
!         WRITE(0,*)' '
!!
!         DO K=KTE-1,KTS+1,-1
!           RDP=1./(DETA1_PDTOP(K)+DETA2(K)*PDOP)
!           PVVUP=PVVLO
!           PVVLO=PETDT(I,J,K-1)*DT*0.25
!           VVUP=PVVUP*RDP
!           VVLO=PVVLO*RDP
!           TTUP=VVUP*(T(I,J,K)-T(I,J,K+1)+TN(K)-TN(K+1))
!           TTLO=VVLO*(T(I,J,K-1)-T(I,J,K)+TN(K-1)-TN(K))
!           ADTP=TTLO+TTUP+TN(K)-T(I,J,K)
!           WRITE(0,*)' NTSD=',NTSD,' I=',I,' J=',J,' K=',K             &
!    &,               ' ADTP=',ADTP
!           WRITE(0,*)' T=',T(I,J,K),' TN=',TN(K)                       &
!    &,               ' VAD_TEND_T=',VAD_TEND_T(I,J,K)
!           WRITE(0,*)' '
!         ENDDO
!!
!         PVVUP=PVVLO
!         VVUP=PVVUP/(DETA1_PDTOP(KTS)+DETA2(KTS)*PDOP)
!         TTUP=VVUP*(T(I,J,KTS)-T(I,J,KTS+1)+TN(KTS)-TN(KTS+1))
!         ADTP=TTUP+TN(KTS)-T(I,J,KTS)
!         WRITE(0,*)' NTSD=',NTSD,' I=',I,' J=',J,' K=',KTS             &
!    &,             ' ADTP=',ADTP
!         WRITE(0,*)' T=',T(I,J,KTS),' TN=',TN(KTS)                     &
!    &,             ' VAD_TEND_T=',VAD_TEND_T(I,J,KTS)
!         WRITE(0,*)' '
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
!
!***  NOW VERTICAL ADVECTION OF WIND COMPONENTS
!
!-----------------------------------------------------------------------
!
        iloop_for_uv:  DO I=MYIS1,MYIE1
!
!-----------------------------------------------------------------------
!***  EXTRACT U AND V FROM THE COLUMN
!-----------------------------------------------------------------------
!
          DO K=KTS,KTE
            U_K(K)=U(I,J,K)
            V_K(K)=V(I,J,K)
          ENDDO
!
!-----------------------------------------------------------------------
!
          PDOPU=(PDSLO(I+IVW(J),J)+PDSLO(I+IVE(J),J))*0.5
          PDOPV=(PDSLO(I,J-1)+PDSLO(I,J+1))*0.5
          PVVLOU=(PETDT(I+IVW(J),J,KTE-1)+PETDT(I+IVE(J),J,KTE-1))*DTE
          PVVLOV=(PETDT(I,J-1,KTE-1)+PETDT(I,J+1,KTE-1))*DTE
          VVLOU=PVVLOU/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOPU)
          VVLOV=PVVLOV/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOPV)
          CMU=-VVLOU*WGT2+1.
          CMV=-VVLOV*WGT2+1.
          RCMU(KTE)=1./CMU
          RCMV(KTE)=1./CMV
          CRU(KTE)=VVLOU*WGT2
          CRV(KTE)=VVLOV*WGT2
          RSTU(KTE)=-VVLOU*WGT1*(U_K(KTE-1)-U_K(KTE))+U_K(KTE)
          RSTV(KTE)=-VVLOV*WGT1*(V_K(KTE-1)-V_K(KTE))+V_K(KTE)
!
!-----------------------------------------------------------------------
!
          DO K=KTE-1,KTS+1,-1
            RDPU=1./(DETA1_PDTOP(K)+DETA2(K)*PDOPU)
            RDPV=1./(DETA1_PDTOP(K)+DETA2(K)*PDOPV)
            PVVUPU=PVVLOU
            PVVUPV=PVVLOV
            PVVLOU=(PETDT(I+IVW(J),J,K-1)+PETDT(I+IVE(J),J,K-1))*DTE
            PVVLOV=(PETDT(I,J-1,K-1)+PETDT(I,J+1,K-1))*DTE
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
            RSTU(K)=-RSTU(K+1)*CFU+U_K(K)                               &
     &              -(U_K(K)-U_K(K+1))*VVUPU*WGT1                       &
     &              -(U_K(K-1)-U_K(K))*VVLOU*WGT1
            RSTV(K)=-RSTV(K+1)*CFV+V_K(K)                               &
     &              -(V_K(K)-V_K(K+1))*VVUPV*WGT1                       &
     &              -(V_K(K-1)-V_K(K))*VVLOV*WGT1
          ENDDO
!
!-----------------------------------------------------------------------
!
          RDPU=1./(DETA1_PDTOP(KTS)+DETA2(KTS)*PDOPU)
          RDPV=1./(DETA1_PDTOP(KTS)+DETA2(KTS)*PDOPV)
          PVVUPU=PVVLOU
          PVVUPV=PVVLOV
          VVUPU=PVVUPU*RDPU
          VVUPV=PVVUPV*RDPV
          CFU=-VVUPU*WGT2*RCMU(KTS+1)
          CFV=-VVUPV*WGT2*RCMV(KTS+1)
          CMU=-CRU(KTS+1)*CFU+VVUPU*WGT2+1.
          CMV=-CRV(KTS+1)*CFV+VVUPV*WGT2+1.
          CRU(KTS)=0.
          CRV(KTS)=0.
          RSTU(KTS)=-(U_K(KTS)-U_K(KTS+1))*VVUPU*WGT1                   &
       &               -RSTU(KTS+1)*CFU+U_K(KTS)
          RSTV(KTS)=-(V_K(KTS)-V_K(KTS+1))*VVUPV*WGT1                   &
       &               -RSTV(KTS+1)*CFV+V_K(KTS)
          UN(KTS)=RSTU(KTS)/CMU
          VN(KTS)=RSTV(KTS)/CMV
          VAD_TEND_U(I,J,KTS)=UN(KTS)-U_K(KTS)
          VAD_TEND_V(I,J,KTS)=VN(KTS)-V_K(KTS)
!
          DO K=KTS+1,KTE
            UN(K)=(-CRU(K)*UN(K-1)+RSTU(K))*RCMU(K)
            VN(K)=(-CRV(K)*VN(K-1)+RSTV(K))*RCMV(K)
            VAD_TEND_U(I,J,K)=UN(K)-U_K(K)
            VAD_TEND_V(I,J,K)=VN(K)-V_K(K)
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
!         PVVLOU=(PETDT(I+IVW(J),J,KTE-1)                               &
!    &           +PETDT(I+IVE(J),J,KTE-1))*DTE
!         PVVLOV=(PETDT(I,J-1,KTE-1)                                    &
!    &           +PETDT(I,J+1,KTE-1))*DTE
!         VVLOU=PVVLOU/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOPU)
!         VVLOV=PVVLOV/(DETA1_PDTOP(KTE)+DETA2(KTE)*PDOPV)
!         TULO=VVLOU*(U(I,J,KTE-1)-U(I,J,KTE)+UN(KTE-1)-UN(KTE))
!         TVLO=VVLOV*(V(I,J,KTE-1)-V(I,J,KTE)+VN(KTE-1)-VN(KTE))
!         ADUP=TULO+UN(KTE)-U(I,J,KTE)
!         ADVP=TVLO+VN(KTE)-V(I,J,KTE)
!         WRITE(0,*)' NTSD=',NTSD,' I=',I,' J=',J,' K=',KTE             &
!    &,             ' ADUP=',ADUP,' ADVP=',ADVP
!         WRITE(0,*)' U=',U(I,J,KTE),' UN=',UN(KTE)                     &
!    &,             ' VAD_TEND_U=',VAD_TEND_U(I,KTE)                    &
!    &,             ' V=',V(I,J,KTE),' VN=',VN(KTE)                     &
!    &,             ' VAD_TEND_V=',VAD_TEND_V(I,KTE)
!         WRITE(0,*)' '
!!
!         DO K=KTE-1,KTS+1,-1
!           RDPU=1./(DETA1_PDTOP(K)+DETA2(K)*PDOPU)
!           RDPV=1./(DETA1_PDTOP(K)+DETA2(K)*PDOPV)
!           PVVUPU=PVVLOU
!           PVVUPV=PVVLOV
!           PVVLOU=(PETDT(I+IVW(J),J,K-1)                               &
!    &            +PETDT(I+IVE(J),J,K-1))*DTE
!           PVVLOV=(PETDT(I,J-1,K-1)+PETDT(I,J+1,K-1))*DTE
!           VVUPU=PVVUPU*RDPU
!           VVUPV=PVVUPV*RDPV
!           VVLOU=PVVLOU*RDPU
!           VVLOV=PVVLOV*RDPV
!           TUUP=VVUPU*(U(I,J,K)-U(I,J,K+1)+UN(K)-UN(K+1))
!           TVUP=VVUPV*(V(I,J,K)-V(I,J,K+1)+VN(K)-VN(K+1))
!           TULO=VVLOU*(U(I,J,K-1)-U(I,J,K)+UN(K-1)-UN(K))
!           TVLO=VVLOV*(V(I,J,K-1)-V(I,J,K)+VN(K-1)-VN(K))
!           ADUP=TUUP+TULO+UN(K)-U(I,J,K)
!           ADVP=TVUP+TVLO+VN(K)-V(I,J,K)
!           WRITE(0,*)' NTSD=',NTSD,' I=',ITEST,' J=',JTEST,' K=',K     &
!    &,               ' ADUP=',ADUP,' ADVP=',ADVP
!           WRITE(0,*)' U=',U(I,J,K),' UN=',UN(K)                       &
!    &,               ' VAD_TEND_U=',VAD_TEND_U(I,K)                    &
!    &,               ' V=',V(I,J,K),' VN=',VN(K)                       &
!    &,               ' VAD_TEND_V=',VAD_TEND_V(I,K)
!           WRITE(0,*)' '
!         ENDDO
!!
!         PVVUPU=PVVLOU
!         PVVUPV=PVVLOV
!         VVUPU=PVVUPU/(DETA1_PDTOP(KTS)+DETA2(KTS)*PDOPU)
!         VVUPV=PVVUPV/(DETA1_PDTOP(KTS)+DETA2(KTS)*PDOPV)
!         TUUP=VVUPU*(U(I,J,KTS)-U(I,J,KTS+1)+UN(KTS)-UN(KTS+1))
!         TVUP=VVUPV*(V(I,J,KTS)-V(I,J,KTS+1)+VN(KTS)-VN(KTS+1))
!         ADUP=TUUP+UN(KTS)-U(I,J,KTS)
!         ADVP=TVUP+VN(KTS)-V(I,J,KTS)
!         WRITE(0,*)' NTSD=',NTSD,' I=',ITEST,' J=',JTEST,' K=',KTS     &
!    &,             ' ADUP=',ADUP,' ADVP=',ADVP
!         WRITE(0,*)' U=',U(I,J,KTS),' UN=',UN(KTS)                     &
!    &,             ' VAD_TEND_U=',VAD_TEND_U(I,KTS)                    &
!    &,             ' V=',V(I,J,KTS),' VN=',VN(KTS)                     &
!    &,             ' VAD_TEND_V=',VAD_TEND_V(I,KTS)
!         WRITE(0,*)' '
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
      ENDDO main_vertical
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!***  COMPUTE HORIZONTAL ADVECTION TENDENCIES.
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(adpdx,adpdy,adt,adu,adv,array0,array1,array2,array3      &
!$omp&        ,array3_x,dpde,f0,f1,f2,f3,fewp,fnep,fnsp,fpp,fsep,hm     &
!$omp&        ,i,ifp,ifq,ii,ipq,isp,ispa,isq,isqa,iup_adh_j,j,k         &
!$omp&        ,knti_adh,n_iupadh_j,n_iupadv_j,n_iuph_j,pp,qp            &
!$omp&        ,rdpd,rdpdx,rdpdy,tew,tne,tns,tse,tst,tta,ttb             &
!$omp&        ,uew,udy,une,uned,uns,use,used,ust                        &
!$omp&        ,vdx,vew,vm,vne,vns,vse,vst)
!-----------------------------------------------------------------------
!
      main_horizontal: DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!
        DO J=MYJS_P4,MYJE_P4
        DO I=MYIS_P4,MYIE_P4
          DPDE(I,J)=DETA1_PDTOP(K)+DETA2(K)*PDSLO(I,J)
          RDPD(I,J)=1./DPDE(I,J)
          TST(I,J)=T(I,J,K)*FFC+TOLD(I,J,K)*FBC
          UST(I,J)=U(I,J,K)*FFC+UOLD(I,J,K)*FBC
          VST(I,J)=V(I,J,K)*FFC+VOLD(I,J,K)*FBC
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  MASS FLUXES AND MASS POINT ADVECTION COMPONENTS
!***  THE NS AND EW FLUXES IN THE FOLLOWING LOOP ARE ON V POINTS
!***  FOR T.
!-----------------------------------------------------------------------
!
        DO J=MYJS1_P3,MYJE1_P3
        DO I=MYIS_P3,MYIE_P3
!
          ADPDX=DPDE(I+IVW(J),J)+DPDE(I+IVE(J),J)
          ADPDY=DPDE(I,J-1)+DPDE(I,J+1)
          RDPDX(I,J)=1./ADPDX
          RDPDY(I,J)=1./ADPDY
!
          UDY=U(I,J,K)*DY
          VDX=V(I,J,K)*DX(I,J)
!
          FEWP=UDY*ADPDX
          FNSP=VDX*ADPDY
!
          FEW(I,J,K)=FEWP
          FNS(I,J,K)=FNSP
!
          TEW(I,J)=FEWP*(TST(I+IVE(J),J)-TST(I+IVW(J),J))
          TNS(I,J)=FNSP*(TST(I,J+1)-TST(I,J-1))
!
          UNED(I,J)=UDY+VDX
          USED(I,J)=UDY-VDX
!
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  DIAGONAL FLUXES AND DIAGONALLY AVERAGED WIND
!***  THE NE AND SE FLUXES ARE ASSOCIATED WITH H POINTS
!***  (ACTUALLY JUST TO THE NE AND SE OF EACH H POINT).
!-----------------------------------------------------------------------
!
        DO J=MYJS1_P2,MYJE2_P2
        DO I=MYIS_P2,MYIE_P2
          FNEP=(UNED(I+IHE(J),J)+UNED(I       ,J+1))                    &
     &        *(DPDE(I       ,J)+DPDE(I+IHE(J),J+1))
          FNE(I,J,K)=FNEP
          TNE(I,J)=FNEP*(TST(I+IHE(J),J+1)-TST(I,J))
        ENDDO
        ENDDO
!
        DO J=MYJS2_P2,MYJE1_P2
        DO I=MYIS_P2,MYIE_P2
          FSEP=(USED(I+IHE(J),J)+USED(I       ,J-1))                    &
     &        *(DPDE(I       ,J)+DPDE(I+IHE(J),J-1))
          FSE(I,J,K)=FSEP
          TSE(I,J)=FSEP*(TST(I+IHE(J),J-1)-TST(I,J))
!
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  HORIZONTAL T ADVECTION TENDENCY ADT IS ON H POINTS OF COURSE.
!-----------------------------------------------------------------------
!
        DO J=MYJS5,MYJE5
        DO I=MYIS2,MYIE2
          ADT(I,J)=(TEW(I+IHW(J),J)+TEW(I+IHE(J),J)                     &
     &             +TNS(I,J-1)+TNS(I,J+1)                               &
     &             +TNE(I+IHW(J),J-1)+TNE(I,J)                          &
     &             +TSE(I,J)+TSE(I+IHW(J),J+1))                         &
     &             *RDPD(I,J)*FAD(I,J)
        ENDDO
        ENDDO
!
!
!-----------------------------------------------------------------------
!***  CALCULATION OF MOMENTUM ADVECTION COMPONENTS.
!-----------------------------------------------------------------------
!
        DO J=MYJS4_P1,MYJE4_P1
        DO I=MYIS_P1,MYIE_P1
!
!-----------------------------------------------------------------------
!***  THE NS AND EW FLUXES ARE ON H POINTS FOR U AND V.
!-----------------------------------------------------------------------
!
          UEW(I,J)=(FEW(I+IHW(J),J,K)+FEW(I+IHE(J),J,K))                &
     &            *(UST(I+IHE(J),J)-UST(I+IHW(J),J))
          UNS(I,J)=(FNS(I+IHW(J),J,K)+FNS(I+IHE(J),J,K))                &
     &            *(UST(I,J+1)-UST(I,J-1))
          VEW(I,J)=(FEW(I,J-1,K)+FEW(I,J+1,K))                          &
     &            *(VST(I+IHE(J),J)-VST(I+IHW(J),J))
          VNS(I,J)=(FNS(I,J-1,K)+FNS(I,J+1,K))                          &
     &            *(VST(I,J+1)-VST(I,J-1))
!
!-----------------------------------------------------------------------
!***  THE FOLLOWING NE AND SE FLUXES ARE TIED TO V POINTS AND ARE
!***  LOCATED JUST TO THE NE AND SE OF THE GIVEN I,J.
!-----------------------------------------------------------------------
!
          UNE(I,J)=(FNE(I+IVW(J),J,K)+FNE(I+IVE(J),J,K))                &
     &            *(UST(I+IVE(J),J+1)-UST(I,J))
          USE(I,J)=(FSE(I+IVW(J),J,K)+FSE(I+IVE(J),J,K))                &
     &            *(UST(I+IVE(J),J-1)-UST(I,J))
          VNE(I,J)=(FNE(I,J-1,K)+FNE(I,J+1,K))                          &
     &            *(VST(I+IVE(J),J+1)-VST(I,J))
          VSE(I,J)=(FSE(I,J-1,K)+FSE(I,J+1,K))                          &
     &            *(VST(I+IVE(J),J-1)-VST(I,J))
!
!-----------------------------------------------------------------------
!
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  COMPUTE THE ADVECTION TENDENCIES FOR U AND V.
!***  THE AD ARRAYS ARE ON THE VELOCITY POINTS.
!-----------------------------------------------------------------------
!
        DO J=MYJS5,MYJE5
        DO I=MYIS2,MYIE2
          ADU(I,J)=(UEW(I+IVW(J),J)+UEW(I+IVE(J),J)                     &
     &             +UNS(I,J-1)+UNS(I,J+1)                               &
     &             +UNE(I+IVW(J),J-1)+UNE(I,J)                          &
     &             +USE(I,J)+USE(I+IVW(J),J+1))                         &
     &             *RDPDX(I,J)*FAD(I+IVW(J),J)
!
          ADV(I,J)=(VEW(I+IVW(J),J)+VEW(I+IVE(J),J)                     &
     &             +VNS(I,J-1)+VNS(I,J+1)                               &
     &             +VNE(I+IVW(J),J-1)+VNE(I,J)                          &
     &             +VSE(I,J)+VSE(I+IVW(J),J+1))                         &
     &             *RDPDY(I,J)*FAD(I+IVW(J),J)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
!***  END OF JANJIC HORIZONTAL ADVECTION
!
!-----------------------------------------------------------------------
!
!***  UPSTREAM ADVECTION OF T
!
!-----------------------------------------------------------------------
!
        upstream: IF(UPSTRM)THEN
!
!-----------------------------------------------------------------------
!***
!***  COMPUTE UPSTREAM COMPUTATIONS ON THIS TASK'S ROWS.
!***
!-----------------------------------------------------------------------
!
          jloop_upstream: DO J=MYJS2,MYJE2
!
            N_IUPH_J=N_IUP_H(J)   ! See explanation in START_DOMAIN_NMM
            DO II=0,N_IUPH_J-1
!
              I=IUP_H(IMS+II,J)
              TTA=EMT_LOC(J)*(UST(I,J-1)+UST(I+IHW(J),J)                &
     &                       +UST(I+IHE(J),J)+UST(I,J+1))
              TTB=ENT       *(VST(I,J-1)+VST(I+IHW(J),J)                &
     &                       +VST(I+IHE(J),J)+VST(I,J+1))
              PP=-TTA-TTB
              QP= TTA-TTB
!
              IF(PP<0.)THEN
                ISPA(I,J)=-1
              ELSE
                ISPA(I,J)= 1
              ENDIF
!
              IF(QP<0.)THEN
                ISQA(I,J)=-1
              ELSE
                ISQA(I,J)= 1
              ENDIF
!
              PP=ABS(PP)
              QP=ABS(QP)
              ARRAY3_X=PP*QP
              ARRAY0(I,J)=ARRAY3_X-PP-QP
              ARRAY1(I,J)=PP-ARRAY3_X
              ARRAY2(I,J)=QP-ARRAY3_X
              ARRAY3(I,J)=ARRAY3_X
            ENDDO
!
!-----------------------------------------------------------------------
!
            N_IUPADH_J=N_IUP_ADH(J)
            KNTI_ADH=1
            IUP_ADH_J=IUP_ADH(IMS,J)
!
            iloop_T: DO II=0,N_IUPH_J-1
!
              I=IUP_H(IMS+II,J)
!
              ISP=ISPA(I,J)
              ISQ=ISQA(I,J)
              IFP=(ISP-1)/2
              IFQ=(-ISQ-1)/2
              IPQ=(ISP-ISQ)/2
!
!-----------------------------------------------------------------------
!
              IF(I==IUP_ADH_J)THEN  ! Upstream advection T tendencies
!
                ISP=ISPA(I,J)
                ISQ=ISQA(I,J)
                IFP=(ISP-1)/2
                IFQ=(-ISQ-1)/2
                IPQ=(ISP-ISQ)/2
!
                F0=ARRAY0(I,J)
                F1=ARRAY1(I,J)
                F2=ARRAY2(I,J)
                F3=ARRAY3(I,J)
!
                ADT(I,J)=F0*T(I,J,K)                                    &
     &                  +F1*T(I+IHE(J)+IFP,J+ISP,K)                     &
     &                  +F2*T(I+IHE(J)+IFQ,J+ISQ,K)                     &
                        +F3*T(I+IPQ,J+ISP+ISQ,K)
!
!-----------------------------------------------------------------------
!
                IF(KNTI_ADH<N_IUPADH_J)THEN
                  IUP_ADH_J=IUP_ADH(IMS+KNTI_ADH,J)
                  KNTI_ADH=KNTI_ADH+1
                ENDIF
!
              ENDIF  ! End of upstream advection T tendency IF block
!
            ENDDO iloop_T
!
!-----------------------------------------------------------------------
!
!***  UPSTREAM ADVECTION OF VELOCITY COMPONENTS
!
!-----------------------------------------------------------------------
!
            N_IUPADV_J=N_IUP_ADV(J)
!
            DO II=0,N_IUPADV_J-1
              I=IUP_ADV(IMS+II,J)
!
              TTA=EM_LOC(J)*UST(I,J)
              TTB=EN       *VST(I,J)
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
              ADU(I,J)=F0*U(I,J,K)                                      &
     &                +F1*U(I+IVE(J)+IFP,J+ISP,K)                       &
     &                +F2*U(I+IVE(J)+IFQ,J+ISQ,K)                       &
     &                +F3*U(I+IPQ,J+ISP+ISQ,K)
!
              ADV(I,J)=F0*V(I,J,K)                                      &
     &                +F1*V(I+IVE(J)+IFP,J+ISP,K)                       &
     &                +F2*V(I+IVE(J)+IFQ,J+ISQ,K)                       &
     &                +F3*V(I+IPQ,J+ISP+ISQ,K)
!
            ENDDO
!
          ENDDO jloop_upstream
!
!-----------------------------------------------------------------------
!
        ENDIF upstream
!
!-----------------------------------------------------------------------
!
!***  END OF HORIZONTAL ADVECTION
!
!-----------------------------------------------------------------------
!
!***  NOW SUM THE VERTICAL AND HORIZONTAL TENDENCIES,
!***  CURVATURE AND CORIOLIS TERMS.
!
!-----------------------------------------------------------------------
!
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          HM=HBM2(I,J)
          VM=VBM2(I,J)
          ADT(I,J)=(VAD_TEND_T(I,J,K)+2.*ADT(I,J))*HM
!
          FPP=CURV(I,J)*2.*UST(I,J)+F(I,J)*2.
          ADU(I,J)=(VAD_TEND_U(I,J,K)+2.*ADU(I,J)+VST(I,J)*FPP)*VM
          ADV(I,J)=(VAD_TEND_V(I,J,K)+2.*ADV(I,J)-UST(I,J)*FPP)*VM
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  SAVE THE OLD VALUES FOR TIMESTEPPING
!-----------------------------------------------------------------------
!
        DO J=MYJS_P4,MYJE_P4
        DO I=MYIS_P4,MYIE_P4
          TOLD(I,J,K)=T(I,J,K)
          UOLD(I,J,K)=U(I,J,K)
          VOLD(I,J,K)=V(I,J,K)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!***  FINALLY UPDATE THE PROGNOSTIC VARIABLES
!-----------------------------------------------------------------------
!
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          T(I,J,K)=ADT(I,J)+T(I,J,K)
          U(I,J,K)=ADU(I,J)+U(I,J,K)
          V(I,J,K)=ADV(I,J)+V(I,J,K)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO main_horizontal
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE ADVE
!
!-----------------------------------------------------------------------
!
!***********************************************************************
      SUBROUTINE VAD2(NTSD,DT,IDTAD,DX,DY                               &
     &               ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP,HBM2           &
     &               ,Q,Q2,CWM,PETDT                                    &
     &               ,N_IUP_H,N_IUP_V                                   &
     &               ,N_IUP_ADH,N_IUP_ADV                               &
     &               ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                       &
     &               ,IHE,IHW,IVE,IVW                                   &
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
!   05-12-14  BLACK    - CONVERTED FROM IKJ TO IJK
!   07-08-14  janjic   - bc & no conservation in the advection step
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
!
      INTEGER,INTENT(IN) :: IDTAD,NTSD
!
      REAL,INTENT(IN) :: DT,DY,PDTOP
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX,HBM2,PDSL
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PETDT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CWM,Q,Q2
!
!***  LOCAL VARIABLES
!----------------------------------------------------------------------
!
      REAL,PARAMETER :: FF1=0.500
!
      LOGICAL,SAVE :: TRADITIONAL=.TRUE.
!
      INTEGER :: I,IRECV,J,JFP,JFQ,K,LAP,LLAP
!
      INTEGER,DIMENSION(KTS:KTE) :: LA
!
      REAL*8 :: ADDT,AFRP,D2PQE,D2PQQ,D2PQW,DEP,DETAP,DQP               &
     &       ,DWP,E00,E4P,EP,EP0,HADDT,HBM2IJ                           &
     &       ,Q00,Q4P,QP,QP0                                            &
     &       ,rdpdn,rdpup,sfacek,sfacqk,sfacwk,RFC,RR                   &
     &       ,SUMNE,SUMNQ,SUMNW,SUMPE,SUMPQ,SUMPW                       &
     &       ,W00,W4P,WP,WP0
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,DQL,DWL,E3,E4,PETDTK           &
     &                          ,RFACE,RFACQ,RFACW,Q3,Q4,W3,W4
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ADDT=REAL(IDTAD)*DT
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(afr,afrp,bot,d2pqe,d2pqq,d2pqw,del,dep,detap,dpdn,dpup   &
!$omp&        ,dql,dqp,dwl,dwp,e00,e3,e4,e4p,ep,ep0,haddt,i,j,k         &
!$omp&        ,la,lap,llap,petdtk,q00,q3,q4,q4p,qp,qp0,rfacek,rfacqk    &
!$omp&        ,rfacwk,rfc,rr,sumne,sumnq,sumnw,sumpe,sumpq,sumpw,top    &
!$omp&        ,w00,w3,w4,w4p,wp,wp0)
!-----------------------------------------------------------------------
!
      main_integration: DO J=MYJS2,MYJE2
!
!-----------------------------------------------------------------------
!
        main_iloop: DO I=MYIS1_P1,MYIE1_P1
!
!-----------------------------------------------------------------------
!
          E3(KTE)=Q2(I,J,KTE)*0.5
!
          DO K=KTE-1,KTS,-1
            E3(K)=MAX((Q2(I,J,K+1)+Q2(I,J,K))*0.5,EPSQ2)
          ENDDO
!
          DO K=KTS,KTE
            Q3(K)=MAX(Q(I,J,K),EPSQ)
            W3(K)=MAX(CWM(I,J,K),CLIMIT)
            E4(K)=E3(K)
            Q4(K)=Q3(K)
            W4(K)=W3(K)
          ENDDO
!
          IF(TRADITIONAL)THEN
            PETDTK(KTE)=PETDT(I,J,KTE-1)*0.5
!
            DO K=KTE-1,KTS+1,-1
              PETDTK(K)=(PETDT(I,J,K)+PETDT(I,J,K-1))*0.5
            ENDDO
!
            PETDTK(KTS)=PETDT(I,J,KTS)*0.5
!
          ELSE
!
!-----------------------------------------------------------------------
!***    PERFORM HORIZONTAL AVERAGING OF VERTICAL VELOCITY
!-----------------------------------------------------------------------
!
            PETDTK(KTE)=(PETDT(I+IHW(J-1),J-1,KTE-1)                    &
     &                  +PETDT(I+IHE(J-1),J-1,KTE-1)                    &
     &                  +PETDT(I+IHW(J+1),J+1,KTE-1)                    &
     &                  +PETDT(I+IHE(J+1),J+1,KTE-1)                    &
     &                  +PETDT(I,J,KTE-1)*4.        )*0.0625
!
            DO K=KTE-1,KTS+1,-1
              PETDTK(K)=(PETDT(I+IHW(J-1),J-1,K-1)                      &
                        +PETDT(I+IHE(J-1),J-1,K-1)                      &
     &                  +PETDT(I+IHW(J+1),J+1,K-1)                      &
     &                  +PETDT(I+IHE(J+1),J+1,K-1)                      &
     &                  +PETDT(I+IHW(J-1),J-1,K  )                      &
     &                  +PETDT(I+IHE(J-1),J-1,K  )                      &
     &                  +PETDT(I+IHW(J+1),J+1,K  )                      &
     &                  +PETDT(I+IHE(J+1),J+1,K  )                      &
     &                  +(PETDT(I,J,K-1)+PETDT(I,J,K))*4.               &
     &                                                   )*0.0625
            ENDDO
!
            PETDTK(KTS)=(PETDT(I+IHW(J-1),J-1,KTS)                      &
     &                  +PETDT(I+IHE(J-1),J-1,KTS)                      &
     &                  +PETDT(I+IHW(J+1),J+1,KTS)                      &
     &                  +PETDT(I+IHE(J+1),J+1,KTS)                      &
     &                  +PETDT(I,J,KTS)*4.        )*0.0625

          ENDIF
!
!-----------------------------------------------------------------------
!
          HADDT=-ADDT*HBM2(I,J)
!
          DO K=KTE,KTS,-1
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
            if(llap.gt.kts-1.and.llap.lt.kte+1) then ! internal and outflow pts.
              rr=abs(rr &
     &          /((aeta1(llap)-aeta1(k))*pdtop &
     &           +(aeta2(llap)-aeta2(k))*pdsl(i,j)))
              if(rr.gt.0.999) rr=0.999   
!
              AFR(K)=(((FF4*RR+FF3)*RR+FF2)*RR+FF1)*RR
              dql(k)=(q3(llap)-q3(k))*rr
              dwl(k)=(w3(llap)-w3(k))*rr
              del(k)=(e3(llap)-e3(k))*rr
            elseif(llap.eq.kts-1) then
!
!chem              rr=abs(rr &
!chem                /((1.-aeta2(kts))*pdsl(i,j)))
!chem              afr(kts)=0.
!chem              dql(kts)=(epsq  -q3(kts))*rr
!chem              dwl(kts)=(climit-w3(kts))*rr
!chem              del(kts)=(epsq2 -e3(kts))*rr
!
              rr=0.
              afr(kts)=0.
              dql(kts)=0.
              dwl(kts)=0.
              del(kts)=0.
            else
              rr=abs(rr &
                /(aeta1(kte)*pdtop))
              afr(kte)=0.
              dql(kte)=(epsq  -q3(kte))*rr
              dwl(kte)=(climit-w3(kte))*rr
              del(kte)=(epsq2 -e3(kte))*rr
            endif
          ENDDO
!
!-----------------------------------------------------------------------
!
          DO K=KTS,KTE
            Q4(K)=Q3(K)+DQL(K)
            W4(K)=W3(K)+DWL(K)
            E4(K)=E3(K)+DEL(K)
          ENDDO
!
!-----------------------------------------------------------------------
!***  ANTI-FILTERING STEP
!-----------------------------------------------------------------------
!
          SUMPQ=0.
          SUMNQ=0.
          SUMPW=0.
          SUMNW=0.
          SUMPE=0.
          SUMNE=0.
!
!***  ANTI-FILTERING LIMITERS
!
          antifilter: DO K=KTE-1,KTS+1,-1
!
            DETAP=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
!
            DQL(K)=0.
            DWL(K)=0.
            DEL(K)=0.
!
            Q4P=Q4(K)
            W4P=W4(K)
            E4P=E4(K)
!
            LAP=LA(K)
!
            if(lap.ne.0)then
              rdpdn=1./((aeta1(k+lap)-aeta1(k))*pdtop &
     &                 +(aeta2(k+lap)-aeta2(k))*pdsl(i,j))
              rdpup=1./((aeta1(k)-aeta1(k-lap))*pdtop &
     &                 +(aeta2(k)-aeta2(k-lap))*pdsl(i,j))
!
              afrp=afr(k)*detap
!
              d2pqq=((q4(k+lap)-q4p)*rdpdn &
     &              -(q4p-q4(k-lap))*rdpup)*afrp
              d2pqw=((w4(k+lap)-w4p)*rdpdn &
     &              -(w4p-w4(k-lap))*rdpup)*afrp
              d2pqe=((e4(k+lap)-e4p)*rdpdn &
     &              -(e4p-e4(k-lap))*rdpup)*afrp
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
            dqp=qp-q4p
            dwp=wp-w4p
            dep=ep-e4p
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
          ENDDO antifilter
!
!-----------------------------------------------------------------------
!
          DQL(KTS)=0.
          DWL(KTS)=0.
          DEL(KTS)=0.
!
          DQL(KTE)=0.
          DWL(KTE)=0.
          DEL(KTE)=0.
!
!-----------------------------------------------------------------------
!***  FIRST MOMENT CONSERVING FACTOR
!-----------------------------------------------------------------------
!
          if(sumpq*(-sumnq).gt.1.e-9) then
            sfacqk=-sumnq/sumpq
          else
            sfacqk=0.
          endif
!
          if(sumpw*(-sumnw).gt.1.e-9) then
            sfacwk=-sumnw/sumpw
          else
            sfacwk=0.
          endif
!
          if(sumpe*(-sumne).gt.1.e-9) then
            sfacek=-sumne/sumpe
          else
            sfacek=0.
          endif
!
!-----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!-----------------------------------------------------------------------
!
          DO K=KTE,KTS,-1
!
            dqp=dql(k)
            if(sfacqk.gt.0.) then
              if(sfacqk.ge.1.) then
                if(dqp.lt.0.) dqp=dqp/sfacqk
              else
                if(dqp.gt.0.) dqp=dqp*sfacqk
              endif
            else
              dqp=0.
            endif
            q  (i,j,k)=q4(k)+dqp
!
            dwp=dwl(k)
            if(sfacwk.gt.0.) then
              if(sfacwk.ge.1.) then
                if(dwp.lt.0.) dwp=dwp/sfacwk
              else
                if(dwp.gt.0.) dwp=dwp*sfacwk
              endif
            else
              dwp=0.
            endif
            cwm(i,j,k)=w4(k)+dwp
!
            dep=del(k)
            if(sfacek.gt.0.) then
              if(sfacek.ge.1.) then
                if(dep.lt.0.) dep=dep/sfacek
              else
                if(dep.gt.0.) dep=dep*sfacek
              endif
            else
              dep=0.
            endif
            e3 (    k)=e4(k)+dep
!
          ENDDO
!-----------------------------------------------------------------------
          HBM2IJ=HBM2(I,J)
          Q2(I,J,KTE)=MAX(E3(KTE)+E3(KTE)-EPSQ2,EPSQ2)*HBM2IJ           &
       &             +Q2(I,J,KTE)*(1.-HBM2IJ)
          DO K=KTE-1,KTS+1,-1
            Q2(I,J,K)=MAX(E3(K)+E3(K)-Q2(I,J,K+1),EPSQ2)*HBM2IJ         &
       &             +Q2(I,J,K)*(1.-HBM2IJ)
          ENDDO
!-----------------------------------------------------------------------
!
        ENDDO main_iloop
!
!-----------------------------------------------------------------------
!
      ENDDO main_integration
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE VAD2
!

!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***********************************************************************
      SUBROUTINE HAD2(                                                  &
#if defined(DM_PARALLEL)
     &                domdesc ,                                         &
#endif
     &                NTSD,DT,IDTAD,DX,DY                               &
     &               ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                &
     &               ,HBM2,HBM3                                         &
     &               ,Q,Q2,CWM,U,V,Z,HYDRO                              &
     &               ,N_IUP_H,N_IUP_V                                   &
     &               ,N_IUP_ADH,N_IUP_ADV                               &
     &               ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                       &
     &               ,IHE,IHW,IVE,IVW                                   &
     &               ,advect_Q2                                         &
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
!   05-12-14  BLACK    - CONVERTED FROM IKJ TO IJK
!   07-08-14  janjic   - no conservation in advection step
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
      LOGICAL, INTENT(IN) :: advect_Q2
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V          &
     &                                        ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V      &
     &                                                ,IUP_ADH,IUP_ADV
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDTAD,NTSD
!
      REAL,INTENT(IN) :: DT,DY,PDTOP
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX,HBM2,HBM3,PDSL
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: U,V,Z
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(INOUT) :: CWM,Q,Q2
!
      LOGICAL,INTENT(IN) :: HYDRO
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: FF1=0.530
!
#ifdef DM_PARALLEL
      INTEGER :: DOMDESC
#endif
!
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL) && !defined(STUBMPI)
      LOGICAL,EXTERNAL :: WRF_DM_ON_MONITOR
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,6) :: XSUMS_L
      REAL,DIMENSION(IDS:IDE,JDS:JDE,KDS:KDE,6) :: XSUMS_G
#endif
!
      LOGICAL :: BOT,TOP
!
      INTEGER :: I,IRECV,J,JFP,JFQ,K,LAP,LLAP,MPI_COMM_COMP
      INTEGER :: N
!
      INTEGER,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5,KTS:KTE) :: IFPA,IFPF   &
     &                                                     ,IFQA,IFQF   &
     &                                                     ,JFPA,JFPF   &
     &                                                     ,JFQA,JFQF
!
      REAL :: ADDT,AFRP,CRIT,D2PQE,D2PQQ,D2PQW,DEP,DESTIJ,DQP,DQSTIJ    &
     &       ,DVOLP,DWP,DWSTIJ,DZA,DZB,E00,E0Q,E1X,E2IJ,E4P,ENH,EP,EP0  &
     &       ,ESTIJ,FPQ,HAFP,HAFQ,HBM2IJ,HM,PP,PPQ00,Q00,Q0Q            &
     &       ,Q1IJ,Q4P,QP,QP0,QSTIJ,RDY,RFACE,RFACQ,RFACW,RFC           &
     &       ,RFEIJ,RFQIJ,RFWIJ,RR,SLOPAC,SPP,SQP,SSA,SSB,SUMNE,SUMNQ   &
     &       ,SUMNW,SUMPE,SUMPQ,SUMPW,TTA,TTB,W00,W0Q,W1IJ,W4P,WP,WP0   &
     &       ,WSTIJ
!
      DOUBLE PRECISION,DIMENSION(6,KTS:KTE) :: GSUMS,XSUMS
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,DQL,DWL,E3,E4                  &
     &                          ,Q3,Q4,W3,W4
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: DARE,EMH
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5,KTS:KTE) :: AFP,AFQ,DEST   &
     &                                                  ,DQST,DVOL,DWST &
     &                                                  ,E1,E2,Q1,W1
!-----------------------------------------------------------------------
      integer :: nunit,ier
      save nunit
!-----------------------------------------------------------------------
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
      ENDDO
      ENDDO
!$omp parallel do                                                       &
!$omp& private(i,j)
      if(advect_Q2) then
        DO J=MYJS_P3,MYJE_P3
        DO I=MYIS_P2,MYIE_P2
          E1(I,J,KTE)=MAX(Q2(I,J,KTE)*0.5,EPSQ2)
          E2(I,J,KTE)=E1(I,J,KTE)
        ENDDO
        ENDDO
      endif
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dza,dzb,e1x,fpq,hm,i,j,jfp,jfq,k,pp,qp,ssa,ssb,spp,sqp   &
!$omp&        ,tta,ttb)
!-----------------------------------------------------------------------
!
      vertical_1: DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!
        DO J=MYJS_P3,MYJE_P3
        DO I=MYIS_P2,MYIE_P2
          DVOL(I,J,K)=DARE(I,J)*(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))
          Q  (I,J,K)=MAX(Q  (I,J,K),EPSQ)
          CWM(I,J,K)=MAX(CWM(I,J,K),CLIMIT)
          Q1 (I,J,K)=Q  (I,J,K)
          W1 (I,J,K)=CWM(I,J,K)
        ENDDO
        ENDDO
!
        if(advect_Q2) then
          IF(K<KTE)THEN
            DO J=MYJS_P3,MYJE_P3
            DO I=MYIS_P2,MYIE_P2
              E1X=(Q2(I,J,K+1)+Q2(I,J,K))*0.5
              E1(I,J,K)=MAX(E1X,EPSQ2)
              E2(I,J,K)=E1(I,J,K)
            ENDDO
            ENDDO
          ENDIF
        endif
!
!-----------------------------------------------------------------------
!
        DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
!
          HM=HBM2(I,J)
          TTA=(U(I,J-1,K)+U(I+IHW(J),J,K)+U(I+IHE(J),J,K)+U(I,J+1,K))   &
     &        *EMH(I,J)*HM
          TTB=(V(I,J-1,K)+V(I+IHW(J),J,K)+V(I+IHE(J),J,K)+V(I,J+1,K))   &
     &        *ENH*HBM2(I,J)
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
          IFPA(I,J,K)=IHE(J)+I+( JFP-1)/2
          IFQA(I,J,K)=IHE(J)+I+(-JFQ-1)/2
!
          JFPA(I,J,K)=J+JFP
          JFQA(I,J,K)=J+JFQ
!
          IFPF(I,J,K)=IHE(J)+I+(-JFP-1)/2
          IFQF(I,J,K)=IHE(J)+I+( JFQ-1)/2
!
          JFPF(I,J,K)=J-JFP
          JFQF(I,J,K)=J-JFQ
!     if(i==111.and.j==438.and.k==1)then
!     endif
!
!-----------------------------------------------------------------------
          IF(.NOT.HYDRO)THEN ! z currently not available for hydro=.true.
            DZA=(Z(IFPA(I,J,K),JFPA(I,J,K),K)-Z(I,J,K))*RDY
            DZB=(Z(IFQA(I,J,K),JFQA(I,J,K),K)-Z(I,J,K))*RDY
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
!
!-----------------------------------------------------------------------
!
          FPQ=SPP*SQP*0.25
          PP=ABS(SPP)
          QP=ABS(SQP)
!
          AFP(I,J,K)=(((FF4*PP+FF3)*PP+FF2)*PP+FF1)*PP
          AFQ(I,J,K)=(((FF4*QP+FF3)*QP+FF2)*QP+FF1)*QP
!
          Q1(I,J,K)=(Q  (IFPA(I,J,K),JFPA(I,J,K),K)-Q  (I,J,K))*PP        &
       &           +(Q  (IFQA(I,J,K),JFQA(I,J,K),K)-Q  (I,J,K))*QP        &
       &           +(Q  (I,J-2,K)+Q  (I,J+2,K)                            &
       &            -Q  (I-1,J,K)-Q  (I+1,J,K))*FPQ                       &
       &           +Q(I,J,K)
!
          W1(I,J,K)=(CWM(IFPA(I,J,K),JFPA(I,J,K),K)-CWM(I,J,K))*PP        &
       &           +(CWM(IFQA(I,J,K),JFQA(I,J,K),K)-CWM(I,J,K))*QP        &
       &           +(CWM(I,J-2,K)+CWM(I,J+2,K)                            &
       &            -CWM(I-1,J,K)-CWM(I+1,J,K))*FPQ                       &
       &           +CWM(I,J,K)
!
         if(advect_Q2) then
          E2(I,J,K)=(E1 (IFPA(I,J,K),JFPA(I,J,K),K)-E1 (I,J,K))*PP        &
       &           +(E1 (IFQA(I,J,K),JFQA(I,J,K),K)-E1 (I,J,K))*QP        &
       &           +(E1 (I,J-2,K)+E1 (I,J+2,K)                            &
       &            -E1 (I-1,J,K)-E1 (I+1,J,K))*FPQ                       &
       &           +E1(I,J,K)
         endif
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO vertical_1
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
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL) && !defined(STUBMPI)
      DO N=1,6
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME 
        DO J=JMS,JME 
        DO I=IMS,IME 
          XSUMS_L(I,J,K,N)=0.
        ENDDO
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KDS,KDE 
        DO J=JDS,JDE 
        DO I=IDS,IDE 
          XSUMS_G(I,J,K,N)=0.
        ENDDO
        ENDDO
        ENDDO
!
      ENDDO
!
#endif
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(d2pqe,d2pqq,d2pqw,destij,dqstij,dvolp,dwstij             &
!$omp&        ,e00,e0q,e2ij,ep0,estij,hafp,hafq,i,j,k                   &
!$omp&        ,q00,q0q,q1ij,qp0,qstij,w00,w0q,w1ij,wp0,wstij)
!-----------------------------------------------------------------------
!
      vertical_2: DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
!
          DVOLP=DVOL(I,J,K)
          Q1IJ =Q1(I,J,K)
          W1IJ =W1(I,J,K)
          E2IJ =E2(I,J,K)
!
          HAFP=AFP(I,J,K)
          HAFQ=AFQ(I,J,K)
!
          D2PQQ=(Q1(IFPA(I,J,K),JFPA(I,J,K),K)-Q1IJ                     &
     &          -Q1IJ+Q1(IFPF(I,J,K),JFPF(I,J,K),K))                    &
     &          *HAFP                                                   &
     &         +(Q1(IFQA(I,J,K),JFQA(I,J,K),K)-Q1IJ                     &
     &          -Q1IJ+Q1(IFQF(I,J,K),JFQF(I,J,K),K))                    &
     &          *HAFQ
!
          D2PQW=(W1(IFPA(I,J,K),JFPA(I,J,K),K)-W1IJ                     &
     &          -W1IJ+W1(IFPF(I,J,K),JFPF(I,J,K),K))                    &
     &          *HAFP                                                   &
     &         +(W1(IFQA(I,J,K),JFQA(I,J,K),K)-W1IJ                     &
     &          -W1IJ+W1(IFQF(I,J,K),JFQF(I,J,K),K))                    &
     &          *HAFQ
!
         if( advect_Q2) then
          D2PQE=(E2(IFPA(I,J,K),JFPA(I,J,K),K)-E2IJ                     &
     &          -E2IJ+E2(IFPF(I,J,K),JFPF(I,J,K),K))                    &
     &          *HAFP                                                   &
     &         +(E2(IFQA(I,J,K),JFQA(I,J,K),K)-E2IJ                     &
     &          -E2IJ+E2(IFQF(I,J,K),JFQF(I,J,K),K))                    &
     &          *HAFQ
         endif
!
          QSTIJ=Q1IJ-D2PQQ
          WSTIJ=W1IJ-D2PQW
          if(advect_q2) ESTIJ=E2IJ-D2PQE
!
          Q00=Q  (I          ,J          ,K)
          QP0=Q  (IFPA(I,J,K),JFPA(I,J,K),K)
          Q0Q=Q  (IFQA(I,J,K),JFQA(I,J,K),K)
!
          W00=CWM(I          ,J          ,K)
          WP0=CWM(IFPA(I,J,K),JFPA(I,J,K),K)
          W0Q=CWM(IFQA(I,J,K),JFQA(I,J,K),K)
!
         if(advect_Q2) then
          E00=E1 (I          ,J          ,K)
          EP0=E1 (IFPA(I,J,K),JFPA(I,J,K),K)
          E0Q=E1 (IFQA(I,J,K),JFQA(I,J,K),K)
         endif
!
          QSTIJ=MAX(QSTIJ,MIN(Q00,QP0,Q0Q))
          QSTIJ=MIN(QSTIJ,MAX(Q00,QP0,Q0Q))
          WSTIJ=MAX(WSTIJ,MIN(W00,WP0,W0Q))
          WSTIJ=MIN(WSTIJ,MAX(W00,WP0,W0Q))
         if(advect_Q2) then
          ESTIJ=MAX(ESTIJ,MIN(E00,EP0,E0Q))
          ESTIJ=MIN(ESTIJ,MAX(E00,EP0,E0Q))
         endif
!
!          DQSTIJ=QSTIJ-Q(I,J,K)
!          DWSTIJ=WSTIJ-CWM(I,J,K)
!          DESTIJ=ESTIJ-E1(I,J,K)
!
          dqstij=qstij-q1(i,j,k)
          dwstij=wstij-w1(i,j,k)
          if(advect_Q2) destij=estij-e2(i,j,k)
!
          DQST(I,J,K)=DQSTIJ
          DWST(I,J,K)=DWSTIJ
          if(advect_Q2) DEST(I,J,K)=DESTIJ
!
          DQSTIJ=DQSTIJ*DVOLP
          DWSTIJ=DWSTIJ*DVOLP
          if(advect_Q2) DESTIJ=DESTIJ*DVOLP
!
!-----------------------------------------------------------------------
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL) && !defined(STUBMPI)
!-----------------------------------------------------------------------
          DO N=1,6
            XSUMS_L(I,J,K,N)=0.
          ENDDO
!
          IF(DQSTIJ>0.)THEN
            XSUMS_L(I,J,K,1)=DQSTIJ
          ELSE
            XSUMS_L(I,J,K,2)=DQSTIJ
          ENDIF
!
          IF(DWSTIJ>0.)THEN
            XSUMS_L(I,J,K,3)=DWSTIJ
          ELSE
            XSUMS_L(I,J,K,4)=DWSTIJ
          ENDIF
!
          if(advect_q2) then
             IF(DESTIJ>0.)THEN
                XSUMS_L(I,J,K,5)=DESTIJ
             ELSE
                XSUMS_L(I,J,K,6)=DESTIJ
             ENDIF
          endif
!-----------------------------------------------------------------------
#else
!-----------------------------------------------------------------------
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
          if(advect_q2) then
             IF(DESTIJ>0.)THEN
                XSUMS(5,K)=XSUMS(5,K)+DESTIJ
             ELSE
                XSUMS(6,K)=XSUMS(6,K)+DESTIJ
             ENDIF
          endif
!-----------------------------------------------------------------------
#endif
!-----------------------------------------------------------------------
!
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO vertical_2
!
!-----------------------------------------------------------------------
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL) && !defined(STUBMPI)
!-----------------------------------------------------------------------
      DO N=1,6
        CALL WRF_PATCH_TO_GLOBAL_REAL(XSUMS_L(IMS,JMS,KMS,N)            &
     &,                               XSUMS_G(1,1,1,N),DOMDESC          & 
     &,                              'xyz','xyz'                        &
     &,                               IDS,IDE,JDS,JDE,KDS,KDE           &    
     &,                               IMS,IME,JMS,JME,KMS,KME           &
     &,                               ITS,ITE,JTS,JTE,KTS,KTE)
      ENDDO
!
      DO K=KTS,KTE
        DO N=1,6
          GSUMS(N,K)=0.
        ENDDO
      ENDDO
!
      IF(WRF_DM_ON_MONITOR())THEN
        DO N=1,6
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO K=KTS,KTE
          DO J=JDS,JDE
          DO I=IDS,IDE
            GSUMS(N,K)=GSUMS(N,K)+XSUMS_G(I,J,K,N)
          ENDDO
          ENDDO
          ENDDO
        ENDDO
      ENDIF

      CALL WRF_DM_BCAST_BYTES(GSUMS,2*RWORDSIZE*6*(KTE-KTS+1) )

!-----------------------------------------------------------------------
#else
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
# if defined(DM_PARALLEL) && !defined(STUBMPI)
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,6*(KTE-KTS+1)                      &
     &                  ,MPI_DOUBLE_PRECISION,MPI_SUM                   &
     &                  ,MPI_COMM_COMP,IRECV)
# else
      DO K=KTS,KTE
        DO N=1,6
          GSUMS(N,K)=XSUMS(N,K)
        ENDDO
      ENDDO
# endif
!
!-----------------------------------------------------------------------
#endif
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  END OF GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
!     if(mype==0)then
!!!     if(ntsd==0)then
!!!       call int_get_fresh_handle(nunit)
!!!       close(nunit)
!         nunit=56
!!!       open(unit=nunit,file='gsums',form='unformatted',iostat=ier)
!!!     endif
!     endif
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(destij,dqstij,dwstij,i,j,k,rface,rfacq,rfacw             &
!$omp&        ,rfeij,rfqij,rfwij,sumne,sumnq,sumnw,sumpe,sumpq,sumpw)
!-----------------------------------------------------------------------
!
      vertical_3: DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!       if(mype==0)then
!         write(nunit)(gsums(i,k),i=1,6)
!       endif
!!!     read(nunit)(gsums(i,k),i=1,6)
!-----------------------------------------------------------------------
!
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
          RFACQ=-SUMNQ/SUMPQ
        ELSE
          RFACQ=1.
        ENDIF
!
        IF(SUMPW>1.)THEN
          RFACW=-SUMNW/SUMPW
        ELSE
          RFACW=1.
        ENDIF
!
        IF(SUMPE>1.)THEN
          RFACE=-SUMNE/SUMPE
        ELSE
          RFACE=1.
        ENDIF
!
        IF(RFACQ<CONSERVE_MIN.OR.RFACQ>CONSERVE_MAX)RFACQ=1.
        IF(RFACW<CONSERVE_MIN.OR.RFACW>CONSERVE_MAX)RFACW=1.
        IF(RFACE<CONSERVE_MIN.OR.RFACE>CONSERVE_MAX)RFACE=1.
!
!-----------------------------------------------------------------------
!       if(mype==0.and.ntsd==181)close(nunit)
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!-----------------------------------------------------------------------
!
        if(rfacq<1.)then
          do j=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              DQSTIJ=DQST(I,J,K)
              RFQIJ=HBM2(I,J)*(RFACQ-1.)+1.
              IF(DQSTIJ>=0.)DQSTIJ=DQSTIJ*RFQIJ
              q(i,j,k)=q1(i,j,k)+dqstij
            ENDDO
          enddo
        else
          do j=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              DQSTIJ=DQST(I,J,K)
              RFQIJ=HBM2(I,J)*(RFACQ-1.)+1.
              IF(DQSTIJ<0.)DQSTIJ=DQSTIJ/RFQIJ
              q(i,j,k)=q1(i,j,k)+dqstij
            ENDDO
          enddo
        endif
!
!-----------------------------------------------------------------------
!
        if(rfacw<1.)then
          do j=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              DWSTIJ=DWST(I,J,K)
              RFWIJ=HBM2(I,J)*(RFACW-1.)+1.
              IF(DWSTIJ>=0.)DWSTIJ=DWSTIJ*RFWIJ
              cwm(i,j,k)=w1(i,j,k)+dwstij
            ENDDO
          enddo
        else
          do j=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              DWSTIJ=DWST(I,J,K)
              RFWIJ=HBM2(I,J)*(RFACW-1.)+1.
              IF(DWSTIJ<0.)DWSTIJ=DWSTIJ/RFWIJ
              cwm(i,j,k)=w1(i,j,k)+dwstij
            ENDDO
          enddo
        endif

!-----------------------------------------------------------------------
!
       if(advect_Q2) then
        if(rface<1.)then
          do j=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              DESTIJ=DEST(I,J,K)
              RFEIJ=HBM2(I,J)*(RFACE-1.)+1.
              IF(DESTIJ>=0.)DESTIJ=DESTIJ*RFEIJ
              e1(i,j,k)=e2(i,j,k)+destij
            ENDDO
          enddo
        else
          do j=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              DESTIJ=DEST(I,J,K)
              RFEIJ=HBM2(I,J)*(RFACE-1.)+1.
              IF(DESTIJ<0.)DESTIJ=DESTIJ/RFEIJ
              e1(i,j,k)=e2(i,j,k)+destij
            ENDDO
          enddo
        endif
       endif
!
!-----------------------------------------------------------------------
!
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          Q  (I,J,K)=MAX(Q  (I,J,K),EPSQ)
          CWM(I,J,K)=MAX(CWM(I,J,K),CLIMIT)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO vertical_3
!
!-----------------------------------------------------------------------
!
    if(advect_Q2) then
!$omp parallel do                                                       &
!$omp& private(i,j)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        Q2(I,J,KTE)=MAX(E1(I,J,KTE)+E1(I,J,KTE)-EPSQ2,EPSQ2)
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      DO K=KTE-1,KTS+1,-1
!$omp parallel do                                                       &
!$omp& private(i,j)
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          IF(K>KTS)THEN
            Q2(I,J,K)=MAX(E1(I,J,K)+E1(I,J,K)-Q2(I,J,K+1),EPSQ2)
          ELSE
            Q2(I,J,K)=Q2(I,J,K+1)
          ENDIF
        ENDDO
        ENDDO
      ENDDO
     endif
!-----------------------------------------------------------------------
!
      END SUBROUTINE HAD2
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
! New routines added by Georg Grell to handle advection more like ARW
! core.  Instead of VAD2/HAD2 that advect TKE, specific humidity, and
! condensed water species all in one routine, we call VAD2/HAD2_SCAL
! with multidimensioned arrays to advect each variable.  For purposes
! here, solve_nmm.F calls this routine once for TKE, then again for
! all the species held in the moist array (qv, qc, qi, qr, qs, qg),
! then call again for number concentrations held in scalar array (qni).
! The dummy argument lstart is the starting index of the multidimensioned
! array for starting the advection since the 1st index of moist and
! scalar are actually empty placeholders (and the 2nd element is vapor,
! then qc, etc.)  When calling with single 3D array (like TKE), just
! set NUM_SCAL=1 and lstart=1.  The variable to advect is called SCAL
! herein.
!***********************************************************************
      SUBROUTINE VAD2_SCAL(NTSD,DT,IDTAD,DX,DY                          &
     &               ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                &
     &               ,HBM2                                              &
     &               ,SCAL,PETDT                                        &
     &               ,N_IUP_H,N_IUP_V                                   &
     &               ,N_IUP_ADH,N_IUP_ADV                               &
     &               ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                       &
     &               ,IHE,IHW,IVE,IVW                                   &
     &               ,NUM_SCAL,LSTART                                   &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &               ,IMS,IME,JMS,JME,KMS,KME                           &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    VAD2_SCAL   VERTICAL ADVECTION OF SCALARS
!
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 96-07-19
!            GRELL,PECKHAM   ORG: NOAA/FSL   DATE: 05-02-03
!     
! ABSTRACT:          
!     VAD2_SCAL CALCULATES THE CONTRIBUTION OF THE VERTICAL ADVECTION   
!     TO THE TENDENCIES OF SCALAR SUBSTANCES AND THEN UPDATES           
!     THOSE VARIABLES.  AN ANTI-FILTERING TECHNIQUE IS USED.            
!    
! PROGRAM HISTORY LOG:
!   96-07-19  JANJIC           - ORIGINATOR
!   05-02-03  GRELL,PECKHAM    - MODIFIED FOR SCALARS                   
!    
! USAGE: CALL VAD2_SCAL FROM SUBROUTINE SOLVE_NMM                       
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
!   MACHINE : IBM 
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
      INTEGER,INTENT(IN) :: LSTART,NUM_SCAL
!
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
      INTEGER,DIMENSION(JMS:JME),INTENT(IN) :: N_IUP_H,N_IUP_V          &
     &                                        ,N_IUP_ADH,N_IUP_ADV
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: IUP_H,IUP_V      &
     &                                                ,IUP_ADH,IUP_ADV
!
      INTEGER,INTENT(IN) :: IDTAD,NTSD
!
      REAL,INTENT(IN) :: DT,DY,PDTOP
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX,HBM2,PDSL
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: PETDT
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,1:NUM_SCAL)               &
                                                 ,INTENT(INOUT) :: SCAL
!
!----------------------------------------------------------------------
!***  LOCAL VARIABLES
!----------------------------------------------------------------------
!
      REAL,PARAMETER :: FF1=0.500
!
      LOGICAL,SAVE :: TRADITIONAL=.TRUE.
!
      INTEGER :: I,IRECV,J,JFP,JFQ,K,L,LAP,LLAP
!
      INTEGER,DIMENSION(KTS:KTE) :: LA
!
      REAL*8 :: ADDT,AFRP,D2PQQ,DETAP,DPDN,DPUP,DQP                     &
     &         ,HADDT,HBM2IJ                                            &
     &         ,Q00,Q4P,QP,QP0                                          &
     &         ,RFACQK,RFC,RR                                           &
     &         ,SUMNQ,SUMPQ
!
      REAL :: SFACQK
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,DQL,DWL,E3,E4,PETDTK           &
     &                          ,RFACE,RFACQ,RFACW,Q3,Q4,W3,W4
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ADDT=REAL(IDTAD)*DT
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(afr,afrp,d2pqq,detap,dpdn,dpup                           &
!$omp&        ,dql,dqp,haddt,i,j,k                                      &
!$omp&        ,la,lap,llap,petdtk,q00,q3,q4,q4p,qp,qp0,rfacqk           &
!$omp&        ,rfc,rr,sfacqk,sumnq,sumpq)
!-----------------------------------------------------------------------
!
      scalar_loop: DO L=LSTART,NUM_SCAL
!
      main_integration: DO J=MYJS2,MYJE2
!
!-----------------------------------------------------------------------
!
        main_iloop: DO I=MYIS1_P1,MYIE1_P1
!
!-----------------------------------------------------------------------
!
          DO K=KTS,KTE
            Q3(K)=SCAL(I,J,K,L)
            Q4(K)=Q3(K)
          ENDDO
!
          IF(TRADITIONAL)THEN
            PETDTK(KTE)=PETDT(I,J,KTE-1)*0.5
!
            DO K=KTE-1,KTS+1,-1
              PETDTK(K)=(PETDT(I,J,K)+PETDT(I,J,K-1))*0.5
            ENDDO
!
            PETDTK(KTS)=PETDT(I,J,KTS)*0.5
!
          ELSE
!
!-----------------------------------------------------------------------
!***    PERFORM HORIZONTAL AVERAGING OF VERTICAL VELOCITY
!-----------------------------------------------------------------------
!
            PETDTK(KTE)=(PETDT(I+IHW(J-1),J-1,KTE-1)                    &
     &                  +PETDT(I+IHE(J-1),J-1,KTE-1)                    &
     &                  +PETDT(I+IHW(J+1),J+1,KTE-1)                    &
     &                  +PETDT(I+IHE(J+1),J+1,KTE-1)                    &
     &                  +PETDT(I,J,KTE-1)*4.        )*0.0625
!
            DO K=KTE-1,KTS+1,-1
              PETDTK(K)=(PETDT(I+IHW(J-1),J-1,K-1)                      &
                        +PETDT(I+IHE(J-1),J-1,K-1)                      &
     &                  +PETDT(I+IHW(J+1),J+1,K-1)                      &
     &                  +PETDT(I+IHE(J+1),J+1,K-1)                      &
     &                  +PETDT(I+IHW(J-1),J-1,K  )                      &
     &                  +PETDT(I+IHE(J-1),J-1,K  )                      &
     &                  +PETDT(I+IHW(J+1),J+1,K  )                      &
     &                  +PETDT(I+IHE(J+1),J+1,K  )                      &
     &                  +(PETDT(I,J,K-1)+PETDT(I,J,K))*4.               &
     &                                                   )*0.0625
            ENDDO
!
            PETDTK(KTS)=(PETDT(I+IHW(J-1),J-1,KTS)                      &
     &                  +PETDT(I+IHE(J-1),J-1,KTS)                      &
     &                  +PETDT(I+IHW(J+1),J+1,KTS)                      &
     &                  +PETDT(I+IHE(J+1),J+1,KTS)                      &
     &                  +PETDT(I,J,KTS)*4.        )*0.0625
 
          ENDIF
!
!-----------------------------------------------------------------------
!
          HADDT=-ADDT*HBM2(I,J)
!
          DO K=KTE,KTS,-1
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
            IF(LLAP>KTS-1.AND.LLAP<KTE+1)THEN
              RR=ABS(RR/((AETA1(LLAP)-AETA1(K))*PDTOP                   &
     &                  +(AETA2(LLAP)-AETA2(K))*PDSL(I,J)))
              IF(RR>0.9)RR=0.9
!
              AFR(K)=(((FF4*RR+FF3)*RR+FF2)*RR+FF1)*RR
              DQP=(Q3(LLAP)-Q3(K))*RR
              DQL(K)=DQP
            ELSE
              RR=0.
              AFR(K)=0.
              DQL(K)=0.
            ENDIF
          ENDDO
!
!-----------------------------------------------------------------------
!
          IF(LA(KTE-1)>0)THEN
            RFC=(DETA1(KTE-1)*PDTOP+DETA2(KTE-1)*PDSL(I,J))             &
     &         /(DETA1(KTE  )*PDTOP+DETA2(KTE  )*PDSL(I,J))
            DQL(KTE)=-DQL(KTE-1)*RFC
          ENDIF
!
          IF(LA(KTS+1)<0)THEN
            RFC=(DETA1(KTS+1)*PDTOP+DETA2(KTS+1)*PDSL(I,J))           &
     &         /(DETA1(KTS  )*PDTOP+DETA2(KTS  )*PDSL(I,J))
            DQL(KTS)=-DQL(KTS+1)*RFC
          ENDIF
!
          DO K=KTS,KTE
            Q4(K)=Q3(K)+DQL(K)
          ENDDO
!
!-----------------------------------------------------------------------
!***  ANTI-FILTERING STEP
!-----------------------------------------------------------------------
!
          SUMPQ=0.
          SUMNQ=0.
!
!***  ANTI-FILTERING LIMITERS
!
          antifilter: DO K=KTE-1,KTS+1,-1
!
            DETAP=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
!
            Q4P=Q4(K)
!
            LAP=LA(K)
!
            DPDN=(AETA1(K+LAP)-AETA1(K))*PDTOP                          &
     &          +(AETA2(K+LAP)-AETA2(K))*PDSL(I,J)
            DPUP=(AETA1(K)-AETA1(K-LAP))*PDTOP                          &
     &          +(AETA2(K)-AETA2(K-LAP))*PDSL(I,J)
!
            AFRP=2.*AFR(K)*DPDN*DPDN/(DPDN+DPUP)
            D2PQQ=((Q4(K+LAP)-Q4P)/DPDN                                 &
     &            -(Q4P-Q4(K-LAP))/DPUP)*AFRP
!
            QP=Q4P-D2PQQ
!
            Q00=Q3(K)
            QP0=Q3(K+LAP)
!
            QP=MAX(QP,MIN(Q00,QP0))
            QP=MIN(QP,MAX(Q00,QP0))
!
            DQP=QP-Q00
!
            DQL(K)=DQP
!
          ENDDO antifilter
!
!-----------------------------------------------------------------------
!
          IF(LA(KTE-1)>0)THEN
            RFC=(DETA1(KTE-1)*PDTOP+DETA2(KTE-1)*PDSL(I,J))             &
     &         /(DETA1(KTE  )*PDTOP+DETA2(KTE  )*PDSL(I,J))
            DQL(KTE)=-DQL(KTE-1)*RFC+DQL(KTE)
          ENDIF
!
          IF(LA(KTS+1)<0)THEN
            RFC=(DETA1(KTS+1)*PDTOP+DETA2(KTS+1)*PDSL(I,J))             &
     &         /(DETA1(KTS  )*PDTOP+DETA2(KTS  )*PDSL(I,J))
            DQL(KTS)=-DQL(KTS+1)*RFC+DQL(KTS)
          ENDIF
!
          DO K=KTS,KTE
            DETAP=DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J)
            DQP=DQL(K)*DETAP
!
            IF(DQP>0.)THEN
              SUMPQ=SUMPQ+DQP
            ELSE
              SUMNQ=SUMNQ+DQP
            ENDIF
          ENDDO
!
!-----------------------------------------------------------------------
!***  FIRST MOMENT CONSERVING FACTOR
!-----------------------------------------------------------------------
!
          IF(SUMPQ>1.E-9)THEN
            SFACQK=-SUMNQ/SUMPQ
          ELSE
            SFACQK=1.
          ENDIF
!
          IF(SFACQK<CONSERVE_MIN.OR.SFACQK>CONSERVE_MAX)SFACQK=1.
!
          RFACQK=1./SFACQK
!
!-----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!-----------------------------------------------------------------------
!
          DO K=KTE,KTS,-1
            DQP=DQL(K)
            IF(SFACQK>=1.)THEN
              IF(DQP<0.)DQP=DQP*RFACQK
            ELSE
              IF(DQP>0.)DQP=DQP*SFACQK
            ENDIF
            SCAL(I,J,K,L)=Q3(K)+DQP
          ENDDO
!
!-----------------------------------------------------------------------
!
        ENDDO main_iloop
!
!-----------------------------------------------------------------------
!
      ENDDO main_integration
!
!-----------------------------------------------------------------------
!
      ENDDO scalar_loop
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE VAD2_SCAL
!
!-----------------------------------------------------------------------
!         
!***********************************************************************
      SUBROUTINE HAD2_SCAL(                                             &
#if defined(DM_PARALLEL)
     &                DOMDESC ,                                         &
#endif  
     &                NTSD,DT,IDTAD,DX,DY                               &
     &               ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                &
     &               ,HBM2,HBM3                                         &
     &               ,SCAL,U,V,Z,HYDRO                                  &
     &               ,N_IUP_H,N_IUP_V                                   &
     &               ,N_IUP_ADH,N_IUP_ADV                               &
     &               ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                       &
     &               ,IHE,IHW,IVE,IVW                                   &
     &               ,NUM_SCAL,LSTART                                   &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &               ,IMS,IME,JMS,JME,KMS,KME                           &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    HAD2_SCAL   HORIZONTAL ADVECTION OF SCALAR
!   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 96-07-19
!            GRELL,PECKHAM   ORG: NOAA/FSL   DATE: 05-02-03
!
! ABSTRACT:
!     HAD2_SCAL CALCULATES THE CONTRIBUTION OF THE HORIZONTAL ADVECTION
!     TO THE TENDENCIES OF SCALAR SUBSTANCES AND THEN
!     UPDATES THOSE VARIABLES.  AN ANTI-FILTERING TECHNIQUE IS USED.
!
! PROGRAM HISTORY LOG:
!   96-07-19  JANJIC           - ORIGINATOR
!   05-01-03  GRELL,PECKHAM    - MODIFIED FOR SCALAR
!
! USAGE: CALL HAD2_SCAL FROM SUBROUTINE SOLVE_NMM
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
!   MACHINE : IBM 
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
!
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IDTAD,LSTART,NTSD,NUM_SCAL
!
      REAL,INTENT(IN) :: DT,DY,PDTOP
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: DX,HBM2,HBM3,PDSL
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: U,V,Z
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,1:NUM_SCAL)                &
                                                  ,INTENT(INOUT) :: SCAL
!
      LOGICAL,INTENT(IN) :: HYDRO
!
!-----------------------------------------------------------------------
!***  LOCAL VARIABLES
!-----------------------------------------------------------------------
!
      REAL,PARAMETER :: FF1=0.530
!
#ifdef DM_PARALLEL
      INTEGER :: DOMDESC
#endif
!
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL) && !defined(STUBMPI)
      LOGICAL,EXTERNAL :: WRF_DM_ON_MONITOR
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME,2) :: XSUMS_L
      REAL,DIMENSION(IDS:IDE,JDS:JDE,KDS:KDE,2) :: XSUMS_G
#endif
!
      LOGICAL :: BOT,TOP
!
      INTEGER :: I,IRECV,J,JFP,JFQ,K,L,LAP,LLAP,MPI_COMM_COMP
      INTEGER :: N
!
      INTEGER,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5,KTS:KTE) :: IFPA,IFPF   &
     &                                                     ,IFQA,IFQF   &
     &                                                     ,JFPA,JFPF   &
     &                                                     ,JFQA,JFQF
!
      REAL :: ADDT,AFRP,CRIT,D2PQE,D2PQQ,D2PQW,DEP,DESTIJ,DQP,DQSTIJ    &
     &       ,DVOLP,DWP,DWSTIJ,DZA,DZB,E00,E0Q,E1X,E2IJ,E4P,ENH,EP,EP0  &
     &       ,ESTIJ,FPQ,HAFP,HAFQ,HBM2IJ,HM,PP,PPQ00,Q00,Q0Q            &
     &       ,Q1IJ,Q4P,QP,QP0,QSTIJ,RDY,RFACE,RFACQ,RFACW,RFC           &
     &       ,RFEIJ,RFQIJ,RFWIJ,RR,SLOPAC,SPP,SQP,SSA,SSB,SUMNQ,SUMPQ   &
     &       ,TTA,TTB,W00,W0Q,W1IJ,W4P,WP,WP0,WSTIJ
!
      DOUBLE PRECISION,DIMENSION(2,KTS:KTE) :: GSUMS,XSUMS
!
      REAL,DIMENSION(KTS:KTE) :: AFR,DEL,DQL,DWL,Q3,Q4
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: DARE,EMH
!
      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5,KTS:KTE) :: AFP,AFQ,DEST   &
     &                                                  ,DQST,DVOL,DWST &
     &                                                  ,Q1
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME) :: Q
!
!-----------------------------------------------------------------------
      integer :: nunit,ier
      save nunit
!-----------------------------------------------------------------------
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
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
      scalar_loop: DO L=LSTART,NUM_SCAL
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(dza,dzb,e1x,fpq,hm,i,j,jfp,jfq,k,pp,qp,ssa,ssb,spp,sqp   &
!$omp&        ,tta,ttb)
!-----------------------------------------------------------------------
!
      vertical_1: DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!
        DO J=MYJS_P3,MYJE_P3
        DO I=MYIS_P2,MYIE_P2
          DVOL(I,J,K)=DARE(I,J)*(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))
          Q (I,J,K)=SCAL(I,J,K,L)
          Q1(I,J,K)=Q(I,J,K)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
        DO J=MYJS2_P1,MYJE2_P1
        DO I=MYIS1_P1,MYIE1_P1
!
          HM=HBM2(I,J)
          TTA=(U(I,J-1,K)+U(I+IHW(J),J,K)+U(I+IHE(J),J,K)+U(I,J+1,K))   &
     &        *EMH(I,J)*HM
          TTB=(V(I,J-1,K)+V(I+IHW(J),J,K)+V(I+IHE(J),J,K)+V(I,J+1,K))   &
     &        *ENH*HBM2(I,J)
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
          IFPA(I,J,K)=IHE(J)+I+( JFP-1)/2
          IFQA(I,J,K)=IHE(J)+I+(-JFQ-1)/2
!
          JFPA(I,J,K)=J+JFP
          JFQA(I,J,K)=J+JFQ
!
          IFPF(I,J,K)=IHE(J)+I+(-JFP-1)/2
          IFQF(I,J,K)=IHE(J)+I+( JFQ-1)/2
!
          JFPF(I,J,K)=J-JFP
          JFQF(I,J,K)=J-JFQ
!
!-----------------------------------------------------------------------
          IF(.NOT.HYDRO)THEN ! z currently not available for hydro=.true.
            DZA=(Z(IFPA(I,J,K),JFPA(I,J,K),K)-Z(I,J,K))*RDY
            DZB=(Z(IFQA(I,J,K),JFQA(I,J,K),K)-Z(I,J,K))*RDY
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
!
!-----------------------------------------------------------------------
!
          FPQ=SPP*SQP*0.25
          PP=ABS(SPP)
          QP=ABS(SQP)
!
          AFP(I,J,K)=(((FF4*PP+FF3)*PP+FF2)*PP+FF1)*PP
          AFQ(I,J,K)=(((FF4*QP+FF3)*QP+FF2)*QP+FF1)*QP
!
          Q1(I,J,K)=(Q  (IFPA(I,J,K),JFPA(I,J,K),K)-Q  (I,J,K))*PP        &
       &           +(Q  (IFQA(I,J,K),JFQA(I,J,K),K)-Q  (I,J,K))*QP        &
       &           +(Q  (I,J-2,K)+Q  (I,J+2,K)                            &
       &            -Q  (I-1,J,K)-Q  (I+1,J,K))*FPQ                       &
       &           +Q(I,J,K)
!
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO vertical_1
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
!-----------------------------------------------------------------------
!
!***  ANTI-FILTERING LIMITERS
!
!-----------------------------------------------------------------------
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL) && !defined(STUBMPI)
      DO N=1,2
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KMS,KME 
        DO J=JMS,JME 
        DO I=IMS,IME 
          XSUMS_L(I,J,K,N)=0.
        ENDDO
        ENDDO
        ENDDO
!
!$omp parallel do                                                       &
!$omp& private(i,j,k)
        DO K=KDS,KDE 
        DO J=JDS,JDE 
        DO I=IDS,IDE 
          XSUMS_G(I,J,K,N)=0.
        ENDDO
        ENDDO
        ENDDO
!
      ENDDO
!
#endif
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(d2pqe,d2pqq,d2pqw,destij,dqstij,dvolp,dwstij             &
!$omp&        ,e00,e0q,ep0,estij,hafp,hafq,i,j,k                        &
!$omp&        ,q00,q0q,q1ij,qp0,qstij,w00,w0q,wp0,wstij)
!-----------------------------------------------------------------------
!
      vertical_2: DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
!
          DVOLP=DVOL(I,J,K)
          Q1IJ =Q1(I,J,K)
!
          HAFP=AFP(I,J,K)
          HAFQ=AFQ(I,J,K)
!
          D2PQQ=(Q1(IFPA(I,J,K),JFPA(I,J,K),K)-Q1IJ                     &
     &          -Q1IJ+Q1(IFPF(I,J,K),JFPF(I,J,K),K))                    &
     &          *HAFP                                                   &
     &         +(Q1(IFQA(I,J,K),JFQA(I,J,K),K)-Q1IJ                     &
     &          -Q1IJ+Q1(IFQF(I,J,K),JFQF(I,J,K),K))                    &
     &          *HAFQ
!
          QSTIJ=Q1IJ-D2PQQ
!
          Q00=Q  (I          ,J          ,K)
          QP0=Q  (IFPA(I,J,K),JFPA(I,J,K),K)
          Q0Q=Q  (IFQA(I,J,K),JFQA(I,J,K),K)
!
          QSTIJ=MAX(QSTIJ,MIN(Q00,QP0,Q0Q))
          QSTIJ=MIN(QSTIJ,MAX(Q00,QP0,Q0Q))
!
          DQSTIJ=QSTIJ-Q(I,J,K)
!
          DQST(I,J,K)=DQSTIJ
!
          DQSTIJ=DQSTIJ*DVOLP
!
!-----------------------------------------------------------------------
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL) && !defined(STUBMPI)
!-----------------------------------------------------------------------
          DO N=1,2
            XSUMS_L(I,J,K,N)=0.
          ENDDO
!
          IF(DQSTIJ>0.)THEN
            XSUMS_L(I,J,K,1)=DQSTIJ
          ELSE
            XSUMS_L(I,J,K,2)=DQSTIJ
          ENDIF
!
!-----------------------------------------------------------------------
#else
!-----------------------------------------------------------------------
          IF(DQSTIJ>0.)THEN
            XSUMS(1,K)=XSUMS(1,K)+DQSTIJ
          ELSE
            XSUMS(2,K)=XSUMS(2,K)+DQSTIJ
          ENDIF
!
!-----------------------------------------------------------------------
#endif
!-----------------------------------------------------------------------
!
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO vertical_2
!
!-----------------------------------------------------------------------
#if defined(BIT_FOR_BIT) && defined(DM_PARALLEL) && !defined(STUBMPI)
!-----------------------------------------------------------------------
      DO N=1,2
        CALL WRF_PATCH_TO_GLOBAL_REAL(XSUMS_L(IMS,JMS,KMS,N)            &
     &,                               XSUMS_G(1,1,1,N),DOMDESC          & 
     &,                              'xyz','xzy'                        &
     &,                               IDS,IDE,KDS,KDE,JDS,JDE           &    
     &,                               IMS,IME,KMS,KME,JMS,JME           &
     &,                               ITS,ITE,KTS,KTE,JTS,JTE)
      ENDDO
!
      DO K=KTS,KTE
        DO N=1,2
          GSUMS(N,K)=0.
        ENDDO
      ENDDO
!
      IF(WRF_DM_ON_MONITOR())THEN
        DO N=1,2
!$omp parallel do                                                       &
!$omp& private(i,j,k)
          DO K=KDS,KDE
          DO J=JDS,JDE
          DO I=IDS,IDE
            GSUMS(N,K)=GSUMS(N,K)+XSUMS_G(I,J,K,N)
          ENDDO
          ENDDO
          ENDDO
        ENDDO
      ENDIF

      CALL WRF_DM_BCAST_BYTES(GSUMS,2*RWORDSIZE*2*(KDE-KDS+1) )

!-----------------------------------------------------------------------
#else
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
# if defined(DM_PARALLEL) && !defined(STUBMPI)
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,2*(KTE-KTS+1)                      &
     &                  ,MPI_DOUBLE_PRECISION,MPI_SUM                   &
     &                  ,MPI_COMM_COMP,IRECV)
# else
      DO K=KTS,KTE
        DO N=1,2
          GSUMS(N,K)=XSUMS(N,K)
        ENDDO
      ENDDO
# endif
!
!-----------------------------------------------------------------------
#endif
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  END OF GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
!     if(mype==0)then
!!!     if(ntsd==0)then
!!!       call int_get_fresh_handle(nunit)
!!!       close(nunit)
!         nunit=56
!!!       open(unit=nunit,file='gsums',form='unformatted',iostat=ier)
!!!     endif
!     endif
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(destij,dqstij,dwstij,i,j,k,rface,rfacq,rfacw             &
!$omp&        ,rfeij,rfqij,rfwij,sumnq,sumpq)
!-----------------------------------------------------------------------
!
      vertical_3: DO K=KTS,KTE
!
!-----------------------------------------------------------------------
!       if(mype==0)then
!         write(nunit)(gsums(i,k),i=1,6)
!       endif
!!!     read(nunit)(gsums(i,k),i=1,6)
!-----------------------------------------------------------------------
!
        SUMPQ=GSUMS(1,K)
        SUMNQ=GSUMS(2,K)
!
!-----------------------------------------------------------------------
!***  FIRST MOMENT CONSERVING FACTOR
!-----------------------------------------------------------------------
!
        IF(SUMPQ>1.)THEN
          RFACQ=-SUMNQ/SUMPQ
        ELSE
          RFACQ=1.
        ENDIF
!
        IF(RFACQ<CONSERVE_MIN.OR.RFACQ>CONSERVE_MAX)RFACQ=1.
!
!-----------------------------------------------------------------------
!       if(mype==0.and.ntsd==181)close(nunit)
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  IMPOSE CONSERVATION ON ANTI-FILTERING
!-----------------------------------------------------------------------
!
        DO J=MYJS2,MYJE2
          IF(RFACQ<1.)THEN
            DO I=MYIS1,MYIE1
              DQSTIJ=DQST(I,J,K)
              RFQIJ=HBM2(I,J)*(RFACQ-1.)+1.
              IF(DQSTIJ>=0.)DQSTIJ=DQSTIJ*RFQIJ
              Q(I,J,K)=Q(I,J,K)+DQSTIJ
            ENDDO
          ELSE
            DO I=MYIS1,MYIE1
              DQSTIJ=DQST(I,J,K)
              RFQIJ=HBM2(I,J)*(RFACQ-1.)+1.
              IF(DQSTIJ<0.)DQSTIJ=DQSTIJ/RFQIJ
              Q(I,J,K)=Q(I,J,K)+DQSTIJ
            ENDDO
          ENDIF
        ENDDO
!
!-----------------------------------------------------------------------
!
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          SCAL(I,J,K,L)=Q(I,J,K)
        ENDDO
        ENDDO
!
!-----------------------------------------------------------------------
!
      ENDDO vertical_3
!
!-----------------------------------------------------------------------
!
      ENDDO scalar_loop
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE HAD2_SCAL
!
!-----------------------------------------------------------------------
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                        subroutine adv2 &
(UPSTRM &
,mype,kss,kse &
,ids,ide,jds,jde,kds,kde &
,ims,ime,jms,jme,kms,kme &
,its,ite,jts,jte,kts,kte &
,N_IUP_H &
,N_IUP_ADH &
,IUP_H,IUP_ADH &
,ENT &
,idtad &
,dt,pdtop &
,ihe,ihw,ive,ivw &
,deta1,deta2 &
,EMT_LOC &
,fad,hbm2,pdsl,pdslo &
,petdt &
,UOLD,VOLD &
,s,sp &
!---temporary arguments-------------------------------------------------
,fne,fse,few,fns,s1,tcs)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
implicit none
!-----------------------------------------------------------------------
real,parameter:: &
 cfc=1.533 &                 ! adams-bashforth positioning in time
,bfc=1.-cfc &                ! adams bashforth positioning in time
,cflc=9.005 &                !
,epsq=1.e-20 &               ! floor value for specific humidity
,epsq2=0.2 &                 ! floor value for 2tke
,epscm=2.e-6 &               ! a floor value (not used)
,w1=1.0 &                    ! crank-nicholson uncentering
!,w1=-1.00 &                  ! crank-nicholson uncentering
,w2=2.-w1                    ! crank-nicholson uncentering

logical,intent(in):: &
 upstrm

integer,intent(in):: &
 idtad &                     ! time step multiplier
,kse &                       ! terminal species index
,kss &                       ! initial species index
,mype &                      !
,ids,ide,jds,jde,kds,kde &
,ims,ime,jms,jme,kms,kme &
,its,ite,jts,jte,kts,kte

real,intent(in):: &
 ent &                       !
,dt &                        ! dynamics time step
,pdtop                       !

real,dimension(kts:kte),intent(in):: &
 deta1 &                     ! delta sigmas
,deta2                       ! delta pressures

integer,dimension(jms:jme),intent(in):: &
 ihe,ihw,ive,ivw &
,n_iup_adh,n_iup_h

integer,dimension(ims:ime,jms:jme),intent(in):: &
 iup_h,iup_adh

real,dimension(2600),intent(in):: & !!!zj see nmm_max_dim in adve !!!zj
 emt_loc

real,dimension(ims:ime,jms:jme),intent(in):: &
 fad &                       !
,hbm2 &                      !
,pdsl &                      ! sigma range pressure difference
,pdslo                       ! sigma range pressure difference

real,dimension(ims:ime,jms:jme,kms:kme),intent(in):: &
 petdt &                     ! vertical mass flux
,uold,vold

real,dimension(ims:ime,jms:jme,kms:kme,kss:kse),intent(inout):: &
 s                           ! tracers

real,dimension(ims:ime,jms:jme,kms:kme,kss:kse),intent(inout):: &
 sp                          ! s at previous time level

!---temporary arguments-------------------------------------------------
real,dimension(ims:ime,jms:jme,kms:kme),intent(in):: &
 fne &                       ! mass flux, ne direction
,fse &                       ! mass flux, se direction
,few &                       ! mass flux, x direction
,fns                         ! mass flux, y direction

real,dimension(ims:ime,jms:jme,kms:kme,kss:kse),intent(inout):: &
 s1 &                        ! intermediate value of s
,tcs                         ! timechange of s

!--local variables------------------------------------------------------
integer:: &
 i &                         !
,j &                         !
,k &                         !
,ks                          !

      INTEGER :: IEND,IFP,IFQ,II,IPQ,ISP,ISQ,ISTART &
     &          ,IUP_ADH_J &
     &          ,J1,JA,JAK,JEND,JGLOBAL,JJ,JKNT,JP2,JSTART &
     &          ,KNTI_ADH,KSTART,KSTOP &
     &          ,N,N_IUPH_J,N_IUPADH_J,N_IUPADV_J

      INTEGER :: MY_IS_GLB,MY_IE_GLB,MY_JS_GLB,MY_JE_GLB

real:: &
 cf &                        ! temporary
,cms &                       ! temporary
,dtq &                       ! dt/4
,fahp &                      ! temporary grid factor
,sn &                        !
,rdp &                       ! 1/deltap
,vvlo &                      ! vertical velocity, lower interface
,vvup &                      ! vertical velocity, upper interface
,pvvup                       ! vertical mass flux, upper interface

      REAL :: ARRAY3_X &
     &       ,F0,F1,F2,F3 &
     &       ,PP &
     &       ,QP &
     &       ,TEMPA,TEMPB,TTA,TTB

real,dimension(kts:kte):: &
 deta1_pdtop                 !

      INTEGER,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: ISPA,ISQA

real,dimension(its-5:ite+5,jts-5:jte+5):: &
 pdop &                      ! hydrostatic pressure difference at h points
,pvvlo &                     ! vertical mass flux, lower interface
,ss1 &                       ! extrapolated species between time levels
,ssne &                      ! flux, ne direction
,ssse &                      ! flux, nw direction
,ssx &                       ! flux, x direction
,ssy                         ! flux, y direction

      REAL,DIMENSION(ITS-5:ITE+5,JTS-5:JTE+5) :: ARRAY0,ARRAY1 &
     &                                          ,ARRAY2,ARRAY3

real,dimension(its-5:ite+5,jts-5:jte+5,kts:kte):: &
 crs &                       ! vertical advection temporary
,rcms                        ! vertical advection temporary

real,dimension(its-5:ite+5,jts-5:jte+5,kts:kte,kss:kse):: &
 rsts                        ! vertical advection temporary
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DO J=JTS-5,JTE+5
        DO I=ITS-5,ITE+5
          pdop (i,j)=0.
          pvvlo(i,j)=0.
          ss1  (i,j)=0.
          ssne (i,j)=0.
          ssse (i,j)=0.
        enddo
      enddo
!
      DO K=KTS,KTE
        DO J=JTS-5,JTE+5
          DO I=ITS-5,ITE+5
            crs (i,j,k)=0.
            rcms(i,j,k)=0.
          enddo
        enddo
      enddo
!
      do ks=kss,kse
        DO K=KTS,KTE
          DO J=JTS-5,JTE+5
            DO I=ITS-5,ITE+5
              rsts(i,j,k,ks)=0.
            enddo
          enddo
        enddo
      enddo
!
      do ks=kss,kse
        DO K=KMS,KME
          DO J=JMS,JME
            DO I=IMS,IME
              s1 (i,j,k,ks)=0.
              tcs(i,j,k,ks)=0.
            enddo
          enddo
        enddo
      enddo
!-----------------------------------------------------------------------
      do k=kts,kte
        deta1_pdtop(k)=deta1(k)*pdtop
      enddo
!-----------------------------------------------------------------------
      do ks=kss,kse ! loop by species
!-----------------------------------------------------------------------
        DO K=KTS,KTE
          DO J=MYJS_P4,MYJE_P4
            DO I=MYIS_P4,MYIE_P4
              s1(i,j,k,ks)=sqrt(s(i,j,k,ks))
            enddo
          enddo
        enddo
!-----------------------------------------------------------------------
      enddo ! end of the loop by species
!-----------------------------------------------------------------------
      DO J=MYJS_P4,MYJE_P4
        DO I=MYIS_P4,MYIE_P4
          pdop(i,j)=(pdslo(i,j)+pdsl(i,j))*0.5
        enddo
      enddo
!---crank-nicholson vertical advection----------------------------------
      dtq=dt*idtad*0.25

      DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          pvvlo(i,j)=petdt(i,j,kte-1)*dtq
          vvlo=pvvlo(i,j)/(deta2(kte)*pdop(i,j)+deta1_pdtop(kte))
!
          cms=-vvlo*w2+1.
          rcms(i,j,kte)=1./cms
          crs(i,j,kte)=vvlo*w2
!
          do ks=kss,kse
            rsts(i,j,kte,ks)=(-vvlo*w1) &
                            *(s1(i,j,kte-1,ks)-s1(i,j,kte,ks)) &
                            +s1(i,j,kte,ks)
          enddo
        enddo
      enddo
      DO K=KTE-1,KTS+1,-1
        DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            rdp=1./(deta2(k)*pdop(i,j)+deta1_pdtop(k))
            pvvup=pvvlo(i,j)
            pvvlo(i,j)=petdt(i,j,k-1)*dtq
!
            vvup=pvvup*rdp
            vvlo=pvvlo(i,j)*rdp
!
!            if(abs(vvlo).gt.cflc) then
!              if(vvlo.lt.0.) then
!                vvlo=-cflc
!              else
!                vvlo= cflc
!              endif
!            endif
!
            cf=-vvup*w2*rcms(i,j,k+1)
            cms=-crs(i,j,k+1)*cf+((vvup-vvlo)*w2+1.)
            rcms(i,j,k)=1./cms
            crs(i,j,k)=vvlo*w2
!
            do ks=kss,kse
              rsts(i,j,k,ks)=-rsts(i,j,k+1,ks)*cf+s1(i,j,k,ks) &
                             -(s1(i,j,k  ,ks)-s1(i,j,k+1,ks))*vvup*w1 &
                             -(s1(i,j,k-1,ks)-s1(i,j,k  ,ks))*vvlo*w1
            enddo
          enddo
        enddo
      enddo
      DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          pvvup=pvvlo(i,j)
          vvup=pvvup/(deta2(kts)*pdop(i,j)+deta1_pdtop(kts))
!
          cf=-vvup*w2*rcms(i,j,kts+1)
          cms=-crs(i,j,kts+1)*cf+(vvup*w2+1.)
          rcms(i,j,kts)=1./cms
          crs(i,j,kts)=0.
!
          do ks=kss,kse
            rsts(i,j,kts,ks)=-rsts(i,j,kts+1,ks)*cf+s1(i,j,kts,ks) &
                             -(s1(i,j,kts,ks)-s1(i,j,kts+1,ks))*vvup*w1
!
            tcs(i,j,kts,ks)=rsts(i,j,kts,ks)*rcms(i,j,kts)-s1(i,j,kts,ks)
          enddo
        enddo
      enddo
      do ks=kss,kse
        DO K=KTS+1,KTE
          DO J=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              tcs(i,j,k,ks)=(-crs(i,j,k)*(s1(i,j,k-1,ks)+tcs(i,j,k-1,ks)) &
                             +rsts(i,j,k,ks)) &
                           *rcms(i,j,k)-s1(i,j,k,ks)
            enddo
          enddo
        enddo
      enddo
!-----------------------------------------------------------------------
      do ks=kss,kse ! loop by species
!-----------------------------------------------------------------------
        DO K=KTS,KTE
          DO J=MYJS_P5,MYJE_P5
            DO I=MYIS_P5,MYIE_P5
              ss1(i,j)=s1(i,j,k,ks)*cfc+sp(i,j,k,ks)*bfc
              sp(i,j,k,ks)=s1(i,j,k,ks)
            enddo
          enddo
!---fluxes--------------------------------------------------------------
          DO J=MYJS1_P2,MYJE1_P2
            DO I=MYIS_P2,MYIE_P3
              ssx(i,j)=(ss1(i+ive(j),j  )-ss1(i+ivw(j),j  ))*few(i,j,k) &
                      *hbm2(i,j)
              ssy(i,j)=(ss1(i       ,j+1)-ss1(i       ,j-1))*fns(i,j,k) &
                      *hbm2(i,j)
            enddo
          enddo
          DO J=MYJS1_P2,MYJE2_P2
            DO I=MYIS_P2,MYIE_P2
              ssne(i,j)=(ss1(i+ihe(j),j+1)-ss1(i,j))*fne(i,j,k)*hbm2(i,j)
            enddo
          enddo
          DO J=MYJS2_P2,MYJE1_P2
            DO I=MYIS_P2,MYIE_P2
              ssse(i,j)=(ss1(i+ihe(j),j-1)-ss1(i,j))*fse(i,j,k)*hbm2(i,j)
            enddo
          enddo
!---advection of species------------------------------------------------
          DO J=MYJS5,MYJE5
            DO I=MYIS2,MYIE2
              tcs(i,j,k,ks)=((ssx (i+ihw(j),j  )+ssx (i+ihe(j),j  ) &
                             +ssy (i       ,j-1)+ssy (i       ,j+1) &
                             +ssne(i+ihw(j),j-1)+ssne(i       ,j  ) &
                             +ssse(i       ,j  )+ssse(i+ihw(j),j+1)) &
                            *fad(i,j)*2.0*idtad &   !! 2.0 compensates for fad
                            /(deta2(k)*pdop(i,j)+deta1_pdtop(k)) &
                           +tcs(i,j,k,ks))*hbm2(i,j)
            enddo
          enddo
!-----------------------------------------------------------------------
!
!***  upstream advection
!
!-----------------------------------------------------------------------
!
          upstream: IF(UPSTRM)THEN
!
!-----------------------------------------------------------------------
!***
!***  COMPUTE UPSTREAM COMPUTATIONS ON THIS TASK'S ROWS.
!***
!-----------------------------------------------------------------------
!
            jloop_upstream: DO J=MYJS2,MYJE2
!
              N_IUPH_J=N_IUP_H(J)   ! See explanation in START_DOMAIN_NMM
              DO II=0,N_IUPH_J-1
!
                I=IUP_H(IMS+II,J)
                tta=emt_loc(j) &
                   *(uold(i       ,j-1,k)+uold(i+ihw(j),j  ,k) &
                    +uold(i+ihe(j),j  ,k)+uold(i       ,j+1,k))
                ttb=ent &
                   *(vold(i       ,j-1,k)+vold(i+ihw(j),j  ,k) &
                    +vold(i+ihe(j),j  ,k)+vold(i       ,j+1,k))
                PP=-TTA-TTB
                QP= TTA-TTB
!
                IF(PP<0.)THEN
                  ISPA(I,J)=-1
                ELSE
                  ISPA(I,J)= 1
                ENDIF
!
                IF(QP<0.)THEN
                  ISQA(I,J)=-1
                ELSE
                  ISQA(I,J)= 1
                ENDIF
!
                PP=ABS(PP)
                QP=ABS(QP)
                ARRAY3_X=PP*QP
                ARRAY0(I,J)=ARRAY3_X-PP-QP
                ARRAY1(I,J)=PP-ARRAY3_X
                ARRAY2(I,J)=QP-ARRAY3_X
                ARRAY3(I,J)=ARRAY3_X
              ENDDO
!
!-----------------------------------------------------------------------
!
              N_IUPADH_J=N_IUP_ADH(J)
              KNTI_ADH=1
              IUP_ADH_J=IUP_ADH(IMS,J)
!
              iloop_T: DO II=0,N_IUPH_J-1
!
                I=IUP_H(IMS+II,J)
!
                ISP=ISPA(I,J)
                ISQ=ISQA(I,J)
                IFP=(ISP-1)/2
                IFQ=(-ISQ-1)/2
                IPQ=(ISP-ISQ)/2
!
!-----------------------------------------------------------------------
!
                IF(I==IUP_ADH_J)THEN  ! Upstream advection T tendencies
!
                  ISP=ISPA(I,J)
                  ISQ=ISQA(I,J)
                  IFP=(ISP-1)/2
                  IFQ=(-ISQ-1)/2
                  IPQ=(ISP-ISQ)/2
!
                  F0=ARRAY0(I,J)
                  F1=ARRAY1(I,J)
                  F2=ARRAY2(I,J)
                  F3=ARRAY3(I,J)
!
                  tcs(i,j,k,ks)=(f0*s1(i,j,k,ks) &
     &                          +f1*s1(i+ihe(j)+ifp,j+isp,k,ks) &
     &                          +f2*s1(i+ihe(j)+ifq,j+isq,k,ks) &
     &                          +f3*s1(i+ipq,j+isp+isq,k,ks))*2.0 &
     &                          *idtad &
     &                         +tcs(i,j,k,ks)*hbm2(i,j)
!
!-----------------------------------------------------------------------
!
                  IF(KNTI_ADH<N_IUPADH_J)THEN
                    IUP_ADH_J=IUP_ADH(IMS+KNTI_ADH,J)
                    KNTI_ADH=KNTI_ADH+1
                  ENDIF
!
                ENDIF  ! End of upstream advection tendency IF block
!
              ENDDO iloop_T
!
!-----------------------------------------------------------------------
            enddo jloop_upstream
!-----------------------------------------------------------------------
          endif upstream
!-----------------------------------------------------------------------
        enddo ! kts,kte
!-----------------------------------------------------------------------
      enddo ! end of the loop by the species
!-----------------------------------------------------------------------
                        endsubroutine adv2
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                        subroutine mono &
( &
#if defined(DM_PARALLEL)
 domdesc, &
#endif
 mype,ntsd,hours,kss,kse &
,ids,ide,jds,jde,kds,kde &
,ims,ime,jms,jme,kms,kme &
,its,ite,jts,jte,kts,kte &
,idtad &
,dy,pdtop &
,sumdrrw &
,ihe,ihw &
,deta1,deta2 &
,dx,hbm2,pd &
,s &
!---temporary arguments-------------------------------------------------
,s1,tcs)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
implicit none
!-----------------------------------------------------------------------
real,parameter:: &
 epsq=1.e-20 &               ! floor value for specific humidity
,epsq2=0.02                  ! floor value for 2tke

#ifdef DM_PARALLEL
      INTEGER :: DOMDESC
#endif

integer,intent(in):: &
 idtad &                     ! time step multiplier
,kse &                       ! terminal species index
,kss &                       ! initial species index
,mype &                      !
,ntsd &                      !
,ids,ide,jds,jde,kds,kde &
,ims,ime,jms,jme,kms,kme &
,its,ite,jts,jte,kts,kte

real,intent(in):: &
 dy &                        !
,pdtop &                     !
,hours                       !

real,intent(inout):: &
 sumdrrw

integer,dimension(jms:jme),intent(in):: &
 ihe,ihw

real,dimension(kts:kte),intent(in):: &
 deta1 &                     ! delta sigmas
,deta2                       ! delta sigmas

real,dimension(ims:ime,jms:jme),intent(in):: &
 dx &                        !
,hbm2 &                      !
,pd                          ! sigma range pressure difference

real,dimension(ims:ime,jms:jme,kms:kme,kss:kse),intent(in):: &
 s                           ! s
!---temporary arguments-------------------------------------------------
real,dimension(ims:ime,jms:jme,kms:kme,kss:kse),intent(inout):: &
 s1 &                        ! intermediate value of s
,tcs                         ! timechange of s
!--local variables------------------------------------------------------
integer:: &
 i &                         !
,ierr &                      !
,irecv &                     !
,j &                         !
,k &                         !
,ks &                        !
,lngth &                     !
,mpi_comm_comp               !

real:: &
 dsks &                      !
,dvolp &                     !
,rfacs &                     !
,s1p &                       !
,sfacs &                     !
,smax &                      ! local maximum
,smin &                      ! local minimum
,smaxh &                     ! horizontal local maximum
,sminh &                     ! horizontal local minimum
,smaxv &                     ! vertical local maximum
,sminv &                     ! vertical local minimum
,sn &                        !
,sumns &                     !
,sumps &                     !
,steep

double precision:: &
 xsmp,gsmp

double precision,dimension(2*kss-1:2*kse):: &
 vgsms

real,dimension(kts:kte):: &
 deta1_pdtop                 !

real,dimension(2*kss-1:2*kse,kts:kte):: &
 gsms_single                 !

double precision,dimension(2*kss-1:2*kse,kts:kte):: &
 gsms &                      ! sum of neg/pos changes all global fields
,xsms                        ! sum of neg/pos changes all local fields

double precision,dimension(2*kss-1:2*kse):: &
 vgsums                      !

real,dimension(its-5:ite+5,jts-5:jte+5,kts:kte):: &
 dvol &                      ! grid box volume
,rdvol                       ! 1./grid box volume

logical, save :: first=.true.
real, save :: sumrrw_first, summass_first
real :: summass
double precision, save :: gsmp_first
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!     steep=1.-0.040*idtad
      steep=1.

      DO K=KTS,KTE
        deta1_pdtop(k)=deta1(k)*pdtop
      enddo
!
      DO K=KTS,KTE
        DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            dvolp=(deta2(k)*pd(i,j)+deta1_pdtop(k))*dx(i,j)*dy
            rdvol(i,j,k)=hbm2(i,j)/dvolp
            dvol (i,j,k)=hbm2(i,j)*dvolp
          enddo
        enddo
      enddo

!      go to 109
!---monotonization------------------------------------------------------
      do ks=kss,kse ! loop by species
!-----------------------------------------------------------------------
        DO K=KTS,KTE
          xsms(2*ks-1,k)=0.
          xsms(2*ks  ,k)=0.
          gsms(2*ks-1,k)=0.
          gsms(2*ks  ,k)=0.
!
          DO J=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
!
              s1p=(s1(i,j,k,ks)+tcs(i,j,k,ks))**2
              tcs(i,j,k,ks)=s1p-s(i,j,k,ks)
!
              sminh=min(s(i       ,j-2,k,ks) &
                       ,s(i+ihw(j),j-1,k,ks) &
                       ,s(i+ihe(j),j-1,k,ks) &
                       ,s(i-1     ,j  ,k,ks) &
                       ,s(i       ,j  ,k,ks) &
                       ,s(i+1     ,j  ,k,ks) &
                       ,s(i+ihw(j),j+1,k,ks) &
                       ,s(i+ihe(j),j+1,k,ks) &
                       ,s(i       ,j+2,k,ks))
              smaxh=max(s(i       ,j-2,k,ks) &
                       ,s(i+ihw(j),j-1,k,ks) &
                       ,s(i+ihe(j),j-1,k,ks) &
                       ,s(i-1     ,j  ,k,ks) &
                       ,s(i       ,j  ,k,ks) &
                       ,s(i+1     ,j  ,k,ks) &
                       ,s(i+ihw(j),j+1,k,ks) &
                       ,s(i+ihe(j),j+1,k,ks) &
                       ,s(i       ,j+2,k,ks))
!
              if(k.gt.kts.and.k.lt.kte) then
                sminv=min(s(i,j,k-1,ks),s(i,j,k  ,ks),s(i,j,k+1,ks))
                smaxv=max(s(i,j,k-1,ks),s(i,j,k  ,ks),s(i,j,k+1,ks))
              elseif(k.eq.kts) then
                sminv=min(s(i,j,k  ,ks),s(i,j,k+1,ks))
                smaxv=max(s(i,j,k  ,ks),s(i,j,k+1,ks))
              elseif(k.eq.kte) then
                sminv=min(s(i,j,k-1,ks),s(i,j,k  ,ks))
                smaxv=max(s(i,j,k-1,ks),s(i,j,k  ,ks))
              endif
!
              smin=min(sminh,sminv)
              smax=max(smaxh,smaxv)
!
              sn=s1p
              if(sn.gt.steep*smax) sn=smax
              if(sn.lt.      smin) sn=smin
!
              dsks=(sn-s1p)*dvol(i,j,k)
              s1(i,j,k,ks)=dsks
              if(dsks.gt.0.) then
                xsms(2*ks-1,k)=xsms(2*ks-1,k)+dsks
              else
                xsms(2*ks  ,k)=xsms(2*ks  ,k)+dsks
              endif
!
            enddo
          enddo
        enddo
!-----------------------------------------------------------------------
      enddo ! end of the loop by species
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***  GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
# if defined(DM_PARALLEL) && !defined(STUBMPI)
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      lngth=(2*kse-2*kss+2)*(KTE-KTS+1)
      CALL MPI_ALLREDUCE(XSMS,GSMS,lngth                              &
     &                  ,MPI_DOUBLE_PRECISION,MPI_SUM                   &
     &                  ,MPI_COMM_COMP,IRECV)
# else
      DO K=KTS,KTE
        do ks=kss,kse
          gsms(2*ks-1,k)=xsms(2*ks-1,k)
          gsms(2*ks  ,k)=xsms(2*ks  ,k)
        enddo
      enddo
# endif
!
      DO K=KTS,KTE
        do ks=kss,kse
          gsms_single(2*ks-1,k)=gsms(2*ks-1,k)
          gsms_single(2*ks  ,k)=gsms(2*ks  ,k)
        enddo
      enddo
!
!-----------------------------------------------------------------------
!***  END OF GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
!dusan!---forced conservation after monotonization----------------------------
!dusan      do ks=kss,kse
!dusan        DO K=KTS,KTE
!dusan          sumps=gsms_single(2*ks-1,k)
!dusan          sumns=gsms_single(2*ks  ,k)
!dusan!
!dusan          if(sumps*(-sumns).gt.1.) then
!dusan            sfacs=-sumns/sumps
!dusan            rfacs=1./sfacs
!dusan          else
!dusan            sfacs=0.
!dusan            rfacs=0.
!dusan          endif
!dusan!
!dusan          DO J=MYJS2,MYJE2
!dusan            DO I=MYIS1,MYIE1
!dusan              dsks=s1(i,j,k,ks)*rdvol(i,j,k)
!dusan              if(sfacs.gt.1.) then
!dusan                if(dsks.lt.0.) dsks=dsks*rfacs
!dusan!zjtest                if(dsks.lt.0.) dsks=dsks
!dusan              else
!dusan                if(dsks.ge.0.) dsks=dsks*sfacs
!dusan              endif
!dusan              tcs(i,j,k,ks)=tcs(i,j,k,ks)+dsks
!dusan            enddo
!dusan          enddo
!dusan        enddo
!dusan!-----------------------------------------------------------------------
!dusan      enddo ! end of the loop by species

!---forced conservation after monotonization----------------------------
      do ks=kss,kse
        vgsums(2*ks-1)=0.
        vgsums(2*ks  )=0.
        DO K=KTS,KTE
          vgsums(2*ks-1)=gsms(2*ks-1,k)+vgsums(2*ks-1)
          vgsums(2*ks  )=gsms(2*ks  ,k)+vgsums(2*ks  )
        enddo
      enddo

      do ks=kss,kse
        sumps=vgsums(2*ks-1)
        sumns=vgsums(2*ks  )
!
        if(sumps*(-sumns).gt.1.) then
          sfacs=-sumns/sumps
          rfacs=1./sfacs
        else
          sfacs=0.
          rfacs=0.
        endif
!
        DO K=KTS,KTE
          DO J=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              dsks=s1(i,j,k,ks)*rdvol(i,j,k)
              if(sfacs.lt.1.) then
                if(dsks.gt.0.) dsks=dsks*sfacs
              endif
              tcs(i,j,k,ks)=tcs(i,j,k,ks)+dsks
            enddo
          enddo
        enddo
!-----------------------------------------------------------------------
      enddo ! end of the loop by species
109   continue
!-----------------------------------------------------------------------
!zjwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
!-----------------------------------------------------------------------
      do ks=kss,kse ! loop by species
        DO K=KTS,KTE
          xsms(2*ks-1,k)=0.
          xsms(2*ks  ,k)=0.
          gsms(2*ks-1,k)=0.
          gsms(2*ks  ,k)=0.
!
          DO J=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              xsms(2*ks-1,k)=xsms(2*ks-1,k)+s  (i,j,k,ks)*dvol(i,j,k)
              xsms(2*ks  ,k)=xsms(2*ks  ,k)+tcs(i,j,k,ks)*dvol(i,j,k)
            enddo
          enddo
        enddo
      enddo
!
      xsmp=0.
      DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          xsmp=pd(i,j)*dx(i,j)*dy*hbm2(i,j)+xsmp
        enddo
      enddo
!
!-----------------------------------------------------------------------
!***  GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
# if defined(DM_PARALLEL) && !defined(STUBMPI)
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      lngth=1
      CALL MPI_ALLREDUCE(xsmp,gsmp,lngth                                &
     &                  ,MPI_DOUBLE_PRECISION,MPI_SUM                   &
     &                  ,MPI_COMM_COMP,IRECV)
# else
      gsmp=xsmp
# endif
!
!-----------------------------------------------------------------------
!***  END OF GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!***  GLOBAL REDUCTION
!-----------------------------------------------------------------------
!
# if defined(DM_PARALLEL) && !defined(STUBMPI)
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      lngth=(2*kse-2*kss+2)*(KTE-KTS+1)
      CALL MPI_ALLREDUCE(XSMS,GSMS,lngth                                &
     &                  ,MPI_DOUBLE_PRECISION,MPI_SUM                   &
     &                  ,MPI_COMM_COMP,IRECV)
# else
      DO K=KTS,KTE
        do ks=kss,kse
          gsms(2*ks-1,k)=xsms(2*ks-1,k)
          gsms(2*ks  ,k)=xsms(2*ks  ,k)
        enddo
      enddo
# endif
!
      DO K=KTS,KTE
        do ks=kss,kse
          gsms_single(2*ks-1,k)=gsms(2*ks-1,k)
          gsms_single(2*ks  ,k)=gsms(2*ks  ,k)
        enddo
      enddo
!
!-----------------------------------------------------------------------
!***  END OF GLOBAL REDUCTION
!-----------------------------------------------------------------------
!

      do ks=kss,kse
        vgsms(2*ks-1)=0.
        vgsms(2*ks  )=0.
      enddo
!
      do ks=kss,kse
        DO K=KTS,KTE
          vgsms(2*ks-1)=gsms(2*ks-1,k)+vgsms(2*ks-1)
          vgsms(2*ks  )=gsms(2*ks  ,k)+vgsms(2*ks  )
        enddo
      enddo
!
      sumdrrw=vgsms(6)+sumdrrw
!
      summass=0.0
      DO K=KTS,KTE
        DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            summass=summass+dvol(i,j,k)
          ENDDO
        ENDDO
      ENDDO
!
      if (first) then
         sumrrw_first = vgsms(5)
         gsmp_first = gsmp
         summass_first = summass
         first=.false.
      end if
!
!     write(0,1000) ntsd,hours, &                                       ! 4,5
!                   (vgsms(ks),ks=2*kss-1,2*kse), &                     ! 6-13
!                   gsmp,gsmp_first,gsmp/gsmp_first, &                  ! 14-16
!                   sumdrrw, &                                          ! 17
!                   vgsms(5)/sumrrw_first,sumrrw_first, &               ! 18-19
!                   summass,summass_first,summass/summass_first         ! 20-22
 1000 format('global vol sums ',i6,f8.3,30d13.5)
!zjmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
                        endsubroutine mono
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      END MODULE MODULE_ADVECTION
!
!-----------------------------------------------------------------------
