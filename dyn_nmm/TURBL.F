!NCEP_MESO:MODEL_LAYER: PHYSICS
!
!***********************************************************************
      SUBROUTINE TURBL(GRID                                             &
     &                ,NTSD,DT,NPHS,RESTRT                              &
     &                ,N_MOIST,NSOIL,SLDPTH,DZSOIL                      &
     &                ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT       &
     &                ,SM,LMH,HTM,VTM,HBM2,VBM2,DX,DFRLG                &
     &                ,CZEN,CZMEAN,SIGT4,RLWIN,RSWIN,RADOT              &
     &                ,PD,RES,PINT,T,Q,CWM,F_ICE,F_RAIN,Q2,U,V          &
     &                ,THS,SST,PREC,SNO,ZERO_3D                         &
     &                ,FIS,Z0,Z0BASE,USTAR,PBLH,LPBL,AKHS,AKMS          &
     &                ,THZ0,QZ0,UZ0,VZ0,QS                              &
     &                ,STC,SMC,CMC,SMSTAV,SMSTOT,SSROFF,BGROFF          &
     &                ,IVGTYP,ISLTYP,VEGFRA,SHDMIN,SHDMAX,GRNFLX        &
     &                ,SFCEXC,ACSNOW,ACSNOM,SNOPCX,SICE,TG,SOILTB       &
     &                ,ALBASE,MXSNAL,ALBEDO,SH2O,SI,EPSR                &
     &                ,U10,V10,TH10,Q10,TSHLTR,QSHLTR,PSHLTR            &
     &                ,TWBS,QWBS,SFCSHX,SFCLHX,SFCEVP                   &
     &                ,POTEVP,POTFLX,SUBSHX                             &
     &                ,APHTIM,ARDSW,ARDLW,ASRFC                         &
     &                ,RSWOUT,RSWTOA,RLWTOA                             &
     &                ,ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA          &
     &                ,UZ0H,VZ0H,DUDT,DVDT                              & !jm
     &                ,PSURF                                            & ! PSFC in caller
     &                ,CONFIG_FLAGS                                     &
     &                ,IHE,IHW,IVE,IVW                                  &
     &                ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!***********************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    TURBL       TURBULENCE OUTER DRIVER
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 02-04-19       
!     
! ABSTRACT:
!     TURBL DRIVES THE TURBULENCE SCHEMES
!     
! PROGRAM HISTORY LOG (with changes to called routines) :
!   95-03-15  JANJIC     - ORIGINATOR OF THE SUBROUTINES CALLED
!   BLACK & JANJIC       - ORIGINATORS OF THE DRIVER
!   95-03-28  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   96-03-29  BLACK      - ADDED EXTERNAL EDGE; REMOVED SCRCH COMMON
!   96-07-19  MESINGER   - ADDED Z0 EFFECTIVE
!   98-??-??  TUCCILLO   - MODIFIED FOR CLASS VIII PARALLELISM
!   98-10-27  BLACK      - PARALLEL CHANGES INTO MOST RECENT CODE
!   02-01-10  JANJIC     - MOIST TURBULENCE (DRIVER, MIXLEN, VDIFH)
!   02-01-10  JANJIC     - VERT. DIF OF Q2 INCREASED (Grenier & Bretherton)
!   02-02-02  JANJIC     - NEW SFCDIF
!   02-04-19  BLACK      - ORIGINATOR OF THIS OUTER DRIVER FOR WRF
!   02-05-03  JANJIC     - REMOVAL OF SUPERSATURATION AT 2m AND 10m
!     
! USAGE: CALL TURBL FROM SOLVE_RUNSTREAM
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!$$$  
!-----------------------------------------------------------------------
      USE MODULE_DOMAIN
      USE MODULE_DM
      USE MODULE_CONFIGURE
      USE MODULE_STATE_DESCRIPTION,ONLY : P_QV,P_QC
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_MPP
      USE MODULE_SF_MYJSFC
      USE MODULE_SURFACE_DRIVER
      USE MODULE_PBL_DRIVER
!!!   USE MODULE_TIMERS
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      TYPE(DOMAIN) :: GRID
!
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                     &
     &                     ,IMS,IME,JMS,JME,KMS,KME                     &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                     &
     &                     ,N_MOIST,NPHS,NSOIL,NTSD
!
      INTEGER, DIMENSION(JMS:JME),INTENT(IN) :: IHE,IHW,IVE,IVW
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: ISLTYP,IVGTYP    &
     &                                                ,LMH
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: LPBL
!
      REAL,INTENT(IN) :: DT,DX,PDTOP,PT
!
      REAL,INTENT(INOUT) :: APHTIM,ARDSW,ARDLW,ASRFC
!
      REAL,DIMENSION(KMS:KME-1),INTENT(IN) :: AETA1,AETA2,DETA1,DETA2
!
      REAL,DIMENSION(KMS:KME),INTENT(IN) :: DFRLG,ETA1,ETA2
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ALBASE,MXSNAL
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) :: CZEN,CZMEAN         &
     &                                             ,EPSR,FIS,HBM2       &
     &                                             ,PD,RES              &
     &                                             ,RLWIN,RLWTOA        &
     &                                             ,RSWIN,RSWOUT,RSWTOA &
     &                                             ,SHDMIN,SHDMAX       &
     &                                             ,SICE,SIGT4,SM,SST   &
     &                                             ,TG,VBM2,VEGFRA
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: GRNFLX,QWBS,RADOT  &
                                                    ,SFCEXC,SMSTAV      &
                                                    ,SMSTOT,SOILTB,TWBS
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: ACSNOM,ACSNOW    &
     &                                                ,AKHS,AKMS        &
     &                                                ,ALBEDO           &
     &                                                ,BGROFF,CMC       &
     &                                                ,PBLH,POTEVP      &
     &                                                ,POTFLX,PREC      &
     &                                                ,QS,QZ0,SFCEVP    &
     &                                                ,SFCLHX,SFCSHX    &
     &                                                ,SH2O,SI          &
     &                                                ,SNO,SNOPCX       &
     &                                                ,SSROFF,SUBSHX    &
     &                                                ,THS,THZ0         &
     &                                                ,USTAR,UZ0,UZ0H   &
     &                                                ,VZ0,VZ0H         &
     &                                                ,Z0,Z0BASE        &
     &                                                ,PSURF
!
      REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(OUT) :: ALWIN,ALWOUT       &
     &                                              ,ALWTOA,ASWIN       &
     &                                              ,ASWOUT,ASWTOA      &
     &                                              ,PSHLTR,Q10,QSHLTR  &
     &                                              ,TH10,TSHLTR        &
     &                                              ,U10,V10
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: CWM      &
     &                                                        ,DUDT     &
     &                                                        ,DVDT     &
     &                                                        ,F_ICE    &
     &                                                        ,F_RAIN   &
     &                                                        ,Q,Q2     &
     &                                                        ,T,U,V
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: HTM,VTM
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: PINT
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(INOUT) :: ZERO_3D
!
      REAL,DIMENSION(NSOIL),INTENT(IN) :: DZSOIL,SLDPTH
!
      REAL,DIMENSION(IMS:IME,NSOIL,JMS:JME),INTENT(INOUT) :: SMC,STC
!
      LOGICAL,INTENT(IN) :: RESTRT
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!***
!***  LOCAL VARIABLES
!***
#ifdef DM_PARALLEL
      INTEGER :: IPS,IPE,JPS,JPE,KPS,KPE
#endif
#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif
#define  DATA_CALLS_INCLUDED
      INTEGER :: I,IDUMMY,IEND,ISFFLX,ISTR,J,K,KOUNT_ALL,LENGTH_ROW     &
     &          ,LLIJ,LLMH,LLYR,N
!
      INTEGER,DIMENSION(1) :: I_START,I_END,J_START,J_END
!
      INTEGER,DIMENSION(IMS:IME,JMS:JME) :: KPBL,LOWLYR
!
      REAL :: ALTITUDE,CWML,DQDT,DTDT,DTPHS,FACTR,FACTRL,G_INV          &
     &       ,PDSL,PLYR,PSFC,QI,QL,QOLD,QR,QW,RATIOMX,RDTPHS,ROG        &
     &       ,RWMSK,SDEPTH,TL,TLMH,TLMH4,TNEW,TSFC2,U_FRAME,V_FRAME     &
     &       ,WMSK,XLVRW
      REAL :: APES,CKLQ,FACTOR,FFS,PQ0X,Q2SAT,QFC1,QLOWX,QSX,RLIVWV     &
     &       ,THBOT
!
      REAL :: TRESH=0.95
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: EXNSFC,FACTRS,ONE,PLM,Q2X      &
     &                                  ,RAIN,RAINBL                    &
     &                                  ,RLW_DN_SFC,RSW_DN_SFC          &
     &                                  ,SFCZ,SNOW,SNOWC,SNOWH          &
     &                                  ,TH2X,TSFC,VGFRCK               &
     &                                  ,XLAND,ZERO_2D
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: CWMLOW,QLOW,SFCEVPx,THLOW,TLOW
!
      REAL,DIMENSION(IMS:IME,JMS:JME) :: BR,CHKLOWQ,CT,ELFLX,FLHC,FLQC  &
     &                                  ,GZ1OZ0,PSIH,PSIM,WSPD
!
      REAL,DIMENSION(IMS:IME,KMS:KME-1,JMS:JME) :: EXNER
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME) :: DZ,P8W,P_PHY,PI_PHY    &
     &                                          ,RQCBLTEN,RQIBLTEN      &
     &                                          ,RQVBLTEN,RR,RTHBLTEN   &
     &                                          ,T_PHY,TH_PHY,TKE       &
     &                                          ,U_PHY,V_PHY,Z
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,N_MOIST) :: WATER
!
      REAL,DIMENSION(IMS:IME,NSOIL,JMS:JME) :: ZERO_SOIL
!
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: TOLD
!
      LOGICAL :: E_BDY,WARM_RAIN
      CHARACTER*256 :: errmess
!
!-----------------------------------------------------------------------
#include "deref_kludge.h"
!-----------------------------------------------------------------------
      WARM_RAIN = .FALSE.

      MYIS    =MAX(IDS  ,ITS  )
      MYIE    =MIN(IDE  ,ITE  )
      MYJS    =MAX(JDS  ,JTS  )
      MYJE    =MIN(JDE  ,JTE  )
!
      MYIS1   =MAX(IDS+1,ITS  )
      MYIE1   =MIN(IDE-1,ITE  )
      MYJS1   =MAX(JDS+1,JTS  )
      MYJE1   =MIN(JDE-1,JTE  )
      MYJS2   =MAX(JDS+2,JTS  )
      MYJE2   =MIN(JDE-2,JTE  )
!
      MYIS_P1 =MAX(IDS  ,ITS-1)
      MYIE_P1 =MIN(IDE  ,ITE+1)
!
      MYJS1_P1=MAX(JDS+1,JTS-1)
      MYJE1_P1=MIN(JDE-1,JTE+1)
      MYJS2_P1=MAX(JDS+2,JTS-1)
      MYJE2_P1=MIN(JDE-2,JTE+1)
!
      I_START(1)=MYIS
      I_END(1)  =MYIE
      J_START(1)=MYJS
      J_END(1)  =MYJE
!
      DTPHS=NPHS*DT
      RDTPHS=1./DTPHS
      G_INV=1./G
      ROG=R_D*G_INV
      FACTOR=-XLV*RHOWATER/DTPHS
!
      U_FRAME=0.
      V_FRAME=0.
!
      IDUMMY=0
      ISFFLX=1
!
      DO J=JMS,JME
      DO I=IMS,IME
        UZ0H(I,J)=0.
        VZ0H(I,J)=0.
        ONE(I,J)=1.
      ENDDO
      ENDDO
!
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        LOWLYR(I,J)=1
        VGFRCK(I,J)=100.*VEGFRA(I,J)
        SNOW(I,J)=SNO(I,J)
        SNOWH(I,J)=SI(I,J)
        XLAND(I,J)=SM(I,J)+1.
      ENDDO
      ENDDO
!
      IF(NTSD.EQ.0)THEN
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          SNOW(I,J)=SNO(I,J)*1000.
          SNOWH(I,J)=SI(I,J)*1000.
          Z0BASE(I,J)=Z0(I,J)
        ENDDO
        ENDDO
      ENDIF
!
      DO J=MYJS,MYJE
      DO K=KTS,KTE+1
      DO I=MYIS,MYIE
        Z(I,K,J)=0.
        DZ(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO

!
!-----------------------------------------------------------------------
!
!***  PREPARE NEEDED ARRAYS
!
!-----------------------------------------------------------------------
!
      ALLOCATE(TOLD(IMS:IME,KMS:KME-1,JMS:JME),STAT=I)
!
!     DO J=MYJS2,MYJE2
!     DO I=MYIS1,MYIE1
      SNOWC = 0.
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
!
        LLMH=LMH(I,J)
        PDSL=PD(I,J)*RES(I,J)
!!!     PSFC=PD(I,J)+PDTOP+PT
!!!     P8W(I,KTS,J)=PSFC
        P8W(I,KTS,J)=PINT(I,KTS,J)
        PSFC=PINT(I,KTS,J)
        LOWLYR(I,J)=KTE+1-LLMH
        EXNSFC(I,J)=(1.E5/PSFC)**CAPA
        THS(I,J)=(SST(I,J)*EXNSFC(I,J))*SM(I,J)+THS(I,J)*(1.-SM(I,J))
        TSFC(I,J)=THS(I,J)/EXNSFC(I,J)
        SFCZ(I,J)=FIS(I,J)*G_INV
        ZERO_2D(I,J)=0.
        RAIN(I,J)=PREC(I,J)*RHOWATER
        RAINBL(I,J)=0.
        IF(SNO(I,J).GT.0.)SNOWC(I,J)=1.
        LLIJ=LOWLYR(I,J)
        PLM(I,J)=(PINT(I,LLIJ,J)+PINT(I,LLIJ+1,J))*0.5
        TH2X(I,J)=T(I,LLIJ,J)*(1.E5/PLM(I,J))**CAPA
        Q2X(I,J)=Q(I,LLIJ,J)
!
!*** LONG AND SHORTWAVE FLUX AT GROUND SURFACE
!
        IF(CZMEAN(I,J).GT.0.)THEN
          FACTRS(I,J)=CZEN(I,J)/CZMEAN(I,J)
        ELSE
          FACTRS(I,J)=0.
        ENDIF
!
        IF(SIGT4(I,J).GT.0.)THEN
          TLMH=T(I,LLIJ,J)
          TLMH4=TLMH**4
          FACTRL=STBOLT*TLMH4/SIGT4(I,J)
        ELSE
          FACTRL=0.
        ENDIF
!     
        RLW_DN_SFC(I,J)=RLWIN(I,J)*HBM2(I,J)*FACTRL
        RSW_DN_SFC(I,J)=RSWIN(I,J)*HBM2(I,J)*FACTRS(I,J)
!
!***  FILL THE ARRAYS FOR CALLING THE INNER DRIVER.
!
        Z(I,KTS,J)=SFCZ(I,J)
!
        DO K=KTS,KTE
          Q2(I,K,J)=AMAX1(Q2(I,K,J)*HBM2(I,J),EPSQ2)
          QL=AMAX1(Q(I,K,J),EPSQ)
          PLYR=(PINT(I,K,J)+PINT(I,K+1,J))*0.5
!!!       PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL+PT
          TL=T(I,K,J)
          CWML=CWM(I,K,J)
!
          RR(I,K,J)=PLYR/(R_D*TL)
          T_PHY(I,K,J)=TL
          TOLD(I,K,J)=TL
!
!***  WATER VAPOR, CLOUD LIQUID AND ICE
!
          WATER(I,K,J,P_QV)=QL/(1.-QL) !Need to pass mixing ratio
!
          QW=0.
          QI=0.
          QR=0.
          IF(F_ICE(I,K,J)>=1.)THEN
            QI=CWML
          ELSEIF(F_ICE(I,K,J)<=0.)THEN
            QW=CWML
          ELSE
            QI=F_ICE(I,K,J)*CWML
            QW=CWML-QI
          ENDIF
          IF(QW>0..AND.F_RAIN(I,K,J)>0.)THEN
            IF(F_RAIN(I,K,J)>=1.)THEN
              QR=QW
              QW=0.
            ELSE
              QR=F_RAIN(I,K,J)*QW
              QW=QW-QR
            ENDIF
          ENDIF 
          WATER(I,K,J,P_QC)=QW
          WATER(I,K,J,P_QI)=QI
          WATER(I,K,J,P_QR)=QR
!
          EXNER(I,K,J)=(1.E5/PLYR)**CAPA
          PI_PHY(I,K,J)=1./EXNER(I,K,J)
          TH_PHY(I,K,J)=TL*EXNER(I,K,J)
          P8W(I,K+1,J)=PINT(I,K+1,J)
!!!       P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL+PT
          P_PHY(I,K,J)=PLYR
          TKE(I,K,J)=0.5*Q2(I,K,J)
!
          RTHBLTEN(I,K,J)=0.
          RQVBLTEN(I,K,J)=0.
          RQCBLTEN(I,K,J)=0.
          RQIBLTEN(I,K,J)=0.
!
          Z(I,K+1,J)=Z(I,K,J)+TL/PLYR                                   &
     &                  *(DETA1(K)*PDTOP+DETA2(K)*PDSL)*ROG             &
                        *(Q(I,K,J)*P608-CWML+1.)
          Z(I,K+1,J)=(Z(I,K+1,J)-DFRLG(K+1))*HTM(I,K,J)+DFRLG(K+1)
!!!       FACTR=1.-HTM(I,K,J)
!!!       Z(I,K+1,J)=Z(I,K+1,J)*HTM(I,K,J)+FACTR*DFRLG(K+1)
          DZ(I,K,J)=Z(I,K+1,J)-Z(I,K,J)
        ENDDO
      ENDDO
      ENDDO
!
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        TWBS(I,J)=0.
        QWBS(I,J)=0.
        LLYR=LOWLYR(I,J)
        THLOW(I,J)=TH_PHY(I,LLYR,J)
        TLOW(I,J)=T_PHY(I,LLYR,J)
        QLOW(I,J)=MAX(Q(I,LLYR,J),EPSQ)
        QLOWX=QLOW(I,J)/(1.-QLOW(I,J))
        QLOW(I,J)=QLOWX/(1.+QLOWX)
        CWMLOW(I,J)=CWM(I,LLYR,J)
        PBLH(I,J)=MAX(PBLH(I,J),0.)
        PBLH(I,J)=MIN(PBLH(I,J),Z(I,KTE,J))
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  COMPUTE VELOCITY COMPONENTS AT MASS POINTS
!
!-----------------------------------------------------------------------
      DO J=MYJS1_P1,MYJE1_P1
!
        DO K=KTS,KTE
          DO I=MYIS_P1,MYIE_P1
            WMSK=VTM(I+IHE(J),K,J)+VTM(I+IHW(J),K,J)                    &
     &          +VTM(I,K,J+1)+VTM(I,K,J-1)
            IF(WMSK.GT.0.)THEN
              RWMSK=1./WMSK
              U_PHY(I,K,J)=(U(I+IHE(J),K,J)*VTM(I+IHE(J),K,J)           &
     &                         +U(I+IHW(J),K,J)*VTM(I+IHW(J),K,J)       &
     &                         +U(I,K,J+1)*VTM(I,K,J+1)                 &
     &                         +U(I,K,J-1)*VTM(I,K,J-1))*RWMSK
              V_PHY(I,K,J)=(V(I+IHE(J),K,J)*VTM(I+IHE(J),K,J)           &
     &                         +V(I+IHW(J),K,J)*VTM(I+IHW(J),K,J)       &
     &                         +V(I,K,J+1)*VTM(I,K,J+1)                 &
     &                         +V(I,K,J-1)*VTM(I,K,J-1))*RWMSK
            ELSE
              U_PHY(I,K,J)=0.
              V_PHY(I,K,J)=0.
            ENDIF
          ENDDO
        ENDDO
      ENDDO
!
      DO J=MYJS1_P1,MYJE1_P1
      IF(MOD(J,2).EQ.0)THEN
        ISTR=MYIS_P1
        IEND=MIN(MYIE_P1,IDE-1)
      ELSE
        ISTR=MAX(MYIS_P1,IDS+1)
        IEND=MIN(MYIE_P1,IDE-1)
      ENDIF
!     DO I=MYIS_P1,MYIE_P1
      DO I=ISTR,IEND
        UZ0H(I,J)=(UZ0(I+IHE(J),J)+UZ0(I+IHW(J),J)                      &
     &            +UZ0(I,J+1)+UZ0(I,J-1))*0.25
!!!  &            +UZ0(I,J+1)+UZ0(I,J-1))*HBM2(I,J)*0.25
        VZ0H(I,J)=(VZ0(I+IHE(J),J)+VZ0(I+IHW(J),J)                      &
     &            +VZ0(I,J+1)+VZ0(I,J-1))*0.25
!!!  &            +VZ0(I,J+1)+VZ0(I,J-1))*HBM2(I,J)*0.25
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  CALL SURFACE LAYER AND LAND SURFACE PHYSICS
!
!-----------------------------------------------------------------------
!

      ZERO_SOIL = 0.
      WSPD = 0.
      BR = 0.
      CHKLOWQ = 0.
      GZ1OZ0 = 0.
      PSIH = 0.
      PSIM = 0.
      SFCEVPX = 0.

      CALL SURFACE_DRIVER(                                              &
     &           ACSNOM,ACSNOW,AKHS,AKMS,ALBEDO,BR,CMC,ZERO_2D          &
     &          ,CHKLOWQ,CONFIG_FLAGS,DT,DX,DZ,SLDPTH,ZERO_2D           &
     &          ,RLW_DN_SFC,GRNFLX,RSW_DN_SFC,GZ1OZ0,TWBS,ZERO_2D       &
     &          ,SFCZ,IDUMMY,ISFFLX,ISLTYP,NTSD,IVGTYP,LOWLYR           &
     &          ,ONE,WATER,ZERO_2D,ZERO_2D,NSOIL,N_MOIST,P8W,PBLH       & ! added ZERO_2D for RMOL, JM
     &          ,PI_PHY,PSHLTR,PSIH,PSIM,P_PHY,Q10,Q2X,QWBS,QS,QSHLTR   &
     &          ,QZ0,RAINBL,RAIN,ZERO_2D,ZERO_2D,RR,SFCEVPX             &
     &          ,SFCEXC,SSROFF,SMC,SMSTAV,SMSTOT,MXSNAL,SNOW,SNOWC      &
     &          ,SNOWH,NPHS,ZERO_2D,TH10,TH2X,ZERO_2D,THZ0,TH_PHY       &
     &          ,TG,TSHLTR,TSFC,STC,T_PHY,U10,BGROFF,USTAR,UZ0H         &
     &          ,U_FRAME,U_PHY,V10,VGFRCK,VZ0H,V_FRAME,V_PHY            &
     &          ,WARM_RAIN,WSPD,SICE,XLAND,Z,Z0,DZSOIL                  &
     &          ,CT,TKE                                                 &
     &          ,ALBASE,ELFLX,SH2O,SHDMAX,SHDMIN,Z0BASE                 &
     &          ,FLQC,FLHC,ZERO_2D,ZERO_2D,ZERO_2D,ZERO_2D,ZERO_2D      & ! RUC LSM
     &          ,ZERO_SOIL,ZERO_SOIL                                    &
     &          ,PSURF                                                  & ! added for v2 integration
     &          ,IDS,IDE,JDS,JDE,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,I_START,MIN(I_END,IDE-1)                               &
     &          ,MAX(J_START,2),MIN(J_END,JDE-1)                        &
     &          ,KTS,KTE,1                                              &
     &          ,POTEVP=POTEVP,SNOPCX=SNOPCX,SOILTB=SOILTB              ) ! Optional args for NMM LSM
!
!-----------------------------------------------------------------------
!
!***  CALL FREE ATMOSPHERE TURBULENCE
!
!-----------------------------------------------------------------------
!
      DO J=JMS,JME
      DO K=KMS,KME
      DO I=IMS,IME
        DUDT(I,K,J)=0.
        DVDT(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!

      CALL PBL_DRIVER(NTSD,DT,U_FRAME,V_FRAME                           &
     &               ,DUDT,DVDT,RTHBLTEN                                &
     &               ,RQVBLTEN,RQCBLTEN,RQIBLTEN                        &
     &               ,TSFC,XLAND,Z0,SFCZ                                &
     &               ,USTAR,ZERO_2D,ZERO_2D,PBLH                        &
     &               ,TWBS,QWBS,ZERO_2D,GRNFLX                          &
     &               ,U_PHY,V_PHY,TH_PHY,RR,WATER                       &
     &               ,P_PHY,PI_PHY,P8W,T_PHY,DZ,Z                       &
     &               ,TKE,ZERO_3D,AKHS,AKMS                             & ! ZERO_3D for EXCH_H
     &               ,THZ0,QZ0,UZ0H,VZ0H,QS,LOWLYR                      &
     &               ,PSIM,PSIH,GZ1OZ0,WSPD,BR,CHKLOWQ                  &
     &               ,CONFIG_FLAGS,DX,N_MOIST                           &
     &               ,NPHS,WARM_RAIN                                    &
     &               ,KPBL,CT,ELFLX,SNOW,SICE                           &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &               ,IMS,IME,JMS,JME,KMS,KME                           &
     &               ,I_START,MIN(I_END,IDE-1)                          &
     &               ,MAX(J_START,2),MIN(J_END,JDE-1)                   &
     &               ,KTS,KTE,1)

!*** EQUIVALENT TO --------->  ,MYIS,MYIE1,MYJS1,MYJE1,KTS,KTE,1)
!
!-----------------------------------------------------------------------
! UNCOMPUTED LOCATIONS MUST BE FILLED IN FOR THE POST-PROCESSOR
!-----------------------------------------------------------------------
!
!***  EASTERN GLOBAL BOUNDARY
!
      IF(MYIE.EQ.IDE)THEN
        DO J=JDS,JDE
        IF (J .GE. MYJS .AND. J .LE. MYJE) THEN
          TH10(MYIE,J)=TH10(MYIE-1,J)
          Q10(MYIE,J)=Q10(MYIE-1,J)
          U10(MYIE,J)=U10(MYIE-1,J)
          V10(MYIE,J)=V10(MYIE-1,J)
          TSHLTR(MYIE,J)=TSHLTR(MYIE-1,J)
          QSHLTR(MYIE,J)=QSHLTR(MYIE-1,J)
        ENDIF
        ENDDO
      ENDIF
!
!***  SOUTHERN GLOBAL BOUNDARY
!

      IF(MYJS.EQ.1)THEN
        DO J=1,2
        DO I=IDS,IDE
          IF (I .GE. MYIS .and. I .LE. MYIE) THEN
            TH10(I,J)=TH10(I,MYJS+2)
            Q10(I,J)=Q10(I,MYJS+2)
            U10(I,J)=U10(I,MYJS+2)
            V10(I,J)=V10(I,MYJS+2)
            TSHLTR(I,J)=TSHLTR(I,MYJS+2)
            QSHLTR(I,J)=QSHLTR(I,MYJS+2)
          ENDIF
        ENDDO
        ENDDO
      ENDIF
!
!***  NORTHERN GLOBAL BOUNDARY
!
      IF(MYJE.EQ.JDE)THEN
        DO J=MYJE-1,MYJE
        DO I=IDS,JDE
          IF (I .GE. MYIS .and. I .LE. MYIE) THEN
            TH10(I,J)=TH10(I,MYJE-2)
            Q10(I,J)=Q10(I,MYJE-2)
            U10(I,J)=U10(I,MYJE-2)
            V10(I,J)=V10(I,MYJE-2)
            TSHLTR(I,J)=TSHLTR(I,MYJE-2)
            QSHLTR(I,J)=QSHLTR(I,MYJE-2)
          ENDIF
        ENDDO
        ENDDO
      ENDIF
!
      IF(CONFIG_FLAGS%SF_SFCLAY_PHYSICS.EQ.1)THEN ! non-NMM package
        DO J=MYJS1,MYJE1
        DO I=MYIS, MYIE1
!         TSHLTR(I,J)=TSHLTR(I,J)*(1.E5/PSHLTR(I,J))**RCP
          IF(TSHLTR(I,J).LT.200..OR.TSHLTR(I,J).GT.350.)THEN
            WRITE(errmess,*)'Troublesome TSHLTR...I,J,TSHLTR,PSHLTR: ',       &
               I,J,TSHLTR(I,J),PSHLTR(I,J)
            CALL wrf_message( errmess )
          ENDIF
	ENDDO
	ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        LLYR=LOWLYR(I,J)
        PQ0X=PQ0
        IF(SM(I,J).GT.0.5)PQ0X=PQ0SEA
        Q2SAT=(PQ0X/P_PHY(I,LLYR,J))*EXP(A2S*(TLOW(I,J)-A3S)            &
     &                                      /(TLOW(I,J)-A4S))
        CKLQ=1.
        IF(QLOW(I,J).GE.Q2SAT*TRESH)CKLQ=0.
        FFS=AKHS(I,J)*P_PHY(I,LLYR,J)/                                  &
     &      ((QLOW(I,J)*P608-CWMLOW(I,J)+1.)*TLOW(I,J)*R_D)
        APES=(1.E5/PINT(I,KTS,J))**CAPA
        QFC1=APES*FFS*XLV
        QFC1=QFC1*CKLQ
        IF(SNO(I,J).GT.0..OR.SICE(I,J).GT.0.)THEN
          RLIVWV=XLS/XLV
          QFC1=QFC1*RLIVWV
        ENDIF
        QSX=QS(I,J)
        IF(SM(I,J).GT.0.5)QSX=QZ0(I,J)
        QWBS(I,J)=(QLOW(I,J)-QSX)*QFC1/APES
      ENDDO
      ENDDO
!
!***  COMPUTE MODEL LAYER CONTAINING THE TOP OF THE BOUNDARY LAYER
!
      IF(CONFIG_FLAGS%BL_PBL_PHYSICS.NE.MYJPBLSCHEME)THEN
        LENGTH_ROW=MYIE1-MYIS1+1
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          KPBL(I,J)=-1000
        ENDDO
        ENDDO
!
        DO J=MYJS2,MYJE2
          KOUNT_ALL=0
          DO K=KTS,KTE
          DO I=MYIS1,MYIE1
            ALTITUDE=Z(I,K+1,J)-SFCZ(I,J)
            IF(PBLH(I,J).LE.ALTITUDE.AND.KPBL(I,J).LT.0)THEN
              KPBL(I,J)=K
              KOUNT_ALL=KOUNT_ALL+1
            ENDIF
            IF(KOUNT_ALL.EQ.LENGTH_ROW)GO TO 100
          ENDDO
          ENDDO
  100     CONTINUE
        ENDDO
      ENDIF
!
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        SNO(I,J)=SNOW(I,J)
        SI(I,J)=SNOWH(I,J)
        LPBL(I,J)=KTE-KPBL(I,J)+1
      ENDDO
      ENDDO
!
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
!
!***  DIAGNOSTIC RADIATION ACCUMULATION
!
        ASWIN (I,J)=ASWIN (I,J)+RSW_DN_SFC(I,J)
        ASWOUT(I,J)=ASWOUT(I,J)-RSWOUT(I,J)*HBM2(I,J)*FACTRS(I,J)
        ASWTOA(I,J)=ASWTOA(I,J)+RSWTOA(I,J)*HBM2(I,J)*FACTRS(I,J)
        ALWIN (I,J)=ALWIN (I,J)+RLW_DN_SFC(I,J)
        ALWOUT(I,J)=ALWOUT(I,J)-RADOT (I,J)*HBM2(I,J)
        ALWTOA(I,J)=ALWTOA(I,J)+RLWTOA(I,J)*HBM2(I,J)
!
        TSFC2=TSFC(I,J)*TSFC(I,J)
        RADOT(I,J)=HBM2(I,J)*EPSR(I,J)*STBOLT*TSFC2*TSFC2
        THS(I,J)=TSFC(I,J)*EXNSFC(I,J)
        PREC(I,J)=0.
      ENDDO
      ENDDO
!
!***  RECONSTRUCT UZ0 AND VZ0 ON VELOCITY POINTS.
!

#ifdef DM_PARALLEL
      IPS=ITS;IPE=ITE;JPS=JTS;JPE=JTE;KPS=KTS;KPE=KTE
# include "HALO_NMM_TURBL_A.inc"
#endif

!
      DO J=MYJS2,MYJE2
      DO I=MYIS,MYIE
        UZ0(I,J)=(UZ0H(I+IVE(J),J)*HBM2(I+IVE(J),J)                     &
     &           +UZ0H(I+IVW(J),J)*HBM2(I+IVW(J),J)                     &
     &           +UZ0H(I,J+1)*HBM2(I,J+1)+UZ0H(I,J-1)*HBM2(I,J-1))*0.25
        VZ0(I,J)=(VZ0H(I+IVE(J),J)*HBM2(I+IVE(J),J)                     &
     &           +VZ0H(I+IVW(J),J)*HBM2(I+IVW(J),J)                     &
     &           +VZ0H(I,J+1)*HBM2(I,J+1)+VZ0H(I,J-1)*HBM2(I,J-1))*0.25
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  UPDATE TEMPERATURE, SPECIFIC HUMIDITY, CLOUD, AND TKE.
!-----------------------------------------------------------------------
!
      E_BDY=(ITE.GE.IDE)
!
      DO J=MYJS2,MYJE2
        IEND=MYIE1
        IF(E_BDY.AND.MOD(J,2).EQ.0)IEND=IEND-1
!
        DO K=KTS,KTE
        DO I=MYIS1,IEND
          DTDT=RTHBLTEN(I,K,J)*PI_PHY(I,K,J)
          DQDT=RQVBLTEN(I,K,J)         !Mixing ratio tendency
          T(I,K,J)=T(I,K,J)+DTDT*DTPHS
          QOLD=Q(I,K,J)
          RATIOMX=QOLD/(1.-QOLD)+DQDT*DTPHS
          Q(I,K,J)=RATIOMX/(1.+RATIOMX)
!         Q(I,K,J)=MAX(Q(I,K,J),EPSQ)
          QW=WATER(I,K,J,P_QC)+RQCBLTEN(I,K,J)*DTPHS  
          QI=WATER(I,K,J,P_QI)+RQIBLTEN(I,K,J)*DTPHS  
          QR=WATER(I,K,J,P_QR)  
          CWM(I,K,J)=QW+QI+QR  
!
          IF(QI<=EPSQ)THEN  
            F_ICE(I,K,J)=0.  
          ELSE  
            F_ICE(I,K,J)=MAX(0.,MIN(1.,QI/CWM(I,K,J)))  
          ENDIF 
          IF(QR<=EPSQ)THEN  
            F_RAIN(I,K,J)=0.  
          ELSE  
            F_RAIN(I,K,J)=QR/(QW+QR)  
          ENDIF  
!
          Q2(I,K,J)=2.*TKE(I,K,J)
        ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!***  INTERPOLATE WIND COMPONENTS BACK TO VELOCITY POINTS.
!-----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
# include "HALO_NMM_TURBL_B.inc"
#endif
!
      DO J=MYJS2,MYJE2
        IEND=MYIE1
        IF(E_BDY.AND.MOD(J,2).EQ.1)IEND=IEND-1
!
        DO K=KTS,KTE
        DO I=MYIS1,IEND

          U(I,K,J)=(DUDT(I+IVE(J),K,J)+DUDT(I+IVW(J),K,J)               &
     &             +DUDT(I,K,J+1)+DUDT(I,K,J-1))*0.25*DTPHS             &
     &             *VTM(I,K,J)+U(I,K,J)
          V(I,K,J)=(DVDT(I+IVE(J),K,J)+DVDT(I+IVW(J),K,J)               &
     &             +DVDT(I,K,J+1)+DVDT(I,K,J-1))*0.25*DTPHS             &
     &             *VTM(I,K,J)+V(I,K,J)

!       if ( abs ((DUDT(I+IVE(J),K,J)+DUDT(I+IVW(J),K,J)      &
!    &             +DUDT(I,K,J+1)+DUDT(I,K,J-1))*0.25*DTPHS   &
!    &             *VTM(I,K,J)) .gt. 10 ) then
!       write(0,*) 'PBL changed U component by more than 10 m/s:: ', I,K,J,U(I,K,J)
!       endif
        ENDDO
        ENDDO
      ENDDO
!
      DEALLOCATE(TOLD)
!-----------------------------------------------------------------------
!***
!***  WE MUST COMPUTE QUANTITIES THAT ARE NO LONGER AVAILABLE
!***  IN THE PBL MODULE.
!***
!-----------------------------------------------------------------------
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        LLIJ=LOWLYR(I,J)
!
!***  INSTANTANEOUS SENSIBLE AND LATENT HEAT FLUX
!
        THBOT=THZ0(I,J)
        IF(SM(I,J).LT.0.5)THBOT=THS(I,J)
        QL=MAX(Q(I,LOWLYR(I,J),J),EPSQ)
        LLYR=LOWLYR(I,J)
        FFS=AKHS(I,J)*P_PHY(I,LLYR,J)/                                  &
     &      ((QLOW(I,J)*P608-CWMLOW(I,J)+1.)*TLOW(I,J)*R_D)
        FFS=FFS*CP
        APES=(1.E5/PINT(I,KTS,J))**CAPA
        TWBS(I,J)=(THLOW(I,J)-THBOT)*FFS/APES
!!!!!!  TWBS(I,J)=-TWBS(I,J)
!
!!!!!!  QWBS(I,J)=-QWBS(I,J)*XLV
!
!***  ACCUMULATED QUANTITIES.
!***  IN OPNL LSM, SFCEVP APPEARS TO BE IN UNITS OF
!***  METERS OF LIQUID WATER.  IT IS COMING FROM
!***  WRF MODULE AS KG/M**2.
!
        SFCSHX(I,J)=SFCSHX(I,J)+TWBS(I,J)
        SFCLHX(I,J)=SFCLHX(I,J)+QWBS(I,J)
        XLVRW=DTPHS/(XLV*RHOWATER)
        SFCEVP(I,J)=SFCEVP(I,J)-QWBS(I,J)*XLVRW
        POTEVP(I,J)=POTEVP(I,J)-QWBS(I,J)*SM(I,J)*XLVRW
        POTFLX(I,J)=POTEVP(I,J)*FACTOR
        SUBSHX(I,J)=SUBSHX(I,J)+GRNFLX(I,J)
!
      ENDDO
      ENDDO
!
!***  COUNTERS
!
      APHTIM=APHTIM+1.
      ARDSW =ARDSW +1.
      ARDLW =ARDLW +1.
      ASRFC =ASRFC +1.
!-----------------------------------------------------------------------
!
      END SUBROUTINE TURBL
!
!-----------------------------------------------------------------------
