!
!NCEP_MESO:MEDIATION_LAYER:SOLVER
!
!***********************************************************************
!

      SUBROUTINE SOLVE_NMM ( grid , config_flags ,                      &
!
#include "nmm_dummy_args.inc"
!
     &           )

!-----------------------------------------------------------------------
      USE MODULE_DOMAIN
      USE MODULE_CONFIGURE
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_STATE_DESCRIPTION
      USE MODULE_CTLBLK
      USE MODULE_MPP
      USE MODULE_DM
      USE MODULE_PHYS
      USE MODULE_IGWAVE_ADJUST,         ONLY: PDTEDT, PFDHT, DDAMP
      USE MODULE_ADVECTION,             ONLY: ADVE, VAD2, HAD2
      USE MODULE_NONHY_DYNAM,           ONLY: EPS, VADZ, HADZ
      USE MODULE_DIFFUSION_NMM,         ONLY: HDIFF
      USE MODULE_BNDRY_COND,            ONLY: BOCOH, BOCOV
      USE MODULE_EXT_INTERNAL
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!***  INPUT DATA
!

      TYPE(DOMAIN) , TARGET :: GRID

!  Definitions of dummy arguments to solve
#include <nmm_dummy_decl.inc>
!  WRF state bcs
      TYPE (GRID_CONFIG_REC_TYPE) , INTENT(IN)          :: CONFIG_FLAGS

!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
      INTEGER                    :: IDS , IDE , JDS , JDE , KDS , KDE , &
     &                              IMS , IME , JMS , JME , KMS , KME , &
     &                              IPS , IPE , JPS , JPE , KPS , KPE
      INTEGER                    :: ITS , ITE , JTS , JTE , KTS , KTE

      INTEGER :: N_MOIST
      INTEGER :: I,ICLTEND,J,JC,LB
      INTEGER :: IJDS,IJDE,IDF,JDF,KDF
      CHARACTER*80 :: MESSAGE
!
      REAL :: GPS
      REAL,DIMENSION(GRID%SM31:GRID%EM31,GRID%SM33:GRID%EM33) :: PBLH 
!      REAL,DIMENSION(GRID%SM31:GRID%EM31,GRID%SM32:GRID%EM32,           &
!     &               GRID%SM33:GRID%EM33)                     :: W0AVG

      LOGICAL :: LAST_TIME
      INTEGER rc
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------


#define COPY_IN
#include <nmm_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    define REGISTER_I1
#      include <nmm_data_calls.inc>
#endif

      CALL GET_IJK_FROM_GRID ( GRID ,                                   &
     &                         IDS, IDE, JDS, JDE, KDS, KDE,            &
     &                         IMS, IME, JMS, JME, KMS, KME,            &
     &                         IPS, IPE, JPS, JPE, KPS, KPE )
      CALL WRF_DEBUG(100,                                               & 
     &              "dyn_nmm/solve_nmm.F: warning SIGMA hard coded") 

      SIGMA=1 
      HYDRO=.FALSE.
      IJDS = MIN(IDS, JDS)
      IJDE = MAX(IDE, JDE)

      IDF = IDE-1
      JDF = JDE-1
      KDF = KDE-1
 
!-----------------------------------------------------------------------
!
!***  FOR NOW SET CONTROLS FOR TILES TO PATCHES
!
      ITS = IPS
      ITE = MIN(IPE,IDF)
      JTS = JPS
      JTE = MIN(JPE,JDF)
      KTS = KPS
      KTE = MIN(KPE,KDF)
!-----------------------------------------------------------------------
      N_MOIST = NUM_MOIST

      MYIS    =MAX(IDS  ,IPS  )
      MYIE    =MIN(IDF  ,IPE  )
      MYJS    =MAX(JDS  ,JPS  )
      MYJE    =MIN(JDF  ,JPE  )
      MYIS1   =MAX(IDS+1,IPS  )
      MYIE1   =MIN(IDF-1,IPE  )
      MYJS2   =MAX(JDS+2,JPS  )
      MYJE2   =MIN(JDF-2,JPE  )

      MYIS_P1 =MAX(IDS  ,IPS-1)
      MYIE_P1 =MIN(IDF  ,IPE+1)

      MYIS_P2 =MAX(IDS  ,IPS-2)
      MYIE_P2 =MIN(IDF  ,IPE+2)

      MYIS_P3 =MAX(IDS  ,IPS-3)
      MYIE_P3 =MIN(IDF  ,IPE+3)
      MYJS_P3 =MAX(JDS  ,JPS-3)
      MYJE_P3 =MIN(JDF  ,JPE+3)
      MYIS_P4 =MAX(IDS  ,IPS-4)
      MYIE_P4 =MIN(IDF  ,IPE+4)
      MYJS_P4 =MAX(JDS  ,JPS-4)
      MYJE_P4 =MIN(JDF  ,JPE+4)
      MYIS_P5 =MAX(IDS  ,IPS-5)
      MYIE_P5 =MIN(IDF  ,IPE+5)
      MYJS_P5 =MAX(JDS  ,JPS-5)
      MYJE_P5 =MIN(JDF  ,JPE+5)
      MYIS1_P1=MAX(IDS+1,IPS-1)
      MYIE1_P1=MIN(IDF-1,IPE+1)
      MYJS1_P1=MAX(JDS+1,JPS-1)
      MYJE1_P1=MIN(JDF-1,JPE+1)
      MYJS2_P1=MAX(JDS+2,JPS-1)
      MYJE2_P1=MIN(JDF-2,JPE+1)
      MYJS1_P2=MAX(JDS+1,JPS-2)
      MYJE1_P2=MIN(JDF-1,JPE+2)
      MYIS1_P2=MAX(IDS+1,IPS-2)
      MYIE1_P2=MIN(IDF-1,IPE+2)
      MYJS2_P2=MAX(JDS+2,JPS-2)
      MYJE2_P2=MIN(JDF-2,JPE+2)
      MYJS1_P3=MAX(JDS+1,JPS-3)
      MYJE1_P3=MIN(JDF-1,JPE+3)
      MYJS2_P3=MAX(JDS+2,JPS-3)
      MYJE2_P3=MIN(JDF-2,JPE+3)

      DO J=MYJS_P4,MYJE_P4
        IHEG(J)=MOD(J+1,2)
        IHWG(J)=IHEG(J)-1
        IVEG(J)=MOD(J,2)
        IVWG(J)=IVEG(J)-1
      ENDDO

      DO J=MYJS_P4,MYJE_P4
        IVW(J)=IVWG(J)
        IVE(J)=IVEG(J)
        IHE(J)=IHEG(J)
        IHW(J)=IHWG(J)
      ENDDO
!
!***  LATERAL POINTS IN THE BOUNDARY ARRAYS
!
      LB=2*(IDF-IDS+1)+(JDF-JDS+1)-3
!
!***  APPROXIMATE GRIDPOINT SPACING (METERS)
!
      JC=JMS+(JME-JMS)/2
      GPS=SQRT(DX_NMM(IMS,JC)**2+DY_NMM**2)
!
!***  TIMESTEPS PER HOUR
!
      TSPH=3600./DT
!-----------------------------------------------------------------------
!
      DO J=JMS,JME
      DO I=IMS,IME
        PBLH(I,J)=-1.
      ENDDO
      ENDDO
!
      NBOCO=0
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!***
!***               THE MAIN TIME INTEGRATION LOOP
!***
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!***  NTSD IS THE TIMESTEP COUNTER (Number of Time Steps Done)
!
!-----------------------------------------------------------------------
!
!***
!***  FIRST TIME THROUGH TOTAL_TIME_STEPS IS ZERO.
!***  IT IS INCREMENTED IN SOLVE_INTERFACE.
!***
      CALL ESMF_ClockGetAdvanceCount( grid%domain_clock, NTSD, rc )
      LAST_TIME = grid%stop_time .EQ. grid%current_time + grid%step_time

!      CALL ESMF_TimeGetString( grid%current_time, message, rc=rc )
!      WRITE(message,*)' SOLVE_NMM CALLED: TIMESTEP IS ',NTSD            &
!     &         ,' FCST TIME IS',TRIM(message)
!      CALL wrf_message( message )

      IF(NTSD.EQ.0)THEN
        FIRST=.TRUE.
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
        GO TO 2003
      ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 2000 CONTINUE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  PRESSURE TENDENCY, SIGMA DOT, VERTICAL PART OF OMEGA-ALPHA
!-----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
#    include "HALO_NMM_D.inc"
#endif

      CALL WRF_DEBUG ( 100 , 'nmm: in pdtedt' )

      CALL PDTEDT(                                                      &
#ifdef DM_PARALLEL
     &            GRID,                                                 &
#endif
     &            NTSD,DT,PT,ETA2,RES,HYDRO                             &
     &           ,HTM,HBM2,EF4T                                         &
     &           ,PD,PDSL,PDSLO,T,DWDT,RTOP,OMGALF                      &
     &           ,PETDT,PINT,DIV,PSDT                                   &
     &           ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)

      CALL WRF_DEBUG ( 100 , 'nmm: out of pdtedt' )
!
!-----------------------------------------------------------------------
!***  ADVECTION OF T, U, AND V
!-----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
#    include "HALO_NMM_F.inc"
#    include "HALO_NMM_F1.inc"
#endif

      CALL WRF_DEBUG ( 100 , 'nmm: in adve' )

      CALL ADVE(NTSD,DT,DETA1,DETA2,PDTOP                               &
     &         ,CURV,F,FAD,F4D,EM_LOC,EMT_LOC,EN,ENT,DX_NMM,DY_NMM      &
     &         ,HTM,HBM2,VTM,VBM2                                       &
     &         ,T,U,V,PDSLO,TOLD,UOLD,VOLD                              &
     &         ,PETDT,UPSTRM                                            &
     &         ,FEW,FNS,FNE,FSE                                         &
     &         ,ADT,ADU,ADV                                             &
     &         ,N_IUP_H,N_IUP_V                                         &
     &         ,N_IUP_ADH,N_IUP_ADV                                     &
     &         ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                             &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      CALL WRF_DEBUG ( 100 , 'nmm: out of adve' )

!
!-----------------------------------------------------------------------
!***  VERTICAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
      CALL WRF_DEBUG ( 100 , 'nmm: in vadz' )

      CALL VADZ(NTSD,DT,FIS,SIGMA,DFL,HTM,HBM2                          &
     &         ,DETA1,DETA2,PDTOP                                       &
     &         ,PINT,PDSL,PDSLO,PETDT                                   &
     &         ,RTOP,T,Q,CWM,Z,W,DWDT,PDWDT                             &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      CALL WRF_DEBUG ( 100 , 'nmm: out of vadz' )

!
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
#    include "HALO_NMM_G.inc"
#endif

      CALL WRF_DEBUG ( 100 , 'nmm: in hadz' )

      CALL HADZ(NTSD,DT,HYDRO,HTM,HBM2,DETA1,DETA2,PDTOP                &
     &         ,DX_NMM,DY_NMM,FAD                                       &
     &         ,FEW,FNS,FNE,FSE                                         &
     &         ,PDSL,U,V,W,Z                                            &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      CALL WRF_DEBUG ( 100 , 'nmm: out of hadz' )
!
!-----------------------------------------------------------------------
!***  ADVECTION OF W
!-----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
#    include "HALO_NMM_H.inc"
#endif

      CALL WRF_DEBUG ( 100 , 'nmm: in eps' )

      CALL EPS(NTSD,DT,HYDRO,DX_NMM,DY_NMM,FAD                          &
     &        ,DETA1,DETA2,PDTOP,PT                                     &
     &        ,HTM,HBM2,HBM3,LMH                                        &
     &        ,PDSL,PDSLO,PINT,RTOP,PETDT,PDWDT                         &
     &        ,DWDT,DWDTMN,DWDTMX                                       &
     &        ,FNS,FEW,FNE,FSE                                          &
     &        ,T,U,V,W,Q,CWM                                            &
     &        ,IHE,IHW,IVE,IVW,INDX3_WRK                                &
     &        ,IDS,IDF,JDS,JDF,KDS,KDE                                  &
     &        ,IMS,IME,JMS,JME,KMS,KME                                  &
     &        ,ITS,ITE,JTS,JTE,KTS,KTE)

      CALL WRF_DEBUG ( 100 , 'nmm: out of eps' )
!
!-----------------------------------------------------------------------
!***  VERTICAL/HORIZONTAL ADVECTION OF Q, TKE, AND CLOUD WATER
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,IDTAD).EQ.0)THEN

        CALL WRF_DEBUG ( 100 , 'nmm: in vad2' )

        CALL VAD2(NTSD,DT,IDTAD,DX_NMM,DY_NMM                           &
     &           ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                    &
     &           ,HBM2,LMH                                              &
     &           ,Q,Q2,CWM,PETDT                                        &
     &           ,N_IUP_H,N_IUP_V                                       &
     &           ,N_IUP_ADH,N_IUP_ADV                                   &
     &           ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                           &
     &           ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)

        CALL WRF_DEBUG ( 100 , 'nmm: out of vad2' )

      ENDIF
!
!***
!
      IF(MOD(NTSD,IDTAD).EQ.0)THEN

#ifdef DM_PARALLEL
#    include "HALO_NMM_I.inc"
#endif

        CALL WRF_DEBUG ( 100 , 'nmm: in had2' )

        CALL HAD2(                                                      &
#if defined(DM_PARALLEL)
     &            grid%domdesc,                                         &
#endif
                  NTSD,DT,IDTAD,DX_NMM,DY_NMM                           &
     &           ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                    &
     &           ,HTM,HBM2,HBM3,LMH                                     &
     &           ,Q,Q2,CWM,U,V,Z,HYDRO                                  &
     &           ,N_IUP_H,N_IUP_V                                       &
     &           ,N_IUP_ADH,N_IUP_ADV                                   &
     &           ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                           &
     &           ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)

        CALL WRF_DEBUG ( 100 , 'nmm: out of had2' )

      ENDIF

!
!-----------------------------------------------------------------------
!***  RADIATION
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,NRADS).EQ.0.OR.MOD(NTSD,NRADL).EQ.0)THEN

#if 1
        CALL RADIATION(NTSD,DT,JULDAY,JULYR,IHRST,NPHS,GLAT,GLON        &
     &                ,NRADS,NRADL                                      &
     &                ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT       &
     &                ,PD,RES,PINT,T,Q,CWM,THS,ALBEDO,EPSR              &
     &                ,SM,HBM2,LMH,ZERO_3D,MOIST,N_MOIST,LPTOP,RESTRT         &
     &                ,RLWTT,RSWTT,RLWIN,RSWIN,RSWOUT                   &
     &                ,TOTSWDN,TOTLWDN,RLWTOA,RSWTOA,CZMEAN             &
     &                ,CFRACL,CFRACM,CFRACH,SIGT4                       &
     &                ,ACFRST,NCFRST,ACFRCV,NCFRCV                      &
     &                ,CUPPT,VEGFRC,SNO,HTOP,HBOT                       &
     &                ,CONFIG_FLAGS                                     &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
#else
       write(0,*)"WARNING  RADIATION COMMENTED OUT"
#endif

      ENDIF
!
!-----------------------------------------------------------------------
!***  APPLY TEMPERATURE TENDENCY DUE TO RADIATION
!-----------------------------------------------------------------------
!
      CALL RDTEMP(NTSD,DT,JULDAY,JULYR,IHRST,GLAT,GLON                  &
     &           ,CZEN,CZMEAN,T,RSWTT,RLWTT,HTM,HBM2                    &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)

!
!-----------------------------------------------------------------------
!***  TURBULENT PROCESSES
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,NPHS).EQ.0)THEN
#if 1
        CALL TURBL(GRID                                                 &
     &            ,NTSD,DT,NPHS,RESTRT                                  &
     &            ,N_MOIST,NUM_SOIL_LAYERS,SLDPTH,DZSOIL                &
     &            ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT           &
     &            ,SM,LMH,HTM,VTM,HBM2,VBM2,DX_NMM,DFL                  &
     &            ,CZEN,CZMEAN,SIGT4,TOTLWDN,TOTSWDN,RADOT              &
     &            ,PD,RES,PINT,T,Q,CWM,Q2,U,V                           &
     &            ,THS,SST,PREC,SNO,ZERO_3D                             &
     &            ,FIS,Z0,Z0BASE,USTAR,PBLH,LPBL,AKHS,AKMS              &
     &            ,THZ0,QZ0,UZ0,VZ0,QS                                  &
     &            ,STC,SMC,CMC,SMSTAV,SMSTOT,SSROFF,BGROFF              &
     &            ,IVGTYP,ISLTYP,VEGFRC,SHDMIN,SHDMAX,GRNFLX            &
     &            ,SFCEXC,ACSNOW,ACSNOM,SNOPCX,SICE,TG,SOILTB           &
     &            ,ALBASE,MXSNAL,ALBEDO,SH2O,SI,EPSR                    &
     &            ,U10,V10,TH10,Q10,TSHLTR,QSHLTR,PSHLTR                &
     &            ,TWBS,QWBS,SFCSHX,SFCLHX,SFCEVP                       &
     &            ,POTEVP,POTFLX,SUBSHX                                 &
     &            ,APHTIM,ARDSW,ARDLW,ASRFC                             &
     &            ,RSWOUT,RSWTOA,RLWTOA                                 &
     &            ,ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA              &
     &            ,UZ0H,VZ0H,DUDT,DVDT                                  &
     &            ,CONFIG_FLAGS                                         &
     &            ,IHE,IHW,IVE,IVW                                      &
     &            ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &            ,IMS,IME,JMS,JME,KMS,KME                              &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)

#else
      write(0,*)' --- TURBL commented out --- '
#endif
!
!-----------------------------------------------------------------------
!*** STORE ORIGINAL TEMPERATURE ARRAY
!-----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
#    include "HALO_NMM_J.inc"
#endif

        CALL WRF_DEBUG ( 100 , 'nmm: in cltend' )

        ICLTEND=-1
        CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                          &
                   ,IDS,IDF,JDS,JDF,KDS,KDE                             &
                   ,IMS,IME,JMS,JME,KMS,KME                             &
                   ,ITS,ITE,JTS,JTE,KTS,KTE)

        CALL WRF_DEBUG ( 100 , 'nmm: out of cltend' )
 
      ENDIF
!
!-----------------------------------------------------------------------
!***  CONVECTIVE PRECIPITATION
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,NCNVC).EQ.0)THEN
#if 1
        CALL CUCNVC(NTSD,DT,NCNVC,GPS,RESTRT                            &
     &             ,CLDEFI,LMH,MOIST,N_MOIST,ENSDIM                     &
     &             ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                   &
     &             ,PDTOP,PT,PD,RES,PINT,T,Q,TCUCN                      &
     &             ,PREC,ACPREC,CUPREC,CUPPT                            &
     &             ,SM,HBM2,LPBL,HBOT,HTOP,CNVBOT,CNVTOP                &
     &             ,AVCNVC,ACUTIM,ZERO_3D                               & 
     &             ,CONFIG_FLAGS                                        &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &             ,IMS,IME,JMS,JME,KMS,KME                             &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
#else
      write(0,*)' --- CUCNVC commented out --- '
#endif
      ENDIF
!
!-----------------------------------------------------------------------
!***  GRIDSCALE MICROPHYSICS (CONDENSATION & PRECIPITATION)
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,NPHS).EQ.0)THEN
        CALL GSMDRIVE(NTSD,DT,NPHS,MOIST,N_MOIST                              &
     &               ,DX_NMM(ITS,JC),DY,LMH,SM,HBM2,FIS                 &
     &               ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                 &
     &               ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TRAIN                &
     &               ,F_ICE,F_RAIN,F_RIMEF,SR                           &
     &               ,PREC,ACPREC,ZERO_3D                               &
     &               ,CONFIG_FLAGS                                      &
     &               ,IDS,IDF,JDS,JDF,KDS,KDE                           &
     &               ,IMS,IME,JMS,JME,KMS,KME                           &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!***  CALCULATE TEMP TENDENCIES AND RESTORE ORIGINAL TEMPS
!-----------------------------------------------------------------------
!
        CALL WRF_DEBUG ( 100 , 'nmm: in cltend' )

        ICLTEND=0
        CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                          &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &             ,IMS,IME,JMS,JME,KMS,KME                             &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)

        CALL WRF_DEBUG ( 100 , 'nmm: out of cltend' )

      ENDIF
!
!-----------------------------------------------------------------------
!***  UPDATE TEMP TENDENCIES FROM CLOUD PROCESSES EVERY TIME STEP
!-----------------------------------------------------------------------
!
      CALL WRF_DEBUG ( 100 , 'nmm: in cltend' )
   
      ICLTEND=1
      CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                            &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)

      CALL WRF_DEBUG ( 100 , 'nmm: out of cltend' )
!
!-----------------------------------------------------------------------
!***  LATERAL DIFFUSION
!-----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
#    include "HALO_NMM_K.inc"
#endif

      CALL WRF_DEBUG ( 100 , 'nmm: in hdiff' )

      CALL HDIFF(NTSD,DT,FIS,DY_NMM,HDAC,HDACV                          &
     &          ,HTM,HBM2,VTM,DETA1,SIGMA                               &
     &          ,T,Q,U,V,Q2,Z                                           &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                              &
     &          ,IDS,idf,JDS,jdf,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)

      CALL WRF_DEBUG ( 100 , 'nmm: out of hdiff' )
!
!-----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT HEIGHT POINTS
!-----------------------------------------------------------------------
!
      CALL WRF_DEBUG ( 100 , 'nmm: in bocoh' )

#ifdef DM_PARALLEL
#    include "HALO_NMM_L.inc"
#endif

      CALL BOCOH(NTSD,DT,NEST,NUNIT_NBC,NBOCO,LAST_TIME,TSPH            &
     &          ,LB,ETA1,ETA2,PDTOP,PT,RES,HTM                          &
     &          ,PD_B,T_B,Q_B,U_B,V_B,Q2_B,CWM_B                        &
     &          ,PD_BT,T_BT,Q_BT,U_BT,V_BT,Q2_BT,CWM_BT                 &
     &          ,PD,T,Q,Q2,CWM,PINT                                     &
     &          ,IJDS,IJDE,SPEC_BDY_WIDTH                               &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                              &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)

      CALL WRF_DEBUG ( 100 , 'nmm: out of bocoh' )
!
!-----------------------------------------------------------------------
!
 2003 CONTINUE
!
!-----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif

      CALL WRF_DEBUG ( 100 , 'nmm: in pfdht' )
 
      CALL PFDHT(NTSD,LAST_TIME,PT,DETA1,DETA2,PDTOP,RES,FIS            &
     &          ,HYDRO,SIGMA,FIRST,DX_NMM,DY_NMM                        &
     &          ,HTM,HBM2,VTM,VBM2,VBM3                                 &
     &          ,FDIV,FCP,WPDAR,DFL,CPGFU,CPGFV                         &
     &          ,PD,PDSL,T,Q,U,V,CWM,OMGALF,PINT,DWDT                   &
     &          ,RTOP,DIV,FEW,FNS,FNE,FSE                               &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                              &
     &          ,IDS,idf,JDS,jdf,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!-----------------------------------------------------------------------
!***  DIVERGENCE DAMPING
!-----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
#    include "HALO_NMM_B.inc"
#endif

      CALL WRF_DEBUG ( 100 , 'nmm: in ddamp' )

      CALL DDAMP(NTSD,DT,DETA1,DETA2,PDSL,PDTOP,DIV,HBM2,VTM            &
     &          ,T,U,V,DDMPU,DDMPV                                      &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                              &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)

      CALL WRF_DEBUG ( 100 , 'nmm: out of ddamp' )
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      IF(FIRST.AND.NTSD.EQ.0)THEN
        FIRST=.FALSE.
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
        GO TO 2000
      ENDIF
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT VELOCITY POINTS
!-----------------------------------------------------------------------
!
#ifdef DM_PARALLEL
#    include "HALO_NMM_C.inc"
#endif

      CALL WRF_DEBUG ( 100 , 'nmm: in bocov' )

      CALL BOCOV(NTSD,DT,LB,VTM,U_B,V_B,U_BT,V_BT                       &
     &          ,U,V                                                    &
     &          ,IJDS,IJDE,SPEC_BDY_WIDTH                               &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                              &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE )

      CALL WRF_DEBUG ( 100 , 'nmm: out of bocov' )

#define COPY_OUT
#include <nmm_scalar_derefs.inc>

!----------------------------------------------------------------------
!
      RETURN
!
!----------------------------------------------------------------------
!**********************************************************************
!**********************************************************************
!*************    EXIT FROM THE TIME LOOP    **************************
!**********************************************************************
!**********************************************************************
!----------------------------------------------------------------------
      END SUBROUTINE SOLVE_NMM
!----------------------------------------------------------------------
!**********************************************************************
