!NCEP_MESO:MEDIATION_LAYER:SOLVER
!
!***********************************************************************
!

SUBROUTINE SOLVE_NMM(GRID,CONFIG_FLAGS,                                 &
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
      USE MODULE_IGWAVE_ADJUST,		ONLY: PDTE, PFDHT, DDAMP, VTOA
      USE MODULE_ADVECTION,		ONLY: ADVE, VAD2, HAD2
      USE MODULE_NONHY_DYNAM,		ONLY: EPS, VADZ, HADZ
      USE MODULE_DIFFUSION_NMM, 	ONLY: HDIFF
      USE MODULE_BNDRY_COND,		ONLY: BOCOH, BOCOV
      USE MODULE_EXT_INTERNAL
      USE MODULE_TILES
!     integer :: npes
!     integer :: MPI_COMM_COMP
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!***  INPUT DATA
!
      TYPE(DOMAIN),TARGET :: GRID
!
#include <nmm_dummy_decl.inc>
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
      INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE,                               &
                 IMS,IME,JMS,JME,KMS,KME,                               & 
                 IPS,IPE,JPS,JPE,KPS,KPE
!
      INTEGER :: ITS,ITE,JTS,JTE,KTS,KTE

      INTEGER :: N_MOIST
      INTEGER :: I,ICLTEND,J,JC,LB,k,ierr,mpi_comm_world,irtn
      INTEGER :: IJDS,IJDE,IDF,JDF,KDF
      INTEGER MYPROC
      INTEGER RC
      CHARACTER*80 :: message
!
      REAL :: GPS
      REAL,DIMENSION(GRID%SM31:GRID%EM31,GRID%SM33:GRID%EM33) :: PBLH 

      LOGICAL :: LAST_TIME
!
      real,save :: solve_tim,exch_tim,pdte_tim,adve_tim,vtoa_tim        &
     &,            vadz_tim,hadz_tim,eps_tim,vad2_tim,had2_tim          &
     &,            radiation_tim,rdtemp_tim,turbl_tim,cltend_tim        &
     &,            cucnvc_tim,gsmdrive_tim,hdiff_tim,bocoh_tim          &
     &,            pfdht_tim,ddamp_tim,bocov_tim,sum_tim
      real :: btim,btimx
      integer ij
#ifdef DEREF_KLUDGE
      INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif
#ifdef RSL
      integer rsl_internal_microclock
      external rsl_internal_microclock 
#  define timef rsl_internal_microclock
#else
      real*8 :: timef
      timef() = 0.
#endif
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------


#define COPY_IN
#include <nmm_scalar_derefs.inc>
#include "deref_kludge.h"
#ifdef DM_PARALLEL
#    define REGISTER_I1
#    include <nmm_data_calls.inc>
#endif

      CALL WRF_GET_MYPROC(MYPROC)
!
      CALL GET_IJK_FROM_GRID(grid,                                      &
                             ids,ide,jds,jde,kds,kde,                   &
                             ims,ime,jms,jme,kms,kme,                   &
                             ips,ipe,jps,jpe,kps,kpe )
!
      CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )
!
      CALL wrf_debug(100,"dyn_nmm/solve_nmm.F: warning SIGMA hard coded") 
!
      SIGMA=1 
      HYDRO=.FALSE.
      IHRST=GRID%GMT

      IJDS=MIN(IDS, JDS)
      IJDE=MAX(IDE, JDE)

      IDF=IDE-1
      JDF=JDE-1
      KDF=KDE-1
!
!-----------------------------------------------------------------------
!
!***  FOR NOW SET CONTROLS FOR TILES TO PATCHES
!
      ITS=IPS
      ITE=MIN(IPE,IDF)
      JTS=JPS
      JTE=MIN(JPE,JDF)
      KTS=KPS
      KTE=MIN(KPE,KDF)
!-----------------------------------------------------------------------
!      if(ntsd.eq.0)then
        solve_tim=0.
        exch_tim=0.
        pdte_tim=0.
        adve_tim=0.
        vtoa_tim=0.
        vadz_tim=0.
        hadz_tim=0.
        eps_tim=0.
        vad2_tim=0.
        had2_tim=0.
        radiation_tim=0.
        rdtemp_tim=0.
        turbl_tim=0.
        cltend_tim=0.
        cucnvc_tim=0.
        gsmdrive_tim=0.
        hdiff_tim=0.
        bocoh_tim=0.
        pfdht_tim=0.
        ddamp_tim=0.
        bocov_tim=0.
!      endif
!-----------------------------------------------------------------------
      N_MOIST = NUM_MOIST
!
      MYIS1   =MAX(IDS+1,IPS  )
      MYIE1   =MIN(IDF-1,IPE  )
!
      MYIS_P1 =MAX(IDS  ,IPS-1)
      MYIE_P1 =MIN(IDF  ,IPE+1)
      MYIS_P2 =MAX(IDS  ,IPS-2)
!
      MYIE_P2 =MIN(IDF  ,IPE+2)
!
      MYIS_P3 =MAX(IDS  ,IPS-3)
      MYIE_P3 =MIN(IDF  ,IPE+3)
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
      MYIS1_P2=MAX(IDS+1,IPS-2)
      MYIE1_P2=MIN(IDF-1,IPE+2)
      MYJS2_P2=MAX(JDS+2,JPS-2)
      MYJE2_P2=MIN(JDF-2,JPE+2)
!
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
      PBLH = 0.

      DO J=JMS,JME
      DO I=IMS,IME
        PBLH(I,J)=-1.
      ENDDO
      ENDDO
!
      NBOCO=0
!-----------------------------------------------------------------------
!***
!***               THE MAIN TIME INTEGRATION LOOP
!***
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
      CALL ESMF_ClockGet(grid%domain_clock,advanceCount=NTSD,rc=rc)
      LAST_TIME=grid%stop_time.EQ.grid%current_time+grid%step_time

!      CALL wrf_timetoa ( grid%current_time, message )
!      WRITE(0,*)' SOLVE_NMM CALLED: TIMESTEP IS ',NTSD
!      WRITE(message,*)' SOLVE_NMM CALLED: TIMESTEP IS ',NTSD           &
!               ,' FCST TIME IS',TRIM(message)
!      CALL wrf_message( message )

      CALL WRF_GET_MYPROC(MYPE)
      CALL WRF_GET_DM_COMMUNICATOR ( mpi_comm_world )
!
      btim=timef()
!
      IF(NTSD.EQ.0)THEN
        FIRST=.TRUE.
      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
      exch_tim=exch_tim+timef()-btimx
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
      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_D.inc"
#endif
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in pdte' )

      btimx=timef()

      CALL PDTE(                                                        &
#ifdef DM_PARALLEL
     &            GRID,                                                 &
#endif
     &            NTSD,DT,PT,ETA2,RES,HYDRO                             &
     &           ,HTM,HBM2                                              &
     &           ,PD,PDSL,PDSLO                                         &
     &           ,PETDT,DIV,PSDT                                        &
     &           ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
      pdte_tim=pdte_tim+timef()-btimx
      CALL wrf_debug ( 100 , 'nmm: out of pdte' )
!
!-----------------------------------------------------------------------
!***  ADVECTION OF T, U, AND V
!-----------------------------------------------------------------------
!
      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_F.inc"
#    include "HALO_NMM_F1.inc"
#endif
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in adve' )

      btimx=timef()

      CALL ADVE(NTSD,DT,DETA1,DETA2,PDTOP                               &
     &         ,CURV,F,FAD,F4D,EM_LOC,EMT_LOC,EN,ENT,DX_NMM,DY_NMM      &
     &         ,HTM,HBM2,VTM,VBM2,LMH,LMV                               &
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
      adve_tim=adve_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: out of adve' )

!-----------------------------------------------------------------------
!***  PRESSURE TENDENCY, ETA/SIGMADOT, VERTICAL PART OF OMEGA-ALPHA TERM
!-----------------------------------------------------------------------
!
      CALL wrf_debug ( 100 , 'nmm: in vtoa' )

      btimx=timef()

      CALL VTOA(                                                        &
#ifdef DM_PARALLEL
     &          grid,                                                   &
#endif
     &          NTSD,DT,PT,ETA2                                         &
     &         ,HTM,HBM2,EF4T                                           &
     &         ,T,DWDT,RTOP,OMGALF                                      &
     &         ,PINT,DIV,PSDT,RES                                       &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
      vtoa_tim=vtoa_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: out of vtoa' )

!-----------------------------------------------------------------------
!***  VERTICAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
      CALL wrf_debug ( 100 , 'nmm: in vadz' )

      btimx=timef()

      CALL VADZ(NTSD,DT,FIS,SIGMA,DFL,HTM,HBM2                          &
     &         ,DETA1,DETA2,PDTOP                                       &
     &         ,PINT,PDSL,PDSLO,PETDT                                   &
     &         ,RTOP,T,Q,CWM,Z,W,DWDT,PDWDT                             &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      vadz_tim=vadz_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: out of vadz' )
!
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_G.inc"
#endif
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in hadz' )

      btimx=timef()

      CALL HADZ(NTSD,DT,HYDRO,HTM,HBM2,DETA1,DETA2,PDTOP                &
     &         ,DX_NMM,DY_NMM,FAD                                       &
     &         ,FEW,FNS,FNE,FSE                                         &
     &         ,PDSL,U,V,W,Z                                            &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
      hadz_tim=hadz_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: out of hadz' )
!
!-----------------------------------------------------------------------
!***  ADVECTION OF W
!-----------------------------------------------------------------------
!
      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_H.inc"
#endif
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in eps' )

      btimx=timef()

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
      eps_tim=eps_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: out of eps' )
!
!-----------------------------------------------------------------------
!***  ADVECTION OF Q, TKE, AND CLOUD WATER
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,IDTAD).EQ.0)THEN
        CALL wrf_debug ( 100 , 'nmm: in vad2' )
        btimx=timef()
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
        vad2_tim=vad2_tim+timef()-btimx

        CALL wrf_debug ( 100 , 'nmm: out of vad2' )
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,IDTAD).EQ.0)THEN

        btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_I.inc"
#endif
        exch_tim=exch_tim+timef()-btimx

        CALL wrf_debug ( 100 , 'nmm: in had2' )
        btimx=timef()

        CALL HAD2(                                                     &
#if defined(DM_PARALLEL)
     &            grid%domdesc,                                        &
#endif
     &            NTSD,DT,IDTAD,DX_NMM,DY_NMM                          &
     &           ,AETA1,AETA2,DETA1,DETA2,PDSL,PDTOP                   &
     &           ,HTM,HBM2,HBM3,LMH                                    &
     &           ,Q,Q2,CWM,U,V,Z,HYDRO                                 &
     &           ,N_IUP_H,N_IUP_V                                      &
     &           ,N_IUP_ADH,N_IUP_ADV                                  &
     &           ,IUP_H,IUP_V,IUP_ADH,IUP_ADV                          &
     &           ,IHE,IHW,IVE,IVW,INDX3_WRK                            &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
        had2_tim=had2_tim+timef()-btimx
        CALL wrf_debug ( 100 , 'nmm: out of had2' )
      ENDIF
!
!----------------------------------------------------------------------
!***  RADIATION
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,NRADS).EQ.0.OR.MOD(NTSD,NRADL).EQ.0)THEN

        btimx=timef()
	
         
        CALL wrf_debug ( 100 , 'solve_nmm: calling radiation ' )
        CALL RADIATION(NTSD,DT,JULDAY,JULYR,IHRST,NPHS,GLAT,GLON       &
     &                ,NRADS,NRADL                                     &
     &                ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT      &
     &                ,PD,RES,PINT,T,Q,CWM,THS,ALBEDO,EPSR             &
     &                ,F_ICE,F_RAIN                                    &
!    &                ,SM,HBM2,LMH,ZERO_3D,MOIST,N_MOIST,RESTRT        &
     &                ,SM,HBM2,LMH,ZERO_3D,N_MOIST,RESTRT              &
     &                ,RLWTT,RSWTT,RLWIN,RSWIN,RSWOUT                  &
     &                ,TOTSWDN,TOTLWDN,RLWTOA,RSWTOA,CZMEAN            &
     &                ,CFRACL,CFRACM,CFRACH,SIGT4                      &
     &                ,ACFRST,NCFRST,ACFRCV,NCFRCV                     &
     &                ,CUPPT,VEGFRA,SNO,HTOP,HBOT                      &
     &                ,CONFIG_FLAGS                                    &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                         &
     &                ,IMS,IME,JMS,JME,KMS,KME                         &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        radiation_tim=radiation_tim+timef()-btimx
        CALL wrf_debug ( 100 , 'solve_nmm: back from radiation ' )
      ENDIF

!
!----------------------------------------------------------------------
!***  APPLY TEMPERATURE TENDENCY DUE TO RADIATION
!----------------------------------------------------------------------
!
      btimx=timef()

      CALL wrf_debug ( 100 , 'solve_nmm: calling rdtemp ' )
      CALL RDTEMP(NTSD,DT,JULDAY,JULYR,IHRST,GLAT,GLON                 &
     &           ,CZEN,CZMEAN,T,RSWTT,RLWTT,HTM,HBM2                   &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
      rdtemp_tim=rdtemp_tim+timef()-btimx
      CALL wrf_debug ( 100 , 'solve_nmm: back from rdtemp ' )

!
!----------------------------------------------------------------------
!***  TURBULENT PROCESSES 
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,NPHS).EQ.0)THEN
        btimx=timef()

        CALL wrf_debug ( 100 , 'solve_nmm: calling turbl ' )
        CALL TURBL(GRID                                                &
     &            ,NTSD,DT,NPHS,RESTRT                                 &
     &            ,N_MOIST,NUM_SOIL_LAYERS,SLDPTH,DZSOIL               &
     &            ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT          &
     &            ,SM,LMH,HTM,VTM,HBM2,VBM2,DX_NMM,DFL                 &
     &            ,CZEN,CZMEAN,SIGT4,TOTLWDN,TOTSWDN,RADOT             &
     &            ,PD,RES,PINT,T,Q,CWM,F_ICE,F_RAIN,Q2,U,V             &
     &            ,THS,SST,PREC,SNO,ZERO_3D                            &
     &            ,FIS,Z0,Z0BASE,USTAR,PBLH,LPBL,AKHS,AKMS             &
     &            ,THZ0,QZ0,UZ0,VZ0,QS                                 &
     &            ,STC,SMC,CMC,SMSTAV,SMSTOT,SSROFF,BGROFF             &
     &            ,IVGTYP,ISLTYP,VEGFRA,SHDMIN,SHDMAX,GRNFLX           &
     &            ,SFCEXC,ACSNOW,ACSNOM,SNOPCX,SICE,TG,SOILTB          &
     &            ,ALBASE,MXSNAL,ALBEDO,SH2O,SI,EPSR                   &
     &            ,U10,V10,TH10,Q10,TSHLTR,QSHLTR,PSHLTR               &
     &            ,TWBS,QWBS,SFCSHX,SFCLHX,SFCEVP                      &
     &            ,POTEVP,POTFLX,SUBSHX                                &
     &            ,APHTIM,ARDSW,ARDLW,ASRFC                            &
     &            ,RSWOUT,RSWTOA,RLWTOA                                &
     &            ,ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA             &
     &            ,UZ0H,VZ0H,DUDT,DVDT                                 & !jm
     &            ,PSFC                                                & !jm
     &            ,CONFIG_FLAGS                                        &
     &            ,IHE,IHW,IVE,IVW                                     &
     &            ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &            ,IMS,IME,JMS,JME,KMS,KME                             &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)
        turbl_tim=turbl_tim+timef()-btimx
        CALL wrf_debug ( 100 , 'solve_nmm: back from turbl ' )

!----------------------------------------------------------------------
!*** STORE ORIGINAL TEMPERATURE ARRAY
!----------------------------------------------------------------------

        btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_J.inc"
#endif
        exch_tim=exch_tim+timef()-btimx

        ICLTEND=-1
        CALL wrf_debug ( 100 , 'nmm: in cltend' )

        btimx=timef()
 
        CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                         &
                   ,IDS,IDF,JDS,JDF,KDS,KDE                            &
                   ,IMS,IME,JMS,JME,KMS,KME                            &
                   ,ITS,ITE,JTS,JTE,KTS,KTE)

        cltend_tim=cltend_tim+timef()-btimx
        CALL wrf_debug ( 100 , 'nmm: out of cltend' )
      ENDIF
!
!----------------------------------------------------------------------
!***  CONVECTIVE PRECIPITATION
!----------------------------------------------------------------------
      IF(MOD(NTSD,NCNVC).EQ.0.AND.                                     &
     &   CONFIG_FLAGS%CU_PHYSICS.EQ.KFETASCHEME)THEN
        btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_C.inc"
#endif
        exch_tim=exch_tim+timef()-btimx
      ENDIF
      btimx=timef()
      CALL wrf_debug ( 100 , 'solve_nmm: calling cucnvc' )
      CALL CUCNVC(grid                                                 &
     &           ,NTSD,DT,NCNVC,GPS,RESTRT,HYDRO                       &
!    &           ,CLDEFI,LMH,MOIST,N_MOIST,ENSDIM                      &
     &           ,CLDEFI,LMH,N_MOIST,ENSDIM                            &
     &           ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                    &
     &           ,PDTOP,PT,PD,RES,PINT,T,Q,TCUCN                       &
     &           ,OMGALF,U,V,VTM,W,Z,FIS,W0AVG                         &
     &           ,PREC,ACPREC,CUPREC,CUPPT                             &
     &           ,SM,HBM2,LPBL,HBOT,HTOP,CNVBOT,CNVTOP                 &
     &           ,AVCNVC,ACUTIM,ZERO_3D,IHE,IHW                        &
     &           ,CONFIG_FLAGS                                         &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
      cucnvc_tim=cucnvc_tim+timef()-btimx
      CALL wrf_debug ( 100 , 'solve_nmm: back from cucnvc' )
!----------------------------------------------------------------------
!***  GRIDSCALE MICROPHYSICS (CONDENSATION & PRECIPITATION)
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,NPHS).EQ.0)THEN
        btimx=timef()
        CALL GSMDRIVE(NTSD,DT,NPHS,N_MOIST                             &
     &               ,DX_NMM(ITS,JC),DY,LMH,SM,HBM2,FIS                &
     &               ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                &
     &               ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TRAIN               &
     &               ,F_ICE,F_RAIN,F_RIMEF,SR                          &
     &               ,PREC,ACPREC,ZERO_3D                              &
     &               ,CONFIG_FLAGS                                     &
     &               ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)

      gsmdrive_tim=gsmdrive_tim+timef()-btimx
!----------------------------------------------------------------------
!***  CALCULATE TEMP TENDENCIES AND RESTORE ORIGINAL TEMPS
!----------------------------------------------------------------------
!       btim=timef()
        ICLTEND=0
        CALL wrf_debug ( 100 , 'nmm: in cltend' )

        btimx=timef()

        CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                         &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)

        cltend_tim=cltend_tim+timef()-btimx
        CALL wrf_debug ( 100 , 'nmm: out of cltend' )
      ENDIF
!
!----------------------------------------------------------------------
!***  UPDATE TEMP TENDENCIES FROM CLOUD PROCESSES EVERY TIME STEP
!----------------------------------------------------------------------
!
      ICLTEND=1
      CALL wrf_debug ( 100 , 'nmm: in cltend' )

      btimx=timef()

      CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                           &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
      cltend_tim=cltend_tim+timef()-btimx
      CALL wrf_debug ( 100 , 'nmm: out of cltend' )
!
!----------------------------------------------------------------------
!***  LATERAL DIFFUSION
!----------------------------------------------------------------------
!
      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_K.inc"
#endif
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in hdiff' )

      btimx=timef()

      CALL HDIFF(NTSD,DT,FIS,DY_NMM,HDAC,HDACV                         &
     &          ,HTM,HBM2,VTM,DETA1,SIGMA                              &
     &          ,T,Q,U,V,Q2,Z,W                                        &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
      hdiff_tim=hdiff_tim+timef()-btimx
      CALL wrf_debug ( 100 , 'nmm: out of hdiff' )

!
!----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT HEIGHT POINTS
!----------------------------------------------------------------------
!
      CALL wrf_debug ( 100 , 'nmm: in bocoh' )

      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_L.inc"
#endif
      exch_tim=exch_tim+timef()-btimx

      btimx=timef()

      CALL BOCOH(NTSD,DT,NEST,NUNIT_NBC,NBOCO,LAST_TIME,TSPH           &
     &          ,LB,ETA1,ETA2,PDTOP,PT,RES,HTM                         &
     &          ,PD_B,T_B,Q_B,U_B,V_B,Q2_B,CWM_B                       &
     &          ,PD_BT,T_BT,Q_BT,U_BT,V_BT,Q2_BT,CWM_BT                &
     &          ,PD,T,Q,Q2,CWM,PINT                                    &
     &          ,IJDS,IJDE,SPEC_BDY_WIDTH                              &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
      bocoh_tim=bocoh_tim+timef()-btimx
!

      CALL wrf_debug ( 100 , 'nmm: out of bocoh' )
!
!----------------------------------------------------------------------
!***  IS IT TIME FOR A CHECK POINT ON THE MODEL HISTORY FILE?
!----------------------------------------------------------------------
!
 2003 CONTINUE

!
!----------------------------------------------------------------------
!***  PRESSURE GRD, CORIOLIS, DIVERGENCE, AND HORIZ PART OF OMEGA-ALPHA
!----------------------------------------------------------------------
!
      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in pfdht' )
!
      btimx=timef()

      CALL PFDHT(NTSD,LAST_TIME,PT,DETA1,DETA2,PDTOP,RES,FIS           &
     &          ,HYDRO,SIGMA,FIRST,DX_NMM,DY_NMM                       &
     &          ,HTM,HBM2,VTM,VBM2,VBM3                                &
     &          ,FDIV,FCP,WPDAR,DFL,CPGFU,CPGFV                        &
     &          ,PD,PDSL,T,Q,U,V,CWM,OMGALF,PINT,DWDT                  &
     &          ,RTOP,DIV,FEW,FNS,FNE,FSE                              &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
      pfdht_tim=pfdht_tim+timef()-btimx
      CALL wrf_debug ( 100 , 'nmm: out of pfdht' )

!
!----------------------------------------------------------------------
!***  DIVERGENCE DAMPING
!----------------------------------------------------------------------
!
      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_B.inc"
#endif
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in ddamp' )
      btimx=timef()
      CALL DDAMP(NTSD,DT,DETA1,DETA2,PDSL,PDTOP,DIV,HBM2,VTM           &
     &          ,T,U,V,DDMPU,DDMPV                                     &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
      ddamp_tim=ddamp_tim+timef()-btimx
      CALL wrf_debug ( 100 , 'nmm: out of ddamp' )
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      IF(FIRST.AND.NTSD.EQ.0)THEN
        FIRST=.FALSE.
      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
      exch_tim=exch_tim+timef()-btimx
        GO TO 2000
      ENDIF
!
!----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT VELOCITY POINTS
!----------------------------------------------------------------------
!
      btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_C.inc"
#endif
      exch_tim=exch_tim+timef()-btimx

      CALL wrf_debug ( 100 , 'nmm: in bocov' )

      btimx=timef()
      CALL BOCOV(NTSD,DT,LB,VTM,U_B,V_B,U_BT,V_BT                      &
     &          ,U,V                                                   &
     &          ,IJDS,IJDE,SPEC_BDY_WIDTH                              &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE )
      bocov_tim=bocov_tim+timef()-btimx
        CALL wrf_debug ( 100 , 'nmm: out of bocov' )
      solve_tim=solve_tim+timef()-btim
!----------------------------------------------------------------------
#if 0
      sum_tim=pdte_tim+adve_tim+vtoa_tim+vadz_tim+hadz_tim+eps_tim     &
     &       +vad2_tim+had2_tim+radiation_tim+rdtemp_tim+turbl_tim     &
     &       +cltend_tim+cucnvc_tim+gsmdrive_tim+hdiff_tim             &
     &       +bocoh_tim+pfdht_tim+ddamp_tim+bocov_tim+exch_tim
!     if(ntsd.ge.9000 .and. mod(ntsd,100) .eq. 0)then
      write(0,*)' ntsd=',ntsd,' solve_tim=',solve_tim*1.e-3 &
     &         ,' sum_tim=',sum_tim*1.e-3
      write(0,*)' pdte_tim=',pdte_tim*1.e-3,' pct=',pdte_tim/sum_tim*100.
      write(0,*)' adve_tim=',adve_tim*1.e-3,' pct=',adve_tim/sum_tim*100.
      write(0,*)' vtoa_tim=',vtoa_tim*1.e-3,' pct=',vtoa_tim/sum_tim*100.
      write(0,*)' vadz_tim=',vadz_tim*1.e-3,' pct=',vadz_tim/sum_tim*100.
      write(0,*)' hadz_tim=',hadz_tim*1.e-3,' pct=',hadz_tim/sum_tim*100.
      write(0,*)' eps_tim=',eps_tim*1.e-3,' pct=',eps_tim/sum_tim*100.
      write(0,*)' vad2_tim=',vad2_tim*1.e-3,' pct=',vad2_tim/sum_tim*100.
      write(0,*)' had2_tim=',had2_tim*1.e-3,' pct=',had2_tim/sum_tim*100.
      write(0,*)' radiation_tim=',radiation_tim*1.e-3,' pct=',radiation_tim/sum_tim*100.
      write(0,*)' rdtemp_tim=',rdtemp_tim*1.e-3,' pct=',rdtemp_tim/sum_tim*100.
      write(0,*)' turbl_tim=',turbl_tim*1.e-3,' pct=',turbl_tim/sum_tim*100.
      write(0,*)' cltend_tim=',cltend_tim*1.e-3,' pct=',cltend_tim/sum_tim*100.
      write(0,*)' cucnvc_tim=',cucnvc_tim*1.e-3,' pct=',cucnvc_tim/sum_tim*100.
      write(0,*)' gsmdrive_tim=',gsmdrive_tim*1.e-3,' pct=',gsmdrive_tim/sum_tim*100.
      write(0,*)' hdiff_tim=',hdiff_tim*1.e-3,' pct=',hdiff_tim/sum_tim*100.
      write(0,*)' bocoh_tim=',bocoh_tim*1.e-3,' pct=',bocoh_tim/sum_tim*100.
      write(0,*)' pfdht_tim=',pfdht_tim*1.e-3,' pct=',pfdht_tim/sum_tim*100.
      write(0,*)' ddamp_tim=',ddamp_tim*1.e-3,' pct=',ddamp_tim/sum_tim*100.
      write(0,*)' bocov_tim=',bocov_tim*1.e-3,' pct=',bocov_tim/sum_tim*100.
      write(0,*)' exch_tim=',exch_tim*1.e-3,' pct=',exch_tim/sum_tim*100.
!      endif
#endif

#define COPY_OUT
#include <nmm_scalar_derefs.inc>

      Return
!----------------------------------------------------------------------
!**********************************************************************
!**********************************************************************
!*************    EXIT FROM THE TIME LOOP    **************************
!**********************************************************************
!**********************************************************************
!----------------------------------------------------------------------
      END SUBROUTINE SOLVE_NMM
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      SUBROUTINE TWR(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,NTSD
!
      REAL,DIMENSION(IMS:IME,KMS:KME+KK,JMS:JME),INTENT(IN) :: ARRAY
!
      CHARACTER(*),INTENT(IN) :: FIELD
!
!*** LOCAL VARIABLES
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER,DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM
!
      INTEGER :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND,IUNIT                &
     &          ,J,K,N,NLEN,NSIZE
      INTEGER :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL,ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(12) :: FILENAME
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(NTSD.LE.9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD.LE.99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD.LE.999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD.LE.9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD.LE.99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
      FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)
!
      IF(MYPE.EQ.0)THEN
        CALL INT_GET_FRESH_HANDLE(IUNIT)
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF
!
!----------------------------------------------------------------------
!!!!  DO 500 K=KTS,KTE+KK     !Unflipped
!!!!  DO 500 K=KTE+KK,KTS,-1
      DO 500 K=KDE-1,KDS,-1   !Write LM layers top down for checking
!----------------------------------------------------------------------
!
      IF(MYPE.EQ.0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,K,J)
        ENDDO
        ENDDO
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(IT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          CALL MPI_RECV(JT_REM,2,MPI_INTEGER,IPE,IPE                    &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
!
          ITS_REM=IT_REM(1)
          ITE_REM=IT_REM(2)
          JTS_REM=JT_REM(1)
          JTE_REM=JT_REM(2)
!
          NSIZE=(ITE_REM-ITS_REM+1)*(JTE_REM-JTS_REM+1)
          ALLOCATE(VALUES(1:NSIZE))
!
          CALL MPI_RECV(VALUES,NSIZE,MPI_REAL,IPE,IPE                   &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
          N=0
          DO J=JTS_REM,JTE_REM
            DO I=ITS_REM,ITE_REM
              N=N+1
              TWRITE(I,J)=VALUES(N)
            ENDDO
          ENDDO
!
          DEALLOCATE(VALUES)
!
        ENDDO
!
!----------------------------------------------------------------------
      ELSE
        NSIZE=(ITE-ITS+1)*(JTE-JTS+1)
        ALLOCATE(VALUES(1:NSIZE))
!
        N=0
        DO J=JTS,JTE
        DO I=ITS,ITE
          N=N+1
          VALUES(N)=ARRAY(I,K,J)
        ENDDO
        ENDDO
!
        IT_REM(1)=ITS
        IT_REM(2)=ITE
        JT_REM(1)=JTS
        JT_REM(2)=JTE
!
        CALL MPI_SEND(IT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
        CALL MPI_SEND(JT_REM,2,MPI_INTEGER,0,MYPE                       &
     &               ,MPI_COMM_COMP,ISEND)
!
        CALL MPI_SEND(VALUES,NSIZE,MPI_REAL,0,MYPE                      &
     &               ,MPI_COMM_COMP,ISEND)
!
        DEALLOCATE(VALUES)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
      IF(MYPE.EQ.0)THEN
!
        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2).EQ.0)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO
!
      ENDIF
!
!----------------------------------------------------------------------
  500 CONTINUE
!
      IF(MYPE.EQ.0)CLOSE(IUNIT)
!----------------------------------------------------------------------
!
      END SUBROUTINE TWR
