!-----------------------------------------------------------------------
!
!NCEP_MESO:MEDIATION_LAYER:SOLVER
!
!-----------------------------------------------------------------------
#include "nmm_loop_basemacros.h"
#include "nmm_loop_macros.h"
!-----------------------------------------------------------------------
!
      SUBROUTINE SOLVE_NMM(GRID,CONFIG_FLAGS,                           &
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
      USE MODULE_DM
      USE MODULE_IGWAVE_ADJUST,		ONLY: PDTE,PFDHT,DDAMP,VTOA
      USE MODULE_ADVECTION,		ONLY: ADVE,VAD2,HAD2
      USE MODULE_NONHY_DYNAM,		ONLY: EPS,VADZ,HADZ
      USE MODULE_DIFFUSION_NMM, 	ONLY: HDIFF
      USE MODULE_BNDRY_COND,		ONLY: BOCOH,BOCOV
      USE MODULE_PHYSICS_CALLS
      USE MODULE_EXT_INTERNAL
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
      INCLUDE "mpif.h"
!-----------------------------------------------------------------------
!
!***  INPUT DATA
!
!-----------------------------------------------------------------------
!
      TYPE(DOMAIN),TARGET :: GRID
!
!***  DEFINITIONS OF DUMMY ARGUMENTS TO THIS ROUTINE (GENERATED FROM REGISTRY)
!----------------------------
#include <nmm_dummy_decl.inc>
!----------------------------
!
!***  STRUCTURE THAT CONTAINS RUN-TIME CONFIGURATION (NAMELIST) DATA FOR DOMAIN
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
      INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                & 
     &          ,IPS,IPE,JPS,JPE,KPS,KPE                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER :: I,ICLTEND,IDF,IJDE,IJDS,IRTN,J,JC,JDF,K,KDF,LB,N_MOIST
      INTEGER :: MPI_COMM_COMP,MYPE,MYPROC,NPES
      INTEGER :: RC
!
      CHARACTER*80 :: MESSAGE
!
      REAL :: GPS
!
      LOGICAL :: LAST_TIME
!
!-----------------------------------------------------------------------
!***  TIMING VARIABLES
!-----------------------------------------------------------------------
      real,save :: solve_tim,exch_tim,pdte_tim,adve_tim,vtoa_tim        &
     &,            vadz_tim,hadz_tim,eps_tim,vad2_tim,had2_tim          &
     &,            radiation_tim,rdtemp_tim,turbl_tim,cltend_tim        &
     &,            cucnvc_tim,gsmdrive_tim,hdiff_tim,bocoh_tim          &
     &,            pfdht_tim,ddamp_tim,bocov_tim,uv_htov_tim,sum_tim
      real,save :: exch_tim_max
      real :: btim,btimx
      real :: et_max,this_tim
      integer :: n_print_time
!
#ifdef RSL
      integer rsl_internal_microclock
      external rsl_internal_microclock
# define timef rsl_internal_microclock
#else
      real*8 :: timef
      timef()=0.
#endif
!-----------------------------------------------------------------------
!
#ifdef DEREF_KLUDGE
! SEE http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
      INTEGER :: SM31,EM31,SM32,EM32,SM33,EM33 
      INTEGER :: SM31X,EM31X,SM32X,EM32X,SM33X,EM33X
      INTEGER :: SM31Y,EM31Y,SM32Y,EM32Y,SM33Y,EM33Y
#endif
!
!-----------------------------------------------------------------------
!
! LIMIT THE NUMBER OF ARGUMENTS IF COMPILED WITH -DLIMIT_ARGS BY COPYING
! SCALAR (NON-ARRAY) ARGUMENTS OUT OF THE GRID DATA STRUCTURE INTO LOCALLY
! DEFINED COPIES (DEFINED IN EM_DUMMY_DECL.INC, ABOVE, AS THEY ARE IF THEY
! ARE ARGUMENTS).  AN EQUIVALENT INCLUDE OF EM_SCALAR_DEREFS.INC APPEARS
! AT THE END OF THE ROUTINE TO COPY BACK ANY CHNAGED NON-ARRAY VALUES.
! THE DEFINITION OF COPY_IN OR COPY_OUT BEFORE THE INCLUDE DEFINES THE
! DIRECTION OF THE COPY.  nmm_scalar_derefs IS GENERATED FROM REGISTRY.
!
!-----------------------------------------------------------------------
#define COPY_IN
#include <nmm_scalar_derefs.inc>
!-----------------------------------------------------------------------
!
! TRICK PROBLEMATIC COMPILERS INTO NOT PERFORMING COPY-IN/COPY-OUT BY ADDING
! INDICES TO ARRAY ARGUMENTS IN THE CALL STATEMENTS IN THIS ROUTINE.
! IT HAS THE EFFECT OF PASSING ONLY THE FIRST ELEMENT OF THE ARRAY, RATHER
! THAN THE ENTIRE ARRAY.  SEE:
! http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
!
!-----------------------------------------------------------------------
#include "deref_kludge.h"
!-----------------------------------------------------------------------
!
! NEEDED BY SOME COMM LAYERS, E.G. RSL.  IF NEEDED, nmm_data_calls.inc IS
! GENERATED FROM THE REGISTRY.  THE DEFINITION OF REGISTER_I1 ALLOWS
! I1 DATA TO BE COMMUNICATED IN THIS ROUTINE IF NECESSARY.
!
!-----------------------------------------------------------------------
#ifdef DM_PARALLEL
#    define REGISTER_I1
#    include <nmm_data_calls.inc>
#endif
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
      CALL WRF_GET_MYPROC(MYPROC)
!-----------------------------------------------------------------------
!
!***  OBTAIN DIMENSION INFORMATION STORED IN THE GRID DATA STRUCTURE.
!
      CALL GET_IJK_FROM_GRID(GRID                                       &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                      ,IMS,IME,JMS,JME,KMS,KME                    &
     &                      ,IPS,IPE,JPS,JPE,KPS,KPE )
!-----------------------------------------------------------------------
!
!***  COMPUTE THESE STARTING AND STOPPING LOCATIONS FOR EACH TILE AND
!***  NUMBER OF TILES.
!***  SEE: http://www.mmm.ucar.edu/wrf/WG2/topics/settiles
!
      CALL SET_TILES(GRID,IDS,IDE,JDS,JDE,IPS,IPE,JPS,JPE)
!-----------------------------------------------------------------------
!
      SIGMA=1 
      HYDRO=.FALSE.
      IHRST=GRID%GMT
      IJDS=MIN(IDS,JDS)
      IJDE=MAX(IDE,JDE)
!
      IDF=IDE-1
      JDF=JDE-1
      KDF=KDE-1
!
!-----------------------------------------------------------------------
!
!***  FOR NOW SET CONTROLS FOR TILES TO PATCHES
!
!-----------------------------------------------------------------------
      ITS=IPS
      ITE=MIN(IPE,IDF)
      JTS=JPS
      JTE=MIN(JPE,JDF)
      KTS=KPS
      KTE=MIN(KPE,KDF)
!-----------------------------------------------------------------------
!***  SET TIMING VARIABLES TO ZERO AT START OF FORECAST.
!-----------------------------------------------------------------------
      if(ntsd==0)then
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
        uv_htov_tim=0.
        exch_tim_max=0.
      endif
!-----------------------------------------------------------------------
      N_MOIST=NUM_MOIST
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
      CALL WRF_UTIL_ClockGet(GRID%DOMAIN_CLOCK,ADVANCEcOUNT=NTSD,RC=RC)
      LAST_TIME=GRID%STOP_TIME.EQ.GRID%CURRENT_TIME+GRID%STEP_TIME
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      WRITE(0,125)NTSD,NTSD*DT/3600.
  125 FORMAT(' SOLVE_NMM: TIMESTEP IS ',I5,'   TIME IS ',F7.3,' HOURS')
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!      WRITE(MESSAGE,*)' SOLVE_NMM CALLED: TIMESTEP IS ',NTSD            &
!     &         ,' FCST TIME IS',TRIM(message)
!      CALL WRF_MESSAGE(MESSAGE)
!-----------------------------------------------------------------------
      CALL WRF_GET_MYPROC(MYPE)
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL WRF_GET_NPROC(NPES)
!-----------------------------------------------------------------------
!
      btim=timef()
!
!-----------------------------------------------------------------------
!***  ZERO OUT ACCUMULATED QUANTITIES WHEN NEEDED.
!-----------------------------------------------------------------------
!
      CALL BUCKETS(NTSD,NPREC,NSRFC,NRDSW,NRDLW,NHEAT,NPHS              &
     &            ,ACPREC,CUPREC,ACSNOW,ACSNOM,SSROFF,BGROFF            &
     &            ,SFCEVP,POTEVP,SFCSHX,SFCLHX,SUBSHX,SNOPCX            &
     &            ,SFCUVX,POTFLX                                        &
     &            ,ARDSW,ASWIN,ASWOUT,ASWTOA                            &
     &            ,ARDLW,ALWIN,ALWOUT,ALWTOA                            &
     &            ,AVCNVC,AVRAIN,TCUCN,TRAIN                            &
     &            ,IDS,IDE,JDS,JDE,KDS,KDE                              &
     &            ,IMS,IME,JMS,JME,KMS,KME                              &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!
      IF(NTSD==0)THEN
        FIRST=.TRUE.
!       call hpm_init()
        btimx=timef()
!
!-----------------------------------------------------------------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
!-----------------------------------------------------------------------
!***  USE THE FOLLOWING VARIABLES TO KEEP TRACK OF EXCHANGE TIMES.
!-----------------------------------------------------------------------
!       exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!-----------------------------------------------------------------------
!
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
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_D.inc"
#endif
!-----------------
!     exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
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
!
!-----------------------------------------------------------------------
!***  ADVECTION OF T, U, AND V
!-----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_F.inc"
#    include "HALO_NMM_F1.inc"
#endif
!-----------------
!     exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
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
!
      adve_tim=adve_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  PRESSURE TENDENCY, ETA/SIGMADOT, VERTICAL PART OF OMEGA-ALPHA TERM
!-----------------------------------------------------------------------
!
      btimx=timef()
!
      CALL VTOA(                                                        &
#ifdef DM_PARALLEL
     &          GRID,                                                   &
#endif
     &          NTSD,DT,PT,ETA2                                         &
     &         ,HTM,HBM2,EF4T                                           &
     &         ,T,DWDT,RTOP,OMGALF                                      &
     &         ,PINT,DIV,PSDT,RES                                       &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      vtoa_tim=vtoa_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  VERTICAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
      btimx=timef()
!
      CALL VADZ(NTSD,DT,FIS,SIGMA,DFL,HTM,HBM2                          &
     &         ,DETA1,DETA2,PDTOP                                       &
     &         ,PINT,PDSL,PDSLO,PETDT                                   &
     &         ,RTOP,T,Q,CWM,Z,W,DWDT,PDWDT                             &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      vadz_tim=vadz_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_G.inc"
#endif
!-----------------
!     exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL HADZ(NTSD,DT,HYDRO,HTM,HBM2,DETA1,DETA2,PDTOP                &
     &         ,DX_NMM,DY_NMM,FAD                                       &
     &         ,FEW,FNS,FNE,FSE                                         &
     &         ,PDSL,U,V,W,Z                                            &
     &         ,IHE,IHW,IVE,IVW,INDX3_WRK                               &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      hadz_tim=hadz_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  ADVECTION OF W
!-----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_H.inc"
#endif
!-----------------
!     exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
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
!
      eps_tim=eps_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  ADVECTION OF Q, TKE, AND CLOUD WATER
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,IDTAD)==0)THEN
        btimx=timef()
!
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
!
        vad2_tim=vad2_tim+timef()-btimx
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(MOD(NTSD,IDTAD)==0)THEN
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_I.inc"
#endif
!-----------------
!       exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        btimx=timef()
!
        CALL HAD2(                                                     &
#if defined(DM_PARALLEL)
     &            GRID%DOMDESC,                                        &
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
!
        had2_tim=had2_tim+timef()-btimx
      ENDIF
!
!----------------------------------------------------------------------
!***  RADIATION
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,NRADS)==0.OR.MOD(NTSD,NRADL)==0)THEN
        btimx=timef()
!
        CALL RADIATION(NTSD,DT,JULDAY,JULYR,IHRST,NPHS,GLAT,GLON       &
     &                ,NRADS,NRADL                                     &
     &                ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT      &
     &                ,PD,RES,PINT,T,Q,CWM,THS,ALBEDO,EPSR             &
     &                ,F_ICE,F_RAIN                                    &
     &                ,SM,HBM2,LMH,ZERO_3D,N_MOIST,RESTRT              &
     &                ,RLWTT,RSWTT,RLWIN,RSWIN,RSWOUT                  &
     &                ,TOTSWDN,TOTLWDN,RLWTOA,RSWTOA,CZMEAN            &
     &                ,CFRACL,CFRACM,CFRACH,SIGT4                      &
     &                ,ACFRST,NCFRST,ACFRCV,NCFRCV                     &
     &                ,CUPPT,VEGFRC,SNO                                &
     &                ,HTOP,HBOT,HTOPD,HBOTD,HTOPS,HBOTS               &
     &                ,GRID,CONFIG_FLAGS                               &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                         &
     &                ,IMS,IME,JMS,JME,KMS,KME                         &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        radiation_tim=radiation_tim+timef()-btimx
      ENDIF
!
!----------------------------------------------------------------------
!***  APPLY TEMPERATURE TENDENCY DUE TO RADIATION
!----------------------------------------------------------------------
!
      btimx=timef()
!
      CALL RDTEMP(NTSD,DT,JULDAY,JULYR,IHRST,GLAT,GLON                 &
     &           ,CZEN,CZMEAN,T,RSWTT,RLWTT,HTM,HBM2                   &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      rdtemp_tim=rdtemp_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  TURBULENT PROCESSES 
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,NPHS)==0)THEN
!
        btimx=timef()
!
        CALL TURBL(NTSD,DT,NPHS,RESTRT                                 &
     &            ,N_MOIST,NUM_SOIL_LAYERS,SLDPTH,DZSOIL               &
     &            ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2,PDTOP,PT          &
!    &            ,SM,LMH,HTM,VTM,HBM2,VBM2,DX_NMM,DFL                 &
     &            ,SM,LMH,HTM,VTM,HBM2,VBM2,DX_NMM,DFRLG               &
     &            ,CZEN,CZMEAN,SIGT4,TOTLWDN,TOTSWDN,RADOT             &
     &            ,PD,RES,PINT,T,Q,CWM,F_ICE,F_RAIN,SR                 &
     &            ,Q2,U,V,THS,SST,PREC,SNO,ZERO_3D                     &
     &            ,FIS,Z0,Z0BASE,USTAR,PBLH,LPBL,EL_MYJ                &
     &            ,EXCH_H,AKHS,AKMS,AKHS_OUT,AKMS_OUT                  &
     &            ,THZ0,QZ0,UZ0,VZ0,QS                                 &
     &            ,STC,SMC,CMC,SMSTAV,SMSTOT,SSROFF,BGROFF             &
     &            ,IVGTYP,ISLTYP,VEGFRC,SHDMIN,SHDMAX,GRNFLX           &
     &            ,SFCEXC,ACSNOW,ACSNOM,SNOPCX,SICE,TG,SOILTB          &
     &            ,ALBASE,MXSNAL,ALBEDO,SH2O,SI,EPSR                   &
     &            ,U10,V10,TH10,Q10,TSHLTR,QSHLTR,PSHLTR               &
     &            ,TWBS,QWBS,SFCSHX,SFCLHX,SFCEVP                      &
     &            ,POTEVP,POTFLX,SUBSHX                                &
     &            ,APHTIM,ARDSW,ARDLW,ASRFC                            &
     &            ,RSWOUT,RSWTOA,RLWTOA                                &
     &            ,ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA             &
     &            ,UZ0H,VZ0H,DUDT,DVDT                                 & 
     &            ,GRID,CONFIG_FLAGS                                   &
     &            ,IHE,IHW,IVE,IVW                                     &
     &            ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &            ,IMS,IME,JMS,JME,KMS,KME                             &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        turbl_tim=turbl_tim+timef()-btimx
!
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
# include "HALO_NMM_TURBL_A.inc"
#endif
!-----------------
#ifdef DM_PARALLEL
# include "HALO_NMM_TURBL_B.inc"
#endif
!-----------------
!       exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
!***  INTERPOLATE WINDS FROM H POINTS BACK TO V POINTS.
!
        btimx=timef()
        CALL UV_H_TO_V(NTSD,DT,NPHS,UZ0H,VZ0H,UZ0,VZ0                   &
     &                ,DUDT,DVDT,U,V,HBM2,VTM,IVE,IVW                   &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        uv_htov_tim=uv_htov_tim+timef()-btimx
!
!----------------------------------------------------------------------
!*** STORE ORIGINAL TEMPERATURE ARRAY
!----------------------------------------------------------------------
!
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_J.inc"
#endif
!-----------------
!       exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        ICLTEND=-1
        btimx=timef()
! 
        CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                         &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        cltend_tim=cltend_tim+timef()-btimx
      ENDIF
!
!----------------------------------------------------------------------
!***  CONVECTIVE PRECIPITATION
!----------------------------------------------------------------------
      IF(MOD(NTSD,NCNVC)==0.AND.                                       &
     &   CONFIG_FLAGS%CU_PHYSICS.EQ.KFETASCHEME)THEN
!
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_C.inc"
#endif
!-----------------
!       exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max          &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
      ENDIF
!
      btimx=timef()
!
      IF(NCNVC/=999)THEN
        CALL CUCNVC(NTSD,DT,NCNVC,GPS,RESTRT,HYDRO                     &
     &             ,CLDEFI,LMH,N_MOIST,ENSDIM                          &
     &             ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                  &
     &             ,F_ICE,F_RAIN                                       &
     &             ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TCUCN                 &
     &             ,OMGALF,U,V,VTM,W,Z,FIS,W0AVG                       &
     &             ,PREC,ACPREC,CUPREC,CUPPT                           &
     &             ,SM,HBM2,LPBL,CNVBOT,CNVTOP                         &
     &             ,HTOP,HBOT,HTOPD,HBOTD,HTOPS,HBOTS                  &
     &             ,AVCNVC,ACUTIM,ZERO_3D,IHE,IHW                      &
     &             ,GRID,CONFIG_FLAGS                                  &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        cucnvc_tim=cucnvc_tim+timef()-btimx
      ENDIF
!
!----------------------------------------------------------------------
!***  GRIDSCALE MICROPHYSICS (CONDENSATION & PRECIPITATION)
!----------------------------------------------------------------------
!
      IF(MOD(NTSD,NPHS)==0)THEN
        btimx=timef()
!
        CALL GSMDRIVE(NTSD,DT,NPHS,N_MOIST                             &
     &               ,DX_NMM(ITS,JC),DY,LMH,SM,HBM2,FIS                &
     &               ,DETA1,DETA2,AETA1,AETA2,ETA1,ETA2                &
     &               ,PDTOP,PT,PD,RES,PINT,T,Q,CWM,TRAIN               &
     &               ,F_ICE,F_RAIN,F_RIMEF,SR                          &
     &               ,PREC,ACPREC,AVRAIN,ZERO_3D                       &
     &               ,MP_RESTART_STATE                                 &
     &               ,TBPVS_STATE                                      &
     &               ,TBPVS0_STATE                                     &
     &               ,GRID,CONFIG_FLAGS                                &
     &               ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        gsmdrive_tim=gsmdrive_tim+timef()-btimx
!----------------------------------------------------------------------
!***  CALCULATE TEMP TENDENCIES AND RESTORE ORIGINAL TEMPS
!----------------------------------------------------------------------
!       btim=timef()
        ICLTEND=0
        btimx=timef()
!
        CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                         &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        cltend_tim=cltend_tim+timef()-btimx
      ENDIF
!
!----------------------------------------------------------------------
!***  UPDATE TEMP TENDENCIES FROM CLOUD PROCESSES EVERY TIME STEP
!----------------------------------------------------------------------
!
      ICLTEND=1
      btimx=timef()
!
      CALL CLTEND(ICLTEND,NPHS,T,T_OLD,T_ADJ                           &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      cltend_tim=cltend_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  LATERAL DIFFUSION
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_K.inc"
#endif
!-----------------
!     exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL HDIFF(NTSD,DT,FIS,DY_NMM,HDAC,HDACV                         &
     &          ,HTM,HBM2,VTM,DETA1,SIGMA                              &
     &          ,T,Q,U,V,Q2,Z,W                                        &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      hdiff_tim=hdiff_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT HEIGHT POINTS
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_L.inc"
#endif
!-----------------
!     exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
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
!
      bocoh_tim=bocoh_tim+timef()-btimx
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
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
!-----------------
!     exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
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
!
      pfdht_tim=pfdht_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  DIVERGENCE DAMPING
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_B.inc"
#endif
!-----------------
!     exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max            &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL DDAMP(NTSD,DT,DETA1,DETA2,PDSL,PDTOP,DIV,HBM2,VTM           &
     &          ,T,U,V,DDMPU,DDMPV                                     &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      ddamp_tim=ddamp_tim+timef()-btimx
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      IF(FIRST.AND.NTSD==0)THEN
        FIRST=.FALSE.
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
!-----------------
!       exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max          &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        GO TO 2000
      ENDIF
!
!----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT VELOCITY POINTS
!----------------------------------------------------------------------
!
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_C.inc"
#endif
!-----------------
!     exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max            &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL BOCOV(NTSD,DT,LB,VTM,U_B,V_B,U_BT,V_BT                      &
     &          ,U,V                                                   &
     &          ,IJDS,IJDE,SPEC_BDY_WIDTH                              &
     &          ,IHE,IHW,IVE,IVW,INDX3_WRK                             &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE )
!
      bocov_tim=bocov_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  COPY THE NMM VARIABLE Q2 TO THE WRF VARIABLE TKE_MYJ
!----------------------------------------------------------------------
!
!     DO J=JTS,JTE
!     DO K=KTS,KTE
!     DO I=ITS,ITE
!       TKE_MYJ(I,K,J)=0.5*Q2(I,K,J) !TKE is q squared over 2
!     ENDDO
!     ENDDO
!     ENDDO
!
!----------------------------------------------------------------------
!
      solve_tim=solve_tim+timef()-btim
!
!----------------------------------------------------------------------
!***  PRINT TIMING VARIABLES WHEN DESIRED.
!----------------------------------------------------------------------
!
!     sum_tim=pdte_tim+adve_tim+vtoa_tim+vadz_tim+hadz_tim+eps_tim     &
!    &       +vad2_tim+had2_tim+radiation_tim+rdtemp_tim+turbl_tim     &
!    &       +cltend_tim+cucnvc_tim+gsmdrive_tim+hdiff_tim             &
!    &       +bocoh_tim+pfdht_tim+ddamp_tim+bocov_tim+uv_htov_tim      &
!    &       +exch_tim
!
      n_print_time=nint(3600./dt)   ! Print stats once per hour
!
      if(mod(ntsd,n_print_time)==0)then
!       write(0,*)' ntsd=',ntsd,' solve_tim=',solve_tim*1.e-3          &
!    &           ,' sum_tim=',sum_tim*1.e-3
!       write(0,*)' pdte_tim=',pdte_tim*1.e-3,' pct=',pdte_tim/sum_tim*100.
!       write(0,*)' adve_tim=',adve_tim*1.e-3,' pct=',adve_tim/sum_tim*100.
!       write(0,*)' vtoa_tim=',vtoa_tim*1.e-3,' pct=',vtoa_tim/sum_tim*100.
!       write(0,*)' vadz_tim=',vadz_tim*1.e-3,' pct=',vadz_tim/sum_tim*100.
!       write(0,*)' hadz_tim=',hadz_tim*1.e-3,' pct=',hadz_tim/sum_tim*100.
!       write(0,*)' eps_tim=',eps_tim*1.e-3,' pct=',eps_tim/sum_tim*100.
!       write(0,*)' vad2_tim=',vad2_tim*1.e-3,' pct=',vad2_tim/sum_tim*100.
!       write(0,*)' had2_tim=',had2_tim*1.e-3,' pct=',had2_tim/sum_tim*100.
!       write(0,*)' radiation_tim=',radiation_tim*1.e-3,' pct=',radiation_tim/sum_tim*100.
!       write(0,*)' rdtemp_tim=',rdtemp_tim*1.e-3,' pct=',rdtemp_tim/sum_tim*100.
!       write(0,*)' turbl_tim=',turbl_tim*1.e-3,' pct=',turbl_tim/sum_tim*100.
!       write(0,*)' cltend_tim=',cltend_tim*1.e-3,' pct=',cltend_tim/sum_tim*100.
!       write(0,*)' cucnvc_tim=',cucnvc_tim*1.e-3,' pct=',cucnvc_tim/sum_tim*100.
!       write(0,*)' gsmdrive_tim=',gsmdrive_tim*1.e-3,' pct=',gsmdrive_tim/sum_tim*100.
!       write(0,*)' hdiff_tim=',hdiff_tim*1.e-3,' pct=',hdiff_tim/sum_tim*100.
!       write(0,*)' bocoh_tim=',bocoh_tim*1.e-3,' pct=',bocoh_tim/sum_tim*100.
!       write(0,*)' pfdht_tim=',pfdht_tim*1.e-3,' pct=',pfdht_tim/sum_tim*100.
!       write(0,*)' ddamp_tim=',ddamp_tim*1.e-3,' pct=',ddamp_tim/sum_tim*100.
!       write(0,*)' bocov_tim=',bocov_tim*1.e-3,' pct=',bocov_tim/sum_tim*100.
!       write(0,*)' uv_h_to_v_tim=',uv_htov_tim*1.e-3,' pct=',uv_htov_tim/sum_tim*100.
!       write(0,*)' exch_tim=',exch_tim*1.e-3,' pct=',exch_tim/sum_tim*100.
!       call time_stats(exch_tim,'exchange',ntsd,mype,npes,mpi_comm_comp)
!       write(0,*)' exch_tim_max=',exch_tim_max*1.e-3
!
        call field_stats(t,mype,mpi_comm_comp                          &
     &                  ,ids,ide,jds,jde,kds,kde                       &
     &                  ,ims,ime,jms,jme,kms,kme                       &
     &                  ,its,ite,jts,jte,kts,kte)
      endif
!
!     if(last_time)then
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
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE TWR(ARRAY,KK,FIELD,NTSD,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
      IMPLICIT NONE
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
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
      IF(NTSD<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
      FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)
!
      IF(MYPE==0)THEN
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
      IF(MYPE==0)THEN
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
      IF(MYPE==0)THEN
!
        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==0)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO
!
      ENDIF
!
!----------------------------------------------------------------------
  500 CONTINUE
!
      IF(MYPE==0)CLOSE(IUNIT)
!----------------------------------------------------------------------
!
      END SUBROUTINE TWR
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE EXIT(NAME,T,Q,U,V,Q2,NTSD,MYPE,MPI_COMM_COMP          &
     &               ,IDS,IDE,JDS,JDE,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
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
     &                     ,MYPE,MPI_COMM_COMP,NTSD      
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: T,Q,U,V,Q2
      CHARACTER(*),INTENT(IN) :: NAME
!
      INTEGER :: I,J,K,IEND,IERR,IRET
      CHARACTER(256) :: ERRMESS
      LOGICAL :: E_BDY,S_BDY
!----------------------------------------------------------------------
      IRET=0
  100 FORMAT(' EXIT ',A,' AT NTSD=',I5)
      IEND=ITE
      S_BDY=(JTS==JDS)
      E_BDY=(ITE==IDE-1)
!
      DO J=JTS,JTE
      IEND=ITE
      DO K=KTS,KTE
      IF(E_BDY.AND.MOD(J,2)==0)IEND=ITE-1
!
      DO I=ITS,IEND
        IF(T(I,K,J)>330..OR.T(I,K,J)<180..OR.T(I,K,J)/=T(I,K,J))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,200)I,J,K,T(I,K,J),MYPE,NTSD
  200     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' T=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,205)NAME,T(I,K,J),I,K,J,MYPE
  205     FORMAT(' EXIT ',A,' TEMPERATURE=',E12.5                      &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL WRF_ERROR_FATAL(ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ELSEIF(Q(I,K,J)<-1.E-4.OR.Q(I,K,J)>30.E-3                      &
     &         .OR.Q(I,K,J)/=Q(I,K,J))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,300)I,J,K,Q(I,K,J),MYPE,NTSD
  300     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' Q=',E12.5      &
     &,          ' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,305)NAME,Q(I,K,J),I,K,J,MYPE
  305     FORMAT(' EXIT ',A,' SPEC HUMIDITY=',E12.5                    &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL WRF_ERROR_FATAL(ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!
      DO J=JTS,JTE
      IEND=ITE
      DO K=KTS,KTE
      IF(E_BDY.AND.MOD(J,2)==1)IEND=ITE-1
      DO I=ITS,IEND
        IF(ABS(U(I,K,J))>125..OR.ABS(V(I,K,J))>125.                    &
     &         .OR.U(I,K,J)/=U(I,K,J).OR.V(I,K,J)/=V(I,K,J))THEN
          WRITE(0,100)NAME,NTSD
          WRITE(0,400)I,J,K,U(I,K,J),V(I,K,J),MYPE,NTSD
  400     FORMAT(' BAD VALUE I=',I3,' J=',I3,' K=',I2,' U=',E12.5      &
     &,          ' V=',E12.5,' MYPE=',I3,' NTSD=',I5)
          IRET=666
          return
!         WRITE(ERRMESS,405)NAME,U(I,K,J),V(I,K,J),I,K,J,MYPE
  405     FORMAT(' EXIT ',A,' U=',E12.5,' V=',E12.5                    &
     &,          ' AT (',I3,',',I2,',',I3,')',' MYPE=',I3)
!         CALL WRF_ERROR_FATAL(ERRMESS)
!         CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
      END SUBROUTINE EXIT
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE TIME_STATS(TIME_LCL,NAME,NTSD,MYPE,NPES,MPI_COMM_COMP)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE,NPES,NTSD
      REAL,INTENT(IN) :: TIME_LCL
!
      CHARACTER(*),INTENT(IN) :: NAME
!
!*** LOCAL VARIABLES
!
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
      INTEGER,ALLOCATABLE,DIMENSION(:) :: ID_PE,IPE_SORT
!
      INTEGER :: IPE,IPE_MAX,IPE_MEDIAN,IPE_MIN,IRECV,IRTN,ISEND       &
     &          ,N,N_MEDIAN,NLEN
!
      REAL,ALLOCATABLE,DIMENSION(:) :: TIME,SORT_TIME
      REAL,DIMENSION(2) :: REMOTE
      REAL :: TIME_MAX,TIME_MEAN,TIME_MEDIAN,TIME_MIN
!
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(25) :: TITLE
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(NTSD<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(NTSD<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(NTSD<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(NTSD<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(NTSD<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)NTSD
      TITLE=NAME//'_'//TIMESTEP(1:NLEN)
!
!----------------------------------------------------------------------
!
      IF(MYPE==0)THEN
        ALLOCATE(TIME(1:NPES))
        ALLOCATE(SORT_TIME(1:NPES))
        ALLOCATE(ID_PE(1:NPES))
        ALLOCATE(IPE_SORT(1:NPES))
!
        TIME(1)=TIME_LCL
        ID_PE(1)=MYPE
!
!***  COLLECT TIMES AND PE VALUES FROM OTHER PEs
!
        DO IPE=1,NPES-1
          CALL MPI_RECV(REMOTE,2,MPI_REAL,IPE,IPE                      &
     &                 ,MPI_COMM_COMP,JSTAT,IRECV)
!
          TIME(IPE+1)=REMOTE(1)
          ID_PE(IPE+1)=NINT(REMOTE(2))
        ENDDO
!
!***  NOW GET STATS.
!***  FIRST THE MAX, MIN, AND MEAN TIMES.
!
        TIME_MEAN=0.
        TIME_MAX=-1.
        TIME_MIN=1.E10
        IPE_MAX=-1
        IPE_MIN=-1
!
        DO N=1,NPES
          TIME_MEAN=TIME_MEAN+TIME(N)
!
          IF(TIME(N)>TIME_MAX)THEN
            TIME_MAX=TIME(N)
            IPE_MAX=ID_PE(N)
          ENDIF
!
          IF(TIME(N)<TIME_MIN)THEN
            TIME_MIN=TIME(N)
            IPE_MIN=ID_PE(N)
          ENDIF
!
        ENDDO
!
        TIME_MAX=TIME_MAX*1.E-3
        TIME_MIN=TIME_MIN*1.E-3
        TIME_MEAN=TIME_MEAN*1.E-3/REAL(NPES)
!
!***  THEN THE MEDIAN TIME.
!
        CALL SORT(TIME,NPES,SORT_TIME,IPE_SORT)
        N_MEDIAN=(NPES+1)/2
        TIME_MEDIAN=SORT_TIME(N_MEDIAN)*1.E-3
        IPE_MEDIAN=IPE_SORT(N_MEDIAN)
!
!----------------------------------------------------------------------
      ELSE
!
!***  SEND TIME AND PE VALUE TO PE0.
!
        REMOTE(1)=TIME_LCL
        REMOTE(2)=REAL(MYPE)
!
        CALL MPI_SEND(REMOTE,2,MPI_REAL,0,MYPE,MPI_COMM_COMP,ISEND)
!
      ENDIF
!----------------------------------------------------------------------
!
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
!
!***  WRITE RESULTS
!
      IF(MYPE==0)THEN
        WRITE(0,100)TITLE
        WRITE(0,105)TIME_MAX,IPE_MAX
        WRITE(0,110)TIME_MIN,IPE_MIN
        WRITE(0,115)TIME_MEDIAN,IPE_MEDIAN
        WRITE(0,120)TIME_MEAN
  100   FORMAT(' Time for ',A)
  105   FORMAT(' Maximum=',G11.5,' for PE ',I2.2)
  110   FORMAT(' Minimum=',G11.5,' for PE ',I2.2)
  115   FORMAT(' Median =',G11.5,' for PE ',I2.2)
  120   FORMAT(' Mean   =',G11.5)
      ENDIF
!----------------------------------------------------------------------
!
      END SUBROUTINE TIME_STATS
!
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE SORT(DATA,NPES,DATA_SORTED,IPE_SORTED)
!----------------------------------------------------------------------
!***
!***  SORT DATA FROM MULTIPLE PEs.  SEND BACK THE SORTED DATA ITEMS
!***  ALONG WITH THE ASSOCIATED TASK IDs.
!***
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NPES
      REAL,DIMENSION(NPES),INTENT(IN) :: DATA
!
      INTEGER,DIMENSION(NPES),INTENT(OUT) :: IPE_SORTED
      REAL,DIMENSION(NPES),INTENT(OUT) :: DATA_SORTED
!----------------------------------------------------------------------
      TYPE :: DATA_LINK
        REAL :: VALUE
        INTEGER :: IPE
        TYPE(DATA_LINK),POINTER :: NEXT_VALUE
      END TYPE
!----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!----------------------------------------------------------------------
      INTEGER :: ISTAT,N
!
      TYPE(DATA_LINK),POINTER :: HEAD,TAIL  ! Smallest, largest
      TYPE(DATA_LINK),POINTER :: PTR_NEW    ! Each new value
      TYPE(DATA_LINK),POINTER :: PTR1,PTR2  ! Working pointers
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      pe_loop: DO N=1,NPES
        ALLOCATE(PTR_NEW,STAT=ISTAT)  ! Location for next data items
        PTR_NEW%VALUE=DATA(N)
        PTR_NEW%IPE=N-1
!
!----------------------------------------------------------------------
!***  DETERMINE WHERE IN LIST TO INSERT VALUE.
!***  FIRST THE INITIAL DATA VALUE.
!----------------------------------------------------------------------
!
!       main: IF(.NOT.ASSOCIATED(HEAD))THEN
        main: IF(N==1)THEN
          HEAD=>PTR_NEW
          TAIL=>HEAD
          NULLIFY(PTR_NEW%NEXT_VALUE)
!
!----------------------------------------------------------------------
!***  THE NEW VALUE IS LESS THAN THE SMALLEST VALUE ALREADY SORTED.
!----------------------------------------------------------------------
!
        ELSE
          check: IF(PTR_NEW%VALUE<HEAD%VALUE)THEN
            PTR_NEW%NEXT_VALUE=>HEAD
            HEAD=>PTR_NEW
!
!----------------------------------------------------------------------
!***  THE NEW VALUE IS GREATER THAN THE LARGEST VALUE ALREADY SORTED.
!----------------------------------------------------------------------
!
          ELSEIF(PTR_NEW%VALUE>=TAIL%VALUE)THEN
            TAIL%NEXT_VALUE=>PTR_NEW  ! This is what connects the former
                                      ! final value in the list to
                                      ! the new value being appended.
            TAIL=>PTR_NEW
            NULLIFY(TAIL%NEXT_VALUE)
!
!----------------------------------------------------------------------
!***  THE NEW VALUE IS IN BETWEEN VALUES ALREADY SORTED.
!----------------------------------------------------------------------
!
          ELSE
            PTR1=>HEAD
            PTR2=>PTR1%NEXT_VALUE
!
            search: DO
              IF((PTR_NEW%VALUE>=PTR1%VALUE).AND.                      &
     &           (PTR_NEW%VALUE<PTR2%VALUE))THEN
                PTR_NEW%NEXT_VALUE=>PTR2
                PTR1%NEXT_VALUE=>PTR_NEW
                EXIT search
              ENDIF
!
              PTR1=>PTR2
              PTR2=>PTR2%NEXT_VALUE
            ENDDO search
!
          ENDIF check
!
        ENDIF main
!
      ENDDO pe_loop
!
!----------------------------------------------------------------------
!***  COLLECT THE SORTED NUMBERS FROM THE LINKED LIST.
!----------------------------------------------------------------------
!
      PTR1=>HEAD
!
      DO N=1,NPES
!       IF(.NOT.ASSOCIATED(PTR_NEW))EXIT
        DATA_SORTED(N)=PTR1%VALUE
        IPE_SORTED(N)=PTR1%IPE
        PTR1=>PTR1%NEXT_VALUE
      ENDDO
!
      DEALLOCATE(PTR_NEW)
      NULLIFY (HEAD,TAIL,PTR1,PTR2)
!----------------------------------------------------------------------
      END SUBROUTINE SORT
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE FIELD_STATS(FIELD,MYPE,MPI_COMM_COMP                  &
     &                      ,IDS,IDE,JDS,JDE,KDS,KDE                   &
     &                      ,IMS,IME,JMS,JME,KMS,KME                   &
     &                      ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!***
!***  GENERATE STANDARD STATISTICS FOR THE DESIRED FIELD.
!***
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
      INCLUDE "mpif.h"
!----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: FIELD
!
!----------------------------------------------------------------------
!***  LOCAL
!----------------------------------------------------------------------
!
      INTEGER,PARAMETER :: DOUBLE=SELECTED_REAL_KIND(15,300)
!
      INTEGER :: I,IEND,IRTN,I_BY_J,J,K,KFLIP
!
      REAL :: FIKJ,FMAXK,FMINK
      REAL(KIND=DOUBLE) :: F_MEAN,POINTS,RMS,ST_DEV,SUMFK,SUMF2K
      REAL,DIMENSION(KTS:KTE) :: FMAX,FMAX_0,FMIN,FMIN_0
      REAL(KIND=DOUBLE),DIMENSION(KTS:KTE) :: SUMF,SUMF_0,SUMF2,SUMF2_0
!----------------------------------------------------------------------
!
      I_BY_J=(IDE-IDS)*(JDE-JDS)-(JDE-JDS-1)/2  ! This assumes that
                                                ! IDE and JDE are each
                                                ! one greater than the
                                                ! true grid size.
!
      layer_loop:  DO K=KTS,KTE
!
        FMAXK=-1.E10
        FMINK=1.E10
        SUMFK=0.
        SUMF2K=0.
!
        DO J=JTS,JTE
          IEND=MIN(ITE,IDE-1)
          IF(MOD(J,2)==0.AND.ITE==IDE-1)IEND=IEND-1
          DO I=ITS,IEND
            FIKJ=FIELD(I,K,J)
            FMAXK=MAX(FMAXK,FIKJ)
            FMINK=MIN(FMINK,FIKJ)
            SUMFK=SUMFK+FIKJ
            SUMF2K=SUMF2K+FIKJ*FIKJ
          ENDDO
        ENDDO
!
        FMAX(K)=FMAXK
        FMIN(K)=FMINK
        SUMF(K)=SUMFK
        SUMF2(K)=SUMF2K
!
      ENDDO layer_loop
!
!----------------------------------------------------------------------
!***  GLOBAL STATS
!----------------------------------------------------------------------
!
      CALL MPI_REDUCE(SUMF,SUMF_0,KTE,MPI_REAL8,MPI_SUM,0              &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(SUMF2,SUMF2_0,KTE,MPI_REAL8,MPI_SUM,0            &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMAX,FMAX_0,KTE,MPI_REAL,MPI_MAX,0               &
     &               ,MPI_COMM_COMP,IRTN)
      CALL MPI_REDUCE(FMIN,FMIN_0,KTE,MPI_REAL,MPI_MIN,0               &
     &               ,MPI_COMM_COMP,IRTN)
!
      IF(MYPE==0)THEN
        POINTS=I_BY_J
        DO K=KTE,KTS,-1
          F_MEAN=SUMF_0(K)/POINTS
          ST_DEV=SQRT((POINTS*SUMF2_0(K)-SUMF_0(K)*SUMF_0(K))/         &
     &                (POINTS*(POINTS-1)))
          RMS=SQRT(SUMF2_0(K)/POINTS)
          KFLIP=KTE-K+1
          WRITE(0,101)KFLIP,FMAX_0(K),FMIN_0(K)
          WRITE(0,102)F_MEAN,ST_DEV,RMS
  101     FORMAT(' LAYER=',I2,' MAX=',E13.6,' MIN=',E13.6)
  102     FORMAT(9X,' MEAN=',E13.6,' STDEV=',E13.6,' RMS=',E13.6)
        ENDDO
      ENDIF
!----------------------------------------------------------------------
      END SUBROUTINE FIELD_STATS
!----------------------------------------------------------------------
