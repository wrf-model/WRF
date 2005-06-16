!#define NO_RESTRICT_ACCEL
!#define NO_GFDLETAINIT
!#define NO_UPSTREAM_ADVECTION
!----------------------------------------------------------------------
!
      SUBROUTINE START_DOMAIN_NMM(GRID, allowed_to_read ,              &
!
#include <nmm_dummy_args.inc>
!
     &           )
!----------------------------------------------------------------------
!
      USE MODULE_DOMAIN
      USE MODULE_DRIVER_CONSTANTS
      USE module_model_constants
      USE MODULE_CONFIGURE
      USE MODULE_WRF_ERROR
      USE MODULE_MPP
      USE MODULE_CTLBLK
      USE MODULE_DM
!
      USE MODULE_IGWAVE_ADJUST,ONLY: PDTE, PFDHT, DDAMP
      USE MODULE_ADVECTION,    ONLY: ADVE, VAD2, HAD2
      USE MODULE_NONHY_DYNAM,  ONLY: VADZ, HADZ
      USE MODULE_DIFFUSION_NMM,ONLY: HDIFF
      USE MODULE_BNDRY_COND,   ONLY: BOCOH, BOCOV
      USE MODULE_PHYSICS_INIT
      USE MODULE_RA_GFDLETA
!
      USE MODULE_EXT_INTERNAL
!
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!----------------------------------------------------------------------
!***
!***  Arguments
!***
      TYPE(DOMAIN),INTENT(INOUT) :: GRID
      LOGICAL , INTENT(IN)       :: allowed_to_read
!
#include <nmm_dummy_decl.inc>
!
      TYPE(GRID_CONFIG_REC_TYPE) :: CONFIG_FLAGS
!
!
!***
!***  LOCAL DATA
!***
      INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,IPS,IPE,JPS,JPE,KPS,KPE
!
      INTEGER :: ERROR,LOOP

      REAL,ALLOCATABLE,DIMENSION(:) :: PHALF
!
      REAL :: EPSB=0.1,EPSIN=9.8
!
      INTEGER :: JHL=7
!
      INTEGER :: I,IEND,IER,IERR,IFE,IFS,IHH,IHL,IHRSTB,II,IRTN        &
     &          ,ISIZ1,ISIZ2,ISTART,IX,J,J00,JFE,JFS,JHH,JJ             &
     &          ,JM1,JM2,JM3,JP1,JP2,JP3,JX                             &
     &          ,K,K400,KBI,KBI2,KCCO2,KNT,KNTI,KOFF,KOFV               &
     &          ,LB,LLMH,LMHK,LMVK,LRECBC                               &
     &          ,N,NMAP,NRADLH,NRADSH,NREC,NS,RECL,STAT                 &
     &          ,STEPBL,STEPCU,STEPRA
!
      INTEGER :: ILPAD2,IRPAD2,JBPAD2,JTPAD2
      INTEGER :: ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(3) :: LPTOP
!
      REAL :: ADDL,APELM,APELMNW,APEM1,CAPA,CLOGES,DPLM,DZLM,EPS,ESE   &
     &       ,FAC1,FAC2,PDIF,PLM,PM1,PSFCK,PSS,PSUM,QLM,RANG,RCOS1      &
     &       ,RCOS2,RSIN1,SLPM,TERM1,THLM,TIME,TLM,TSFCK,ULM,VLM
!
!!!   REAL :: BLDT,CWML,EXNSFC,G_INV,PLYR,PSFC,ROG,SFCZ,THSIJ,TL
      REAL :: CWML,EXNSFC,G_INV,PLYR,PSURF,ROG,SFCZ,THSIJ,TL
      REAL :: TSTART,TEND,TPREC,THEAT,TCLOD,TRDSW,TRDLW,TSRFC

!
!!!   REAL,ALLOCATABLE,DIMENSION(:,:) :: RAINBL,RAINNC,RAINNC           &
      INTEGER,ALLOCATABLE,DIMENSION(:,:) :: LOWLYR
      REAL,ALLOCATABLE,DIMENSION(:) :: SFULL,SMID
!state    real   DZS             l        dyn_em      -         Z     ir 
!state    real  CLDFRA          ikj      dyn_em        1         -      r
!state    real  RQCBLTEN        ikj      dyn_em        1         -      r
!state    real  RQIBLTEN        ikj      dyn_em        1         -      r
!state    real  RQVBLTEN        ikj      dyn_em        1         -      r
!state    real  RTHBLTEN        ikj      dyn_em        1         -      r
!state    real  RUBLTEN         ikj      dyn_em        1         -      r
!state    real  RVBLTEN         ikj      dyn_em        1         -      r
!state    real  RQCCUTEN        ikj      dyn_em        1         -      r
!state    real  RQICUTEN        ikj      dyn_em        1         -      r
!state    real  RQRCUTEN        ikj      dyn_em        1         -      r
!state    real  RQSCUTEN        ikj      dyn_em        1         -      r
!state    real  RQVCUTEN        ikj      dyn_em        1         -      r
!state    real  RTHCUTEN        ikj      dyn_em        1         -      r
!state    real  RTHRATEN        ikj      dyn_em        1         -      r
!state    real  RTHRATENLW      ikj      dyn_em        1         -      r
!state    real  RTHRATENSW      ikj      dyn_em        1         -      r
!state    real   TSLB           ilj       dyn_em      1         Z     irh
!state    real   ZS              l        dyn_em      -         Z     ir 
      REAL,ALLOCATABLE,DIMENSION(:) :: DZS,ZS
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: CLDFRA                       &
     &                                  ,RQCBLTEN,RQIBLTEN              &
     &                                  ,RQVBLTEN,RTHBLTEN              &
     &                                  ,RUBLTEN,RVBLTEN                &
     &                                  ,RQCCUTEN,RQICUTEN,RQRCUTEN     &
     &                                  ,RQSCUTEN,RQVCUTEN,RTHCUTEN     &
     &                                  ,RTHRATEN                       &
     &                                  ,RTHRATENLW,RTHRATENSW          &
     &                                  ,TSLB
      REAL,ALLOCATABLE,DIMENSION(:,:) :: EMISS,GLW,GSW,HFX              &
     &                                  ,MAVAIL,NCA                     &
     &                                  ,QFX,RAINBL,RAINC,RAINNC        &
     &                                  ,RAINCV,RAINNCV                 &
     &                                  ,SNOWC,THC,TMN,TSFC             &
     &                                  ,XLAND,XLAT,XLONG

      REAL,ALLOCATABLE,DIMENSION(:,:) :: Z0_DUM
!
!!!   REAL,ALLOCATABLE,DIMENSION(:,:,:) :: W0AVG,ZINT,ZMID
#if 0
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: W0AVG
#endif
      LOGICAL :: E_BDY,N_BDY,S_BDY,W_BDY,WARM_RAIN
      integer :: jam,retval
      character(20) :: seeout="hi08.t00z.nhbmeso"
      real :: dummyx(791)
      integer myproc
      real :: dsig,dsigsum,pdbot,pdtot,rpdtot
      real :: fisx,ht,prodx,rg
      integer :: i_t=096,j_t=195,n_t=11
      integer :: i_u=49,j_u=475,n_u=07
      integer :: i_v=49,j_v=475,n_v=07

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

#include "deref_kludge.h"

!
!----------------------------------------------------------------------
#define COPY_IN
#include <nmm_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    include <nmm_data_calls.inc>
#endif
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      CALL GET_IJK_FROM_GRID(GRID,                                     &
     &                       IDS,IDE,JDS,JDE,KDS,KDE,                  &
     &                       IMS,IME,JMS,JME,KMS,KME,                  &
     &                       IPS,IPE,JPS,JPE,KPS,KPE)
!
      ITS=IPS
      ITE=IPE
      JTS=JPS
      JTE=JPE
      KTS=KPS
      KTE=KPE

      CALL model_to_grid_config_rec(grid%id,model_config_rec           &
     &                             ,config_flags)
!
        RESTRT=config_flags%restart
        write(0,*) 'set RESTRT to: ', RESTRT

#if 1
      IF(IME.GT. NMM_MAX_DIM )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm ime (',ime,') > ',NMM_MAX_DIM,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL WRF_ERROR_FATAL(wrf_err_message)
      ENDIF
!
      IF(JME.GT. NMM_MAX_DIM )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm jme (',jme,') > ',NMM_MAX_DIM,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL WRF_ERROR_FATAL(wrf_err_message)
      ENDIF
#else
      IF(IMS.GT.-2.OR.IME.GT. NMM_MAX_DIM )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm ims(',ims,' > -2 or ime (',ime,') > ',NMM_MAX_DIM,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL WRF_ERROR_FATAL(wrf_err_message)
      ENDIF
!
      IF(JMS.GT.-2.OR.JME.GT. NMM_MAX_DIM )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm jms(',jms,' > -2 or jme (',jme,') > ',NMM_MAX_DIM,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL WRF_ERROR_FATAL(wrf_err_message)
      ENDIF
#endif
!
!---------------------------------------------------------------------- 
!
      WRITE(0,196)IHRST,IDAT
      WRITE(LIST,196)IHRST,IDAT
  196 FORMAT(' FORECAST BEGINS ',I2,' GMT ',2(I2,'/'),I4)
!!!!!!tlb
!!!! For now, set NPES to 1
      NPES=1
!!!!!!tlb
      MY_IS_GLB=IPS
      MY_IE_GLB=IPE-1
      MY_JS_GLB=JPS
      MY_JE_GLB=JPE-1
!
      IM=IPE-1
      JM=JPE-1
!!!!!!!!!
!! All "my" variables defined below have had the IDE or JDE specification
!! reduced by 1
!!!!!!!!!!!

      MYIS=MAX(IDS,IPS)
      MYIE=MIN(IDE-1,IPE)
      MYJS=MAX(JDS,JPS)
      MYJE=MIN(JDE-1,JPE)

      MYIS1  =MAX(IDS+1,IPS)
      MYIE1  =MIN(IDE-2,IPE)
      MYJS2  =MAX(JDS+2,JPS)
      MYJE2  =MIN(JDE-3,JPE)
!
      MYIS_P1=MAX(IDS,IPS-1)
      MYIE_P1=MIN(IDE-1,IPE+1)
      MYIS_P2=MAX(IDS,IPS-2)
      MYIE_P2=MIN(IDE-1,IPE+2)
      MYIS_P3=MAX(IDS,IPS-3)
      MYIE_P3=MIN(IDE-1,IPE+3)
      MYJS_P3=MAX(JDS,JPS-3)
      MYJE_P3=MIN(JDE-1,JPE+3)
      MYIS_P4=MAX(IDS,IPS-4)
      MYIE_P4=MIN(IDE-1,IPE+4)
      MYJS_P4=MAX(JDS,JPS-4)
      MYJE_P4=MIN(JDE-1,JPE+4)
      MYIS_P5=MAX(IDS,IPS-5)
      MYIE_P5=MIN(IDE-1,IPE+5)
      MYJS_P5=MAX(JDS,JPS-5)
      MYJE_P5=MIN(JDE-1,JPE+5)
!
      MYIS1_P1=MAX(IDS+1,IPS-1)
      MYIE1_P1=MIN(IDE-2,IPE+1)
      MYIS1_P2=MAX(IDS+1,IPS-2)
      MYIE1_P2=MIN(IDE-2,IPE+2)
!
      MYJS1_P1=MAX(JDS+1,JPS-1)
      MYJS2_P1=MAX(JDS+2,JPS-1)
      MYJE1_P1=MIN(JDE-2,JPE+1)
      MYJE2_P1=MIN(JDE-3,JPE+1)
      MYJS1_P2=MAX(JDS+1,JPS-2)
      MYJE1_P2=MIN(JDE-2,JPE+2)
      MYJS2_P2=MAX(JDS+2,JPS-2)
      MYJE2_P2=MIN(JDE-3,JPE+2)
      MYJS1_P3=MAX(JDS+1,JPS-3)
      MYJE1_P3=MIN(JDE-2,JPE+3)
      MYJS2_P3=MAX(JDS+2,JPS-3)
      MYJE2_P3=MIN(JDE-3,JPE+3)
!!!!!!!!!!!
!
#ifdef DM_PARALLEL

        call wrf_get_myproc(myproc)

#  include <HALO_NMM_INIT_1.inc>
#  include <HALO_NMM_INIT_2.inc>
#  include <HALO_NMM_INIT_3.inc>
#  include <HALO_NMM_INIT_4.inc>
#  include <HALO_NMM_INIT_5.inc>
#  include <HALO_NMM_INIT_6.inc>
#  include <HALO_NMM_INIT_7.inc>
#  include <HALO_NMM_INIT_8.inc>
#  include <HALO_NMM_INIT_9.inc>
#  include <HALO_NMM_INIT_10.inc>
#  include <HALO_NMM_INIT_11.inc>
#  include <HALO_NMM_INIT_12.inc>

#  include <HALO_NMM_INIT_13.inc>

!        CALL wrf_shutdown
!        stop

#  include <HALO_NMM_INIT_14.inc>
#  include <HALO_NMM_INIT_15.inc>
#  include <HALO_NMM_INIT_16.inc>
#  include <HALO_NMM_INIT_17.inc>
#  include <HALO_NMM_INIT_18.inc>
#  include <HALO_NMM_INIT_19.inc>
#  include <HALO_NMM_INIT_20.inc>
#  include <HALO_NMM_INIT_21.inc>
#  include <HALO_NMM_INIT_22.inc>
#  include <HALO_NMM_INIT_23.inc>
#  include <HALO_NMM_INIT_24.inc>
#  include <HALO_NMM_INIT_25.inc>
#  include <HALO_NMM_INIT_26.inc>
#  include <HALO_NMM_INIT_27.inc>
#  include <HALO_NMM_INIT_28.inc>
#  include <HALO_NMM_INIT_29.inc>
#  include <HALO_NMM_INIT_30.inc>
#  include <HALO_NMM_INIT_31.inc>
#  include <HALO_NMM_INIT_32.inc>
#  include <HALO_NMM_INIT_33.inc>
#  include <HALO_NMM_INIT_34.inc>
#  include <HALO_NMM_INIT_35.inc>
#  include <HALO_NMM_INIT_36.inc>
#  include <HALO_NMM_INIT_37.inc>
#  include <HALO_NMM_INIT_38.inc>
#  include <HALO_NMM_INIT_39.inc>
#endif

      DO J=MYJS_P4,MYJE_P4
        IHEG(J)=MOD(J+1,2)
        IHWG(J)=IHEG(J)-1
        IVEG(J)=MOD(J,2)
        IVWG(J)=IVEG(J)-1
      ENDDO
!
      DO J=MYJS_P4,MYJE_P4
        IVW(J)=IVWG(J)
        IVE(J)=IVEG(J)
        IHE(J)=IHEG(J)
        IHW(J)=IHWG(J)
      ENDDO
!
      CAPA=R_D/CP
      LM=KPE-KPS+1
!
      IFS=IPS
      JFS=JPS
      JFE=MIN(JPE,JDE-1)
      IFE=MIN(IPE,IDE-1)
!
      IF(.NOT.RESTRT)THEN
       call wrf_get_myproc(mype)
        DO J=JFS,JFE
        DO I=IFS,IFE
          LLMH=LMH(I,J)
          KOFF=KPE-1-LLMH
          PDSL(I,J)  =PD(I,J)*RES(I,J)
          PREC(I,J)  =0.
          ACPREC(I,J)=0.
          CUPREC(I,J)=0.
          rg=1./g
          ht=fis(i,j)*rg
!!!       fisx=ht*g
!          fisx=max(fis(i,j),0.)
!          prodx=Z0(I,J)*Z0MAX
!          Z0(I,J)    =SM(I,J)*Z0SEA+(1.-SM(I,J))*                      &
!     &                (Z0(I,J)*Z0MAX+FISx    *FCM+Z0LAND)
!!!  &                (prodx        +FISx    *FCM+Z0LAND)
          QS(I,J)    =0.
          AKMS(I,J)  =0.
          AKHS(I,J)  =0.
          TWBS(I,J)  =0.
          QWBS(I,J)  =0.
          CLDEFI(I,J)=1.
!!!!          HTOP(I,J)  =REAL(LLMH)
!!!!          HBOT(I,J)  =REAL(LLMH)
          HTOP(I,J)  =REAL(KTS)
          HTOPD(I,J) =REAL(KTS)
          HTOPS(I,J) =REAL(KTS)
          HBOT(I,J)  =REAL(KTE)
          HBOTD(I,J) =REAL(KTE)
          HBOTS(I,J) =REAL(KTE)
!***
!***  AT THIS POINT, WE MUST CALCULATE THE INITIAL POTENTIAL TEMPERATURE
!***  OF THE SURFACE AND OF THE SUBGROUND.
!***  EXTRAPOLATE DOWN FOR INITIAL SURFACE POTENTIAL TEMPERATURE.
!***  ALSO DO THE SHELTER PRESSURE.
!***
          PM1=AETA1(KOFF+1)*PDTOP+AETA2(KOFF+1)*PDSL(I,J)+PT
          APEM1=(1.E5/PM1)**CAPA

        IF (NMM_TSK(I,J) .ge. 200.) THEN ! have a specific skin temp, use it
               THS(I,J)=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
               TSFCK=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))
	ELSE ! use lowest layer as a proxy
          THS(I,J)=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
          TSFCK=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))
	ENDIF

	if (I .eq. IFE/2 .and. J .eq. JFE/2) then
	write(6,*) 'I,J,T(I,KOFF+1,J),NMM_TSK(I,J):: ', I,J,T(I,KOFF+1,J),NMM_TSK(I,J)
	write(6,*) 'THS(I,J): ', THS(I,J)
	endif

          PSFCK=PD(I,J)+PDTOP+PT
!
          IF(SM(I,J).LT.0.5) THEN
            QS(I,J)=PQ0/PSFCK*EXP(A2*(TSFCK-A3)/(TSFCK-A4))
          ELSEIF(SM(I,J).GT.0.5) THEN
            THS(I,J)=SST(I,J)*(1.E5/(PD(I,J)+PDTOP+PT))**CAPA
          ENDIF
!
          TERM1=-0.068283/T(I,KOFF+1,J)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)
!
          USTAR(I,J)=0.1
          THZ0(I,J)=THS(I,J)
          QZ0(I,J)=QS(I,J)
          UZ0(I,J)=0.
          VZ0(I,J)=0.
! 
        ENDDO
        ENDDO

!***
!***  INITIALIZE 3D MASKS
!***
        DO J=JFS,JFE
          DO K=KPS,KPE
          DO I=IFS,IFE
            HTM(I,K,J)=1.
            VTM(I,K,J)=1.
          ENDDO
          ENDDO
        ENDDO
!***
!***  INITIALIZE CLOUD FIELDS
!***
        DO J=JFS,JFE
          DO K=KPS,KPE
          DO I=IFS,IFE
            CWM(I,K,J)=0.
          ENDDO
          ENDDO
        ENDDO
!***
!***  INITIALIZE ACCUMULATOR ARRAYS TO ZERO.
!***
        ARDSW=0.0
        ARDLW=0.0
        ASRFC=0.0
        AVRAIN=0.0
        AVCNVC=0.0
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          ACFRCV(I,J)=0.
          NCFRCV(I,J)=0
          ACFRST(I,J)=0.
          NCFRST(I,J)=0
          ACSNOW(I,J)=0.
          ACSNOM(I,J)=0.
          SSROFF(I,J)=0.
          BGROFF(I,J)=0.
          ALWIN(I,J) =0.
          ALWOUT(I,J)=0.
          ALWTOA(I,J)=0.
          ASWIN(I,J) =0.
          ASWOUT(I,J)=0.
          ASWTOA(I,J)=0.
          SFCSHX(I,J)=0.
          SFCLHX(I,J)=0.
          SUBSHX(I,J)=0.
          SNOPCX(I,J)=0.
          SFCUVX(I,J)=0.
          SFCEVP(I,J)=0.
          POTEVP(I,J)=0.
          POTFLX(I,J)=0.
        ENDDO
        ENDDO
!***
!***  INITIALIZE SATURATION SPECIFIC HUMIDITY OVER THE WATER.
!***
        EPS=R_D/R_V
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          IF(SM(I,J).GT.0.5)THEN
            CLOGES =-CM1/SST(I,J)-CM2*ALOG10(SST(I,J))+CM3
            ESE    = 10.**(CLOGES+2.)
            QS(I,J)= SM(I,J)*EPS*ESE/(PD(I,J)+PDTOP+PT-ESE*(1.-EPS))
          ENDIF
        ENDDO
        ENDDO
!***  
!***  INITIALIZE TURBULENT KINETIC ENERGY (TKE) TO A SMALL
!***  VALUE (EPSQ2) ABOVE GROUND.  SET TKE TO ZERO IN THE
!***  THE LOWEST MODEL LAYER.  IN THE LOWEST TWO ATMOSPHERIC
!***  ETA LAYERS SET TKE TO A SMALL VALUE (Q2INI).
!***
        DO J=JFS,JFE
        DO K=KPS,KPE-1
        DO I=IFS,IFE
          Q2(I,K,J)=HTM(I,K+1,J)*HBM2(I,J)*EPSQ2
        ENDDO
        ENDDO
        ENDDO
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          Q2(I,LM,J)    = 0.
          LLMH          = LMH(I,J)
          Q2(I,LLMH-2,J)= HBM2(I,J)*Q2INI
          Q2(I,LLMH-1,J)= HBM2(I,J)*Q2INI
        ENDDO
        ENDDO
!***  
!***  PAD ABOVE GROUND SPECIFIC HUMIDITY IF IT IS TOO SMALL.
!***  INITIALIZE LATENT HEATING ACCUMULATION ARRAYS.
!***
        DO J=JFS,JFE
        DO K=KPS,KPE
        DO I=IFS,IFE
          IF(Q(I,K,J).LT.EPSQ)Q(I,K,J)=EPSQ*HTM(I,K,J)
          TRAIN(I,K,J)=0.
          TCUCN(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
!
!----------------------------------------------------------------------
!***  END OF SCRATCH START INITIALIZATION BLOCK.
!----------------------------------------------------------------------
!
        CALL wrf_message('INIT:  INITIALIZED ARRAYS FOR CLEAN START')
      ENDIF ! <--- (not restart)


!
!----------------------------------------------------------------------
!***  INITIALIZE PHYSICS VARIABLES IF STARTING THIS RUN FROM SCRATCH.
!----------------------------------------------------------------------
!
      IF(NEST)THEN
        DO J=JFS,JFE
        DO I=IFS,IFE
!
          LLMH=LMH(I,J)
          KOFF=KPE-1-LLMH
!
          IF(T(I,KOFF+1,J).EQ.0.)THEN
            T(I,KOFF+1,J)=T(I,KOFF+2,J)
          ENDIF
!
          TERM1=-0.068283/T(I,KOFF+1,J)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)
        ENDDO
        ENDDO
      ENDIF
!
      IF(.NOT.RESTRT)THEN
        DO J=JFS,JFE
        DO I=IFS,IFE
          LLMH=LMH(I,J)
          KOFF=KPE-1-LLMH
          PDSL(I,J)  =PD(I,J)*RES(I,J)
          PREC(I,J)  =0.
          ACPREC(I,J)=0.
          CUPREC(I,J)=0.
!          Z0(I,J)    =SM(I,J)*Z0SEA+(1.-SM(I,J))*                      &
!                      (FIS(I,J)*FCM+Z0LAND+Z0(I,J))
          QS(I,J)    =0.
          AKMS(I,J)  =0.
          AKHS(I,J)  =0.
          TWBS(I,J)  =0.
          QWBS(I,J)  =0.
          CLDEFI(I,J)=1.
!!!!          HTOP(I,J)  =REAL(LLMH)
!!!!          HBOT(I,J)  =REAL(LLMH)
          HTOP(I,J)  =REAL(KTS)
          HBOT(I,J)  =REAL(KTE)
!***
!***  AT THIS POINT, WE MUST CALCULATE THE INITIAL POTENTIAL TEMPERATURE
!***  OF THE SURFACE AND OF THE SUBGROUND.
!***  EXTRAPOLATE DOWN FOR INITIAL SURFACE POTENTIAL TEMPERATURE.
!***  ALSO DO THE SHELTER PRESSURE.
!***
          PM1=AETA1(KOFF+1)*PDTOP+AETA2(KOFF+1)*PDSL(I,J)+PT
          APEM1=(1.E5/PM1)**CAPA
        IF (NMM_TSK(I,J) .ge. 200.) THEN ! have a specific skin temp, use it
               THS(I,J)=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
               TSFCK=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))
	ELSE ! use lowest layer as a proxy
          THS(I,J)=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
          TSFCK=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))
	ENDIF
!!!!!          THS(I,J)=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
!               THS(I,J)=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))*APEM1
!!          TSFCK=T(I,KOFF+1,J)*(1.+P608*Q(I,KOFF+1,J))
!               TSFCK=NMM_TSK(I,J)*(1.+P608*Q(I,KOFF+1,J))

          PSFCK=PD(I,J)+PDTOP+PT
!
          IF(SM(I,J).LT.0.5) THEN
            QS(I,J)=PQ0/PSFCK*EXP(A2*(TSFCK-A3)/(TSFCK-A4))
          ELSEIF(SM(I,J).GT.0.5) THEN
!reinstated below 1020
            THS(I,J)=SST(I,J)*(1.E5/(PD(I,J)+PDTOP+PT))**CAPA
          ENDIF

	IF (THS(I,J) .lt. 200) then
	write(6,*) 'bad THS in start_domain_nmm: ', I,J,THS(I,J)
	endif
!
          TERM1=-0.068283/T(I,KOFF+1,J)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)
!
          USTAR(I,J)=0.1
          THZ0(I,J)=THS(I,J)
          QZ0(I,J)=QS(I,J)
          UZ0(I,J)=0.
          VZ0(I,J)=0.
! 
        ENDDO
        ENDDO
!***
!***  INITIALIZE CLOUD FIELDS
!***
        DO J=JFS,JFE
          DO K=KPS,KPE
          DO I=IFS,IFE
            CWM(I,K,J)=0.
          ENDDO
          ENDDO
        ENDDO
!***
!***  INITIALIZE ACCUMULATOR ARRAYS TO ZERO.
!***
        ARDSW=0.0
        ARDLW=0.0
        ASRFC=0.0
        AVRAIN=0.0
        AVCNVC=0.0
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          ACFRCV(I,J)=0.
          NCFRCV(I,J)=0
          ACFRST(I,J)=0.
          NCFRST(I,J)=0
          ACSNOW(I,J)=0.
          ACSNOM(I,J)=0.
          SSROFF(I,J)=0.
          BGROFF(I,J)=0.
          ALWIN(I,J) =0.
          ALWOUT(I,J)=0.
          ALWTOA(I,J)=0.
          ASWIN(I,J) =0.
          ASWOUT(I,J)=0.
          ASWTOA(I,J)=0.
          SFCSHX(I,J)=0.
          SFCLHX(I,J)=0.
          SUBSHX(I,J)=0.
          SNOPCX(I,J)=0.
          SFCUVX(I,J)=0.
          SFCEVP(I,J)=0.
          POTEVP(I,J)=0.
          POTFLX(I,J)=0.
        ENDDO
        ENDDO
!***
!***  INITIALIZE SATURATION SPECIFIC HUMIDITY OVER THE WATER.
!***
        EPS=R_D/R_V
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          IF(SM(I,J).GT.0.5)THEN
            CLOGES =-CM1/SST(I,J)-CM2*ALOG10(SST(I,J))+CM3
            ESE    = 10.**(CLOGES+2.)
            QS(I,J)= SM(I,J)*EPS*ESE/(PD(I,J)+PDTOP+PT-ESE*(1.-EPS))
          ENDIF
        ENDDO
        ENDDO
!***  
!***  INITIALIZE TURBULENT KINETIC ENERGY (TKE) TO A SMALL
!***  VALUE (EPSQ2) ABOVE GROUND.  SET TKE TO ZERO IN THE
!***  THE LOWEST MODEL LAYER.  IN THE LOWEST TWO ATMOSPHERIC
!***  ETA LAYERS SET TKE TO A SMALL VALUE (Q2INI).
!***
        DO J=JFS,JFE
        DO K=KPS,KPE-1
        DO I=IFS,IFE
!!!tlb    Q2(I,K,J)=HTM(I,K+1,J)*HBM2(I,J)*EPSQ2
          Q2(I,K+1,J)=HTM(I,K,J)*HBM2(I,J)*EPSQ2
        ENDDO
        ENDDO
        ENDDO
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          Q2(I,KPS,J)    = 0.
          LLMH          = LMH(I,J)
          Q2(I,KOFF+2,J)= HBM2(I,J)*Q2INI
          Q2(I,KOFF+3,J)= HBM2(I,J)*Q2INI
        ENDDO
        ENDDO
!***  
!***  PAD ABOVE GROUND SPECIFIC HUMIDITY IF IT IS TOO SMALL.
!***  INITIALIZE LATENT HEATING ACCUMULATION ARRAYS.
!***
        DO J=JFS,JFE
        DO K=KPS,KPE
        DO I=IFS,IFE
          IF(Q(I,K,J).LT.EPSQ)Q(I,K,J)=EPSQ*HTM(I,K,J)
          TRAIN(I,K,J)=0.
          TCUCN(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
!
!----------------------------------------------------------------------
!***  END OF SCRATCH START INITIALIZATION BLOCK.
!----------------------------------------------------------------------
!
        CALL wrf_message('INIT:  INITIALIZED ARRAYS FOR CLEAN START')
      ENDIF
!
!----------------------------------------------------------------------
!***  RESTART INITIALIZING.  CHECK TO SEE IF WE NEED TO ZERO
!***  ACCUMULATION ARRAYS.
!----------------------------------------------------------------------

      TSPH=3600./DT ! needed?
      NPHS0=NPHS

!!!!  How do we pass in this TSTART information in reality???

      TSTART=00.0
      TPREC=06.0
      THEAT=06.0
      TCLOD=06.0
      TRDSW=03.0
      TRDLW=03.0
      TSRFC=03.0

      NSTART = INT(TSTART*TSPH+0.5)

      NTSD = NSTART  ! (this NTSD value not honored by integrate)


!! want non-zero values for NPREC, NHEAT type vars to avoid problems
!! with mod statements below.

      NPREC  = INT(TPREC *TSPH+0.5)
      NHEAT  = INT(THEAT *TSPH+0.5)
      NCLOD  = INT(TCLOD *TSPH+0.5)
      NRDSW  = INT(TRDSW *TSPH+0.5)
      NRDLW  = INT(TRDLW *TSPH+0.5)
      NSRFC  = INT(TSRFC *TSPH+0.5)

      IF(RESTRT)THEN
!
!***
!***    AVERAGE CLOUD AMOUNT ARRAY
!***
        IF(MOD(NTSD,NCLOD).LT.NPHS)THEN
          CALL wrf_message('  ZERO AVG CLD AMT ARRAY')
          DO J=JFS,JFE
          DO I=IFS,IFE
            ACFRCV(I,J)=0.
            NCFRCV(I,J)=0
            ACFRST(I,J)=0.
            NCFRST(I,J)=0
          ENDDO
          ENDDO
        ENDIF
!***  
!***     GRID-SCALE AND CONVECTIVE LATENT HEATING ARRAYS.
!***  
        IF(MOD(NTSD,NHEAT).LT.NCNVC)THEN
          CALL wrf_message('  ZERO ACCUM LATENT HEATING ARRAYS')
!
          AVRAIN=0.
          AVCNVC=0.
          DO J=JFS,JFE
          DO K=KPS,KPE
          DO I=IFS,IFE
            TRAIN(I,K,J)=0.
            TCUCN(I,K,J)=0.
          ENDDO
          ENDDO
          ENDDO
        ENDIF
!***
!***  IF THIS IS NOT A NESTED RUN, INITIALIZE TKE
!***
!       IF(.NOT.NEST)THEN
!         DO K=1,LM
!           DO J=JFS,JFE
!           DO I=IFS,IFE
!             Q2(I,K,J)=AMAX1(Q2(I,K,J)*HBM2(I,J),EPSQ2)
!           ENDDO
!           ENDDO
!         ENDDO
!       ENDIF
!***
!***  CLOUD EFFICIENCY
!***
!       DO J=JFS,JFE
!       DO I=IFS,IFE
!!!       CLDEFI(I,J)=AVGEFI*SM(I,J)+STEFI*(1.-SM(I,J))
!         CLDEFI(I,J)=1.
!       ENDDO
!       ENDDO
!***
!***  TOTAL AND CONVECTIVE PRECIPITATION ARRAYS.
!***  TOTAL SNOW AND SNOW MELT ARRAYS.
!***  STORM SURFACE AND BASE GROUND RUN OFF ARRAYS.
!     
        IF(MOD(NTSD,NPREC).LT.NPHS)THEN
          CALL wrf_message('  ZERO ACCUM PRECIP ARRAYS')
          DO J=JFS,JFE
          DO I=IFS,IFE
            ACPREC(I,J)=0.
            CUPREC(I,J)=0.
            ACSNOW(I,J)=0.
            ACSNOM(I,J)=0.
            SSROFF(I,J)=0.
            BGROFF(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!***  
!***  LONG WAVE RADIATION ARRAYS.
!***  
        IF(MOD(NTSD,NRDLW).LT.NPHS)THEN
          CALL wrf_message('  ZERO ACCUM LW RADTN ARRAYS')
          ARDLW=0.
          DO J=JFS,JFE
          DO I=IFS,IFE
            ALWIN(I,J) =0.
            ALWOUT(I,J)=0.
            ALWTOA(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!***  
!***  SHORT WAVE RADIATION ARRAYS.
!***  
        IF(MOD(NTSD,NRDSW).LT.NPHS)THEN
          CALL wrf_message('  ZERO ACCUM SW RADTN ARRAYS')
          ARDSW=0.
          DO J=JFS,JFE
          DO I=IFS,IFE
            ASWIN(I,J) =0.
            ASWOUT(I,J)=0.
            ASWTOA(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!***  
!***  SURFACE SENSIBLE AND LATENT HEAT FLUX ARRAYS.
!***  
        IF(MOD(NTSD,NSRFC).LT.NPHS)THEN
          CALL wrf_message('  ZERO ACCUM SFC FLUX ARRAYS')
          ASRFC=0.
          DO J=JFS,JFE
          DO I=IFS,IFE
            SFCSHX(I,J)=0.
            SFCLHX(I,J)=0.
            SUBSHX(I,J)=0.
            SNOPCX(I,J)=0.
            SFCUVX(I,J)=0.
            SFCEVP(I,J)=0.
            POTEVP(I,J)=0.
            POTFLX(I,J)=0.
          ENDDO
          ENDDO
        ENDIF
!***
!***  ENDIF FOR RESTART FILE ACCUMULATION ZERO BLOCK.
!***
        CALL wrf_message('INIT:  INITIALIZED ARRAYS FOR RESTART START')
      ENDIF
!
      DO J=JFS,JFE
      DO K=KPS,KPE
      DO I=IFS,IFE
        ZERO_3D(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
!
!***  FLAG FOR INITIALIZING ARRAYS, LOOKUP TABLES, & CONSTANTS USED IN
!***  MICROPHYSICS AND RADIATION
!
!----------------------------------------------------------------------
!
      MICRO_START=.TRUE.
!
!----------------------------------------------------------------------
!***
!***  INITIALIZE ADVECTION TENDENCIES TO ZERO SO THAT
!***  BOUNDARY POINTS WILL ALWAYS BE ZERO
!***
      DO J=JFS,JFE
      DO K=KPS,KPE
      DO I=IFS,IFE
        ADT(I,K,J)=0.
        ADU(I,K,J)=0.
        ADV(I,K,J)=0.
      ENDDO
      ENDDO
      ENDDO
!----------------------------------------------------------------------
!***
!***  SET INDEX ARRAYS FOR UPSTREAM ADVECTION
!***
!----------------------------------------------------------------------
      DO J=JFS,JFE
        N_IUP_H(J)=0
        N_IUP_V(J)=0
        N_IUP_ADH(J)=0
        N_IUP_ADV(J)=0
!
        DO I=IFS,IFE
          IUP_H(I,J)=-999
          IUP_V(I,J)=-999
          IUP_ADH(I,J)=-999
          IUP_ADV(I,J)=-999
        ENDDO
!
      ENDDO

#ifndef NO_UPSTREAM_ADVECTION
!
!***  N_IUP_H HOLDS THE NUMBER OF MASS POINTS NEEDED IN EACH ROW
!***  FOR UPSTREAM ADVECTION (FULL ROWS IN THE 3RD THROUGH 7TH
!***  ROWS FROM THE SOUTH AND NORTH GLOBAL BOUNDARIES AND 
!***  FOUR POINTS ADJACENT TO THE WEST AND EAST GLOBAL BOUNDARIES
!***  ON ALL OTHER INTERNAL ROWS).  SIMILARLY FOR N_IUP_V.
!***  BECAUSE OF HORIZONTAL OPERATIONS, THESE POINTS EXTEND OUTSIDE 
!***  OF THE UPSTREAM REGION SOMEWHAT.
!***  N_IUP_ADH HOLDS THE NUMBER OF MASS POINTS NEEDED IN EACH ROW
!***  FOR THE COMPUTATION OF THE TENDENCIES THEMSELVES (ADT, ADQ2M
!***  AND ADQ2L); SPECIFICALLY THESE TENDENCIES ARE ONLY DONE IN
!***  THE UPSTREAM REGION.
!***  N_IUP_ADV HOLDS THE NUMBER OF MASS POINTS NEEDED IN EACH ROW
!***  FOR THE VELOCITY POINT TENDENCIES.
!***  IUP_H AND IUP_V HOLD THE ACTUAL I VALUES USED IN EACH ROW.
!***  LIKEWISE FOR IUP_ADH AND IUP_ADV. 
!***  ALSO, SET UPSTRM FOR THOSE TASKS AROUND THE GLOBAL EDGE.
!
      UPSTRM=.FALSE.
!
      S_BDY=(JPS==JDS)
      N_BDY=(JPE==JDE)
      W_BDY=(IPS==IDS)
      E_BDY=(IPE==IDE)
!
      JTPAD2=2
      JBPAD2=2
      IRPAD2=2
      ILPAD2=2
!
      IF(S_BDY)THEN
        UPSTRM=.TRUE.
        JBPAD2=0
!
        DO JJ=1,7
          J=JJ      ! -MY_JS_GLB+1
          KNTI=0
          DO I=MYIS_P2,MYIE_P2
            IUP_H(IMS+KNTI,J)=I
            IUP_V(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_H(J)=KNTI
          N_IUP_V(J)=KNTI
        ENDDO
!
        DO JJ=3,5
          J=JJ      ! -MY_JS_GLB+1
          KNTI=0
          ISTART=MYIS1_P2
          IEND=MYIE1_P2
          IF(E_BDY)IEND=IEND-MOD(JJ+1,2)
          DO I=ISTART,IEND
            IUP_ADH(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_ADH(J)=KNTI
!
          KNTI=0
          ISTART=MYIS1_P2
          IEND=MYIE1_P2
          IF(E_BDY)IEND=IEND-MOD(JJ,2)
          DO I=ISTART,IEND
            IUP_ADV(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_ADV(J)=KNTI
        ENDDO
      ENDIF
!
      IF(N_BDY)THEN
        UPSTRM=.TRUE.
        JTPAD2=0
!
        DO JJ=JDE-7, JDE-1 ! JM-6,JM
          J=JJ      ! -MY_JS_GLB+1
          KNTI=0
          DO I=MYIS_P2,MYIE_P2
            IUP_H(IMS+KNTI,J)=I
            IUP_V(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_H(J)=KNTI
          N_IUP_V(J)=KNTI
        ENDDO
!
        DO JJ=JDE-5, JDE-3 ! JM-4,JM-2
          J=JJ      ! -MY_JS_GLB+1
          KNTI=0
          ISTART=MYIS1_P2
          IEND=MYIE1_P2
          IF(E_BDY)IEND=IEND-MOD(JJ+1,2)
          DO I=ISTART,IEND
            IUP_ADH(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_ADH(J)=KNTI
!
          KNTI=0
          ISTART=MYIS1_P2
          IEND=MYIE1_P2
          IF(E_BDY)IEND=IEND-MOD(JJ,2)
          DO I=ISTART,IEND
            IUP_ADV(IMS+KNTI,J)=I
            KNTI=KNTI+1
          ENDDO
          N_IUP_ADV(J)=KNTI
        ENDDO
      ENDIF
!
      IF(W_BDY)THEN
        UPSTRM=.TRUE.
        ILPAD2=0
        DO JJ=8,JDE-8   ! JM-7
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
!
            DO I=1,4
              IUP_H(IMS+I-1,J)=I
              IUP_V(IMS+I-1,J)=I
            ENDDO
            N_IUP_H(J)=4
            N_IUP_V(J)=4
          ENDIF
        ENDDO
!
        DO JJ=6,JDE-6   ! JM-5
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            KNTI=0
            IEND=2+MOD(JJ,2)
            DO I=2,IEND
              IUP_ADH(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADH(J)=KNTI
!
            KNTI=0
            IEND=2+MOD(JJ+1,2)
            DO I=2,IEND
              IUP_ADV(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADV(J)=KNTI
!
          ENDIF
        ENDDO
      ENDIF
!
      CALL WRF_GET_NPROCX(INPES)
!
      IF(E_BDY)THEN
        UPSTRM=.TRUE.
        IRPAD2=0
        DO JJ=8,JDE-8   ! JM-7
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            IEND=IM-MOD(JJ+1,2)
            ISTART=IEND-3
!
!***  IN CASE THERE IS ONLY A SINGLE GLOBAL TASK IN THE
!***  I DIRECTION THEN WE MUST ADD THE WESTSIDE UPSTREAM
!***  POINTS TO THE EASTSIDE POINTS IN EACH ROW.
!
            KNTI=0
            IF(INPES.EQ.1)KNTI=N_IUP_H(J)
!
            DO II=ISTART,IEND
              I=II      ! -MY_IS_GLB+1
              IUP_H(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_H(J)=KNTI
          ENDIF
        ENDDO
!
        DO JJ=6,JDE-6   ! JM-5
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            IEND=IM-1-MOD(JJ+1,2)
            ISTART=IEND-MOD(JJ,2)
            KNTI=0
            IF(INPES.EQ.1)KNTI=N_IUP_ADH(J)
            DO II=ISTART,IEND
              I=II      ! -MY_IS_GLB+1
              IUP_ADH(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADH(J)=KNTI
          ENDIF
        ENDDO
!***
        DO JJ=8,JDE-8  ! JM-7
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            IEND=IM-MOD(JJ,2)
            ISTART=IEND-3
            KNTI=0
            IF(INPES.EQ.1)KNTI=N_IUP_V(J)
!
            DO II=ISTART,IEND
              I=II      ! -MY_IS_GLB+1
              IUP_V(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_V(J)=KNTI
          ENDIF
        ENDDO
!
        DO JJ=6,JDE-6  !  JM-5
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            IEND=IM-1-MOD(JJ,2)
            ISTART=IEND-MOD(JJ+1,2)
            KNTI=0
            IF(INPES.EQ.1)KNTI=N_IUP_ADV(J)
            DO II=ISTART,IEND
              I=II      ! -MY_IS_GLB+1
              IUP_ADV(IMS+KNTI,J)=I
              KNTI=KNTI+1
            ENDDO
            N_IUP_ADV(J)=KNTI
          ENDIF
        ENDDO
      ENDIF
!----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!tlb
!!!Read in EM and EMT from the original NMM nhb file
!!!   call int_get_fresh_handle( retval )
!!!   close(retval)
!!!   open(unit=retval,file=seeout,form='UNFORMATTED',iostat=ier)
!!!!!!do j=1,128
!!!     read(seeout)
!!!!!!  read(55)
!!!!!!enddo
!!!   read(seeout)dummyx,em,emt
!!!!!!read(55)dummyx,em,emt
!!!   close(retval)
      jam=6+2*(JDE-JDS-1-9)
!     read(55)(em(j),j=1,jam),(emt(j),j=1,jam)
!!!!!!!!!!!!!!!!!!!!tlb
!
!***  EXTRACT EM AND EMT FOR THE LOCAL SUBDOMAINS
!
      DO J=MYJS_P5,MYJE_P5
        EM_LOC(J)=-9.E9
        EMT_LOC(J)=-9.E9
      ENDDO
!!!   IF(IBROW==1)THEN
      IF(S_BDY)THEN
        DO J=3,5
          EM_LOC(J)=EM(J-2)
          EMT_LOC(J)=EMT(J-2)
        ENDDO
      ENDIF
!!!   IF(ITROW==1)THEN
      IF(N_BDY)THEN
        KNT=3
        DO JJ=JDE-5,JDE-3 ! JM-4,JM-2
          KNT=KNT+1
          J=JJ      ! -MY_JS_GLB+1
          EM_LOC(J)=EM(KNT)
          EMT_LOC(J)=EMT(KNT)
        ENDDO
      ENDIF
!!!   IF(ILCOL==1)THEN
      IF(W_BDY)THEN
        KNT=6
        DO JJ=6,JDE-6 ! JM-5
          KNT=KNT+1
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            EM_LOC(J)=EM(KNT)
            EMT_LOC(J)=EMT(KNT)
          ENDIF
        ENDDO
      ENDIF
!!!   IF(IRCOL==1)THEN
      IF(E_BDY)THEN
        KNT=6+JDE-11 ! JM-10
        DO JJ=6,JDE-6 ! JM-5
          KNT=KNT+1
          IF(JJ.GE.MY_JS_GLB-2.AND.JJ.LE.MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            EM_LOC(J)=EM(KNT)
            EMT_LOC(J)=EMT(KNT)
          ENDIF
        ENDDO
      ENDIF
#else
      CALL wrf_message( 'start_domain_nmm: upstream advection commented out')
#endif
!
!***
!*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS
!***
      IF(NSTART.EQ.0)THEN
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          PCTSNO(I,J)=-999.0
          IF(SM(I,J).LT.0.5)THEN
            IF(SICE(I,J).GT.0.5)THEN
!***
!***  SEA-ICE CASE
!***
              SMSTAV(I,J)=1.0
              SMSTOT(I,J)=1.0
              SSROFF(I,J)=0.0
              BGROFF(I,J)=0.0
              CMC(I,J)=0.0
              DO NS=1,NSOIL
                SMC(I,NS,J)=1.0
!               SH2O(I,NS,J)=0.05
                SH2O(I,NS,J)=1.0
              ENDDO
            ENDIF
          ELSE
!***
!***  WATER CASE
!***
            SMSTAV(I,J)=1.0
            SMSTOT(I,J)=1.0
            SSROFF(I,J)=0.0
            BGROFF(I,J)=0.0
            SOILTB(I,J)=273.16
            GRNFLX(I,J)=0.
            SUBSHX(I,J)=0.0
            ACSNOW(I,J)=0.0
            ACSNOM(I,J)=0.0
            SNOPCX(I,J)=0.0
            CMC(I,J)=0.0
            SNO(I,J)=0.0
            DO NS=1,NSOIL
              SMC(I,NS,J)=1.0
              STC(I,NS,J)=273.16
!             SH2O(I,NS,J)=0.05
              SH2O(I,NS,J)=1.0
            ENDDO
          ENDIF
!
        ENDDO
        ENDDO
!
        APHTIM=0.0
        ARATIM=0.0
        ACUTIM=0.0
!
      ENDIF
!
!----------------------------------------------------------------------
!***  INITIALIZE RADTN VARIABLES
!***  CALCULATE THE NUMBER OF STEPS AT EACH POINT.
!***  THE ARRAY 'LVL' WILL COORDINATE VERTICAL LOCATIONS BETWEEN
!***  THE LIFTED WORKING ARRAYS AND THE FUNDAMENTAL MODEL ARRAYS.
!***  LVL HOLDS THE HEIGHT (IN MODEL LAYERS) OF THE TOPOGRAPHY AT
!***  EACH GRID POINT.
!----------------------------------------------------------------------
!   
      DO J=JFS,JFE
      DO I=IFS,IFE
        LVL(I,J)=LM-LMH(I,J)
      ENDDO
      ENDDO
!***
!***  DETERMINE MODEL LAYER LIMITS FOR HIGH(3), MIDDLE(2),
!***  AND LOW(1) CLOUDS.  ALSO FIND MODEL LAYER THAT IS JUST BELOW
!***  (HEIGHT-WISE) 400 MB. (K400)
!*** 
      K400=0
      PSUM=PT
      SLPM=101325.
      PDIF=SLPM-PT
      DO K=1,LM
        PSUM=PSUM+DETA(K)*PDIF
        IF(LPTOP(3).EQ.0)THEN
          IF(PSUM.GT.PHITP)LPTOP(3)=K
        ELSEIF(LPTOP(2).EQ.0)THEN
          IF(PSUM.GT.PMDHI)LPTOP(2)=K
        ELSEIF(K400.EQ.0)THEN
          IF(PSUM.GT.P400)K400=K
        ELSEIF(LPTOP(1).EQ.0)THEN
          IF(PSUM.GT.PLOMD)LPTOP(1)=K
        ENDIF
      ENDDO
!***
!*** CALL GRADFS ONCE TO CALC. CONSTANTS AND GET O3 DATA
!***
      KCCO2=0
!***
!*** CALCULATE THE MIDLAYER PRESSURES IN THE STANDARD ATMOSPHERE
!***
      PSS=101325.
      PDIF=PSS-PT
!
      ALLOCATE(PHALF(LM+1),STAT=I)
!
      DO K=KPS,KPE-1
        PHALF(K+1)=AETA(K)*PDIF+PT
      ENDDO
      
!
      PHALF(1)=0.
      PHALF(LM+1)=PSS
!***
!!!   CALL GRADFS(PHALF,KCCO2,NUNIT_CO2)
!***
!***  CALL SOLARD TO CALCULATE NON-DIMENSIONAL SUN-EARTH DISTANCE
!***
!!!   IF(MYPE.EQ.0)CALL SOLARD(SUN_DIST)
!!!   CALL MPI_BCAST(SUN_DIST,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)

!***
!***  CALL ZENITH SIMPLY TO GET THE DAY OF THE YEAR FOR
!***  THE SETUP OF THE OZONE DATA
!***
      TIME=(NTSD-1)*DT
!
!!!   CALL ZENITH(TIME,DAYI,HOUR)
!
      ADDL=0.
      IF(MOD(IDAT(3),4).EQ.0)ADDL=1.
!
!!!   CALL O3CLIM
!
!
      DEALLOCATE(PHALF)
!----------------------------------------------------------------------
!***  SOME INITIAL VALUES RELATED TO TURBULENCE SCHEME
!----------------------------------------------------------------------
!
      DO J=JFS,JFE
      DO I=IFS,IFE
!***
!***  TRY A SIMPLE LINEAR INTERP TO GET 2/10 M VALUES
!***
        PDSL(I,J)=PD(I,J)*RES(I,J)
        LMHK=LMH(I,J)
        LMVK=LMV(I,J)
!
        KOFF=KPE-1-LMHK
        KOFV=KPE-1-LMVK
!
        ULM=U(I,KOFV+1,J)
        VLM=V(I,KOFV+1,J)
        TLM=T(I,KOFF+1,J)
        QLM=Q(I,KOFF+1,J)
        PLM=AETA1(KOFF+1)*PDTOP+AETA2(KOFF+1)*PDSL(I,J)+PT
        APELM=(1.0E5/PLM)**CAPA
        APELMNW=(1.0E5/PSHLTR(I,J))**CAPA
        THLM=TLM*APELM
        DPLM=(DETA1(KOFF+1)*PDTOP+DETA2(KOFF+1)*PDSL(I,J))*0.5
        DZLM=R_D*DPLM*TLM*(1.+P608*QLM)/(G*PLM)
        FAC1=10./DZLM
        FAC2=(DZLM-10.)/DZLM
        IF(DZLM.LE.10.)THEN
          FAC1=1.
          FAC2=0.
        ENDIF
!
        IF(.NOT.RESTRT)THEN
          TH10(I,J)=FAC2*THS(I,J)+FAC1*THLM
          Q10(I,J)=FAC2*QS(I,J)+FAC1*QLM
          U10(I,J)=ULM
          V10(I,J)=VLM
        ENDIF
!
        FAC1=2./DZLM
        FAC2=(DZLM-2.)/DZLM
        IF(DZLM.LE.2.)THEN
          FAC1=1.
          FAC2=0.
        ENDIF
!
        IF(.NOT.RESTRT.OR.NEST)THEN
!mp          TSHLTR(I,J)=FAC2*THS(I,J)+FAC1*THLM
          TSHLTR(I,J)=0.2*THS(I,J)+0.8*THLM
!mp          QSHLTR(I,J)=FAC2*QS(I,J)+FAC1*QLM
          QSHLTR(I,J)=0.2*QS(I,J)+0.8*QLM
        ENDIF
!***
!***  NEED TO CONVERT TO THETA IF IS THE RESTART CASE
!***  AS CHKOUT.f WILL CONVERT TO TEMPERATURE
!***
        IF(RESTRT)THEN
          TSHLTR(I,J)=TSHLTR(I,J)*APELMNW
        ENDIF
      ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!***  INITIALIZE TAU-1 VALUES FOR ADAMS-BASHFORTH 
!----------------------------------------------------------------------
!
      DO J=jfs,jfe
        DO K=KPS,KPE
        DO I=ifs,ife
          TOLD(I,K,J)=T(I,K,J)   ! T AT TAU-1
          UOLD(I,K,J)=U(I,K,J)   ! U AT TAU-1
          VOLD(I,K,J)=V(I,K,J)   ! V AT TAU-1
        ENDDO
        ENDDO
      ENDDO
!
!----------------------------------------------------------------------
!***  INITIALIZE NONHYDROSTATIC QUANTITIES
!----------------------------------------------------------------------
!
!!!!	SHOULD DWDT BE REDEFINED IF RESTRT?

        IF(.NOT.RESTRT.OR.NEST)THEN
      DO J=jfs,jfe
        DO K=KPS,KPE
        DO I=ifs,ife
          DWDT(I,K,J)=1.
        ENDDO
        ENDDO
      ENDDO
	ENDIF
!***
      IF(SIGMA.EQ.1)THEN
        DO J=jfs,jfe
        DO I=ifs,ife
          PDSL(I,J)=PD(I,J)
        ENDDO
        ENDDO
      ELSE
        DO J=jfs,jfe
        DO I=ifs,ife
          PDSL(I,J)=RES(I,J)*PD(I,J)
        ENDDO
        ENDDO
      ENDIF
!
!***
!
!
!!!!	SHOULD PINT,Z,W BE REDEFINED IF RESTRT?

      write(0,*)' restrt=',restrt,' nest=',nest
      write(0,*)' ifs=',ifs,' ife=',ife
      write(0,*)' jfs=',jfs,' jfe=',jfe
      write(0,*)' kps=',kps,' kpe=',kpe
      write(0,*)' pdtop=',pdtop,' pt=',pt
        IF(.NOT.RESTRT.OR.NEST)THEN
      DO J=jfs,jfe
        DO K=KPS,KPE
        DO I=ifs,ife
          PINT(I,K,J)=ETA1(K)*PDTOP+ETA2(K)*PDSL(I,J)+PT
          Z(I,K,J)=PINT(I,K,J)
          W(I,K,J)=0.
        ENDDO
        ENDDO
      ENDDO
	ENDIF

#ifndef NO_RESTRICT_ACCEL
!----------------------------------------------------------------------
!***  RESTRICTING THE ACCELERATION ALONG THE BOUNDARIES
!----------------------------------------------------------------------
!
      DO J=jfs,jfe
      DO I=ifs,ife
        DWDTMN(I,J)=-EPSIN
        DWDTMX(I,J)= EPSIN
      ENDDO
      ENDDO


!
!***
      IF(JHL.GT.1)THEN
        JHH=JDE-1-JHL+1 ! JM-JHL+1
        IHL=JHL/2+1
!
        DO J=1,JHL
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
            DO I=1,IDE-1 ! IM
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I      ! -MY_IS_GLB+1
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
        DO J=JHH,JDE-1   ! JM
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
            DO I=1,IDE-1 ! IM
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I      ! -MY_IS_GLB+1
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
        DO J=1,JDE-1 ! JM
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
            DO I=1,IHL
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I      ! -MY_IS_GLB+1
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
        DO J=1,JDE-1 ! JM
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
             ! moved this line to inside the J-loop, 20030429, jm
            IHH=IDE-1-IHL+MOD(j,2) ! IM-IHL+MOD(J,2)
            DO I=IHH,IDE-1 ! IM
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I      ! -MY_IS_GLB+1
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
      ENDIF

#else
      CALL wrf_message('start_domain_nmm: NO_RESTRICT_ACCEL')
#endif

!-----------------------------------------------------------------------
!***  CALL THE GENERAL PHYSICS INITIALIZATION
!-----------------------------------------------------------------------
!

      ALLOCATE(SFULL(KMS:KME),STAT=I)           ; SFULL    = 0.
      ALLOCATE(SMID(KMS:KME),STAT=I)            ; SMID     = 0.
      ALLOCATE(EMISS(IMS:IME,JMS:JME),STAT=I)   ; EMISS    = 0.
      ALLOCATE(GLW(IMS:IME,JMS:JME),STAT=I)     ; GLW      = 0.
      ALLOCATE(GSW(IMS:IME,JMS:JME),STAT=I)     ; GSW      = 0.
      ALLOCATE(HFX(IMS:IME,JMS:JME),STAT=I)     ; HFX      = 0.
      ALLOCATE(LOWLYR(IMS:IME,JMS:JME),STAT=I)  ; LOWLYR   = 0.
      ALLOCATE(MAVAIL(IMS:IME,JMS:JME),STAT=I)  ; MAVAIL   = 0.
      ALLOCATE(NCA(IMS:IME,JMS:JME),STAT=I)     ; NCA      = 0.
      ALLOCATE(QFX(IMS:IME,JMS:JME),STAT=I)     ; QFX      = 0.
      ALLOCATE(RAINBL(IMS:IME,JMS:JME),STAT=I)  ; RAINBL   = 0.
      ALLOCATE(RAINC(IMS:IME,JMS:JME),STAT=I)   ; RAINC    = 0.
      ALLOCATE(RAINCV(IMS:IME,JMS:JME),STAT=I)  ; RAINCV   = 0.
      ALLOCATE(RAINNC(IMS:IME,JMS:JME),STAT=I)  ; RAINNC   = 0.
      ALLOCATE(RAINNCV(IMS:IME,JMS:JME),STAT=I) ; RAINNCV  = 0.

      ALLOCATE(ZS(KMS:KME),STAT=I)              ; ZS       = 0.
      ALLOCATE(SNOWC(IMS:IME,JMS:JME),STAT=I)   ; SNOWC    = 0.
      ALLOCATE(THC(IMS:IME,JMS:JME),STAT=I)     ; THC      = 0.
      ALLOCATE(TMN(IMS:IME,JMS:JME),STAT=I)     ; TMN      = 0.
      ALLOCATE(TSFC(IMS:IME,JMS:JME),STAT=I)    ; TSFC     = 0.
      ALLOCATE(XLAND(IMS:IME,JMS:JME),STAT=I)   ; XLAND    = 0.
      ALLOCATE(XLAT(IMS:IME,JMS:JME),STAT=I)    ; XLAT     = 0.
      ALLOCATE(XLONG(IMS:IME,JMS:JME),STAT=I)   ; XLONG    = 0.
      ALLOCATE(Z0_DUM(IMS:IME,JMS:JME),STAT=I)  ; Z0_DUM   = 0.

      ALLOCATE(DZS(KMS:KME),STAT=I)                         ; DZS = 0.
      ALLOCATE(CLDFRA(IMS:IME,KMS:KME, JMS:JME),STAT=I)     ; CLDFRA = 0.
      ALLOCATE(RQCBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQCBLTEN = 0.
      ALLOCATE(RQIBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQIBLTEN = 0.
      ALLOCATE(RQVBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQVBLTEN =  0.
      ALLOCATE(RTHBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RTHBLTEN =  0.
      ALLOCATE(RUBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)     ; RUBLTEN = 0.
      ALLOCATE(RVBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)     ; RVBLTEN = 0.
      ALLOCATE(RQCCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQCCUTEN = 0.
      ALLOCATE(RQICUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQICUTEN  = 0.
      ALLOCATE(RQRCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQRCUTEN = 0.
      ALLOCATE(RQSCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQSCUTEN = 0.
      ALLOCATE(RQVCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RQVCUTEN = 0.
      ALLOCATE(RTHCUTEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RTHCUTEN = 0.
      ALLOCATE(RTHRATEN(IMS:IME,KMS:KME,JMS:JME),STAT=I)    ; RTHRATEN  = 0.
      ALLOCATE(RTHRATENLW(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; RTHRATENLW = 0.
      ALLOCATE(RTHRATENSW(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; RTHRATENSW = 0.
      ALLOCATE(TSLB(IMS:IME,KMS:KME,JMS:JME),STAT=I)        ; TSLB = 0.
#if 0
      ALLOCATE(W0AVG(IMS:IME,KMS:KME,JMS:JME),STAT=I)       ; W0AVG = 0.
#endif
!-----------------------------------------------------------------------
!jm added set of g_inv
      write(0,*)' start nrads=',nrads,' nradl=',nradl,' nphs=',nphs,' ncnvc=',ncnvc
      G_INV=1./G
      ROG=R_D*G_INV
      RADT=NRADS*DT/60.
      BLDT=NPHS*DT/60.
      CUDT=NCNVC*DT/60.
      GSMDT=NPHS*DT/60.
!
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        SFCZ=FIS(I,J)*G_INV
!!!!    ZINT(I,KTS,J)=SFCZ
        PDSL(I,J)=PD(I,J)*RES(I,J)
        PSURF=PINT(I,KTS,J)
        EXNSFC=(1.E5/PSURF)**CAPA
        XLAND(I,J)=SM(I,J)+1.
        THSIJ=(SST(I,J)*EXNSFC)*(XLAND(I,J)-1.)                         &
     &        +THS(I,J)*(2.-SM(I,J))
        TSFC(I,J)=THSIJ/EXNSFC
!
        DO K=KTS,KTE-1
          PLYR=(PINT(I,K,J)+PINT(I,K+1,J))*0.5
          TL=T(I,K,J)
          CWML=CWM(I,K,J)
!!!       ZINT(I,K+1,J)=ZINT(I,K,J)+TL/PLYR                             &
!!!  &                  *(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))*ROG        &
!!!  &                  *(Q(I,K,J)*P608-CWML+1.)
        ENDDO
!
        DO K=KTS,KTE
!!!       ZMID(I,K,J)=0.5*(ZINT(I,K,J)+ZINT(I,K+1,J))
        ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  RECREATE SIGMA VALUES AT LAYER INTERFACES FOR THE FULL VERTICAL
!***  DOMAIN FROM THICKNESS VALUES FOR THE TWO SUBDOMAINS.
!***  NOTE: KTE=NUMBER OF LAYERS PLUS ONE
!-----------------------------------------------------------------------
!
      write(0,*)' start_domain kte=',kte
      PDTOT=101325.-PT
      RPDTOT=1./PDTOT
      PDBOT=PDTOT-PDTOP
      SFULL(KTS)=1.
      SFULL(KTE)=0.
      dsigsum = 0.
      DO K=KTS+1,KTE
        DSIG=(DETA1(K-1)*PDTOP+DETA2(K-1)*PDBOT)*RPDTOT
        dsigsum=dsigsum+dsig
        SFULL(K)=SFULL(K-1)-DSIG
        SMID(K-1)=0.5*(SFULL(K-1)+SFULL(K))
      ENDDO
      dsig=(deta1(kte-1)*pdtop+deta2(kte-1)*pdbot)*rpdtot
      dsigsum=dsigsum+dsig
      SMID(KTE-1)=0.5*(SFULL(KTE-1)+SFULL(KTE))
!
!-----------------------------------------------------------------------

       LU_INDEX=IVGTYP

      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
	Z0_DUM(I,J)=Z0(I,J) ! hold topography component
      ENDDO
      ENDDO

      CALL PHY_INIT(GRID,GRID%ID,CONFIG_FLAGS,DT,sfull,smid             &
     &             ,PT,TSFC,RADT,BLDT,CUDT,GSMDT                        &
     &             ,RTHCUTEN, RQVCUTEN, RQRCUTEN                        &
     &             ,RQCCUTEN, RQSCUTEN, RQICUTEN                        &
     &             ,RUBLTEN,RVBLTEN,RTHBLTEN                            &
     &             ,RQVBLTEN,RQCBLTEN,RQIBLTEN                          &
     &             ,RTHRATEN,RTHRATENLW,RTHRATENSW                      &
     &             ,STEPBL,STEPRA,STEPCU                                &
     &             ,W0AVG, RAINNC, RAINC, RAINCV, RAINNCV               &
     &             ,NCA                                                 &
     &             ,CLDEFI,LOWLYR                                       &
     &             ,MASS_FLUX                                           &
     &             ,RTHFTEN, RQVFTEN                                    &
     &             ,CLDFRA,GLW,GSW,EMISS,LU_INDEX                       &
     &             ,XLAT,XLONG,ALBEDO,ALBBCK,GMT,JULYR,JULDAY           &
     &             ,TMN,XLAND,ZNT,Z0,USTAR,MOL,PBLH,TKE_MYJ             &
     &             ,EXCH_H,THC,SNOWC,MAVAIL,HFX,QFX,RAINBL              &
     &             ,TSLB,ZS,DZS,num_soil_layers,warm_rain               &
     &             ,APR_GR,APR_W,APR_MC,APR_ST,APR_AS                   &
     &             ,APR_CAPMA,APR_CAPME,APR_CAPMI                       &
     &             ,XICE,VEGFRA,SNOW,CANWAT,SMSTAV                      &
     &             ,SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW            &
     &             ,ACSNOM,IVGTYP,ISLTYP,SFCEVP,SMOIS                   &
     &             ,SH2O, SNOWH, SMFR3D                                 &  ! temporary
     &             ,DX,DY,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY              &
     &             ,MP_RESTART_STATE,TBPVS_STATE,TBPVS0_STATE           &
     &             ,.TRUE.,.FALSE.                                      &
     &             ,ids, ide, jds, jde, kds, kde                        &
     &             ,ims, ime, jms, jme, kms, kme                        &
     &             ,its, ite, jts, jte, kts, kte)
!-----------------------------------------------------------------------
!
      DO J=JMS,JME
      DO I=IMS,IME
	Z0(I,J)=Z0_DUM(I,J)+0.5*Z0(I,J) ! add 1/2 of veg Z0 component, 
                                        ! expecting this code to be called
                                        ! both by real and by the model.
        APREC(I,J)=RAINNC(I,J)*1.E-3
        CUPREC(I,J)=RAINCV(I,J)*1.E-3
      ENDDO
      ENDDO
!
      DEALLOCATE(SFULL)
      DEALLOCATE(SMID)
      DEALLOCATE(CLDFRA)
      DEALLOCATE(DZS)
      DEALLOCATE(EMISS)
      DEALLOCATE(GLW)
      DEALLOCATE(GSW)
      DEALLOCATE(HFX)
      DEALLOCATE(LOWLYR)
      DEALLOCATE(MAVAIL)
      DEALLOCATE(NCA)
      DEALLOCATE(QFX)
      DEALLOCATE(RAINBL)
      DEALLOCATE(RAINC)
      DEALLOCATE(RAINCV)
      DEALLOCATE(RAINNC)
      DEALLOCATE(RAINNCV)
      DEALLOCATE(RQCBLTEN)
      DEALLOCATE(RQIBLTEN)
      DEALLOCATE(RQVBLTEN)
      DEALLOCATE(RTHBLTEN)
      DEALLOCATE(RUBLTEN)
      DEALLOCATE(RVBLTEN)
      DEALLOCATE(RQCCUTEN)
      DEALLOCATE(RQICUTEN)
      DEALLOCATE(RQRCUTEN)
      DEALLOCATE(RQSCUTEN)
      DEALLOCATE(RQVCUTEN)
      DEALLOCATE(RTHCUTEN)
      DEALLOCATE(RTHRATEN)
      DEALLOCATE(RTHRATENLW)
      DEALLOCATE(RTHRATENSW)
      DEALLOCATE(SNOWC)
      DEALLOCATE(THC)
      DEALLOCATE(TMN)
      DEALLOCATE(TSFC)
      DEALLOCATE(TSLB)
      DEALLOCATE(XLAND)
      DEALLOCATE(XLAT)
      DEALLOCATE(XLONG)
      DEALLOCATE(ZS)
#if 0
      DEALLOCATE(W0AVG)
#endif
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
        DO J=jfs,jfe
        DO I=ifs,ife
          DWDTMN(I,J)=DWDTMN(I,J)*HBM3(I,J)
          DWDTMX(I,J)=DWDTMX(I,J)*HBM3(I,J)
        ENDDO
        ENDDO
!----------------------------------------------------------------------
!***  INITIALIZE 3RD INDEX IN WORKING ARRAYS USED IN PFDHT, DDAMP, AND  
!***  HZADV.  THESE ARRAYS MUST HAVE AN EXTENT OF MORE THAN 1 IN J DUE 
!***  TO THE MANY DIFFERENCES AND AVERAGES THAT ARE COMPUTED IN J
!***  OR BECAUSE THE ARRAY IS SIMPLY REFERENCED AT MORE THAN ONE J.
!***  THE WORKING "SPACE" SPANS FROM 3 ROWS SOUTH TO 3 ROWS NORTH
!***  OF THE ROW FOR WHICH THE PRIMARY COMPUTATION IS BEING DONE
!***  THUS THE 3RD DIMENSION CAN VARY FROM -3 TO +3 ALTHOUGH ALL OF
!***  THESE ARRAYS DO NOT NEED TO SPAN THAT MANY ROWS.  FOR INSTANCE,
!***  SOME OF THE ARRAYS ARE ONLY USED FROM 2 ROWS SOUTH TO 1 ROW 
!***  NORTH, OR FROM 1 ROW SOUTH TO THE CENTRAL ROW.  AS THE INTEGRATION
!***  MOVES NORTHWARD, THE SOUTHERNMOST I,K SLAB IS DROPPED FOR EACH
!***  WORKING ARRAY AND THE NORTHERNMOST IS GENERATED.  SO AS NOT TO
!***  HAVE TO ACTUALLY MOVE ANY OF THE I,K SLABS NORTHWARD, THE 3RD
!***  INDEX IS CYCLED THROUGH THE EXTENT OF EACH ARRAY'S J DIMENSION.
!***  THE FOLLOWING WILL FILL AN ARRAY WITH THE VALUES OF THE 3RD
!***  INDEX FOR EACH THESE VARIATIONS OF J EXTENTS FOR ALL J's IN
!***  THE LOCAL DOMAIN.
!----------------------------------------------------------------------
!
!***  CASE 0: J EXTENT IS -3 TO 3
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP3=KNT+2-7*((KNT+5)/7)
        JP2=JP3-1+7*((4-JP3)/7)
        JP1=JP2-1+7*((4-JP2)/7)
        J00=JP1-1+7*((4-JP1)/7)
        JM1=J00-1+7*((4-J00)/7)
        JM2=JM1-1+7*((4-JM1)/7)
        JM3=JM2-1+7*((4-JM2)/7)
        INDX3_WRK(3,KNT,0)=JP3
        INDX3_WRK(2,KNT,0)=JP2
        INDX3_WRK(1,KNT,0)=JP1
        INDX3_WRK(0,KNT,0)=J00
        INDX3_WRK(-1,KNT,0)=JM1
        INDX3_WRK(-2,KNT,0)=JM2
        INDX3_WRK(-3,KNT,0)=JM3
      ENDDO
!
!***  CASE 1: J EXTENT IS -2 TO 2
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP2=KNT+1-5*((KNT+3)/5)
        JP1=JP2-1+5*((3-JP2)/5)
        J00=JP1-1+5*((3-JP1)/5)
        JM1=J00-1+5*((3-J00)/5)
        JM2=JM1-1+5*((3-JM1)/5)
        INDX3_WRK(3,KNT,1)=999
        INDX3_WRK(2,KNT,1)=JP2
        INDX3_WRK(1,KNT,1)=JP1
        INDX3_WRK(0,KNT,1)=J00
        INDX3_WRK(-1,KNT,1)=JM1
        INDX3_WRK(-2,KNT,1)=JM2
        INDX3_WRK(-3,KNT,1)=999
      ENDDO
!
!***  CASE 2: J EXTENT IS -2 TO 1
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP1=KNT-4*((KNT+2)/4)
        J00=JP1-1+4*((2-JP1)/4)
        JM1=J00-1+4*((2-J00)/4)
        JM2=JM1-1+4*((2-JM1)/4)
        INDX3_WRK(3,KNT,2)=999
        INDX3_WRK(2,KNT,2)=999
        INDX3_WRK(1,KNT,2)=JP1
        INDX3_WRK(0,KNT,2)=J00
        INDX3_WRK(-1,KNT,2)=JM1
        INDX3_WRK(-2,KNT,2)=JM2
        INDX3_WRK(-3,KNT,2)=999
      ENDDO
!
!***  CASE 3: J EXTENT IS -1 TO 2
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP2=KNT+1-4*((KNT+2)/4)
        JP1=JP2-1+4*((3-JP2)/4)
        J00=JP1-1+4*((3-JP1)/4)
        JM1=J00-1+4*((3-J00)/4)
        INDX3_WRK(3,KNT,3)=999
        INDX3_WRK(2,KNT,3)=JP2
        INDX3_WRK(1,KNT,3)=JP1
        INDX3_WRK(0,KNT,3)=J00
        INDX3_WRK(-1,KNT,3)=JM1
        INDX3_WRK(-2,KNT,3)=999
        INDX3_WRK(-3,KNT,3)=999
      ENDDO
!
!***  CASE 4: J EXTENT IS -1 TO 1
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP1=KNT-3*((KNT+1)/3)
        J00=JP1-1+3*((2-JP1)/3)
        JM1=J00-1+3*((2-J00)/3)
        INDX3_WRK(3,KNT,4)=999
        INDX3_WRK(2,KNT,4)=999
        INDX3_WRK(1,KNT,4)=JP1
        INDX3_WRK(0,KNT,4)=J00
        INDX3_WRK(-1,KNT,4)=JM1
        INDX3_WRK(-2,KNT,4)=999
        INDX3_WRK(-3,KNT,4)=999
      ENDDO
!
!***  CASE 5: J EXTENT IS -1 TO 0
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        J00=-MOD(KNT+1,2)
        JM1=-1-J00
        INDX3_WRK(3,KNT,5)=999
        INDX3_WRK(2,KNT,5)=999
        INDX3_WRK(1,KNT,5)=999
        INDX3_WRK(0,KNT,5)=J00
        INDX3_WRK(-1,KNT,5)=JM1
        INDX3_WRK(-2,KNT,5)=999
        INDX3_WRK(-3,KNT,5)=999
      ENDDO
!
!***  CASE 6: J EXTENT IS 0 TO 1
!
      KNT=0
      DO J=MYJS2_P2,MYJE2_P2
        KNT=KNT+1
        JP1=MOD(KNT,2)
        J00=1-JP1
        INDX3_WRK(3,KNT,6)=999
        INDX3_WRK(2,KNT,6)=999
        INDX3_WRK(1,KNT,6)=JP1
        INDX3_WRK(0,KNT,6)=J00
        INDX3_WRK(-1,KNT,6)=999
        INDX3_WRK(-2,KNT,6)=999
        INDX3_WRK(-3,KNT,6)=999
      ENDDO

#ifdef DM_PARALLEL
#  include <HALO_NMM_INIT_1.inc>
#  include <HALO_NMM_INIT_2.inc>
#  include <HALO_NMM_INIT_3.inc>
#  include <HALO_NMM_INIT_4.inc>
#  include <HALO_NMM_INIT_5.inc>
#  include <HALO_NMM_INIT_6.inc>
#  include <HALO_NMM_INIT_7.inc>
#  include <HALO_NMM_INIT_8.inc>
#  include <HALO_NMM_INIT_9.inc>
#  include <HALO_NMM_INIT_10.inc>
#  include <HALO_NMM_INIT_11.inc>
#  include <HALO_NMM_INIT_12.inc>
#  include <HALO_NMM_INIT_13.inc>
#  include <HALO_NMM_INIT_14.inc>
#  include <HALO_NMM_INIT_15.inc>
#  include <HALO_NMM_INIT_16.inc>
#  include <HALO_NMM_INIT_17.inc>
#  include <HALO_NMM_INIT_18.inc>
#  include <HALO_NMM_INIT_19.inc>
#  include <HALO_NMM_INIT_20.inc>
#  include <HALO_NMM_INIT_21.inc>
#  include <HALO_NMM_INIT_22.inc>
#  include <HALO_NMM_INIT_23.inc>
#  include <HALO_NMM_INIT_24.inc>
#  include <HALO_NMM_INIT_25.inc>
#  include <HALO_NMM_INIT_26.inc>
#  include <HALO_NMM_INIT_27.inc>
#  include <HALO_NMM_INIT_28.inc>
#  include <HALO_NMM_INIT_29.inc>
#  include <HALO_NMM_INIT_30.inc>
#  include <HALO_NMM_INIT_31.inc>
#  include <HALO_NMM_INIT_32.inc>
#  include <HALO_NMM_INIT_33.inc>
#  include <HALO_NMM_INIT_34.inc>
#  include <HALO_NMM_INIT_35.inc>
#  include <HALO_NMM_INIT_36.inc>
#  include <HALO_NMM_INIT_37.inc>
#  include <HALO_NMM_INIT_38.inc>
#  include <HALO_NMM_INIT_39.inc>
#endif

#define COPY_OUT
#include <nmm_scalar_derefs.inc>

   RETURN


END SUBROUTINE start_domain_nmm

