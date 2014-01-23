!-----------------------------------------------------------------------
!
!NCEP_MESO:MEDIATION_LAYER:SOLVER
!
!-----------------------------------------------------------------------
#include "nmm_loop_basemacros.h"
#include "nmm_loop_macros.h"
!-----------------------------------------------------------------------
!
      SUBROUTINE SOLVE_NMM(GRID,CONFIG_FLAGS                            &
!
#include "dummy_new_args.inc"
!
     &           )
!-----------------------------------------------------------------------
        use module_timing
      USE MODULE_DOMAIN,                ONLY : DOMAIN, GET_IJK_FROM_GRID &
                                              ,domain_clock_get,is_alarm_tstep_nphs

      USE MODULE_CONFIGURE,             ONLY : GRID_CONFIG_REC_TYPE
      USE MODULE_MODEL_CONSTANTS
      USE MODULE_STATE_DESCRIPTION
      USE MODULE_CTLBLK
      use MODULE_RANDOM,                ONLY : rand_grid_r4
#ifdef DM_PARALLEL
      USE MODULE_DM,                    ONLY : LOCAL_COMMUNICATOR       &
                                              ,MYTASK,NTASKS,NTASKS_X   &
                                              ,NTASKS_Y
      USE MODULE_COMM_DM
#endif
#ifdef HWRF
      USE MODULE_HIFREQ,                ONLY: HIFREQ_WRITE, HIFREQ_OPEN
#endif
      USE MODULE_IGWAVE_ADJUST,         ONLY: PDTE,PFDHT,DDAMP,VTOA
      USE MODULE_ADVECTION,             ONLY: ADVE,VAD2,HAD2            &
                                             ,ADV2,MONO                 &
                                             ,VAD2_SCAL,HAD2_SCAL
      USE MODULE_NONHY_DYNAM,           ONLY: EPS,VADZ,HADZ
      USE MODULE_DIFFUSION_NMM,         ONLY: HDIFF
      USE MODULE_BNDRY_COND,            ONLY: &
           BOCOV, MASS_BOUNDARY, MP_BULK_BOUNDARY, MP_SPECIES_BDY
      USE MODULE_PHYSICS_CALLS
      USE MODULE_EXT_INTERNAL
      USE MODULE_PRECIP_ADJUST
      USE MODULE_NEST_UTIL     ! USEs module_MPP (contains MYPE,NPES,MPI_COMM_COMP)
#ifdef MOVE_NESTS
      USE MODULE_STATS_FOR_MOVE,        ONLY: STATS_FOR_MOVE
#endif
#ifdef WRF_CHEM
      USE MODULE_INPUT_CHEM_DATA,       ONLY: GET_LAST_GAS
#endif
      USE MODULE_DIAGNOSTICS


!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!***  INPUT DATA
!
!-----------------------------------------------------------------------
!
      TYPE(DOMAIN),TARGET :: GRID
!
!***  DEFINITIONS OF DUMMY ARGUMENTS TO THIS ROUTINE (GENERATED FROM REGISTRY)
!
! NOTE, REGISTRY NO LONGER GENERATES DUMMY ARGUMENTS OR DUMMY ARGUMENT
! DECLARATIONS FOR RCONFIG ENTRIES. THEY ARE STILL PART OF STATE. ACCESS
! TO THESE VARIABLES IS NOW THROUGH GRID STRUCTURE, AS MODIFIED BELOW.
! AFFECTED VARIABLES: SIGMA, DT, NPHS, IDTAD, NRADS, NRADL, JULDAY,
! JULYR, NUM_SOIL_LAYERS, NCNVC, ENSDIM, DY, AND SPEC_BDY_WIDTH.
! JM, 20050819
!
!----------------------------
#include <dummy_new_decl.inc>
!----------------------------
!
!***  STRUCTURE THAT CONTAINS RUN-TIME CONFIGURATION (NAMELIST) DATA FOR DOMAIN
!
      TYPE(GRID_CONFIG_REC_TYPE),INTENT(IN) :: CONFIG_FLAGS
#ifdef WRF_CHEM
      INTEGER :: NUMGAS
#endif
!
!-----------------------------------------------------------------------
!
!***  LOCAL VARIABLES
!
!-----------------------------------------------------------------------
!
      INTEGER :: IDS,IDE,JDS,JDE,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                & 
     &          ,IPS,IPE,JPS,JPE,KPS,KPE                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER :: I,ICLTEND,IDF,IRTN,J,JC,JDF,K,KDF,LB,N_MOIST &
     &          ,NTSD_current,L
#ifdef HWRF
!zhang's doing
      INTEGER,SAVE :: NTSD_restart1,NTSD_restart2,NTSD_restart3
#endif
      integer :: ierr,nrand,idt
      INTEGER,SAVE :: NTSD_restart
!     INTEGER :: MPI_COMM_COMP,MYPE,MYPROC,NPES
      INTEGER :: MYPROC,imid,jmid
      INTEGER :: KVH,NTSD_rad,RC
      INTEGER :: NUM_OZMIXM,NUM_AEROSOLC
!
      REAL :: DT_INV,FICE,FRAIN,GPS,QI,QR,QW,WC,WP
!
      LOGICAL :: LAST_TIME,OPERATIONAL_PHYSICS,ETAMP_PHYSICS
!
      CHARACTER(80) :: MESSAGE
!
!***  For precip assimilation:
      INTEGER :: ISTAT,DOM,one
      LOGICAL :: HF
      REAL,ALLOCATABLE,SAVE,DIMENSION(:,:,:) :: PPTDAT
!
!-----------------------------------------------------------------------
!***  For physics compatibility with other packages
!-----------------------------------------------------------------------
!
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: TTEN,QTEN
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: RTHRATEN,RTHBLTEN,RQVBLTEN
!
!-----------------------------------------------------------------------
!
      LOGICAL wrf_dm_on_monitor
      EXTERNAL wrf_dm_on_monitor
!
!-----------------------------------------------------------------------
!***  TIMING VARIABLES
!-----------------------------------------------------------------------
      real,save :: solve_tim,exch_tim,pdte_tim,adve_tim,vtoa_tim        &
     &,            vadz_tim,hadz_tim,eps_tim,vad2_tim,had2_tim          &
     &,            radiation_tim,rdtemp_tim,turbl_tim,cltend_tim        &
     &,            cucnvc_tim,gsmdrive_tim,hdiff_tim,bocoh_tim          &
     &,            pfdht_tim,ddamp_tim,bocov_tim,uv_htov_tim,sum_tim    &
#ifdef HWRF
     &,            adjppt_tim,sst_tim,flux_tim
#else
     &,            adjppt_tim
#endif

! Flag for producing diagnostic fields (e.g., radar reflectivity) 
      LOGICAL                        :: diag_flag

#ifdef NMM_FIND_LOAD_IMBALANCE
      real,save :: loadimbal_tim,previmbal_tim
#endif
      real,save :: exch_tim_max
      real :: btim,btimx
      real :: et_max,this_tim
      integer :: n_print_time
!
#ifdef RSL
      integer rsl_internal_milliclock
      external rsl_internal_milliclock
# define timef rsl_internal_milliclock
#else
      real*8 :: timef
#endif
!-----------------------------------------------------------------------
!
!#ifdef DEREF_KLUDGE
!! SEE http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
!      INTEGER :: SM31,EM31,SM32,EM32,SM33,EM33 
!      INTEGER :: SM31X,EM31X,SM32X,EM32X,SM33X,EM33X
!      INTEGER :: SM31Y,EM31Y,SM32Y,EM32Y,SM33Y,EM33Y
!#endif
!
!-----------------------------------------------------------------------
!***  Passive substance variables
!-----------------------------------------------------------------------
!
      LOGICAL :: EULER
      INTEGER :: IDTADT 
      INTEGER :: IDTADC
      INTEGER :: KS                                                        ! species index in 4d tracer array
!
      REAL,SAVE :: SUMDRRW
!
#ifdef WRF_CHEM
      REAL,ALLOCATABLE,SAVE,DIMENSION(:,:,:,:) ::      &  ! i,j,k,ks
                                                  CHE  &  ! 4d i,j,k chem tracers
                                                 ,CH1  &  ! intermediate tracer variable
                                                 ,CHP  &  ! ch1 at previous time level
                                                 ,TCC     ! time change of tracers
#endif
!-----------------------------------------------------------------------
!
! LIMIT THE NUMBER OF ARGUMENTS IF COMPILED WITH -DLIMIT_ARGS BY COPYING
! SCALAR (NON-ARRAY) ARGUMENTS OUT OF THE GRID DATA STRUCTURE INTO LOCALLY
! DEFINED COPIES (DEFINED IN EM_DUMMY_DECL.INC, ABOVE, AS THEY ARE IF THEY
! ARE ARGUMENTS).  AN EQUIVALENT INCLUDE OF EM_SCALAR_DEREFS.INC APPEARS
! AT THE END OF THE ROUTINE TO COPY BACK ANY CHNAGED NON-ARRAY VALUES.
! THE DEFINITION OF COPY_IN OR COPY_OUT BEFORE THE INCLUDE DEFINES THE
! DIRECTION OF THE COPY.  NMM_SCALAR_DEREFS IS GENERATED FROM REGISTRY.
!
!-----------------------------------------------------------------------
!#define COPY_IN
!#include <scalar_derefs.inc>
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
!-----------------------------------------------------------------------
!***********************************************************************
!***
!***               THE MAIN TIME INTEGRATION LOOP
!***
!-----------------------------------------------------------------------
!
!***  ntsd IS THE TIMESTEP COUNTER (Number of Time Steps Done)
!
!-----------------------------------------------------------------------
!***
!***  ADVANCE_count STARTS AT ZERO FOR ALL RUNS (REGULAR AND RESTART).
!***
!-----------------------------------------------------------------------
!
      CALL DOMAIN_CLOCK_GET(GRID,ADVANCEcOUNT=NTSD_current)
!
      IF(NTSD_current==0)THEN
        IF(GRID%RESTART.AND.GRID%TSTART>0.)THEN
#ifdef HWRF
!zhang's doing: temporarily hardwired for two domains
          if( grid%id .eq. 1 ) NTSD_restart1=INT(grid%TSTART*3600./GRID%DT+0.5)
          if( grid%id .eq. 2 ) NTSD_restart2=INT(grid%TSTART*3600./GRID%DT+0.5)
          if( grid%id .eq. 3 ) NTSD_restart3=INT(grid%TSTART*3600./GRID%DT+0.5)
#endif
          IHRST=grid%nstart_hour
          NTSD_restart=grid%ntsd
        ELSE
          IHRST=GRID%GMT
          grid%nstart_hour=IHRST
#ifdef HWRF
!zhang's doing
          NTSD_restart1=0
          NTSD_restart2=0
          NTSD_restart3=0
#else
          NTSD_restart=0
#endif
        ENDIF
      ENDIF
#ifdef HWRF
!zhang's doing
      if( grid%id .eq. 1 ) grid%ntsd=NTSD_restart1+NTSD_current
      if( grid%id .eq. 2 ) grid%ntsd=NTSD_restart2+NTSD_current
      if( grid%id .eq. 3 ) grid%ntsd=NTSD_restart3+NTSD_current
#else
      grid%ntsd=NTSD_restart+NTSD_current
#endif
      LAST_TIME=domain_last_time_step(GRID)
!-----------------------------------------------------------------------------
!
! Set diagnostic flag value at history output time
!-----------------------------------------------------------------------------
    diag_flag = .false.
    if ( Is_alarm_tstep_nphs(grid%domain_clock, grid%alarms(HISTORY_ALARM), grid%nphs) ) then
       diag_flag = .true.
    endif

!
!-----------------------------------------------------------------------
!
!!!!! IF(WRF_DM_ON_MONITOR() )THEN
        WRITE(MESSAGE,125)grid%ntsd,grid%ntsd*GRID%DT/3600.
  125   FORMAT(' SOLVE_NMM: TIMESTEP IS ',I5,'   TIME IS ',F7.3,' HOURS')
        CALL WRF_MESSAGE(TRIM(MESSAGE))
!!!!  ENDIF
!
!-----------------------------------------------------------------------
!
      EULER=model_config_rec%EULER_ADV
      IDTADT=model_config_rec%IDTADT
      IDTADC=model_config_rec%IDTADC
      WP=model_config_rec%WP(grid%id)
!
!-----------------------------------------------------------------------
      CALL WRF_GET_DM_COMMUNICATOR(MPI_COMM_COMP)
      CALL WRF_GET_NPROC(NPES)
      CALL WRF_GET_MYPROC(MYPROC)
      MYPE=MYPROC
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
!
!-----------------------------------------------------------------------
!***  SET FLAG FOR NAM, HRW, or HWRF (Ferrier-based) microphysics
!-----------------------------------------------------------------------
!
      ETAMP_PHYSICS=.FALSE.
!
      IF (CONFIG_FLAGS%MP_PHYSICS == ETAMPNEW .OR.                 &
     &    CONFIG_FLAGS%MP_PHYSICS == ETAMPOLD .OR.                 &
     &    CONFIG_FLAGS%MP_PHYSICS == ETAMP_HWRF ) THEN
!
         ETAMP_PHYSICS=.TRUE.
!
      ENDIF



!
!-----------------------------------------------------------------------
!***  SET FLAG FOR THE OPERATIONAL PHYSICS SUITE.
!***  THIS WILL BE USED TO SAVE CLOCKTIME BY SKIPPING
!***  FREQUENT UPDATES OF THE MOIST ARRAY AND INSTEAD
!***  UPDATE IT ONLY WHEN IT IS NEEDED FOR PHYSICS.
!-----------------------------------------------------------------------
!
      OPERATIONAL_PHYSICS=.FALSE.
!
      IF(CONFIG_FLAGS%RA_SW_PHYSICS    ==GFDLSWSCHEME.AND.              &
     &   CONFIG_FLAGS%RA_LW_PHYSICS    ==GFDLLWSCHEME.AND.              &
     &   CONFIG_FLAGS%SF_SFCLAY_PHYSICS==MYJSFCSCHEME.AND.              &
     &   CONFIG_FLAGS%BL_PBL_PHYSICS   ==MYJPBLSCHEME.AND.              &
     &   CONFIG_FLAGS%CU_PHYSICS       ==BMJSCHEME.AND.                 &
     &   ETAMP_PHYSICS ) THEN

!
        OPERATIONAL_PHYSICS=.TRUE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!***  TTEN, QTEN are used by GD convection scheme
!
      ALLOCATE(TTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(QTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RTHBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RQVBLTEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
      ALLOCATE(RTHRATEN(IMS:IME,KMS:KME,JMS:JME),STAT=ISTAT)
#ifdef WRF_CHEM
  NUMGAS          = GET_LAST_GAS(CONFIG_FLAGS%CHEM_OPT)
#endif
!
        IF(CONFIG_FLAGS%CU_PHYSICS==GDSCHEME.OR.                        &
     &  CONFIG_FLAGS%CU_PHYSICS==TIEDTKESCHEME.OR.                      &
     &  CONFIG_FLAGS%CU_PHYSICS==KFETASCHEME)THEN

        DO J=JMS,JME
        DO K=KMS,KME
        DO I=IMS,IME
          TTEN(I,K,J)=grid%t(I,J,K)
          QTEN(I,K,J)=grid%q(I,J,K)
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!
      GRID%SIGMA=1 

      IF (config_flags%non_hydrostatic) THEN
        grid%hydro=.FALSE.
      ELSE
        grid%hydro=.TRUE.
      ENDIF
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
!
      if(grid%ntsd==0)then
        write(message,*)' its=',its,' ite=',ite
        call wrf_message(trim(message))
        write(message,*)' jts=',jts,' jte=',jte
        call wrf_message(trim(message))
        write(message,*)' kts=',kts,' kte=',kte
        call wrf_message(trim(message))
!
#ifdef WRF_CHEM
        ALLOCATE (CHE(IMS:IME,JMS:JME,KMS:KME,1:NUM_CHEM),STAT=ISTAT)
        ALLOCATE (CH1(IMS:IME,JMS:JME,KMS:KME,1:NUM_CHEM),STAT=ISTAT)
        ALLOCATE (CHP(IMS:IME,JMS:JME,KMS:KME,1:NUM_CHEM),STAT=ISTAT)
        ALLOCATE (TCC(IMS:IME,JMS:JME,KMS:KME,1:NUM_CHEM),STAT=ISTAT)
#endif
!-----------------------------------------------------------------------
      endif
!-----------------------------------------------------------------------
!***  SET TIMING VARIABLES TO ZERO AT START OF FORECAST.
!-----------------------------------------------------------------------
      if(grid%ntsd==0)then
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
        adjppt_tim=0.
#ifdef NMM_FIND_LOAD_IMBALANCE
        previmbal_tim=0.
        loadimbal_tim=0.
#endif
      endif
!-----------------------------------------------------------------------
      N_MOIST=NUM_MOIST

#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(previmbal_tim)
#endif
!
      DO J=MYJS_P4,MYJE_P4
        grid%iheg(J)=MOD(J+1,2)
        grid%ihwg(J)=grid%iheg(J)-1
        grid%iveg(J)=MOD(J,2)
        grid%ivwg(J)=grid%iveg(J)-1
      ENDDO

      DO J=MYJS_P4,MYJE_P4
        grid%ivw(J)=grid%ivwg(J)
        grid%ive(J)=grid%iveg(J)
        grid%ihe(J)=grid%iheg(J)
        grid%ihw(J)=grid%ihwg(J)
      ENDDO
!
!***  LATERAL POINTS IN THE BOUNDARY ARRAYS
!
      LB=2*(IDF-IDS+1)+(JDF-JDS+1)-3
!
!***  APPROXIMATE GRIDPOINT SPACING (METERS)
!
      JC=JMS+(JME-JMS)/2
      GPS=SQRT(grid%dx_nmm(IMS,JC)**2+grid%dy_nmm**2)
!
!***  TIMESTEPS PER HOUR
!
      TSPH=3600./GRID%DT
!
      n_print_time=nint(3600./grid%dt)   ! Print stats once per hour
!-----------------------------------------------------------------------
!
      NBOCO=0
!
!-----------------------------------------------------------------------
!
#if (NMM_NEST == 1)
!-----------------------------------------------------------------------------
!***  PATCHING NESTED BOUNDARIES.
!-----------------------------------------------------------------------------
!
      CALL wrf_debug ( 100 , 'nmm: in patch' )
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
      btimx=timef()
!#ifdef DM_PARALLEL
!#    include "HALO_NMM_ZZ.inc"
!#endif

      IF(GRID%ID/=1)THEN
!
#ifdef MOVE_NESTS

        IF(GRID%ID/=1.AND.MOD(grid%ntsd,1)==0.AND.GRID%NUM_MOVES==-99)THEN
          grid%XLOC_1=(IDE-1)/2     ! This maneuvers the storm to the center of the nest quickly
          grid%YLOC_1=(JDE-1)/2     ! This maneuvers the storm to the center of the nest quickly
        ENDIF
#endif

      ENDIF
#endif
!
!-----------------------------------------------------------------------
!***  ALLOCATE PPTDAT ARRAY (PRECIP ASSIM):
!-----------------------------------------------------------------------
!
      IF(GRID%PCPFLG.AND..NOT.ALLOCATED(PPTDAT))THEN
        ALLOCATE(PPTDAT(IMS:IME,JMS:JME,3),STAT=ISTAT)
      ENDIF
!
!-----------------------------------------------------------------------
!***
!***      Call READPCP to
!***            1) READ IN PRECIPITATION FOR HOURS 1, 2 and 3;
!***            2) Initialize grid%ddata to 999. (this is the amount
!***               of input precip allocated to each physics time step
!***               in ADJPPT; TURBL/SURFCE, which uses grid%ddata, is called
!***               before ADJPPT)
!***            3) Initialize grid%lspa to zero
!***
!-----------------------------------------------------------------------

      IF (grid%ntsd==0) THEN
        IF (GRID%PCPFLG) THEN
          CALL READPCP(PPTDAT,grid%ddata,grid%lspa                                &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
      ENDIF
!-----------------------------------------------------------------------
!
      btim=timef()
!
!-----------------------------------------------------------------------
!***  UPDATE RANDOM NUMBERS IF REQUIRED
!-----------------------------------------------------------------------

      randif: IF(in_use_for_config(grid%id,'random')) THEN
!
         nrand=config_flags%nrand
         if(nrand==0) nrand=grid%ncnvc
         if(nrand==0) nrand=1
         IDT=MOD(grid%NTSD,nrand)
 
         IF(IDT.EQ.0 .OR. grid%NTSD .EQ. 0)THEN
            call start_timing
            call wrf_message('Update random numbers...')
            one=1

            imid=(its+ite)/2   ;   jmid=(jts+jte)/2

            write(message,'(A,": random(",I0,",",I0,") = ",E15.10)') 'before call',imid,jmid,grid%random(imid,jmid)
            call wrf_debug(3,message)

            call rand_grid_r4(grid%randstate1,grid%randstate2, &
                              grid%randstate3,grid%randstate4, &
                              grid%random, &
                              IDS,IDE,JDS,JDE,one,one, &
                              IMS,IME,JMS,JME,one,one, &
                              ITS,ITE,JTS,JTE,one,one)

            write(message,'(A,": random(",I0,",",I0,") = ",E15.10)') 'after call',imid,jmid,grid%random(imid,jmid)
            call wrf_debug(3,message)

            call end_timing('Updating random numbers')
         ENDIF    ! for IF(IDT.EQ.0 .OR. NTSD .EQ. 0)
      ENDIF randif
!-----------------------------------------------------------------------
!***  ZERO OUT ACCUMULATED QUANTITIES WHEN NEEDED.
!-----------------------------------------------------------------------
!
      CALL BUCKETS(grid%ntsd,grid%nprec,grid%nsrfc,grid%nrdsw,grid%nrdlw                         &
     &            ,GRID%RESTART,GRID%TSTART                             &
     &            ,grid%nclod,grid%nheat,GRID%NPHS,TSPH                           &
     &            ,grid%acprec,grid%cuprec,grid%acsnow,grid%acsnom,grid%ssroff,grid%bgroff            &
     &            ,grid%sfcevp,grid%potevp,grid%sfcshx,grid%sfclhx,grid%subshx,grid%snopcx            &
     &            ,grid%sfcuvx,grid%potflx                                        &
     &            ,grid%ardsw,grid%aswin,grid%aswout,grid%aswtoa                            &
     &            ,grid%ardlw,grid%alwin,grid%alwout,grid%alwtoa                            &
     &            ,grid%acfrst,grid%ncfrst,grid%acfrcv,grid%ncfrcv                          &
     &            ,grid%avcnvc,grid%avrain,grid%tcucn,grid%train                            &
     &            ,grid%asrfc                                                &
     &            ,grid%t,grid%tlmax,grid%tlmin,grid%tshltr,grid%pshltr,grid%qshltr                   &
     &            ,grid%t02_max,grid%t02_min,grid%rh02_max,grid%rh02_min                    &
     &            ,IDS,IDE,JDS,JDE,KDS,KDE                              &
     &            ,IMS,IME,JMS,JME,KMS,KME                              &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)
!-----------------------------------------------------------------------
!
!!#ifdef HWRF
!!!zhang
!!      IF(NTSD_current==0)THEN
!!#else
      IF(grid%ntsd==0)THEN
!!#endif
        FIRST=.TRUE.
!       call hpm_init()
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
        btimx=timef()
!emc_2010_bugfix_h50
        grid%mommix=amin1(grid%mommix,1.0)
!emc_2010_bugfix_h50
!
!-----------------------------------------------------------------------
!***  FIRST STEP INITIALIZATION OF PASSIVE SUBSTANCE VARIABLES
!-----------------------------------------------------------------------
!
      IF(EULER) THEN
        SUMDRRW=0.
!
        DO K=KTS,KTE
          DO J=JMS,JME
            DO I=IMS,IME
              grid%rrw(I,J,K)=0.
!
              IF(I>=IDE/2-6.AND.I<=IDE/2+6.AND.   &
                 J>=JDE/2-6.AND.J<=JDE/2+6     ) THEN
                grid%rrw(I,J,K)=10.0 !youhua
!               grid%rrw(I,J,K)=0.9  !zj
              ENDIF
!
            ENDDO
          ENDDO
        ENDDO
!
        DO KS=PARAM_FIRST_SCALAR,NUM_SZJ
          DO K=KMS,KME
            DO J=JMS,JME
              DO I=IMS,IME
                SZJ(I,J,K,KS)=0.
                S1Z(I,J,K,KS)=0.
                SPZ(I,J,K,KS)=0.
                TCS(I,J,K,KS)=0.
              ENDDO
            ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
#ifdef WRF_CHEM
      DO KS=1,NUM_CHEM
        DO K=KMS,KME
          DO J=JMS,JME
            DO I=IMS,IME
              CHE(I,J,K,KS)=0.
              CH1(I,J,K,KS)=0.
              CHP(I,J,K,KS)=0.
              TCC(I,J,K,KS)=0.
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif
!
!-----------------------------------------------------------------------
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
!
!-----------------------------------------------------------------------
#ifdef DM_PARALLEL
      IF (.NOT.ETAMP_PHYSICS) THEN
#    include "HALO_NMM_A_3.inc"
      ENDIF
#endif
!-----------------------------------------------------------------------
!***  FIRST STEP INITIALIZATION OF PASSIVE SUBSTANCE VARIABLES
!-----------------------------------------------------------------------
!
      IF(EULER) THEN
!
        DO K=KTS,KTE
          DO J=JMS,JME
            DO I=IMS,IME
              SPZ(I,J,K,P_SPZ1)=SQRT(MAX(grid%q  (I,J,K),EPSQ))
              SPZ(I,J,K,P_SPZ2)=SQRT(MAX(grid%cwm(I,J,K),EPSQ))
              SPZ(I,J,K,P_SPZ4)=SQRT(MAX(grid%rrw(I,J,K),0.  ))
            ENDDO
          ENDDO
        ENDDO
!
        DO J=JMS,JME
          DO I=IMS,IME
            SPZ(I,J,KTE,P_SPZ3)=SQRT(MAX((grid%q2(I,J,KTE)+EPSQ2)*0.5,EPSQ2))
          ENDDO
        ENDDO
!
        DO K=KTE-1,KTS,-1
          DO J=JMS,JME
            DO I=IMS,IME
              SPZ(I,J,K,P_SPZ3)=SQRT(MAX((grid%q2(I,J,K)+grid%q2(I,J,K+1))*0.5,EPSQ2))
            ENDDO
          ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
#ifdef WRF_CHEM
      DO KS=1,NUM_CHEM
        DO K=KMS,KME
          DO J=JMS,JME
            DO I=IMS,IME
              CHP(I,J,K,KS)=SQRT(MAX(CHEM(I,K,J,KS),0.  ))
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif
!
!-----------------------------------------------------------------------
!
!***  Only for chemistry:
!
#ifdef WRF_CHEM   
#ifdef DM_PARALLEL
#    include "HALO_NMM_A_2.inc"
#endif
#endif
!
!-----------------------------------------------------------------------
!***  USE THE FOLLOWING VARIABLES TO KEEP TRACK OF EXCHANGE TIMES.
!-----------------------------------------------------------------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!-----------------------------------------------------------------------
!
#ifdef HWRF
!zhang's doing
       if(GRID%RESTART) then
       FIRST=.FALSE.
       else
       GO TO 2003
       endif
!end of zhang's doing
#else
        GO TO 2003
#endif
      ENDIF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
 2000 CONTINUE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
#ifdef HWRF
! Coupling insertion:->
      call ATM_TSTEP_INIT(NTSD_current,grid%NPHS,GRID%ID,grid%NPHS*grid%dt, &
      ids,idf,jds,jdf,its,ite,jts,jte,ims,ime,jms,jme,           &
                       kds,kde,kts,kte,kms,kme,                  &
                       grid%HLON,grid%HLAT,grid%VLON,grid%VLAT,grid%sm,                   &
                       grid%i_parent_start,grid%j_parent_start,  &
                       grid%guessdtc,grid%dtc)
!<-:coupling insertion
!
#endif
!-----------------------------------------------------------------------
!***  PRESSURE TENDENCY, SIGMA DOT, VERTICAL PART OF OMEGA-ALPHA
!-----------------------------------------------------------------------
!
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_D.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL PDTE(                                                        &
#ifdef DM_PARALLEL
     &            GRID,MYPE,MPI_COMM_COMP,                              &
#endif
     &            grid%ntsd,GRID%DT,grid%pt,grid%eta2,grid%res,grid%hydro,grid%hbm2                   &
     &           ,grid%pd,grid%pdsl,grid%pdslo                                         &
     &           ,grid%petdt,grid%div,grid%psdt                                        &
     &           ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                       &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &           ,IMS,IME,JMS,JME,KMS,KME                               &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)


      pdte_tim=pdte_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  ADVECTION OF grid%t, grid%u, AND grid%v
!-----------------------------------------------------------------------
!
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_F.inc"
#    include "HALO_NMM_F1.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
      btimx=timef()
!
      CALL ADVE(grid%ntsd,GRID%DT,grid%deta1,grid%deta2,grid%pdtop                          &
     &         ,grid%curv,grid%f,grid%fad,grid%f4d,grid%em_loc,grid%emt_loc,grid%en,grid%ent,grid%dx_nmm,grid%dy_nmm      &
     &         ,grid%hbm2,grid%vbm2                                               &
     &         ,grid%t,grid%u,grid%v,grid%pdslo,grid%told,grid%uold,grid%vold                              &
     &         ,grid%petdt,grid%upstrm                                            &
     &         ,grid%few,grid%fns,grid%fne,grid%fse                                         &
     &         ,grid%adt,grid%adu,grid%adv                                             & 
     &         ,grid%n_iup_h,grid%n_iup_v                                         &
     &         ,grid%n_iup_adh,grid%n_iup_adv                                     &
     &         ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                             &
     &         ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      adve_tim=adve_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  PASSIVE SUBSTANCE WORKING PART
!-----------------------------------------------------------------------
!
      eulerian: IF(EULER) THEN    ! Eulerian advection for model tracers
!
!-----------------------------------------------------------------------
!
!mp - allow for it to be applied in the no-physics realm
        IF(.NOT.ETAMP_PHYSICS.and.CONFIG_FLAGS%MP_PHYSICS/=0) THEN
          WRITE( wrf_err_message , * ) 'EULER advection works only with ETAMPNEW microphysics.'
          CALL wrf_error_fatal ( wrf_err_message )
        ENDIF
!
!-----------------------------------------------------------------------
        idtadt_block: IF(MOD(grid%ntsd,IDTADT)==0) THEN
!-----------------------------------------------------------------------
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
          btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_I.inc"
#endif
          exch_tim=exch_tim+timef()-btimx
!
          btimx=timef()
!
          DO K=KTS,KTE
            DO J=JMS,JME
              DO I=IMS,IME
                SZJ(I,J,K,P_SPZ1)=MAX(grid%q  (I,J,K),EPSQ)
                SZJ(I,J,K,P_SPZ2)=MAX(grid%cwm(I,J,K),EPSQ)
                SZJ(I,J,K,P_SPZ4)=MAX(grid%rrw(I,J,K),0.  )
              ENDDO
            ENDDO
          ENDDO
!
          DO J=JMS,JME
            DO I=IMS,IME
              SZJ(I,J,KTE,P_SPZ3)=MAX((grid%q2 (I,J,KTE)+EPSQ2)*0.5,EPSQ2)
            ENDDO
          ENDDO
!
          DO K=KTE-1,KTS,-1
            DO J=JMS,JME
              DO I=IMS,IME
                SZJ(I,J,K,P_SPZ3)=MAX((grid%q2 (I,J,K)+grid%q2 (I,J,K+1))*0.5,EPSQ2)
              ENDDO
            ENDDO
          ENDDO
!
#ifdef DM_PARALLEL
#    include "HALO_TRACERS.inc"
#endif
          CALL ADV2                &
          (grid%upstrm                  &
          ,MYPE,PARAM_FIRST_SCALAR,NUM_SZJ &
          ,IDS,IDE,JDS,JDE,KDS,KDE &
          ,IMS,IME,JMS,JME,KMS,KME &
          ,ITS,ITE,JTS,JTE,KTS,KTE &
          ,grid%n_iup_h                 &
          ,grid%n_iup_adh               &
          ,grid%iup_h,grid%iup_adh           &
          ,grid%ent                     &
          ,IDTADT                  &
          ,grid%DT,grid%pdtop           &
          ,grid%ihe,grid%ihw,grid%ive,grid%ivw         &
          ,grid%deta1,grid%deta2             &
          ,grid%emt_loc                 &
          ,grid%fad,grid%hbm2,grid%pdsl,grid%pdslo     &
          ,grid%petdt                   &
          ,grid%uold,grid%vold               &
          ,SZJ,SPZ                 &
          !temporary arguments
          ,grid%fne,grid%fse,grid%few,grid%fns,S1Z,TCS)
!
#ifdef DM_PARALLEL
#    include "HALO_TRACERS.inc"
#endif
          CALL MONO &
          (         &
#if defined(DM_PARALLEL)
           GRID%DOMDESC, &
#endif
           MYPE,grid%ntsd,grid%ntsd*GRID%DT/3600.,PARAM_FIRST_SCALAR,NUM_SZJ &
          ,IDS,IDE,JDS,JDE,KDS,KDE              &
          ,IMS,IME,JMS,JME,KMS,KME              &
          ,ITS,ITE,JTS,JTE,KTS,KTE              &
          ,IDTADT                               &
          ,grid%dy_nmm,grid%pdtop                         &
          ,SUMDRRW                              &
          ,grid%ihe,grid%ihw                              &
          ,grid%deta1,grid%deta2                          &
          ,grid%dx_nmm,grid%hbm2,grid%pdsl                     &
          ,SZJ                                  &
          !temporary arguments
          ,S1Z,TCS)
!
          DO KS=PARAM_FIRST_SCALAR,NUM_SZJ ! loop by species
            DO K=KTS,KTE
              DO J=MYJS2,MYJE2
                DO I=MYIS1,MYIE1
                  SZJ(I,J,K,KS)=SZJ(I,J,K,KS)+TCS(I,J,K,KS)
                ENDDO
              ENDDO
            ENDDO
          ENDDO         ! end of the loop by the species
!
          DO K=KTS,KTE
            DO J=MYJS2,MYJE2
              DO I=MYIS1,MYIE1
                grid%q  (I,J,K)=SZJ(I,J,K,P_SZJ1)
                grid%cwm(I,J,K)=SZJ(I,J,K,P_SZJ2)
                grid%rrw(I,J,K)=SZJ(I,J,K,P_SZJ4)
              ENDDO
            ENDDO
          ENDDO
!
          DO J=MYJS2,MYJE2
            DO I=MYIS1,MYIE1
              grid%q2(I,J,KTE)=MAX(SZJ(I,J,KTE,P_SZJ3)+SZJ(I,J,KTE,P_SZJ3)-EPSQ2 &
                             ,EPSQ2)
            ENDDO
          ENDDO
!
          DO K=KTE-1,KTS+1,-1
            DO J=MYJS2,MYJE2
              DO I=MYIS1,MYIE1
                IF(K>KTS)THEN
                  grid%q2(I,J,K)=MAX(SZJ(I,J,K,P_SZJ3)+SZJ(I,J,K,P_SZJ3)-grid%q2(I,J,K+1) &
                               ,EPSQ2)
                ELSE
                  grid%q2(I,J,K)=grid%q2(I,J,K+1)
                ENDIF
              ENDDO
            ENDDO
          ENDDO
!-----------------------------------------------------------------------

!
!***  UPDATE MOIST ARRAY.
!***  REMEMBER THAT MOIST IS ONLY USED WITH THE PHYSICS AND THUS
!***  THE P_QV SLOT (=2) IS MIXING RATIO, NOT SPECIFIC HUMIDITY.
!***  ALTHOUGH MOIST IS ONLY USED FOR PHYSICS IN OPERATIONS, IT IS
!***  UPDATED HERE FROM grid%q EVERY ADVECTION TIMESTEP FOR NON-OPERATIONAL
!***  CONFIGURATIONS WHERE IT MAY BE USED OUTSIDE OF THE PHYSICS.
!
        IF(.NOT.OPERATIONAL_PHYSICS)THEN
          DO K=KTS,KTE
            DO J=MYJS,MYJE
              DO I=MYIS,MYIE
                MOIST(I,J,K,P_QV)=grid%q(I,J,K)/(1.-grid%q(I,J,K))
                WC = grid%cwm(I,J,K)
                QI = 0.
                QR = 0.
                QW = 0.
                FICE=grid%f_ice(I,K,J)
                FRAIN=grid%f_rain(I,K,J)
!
                IF(FICE>=1.)THEN
                  QI=WC
                ELSEIF(FICE<=0.)THEN
                  QW=WC
                ELSE
                  QI=FICE*WC
                  QW=WC-QI
                ENDIF
!
                IF(QW>0..AND.FRAIN>0.)THEN
                  IF(FRAIN>=1.)THEN
                    QR=QW
                    QW=0.
                  ELSE
                    QR=FRAIN*QW
                    QW=QW-QR
                  ENDIF
                ENDIF
!
                MOIST(I,J,K,P_QC)=QW
                MOIST(I,J,K,P_QR)=QR
                MOIST(I,J,K,P_QI)=0.
                MOIST(I,J,K,P_QS)=QI
                MOIST(I,J,K,P_QG)=0.
              ENDDO
            ENDDO
          ENDDO
        ENDIF
!
        had2_tim=had2_tim+timef()-btimx
!-----------------------------------------------------------------------
!
        ENDIF idtadt_block
!
!-----------------------------------------------------------------------
!
      ENDIF eulerian  ! eulerian advection for model tracers
!
!-----------------------------------------------------------------------
!
#ifdef WRF_CHEM
!-----------------------------------------------------------------------
!
      idtadc_block: IF(MOD(grid%ntsd,IDTADC)==0) THEN
!
!-----------------------------------------------------------------------
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
        btimx=timef()
#ifdef DM_PARALLEL
#    include "HALO_NMM_I_2.inc"
#endif
        exch_tim=exch_tim+timef()-btimx
!
        btimx=timef()
!
        do KS=1,NUM_CHEM
          DO K=KTS,KTE
            DO J=JMS,JME
              DO I=IMS,IME
                CHE(I,J,K,KS)=MAX(CHEM(I,K,J,KS),0.  )
              ENDDO
            ENDDO
          ENDDO
        ENDDO
!
        CALL ADV2                &
        (grid%upstrm                  &
        ,MYPE,1,NUM_CHEM         &
        ,IDS,IDE,JDS,JDE,KDS,KDE &
        ,IMS,IME,JMS,JME,KMS,KME &
        ,ITS,ITE,JTS,JTE,KTS,KTE &
        ,grid%n_iup_h                 &
        ,grid%n_iup_adh               &
        ,grid%iup_h,grid%iup_adh           &
        ,grid%ent                     &
        ,IDTADC                  &
        ,grid%DT,grid%pdtop           &
        ,grid%ihe,grid%ihw,grid%ive,grid%ivw         &
        ,grid%deta1,grid%deta2             &
        ,grid%emt_loc                 &
        ,grid%fad,grid%hbm2,grid%pdsl,grid%pdslo     &
        ,grid%petdt                   &
        ,grid%uold,grid%vold               &
        ,CHE,CHP                 &
        !temporary arguments
        ,grid%fne,grid%fse,grid%few,grid%fns,CH1,TCC)
!
        CALL MONO &
        (         &
#if defined(DM_PARALLEL)
         GRID%DOMDESC, &
#endif
         MYPE,grid%ntsd,grid%ntsd*GRID%DT/3600.,1,NUM_CHEM &
        ,IDS,IDE,JDS,JDE,KDS,KDE &
        ,IMS,IME,JMS,JME,KMS,KME &
        ,ITS,ITE,JTS,JTE,KTS,KTE &
        ,IDTADT                  &
        ,grid%dy_nmm,grid%pdtop            &
        ,SUMDRRW                 &
        ,grid%ihe,grid%ihw                 &
        ,grid%deta1,grid%deta2             &
        ,grid%dx_nmm,grid%hbm2,grid%pdsl        &
        ,CHE                     &
        !temporary arguments
        ,CH1,TCC)
!
        DO KS=1,NUM_CHEM
          DO K=KTS,KTE
            DO J=JMS,JME
              DO I=IMS,IME
                CHEM(I,K,J,KS)=CHE(I,J,K,KS)+TCC(I,J,K,KS)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
!-----------------------------------------------------------------------
!
      ENDIF idtadc_block
!
!-----------------------------------------------------------------------
#endif
!
!-----------------------------------------------------------------------
!***  PRESSURE TENDENCY, ETA/SIGMADOT, VERTICAL PART OF OMEGA-ALPHA TERM
!-----------------------------------------------------------------------
!
      btimx=timef()
!
      CALL VTOA(                                                        &
     &          grid%ntsd,GRID%DT,grid%pt,grid%eta2                                    &
     &         ,grid%hbm2,grid%ef4t                                               &
     &         ,grid%t,grid%dwdt,grid%rtop,grid%omgalf                                      &
     &         ,grid%pint,grid%div,grid%psdt,grid%res                                       &
     &         ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                         &
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



      CALL VADZ(grid%ntsd,GRID%DT,grid%fis,GRID%SIGMA,grid%dfl,grid%hbm2                    &
     &         ,grid%deta1,grid%deta2,grid%pdtop                                       &
     &         ,grid%pint,grid%pdsl,grid%pdslo,grid%petdt                                   &
     &         ,grid%rtop,grid%t,grid%q,grid%cwm,grid%z,grid%w,grid%dwdt,grid%pdwdt                             &
     &         ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)

      vadz_tim=vadz_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF HEIGHT
!-----------------------------------------------------------------------
!
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_G.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL HADZ(grid%ntsd,GRID%DT,grid%hydro,grid%hbm2,grid%deta1,grid%deta2,grid%pdtop               &
     &         ,grid%dx_nmm,grid%dy_nmm,grid%fad                                       &
     &         ,grid%few,grid%fns,grid%fne,grid%fse                                         &
     &         ,grid%pdsl,grid%u,grid%v,grid%w,grid%z,WP,grid%BARO                          &
     &         ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                         &
     &         ,IDS,IDF,JDS,JDF,KDS,KDE                                 &
     &         ,IMS,IME,JMS,JME,KMS,KME                                 &
     &         ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      hadz_tim=hadz_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!***  ADVECTION OF grid%w
!-----------------------------------------------------------------------
!
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_H.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!

      CALL EPS(grid%ntsd,GRID%DT,grid%hydro,grid%dx_nmm,grid%dy_nmm,grid%fad                     &
     &        ,grid%aeta1,grid%deta1,grid%deta2,grid%pdtop,grid%pt                                     &
     &        ,grid%hbm2,grid%hbm3                                                &
     &        ,grid%pdsl,grid%pdslo,grid%pint,grid%rtop,grid%petdt,grid%pdwdt                         &
     &        ,grid%dwdt,grid%dwdtmn,grid%dwdtmx                                       &
     &        ,grid%fns,grid%few,grid%fne,grid%fse                                          &
     &        ,grid%t,grid%u,grid%v,grid%w,grid%w_tot,grid%q,grid%cwm                                            &
     &        ,grid%def3d,grid%hdac,grid%baro                                              &
     &        ,WP                                                 &
     &        ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                          &
     &        ,IDS,IDF,JDS,JDF,KDS,KDE                                  &
     &        ,IMS,IME,JMS,JME,KMS,KME                                  &
     &        ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      eps_tim=eps_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!
      not_euler: IF(.NOT.EULER) THEN ! Lagrangian model tracer advection
!
!-----------------------------------------------------------------------
!***  VERTICAL ADVECTION OF grid%q, TKE, AND CLOUD WATER
!-----------------------------------------------------------------------
!
      IF(MOD(grid%ntsd,GRID%IDTAD)==0)THEN
        btimx=timef()
!
        vad2_micro_check: IF (ETAMP_PHYSICS) THEN
          CALL VAD2(grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm               &
     &             ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop,grid%hbm2             &
     &             ,grid%q,grid%q2,grid%cwm,grid%petdt                                      &
     &             ,grid%n_iup_h,grid%n_iup_v                                     &
     &             ,grid%n_iup_adh,grid%n_iup_adv                                 &
     &             ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                         &
     &             ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                     &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &             ,IMS,IME,JMS,JME,KMS,KME                             &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        ELSE vad2_micro_check
          CALL VAD2_SCAL(grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm          &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop             &
     &                  ,grid%hbm2                                           &
     &                  ,grid%q2,grid%petdt                                       &
     &                  ,grid%n_iup_h,grid%n_iup_v                                &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                            &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                    &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                &
     &                  ,1,1                                            &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)                              
     
          CALL VAD2_SCAL(grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm          &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop             &
     &                  ,grid%hbm2                                           &
     &                  ,MOIST,grid%petdt                                    &
     &                  ,grid%n_iup_h,grid%n_iup_v                                &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                            &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                    &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                &
     &                  ,NUM_MOIST,2                                    &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!
          CALL VAD2_SCAL(grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm          &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop             &
     &                  ,grid%hbm2                                           &
     &                  ,SCALAR,grid%petdt                                   &
     &                  ,grid%n_iup_h,grid%n_iup_v                                &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                            &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                    &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                &
     &                  ,NUM_SCALAR,2                                   &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                        &
     &                  ,IMS,IME,JMS,JME,KMS,KME                        &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!

          DO K=KTS,KTE
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            grid%q(I,J,K)=MOIST(I,J,K,P_QV)/(1.+MOIST(I,J,K,P_QV))
          ENDDO
          ENDDO   
          ENDDO   
!
        ENDIF vad2_micro_check
!
        vad2_tim=vad2_tim+timef()-btimx
!
      ENDIF
!    
!-----------------------------------------------------------------------
!***  HORIZONTAL ADVECTION OF grid%q, TKE, AND CLOUD WATER
!-----------------------------------------------------------------------
!
      idtad_block: IF(MOD(grid%ntsd,GRID%IDTAD)==0)THEN
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_I.inc"
#endif
!
#ifdef DM_PARALLEL
        IF (.NOT.ETAMP_PHYSICS) THEN
#    include "HALO_NMM_I_3.inc"
        ENDIF
#endif
!
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        btimx=timef()
!
!-----------------------------------------------------------------------
        had2_micro_check: IF (ETAMP_PHYSICS) THEN
!-----------------------------------------------------------------------
!
          CALL HAD2(                                                   &
#if defined(DM_PARALLEL)
     &              GRID%DOMDESC,                                      &
#endif
     &              grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm              &
     &             ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop                 &
     &             ,grid%hbm2,grid%hbm3                                          &
     &             ,grid%q,grid%q2,grid%cwm,grid%u,grid%v,grid%z,grid%hydro                               &
     &             ,grid%n_iup_h,grid%n_iup_v                                    &
     &             ,grid%n_iup_adh,grid%n_iup_adv                                &
     &             ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                        &
     &             ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                    &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!***  UPDATE MOIST ARRAY.
!***  REMEMBER THAT MOIST IS ONLY USED WITH THE PHYSICS AND THUS
!***  THE P_QV SLOT (=2) IS MIXING RATIO, NOT SPECIFIC HUMIDITY.
!***  ALTHOUGH MOIST IS ONLY USED FOR PHYSICS IN OPERATIONS, IT IS 
!***  UPDATED HERE FROM grid%q EVERY ADVECTION TIMESTEP FOR NON-OPERATIONAL
!***  CONFIGURATIONS WHERE IT MAY BE USED OUTSIDE OF THE PHYSICS.
!
          IF(.NOT.OPERATIONAL_PHYSICS)THEN
            DO K=KTS,KTE
            DO J=MYJS,MYJE
            DO I=MYIS,MYIE
              MOIST(I,J,K,P_QV)=grid%q(I,J,K)/(1.-grid%q(I,J,K))
              WC = grid%cwm(I,J,K)
              QI = 0.
              QR = 0.
              QW = 0.
              FICE=grid%f_ice(I,K,J)
              FRAIN=grid%f_rain(I,K,J)
!
              IF(FICE>=1.)THEN
                QI=WC
              ELSEIF(FICE<=0.)THEN
                QW=WC
              ELSE
                QI=FICE*WC
                QW=WC-QI
              ENDIF
!
              IF(QW>0..AND.FRAIN>0.)THEN
                IF(FRAIN>=1.)THEN
                  QR=QW
                  QW=0.
                ELSE
                  QR=FRAIN*QW
                  QW=QW-QR
                ENDIF
              ENDIF
!
              MOIST(I,J,K,P_QC)=QW
              MOIST(I,J,K,P_QR)=QR

              IF (ETAMP_PHYSICS) THEN
#ifdef HWRF
                 MOIST(I,J,K,P_QI)=QI
                 MOIST(I,J,K,P_QS)=0.
#else
                 MOIST(I,J,K,P_QI)=0.
                 MOIST(I,J,K,P_QS)=QI
#endif
              endif
              MOIST(I,J,K,P_QG)=0.
            ENDDO
            ENDDO
            ENDDO
          ENDIF
!
!-----------------------------------------------------------------------
        ELSE had2_micro_check
!-----------------------------------------------------------------------
!
          CALL HAD2_SCAL(                                              &
#if defined(DM_PARALLEL)
     &                   GRID%DOMDESC,                                 &
#endif        
     &                   grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm         &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop            &
     &                  ,grid%hbm2,grid%hbm3                                     &
     &                  ,grid%q2,grid%u,grid%v,grid%z,grid%hydro                                &
     &                  ,grid%n_iup_h,grid%n_iup_v                               &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                           &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                   &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                               &
     &                  ,1,1                                           &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!        
          CALL HAD2_SCAL(                                              &
#if defined(DM_PARALLEL)
     &                   GRID%DOMDESC,                                 &
#endif
     &                   grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm         &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop            &
     &                  ,grid%hbm2,grid%hbm3                                     &
     &                  ,MOIST,grid%u,grid%v,grid%z,grid%hydro                             &
     &                  ,grid%n_iup_h,grid%n_iup_v                               &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                           &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                   &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                               &
     &                  ,NUM_MOIST,2                                   &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)
!
          CALL HAD2_SCAL(                                              &
#if defined(DM_PARALLEL)
     &                   GRID%DOMDESC,                                 &
#endif
     &                   grid%ntsd,GRID%DT,GRID%IDTAD,grid%dx_nmm,grid%dy_nmm         &
     &                  ,grid%aeta1,grid%aeta2,grid%deta1,grid%deta2,grid%pdsl,grid%pdtop            &
     &                  ,grid%hbm2,grid%hbm3                                     &
     &                  ,SCALAR,grid%u,grid%v,grid%z,grid%hydro                            &
     &                  ,grid%n_iup_h,grid%n_iup_v                               &
     &                  ,grid%n_iup_adh,grid%n_iup_adv                           &
     &                  ,grid%iup_h,grid%iup_v,grid%iup_adh,grid%iup_adv                   &
     &                  ,grid%ihe,grid%ihw,grid%ive,grid%ivw                               &
     &                  ,NUM_SCALAR,2                                  &
     &                  ,IDS,IDF,JDS,JDF,KDS,KDE                       &
     &                  ,IMS,IME,JMS,JME,KMS,KME                       &
     &                  ,ITS,ITE,JTS,JTE,KTS,KTE)                             
!    
          DO K=KTS,KTE 
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            grid%q(I,J,K)=MOIST(I,J,K,P_QV)/(1.+MOIST(I,J,K,P_QV))           
          ENDDO
          ENDDO    
          ENDDO   
!
!-----------------------------------------------------------------------
        ENDIF had2_micro_check
!-----------------------------------------------------------------------
        had2_tim=had2_tim+timef()-btimx
!-----------------------------------------------------------------------
!
      ENDIF idtad_block
!
!----------------------------------------------------------------------
!
      ENDIF not_euler  ! Lagrangian model tracer advection
!
!----------------------------------------------------------------------
!***  RADIATION
!----------------------------------------------------------------------
!
!***  When allocating CAM radiation 4d arrays (ozmixm, aerosolc), 
!***  the following two scalars are not needed.
!
      NUM_OZMIXM=1
      NUM_AEROSOLC=1
!
      IF(grid%ntsd<=0)THEN
        NTSD_rad=grid%ntsd
      ELSE
!
!***  Call radiation just BEFORE the top of the hour
!***  so that updated fields are written to history files.
!
        NTSD_rad=grid%ntsd+1
      ENDIF
!
#ifdef HWRF
!emc_2010_bugfix_h50
! remove this - not needed for V3.2
!      call nl_get_start_hour(1,IHRST)
!emc_2010_bugfix_h50
#endif

      IF(MOD(NTSD_rad,GRID%NRADS)==0.OR.                               &
     &   MOD(NTSD_rad,GRID%NRADL)==0)THEN
!
        btimx=timef()
        IF(OPERATIONAL_PHYSICS)THEN
          CALL UPDATE_MOIST(MOIST,grid%q,grid%cwm,grid%f_ice,grid%f_rain,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
!
        CALL RADIATION(NTSD_rad,GRID%DT,GRID%JULDAY,GRID%JULYR         &
     &                ,GRID%XTIME,GRID%JULIAN                          &
     &                ,IHRST,GRID%NPHS                                 &
     &                ,grid%glat,grid%glon,GRID%NRADS,GRID%NRADL                 &
     &                ,grid%deta1,grid%deta2,grid%aeta1,grid%aeta2,grid%eta1,grid%eta2,grid%pdtop,grid%pt      &
     &                ,grid%pd,grid%res,grid%pint,grid%t,grid%q,MOIST,grid%ths,grid%albedo,grid%epsr           &
     &                ,grid%f_ice,grid%f_rain                                    &
     &                ,grid%GD_CLOUD,grid%GD_CLOUD2                              &
     &                ,grid%sm,grid%hbm2,grid%cldfra,N_MOIST,RESTRT                   &
     &                ,grid%rlwtt,grid%rswtt,grid%rlwin,grid%rswin,grid%rswinc,grid%rswout           &
     &                ,grid%rlwtoa,grid%rswtoa,grid%czmean                            &
     &                ,grid%cfracl,grid%cfracm,grid%cfrach,grid%sigt4                      &
     &                ,grid%acfrst,grid%ncfrst,grid%acfrcv,grid%ncfrcv                     &
     &                ,grid%cuppt,grid%vegfrc,grid%sno,grid%htop,grid%hbot                      &
     &                ,grid%z,grid%sice,NUM_AEROSOLC,NUM_OZMIXM                  &
     &                ,GRID,CONFIG_FLAGS                               &
     &                ,RTHRATEN                                        &  
     &                ,grid%re_cloud,grid%re_ice,grid%re_snow          &  ! G. Thompson
     &                ,grid%has_reqc,grid%has_reqi,grid%has_reqs       &  ! G. Thompson
     &                ,grid%SWUPT,grid%SWUPTC,grid%SWDNT,grid%SWDNTC   &
     &                ,grid%SWUPB,grid%SWUPBC,grid%SWDNB,grid%SWDNBC   &
     &                ,grid%LWUPT,grid%LWUPTC,grid%LWDNT,grid%LWDNTC   &
     &                ,grid%LWUPB,grid%LWUPBC,grid%LWDNB,grid%LWDNBC   &
     &                ,grid%ACSWUPT,grid%ACSWUPTC,grid%ACSWDNT,grid%ACSWDNTC   &
     &                ,grid%ACSWUPB,grid%ACSWUPBC,grid%ACSWDNB,grid%ACSWDNBC   &
     &                ,grid%ACLWUPT,grid%ACLWUPTC,grid%ACLWDNT,grid%ACLWDNTC   &
     &                ,grid%ACLWUPB,grid%ACLWUPBC,grid%ACLWDNB,grid%ACLWDNBC   &
     &                 ,grid%swvisdir ,grid%swvisdif &  !ssib
     &                 ,grid%swnirdir ,grid%swnirdif &  !ssib
#ifdef WRF_CHEM
     &                ,PM2_5_DRY, PM2_5_WATER, PM2_5_DRY_EC            &
     &                ,TAUAER1, TAUAER2, TAUAER3, TAUAER4              & 
     &                ,GAER1, GAER2, GAER3, GAER4                      &
     &                ,WAER1, WAER2, WAER3, WAER4                      &
#endif
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                         &
     &                ,IMS,IME,JMS,JME,KMS,KME                         &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        DO J=JMS,JME
        DO I=IMS,IME
          grid%gsw(I,J)=grid%rswin(I,J)-grid%rswout(I,J)
        ENDDO
        ENDDO
!
!                            ***  NOTE  ***
! grid%rlwin/grid%rswin  - downward longwave/shortwave at the surface (formerly TOTLWDN/TOTSWDN)
! grid%rswinc - CLEAR-SKY downward shortwave at the surface (new for AQ)
!                            ***  NOTE  ***
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
!     Pass in XTIME (elapsed time from start of parent) to compute
!     the time passed into the zenith angle code consistently between
!     RDTEMP and RADIATION.

      CALL RDTEMP(grid%ntsd,GRID%DT,GRID%JULDAY,GRID%JULYR                  &
     &           ,GRID%XTIME,IHRST,grid%glat,grid%glon                           &
     &           ,grid%czen,grid%czmean,grid%t,grid%rswtt,grid%rlwtt,grid%hbm2                       &
     &           ,IDS,IDF,JDS,JDF,KDS,KDE                              &
     &           ,IMS,IME,JMS,JME,KMS,KME                              &
     &           ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      rdtemp_tim=rdtemp_tim+timef()-btimx
!
!
#ifdef HWRF
!
!-------------------------------------------------------------------------------------
!*** GET SSTs FROM DMITRY's COUPLER ON TO THE PARENT AND NESTED GRID
!-------------------------------------------------------------------------------------
! Coupling insertion:->
        CALL ATM_GETSST(grid%sst,grid%sm)
!<-:Coupling insertion
      IF(GRID%ID .EQ. 1 .AND. MOD(grid%ntsd,grid%NPHS)==0)THEN
        btimx=timef()
        sst_tim=sst_tim+timef()-btimx
      ENDIF

#endif
!----------------------------------------------------------------------
!***  TURBULENT PROCESSES 
!----------------------------------------------------------------------
!
      IF(MOD(grid%ntsd,GRID%NPHS)==0)THEN
!
        btimx=timef()
!
        IF(OPERATIONAL_PHYSICS                                         &
     &    .AND.MOD(NTSD_rad,GRID%NRADS)/=0                             &
     &    .AND.MOD(NTSD_rad,GRID%NRADL)/=0)THEN
          CALL UPDATE_MOIST(MOIST,grid%q,grid%cwm,grid%f_ice,grid%f_rain,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
        ENDIF
!
        CALL TURBL(grid%ntsd,GRID%DT,GRID%NPHS,RESTRT                       &
     &            ,N_MOIST,GRID%NUM_SOIL_LAYERS,grid%sldpth,grid%dzsoil          &
     &            ,grid%deta1,grid%deta2,grid%aeta1,grid%aeta2,grid%eta1,grid%eta2,grid%pdtop,grid%pt          &
     &            ,grid%sm,grid%hbm2,grid%vbm2,grid%dx_nmm,grid%dfrlg                           &
     &            ,grid%czen,grid%czmean,grid%sigt4,grid%rlwin,grid%rswin,grid%radot                 &
     &            ,grid%pd,grid%res,grid%pint,grid%t,grid%q,grid%cwm,grid%f_ice,grid%f_rain,grid%sr                 &
     &            ,grid%q2,grid%u,grid%v,grid%ths,grid%nmm_tsk,grid%sst,grid%prec,grid%sno                     &
     &            ,grid%fis,grid%z0,grid%mz0,grid%z0base,grid%ustar,grid%mixht,grid%pblh,grid%lpbl,grid%el_pbl    &   !KWON MZ0
     &            ,MOIST,grid%rmol,grid%mol                                      &
     &            ,grid%exch_h,grid%exch_m,grid%f,grid%akhs,grid%akms,grid%akhs_out,grid%akms_out         &
     &            ,grid%thz0,grid%qz0,grid%uz0,grid%vz0,grid%qsh,grid%mavail                         &
     &            ,grid%stc,grid%smc,grid%cmc,grid%smstav,grid%smstot,grid%ssroff,grid%bgroff             &
     &            ,grid%ivgtyp,grid%isltyp,grid%vegfrc,grid%shdmin,grid%shdmax,grid%grnflx           &
     &            ,grid%snotime                                             &
     &            ,grid%sfcexc,grid%acsnow,grid%acsnom,grid%snopcx,grid%sice,grid%tg,grid%soiltb          &
     &            ,grid%albsi,grid%icedepth,grid%snowsi                                                   &
     &            ,grid%albase,grid%mxsnal,grid%albedo,grid%sh2o,grid%si,grid%epsr,grid%embck             &
     &            ,grid%u10,grid%v10,grid%uoce,grid%voce,grid%th10,grid%q10,grid%tshltr,grid%qshltr,grid%pshltr               &
     &            ,grid%t2,grid%qsg,grid%qvg,grid%qcg,grid%soilt1,grid%tsnav,grid%smfr3d,grid%keepfr3dflag     &
#if (NMM_CORE==1)
     &            ,grid%twbs,grid%qwbs,grid%taux,grid%tauy,grid%sfcshx,grid%sfclhx,grid%sfcevp,RTHRATEN                      &
#else
     &            ,grid%twbs,grid%qwbs,grid%sfcshx,grid%sfclhx,grid%sfcevp                      &
#endif
     &            ,grid%potevp,grid%potflx,grid%subshx                                &
     &            ,grid%aphtim,grid%ardsw,grid%ardlw,grid%asrfc                            &
     &            ,grid%rswout,grid%rswtoa,grid%rlwtoa                                &
     &            ,grid%aswin,grid%aswout,grid%aswtoa,grid%alwin,grid%alwout,grid%alwtoa             &
#if (NMM_CORE==1)
     &            ,grid%uz0h,grid%vz0h,grid%dudt,grid%dvdt,grid%ugwdsfc,grid%vgwdsfc,grid%sfenth          & 
#else
     &            ,grid%uz0h,grid%vz0h,grid%dudt,grid%dvdt                                 & 
#endif
     &            ,RTHBLTEN,RQVBLTEN                                   & 
     &            ,GRID%PCPFLG,grid%ddata                                   &
     &            ,grid%hstdv,grid%hcnvx,grid%hasyw,grid%hasys,grid%hasysw,grid%hasynw,grid%hlenw,grid%hlens   & ! GWD
     &            ,grid%hlensw,grid%hlennw,grid%hangl,grid%hanis,grid%hslop,grid%hzmax,grid%crot,grid%srot     & ! GWD
     &            ,grid%dew                                            & ! RUC LSM
     &            ,grid%rc_mf                                          & ! QNSE
     &            ,GRID,CONFIG_FLAGS                                   &
     &            ,grid%ihe,grid%ihw,grid%ive,grid%ivw                 &
     &            ,GRID%DISHEAT,GRID%DKU3D,GRID%DKT3D                          &
     &            ,GRID%HPBL2D, GRID%EVAP2D, GRID%HEAT2D,GRID%RC2D               &  !S&P Kwon
     &            ,GRID%SFCHEADRT,GRID%INFXSRT,GRID%SOLDRAIN           &  !Hydrology, no-op right now
     &            ,IDS,IDF,JDS,JDF,KDS,KDE                             &
     &            ,IMS,IME,JMS,JME,KMS,KME                             &
     &            ,IPS,IPE,JPS,JPE,KPS,KPE                             &
     &            ,ITS,ITE,JTS,JTE,KTS,KTE)
!
!                     ***  NOTE  ***
! grid%rlwin/grid%rswin - downward longwave/shortwave at the surface
!                     ***  NOTE  ***
!
        turbl_tim=turbl_tim+timef()-btimx
#ifdef HWRF


!------------------------------------------------------------------------------
!*** ATMOSPHERIC MODEL OUTPUTS FROM PARENT AND NESTED GRID FOR DMITRYs COUPLER
!------------------------------------------------------------------------------
!
!-- grid%twbs  :  surface sensible heat flux, positive downward (grid%w/m2)
!-- grid%qwbs  :  surface latent heat flux, positive downward (grid%w/m2)
!-- grid%rlwin :  downward long wave flux at ground surface,positive downward (grid%w/m2)
!-- grid%rswin :  downward short wave flux at ground surface, positive downward (grid%w/m2)
!-- grid%radot :  outgoing long wave flux at ground surface, positive upward (grid%w/m2)
!-- grid%rswout:  outgoing short wave flux at ground surface, positive upward (grid%w/m2)
!-- grid%taux  :  x component of surface stress, grid%u positive Eastward
!-- grid%tauy  :  y component of surface stress, grid%v positive Northward
!-- grid%pint  :  3d array of interface pressure (pascals)
!-- grid%prec  :  grid%prec (m/timestep;timestep on grid1=60 sec)

!
!

! Coupling insertion:->
      call ATM_DOFLUXES(grid%twbs,grid%qwbs,grid%rlwin,grid%rswin,grid%radot,grid%rswout, &
      grid%taux,grid%tauy,grid%pint(:,:,1),grid%prec,grid%u10,grid%v10)
!<-:Coupling insertion
!

      IF(GRID%ID .EQ. 1 .AND. MOD(grid%ntsd,grid%NPHS)==0)THEN
        btimx=timef()
        flux_tim=flux_tim+timef()-btimx
      ENDIF

#endif
!
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
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
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
!***  INTERPOLATE WINDS FROM H POINTS BACK TO grid%v POINTS.
!
        btimx=timef()
        CALL UV_H_TO_V(grid%ntsd,GRID%DT,GRID%NPHS,grid%uz0h,grid%vz0h,grid%uz0,grid%vz0         &
     &                ,grid%dudt,grid%dvdt,grid%u,grid%v,grid%hbm2,grid%ive,grid%ivw                       &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        uv_htov_tim=uv_htov_tim+timef()-btimx
!
!----------------------------------------------------------------------
!*** STORE ORIGINAL TEMPERATURE ARRAY
!----------------------------------------------------------------------
!
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_J.inc"
#endif
!
#ifdef DM_PARALLEL
        IF (.NOT.ETAMP_PHYSICS) THEN
#    include "HALO_NMM_J_3.inc"
        ENDIF
#endif
!
#ifdef WRF_CHEM
#ifdef DM_PARALLEL
#    include "HALO_NMM_J_2.inc"
#endif
#endif
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max           &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
!
        ICLTEND=-1
        btimx=timef()
! 
        CALL CLTEND(ICLTEND,GRID%NPHS,grid%t,grid%t_old,grid%t_adj                    &
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
!
      IF(MOD(grid%ntsd,GRID%NCNVC)==0.AND.                              &
     &   (CONFIG_FLAGS%CU_PHYSICS.eq.KFETASCHEME .or.                     &
     &   CONFIG_FLAGS%CU_PHYSICS.eq.OSASSCHEME .or.                       &
     &   CONFIG_FLAGS%CU_PHYSICS.eq.NSASSCHEME .or.                       &
     &   CONFIG_FLAGS%CU_PHYSICS.eq.MESO_SAS .or.                         &   !Kwon
     &   CONFIG_FLAGS%CU_PHYSICS.eq.SASSCHEME))THEN                       ! 
!
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_C.inc"
#endif
!-----------------
        exch_tim=exch_tim+timef()-btimx
!       this_tim=timef()-btimx
!       call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max          &
!    &                    ,mpi_comm_comp,irtn)
!       exch_tim_max=exch_tim_max+et_max
     ENDIF
!
      convection: IF(CONFIG_FLAGS%CU_PHYSICS/=0)THEN
        btimx=timef()
!
!***  GET TENDENCIES FOR GD SCHEME.
!    
        IF(CONFIG_FLAGS%CU_PHYSICS==GDSCHEME.OR.                        &
     &  CONFIG_FLAGS%CU_PHYSICS==TIEDTKESCHEME.OR.                      &
     &  CONFIG_FLAGS%CU_PHYSICS==KFETASCHEME)THEN
          DT_INV=1./GRID%DT
          DO J=JMS,JME
          DO K=KMS,KME
          DO I=IMS,IME
            TTEN(I,K,J)=(grid%t(I,J,K)-TTEN(I,K,J))*DT_INV
            QTEN(I,K,J)=(grid%q(I,J,K)-QTEN(I,K,J))*DT_INV
          ENDDO
          ENDDO
          ENDDO
        ENDIF
!
!***  UPDATE THE MOIST ARRAY IF NEEDED.
!
        IF(OPERATIONAL_PHYSICS                                         &
     &    .AND.MOD(NTSD_rad,GRID%NRADS)/=0                             &
     &    .AND.MOD(NTSD_rad,GRID%NRADL)/=0                             &
     &    .AND.MOD(grid%ntsd,GRID%NPHS)/=0)THEN
          CALL UPDATE_MOIST(MOIST,grid%q,grid%cwm,grid%f_ice,grid%f_rain,N_MOIST           &
     &                     ,IDS,IDF,JDS,JDF,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE)
       ENDIF
!
!----------------------------------------------------------------------
       call wrf_message('call cucnvc')
       call start_timing
        CALL CUCNVC(grid%ntsd,GRID%DT,GRID%NCNVC,GRID%NRADS,GRID%NRADL      &
     &             ,GPS,RESTRT,grid%hydro,grid%cldefi,N_MOIST,GRID%ENSDIM        &
     &             ,MOIST                                              &
     &             ,grid%deta1,grid%deta2,grid%aeta1,grid%aeta2,grid%eta1,grid%eta2                  &
     &             ,grid%f_ice,grid%f_rain                                       &
!***  Changes for other cu schemes, most for GD scheme
     &             ,grid%apr_gr,grid%apr_w,grid%apr_mc,TTEN,QTEN                      &
     &             ,grid%apr_st,grid%apr_as,grid%apr_capma                            &
     &             ,grid%apr_capme,grid%apr_capmi                                &
     &             ,grid%mass_flux,grid%xf_ens                                   &
     &             ,grid%pr_ens,grid%gsw                                         &
     &             ,grid%GD_CLOUD,grid%GD_CLOUD2,grid%ktop_deep                  &
#ifdef WRF_CHEM
     &             ,RAINCV                          &
#endif
     &             ,grid%pdtop,grid%pt,grid%pd,grid%res,grid%pint,grid%t,grid%q,grid%cwm,grid%tcucn                 &
     &             ,grid%omgalf,grid%u,grid%v,grid%w,grid%z,grid%fis,grid%w0avg                           &
     &             ,grid%prec,grid%acprec,grid%cuprec,grid%cuppt,grid%cprate                    &
     &             ,grid%sm,grid%hbm2,grid%pblh,grid%lpbl,grid%cnvbot,grid%cnvtop                         &
     &             ,grid%htop,grid%hbot,grid%htopd,grid%hbotd,grid%htops,grid%hbots                  &
     &             ,RTHBLTEN,RQVBLTEN,RTHRATEN                         & 
#if (NMM_CORE==1)                  
     &             ,grid%twbs,grid%qwbs                                &
     &                 ,grid%DUCUDT, grid%DVCUDT, GRID%MOMMIX, grid%random             &
#endif
     &             ,grid%hpbl2d,grid%evap2d,grid%heat2d                &
     &             ,grid%avcnvc,grid%acutim,grid%ihe,grid%ihw          &
     &             ,GRID,CONFIG_FLAGS                                  &
     &             ,IDS,IDF,JDS,JDF,KDS,KDE                            &
     &             ,IMS,IME,JMS,JME,KMS,KME                            &
     &             ,IPS,IPE,JPS,JPE,KPS,KPE                            &
     &             ,ITS,ITE,JTS,JTE,KTS,KTE)
        call end_timing('cucnvc')
!----------------------------------------------------------------------
!
        cucnvc_tim=cucnvc_tim+timef()-btimx
!


#if (NMM_CORE==1)
!#ifdef HWRF
!-------------------------------------------------------------------------------------
!       This is gopal's doing for HWRFSAS

!        IF(MOD(grid%ntsd,GRID%NCNVC).eq.0.and.(CONFIG_FLAGS%CU_PHYSICS.eq.OSASSCHEME))THEN
! update to match HWRFV2 behaviour - review later (1/15/10)
!
!emc_2010_bugfix_h50
        IF(MOD(grid%ntsd, GRID%NCNVC).eq.0.and.                 &
     &    (CONFIG_FLAGS%CU_PHYSICS.eq.OSASSCHEME.or.            &
     &     CONFIG_FLAGS%CU_PHYSICS.eq.NSASSCHEME.or.            &
     &     CONFIG_FLAGS%CU_PHYSICS.eq.MESO_SAS.or.              & !Kwon
     &     CONFIG_FLAGS%CU_PHYSICS.eq.SASSCHEME))THEN 
!emc_2010_bugfix_h50
!
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
# include "HALO_NMM_SAS_A.inc"
#endif
!-----------------
#ifdef DM_PARALLEL
# include "HALO_NMM_SAS_B.inc"
#endif
!-----------------
        exch_tim=exch_tim+timef()-btimx

!
!***  INTERPOLATE WINDS FROM H POINTS BACK TO V POINTS AFTER SAS 
!
        btimx=timef()

!emc_2010_bugfix_h50
        CALL UV_H_TO_V(grid%NTSD,GRID%DT,GRID%NCNVC,grid%UZ0H,grid%VZ0H,grid%UZ0,grid%VZ0         &
     &                ,grid%DUCUDT,grid%DVCUDT,grid%U,grid%V,grid%HBM2,grid%IVE,grid%IVW                   &
     &                ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &                ,IMS,IME,JMS,JME,KMS,KME                          &
     &                ,ITS,ITE,JTS,JTE,KTS,KTE)
        uv_htov_tim=uv_htov_tim+timef()-btimx
!emc_2010_bugfix_h50

     ENDIF ! for SAS only 
!#endif
#endif
!--------------------------------------------------------------------------------
!
  ENDIF convection
!
!----------------------------------------------------------------------
!***  GRIDSCALE MICROPHYSICS (CONDENSATION & PRECIPITATION)
!----------------------------------------------------------------------
!
      IF(MOD(grid%ntsd,GRID%NPHS)==0)THEN
        btimx=timef()
!
        CALL GSMDRIVE(grid%ntsd,GRID%DT,GRID%NPHS,N_MOIST                   &
     &               ,grid%dx_nmm(ITS,JC),GRID%DY,grid%sm,grid%hbm2,grid%fis               &
     &               ,grid%deta1,grid%deta2,grid%aeta1,grid%aeta2,grid%eta1,grid%eta2                &
     &               ,grid%pdtop,grid%pt,grid%pd,grid%res,grid%pint,grid%t,grid%q,grid%cwm,grid%train               &
     &               ,MOIST,SCALAR,NUM_SCALAR                          &
     &               ,grid%f_ice,grid%f_rain,grid%f_rimef,grid%sr                          &
     &               ,grid%prec,grid%acprec,grid%avrain                               &
     &               ,grid%mp_restart_state                                 &
     &               ,grid%tbpvs_state                                      &
     &               ,grid%tbpvs0_state                                     &
     &               ,GRID,CONFIG_FLAGS                                &
     &               ,grid%re_cloud,grid%re_ice,grid%re_snow           &  ! G. Thompson
     &               ,grid%has_reqc,grid%has_reqi,grid%has_reqs        &  ! G. Thompson
     &               ,diag_flag                                        &  
     &               ,IDS,IDF,JDS,JDF,KDS,KDE                          &
     &               ,IMS,IME,JMS,JME,KMS,KME                          &
     &               ,ITS,ITE,JTS,JTE,KTS,KTE)
!
        gsmdrive_tim=gsmdrive_tim+timef()-btimx
!
!-----------------------------------------------------------------------
!---------PRECIPITATION ASSIMILATION------------------------------------
!-----------------------------------------------------------------------
!
        IF (GRID%PCPFLG) THEN
          btimx=timef()
!
          CALL CHKSNOW(grid%ntsd,GRID%DT,GRID%NPHS,grid%sr,PPTDAT                 &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
          CALL ADJPPT(grid%ntsd,GRID%DT,GRID%NPHS,grid%prec,grid%lspa,PPTDAT,grid%ddata     &
     &      ,IDS,IDE,JDS,JDE,KDS,KDE                                    &
     &      ,IMS,IME,JMS,JME,KMS,KME                                    &
     &      ,ITS,ITE,JTS,JTE,KTS,KTE)
!
          adjppt_tim=adjppt_tim+timef()-btimx
        ENDIF
!
!----------------------------------------------------------------------
!***  CALCULATE TEMP TENDENCIES AND RESTORE ORIGINAL TEMPS
!----------------------------------------------------------------------
!      
        ICLTEND=0
        btimx=timef()
!
        CALL CLTEND(ICLTEND,GRID%NPHS,grid%t,grid%t_old,grid%t_adj                    &
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
      CALL CLTEND(ICLTEND,GRID%NPHS,grid%t,grid%t_old,grid%t_adj                      &
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
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_K.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL HDIFF(grid%ntsd,GRID%DT,grid%fis,grid%dy_nmm,grid%hdac,grid%hdacv         &
     &          ,grid%hbm2,grid%deta1,GRID%SIGMA                       &
#ifdef HWRF
     &          ,grid%t,grid%q,grid%u,grid%v,grid%q2,grid%z,grid%w,grid%sm,grid%sice,grid%h_diff    &
#else
     &          ,grid%t,grid%q,grid%u,grid%v,grid%q2,grid%z,grid%w,grid%sm,grid%sice                &
#endif
     &          ,grid%def3d                                              &
     &          ,grid%ihe,grid%ihw,grid%ive,grid%ivw                   &
     &          ,CONFIG_FLAGS                                          &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      IF(.NOT.OPERATIONAL_PHYSICS)THEN
        DO K=KTS,KTE
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
!!!       MOIST(I,J,K,P_QV)=MAX(0.,grid%q(I,J,K)/(1.-grid%q(I,J,K)))
          MOIST(I,J,K,P_QV)=grid%q(I,J,K)/(1.-grid%q(I,J,K))           !<-- Update mixing ratio
        ENDDO
        ENDDO
        ENDDO
      ENDIF
!
      hdiff_tim=hdiff_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  UPDATING BOUNDARY VALUES AT HEIGHT POINTS
!----------------------------------------------------------------------
!
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_L.inc"
#endif
!
#ifdef DM_PARALLEL
#    include "HALO_NMM_L_3.inc"
#endif
!
#ifdef WRF_CHEM
#ifdef DM_PARALLEL
#    include "HALO_NMM_L_2.inc"
#endif
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL MASS_BOUNDARY(GRID%ID,grid%ntsd,GRID%DT,NEST,NUNIT_NBC,NBOCO,LAST_TIME,TSPH &
     &          ,LB,grid%eta1,grid%eta2,grid%pdtop,grid%pt,grid%res                              &
     &          ,grid%PD_BXS,grid%PD_BXE,grid%PD_BYS,grid%PD_BYE,grid%T_BXS,grid%T_BXE,grid%T_BYS,grid%T_BYE    &
     &          ,grid%Q_BXS,grid%Q_BXE,grid%Q_BYS,grid%Q_BYE,grid%U_BXS,grid%U_BXE,grid%U_BYS,grid%U_BYE,grid%V_BXS  &
     &          ,grid%V_BXE,grid%V_BYS,grid%V_BYE,grid%Q2_BXS,grid%Q2_BXE,grid%Q2_BYS,grid%Q2_BYE  &
     &          ,grid%PD_BTXS,grid%PD_BTXE,grid%PD_BTYS        &
     &          ,grid%PD_BTYE,grid%T_BTXS,grid%T_BTXE,grid%T_BTYS,grid%T_BTYE,grid%Q_BTXS,grid%Q_BTXE      &
     &          ,grid%Q_BTYS,grid%Q_BTYE,grid%U_BTXS,grid%U_BTXE,grid%U_BTYS,grid%U_BTYE,grid%V_BTXS       &
     &          ,grid%V_BTXE,grid%V_BTYS,grid%V_BTYE,grid%Q2_BTXS,grid%Q2_BTXE,grid%Q2_BTYS,grid%Q2_BTYE   &
     &          ,grid%pd,grid%t,grid%q,grid%q2,grid%pint &
#ifdef WRF_CHEM
     &          ,CHEM,NUMGAS,CONFIG_FLAGS                               &
#endif
     &          ,GRID%SPEC_BDY_WIDTH,grid%z                                  &
     &          ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                        &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
#if (NMM_NEST==1)
      if(ETAMP_PHYSICS) then
#endif
!         write(0,*) 'MP_BULK_BOUNDARY'
      CALL MP_BULK_BOUNDARY(GRID%ID,grid%ntsd,GRID%DT &
     &          ,LB,grid%eta1,grid%eta2,grid%pdtop,grid%pt              &
     &          ,grid%CWM_BXS,grid%CWM_BXE,grid%CWM_BYS,grid%CWM_BYE    &
     &          ,grid%CWM_BTXS,grid%CWM_BTXE,grid%CWM_BTYS,grid%CWM_BTYE&
     &          ,grid%q,grid%cwm                                        &
     &          ,MOIST,N_MOIST,SCALAR,NUM_SCALAR                        &
     &          ,GRID%SPEC_BDY_WIDTH                                    &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
#if (NMM_NEST==1)
      else
         CALL MP_SPECIES_BDY(grid%id,1,grid%dt,               &
              grid%CWM,grid%Q,                                &
              MOIST,N_MOIST,                                  &
              MOIST_bxs,MOIST_bxe,MOIST_bys,MOIST_bye,        &
              MOIST_btxs,MOIST_btxe,MOIST_btys,MOIST_btye,    &
              SCALAR,NUM_SCALAR,                              &
              SCALAR_bxs,SCALAR_bxe,SCALAR_bys,SCALAR_bye,    &
              SCALAR_btxs,SCALAR_btxe,SCALAR_btys,SCALAR_btye,&
              ids,idf,jds,jdf,kds,kde,                        &
              ims,ime,jms,jme,kms,kme,                        &
              its,ite,jts,jte,kts,kte)
      endif
#endif
!
      bocoh_tim=bocoh_tim+timef()-btimx
!     if(mod(grid%ntsd,n_print_time)==0)then
!       call twr(grid%t,0,'grid%t',grid%ntsd,mype,npes,mpi_comm_comp &
!    &          ,ids,ide,jds,jde,kds,kde                               &
!    &          ,ims,ime,jms,jme,kms,kme                               &
!    &          ,its,ite,jts,jte,kts,kte)
!     endif
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
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
!
#ifdef DM_PARALLEL
      IF (ETAMP_PHYSICS) THEN
#    include "HALO_NMM_A_3.inc"
      ENDIF
#endif
!
#ifdef WRF_CHEM
#ifdef DM_PARALLEL
#    include "HALO_NMM_A_2.inc"
#endif
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max             &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL PFDHT(grid%ntsd,LAST_TIME,grid%pt,grid%deta1,grid%deta2,grid%pdtop,grid%res,grid%fis           &
     &          ,grid%hydro,GRID%SIGMA,FIRST,grid%dx_nmm,grid%dy_nmm                  &
     &          ,grid%hbm2,grid%vbm2,grid%vbm3                                        &
     &          ,grid%fdiv,grid%fcp,grid%wpdar,grid%dfl,grid%cpgfu,grid%cpgfv                        &
     &          ,grid%pd,grid%pdsl,grid%t,grid%q,grid%u,grid%v,grid%cwm,grid%omgalf,grid%pint,grid%dwdt                  &
     &          ,grid%rtop,grid%div,grid%few,grid%fns,grid%fne,grid%fse                              &
     &          ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                       &
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
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_B.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max            &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL DDAMP(grid%ntsd,GRID%DT,grid%deta1,grid%deta2,grid%pdsl      &
     &          ,grid%pdtop,grid%div,grid%hbm2                          &
     &          ,grid%t,grid%u,grid%v,grid%ddmpu,grid%ddmpv             &
     &          ,grid%ihe,grid%ihw,grid%ive,grid%ivw                    &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                                &
     &          ,IMS,IME,JMS,JME,KMS,KME                                &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE)
!
      ddamp_tim=ddamp_tim+timef()-btimx
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
      IF(FIRST.AND.grid%ntsd==0)THEN
        FIRST=.FALSE.
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
        btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_A.inc"
#endif
#ifdef WRF_CHEM
#ifdef DM_PARALLEL
#    include "HALO_NMM_A_2.inc"
#endif
#endif
!-----------------
        exch_tim=exch_tim+timef()-btimx
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
#ifdef NMM_FIND_LOAD_IMBALANCE
      call blockf(loadimbal_tim)
#endif
      btimx=timef()
!-----------------
#ifdef DM_PARALLEL
#    include "HALO_NMM_C.inc"
#endif
!-----------------
      exch_tim=exch_tim+timef()-btimx
!     this_tim=timef()-btimx
!     call mpi_allreduce(this_tim,et_max,1,mpi_real,mpi_max            &
!    &                  ,mpi_comm_comp,irtn)
!     exch_tim_max=exch_tim_max+et_max
!
      btimx=timef()
!
      CALL BOCOV(GRID%ID,grid%ntsd,GRID%DT,LB,grid%U_BXS,grid%U_BXE,grid%U_BYS,grid%U_BYE,grid%V_BXS &
     &          ,grid%V_BXE,grid%V_BYS,grid%V_BYE,grid%U_BTXS,grid%U_BTXE,grid%U_BTYS,grid%U_BTYE,grid%V_BTXS  &
     &          ,grid%V_BTXE,grid%V_BTYS,grid%V_BTYE,grid%u,grid%v                              &
     &          ,GRID%SPEC_BDY_WIDTH                                   &
     &          ,grid%ihe,grid%ihw,grid%ive,grid%ivw                                       &
     &          ,IDS,IDF,JDS,JDF,KDS,KDE                               &
     &          ,IMS,IME,JMS,JME,KMS,KME                               &
     &          ,ITS,ITE,JTS,JTE,KTS,KTE )
 
!
      bocov_tim=bocov_tim+timef()-btimx
!
!----------------------------------------------------------------------
!***  COPY THE NMM VARIABLE grid%q2 TO THE WRF VARIABLE grid%tke_pbl
!----------------------------------------------------------------------
!
      DO K=KTS,KTE
      DO J=JTS,JTE
      DO I=ITS,ITE
        grid%tke_pbl(I,J,K)=0.5*grid%q2(I,J,K) !TKE is grid%q squared over 2
      ENDDO
      ENDDO
      ENDDO
! calculate some model diagnostics.

   CALL wrf_debug ( 200 , ' call diagnostic_driver' )

   CALL diagnostic_output_calc_refl(                                   &
      &              DIAGFLAG=diag_flag                                &
      &             ,REFD_MAX=grid%refd_max                            &
      &             ,refl_10cm=grid%refl_10cm                          &
      &             ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
      &             ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
      &             ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
      &                                                          )

!
!----------------------------------------------------------------------
!
      IF(LAST_TIME.AND.ALLOCATED(PPTDAT))THEN
        DEALLOCATE(PPTDAT,STAT=ISTAT)
      ENDIF
!
!----------------------------------------------------------------------
!
      solve_tim=solve_tim+timef()-btim
!
!----------------------------------------------------------------------
!***  PRINT TIMING VARIABLES WHEN DESIRED.
!----------------------------------------------------------------------
!
      sum_tim=pdte_tim+adve_tim+vtoa_tim+vadz_tim+hadz_tim+eps_tim     &
     &       +vad2_tim+had2_tim+radiation_tim+rdtemp_tim+turbl_tim     &
     &       +cltend_tim+cucnvc_tim+gsmdrive_tim+hdiff_tim             &
     &       +bocoh_tim+pfdht_tim+ddamp_tim+bocov_tim+uv_htov_tim      &
     &       +exch_tim+adjppt_tim
#if defined(NMM_FIND_LOAD_IMBALANCE)
      sum_tim=sum_tim + loadimbal_tim + previmbal_tim
#endif
!
      if(mod(grid%ntsd,n_print_time)==0)then
        write(message,*)' grid%ntsd=',grid%ntsd,' solve_tim=',solve_tim*1.e-3          &
     &           ,' sum_tim=',sum_tim*1.e-3
        call wrf_message(trim(message))
        write(message,*)' pdte_tim=',pdte_tim*1.e-3,' pct=',pdte_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' adve_tim=',adve_tim*1.e-3,' pct=',adve_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' vtoa_tim=',vtoa_tim*1.e-3,' pct=',vtoa_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' vadz_tim=',vadz_tim*1.e-3,' pct=',vadz_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' hadz_tim=',hadz_tim*1.e-3,' pct=',hadz_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' eps_tim=',eps_tim*1.e-3,' pct=',eps_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' vad2_tim=',vad2_tim*1.e-3,' pct=',vad2_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' had2_tim=',had2_tim*1.e-3,' pct=',had2_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' radiation_tim=',radiation_tim*1.e-3,' pct=',radiation_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' rdtemp_tim=',rdtemp_tim*1.e-3,' pct=',rdtemp_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' turbl_tim=',turbl_tim*1.e-3,' pct=',turbl_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' cltend_tim=',cltend_tim*1.e-3,' pct=',cltend_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' cucnvc_tim=',cucnvc_tim*1.e-3,' pct=',cucnvc_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' gsmdrive_tim=',gsmdrive_tim*1.e-3,' pct=',gsmdrive_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' adjppt_tim=',adjppt_tim*1.e-3,' pct=',adjppt_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' hdiff_tim=',hdiff_tim*1.e-3,' pct=',hdiff_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' bocoh_tim=',bocoh_tim*1.e-3,' pct=',bocoh_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' pfdht_tim=',pfdht_tim*1.e-3,' pct=',pfdht_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' ddamp_tim=',ddamp_tim*1.e-3,' pct=',ddamp_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' bocov_tim=',bocov_tim*1.e-3,' pct=',bocov_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' uv_h_to_v_tim=',uv_htov_tim*1.e-3,' pct=',uv_htov_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' exch_tim=',exch_tim*1.e-3,' pct=',exch_tim/sum_tim*100.
        call wrf_message(trim(message))
#ifdef NMM_FIND_LOAD_IMBALANCE
        write(message,*)' loadimbal_tim=',loadimbal_tim*1.e-3,' pct=',loadimbal_tim/sum_tim*100.
        call wrf_message(trim(message))
        write(message,*)' previmbal_tim=',previmbal_tim*1.e-3,' pct=',previmbal_tim/sum_tim*100.
        call wrf_message(trim(message))
#endif

!        call time_stats(exch_tim,'exchange',grid%ntsd,mype,npes,mpi_comm_comp)
!        write(message,*)' exch_tim_max=',exch_tim_max*1.e-3
!        call wrf_message(trim(message))
!
        call field_stats(grid%t,mype,mpi_comm_comp                          &
     &                  ,ids,ide,jds,jde,kds,kde                       &
     &                  ,ims,ime,jms,jme,kms,kme                       &
     &                  ,its,ite,jts,jte,kts,kte)
      endif
!
!     if(last_time)then
      DEALLOCATE(TTEN,STAT=ISTAT)
      DEALLOCATE(QTEN,STAT=ISTAT)
      DEALLOCATE(RTHRATEN,STAT=ISTAT)
      DEALLOCATE(RTHBLTEN,STAT=ISTAT)
      DEALLOCATE(RQVBLTEN,STAT=ISTAT)
#ifdef WRF_CHEM
#endif
!
! FOR VORTEX FOLLOWING MOVING NEST
!
!-----------------------------------------------------------------------------
!***  CRITERIA SET FOR GRID MOTION. This is gopal's doing
!-----------------------------------------------------------------------------
!
#ifdef MOVE_NESTS
   IF ( grid%num_moves.EQ.-99 ) THEN
      call start_timing()
      call stats_for_move(grid,config_flags &
                         ,IDS,IDE,JDS,JDE,KDS,KDE &
                         ,IMS,IME,JMS,JME,KMS,KME &
                         ,IPS,IPE,JPS,JPE,KPS,KPE &
                         ,ITS,ITE,JTS,JTE,KTS,KTE)
3303  format('stats_for_move on domain ',I0)
      write(message,3303) grid%id
      call end_timing(message)
   CALL wrf_debug ( 100 , 'nmm stats_for_move: after advection' )
   ENDIF
#endif
#ifdef HWRF
   hwrfx_mlsp: if(grid%vortex_tracker /= 1) then

!    output MSLP over parent domain for diagonostic purposes. outputs are hourly.
!    This is gopal's doing

     IF(grid%id .EQ. 1 .AND. MOD(grid%NTSD,n_print_time)==0)THEN
      call wrf_debug(1,'COMPUTING MSLP OVER THE PARENT DOMAIN')

      CALL MSLP_DIAG (grid%MSLP,grid%PINT,grid%T,grid%Q                &
                     ,grid%FIS,grid%PD,grid%DETA1,grid%DETA2,grid%PDTOP     &
                     ,IDS,IDF,JDS,JDF,KDS,KDE      &
                     ,IMS,IME,JMS,JME,KMS,KME      &
                     ,ITS,ITE,JTS,JTE,KTS,KTE      )


     ENDIF
   endif hwrfx_mlsp
#endif

!#define COPY_OUT
!#include <scalar_derefs.inc>
#ifdef HWRF
!-----------------------------------------------------------------------
!*** ACCUMULATED ATMOSPHERIC MODEL FLUXES FOR DMITRYs COUPLER
!-----------------------------------------------------------------------
!
!
!
! Coupling insertion:->
      call ATM_SENDFLUXES
!<-:Coupling insertion
!
! Kwon's doing to check heat flux
!
!  IF(grid%id==2)WRITE(0,*)'AFTER ATM_SENDFLUX grid%qwbs grid%twbs AT 10 10 ',grid%ntsd,grid%qwbs(10,10),grid%twbs(10,10)
!
#endif

!--------------------------------------------------------------------------------------------------------------
!
!      HIGH FREQUENCY OUTPUT    (STORM CENTER, MIN MSLP, MAX WINDS)
!          FOR NEST DOMAIN (9KM)        KWON 2011.4, TRAHAN 2011.6
!
!--------------------------------------------------------------------------------------------------------------
!
#ifdef HWRF
     IF(grid%hifreq_lun /= 0) THEN
        CALL HIFREQ_WRITE(grid%hifreq_lun,GRID%NTSD,GRID%DT,GRID%HLAT,GRID%HLON    &
                      ,GRID%U10,GRID%V10,grid%pint,grid%t,grid%q                   &
                      ,grid%fis,grid%pd,grid%pdtop,grid%deta1,grid%deta2           &
                      ,IDS,IDF,JDS,JDF,KDS,KDE                                     &
                      ,IMS,IME,JMS,JME,KMS,KME                                     &
                      ,ITS,ITE,JTS,JTE,KTS,KTE   )
     ENDIF
#endif
!
!-------------------  END OF HIGH FREQUENCY OUTPUT MODULE  ----------------------------
 

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
      SUBROUTINE TWR(ARRAY,KK,FIELD,ntsd,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
      IMPLICIT NONE
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INCLUDE "mpif.h"
#endif
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,ntsd
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME+KK),INTENT(IN) :: ARRAY
!
      CHARACTER(*),INTENT(IN) :: FIELD
!
!*** LOCAL VARIABLES
!
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
#endif
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
      IF(ntsd<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(ntsd<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(ntsd<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(ntsd<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(ntsd<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)ntsd
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
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
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
          VALUES(N)=ARRAY(I,J,K)
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
#else

        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==0)IENDX=IENDX-1
          WRITE(IUNIT)(ARRAY(I,K,J),I=1,IENDX)
        ENDDO

#endif

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
      SUBROUTINE VWR(ARRAY,KK,FIELD,ntsd,MYPE,NPES,MPI_COMM_COMP       &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                           &
     &              ,IMS,IME,JMS,JME,KMS,KME                           &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
      IMPLICIT NONE
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INCLUDE "mpif.h"
#endif
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE                    &
     &                     ,KK,MPI_COMM_COMP,MYPE,NPES,ntsd
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME+KK),INTENT(IN) :: ARRAY
!
      CHARACTER(*),INTENT(IN) :: FIELD
!
!*** LOCAL VARIABLES
!
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
#endif
      INTEGER,DIMENSION(2) :: IM_REM,JM_REM,IT_REM,JT_REM
!
      INTEGER :: I,IENDX,IER,IPE,IRECV,IRTN,ISEND,IUNIT                &
     &          ,J,K,L,N,NLEN,NSIZE
      INTEGER :: ITS_REM,ITE_REM,JTS_REM,JTE_REM
!
      REAL,DIMENSION(IDS:IDE,JDS:JDE) :: TWRITE
      REAL,ALLOCATABLE,DIMENSION(:) :: VALUES
      CHARACTER(5) :: TIMESTEP
      CHARACTER(6) :: FMT
      CHARACTER(12) :: FILENAME
      LOGICAL :: OPENED
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(ntsd<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(ntsd<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(ntsd<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(ntsd<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(ntsd<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)ntsd
      FILENAME=FIELD//'_'//TIMESTEP(1:NLEN)
!
      IF(MYPE==0)THEN
        CALL INT_GET_FRESH_HANDLE(IUNIT)
        CLOSE(IUNIT)
        OPEN(UNIT=IUNIT,FILE=FILENAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF
!     IF(MYPE==0)THEN
!       OPEN_UNIT: DO L=51,99
!         INQUIRE(L,OPENED=OPENED)
!         IF(.NOT.OPENED)THEN
!           IUNIT=L
!           OPEN(UNIT=IUNIT,FILE=FILENAME,STATUS='NEW'                 &
!               ,FORM='UNFORMATTED',IOSTAT=IER)
!           IF(IER/=0)THEN
!             WRITE(message,*)' Opening file error=',IER
!             CALL wrf_message(trim(message))
!           ENDIF
!           EXIT OPEN_UNIT
!         ENDIF
!       ENDDO OPEN_UNIT
!     ENDIF
!
!----------------------------------------------------------------------
!!!!  DO 500 K=KTS,KTE+KK     !Unflipped
!!!!  DO 500 K=KTE+KK,KTS,-1
      DO 500 K=KDE-1,KDS,-1   !Write LM layers top down for checking
!----------------------------------------------------------------------
!
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      IF(MYPE==0)THEN
        DO J=JTS,JTE
        DO I=ITS,ITE
          TWRITE(I,J)=ARRAY(I,J,K)
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
          VALUES(N)=ARRAY(I,J,K)
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
          IF(MOD(J,2)==1)IENDX=IENDX-1
          WRITE(IUNIT)(TWRITE(I,J),I=1,IENDX)
        ENDDO
!
      ENDIF
#else

        DO J=JDS,JDE-1
          IENDX=IDE-1
          IF(MOD(J,2)==0)IENDX=IENDX-1
          WRITE(IUNIT)(ARRAY(I,K,J),I=1,IENDX)
        ENDDO

#endif
!
!----------------------------------------------------------------------
  500 CONTINUE
!
      IF(MYPE==0)CLOSE(IUNIT)
!----------------------------------------------------------------------
!
      END SUBROUTINE VWR
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
      SUBROUTINE TIME_STATS(TIME_LCL,NAME,ntsd,MYPE,NPES,MPI_COMM_COMP)
!----------------------------------------------------------------------
!**********************************************************************
      USE MODULE_EXT_INTERNAL
!
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INCLUDE "mpif.h"
#endif
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE,NPES,ntsd
      REAL,INTENT(IN) :: TIME_LCL
!
      CHARACTER(*),INTENT(IN) :: NAME
!
!*** LOCAL VARIABLES
!
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INTEGER,DIMENSION(MPI_STATUS_SIZE) :: JSTAT
      INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: STATUS_ARRAY
#endif
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
      CHARACTER(LEN=256) :: message
!----------------------------------------------------------------------
!**********************************************************************
!----------------------------------------------------------------------
!
      IF(ntsd<=9)THEN
        FMT='(I1.1)'
        NLEN=1
      ELSEIF(ntsd<=99)THEN
        FMT='(I2.2)'
        NLEN=2
      ELSEIF(ntsd<=999)THEN
        FMT='(I3.3)'
        NLEN=3
      ELSEIF(ntsd<=9999)THEN
        FMT='(I4.4)'
        NLEN=4
      ELSEIF(ntsd<=99999)THEN
        FMT='(I5.5)'
        NLEN=5
      ENDIF
      WRITE(TIMESTEP,FMT)ntsd
      TITLE=NAME//'_'//TIMESTEP(1:NLEN)
!
!----------------------------------------------------------------------
!
#if defined(DM_PARALLEL) && !defined(STUBMPI)
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
        WRITE(message,100)TITLE
        CALL wrf_message(trim(message))
        WRITE(message,105)TIME_MAX,IPE_MAX
        CALL wrf_message(trim(message))
        WRITE(message,110)TIME_MIN,IPE_MIN
        CALL wrf_message(trim(message))
        WRITE(message,115)TIME_MEDIAN,IPE_MEDIAN
        CALL wrf_message(trim(message))
        WRITE(message,120)TIME_MEAN
        CALL wrf_message(trim(message))
  100   FORMAT(' Time for ',A)
  105   FORMAT(' Maximum=',G11.5,' for PE ',I2.2)
  110   FORMAT(' Minimum=',G11.5,' for PE ',I2.2)
  115   FORMAT(' Median =',G11.5,' for PE ',I2.2)
  120   FORMAT(' Mean   =',G11.5)
      ENDIF
!----------------------------------------------------------------------
!
#endif
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
!***  GENERATE STANDARD LAYER STATISTICS FOR THE DESIRED FIELD.
!***
!----------------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------------
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      INCLUDE "mpif.h"
#endif
!----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: MPI_COMM_COMP,MYPE
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE
!
      REAL,DIMENSION(IMS:IME,JMS:JME,KMS:KME),INTENT(IN) :: FIELD
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
 
      CHARACTER(LEN=256) :: message
!----------------------------------------------------------------------
!
      I_BY_J=(IDE-IDS)*(JDE-JDS)-(JDE-JDS-1)/2  ! This assumes that
                                                ! IDE and JDE are each
                                                ! one greater than the
                                                ! true grid size.
#if defined(DM_PARALLEL) && !defined(STUBMPI)
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
            FIKJ=FIELD(I,J,K)
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
          WRITE(message,101)KFLIP,FMAX_0(K),FMIN_0(K)
          CALL wrf_message(trim(message))
          WRITE(message,102)F_MEAN,ST_DEV,RMS
          CALL wrf_message(trim(message))
  101     FORMAT(' LAYER=',I2,' MAX=',E13.6,' MIN=',E13.6)
  102     FORMAT(9X,' MEAN=',E13.6,' STDEV=',E13.6,' RMS=',E13.6)
        ENDDO
      ENDIF
#endif
!----------------------------------------------------------------------
      END SUBROUTINE FIELD_STATS
!----------------------------------------------------------------------
      FUNCTION TIMEF()
      implicit none
      REAL*8 TIMEF
#if defined(OLD_TIMERS)
      INTEGER :: IC,IR
      CALL SYSTEM_CLOCK(COUNT=IC,COUNT_RATE=IR)
      TIMEF=REAL(IC)/REAL(IR)*1000.0
#else
      call hires_timer(timef)
      timef=timef*1000
#endif
      END FUNCTION TIMEF
#if defined(NMM_FIND_LOAD_IMBALANCE)
      SUBROUTINE BLOCKF(block_tim)
#if defined(DM_PARALLEL)
        use module_dm, only : local_communicator
        implicit none
        interface
           function timef()
             real*8 timef
           end function timef
        end interface
        integer :: ierr
        real, intent(inout) :: block_tim
        real*8 :: when
        when=timef()
        call mpi_barrier(local_communicator,ierr)
        block_tim=real(block_tim+(timef()-when))
#else
        return
#endif
      END SUBROUTINE BLOCKF
#endif
