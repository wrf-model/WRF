!#define NO_RESTRICT_ACCEL
!#define NO_GFDLETAINIT
!#define NO_UPSTREAM_ADVECTION
!----------------------------------------------------------------------
!
      SUBROUTINE START_DOMAIN_NMM(GRID, allowed_to_read                &
!
#include <dummy_args.inc>
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
      USE MODULE_GWD
!     USE MODULE_RA_GFDLETA
!
      USE MODULE_EXT_INTERNAL
!
#ifdef WRF_CHEM
   USE MODULE_AEROSOLS_SORGAM, ONLY: SUM_PM_SORGAM
   USE MODULE_MOSAIC_DRIVER, ONLY: SUM_PM_MOSAIC
#endif
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
#include <dummy_decl.inc>
!
      TYPE(GRID_CONFIG_REC_TYPE) :: CONFIG_FLAGS
!
#ifdef WRF_CHEM
   REAL        RGASUNIV ! universal gas constant [ J/mol-K ]
   PARAMETER ( RGASUNIV = 8.314510 )
#endif
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
      INTEGER :: I,IEND,IER,IFE,IFS,IHH,IHL,IHRSTB,II,IRTN          &
     &          ,ISIZ1,ISIZ2,ISTART,ISTAT,IX,J,J00,JFE,JFS,JHH,JJ       &
     &          ,JM1,JM2,JM3,JP1,JP2,JP3,JX,KK                          &
     &          ,K,K400,KBI,KBI2,KCCO2,KNT,KNTI                         &
     &          ,LB,LRECBC,L                                            &
     &          ,N,NMAP,NRADLH,NRADSH,NREC,NS,RECL,STAT                 &
     &          ,STEPBL,STEPCU,STEPRA
!
      INTEGER :: MY_E,MY_N,MY_S,MY_W                                    &
     &          ,MY_NE,MY_NW,MY_SE,MY_SW,MYI,MYJ,NPE
!
      INTEGER :: I_M
!
      INTEGER :: ILPAD2,IRPAD2,JBPAD2,JTPAD2
      INTEGER :: ITS,ITE,JTS,JTE,KTS,KTE
!
      INTEGER,DIMENSION(3) :: LPTOP
!
      REAL :: ADDL,APELM,APELMNW,APEM1,CAPA,CLOGES,DPLM,DZLM,EPS,ESE   &
     &       ,FAC1,FAC2,PDIF,PLM,PM1,PSFCK,PSS,PSUM,QLM,RANG           &
     &       ,SLPM,TERM1,THLM,TIME,TLM,TSFCK,ULM,VLM
!
!!!   REAL :: BLDT,CWML,EXNSFC,G_INV,PLYR,PSFC,ROG,SFCZ,THSIJ,TL
      REAL :: CWML,EXNSFC,G_INV,PLYR,PSURF,ROG,SFCZ,THSIJ,TL
      REAL :: TEND

!
!!!   REAL,ALLOCATABLE,DIMENSION(:,:) :: RAINBL,RAINNC,RAINNC           &
      INTEGER,ALLOCATABLE,DIMENSION(:,:) :: ITEMP,LOWLYR
      REAL,ALLOCATABLE,DIMENSION(:) :: SFULL,SMID
      REAL,ALLOCATABLE,DIMENSION(:) :: DZS,ZS
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: RQCBLTEN,RQIBLTEN            &
     &                                    ,RQVBLTEN,RTHBLTEN            &
     &                                    ,RUBLTEN,RVBLTEN              &
     &                                    ,RQCCUTEN,RQICUTEN,RQRCUTEN   &
     &                                    ,RQSCUTEN,RQVCUTEN,RTHCUTEN   &
     &                                    ,RTHRATEN                     &
     &                                    ,RTHRATENLW,RTHRATENSW
      REAL,ALLOCATABLE,DIMENSION(:,:) :: EMISS,EMTEMP,GLW,HFX           &
     &                                  ,NCA                            &
     &                                  ,QFX,RAINBL,RAINC,RAINNC        &
     &                                  ,RAINNCV                        &
     &                                  ,SNOWC,THC,TMN,TSFC

      REAL,ALLOCATABLE,DIMENSION(:,:) :: Z0_DUM, ALBEDO_DUM
!
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: ZINT,RRI,CONVFAC,ZMID
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: T_TRANS,PINT_TRANS
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: CLDFRA_TRANS
#ifndef WRF_CHEM
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: CLDFRA_OLD
#endif
#if 0
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: W0AVG
#endif
      LOGICAL :: E_BDY,N_BDY,S_BDY,W_BDY,WARM_RAIN,ADV_MOIST_COND
      LOGICAL :: START_OF_SIMULATION
      LOGICAL :: LRESTART


      integer :: jam,retval
      CHARACTER(LEN=255) :: message
      integer myproc
      real :: dsig,dsigsum,pdbot,pdtot,rpdtot
      real :: fisx,ht,prodx,rg
      integer :: i_t=096,j_t=195,n_t=11
      integer :: i_u=49,j_u=475,n_u=07
      integer :: i_v=49,j_v=475,n_v=07
      integer :: num_ozmixm, num_aerosolc
      real :: cen_lat,cen_lon,dtphs   ! GWD
      integer :: num_urban_layers
!Rogers GMT
      INTEGER :: hr, mn, sec, ms, rc
      TYPE(WRFU_Time) :: currentTime

      INTEGER :: interval_seconds, restart_interval

! z0base new
 
      REAL,DIMENSION(0:30) :: VZ0TBL_24
      VZ0TBL_24= (/0.,                                                 &
     &            1.00,  0.07,  0.07,  0.07,  0.07,  0.15,             &
     &            0.08,  0.03,  0.05,  0.86,  0.80,  0.85,             &
     &            2.65,  1.09,  0.80,  0.001, 0.04,  0.05,             &
     &            0.01,  0.04,  0.06,  0.05,  0.03,  0.001,            &
     &            0.000, 0.000, 0.000, 0.000, 0.000, 0.000/)
 
! end z0base new
!
!----------------------------------------------------------------------
#define COPY_IN
#include <scalar_derefs.inc>
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

#if 1
      IF(IME>NMM_MAX_DIM )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm ime (',ime,') > ',NMM_MAX_DIM,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL WRF_ERROR_FATAL(wrf_err_message)
      ENDIF
!
      IF(JME>NMM_MAX_DIM )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm jme (',jme,') > ',NMM_MAX_DIM,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL WRF_ERROR_FATAL(wrf_err_message)
      ENDIF
#else
      IF(IMS>-2.OR.IME>NMM_MAX_DIM )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm ims(',ims,' > -2 or ime (',ime,') > ',NMM_MAX_DIM,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL WRF_ERROR_FATAL(wrf_err_message)
      ENDIF
!
      IF(JMS>-2.OR.JME>NMM_MAX_DIM )THEN
        WRITE(wrf_err_message,*)                                       &
         'start_domain_nmm jms(',jms,' > -2 or jme (',jme,') > ',NMM_MAX_DIM,    &
         '. Increase NMM_MAX_DIM in configure.wrf, clean, and recompile.'
        CALL WRF_ERROR_FATAL(wrf_err_message)
      ENDIF
#endif
!
!---------------------------------------------------------------------- 
!
      WRITE(message,196)IHRST,IDAT
      CALL wrf_message(trim(message))
  196 FORMAT(' FORECAST BEGINS ',I2,' GMT ',2(I2,'/'),I4)


!!    Restarts must be made from times for which boundary data is available

      CALL nl_get_interval_seconds(GRID%ID, interval_seconds)
      CALL nl_get_restart_interval(GRID%ID, restart_interval)
      IF (MOD(restart_interval*60,interval_seconds) /= 0) THEN
         WRITE(wrf_err_message,*)' restart_interval is not integer multiplier of interval_seconds'
         CALL WRF_ERROR_FATAL(wrf_err_message)
      END IF

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

      CALL WRF_GET_MYPROC(MYPROC)
      MYPE=MYPROC

!
!----------------------------------------------------------------------
!***  Let each task determine who its eight neighbors are because we
!***  will need to know that for the halo exchanges.  The direction
!***  to each neighbor will be designated by the following integers:
!
!***      north: 1
!***       east: 2
!***      south: 3
!***       west: 4
!***  northeast: 5
!***  southeast: 6
!***  southwest: 7
!***  northwest: 8
!
!***  If a task has no neighbor in a particular direction because of
!***  the presence of the global domain boundary then that element
!***  of my_neb is set to -1.
!-----------------------------------------------------------------------
!
      call wrf_get_nprocx(inpes)
      call wrf_get_nprocy(jnpes)
!
      allocate(itemp(inpes,jnpes),stat=istat)
      npe=0
!
      do j=1,jnpes
      do i=1,inpes
        itemp(i,j)=npe
        if(npe==mype)then
          myi=i
          myj=j
        endif
        npe=npe+1
      enddo
      enddo
!
      my_n=-1
      if(myj+1<=jnpes)my_n=itemp(myi,myj+1)
!
      my_e=-1
      if(myi+1<=inpes)my_e=itemp(myi+1,myj)
!
      my_s=-1
      if(myj-1>=1)my_s=itemp(myi,myj-1)
!
      my_w=-1
      if(myi-1>=1)my_w=itemp(myi-1,myj)
!
      my_ne=-1
      if((myi+1<=inpes).and.(myj+1<=jnpes)) &
         my_ne=itemp(myi+1,myj+1)
!
      my_se=-1
      if((myi+1<=inpes).and.(myj-1>=1)) &
         my_se=itemp(myi+1,myj-1)
!
      my_sw=-1
      if((myi-1>=1).and.(myj-1>=1)) &
         my_sw=itemp(myi-1,myj-1)
!
      my_nw=-1
      if((myi-1>=1).and.(myj+1<=jnpes)) &
         my_nw=itemp(myi-1,myj+1)
!
!     my_neb(1)=my_n
!     my_neb(2)=my_e
!     my_neb(3)=my_s
!     my_neb(4)=my_w
!     my_neb(5)=my_ne
!     my_neb(6)=my_se
!     my_neb(7)=my_sw
!     my_neb(8)=my_nw
!
      deallocate(itemp)
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
!
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
        DO J=JFS,JFE
        DO I=IFS,IFE
          PDSL(I,J)  =PD(I,J)*RES(I,J)
          PREC(I,J)  =0.
          IF(allowed_to_read)ACPREC(I,J)=0.  ! This is gopal's inclusion for moving nest
          CUPREC(I,J)=0.
          rg=1./g
          ht=fis(i,j)*rg
!!!       fisx=ht*g
!          fisx=max(fis(i,j),0.)
!          prodx=Z0(I,J)*Z0MAX
!          Z0(I,J)    =SM(I,J)*Z0SEA+(1.-SM(I,J))*                      &
!     &                (Z0(I,J)*Z0MAX+FISx    *FCM+Z0LAND)
!!!  &                (prodx        +FISx    *FCM+Z0LAND)
          QSH(I,J)   =0.
          AKMS(I,J)  =0.
          AKHS(I,J)  =0.
          TWBS(I,J)  =0.
          QWBS(I,J)  =0.
          IF(allowed_to_read)THEN       ! This is gopal's inclusion for moving nest
          CLDEFI(I,J)=1.
          HTOP(I,J)  =REAL(KTS)
          HTOPD(I,J) =REAL(KTS)
          HTOPS(I,J) =REAL(KTS)
          HBOT(I,J)  =REAL(KTE)
          HBOTD(I,J) =REAL(KTE)
          HBOTS(I,J) =REAL(KTE)
          ENDIF
!***
!***  AT THIS POINT, WE MUST CALCULATE THE INITIAL POTENTIAL TEMPERATURE
!***  OF THE SURFACE AND OF THE SUBGROUND.
!***  EXTRAPOLATE DOWN FOR INITIAL SURFACE POTENTIAL TEMPERATURE.
!***  ALSO DO THE SHELTER PRESSURE.
!***
!
!***  BECAUSE WE REINITIALIZE TOPOGRAPHY, LAND SEA MASK AND FIND THE TEMPERATURE
!***  FIELD OVER THE NEW TOPOGRAPHY, AFTER THE MOVE, I THINK IT MORE APPROPRIATE
!***  TO USE NMM_TSK OR SST TO RE-DERIVE THS AND QS (AND CONSEQUENTLY THZ0 AND QZ0).
!***  THIS MAY BE MORE CONSISTENT WITH THE PSEUDO-HYDROSTATIC BALANCING THAT IS
!***  DONE OVER THE NEW TERRAIN (AND WITH NEW SM). gopal!
!***
!***

      IF(allowed_to_read)THEN       ! This is gopal's inclusion for moving nest

          PM1=AETA1(KTS)*PDTOP+AETA2(KTS)*PDSL(I,J)+PT
          APEM1=(1.E5/PM1)**CAPA

        IF(NMM_TSK(I,J)>=200.)THEN         ! have a specific skin temp, use it
          THS(I,J)=NMM_TSK(I,J)*APEM1
          TSFCK=NMM_TSK(I,J)
	ELSE                               ! use lowest layer as a proxy
          THS(I,J)=T(I,J,KTS)*APEM1
          TSFCK=T(I,J,KTS)
	ENDIF

          PSFCK=PD(I,J)+PDTOP+PT
!
          IF(SM(I,J)<0.5) THEN
            QSH(I,J)=PQ0/PSFCK*EXP(A2*(TSFCK-A3)/(TSFCK-A4))
          ELSEIF(SM(I,J)>0.5) THEN
            THS(I,J)=SST(I,J)*(1.E5/(PD(I,J)+PDTOP+PT))**CAPA
          ENDIF
!
          TERM1=-0.068283/T(I,J,KTS)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)
!
          USTAR(I,J)=0.1
          THZ0(I,J)=THS(I,J)
          QZ0(I,J)=QSH(I,J)
          UZ0(I,J)=0.
          VZ0(I,J)=0.

      ENDIF  ! endif for allowed to read
! 
        ENDDO
        ENDDO

!***
!***  INITIALIZE CLOUD FIELDS
!***
      IF (MAXVAL(CWM) .gt. 0. .and. MAXVAL(CWM) .lt. 1.) then
        CALL wrf_message('appear to have CWM values...do not zero')
      ELSE
        IF(allowed_to_read)THEN       ! This is gopal's inclusion for moving nest
        CALL wrf_message('zeroing CWM')
        DO K=KPS,KPE
          DO J=JFS,JFE
          DO I=IFS,IFE
            CWM(I,J,K)=0.
          ENDDO
          ENDDO
        ENDDO
        ENDIF
      ENDIF
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
      IF(allowed_to_read)THEN       ! This is gopal's inclusion for moving nest
        DO J=JFS,JFE
        DO I=IFS,IFE
          IF(SM(I,J)>0.5)THEN
            CLOGES =-CM1/SST(I,J)-CM2*ALOG10(SST(I,J))+CM3
            ESE    = 10.**(CLOGES+2.)
            QSH(I,J)= SM(I,J)*EPS*ESE/(PD(I,J)+PDTOP+PT-ESE*(1.-EPS))
          ENDIF
        ENDDO
        ENDDO
      ENDIF
!***  
!***  INITIALIZE TURBULENT KINETIC ENERGY (TKE) TO A SMALL
!***  VALUE (EPSQ2) ABOVE GROUND.  SET TKE TO ZERO IN THE
!***  THE LOWEST MODEL LAYER.  IN THE LOWEST TWO ATMOSPHERIC
!***  ETA LAYERS SET TKE TO A SMALL VALUE (Q2INI).
!***
!***EROGERS: add check for realistic values of q2
!
      IF (MAXVAL(Q2) .gt. epsq2 .and. MAXVAL(Q2) .lt. 200.) then
        CALL wrf_message('appear to have Q2 values...do not zero')
      ELSE
      IF(allowed_to_read)THEN       ! This is gopal's inclusion for moving nest
        CALL wrf_message('zeroing Q2')
        DO K=KPS,KPE-1
        DO J=JFS,JFE
        DO I=IFS,IFE
          Q2(I,J,K)=HBM2(I,J)*EPSQ2
        ENDDO
        ENDDO
        ENDDO
!
        DO J=JFS,JFE
        DO I=IFS,IFE
          Q2(I,J,LM)    = 0.
          Q2(I,J,KTE-2)= HBM2(I,J)*Q2INI
          Q2(I,J,KTE-1)= HBM2(I,J)*Q2INI
        ENDDO
        ENDDO
      ENDIF
      ENDIF
!***  
!***  PAD ABOVE GROUND SPECIFIC HUMIDITY IF IT IS TOO SMALL.
!***  INITIALIZE LATENT HEATING ACCUMULATION ARRAYS.
!***
        DO K=KPS,KPE
        DO J=JFS,JFE
        DO I=IFS,IFE
          IF(Q(I,J,K)<EPSQ)Q(I,J,K)=EPSQ
          TRAIN(I,J,K)=0.
          TCUCN(I,J,K)=0.
        ENDDO
        ENDDO
        ENDDO
!
!***
!***  INITIALIZE MAX/MIN TEMPERATURES.
!***
        DO J=JFS,JFE
        DO I=IFS,IFE
          TLMAX(I,J)=T(I,J,KPS)
          TLMIN(I,J)=T(I,J,KPS)
        ENDDO
        ENDDO
!
!----------------------------------------------------------------------
!***  END OF SCRATCH START INITIALIZATION BLOCK.
!----------------------------------------------------------------------
!
        CALL wrf_message('INIT:  INITIALIZED ARRAYS FOR CLEAN START')
      ENDIF ! <--- (not restart)

      IF(NEST)THEN
        DO J=JFS,JFE
        DO I=IFS,IFE
!
          IF(T(I,J,KTS)==0.)THEN
            T(I,J,KTS)=T(I,J,KTS+1)
          ENDIF
!
          TERM1=-0.068283/T(I,J,KTS)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)
        ENDDO
        ENDDO
      ENDIF
!
!----------------------------------------------------------------------
!***  RESTART INITIALIZING.  CHECK TO SEE IF WE NEED TO ZERO
!***  ACCUMULATION ARRAYS.
!----------------------------------------------------------------------

      TSPH=3600./GRID%DT ! needed?
      NPHS0=GRID%NPHS

      IF(MYPE==0)THEN
        WRITE( wrf_err_message, * )' start_nmm TSTART=',grid%tstart
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm TPREC=',grid%tprec
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm THEAT=',grid%theat
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm TCLOD=',grid%tclod
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm TRDSW=',grid%trdsw
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm TRDLW=',grid%trdlw
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm TSRFC=',grid%tsrfc
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
        WRITE( wrf_err_message, * )' start_nmm PCPFLG=',grid%pcpflg
        CALL wrf_debug( 1, TRIM(wrf_err_message) )
      ENDIF

      NSTART = INT(grid%TSTART*TSPH+0.5)
!
      NTSD = NSTART


!! want non-zero values for NPREC, NHEAT type vars to avoid problems
!! with mod statements below.

      NPREC  = INT(grid%TPREC *TSPH+0.5)
      NHEAT  = INT(grid%THEAT *TSPH+0.5)
      NCLOD  = INT(grid%TCLOD *TSPH+0.5)
      NRDSW  = INT(grid%TRDSW *TSPH+0.5)
      NRDLW  = INT(grid%TRDLW *TSPH+0.5)
      NSRFC  = INT(grid%TSRFC *TSPH+0.5)
!
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
      DO I=IFS,IFE
        ADT(I,J)=0.
        ADU(I,J)=0.
        ADV(I,J)=0.
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
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
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
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
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
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
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
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            IEND=IM-1-MOD(JJ+1,2)
            ISTART=IEND-MOD(JJ,2)
            KNTI=0
            IF(INPES==1)KNTI=N_IUP_ADH(J)
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
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            IEND=IM-MOD(JJ,2)
            ISTART=IEND-3
            KNTI=0
            IF(INPES==1)KNTI=N_IUP_V(J)
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
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
            J=JJ      ! -MY_JS_GLB+1
            IEND=IM-1-MOD(JJ,2)
            ISTART=IEND-MOD(JJ+1,2)
            KNTI=0
            IF(INPES==1)KNTI=N_IUP_ADV(J)
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
      jam=6+2*(JDE-JDS-1-9)
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
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
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
          IF(JJ>=MY_JS_GLB-2.AND.JJ<=MY_JE_GLB+2)THEN
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
         GRID%NSOIL= GRID%NUM_SOIL_LAYERS
        DO J=JFS,JFE
        DO I=IFS,IFE
          PCTSNO(I,J)=-999.0
          IF(SM(I,J)<0.5)THEN
              CMC(I,J)=0.0
!              CMC(I,J)=canwat(i,j)   ! tgs
            IF(SICE(I,J)>0.5)THEN
!***
!***  SEA-ICE CASE
!***
              SMSTAV(I,J)=1.0
              SMSTOT(I,J)=1.0
              SSROFF(I,J)=0.0
              BGROFF(I,J)=0.0
              CMC(I,J)=0.0
              DO NS=1,GRID%NSOIL
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
            DO NS=1,GRID%NSOIL
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
        LVL(I,J)=LM-KTE
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
        IF(LPTOP(3)==0)THEN
          IF(PSUM>PHITP)LPTOP(3)=K
        ELSEIF(LPTOP(2)==0)THEN
          IF(PSUM>PMDHI)LPTOP(2)=K
        ELSEIF(K400==0)THEN
          IF(PSUM>P400)K400=K
        ELSEIF(LPTOP(1)==0)THEN
          IF(PSUM>PLOMD)LPTOP(1)=K
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
!!!   IF(MYPE==0)CALL SOLARD(SUN_DIST)
!!!   CALL MPI_BCAST(SUN_DIST,1,MPI_REAL,0,MPI_COMM_COMP,IRTN)

!***
!***  CALL ZENITH SIMPLY TO GET THE DAY OF THE YEAR FOR
!***  THE SETUP OF THE OZONE DATA
!***
      TIME=(NTSD-1)*GRID%DT
!
!!!   CALL ZENITH(TIME,DAYI,HOUR)
!
      ADDL=0.
      IF(MOD(IDAT(3),4)==0)ADDL=1.
!
!!!   CALL O3CLIM
!
!
      DEALLOCATE(PHALF)
!----------------------------------------------------------------------
!***  SOME INITIAL VALUES RELATED TO TURBULENCE SCHEME
!----------------------------------------------------------------------
!
      IF(allowed_to_read.and.(.NOT.RESTRT))THEN       ! This is gopal's inclusion for moving nest

      DO J=JFS,JFE
      DO I=IFS,IFE
!***
!***  TRY A SIMPLE LINEAR INTERP TO GET 2/10 M VALUES
!***
        PDSL(I,J)=PD(I,J)*RES(I,J)
!
        ULM=U(I,J,KTS)
        VLM=V(I,J,KTS)
        TLM=T(I,J,KTS)
        QLM=Q(I,J,KTS)
        PLM=AETA1(KTS)*PDTOP+AETA2(KTS)*PDSL(I,J)+PT
        APELM=(1.0E5/PLM)**CAPA
          TERM1=-0.068283/T(I,J,KTS)
          PSHLTR(I,J)=(PD(I,J)+PDTOP+PT)*EXP(TERM1)
        APELMNW=(1.0E5/PSHLTR(I,J))**CAPA
        THLM=TLM*APELM
        DPLM=(DETA1(KTS)*PDTOP+DETA2(KTS)*PDSL(I,J))*0.5
        DZLM=R_D*DPLM*TLM*(1.+P608*QLM)/(G*PLM)
        FAC1=10./DZLM
        FAC2=(DZLM-10.)/DZLM
        IF(DZLM<=10.)THEN
          FAC1=1.
          FAC2=0.
        ENDIF
!
        IF(.NOT.RESTRT)THEN
          TH10(I,J)=FAC2*THS(I,J)+FAC1*THLM
          Q10(I,J)=FAC2*QSH(I,J)+FAC1*QLM
          U10(I,J)=ULM
          V10(I,J)=VLM
        ENDIF
!
!        FAC1=2./DZLM
!        FAC2=(DZLM-2.)/DZLM
!        IF(DZLM.LE.2.)THEN
!          FAC1=1.
!          FAC2=0.
!        ENDIF
!
        IF(.NOT.RESTRT.OR.NEST)THEN
!
          IF ( (THLM-THS(I,J))>2.0) THEN  ! weight differently in different scenarios
            FAC1=0.3
            FAC2=0.7
          ELSE
            FAC1=0.8
            FAC2=0.2
          ENDIF

          TSHLTR(I,J)=FAC2*THS(I,J)+FAC1*THLM
!          TSHLTR(I,J)=0.2*THS(I,J)+0.8*THLM
          QSHLTR(I,J)=FAC2*QSH(I,J)+FAC1*QLM
!          QSHLTR(I,J)=0.2*QSH(I,J)+0.8*QLM
        ENDIF
!***
!***  NEED TO CONVERT TO THETA IF IS THE RESTART CASE
!***  AS CHKOUT.f WILL CONVERT TO TEMPERATURE
!***
!EROGERS: COMMENT OUT IN WRF-NMM
!***
!       IF(RESTRT)THEN
!         TSHLTR(I,J)=TSHLTR(I,J)*APELMNW
!       ENDIF
      ENDDO
      ENDDO

      END IF ! IF(allowed_to_read)THEN
!
!----------------------------------------------------------------------
!***  INITIALIZE TAU-1 VALUES FOR ADAMS-BASHFORTH 
!----------------------------------------------------------------------
!
      IF(.NOT.RESTRT)THEN
        DO K=KPS,KPE
          DO J=JFS,JFE
          DO I=ifs,ife
          TOLD(I,J,K)=T(I,J,K)   ! T AT TAU-1
          UOLD(I,J,K)=U(I,J,K)   ! U AT TAU-1
          VOLD(I,J,K)=V(I,J,K)   ! V AT TAU-1
          ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!----------------------------------------------------------------------
!***  INITIALIZE NONHYDROSTATIC QUANTITIES
!----------------------------------------------------------------------
!
!!!!	SHOULD DWDT BE REDEFINED IF RESTRT?

      IF((.NOT.RESTRT.OR.NEST).AND. allowed_to_read)THEN ! This is gopal's inclusion for moving nest
        DO K=KPS,KPE
          DO J=JFS,JFE
          DO I=IFS,IFE
            DWDT(I,J,K)=1.
          ENDDO
          ENDDO
        ENDDO
      ENDIF
!***
      IF(GRID%SIGMA==1)THEN
        DO J=JFS,JFE
        DO I=IFS,IFE
          PDSL(I,J)=PD(I,J)
        ENDDO
        ENDDO
      ELSE
        DO J=JFS,JFE
        DO I=IFS,IFE
          PDSL(I,J)=RES(I,J)*PD(I,J)
        ENDDO
        ENDDO
      ENDIF
!
!***
!
!
!!!!	SHOULD PINT,Z,W BE REDEFINED IF RESTRT?

      WRITE( wrf_err_message, * )' restrt=',restrt,' nest=',nest
        CALL wrf_debug( 0, TRIM(wrf_err_message) )
      WRITE( wrf_err_message, * )' pdtop=',pdtop,' pt=',pt
        CALL wrf_debug( 0, TRIM(wrf_err_message) )

      IF(.NOT.RESTRT.OR.NEST)THEN
        DO K=KPS,KPE
        DO J=JFS,JFE
        DO I=IFS,IFE
          PINT(I,J,K)=ETA1(K)*PDTOP+ETA2(K)*PDSL(I,J)+PT
          Z(I,J,K)=PINT(I,J,K)
          W(I,J,K)=0.
        ENDDO
        ENDDO
        ENDDO
      ENDIF

        DO K=KTS,KTE-1
        DO J=JFS,JFE
        DO I=IFS,IFE
          RTOP(I,J,K)=(Q(I,J,K)*P608-CWM(I,J,K)+1.)*T(I,J,K)*R_D/ &
                      ((PINT(I,J,K+1)+PINT(I,J,K))*0.5)
        ENDDO
        ENDDO
        ENDDO

#ifndef NO_RESTRICT_ACCEL
!----------------------------------------------------------------------
!***  RESTRICTING THE ACCELERATION ALONG THE BOUNDARIES
!----------------------------------------------------------------------
!
      DO J=JFS,JFE
      DO I=IFS,IFE
        DWDTMN(I,J)=-EPSIN
        DWDTMX(I,J)= EPSIN
      ENDDO
      ENDDO
!
!***
      IF(JHL>1)THEN
        JHH=JDE-1-JHL+1 ! JM-JHL+1
        IHL=JHL/2+1
!
        DO J=1,JHL
          IF(J>=MY_JS_GLB-JBPAD2.AND.J<=MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
            DO I=1,IDE-1 ! IM
              IF(I>=MY_IS_GLB-ILPAD2.AND.I<=MY_IE_GLB+IRPAD2)THEN
                IX=I      ! -MY_IS_GLB+1
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
        DO J=JHH,JDE-1   ! JM
          IF(J>=MY_JS_GLB-JBPAD2.AND.J<=MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
            DO I=1,IDE-1 ! IM
              IF(I>=MY_IS_GLB-ILPAD2.AND.I<=MY_IE_GLB+IRPAD2)THEN
                IX=I      ! -MY_IS_GLB+1
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
        DO J=1,JDE-1 ! JM
          IF(J>=MY_JS_GLB-JBPAD2.AND.J<=MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
            DO I=1,IHL
              IF(I>=MY_IS_GLB-ILPAD2.AND.I<=MY_IE_GLB+IRPAD2)THEN
                IX=I      ! -MY_IS_GLB+1
                DWDTMN(IX,JX)=-EPSB
                DWDTMX(IX,JX)= EPSB
              ENDIF
            ENDDO
          ENDIF
        ENDDO
!
        DO J=1,JDE-1 ! JM
          IF(J>=MY_JS_GLB-JBPAD2.AND.J<=MY_JE_GLB+JTPAD2)THEN
            JX=J      ! -MY_JS_GLB+1
             ! moved this line to inside the J-loop, 20030429, jm
            IHH=IDE-1-IHL+MOD(J,2) ! IM-IHL+MOD(J,2)
            DO I=IHH,IDE-1 ! IM
              IF(I>=MY_IS_GLB-ILPAD2.AND.I<=MY_IE_GLB+IRPAD2)THEN
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
      ALLOCATE(EMTEMP(IMS:IME,JMS:JME),STAT=I)  ; EMTEMP   = 0.
      ALLOCATE(GLW(IMS:IME,JMS:JME),STAT=I)     ; GLW      = 0.
      ALLOCATE(HFX(IMS:IME,JMS:JME),STAT=I)     ; HFX      = 0.
      ALLOCATE(LOWLYR(IMS:IME,JMS:JME),STAT=I)  ; LOWLYR   = 0.
!     ALLOCATE(MAVAIL(IMS:IME,JMS:JME),STAT=I)  ; MAVAIL   = 0.
      ALLOCATE(NCA(IMS:IME,JMS:JME),STAT=I)     ; NCA      = 0.
      ALLOCATE(QFX(IMS:IME,JMS:JME),STAT=I)     ; QFX      = 0.
      ALLOCATE(RAINBL(IMS:IME,JMS:JME),STAT=I)  ; RAINBL   = 0.
      ALLOCATE(RAINC(IMS:IME,JMS:JME),STAT=I)   ; RAINC    = 0.
      ALLOCATE(RAINNC(IMS:IME,JMS:JME),STAT=I)  ; RAINNC   = 0.
      ALLOCATE(RAINNCV(IMS:IME,JMS:JME),STAT=I) ; RAINNCV  = 0.

      ALLOCATE(ZS(KMS:KME),STAT=I)              ; ZS       = 0.
      ALLOCATE(SNOWC(IMS:IME,JMS:JME),STAT=I)   ; SNOWC    = 0.
      ALLOCATE(THC(IMS:IME,JMS:JME),STAT=I)     ; THC      = 0.
      ALLOCATE(TMN(IMS:IME,JMS:JME),STAT=I)     ; TMN      = 0.
      ALLOCATE(TSFC(IMS:IME,JMS:JME),STAT=I)    ; TSFC     = 0.
      ALLOCATE(Z0_DUM(IMS:IME,JMS:JME),STAT=I)  ; Z0_DUM   = 0.
      ALLOCATE(ALBEDO_DUM(IMS:IME,JMS:JME),STAT=I)  ; ALBEDO_DUM   = 0.

      ALLOCATE(DZS(KMS:KME),STAT=I)                         ; DZS = 0.
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
      ALLOCATE(ZINT(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; ZINT = 0.
      ALLOCATE(CONVFAC(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; CONVFAC = 0.
      ALLOCATE(PINT_TRANS(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; PINT_TRANS = 0.
      ALLOCATE(T_TRANS(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ;  T_TRANS = 0.
      ALLOCATE(RRI(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ;  RRI = 0.
      ALLOCATE(CLDFRA_TRANS(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; CLDFRA_TRANS = 0.
#ifndef WRF_CHEM      
      ALLOCATE(CLDFRA_OLD(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; CLDFRA_OLD = 0.
#endif
#if 0
      ALLOCATE(W0AVG(IMS:IME,KMS:KME,JMS:JME),STAT=I)       ; W0AVG = 0.
#endif
!-----------------------------------------------------------------------
!jm added set of g_inv
      G_INV=1./G
      ROG=R_D*G_INV
      GRID%RADT=GRID%NRADS*GRID%DT/60.
      GRID%BLDT=GRID%NPHS*GRID%DT/60.
      GRID%CUDT=GRID%NCNVC*GRID%DT/60.
      GRID%GSMDT=GRID%NPHS*GRID%DT/60.
!
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        SFCZ=FIS(I,J)*G_INV
        ZINT(I,KTS,J)=SFCZ
        PDSL(I,J)=PD(I,J)*RES(I,J)
        PSURF=PINT(I,J,KTS)
        EXNSFC=(1.E5/PSURF)**CAPA
        XLAND(I,J)=SM(I,J)+1.
        THSIJ=(SST(I,J)*EXNSFC)*(XLAND(I,J)-1.)                         &
     &        +THS(I,J)*(2.-SM(I,J))
        TSFC(I,J)=THSIJ/EXNSFC
!
        DO K=KTS,KTE-1
          PLYR=(PINT(I,J,K)+PINT(I,J,K+1))*0.5
          TL=T(I,J,K)
          CWML=CWM(I,J,K)
          RRI(I,K,J)=R_D*TL*(1.+P608*Q(I,J,K))/PLYR
          ZINT(I,K+1,J)=ZINT(I,K,J)+TL/PLYR                             & 
                     *(DETA1(K)*PDTOP+DETA2(K)*PDSL(I,J))*ROG        & 
                     *(Q(I,J,K)*P608-CWML+1.)
        ENDDO
!
!        DO K=KTS,KTE
!!!       ZMID(I,K,J)=0.5*(ZINT(I,K,J)+ZINT(I,K+1,J))
!        ENDDO
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!***  RECREATE SIGMA VALUES AT LAYER INTERFACES FOR THE FULL VERTICAL
!***  DOMAIN FROM THICKNESS VALUES FOR THE TWO SUBDOMAINS.
!***  NOTE: KTE=NUMBER OF LAYERS PLUS ONE
!-----------------------------------------------------------------------
!
      PDTOT=101325.-PT
      RPDTOT=1./PDTOT
      PDBOT=PDTOT-PDTOP
      SFULL(KTS)=1.
      SFULL(KTE)=0.
      DSIGSUM = 0.
      DO K=KTS+1,KTE
        DSIG=(DETA1(K-1)*PDTOP+DETA2(K-1)*PDBOT)*RPDTOT
        DSIGSUM=DSIGSUM+DSIG
        SFULL(K)=SFULL(K-1)-DSIG
        SMID(K-1)=0.5*(SFULL(K-1)+SFULL(K))
      ENDDO
      DSIG=(DETA1(KTE-1)*PDTOP+DETA2(KTE-1)*PDBOT)*RPDTOT
      DSIGSUM=DSIGSUM+DSIG
      SMID(KTE-1)=0.5*(SFULL(KTE-1)+SFULL(KTE))
!
!-----------------------------------------------------------------------

      LU_INDEX=IVGTYP

      IF(.NOT.RESTRT)THEN
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          Z0_DUM(I,J)=Z0(I,J) ! hold
          ALBEDO_DUM(I,J)=ALBEDO(I,J) ! Save albedos
        ENDDO
        ENDDO
      ENDIF
!
!***  Always define the quantity Z0BASE
                                                                                                                                              
      IF(.NOT.RESTRT)THEN
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
!
          IF(SM(I,J)==0)then
            Z0BASE(I,J)=VZ0TBL_24(IVGTYP(I,J))+Z0LAND
          ELSE
            Z0BASE(I,J)=VZ0TBL_24(IVGTYP(I,J))+Z0SEA
          ENDIF
!
        ENDDO
        ENDDO
      ENDIF
!
! when allocating CAM radiation 4d arrays (ozmixm, aerosolc) these are not needed
      num_ozmixm=1
      num_aerosolc=1

! Set GMT, JULDAY, and JULYR outside of phy_init because it is no longer 
! called inside phy_init due to moving nest changes.  (When nests move 
! phy_init may not be called on a process if, for example, it is a moving 
! nest and if this part of the domain is not being initialized (not the 
! leading edge).)  Calling domain_setgmtetc() here will avoid this problem 
! when NMM moves to moving nests.  
      CALL domain_setgmtetc( GRID, START_OF_SIMULATION )

      if(restrt) then
        CALL domain_clock_get( grid, current_time=currentTime )
        CALL WRFU_TimeGet( currentTime, YY=grid%julyr, dayOfYear=grid%julday, &
                           H=hr, M=mn, S=sec, MS=ms, rc=rc)
        grid%gmt=hr+real(mn)/60.+real(sec)/3600.+real(ms)/(1000*3600)
        WRITE( wrf_err_message , * ) 'DEBUG start_domain_nmm():  gmt = ',grid%gmt
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
      endif

! Several arguments are RCONFIG entries in Registry.NMM. Registry no longer
! includes these as dummy arguments or declares them.  Access them from 
! GRID.  JM 20050819
#ifndef WRF_NMM_NEST
      MOVED = .FALSE.
#endif

      IF (GRID%RESTART) THEN
         LRESTART = GRID%RESTART
      ELSE
         IF (moved) THEN
            LRESTART = .TRUE.
         ELSE
            LRESTART = .FALSE.
         ENDIF
      END IF

      CALL PHY_INIT(GRID%ID,CONFIG_FLAGS,GRID%DT,LRESTART,SFULL,SMID    &
     &             ,PT,TSFC,GRID%RADT,GRID%BLDT,GRID%CUDT,GRID%GSMDT    &
     &             ,RTHCUTEN, RQVCUTEN, RQRCUTEN                        &
     &             ,RQCCUTEN, RQSCUTEN, RQICUTEN                        &
     &             ,RUBLTEN,RVBLTEN,RTHBLTEN                            &
     &             ,RQVBLTEN,RQCBLTEN,RQIBLTEN                          &
     &             ,RTHRATEN,RTHRATENLW,RTHRATENSW                      &
     &             ,STEPBL,STEPRA,STEPCU                                &
     &             ,W0AVG, RAINNC, RAINC, RAINCV, RAINNCV               &
     &             ,NCA,GRID%SWRAD_SCAT                                 &
     &             ,CLDEFI,LOWLYR                                       &
     &             ,MASS_FLUX                                           &
     &             ,RTHFTEN, RQVFTEN                                    &
     &             ,CLDFRA_TRANS,CLDFRA_OLD,GLW,GSW,EMISS,EMTEMP,LU_INDEX&
     &             ,GRID%LANDUSE_ISICE, GRID%LANDUSE_LUCATS             &
     &             ,GRID%LANDUSE_LUSEAS, GRID%LANDUSE_ISN               &
     &             ,GRID%LU_STATE                                       &
     &             ,XLAT,XLONG,ALBEDO,ALBBCK                            &
     &             ,GRID%GMT,GRID%JULYR,GRID%JULDAY                     &
     &             ,GRID%LEVSIZ, NUM_OZMIXM, NUM_AEROSOLC, GRID%PAERLEV &
     &             ,TMN,XLAND,ZNT,Z0,USTAR,MOL,PBLH,TKE_MYJ             &
     &             ,EXCH_H,THC,SNOWC,MAVAIL,HFX,QFX,RAINBL              &
     &             ,STC,ZS,DZS,GRID%NUM_SOIL_LAYERS,WARM_RAIN           &
     &             ,ADV_MOIST_COND                                      &
     &             ,APR_GR,APR_W,APR_MC,APR_ST,APR_AS                   &
     &             ,APR_CAPMA,APR_CAPME,APR_CAPMI                       &
     &             ,XICE,XICE,VEGFRA,SNOW,CANWAT,SMSTAV                 &
     &             ,SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW            &
     &             ,ACSNOM,IVGTYP,ISLTYP,SFCEVP,SMC                     &
     &             ,SH2O, SNOWH, SMFR3D                                 &  ! temporary
     &             ,grid%SNOALB                                         &
     &             ,GRID%DX,GRID%DY,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY    &
     &             ,MP_RESTART_STATE,TBPVS_STATE,TBPVS0_STATE           &
     &             ,.TRUE.,moved,START_OF_SIMULATION                    &
     &             ,1                                                   & ! lagday
     &             ,IDS, IDE, JDS, JDE, KDS, KDE                        &
     &             ,IMS, IME, JMS, JME, KMS, KME                        &
     &             ,ITS, ITE, JTS, JTE, KTS, KTE                        &
     &             ,NUM_URBAN_LAYERS                                    &
     &                )

!-----------------------------------------------------------------------
!---- Initialization for gravity wave drag (GWD) & mountain blocking (MB)
!
      CALL nl_get_cen_lat(GRID%ID, CEN_LAT)    !-- CEN_LAT in deg
      CALL nl_get_cen_lon(GRID%ID, CEN_LON)    !-- CEN_LON in deg
      DTPHS=grid%dt*grid%nphs
      CALL GWD_init(DTPHS,GRID%DX,GRID%DY,CEN_LAT,CEN_LON,RESTRT        &
     &              ,GLAT,GLON,CROT,SROT,HANGL                          &
     &              ,IDS,IDE,JDS,JDE,KDS,KDE                            &
     &              ,IMS,IME,JMS,JME,KMS,KME                            &
     &              ,ITS,ITE,JTS,JTE,KTS,KTE )
      IF(.NOT.RESTRT)THEN
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          UGWDsfc(I,J)=0.
          VGWDsfc(I,J)=0.
        ENDDO
        ENDDO
      ENDIF

!-----------------------------------------------------------------------
!
      IF(NSTART==0)THEN

        DO J=JMS,JME
        DO I=IMS,IME
          Z0(I,J)=Z0BASE(I,J)
        ENDDO
        ENDDO

        DO K=KMS,KME
        DO J=JMS,JME
        DO I=IMS,IME
          CLDFRA(I,J,K)=CLDFRA_TRANS(I,K,J)
        ENDDO
        ENDDO
        ENDDO

      ENDIF

!
!
!mp replace F*_PHY with values defined in module_initialize_real.F?

      IF (.NOT. RESTRT) THEN
! Added by Greg Thompson, NCAR-RAL, for initializing water vapor
! mixing ratio (from NMM's specific humidity var) into moist array.

!!mp
        CALL wrf_message('Initializng moist(:,:,:, Qv) from Q')
        DO K=KPS,KPE
        DO J=JFS,JFE
        DO I=IFS,IFE
           moist(I,J,K,P_QV) = Q(I,J,K) / (1.-Q(I,J,K))                 
        enddo      
        enddo      
        enddo      
     
! Also sum cloud water, ice, rain, snow, graupel into Ferrier CWM       
! array (if any hydrometeors found and non-zero from initialization     
! package).  Then, determine fractions ice and rain from species.       
     
        IF (.not. (MAXVAL(CWM).gt.0. .and. MAXVAL(CWM).lt.1.) ) then    
          do i_m = 2, num_moist
          if (i_m.ne.p_qv) &
     &       CALL wrf_message(' summing moist(:,:,:,i_m) into CWM array')
          DO K=KPS,KPE
          DO J=JFS,JFE
          DO I=IFS,IFE
            IF ( (moist(I,J,K,i_m).gt.EPSQ) .and. (i_m.ne.p_qv) ) THEN  
               CWM(I,J,K) = CWM(I,J,K) + moist(I,J,K,i_m)               
            ENDIF  
          enddo    
          enddo
          enddo
          enddo

          IF (.not. ( (maxval(F_ICE)+maxval(F_RAIN)) .gt. EPSQ) ) THEN
            CALL wrf_message(' computing F_ICE')
            do i_m = 2, num_moist
            DO J=JFS,JFE
            DO K=KPS,KPE
            DO I=IFS,IFE
              IF ( (moist(I,J,K,i_m).gt.EPSQ) .and. &
     &               ( (i_m.eq.p_qi).or.(i_m.eq.p_qs).or.(i_m.eq.p_qg) ) ) THEN
                 F_ICE(I,K,J) = F_ICE(I,K,J) + moist(I,J,K,i_m)
              ENDIF
              if (model_config_rec%mp_physics(grid%id).EQ.ETAMPNEW) then
                if ((i_m.eq.p_qi).or.(i_m.eq.p_qg) ) then
                  moist(I,J,K,p_qs)=moist(I,J,K,p_qs)+moist(I,J,K,i_m)
                  moist(I,J,K,i_m) =0.
                endif
              endif
            enddo
            enddo
            enddo
            enddo
            CALL wrf_message(' computing F_RAIN')
!
            DO J=JFS,JFE
            DO K=KPS,KPE
            DO I=IFS,IFE
              IF(F_ICE(i,k,j)<=EPSQ)THEN
                F_ICE(I,K,J)=0.
              ELSE
                F_ICE(I,K,J) = F_ICE(I,K,J)/CWM(I,J,K)
              ENDIF
              IF ( (moist(I,J,K,p_qr)+moist(I,J,K,p_qc)).gt.EPSQ) THEN
                IF(moist(i,j,k,p_qr)<=EPSQ)THEN
                  F_RAIN(I,K,J)=0.
                ELSE
                  F_RAIN(I,K,J) = moist(i,j,k,p_qr) &
     &                    / (moist(i,j,k,p_qr)+moist(i,j,k,p_qc))
                ENDIF
              ENDIF
            enddo
            enddo
            enddo
          ENDIF
        ENDIF
! End addition by Greg Thompson

        IF (maxval(F_ICE) .gt. 0.) THEN
         do J=JMS,JME
         do K=KMS,KME
         do I=IMS,IME
          F_ICE_PHY(I,K,J)=F_ICE(I,K,J)
         enddo
         enddo
         enddo
        ENDIF

        IF (maxval(F_RAIN) .gt. 0.) THEN
         do J=JMS,JME
         do K=KMS,KME
         do I=IMS,IME
          F_RAIN_PHY(I,K,J)=F_RAIN(I,K,J)
         enddo
         enddo
         enddo
        ENDIF

        IF (maxval(F_RIMEF) .gt. 0.) THEN
          do J=JMS,JME
          do K=KMS,KME
          do I=IMS,IME
            F_RIMEF_PHY(I,K,J)=F_RIMEF(I,K,J)
          enddo
          enddo
          enddo
        ENDIF
      ENDIF
!
      IF (.NOT. RESTRT) THEN
  !-- Replace albedos if original albedos are nonzero
        IF(MAXVAL(ALBEDO_DUM)>0.)THEN
          DO J=JMS,JME
          DO I=IMS,IME
            ALBEDO(I,J)=ALBEDO_DUM(I,J)
          ENDDO
          ENDDO
        ENDIF
      ENDIF

      IF(.NOT.RESTRT)THEN
        DO J=JMS,JME
        DO I=IMS,IME
          APREC(I,J)=RAINNC(I,J)*1.E-3
          CUPREC(I,J)=RAINCV(I,J)*1.E-3
        ENDDO
        ENDDO
      ENDIF
!following will need mods Sep06
!
#ifdef WRF_CHEM
      DO J=JTS,JTE
        JJ=MIN(JDE-1,J)
        DO K=KTS,KTE-1
          KK=MIN(KDE-1,K)
          DO I=ITS,ITE
            II=MIN(IDE-1,I)
            CONVFAC(I,K,J) = PINT(II,JJ,KK)/RGASUNIV/T(II,JJ,KK)
          ENDDO
        ENDDO
      ENDDO
      
      DO J=JMS,JME
        DO K=KMS,KME
          DO I=IMS,IME
            PINT_TRANS(I,K,J)=PINT(I,J,K)
            T_TRANS(I,K,J)=T(I,J,K)
          ENDDO
        ENDDO
      ENDDO 
       CALL CHEM_INIT (GRID%ID,CHEM,GRID%DT,GRID%BIOEMDT,GRID%PHOTDT,GRID%CHEMDT, &
               STEPBIOE,STEPPHOT,STEPCHEM,STEPFIREPL,GRID%PLUMERISEFIRE_FRQ,      &
               ZINT,G,AERWRF,CONFIG_FLAGS,                     &
               RRI,T_TRANS,PINT_TRANS,CONVFAC,                 &
               GD_CLOUD,GD_CLOUD2,GD_CLOUD_B,GD_CLOUD2_B,            &
               TAUAER1,TAUAER2,TAUAER3,TAUAER4,                      &
               GAER1,GAER2,GAER3,GAER4,                              &
               WAER1,WAER2,WAER3,WAER4,                              &
               PM2_5_DRY,PM2_5_WATER,PM2_5_DRY_EC,GRID%CHEM_IN_OPT,  &
               GRID%KEMIT,                                           &
               IDS , IDE , JDS , JDE , KDS , KDE ,                   &
               IMS , IME , JMS , JME , KMS , KME ,                   &
               ITS , ITE , JTS , JTE , KTS , KTE                     )

!     
! calculate initial pm
!     
        SELECT CASE (CONFIG_FLAGS%CHEM_OPT)
        CASE (RADM2SORG, RACMSORG,RACMSORG_KPP)
           CALL SUM_PM_SORGAM (                                             &
                RRI, CHEM, H2OAJ, H2OAI,                              &
                PM2_5_DRY, PM2_5_WATER, PM2_5_DRY_EC, PM10,                 &
                IDS,IDE, JDS,JDE, KDS,KDE,                                  &
                IMS,IME, JMS,JME, KMS,KME,                                  &
                ITS,ITE, JTS,JTE, KTS,KTE-1                                 )
             
        CASE (CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ)
           CALL SUM_PM_MOSAIC (                                             &
                RRI, CHEM,                                            &
                PM2_5_DRY, PM2_5_WATER, PM2_5_DRY_EC, PM10,                 &
                IDS,IDE, JDS,JDE, KDS,KDE,                                  &
                IMS,IME, JMS,JME, KMS,KME,                                  &
                ITS,ITE, JTS,JTE, KTS,KTE-1                                 )
             
        CASE DEFAULT
           DO J=JTS,MIN(JTE,JDE-1)
              DO K=KTS,MIN(KTE,KDE-1)
                 DO I=ITS,MIN(ITE,IDE-1)
                    PM2_5_DRY(I,K,J)    = 0.
                    PM2_5_WATER(I,K,J)  = 0.
                    PM2_5_DRY_EC(I,K,J) = 0.
                    PM10(I,K,J)         = 0.
                 ENDDO
              ENDDO
           ENDDO
        END SELECT
#endif
      DEALLOCATE(SFULL)
      DEALLOCATE(SMID)
      DEALLOCATE(DZS)
      DEALLOCATE(EMISS)
      DEALLOCATE(EMTEMP)
      DEALLOCATE(GLW)
      DEALLOCATE(HFX)
      DEALLOCATE(LOWLYR)
!     DEALLOCATE(MAVAIL)
      DEALLOCATE(NCA)
      DEALLOCATE(QFX)
      DEALLOCATE(RAINBL)
      DEALLOCATE(RAINC)
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
      DEALLOCATE(ZINT)
      DEALLOCATE(CONVFAC)
      DEALLOCATE(RRI)
      DEALLOCATE(SNOWC)
      DEALLOCATE(THC)
      DEALLOCATE(TMN)
      DEALLOCATE(TSFC)
      DEALLOCATE(ZS)
      DEALLOCATE(PINT_TRANS)
      DEALLOCATE(T_TRANS)
      DEALLOCATE(CLDFRA_TRANS)
#ifndef WRF_CHEM
      DEALLOCATE(CLDFRA_OLD)
#endif
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
#  include <HALO_NMM_INIT_15B.inc>
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
#include <scalar_derefs.inc>

   RETURN


END SUBROUTINE START_DOMAIN_NMM

