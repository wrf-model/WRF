!WRF:MODEL_LAYER:INITIALIZATION
!

!  This MODULE holds the routines which are used to perform model start-up operations
!  for the individual domains.  This is the stage after inputting wrfinput and before
!  calling 'integrate'.

!  This MODULE CONTAINS the following routines:


MODULE module_physics_init

   USE module_domain
!  USE module_io_domain
   USE module_state_description
   USE module_model_constants
!  USE module_timing
   USE module_configure
   USE module_date_time
#ifdef DM_PARALLEL
   USE module_dm
#endif

CONTAINS


!=================================================================
   SUBROUTINE phy_init ( grid,                                   &
                         id, config_flags, DT, zfull, zhalf,     &
                         p_top, TSK,RADT,BLDT,CUDT,MPDT,         &
                         RTHCUTEN, RQVCUTEN, RQRCUTEN,           &
                         RQCCUTEN, RQSCUTEN, RQICUTEN,           &
                         RUBLTEN,RVBLTEN,RTHBLTEN,               &
                         RQVBLTEN,RQCBLTEN,RQIBLTEN,             &
                         RTHRATEN,RTHRATENLW,RTHRATENSW,         &
                         STEPBL,STEPRA,STEPCU,                   &
                         W0AVG, RAINNC, RAINC, RAINCV, RAINNCV,  &
                         NCA,                                    &
                         CLDEFI,LOWLYR,                          &
                         MASS_FLUX,                              &
                         RTHFTEN, RQVFTEN,                       &
                         CLDFRA,GLW,GSW,EMISS,LU_INDEX,          &
                         XLAT,XLONG,ALBEDO,ALBBCK,GMT,JULYR,JULDAY,&
                         TMN,XLAND,ZNT,Z0,UST,MOL,PBLH,TKE_MYJ,  &
                         EXCH_H,THC,SNOWC,MAVAIL,HFX,QFX,RAINBL,        &
                         TSLB,ZS,DZS,num_soil_layers,warm_rain,  & 
                         APR_GR,APR_W,APR_MC,APR_ST,APR_AS,      &
                         APR_CAPMA,APR_CAPME,APR_CAPMI,          &
                         XICE,VEGFRA,SNOW,CANWAT,SMSTAV,         &
                         SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,&
                         ACSNOM,IVGTYP,ISLTYP, SFCEVP, SMOIS,    &
                         SH2O, SNOWH, SMFR3D,                    &  ! temporary
                         DX,DY,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY, &
                         mp_restart_state,tbpvs_state,tbpvs0_state,&
                         allowed_to_read, moved,                 &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         its, ite, jts, jte, kts, kte           )

!-----------------------------------------------------------------
   USE module_wrf_error
   IMPLICIT NONE
!-----------------------------------------------------------------
   TYPE (domain), INTENT(INOUT)             :: grid
   TYPE (grid_config_rec_type)              :: config_flags

   INTEGER , INTENT(IN)        :: id
   LOGICAL , INTENT(OUT)       :: warm_rain
!   LOGICAL , INTENT (IN)       :: FNDSOILW, FNDSNOWH
   LOGICAL, PARAMETER          :: FNDSOILW=.true., FNDSNOWH=.true.
   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,  &
                                  ims, ime, jms, jme, kms, kme,  &
                                  its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)        :: num_soil_layers

   REAL,     INTENT(IN)        :: DT, p_top, DX, DY
   REAL,     INTENT(IN)        :: RADT,BLDT,CUDT,MPDT

   REAL,     DIMENSION( kms:kme ) , INTENT(IN) :: zfull, zhalf
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: TSK, XLAT, XLONG

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),&
                 INTENT(INOUT) :: SMOIS, SH2O,TSLB
   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ), INTENT(OUT) :: SMFR3D

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                           SNOW, &
                                                         SNOWC, &
                                                         SNOWH, &
                                                        CANWAT, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        SFCEVP, &
                                                        GRDFLX, &
                                                        ACSNOW, &
                                                          XICE, &
                                                        VEGFRA, &
                                                        ACSNOM

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP

! rad

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RTHRATEN, RTHRATENLW, RTHRATENSW, CLDFRA

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::         &
             GSW,ALBEDO,ALBBCK,GLW,EMISS

   REAL,     INTENT(INOUT) :: GMT

   INTEGER , INTENT(OUT) :: STEPRA, STEPBL, STEPCU
   INTEGER , INTENT(INOUT) :: JULYR, JULDAY

! cps

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RTHCUTEN, RQVCUTEN, RQRCUTEN, RQCCUTEN, RQSCUTEN,   &
             RQICUTEN

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: MASS_FLUX,   &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RTHFTEN, RQVFTEN

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) ::           &
             RAINNC, RAINC, RAINCV, RAINNCV

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: CLDEFI, NCA

   INTEGER,  DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: LOWLYR

!pbl

   ! soil layer


   REAL,     DIMENSION(1:num_soil_layers),      INTENT(INOUT) :: ZS,DZS

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,RQIBLTEN,EXCH_H,TKE_MYJ

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::         &
             XLAND,ZNT,Z0,UST,MOL,LU_INDEX,                         &
             PBLH,THC,MAVAIL,HFX,QFX,RAINBL

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: TMN

!mp
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::   &
             F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL, DIMENSION(:), INTENT(INOUT)   :: mp_restart_state,tbpvs_state,tbpvs0_state
   LOGICAL,  INTENT(IN)  :: allowed_to_read, moved

! Local data

   REAL    :: ALBLND,ZZLND,ZZWTR,THINLD,XMAVA,CEN_LAT,pptop 
   REAL,     DIMENSION( kms:kme )  :: sfull, shalf
   
   CHARACTER*4 :: MMINLU_loc
   CHARACTER*80 :: message
   INTEGER :: ISWATER
! to be added to namelist: option to use climatological monthly albedo
   LOGICAL :: usebgalb

   INTEGER :: i, j, itf, jtf
   LOGICAL :: restart
   INTEGER :: hr, min, sec, ms, rc
integer myproc

!-----------------------------------------------------------------

!-- should be from the namelist

   sfull = 0.
   shalf = 0.

   CALL wrf_debug(100,'top of phy_init')

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   IF ( .NOT. grid%restart ) THEN
     restart = .false.
     CALL wrf_clockprint(150, grid%domain_clock, &
                         'DEBUG phy_init():  get currTime from grid%domain_clock,')
     CALL WRF_UTIL_ClockGet( grid%domain_clock, currTime=grid%current_time, rc=rc )
     CALL WRF_UTIL_TimeGet( grid%current_time, YY=julyr, dayOfYear=julday, H=hr, M=min, S=sec, MS=ms, rc=rc)
     CALL wrf_timetoa ( grid%current_time, message )
     WRITE( wrf_err_message , * ) 'DEBUG phy_init():  currTime = [',TRIM( message ),']'
     CALL wrf_debug( 150, TRIM(wrf_err_message) )
     gmt=hr+real(min)/60.+real(sec)/3600.+real(ms)/(1000*3600)
     WRITE( wrf_err_message , * ) 'DEBUG phy_init():  julyr,hr,min,sec,ms,julday = ', &
                                  julyr,hr,min,sec,ms,julday
     CALL wrf_debug( 150, TRIM(wrf_err_message) )
     WRITE( wrf_err_message , * ) 'DEBUG phy_init():  gmt = ',gmt
     CALL wrf_debug( 150, TRIM(wrf_err_message) )
   ELSE
     restart = .true.
     CALL nl_get_julyr (id, julyr)
     CALL nl_get_julday (id, julday)
     CALL nl_get_gmt (id, gmt)
   END IF

   ZZLND=0.1
   ZZWTR=0.0001
   THINLD=0.04
   ALBLND=0.2
   XMAVA=0.3
   usebgalb = .FALSE.

   CALL nl_get_cen_lat(id,cen_lat)
   CALL wrf_debug(100,'calling nl_get_iswater, nl_get_mminlu_loc')
   CALL nl_get_iswater(id,iswater)
   CALL nl_get_mminlu( 1, mminlu_loc )
   CALL wrf_debug(100,'after nl_get_iswater, nl_get_mminlu_loc')

  IF(.not.restart)THEN
!-- initialize common variables

   IF ( .NOT. moved ) THEN
   DO j=jts,jtf
   DO i=its,itf
      XLAND(i,j)=1.
      GSW(i,j)=0.
      GLW(i,j)=0.
      UST(i,j)=0.
      MOL(i,j)=0.0
      PBLH(i,j)=0.0
      HFX(i,j)=0.
      QFX(i,j)=0.
      RAINBL(i,j)=0.
      RAINNCV(i,j)=0.
   ENDDO
   ENDDO
   ENDIF

!
   DO j=jts,jtf
   DO i=its,itf
     IF(XLAND(i,j) .LT. 1.5)THEN
       IF(mminlu_loc .EQ. '    ') ALBBCK(i,j)=ALBLND
       ALBEDO(i,j)=ALBLND
       EMISS(i,j)=0.85
       THC(i,j)=THINLD
       ZNT(i,j)=ZZLND
#if  ! ( NMM_CORE == 1 ) 
       Z0(i,j)=ZZLND
#endif
       MAVAIL(i,j)=XMAVA
     ELSE
       IF(mminlu_loc .EQ. '    ') ALBBCK(i,j)=0.08
       ALBEDO(i,j)=0.08
       EMISS(i,j)=0.98
       THC(i,j)=THINLD
       ZNT(i,j)=ZZWTR
#if  ! ( NMM_CORE == 1 ) 
       Z0(i,j)=ZZWTR
#endif
       MAVAIL(i,j)=1.0 
     ENDIF
   ENDDO
   ENDDO

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to landuse_init' )

   IF(mminlu_loc .ne. '    ')THEN
!-- initialize surface properties

     CALL landuse_init(lu_index, snowc, albedo, albbck, mavail, emiss,      &
                znt, Z0, thc, xland, xice, julday, cen_lat, iswater, mminlu_loc,  &
                allowed_to_read , usebgalb ,                        &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                its, ite, jts, jte, kts, kte                       ) 
   ENDIF
  ENDIF

!-- convert zfull and zhalf to sigma values for ra_init (Eta CO2 needs these)
!-- zfull/zhalf may be either zeta or eta
!-- what is done here depends on coordinate (check this code if adding new coordinates)
   CALL z2sigma(zfull,zhalf,sfull,shalf,p_top,pptop,config_flags, &
                allowed_to_read,                                  &
                kds,kde,kms,kme,kts,kte)

!-- initialize physics
!-- ra: radiation
!-- bl: pbl
!-- cu: cumulus
!-- mp: microphysics

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to ra_init' )

   CALL ra_init(grid,STEPRA,RADT,DT,RTHRATEN,RTHRATENLW,             &
                RTHRATENSW,CLDFRA,EMISS,cen_lat,JULYR,JULDAY,GMT,    &
                sfull,shalf,pptop,                              &
                config_flags,restart,                           & 
                allowed_to_read ,                                   &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to bl_init' )

   CALL bl_init(STEPBL,BLDT,DT,RUBLTEN,RVBLTEN,RTHBLTEN,        &
                RQVBLTEN,RQCBLTEN,RQIBLTEN,TSK,TMN,             &
                config_flags,restart,UST,LOWLYR,TSLB,ZS,DZS,    &
                num_soil_layers,TKE_MYJ,EXCH_H,VEGFRA,          &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,SMOIS,SMFR3D,                     &
                SNOWH,SH2O,FNDSOILW, FNDSNOWH,                  &
#if (NMM_CORE == 1)
                Z0,XLAND,XICE,                                  &
#else
                ZNT,XLAND,XICE,                                 &
#endif
                SFCEVP,GRDFLX,                                  &
                allowed_to_read ,                                   &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to cu_init' )

   CALL cu_init(STEPCU,CUDT,DT,RTHCUTEN,RQVCUTEN,RQRCUTEN,      &
                RQCCUTEN,RQSCUTEN,RQICUTEN,NCA,RAINC,           &
                RAINCV,W0AVG,config_flags,restart,              &
                CLDEFI,LOWLYR,MASS_FLUX,                        &
                RTHFTEN, RQVFTEN,                               &
                APR_GR,APR_W,APR_MC,APR_ST,APR_AS,              &
                APR_CAPMA,APR_CAPME,APR_CAPMI,                  &
                allowed_to_read ,                                   &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to mp_init' )

   CALL mp_init(RAINNC,config_flags,restart,warm_rain,          &
                MPDT, DT, DX, DY, LOWLYR,                       & 
                F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,               &
                mp_restart_state,tbpvs_state,tbpvs0_state,       &
                allowed_to_read ,                               &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   write(message,*)'STEPRA,STEPCU,STEPBL',STEPRA,STEPCU,STEPBL
   CALL wrf_message( message )

   END SUBROUTINE phy_init

!=====================================================================
   SUBROUTINE landuse_init(lu_index, snowc, albedo, albbck, mavail, emiss,  &
                znt,Z0,thc,xland, xice, julday, cen_lat, iswater, mminlu, &
                allowed_to_read , usebgalb ,                        &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                its, ite, jts, jte, kts, kte                       )

   USE module_wrf_error
   IMPLICIT NONE

!---------------------------------------------------------------------
   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)           :: iswater, julday
   REAL    , INTENT(IN)           :: cen_lat
   CHARACTER*4, INTENT(IN)        :: mminlu
   LOGICAL,  INTENT(IN)           :: allowed_to_read , usebgalb
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: lu_index, snowc, xice
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT  ) :: albedo, albbck, mavail, emiss, &
                                                         znt, Z0, thc, xland

!---------------------------------------------------------------------
! Local
   CHARACTER*4 LUTYPE
   CHARACTER*80 :: message
   INTEGER  :: landuse_unit, LS, LC, LI, LUN, NSN
   INTEGER  :: i, j, itf, jtf, is
   INTEGER , PARAMETER :: max_cats = 100 , max_seas = 12 
   INTEGER , PARAMETER :: OPEN_OK = 0
   INTEGER :: ierr
! save these fields in case nest moves or has to be reinitialized
! and this routine is called with allowed_to_read set to false
! note that by saving these, we're locking in the same landuse for
! the duration of a run; possible implications for long climate runs
   INTEGER , SAVE :: ISICE, LUCATS, LUSEAS, ISN
   REAL    , SAVE , DIMENSION( max_cats, max_seas ) :: ALBD, SLMO, SFEM, SFZ0, THERIN, SFHC
   REAL    , SAVE , DIMENSION( max_cats )     :: SCFX
   LOGICAL :: FOUND_LU
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

!---------------------------------------------------------------------
   CALL wrf_debug( 100 , 'top of landuse_init' )

   FOUND_LU = .TRUE.
   IF ( allowed_to_read ) THEN
      landuse_unit = 29
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(landuse_unit, FILE='LANDUSE.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF ( ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_physics_init.F: LANDUSE_INIT: open failure for LANDUSE.TBL'
          CALL wrf_error_fatal ( message ) 
        END IF
      ENDIF
! Determine season (summer=1, winter=2)
      ISN=1                                                            
      IF(JULDAY.LT.105.OR.JULDAY.GT.288)ISN=2                         
      IF(CEN_LAT.LT.0.0)ISN=3-ISN                                   

! Read info from file LANDUSE.TBL
      IF(MMINLU.EQ.'OLD ')THEN
!       ISWATER=7
        ISICE=11 
      ELSE IF(MMINLU.EQ.'USGS')THEN
!       ISWATER=16
        ISICE=24
      ELSE IF(MMINLU.EQ.'SiB ')THEN
!       ISWATER=15
        ISICE=16
      ELSE IF(MMINLU.EQ.'LW12')THEN
!       ISWATER=15
        ISICE=3
      ENDIF
      PRINT *, 'INPUT LANDUSE = ',MMINLU
      FOUND_LU = .FALSE.
 1999   CONTINUE                                                      
      if ( wrf_dm_on_monitor() ) then
        READ (landuse_unit,2000,END=2001)LUTYPE                                
        READ (landuse_unit,*)LUCATS,LUSEAS                                    
        FOUND_LU = LUTYPE.EQ.MMINLU
      endif
      CALL wrf_dm_bcast_string(lutype, 4)
      CALL wrf_dm_bcast_bytes (lucats,  IWORDSIZE )
      CALL wrf_dm_bcast_bytes (luseas,  IWORDSIZE )
      CALL wrf_dm_bcast_bytes (found_lu,  LWORDSIZE )
 2000   FORMAT (A4)                                                
        IF(FOUND_LU)THEN                                  
          LUN=LUCATS                                             
          NSN=LUSEAS                                            
            PRINT *, 'LANDUSE TYPE = ',LUTYPE,' FOUND',        &
                   LUCATS,' CATEGORIES',LUSEAS,' SEASONS',     &
                   ' WATER CATEGORY = ',ISWATER,               &
                   ' SNOW CATEGORY = ',ISICE                
        ENDIF                                             
        DO LS=1,LUSEAS                                   
          if ( wrf_dm_on_monitor() ) then
            READ (landuse_unit,*)                                   
          endif
          DO LC=1,LUCATS                               
            IF(FOUND_LU)THEN                  
              IF ( wrf_dm_on_monitor() ) THEN
                READ (landuse_unit,*)LI,ALBD(LC,LS),SLMO(LC,LS),SFEM(LC,LS),        &       
                           SFZ0(LC,LS),THERIN(LC,LS),SCFX(LC),SFHC(LC,LS)       
              ENDIF
              CALL wrf_dm_bcast_bytes (LI,  IWORDSIZE )
              IF(LC.NE.LI)CALL wrf_error_fatal ( 'module_start: MISSING LANDUSE UNIT ' )
            ELSE                                                            
              IF ( wrf_dm_on_monitor() ) THEN
                READ (landuse_unit,*)                                                  
              ENDIF
            ENDIF                                                         
          ENDDO                                                          
        ENDDO                                                           
        IF(NSN.EQ.1) THEN
           ISN = 1
        END IF
        CALL wrf_dm_bcast_bytes (albd,   max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (slmo,   max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (sfem,   max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (sfz0,   max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (therin, max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (sfhc,   max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (scfx,   max_cats *            RWORDSIZE )

        IF(.NOT. FOUND_LU) GOTO 1999
 2001   CONTINUE                                                      
        IF(.NOT. FOUND_LU)THEN                                         
          CALL wrf_message ( 'LANDUSE IN INPUT FILE DOES NOT MATCH LUTABLE: TABLE NOT USED' )
        ENDIF                                                     
     ENDIF  ! allowed_to_read

    IF(FOUND_LU)THEN
! Set arrays according to lu_index
      itf = min0(ite, ide-1)
      jtf = min0(jte, jde-1)
      IF(usebgalb)CALL wrf_message ( 'Climatological albedo is used instead of table values' )
      DO j = jts, jtf
        DO i = its, itf
          IS=nint(lu_index(i,j))
          ! only do this check on read-in data
          IF(IS.LT.0.OR.IS.GT.LUN.AND.allowed_to_read)THEN                                        
            WRITE ( wrf_err_message , * ) 'ERROR: LANDUSE OUTSIDE RANGE =',IS,' AT ',I,J,' LUN= ',LUN
            CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
          ENDIF                                                            
!   SET NO-DATA POINTS (IS=0) TO WATER                                    
          IF(IS.EQ.0)THEN                                                
            IS=ISWATER                                                  
          ENDIF                                                        
          IF(.NOT.usebgalb)ALBBCK(I,J)=ALBD(IS,ISN)/100.                                  
          ALBEDO(I,J)=ALBBCK(I,J)
          IF(SNOWC(I,J) .GT. 0.5)ALBEDO(I,J)=ALBBCK(I,J)*(1.+SCFX(IS))
          THC(I,J)=THERIN(IS,ISN)/100.                               
          Z0(I,J)=SFZ0(IS,ISN)/100.                                
          ZNT(I,J)=Z0(I,J)
          EMISS(I,J)=SFEM(IS,ISN)                                  
          MAVAIL(I,J)=SLMO(IS,ISN)                                
          IF(IS.NE.ISWATER)THEN                                  
            XLAND(I,J)=1.0                                      
          ELSE                                                 
            XLAND(I,J)=2.0                                    
          ENDIF                                              
!    SET SEA-ICE POINTS TO LAND WITH ICE/SNOW SURFACE PROPERTIES
          IF(XICE(I,J).GT.0.5)THEN
            XLAND(I,J)=1.0
            ALBBCK(I,J)=ALBD(ISICE,ISN)/100.
            ALBEDO(I,J)=ALBBCK(I,J)
            THC(I,J)=THERIN(ISICE,ISN)/100.                              
            Z0(I,J)=SFZ0(ISICE,ISN)/100.                               
            ZNT(I,J)=Z0(I,J)
            EMISS(I,J)=SFEM(ISICE,ISN)                                 
            MAVAIL(I,J)=SLMO(ISICE,ISN)                               
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    if ( wrf_dm_on_monitor() .and. allowed_to_read ) then
      CLOSE (landuse_unit)
    endif
    CALL wrf_debug( 100 , 'returning from of landuse_init' )

    RETURN
        
   END SUBROUTINE landuse_init 
!===================================================================== 
   SUBROUTINE ra_init(grid,STEPRA,RADT,DT,RTHRATEN,RTHRATENLW,       & 
                      RTHRATENSW,CLDFRA,EMISS,cen_lat,JULYR,JULDAY,GMT,    &
                      sfull,shalf,pptop,                             &
                      config_flags,restart,                          & 
                      allowed_to_read ,                              &
                      ids, ide, jds, jde, kds, kde,                  &
                      ims, ime, jms, jme, kms, kme,                  &
                      its, ite, jts, jte, kts, kte                   )
!---------------------------------------------------------------------
   USE module_ra_rrtm
   USE module_ra_sw
   USE module_ra_gsfcsw
   USE module_ra_gfdleta
   USE module_domain
!---------------------------------------------------------------------
   IMPLICIT NONE
!---------------------------------------------------------------------
   TYPE (domain)                  :: grid
   TYPE (grid_config_rec_type)    :: config_flags
   LOGICAL , INTENT(IN)           :: restart
   LOGICAL,  INTENT(IN)           :: allowed_to_read

   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)           :: JULDAY,JULYR
   REAL ,    INTENT(IN)           :: DT, RADT, cen_lat, GMT, pptop

   INTEGER , INTENT(INOUT)        :: STEPRA
   INTEGER :: isn

   REAL , DIMENSION( kms:kme ) , INTENT(IN) :: sfull, shalf
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::           &
                                                           RTHRATEN, &
                                                         RTHRATENLW, &
                                                         RTHRATENSW, &
                                                             CLDFRA
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: EMISS
   LOGICAL :: etalw = .false.
   integer :: month,iday
!---------------------------------------------------------------------

!-- calculate radiation time step

    STEPRA = nint(RADT*60./DT)
    STEPRA = max(STEPRA,1)


!-- chose long wave radiation scheme
 
   lwrad_select: SELECT CASE(config_flags%ra_lw_physics)

        CASE (RRTMSCHEME)
             CALL rrtminit(RTHRATEN,RTHRATENLW,CLDFRA,      &
                           restart,                         &
                           allowed_to_read ,                &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )
        CASE (GFDLLWSCHEME)
             CALL nl_get_start_month(grid%id,month)
             CALL nl_get_start_day(grid%id,iday)
             CALL gfdletainit(emiss,sfull,shalf,pptop,JULYR,MONTH,IDAY,GMT,&
                           config_flags,allowed_to_read ,            &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )
             etalw = .true.
        CASE DEFAULT

   END SELECT lwrad_select
!-- initialize short wave radiation scheme
 
   swrad_select: SELECT CASE(config_flags%ra_sw_physics)

        CASE (SWRADSCHEME)
             CALL swinit(RTHRATEN,RTHRATENSW,              &
                         restart,                          &
                         allowed_to_read ,                 &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         its, ite, jts, jte, kts, kte      )

        CASE (GSFCSWSCHEME)
             CALL gsfc_swinit(cen_lat, allowed_to_read )

        CASE (GFDLSWSCHEME)
             IF(.not.etalw)THEN
             CALL nl_get_start_month(grid%id,month)
             CALL nl_get_start_day(grid%id,iday)
             CALL gfdletainit(emiss,sfull,shalf,pptop,JULYR,MONTH,IDAY,GMT,&
                           config_flags,allowed_to_read,             &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )
             ENDIF

        CASE DEFAULT

   END SELECT swrad_select

   END SUBROUTINE ra_init

   SUBROUTINE bl_init(STEPBL,BLDT,DT,RUBLTEN,RVBLTEN,RTHBLTEN,  &
                RQVBLTEN,RQCBLTEN,RQIBLTEN,TSK,TMN,             &
                config_flags,restart,UST,LOWLYR,TSLB,ZS,DZS,    &
                num_soil_layers,TKE_MYJ,EXCH_H,VEGFRA,          &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,SMOIS,SMFR3D,                     &
                SNOWH,SH2O,FNDSOILW, FNDSNOWH,                  &
#if  ( NMM_CORE == 1 )
                Z0,XLAND,XICE,                                  &
#else
                ZNT,XLAND,XICE,                                 &
#endif
                SFCEVP,GRDFLX,                                  &
                allowed_to_read,                                &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )
!--------------------------------------------------------------------
   USE module_sf_sfclay
   USE module_sf_slab
   USE module_bl_ysu
   USE module_bl_mrf
   USE module_bl_gfs
   USE module_sf_myjsfc
   USE module_sf_noahlsm
   USE module_sf_ruclsm
   USE module_bl_myjpbl
#if (NMM_CORE == 1)
   USE module_sf_lsm_nmm
#endif
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart
   LOGICAL, INTENT(IN)         ::   FNDSOILW, FNDSNOWH

   INTEGER , INTENT(IN)        ::     ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)        ::     num_soil_layers

   REAL ,    INTENT(IN)        ::     DT, BLDT
   INTEGER , INTENT(INOUT)     ::     STEPBL

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),    &
             INTENT(OUT) :: SMFR3D

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),&
                   INTENT(INOUT) :: SMOIS,SH2O,TSLB 

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                           SNOW, &
                                                         SNOWH, &
                                                         SNOWC, &
                                                        CANWAT, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        ACSNOW, &
                                                        VEGFRA, &
                                                        ACSNOM, &
                                                        SFCEVP, &
                                                        GRDFLX, &
                                                           UST, &
#if ( NMM_CORE == 1 )
                                                            Z0, &
#else
                                                           ZNT, &
#endif
                                                         XLAND, &
                                                         XICE

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP, &
                                                        LOWLYR


   REAL,     DIMENSION(1:num_soil_layers), INTENT(INOUT)  ::  ZS,DZS

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                           RUBLTEN, &
                                                           RVBLTEN, &
 						          EXCH_H,   &
                                                          RTHBLTEN, &
                                                          RQVBLTEN, &
                                                          RQCBLTEN, &
                                                          RQIBLTEN, &
                                                          TKE_MYJ

   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::     TSK
   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::  TMN
   LOGICAL,  INTENT(IN)           :: allowed_to_read
   INTEGER :: isn, isfc
!--------------------------------------------------------------------

!-- calculate pbl time step

   STEPBL = nint(BLDT*60./DT)
   STEPBL = max(STEPBL,1)


!-- initialize surface layer scheme

   sfclay_select: SELECT CASE(config_flags%sf_sfclay_physics)

      CASE (SFCLAYSCHEME)
           CALL sfclayinit( allowed_to_read )
           isfc = 1
      CASE (MYJSFCSCHEME)
           CALL myjsfcinit(LOWLYR,UST,                         &
#if ( NMM_CORE == 1 )
                                      Z0,                      &
#else
                                      ZNT,                     &
#endif
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 2

      CASE (GFSSFCSCHEME)
#if ( NMM_CORE == 1 )
           CALL myjsfcinit(LOWLYR,UST,Z0,XLAND,XICE,           &
#else
           CALL myjsfcinit(LOWLYR,UST,ZNT,XLAND,XICE,           &
#endif
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 1

      CASE DEFAULT

   END SELECT sfclay_select


!-- initialize surface scheme

   sfc_select: SELECT CASE(config_flags%sf_surface_physics)

      CASE (SLABSCHEME)
           CALL slabinit(TSK,TMN,                              &
                         TSLB,ZS,DZS,num_soil_layers,          & 
                         restart,                              &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
#if (NMM_CORE == 1)
      CASE (NMMLSMSCHEME)
           CALL nmmlsminit(isn,XICE,VEGFRA,SNOW,SNOWC, CANWAT,SMSTAV, &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,     &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,DZS,SFCEVP,   &
                     TMN,                                          &
                     num_soil_layers,                              &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
#endif
      CASE (LSMSCHEME)
          CALL LSMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,  &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,        &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS, &
                     FNDSOILW, FNDSNOWH,                       &
                     num_soil_layers, restart,                 &
                     allowed_to_read ,                         &
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 )

      CASE (RUCLSMSCHEME)
!          if(isfc .ne. 2)CALL wrf_error_fatal &
!           ( 'module_physics_init: use myjsfc and myjpbl scheme for this lsm option' )
           CALL lsmrucinit( SMFR3D,TSLB,SMOIS,ISLTYP,              &
                     num_soil_layers, restart,                     &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

      CASE DEFAULT

   END SELECT sfc_select


!-- initialize pbl scheme

   pbl_select: SELECT CASE(config_flags%bl_pbl_physics)

      CASE (YSUSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
           CALL ysuinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (MRFSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
           CALL mrfinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (GFSSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
           CALL gfsinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (MYJPBLSCHEME)
           if(isfc .ne. 2)CALL wrf_error_fatal &
            ( 'module_physics_init: use myjsfc scheme for this pbl option' )
           CALL myjpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_MYJ,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE DEFAULT

   END SELECT pbl_select

   END SUBROUTINE bl_init

!==================================================================
   SUBROUTINE cu_init(STEPCU,CUDT,DT,RTHCUTEN,RQVCUTEN,RQRCUTEN,  &
                      RQCCUTEN,RQSCUTEN,RQICUTEN,NCA,RAINC,       &
                      RAINCV,W0AVG,config_flags,restart,          &
                      CLDEFI,LOWLYR,MASS_FLUX,                    &
                      RTHFTEN, RQVFTEN,                           &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
!------------------------------------------------------------------
   USE module_cu_kf
   USE module_cu_kfeta
   USE MODULE_CU_BMJ
   USE module_cu_gd
   USE module_cu_sas
!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart


   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   REAL ,    INTENT(IN)        :: DT, CUDT
   LOGICAL , INTENT(IN)        :: allowed_to_read
   INTEGER , INTENT(INOUT)     :: STEPCU

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &
            RTHCUTEN, RQVCUTEN, RQCCUTEN, RQRCUTEN, RQICUTEN,     &
            RQSCUTEN

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
            RTHFTEN, RQVFTEN

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: RAINC, RAINCV

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: CLDEFI

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: NCA

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: MASS_FLUX,   &
                                   APR_GR,APR_W,APR_MC,APR_ST,APR_AS,    &
                                   APR_CAPMA,APR_CAPME,APR_CAPMI

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: LOWLYR

! LOCAL VAR
   
  INTEGER :: i,j,itf,jtf

!--------------------------------------------------------------------

!-- calculate cumulus parameterization time step

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)
!
   STEPCU = nint(CUDT*60./DT)
   STEPCU = max(STEPCU,1)

!-- initialization

   IF(.not.restart)THEN
     DO j=jts,jtf
     DO i=its,itf
        RAINC(i,j)=0.
        RAINCV(i,j)=0.
     ENDDO
     ENDDO
   ENDIF

   cps_select: SELECT CASE(config_flags%cu_physics)

     CASE (KFSCHEME)
          CALL kfinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,        &
                      RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,      &
                      PARAM_FIRST_SCALAR,restart,                 &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (BMJSCHEME)
          CALL bmjinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,       &
                      CLDEFI,LOWLYR,cp,r_d,restart,               &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (KFETASCHEME)
          CALL kf_eta_init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,   &
                      RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,      &
                      SVP1,SVP2,SVP3,SVPT0,                       &
                      PARAM_FIRST_SCALAR,restart,                 &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
     CASE (GDSCHEME)
          CALL gdinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,        &
                      MASS_FLUX,cp,restart,                       &
                      P_QC,P_QI,PARAM_FIRST_SCALAR,               &
                      RTHFTEN, RQVFTEN,                           &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
     CASE (SASSCHEME)
          CALL sasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,       &
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE DEFAULT

   END SELECT cps_select

   END SUBROUTINE cu_init

!==================================================================
   SUBROUTINE mp_init(RAINNC,config_flags,restart,warm_rain,      &
                      MPDT, DT, DX, DY, LOWLYR,                   & ! for eta mp
                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           & ! for eta mp
                      mp_restart_state,tbpvs_state,tbpvs0_state,   & ! eta mp
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
!------------------------------------------------------------------
   USE module_mp_ncloud3
   USE module_mp_ncloud5
   USE module_mp_wsm3
   USE module_mp_wsm5
   USE module_mp_wsm6
   USE module_mp_etanew
   USE module_mp_thompson
!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
! Arguments
   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart
   LOGICAL , INTENT(OUT)       :: warm_rain
   REAL    , INTENT(IN)        :: MPDT, DT, DX, DY

   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   INTEGER , DIMENSION( ims:ime , jms:jme ) ,INTENT(INOUT)  :: LOWLYR
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: RAINNC
   REAL,     DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) :: &
                                  F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL , DIMENSION(:) ,INTENT(INOUT)  :: mp_restart_state,tbpvs_state,tbpvs0_state
   LOGICAL , INTENT(IN)  :: allowed_to_read

! Local
   INTEGER :: i, j, itf, jtf

   warm_rain = .false.
   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   IF(.not.restart)THEN
     DO j=jts,jtf
     DO i=its,itf
        RAINNC(i,j) = 0.
     ENDDO
     ENDDO
   ENDIF

   mp_select: SELECT CASE(config_flags%mp_physics)

     CASE (KESSLERSCHEME)
          warm_rain = .true.
     CASE (WSM3SCHEME)
          CALL wsm3init(rhoair0,rhowater,rhosnow,cliq,cv, allowed_to_read )
     CASE (WSM5SCHEME)
          CALL wsm5init(rhoair0,rhowater,rhosnow,cliq,cv, allowed_to_read )
     CASE (WSM6SCHEME)
          CALL wsm6init(rhoair0,rhowater,rhosnow,cliq,cv, allowed_to_read )
     CASE (ETAMPNEW)
         CALL etanewinit (MPDT,DT,DX,DY,LOWLYR,restart,           &
                          F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,       &
                          mp_restart_state,tbpvs_state,tbpvs0_state,&
                          allowed_to_read,                        &
                          ids, ide, jds, jde, kds, kde,           &
                          ims, ime, jms, jme, kms, kme,           &
                          its, ite, jts, jte, kts, kte            )
     CASE (THOMPSON)
         CALL thomp_init
     CASE (NCEPCLOUD3)
          CALL ncloud3init(rhoair0,rhowater,rhosnow,cliq,cv, allowed_to_read )
     CASE (NCEPCLOUD5)
          CALL ncloud5init(rhoair0,rhowater,rhosnow,cliq,cv, allowed_to_read )

     CASE DEFAULT

   END SELECT mp_select

   END SUBROUTINE mp_init

   SUBROUTINE z2sigma(zf,zh,sf,sh,p_top,pptop,config_flags, &
                allowed_to_read , &
                kds,kde,kms,kme,kts,kte)
   IMPLICIT NONE
! Arguments
   INTEGER, INTENT(IN) :: kds,kde,kms,kme,kts,kte
   REAL , DIMENSION( kms:kme ), INTENT(IN) :: zf,zh
   REAL , DIMENSION( kms:kme ), INTENT(OUT):: sf,sh
   REAL , INTENT(IN) :: p_top
   REAL , INTENT(OUT) :: pptop
   TYPE (grid_config_rec_type)              :: config_flags
   LOGICAL , INTENT(IN) :: allowed_to_read
! Local
   REAL R, G, TS, GAMMA, PS, ZTROP, TSTRAT, PTROP, Z, T, P, ZTOP, PTOP
   INTEGER K

   IF(zf(kde/2) .GT. 1.0)THEN
! Height levels assumed (zeta coordinate)
! Convert to sigma using standard atmosphere for pressure-height relation
! constants for standard atmosphere definition
      r=287.05
      g=9.80665
      ts=288.15
      gamma=-6.5/1000.
      ps=1013.25
      ztrop=11000.
      tstrat=ts+gamma*ztrop
      ptrop=ps*(tstrat/ts)**(-g/(gamma*r))

      do k=kde,kds,-1
! full levels
        z=zf(k)
        if(z.le.ztrop)then
          t=ts+gamma*z
          p=ps*(t/ts)**(-g/(gamma*r))
        else
          t=tstrat
          p=ptrop*exp(-g*(z-ztrop)/(r*tstrat))
        endif
        if(k.eq.kde)then
          ztop=zf(k)
          ptop=p
        endif
        sf(k)=(p-ptop)/(ps-ptop)
! half levels
        if(k.ne.kds)then
        z=0.5*(zf(k)+zf(k-1))
        if(z.le.ztrop)then
          t=ts+gamma*z
          p=ps*(t/ts)**(-g/(gamma*r))
        else
          t=tstrat
          p=ptrop*exp(-g*(z-ztrop)/(r*tstrat))
        endif
        sh(k-1)=(p-ptop)/(ps-ptop)
        endif
      enddo
      pptop=ptop/10.
   ELSE
!  Levels are already sigma/eta
      do k=kde,kds,-1
!        sf(k)=zf(kde-k+kds)
!        if(k .ne. kde)sh(k)=zh(kde-1-k+kds)
         sf(k)=zf(k)
         if(k .ne. kde)sh(k)=zh(k)
      enddo
      pptop=p_top/1000.

   ENDIF

   END SUBROUTINE z2sigma

END MODULE module_physics_init
