!WRF:MEDIATION_LAYER:PHYSICS
!
MODULE module_surface_driver
CONTAINS

   SUBROUTINE surface_driver(                                         &
     &           ACSNOM,ACSNOW,AKHS,AKMS,ALBEDO,BR,CANWAT,CAPG        &
     &          ,CHKLOWQ,CONFIG_FLAGS,DT,DX,DZ8W,DZS,EMISS,GLW        &
     &          ,GRDFLX,GSW,GZ1OZ0,HFX,HOL,HT,IFSNOW,ISFFLX           &
     &          ,ISLTYP,ITIMESTEP,IVGTYP,LOWLYR,MAVAIL,MOIST,MOL      &
     &          ,NUM_SOIL_LAYERS,N_MOIST,P8W,PBLH,PI_PHY,PSHLTR,PSIH  &
     &          ,PSIM,P_PHY,Q10,Q2,QFX,QSFC,QSHLTR,QZ0,RAINBL         &
     &          ,RAINCV,RAINNCV,REGIME,RHO,SFCEVP,SFCEXC,SFCRUNOFF    &
     &          ,SMOIS,SMSTAV,SMSTOT,SNOALB,SNOW,SNOWC,SNOWH,STEPBL   &
     &          ,T2,TH10,TH2,THC,THZ0,TH_PHY,TMN,TSHLTR,TSK,TSLB      &
     &          ,T_PHY,U10,UDRUNOFF,UST,UZ0,U_FRAME,U_PHY,V10,VEGFRA  &
     &          ,VZ0,V_FRAME,V_PHY,WARM_RAIN,WSPD,XICE,XLAND,Z,ZNT,ZS &
     &          ,CT,TKE_MYJ                                           &
     &          ,ALBBCK,LH,SH2O,SHDMAX,SHDMIN,Z0                      &
     &          ,FLQC,FLHC,QSG,QVG,QCG,SOILT1,TSNAV                   & ! RUC LSM only
     &          ,SMFR3D,KEEPFR3DFLAG                                  &
     &          ,POTEVP,SNOPCX,SOILTB                                 & ! NMM LSM only
     &          ,PSFC                                                 &
     &          ,ids,ide,jds,jde,kds,kde                              &
     &          ,ims,ime,jms,jme,kms,kme                              &
     &          ,i_start,i_end,j_start,j_end,kts,kte,num_tiles        )
!    &          ,ALBSF,POTEVP,PSHLTR,SMLIQ,SNOPCX,TBOT                &
!    &          ,ELFLX                                                )

   USE module_bc
   USE module_state_description
   USE module_model_constants

! *** add new modules of schemes here

   USE module_sf_sfclay
   USE module_sf_myjsfc
   USE module_sf_noahlsm
   USE module_sf_ruclsm
#if ( NMM_CORE==1)
   USE module_sf_lsm_nmm
#endif

   USE module_sf_slab
!
   USE module_sf_sfcdiags
!

   !  This driver calls subroutines for the surface parameterizations.
   !
   !  surface layer: (between surface and pbl)
   !      1. sfclay
   !      2. myjsfc
   !  surface: ground temp/lsm scheme:
   !      1. slab
   !      2. Noah LSM
   !      99. NMM LSM (NMM core only)
!------------------------------------------------------------------
   IMPLICIT NONE
!======================================================================
! Grid structure in physics part of WRF
!----------------------------------------------------------------------
! The horizontal velocities used in the physics are unstaggered
! relative to temperature/moisture variables. All predicted
! variables are carried at half levels except w, which is at full
! levels. Some arrays with names (*8w) are at w (full) levels.
!
!----------------------------------------------------------------------
! In WRF, kms (smallest number) is the bottom level and kme (largest
! number) is the top level.  In your scheme, if 1 is at the top level,
! then you have to reverse the order in the k direction.
!
!         kme      -   half level (no data at this level)
!         kme    ----- full level
!         kme-1    -   half level
!         kme-1  ----- full level
!         .
!         kms+2    -   half level
!         kms+2  ----- full level
!         kms+1    -   half level
!         kms+1  ----- full level
!         kms      -   half level
!         kms    ----- full level
!
!======================================================================
! Definitions
!-----------
! Theta      potential temperature (K)
! Qv         water vapor mixing ratio (kg/kg)
! Qc         cloud water mixing ratio (kg/kg)
! Qr         rain water mixing ratio (kg/kg)
! Qi         cloud ice mixing ratio (kg/kg)
! Qs         snow mixing ratio (kg/kg)
!-----------------------------------------------------------------
!-- itimestep     number of time steps
!-- GLW           downward long wave flux at ground surface (W/m^2)
!-- GSW           downward short wave flux at ground surface (W/m^2)
!-- EMISS         surface emissivity (between 0 and 1)
!-- TSK           surface temperature (K)
!-- TMN           soil temperature at lower boundary (K)
!-- XLAND         land mask (1 for land, 2 for water)
!-- ZNT           time-varying roughness length (m)
!-- Z0            background roughness length (m)
!-- MAVAIL        surface moisture availability (between 0 and 1)
!-- UST           u* in similarity theory (m/s)
!-- MOL           q* (similarity theory) (kg/kg)
!-- HOL           PBL height over Monin-Obukhov length
!-- PBLH          PBL height (m)
!-- CAPG          heat capacity for soil (J/K/m^3)
!-- THC           thermal inertia (Cal/cm/K/s^0.5)
!-- SNOWC         flag indicating snow coverage (1 for snow cover)
!-- HFX           net upward heat flux at the surface (W/m^2)
!-- QFX           net upward moisture flux at the surface (kg/m^2/s)
!-- LH            net upward latent heat flux at surface (W/m^2)
!-- REGIME        flag indicating PBL regime (stable, unstable, etc.)
!-- tke_myj       turbulence kinetic energy from Mellor-Yamada-Janjic (MYJ) (m^2/s^2)
!-- akhs          sfc exchange coefficient of heat/moisture from MYJ
!-- akms          sfc exchange coefficient of momentum from MYJ
!-- thz0          potential temperature at roughness length (K)
!-- uz0           u wind component at roughness length (m/s)
!-- vz0           v wind component at roughness length (m/s)
!-- qsfc          specific humidity at lower boundary (kg/kg)
!-- u10           diagnostic 10-m u component from surface layer
!-- v10           diagnostic 10-m v component from surface layer
!-- th2           diagnostic 2-m theta from surface layer and lsm
!-- t2            diagnostic 2-m temperature from surface layer and lsm
!-- q2            diagnostic 2-m mixing ratio from surface layer and lsm
!-- tshltr        diagnostic 2-m theta from MYJ
!-- th10          diagnostic 10-m theta from MYJ
!-- qshltr        diagnostic 2-m specific humidity from MYJ
!-- q10           diagnostic 10-m specific humidity from MYJ
!-- lowlyr        index of lowest model layer above ground
!-- rr            dry air density (kg/m^3)
!-- u_phy         u-velocity interpolated to theta points (m/s)
!-- v_phy         v-velocity interpolated to theta points (m/s)
!-- th_phy        potential temperature (K)
!-- moist         moisture array (4D - last index is species) (kg/kg)
!-- p_phy         pressure (Pa)
!-- pi_phy        exner function (dimensionless)
!-- pshltr        diagnostic shelter (2m) pressure from MYJ (Pa)
!-- p8w           pressure at full levels (Pa)
!-- t_phy         temperature (K)
!-- dz8w          dz between full levels (m)
!-- z             height above sea level (m)
!-- config_flags
!-- DX            horizontal space interval (m)
!-- DT            time step (second)
!-- PSFC          pressure at the surface (Pa)
!-- TSLB          
!-- ZS
!-- DZS
!-- num_soil_layers number of soil layer
!-- IFSNOW      ifsnow=1 for snow-cover effects
!
!-- P_QV          species index for water vapor
!-- P_QC          species index for cloud water
!-- P_QR          species index for rain water
!-- P_QI          species index for cloud ice
!-- P_QS          species index for snow
!-- P_QG          species index for graupel
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- its           start index for i in tile
!-- ite           end index for i in tile
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!
!******************************************************************
!------------------------------------------------------------------ 

   INTEGER, INTENT(IN) ::                                             &
     &           ids,ide,jds,jde,kds,kde                              &
     &          ,ims,ime,jms,jme,kms,kme                              &
     &          ,kts,kte,num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   TYPE(grid_config_rec_type), INTENT(IN )::   CONFIG_FLAGS
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   ISLTYP
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   IVGTYP
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   LOWLYR
   INTEGER, INTENT(IN )::   IFSNOW
   INTEGER, INTENT(IN )::   ISFFLX
   INTEGER, INTENT(IN )::   ITIMESTEP
   INTEGER, INTENT(IN )::   NUM_SOIL_LAYERS
   INTEGER, INTENT(IN )::   N_MOIST
   INTEGER, INTENT(IN )::   STEPBL
   LOGICAL, INTENT(IN )::   WARM_RAIN
   REAL , INTENT(IN )::   U_FRAME
   REAL , INTENT(IN )::   V_FRAME
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   SMOIS
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   TSLB
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   SMFR3D
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   KEEPFR3DFLAG
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   GLW
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   GSW
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   HT
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   RAINCV
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   RAINNCV
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   THC
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   TMN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   VEGFRA
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   XICE
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   XLAND
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   MAVAIL
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   EMISS
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   SNOALB
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::   T2
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ACSNOW
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   AKHS
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   AKMS
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ALBEDO
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   CANWAT
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   CAPG
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   GRDFLX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   HFX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   HOL
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   MOL
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   PBLH
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   Q2
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QFX
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QSFC
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   QZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   RAINBL
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   REGIME
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SFCRUNOFF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SMSTAV
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SMSTOT
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOW
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOWC
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOWH
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   TH2
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   THZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   TSK
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UDRUNOFF
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UST
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   UZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   VZ0
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   WSPD
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ZNT
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   POTEVP ! NMM LSM
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SNOPCX ! NMM LSM
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   SOILTB ! NMM LSM
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   BR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   CHKLOWQ
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   GZ1OZ0
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSIH
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSIM
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   Q10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   QSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   TH10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   TSHLTR
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   U10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   V10
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)::   PSFC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   ACSNOM
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   SFCEVP
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   SFCEXC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   FLHC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   FLQC
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   QSG
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   QVG
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   QCG
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   SOILT1
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)::   TSNAV
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::   CT
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   DZ8W
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   P8W
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   PI_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   P_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   RHO
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   TH_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   T_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   U_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   V_PHY
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN )::   Z
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::   TKE_MYJ
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ) , INTENT(IN )::   MOIST
   REAL, DIMENSION(1:num_soil_layers), INTENT(IN)::   DZS
   REAL, DIMENSION(1:num_soil_layers), INTENT(IN)::   ZS
   REAL, INTENT(IN )::   DT
   REAL, INTENT(IN )::   DX

!  arguments for NCAR surface physics

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   ALBBCK  ! INOUT needed for NMM
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT )::   LH
   REAL, DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), INTENT(INOUT)::   SH2O
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   SHDMAX
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   SHDMIN
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   Z0

!  LOCAL  VAR

   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::v_phytmp
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::u_phytmp

   REAL,       DIMENSION( ims:ime, jms:jme )          ::  TSKOLD, &
                                                          USTOLD, &
                                                          ZNTOLD, &
                                                             ZOL

   REAL,       DIMENSION( ims:ime, jms:jme )          ::          &
                                                             QGH, &
                                                             CHS, &
                                                             CPM, &
                                                            CHS2

   REAL    :: DTMIN,DTBL
!
   INTEGER :: i,J,K,NK,jj,ij
   LOGICAL :: radiation
!
!
!------------------------------------------------------------------
!
  if (config_flags%sf_sfclay_physics .eq. 0) return

! RAINBL in mm (Accumulation between PBL calls)

  !$OMP PARALLEL DO   &
  !$OMP PRIVATE ( ij )
  DO ij = 1 , num_tiles
    DO j=j_start(ij),j_end(ij)
    DO i=i_start(ij),i_end(ij)
       RAINBL(i,j) = RAINBL(i,j) + RAINCV(i,j) + RAINNCV(i,j) 
    ENDDO
    ENDDO
  ENDDO

  IF (itimestep .eq. 1 .or. mod(itimestep,STEPBL) .eq. 0) THEN

  radiation = .false.
  IF (config_flags%ra_lw_physics .gt. 0) radiation = .true.

!---- 
! CALCULATE CONSTANT
 
     DTMIN=DT/60.
! Surface schemes need PBL time step for updates and accumulations
! Assume these schemes provide no tendencies
     DTBL=DT*STEPBL

! SAVE OLD VALUES


     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , num_tiles
       DO j=j_start(ij),j_end(ij)
       DO i=i_start(ij),i_end(ij)
          TSKOLD(i,j)=TSK(i,j)
          USTOLD(i,j)=UST(i,j)
          ZNTOLD(i,j)=ZNT(i,j)
! PSFC : in Pa
          PSFC(I,J)=p8w(I,kts,J)
! REVERSE ORDER IN THE VERTICAL DIRECTION
          DO k=kts,kte
            v_phytmp(i,k,j)=v_phy(i,k,j)+v_frame
            u_phytmp(i,k,j)=u_phy(i,k,j)+u_frame
          ENDDO
       ENDDO
       ENDDO
     ENDDO


     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , num_tiles
     sfclay_select: SELECT CASE(config_flags%sf_sfclay_physics)

     CASE (SFCLAYSCHEME)
       CALL wrf_debug( 100, 'in SFCLAY' )
         CALL SFCLAY(u_phytmp,v_phytmp,t_phy,moist(ims,kms,jms,P_QV),&
               p_phy,dz8w,CP,G,RCP,R_d,XLV,PSFC,CHS,CHS2,CPM,      &
               ZNTOLD,USTOLD,PBLH,MAVAIL,ZOL,MOL,REGIME,PSIM,PSIH, &
               XLAND,HFX,QFX,LH,TSK,FLHC,FLQC,QGH,QSFC,            &
               U10,V10,TH2,T2,Q2,                                  &
               GZ1OZ0,WSPD,BR,ISFFLX,DX,                           &
               SVP1,SVP2,SVP3,SVPT0,EP_1,EP_2,KARMAN,EOMEG,STBOLT, &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
      CASE (MYJSFCSCHEME)

          CALL wrf_debug(100,'in MYJSFC')
            CALL MYJSFC(itimestep,ht,dz8w,                         &
              p_phy,p8w,th_phy,t_phy,                              &
              moist(ims,kms,jms,P_QV),moist(ims,kms,jms,P_QC),     &
              u_phy,v_phy,tke_myj,                                 &
              TSK,QSFC,THZ0,QZ0,UZ0,VZ0,                           &
              LOWLYR,                                              &
              XLAND,                                               &
              UST,ZNT,PBLH,MAVAIL,                                 &
              AKHS,AKMS,                                           &
              CHS,CHS2,HFX,QFX,LH,FLHC,FLQC,QGH,CPM,CT,            &
              U10,V10,TSHLTR,TH10,QSHLTR,Q10,PSHLTR,               &
              ids,ide, jds,jde, kds,kde,                           &
              ims,ime, jms,jme, kms,kme,                           &
              i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

     CASE DEFAULT
        
       WRITE( wrf_err_message , * ) 'The sfclay option does not exist: sf_sfclay_physics = ', config_flags%sf_sfclay_physics
       CALL wrf_error_fatal ( wrf_err_message )

     END SELECT sfclay_select
     ENDDO

     IF (ISFFLX.EQ.0 ) GOTO 430
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , num_tiles

     sfc_select: SELECT CASE(config_flags%sf_surface_physics)

     CASE (SLABSCHEME)
    

        CALL wrf_debug(100,'in SLAB')
          CALL SLAB(t_phy,moist(ims,kms,jms,P_QV),p_phy,FLHC,FLQC,  &
             PSFC,XLAND,TMN,HFX,QFX,LH,TSK,QSFC,CHKLOWQ,          &
             GSW,GLW,CAPG,THC,SNOWC,EMISS,MAVAIL,                 &
             DTBL,RCP,XLV,DTMIN,IFSNOW,                           &
             SVP1,SVP2,SVP3,SVPT0,EP_2,KARMAN,EOMEG,STBOLT,       &
             TSLB,ZS,DZS,num_soil_layers,radiation,               &
             ids,ide, jds,jde, kds,kde,                           &
             ims,ime, jms,jme, kms,kme,                           &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

        CALL wrf_debug(100,'in SFCDIAGS')
          CALL SFCDIAGS(HFX,QFX,TSK,QSFC,CHS2,T2,TH2,Q2,              &
                     PSFC,CP,R_d,RCP,                              &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
             i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )


#if ( NMM_CORE==1)
     CASE (NMMLSMSCHEME)
         CALL wrf_debug(100,'in NMM LSM')
           CALL nmmlsm(dz8w,moist(ims,kms,jms,P_QV),p8w,rho,       &
                t_phy,th_phy,TSK,CHS,                           &
                HFX,QFX,QGH,GSW,GLW,LH,                         &
                SMSTAV,SMSTOT,SFCRUNOFF,                        &
                UDRUNOFF,IVGTYP,ISLTYP,VEGFRA,SFCEVP,POTEVP,    &
                GRDFLX,SFCEXC,ACSNOW,ACSNOM,SNOPCX,             &
                ALBBCK,TMN,XLAND,XICE,QZ0,                      &
                th2,q2,SNOWC,CHS2,QSFC,SOILTB,CHKLOWQ,RAINBL,   &
                num_soil_layers,DTBL,DZS,itimestep,             &
                SMOIS,TSLB,SNOW,CANWAT,CPM,RCP,                 &    
                ALBEDO,SNOALB,SH2O,SNOWH,                       &
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
#endif
     CASE (LSMSCHEME)

!------------------------------------------------------------------
         CALL wrf_debug(100,'in NOAH LSM')
           CALL lsm(dz8w,moist(ims,kms,jms,P_QV),p8w,t_phy,TSK,  &
                HFX,QFX,LH,GRDFLX,QGH,GSW,GLW,SMSTAV,SMSTOT,    &
                SFCRUNOFF,UDRUNOFF,IVGTYP,ISLTYP,VEGFRA,        &
                ALBEDO,ALBBCK,ZNT,Z0, TMN,XLAND,XICE, EMISS,    &
                SNOWC,QSFC,RAINBL,                              & 
                num_soil_layers,DTBL,DZS,itimestep,             &
                SMOIS,TSLB,SNOW,CANWAT,CHS, CPM,RCP,            &    
                SH2O,SNOWH,                                     & !H  
                SNOALB,SHDMIN,SHDMAX,                           & !I
                ACSNOW,ACSNOW,                                  & !O 
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              CHKLOWQ(I,J)= 1.0
           ENDDO
           ENDDO
         

         CALL SFCDIAGS(HFX,QFX,TSK,QSFC,CHS2,T2,TH2,Q2,              &
                     PSFC,CP,R_d,RCP,                              &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )
!------------------------------------------------------------------

     CASE (RUCLSMSCHEME)
         CALL wrf_debug(100,'in RUC LSM')
           CALL LSMRUC(DTBL,itimestep,num_soil_layers,          &
                ZS,RAINBL,SNOW,SNOWC,                           &
                dz8w,P8W,T_PHY,moist(ims,kms,jms,p_qv),         & !p8W in [PA]
                moist(ims,kms,jms,P_QC),RHO,                    &
                GLW,GSW,EMISS,CHKLOWQ,                          &
                FLQC,FLHC,MAVAIL,CANWAT,VEGFRA,ALBEDO,ZNT,      &
                QSFC,QSG,QVG,QCG,SOILT1,TSNAV,                  &
                TMN,IVGTYP,ISLTYP,XLAND,XICE,                   &
                CP,G,XLV,STBOLT,                                &
                SMOIS,SMSTAV,SMSTOT,TSLB,TSK,HFX,QFX,LH,        &
                SFCRUNOFF,UDRUNOFF,SFCEXC,                      &
                SFCEVP,GRDFLX,ACSNOW,                           &
                SMFR3D,KEEPFR3DFLAG,                            &
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
              CHKLOWQ(I,J)= 1.0
           ENDDO
           ENDDO

         CALL SFCDIAGS(HFX,QFX,TSK,QSFC,CHS2,T2,TH2,Q2,     &
              PSFC,CP,R_d,RCP,                              &
              ids,ide, jds,jde, kds,kde,                    &
              ims,ime, jms,jme, kms,kme,                    &
              i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte    )

     CASE DEFAULT

       WRITE( wrf_err_message , * ) 'The surface option does not exist: sf_surface_physics = ', config_flags%sf_surface_physics
       CALL wrf_error_fatal ( wrf_err_message )

     END SELECT sfc_select
     ENDDO

 430 CONTINUE


! Reset RAINBL in mm (Accumulation between PBL calls)

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , num_tiles
       DO j=j_start(ij),j_end(ij)
       DO i=i_start(ij),i_end(ij)
          RAINBL(i,j) = 0.
       ENDDO
       ENDDO
     ENDDO

   ENDIF

   END SUBROUTINE surface_driver

END MODULE module_surface_driver

