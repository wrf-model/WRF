!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_pbl_driver
CONTAINS

!------------------------------------------------------------------
   SUBROUTINE pbl_driver(itimestep,dt,u_frame,v_frame,            &
                  RUBLTEN,RVBLTEN,RTHBLTEN,                       &
                  RQVBLTEN,RQCBLTEN,RQIBLTEN,                     &
                  TSK,XLAND,ZNT,HT,                               &
                  UST,HOL,MOL,PBLH,                               &
                  HFX,QFX,REGIME,                                 &
                  GRDFLX,                                         &
                  u_phy,v_phy,th_phy,rho,moist,                   &
                  p_phy,pi_phy,p8w,t_phy,dz8w,z,                  &
                  TKE_MYJ,AKHS,AKMS,                              &
                  THZ0,QZ0,UZ0,VZ0,QSFC,                          &
                  LOWLYR,                                         &
                  PSIM,PSIH,GZ1OZ0, WSPD,BR,CHKLOWQ,              &  !m  26 Aug 2002
                  config_flags,DX,n_moist,                        & !TSLB (STEMP)
                  STEPBL,warm_rain,                               &
                  KPBL,CT,LH,SNOW,XICE,                           &
                  ids,ide, jds,jde, kds,kde,                      &
                  ims,ime, jms,jme, kms,kme,                      &
                  i_start,i_end, j_start,j_end, kts,kte, num_tiles)
!------------------------------------------------------------------
   USE module_bc
   USE module_state_description
   USE module_model_constants

! *** add new modules of schemes here

   USE module_bl_myjpbl
   USE module_bl_ysu
   USE module_bl_mrf

   !  This driver calls subroutines for the PBL parameterizations.
   !
   !  pbl scheme:
   !      1. ysupbl
   !      2. myjpbl
   !      99. mrfpbl
   !
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
!         .
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
! Rho_d      dry density (kg/m^3)
! Theta_m    moist potential temperature (K)
! Qv         water vapor mixing ratio (kg/kg)
! Qc         cloud water mixing ratio (kg/kg)
! Qr         rain water mixing ratio (kg/kg)
! Qi         cloud ice mixing ratio (kg/kg)
! Qs         snow mixing ratio (kg/kg)
!-----------------------------------------------------------------
!-- RUBLTEN       U tendency due to 
!                 PBL parameterization (m/s^2)
!-- RVBLTEN       V tendency due to 
!                 PBL parameterization (m/s^2)
!-- RTHBLTEN      Theta tendency due to 
!                 PBL parameterization (K/s)
!-- RQVBLTEN      Qv tendency due to 
!                 PBL parameterization (kg/kg/s)
!-- RQCBLTEN      Qc tendency due to 
!                 PBL parameterization (kg/kg/s)
!-- RQIBLTEN      Qi tendency due to 
!                 PBL parameterization (kg/kg/s)
!-- itimestep     number of time steps
!-- GLW           downward long wave flux at ground surface (W/m^2)
!-- GSW           downward short wave flux at ground surface (W/m^2)
!-- EMISS         surface emissivity (between 0 and 1)
!-- TSK           surface temperature (K)
!-- TMN           soil temperature at lower boundary (K)
!-- XLAND         land mask (1 for land, 2 for water)
!-- ZNT           roughness length (m)
!-- MAVAIL        surface moisture availability (between 0 and 1)
!-- UST           u* in similarity theory (m/s)
!-- MOL           q* (similarity theory) (kg/kg)
!-- HOL           PBL height over Monin-Obukhov length
!-- PBLH          PBL height (m)
!-- CAPG          heat capacity for soil (J/K/m^3)
!-- THC           thermal inertia (Cal/cm/K/s^0.5)
!-- SNOWC         flag indicating snow coverage (1 for snow cover)
!-- HFX           upward heat flux at the surface (W/m^2)
!-- QFX           upward moisture flux at the surface (kg/m^2/s)
!-- REGIME        flag indicating PBL regime (stable, unstable, etc.)
!-- tke_myj       turbulence kinetic energy from Mellor-Yamada-Janjic (MYJ) (m^2/s^2)
!-- akhs          sfc exchange coefficient of heat/moisture from MYJ
!-- akms          sfc exchange coefficient of momentum from MYJ
!-- thz0          potential temperature at roughness length (K)
!-- uz0           u wind component at roughness length (m/s)
!-- vz0           v wind component at roughness length (m/s)
!-- qsfc          specific humidity at lower boundary (kg/kg)
!-- th2           diagnostic 2-m theta from surface layer and lsm
!-- t2            diagnostic 2-m temperature from surface layer and lsm
!-- q2            diagnostic 2-m mixing ratio from surface layer and lsm
!-- lowlyr        index of lowest model layer above ground
!-- rr            dry air density (kg/m^3)
!-- u_phy         u-velocity interpolated to theta points (m/s)
!-- v_phy         v-velocity interpolated to theta points (m/s)
!-- th_phy        potential temperature (K)
!-- moist         moisture array (4D - last index is species) (kg/kg)
!-- p_phy         pressure (Pa)
!-- pi_phy        exner function (dimensionless)
!-- p8w           pressure at full levels (Pa)
!-- t_phy         temperature (K)
!-- dz8w          dz between full levels (m)
!-- z             height above sea level (m)
!-- config_flags
!-- DX            horizontal space interval (m)
!-- DT            time step (second)
!-- n_moist       number of moisture species
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
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!
!******************************************************************
!------------------------------------------------------------------ 
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
!



   INTEGER,    INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       kts,kte, num_tiles,        &
                                       n_moist           

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                   &
  &                                    i_start,i_end,j_start,j_end

   INTEGER,    INTENT(IN   )    ::     itimestep,STEPBL
   INTEGER,    DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN   )    ::                        LOWLYR
!
   LOGICAL,      INTENT(IN   )    ::   warm_rain
!
   REAL,       INTENT(IN   )    ::     DT,DX


!
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(IN   )    ::                         p_phy, &
                                                          pi_phy, &
                                                             p8w, &
                                                             rho, &
                                                           t_phy, &
                                                           u_phy, &
                                                           v_phy, &
                                                            dz8w, &
                                                               z, &
                                                          th_phy
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ),         &
         INTENT(IN ) ::                                    moist
!
!
   REAL,       DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN   )    ::                         XLAND, &
                                                              HT, &
                                                            PSIM, &
                                                            PSIH, &
                                                          GZ1OZ0, &
                                                              BR, &
                                                         CHKLOWQ
!
   REAL,       DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)    ::                           TSK, &
                                                             UST, &
                                                             HOL, &
                                                             MOL, &
                                                            PBLH, &
                                                             HFX, &
                                                             QFX, &
                                                          REGIME, &
                                                             ZNT, &
                                                            QSFC, &
                                                            AKHS, &
                                                            AKMS, &
                                                             QZ0, &
                                                            THZ0, &
                                                             UZ0, &
                                                             VZ0, &
                                                          GRDFLX  , &
                                                            WSPD

!
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(INOUT)    ::                       RUBLTEN, &
                                                         RVBLTEN, &
                                                        RTHBLTEN, &
                                                        RQVBLTEN, &
                                                        RQCBLTEN, &
                                                        RQIBLTEN, &
                                                          TKE_MYJ

   REAL ,                             INTENT(IN   )  ::  u_frame, &
                                                         v_frame
!

   INTEGER,    DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(INOUT) ::                             KPBL

   REAL,       DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN)    :: XICE, CT, SNOW, LH

!  LOCAL  VAR

   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::v_phytmp
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::u_phytmp

   REAL,       DIMENSION( ims:ime, jms:jme )          ::  TSKOLD, &
                                                          USTOLD, &
                                                          ZNTOLD, &
                                                             ZOL, &
                                                            PSFC

!

   REAL    :: DTMIN,DTBL
!
   INTEGER :: i,J,K,NK,jj,ij
   LOGICAL :: radiation

!------------------------------------------------------------------
!


  if (config_flags%bl_pbl_physics .eq. 0) return
! RAINBL in mm (Accumulation between PBL calls)


  IF (itimestep .eq. 1 .or. mod(itimestep,STEPBL) .eq. 0) THEN

  radiation = .false.
  IF (config_flags%ra_lw_physics .gt. 0) radiation = .true.

!---- 
! CALCULATE CONSTANT
 
   DTMIN=DT/60.
! PBL schemes need PBL time step for updates
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

! REVERSE ORDER IN THE VERTICAL DIRECTION

! testing change later

         DO k=kts,kte
            v_phytmp(i,k,j)=v_phy(i,k,j)+v_frame
            u_phytmp(i,k,j)=u_phy(i,k,j)+u_frame
         ENDDO

! PSFC : in Pa

         PSFC(I,J)=p8w(I,kms,J)

         DO k=kts,min(kte+1,kde)
            RTHBLTEN(I,K,J)=0.
            RUBLTEN(I,K,J)=0.
            RVBLTEN(I,K,J)=0.
            RQCBLTEN(I,K,J)=0.
            RQVBLTEN(I,K,J)=0.
         ENDDO

         IF (P_QI .ge. PARAM_FIRST_SCALAR) THEN
            DO k=kts,min(kte+1,kde)
               RQIBLTEN(I,K,J)=0.
            ENDDO
         ENDIF
      ENDDO
      ENDDO

   ENDDO

!
  !$OMP PARALLEL DO   &
  !$OMP PRIVATE ( ij )
  DO ij = 1 , num_tiles
   pbl_select: SELECT CASE(config_flags%bl_pbl_physics)

      CASE (YSUSCHEME)
        CALL wrf_debug(100,'in YSU PBL')
           CALL YSU(u_phytmp,v_phytmp,th_phy,t_phy,                 &
               moist(ims,kms,jms,P_QV),moist(ims,kms,jms,P_QC),     &
               moist(ims,kms,jms,P_QI),p_phy,pi_phy,                &
               RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,RQIBLTEN, &
               CP,G,RCP,R_d,ROVG,P_QI,PARAM_FIRST_SCALAR,           &
               dz8w,z,XLV,R_v,PSFC,                                 &
               ZNT,UST,ZOL,HOL,PBLH,REGIME,PSIM,PSIH,               &
               XLAND,HFX,QFX,TSKOLD,GZ1OZ0,WSPD,BR,                 &
               DTBL,DTMIN,KPBL,                                     &
               SVP1,SVP2,SVP3,SVPT0,EP_1,EP_2,KARMAN,EOMEG,STBOLT,  &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               i_start(ij),i_end(ij),j_start(ij),j_end(ij),kts,kte  )

      CASE (MRFSCHEME)
        CALL wrf_debug(100,'in MRF')
           CALL MRF(u_phytmp,v_phytmp,th_phy,t_phy,                 &
               moist(ims,kms,jms,P_QV),moist(ims,kms,jms,P_QC),     &
               moist(ims,kms,jms,P_QI),p_phy,pi_phy,                &
               RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,RQIBLTEN, &
               CP,G,RCP,R_d,ROVG,P_QI,PARAM_FIRST_SCALAR,           &
               dz8w,z,XLV,R_v,PSFC,                                 &
               ZNT,UST,ZOL,HOL,PBLH,REGIME,PSIM,PSIH,               &
               XLAND,HFX,QFX,TSKOLD,GZ1OZ0,WSPD,BR,                 &
               DTBL,DTMIN,KPBL,                                     &
               SVP1,SVP2,SVP3,SVPT0,EP_1,EP_2,KARMAN,EOMEG,STBOLT,  &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               i_start(ij),i_end(ij),j_start(ij),j_end(ij),kts,kte  )

      CASE (MYJPBLSCHEME)
        CALL wrf_debug(100,'in MYJPBL')

             CALL MYJPBL(DT,STEPBL,ht,dz8w,                           &
               p_phy,p8w,th_phy,t_phy,pi_phy,                       &
               moist(ims,kms,jms,P_QV),moist(ims,kms,jms,P_QC),     &
               u_phy,v_phy,rho,                                     &
               TSK,QSFC,CHKLOWQ,THZ0,QZ0,UZ0,VZ0,                   &
               LOWLYR,                                              &
               XLAND,XICE,SNOW,                                     &
               TKE_MYJ,UST,ZNT,PBLH,KPBL,CT,                        &
               AKHS,AKMS,LH,                                        &
               RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,          &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               i_start(ij),i_end(ij),j_start(ij),j_end(ij),kts,kte  )

     CASE DEFAULT

       WRITE( wrf_err_message , * ) 'The pbl option does not exist: bl_pbl_physics = ', config_flags%bl_pbl_physics
       CALL wrf_error_fatal ( wrf_err_message )

   END SELECT pbl_select

   ENDDO


   ENDIF

!
   END SUBROUTINE pbl_driver
END MODULE module_pbl_driver
