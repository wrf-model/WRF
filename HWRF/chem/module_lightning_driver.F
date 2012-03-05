!WRF:MEDIATION_LAYER:PHYSICS
! *** add new modules of schemes here
!
MODULE module_lightning_driver
CONTAINS

SUBROUTINE lightning_driver(                                            &
                       t_phy, p_phy, rho, u, v, w                       &
                      ,ht, xlat, xlon, dx, dy, z                        &
                      ,moist                                            &
                      ,ids,ide, jds,jde, kds,kde                        &
                      ,ims,ime, jms,jme, kms,kme                        &
                      ,its,ite, jts,jte, kts,kte                        &
                      ,c_no, s_ic, s_cg                                 &
                      ,tracer_opt                                       &
                      ,lightning,itimestep                              &
                      ,time_step,lightning_time_step                    &
                      ,lightning_start_seconds, passive_ltng            &
                      ,temp_upper, temp_lower, N_IC, N_CG               &
                      ,lflash_data, flashrate_factor                    &
                      ,flashrate_method, iccg_method                    &
                      ,cellcount_method                                 &
                      ,ic_fr, cg_fr                                     &
                      ,xland,ktop )

! Framework
   USE module_state_description

! Driver layer modules
!  USE module_dm

! Model Layer
   USE module_model_constants
   USE module_wrf_error

! *** add new modules of schemes here

   USE module_ltng_crm
   USE module_ltng_cpm
    
!----------------------------------------------------------------------
   ! This driver calls subroutines for the lightning-NOx parameterization.
   !
   ! Schemes
   !
   ! DeCaria et al. (2005) scheme  for cloud resolving models 
   !  -- implemented by Lesley Ott
   ! Barthe 2007 scheme for cloud resolving models
   ! Price and Rind (1993) scheme for parameterized convection
   ! Allen and Pickering (2000) scheme for parameterized convection
   ! 
!----------------------------------------------------------------------
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
! Qni        cloud ice number concentration (#/kg)
!
!----------------------------------------------------------------------
!-- th        potential temperature    (K)
!-- moist_new     updated moisture array   (kg/kg)
!-- moist_old     Old moisture array       (kg/kg)
!-- rho           density of air           (kg/m^3)
!-- pi_phy        exner function           (dimensionless)
!-- p             pressure                 (Pa)
!-- z             Height above sea level   (m)
!-- G             acceleration due to gravity  (m/s^2)
!-- CP            heat capacity at constant pressure for dry air (J/kg/K)
!-- R_d           gas constant for dry air (J/kg/K)
!-- R_v           gas constant for water vapor (J/kg/K)
!-- XLS           latent heat of sublimation   (J/kg)
!-- XLV           latent heat of vaporization  (J/kg)
!-- XLF           latent heat of melting       (J/kg)
!-- rhowater      water density                      (kg/m^3)
!-- rhosnow       snow density               (kg/m^3)
!-- F_ICE_PHY     Fraction of ice.
!-- F_RAIN_PHY    Fraction of rain.
!-- F_RIMEF_PHY   Mass ratio of rimed ice (rime factor)
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
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- its           start index for i in tile
!-- ite           end index for i in tile
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!
!======================================================================

   INTEGER,    INTENT(IN   )    :: lightning
!
   INTEGER,      INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
   INTEGER,      INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
   INTEGER,      INTENT(IN   )    ::       its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   )    ::       tracer_opt, itimestep

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),             &
         INTENT(INOUT) ::                                      &
                                                     c_no,     &
                                                     s_ic,     &
                                                     s_cg       
   REAL, DIMENSION( ims:ime , jms:jme ) ,                      &
         INTENT(INOUT)   ::                          ic_fr,    &     ! cumulative number of intracloud flashes (diagnostic)
                                                     cg_fr           ! cumulative number of cloud-to-ground flashes (diagnostic)

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(IN   ) ::                                             &
                                                                 rho, &
                                                               p_phy, &
                                                               t_phy, &
                                                                   u, &
                                                                   v, &
                                                                   w
   REAL, INTENT(IN   ) :: dx,dy

   REAL, INTENT(IN   )  ::   temp_upper, temp_lower, N_IC, N_CG                   
   REAL, INTENT(IN   )  ::   time_step
   INTEGER, INTENT(IN   )  ::   lightning_time_step, lightning_start_seconds, &
                                passive_ltng, lflash_data

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN ) ::                          z

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),                &
         INTENT(IN ) ::                                   moist

   REAL , DIMENSION( ims:ime , jms:jme ) ,                        &
         INTENT(IN)   ::                               ht, xlat, xlon

   REAL,    INTENT(IN   )  ::   flashrate_factor
   INTEGER, INTENT(IN   )  ::   flashrate_method
   INTEGER, INTENT(IN   )  ::   iccg_method
   INTEGER, INTENT(IN   )  ::   cellcount_method

! For CPM lightning
   INTEGER, DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: ktop
   REAL,    DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: xland
! LOCAL  VAR

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme )          :: dbz
   CHARACTER (LEN=80) :: message


!---------------------------------------------------------------------
!  check for type of lightning parameterization.  
!---------------------------------------------------------------------


   if (lightning .eq. 0) return
   IF ((itimestep*time_step < float(lightning_start_seconds)) .or. &
        (MOD((itimestep*time_step),float(lightning_time_step)) .ne. 0.)) return

     ltng_select: SELECT CASE(lightning)

        CASE (CRMPICKERING)
             IF ((itimestep*time_step >= float(lightning_start_seconds)) .and. &
               (MOD((itimestep*time_step),float(lightning_time_step)) .eq. 0.)) THEN

              CALL wrf_debug ( 100 , 'lightning: calling CRM Pickering' )
!              write(message,'(" it,dt,ltngdt  ",i8,f7.2,i5)')  &
!                       itimestep, time_step, lightning_time_step
!              CALL WRF_MESSAGE (message)
!              write(message,'(" MAX UPDRAFT  ",f9.2)') MAXVAL(w(its:ite,kts:kte,jts:jte))
!              CALL WRF_MESSAGE (message)


               CALL radar(t_phy, p_phy, rho,                   &
                  moist(ims,kms,jms,P_QR),                     &
                  moist(ims,kms,jms,P_QS),                     &
                  moist(ims,kms,jms,P_QG),                     &
                  dbz,                                         &
                  ids,ide, jds,jde, kds,kde,                   &
                  ims,ime, jms,jme, kms,kme,                   &
                  its,ite, jts,jte, kts,kte                    )

!              write(message,'(" MAX REFLECTIVITY  ",f9.2)') MAXVAL(dbz(its:ite,kts:kte,jts:jte))
!              CALL WRF_MESSAGE (message)

               CALL wrf_debug(100,'calling ltng_decaria from lightning_driver')
               CALL ltng_decaria (itimestep,time_step,              &
                      w,t_phy,p_phy,rho,                            &
                      moist(ims,kms,jms,P_QR),                      &
                      moist(ims,kms,jms,P_QS),                      &
                      moist(ims,kms,jms,P_QG),                      &
                      c_no,s_ic,s_cg,                               &
                      tracer_opt,                                   &
                      ids,ide, jds,jde, kds,kde,                    &
                      ims,ime, jms,jme, kms,kme,                    &
                      its,ite, jts,jte, kts,kte,                    &
                      dx,dy,z,ht,xlat,xlon,                         &
                      dbz,                                          &
                      lightning_time_step,                          &
                      lightning_start_seconds,temp_upper,           &
                      temp_lower,N_IC,N_CG,                         &
                      passive_ltng, flashrate_factor,               &
                      flashrate_method, iccg_method,                & 
                      cellcount_method,                             &
                      ic_fr, cg_fr)
             END IF

!
!        CASE (CRMBARTHE)
!             CALL wrf_debug ( 100 , 'lightning_driver: calling barthe scheme' )
!
!        CASE (REGPRICE)
!             CALL wrf_debug ( 100 , 'lightning_driver: calling Price and Rind scheme' )
!
!        CASE (REGALLEN)
!             CALL wrf_debug ( 100 , 'lightning_driver: calling Allen and Pickering scheme' )
        CASE (CPMPRO)

              CALL wrf_debug ( 100 , 'lightning: calling CPMPRO(Price-Rind-Ott)' )
              CALL wrf_debug ( 100 , 'calling ltng_cpm from lightning_driver')
              CALL ltng_cpm (itimestep,time_step,                  &
                     t_phy,rho,                                    &
                     c_no,s_ic,s_cg,                               & ! s_ic here is total ltng, s_cg is decaying ltng
                     tracer_opt,                                   &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte,                    &
                     dx,dy,z,ht,xlat,                              &
                     lightning_time_step,                          &
                     lightning_start_seconds,N_IC,                 & ! N_IC here is total ltng
                     flashrate_factor,ic_fr,xland,ktop )

      CASE DEFAULT 

         WRITE( wrf_err_message , * ) 'The lightning option does not exist: lightning = ', lightning
         CALL wrf_error_fatal ( wrf_err_message )

      END SELECT ltng_select 


   CALL wrf_debug ( 100 , 'lightning_driver: returning from' )

   RETURN

   END SUBROUTINE lightning_driver

END MODULE module_lightning_driver

