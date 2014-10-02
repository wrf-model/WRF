!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_shallowcu_driver
CONTAINS
   SUBROUTINE shallowcu_driver(                                       &
                 ! Order dependent args for domain, mem, and tile dims
                      ids,ide, jds,jde, kds,kde                       &
                     ,ims,ime, jms,jme, kms,kme                       &
                     ,ips,ipe, jps,jpe, kps,kpe                       &
                     ,i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                 ! Order independent args (use VAR= in call)
                 ! --Prognostic
                     ,u,v,th,t                                        &
                     ,p,pi,rho,moist                                  &
                 ! --Other arguments
                     ,num_moist                                       &
                     ,itimestep,dt,dx,cudt,curr_secs,adapt_step_flag  &
                     ,rainsh,pratesh,nca,rainshv                      &
                     ,z,z_at_w,dz8w,mavail,pblh,p8w                   &
                     ,tke_pbl                                         &
                     ,cldfra,cldfra_old,cldfra_old_mp,cldfra_conv     &
                     ,cldfrash                                        &
                     ,htop,hbot                                       &
                 ! Package selection variables
                     ,shcu_physics                                    &
                 ! Optional moisture tracers 
                     ,qv_curr, qc_curr, qr_curr                       &
                     ,qi_curr, qs_curr, qg_curr                       & 
                     ,qnc_curr,qni_curr                               &
#if ( WRF_CHEM == 1 )
                     ,chem, chem_opt                                  &
#endif
                 ! Optional output arguments for CAMZM scheme
                     ,dlf, rliq, rliq2,dlf2  &
                     ,cmfmc, cmfmc2       &
                 ! Optional output arguments for CAMUW scheme
                     ,cush, snowsh, icwmrsh, rprdsh, cbmf, cmfsl      &
                     ,cmflq, evapcsh                                  &
                 ! Optional moisture and other tendencies
                     ,rqvshten,rqcshten,rqrshten                      &
                     ,rqishten,rqsshten,rqgshten                      &
                     ,rqcnshten,rqinshten                             &
                     ,rqvblten,rqvften                                &
                     ,rushten,rvshten                                 &
                     ,rthshten,rthraten,rthblten,rthften              &
                 ! Optional moisture tracer flags 
                     ,f_qv,f_qc,f_qr                                  &
                     ,f_qi,f_qs,f_qg                                  &
                     ,ht,shfrc3d,is_CAMMGMP_used                      &
                 ! for grims shallow convection with ysupbl
                     ,wstar,delta,kpbl,znu,raincv                   &
                                                                      )
!----------------------------------------------------------------------
   USE module_model_constants
   USE module_state_description, ONLY: CAMUWSHCUSCHEME    &
# if (EM_CORE == 1)
                                       , CAMMGMPSCHEME    & 
# endif
                                       , G3SHCUSCHEME     & 
                                       , GRIMSSHCUSCHEME 
! *** add new modules of schemes here

   USE module_shcu_camuwshcu_driver, ONLY : camuwshcu_driver
   USE module_shcu_grims
   USE module_dm
   USE module_domain, ONLY: domain
#if ( WRF_CHEM == 1 )
      USE module_state_description, ONLY: num_chem
#endif

   !  This driver calls subroutines for the shallow cumulus
   !  parameterizations.
   !
   !  1. G3 shallow cumulus
   !  2. UW shallow cumulus from CAM
   !  3. GRIMs shallow cumulus from GRIMs (available only with ysupbl)
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
!-----------------------------------------------------------------
!-- DT            time step (second)
!-- CUDT          cumulus time step (minute)
!-- curr_secs     current forecast time (seconds)
!-- itimestep     number of time step (integer)   
!-- DX            horizontal space interval (m)
!-- rr            dry air density (kg/m^3)
!
!-- RUSHTEN       Zonal wind tendency due to shallow
!                 cumulus scheme precipitation (m/s/s)
!-- RVSHTEN       Meridional wind tendency due to 
!                 cumulus scheme precipitation (m/s/s)
!-- RTHSHTEN      Theta tendency due to shallow
!                 cumulus scheme precipitation (K/s)
!-- RQVSHTEN      Qv tendency due to shallow
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQRSHTEN      Qr tendency due to shallow
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQCSHTEN      Qc tendency due to shallow
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQSSHTEN      Qs tendency due to shallow
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQISHTEN      Qi tendency due to shallow
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQGSHTEN      Qg tendency due to shallow
!                 cumulus scheme precipitation (kg/kg/s)
!
!-- RAINSH        accumulated total shallow cumulus scheme precipitation (mm)
!-- RAINSHV       time-step shallow cumulus scheme precipitation (mm)
!-- PRATESH       precipitiation rate from shallow cumulus scheme (mm/s)
!-- NCA           counter of the cloud relaxation 
!                 time in KF cumulus scheme (integer)
!-- u_phy         u-velocity interpolated to theta points (m/s)
!-- v_phy         v-velocity interpolated to theta points (m/s)
!-- th_phy        potential temperature (K)
!-- t_phy         temperature (K)
!-- tke_pbl       turbulent kinetic energy from PBL scheme (m2/s2)
!-- w             vertical velocity (m/s)
!-- moist         moisture array (4D - last index is species) (kg/kg)
!-- z             height above sea level at middle of layers (m)
!-- z_at_w        height above sea level at layer interfaces (m)
!-- dz8w          dz between full levels (m)
!-- pblh          planetary boundary layer height (m)
!-- mavail        soil moisture availability
!-- p8w           pressure at full levels (Pa)
!-- p_phy         pressure (Pa)
!-- pi_phy        the exner function, (p/p0)**(R/Cp) (dimensionless)
!                 points (dimensionless)
!-- hfx           upward heat flux at surface (W/m2)
!-- RTHRATEN      radiative temp forcing for Grell-Devenyi scheme
!-- RTHBLTEN      PBL temp forcing for Grell-Devenyi scheme
!-- RQVBLTEN      PBL moisture forcing for Grell-Devenyi scheme
!-- RTHFTEN
!-- RQVFTEN
!-- cldfra        cloud fraction
!-- cldfra_old    cloud fraction from previous time step
!-- cldfrash      cloud fraction from shallow Cu
!-- cldfra_old_mp cloud fraction from previous time step if CAMMGMP microphysics is used
!-- cldfra_conv   convective cloud fraction
!-- rho           density (kg/m^3)
!-- XLV0          latent heat of vaporization constant 
!                 used in temperature dependent formula (J/kg)
!-- XLV1          latent heat of vaporization constant 
!                 used in temperature dependent formula (J/kg/K)
!-- XLS0          latent heat of sublimation constant 
!                 used in temperature dependent formula (J/kg)
!-- XLS1          latent heat of sublimation constant
!                 used in temperature dependent formula (J/kg/K)
!-- R_d           gas constant for dry air ( 287. J/kg/K)
!-- R_v           gas constant for water vapor (461 J/k/kg)
!-- Cp            specific heat at constant pressure (1004 J/k/kg)
!-- rvovrd        R_v divided by R_d (dimensionless)
!-- G             acceleration due to gravity (m/s^2)
!-- EP_1          constant for virtual temperature 
!                 (R_v/R_d - 1) (dimensionless)
!--shfrc3d        Shallow cloud fraction
!-- cmfmc         Deep + Shallow Convective m
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
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!-- HBOT          index of lowest model layer with convection
!-- HTOP          index of highest model layer with convection
!-- LBOT          index of lowest model layer with convection
!-- LTOP          index of highest model layer with convection
!-- periodic_x    T/F this is using periodic lateral boundaries in the X direction
!-- periodic_y    T/F this is using periodic lateral boundaries in the Y-direction
!
!======================================================================
   LOGICAL,      INTENT(IN   )    :: is_CAMMGMP_used !BSINGH:01/31/2013: Added for CAMUWSHCU
   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                                        kts,kte, &
                                      itimestep, num_tiles
#if ( WRF_CHEM == 1 )
   INTEGER,      INTENT(IN   )    :: chem_opt
#endif

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::                             &
                  num_moist

   INTEGER,      INTENT(IN   )    ::               shcu_physics

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),      &
         INTENT(INOUT)  ::                                       &
                                                          moist
#if ( WRF_CHEM == 1 )
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),       &
         INTENT(INOUT)  ::                                    &
                                                          chem
#endif
   

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                          &
                                                         cldfra  &
                                                    ,cldfra_old  &
                                                  ,cldfra_old_mp &
                                                    ,cldfra_conv &
                                                      ,       z  &
                                                      ,  z_at_w  &
                                                      ,    dz8w  &
                                                      ,     p8w  &
                                                      ,       p  &
                                                      ,      pi  &
                                                      ,       u  &
                                                      ,       v  &
                                                      ,      th  &
                                                      ,       t  &
                                                      , tke_pbl  &
                                                      ,     rho


   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
                  MAVAIL,PBLH,ht

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                               RAINSH  &
                                                    ,       NCA  & 
                                                    ,      HTOP  & 
                                                    ,      HBOT
 

   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(INOUT),OPTIONAL :: &
        PRATESH, RAINSHV
   REAL, DIMENSION( ims:ime , jms:jme ) :: tmppratesh
                                                    
   REAL,  INTENT(IN   ) :: DT, DX
   INTEGER,      INTENT(IN   ),OPTIONAL    ::                             &
                                      ips,ipe, jps,jpe, kps,kpe
   REAL,  INTENT(IN   ),OPTIONAL :: CUDT
   REAL,  INTENT(IN   ),OPTIONAL :: CURR_SECS
   LOGICAL,INTENT(IN   ),OPTIONAL    ::     adapt_step_flag
   REAL   :: cudt_pass, curr_secs_pass
   LOGICAL :: adapt_step_flag_pass

!
! optional arguments
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      ! optional moisture tracers
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr                  & 
                     ! optional scalar tracers !BSINGH
                     ,qnc_curr,qni_curr                          &
                      ! optional moisture and other tendencies
                     ,rqvshten,rqcshten,rqrshten                 &
                     ,rqishten,rqsshten,rqgshten                 &
                     ,rqcnshten,rqinshten                        &
                     ,rqvblten,rqvften                           &
                     ,rthraten,rthblten                          &
                     ,rthften,rushten,rvshten,rthshten

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                    OPTIONAL, INTENT(INOUT) ::                   &
                rliq, rliq2 &
               ,cbmf, cush, snowsh
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                  cldfrash, cmfsl, cmflq, icwmrsh,               &
                  dlf, evapcsh,                                  &
                  cmfmc, cmfmc2, rprdsh
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
        INTENT(OUT) ::                                 &
                  dlf2                                             ! Required by CAMMGMP Microphysics
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
        INTENT(OUT) ::                                 &
                 shfrc3d                                           ! Required by wet scavenging code in WRF_CHEM
! for grims shallow convection with ysupbl
!
   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL, INTENT(IN   )                 ::      wstar
   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL, INTENT(IN   )                 ::        delta
   REAL, DIMENSION( ims:ime, jms:jme )                         , &
         OPTIONAL, INTENT(IN   )                 ::       raincv
   REAL, DIMENSION( kms:kme )                                  , &
         OPTIONAL, INTENT(IN   )       ::                    znu
   INTEGER, DIMENSION( ims:ime , jms:jme )                     , &
         OPTIONAL, INTENT(IN)                    ::         kpbl
!
! Flags relating to the optional tendency arrays declared above
! Models that carry the optional tendencies will provdide the
! optional arguments at compile time; these flags all the model
! to determine at run-time whether a particular tracer is in
! use or not.
!
   LOGICAL, INTENT(IN), OPTIONAL ::                             &
                                                      f_qv      &
                                                     ,f_qc      &
                                                     ,f_qr      &
                                                     ,f_qi      &
                                                     ,f_qs      &
                                                     ,f_qg


! LOCAL  VAR

   INTEGER :: i,j,k,its,ite,jts,jte,ij
   CHARACTER(len=200) :: message


!-----------------------------------------------------------------

   if (.not. PRESENT(CURR_SECS)) then
      curr_secs_pass = -1
   else 
      curr_secs_pass = curr_secs
   endif

   if (.not. PRESENT(CUDT)) then
      cudt_pass = -1
   else
      cudt_pass = cudt
   endif

   if (.not. PRESENT(adapt_step_flag)) then
      adapt_step_flag_pass = .false.
   else
      adapt_step_flag_pass = adapt_step_flag
   endif

   ! Initialize tmppratesh to pratesh

   if ( PRESENT ( pratesh ) ) then
      tmppratesh(:,:) = pratesh(:,:)
   else
      tmppratesh(:,:) = 0.
   end if
   

   IF (shcu_physics .eq. 0) return
   
   
! DON'T JUDGE TIME STEP HERE, SINCE KF NEEDS ACCUMULATED W FIELD.
! DO IT INSIDE THE INDIVIDUAL CUMULUS SCHEME

! SET START AND END POINTS FOR TILES
!$OMP PARALLEL DO   &
!$OMP PRIVATE ( ij ,its,ite,jts,jte, i,j,k)
   DO ij = 1 , num_tiles
      its = i_start(ij)
      ite = i_end(ij)
      jts = j_start(ij)
      jte = j_end(ij)


   scps_select: SELECT CASE(shcu_physics)

   CASE (G3SHCUSCHEME)
      ! This setting takes the place of ishallow in v3.1.1+

   CASE (CAMUWSHCUSCHEME)
      CALL wrf_debug(100,'in camuw_scps')
      IF(.not.f_qi)THEN
         WRITE( message , * ) 'This shallow cumulus option requires ice microphysics option: f_qi = ', f_qi
         CALL wrf_error_fatal ( message )
      ENDIF
      CALL camuwshcu_driver(                                             &
            IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde              &
           ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme              &
           ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte              &
           ,NUM_MOIST=num_moist, DT=dt                                   &
           ,P=p, P8W=p8w, PI_PHY=pi                                      &
           ,Z=z, Z_AT_W=z_at_w, DZ8W=dz8w                                &
           ,T_PHY=t, U_PHY=u, V_PHY=v                                    &
           ,MOIST=moist, QV=qv_curr, QC=qc_curr, QI=qi_curr              &
           ,QNC=qnc_curr, QNI=qni_curr                                   & 
#if ( WRF_CHEM == 1 )
           ,CHEM=chem, CHEM_OPT=chem_opt                                 &
#endif
           ,PBLH_IN=pblh, TKE_PBL=tke_pbl                                &
           ,CLDFRA=cldfra, CLDFRA_OLD=cldfra_old                         &
           ,CLDFRA_OLD_MP=cldfra_old_mp                                  &
           ,CLDFRA_CONV=cldfra_conv,IS_CAMMGMP_USED=is_CAMMGMP_used      &
           ,CLDFRASH=cldfrash                                            &
           ,CUSH_INOUT=cush, PRATESH=tmppratesh                          &
           ,SNOWSH=snowsh                                                &
           ,ICWMRSH=icwmrsh, CMFMC=cmfmc, CMFMC2_INOUT=cmfmc2            &
           ,RPRDSH_INOUT=rprdsh, CBMF_INOUT=cbmf                         &
           ,CMFSL=cmfsl, CMFLQ=cmflq, DLF=dlf,DLF2=dlf2                  & !DLF2 is required by CAMMGMP microphysics
           ,EVAPCSH_INOUT=evapcsh                                        &
           ,RLIQ=rliq, RLIQ2_INOUT=rliq2, CUBOT=hbot, CUTOP=htop         &
           ,RUSHTEN=rushten, RVSHTEN=rvshten, RTHSHTEN=rthshten          &
           ,RQVSHTEN=rqvshten, RQCSHTEN=rqcshten, RQRSHTEN=rqrshten      &
           ,RQISHTEN=rqishten, RQSSHTEN=rqsshten, RQGSHTEN=rqgshten      &
           ,RQCNSHTEN=rqcnshten,RQINSHTEN=rqinshten                      & 
           ,HT=ht,SHFRC3D=shfrc3d,ITIMESTEP=itimestep                    &
                                                                         )

   CASE (GRIMSSHCUSCHEME)
      CALL wrf_debug(100,'in grims_scps')
      IF ( PRESENT( wstar ) ) THEN
      CALL grims(                                                        &
            QV3D=qv_curr,T3D=t                                           &
           ,P3DI=p8w,P3D=p,PI3D=pi,Z3DI=Z_AT_W                           &
           ,WSTAR=wstar,HPBL=pblh,DELTA=delta                        &
           ,RTHSHTEN=rthshten,RQVSHTEN=rqvshten                          &
           ,DT=dt,G=g,XLV=xlv,RD=r_d,RV=r_v                        &
           ,RCP=rcp,P1000MB=p1000mb                                      &
           ,KPBL2D=kpbl,ZNU=znu,RAINCV=raincv                            &
           ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde              &
           ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme              &
           ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte              &
                                                                         )
      ENDIF

   CASE DEFAULT 
      WRITE( message , * ) 'The shallow cumulus option does not exist: shcu_physics = ', shcu_physics
      CALL wrf_error_fatal ( message )

   END SELECT scps_select

   ENDDO
   !$OMP END PARALLEL DO

   !
   ! Copy pratesh back to output array, if necessary.
   !
   if (PRESENT(PRATESH)) then
      pratesh(:,:) = tmppratesh(:,:)
      if (PRESENT(RAINSHV)) then
         rainshv(:,:) = pratesh(:,:)*dt
      endif
   endif

   END SUBROUTINE shallowcu_driver

END MODULE module_shallowcu_driver
