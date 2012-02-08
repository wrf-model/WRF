!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE a_module_cumulus_driver
CONTAINS
   SUBROUTINE a_cumulus_driver(grid                                     &
                 ! Order dependent args for domain, mem, and tile dims
                     ,ids,ide, jds,jde, kds,kde                       &
                     ,ims,ime, jms,jme, kms,kme                       &
                     ,ips,ipe, jps,jpe, kps,kpe                       &
                     ,i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                 ! Order independent args (use VAR= in call)
                 ! --Prognostic
                     ,u,v,th,a_th,t,a_t,w                             &
                     ,p,a_p,pi,rho,a_rho                              &
                 ! --Other arguments
                     ,itimestep,dt,dx,cudt,curr_secs,adapt_step_flag  &
                     ,cudtacttime                                     & 
                     ,rainc,raincv,a_raincv,pratec,a_pratec,nca       &
                     ,z,a_z,z_at_w,dz8w,a_dz8w,mavail,pblh,p8w,psfc,tsk          &
                     ,tke_pbl, ust                                    &
                     ,forcet,forceq,w0avg,stepcu,gsw                  &
                     ,cldefi,lowlyr,xland,cu_act_flag,warm_rain       &
                     ,hfx,qfx,cldfra,tpert2d,htop,a_htop,hbot,a_hbot,kpbl,ht&
                     ,ensdim,maxiens,maxens,maxens2,maxens3           &
                     ,periodic_x,periodic_y                           &
                 ! Package selection variables
                     ,cu_physics, bl_pbl_physics, sf_sfclay_physics   &
                 ! Optional moisture tracers 
                     ,qv_curr, a_qv_curr,qc_curr, qr_curr                       &
                     ,qi_curr, qs_curr, qg_curr                       & 
                     ,qv_prev, qc_prev, qr_prev                       & 
                     ,qi_prev, qs_prev, qg_prev                       &
                 ! Optional arguments for GD scheme
                     ,apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma     &
                     ,apr_capme,apr_capmi,edt_out,clos_choice         &
                     ,mass_flux,xf_ens,pr_ens,cugd_avedx,imomentum    &
                     ,ishallow,cugd_tten,cugd_qvten,cugd_qcten        &
                     ,cugd_ttens,cugd_qvtens                          &
                     ,gd_cloud,gd_cloud2                              &
                 ! Optional output arguments for CAMZM scheme
                     ,cape, zmmu, zmmd, zmdt, zmdq, dlf, rliq         &
                     ,pconvb, pconvt                                  &
                     ,evaptzm, fzsntzm, evsntzm, evapqzm, zmflxprc    &
                     ,zmflxsnw, zmntprpd, zmntsnpd, zmeiheat          &
                     ,cmfmc, cmfmcdzm, preccdzm, precz                &
                     ,zmmtu, zmmtv, zmupgu, zmupgd, zmvpgu, zmvpgd    &
                     ,zmicuu, zmicud, zmicvu, zmicvd, zmdice, zmdliq  &
                     ,k22_shallow,kbcon_shallow,ktop_shallow,xmb_shallow &
                 ! Optional arguments for SAS scheme
                     ,pgcon,sas_mass_flux                             &
                     ,shalconv,shal_pgcon                             &
                     ,HPBL2D,EVAP2D,HEAT2D                            &     !Kwon for SAS2010 shallow convection
                 ! Optional arguments for NSAS scheme
                     ,mp_physics                                      &
                 ! Optional moisture and other tendencies
                     ,rqvcuten,a_rqvcuten,rqccuten,rqrcuten           &
                     ,rqicuten,rqscuten,rqgcuten                      &
                     ,rqvblten,rqvften                                &
                     ,rucuten,rvcuten                                 &
                     ,rthcuten,a_rthcuten,rthraten,rthblten,rthften   &
                     ,mommix,store_rand                               &
! Optional variables for tiedtke scheme - add by ZCX&YQW 
                     ,znu                                             &
                 ! Optional moisture tracer flags 
                     ,f_qv,f_qc,f_qr                                  &
                     ,f_qi,f_qs,f_qg                                  &
                     ,CFU1,CFD1,DFU1,EFU1,DFD1,EFD1,f_flux            &
                 ! Optional trigger function activation variable
                     ,kfeta_trigger                                   &
#if ( WRF_DFI_RADAR == 1 )
                 ! Optional CAP suppress option      --- 3.2 CLEANUP TODO -- THESE SHOULD BE OPTIONAL, NOT #IF/#ENDIF
                     ,do_capsuppress                                  &
#endif                                 
                                                                      )
!----------------------------------------------------------------------
   USE module_model_constants
   USE module_state_description, ONLY:     KFSCHEME,BMJSCHEME         &
                                          ,KFETASCHEME,GDSCHEME       &
                                          ,G3SCHEME                   &
                                          ,P_QC,P_QI,Param_FIRST_SCALAR &
                                          ,CAMZMSCHEME, SASSCHEME     &
                                          ,NSASSCHEME, DUCUSCHEME     &
                                          ,TIEDTKESCHEME

! *** add new modules of schemes here

#ifdef DM_PARALLEL
   USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks 
# if (EM_CORE == 1)
   USE module_comm_dm   ,  ONLY : halo_cup_g3_in_sub, halo_cup_g3_out_sub
# endif
#endif
   USE module_domain    , ONLY: domain
   USE a_module_cu_du     , ONLY : a_DUCU   
   USE module_wrf_error , ONLY : wrf_err_message

   !  This driver calls subroutines for the cumulus parameterizations.
   !
   !  1. Kain & Fritsch (1993)
   !  2. Betts-Miller-Janjic (Janjic, 1994)
   !  3. Grell-Devenyi (Grell and Devenyi, 2002)
   !  4. Simplified Arakawa-Schubert scheme (NCEP)
   !     (adapted by Zhang and Wang to work with ARW in V3.3)
   !  5. Grell 3D ensemble scheme
   !  6. Modified Tiedtke scheme (Zhang and Wang 2010)
   ! 14. New simplified Arakawa-Schubert scheme (NCEP, YSU)
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
!-- RUCUTEN       Zonal wind tendency due to 
!                 cumulus scheme precipitation (m/s/s)
!-- RVCUTEN       Meridional wind tendency due to 
!                 cumulus scheme precipitation (m/s/s)
!-- RTHCUTEN      Theta tendency due to 
!                 cumulus scheme precipitation (K/s)
!-- RQVCUTEN      Qv tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQRCUTEN      Qr tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQCCUTEN      Qc tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQSCUTEN      Qs tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQICUTEN      Qi tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!
!-- RAINC         accumulated total cumulus scheme precipitation (mm)
!-- RAINCV        time-step cumulus scheme precipitation (mm)
!-- PRATEC        precipitiation rate from cumulus scheme (mm/s)
!-- NCA           counter of the cloud relaxation 
!                 time in KF cumulus scheme (integer)
!-- u_phy         u-velocity interpolated to theta points (m/s)
!-- v_phy         v-velocity interpolated to theta points (m/s)
!-- th_phy        potential temperature (K)
!-- t_phy         temperature (K)
!-- tsk           skin temperature (K)
!-- tke_pbl       turbulent kinetic energy from PBL scheme (m2/s2)
!-- ust           u* in similarity theory (m/s)
!-- w             vertical velocity (m/s)
!-- moist         moisture array (4D - last index is species) (kg/kg)
!-- z             height above sea level at middle of layers (m)
!-- z_at_w        height above sea level at layer interfaces (m)
!-- dz8w          dz between full levels (m)
!-- pblh          planetary boundary layer height (m)
!-- mavail        soil moisture availability
!-- p8w           pressure at full levels (Pa)
!-- psfc          surface pressure (Pa)
!-- p_phy         pressure (Pa)
!-- pi_phy        exner function (dimensionless)
!                 points (dimensionless)
!-- hfx           upward heat flux at surface (W/m2)
!-- qfx           upward moisture flux at surface (kg/m2/s)
!-- RTHRATEN      radiative temp forcing for Grell-Devenyi scheme
!-- RTHBLTEN      PBL temp forcing for Grell-Devenyi scheme
!-- RQVBLTEN      PBL moisture forcing for Grell-Devenyi scheme
!-- RTHFTEN
!-- RQVFTEN
!-- MASS_FLUX
!-- XF_ENS
!-- PR_ENS
!-- warm_rain
!-- cldfra        cloud fraction
!-- CU_ACT_FLAG
!-- W0AVG         average vertical velocity, (for KF scheme) (m/s)
!-- kfeta_trigger namelist for KF trigger (=1, default; =2, moisture-advection-dependent trigger)
!-- rho           density (kg/m^3)
!-- CLDEFI        precipitation efficiency (for BMJ scheme) (dimensionless)
!-- STEPCU        # of fundamental timesteps between convection calls
!-- XLAND         land-sea mask (1.0 for land; 2.0 for water)
!-- LOWLYR        index of lowest model layer above the ground
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
!-- pi_phy        the exner function, (p/p0)**(R/Cp) (none unit)
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
!-- KPBL          layer index of the PBL
!-- periodic_x    T/F this is using periodic lateral boundaries in the X direction
!-- periodic_y    T/F this is using periodic lateral boundaries in the Y-direction
!
!======================================================================

   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                                        kts,kte, &
                                      itimestep, num_tiles
   LOGICAL periodic_x, periodic_y
   TYPE(domain) , INTENT(INOUT)          :: grid
   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::                             &
                  ensdim,maxiens,maxens,maxens2,maxens3

   INTEGER, OPTIONAL,     INTENT(IN   )    ::                    &
                   cugd_avedx,clos_choice,bl_pbl_physics,sf_sfclay_physics

   INTEGER,      INTENT(IN   )    ::                 cu_physics  
   INTEGER,      INTENT(IN   )    ::   STEPCU
   LOGICAL,      INTENT(IN   )    ::   warm_rain

   REAL, INTENT(IN), OPTIONAL :: pgcon,shal_pgcon,sas_mass_flux
   INTEGER, INTENT(IN), OPTIONAL :: shalconv

   INTEGER,DIMENSION( ims:ime, jms:jme ),                        &
           INTENT(IN ) ::                                LOWLYR

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                          &
                                                              z  &
                                                      ,    dz8w  &
                                                      ,     p8w  &
                                                      ,       p  &
                                                      ,      pi  &
                                                      ,       u  &
                                                      ,       v  &
                                                      ,      th  &
                                                      ,       t  &
                                                      ,     rho  &
                                                      ,       w  

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::               &
                                                            a_z  &
                                                      ,  a_dz8w  &
                                                      ,     a_p  &
                                                      ,    a_th  &
                                                      ,     a_t  &
                                                      ,   a_rho

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ),OPTIONAL ::                         z_at_w  &
                                                      ,  cldfra  &
                                                      , tke_pbl  

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT)  ::                                       &
                                                          W0AVG

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
          GSW,HT,XLAND

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                RAINC  &
                                                    ,    RAINCV  &
                                                    ,  a_RAINCV  &
                                                    ,       NCA  & 
                                                    ,      HTOP  & 
                                                    ,    a_HTOP  & 
                                                    ,      HBOT  &
                                                    ,    a_HBOT  &
                                                    ,    CLDEFI
 
   REAL, DIMENSION( kms:kme ), OPTIONAL, INTENT(IN   ) ::        &
                                                            znu

   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(INOUT),OPTIONAL :: &
        PRATEC,a_PRATEC,MAVAIL,PBLH,PSFC,TSK,TPERT2D,UST,HFX,QFX
   REAL, DIMENSION( ims:ime , jms:jme ) :: tmppratec, a_tmppratec
                                                    
   INTEGER, DIMENSION( ims:ime , jms:jme ),                      &
                    INTENT(IN) ::                          KPBL

   LOGICAL, DIMENSION( ims:ime , jms:jme ),                      &
          INTENT(INOUT) :: CU_ACT_FLAG

   INTEGER, INTENT(IN   ), OPTIONAL        ::   kfeta_trigger

   REAL,  INTENT(IN   ) :: DT, DX
   INTEGER,      INTENT(IN   ),OPTIONAL    ::                             &
                   ips,ipe, jps,jpe, kps,kpe,imomentum,ishallow
   REAL,  INTENT(IN   ),OPTIONAL :: CUDT
   REAL,  INTENT(IN   ),OPTIONAL :: CURR_SECS
   LOGICAL,INTENT(IN   ),OPTIONAL    ::     adapt_step_flag
   REAL,  INTENT(INOUT ),OPTIONAL :: cudtacttime                 
   REAL   :: cudt_pass, curr_secs_pass,cudtacttime_pass         
   LOGICAL :: adapt_step_flag_pass

   INTEGER,      INTENT(IN   ), OPTIONAL    ::   mp_physics

   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(IN) ::  STORE_RAND
!
   REAL, OPTIONAL,  INTENT(INOUT) :: mommix

   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,               &    !Kwon for sas2010 shallow convection
                    INTENT(INOUT) ::  HPBL2D, EVAP2D, HEAT2D
!
   REAL, DIMENSION( ims:ime, jms:jme, kms:kme ),                 &
                   INTENT(INOUT) :: rucuten,rvcuten
!
! optional arguments
!
   INTEGER, DIMENSION( ims:ime, jms:jme ),                       &
         OPTIONAL, INTENT(INOUT) ::                              &
     k22_shallow,kbcon_shallow,ktop_shallow
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      ! optional moisture tracers
                      ! 2 time levels; if only one then use CURR
                      qv_curr, a_qv_curr, qc_curr, qr_curr       &
                     ,qi_curr, qs_curr, qg_curr                  & 
                     ,qv_prev, qc_prev, qr_prev                  & 
                     ,qi_prev, qs_prev, qg_prev                  &
                      ! optional moisture and other tendencies
                     ,rqvcuten,a_rqvcuten,rqccuten,rqrcuten      &
                     ,rqicuten,rqscuten,rqgcuten                 &
                     ,rqvblten,rqvften                           &
                     ,rthraten,rthblten                          &
                     ,cugd_tten,cugd_qvten,cugd_qcten            &
                     ,cugd_ttens,cugd_qvtens                     &
                     ,forcet, forceq                             &
                     ,rthften,rthcuten,a_rthcuten

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                    OPTIONAL,                                    &
                    INTENT(INOUT) ::                             &
                apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma      &
               ,apr_capme,apr_capmi,edt_out,xmb_shallow          &
                                                    , MASS_FLUX  &
               ,cape, pconvb, pconvt, preccdzm, precz, rliq
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                  GD_CLOUD,GD_CLOUD2,                            &
                  zmmd, zmmu, zmdt, zmdq, dlf,                   &
                  evaptzm, fzsntzm, evsntzm, evapqzm, zmflxprc,  &
                  zmflxsnw, zmntprpd, zmntsnpd, zmeiheat,        &
                  cmfmc, cmfmcdzm,                               &
                  zmmtu, zmmtv, zmupgu, zmupgd, zmvpgu, zmvpgd,  &
                  zmicuu, zmicud, zmicvu, zmicvd, zmdice, zmdliq
   REAL, DIMENSION( ims:ime , jms:jme , 1:ensdim ),              &
          OPTIONAL,                                              &
          INTENT(INOUT) ::                       XF_ENS, PR_ENS
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                                       &
                                                   CFU1,        &
                                                   CFD1,        &
                                                   DFU1,        &
                                                   EFU1,        &
                                                   DFD1,        &
                                                   EFD1

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
   LOGICAL, INTENT(IN), OPTIONAL ::                   f_flux

! LOCAL  VAR

   INTEGER :: i,j,k,its,ite,jts,jte,ij,trigger_kf
   logical :: l_flux

!-----------------------------------------------------------------

   l_flux=.FALSE.
   if (present(f_flux)) l_flux=f_flux
   if (.not. PRESENT(CURR_SECS)) then
      curr_secs_pass = -1
   else 
      curr_secs_pass = curr_secs
   endif

   if (.not. PRESENT(CUDT)) then
      cudt_pass = -1
      cudtacttime_pass = -1
   else
      cudt_pass = cudt
      cudtacttime_pass = cudtacttime
   endif

   if (.not. PRESENT(adapt_step_flag)) then
      adapt_step_flag_pass = .false.
   else
      adapt_step_flag_pass = adapt_step_flag
   endif

   ! Initialize tmppratec to pratec

   if ( PRESENT ( pratec ) ) then
      tmppratec(:,:) = pratec(:,:)
   else
      tmppratec(:,:) = 0.
   end if

   if (.not. PRESENT(kfeta_trigger)) then
      trigger_kf = 1
   else
      trigger_kf = kfeta_trigger
   endif

   IF (cu_physics .eq. 0) return

   !
   ! Copy pratec back to output array, if necessary.
   !
   a_tmppratec(:,:) = 0.0
   if (PRESENT(PRATEC)) then
      a_tmppratec(:,:) = a_tmppratec(:,:) + a_pratec(:,:)
      a_pratec(:,:) = 0.0
   endif

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


   cps_select: SELECT CASE(cu_physics)

     CASE (DUCUSCHEME)
          CALL wrf_debug(100,'in ducu')

          CALL A_DUCU(                                          &
              ! order independent arguments
                DT=dt ,KTAU=itimestep ,DX=dx                    &    !
               ,RHO=rho,a_RHO=a_rho                             &    !
               ,U=u ,V=v ,TH=th ,a_TH=a_th, T=t ,a_T=a_t,W=w    &    !
               ,PCPS=p ,a_PCPS=a_p, PI=pi                       &    !
               ,XLV=xlv0                                        &    !  or XLV=xlv1
               ,RAINCV=raincv, a_RAINCV=a_raincv                &
               ,NCA=nca, PRATEC=tmppratec, a_PRATEC=a_tmppratec &    
               ,DZ8W=dz8w, a_DZ8W=a_dz8w, Z= z, a_Z=a_z         &    !  
               ,W0AVG=w0avg                                     &    !
               ,CP=cp ,RV=R_v, RD=R_d, G=g                      &    !  RD, RV
               ,EP2=ep_2                                        &    !  only EP2 needed
               ,SVP1=svp1 ,SVP2=svp2 ,SVP3=svp3 ,SVPT0=svpt0    &    !  
               ,STEPCU=stepcu                                   &    !
               ,CU_ACT_FLAG=cu_act_flag                         &    !
               ,WARM_RAIN=warm_rain                             &    !
               ,CUTOP=htop, a_CUTOP=a_htop, CUBOT=hbot, a_CUBOT=a_hbot    &    ! from other scheme
               ,QV=qv_curr , a_QV=a_qv_curr                     &    !
               ,CUDTACTTIME=cudtacttime_pass                    &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &    !
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &    !
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &    !
              ! optionals
               ,RTHCUTEN=rthcuten ,a_RTHCUTEN=a_rthcuten        &
               ,RQVCUTEN=rqvcuten ,a_RQVCUTEN=a_rqvcuten        )    !

     CASE DEFAULT 

         WRITE( wrf_err_message , * ) 'The cumulus option does not exist: cu_physics = ', cu_physics
         CALL wrf_error_fatal ( wrf_err_message )

   END SELECT cps_select

      ENDDO
      !$OMP END PARALLEL DO

   ! Initialize tmppratec to pratec

   if ( PRESENT ( pratec ) ) then
      a_pratec(:,:) = a_pratec(:,:) + a_tmppratec(:,:)
      a_tmppratec(:,:) = 0.0
   else
      a_tmppratec(:,:) = 0.
   end if

   ! Copy cudtacttime back if necessary

   if ( PRESENT(CUDTACTTIME) ) then
      cudtacttime = cudtacttime_pass
   end if

   CALL wrf_debug(200,'returning from a_cumulus_driver')

   END SUBROUTINE a_cumulus_driver

END MODULE a_module_cumulus_driver
