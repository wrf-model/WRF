!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_cumulus_driver
CONTAINS
   SUBROUTINE cumulus_driver(                                         &
                 ! Order dependent args for domain, mem, and tile dims
                      ids,ide, jds,jde, kds,kde                       &
                     ,ims,ime, jms,jme, kms,kme                       &
                     ,i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                 ! Order independent args (use VAR= in call)
                 ! --Prognostic
                     ,u,v,th,t,w                                      &
                     ,p,pi,rho                                        &
                 ! --Other arguments
                     ,itimestep,dt,dx                                 &
                     ,rainc,raincv,nca                                &
                     ,dz8w,p8w                                        &
                     ,w0avg,stepcu                                    &
                     ,cldefi,lowlyr,xland,cu_act_flag,warm_rain       &
                     ,htop,hbot,kpbl,ht                               &  
                     ,ensdim,maxiens,maxens,maxens2,maxens3           &
                 ! Package selection variable
                     ,cu_physics                                      &
                 ! Optional moisture tracers 
                     ,qv_curr, qc_curr, qr_curr                       &
                     ,qi_curr, qs_curr, qg_curr                       & 
                     ,qv_prev, qc_prev, qr_prev                       & 
                     ,qi_prev, qs_prev, qg_prev                       &
                 ! Optional arguments for GD scheme
                     ,apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma     &
                     ,apr_capme,apr_capmi                             &
                     ,mass_flux,xf_ens,pr_ens                         &
                 ! Optional moisture and other tendencies
                     ,rqvcuten,rqccuten,rqrcuten                      &
                     ,rqicuten,rqscuten,rqgcuten                      &
                     ,rqvblten,rqvften                                &
                     ,rthcuten,rthraten,rthblten,rthften              &
                 ! Optional moisture tracer flags 
                     ,f_qv,f_qc,f_qr                                  &
                     ,f_qi,f_qs,f_qg                                  &
                                                                      )
!----------------------------------------------------------------------
   USE module_model_constants
   USE module_state_description, ONLY:     KFSCHEME,BMJSCHEME         &
                                          ,KFETASCHEME,GDSCHEME       &
                                          ,SASSCHEME

! *** add new modules of schemes here

   USE module_cu_kf
   USE module_cu_bmj
   USE module_cu_kfeta
   USE module_cu_gd
   USE module_cu_sas

   !  This driver calls subroutines for the cumulus parameterizations.
   !
   !  1. Kain & Fritsch (1993)
   !  2. Betts-Miller-Janjic (Janjic, 1994)
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
!-- itimestep     number of time step (integer)   
!-- DX            horizontal space interval (m)
!-- rr            dry air density (kg/m^3)
!
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
!-- RAINCV        cumulus scheme precipitation (mm)
!-- NCA           counter of the cloud relaxation 
!                 time in KF cumulus scheme (integer)
!-- u_phy         u-velocity interpolated to theta points (m/s)
!-- v_phy         v-velocity interpolated to theta points (m/s)
!-- th_phy        potential temperature (K)
!-- t_phy         temperature (K)
!-- w             vertical velocity (m/s)
!-- moist         moisture array (4D - last index is species) (kg/kg)
!-- dz8w          dz between full levels (m)
!-- p8w           pressure at full levels (Pa)  
!-- p_phy         pressure (Pa)
!-- pi_phy        exner function (dimensionless)
!                 points (dimensionless)
!-- RTHRATEN      radiative temp forcing for Grell-Devenyi scheme
!-- RTHBLTEN      PBL temp forcing for Grell-Devenyi scheme
!-- RQVBLTEN      PBL moisture forcing for Grell-Devenyi scheme
!-- RTHFTEN
!-- RQVFTEN
!-- MASS_FLUX
!-- XF_ENS
!-- PR_ENS
!-- warm_rain
!-- CU_ACT_FLAG
!-- W0AVG         average vertical velocity, (for KF scheme) (m/s)
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
!
!======================================================================

   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                                        kts,kte, &
                                      itimestep, num_tiles
   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::                             &
                           ensdim,maxiens,maxens,maxens2,maxens3

   INTEGER,      INTENT(IN   )    ::   cu_physics
   INTEGER,      INTENT(IN   )    ::   STEPCU
   LOGICAL,      INTENT(IN   )    ::   warm_rain

   INTEGER,DIMENSION( ims:ime, jms:jme ),                        &
           INTENT(IN ) ::                                LOWLYR

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                          &
                                                           dz8w  &
                                                      ,     p8w  &
                                                      ,       p  &
                                                      ,      pi  &
                                                      ,       u  &
                                                      ,       v  &
                                                      ,      th  &
                                                      ,       t  &
                                                      ,     rho  &
                                                      ,       w

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT)  ::                                       &
                                                          W0AVG

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
                                                       HT,XLAND

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                RAINC  &
                                                    ,    RAINCV  &
                                                    ,       NCA  & 
                                                    ,      HTOP  & 
                                                    ,      HBOT  &
                                                    ,    CLDEFI 
 

   INTEGER, DIMENSION( ims:ime , jms:jme ),                      &
                    INTENT(IN) ::                          KPBL


   LOGICAL, DIMENSION( ims:ime , jms:jme ),                      &
          INTENT(INOUT) :: CU_ACT_FLAG

   REAL,  INTENT(IN   ) :: DT, DX

!
! optional arguments
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      ! optional moisture tracers
                      ! 2 time levels; if only one then use CURR
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr                  & 
                     ,qv_prev, qc_prev, qr_prev                  & 
                     ,qi_prev, qs_prev, qg_prev                  &
                      ! optional moisture and other tendencies
                     ,rqvcuten,rqccuten,rqrcuten                 &
                     ,rqicuten,rqscuten,rqgcuten                 &
                     ,rqvblten,rqvften                           &
                     ,rthraten,rthblten                          &
                     ,rthften,rthcuten

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                    OPTIONAL,                                    &
                    INTENT(INOUT) ::                             &
                apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma      &
               ,apr_capme,apr_capmi                              &
                                                    , MASS_FLUX

   REAL, DIMENSION( ims:ime , jms:jme , 1:ensdim ),              &
          OPTIONAL,                                              &
          INTENT(INOUT) ::                       XF_ENS, PR_ENS

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

!-----------------------------------------------------------------

   IF (cu_physics .eq. 0) return

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

     CASE (KFSCHEME)
          CALL wrf_debug(100,'in kfcps')

          CALL KFCPS(                                           &
              ! order independent arguments
                DT=dt ,KTAU=itimestep ,DX=dx ,RHO=rho           &
               ,U=u ,V=v ,TH=th ,T=t ,W=w                       &
               ,PCPS=p ,PI=pi                                   &
               ,XLV0=xlv0 ,XLV1=xlv1 ,XLS0=xls0 ,XLS1=xls1      &
               ,RAINCV=raincv ,NCA=nca                          &
               ,DZ8W=dz8w                                       &
               ,W0AVG=w0avg                                     &
               ,CP=cp ,R=r_d ,G=g ,EP1=ep_1 ,EP2=ep_2           &
               ,SVP1=svp1 ,SVP2=svp2 ,SVP3=svp3 ,SVPT0=svpt0    &
               ,STEPCU=stepcu                                   &
               ,CU_ACT_FLAG=cu_act_flag                         &
               ,WARM_RAIN=warm_rain                             &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              ! optionals
               ,RTHCUTEN=rthcuten ,RQVCUTEN=rqvcuten            &
               ,RQCCUTEN=rqccuten ,RQRCUTEN=rqrcuten            &
               ,RQICUTEN=rqicuten ,RQSCUTEN=rqscuten            &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )

     CASE (BMJSCHEME)
          CALL wrf_debug(100,'in bmj_cps')
          CALL BMJDRV(                                          &
                TH=th,T=T ,RAINCV=raincv, RHO=rho               &
               ,DT=dt ,ITIMESTEP=itimestep ,STEPCU=stepcu       &
               ,CUTOP=htop, CUBOT=hbot, KPBL=kpbl               &
               ,DZ8W=dz8w ,PINT=p8w, PMID=p, PI=pi              &
               ,CP=cp ,R=r_d ,ELWV=xlv ,ELIV=xls ,G=g           &
               ,TFRZ=svpt0 ,D608=ep_1 ,CLDEFI=cldefi            &
               ,LOWLYR=lowlyr ,XLAND=xland                      &
               ,CU_ACT_FLAG=cu_act_flag                         &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              ! optionals
               ,RTHCUTEN=rthcuten ,RQVCUTEN=rqvcuten            &
                                                                )

     CASE (KFETASCHEME)
          CALL wrf_debug(100,'in kf_eta_cps')
          CALL KF_ETA_CPS(                                      &
                U=u ,V=v ,TH=th ,T=t ,W=w ,RHO=rho              &
               ,RAINCV=raincv, NCA=nca ,DZ8W=dz8w               &
               ,PCPS=p, PI=pi ,W0AVG=W0AVG                      &
               ,CUTOP=HTOP,CUBOT=HBOT                           &
               ,XLV0=XLV0 ,XLV1=XLV1 ,XLS0=XLS0 ,XLS1=XLS1      &
               ,CP=CP ,R=R_d ,G=G ,EP1=EP_1 ,EP2=EP_2           &
               ,SVP1=SVP1 ,SVP2=SVP2 ,SVP3=SVP3 ,SVPT0=SVPT0    &
               ,DT=dt ,KTAU=itimestep ,DX=dx                    &
               ,STEPCU=stepcu                                   &
               ,CU_ACT_FLAG=cu_act_flag ,warm_rain=warm_rain    &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              ! optionals
               ,RTHCUTEN=rthcuten                               &
               ,RQVCUTEN=rqvcuten ,RQCCUTEN=rqccuten            &
               ,RQRCUTEN=rqrcuten ,RQICUTEN=rqicuten            &
               ,RQSCUTEN=rqscuten                               &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )

     CASE (GDSCHEME)
          CALL wrf_debug(100,'in grelldrv')
          CALL GRELLDRV(                                        &
                DT=dt, ITIMESTEP=itimestep, DX=dx               &
               ,U=u,V=v,T=t,W=w ,RHO=rho                        &
               ,P=p,PI=pi ,Q=qv_curr ,RAINCV=raincv             &
               ,DZ8W=dz8w ,XLV=xlv0,CP=cp,G=g,R_V=r_v           &
               ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
               ,APR_ST=apr_st,APR_AS=apr_as                     &
               ,APR_CAPMA=apr_capma,APR_CAPME=apr_capme         &
               ,APR_CAPMI=apr_capmi,MASS_FLUX=mass_flux         &
               ,XF_ENS=xf_ens,PR_ENS=pr_ens,HT=ht               &
               ,ENSDIM=ensdim,MAXIENS=maxiens,MAXENS=maxens     &
               ,MAXENS2=maxens2,MAXENS3=maxens3                 &
               ,STEPCU=STEPCU                                   &
               ,CU_ACT_FLAG=CU_ACT_FLAG,warm_rain=warm_rain     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              ! optionals
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=RTHFTEN              & 
               ,RTHRATEN=RTHRATEN,RTHBLTEN=RTHBLTEN             &
               ,RQVCUTEN=RQVCUTEN,RQCCUTEN=RQCCUTEN             &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=RQVFTEN              &
               ,RQVBLTEN=RQVBLTEN                               &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )
     CASE (SASSCHEME)
                                                                                                                                           
          CALL wrf_debug(100,'in cu_sas')
            CALL CU_SAS(                                        &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU         &
               ,RAINCV=RAINCV,HTOP=HTOP,HBOT=HBOT               &
               ,U3D=u,V3D=v,W=w,T3D=t,PI3D=pi,RHO3D=rho         &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND            &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              ! optionals
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                                )

     CASE DEFAULT 

         WRITE( wrf_err_message , * ) 'The cumulus option does not exist: cu_physics = ', cu_physics
         CALL wrf_error_fatal ( wrf_err_message )

   END SELECT cps_select

      ENDDO
      !$OMP END PARALLEL DO

   END SUBROUTINE cumulus_driver

END MODULE module_cumulus_driver
