!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_cumulus_driver
CONTAINS
   SUBROUTINE cumulus_driver(grid                                     &
                 ! Order dependent args for domain, mem, and tile dims
                     ,ids,ide, jds,jde, kds,kde                       &
                     ,ims,ime, jms,jme, kms,kme                       &
                     ,ips,ipe, jps,jpe, kps,kpe                       &
                     ,i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                 ! Order independent args (use VAR= in call)
                 ! --Prognostic
                     ,u,v,th,t,w                                      &
                     ,p,pi,rho                                        &
                 ! --Other arguments
                     ,itimestep,dt,dx,cudt,curr_secs,adapt_step_flag  &
                     ,rainc,raincv,pratec,nca                         &
                     ,dz8w,p8w,forcet,forceq                          &
                     ,w0avg,stepcu,gsw                                &
                     ,cldefi,lowlyr,xland,cu_act_flag,warm_rain       &
                     ,htop,hbot,kpbl,ht                               &  
                     ,ensdim,maxiens,maxens,maxens2,maxens3           &
                     ,periodic_x,periodic_y                           &
                 ! Package selection variable
                     ,cu_physics                                      &
                 ! Optional moisture tracers 
                     ,qv_curr, qc_curr, qr_curr                       &
                     ,qi_curr, qs_curr, qg_curr                       & 
                     ,qv_prev, qc_prev, qr_prev                       & 
                     ,qi_prev, qs_prev, qg_prev                       &
                 ! Optional arguments for GD scheme
                     ,apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma     &
                     ,apr_capme,apr_capmi,edt_out,clos_choice         &
                     ,mass_flux,xf_ens,pr_ens,cugd_avedx,imomentum    &
                     ,ishallow,cugd_tten,cugd_qvten,cugd_qcten        &
                     ,cugd_ttens,cugd_qvtens                          &
                     ,gd_cloud,gd_cloud2      &
                     ,k22_shallow,kbcon_shallow,ktop_shallow,xmb_shallow &
                 ! Optional moisture and other tendencies
                     ,rqvcuten,rqccuten,rqrcuten                      &
                     ,rqicuten,rqscuten,rqgcuten                      &
                     ,rqvblten,rqvften                                &
                     ,rthcuten,rthraten,rthblten,rthften              &
#if (NMM_CORE==1)
                 ! for hwrf-sas                      --- 3.2 CLEANUP TODO -- THESE SHOULD BE OPTIONAL, NOT #IF/#ENDIF
                     ,rucuten,rvcuten,mommix,store_rand               &
#endif
                 ! Optional moisture tracer flags 
                     ,f_qv,f_qc,f_qr                                  &
                     ,f_qi,f_qs,f_qg                                  &
                     ,CFU1,CFD1,DFU1,EFU1,DFD1,EFD1,f_flux            &
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
                                          ,SASSCHEME

! *** add new modules of schemes here

   USE module_cu_kf     ,  ONLY : kfcps
   USE module_cu_bmj    ,  ONLY : bmjdrv
#ifdef DM_PARALLEL
   USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks 
# if (EM_CORE == 1)
   USE module_comm_dm   ,  ONLY : halo_cup_g3_in_sub, halo_cup_g3_out_sub
# endif
#endif
   USE module_domain    , ONLY: domain
   USE module_cu_kfeta  , ONLY : kf_eta_cps
   USE module_cu_gd     , ONLY : grelldrv
   USE module_cu_g3     , ONLY : g3drv,conv_grell_spread3d
   USE module_cu_sas
   USE module_wrf_error , ONLY : wrf_err_message

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
!-- CUDT          cumulus time step (minute)
!-- curr_secs     current forecast time (seconds)
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
!-- PRATEC        precipitiation rate from cumulus scheme (mm/s)
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
                   cugd_avedx,clos_choice

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
                                                 GSW,HT,XLAND

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                RAINC  &
                                                    ,    RAINCV  &
                                                    ,       NCA  & 
                                                    ,      HTOP  & 
                                                    ,      HBOT  &
                                                    ,    CLDEFI 
 

   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(INOUT),OPTIONAL :: &
        PRATEC
   REAL, DIMENSION( ims:ime , jms:jme ) :: tmppratec
                                                    
   INTEGER, DIMENSION( ims:ime , jms:jme ),                      &
                    INTENT(IN) ::                          KPBL


   LOGICAL, DIMENSION( ims:ime , jms:jme ),                      &
          INTENT(INOUT) :: CU_ACT_FLAG

   REAL,  INTENT(IN   ) :: DT, DX
   INTEGER,      INTENT(IN   ),OPTIONAL    ::                             &
                   ips,ipe, jps,jpe, kps,kpe,imomentum,ishallow
   REAL,  INTENT(IN   ),OPTIONAL :: CUDT
   REAL,  INTENT(IN   ),OPTIONAL :: CURR_SECS
   LOGICAL,INTENT(IN   ),OPTIONAL    ::     adapt_step_flag
   REAL   :: cudt_pass, curr_secs_pass
   LOGICAL :: adapt_step_flag_pass

#if (NMM_CORE==1)
   REAL, DIMENSION( ims:ime, jms:jme, kms:kme ),                 &
                   INTENT(INOUT) :: rucuten,rvcuten
   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                    INTENT(IN) ::  STORE_RAND
   REAL,            INTENT(INOUT) :: mommix
#endif

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
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr                  & 
                     ,qv_prev, qc_prev, qr_prev                  & 
                     ,qi_prev, qs_prev, qg_prev                  &
                      ! optional moisture and other tendencies
                     ,rqvcuten,rqccuten,rqrcuten                 &
                     ,rqicuten,rqscuten,rqgcuten                 &
                     ,rqvblten,rqvften                           &
                     ,rthraten,rthblten                          &
                     ,cugd_tten,cugd_qvten,cugd_qcten            &
                     ,cugd_ttens,cugd_qvtens                     &
                                                      ,   forcet &
                                                      ,   forceq &
                     ,rthften,rthcuten

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                    OPTIONAL,                                    &
                    INTENT(INOUT) ::                             &
                apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma      &
               ,apr_capme,apr_capmi,edt_out,xmb_shallow          &
                                                    , MASS_FLUX
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                  GD_CLOUD,GD_CLOUD2
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

#if ( WRF_DFI_RADAR == 1 )
!
!  option of cap suppress: 
!        do_capsuppress = 1   do
!        do_capsuppress = other   don't
!
!
   INTEGER,      INTENT(IN   ) ,OPTIONAL   :: do_capsuppress
   REAL, DIMENSION( ims:ime, jms:jme ) :: cap_suppress_loc
#endif

! LOCAL  VAR

   INTEGER :: i,j,k,its,ite,jts,jte,ij
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
   else
      cudt_pass = cudt
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

   IF (cu_physics .eq. 0) return
#if  ( EM_CORE == 1 )
      if(cu_physics .eq. 5 ) then
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij,i,j,k,its,ite,jts,jte )

      DO ij = 1 , num_tiles
        its = i_start(ij)
        ite = i_end(ij)
        jts = j_start(ij)
        jte = j_end(ij)
        do j=jts,min(jte,jde-1)
        do k=kts,kte
        do i=its,min(ite,ide-1)
           RTHFTEN(i,k,j)=(RTHFTEN(i,k,j)+RTHRATEN(i,k,j) &
                               +RTHBLTEN(i,k,j))*pi(i,k,j)
           RQVFTEN(i,k,j)=RQVFTEN(i,k,j)+RQVBLTEN(i,k,j)
       enddo
       enddo
       enddo
      ENDDO
      !$OMP END PARALLEL DO
#ifdef DM_PARALLEL
#include "HALO_CUP_G3_IN.inc"
#endif
      endif
#endif 

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
                DT=dt ,KTAU=itimestep ,DX=dx , CUDT=cudt_pass   &
               ,CURR_SECS=curr_secs_pass                        &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,RHO=rho                                         &
               ,U=u ,V=v ,TH=th ,T=t ,W=w                       &
               ,PCPS=p ,PI=pi                                   &
               ,XLV0=xlv0 ,XLV1=xlv1 ,XLS0=xls0 ,XLS1=xls1      &
               ,RAINCV=raincv, PRATEC=tmppratec, NCA=nca        &
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
                TH=th,T=T ,RAINCV=raincv, PRATEC=tmppratec      &
               ,RHO=rho                                         &
               ,DT=dt ,ITIMESTEP=itimestep ,STEPCU=stepcu       &
               ,CUDT=cudt_pass                                  &
               ,CURR_SECS=curr_secs_pass                        &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
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
               ,CUDT=cudt_pass                                  &
               ,CURR_SECS=curr_secs_pass                        &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,RAINCV=raincv, PRATEC=tmppratec, NCA=nca        &
               ,DZ8W=dz8w                                       &
               ,PCPS=p, PI=pi ,W0AVG=W0AVG                      &
               ,CUTOP=HTOP,CUBOT=HBOT                           &
               ,XLV0=XLV0 ,XLV1=XLV1 ,XLS0=XLS0 ,XLS1=XLS1      &
               ,CP=CP ,R=R_d ,G=G ,EP1=EP_1 ,EP2=EP_2           &
               ,SVP1=SVP1 ,SVP2=SVP2 ,SVP3=SVP3 ,SVPT0=SVPT0    &
               ,DT=dt ,KTAU=itimestep ,DX=dx                    &
               ,STEPCU=stepcu                                   &
               ,CU_ACT_FLAG=cu_act_flag ,WARM_RAIN=warm_rain    &
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
               ,DZ8W=dz8w,P8W=p8w,XLV=xlv,CP=cp,G=g,R_V=r_v     &
               ,PRATEC=tmppratec                                &
               ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
               ,APR_ST=apr_st,APR_AS=apr_as                     &
               ,APR_CAPMA=apr_capma,APR_CAPME=apr_capme         &
               ,APR_CAPMI=apr_capmi,MASS_FLUX=mass_flux         &
               ,XF_ENS=xf_ens,PR_ENS=pr_ens,HT=ht               &
               ,xland=xland,gsw=gsw                             &
               ,GDC=gd_cloud,GDC2=gd_cloud2 &
               ,ENSDIM=ensdim,MAXIENS=maxiens,MAXENS=maxens     &
               ,MAXENS2=maxens2,MAXENS3=maxens3                 &
               ,STEPCU=STEPCU,htop=htop,hbot=hbot               &
               ,CU_ACT_FLAG=CU_ACT_FLAG,warm_rain=warm_rain     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,PERIODIC_X=periodic_x,PERIODIC_Y=periodic_y     &
              ! optionals
#if (NMM_CORE == 1 )
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=forcet               &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=forceq               &
#else
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=RTHFTEN              &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=RQVFTEN              &
#endif
               ,RTHRATEN=RTHRATEN,RTHBLTEN=RTHBLTEN             &
               ,RQVCUTEN=RQVCUTEN,RQCCUTEN=RQCCUTEN             &
               ,RQVBLTEN=RQVBLTEN                               &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
               ,CFU1=CFU1,CFD1=CFD1,DFU1=DFU1,EFU1=EFU1         &
               ,DFD1=DFD1,EFD1=EFD1,f_flux=l_flux               )
          CALL wrf_debug(200,'back from grelldrv')

     CASE (SASSCHEME)
                                                                                                                                           
          IF ( adapt_step_flag_pass ) THEN
            WRITE( wrf_err_message , * ) 'The SAS cumulus option will not work properly with an adaptive time step'
            CALL wrf_error_fatal ( wrf_err_message )
          END IF
          CALL wrf_debug(100,'in cu_sas')
          CALL CU_SAS(                                          &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU         &
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
#if (NMM_CORE==1)
               ,RUCUTEN=RUCUTEN, RVCUTEN=RVCUTEN                &
#endif
               ,RAINCV=RAINCV,PRATEC=tmpPRATEC,HTOP=HTOP,HBOT=HBOT &
               ,U3D=u,V3D=v,W=w,T3D=t                           &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,PI3D=pi,RHO3D=rho                               &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND            &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,P_QC=p_qc                                       &
#if (NMM_CORE==1)
               ,store_rand=store_rand  &
               ,MOMMIX=MOMMIX  &
#endif
               ,P_QI=p_qi,P_FIRST_SCALAR=param_first_scalar     &
               ,CUDT=cudt_pass                                  &
               ,CURR_SECS=curr_secs_pass                        &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
                                                                )
     CASE (G3SCHEME)
          CALL wrf_debug(100,'in grelldrv')
#if ( WRF_DFI_RADAR == 1 )
          if (do_capsuppress == 1) then
            WRITE( wrf_err_message , * ) 'G3 do CAP suppress',its,jts,min( jte,jde-1 ),min( ite,ide-1 ),kte
            CALL wrf_debug(200, wrf_err_message)
            DO j = jts, min( jte,jde-1 )
            DO i = its, min( ite,ide-1 )
               cap_suppress_loc(i,j) = grid%dfi_tten_rad(i,kte,j)
            ENDDO
            ENDDO
         endif
#endif
          CALL G3DRV(                                           &
                DT=dt, ITIMESTEP=itimestep, DX=dx               &
               ,U=u,V=v,T=t,W=w ,RHO=rho                        &
               ,P=p,PI=pi,Q=qv_curr,RAINCV=raincv               &
               ,DZ8W=dz8w ,P8W=p8w,XLV=xlv,CP=cp,G=g,R_V=r_v    &
               ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
               ,APR_ST=apr_st,APR_AS=apr_as,PRATEC=tmppratec    &
               ,APR_CAPMA=apr_capma,APR_CAPME=apr_capme         &
               ,APR_CAPMI=apr_capmi,MASS_FLUX=mass_flux         &
               ,XF_ENS=xf_ens,PR_ENS=pr_ens,HT=ht               &
               ,xland=xland,gsw=gsw,edt_out=edt_out             &
               ,GDC=gd_cloud,GDC2=gd_cloud2,kpbl=kpbl           &
               ,k22_shallow=k22_shallow                         &
               ,kbcon_shallow=kbcon_shallow                     &
               ,ktop_shallow=ktop_shallow                       &
               ,xmb_shallow=xmb_shallow                         &
               ,cugd_tten=cugd_tten,cugd_qvten=cugd_qvten       &
               ,cugd_ttens=cugd_ttens,cugd_qvtens=cugd_qvtens   &
               ,cugd_qcten=cugd_qcten,cugd_avedx=cugd_avedx     &
               ,imomentum=imomentum,ishallow_g3=ishallow        &
               ,ENSDIM=ensdim,MAXIENS=maxiens,MAXENS=maxens     &
               ,MAXENS2=maxens2,MAXENS3=maxens3,ichoice=clos_choice &
               ,STEPCU=STEPCU,htop=htop,hbot=hbot               &
               ,CU_ACT_FLAG=CU_ACT_FLAG,warm_rain=warm_rain     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,IPS=ips,IPE=ipe,JPS=jps,JPE=jpe,KPS=kps,KPE=kpe &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,PERIODIC_X=periodic_x,PERIODIC_Y=periodic_y     &
              ! optionals
#if (NMM_CORE == 1 )
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=forcet               &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=forceq               &
#else
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=RTHFTEN              &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=RQVFTEN              &
               ,rqvblten=rqvblten,rthblten=rthblten             &
#endif
               ,RQVCUTEN=RQVCUTEN,RQCCUTEN=RQCCUTEN             &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
#if ( WRF_DFI_RADAR == 1 )
                 ! Optional CAP suppress option
              ,do_capsuppress=do_capsuppress                    &
              ,cap_suppress_loc=cap_suppress_loc                &
#endif                                 
                                                                )

     CASE DEFAULT 

         WRITE( wrf_err_message , * ) 'The cumulus option does not exist: cu_physics = ', cu_physics
         CALL wrf_error_fatal ( wrf_err_message )

   END SELECT cps_select

      ENDDO
      !$OMP END PARALLEL DO
#if  ( EM_CORE == 1 )
   IF(cu_physics .eq. 5 )then
#ifdef DM_PARALLEL
#     include "HALO_CUP_G3_OUT.inc"
#endif
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij ,its,ite,jts,jte, i,j,k)
      DO ij = 1 , num_tiles
        its = i_start(ij)
        ite = i_end(ij)
        jts = j_start(ij)
        jte = j_end(ij)

        call conv_grell_spread3d(rthcuten=rthcuten,rqvcuten=rqvcuten                &
     &            ,rqccuten=rqccuten,raincv=raincv,cugd_avedx=cugd_avedx            &
     &            ,cugd_tten=cugd_tten,cugd_qvten=cugd_qvten,rqicuten=rqicuten      &
     &            ,cugd_ttens=cugd_ttens,cugd_qvtens=cugd_qvtens                    &
     &            ,cugd_qcten=cugd_qcten,pi_phy=pi,moist_qv=qv_curr                 &
     &            ,PRATEC=tmppratec,dt=dt,num_tiles=num_tiles                       &
     &            ,imomentum=imomentum                             &
     &            ,F_QV=F_QV,F_QC=F_QC,F_QR=F_QR,F_QI=F_QI,F_QS=F_QS                &
     &            ,ids=IDS,ide=IDE, jds=JDS,jde=JDE, kds=KDS,kde=KDE                &
     &            ,ips=IPS,ipe=IPE, jps=JPS,jpe=JPE, kps=KPS,kpe=KPE                &
     &            ,ims=IMS,ime=IME, jms=JMS,jme=JME, kms=KMS,kme=KME                &
     &            ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte)
      ENDDO
      !$OMP END PARALLEL DO
   endif
#endif

      !
      ! Copy pratec back to output array, if necessary.
      !
      if (PRESENT(PRATEC)) then
         pratec(:,:) = tmppratec(:,:)
      endif
          CALL wrf_debug(200,'returning from cumulus_driver')
   END SUBROUTINE cumulus_driver

END MODULE module_cumulus_driver
