! Note: Comments starting with "!!" are directly taken from CAM's driver routine for the micro-physics (stratiform.F90)
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov

!----------------------------------------------------------------------------------------------------------
!Possible future Modifications:
!1. *** IMPORTANT-WARNING *** conv_water_in_rad is hardwired to be equal to 1. It should be a namelist variable
!2. QME3D computation is WRONG as it doesn't include CMELIQ contribution as CMELIQ is 
!   computed in macrophysics, which is not ported into WRF yet
!3. Computations such as cldfsnow,icswp etc. are not correct as CONCLD (convective 
!   cloud fraction) variable is not avaible to microphysics
!4. The code ouputs variables which can be used in RRTMG radiation but RRTMG radiation doesn't 
!   use these variables as of now
!5. More robust cloud fraction calculation can be included in the future versions
!----------------------------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------------------------
!Important Notes:
!1. New cloud fraction computations are added by Po-Lun Ma (PMA)
!2. Macrophysics processes are added by PMA into this driver
!----------------------------------------------------------------------------------------------------------

#define MODAL_AERO
module module_mp_cammgmp_driver
  
  !!-------------------------------------------------------------------------------------------------------
  !! Purpose:
  !!
  !! Provides the CAM interface to the prognostic cloud microphysics
  !!
  !! Author: Andrew Gettelman, Cheryl Craig, October 2010
  !! Origin: modified from stratiform.F90 
  !!         (Boville 2002, Coleman 2004, Park 2009, Kay 2010)
  ! Ported to WRF by Balwinder.Singh@pnnl.gov
  !!-------------------------------------------------------------------------------------------------------
  
  use shr_kind_mod,  only: r8=>shr_kind_r8
  use physconst,     only: gravit  
  use module_cam_support,  only: pcnst =>pcnst_mp, pcols, pver, pverp
#if ( WRF_CHEM == 1 )
   use module_cam_support, only: cam_mam_aerosols
#endif
  use constituents,  only: cnst_get_ind, cnst_name, qmin

#if ( WRF_CHEM == 1 )
  use module_state_description,  only: num_chem, param_first_scalar, &
         CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ, CBMZ_CAM_MAM3_NOAQ,     &
         CBMZ_CAM_MAM3_AQ
#endif

  
  implicit none
  private
  save
  
  public :: CAMMGMP_INIT
  public :: CAMMGMP
#if ( WRF_CHEM == 1 )
  public :: physics_ptend_init ! Mimics CAM's physics ptend init. Also used in wet scavaging code in WRF_CHEM
  public :: physics_update     ! Mimics CAM's physics update. Also used in wet scavaging code in WRF_CHEM 
#endif
  
  !! ------------------------- !
  !! Private Module Parameters !
  !! ------------------------- !
  
  !! Choose either 'intermediate' ('inter') or complete ('compl') cloud microphysics 
  !! inter : Microphysics assumes 'liquid stratus frac = ice stratus frac = max( liquid stratus frac, ice stratus frac )'.
  !! compl : Microphysics explicitly treats 'liquid stratus frac .ne. ice stratus frac'  
  !! for CAM5, only 'inter' is functional
  
  character(len=5), private, parameter :: micro_treatment = 'inter' 
  
  logical, private :: sub_column = .false. ! True = configure microphysics for sub-columns 
  !! False = use in regular mode w/o sub-columns
  
  !! -------------------------------- !
  !! End of Private Module Parameters !
  !! -------------------------------- !
  
  integer :: &
       ixcldliq,     &! cloud liquid amount index
       ixcldice,     &! cloud ice amount index
       ixnumliq,     &! cloud liquid number index
       ixnumice       ! cloud ice water index
  integer  :: chem_opt, ptend_top_level, ptend_bot_level

  real(r8), parameter :: qthresh_mass = 1.0e-11 ! Typical val: 1e-6 kg/kg ; Negligible val: 1e-11 kg/kg
  real(r8), parameter :: qthresh_numl = 5.0e2   ! Typical val: 5E7  #/kg  ; Negligible val: 5e2  #/kg
  real(r8), parameter :: qthresh_numi = 1.0e-1  ! Typical val: 1E4  #/kg  ; Negligible val: 1e-1 #/kg

  !Following constants (dp1 and dp2) are  obtained from cloud_fraction.F90 of CAM for highest resolution(0.23x0.31)
  real(r8), parameter :: dp1 = 0.10_r8 
  real(r8), parameter :: dp2 = 500.0_r8

contains
  
  subroutine CAMMGMP(itimestep, dt, p8w, p_hyd   &
       , t_phy, pi_phy, z_at_w, qfx              &
       , tke_pbl, turbtype3d, smaw3d             &
       , dlf3d, dlf2_3d, rliq2d, z_sea_level     &
       , kvh3d, ht, alt, accum_mode              &
       , aitken_mode, coarse_mode, icwmrsh3d     &
       , icwmrdp3d, shfrc3d, cmfmc3d, cmfmc2_3d  &
       , config_flags, f_ice_phy, f_rain_phy     &
#if ( WRF_CHEM == 1 )                     
       , dgnum4d, dgnumwet4d                     &
#endif
       , ids, ide,  jds, jde,  kds, kde          &
       , ims, ime,  jms, jme,  kms, kme          &
       , its, ite,  jts, jte,  kts, kte          &
       !Output variables from CAMMGMP
       , th, cldfra_old_mp, cldfra_mp            &
       ,cldfra_mp_all, lradius, iradius, cldfrai &
       , cldfral, cldfra_conv,wsedl3d, rainnc    &
       , rainncv, snownc, snowncv,sr             &
       , qv_curr, qc_curr, qi_curr,qs_curr       &
       , qr_curr, nc3d, ni3d,ns3d,nr3d,qndrop    &
       , rh_old_mp,lcd_old_mp                    & !PMA- added for macrophysics
#if ( WRF_CHEM == 1 )                     
       , chem                                    &
       , qme3d,prain3d,nevapr3d                  &
       , rate1ord_cw2pr_st3d                     &
#endif
       , xland,snowh                            )
    
    !!-------------------------------------------------------- !  
    !!                                                         ! 
    !! Purpose:                                                !
    !!                                                         !
    !! Interface to sedimentation, detrain, cloud fraction and !
    !!        cloud macro - microphysics subroutines           !
    !!                                                         ! 
    !! Author: D.B. Coleman                                    !
    !! Date: Apr 2004                                          !
    !!                                                         !
    !  Polun Ma added simple macrophysics scheme
    !!-------------------------------------------------------- !
    use shr_kind_mod,             only: r8 => shr_kind_r8
    use cldwat2m_micro,           only: mmicro_pcond
    use microp_aero,              only: microp_aero_ts 
    use physconst,                only: cpair, tmelt ! tmelt is added for computing saturation specific humidity for ice and liquid
    
#ifdef MODAL_AERO
    use modal_aero_data,          only: modeptr_accum => modeptr_accum_mp, &
         numptr_amode   => numptr_amode_mp  , lspectype_amode => lspectype_amode_mp, &
         specdens_amode => specdens_amode_mp, voltonumb_amode => voltonumb_amode_mp, &
         lmassptr_amode => lmassptr_amode_mp, modeptr_aitken  => modeptr_aitken_mp,  &
         modeptr_coarse => modeptr_coarse_mp, ntot_amode      => ntot_amode_mp,      &
         nspec_amode    => nspec_amode_mp
#endif
    
#if ( WRF_CHEM == 1 )       
    use module_data_cam_mam_asect, only: lptr_chem_to_q, lptr_chem_to_qqcw, factconv_chem_to_q
#endif
    
    use wv_saturation,             only: epsqs, polysvp ! Added for computing saturation specific humidity for ice and liquid
    use conv_water,                only: conv_water_4rad
    use cldwat,                    only: cldwat_fice
    use module_state_description,  only: CAMZMSCHEME, CAMUWSHCUSCHEME,F_QV, F_QC, F_QI, F_QS
    use module_configure,          only: grid_config_rec_type
    
    implicit none    
    !
    ! Input arguments
    !
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
    integer, intent(in) :: itimestep       !Timestep count
    
    integer, intent(in) :: ids,ide, jds,jde, kds,kde
    integer, intent(in) :: ims,ime, jms,jme, kms,kme
    integer, intent(in) :: its,ite, jts,jte, kts,kte
    
    real,    intent(in) :: dt              !Timestep
    
    real,    intent(in) :: accum_mode, aitken_mode, coarse_mode     
    
    real, dimension( ims:ime, jms:jme ), intent(in) :: ht     !Terrain height (m)
    real, dimension( ims:ime, jms:jme ), intent(in) :: qfx    !Upward moisture flux at the surface(kg m-2 s-1)
    real, dimension( ims:ime, jms:jme ), intent(in) :: rliq2d !Vertically-integrated reserved cloud condensate(m s-1)
    real, dimension( ims:ime, jms:jme ), intent(in) :: xland
    real, dimension( ims:ime, jms:jme ), intent(in) :: snowh
    
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: dlf3d       !Detraining cloud water tendency from convection
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: dlf2_3d     !dq/dt due to export of cloud water into environment by shallow convection(kg/kg/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: p8w         !Hydrostatic Pressure at level interface (Pa)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: p_hyd       !Hydrostatic pressure(Pa)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: z_sea_level !Height above sea level at mid-level (m) 
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: t_phy       !Temperature (K)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: z_at_w      !Height above sea level at layer interfaces (m) 
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: pi_phy      !Dimensionless pressure [unitless]
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: tke_pbl     !Turbulence kinetic energy from Mellor-Yamada-Janjic (MYJ) (m^2/s^2)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: kvh3d       !Eddy diffusivity for heat(m^2/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: turbtype3d  !Turbulent interface types [ no unit ]
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: smaw3d      !Normalized Galperin instability function for momentum  ( 0<= <=4.964 and 1 at neutral ) [no units]
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: alt         !inverse density(m3/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: F_ICE_PHY   !Fraction of ice
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in)    :: F_RAIN_PHY  !Fraction of rain
    !For conv_water_4rad
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: shfrc3d        !Shallow cloud fraction
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cmfmc3d        !Deep + Shallow Convective mass flux [ kg /s/m^2 ]
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cmfmc2_3d      !Shallow convective mass flux [ kg/s/m^2 ]
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: icwmrsh3d      !Shallow cumulus in-cloud water mixing ratio (kg/m2)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: icwmrdp3d      !Deep Convection in-cloud water mixing ratio (kg/m2)

#if ( WRF_CHEM == 1 ) 
    real, dimension( ims:ime, kms:kme, jms:jme, ntot_amode ), intent(in) :: dgnum4d, dgnumwet4d ! 4-dimensional Number mode diameters
#endif
    !2d in-outs
    real, dimension( ims:ime , jms:jme ), intent(inout) :: rainnc              !grid scale precipitation (mm)
    real, dimension( ims:ime , jms:jme ), intent(inout) :: rainncv             !one time step grid scale precipitation (mm/step)
    real, dimension( ims:ime , jms:jme ), intent(inout) :: snownc              !grid scale snow and ice (mm)
    real, dimension( ims:ime , jms:jme ), intent(inout) :: snowncv             !one time step grid scale snow and ice (mm/step)
    
    !3d in-outs
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfra_old_mp  !Old Cloud fraction 
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rh_old_mp      !Old RH
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: lcd_old_mp     !Old liquid cloud fraction
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qv_curr        !Water vapor mixing ratio - Moisture  (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qc_curr        !Cloud water mixing ratio - Cloud liq (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qi_curr        !Ice mixing ratio         -Cloud ice  (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qs_curr        !Snow mixing ratio         -Cloud ice  (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qr_curr        !Rain mixing ratio         -Cloud ice  (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: ni3d           !Cloud ice number concentration (#/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: nc3d           !Cloud water  number concentration (#/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: ns3d           !Snow number concentration (#/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: nr3d           !rain number concentration (#/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qndrop         !droplet number mixing ratio (#/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: th             !Potential temperature (K)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: wsedl3d        !Sedimentation velocity of stratiform liquid cloud droplet (m/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfra_mp      !New Cloud fraction 
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfra_conv    !New Cloud fraction 
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfra_mp_all  !New Cloud fraction 
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: lradius
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: iradius
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfrai
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: cldfral
#if ( WRF_CHEM == 1 )
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qme3d          !Net condensation rate (kg/kg/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: prain3d        !Rate of conversion of condensate to precipitation (kg/kg/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: nevapr3d       !Evaporation rate of rain + snow (kg/kg/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rate1ord_cw2pr_st3d !1st order rate for direct conversion of strat. cloud water to precip (1/s)
#endif

#if ( WRF_CHEM == 1 )    
    !4d in-outs
    real, dimension( ims:ime, kms:kme, jms:jme, num_chem ), intent(inout) :: chem !Chem array
#endif
    !2d outs
    real, dimension( ims:ime , jms:jme ), intent(  out) :: sr                     !one time step mass ratio of snow to total precip    
    
    
    !
    ! Local variables
    !
    
    !Other variables
    character*250 :: wrfmessage
    logical  :: is_first_step                             !Flag to know if this a first time step
    logical  :: ptend_loc_ls, ptend_all_ls                !Flag for updating tendencies
    logical  :: ptend_loc_lq(pcnst),ptend_all_lq(pcnst)   !Flag for updating tendencies
    integer   :: i,k,m,n,iw,jw,kw,imode,itsm1,itile_len,ktep1,kflip,ips,kcam
#if ( WRF_CHEM == 1 )
    integer  :: p1st,l2                                   !For iterating loop of NUM_CHEM for CHEM array
#endif
    integer   :: lchnk                                    !Chunk identifier
    integer   :: ncol                                     !Number of atmospheric columns
    integer   :: conv_water_in_rad                        
    real (r8) :: multFrc
    real(r8)  :: dtime                                    !Timestep in real(8) format
    real(r8)  :: dp

    real(r8) :: phis(pcols)                               !Geopotential at terrain height (m2/s2)
    real(r8) :: state_t(pcols,kte)
    real(r8) :: state_q(pcols,kte,pcnst)
    real(r8) :: state_s(pcols,kte)
    real(r8) :: state_pmid(pcols,kte)
    real(r8) :: state_pdel(pcols,kte)
    real(r8) :: state_rpdel(pcols,kte)
    real(r8) :: state_zm(pcols,kte)
    real(r8) :: state_omega(pcols,kte) 
    
    real(r8) :: state_pint(pcols,kte+1)
    
    real(r8) :: state1_t(pcols,kte)
    real(r8) :: state1_q(pcols,kte,pcnst)
    real(r8) :: state1_s(pcols,kte)
    real(r8) :: state1_pmid(pcols,kte)
    real(r8) :: state1_pdel(pcols,kte)
    real(r8) :: state1_rpdel(pcols,kte)
    real(r8) :: state1_zm(pcols,kte)
    real(r8) :: state1_omega(pcols,kte) 
    
    real(r8) :: state1_pint(pcols,kte+1)
    
    character*24 :: ptend_loc_name 
    real(r8) :: ptend_loc_s(pcols,kte)
    real(r8) :: ptend_loc_q(pcols,kte,pcnst)
    
    character*24 :: ptend_all_name 
    real(r8) :: ptend_all_s(pcols,kte)
    real(r8) :: ptend_all_q(pcols,kte,pcnst)

    real(r8) :: tmpnaer,tmpmaer
    real(r8) :: cmeliq(pcols,kte)                 ! Rate of cond-evap of liq within the cloud
    real(r8) :: nrout(pcols,kte)                  ! rain number concentration (1/m3)
    real(r8) :: nsout(pcols,kte)                  ! snow number concentration (1/m3)
    real(r8) :: sh_icwmr(pcols,kte)               ! Shallow conv. cloud water
    real(r8) :: dp_icwmr(pcols,kte)               ! Deep conv. cloud water
    real(r8) :: fice(pcols,kte)                   ! Ice partitioning ratio
    real(r8) :: fsnow(pcols,kte)                  ! Fractional snow content for convection
    real(r8) :: sh_frac(pcols,kte)                ! Shallow convective cloud fraction
    real(r8) :: dp_frac(pcols,kte)                ! Deep convective cloud fraction
    real(r8) :: cmfmc(pcols,kte+1)
    real(r8) :: cmfmc2(pcols,kte+1)
   
    real(r8), dimension(pcols,kte) :: esl         ! liquid sat vapor pressure (pa) ! for phil suggested modifications
    real(r8), dimension(pcols,kte) :: esi         ! ice    sat vapor pressure (pa)
    real(r8), dimension(pcols,kte) :: qvs         ! liquid saturation vapor mixing ratio ! for phil suggested modifications
    real(r8), dimension(pcols,kte) :: qvi         ! ice    saturation vapor mixing ratio


    !Following variables are added by PMA for Macrophysics
    real(r8), dimension(pcols,kte) :: ttt         ! Temperature (PMA)
    real(r8), dimension(pcols,kte) :: rh_old      ! RH at previous timestep (PMA)
    real(r8), dimension(pcols,kte) :: qi_mac      ! Temperature (PMA)


    !End variables addition by PMA


    !Variables with CAM data structure
    real(r8)   :: rliq(pcols)              !Vertical integral of liquid not yet in q(ixcldliq) or vertically-integrated reserved cloud condensate (m/s)
 
    !Following 6 variables are intent-out in CAM's driver for micro-physics
    real(r8)   :: prec_str(pcols)          ! [Total] Sfc flux of precip from stratiform [ m/s ] 
    real(r8)   :: snow_str(pcols)          ! [Total] Sfc flux of snow from stratiform   [ m/s ]
    real(r8)   :: prec_sed(pcols)          ! Surface flux of total cloud water from sedimentation
    real(r8)   :: snow_sed(pcols)          ! Surface flux of cloud ice from sedimentation
    real(r8)   :: prec_pcw(pcols)          ! Sfc flux of precip from microphysics [ m/s ]
    real(r8)   :: snow_pcw(pcols)          ! Sfc flux of snow from microphysics [ m/s ]
    
    real(r8)   :: cflx(pcols,pcnst)        ! Constituent flux from surface    
    real(r8)   :: dlf(pcols,kte)           ! Detrained water from convection schemes
    real(r8)   :: dlf2(pcols,kte)          ! Detrained water from shallow convection scheme
    real(r8)   :: rate1cld(pcols,kte)                    ! array to hold rate1ord_cw2pr_st from microphysics
    
    ! Physics buffer fields

    real(r8), dimension(pcols,kte)   :: cld          ! Total cloud fraction
    real(r8), dimension(pcols,kte)   :: ast          ! Relative humidity cloud fraction
    real(r8), dimension(pcols,kte)   :: aist         ! Physical ice stratus fraction
    real(r8), dimension(pcols,kte)   :: alst         ! Physical liquid stratus fraction
    real(r8), dimension(pcols,kte)   :: concld       ! Convective cloud fraction
    real(r8), dimension(pcols,kte)   :: qme
    real(r8), dimension(pcols,kte)   :: prain        ! Total precipitation (rain + snow)
    real(r8), dimension(pcols,kte)   :: nevapr       ! Evaporation of total precipitation (rain + snow)
    real(r8), dimension(pcols,kte)   :: rel          ! Liquid effective drop radius (microns)
    real(r8), dimension(pcols,kte)   :: rei          ! Ice effective drop size (microns)
    real(r8), dimension(pcols,kte)   :: rel2         ! Liquid effective drop radius (microns)
    real(r8), dimension(pcols,kte)   :: rei2         ! Ice effective drop size (microns)
    real(r8), dimension(pcols,kte)   :: cldo         ! Old cloud fraction
    real(r8), dimension(pcols,kte)   :: wsedl        ! Sedimentation velocity of liquid stratus cloud droplet [ m/s ]
    real(r8), dimension(pcols,kte)   :: rel_fn       ! Ice effective drop size at fixed number (indirect effect) (microns)
    real(r8), dimension(pcols,kte+1) :: kkvh         ! Vertical eddy diffusivity    
#ifdef MODAL_AERO
    real(r8), target, dimension(pcols,kte,pcnst) :: qqcw                ! Cloud-borne aerosol
    real(r8), dimension(pcols,kte,ntot_amode)    :: dgnumwet            ! Number mode diameter
    real(r8), dimension(pcols,kte,ntot_amode)    :: dgnum               ! Number mode diameter
    real(r8), dimension(pcols,kte)               :: rate1ord_cw2pr_st   ! 1st order rate for direct conversion of 
    ! strat. cloud water to precip (1/s)    ! rce 2010/05/01
#endif
    
    
    ! physics buffer fields for COSP simulator
    real(r8), dimension(pcols,kte+1) :: mgflxprc     ! MG grid-box mean flux_large_scale_cloud_rain+snow at interfaces (kg/m2/s)
    real(r8), dimension(pcols,kte+1) :: mgflxsnw     ! MG grid-box mean flux_large_scale_cloud_snow at interfaces (kg/m2/s)
    real(r8), dimension(pcols,kte)   :: mgmrprc      ! MG grid-box mean mixingratio_large_scale_cloud_rain+snow at interfaces (kg/kg)
    real(r8), dimension(pcols,kte)   :: mgmrsnw      ! MG grid-box mean mixingratio_large_scale_cloud_snow at interfaces (kg/kg)
    real(r8), dimension(pcols,kte)   :: mgreffrain   ! MG diagnostic rain effective radius (um)
    real(r8), dimension(pcols,kte)   :: mgreffsnow   ! MG diagnostic snow effective radius (um)
    real(r8), dimension(pcols,kte)   :: cvreffliq    ! convective cloud liquid effective radius (um)
    real(r8), dimension(pcols,kte)   :: cvreffice    ! convective cloud ice effective radius (um)
    
    ! physics buffer fields for radiation
    
    real(r8), dimension(pcols,kte) :: dei          ! Ice effective diameter (meters) (AG: microns?)
    real(r8), dimension(pcols,kte) :: mu           ! Size distribution shape parameter for radiation
    real(r8), dimension(pcols,kte) :: lambdac      ! Size distribution slope parameter for radiation
    real(r8), dimension(pcols,kte) :: iciwp        ! In-cloud ice water path for radiation
    real(r8), dimension(pcols,kte) :: iclwp        ! In-cloud liquid water path for radiation
    
    ! For rrtm optics. specificed distribution.
    
    real(r8) :: mucon                                 ! Convective size distribution shape parameter
    real(r8) :: dcon                                  ! Convective size distribution effective radius (meters)  
    real(r8) :: lamcon                                ! Convective size distribution slope parameter (meters-1)
    real(r8) :: deicon                                ! Convective ice effective diameter (meters)
    
    ! Physics buffer fields
    
    real(r8), dimension(pcols,kte) :: deiconv      ! Ice effective diameter (microns)
    real(r8), dimension(pcols,kte) :: muconv       ! Size distribution shape parameter for radiation
    real(r8), dimension(pcols,kte) :: lambdaconv   ! Size distribution slope parameter for radiation
    real(r8), dimension(pcols,kte) :: iciwpst      ! Stratiform in-cloud ice water path for radiation
    real(r8), dimension(pcols,kte) :: iclwpst      ! Stratiform in-cloud liquid water path for radiation
    real(r8), dimension(pcols,kte) :: iciwpconv    ! Convective in-cloud ice water path for radiation
    real(r8), dimension(pcols,kte) :: iclwpconv    ! Convective in-cloud liquid water path for radiation
    
    real(r8), dimension(pcols,kte+1) :: tke          ! TKE from the moist PBL scheme
    real(r8), dimension(pcols,kte+1) :: turbtype     ! Turbulence type from the moist PBL scheme
    real(r8), dimension(pcols,kte+1) :: smaw         ! Instability function of momentum from the moist PBL scheme
    
    ! Local variables for in-cloud water quantities adjusted for convective water
    
    real(r8)  allcld_ice (pcols,kte)                 ! All-cloud cloud ice
    real(r8)  allcld_liq (pcols,kte)                 ! All-cloud liquid
    
    ! Snow
    
    real(r8), dimension(pcols,kte) :: cldfsnow     ! Cloud fraction for liquid+snow
    real(r8), dimension(pcols,kte) :: icswp        ! In-cloud snow water path
    real(r8), dimension(pcols,kte) :: des          ! Snow effective diameter (m)
    real(r8)  qsout(pcols,kte)                       ! Snow mixing ratio
    
    real(r8) :: qrout(pcols,kte)                     ! Rain mixing ratio
    real(r8) :: rflx(pcols,kte+1)                   ! grid-box average rain flux (kg m^-2 s^-1)
    real(r8) :: sflx(pcols,kte+1)                   ! grid-box average snow flux (kg m^-2 s^-1)
    real(r8) :: reff_rain(pcols,kte)                ! rain effective radius (um)
    real(r8) :: reff_snow(pcols,kte)                ! snow effective radius (um)
    
    real(r8)  icecldf(pcols,kte)                     ! Ice cloud fraction
    real(r8)  liqcldf(pcols,kte)                     ! Liquid cloud fraction (combined into cloud)
    real(r8)  icecldf_out(pcols,kte)                 ! Ice cloud fraction
    real(r8)  liqcldf_out(pcols,kte)                 ! Liquid cloud fraction (combined into cloud)
    
    ! Local variables for microphysics
    
    real(r8)  rdtime                                  ! 1./dtime
    real(r8)  qtend(pcols,kte)                       ! Moisture tendencies
    real(r8)  ttend(pcols,kte)                       ! Temperature tendencies
    real(r8)  ltend(pcols,kte)                       ! Cloud liquid water tendencies
    real(r8)  evapsnow(pcols,kte)                    ! Local evaporation of snow
    real(r8)  prodsnow(pcols,kte)                    ! Local production of snow
    real(r8)  icimr(pcols,kte)                       ! In cloud ice mixing ratio
    real(r8)  icwmr(pcols,kte)                       ! In cloud water mixing ratio
    real(r8)  icimrst(pcols,kte)                     ! In stratus ice mixing ratio
    real(r8)  icwmrst(pcols,kte)                     ! In stratus water mixing ratio
    real(r8)  icimrst_out(pcols,kte)                 ! In stratus ice mixing ratio
    real(r8)  icwmrst_out(pcols,kte)                 ! In stratus water mixing ratio
    real(r8)  cmeice(pcols,kte)                      ! Rate of cond-evap of ice within the cloud
    real(r8)  temp(pcols)
    real(r8)  res(pcols,kte)
    
    ! MG micro diagnostics
    
    real(r8)  qcsevap(pcols,kte)                     ! Evaporation of falling cloud water
    real(r8)  qisevap(pcols,kte)                     ! Sublimation of falling cloud ice
    real(r8)  qvres(pcols,kte)                       ! Residual condensation term to remove excess saturation
    real(r8)  cmeiout(pcols,kte)                     ! Deposition/sublimation rate of cloud ice
    real(r8)  vtrmc(pcols,kte)                       ! Mass-weighted cloud water fallspeed
    real(r8)  vtrmi(pcols,kte)                       ! Mass-weighted cloud ice fallspeed
    real(r8)  qcsedten(pcols,kte)                    ! Cloud water mixing ratio tendency from sedimentation
    real(r8)  qisedten(pcols,kte)                    ! Cloud ice mixing ratio tendency from sedimentation
    
    real(r8)  prao(pcols,kte)  
    real(r8)  prco(pcols,kte)  
    real(r8)  mnuccco(pcols,kte)  
    real(r8)  mnuccto(pcols,kte)  
    real(r8)  mnuccdo(pcols,kte)
    real(r8)  mnuccdohet(pcols,kte)
    real(r8)  msacwio(pcols,kte)  
    real(r8)  psacwso(pcols,kte)  
    real(r8)  bergso(pcols,kte)  
    real(r8)  bergo(pcols,kte)  
    real(r8)  melto(pcols,kte)  
    real(r8)  homoo(pcols,kte)  
    real(r8)  qcreso(pcols,kte)  
    real(r8)  prcio(pcols,kte)  
    real(r8)  praio(pcols,kte)  
    real(r8)  qireso(pcols,kte)
    real(r8)  ftem(pcols,kte)
    real(r8)  mnuccro(pcols,kte) 
    real(r8)  pracso (pcols,kte) 
    real(r8)  meltsdt(pcols,kte) 
    real(r8)  frzrdt (pcols,kte) 
    real(r8)  dpdlfliq(pcols,kte)
    real(r8)  dpdlfice(pcols,kte)
    real(r8)  shdlfliq(pcols,kte)
    real(r8)  shdlfice(pcols,kte)
    real(r8)  dpdlft  (pcols,kte)
    real(r8)  shdlft  (pcols,kte)
    
#ifdef MODAL_AERO
    integer l, lnum, lnumcw, lmass, lmasscw
#endif
    
    ! Variables for MG microphysics
    
    real(r8)  dum1,dum2
    real(r8)  qc(pcols,kte)
    real(r8)  qi(pcols,kte)
    real(r8)  nc(pcols,kte)
    real(r8)  ni(pcols,kte)
    real(r8)  icinc(pcols,kte)                       ! In cloud ice number conc
    real(r8)  cdnumc(pcols)                          ! Vertically-integrated droplet concentration
    real(r8)  icwnc(pcols,kte)                       ! In cloud water number conc
    real(r8)  iwc(pcols,kte)                         ! Grid box average ice water content
    real(r8)  lwc(pcols,kte)                         ! Grid box average liquid water content  
    real(r8)  effliq(pcols,kte)                      ! In cloud liq eff rad
    real(r8)  effice(pcols,kte)                      ! In cloud ice eff rad
    real(r8)  effliq_fn(pcols,kte)                   ! In cloud liq eff rad at fixed number concentration	
    real(r8)  wsub(pcols,kte)                        ! Sub-grid vertical velocity (m/s)
    real(r8)  wsubi(pcols,kte)                       ! Sub-grid vertical velocity for ice (m/s)
    
    ! Output from mmicro_pcond
    
    real(r8)  tlat(pcols,kte)
    real(r8)  qvlat(pcols,kte)
    real(r8)  qcten(pcols,kte)
    real(r8)  qiten(pcols,kte)
    real(r8)  ncten(pcols,kte)
    real(r8)  niten(pcols,kte)
    real(r8)  effc(pcols,kte)
    real(r8)  effc_fn(pcols,kte)                     ! Liquid effective radius at fixed number (for indirect calc)
    real(r8)  effi(pcols,kte)
    real(r8)  prect(pcols)
    real(r8)  preci(pcols)
    
    ! Output from microp_aero_ts for aerosol actication
    real(r8)  naai(pcols,kte)      !ice nucleation number
    real(r8)  naai_hom(pcols,kte)  !ice nucleation number (homogeneous)
    real(r8)  npccn(pcols,kte)     !liquid activation number tendency
    real(r8)  rndst(pcols,kte,4)
    real(r8)  nacon(pcols,kte,4)
    
    ! Averaging arrays for effective radius and number....
    
    real(r8)  efiout(pcols,kte)
    real(r8)  efcout(pcols,kte)
    real(r8)  ncout(pcols,kte)
    real(r8)  niout(pcols,kte)
    
    real(r8)  freqi(pcols,kte)
    real(r8)  freql(pcols,kte)
    
    ! Average cloud top radius & number
    
    real(r8)  ctrel(pcols)
    real(r8)  ctrei(pcols)
    real(r8)  ctnl(pcols)
    real(r8)  ctni(pcols)
    real(r8)  fcti(pcols)
    real(r8)  fctl(pcols)
    
    ! Gather mass mixing ratio for all aerosols affecting the climate
    
    integer               :: naer_all
    real(r8)     :: aermmr1(pcols,kte)
    real(r8), allocatable :: aer_mmr(:,:,:)           ! Aerosol mass mixing ratio
    
    real(r8)  zeros(pcols,kte)
    
    real(r8)  alst_mic(pcols,kte)
    real(r8)  aist_mic(pcols,kte)
    
    real(r8), pointer :: fldcw(:,:)
    real(r8) :: xland_pt
    real(r8) :: snowh_pt
    ! ======================================================================
    
    
    !cmeliq is an output from Macro physics. Its assigned zero here as Macrophysics is NOT implemented yet
    !*** IMPORTANT-WARNING ***: The following assignment will produce an incorrect value of QME3D
    cmeliq(:,:)=0.0_r8
    
    !*** IMPORTANT-WARNING *** :Should be in the namelist file. Hardwired currently
    conv_water_in_rad = 1
    
#if ( WRF_CHEM == 1 )
    p1st = param_first_scalar ! Obtain CHEM array's first element's index
#endif          
    
    ! Specify diameter for each mode
#if ( WRF_CHEM != 1 )
    dgnumwet(:,:,:)              = 0.1e-6_r8      !*** IMPORTANT-WARNING *** :Constant value for the whole domain (prescribed value). The value match precribe_aerosol_mixactivate, but units change
    dgnumwet(:,:,modeptr_aitken) = 0.02e-6
    dgnumwet(:,:,modeptr_coarse) = 1.0e-6 
#else
    if(chem_opt == 0 ) then
       dgnumwet(:,:,:)              = 0.1e-6_r8      !*** IMPORTANT-WARNING *** :Constant value for the whole domain (prescribed value). The value match precribe_aerosol_mixactivate, but units change
       dgnumwet(:,:,modeptr_aitken) = 0.02e-6
       dgnumwet(:,:,modeptr_coarse) = 1.0e-6 
    endif
#endif
    
    !dgnum wet and dry are assumed to be same for now
    dgnum(:,:,:) = dgnumwet(:,:,:) 
    
    !Time step is stored in the (r8) format in dtime
    dtime = dt

    
    !default values for radius 
    lradius(:,:,:) = 10._r8
    iradius(:,:,:) = 70._r8
  
    !Flag for first time step
    is_first_step  = .false.
    if(itimestep == 1) then
       is_first_step          = .true.
       cldfra_old_mp(:,:,:)   = 0.0_r8
       rate1ord_cw2pr_st(:,:) = 0.0_r8 
       cldfrai(:,:,:)       = 0._r8
       cldfral(:,:,:)       = 0._r8
       cldfra_mp(:,:,:)     = 0._r8
       cldfra_mp_all(:,:,:) = 0._r8
       cldfra_conv(:,:,:)   = 0._r8

       if(config_flags%shcu_physics .NE. CAMUWSHCUSCHEME) call wrf_message('WARNING: sh_icwmr,cmfmc,cmfmc2 and sh_frac are set to zero in CAMMGMP')
       if(config_flags%cu_physics   .NE. CAMZMSCHEME)     call wrf_message('WARNING: dp_icwmr is set to zero in CAMMGMP')
    endif
    
    ncol = pcols    
    !This subroutine requires that ncol == 1
    if(ncol .NE. 1) then
       call wrf_error_fatal('Number of CAM Columns (NCOL) in CAMMGMP scheme must be 1')
    endif
    
    !*** IMPORTANT-WARNING ***:Initializing variables which are **NEVER** used but required for call consistency for CAM specific subroutine calls
    concld(:,:)      =0.0_r8  !Required for computing some diagnostic variables. These diagnostic variables are 
                              !commented out for now as we didn't map 'concld' with the WRF sided variables
    
    state_omega(:,:) = 0.0_r8 !Required for calling mmicro_pcond and microp_aero_ts. Used in microp_aero_ts for 
                              !calling dropmixnuc but **NEVER** used for any output from dropmixnuc
    
    !Divide domain in chuncks and map WRF variables into CAM
    !Loop counters are named iw,jw,kw to represent that they index WRF sided variables
    !The CAM's side variables are kept almost same by just replacing '%' with an '_' [e.g state%t in CAM is state_t in WRF]
    
    itsm1     = its - 1 
    itile_len = ite - itsm1
    do jw     = jts , jte 
       do iw  = its , ite 
          
          xland_pt = xland(iw,jw)
          snowh_pt = snowh(iw,jw)

          lchnk   = (jw - jts) * itile_len + (iw - itsm1)          !1-D index location from a 2-D tile
          phis(1) = ht(iw,jw)  * gravit   
          ktep1   = kte + 1
          
          !Flip vertically quantities computed at the mid points        
          do kw  = kts, kte
             kflip                = ktep1 - kw
             state_pmid(1,kflip)  = p_hyd(iw,kw,jw)                   !Pressure at the mid-points (Pa) [state%pmid in CAM]  
             dp                   = p8w(iw,kw,jw) - p8w(iw,kw+1,jw)   !Change in pressure (Pa) 
             state_pdel(1,kflip)  = dp
             state_rpdel(1,kflip) = 1.0_r8/dp                         !Reciprocal of pressure difference (1/Pa) [state%rpdel in CAM]
             state_zm(1,kflip)    = z_sea_level(iw,kw,jw) - ht(iw,jw) !Height above the ground at the midpoints (m) [state%zm in CAM]
             state_t(1,kflip)     = t_phy(iw,kw,jw)                   !Temprature at the mid points (K) [state%t in CAM]
             state_s(1,kflip)     = cpair *th(iw,kw,jw)*pi_phy(iw,kw,jw) + gravit*state_zm(1,kflip) + phis(1) ! Dry static energy (m2/s2) [state%s in CAM]-Formula tested in vertical_diffusion.F90 of CAM
             
             !Following three formulas are obtained from ported CAM's ZM cumulus scheme
             !Values of 0 cause a crash in entropy
             multFrc              = 1._r8/(1._r8 + qv_curr(iw,kw,jw))
             state_q(1,kflip,1)   = qv_curr(iw,kw,jw)*multFrc      !Specific humidity                       [state%q(:,:,1) in CAM]
             state_q(1,kflip,ixcldliq) = qc_curr(iw,kw,jw)*multFrc !Convert to moist mix ratio-cloud liquid [state%q(:,:,2) in CAM]
             state_q(1,kflip,ixcldice) = qi_curr(iw,kw,jw)*multFrc !cloud ice                               [state%q(:,:,3) in CAM]
             state_q(1,kflip,ixnumliq) = nc3d(iw,kw,jw)*multFrc
             state_q(1,kflip,ixnumice) = ni3d(iw,kw,jw)*multFrc

             qi_mac(1,kflip)           = qi_curr(iw,kw,jw)*multFrc

             !Check for negative constituents, value below 'qthresh_...' are reset to qmin 
             !For mass mixing ratio
             if(abs(state_q(1,kflip,1))        < qthresh_mass .and. state_q(1,kflip,1)        < qmin(1))        state_q(1,kflip,1)        = qmin(1)
             if(abs(state_q(1,kflip,ixcldliq)) < qthresh_mass .and. state_q(1,kflip,ixcldliq) < qmin(ixcldliq)) state_q(1,kflip,ixcldliq) = qmin(ixcldliq)
             if(abs(state_q(1,kflip,ixcldice)) < qthresh_mass .and. state_q(1,kflip,ixcldice) < qmin(ixcldice)) state_q(1,kflip,ixcldice) = qmin(ixcldice)

             !For number mixing ratio
             if(abs(state_q(1,kflip,ixnumliq)) < qthresh_numl .and. state_q(1,kflip,ixnumliq) < qmin(ixnumliq)) state_q(1,kflip,ixnumliq) = qmin(ixnumliq)
             if(abs(state_q(1,kflip,ixnumice)) < qthresh_numi .and. state_q(1,kflip,ixnumice) < qmin(ixnumice)) state_q(1,kflip,ixnumice) = qmin(ixnumice)


             !Set negative values to qmin and generate a warning message
             if(state_q(1,kflip,1) < 0.0_r8)then
                !write(wrfmessage,*)'Water vapor mass mixing ratio was:', state_q(1,kflip,1),'; Reset to',qmin(1), 'in CAMMGMP driver'
                !call wrf_message(wrfmessage)                                
                state_q(1,kflip,1)        = qmin(1)
             endif
             
             if(state_q(1,kflip,ixcldliq) < 0.0_r8) then
                !write(wrfmessage,*)'Liquid cloud mass mixing ratio was:', state_q(1,kflip,ixcldliq),'; Reset to',qmin(ixcldliq), 'in CAMMGMP driver'
                !call wrf_message(wrfmessage) 
                state_q(1,kflip,ixcldliq) = qmin(ixcldliq)
             endif

             if(state_q(1,kflip,ixcldice) < 0.0_r8) then
                !write(wrfmessage,*)'Ice cloud mass mixing ratio was:', state_q(1,kflip,ixcldice),'; Reset to',qmin(ixcldice), 'in CAMMGMP driver'
                !call wrf_message(wrfmessage) 
                state_q(1,kflip,ixcldice) = qmin(ixcldice)
             endif

             if(state_q(1,kflip,ixnumliq) < 0.0_r8) then
                !write(wrfmessage,*)'Liquid droplet number was:', state_q(1,kflip,ixnumliq),'; Reset to',qmin(ixnumliq), 'in CAMMGMP driver'
                !call wrf_message(wrfmessage) 
                state_q(1,kflip,ixnumliq) = qmin(ixnumliq)
             endif
             
             if(state_q(1,kflip,ixnumice) < 0.0_r8) then
                !write(wrfmessage,*)'Ice crystal number was:', state_q(1,kflip,ixnumice),'; Reset to',qmin(ixnumice), 'in CAMMGMP driver'
                !call wrf_message(wrfmessage) 
                state_q(1,kflip,ixnumice) = qmin(ixnumice)
             endif
             ! For prescribed aerosols
             qqcw(1,kflip,:)                  = 1.e-38_r8      !used in microp_aero for dropmicnuc, presently set to a constant value
#if ( WRF_CHEM == 1 )
             if(chem_opt.NE.0 .and. config_flags%CAM_MP_MAM_cpled ) then
                !Following Do-Loop is obtained from chem/module_cam_mam_aerchem_driver.F 
                do l = p1st, num_chem
                   l2 = lptr_chem_to_q(l)
                   if ((l2 >= 1) .and. (l2 <= pcnst)) then
                      state_q(1,kflip,l2) = chem(iw,kw,jw,l)*factconv_chem_to_q(l)
                      if(state_q(1,kflip,l2) < 0.0_r8) then
                         !write(wrfmessage,*)'Chem species with index', l,' was:', state_q(1,kflip,l2),'; Reset to 0.0 in CAMMGMP driver'
                         !call wrf_message(wrfmessage) 
                         state_q(1,kflip,l2) = 0.0_r8
                      endif
                   end if
                   l2 = lptr_chem_to_qqcw(l)
                   if ((l2 >= 1) .and. (l2 <= pcnst)) then
                      qqcw(1,kflip,l2) = chem(iw,kw,jw,l)*factconv_chem_to_q(l)
                       if(qqcw(1,kflip,l2)< 0.0_r8) then
                          !write(wrfmessage,*)'Chem species with index', l,' was:', qqcw(1,kflip,l2),'; Reset to 0.0 in CAMMGMP driver'
                          !call wrf_message(wrfmessage) 
                         qqcw(1,kflip,l2) = 0.0_r8
                      endif
                   end if
                end do ! l
                !Populate dgnums appropriately
                do imode = 1 , ntot_amode
                   if(is_first_step) then
                      dgnum(1,kflip,imode)    = 0.0_r8 !Set it to zero for the first time step
                      dgnumwet(1,kflip,imode) = 0.0_r8
                   else
                      dgnum(1,kflip,imode)    = dgnum4D(iw,kw,jw,imode)   !Obtained from 4D arrays 
                      dgnumwet(1,kflip,imode) = dgnumwet4D(iw,kw,jw,imode) 
                   endif
                enddo
             endif
#endif

             cldo(1,kflip)        = cldfra_mp_all(iw,kw,jw)
             
             !New code by PMA
             rh_old(1,kflip)      = rh_old_mp(iw,kw,jw)
             dlf(1,kflip)         = 0.0_r8
             dlf2(1,kflip)        = 0.0_r8

             sh_icwmr(1,kflip)    = 0.0_r8
             sh_frac(1,kflip)     = 0.0_r8
             dp_frac(1,kflip)     = 0._r8
             if(config_flags%shcu_physics==CAMUWSHCUSCHEME) then
                sh_icwmr(1,kflip)  = icwmrsh3d(iw,kw,jw)      !Shallow cumulus in-cloud water mixing ratio (kg/m2)
                sh_frac(1,kflip)   = shfrc3d(iw,kw,jw)        !Shallow convective cloud fraction
                dlf2(1,kflip)      = dlf2_3d(iw,kw,jw)        !PMA QC detrained from shallow
             endif
             dp_icwmr(1,kflip)     = 0.0_r8
             if(config_flags%cu_physics==CAMZMSCHEME)then
                dp_icwmr(1,kflip)  = icwmrdp3d(iw,kw,jw)      !Deep conv. cloud water
                dlf(1,kflip)       = dlf3d(iw,kw,jw)          !PMA Qc detrained from deep+shallow
             endif
             
          enddo
          
          do kw = kts, kte+1
             kflip = kte - kw + 2
             
             state_pint(1,kflip) = p8w(iw,kw,jw)        !Pressure at interfaces [state%pint in CAM]
             tke(1,kflip)        = tke_pbl(iw,kw,jw)    !Turbulent kinetic energy 
             kkvh(1,kflip)       = kvh3d(iw,kw,jw)      !Vertical diffusivity (m2/s)
             turbtype(1,kflip)   = turbtype3d(iw,kw,jw) !Turbulent interface types [ no unit ]
             smaw(1,kflip)       = smaw3d(iw,kw,jw)     !Normalized Galperin instability function for momentum  ( 0<= <=4.964 and 1 at neutral ) [no units]

             cmfmc(1,kflip)      = 0.0_r8
             cmfmc2(1,kflip)     = 0.0_r8
             if(config_flags%shcu_physics==CAMUWSHCUSCHEME  .or. config_flags%cu_physics   == CAMZMSCHEME ) then 
                cmfmc(1,kflip)   = cmfmc3d(iw,kw,jw)    !Deep + Shallow Convective mass flux [ kg /s/m^2 ]
             endif
             
             if(config_flags%shcu_physics==CAMUWSHCUSCHEME ) then
                cmfmc2(1,kflip)  = cmfmc2_3d(iw,kw,jw)  !Shallow convective mass flux [ kg/s/m^2 ]
             endif
             
          end do
          do kcam = 1, kte
             !Formulation for dp_frac is obtained from cloud_fraction.F90 of CAM
             dp_frac(1,kcam)         = max(0.0_r8,min(dp1*log(1.0_r8+dp2*(cmfmc(1,kcam+1)-cmfmc2(1,kcam+1))),0.60_r8)) ! Deep convective cloud fraction
          end do


          !set cflx zero for all the constituents, update the water vapor surface flux after that
          cflx(:pcols,:) = 0.0_r8        !Surface constituent flux (kg/m2/s)
          cflx(:pcols,1) = qfx(iw,jw)    !Surface constituent flux (kg/m2/s)
          rliq(:pcols)   = rliq2d(iw,jw) !Vertically-integrated reserved cloud condensate (m/s)
          

          !Following assignments mimics CAM's 'physics_state_copy' call
          state1_pmid(:,:)  = state_pmid(:,:)
          state1_pdel(:,:)  = state_pdel(:,:)
          state1_rpdel(:,:) = state_rpdel(:,:)
          state1_zm(:,:)    = state_zm(:,:)
          state1_t(:,:)     = state_t(:,:)
          state1_s(:,:)     = state_s(:,:)
          state1_pint(:,:)  = state_pint(:,:)
          state1_q(:,:,:)   = state_q(:,:,:)
          state1_omega(:,:) = state_omega(:,:) !*** IMPORTANT-WARNING ***:State_omega is *NOT* used for any meaningful computation at this time
          
          call physics_ptend_init(ptend_loc_name,ptend_loc_q,ptend_loc_s,ptend_loc_lq,ptend_loc_ls,pcnst)  !! Initialize local ptend type 
          call physics_ptend_init(ptend_all_name,ptend_all_q,ptend_all_s,ptend_all_lq,ptend_all_ls,pcnst)  !! Initialize output ptend type
          
#ifdef MODAL_AERO
          if( is_first_step) then
             do i=1,pcnst
                fldcw => qqcw(:,:,i)
                if(associated(fldcw)) then
                   fldcw(1:pcols,1:pver)          = 1.e-38_r8
                end if
             end do
          endif
#endif
          
          !! If first timestep, initialize heatflux....in pbuf at all time levels.
          
          if( is_first_step) then
             kkvh(:,:)     = 0._r8
             tke(:,:)      = 0._r8
             ! *** IMPORTANT-WARNING ***:turbtype and smaw are used to compute 'smm' in microp_aero_ts subroutine but 'smm' is NEVER
             ! again used for any output from microp_aero_ts. Therefore, these variables are NOT used in any
             ! meaningful computations
             turbtype(:,:) = 0._r8
             smaw(:,:)     = 0._r8
             dlf(:,:)      = 0._r8
             dlf2(:,:)     = 0._r8
          endif
          
          !! Assign default size distribution parameters for no-stratiform clouds (convection only)
          !! Also put into physics buffer for possible separate use by radiation
          
          dcon   = 25.e-6_r8
          mucon  = 5.3_r8
          deicon = 50._r8
          
          muconv(:,:)     = mucon
          lambdaconv(:,:) = (mucon + 1._r8)/dcon
          deiconv(:,:)    = deicon
          
          !BSINGH - CALL Macrophysics
          call Macrophysics_simple(is_first_step, pcnst, pcols, kte, ixcldliq, ixnumliq,         &
               lchnk, dtime, state1_t, state_pmid, state_q, dp_icwmr, sh_icwmr, dp_frac,      &
               sh_frac, ast,aist,alst,qi_mac,xland_pt,snowh_pt,                               &
               !intent-inout
               state1_s, state1_q, rh_old, cldo, esl, qvs, ptend_loc_name, ptend_loc_ls,      &
               ptend_loc_lq, ptend_loc_s,ptend_loc_q, ptend_all_name, ptend_all_ls,           &
               ptend_all_lq,ptend_all_s,ptend_all_q                                           )          
          
          do kw = kts , kte
             kflip                   = kte-kw+1

             cldfral(iw,kw,jw)       = alst(1,kflip)
             cldfrai(iw,kw,jw)       = aist(1,kflip)
             CLDFRA_MP(iw,kw,jw)     = ast(1,kflip)  
             cld(1,kflip)            = ast(1,kflip)
             CLDFRA_MP_all(iw,kw,jw) = min(ast(1,kflip)+min(0.8_r8,dp_frac(1,kflip)+sh_frac(1,kflip)),1._r8)
             CLDFRA_CONV(iw,kw,jw)   = max(min(0.8_r8,dp_frac(1,kflip)+sh_frac(1,kflip)),0._r8)
             concld(1,kflip)         = CLDFRA_CONV(iw,kw,jw) 
             cldo(1,kflip)           = cldfra_old_mp(iw,kw,jw)
          end do

          !! ------------------------------------- !
          !! From here, process computation begins ! 
          !! ------------------------------------- !

          
          !! ----------------------- !
          !! Microp_Driver Microphysics !
          !! ----------------------- !

          ptend_loc_name         = 'microp'
          ptend_loc_ls           = .true.
          ptend_loc_lq(1)        = .true.
          ptend_loc_lq(ixcldliq) = .true.
          ptend_loc_lq(ixcldice) = .true.
          ptend_loc_lq(ixnumliq) = .true.
          ptend_loc_lq(ixnumice) = .true.
          
#ifndef MODAL_AERO
          call rad_cnst_get_info( 0, naero = naer_all )
          allocate( aer_mmr( pcols, pver, naer_all ) )
          do m = 1, naer_all
             call rad_cnst_get_aer_mmr( 0, m, state1, pbuf, aermmr1 )
             aer_mmr(:ncol,:,m) = aermmr1(:ncol,:)
          enddo
#endif
          
#ifdef MODAL_AERO
#if ( WRF_CHEM == 1 )
          do m = 1, ntot_amode
             lnum = numptr_amode(m)
             if( lnum > 0 ) then
                ptend_loc_lq(lnum)= .true.
             endif
             do l = 1, nspec_amode(m)
                lmass = lmassptr_amode(l,m)
                ptend_loc_lq(lmass)= .true.
             enddo
          enddo
          !Do not update dummy aerosols in state if chem_opt is 0 or CAM_MP_MAM_cpled == .false.
          if(chem_opt == 0 .or. .NOT.config_flags%CAM_MP_MAM_cpled) then
             do m = 6 , pcnst   !First m = 1 to 5 are water species
                ptend_loc_lq(m)= .false.
             enddo
          endif
#endif
#endif
          
          zeros(:ncol,:pver)  = 0._r8
          qc(:ncol,:pver) = state1_q(:ncol,:pver,ixcldliq)
          qi(:ncol,:pver) = state1_q(:ncol,:pver,ixcldice)
          nc(:ncol,:pver) = state1_q(:ncol,:pver,ixnumliq)
          ni(:ncol,:pver) = state1_q(:ncol,:pver,ixnumice)
          
          if( micro_treatment .eq. 'inter' ) then
             alst_mic(:ncol,:pver) = ast(:ncol,:pver)
             aist_mic(:ncol,:pver) = ast(:ncol,:pver)
          elseif( micro_treatment .eq. 'compl' ) then
             alst_mic(:ncol,:pver) = alst(:ncol,:pver)
             aist_mic(:ncol,:pver) = aist(:ncol,:pver)
          endif
          
#if ( WRF_CHEM != 1 )          
          !Adding 7 new fields to state1_q(# mixing ratio and mass mixing ratio
          !for 3 modes of prescribed aerosols[s04,dust(+sea salt),aitken]) array. 
          !state%q holds 5 constituents already therefore the prescribed aerosol 
          !are defined from 6th to 12th constituents
          
          n = modeptr_accum    !Adding 7th constituent (# mixing ratio) 
          !tmpnaer = 1000.0e6  !This value is taken from precribe_aerosol_mixactivate! 1.e9 #/kg ~= 1.e3 #/cm3
          tmpnaer = accum_mode !172.7e6 -For ISDAC case ONLY
          l = numptr_amode(n)  
          state1_q(:,:,l) = tmpnaer
          
          m = 1                !Adding 6th constituent (mass mixing ratio) 
          tmpmaer = specdens_amode(lspectype_amode(m,n)) * tmpnaer / voltonumb_amode(n)
          l = lmassptr_amode(m,n)
          state1_q(:,:,l) = tmpmaer
          
          n = modeptr_aitken   !Adding 9th constituent (# mixing ratio) 
          !tmpnaer = 300.0e6
          tmpnaer = aitken_mode !0.0e6 -For ISDAC case ONLY
          l = numptr_amode(n)
          state1_q(:,:,l) = tmpnaer
          
          m = 1                !Adding 8th constituent (mass mixing ratio)
          tmpmaer = specdens_amode(lspectype_amode(m,n)) * tmpnaer / voltonumb_amode(n)
          l = lmassptr_amode(m,n)
          state1_q(:,:,l) = tmpmaer
          
          n = modeptr_coarse   !Adding 12th constituent (# mixing ratio) 
          !tmpnaer = 0.2e6
          tmpnaer = coarse_mode !1.1e6 -For ISDAC case ONLY
          l = numptr_amode(n)
          state1_q(:,:,l) = tmpnaer
          
          tmpmaer = specdens_amode(lspectype_amode(m,n)) * tmpnaer / voltonumb_amode(n)
          m = 1                !Adding 10th constituent (mass mixing ratio)
          l = lmassptr_amode(m,n)
          state1_q(:,:,l) = tmpmaer*0.5
          
          m = 2                !Adding 11th constituent (mass mixing ratio)
          l = lmassptr_amode(m,n)
          state1_q(:,:,l) = tmpmaer*0.5
#else
          if(chem_opt==0 .or. .NOT. config_flags%CAM_MP_MAM_cpled) then
          do k = 1, pver
             n = modeptr_accum    !Adding 7th constituent (# mixing ratio) 
             tmpnaer = accum_mode !172.7e6 -For ISDAC case ONLY
             l = numptr_amode(n)  
             qqcw(1,k,l) = tmpnaer*alst(1,k)*0.8
             state1_q(1,k,l) = tmpnaer-qqcw(1,k,l)
             
             m = 1                !Adding 6th constituent (mass mixing ratio) 
             tmpmaer = specdens_amode(lspectype_amode(m,n)) * tmpnaer / voltonumb_amode(n)
             l = lmassptr_amode(m,n)
             qqcw(1,k,l) = tmpmaer*alst(1,k)*0.8
             state1_q(1,k,l) = tmpmaer-qqcw(1,k,l)
             
             n = modeptr_aitken   !Adding 9th constituent (# mixing ratio) 
             tmpnaer = aitken_mode !0.0e6 -For ISDAC case ONLY
             l = numptr_amode(n)
             qqcw(1,k,l) = tmpnaer*alst(1,k)*0.8
             state1_q(1,k,l) = tmpnaer-qqcw(1,k,l)
             
             m = 1                !Adding 8th constituent (mass mixing ratio)
             tmpmaer = specdens_amode(lspectype_amode(m,n)) * tmpnaer / voltonumb_amode(n)
             l = lmassptr_amode(m,n)
             qqcw(1,k,l) = tmpmaer*alst(1,k)*0.8
             state1_q(1,k,l) = tmpmaer-qqcw(1,k,l)
             n = modeptr_coarse   !Adding 12th constituent (# mixing ratio) 
             tmpnaer = coarse_mode !1.1e6 -For ISDAC case ONLY
             l = numptr_amode(n)
             qqcw(1,k,l) = tmpnaer*alst(1,k)*0.8
             state1_q(1,k,l) = tmpnaer-qqcw(1,k,l)
             
             tmpmaer = specdens_amode(lspectype_amode(m,n)) * tmpnaer / voltonumb_amode(n)
             m = 1                !Adding 10th constituent (mass mixing ratio)
             l = lmassptr_amode(m,n)
             qqcw(1,k,l) = tmpmaer*alst(1,k)*0.8*0.5
             state1_q(1,k,l) = tmpmaer*0.5-qqcw(1,k,l)
             
             m = 2                !Adding 11th constituent (mass mixing ratio)
             l = lmassptr_amode(m,n)
             qqcw(1,k,l) = tmpmaer*alst(1,k)*0.8*0.5
             state1_q(1,k,l) = tmpmaer*0.5-qqcw(1,k,l)
            enddo
          endif
          
#endif           
          !! calculate aerosol activation (naai for ice, npccn for liquid) and 
          !! dust size (rndst) and number (nacon) for contact nucleation
          
          ! Output from the following call:wsub, wsubi, naai, naai_hom, npccn, rndst,nacon 
          ! In-out from the following call: ptend_loc_q, qqcw
          !*NOTE*: Added QQCW as an output
          call microp_aero_ts ( lchnk, ncol, dtime, state1_t, zeros,      & 
               state1_q(1,1,1), qc, qi,                                   &
               nc, ni, state1_pmid, state1_pdel, ast,                     &
               alst_mic, aist_mic,                                        &
               cldo, state1_pint, state1_rpdel, state1_zm, state1_omega,  &
#ifdef MODAL_AERO
               state1_q, cflx, ptend_loc_q, dgnumwet, dgnum,              &
#else
               aer_mmr,                                                   &
#endif
               kkvh, tke, turbtype, smaw, wsub, wsubi,                    &
               naai,naai_hom, npccn, rndst,nacon,qqcw)
          
          !! Call MG Microphysics
          
          
          !Output from the following call:rate1cld,tlat,qvlat,qcten,qiten,
          !     ncten,niten,effc,effc_fn,effi,prect,preci,nevapr,evapsnow,
          !     prain,prodsnow,cmeice,dei,mu,lambdac,qsout,des,rflx,sflx,
          !     qrout,reff_rain,reff_snow,qcsevap,qisevap,qvres,cmeiout,
          !     vtrmc,vtrmi,qcsedten,qisedten,prao,prco,mnuccco,mnuccto,
          !     msacwio,psacwso,bergso,bergo,melto,homoo,qcreso,prcio,
          !     praio,qireso,mnuccro,pracso,meltsdt,frzrdt,mnuccdo	
          
          !in-out from the following call: qc,qi,nc,ni,cldo
          !*NOTE*: Added nsout and nrout as an ouput
          call mmicro_pcond( sub_column, lchnk, ncol, dtime, state1_t,    &
               state1_q(1,1,1), qc, qi,                                   &
               nc, ni, state1_pmid, state1_pdel, ast,                     &
               alst_mic, aist_mic,                                        &
               cldo,                                                      &
               rate1cld,                                                  & 
               naai, npccn, rndst,nacon,                                  &
               tlat, qvlat,                                               &
               qcten, qiten, ncten, niten, effc,                          &
               effc_fn, effi, prect, preci,                               & 
               nevapr, evapsnow,                                          &
               prain, prodsnow, cmeice, dei, mu,                          &
               lambdac, qsout, des,                                       &
               rflx,sflx, qrout,reff_rain,reff_snow,                      &
               qcsevap, qisevap, qvres, cmeiout,                          &
               vtrmc, vtrmi, qcsedten, qisedten,                          &
               prao, prco, mnuccco, mnuccto, msacwio, psacwso,            &
               bergso, bergo, melto, homoo, qcreso, prcio, praio, qireso, &
               mnuccro, pracso, meltsdt, frzrdt , mnuccdo, nsout, nrout )
          
          do k=1,pver
             do i=1,ncol
                if (naai(i,k) .gt. 0._r8) then
                   mnuccdohet(i,k) = mnuccdo(i,k) - (naai_hom(i,k)/naai(i,k))*mnuccdo(i,k)
                else
                   mnuccdohet(i,k) = 0._r8
                end if
             end do
          end do
          
          mgflxprc(:ncol,:pverp) = rflx(:ncol,:pverp) + sflx(:ncol,:pverp)
          mgflxsnw(:ncol,:pverp) = sflx(:ncol,:pverp)
          
          mgmrprc(:ncol,:pver) = qrout(:ncol,:pver) + qsout(:ncol,:pver)
          mgmrsnw(:ncol,:pver) = qsout(:ncol,:pver)
          
          
          mgreffrain(:ncol,:pver) = reff_rain(:ncol,:pver)
          mgreffsnow(:ncol,:pver) = reff_snow(:ncol,:pver)
          
          !! calculate effective radius of convective liquid and ice using dcon and deicon (not used by code, not useful for COSP)
          cvreffliq(:ncol,:pver) = 9.0_r8  !! hard-coded as average of hard-coded values used for deep/shallow convective detrainment (near line 1502)
          cvreffice(:ncol,:pver) = 37.0_r8 !! hard-coded as average of hard-coded values used for deep/shallow convective detrainment (near line 1505)
          
          !! Reassign rate1 if modal aerosols
#ifdef MODAL_AERO
          rate1ord_cw2pr_st(1:ncol,1:pver)=rate1cld(1:ncol,1:pver)
#endif
          !! Sedimentation velocity for liquid stratus cloud droplet
          
          wsedl(:ncol,:pver) = vtrmc(:ncol,:pver)
          
          !! Nominal values for no microp_driver (convective only) cloud.
          !! Convert snow mixing ratio to microns
          
          do k = 1, pver
             do i = 1, ncol 
                des(i,k) = des(i,k) * 1.e6_r8
                if( ast(i,k) .lt. 1.e-4_r8 ) then
                   mu(i,k) = mucon
                   lambdac(i,k) = (mucon + 1._r8)/dcon
                   dei(i,k) = deicon
                endif
             end do
          end do
          
          !! Net microp_driver condensation rate
          !*** IMPORTANT-WARNING ***: Following computation of QME will yeild WRONG answer as CMELIQ is set to zero
          qme(:ncol,:pver) = cmeliq(:ncol,:pver) + cmeiout(:ncol,:pver) 
          
          
#ifndef MODAL_AERO
          deallocate(aer_mmr) 
#endif
          
          do k = 1, pver
             do i = 1, ncol
                ptend_loc_s(i,k)          =  tlat(i,k)
                ptend_loc_q(i,k,1)        = qvlat(i,k)
                ptend_loc_q(i,k,ixcldliq) = qcten(i,k)
                ptend_loc_q(i,k,ixcldice) = qiten(i,k)
                ptend_loc_q(i,k,ixnumliq) = ncten(i,k)
                ptend_loc_q(i,k,ixnumice) = niten(i,k)
             enddo
          enddo
          
          !! For precip, accumulate only total precip in prec_pwc and snow_pwc variables.
          !! Other precip output varirables are set to 0
          prec_pcw(:ncol) = prect(:ncol)
          snow_pcw(:ncol) = preci(:ncol)
          prec_sed(:ncol) = 0._r8
          snow_sed(:ncol) = 0._r8
          prec_str(:ncol) = prec_pcw(:ncol) + prec_sed(:ncol) - rliq(:ncol)
          snow_str(:ncol) = snow_pcw(:ncol) + snow_sed(:ncol) 
          
          !! ------------------------------- !
          !! Update microphysical tendencies !
          !! ------------------------------- !
          
          call physics_ptend_sum(ptend_loc_ls,ptend_loc_lq,ptend_loc_s,ptend_loc_q,ptend_all_ls,ptend_all_lq,ptend_all_s,ptend_all_q,pcnst)
          ptend_all_name = 'cldwat'
          call physics_update( lchnk,dtime,state1_q,ptend_loc_q,state1_s,ptend_loc_s,ptend_loc_name,ptend_loc_lq,ptend_loc_ls,pcnst)
          call physics_ptend_init( ptend_loc_name,ptend_loc_q,ptend_loc_s, ptend_loc_lq,ptend_loc_ls,pcnst)
          
          if( micro_treatment .eq. 'inter' ) then
             icecldf(:ncol,:pver) = ast(:ncol,:pver)
             liqcldf(:ncol,:pver) = ast(:ncol,:pver)
          elseif( micro_treatment .eq. 'compl' ) then
             icecldf(:ncol,:pver) = aist(:ncol,:pver)
             liqcldf(:ncol,:pver) = alst(:ncol,:pver)
          endif
          
          !! Effective droplet radius
          rel(:ncol,:pver)        = effc(:ncol,:pver)
          rel_fn(:ncol,:pver)     = effc_fn(:ncol,:pver)	
          rei(:ncol,:pver)        = effi(:ncol,:pver)
          rel2(:ncol,:pver)       = rel(:ncol,:pver) * 0.9071_r8 !! Convect to effective volume radius assuming pgam = 8
          rei2(:ncol,:pver)       = rei(:ncol,:pver) * 0.6057_r8 !! Convect to effective volume radius at pgam = 0 for ice 
          !! ----------------------------------------------------------- ! 
          !! Adjust in-cloud water values to take account of convective  !
          !! in-cloud water. It is used to calculate the values of       !
          !! icwlp and iciwp to pass to the radiation.                   ! 
          !! ----------------------------------------------------------- !
          allcld_ice(:ncol,:pver) = 0._r8 !! Grid-avg all cloud liquid
          allcld_liq(:ncol,:pver) = 0._r8 !! Grid-avg all cloud ice
          if( conv_water_in_rad /= 0 ) then
             !Following formulation is obtained from macrop_driver.F90 of CAM
             call cldwat_fice( ncol, state1_t, fice, fsnow )

             !Balwinder.Singh@pnnl.gov: Changed the argument list to avoid pbuf
             call conv_water_4rad( lchnk, ncol, ast, sh_icwmr, dp_icwmr,       &
                  fice, sh_frac, dp_frac, conv_water_in_rad, rei, state1_pdel, &
                  state1_q(:,:,ixcldliq), state1_q(:,:,ixcldice),              &
                  allcld_liq, allcld_ice )
          else
             allcld_liq(:ncol,:) = state1_q(:ncol,:,ixcldliq)  ! Grid-ave all cloud liquid
             allcld_ice(:ncol,:) = state1_q(:ncol,:,ixcldice)  !           "        ice 
          end if
          !! ------------------------------------------------------------ !
          !! Compute in cloud ice and liquid mixing ratios                !
          !! Note that 'iclwp, iciwp' are used for radiation computation. !
          !! ------------------------------------------------------------ !
          do k = 1, pver
             do i = 1, ncol
                !! Limits for in-cloud mixing ratios consistent with MG microphysics
                !! in-cloud mixing ratio 0.0001 to 0.005 kg/kg
                icimr(i,k)     = min( allcld_ice(i,k) / max(0.0001_r8,cld(i,k)),0.005_r8 )
                icwmr(i,k)     = min( allcld_liq(i,k) / max(0.0001_r8,cld(i,k)),0.005_r8 )
                icimrst(i,k)   = min( state1_q(i,k,ixcldice) / max(0.0001_r8,icecldf(i,k)),0.005_r8 )
                icwmrst(i,k)   = min( state1_q(i,k,ixcldliq) / max(0.0001_r8,liqcldf(i,k)),0.005_r8 )
                icinc(i,k)     = state1_q(i,k,ixnumice) / max(0.0001_r8,icecldf(i,k)) * state1_pmid(i,k) / (287.15_r8*state1_t(i,k))
                icwnc(i,k)     = state1_q(i,k,ixnumliq) / max(0.0001_r8,liqcldf(i,k)) * state1_pmid(i,k) / (287.15_r8*state1_t(i,k))
                iwc(i,k)       = allcld_ice(i,k) * state1_pmid(i,k) / (287.15_r8*state1_t(i,k))
                lwc(i,k)       = allcld_liq(i,k) * state1_pmid(i,k) / (287.15_r8*state1_t(i,k))
                effliq(i,k)    = effc(i,k)
                effliq_fn(i,k) = effc_fn(i,k)
                effice(i,k)    = effi(i,k)
                !! Calculate total cloud water paths in each layer
                iciwp(i,k)     = icimr(i,k) * state1_pdel(i,k) / gravit
                iclwp(i,k)     = icwmr(i,k) * state1_pdel(i,k) / gravit
                !! Calculate microp_driver cloud water paths in each layer
                !! Note: uses stratiform cloud fraction!
                iciwpst(i,k)   = min(state1_q(i,k,ixcldice)/max(0.0001_r8,ast(i,k)),0.005_r8) * state1_pdel(i,k) / gravit
                iclwpst(i,k)   = min(state1_q(i,k,ixcldliq)/max(0.0001_r8,ast(i,k)),0.005_r8) * state1_pdel(i,k) / gravit
                !! Calculate convective in-cloud LWP.
                !Commented the following two diagnostics as 'concld' is not mapped correctly with variables on WRF side
                !iclwpconv(i,k) = max(allcld_liq(i,k) - state_q(i,k,ixcldliq),0._r8)/max(0.0001_r8,concld(i,k)) 
                !iciwpconv(i,k) = max(allcld_ice(i,k) - state_q(i,k,ixcldice),0._r8)/max(0.0001_r8,concld(i,k)) 
                !! ------------------------------ !
                !! Adjust cloud fraction for snow !
                !! ------------------------------ !
                cldfsnow(i,k) = cld(i,k)
                !! If cloud and only ice ( no convective cloud or ice ), then set to 0.
                if( ( cldfsnow(i,k) .gt. 1.e-4_r8 ) .and. & 
                     ( concld(i,k)   .lt. 1.e-4_r8 ) .and. & 
                     ( state1_q(i,k,ixcldliq) .lt. 1.e-10_r8 ) ) then
                   !Commented the following diagnostics as 'concld' is not mapped correctly with variables on WRF side
                   !cldfsnow(i,k) = 0._r8
                endif
                !! If no cloud and snow, then set to 0.25
                if( ( cldfsnow(i,k) .lt. 1.e-4_r8 ) .and. ( qsout(i,k) .gt. 1.e-6_r8 ) ) then 
                   !Commented the following diagnostics as 'concld' is not mapped correctly with variables on WRF side
                   !cldfsnow(i,k) = 0.25_r8
                endif
                !! Calculate in-cloud snow water path
                !Commented the following diagnostics as 'concld' is not mapped correctly with variables on WRF side
                !icswp(i,k) = qsout(i,k) / max( 0.0001_r8, cldfsnow(i,k) ) * state1_pdel(i,k) / gravit
             enddo
          enddo
          
          !! --------------------- !
          !! History Output Fields !
          !! --------------------- !
          
          !! Column droplet concentration
          
          do i = 1, ncol
             cdnumc(i) = 0._r8
             do k = 1, pver
                cdnumc(i) = cdnumc(i) + state1_q(i,k,ixnumliq)*state1_pdel(i,k)/gravit
             end do
          end do
          
          !! Averaging for new output fields
          
          efcout(:,:)      = 10._r8 
          efiout(:,:)      = 25._r8 
          ncout(:,:)       = 0._r8
          niout(:,:)       = 0._r8	
          freql(:,:)       = 0._r8
          freqi(:,:)       = 0._r8
          liqcldf_out(:,:) = 0._r8
          icecldf_out(:,:) = 0._r8
          icwmrst_out(:,:) = 0._r8
          icimrst_out(:,:) = 0._r8
          do k = 1, pver
             do i = 1, ncol
                if( liqcldf(i,k) .gt. 0.01_r8 .and. icwmrst(i,k) .gt. 5.e-5_r8 ) then
                   efcout(i,k) = effc(i,k)
                   ncout(i,k)  = icwnc(i,k)
                   freql(i,k)  = 1._r8
                   liqcldf_out(i,k) = liqcldf(i,k)
                   icwmrst_out(i,k) = icwmrst(i,k)
                endif
                if( icecldf(i,k) .gt. 0.01_r8 .and. icimrst(i,k) .gt. 1.e-6_r8 ) then
                   efiout(i,k) = effi(i,k)
                   niout(i,k)  = icinc(i,k)
                   freqi(i,k)  = 1._r8
                   icecldf_out(i,k) = icecldf(i,k)
                   icimrst_out(i,k) = icimrst(i,k)
                endif
             end do
          end do
          
          !! Cloud top effective radius and number.
          
          fcti(:)  = 0._r8
          fctl(:)  = 0._r8
          ctrel(:) = 0._r8
          ctrei(:) = 0._r8
          ctnl(:)  = 0._r8
          ctni(:)  = 0._r8
          do i = 1, ncol
             do k = 1, pver
                if( liqcldf(i,k) .gt. 0.01_r8 .and. icwmrst(i,k) .gt. 1.e-7_r8 ) then
                   ctrel(i) = effc(i,k)
                   ctnl(i)  = icwnc(i,k)
                   fctl(i)  = 1._r8
                   exit
                endif
                if( icecldf(i,k) .gt. 0.01_r8 .and. icimrst(i,k) .gt. 1.e-7_r8 ) then
                   ctrei(i) = effi(i,k)
                   ctni(i)  = icinc(i,k)
                   fcti(i)  = 1._r8
                   exit
                endif
             enddo
          enddo
          
          !Following update finally updates the state variables
          call physics_update( lchnk,dtime,state_q,ptend_all_q,state_s,ptend_all_s,ptend_all_name,ptend_all_lq,ptend_all_ls,pcnst)
          
          !Post processing of the output from CAMMGMP                    
          do kw=kts,kte
             
             kflip = kte-kw+1
             !Following equation are derived following UWPBL and CAMZM schemes
             qv_curr(iw,kw,jw)       = state_q(1,kflip,1) / (1.0_r8 - state_q(1,kflip,1)) 
             multFrc                 = 1._r8 + qv_curr(iw,kw,jw)
             
             qc_curr(iw,kw,jw)       = state_q(1,kflip,2) * multFrc
             qi_curr(iw,kw,jw)       = state_q(1,kflip,3) * multFrc 
             qs_curr(iw,kw,jw)       = qsout(1,kflip)     * multFrc 
             qr_curr(iw,kw,jw)       = qrout(1,kflip)     * multFrc 
             
             nc3d(iw,kw,jw)          = state_q(1,kflip,4) * multFrc 
             qndrop(iw,kw,jw)        =  nc3d(iw,kw,jw)              !QNDROP is used in RRTMG radiation scheme
             ni3d(iw,kw,jw)          = state_q(1,kflip,5) * multFrc
             !nr3d ans ns3d are in the units of #/kg but nrout and nsout are in the units of #/m3. 
             !Therefore these variables are multiplied by ALT (inverse density(m3/kg))
             ns3d(iw,kw,jw)          = nsout(1,kflip) * alt(iw,kw,jw) * multFrc  
             nr3d(iw,kw,jw)          = nrout(1,kflip) * alt(iw,kw,jw) * multFrc  
             
             th(iw,kw,jw)            = (state_s(1,kflip)-gravit*z_sea_level(iw,kw,jw))/(cpair*pi_phy(iw,kw,jw))
             wsedl3d(iw,kw,jw)       = wsedl(1,kflip)
             cldfra_old_mp(iw,kw,jw) = ast(1,kflip)

             !Update RH - PMA
             ttt(1,kflip)     = (state_s(1,kflip)-gravit*z_sea_level(iw,kw,jw))/cpair
             esl(1,kflip)     = polysvp(ttt(1,kflip),0)
             qvs(1,kflip)     = max(1.e-30_r8,epsqs*esl(1,kflip)/(state_pmid(1,kflip)-(1._r8-epsqs)*esl(1,kflip)))
             rh_old_mp(iw,kw,jw)     = max(0._r8,state_q(1,kflip,1) / qvs(1,kflip))
             lcd_old_mp(iw,kw,jw)    = alst(1,kflip)
             lradius(iw,kw,jw)    = efcout(1,kflip)
             iradius(iw,kw,jw)    = efiout(1,kflip)
             
#if ( WRF_CHEM == 1 )
             if(chem_opt .NE. 0 .and. config_flags%CAM_MP_MAM_cpled ) then
                do l = p1st, num_chem
                   l2 = lptr_chem_to_q(l)
                   if ((l2 >= 1) .and. (l2 <= pcnst)) then
                      chem(iw,kw,jw,l) = state_q(1,kflip,l2)/factconv_chem_to_q(l)
                   end if
                   l2 = lptr_chem_to_qqcw(l)
                   if ((l2 >= 1) .and. (l2 <= pcnst)) then
                      chem(iw,kw,jw,l) = qqcw(1,kflip,l2)/factconv_chem_to_q(l)
                   end if
                end do ! l
             endif
             
             qme3d(iw,kw,jw)               = qme(1,kflip)
             prain3d(iw,kw,jw)             = prain(1,kflip)
             nevapr3d(iw,kw,jw)            = nevapr(1,kflip)
             rate1ord_cw2pr_st3d(iw,kw,jw) = rate1ord_cw2pr_st(1,kflip)
#endif   
          end do
          
          ! Precipitation(for each time step) is stored in RAINNCV variable of WRF. Following precipitation 
          ! calculation is taken from 'PRECL'variable of CAM. In 'cam_diagnostic.F90' file of CAM, PRECL is 
          ! computed as PRECL=PREC_SED+PREC_PCW.
          ! PREC_PCW and PREC_SED are output from CAMMGMP (or stratiform.F90 in CAM). Here, we store PREC_SED+PREC_PCW
          ! (i.e. PRECL) in RAINNCV variable of WRF. Units of PRECL are m/s. We convert it to mm (millimeter) 
          ! by multiplying it by 1.e3 and multiplying it by 'DT'
          
          rainncv(iw,jw) = (prec_sed(1) + prec_pcw(1))*dtime*1.0e3_r8 ! in mm
          rainnc (iw,jw) = rainnc(iw,jw)+ rainncv(iw,jw)

          !Similar to RAINNCV, SNOWNCV is computed following CAM's SNOWL variable in 'cam_diagnostic.F90'
          snowncv(iw,jw) = (snow_sed(1)  + snow_pcw(1))*dtime*1.0e3_r8 ! in mm
          snownc (iw,jw) = snownc(iw,jw) + snowncv(iw,jw)

          sr(iw,jw) = snowncv(iw,jw)/(rainncv(iw,jw) + 1.E-12_r8)

        
       enddo !iw loop
    enddo !jw loop
  end subroutine CAMMGMP

  
  
  
  !---------------------------------------------------------------------------------------------
  subroutine physics_update(state_lchnk,dt,state_q,ptend_q,state_s,ptend_s,ptend_name,ptend_lq,ptend_ls,pcnst_in)
  ! This subroutine partially mimics CAM's subroutine by similar name in physics_type.F90 module
  ! Please note that it is implimented just to serve the purpose of porting CAMMGMP scheme 
  !
  !Author: Balwinder.Singh@pnl.gov
  !---------------------------------------------------------------------------------------------

    implicit none
    integer , intent(in) :: state_lchnk, pcnst_in
    real(r8), intent(in) :: dt
    
    character*24, intent(inout) :: ptend_name    
    
    logical , intent(inout) :: ptend_ls
    logical , intent(inout) :: ptend_lq(pcnst_in)
    real(r8), intent(inout) :: ptend_s(pcols,pver)
    real(r8), intent(inout) :: ptend_q(pcols,pver,pcnst_in)
    
    real(r8), intent(inout) :: state_q(pcols,pver,pcnst_in)
    real(r8), intent(inout) :: state_s(pcols,pver)
    
    character*40            :: name    ! param and tracer name for qneg3
    character(len=16)       :: microp_scheme
    integer                 :: m,i,k,ncol
    integer                 :: ixcldice, ixcldliq, ixnumice, ixnumliq


    microp_scheme   = 'MG' 
    ptend_top_level = 1
    ptend_bot_level = pver
    ncol = pcols

    ! Update dry static energy
    if(ptend_ls) then
       do k = ptend_top_level, ptend_bot_level
          do i = 1, ncol
             state_s(i,k)   = state_s(i,k)   + ptend_s(i,k) * dt
          end do
       end do
    end if

    ! Update constituents, all schemes use time split q: no tendency kept
    call cnst_get_ind('CLDICE', ixcldice, abort=.false.)
    call cnst_get_ind('CLDLIQ', ixcldliq, abort=.false.)
    ! Check for number concentration of cloud liquid and cloud ice (if not present
    ! the indices will be set to -1)
    call cnst_get_ind('NUMICE', ixnumice, abort=.false.)
    call cnst_get_ind('NUMLIQ', ixnumliq, abort=.false.)
 
    do m = 1, pcnst_in
       if(ptend_lq(m)) then
          do k = ptend_top_level, ptend_bot_level
             do i = 1,ncol
                state_q(i,k,m) = state_q(i,k,m) + ptend_q(i,k,m) * dt
             end do
          end do

          ! now test for mixing ratios which are too small
          ! don't call qneg3 for number concentration variables
          if (m .ne. ixnumice  .and.  m .ne. ixnumliq) then
             name = trim(ptend_name) // '/' // trim(cnst_name(m))
             call qneg3(trim(name), state_lchnk, ncol, pcols, pver, m, m, qmin(m), state_q(1,1,m))
          else
             do k = ptend_top_level, ptend_bot_level
                do i = 1,ncol
                   ! checks for number concentration
                   state_q(i,k,m) = max(1.e-12_r8,state_q(i,k,m))
                   state_q(i,k,m) = min(1.e10_r8,state_q(i,k,m))
                end do
             end do
          end if

       end if
    end do

    ! special tests for cloud liquid
    if (ixcldliq > 1) then
       if(ptend_lq(ixcldliq)) then
          if (ptend_name == 'stratiform' .or. ptend_name == 'cldwat'  ) then

          else if (ptend_name == 'convect_deep') then
             where (state_q(:ncol,:pver,ixcldliq) < 1.e-36_r8)
                state_q(:ncol,:pver,ixcldliq) = 0._r8
             end where
             ! also zero out number concentration
             if ( microp_scheme .eq. 'MG' ) then
                where (state_q(:ncol,:pver,ixcldliq) < 1.e-36_r8)
                   state_q(:ncol,:pver,ixnumliq) = 0._r8
                end where
             end if
          end if
       end if
    end if

    ! special tests for cloud ice
    if (ixcldice > 1) then
       if(ptend_lq(ixcldice)) then
          if (ptend_name == 'stratiform' .or. ptend_name == 'cldwat'  ) then

          else if (ptend_name == 'convect_deep') then
             where (state_q(:ncol,:pver,ixcldice) < 1.e-36_r8)
                state_q(:ncol,:pver,ixcldice) = 0._r8
             end where
             ! also zero out number concentration
             if ( microp_scheme .eq. 'MG' ) then
                where (state_q(:ncol,:pver,ixcldice) < 1.e-36_r8)
                   state_q(:ncol,:pver,ixnumice) = 0._r8
                end where
             end if
          end if
       end if
    end if

    ! Derive new temperature and geopotential fields if heating or water tendency not 0.
    !if (ptend_ls .or. ptend_lq(1)) then
    !   call geopotential_dse(                                                                    &
    !        state_lnpint, state_lnpmid, state_pint  , state_pmid  , state_pdel  , state_rpdel  , &
    !        state_s     , state_q(1,1,1),state_phis , rair        , gravit      , cpair        , &
    !        zvir        , state_t     , state_zi    , state_zm    , ncol         )
    !end if

    ! Reset all parameterization tendency flags to false
    call physics_ptend_reset(ptend_name,ptend_q,ptend_s,ptend_lq,ptend_ls,pcnst_in)
    
  end subroutine physics_update
  
  
  !---------------------------------------------------------------------------------------------
  subroutine physics_ptend_init(ptend_name,ptend_q,ptend_s,ptend_lq,ptend_ls,pcnst_in)
  ! This subroutine partially mimics CAM's subroutine by similar name in physics_type.F90 module
  ! Please note that it is implimented just to serve the purpose of porting CAMMGMP scheme 
  !Author: Balwinder.Singh@pnl.gov
  !---------------------------------------------------------------------------------------------
    implicit none
    integer, intent(in)    :: pcnst_in

    character*24, intent(inout) :: ptend_name

    logical , intent(inout):: ptend_ls 
    logical , intent(inout):: ptend_lq(pcnst_in)
    real(r8), intent(inout):: ptend_s(pcols,pver)
    real(r8), intent(inout):: ptend_q(pcols,pver,pcnst_in)
    
    ptend_name  = "none"
    ptend_lq(:) = .TRUE.
    ptend_ls    = .TRUE.
    call physics_ptend_reset(ptend_name,ptend_q,ptend_s,ptend_lq,ptend_ls,pcnst_in)
    return
  end subroutine physics_ptend_init
  
  
!---------------------------------------------------------------------------------------------
  subroutine physics_ptend_reset(ptend_name,ptend_q,ptend_s,ptend_lq,ptend_ls,pcnst_in)
! This subroutine partially mimics CAM's subroutine by similar name in physics_type.F90 module
! Please note that it is implimented just to serve the purpose of porting CAMMGMP scheme 
!Author: Balwinder.Singh@pnl.gov
!---------------------------------------------------------------------------------------------
    implicit none
    integer, intent(in)    :: pcnst_in

    character*24, intent(inout) :: ptend_name
    
    logical , intent(inout):: ptend_ls 
    logical , intent(inout):: ptend_lq(pcnst_in)
    real(r8), intent(inout):: ptend_s(pcols,pver)
    real(r8), intent(inout):: ptend_q(pcols,pver,pcnst_in)
    integer :: m
    
    if(ptend_ls) then
       ptend_s = 0._r8
    endif
    do m = 1, pcnst_in
       if(ptend_lq(m)) then
          ptend_q(:,:,m) = 0._r8
       endif
    end do

    ptend_name  = "none"
    ptend_lq(:) = .FALSE.
    ptend_ls    = .FALSE.

    ptend_top_level = 1
    ptend_bot_level = pver

    return
  end subroutine physics_ptend_reset
  

!---------------------------------------------------------------------------------------------
  subroutine physics_ptend_sum(ptend_ls,ptend_lq,ptend_s,ptend_q,ptend_sum_ls,ptend_sum_lq,ptend_sum_s,ptend_sum_q, pcnst_in)
! This subroutine partially mimics CAM's subroutine by similar name in physics_type.F90 module
! Please note that it is implimented just to serve the purpose of porting CAMMGMP scheme 
!Author: Balwinder.Singh@pnl.gov
!---------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------
! Add ptend fields to ptend_sum for ptend logical flags = .true.
! Where ptend logical flags = .false, don't change ptend_sum
!-----------------------------------------------------------------------

!------------------------------Arguments--------------------------------
    integer,  intent(in) :: pcnst_in
    logical,  intent(in) :: ptend_ls                  ! New parameterization flag
    logical,  intent(in) :: ptend_lq(pcnst_in)           ! New parameterization flag
    real(r8), intent(in) :: ptend_s(pcols,pver)       ! New parameterization tendency
    real(r8), intent(in) :: ptend_q(pcols,pver,pcnst_in) ! New parameterization tendency
    
    logical,  intent(inout) :: ptend_sum_ls                  ! Flag for ptend_sum 
    logical,  intent(inout) :: ptend_sum_lq(pcnst_in)           ! Flag for ptend_sum 
    real(r8), intent(inout) :: ptend_sum_s(pcols,pver)       ! Sum of incoming ptend_sum and ptend
    real(r8), intent(inout) :: ptend_sum_q(pcols,pver,pcnst_in) ! Sum of incoming ptend_sum and ptend
    

!---------------------------Local storage-------------------------------
    integer :: k,m                              ! constituent indices
    integer :: ncol,i
    
!-----------------------------------------------------------------------
    ptend_top_level = 1
    ptend_bot_level = pver

    ncol = pcols
    
    if(ptend_ls) then
       ptend_sum_ls = .true.
       do i = 1, ncol
          do k = ptend_top_level, ptend_bot_level
             ptend_sum_s(i,k) = ptend_sum_s(i,k) + ptend_s(i,k)
          end do
       end do
    end if

! Update constituents
    do m = 1, pcnst_in
       if(ptend_lq(m)) then
          ptend_sum_lq(m) = .true.
          do i = 1,ncol
             do k = ptend_top_level, ptend_bot_level
                ptend_sum_q(i,k,m) = ptend_sum_q(i,k,m) + ptend_q(i,k,m)
             end do
          end do
       end if
    end do

  end subroutine physics_ptend_sum


!---------------------------------------------------------------------------------------------
  subroutine cal_cldfra_mp_1d(cldl,cldi,qqv, qqc, qqi,tt,pp,xland_pt,snowh_pt)
    !Author: Po-Lun Ma
    !Ported by: Balwinder Singh
!---------------------------------------------------------------------------------------------

    use wv_saturation,      only: epsqs, polysvp
    use shr_kind_mod,  only: r8=>shr_kind_r8
    use physconst,          only: tmelt
    
    real(r8), intent(in) :: qqv
    real(r8), intent(in) :: qqc
    real(r8), intent(in) :: qqi
    real(r8), intent(in) :: tt
    real(r8), intent(in) :: pp
    real(r8), intent(in) :: xland_pt
    real(r8), intent(in) :: snowh_pt
    real(r8), intent(inout) :: cldl,cldi
    REAL(r8):: rhl,dV,esl,omeps,cldrh,qvl
    real(r8):: cldcr,esi,qvi,rhi,rhdif,icimr,qv,qc,qi
    
    qv=max(qqv,0._r8)
    qc=max(qqc,0._r8)
    qi=max(qqi,0._r8)
    cldi= 0._r8
    cldl= 0._r8
    
    
    cldrh=1._r8
    
    if (pp >= 70000._r8) then
       if (xland_pt < 1.1_r8 .and. snowh_pt < 0.001_r8) then    !xland_pt=1: land, snowh_pt<1e-6m=1e-3kgm-2
          cldcr = 0.7875_r8
       else
          cldcr = 0.8875_r8
       endif
    elseif (pp <= 40000._r8) then
       cldcr = 0.8_r8
    else
       cldcr = (0.8875_r8 - 0.8_r8)/30000._r8*(pp-40000._r8)+0.8_r8
    end if
    
    cldcr=min(max(0._r8,cldcr),1._r8)
    
    dV=cldrh-cldcr
    omeps = 1._r8 - 0.622_r8
    rhdif = 0._r8
    icimr = 0._r8
    
    esl  = polysvp(tt,0)
    qvl  = max(1.e-12_r8,epsqs*esl/(pp-(1._r8-epsqs)*esl))
    rhl  = (qv+qc)/qvl
    rhl  = min(1.1_r8,max(0._r8,rhl))
    if( rhl .ge. 1._r8 ) then
       cldl  = 1._r8
    elseif( rhl .gt. (cldrh-dV/6._r8) .and. rhl .lt. 1._r8 ) then
       cldl  = 1._r8 - (-3._r8/sqrt(2._r8)*(rhl-cldrh)/dV)**(2._r8/3._r8)
    elseif( rhl .gt. (cldrh-dV) .and. rhl .le. (cldrh-dV/6._r8) ) then
       cldl  = 4._r8*(cos((1._r8/3._r8)*(acos((3._r8/2._r8/sqrt(2._r8))* &
            (1._r8+(rhl-cldrh)/dV))-2._r8*3.14159_r8)))**2._r8
    elseif( rhl .le. (cldrh-dV) ) then
       cldl  = 0._r8
    endif
    
    esi  = polysvp(tt,1)
    if (tt.gt.tmelt) esi=esl
    qvi  = max(1.e-12_r8,epsqs*esi/(pp-(1._r8-epsqs)*esi))
    rhi  = (qv+qi)/qvi
    rhi  = min(1.1_r8,max(0._r8,rhi))
    
    if (tt < 273.15_r8) then
       rhdif = (rhi-0.8_r8) / 0.3_r8   ! rhmaxi=1.1 - rhmini=0.8
       cldi  = min(1._r8, max(rhdif,0._r8)**2._r8)
       if (qi.lt.1.e-12_r8) then
          cldi=0._r8
       else
          cldi=max(1.e-4_r8,cldi)
       endif
       
       if (qi.ge.1.e-12_r8) then
          icimr=qi/(max(cldi,1.e-20_r8))
          if (icimr.lt.1.e-7_r8) then
             cldi = max(0._r8,min(1._r8,qi/1.e-7_r8))
          endif
          if (icimr.gt.5.e-3_r8) then
             cldi = max(0._r8,min(1._r8,qi/5.e-3_r8))
          endif
       endif
    endif
    
    cldl=min(max(0._r8,cldl),1._r8)
    cldi=min(max(0._r8,cldi),1._r8)
    
  end subroutine cal_cldfra_mp_1d


  !---------------------------------------------------------------------------------------------
  subroutine Macrophysics_simple(is_first_step, pcnst, pcols, kte, ixcldliq, ixnumliq, &
       lchnk, dtime, state1_t, state_pmid, state_q, dp_icwmr, sh_icwmr, dp_frac,    &
       sh_frac, ast,aist,alst,qi_mac,xland_pt,snowh_pt,                             &
       !intent-inout
       state1_s, state1_q, rh_old, cldo, esl, qvs, ptend_loc_name, ptend_loc_ls,    &
       ptend_loc_lq, ptend_loc_s,ptend_loc_q, ptend_all_name, ptend_all_ls,         &
       ptend_all_lq,ptend_all_s,ptend_all_q )

    ! This subroutine implements macrophysical processes
    ! 
    ! Author: Po-Lun Ma
    ! Ported by Balwinder Singh    
    !-------------------------------------------------------------------------------------------

    !PMA
    !-------------------------------------------------------------------------------------------
    !This simple scheme does the followings:
    !(1) QC detrained from convections are converted to (a) cloud water and (b) cloud ice in the
    !    cloudy part; and (c) vapor in the cloud-free part then latent heating from the phase 
    !    change
    !(2) RH change, then apportion the change of Qv to cloudy part and cloud-free part, then 
    !    latent heating from phase change
    !!(3) If a grid is super-saturated, condense to Qc and update latent heating
    !(4) Brutal positive moisture fixer
    !
    !The idea is to take the essential formulations from Macrophysics in CAM5, but simplify the 
    !computation. When the full CAM5 macrophysics is implemented, this scheme should be removed
    !Cloud fraction calculation is in this module
    !-------------------------------------------------------------------------------------------

    use wv_saturation, only: epsqs, polysvp
    use physconst,     only: latvap,cpair 

    !Subroutine arguments

    !Intent -ins
    logical,  intent(in) :: is_first_step
    integer,  intent(in) :: pcnst, pcols, kte, ixcldliq, ixnumliq, lchnk
    real(r8), intent(in) :: dtime,xland_pt,snowh_pt

    real(r8), dimension(pcols,kte), intent(in) :: state_pmid
    real(r8), dimension(pcols,kte), intent(in) :: dp_icwmr
    real(r8), dimension(pcols,kte), intent(in) :: sh_icwmr
    real(r8), dimension(pcols,kte), intent(in) :: dp_frac
    real(r8), dimension(pcols,kte), intent(in) :: sh_frac
    real(r8), dimension(pcols,kte), intent(in) :: qi_mac

    real(r8), dimension(pcols,kte,pcnst), intent(in) :: state_q

    !Intents-inouts
    character*24, intent(inout) :: ptend_loc_name, ptend_all_name  

    logical, intent(inout) :: ptend_loc_ls, ptend_all_ls    !Flag for updating tendencies
    logical, dimension(pcnst),intent(inout) :: ptend_loc_lq !Flag for updating tendencies
    logical, dimension(pcnst),intent(inout) :: ptend_all_lq !Flag for updating tendencies

    real(r8), dimension(pcols,kte),intent(inout) :: ptend_loc_s
    real(r8), dimension(pcols,kte),intent(inout) :: ptend_all_s
    real(r8), dimension(pcols,kte),intent(inout) :: rh_old  ! RH at previous timestep (PMA)
    real(r8), dimension(pcols,kte),intent(inout) :: ast
    real(r8), dimension(pcols,kte),intent(inout) :: alst
    real(r8), dimension(pcols,kte),intent(inout) :: aist
    real(r8), dimension(pcols,kte),intent(inout) :: cldo    ! Old cloud fraction
    real(r8), dimension(pcols,kte),intent(inout) :: esl     ! liquid sat vapor pressure (pa) ! for phil suggested modifications
    real(r8), dimension(pcols,kte),intent(inout) :: qvs     ! liquid saturation vapor mixing ratio ! for phil suggested modifications
    real(r8), dimension(pcols,kte),intent(inout) :: state1_s
    real(r8), dimension(pcols,kte),intent(inout) :: state1_t

    real(r8), dimension(pcols,kte,pcnst), intent(inout) :: ptend_loc_q
    real(r8), dimension(pcols,kte,pcnst), intent(inout) :: ptend_all_q
    real(r8), dimension(pcols,kte,pcnst), intent(inout) :: state1_q


    !Local workspace
    integer  :: k
    real(r8), dimension(pcols,kte) :: cv_frac     ! Total convective cloud fraction (PMA)
    real(r8), dimension(pcols,kte) :: st_frac     ! Total stratiform cloud fraction (PMA)
    real(r8), dimension(pcols,kte) :: ttt         ! Temperature (PMA)
    real(r8), dimension(pcols,kte) :: rh_curr     ! RH at current  timestep (PMA)
    real(r8), dimension(pcols,kte) :: del_rh      ! RH difference between 2 timesteps (PMA)
    real(r8), dimension(pcols,kte) :: qc_nc       ! non-convective LWC (PMA)
    real(r8), dimension(pcols,kte) :: del_qq      ! Qv difference between 2 timesteps (PMA)
    real(r8), dimension(pcols,kte) :: del_cf      ! Qv difference between 2 timesteps (PMA)
    real(r8), dimension(pcols,kte) :: rh_temp     !

    real(r8), dimension(pcols) :: qsll        !
    real(r8), dimension(pcols) :: tsp         !
    real(r8) :: cldcr
    real(r8) :: factcc,fact_adj


    character*250 :: wrfmessage

   
    !
    !restore flags to true
    !

    ptend_loc_name         = 'Macro1'
    ptend_loc_ls           = .true.
    ptend_loc_lq(1)        = .true.
    ptend_loc_lq(ixcldliq) = .true.
    ptend_loc_lq(ixnumliq) = .true.

    factcc   = 0.1_r8
    fact_adj = 0.35_r8
    cldcr    = 0.8875_r8 
    !
    !Simple computation for condensation/evaporation 
    !
    do k=1,pver
      if (state_pmid(1,k) >= 70000._r8) then
         if (xland_pt < 1.1_r8 .and. snowh_pt < 0.001_r8) then    !xland_pt=1: land, snowh_pt<1e-6m=1e-3kgm-2
          cldcr = 0.7875_r8
         else
          cldcr = 0.8875_r8
         endif
      elseif (state_pmid(1,k)<= 40000._r8) then
        cldcr = 0.8_r8
      else   
        cldcr = (0.8875_r8 - 0.8_r8)/30000._r8*(state_pmid(1,k)-40000._r8)+0.8_r8
      end if 

       cldcr=min(max(0._r8,cldcr),1._r8)

       call cal_cldfra_mp_1d(alst(1,k),aist(1,k),state1_q(1,k,1), state1_q(1,k,ixcldliq), qi_mac(1,k),state1_t(1,k),state_pmid(1,k),xland_pt,snowh_pt)
       qsll         = 0._r8
       tsp          = 0._r8
       del_qq(1,k)  = 0._r8
       rh_temp(1,k) = 0._r8
       cv_frac(1,k) = max(0._r8,min(0.8_r8,dp_frac(1,k)+sh_frac(1,k)))
       alst(1,k)    = (1._r8-cv_frac(1,k))*alst(1,k)
       aist(1,k)    = (1._r8-cv_frac(1,k))*aist(1,k)
       ast(1,k)     = max(alst(1,k),aist(1,k))
       st_frac(1,k) = min(1._r8,ast(1,k)+cv_frac(1,k))
       esl(1,k)     = polysvp(state1_t(1,k),0)
       qvs(1,k)     = max(1.e-30_r8,epsqs*esl(1,k)/(state_pmid(1,k)-(1._r8-epsqs)*esl(1,k)))
       rh_curr(1,k) = max(0._r8,state1_q(1,k,1) / qvs(1,k))
       qc_nc(1,k)   = state1_q(1,k,ixcldliq)
             
       if( is_first_step ) then
          rh_old(1,k) = rh_curr(1,k)
          cldo(1,k)   = min(1._r8,ast(1,k)+cv_frac(1,k))
       endif
       
       !
       !if super-saturate, condense delta RH
       !
       
       if (rh_curr(1,k) > 1._r8) then
          call findsp_water(lchnk,pcols,state1_q(1,k,1),state1_t(1,k),state_pmid(1,k),tsp,qsll)
          del_qq(1,k)               = min(3.e-3_r8,max(0._r8,(state1_q(1,k,1)-qsll(1))*0.999_r8))  
          ptend_loc_q(1,k,ixcldliq) =  del_qq(1,k) /dtime
          ptend_loc_s(1,k)          =  del_qq(1,k) * latvap/dtime !condensational heating
          ptend_loc_q(1,k,1)        = -del_qq(1,k)/dtime

          !
          !if not super-saturate, compare RH diff between two timesteps,
          !then condense/evaporate the cloudy part
          !
       else if (rh_curr(1,k) > cldcr .and. rh_curr(1,k) <= 1._r8 ) then
          del_cf(1,k) = st_frac(1,k)-cldo(1,k)  
          if( del_cf(1,k) < 0._r8 ) then
             del_qq(1,k)               = qc_nc(1,k)*del_cf(1,k)/cldo(1,k)*factcc
             ptend_loc_q(1,k,ixcldliq) = del_qq(1,k)  /dtime
             ptend_loc_s(1,k)          = del_qq(1,k)  * latvap/dtime !evaporative cooling
             ptend_loc_q(1,k,1)        = -del_qq(1,k) /dtime
             ptend_loc_q(1,k,ixnumliq) = ptend_loc_q(1,k,ixcldliq)*state1_q(1,k,ixnumliq)/max(1.e-30_r8,state1_q(1,k,ixcldliq))
             ptend_loc_q(1,k,ixnumliq) = min(0._r8,max(ptend_loc_q(1,k,ixnumliq),-state1_q(1,k,ixnumliq)/dtime))
          else if( del_cf(1,k) > 0._r8 .and. cldo(1,k) > 1.e-8_r8) then
             del_qq(1,k)               = min(state1_q(1,k,1)*0.1_r8,min(3.e-3_r8*st_frac(1,k),qc_nc(1,k)*del_cf(1,k)/cldo(1,k)))*factcc
             ptend_loc_q(1,k,ixcldliq) = del_qq(1,k)  /dtime
             ptend_loc_s(1,k)          = del_qq(1,k)  * latvap/dtime !condensational heating 
             ptend_loc_q(1,k,1)        = -del_qq(1,k) /dtime
             ptend_loc_q(1,k,ixnumliq) = 0._r8
          else if( del_cf(1,k) > 0._r8 .and. cldo(1,k) < 1.e-8_r8) then
             del_qq(1,k)               = min(state1_q(1,k,1)*0.1_r8,2.e-5_r8*st_frac(1,k))*factcc
             ptend_loc_q(1,k,ixcldliq) = del_qq(1,k)  /dtime
             ptend_loc_s(1,k)          = del_qq(1,k)  * latvap/dtime !condensational heating 
             ptend_loc_q(1,k,1)        = -del_qq(1,k) /dtime
             ptend_loc_q(1,k,ixnumliq) = 0._r8
          else
             del_qq(1,k)               = 0._r8
             ptend_loc_q(1,k,ixcldliq) = 0._r8
             ptend_loc_s(1,k)          = 0._r8
             ptend_loc_q(1,k,1)        = 0._r8
             ptend_loc_q(1,k,ixnumliq) = 0._r8
          end if
       else if (rh_curr(1,k) < cldcr  .and. cldo(1,k) <1.e-2_r8 .and. qc_nc(1,k) >1.e-12_r8) then !RH<cldcr, evaporate qc
          call findsp_water(lchnk,pcols,state1_q(1,k,1),state1_t(1,k),state_pmid(1,k),tsp,qsll)                      
             del_qq(1,k)= max(min(qc_nc(1,k), (qsll(1)-state1_q(1,k,1))),0._r8)*0.99_r8
             ptend_loc_q(1,k,ixcldliq) = -del_qq(1,k) /dtime
             ptend_loc_s(1,k)          = -del_qq(1,k) * latvap/dtime !evaporative cooling
             ptend_loc_q(1,k,1)        =  del_qq(1,k)/dtime
             ptend_loc_q(1,k,ixnumliq) = ptend_loc_q(1,k,ixcldliq)*state1_q(1,k,ixnumliq)/max(1.e-30_r8,state1_q(1,k,ixcldliq))
             ptend_loc_q(1,k,ixnumliq) = min(0._r8,max(ptend_loc_q(1,k,ixnumliq),-state1_q(1,k,ixnumliq)/dtime))
       else if (rh_curr(1,k) < cldcr  .and. cldo(1,k) >=1.e-2_r8 .and. qc_nc(1,k) >1.e-12_r8) then !RH<cldcr, evaporate qc
          call findsp_water(lchnk,pcols,state1_q(1,k,1),state1_t(1,k),state_pmid(1,k),tsp,qsll)
             del_qq(1,k)= max(min(qc_nc(1,k), qsll(1)-state1_q(1,k,1)),0._r8)*factcc
             ptend_loc_q(1,k,ixcldliq) = -del_qq(1,k) /dtime
             ptend_loc_s(1,k)          = -del_qq(1,k) * latvap/dtime !evaporative cooling
             ptend_loc_q(1,k,1)        =  del_qq(1,k)/dtime
             ptend_loc_q(1,k,ixnumliq) = ptend_loc_q(1,k,ixcldliq)*state1_q(1,k,ixnumliq)/max(1.e-30_r8,state1_q(1,k,ixcldliq))
!             ptend_loc_q(1,k,ixnumliq) = ptend_loc_q(1,k,ixcldliq)*state1_q(1,k,ixnumliq)/max(1.e-30_r8,qc_nc(1,k))
             ptend_loc_q(1,k,ixnumliq) = min(0._r8,max(ptend_loc_q(1,k,ixnumliq),-state1_q(1,k,ixnumliq)/dtime))
       else
             ptend_loc_q(1,k,ixcldliq) = 0._r8
             ptend_loc_s(1,k)          = 0._r8
             ptend_loc_q(1,k,1)        = 0._r8
             ptend_loc_q(1,k,ixnumliq) = 0._r8
       end if
       state1_t(1,k)=state1_t(1,k)+ptend_loc_s(1,k)*dtime/cpair
    end do  !k loop
    
    !
    !Update state1 variables and tendencies 
    !
    
    call physics_ptend_sum(ptend_loc_ls,ptend_loc_lq,ptend_loc_s,ptend_loc_q,ptend_all_ls,ptend_all_lq,ptend_all_s,ptend_all_q,pcnst)
    ptend_all_name = 'cldwat'
    call physics_update( lchnk,dtime,state1_q,ptend_loc_q,state1_s,ptend_loc_s,ptend_loc_name,ptend_loc_lq,ptend_loc_ls,pcnst)
    call physics_ptend_init( ptend_loc_name,ptend_loc_q,ptend_loc_s, ptend_loc_lq,ptend_loc_ls,pcnst)

    !
    !restore flags to true
    !
    ptend_loc_name         = 'Macro2'
    ptend_loc_ls           = .true.
    ptend_loc_lq(1)        = .true.
    ptend_loc_lq(ixcldliq) = .true.
    ptend_loc_lq(ixnumliq) = .true.
    
    !
    !Some limiters
    !
    
    do k = 1 , pver
       del_qq(1,k)=0._r8
       call cal_cldfra_mp_1d(alst(1,k),aist(1,k),state1_q(1,k,1), state1_q(1,k,ixcldliq), qi_mac(1,k),state1_t(1,k),state_pmid(1,k),xland_pt,snowh_pt)
       alst(1,k) = (1._r8-cv_frac(1,k))*alst(1,k)
       aist(1,k) = (1._r8-cv_frac(1,k))*aist(1,k)
       ast(1,k)  = max(alst(1,k),aist(1,k))
       st_frac(1,k) = alst(1,k)

       qc_nc(1,k) = max(0._r8, state1_q(1,k,ixcldliq) - &
            max(0._r8,(dp_frac(1,k)*dp_icwmr(1,k) +sh_frac(1,k)*sh_icwmr(1,k))))
       !
       !Case 1: unrealistic cloud: evaporate qc under no cldfra 
       !
       ! THIS SHOULD NO LONGER HAPPEN
       !
       !       if (st_frac(1,k) < 1.e-8_r8 ) then
       !          if ( qc_nc(1,k) > 0._r8 ) then
       !           ptend_loc_q(1,k,ixcldliq) = -(qc_nc(1,k))/dtime*0.99_r8
       !           ptend_loc_q(1,k,1)= qc_nc(1,k)/dtime*0.99_r8
       !           ptend_loc_s(1,k)  =-(qc_nc(1,k)) * latvap /dtime*0.99_r8  !evaporative cooling
       !           ptend_loc_q(1,k,ixnumliq) = -state1_q(1,k,ixnumliq)/dtime*0.99_r8
       !           ptend_loc_q(1,k,ixnumliq) = min(0._r8,-0.99_r8*state1_q(1,k,ixnumliq)/dtime)
       !          end if
          
       if (st_frac(1,k) > 1.e-5_r8 ) then
          !       else  !if stratiform cldfra not zero
          !  
          !Case 2: dense cloud: evaporate some stratiform qc until qc_max
          !     
          if (qc_nc(1,k) > 3.e-3_r8 * st_frac(1,k)) then
             del_qq(1,k)               = qc_nc(1,k)-3.e-3_r8* st_frac(1,k)*fact_adj
             ptend_loc_q(1,k,ixcldliq) = -del_qq(1,k)/dtime
             ptend_loc_q(1,k,1)        =  del_qq(1,k)/dtime
             ptend_loc_s(1,k)          = -del_qq(1,k) * latvap /dtime  !evaporative cooling
             ptend_loc_q(1,k,ixnumliq) = ptend_loc_q(1,k,ixcldliq)*state1_q(1,k,ixnumliq)/max(1.e-20_r8,state1_q(1,k,ixcldliq))
             ptend_loc_q(1,k,ixnumliq) = min(0._r8,max(ptend_loc_q(1,k,ixnumliq),-state1_q(1,k,ixnumliq)/dtime))
             
             !     
             !Case 3: empty cloud: condense stratiform qc until qc_min when cldfra is not zero
             !    
          else if (qc_nc(1,k) < 2.e-5_r8 * st_frac(1,k)) then
             del_qq(1,k)               = 2.e-5_r8 *st_frac(1,k)-qc_nc(1,k)
             del_qq(1,k)               = max(0._r8,min(del_qq(1,k), state1_q(1,k,1)*0.1_r8))*fact_adj
             ptend_loc_q(1,k,ixcldliq) = del_qq(1,k)/dtime
             ptend_loc_q(1,k,1)        = -del_qq(1,k)/dtime
             ptend_loc_s(1,k)          = del_qq(1,k)* latvap / dtime  !condensational heating
          end if
       end if
       state1_t(1,k)=state1_t(1,k)+ptend_loc_s(1,k)*dtime/cpair
    end do
    !
    !Update state1 variables and tendencies 
    !
    call physics_ptend_sum(ptend_loc_ls,ptend_loc_lq,ptend_loc_s,ptend_loc_q,ptend_all_ls,ptend_all_lq,ptend_all_s,ptend_all_q,pcnst)
    ptend_all_name = 'cldwat'
    call physics_update( lchnk,dtime,state1_q,ptend_loc_q,state1_s,ptend_loc_s,ptend_loc_name,ptend_loc_lq,ptend_loc_ls,pcnst)
    call physics_ptend_init( ptend_loc_name,ptend_loc_q,ptend_loc_s, ptend_loc_lq,ptend_loc_ls,pcnst)

 go to 2000
    !
    !restore flags to true
    !
    
    ptend_loc_name         = 'Macro3'
    ptend_loc_ls           = .true.
    ptend_loc_lq(1)        = .true.
    ptend_loc_lq(ixcldliq) = .true.
    
    !
    !Last checker to remove supersaturation
    !
    do k=1,pver
       qsll = 0._r8
       tsp  = 0._r8
       del_qq(1,k)  = 0._r8
       esl(1,k)     = polysvp(state1_t(1,k),0)
       qvs(1,k)     = max(1.e-30_r8,epsqs*esl(1,k)/(state_pmid(1,k)-(1._r8-epsqs)*esl(1,k)))
       rh_curr(1,k) = state1_q(1,k,1) / qvs(1,k)
             
       if (rh_curr(1,k) > 1._r8) then
          call findsp_water(lchnk,pcols,state1_q(1,k,1),state1_t(1,k),state_pmid(1,k),tsp,qsll)
          del_qq(1,k)=min(2.e-2_r8,max(0._r8,(state1_q(1,k,1)-qsll(1))*0.999_r8))  
          ptend_loc_q(1,k,ixcldliq) =  del_qq(1,k) /dtime
          ptend_loc_s(1,k)          =  del_qq(1,k) * latvap/dtime !condensational heating
          ptend_loc_q(1,k,1)        = -del_qq(1,k)/dtime
       end if
       state1_t(1,k)=state1_t(1,k)+ptend_loc_s(1,k)*dtime/cpair
    end do

    !Update state1 variables and tendencies 
    !
    call physics_ptend_sum(ptend_loc_ls,ptend_loc_lq,ptend_loc_s,ptend_loc_q,ptend_all_ls,ptend_all_lq,ptend_all_s,ptend_all_q,pcnst)
    ptend_all_name = 'cldwat'
    call physics_update( lchnk,dtime,state1_q,ptend_loc_q,state1_s,ptend_loc_s,ptend_loc_name,ptend_loc_lq,ptend_loc_ls,pcnst)
    call physics_ptend_init( ptend_loc_name,ptend_loc_q,ptend_loc_s, ptend_loc_lq,ptend_loc_ls,pcnst)

    !         
    !positive state1 variables
    !
2000 continue
    
    do k = 1 , pver
       state1_q(1,k,1)        = max(qmin(1),state1_q(1,k,1))
       state1_q(1,k,ixcldliq) = max(qmin(ixcldliq),state1_q(1,k,ixcldliq))
       state1_q(1,k,ixcldice) = max(qmin(ixcldice),state1_q(1,k,ixcldice))
       state1_q(1,k,ixnumliq) = max(qmin(ixnumliq),state1_q(1,k,ixnumliq))
       state1_q(1,k,ixnumice) = max(qmin(ixnumice),state1_q(1,k,ixnumice))
       call  cal_cldfra_mp_1d(alst(1,k),aist(1,k),state1_q(1,k,1), state1_q(1,k,ixcldliq), qi_mac(1,k),state1_t(1,k),state_pmid(1,k),xland_pt,snowh_pt)
!       ast(1,k)=max(alst(1,k),aist(1,k))
       aist(1,k) = (1._r8-cv_frac(1,k))*aist(1,k)
       alst(1,k) = (1._r8-cv_frac(1,k))*alst(1,k)
       ast(1,k)=max(alst(1,k),aist(1,k))
    end do
    !
    !PMA<<<
    !
  end subroutine Macrophysics_simple
  
  subroutine CAMMGMP_INIT(ixcldliq_in,ixcldice_in &
       ,ixnumliq_in,ixnumice_in,chem_opt_in       &    
       ,ids, ide, jds, jde, kds, kde              &
       ,ims, ime, jms, jme, kms, kme              &
       ,its, ite, jts, jte, kts, kte              )        
    
    !!-------------------------------------------- !
    !!                                             !
    !! Initialize the cloud water parameterization !
    !!                                             ! 
    !!-------------------------------------------- !
    use microp_aero,     only: ini_microp_aero
    use cldwat2m_micro,  only: ini_micro
#ifdef MODAL_AERO
    use ndrop,           only: activate_init
#endif
    
    !!-----------------------------------------------------------------------
    implicit none
    integer, intent(in) :: ixcldliq_in, ixcldice_in, ixnumliq_in, ixnumice_in
    integer, intent(in) :: chem_opt_in
    integer, intent(in) :: ids, ide, jds, jde, kds, kde &
         ,ims, ime, jms, jme, kms, kme                  &
         ,its, ite, jts, jte, kts, kte  
    
    !Local variables
    character(len=1000) :: msg
    integer :: jtf,ktf,itf
    
    chem_opt = chem_opt_in

    jtf   = min(jte,jde-1)
    ktf   = min(kte,kde-1)
    itf   = min(ite,ide-1)
    
    !Map CAM veritcal level variables 
    pver  = ktf - kts + 1
    pverp = pver + 1
    
    !!-----------------------------------------------------------------------
    ixcldliq = ixcldliq_in
    ixcldice = ixcldice_in
    ixnumliq = ixnumliq_in
    ixnumice = ixnumice_in
    !! Initialization routine for cloud macrophysics and microphysics
    
    call ini_micro
    call ini_microp_aero
    
#ifdef MODAL_AERO
#if ( WRF_CHEM != 1 )
    !When WRF_CHEM is 1, activate_init is called from MODULE_CAM_MAM_INIT after initializing aerosols
    call activate_init
#else
    !BSINGH:02/01/2013: Sanity check for Non-MAM simulations
    if(.NOT.cam_mam_aerosols .AND. chem_opt .NE. 0) then
       write(msg,*)'CAMMGMP DRIVER - cammgmp is valid for only MAM aerosols ', &
            '(chem_opts:',CBMZ_CAM_MAM3_NOAQ,CBMZ_CAM_MAM3_AQ,CBMZ_CAM_MAM7_NOAQ,CBMZ_CAM_MAM7_AQ ,')'
       call wrf_error_fatal( msg )
    endif
    if(chem_opt == 0)then
       !NOTE*: Not tested yet for other CHEM packages except MAM packages
       call activate_init
    endif
#endif
#endif
    return
  end subroutine CAMMGMP_INIT
subroutine findsp_water (lchnk, ncol, q, t, p, tsp, qsp)
!-----------------------------------------------------------------------
!
! Purpose:
!     find the wet bulb temperature for a given t and q
!     in a longitude height section
!     wet bulb temp is the temperature and spec humidity that is
!     just saturated and has the same enthalpy
!     if q > qs(t) then tsp > t and qsp = qs(tsp) < q
!     if q < qs(t) then tsp < t and qsp = qs(tsp) > q
!
! Method:
! a Newton method is used
! first guess uses an algorithm provided by John Petch from the UKMO
! we exclude points where the physical situation is unrealistic
! e.g. where the temperature is outside the range of validity for the
!      saturation vapor pressure, or where the water vapor pressure
!      exceeds the ambient pressure, or the saturation specific humidity is
!      unrealistic
!
! Author: P. Rasch
!
!-----------------------------------------------------------------------

  use wv_saturation, only: estblf, hlatv, tmin, hlatf, rgasv, pcf, &
                           cp, epsqs, ttrice,polysvp

!
!     input arguments
!
   integer, intent(in) :: lchnk                 ! chunk identifier
   integer, intent(in) :: ncol                  ! number of atmospheric columns

   real(r8), intent(in) :: q(pcols)        ! water vapor (kg/kg)
   real(r8), intent(in) :: t(pcols)        ! temperature (K)
   real(r8), intent(in) :: p(pcols)        ! pressure    (Pa)
!
! output arguments
!
   real(r8), intent(out) :: tsp(pcols)      ! saturation temp (K)
   real(r8), intent(out) :: qsp(pcols)      ! saturation mixing ratio (kg/kg)
!
! local variables
!
   character*250 :: iulog
   integer i                 ! work variable
!  integer k                 ! work variable
   logical lflg              ! work variable
   integer iter              ! work variable
   integer l                 ! work variable
   logical :: error_found

   real(r8) omeps                ! 1 minus epsilon
   real(r8) trinv                ! work variable
   real(r8) es                   ! sat. vapor pressure
   real(r8) desdt                ! change in sat vap pressure wrt temperature
!     real(r8) desdp                ! change in sat vap pressure wrt pressure
   real(r8) dqsdt                ! change in sat spec. hum. wrt temperature
   real(r8) dgdt                 ! work variable
   real(r8) g                    ! work variable
   real(r8) weight(pcols)        ! work variable
   real(r8) hlatsb               ! (sublimation)
   real(r8) hlatvp               ! (vaporization)
   real(r8) hltalt(pcols)   ! lat. heat. of vap.
   real(r8) tterm                ! work var.
   real(r8) qs                   ! spec. hum. of water vapor
   real(r8) tc                   ! crit temp of transition to ice
   real(r8) tt0                  ! Freezing temperature. Needed for findsp 

! work variables
   real(r8) t1, q1, dt, dq
   real(r8) dtm, dqm
   real(r8) qvd, a1, tmp
   real(r8) rair
   real(r8) r1b, c1, c2, c3
   real(r8) denom
   real(r8) dttol
   real(r8) dqtol
   integer doit(pcols)
   real(r8) enin(pcols), enout(pcols)
   real(r8) tlim(pcols)

   omeps = 1.0_r8 - epsqs
!  trinv = 1.0_r8/ttrice  ! put just before it is used, in case ttrice = 0.0  !++xl
   a1 = 7.5_r8*log(10._r8)
   rair =  287.04_r8
   c3 = rair*a1/cp
   dtm = 0._r8    ! needed for iter=0 blowup with f90 -ei
   dqm = 0._r8    ! needed for iter=0 blowup with f90 -ei
   dttol = 1.e-4_r8 ! the relative temp error tolerance required to quit the iteration
   dqtol = 1.e-4_r8 ! the relative moisture error tolerance required to quit the iteration
!  tmin = 173.16 ! the coldest temperature we can deal with
   tt0  = 273.15_r8
!
! max number of times to iterate the calculation
   iter = 8
!

!  Sungsu comment out k loop
!  do k = 1,pver

!
! first guess on the wet bulb temperature
!
      do i = 1,ncol

!#ifdef DEBUG
!         if ( (lchnk == lchnklook(nlook) ) .and. (i == icollook(nlook) ) ) then
!            write (iulog,*) ' '
!            write (iulog,*) ' level, t, q, p', t(i), q(i), p(i)
!         endif
!#endif
! limit the temperature range to that relevant to the sat vap pres tables
!#if ( ! defined WACCM_MOZART )   
!         tlim(i) = min(max(t(i),173._r8),373._r8)
!#else
         tlim(i) = min(max(t(i),128._r8),373._r8)
!#endif
!        es = estblf(tlim(i))    
         es = polysvp(tlim(i),0)  !++xl
         denom = p(i) - omeps*es 
         qs = epsqs*es/denom     
         doit(i) = 0
         enout(i) = 1._r8
! make sure a meaningful calculation is possible
         if (p(i) > 5._r8*es .and. qs > 0._r8 .and. qs < 0.5_r8) then
!
! Saturation specific humidity
!
             qs = min(epsqs*es/denom,1._r8)
!
! "generalized" analytic expression for t derivative of es
!  accurate to within 1 percent for 173.16 < t < 373.16

!++xl
! Weighting of hlat accounts for transition from water to ice
! polynomial expression approximates difference between es over
! water and es over ice from 0 to -ttrice (C) (min of ttrice is
! -40): required for accurate estimate of es derivative in transition
! range from ice to water also accounting for change of hlatv with t
! above freezing where const slope is given by -2369 j/(kg c) = cpv - cw
!  
!            tc     = tlim(i) - tt0
!            lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
!            weight(i) = min(-tc*trinv,1.0_r8)
!            hlatsb = hlatv + weight(i)*hlatf
!            hlatvp = hlatv - 2369.0_r8*tc
!            if (tlim(i) < tt0) then
!               hltalt(i) = hlatsb
!            else
!               hltalt(i) = hlatvp
!            end if
!
! No icephs or water to ice transition
!
             hlatvp = hlatv - 2369.0*(tlim(i)-tt0)
             hlatsb = hlatv
             if (tlim(i) < tt0) then
               hltalt(i) = hlatsb
             else
               hltalt(i) = hlatvp
             end if
!--xl       
             enin(i) = cp*tlim(i) + hltalt(i)*q(i)
         
! make a guess at the wet bulb temp using a UKMO algorithm (from J. Petch)
             tmp =  q(i) - qs
             c1 = hltalt(i)*c3
             c2 = (tlim(i) + 36._r8)**2
             r1b    = c2/(c2 + c1*qs)
             qvd   = r1b*tmp
             tsp(i) = tlim(i) + ((hltalt(i)/cp)*qvd)
!#ifdef DEBUG 
!             if ( (lchnk == lchnklook(nlook) ) .and. (i == icollook(nlook) ) ) then
!                write (iulog,*) ' relative humidity ', q(i)/qs
!                write (iulog,*) ' first guess ', tsp(i)
!             endif
!#endif
!            es = estblf(tsp(i))
             es = polysvp(tsp(i),0)  !++xl 
             qsp(i) = min(epsqs*es/(p(i) - omeps*es),1._r8)
          else
             doit(i) = 1
             tsp(i) = tlim(i)
             qsp(i) = q(i)
             enin(i) = 1._r8
          endif 
       end do   ! end do i
!
! now iterate on first guess
!     
      do l = 1, iter
         dtm = 0
         dqm = 0
         do i = 1,ncol
            if (doit(i) == 0) then
!              es = estblf(tsp(i))
               es = polysvp(tsp(i),0)  !++xl
!
! Saturation specific humidity
!              
               qs = min(epsqs*es/(p(i) - omeps*es),1._r8)
!
! "generalized" analytic expression for t derivative of es
! accurate to within 1 percent for 173.16 < t < 373.16
!
!++xl
! Weighting of hlat accounts for transition from water to ice
! polynomial expression approximates difference between es over
! water and es over ice from 0 to -ttrice (C) (min of ttrice is
! -40): required for accurate estimate of es derivative in transition
! range from ice to water also accounting for change of hlatv with t
! above freezing where const slope is given by -2369 j/(kg c) = cpv - cw
!
!              tc     = tsp(i) - tt0
!              lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
!              weight(i) = min(-tc*trinv,1.0_r8)
!              hlatsb = hlatv + weight(i)*hlatf
!              hlatvp = hlatv - 2369.0_r8*tc
!              if (tsp(i) < tt0) then
!                 hltalt(i) = hlatsb
!              else
!                 hltalt(i) = hlatvp
!              end if
!              if (lflg) then
!                 tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3)+tc*(pcf(4) + tc*pcf(5))))
!              else
!                 tterm = 0.0_r8
!              end if
!              desdt = hltalt(i)*es/(rgasv*tsp(i)*tsp(i)) + tterm*trinv
!
! No icephs or water to ice transition
!              
               hlatvp = hlatv - 2369.0*(tsp(i)-tt0)
               hlatsb = hlatv
               if (tsp(i) < tt0) then
                 hltalt(i) = hlatsb
               else
                 hltalt(i) = hlatvp
               end if
               desdt = hltalt(i)*es/(rgasv*tsp(i)*tsp(i))
!--xl          
               dqsdt = (epsqs + omeps*qs)/(p(i) - omeps*es)*desdt
!              g = cp*(tlim(i)-tsp(i)) + hltalt(i)*q(i)- hltalt(i)*qsp(i)
               g = enin(i) - (cp*tsp(i) + hltalt(i)*qsp(i))
               dgdt = -(cp + hltalt(i)*dqsdt)
               t1 = tsp(i) - g/dgdt
               dt = abs(t1 - tsp(i))/t1
               tsp(i) = max(t1,tmin)
!              es = estblf(tsp(i))
               es = polysvp(tsp(i),0)  !++xl
               q1 = min(epsqs*es/(p(i) - omeps*es),1._r8)
               dq = abs(q1 - qsp(i))/max(q1,1.e-12_r8)
               qsp(i) = q1
!#ifdef DEBUG   
!               if ( (lchnk == lchnklook(nlook) ) .and. (i == icollook(nlook) ) ) then
!                  write (iulog,*) ' rel chg lev, iter, t, q ', l, dt, dq, g
!               endif
!#endif         
               dtm = max(dtm,dt)
               dqm = max(dqm,dq)
! if converged at this point, exclude it from more iterations
               if (dt < dttol .and. dq < dqtol) then
                  doit(i) = 2
               endif
               enout(i) = cp*tsp(i) + hltalt(i)*qsp(i)
! bail out if we are too near the end of temp range
!#if ( ! defined WACCM_MOZART )
               if (tsp(i) < 174.16_r8) then
!#else
!               if (tsp(i) < 130.16_r8) then
!#endif
                  doit(i) = 4
               endif
            else
            endif
         end do              ! do i = 1,ncol

         if (dtm < dttol .and. dqm < dqtol) then
            go to 10
         endif

      end do                 ! do l = 1,iter
10    continue

      error_found = .false.
      if (dtm > dttol .or. dqm > dqtol) then
         do i = 1,ncol
            if (doit(i) == 0) error_found = .true.
         end do
         if (error_found) then
            do i = 1,ncol
               if (doit(i) == 0) then
                  write (iulog,*) ' findsp not converging at point i ', i
                  write (iulog,*) ' t, q, p, enin ', t(i), q(i), p(i), enin(i)
                  write (iulog,*) ' tsp, qsp, enout ', tsp(i), qsp(i), enout(i)
                  call  wrf_error_fatal ('FINDSP')
               endif
            end do
         endif
      endif
      do i = 1,ncol
         if (doit(i) == 2 .and. abs((enin(i)-enout(i))/(enin(i)+enout(i))) > 1.e-4_r8) then
            error_found = .true.
         endif
      end do
      if (error_found) then
         do i = 1,ncol
            if (doit(i) == 2 .and. abs((enin(i)-enout(i))/(enin(i)+enout(i))) > 1.e-4_r8) then
               write (iulog,*) ' the enthalpy is not conserved for point ', &
                  i, enin(i), enout(i)
               write (iulog,*) ' t, q, p, enin ', t(i), q(i), p(i), enin(i)
               write (iulog,*) ' tsp, qsp, enout ', tsp(i), qsp(i), enout(i)
               call wrf_error_fatal ('FINDSP')
            endif
         end do
      endif

 ! end do                    ! level loop (k=1,pver)

   return
end subroutine findsp_water
end module module_mp_cammgmp_driver
