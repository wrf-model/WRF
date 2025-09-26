#define MODAL_AERO
MODULE module_bl_camuwpbl_driver
  !Note: Comments starting with "!!" are directly taken from CAM's interface routine for the UW PBL (vertical_diffusion.F90)
  !This modules is based on vertical_diffusion.F90 of CAM5

  !List of possible modifications in the future:
  !_____________________________________________
  !1. Constituents(per Kg of DRY air or "dry" constituents) are not diffused currently. The call which 
  !   makes DRY constituents diffusion decision flag 'true' is commented out in camuwpblinit subroutine. 
  !   Therefore the flag is ALWAYS false for DRY constituent diffusion.
  !
  !   REASON: DRY constituents are Aerosols and other species (excepts for cloud mass mixing ratios and number concentrations).
  !   ------  In WRF, DRY constituents appear only when WRF_CHEM simulations are run. DRY constituents are vertically mixed(diffused)
  !           in dry_dep_driver of WRF_CHEM. Therefore, DRY constituents are not diffused in this PBL subroutine
  !
  !2. The liquid number concentrations is NOT diffused(or mixed) in PBL to mimic CAM5, which diffuses liquid number 
  !   concentrations in dropmixnuc. 
  !3. Surface fluxes for ALL the constituents are ZERO except for the water vapours (1st constituent)
  !4. Molecular diffusion is turned off for now
  !5. Mountain stresses are not computed for now (all calls to Mountain stresses currently may hold undefined variables)
  !6. *Formulas used for computing surface stresses is based on the formula given in CAM5's BareGround.F90.
  !   This formula may not work well for ocean surfaces (possible future modification)
  !_____________________________________________


  !!----------------------------------------------------------------------------------------------------- !
  !! Module to compute vertical diffusion of momentum,  moisture, trace constituents                      !
  !! and static energy. Separate modules compute                                                          !  
  !!   1. stresses associated with turbulent flow over orography                                          !
  !!      ( turbulent mountain stress )                                                                   !
  !!   2. eddy diffusivities, including nonlocal tranport terms                                           !
  !!   3. molecular diffusivities                                                                         ! 
  !!   4. coming soon... gravity wave drag                                                                !  
  !! Lastly, a implicit diffusion solver is called, and tendencies retrieved by                           !
  !! differencing the diffused and initial states.                                                        !
  !!                                                                                                      ! 
  !! Calling sequence:                                                                                    !
  !!                                                                                                      !
  !!  vertical_diffusion_init      Initializes vertical diffustion constants and modules                  !
  !!        init_molec_diff        Initializes molecular diffusivity module                               !
  !!        init_eddy_diff         Initializes eddy diffusivity module (includes PBL)                     !  
  !!        init_tms               Initializes turbulent mountain stress module                           !
  !!        init_vdiff             Initializes diffusion solver module                                    !
  !!  vertical_diffusion_ts_init   Time step initialization (only used for upper boundary condition)      ! 
  !!  vertical_diffusion_tend      Computes vertical diffusion tendencies                                 ! 
  !!        compute_tms            Computes turbulent mountain stresses                                   !
  !!        compute_eddy_diff      Computes eddy diffusivities and countergradient terms                  !
  !!        compute_vdiff          Solves vertical diffusion equations, including molecular diffusivities !         
  !!                                                                                                      !
  !!---------------------------Code history-------------------------------------------------------------- !
  !! J. Rosinski : Jun. 1992                                                                              !
  !! J. McCaa    : Sep. 2004                                                                              !
  !! S. Park     : Aug. 2006, Dec. 2008. Jan. 2010                                                        !
  !  B. Singh    : Nov. 2010 (ported to WRF by balwinder.singh@pnl.gov)
  !!----------------------------------------------------------------------------------------------------- !

  use shr_kind_mod,       only : r8 => shr_kind_r8
  use module_cam_support, only : pcols, pver, pverp, endrun, iulog,fieldname_len,pcnst =>pcnst_runtime
  use constituents,       only : qmin
  use diffusion_solver,   only : vdiff_selector
  use physconst,          only :          &
       cpair  , &     ! Specific heat of dry air
       gravit , &     ! Acceleration due to gravity
       rair   , &     ! Gas constant for dry air
       zvir   , &     ! rh2o/rair - 1
       latvap , &     ! Latent heat of vaporization
       latice , &     ! Latent heat of fusion
       karman , &     ! von Karman constant 
       mwdry  , &     ! Molecular weight of dry air
       avogad , &     ! Avogadro's number
       boltz          ! Boltzman's constant
  
  implicit none
  private      
  save
  
  !! ----------------- !
  !! Public interfaces !
  !! ----------------- !
  
  public camuwpblinit   ! Initialization
  public camuwpbl       ! Driver for the PBL scheme
  public vd_register    ! Init routine for constituents
  
  !! ------------ !
  !! Private data !
  !! ------------ !
  
  character(len=16)    :: eddy_scheme                  !! Default set in phys_control.F90, use namelist to change
  !!     'HB'       = Holtslag and Boville (default)
  !!     'HBR'      = Holtslag and Boville and Rash 
  !!     'diag_TKE' = Bretherton and Park ( UW Moist Turbulence Scheme )
  integer, parameter   :: nturb = 5                    !! Number of iterations for solution ( when 'diag_TKE' scheme is selected )
  logical, parameter   :: wstarent = .true.            !! Use wstar (.true.) or TKE (.false.) entrainment closure ( when 'diag_TKE' scheme is selected )
  logical              :: do_pseudocon_diff = .false.  !! If .true., do pseudo-conservative variables diffusion

  
  character(len=16)    :: shallow_scheme               !! For checking compatibility between eddy diffusion and shallow convection schemes
  !!     'Hack'     = Hack Shallow Convection Scheme
  !!     'UW'       = Park and Bretherton ( UW Shallow Convection Scheme )
  character(len=16)    :: microp_scheme                !! Microphysics scheme
  
  logical              :: do_molec_diff = .false.      !! Switch for molecular diffusion
  logical              :: do_tms                       !! Switch for turbulent mountain stress
  real(r8)             :: tms_orocnst                  !! Converts from standard deviation to height
  real(r8)             :: tms_z0fac                    ! Converts from standard deviation to height
  
  type(vdiff_selector) :: fieldlist_wet                !! Logical switches for moist mixing ratio diffusion
  type(vdiff_selector) :: fieldlist_dry                !! Logical switches for dry mixing ratio diffusion
  integer              :: ntop                         !! Top interface level to which vertical diffusion is applied ( = 1 ).
  integer              :: nbot                         !! Bottom interface level to which vertical diffusion is applied ( = pver ).
  integer              :: tke_idx, kvh_idx, kvm_idx    !! TKE and eddy diffusivity indices for fields in the physics buffer
  integer              :: turbtype_idx, smaw_idx       !! Turbulence type and instability functions
  integer              :: tauresx_idx, tauresy_idx     !! Redisual stress for implicit surface stress
 
  integer              :: ixcldice, ixcldliq           !! Constituent indices for cloud liquid and ice water
  integer              :: ixnumice, ixnumliq
#ifdef MODAL_AERO
  integer              :: ixndrop
#endif
  integer              :: wgustd_index   
CONTAINS
  
  subroutine camuwpbl(dt,u_phy,v_phy,th_phy,rho,qv_curr,hfx,qfx,ustar,p8w &
       ,p_phy,z,t_phy,qc_curr,qi_curr,z_at_w,cldfra_old_mp,cldfra,ht      &
       ,rthratenlw,exner,is_CAMMGMP_used                                  &
       ,itimestep,qnc_curr,qni_curr,wsedl3d                               &
       ,ids,ide, jds,jde, kds,kde                                         &
       ,ims,ime, jms,jme, kms,kme                                         &
       ,its,ite, jts,jte, kts,kte                                         &
!Intent-inout 
       ,tauresx2d,tauresy2d                                               &
       ,rublten,rvblten,rthblten,rqiblten,rqniblten,rqvblten,rqcblten     &
       ,kvm3d,kvh3d                                                       &
!Intent-out
       ,tpert2d,qpert2d,wpert2d,smaw3d,turbtype3d                         &
       ,tke_pbl,pblh2d,kpbl2d                                             )  

    !!---------------------------------------------------- !
    !! This is an interface routine for vertical diffusion !
    !  This subroutine is called by module_pbl_driver and  !
    !  it calls: wrf_error_fatal,compute_tms,              !
    !  compute_eddy_diff,aqsat,compute_vdiff and           !
    !  positive_moisture.
    !!---------------------------------------------------- !
    use module_cam_support,    only : pcols
    use trb_mtn_stress,        only : compute_tms
    use eddy_diff,             only : compute_eddy_diff
    use wv_saturation,         only : fqsatd, aqsat
    use molec_diff,            only : compute_molec_diff, vd_lu_qdecomp
    use constituents,          only : qmincg, qmin
    use diffusion_solver !!, only : compute_vdiff, any, operator(.not.)
#ifdef MODAL_AERO
    use modal_aero_data
#endif
    
    implicit none   
    
    !------------------------------------------------------------------------!
    !                             Input                                      !
    !------------------------------------------------------------------------!
    logical, intent(in) :: is_CAMMGMP_used
    integer, intent(in) :: ids,ide, jds,jde, kds,kde
    integer, intent(in) :: ims,ime, jms,jme, kms,kme
    integer, intent(in) :: its,ite, jts,jte, kts,kte
    integer, intent(in) :: itimestep

    real, intent(in) :: dt                                                ! Time step (s)
    
    real, dimension( ims:ime,jms:jme ), intent(in) :: hfx                   !Sensible heat flux at surface (w/m2)
    real, dimension( ims:ime,jms:jme ), intent(in) :: qfx                   !Moisture      flux at surface (kg m-2 s-1)
    real, dimension( ims:ime,jms:jme ), intent(in) :: ustar                 !Friction velocity (m/s)
    real, dimension( ims:ime,jms:jme ), intent(in) :: ht                    !Terrain height (m)

    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: v_phy       !Y-component of wind (m/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: u_phy       !X-component of wind (m/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: th_phy      !Potential temperature (K)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: rho         !Air density (kg/m3)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qv_curr     !Water vapor mixing ratio - Moisture  (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qc_curr     !Cloud water mixing ratio - Cloud liq (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qi_curr     !Ice mixing ratio         - Cloud ice  (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qnc_curr    !Liq # mixing ratio       - Cloud liq #  (#/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qni_curr    !Ice # mixing ratio       - Cloud ice #  (#/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: p_phy       !Pressure at mid-level (Pa)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: p8w         !Pressure at level interface (Pa)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: z           !Height above sea level at mid-level (m) 
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: t_phy       !Temperature (K)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: z_at_w      !Height above sea level at layer interfaces (m) 
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cldfra_old_mp !Cloud fraction [unitless]
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cldfra      !Cloud fraction [unitless]
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: exner       !Dimensionless pressure [unitless]
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: rthratenlw  !Tendency for LW ( K/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: wsedl3d     !Sedimentation velocity of stratiform liquid cloud droplet (m/s)

    
    
    !------------------------------------------------------------------------!
    !                          Output                                        !
    !------------------------------------------------------------------------!   
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rublten     !Tendency for u_phy   (Pa m s-2)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rvblten     !Tendency for v_phy   (Pa m s-2)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rthblten    !Tendency for th_phy  (Pa K s-1)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rqvblten    !Tendency for qv_curr (Pa kg kg-1 s-1)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rqcblten    !Tendency for qc_curr (Pa kg kg-1 s-1)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rqiblten    !Tendency for qi_curr (Pa kg kg-1 s-1)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: rqniblten   !Tendency for qni_curr(Pa # kg-1 s-1)

    integer, dimension( ims:ime,jms:jme ), intent(out) :: kpbl2d !Layer index containing PBL top within or at the base interface
    real,    dimension( ims:ime,jms:jme ), intent(out) :: pblh2d !Planetary boundary layer height (m)

    real, dimension( ims:ime, kms:kme, jms:jme ), intent(out) :: tke_pbl     !Turbulence kinetic energy at midpoints  (m^2/s^2)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(out) :: turbtype3d  !Turbulent interface types [ no unit ]
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(out) :: smaw3d      !Normalized Galperin instability function for momentum  ( 0<= <=4.964 and 1 at neutral ) [no units]
        

    
    !---------------------------------------------------------------------------!
    !     Local, carried on from one timestep to the other (defined in registry)!
    !---------------------------------------------------------------------------!
    
    real, dimension( ims:ime, jms:jme )         , intent(inout ):: tauresx2d,tauresy2d !X AND Y-COMP OF RESIDUAL STRESSES(m^2/s^2)
    real, dimension( ims:ime, jms:jme )         , intent(out)   :: tpert2d             ! Convective temperature excess (K)
    real, dimension( ims:ime, jms:jme )         , intent(out)   :: qpert2d             ! Convective humidity excess (kg/kg)
    real, dimension( ims:ime, jms:jme )         , intent(out)   :: wpert2d             ! Turbulent velocity excess (m/s)

    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: kvm3d,kvh3d         !Eddy diffusivity for momentum and heat(m^2/s)


    !---------------------------------------------------------------------------!
    !                               Local                                       !
    !---------------------------------------------------------------------------!

    character(128) :: errstring                              ! Error status for compute_vdiff
    logical        :: kvinit                                 ! Tell compute_eddy_diff/ caleddy to initialize kvh, kvm (uses kvf)
    logical        :: is_first_step                          ! Flag to know if this a first time step
    integer        :: i,j,k,itsp1,itile_len,ktep1,kflip,ncol,ips,icnst,m,kp1
    integer        :: lchnk                                  ! Chunk identifier
    real(r8)       :: tauFac, uMean, dp, multFrc
    real(r8)       :: ztodt                                  ! 2*delta-t (s)
    real(r8)       :: rztodt                                 ! 1./ztodt (1/s)
	real(r8),parameter :: invalidVal = -999888777.0_r8

    real(r8) :: topflx(  pcols)                              ! Molecular heat flux at top interface                                                
    real(r8) :: wpert(   pcols)                              ! Turbulent velocity excess (m/s)
    real(r8) :: tauresx( pcols)                              ! [Residual stress to be added in vdiff to correct...
    real(r8) :: tauresy( pcols)                              ! for turb stress mismatch between sfc and atm accumulated.] (N/m2)
    real(r8) :: ipbl(    pcols)                              ! If 1, PBL is CL, while if 0, PBL is STL.
    real(r8) :: kpblh(   pcols)                              ! Layer index containing PBL top within or at the base interface
    real(r8) :: wstarPBL(pcols)                              ! Convective velocity within PBL (m/s)
    real(r8) :: sgh(     pcols)                              ! Standard deviation of orography (m) 
    real(r8) :: landfrac(pcols)                              ! Land fraction [unitless]
    real(r8) :: taux(    pcols)                              ! x surface stress  (N/m2)
    real(r8) :: tauy(    pcols)                              ! y surface stress  (N/m2)
    real(r8) :: tautotx( pcols)                              ! U component of total surface stress (N/m2)
    real(r8) :: tautoty( pcols)                              ! V component of total surface stress (N/m2)
    real(r8) :: ksrftms( pcols)                              ! Turbulent mountain stress surface drag coefficient (kg/s/m2)
    real(r8) :: tautmsx( pcols)                              ! U component of turbulent mountain stress (N/m2)
    real(r8) :: tautmsy( pcols)                              ! V component of turbulent mountain stress (N/m2)
    real(r8) :: ustar8(  pcols)                              ! Surface friction velocity (m/s)
    real(r8) :: pblh(    pcols)                              ! Planetary boundary layer height (m)
    real(r8) :: tpert(   pcols)                              ! Convective temperature excess (K)
    real(r8) :: qpert(   pcols)                              ! Convective humidity excess (kg/kg)
    real(r8) :: shflx(   pcols)                              ! Surface sensible heat flux  (w/m2)
    real(r8) :: phis(    pcols)                              ! Geopotential at terrain height (m2/s2)

    real(r8) :: cldn8(      pcols,kte)                       ! New stratus fraction (fraction)
    real(r8) :: qrl8(       pcols,kte)                       ! LW radiative cooling rate(W/kg*Pa)
    real(r8) :: wsedl8(     pcols,kte)                       ! Sedimentation velocity of stratiform liquid cloud droplet (m/s)
    real(r8) :: dtk(        pcols,kte)                       ! T tendency from KE dissipation
    real(r8) :: qt(         pcols,kte)                       !
    real(r8) :: sl_prePBL(  pcols,kte)                       !
    real(r8) :: qt_prePBL(  pcols,kte)                       !
    real(r8) :: slten(      pcols,kte)                       !
    real(r8) :: qtten(      pcols,kte)                       !
    real(r8) :: sl(         pcols,kte)                       !                       
    real(r8) :: ftem(       pcols,kte)                       ! Saturation vapor pressure before PBL
    real(r8) :: ftem_prePBL(pcols,kte)                       ! Saturation vapor pressure before PBL
    real(r8) :: ftem_aftPBL(pcols,kte)                       ! Saturation vapor pressure after PBL
    real(r8) :: tem2(       pcols,kte)                       ! Saturation specific humidity and RH
    real(r8) :: t_aftPBL(   pcols,kte)                       ! Temperature after PBL diffusion
    real(r8) :: tten(       pcols,kte)                       ! Temperature tendency by PBL diffusion
    real(r8) :: rhten(      pcols,kte)                       ! RH tendency by PBL diffusion
    real(r8) :: qv_aft_PBL( pcols,kte)                       ! qv after PBL diffusion
    real(r8) :: ql_aft_PBL( pcols,kte)                       ! ql after PBL diffusion
    real(r8) :: qi_aft_PBL( pcols,kte)                       ! qi after PBL diffusion
    real(r8) :: s_aft_PBL(  pcols,kte)                       ! s after PBL diffusion
    real(r8) :: u_aft_PBL(  pcols,kte)                       ! u after PBL diffusion
    real(r8) :: v_aft_PBL(  pcols,kte)                       ! v after PBL diffusion
    real(r8) :: qv_pro(     pcols,kte)                       !
    real(r8) :: ql_pro(     pcols,kte)                       !
    real(r8) :: qi_pro(     pcols,kte)                       !
    real(r8) :: s_pro(      pcols,kte)                       !
    real(r8) :: t_pro(      pcols,kte)                       !
    real(r8) :: u8(         pcols,kte)                       ! x component of velocity in CAM's data structure (m/s)
    real(r8) :: v8(         pcols,kte)                       ! y component of velocity in CAM's data structure (m/s)
    real(r8) :: t8(         pcols,kte)                       ! 
    real(r8) :: pmid8(      pcols,kte)                       ! Pressure at the midpoints in CAM's data structure (Pa)
    real(r8) :: pmiddry8(   pcols,kte)                       ! Dry Pressure at the midpoints in CAM's data structure (Pa)
    real(r8) :: zm8(        pcols,kte)                       ! Height at the midpoints in CAM's data structure  (m)
    real(r8) :: exner8(     pcols,kte)                       ! exner function in CAM's data structure
    real(r8) :: s8(         pcols,kte)                       ! Dry static energy (m2/s2)
    real(r8) :: rpdel8(     pcols,kte)                       ! Inverse of pressure difference (1/Pa)
    real(r8) :: pdel8(      pcols,kte)                       ! Pressure difference (Pa) 
    real(r8) :: rpdeldry8(  pcols,kte)                       ! Inverse of dry pressure difference (1/Pa)
    real(r8) :: pdeldry8(   pcols,kte)                       ! dry pressure difference (1/Pa)
    REAL(r8) :: stnd(       pcols,kte)                       ! Heating rate (dry static energy tendency, W/kg)
    
    real(r8) :: tke8(     pcols,kte+1)                       ! Turbulent kinetic energy [ m2/s2 ]
    real(r8) :: turbtype( pcols,kte+1)                       ! Turbulent interface types [ no unit ]
    real(r8) :: smaw(     pcols,kte+1)                       ! Normalized Galperin instability function for momentum  ( 0<= <=4.964 and 1 at neutral ) [no units]
                                                             ! This is 1 when neutral condition (Ri=0), 4.964 for maximum unstable case, and 0 when Ri > Ricrit=0.19. 
    real(r8) :: cgs(      pcols,kte+1)                       ! Counter-gradient star  [ cg/flux ]
    real(r8) :: cgh(      pcols,kte+1)                       ! Counter-gradient term for heat [ J/kg/m ]
    real(r8) :: kvh(      pcols,kte+1)                       ! Eddy diffusivity for heat [ m2/s ]
    real(r8) :: kvm(      pcols,kte+1)                       ! Eddy diffusivity for momentum [ m2/s ]
    real(r8) :: kvq(      pcols,kte+1)                       ! Eddy diffusivity for constituents [ m2/s ]
    real(r8) :: kvh_in(   pcols,kte+1)                       ! kvh from previous timestep [ m2/s ]
    real(r8) :: kvm_in(   pcols,kte+1)                       ! kvm from previous timestep [ m2/s ]
    real(r8) :: bprod(    pcols,kte+1)                       ! Buoyancy production of tke [ m2/s3 ]
    real(r8) :: sprod(    pcols,kte+1)                       ! Shear production of tke [ m2/s3 ]
    real(r8) :: sfi(      pcols,kte+1)                       ! Saturation fraction at interfaces [ fraction ]
    real(r8) :: pint8(    pcols,kte+1)                       ! Pressure at the interfaces in CAM's data structure (Pa)
    real(r8) :: pintdry8( pcols,kte+1)                       ! Dry pressure at the interfaces in CAM's data structure (Pa)
    real(r8) :: zi8(      pcols,kte+1)                       ! Height at the interfacesin CAM's data structure  (m)

    real(r8) :: cloud(     pcols,kte,pcnst)                      ! Holder for cloud water and ice (q in cam)
    real(r8) :: cloudtnd(  pcols,kte,pcnst)                      ! Holder for cloud tendencies (ptend_loc%q in cam)
    real(r8) :: wind_tends(pcols,kte,2)                      ! Wind component tendencies (m/s2)

    real(r8) :: cflx(pcols,pcnst)                            ! Surface constituent flux [ kg/m2/s ]
    
#ifdef MODAL_AERO
    real(r8) :: tmp1(pcols)                                         ! Temporary storage
    integer  :: l, lspec
#endif
    !! ----------------------- !
    !! Main Computation Begins !
    !! ----------------------- !

    do icnst = 1 , pcnst
       !Setting curface fluxes for all constituents to be zero. Later, cflx(:,1) is populated by the water vapour surface flux
       cflx(:pcols,icnst)  = 0.0_r8
    enddo

    is_first_step  = .false.
    if(itimestep == 1) then
       is_first_step = .true.
    endif
    ncol   = pcols
    ztodt  = DT
    rztodt = 1.0_r8 / ztodt

    !Few definitions in this subroutine require that ncol==1
    if(ncol .NE. 1) then
       call wrf_error_fatal('Number of CAM Columns (NCOL) in CAMUWPBL scheme must be 1')
    endif

    !Initialize errstring
    errstring = ''

    
    !-------------------------------------------------------------------------------------!
    !Map CAM variables to the corresponding WRF variables                                 !
    !Loop over the points in the tile and treat them each as a CAM Chunk                  !
    !-------------------------------------------------------------------------------------!
    itsp1     = its - 1 
    itile_len = ite - itsp1
    do j    = jts , jte
       do i = its , ite

          lchnk   = (j - jts) * itile_len + (i - itsp1)          !1-D index location from a 2-D tile
          phis(1) = ht(i,j) * gravit                             !Used for computing dry static energy

          !Flip vertically quantities computed at the mid points
          ktep1 = kte + 1
          do k  = kts,kte
             kflip               = ktep1 - k
             
             !Loop is used as vector assignment may take more computational time
             do icnst = 1 , pcnst
                !Setting cloud and cloudtnd to values (obtained from module_cam_support) which should produce errors if used 
                !1st,2nd,3rd and 5th constituents are used (see the assignments below) and diffused presently in this scheme
                cloud(1,kflip,icnst)    = invalidVal
                cloudtnd(1,kflip,icnst) = invalidVal
             enddo

             u8(      1,kflip)   = u_phy(i,k,j)  ! X-component of velocity at the mid-points (m/s) [state%u in CAM]
             v8(      1,kflip)   = v_phy(i,k,j)  ! Y-component of velocity at the mid-points (m/s) [state%v in CAM]

             pmid8(   1,kflip)   = p_phy(i,k,j)  ! Pressure     at the mid-points    (Pa)      [state%pmid    in CAM]  
             
             dp                  = p8w(i,k,j) - p8w(i,k+1,j) !Change in pressure (Pa) 
             pdel8(  1,kflip)    = dp
             rpdel8(  1,kflip)   = 1.0_r8/dp     ! Reciprocal of pressure difference (1/Pa)     [state%rpdel in CAM]
             zm8(     1,kflip)   = z(i,k,j)-ht(i,j)      ! Height above the ground at the midpoints (m) [state%zm    in CAM]
             t8(      1,kflip)   = t_phy(i,k,j)  ! Temprature at the mid points (K)             [state%t     in CAM]
             
             s8(      1,kflip)   = cpair *t8(1,kflip) + gravit*zm8(1,kflip) + phis(1) ! Dry static energy (m2/s2) [state%s in CAM]-Formula tested in vertical_diffusion.F90 of CAM
             qrl8(    1,kflip)   = rthratenlw(i,k,j)* cpair * dp ! Long Wave heating rate (W/kg*Pa)- Formula obtained from definition of qrlw(pcols,pver) in eddy_diff.F90 in CAM

             wsedl8(  1,kflip)   = wsedl3d(i,k,j)               ! Sedimentation velocity of stratiform liquid cloud droplet (m/s)
             
             !Following three formulas are obtained from ported CAM's ZM cumulus scheme
             !Values of 0 cause a crash in entropy
             multFrc                  =  1._r8/(1._r8 + qv_curr(i,k,j))
             cloud( 1,kflip,1)        = max( qv_curr(i,k,j) * multFrc, 1.0e-30_r8 ) !Specific humidity                       [state%q(:,:,1) in CAM]
             cloud( 1,kflip,ixcldliq) = qc_curr(i,k,j)  * multFrc              !Convert to moist mix ratio-cloud liquid [state%q(:,:,2) in CAM]
             cloud( 1,kflip,ixcldice) = qi_curr(i,k,j)  * multFrc              !cloud ice                               [state%q(:,:,3) in CAM]
             cloud( 1,kflip,ixnumliq) = qnc_curr(i,k,j) * multFrc 
             cloud( 1,kflip,ixnumice) = qni_curr(i,k,j) * multFrc 

             exner8(1,kflip)   = exner(i,k,j)   !Exner function (no units)
             if(is_CAMMGMP_used) then
                cldn8( 1,kflip)   = cldfra_old_mp(i,k,j)  !Cloud fraction (no unit)
             else
                cldn8( 1,kflip)   = cldfra(i,k,j)  !Cloud fraction (no unit)
             endif

             !Following formula is obtained from physics_types.F90 of CAM (CESM101)
             pdeldry8(1,kflip)    =  pdel8(1,kflip)*(1._r8-cloud(1,kflip,1))            
             rpdeldry8(1,kflip)   =  1._r8/pdeldry8(1,kflip)

          enddo
          
          do k = kts,kte+1
             kflip = kte - k + 2

             pint8(   1,kflip) = p8w(   i,k,j) ! Pressure at interfaces [state%pint in CAM]

             zi8(     1,kflip) = z_at_w(i,k,j) -ht(i,j) ! Height at interfaces [state%zi   in CAM]

             !Initialize Variables to zero, these are outputs from the "compute_eddy_diff" 
             kvq(1,kflip)   = 0.0_r8        ! Eddy diffusivity for constituents (m2/s)
             cgh(1,kflip)   = 0.0_r8        ! Counter-gradient term for heat
             cgs(1,kflip)   = 0.0_r8        ! Counter-gradient star     (cg/flux)
             if( is_first_step ) then
                kvm3d(i,k,j) = 0.0_r8     ! Eddy diffusivity for heat     (m2/s)
                kvh3d(i,k,j) = 0.0_r8     ! Eddy diffusivity for momentum (m2/s)
             endif
             kvh(1,kflip)   = kvh3d(i,k,j)  
             kvm(1,kflip)   = kvm3d(i,k,j)  
          end do

          !Compute pintdry8 and pmiddry8
          !Following formula is obtained from physics_types.F90 of CAM (CESM101)
          pintdry8(1,1) = pint8(1,1)          
          do k = 1, pver
             pintdry8(1,k+1) =  pintdry8(1,k)  + pdeldry8(1,k)
             pmiddry8(1,k)   = (pintdry8(1,k+1)+ pintdry8(1,k))*0.5_r8
          end do

          shflx(   ncol) = hfx(i,j)  ! Surface sensible heat flux (w/m2) 

          !SGH and LANDFRAC are inputs for the compute_tms subroutine. Presently set to zero as do_tms is always false for now
          sgh(     ncol) = 0.0_r8    ! Standard deviation of orography (m) 
          landfrac(ncol) = 0.0_r8    ! Fraction (unitless)                 

          uMean      = sqrt( u_phy(i,kts,j) * u_phy(i,kts,j) +  v_phy(i,kts,j) * v_phy(i,kts,j) ) ! Mean velocity
          tauFac     = rho(i,kts,j) * ustar(i,j) *ustar(i,j)/uMean

          taux(ncol) = -tauFac * u_phy(i,kts,j)  ! x surface stress (N/m2) [Formulation obtained from CAM's BareGround.F90]
          tauy(ncol) = -tauFac * v_phy(i,kts,j)  ! y surface stress (N/m2)

          !! Retrieve 'tauresx, tauresy' from from the last timestep
          if( is_first_step ) then
             tauresx2d(i,j) = 0._r8
             tauresy2d(i,j) = 0._r8
          endif
          tauresx(:ncol) = tauresx2d(i,j)
          tauresy(:ncol) = tauresy2d(i,j)
          
          !! All variables are modified by vertical diffusion
          
          !!------------------------------------------!
          !! Computation of turbulent mountain stress !
          !!------------------------------------------!
          
          !! Consistent with the computation of 'normal' drag coefficient, we are using 
          !! the raw input (u,v) to compute 'ksrftms', not the provisionally-marched 'u,v' 
          !! within the iteration loop of the PBL scheme. 

          if( do_tms ) then
             call compute_tms( pcols   , pver    , ncol  , &
                  u8      , v8      , t8       , pmid8   , & 
                  exner8  , zm8     , sgh      , ksrftms , & 
                  tautmsx , tautmsy , landfrac )
             !! Here, both 'taux, tautmsx' are explicit surface stresses.        
             !! Note that this 'tautotx, tautoty' are different from the total stress
             !! that has been actually added into the atmosphere. This is because both
             !! taux and tautmsx are fully implicitly treated within compute_vdiff.
             !! However, 'tautotx, tautoty' are not used in the actual numerical
             !! computation in this module.   
             tautotx(:ncol) = taux(:ncol) + tautmsx(:ncol)
             tautoty(:ncol) = tauy(:ncol) + tautmsy(:ncol)
          else
             ksrftms(:ncol) = 0.0_r8
             tautotx(:ncol) = taux(:ncol)
             tautoty(:ncol) = tauy(:ncol)
          endif
          
          !-------------------------------------------------------------------------------------!
          !We are currenly using just water vapour flux at the surface, rest are set to zero
          !-------------------------------------------------------------------------------------!
          cflx(:pcols,1)   = qfx(i,j) ! Surface constituent flux (kg/m2/s)
          
          !Following variables are initialized to zero, they are the output from the "compute_eddy_diff" call
          ustar8(  :pcols) = 0.0_r8   ! Surface friction velocity       (m/s)
          pblh(    :pcols) = 0.0_r8   ! Planetary boundary layer height (m  )
          ipbl(    :pcols) = 0.0_r8   ! If 1, PBL is CL, while if 0, PBL is STL.
          kpblh(   :pcols) = 0.0_r8   ! Layer index containing PBL top within or at the base interface
          wstarPBL(:pcols) = 0.0_r8   ! Convective velocity within PBL  (m/s)


          !!----------------------------------------------------------------------- !
          !!   Computation of eddy diffusivities - Select appropriate PBL scheme    !
          !!----------------------------------------------------------------------- !
          
          select case (eddy_scheme)
          case ( 'diag_TKE' ) 
             
             !! ---------------------------------------------------------------- !
             !! At first time step, have eddy_diff.F90:caleddy() use kvh=kvm=kvf !
             !! This has to be done in compute_eddy_diff after kvf is calculated !
             !! ---------------------------------------------------------------- !
             
             kvinit = .false.
             if(is_first_step) then
                kvinit = .true.
             endif
             !! ---------------------------------------------- !
             !! Get LW radiative heating out of physics buffer !
             !! ---------------------------------------------- !
             
             !! Retrieve eddy diffusivities for heat and momentum from physics buffer
             !! from last timestep ( if first timestep, has been initialized by inidat.F90 )
             
             kvm_in(:ncol,:) = kvm(:ncol,:) 
             kvh_in(:ncol,:) = kvh(:ncol,:) 
             call compute_eddy_diff( lchnk  , pcols  , pver   , ncol   , t8      , cloud(:,:,1)   , ztodt    ,           &
                  cloud(:,:,2), cloud(:,:,3), s8     , rpdel8 , cldn8  , qrl8    , wsedl8         , zm8      , zi8     , &
                  pmid8       , pint8       , u8     , v8     , taux   , tauy    , shflx          , cflx(:,1), wstarent, &
                  nturb       , ustar8      , pblh   , kvm_in , kvh_in , kvm     , kvh            , kvq      , cgh     , &                                                     
                  cgs         , tpert       , qpert  , wpert  , tke8   , bprod   , sprod          , sfi      , fqsatd  , &
                  kvinit      , tauresx     , tauresy, ksrftms, ipbl(:), kpblh(:), wstarPBL(:)    , turbtype , smaw      )

             !! ----------------------------------------------- !       
             !! Store TKE in pbuf for use by shallow convection !
             !! ----------------------------------------------- !   
             
             !! Store updated kvh, kvm in pbuf to use here on the next timestep 
             do k = kts,kte+1
                kflip          = kte - k + 2
                
                kvh3d(i,k,j)   = kvh(1,kflip)
                kvm3d(i,k,j)   = kvm(1,kflip)  
             end do
             
             
          end select
          
          !!------------------------------------ ! 
          !!    Application of diffusivities     !
          !!------------------------------------ !
          cloudtnd(  :ncol,:,:) = cloud(:ncol,:,:)
          stnd(      :ncol,:  ) = s8(   :ncol,:  )
          wind_tends(:ncol,:,1) = u8(   :ncol,:  )
          wind_tends(:ncol,:,2) = v8(   :ncol,:  )

          !!------------------------------------------------------ !
          !! Write profile output before applying diffusion scheme !
          !!------------------------------------------------------ !
          
          sl_prePBL(:ncol,:pver)  = stnd(:ncol,:pver) - latvap * cloudtnd(:ncol,:pver,ixcldliq) &
               - ( latvap + latice) * cloudtnd(:ncol,:pver,ixcldice)
          qt_prePBL(:ncol,:pver)  = cloudtnd(:ncol,:pver,1) + cloudtnd(:ncol,:pver,ixcldliq) &
               + cloudtnd(:ncol,:pver,ixcldice)

          call aqsat( t8, pmid8, tem2, ftem, pcols, ncol, pver, 1, pver )
          ftem_prePBL(:ncol,:) = cloud(:ncol,:,1)/ftem(:ncol,:)*100._r8

          !! --------------------------------------------------------------------------------- !
          !! Call the diffusivity solver and solve diffusion equation                          !
          !! The final two arguments are optional function references to                       !
          !! constituent-independent and constituent-dependent moleculuar diffusivity routines !
          !! --------------------------------------------------------------------------------- !
          
          !! Modification : We may need to output 'tautotx_im,tautoty_im' from below 'compute_vdiff' and
          !!                separately print out as diagnostic output, because these are different from
          !!                the explicit 'tautotx, tautoty' computed above. 
          !! Note that the output 'tauresx,tauresy' from below subroutines are fully implicit ones.

          if( any(fieldlist_wet) ) then
             call compute_vdiff( lchnk, pcols, pver, pcnst, ncol, pmid8, pint8, rpdel8, t8, ztodt, &
                  taux, tauy, shflx, cflx, ntop, nbot, kvh, kvm, kvq, cgs, cgh, zi8, ksrftms, qmincg, &
                  fieldlist_wet, wind_tends(:,:,1), wind_tends(:,:,2), cloudtnd, stnd, tautmsx,       &
                  tautmsy, dtk, topflx, errstring, tauresx, tauresy, 1, do_molec_diff,                &
                  compute_molec_diff, vd_lu_qdecomp)
          end if

          if( errstring .ne. '' ) then
             call wrf_error_fatal(errstring)
          endif
          
          if( any( fieldlist_dry ) ) then
             if( do_molec_diff ) then
                errstring = "Design flaw: dry vdiff not currently supported with molecular diffusion"
                call wrf_error_fatal(errstring)
             end if
             
             call compute_vdiff( lchnk, pcols, pver, pcnst, ncol, pmiddry8, pintdry8, rpdeldry8, t8, &
                  ztodt, taux, tauy, shflx, cflx, ntop, nbot, kvh, kvm, kvq, cgs, cgh, zi8, ksrftms,    &
                  qmincg, fieldlist_dry, wind_tends(:,:,1), wind_tends(:,:,2), cloudtnd, stnd, tautmsx, &
                  tautmsy, dtk, topflx, errstring, tauresx, tauresy, 1, do_molec_diff,                  &
                  compute_molec_diff, vd_lu_qdecomp)

             if( errstring .ne. '' ) call wrf_error_fatal(errstring)

          end if
          
          !! Store updated tauresx, tauresy to use here on the next timestep
          tauresx2d(i,j) = tauresx(ncol)
          tauresy2d(i,j) = tauresy(ncol)

#ifdef MODAL_AERO
          
          !! Add the explicit surface fluxes to the lowest layer
          !! Modification : I should check whether this explicit adding is consistent with
          !!                the treatment of other tracers.

          !The following code is commented out as the diffusion for the Aerosols and other species is handled by CAMMGMP and WRF_CHEM's dry_dep_driver subroutines          
          !tmp1(:ncol) = ztodt * gravit * rpdel8(:ncol,pver)
          !do m = 1, ntot_amode
          !   l = numptr_amode(m)
          !   cloudtnd(:ncol,pver,l) = cloudtnd(:ncol,pver,l) + tmp1(:ncol) * cflx(:ncol,l)
          !   do lspec = 1, nspec_amode(m)
          !      l = lmassptr_amode(lspec,m)
          !      cloudtnd(:ncol,pver,l) = cloudtnd(:ncol,pver,l) + tmp1(:ncol) * cflx(:ncol,l)
          !   enddo
          !enddo
          
#endif          
          !! -------------------------------------------------------- !
          !! Diagnostics and output writing after applying PBL scheme !
          !! -------------------------------------------------------- !
          sl(:ncol,:pver)  = stnd(:ncol,:pver) -   latvap           * cloudtnd(:ncol,:pver,ixcldliq) &
               - ( latvap + latice) * cloudtnd(:ncol,:pver,ixcldice)
          qt(:ncol,:pver)  = cloudtnd(:ncol,:pver,1) + cloudtnd(:ncol,:pver,ixcldliq) &
               + cloudtnd(:ncol,:pver,ixcldice)
          
          
          
          !! --------------------------------------------------------------- !
          !! Convert the new profiles into vertical diffusion tendencies.    !
          !! Convert KE dissipative heat change into "temperature" tendency. !
          !! --------------------------------------------------------------- !

          slten(:ncol,:)          = ( sl(:ncol,:)             - sl_prePBL(:ncol,:) )   * rztodt 
          qtten(:ncol,:)          = ( qt(:ncol,:)             - qt_prePBL(:ncol,:) )   * rztodt 
          stnd(:ncol,:)           = ( stnd(:ncol,:)           - s8(:ncol,:) )          * rztodt
          wind_tends(:ncol,:,1)   = ( wind_tends(:ncol,:,1)   - u8(:ncol,:) )          * rztodt
          wind_tends(:ncol,:,2)   = ( wind_tends(:ncol,:,2)   - v8(:ncol,:) )          * rztodt
          cloudtnd(:ncol,:pver,:) = ( cloudtnd(:ncol,:pver,:) - cloud(:ncol,:pver,:) ) * rztodt

          !! ----------------------------------------------------------- !
          !! In order to perform 'pseudo-conservative varible diffusion' !
          !! perform the following two stages:                           !
          !!                                                             !
          !! I.  Re-set (1) 'qvten' by 'qtten', and 'qlten = qiten = 0'  !
          !!            (2) 'sten'  by 'slten', and                      !
          !!            (3) 'qlten = qiten = 0'                          !
          !!                                                             !
          !! II. Apply 'positive_moisture'                               !
          !!                                                             !
          !! ----------------------------------------------------------- !
          if( eddy_scheme .eq. 'diag_TKE' .and. do_pseudocon_diff ) then
             cloudtnd(:ncol,:pver,1) = qtten(:ncol,:pver)
             stnd(:ncol,:pver)       = slten(:ncol,:pver)
             cloudtnd(:ncol,:pver,ixcldliq) = 0._r8         
             cloudtnd(:ncol,:pver,ixcldice) = 0._r8         
             cloudtnd(:ncol,:pver,ixnumliq) = 0._r8         
             cloudtnd(:ncol,:pver,ixnumice) = 0._r8         
             
             do ips = 1, ncol
                do k = 1, pver
                   qv_pro(ips,k) = cloud(ips,k,1)        + cloudtnd(ips,k,1)             * ztodt       
                   ql_pro(ips,k) = cloud(ips,k,ixcldliq) + cloudtnd(ips,k,ixcldliq)      * ztodt
                   qi_pro(ips,k) = cloud(ips,k,ixcldice) + cloudtnd(ips,k,ixcldice)      * ztodt              
                   s_pro(ips,k)  = s8(ips,k)             + stnd(ips,k)                   * ztodt
                   t_pro(ips,k)  = t8(ips,k)             + (1._r8/cpair)*stnd(ips,k) * ztodt

                end do
             end do
             call positive_moisture( cpair, latvap, latvap+latice, ncol, pver, ztodt, qmin(1), qmin(2), qmin(3),    &
                  pdel8(:ncol,pver:1:-1), qv_pro(:ncol,pver:1:-1), ql_pro(:ncol,pver:1:-1), &
                  qi_pro(:ncol,pver:1:-1), t_pro(:ncol,pver:1:-1), s_pro(:ncol,pver:1:-1),       &
                  cloudtnd(:ncol,pver:1:-1,1), cloudtnd(:ncol,pver:1:-1,ixcldliq),                 &
                  cloudtnd(:ncol,pver:1:-1,ixcldice), stnd(:ncol,pver:1:-1) )
             
          end if
          
          !! ----------------------------------------------------------------- !
          !! Re-calculate diagnostic output variables after vertical diffusion !
          !! ----------------------------------------------------------------- !
          qv_aft_PBL(:ncol,:pver)  =   cloud(:ncol,:pver,1)         + cloudtnd(:ncol,:pver,1)        * ztodt
          ql_aft_PBL(:ncol,:pver)  =   cloud(:ncol,:pver,ixcldliq)  + cloudtnd(:ncol,:pver,ixcldliq) * ztodt
          qi_aft_PBL(:ncol,:pver)  =   cloud(:ncol,:pver,ixcldice)  + cloudtnd(:ncol,:pver,ixcldice) * ztodt

          s_aft_PBL(:ncol,:pver)   =   s8(:ncol,:pver)           + stnd(:ncol,:pver)          * ztodt
          t_aftPBL(:ncol,:pver)    = ( s_aft_PBL(:ncol,:pver) - gravit*zm8(:ncol,:pver) ) / cpair 
          
          u_aft_PBL(:ncol,:pver)   =  u8(:ncol,:pver)          + wind_tends(:ncol,:pver,1)            * ztodt
          v_aft_PBL(:ncol,:pver)   =  v8(:ncol,:pver)          + wind_tends(:ncol,:pver,2)            * ztodt
          
          call aqsat( t_aftPBL, pmid8, tem2, ftem, pcols, ncol, pver, 1, pver )
          ftem_aftPBL(:ncol,:pver) = qv_aft_PBL(:ncol,:pver) / ftem(:ncol,:pver) * 100._r8
          
          tten(:ncol,:pver)        = ( t_aftPBL(:ncol,:pver)    - t8(:ncol,:pver) )              * rztodt     
          rhten(:ncol,:pver)       = ( ftem_aftPBL(:ncol,:pver) - ftem_prePBL(:ncol,:pver) )          * rztodt 


          !Post processing of the output from CAM's parameterization
          do k=kts,kte
          
             kflip = kte-k+1
             
             rublten(i,k,j)    = wind_tends(1,kflip,1)
             rvblten(i,k,j)    = wind_tends(1,kflip,2)
             rthblten(i,k,j)   = stnd(1,kflip)/cpair/exner8(1,kflip)
             
             multFrc           =  1._r8 + qv_curr(i,k,j)

             rqvblten(i,k,j)   = cloudtnd(1,kflip,1       ) * multFrc * multFrc
             rqcblten(i,k,j)   = cloudtnd(1,kflip,ixcldliq) * multFrc
             rqiblten(i,k,j)   = cloudtnd(1,kflip,ixcldice) * multFrc
             !*Important* : ixnumliq is mixed in the dropmixnuc, therefore ixnumliq is NOT mixed here (ONLY if CAMMGMP is used for mp_physics)
             rqniblten(i,k,j)  = cloudtnd(1,kflip,ixnumice) * multFrc

             !Diffusivity coefficients at the midpoints
             kp1 = k + 1
          end do

          do k = kts,kte+1
             kflip             = kte - k + 2
             
             tke_pbl(i,k,j)    = tke8(1,kflip)    !TKE at the interfaces
             turbtype3d(i,k,j) = turbtype(1,kflip)
             smaw3d(i,k,j)     = smaw(1,kflip)
          end do

          

          kpbl2d(i,j)  = kte - int(kpblh(1)) + 1  !int(kpblh(1)) 
          pblh2d(i,j)  = pblh( 1)
          tpert2d(i,j) = tpert(1)
          qpert2d(i,j) = qpert(1)
          wpert2d(i,j) = wpert(1)
          
          !End Post processing of the output from CAM
       enddo !loop of i
    enddo !loop of j
    return
  end subroutine camuwpbl




!-----------------------------------------------------------------------------------------   
  subroutine positive_moisture( cp, xlv, xls, ncol, mkx, dt, qvmin, qlmin, qimin, & 
       dp, qv, ql, qi, t, s, qvten, qlten, qiten, sten )
    !! ------------------------------------------------------------------------------- !
    !! If any 'ql < qlmin, qi < qimin, qv < qvmin' are developed in any layer,         !
    !! force them to be larger than minimum value by (1) condensating water vapor      !
    !! into liquid or ice, and (2) by transporting water vapor from the very lower     !
    !! layer. '2._r8' is multiplied to the minimum values for safety.                  !
    !! Update final state variables and tendencies associated with this correction.    !
    !! If any condensation happens, update (s,t) too.                                  !
    !! Note that (qv,ql,qi,t,s) are final state variables after applying corresponding !
    !! input tendencies.                                                               !
    !! Be careful the order of k : '1': near-surface layer, 'mkx' : top layer          ! 
    !! ------------------------------------------------------------------------------- !
!-----------------------------------------------------------------------------------------   
    implicit none
    integer,  intent(in)     :: ncol, mkx
    real(r8), intent(in)     :: cp, xlv, xls
    real(r8), intent(in)     :: dt, qvmin, qlmin, qimin
    real(r8), intent(in)     :: dp(ncol,mkx)
    real(r8), intent(inout)  :: qv(ncol,mkx), ql(ncol,mkx), qi(ncol,mkx), t(ncol,mkx), s(ncol,mkx)
    real(r8), intent(inout)  :: qvten(ncol,mkx), qlten(ncol,mkx), qiten(ncol,mkx), sten(ncol,mkx)
    integer   i, k
    real(r8)  dql, dqi, dqv, sum, aa, dum 
    
    !! Modification : I should check whether this is exactly same as the one used in
    !!                shallow convection and cloud macrophysics.
    
    do i = 1, ncol
       do k = mkx, 1, -1    ! From the top to the 1st (lowest) layer from the surface
          dql        = max(0._r8,1._r8*qlmin-ql(i,k))
          dqi        = max(0._r8,1._r8*qimin-qi(i,k))
          qlten(i,k) = qlten(i,k) +  dql/dt
          qiten(i,k) = qiten(i,k) +  dqi/dt
          qvten(i,k) = qvten(i,k) - (dql+dqi)/dt
          sten(i,k)  = sten(i,k)  + xlv * (dql/dt) + xls * (dqi/dt)
          ql(i,k)    = ql(i,k) +  dql
          qi(i,k)    = qi(i,k) +  dqi
          qv(i,k)    = qv(i,k) -  dql - dqi
          s(i,k)     = s(i,k)  +  xlv * dql + xls * dqi
          t(i,k)     = t(i,k)  + (xlv * dql + xls * dqi)/cp
          dqv        = max(0._r8,1._r8*qvmin-qv(i,k))
          qvten(i,k) = qvten(i,k) + dqv/dt
          qv(i,k)    = qv(i,k)    + dqv
          if( k .ne. 1 ) then 
             qv(i,k-1)    = qv(i,k-1)    - dqv*dp(i,k)/dp(i,k-1)
             qvten(i,k-1) = qvten(i,k-1) - dqv*dp(i,k)/dp(i,k-1)/dt
          endif
          qv(i,k) = max(qv(i,k),qvmin)
          ql(i,k) = max(ql(i,k),qlmin)
          qi(i,k) = max(qi(i,k),qimin)
       end do
       !! Extra moisture used to satisfy 'qv(i,1)=qvmin' is proportionally 
       !! extracted from all the layers that has 'qv > 2*qvmin'. This fully
       !! preserves column moisture. 
       if( dqv .gt. 1.e-20_r8 ) then
          sum = 0._r8
          do k = 1, mkx
             if( qv(i,k) .gt. 2._r8*qvmin ) sum = sum + qv(i,k)*dp(i,k)
          enddo
          aa = dqv*dp(i,1)/max(1.e-20_r8,sum)
          if( aa .lt. 0.5_r8 ) then
             do k = 1, mkx
                if( qv(i,k) .gt. 2._r8*qvmin ) then
                   dum        = aa*qv(i,k)
                   qv(i,k)    = qv(i,k) - dum
                   qvten(i,k) = qvten(i,k) - dum/dt
                endif
             enddo
          else 
             write(iulog,*) 'Full positive_moisture is impossible in vertical_diffusion'
             call wrf_message(iulog)
          endif
       endif
    end do
    return
    
  end subroutine positive_moisture
  
  
 
 !-----------------------------------------------------------------------------------------   
  subroutine camuwpblinit(rublten,rvblten,rthblten,rqvblten, &
       restart,tke_pbl,is_CAMMGMP_used,                      &
       ids,ide,jds,jde,kds,kde,                              &
       ims,ime,jms,jme,kms,kme,                              &
       its,ite,jts,jte,kts,kte)
    !!------------------------------------------------------------------!
    !! Initialization of time independent fields for vertical diffusion !
    !! Calls initialization routines for subsidiary modules             !
    !!----------------------------------------------------------------- !

    !This subroutine is based on vertical_diffusion_init of CAM. This subroutine 
    !initializes variables for vertical diffusion subroutine calls. The layout 
    !is kept similar to the original CAM subroutine to facillitate future adaptations.
    !Called by : physics_init.F
    !Calls: vd_register, cnst_get_ind, wrf_error_fatal,init_eddy_diff,init_tms,init_vdiff 
!-----------------------------------------------------------------------------------------

    use eddy_diff,              only : init_eddy_diff
    use molec_diff,             only : init_molec_diff
    use trb_mtn_stress,         only : init_tms
    use diffusion_solver,       only : init_vdiff, vdiff_select
    use constituents,           only : cnst_get_ind, cnst_get_type_byind, cnst_name
    use module_cam_support,     only : masterproc
    use module_model_constants, only : epsq2
#ifdef MODAL_AERO
    use modal_aero_data
#endif
    
    implicit none

    !-------------------------------------------------------------------------------------!
    !Input and output variables                                                           !
    !-------------------------------------------------------------------------------------!
    logical,intent(in) :: restart,is_CAMMGMP_used 
    integer,intent(in) :: ids,ide,jds,jde,kds,kde
    integer,intent(in) :: ims,ime,jms,jme,kms,kme
    integer,intent(in) :: its,ite,jts,jte,kts,kte
    
    real,dimension(ims:ime,kms:kme,jms:jme),intent(inout) :: &
         rublten, rvblten, rthblten, rqvblten
    real,dimension(ims:ime,kms:kme,jms:jme),intent(out) :: TKE_PBL

    !-------------------------------------------------------------------------------------!
    !Local Variables                                                                      ! 
    !-------------------------------------------------------------------------------------!
    integer        :: i,j,k,itf,jtf,ktf 
    integer        :: ntop_eddy         !! Top    interface level to which eddy vertical diffusion is applied ( = 1 )
    integer        :: nbot_eddy         !! Bottom interface level to which eddy vertical diffusion is applied ( = pver )
    integer        :: ntop_molec        !! Top    interface level to which molecular vertical diffusion is applied ( = 1 )
    integer        :: nbot_molec        !! Bottom interface level to which molecular vertical diffusion is applied
    character(128) :: errstring         !! Error status for init_vdiff
    real(r8)       :: hypm(kte)         !! reference state midpoint pressures
#ifdef MODAL_AERO
    integer        :: m, l
#endif

    
    jtf   = min(jte,jde-1)
    ktf   = min(kte,kde-1)
    itf   = min(ite,ide-1)
    
    !Map CAM veritcal level variables
    pver  = ktf - kts + 1
    pverp = pver + 1
    
    !Initialize flags and add constituents
    call vd_register()  
    
    !! ----------------------------------------------------------------- !
    !! Get indices of cloud liquid and ice within the constituents array !
    !! ----------------------------------------------------------------- !
    
    call cnst_get_ind( 'CLDLIQ', ixcldliq )
    call cnst_get_ind( 'CLDICE', ixcldice )
    if( microp_scheme .eq. 'MG' ) then
       call cnst_get_ind( 'NUMLIQ', ixnumliq )
       call cnst_get_ind( 'NUMICE', ixnumice )
    endif
    
    if (masterproc) then
       write(iulog,*)'Initializing vertical diffusion (vertical_diffusion_init)'
       call wrf_debug(1,iulog)
    end if
    
    !! ---------------------------------------------------------------------------------------- !
    !! Initialize molecular diffusivity module                                                  !
    !! Molecular diffusion turned on above ~60 km (50 Pa) if model top is above ~90 km (.1 Pa). !
    !! Note that computing molecular diffusivities is a trivial expense, but constituent        !
    !! diffusivities depend on their molecular weights. Decomposing the diffusion matric        !
    !! for each constituent is a needless expense unless the diffusivity is significant.        !
    !! ---------------------------------------------------------------------------------------- !
    
    ntop_molec = 1       !! Should always be 1
    nbot_molec = 0       !! Should be set below about 70 km
    
    !! ---------------------------------- !    
    !! Initialize eddy diffusivity module !
    !! ---------------------------------- !
    
    ntop_eddy  = 1       !! No reason not to make this 1, if > 1, must be <= nbot_molec
    nbot_eddy  = pver    !! Should always be pver
    
    if( masterproc ) write(iulog,fmt='(a,i3,5x,a,i3)') 'NTOP_EDDY  =', ntop_eddy, 'NBOT_EDDY  =', nbot_eddy
    call wrf_debug(1,iulog)
    
    select case ( eddy_scheme )
    case ( 'diag_TKE' ) 
       if( masterproc ) write(iulog,*) 'vertical_diffusion_init: eddy_diffusivity scheme'
       call wrf_debug(1,iulog)
       if( masterproc ) write(iulog,*) 'UW Moist Turbulence Scheme by Bretherton and Park'
       call wrf_debug(1,iulog)
       !! Check compatibility of eddy and shallow scheme
       if( shallow_scheme .ne. 'UW' ) then
          write(iulog,*) 'ERROR: shallow convection scheme ', shallow_scheme,' is incompatible with eddy scheme ', eddy_scheme
          call wrf_message(iulog)
          call wrf_error_fatal( 'convect_shallow_init: shallow_scheme and eddy_scheme are incompatible' )
       endif
       
       call init_eddy_diff( r8, pver, gravit, cpair, rair, zvir, latvap, latice, &
            ntop_eddy, nbot_eddy, hypm, karman )
       
       if( masterproc ) write(iulog,*) 'vertical_diffusion: nturb, ntop_eddy, nbot_eddy ', nturb, ntop_eddy, nbot_eddy
       call wrf_debug(1,iulog)
    end select
    
    !!-------------------------------------------------------------------------------------!
    !! The vertical diffusion solver must operate                                          !
    !! over the full range of molecular and eddy diffusion                                 !
    !!-------------------------------------------------------------------------------------!
    
    ntop = min(ntop_molec,ntop_eddy)
    nbot = max(nbot_molec,nbot_eddy)
    
    !! ------------------------------------------- !
    !! Initialize turbulent mountain stress module !
    !! ------------------------------------------- !
    
    if( do_tms ) then
       call init_tms( r8, tms_orocnst, tms_z0fac, karman, gravit, rair )
       if (masterproc) then
          write(iulog,*)'Using turbulent mountain stress module'
          call wrf_message(iulog)
          write(iulog,*)'  tms_orocnst = ',tms_orocnst
          call wrf_message(iulog)
       end if
    endif
    
    !! ---------------------------------- !
    !! Initialize diffusion solver module !
    !! ---------------------------------- !
    
    call init_vdiff( r8, pcnst, rair, gravit, fieldlist_wet, fieldlist_dry, errstring )
    if( errstring .ne. '' ) call wrf_error_fatal( errstring )
    
    !!-------------------------------------------------------------------------------------
    !! Use fieldlist_wet to select the fields which will be diffused using moist mixing ratios ( all by default )
    !! Use fieldlist_dry to select the fields which will be diffused using dry   mixing ratios.
    !!-------------------------------------------------------------------------------------
    
    if( vdiff_select( fieldlist_wet, 'u' ) .ne. '' ) call wrf_error_fatal( vdiff_select( fieldlist_wet, 'u' ) )
    if( vdiff_select( fieldlist_wet, 'v' ) .ne. '' ) call wrf_error_fatal( vdiff_select( fieldlist_wet, 'v' ) )
    if( vdiff_select( fieldlist_wet, 's' ) .ne. '' ) call wrf_error_fatal( vdiff_select( fieldlist_wet, 's' ) )

#ifdef MODAL_AERO
    !Get index for the liquid number concentration (#/kg)
    call cnst_get_ind( 'NUMLIQ', ixndrop )
#endif    

    do k = 1, pcnst
       
#ifdef MODAL_AERO
       !! Do not diffuse droplet number - treated in dropmixnuc

       !BSINGH:01/31/2013: The following if condition is modified from its original form[which is:if( k == ixndrop ) cycle]
       !to allow the PBL scheme to mix liquid number when CAMMGMP scheme is not being used

       if(is_CAMMGMP_used) then!BSINGH:01/31/2013: Modified this if cond. see comment above
          if( k == ixndrop ) cycle 
       endif
       !BSINGH-08/31/2012: In WRF, Physics init is called before chem init therefore
       !aerosol species are not yet added. Following condition, skip all other constituents
       !(which will be added in chem init) except first five (vapor, cld liq, cld ice, liq num and ice num)
       if( k > 5 ) cycle


       !The Following do-loop is commented out as Aerosols and other species are diffused in CAMMGMP and WRF_CHEM's dry_dep_driver subroutines

       !! Don't diffuse aerosol - treated in dropmixnuc
       !do m = 1, ntot_amode
       !   if( k == numptr_amode(m)   ) cycle
          !!         if( k == numptrcw_amode(m) ) go to 20
       !   do l = 1, nspec_amode(m)
       !      if( k == lmassptr_amode(l,m)   )cycle
             !!            if( k == lmassptrcw_amode(l,m) ) go to 20
       !   enddo
          !!         if( k == lwaterptr_amode(m) ) go to 20
       !enddo
#endif
       !endif
       if( cnst_get_type_byind(k) .eq. 'wet' ) then
          if( vdiff_select( fieldlist_wet, 'q', k ) .ne. '' ) call wrf_error_fatal( vdiff_select( fieldlist_wet, 'q', k ) )
       else
          !The DRY constituents are not diffused currently in PBL, they are diffused in WRF_CHEM's dry_dep_driver subroutine. Therefore the following line is commented out
          !if( vdiff_select( fieldlist_dry, 'q', k ) .ne. '' ) call wrf_error_fatal( vdiff_select( fieldlist_dry, 'q', k ) )
       endif
    enddo
    
    !Initialize the tendencies
    jtf=min0(jte,jde-1)
    ktf=min0(kte,kde-1)
    itf=min0(ite,ide-1)
    
    if(.not.restart)then
       do j = jts , jtf
          do k = kts , ktf
             do i = its , itf
                tke_pbl(i,k,j)  = epsq2
                rublten(i,k,j)  = 0.
                rvblten(i,k,j)  = 0.
                rthblten(i,k,j) = 0.
                rqvblten(i,k,j) = 0.
             enddo
          enddo
       enddo
    endif
  end subroutine camuwpblinit



!-----------------------------------------------------------------------------------------
  subroutine vd_register()
!
!This subroutine is based on the vd_register subroutine of CAM.
!-----------------------------------------------------------------------------------------
    
    !Set flags for the PBL scheme (these flags are obtained using phys_getopts in CAM) 
    microp_scheme  = 'MG'       !Used for adding constituents
    eddy_scheme    = 'diag_TKE' !Used for calling eddy scheme 

    !The following flag is deliberately set to UW for now. 
    shallow_scheme = 'UW'       !Eddy scheme is ONLY compaticle with 'UW' shallow scheme; 

    do_tms         = .false.    !To include stresses due to orography
    tms_orocnst    = 1._r8      !Orography constant
    
  end subroutine vd_register
  
end module module_bl_camuwpbl_driver

