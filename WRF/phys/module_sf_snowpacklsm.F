MODULE module_sf_snowpacklsm

use libcoupler
use funcs
!
CONTAINS
!
  SUBROUTINE snowpacklsm(inest,ITIMESTEP,XLAT,XLONG, & ! IN : Time/Space-related
                  DZ8W,       DT,       DX,            & ! IN : Model configuration 
	        IVGTYP,   ISLTYP,    & ! IN : Vegetation/Soil characteristics
		 XLAND,     XICE,XICE_THRES,  & ! IN : Vegetation/Soil characteristics
                   T3D,     QV3D,     U_PHY,    V_PHY,   SWDOWN,      GLW,  & ! IN : Forcing
		 P8W3D,PRECIP_IN,        & ! IN : Forcing
                   TSK,      HFX,      QFX,        LH,   & ! IN/OUT LSM eqv
                ALBEDO,    & ! IN/OUT LSM eqv
		  SNOW,     SNOWH,   & ! IN/OUT LSM eqv
		EMISS,     QSFC,                                 & ! IN/OUT LSM eqv
 		    Z0,      ZNT,                                           & ! IN/OUT LSM eqv
               ids,ide,  jds,jde,  kds,kde,                    &
               ims,ime,  jms,jme,  kms,kme,                    &
               its,ite,  jts,jte,  kts,kte,                    &
               in_drift, in_conc, in_csalt, in_q_lb, in_N_lb, in_bs_m_exch, &
               ice_fluxq, ice_fluxN, &
               m_budg_precip,m_budg_erosion,m_budg_sublim,m_budg_deposit,m_budg_swe,m_budg_melt,m_budg_refreeze &
               ,e_budg_lw_in,e_budg_lw_out,e_budg_sw_in,e_budg_sw_out,e_budg_sensib,e_budg_latent,e_budg_low_bc,e_budg_raine &
               ,e_budg_totale, in_glo_counter,sn_T &
               ,sn_thick,sn_volI,sn_volW,sn_volV,sn_rg,sn_rb,sn_dd,sn_sp,sn_cdot,sn_meta,sn_depd,sn_graintype,sn_mk &
               ,bs_bool,sn_nlayers, &
               snc_Z,snc_N,sn_qs,sn_ns,sfc_T,sfc_Q,sfc_RH,sfc_SQ,sfc_SN,sfc_meanR,sfc_SQ_HFX,sfc_SQ_QFX,rho_v,  &
               corr_q_for_rad,corr_N_for_rad,exch_h,bs_qi,bs_qni,in_hsalt,global_bs_bdg_div,global_bs_bdg_xch, &
               sfc_qi_drift,sfc_qni_drift,sfc_u_angle,sfc_v_angle,bs_sfc_qi_advt,bs_sfc_qni_advt,snpack_dt, &
               snpack_write_dt,NSOIL,SMOIS,TSLB,SH2O,snpack_layers_to_save,snpack_psi_s,Q2_snpack,T2_snpack, &
               U10_snpack,V10_snpack,UST_V,loc_sza,tau_qc,tau_qi,tau_qc_tot,tau_qi_tot)
!----------------------------------------------------------------
!!! ADDED FOR BLOWING SNOW ADVECTION    
    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    use module_comm_dm, only : halo_em_blowingsnow_sub
!----------------------------------------------------------------
    IMPLICIT NONE
!----------------------------------------------------------------

    INTEGER,                                         INTENT(IN   ) ::  NSOIL     ! number of soil layers
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  SMOIS     ! volumetric soil moisture [m3/m3]
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  SH2O      ! volumetric liquid soil moisture [m3/m3]
    REAL,    DIMENSION( ims:ime, 1:nsoil, jms:jme ), INTENT(INOUT) ::  TSLB      ! soil temperature [K]
    real,    dimension( ims:ime,jms:jme), intent(inout) :: Q2_snpack,T2_snpack
    real,    dimension( ims:ime,jms:jme), intent(inout) :: U10_snpack,V10_snpack

! IN only
    integer, intent(in) :: snpack_layers_to_save
    real,dimension(ims:ime,kms:kme,jms:jme),intent(in) :: rho_v
    integer, intent(in) :: inest
    integer,intent(in) :: snc_N
    real,dimension(ims:ime,snc_N,jms:jme), intent(inout) :: snc_Z,sn_ns,sn_qs
    real,dimension(ims:ime,snc_N,jms:jme), intent(inout) :: sfc_T,sfc_Q,sfc_RH
    real,dimension(ims:ime,snc_N,jms:jme), intent(inout) :: sfc_SQ,sfc_SN,sfc_meanR
    real,dimension(ims:ime,jms:jme), intent(inout) :: sfc_SQ_HFX,sfc_SQ_QFX
    real,dimension(ims:ime,jms:jme), intent(inout) :: corr_q_for_rad,corr_N_for_rad
    real,dimension(ims:ime,kms:kme,jms:jme), intent(in) :: exch_h
    real,dimension(ims:ime,kms:kme,jms:jme), intent(inout) :: bs_qi,bs_qni
    real,dimension(ims:ime,jms:jme), intent(inout) :: in_hsalt,snpack_psi_s
    real,dimension(ims:ime,jms:jme), intent(inout) :: global_bs_bdg_div
    real,dimension(ims:ime,jms:jme), intent(inout) :: global_bs_bdg_xch
    real :: A,B,C,diffu,lsub,tcond,undsat


    INTEGER,                                         INTENT(IN   ) ::  ITIMESTEP ! timestep number
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLAT      ! latitude [rad]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLONG     ! latitude [rad]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  DZ8W      ! thickness of atmo layers [m]
    REAL,                                            INTENT(IN   ) ::  DT        ! timestep [s]
    REAL,                                            INTENT(IN   ) ::  DX        ! horizontal grid spacing [m]
    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  IVGTYP    ! vegetation type
    INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  ISLTYP    ! soil type
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XLAND     ! =2 ocean; =1 land/seaice
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  XICE      ! fraction of grid that is seaice
    REAL,                                            INTENT(IN   ) ::  XICE_THRES! fraction of grid determining seaice
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  T3D       ! 3D atmospheric temperature valid at mid-levels [K]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  QV3D      ! 3D water vapor mixing ratio [kg/kg_dry]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  U_PHY     ! 3D U wind component [m/s]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  V_PHY     ! 3D V wind component [m/s]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  SWDOWN    ! solar down at surface [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  GLW       ! longwave down at surface [W m-2]
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::  P8W3D     ! 3D pressure, valid at interface [Pa]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) ::  PRECIP_IN ! total input precipitation [mm]

    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  TSK       ! surface radiative temperature [K]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  HFX       ! sensible heat flux [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QFX       ! latent heat flux [kg s-1 m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  LH        ! latent heat flux [W m-2]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ALBEDO    ! total grid albedo []
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNOW      ! snow water equivalent [mm]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  SNOWH     ! physical snow depth [m]
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  EMISS     ! surface bulk emissivity
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  QSFC      ! bulk surface specific humidity
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  Z0        ! combined z0 sent to coupled model
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ZNT       ! combined z0 sent to coupled model

    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  in_drift,in_conc,in_csalt,in_q_lb,in_N_lb,in_bs_m_exch
    REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::  ice_fluxq, ice_fluxN, UST_V

    real,    dimension( ims:ime,          jms:jme ), intent(in) :: loc_sza,tau_qc,tau_qi,tau_qc_tot,tau_qi_tot

   real, optional, dimension(ims:ime,jms:jme),intent(inout) :: m_budg_precip,m_budg_erosion,m_budg_sublim,m_budg_deposit,m_budg_swe
   real, optional, dimension(ims:ime,jms:jme),intent(inout) :: m_budg_melt,m_budg_refreeze 
   real, optional, dimension(ims:ime,jms:jme),intent(inout) :: e_budg_lw_in,e_budg_lw_out,e_budg_sw_in,e_budg_sw_out,e_budg_sensib
   real, optional, dimension(ims:ime,jms:jme),intent(inout) :: e_budg_latent,e_budg_low_bc,e_budg_raine,e_budg_totale
 
    
   integer, optional,dimension(ims:ime,jms:jme),intent(inout) :: in_glo_counter
   real,    optional, dimension(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)     :: sn_T
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_thick
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_volI
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_volW
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_volV
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_rg
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_rb
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_dd
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_sp
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_cdot
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_meta
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_depd
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_graintype
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(inout)  :: sn_mk

   integer, dimension(ims:ime,jms:jme),intent(inout) :: sn_nlayers

        INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &  ! d -> domain
         &                           ims,ime, jms,jme, kms,kme,  &  ! m -> memory
         &                           its,ite, jts,jte, kts,kte      ! t -> tile


! 1D equivalent of 2D/3D fields

! IN only

    REAL                                :: COSZ         ! cosine zenith angle
    REAL                                :: LAT          ! latitude [rad]
    REAL                                :: Z_ML         ! model height [m]
    REAL                                :: T_ML         ! temperature valid at mid-levels [K]
    REAL                                :: Q_ML         ! water vapor mixing ratio [kg/kg_dry]
    REAL                                :: U_ML         ! U wind component [m/s]
    REAL                                :: V_ML         ! V wind component [m/s]
    REAL                                :: SWDN         ! solar down at surface [W m-2]
    REAL                                :: LWDN         ! longwave down at surface [W m-2]
    REAL                                :: P_ML         ! pressure, valid at interface [Pa]
    REAL                                :: PSFC         ! surface pressure [Pa]

! INOUT (with generic LSM equivalent)

    REAL                                :: FSH          ! total sensible heat (w/m2) [+ to atm]
    REAL                                :: SALB         ! surface albedo (-)
    REAL                                :: FSNO         ! snow cover fraction (-)
    REAL                                :: SWE          ! snow water equivalent (mm)
    REAL                                :: SNDPTH       ! snow depth (m)
    REAL                                :: EMISSI       ! net surface emissivity
    REAL                                :: QSFC1D       ! bulk surface specific humidity

! OUT (with no Noah LSM equivalent)

    REAL                                :: Z0WRF        ! combined z0 sent to coupled model
    REAL                                :: T2MV         ! 2m temperature of vegetation part
    REAL                                :: T2MB         ! 2m temperature of bare ground part
    REAL                                :: Q2MV         ! 2m mixing ratio of vegetation part
    REAL                                :: Q2MB         ! 2m mixing ratio of bare ground part
    REAL                                :: TRAD         ! surface radiative temperature (k)
    REAL                                :: NEE          ! net ecosys exchange (g/m2/s CO2)
    REAL                                :: GPP          ! gross primary assimilation [g/m2/s C]
    REAL                                :: NPP          ! net primary productivity [g/m2/s C]
    REAL                                :: FVEGMP       ! greenness vegetation fraction [-]
    REAL                                :: RUNSF        ! surface runoff [mm/s]
    REAL                                :: RUNSB        ! subsurface runoff [mm/s]
    REAL                                :: ECAN         ! evaporation of intercepted water (mm/s)
    REAL                                :: ETRAN        ! transpiration rate (mm/s)
    REAL                                :: ESOIL        ! soil surface evaporation rate (mm/s]
    REAL                                :: FSA          ! total absorbed solar radiation (w/m2)
    REAL                                :: FIRA         ! total net longwave rad (w/m2) [+ to atm]
    REAL                                :: APAR         ! photosyn active energy by canopy (w/m2)
    REAL                                :: PSN          ! total photosynthesis (umol co2/m2/s) [+]
    REAL                                :: SAV          ! solar rad absorbed by veg. (w/m2)
    REAL                                :: SAG          ! solar rad absorbed by ground (w/m2)
    REAL                                :: RSSUN        ! sunlit leaf stomatal resistance (s/m)
    REAL                                :: RSSHA        ! shaded leaf stomatal resistance (s/m)
    REAL, DIMENSION(1:2)                :: ALBSND       ! snow albedo (direct)
    REAL, DIMENSION(1:2)                :: ALBSNI       ! snow albedo (diffuse)
    REAL                                :: RB           ! leaf boundary layer resistance (s/m)
    REAL                                :: LAISUN       ! sunlit leaf area index (m2/m2)
    REAL                                :: LAISHA       ! shaded leaf area index (m2/m2)
    REAL                                :: BGAP         ! between gap fraction
    REAL                                :: WGAP         ! within gap fraction
    REAL                                :: TGV          ! under canopy ground temperature[K]
    REAL                                :: TGB          ! bare ground temperature [K]
    REAL                                :: CHV          ! sensible heat exchange coefficient vegetated
    REAL                                :: CHB          ! sensible heat exchange coefficient bare-ground
    REAL                                :: IRC          ! canopy net LW rad. [w/m2] [+ to atm]
    REAL                                :: IRG          ! veg ground net LW rad. [w/m2] [+ to atm]
    REAL                                :: SHC          ! canopy sen. heat [w/m2]   [+ to atm]
    REAL                                :: SHG          ! veg ground sen. heat [w/m2]   [+ to atm]
    REAL                                :: EVG          ! veg ground evap. heat [w/m2]  [+ to atm]
    REAL                                :: GHV          ! veg ground heat flux [w/m2]  [+ to soil]
    REAL                                :: IRB          ! bare net longwave rad. [w/m2] [+ to atm]
    REAL                                :: SHB          ! bare sensible heat [w/m2]     [+ to atm]
    REAL                                :: EVB          ! bare evaporation heat [w/m2]  [+ to atm]
    REAL                                :: GHB          ! bare ground heat flux [w/m2] [+ to soil]
    REAL                                :: TR           ! transpiration [w/m2]  [+ to atm]
    REAL                                :: EVC          ! canopy evaporation heat [w/m2]  [+ to atm]
    REAL                                :: CHLEAF       ! leaf exchange coefficient 
    REAL                                :: CHUC         ! under canopy exchange coefficient 
    REAL                                :: CHV2         ! veg 2m exchange coefficient 
    REAL                                :: CHB2         ! bare 2m exchange coefficient 
  REAL   :: PAHV    !precipitation advected heat - vegetation net (W/m2)
  REAL   :: PAHG    !precipitation advected heat - under canopy net (W/m2)
  REAL   :: PAHB    !precipitation advected heat - bare ground net (W/m2)
  REAL   :: PAH     !precipitation advected heat - total (W/m2)

! Local terms

    REAL                                :: DZ8W1D       ! model level heights for MYJ [not used]

    INTEGER                             :: I
    INTEGER                             :: J
    INTEGER                             :: K

    double precision :: in_H,in_TA,in_RH,in_VW,in_VW_MAX,in_DW,in_ISWR,in_ILWR,in_PSUM,in_QI,in_QNI,in_UST
    real :: in_ES,in_Qsat
    double precision :: l_in_drift,l_in_conc,l_in_csalt,l_in_q_lb,l_in_N_lb,l_in_q_corr,l_in_N_corr
    double precision :: out_TSK,out_HFX,out_QFX,out_LH,out_ALBEDO,out_SNOWC,out_SNOW,out_SNOWH,out_QSFC
    double precision :: log_z_z0
    double precision :: in_SFP
    double precision :: l_mbug_precip,l_mbug_erosion,l_mbug_sublim,l_mbug_deposit,l_mbug_swe
    double precision :: l_mbug_melt,l_mbug_refreeze
    double precision :: l_ebug_lw_in,l_ebug_lw_out,l_ebug_sw_in,l_ebug_sw_out,l_ebug_sens,l_ebug_lat
    double precision :: l_ebug_low_bc,l_ebug_raine,l_ebug_totale

    real :: add_to_top_qi,add_to_top_ni
    integer :: l_sn_nlayers
    double precision, dimension(snpack_layers_to_save) :: arr_T

    double precision, dimension(snpack_layers_to_save) :: arr_thick
    double precision, dimension(snpack_layers_to_save) :: arr_volI
    double precision, dimension(snpack_layers_to_save) :: arr_volW
    double precision, dimension(snpack_layers_to_save) :: arr_volV
    double precision, dimension(snpack_layers_to_save) :: arr_rg

    double precision, dimension(snpack_layers_to_save) :: arr_rb
    double precision, dimension(snpack_layers_to_save) :: arr_dd
    double precision, dimension(snpack_layers_to_save) :: arr_sp
    double precision, dimension(snpack_layers_to_save) :: arr_cdot
    double precision, dimension(snpack_layers_to_save) :: arr_meta
    double precision, dimension(snpack_layers_to_save) :: arr_depd
    double precision, dimension(snpack_layers_to_save) :: arr_graintype
    double precision, dimension(snpack_layers_to_save) :: arr_mk

    double precision :: l_rho_v
    logical,intent(in) :: bs_bool
    integer :: counter_snowpoints,second_counter    
    real,parameter :: pi_v = 3.14159265
    logical :: tmp_bs_bool
    real :: dyn_vis
    real :: delZ_b,delZ_m
    integer :: N_can
    real,dimension(snc_N) :: z_loc,qi_loc,Ni_loc,lambda,VN,VQ,g,del_z_loc
    real,dimension(snc_N) :: mat_a,mat_b,mat_c,mat_d
    real,dimension(snc_N) :: qi_final,ni_final
    real,dimension(snc_N) :: loc_sfc_T,loc_sfc_Q,loc_sfc_RH,loc_sfc_SQ,loc_sfc_SN
    real,dimension(snc_N) :: loc_sfc_meanR
    real,dimension(snc_N) :: loc_sfc_SQ_HFX
    real,dimension(snc_N) :: weight_h
    real :: weight_larger
    real :: z1,z2,z3,K1,K2,K_alpha
    real,parameter :: kappa = 0.4
    real :: ust
    real :: a_loc,b_loc
    real :: mean_NBest,eta,N_top,N_salt,q_salt,del_time,del_t,Ni_top,qi_top,g_top
    real, parameter :: alpha = 3.0
    real :: h_salt,lambda_top,VN_top,VQ_top
    real :: ici_TSK,ici_Q
    real, parameter :: Rv = 461.5
    real :: tmp_SQ,tmp_SN
    real :: total_sfc_SQ_HFX,total_sfc_SQ_QFX
    real,dimension(snc_N+kte-kts+1) :: bs_Z,bs_Kmix
    real,dimension(1:snc_N+1) :: bs_zm_sfclayer
    real,dimension(kts:kte) :: tmp_bs_z
    real,dimension(snc_N+(kte-kts)+1) :: bs_qi_loc,bs_qni_loc
    integer :: bs_Ntot
    double precision:: bs_bdg_precip,bs_sus_mass_xchange,l_bs_bdg_total,qi_in,qni_in,bs_K,bs_mass_turb
    double precision, dimension(ims:ime,jms:jme) :: bs_bdg_total
    double precision:: bs_bdg_div,bs_number_turb
    real :: u_right,u_left,u_up,u_down
    real :: v_right,v_left,v_up,v_down
    real :: vw_right,vw_left,vw_up,vw_down
    real :: divQx, divQy,tmp_divQ
    double precision :: l_in_hsalt
    double precision :: l_psi_s
    double precision :: l_loc_sza, l_tau_qc, l_tau_qi
    double precision :: l_tau_qc_tot, l_tau_qi_tot
    real, dimension(ims:ime,snc_N,jms:jme)  :: U_SFC,V_SFC,VW_SFC
    real, dimension(ims:ime,jms:jme),intent(inout) :: sfc_u_angle,sfc_v_angle
    real, dimension(ims:ime,snc_N,jms:jme),intent(inout) :: sfc_qi_drift,sfc_qni_drift
    real, dimension(ims:ime,snc_N,jms:jme),intent(inout) :: bs_sfc_qi_advt,bs_sfc_qni_advt
    real, dimension(snc_N) :: d_sn_qi_dt,d_sn_qni_dt

    real,intent(in) :: snpack_dt,snpack_write_dt
    real :: prof_const_a,prof_const_b
    integer :: c_x,c_y,iii,jjj,ccci,cccj
    real :: min_d,d

    character*256 :: snpack_mode
    call nl_get_snpack_mode  (1, snpack_mode)

! ----------------------------------------------------------------------


    in_q_lb = 0.0
    in_N_lb = 0.0
    in_hsalt = 0.0
    snpack_psi_s = 0.0
    sfc_T = 0.0
    sfc_Q = 0.0
do j = jts,jte
do i = its,ite


!!! MASS BALANCE OF SALTATION

! FIRST: CALCULATE Qsalt Divergence  


u_right = U_PHY(I+1,1,J)
u_left  = U_PHY(I-1,1,J)
u_up    = U_PHY(I,1,J+1)
u_down  = U_PHY(I,1,J-1)

v_right = V_PHY(I+1,1,J)
v_left  = V_PHY(I-1,1,J)
v_up    = V_PHY(I,1,J+1)
v_down  = V_PHY(I,1,J-1)

vw_right = sqrt(u_right*u_right + v_right*v_right)
vw_left  = sqrt(u_left*u_left + v_left*v_left)
vw_up    = sqrt(u_up*u_up + v_up*v_up)
vw_down  = sqrt(u_down*u_down + v_down*v_down)

U_ML = U_PHY(I,1,J)
V_ML = V_PHY(I,1,J)
in_VW = sqrt(U_ML*U_ML + V_ML*V_ML)

if( (vw_left .eq. 0) .or. (vw_right .eq. 0) .or. (vw_up .eq. 0) .or. (vw_down .eq. 0) ) then
    divQx = 0.
    divQy = 0.
else
    divQx = (max(in_drift(I-1,J)*u_left/vw_left,0.0) + abs(min(in_drift(I+1,J)*u_right/vw_right,0.0)) - in_drift(I,J)*abs(U_ML)/in_VW   ) / DX
    divQy = (max(in_drift(I,J-1)*v_up/vw_up,0.0) + abs(min(in_drift(I,J+1)*v_down/vw_down,0.0)) - in_drift(I,J)*abs(V_ML)/in_VW ) / DX
endif

tmp_divQ = sign(max(1e-12,abs(divQx+divQy)),divQx+divQy)

bs_bdg_div = real( tmp_divQ * DT, 8)

! SECOND: PRECIP is clear
bs_bdg_precip = real(PRECIP_IN(I,J),8)
! THIRD: is mass exchange based on net effect of sedimentation and turbulent 
!        exchange between saltation and bottom of the suspension layer 
bs_sus_mass_xchange = real(in_bs_m_exch(I,J),8)  
!!! NOTE in_bs_m_exch is +ve when mass is being deposited
!        in_bs_m_exch is -ve when mass is being eroded
 

! total budg 

if( snpack_mode == 'land' ) then
        bs_bdg_div = 0.0
endif

bs_bdg_total(I,J) = bs_bdg_div + bs_sus_mass_xchange  

global_bs_bdg_div(I,J) = global_bs_bdg_div(I,J) + real(bs_bdg_div,4)
global_bs_bdg_xch(I,J) = global_bs_bdg_xch(I,J) + real(bs_sus_mass_xchange,4)

enddo ! END I LOOP
enddo ! END J LOOP

!!!! QUIRKY ADVECTION FOR sfc layer
!!
do j=jts,jte
do i=its,ite

    d_sn_qi_dt  = 0.
    d_sn_qni_dt = 0.

do k=1,snc_N
    d_sn_qi_dt(k) = (max(sfc_qi_drift(I-1,k,J)*sfc_u_angle(I-1,J)     ,0.0)        + &
&                 max(sfc_qi_drift(I,k,J-1)*sfc_v_angle(I,J-1)     ,0.0)        + &
&                 abs(min(sfc_qi_drift(I+1,k,J)*sfc_u_angle(I+1,J) ,0.0))       + &
&                 abs(min(sfc_qi_drift(I,k,J+1)*sfc_v_angle(I,J+1) ,0.0))       - &
&                 sfc_qi_drift(I,k,J)*abs(sfc_u_angle(I,J))                     - &
&                 sfc_qi_drift(I,k,J)*abs(sfc_v_angle(I,J))          ) * (DT / DX )

   d_sn_qni_dt(k) = (max(sfc_qni_drift(I-1,k,J)*sfc_u_angle(I-1,J)     ,0.0)        + &
&                 max(sfc_qni_drift(I,k,J-1)*sfc_v_angle(I,J-1)     ,0.0)        + &
&                 abs(min(sfc_qni_drift(I+1,k,J)*sfc_u_angle(I+1,J) ,0.0))       + &
&                 abs(min(sfc_qni_drift(I,k,J+1)*sfc_v_angle(I,J+1) ,0.0))       - &
&                 sfc_qni_drift(I,k,J)*abs(sfc_u_angle(I,J))                     - &
&                 sfc_qni_drift(I,k,J)*abs(sfc_v_angle(I,J))          ) * (DT / DX )



d_sn_qi_dt(k) = sign(max(1.e-10,abs(d_sn_qi_dt(k))),d_sn_qi_dt(k))
d_sn_qni_dt(k) = sign(max(1.e-5,abs(d_sn_qni_dt(k))),d_sn_qni_dt(k))

if( snpack_mode == 'land' ) then
         d_sn_qi_dt(k) = 0.0
        d_sn_qni_dt(k) = 0.0
endif




bs_sfc_qi_advt(I,k,J)  = d_sn_qi_dt(k) / rho_v(I,k,J)
bs_sfc_qni_advt(I,k,J) = d_sn_qni_dt(k) / rho_v(I,k,J)

sn_qs(I,k,J) = sn_qs(I,k,J) + d_sn_qi_dt(k) / rho_v(I,k,J)
sn_ns(I,k,J) = sn_ns(I,k,J) + d_sn_qni_dt(k) / rho_v(I,k,J)
enddo

enddo
enddo 


!! GET U_SFC and V_SFC , i.e., getting surface layer wind speeds
do j=jts,jte
do i=its,ite

     Z_ML = 0.5*dz8w(I,1,J)
do k=1,snc_N
     U_SFC(i,k,j) = (U_PHY(I,1,J)/Z_ML) * snc_Z(I,k,J)
     V_SFC(i,k,j) = (V_PHY(I,1,J)/Z_ML) * snc_Z(I,k,J)
enddo
    

enddo
enddo

do j=jts,jte
do i=its,ite
do k=1,snc_N
VW_SFC(i,k,j) = sqrt(U_SFC(i,k,j)*U_SFC(i,k,j) + V_SFC(i,k,j)*V_SFC(i,k,j))
enddo
enddo
enddo

do j=jts,jte
do i=its,ite

if(VW_SFC(I,1,J) .ge. 1e-12) then
sfc_u_angle(I,J) = U_SFC(I,1,J)/VW_SFC(I,1,J)
sfc_v_angle(I,J) = V_SFC(I,1,J)/VW_SFC(I,1,J)
else
sfc_u_angle(I,J) = 0.
sfc_v_angle(I,J) = 0.
endif

enddo
enddo

do k=1,snc_N
   sfc_qi_drift(its:ite,k,jts:jte)  = VW_SFC(its:ite,k,jts:jte) * sn_qs(its:ite,k,jts:jte) * rho_v(its:ite,kms,jts:jte)  
   sfc_qni_drift(its:ite,k,jts:jte) = VW_SFC(its:ite,k,jts:jte) * sn_ns(its:ite,k,jts:jte) * rho_v(its:ite,kms,jts:jte)
enddo


if( (snpack_mode == 'land') .and. (SNOWH(I,J) .le. 0.2)  ) then

      bs_bdg_total(I,J) = 0.0       

endif

!!!!!!!!!! END OF BLOWING SNOW SURFACE MASS BALANCE and SFC LAYER ADVECTION

    second_counter = 0
    counter_snowpoints = 0
    JLOOP : DO J=jts,jte

       IF(ITIMESTEP == 1)THEN
          DO I=its,ite
             IF((XLAND(I,J)-1.5) >= 0.) THEN    ! Open water case
                IF(XICE(I,J) == 1.) PRINT *,' sea-ice at water point, I=',I,'J=',J
                DO K = 1,NSOIL
                   SMOIS(I,K,J) = 1.0
                    TSLB(I,K,J) = 273.16
                ENDDO
             ELSE
                IF(XICE(I,J) == 1.) THEN        ! Sea-ice case
                   DO K = 1,NSOIL
                     SMOIS(I,K,J) = 1.0
                   ENDDO
                ENDIF
             ENDIF
          ENDDO
       ENDIF                                                               ! end of initialization over ocean


!-----------------------------------------------------------------------
   ILOOP : DO I = its, ite


    l_mbug_deposit = bs_bdg_total(I,J)

    IF (XICE(I,J) >= XICE_THRES) THEN

       SH2O (i,1:NSOIL,j) = 1.0
       m_budg_deposit(I,J) =    m_budg_deposit(I,J)  +  real(l_mbug_deposit   ,4)

       CYCLE ILOOP ! Skip any processing at sea-ice points

    ELSE
       !!!!!!!! HERE WE CHECK IF IT IS OPEN WATER OR NOT
       IF((XLAND(I,J)-1.5) >= 0.) then
           m_budg_deposit(I,J) =    m_budg_deposit(I,J)  +  real(l_mbug_deposit   ,4)
           CYCLE ILOOP   ! Open water case
       ENDIF
!     2D to 1D       

! IN only
       second_counter = second_counter + 1

       Z_ML   = 0.5*DZ8W(I,1,J)                       ! DZ8W: thickness of full levels; ZLVL forcing height [m]
       T_ML   = T3D(I,1,J)                            ! temperature defined at intermediate level [K]
       Q_ML   = QV3D(I,1,J)/(1.0+QV3D(I,1,J))         ! convert from mixing ratio to specific humidity [kg/kg]
       U_ML   = U_PHY(I,1,J)                          ! u-wind at interface [m/s]
       V_ML   = V_PHY(I,1,J)                          ! v-wind at interface [m/s]i

       SWDN   = SWDOWN(I,J)                           ! shortwave down from SW scheme [W/m2]
       LWDN   = GLW(I,J)                              ! total longwave down from LW scheme [W/m2]
       P_ML   =(P8W3D(I,KTS+1,J)+P8W3D(I,KTS,J))*0.5  ! surface pressure defined at intermediate level [Pa]
	                                              !    consistent with temperature, mixing ratio
       PSFC   = P8W3D(I,1,J)                          ! surface pressure defined a full levels [Pa]

! IN/OUT fields

       SWE                   = SNOW    (I,J)                ! snow water equivalent [mm]
       SNDPTH                = SNOWH   (I,J)                ! snow depth [m]
       QSFC1D                = QSFC    (I,J)

! INOUT (with no Noah LSM equivalent)

       l_in_drift            = real(in_drift(I,J) , 8)
       l_in_conc             = real(in_conc(I,J)  , 8)
       l_in_csalt            = real(in_csalt(I,J) , 8)
       l_in_q_lb             = real(in_q_lb(I,J)  , 8)
       l_in_N_lb             = real(in_N_lb(I,J)  , 8)

       l_in_q_corr           = real(ice_fluxq(I,J),8)
       l_in_N_corr           = real(ice_fluxN(I,J),8) 


       l_loc_sza = real(loc_sza(I,J),8)
       l_tau_qc = real(tau_qc(I,J),8)
       l_tau_qi = real(tau_qi(I,J),8)
       l_tau_qc_tot = real(tau_qc_tot(I,J),8)
       l_tau_qi_tot = real(tau_qi_tot(I,J),8)


       DZ8W1D = DZ8W (I,1,J)                          ! thickness of atmospheric layers

      !!!! snpack-ANTARCTICA: HERE THE CODE SPLITS BETWEEN LAND WITH ICE (isltyp==8) AND LAND WITHOUT ICE
      !!!! snpack-LAND : no if block here
      IF( ((snpack_mode == 'antarctica') .and. ISLTYP(I,J) == 8) .or. &
&          ( snpack_mode == 'land' ) ) then


         in_H   = real(Z_ML,8)    ! (1) 
         in_TA  = real(T_ML,8)    ! (2)

         call Qsat_noah(T_ML,P_ML,in_ES,in_Qsat)

         in_RH  = real(Q_ML,8)/real(in_Qsat,8)
         in_RH  = min(1.0,max(in_RH,0.0))            !(5)

         in_VW  = SQRT(real(U_ML * U_ML + V_ML * V_ML,8))    !(6)

         in_VW_MAX = in_VW                           !(7)
         in_DW = 0.0                                 !(8) 
         in_ISWR = real(SWDN,8)                      !(9)
         in_ILWR = real(LWDN,8)                      !(10)
         in_PSUM = real(PRECIP_IN(I,J),8)            !(11)

         out_TSK = 0.0 
         out_HFX = 0.0
         out_QFX = 0.0 
         out_LH  = 0.0
         out_ALBEDO = 0.0 
         out_SNOWC  = 0.0
         out_SNOW   = 0.0 
         out_SNOWH  = 0.0 

         if (snpack_mode == 'antarctica') then 
            log_z_z0 = log(real(Z_ML,8)/real(0.002,8))
         else
            log_z_z0 = log(real(Z_ML,8)/real(0.002,8))
         endif

         in_UST = real(UST_V(I,J),8)
         in_SFP = real(PSFC,8)


         l_mbug_precip   = 0.0
         l_mbug_erosion  = 0.0
         l_mbug_sublim   = 0.0
         l_mbug_deposit  = 0.0
         l_mbug_swe      = 0.0
         l_mbug_melt     = 0.0
         l_mbug_refreeze = 0.0

         l_ebug_lw_in    = 0.0
         l_ebug_lw_out   = 0.0
         l_ebug_sw_in    = 0.0
         l_ebug_sw_out   = 0.0
         l_ebug_sens     = 0.0
         l_ebug_lat      = 0.0
         l_ebug_low_bc   = 0.0
         l_ebug_raine    = 0.0
         l_ebug_totale   = 0.0

         arr_T     = -999.0 
         arr_thick = -999.0 
         arr_volI  = -999.0 
         arr_volW  = -999.0 
         arr_volV  = -999.0 
         arr_rg    = -999.0 

         arr_rb    = -999.0 
         arr_dd    = -999.0 
         arr_sp    = -999.0 
         arr_cdot  = -999.0 
         arr_meta  = -999.0 
         arr_depd  = -999.0 
         arr_graintype = -999.0
         arr_mk    = -999.0

         counter_snowpoints = in_glo_counter(I,J)
         l_sn_nlayers = 0
         l_rho_v = real(rho_v(I,kms,J),8)
         l_bs_bdg_total = bs_bdg_total(I,J)
 
         tmp_bs_bool = bs_bool

         if( (SNDPTH .le. 0.2) .and. (snpack_mode == 'land') ) then
                 tmp_bs_bool = .false.
                 l_bs_bdg_total = 0.0
         endif

!         write(*,*) I,J,in_ISWR

 
               call run_code(inest,counter_snowpoints,snpack_layers_to_save,in_H  , in_TA,       &
  &                          in_RH             ,in_VW       , in_VW_MAX,   &
  &                          in_DW             ,l_in_drift  , l_in_conc,   &
  &                          l_in_csalt        ,l_in_q_lb   , l_in_N_lb,   &
  &                          in_ISWR           ,in_ILWR     , in_PSUM,     &
  &                          out_TSK           ,out_HFX     , out_QFX,     &
  &                          out_ALBEDO        ,out_SNOWC   , out_SNOW,    &
  &                          out_SNOWH         ,l_in_q_corr , l_in_N_corr, &
  &                          log_z_z0          ,in_QI       , in_QNI, in_UST, I, J, in_SFP, &
  &                          l_mbug_precip,l_mbug_erosion,l_mbug_sublim,l_mbug_deposit,l_mbug_swe, &
  &                          l_mbug_melt,l_mbug_refreeze, &
  &                          l_ebug_lw_in,l_ebug_lw_out,l_ebug_sw_in,l_ebug_sw_out,l_ebug_sens,l_ebug_lat, &
  &                          l_ebug_low_bc,l_ebug_raine,l_ebug_totale,arr_T,arr_thick,arr_volI,arr_volW, &
  &                          arr_volV,arr_rg,arr_rb,arr_dd,arr_sp,arr_cdot,arr_meta,arr_depd,arr_graintype,arr_mk,tmp_bs_bool, &
  &                          l_sn_nlayers,l_rho_v, l_bs_bdg_total, qi_in, qni_in,bs_K, bs_mass_turb,    &
  &                          bs_number_turb,l_in_hsalt,l_psi_s, l_loc_sza,l_tau_qc,l_tau_qi, &
  &                          l_tau_qc_tot,l_tau_qi_tot )

    UST_V(I,J)    = real(in_UST,4)
    in_hsalt(I,J) = real(l_in_hsalt,4)
    snpack_psi_s(I,J) = real(l_psi_s,4)

    total_sfc_SQ_QFX = sfc_SQ_QFX(I,J)
    total_sfc_SQ_HFX = sfc_SQ_HFX(I,J)

    if(out_SNOWH .le. 0.2) then
            total_sfc_SQ_QFX = 0.0
            total_sfc_SQ_HFX = 0.0
    endif

    out_HFX = out_HFX + real(total_sfc_SQ_HFX,8)

    sn_nlayers(I,J)     = l_sn_nlayers

    if( mod(itimestep,int(snpack_write_dt/dt)) .eq. 0) then
        sn_T(I,:,J)     = real(arr_T,4)
        sn_thick(I,:,J) = real(arr_thick,4)
        sn_volI(I,:,J)  = real(arr_volI,4)
        sn_volW(I,:,J)  = real(arr_volW,4)
        sn_volV(I,:,J)  = real(arr_volV,4)
        sn_rg(I,:,J)    = real(arr_rg,4)
        
        sn_rb(I,:,J)   = real(arr_rb,4)
        sn_dd(I,:,J)   = real(arr_dd,4)
        sn_sp(I,:,J)   = real(arr_sp,4)
        sn_cdot(I,:,J) = real(arr_cdot,4)
        sn_meta(I,:,J) = real(arr_meta,4)
        sn_depd(I,:,J) = real(arr_depd,4)
        sn_graintype(I,:,J) = real(arr_graintype,4)
        sn_mk(I,:,J) = real(arr_mk,4)
    endif

    m_budg_precip(I,J)  =    m_budg_precip(I,J)   +  real(l_mbug_precip   ,4)
    m_budg_erosion(I,J) =    m_budg_erosion(I,J)  +  real(l_mbug_erosion   ,4)
    m_budg_sublim(I,J)  =    m_budg_sublim(I,J)   +  real(l_mbug_sublim    ,4)

! m_budg_deposit is added up further below
!    m_budg_deposit(I,J) =    m_budg_deposit(I,J)  +  real(l_mbug_deposit   ,4)
!!!
    m_budg_swe(I,J)     =    m_budg_swe(I,J)      +  real(l_mbug_swe   ,4)
    m_budg_melt(I,J)    =    m_budg_melt(I,J)     +  real(l_mbug_melt,  4)
    m_budg_refreeze(I,J)=    m_budg_refreeze(I,J) +  real(l_mbug_refreeze,4)   


    e_budg_lw_in(I,J)   =    e_budg_lw_in(I,J)    +  real(l_ebug_lw_in     ,4)
    e_budg_lw_out(I,J)  =    e_budg_lw_out(I,J)   +  real(l_ebug_lw_out    ,4)
    e_budg_sw_in(I,J)   =    e_budg_sw_in(I,J)    +  real(l_ebug_sw_in     ,4)
    e_budg_sw_out(I,J)  =    e_budg_sw_out(I,J)   +  real(l_ebug_sw_out    ,4)
    e_budg_sensib(I,J)  =    e_budg_sensib(I,J)   +  real(l_ebug_sens    ,4)
    e_budg_latent(I,J)  =    e_budg_latent(I,J)   +  real(l_ebug_lat    ,4)
    e_budg_low_bc(I,J)  =    e_budg_low_bc(I,J)   +  real(l_ebug_low_bc  ,4)
    e_budg_raine(I,J)   =    e_budg_raine(I,J)    +  real(l_ebug_raine   ,4)
    e_budg_totale(I,J)  =    e_budg_totale(I,J)   +  real(l_ebug_totale  ,4)


    TSK(I,J)            =  real(out_TSK,4)

    call Qsat_noah(real(out_TSK,4),PSFC,in_ES,in_Qsat)
    out_QSFC = real(in_Qsat,8)

    if ( out_SNOWH .gt. 0) then
      FSNO   = 1.0
    else
      FSNO   = 0.0
    endif
    
    if ( out_SNOWH .gt. 0 ) then
         if (snpack_mode == 'antarctica') then 
            Z0WRF = 0.002! log(real(Z_ML,8)/real(0.002,8))
         else
            Z0WRF = 0.002 ! log(real(Z_ML,8)/real(0.2,8))
         endif
    else
      Z0WRF = Z0(I,J)
    endif
    
    TGB    = 0.0 
    FVEGMP = 0.0 
    IRB    = 0.0 !FIRA
    SHB    = 0.0
    
    !Z0WRF  = 0.002

  ! FROM SNOWPACK
    TRAD = real(out_TSK,4)
    FSH  = real(out_HFX,4)
    LH (I,J) = real(out_QFX,4)
    if(out_TSK > 273.15) then
       QFX (I,J) = real(out_QFX/real(2.5104E06,8) ,4)   + total_sfc_SQ_QFX
    else
       QFX (I,J) = real(out_QFX/real(2.8440E06,8) ,4) + total_sfc_SQ_QFX
    endif
  ! FROM SNOWPACK
    SWE    = real(out_SNOW,4)
    SNDPTH = real(out_SNOWH,4)
    QSFC1D = real(out_QSFC,4)
    EMISSI = 0.98
    SALB   = real(out_ALBEDO,4)

    prof_const_a = (out_HFX /(l_rho_v*1004.67*IN_UST*0.4))
    prof_const_b = (QFX(I,J)/(l_rho_v*IN_UST*0.4)) 
  ! compute T2 and Q2



    T2_snpack(I,J) = real(out_TSK  - prof_const_a*(max((log(2.0/Z0WRF)-real(l_psi_s,4)),0.7)),4)
    Q2_snpack(I,J) = real(out_QSFC - prof_const_b*(max((log(2.0/Z0WRF)-real(l_psi_s,4)),0.7)),4)

  ! compute U10 and V10
    U10_snpack(I,J) = (real(in_UST,4)/real(0.4,4))*(max((log(10.0/Z0WRF)-real(l_psi_s,4)),0.7)) * U_ML/real(in_VW,4)
    V10_snpack(I,J) = (real(in_UST,4)/real(0.4,4))*(max((log(10.0/Z0WRF)-real(l_psi_s,4)),0.7)) * V_ML/real(in_VW,4)

!    if( (I .eq. 37) .and. (J .eq. 99)) then
!
!            write(*,*) 'in sfc:',itimestep,out_TSK,out_HFX,QFX(I,J),in_UST, &
!                    & Z0WRF,SNOWH(I,J),l_psi_s,T2_snpack(I,J),Q2_snpack(I,J),out_QSFC
!    endif


  ! sfc_T,sfc_Q
    sfc_T(I,1:snc_N,J) = real(TRAD  - prof_const_a*(max((log(real(snc_Z(I,:,J),4)/Z0WRF)-real(l_psi_s,4)),0.7)),4)
    sfc_Q(I,1:snc_N,J) = real(QSFC1D - prof_const_b*(max((log(real(snc_Z(I,:,J),4)/Z0WRF)-real(l_psi_s,4)),0.7)),4)
   
   ENDIF ! glacial split ends 

! INPUT/OUTPUT
             m_budg_deposit(I,J) =    m_budg_deposit(I,J)  +  real(l_mbug_deposit   ,4)
             TSK      (I,J)                = TRAD
             HFX      (I,J)                = FSH
             IF ( SALB > -999 ) THEN
                ALBEDO(I,J)                = SALB
             ENDIF
             SNOW     (I,J)                = SWE
             SNOWH    (I,J)                = SNDPTH
             EMISS    (I,J)                = EMISSI
             QSFC     (I,J)                = QSFC1D

! OUTPUT

             Z0       (I,J)                = Z0WRF
             ZNT      (I,J)                = Z0WRF
         
             in_drift(I,J) = real(l_in_drift,4)
             in_conc(I,J)  = real(l_in_conc,4)
             in_csalt(I,J) = real(l_in_csalt,4)
             in_q_lb(I,J)  = real(l_in_q_lb,4)
             in_N_lb(I,J)  = real(l_in_N_lb,4)

             ice_fluxq(I,J) = 0.0 
             ice_fluxN(I,J) = 0.0 

          ENDIF                                                         ! endif of land-sea test

      ENDDO ILOOP                                                       ! of I loop
   ENDDO JLOOP                                                          ! of J loop


!!! expanding the sn_XXX arrays to have non-zero values even for points that are 'in ocean' at the moment. 
!!! This is done so that when the downscaling is done for the nested domain, points in the high resolution coastline
!!! where there is somehow no snowpack will get initialized properly
    if( mod(itimestep,int(snpack_write_dt/dt)) .eq. 0) then

   IILOOP : DO I = its, ite
        JJLOOP : DO J = jts , jte

           IF( ((snpack_mode == 'antarctica') .and. ISLTYP(I,J) == 8) .or. &
&              ( snpack_mode == 'land' ) ) then

           else

                min_d = 0.0
                c_x = i
                c_y = j
                do iii=-5,5
                   do jjj=-5,5
                      ccci = i + iii
                      cccj = j + jjj

                 if ( ( ((snpack_mode == 'antarctica') .and. (isltyp(ccci,cccj) == 8)) .or. &
&                     ((snpack_mode == 'land') .and. (isltyp(ccci,cccj) .ne. 14)) )  .and. &
&                      (sn_nlayers(ccci,cccj) .gt. 0) ) then
                      !if( (ISLTYP(ccci,cccj) == 8) .and. (sn_nlayers(ccci,cccj) .gt. 0)  ) then
                           d = sqrt( real( (iii)*(iii) + (jjj) * (jjj) , 4) )
                           if(min_d .eq. 0.0) then
                              d = min_d
                           endif
                           if(d .le. min_d) then
                              min_d = d
                              c_x = ccci
                              c_y = cccj 
                           endif
                      endif 
                   enddo
                enddo    

         sn_T(I,1:snpack_layers_to_save,J)     = sn_T(c_x,1:snpack_layers_to_save,c_y)
         sn_thick(I,1:snpack_layers_to_save,J) = sn_thick(c_x,1:snpack_layers_to_save,c_y)
         sn_volI(I,1:snpack_layers_to_save,J)  = sn_volI(c_x,1:snpack_layers_to_save,c_y)
         sn_volW(I,1:snpack_layers_to_save,J)  = sn_volW(c_x,1:snpack_layers_to_save,c_y)
         sn_volV(I,1:snpack_layers_to_save,J)  = sn_volV(c_x,1:snpack_layers_to_save,c_y)
         sn_rg(I,1:snpack_layers_to_save,J)    = sn_rg(c_x,1:snpack_layers_to_save,c_y)
                                                  
         sn_rb(I,1:snpack_layers_to_save,J)    = sn_rb(c_x,1:snpack_layers_to_save,c_y)
         sn_dd(I,1:snpack_layers_to_save,J)    = sn_dd(c_x,1:snpack_layers_to_save,c_y)
         sn_sp(I,1:snpack_layers_to_save,J)    = sn_sp(c_x,1:snpack_layers_to_save,c_y)
         sn_cdot(I,1:snpack_layers_to_save,J)  = sn_cdot(c_x,1:snpack_layers_to_save,c_y)
         sn_meta(I,1:snpack_layers_to_save,J)  = sn_meta(c_x,1:snpack_layers_to_save,c_y)
         sn_depd(I,1:snpack_layers_to_save,J)  = sn_depd(c_x,1:snpack_layers_to_save,c_y)
         sn_graintype(I,1:snpack_layers_to_save,J)  = sn_graintype(c_x,1:snpack_layers_to_save,c_y)
         sn_mk(I,1:snpack_layers_to_save,J)  = sn_mk(c_x,1:snpack_layers_to_save,c_y)

         sn_nlayers(I,J) = sn_nlayers(c_x,c_y)
 
             endif

        ENDDO JJLOOP
   ENDDO IILOOP

endif


!------------------------------------------------------
  END SUBROUTINE snowpacklsm
!------------------------------------------------------

  SUBROUTINE snowpack_init (SNOW,SNOWH,ISLTYP,XLAT,XLONG, &
       ids,ide, jds,jde, kds,kde,                &
       ims,ime, jms,jme, kms,kme,                &
       its,ite, jts,jte, kts,kte,                &
       dt,ht,                             &
       id,in_glo_counter,snpack_nlayers, sn_T, &
       sn_thick,sn_volI,sn_volW,sn_volV,sn_rg,sn_rb,sn_dd,sn_sp,sn_cdot,sn_meta,sn_depd, &
       sn_graintype,sn_mk, &
       sn_from_file,z_firstlev,snc_N,snc_z,snpack_dt,snpack_write_dt,snpack_layers_to_save)                                                    

  use module_driver_constants, ONLY : max_domains
  use module_dm

  IMPLICIT NONE
   real, intent(in) :: dt
   real,intent(inout) :: snpack_dt,snpack_write_dt
   double precision :: tmp_snpack_dt,tmp_snpack_write_dt
   real, dimension(ims:ime,jms:jme),intent(in) :: z_firstlev
   integer, intent(in) :: snpack_layers_to_save
   real,    optional, dimension(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_T
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_thick
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_volI
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_volW
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_volV
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_rg
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_rb
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_dd
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_sp
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_cdot
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_meta
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_depd
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_graintype
   REAL,    OPTIONAL, DIMENSION(ims:ime,snpack_layers_to_save,jms:jme),intent(in)  :: sn_mk

   integer,intent(in) :: snc_N
   real,dimension(ims:ime,snc_N,jms:jme), intent(inout) :: snc_Z
    double precision, dimension(snpack_layers_to_save) :: arr_T
    double precision, dimension(snpack_layers_to_save) :: arr_thick
    double precision, dimension(snpack_layers_to_save) :: arr_volI
    double precision, dimension(snpack_layers_to_save) :: arr_volW
    double precision, dimension(snpack_layers_to_save) :: arr_volV
    double precision, dimension(snpack_layers_to_save) :: arr_rg
    double precision, dimension(snpack_layers_to_save) :: arr_rb
    double precision, dimension(snpack_layers_to_save) :: arr_dd
    double precision, dimension(snpack_layers_to_save) :: arr_sp
    double precision, dimension(snpack_layers_to_save) :: arr_cdot
    double precision, dimension(snpack_layers_to_save) :: arr_meta
    double precision, dimension(snpack_layers_to_save) :: arr_depd
    double precision, dimension(snpack_layers_to_save) :: arr_graintype
    double precision, dimension(snpack_layers_to_save) :: arr_mk

    integer,intent(inout),dimension(ims:ime,jms:jme) :: snpack_nlayers
    logical,intent(in) :: sn_from_file
    INTEGER, INTENT(IN) :: id
    INTEGER, INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde,  &
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           its,ite, jts,jte, kts,kte

    REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)    ::     SNOW,SNOWH
    INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN)   :: ISLTYP
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: XLAT,XLONG         !latitude
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: HT          !terrain height (m)
    integer, optional, dimension(ims:ime,jms:jme), intent(inout) :: in_glo_counter
    integer :: myproc
    integer :: simulation_start_year,simulation_start_month,&
&               simulation_start_day,simulation_start_hour,&
&               simulation_start_minute,simulation_start_second
    character(len=24) :: simulation_start_date
    INTEGER                   :: i,j,itf,jtf
    INTEGER :: counter_snowpoints
    logical :: new_alloc
    integer :: l_maxdom
    real :: z_top,z_top_c,z_bot,dz_bot
    integer :: nz_c
    double precision :: tmp_snow,tmp_snowh
    character*256 :: snpack_mode

    itf=min0(ite,ide-1)
    jtf=min0(jte,jde-1)

    call wrf_get_myproc( myproc )

    write(*,*) ' MY PROC: ', myproc
    call nl_get_snpack_mode  ( id, snpack_mode)
    write(*,*) 'SNPACK_MODE: ', snpack_mode

    counter_snowpoints=0
    do J=jts,jtf
         do I=its,itf
            if( ((snpack_mode == 'antarctica') .and. (ISLTYP(I,J) == 8)) .or. &
&                (snpack_mode == 'land')  ) then
               counter_snowpoints = counter_snowpoints + 1
            endif
         enddo
    enddo
    write(*,*) 'AFTER: counter snowpoints: ', counter_snowpoints

    write(*,*) 'IS TIME STEP present ? : DT == ', dt
    write(*,*) 'WHAT IS THE ID AND THE MAX ID ? : ID, MAX_DOM == ', id, max_domains

    call nl_get_max_dom ( id, l_maxdom)
    call initialize_global_f(l_maxdom,id)
    new_alloc = .false.
    call initialize_f(id,counter_snowpoints,new_alloc)

    CALL nl_get_start_year   ( id, simulation_start_year   )
    CALL nl_get_start_month  ( id, simulation_start_month  )
    CALL nl_get_start_day    ( id, simulation_start_day    )
    CALL nl_get_start_hour   ( id, simulation_start_hour   )
    CALL nl_get_start_minute ( id, simulation_start_minute )
    CALL nl_get_start_second ( id, simulation_start_second )


    tmp_snpack_dt = real(snpack_dt,8)
    tmp_snpack_write_dt = real(snpack_write_dt,8)
   
    WRITE ( * , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
             simulation_start_year,simulation_start_month,simulation_start_day,&
             simulation_start_hour,simulation_start_minute,simulation_start_second
       simulation_start_date(1:24) = simulation_start_date(1:19)//'.0000'
     
       counter_snowpoints=0
       do J=jts,jtf
          do I=its,itf
            if( ((snpack_mode == 'antarctica') .and. (ISLTYP(I,J) == 8)) .or. &
&                (snpack_mode == 'land')  ) then

                counter_snowpoints = counter_snowpoints + 1
                in_glo_counter(I,J) = counter_snowpoints
                     arr_T     = real(sn_T(I,1:snpack_layers_to_save,J),8)
                     arr_thick = real(sn_thick(I,1:snpack_layers_to_save,J),8)
                     arr_volI  = real(sn_volI(I,1:snpack_layers_to_save,J),8)
                     arr_volW  = real(sn_volW(I,1:snpack_layers_to_save,J),8)
                     arr_volV  = real(sn_volV(I,1:snpack_layers_to_save,J),8)
                     arr_rg    = real(sn_rg(I,1:snpack_layers_to_save,J),8)
     
                     arr_rb    = real(sn_rb(I,1:snpack_layers_to_save,J),8)
                     arr_dd    = real(sn_dd(I,1:snpack_layers_to_save,J),8)
                     arr_sp    = real(sn_sp(I,1:snpack_layers_to_save,J),8)
                     arr_cdot  = real(sn_cdot(I,1:snpack_layers_to_save,J),8)
                     arr_meta  = real(sn_meta(I,1:snpack_layers_to_save,J),8)
                     arr_depd  = real(sn_depd(I,1:snpack_layers_to_save,J),8)
                     arr_graintype = real(sn_graintype(I,1:snpack_layers_to_save,J),8)
                     arr_mk = real(sn_mk(I,1:snpack_layers_to_save,J),8)

                     if(snpack_nlayers(I,J) .eq. 0) then
                          write(*,*) 'OH MIEN GOTTTT : ', I,J,snpack_nlayers(I,J),id,sn_from_file
                          if ( (id .gt. 1) .and. ( .not. (sn_from_file) )) then
                             write(*,*) ' creating snowpack from thin air @ ', id, I,J
                             arr_T(1:40) = real(263.15,8)
                             arr_thick(1:40) = real(0.25,8)
                             arr_volI(1:40) = real(0.5,8)  
                             arr_volW(1:40) = real(0.0,8)
                             arr_volV(1:40) = real(0.5,8)
                             arr_rg(1:40) = real(0.0002,8)
                             arr_rb(1:40) = real(0.0001,8)
                             arr_dd(1:40) = real(0.0,8)
                             arr_sp(1:40) = real(1.0,8)
                             arr_cdot(1:40) = real(0.0,8)
                             arr_meta(1:40) = real(0.0,8)
                             arr_depd(1:40) = real(0.0,8)
                             arr_graintype = real(0.0,8)
                             arr_mk = real(0.0,8)
                             snpack_nlayers(I,J) = 39
                          endif
                     endif
 
                if(new_alloc) then
                   write(*,*) 'NEW ALLOC MUST BE DONE for GRID: ',id, I,J
                   call init_snowpack(id,counter_snowpoints,snpack_layers_to_save,real(XLAT(I,J),8),real(XLONG(I,J),8), &
    &                                 real(HT(I,J),8),real(263.15,8),real(dt,8)/real(60.0,8),I,J, &
    &                                 simulation_start_year,simulation_start_month,simulation_start_day,simulation_start_hour,&
    &                                 simulation_start_minute,snpack_nlayers(I,J),arr_T,arr_thick,arr_volI,arr_volW,arr_volV, &
    &                                 arr_rg,arr_rb,arr_dd,arr_sp,arr_cdot,arr_meta,arr_depd,arr_graintype,arr_mk,sn_from_file, &
    &                                 real(1.1,8),tmp_snowh,tmp_snow,tmp_snpack_dt, &
    &                                 tmp_snpack_write_dt)
                SNOWH(I,J) = real(tmp_snowh,4)
                SNOW(I,J) = real(tmp_snow,4)
                else
                   write(*,*) 'OLD ALLOC CONTINUED for GRID: ',id, I,J
                endif
             else
  
             endif
           enddo
        enddo

        counter_snowpoints=0
        do J=jts,jtf
           do I=its,itf
                counter_snowpoints = counter_snowpoints + 1
                !write(*,*) 'z_firstlev: ', z_firstlev(I,J)
                Z_top = z_firstlev(I,J) * 0.5
                Z_top_c = real(floor(Z_top)) - 3.0
                z_bot = 0.5
                dz_bot = (Z_top_c-z_bot)/(snc_N-1)
                do nz_c = 1,snc_N
                   snc_Z(I,nz_c,J) = z_bot + (nz_c-1)*dz_bot  
                enddo
           enddo
        enddo

  END SUBROUTINE snowpack_init

!------------------------------------------------------------------------------------------
SUBROUTINE Qsat_noah(T,p,es,qs)

  implicit none
  real, intent(in) :: T
  real, intent(in)  :: p        ! surface atmospheric pressure (pa)
  real, intent(out) :: es       ! vapor pressure (pa)
  real, intent(out) :: qs       ! humidity (kg/kg)

      real :: T_limit
      real :: td,vp,vp1,vp2
  !
  ! For water vapor (temperature range 0C-100C)
  !
      real, parameter :: a0 =  6.11213476
      real, parameter :: a1 =  0.444007856
      real, parameter :: a2 =  0.143064234e-01
      real, parameter :: a3 =  0.264461437e-03
      real, parameter :: a4 =  0.305903558e-05
      real, parameter :: a5 =  0.196237241e-07
      real, parameter :: a6 =  0.892344772e-10
      real, parameter :: a7 = -0.373208410e-12
      real, parameter :: a8 =  0.209339997e-15
  !
  ! For derivative:water vapor
  !
      real, parameter :: b0 =  0.444017302
      real, parameter :: b1 =  0.286064092e-01
      real, parameter :: b2 =  0.794683137e-03
      real, parameter :: b3 =  0.121211669e-04
      real, parameter :: b4 =  0.103354611e-06
      real, parameter :: b5 =  0.404125005e-09
      real, parameter :: b6 = -0.788037859e-12
      real, parameter :: b7 = -0.114596802e-13
      real, parameter :: b8 =  0.381294516e-16
  !
  ! For ice (temperature range -75C-0C)
  !
      real, parameter :: c0 =  6.11123516
      real, parameter :: c1 =  0.503109514
      real, parameter :: c2 =  0.188369801e-01
      real, parameter :: c3 =  0.420547422e-03
      real, parameter :: c4 =  0.614396778e-05
      real, parameter :: c5 =  0.602780717e-07
      real, parameter :: c6 =  0.387940929e-09
      real, parameter :: c7 =  0.149436277e-11
      real, parameter :: c8 =  0.262655803e-14
  !
  ! For derivative:ice
  !
      real, parameter :: d0 =  0.503277922
      real, parameter :: d1 =  0.377289173e-01
      real, parameter :: d2 =  0.126801703e-02
      real, parameter :: d3 =  0.249468427e-04
      real, parameter :: d4 =  0.313703411e-06
      real, parameter :: d5 =  0.257180651e-08
      real, parameter :: d6 =  0.133268878e-10
      real, parameter :: d7 =  0.394116744e-13
      real, parameter :: d8 =  0.498070196e-16


      T_limit = T - 273.15
      if (T_limit > 100.0) T_limit=100.0
      if (T_limit < -75.0) T_limit=-75.0

      td       = T_limit
      if (td >= 0.0) then
         es   = a0 + td*(a1 + td*(a2 + td*(a3 + td*(a4 &
              + td*(a5 + td*(a6 + td*(a7 + td*a8)))))))
      else
         es   = c0 + td*(c1 + td*(c2 + td*(c3 + td*(c4 &
              + td*(c5 + td*(c6 + td*(c7 + td*c8)))))))
      endif

      es    = es    * 100.0            ! pa

      vp    = 1.0   / (p - 0.378*es)
      vp1   = 0.622 * vp
      vp2   = vp1   * vp

      qs    = es    * vp1             ! kg/kg

!      write(*,*) 'in Qsat_noah: ', es

    end subroutine Qsat_noah






!------------------------------------------------------------------------------------------
!
END MODULE module_sf_snowpacklsm
