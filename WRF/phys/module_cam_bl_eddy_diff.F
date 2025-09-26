#define WRF_PORT
#define MODAL_AERO
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov

  module eddy_diff

  !--------------------------------------------------------------------------------- !
  !                                                                                  !
  ! The University of Washington Moist Turbulence Scheme to compute eddy diffusion   ! 
  ! coefficients associated with dry and moist turbulences in the whole              !
  ! atmospheric layers.                                                              !
  !                                                                                  !
  ! For detailed description of the code and its performances, see                   !
  !                                                                                  !
  ! 1.'A new moist turbulence parametrization in the Community Atmosphere Model'     !
  !    by Christopher S. Bretherton and Sungsu Park. J. Climate. 2009. 22. 3422-3448 !
  ! 2.'The University of Washington shallow convection and moist turbulence schemes  !
  !    and their impact on climate simulations with the Community Atmosphere Model'  !
  !    by Sungsu Park and Christopher S. Bretherton. J. Climate. 2009. 22. 3449-3469 !
  !                                                                                  !
  ! For questions on the scheme and code, send an email to                           !
  !     Sungsu Park      at sungsup@ucar.edu (tel: 303-497-1375)                     !
  !     Chris Bretherton at breth@washington.edu                                     !
  !                                                                                  ! 
  ! Developed by Chris Bretherton at the University of Washington, Seattle, WA.      !
  !              Sungsu Park      at the CGD/NCAR, Boulder, CO.                      !
  ! Last coded on May.2006, Dec.2009 by Sungsu Park.                                 !
  !                                                                                  !  
  !--------------------------------------------------------------------------------- !

  use diffusion_solver, only : vdiff_selector
#ifndef WRF_PORT 
  use cam_history,      only : outfld, addfld, phys_decomp
  use cam_logfile,      only : iulog
  use ppgrid,           only : pver
#else
  use module_cam_support,   only: iulog, pver,outfld, addfld, phys_decomp  
#endif

  implicit none
  private
  save

  public init_eddy_diff
  public compute_eddy_diff

  type(vdiff_selector)        :: fieldlist_wet                  ! Logical switches for moist mixing ratio diffusion
  type(vdiff_selector)        :: fieldlist_dry                  ! Logical switches for dry   mixing ratio diffusion
  integer,          parameter :: r8 = selected_real_kind(12)    ! 8 byte real

  ! --------------------------------- !
  ! PBL Parameters used in the UW PBL !
  ! --------------------------------- !

  character,        parameter :: sftype         = 'l'           ! Method for calculating saturation fraction

  character(len=4), parameter :: choice_evhc    = 'maxi'        ! 'orig',   'ramp',   'maxi'   : recommended to be used with choice_radf 
  character(len=6), parameter :: choice_radf    = 'maxi'        ! 'orig',   'ramp',   'maxi'   : recommended to be used with choice_evhc 
  character(len=6), parameter :: choice_SRCL    = 'nonamb'      ! 'origin', 'remove', 'nonamb'
 
  character(len=6), parameter :: choice_tunl    = 'rampcl'      ! 'origin', 'rampsl'(Sungsu), 'rampcl'(Chris)
  real(r8),         parameter :: ctunl          =  2._r8        !  Maximum asympt leng = ctunl*tunl when choice_tunl = 'rampsl(cl)' [ no unit ]
  character(len=6), parameter :: choice_leng    = 'origin'      ! 'origin', 'takemn'
  real(r8),         parameter :: cleng          =  3._r8        !  Order of 'leng' when choice_leng = 'origin' [ no unit ]
  character(len=6), parameter :: choice_tkes    = 'ibprod'      ! 'ibprod' (include tkes in computing bprod), 'ebprod'(exclude)

  ! Parameters for 'sedimenttaion-entrainment feedback' for liquid stratus 
  ! If .false.,  no sedimentation entrainment feedback ( i.e., use default evhc )

  logical,          parameter :: id_sedfact     = .false.
  real(r8),         parameter :: ased           =  9._r8        !  Valid only when id_sedfact = .true.

  ! --------------------------------------------------------------------------------------------------- !
  ! Parameters governing entrainment efficiency A = a1l(i)*evhc, evhc = 1 + a2l * a3l * L * ql / jt2slv !
  ! Here, 'ql' is cloud-top LWC and 'jt2slv' is the jump in 'slv' across                                !
  ! the cloud-top entrainment zone ( across two grid layers to consider full mixture )                  !
  ! --------------------------------------------------------------------------------------------------- !

  real(r8),         parameter :: a1l            =   0.10_r8     ! Dry entrainment efficiency for TKE closure
                                                                ! a1l = 0.2*tunl*erat^-1.5, where erat = <e>/wstar^2 for dry CBL =  0.3.

  real(r8),         parameter :: a1i            =   0.2_r8      ! Dry entrainment efficiency for wstar closure
  real(r8),         parameter :: ccrit          =   0.5_r8      ! Minimum allowable sqrt(tke)/wstar. Used in solving cubic equation for 'ebrk'
  real(r8),         parameter :: wstar3factcrit =   0.5_r8      ! 1/wstar3factcrit is the maximally allowed enhancement of 'wstar3' due to entrainment.

  real(r8),         parameter :: a2l            =   30._r8      ! Moist entrainment enhancement param (recommended range : 10~30 )
  real(r8),         parameter :: a3l            =   0.8_r8      ! Approximation to a complicated thermodynamic parameters

  real(r8),         parameter :: jbumin         =   .001_r8     ! Minimum buoyancy jump at an entrainment jump, [m/s2]
  real(r8),         parameter :: evhcmax        =   10._r8      ! Upper limit of evaporative enhancement factor

  real(r8),         parameter :: ustar_min      =   0.01_r8     ! Minimum permitted value of ustar [ m/s ] 
  real(r8),         parameter :: onet           =   1._r8/3._r8 ! 1/3 power in wind gradient expression [ no unit ]
#ifndef WRF_PORT
  !pver is not a parameter in cam_support module in WRF, therefore ncvmax cannot be equated to pver as parameter 
  integer,          parameter :: ncvmax         =   pver        ! Max numbers of CLs (good to set to 'pver')
#else
  integer                     :: ncvmax  
#endif
  real(r8),         parameter :: qmin           =   1.e-5_r8    ! Minimum grid-mean LWC counted as clouds [kg/kg]
  real(r8),         parameter :: ntzero         =   1.e-12_r8   ! Not zero (small positive number used in 's2')
  real(r8),         parameter :: b1             =   5.8_r8      ! TKE dissipation D = e^3/(b1*leng), e = b1*W.
  real(r8)                    :: b123                           ! b1**(2/3)
  real(r8),         parameter :: tunl           =   0.085_r8    ! Asympt leng = tunl*(turb lay depth)
  real(r8),         parameter :: alph1          =   0.5562_r8   ! alph1~alph5 : Galperin instability function parameters
  real(r8),         parameter :: alph2          =  -4.3640_r8   !               These coefficients are used to calculate 
  real(r8),         parameter :: alph3          = -34.6764_r8   !               'sh' and 'sm' from 'gh'.
  real(r8),         parameter :: alph4          =  -6.1272_r8   !
  real(r8),         parameter :: alph5          =   0.6986_r8   !
  real(r8),         parameter :: ricrit         =   0.19_r8     ! Critical Richardson number for turbulence. Can be any value >= 0.19.
  real(r8),         parameter :: ae             =   1._r8       ! TKE transport efficiency [no unit]
  real(r8),         parameter :: rinc           =  -0.04_r8     ! Minimum W/<W> used for CL merging test 
  real(r8),         parameter :: wpertmin       =   1.e-6_r8    ! Minimum PBL eddy vertical velocity perturbation
  real(r8),         parameter :: wfac           =   1._r8       ! Ratio of 'wpert' to sqrt(tke) for CL.
  real(r8),         parameter :: tfac           =   1._r8       ! Ratio of 'tpert' to (w't')/wpert for CL. Same ratio also used for q
  real(r8),         parameter :: fak            =   8.5_r8      ! Constant in surface temperature excess for stable STL. [ no unit ]         
  real(r8),         parameter :: rcapmin        =   0.1_r8      ! Minimum allowable e/<e> in a CL
  real(r8),         parameter :: rcapmax        =   2.0_r8      ! Maximum allowable e/<e> in a CL
  real(r8),         parameter :: tkemax         =  20._r8       ! TKE is capped at tkemax [m2/s2]
  real(r8),         parameter :: lambda         =   0.5_r8      ! Under-relaxation factor ( 0 < lambda =< 1 )

  logical,          parameter :: use_kvf        =  .false.      ! .true. (.false.) : initialize kvh/kvm =  kvf ( 0. )
  logical,          parameter :: use_dw_surf    =  .true.       ! Used in 'zisocl'. Default is 'true'
                                                                ! If 'true', surface interfacial energy does not contribute to the CL mean
                                                                !            stbility functions after finishing merging.     For this case,
                                                                !           'dl2n2_surf' is only used for a merging test based on 'l2n2'
                                                                ! If 'false',surface interfacial enery explicitly contribute to    CL mean
                                                                !            stability functions after finishing merging.    For this case,
                                                                !           'dl2n2_surf' and 'dl2s2_surf' are directly used for calculating
                                                                !            surface interfacial layer energetics

  logical,          parameter :: set_qrlzero    =  .false.      ! .true. ( .false.) : turning-off ( on) radiative-turbulence interaction by setting qrl = 0.

  ! ------------------------------------- !
  ! PBL Parameters not used in the UW PBL !
  ! ------------------------------------- !

  real(r8),         parameter :: pblmaxp        =  4.e4_r8      ! PBL max depth in pressure units. 
  real(r8),         parameter :: zkmin          =  0.01_r8      ! Minimum kneutral*f(ri). 
  real(r8),         parameter :: betam          = 15.0_r8       ! Constant in wind gradient expression.
  real(r8),         parameter :: betas          =  5.0_r8       ! Constant in surface layer gradient expression.
  real(r8),         parameter :: betah          = 15.0_r8       ! Constant in temperature gradient expression.
  real(r8),         parameter :: fakn           =  7.2_r8       ! Constant in turbulent prandtl number.
  real(r8),         parameter :: ricr           =  0.3_r8       ! Critical richardson number.
  real(r8),         parameter :: sffrac         =  0.1_r8       ! Surface layer fraction of boundary layer
  real(r8),         parameter :: binm           =  betam*sffrac ! betam * sffrac
  real(r8),         parameter :: binh           =  betah*sffrac ! betah * sffrac

  ! ------------------------------------------------------- !
  ! PBL constants set using values from other parts of code !
  ! ------------------------------------------------------- !

  real(r8)                    :: cpair                          ! Specific heat of dry air
  real(r8)                    :: rair                           ! Gas const for dry air
  real(r8)                    :: zvir                           ! rh2o/rair - 1
  real(r8)                    :: latvap                         ! Latent heat of vaporization
  real(r8)                    :: latice                         ! Latent heat of fusion
  real(r8)                    :: latsub                         ! Latent heat of sublimation
  real(r8)                    :: g                              ! Gravitational acceleration
  real(r8)                    :: vk                             ! Von Karman's constant
  real(r8)                    :: ccon                           ! fak * sffrac * vk

  integer                     :: ntop_turb                      ! Top interface level to which turbulent vertical diffusion is applied ( = 1 )
  integer                     :: nbot_turb                      ! Bottom interface level to which turbulent vertical diff is applied ( = pver )

  real(r8), allocatable       :: ml2(:)                         ! Mixing lengths squared. Not used in the UW PBL. Used for computing free air diffusivity.

  CONTAINS

  !============================================================================ !
  !                                                                             !
  !============================================================================ !
  
  subroutine init_eddy_diff( kind, pver, gravx, cpairx, rairx, zvirx, & 
                             latvapx, laticex, ntop_eddy, nbot_eddy, hypm, vkx )
    !---------------------------------------------------------------- ! 
    ! Purpose:                                                        !
    ! Initialize time independent constants/variables of PBL package. !
    !---------------------------------------------------------------- !
    use diffusion_solver, only: init_vdiff, vdiff_select
#ifndef WRF_PORT 
    use cam_history,      only: outfld, addfld, phys_decomp
#else
    use module_cam_support,   only: outfld, addfld, phys_decomp  
#endif
    implicit none
    ! --------- !
    ! Arguments !
    ! --------- !
    integer,  intent(in) :: kind       ! Kind of reals being passed in
    integer,  intent(in) :: pver       ! Number of vertical layers
    integer,  intent(in) :: ntop_eddy  ! Top interface level to which eddy vertical diffusivity is applied ( = 1 )
    integer,  intent(in) :: nbot_eddy  ! Bottom interface level to which eddy vertical diffusivity is applied ( = pver )
    real(r8), intent(in) :: gravx      ! Acceleration of gravity
    real(r8), intent(in) :: cpairx     ! Specific heat of dry air
    real(r8), intent(in) :: rairx      ! Gas constant for dry air
    real(r8), intent(in) :: zvirx      ! rh2o/rair - 1
    real(r8), intent(in) :: latvapx    ! Latent heat of vaporization
    real(r8), intent(in) :: laticex    ! Latent heat of fusion
    real(r8), intent(in) :: hypm(pver) ! Reference pressures at midpoints
    real(r8), intent(in) :: vkx        ! Von Karman's constant

    character(128)       :: errstring  ! Error status for init_vdiff
    integer              :: k          ! Vertical loop index
#ifdef WRF_PORT    
    !As pver is not parameter in the cam_support module in WRF, a value to ncvmax is given here
    ncvmax = pver        ! Max numbers of CLs (good to set to 'pver') 
#endif
    if( kind .ne. r8 ) then
        write(iulog,*) 'wrong KIND of reals passed to init_diffusvity -- exiting.'
#ifdef WRF_PORT
        call wrf_message(iulog)
#endif 
        stop 'init_eddy_diff'
    endif

    ! --------------- !
    ! Basic constants !
    ! --------------- !

    cpair     = cpairx
    rair      = rairx
    g         = gravx
    zvir      = zvirx
    latvap    = latvapx
    latice    = laticex
    latsub    = latvap + latice
    vk        = vkx
    ccon      = fak*sffrac*vk
    ntop_turb = ntop_eddy
    nbot_turb = nbot_eddy
    b123      = b1**(2._r8/3._r8)

    ! Set the square of the mixing lengths. Only for CAM3 HB PBL scheme.
    ! Not used for UW moist PBL. Used for free air eddy diffusivity.
#ifndef WRF_PORT 
    ! ml2 is never used currently as use_kvf is set to false
    allocate(ml2(pver+1))
    ml2(1:ntop_turb) = 0._r8
    do k = ntop_turb + 1, nbot_turb
       ml2(k) = 30.0_r8**2
    end do
    ml2(nbot_turb+1:pver+1) = 0._r8
#endif
    
    ! Initialize diffusion solver module

    call init_vdiff(r8, 1, rair, g, fieldlist_wet, fieldlist_dry, errstring)

    ! Select the fields which will be diffused 

    if(vdiff_select(fieldlist_wet,'s').ne.'')   write(iulog,*) 'error: ', vdiff_select(fieldlist_wet,'s')
#ifdef WRF_PORT
        call wrf_message(iulog)
#endif 
    if(vdiff_select(fieldlist_wet,'q',1).ne.'') write(iulog,*) 'error: ', vdiff_select(fieldlist_wet,'q',1)
#ifdef WRF_PORT
        call wrf_message(iulog)
#endif 
    if(vdiff_select(fieldlist_wet,'u').ne.'')   write(iulog,*) 'error: ', vdiff_select(fieldlist_wet,'u')
#ifdef WRF_PORT
        call wrf_message(iulog)
#endif 
    if(vdiff_select(fieldlist_wet,'v').ne.'')   write(iulog,*) 'error: ', vdiff_select(fieldlist_wet,'v')
#ifdef WRF_PORT
        call wrf_message(iulog)
#endif 

    ! ------------------------------------------------------------------- !
    ! Writing outputs for detailed analysis of UW moist turbulence scheme !
    ! ------------------------------------------------------------------- !

    call addfld('UW_errorPBL',      'm2/s',    1,      'A',  'Error function of UW PBL',                              phys_decomp )
    call addfld('UW_n2',            's-2',     pver,   'A',  'Buoyancy Frequency, LI',                                phys_decomp )
    call addfld('UW_s2',            's-2',     pver,   'A',  'Shear Frequency, LI',                                   phys_decomp )
    call addfld('UW_ri',            'no',      pver,   'A',  'Interface Richardson Number, I',                        phys_decomp )
    call addfld('UW_sfuh',          'no',      pver,   'A',  'Upper-Half Saturation Fraction, L',                     phys_decomp )
    call addfld('UW_sflh',          'no',      pver,   'A',  'Lower-Half Saturation Fraction, L',                     phys_decomp )
    call addfld('UW_sfi',           'no',      pver+1, 'A',  'Interface Saturation Fraction, I',                      phys_decomp )
    call addfld('UW_cldn',          'no',      pver,   'A',  'Cloud Fraction, L',                                     phys_decomp )
    call addfld('UW_qrl',           'g*W/m2',  pver,   'A',  'LW cooling rate, L',                                    phys_decomp )
    call addfld('UW_ql',            'kg/kg',   pver,   'A',  'ql(LWC), L',                                            phys_decomp )
    call addfld('UW_chu',           'g*kg/J',  pver+1, 'A',  'Buoyancy Coefficient, chu, I',                          phys_decomp )
    call addfld('UW_chs',           'g*kg/J',  pver+1, 'A',  'Buoyancy Coefficient, chs, I',                          phys_decomp )
    call addfld('UW_cmu',           'g/kg/kg', pver+1, 'A',  'Buoyancy Coefficient, cmu, I',                          phys_decomp )
    call addfld('UW_cms',           'g/kg/kg', pver+1, 'A',  'Buoyancy Coefficient, cms, I',                          phys_decomp )    
    call addfld('UW_tke',           'm2/s2',   pver+1, 'A',  'TKE, I',                                                phys_decomp )
    call addfld('UW_wcap',          'm2/s2',   pver+1, 'A',  'Wcap, I',                                               phys_decomp )        
    call addfld('UW_bprod',         'm2/s3',   pver+1, 'A',  'Buoyancy production, I',                                phys_decomp )
    call addfld('UW_sprod',         'm2/s3',   pver+1, 'A',  'Shear production, I',                                   phys_decomp )    
    call addfld('UW_kvh',           'm2/s',    pver+1, 'A',  'Eddy diffusivity of heat, I',                           phys_decomp )
    call addfld('UW_kvm',           'm2/s',    pver+1, 'A',  'Eddy diffusivity of uv, I',                             phys_decomp )
    call addfld('UW_pblh',          'm',       1,      'A',  'PBLH, 1',                                               phys_decomp )
    call addfld('UW_pblhp',         'Pa',      1,      'A',  'PBLH pressure, 1',                                      phys_decomp )
    call addfld('UW_tpert',         'K',       1,      'A',  'Convective T excess, 1',                                phys_decomp )
    call addfld('UW_qpert',         'kg/kg',   1,      'A',  'Convective qt excess, I',                               phys_decomp )
    call addfld('UW_wpert',         'm/s',     1,      'A',  'Convective W excess, I',                                phys_decomp )
    call addfld('UW_ustar',         'm/s',     1,      'A',  'Surface Frictional Velocity, 1',                        phys_decomp )
    call addfld('UW_tkes',          'm2/s2',   1,      'A',  'Surface TKE, 1',                                        phys_decomp )
    call addfld('UW_minpblh',       'm',       1,      'A',  'Minimum PBLH, 1',                                       phys_decomp )
    call addfld('UW_turbtype',      'no',      pver+1, 'A',  'Interface Turbulence Type, I',                          phys_decomp )    
    call addfld('UW_kbase_o',       'no',      ncvmax, 'A',  'Initial CL Base Exterbal Interface Index, CL',          phys_decomp )
    call addfld('UW_ktop_o',        'no',      ncvmax, 'A',  'Initial Top Exterbal Interface Index, CL',              phys_decomp )
    call addfld('UW_ncvfin_o',      '#',       1,      'A',  'Initial Total Number of CL regimes, CL',                phys_decomp )
    call addfld('UW_kbase_mg',      'no',      ncvmax, 'A',  'kbase after merging, CL',                               phys_decomp )
    call addfld('UW_ktop_mg',       'no',      ncvmax, 'A',  'ktop after merging, CL',                                phys_decomp )
    call addfld('UW_ncvfin_mg',     '#',       1,      'A',  'ncvfin after merging, CL',                              phys_decomp )
    call addfld('UW_kbase_f',       'no',      ncvmax, 'A',  'Final kbase with SRCL, CL',                             phys_decomp )
    call addfld('UW_ktop_f',        'no',      ncvmax, 'A',  'Final ktop with SRCL, CL',                              phys_decomp )
    call addfld('UW_ncvfin_f',      '#',       1,      'A',  'Final ncvfin with SRCL, CL',                            phys_decomp )
    call addfld('UW_wet',           'm/s',     ncvmax, 'A',  'Entrainment rate at CL top, CL',                        phys_decomp )
    call addfld('UW_web',           'm/s',     ncvmax, 'A',  'Entrainment rate at CL base, CL',                       phys_decomp )
    call addfld('UW_jtbu',          'm/s2',    ncvmax, 'A',  'Buoyancy jump across CL top, CL',                       phys_decomp )
    call addfld('UW_jbbu',          'm/s2',    ncvmax, 'A',  'Buoyancy jump across CL base, CL',                      phys_decomp )
    call addfld('UW_evhc',          'no',      ncvmax, 'A',  'Evaporative enhancement factor, CL',                    phys_decomp )
    call addfld('UW_jt2slv',        'J/kg',    ncvmax, 'A',  'slv jump for evhc, CL',                                 phys_decomp )
    call addfld('UW_n2ht',          's-2',     ncvmax, 'A',  'n2 at just below CL top interface, CL',                 phys_decomp )
    call addfld('UW_n2hb',          's-2',     ncvmax, 'A',  'n2 at just above CL base interface',                    phys_decomp )
    call addfld('UW_lwp',           'kg/m2',   ncvmax, 'A',  'LWP in the CL top layer, CL',                           phys_decomp )
    call addfld('UW_optdepth',      'no',      ncvmax, 'A',  'Optical depth of the CL top layer, CL',                 phys_decomp )
    call addfld('UW_radfrac',       'no',      ncvmax, 'A',  'Fraction of radiative cooling confined in the CL top',  phys_decomp )
    call addfld('UW_radf',          'm2/s3',   ncvmax, 'A',  'Buoyancy production at the CL top by radf, I',          phys_decomp )        
    call addfld('UW_wstar',         'm/s',     ncvmax, 'A',  'Convective velocity, Wstar, CL',                        phys_decomp )
    call addfld('UW_wstar3fact',    'no',      ncvmax, 'A',  'Enhancement of wstar3 due to entrainment, CL',          phys_decomp )
    call addfld('UW_ebrk',          'm2/s2',   ncvmax, 'A',  'CL-averaged TKE, CL',                                   phys_decomp )
    call addfld('UW_wbrk',          'm2/s2',   ncvmax, 'A',  'CL-averaged W, CL',                                     phys_decomp )
    call addfld('UW_lbrk',          'm',       ncvmax, 'A',  'CL internal thickness, CL',                             phys_decomp )
    call addfld('UW_ricl',          'no',      ncvmax, 'A',  'CL-averaged Ri, CL',                                    phys_decomp )
    call addfld('UW_ghcl',          'no',      ncvmax, 'A',  'CL-averaged gh, CL',                                    phys_decomp )
    call addfld('UW_shcl',          'no',      ncvmax, 'A',  'CL-averaged sh, CL',                                    phys_decomp )
    call addfld('UW_smcl',          'no',      ncvmax, 'A',  'CL-averaged sm, CL',                                    phys_decomp )
    call addfld('UW_gh',            'no',      pver+1, 'A',  'gh at all interfaces, I',                               phys_decomp )
    call addfld('UW_sh',            'no',      pver+1, 'A',  'sh at all interfaces, I',                               phys_decomp )
    call addfld('UW_sm',            'no',      pver+1, 'A',  'sm at all interfaces, I',                               phys_decomp )
    call addfld('UW_ria',           'no',      pver+1, 'A',  'ri at all interfaces, I',                               phys_decomp )
    call addfld('UW_leng',          'm/s',     pver+1, 'A',  'Turbulence length scale, I',                            phys_decomp )

  return

  end subroutine init_eddy_diff

  !=============================================================================== !
  !                                                                                !
  !=============================================================================== !
  
  subroutine compute_eddy_diff( lchnk  ,                                                            &
                                pcols  , pver   , ncol     , t       , qv       , ztodt   ,         &
                                ql     , qi     , s        , rpdel   , cldn     , qrl     , wsedl , &
                                z      , zi     , pmid     , pi      , u        , v       ,         &
                                taux   , tauy   , shflx    , qflx    , wstarent , nturb   ,         &
                                ustar  , pblh   , kvm_in   , kvh_in  , kvm_out  , kvh_out , kvq   , & 
                                cgh    , cgs    , tpert    , qpert   , wpert    , tke     , bprod , &
                                sprod  , sfi    , qsat     , kvinit  ,                              &
                                tauresx, tauresy, ksrftms  ,                                        &
                                ipbl   , kpblh  , wstarPBL , turbtype, sm_aw )
       
    !-------------------------------------------------------------------- ! 
    ! Purpose: Interface to compute eddy diffusivities.                   !
    !          Eddy diffusivities are calculated in a fully implicit way  !
    !          through iteration process.                                 !   
    ! Author:  Sungsu Park. August. 2006.                                 !
    !                       May.    2008.                                 !
    !-------------------------------------------------------------------- !
    use diffusion_solver, only: compute_vdiff
#ifndef WRF_PORT 
    use cam_history,      only: outfld, addfld, phys_decomp
  ! use physics_types,    only: physics_state
    use phys_debug_util,  only: phys_debug_col
    use time_manager,     only: is_first_step, get_nstep
#else
    use module_cam_support,   only: outfld, addfld, phys_decomp 
#endif

    implicit none

  ! type(physics_state)     :: state                     ! Physics state variables

    ! --------------- !
    ! Input Variables !
    ! --------------- ! 

    integer,  intent(in)    :: lchnk   
    integer,  intent(in)    :: pcols                     ! Number of atmospheric columns [ # ]
    integer,  intent(in)    :: pver                      ! Number of atmospheric layers  [ # ]
    integer,  intent(in)    :: ncol                      ! Number of atmospheric columns [ # ]
    integer,  intent(in)    :: nturb                     ! Number of iteration steps for calculating eddy diffusivity [ # ]
    logical,  intent(in)    :: wstarent                  ! .true. means use the 'wstar' entrainment closure. 
    logical,  intent(in)    :: kvinit                    ! 'true' means time step = 1 : used for initializing kvh, kvm (uses kvf or zero)
    real(r8), intent(in)    :: ztodt                     ! Physics integration time step 2 delta-t [ s ]
    real(r8), intent(in)    :: t(pcols,pver)             ! Temperature [K]
    real(r8), intent(in)    :: qv(pcols,pver)            ! Water vapor  specific humidity [ kg/kg ]
    real(r8), intent(in)    :: ql(pcols,pver)            ! Liquid water specific humidity [ kg/kg ]
    real(r8), intent(in)    :: qi(pcols,pver)            ! Ice specific humidity [ kg/kg ]
    real(r8), intent(in)    :: s(pcols,pver)             ! Dry static energy [ J/kg ]
    real(r8), intent(in)    :: rpdel(pcols,pver)         ! 1./pdel where 'pdel' is thickness of the layer [ Pa ]
    real(r8), intent(in)    :: cldn(pcols,pver)          ! Stratiform cloud fraction [ fraction ]
    real(r8), intent(in)    :: qrl(pcols,pver)           ! LW cooling rate
    real(r8), intent(in)    :: wsedl(pcols,pver)         ! Sedimentation velocity of liquid stratus cloud droplet [ m/s ]
    real(r8), intent(in)    :: z(pcols,pver)             ! Layer mid-point height above surface [ m ]
    real(r8), intent(in)    :: zi(pcols,pver+1)          ! Interface height above surface [ m ]
    real(r8), intent(in)    :: pmid(pcols,pver)          ! Layer mid-point pressure [ Pa ]
    real(r8), intent(in)    :: pi(pcols,pver+1)          ! Interface pressure [ Pa ]
    real(r8), intent(in)    :: u(pcols,pver)             ! Zonal velocity [ m/s ]
    real(r8), intent(in)    :: v(pcols,pver)             ! Meridional velocity [ m/s ]
    real(r8), intent(in)    :: taux(pcols)               ! Zonal wind stress at surface [ N/m2 ]
    real(r8), intent(in)    :: tauy(pcols)               ! Meridional wind stress at surface [ N/m2 ]
    real(r8), intent(in)    :: shflx(pcols)              ! Sensible heat flux at surface [ unit ? ]
    real(r8), intent(in)    :: qflx(pcols)               ! Water vapor flux at surface [ unit ? ]
    real(r8), intent(in)    :: kvm_in(pcols,pver+1)      ! kvm saved from last timestep [ m2/s ]
    real(r8), intent(in)    :: kvh_in(pcols,pver+1)      ! kvh saved from last timestep [ m2/s ]
    real(r8), intent(in)    :: ksrftms(pcols)            ! Surface drag coefficient of turbulent mountain stress [ unit ? ]

    ! ---------------- !
    ! Output Variables !
    ! ---------------- ! 

    real(r8), intent(out)   :: kvm_out(pcols,pver+1)     ! Eddy diffusivity for momentum [ m2/s ]
    real(r8), intent(out)   :: kvh_out(pcols,pver+1)     ! Eddy diffusivity for heat [ m2/s ]
    real(r8), intent(out)   :: kvq(pcols,pver+1)         ! Eddy diffusivity for constituents, moisture and tracers [ m2/s ] (note not having '_out')
    real(r8), intent(out)   :: ustar(pcols)              ! Surface friction velocity [ m/s ]
    real(r8), intent(out)   :: pblh(pcols)               ! PBL top height [ m ]
    real(r8), intent(out)   :: cgh(pcols,pver+1)         ! Counter-gradient term for heat [ J/kg/m ]
    real(r8), intent(out)   :: cgs(pcols,pver+1)         ! Counter-gradient star [ cg/flux ]
    real(r8), intent(out)   :: tpert(pcols)              ! Convective temperature excess [ K ]
    real(r8), intent(out)   :: qpert(pcols)              ! Convective humidity excess [ kg/kg ]
    real(r8), intent(out)   :: wpert(pcols)              ! Turbulent velocity excess [ m/s ]
    real(r8), intent(out)   :: tke(pcols,pver+1)         ! Turbulent kinetic energy [ m2/s2 ]
    real(r8), intent(out)   :: bprod(pcols,pver+1)       ! Buoyancy production [ m2/s3 ] 
    real(r8), intent(out)   :: sprod(pcols,pver+1)       ! Shear production [ m2/s3 ] 
    real(r8), intent(out)   :: sfi(pcols,pver+1)         ! Interfacial layer saturation fraction [ fraction ]
    real(r8), intent(out)   :: turbtype(pcols,pver+1)    ! Turbulence type identifier at all interfaces [ no unit ]
    real(r8), intent(out)   :: sm_aw(pcols,pver+1)       ! Normalized Galperin instability function for momentum [ no unit ]
                                                         ! This is 1 when neutral condition (Ri=0), 4.964 for maximum unstable case, and 0 when Ri > Ricrit=0.19. 
    real(r8), intent(out)   :: ipbl(pcols)               ! If 1, PBL is CL, while if 0, PBL is STL.
    real(r8), intent(out)   :: kpblh(pcols)              ! Layer index containing PBL top within or at the base interface
    real(r8), intent(out)   :: wstarPBL(pcols)           ! Convective velocity within PBL [ m/s ]

    ! ---------------------- !
    ! Input-Output Variables !
    ! ---------------------- ! 

    real(r8), intent(inout) :: tauresx(pcols)            ! Residual stress to be added in vdiff to correct for turb
    real(r8), intent(inout) :: tauresy(pcols)            ! Stress mismatch between sfc and atm accumulated in prior timesteps

    ! --------------- !
    ! Local Variables !
    ! --------------- !

    integer                    icol
    integer                    i, k, iturb, status
    integer,  external      :: qsat
    character(128)          :: errstring                 ! Error status for compute_vdiff

    real(r8)                :: tautotx(pcols)            ! Total stress including tms
    real(r8)                :: tautoty(pcols)            ! Total stress including tms
    real(r8)                :: kvf(pcols,pver+1)         ! Free atmospheric eddy diffusivity [ m2/s ]
    real(r8)                :: kvm(pcols,pver+1)         ! Eddy diffusivity for momentum [ m2/s ]
    real(r8)                :: kvh(pcols,pver+1)         ! Eddy diffusivity for heat [ m2/s ]
    real(r8)                :: kvm_preo(pcols,pver+1)    ! Eddy diffusivity for momentum [ m2/s ]
    real(r8)                :: kvh_preo(pcols,pver+1)    ! Eddy diffusivity for heat [ m2/s ]
    real(r8)                :: kvm_pre(pcols,pver+1)     ! Eddy diffusivity for momentum [ m2/s ]
    real(r8)                :: kvh_pre(pcols,pver+1)     ! Eddy diffusivity for heat [ m2/s ]
    real(r8)                :: errorPBL(pcols)           ! Error function showing whether PBL produced convergent solution or not. [ unit ? ]
    real(r8)                :: s2(pcols,pver)            ! Shear squared, defined at interfaces except surface [ s-2 ]
    real(r8)                :: n2(pcols,pver)            ! Buoyancy frequency, defined at interfaces except surface [ s-2 ]
    real(r8)                :: ri(pcols,pver)            ! Richardson number, 'n2/s2', defined at interfaces except surface [ s-2 ]
    real(r8)                :: pblhp(pcols)              ! PBL top pressure [ Pa ]
    real(r8)                :: minpblh(pcols)            ! Minimum PBL height based on surface stress

    real(r8)                :: qt(pcols,pver)            ! Total specific humidity [ kg/kg ]
    real(r8)                :: sfuh(pcols,pver)          ! Saturation fraction in upper half-layer [ fraction ]
    real(r8)                :: sflh(pcols,pver)          ! Saturation fraction in lower half-layer [ fraction ]
    real(r8)                :: sl(pcols,pver)            ! Liquid water static energy [ J/kg ]
    real(r8)                :: slv(pcols,pver)           ! Liquid water virtual static energy [ J/kg ]
    real(r8)                :: slslope(pcols,pver)       ! Slope of 'sl' in each layer
    real(r8)                :: qtslope(pcols,pver)       ! Slope of 'qt' in each layer
    real(r8)                :: rrho(pcols)               ! Density at the lowest layer
    real(r8)                :: qvfd(pcols,pver)          ! Specific humidity for diffusion [ kg/kg ]
    real(r8)                :: tfd(pcols,pver)           ! Temperature for diffusion [ K ]
    real(r8)                :: slfd(pcols,pver)          ! Liquid static energy [ J/kg ]
    real(r8)                :: qtfd(pcols,pver)          ! Total specific humidity [ kg/kg ] 
    real(r8)                :: qlfd(pcols,pver)          ! Liquid water specific humidity for diffusion [ kg/kg ]
    real(r8)                :: ufd(pcols,pver)           ! U-wind for diffusion [ m/s ]
    real(r8)                :: vfd(pcols,pver)           ! V-wind for diffusion [ m/s ]

    ! Buoyancy coefficients : w'b' = ch * w'sl' + cm * w'qt'

    real(r8)                :: chu(pcols,pver+1)         ! Heat buoyancy coef for dry states, defined at each interface, finally.
    real(r8)                :: chs(pcols,pver+1)         ! Heat buoyancy coef for sat states, defined at each interface, finally. 
    real(r8)                :: cmu(pcols,pver+1)         ! Moisture buoyancy coef for dry states, defined at each interface, finally.
    real(r8)                :: cms(pcols,pver+1)         ! Moisture buoyancy coef for sat states, defined at each interface, finally. 

    real(r8)                :: jnk1d(pcols)
    real(r8)                :: jnk2d(pcols,pver+1)  
    real(r8)                :: zero(pcols)
    real(r8)                :: zero2d(pcols,pver+1)
    real(r8)                :: es(1)                     ! Saturation vapor pressure
    real(r8)                :: qs(1)                     ! Saturation specific humidity
    real(r8)                :: gam(1)                    ! (L/cp)*dqs/dT
    real(r8)                :: ep2, templ, temps

    ! ------------------------------- !
    ! Variables for diagnostic output !
    ! ------------------------------- !

    real(r8)                :: tkes(pcols)               ! TKE at surface interface [ m2/s2 ]
    real(r8)                :: kbase_o(pcols,ncvmax)     ! Original external base interface index of CL from 'exacol'
    real(r8)                :: ktop_o(pcols,ncvmax)      ! Original external top  interface index of CL from 'exacol'
    real(r8)                :: ncvfin_o(pcols)           ! Original number of CLs from 'exacol'
    real(r8)                :: kbase_mg(pcols,ncvmax)    ! 'kbase' after extending-merging from 'zisocl'
    real(r8)                :: ktop_mg(pcols,ncvmax)     ! 'ktop' after extending-merging from 'zisocl'
    real(r8)                :: ncvfin_mg(pcols)          ! 'ncvfin' after extending-merging from 'zisocl'
    real(r8)                :: kbase_f(pcols,ncvmax)     ! Final 'kbase' after extending-merging & including SRCL
    real(r8)                :: ktop_f(pcols,ncvmax)      ! Final 'ktop' after extending-merging & including SRCL
    real(r8)                :: ncvfin_f(pcols)           ! Final 'ncvfin' after extending-merging & including SRCL
    real(r8)                :: wet(pcols,ncvmax)         ! Entrainment rate at the CL top  [ m/s ] 
    real(r8)                :: web(pcols,ncvmax)         ! Entrainment rate at the CL base [ m/s ]. Set to zero if CL is based at surface.
    real(r8)                :: jtbu(pcols,ncvmax)        ! Buoyancy jump across the CL top  [ m/s2 ]  
    real(r8)                :: jbbu(pcols,ncvmax)        ! Buoyancy jump across the CL base [ m/s2 ]  
    real(r8)                :: evhc(pcols,ncvmax)        ! Evaporative enhancement factor at the CL top
    real(r8)                :: jt2slv(pcols,ncvmax)      ! Jump of slv ( across two layers ) at CL top used only for evhc [ J/kg ]
    real(r8)                :: n2ht(pcols,ncvmax)        ! n2 defined at the CL top  interface but using sfuh(kt)   instead of sfi(kt) [ s-2 ] 
    real(r8)                :: n2hb(pcols,ncvmax)        ! n2 defined at the CL base interface but using sflh(kb-1) instead of sfi(kb) [ s-2 ]
    real(r8)                :: lwp(pcols,ncvmax)         ! LWP in the CL top layer [ kg/m2 ]
    real(r8)                :: opt_depth(pcols,ncvmax)   ! Optical depth of the CL top layer
    real(r8)                :: radinvfrac(pcols,ncvmax)  ! Fraction of radiative cooling confined in the top portion of CL top layer
    real(r8)                :: radf(pcols,ncvmax)        ! Buoyancy production at the CL top due to LW radiative cooling [ m2/s3 ]
    real(r8)                :: wstar(pcols,ncvmax)       ! Convective velocity in each CL [ m/s ]
    real(r8)                :: wstar3fact(pcols,ncvmax)  ! Enhancement of 'wstar3' due to entrainment (inverse) [ no unit ]
    real(r8)                :: ebrk(pcols,ncvmax)        ! Net mean TKE of CL including entrainment effect [ m2/s2 ]
    real(r8)                :: wbrk(pcols,ncvmax)        ! Net mean normalized TKE (W) of CL, 'ebrk/b1' including entrainment effect [ m2/s2 ]
    real(r8)                :: lbrk(pcols,ncvmax)        ! Energetic internal thickness of CL [m]
    real(r8)                :: ricl(pcols,ncvmax)        ! CL internal mean Richardson number
    real(r8)                :: ghcl(pcols,ncvmax)        ! Half of normalized buoyancy production of CL
    real(r8)                :: shcl(pcols,ncvmax)        ! Galperin instability function of heat-moisture of CL
    real(r8)                :: smcl(pcols,ncvmax)        ! Galperin instability function of mementum of CL
    real(r8)                :: ghi(pcols,pver+1)         ! Half of normalized buoyancy production at all interfaces
    real(r8)                :: shi(pcols,pver+1)         ! Galperin instability function of heat-moisture at all interfaces
    real(r8)                :: smi(pcols,pver+1)         ! Galperin instability function of heat-moisture at all interfaces
    real(r8)                :: rii(pcols,pver+1)         ! Interfacial Richardson number defined at all interfaces
    real(r8)                :: lengi(pcols,pver+1)       ! Turbulence length scale at all interfaces [ m ]
    real(r8)                :: wcap(pcols,pver+1)        ! Normalized TKE at all interfaces [ m2/s2 ]

    ! ---------- !
    ! Initialize !
    ! ---------- !

    zero(:)     = 0._r8
    zero2d(:,:) = 0._r8

    ! ----------------------- !
    ! Main Computation Begins ! 
    ! ----------------------- !

    ufd(:ncol,:)  = u(:ncol,:)
    vfd(:ncol,:)  = v(:ncol,:)
    tfd(:ncol,:)  = t(:ncol,:)
    qvfd(:ncol,:) = qv(:ncol,:)
    qlfd(:ncol,:) = ql(:ncol,:)
    
    do iturb = 1, nturb

     ! Compute total stress by including 'tms'.
     ! Here, in computing 'tms', we can use either iteratively changed 'ufd,vfd' or the
     ! initially given 'u,v' to the PBL scheme. Note that normal stress, 'taux, tauy'
     ! are not changed by iteration. In order to treat 'tms' in a fully implicit way,
     ! I am using updated wind, here.

       tautotx(:ncol) = taux(:ncol) - ksrftms(:ncol) * ufd(:ncol,pver)
       tautoty(:ncol) = tauy(:ncol) - ksrftms(:ncol) * vfd(:ncol,pver)

     ! Calculate (qt,sl,n2,s2,ri) from a given set of (t,qv,ql,qi,u,v)

       call trbintd( &
                     pcols    , pver    , ncol  , z       , ufd     , vfd     , tfd   , pmid    , &
                     tautotx  , tautoty , ustar , rrho    , s2      , n2      , ri    , zi      , &
                     pi       , cldn    , qtfd  , qvfd    , qlfd    , qi      , sfi   , sfuh    , &
                     sflh     , slfd    , slv   , slslope , qtslope , chs     , chu   , cms     , &
                     cmu      , minpblh , qsat )

     ! Save initial (i.e., before iterative diffusion) profile of (qt,sl) at each iteration.         
     ! Only necessary for (qt,sl) not (u,v) because (qt,sl) are newly calculated variables. 

       if( iturb .eq. 1 ) then
           qt(:ncol,:) = qtfd(:ncol,:)
           sl(:ncol,:) = slfd(:ncol,:)
       endif

     ! Get free atmosphere exchange coefficients. This 'kvf' is not used in UW moist PBL scheme
#ifndef WRF_PORT
       call austausch_atm( pcols, pver, ncol, ri, s2, kvf )
#else
       if(use_kvf)call austausch_atm( pcols, pver, ncol, ri, s2, kvf )
#endif

     ! Initialize kvh/kvm to send to caleddy, depending on model timestep and iteration number
     ! This is necessary for 'wstar-based' entrainment closure.

       if( iturb .eq. 1 ) then
           if( kvinit ) then
           ! First iteration of first model timestep : Use free tropospheric value or zero.
             if( use_kvf ) then
                 kvh(:ncol,:) = kvf(:ncol,:)
                 kvm(:ncol,:) = kvf(:ncol,:)
             else
                 kvh(:ncol,:) = 0._r8
                 kvm(:ncol,:) = 0._r8
             endif
           else
             ! First iteration on any model timestep except the first : Use value from previous timestep
             kvh(:ncol,:) = kvh_in(:ncol,:)
             kvm(:ncol,:) = kvm_in(:ncol,:)
           endif
       else
        ! Not the first iteration : Use from previous iteration
          kvh(:ncol,:) = kvh_out(:ncol,:)
          kvm(:ncol,:) = kvm_out(:ncol,:)
       endif

     ! Calculate eddy diffusivity (kvh_out,kvm_out) and (tke,bprod,sprod) using
     ! a given (kvh,kvm) which are used only for initializing (bprod,sprod)  at
     ! the first part of caleddy. (bprod,sprod) are fully updated at the end of
     ! caleddy after calculating (kvh_out,kvm_out) 

       call caleddy( pcols     , pver      , ncol      ,                     &
                     slfd      , qtfd      , qlfd      , slv      ,ufd     , &
                     vfd       , pi        , z         , zi       ,          &
                     qflx      , shflx     , slslope   , qtslope  ,          &
                     chu       , chs       , cmu       , cms      ,sfuh    , &
                     sflh      , n2        , s2        , ri       ,rrho    , &
                     pblh      , ustar     ,                                 &
                     kvh       , kvm       , kvh_out   , kvm_out  ,          &
                     tpert     , qpert     , qrl       , kvf      , tke    , &
                     wstarent  , bprod     , sprod     , minpblh  , wpert  , &
                     tkes      , turbtype  , sm_aw     ,                     & 
                     kbase_o   , ktop_o    , ncvfin_o  ,                     &
                     kbase_mg  , ktop_mg   , ncvfin_mg ,                     &                  
                     kbase_f   , ktop_f    , ncvfin_f  ,                     &                  
                     wet       , web       , jtbu      , jbbu     ,          &
                     evhc      , jt2slv    , n2ht      , n2hb     ,          & 
                     lwp       , opt_depth , radinvfrac, radf     ,          &
                     wstar     , wstar3fact,                                 &
                     ebrk      , wbrk      , lbrk      , ricl     , ghcl   , & 
                     shcl      , smcl      , ghi       , shi      , smi    , &
                     rii       , lengi     , wcap      , pblhp    , cldn   , &
                     ipbl      , kpblh     , wsedl )

     ! Calculate errorPBL to check whether PBL produced convergent solutions or not.

       if( iturb .eq. nturb ) then
           do i = 1, ncol
              errorPBL(i) = 0._r8 
              do k = 1, pver
                 errorPBL(i) = errorPBL(i) + ( kvh(i,k) - kvh_out(i,k) )**2 
              end do 
              errorPBL(i) = sqrt(errorPBL(i)/pver)
           end do
       end if

     ! Eddy diffusivities which will be used for the initialization of (bprod,
     ! sprod) in 'caleddy' at the next iteration step.

       if( iturb .gt. 1 .and. iturb .lt. nturb ) then
           kvm_out(:ncol,:) = lambda * kvm_out(:ncol,:) + ( 1._r8 - lambda ) * kvm(:ncol,:)
           kvh_out(:ncol,:) = lambda * kvh_out(:ncol,:) + ( 1._r8 - lambda ) * kvh(:ncol,:)
       endif

     ! Set nonlocal terms to zero for flux diagnostics, since not used by caleddy.

       cgh(:ncol,:) = 0._r8
       cgs(:ncol,:) = 0._r8      

       if( iturb .lt. nturb ) then

         ! Each time we diffuse the original state

           slfd(:ncol,:)  = sl(:ncol,:)
           qtfd(:ncol,:)  = qt(:ncol,:)
           ufd(:ncol,:)   = u(:ncol,:)
           vfd(:ncol,:)   = v(:ncol,:)

         ! Diffuse initial profile of each time step using a given (kvh_out,kvm_out)
         ! In the below 'compute_vdiff', (slfd,qtfd,ufd,vfd) are 'inout' variables.

           call compute_vdiff( lchnk   ,                                                  &
                               pcols   , pver     , 1        , ncol         , pmid      , &
                               pi      , rpdel    , t        , ztodt        , taux      , &
                               tauy    , shflx    , qflx     , ntop_turb    , nbot_turb , &
                               kvh_out , kvm_out  , kvh_out  , cgs          , cgh       , &
                               zi      , ksrftms  , zero     , fieldlist_wet,             &
                               ufd     , vfd      , qtfd     , slfd         ,             &
                               jnk1d   , jnk1d    , jnk2d    , jnk1d        , errstring , &
                               tauresx , tauresy  , 0        , .false. )

         ! Retrieve (tfd,qvfd,qlfd) from (slfd,qtfd) in order to 
         ! use 'trbintd' at the next iteration.
          
          do k = 1, pver
             do i = 1, ncol
              ! ----------------------------------------------------- ! 
              ! Compute the condensate 'qlfd' in the updated profiles !
              ! ----------------------------------------------------- !  
              ! Option.1 : Assume grid-mean condensate is homogeneously diffused by the moist turbulence scheme.
              !            This should bs used if 'pseudodiff = .false.' in vertical_diffusion.F90.
              ! Modification : Need to be check whether below is correct in the presence of ice, qi.       
              !                I should understand why the variation of ice, qi is neglected during diffusion.
                templ     = ( slfd(i,k) - g*z(i,k) ) / cpair
                status    =   qsat( templ, pmid(i,k), es(1), qs(1), gam(1), 1 )
                ep2       =  .622_r8 
                temps     =   templ + ( qtfd(i,k) - qs(1) ) / ( cpair / latvap + latvap * qs(1) / ( rair * templ**2 ) )
                status    =   qsat( temps, pmid(i,k), es(1), qs(1), gam(1), 1 )
                qlfd(i,k) =   max( qtfd(i,k) - qi(i,k) - qs(1) ,0._r8 )
              ! Option.2 : Assume condensate is not diffused by the moist turbulence scheme. 
              !            This should bs used if 'pseudodiff = .true.'  in vertical_diffusion.F90.       
              ! qlfd(i,k) = ql(i,k)
              ! ----------------------------- !
              ! Compute the other 'qvfd, tfd' ! 
              ! ----------------------------- !
                qvfd(i,k) = max( 0._r8, qtfd(i,k) - qi(i,k) - qlfd(i,k) )
                tfd(i,k)  = ( slfd(i,k) + latvap * qlfd(i,k) + latsub * qi(i,k) - g*z(i,k)) / cpair
             end do
          end do
       endif

     ! Debug 
     ! icol = phys_debug_col(lchnk) 
     ! if( icol > 0 .and. get_nstep() .ge. 1 ) then
     !     write(iulog,*) ' '
     !     write(iulog,*) 'eddy_diff debug at the end of iteration' 
     !     write(iulog,*) 't,     qv,     ql,     cld,     u,     v'
     !     do k = pver-3, pver
     !        write (iulog,*) k, tfd(icol,k), qvfd(icol,k), qlfd(icol,k), cldn(icol,k), ufd(icol,k), vfd(icol,k)
     !     end do
     ! endif
     ! Debug

    end do  ! End of 'iturb' iteration

    kvq(:ncol,:) = kvh_out(:ncol,:)

  ! Compute 'wstar' within the PBL for use in the future convection scheme.

    do i = 1, ncol
       if( ipbl(i) .eq. 1._r8 ) then 
           wstarPBL(i) = max( 0._r8, wstar(i,1) )
       else
           wstarPBL(i) = 0._r8
       endif
    end do

    ! --------------------------------------------------------------- !
    ! Writing for detailed diagnostic analysis of UW moist PBL scheme !
    ! --------------------------------------------------------------- !

    call outfld( 'UW_errorPBL',    errorPBL,   pcols,   lchnk )

    call outfld( 'UW_n2',          n2,         pcols,   lchnk )
    call outfld( 'UW_s2',          s2,         pcols,   lchnk )
    call outfld( 'UW_ri',          ri,         pcols,   lchnk )

    call outfld( 'UW_sfuh',        sfuh,       pcols,   lchnk )
    call outfld( 'UW_sflh',        sflh,       pcols,   lchnk )
    call outfld( 'UW_sfi',         sfi,        pcols,   lchnk )

    call outfld( 'UW_cldn',        cldn,       pcols,   lchnk )
    call outfld( 'UW_qrl',         qrl,        pcols,   lchnk )
    call outfld( 'UW_ql',          qlfd,       pcols,   lchnk )

    call outfld( 'UW_chu',         chu,        pcols,   lchnk )
    call outfld( 'UW_chs',         chs,        pcols,   lchnk )
    call outfld( 'UW_cmu',         cmu,        pcols,   lchnk )
    call outfld( 'UW_cms',         cms,        pcols,   lchnk )

    call outfld( 'UW_tke',         tke,        pcols,   lchnk )
    call outfld( 'UW_wcap',        wcap,       pcols,   lchnk )
    call outfld( 'UW_bprod',       bprod,      pcols,   lchnk )
    call outfld( 'UW_sprod',       sprod,      pcols,   lchnk )

    call outfld( 'UW_kvh',         kvh_out,    pcols,   lchnk )
    call outfld( 'UW_kvm',         kvm_out,    pcols,   lchnk )

    call outfld( 'UW_pblh',        pblh,       pcols,   lchnk )
    call outfld( 'UW_pblhp',       pblhp,      pcols,   lchnk )
    call outfld( 'UW_tpert',       tpert,      pcols,   lchnk )
    call outfld( 'UW_qpert',       qpert,      pcols,   lchnk )
    call outfld( 'UW_wpert',       wpert,      pcols,   lchnk )

    call outfld( 'UW_ustar',       ustar,      pcols,   lchnk )
    call outfld( 'UW_tkes',        tkes,       pcols,   lchnk )
    call outfld( 'UW_minpblh',     minpblh,    pcols,   lchnk )

    call outfld( 'UW_turbtype',    turbtype,   pcols,   lchnk )

    call outfld( 'UW_kbase_o',     kbase_o,    pcols,   lchnk )
    call outfld( 'UW_ktop_o',      ktop_o,     pcols,   lchnk )
    call outfld( 'UW_ncvfin_o',    ncvfin_o,   pcols,   lchnk )

    call outfld( 'UW_kbase_mg',    kbase_mg,   pcols,   lchnk )
    call outfld( 'UW_ktop_mg',     ktop_mg,    pcols,   lchnk )
    call outfld( 'UW_ncvfin_mg',   ncvfin_mg,  pcols,   lchnk )

    call outfld( 'UW_kbase_f',     kbase_f,    pcols,   lchnk )
    call outfld( 'UW_ktop_f',      ktop_f,     pcols,   lchnk )
    call outfld( 'UW_ncvfin_f',    ncvfin_f,   pcols,   lchnk ) 

    call outfld( 'UW_wet',         wet,        pcols,   lchnk )
    call outfld( 'UW_web',         web,        pcols,   lchnk )
    call outfld( 'UW_jtbu',        jtbu,       pcols,   lchnk )
    call outfld( 'UW_jbbu',        jbbu,       pcols,   lchnk )
    call outfld( 'UW_evhc',        evhc,       pcols,   lchnk )
    call outfld( 'UW_jt2slv',      jt2slv,     pcols,   lchnk )
    call outfld( 'UW_n2ht',        n2ht,       pcols,   lchnk )
    call outfld( 'UW_n2hb',        n2hb,       pcols,   lchnk )
    call outfld( 'UW_lwp',         lwp,        pcols,   lchnk )
    call outfld( 'UW_optdepth',    opt_depth,  pcols,   lchnk )
    call outfld( 'UW_radfrac',     radinvfrac, pcols,   lchnk )
    call outfld( 'UW_radf',        radf,       pcols,   lchnk )
    call outfld( 'UW_wstar',       wstar,      pcols,   lchnk )
    call outfld( 'UW_wstar3fact',  wstar3fact, pcols,   lchnk )
    call outfld( 'UW_ebrk',        ebrk,       pcols,   lchnk )
    call outfld( 'UW_wbrk',        wbrk,       pcols,   lchnk )
    call outfld( 'UW_lbrk',        lbrk,       pcols,   lchnk )
    call outfld( 'UW_ricl',        ricl,       pcols,   lchnk )
    call outfld( 'UW_ghcl',        ghcl,       pcols,   lchnk )
    call outfld( 'UW_shcl',        shcl,       pcols,   lchnk )
    call outfld( 'UW_smcl',        smcl,       pcols,   lchnk )

    call outfld( 'UW_gh',          ghi,        pcols,   lchnk )
    call outfld( 'UW_sh',          shi,        pcols,   lchnk )
    call outfld( 'UW_sm',          smi,        pcols,   lchnk )
    call outfld( 'UW_ria',         rii,        pcols,   lchnk )
    call outfld( 'UW_leng',        lengi,      pcols,   lchnk )

    return
    
  end subroutine compute_eddy_diff

  !=============================================================================== !
  !                                                                                !
  !=============================================================================== !
  
  subroutine sfdiag( pcols   , pver    , ncol    , qt      , ql      , sl      , &
                     pi      , pm      , zi      , cld     , sfi     , sfuh    , &
                     sflh    , slslope , qtslope , qsat )
    !----------------------------------------------------------------------- ! 
    !                                                                        !
    ! Purpose: Interface for calculating saturation fractions  at upper and  ! 
    !          lower-half layers, & interfaces for use by turbulence scheme  !
    !                                                                        !
    ! Method : Various but 'l' should be chosen for consistency.             !
    !                                                                        ! 
    ! Author : B. Stevens and C. Bretherton (August 2000)                    !
    !          Sungsu Park. August 2006.                                     !
    !                       May.   2008.                                     ! 
    !                                                                        !  
    ! S.Park : The computed saturation fractions are repeatedly              !
    !          used to compute buoyancy coefficients in'trbintd' & 'caleddy'.!  
    !----------------------------------------------------------------------- !

    implicit none       

    ! --------------- !
    ! Input arguments !
    ! --------------- !

    integer,  external    :: qsat

    integer,  intent(in)  :: pcols               ! Number of atmospheric columns   
    integer,  intent(in)  :: pver                ! Number of atmospheric layers   
    integer,  intent(in)  :: ncol                ! Number of atmospheric columns   

    real(r8), intent(in)  :: sl(pcols,pver)      ! Liquid water static energy [ J/kg ]
    real(r8), intent(in)  :: qt(pcols,pver)      ! Total water specific humidity [ kg/kg ]
    real(r8), intent(in)  :: ql(pcols,pver)      ! Liquid water specific humidity [ kg/kg ]
    real(r8), intent(in)  :: pi(pcols,pver+1)    ! Interface pressures [ Pa ]
    real(r8), intent(in)  :: pm(pcols,pver)      ! Layer mid-point pressures [ Pa ]
    real(r8), intent(in)  :: zi(pcols,pver+1)    ! Interface heights [ m ]
    real(r8), intent(in)  :: cld(pcols,pver)     ! Stratiform cloud fraction [ fraction ]
    real(r8), intent(in)  :: slslope(pcols,pver) ! Slope of 'sl' in each layer
    real(r8), intent(in)  :: qtslope(pcols,pver) ! Slope of 'qt' in each layer

    ! ---------------- !
    ! Output arguments !
    ! ---------------- !

    real(r8), intent(out) :: sfi(pcols,pver+1)   ! Interfacial layer saturation fraction [ fraction ]
    real(r8), intent(out) :: sfuh(pcols,pver)    ! Saturation fraction in upper half-layer [ fraction ]
    real(r8), intent(out) :: sflh(pcols,pver)    ! Saturation fraction in lower half-layer [ fraction ]

    ! --------------- !
    ! Local Variables !
    ! --------------- !

    integer               :: i                   ! Longitude index
    integer               :: k                   ! Vertical index
    integer               :: km1                 ! k-1
    integer               :: status              ! Status returned by function calls
    real(r8)              :: sltop, slbot        ! sl at top/bot of grid layer
    real(r8)              :: qttop, qtbot        ! qt at top/bot of grid layer
    real(r8)              :: tltop(1), tlbot(1)  ! Liquid water temperature at top/bot of grid layer
    real(r8)              :: qxtop, qxbot        ! Sat excess at top/bot of grid layer
    real(r8)              :: qxm                 ! Sat excess at midpoint
    real(r8)              :: es(1)               ! Saturation vapor pressure
    real(r8)              :: qs(1)               ! Saturation spec. humidity
    real(r8)              :: gam(1)              ! (L/cp)*dqs/dT
    real(r8)              :: cldeff(pcols,pver)  ! Effective Cloud Fraction [ fraction ]

    ! ----------------------- !
    ! Main Computation Begins ! 
    ! ----------------------- !

    sfi(1:ncol,:)    = 0._r8
    sfuh(1:ncol,:)   = 0._r8
    sflh(1:ncol,:)   = 0._r8
    cldeff(1:ncol,:) = 0._r8

    select case (sftype)
    case ('d')
       ! ----------------------------------------------------------------------- !
       ! Simply use the given stratus fraction ('horizontal' cloud partitioning) !
       ! ----------------------------------------------------------------------- !
       do k = ntop_turb + 1, nbot_turb
          km1 = k - 1
          do i = 1, ncol
             sfuh(i,k) = cld(i,k)
             sflh(i,k) = cld(i,k)
             sfi(i,k)  = 0.5_r8 * ( sflh(i,km1) + min( sflh(i,km1), sfuh(i,k) ) )
          end do
       end do
       do i = 1, ncol
          sfi(i,pver+1) = sflh(i,pver) 
       end do
    case ('l')
       ! ------------------------------------------ !
       ! Use modified stratus fraction partitioning !
       ! ------------------------------------------ !
       do k = ntop_turb + 1, nbot_turb
          km1 = k - 1
          do i = 1, ncol
             cldeff(i,k) = cld(i,k)
             sfuh(i,k)   = cld(i,k)
             sflh(i,k)   = cld(i,k)
             if( ql(i,k) .lt. qmin ) then
                 sfuh(i,k) = 0._r8
                 sflh(i,k) = 0._r8
             end if
           ! Modification : The contribution of ice should be carefully considered.
             if( choice_evhc .eq. 'ramp' .or. choice_radf .eq. 'ramp' ) then 
                 cldeff(i,k) = cld(i,k) * min( ql(i,k) / qmin, 1._r8 )
                 sfuh(i,k)   = cldeff(i,k)
                 sflh(i,k)   = cldeff(i,k)
             elseif( choice_evhc .eq. 'maxi' .or. choice_radf .eq. 'maxi' ) then 
                 cldeff(i,k) = cld(i,k)
                 sfuh(i,k)   = cldeff(i,k)
                 sflh(i,k)   = cldeff(i,k)
             endif
           ! At the stratus top, take the minimum interfacial saturation fraction
             sfi(i,k) = 0.5_r8 * ( sflh(i,km1) + min( sfuh(i,k), sflh(i,km1) ) )
           ! Modification : Currently sfi at the top and surface interfaces are set to be zero.
           !                Also, sfuh and sflh in the top model layer is set to be zero.
           !                However, I may need to set 
           !                         do i = 1, ncol
           !                            sfi(i,pver+1) = sflh(i,pver) 
           !                         end do
           !                for treating surface-based fog. 
           ! OK. I added below block similar to the other cases.
          end do
       end do
       do i = 1, ncol
          sfi(i,pver+1) = sflh(i,pver)
       end do
    case ('u')
       ! ------------------------------------------------------------------------- !
       ! Use unsaturated buoyancy - since sfi, sfuh, sflh have already been zeroed !
       ! nothing more need be done for this case.                                  !
       ! ------------------------------------------------------------------------- !
    case ('z')
       ! ------------------------------------------------------------------------- !
       ! Calculate saturation fraction based on whether the air just above or just !
       ! below the interface is saturated, i.e. with vertical cloud partitioning.  !
       ! The saturation fraction of the interfacial layer between mid-points k and !
       ! k+1 is computed by averaging the saturation fraction   of the half-layers !
       ! above and below the interface,  with a special provision   for cloud tops !
       ! (more cloud in the half-layer below than in the half-layer above).In each !
       ! half-layer, vertical partitioning of  cloud based on the slopes diagnosed !
       ! above is used.     Loop down through the layers, computing the saturation !
       ! fraction in each half-layer (sfuh for upper half, sflh for lower half).   !
       ! Once sfuh(i,k) is computed, use with sflh(i,k-1) to determine  saturation !
       ! fraction sfi(i,k) for interfacial layer k-0.5.                            !
       ! This is 'not' chosen for full consistent treatment of stratus fraction in !
       ! all physics schemes.                                                      !
       ! ------------------------------------------------------------------------- !
       do k = ntop_turb + 1, nbot_turb
          km1 = k - 1
          do i = 1, ncol
           ! Compute saturation excess at the mid-point of layer k
             sltop    = sl(i,k) + slslope(i,k) * ( pi(i,k) - pm(i,k) )      
             qttop    = qt(i,k) + qtslope(i,k) * ( pi(i,k) - pm(i,k) )
             tltop(1) = ( sltop - g * zi(i,k) ) / cpair 
             status   = qsat( tltop(1), pi(i,k), es(1), qs(1), gam(1), 1 )
             qxtop    = qttop - qs(1) 
             slbot    = sl(i,k) + slslope(i,k) * ( pi(i,k+1) - pm(i,k) )      
             qtbot    = qt(i,k) + qtslope(i,k) * ( pi(i,k+1) - pm(i,k) )
             tlbot(1) = ( slbot - g * zi(i,k+1) ) / cpair 
             status   = qsat( tlbot(1), pi(i,k+1), es(1), qs(1), gam(1), 1 )
             qxbot    = qtbot - qs(1) 
             qxm      = qxtop + ( qxbot - qxtop ) * ( pm(i,k) - pi(i,k) ) / ( pi(i,k+1) - pi(i,k) )
           ! Find the saturation fraction sfuh(i,k) of the upper half of layer k.
             if( ( qxtop .lt. 0._r8 ) .and. ( qxm .lt. 0._r8 ) ) then
                   sfuh(i,k) = 0._r8 
             else if( ( qxtop .gt. 0._r8 ) .and. ( qxm .gt. 0._r8 ) ) then
                   sfuh(i,k) = 1._r8  
             else ! Either qxm < 0 and qxtop > 0 or vice versa
                   sfuh(i,k) = max( qxtop, qxm ) / abs( qxtop - qxm )
             end if
           ! Combine with sflh(i) (still for layer k-1) to get interfac layer saturation fraction
             sfi(i,k) = 0.5_r8 * ( sflh(i,k-1) + min( sflh(i,k-1), sfuh(i,k) ) )
           ! Update sflh to be for the lower half of layer k.             
             if( ( qxbot .lt. 0._r8 ) .and. ( qxm .lt. 0._r8 ) ) then
                   sflh(i,k) = 0._r8 
             else if( ( qxbot .gt. 0._r8 ) .and. ( qxm .gt. 0._r8 ) ) then
                   sflh(i,k) = 1._r8 
             else ! Either qxm < 0 and qxbot > 0 or vice versa
                   sflh(i,k) = max( qxbot, qxm ) / abs( qxbot - qxm )
             end if
          end do  ! i
       end do ! k
       do i = 1, ncol
          sfi(i,pver+1) = sflh(i,pver)  ! Saturation fraction in the lowest half-layer. 
       end do
    end select

  return
  end subroutine sfdiag
  
  !=============================================================================== !
  !                                                                                !
  !=============================================================================== !
 
  subroutine trbintd( pcols   , pver    , ncol    ,                               &
                      z       , u       , v       ,                               &
                      t       , pmid    , taux    ,                               &
                      tauy    , ustar   , rrho    ,                               &
                      s2      , n2      , ri      ,                               &
                      zi      , pi      , cld     ,                               &
                      qt      , qv      , ql      , qi      , sfi     , sfuh    , &
                      sflh    , sl      , slv     , slslope , qtslope ,           &
                      chs     , chu     , cms     , cmu     , minpblh , qsat )
    !----------------------------------------------------------------------- !
    ! Purpose: Calculate buoyancy coefficients at all interfaces including   !
    !          surface. Also, computes the profiles of ( sl,qt,n2,s2,ri ).   !
    !          Note that (n2,s2,ri) are defined at each interfaces except    !
    !          surface.                                                      !
    !                                                                        !
    ! Author: B. Stevens  ( Extracted from pbldiff, August, 2000 )           !
    !         Sungsu Park ( August 2006, May. 2008 )                         !
    !----------------------------------------------------------------------- !

    implicit none

    ! --------------- !
    ! Input arguments !
    ! --------------- !

    integer,  intent(in)  :: pcols                            ! Number of atmospheric columns   
    integer,  intent(in)  :: pver                             ! Number of atmospheric layers   
    integer,  intent(in)  :: ncol                             ! Number of atmospheric columns
    real(r8), intent(in)  :: z(pcols,pver)                    ! Layer mid-point height above surface [ m ]
    real(r8), intent(in)  :: u(pcols,pver)                    ! Layer mid-point u [ m/s ]
    real(r8), intent(in)  :: v(pcols,pver)                    ! Layer mid-point v [ m/s ]
    real(r8), intent(in)  :: t(pcols,pver)                    ! Layer mid-point temperature [ K ]
    real(r8), intent(in)  :: pmid(pcols,pver)                 ! Layer mid-point pressure [ Pa ]
    real(r8), intent(in)  :: taux(pcols)                      ! Surface u stress [ N/m2 ]
    real(r8), intent(in)  :: tauy(pcols)                      ! Surface v stress [ N/m2 ]
    real(r8), intent(in)  :: zi(pcols,pver+1)                 ! Interface height [ m ]
    real(r8), intent(in)  :: pi(pcols,pver+1)                 ! Interface pressure [ Pa ]
    real(r8), intent(in)  :: cld(pcols,pver)                  ! Stratus fraction
    real(r8), intent(in)  :: qv(pcols,pver)                   ! Water vapor specific humidity [ kg/kg ]
    real(r8), intent(in)  :: ql(pcols,pver)                   ! Liquid water specific humidity [ kg/kg ]
    real(r8), intent(in)  :: qi(pcols,pver)                   ! Ice water specific humidity [ kg/kg ]
    integer,  external    :: qsat

    ! ---------------- !
    ! Output arguments !
    ! ---------------- !

    real(r8), intent(out) :: ustar(pcols)                     ! Surface friction velocity [ m/s ]
    real(r8), intent(out) :: s2(pcols,pver)                   ! Interfacial ( except surface ) shear squared [ s-2 ]
    real(r8), intent(out) :: n2(pcols,pver)                   ! Interfacial ( except surface ) buoyancy frequency [ s-2 ]
    real(r8), intent(out) :: ri(pcols,pver)                   ! Interfacial ( except surface ) Richardson number, 'n2/s2'
 
    real(r8), intent(out) :: qt(pcols,pver)                   ! Total specific humidity [ kg/kg ]
    real(r8), intent(out) :: sfi(pcols,pver+1)                ! Interfacial layer saturation fraction [ fraction ]
    real(r8), intent(out) :: sfuh(pcols,pver)                 ! Saturation fraction in upper half-layer [ fraction ]
    real(r8), intent(out) :: sflh(pcols,pver)                 ! Saturation fraction in lower half-layer [ fraction ]
    real(r8), intent(out) :: sl(pcols,pver)                   ! Liquid water static energy [ J/kg ] 
    real(r8), intent(out) :: slv(pcols,pver)                  ! Liquid water virtual static energy [ J/kg ]
   
    real(r8), intent(out) :: chu(pcols,pver+1)                ! Heat buoyancy coef for dry states at all interfaces, finally. [ unit ? ]
    real(r8), intent(out) :: chs(pcols,pver+1)                ! heat buoyancy coef for sat states at all interfaces, finally. [ unit ? ]
    real(r8), intent(out) :: cmu(pcols,pver+1)                ! Moisture buoyancy coef for dry states at all interfaces, finally. [ unit ? ]
    real(r8), intent(out) :: cms(pcols,pver+1)                ! Moisture buoyancy coef for sat states at all interfaces, finally. [ unit ? ]
    real(r8), intent(out) :: slslope(pcols,pver)              ! Slope of 'sl' in each layer
    real(r8), intent(out) :: qtslope(pcols,pver)              ! Slope of 'qt' in each layer
    real(r8), intent(out) :: rrho(pcols)                      ! 1./bottom level density [ m3/kg ]
    real(r8), intent(out) :: minpblh(pcols)                   ! Minimum PBL height based on surface stress [ m ]
 
    ! --------------- !
    ! Local Variables !
    ! --------------- ! 

    integer               :: i                                ! Longitude index
    integer               :: k, km1                           ! Level index
    integer               :: status                           ! Status returned by function calls

    real(r8)              :: qs(pcols,pver)                   ! Saturation specific humidity
    real(r8)              :: es(pcols,pver)                   ! Saturation vapor pressure
    real(r8)              :: gam(pcols,pver)                  ! (l/cp)*(d(qs)/dT)
    real(r8)              :: rdz                              ! 1 / (delta z) between midpoints
    real(r8)              :: dsldz                            ! 'delta sl / delta z' at interface
    real(r8)              :: dqtdz                            ! 'delta qt / delta z' at interface
    real(r8)              :: ch                               ! 'sfi' weighted ch at the interface
    real(r8)              :: cm                               ! 'sfi' weighted cm at the interface
    real(r8)              :: bfact                            ! Buoyancy factor in n2 calculations
    real(r8)              :: product                          ! Intermediate vars used to find slopes
    real(r8)              :: dsldp_a, dqtdp_a                 ! Slopes across interface above 
    real(r8)              :: dsldp_b(pcols), dqtdp_b(pcols)   ! Slopes across interface below

    ! ----------------------- !
    ! Main Computation Begins !
    ! ----------------------- !

    ! Compute ustar, and kinematic surface fluxes from surface energy fluxes

    do i = 1, ncol
       rrho(i)    = rair * t(i,pver) / pmid(i,pver)
       ustar(i)   = max( sqrt( sqrt( taux(i)**2 + tauy(i)**2 ) * rrho(i) ), ustar_min )
       minpblh(i) = 100.0_r8 * ustar(i)                       ! By construction, 'minpblh' is larger than 1 [m] when 'ustar_min = 0.01'. 
    end do

    ! Calculate conservative scalars (qt,sl,slv) and buoyancy coefficients at the layer mid-points.
    ! Note that 'ntop_turb = 1', 'nbot_turb = pver'

    do k = ntop_turb, nbot_turb
       status = qsat( t(1,k), pmid(1,k), es(1,k), qs(1,k), gam(1,k), ncol )
       do i = 1, ncol
          qt(i,k)  = qv(i,k) + ql(i,k) + qi(i,k) 
          sl(i,k)  = cpair * t(i,k) + g * z(i,k) - latvap * ql(i,k) - latsub * qi(i,k)
          slv(i,k) = sl(i,k) * ( 1._r8 + zvir * qt(i,k) )
        ! Thermodynamic coefficients for buoyancy flux - in this loop these are
        ! calculated at mid-points; later,  they will be averaged to interfaces,
        ! where they will ultimately be used.  At the surface, the coefficients
        ! are taken from the lowest mid point.
          bfact    = g / ( t(i,k) * ( 1._r8 + zvir * qv(i,k) - ql(i,k) - qi(i,k) ) )
          chu(i,k) = ( 1._r8 + zvir * qt(i,k) ) * bfact / cpair
          chs(i,k) = ( ( 1._r8 + ( 1._r8 + zvir ) * gam(i,k) * cpair * t(i,k) / latvap ) / ( 1._r8 + gam(i,k) ) ) * bfact / cpair
          cmu(i,k) = zvir * bfact * t(i,k)
          cms(i,k) = latvap * chs(i,k)  -  bfact * t(i,k)
       end do
    end do

    do i = 1, ncol
       chu(i,pver+1) = chu(i,pver)
       chs(i,pver+1) = chs(i,pver)
       cmu(i,pver+1) = cmu(i,pver)
       cms(i,pver+1) = cms(i,pver)
    end do

    ! Compute slopes of conserved variables sl, qt within each layer k. 
    ! 'a' indicates the 'above' gradient from layer k-1 to layer k and 
    ! 'b' indicates the 'below' gradient from layer k   to layer k+1.
    ! We take a smaller (in absolute value)  of these gradients as the
    ! slope within layer k. If they have opposite signs,   gradient in 
    ! layer k is taken to be zero. I should re-consider whether   this
    ! profile reconstruction is the best or not.
    ! This is similar to the profile reconstruction used in the UWShCu. 

    do i = 1, ncol
     ! Slopes at endpoints determined by extrapolation
       slslope(i,pver) = ( sl(i,pver) - sl(i,pver-1) ) / ( pmid(i,pver) - pmid(i,pver-1) )
       qtslope(i,pver) = ( qt(i,pver) - qt(i,pver-1) ) / ( pmid(i,pver) - pmid(i,pver-1) )
       slslope(i,1)    = ( sl(i,2) - sl(i,1) ) / ( pmid(i,2) - pmid(i,1) )
       qtslope(i,1)    = ( qt(i,2) - qt(i,1) ) / ( pmid(i,2) - pmid(i,1) )
       dsldp_b(i)      = slslope(i,1)
       dqtdp_b(i)      = qtslope(i,1)
    end do

    do k = 2, pver - 1
       do i = 1, ncol
          dsldp_a    = dsldp_b(i)
          dqtdp_a    = dqtdp_b(i)
          dsldp_b(i) = ( sl(i,k+1) - sl(i,k) ) / ( pmid(i,k+1) - pmid(i,k) )
          dqtdp_b(i) = ( qt(i,k+1) - qt(i,k) ) / ( pmid(i,k+1) - pmid(i,k) )
          product    = dsldp_a * dsldp_b(i)
          if( product .le. 0._r8 ) then 
              slslope(i,k) = 0._r8
          else if( product .gt. 0._r8 .and. dsldp_a .lt. 0._r8 ) then 
              slslope(i,k) = max( dsldp_a, dsldp_b(i) )
          else if( product .gt. 0._r8 .and. dsldp_a .gt. 0._r8 ) then 
              slslope(i,k) = min( dsldp_a, dsldp_b(i) )
          end if
          product = dqtdp_a*dqtdp_b(i)
          if( product .le. 0._r8 ) then 
              qtslope(i,k) = 0._r8
          else if( product .gt. 0._r8 .and. dqtdp_a .lt. 0._r8 ) then 
              qtslope(i,k) = max( dqtdp_a, dqtdp_b(i) )
          else if( product .gt. 0._r8 .and. dqtdp_a .gt. 0._r8 ) then 
              qtslope(i,k) = min( dqtdp_a, dqtdp_b(i) )
          end if
       end do ! i
    end do ! k

    !  Compute saturation fraction at the interfacial layers for use in buoyancy
    !  flux computation.

    call sfdiag( pcols  , pver    , ncol    , qt      , ql      , sl      , & 
                 pi     , pmid    , zi      , cld     , sfi     , sfuh    , &
                 sflh   , slslope , qtslope , qsat )

    ! Calculate buoyancy coefficients at all interfaces (1:pver+1) and (n2,s2,ri) 
    ! at all interfaces except surface. Note 'nbot_turb = pver', 'ntop_turb = 1'.
    ! With the previous definition of buoyancy coefficients at the surface, the 
    ! resulting buoyancy coefficients at the top and surface interfaces becomes 
    ! identical to the buoyancy coefficients at the top and bottom layers. Note 
    ! that even though the dimension of (s2,n2,ri) is 'pver',  they are defined
    ! at interfaces ( not at the layer mid-points ) except the surface. 

    do k = nbot_turb, ntop_turb + 1, -1
       km1 = k - 1
       do i = 1, ncol
          rdz      = 1._r8 / ( z(i,km1) - z(i,k) )
          dsldz    = ( sl(i,km1) - sl(i,k) ) * rdz
          dqtdz    = ( qt(i,km1) - qt(i,k) ) * rdz 
          chu(i,k) = ( chu(i,km1) + chu(i,k) ) * 0.5_r8
          chs(i,k) = ( chs(i,km1) + chs(i,k) ) * 0.5_r8
          cmu(i,k) = ( cmu(i,km1) + cmu(i,k) ) * 0.5_r8
          cms(i,k) = ( cms(i,km1) + cms(i,k) ) * 0.5_r8
          ch       = chu(i,k) * ( 1._r8 - sfi(i,k) ) + chs(i,k) * sfi(i,k)
          cm       = cmu(i,k) * ( 1._r8 - sfi(i,k) ) + cms(i,k) * sfi(i,k)
          n2(i,k)  = ch * dsldz +  cm * dqtdz
          s2(i,k)  = ( ( u(i,km1) - u(i,k) )**2 + ( v(i,km1) - v(i,k) )**2) * rdz**2
          s2(i,k)  = max( ntzero, s2(i,k) )
          ri(i,k)  = n2(i,k) / s2(i,k)
       end do
    end do 
    do i = 1, ncol
       n2(i,1) = n2(i,2)
       s2(i,1) = s2(i,2)
       ri(i,1) = ri(i,2)
    end do

  return

  end subroutine trbintd

  !=============================================================================== !
  !                                                                                !
  !=============================================================================== !
  
  subroutine austausch_atm( pcols, pver, ncol, ri, s2, kvf )

    !---------------------------------------------------------------------- ! 
    !                                                                       !
    ! Purpose: Computes exchange coefficients for free turbulent flows.     !
    !          This is not used in the UW moist turbulence scheme.          !
    !                                                                       !
    ! Method:                                                               !
    !                                                                       !
    ! The free atmosphere diffusivities are based on standard mixing length !
    ! forms for the neutral diffusivity multiplied by functns of Richardson !
    ! number. K = l^2 * |dV/dz| * f(Ri). The same functions are used for    !
    ! momentum, potential temperature, and constitutents.                   !
    !                                                                       !
    ! The stable Richardson num function (Ri>0) is taken from Holtslag and  !
    ! Beljaars (1989), ECMWF proceedings. f = 1 / (1 + 10*Ri*(1 + 8*Ri))    !
    ! The unstable Richardson number function (Ri<0) is taken from  CCM1.   !
    ! f = sqrt(1 - 18*Ri)                                                   !
    !                                                                       !
    ! Author: B. Stevens (rewrite, August 2000)                             !
    !                                                                       !
    !---------------------------------------------------------------------- !
    implicit none
    
    ! --------------- ! 
    ! Input arguments !
    ! --------------- !

    integer,  intent(in)  :: pcols                ! Number of atmospheric columns   
    integer,  intent(in)  :: pver                 ! Number of atmospheric layers   
    integer,  intent(in)  :: ncol                 ! Number of atmospheric columns

    real(r8), intent(in)  :: s2(pcols,pver)       ! Shear squared
    real(r8), intent(in)  :: ri(pcols,pver)       ! Richardson no

    ! ---------------- !
    ! Output arguments !
    ! ---------------- !

    real(r8), intent(out) :: kvf(pcols,pver+1)    ! Eddy diffusivity for heat and tracers

    ! --------------- !
    ! Local Variables !
    ! --------------- !

    real(r8)              :: fofri                ! f(ri)
    real(r8)              :: kvn                  ! Neutral Kv

    integer               :: i                    ! Longitude index
    integer               :: k                    ! Vertical index

    ! ----------------------- !
    ! Main Computation Begins !
    ! ----------------------- !

    kvf(:ncol,:)           = 0.0_r8
    kvf(:ncol,pver+1)      = 0.0_r8
    kvf(:ncol,1:ntop_turb) = 0.0_r8

    ! Compute the free atmosphere vertical diffusion coefficients: kvh = kvq = kvm. 

    do k = ntop_turb + 1, nbot_turb
       do i = 1, ncol
          if( ri(i,k) < 0.0_r8 ) then
              fofri = sqrt( max( 1._r8 - 18._r8 * ri(i,k), 0._r8 ) )
          else 
              fofri = 1.0_r8 / ( 1.0_r8 + 10.0_r8 * ri(i,k) * ( 1.0_r8 + 8.0_r8 * ri(i,k) ) )    
          end if
          kvn = ml2(k) * sqrt(s2(i,k))
          kvf(i,k) = max( zkmin, kvn * fofri )
       end do
    end do

    return

    end subroutine austausch_atm

    ! ---------------------------------------------------------------------------- !
    !                                                                              !
    ! The University of Washington Moist Turbulence Scheme                         !
    !                                                                              !
    ! Authors : Chris Bretherton at the University of Washington, Seattle, WA      ! 
    !           Sungsu Park at the CGD/NCAR, Boulder, CO                           !
    !                                                                              !
    ! ---------------------------------------------------------------------------- !

    subroutine caleddy( pcols        , pver         , ncol        ,                             &
                        sl           , qt           , ql          , slv        , u            , &
                        v            , pi           , z           , zi         ,                &
                        qflx         , shflx        , slslope     , qtslope    ,                &
                        chu          , chs          , cmu         , cms        , sfuh         , &
                        sflh         , n2           , s2          , ri         , rrho         , &
                        pblh         , ustar        ,                                           &
                        kvh_in       , kvm_in       , kvh         , kvm        ,                &
                        tpert        , qpert        , qrlin       , kvf        , tke          , & 
                        wstarent     , bprod        , sprod       , minpblh    , wpert        , &
                        tkes         , turbtype_f   , sm_aw       ,                             &
                        kbase_o      , ktop_o       , ncvfin_o    ,                             & 
                        kbase_mg     , ktop_mg      , ncvfin_mg   ,                             & 
                        kbase_f      , ktop_f       , ncvfin_f    ,                             & 
                        wet_CL       , web_CL       , jtbu_CL     , jbbu_CL    ,                &
                        evhc_CL      , jt2slv_CL    , n2ht_CL     , n2hb_CL    , lwp_CL       , &
                        opt_depth_CL , radinvfrac_CL, radf_CL     , wstar_CL   , wstar3fact_CL, &
                        ebrk         , wbrk         , lbrk        , ricl       , ghcl         , & 
                        shcl         , smcl         ,                                           &
                        gh_a         , sh_a         , sm_a        , ri_a       , leng         , & 
                        wcap         , pblhp        , cld         , ipbl       , kpblh        , &
                        wsedl        )

    !--------------------------------------------------------------------------------- !
    !                                                                                  !
    ! Purpose : This is a driver routine to compute eddy diffusion coefficients        !
    !           for heat (sl), momentum (u, v), moisture (qt), and other  trace        !
    !           constituents.   This scheme uses first order closure for stable        !
    !           turbulent layers (STL). For convective layers (CL), entrainment        !
    !           closure is used at the CL external interfaces, which is coupled        !
    !           to the diagnosis of a CL regime mean TKE from the instantaneous        !
    !           thermodynamic and velocity profiles.   The CLs are diagnosed by        !
    !           extending original CL layers of moist static instability   into        !
    !           adjacent weakly stably stratified interfaces,   stopping if the        !
    !           stability is too strong.   This allows a realistic depiction of        !
    !           dry convective boundary layers with a downgradient approach.           !
    !                                                                                  !   
    ! NOTE:     This routine currently assumes ntop_turb = 1, nbot_turb = pver         !
    !           ( turbulent diffusivities computed at all interior interfaces )        !
    !           and will require modification to handle a different ntop_turb.         ! 
    !                                                                                  !
    ! Authors:  Sungsu Park and Chris Bretherton. 08/2006, 05/2008.                    !
    !                                                                                  ! 
    ! For details, see                                                                 !
    !                                                                                  !
    ! 1. 'A new moist turbulence parametrization in the Community Atmosphere Model'    !
    !     by Christopher S. Bretherton & Sungsu Park. J. Climate. 22. 3422-3448. 2009. !
    !                                                                                  !
    ! 2. 'The University of Washington shallow convection and moist turbulence schemes !
    !     and their impact on climate simulations with the Community Atmosphere Model' !
    !     by Sungsu Park & Christopher S. Bretherton. J. Climate. 22. 3449-3469. 2009. !
    !                                                                                  !
    ! For questions on the scheme and code, send an email to                           !
    !     sungsup@ucar.edu or breth@washington.edu                                     !
    !                                                                                  !
    !--------------------------------------------------------------------------------- !

    ! ---------------- !
    ! Inputs variables !
    ! ---------------- !

    implicit none

    integer,  intent(in) :: pcols                     ! Number of atmospheric columns   
    integer,  intent(in) :: pver                      ! Number of atmospheric layers   
    integer,  intent(in) :: ncol                      ! Number of atmospheric columns   
    real(r8), intent(in) :: u(pcols,pver)             ! U wind [ m/s ]
    real(r8), intent(in) :: v(pcols,pver)             ! V wind [ m/s ]
    real(r8), intent(in) :: sl(pcols,pver)            ! Liquid water static energy, cp * T + g * z - Lv * ql - Ls * qi [ J/kg ]
    real(r8), intent(in) :: slv(pcols,pver)           ! Liquid water virtual static energy, sl * ( 1 + 0.608 * qt ) [ J/kg ]
    real(r8), intent(in) :: qt(pcols,pver)            ! Total speccific humidity  qv + ql + qi [ kg/kg ] 
    real(r8), intent(in) :: ql(pcols,pver)            ! Liquid water specific humidity [ kg/kg ]
    real(r8), intent(in) :: pi(pcols,pver+1)          ! Interface pressures [ Pa ]
    real(r8), intent(in) :: z(pcols,pver)             ! Layer midpoint height above surface [ m ]
    real(r8), intent(in) :: zi(pcols,pver+1)          ! Interface height above surface, i.e., zi(pver+1) = 0 all over the globe [ m ]
    real(r8), intent(in) :: chu(pcols,pver+1)         ! Buoyancy coeffi. unsaturated sl (heat) coef. at all interfaces. [ unit ? ]
    real(r8), intent(in) :: chs(pcols,pver+1)         ! Buoyancy coeffi. saturated sl (heat) coef. at all interfaces. [ unit ? ]
    real(r8), intent(in) :: cmu(pcols,pver+1)         ! Buoyancy coeffi. unsaturated qt (moisture) coef. at all interfaces [ unit ? ]
    real(r8), intent(in) :: cms(pcols,pver+1)         ! Buoyancy coeffi. saturated qt (moisture) coef. at all interfaces [ unit ? ]
    real(r8), intent(in) :: sfuh(pcols,pver)          ! Saturation fraction in upper half-layer [ fraction ]
    real(r8), intent(in) :: sflh(pcols,pver)          ! Saturation fraction in lower half-layer [ fraction ]
    real(r8), intent(in) :: n2(pcols,pver)            ! Interfacial (except surface) moist buoyancy frequency [ s-2 ]
    real(r8), intent(in) :: s2(pcols,pver)            ! Interfacial (except surface) shear frequency [ s-2 ]
    real(r8), intent(in) :: ri(pcols,pver)            ! Interfacial (except surface) Richardson number
    real(r8), intent(in) :: qflx(pcols)               ! Kinematic surface constituent ( water vapor ) flux [ kg/m2/s ]
    real(r8), intent(in) :: shflx(pcols)              ! Kinematic surface heat flux [ unit ? ] 
    real(r8), intent(in) :: slslope(pcols,pver)       ! Slope of 'sl' in each layer [ J/kg/Pa ]
    real(r8), intent(in) :: qtslope(pcols,pver)       ! Slope of 'qt' in each layer [ kg/kg/Pa ]
    real(r8), intent(in) :: qrlin(pcols,pver)         ! Input grid-mean LW heating rate : [ K/s ] * cpair * dp = [ W/kg*Pa ]
    real(r8), intent(in) :: wsedl(pcols,pver)         ! Sedimentation velocity of liquid stratus cloud droplet [ m/s ]
    real(r8), intent(in) :: ustar(pcols)              ! Surface friction velocity [ m/s ]
    real(r8), intent(in) :: rrho(pcols)               ! 1./bottom mid-point density. Specific volume [ m3/kg ]
    real(r8), intent(in) :: kvf(pcols,pver+1)         ! Free atmosphere eddy diffusivity [ m2/s ]
    logical,  intent(in) :: wstarent                  ! Switch for choosing wstar3 entrainment parameterization
    real(r8), intent(in) :: minpblh(pcols)            ! Minimum PBL height based on surface stress [ m ]
    real(r8), intent(in) :: kvh_in(pcols,pver+1)      ! kvh saved from last timestep or last iterative step [ m2/s ] 
    real(r8), intent(in) :: kvm_in(pcols,pver+1)      ! kvm saved from last timestep or last iterative step [ m2/s ]
    real(r8), intent(in) :: cld(pcols,pver)           ! Stratus Cloud Fraction [ fraction ]

    ! ---------------- !
    ! Output variables !
    ! ---------------- !

    real(r8), intent(out) :: kvh(pcols,pver+1)        ! Eddy diffusivity for heat, moisture, and tracers [ m2/s ]
    real(r8), intent(out) :: kvm(pcols,pver+1)        ! Eddy diffusivity for momentum [ m2/s ]
    real(r8), intent(out) :: pblh(pcols)              ! PBL top height [ m ]
    real(r8), intent(out) :: pblhp(pcols)             ! PBL top height pressure [ Pa ]
    real(r8), intent(out) :: tpert(pcols)             ! Convective temperature excess [ K ]
    real(r8), intent(out) :: qpert(pcols)             ! Convective humidity excess [ kg/kg ]
    real(r8), intent(out) :: wpert(pcols)             ! Turbulent velocity excess [ m/s ]
    real(r8), intent(out) :: tke(pcols,pver+1)        ! Turbulent kinetic energy [ m2/s2 ], 'tkes' at surface, pver+1.
    real(r8), intent(out) :: bprod(pcols,pver+1)      ! Buoyancy production [ m2/s3 ],     'bflxs' at surface, pver+1.
    real(r8), intent(out) :: sprod(pcols,pver+1)      ! Shear production [ m2/s3 ], (ustar(i)**3)/(vk*z(i,pver))  at surface, pver+1.
    real(r8), intent(out) :: turbtype_f(pcols,pver+1) ! Turbulence type at each interface:
                                                      ! 0. = Non turbulence interface
                                                      ! 1. = Stable turbulence interface
                                                      ! 2. = CL interior interface ( if bflxs > 0, surface is this )
                                                      ! 3. = Bottom external interface of CL
                                                      ! 4. = Top external interface of CL.
                                                      ! 5. = Double entraining CL external interface 
    real(r8), intent(out) :: sm_aw(pcols,pver+1)      ! Galperin instability function of momentum for use in the microphysics [ no unit ]
    real(r8), intent(out) :: ipbl(pcols)              ! If 1, PBL is CL, while if 0, PBL is STL.
    real(r8), intent(out) :: kpblh(pcols)             ! Layer index containing PBL within or at the base interface

    ! --------------------------- !
    ! Diagnostic output variables !
    ! --------------------------- !

    real(r8) :: tkes(pcols)                           ! TKE at surface [ m2/s2 ] 
    real(r8) :: kbase_o(pcols,ncvmax)                 ! Original external base interface index of CL just after 'exacol'
    real(r8) :: ktop_o(pcols,ncvmax)                  ! Original external top  interface index of CL just after 'exacol'
    real(r8) :: ncvfin_o(pcols)                       ! Original number of CLs just after 'exacol'
    real(r8) :: kbase_mg(pcols,ncvmax)                ! kbase  just after extending-merging (after 'zisocl') but without SRCL
    real(r8) :: ktop_mg(pcols,ncvmax)                 ! ktop   just after extending-merging (after 'zisocl') but without SRCL
    real(r8) :: ncvfin_mg(pcols)                      ! ncvfin just after extending-merging (after 'zisocl') but without SRCL
    real(r8) :: kbase_f(pcols,ncvmax)                 ! Final kbase  after adding SRCL
    real(r8) :: ktop_f(pcols,ncvmax)                  ! Final ktop   after adding SRCL
    real(r8) :: ncvfin_f(pcols)                       ! Final ncvfin after adding SRCL
    real(r8) :: wet_CL(pcols,ncvmax)                  ! Entrainment rate at the CL top [ m/s ] 
    real(r8) :: web_CL(pcols,ncvmax)                  ! Entrainment rate at the CL base [ m/s ]
    real(r8) :: jtbu_CL(pcols,ncvmax)                 ! Buoyancy jump across the CL top [ m/s2 ]  
    real(r8) :: jbbu_CL(pcols,ncvmax)                 ! Buoyancy jump across the CL base [ m/s2 ]  
    real(r8) :: evhc_CL(pcols,ncvmax)                 ! Evaporative enhancement factor at the CL top
    real(r8) :: jt2slv_CL(pcols,ncvmax)               ! Jump of slv ( across two layers ) at CL top for use only in evhc [ J/kg ]
    real(r8) :: n2ht_CL(pcols,ncvmax)                 ! n2 defined at the CL top  interface but using sfuh(kt)   instead of sfi(kt) [ s-2 ]
    real(r8) :: n2hb_CL(pcols,ncvmax)                 ! n2 defined at the CL base interface but using sflh(kb-1) instead of sfi(kb) [ s-2 ]
    real(r8) :: lwp_CL(pcols,ncvmax)                  ! LWP in the CL top layer [ kg/m2 ]
    real(r8) :: opt_depth_CL(pcols,ncvmax)            ! Optical depth of the CL top layer
    real(r8) :: radinvfrac_CL(pcols,ncvmax)           ! Fraction of LW radiative cooling confined in the top portion of CL
    real(r8) :: radf_CL(pcols,ncvmax)                 ! Buoyancy production at the CL top due to radiative cooling [ m2/s3 ]
    real(r8) :: wstar_CL(pcols,ncvmax)                ! Convective velocity of CL including entrainment contribution finally [ m/s ]
    real(r8) :: wstar3fact_CL(pcols,ncvmax)           ! "wstar3fact" of CL. Entrainment enhancement of wstar3 (inverse)

    real(r8) :: gh_a(pcols,pver+1)                    ! Half of normalized buoyancy production, -l2n2/2e. [ no unit ]
    real(r8) :: sh_a(pcols,pver+1)                    ! Galperin instability function of heat-moisture at all interfaces [ no unit ]
    real(r8) :: sm_a(pcols,pver+1)                    ! Galperin instability function of momentum      at all interfaces [ no unit ]
    real(r8) :: ri_a(pcols,pver+1)                    ! Interfacial Richardson number                  at all interfaces [ no unit ]

    real(r8) :: ebrk(pcols,ncvmax)                    ! Net CL mean TKE [ m2/s2 ]
    real(r8) :: wbrk(pcols,ncvmax)                    ! Net CL mean normalized TKE [ m2/s2 ]
    real(r8) :: lbrk(pcols,ncvmax)                    ! Net energetic integral thickness of CL [ m ]
    real(r8) :: ricl(pcols,ncvmax)                    ! Mean Richardson number of CL ( l2n2/l2s2 )
    real(r8) :: ghcl(pcols,ncvmax)                    ! Half of normalized buoyancy production of CL                 
    real(r8) :: shcl(pcols,ncvmax)                    ! Instability function of heat and moisture of CL
    real(r8) :: smcl(pcols,ncvmax)                    ! Instability function of momentum of CL

    real(r8) :: leng(pcols,pver+1)                    ! Turbulent length scale [ m ], 0 at the surface.
    real(r8) :: wcap(pcols,pver+1)                    ! Normalized TKE [m2/s2], 'tkes/b1' at the surface and 'tke/b1' at
                                                      ! the top/bottom entrainment interfaces of CL assuming no transport.
    ! ------------------------ !
    ! Local Internal Variables !
    ! ------------------------ !

    logical :: belongcv(pcols,pver+1)                 ! True for interfaces in a CL (both interior and exterior are included)
    logical :: belongst(pcols,pver+1)                 ! True for stable turbulent layer interfaces (STL)
    logical :: in_CL                                  ! True if interfaces k,k+1 both in same CL.
    logical :: extend                                 ! True when CL is extended in zisocl
    logical :: extend_up                              ! True when CL is extended upward in zisocl
    logical :: extend_dn                              ! True when CL is extended downward in zisocl

    integer :: i                                      ! Longitude index
    integer :: k                                      ! Vertical index
    integer :: ks                                     ! Vertical index
    integer :: ncvfin(pcols)                          ! Total number of CL in column
    integer :: ncvf                                   ! Total number of CL in column prior to adding SRCL
    integer :: ncv                                    ! Index of current CL
    integer :: ncvnew                                 ! Index of added SRCL appended after regular CLs from 'zisocl'
    integer :: ncvsurf                                ! If nonzero, CL index based on surface (usually 1, but can be > 1 when SRCL is based at sfc)
    integer :: kbase(pcols,ncvmax)                    ! Vertical index of CL base interface
    integer :: ktop(pcols,ncvmax)                     ! Vertical index of CL top interface
    integer :: kb, kt                                 ! kbase and ktop for current CL
    integer :: ktblw                                  ! ktop of the CL located at just below the current CL
    integer :: turbtype(pcols,pver+1)                 ! Interface turbulence type :
                                                      ! 0 = Non turbulence interface
                                                      ! 1 = Stable turbulence interface
                                                      ! 2 = CL interior interface ( if bflxs > 0, sfc is this )
                                                      ! 3 = Bottom external interface of CL
                                                      ! 4 = Top external interface of CL
                                                      ! 5 = Double entraining CL external interface
    integer  :: ktopbl(pcols)                         ! PBL top height or interface index 
    real(r8) :: bflxs(pcols)                          ! Surface buoyancy flux [ m2/s3 ]
    real(r8) :: rcap                                  ! 'tke/ebrk' at all interfaces of CL. Set to 1 at the CL entrainment interfaces
    real(r8) :: jtzm                                  ! Interface layer thickness of CL top interface [ m ]
    real(r8) :: jtsl                                  ! Jump of s_l across CL top interface [ J/kg ]
    real(r8) :: jtqt                                  ! Jump of q_t across CL top interface [ kg/kg ]
    real(r8) :: jtbu                                  ! Jump of buoyancy across CL top interface [ m/s2 ]
    real(r8) :: jtu                                   ! Jump of u across CL top interface [ m/s ]
    real(r8) :: jtv                                   ! Jump of v across CL top interface [ m/s ]
    real(r8) :: jt2slv                                ! Jump of slv ( across two layers ) at CL top for use only in evhc [ J/kg ]
    real(r8) :: radf                                  ! Buoyancy production at the CL top due to radiative cooling [ m2/s3 ]
    real(r8) :: jbzm                                  ! Interface layer thickness of CL base interface [ m ]
    real(r8) :: jbsl                                  ! Jump of s_l across CL base interface [ J/kg ]
    real(r8) :: jbqt                                  ! Jump of q_t across CL top interface [ kg/kg ]
    real(r8) :: jbbu                                  ! Jump of buoyancy across CL base interface [ m/s2 ]
    real(r8) :: jbu                                   ! Jump of u across CL base interface [ m/s ]
    real(r8) :: jbv                                   ! Jump of v across CL base interface [ m/s ]
    real(r8) :: ch                                    ! Buoyancy coefficients defined at the CL top and base interfaces using CL internal
    real(r8) :: cm                                    ! sfuh(kt) and sflh(kb-1) instead of sfi(kt) and sfi(kb), respectively. These are 
                                                      ! used for entrainment calculation at CL external interfaces and SRCL identification.
    real(r8) :: n2ht                                  ! n2 defined at the CL top  interface but using sfuh(kt)   instead of sfi(kt) [ s-2 ]
    real(r8) :: n2hb                                  ! n2 defined at the CL base interface but using sflh(kb-1) instead of sfi(kb) [ s-2 ]
    real(r8) :: n2htSRCL                              ! n2 defined at the upper-half layer of SRCL. This is used only for identifying SRCL.
                                                      ! n2htSRCL use SRCL internal slope sl and qt as well as sfuh(kt) instead of sfi(kt) [ s-2 ]
    real(r8) :: gh                                    ! Half of normalized buoyancy production ( -l2n2/2e ) [ no unit ]
    real(r8) :: sh                                    ! Galperin instability function for heat and moisture
    real(r8) :: sm                                    ! Galperin instability function for momentum
    real(r8) :: lbulk                                 ! Depth of turbulent layer, Master length scale (not energetic length)
    real(r8) :: dzht                                  ! Thickness of top    half-layer [ m ]
    real(r8) :: dzhb                                  ! Thickness of bottom half-layer [ m ]
    real(r8) :: rootp                                 ! Sqrt(net CL-mean TKE including entrainment contribution) [ m/s ]     
    real(r8) :: evhc                                  ! Evaporative enhancement factor: (1+E) with E = evap. cool. efficiency [ no unit ]
    real(r8) :: kentr                                 ! Effective entrainment diffusivity 'wet*dz', 'web*dz' [ m2/s ]
    real(r8) :: lwp                                   ! Liquid water path in the layer kt [ kg/m2 ]
    real(r8) :: opt_depth                             ! Optical depth of the layer kt [ no unit ]
    real(r8) :: radinvfrac                            ! Fraction of LW cooling in the layer kt concentrated at the CL top [ no unit ]
    real(r8) :: wet                                   ! CL top entrainment rate [ m/s ]
    real(r8) :: web                                   ! CL bot entrainment rate [ m/s ]. Set to zero if CL is based at surface.
    real(r8) :: vyt                                   ! n2ht/n2 at the CL top  interface
    real(r8) :: vyb                                   ! n2hb/n2 at the CL base interface
    real(r8) :: vut                                   ! Inverse Ri (=s2/n2) at the CL top  interface
    real(r8) :: vub                                   ! Inverse Ri (=s2/n2) at the CL base interface
    real(r8) :: fact                                  ! Factor relating TKE generation to entrainment [ no unit ]
    real(r8) :: trma                                  ! Intermediate variables used for solving quadratic ( for gh from ri )
    real(r8) :: trmb                                  ! and cubic equations ( for ebrk: the net CL mean TKE )
    real(r8) :: trmc                                  !
    real(r8) :: trmp                                  !
    real(r8) :: trmq                                  !
    real(r8) :: qq                                    ! 
    real(r8) :: det                                   !
    real(r8) :: gg                                    ! Intermediate variable used for calculating stability functions of
                                                      ! SRCL or SBCL based at the surface with bflxs > 0.
    real(r8) :: dzhb5                                 ! Half thickness of the bottom-most layer of current CL regime
    real(r8) :: dzht5                                 ! Half thickness of the top-most layer of adjacent CL regime just below current CL
    real(r8) :: qrlw(pcols,pver)                      ! Local grid-mean LW heating rate : [K/s] * cpair * dp = [ W/kg*Pa ]

    real(r8) :: cldeff(pcols,pver)                    ! Effective stratus fraction
    real(r8) :: qleff                                 ! Used for computing evhc
    real(r8) :: tunlramp                              ! Ramping tunl
    real(r8) :: leng_imsi                             ! For Kv = max(Kv_STL, Kv_entrain)
    real(r8) :: tke_imsi                              !
    real(r8) :: kvh_imsi                              !
    real(r8) :: kvm_imsi                              !
    real(r8) :: alph4exs                              ! For extended stability function in the stable regime
    real(r8) :: ghmin                                 !   

    real(r8) :: sedfact                               ! For 'sedimentation-entrainment feedback' 

    ! Local variables specific for 'wstar' entrainment closure

    real(r8) :: cet                                   ! Proportionality coefficient between wet and wstar3
    real(r8) :: ceb                                   ! Proportionality coefficient between web and wstar3
    real(r8) :: wstar                                 ! Convective velocity for CL [ m/s ]
    real(r8) :: wstar3                                ! Cubed convective velocity for CL [ m3/s3 ]
    real(r8) :: wstar3fact                            ! 1/(relative change of wstar^3 by entrainment)
    real(r8) :: rmin                                  ! sqrt(p)
    real(r8) :: fmin                                  ! f(rmin), where f(r) = r^3 - 3*p*r - 2q
    real(r8) :: rcrit                                 ! ccrit*wstar
    real(r8) :: fcrit                                 ! f(rcrit)
    logical     noroot                                ! True if f(r) has no root r > rcrit

    !-----------------------!
    ! Start of Main Program !
    !-----------------------!
    
    ! Option: Turn-off LW radiative-turbulence interaction in PBL scheme
    !         by setting qrlw = 0.  Logical parameter 'set_qrlzero'  was
    !         defined in the first part of 'eddy_diff.F90' module. 

    if( set_qrlzero ) then
        qrlw(:,:) = 0._r8
    else
        qrlw(:ncol,:pver) = qrlin(:ncol,:pver)
    endif

    ! Define effective stratus fraction using the grid-mean ql.
    ! Modification : The contribution of ice should be carefully considered.
    !                This should be done in combination with the 'qrlw' and
    !                overlapping assumption of liquid and ice stratus. 

    do k = 1, pver
       do i = 1, ncol
          if( choice_evhc .eq. 'ramp' .or. choice_radf .eq. 'ramp' ) then 
              cldeff(i,k) = cld(i,k) * min( ql(i,k) / qmin, 1._r8 )
          else
              cldeff(i,k) = cld(i,k)
          endif
       end do
    end do

    ! For an extended stability function in the stable regime, re-define
    ! alph4exe and ghmin. This is for future work.

    if( ricrit .eq. 0.19_r8 ) then
        alph4exs = alph4
        ghmin    = -3.5334_r8
    elseif( ricrit .gt. 0.19_r8 ) then
        alph4exs = -2._r8 * b1 * alph2 / ( alph3 - 2._r8 * b1 * alph5 ) / ricrit
        ghmin    = -1.e10_r8
    else
        write(iulog,*) 'Error : ricrit should be larger than 0.19 in UW PBL'       
#ifdef WRF_PORT
        call wrf_message(iulog)
#endif 
        stop
    endif

    !
    ! Initialization of Diagnostic Output
    !

    do i = 1, ncol
       wet_CL(i,:ncvmax)        = 0._r8
       web_CL(i,:ncvmax)        = 0._r8
       jtbu_CL(i,:ncvmax)       = 0._r8
       jbbu_CL(i,:ncvmax)       = 0._r8
       evhc_CL(i,:ncvmax)       = 0._r8
       jt2slv_CL(i,:ncvmax)     = 0._r8
       n2ht_CL(i,:ncvmax)       = 0._r8
       n2hb_CL(i,:ncvmax)       = 0._r8                    
       lwp_CL(i,:ncvmax)        = 0._r8
       opt_depth_CL(i,:ncvmax)  = 0._r8
       radinvfrac_CL(i,:ncvmax) = 0._r8
       radf_CL(i,:ncvmax)       = 0._r8
       wstar_CL(i,:ncvmax)      = 0._r8          
       wstar3fact_CL(i,:ncvmax) = 0._r8
       ricl(i,:ncvmax)          = 0._r8
       ghcl(i,:ncvmax)          = 0._r8
       shcl(i,:ncvmax)          = 0._r8
       smcl(i,:ncvmax)          = 0._r8
       ebrk(i,:ncvmax)          = 0._r8
       wbrk(i,:ncvmax)          = 0._r8
       lbrk(i,:ncvmax)          = 0._r8
       gh_a(i,:pver+1)          = 0._r8
       sh_a(i,:pver+1)          = 0._r8
       sm_a(i,:pver+1)          = 0._r8
       ri_a(i,:pver+1)          = 0._r8
       sm_aw(i,:pver+1)         = 0._r8
       ipbl(i)                  = 0._r8
       kpblh(i)                 = real(pver,r8)
    end do  

    ! kvh and kvm are stored over timesteps in 'vertical_diffusion.F90' and 
    ! passed in as kvh_in and kvm_in.  However,  at the first timestep they
    ! need to be computed and these are done just before calling 'caleddy'.   
    ! kvm and kvh are also stored over iterative time step in the first part
    ! of 'eddy_diff.F90'

    do k = 1, pver + 1
       do i = 1, ncol
        ! Initialize kvh and kvm to zero or kvf
          if( use_kvf ) then
              kvh(i,k) = kvf(i,k)
              kvm(i,k) = kvf(i,k)
          else
              kvh(i,k) = 0._r8
              kvm(i,k) = 0._r8
          end if
        ! Zero diagnostic quantities for the new diffusion step.
          wcap(i,k) = 0._r8
          leng(i,k) = 0._r8
          tke(i,k)  = 0._r8
          turbtype(i,k) = 0
       end do
    end do

    ! Initialize 'bprod' [ m2/s3 ] and 'sprod' [ m2/s3 ] at all interfaces.
    ! Note this initialization is a hybrid initialization since 'n2' [s-2] and 's2' [s-2]
    ! are calculated from the given current initial profile, while 'kvh_in' [m2/s] and 
    ! 'kvm_in' [m2/s] are from the previous iteration or previous time step.
    ! This initially guessed 'bprod' and 'sprod' will be updated at the end of this 
    ! 'caleddy' subroutine for diagnostic output.
    ! This computation of 'brpod,sprod' below is necessary for wstar-based entrainment closure.

    do k = 2, pver
       do i = 1, ncol
            bprod(i,k) = -kvh_in(i,k) * n2(i,k)
            sprod(i,k) =  kvm_in(i,k) * s2(i,k)
       end do
    end do

    ! Set 'bprod' and 'sprod' at top and bottom interface.
    ! In calculating 'surface' (actually lowest half-layer) buoyancy flux,
    ! 'chu' at surface is defined to be the same as 'chu' at the mid-point
    ! of lowest model layer (pver) at the end of 'trbind'. The same is for
    ! the other buoyancy coefficients.  'sprod(i,pver+1)'  is defined in a
    ! consistent way as the definition of 'tkes' in the original code.
    ! ( Important Option ) If I want to isolate surface buoyancy flux from
    ! the other parts of CL regimes energetically even though bflxs > 0,
    ! all I should do is to re-define 'bprod(i,pver+1)=0' in the below 'do'
    ! block. Additionally for merging test of extending SBCL based on 'l2n2'
    ! in 'zisocl', I should use 'l2n2 = - wint / sh'  for similar treatment
    ! as previous code. All other parts of the code  are fully consistently
    ! treated by these change only.
    ! My future general convection scheme will use bflxs(i).

    do i = 1, ncol
       bprod(i,1) = 0._r8 ! Top interface
       sprod(i,1) = 0._r8 ! Top interface
       ch = chu(i,pver+1) * ( 1._r8 - sflh(i,pver) ) + chs(i,pver+1) * sflh(i,pver)   
       cm = cmu(i,pver+1) * ( 1._r8 - sflh(i,pver) ) + cms(i,pver+1) * sflh(i,pver)   
       bflxs(i) = ch * shflx(i) * rrho(i) + cm * qflx(i) * rrho(i)
       if( choice_tkes .eq. 'ibprod' ) then
           bprod(i,pver+1) = bflxs(i)
       else
           bprod(i,pver+1) = 0._r8
       endif
       sprod(i,pver+1) = (ustar(i)**3)/(vk*z(i,pver))
    end do

    ! Initially identify CL regimes in 'exacol'
    !    ktop  : Interface index of the CL top  external interface
    !    kbase : Interface index of the CL base external interface
    !    ncvfin: Number of total CLs
    ! Note that if surface buoyancy flux is positive ( bflxs = bprod(i,pver+1) > 0 ),
    ! surface interface is identified as an internal interface of CL. However, even
    ! though bflxs <= 0, if 'pver' interface is a CL internal interface (ri(pver)<0),
    ! surface interface is identified as an external interface of CL. If bflxs =< 0 
    ! and ri(pver) >= 0, then surface interface is identified as a stable turbulent
    ! intereface (STL) as shown at the end of 'caleddy'. Even though a 'minpblh' is
    ! passed into 'exacol', it is not used in the 'exacol'.

    call exacol( pcols, pver, ncol, ri, bflxs, minpblh, zi, ktop, kbase, ncvfin )

    ! Diagnostic output of CL interface indices before performing 'extending-merging'
    ! of CL regimes in 'zisocl'
    do i = 1, ncol
    do k = 1, ncvmax
       kbase_o(i,k) = real(kbase(i,k),r8)
       ktop_o(i,k)  = real(ktop(i,k),r8) 
       ncvfin_o(i)  = real(ncvfin(i),r8)
    end do
    end do 

    ! ----------------------------------- !
    ! Perform calculation for each column !
    ! ----------------------------------- !

    do i = 1, ncol

       ! Define Surface Interfacial Layer TKE, 'tkes'.
       ! In the current code, 'tkes' is used as representing TKE of surface interfacial
       ! layer (low half-layer of surface-based grid layer). In the code, when bflxs>0,
       ! surface interfacial layer is assumed to be energetically  coupled to the other
       ! parts of the CL regime based at the surface. In this sense, it is conceptually
       ! more reasonable to include both 'bprod' and 'sprod' in the definition of 'tkes'.
       ! Since 'tkes' cannot be negative, it is lower bounded by small positive number. 
       ! Note that inclusion of 'bprod' in the definition of 'tkes' may increase 'ebrk'
       ! and 'wstar3', and eventually, 'wet' at the CL top, especially when 'bflxs>0'.
       ! This might help to solve the problem of too shallow PBLH over the overcast Sc
       ! regime. If I want to exclude 'bprod(i,pver+1)' in calculating 'tkes' even when
       ! bflxs > 0, all I should to do is to set 'bprod(i,pver+1) = 0' in the above 
       ! initialization 'do' loop (explained above), NOT changing the formulation of
       ! tkes(i) in the below block. This is because for consistent treatment in the 
       ! other parts of the code also.
  
     ! tkes(i) = (b1*vk*z(i,pver)*sprod(i,pver+1))**(2._r8/3._r8)
       tkes(i) = max(b1*vk*z(i,pver)*(bprod(i,pver+1)+sprod(i,pver+1)), 1.e-7_r8)**(2._r8/3._r8)
       tkes(i) = min(tkes(i), tkemax)
       tke(i,pver+1)  = tkes(i)
       wcap(i,pver+1) = tkes(i)/b1

       ! Extend and merge the initially identified CLs, relabel the CLs, and calculate
       ! CL internal mean energetics and stability functions in 'zisocl'. 
       ! The CL nearest to the surface is CL(1) and the CL index, ncv, increases 
       ! with height. The following outputs are from 'zisocl'. Here, the dimension
       ! of below outputs are (pcols,ncvmax) (except the 'ncvfin(pcols)' and 
       ! 'belongcv(pcols,pver+1)) and 'ncv' goes from 1 to 'ncvfin'. 
       ! For 'ncv = ncvfin+1, ncvmax', below output are already initialized to be zero. 
       !      ncvfin       : Total number of CLs
       !      kbase(ncv)   : Base external interface index of CL
       !      ktop         : Top  external interface index of CL
       !      belongcv     : True if the interface (either internal or external) is CL  
       !      ricl         : Mean Richardson number of internal CL
       !      ghcl         : Normalized buoyancy production '-l2n2/2e' [no unit] of internal CL
       !      shcl         : Galperin instability function of heat-moisture of internal CL
       !      smcl         : Galperin instability function of momentum of internal CL
       !      lbrk, <l>int : Thickness of (energetically) internal CL (lint, [m])
       !      wbrk, <W>int : Mean normalized TKE of internal CL  ([m2/s2])
       !      ebrk, <e>int : Mean TKE of internal CL (b1*wbrk,[m2/s2])
       ! The ncvsurf is an identifier saying which CL regime is based at the surface.
       ! If 'ncvsurf=1', then the first CL regime is based at the surface. If surface
       ! interface is not a part of CL (neither internal nor external), 'ncvsurf = 0'.
       ! After identifying and including SRCLs into the normal CL regimes (where newly
       ! identified SRCLs are simply appended to the normal CL regimes using regime 
       ! indices of 'ncvfin+1','ncvfin+2' (as will be shown in the below SRCL part),..
       ! where 'ncvfin' is the final CL regime index produced after extending-merging 
       ! in 'zisocl' but before adding SRCLs), if any newly identified SRCL (e.g., 
       ! 'ncvfin+1') is based at surface, then 'ncvsurf = ncvfin+1'. Thus 'ncvsurf' can
       ! be 0, 1, or >1. 'ncvsurf' can be a useful diagnostic output.   

       ncvsurf = 0
       if( ncvfin(i) .gt. 0 ) then 
           call zisocl( pcols  , pver     , i        ,           &
                        z      , zi       , n2       , s2      , & 
                        bprod  , sprod    , bflxs    , tkes    , &
                        ncvfin , kbase    , ktop     , belongcv, &
                        ricl   , ghcl     , shcl     , smcl    , & 
                        lbrk   , wbrk     , ebrk     ,           & 
                        extend , extend_up, extend_dn )
           if( kbase(i,1) .eq. pver + 1 ) ncvsurf = 1
       else
           belongcv(i,:) = .false.
       endif

       ! Diagnostic output after finishing extending-merging process in 'zisocl'
       ! Since we are adding SRCL additionally, we need to print out these here.

       do k = 1, ncvmax
          kbase_mg(i,k) = real(kbase(i,k))
          ktop_mg(i,k)  = real(ktop(i,k)) 
          ncvfin_mg(i)  = real(ncvfin(i))
       end do 

       ! ----------------------- !
       ! Identification of SRCLs !
       ! ----------------------- !

     ! Modification : This cannot identify the 'cirrus' layer due to the condition of
     !                ql(i,k) .gt. qmin. This should be modified in future to identify
     !                a single thin cirrus layer.  
     !                Instead of ql, we may use cldn in future, including ice 
     !                contribution.

       ! ------------------------------------------------------------------------------ !
       ! Find single-layer radiatively-driven cloud-topped convective layers (SRCLs).   !
       ! SRCLs extend through a single model layer k, with entrainment at the top and   !
       ! bottom interfaces, unless bottom interface is the surface.                     !
       ! The conditions for an SRCL is identified are:                                  ! 
       !                                                                                !
       !   1. Cloud in the layer, k : ql(i,k) .gt. qmin = 1.e-5 [ kg/kg ]               !
       !   2. No cloud in the above layer (else assuming that some fraction of the LW   !
       !      flux divergence in layer k is concentrated at just below top interface    !
       !      of layer k is invalid). Then, this condition might be sensitive to the    !
       !      vertical resolution of grid.                                              !
       !   3. LW radiative cooling (SW heating is assumed uniformly distributed through !
       !      layer k, so not relevant to buoyancy production) in the layer k. However, !
       !      SW production might also contribute, which may be considered in a future. !
       !   4. Internal stratification 'n2ht' of upper-half layer should be unstable.    !
       !      The 'n2ht' is pure internal stratification of upper half layer, obtained  !
       !      using internal slopes of sl, qt in layer k (in contrast to conventional   !
       !      interfacial slope) and saturation fraction in the upper-half layer,       !
       !      sfuh(k) (in contrast to sfi(k)).                                          !
       !   5. Top and bottom interfaces not both in the same existing convective layer. !
       !      If SRCL is within the previouisly identified CL regimes, we don't define  !
       !      a new SRCL.                                                               !
       !   6. k >= ntop_turb + 1 = 2                                                    !
       !   7. Ri at the top interface > ricrit = 0.19 (otherwise turbulent mixing will  !
       !      broadly distribute the cloud top in the vertical, preventing localized    !
       !      radiative destabilization at the top interface).                          !
       !                                                                                !
       ! Note if 'k = pver', it identifies a surface-based single fog layer, possibly,  !
       ! warm advection fog. Note also the CL regime index of SRCLs itself increases    !
       ! with height similar to the regular CLs indices identified from 'zisocl'.       !
       ! ------------------------------------------------------------------------------ !

       ncv  = 1
       ncvf = ncvfin(i)

       if( choice_SRCL .eq. 'remove' ) goto 222 

       do k = nbot_turb, ntop_turb + 1, -1 ! 'k = pver, 2, -1' is a layer index.

          if( ql(i,k) .gt. qmin .and. ql(i,k-1) .lt. qmin .and. qrlw(i,k) .lt. 0._r8 &
                                .and. ri(i,k) .ge. ricrit ) then

              ! In order to avoid any confliction with the treatment of ambiguous layer,
              ! I need to impose an additional constraint that ambiguous layer cannot be
              ! SRCL. So, I added constraint that 'k+1' interface (base interface of k
              ! layer) should not be a part of previously identified CL. Since 'belongcv'
              ! is even true for external entrainment interfaces, below constraint is
              ! fully sufficient.
 
              if( choice_SRCL .eq. 'nonamb' .and. belongcv(i,k+1) ) then
                  go to 220 
              endif

              ch = ( 1._r8 - sfuh(i,k) ) * chu(i,k) + sfuh(i,k) * chs(i,k)
              cm = ( 1._r8 - sfuh(i,k) ) * cmu(i,k) + sfuh(i,k) * cms(i,k)

              n2htSRCL = ch * slslope(i,k) + cm * qtslope(i,k)

              if( n2htSRCL .le. 0._r8 ) then

                  ! Test if bottom and top interfaces are part of the pre-existing CL. 
                  ! If not, find appropriate index for the new SRCL. Note that this
                  ! calculation makes use of 'ncv set' obtained from 'zisocl'. The 
                  ! 'in_CL' is a parameter testing whether the new SRCL is already 
                  ! within the pre-existing CLs (.true.) or not (.false.). 

                  in_CL = .false.

                  do while ( ncv .le. ncvf )
                     if( ktop(i,ncv) .le. k ) then
                        if( kbase(i,ncv) .gt. k ) then 
                            in_CL = .true.
                        endif
                        exit             ! Exit from 'do while' loop if SRCL is within the CLs.
                     else
                        ncv = ncv + 1    ! Go up one CL
                     end if
                  end do ! ncv

                  if( .not. in_CL ) then ! SRCL is not within the pre-existing CLs.

                     ! Identify a new SRCL and add it to the pre-existing CL regime group.

                     ncvfin(i)       =  ncvfin(i) + 1
                     ncvnew          =  ncvfin(i)
                     ktop(i,ncvnew)  =  k
                     kbase(i,ncvnew) =  k+1
                     belongcv(i,k)   = .true.
                     belongcv(i,k+1) = .true.

                     ! Calculate internal energy of SRCL. There is no internal energy if
                     ! SRCL is elevated from the surface. Also, we simply assume neutral 
                     ! stability function. Note that this assumption of neutral stability
                     ! does not influence numerical calculation- stability functions here
                     ! are just for diagnostic output. In general SRCLs other than a SRCL 
                     ! based at surface with bflxs <= 0, there is no other way but to use
                     ! neutral stability function.  However, in case of SRCL based at the
                     ! surface,  we can explicitly calculate non-zero stability functions            
                     ! in a consistent way.   Even though stability functions of SRCL are
                     ! just diagnostic outputs not influencing numerical calculations, it
                     ! would be informative to write out correct reasonable values rather
                     ! than simply assuming neutral stability. I am doing this right now.
                     ! Similar calculations were done for the SBCL and when surface inter
                     ! facial layer was merged by overlying CL in 'ziscol'.

                     if( k .lt. pver ) then

                         wbrk(i,ncvnew) = 0._r8
                         ebrk(i,ncvnew) = 0._r8
                         lbrk(i,ncvnew) = 0._r8
                         ghcl(i,ncvnew) = 0._r8
                         shcl(i,ncvnew) = 0._r8
                         smcl(i,ncvnew) = 0._r8
                         ricl(i,ncvnew) = 0._r8

                     else ! Surface-based fog

                         if( bflxs(i) .gt. 0._r8 ) then    ! Incorporate surface TKE into CL interior energy
                                                           ! It is likely that this case cannot exist  since
                                                           ! if surface buoyancy flux is positive,  it would
                                                           ! have been identified as SBCL in 'zisocl' ahead. 
                             ebrk(i,ncvnew) = tkes(i)
                             lbrk(i,ncvnew) = z(i,pver)
                             wbrk(i,ncvnew) = tkes(i) / b1    
        
                             write(iulog,*) 'Major mistake in SRCL: bflxs > 0 for surface-based SRCL'
#ifdef WRF_PORT
                             call wrf_message(iulog)
#endif 
                             write(iulog,*) 'bflxs = ', bflxs(i)
#ifdef WRF_PORT
                             call wrf_message(iulog)
#endif                              
                             write(iulog,*) 'ncvfin_o = ', ncvfin_o(i)
#ifdef WRF_PORT
                             call wrf_message(iulog)
#endif 
                             write(iulog,*) 'ncvfin_mg = ', ncvfin_mg(i)
#ifdef WRF_PORT
                             call wrf_message(iulog)
#endif 
                             do ks = 1, ncvmax
                                write(iulog,*) 'ncv =', ks, ' ', kbase_o(i,ks), ktop_o(i,ks), kbase_mg(i,ks), ktop_mg(i,ks)
#ifdef WRF_PORT
                                call wrf_message(iulog)
#endif 
                             end do
                             stop

                         else                              ! Don't incorporate surface interfacial TKE into CL interior energy

                             ebrk(i,ncvnew) = 0._r8
                             lbrk(i,ncvnew) = 0._r8
                             wbrk(i,ncvnew) = 0._r8

                         endif

                         ! Calculate stability functions (ghcl, shcl, smcl, ricl) explicitly
                         ! using an reverse procedure starting from tkes(i). Note that it is
                         ! possible to calculate stability functions even when bflxs < 0.
                         ! Previous code just assumed neutral stability functions. Note that
                         ! since alph5 = 0.7 > 0, alph3 = -35 < 0, the denominator of gh  is
                         ! always positive if bflxs > 0. However, if bflxs < 0,  denominator
                         ! can be zero. For this case, we provide a possible maximum negative
                         ! value (the most stable state) to gh. Note also tkes(i) is always a
                         ! positive value by a limiter. Also, sprod(i,pver+1) > 0 by limiter.
                         
                         gg = 0.5_r8 * vk * z(i,pver) * bprod(i,pver+1) / ( tkes(i)**(3._r8/2._r8) )
                         if( abs(alph5-gg*alph3) .le. 1.e-7_r8 ) then
                           ! gh = -0.28_r8
                           ! gh = -3.5334_r8
                             gh = ghmin
                         else    
                             gh = gg / ( alph5 - gg * alph3 )
                         end if 
                       ! gh = min(max(gh,-0.28_r8),0.0233_r8)
                       ! gh = min(max(gh,-3.5334_r8),0.0233_r8)
                         gh = min(max(gh,ghmin),0.0233_r8)
                         ghcl(i,ncvnew) =  gh
                         shcl(i,ncvnew) =  max(0._r8,alph5/(1._r8+alph3*gh))
                         smcl(i,ncvnew) =  max(0._r8,(alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4exs*gh))
                         ricl(i,ncvnew) = -(smcl(i,ncvnew)/shcl(i,ncvnew))*(bprod(i,pver+1)/sprod(i,pver+1))

                       ! 'ncvsurf' is CL regime index based at the surface. If there is no
                       ! such regime, then 'ncvsurf = 0'.
    
                         ncvsurf = ncvnew

                      end if

                  end if

              end if

          end if

   220 continue    

       end do ! End of 'k' loop where 'k' is a grid layer index running from 'pver' to 2

   222 continue

       ! -------------------------------------------------------------------------- !
       ! Up to this point, we identified all kinds of CL regimes :                  !
       !   1. A SBCL. By construction, 'bflxs > 0' for SBCL.                        !
       !   2. Surface-based CL with multiple layers and 'bflxs =< 0'                !
       !   3. Surface-based CL with multiple layers and 'bflxs > 0'                 !
       !   4. Regular elevated CL with two entraining interfaces                    ! 
       !   5. SRCLs. If SRCL is based at surface, it will be bflxs < 0.             !
       ! '1-4' were identified from 'zisocl' while '5' were identified separately   !
       ! after performing 'zisocl'. CL regime index of '1-4' increases with height  !
       ! ( e.g., CL = 1 is the CL regime nearest to the surface ) while CL regime   !
       ! index of SRCL is simply appended after the final index of CL regimes from  !
       ! 'zisocl'. However, CL regime indices of SRCLs itself increases with height !
       ! when there are multiple SRCLs, similar to the regular CLs from 'zisocl'.   !
       ! -------------------------------------------------------------------------- !

       ! Diagnostic output of final CL regimes indices
       
       do k = 1, ncvmax
          kbase_f(i,k) = real(kbase(i,k))
          ktop_f(i,k)  = real(ktop(i,k)) 
          ncvfin_f(i)  = real(ncvfin(i))
       end do 

       ! ---------------------------------------- !
       ! Perform do loop for individual CL regime !
       ! ---------------------------------------- ! -------------------------------- !
       ! For individual CLs, compute                                                 !
       !   1. Entrainment rates at the CL top and (if any) base interfaces using     !
       !      appropriate entrainment closure (current code use 'wstar' closure).    !
       !   2. Net CL mean (i.e., including entrainment contribution) TKE (ebrk)      !
       !      and normalized TKE (wbrk).                                             ! 
       !   3. TKE (tke) and normalized TKE (wcap) profiles at all CL interfaces.     !
       !   4. ( kvm, kvh ) profiles at all CL interfaces.                            !
       !   5. ( bprod, sprod ) profiles at all CL interfaces.                        !
       ! Also calculate                                                              !
       !   1. PBL height as the top external interface of surface-based CL, if any.  !
       !   2. Characteristic excesses of convective 'updraft velocity (wpert)',      !
       !      'temperature (tpert)', and 'moisture (qpert)' in the surface-based CL, !
       !      if any, for use in the separate convection scheme.                     ! 
       ! If there is no surface-based CL, 'PBL height' and 'convective excesses' are !
       ! calculated later from surface-based STL (Stable Turbulent Layer) properties.!
       ! --------------------------------------------------------------------------- !

       ktblw = 0
       do ncv = 1, ncvfin(i)

          kt = ktop(i,ncv)
          kb = kbase(i,ncv)
          ! Check whether surface interface is energetically interior or not.
          if( kb .eq. (pver+1) .and. bflxs(i) .le. 0._r8 ) then
              lbulk = zi(i,kt) - z(i,pver)
          else
              lbulk = zi(i,kt) - zi(i,kb)
          end if

          ! Calculate 'turbulent length scale (leng)' and 'normalized TKE (wcap)'
          ! at all CL interfaces except the surface.  Note that below 'wcap' at 
          ! external interfaces are not correct. However, it does not influence 
          ! numerical calculation and correct normalized TKE at the entraining 
          ! interfaces will be re-calculated at the end of this 'do ncv' loop. 

          do k = min(kb,pver), kt, -1 
             if( choice_tunl .eq. 'rampcl' ) then
               ! In order to treat the case of 'ricl(i,ncv) >> 0' of surface-based SRCL
               ! with 'bflxs(i) < 0._r8', I changed ricl(i,ncv) -> min(0._r8,ricl(i,ncv))
               ! in the below exponential. This is necessary to prevent the model crash
               ! by too large values (e.g., 700) of ricl(i,ncv)   
                 tunlramp = ctunl*tunl*(1._r8-(1._r8-1._r8/ctunl)*exp(min(0._r8,ricl(i,ncv))))
                 tunlramp = min(max(tunlramp,tunl),ctunl*tunl)
             elseif( choice_tunl .eq. 'rampsl' ) then
                 tunlramp = ctunl*tunl
               ! tunlramp = 0.765_r8
             else
                 tunlramp = tunl
             endif
             if( choice_leng .eq. 'origin' ) then
                 leng(i,k) = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
               ! leng(i,k) = vk*zi(i,k) / (1._r8+vk*zi(i,k)/(tunlramp*lbulk))
             else
                 leng(i,k) = min( vk*zi(i,k), tunlramp*lbulk )              
             endif
             wcap(i,k) = (leng(i,k)**2) * (-shcl(i,ncv)*n2(i,k)+smcl(i,ncv)*s2(i,k))
          end do ! k

          ! Calculate basic cross-interface variables ( jump condition ) across the 
          ! base external interface of CL.

          if( kb .lt. pver+1 ) then 

              jbzm = z(i,kb-1) - z(i,kb)                                      ! Interfacial layer thickness [m]
              jbsl = sl(i,kb-1) - sl(i,kb)                                    ! Interfacial jump of 'sl' [J/kg]
              jbqt = qt(i,kb-1) - qt(i,kb)                                    ! Interfacial jump of 'qt' [kg/kg]
              jbbu = n2(i,kb) * jbzm                                          ! Interfacial buoyancy jump [m/s2] considering saturation ( > 0 ) 
              jbbu = max(jbbu,jbumin)                                         ! Set minimum buoyancy jump, jbumin = 1.e-3
              jbu  = u(i,kb-1) - u(i,kb)                                      ! Interfacial jump of 'u' [m/s]
              jbv  = v(i,kb-1) - v(i,kb)                                      ! Interfacial jump of 'v' [m/s]
              ch   = (1._r8 -sflh(i,kb-1))*chu(i,kb) + sflh(i,kb-1)*chs(i,kb) ! Buoyancy coefficient just above the base interface
              cm   = (1._r8 -sflh(i,kb-1))*cmu(i,kb) + sflh(i,kb-1)*cms(i,kb) ! Buoyancy coefficient just above the base interface
              n2hb = (ch*jbsl + cm*jbqt)/jbzm                                 ! Buoyancy frequency [s-2] just above the base interface
              vyb  = n2hb*jbzm/jbbu                                           ! Ratio of 'n2hb/n2' at 'kb' interface
              vub  = min(1._r8,(jbu**2+jbv**2)/(jbbu*jbzm) )                  ! Ratio of 's2/n2 = 1/Ri' at 'kb' interface

          else 

            ! Below setting is necessary for consistent treatment when 'kb' is at the surface.
              jbbu = 0._r8
              n2hb = 0._r8
              vyb  = 0._r8
              vub  = 0._r8
              web  = 0._r8

          end if

          ! Calculate basic cross-interface variables ( jump condition ) across the 
          ! top external interface of CL. The meanings of variables are similar to
          ! the ones at the base interface.

          jtzm = z(i,kt-1) - z(i,kt)
          jtsl = sl(i,kt-1) - sl(i,kt)
          jtqt = qt(i,kt-1) - qt(i,kt)
          jtbu = n2(i,kt)*jtzm                                                ! Note : 'jtbu' is guaranteed positive by definition of CL top.
          jtbu = max(jtbu,jbumin)                                             ! But threshold it anyway to be sure.
          jtu  = u(i,kt-1) - u(i,kt)
          jtv  = v(i,kt-1) - v(i,kt)
          ch   = (1._r8 -sfuh(i,kt))*chu(i,kt) + sfuh(i,kt)*chs(i,kt) 
          cm   = (1._r8 -sfuh(i,kt))*cmu(i,kt) + sfuh(i,kt)*cms(i,kt) 
          n2ht = (ch*jtsl + cm*jtqt)/jtzm                       
          vyt  = n2ht*jtzm/jtbu                                  
          vut  = min(1._r8,(jtu**2+jtv**2)/(jtbu*jtzm))             

          ! Evaporative enhancement factor of entrainment rate at the CL top interface, evhc. 
          ! We take the full inversion strength to be 'jt2slv = slv(i,kt-2)-slv(i,kt)' 
          ! where 'kt-1' is in the ambiguous layer. However, for a cloud-topped CL overlain
          ! by another CL, it is possible that 'slv(i,kt-2) < slv(i,kt)'. To avoid negative
          ! or excessive evhc, we lower-bound jt2slv and upper-bound evhc.  Note 'jtslv' is
          ! used only for calculating 'evhc' : when calculating entrainment rate,   we will
          ! use normal interfacial buoyancy jump across CL top interface.

          evhc   = 1._r8
          jt2slv = 0._r8

        ! Modification : I should check whether below 'jbumin' produces reasonable limiting value.   
        !                In addition, our current formulation does not consider ice contribution. 

          if( choice_evhc .eq. 'orig' ) then

              if( ql(i,kt) .gt. qmin .and. ql(i,kt-1) .lt. qmin ) then 
                  jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
                  jt2slv = max( jt2slv, jbumin*slv(i,kt-1)/g )
                  evhc   = 1._r8 + a2l * a3l * latvap * ql(i,kt) / jt2slv
                  evhc   = min( evhc, evhcmax )
              end if

          elseif( choice_evhc .eq. 'ramp' ) then

              jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
              jt2slv = max( jt2slv, jbumin*slv(i,kt-1)/g )
              evhc   = 1._r8 + max(cldeff(i,kt)-cldeff(i,kt-1),0._r8) * a2l * a3l * latvap * ql(i,kt) / jt2slv
              evhc   = min( evhc, evhcmax )

          elseif( choice_evhc .eq. 'maxi' ) then

              qleff  = max( ql(i,kt-1), ql(i,kt) ) 
              jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
              jt2slv = max( jt2slv, jbumin*slv(i,kt-1)/g )
              evhc   = 1._r8 + a2l * a3l * latvap * qleff / jt2slv
              evhc   = min( evhc, evhcmax )

          endif

          ! Calculate cloud-top radiative cooling contribution to buoyancy production.
          ! Here,  'radf' [m2/s3] is additional buoyancy flux at the CL top interface 
          ! associated with cloud-top LW cooling being mainly concentrated near the CL
          ! top interface ( just below CL top interface ).  Contribution of SW heating
          ! within the cloud is not included in this radiative buoyancy production 
          ! since SW heating is more broadly distributed throughout the CL top layer. 

          lwp        = 0._r8
          opt_depth  = 0._r8
          radinvfrac = 0._r8 
          radf       = 0._r8

          if( choice_radf .eq. 'orig' ) then

              if( ql(i,kt) .gt. qmin .and. ql(i,kt-1) .lt. qmin ) then 

                  lwp       = ql(i,kt) * ( pi(i,kt+1) - pi(i,kt) ) / g
                  opt_depth = 156._r8 * lwp  ! Estimated LW optical depth in the CL top layer

                  ! Approximate LW cooling fraction concentrated at the inversion by using
                  ! polynomial approx to exact formula 1-2/opt_depth+2/(exp(opt_depth)-1))

                  radinvfrac  = opt_depth * ( 4._r8 + opt_depth ) / ( 6._r8 * ( 4._r8 + opt_depth ) + opt_depth**2 )
                  radf        = qrlw(i,kt) / ( pi(i,kt) - pi(i,kt+1) ) ! Cp*radiative cooling = [ W/kg ] 
                  radf        = max( radinvfrac * radf * ( zi(i,kt) - zi(i,kt+1) ), 0._r8 ) * chs(i,kt)
                ! We can disable cloud LW cooling contribution to turbulence by uncommenting:
                ! radf = 0._r8

              end if

          elseif( choice_radf .eq. 'ramp' ) then

                  lwp         = ql(i,kt) * ( pi(i,kt+1) - pi(i,kt) ) / g
                  opt_depth   = 156._r8 * lwp  ! Estimated LW optical depth in the CL top layer
                  radinvfrac  = opt_depth * ( 4._r8 + opt_depth ) / ( 6._r8 * ( 4._r8 + opt_depth ) + opt_depth**2 )
                  radinvfrac  = max(cldeff(i,kt)-cldeff(i,kt-1),0._r8) * radinvfrac 
                  radf        = qrlw(i,kt) / ( pi(i,kt) - pi(i,kt+1) ) ! Cp*radiative cooling [W/kg] 
                  radf        = max( radinvfrac * radf * ( zi(i,kt) - zi(i,kt+1) ), 0._r8 ) * chs(i,kt)

          elseif( choice_radf .eq. 'maxi' ) then

                ! Radiative flux divergence both in 'kt' and 'kt-1' layers are included 
                ! 1. From 'kt' layer
                  lwp         = ql(i,kt) * ( pi(i,kt+1) - pi(i,kt) ) / g
                  opt_depth   = 156._r8 * lwp  ! Estimated LW optical depth in the CL top layer
                  radinvfrac  = opt_depth * ( 4._r8 + opt_depth ) / ( 6._r8 * ( 4._r8 + opt_depth ) + opt_depth**2 )
                  radf        = max( radinvfrac * qrlw(i,kt) / ( pi(i,kt) - pi(i,kt+1) ) * ( zi(i,kt) - zi(i,kt+1) ), 0._r8 )
                ! 2. From 'kt-1' layer and add the contribution from 'kt' layer
                  lwp         = ql(i,kt-1) * ( pi(i,kt) - pi(i,kt-1) ) / g
                  opt_depth   = 156._r8 * lwp  ! Estimated LW optical depth in the CL top layer
                  radinvfrac  = opt_depth * ( 4._r8 + opt_depth ) / ( 6._r8 * ( 4._r8 + opt_depth) + opt_depth**2 )
                  radf        = radf + max( radinvfrac * qrlw(i,kt-1) / ( pi(i,kt-1) - pi(i,kt) ) * ( zi(i,kt-1) - zi(i,kt) ), &
                                            0._r8 )
                  radf        = max( radf, 0._r8 ) * chs(i,kt) 

          endif

          ! ------------------------------------------------------------------- !
          ! Calculate 'wstar3' by summing buoyancy productions within CL from   !
          !   1. Interior buoyancy production ( bprod: fcn of TKE )             !
          !   2. Cloud-top radiative cooling                                    !
          !   3. Surface buoyancy flux contribution only when bflxs > 0.        !
          !      Note that master length scale, lbulk, has already been         !
          !      corrctly defined at the first part of this 'do ncv' loop       !
          !      considering the sign of bflxs.                                 !
          ! This 'wstar3' is used for calculation of entrainment rate.          !
          ! Note that this 'wstar3' formula does not include shear production   !
          ! and the effect of drizzle, which should be included later.          !
          ! Q : Strictly speaking, in calculating interior buoyancy production, ! 
          !     the use of 'bprod' is not correct, since 'bprod' is not correct !
          !     value but initially guessed value.   More reasonably, we should ! 
          !     use '-leng(i,k)*sqrt(b1*wcap(i,k))*shcl(i,ncv)*n2(i,k)' instead !
          !     of 'bprod(i,k)', although this is still an  approximation since !
          !     tke(i,k) is not exactly 'b1*wcap(i,k)'  due to a transport term.! 
          !     However since iterative calculation will be performed after all,! 
          !     below might also be OK. But I should test this alternative.     !
          ! ------------------------------------------------------------------- !      

          dzht   = zi(i,kt)  - z(i,kt)     ! Thickness of CL top half-layer
          dzhb   = z(i,kb-1) - zi(i,kb)    ! Thickness of CL bot half-layer
          wstar3 = radf * dzht
          do k = kt + 1, kb - 1 ! If 'kt = kb - 1', this loop will not be performed. 
               wstar3 =  wstar3 + bprod(i,k) * ( z(i,k-1) - z(i,k) )
             ! Below is an alternative which may speed up convergence.
             ! However, for interfaces merged into original CL, it can
             ! be 'wcap(i,k)<0' since 'n2(i,k)>0'.  Thus, I should use
             ! the above original one.
             ! wstar3 =  wstar3 - leng(i,k)*sqrt(b1*wcap(i,k))*shcl(i,ncv)*n2(i,k)* &
             !                    (z(i,k-1) - z(i,k))
          end do      
          if( kb .eq. (pver+1) .and. bflxs(i) .gt. 0._r8 ) then
             wstar3 = wstar3 + bflxs(i) * dzhb
           ! wstar3 = wstar3 + bprod(i,pver+1) * dzhb
          end if   
          wstar3 = max( 2.5_r8 * wstar3, 0._r8 )
   
          ! -------------------------------------------------------------- !
          ! Below single block is for 'sedimentation-entrainment feedback' !
          ! -------------------------------------------------------------- !          

          if( id_sedfact ) then
            ! wsed    = 7.8e5_r8*(ql(i,kt)/ncliq(i,kt))**(2._r8/3._r8)
              sedfact = exp(-ased*wsedl(i,kt)/(wstar3**(1._r8/3._r8)+1.e-6))
              if( choice_evhc .eq. 'orig' ) then
                  if (ql(i,kt).gt.qmin .and. ql(i,kt-1).lt.qmin) then
                      jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
                      jt2slv = max(jt2slv, jbumin*slv(i,kt-1)/g)
                      evhc = 1._r8+sedfact*a2l*a3l*latvap*ql(i,kt) / jt2slv
                      evhc = min(evhc,evhcmax)
                  end if
              elseif( choice_evhc .eq. 'ramp' ) then
                  jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
                  jt2slv = max(jt2slv, jbumin*slv(i,kt-1)/g)
                  evhc = 1._r8+max(cldeff(i,kt)-cldeff(i,kt-1),0._r8)*sedfact*a2l*a3l*latvap*ql(i,kt) / jt2slv
                  evhc = min(evhc,evhcmax)
              elseif( choice_evhc .eq. 'maxi' ) then
                  qleff  = max(ql(i,kt-1),ql(i,kt))
                  jt2slv = slv(i,max(kt-2,1)) - slv(i,kt)
                  jt2slv = max(jt2slv, jbumin*slv(i,kt-1)/g)
                  evhc = 1._r8+sedfact*a2l*a3l*latvap*qleff / jt2slv
                  evhc = min(evhc,evhcmax)
              endif
          endif

          ! -------------------------------------------------------------------------- !
          ! Now diagnose CL top and bottom entrainment rates (and the contribution of  !
          ! top/bottom entrainments to wstar3) using entrainment closures of the form  !
          !                                                                            !        
          !                   wet = cet*wstar3, web = ceb*wstar3                       !
          !                                                                            !
          ! where cet and ceb depend on the entrainment interface jumps, ql, etc.      !
          ! No entrainment is diagnosed unless the wstar3 > 0. Note '1/wstar3fact' is  !
          ! a factor indicating the enhancement of wstar3 due to entrainment process.  !
          ! Q : Below setting of 'wstar3fact = max(..,0.5)'might prevent the possible  !
          !     case when buoyancy consumption by entrainment is  stronger than cloud  !
          !     top radiative cooling production. Is that OK ? No.  According to bulk  !
          !     modeling study, entrainment buoyancy consumption was always a certain  !
          !     fraction of other net productions, rather than a separate sum.  Thus,  !
          !     below max limit of wstar3fact is correct.   'wstar3fact = max(.,0.5)'  !
          !     prevents unreasonable enhancement of CL entrainment rate by cloud-top  !
          !     entrainment instability, CTEI.                                         !
          ! Q : Use of the same dry entrainment coefficient, 'a1i' both at the CL  top !
          !     and base interfaces may result in too small 'wstar3' and 'ebrk' below, !
          !     as was seen in my generalized bulk modeling study. This should be re-  !
          !     considered later                                                       !
          ! -------------------------------------------------------------------------- !
          
          if( wstar3 .gt. 0._r8 ) then
              cet = a1i * evhc / ( jtbu * lbulk )
              if( kb .eq. pver + 1 ) then 
                  wstar3fact = max( 1._r8 + 2.5_r8 * cet * n2ht * jtzm * dzht, wstar3factcrit )
              else    
                  ceb = a1i / ( jbbu * lbulk )
                  wstar3fact = max( 1._r8 + 2.5_r8 * cet * n2ht * jtzm * dzht &
                                          + 2.5_r8 * ceb * n2hb * jbzm * dzhb, wstar3factcrit )
              end if
              wstar3 = wstar3 / wstar3fact       
          else ! wstar3 == 0
              wstar3fact = 0._r8 ! This is just for dianostic output
              cet        = 0._r8
              ceb        = 0._r8
          end if 

          ! ---------------------------------------------------------------------------- !
          ! Calculate net CL mean TKE including entrainment contribution by solving a    !
          ! canonical cubic equation. The solution of cubic equ. is 'rootp**2 = ebrk'    !
          ! where 'ebrk' originally (before solving cubic eq.) was interior CL mean TKE, !
          ! but after solving cubic equation,  it is replaced by net CL mean TKE in the  !
          ! same variable 'ebrk'.                                                        !
          ! ---------------------------------------------------------------------------- !
          ! Solve cubic equation (canonical form for analytic solution)                  !
          !   r^3 - 3*trmp*r - 2*trmq = 0,   r = sqrt<e>                                 ! 
          ! to estimate <e> for CL, derived from layer-mean TKE balance:                 !
          !                                                                              !
          !   <e>^(3/2)/(b_1*<l>) \approx <B + S>   (*)                                  !
          !   <B+S> = (<B+S>_int * l_int + <B+S>_et * dzt + <B+S>_eb * dzb)/lbulk        !
          !   <B+S>_int = <e>^(1/2)/(b_1*<l>)*<e>_int                                    !
          !   <B+S>_et  = (-vyt+vut)*wet*jtbu + radf                                     !
          !   <B+S>_eb  = (-vyb+vub)*web*jbbu                                            !
          !                                                                              !
          ! where:                                                                       !
          !   <> denotes a vertical avg (over the whole CL unless indicated)             !
          !   l_int (called lbrk below) is aggregate thickness of interior CL layers     !
          !   dzt = zi(i,kt)-z(i,kt)   is thickness of top entrainment layer             !
          !   dzb = z(i,kb-1)-zi(i,kb) is thickness of bot entrainment layer             !
          !   <e>_int (called ebrk below) is the CL-mean TKE if only interior            !
          !                               interfaces contributed.                        !
          !   wet, web                  are top. bottom entrainment rates                !
          !                                                                              !
          ! For a single-level radiatively-driven convective layer, there are no         ! 
          ! interior interfaces so 'ebrk' = 'lbrk' = 0. If the CL goes to the            !
          ! surface, 'vyb' and 'vub' are set to zero before and 'ebrk' and 'lbrk'        !
          ! have already incorporated the surface interfacial layer contribution,        !
          ! so the same formulas still apply.                                            !
          !                                                                              !
          ! In the original formulation based on TKE,                                    !
          !    wet*jtbu = a1l*evhc*<e>^3/2/leng(i,kt)                                    ! 
          !    web*jbbu = a1l*<e>^3/2/leng(i,kt)                                         !
          !                                                                              !
          ! In the wstar formulation                                                     !
          !    wet*jtbu = a1i*evhc*wstar3/lbulk                                          !
          !    web*jbbu = a1i*wstar3/lbulk,                                              !
          ! ---------------------------------------------------------------------------- !

          fact = ( evhc * ( -vyt + vut ) * dzht + ( -vyb + vub ) * dzhb * leng(i,kb) / leng(i,kt) ) / lbulk

          if( wstarent ) then

              ! (Option 1) 'wstar' entrainment formulation 
              ! Here trmq can have either sign, and will usually be nonzero even for non-
              ! cloud topped CLs.  If trmq > 0, there will be two positive roots r; we take 
              ! the larger one. Why ? If necessary, we limit entrainment and wstar to prevent
              ! a solution with r < ccrit*wstar ( Why ? ) where we take ccrit = 0.5. 

              trma = 1._r8          
              trmp = ebrk(i,ncv) * ( lbrk(i,ncv) / lbulk ) / 3._r8 + ntzero
              trmq = 0.5_r8 * b1 * ( leng(i,kt)  / lbulk ) * ( radf * dzht + a1i * fact * wstar3 )

              ! Check if there is an acceptable root with r > rcrit = ccrit*wstar. 
              ! To do this, first find local minimum fmin of the cubic f(r) at sqrt(p), 
              ! and value fcrit = f(rcrit).

              rmin  = sqrt(trmp)
              fmin  = rmin * ( rmin * rmin - 3._r8 * trmp ) - 2._r8 * trmq
              wstar = wstar3**onet
              rcrit = ccrit * wstar
              fcrit = rcrit * ( rcrit * rcrit - 3._r8 * trmp ) - 2._r8 * trmq

              ! No acceptable root exists (noroot = .true.) if either:
              !    1) rmin < rcrit (in which case cubic is monotone increasing for r > rcrit)
              !       and f(rcrit) > 0.
              ! or 2) rmin > rcrit (in which case min of f(r) in r > rcrit is at rmin)
              !       and f(rmin) > 0.  
              ! In this case, we reduce entrainment and wstar3 such that r/wstar = ccrit;
              ! this changes the coefficients of the cubic.   It might be informative to
              ! check when and how many 'noroot' cases occur,  since when 'noroot',   we
              ! will impose arbitrary limit on 'wstar3, wet, web, and ebrk' using ccrit.

              noroot = ( ( rmin .lt. rcrit ) .and. ( fcrit .gt. 0._r8 ) ) &
                  .or. ( ( rmin .ge. rcrit ) .and. ( fmin  .gt. 0._r8 ) )
              if( noroot ) then ! Solve cubic for r
                  trma = 1._r8 - b1 * ( leng(i,kt) / lbulk ) * a1i * fact / ccrit**3
                  trma = max( trma, 0.5_r8 )  ! Limit entrainment enhancement of ebrk
                  trmp = trmp / trma 
                  trmq = 0.5_r8 * b1 * ( leng(i,kt) / lbulk ) * radf * dzht / trma
              end if   ! noroot

              ! Solve the cubic equation

              qq = trmq**2 - trmp**3
              if( qq .ge. 0._r8 ) then 
                  rootp = ( trmq + sqrt(qq) )**(1._r8/3._r8) + ( max( trmq - sqrt(qq), 0._r8 ) )**(1._r8/3._r8)
              else
                  rootp = 2._r8 * sqrt(trmp) * cos( acos( trmq / sqrt(trmp**3) ) / 3._r8 )
              end if
 
              ! Adjust 'wstar3' only if there is 'noroot'. 
              ! And calculate entrainment rates at the top and base interfaces.

              if( noroot )  wstar3 = ( rootp / ccrit )**3     ! Adjust wstar3 
              wet = cet * wstar3                              ! Find entrainment rates
              if( kb .lt. pver + 1 ) web = ceb * wstar3       ! When 'kb.eq.pver+1', it was set to web=0. 

          else !

              ! (Option.2) wstarentr = .false. Use original entrainment formulation.
              ! trmp > 0 if there are interior interfaces in CL, trmp = 0 otherwise.
              ! trmq > 0 if there is cloudtop radiative cooling, trmq = 0 otherwise.
             
              trma = 1._r8 - b1 * a1l * fact
              trma = max( trma, 0.5_r8 )  ! Prevents runaway entrainment instability
              trmp = ebrk(i,ncv) * ( lbrk(i,ncv) / lbulk ) / ( 3._r8 * trma )
              trmq = 0.5_r8 * b1 * ( leng(i,kt)  / lbulk ) * radf * dzht / trma

              qq = trmq**2 - trmp**3
              if( qq .ge. 0._r8 ) then 
                  rootp = ( trmq + sqrt(qq) )**(1._r8/3._r8) + ( max( trmq - sqrt(qq), 0._r8 ) )**(1._r8/3._r8)
              else ! Also part of case 3
                  rootp = 2._r8 * sqrt(trmp) * cos( acos( trmq / sqrt(trmp**3) ) / 3._r8 )
              end if   ! qq

             ! Find entrainment rates and limit them by free-entrainment values a1l*sqrt(e)

              wet = a1l * rootp * min( evhc * rootp**2 / ( leng(i,kt) * jtbu ), 1._r8 )   
              if( kb .lt. pver + 1 ) web = a1l * rootp * min( evhc * rootp**2 / ( leng(i,kb) * jbbu ), 1._r8 )

          end if ! wstarentr

          ! ---------------------------------------------------- !
          ! Finally, get the net CL mean TKE and normalized TKE  ! 
          ! ---------------------------------------------------- !

          ebrk(i,ncv) = rootp**2
          ebrk(i,ncv) = min(ebrk(i,ncv),tkemax) ! Limit CL-avg TKE used for entrainment
          wbrk(i,ncv) = ebrk(i,ncv)/b1  
        
          ! The only way ebrk = 0 is for SRCL which are actually radiatively cooled 
          ! at top interface. In this case, we remove 'convective' label from the 
          ! interfaces around this layer. This case should now be impossible, so 
          ! we flag it. Q: I can't understand why this case is impossible now. Maybe,
          ! due to various limiting procedures used in solving cubic equation ? 
          ! In case of SRCL, 'ebrk' should be positive due to cloud top LW radiative
          ! cooling contribution, although 'ebrk(internal)' of SRCL before including
          ! entrainment contribution (which include LW cooling contribution also) is
          ! zero. 

          if( ebrk(i,ncv) .le. 0._r8 ) then
              write(iulog,*) 'CALEDDY: Warning, CL with zero TKE, i, kt, kb ', i, kt, kb
#ifdef WRF_PORT
              call wrf_message(iulog)
#endif               
              belongcv(i,kt) = .false.
              belongcv(i,kb) = .false. 
          end if
          
          ! ----------------------------------------------------------------------- !
          ! Calculate complete TKE profiles at all CL interfaces, capped by tkemax. !
          ! We approximate TKE = <e> at entrainment interfaces. However when CL is  !
          ! based at surface, correct 'tkes' will be inserted to tke(i,pver+1).     !
          ! Note that this approximation at CL external interfaces do not influence !
          ! numerical calculation since 'e' at external interfaces are not used  in !
          ! actual numerical calculation afterward. In addition in order to extract !
          ! correct TKE averaged over the PBL in the cumulus scheme,it is necessary !
          ! to set e = <e> at the top entrainment interface.  Since net CL mean TKE !
          ! 'ebrk' obtained by solving cubic equation already includes tkes  ( tkes !
          ! is included when bflxs > 0 but not when bflxs <= 0 into internal ebrk ),!
          ! 'tkes' should be written to tke(i,pver+1)                               !
          ! ----------------------------------------------------------------------- !

          ! 1. At internal interfaces          
          do k = kb - 1, kt + 1, -1
             rcap = ( b1 * ae + wcap(i,k) / wbrk(i,ncv) ) / ( b1 * ae + 1._r8 )
             rcap = min( max(rcap,rcapmin), rcapmax )
             tke(i,k) = ebrk(i,ncv) * rcap
             tke(i,k) = min( tke(i,k), tkemax )
             kvh(i,k) = leng(i,k) * sqrt(tke(i,k)) * shcl(i,ncv)
             kvm(i,k) = leng(i,k) * sqrt(tke(i,k)) * smcl(i,ncv)
             bprod(i,k) = -kvh(i,k) * n2(i,k)
             sprod(i,k) =  kvm(i,k) * s2(i,k)
             turbtype(i,k) = 2                     ! CL interior interfaces.
             sm_aw(i,k) = smcl(i,ncv)/alph1        ! Diagnostic output for microphysics
          end do

          ! 2. At CL top entrainment interface
          kentr = wet * jtzm
          kvh(i,kt) = kentr
          kvm(i,kt) = kentr
          bprod(i,kt) = -kentr * n2ht + radf       ! I must use 'n2ht' not 'n2'
          sprod(i,kt) =  kentr * s2(i,kt)
          turbtype(i,kt) = 4                       ! CL top entrainment interface
          trmp = -b1 * ae / ( 1._r8 + b1 * ae )
          trmq = -(bprod(i,kt)+sprod(i,kt))*b1*leng(i,kt)/(1._r8+b1*ae)/(ebrk(i,ncv)**(3._r8/2._r8))
          rcap = compute_cubic(0._r8,trmp,trmq)**2._r8
          rcap = min( max(rcap,rcapmin), rcapmax )
          tke(i,kt)  = ebrk(i,ncv) * rcap
          tke(i,kt)  = min( tke(i,kt), tkemax )
          sm_aw(i,kt) = smcl(i,ncv) / alph1        ! Diagnostic output for microphysics

          ! 3. At CL base entrainment interface and double entraining interfaces
          ! When current CL base is also the top interface of CL regime below,
          ! simply add the two contributions for calculating eddy diffusivity
          ! and buoyancy/shear production. Below code correctly works because
          ! we (CL regime index) always go from surface upward.

          if( kb .lt. pver + 1 ) then 

              kentr = web * jbzm

              if( kb .ne. ktblw ) then

                  kvh(i,kb) = kentr
                  kvm(i,kb) = kentr
                  bprod(i,kb) = -kvh(i,kb)*n2hb     ! I must use 'n2hb' not 'n2'
                  sprod(i,kb) =  kvm(i,kb)*s2(i,kb)
                  turbtype(i,kb) = 3                ! CL base entrainment interface
                  trmp = -b1*ae/(1._r8+b1*ae)
                  trmq = -(bprod(i,kb)+sprod(i,kb))*b1*leng(i,kb)/(1._r8+b1*ae)/(ebrk(i,ncv)**(3._r8/2._r8))
                  rcap = compute_cubic(0._r8,trmp,trmq)**2._r8
                  rcap = min( max(rcap,rcapmin), rcapmax )
                  tke(i,kb)  = ebrk(i,ncv) * rcap
                  tke(i,kb)  = min( tke(i,kb),tkemax )

              else
                  
                  kvh(i,kb) = kvh(i,kb) + kentr 
                  kvm(i,kb) = kvm(i,kb) + kentr
                ! dzhb5 : Half thickness of the lowest  layer of  current CL regime
                ! dzht5 : Half thickness of the highest layer of adjacent CL regime just below current CL. 
                  dzhb5 = z(i,kb-1) - zi(i,kb)
                  dzht5 = zi(i,kb) - z(i,kb)
                  bprod(i,kb) = ( dzht5*bprod(i,kb) - dzhb5*kentr*n2hb )     / ( dzhb5 + dzht5 )
                  sprod(i,kb) = ( dzht5*sprod(i,kb) + dzhb5*kentr*s2(i,kb) ) / ( dzhb5 + dzht5 )
                  trmp = -b1*ae/(1._r8+b1*ae)
                  trmq = -kentr*(s2(i,kb)-n2hb)*b1*leng(i,kb)/(1._r8+b1*ae)/(ebrk(i,ncv)**(3._r8/2._r8))
                  rcap = compute_cubic(0._r8,trmp,trmq)**2._r8
                  rcap = min( max(rcap,rcapmin), rcapmax )
                  tke_imsi = ebrk(i,ncv) * rcap
                  tke_imsi = min( tke_imsi, tkemax )
                  tke(i,kb)  = ( dzht5*tke(i,kb) + dzhb5*tke_imsi ) / ( dzhb5 + dzht5 )               
                  tke(i,kb)  = min(tke(i,kb),tkemax)
                  turbtype(i,kb) = 5                ! CL double entraining interface      
                 
              end if

           else

             ! If CL base interface is surface, compute similarly using wcap(i,kb)=tkes/b1    
             ! Even when bflx < 0, use the same formula in order to impose consistency of
             ! tke(i,kb) at bflx = 0._r8
 
             rcap = (b1*ae + wcap(i,kb)/wbrk(i,ncv))/(b1*ae + 1._r8)
             rcap = min( max(rcap,rcapmin), rcapmax )
             tke(i,kb) = ebrk(i,ncv) * rcap
             tke(i,kb) = min( tke(i,kb),tkemax )

          end if

          ! For double entraining interface, simply use smcl(i,ncv) of the overlying CL. 
          ! Below 'sm_aw' is a diagnostic output for use in the microphysics.
          ! When 'kb' is surface, 'sm' will be over-written later below.

          sm_aw(i,kb) = smcl(i,ncv)/alph1             

          ! Calculate wcap at all interfaces of CL. Put a  minimum threshold on TKE
          ! to prevent possible division by zero.  'wcap' at CL internal interfaces
          ! are already calculated in the first part of 'do ncv' loop correctly.
          ! When 'kb.eq.pver+1', below formula produces the identical result to the
          ! 'tkes(i)/b1' if leng(i,kb) is set to vk*z(i,pver). Note  wcap(i,pver+1)
          ! is already defined as 'tkes(i)/b1' at the first part of caleddy.
          
          wcap(i,kt) = (bprod(i,kt)+sprod(i,kt))*leng(i,kt)/sqrt(max(tke(i,kt),1.e-6_r8))
          if( kb .lt. pver + 1 ) then
              wcap(i,kb) = (bprod(i,kb)+sprod(i,kb))*leng(i,kb)/sqrt(max(tke(i,kb),1.e-6_r8))
          end if

          ! Save the index of upper external interface of current CL-regime in order to
          ! handle the case when this interface is also the lower external interface of 
          ! CL-regime located just above. 

          ktblw = kt 

          ! Diagnostic Output

          wet_CL(i,ncv)        = wet
          web_CL(i,ncv)        = web
          jtbu_CL(i,ncv)       = jtbu
          jbbu_CL(i,ncv)       = jbbu
          evhc_CL(i,ncv)       = evhc
          jt2slv_CL(i,ncv)     = jt2slv
          n2ht_CL(i,ncv)       = n2ht
          n2hb_CL(i,ncv)       = n2hb          
          lwp_CL(i,ncv)        = lwp
          opt_depth_CL(i,ncv)  = opt_depth
          radinvfrac_CL(i,ncv) = radinvfrac
          radf_CL(i,ncv)       = radf
          wstar_CL(i,ncv)      = wstar          
          wstar3fact_CL(i,ncv) = wstar3fact          

       end do        ! ncv
 
       ! Calculate PBL height and characteristic cumulus excess for use in the
       ! cumulus convection shceme. Also define turbulence type at the surface
       ! when the lowest CL is based at the surface. These are just diagnostic
       ! outputs, not influencing numerical calculation of current PBL scheme.
       ! If the lowest CL is based at the surface, define the PBL depth as the
       ! CL top interface. The same rule is applied for all CLs including SRCL.

       if( ncvsurf .gt. 0 ) then

           ktopbl(i) = ktop(i,ncvsurf)
           pblh(i)   = zi(i, ktopbl(i))
           pblhp(i)  = pi(i, ktopbl(i))
           wpert(i)  = max(wfac*sqrt(ebrk(i,ncvsurf)),wpertmin)
           tpert(i)  = max(abs(shflx(i)*rrho(i)/cpair)*tfac/wpert(i),0._r8)
           qpert(i)  = max(abs(qflx(i)*rrho(i))*tfac/wpert(i),0._r8)

           if( bflxs(i) .gt. 0._r8 ) then
               turbtype(i,pver+1) = 2 ! CL interior interface
           else
               turbtype(i,pver+1) = 3 ! CL external base interface
           endif

           ipbl(i)  = 1._r8
           kpblh(i) = ktopbl(i) - 1._r8

       end if ! End of the calculationf of te properties of surface-based CL.

       ! -------------------------------------------- !
       ! Treatment of Stable Turbulent Regime ( STL ) !
       ! -------------------------------------------- !

       ! Identify top and bottom most (internal) interfaces of STL except surface.
       ! Also, calculate 'turbulent length scale (leng)' at each STL interfaces.     

       belongst(i,1) = .false.   ! k = 1 (top interface) is assumed non-turbulent
       do k = 2, pver            ! k is an interface index
          belongst(i,k) = ( ri(i,k) .lt. ricrit ) .and. ( .not. belongcv(i,k) )
          if( belongst(i,k) .and. ( .not. belongst(i,k-1) ) ) then
              kt = k             ! Top interface index of STL
          elseif( .not. belongst(i,k) .and. belongst(i,k-1) ) then
              kb = k - 1         ! Base interface index of STL
              lbulk = z(i,kt-1) - z(i,kb)
              do ks = kt, kb
                 if( choice_tunl .eq. 'rampcl' ) then
                     tunlramp = tunl
                 elseif( choice_tunl .eq. 'rampsl' ) then
                    tunlramp = max( 1.e-3_r8, ctunl * tunl * exp(-log(ctunl)*ri(i,ks)/ricrit) )
                  ! tunlramp = 0.065_r8 + 0.7_r8 * exp(-20._r8*ri(i,ks))
                 else
                    tunlramp = tunl
                 endif
                 if( choice_leng .eq. 'origin' ) then
                     leng(i,ks) = ( (vk*zi(i,ks))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                   ! leng(i,ks) = vk*zi(i,ks) / (1._r8+vk*zi(i,ks)/(tunlramp*lbulk))
                 else
                     leng(i,ks) = min( vk*zi(i,ks), tunlramp*lbulk )              
                 endif
              end do
          end if
       end do ! k

       ! Now look whether STL extends to ground.  If STL extends to surface,
       ! re-define master length scale,'lbulk' including surface interfacial
       ! layer thickness, and re-calculate turbulent length scale, 'leng' at
       ! all STL interfaces again. Note that surface interface is assumed to
       ! always be STL if it is not CL.   
       
       belongst(i,pver+1) = .not. belongcv(i,pver+1)

       if( belongst(i,pver+1) ) then     ! kb = pver+1 (surface  STL)

           turbtype(i,pver+1) = 1        ! Surface is STL interface
          
           if( belongst(i,pver) ) then   ! STL includes interior
             ! 'kt' already defined above as the top interface of STL
               lbulk = z(i,kt-1)          
           else                          ! STL with no interior turbulence
               kt = pver+1
               lbulk = z(i,kt-1)
           end if

           ! PBL height : Layer mid-point just above the highest STL interface
           ! Note in contrast to the surface based CL regime where  PBL height
           ! was defined at the top external interface, PBL height of  surface
           ! based STL is defined as the layer mid-point.

           ktopbl(i) = kt - 1
           pblh(i)   = z(i,ktopbl(i))
           pblhp(i)  = 0.5_r8 * ( pi(i,ktopbl(i)) + pi(i,ktopbl(i)+1) )          

           ! Re-calculate turbulent length scale including surface interfacial
           ! layer contribution to lbulk.

           do ks = kt, pver
              if( choice_tunl .eq. 'rampcl' ) then
                  tunlramp = tunl
              elseif( choice_tunl .eq. 'rampsl' ) then
                  tunlramp = max(1.e-3_r8,ctunl*tunl*exp(-log(ctunl)*ri(i,ks)/ricrit))
                ! tunlramp = 0.065_r8 + 0.7_r8 * exp(-20._r8*ri(i,ks))
              else
                  tunlramp = tunl
              endif
              if( choice_leng .eq. 'origin' ) then
                  leng(i,ks) = ( (vk*zi(i,ks))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                ! leng(i,ks) = vk*zi(i,ks) / (1._r8+vk*zi(i,ks)/(tunlramp*lbulk))
              else
                  leng(i,ks) = min( vk*zi(i,ks), tunlramp*lbulk )              
              endif
           end do ! ks

           ! Characteristic cumulus excess of surface-based STL.
           ! We may be able to use ustar for wpert.

           wpert(i) = 0._r8 
           tpert(i) = max(shflx(i)*rrho(i)/cpair*fak/ustar(i),0._r8) ! CCM stable-layer forms
           qpert(i) = max(qflx(i)*rrho(i)*fak/ustar(i),0._r8)

           ipbl(i)  = 0._r8
           kpblh(i) = ktopbl(i)

       end if

       ! Calculate stability functions and energetics at the STL interfaces
       ! except the surface. Note that tke(i,pver+1) and wcap(i,pver+1) are
       ! already calculated in the first part of 'caleddy', kvm(i,pver+1) &
       ! kvh(i,pver+1) were already initialized to be zero, bprod(i,pver+1)
       ! & sprod(i,pver+1) were direcly calculated from the bflxs and ustar.
       ! Note transport term is assumed to be negligible at STL interfaces.
           
       do k = 2, pver

          if( belongst(i,k) ) then

              turbtype(i,k) = 1    ! STL interfaces
              trma = alph3*alph4exs*ri(i,k) + 2._r8*b1*(alph2-alph4exs*alph5*ri(i,k))
              trmb = (alph3+alph4exs)*ri(i,k) + 2._r8*b1*(-alph5*ri(i,k)+alph1)
              trmc = ri(i,k)
              det = max(trmb*trmb-4._r8*trma*trmc,0._r8)
              ! Sanity Check
              if( det .lt. 0._r8 ) then
                  write(iulog,*) 'The det < 0. for the STL in UW eddy_diff'             
#ifdef WRF_PORT
                  call wrf_message(iulog)
#endif                   
                  stop
              end if                  
              gh = (-trmb + sqrt(det))/(2._r8*trma)
            ! gh = min(max(gh,-0.28_r8),0.0233_r8)
            ! gh = min(max(gh,-3.5334_r8),0.0233_r8)
              gh = min(max(gh,ghmin),0.0233_r8)
              sh = max(0._r8,alph5/(1._r8+alph3*gh))
              sm = max(0._r8,(alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4exs*gh))

              tke(i,k)   = b1*(leng(i,k)**2)*(-sh*n2(i,k)+sm*s2(i,k))
              tke(i,k)   = min(tke(i,k),tkemax)
              wcap(i,k)  = tke(i,k)/b1
              kvh(i,k)   = leng(i,k) * sqrt(tke(i,k)) * sh
              kvm(i,k)   = leng(i,k) * sqrt(tke(i,k)) * sm
              bprod(i,k) = -kvh(i,k) * n2(i,k)
              sprod(i,k) =  kvm(i,k) * s2(i,k)

              sm_aw(i,k) = sm/alph1     ! This is diagnostic output for use in the microphysics             

          end if

       end do  ! k

       ! --------------------------------------------------- !
       ! End of treatment of Stable Turbulent Regime ( STL ) !
       ! --------------------------------------------------- !

       ! --------------------------------------------------------------- !
       ! Re-computation of eddy diffusivity at the entrainment interface !
       ! assuming that it is purely STL (0<Ri<0.19). Note even Ri>0.19,  !
       ! turbulent can exist at the entrainment interface since 'Sh,Sm'  !
       ! do not necessarily go to zero even when Ri>0.19. Since Ri can   !
       ! be fairly larger than 0.19 at the entrainment interface, I      !
       ! should set minimum value of 'tke' to be 0. in order to prevent  !
       ! sqrt(tke) from being imaginary.                                 !
       ! --------------------------------------------------------------- !

       ! goto 888

         do k = 2, pver

         if( ( turbtype(i,k) .eq. 3 ) .or. ( turbtype(i,k) .eq. 4 ) .or. &
             ( turbtype(i,k) .eq. 5 ) ) then

             trma = alph3*alph4exs*ri(i,k) + 2._r8*b1*(alph2-alph4exs*alph5*ri(i,k))
             trmb = (alph3+alph4exs)*ri(i,k) + 2._r8*b1*(-alph5*ri(i,k)+alph1)
             trmc = ri(i,k)
             det  = max(trmb*trmb-4._r8*trma*trmc,0._r8)
             gh   = (-trmb + sqrt(det))/(2._r8*trma)
           ! gh   = min(max(gh,-0.28_r8),0.0233_r8)
           ! gh   = min(max(gh,-3.5334_r8),0.0233_r8)
             gh   = min(max(gh,ghmin),0.0233_r8)
             sh   = max(0._r8,alph5/(1._r8+alph3*gh))
             sm   = max(0._r8,(alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4exs*gh))

             lbulk = z(i,k-1) - z(i,k)

             if( choice_tunl .eq. 'rampcl' ) then
                 tunlramp = tunl
             elseif( choice_tunl .eq. 'rampsl' ) then
                 tunlramp = max(1.e-3_r8,ctunl*tunl*exp(-log(ctunl)*ri(i,k)/ricrit))
               ! tunlramp = 0.065_r8 + 0.7_r8*exp(-20._r8*ri(i,k))
             else
                 tunlramp = tunl
             endif
             if( choice_leng .eq. 'origin' ) then
                 leng_imsi = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
               ! leng_imsi = vk*zi(i,k) / (1._r8+vk*zi(i,k)/(tunlramp*lbulk))
             else
                 leng_imsi = min( vk*zi(i,k), tunlramp*lbulk )              
             endif

             tke_imsi = b1*(leng_imsi**2)*(-sh*n2(i,k)+sm*s2(i,k))
             tke_imsi = min(max(tke_imsi,0._r8),tkemax)
             kvh_imsi = leng_imsi * sqrt(tke_imsi) * sh
             kvm_imsi = leng_imsi * sqrt(tke_imsi) * sm

             if( kvh(i,k) .lt. kvh_imsi ) then 
                 kvh(i,k)   =  kvh_imsi
                 kvm(i,k)   =  kvm_imsi
                 leng(i,k)  = leng_imsi
                 tke(i,k)   =  tke_imsi
                 wcap(i,k)  =  tke_imsi / b1
                 bprod(i,k) = -kvh_imsi * n2(i,k)
                 sprod(i,k) =  kvm_imsi * s2(i,k)
                 sm_aw(i,k) =  sm/alph1     ! This is diagnostic output for use in the microphysics             
                 turbtype(i,k) = 1          ! This was added on Dec.10.2009 for use in microphysics.
             endif

         end if

         end do

 ! 888   continue 

       ! ------------------------------------------------------------------ !
       ! End of recomputation of eddy diffusivity at entrainment interfaces !
       ! ------------------------------------------------------------------ !

       ! As an option, we can impose a certain minimum back-ground diffusivity.

       ! do k = 1, pver+1
       !    kvh(i,k) = max(0.01_r8,kvh(i,k))
       !    kvm(i,k) = max(0.01_r8,kvm(i,k))
       ! enddo
 
       ! --------------------------------------------------------------------- !
       ! Diagnostic Output                                                     !
       ! Just for diagnostic purpose, calculate stability functions at  each   !
       ! interface including surface. Instead of assuming neutral stability,   !
       ! explicitly calculate stability functions using an reverse procedure   !
       ! starting from tkes(i) similar to the case of SRCL and SBCL in zisocl. !
       ! Note that it is possible to calculate stability functions even when   !
       ! bflxs < 0. Note that this inverse method allows us to define Ri even  !
       ! at the surface. Note also tkes(i) and sprod(i,pver+1) are always      !
       ! positive values by limiters (e.g., ustar_min = 0.01).                 !
       ! Dec.12.2006 : Also just for diagnostic output, re-set                 !
       ! 'bprod(i,pver+1)= bflxs(i)' here. Note that this setting does not     !
       ! influence numerical calculation at all - it is just for diagnostic    !
       ! output.                                                               !
       ! --------------------------------------------------------------------- !

       bprod(i,pver+1) = bflxs(i)
              
       gg = 0.5_r8*vk*z(i,pver)*bprod(i,pver+1)/(tkes(i)**(3._r8/2._r8))
       if( abs(alph5-gg*alph3) .le. 1.e-7_r8 ) then
         ! gh = -0.28_r8
           if( bprod(i,pver+1) .gt. 0._r8 ) then
               gh = -3.5334_r8
           else
               gh = ghmin
           endif
       else    
           gh = gg/(alph5-gg*alph3)
       end if 

     ! gh = min(max(gh,-0.28_r8),0.0233_r8)
       if( bprod(i,pver+1) .gt. 0._r8 ) then
           gh = min(max(gh,-3.5334_r8),0.0233_r8)
       else
           gh = min(max(gh,ghmin),0.0233_r8)
       endif

       gh_a(i,pver+1) = gh     
       sh_a(i,pver+1) = max(0._r8,alph5/(1._r8+alph3*gh))
       if( bprod(i,pver+1) .gt. 0._r8 ) then       
           sm_a(i,pver+1) = max(0._r8,(alph1+alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh))
       else
           sm_a(i,pver+1) = max(0._r8,(alph1+alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4exs*gh))
       endif
       sm_aw(i,pver+1) = sm_a(i,pver+1)/alph1
       ri_a(i,pver+1)  = -(sm_a(i,pver+1)/sh_a(i,pver+1))*(bprod(i,pver+1)/sprod(i,pver+1))

       do k = 1, pver
          if( ri(i,k) .lt. 0._r8 ) then
              trma = alph3*alph4*ri(i,k) + 2._r8*b1*(alph2-alph4*alph5*ri(i,k))
              trmb = (alph3+alph4)*ri(i,k) + 2._r8*b1*(-alph5*ri(i,k)+alph1)
              trmc = ri(i,k)
              det  = max(trmb*trmb-4._r8*trma*trmc,0._r8)
              gh   = (-trmb + sqrt(det))/(2._r8*trma)
              gh   = min(max(gh,-3.5334_r8),0.0233_r8)
              gh_a(i,k) = gh
              sh_a(i,k) = max(0._r8,alph5/(1._r8+alph3*gh))
              sm_a(i,k) = max(0._r8,(alph1+alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh))
              ri_a(i,k) = ri(i,k)
          else
              if( ri(i,k) .gt. ricrit ) then
                  gh_a(i,k) = ghmin
                  sh_a(i,k) = 0._r8
                  sm_a(i,k) = 0._r8
                  ri_a(i,k) = ri(i,k)
              else
                  trma = alph3*alph4exs*ri(i,k) + 2._r8*b1*(alph2-alph4exs*alph5*ri(i,k))
                  trmb = (alph3+alph4exs)*ri(i,k) + 2._r8*b1*(-alph5*ri(i,k)+alph1)
                  trmc = ri(i,k)
                  det  = max(trmb*trmb-4._r8*trma*trmc,0._r8)
                  gh   = (-trmb + sqrt(det))/(2._r8*trma)
                  gh   = min(max(gh,ghmin),0.0233_r8)
                  gh_a(i,k) = gh
                  sh_a(i,k) = max(0._r8,alph5/(1._r8+alph3*gh))
                  sm_a(i,k) = max(0._r8,(alph1+alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4exs*gh))
                  ri_a(i,k) = ri(i,k)
              endif
          endif

       end do

       do k = 1, pver + 1
          turbtype_f(i,k) = real(turbtype(i,k))
       end do

    end do   ! End of column index loop, i 

    return

    end subroutine caleddy

    !============================================================================== !
    !                                                                               !
    !============================================================================== !

    subroutine exacol( pcols, pver, ncol, ri, bflxs, minpblh, zi, ktop, kbase, ncvfin ) 

    ! ---------------------------------------------------------------------------- !
    ! Object : Find unstable CL regimes and determine the indices                  !
    !          kbase, ktop which delimit these unstable layers :                   !
    !          ri(kbase) > 0 and ri(ktop) > 0, but ri(k) < 0 for ktop < k < kbase. ! 
    ! Author : Chris  Bretherton 08/2000,                                          !
    !          Sungsu Park       08/2006, 11/2008                                  !
    !----------------------------------------------------------------------------- !

    implicit none

    ! --------------- !
    ! Input variables !
    ! --------------- !

    integer,  intent(in) :: pcols                  ! Number of atmospheric columns   
    integer,  intent(in) :: pver                   ! Number of atmospheric vertical layers   
    integer,  intent(in) :: ncol                   ! Number of atmospheric columns   

    real(r8), intent(in) :: ri(pcols,pver)         ! Moist gradient Richardson no.
    real(r8), intent(in) :: bflxs(pcols)           ! Buoyancy flux at surface
    real(r8), intent(in) :: minpblh(pcols)         ! Minimum PBL height based on surface stress
    real(r8), intent(in) :: zi(pcols,pver+1)       ! Interface heights

    ! ---------------- !
    ! Output variables !      
    ! ---------------- !

    integer, intent(out) :: kbase(pcols,ncvmax)    ! External interface index of CL base
    integer, intent(out) :: ktop(pcols,ncvmax)     ! External interface index of CL top
    integer, intent(out) :: ncvfin(pcols)          ! Total number of CLs

    ! --------------- !
    ! Local variables !
    ! --------------- !

    integer              :: i
    integer              :: k
    integer              :: ncv
    real(r8)             :: rimaxentr
    real(r8)             :: riex(pver+1)           ! Column Ri profile extended to surface

    ! ----------------------- !
    ! Main Computation Begins !
    ! ----------------------- !

    do i = 1, ncol
       ncvfin(i) = 0
       do ncv = 1, ncvmax
          ktop(i,ncv)  = 0
          kbase(i,ncv) = 0
       end do
    end do

    ! ------------------------------------------------------ !
    ! Find CL regimes starting from the surface going upward !
    ! ------------------------------------------------------ !
    
    rimaxentr = 0._r8   
    
    do i = 1, ncol

       riex(2:pver) = ri(i,2:pver)

       ! Below allows consistent treatment of surface and other interfaces.
       ! Simply, if surface buoyancy flux is positive, Ri of surface is set to be negative.

       riex(pver+1) = rimaxentr - bflxs(i) 

       ncv = 0
       k   = pver + 1 ! Work upward from surface interface

       do while ( k .gt. ntop_turb + 1 )

        ! Below means that if 'bflxs > 0' (do not contain '=' sign), surface
        ! interface is energetically interior surface. 
       
          if( riex(k) .lt. rimaxentr ) then 

              ! Identify a new CL

              ncv = ncv + 1

              ! First define 'kbase' as the first interface below the lower-most unstable interface
              ! Thus, Richardson number at 'kbase' is positive.

              kbase(i,ncv) = min(k+1,pver+1)

              ! Decrement k until top unstable level

              do while( riex(k) .lt. rimaxentr .and. k .gt. ntop_turb + 1 )
                 k = k - 1
              end do

              ! ktop is the first interface above upper-most unstable interface
              ! Thus, Richardson number at 'ktop' is positive. 

              ktop(i,ncv) = k
             
          else

              ! Search upward for a CL.

              k = k - 1

          end if

       end do ! End of CL regime finding for each atmospheric column

       ncvfin(i) = ncv    

    end do  ! End of atmospheric column do loop

    return 

    end subroutine exacol

    !============================================================================== !
    !                                                                               !
    !============================================================================== !
    
    subroutine zisocl( pcols  , pver  , long ,                                 & 
                       z      , zi    , n2   ,  s2      ,                      & 
                       bprod  , sprod , bflxs,  tkes    ,                      & 
                       ncvfin , kbase , ktop ,  belongcv,                      & 
                       ricl   , ghcl  , shcl ,  smcl    ,                      &
                       lbrk   , wbrk  , ebrk ,  extend  , extend_up, extend_dn )

    !------------------------------------------------------------------------ !
    ! Object : This 'zisocl' vertically extends original CLs identified from  !
    !          'exacol' using a merging test based on either 'wint' or 'l2n2' !
    !          and identify new CL regimes. Similar to the case of 'exacol',  !
    !          CL regime index increases with height.  After identifying new  !
    !          CL regimes ( kbase, ktop, ncvfin ),calculate CL internal mean  !
    !          energetics (lbrk : energetic thickness integral, wbrk, ebrk )  !
    !          and stability functions (ricl, ghcl, shcl, smcl) by including  !
    !          surface interfacial layer contribution when bflxs > 0.   Note  !
    !          that there are two options in the treatment of the energetics  !
    !          of surface interfacial layer (use_dw_surf= 'true' or 'false')  !
    ! Author : Sungsu Park 08/2006, 11/2008                                   !
    !------------------------------------------------------------------------ !

    implicit none

    ! --------------- !    
    ! Input variables !
    ! --------------- !

    integer,  intent(in)   :: long                    ! Longitude of the column
    integer,  intent(in)   :: pcols                   ! Number of atmospheric columns   
    integer,  intent(in)   :: pver                    ! Number of atmospheric vertical layers   
    real(r8), intent(in)   :: z(pcols, pver)          ! Layer mid-point height [ m ]
    real(r8), intent(in)   :: zi(pcols, pver+1)       ! Interface height [ m ]
    real(r8), intent(in)   :: n2(pcols, pver)         ! Buoyancy frequency at interfaces except surface [ s-2 ]
    real(r8), intent(in)   :: s2(pcols, pver)         ! Shear frequency at interfaces except surface [ s-2 ]
    real(r8), intent(in)   :: bprod(pcols,pver+1)     ! Buoyancy production [ m2/s3 ]. bprod(i,pver+1) = bflxs 
    real(r8), intent(in)   :: sprod(pcols,pver+1)     ! Shear production [ m2/s3 ]. sprod(i,pver+1) = usta**3/(vk*z(i,pver))
    real(r8), intent(in)   :: bflxs(pcols)            ! Surface buoyancy flux [ m2/s3 ]. bprod(i,pver+1) = bflxs 
    real(r8), intent(in)   :: tkes(pcols)             ! TKE at the surface [ s2/s2 ]

    ! ---------------------- !
    ! Input/output variables !
    ! ---------------------- !

    integer, intent(inout) :: kbase(pcols,ncvmax)     ! Base external interface index of CL
    integer, intent(inout) :: ktop(pcols,ncvmax)      ! Top external interface index of CL
    integer, intent(inout) :: ncvfin(pcols)           ! Total number of CLs

    ! ---------------- !
    ! Output variables !
    ! ---------------- !

    logical,  intent(out) :: belongcv(pcols,pver+1)   ! True if interface is in a CL ( either internal or external )
    real(r8), intent(out) :: ricl(pcols,ncvmax)       ! Mean Richardson number of internal CL
    real(r8), intent(out) :: ghcl(pcols,ncvmax)       ! Half of normalized buoyancy production of internal CL
    real(r8), intent(out) :: shcl(pcols,ncvmax)       ! Galperin instability function of heat-moisture of internal CL
    real(r8), intent(out) :: smcl(pcols,ncvmax)       ! Galperin instability function of momentum of internal CL
    real(r8), intent(out) :: lbrk(pcols,ncvmax)       ! Thickness of (energetically) internal CL ( lint, [m] )
    real(r8), intent(out) :: wbrk(pcols,ncvmax)       ! Mean normalized TKE of internal CL  [ m2/s2 ]
    real(r8), intent(out) :: ebrk(pcols,ncvmax)       ! Mean TKE of internal CL ( b1*wbrk, [m2/s2] )

    ! ------------------ !
    ! Internal variables !
    ! ------------------ !

    logical               :: extend                   ! True when CL is extended in zisocl
    logical               :: extend_up                ! True when CL is extended upward in zisocl
    logical               :: extend_dn                ! True when CL is extended downward in zisocl
    logical               :: bottom                   ! True when CL base is at surface ( kb = pver + 1 )

    integer               :: i                        ! Local index for the longitude
    integer               :: ncv                      ! CL Index increasing with height
    integer               :: incv
    integer               :: k
    integer               :: kb                       ! Local index for kbase
    integer               :: kt                       ! Local index for ktop
    integer               :: ncvinit                  ! Value of ncv at routine entrance 
    integer               :: cntu                     ! Number of merged CLs during upward   extension of individual CL
    integer               :: cntd                     ! Number of merged CLs during downward extension of individual CL
    integer               :: kbinc                    ! Index for incorporating underlying CL
    integer               :: ktinc                    ! Index for incorporating  overlying CL

    real(r8)              :: wint                     ! Normalized TKE of internal CL
    real(r8)              :: dwinc                    ! Normalized TKE of CL external interfaces
    real(r8)              :: dw_surf                  ! Normalized TKE of surface interfacial layer
    real(r8)              :: dzinc
    real(r8)              :: gh
    real(r8)              :: sh
    real(r8)              :: sm
    real(r8)              :: gh_surf                  ! Half of normalized buoyancy production in surface interfacial layer 
    real(r8)              :: sh_surf                  ! Galperin instability function in surface interfacial layer  
    real(r8)              :: sm_surf                  ! Galperin instability function in surface interfacial layer 
    real(r8)              :: l2n2                     ! Vertical integral of 'l^2N^2' over CL. Include thickness product
    real(r8)              :: l2s2                     ! Vertical integral of 'l^2S^2' over CL. Include thickness product
    real(r8)              :: dl2n2                    ! Vertical integration of 'l^2*N^2' of CL external interfaces
    real(r8)              :: dl2s2                    ! Vertical integration of 'l^2*S^2' of CL external interfaces
    real(r8)              :: dl2n2_surf               ! 'dl2n2' defined in the surface interfacial layer
    real(r8)              :: dl2s2_surf               ! 'dl2s2' defined in the surface interfacial layer  
    real(r8)              :: lint                     ! Thickness of (energetically) internal CL
    real(r8)              :: dlint                    ! Interfacial layer thickness of CL external interfaces
    real(r8)              :: dlint_surf               ! Surface interfacial layer thickness 
    real(r8)              :: lbulk                    ! Master Length Scale : Whole CL thickness from top to base external interface
    real(r8)              :: lz                       ! Turbulent length scale
    real(r8)              :: ricll                    ! Mean Richardson number of internal CL 
    real(r8)              :: trma
    real(r8)              :: trmb
    real(r8)              :: trmc
    real(r8)              :: det
    real(r8)              :: zbot                     ! Height of CL base
    real(r8)              :: l2rat                    ! Square of ratio of actual to initial CL (not used)
    real(r8)              :: gg                       ! Intermediate variable used for calculating stability functions of SBCL
    real(r8)              :: tunlramp                 ! Ramping tunl

    ! ----------------------- !
    ! Main Computation Begins !
    ! ----------------------- ! 

    i = long

    ! Initialize main output variables
    
    do k = 1, ncvmax
       ricl(i,k) = 0._r8
       ghcl(i,k) = 0._r8
       shcl(i,k) = 0._r8
       smcl(i,k) = 0._r8
       lbrk(i,k) = 0._r8
       wbrk(i,k) = 0._r8
       ebrk(i,k) = 0._r8
    end do
    extend    = .false.
    extend_up = .false.
    extend_dn = .false.

    ! ----------------------------------------------------------- !
    ! Loop over each CL to see if any of them need to be extended !
    ! ----------------------------------------------------------- !

    ncv = 1

    do while( ncv .le. ncvfin(i) )

       ncvinit = ncv
       cntu    = 0
       cntd    = 0
       kb      = kbase(i,ncv) 
       kt      = ktop(i,ncv)
       
       ! ---------------------------------------------------------------------------- !
       ! Calculation of CL interior energetics including surface before extension     !
       ! ---------------------------------------------------------------------------- !
       ! Note that the contribution of interior interfaces (not surface) to 'wint' is !
       ! accounted by using '-sh*l2n2 + sm*l2s2' while the contribution of surface is !
       ! accounted by using 'dwsurf = tkes/b1' when bflxs > 0. This approach is fully !
       ! reasonable. Another possible alternative,  which seems to be also consistent !
       ! is to calculate 'dl2n2_surf'  and  'dl2s2_surf' of surface interfacial layer !
       ! separately, and this contribution is explicitly added by initializing 'l2n2' !
       ! 'l2s2' not by zero, but by 'dl2n2_surf' and 'ds2n2_surf' below.  At the same !
       ! time, 'dwsurf' should be excluded in 'wint' calculation below. The only diff.!
       ! between two approaches is that in case of the latter approach, contributions !
       ! of surface interfacial layer to the CL mean stability function (ri,gh,sh,sm) !
       ! are explicitly included while the first approach is not. In this sense,  the !
       ! second approach seems to be more conceptually consistent,   but currently, I !
       ! (Sungsu) will keep the first default approach. There is a switch             !
       ! 'use_dw_surf' at the first part of eddy_diff.F90 chosing one of              !
       ! these two options.                                                           !
       ! ---------------------------------------------------------------------------- !
       
       ! ------------------------------------------------------ !   
       ! Step 0: Calculate surface interfacial layer energetics !
       ! ------------------------------------------------------ !

       lbulk      = zi(i,kt) - zi(i,kb)
       dlint_surf = 0._r8
       dl2n2_surf = 0._r8
       dl2s2_surf = 0._r8
       dw_surf    = 0._r8
       if( kb .eq. pver+1 ) then

           if( bflxs(i) .gt. 0._r8 ) then

               ! Calculate stability functions of surface interfacial layer
               ! from the given 'bprod(i,pver+1)' and 'sprod(i,pver+1)' using
               ! inverse approach. Since alph5>0 and alph3<0, denominator of
               ! gg is always positive if bprod(i,pver+1)>0.               

               gg    = 0.5_r8*vk*z(i,pver)*bprod(i,pver+1)/(tkes(i)**(3._r8/2._r8))
               gh    = gg/(alph5-gg*alph3)
             ! gh    = min(max(gh,-0.28_r8),0.0233_r8)
               gh    = min(max(gh,-3.5334_r8),0.0233_r8)
               sh    = alph5/(1._r8+alph3*gh)
               sm    = (alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh)
               ricll = min(-(sm/sh)*(bprod(i,pver+1)/sprod(i,pver+1)),ricrit)

               ! Calculate surface interfacial layer contribution to CL internal
               ! energetics. By construction, 'dw_surf = -dl2n2_surf + ds2n2_surf'
               ! is exactly satisfied, which corresponds to assuming turbulent
               ! length scale of surface interfacial layer = vk * z(i,pver). Note
               ! 'dl2n2_surf','dl2s2_surf','dw_surf' include thickness product.   

               dlint_surf = z(i,pver)
               dl2n2_surf = -vk*(z(i,pver)**2)*bprod(i,pver+1)/(sh*sqrt(tkes(i)))
               dl2s2_surf =  vk*(z(i,pver)**2)*sprod(i,pver+1)/(sm*sqrt(tkes(i)))
               dw_surf    = (tkes(i)/b1)*z(i,pver) 

           else

               ! Note that this case can happen when surface is an external 
               ! interface of CL.
               lbulk = zi(i,kt) - z(i,pver)

           end if

       end if
           
       ! ------------------------------------------------------ !   
       ! Step 1: Include surface interfacial layer contribution !
       ! ------------------------------------------------------ !
       
       lint = dlint_surf
       l2n2 = dl2n2_surf
       l2s2 = dl2s2_surf          
       wint = dw_surf
       if( use_dw_surf ) then
           l2n2 = 0._r8
           l2s2 = 0._r8
       else
           wint = 0._r8
       end if    
       
       ! --------------------------------------------------------------------------------- !
       ! Step 2. Include the contribution of 'pure internal interfaces' other than surface !
       ! --------------------------------------------------------------------------------- ! 
       
       if( kt .lt. kb - 1 ) then ! The case of non-SBCL.
                              
           do k = kb - 1, kt + 1, -1       
              if( choice_tunl .eq. 'rampcl' ) then
                ! Modification : I simply used the average tunlramp between the two limits.
                  tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
              elseif( choice_tunl .eq. 'rampsl' ) then
                  tunlramp = ctunl*tunl
                ! tunlramp = 0.765_r8
              else
                  tunlramp = tunl
              endif
              if( choice_leng .eq. 'origin' ) then
                  lz = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                ! lz = vk*zi(i,k) / (1._r8+vk*zi(i,k)/(tunlramp*lbulk))
              else
                  lz = min( vk*zi(i,k), tunlramp*lbulk )              
              endif
              dzinc = z(i,k-1) - z(i,k)
              l2n2  = l2n2 + lz*lz*n2(i,k)*dzinc
              l2s2  = l2s2 + lz*lz*s2(i,k)*dzinc
              lint  = lint + dzinc
           end do

           ! Calculate initial CL stability functions (gh,sh,sm) and net
           ! internal energy of CL including surface contribution if any. 

         ! Modification : It seems that below cannot be applied when ricrit > 0.19.
         !                May need future generalization.

           ricll = min(l2n2/max(l2s2,ntzero),ricrit) ! Mean Ri of internal CL
           trma  = alph3*alph4*ricll+2._r8*b1*(alph2-alph4*alph5*ricll)
           trmb  = ricll*(alph3+alph4)+2._r8*b1*(-alph5*ricll+alph1)
           trmc  = ricll
           det   = max(trmb*trmb-4._r8*trma*trmc,0._r8)
           gh    = (-trmb + sqrt(det))/2._r8/trma
         ! gh    = min(max(gh,-0.28_r8),0.0233_r8)
           gh    = min(max(gh,-3.5334_r8),0.0233_r8)
           sh    = alph5/(1._r8+alph3*gh)
           sm    = (alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh)
           wint  = wint - sh*l2n2 + sm*l2s2 

       else ! The case of SBCL
 
           ! If there is no pure internal interface, use only surface interfacial
           ! values. However, re-set surface interfacial values such  that it can
           ! be used in the merging tests (either based on 'wint' or 'l2n2')  and
           ! in such that surface interfacial energy is not double-counted.
           ! Note that regardless of the choise of 'use_dw_surf', below should be
           ! kept as it is below, for consistent merging test of extending SBCL. 
       
           lint = dlint_surf
           l2n2 = dl2n2_surf
           l2s2 = dl2s2_surf 
           wint = dw_surf

           ! Aug.29.2006 : Only for the purpose of merging test of extending SRCL
           ! based on 'l2n2', re-define 'l2n2' of surface interfacial layer using
           ! 'wint'. This part is designed for similar treatment of merging as in
           ! the original 'eddy_diff.F90' code,  where 'l2n2' of SBCL was defined
           ! as 'l2n2 = - wint / sh'. Note that below block is used only when (1)
           ! surface buoyancy production 'bprod(i,pver+1)' is NOT included in the
           ! calculation of surface TKE in the initialization of 'bprod(i,pver+1)'
           ! in the main subroutine ( even though bflxs > 0 ), and (2) to force 
           ! current scheme be similar to the previous scheme in the treatment of  
           ! extending-merging test of SBCL based on 'l2n2'. Otherwise below line
           ! must be commented out. Note at this stage, correct non-zero value of
           ! 'sh' has been already computed.      

           if( choice_tkes .eq. 'ebprod' ) then
               l2n2 = - wint / sh 
           endif
           
       endif
           
       ! Set consistent upper limits on 'l2n2' and 'l2s2'. Below limits are
       ! reasonable since l2n2 of CL interior interface is always negative.

       l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
       l2s2 =  min( l2s2, tkemax*lint/(b1*sm))
       
       ! Note that at this stage, ( gh, sh, sm )  are the values of surface
       ! interfacial layer if there is no pure internal interface, while if
       ! there is pure internal interface, ( gh, sh, sm ) are the values of
       ! pure CL interfaces or the values that include both the CL internal
       ! interfaces and surface interfaces, depending on the 'use_dw_surf'.       
       
       ! ----------------------------------------------------------------------- !
       ! Perform vertical extension-merging process                              !
       ! ----------------------------------------------------------------------- !
       ! During the merging process, we assumed ( lbulk, sh, sm ) of CL external !
       ! interfaces are the same as the ones of the original merging CL. This is !
       ! an inevitable approximation since we don't know  ( sh, sm ) of external !
       ! interfaces at this stage.     Note that current default merging test is !
       ! purely based on buoyancy production without including shear production, !
       ! since we used 'l2n2' instead of 'wint' as a merging parameter. However, !
       ! merging test based on 'wint' maybe conceptually more attractable.       !
       ! Downward CL merging process is identical to the upward merging process, !
       ! but when the base of extended CL reaches to the surface, surface inter  !
       ! facial layer contribution to the energetic of extended CL must be done  !
       ! carefully depending on the sign of surface buoyancy flux. The contribu  !
       ! tion of surface interfacial layer energetic is included to the internal !
       ! energetics of merging CL only when bflxs > 0.                           !
       ! ----------------------------------------------------------------------- !
       
       ! ---------------------------- !
       ! Step 1. Extend the CL upward !
       ! ---------------------------- !
       
       extend = .false.    ! This will become .true. if CL top or base is extended

       ! Calculate contribution of potentially incorporable CL top interface

       if( choice_tunl .eq. 'rampcl' ) then
           tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
       elseif( choice_tunl .eq. 'rampsl' ) then
           tunlramp = ctunl*tunl
         ! tunlramp = 0.765_r8
       else
           tunlramp = tunl
       endif
       if( choice_leng .eq. 'origin' ) then
           lz = ( (vk*zi(i,kt))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
         ! lz = vk*zi(i,kt) / (1._r8+vk*zi(i,kt)/(tunlramp*lbulk))
       else
           lz = min( vk*zi(i,kt), tunlramp*lbulk )              
       endif

       dzinc = z(i,kt-1)-z(i,kt)
       dl2n2 = lz*lz*n2(i,kt)*dzinc
       dl2s2 = lz*lz*s2(i,kt)*dzinc
       dwinc = -sh*dl2n2 + sm*dl2s2

       ! ------------ !
       ! Merging Test !
       ! ------------ !
 
     ! do while (  dwinc .gt. ( rinc*dzinc*wint/(lint+(1._r8-rinc)*dzinc)) )  ! Merging test based on wint
     ! do while ( -dl2n2 .gt. (-rinc*dzinc*l2n2/(lint+(1._r8-rinc)*dzinc)) )  ! Merging test based on l2n2 
#ifndef WRF_PORT
       do while ( -dl2n2 .gt. (-rinc*l2n2/(1._r8-rinc)) )                     ! Integral merging test
#else
       do while ( -dl2n2 .gt. (-rinc*l2n2/(1._r8-rinc)) .and. kt-1 .gt. ntop_turb )                     ! Integral merging test
#endif

          ! Add contribution of top external interface to interior energy.
          ! Note even when we chose 'use_dw_surf='true.', the contribution
          ! of surface interfacial layer to 'l2n2' and 'l2s2' are included
          ! here. However it is not double counting of surface interfacial
          ! energy : surface interfacial layer energy is counted in 'wint'
          ! formula and 'l2n2' is just used for performing merging test in
          ! this 'do while' loop.     

          lint = lint + dzinc
          l2n2 = l2n2 + dl2n2
          l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
          l2s2 = l2s2 + dl2s2
          wint = wint + dwinc

          ! Extend top external interface of CL upward after merging

          kt        = kt - 1
          extend    = .true.
          extend_up = .true.
          if( kt .eq. ntop_turb ) then
              write(iulog,*) 'zisocl: Error: Tried to extend CL to the model top'
#ifdef WRF_PORT
              call wrf_message(iulog)
#endif 
              stop
          end if

          ! If the top external interface of extending CL is the same as the 
          ! top interior interface of the overlying CL, overlying CL will be
          ! automatically merged. Then,reduce total number of CL regime by 1. 
          ! and increase 'cntu'(number of merged CLs during upward extension)
          ! by 1.
 
          ktinc = kbase(i,ncv+cntu+1) - 1  ! Lowest interior interface of overlying CL

          if( kt .eq. ktinc ) then

              do k = kbase(i,ncv+cntu+1) - 1, ktop(i,ncv+cntu+1) + 1, -1

                 if( choice_tunl .eq. 'rampcl' ) then
                     tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
                 elseif( choice_tunl .eq. 'rampsl' ) then
                     tunlramp = ctunl*tunl
                   ! tunlramp = 0.765_r8
                 else
                     tunlramp = tunl
                 endif
                 if( choice_leng .eq. 'origin' ) then
                     lz = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                   ! lz = vk*zi(i,k) / (1._r8+vk*zi(i,k)/(tunlramp*lbulk))
                 else
                     lz = min( vk*zi(i,k), tunlramp*lbulk )              
                 endif

                 dzinc = z(i,k-1)-z(i,k)
                 dl2n2 = lz*lz*n2(i,k)*dzinc
                 dl2s2 = lz*lz*s2(i,k)*dzinc
                 dwinc = -sh*dl2n2 + sm*dl2s2

                 lint = lint + dzinc
                 l2n2 = l2n2 + dl2n2
                 l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
                 l2s2 = l2s2 + dl2s2
                 wint = wint + dwinc

              end do 

              kt        = ktop(i,ncv+cntu+1) 
              ncvfin(i) = ncvfin(i) - 1
              cntu      = cntu + 1
        
          end if

          ! Again, calculate the contribution of potentially incorporatable CL
          ! top external interface of CL regime.

          if( choice_tunl .eq. 'rampcl' ) then
              tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
          elseif( choice_tunl .eq. 'rampsl' ) then
              tunlramp = ctunl*tunl
            ! tunlramp = 0.765_r8
          else
              tunlramp = tunl
          endif
          if( choice_leng .eq. 'origin' ) then
              lz = ( (vk*zi(i,kt))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
            ! lz = vk*zi(i,kt) / (1._r8+vk*zi(i,kt)/(tunlramp*lbulk))
          else
              lz = min( vk*zi(i,kt), tunlramp*lbulk )              
          endif

          dzinc = z(i,kt-1)-z(i,kt)
          dl2n2 = lz*lz*n2(i,kt)*dzinc
          dl2s2 = lz*lz*s2(i,kt)*dzinc
          dwinc = -sh*dl2n2 + sm*dl2s2

       end do   ! End of upward merging test 'do while' loop

       ! Update CL interface indices appropriately if any CL was merged.
       ! Note that below only updated the interface index of merged CL,
       ! not the original merging CL.  Updates of 'kbase' and 'ktop' of 
       ! the original merging CL  will be done after finishing downward
       ! extension also later.

       if( cntu .gt. 0 ) then
           do incv = 1, ncvfin(i) - ncv
              kbase(i,ncv+incv) = kbase(i,ncv+cntu+incv)
              ktop(i,ncv+incv)  = ktop(i,ncv+cntu+incv)
           end do
       end if

       ! ------------------------------ !
       ! Step 2. Extend the CL downward !
       ! ------------------------------ !
       
       if( kb .ne. pver + 1 ) then

           ! Calculate contribution of potentially incorporable CL base interface

           if( choice_tunl .eq. 'rampcl' ) then
               tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
           elseif( choice_tunl .eq. 'rampsl' ) then
               tunlramp = ctunl*tunl
             ! tunlramp = 0.765_r8
           else
               tunlramp = tunl
           endif
           if( choice_leng .eq. 'origin' ) then
               lz = ( (vk*zi(i,kb))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
             ! lz = vk*zi(i,kb) / (1._r8+vk*zi(i,kb)/(tunlramp*lbulk))
           else
               lz = min( vk*zi(i,kb), tunlramp*lbulk )              
           endif

           dzinc = z(i,kb-1)-z(i,kb)
           dl2n2 = lz*lz*n2(i,kb)*dzinc
           dl2s2 = lz*lz*s2(i,kb)*dzinc
           dwinc = -sh*dl2n2 + sm*dl2s2

           ! ------------ ! 
           ! Merging test !
           ! ------------ ! 

           ! In the below merging tests, I must keep '.and.(kb.ne.pver+1)',   
           ! since 'kb' is continuously updated within the 'do while' loop  
           ! whenever CL base is merged.

         ! do while( (  dwinc .gt. ( rinc*dzinc*wint/(lint+(1._r8-rinc)*dzinc)) ) &  ! Merging test based on wint
         ! do while( ( -dl2n2 .gt. (-rinc*dzinc*l2n2/(lint+(1._r8-rinc)*dzinc)) ) &  ! Merging test based on l2n2
         !             .and.(kb.ne.pver+1))
           do while( ( -dl2n2 .gt. (-rinc*l2n2/(1._r8-rinc)) ) &                     ! Integral merging test
                       .and.(kb.ne.pver+1))

              ! Add contributions from interfacial layer kb to CL interior 

              lint = lint + dzinc
              l2n2 = l2n2 + dl2n2
              l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
              l2s2 = l2s2 + dl2s2
              wint = wint + dwinc

              ! Extend the base external interface of CL downward after merging

              kb        =  kb + 1
              extend    = .true.
              extend_dn = .true.

              ! If the base external interface of extending CL is the same as the 
              ! base interior interface of the underlying CL, underlying CL  will
              ! be automatically merged. Then, reduce total number of CL by 1. 
              ! For a consistent treatment with 'upward' extension,  I should use
              ! 'kbinc = kbase(i,ncv-1) - 1' instead of 'ktop(i,ncv-1) + 1' below.
              ! However, it seems that these two methods produce the same results.
              ! Note also that in contrast to upward merging, the decrease of ncv
              ! should be performed here.
              ! Note that below formula correctly works even when upperlying CL 
              ! regime incorporates below SBCL.

              kbinc = 0
              if( ncv .gt. 1 ) kbinc = ktop(i,ncv-1) + 1
              if( kb .eq. kbinc ) then

                  do k =  ktop(i,ncv-1) + 1, kbase(i,ncv-1) - 1

                     if( choice_tunl .eq. 'rampcl' ) then
                         tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
                     elseif( choice_tunl .eq. 'rampsl' ) then
                         tunlramp = ctunl*tunl
                       ! tunlramp = 0.765_r8
                     else
                         tunlramp = tunl
                     endif
                     if( choice_leng .eq. 'origin' ) then
                         lz = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                       ! lz = vk*zi(i,k) / (1._r8+vk*zi(i,k)/(tunlramp*lbulk))
                     else
                         lz = min( vk*zi(i,k), tunlramp*lbulk )              
                     endif

                     dzinc = z(i,k-1)-z(i,k)
                     dl2n2 = lz*lz*n2(i,k)*dzinc
                     dl2s2 = lz*lz*s2(i,k)*dzinc
                     dwinc = -sh*dl2n2 + sm*dl2s2

                     lint = lint + dzinc
                     l2n2 = l2n2 + dl2n2
                     l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
                     l2s2 = l2s2 + dl2s2
                     wint = wint + dwinc

                  end do 

                  ! We are incorporating interior of CL ncv-1, so merge
                  ! this CL into the current CL.

                  kb        = kbase(i,ncv-1)
                  ncv       = ncv - 1
                  ncvfin(i) = ncvfin(i) -1
                  cntd      = cntd + 1

              end if

              ! Calculate the contribution of potentially incorporatable CL
              ! base external interface. Calculate separately when the base
              ! of extended CL is surface and non-surface.
             
              if( kb .eq. pver + 1 ) then 

                  if( bflxs(i) .gt. 0._r8 ) then 
                      ! Calculate stability functions of surface interfacial layer
                      gg = 0.5_r8*vk*z(i,pver)*bprod(i,pver+1)/(tkes(i)**(3._r8/2._r8))
                      gh_surf = gg/(alph5-gg*alph3)
                    ! gh_surf = min(max(gh_surf,-0.28_r8),0.0233_r8)
                      gh_surf = min(max(gh_surf,-3.5334_r8),0.0233_r8)
                      sh_surf = alph5/(1._r8+alph3*gh_surf)
                      sm_surf = (alph1 + alph2*gh_surf)/(1._r8+alph3*gh_surf)/(1._r8+alph4*gh_surf)
                      ! Calculate surface interfacial layer contribution. By construction,
                      ! it exactly becomes 'dw_surf = -dl2n2_surf + ds2n2_surf'  
                      dlint_surf = z(i,pver)
                      dl2n2_surf = -vk*(z(i,pver)**2._r8)*bprod(i,pver+1)/(sh_surf*sqrt(tkes(i)))
                      dl2s2_surf =  vk*(z(i,pver)**2._r8)*sprod(i,pver+1)/(sm_surf*sqrt(tkes(i)))
                      dw_surf = (tkes(i)/b1)*z(i,pver) 
                  else
                      dlint_surf = 0._r8
                      dl2n2_surf = 0._r8
                      dl2s2_surf = 0._r8
                      dw_surf = 0._r8
                  end if
                  ! If (kb.eq.pver+1), updating of CL internal energetics should be 
                  ! performed here inside of 'do while' loop, since 'do while' loop
                  ! contains the constraint of '.and.(kb.ne.pver+1)',so updating of
                  ! CL internal energetics cannot be performed within this do while
                  ! loop when kb.eq.pver+1. Even though I updated all 'l2n2','l2s2',
                  ! 'wint' below, only the updated 'wint' is used in the following
                  ! numerical calculation.                
                  lint = lint + dlint_surf
                  l2n2 = l2n2 + dl2n2_surf
                  l2n2 = -min(-l2n2, tkemax*lint/(b1*sh))
                  l2s2 = l2s2 + dl2s2_surf 
                  wint = wint + dw_surf                
                
              else

                  if( choice_tunl .eq. 'rampcl' ) then
                      tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
                  elseif( choice_tunl .eq. 'rampsl' ) then
                      tunlramp = ctunl*tunl
                    ! tunlramp = 0.765_r8
                  else
                      tunlramp = tunl
                  endif
                  if( choice_leng .eq. 'origin' ) then
                      lz = ( (vk*zi(i,kb))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                    ! lz = vk*zi(i,kb) / (1._r8+vk*zi(i,kb)/(tunlramp*lbulk))
                  else
                      lz = min( vk*zi(i,kb), tunlramp*lbulk )              
                  endif

                  dzinc = z(i,kb-1)-z(i,kb)
                  dl2n2 = lz*lz*n2(i,kb)*dzinc
                  dl2s2 = lz*lz*s2(i,kb)*dzinc
                  dwinc = -sh*dl2n2 + sm*dl2s2

              end if

          end do ! End of merging test 'do while' loop

          if( (kb.eq.pver+1) .and. (ncv.ne.1) ) then 
               write(iulog,*) 'Major mistake zisocl: the CL based at surface is not indexed 1'
#ifdef WRF_PORT
               call wrf_message(iulog)
#endif 
               stop
          end if

       end if   ! Done with bottom extension of CL 

       ! Update CL interface indices appropriately if any CL was merged.
       ! Note that below only updated the interface index of merged CL,
       ! not the original merging CL.  Updates of 'kbase' and 'ktop' of 
       ! the original merging CL  will be done later below. I should 
       ! check in detail if below index updating is correct or not.   

       if( cntd .gt. 0 ) then
           do incv = 1, ncvfin(i) - ncv
              kbase(i,ncv+incv) = kbase(i,ncvinit+incv)
              ktop(i,ncv+incv)  = ktop(i,ncvinit+incv)
           end do
       end if

       ! Sanity check for positive wint.

       if( wint .lt. 0.01_r8 ) then
           wint = 0.01_r8
       end if

       ! -------------------------------------------------------------------------- !
       ! Finally update CL mean internal energetics including surface contribution  !
       ! after finishing all the CL extension-merging process.  As mentioned above, !
       ! there are two possible ways in the treatment of surface interfacial layer, !
       ! either through 'dw_surf' or 'dl2n2_surf and dl2s2_surf' by setting logical !
       ! variable 'use_dw_surf' =.true. or .false.    In any cases, we should avoid !
       ! double counting of surface interfacial layer and one single consistent way !
       ! should be used throughout the program.                                     !
       ! -------------------------------------------------------------------------- !

       if( extend ) then

           ktop(i,ncv)  = kt
           kbase(i,ncv) = kb

           ! ------------------------------------------------------ !   
           ! Step 1: Include surface interfacial layer contribution !
           ! ------------------------------------------------------ !        
          
           lbulk      = zi(i,kt) - zi(i,kb)
           dlint_surf = 0._r8
           dl2n2_surf = 0._r8
           dl2s2_surf = 0._r8
           dw_surf    = 0._r8
           if( kb .eq. pver + 1 ) then
               if( bflxs(i) .gt. 0._r8 ) then
                   ! Calculate stability functions of surface interfacial layer
                   gg = 0.5_r8*vk*z(i,pver)*bprod(i,pver+1)/(tkes(i)**(3._r8/2._r8))
                   gh = gg/(alph5-gg*alph3)
                 ! gh = min(max(gh,-0.28_r8),0.0233_r8)
                   gh = min(max(gh,-3.5334_r8),0.0233_r8)
                   sh = alph5/(1._r8+alph3*gh)
                   sm = (alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh)
                   ! Calculate surface interfacial layer contribution. By construction,
                   ! it exactly becomes 'dw_surf = -dl2n2_surf + ds2n2_surf'  
                   dlint_surf = z(i,pver)
                   dl2n2_surf = -vk*(z(i,pver)**2._r8)*bprod(i,pver+1)/(sh*sqrt(tkes(i)))
                   dl2s2_surf =  vk*(z(i,pver)**2._r8)*sprod(i,pver+1)/(sm*sqrt(tkes(i)))
                   dw_surf    = (tkes(i)/b1)*z(i,pver) 
               else
                   lbulk = zi(i,kt) - z(i,pver)
               end if
           end if
           lint = dlint_surf
           l2n2 = dl2n2_surf
           l2s2 = dl2s2_surf
           wint = dw_surf
           if( use_dw_surf ) then
               l2n2 = 0._r8
               l2s2 = 0._r8
           else
               wint = 0._r8
           end if   
       
           ! -------------------------------------------------------------- !
           ! Step 2. Include the contribution of 'pure internal interfaces' !
           ! -------------------------------------------------------------- ! 
          
           do k = kt + 1, kb - 1
              if( choice_tunl .eq. 'rampcl' ) then
                  tunlramp = 0.5_r8*(1._r8+ctunl)*tunl
              elseif( choice_tunl .eq. 'rampsl' ) then
                  tunlramp = ctunl*tunl
                ! tunlramp = 0.765_r8
              else
                  tunlramp = tunl
              endif
              if( choice_leng .eq. 'origin' ) then
                  lz = ( (vk*zi(i,k))**(-cleng) + (tunlramp*lbulk)**(-cleng) )**(-1._r8/cleng)
                ! lz = vk*zi(i,k) / (1._r8+vk*zi(i,k)/(tunlramp*lbulk))
              else
                  lz = min( vk*zi(i,k), tunlramp*lbulk )              
              endif
              dzinc = z(i,k-1) - z(i,k)
              lint = lint + dzinc
              l2n2 = l2n2 + lz*lz*n2(i,k)*dzinc
              l2s2 = l2s2 + lz*lz*s2(i,k)*dzinc
           end do

           ricll = min(l2n2/max(l2s2,ntzero),ricrit)
           trma = alph3*alph4*ricll+2._r8*b1*(alph2-alph4*alph5*ricll)
           trmb = ricll*(alph3+alph4)+2._r8*b1*(-alph5*ricll+alph1)
           trmc = ricll
           det = max(trmb*trmb-4._r8*trma*trmc,0._r8)
           gh = (-trmb + sqrt(det))/2._r8/trma
         ! gh = min(max(gh,-0.28_r8),0.0233_r8)
           gh = min(max(gh,-3.5334_r8),0.0233_r8)
           sh = alph5 / (1._r8+alph3*gh)
           sm = (alph1 + alph2*gh)/(1._r8+alph3*gh)/(1._r8+alph4*gh)
           ! Even though the 'wint' after finishing merging was positive, it is 
           ! possible that re-calculated 'wint' here is negative.  In this case,
           ! correct 'wint' to be a small positive number
           wint = max( wint - sh*l2n2 + sm*l2s2, 0.01_r8 )

       end if

       ! ---------------------------------------------------------------------- !
       ! Calculate final output variables of each CL (either has merged or not) !
       ! ---------------------------------------------------------------------- !

       lbrk(i,ncv) = lint
       wbrk(i,ncv) = wint/lint
       ebrk(i,ncv) = b1*wbrk(i,ncv)
       ebrk(i,ncv) = min(ebrk(i,ncv),tkemax)
       ricl(i,ncv) = ricll 
       ghcl(i,ncv) = gh 
       shcl(i,ncv) = sh
       smcl(i,ncv) = sm

       ! Increment counter for next CL. I should check if the increament of 'ncv'
       ! below is reasonable or not, since whenever CL is merged during downward
       ! extension process, 'ncv' is lowered down continuously within 'do' loop.
       ! But it seems that below 'ncv = ncv + 1' is perfectly correct.

       ncv = ncv + 1

    end do                   ! End of loop over each CL regime, ncv.

    ! ---------------------------------------------------------- !
    ! Re-initialize external interface indices which are not CLs !
    ! ---------------------------------------------------------- !

    do ncv = ncvfin(i) + 1, ncvmax
       ktop(i,ncv)  = 0
       kbase(i,ncv) = 0
    end do

    ! ------------------------------------------------ !
    ! Update CL interface identifiers, 'belongcv'      !
    ! CL external interfaces are also identified as CL !
    ! ------------------------------------------------ !

    do k = 1, pver + 1
       belongcv(i,k) = .false.
    end do

    do ncv = 1, ncvfin(i)
       do k = ktop(i,ncv), kbase(i,ncv)
          belongcv(i,k) = .true.
       end do
    end do

    return

    end subroutine zisocl

    real(r8) function compute_cubic(a,b,c)
    ! ------------------------------------------------------------------------- !
    ! Solve canonical cubic : x^3 + a*x^2 + b*x + c = 0,  x = sqrt(e)/sqrt(<e>) !
    ! Set x = max(xmin,x) at the end                                            ! 
    ! ------------------------------------------------------------------------- !
    implicit none
    real(r8), intent(in)     :: a, b, c
    real(r8)  qq, rr, dd, theta, aa, bb, x1, x2, x3
    real(r8), parameter      :: xmin = 1.e-2_r8
    
    qq = (a**2-3._r8*b)/9._r8 
    rr = (2._r8*a**3 - 9._r8*a*b + 27._r8*c)/54._r8
    
    dd = rr**2 - qq**3
    if( dd .le. 0._r8 ) then
        theta = acos(rr/qq**(3._r8/2._r8))
        x1 = -2._r8*sqrt(qq)*cos(theta/3._r8) - a/3._r8
        x2 = -2._r8*sqrt(qq)*cos((theta+2._r8*3.141592)/3._r8) - a/3._r8
        x3 = -2._r8*sqrt(qq)*cos((theta-2._r8*3.141592)/3._r8) - a/3._r8
        compute_cubic = max(max(max(x1,x2),x3),xmin)        
        return
    else
        if( rr .ge. 0._r8 ) then
            aa = -(sqrt(rr**2-qq**3)+rr)**(1._r8/3._r8)
        else
            aa =  (sqrt(rr**2-qq**3)-rr)**(1._r8/3._r8)
        endif
        if( aa .eq. 0._r8 ) then
            bb = 0._r8
        else
            bb = qq/aa
        endif
        compute_cubic = max((aa+bb)-a/3._r8,xmin) 
        return
    endif

    return
    end function compute_cubic

END MODULE eddy_diff
