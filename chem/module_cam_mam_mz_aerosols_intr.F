! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
#define WRF_PORT
#define MODAL_AERO
module mz_aerosols_intr

  use shr_kind_mod,only: r8 => shr_kind_r8
#ifndef WRF_PORT
  use spmd_utils,  only: masterproc
  use pmgrid,      only: plev, plevp
  use ppgrid,      only: pcols, pver
  use constituents,only: pcnst, cnst_name, cnst_get_ind
  use phys_control,only: phys_getopts
  use abortutils,  only: endrun
#else
  use module_cam_support, only: masterproc, pcnst =>pcnst_runtime, pcols, pver, endrun
  use constituents,only: cnst_name, cnst_get_ind 
#endif
#ifdef MODAL_AERO
    use modal_aero_data, only: ntot_amode
#endif
#ifndef WRF_PORT
  use cam_logfile, only: iulog
#else
  use module_cam_support, only: iulog
#endif

  implicit none

  private          ! Make default type private to the module

  save

  !
  ! Public interfaces
  !

  public :: mz_aero_initialize                           
  public :: mz_aero_wet_intr                             ! interface to wet deposition
#ifndef WRF_PORT
  public :: mz_aero_setopts
  public :: mz_aero_defaultopts
#endif
  public :: aer_wetdep_list
  public :: do_cam_sulfchem
#ifdef MODAL_AERO
#ifndef WRF_PORT
  public :: mz_aero_dry_intr                             ! interface to dry deposition
  public :: aer_drydep_list
#endif
  public :: modal_aero_bcscavcoef_init  ! initialize impaction scavenging table
  public :: modal_aero_bcscavcoef_get   ! get impaction scavenging coefficients
#endif
#ifndef WRF_PORT
  public :: mz_prescribed_dust              ! return true if MOZART code provides prescribed dust
#endif

  integer :: num_mz_aerosols
  integer, allocatable :: mz_aerosol_ids(:) ! (/ id_cb2, id_oc2, id_so4, id_sa1, id_sa2, id_sa3, id_sa4 /)
#ifndef WRF_PORT
  character(len=16), dimension(pcnst) :: aer_wetdep_list = ' '
#else
  character(len=16), allocatable, dimension(:) :: aer_wetdep_list
#endif
  logical :: use_cam_sulfchem = .false.
  logical :: do_cam_sulfchem
  logical :: inv_o3, inv_oh, inv_no3, inv_ho2
  integer, pointer :: id_so2, id_so4, id_dms, id_o3, id_h2o2, id_oh, id_no3, id_ho2
  integer, target  :: spc_ids(8)

#ifdef MODAL_AERO
#ifndef WRF_PORT
  character(len=16), dimension(pcnst) :: aer_drydep_list = ''
#else
  character(len=16), allocatable,dimension(:) :: aer_drydep_lis
#endif
! variables for table lookup of aerosol impaction/interception scavenging rates
 integer, parameter :: nimptblgrow_mind=-7, nimptblgrow_maxd=12
 real(r8) dlndg_nimptblgrow
 real(r8) scavimptblnum(nimptblgrow_mind:nimptblgrow_maxd, ntot_amode)
 real(r8) scavimptblvol(nimptblgrow_mind:nimptblgrow_maxd, ntot_amode)
#endif

contains
#ifndef WRF_PORT
  !===============================================================================
#ifdef MODAL_AERO
  subroutine mz_aero_defaultopts( aer_wetdep_list_out, aer_drydep_list_out, use_cam_sulfchem_out )
#else
  subroutine mz_aero_defaultopts( aer_wetdep_list_out, use_cam_sulfchem_out )
#endif
    implicit none
    character(len=*), intent(out), optional :: aer_wetdep_list_out(:)
#ifdef MODAL_AERO
    character(len=*), intent(out), optional :: aer_drydep_list_out(:)
#endif
    logical,          intent(out), optional :: use_cam_sulfchem_out

#ifdef MODAL_AERO
    if ( present(aer_drydep_list_out) ) then
       aer_drydep_list_out(:) = aer_drydep_list(:)
    endif
#endif
    if ( present(aer_wetdep_list_out) ) then
       aer_wetdep_list_out(:) = aer_wetdep_list(:)
    endif
    if ( present(use_cam_sulfchem_out) ) then
       use_cam_sulfchem_out = use_cam_sulfchem
    endif

  end subroutine mz_aero_defaultopts

  !===============================================================================
#ifdef MODAL_AERO
  subroutine mz_aero_setopts( aer_wetdep_list_in, aer_drydep_list_in, use_cam_sulfchem_in )
#else
  subroutine mz_aero_setopts( aer_wetdep_list_in, use_cam_sulfchem_in )
#endif
    implicit none
    character(len=*), intent(in), optional :: aer_wetdep_list_in(:)
#ifdef MODAL_AERO
    character(len=*), intent(in), optional :: aer_drydep_list_in(:)
#endif
    logical,          intent(in), optional :: use_cam_sulfchem_in

#ifdef MODAL_AERO
    if ( present(aer_drydep_list_in) ) then
       aer_drydep_list(:) = aer_drydep_list_in(:)
    endif
#endif
    if ( present(aer_wetdep_list_in) ) then
       aer_wetdep_list(:) = aer_wetdep_list_in(:)
    endif
    if ( present(use_cam_sulfchem_in) ) then
       use_cam_sulfchem = use_cam_sulfchem_in
    endif
    
  end subroutine mz_aero_setopts
#endif
  !===============================================================================
  subroutine mz_aero_initialize( )
#ifndef WRF_PORT
    use cam_history,      only : addfld, add_default, phys_decomp
    use sulchem,          only : inisulchem
    use mo_chem_utls,     only : get_inv_ndx
    use dust_intr,        only : dst_wet_dep=>dust_has_wet_dep
    use progseasalts_intr,only : sst_wet_dep=>progseasalts_has_wet_dep
    use dust_intr,        only : dust_names
    use progseasalts_intr,only : progseasalts_names
    use gas_wetdep_opts,  only : gas_wetdep_list, gas_wetdep_cnt
#else
    use module_cam_support, only: addfld, add_default, phys_decomp
#endif

    implicit none

    integer :: m                                
    integer :: mm                               
    integer :: astat, id

    logical :: history_aerosol      ! Output the MAM aerosol tendencies

    !-----------------------------------------------------------------------
#ifndef WRF_PORT
    call phys_getopts( history_aerosol_out        = history_aerosol   )

    dst_wet_dep(:) = .false.
    sst_wet_dep(:) = .false.
#else
    history_aerosol = .FALSE.
#endif

    num_mz_aerosols = 0
#ifdef WRF_PORT 
    !*NOTE* -only when invoked inside WRF
    !Set num_mz_aerosols to some value greater than 0 so that the code inside 'mz_aero_wet_intr'
    !subroutine executes
    num_mz_aerosols = pcnst
#endif
#ifndef WRF_PORT
    count_species: do m = 1,pcnst

       if ( len_trim(aer_wetdep_list(m)) == 0 ) exit count_species

       do mm=1,gas_wetdep_cnt
          if ( trim(gas_wetdep_list(mm)) == trim(aer_wetdep_list(m)) ) then
             call endrun('mz_aero_initialize: '//trim(aer_wetdep_list(m))//' is in both het_lst and aer_wetdep_list')
          endif
       enddo

       call cnst_get_ind ( aer_wetdep_list(m), id, abort=.false. )
       if ( id < 1 ) then 
          write(iulog,*) 'mz_aero_initialize: '//trim(aer_wetdep_list(m))//' does not exit in simulation'
#ifdef WRF_PORT
          call wrf_message(iulog)
#endif
          call endrun('mz_aero_initialize: invalid washout species')
       else
          where( aer_wetdep_list(m) == dust_names )
             dst_wet_dep = .true.
          endwhere
          where( aer_wetdep_list(m) == progseasalts_names )
             sst_wet_dep = .true.
          endwhere

          if (masterproc) then
             write(iulog,*) 'mz_aero_initialize: '//aer_wetdep_list(m)//' will have wet removal'
#ifdef WRF_PORT
             call wrf_message(iulog)
#endif
          endif
          call addfld (trim(aer_wetdep_list(m))//'SFWET','kg/m2/s ',1,  'A','Wet deposition flux at surface',phys_decomp)
          call addfld (trim(aer_wetdep_list(m))//'SFSIC','kg/m2/s ', &
                1,  'A','Wet deposition flux (incloud, convective) at surface',phys_decomp)
          call addfld (trim(aer_wetdep_list(m))//'SFSIS','kg/m2/s ', &
                1,  'A','Wet deposition flux (incloud, stratiform) at surface',phys_decomp)
          call addfld (trim(aer_wetdep_list(m))//'SFSBC','kg/m2/s ', &
                1,  'A','Wet deposition flux (belowcloud, convective) at surface',phys_decomp)
          call addfld (trim(aer_wetdep_list(m))//'SFSBS','kg/m2/s ', &
                1,  'A','Wet deposition flux (belowcloud, stratiform) at surface',phys_decomp)
          call addfld (trim(aer_wetdep_list(m))//'WET','kg/kg/s ',pver, 'A','wet deposition tendency',phys_decomp)
          call addfld (trim(aer_wetdep_list(m))//'SIC','kg/kg/s ',pver, 'A', &
               trim(aer_wetdep_list(m))//' ic wet deposition',phys_decomp)
          call addfld (trim(aer_wetdep_list(m))//'SIS','kg/kg/s ',pver, 'A', &
               trim(aer_wetdep_list(m))//' is wet deposition',phys_decomp)
          call addfld (trim(aer_wetdep_list(m))//'SBC','kg/kg/s ',pver, 'A', &
               trim(aer_wetdep_list(m))//' bc wet deposition',phys_decomp)
          call addfld (trim(aer_wetdep_list(m))//'SBS','kg/kg/s ',pver, 'A', &
               trim(aer_wetdep_list(m))//' bs wet deposition',phys_decomp)
          if ( history_aerosol ) then          
             call add_default (trim(aer_wetdep_list(m))//'SFWET', 1, ' ')
             call add_default (trim(aer_wetdep_list(m))//'SFSIC', 1, ' ')
             call add_default (trim(aer_wetdep_list(m))//'SFSIS', 1, ' ')
             call add_default (trim(aer_wetdep_list(m))//'SFSBC', 1, ' ')
             call add_default (trim(aer_wetdep_list(m))//'SFSBS', 1, ' ')
          endif
          num_mz_aerosols = num_mz_aerosols + 1
       endif

    enddo count_species

#ifdef MODAL_AERO
    count_dry_species: do m = 1,pcnst

       if ( len_trim(aer_drydep_list(m)) == 0 ) exit count_dry_species

       call cnst_get_ind ( aer_drydep_list(m), id, abort=.false. )
       if ( id < 1 ) then
          write(*,*) 'mz_aero_initialize: '//trim(aer_drydep_list(m))//' does not exist in simulation'
          call endrun('mz_aero_initialize: invalid drydep species')
       else
          if (masterproc) then
             write(*,*) 'mz_aero_initialize: '//aer_drydep_list(m)//' will have dry deposition'
          endif
          call addfld (trim(aer_drydep_list(m))//'DDF','kg/m2/s ',   1, 'A', &
                       trim(aer_drydep_list(m))//' dry deposition flux at bottom (grav + turb)',phys_decomp)
          call addfld (trim(aer_drydep_list(m))//'TBF','kg/m2/s ',   1, 'A', &
                       trim(aer_drydep_list(m))//' turbulent dry deposition flux',phys_decomp)
          call addfld (trim(aer_drydep_list(m))//'GVF','kg/m2/s ',   1, 'A', &
                       trim(aer_drydep_list(m))//' gravitational dry deposition flux',phys_decomp)
          call addfld (trim(aer_drydep_list(m))//'DTQ','kg/kg/s ',pver, 'A', &
                       trim(aer_drydep_list(m))//' dry deposition',phys_decomp)
          call addfld (trim(aer_drydep_list(m))//'DDV','m/s     ',pver, 'A', &
                       trim(aer_drydep_list(m))//' deposition velocity',phys_decomp)
          if ( history_aerosol ) then 
             call add_default (trim(aer_drydep_list(m))//'DDF', 1, ' ')
             call add_default (trim(aer_drydep_list(m))//'TBF', 1, ' ')
             call add_default (trim(aer_drydep_list(m))//'GVF', 1, ' ')
          endif
       endif

    enddo count_dry_species
#endif

    allocate( mz_aerosol_ids(num_mz_aerosols) )

    do m = 1, num_mz_aerosols
       call cnst_get_ind ( aer_wetdep_list(m), mz_aerosol_ids(m), abort=.false. )
    enddo

    id_so2  => spc_ids(1)
    id_so4  => spc_ids(2)
    id_o3   => spc_ids(3)
    id_h2o2 => spc_ids(4)
    id_oh   => spc_ids(5)
    id_no3  => spc_ids(6)
    id_ho2  => spc_ids(7)
    id_dms  => spc_ids(8)

    inv_o3   = get_inv_ndx('O3') > 0
    inv_oh   = get_inv_ndx('OH') > 0
    inv_no3  = get_inv_ndx('NO3') > 0
    inv_ho2  = get_inv_ndx('HO2') > 0
    call cnst_get_ind ( 'SO2',  id_so2,  abort=.false. )
    call cnst_get_ind ( 'SO4',  id_so4,  abort=.false. )
    call cnst_get_ind ( 'H2O2', id_h2o2, abort=.false. )
    call cnst_get_ind ( 'DMS',  id_dms,  abort=.false. )

    if (inv_o3) then
       id_o3 = get_inv_ndx('O3')
    else
       call cnst_get_ind ( 'O3',   id_o3,   abort=.false. )
    endif
    if (inv_oh) then
       id_oh = get_inv_ndx('OH')
    else
       call cnst_get_ind ( 'OH',   id_oh,   abort=.false. )
    endif
    if (inv_no3) then
       id_no3 = get_inv_ndx('NO3')
    else
       call cnst_get_ind ( 'NO3',  id_no3,  abort=.false. )
    endif
    if (inv_ho2) then
       id_ho2 = get_inv_ndx('HO2')
    else
       call cnst_get_ind ( 'HO2',  id_ho2,  abort=.false. )
    endif

    do_cam_sulfchem = use_cam_sulfchem .and. all(spc_ids > 0)
    if ( do_cam_sulfchem ) then
       call inisulchem()
    endif

    if (masterproc) then
       write(iulog,*) 'mz_aero_initialize: do_cam_sulfchem = ',do_cam_sulfchem
#ifdef WRF_PORT
       call wrf_message(iulog)
#endif
    endif
#endif
  end subroutine mz_aero_initialize

  !===============================================================================
  subroutine mz_aero_wet_intr ( lchnk_in, ncol_in, state_q, state_pdel,          &
       state_pmid, state_t, ptend_name, ptend_lq, ptend_q, nstep, dt, cme, prain,    &
       evapr, cldv, cldvcu, cldvst, cldc, cldn, fracis, calday, cmfdqr, evapc, conicw, rainmr  &
#if ( defined MODAL_AERO )
     , rate1ord_cw2pr_st                                                & ! rce 2010/05/01
     , dgncur_awet,  qqcw, qaerwat                                       &
#endif
     , cam_out , dlf                                                         )
    !----------------------------------------------------------------------- 
    !-----------------------------------------------------------------------
#ifndef WRF_PORT
    use cam_history,   only: outfld
    use physics_types, only: physics_state, physics_ptend
    use camsrfexch_types, only: cam_out_t     
#else
    use module_cam_support, only: outfld
#endif
    use wetdep,        only: wetdepa_v1, wetdepa_v2
#ifndef WRF_PORT
    use sulchem,       only: sulf_chemdr, cldychmini, dicor
#endif
    use physconst,     only: gravit
    use constituents,  only: cnst_mw
    use physconst,     only: amass => mwdry         ! molecular weight dry air ~ kg/kmole
    use physconst,     only: boltz                  ! J/K/molecule
#ifndef WRF_PORT
    use dust_intr,        only: dust_names
    use progseasalts_intr,only: progseasalts_names
    use tracer_cnst,   only: get_cnst_data
    use chem_mods,     only: fix_mass
#endif
#ifdef MODAL_AERO
    use modal_aero_data
#ifndef WRF_PORT
    use modal_aero_deposition, only: set_srf_wetdep
#endif
#endif

    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments:
    !
    real(r8),            intent(in)  :: dt             ! time step
#ifndef WRF_PORT
    type(physics_state), intent(in ) :: state          ! Physics state variables
#else
    integer,  intent(in) :: lchnk_in,ncol_in
    real(r8), intent(in) :: state_q(pcols,pver,pcnst) 
    real(r8), intent(in) :: state_pdel(pcols,pver)
    real(r8), intent(in) :: state_t(pcols,pver)
    real(r8), intent(in) :: state_pmid(pcols,pver) 
#endif
    integer,  intent(in) :: nstep
    real(r8), intent(in) :: cme(pcols,pver)            ! local condensation of cloud water
    real(r8), intent(in) :: prain(pcols,pver)          ! production of rain
    real(r8), intent(in) :: evapr(pcols,pver)          ! evaporation of rain
    real(r8), intent(in) :: cldn(pcols,pver)           ! cloud fraction
    real(r8), intent(in) :: cldc(pcols,pver)           ! convective cloud fraction
    real(r8), intent(in) :: cldv(pcols,pver)           ! cloudy volume undergoing scavenging
    real(r8), intent(in) :: cldvcu(pcols,pver)         ! Convective precipitation area at the top interface of current layer
    real(r8), intent(in) :: cldvst(pcols,pver)         ! Stratiform precipitation area at the top interface of current layer
    real(r8), intent(in) :: conicw(pcols, pver)
    real(r8), intent(in) :: cmfdqr(pcols, pver)
    real(r8), intent(in) :: evapc(pcols, pver)         ! Evaporation rate of convective precipitation  
    real(r8), intent(in) :: rainmr(pcols, pver)         ! rain mixing ratio
    real(r8), intent(in) :: dlf(pcols,pver)            ! Detrainment of convective condensate
#ifndef WRF_PORT
    type(physics_ptend), intent(inout) :: ptend        ! indivdual parameterization tendencies
#else
    character(*), intent(inout) :: ptend_name 
    logical,      intent(inout) :: ptend_lq(pcnst)
    real(r8),     intent(inout) :: ptend_q(pcols,pver,pcnst) 
    real(r8), target, intent(inout) :: qqcw(pcols,pver,pcnst) ! cloudborne tracer MR array 
#endif
    real(r8),            intent(inout) :: fracis(pcols,pver,pcnst) ! fraction of transported species that are insoluble
    real(r8), intent(in) :: calday        ! current calendar day
#if ( defined MODAL_AERO )
    real(r8), intent(in) :: rate1ord_cw2pr_st(pcols,pver) ! 1st order rate for strat cw to precip (1/s) ! rce 2010/05/01
    real(r8), intent(in) :: dgncur_awet(pcols,pver,ntot_amode) ! wet/ambient geom. mean diameter (m)
                                                               ! for number distribution
    real(r8), intent(inout) :: qaerwat(pcols,pver,ntot_amode)
#endif
#ifndef WRF_PORT
    type(cam_out_t), intent(inout) :: cam_out
#else
    !Balwinder.Singh@pnnl.gov: Temporarily defined as 'real' as it is not currently being used in any computations
    real(r8), intent(in) :: cam_out
#endif

    !
    ! Local variables
    !
    integer  :: m                                  ! tracer index
    integer  :: ixcldice, ixcldliq
    integer  :: lchnk                              ! chunk identifier
    integer  :: ncol                               ! number of atmospheric columns
    real(r8) :: obuf(1)
    real(r8) :: iscavt(pcols, pver)
    real(r8) :: totcond(pcols, pver) ! sum of cldice and cldliq
    integer  :: yr, mon, day, ncsec
    integer  :: ncdate
    integer  :: mm
    integer  :: nphob
    real(r8), dimension(pcols,pver) :: h2o23d,o3,oh,no3,ho2,so2,so4,dms,h2o2,ekso2,ekh2o2

    real(r8), dimension(pcols,pver) ::&
         ph,          &! ph of drops
         hion          ! Hydrogen ion concentration in drops

    integer ::&
         indcp(pcols,pver),    &! Indices of cloudy grid points
         ncldypts(pver)         ! Number of cloudy grid points

    real(r8) :: icwmr2(pcols,pver) ! in cloud water mixing ratio for hack scheme
    real(r8) :: icscavt(pcols, pver)
    real(r8) :: isscavt(pcols, pver)
    real(r8) :: bcscavt(pcols, pver)
    real(r8) :: bsscavt(pcols, pver)
    real(r8) :: sol_factb, sol_facti
    real(r8) :: sol_factic(pcols,pver)
    real(r8) :: sol_factbi, sol_factii, sol_factiic
    real(r8) :: xhnm(pcols, pver)
    real(r8) :: sflx(pcols)            ! deposition flux
    real(r8) :: dicorfac(pcols)        ! factors to increase HO2 diurnal averages
    integer :: i,k
    real(r8) :: scavcoef(pcols,pver) ! Dana and Hales coefficient (/mm) (0.1)
#ifdef MODAL_AERO
    integer :: jnv                     ! index for scavcoefnv 3rd dimension
    integer :: lphase                  ! index for interstitial / cloudborne aerosol
    integer :: lspec                   ! index for aerosol number / chem-mass / water-mass
    integer :: lcoardust, lcoarnacl    ! indices for coarse mode dust and seasalt masses
    real(r8) :: dqdt_tmp(pcols,pver)   ! temporary array to hold tendency for 1 species
    real(r8) :: f_act_conv(pcols,pver) ! prescribed aerosol activation fraction for convective cloud   ! rce 2010/05/01
    real(r8) :: f_act_conv_coarse(pcols,pver) ! similar but for coarse mode                            ! rce 2010/05/02
    real(r8) :: f_act_conv_coarse_dust, f_act_conv_coarse_nacl                                         ! rce 2010/05/02
    real(r8) :: fracis_cw(pcols,pver)
    real(r8) :: hygro_sum_old(pcols,pver)  ! before removal    [sum of (mass*hydro/dens)]
    real(r8) :: hygro_sum_del(pcols,pver)  ! removal change to [sum of (mass*hydro/dens)]
    real(r8) :: hygro_sum_old_ik, hygro_sum_new_ik
    real(r8) :: prec(pcols)                ! precipitation rate
    real(r8) :: q_tmp(pcols,pver)          ! temporary array to hold "most current" mixing ratio for 1 species
    real(r8) :: qqcw_tmp(pcols,pver)       ! temporary array to hold qqcw   ! rce 2010/05/01
    real(r8) :: scavcoefnv(pcols,pver,0:2) ! Dana and Hales coefficient (/mm) for
                                           ! cloud-borne num & vol (0), 
                                           ! interstitial num (1), interstitial vol (2)
    real(r8) :: tmpa, tmpb
    real(r8) :: tmpdust, tmpnacl
    real(r8) :: water_old, water_new   ! temporary old/new aerosol water mix-rat
    logical :: isprx(pcols,pver) ! true if precipation
    real(r8) :: aerdepwetis(pcols,pcnst)  ! aerosol wet deposition (interstitial)
    real(r8) :: aerdepwetcw(pcols,pcnst)  ! aerosol wet deposition (cloud water)
    real(r8), pointer :: fldcw(:,:)
#endif

    !-----------------------------------------------------------------------
    call cnst_get_ind('CLDICE', ixcldice)
    call cnst_get_ind('CLDLIQ', ixcldliq)
#ifndef WRF_PORT
    lchnk = state%lchnk
    ncol  = state%ncol
#else
    lchnk = lchnk_in
    ncol  = ncol_in
#endif

    totcond(:ncol, :) = state_q(:ncol,:,ixcldliq) + state_q(:ncol,:,ixcldice)

    if ( num_mz_aerosols > 0 ) then

       ! Wet deposition of mozart aerosol species.
       ptend_name  = ptend_name//'+mz_aero_wetdep'

#ifdef MODAL_AERO
     prec(:ncol)=0._r8
     do k=1,pver
        where (prec(:ncol) >= 1.e-7)
            isprx(:ncol,k) = .true.
        elsewhere
            isprx(:ncol,k) = .false.
        endwhere
        prec(:ncol) = prec(:ncol) + (prain(:ncol,k) + cmfdqr(:ncol,k) - evapr(:ncol,k)) &
                    *state_pdel(:ncol,k)/gravit
     end do


! calculate the mass-weighted sol_factic for coarse mode species
!    sol_factic_coarse(:,:) = 0.30_r8   ! tuned 1/4
     f_act_conv_coarse(:,:) = 0.60_r8   ! rce 2010/05/02
     f_act_conv_coarse_dust = 0.40_r8   ! rce 2010/05/02
     f_act_conv_coarse_nacl = 0.80_r8   ! rce 2010/05/02
     if (modeptr_coarse > 0) then
        lcoardust = lptr_dust_a_amode(modeptr_coarse)
        lcoarnacl = lptr_nacl_a_amode(modeptr_coarse)
        if ((lcoardust > 0) .and. (lcoarnacl > 0)) then
           do k = 1, pver
           do i = 1, ncol
              tmpdust = max( 0.0_r8, state_q(i,k,lcoardust) + ptend_q(i,k,lcoardust)*dt )
              tmpnacl = max( 0.0_r8, state_q(i,k,lcoarnacl) + ptend_q(i,k,lcoarnacl)*dt )
              if ((tmpdust+tmpnacl) > 1.0e-30_r8) then
!                sol_factic_coarse(i,k) = (0.2_r8*tmpdust + 0.4_r8*tmpnacl)/(tmpdust+tmpnacl)  ! tuned 1/6
                 f_act_conv_coarse(i,k) = (f_act_conv_coarse_dust*tmpdust &
                                         + f_act_conv_coarse_nacl*tmpnacl)/(tmpdust+tmpnacl)  ! rce 2010/05/02
              end if
           end do
           end do
        end if
     end if


    scavcoefnv(:,:,0) = 0.0_r8   ! below-cloud scavcoef = 0.0 for cloud-borne species

    do m = 1, ntot_amode   ! main loop over aerosol modes

       do lphase = 1, 2   ! loop over interstitial (1) and cloud-borne (2) forms

! sol_factb and sol_facti values
!    sol_factb - currently this is basically a tuning factor
!    sol_facti & sol_factic - currently has a physical basis, and reflects activation fraction
!
! 2008-mar-07 rce - sol_factb  (interstitial) changed from 0.3 to 0.1
!                 - sol_factic (interstitial, dust modes) changed from 1.0 to 0.5
!                 - sol_factic (cloud-borne, pcarb modes) no need to set it to 0.0
!                       because the cloud-borne pcarbon == 0 (no activation)
!
! rce 2010/05/02
! prior to this date, sol_factic was used for convective in-cloud wet removal,
!    and its value reflected a combination of an activation fraction (which varied between modes)
!    and a tuning factor
! from this date forward, two parameters are used for convective in-cloud wet removal
!    f_act_conv is the activation fraction
!       note that "non-activation" of aerosol in air entrained into updrafts should
!          be included here
!       eventually we might use the activate routine (with w ~= 1 m/s) to calculate 
!          this, but there is still the entrainment issue
!    sol_factic is strictly a tuning factor
!
          if (lphase == 1) then   ! interstial aerosol
             hygro_sum_old(:,:) = 0.0_r8
             hygro_sum_del(:,:) = 0.0_r8
             call modal_aero_bcscavcoef_get( m, ncol, isprx, dgncur_awet,   &
                                             scavcoefnv(:,:,1), scavcoefnv(:,:,2) )

             sol_factb  = 0.1_r8   ! all below-cloud scav ON (0.1 "tuning factor")
!            sol_factb  = 0.03_r8   ! all below-cloud scav ON (0.1 "tuning factor")  ! tuned 1/6

             sol_facti  = 0.0_r8   ! strat  in-cloud scav totally OFF for institial

             sol_factic = 0.4_r8      ! xl 2010/05/20

             if (m == modeptr_pcarbon) then
!               sol_factic = 0.0_r8   ! conv   in-cloud scav OFF (0.0 activation fraction)
                f_act_conv = 0.0_r8   ! rce 2010/05/02
             else if ((m == modeptr_finedust) .or. (m == modeptr_coardust)) then
!               sol_factic = 0.2_r8   ! conv   in-cloud scav ON  (0.5 activation fraction)  ! tuned 1/4
                f_act_conv = 0.4_r8   ! rce 2010/05/02
             else
!               sol_factic = 0.4_r8   ! conv   in-cloud scav ON  (1.0 activation fraction)  ! tuned 1/4
                f_act_conv = 0.8_r8   ! rce 2010/05/02
             end if

          else   ! cloud-borne aerosol (borne by stratiform cloud drops)
             sol_factb  = 0.0_r8   ! all below-cloud scav OFF (anything cloud-borne is located "in-cloud")

             sol_facti  = 1.0_r8   ! strat  in-cloud scav totally ON for cloud-borne
!            sol_facti  = 0.6_r8   ! strat  in-cloud scav totally ON for cloud-borne  ! tuned 1/6
             sol_factic = 0.0_r8   ! conv   in-cloud scav OFF (having this on would mean
                                   !        that conv precip collects strat droplets)
             f_act_conv = 0.0_r8   ! conv   in-cloud scav OFF (having this on would mean

          end if
!
! rce 2010/05/03
! wetdepa has 6 "sol_fact" parameters: 
!      sol_facti, sol_factic, sol_factb for liquid cloud
!      sol_factii, sol_factiic, sol_factbi for ice cloud
! the ice cloud parameters are optional, and if not provided, they default to
!    one of the other sol_fact parameters (see subr. wetdepa about this)
! for now, we set the ice cloud parameters equal 
!    to their liquid cloud counterparts
! currently the ice parameters are not used in wetdepa as
!    wetdepa sets "weight" (the ice cloud fraction) to 0.0
! if this changes, we will have to give more thought to 
!    the ice cloud parameter values
!
          sol_factbi  = sol_factb
          sol_factii  = sol_facti
          sol_factiic = sol_factic(1,1)


          do lspec = 0, nspec_amode(m)+1   ! loop over number + chem constituents + water

             if (lspec == 0) then   ! number
                if (lphase == 1) then
                   mm = numptr_amode(m)
                   jnv = 1
                else
                   mm = numptrcw_amode(m)
                   jnv = 0
                endif
             else if (lspec <= nspec_amode(m)) then   ! non-water mass
                if (lphase == 1) then
                   mm = lmassptr_amode(lspec,m)
                   jnv = 2
                else
                   mm = lmassptrcw_amode(lspec,m)
                   jnv = 0
                endif
             else   ! water mass
!   bypass wet removal of aerosol water
                cycle
                if (lphase == 1) then
                   mm = 0
!                  mm = lwaterptr_amode(m)
                   jnv = 2
                else
                   mm = 0
                   jnv = 0
                endif
             endif

          if (mm <= 0) cycle


! set f_act_conv for interstitial (lphase=1) coarse mode species
! for the convective in-cloud, we conceptually treat the coarse dust and seasalt
!    as being externally mixed, and apply f_act_conv = f_act_conv_coarse_dust/nacl to dust/seasalt
! number and sulfate are conceptually partitioned to the dust and seasalt
!    on a mass basis, so the f_act_conv for number and sulfate are
!    mass-weighted averages of the values used for dust/seasalt
          if ((lphase == 1) .and. (m == modeptr_coarse)) then
!            sol_factic = sol_factic_coarse
             f_act_conv = f_act_conv_coarse   ! rce 2010/05/02
             if (lspec > 0) then
                if (lmassptr_amode(lspec,m) == lptr_dust_a_amode(m)) then
!                  sol_factic = 0.2_r8   ! tuned 1/4
                   f_act_conv = f_act_conv_coarse_dust   ! rce 2010/05/02
                else if (lmassptr_amode(lspec,m) == lptr_nacl_a_amode(m)) then
!                  sol_factic = 0.4_r8   ! tuned 1/6
                   f_act_conv = f_act_conv_coarse_nacl   ! rce 2010/05/02
                end if
             end if
          end if
          if ((lphase == 1) .and. (lspec <= nspec_amode(m))) then
             ptend_lq(mm) = .TRUE.
             dqdt_tmp(:,:) = 0.0_r8
! q_tmp reflects changes from modal_aero_calcsize and is the "most current" q
             q_tmp(1:ncol,:) = state_q(1:ncol,:,mm) + ptend_q(1:ncol,:,mm)*dt
#ifndef WRF_PORT
             fldcw => qqcw_get_field(mm,lchnk)
#else
             fldcw => qqcw(:,:,mm)
#endif
             call wetdepa_v2( state_t, state_pmid, state_q, state_pdel,  &
                  cldn, cldc, cmfdqr, evapc, conicw, prain, cme,                     &
                  evapr, totcond, q_tmp(:,:), dt,            &
                  dqdt_tmp(:,:), iscavt, cldv, cldvcu, cldvst, dlf, fracis(:,:,mm), sol_factb, ncol, &
                  scavcoefnv(:,:,jnv), &
                  .false., rate1ord_cw2pr_st, fldcw, f_act_conv, &     ! rce 2010/05/01
                  icscavt=icscavt, isscavt=isscavt, bcscavt=bcscavt, bsscavt=bsscavt, &
                  sol_facti_in=sol_facti, sol_factbi_in=sol_factbi, sol_factii_in=sol_factii, &   ! rce 2010/05/03
                  sol_factic_in=sol_factic, sol_factiic_in=sol_factiic )                          ! rce 2010/05/03

             ptend_q(1:ncol,:,mm) = ptend_q(1:ncol,:,mm) + dqdt_tmp(1:ncol,:)

             call outfld( trim(cnst_name(mm))//'WET', dqdt_tmp(:,:), pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SIC', icscavt, pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SIS', isscavt, pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SBC', bcscavt, pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SBS', bsscavt, pcols, lchnk)

             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+dqdt_tmp(i,k)*state_pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name(mm))//'SFWET', sflx, pcols, lchnk)
             aerdepwetis(:ncol,mm) = sflx(:ncol)

             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+icscavt(i,k)*state_pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name(mm))//'SFSIC', sflx, pcols, lchnk)
             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+isscavt(i,k)*state_pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name(mm))//'SFSIS', sflx, pcols, lchnk)
             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+bcscavt(i,k)*state_pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name(mm))//'SFSBC', sflx, pcols, lchnk)
             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+bsscavt(i,k)*state_pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name(mm))//'SFSBS', sflx, pcols, lchnk)

             if (lspec > 0) then
                tmpa = spechygro(lspectype_amode(lspec,m))/ &
                       specdens_amode(lspectype_amode(lspec,m))
                tmpb = tmpa*dt
                hygro_sum_old(1:ncol,:) = hygro_sum_old(1:ncol,:) &
                                         + tmpa*q_tmp(1:ncol,:)
                hygro_sum_del(1:ncol,:) = hygro_sum_del(1:ncol,:) &
                                         + tmpb*dqdt_tmp(1:ncol,:)
             end if

          else if ((lphase == 1) .and. (lspec == nspec_amode(m)+1)) then
! aerosol water -- because of how wetdepa treats evaporation of stratiform
!    precip, it is not appropriate to apply wetdepa to aerosol water
! instead, "hygro_sum" = [sum of (mass*hygro/dens)] is calculated before and 
!    after wet removal, and new water is calculated using
!    new_water = old_water*min(10,(hygro_sum_new/hygro_sum_old))
! the "min(10,...)" is to avoid potential problems when hygro_sum_old ~= 0
! also, individual wet removal terms (ic,is,bc,bs) are not output to history
!            ptend%lq(mm) = .TRUE.
!            dqdt_tmp(:,:) = 0.0_r8
             do k = 1, pver
             do i = 1, ncol
!               water_old = max( 0.0_r8, state%q(i,k,mm)+ptend%q(i,k,mm)*dt )
                water_old = max( 0.0_r8, qaerwat(i,k,mm) )
                hygro_sum_old_ik = max( 0.0_r8, hygro_sum_old(i,k) )
                hygro_sum_new_ik = max( 0.0_r8, hygro_sum_old_ik+hygro_sum_del(i,k) )
                if (hygro_sum_new_ik >= 10.0_r8*hygro_sum_old_ik) then
                   water_new = 10.0_r8*water_old
                else
                   water_new = water_old*(hygro_sum_new_ik/hygro_sum_old_ik)
                end if
!               dqdt_tmp(i,k) = (water_new - water_old)/dt
                qaerwat(i,k,mm) = water_new
             end do
             end do

!            ptend%q(1:ncol,:,mm) = ptend%q(1:ncol,:,mm) + dqdt_tmp(1:ncol,:)

!            call outfld( trim(cnst_name(mm))//'WET', dqdt_tmp(:,:), pcols, lchnk)

!            sflx(:)=0._r8
!            do k=1,pver
!               do i=1,ncol
!                  sflx(i)=sflx(i)+dqdt_tmp(i,k)*state%pdel(i,k)/gravit
!               enddo
!            enddo
!            call outfld( trim(cnst_name(mm))//'SFWET', sflx, pcols, lchnk)

          else   ! lphase == 2
             dqdt_tmp(:,:) = 0.0_r8
             qqcw_tmp(:,:) = 0.0_r8    ! rce 2010/05/01
#ifndef WRF_PORT
             fldcw => qqcw_get_field(mm,lchnk)
#else
             fldcw => qqcw(:,:,mm)
#endif
             call wetdepa_v2( state_t, state_pmid, state_q, state_pdel,  &
                  cldn, cldc, cmfdqr, evapc, conicw, prain, cme,                     &
                  evapr, totcond, fldcw, dt,            &
                  dqdt_tmp(:,:), iscavt, cldv, cldvcu, cldvst, dlf, fracis_cw(:,:), sol_factb, ncol, &
                  scavcoefnv(:,:,jnv), &
                  .true., rate1ord_cw2pr_st, qqcw_tmp, f_act_conv, &     ! rce 2010/05/01
                  icscavt=icscavt, isscavt=isscavt, bcscavt=bcscavt, bsscavt=bsscavt, &
                  sol_facti_in=sol_facti, sol_factbi_in=sol_factbi, sol_factii_in=sol_factii, &   ! rce 2010/05/03
                  sol_factic_in=sol_factic, sol_factiic_in=sol_factiic )                          ! rce 2010/05/03

             fldcw(1:ncol,:) = fldcw(1:ncol,:) + dqdt_tmp(1:ncol,:) * dt

             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+dqdt_tmp(i,k)*state_pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name_cw(mm))//'SFWET', sflx, pcols, lchnk)
             aerdepwetcw(:ncol,mm) = sflx(:ncol)

             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+icscavt(i,k)*state_pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name_cw(mm))//'SFSIC', sflx, pcols, lchnk)
             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+isscavt(i,k)*state_pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name_cw(mm))//'SFSIS', sflx, pcols, lchnk)
             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+bcscavt(i,k)*state_pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name_cw(mm))//'SFSBC', sflx, pcols, lchnk)
             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+bsscavt(i,k)*state_pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name_cw(mm))//'SFSBS', sflx, pcols, lchnk)

          endif

          enddo   ! lspec = 0, nspec_amode(m)+1
       enddo   ! lphase = 1, 2
    enddo   ! m = 1, ntot_amode
#ifndef WRF_PORT
    call set_srf_wetdep(aerdepwetis, aerdepwetcw, cam_out)
#endif

#else
       scavcoef(:ncol,:)=0.1_r8
       do m = 1, num_mz_aerosols

          mm = mz_aerosol_ids(m)

          if (mm > 0 .and. (.not. any(dust_names==cnst_name(mm)))  .and. (.not. any(progseasalts_names==cnst_name(mm))) ) then
             ptend%lq(mm) = .TRUE.

             sol_factb = 0.3_r8
             sol_facti = 0.3_r8
             call wetdepa_v1( state%t, state%pmid, state%q, state%pdel,  &
                  cldn, cldc, cmfdqr, conicw, prain, cme,                     &
                  evapr, totcond, state%q(:,:,mm), dt,            &
                  ptend%q(:,:,mm), iscavt, cldv, fracis(:,:,mm), sol_factb, ncol, &
                  scavcoef, icscavt=icscavt, isscavt=isscavt, bcscavt=bcscavt, bsscavt=bsscavt, &
                  sol_facti_in=sol_facti)

             call outfld( trim(cnst_name(mm))//'WET', ptend%q(:,:,mm), pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SIC', icscavt , pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SIS', isscavt, pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SBC', bcscavt, pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'SBS', bsscavt, pcols, lchnk)

             sflx(:)=0._r8
             do k=1,pver
                do i=1,ncol
                   sflx(i)=sflx(i)+ptend%q(i,k,mm)*state%pdel(i,k)/gravit
                enddo
             enddo
             call outfld( trim(cnst_name(mm))//'SFWET', sflx, pcols, lchnk)

             ! export deposition fluxes to coupler
             select case (trim(cnst_name(mm)))
             case('CB2')
                do i = 1, ncol
                   cam_out%bcphiwet(i) = max(-sflx(i), 0._r8)
                end do
             case('OC2')
                do i = 1, ncol
                   cam_out%ocphiwet(i) = max(-sflx(i), 0._r8)
                end do
             end select

          endif

       end do
#endif
    endif
#ifndef WRF_PORT
    if ( do_cam_sulfchem ) then

       so4(:ncol,:)  = state%q(:ncol,:,id_so4) + ptend%q(:ncol,:,id_so4)*dt
       so2(:ncol,:)  = state%q(:ncol,:,id_so2) + ptend%q(:ncol,:,id_so2)*dt
       dms(:ncol,:)  = state%q(:ncol,:,id_dms)
       h2o2(:ncol,:) = state%q(:ncol,:,id_h2o2)

       ! need oh,ho2,no3 in molec/cm3, o3 in kg/kg

       if ( inv_o3 ) then
         call get_cnst_data( 'O3', o3(:ncol,:),  ncol, lchnk ) ! vmr
       else
         o3(:ncol,:) = state%q(:ncol,:,id_o3)*amass/fix_mass(id_o3) ! vmr
       endif

       ! vmr -> molec/cm3
       xhnm(:ncol,:) = 1.e-6_r8 * state%pmiddry(:ncol,:) / (boltz*state%t(:ncol,:))

       if ( inv_oh ) then
         call get_cnst_data( 'OH', oh(:ncol,:),  ncol, lchnk ) ! vmr
         oh(:ncol,:) = oh(:ncol,:) * xhnm(:ncol,:)             ! molec/cm3
       else
         oh(:ncol,:) = state%q(:ncol,:,id_oh)* (amass/cnst_mw(id_oh )) * xhnm(:ncol,:)
       endif
       if ( inv_no3 ) then
         call get_cnst_data( 'NO3', no3(:ncol,:),  ncol, lchnk ) ! vmr
         no3(:ncol,:) = no3(:ncol,:) * xhnm(:ncol,:)             ! molec/cm3
       else
         no3(:ncol,:) = state%q(:ncol,:,id_no3)* (amass/cnst_mw(id_no3)) * xhnm(:ncol,:)
       endif
       if ( inv_ho2 ) then
         call get_cnst_data( 'HO2', ho2(:ncol,:),  ncol, lchnk ) ! vmr
         ho2(:ncol,:) = ho2(:ncol,:) * xhnm(:ncol,:)             ! molec/cm3
       else
         ho2(:ncol,:) = state%q(:ncol,:,id_ho2)* (amass/cnst_mw(id_ho2)) * xhnm(:ncol,:) 
       endif

       ! alter HO2 from diurnal average to instantaneous value
       call dicor( calday, state%lat, ncol, dicorfac)
       do k = 1, pver
          do i = 1, ncol
             ho2(i,k) = ho2(i,k)*dicorfac(i)
          end do
       end do

       icwmr2(:,:) = 0._r8
       call cldychmini( state%t,  state%pmid, totcond, rainmr, cldn, &
            cldv, conicw, icwmr2, so2, so4, &
            ph, hion, ekso2, ekh2o2, indcp, &
            ncldypts, ncol )
       call outfld( 'PH', ph, pcols, lchnk )

       call sulf_chemdr( state%t, state%pmid, totcond, rainmr, cldn, cldv, &
                         conicw, icwmr2, indcp, ncldypts, hion, &
                         so2, so4, dms,  h2o2, ekso2, ekh2o2, &
                         o3, h2o23d, oh, no3, ho2, state%q(:,:,1),  &
                         state%zm, dt, calday, state%lat, state%lon, ncol, lchnk  )

       ptend%q(:ncol,:,id_so2) = (so2(:ncol,:)-state%q(:ncol,:,id_so2))/dt
       ptend%lq(id_so2) = .true.

       ptend%q(:ncol,:,id_so4) = (so4(:ncol,:)-state%q(:ncol,:,id_so4))/dt
       ptend%lq(id_so4) = .true.

       ptend%q(:ncol,:,id_dms) = (dms(:ncol,:)-state%q(:ncol,:,id_dms))/dt
       ptend%lq(id_dms) = .true.

       ptend%q(:ncol,:,id_h2o2) = (h2o2(:ncol,:)-state%q(:ncol,:,id_h2o2))/dt
       ptend%lq(id_h2o2) = .true.

    endif
#endif
    return

  end subroutine mz_aero_wet_intr


#ifdef MODAL_AERO
#ifndef WRF_PORT
  !===============================================================================
  subroutine mz_aero_dry_intr ( state, ptend, wvflx, dt, lat, clat, &
       fsds, obklen, ts, ustar, prect, snowh, pblh, hflx, month, landfrac, &
       icefrac, ocnfrac, fvin,ram1in, dgncur_awet, wetdens,  qaerwat, &
       cam_out)
    !-----------------------------------------------------------------------
    !
    ! Purpose:
    ! Interface to dry deposition and sedimentation of aerosols
    !
    ! Author: Xiaohong Liu and Dick Easter
    !
    !-----------------------------------------------------------------------
    use cam_history,       only: outfld
    use ppgrid,            only: pverp
    use physics_types,     only: physics_state, physics_ptend
    use camsrfexch_types,  only: cam_out_t
    use physconst,         only: gravit, rair, rhoh2o
    use drydep_mod,        only: setdvel,  d3ddflux, calcram
    use dust_sediment_mod, only: dust_sediment_tend, dust_sediment_vel
    use modal_aero_data
    use modal_aero_deposition, only: set_srf_drydep
    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments:
    !
    real(r8),            intent(in)  :: dt             ! time step
    type(physics_state), intent(in ) :: state          ! Physics state variables
    integer, intent(in) :: lat(pcols)                  ! latitude index for S->N storage
    real(r8), intent(in) :: clat(pcols)                 ! latitude
    real(r8), intent(in) :: fsds(pcols)                 ! longwave down at sfc
    real(r8), intent(in) :: obklen(pcols)                 ! obklen
    real(r8), intent(in) :: ustar(pcols)                  ! sfc fric vel--used over oceans and sea ice.
    real(r8), intent(in) :: ts(pcols)                     ! sfc temp
    real(r8), intent(in) :: landfrac(pcols)               ! land fraction
    real(r8), intent(in) :: icefrac(pcols)                ! ice fraction
    real(r8), intent(in) :: ocnfrac(pcols)                ! ocean fraction
    real(r8), intent(in) :: hflx(pcols)                  ! sensible heat flux
    real(r8), intent(in) :: prect(pcols)                     ! prect
    real(r8), intent(in) :: snowh(pcols)                     ! snow depth
    real(r8), intent(in) :: pblh(pcols)                     ! pbl height
    integer, intent(in)  :: month
    real(r8), intent(in) :: wvflx(pcols)       ! water vapor flux
    real(r8), intent(in) :: fvin(pcols)        ! for dry dep velocities from land model
    real(r8), intent(in) :: ram1in(pcols)       ! for dry dep velocities from land model
    real(r8), intent(in) :: dgncur_awet(pcols,pver,ntot_amode)
    real(r8), intent(in) :: wetdens(pcols,pver,ntot_amode)
    real(r8), intent(inout) :: qaerwat(pcols,pver,ntot_amode)

    type(physics_ptend), intent(inout) :: ptend          ! indivdual parameterization tendencies
    type(cam_out_t),     intent(inout) :: cam_out
    !
    ! Local variables
    !
    integer :: i,k
    integer :: jvlc                    ! index for last dimension of vlc_xxx arrays
    integer :: lphase                  ! index for interstitial / cloudborne aerosol
    integer :: lspec                   ! index for aerosol number / chem-mass / water-mass
    integer :: m                       ! aerosol mode index
    integer :: mm                      ! tracer index
    integer :: lchnk                   ! chunk identifier
    integer :: ncol                    ! number of atmospheric columns
    real(r8) :: tvs(pcols,pver)
    real(r8) :: sflx(pcols)            ! deposition flux
    real(r8)::  dep_trb(pcols)       !kg/m2/s
    real(r8)::  dep_dry(pcols)       !kg/m2/s (total of grav and trb)
    real(r8)::  dep_grv(pcols)       !kg/m2/s (total of grav and trb)
    real (r8) :: rho(pcols,pver)                    ! air density in kg/m3
    real(r8) :: pvmzaer(pcols,pverp)    ! sedimentation velocity in Pa
    real(r8) :: fv(pcols)         ! for dry dep velocities, from land modified over ocean & ice
    real(r8) :: ram1(pcols)       ! for dry dep velocities, from land modified over ocean & ice
    real(r8) :: dqdt_tmp(pcols,pver)   ! temporary array to hold tendency for 1 species

    real(r8) :: rad_drop(pcols,pver)
    real(r8) :: dens_drop(pcols,pver)
    real(r8) :: sg_drop(pcols,pver)
    real(r8) :: rad_aer(pcols,pver)
    real(r8) :: dens_aer(pcols,pver)
    real(r8) :: sg_aer(pcols,pver)

    real(r8) :: vlc_dry(pcols,pver,4)       ! dep velocity
    real(r8) :: vlc_grv(pcols,pver,4)       ! dep velocity
    real(r8)::  vlc_trb(pcols,4)            ! dep velocity
    real(r8) :: aerdepdryis(pcols,pcnst)  ! aerosol dry deposition (interstitial)
    real(r8) :: aerdepdrycw(pcols,pcnst)  ! aerosol dry deposition (cloud water)
    real(r8), pointer :: fldcw(:,:)
    !-----------------------------------------------------------------------

    lchnk = state%lchnk
    ncol  = state%ncol
    tvs(:ncol,:) = state%t(:ncol,:)!*(1+state%q(:ncol,k)
    rho(:ncol,:)=  state%pmid(:ncol,:)/(rair*state%t(:ncol,:))

    ! Dry deposition of mozart aerosol species.
    ptend%name  = ptend%name//'+mz_aero_drydep'

    !   #################################
    !    call setdvel( ncol, landfrac, icefrac, ocnfrac, .001_r8, .001_r8, .001_r8, dvel )
    !  we get the ram1,fv from the land model as ram1in,fvin, but need to calculate it over oceans and ice.
    !  better if we got thse from the ocean and ice model
    !  for friction velocity, we use ustar (from taux and tauy), except over land, where we use fv from the land model.

    call calcram(ncol,landfrac,icefrac,ocnfrac,obklen,&
         ustar,ram1in,ram1,state%t(:,pver),state%pmid(:,pver),&
         state%pdel(:,pver),fvin,fv)
!   call outfld( 'airFV', fv(:), pcols, lchnk )
!   call outfld( 'RAM1', ram1(:), pcols, lchnk )
    !       call outfld( 'icefrc', icefrac(:), pcols, lchnk )


!
! calc settling/deposition velocities for cloud droplets (and cloud-borne aerosols)
!
! *** mean drop radius should eventually be computed from ndrop and qcldwtr
    rad_drop(:,:) = 5.0e-6_r8
    dens_drop(:,:) = rhoh2o
    sg_drop(:,:) = 1.46
    jvlc = 3
    call modal_aero_depvel_part( ncol,state%t(:,:), state%pmid(:,:), ram1, fv,  &
                     vlc_dry(:,:,jvlc), vlc_trb(:,jvlc), vlc_grv(:,:,jvlc),  &
                     rad_drop(:,:), dens_drop(:,:), sg_drop(:,:), 0, lchnk)
    jvlc = 4
    call modal_aero_depvel_part( ncol,state%t(:,:), state%pmid(:,:), ram1, fv,  &
                     vlc_dry(:,:,jvlc), vlc_trb(:,jvlc), vlc_grv(:,:,jvlc),  &
                     rad_drop(:,:), dens_drop(:,:), sg_drop(:,:), 3, lchnk)



    do m = 1, ntot_amode   ! main loop over aerosol modes

       do lphase = 1, 2   ! loop over interstitial / cloud-borne forms

          if (lphase == 1) then   ! interstial aerosol - calc settling/dep velocities of mode

! rad_aer = volume mean wet radius (m)
! dgncur_awet = geometric mean wet diameter for number distribution (m)
             rad_aer(1:ncol,:) = 0.5_r8*dgncur_awet(1:ncol,:,m)   &
                                 *exp(1.5*(alnsg_amode(m)**2))
! dens_aer(1:ncol,:) = wet density (kg/m3)
             dens_aer(1:ncol,:) = wetdens(1:ncol,:,m)
             sg_aer(1:ncol,:) = sigmag_amode(m)

             jvlc = 1
             call modal_aero_depvel_part( ncol, state%t(:,:), state%pmid(:,:), ram1, fv,  & 
                        vlc_dry(:,:,jvlc), vlc_trb(:,jvlc), vlc_grv(:,:,jvlc),  &
                        rad_aer(:,:), dens_aer(:,:), sg_aer(:,:), 0, lchnk)
             jvlc = 2
             call modal_aero_depvel_part( ncol, state%t(:,:), state%pmid(:,:), ram1, fv,  & 
                        vlc_dry(:,:,jvlc), vlc_trb(:,jvlc), vlc_grv(:,:,jvlc),  &
                        rad_aer(:,:), dens_aer(:,:), sg_aer(:,:), 3, lchnk)
          end if

          do lspec = 0, nspec_amode(m)+1   ! loop over number + constituents + water

             if (lspec == 0) then   ! number
                if (lphase == 1) then
                   mm = numptr_amode(m)
                   jvlc = 1
                else
                   mm = numptrcw_amode(m)
                   jvlc = 3
                endif
             else if (lspec <= nspec_amode(m)) then   ! non-water mass
                if (lphase == 1) then
                   mm = lmassptr_amode(lspec,m)
                   jvlc = 2
                else
                   mm = lmassptrcw_amode(lspec,m)
                   jvlc = 4
                endif
             else   ! water mass
!   bypass dry deposition of aerosol water
                cycle
                if (lphase == 1) then
                   mm = 0
!                  mm = lwaterptr_amode(m)
                   jvlc = 2
                else
                   mm = 0
                   jvlc = 4
                endif
             endif


          if (mm <= 0) cycle

!         if (lphase == 1) then
          if ((lphase == 1) .and. (lspec <= nspec_amode(m))) then
             ptend%lq(mm) = .TRUE.

             ! use pvprogseasalts instead (means making the top level 0)
             pvmzaer(:ncol,1)=0._r8
             pvmzaer(:ncol,2:pverp) = vlc_dry(:ncol,:,jvlc)

             call outfld( trim(cnst_name(mm))//'DDV', pvmzaer(:,2:pverp), pcols, lchnk )

             if(.true.) then ! use phil's method
             !      convert from meters/sec to pascals/sec
             !      pvprogseasalts(:,1) is assumed zero, use density from layer above in conversion
                pvmzaer(:ncol,2:pverp) = pvmzaer(:ncol,2:pverp) * rho(:ncol,:)*gravit

             !      calculate the tendencies and sfc fluxes from the above velocities
                call dust_sediment_tend( &
                     ncol,             dt,       state%pint(:,:), state%pmid, state%pdel, state%t , &
                     state%q(:,:,mm),  pvmzaer,  ptend%q(:,:,mm), sflx  )
             else   !use charlie's method
                call d3ddflux( ncol, vlc_dry(:,:,jvlc), state%q(:,:,mm), state%pmid, &
                               state%pdel, tvs, sflx, ptend%q(:,:,mm), dt )
             endif

             ! apportion dry deposition into turb and gravitational settling for tapes
             do i=1,ncol
                dep_trb(i)=sflx(i)*vlc_trb(i,jvlc)/vlc_dry(i,pver,jvlc)
                dep_grv(i)=sflx(i)*vlc_grv(i,pver,jvlc)/vlc_dry(i,pver,jvlc)
             enddo

             call outfld( trim(cnst_name(mm))//'DDF', sflx, pcols, lchnk)
             call outfld( trim(cnst_name(mm))//'TBF', dep_trb, pcols, lchnk )
             call outfld( trim(cnst_name(mm))//'GVF', dep_grv, pcols, lchnk )
             call outfld( trim(cnst_name(mm))//'DTQ', ptend%q(:,:,mm), pcols, lchnk)
             aerdepdryis(:ncol,mm) = sflx(:ncol)

          else if ((lphase == 1) .and. (lspec == nspec_amode(m)+1)) then  ! aerosol water
             ! use pvprogseasalts instead (means making the top level 0)
             pvmzaer(:ncol,1)=0._r8
             pvmzaer(:ncol,2:pverp) = vlc_dry(:ncol,:,jvlc)

             if(.true.) then ! use phil's method
             !      convert from meters/sec to pascals/sec
             !      pvprogseasalts(:,1) is assumed zero, use density from layer above in conversion
                pvmzaer(:ncol,2:pverp) = pvmzaer(:ncol,2:pverp) * rho(:ncol,:)*gravit

             !      calculate the tendencies and sfc fluxes from the above velocities
                call dust_sediment_tend( &
                     ncol,             dt,       state%pint(:,:), state%pmid, state%pdel, state%t , &
                     qaerwat(:,:,mm),  pvmzaer,  dqdt_tmp(:,:), sflx  )
             else   !use charlie's method
                call d3ddflux( ncol, vlc_dry(:,:,jvlc), qaerwat(:,:,mm), state%pmid, &
                               state%pdel, tvs, sflx, dqdt_tmp(:,:), dt )
             endif

             ! apportion dry deposition into turb and gravitational settling for tapes
             do i=1,ncol
                dep_trb(i)=sflx(i)*vlc_trb(i,jvlc)/vlc_dry(i,pver,jvlc)
                dep_grv(i)=sflx(i)*vlc_grv(i,pver,jvlc)/vlc_dry(i,pver,jvlc)
             enddo

             qaerwat(1:ncol,:,mm) = qaerwat(1:ncol,:,mm) + dqdt_tmp(1:ncol,:) * dt

          else  ! lphase == 2
             ! use pvprogseasalts instead (means making the top level 0)
             pvmzaer(:ncol,1)=0._r8
             pvmzaer(:ncol,2:pverp) = vlc_dry(:ncol,:,jvlc)
             fldcw => qqcw_get_field(mm,lchnk)
             if(.true.) then ! use phil's method
             !      convert from meters/sec to pascals/sec
             !      pvprogseasalts(:,1) is assumed zero, use density from layer above in conversion
                pvmzaer(:ncol,2:pverp) = pvmzaer(:ncol,2:pverp) * rho(:ncol,:)*gravit

             !      calculate the tendencies and sfc fluxes from the above velocities
                call dust_sediment_tend( &
                     ncol,             dt,       state%pint(:,:), state%pmid, state%pdel, state%t , &
                     fldcw(:,:),  pvmzaer,  dqdt_tmp(:,:), sflx  )
             else   !use charlie's method
                call d3ddflux( ncol, vlc_dry(:,:,jvlc), fldcw(:,:), state%pmid, &
                               state%pdel, tvs, sflx, dqdt_tmp(:,:), dt )
             endif

             ! apportion dry deposition into turb and gravitational settling for tapes
             do i=1,ncol
                dep_trb(i)=sflx(i)*vlc_trb(i,jvlc)/vlc_dry(i,pver,jvlc)
                dep_grv(i)=sflx(i)*vlc_grv(i,pver,jvlc)/vlc_dry(i,pver,jvlc)
             enddo

             fldcw(1:ncol,:) = fldcw(1:ncol,:) + dqdt_tmp(1:ncol,:) * dt

             call outfld( trim(cnst_name_cw(mm))//'DDF', sflx, pcols, lchnk)
             call outfld( trim(cnst_name_cw(mm))//'TBF', dep_trb, pcols, lchnk )
             call outfld( trim(cnst_name_cw(mm))//'GVF', dep_grv, pcols, lchnk )
             aerdepdrycw(:ncol,mm) = sflx(:ncol)

          endif

          enddo   ! lspec = 0, nspec_amode(m)+1
       enddo   ! lphase = 1, 2
    enddo   ! m = 1, ntot_amode

    call set_srf_drydep(aerdepdryis, aerdepdrycw, cam_out)

    return
  end subroutine mz_aero_dry_intr


  !===============================================================================
  subroutine modal_aero_depvel_part( ncol, t, pmid, ram1, fv, vlc_dry, vlc_trb, vlc_grv,  &
                                     radius_part, density_part, sig_part, moment, lchnk )

!    calculates surface deposition velocity of particles
!    L. Zhang, S. Gong, J. Padro, and L. Barrie
!    A size-seggregated particle dry deposition scheme for an atmospheric aerosol module
!    Atmospheric Environment, 35, 549-560, 2001.
!
!    Authors: X. Liu

    !
    ! !USES
    !
    use physconst,     only: pi,boltz, gravit, rair
    use mo_drydep,     only: n_land_type, fraction_landuse

    ! !ARGUMENTS:
    !
    implicit none
    !
    real(r8), intent(in) :: t(pcols,pver)       !atm temperature (K)
    real(r8), intent(in) :: pmid(pcols,pver)    !atm pressure (Pa)
    real(r8), intent(in) :: fv(pcols)           !friction velocity (m/s)
    real(r8), intent(in) :: ram1(pcols)         !aerodynamical resistance (s/m)
    real(r8), intent(in) :: radius_part(pcols,pver)    ! mean (volume/number) particle radius (m)
    real(r8), intent(in) :: density_part(pcols,pver)   ! density of particle material (kg/m3)
    real(r8), intent(in) :: sig_part(pcols,pver)       ! geometric standard deviation of particles
    integer,  intent(in) :: moment ! moment of size distribution (0 for number, 2 for surface area, 3 for volume)
    integer,  intent(in) :: ncol
    integer,  intent(in) :: lchnk

    real(r8), intent(out) :: vlc_trb(pcols)       !Turbulent deposn velocity (m/s)
    real(r8), intent(out) :: vlc_grv(pcols,pver)       !grav deposn velocity (m/s)
    real(r8), intent(out) :: vlc_dry(pcols,pver)       !dry deposn velocity (m/s)
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    ! Local Variables
    integer  :: m,i,k,ix                !indices
    real(r8) :: rho     !atm density (kg/m**3)
    real(r8) :: vsc_dyn_atm(pcols,pver)   ![kg m-1 s-1] Dynamic viscosity of air
    real(r8) :: vsc_knm_atm(pcols,pver)   ![m2 s-1] Kinematic viscosity of atmosphere
    real(r8) :: shm_nbr       ![frc] Schmidt number
    real(r8) :: stk_nbr       ![frc] Stokes number
    real(r8) :: mfp_atm(pcols,pver)       ![m] Mean free path of air
    real(r8) :: dff_aer       ![m2 s-1] Brownian diffusivity of particle
    real(r8) :: slp_crc(pcols,pver) ![frc] Slip correction factor
    real(r8) :: rss_trb       ![s m-1] Resistance to turbulent deposition
    real(r8) :: rss_lmn       ![s m-1] Quasi-laminar layer resistance
    real(r8) :: brownian      ! collection efficiency for Browning diffusion
    real(r8) :: impaction     ! collection efficiency for impaction
    real(r8) :: interception  ! collection efficiency for interception
    real(r8) :: stickfrac     ! fraction of particles sticking to surface
    real(r8) :: radius_moment(pcols,pver) ! median radius (m) for moment
    real(r8) :: lnsig         ! ln(sig_part)
    real(r8) :: dispersion    ! accounts for influence of size dist dispersion on bulk settling velocity
                              ! assuming radius_part is number mode radius * exp(1.5 ln(sigma))

    integer  :: lt
    real(r8) :: lnd_frc
    real(r8) :: wrk1, wrk2, wrk3

    ! constants
    real(r8) gamma(11)      ! exponent of schmidt number
!   data gamma/0.54d+00,  0.56d+00,  0.57d+00,  0.54d+00,  0.54d+00, &
!              0.56d+00,  0.54d+00,  0.54d+00,  0.54d+00,  0.56d+00, &
!              0.50d+00/
    data gamma/0.56d+00,  0.54d+00,  0.54d+00,  0.56d+00,  0.56d+00, &        
               0.56d+00,  0.50d+00,  0.54d+00,  0.54d+00,  0.54d+00, &
               0.54d+00/
    save gamma

    real(r8) alpha(11)      ! parameter for impaction
!   data alpha/50.00d+00,  0.95d+00,  0.80d+00,  1.20d+00,  1.30d+00, &
!               0.80d+00, 50.00d+00, 50.00d+00,  2.00d+00,  1.50d+00, &
!             100.00d+00/
    data alpha/1.50d+00,   1.20d+00,  1.20d+00,  0.80d+00,  1.00d+00, &
               0.80d+00, 100.00d+00, 50.00d+00,  2.00d+00,  1.20d+00, &
              50.00d+00/
    save alpha

    real(r8) radius_collector(11) ! radius (m) of surface collectors
!   data radius_collector/-1.00d+00,  5.10d-03,  3.50d-03,  3.20d-03, 10.00d-03, &
!                          5.00d-03, -1.00d+00, -1.00d+00, 10.00d-03, 10.00d-03, &
!                         -1.00d+00/
    data radius_collector/10.00d-03,  3.50d-03,  3.50d-03,  5.10d-03,  2.00d-03, &
                           5.00d-03, -1.00d+00, -1.00d+00, 10.00d-03,  3.50d-03, &
                          -1.00d+00/
    save radius_collector

    integer            :: iwet(11) ! flag for wet surface = 1, otherwise = -1
!   data iwet/1,   -1,   -1,   -1,   -1,  &
!            -1,   -1,   -1,    1,   -1,  &
!             1/
    data iwet/-1,  -1,   -1,   -1,   -1,  &
              -1,   1,   -1,    1,   -1,  &
              -1/
    save iwet


    !------------------------------------------------------------------------
    do k=1,pver
       do i=1,ncol

          lnsig = log(sig_part(i,k))
! use a maximum radius of 50 microns when calculating deposition velocity
          radius_moment(i,k) = min(50.0d-6,radius_part(i,k))*   &
                          exp((float(moment)-1.5)*lnsig*lnsig)
          dispersion = exp(2.*lnsig*lnsig)

          rho=pmid(i,k)/rair/t(i,k)

          ! Quasi-laminar layer resistance: call rss_lmn_get
          ! Size-independent thermokinetic properties
          vsc_dyn_atm(i,k) = 1.72e-5_r8 * ((t(i,k)/273.0_r8)**1.5_r8) * 393.0_r8 / &
               (t(i,k)+120.0_r8)      ![kg m-1 s-1] RoY94 p. 102
          mfp_atm(i,k) = 2.0_r8 * vsc_dyn_atm(i,k) / &   ![m] SeP97 p. 455
               (pmid(i,k)*sqrt(8.0_r8/(pi*rair*t(i,k))))
          vsc_knm_atm(i,k) = vsc_dyn_atm(i,k) / rho ![m2 s-1] Kinematic viscosity of air

          slp_crc(i,k) = 1.0_r8 + mfp_atm(i,k) * &
                  (1.257_r8+0.4_r8*exp(-1.1_r8*radius_moment(i,k)/(mfp_atm(i,k)))) / &
                  radius_moment(i,k)   ![frc] Slip correction factor SeP97 p. 464
          vlc_grv(i,k) = (4.0_r8/18.0_r8) * radius_moment(i,k)*radius_moment(i,k)*density_part(i,k)* &
                  gravit*slp_crc(i,k) / vsc_dyn_atm(i,k) ![m s-1] Stokes' settling velocity SeP97 p. 466
          vlc_grv(i,k) = vlc_grv(i,k) * dispersion

          vlc_dry(i,k)=vlc_grv(i,k)
       enddo
    enddo
    k=pver  ! only look at bottom level for next part
    do i=1,ncol
       dff_aer = boltz * t(i,k) * slp_crc(i,k) / &    ![m2 s-1]
                 (6.0_r8*pi*vsc_dyn_atm(i,k)*radius_moment(i,k)) !SeP97 p.474
       shm_nbr = vsc_knm_atm(i,k) / dff_aer                        ![frc] SeP97 p.972

       wrk2 = 0._r8
       wrk3 = 0._r8
       do lt = 1,n_land_type
          lnd_frc = fraction_landuse(i,lt,lchnk)
          if ( lnd_frc /= 0._r8 ) then
             brownian = shm_nbr**(-gamma(lt))
             if (radius_collector(lt) > 0.0d0) then
!       vegetated surface
                stk_nbr = vlc_grv(i,k) * fv(i) / (gravit*radius_collector(lt))
                interception = 2.0_r8*(radius_moment(i,k)/radius_collector(lt))**2.0_r8
             else
!       non-vegetated surface
                stk_nbr = vlc_grv(i,k) * fv(i) * fv(i) / (gravit*vsc_knm_atm(i,k))  ![frc] SeP97 p.965
                interception = 0.0_r8
             endif
             impaction = (stk_nbr/(alpha(lt)+stk_nbr))**2.0_r8   

             if (iwet(lt) > 0) then
                stickfrac = 1.0_r8
             else
                stickfrac = exp(-sqrt(stk_nbr))
                if (stickfrac < 1.0d-10) stickfrac = 1.0d-10
             endif
             rss_lmn = 1.0_r8 / (3.0_r8 * fv(i) * stickfrac * (brownian+interception+impaction))
             rss_trb = ram1(i) + rss_lmn + ram1(i)*rss_lmn*vlc_grv(i,k)

             wrk1 = 1.0_r8 / rss_trb
             wrk2 = wrk2 + lnd_frc*( wrk1 )
             wrk3 = wrk3 + lnd_frc*( wrk1 + vlc_grv(i,k) )
          endif
       enddo  ! n_land_type
       vlc_trb(i) = wrk2
       vlc_dry(i,k) = wrk3
    enddo !ncol

    return
  end subroutine modal_aero_depvel_part

#endif
!(WRF_PORT end)
  !===============================================================================
  subroutine modal_aero_bcscavcoef_get( m, ncol, isprx, dgn_awet, scavcoefnum, scavcoefvol )

    use modal_aero_data
    !-----------------------------------------------------------------------
    implicit none

	  integer,intent(in) :: m, ncol
	  logical,intent(in):: isprx(pcols,pver)
	  real(r8), intent(in) :: dgn_awet(pcols,pver,ntot_amode)
	  real(r8), intent(out) :: scavcoefnum(pcols,pver), scavcoefvol(pcols,pver)

	  integer i, k, jgrow
	  real(r8) dumdgratio, xgrow, dumfhi, dumflo, scavimpvol, scavimpnum


      do k = 1, pver
      do i = 1, ncol

!        do only if no precip
         if ( isprx(i,k)  ) then
!
!           interpolate table values using log of (actual-wet-size)/(base-dry-size)

	    dumdgratio = dgn_awet(i,k,m)/dgnum_amode(m)

	    if ((dumdgratio .ge. 0.99) .and. (dumdgratio .le. 1.01)) then
	        scavimpvol = scavimptblvol(0,m)
	        scavimpnum = scavimptblnum(0,m)
	    else
	        xgrow = log( dumdgratio ) / dlndg_nimptblgrow
	        jgrow = int( xgrow )
	        if (xgrow .lt. 0.) jgrow = jgrow - 1
	        if (jgrow .lt. nimptblgrow_mind) then
		    jgrow = nimptblgrow_mind
		    xgrow = jgrow
	        else
	            jgrow = min( jgrow, nimptblgrow_maxd-1 )
	        end if

	        dumfhi = xgrow - jgrow
	        dumflo = 1. - dumfhi

	        scavimpvol = dumflo*scavimptblvol(jgrow,m) +   &
			  dumfhi*scavimptblvol(jgrow+1,m)
	        scavimpnum = dumflo*scavimptblnum(jgrow,m) +   &
			  dumfhi*scavimptblnum(jgrow+1,m)

	    end if

!           impaction scavenging removal amount for volume
	    scavcoefvol(i,k) = exp( scavimpvol  )
!           impaction scavenging removal amount to number
	    scavcoefnum(i,k) = exp( scavimpnum  )

!           scavcoef = impaction scav rate (1/h) for precip = 1 mm/h
!           scavcoef = impaction scav rate (1/s) for precip = pfx_inrain
!           (scavcoef/3600) = impaction scav rate (1/s) for precip = 1 mm/h
!           (pfx_inrain*3600) = in-rain-area precip rate (mm/h)
!           impactrate = (scavcoef/3600) * (pfx_inrain*3600)
          else
            scavcoefvol(i,k) = 0._r8
	    scavcoefnum(i,k) = 0._r8
          end if

      end do
      end do

        return
	end subroutine modal_aero_bcscavcoef_get


  !===============================================================================
  subroutine modal_aero_bcscavcoef_init
!-----------------------------------------------------------------------
!
! Purpose:
! Computes lookup table for aerosol impaction/interception scavenging rates
!
! Authors: R. Easter
!
!-----------------------------------------------------------------------

  use shr_kind_mod,only: r8 => shr_kind_r8
  use modal_aero_data
#ifndef WRF_PORT
  use abortutils,  only: endrun
#else
  use module_cam_support, only: endrun
#endif

  implicit none


!   local variables
	integer nnfit_maxd
	parameter (nnfit_maxd=27)

	integer i, jgrow, jdens, jpress, jtemp, ll, mode, nnfit
        integer lunerr

	real(r8) dg0, dg0_cgs, press, &
	rhodryaero, rhowetaero, rhowetaero_cgs, rmserr, &
	scavratenum, scavratevol, sigmag,                &
	temp, wetdiaratio, wetvolratio
	real(r8) aafitnum(1), xxfitnum(1,nnfit_maxd), yyfitnum(nnfit_maxd)
	real(r8) aafitvol(1), xxfitvol(1,nnfit_maxd), yyfitvol(nnfit_maxd)
        
	lunerr = 6
	dlndg_nimptblgrow = log( 1.25d00 )

	do 7900 mode = 1, ntot_amode

	sigmag = sigmag_amode(mode)

	ll = lspectype_amode(1,mode)
	rhodryaero = specdens_amode(ll)

	do 7800 jgrow = nimptblgrow_mind, nimptblgrow_maxd

	wetdiaratio = exp( jgrow*dlndg_nimptblgrow )
	dg0 = dgnum_amode(mode)*wetdiaratio

	wetvolratio = exp( jgrow*dlndg_nimptblgrow*3. )
	rhowetaero = 1.0 + (rhodryaero-1.0)/wetvolratio
	rhowetaero = min( rhowetaero, rhodryaero )

!
!   compute impaction scavenging rates at 1 temp-press pair and save
!
	nnfit = 0

	temp = 273.16
	press = 0.75e6   ! dynes/cm2
	rhowetaero = rhodryaero

	dg0_cgs = dg0*1.0e2   ! m to cm
	rhowetaero_cgs = rhowetaero*1.0e-3   ! kg/m3 to g/cm3
	call calc_1_impact_rate( &
     		dg0_cgs, sigmag, rhowetaero_cgs, temp, press, &
     		scavratenum, scavratevol, lunerr )

	nnfit = nnfit + 1
	if (nnfit .gt. nnfit_maxd) then
	    write(lunerr,9110)
	    call endrun()
	end if
9110	format( '*** subr. modal_aero_bcscavcoef_init -- nnfit too big' )

	xxfitnum(1,nnfit) = 1.
	yyfitnum(nnfit) = log( scavratenum )

	xxfitvol(1,nnfit) = 1.
	yyfitvol(nnfit) = log( scavratevol )

5900	continue

!
! skip mlinfit stuff because scav table no longer has dependencies on
!    air temp, air press, and particle wet density
! just load the log( scavrate--- ) values
!
!!
!!   do linear regression
!!	log(scavrate) = a1 + a2*log(wetdens)
!!
!	call mlinft( xxfitnum, yyfitnum, aafitnum, nnfit, 1, 1, rmserr )
!	call mlinft( xxfitvol, yyfitvol, aafitvol, nnfit, 1, 1, rmserr )
!
!	scavimptblnum(jgrow,mode) = aafitnum(1)
!	scavimptblvol(jgrow,mode) = aafitvol(1)

	scavimptblnum(jgrow,mode) = yyfitnum(1)
	scavimptblvol(jgrow,mode) = yyfitvol(1)



7800	continue
7900	continue

        return
        end subroutine modal_aero_bcscavcoef_init


  !===============================================================================
	subroutine calc_1_impact_rate(             &
     		dg0, sigmag, rhoaero, temp, press, &
     		scavratenum, scavratevol, lunerr )
!
!   routine computes a single impaction scavenging rate
!	for precipitation rate of 1 mm/h
!
!   dg0 = geometric mean diameter of aerosol number size distrib. (cm)
!   sigmag = geometric standard deviation of size distrib.
!   rhoaero = density of aerosol particles (g/cm^3)
!   temp = temperature (K)
!   press = pressure (dyne/cm^2)
!   scavratenum = number scavenging rate (1/h)
!   scavratevol = volume or mass scavenging rate (1/h)
!   lunerr = logical unit for error message
!
     use shr_kind_mod, only: r8 => shr_kind_r8

	implicit none

!   subr. parameters
	integer lunerr
	real(r8) dg0, sigmag, rhoaero, temp, press, scavratenum, scavratevol

!   local variables
	integer nrainsvmax
	parameter (nrainsvmax=50)
	real(r8) rrainsv(nrainsvmax), xnumrainsv(nrainsvmax),&
     		vfallrainsv(nrainsvmax)

	integer naerosvmax
	parameter (naerosvmax=51)
	real(r8) aaerosv(naerosvmax), &
     	ynumaerosv(naerosvmax), yvolaerosv(naerosvmax)

	integer i, ja, jr, na, nr
	real(r8) a, aerodiffus, aeromass, ag0, airdynvisc, airkinvisc
     	real(r8) anumsum, avolsum, boltzmann, cair, chi
     	real(r8) d, dr, dum, dumfuchs, dx
     	real(r8) ebrown, eimpact, eintercept, etotal, freepath, gravity 
     	real(r8) pi, precip, precipmmhr, precipsum
     	real(r8) r, rainsweepout, reynolds, rhi, rhoair, rhowater, rlo, rnumsum
     	real(r8) scavsumnum, scavsumnumbb
     	real(r8) scavsumvol, scavsumvolbb
     	real(r8) schmidt, sqrtreynolds, sstar, stokes, sx              
     	real(r8) taurelax, vfall, vfallstp
     	real(r8) x, xg0, xg3, xhi, xlo, xmuwaterair                     


	rlo = .005
	rhi = .250
	dr = 0.005
	nr = 1 + nint( (rhi-rlo)/dr )
	if (nr .gt. nrainsvmax) then
	    write(lunerr,9110)
	    call endrun()
	end if
9110	format( '*** subr. calc_1_impact_rate -- nr > nrainsvmax' )

	precipmmhr = 1.0
	precip = precipmmhr/36000.

	ag0 = dg0/2.
	sx = log( sigmag )
	xg0 = log( ag0 )
	xg3 = xg0 + 3.*sx*sx

	xlo = xg3 - 4.*sx
	xhi = xg3 + 4.*sx
	dx = 0.2*sx

	dx = max( 0.2_r8*sx, 0.01_r8 )
	xlo = xg3 - max( 4._r8*sx, 2._r8*dx )
	xhi = xg3 + max( 4._r8*sx, 2._r8*dx )

	na = 1 + nint( (xhi-xlo)/dx )
	if (na .gt. naerosvmax) then
	    write(lunerr,9120)
	    call endrun()
	end if
9120	format( '*** subr. calc_1_impact_rate -- na > naerosvmax' )

!   air molar density
	cair = press/(8.31436e7*temp)
!   air mass density
	rhoair = 28.966*cair
!   molecular freepath
	freepath = 2.8052e-10/cair
!   boltzmann constant
	boltzmann = 1.3807e-16
!   water density
	rhowater = 1.0
!   gravity
	gravity = 980.616
!   air dynamic viscosity
	airdynvisc = 1.8325e-4 * (416.16/(temp+120.)) *    &
                                        ((temp/296.16)**1.5)
!   air kinemaic viscosity
	airkinvisc = airdynvisc/rhoair
!   ratio of water viscosity to air viscosity (from Slinn)
	xmuwaterair = 60.0

	pi = 3.1415926536

!
!   compute rain drop number concentrations
!	rrainsv = raindrop radius (cm)
!	xnumrainsv = raindrop number concentration (#/cm^3)
!		(number in the bin, not number density)
!	vfallrainsv = fall velocity (cm/s)
!
	precipsum = 0.
	do i = 1, nr
	    r = rlo + (i-1)*dr
	    rrainsv(i) = r
	    xnumrainsv(i) = exp( -r/2.7e-2 )

	    d = 2.*r
	    if (d .le. 0.007) then
		vfallstp = 2.88e5 * d**2.
	    else if (d .le. 0.025) then
		vfallstp = 2.8008e4 * d**1.528
	    else if (d .le. 0.1) then
		vfallstp = 4104.9 * d**1.008
	    else if (d .le. 0.25) then
		vfallstp = 1812.1 * d**0.638
	    else
		vfallstp = 1069.8 * d**0.235
	    end if

	    vfall = vfallstp * sqrt(1.204e-3/rhoair)
	    vfallrainsv(i) = vfall
	    precipsum = precipsum + vfall*(r**3)*xnumrainsv(i)
	end do
	precipsum = precipsum*pi*1.333333

	rnumsum = 0.
	do i = 1, nr
	    xnumrainsv(i) = xnumrainsv(i)*(precip/precipsum)
	    rnumsum = rnumsum + xnumrainsv(i)
	end do

!
!   compute aerosol concentrations
!	aaerosv = particle radius (cm)
!	fnumaerosv = fraction of total number in the bin (--)
!	fvolaerosv = fraction of total volume in the bin (--)
!
	anumsum = 0.
	avolsum = 0.
	do i = 1, na
	    x = xlo + (i-1)*dx
	    a = exp( x )
	    aaerosv(i) = a
	    dum = (x - xg0)/sx
	    ynumaerosv(i) = exp( -0.5*dum*dum )
	    yvolaerosv(i) = ynumaerosv(i)*1.3333*pi*a*a*a
	    anumsum = anumsum + ynumaerosv(i)
	    avolsum = avolsum + yvolaerosv(i)
	end do

	do i = 1, na
	    ynumaerosv(i) = ynumaerosv(i)/anumsum
	    yvolaerosv(i) = yvolaerosv(i)/avolsum
	end do


!
!   compute scavenging
!
	scavsumnum = 0.
	scavsumvol = 0.
!
!   outer loop for rain drop radius
!
	do 5900 jr = 1, nr

	r = rrainsv(jr)
	vfall = vfallrainsv(jr)

	reynolds = r * vfall / airkinvisc
	sqrtreynolds = sqrt( reynolds )

!
!   inner loop for aerosol particle radius
!
	scavsumnumbb = 0.
	scavsumvolbb = 0.

	do 5500 ja = 1, na

	a = aaerosv(ja)

	chi = a/r

	dum = freepath/a
	dumfuchs = 1. + 1.246*dum + 0.42*dum*exp(-0.87/dum)
	taurelax = 2.*rhoaero*a*a*dumfuchs/(9.*rhoair*airkinvisc)

	aeromass = 4.*pi*a*a*a*rhoaero/3.
	aerodiffus = boltzmann*temp*taurelax/aeromass

	schmidt = airkinvisc/aerodiffus
	stokes = vfall*taurelax/r

	ebrown = 4.*(1. + 0.4*sqrtreynolds*(schmidt**0.3333333)) /  &
     			(reynolds*schmidt)

	dum = (1. + 2.*xmuwaterair*chi) /         &
     			(1. + xmuwaterair/sqrtreynolds)
	eintercept = 4.*chi*(chi + dum)

	dum = log( 1. + reynolds )
	sstar = (1.2 + dum/12.) / (1. + dum)
	eimpact = 0.
	if (stokes .gt. sstar) then
	    dum = stokes - sstar
	    eimpact = (dum/(dum+0.6666667)) ** 1.5
	end if

	etotal = ebrown + eintercept + eimpact
	etotal = min( etotal, 1.0_r8 )

	rainsweepout = xnumrainsv(jr)*4.*pi*r*r*vfall

	scavsumnumbb = scavsumnumbb + rainsweepout*etotal*ynumaerosv(ja)
	scavsumvolbb = scavsumvolbb + rainsweepout*etotal*yvolaerosv(ja)

5500	continue

	scavsumnum = scavsumnum + scavsumnumbb
	scavsumvol = scavsumvol + scavsumvolbb
5900	continue

	scavratenum = scavsumnum*3600.
	scavratevol = scavsumvol*3600.


   
  return
  end subroutine calc_1_impact_rate

#endif 
! (defined MODAL_AERO)

#ifndef WRF_PORT
!-------------------------------------------------------------------
!-------------------------------------------------------------------
  !===============================================================================

  function mz_prescribed_dust()

     ! Return true if MOZART is supplying prescribed dust.

     use dust_intr,    only: dust_names
     use tracer_cnst,  only: num_tracer_cnst, tracer_cnst_flds

     logical :: mz_prescribed_dust

     integer :: i
     !-----------------------------------------------------------------------

     mz_prescribed_dust = .false.
     do i = 1, num_tracer_cnst
        if ( trim(tracer_cnst_flds(i)) == dust_names(1) ) then
           mz_prescribed_dust = .true.
           exit
        end if
     end do

  end function mz_prescribed_dust

  !===============================================================================
#endif

end module mz_aerosols_intr
