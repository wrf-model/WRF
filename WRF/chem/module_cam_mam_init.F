! module_cam_mam_init.F
! created by r.c.easter, june 2010
!
! 2010-07-03 notes:
! 1.  In CAM, stratiform-cloudborne aerosol species are held in the qqcw
! array (of physics buffer), interstital aerosol water species are held in
! the qaerwat array (of physics buffer), and other trace species (water vapor,
! "stratiform cloud microphysical", trace gases, interstitial aerosol)
! are held in the state%q array.  Which interfacing to CAM routines,
! species must by transferred to/from the q and qqcw arrays.
!     Initial implementations in WRF-Chem will use CBMZ gas-phase chemistry
! and CAM MAM aerosols, so the trace gases will differ from CAM5.
!     The species in the q array will be
!     a.  Moisture species (pcnst_non_chem_modal_aero of them)
!     b.  Trace gas species with WRF-Chem indices between param_first_scalar
! and numgas.
!     c.  Interstitial aerosol species (except for aerosol water).
!
!--------------------------------------------------------------
#include "MODAL_AERO_CPP_DEFINES.h"

module module_cam_mam_init
  
  private
  public :: cam_mam_init
  
  
  ! in cam5, pom is assumed to by 10/14 carbon and 4/14 other elements (oxygen, etc)
  ! when the following 2 parameters are positive, a factor of 1.4 is applied
  !    in the emissions and IC/BC routines, so that pom in cam5 will be
  !    ~1.4x the equivalent mosaic and sorgam primary organic species
  ! when a flag is zero/negative, the factor is not applied
  ! *** the 2 integer parameters can be modified by user (normally all 0 or all 1),
  !     but the 2 real variables are set later based on the parameter values
  integer, parameter :: pom_emit_1p4_factor_flag = 0
  integer, parameter :: pom_icbc_1p4_factor_flag = 0
  real, public :: pom_emit_1p4_factor
  real, public :: pom_icbc_1p4_factor
  
  ! in 3-mode cam5, so4 is assumed to be ammonium-bisulfate (molec-wght = 115)
  ! when the following 3 parameters are positive, a factor of 115/96 ~= 1.2 is applied
  !    to so4 species in the emissions and IC/BC routines, and the so4 molec-wght
  !    is set to 115 (which affect so2->so4 gas and aqueous oxidation),
  !    so that so4 in cam5 will be ~1.2x the equivalent mosaic and sorgam primary organic species
  ! when a flag is zero/negative, the factor is not applied, or the molec-wght is set to 96
  ! *** the 3 integer parameters can be modified by user (normally all 0 or all 1),
  !     but the 2 real variables are set later based on the parameter values
  integer, parameter :: so4_emit_1p2_factor_flag = 1
  integer, parameter :: so4_icbc_1p2_factor_flag = 1
  integer, parameter :: so4_mwgt_1p2_factor_flag = 1
  real, public :: so4_emit_1p2_factor
  real, public :: so4_icbc_1p2_factor
  
  
  !Balwinder.Singh@pnnl.gov: cnst_name_loc is defined as a local array for this module
  !The cnst_name array of phys/constituent module is populated using 'cnst_add' calls
  !in this module to be consistent with the CAM way of populating cnst_name array
  character(len=16),allocatable, public :: cnst_name_loc(:)     ! constituent names

  integer, parameter :: init_val = -999888777
  LOGICAL :: CAM_INITIALIZED_CHEM = .FALSE.
contains
  
  
  !==============================================================
  subroutine cam_mam_init(           &
       id, numgas, config_flags,       &
       ids,ide, jds,jde, kds,kde,      &
       ims,ime, jms,jme, kms,kme,      &
       its,ite, jts,jte, kts,kte       )
    !--------------------------------------------------------------
    ! purpose:
    ! 1. call routines that allocated and initialize data structures used
    !    by cam modal aerosol (cam_mam) code
    ! 2. call routines that initialize aerosol mixing ratios
    !    for specific test scenarios
    !--------------------------------------------------------------
    
    use module_state_description, only: num_chem,CBMZ_CAM_MAM3_NOAQ,CBMZ_CAM_MAM3_AQ,CBMZ_CAM_MAM7_NOAQ,CBMZ_CAM_MAM7_AQ
    use module_configure, only:  grid_config_rec_type
    
    use shr_kind_mod, only: r8 => shr_kind_r8
    use physconst, only: epsilo, latvap, latice, rh2o, cpair, tmelt
    use module_cam_esinti, only: esinti
    
    use module_cam_support, only: pver, pverp, pcols, &
         pcnst => pcnst_runtime, &
         endrun, masterproc
    
    use modal_aero_data, only:  cnst_name_cw, ntot_aspectype, &
         qneg3_worst_thresh_amode, species_class, &
         specmw_amode, specname_amode, &
         specdens_so4_amode, specmw_so4_amode, &
         specdens_nh4_amode, specmw_nh4_amode, &
         specdens_no3_amode, specmw_no3_amode, &
         specdens_pom_amode, specmw_pom_amode, &
         specdens_soa_amode, specmw_soa_amode, &
         specdens_bc_amode, specmw_bc_amode, &
         specdens_dust_amode, specmw_dust_amode, &
         specdens_seasalt_amode, specmw_seasalt_amode
    
    use modal_aero_initialize_data, only: modal_aero_initialize, modal_aero_register, decouple_mam_mp
    use ndrop , only: activate_init
    USE module_cam_mam_cloudchem, only: cam_mam_cloudchem_inti
    USE module_cam_mam_gas_wetdep_driver, only: cam_mam_gas_wetdep_inti
    
    implicit none
    
    !--------------------------------------------------------------
    ! ... arguments
    !--------------------------------------------------------------
    type(grid_config_rec_type), intent(in) :: config_flags
    
    integer, intent(in) ::   &
         id, numgas,   &
         ids, ide, jds, jde, kds, kde,   &
         ims, ime, jms, jme, kms, kme,   &
         its, ite, jts, jte, kts, kte
    
    
    !--------------------------------------------------------------
    ! ... local variables
    !--------------------------------------------------------------
    integer :: ierr, l, m
    character(len=16)  :: tmpname
    character(len=160) :: msg
    
    
    !Balwinder.Singh@pnnl.gov: Added a sanity check so that chem_opt package corresponds to the right CPP directive for 3 or 7 mode 
#if ( defined MODAL_AERO_3MODE )
    if ( (config_flags%chem_opt /= CBMZ_CAM_MAM3_NOAQ) .and. &
         (config_flags%chem_opt /= CBMZ_CAM_MAM3_AQ  ) ) then
       call wrf_error_fatal( 'cam_mam_init - MODAL_AERO_3MODE is defined but chem_opt is not a CAM_MAM3 package' )
    end if
    
#elif ( defined MODAL_AERO_7MODE )
    if ( (config_flags%chem_opt /= CBMZ_CAM_MAM7_NOAQ) .and. &
         (config_flags%chem_opt /= CBMZ_CAM_MAM7_AQ  ) ) then
       call wrf_error_fatal( 'cam_mam_init - MODAL_AERO_7MODE is defined but chem_opt is not a CAM_MAM7 package' )
    end if
    
#else
    call wrf_error_fatal( 'cam_mam_init - neither MODAL_AERO_3MODE or MODAL_AERO_7MODE is defined' )
    
#endif
    
    !Balwinder.Singh@pnnl.gov: Sanity check for cam_mam_mode variable in namelist
    if((config_flags%chem_opt == CBMZ_CAM_MAM3_NOAQ .OR. config_flags%chem_opt == CBMZ_CAM_MAM3_AQ) .AND.  config_flags%cam_mam_mode .NE. 3)then
       call wrf_error_fatal( 'CAM_MAM_INIT - For MODAL_AERO_3MODE (chem_opt - 503 CAM_MAM3 package), cam_mam_mode in namelist should be set to 3' )
    elseif((config_flags%chem_opt == CBMZ_CAM_MAM7_NOAQ .OR. config_flags%chem_opt == CBMZ_CAM_MAM7_AQ) .AND.  config_flags%cam_mam_mode .NE. 7)then
       call wrf_error_fatal('CAM_MAM_INIT - For MODAL_AERO_7MODE (chem_opt - 504 CAM_MAM7 package), cam_mam_mode in namelist should be set to 7')
    endif

    !Balwinder.Singh@pnnl.gov: Sanity check for cam_mam_nspec variable in namelist
    !BSINGH (01/23/2014):Please make sure cam_mam_nspec is equal to pcnst in phys/module_physics_init.F and registry.chem
    if((config_flags%chem_opt == CBMZ_CAM_MAM3_NOAQ .OR. config_flags%chem_opt == CBMZ_CAM_MAM3_AQ) .AND.  config_flags%cam_mam_nspec .NE. 85)then
       call wrf_error_fatal( 'CAM_MAM_INIT - For MODAL_AERO_3MODE (chem_opt - 503 CAM_MAM3 package), cam_mam_nspec in namelist should be set to 85' )
    elseif((config_flags%chem_opt == CBMZ_CAM_MAM7_NOAQ .OR. config_flags%chem_opt == CBMZ_CAM_MAM7_AQ) .AND.  config_flags%cam_mam_nspec .NE. 90)then
       !BSINGH (01/23/2014): DMS species are NOT included in MAM7 package.
       call wrf_error_fatal('CAM_MAM_INIT - For MODAL_AERO_7MODE (chem_opt - 504 CAM_MAM7 package), cam_mam_nspec in namelist should be set to 90')
    endif
    
    !--------------------------------------------------------------
    ! ... executable
    !--------------------------------------------------------------
    write(*,'(/a)') 'cam_mam_init'
    write(*,*) 'id, num_chem, pcnst =', id, num_chem, pcnst
    
    write(*,'(a,3(4x,2i5))') 'ids/e, j..., k... ', ids,ide, jds,jde, kds,kde
    write(*,'(a,3(4x,2i5))') 'ims/e, j..., k... ', ims,ime, jms,jme, kms,kme
    write(*,'(a,3(4x,2i5))') 'its/e, j..., k... ', its,ite, jts,jte, kts,kte
    write(*,'(a,3(   i14))') 'pver, pverp, pcols', pver, pverp, pcols
    
    
    ! initialize water vapor saturation routines
    call esinti(epsilo, latvap, latice, rh2o, cpair, tmelt)
    
    
    ! set pver and pverp
    if ( (max(pver,pverp) > 0) .and. &
         (pver /= kte-kts+1) .and. &
         (pverp /= pver+1) ) then
       write( msg, '(2a,3i15)' ) &
            'cam_mam_init fatal error ', &
            '- bad pver - id, pver, pverp = ', id, pver, pverp
       call wrf_error_fatal( msg )
    end if
    if (pver <= 0) then
       pver = kde - kds
       pverp = pver + 1
    end if
    write(*,'(a,3(   i14))') 'pver, pverp, pcols', pver, pverp, pcols
    
    
    ! set pcnst and cnst_name_loc
    write(*,'(/a)') &
         'cam_mam_init calling cam_mam_init_set_cnst'
    call cam_mam_init_set_cnst( id, numgas, config_flags )
    
    !Balwinder.Singh@pnnl.gov: Before calling 'modal_aero_initialize', we have to
    !call 'modal_aero_register'[nuance of CAM5.1]
    write(*,'(/a)') &
         'cam_mam_init calling modal_aero_register'
    call modal_aero_register
    
    ! do modal_aero_initialize_data
    write(*,'(/a)') &
         'cam_mam_init calling modal_aero_initialize'
    call modal_aero_initialize

    !For assisting decoupled microphysics (MP) CAM MAM simulations (simulations, where MAM package is coupled with 
    !radiation but decoupled with MP - i.e. MP runs with 'prescribed' aerosols)
    call decouple_mam_mp(config_flags%CAM_MP_MAM_cpled)

    !Balwinder.Singh@pnnl.gov: initialize aerosol activation
    call activate_init
    
#if ( defined MODAL_AERO_3MODE )
    if (so4_mwgt_1p2_factor_flag <= 0) then
       ! in this case, the so4 molec-wght will be 96 instead of 115,
       ! so do the following to override what was done in modal_aero_initialize
       do m = 1, ntot_aspectype
          if      (specname_amode(m).eq.'sulfate   ') then
             !specmw_amode(m) = 96.0_r8 !Balwinder.Singh@pnnl.gov: defined as a 'parameter' in module_data_cam_mam_aero.F
             specmw_so4_amode = specmw_amode(m)
          else if (specname_amode(m).eq.'ammonium  ') then
             !specmw_amode(m) = 18.0_r8 !Balwinder.Singh@pnnl.gov: defined as a 'parameter' in module_data_cam_mam_aero.F
             specmw_nh4_amode = specmw_amode(m)
          end if
       end do
    end if
#endif
    
    write(*,'(a,2f12.4)') 'so4 dens, mw', specdens_so4_amode, specmw_so4_amode
    write(*,'(a,2f12.4)') 'nh4 dens, mw', specdens_nh4_amode, specmw_nh4_amode
    write(*,'(a,2f12.4)') 'no3 dens, mw', specdens_no3_amode, specmw_no3_amode
    write(*,'(a,2f12.4)') 'pom dens, mw', specdens_pom_amode, specmw_pom_amode
    write(*,'(a,2f12.4)') 'soa dens, mw', specdens_soa_amode, specmw_soa_amode
    write(*,'(a,2f12.4)') 'bc  dens, mw', specdens_bc_amode, specmw_bc_amode
    write(*,'(a,2f12.4)') 'dst dens, mw', specdens_dust_amode, specmw_dust_amode
    write(*,'(a,2f12.4)') 'ncl dens, mw', specdens_seasalt_amode, specmw_seasalt_amode
    
    
    ! set variables that contain the pom 1.4 or 1.0 factors 
    ! and the so4 1.2 or 1.0 factors used with emissions and IC/BC
    pom_emit_1p4_factor = 1.0
    pom_icbc_1p4_factor = 1.0
    if (pom_emit_1p4_factor_flag > 0) pom_emit_1p4_factor = 1.4
    if (pom_icbc_1p4_factor_flag > 0) pom_icbc_1p4_factor = 1.4
    
    so4_emit_1p2_factor = 1.0
    so4_icbc_1p2_factor = 1.0
#if ( defined MODAL_AERO_3MODE )
    if (so4_emit_1p2_factor_flag > 0) so4_emit_1p2_factor = 115.0/96.0
    if (so4_icbc_1p2_factor_flag > 0) so4_icbc_1p2_factor = 115.0/96.0
#endif
    
    write(*,'(/a,2f10.4)') 'pom_emit_1p4_factor & _init_', &
         pom_emit_1p4_factor, pom_icbc_1p4_factor
    write(*,'( a,2f10.4)') 'so4_emit_1p2_factor & _init_', &
         so4_emit_1p2_factor, so4_icbc_1p2_factor
    
    
    ! initialize the module_data_cam_mam_asect data
    write(*,'(/a)') &
         'cam_mam_init calling cam_mam_init_asect'
    call cam_mam_init_asect( id, config_flags )
    
    
    ! allocate and initialize arrays used to map aerosol and trace gas species
    ! between the wrf-chem "chem" array and the cam "q" array
    write(*,'(/a)') &
         'cam_mam_init calling cam_mam_init_other'
    call cam_mam_init_other( id, numgas, config_flags )

    !Initialize CAM Cloud Chemistry
    call cam_mam_cloudchem_inti()

    !Initialize CAM gas wetdep
    call cam_mam_gas_wetdep_inti

    deallocate(cnst_name_loc)  
    ! done
    write(*,'(/a)') &
         'cam_mam_init done'
    
    
    return
  end subroutine cam_mam_init
  
  
  !==============================================================
  subroutine cam_mam_init_set_cnst( id, numgas, config_flags )
    !--------------------------------------------------------------
    ! purpose:
    ! 1. set the value of the pcnst variable
    !    (which currently is pcnst_runtime in module_cam_support),
    !    and the pcnst_non_chem variable too.
    ! 2. load the cnst_name array of constituent module using 'cnst_add' calls
    !    and load it with appropriate trace species names
    !
    ! Modified by Balwinder.Singh@pnnl.gov: pcnst is set up at runtime
    !                                       now. This subroutine serves
    !                                       as a test to check whether
    !                                       the value of pcnst is set
    !                                       approprately in 
    !                                       module_physics_init.F
    !*NOTE*: As a quick solution, correct value for the Molecular weights 
    !        of the constituents is updated in 'cam_mam_init_other'. In 
    !        the current subroutine, molecular weights are set to 1. 
    !
    !*WARNING*: Minimum threshold for the constituents is set to ZERO here.
    !           This value is updated in  'cam_mam_init_other' subroutine
    !--------------------------------------------------------------
    
    use module_configure, only:  grid_config_rec_type
    use module_state_description, only:  num_chem, param_first_scalar
    use module_scalar_tables, only:  chem_dname_table
    
    use module_cam_support,    only: pcnst => pcnst_runtime, &
         pcnst_non_chem => pcnst_non_chem_modal_aero, &
         gas_pcnst => gas_pcnst_modal_aero, &
         endrun, masterproc
    use modal_aero_data
    use constituents,             only: cnst_add
    use physconst,                only: cpair
    
    implicit none
    
    !--------------------------------------------------------------
    ! ... arguments
    !--------------------------------------------------------------
    type(grid_config_rec_type), intent(in) :: config_flags
    integer, intent(in) :: id   ! domain index
    integer, intent(in) :: numgas
    
    !--------------------------------------------------------------
    ! ... local variables
    !--------------------------------------------------------------
    integer :: ierr, itmpa, dumind
    integer :: l, l2
    integer :: p1st
    character(len=360) :: msg
    
    !--------------------------------------------------------------
    ! ... executable
    !--------------------------------------------------------------
    write(*,*) 'cam_mam_init_set_cnst'
    write(*,*) 'id, num_chem         =', id, num_chem
    write(*,*) 'pcnst, gas_pcnst old =', pcnst, gas_pcnst
    
    ! set pcnst value (and pcnst_non_chem too)
    p1st = param_first_scalar
    pcnst_non_chem = 5
    
    ! start with the non-chemistry trace species
    itmpa = pcnst_non_chem
    ! add on the trace gas species
    do l = p1st, numgas
       itmpa = itmpa + 1
    end do
    ! add on the interstitial aerosol apecies
    ! (except aerosol water)
    ! *** their species names are assumed to end
    !     with "_aN", where N=1,2,...,9 ***
    do l = numgas+1, num_chem
       if ( cam_mam_is_q_aerosol_species( l, numgas ) ) then
          itmpa = itmpa + 1
       end if
    end do
    
    if (pcnst == itmpa) then !Balwinder.Singh@pnnl.gov: check if pcnst has the correct value  
       !pcnst = itmpa 
       gas_pcnst = pcnst - pcnst_non_chem
       write(*,*) 'pcnst, gas_pcnst new =', pcnst, gas_pcnst
    else if (pcnst /= itmpa) then
       write( msg, * ) &
            'CAM_MAM_INIT_SET_CNST fatal error: The value of PCNST should be set to:',itmpa, &
            ' in module_physics_init.F where pcnst is mentioned as', pcnst,'. ID is',id
       call wrf_error_fatal( msg )
    end if
    
    ! allocate cnst_name_loc
    if ( .not. allocated(cnst_name_loc) ) then
       allocate( cnst_name_loc(pcnst) )
    end if
    
    
    ! set cnst_name_loc values
    do l = 1, pcnst
       write( cnst_name_loc(l), '(a,i4.4)' ) 'empty_cnst_', l
    end do
    !First 5 constituents are already added in the phys/module_physics_init module.
    !Therefore cnst_add calls are not required for the same
    if (pcnst_non_chem >= 1) cnst_name_loc(1) = 'Q'
    if (pcnst_non_chem >= 2) cnst_name_loc(2) = 'CLDLIQ'
    if (pcnst_non_chem >= 3) cnst_name_loc(3) = 'CLDICE'
    if (pcnst_non_chem >= 4) cnst_name_loc(4) = 'NUMLIQ'
    if (pcnst_non_chem >= 5) cnst_name_loc(5) = 'NUMICE'
    
    l2 = pcnst_non_chem
    do l = p1st, numgas
       l2 = l2 + 1
       IF(.NOT.CAM_INITIALIZED_CHEM) call cnst_add(trim(adjustl(chem_dname_table(1,l))), 1.0_r8, cpair, 0._r8, dumind)   
       cnst_name_loc(l2) = chem_dname_table(1,l)
    end do
    
    do l = numgas+1, num_chem
       if ( cam_mam_is_q_aerosol_species( l, numgas ) ) then
          l2 = l2 + 1
          IF(.NOT.CAM_INITIALIZED_CHEM) call cnst_add(trim(adjustl(chem_dname_table(1,l))), 1.0_r8, cpair, 0._r8, dumind)      
          cnst_name_loc(l2) = chem_dname_table(1,l)
       end if
    end do
    if (l2 /= pcnst) then
       write( msg, '(2a,3i15)' ) &
            'cam_mam_init_set_cnst fatal error 101', &
            'for cnst_name, id, l2, pcnst = ', id, l2, pcnst
       call wrf_error_fatal( msg )
    end if
    
    
    return
  end subroutine cam_mam_init_set_cnst
  
  
  !==============================================================
  function cam_mam_is_q_aerosol_species( lspec, numgas )
    !
    ! returns true if the wrf-chem species with index lspec is an aerosol species 
    !     that belongs in the q array, otherwise returns false.
    ! the q array aerosol species are interstitial aerosol species
    !     other than aerosol water
    ! *** currently it is assumed that for these species, the
    !     wrf-chem species name ends with "_aN", where N=1,2,...,7
    !
    use module_state_description, only:  num_chem, param_first_scalar
    use module_scalar_tables, only:  chem_dname_table
    use modal_aero_data, only:  ntot_amode
    
    implicit none
    
    logical ::  cam_mam_is_q_aerosol_species
    integer, intent(in) :: lspec, numgas
    
    integer :: i, n
    integer, parameter :: upper_to_lower = iachar('a')-iachar('A')
    character(len=32) :: tmpname
    character(len=1)  :: tmpch
    
    cam_mam_is_q_aerosol_species = .false.
    if ( (lspec <  param_first_scalar) .or. &
         (lspec <= numgas) .or. &
         (lspec >  num_chem) ) return
    
    call lower_case( chem_dname_table(1,lspec), tmpname )
    
    ! aerosol water species are not in the q array
    if (tmpname(1:5) == 'wtr_a') return
    
    n = len( trim(tmpname) )
    n = max( n, 4 )
    if (tmpname(n-2:n) == '_a1') cam_mam_is_q_aerosol_species = .true.
    if (tmpname(n-2:n) == '_a2') cam_mam_is_q_aerosol_species = .true.
    if (tmpname(n-2:n) == '_a3') cam_mam_is_q_aerosol_species = .true.
    if (ntot_amode == 3) return
    
    if (tmpname(n-2:n) == '_a4') cam_mam_is_q_aerosol_species = .true.
    if (tmpname(n-2:n) == '_a5') cam_mam_is_q_aerosol_species = .true.
    if (tmpname(n-2:n) == '_a6') cam_mam_is_q_aerosol_species = .true.
    if (tmpname(n-2:n) == '_a7') cam_mam_is_q_aerosol_species = .true.
    if (ntot_amode == 7) return
    
    return
  end function cam_mam_is_q_aerosol_species
  
  
  !==============================================================
  subroutine lower_case( txt_in, txt_lc )
    !
    ! converts a character string (txt_in) to lowercase (txt_lc)
    !
    implicit none
    
    character(len=*), intent(in)  :: txt_in
    character(len=*), intent(out) :: txt_lc
    
    integer :: i, j
    integer, parameter :: iachar_lowera = iachar('a')
    integer, parameter :: iachar_uppera = iachar('A')
    integer, parameter :: iachar_upperz = iachar('Z')
    
    txt_lc = txt_in
    do i = 1, len( trim(txt_lc) )
       j = iachar( txt_lc(i:i) )
       if (j < iachar_uppera) cycle
       if (j > iachar_upperz) cycle
       txt_lc(i:i) = achar( j + iachar_lowera - iachar_uppera )
    end do
    
    return
  end subroutine lower_case
  
  
  !==============================================================
  subroutine cam_mam_init_asect( id, config_flags )
    !--------------------------------------------------------------
    ! purpose:
    ! initialize the aerosol pointer and property (density, molec wght,
    !    hygroscopicity) variables in module_data_cam_mam_asect
    !    from information in module_data_cam_mam_aero
    ! the module_data_cam_mam_aero variables are an "older version"
    !    designed for the mirage code, and are used by cam routines
    ! the module_data_cam_mam_asect variables are a "newer version"
    !    designed for the wrf-chem code, and they are useful for
    !    the wrf-chem-->cam interface/driver routines
    !--------------------------------------------------------------
    
    use module_state_description, only: num_chem,param_first_scalar,CBMZ_CAM_MAM3_AQ,CBMZ_CAM_MAM7_AQ

    use module_configure, only:  grid_config_rec_type
    use module_scalar_tables, only:     chem_dname_table
    
    use modal_aero_data, only:  &
         dgnum_amode, lspectype_amode, nspec_amode, ntot_amode, ntot_aspectype, &
         sigmag_amode, specdens_amode, spechygro, specmw_amode, specname_amode
    
    use module_data_cam_mam_asect
    
    
    implicit none
    
    ! arguments
    integer, intent(in) :: id
    type(grid_config_rec_type), intent(in) :: config_flags
    
    ! local variables
    integer :: iphase, isize, itype
    integer :: l, l2, l3, l4, la, lc
    integer :: p1st
    
    real, parameter :: pi=3.1415926536
    real :: dp_meanvol_tmp
    
    character(len=160) :: msg
    character(len=32)  :: tmpname, tmpnamec, tmptxtaa
    
    
    p1st = param_first_scalar
    
    ! set master component information
    ntot_mastercomp_aer = ntot_aspectype
    do l = 1, ntot_mastercomp_aer
       name_mastercomp_aer(l) = specname_amode(l)
       ! densities in modal_aero_data are kg/m3
       ! densities in module_data_cam_mam_asect are g/cm3 (for consistency with mosaic and sorgam)
       dens_mastercomp_aer(l) = specdens_amode(l)*1.0e-3
       mw_mastercomp_aer(l) = specmw_amode(l)
       hygro_mastercomp_aer(l) = spechygro(l)
       if (name_mastercomp_aer(l) == 'sulfate') then
          mastercompindx_so4_aer = l
          namebb_mastercomp_aer(l) = 'so4'
          mw_so4_aer = mw_mastercomp_aer(l)
          dens_so4_aer = dens_mastercomp_aer(l)
       else if (name_mastercomp_aer(l) == 'ammonium') then
          mastercompindx_nh4_aer = l
          namebb_mastercomp_aer(l) = 'nh4'
          mw_nh4_aer = mw_mastercomp_aer(l)
          dens_nh4_aer = dens_mastercomp_aer(l)
       else if (name_mastercomp_aer(l) == 'nitrate') then
          mastercompindx_no3_aer = l
          namebb_mastercomp_aer(l) = 'no3'
          mw_no3_aer = mw_mastercomp_aer(l)
          dens_no3_aer = dens_mastercomp_aer(l)
       else if (name_mastercomp_aer(l) == 'p-organic') then
          mastercompindx_pom_aer = l
          namebb_mastercomp_aer(l) = 'pom'
          mw_pom_aer = mw_mastercomp_aer(l)
          dens_pom_aer = dens_mastercomp_aer(l)
       else if (name_mastercomp_aer(l) == 's-organic') then
          mastercompindx_soa_aer = l
          namebb_mastercomp_aer(l) = 'soa'
          mw_soa_aer = mw_mastercomp_aer(l)
          dens_soa_aer = dens_mastercomp_aer(l)
       else if (name_mastercomp_aer(l) == 'black-c') then
          mastercompindx_bc_aer = l
          namebb_mastercomp_aer(l) = 'bc'
          mw_bc_aer = mw_mastercomp_aer(l)
          dens_bc_aer = dens_mastercomp_aer(l)
       else if (name_mastercomp_aer(l) == 'seasalt') then
          mastercompindx_seas_aer = l
          namebb_mastercomp_aer(l) = 'ncl'
          mw_seas_aer = mw_mastercomp_aer(l)
          dens_seas_aer = dens_mastercomp_aer(l)
       else if (name_mastercomp_aer(l) == 'dust') then
          mastercompindx_dust_aer = l
          namebb_mastercomp_aer(l) = 'dst'
          mw_dust_aer = mw_mastercomp_aer(l)
          dens_dust_aer = dens_mastercomp_aer(l)
       else
          msg = '*** cam_mam_init_asect error 100 - mastercompindx'
          call wrf_message( msg )
          write( msg, '(a,i4,2x,a)' ) 'l, specname_amode = ', &
               l, specname_amode(l)
          call wrf_error_fatal( msg )
       end if
    end do ! l
    
    
    ! set number of phases
    nphase_aer = 1
    ai_phase = 1
!         if (config_flags%chem_opt == ...) then
          if ((config_flags%chem_opt == CBMZ_CAM_MAM3_AQ) .or. &
              (config_flags%chem_opt == CBMZ_CAM_MAM7_AQ  )) then
               nphase_aer = 2
               cw_phase = 2
         end if
    
    if (nphase_aer > 2) then
       msg = '*** cam_mam_init_asect error 120 - nphase_aer > 2'
       call wrf_error_fatal( msg )
    end if
    
    
    nsize_aer(:) = 0
    ncomp_aer(:) = 0
    ncomp_plustracer_aer(:) = 0
    mastercompptr_aer(:,:)  = init_val
    massptr_aer(:,:,:,:)    = init_val
    waterptr_aer(:,:)       = init_val
    hyswptr_aer(:,:)        = init_val
    numptr_aer(:,:,:)       = init_val
    mprognum_aer(:,:,:)     = 0
    
    lptr_so4_aer(:,:,:)  = init_val
    lptr_nh4_aer(:,:,:)  = init_val
    lptr_no3_aer(:,:,:)  = init_val
    lptr_pom_aer(:,:,:)  = init_val
    lptr_soa_aer(:,:,:)  = init_val
    lptr_bc_aer(:,:,:)   = init_val
    lptr_dust_aer(:,:,:) = init_val
    lptr_seas_aer(:,:,:) = init_val
    
    volumcen_sect(:,:) = 0.0
    volumlo_sect(:,:) = 0.0
    volumhi_sect(:,:) = 0.0
    dcen_sect(:,:) = 0.0
    dlo_sect(:,:) = 0.0
    dhi_sect(:,:) = 0.0
    sigmag_aer(:,:) = 1.0
    
    ! set mode information
    !
    ! each cam_mam mode corresponds to a wrfchem type,
    ! and each wrfchm type has a single size bin
    ! (this differs from sorgam, where the aitken and accum. modes
    !  have the same species, and so can both have the same type)
    !
    ntype_aer = ntot_amode
    
    do itype = 1, ntype_aer
       
       nsize_aer(itype) = 1
       ncomp_aer(itype) = nspec_amode(itype)
       ncomp_plustracer_aer(itype) = ncomp_aer(itype)
       
       ! for sectional
       !    the dhi/dlo_sect are the upper/lower bounds for 
       !       mean-volume diameter for a section/bin
       ! for modal
       !    they should be set to reasonable upper/lower
       !       bounds for mean-volume diameters of each modes
       !    they are primarily used to put reasonable bounds
       !       on number (in relation to mass/volume)
       !    the dcen_sect are used by initwet for the impaction scavenging
       !       lookup tables, and should represent a "base" mean-volume diameter
       ! dp_meanvol_tmp (below) is the cam-mam default value 
       !    for mean-volume diameter (in cm)
       ! terminology:  (pi/6) * (mean-volume diameter)**3 ==
       !	(volume mixing ratio of section/mode)/(number mixing ratio)
       isize = 1
       sigmag_aer(isize,itype) = sigmag_amode(itype)
       dp_meanvol_tmp = 1.0e2*dgnum_amode(itype) &   ! 1.0e2 converts m to cm
            *exp( 1.5 * log(sigmag_aer(isize,itype))**2 )
       dcen_sect(isize,itype) = dp_meanvol_tmp
       dhi_sect( isize,itype) = dp_meanvol_tmp*4.0
       dlo_sect( isize,itype) = dp_meanvol_tmp/4.0
       
       do isize = 1, nsize_aer(itype)
          volumcen_sect(isize,itype) = (pi/6.0)*(dcen_sect(isize,itype)**3)
          volumlo_sect( isize,itype) = (pi/6.0)*(dlo_sect( isize,itype)**3)
          volumhi_sect( isize,itype) = (pi/6.0)*(dhi_sect( isize,itype)**3)
       end do
       write(*,'(a,i3,1p,5e11.3)') 'type, sg, dg, dp, vol', itype, &
            sigmag_aer(1,itype), dgnum_amode(itype), dp_meanvol_tmp, volumcen_sect(1,itype)
       
       
       do l = 1, ncomp_aer(itype)
          l2 = lspectype_amode(l,itype)
          if ((l2 > 0) .and. (l2 <= ntot_mastercomp_aer)) then
             mastercompptr_aer(l,itype) = l2
          else
             msg = '*** cam_mam_init_asect error 200 - mastercompptr'
             call wrf_message( msg )
             write( msg, '(a,4(1x,i10))' ) &
                  'itype, l, l2, ntot_mastercomp_aer = ', &
                  itype, l, l2, ntot_mastercomp_aer
             call wrf_error_fatal( msg )
          end if
          dens_aer(l,itype) = dens_mastercomp_aer(l2)
          mw_aer(l,itype) = mw_mastercomp_aer(l2)
          hygro_aer(l,itype) = hygro_mastercomp_aer(l2)
          name_aer(l,itype) = name_mastercomp_aer(l2)
       end do
       
       isize = 1
       do l = -1, ncomp_aer(itype)
          do iphase = 1, nphase_aer
             if (l == -1) then
                tmpname = 'num'
             else if (l == 0) then
                tmpname = 'wtr'
                if (iphase > 1) cycle
             else
                l2 = lspectype_amode(l,itype)
                tmpname = namebb_mastercomp_aer(l2)
             end if
             
             if (iphase == 1) then
                tmpname = trim(tmpname) // '_a'
             else
                tmpname = trim(tmpname) // '_c'
             end if
             write( tmptxtaa, '(i1)' ) itype
             tmpname = trim(tmpname) // tmptxtaa(1:1)
             
             l3 = 0
             do l4 = p1st, num_chem
                if (chem_dname_table(1,l4) == tmpname) then
                   l3 = l4
                   exit
                end if
             end do
             if (l3 <= 0) then
                msg = '*** cam_mam_init_asect error 300' // &
                     ' - finding species - ' // tmpname
                call wrf_error_fatal( msg )
             end if
             
             if (l == -1) then
                numptr_aer(isize,itype,iphase) = l3
                mprognum_aer(isize,itype,iphase) = 1
             else if (l == 0) then
                waterptr_aer(isize,itype) = l3
             else
                massptr_aer(l,isize,itype,iphase) = l3
                mastercompptr_aer(l,itype) = l2
                if (l2 == mastercompindx_so4_aer) then
                   lptr_so4_aer(isize,itype,iphase) = l3
                else if (l2 == mastercompindx_nh4_aer) then
                   lptr_nh4_aer(isize,itype,iphase) = l3
                   !                 else if (l2 == mastercompindx_no3_aer) then
                   !                    lptr_no3_aer(isize,itype,iphase) = l3
                else if (l2 == mastercompindx_pom_aer) then
                   lptr_pom_aer(isize,itype,iphase) = l3
                else if (l2 == mastercompindx_soa_aer) then
                   lptr_soa_aer(isize,itype,iphase) = l3
                else if (l2 == mastercompindx_bc_aer) then
                   lptr_bc_aer(isize,itype,iphase) = l3
                else if (l2 == mastercompindx_dust_aer) then
                   lptr_dust_aer(isize,itype,iphase) = l3
                else if (l2 == mastercompindx_seas_aer) then
                   lptr_seas_aer(isize,itype,iphase) = l3
                else
                   msg = '*** cam_mam_init_asect error 400' // &
                        ' - finding species type - ' // tmpname
                   call wrf_error_fatal( msg )
                end if
             end if
             
          end do ! iphase
          
       end do ! l
       
    end do ! itype
    
    
    ! diagnostics
    write(*,'(/a)') 'cam_mam_init_asect diagnostics'
    
    write(*,'(/a,i5)') 'ntot_mastercomp_aer', ntot_mastercomp_aer
    write(*,'(a)') 'mastercomp name, l, mw, dens, hygro'
    do l = 1, ntot_mastercomp_aer
       write(*,'(a,i5,1p,3e12.4)') name_mastercomp_aer(l), l, &
            mw_mastercomp_aer(l), dens_mastercomp_aer(l), hygro_mastercomp_aer(l) 
    end do
    write(*,'(a)') &
         'mastercompindx_so4_aer, nh4, no3, pom, soa, bc, seas, dust'
    write(*,'(4i12)') &
         mastercompindx_so4_aer, mastercompindx_nh4_aer, mastercompindx_no3_aer, &
         mastercompindx_pom_aer, mastercompindx_soa_aer, mastercompindx_bc_aer, &
         mastercompindx_seas_aer, mastercompindx_dust_aer
    write(*,'(a)') '........... mw_so4_aer, nh4, no3, pom, soa, bc, seas, dust'
    write(*,'(1p,4e12.4)') &
         mw_so4_aer, mw_nh4_aer, mw_no3_aer, &
         mw_pom_aer, mw_soa_aer, mw_bc_aer, &
         mw_seas_aer, mw_dust_aer
    write(*,'(a)') '......... dens_so4_aer, nh4, no3, pom, soa, bc, seas, dust'
    write(*,'(1p,4e12.4)') &
         dens_so4_aer, dens_nh4_aer, dens_no3_aer, &
         dens_pom_aer, dens_soa_aer, dens_bc_aer, &
         dens_seas_aer, dens_dust_aer
    
    write(*,'(/a/6i12)') 'nphase_aer, ai_phase, cw_phase', &
         nphase_aer, ai_phase, cw_phase
    
    do itype = 1, ntype_aer
       do isize = 1, nsize_aer(itype)
          write(*,'(/a,2i5,a)') 'info for itype, isize = ', itype, isize, &
               'species;  id, name for ai & cw;  mw, dens, hygro'
          
          la = numptr_aer(isize,itype,1)
          lc = numptr_aer(isize,itype,2)
          tmpname = '---'
          if ((la >= p1st) .and. (la <= num_chem)) tmpname = chem_dname_table(1,la)
          tmpnamec = '---'
          if ((lc >= p1st) .and. (lc <= num_chem)) tmpnamec = chem_dname_table(1,la)
          write(*,'(a,i12,1x,a,i12,1x,a,1p,3e12.4)') 'number    ', &
               la, tmpname(1:10), lc, tmpnamec(1:10)
          
          do l = 1, ncomp_aer(itype)
             la = massptr_aer(l,isize,itype,1)
             lc = massptr_aer(l,isize,itype,2)
             tmpname = '---'
             if ((la >= p1st) .and. (la <= num_chem)) tmpname = chem_dname_table(1,la)
             tmpnamec = '---'
             if ((lc >= p1st) .and. (lc <= num_chem)) tmpnamec = chem_dname_table(1,la)
             write(*,'(a,i12,1x,a,i12,1x,a,1p,3e12.4)') name_aer(l,itype), &
                  la, tmpname(1:10), lc, tmpnamec(1:10), &
                  mw_aer(l,itype), dens_aer(l,itype), hygro_aer(l,itype) 
          end do ! l
          
          la = waterptr_aer(isize,itype)
          tmpname = '---'
          if ((la >= p1st) .and. (la <= num_chem)) tmpname = chem_dname_table(1,la)
          write(*,'(a,i12,1x,a,23x,1p,3e12.4)') 'water     ', &
               la, tmpname(1:10)
          
          la = hyswptr_aer(isize,itype)
          tmpname = '---'
          if ((la >= p1st) .and. (la <= num_chem)) tmpname = chem_dname_table(1,la)
          write(*,'(a,i12,1x,a,23x,1p,3e12.4)') 'hys-water ', &
               la, tmpname(1:10)
          
       end do ! isize
    end do ! itype
    
    
    return
  end subroutine cam_mam_init_asect
  
  
  
  !==============================================================
  subroutine cam_mam_init_other( id, numgas, config_flags )
    !--------------------------------------------------------------
    ! purpose:
    ! allocate and initialize variables in module_data_cam_mam_asect
    !    that are used for mapping trace species mixing ratios from
    !    wrf-chem arrays (e.g., "chem") to cam arrays (q, qqcw, qaerwat)
    !--------------------------------------------------------------
    use shr_kind_mod,             only: r8 => shr_kind_r8
    use module_state_description, only: num_chem, param_first_scalar
    use module_configure,         only: grid_config_rec_type, &
         p_h2o2, p_hno3, p_nh3, p_o3, p_so2, p_soag, p_sulf
    use module_scalar_tables,     only: chem_dname_table
    
    use module_cam_support,       only: pcnst => pcnst_runtime, &
         pcnst_non_chem => pcnst_non_chem_modal_aero, &
         gas_pcnst => gas_pcnst_modal_aero
    use modal_aero_data,          only: cnst_name_cw
    use module_data_cam_mam_asect
    use constituents,             only: cnst_mw, cnst_rgas, cnst_cv, cnst_cp, &
         qmin,qmincg
    use physconst,                only: r_universal
    use infnan,                   only: nan

    !Balwinder.Singh@pnnl.gov:*NOTE* cnst_cv and cnst_cp are set to 'nan' 
    !as the cpair value for the chemical species is NOT set correctly. 
    !The 'nan' assigment will produce an error whenever cnst_cv or 
    !cnst_cp is used for any computation
    
    
    implicit none
    
    ! arguments
    integer, intent(in) :: id, numgas
    type(grid_config_rec_type), intent(in) :: config_flags
    
    ! local variables
    integer :: iphase, isize, itype, dumind
    integer :: l, ll, l2, l3, l4
    integer :: p1st
    
    character(len=160) :: msg
    character(len=16)  :: tmpname, tmpname2, tmpname3
    real(r8)           :: qmin_gas, qmin_aer,qmin_num

    IF(.NOT.CAM_INITIALIZED_CHEM) THEN
       qmin_gas = 1.0E-17_r8 !Typical val(H2SO4)           : 1E-12 kg/kg; Negligible val: 1E-17 kg/kg - Balwinder.Singh@pnnl.gov
       qmin_aer = 1.0E-14_r8 !Typical val(accum or coarse) : 1E-9  kg/kg; Negligible val: 1E-14 kg/kg - Balwinder.Singh@pnnl.gov
       qmin_num = 1.0E+01_r8 !Typical val(coarse)          : 1E5    #/kg; Negligible val: 1E1    #/kg - Balwinder.Singh@pnnl.gov
    ENDIF
    
    p1st = param_first_scalar
    
    ! allocate lptr_chem_to_..., factconv_chem_to_..., and mw_... arrays
    if ( .not. allocated(lptr_chem_to_q) ) then
       allocate( lptr_chem_to_q(num_chem) )
    end if
    
    if ( .not. allocated(lptr_chem_to_qqcw) ) then
       allocate( lptr_chem_to_qqcw(num_chem) )
    end if
    
    if ( .not. allocated(factconv_chem_to_q) ) then
       allocate( factconv_chem_to_q(num_chem) )
    end if
    
    if ( .not. allocated(factconv_chem_to_qqcw) ) then
       allocate( factconv_chem_to_qqcw(num_chem) )
    end if
    
    if ( .not. allocated(mw_chem_array) ) then
       allocate( mw_chem_array(num_chem) )
    end if
    
    if ( .not. allocated(mw_q_array) ) then
       allocate( mw_q_array(pcnst) )
    end if
    
    if ( .not. allocated(mw_q_mo_array) ) then
       allocate( mw_q_mo_array(gas_pcnst) )
    end if
    
    
    ! set values of lptr_chem_to_... arrays
      lptr_chem_to_q(:) = init_val
      lptr_chem_to_qqcw(:) = init_val

      do l = p1st, num_chem
         tmpname = chem_dname_table(1,l)
         do l2 = 1, pcnst
            if (tmpname == cnst_name_loc(l2)) then
               lptr_chem_to_q(l) = l2
               exit
            end if
            if (tmpname == cnst_name_cw(l2)) then
               lptr_chem_to_qqcw(l) = l2
               exit
            end if
         end do
      end do


! set values of factconv_chem_to_..., and mw_... arrays
      factconv_chem_to_q(:) = 1.0
      factconv_chem_to_qqcw(:) = 1.0
      mw_chem_array(:) = 1.0
      mw_q_array(:) = 1.0
      mw_q_mo_array(:) = 1.0

      l2 = pcnst_non_chem
      do l = p1st, numgas
         l2 = lptr_chem_to_q(l)
         if ((l2 < 1) .or. (l2 > pcnst)) cycle

! set molecular weights of gas species that may be used by cam_mam routines
         if (l == p_sulf) mw_chem_array(l) = 96.0
         if (l == p_so2 ) mw_chem_array(l) = 64.0
         if (l == p_nh3 ) mw_chem_array(l) = 17.0
         if (l == p_hno3) mw_chem_array(l) = 63.0
         if (l == p_soag) mw_chem_array(l) = 12.0
         if (l == p_h2o2) mw_chem_array(l) = 34.0
         if (l == p_o3  ) mw_chem_array(l) = 48.0
         mw_q_array(l2) = mw_chem_array(l)
         IF(.NOT.CAM_INITIALIZED_CHEM) THEN
            cnst_mw(l2)    = mw_q_array(l2)
            cnst_rgas(l2) = r_universal * cnst_mw(l2)
            cnst_cp  (l2) = nan
            cnst_cv  (l2) = nan
            qmin     (l2) = qmin_gas
            qmincg   (l2) = qmin(l2)
         ENDIF
         ! convert wrf-chem ppmv to cam kg/kg-air
         factconv_chem_to_q(l) = 1.0e-6*mw_chem_array(l)/28.966
      end do

      iphase = ai_phase
      do itype = 1, ntype_aer
      do isize = 1, nsize_aer(itype)
         l = numptr_aer(isize,itype,iphase)
         mw_chem_array(l) = 1.0
         l2 = lptr_chem_to_q(l)
         if ((l2 >= 1) .and. (l2 <= pcnst)) then 
            mw_q_array(l2) = mw_chem_array(l)
            IF(.NOT.CAM_INITIALIZED_CHEM) THEN
               cnst_mw(l2)    = mw_q_array(l2)
               cnst_rgas(l2) = r_universal * cnst_mw(l2)
               cnst_cp  (l2) = nan
               cnst_cv  (l2) = nan
               qmin     (l2) = qmin_num
               qmincg   (l2) = qmin(l2)
            ENDIF
            ! wrf-chem and cam units for number are identical (#/kg-air)
            factconv_chem_to_q(l) = 1.0
         end if

         l = waterptr_aer(isize,itype)
         if ((l >= p1st) .and. (l <= num_chem)) then 
            mw_chem_array(l) = 18.0
            ! convert wrf-chem ug/kg-air to cam kg/kg-air
            factconv_chem_to_q(l) = 1.0e-9
         end if

         do ll = 1, ncomp_aer(itype)
            l = massptr_aer(ll,isize,itype,iphase)
            mw_chem_array(l) = mw_aer(ll,itype)
            l2 = lptr_chem_to_q(l)
            if ((l2 >= 1) .and. (l2 <= pcnst)) then 
               mw_q_array(l2) = mw_chem_array(l)
               IF(.NOT.CAM_INITIALIZED_CHEM) THEN
                  cnst_mw(l2)    = mw_q_array(l2)
                  cnst_rgas(l2) = r_universal * cnst_mw(l2)
                  cnst_cp  (l2) = nan
                  cnst_cv  (l2) = nan
                  qmin     (l2) = qmin_aer
                  qmincg   (l2) = qmin(l2)
               ENDIF
               ! convert wrf-chem ug/kg-air to cam kg/kg-air
               factconv_chem_to_q(l) = 1.0e-9
            end if
         end do
      end do ! isize
      end do ! itype 

! conversion for cloud-borne aerosols

      if (nphase_aer > 1) then
      iphase = cw_phase
      do itype = 1, ntype_aer
      do isize = 1, nsize_aer(itype)
         l = numptr_aer(isize,itype,iphase)
         if (l < p1st .or. l > num_chem) then
            write(*,'(a,10i10)') '*** cw_phase numb error', iphase, itype, isize, l
         else
         mw_chem_array(l) = 1.0
         l2 = lptr_chem_to_qqcw(l)
         if ((l2 >= 1) .and. (l2 <= pcnst)) then
!            mw_qqcw_array(l2) = mw_chem_array(l)
            ! wrf-chem and cam units for number are identical (#/kg-air)
            factconv_chem_to_q(l) = 1.0
         end if
         end if
         
         do ll = 1, ncomp_aer(itype)
            l = massptr_aer(ll,isize,itype,iphase)
            if (l < p1st .or. l > num_chem) then
               write(*,'(a,10i10)') '*** cw_phase mass error', iphase, itype, isize, ll, l
            else
            mw_chem_array(l) = mw_aer(ll,itype)
            l2 = lptr_chem_to_qqcw(l)
            if ((l2 >= 1) .and. (l2 <= pcnst)) then
!               mw_qqcw_array(l2) = mw_chem_array(l)
               ! convert wrf-chem ug/kg-air to cam kg/kg-air
               factconv_chem_to_q(l) = 1.0e-9
            end if
            end if
         end do
      end do ! isize
      end do ! itype 
      end if


! mw_q_mo_array is equivalent to the cam adv_mass array
      mw_q_mo_array(1:gas_pcnst) = mw_q_array(pcnst_non_chem+1:pcnst)


      write( *, '(/2a)' ) &
         'l, cnst_name, chem_name, l3, chem_name2, ', &
         'lptr_chem_to_q, factconv_..., mw_...'
      do l = 1, max( pcnst, num_chem )
         tmpname  = ' '
         tmpname2 = ' '
         tmpname3 = ' '
         if (l <= pcnst) then
            tmpname = cnst_name_loc(l)
            do l2 = p1st, num_chem
               if (lptr_chem_to_q(l2) == l) tmpname2 = chem_dname_table(1,l2)
            end do
         end if
         l3 = l
         l4 = init_val
         if ((l3 >= p1st) .and. (l3 <= num_chem)) then
            tmpname3 = chem_dname_table(1,l3)
            l4 = lptr_chem_to_q(l3)
         end if
         if (l3 <= num_chem) then
            write( *, '(i4,2(2x,a),i6,2x,a,i12,1p,2e10.2)' ) &
               l, tmpname, tmpname2, l3, tmpname3, l4, &
               factconv_chem_to_q(l3), mw_chem_array(l3)
         else
            write( *, '(i4,2(2x,a),i6,2x,a,i12,1p,e10.2)' ) &
               l, tmpname, tmpname2
         end if
      end do

      IF(.NOT.CAM_INITIALIZED_CHEM) CAM_INITIALIZED_CHEM = .TRUE.
      return
      end subroutine cam_mam_init_other



!==============================================================
      end module module_cam_mam_init

