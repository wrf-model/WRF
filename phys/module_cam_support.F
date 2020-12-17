MODULE module_cam_support
  !------------------------------------------------------------------------
  ! This module contains global scope variables and routines shared by
  ! multiple CAM physics routines. As much as possible, the codes is copied
  ! verbatim from the corresponding CAM modules noted below.
  !
  ! Author: William.Gustafson@pnl.gov, Nov 2009
  !------------------------------------------------------------------------
  use module_state_description, only: param_num_moist
  use shr_kind_mod
  
  implicit none
  
  public
  save
  
  integer(SHR_KIND_IN),parameter,private :: R8 = SHR_KIND_R8 ! rename for local readability only
  
  ! From spmd_utils in CAM...
  logical, parameter :: masterproc = .TRUE.
  logical, parameter :: iam = .FALSE.
  
  ! From ppgrid in CAM...
  integer, parameter :: pcols = 1   !Always have a chunk size of 1 in WRF
  integer :: pver                   !Number of model level middles in CAM speak
  integer :: pverp                  !Number of model level interfaces in CAM speak
  
  ! From constituents in CAM...
  integer, parameter :: pcnst = param_num_moist  !Number of tracer constituents for CAM q array 
  !In WRF this is currently setup to only handle
  !the moist array, and then even in a half-handed way.
  !We allocate the max possible size, but loops need to
  !be over a smaller number.
  !Scalar and chem need to eventually be handled too.
  
  ! 2010-06-16 rce - about pcnst ...
  ! in CAM with modal aerosols, pcnst = 5 (water vapor + cloud water/ice mass/number) + 
  !     number of trace gas and aerosol species
  ! if we want to do things similarly in wrfchem, then we should have pcnst = 5 + num_chem,
  !     which means that pcnst is set at runtime,
  !     which means that any saved arrays (i.e. module data) need to be allocated
  ! OR, we could use a bunch of CPP directives to produce the appropriate constant value
  !
  ! for now (temporarily), the following will be used in modal aerosol routines
  ! its appropriate value must be set in an initialization routine,
  !    and the initial -999888777 should cause code to crash if it gets used
  !    before being set correctly

  !Balwinder.Singh@pnnl.gov: pcnst is now defined dynamically in module_physics_init.F. pcnst_runtime
  !is referenced by pcnst for getting the runtime (dynamic) value of pcnst. TO accomplish this, 
  !all 'module level' or 'saved' arrays are declared 'allocatable' in CAM specific modules. The 
  !allocatable arrays are allocated in the 'initialization' call of the respective module. If a module 
  !doesn't have an 'initialization' subroutine then allocatable arrays are allocated in module_physics_init.F.
  !Allocatable CAM arrays which are NOT currently used by WRF are not allocated

  integer :: pcnst_runtime    = -999888777 !Number of tracer constituents in CAM q array

  !For assisting decoupled microphysics (MP) CAM MAM simulations (simulations, where MAM package is coupled with 
  !radiation but decoupled with MP- i.e. MP runs with 'prescribed' aerosols) 'pcnst_mp' is defined.'pcnst_mp' will
  !only be used in the CAMMGMP driver and its supporting modules (ndrop and microp_aero)
  integer :: pcnst_mp         = -999888777 
  
  integer :: gas_pcnst_modal_aero = -999888777 !Number of tracer constituents in CAM q array

  !BSINGH - In simulations where no MAM chem package (501, 502,503 or 504) is used, 
  ! following integer(gas_pcnst_modal_aero_pos) is used to specify array dimensions, 
  ! so it has to have a positive value. It will assume a positive value in chemistry
  ! initializations accordling to the chem package specified in the namelist
  integer :: gas_pcnst_modal_aero_pos = -999888777 !Number of tracer constituents in CAM q array

  ! excluding water vapor, cloud water, cloud ice, droplet number, ice number
  
  integer :: pcnst_non_chem_modal_aero = -999888777  !Number of non-chemistry (i.e., moisture) species in CAM q array
  
  ! From cam_logfile...
  character(len=750) :: iulog       !In CAM this is a file handle. In WRF, this is a string
  !that can be used to send messages via wrf_message, etc.
  
  !From cam_pio_utils.F90
  integer, parameter, public :: phys_decomp=100
  
  ! From cam_pio_utils (used in camuwpbl_driver module)...
  integer, parameter :: fieldname_len = 16   ! max chars for field name

  !From chem_mods (dummy value for now)
  integer, parameter :: nfs = -999888777 !Balwinder.Singh@pnnl.gov: NFS is not used for any meaningful computations for now

  !From cam_history_support.F90
  real(r8), parameter, public :: fillvalue = 1.e36_r8     ! fill value for netcdf fields

#if ( WRF_CHEM == 1 )
  !For module_cam_mam_gas_wetdep_driver.F
  !BSINGH - We are going to operate on only 6 gases (following MOSAIC -
  !module_mosaic_wetscav.F). MOSAIC actually operates upon 7 gases but MAM
  !doesn't have msa, therefore MAM will operate on only 6 gases
  
  !so2,h2o2,h2so4,hno3,hcl,nh3 (msa not included)
  
  integer, public, parameter :: gas_wetdep_cnt = 6
  character(len=3), public, parameter  :: gas_wetdep_method = 'MOZ'
  !BSINGH - Following list should be all UPPER CASE and 'sulf' should be replaced with 'H2SO4'
  character(len=5), public, parameter   :: gas_wetdep_list(1:gas_wetdep_cnt) = (/'SO2  ','H2O2 ','H2SO4','HNO3 ','HCL  ','NH3  '/) !Upper Case

  !BSINGH:01/31/2013 - numgas_mam is being used for specifying a variable dimension in che_driver
  !This variable is *UPDATED* in chemics_init.F, where it is assigned a new value based
  !on number of gases being used in the simulation
  integer :: numgas_mam = 1

  !BSINGH:02/01/2013 - Define cam_mam_aerosol to know if the simulation is a mam simulation or not
  ! This variable is updated in module_physics_init.F
  logical :: cam_mam_aerosols = .FALSE.
#endif
  
  !------------------------------------------------------------------------
CONTAINS
  !------------------------------------------------------------------------
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
  
  
  !------------------------------------------------------------------------
  SUBROUTINE endrun(msg)
    ! Pass through routine to wrf_error_fatal that mimics endrun in module
    ! abortutils of CAM.
    !
    ! Replaces endrun in abortutils module in CAM.
    !
    ! Author: William.Gustafson@pnl.gov, Nov 2009
    ! Modified : Balwinder.Singh@pnl.gov - Argument made optional 
    !------------------------------------------------------------------------
    USE module_wrf_error
    
    ! Argument of the subroutine is made optional to accomodate endrun calls with no argument 
    character(len=*), intent(in), optional :: msg
    
    if(present(msg)) then
       call wrf_error_fatal(msg)
    else
       ! The error message is written to iulog before the endrun call
       call wrf_error_fatal(iulog)
    endif
    
  END SUBROUTINE endrun
  
  
  
  !------------------------------------------------------------------------
  SUBROUTINE t_stopf(event)
    ! Stub to accomodate stop time calls of CAM
    !
    ! Replaces t_stopf in perf_mod module in CAM.
    !
    ! Author: Balwinder.Singh@pnl.gov
    !------------------------------------------------------------------------
    character(len=*), intent(in) :: event 
    
  END SUBROUTINE t_stopf
  
  
  
  !------------------------------------------------------------------------
  SUBROUTINE t_startf(event)
    ! Stub to accomodate start time calls of CAM
    !
    ! Replaces t_startf in perf_mod module in CAM.
    !
    ! Author: Balwinder.Singh@pnl.gov
    !------------------------------------------------------------------------
    
    character(len=*), intent(in) :: event
    
  END SUBROUTINE t_startf
  
  
  
  !------------------------------------------------------------------------
  SUBROUTINE outfld( fname, field, idim, c)
    ! Stub to accomodate outfld calls of CAM
    !
    ! Replaces outfld in cam_history module in CAM.
    !
    ! Author: Balwinder.Singh@pnl.gov
    !------------------------------------------------------------------------
    character(len=*), intent(in) :: fname
    integer,          intent(in) :: idim          
    integer,          intent(in) :: c             
    real(r8),         intent(in) :: field(idim,*)
    
  END SUBROUTINE outfld
  
  
  
  !------------------------------------------------------------------------
  SUBROUTINE addfld(fname, units, numlev, avgflag, long_name, &
       decomp_type, flag_xyfill, flag_isccplev, sampling_seq)
    ! Stub to accomodate addfld calls of CAM
    !
    ! Replaces addfld in cam_history module in CAM.
    !
    ! Author: Balwinder.Singh@pnl.gov
    !------------------------------------------------------------------------
    character(len=*), intent(in) :: fname     
    character(len=*), intent(in) :: units     
    character(len=1), intent(in) :: avgflag   
    character(len=*), intent(in) :: long_name 
    
    integer, intent(in) :: numlev             
    integer, intent(in) :: decomp_type        
    
    logical, intent(in), optional :: flag_xyfill
    logical, intent(in), optional :: flag_isccplev
    character(len=*), intent(in), optional :: sampling_seq
    
  END SUBROUTINE ADDFLD
  
  
  
  !------------------------------------------------------------------------
  SUBROUTINE ADD_DEFAULT (name, tindex, flag)
    !
    ! Stub to accomodate add_default calls of CAM 
    ! Relaces add_default in cam_history module of CAM
    ! 
    ! Author: Balwinder.Singh@pnl.gov
    !-----------------------------------------------------------------------
    character(len=*), intent(in) :: name  ! field name
    character(len=1), intent(in) :: flag  ! averaging flag
    
    integer, intent(in) :: tindex         ! history tape index
    
  END SUBROUTINE ADD_DEFAULT
  
END MODULE module_cam_support
