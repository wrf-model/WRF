module modal_aero_initialize_data_phys
  
  !This module is created so that WRF Microphysics can be run without using WRFCHEM-Balwinder.Singh@pnnl.gov
  
  
  private
  public :: modal_aero_initialize_phys
  
contains
  
  
  !==============================================================
  subroutine modal_aero_initialize_phys
    use modal_aero_data
    implicit none
    
    !--------------------------------------------------------------
    ! ... local variables
    !--------------------------------------------------------------
! pjj/cray - correct type
!   real n, pi, tmpsg(ntot_amode)
    real pi, tmpsg(ntot_amode)
    integer :: n
    
    pi = 4.*atan(1._r8)
    
    !
    ! initialize needed variables in module modal_aero_data
    !
    
    modeptr_accum  = 1
    modeptr_aitken = 2
    modeptr_coarse = 3
    
    ! setting portions of these arrays that should not be used
    ! to -999888777 should cause a seg-fault if they get use
    nspec_amode(:) = -999888777
    lspectype_amode(:,:) = -999888777
    lmassptr_amode(:,:) = -999888777
    numptr_amode(:) = -999888777
    lptr_dust_a_amode(:) = -999888777
    lptr_nacl_a_amode(:) = -999888777
    
    n = modeptr_accum
    nspec_amode(n) = 1
    lspectype_amode(1,n) = 1  ! sulfate
    lmassptr_amode(1,n) = 6   ! species 6 in state%q
    numptr_amode(n) = 7   ! species 7 in state%q
    
    n = modeptr_aitken
    nspec_amode(n) = 1
    lspectype_amode(1,n) = 1  ! sulfate
    lmassptr_amode(1,n) = 8   ! species 8 in state%q
    numptr_amode(n) = 9   ! species 9 in state%q
    
    n = modeptr_coarse
    nspec_amode(n) = 2
    lspectype_amode(1,n) = 2  ! dust
    lspectype_amode(2,n) = 3  ! seasalt
    lmassptr_amode(1,n) = 10  ! species 10 in state%q
    lmassptr_amode(2,n) = 11  ! species 11 in state%q
    numptr_amode(n) = 12  ! species 12 in state%q
    lptr_dust_a_amode(n) = lmassptr_amode(1,n)
    lptr_nacl_a_amode(n) = lmassptr_amode(2,n)
    
    lmassptrcw_amode = lmassptr_amode
    numptrcw_amode = numptr_amode
    
    msectional = 0
    alnsg_amode(:) = log( sigmag_amode(:) )
    tmpsg = exp( 4.5 * (alnsg_amode(:)**2) )
    
    voltonumb_amode(  :) = 1.0/( (pi/6.0) * (dgnum_amode(  :)**3) * tmpsg )
    voltonumblo_amode(:) = 1.0/( (pi/6.0) * (dgnumlo_amode(:)**3) * tmpsg )
    voltonumbhi_amode(:) = 1.0/( (pi/6.0) * (dgnumhi_amode(:)**3) * tmpsg )
    
    specdens_amode(:) = 1.0e3   ! match precribe_aerosol_mixactivate, but units change
    specmw_amode(:) = 132.0     ! match precribe_aerosol_mixactivate
    spechygro(:) = 0.5          ! match precribe_aerosol_mixactivate


    !For assisting decoupled microphysics (MP) CAM MAM simulations (simulations, where MAM package is coupled with 
    !radiation but decoupled with MP - i.e. MP runs with 'prescribed' aerosols) following variables are defined.
    !Following variablesn will only be used in the CAMMGMP driver and its supporting modules (ndrop and microp_aero)

    cnst_name_cw_mp(:)       = cnst_name_cw(:)
    
    msectional_mp 	     = msectional 
    modeptr_accum_mp         = modeptr_accum    
    modeptr_coarse_mp  	     = modeptr_coarse   
    modeptr_aitken_mp        = modeptr_aitken   
    ntot_amode_mp 	     = ntot_amode 
    
    numptrcw_amode_mp(:)     = numptrcw_amode(:) 
    lptr_dust_a_amode_mp(:)  = lptr_dust_a_amode(:)
    lptr_nacl_a_amode_mp(:)  = lptr_nacl_a_amode(:)
    numptr_amode_mp(:) 	     = numptr_amode(:) 	
    
    nspec_amode_mp(:)  	     = nspec_amode(:)  
    
    
    lmassptr_amode_mp(:,:)   = lmassptr_amode(:,:) 
    lspectype_amode_mp(:,:)  = lspectype_amode(:,:) 
    lmassptrcw_amode_mp(:,:) = lmassptrcw_amode(:,:)
    
    voltonumb_amode_mp(:)    = voltonumb_amode(:)
    alnsg_amode_mp(:)        = alnsg_amode(:)
    voltonumbhi_amode_mp(:)  = voltonumbhi_amode(:)
    voltonumblo_amode_mp(:)  = voltonumblo_amode(:)
    sigmag_amode_mp(:)	     = sigmag_amode(:)
    dgnum_amode_mp(:)	     = dgnum_amode(:)
    dgnumlo_amode_mp(:)	     = dgnumlo_amode(:)
    dgnumhi_amode_mp(:)      = dgnumhi_amode(:) 
    specdens_amode_mp(:)     = specdens_amode(:)
    specmw_amode_mp(:)       = specmw_amode(:)  
    spechygro_mp(:)	     = spechygro(:)
    
    
    return
  end subroutine modal_aero_initialize_phys
  
  !==============================================================
end module modal_aero_initialize_data_phys

