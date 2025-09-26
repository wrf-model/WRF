#define WRF_PORT
#define MODAL_AERO
  module conv_water

   ! --------------------------------------------------------------------- ! 
   ! Purpose:                                                              !
   ! Computes grid-box average liquid (and ice) from stratus and cumulus   !
   ! Just for the purposes of radiation.                                   !
   !                                                                       ! 
   ! Method:                                                               !
   ! Extract information about deep+shallow liquid and cloud fraction from !
   ! the physics buffer.                                                   !
   !                                                                       !
   ! Author: Rich Neale, August 2006                                       !
   !         October 2006: Allow averaging of liquid to give a linear      !
   !                       average in emissivity.                          !
   !         Andrew Gettelman October 2010  Separate module                !
   !---------------------------------------------------------------------- !

  use shr_kind_mod,  only: r8=>shr_kind_r8
#ifndef WRF_PORT
  use ppgrid,        only: pcols, pver, pverp
#else
  use module_cam_support, only: pcols, pver, pverp
#endif
  use physconst,     only: gravit, latvap, latice
#ifndef WRF_PORT
  use abortutils,    only: endrun

  use perf_mod
  use cam_logfile,   only: iulog
#else
  use module_cam_support, only: endrun, iulog
#endif

  implicit none
  private
  save

  public :: conv_water_register, conv_water_4rad, conv_water_init

! pbuf indices

  integer :: icwmrsh_idx, icwmrdp_idx, fice_idx, sh_frac_idx, dp_frac_idx, concldql_idx, &
             ast_idx, alst_idx, aist_idx, qlst_idx, qist_idx, sh_cldliq1_idx, sh_cldice1_idx

  contains

  !============================================================================ !

  subroutine conv_water_register

  !---------------------------------------------------------------------- !
  !                                                                       !
  ! Register the fields in the physics buffer.                            !
  !                                                                       !
  !---------------------------------------------------------------------- !
#ifndef WRF_PORT
    use constituents, only: cnst_add, pcnst
    use physconst,    only: mwdry, cpair
    use phys_buffer,  only: pbuf_times, pbuf_add

  !-----------------------------------------------------------------------

    ! these calls were already done in convect_shallow...so here I add the same fields to the physics buffer with a "1" at the end
    call pbuf_add('SH_CLDLIQ1', 'physpkg', 1, pver,  1, sh_cldliq1_idx)  ! shallow gbm cloud liquid water (kg/kg)
    call pbuf_add('SH_CLDICE1', 'physpkg', 1, pver,  1, sh_cldice1_idx)  ! shallow gbm cloud ice water (kg/kg)
#endif

  end subroutine conv_water_register


  !============================================================================ !
  !                                                                             !
  !============================================================================ !

   subroutine conv_water_init()
   ! --------------------------------------------------------------------- ! 
   ! Purpose:                                                              !
   !   Initializes the pbuf indices required by conv_water
   ! --------------------------------------------------------------------- ! 
#ifndef WRF_PORT
   use phys_buffer,     only: pbuf_size_max, pbuf_fld, pbuf_old_tim_idx, pbuf_get_fld_idx 
#endif
   implicit none
#ifndef WRF_PORT
   icwmrsh_idx  = pbuf_get_fld_idx('ICWMRSH')
   icwmrdp_idx  = pbuf_get_fld_idx('ICWMRDP')
   fice_idx     = pbuf_get_fld_idx('FICE')
   sh_frac_idx  = pbuf_get_fld_idx('SH_FRAC')
   dp_frac_idx  = pbuf_get_fld_idx('DP_FRAC')
   concldql_idx = pbuf_get_fld_idx('CONCLDQL')
   ast_idx      = pbuf_get_fld_idx('AST')
   alst_idx     = pbuf_get_fld_idx('ALST')
   aist_idx     = pbuf_get_fld_idx('AIST')
   qlst_idx     = pbuf_get_fld_idx('QLST')
   qist_idx     = pbuf_get_fld_idx('QIST')
#endif
   end subroutine conv_water_init
#ifndef WRF_PORT
   subroutine conv_water_4rad( lchnk, ncol, pbuf, conv_water_mode, &
                               rei, pdel, ls_liq, ls_ice, totg_liq, totg_ice )
#else
     !Replace pbuf with actual variables
   subroutine conv_water_4rad( lchnk, ncol, ast, sh_icwmr, dp_icwmr, &
        fice, sh_frac, dp_frac, conv_water_mode, rei, pdel, ls_liq,  &
        ls_ice, totg_liq, totg_ice )
#endif

   ! --------------------------------------------------------------------- ! 
   ! Purpose:                                                              !
   ! Computes grid-box average liquid (and ice) from stratus and cumulus   !
   ! Just for the purposes of radiation.                                   !
   !                                                                       ! 
   ! Method:                                                               !
   ! Extract information about deep+shallow liquid and cloud fraction from !
   ! the physics buffer.                                                   !
   !                                                                       !
   ! Author: Rich Neale, August 2006                                       !
   !         October 2006: Allow averaging of liquid to give a linear      !
   !                       average in emissivity.                          !
   !                                                                       !
   !---------------------------------------------------------------------- !
#ifndef WRF_PORT
   use phys_buffer,     only: pbuf_size_max, pbuf_fld, pbuf_old_tim_idx, pbuf_get_fld_idx 
   use cam_history,     only: outfld
   use phys_control,    only: phys_getopts
   use phys_debug_util, only: phys_debug_col
#else
   use module_cam_support, only: outfld
#endif
   
   implicit none

   ! ---------------------- !
   ! Input-Output Arguments !
   ! ---------------------- !
#ifndef WRF_PORT
   type(pbuf_fld), intent(inout), dimension(pbuf_size_max) :: pbuf
#endif
   integer,  intent(in) :: lchnk
   integer,  intent(in) :: ncol
   integer,  intent(in) :: conv_water_mode
   real(r8), intent(in) :: rei(pcols,pver)        ! Ice effective drop size (microns)
   real(r8), intent(in) :: pdel(pcols,pver)       ! Moist pressure difference across layer
   real(r8), intent(in) :: ls_liq(pcols,pver)     ! Large-scale contributions to GBA cloud liq      
   real(r8), intent(in) :: ls_ice(pcols,pver)     ! Large-scale contributions to GBA cloud ice 
#ifdef WRF_PORT
   real(r8), intent(in) :: ast(pcols,pver)
   real(r8), intent(in) :: sh_icwmr(pcols,pver)
   real(r8), intent(in) :: dp_icwmr(pcols,pver)
   real(r8), intent(in) :: fice(pcols,pver)
   real(r8), intent(in) :: sh_frac(pcols,pver)
   real(r8), intent(in) :: dp_frac(pcols,pver)
#endif
   real(r8), intent(out):: totg_ice(pcols,pver)   ! Total GBA in-cloud ice
   real(r8), intent(out):: totg_liq(pcols,pver)   ! Total GBA in-cloud liquid

   ! --------------- !
   ! Local Workspace !
   ! --------------- !

   ! Physics buffer fields
#ifndef WRF_PORT
   real(r8), pointer, dimension(:,:) ::  ast      ! Physical liquid+ice stratus cloud fraction
   real(r8), pointer, dimension(:,:) ::  cu_frac  ! Final convective cloud fraction
   real(r8), pointer, dimension(:,:) ::  sh_frac  ! Shallow convective cloud fraction
   real(r8), pointer, dimension(:,:) ::  dp_frac  ! Deep convective cloud fraction

   real(r8), pointer, dimension(:,:) ::  alst     ! Physical liquid stratus cloud fraction
   real(r8), pointer, dimension(:,:) ::  aist     ! Physical ice    stratus cloud fraction
   real(r8), pointer, dimension(:,:) ::  qlst     ! Physical in-stratus LWC [kg/kg]
   real(r8), pointer, dimension(:,:) ::  qist     ! Physical in-stratus IWC [kg/kg]

   real(r8), pointer, dimension(:,:) ::  dp_icwmr ! Deep conv. cloud water
   real(r8), pointer, dimension(:,:) ::  sh_icwmr ! Shallow conv. cloud water
   real(r8), pointer, dimension(:,:) ::  fice     ! Ice partitioning ratio
   real(r8), pointer, dimension(:,:) ::  sh_cldliq ! shallow convection gbx liq cld mixing ratio for COSP
   real(r8), pointer, dimension(:,:) ::  sh_cldice ! shallow convection gbx ice cld mixing ratio for COSP
#else
   real(r8), dimension(pcols,pver) ::  sh_cldliq ! shallow convection gbx liq cld mixing ratio for COSP
   real(r8), dimension(pcols,pver) ::  sh_cldice ! shallow convection gbx ice cld mixing ratio for COSP
#endif

   ! Local Variables

   real(r8) :: conv_ice(pcols,pver)               ! Convective contributions to IC cloud ice
   real(r8) :: conv_liq(pcols,pver)               ! Convective contributions to IC cloud liquid
   real(r8) :: tot_ice(pcols,pver)                ! Total IC ice
   real(r8) :: tot_liq(pcols,pver)                ! Total IC liquid

   integer  :: i,k,itim                           ! Lon, lev indices buff stuff.
   real(r8) :: cu_icwmr                           ! Convective  water for this grid-box.   
   real(r8) :: ls_icwmr                           ! Large-scale water for this grid-box. 
   real(r8) :: tot_icwmr                          ! Large-scale water for this grid-box.  
   real(r8) :: ls_frac                            ! Large-scale cloud frac for this grid-box. 
   real(r8) :: tot0_frac, cu0_frac, dp0_frac, sh0_frac 
   real(r8) :: kabs, kabsi, kabsl, alpha, dp0, sh0, ic_limit, frac_limit  
   real(r8) :: wrk1         

   ! --------- !
   ! Parameter !
   ! --------- !

   parameter( kabsl = 0.090361_r8, frac_limit = 0.01_r8, ic_limit = 1.e-12_r8 )

 ! Get microphysics option

   character(len=16) :: microp_scheme 
#ifndef WRF_PORT   
   call phys_getopts( microp_scheme_out = microp_scheme )
#else
   microp_scheme = 'MG'
#endif

 ! Get convective in-cloud water and ice/water temperature partitioning.
#ifndef WRF_PORT   
   sh_icwmr => pbuf(icwmrsh_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1)
   dp_icwmr => pbuf(icwmrdp_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1)
   fice => pbuf(fice_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1)

 ! Get convective in-cloud fraction    

   sh_frac => pbuf(sh_frac_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1)
   dp_frac => pbuf(dp_frac_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1)
   cu_frac => pbuf(concldql_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1)  

   itim = pbuf_old_tim_idx()
   ast => pbuf(ast_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,itim) 

   itim = pbuf_old_tim_idx()
   alst => pbuf(alst_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,itim) 
   itim = pbuf_old_tim_idx()
   aist => pbuf(aist_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,itim) 
   itim = pbuf_old_tim_idx()
   qlst => pbuf(qlst_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,itim) 
   itim = pbuf_old_tim_idx()
   qist => pbuf(qist_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,itim) 
#endif

   ! --------------------------------------------------------------- !
   ! Loop through grid-boxes and determine:                          !
   ! 1. Effective mean in-cloud convective ice/liquid (deep+shallow) !
   ! 2. Effective mean in-cloud total ice/liquid (ls+convective)     !
   ! --------------------------------------------------------------- !

   do k = 1, pver
   do i = 1, ncol

      if( sh_frac(i,k) <= frac_limit .or. sh_icwmr(i,k) <= ic_limit ) then
          sh0_frac = 0._r8
      else
          sh0_frac = sh_frac(i,k)
      endif
      if( dp_frac(i,k) <= frac_limit .or. dp_icwmr(i,k) <= ic_limit ) then
          dp0_frac = 0._r8
      else
          dp0_frac = dp_frac(i,k)
      endif
      cu0_frac = sh0_frac + dp0_frac

    ! For the moment calculate the emissivity based upon the ls clouds ice fraction

      wrk1 = min(1._r8,max(0._r8, ls_ice(i,k)/(ls_ice(i,k)+ls_liq(i,k)+1.e-36_r8)))

      if( ( cu0_frac < frac_limit ) .or. ( ( sh_icwmr(i,k) + dp_icwmr(i,k) ) < ic_limit ) ) then

            cu0_frac = 0._r8
            cu_icwmr = 0._r8
         
            ls_frac = ast(i,k)
            if( ls_frac < frac_limit ) then
                ls_frac  = 0._r8
                ls_icwmr = 0._r8
            else
                ls_icwmr = ( ls_liq(i,k) + ls_ice(i,k) )/max(frac_limit,ls_frac) ! Convert to IC value.
            end if

            tot0_frac = ls_frac
            tot_icwmr = ls_icwmr
           
      else

          ! Select radiation constants (effective radii) for emissivity averaging.
            
            if( microp_scheme .eq. 'MG' ) then
                kabsi = 0.005_r8 + 1._r8/min(max(13._r8,rei(i,k)),130._r8)
            elseif( microp_scheme .eq. 'RK' ) then
                kabsi = 0.005_r8 + 1._r8/rei(i,k)
            endif
            kabs  = kabsl * ( 1._r8 - wrk1 ) + kabsi * wrk1
            alpha = -1.66_r8*kabs*pdel(i,k)/gravit*1000.0_r8

          ! Selecting cumulus in-cloud water.            

            select case (conv_water_mode) ! Type of average
            case (1) ! Area weighted arithmetic average
               cu_icwmr = ( sh0_frac * sh_icwmr(i,k) + dp0_frac*dp_icwmr(i,k))/max(frac_limit,cu0_frac)
            case (2)
               sh0 = exp(alpha*sh_icwmr(i,k))
               dp0 = exp(alpha*dp_icwmr(i,k))               
               cu_icwmr = log((sh0_frac*sh0+dp0_frac*dp0)/max(frac_limit,cu0_frac))
               cu_icwmr = cu_icwmr/alpha
            case default ! Area weighted 'arithmetic in emissivity' average.
!               call endrun ('CONV_WATER_4_RAD: Unknown option for conv_water_in_rad - exiting')
            end select

          ! Selecting total in-cloud water. 
          ! Attribute large-scale/convective area fraction differently from default.

            ls_frac   = ast(i,k) 
            ls_icwmr  = (ls_liq(i,k) + ls_ice(i,k))/max(frac_limit,ls_frac) ! Convert to IC value.
            tot0_frac = (ls_frac + cu0_frac) 

            select case (conv_water_mode) ! Type of average
            case (1) ! Area weighted 'arithmetic in emissivity' average
               tot_icwmr = (ls_frac*ls_icwmr + cu0_frac*cu_icwmr)/max(frac_limit,tot0_frac)
            case (2)
               tot_icwmr = log((ls_frac*exp(alpha*ls_icwmr)+cu0_frac*exp(alpha*cu_icwmr))/max(frac_limit,tot0_frac))
               tot_icwmr = tot_icwmr/alpha
            case default ! Area weighted 'arithmetic in emissivity' average.
!               call endrun ('CONV_WATER_4_RAD: Unknown option for conv_water_in_rad - exiting')
            end select

      end if

    ! Repartition convective cloud water into liquid and ice phase.
    ! Currently, this partition is made using the ice fraction of stratus condensate.
    ! In future, we should use ice fraction explicitly computed from the convection scheme.

      conv_ice(i,k) = cu_icwmr * wrk1
      conv_liq(i,k) = cu_icwmr * (1._r8-wrk1)

      tot_ice(i,k)  = tot_icwmr * wrk1
      tot_liq(i,k)  = tot_icwmr * (1._r8-wrk1)

      totg_ice(i,k) = tot0_frac * tot_icwmr * wrk1
      totg_liq(i,k) = tot0_frac * tot_icwmr * (1._r8-wrk1)

   end do
   end do

!add pbuff calls for COSP
#ifndef WRF_PORT   
   sh_cldliq  => pbuf(sh_cldliq1_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1)
   sh_cldice  => pbuf(sh_cldice1_idx)%fld_ptr(1,1:pcols,1:pver,lchnk,1)
#endif
   sh_cldliq(:ncol,:pver)= sh_icwmr(:ncol,:pver)*(1-fice(:ncol,:pver))*sh_frac(:ncol,:pver)
   sh_cldice(:ncol,:pver)=sh_icwmr(:ncol,:pver)*fice(:ncol,:pver)*sh_frac(:ncol,:pver)

  ! Output convective IC WMRs
   
   call outfld( 'ICLMRCU ', conv_liq  , pcols, lchnk )
   call outfld( 'ICIMRCU ', conv_ice  , pcols, lchnk )
   call outfld( 'ICWMRSH ', sh_icwmr  , pcols, lchnk )
   call outfld( 'ICWMRDP ', dp_icwmr  , pcols, lchnk ) 
   call outfld( 'ICLMRTOT', tot_liq   , pcols, lchnk )
   call outfld( 'ICIMRTOT', tot_ice   , pcols, lchnk )
   call outfld( 'SH_CLD  ', sh_frac   , pcols, lchnk )
   call outfld( 'DP_CLD  ', dp_frac   , pcols, lchnk )

  end subroutine conv_water_4rad

end module conv_water
