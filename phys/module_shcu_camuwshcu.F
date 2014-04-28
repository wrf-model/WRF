#define WRF_PORT
#define MODAL_AERO
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
  module uwshcu
#ifndef WRF_PORT
  use cam_history,    only: outfld, addfld, phys_decomp
#else
  use module_cam_support, only: outfld, addfld, phys_decomp
#if ( NMM_CORE == 1 )
  use module_state_description, only: CAMUWPBLSCHEME, MYJPBLSCHEME
#else
  use module_state_description, only: CAMUWPBLSCHEME, MYJPBLSCHEME, &
      MYNNPBLSCHEME2, MYNNPBLSCHEME3
#endif
#endif
  use error_function, only: erfc
#ifndef WRF_PORT
  use cam_logfile,    only: iulog
  use ppgrid,         only: pcols, pver, pverp
  use abortutils,     only: endrun
  use spmd_utils,     only: masterproc
#else
  use module_cam_support, only: iulog, pcols, pver, pverp, endrun, masterproc
#endif


  implicit none
  private
  save

  public &
     uwshcu_readnl,      &
     init_uwshcu,        &
     compute_uwshcu,     &
     compute_uwshcu_inv
  
  integer , parameter :: r8 = selected_real_kind(12)    !  8 byte real
  real(r8), parameter :: unset_r8 = huge(1.0_r8)
  real(r8)            :: xlv                            !  Latent heat of vaporization
  real(r8)            :: xlf                            !  Latent heat of fusion
  real(r8)            :: xls                            !  Latent heat of sublimation = xlv + xlf
  real(r8)            :: cp                             !  Specific heat of dry air
  real(r8)            :: zvir                           !  rh2o/rair - 1
  real(r8)            :: r                              !  Gas constant for dry air
  real(r8)            :: g                              !  Gravitational constant
  real(r8)            :: ep2                            !  mol wgt water vapor / mol wgt dry air 
  real(r8)            :: p00                            !  Reference pressure for exner function
  real(r8)            :: rovcp                          !  R/cp

  ! Tuning parameters set via namelist
  real(r8) :: rpen          !  For penetrative entrainment efficiency

!===============================================================================
contains
!===============================================================================
  
  real(r8) function exnf(pressure)
           real(r8), intent(in)              :: pressure
           exnf = (pressure/p00)**rovcp
           return
  end function exnf

!===============================================================================

subroutine uwshcu_readnl(nlfile)
#ifndef WRF_PORT
   use namelist_utils,  only: find_group_name
   use units,           only: getunit, freeunit
   use mpishorthand
#endif

   character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

#ifndef WRF_PORT
   ! Local variables
   integer :: unitn, ierr
   character(len=*), parameter :: subname = 'uwshcu_readnl'

   ! Namelist variables
   real(r8) :: uwshcu_rpen =  unset_r8    !  For penetrative entrainment efficiency

   namelist /uwshcu_nl/ uwshcu_rpen
   !-----------------------------------------------------------------------------

   if (masterproc) then
      unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'uwshcu_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, uwshcu_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(subname // ':: ERROR reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)
   end if

#ifdef SPMD
   ! Broadcast namelist variables
   call mpibcast(uwshcu_rpen,            1, mpir8,  0, mpicom)
#endif
   
   rpen=uwshcu_rpen
#else
   !Balwinder.Singh@pnnl.gov: rpen value has been obtained from previous incarnation 
   !of this module (see CESM101)
   rpen= 10.0_r8
#endif  

end subroutine uwshcu_readnl

!===============================================================================

  subroutine init_uwshcu( kind, xlv_in, cp_in, xlf_in, zvir_in, r_in, g_in, ep2_in &
#ifdef WRF_PORT
       , rushten, rvshten, rthshten, rqvshten,                   &
       rqcshten, rqrshten, rqishten, rqsshten, rqgshten,         &
       p_qc, p_qr, p_qi, p_qs, p_qg,                             &
       bl_pbl_physics, param_first_scalar, restart,              &
       ids, ide, jds, jde, kds, kde,                             &
       ims, ime, jms, jme, kms, kme,                             &
       its, ite, jts, jte, kts, kte                              )
#else
                                                                 )
#endif
!~add in zeroing of things like rliq that may come from ZM or may not

    !------------------------------------------------------------- ! 
    ! Purpose:                                                     !
    ! Initialize key constants for the shallow convection package. !
    !------------------------------------------------------------- !
#ifndef WRF_PORT
    use cam_history,   only: outfld, addfld, phys_decomp
    use ppgrid,        only: pcols, pver, pverp
#else
    use module_cam_support, only: outfld, addfld, phys_decomp, pcols, pver, pverp
#endif
    implicit none
    integer , intent(in) :: kind       !  kind of reals being passed in
    real(r8), intent(in) :: xlv_in     !  Latent heat of vaporization
    real(r8), intent(in) :: xlf_in     !  Latent heat of fusion
    real(r8), intent(in) :: cp_in      !  Specific heat of dry air
    real(r8), intent(in) :: zvir_in    !  rh2o/rair - 1
    real(r8), intent(in) :: r_in       !  Gas constant for dry air
    real(r8), intent(in) :: g_in       !  Gravitational constant
    real(r8), intent(in) :: ep2_in     !  mol wgt water vapor / mol wgt dry air 

    character(len=*), parameter :: subname = 'init_uwshcu'
#ifdef WRF_PORT
    LOGICAL , INTENT(IN)           ::   restart
    INTEGER , INTENT(IN)           ::   ids, ide, jds, jde, kds, kde, &
                                        ims, ime, jms, jme, kms, kme, &
                                        its, ite, jts, jte, kts, kte, &
                                        p_qc, p_qr, p_qi, p_qs, p_qg, &
                                        param_first_scalar,           &
                                        bl_pbl_physics

    REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: &
                                                             rushten, &
                                                             rvshten, &
                                                            rthshten, &
                                                            rqvshten, &
                                                            rqcshten, &
                                                            rqrshten, &
                                                            rqishten, &
                                                            rqsshten, &
                                                            rqgshten

  integer :: i, itf, j, jtf, k, ktf

  jtf = min(jte,jde-1)
  ktf = min(kte,kde-1)
  itf = min(ite,ide-1)

#ifdef WRF_PORT
  !Balwinder.Singh@pnnl.gov - Call to assign rpen
  call uwshcu_readnl('dummyString')
#endif
!
! The CAM UW shallow Cu scheme only works with PBL schemes that generate
! TKE. So, for example, YSU would be an invalid choice to combine with
! the UW scheme. For now, we are coupling into the tke_myj version of
! TKE used by MYJ and CAMUWPBL. At some point, it would be great to unify
! all the different TKEs output so that things work together properly.
! For now, we are not trying to deal with the cacophony of variables,
! i.e. tke & tke_pbl.
!
  select case(bl_pbl_physics)
#if ( NMM_CORE == 1 )
  case (CAMUWPBLSCHEME, MYJPBLSCHEME)
#else
  case (CAMUWPBLSCHEME, MYJPBLSCHEME, MYNNPBLSCHEME2, MYNNPBLSCHEME3)
#endif
     !These are acceptable PBL schemes.
  case default
     call wrf_error_fatal("The CAMUWSHCU scheme requires CAMUWPBLSCHEME, MYJPBLSCHEME or MYNN.")
  end select
!
! Initialize module_cam_support variables...
! This could be moved to a master "cam-init" subroutine once multiple CAM
! parameterizations are implemented in WRF. For now, it doesn't hurt to
! have these possibly initialized more than once since they are
! essentially constant.
!
  pver  = ktf-kts+1
  pverp = pver+1
#endif
    ! ------------------------- !
    ! Internal Output Variables !
    ! ------------------------- !

    call addfld( 'qtflx_Cu'       , 'kg/m2/s' , pverp , 'A' , 'Convective qt flux'                                  , phys_decomp )
    call addfld( 'slflx_Cu'       , 'J/m2/s'  , pverp , 'A' , 'Convective sl flux'                                  , phys_decomp )
    call addfld( 'uflx_Cu'        , 'kg/m/s2' , pverp , 'A' , 'Convective  u flux'                                  , phys_decomp )
    call addfld( 'vflx_Cu'        , 'kg/m/s2' , pverp , 'A' , 'Convective  v flux'                                  , phys_decomp )

    call addfld( 'qtten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'qt tendency by convection'                           , phys_decomp )
    call addfld( 'slten_Cu'       , 'J/kg/s'  , pver  , 'A' , 'sl tendency by convection'                           , phys_decomp )
    call addfld( 'uten_Cu'        , 'm/s2'    , pver  , 'A' , ' u tendency by convection'                           , phys_decomp )
    call addfld( 'vten_Cu'        , 'm/s2'    , pver  , 'A' , ' v tendency by convection'                           , phys_decomp )
    call addfld( 'qvten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'qv tendency by convection'                           , phys_decomp )
    call addfld( 'qlten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'ql tendency by convection'                           , phys_decomp )
    call addfld( 'qiten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'qi tendency by convection'                           , phys_decomp )

    call addfld( 'cbmf_Cu'        , 'kg/m2/s' , 1     , 'A' , 'Cumulus base mass flux'                              , phys_decomp )
    call addfld( 'ufrcinvbase_Cu' , 'fraction', 1     , 'A' , 'Cumulus fraction at PBL top'                         , phys_decomp ) 
    call addfld( 'ufrclcl_Cu'     , 'fraction', 1     , 'A' , 'Cumulus fraction at LCL'                             , phys_decomp )
    call addfld( 'winvbase_Cu'    , 'm/s'     , 1     , 'A' , 'Cumulus vertical velocity at PBL top'                , phys_decomp )
    call addfld( 'wlcl_Cu'        , 'm/s'     , 1     , 'A' , 'Cumulus vertical velocity at LCL'                    , phys_decomp )
    call addfld( 'plcl_Cu'        , 'Pa'      , 1     , 'A' , 'LCL of source air'                                   , phys_decomp )
    call addfld( 'pinv_Cu'        , 'Pa'      , 1     , 'A' , 'PBL top pressure'                                    , phys_decomp )
    call addfld( 'plfc_Cu'        , 'Pa'      , 1     , 'A' , 'LFC of source air'                                   , phys_decomp )
    call addfld( 'pbup_Cu'        , 'Pa'      , 1     , 'A' , 'Highest interface level of positive cumulus buoyancy', phys_decomp )
    call addfld( 'ppen_Cu'        , 'Pa'      , 1     , 'A' , 'Highest level where cumulus w is 0'                  , phys_decomp )
    call addfld( 'qtsrc_Cu'       , 'kg/kg'   , 1     , 'A' , 'Cumulus source air qt'                               , phys_decomp )
    call addfld( 'thlsrc_Cu'      , 'K'       , 1     , 'A' , 'Cumulus source air thl'                              , phys_decomp )
    call addfld( 'thvlsrc_Cu'     , 'K'       , 1     , 'A' , 'Cumulus source air thvl'                             , phys_decomp )
    call addfld( 'emfkbup_Cu'     , 'kg/m2/s' , 1     , 'A' , 'Penetrative mass flux at kbup'                       , phys_decomp )
    call addfld( 'cin_Cu'         , 'J/kg'    , 1     , 'A' , 'CIN upto LFC'                                        , phys_decomp )
    call addfld( 'cinlcl_Cu'      , 'J/kg'    , 1     , 'A' , 'CIN upto LCL'                                        , phys_decomp )
    call addfld( 'cbmflimit_Cu'   , 'kg/m2/s' , 1     , 'A' , 'cbmflimiter'                                         , phys_decomp ) 
    call addfld( 'tkeavg_Cu'      , 'm2/s2'   , 1     , 'A' , 'Average tke within PBL for convection scheme'        , phys_decomp ) 
    call addfld( 'zinv_Cu'        , 'm'       , 1     , 'A' , 'PBL top height'                                      , phys_decomp )
    call addfld( 'rcwp_Cu'        , 'kg/m2'   , 1     , 'A' , 'Cumulus LWP+IWP'                                     , phys_decomp )
    call addfld( 'rlwp_Cu'        , 'kg/m2'   , 1     , 'A' , 'Cumulus LWP'                                         , phys_decomp )
    call addfld( 'riwp_Cu'        , 'kg/m2'   , 1     , 'A' , 'Cumulus IWP'                                         , phys_decomp )
    call addfld( 'tophgt_Cu'      , 'm'       , 1     , 'A' , 'Cumulus top height'                                  , phys_decomp )

    call addfld( 'wu_Cu'          , 'm/s'     , pverp , 'A' , 'Convective updraft vertical velocity'                , phys_decomp )
    call addfld( 'ufrc_Cu'        , 'fraction', pverp , 'A' , 'Convective updraft fractional area'                  , phys_decomp )
    call addfld( 'qtu_Cu'         , 'kg/kg'   , pverp , 'A' , 'Cumulus updraft qt'                                  , phys_decomp )
    call addfld( 'thlu_Cu'        , 'K'       , pverp , 'A' , 'Cumulus updraft thl'                                 , phys_decomp )
    call addfld( 'thvu_Cu'        , 'K'       , pverp , 'A' , 'Cumulus updraft thv'                                 , phys_decomp )
    call addfld( 'uu_Cu'          , 'm/s'     , pverp , 'A' , 'Cumulus updraft uwnd'                                , phys_decomp )
    call addfld( 'vu_Cu'          , 'm/s'     , pverp , 'A' , 'Cumulus updraft vwnd'                                , phys_decomp )
    call addfld( 'qtu_emf_Cu'     , 'kg/kg'   , pverp , 'A' , 'qt of penatratively entrained air'                   , phys_decomp )
    call addfld( 'thlu_emf_Cu'    , 'K'       , pverp , 'A' , 'thl of penatratively entrained air'                  , phys_decomp )
    call addfld( 'uu_emf_Cu'      , 'm/s'     , pverp , 'A' , 'uwnd of penatratively entrained air'                 , phys_decomp )
    call addfld( 'vu_emf_Cu'      , 'm/s'     , pverp , 'A' , 'vwnd of penatratively entrained air'                 , phys_decomp )
    call addfld( 'umf_Cu'         , 'kg/m2/s' , pverp , 'A' , 'Cumulus updraft mass flux'                           , phys_decomp )
    call addfld( 'uemf_Cu'        , 'kg/m2/s' , pverp , 'A' , 'Cumulus net ( updraft + entrainment ) mass flux'     , phys_decomp )
    call addfld( 'qcu_Cu'         , 'kg/kg'   , pver  , 'A' , 'Cumulus updraft LWC+IWC'                             , phys_decomp )
    call addfld( 'qlu_Cu'         , 'kg/kg'   , pver  , 'A' , 'Cumulus updraft LWC'                                 , phys_decomp )
    call addfld( 'qiu_Cu'         , 'kg/kg'   , pver  , 'A' , 'Cumulus updraft IWC'                                 , phys_decomp )
    call addfld( 'cufrc_Cu'       , 'fraction', pver  , 'A' , 'Cumulus cloud fraction'                              , phys_decomp )
    call addfld( 'fer_Cu'         , '1/m'     , pver  , 'A' , 'Cumulus lateral fractional entrainment rate'         , phys_decomp )
    call addfld( 'fdr_Cu'         , '1/m'     , pver  , 'A' , 'Cumulus lateral fractional detrainment Rate'         , phys_decomp )

    call addfld( 'dwten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'Expellsion rate of cumulus cloud water to env.'      , phys_decomp )
    call addfld( 'diten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'Expellsion rate of cumulus ice water to env.'        , phys_decomp )
    call addfld( 'qrten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'Production rate of rain by cumulus'                  , phys_decomp )
    call addfld( 'qsten_Cu'       , 'kg/kg/s' , pver  , 'A' , 'Production rate of snow by cumulus'                  , phys_decomp )
    call addfld( 'flxrain_Cu'     , 'kg/m2/s' , pverp , 'A' , 'Rain flux induced by Cumulus'                        , phys_decomp )
    call addfld( 'flxsnow_Cu'     , 'kg/m2/s' , pverp , 'A' , 'Snow flux induced by Cumulus'                        , phys_decomp )
    call addfld( 'ntraprd_Cu'     , 'kg/kg/s' , pver  , 'A' , 'Net production rate of rain by Cumulus'              , phys_decomp )
    call addfld( 'ntsnprd_Cu'     , 'kg/kg/s' , pver  , 'A' , 'Net production rate of snow by Cumulus'              , phys_decomp )

    call addfld( 'excessu_Cu'     , 'no'      , pver  , 'A' , 'Updraft saturation excess'                           , phys_decomp )
    call addfld( 'excess0_Cu'     , 'no'      , pver  , 'A' , 'Environmental saturation excess'                     , phys_decomp )
    call addfld( 'xc_Cu'          , 'no'      , pver  , 'A' , 'Critical mixing ratio'                               , phys_decomp )
    call addfld( 'aquad_Cu'       , 'no'      , pver  , 'A' , 'aquad'                                               , phys_decomp )
    call addfld( 'bquad_Cu'       , 'no'      , pver  , 'A' , 'bquad'                                               , phys_decomp )
    call addfld( 'cquad_Cu'       , 'no'      , pver  , 'A' , 'cquad'                                               , phys_decomp )
    call addfld( 'bogbot_Cu'      , 'no'      , pver  , 'A' , 'Cloud buoyancy at the bottom interface'              , phys_decomp )
    call addfld( 'bogtop_Cu'      , 'no'      , pver  , 'A' , 'Cloud buoyancy at the top interface'                 , phys_decomp )

    call addfld('exit_UWCu_Cu'    , 'no'      , 1     , 'A' , 'exit_UWCu'                                           , phys_decomp ) 
    call addfld('exit_conden_Cu'  , 'no'      , 1     , 'A' , 'exit_conden'                                         , phys_decomp ) 
    call addfld('exit_klclmkx_Cu' , 'no'      , 1     , 'A' , 'exit_klclmkx'                                        , phys_decomp ) 
    call addfld('exit_klfcmkx_Cu' , 'no'      , 1     , 'A' , 'exit_klfcmkx'                                        , phys_decomp ) 
    call addfld('exit_ufrc_Cu'    , 'no'      , 1     , 'A' , 'exit_ufrc'                                           , phys_decomp ) 
    call addfld('exit_wtw_Cu'     , 'no'      , 1     , 'A' , 'exit_wtw'                                            , phys_decomp ) 
    call addfld('exit_drycore_Cu' , 'no'      , 1     , 'A' , 'exit_drycore'                                        , phys_decomp ) 
    call addfld('exit_wu_Cu'      , 'no'      , 1     , 'A' , 'exit_wu'                                             , phys_decomp ) 
    call addfld('exit_cufilter_Cu', 'no'      , 1     , 'A' , 'exit_cufilter'                                       , phys_decomp ) 
    call addfld('exit_kinv1_Cu'   , 'no'      , 1     , 'A' , 'exit_kinv1'                                          , phys_decomp ) 
    call addfld('exit_rei_Cu'     , 'no'      , 1     , 'A' , 'exit_rei'                                            , phys_decomp ) 

    call addfld('limit_shcu_Cu'   , 'no'      , 1     , 'A' , 'limit_shcu'                                          , phys_decomp ) 
    call addfld('limit_negcon_Cu' , 'no'      , 1     , 'A' , 'limit_negcon'                                        , phys_decomp ) 
    call addfld('limit_ufrc_Cu'   , 'no'      , 1     , 'A' , 'limit_ufrc'                                          , phys_decomp ) 
    call addfld('limit_ppen_Cu'   , 'no'      , 1     , 'A' , 'limit_ppen'                                          , phys_decomp ) 
    call addfld('limit_emf_Cu'    , 'no'      , 1     , 'A' , 'limit_emf'                                           , phys_decomp ) 
    call addfld('limit_cinlcl_Cu' , 'no'      , 1     , 'A' , 'limit_cinlcl'                                        , phys_decomp ) 
    call addfld('limit_cin_Cu'    , 'no'      , 1     , 'A' , 'limit_cin'                                           , phys_decomp ) 
    call addfld('limit_cbmf_Cu'   , 'no'      , 1     , 'A' , 'limit_cbmf'                                          , phys_decomp ) 
    call addfld('limit_rei_Cu'    , 'no'      , 1     , 'A' , 'limit_rei'                                           , phys_decomp ) 
    call addfld('ind_delcin_Cu'   , 'no'      , 1     , 'A' , 'ind_delcin'                                          , phys_decomp ) 

    if( kind .ne. r8 ) then
        write(iulog,*) subname//': ERROR -- real KIND does not match internal specification.'
#ifdef WRF_PORT
        call wrf_message(iulog)
#endif
        call endrun(subname//': ERROR -- real KIND does not match internal specification.')
    endif

    xlv   = xlv_in
    xlf   = xlf_in
    xls   = xlv + xlf
    cp    = cp_in
    zvir  = zvir_in
    r     = r_in
    g     = g_in
    ep2   = ep2_in
    p00   = 1.e5_r8
    rovcp = r/cp

    if (rpen == unset_r8) then
       call endrun(subname//': uwshcu_rpen must be set in the namelist')
    end if

    if ( masterproc ) then 
       write(iulog,*) subname//': tuning parameters: rpen=',rpen
#ifdef WRF_PORT
       call wrf_debug(1,iulog)
#endif
    endif

#ifdef WRF_PORT
    !
    ! Set initial values for WRF arrays dependent on restart conditions
    !
    if(.not.restart)then
       do j=jts,jtf
          do k=kts,ktf
             do i=its,itf
                rushten(i,k,j)  = 0.
                rvshten(i,k,j)  = 0.
                rthshten(i,k,j) = 0.
                rqvshten(i,k,j) = 0.
                if( p_qc > param_first_scalar ) rqcshten(i,k,j) = 0.
                if( p_qr > param_first_scalar ) rqrshten(i,k,j) = 0.
                if( p_qi > param_first_scalar ) rqishten(i,k,j) = 0.
                if( p_qs > param_first_scalar ) rqsshten(i,k,j) = 0.
                if( p_qg > param_first_scalar ) rqgshten(i,k,j) = 0.
             enddo
          enddo
       enddo
    end if   
#endif

  end subroutine init_uwshcu

  subroutine compute_uwshcu_inv( mix      , mkx        , iend          , ncnst     , dt       ,  & 
                                 ps0_inv  , zs0_inv    , p0_inv        , z0_inv    , dp0_inv  ,  &
                                 u0_inv   , v0_inv     , qv0_inv       , ql0_inv   , qi0_inv  ,  &
                                 t0_inv   , s0_inv     , tr0_inv       ,                         &
                                 tke_inv  , cldfrct_inv, concldfrct_inv, pblh      , cush     ,  & 
                                 umf_inv  , slflx_inv  , qtflx_inv     ,                         & 
                                 flxprc1_inv, flxsnow1_inv,     				 &
                                 qvten_inv, qlten_inv  , qiten_inv     ,                         &
                                 sten_inv , uten_inv   , vten_inv      , trten_inv ,             &  
                                 qrten_inv, qsten_inv  , precip        , snow      , evapc_inv,  &
                                 cufrc_inv, qcu_inv    , qlu_inv       , qiu_inv   ,             &   
                                 cbmf     , qc_inv     , rliq          ,                         &
                                 cnt_inv  , cnb_inv    , qsat          , lchnk     , dpdry0_inv )

    implicit none
    integer , intent(in)    :: lchnk     
    integer , intent(in)    :: mix
    integer , intent(in)    :: mkx
    integer , intent(in)    :: iend
    integer , intent(in)    :: ncnst
    real(r8), intent(in)    :: dt                       !  Time step : 2*delta_t [ s ]
    real(r8), intent(in)    :: ps0_inv(mix,mkx+1)       !  Environmental pressure at the interfaces [ Pa ]
    real(r8), intent(in)    :: zs0_inv(mix,mkx+1)       !  Environmental height at the interfaces   [ m ]
    real(r8), intent(in)    :: p0_inv(mix,mkx)          !  Environmental pressure at the layer mid-point [ Pa ]
    real(r8), intent(in)    :: z0_inv(mix,mkx)          !  Environmental height at the layer mid-point [ m ]
    real(r8), intent(in)    :: dp0_inv(mix,mkx)         !  Environmental layer pressure thickness [ Pa ] > 0.
    real(r8), intent(in)    :: dpdry0_inv(mix,mkx)      !  Environmental dry layer pressure thickness [ Pa ]
    real(r8), intent(in)    :: u0_inv(mix,mkx)          !  Environmental zonal wind [ m/s ]
    real(r8), intent(in)    :: v0_inv(mix,mkx)          !  Environmental meridional wind [ m/s ]
    real(r8), intent(in)    :: qv0_inv(mix,mkx)         !  Environmental water vapor specific humidity [ kg/kg ]
    real(r8), intent(in)    :: ql0_inv(mix,mkx)         !  Environmental liquid water specific humidity [ kg/kg ]
    real(r8), intent(in)    :: qi0_inv(mix,mkx)         !  Environmental ice specific humidity [ kg/kg ]
    real(r8), intent(in)    :: t0_inv(mix,mkx)          !  Environmental temperature [ K ]
    real(r8), intent(in)    :: s0_inv(mix,mkx)          !  Environmental dry static energy [ J/kg ]
    real(r8), intent(in)    :: tr0_inv(mix,mkx,ncnst)   !  Environmental tracers [ #, kg/kg ]
    real(r8), intent(in)    :: tke_inv(mix,mkx+1)       !  Turbulent kinetic energy at the interfaces [ m2/s2 ]
    real(r8), intent(in)    :: cldfrct_inv(mix,mkx)     !  Total cloud fraction at the previous time step [ fraction ]
    real(r8), intent(in)    :: concldfrct_inv(mix,mkx)  !  Total convective ( shallow + deep ) cloud fraction at the previous time step [ fraction ]
    real(r8), intent(in)    :: pblh(mix)                !  Height of PBL [ m ]
    real(r8), intent(inout) :: cush(mix)                !  Convective scale height [ m ]
    real(r8), intent(out)   :: umf_inv(mix,mkx+1)       !  Updraft mass flux at the interfaces [ kg/m2/s ]
    real(r8), intent(out)   :: qvten_inv(mix,mkx)       !  Tendency of water vapor specific humidity [ kg/kg/s ]
    real(r8), intent(out)   :: qlten_inv(mix,mkx)       !  Tendency of liquid water specific humidity [ kg/kg/s ]
    real(r8), intent(out)   :: qiten_inv(mix,mkx)       !  Tendency of ice specific humidity [ kg/kg/s ]
    real(r8), intent(out)   :: sten_inv(mix,mkx)        !  Tendency of dry static energy [ J/kg/s ]
    real(r8), intent(out)   :: uten_inv(mix,mkx)        !  Tendency of zonal wind [ m/s2 ]
    real(r8), intent(out)   :: vten_inv(mix,mkx)        !  Tendency of meridional wind [ m/s2 ]
    real(r8), intent(out)   :: trten_inv(mix,mkx,ncnst) !  Tendency of tracers [ #/s, kg/kg/s ]
    real(r8), intent(out)   :: qrten_inv(mix,mkx)       !  Tendency of rain water specific humidity [ kg/kg/s ]
    real(r8), intent(out)   :: qsten_inv(mix,mkx)       !  Tendency of snow specific humidity [ kg/kg/s ]
    real(r8), intent(out)   :: precip(mix)              !  Precipitation ( rain + snow ) flux at the surface [ m/s ]
    real(r8), intent(out)   :: snow(mix)                !  Snow flux at the surface [ m/s ]
    real(r8), intent(out)   :: evapc_inv(mix,mkx)       !  Evaporation of precipitation [ kg/kg/s ]
    real(r8), intent(out)   :: rliq(mix)                !  Vertical integral of tendency of detrained cloud condensate qc [ m/s ]
    real(r8), intent(out)   :: slflx_inv(mix,mkx+1)     !  Updraft liquid static energy flux [ J/kg * kg/m2/s ]
    real(r8), intent(out)   :: qtflx_inv(mix,mkx+1)     !  Updraft total water flux [ kg/kg * kg/m2/s ]
    real(r8), intent(out)   :: flxprc1_inv(mix,mkx+1)   ! uw grid-box mean rain+snow flux (kg m^-2 s^-1) for physics buffer calls in convect_shallow.F90
    real(r8), intent(out)   :: flxsnow1_inv(mix,mkx+1)  ! uw grid-box mean snow flux (kg m^-2 s^-1) for physics buffer calls in convect_shallow.F90

    real(r8), intent(out)   :: cufrc_inv(mix,mkx)       !  Shallow cumulus cloud fraction at the layer mid-point [ fraction ]
    real(r8), intent(out)   :: qcu_inv(mix,mkx)         !  Liquid+ice specific humidity within cumulus updraft [ kg/kg ]
    real(r8), intent(out)   :: qlu_inv(mix,mkx)         !  Liquid water specific humidity within cumulus updraft [ kg/kg ]
    real(r8), intent(out)   :: qiu_inv(mix,mkx)         !  Ice specific humidity within cumulus updraft [ kg/kg ]
    real(r8), intent(out)   :: qc_inv(mix,mkx)          !  Tendency of cumulus condensate detrained into the environment [ kg/kg/s ]
    real(r8), intent(out)   :: cbmf(mix)                !  Cumulus base mass flux [ kg/m2/s ]
    real(r8), intent(out)   :: cnt_inv(mix)             !  Cumulus top  interface index, cnt = kpen [ no ]
    real(r8), intent(out)   :: cnb_inv(mix)             !  Cumulus base interface index, cnb = krel - 1 [ no ]
    integer , external      :: qsat                     !  Function pointer to sat vap pressure function

    real(r8)                :: ps0(mix,0:mkx)           !  Environmental pressure at the interfaces [ Pa ]
    real(r8)                :: zs0(mix,0:mkx)           !  Environmental height at the interfaces   [ m ]
    real(r8)                :: p0(mix,mkx)              !  Environmental pressure at the layer mid-point [ Pa ]
    real(r8)                :: z0(mix,mkx)              !  Environmental height at the layer mid-point [ m ]
    real(r8)                :: dp0(mix,mkx)             !  Environmental layer pressure thickness [ Pa ] > 0.
    real(r8)                :: dpdry0(mix,mkx)          !  Environmental dry layer pressure thickness [ Pa ]
    real(r8)                :: u0(mix,mkx)              !  Environmental zonal wind [ m/s ]
    real(r8)                :: v0(mix,mkx)              !  Environmental meridional wind [ m/s ]
    real(r8)                :: tke(mix,0:mkx)           !  Turbulent kinetic energy at the interfaces [ m2/s2 ]
    real(r8)                :: cldfrct(mix,mkx)         !  Total cloud fraction at the previous time step [ fraction ]
    real(r8)                :: concldfrct(mix,mkx)      !  Total convective ( shallow + deep ) cloud fraction at the previous time step [ fraction ]
    real(r8)                :: qv0(mix,mkx)             !  Environmental water vapor specific humidity [ kg/kg ]
    real(r8)                :: ql0(mix,mkx)             !  Environmental liquid water specific humidity [ kg/kg ]
    real(r8)                :: qi0(mix,mkx)             !  Environmental ice specific humidity [ kg/kg ]
    real(r8)                :: t0(mix,mkx)              !  Environmental temperature [ K ]
    real(r8)                :: s0(mix,mkx)              !  Environmental dry static energy [ J/kg ]
    real(r8)                :: tr0(mix,mkx,ncnst)       !  Environmental tracers [ #, kg/kg ]
    real(r8)                :: umf(mix,0:mkx)           !  Updraft mass flux at the interfaces [ kg/m2/s ]
    real(r8)                :: qvten(mix,mkx)           !  Tendency of water vapor specific humidity [ kg/kg/s ]
    real(r8)                :: qlten(mix,mkx)           !  Tendency of liquid water specific humidity [ kg/kg/s ]
    real(r8)                :: qiten(mix,mkx)           !  tendency of ice specific humidity [ kg/kg/s ]
    real(r8)                :: sten(mix,mkx)            !  Tendency of static energy [ J/kg/s ]
    real(r8)                :: uten(mix,mkx)            !  Tendency of zonal wind [ m/s2 ]
    real(r8)                :: vten(mix,mkx)            !  Tendency of meridional wind [ m/s2 ]
    real(r8)                :: trten(mix,mkx,ncnst)     !  Tendency of tracers [ #/s, kg/kg/s ]
    real(r8)                :: qrten(mix,mkx)           !  Tendency of rain water specific humidity [ kg/kg/s ]
    real(r8)                :: qsten(mix,mkx)           !  Tendency of snow speficif humidity [ kg/kg/s ]
    real(r8)                :: evapc(mix,mkx)           !  Tendency of evaporation of precipitation [ kg/kg/s ]
    real(r8)                :: slflx(mix,0:mkx)         !  Updraft liquid static energy flux [ J/kg * kg/m2/s ]
    real(r8)                :: qtflx(mix,0:mkx)         !  Updraft total water flux [ kg/kg * kg/m2/s ]
    real(r8)                :: flxprc1(mix,0:mkx)       ! uw grid-box mean rain+snow flux (kg m^-2 s^-1) for physics buffer calls in convect_shallow.F90
    real(r8)                :: flxsnow1(mix,0:mkx)      ! uw grid-box mean snow flux (kg m^-2 s^-1) for physics buffer calls in convect_shallow.F90
    real(r8)                :: cufrc(mix,mkx)           !  Shallow cumulus cloud fraction at the layer mid-point [ fraction ]
    real(r8)                :: qcu(mix,mkx)             !  Condensate water specific humidity within cumulus updraft at the layer mid-point [ kg/kg ]
    real(r8)                :: qlu(mix,mkx)             !  Liquid water specific humidity within cumulus updraft at the layer mid-point [ kg/kg ]
    real(r8)                :: qiu(mix,mkx)             !  Ice specific humidity within cumulus updraft at the layer mid-point [ kg/kg ]
    real(r8)                :: qc(mix,mkx)              !  Tendency of cumulus condensate detrained into the environment [ kg/kg/s ]
    real(r8)                :: cnt(mix)                 !  Cumulus top  interface index, cnt = kpen [ no ]
    real(r8)                :: cnb(mix)                 !  Cumulus base interface index, cnb = krel - 1 [ no ] 
    integer                 :: k                        !  Vertical index for local fields [ no ] 
    integer                 :: k_inv                    !  Vertical index for incoming fields [ no ]
    integer                 :: m                        !  Tracer index [ no ]

    do k = 1, mkx
       k_inv               = mkx + 1 - k
       p0(:iend,k)         = p0_inv(:iend,k_inv)
       u0(:iend,k)         = u0_inv(:iend,k_inv)
       v0(:iend,k)         = v0_inv(:iend,k_inv)
       z0(:iend,k)         = z0_inv(:iend,k_inv)
       dp0(:iend,k)        = dp0_inv(:iend,k_inv)
       dpdry0(:iend,k)     = dpdry0_inv(:iend,k_inv)
       qv0(:iend,k)        = qv0_inv(:iend,k_inv)
       ql0(:iend,k)        = ql0_inv(:iend,k_inv)
       qi0(:iend,k)        = qi0_inv(:iend,k_inv)
       t0(:iend,k)         = t0_inv(:iend,k_inv)
       s0(:iend,k)         = s0_inv(:iend,k_inv)
       cldfrct(:iend,k)    = cldfrct_inv(:iend,k_inv)
       concldfrct(:iend,k) = concldfrct_inv(:iend,k_inv)
       do m = 1, ncnst
          tr0(:iend,k,m)   = tr0_inv(:iend,k_inv,m)
       enddo
    enddo
    
    do k = 0, mkx
       k_inv               = mkx + 1 - k
       ps0(:iend,k)        = ps0_inv(:iend,k_inv)
       zs0(:iend,k)        = zs0_inv(:iend,k_inv)
       tke(:iend,k)        = tke_inv(:iend,k_inv)
    end do

    call compute_uwshcu( mix  , mkx    , iend      , ncnst , dt   , &
                         ps0  , zs0    , p0        , z0    , dp0  , &
                         u0   , v0     , qv0       , ql0   , qi0  , & 
                         t0   , s0     , tr0       ,                & 
                         tke  , cldfrct, concldfrct, pblh  , cush , & 
                         umf  , slflx  , qtflx     ,                &  
                         flxprc1  , flxsnow1  ,		            &
                         qvten, qlten  , qiten     ,                & 
                         sten , uten   , vten      , trten ,        &
                         qrten, qsten  , precip    , snow  , evapc, &
                         cufrc, qcu    , qlu       , qiu   ,        &
                         cbmf , qc     , rliq      ,                &
                         cnt  , cnb    , qsat      , lchnk , dpdry0 )

    ! Reverse cloud top/base interface indices

       cnt_inv(:iend) = mkx + 1 - cnt(:iend)
       cnb_inv(:iend) = mkx + 1 - cnb(:iend)

    do k = 0, mkx
       k_inv                  = mkx + 1 - k
       umf_inv(:iend,k_inv)   = umf(:iend,k)       
       slflx_inv(:iend,k_inv) = slflx(:iend,k)     
       qtflx_inv(:iend,k_inv) = qtflx(:iend,k)
       flxprc1_inv(:iend,k_inv) = flxprc1(:iend,k)     ! reversed for output to cam
       flxsnow1_inv(:iend,k_inv) = flxsnow1(:iend,k)   ! ""
    end do

    do k = 1, mkx
       k_inv                         = mkx + 1 - k
       qvten_inv(:iend,k_inv)        = qvten(:iend,k)   
       qlten_inv(:iend,k_inv)        = qlten(:iend,k)   
       qiten_inv(:iend,k_inv)        = qiten(:iend,k)   
       sten_inv(:iend,k_inv)         = sten(:iend,k)    
       uten_inv(:iend,k_inv)         = uten(:iend,k)    
       vten_inv(:iend,k_inv)         = vten(:iend,k)    
       qrten_inv(:iend,k_inv)        = qrten(:iend,k)   
       qsten_inv(:iend,k_inv)        = qsten(:iend,k)   
       evapc_inv(:iend,k_inv)        = evapc(:iend,k)
       cufrc_inv(:iend,k_inv)        = cufrc(:iend,k)   
       qcu_inv(:iend,k_inv)          = qcu(:iend,k)     
       qlu_inv(:iend,k_inv)          = qlu(:iend,k)     
       qiu_inv(:iend,k_inv)          = qiu(:iend,k)     
       qc_inv(:iend,k_inv)           = qc(:iend,k)      
       do m = 1, ncnst
          trten_inv(:iend,k_inv,m)   = trten(:iend,k,m) 
       enddo

    enddo

  end subroutine compute_uwshcu_inv

  subroutine compute_uwshcu( mix      , mkx       , iend         , ncnst    , dt        , &
                             ps0_in   , zs0_in    , p0_in        , z0_in    , dp0_in    , &
                             u0_in    , v0_in     , qv0_in       , ql0_in   , qi0_in    , &
                             t0_in    , s0_in     , tr0_in       ,                        &
                             tke_in   , cldfrct_in, concldfrct_in,  pblh_in , cush_inout, & 
                             umf_out  , slflx_out , qtflx_out    ,                        &
                             flxprc1_out  , flxsnow1_out  , 			 	  &
                             qvten_out, qlten_out , qiten_out    ,                        & 
                             sten_out , uten_out  , vten_out     , trten_out,             &
                             qrten_out, qsten_out , precip_out   , snow_out , evapc_out , &
                             cufrc_out, qcu_out   , qlu_out      , qiu_out  ,             &
                             cbmf_out , qc_out    , rliq_out     ,                        &
                             cnt_out  , cnb_out   , qsat         , lchnk    , dpdry0_in )

    ! ------------------------------------------------------------ !
    !                                                              !  
    !  University of Washington Shallow Convection Scheme          !
    !                                                              !
    !  Described in Park and Bretherton. 2008. J. Climate :        !
    !                                                              !
    ! 'The University of Washington shallow convection and         !
    !  moist turbulent schemes and their impact on climate         !
    !  simulations with the Community Atmosphere Model'            !
    !                                                              !
    !  Coded by Sungsu Park. Oct.2005.                             ! 
    !                        May.2008.                             !
    !  For questions, send an email to sungsup@ucar.edu or         ! 
    !                                  sungsu@atmos.washington.edu !
    !                                                              !
    ! ------------------------------------------------------------ !
#ifndef WRF_PORT 
    use cam_history,     only : outfld, addfld, phys_decomp
#else
    use module_cam_support, only : outfld, addfld, phys_decomp
#endif
    use constituents,    only : qmin, cnst_get_type_byind, cnst_get_ind
#ifdef MODAL_AERO
    use modal_aero_data, only : ntot_amode, numptr_amode
#endif

    implicit none

    ! ---------------------- !
    ! Input-Output Variables !
    ! ---------------------- !

    integer , intent(in)    :: lchnk
    integer , intent(in)    :: mix
    integer , intent(in)    :: mkx
    integer , intent(in)    :: iend
    integer , intent(in)    :: ncnst
    real(r8), intent(in)    :: dt                             !  Time step : 2*delta_t [ s ]
    real(r8), intent(in)    :: ps0_in(mix,0:mkx)              !  Environmental pressure at the interfaces [ Pa ]
    real(r8), intent(in)    :: zs0_in(mix,0:mkx)              !  Environmental height at the interfaces [ m ]
    real(r8), intent(in)    :: p0_in(mix,mkx)                 !  Environmental pressure at the layer mid-point [ Pa ]
    real(r8), intent(in)    :: z0_in(mix,mkx)                 !  Environmental height at the layer mid-point [ m ]
    real(r8), intent(in)    :: dp0_in(mix,mkx)                !  Environmental layer pressure thickness [ Pa ] > 0.
    real(r8), intent(in)    :: dpdry0_in(mix,mkx)             !  Environmental dry layer pressure thickness [ Pa ]
    real(r8), intent(in)    :: u0_in(mix,mkx)                 !  Environmental zonal wind [ m/s ]
    real(r8), intent(in)    :: v0_in(mix,mkx)                 !  Environmental meridional wind [ m/s ]
    real(r8), intent(in)    :: qv0_in(mix,mkx)                !  Environmental water vapor specific humidity [ kg/kg ]
    real(r8), intent(in)    :: ql0_in(mix,mkx)                !  Environmental liquid water specific humidity [ kg/kg ]
    real(r8), intent(in)    :: qi0_in(mix,mkx)                !  Environmental ice specific humidity [ kg/kg ]
    real(r8), intent(in)    :: t0_in(mix,mkx)                 !  Environmental temperature [ K ]
    real(r8), intent(in)    :: s0_in(mix,mkx)                 !  Environmental dry static energy [ J/kg ]
    real(r8), intent(in)    :: tr0_in(mix,mkx,ncnst)          !  Environmental tracers [ #, kg/kg ]
    real(r8), intent(in)    :: tke_in(mix,0:mkx)              !  Turbulent kinetic energy at the interfaces [ m2/s2 ]
    real(r8), intent(in)    :: cldfrct_in(mix,mkx)            !  Total cloud fraction at the previous time step [ fraction ]
    real(r8), intent(in)    :: concldfrct_in(mix,mkx)         !  Total convective cloud fraction at the previous time step [ fraction ]
    real(r8), intent(in)    :: pblh_in(mix)                   !  Height of PBL [ m ]
    real(r8), intent(inout) :: cush_inout(mix)                !  Convective scale height [ m ]

    real(r8)                   tw0_in(mix,mkx)                !  Wet bulb temperature [ K ]
    real(r8)                   qw0_in(mix,mkx)                !  Wet-bulb specific humidity [ kg/kg ]

    real(r8), intent(out)   :: umf_out(mix,0:mkx)             !  Updraft mass flux at the interfaces [ kg/m2/s ]
    real(r8), intent(out)   :: qvten_out(mix,mkx)             !  Tendency of water vapor specific humidity [ kg/kg/s ]
    real(r8), intent(out)   :: qlten_out(mix,mkx)             !  Tendency of liquid water specific humidity [ kg/kg/s ]
    real(r8), intent(out)   :: qiten_out(mix,mkx)             !  Tendency of ice specific humidity [ kg/kg/s ]
    real(r8), intent(out)   :: sten_out(mix,mkx)              !  Tendency of dry static energy [ J/kg/s ]
    real(r8), intent(out)   :: uten_out(mix,mkx)              !  Tendency of zonal wind [ m/s2 ]
    real(r8), intent(out)   :: vten_out(mix,mkx)              !  Tendency of meridional wind [ m/s2 ]
    real(r8), intent(out)   :: trten_out(mix,mkx,ncnst)       !  Tendency of tracers [ #/s, kg/kg/s ]
    real(r8), intent(out)   :: qrten_out(mix,mkx)             !  Tendency of rain water specific humidity [ kg/kg/s ]
    real(r8), intent(out)   :: qsten_out(mix,mkx)             !  Tendency of snow specific humidity [ kg/kg/s ]
    real(r8), intent(out)   :: precip_out(mix)                !  Precipitation ( rain + snow ) rate at surface [ m/s ]
    real(r8), intent(out)   :: snow_out(mix)                  !  Snow rate at surface [ m/s ]
    real(r8), intent(out)   :: evapc_out(mix,mkx)             !  Tendency of evaporation of precipitation [ kg/kg/s ]
    real(r8), intent(out)   :: slflx_out(mix,0:mkx)           !  Updraft/pen.entrainment liquid static energy flux [ J/kg * kg/m2/s ]
    real(r8), intent(out)   :: qtflx_out(mix,0:mkx)           !  updraft/pen.entrainment total water flux [ kg/kg * kg/m2/s ]
    real(r8), intent(out)   :: flxprc1_out(mix,0:mkx)         ! precip (rain+snow) flux
    real(r8), intent(out)   :: flxsnow1_out(mix,0:mkx)        ! snow flux
    real(r8), intent(out)   :: cufrc_out(mix,mkx)             !  Shallow cumulus cloud fraction at the layer mid-point [ fraction ]
    real(r8), intent(out)   :: qcu_out(mix,mkx)               !  Condensate water specific humidity within cumulus updraft [ kg/kg ]
    real(r8), intent(out)   :: qlu_out(mix,mkx)               !  Liquid water specific humidity within cumulus updraft [ kg/kg ]
    real(r8), intent(out)   :: qiu_out(mix,mkx)               !  Ice specific humidity within cumulus updraft [ kg/kg ]
    real(r8), intent(out)   :: cbmf_out(mix)                  !  Cloud base mass flux [ kg/m2/s ]
    real(r8), intent(out)   :: qc_out(mix,mkx)                !  Tendency of detrained cumulus condensate into the environment [ kg/kg/s ]
    real(r8), intent(out)   :: rliq_out(mix)                  !  Vertical integral of qc_out [ m/s ]
    real(r8), intent(out)   :: cnt_out(mix)                   !  Cumulus top  interface index, cnt = kpen [ no ]
    real(r8), intent(out)   :: cnb_out(mix)                   !  Cumulus base interface index, cnb = krel - 1 [ no ] 

    !
    ! Internal Output Variables
    !

    integer , external      :: qsat 
    real(r8)                   qtten_out(mix,mkx)             !  Tendency of qt [ kg/kg/s ]
    real(r8)                   slten_out(mix,mkx)             !  Tendency of sl [ J/kg/s ]
    real(r8)                   ufrc_out(mix,0:mkx)            !  Updraft fractional area at the interfaces [ fraction ]
    real(r8)                   uflx_out(mix,0:mkx)            !  Updraft/pen.entrainment zonal momentum flux [ m/s/m2/s ]
    real(r8)                   vflx_out(mix,0:mkx)            !  Updraft/pen.entrainment meridional momentum flux [ m/s/m2/s ]
    real(r8)                   fer_out(mix,mkx)               !  Fractional lateral entrainment rate [ 1/Pa ]
    real(r8)                   fdr_out(mix,mkx)               !  Fractional lateral detrainment rate [ 1/Pa ]
    real(r8)                   cinh_out(mix)                  !  Convective INhibition upto LFC (CIN) [ J/kg ]
    real(r8)                   trflx_out(mix,0:mkx,ncnst)     !  Updraft/pen.entrainment tracer flux [ #/m2/s, kg/kg/m2/s ] 
   
    ! -------------------------------------------- !
    ! One-dimensional variables at each grid point !
    ! -------------------------------------------- !

    ! 1. Input variables

    real(r8)    ps0(0:mkx)                                    !  Environmental pressure at the interfaces [ Pa ]
    real(r8)    zs0(0:mkx)                                    !  Environmental height at the interfaces [ m ]
    real(r8)    p0(mkx)                                       !  Environmental pressure at the layer mid-point [ Pa ]
    real(r8)    z0(mkx)                                       !  Environmental height at the layer mid-point [ m ]
    real(r8)    dp0(mkx)                                      !  Environmental layer pressure thickness [ Pa ] > 0.
    real(r8)    dpdry0(mkx)                                   !  Environmental dry layer pressure thickness [ Pa ]
    real(r8)    u0(mkx)                                       !  Environmental zonal wind [ m/s ]
    real(r8)    v0(mkx)                                       !  Environmental meridional wind [ m/s ]
    real(r8)    tke(0:mkx)                                    !  Turbulent kinetic energy at the interfaces [ m2/s2 ]
    real(r8)    cldfrct(mkx)                                  !  Total cloud fraction at the previous time step [ fraction ]
    real(r8)    concldfrct(mkx)                               !  Total convective cloud fraction at the previous time step [ fraction ]
    real(r8)    qv0(mkx)                                      !  Environmental water vapor specific humidity [ kg/kg ]
    real(r8)    ql0(mkx)                                      !  Environmental liquid water specific humidity [ kg/kg ]
    real(r8)    qi0(mkx)                                      !  Environmental ice specific humidity [ kg/kg ]
    real(r8)    t0(mkx)                                       !  Environmental temperature [ K ]
    real(r8)    s0(mkx)                                       !  Environmental dry static energy [ J/kg ]
    real(r8)    pblh                                          !  Height of PBL [ m ]
    real(r8)    cush                                          !  Convective scale height [ m ]
    real(r8)    tr0(mkx,ncnst)                                !  Environmental tracers [ #, kg/kg ]

    ! 2. Environmental variables directly derived from the input variables

    real(r8)    qt0(mkx)                                      !  Environmental total specific humidity [ kg/kg ]
    real(r8)    thl0(mkx)                                     !  Environmental liquid potential temperature [ K ]
    real(r8)    thvl0(mkx)                                    !  Environmental liquid virtual potential temperature [ K ]
    real(r8)    ssqt0(mkx)                                    !  Linear internal slope of environmental total specific humidity [ kg/kg/Pa ]
    real(r8)    ssthl0(mkx)                                   !  Linear internal slope of environmental liquid potential temperature [ K/Pa ]
    real(r8)    ssu0(mkx)                                     !  Linear internal slope of environmental zonal wind [ m/s/Pa ]
    real(r8)    ssv0(mkx)                                     !  Linear internal slope of environmental meridional wind [ m/s/Pa ]
    real(r8)    thv0bot(mkx)                                  !  Environmental virtual potential temperature at the bottom of each layer [ K ]
    real(r8)    thv0top(mkx)                                  !  Environmental virtual potential temperature at the top of each layer [ K ]
    real(r8)    thvl0bot(mkx)                                 !  Environmental liquid virtual potential temperature at the bottom of each layer [ K ]
    real(r8)    thvl0top(mkx)                                 !  Environmental liquid virtual potential temperature at the top of each layer [ K ]
    real(r8)    exn0(mkx)                                     !  Exner function at the layer mid points [ no ]
    real(r8)    exns0(0:mkx)                                  !  Exner function at the interfaces [ no ]
    real(r8)    sstr0(mkx,ncnst)                              !  Linear slope of environmental tracers [ #/Pa, kg/kg/Pa ]

   ! 2-1. For preventing negative condensate at the provisional time step

    real(r8)    qv0_star(mkx)                                 !  Environmental water vapor specific humidity [ kg/kg ]
    real(r8)    ql0_star(mkx)                                 !  Environmental liquid water specific humidity [ kg/kg ]
    real(r8)    qi0_star(mkx)                                 !  Environmental ice specific humidity [ kg/kg ]
    real(r8)    t0_star(mkx)                                  !  Environmental temperature [ K ]
    real(r8)    s0_star(mkx)                                  !  Environmental dry static energy [ J/kg ]

   ! 3. Variables associated with cumulus convection

    real(r8)    umf(0:mkx)                                    !  Updraft mass flux at the interfaces [ kg/m2/s ]
    real(r8)    emf(0:mkx)                                    !  Penetrative entrainment mass flux at the interfaces [ kg/m2/s ]
    real(r8)    qvten(mkx)                                    !  Tendency of water vapor specific humidity [ kg/kg/s ]
    real(r8)    qlten(mkx)                                    !  Tendency of liquid water specific humidity [ kg/kg/s ]
    real(r8)    qiten(mkx)                                    !  Tendency of ice specific humidity [ kg/kg/s ]
    real(r8)    sten(mkx)                                     !  Tendency of dry static energy [ J/kg ]
    real(r8)    uten(mkx)                                     !  Tendency of zonal wind [ m/s2 ]
    real(r8)    vten(mkx)                                     !  Tendency of meridional wind [ m/s2 ]
    real(r8)    qrten(mkx)                                    !  Tendency of rain water specific humidity [ kg/kg/s ]
    real(r8)    qsten(mkx)                                    !  Tendency of snow specific humidity [ kg/kg/s ]
    real(r8)    precip                                        !  Precipitation rate ( rain + snow) at the surface [ m/s ]
    real(r8)    snow                                          !  Snow rate at the surface [ m/s ]
    real(r8)    evapc(mkx)                                    !  Tendency of evaporation of precipitation [ kg/kg/s ]
    real(r8)    slflx(0:mkx)                                  !  Updraft/pen.entrainment liquid static energy flux [ J/kg * kg/m2/s ]
    real(r8)    qtflx(0:mkx)                                  !  Updraft/pen.entrainment total water flux [ kg/kg * kg/m2/s ]
    real(r8)    uflx(0:mkx)                                   !  Updraft/pen.entrainment flux of zonal momentum [ m/s/m2/s ]
    real(r8)    vflx(0:mkx)                                   !  Updraft/pen.entrainment flux of meridional momentum [ m/s/m2/s ]
    real(r8)    cufrc(mkx)                                    !  Shallow cumulus cloud fraction at the layer mid-point [ fraction ]
    real(r8)    qcu(mkx)                                      !  Condensate water specific humidity within convective updraft [ kg/kg ]
    real(r8)    qlu(mkx)                                      !  Liquid water specific humidity within convective updraft [ kg/kg ]
    real(r8)    qiu(mkx)                                      !  Ice specific humidity within convective updraft [ kg/kg ]
    real(r8)    dwten(mkx)                                    !  Detrained water tendency from cumulus updraft [ kg/kg/s ]
    real(r8)    diten(mkx)                                    !  Detrained ice   tendency from cumulus updraft [ kg/kg/s ]
    real(r8)    fer(mkx)                                      !  Fractional lateral entrainment rate [ 1/Pa ]
    real(r8)    fdr(mkx)                                      !  Fractional lateral detrainment rate [ 1/Pa ]
    real(r8)    uf(mkx)                                       !  Zonal wind at the provisional time step [ m/s ]
    real(r8)    vf(mkx)                                       !  Meridional wind at the provisional time step [ m/s ]
    real(r8)    qc(mkx)                                       !  Tendency due to detrained 'cloud water + cloud ice' (without rain-snow contribution) [ kg/kg/s ]
    real(r8)    qc_l(mkx)                                     !  Tendency due to detrained 'cloud water' (without rain-snow contribution) [ kg/kg/s ]
    real(r8)    qc_i(mkx)                                     !  Tendency due to detrained 'cloud ice' (without rain-snow contribution) [ kg/kg/s ]
    real(r8)    qc_lm
    real(r8)    qc_im
    real(r8)    nc_lm
    real(r8)    nc_im
    real(r8)    ql_emf_kbup
    real(r8)    qi_emf_kbup
    real(r8)    nl_emf_kbup
    real(r8)    ni_emf_kbup
    real(r8)    qlten_det
    real(r8)    qiten_det
    real(r8)    rliq                                          !  Vertical integral of qc [ m/s ] 
    real(r8)    cnt                                           !  Cumulus top  interface index, cnt = kpen [ no ]
    real(r8)    cnb                                           !  Cumulus base interface index, cnb = krel - 1 [ no ] 
    real(r8)    qtten(mkx)                                    !  Tendency of qt [ kg/kg/s ]
    real(r8)    slten(mkx)                                    !  Tendency of sl [ J/kg/s ]
    real(r8)    ufrc(0:mkx)                                   !  Updraft fractional area [ fraction ]
    real(r8)    trten(mkx,ncnst)                              !  Tendency of tracers [ #/s, kg/kg/s ]
    real(r8)    trflx(0:mkx,ncnst)                            !  Flux of tracers due to convection [ # * kg/m2/s, kg/kg * kg/m2/s ]
    real(r8)    trflx_d(0:mkx)                                !  Adjustive downward flux of tracers to prevent negative tracers
    real(r8)    trflx_u(0:mkx)                                !  Adjustive upward   flux of tracers to prevent negative tracers
    real(r8)    trmin                                         !  Minimum concentration of tracers allowed
    real(r8)    pdelx, dum 
    
    !----- Variables used for the calculation of condensation sink associated with compensating subsidence
    !      In the current code, this 'sink' tendency is simply set to be zero.

    real(r8)    uemf(0:mkx)                                   !  Net updraft mass flux at the interface ( emf + umf ) [ kg/m2/s ]
    real(r8)    comsub(mkx)                                   !  Compensating subsidence at the layer mid-point ( unit of mass flux, umf ) [ kg/m2/s ]
    real(r8)    qlten_sink(mkx)                               !  Liquid condensate tendency by compensating subsidence/upwelling [ kg/kg/s ]
    real(r8)    qiten_sink(mkx)                               !  Ice    condensate tendency by compensating subsidence/upwelling [ kg/kg/s ]
    real(r8)    nlten_sink(mkx)                               !  Liquid droplets # tendency by compensating subsidence/upwelling [ kg/kg/s ]
    real(r8)    niten_sink(mkx)                               !  Ice    droplets # tendency by compensating subsidence/upwelling [ kg/kg/s ]
    real(r8)    thlten_sub, qtten_sub                         !  Tendency of conservative scalars by compensating subsidence/upwelling
    real(r8)    qlten_sub, qiten_sub                          !  Tendency of ql0, qi0             by compensating subsidence/upwelling
    real(r8)    nlten_sub, niten_sub                          !  Tendency of nl0, ni0             by compensating subsidence/upwelling
    real(r8)    thl_prog, qt_prog                             !  Prognosed 'thl, qt' by compensating subsidence/upwelling 

    !----- Variables describing cumulus updraft

    real(r8)    wu(0:mkx)                                     !  Updraft vertical velocity at the interface [ m/s ]
    real(r8)    thlu(0:mkx)                                   !  Updraft liquid potential temperature at the interface [ K ]
    real(r8)    qtu(0:mkx)                                    !  Updraft total specific humidity at the interface [ kg/kg ]
    real(r8)    uu(0:mkx)                                     !  Updraft zonal wind at the interface [ m/s ]
    real(r8)    vu(0:mkx)                                     !  Updraft meridional wind at the interface [ m/s ]
    real(r8)    thvu(0:mkx)                                   !  Updraft virtual potential temperature at the interface [ m/s ]
    real(r8)    rei(mkx)                                      !  Updraft fractional mixing rate with the environment [ 1/Pa ]
    real(r8)    tru(0:mkx,ncnst)                              !  Updraft tracers [ #, kg/kg ]

    !----- Variables describing conservative scalars of entraining downdrafts  at the 
    !      entraining interfaces, i.e., 'kbup <= k < kpen-1'. At the other interfaces,
    !      belows are simply set to equal to those of updraft for simplicity - but it
    !      does not influence numerical calculation.

    real(r8)    thlu_emf(0:mkx)                               !  Penetrative downdraft liquid potential temperature at entraining interfaces [ K ]
    real(r8)    qtu_emf(0:mkx)                                !  Penetrative downdraft total water at entraining interfaces [ kg/kg ]
    real(r8)    uu_emf(0:mkx)                                 !  Penetrative downdraft zonal wind at entraining interfaces [ m/s ]
    real(r8)    vu_emf(0:mkx)                                 !  Penetrative downdraft meridional wind at entraining interfaces [ m/s ]
    real(r8)    tru_emf(0:mkx,ncnst)                          !  Penetrative Downdraft tracers at entraining interfaces [ #, kg/kg ]    

    !----- Variables associated with evaporations of convective 'rain' and 'snow'

    real(r8)    flxrain(0:mkx)                                !  Downward rain flux at each interface [ kg/m2/s ]
    real(r8)    flxsnow(0:mkx)                                !  Downward snow flux at each interface [ kg/m2/s ]
    real(r8)    ntraprd(mkx)                                  !  Net production ( production - evaporation +  melting ) rate of rain in each layer [ kg/kg/s ]
    real(r8)    ntsnprd(mkx)                                  !  Net production ( production - evaporation + freezing ) rate of snow in each layer [ kg/kg/s ]
    real(r8)    flxsntm                                       !  Downward snow flux at the top of each layer after melting [ kg/m2/s ]
    real(r8)    snowmlt                                       !  Snow melting tendency [ kg/kg/s ]
    real(r8)    subsat                                        !  Sub-saturation ratio (1-qv/qs) [ no unit ]
    real(r8)    evprain                                       !  Evaporation rate of rain [ kg/kg/s ]
    real(r8)    evpsnow                                       !  Evaporation rate of snow [ kg/kg/s ]
    real(r8)    evplimit                                      !  Limiter of 'evprain + evpsnow' [ kg/kg/s ]
    real(r8)    evplimit_rain                                 !  Limiter of 'evprain' [ kg/kg/s ]
    real(r8)    evplimit_snow                                 !  Limiter of 'evpsnow' [ kg/kg/s ]
    real(r8)    evpint_rain                                   !  Vertically-integrated evaporative flux of rain [ kg/m2/s ]
    real(r8)    evpint_snow                                   !  Vertically-integrated evaporative flux of snow [ kg/m2/s ]
    real(r8)    kevp                                          !  Evaporative efficiency [ complex unit ]

    !----- Other internal variables

    integer     kk, mm, k, i, m, kp1, km1
    integer     iter_scaleh, iter_xc
    integer     id_check, status
    integer     klcl                                          !  Layer containing LCL of source air
    integer     kinv                                          !  Inversion layer with PBL top interface as a lower interface
    integer     krel                                          !  Release layer where buoyancy sorting mixing occurs for the first time
    integer     klfc                                          !  LFC layer of cumulus source air
    integer     kbup                                          !  Top layer in which cloud buoyancy is positive at the top interface
    integer     kpen                                          !  Highest layer with positive updraft vertical velocity - top layer cumulus can reach
    logical     id_exit   
    logical     forcedCu                                      !  If 'true', cumulus updraft cannot overcome the buoyancy barrier just above the PBL top.
    real(r8)    thlsrc, qtsrc, usrc, vsrc, thvlsrc            !  Updraft source air properties
    real(r8)    PGFc, uplus, vplus
    real(r8)    trsrc(ncnst), tre(ncnst)
    real(r8)    plcl, plfc, prel, wrel
    real(r8)    frc_rasn
    real(r8)    ee2, ud2, wtw, wtwb, wtwh
    real(r8)    xc, xc_2                                       
    real(r8)    cldhgt, scaleh, tscaleh, cridis, rle, rkm
    real(r8)    rkfre, sigmaw, epsvarw, tkeavg, dpsum, dpi, thvlmin
    real(r8)    thlxsat, qtxsat, thvxsat, x_cu, x_en, thv_x0, thv_x1
    real(r8)    thj, qvj, qlj, qij, thvj, tj, thv0j, rho0j, rhos0j, qse 
    real(r8)    cin, cinlcl
    real(r8)    pe, dpe, exne, thvebot, thle, qte, ue, ve, thlue, qtue, wue
    real(r8)    mu, mumin0, mumin1, mumin2, mulcl, mulclstar
    real(r8)    cbmf, wcrit, winv, wlcl, ufrcinv, ufrclcl, rmaxfrac
    real(r8)    criqc, exql, exqi, ppen
    real(r8)    thl0top, thl0bot, qt0bot, qt0top, thvubot, thvutop
    real(r8)    thlu_top, qtu_top, qlu_top, qiu_top, qlu_mid, qiu_mid, exntop
    real(r8)    thl0lcl, qt0lcl, thv0lcl, thv0rel, rho0inv, autodet
    real(r8)    aquad, bquad, cquad, xc1, xc2, excessu, excess0, xsat, xs1, xs2
    real(r8)    bogbot, bogtop, delbog, drage, expfac, rbuoy, rdrag
    real(r8)    rcwp, rlwp, riwp, qcubelow, qlubelow, qiubelow
    real(r8)    rainflx, snowflx                     
    real(r8)    es(1)                               
    real(r8)    qs(1)                               
    real(r8)    gam(1)                                        !  (L/cp)*dqs/dT
    real(r8)    qsat_arg             
    real(r8)    xsrc, xmean, xtop, xbot, xflx(0:mkx)
    real(r8)    tmp1, tmp2

    !----- Some diagnostic internal output variables

    real(r8)  ufrcinvbase_out(mix)                            !  Cumulus updraft fraction at the PBL top [ fraction ]
    real(r8)  ufrclcl_out(mix)                                !  Cumulus updraft fraction at the LCL ( or PBL top when LCL is below PBL top ) [ fraction ]
    real(r8)  winvbase_out(mix)                               !  Cumulus updraft velocity at the PBL top [ m/s ]
    real(r8)  wlcl_out(mix)                                   !  Cumulus updraft velocity at the LCL ( or PBL top when LCL is below PBL top ) [ m/s ]
    real(r8)  plcl_out(mix)                                   !  LCL of source air [ Pa ]
    real(r8)  pinv_out(mix)                                   !  PBL top pressure [ Pa ]
    real(r8)  plfc_out(mix)                                   !  LFC of source air [ Pa ]
    real(r8)  pbup_out(mix)                                   !  Highest interface level of positive buoyancy [ Pa ]
    real(r8)  ppen_out(mix)                                   !  Highest interface evel where Cu w = 0 [ Pa ]
    real(r8)  qtsrc_out(mix)                                  !  Sourse air qt [ kg/kg ]
    real(r8)  thlsrc_out(mix)                                 !  Sourse air thl [ K ]
    real(r8)  thvlsrc_out(mix)                                !  Sourse air thvl [ K ]
    real(r8)  emfkbup_out(mix)                                !  Penetrative downward mass flux at 'kbup' interface [ kg/m2/s ]
    real(r8)  cinlclh_out(mix)                                !  Convective INhibition upto LCL (CIN) [ J/kg = m2/s2 ]
    real(r8)  tkeavg_out(mix)                                 !  Average tke over the PBL [ m2/s2 ]
    real(r8)  cbmflimit_out(mix)                              !  Cloud base mass flux limiter [ kg/m2/s ]
    real(r8)  zinv_out(mix)                                   !  PBL top height [ m ]
    real(r8)  rcwp_out(mix)                                   !  Layer mean Cumulus LWP+IWP [ kg/m2 ] 
    real(r8)  rlwp_out(mix)                                   !  Layer mean Cumulus LWP [ kg/m2 ] 
    real(r8)  riwp_out(mix)                                   !  Layer mean Cumulus IWP [ kg/m2 ] 
    real(r8)  wu_out(mix,0:mkx)                               !  Updraft vertical velocity ( defined from the release level to 'kpen-1' interface )
    real(r8)  qtu_out(mix,0:mkx)                              !  Updraft qt [ kg/kg ]
    real(r8)  thlu_out(mix,0:mkx)                             !  Updraft thl [ K ]
    real(r8)  thvu_out(mix,0:mkx)                             !  Updraft thv [ K ]
    real(r8)  uu_out(mix,0:mkx)                               !  Updraft zonal wind [ m/s ] 
    real(r8)  vu_out(mix,0:mkx)                               !  Updraft meridional wind [ m/s ]
    real(r8)  qtu_emf_out(mix,0:mkx)                          !  Penetratively entrained qt [ kg/kg ]   
    real(r8)  thlu_emf_out(mix,0:mkx)                         !  Penetratively entrained thl [ K ]
    real(r8)  uu_emf_out(mix,0:mkx)                           !  Penetratively entrained u [ m/s ]
    real(r8)  vu_emf_out(mix,0:mkx)                           !  Penetratively entrained v [ m/s ]
    real(r8)  uemf_out(mix,0:mkx)                             !  Net upward mass flux including penetrative entrainment (umf+emf) [ kg/m2/s ]
    real(r8)  tru_out(mix,0:mkx,ncnst)                        !  Updraft tracers [ #, kg/kg ]   
    real(r8)  tru_emf_out(mix,0:mkx,ncnst)                    !  Penetratively entrained tracers [ #, kg/kg ]

    real(r8)  wu_s(0:mkx)                                     !  Same as above but for implicit CIN
    real(r8)  qtu_s(0:mkx)
    real(r8)  thlu_s(0:mkx)
    real(r8)  thvu_s(0:mkx)
    real(r8)  uu_s(0:mkx)
    real(r8)  vu_s(0:mkx)
    real(r8)  qtu_emf_s(0:mkx) 
    real(r8)  thlu_emf_s(0:mkx)  
    real(r8)  uu_emf_s(0:mkx)   
    real(r8)  vu_emf_s(0:mkx)
    real(r8)  uemf_s(0:mkx)   
    real(r8)  tru_s(0:mkx,ncnst)
    real(r8)  tru_emf_s(0:mkx,ncnst)   

    real(r8)  dwten_out(mix,mkx)
    real(r8)  diten_out(mix,mkx)
    real(r8)  flxrain_out(mix,0:mkx)  
    real(r8)  flxsnow_out(mix,0:mkx)  
    real(r8)  ntraprd_out(mix,mkx)    
    real(r8)  ntsnprd_out(mix,mkx)    

    real(r8)  dwten_s(mkx)
    real(r8)  diten_s(mkx)
    real(r8)  flxrain_s(0:mkx)  
    real(r8)  flxsnow_s(0:mkx)  
    real(r8)  ntraprd_s(mkx)    
    real(r8)  ntsnprd_s(mkx)    

    real(r8)  excessu_arr_out(mix,mkx)
    real(r8)  excessu_arr(mkx) 
    real(r8)  excessu_arr_s(mkx)
    real(r8)  excess0_arr_out(mix,mkx)
    real(r8)  excess0_arr(mkx)
    real(r8)  excess0_arr_s(mkx)
    real(r8)  xc_arr_out(mix,mkx)
    real(r8)  xc_arr(mkx)
    real(r8)  xc_arr_s(mkx)
    real(r8)  aquad_arr_out(mix,mkx)
    real(r8)  aquad_arr(mkx)
    real(r8)  aquad_arr_s(mkx)
    real(r8)  bquad_arr_out(mix,mkx)
    real(r8)  bquad_arr(mkx)
    real(r8)  bquad_arr_s(mkx)
    real(r8)  cquad_arr_out(mix,mkx) 
    real(r8)  cquad_arr(mkx)
    real(r8)  cquad_arr_s(mkx)
    real(r8)  bogbot_arr_out(mix,mkx)
    real(r8)  bogbot_arr(mkx)
    real(r8)  bogbot_arr_s(mkx)
    real(r8)  bogtop_arr_out(mix,mkx)
    real(r8)  bogtop_arr(mkx)
    real(r8)  bogtop_arr_s(mkx)

    real(r8)  exit_UWCu(mix)
    real(r8)  exit_conden(mix)
    real(r8)  exit_klclmkx(mix)
    real(r8)  exit_klfcmkx(mix)
    real(r8)  exit_ufrc(mix)
    real(r8)  exit_wtw(mix)
    real(r8)  exit_drycore(mix)
    real(r8)  exit_wu(mix)
    real(r8)  exit_cufilter(mix)
    real(r8)  exit_kinv1(mix)
    real(r8)  exit_rei(mix)

    real(r8)  limit_shcu(mix)
    real(r8)  limit_negcon(mix)
    real(r8)  limit_ufrc(mix)
    real(r8)  limit_ppen(mix)
    real(r8)  limit_emf(mix)
    real(r8)  limit_cinlcl(mix)
    real(r8)  limit_cin(mix)
    real(r8)  limit_cbmf(mix)
    real(r8)  limit_rei(mix)
    real(r8)  ind_delcin(mix)

    real(r8) :: ufrcinvbase_s, ufrclcl_s, winvbase_s, wlcl_s, plcl_s, pinv_s, plfc_s, &
                qtsrc_s, thlsrc_s, thvlsrc_s, emfkbup_s, cinlcl_s, pbup_s, ppen_s, cbmflimit_s, &
                tkeavg_s, zinv_s, rcwp_s, rlwp_s, riwp_s 
    real(r8) :: ufrcinvbase, winvbase, pinv, zinv, emfkbup, cbmflimit, rho0rel  

    !----- Variables for implicit CIN computation

    real(r8), dimension(mkx)         :: qv0_s  , ql0_s   , qi0_s   , s0_s    , u0_s    ,           & 
                                        v0_s   , t0_s    , qt0_s   , thl0_s  , thvl0_s , qvten_s , &
                                        qlten_s, qiten_s , qrten_s , qsten_s , sten_s  , evapc_s , &
                                        uten_s , vten_s  , cufrc_s , qcu_s   , qlu_s   , qiu_s   , &
                                        fer_s  , fdr_s   , qc_s    , qtten_s , slten_s 
    real(r8), dimension(0:mkx)       :: umf_s  , slflx_s , qtflx_s , ufrc_s  , uflx_s , vflx_s
    real(r8)                         :: cush_s , precip_s, snow_s  , cin_s   , rliq_s, cbmf_s, cnt_s, cnb_s
    real(r8)                         :: cin_i,cin_f,del_CIN,ke,alpha,thlj
    real(r8)                         :: cinlcl_i,cinlcl_f,del_cinlcl
    integer                          :: iter

    real(r8), dimension(mkx,ncnst)   :: tr0_s, trten_s
    real(r8), dimension(0:mkx,ncnst) :: trflx_s

    !----- Variables for temporary storages

    real(r8), dimension(mkx)         :: qv0_o, ql0_o, qi0_o, t0_o, s0_o, u0_o, v0_o
    real(r8), dimension(mkx)         :: qt0_o    , thl0_o   , thvl0_o   ,                         &
                                        qvten_o  , qlten_o  , qiten_o   , qrten_o   , qsten_o ,   &
                                        sten_o   , uten_o   , vten_o    , qcu_o     , qlu_o   ,   & 
                                        qiu_o    , cufrc_o  , evapc_o   ,                         &
                                        thv0bot_o, thv0top_o, thvl0bot_o, thvl0top_o,             &
                                        ssthl0_o , ssqt0_o  , ssu0_o    , ssv0_o    , qc_o    ,   &
                                        qtten_o  , slten_o  
    real(r8), dimension(0:mkx)       :: umf_o    , slflx_o  , qtflx_o   , ufrc_o 
    real(r8), dimension(mix)         :: cush_o   , precip_o , snow_o    , rliq_o, cbmf_o, cnt_o, cnb_o
    real(r8), dimension(0:mkx)       :: uflx_o   , vflx_o
    real(r8)                         :: tkeavg_o , thvlmin_o, qtsrc_o  , thvlsrc_o, thlsrc_o ,    &
                                        usrc_o   , vsrc_o   , plcl_o   , plfc_o   ,               &
                                        thv0lcl_o, cinlcl_o 
    integer                          :: kinv_o   , klcl_o   , klfc_o  

    real(r8), dimension(mkx,ncnst)   :: tr0_o
    real(r8), dimension(mkx,ncnst)   :: trten_o, sstr0_o  
    real(r8), dimension(0:mkx,ncnst) :: trflx_o
    real(r8), dimension(ncnst)       :: trsrc_o
    integer                          :: ixnumliq, ixnumice

    ! ------------------ !
    !                    !
    ! Define Parameters  !
    !                    !
    ! ------------------ !

    ! ------------------------ !
    ! Iterative xc calculation !
    ! ------------------------ !

    integer , parameter              :: niter_xc = 2

    ! ----------------------------------------------------------- !
    ! Choice of 'CIN = cin' (.true.) or 'CIN = cinlcl' (.false.). !
    ! ----------------------------------------------------------- !

    logical , parameter              :: use_CINcin = .true.

    ! --------------------------------------------------------------- !
    ! Choice of 'explicit' ( 1 ) or 'implicit' ( 2 )  CIN.            !
    !                                                                 !
    ! When choose 'CIN = cinlcl' above,  it is recommended not to use ! 
    ! implicit CIN, i.e., do 'NOT' choose simultaneously :            !
    !            [ 'use_CINcin=.false. & 'iter_cin=2' ]               !
    ! since 'cinlcl' will be always set to zero whenever LCL is below !
    ! the PBL top interface in the current code. So, averaging cinlcl !
    ! of two iter_cin steps is likely not so good. Except that,   all !
    ! the other combinations of  'use_CINcin'  & 'iter_cin' are OK.   !
    !                                                                 !
    ! Feb 2007, Bundy: Note that use_CINcin = .false. will try to use !
    !           a variable (del_cinlcl) that is not currently set     !
    !                                                                 !
    ! --------------------------------------------------------------- !

    integer , parameter              :: iter_cin = 2

    ! ---------------------------------------------------------------- !
    ! Choice of 'self-detrainment' by negative buoyancy in calculating !
    ! cumulus updraft mass flux at the top interface in each layer.    !
    ! ---------------------------------------------------------------- !

    logical , parameter              :: use_self_detrain = .false.
    
    ! --------------------------------------------------------- !
    ! Cumulus momentum flux : turn-on (.true.) or off (.false.) !
    ! --------------------------------------------------------- !

    logical , parameter              :: use_momenflx = .true.

    ! ----------------------------------------------------------------------------------------- !
    ! Penetrative Entrainment : Cumulative ( .true. , original ) or Non-Cumulative ( .false. )  !
    ! This option ( .false. ) is designed to reduce the sensitivity to the vertical resolution. !
    ! ----------------------------------------------------------------------------------------- !

    logical , parameter              :: use_cumpenent = .true.

    ! --------------------------------------------------------------------------------------------------------------- !
    ! Computation of the grid-mean condensate tendency.                                                               !
    !     use_expconten = .true.  : explcitly compute tendency by condensate detrainment and compensating subsidence  !
    !     use_expconten = .false. : use the original proportional condensate tendency equation. ( original )          !
    ! --------------------------------------------------------------------------------------------------------------- !

    logical , parameter              :: use_expconten = .true.

    ! --------------------------------------------------------------------------------------------------------------- !
    ! Treatment of reserved condensate                                                                                !
    !     use_unicondet = .true.  : detrain condensate uniformly over the environment ( original )                    !
    !     use_unicondet = .false. : detrain condensate into the pre-existing stratus                                  !
    ! --------------------------------------------------------------------------------------------------------------- !

    logical , parameter              :: use_unicondet = .false.

    ! ----------------------- !
    ! For lateral entrainment !
    ! ----------------------- !

    parameter (rle = 0.1_r8)         !  For critical stopping distance for lateral entrainment [no unit]
!   parameter (rkm = 16.0_r8)        !  Determine the amount of air that is involved in buoyancy-sorting [no unit] 
    parameter (rkm = 14.0_r8)        !  Determine the amount of air that is involved in buoyancy-sorting [no unit]

    parameter (rkfre = 1.0_r8)       !  Vertical velocity variance as fraction of  tke. 
    parameter (rmaxfrac = 0.10_r8)   !  Maximum allowable 'core' updraft fraction
    parameter (mumin1 = 0.906_r8)    !  Normalized CIN ('mu') corresponding to 'rmaxfrac' at the PBL top
                                     !  obtaind by inverting 'rmaxfrac = 0.5*erfc(mumin1)'.
                                     !  [ rmaxfrac:mumin1 ] = [ 0.05:1.163, 0.075:1.018, 0.1:0.906, 0.15:0.733, 0.2:0.595, 0.25:0.477 ] 
    parameter (rbuoy = 1.0_r8)       !  For nonhydrostatic pressure effects on updraft [no unit]
    parameter (rdrag = 1.0_r8)       !  Drag coefficient [no unit]

    parameter (epsvarw = 5.e-4_r8)   !  Variance of w at PBL top by meso-scale component [m2/s2]          
    parameter (PGFc = 0.7_r8)        !  This is used for calculating vertical variations cumulus  
                                     !  'u' & 'v' by horizontal PGF during upward motion [no unit]

    ! ---------------------------------------- !
    ! Bulk microphysics controlling parameters !
    ! --------------------------------------------------------------------------- ! 
    ! criqc    : Maximum condensate that can be hold by cumulus updraft [kg/kg]   !
    ! frc_rasn : Fraction of precipitable condensate in the expelled cloud water  !
    !            from cumulus updraft. The remaining fraction ('1-frc_rasn')  is  !
    !            'suspended condensate'.                                          !
    !                0 : all expelled condensate is 'suspended condensate'        ! 
    !                1 : all expelled condensate is 'precipitable condensate'     !
    ! kevp     : Evaporative efficiency                                           !
    ! noevap_krelkpen : No evaporation from 'krel' to 'kpen' layers               ! 
    ! --------------------------------------------------------------------------- !    

    parameter ( criqc    = 0.7e-3_r8 ) 
    parameter ( frc_rasn = 1.0_r8    )
    parameter ( kevp     = 2.e-6_r8  )
    logical, parameter :: noevap_krelkpen = .false.

    !------------------------!
    !                        !
    ! Start Main Calculation !
    !                        !
    !------------------------!

    call cnst_get_ind( 'NUMLIQ', ixnumliq )
    call cnst_get_ind( 'NUMICE', ixnumice )

    ! ------------------------------------------------------- !
    ! Initialize output variables defined for all grid points !
    ! ------------------------------------------------------- !

    umf_out(:iend,0:mkx)         = 0.0_r8
    slflx_out(:iend,0:mkx)       = 0.0_r8
    qtflx_out(:iend,0:mkx)       = 0.0_r8
    flxprc1_out(:iend,0:mkx)     = 0.0_r8
    flxsnow1_out(:iend,0:mkx)    = 0.0_r8
    qvten_out(:iend,:mkx)        = 0.0_r8
    qlten_out(:iend,:mkx)        = 0.0_r8
    qiten_out(:iend,:mkx)        = 0.0_r8
    sten_out(:iend,:mkx)         = 0.0_r8
    uten_out(:iend,:mkx)         = 0.0_r8
    vten_out(:iend,:mkx)         = 0.0_r8
    qrten_out(:iend,:mkx)        = 0.0_r8
    qsten_out(:iend,:mkx)        = 0.0_r8
    precip_out(:iend)            = 0.0_r8
    snow_out(:iend)              = 0.0_r8
    evapc_out(:iend,:mkx)        = 0.0_r8
    cufrc_out(:iend,:mkx)        = 0.0_r8
    qcu_out(:iend,:mkx)          = 0.0_r8
    qlu_out(:iend,:mkx)          = 0.0_r8
    qiu_out(:iend,:mkx)          = 0.0_r8
    fer_out(:iend,:mkx)          = 0.0_r8
    fdr_out(:iend,:mkx)          = 0.0_r8
    cinh_out(:iend)              = -1.0_r8
    cinlclh_out(:iend)           = -1.0_r8
    cbmf_out(:iend)              = 0.0_r8
    qc_out(:iend,:mkx)           = 0.0_r8
    rliq_out(:iend)              = 0.0_r8
    cnt_out(:iend)               = real(mkx, r8)
    cnb_out(:iend)               = 0.0_r8
    qtten_out(:iend,:mkx)        = 0.0_r8
    slten_out(:iend,:mkx)        = 0.0_r8
    ufrc_out(:iend,0:mkx)        = 0.0_r8

    uflx_out(:iend,0:mkx)        = 0.0_r8
    vflx_out(:iend,0:mkx)        = 0.0_r8

    trten_out(:iend,:mkx,:ncnst) = 0.0_r8
    trflx_out(:iend,0:mkx,:ncnst)= 0.0_r8
    
    ufrcinvbase_out(:iend)       = 0.0_r8
    ufrclcl_out(:iend)           = 0.0_r8
    winvbase_out(:iend)          = 0.0_r8
    wlcl_out(:iend)              = 0.0_r8
    plcl_out(:iend)              = 0.0_r8
    pinv_out(:iend)              = 0.0_r8
    plfc_out(:iend)              = 0.0_r8
    pbup_out(:iend)              = 0.0_r8
    ppen_out(:iend)              = 0.0_r8
    qtsrc_out(:iend)             = 0.0_r8
    thlsrc_out(:iend)            = 0.0_r8
    thvlsrc_out(:iend)           = 0.0_r8
    emfkbup_out(:iend)           = 0.0_r8
    cbmflimit_out(:iend)         = 0.0_r8
    tkeavg_out(:iend)            = 0.0_r8
    zinv_out(:iend)              = 0.0_r8
    rcwp_out(:iend)              = 0.0_r8
    rlwp_out(:iend)              = 0.0_r8
    riwp_out(:iend)              = 0.0_r8

    wu_out(:iend,0:mkx)          = 0.0_r8
    qtu_out(:iend,0:mkx)         = 0.0_r8
    thlu_out(:iend,0:mkx)        = 0.0_r8
    thvu_out(:iend,0:mkx)        = 0.0_r8
    uu_out(:iend,0:mkx)          = 0.0_r8
    vu_out(:iend,0:mkx)          = 0.0_r8
    qtu_emf_out(:iend,0:mkx)     = 0.0_r8
    thlu_emf_out(:iend,0:mkx)    = 0.0_r8
    uu_emf_out(:iend,0:mkx)      = 0.0_r8
    vu_emf_out(:iend,0:mkx)      = 0.0_r8
    uemf_out(:iend,0:mkx)        = 0.0_r8

    tru_out(:iend,0:mkx,:ncnst)     = 0.0_r8
    tru_emf_out(:iend,0:mkx,:ncnst) = 0.0_r8

    dwten_out(:iend,:mkx)        = 0.0_r8
    diten_out(:iend,:mkx)        = 0.0_r8
    flxrain_out(:iend,0:mkx)     = 0.0_r8  
    flxsnow_out(:iend,0:mkx)     = 0.0_r8
    ntraprd_out(:iend,mkx)       = 0.0_r8
    ntsnprd_out(:iend,mkx)       = 0.0_r8

    excessu_arr_out(:iend,:mkx)  = 0.0_r8
    excess0_arr_out(:iend,:mkx)  = 0.0_r8
    xc_arr_out(:iend,:mkx)       = 0.0_r8
    aquad_arr_out(:iend,:mkx)    = 0.0_r8
    bquad_arr_out(:iend,:mkx)    = 0.0_r8
    cquad_arr_out(:iend,:mkx)    = 0.0_r8
    bogbot_arr_out(:iend,:mkx)   = 0.0_r8
    bogtop_arr_out(:iend,:mkx)   = 0.0_r8

    exit_UWCu(:iend)             = 0.0_r8 
    exit_conden(:iend)           = 0.0_r8 
    exit_klclmkx(:iend)          = 0.0_r8 
    exit_klfcmkx(:iend)          = 0.0_r8 
    exit_ufrc(:iend)             = 0.0_r8 
    exit_wtw(:iend)              = 0.0_r8 
    exit_drycore(:iend)          = 0.0_r8 
    exit_wu(:iend)               = 0.0_r8 
    exit_cufilter(:iend)         = 0.0_r8 
    exit_kinv1(:iend)            = 0.0_r8 
    exit_rei(:iend)              = 0.0_r8 

    limit_shcu(:iend)            = 0.0_r8 
    limit_negcon(:iend)          = 0.0_r8 
    limit_ufrc(:iend)            = 0.0_r8
    limit_ppen(:iend)            = 0.0_r8
    limit_emf(:iend)             = 0.0_r8
    limit_cinlcl(:iend)          = 0.0_r8
    limit_cin(:iend)             = 0.0_r8
    limit_cbmf(:iend)            = 0.0_r8
    limit_rei(:iend)             = 0.0_r8

    ind_delcin(:iend)            = 0.0_r8

    !--------------------------------------------------------------!
    !                                                              !
    ! Start the column i loop where i is a horozontal column index !
    !                                                              !
    !--------------------------------------------------------------!

    ! Compute wet-bulb temperature and specific humidity
    ! for treating evaporation of precipitation.

    call findsp( lchnk, iend, qv0_in, t0_in, p0_in, tw0_in, qw0_in )

    do i = 1, iend                                      

      id_exit = .false.

      ! -------------------------------------------- !
      ! Define 1D input variables at each grid point !
      ! -------------------------------------------- !

      ps0(0:mkx)       = ps0_in(i,0:mkx)
      zs0(0:mkx)       = zs0_in(i,0:mkx)
      p0(:mkx)         = p0_in(i,:mkx)
      z0(:mkx)         = z0_in(i,:mkx)
      dp0(:mkx)        = dp0_in(i,:mkx)
      dpdry0(:mkx)     = dpdry0_in(i,:mkx)
      u0(:mkx)         = u0_in(i,:mkx)
      v0(:mkx)         = v0_in(i,:mkx)
      qv0(:mkx)        = qv0_in(i,:mkx)
      ql0(:mkx)        = ql0_in(i,:mkx)
      qi0(:mkx)        = qi0_in(i,:mkx)
      t0(:mkx)         = t0_in(i,:mkx)
      s0(:mkx)         = s0_in(i,:mkx)
      tke(0:mkx)       = tke_in(i,0:mkx)
      cldfrct(:mkx)    = cldfrct_in(i,:mkx)
      concldfrct(:mkx) = concldfrct_in(i,:mkx)
      pblh             = pblh_in(i)
      cush             = cush_inout(i)
      do m = 1, ncnst
         tr0(:mkx,m)   = tr0_in(i,:mkx,m)
      enddo

      ! --------------------------------------------------------- !
      ! Compute other basic thermodynamic variables directly from ! 
      ! the input variables at each grid point                    !
      ! --------------------------------------------------------- !

      !----- 1. Compute internal environmental variables
      
      exn0(:mkx)   = (p0(:mkx)/p00)**rovcp
      exns0(0:mkx) = (ps0(0:mkx)/p00)**rovcp
      qt0(:mkx)    = (qv0(:mkx) + ql0(:mkx) + qi0(:mkx))
      thl0(:mkx)   = (t0(:mkx) - xlv*ql0(:mkx)/cp - xls*qi0(:mkx)/cp)/exn0(:mkx)
      thvl0(:mkx)  = (1._r8 + zvir*qt0(:mkx))*thl0(:mkx)

      !----- 2. Compute slopes of environmental variables in each layer
      !         Dimension of ssthl0(:mkx) is implicit.

      ssthl0       = slope(mkx,thl0,p0) 
      ssqt0        = slope(mkx,qt0 ,p0)
      ssu0         = slope(mkx,u0  ,p0)
      ssv0         = slope(mkx,v0  ,p0)
      do m = 1, ncnst
         sstr0(:mkx,m) = slope(mkx,tr0(:mkx,m),p0)
      enddo     
 
      !----- 3. Compute "thv0" and "thvl0" at the top/bottom interfaces in each layer
      !         There are computed from the reconstructed thl, qt at the top/bottom.

      do k = 1, mkx

         thl0bot = thl0(k) + ssthl0(k)*(ps0(k-1) - p0(k))
         qt0bot  = qt0(k)  + ssqt0(k) *(ps0(k-1) - p0(k))
         call conden(ps0(k-1),thl0bot,qt0bot,thj,qvj,qlj,qij,qse,id_check,qsat)
         if( id_check .eq. 1 ) then
             exit_conden(i) = 1._r8
             id_exit = .true.
             go to 333
         end if
         thv0bot(k)  = thj*(1._r8 + zvir*qvj - qlj - qij)
         thvl0bot(k) = thl0bot*(1._r8 + zvir*qt0bot)
          
         thl0top = thl0(k) + ssthl0(k)*(ps0(k) - p0(k))
         qt0top  =  qt0(k) + ssqt0(k) *(ps0(k) - p0(k))
         call conden(ps0(k),thl0top,qt0top,thj,qvj,qlj,qij,qse,id_check,qsat)
         if( id_check .eq. 1 ) then
             exit_conden(i) = 1._r8
             id_exit = .true.
             go to 333
         end if 
         thv0top(k)  = thj*(1._r8 + zvir*qvj - qlj - qij)
         thvl0top(k) = thl0top*(1._r8 + zvir*qt0top)

      end do

      ! ------------------------------------------------------------ !
      ! Save input and related environmental thermodynamic variables !
      ! for use at "iter_cin=2" when "del_CIN >= 0"                  !
      ! ------------------------------------------------------------ !

      qv0_o(:mkx)          = qv0(:mkx)
      ql0_o(:mkx)          = ql0(:mkx)
      qi0_o(:mkx)          = qi0(:mkx)
      t0_o(:mkx)           = t0(:mkx)
      s0_o(:mkx)           = s0(:mkx)
      u0_o(:mkx)           = u0(:mkx)
      v0_o(:mkx)           = v0(:mkx)
      qt0_o(:mkx)          = qt0(:mkx)
      thl0_o(:mkx)         = thl0(:mkx)
      thvl0_o(:mkx)        = thvl0(:mkx)
      ssthl0_o(:mkx)       = ssthl0(:mkx)
      ssqt0_o(:mkx)        = ssqt0(:mkx)
      thv0bot_o(:mkx)      = thv0bot(:mkx)
      thv0top_o(:mkx)      = thv0top(:mkx)
      thvl0bot_o(:mkx)     = thvl0bot(:mkx)
      thvl0top_o(:mkx)     = thvl0top(:mkx)
      ssu0_o(:mkx)         = ssu0(:mkx) 
      ssv0_o(:mkx)         = ssv0(:mkx) 
      do m = 1, ncnst
         tr0_o(:mkx,m)     = tr0(:mkx,m)
         sstr0_o(:mkx,m)   = sstr0(:mkx,m)
      enddo 

      ! ---------------------------------------------- !
      ! Initialize output variables at each grid point !
      ! ---------------------------------------------- !

      umf(0:mkx)          = 0.0_r8
      emf(0:mkx)          = 0.0_r8
      slflx(0:mkx)        = 0.0_r8
      qtflx(0:mkx)        = 0.0_r8
      uflx(0:mkx)         = 0.0_r8
      vflx(0:mkx)         = 0.0_r8
      qvten(:mkx)         = 0.0_r8
      qlten(:mkx)         = 0.0_r8
      qiten(:mkx)         = 0.0_r8
      sten(:mkx)          = 0.0_r8
      uten(:mkx)          = 0.0_r8
      vten(:mkx)          = 0.0_r8
      qrten(:mkx)         = 0.0_r8
      qsten(:mkx)         = 0.0_r8
      dwten(:mkx)         = 0.0_r8
      diten(:mkx)         = 0.0_r8
      precip              = 0.0_r8
      snow                = 0.0_r8
      evapc(:mkx)         = 0.0_r8
      cufrc(:mkx)         = 0.0_r8
      qcu(:mkx)           = 0.0_r8
      qlu(:mkx)           = 0.0_r8
      qiu(:mkx)           = 0.0_r8
      fer(:mkx)           = 0.0_r8
      fdr(:mkx)           = 0.0_r8
      cin                 = 0.0_r8
      cbmf                = 0.0_r8
      qc(:mkx)            = 0.0_r8
      qc_l(:mkx)          = 0.0_r8
      qc_i(:mkx)          = 0.0_r8
      rliq                = 0.0_r8
      cnt                 = real(mkx, r8)
      cnb                 = 0.0_r8
      qtten(:mkx)         = 0.0_r8
      slten(:mkx)         = 0.0_r8   
      ufrc(0:mkx)         = 0.0_r8  

      thlu(0:mkx)         = 0.0_r8
      qtu(0:mkx)          = 0.0_r8
      uu(0:mkx)           = 0.0_r8
      vu(0:mkx)           = 0.0_r8
      wu(0:mkx)           = 0.0_r8
      thvu(0:mkx)         = 0.0_r8
      thlu_emf(0:mkx)     = 0.0_r8
      qtu_emf(0:mkx)      = 0.0_r8
      uu_emf(0:mkx)       = 0.0_r8
      vu_emf(0:mkx)       = 0.0_r8
      
      ufrcinvbase         = 0.0_r8
      ufrclcl             = 0.0_r8
      winvbase            = 0.0_r8
      wlcl                = 0.0_r8
      emfkbup             = 0.0_r8 
      cbmflimit           = 0.0_r8
      excessu_arr(:mkx)   = 0.0_r8
      excess0_arr(:mkx)   = 0.0_r8
      xc_arr(:mkx)        = 0.0_r8
      aquad_arr(:mkx)     = 0.0_r8
      bquad_arr(:mkx)     = 0.0_r8
      cquad_arr(:mkx)     = 0.0_r8
      bogbot_arr(:mkx)    = 0.0_r8
      bogtop_arr(:mkx)    = 0.0_r8

      uemf(0:mkx)         = 0.0_r8
      comsub(:mkx)        = 0.0_r8
      qlten_sink(:mkx)    = 0.0_r8
      qiten_sink(:mkx)    = 0.0_r8 
      nlten_sink(:mkx)    = 0.0_r8
      niten_sink(:mkx)    = 0.0_r8 

      do m = 1, ncnst
         trflx(0:mkx,m)   = 0.0_r8
         trten(:mkx,m)    = 0.0_r8
         tru(0:mkx,m)     = 0.0_r8
         tru_emf(0:mkx,m) = 0.0_r8
      enddo

    !-----------------------------------------------! 
    ! Below 'iter' loop is for implicit CIN closure !
    !-----------------------------------------------!

    ! ----------------------------------------------------------------------------- ! 
    ! It is important to note that this iterative cin loop is located at the outest !
    ! shell of the code. Thus, source air properties can also be changed during the !
    ! iterative cin calculation, because cumulus convection induces non-zero fluxes !
    ! even at interfaces below PBL top height through 'fluxbelowinv' subroutine.    !
    ! ----------------------------------------------------------------------------- !

    do iter = 1, iter_cin

       ! ---------------------------------------------------------------------- ! 
       ! Cumulus scale height                                                   ! 
       ! In contrast to the premitive code, cumulus scale height is iteratively !
       ! calculated at each time step, and at each iterative cin step.          !
       ! It is not clear whether I should locate below two lines within or  out !
       ! of the iterative cin loop.                                             !
       ! ---------------------------------------------------------------------- !

       tscaleh = cush                        
       cush    = -1._r8

       ! ----------------------------------------------------------------------- !
       ! Find PBL top height interface index, 'kinv-1' where 'kinv' is the layer !
       ! index with PBLH in it. When PBLH is exactly at interface, 'kinv' is the !
       ! layer index having PBLH as a lower interface.                           !
       ! In the previous code, I set the lower limit of 'kinv' by 2  in order to !
       ! be consistent with the other parts of the code. However in the modified !
       ! code, I allowed 'kinv' to be 1 & if 'kinv = 1', I just exit the program !
       ! without performing cumulus convection. This new approach seems to be    !
       ! more reasonable: if PBL height is within 'kinv=1' layer, surface is STL !
       ! interface (bflxs <= 0) and interface just above the surface should be   !
       ! either non-turbulent (Ri>0.19) or stably turbulent (0<=Ri<0.19 but this !
       ! interface is identified as a base external interface of upperlying CL.  !
       ! Thus, when 'kinv=1', PBL scheme guarantees 'bflxs <= 0'.  For this case !
       ! it is reasonable to assume that cumulus convection does not happen.     !
       ! When these is SBCL, PBL height from the PBL scheme is likely to be very !
       ! close at 'kinv-1' interface, but not exactly, since 'zi' information is !
       ! changed between two model time steps. In order to ensure correct identi !
       ! fication of 'kinv' for general case including SBCL, I imposed an offset !
       ! of 5 [m] in the below 'kinv' finding block.                             !
       ! ----------------------------------------------------------------------- !
       
       do k = mkx - 1, 1, -1 
          if( (pblh + 5._r8 - zs0(k))*(pblh + 5._r8 - zs0(k+1)) .lt. 0._r8 ) then
               kinv = k + 1 
               go to 15
          endif 
       end do
       kinv = 1
15     continue    

       if( kinv .le. 1 ) then          
           exit_kinv1(i) = 1._r8
           id_exit = .true.
           go to 333
       endif
       ! From here, it must be 'kinv >= 2'.

       ! -------------------------------------------------------------------------- !
       ! Find PBL averaged tke ('tkeavg') and minimum 'thvl' ('thvlmin') in the PBL !
       ! In the current code, 'tkeavg' is obtained by averaging all interfacial TKE !
       ! within the PBL. However, in order to be conceptually consistent with   PBL !
       ! scheme, 'tkeavg' should be calculated by considering surface buoyancy flux.!
       ! If surface buoyancy flux is positive ( bflxs >0 ), surface interfacial TKE !
       ! should be included in calculating 'tkeavg', while if bflxs <= 0,   surface !
       ! interfacial TKE should not be included in calculating 'tkeavg'.   I should !
       ! modify the code when 'bflxs' is available as an input of cumulus scheme.   !
       ! 'thvlmin' is a minimum 'thvl' within PBL obtained by comparing top &  base !
       ! interface values of 'thvl' in each layers within the PBL.                  !
       ! -------------------------------------------------------------------------- !
       
       dpsum    = 0._r8
       tkeavg   = 0._r8
       thvlmin  = 1000._r8
       do k = 0, kinv - 1   ! Here, 'k' is an interfacial layer index.  
          if( k .eq. 0 ) then
              dpi = ps0(0) - p0(1)
          elseif( k .eq. (kinv-1) ) then 
              dpi = p0(kinv-1) - ps0(kinv-1)
          else
              dpi = p0(k) - p0(k+1)
          endif 
          dpsum  = dpsum  + dpi  
          tkeavg = tkeavg + dpi*tke(k) 
          if( k .ne. 0 ) thvlmin = min(thvlmin,min(thvl0bot(k),thvl0top(k)))
       end do
       tkeavg  = tkeavg/dpsum

       ! ------------------------------------------------------------------ !
       ! Find characteristics of cumulus source air: qtsrc,thlsrc,usrc,vsrc !
       ! Note that 'thlsrc' was con-cocked using 'thvlsrc' and 'qtsrc'.     !
       ! 'qtsrc' is defined as the lowest layer mid-point value;   'thlsrc' !
       ! is from 'qtsrc' and 'thvlmin=thvlsrc'; 'usrc' & 'vsrc' are defined !
       ! as the values just below the PBL top interface.                    !
       ! ------------------------------------------------------------------ !

       qtsrc   = qt0(1)                     
       thvlsrc = thvlmin 
       thlsrc  = thvlsrc / ( 1._r8 + zvir * qtsrc )  
       usrc    = u0(kinv-1) + ssu0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )             
       vsrc    = v0(kinv-1) + ssv0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )             
       do m = 1, ncnst
          trsrc(m) = tr0(1,m)
       enddo 

       ! ------------------------------------------------------------------ !
       ! Find LCL of the source air and a layer index containing LCL (klcl) !
       ! When the LCL is exactly at the interface, 'klcl' is a layer index  ! 
       ! having 'plcl' as the lower interface similar to the 'kinv' case.   !
       ! In the previous code, I assumed that if LCL is located within the  !
       ! lowest model layer ( 1 ) or the top model layer ( mkx ), then  no  !
       ! convective adjustment is performed and just exited.   However, in  !
       ! the revised code, I relaxed the first constraint and  even though  !
       ! LCL is at the lowest model layer, I allowed cumulus convection to  !
       ! be initiated. For this case, cumulus convection should be started  !
       ! from the PBL top height, as shown in the following code.           !
       ! When source air is already saturated even at the surface, klcl is  !
       ! set to 1.                                                          !
       ! ------------------------------------------------------------------ !

       plcl = qsinvert(qtsrc,thlsrc,ps0(0),qsat)
       do k = 0, mkx
          if( ps0(k) .lt. plcl ) then
              klcl = k
              go to 25
          endif           
       end do
       klcl = mkx
25     continue
       klcl = max(1,klcl)
     
       if( plcl .lt. 30000._r8 ) then               
     ! if( klcl .eq. mkx ) then          
           exit_klclmkx(i) = 1._r8
           id_exit = .true.
           go to 333
       endif

       ! ------------------------------------------------------------- !
       ! Calculate environmental virtual potential temperature at LCL, !
       !'thv0lcl' which is solely used in the 'cin' calculation. Note  !
       ! that 'thv0lcl' is calculated first by calculating  'thl0lcl'  !
       ! and 'qt0lcl' at the LCL, and performing 'conden' afterward,   !
       ! in fully consistent with the other parts of the code.         !
       ! ------------------------------------------------------------- !

       thl0lcl = thl0(klcl) + ssthl0(klcl) * ( plcl - p0(klcl) )
       qt0lcl  = qt0(klcl)  + ssqt0(klcl)  * ( plcl - p0(klcl) )
       call conden(plcl,thl0lcl,qt0lcl,thj,qvj,qlj,qij,qse,id_check,qsat)
       if( id_check .eq. 1 ) then
           exit_conden(i) = 1._r8
           id_exit = .true.
           go to 333
       end if
       thv0lcl = thj * ( 1._r8 + zvir * qvj - qlj - qij )

       ! ------------------------------------------------------------------------ !
       ! Compute Convective Inhibition, 'cin' & 'cinlcl' [J/kg]=[m2/s2] TKE unit. !
       !                                                                          !
       ! 'cin' (cinlcl) is computed from the PBL top interface to LFC (LCL) using ! 
       ! piecewisely reconstructed environmental profiles, assuming environmental !
       ! buoyancy profile within each layer ( or from LCL to upper interface in   !
       ! each layer ) is simply a linear profile. For the purpose of cin (cinlcl) !
       ! calculation, we simply assume that lateral entrainment does not occur in !
       ! updrafting cumulus plume, i.e., cumulus source air property is conserved.!
       ! Below explains some rules used in the calculations of cin (cinlcl).   In !
       ! general, both 'cin' and 'cinlcl' are calculated from a PBL top interface !
       ! to LCL and LFC, respectively :                                           !
       ! 1. If LCL is lower than the PBL height, cinlcl = 0 and cin is calculated !
       !    from PBL height to LFC.                                               !
       ! 2. If LCL is higher than PBL height,   'cinlcl' is calculated by summing !
       !    both positive and negative cloud buoyancy up to LCL using 'single_cin'!
       !    From the LCL to LFC, however, only negative cloud buoyancy is counted !
       !    to calculate final 'cin' upto LFC.                                    !
       ! 3. If either 'cin' or 'cinlcl' is negative, they are set to be zero.     !
       ! In the below code, 'klfc' is the layer index containing 'LFC' similar to !
       ! 'kinv' and 'klcl'.                                                       !
       ! ------------------------------------------------------------------------ !

        cin    = 0._r8
        cinlcl = 0._r8
        plfc   = 0._r8
        klfc   = mkx

        ! ------------------------------------------------------------------------- !
        ! Case 1. LCL height is higher than PBL interface ( 'pLCL <= ps0(kinv-1)' ) !
        ! ------------------------------------------------------------------------- !

        if( klcl .ge. kinv ) then

            do k = kinv, mkx - 1
               if( k .lt. klcl ) then
                   thvubot = thvlsrc
                   thvutop = thvlsrc  
                   cin     = cin + single_cin(ps0(k-1),thv0bot(k),ps0(k),thv0top(k),thvubot,thvutop)
               elseif( k .eq. klcl ) then
                   !----- Bottom to LCL
                   thvubot = thvlsrc
                   thvutop = thvlsrc
                   cin     = cin + single_cin(ps0(k-1),thv0bot(k),plcl,thv0lcl,thvubot,thvutop)
                   if( cin .lt. 0._r8 ) limit_cinlcl(i) = 1._r8
                   cinlcl  = max(cin,0._r8)
                   cin     = cinlcl
                   !----- LCL to Top
                   thvubot = thvlsrc
                   call conden(ps0(k),thlsrc,qtsrc,thj,qvj,qlj,qij,qse,id_check,qsat)
                   if( id_check .eq. 1 ) then
                       exit_conden(i) = 1._r8
                       id_exit = .true.
                       go to 333
                   end if
                   thvutop = thj * ( 1._r8 + zvir*qvj - qlj - qij )
                   call getbuoy(plcl,thv0lcl,ps0(k),thv0top(k),thvubot,thvutop,plfc,cin)
                   if( plfc .gt. 0._r8 ) then 
                       klfc = k 
                       go to 35
                   end if
               else
                   thvubot = thvutop
                   call conden(ps0(k),thlsrc,qtsrc,thj,qvj,qlj,qij,qse,id_check,qsat)
                   if( id_check .eq. 1 ) then
                       exit_conden(i) = 1._r8
                       id_exit = .true.
                       go to 333
                   end if
                   thvutop = thj * ( 1._r8 + zvir*qvj - qlj - qij )
                   call getbuoy(ps0(k-1),thv0bot(k),ps0(k),thv0top(k),thvubot,thvutop,plfc,cin)
                   if( plfc .gt. 0._r8 ) then 
                       klfc = k
                       go to 35
                   end if 
               endif
            end do        

       ! ----------------------------------------------------------------------- !
       ! Case 2. LCL height is lower than PBL interface ( 'pLCL > ps0(kinv-1)' ) !
       ! ----------------------------------------------------------------------- !

       else
          cinlcl = 0._r8 
          do k = kinv, mkx - 1
             call conden(ps0(k-1),thlsrc,qtsrc,thj,qvj,qlj,qij,qse,id_check,qsat)
             if( id_check .eq. 1 ) then
                 exit_conden(i) = 1._r8
                 id_exit = .true.
                 go to 333
             end if
             thvubot = thj * ( 1._r8 + zvir*qvj - qlj - qij )
             call conden(ps0(k),thlsrc,qtsrc,thj,qvj,qlj,qij,qse,id_check,qsat)
             if( id_check .eq. 1 ) then
                 exit_conden(i) = 1._r8
                 id_exit = .true.
                 go to 333
             end if
             thvutop = thj * ( 1._r8 + zvir*qvj - qlj - qij )
             call getbuoy(ps0(k-1),thv0bot(k),ps0(k),thv0top(k),thvubot,thvutop,plfc,cin)
             if( plfc .gt. 0._r8 ) then 
                 klfc = k
                 go to 35
             end if 
          end do
       endif  ! End of CIN case selection

 35    continue
       if( cin .lt. 0._r8 ) limit_cin(i) = 1._r8
       cin = max(0._r8,cin)
       if( klfc .ge. mkx ) then
           klfc = mkx
         ! write(iulog,*) 'klfc >= mkx'
           exit_klfcmkx(i) = 1._r8
           id_exit = .true.
           go to 333
       endif

       ! ---------------------------------------------------------------------- !
       ! In order to calculate implicit 'cin' (or 'cinlcl'), save the initially !
       ! calculated 'cin' and 'cinlcl', and other related variables. These will !
       ! be restored after calculating implicit CIN.                            !
       ! ---------------------------------------------------------------------- !

       if( iter .eq. 1 ) then 
           cin_i       = cin
           cinlcl_i    = cinlcl
           ke          = rbuoy / ( rkfre * tkeavg + epsvarw ) 
           kinv_o      = kinv     
           klcl_o      = klcl     
           klfc_o      = klfc    
           plcl_o      = plcl    
           plfc_o      = plfc     
           tkeavg_o    = tkeavg   
           thvlmin_o   = thvlmin
           qtsrc_o     = qtsrc    
           thvlsrc_o   = thvlsrc  
           thlsrc_o    = thlsrc
           usrc_o      = usrc     
           vsrc_o      = vsrc     
           thv0lcl_o   = thv0lcl  
           do m = 1, ncnst
              trsrc_o(m) = trsrc(m)
           enddo
       endif   

     ! Modification : If I impose w = max(0.1_r8, w) up to the top interface of
     !                klfc, I should only use cinlfc.  That is, if I want to
     !                use cinlcl, I should not impose w = max(0.1_r8, w).
     !                Using cinlcl is equivalent to treating only 'saturated'
     !                moist convection. Note that in this sense, I should keep
     !                the functionality of both cinlfc and cinlcl.
     !                However, the treatment of penetrative entrainment level becomes
     !                ambiguous if I choose 'cinlcl'. Thus, the best option is to use
     !                'cinlfc'.

       ! -------------------------------------------------------------------------- !
       ! Calculate implicit 'cin' by averaging initial and final cins.    Note that !
       ! implicit CIN is adopted only when cumulus convection stabilized the system,!
       ! i.e., only when 'del_CIN >0'. If 'del_CIN<=0', just use explicit CIN. Note !
       ! also that since 'cinlcl' is set to zero whenever LCL is below the PBL top, !
       ! (see above CIN calculation part), the use of 'implicit CIN=cinlcl'  is not !
       ! good. Thus, when using implicit CIN, always try to only use 'implicit CIN= !
       ! cin', not 'implicit CIN=cinlcl'. However, both 'CIN=cin' and 'CIN=cinlcl'  !
       ! are good when using explicit CIN.                                          !
       ! -------------------------------------------------------------------------- !

       if( iter .ne. 1 ) then

           cin_f = cin
           cinlcl_f = cinlcl
           if( use_CINcin ) then
               del_CIN = cin_f - cin_i
           else
               del_CIN = cinlcl_f - cinlcl_i
           endif

           if( del_CIN .gt. 0._r8 ) then

               ! -------------------------------------------------------------- ! 
               ! Calculate implicit 'cin' and 'cinlcl'. Note that when we chose !
               ! to use 'implicit CIN = cin', choose 'cinlcl = cinlcl_i' below: !
               ! because iterative CIN only aims to obtain implicit CIN,  once  !
               ! we obtained 'implicit CIN=cin', it is good to use the original !
               ! profiles information for all the other variables after that.   !
               ! Note 'cinlcl' will be explicitly used in calculating  'wlcl' & !
               ! 'ufrclcl' after calculating 'winv' & 'ufrcinv'  at the PBL top !
               ! interface later, after calculating 'cbmf'.                     !
               ! -------------------------------------------------------------- !
         
               alpha = compute_alpha( del_CIN, ke ) 
               cin   = cin_i + alpha * del_CIN
               if( use_CINcin ) then
                   cinlcl = cinlcl_i                 
               else
                   cinlcl = cinlcl_i + alpha * del_cinlcl   
               endif

               ! ----------------------------------------------------------------- !
               ! Restore the original values from the previous 'iter_cin' step (1) !
               ! to compute correct tendencies for (n+1) time step by implicit CIN !
               ! ----------------------------------------------------------------- !

               kinv      = kinv_o     
               klcl      = klcl_o     
               klfc      = klfc_o    
               plcl      = plcl_o    
               plfc      = plfc_o     
               tkeavg    = tkeavg_o   
               thvlmin   = thvlmin_o
               qtsrc     = qtsrc_o    
               thvlsrc   = thvlsrc_o  
               thlsrc    = thlsrc_o
               usrc      = usrc_o     
               vsrc      = vsrc_o     
               thv0lcl   = thv0lcl_o  
               do m = 1, ncnst
                  trsrc(m) = trsrc_o(m)
               enddo

               qv0(:mkx)            = qv0_o(:mkx)
               ql0(:mkx)            = ql0_o(:mkx)
               qi0(:mkx)            = qi0_o(:mkx)
               t0(:mkx)             = t0_o(:mkx)
               s0(:mkx)             = s0_o(:mkx)
               u0(:mkx)             = u0_o(:mkx)
               v0(:mkx)             = v0_o(:mkx)
               qt0(:mkx)            = qt0_o(:mkx)
               thl0(:mkx)           = thl0_o(:mkx)
               thvl0(:mkx)          = thvl0_o(:mkx)
               ssthl0(:mkx)         = ssthl0_o(:mkx)
               ssqt0(:mkx)          = ssqt0_o(:mkx)
               thv0bot(:mkx)        = thv0bot_o(:mkx)
               thv0top(:mkx)        = thv0top_o(:mkx)
               thvl0bot(:mkx)       = thvl0bot_o(:mkx)
               thvl0top(:mkx)       = thvl0top_o(:mkx)
               ssu0(:mkx)           = ssu0_o(:mkx) 
               ssv0(:mkx)           = ssv0_o(:mkx) 
               do m = 1, ncnst
                  tr0(:mkx,m)   = tr0_o(:mkx,m)
                  sstr0(:mkx,m) = sstr0_o(:mkx,m)
               enddo

               ! ------------------------------------------------------ !
               ! Initialize all fluxes, tendencies, and other variables ! 
               ! in association with cumulus convection.                !
               ! ------------------------------------------------------ ! 

               umf(0:mkx)          = 0.0_r8
               emf(0:mkx)          = 0.0_r8
               slflx(0:mkx)        = 0.0_r8
               qtflx(0:mkx)        = 0.0_r8
               uflx(0:mkx)         = 0.0_r8
               vflx(0:mkx)         = 0.0_r8
               qvten(:mkx)         = 0.0_r8
               qlten(:mkx)         = 0.0_r8
               qiten(:mkx)         = 0.0_r8
               sten(:mkx)          = 0.0_r8
               uten(:mkx)          = 0.0_r8
               vten(:mkx)          = 0.0_r8
               qrten(:mkx)         = 0.0_r8
               qsten(:mkx)         = 0.0_r8
               dwten(:mkx)         = 0.0_r8
               diten(:mkx)         = 0.0_r8
               precip              = 0.0_r8
               snow                = 0.0_r8
               evapc(:mkx)         = 0.0_r8
               cufrc(:mkx)         = 0.0_r8
               qcu(:mkx)           = 0.0_r8
               qlu(:mkx)           = 0.0_r8
               qiu(:mkx)           = 0.0_r8
               fer(:mkx)           = 0.0_r8
               fdr(:mkx)           = 0.0_r8
               qc(:mkx)            = 0.0_r8
               qc_l(:mkx)          = 0.0_r8
               qc_i(:mkx)          = 0.0_r8
               rliq                = 0.0_r8
               cbmf                = 0.0_r8
               cnt                 = real(mkx, r8)
               cnb                 = 0.0_r8
               qtten(:mkx)         = 0.0_r8
               slten(:mkx)         = 0.0_r8
               ufrc(0:mkx)         = 0.0_r8

               thlu(0:mkx)         = 0.0_r8
               qtu(0:mkx)          = 0.0_r8
               uu(0:mkx)           = 0.0_r8
               vu(0:mkx)           = 0.0_r8
               wu(0:mkx)           = 0.0_r8
               thvu(0:mkx)         = 0.0_r8
               thlu_emf(0:mkx)     = 0.0_r8
               qtu_emf(0:mkx)      = 0.0_r8
               uu_emf(0:mkx)       = 0.0_r8
               vu_emf(0:mkx)       = 0.0_r8
             
               do m = 1, ncnst
                  trflx(0:mkx,m)   = 0.0_r8
                  trten(:mkx,m)    = 0.0_r8
                  tru(0:mkx,m)     = 0.0_r8
                  tru_emf(0:mkx,m) = 0.0_r8
               enddo

               ! -------------------------------------------------- !
               ! Below are diagnostic output variables for detailed !
               ! analysis of cumulus scheme.                        !
               ! -------------------------------------------------- ! 

               ufrcinvbase         = 0.0_r8
               ufrclcl             = 0.0_r8
               winvbase            = 0.0_r8
               wlcl                = 0.0_r8
               emfkbup             = 0.0_r8 
               cbmflimit           = 0.0_r8
               excessu_arr(:mkx)   = 0.0_r8
               excess0_arr(:mkx)   = 0.0_r8
               xc_arr(:mkx)        = 0.0_r8
               aquad_arr(:mkx)     = 0.0_r8
               bquad_arr(:mkx)     = 0.0_r8
               cquad_arr(:mkx)     = 0.0_r8
               bogbot_arr(:mkx)    = 0.0_r8
               bogtop_arr(:mkx)    = 0.0_r8

          else ! When 'del_CIN < 0', use explicit CIN instead of implicit CIN.
           
               ! ----------------------------------------------------------- ! 
               ! Identifier showing whether explicit or implicit CIN is used !
               ! ----------------------------------------------------------- ! 

               ind_delcin(i) = 1._r8             
   
               ! --------------------------------------------------------- !
               ! Restore original output values of "iter_cin = 1" and exit !
               ! --------------------------------------------------------- !

               umf_out(i,0:mkx)         = umf_s(0:mkx)
               qvten_out(i,:mkx)        = qvten_s(:mkx)
               qlten_out(i,:mkx)        = qlten_s(:mkx)  
               qiten_out(i,:mkx)        = qiten_s(:mkx)
               sten_out(i,:mkx)         = sten_s(:mkx)
               uten_out(i,:mkx)         = uten_s(:mkx)  
               vten_out(i,:mkx)         = vten_s(:mkx)
               qrten_out(i,:mkx)        = qrten_s(:mkx)
               qsten_out(i,:mkx)        = qsten_s(:mkx)  
               precip_out(i)            = precip_s
               snow_out(i)              = snow_s
               evapc_out(i,:mkx)        = evapc_s(:mkx)
               cush_inout(i)            = cush_s
               cufrc_out(i,:mkx)        = cufrc_s(:mkx)  
               slflx_out(i,0:mkx)       = slflx_s(0:mkx)  
               qtflx_out(i,0:mkx)       = qtflx_s(0:mkx)
               qcu_out(i,:mkx)          = qcu_s(:mkx)    
               qlu_out(i,:mkx)          = qlu_s(:mkx)  
               qiu_out(i,:mkx)          = qiu_s(:mkx)  
               cbmf_out(i)              = cbmf_s
               qc_out(i,:mkx)           = qc_s(:mkx)  
               rliq_out(i)              = rliq_s
               cnt_out(i)               = cnt_s
               cnb_out(i)               = cnb_s
               do m = 1, ncnst
                  trten_out(i,:mkx,m)   = trten_s(:mkx,m)
               enddo  
             
               ! ------------------------------------------------------------------------------ ! 
               ! Below are diagnostic output variables for detailed analysis of cumulus scheme. !
               ! The order of vertical index is reversed for this internal diagnostic output.   !
               ! ------------------------------------------------------------------------------ !   

               fer_out(i,mkx:1:-1)      = fer_s(:mkx)  
               fdr_out(i,mkx:1:-1)      = fdr_s(:mkx)  
               cinh_out(i)              = cin_s
               cinlclh_out(i)           = cinlcl_s
               qtten_out(i,mkx:1:-1)    = qtten_s(:mkx)
               slten_out(i,mkx:1:-1)    = slten_s(:mkx)
               ufrc_out(i,mkx:0:-1)     = ufrc_s(0:mkx)
               uflx_out(i,mkx:0:-1)     = uflx_s(0:mkx)  
               vflx_out(i,mkx:0:-1)     = vflx_s(0:mkx)  

               ufrcinvbase_out(i)       = ufrcinvbase_s
               ufrclcl_out(i)           = ufrclcl_s 
               winvbase_out(i)          = winvbase_s
               wlcl_out(i)              = wlcl_s
               plcl_out(i)              = plcl_s
               pinv_out(i)              = pinv_s    
               plfc_out(i)              = plfc_s    
               pbup_out(i)              = pbup_s
               ppen_out(i)              = ppen_s    
               qtsrc_out(i)             = qtsrc_s
               thlsrc_out(i)            = thlsrc_s
               thvlsrc_out(i)           = thvlsrc_s
               emfkbup_out(i)           = emfkbup_s
               cbmflimit_out(i)         = cbmflimit_s
               tkeavg_out(i)            = tkeavg_s
               zinv_out(i)              = zinv_s
               rcwp_out(i)              = rcwp_s
               rlwp_out(i)              = rlwp_s
               riwp_out(i)              = riwp_s

               wu_out(i,mkx:0:-1)       = wu_s(0:mkx)
               qtu_out(i,mkx:0:-1)      = qtu_s(0:mkx)
               thlu_out(i,mkx:0:-1)     = thlu_s(0:mkx)
               thvu_out(i,mkx:0:-1)     = thvu_s(0:mkx)
               uu_out(i,mkx:0:-1)       = uu_s(0:mkx)
               vu_out(i,mkx:0:-1)       = vu_s(0:mkx)
               qtu_emf_out(i,mkx:0:-1)  = qtu_emf_s(0:mkx)
               thlu_emf_out(i,mkx:0:-1) = thlu_emf_s(0:mkx)
               uu_emf_out(i,mkx:0:-1)   = uu_emf_s(0:mkx)
               vu_emf_out(i,mkx:0:-1)   = vu_emf_s(0:mkx)
               uemf_out(i,mkx:0:-1)     = uemf_s(0:mkx)

               dwten_out(i,mkx:1:-1)    = dwten_s(:mkx)
               diten_out(i,mkx:1:-1)    = diten_s(:mkx)
               flxrain_out(i,mkx:0:-1)  = flxrain_s(0:mkx)
               flxsnow_out(i,mkx:0:-1)  = flxsnow_s(0:mkx)
               ntraprd_out(i,mkx:1:-1)  = ntraprd_s(:mkx)
               ntsnprd_out(i,mkx:1:-1)  = ntsnprd_s(:mkx)

               excessu_arr_out(i,mkx:1:-1)  = excessu_arr_s(:mkx)
               excess0_arr_out(i,mkx:1:-1)  = excess0_arr_s(:mkx)
               xc_arr_out(i,mkx:1:-1)       = xc_arr_s(:mkx)
               aquad_arr_out(i,mkx:1:-1)    = aquad_arr_s(:mkx)
               bquad_arr_out(i,mkx:1:-1)    = bquad_arr_s(:mkx)
               cquad_arr_out(i,mkx:1:-1)    = cquad_arr_s(:mkx)
               bogbot_arr_out(i,mkx:1:-1)   = bogbot_arr_s(:mkx)
               bogtop_arr_out(i,mkx:1:-1)   = bogtop_arr_s(:mkx)

               do m = 1, ncnst
                  trflx_out(i,mkx:0:-1,m)   = trflx_s(0:mkx,m)  
                  tru_out(i,mkx:0:-1,m)     = tru_s(0:mkx,m)
                  tru_emf_out(i,mkx:0:-1,m) = tru_emf_s(0:mkx,m)
               enddo

               id_exit = .false.
               go to 333

          endif

       endif    

       ! ------------------------------------------------------------------ !
       ! Define a release level, 'prel' and release layer, 'krel'.          !
       ! 'prel' is the lowest level from which buoyancy sorting occurs, and !
       ! 'krel' is the layer index containing 'prel' in it, similar to  the !
       ! previous definitions of 'kinv', 'klcl', and 'klfc'.    In order to !
       ! ensure that only PBL scheme works within the PBL,  if LCL is below !
       ! PBL top height, then 'krel = kinv', while if LCL is above  PBL top !
       ! height, then 'krel = klcl'.   Note however that regardless of  the !
       ! definition of 'krel', cumulus convection induces fluxes within PBL !
       ! through 'fluxbelowinv'.  We can make cumulus convection start from !
       ! any level, even within the PBL by appropriately defining 'krel'  & !
       ! 'prel' here. Then it must be accompanied by appropriate definition !
       ! of source air properties, CIN, and re-setting of 'fluxbelowinv', & !
       ! many other stuffs.                                                 !
       ! Note that even when 'prel' is located above the PBL top height, we !
       ! still have cumulus convection between PBL top height and 'prel':   !
       ! we simply assume that no lateral mixing occurs in this range.      !
       ! ------------------------------------------------------------------ !

       if( klcl .lt. kinv ) then
           krel    = kinv
           prel    = ps0(krel-1)
           thv0rel = thv0bot(krel) 
       else
           krel    = klcl
           prel    = plcl 
           thv0rel = thv0lcl
       endif  

       ! --------------------------------------------------------------------------- !
       ! Calculate cumulus base mass flux ('cbmf'), fractional area ('ufrcinv'), and !
       ! and mean vertical velocity (winv) of cumulus updraft at PBL top interface.  !
       ! Also, calculate updraft fractional area (ufrclcl) and vertical velocity  at !
       ! the LCL (wlcl). When LCL is below PBLH, cinlcl = 0 and 'ufrclcl = ufrcinv', !
       ! and 'wlcl = winv.                                                           !
       ! Only updrafts strong enough to overcome CIN can rise over PBL top interface.! 
       ! Thus,  in order to calculate cumulus mass flux at PBL top interface, 'cbmf',!
       ! we need to know 'CIN' ( the strength of potential energy barrier ) and      !
       ! 'sigmaw' ( a standard deviation of updraft vertical velocity at the PBL top !
       ! interface, a measure of turbulentce strength in the PBL ).   Naturally, the !
       ! ratio of these two variables, 'mu' - normalized CIN by TKE- is key variable !
       ! controlling 'cbmf'.  If 'mu' becomes large, only small fraction of updrafts !
       ! with very strong TKE can rise over the PBL - both 'cbmf' and 'ufrc' becomes !
       ! small, but 'winv' becomes large ( this can be easily understood by PDF of w !
       ! at PBL top ).  If 'mu' becomes small, lots of updraft can rise over the PBL !
       ! top - both 'cbmf' and 'ufrc' becomes large, but 'winv' becomes small. Thus, !
       ! all of the key variables associated with cumulus convection  at the PBL top !
       ! - 'cbmf', 'ufrc', 'winv' where 'cbmf = rho*ufrc*winv' - are a unique functi !
       ! ons of 'mu', normalized CIN. Although these are uniquely determined by 'mu',! 
       ! we usually impose two comstraints on 'cbmf' and 'ufrc': (1) because we will !
       ! simply assume that subsidence warming and drying of 'kinv-1' layer in assoc !
       ! iation with 'cbmf' at PBL top interface is confined only in 'kinv-1' layer, !
       ! cbmf must not be larger than the mass within the 'kinv-1' layer. Otherwise, !
       ! instability will occur due to the breaking of stability con. If we consider !
       ! semi-Lagrangian vertical advection scheme and explicitly consider the exten !
       ! t of vertical movement of each layer in association with cumulus mass flux, !
       ! we don't need to impose this constraint. However,  using a  semi-Lagrangian !
       ! scheme is a future research subject. Note that this constraint should be ap !
       ! plied for all interfaces above PBL top as well as PBL top interface.   As a !
       ! result, this 'cbmf' constraint impose a 'lower' limit on mu - 'mumin0'. (2) !
       ! in order for mass flux parameterization - rho*(w'a')= M*(a_c-a_e) - to   be !
       ! valid, cumulus updraft fractional area should be much smaller than 1.    In !
       ! current code, we impose 'rmaxfrac = 0.1 ~ 0.2'   through the whole vertical !
       ! layers where cumulus convection occurs. At the PBL top interface,  the same !
       ! constraint is made by imposing another lower 'lower' limit on mu, 'mumin1'. !
       ! After that, also limit 'ufrclcl' to be smaller than 'rmaxfrac' by 'mumin2'. !
       ! --------------------------------------------------------------------------- !
       
       ! --------------------------------------------------------------------------- !
       ! Calculate normalized CIN, 'mu' satisfying all the three constraints imposed !
       ! on 'cbmf'('mumin0'), 'ufrc' at the PBL top - 'ufrcinv' - ( by 'mumin1' from !
       ! a parameter sentence), and 'ufrc' at the LCL - 'ufrclcl' ( by 'mumin2').    !
       ! Note that 'cbmf' does not change between PBL top and LCL  because we assume !
       ! that buoyancy sorting does not occur when cumulus updraft is unsaturated.   !
       ! --------------------------------------------------------------------------- !
   
       if( use_CINcin ) then       
           wcrit = sqrt( 2._r8 * cin * rbuoy )      
       else
           wcrit = sqrt( 2._r8 * cinlcl * rbuoy )   
       endif
       sigmaw = sqrt( rkfre * tkeavg + epsvarw )
       mu = wcrit/sigmaw/1.4142_r8                  
       if( mu .ge. 3._r8 ) then
         ! write(iulog,*) 'mu >= 3'
           id_exit = .true.
           go to 333
       endif
       rho0inv = ps0(kinv-1)/(r*thv0top(kinv-1)*exns0(kinv-1))
       cbmf = (rho0inv*sigmaw/2.5066_r8)*exp(-mu**2)
       ! 1. 'cbmf' constraint
       cbmflimit = 0.9_r8*dp0(kinv-1)/g/dt
       mumin0 = 0._r8
       if( cbmf .gt. cbmflimit ) mumin0 = sqrt(-log(2.5066_r8*cbmflimit/rho0inv/sigmaw))
       ! 2. 'ufrcinv' constraint
       mu = max(max(mu,mumin0),mumin1)
       ! 3. 'ufrclcl' constraint      
       mulcl = sqrt(2._r8*cinlcl*rbuoy)/1.4142_r8/sigmaw
       mulclstar = sqrt(max(0._r8,2._r8*(exp(-mu**2)/2.5066_r8)**2*(1._r8/erfc(mu)**2-0.25_r8/rmaxfrac**2)))
       if( mulcl .gt. 1.e-8_r8 .and. mulcl .gt. mulclstar ) then
           mumin2 = compute_mumin2(mulcl,rmaxfrac,mu)
           if( mu .gt. mumin2 ) then
               write(iulog,*) 'Critical error in mu calculation in UW_ShCu'
#ifdef WRF_PORT
               call wrf_message(iulog)
#endif
               call endrun
           endif
           mu = max(mu,mumin2)
           if( mu .eq. mumin2 ) limit_ufrc(i) = 1._r8
       endif
       if( mu .eq. mumin0 ) limit_cbmf(i) = 1._r8
       if( mu .eq. mumin1 ) limit_ufrc(i) = 1._r8

       ! ------------------------------------------------------------------- !    
       ! Calculate final ['cbmf','ufrcinv','winv'] at the PBL top interface. !
       ! Note that final 'cbmf' here is obtained in such that 'ufrcinv' and  !
       ! 'ufrclcl' are smaller than ufrcmax with no instability.             !
       ! ------------------------------------------------------------------- !

       cbmf = (rho0inv*sigmaw/2.5066_r8)*exp(-mu**2)                       
       winv = sigmaw*(2._r8/2.5066_r8)*exp(-mu**2)/erfc(mu)
       ufrcinv = cbmf/winv/rho0inv

       ! ------------------------------------------------------------------- !
       ! Calculate ['ufrclcl','wlcl'] at the LCL. When LCL is below PBL top, !
       ! it automatically becomes 'ufrclcl = ufrcinv' & 'wlcl = winv', since !
       ! it was already set to 'cinlcl=0' if LCL is below PBL top interface. !
       ! Note 'cbmf' at the PBL top is the same as 'cbmf' at the LCL.  Note  !
       ! also that final 'cbmf' here is obtained in such that 'ufrcinv' and  !
       ! 'ufrclcl' are smaller than ufrcmax and there is no instability.     !
       ! By construction, it must be 'wlcl > 0' but for assurance, I checked !
       ! this again in the below block. If 'ufrclcl < 0.1%', just exit.      !
       ! ------------------------------------------------------------------- !

       wtw = winv * winv - 2._r8 * cinlcl * rbuoy
       if( wtw .le. 0._r8 ) then
         ! write(iulog,*) 'wlcl < 0 at the LCL'
           exit_wtw(i) = 1._r8
           id_exit = .true.
           go to 333
       endif
       wlcl = sqrt(wtw)
       ufrclcl = cbmf/wlcl/rho0inv
       wrel = wlcl
       if( ufrclcl .le. 0.0001_r8 ) then
         ! write(iulog,*) 'ufrclcl <= 0.0001' 
           exit_ufrc(i) = 1._r8
           id_exit = .true.
           go to 333
       endif
       ufrc(krel-1) = ufrclcl

       ! ----------------------------------------------------------------------- !
       ! Below is just diagnostic output for detailed analysis of cumulus scheme !
       ! ----------------------------------------------------------------------- !

       ufrcinvbase        = ufrcinv
       winvbase           = winv
       umf(kinv-1:krel-1) = cbmf   
       wu(kinv-1:krel-1)  = winv   

       ! -------------------------------------------------------------------------- ! 
       ! Define updraft properties at the level where buoyancy sorting starts to be !
       ! happening, i.e., by definition, at 'prel' level within the release layer.  !
       ! Because no lateral entrainment occurs upto 'prel', conservative scalars of ! 
       ! cumulus updraft at release level is same as those of source air.  However, ! 
       ! horizontal momentums of source air are modified by horizontal PGF forcings ! 
       ! from PBL top interface to 'prel'.  For this case, we should add additional !
       ! horizontal momentum from PBL top interface to 'prel' as will be done below !
       ! to 'usrc' and 'vsrc'. Note that below cumulus updraft properties - umf, wu,!
       ! thlu, qtu, thvu, uu, vu - are defined all interfaces not at the layer mid- !
       ! point. From the index notation of cumulus scheme, wu(k) is the cumulus up- !
       ! draft vertical velocity at the top interface of k layer.                   !
       ! Diabatic horizontal momentum forcing should be treated as a kind of 'body' !
       ! forcing without actual mass exchange between convective updraft and        !
       ! environment, but still taking horizontal momentum from the environment to  !
       ! the convective updrafts. Thus, diabatic convective momentum transport      !
       ! vertically redistributes environmental horizontal momentum.                !
       ! -------------------------------------------------------------------------- !

       emf(krel-1)  = 0._r8
       umf(krel-1)  = cbmf
       wu(krel-1)   = wrel
       thlu(krel-1) = thlsrc
       qtu(krel-1)  = qtsrc
       call conden(prel,thlsrc,qtsrc,thj,qvj,qlj,qij,qse,id_check,qsat)
       if( id_check .eq. 1 ) then
           exit_conden(i) = 1._r8
           id_exit = .true.
           go to 333
       endif
       thvu(krel-1) = thj * ( 1._r8 + zvir*qvj - qlj - qij )       

       uplus = 0._r8
       vplus = 0._r8
       if( krel .eq. kinv ) then
           uplus = PGFc * ssu0(kinv) * ( prel - ps0(kinv-1) )
           vplus = PGFc * ssv0(kinv) * ( prel - ps0(kinv-1) )
       else
           do k = kinv, max(krel-1,kinv)
              uplus = uplus + PGFc * ssu0(k) * ( ps0(k) - ps0(k-1) )
              vplus = vplus + PGFc * ssv0(k) * ( ps0(k) - ps0(k-1) )
           end do
           uplus = uplus + PGFc * ssu0(krel) * ( prel - ps0(krel-1) )
           vplus = vplus + PGFc * ssv0(krel) * ( prel - ps0(krel-1) )
       end if
       uu(krel-1) = usrc + uplus
       vu(krel-1) = vsrc + vplus      

       do m = 1, ncnst
          tru(krel-1,m)  = trsrc(m)
       enddo

       ! -------------------------------------------------------------------------- !
       ! Define environmental properties at the level where buoyancy sorting occurs !
       ! ('pe', normally, layer midpoint except in the 'krel' layer). In the 'krel' !
       ! layer where buoyancy sorting starts to occur, however, 'pe' is defined     !
       ! differently because LCL is regarded as lower interface for mixing purpose. !
       ! -------------------------------------------------------------------------- !

       pe      = 0.5_r8 * ( prel + ps0(krel) )
       dpe     = prel - ps0(krel)
       exne    = exnf(pe)
       thvebot = thv0rel
       thle    = thl0(krel) + ssthl0(krel) * ( pe - p0(krel) )
       qte     = qt0(krel)  + ssqt0(krel)  * ( pe - p0(krel) )
       ue      = u0(krel)   + ssu0(krel)   * ( pe - p0(krel) )
       ve      = v0(krel)   + ssv0(krel)   * ( pe - p0(krel) )
       do m = 1, ncnst
          tre(m) = tr0(krel,m)  + sstr0(krel,m) * ( pe - p0(krel) )
       enddo

       !-------------------------! 
       ! Buoyancy-Sorting Mixing !
       !-------------------------!------------------------------------------------ !
       !                                                                           !
       !  In order to complete buoyancy-sorting mixing at layer mid-point, and so  ! 
       !  calculate 'updraft mass flux, updraft w velocity, conservative scalars'  !
       !  at the upper interface of each layer, we need following 3 information.   ! 
       !                                                                           !
       !  1. Pressure where mixing occurs ('pe'), and temperature at 'pe' which is !
       !     necessary to calculate various thermodynamic coefficients at pe. This !
       !     temperature is obtained by undiluted cumulus properties lifted to pe. ! 
       !  2. Undiluted updraft properties at pe - conservative scalar and vertical !
       !     velocity -which are assumed to be the same as the properties at lower !
       !     interface only for calculation of fractional lateral entrainment  and !
       !     detrainment rate ( fer(k) and fdr(k) [Pa-1] ), respectively.    Final !
       !     values of cumulus conservative scalars and w at the top interface are !
       !     calculated afterward after obtaining fer(k) & fdr(k).                 !
       !  3. Environmental properties at pe.                                       !
       ! ------------------------------------------------------------------------- !
       
       ! ------------------------------------------------------------------------ ! 
       ! Define cumulus scale height.                                             !
       ! Cumulus scale height is defined as the maximum height cumulus can reach. !
       ! In case of premitive code, cumulus scale height ('cush')  at the current !
       ! time step was assumed to be the same as 'cush' of previous time step.    !
       ! However, I directly calculated cush at each time step using an iterative !
       ! method. Note that within the cumulus scheme, 'cush' information is  used !
       ! only at two places during buoyancy-sorting process:                      !
       ! (1) Even negatively buoyancy mixtures with strong vertical velocity      !
       !     enough to rise up to 'rle*scaleh' (rle = 0.1) from pe are entrained  !
       !     into cumulus updraft,                                                !  
       ! (2) The amount of mass that is involved in buoyancy-sorting mixing       !
       !      process at pe is rei(k) = rkm/scaleh/rho*g [Pa-1]                   !
       ! In terms of (1), I think critical stopping distance might be replaced by !
       ! layer thickness. In future, we will use rei(k) = (0.5*rkm/z0(k)/rho/g).  !
       ! In the premitive code,  'scaleh' was largely responsible for the jumping !
       ! variation of precipitation amount.                                       !
       ! ------------------------------------------------------------------------ !   

       scaleh = tscaleh
       if( tscaleh .lt. 0.0_r8 ) scaleh = 1000._r8 

     ! Save time : Set iter_scaleh = 1. This will automatically use 'cush' from the previous time step
     !             at the first implicit iteration. At the second implicit iteration, it will use
     !             the updated 'cush' by the first implicit cin. So, this updating has an effect of
     !             doing one iteration for cush calculation, which is good. 
     !             So, only this setting of 'iter_scaleh = 1' is sufficient-enough to save computation time.
     ! OK

       do iter_scaleh = 1, 3

       ! ---------------------------------------------------------------- !
       ! Initialization of 'kbup' and 'kpen'                              !
       ! ---------------------------------------------------------------- !
       ! 'kbup' is the top-most layer in which cloud buoyancy is positive !
       ! both at the top and bottom interface of the layer. 'kpen' is the !
       ! layer upto which cumulus panetrates ,i.e., cumulus w at the base !
       ! interface is positive, but becomes negative at the top interface.!
       ! Here, we initialize 'kbup' and 'kpen'. These initializations are !  
       ! not trivial but important, expecially   in calculating turbulent !
       ! fluxes without confliction among several physics as explained in !
       ! detail in the part of turbulent fluxes calculation later.   Note !
       ! that regardless of whether 'kbup' and 'kpen' are updated or  not !
       ! during updraft motion,  penetrative entrainments are dumped down !
       ! across the top interface of 'kbup' later.      More specifically,!
       ! penetrative entrainment heat and moisture fluxes are  calculated !
       ! from the top interface of 'kbup' layer  to the base interface of !
       ! 'kpen' layer. Because of this, initialization of 'kbup' & 'kpen' !
       ! influence the convection system when there are not updated.  The !  
       ! below initialization of 'kbup = krel' assures  that  penetrative !
       ! entrainment fluxes always occur at interfaces above the PBL  top !
       ! interfaces (i.e., only at interfaces k >=kinv ), which seems  to !
       ! be attractable considering that the most correct fluxes  at  the !
       ! PBL top interface can be ontained from the 'fluxbelowinv'  using !
       ! reconstructed PBL height.                                        ! 
       ! The 'kbup = krel'(after going through the whole buoyancy sorting !
       ! proces during updraft motion) implies that cumulus updraft  from !
       ! the PBL top interface can not reach to the LFC,so that 'kbup' is !
       ! not updated during upward. This means that cumulus updraft   did !
       ! not fully overcome the buoyancy barrier above just the PBL top.  !
       ! If 'kpen' is not updated either ( i.e., cumulus cannot rise over !
       ! the top interface of release layer),penetrative entrainment will !
       ! not happen at any interfaces.  If cumulus updraft can rise above !
       ! the release layer but cannot fully overcome the buoyancy barrier !
       ! just above PBL top interface, penetratve entrainment   occurs at !
       ! several above interfaces, including the top interface of release ! 
       ! layer. In the latter case, warming and drying tendencies will be !
       ! be initiated in 'krel' layer. Note current choice of 'kbup=krel' !
       ! is completely compatible with other flux physics without  double !
       ! or miss counting turbulent fluxes at any interface. However, the !
       ! alternative choice of 'kbup=krel-1' also has itw own advantage - !
       ! when cumulus updraft cannot overcome buoyancy barrier just above !
       ! PBL top, entrainment warming and drying are concentrated in  the !
       ! 'kinv-1' layer instead of 'kinv' layer for this case. This might !
       ! seems to be more dynamically reasonable, but I will choose the   !
       ! 'kbup = krel' choice since it is more compatible  with the other !
       ! parts of the code, expecially, when we chose ' use_emf=.false. ' !
       ! as explained in detail in turbulent flux calculation part.       !
       ! ---------------------------------------------------------------- ! 

       kbup    = krel
       kpen    = krel
       
       ! ------------------------------------------------------------ !
       ! Since 'wtw' is continuously updated during vertical motion,  !
       ! I need below initialization command within this 'iter_scaleh'!
       ! do loop. Similarily, I need initializations of environmental !
       ! properties at 'krel' layer as below.                         !
       ! ------------------------------------------------------------ !

       wtw     = wlcl * wlcl
       pe      = 0.5_r8 * ( prel + ps0(krel) )
       dpe     = prel - ps0(krel)
       exne    = exnf(pe)
       thvebot = thv0rel
       thle    = thl0(krel) + ssthl0(krel) * ( pe - p0(krel) )
       qte     = qt0(krel)  + ssqt0(krel)  * ( pe - p0(krel) )
       ue      = u0(krel)   + ssu0(krel)   * ( pe - p0(krel) )
       ve      = v0(krel)   + ssv0(krel)   * ( pe - p0(krel) )
       do m = 1, ncnst
          tre(m) = tr0(krel,m)  + sstr0(krel,m)  * ( pe - p0(krel) )
       enddo

       ! ----------------------------------------------------------------------- !
       ! Cumulus rises upward from 'prel' ( or base interface of  'krel' layer ) !
       ! until updraft vertical velocity becomes zero.                           !
       ! Buoyancy sorting is performed via two stages. (1) Using cumulus updraft !
       ! properties at the base interface of each layer,perform buoyancy sorting !
       ! at the layer mid-point, 'pe',  and update cumulus properties at the top !
       ! interface, and then  (2) by averaging updated cumulus properties at the !
       ! top interface and cumulus properties at the base interface,   calculate !
       ! cumulus updraft properties at pe that will be used  in buoyancy sorting !
       ! mixing - thlue, qtue and, wue.  Using this averaged properties, perform !
       ! buoyancy sorting again at pe, and re-calculate fer(k) and fdr(k). Using !
       ! this recalculated fer(k) and fdr(k),  finally calculate cumulus updraft !
       ! properties at the top interface - thlu, qtu, thvu, uu, vu. In the below,!
       ! 'iter_xc = 1' performs the first stage, while 'iter_xc= 2' performs the !
       ! second stage. We can increase the number of iterations, 'nter_xc'.as we !
       ! want, but a sample test indicated that about 3 - 5 iterations  produced !
       ! satisfactory converent solution. Finally, identify 'kbup' and 'kpen'.   !
       ! ----------------------------------------------------------------------- !
       
       do k = krel, mkx - 1 ! Here, 'k' is a layer index.

          km1 = k - 1

          thlue = thlu(km1)
          qtue  = qtu(km1)    
          wue   = wu(km1)
          wtwb  = wtw  

       do iter_xc = 1, niter_xc
          
          wtw = wu(km1) * wu(km1)

          ! ---------------------------------------------------------------- !
          ! Calculate environmental and cumulus saturation 'excess' at 'pe'. !
          ! Note that in order to calculate saturation excess, we should use ! 
          ! liquid water temperature instead of temperature  as the argument !
          ! of "qsat". But note normal argument of "qsat" is temperature.    !
          ! ---------------------------------------------------------------- !

          call conden(pe,thle,qte,thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if
          thv0j    = thj * ( 1._r8 + zvir*qvj - qlj - qij )
          rho0j    = pe / ( r * thv0j * exne )
          qsat_arg = thle*exne     
          status   = qsat(qsat_arg,pe,es(1),qs(1),gam(1),1)
          excess0  = qte - qs(1)

          call conden(pe,thlue,qtue,thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if
          ! ----------------------------------------------------------------- !
          ! Detrain excessive condensate larger than 'criqc' from the cumulus ! 
          ! updraft before performing buoyancy sorting. All I should to do is !
          ! to update 'thlue' &  'que' here. Below modification is completely !
          ! compatible with the other part of the code since 'thule' & 'qtue' !
          ! are used only for buoyancy sorting. I found that as long as I use !
          ! 'niter_xc >= 2',  detraining excessive condensate before buoyancy !
          ! sorting has negligible influence on the buoyancy sorting results. !   
          ! ----------------------------------------------------------------- !
          if( (qlj + qij) .gt. criqc ) then
               exql  = ( ( qlj + qij ) - criqc ) * qlj / ( qlj + qij )
               exqi  = ( ( qlj + qij ) - criqc ) * qij / ( qlj + qij )
               qtue  = qtue - exql - exqi
               thlue = thlue + (xlv/cp/exne)*exql + (xls/cp/exne)*exqi 
          endif
          call conden(pe,thlue,qtue,thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if
          thvj     = thj * ( 1._r8 + zvir * qvj - qlj - qij )
          tj       = thj * exne ! This 'tj' is used for computing thermo. coeffs. below
          qsat_arg = thlue*exne
          status   = qsat(qsat_arg,pe,es(1),qs(1),gam(1),1)
          excessu  = qtue - qs(1)

          ! ------------------------------------------------------------------- !
          ! Calculate critical mixing fraction, 'xc'. Mixture with mixing ratio !
          ! smaller than 'xc' will be entrained into cumulus updraft.  Both the !
          ! saturated updrafts with 'positive buoyancy' or 'negative buoyancy + ! 
          ! strong vertical velocity enough to rise certain threshold distance' !
          ! are kept into the updraft in the below program. If the core updraft !
          ! is unsaturated, we can set 'xc = 0' and let the cumulus  convection !
          ! still works or we may exit.                                         !
          ! Current below code does not entrain unsaturated mixture. However it !
          ! should be modified such that it also entrain unsaturated mixture.   !
          ! ------------------------------------------------------------------- !

          ! ----------------------------------------------------------------- !
          ! cridis : Critical stopping distance for buoyancy sorting purpose. !
          !          scaleh is only used here.                                !
          ! ----------------------------------------------------------------- !

            cridis = rle*scaleh                 ! Original code
          ! cridis = 1._r8*(zs0(k) - zs0(k-1))  ! New code
 
          ! ---------------- !
          ! Buoyancy Sorting !
          ! ---------------- !                   

          ! ----------------------------------------------------------------- !
          ! Case 1 : When both cumulus and env. are unsaturated or saturated. !
          ! ----------------------------------------------------------------- !

          if( ( excessu .le. 0._r8 .and. excess0 .le. 0._r8 ) .or. ( excessu .ge. 0._r8 .and. excess0 .ge. 0._r8 ) ) then
                xc = min(1._r8,max(0._r8,1._r8-2._r8*rbuoy*g*cridis/wue**2._r8*(1._r8-thvj/thv0j)))
              ! Below 3 lines are diagnostic output not influencing
              ! numerical calculations.
                aquad = 0._r8
                bquad = 0._r8
                cquad = 0._r8
          else
          ! -------------------------------------------------- !
          ! Case 2 : When either cumulus or env. is saturated. !
          ! -------------------------------------------------- !
              xsat    = excessu / ( excessu - excess0 );
              thlxsat = thlue + xsat * ( thle - thlue );
              qtxsat  = qtue  + xsat * ( qte - qtue );
              call conden(pe,thlxsat,qtxsat,thj,qvj,qlj,qij,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  exit_conden(i) = 1._r8
                  id_exit = .true.
                  go to 333
              end if
              thvxsat = thj * ( 1._r8 + zvir * qvj - qlj - qij )               
              ! -------------------------------------------------- !
              ! kk=1 : Cumulus Segment, kk=2 : Environment Segment !
              ! -------------------------------------------------- ! 
              do kk = 1, 2 
                   if( kk .eq. 1 ) then
                       thv_x0 = thvj;
                       thv_x1 = ( 1._r8 - 1._r8/xsat ) * thvj + ( 1._r8/xsat ) * thvxsat;
                   else
                       thv_x1 = thv0j;
                       thv_x0 = ( xsat / ( xsat - 1._r8 ) ) * thv0j + ( 1._r8/( 1._r8 - xsat ) ) * thvxsat;
                   endif
                   aquad =  wue**2;
                   bquad =  2._r8*rbuoy*g*cridis*(thv_x1 - thv_x0)/thv0j - 2._r8*wue**2;
                   cquad =  2._r8*rbuoy*g*cridis*(thv_x0 -  thv0j)/thv0j +       wue**2;
                   if( kk .eq. 1 ) then
                       if( ( bquad**2-4._r8*aquad*cquad ) .ge. 0._r8 ) then
                             call roots(aquad,bquad,cquad,xs1,xs2,status)
                             x_cu = min(1._r8,max(0._r8,min(xsat,min(xs1,xs2))))
                       else
                             x_cu = xsat;
                       endif
                   else 
                       if( ( bquad**2-4._r8*aquad*cquad) .ge. 0._r8 ) then
                             call roots(aquad,bquad,cquad,xs1,xs2,status)
                             x_en = min(1._r8,max(0._r8,max(xsat,min(xs1,xs2))))
                       else
                             x_en = 1._r8;
                       endif
                   endif
              enddo
              if( x_cu .eq. xsat ) then
                  xc = max(x_cu, x_en);
              else
                  xc = x_cu;
              endif
          endif

          ! ------------------------------------------------------------------------ !
          ! Compute fractional lateral entrainment & detrainment rate in each layers.!
          ! The unit of rei(k), fer(k), and fdr(k) is [Pa-1].  Alternative choice of !
          ! 'rei(k)' is also shown below, where coefficient 0.5 was from approximate !
          ! tuning against the BOMEX case.                                           !
          ! In order to prevent the onset of instability in association with cumulus !
          ! induced subsidence advection, cumulus mass flux at the top interface  in !
          ! any layer should be smaller than ( 90% of ) total mass within that layer.!
          ! I imposed limits on 'rei(k)' as below,  in such that stability condition ! 
          ! is always satisfied.                                                     !
          ! Below limiter of 'rei(k)' becomes negative for some cases, causing error.!
          ! So, for the time being, I came back to the original limiter.             !
          ! ------------------------------------------------------------------------ !
          ee2    = xc**2
          ud2    = 1._r8 - 2._r8*xc + xc**2
        ! rei(k) = ( rkm / scaleh / g / rho0j )        ! Default.
          rei(k) = ( 0.5_r8 * rkm / z0(k) / g /rho0j ) ! Alternative.
          if( xc .gt. 0.5_r8 ) rei(k) = min(rei(k),0.9_r8*log(dp0(k)/g/dt/umf(km1) + 1._r8)/dpe/(2._r8*xc-1._r8))
          fer(k) = rei(k) * ee2
          fdr(k) = rei(k) * ud2

          ! ------------------------------------------------------------------------------ !
          ! Iteration Start due to 'maxufrc' constraint [ ****************************** ] ! 
          ! ------------------------------------------------------------------------------ !

          ! -------------------------------------------------------------------------- !
          ! Calculate cumulus updraft mass flux and penetrative entrainment mass flux. !
          ! Note that  non-zero penetrative entrainment mass flux will be asigned only !
          ! to interfaces from the top interface of 'kbup' layer to the base interface !
          ! of 'kpen' layer as will be shown later.                                    !
          ! -------------------------------------------------------------------------- !

          umf(k) = umf(km1) * exp( dpe * ( fer(k) - fdr(k) ) )
          emf(k) = 0._r8    

          ! --------------------------------------------------------- !
          ! Compute cumulus updraft properties at the top interface.  !
          ! Also use Tayler expansion in order to treat limiting case !
          ! --------------------------------------------------------- !

          if( fer(k)*dpe .lt. 1.e-4_r8 ) then
              thlu(k) = thlu(km1) + ( thle + ssthl0(k) * dpe / 2._r8 - thlu(km1) ) * fer(k) * dpe
              qtu(k)  =  qtu(km1) + ( qte  +  ssqt0(k) * dpe / 2._r8 -  qtu(km1) ) * fer(k) * dpe
              uu(k)   =   uu(km1) + ( ue   +   ssu0(k) * dpe / 2._r8 -   uu(km1) ) * fer(k) * dpe - PGFc * ssu0(k) * dpe
              vu(k)   =   vu(km1) + ( ve   +   ssv0(k) * dpe / 2._r8 -   vu(km1) ) * fer(k) * dpe - PGFc * ssv0(k) * dpe
              do m = 1, ncnst
                 tru(k,m)  =  tru(km1,m) + ( tre(m)  + sstr0(k,m) * dpe / 2._r8  -  tru(km1,m) ) * fer(k) * dpe
              enddo
          else
              thlu(k) = ( thle + ssthl0(k) / fer(k) - ssthl0(k) * dpe / 2._r8 ) -          &
                        ( thle + ssthl0(k) * dpe / 2._r8 - thlu(km1) + ssthl0(k) / fer(k) ) * exp(-fer(k) * dpe)
              qtu(k)  = ( qte  +  ssqt0(k) / fer(k) -  ssqt0(k) * dpe / 2._r8 ) -          &  
                        ( qte  +  ssqt0(k) * dpe / 2._r8 -  qtu(km1) +  ssqt0(k) / fer(k) ) * exp(-fer(k) * dpe)
              uu(k) =   ( ue + ( 1._r8 - PGFc ) * ssu0(k) / fer(k) - ssu0(k) * dpe / 2._r8 ) - &
                        ( ue +     ssu0(k) * dpe / 2._r8 -   uu(km1) + ( 1._r8 - PGFc ) * ssu0(k) / fer(k) ) * exp(-fer(k) * dpe)
              vu(k) =   ( ve + ( 1._r8 - PGFc ) * ssv0(k) / fer(k) - ssv0(k) * dpe / 2._r8 ) - &
                        ( ve +     ssv0(k) * dpe / 2._r8 -   vu(km1) + ( 1._r8 - PGFc ) * ssv0(k) / fer(k) ) * exp(-fer(k) * dpe)
              do m = 1, ncnst
                 tru(k,m)  = ( tre(m)  + sstr0(k,m) / fer(k) - sstr0(k,m) * dpe / 2._r8 ) - &  
                             ( tre(m)  + sstr0(k,m) * dpe / 2._r8 - tru(km1,m) + sstr0(k,m) / fer(k) ) * exp(-fer(k) * dpe)
              enddo
          end if

          !------------------------------------------------------------------- !
          ! Expel some of cloud water and ice from cumulus  updraft at the top !
          ! interface.  Note that this is not 'detrainment' term  but a 'sink' !
          ! term of cumulus updraft qt ( or one part of 'source' term of  mean !
          ! environmental qt ). At this stage, as the most simplest choice, if !
          ! condensate amount within cumulus updraft is larger than a critical !
          ! value, 'criqc', expels the surplus condensate from cumulus updraft !
          ! to the environment. A certain fraction ( e.g., 'frc_sus' ) of this !
          ! expelled condesnate will be in a form that can be suspended in the !
          ! layer k where it was formed, while the other fraction, '1-frc_sus' ! 
          ! will be in a form of precipitatble (e.g.,can potentially fall down !
          ! across the base interface of layer k ). In turn we should describe !
          ! subsequent falling of precipitable condensate ('1-frc_sus') across !
          ! the base interface of the layer k, &  evaporation of precipitating !
          ! water in the below layer k-1 and associated evaporative cooling of !
          ! the later, k-1, and falling of 'non-evaporated precipitating water !
          ! ( which was initially formed in layer k ) and a newly-formed preci !
          ! pitable water in the layer, k-1', across the base interface of the !
          ! lower layer k-1.  Cloud microphysics should correctly describe all !
          ! of these process.  In a near future, I should significantly modify !
          ! this cloud microphysics, including precipitation-induced downdraft !
          ! also.                                                              !
          ! ------------------------------------------------------------------ !

          call conden(ps0(k),thlu(k),qtu(k),thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if
          if( (qlj + qij) .gt. criqc ) then
               exql    = ( ( qlj + qij ) - criqc ) * qlj / ( qlj + qij )
               exqi    = ( ( qlj + qij ) - criqc ) * qij / ( qlj + qij )
               ! ---------------------------------------------------------------- !
               ! It is very important to re-update 'qtu' and 'thlu'  at the upper ! 
               ! interface after expelling condensate from cumulus updraft at the !
               ! top interface of the layer. As mentioned above, this is a 'sink' !
               ! of cumulus qt (or equivalently, a 'source' of environmentasl qt),!
               ! not a regular convective'detrainment'.                           !
               ! ---------------------------------------------------------------- !
               qtu(k)  = qtu(k) - exql - exqi
               thlu(k) = thlu(k) + (xlv/cp/exns0(k))*exql + (xls/cp/exns0(k))*exqi 
               ! ---------------------------------------------------------------- !
               ! Expelled cloud condensate into the environment from the updraft. ! 
               ! After all the calculation later, 'dwten' and 'diten' will have a !
               ! unit of [ kg/kg/s ], because it is a tendency of qt. Restoration !
               ! of 'dwten' and 'diten' to this correct unit through  multiplying !
               ! 'umf(k)*g/dp0(k)' will be performed later after finally updating !
               ! 'umf' using a 'rmaxfrac' constraint near the end of this updraft !
               ! buoyancy sorting loop.                                           !
               ! ---------------------------------------------------------------- !
               dwten(k) = exql   
               diten(k) = exqi
          else
               dwten(k) = 0._r8
               diten(k) = 0._r8
          endif
          ! ----------------------------------------------------------------- ! 
          ! Update 'thvu(k)' after detraining condensate from cumulus updraft.!
          ! ----------------------------------------------------------------- ! 
          call conden(ps0(k),thlu(k),qtu(k),thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if  
          thvu(k) = thj * ( 1._r8 + zvir * qvj - qlj - qij )

          ! ----------------------------------------------------------- ! 
          ! Calculate updraft vertical velocity at the upper interface. !
          ! In order to calculate 'wtw' at the upper interface, we use  !
          ! 'wtw' at the lower interface. Note  'wtw'  is continuously  ! 
          ! updated as cumulus updraft rises.                           !
          ! ----------------------------------------------------------- !

          bogbot = rbuoy * ( thvu(km1) / thvebot  - 1._r8 ) ! Cloud buoyancy at base interface
          bogtop = rbuoy * ( thvu(k) / thv0top(k) - 1._r8 ) ! Cloud buoyancy at top  interface

          delbog = bogtop - bogbot
          drage  = fer(k) * ( 1._r8 + rdrag )
          expfac = exp(-2._r8*drage*dpe)

          wtwb = wtw
          if( drage*dpe .gt. 1.e-3_r8 ) then
              wtw = wtw*expfac + (delbog + (1._r8-expfac)*(bogbot + delbog/(-2._r8*drage*dpe)))/(rho0j*drage)
          else
              wtw = wtw + dpe * ( bogbot + bogtop ) / rho0j
          endif

        ! Force the plume rise at least to klfc of the undiluted plume.
        ! Because even the below is not complete, I decided not to include this.

        ! if( k .le. klfc ) then
        !     wtw = max( 1.e-2_r8, wtw )
        ! endif 
         
          ! -------------------------------------------------------------- !
          ! Repeat 'iter_xc' iteration loop until 'iter_xc = niter_xc'.    !
          ! Also treat the case even when wtw < 0 at the 'kpen' interface. !
          ! -------------------------------------------------------------- !  
          
          if( wtw .gt. 0._r8 ) then   
              thlue = 0.5_r8 * ( thlu(km1) + thlu(k) )
              qtue  = 0.5_r8 * ( qtu(km1)  +  qtu(k) )         
              wue   = 0.5_r8 *   sqrt( max( wtwb + wtw, 0._r8 ) )
          else
              go to 111
          endif 

       enddo ! End of 'iter_xc' loop  

   111 continue

          ! --------------------------------------------------------------------------- ! 
          ! Add the contribution of self-detrainment  to vertical variations of cumulus !
          ! updraft mass flux. The reason why we are trying to include self-detrainment !
          ! is as follows.  In current scheme,  vertical variation of updraft mass flux !
          ! is not fully consistent with the vertical variation of updraft vertical w.  !
          ! For example, within a given layer, let's assume that  cumulus w is positive !
          ! at the base interface, while negative at the top interface. This means that !
          ! cumulus updraft cannot reach to the top interface of the layer. However,    !
          ! cumulus updraft mass flux at the top interface is not zero according to the !
          ! vertical tendency equation of cumulus mass flux.   Ideally, cumulus updraft ! 
          ! mass flux at the top interface should be zero for this case. In order to    !
          ! assures that cumulus updraft mass flux goes to zero when cumulus updraft    ! 
          ! vertical velocity goes to zero, we are imposing self-detrainment term as    !
          ! below by considering layer-mean cloud buoyancy and cumulus updraft vertical !
          ! velocity square at the top interface. Use of auto-detrainment term will  be !
          ! determined by setting 'use_self_detrain=.true.' in the parameter sentence.  !
          ! --------------------------------------------------------------------------- !
     
          if( use_self_detrain ) then
              autodet = min( 0.5_r8*g*(bogbot+bogtop)/(max(wtw,0._r8)+1.e-4_r8), 0._r8 ) 
              umf(k)  = umf(k) * exp( 0.637_r8*(dpe/rho0j/g) * autodet )   
          end if      
          if( umf(k) .eq. 0._r8 ) wtw = -1._r8

          ! -------------------------------------- !
          ! Below block is just a dignostic output !
          ! -------------------------------------- ! 

          excessu_arr(k) = excessu
          excess0_arr(k) = excess0
          xc_arr(k)      = xc
          aquad_arr(k)   = aquad
          bquad_arr(k)   = bquad
          cquad_arr(K)   = cquad
          bogbot_arr(k)  = bogbot
          bogtop_arr(k)  = bogtop

          ! ------------------------------------------------------------------- !
          ! 'kbup' is the upper most layer in which cloud buoyancy  is positive ! 
          ! both at the base and top interface.  'kpen' is the upper most layer !
          ! up to cumulus can reach. Usually, 'kpen' is located higher than the !
          ! 'kbup'. Note we initialized these by 'kbup = krel' & 'kpen = krel'. !
          ! As explained before, it is possible that only 'kpen' is updated,    !
          ! while 'kbup' keeps its initialization value. For this case, current !
          ! scheme will simply turns-off penetrative entrainment fluxes and use ! 
          ! normal buoyancy-sorting fluxes for 'kbup <= k <= kpen-1' interfaces,!
          ! in order to describe shallow continental cumulus convection.        !
          ! ------------------------------------------------------------------- !
          
        ! if( bogbot .gt. 0._r8 .and. bogtop .gt. 0._r8 ) then 
        ! if( bogtop .gt. 0._r8 ) then          
          if( bogtop .gt. 0._r8 .and. wtw .gt. 0._r8 ) then 
              kbup = k
          end if

          if( wtw .le. 0._r8 ) then
              kpen = k
              go to 45
          end if

          wu(k) = sqrt(wtw)
          if( wu(k) .gt. 100._r8 ) then
              exit_wu(i) = 1._r8
              id_exit = .true.
              go to 333
          endif

          ! ---------------------------------------------------------------------------- !
          ! Iteration end due to 'rmaxfrac' constraint [ ***************************** ] ! 
          ! ---------------------------------------------------------------------------- !

          ! ---------------------------------------------------------------------- !
          ! Calculate updraft fractional area at the upper interface and set upper ! 
          ! limit to 'ufrc' by 'rmaxfrac'. In order to keep the consistency  among !
          ! ['ufrc','umf','wu (or wtw)'], if ufrc is limited by 'rmaxfrac', either !
          ! 'umf' or 'wu' should be changed. Although both 'umf' and 'wu (wtw)' at !
          ! the current upper interface are used for updating 'umf' & 'wu'  at the !
          ! next upper interface, 'umf' is a passive variable not influencing  the !
          ! buoyancy sorting process in contrast to 'wtw'. This is a reason why we !
          ! adjusted 'umf' instead of 'wtw'. In turn we updated 'fdr' here instead !
          ! of 'fer',  which guarantees  that all previously updated thermodynamic !
          ! variables at the upper interface before applying 'rmaxfrac' constraint !
          ! are already internally consistent,  even though 'ufrc'  is  limited by !
          ! 'rmaxfrac'. Thus, we don't need to go through interation loop again.If !
          ! If we update 'fer' however, we should go through above iteration loop. !
          ! ---------------------------------------------------------------------- !
            
          rhos0j  = ps0(k) / ( r * 0.5_r8 * ( thv0bot(k+1) + thv0top(k) ) * exns0(k) )
          ufrc(k) = umf(k) / ( rhos0j * wu(k) )
          if( ufrc(k) .gt. rmaxfrac ) then
              limit_ufrc(i) = 1._r8 
              ufrc(k) = rmaxfrac
              umf(k)  = rmaxfrac * rhos0j * wu(k)
              fdr(k)  = fer(k) - log( umf(k) / umf(km1) ) / dpe
          endif

          ! ------------------------------------------------------------ !
          ! Update environmental properties for at the mid-point of next !
          ! upper layer for use in buoyancy sorting.                     !
          ! ------------------------------------------------------------ ! 

          pe      = p0(k+1)
          dpe     = dp0(k+1)
          exne    = exn0(k+1)
          thvebot = thv0bot(k+1)
          thle    = thl0(k+1)
          qte     = qt0(k+1)
          ue      = u0(k+1)
          ve      = v0(k+1) 
          do m = 1, ncnst
             tre(m)  = tr0(k+1,m)
          enddo

       end do   ! End of cumulus updraft loop from the 'krel' layer to 'kpen' layer.
       
       ! ------------------------------------------------------------------------------- !
       ! Up to this point, we finished all of buoyancy sorting processes from the 'krel' !
       ! layer to 'kpen' layer: at the top interface of individual layers, we calculated !
       ! updraft and penetrative mass fluxes [ umf(k) & emf(k) = 0 ], updraft fractional !
       ! area [ ufrc(k) ],  updraft vertical velocity [ wu(k) ],  updraft  thermodynamic !
       ! variables [thlu(k),qtu(k),uu(k),vu(k),thvu(k)]. In the layer,we also calculated !
       ! fractional entrainment-detrainment rate [ fer(k), fdr(k) ], and detrainment ten !
       ! dency of water and ice from cumulus updraft [ dwten(k), diten(k) ]. In addition,!
       ! we updated and identified 'krel' and 'kpen' layer index, if any.  In the 'kpen' !
       ! layer, we calculated everything mentioned above except the 'wu(k)' and 'ufrc(k)'!
       ! since a real value of updraft vertical velocity is not defined at the kpen  top !
       ! interface (note 'ufrc' at the top interface of layer is calculated from 'umf(k)'!
       ! and 'wu(k)'). As mentioned before, special treatment is required when 'kbup' is !
       ! not updated and so 'kbup = krel'.                                               !
       ! ------------------------------------------------------------------------------- !
       
       ! ------------------------------------------------------------------------------ !
       ! During the 'iter_scaleh' iteration loop, non-physical ( with non-zero values ) !
       ! values can remain in the variable arrays above (also 'including' in case of wu !
       ! and ufrc at the top interface) the 'kpen' layer. This can happen when the kpen !
       ! layer index identified from the 'iter_scaleh = 1' iteration loop is located at !
       ! above the kpen layer index identified from   'iter_scaleh = 3' iteration loop. !
       ! Thus, in the following calculations, we should only use the values in each     !
       ! variables only up to finally identified 'kpen' layer & 'kpen' interface except ! 
       ! 'wu' and 'ufrc' at the top interface of 'kpen' layer.    Note that in order to !
       ! prevent any problems due to these non-physical values, I re-initialized    the !
       ! values of [ umf(kpen:mkx), emf(kpen:mkx), dwten(kpen+1:mkx), diten(kpen+1:mkx),! 
       ! fer(kpen:mkx), fdr(kpen+1:mkx), ufrc(kpen:mkx) ] to be zero after 'iter_scaleh'!
       ! do loop.                                                                       !
       ! ------------------------------------------------------------------------------ !
       
 45    continue

       ! ------------------------------------------------------------------------------ !
       ! Calculate 'ppen( < 0 )', updarft penetrative distance from the lower interface !
       ! of 'kpen' layer. Note that bogbot & bogtop at the 'kpen' layer either when fer !
       ! is zero or non-zero was already calculated above.                              !
       ! It seems that below qudarature solving formula is valid only when bogbot < 0.  !
       ! Below solving equation is clearly wrong ! I should revise this !               !
       ! ------------------------------------------------------------------------------ ! 
            
       if( drage .eq. 0._r8 ) then
           aquad =  ( bogtop - bogbot ) / ( ps0(kpen) - ps0(kpen-1) )
           bquad =  2._r8 * bogbot
           cquad = -wu(kpen-1)**2 * rho0j
           call roots(aquad,bquad,cquad,xc1,xc2,status)
           if( status .eq. 0 ) then
               if( xc1 .le. 0._r8 .and. xc2 .le. 0._r8 ) then
                   ppen = max( xc1, xc2 )
                   ppen = min( 0._r8,max( -dp0(kpen), ppen ) )  
               elseif( xc1 .gt. 0._r8 .and. xc2 .gt. 0._r8 ) then
                   ppen = -dp0(kpen)
                   write(iulog,*) 'Warning : UW-Cumulus penetrates upto kpen interface'
#ifdef WRF_PORT
                   call wrf_message(iulog)
#endif
               else
                   ppen = min( xc1, xc2 )
                   ppen = min( 0._r8,max( -dp0(kpen), ppen ) )  
               endif
           else
               ppen = -dp0(kpen)
               write(iulog,*) 'Warning : UW-Cumulus penetrates upto kpen interface'
#ifdef WRF_PORT
               call wrf_message(iulog)
#endif
           endif       
       else 
           ppen = compute_ppen(wtwb,drage,bogbot,bogtop,rho0j,dp0(kpen))
       endif
       if( ppen .eq. -dp0(kpen) .or. ppen .eq. 0._r8 ) limit_ppen(i) = 1._r8

       ! -------------------------------------------------------------------- !
       ! Re-calculate the amount of expelled condensate from cloud updraft    !
       ! at the cumulus top. This is necessary for refined calculations of    !
       ! bulk cloud microphysics at the cumulus top. Note that ppen < 0._r8   !
       ! In the below, I explicitly calculate 'thlu_top' & 'qtu_top' by       !
       ! using non-zero 'fer(kpen)'.                                          !    
       ! -------------------------------------------------------------------- !

       if( fer(kpen)*(-ppen) .lt. 1.e-4_r8 ) then
           thlu_top = thlu(kpen-1) + ( thl0(kpen) + ssthl0(kpen) * (-ppen) / 2._r8 - thlu(kpen-1) ) * fer(kpen) * (-ppen)
           qtu_top  =  qtu(kpen-1) + (  qt0(kpen) +  ssqt0(kpen) * (-ppen) / 2._r8  - qtu(kpen-1) ) * fer(kpen) * (-ppen)
       else
           thlu_top = ( thl0(kpen) + ssthl0(kpen) / fer(kpen) - ssthl0(kpen) * (-ppen) / 2._r8 ) - &
                      ( thl0(kpen) + ssthl0(kpen) * (-ppen) / 2._r8 - thlu(kpen-1) + ssthl0(kpen) / fer(kpen) ) * &
                        exp(-fer(kpen) * (-ppen))
           qtu_top  = ( qt0(kpen)  +  ssqt0(kpen) / fer(kpen) -  ssqt0(kpen) * (-ppen) / 2._r8 ) - &  
                      ( qt0(kpen)  +  ssqt0(kpen) * (-ppen) / 2._r8 -  qtu(kpen-1) +  ssqt0(kpen) / fer(kpen) ) * &
                        exp(-fer(kpen) * (-ppen))
       end if

       call conden(ps0(kpen-1)+ppen,thlu_top,qtu_top,thj,qvj,qlj,qij,qse,id_check,qsat)
       if( id_check .eq. 1 ) then
           exit_conden(i) = 1._r8
           id_exit = .true.
           go to 333
       end if
       exntop = ((ps0(kpen-1)+ppen)/p00)**rovcp
       if( (qlj + qij) .gt. criqc ) then
            dwten(kpen) = ( ( qlj + qij ) - criqc ) * qlj / ( qlj + qij )
            diten(kpen) = ( ( qlj + qij ) - criqc ) * qij / ( qlj + qij )
            qtu_top  = qtu_top - dwten(kpen) - diten(kpen)
            thlu_top = thlu_top + (xlv/cp/exntop)*dwten(kpen) + (xls/cp/exntop)*diten(kpen) 
       else
            dwten(kpen) = 0._r8
            diten(kpen) = 0._r8
       endif      
 
       ! ----------------------------------------------------------------------- !
       ! Calculate cumulus scale height as the top height that cumulus can reach.!
       ! ----------------------------------------------------------------------- !
       
       rhos0j = ps0(kpen-1)/(r*0.5_r8*(thv0bot(kpen)+thv0top(kpen-1))*exns0(kpen-1))  
       cush   = zs0(kpen-1) - ppen/rhos0j/g
       scaleh = cush 

    end do   ! End of 'iter_scaleh' loop.   

       ! -------------------------------------------------------------------- !   
       ! The 'forcedCu' is logical identifier saying whether cumulus updraft  !
       ! overcome the buoyancy barrier just above the PBL top. If it is true, !
       ! cumulus did not overcome the barrier -  this is a shallow convection !
       ! with negative cloud buoyancy, mimicking  shallow continental cumulus !
       ! convection. Depending on 'forcedCu' parameter, treatment of heat  &  !
       ! moisture fluxes at the entraining interfaces, 'kbup <= k < kpen - 1' !
       ! will be set up in a different ways, as will be shown later.          !
       ! -------------------------------------------------------------------- !
 
       if( kbup .eq. krel ) then 
           forcedCu = .true.
           limit_shcu(i) = 1._r8
       else
           forcedCu = .false.
           limit_shcu(i) = 0._r8
       endif  
       
       ! ------------------------------------------------------------------ !
       ! Filtering of unerasonable cumulus adjustment here.  This is a very !
       ! important process which should be done cautiously. Various ways of !
       ! filtering are possible depending on cases mainly using the indices !
       ! of key layers - 'klcl','kinv','krel','klfc','kbup','kpen'. At this !
       ! stage, the followings are all possible : 'kinv >= 2', 'klcl >= 1', !
       ! 'krel >= kinv', 'kbup >= krel', 'kpen >= krel'. I must design this !
       ! filtering very cautiously, in such that none of  realistic cumulus !
       ! convection is arbitrarily turned-off. Potentially, I might turn-off! 
       ! cumulus convection if layer-mean 'ql > 0' in the 'kinv-1' layer,in !
       ! order to suppress cumulus convection growing, based at the Sc top. ! 
       ! This is one of potential future modifications. Note that ppen < 0. !
       ! ------------------------------------------------------------------ !

       cldhgt = ps0(kpen-1) + ppen
       if( forcedCu ) then
           ! write(iulog,*) 'forcedCu - did not overcome initial buoyancy barrier'
           exit_cufilter(i) = 1._r8
           id_exit = .true.
           go to 333
       end if
       ! Limit 'additional shallow cumulus' for DYCOMS simulation.
       ! if( cldhgt.ge.88000._r8 ) then
       !     id_exit = .true.
       !     go to 333
       ! end if
       
       ! ------------------------------------------------------------------------------ !
       ! Re-initializing some key variables above the 'kpen' layer in order to suppress !
       ! the influence of non-physical values above 'kpen', in association with the use !
       ! of 'iter_scaleh' loop. Note that umf, emf,  ufrc are defined at the interfaces !
       ! (0:mkx), while 'dwten','diten', 'fer', 'fdr' are defined at layer mid-points.  !
       ! Initialization of 'fer' and 'fdr' is for correct writing purpose of diagnostic !
       ! output. Note that we set umf(kpen)=emf(kpen)=ufrc(kpen)=0, in consistent  with !
       ! wtw < 0  at the top interface of 'kpen' layer. However, we still have non-zero !
       ! expelled cloud condensate in the 'kpen' layer.                                 !
       ! ------------------------------------------------------------------------------ !

       umf(kpen:mkx)     = 0._r8
       emf(kpen:mkx)     = 0._r8
       ufrc(kpen:mkx)    = 0._r8
       dwten(kpen+1:mkx) = 0._r8
       diten(kpen+1:mkx) = 0._r8
       fer(kpen+1:mkx)   = 0._r8
       fdr(kpen+1:mkx)   = 0._r8
       
       ! ------------------------------------------------------------------------ !
       ! Calculate downward penetrative entrainment mass flux, 'emf(k) < 0',  and !
       ! thermodynamic properties of penetratively entrained airs at   entraining !
       ! interfaces. emf(k) is defined from the top interface of the  layer  kbup !
       ! to the bottom interface of the layer 'kpen'. Note even when  kbup = krel,!
       ! i.e.,even when 'kbup' was not updated in the above buoyancy  sorting  do !
       ! loop (i.e., 'kbup' remains as the initialization value),   below do loop !
       ! of penetrative entrainment flux can be performed without  any conceptual !
       ! or logical problems, because we have already computed all  the variables !
       ! necessary for performing below penetrative entrainment block.            !
       ! In the below 'do' loop, 'k' is an interface index at which non-zero 'emf'! 
       ! (penetrative entrainment mass flux) is calculated. Since cumulus updraft !
       ! is negatively buoyant in the layers between the top interface of 'kbup'  !
       ! layer (interface index, kbup) and the top interface of 'kpen' layer, the !
       ! fractional lateral entrainment, fer(k) within these layers will be close !
       ! to zero - so it is likely that only strong lateral detrainment occurs in !
       ! thses layers. Under this situation,we can easily calculate the amount of !
       ! detrainment cumulus air into these negatively buoyanct layers by  simply !
       ! comparing cumulus updraft mass fluxes between the base and top interface !
       ! of each layer: emf(k) = emf(k-1)*exp(-fdr(k)*dp0(k))                     !
       !                       ~ emf(k-1)*(1-rei(k)*dp0(k))                       !
       !                emf(k-1)-emf(k) ~ emf(k-1)*rei(k)*dp0(k)                  !
       ! Current code assumes that about 'rpen~10' times of these detrained  mass !
       ! are penetratively re-entrained down into the 'k-1' interface. And all of !
       ! these detrained masses are finally dumped down into the top interface of !
       ! 'kbup' layer. Thus, the amount of penetratively entrained air across the !
       ! top interface of 'kbup' layer with 'rpen~10' becomes too large.          !
       ! Note that this penetrative entrainment part can be completely turned-off !
       ! and we can simply use normal buoyancy-sorting involved turbulent  fluxes !
       ! by modifying 'penetrative entrainment fluxes' part below.                !
       ! ------------------------------------------------------------------------ !
       
       ! -----------------------------------------------------------------------!
       ! Calculate entrainment mass flux and conservative scalars of entraining !
       ! free air at interfaces of 'kbup <= k < kpen - 1'                       !
       ! ---------------------------------------------------------------------- !
 
       do k = 0, mkx
          thlu_emf(k) = thlu(k)
          qtu_emf(k)  = qtu(k)
          uu_emf(k)   = uu(k)
          vu_emf(k)   = vu(k)
          do m = 1, ncnst
             tru_emf(k,m)  = tru(k,m)
          enddo
       end do

       do k = kpen - 1, kbup, -1  ! Here, 'k' is an interface index at which
                                  ! penetrative entrainment fluxes are calculated. 
                                  
          rhos0j = ps0(k) / ( r * 0.5_r8 * ( thv0bot(k+1) + thv0top(k) ) * exns0(k) )

          if( k .eq. kpen - 1 ) then

             ! ------------------------------------------------------------------------ ! 
             ! Note that 'ppen' has already been calculated in the above 'iter_scaleh'  !
             ! loop assuming zero lateral entrainmentin the layer 'kpen'.               !
             ! ------------------------------------------------------------------------ !       
             
             ! -------------------------------------------------------------------- !
             ! Calculate returning mass flux, emf ( < 0 )                           !
             ! Current penetrative entrainment rate with 'rpen~10' is too large and !
             ! future refinement is necessary including the definition of 'thl','qt'! 
             ! of penetratively entrained air.  Penetratively entrained airs across !
             ! the 'kpen-1' interface is assumed to have the properties of the base !
             ! interface of 'kpen' layer. Note that 'emf ~ - umf/ufrc = - w * rho'. !
             ! Thus, below limit sets an upper limit of |emf| to be ~ 10cm/s, which !
             ! is very loose constraint. Here, I used more restricted constraint on !
             ! the limit of emf, assuming 'emf' cannot exceed a net mass within the !
             ! layer above the interface. Similar to the case of warming and drying !
             ! due to cumulus updraft induced compensating subsidence,  penetrative !
             ! entrainment induces compensating upwelling -     in order to prevent !  
             ! numerical instability in association with compensating upwelling, we !
             ! should similarily limit the amount of penetrative entrainment at the !
             ! interface by the amount of masses within the layer just above the    !
             ! penetratively entraining interface.                                  !
             ! -------------------------------------------------------------------- !
             
             if( ( umf(k)*ppen*rei(kpen)*rpen ) .lt. -0.1_r8*rhos0j )         limit_emf(i) = 1._r8
             if( ( umf(k)*ppen*rei(kpen)*rpen ) .lt. -0.9_r8*dp0(kpen)/g/dt ) limit_emf(i) = 1._r8             

             emf(k) = max( max( umf(k)*ppen*rei(kpen)*rpen, -0.1_r8*rhos0j), -0.9_r8*dp0(kpen)/g/dt)
             thlu_emf(k) = thl0(kpen) + ssthl0(kpen) * ( ps0(k) - p0(kpen) )
             qtu_emf(k)  = qt0(kpen)  + ssqt0(kpen)  * ( ps0(k) - p0(kpen) )
             uu_emf(k)   = u0(kpen)   + ssu0(kpen)   * ( ps0(k) - p0(kpen) )     
             vu_emf(k)   = v0(kpen)   + ssv0(kpen)   * ( ps0(k) - p0(kpen) )   
             do m = 1, ncnst
                tru_emf(k,m)  = tr0(kpen,m)  + sstr0(kpen,m)  * ( ps0(k) - p0(kpen) )
             enddo

          else ! if(k.lt.kpen-1). 
              
             ! --------------------------------------------------------------------------- !
             ! Note we are coming down from the higher interfaces to the lower interfaces. !
             ! Also note that 'emf < 0'. So, below operation is a summing not subtracting. !
             ! In order to ensure numerical stability, I imposed a modified correct limit  ! 
             ! of '-0.9*dp0(k+1)/g/dt' on emf(k).                                          !
             ! --------------------------------------------------------------------------- !

             if( use_cumpenent ) then  ! Original Cumulative Penetrative Entrainment

                 if( ( emf(k+1)-umf(k)*dp0(k+1)*rei(k+1)*rpen ) .lt. -0.1_r8*rhos0j )        limit_emf(i) = 1
                 if( ( emf(k+1)-umf(k)*dp0(k+1)*rei(k+1)*rpen ) .lt. -0.9_r8*dp0(k+1)/g/dt ) limit_emf(i) = 1         
                 emf(k) = max(max(emf(k+1)-umf(k)*dp0(k+1)*rei(k+1)*rpen, -0.1_r8*rhos0j), -0.9_r8*dp0(k+1)/g/dt )    
                 if( abs(emf(k)) .gt. abs(emf(k+1)) ) then
                     thlu_emf(k) = ( thlu_emf(k+1) * emf(k+1) + thl0(k+1) * ( emf(k) - emf(k+1) ) ) / emf(k)
                     qtu_emf(k)  = ( qtu_emf(k+1)  * emf(k+1) + qt0(k+1)  * ( emf(k) - emf(k+1) ) ) / emf(k)
                     uu_emf(k)   = ( uu_emf(k+1)   * emf(k+1) + u0(k+1)   * ( emf(k) - emf(k+1) ) ) / emf(k)
                     vu_emf(k)   = ( vu_emf(k+1)   * emf(k+1) + v0(k+1)   * ( emf(k) - emf(k+1) ) ) / emf(k)
                     do m = 1, ncnst
                        tru_emf(k,m)  = ( tru_emf(k+1,m)  * emf(k+1) + tr0(k+1,m)  * ( emf(k) - emf(k+1) ) ) / emf(k)
                     enddo
                 else   
                     thlu_emf(k) = thl0(k+1)
                     qtu_emf(k)  =  qt0(k+1)
                     uu_emf(k)   =   u0(k+1)
                     vu_emf(k)   =   v0(k+1)
                     do m = 1, ncnst
                        tru_emf(k,m)  =  tr0(k+1,m)
                     enddo
                 endif   
                     
             else ! Alternative Non-Cumulative Penetrative Entrainment

                 if( ( -umf(k)*dp0(k+1)*rei(k+1)*rpen ) .lt. -0.1_r8*rhos0j )        limit_emf(i) = 1
                 if( ( -umf(k)*dp0(k+1)*rei(k+1)*rpen ) .lt. -0.9_r8*dp0(k+1)/g/dt ) limit_emf(i) = 1         
                 emf(k) = max(max(-umf(k)*dp0(k+1)*rei(k+1)*rpen, -0.1_r8*rhos0j), -0.9_r8*dp0(k+1)/g/dt )    
                 thlu_emf(k) = thl0(k+1)
                 qtu_emf(k)  =  qt0(k+1)
                 uu_emf(k)   =   u0(k+1)
                 vu_emf(k)   =   v0(k+1)
                 do m = 1, ncnst
                    tru_emf(k,m)  =  tr0(k+1,m)
                 enddo

             endif

          endif

          ! ---------------------------------------------------------------------------- !
          ! In this GCM modeling framework,  all what we should do is to calculate  heat !
          ! and moisture fluxes at the given geometrically-fixed height interfaces -  we !
          ! don't need to worry about movement of material height surface in association !
          ! with compensating subsidence or unwelling, in contrast to the bulk modeling. !
          ! In this geometrically fixed height coordinate system, heat and moisture flux !
          ! at the geometrically fixed height handle everything - a movement of material !
          ! surface is implicitly treated automatically. Note that in terms of turbulent !
          ! heat and moisture fluxes at model interfaces, both the cumulus updraft  mass !
          ! flux and penetratively entraining mass flux play the same role -both of them ! 
          ! warms and dries the 'kbup' layer, cools and moistens the 'kpen' layer,   and !
          ! cools and moistens any intervening layers between 'kbup' and 'kpen' layers.  !
          ! It is important to note these identical roles on turbulent heat and moisture !
          ! fluxes of 'umf' and 'emf'.                                                   !
          ! When 'kbup' is a stratocumulus-topped PBL top interface,  increase of 'rpen' !
          ! is likely to strongly diffuse stratocumulus top interface,  resulting in the !
          ! reduction of cloud fraction. In this sense, the 'kbup' interface has a  very !
          ! important meaning and role : across the 'kbup' interface, strong penetrative !
          ! entrainment occurs, thus any sharp gradient properties across that interface !
          ! are easily diffused through strong mass exchange. Thus, an initialization of ! 
          ! 'kbup' (and also 'kpen') should be done very cautiously as mentioned before. ! 
          ! In order to prevent this stron diffusion for the shallow cumulus convection  !
          ! based at the Sc top, it seems to be good to initialize 'kbup = krel', rather !
          ! that 'kbup = krel-1'.                                                        !
          ! ---------------------------------------------------------------------------- !
          
       end do

       !------------------------------------------------------------------ !
       !                                                                   ! 
       ! Compute turbulent heat, moisture, momentum flux at all interfaces !
       !                                                                   !
       !------------------------------------------------------------------ !
       ! It is very important to note that in calculating turbulent fluxes !
       ! below, we must not double count turbulent flux at any interefaces.!
       ! In the below, turbulent fluxes at the interfaces (interface index !
       ! k) are calculated by the following 4 blocks in consecutive order: !
       !                                                                   !
       ! (1) " 0 <= k <= kinv - 1 "  : PBL fluxes.                         !
       !     From 'fluxbelowinv' using reconstructed PBL height. Currently,!
       !     the reconstructed PBLs are independently calculated for  each !
       !     individual conservative scalar variables ( qt, thl, u, v ) in !
       !     each 'fluxbelowinv',  instead of being uniquely calculated by !
       !     using thvl. Turbulent flux at the surface is assumed to be 0. !
       ! (2) " kinv <= k <= krel - 1 " : Non-buoyancy sorting fluxes       !
       !     Assuming cumulus mass flux  and cumulus updraft thermodynamic !
       !     properties (except u, v which are modified by the PGFc during !
       !     upward motion) are conserved during a updraft motion from the !
       !     PBL top interface to the release level. If these layers don't !
       !     exist (e,g, when 'krel = kinv'), then  current routine do not !
       !     perform this routine automatically. So I don't need to modify !
       !     anything.                                                     ! 
       ! (3) " krel <= k <= kbup - 1 " : Buoyancy sorting fluxes           !
       !     From laterally entraining-detraining buoyancy sorting plumes. ! 
       ! (4) " kbup <= k < kpen-1 " : Penetrative entrainment fluxes       !
       !     From penetratively entraining plumes,                         !
       !                                                                   !
       ! In case of normal situation, turbulent interfaces  in each groups !
       ! are mutually independent of each other. Thus double flux counting !
       ! or ambiguous flux counting requiring the choice among the above 4 !
       ! groups do not occur normally. However, in case that cumulus plume !
       ! could not completely overcome the buoyancy barrier just above the !
       ! PBL top interface and so 'kbup = krel' (.forcedCu=.true.) ( here, !
       ! it can be either 'kpen = krel' as the initialization, or ' kpen > !
       ! krel' if cumulus updraft just penetrated over the top of  release !
       ! layer ). If this happens, we should be very careful in organizing !
       ! the sequence of the 4 calculation routines above -  note that the !
       ! routine located at the later has the higher priority.  Additional ! 
       ! feature I must consider is that when 'kbup = kinv - 1' (this is a !
       ! combined situation of 'kbup=krel-1' & 'krel = kinv' when I  chose !
       ! 'kbup=krel-1' instead of current choice of 'kbup=krel'), a strong !
       ! penetrative entrainment fluxes exists at the PBL top interface, & !
       ! all of these fluxes are concentrated (deposited) within the layer ! 
       ! just below PBL top interface (i.e., 'kinv-1' layer). On the other !
       ! hand, in case of 'fluxbelowinv', only the compensating subsidence !
       ! effect is concentrated in the 'kinv-1' layer and 'pure' turbulent !
       ! heat and moisture fluxes ( 'pure' means the fluxes not associated !
       ! with compensating subsidence) are linearly distributed throughout !
       ! the whole PBL. Thus different choice of the above flux groups can !
       ! produce very different results. Output variable should be written !
       ! consistently to the choice of computation sequences.              !
       ! When the case of 'kbup = krel(-1)' happens,another way to dealing !
       ! with this case is to simply ' exit ' the whole cumulus convection !
       ! calculation without performing any cumulus convection.     We can !
       ! choose this approach by specifying a condition in the  'Filtering !
       ! of unreasonable cumulus adjustment' just after 'iter_scaleh'. But !
       ! this seems not to be a good choice (although this choice was used !
       ! previous code ), since it might arbitrary damped-out  the shallow !
       ! cumulus convection over the continent land, where shallow cumulus ! 
       ! convection tends to be negatively buoyant.                        !
       ! ----------------------------------------------------------------- !  

       ! --------------------------------------------------- !
       ! 1. PBL fluxes :  0 <= k <= kinv - 1                 !
       !    All the information necessary to reconstruct PBL ! 
       !    height are passed to 'fluxbelowinv'.             !
       ! --------------------------------------------------- !

       xsrc  = qtsrc
       xmean = qt0(kinv)
       xtop  = qt0(kinv+1) + ssqt0(kinv+1) * ( ps0(kinv)   - p0(kinv+1) )
       xbot  = qt0(kinv-1) + ssqt0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )        
       call fluxbelowinv( cbmf, ps0(0:mkx), mkx, kinv, dt, xsrc, xmean, xtop, xbot, xflx )
       qtflx(0:kinv-1) = xflx(0:kinv-1)

       xsrc  = thlsrc
       xmean = thl0(kinv)
       xtop  = thl0(kinv+1) + ssthl0(kinv+1) * ( ps0(kinv)   - p0(kinv+1) )
       xbot  = thl0(kinv-1) + ssthl0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )        
       call fluxbelowinv( cbmf, ps0(0:mkx), mkx, kinv, dt, xsrc, xmean, xtop, xbot, xflx )
       slflx(0:kinv-1) = cp * exns0(0:kinv-1) * xflx(0:kinv-1)

       xsrc  = usrc
       xmean = u0(kinv)
       xtop  = u0(kinv+1) + ssu0(kinv+1) * ( ps0(kinv)   - p0(kinv+1) )
       xbot  = u0(kinv-1) + ssu0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )
       call fluxbelowinv( cbmf, ps0(0:mkx), mkx, kinv, dt, xsrc, xmean, xtop, xbot, xflx )
       uflx(0:kinv-1) = xflx(0:kinv-1)

       xsrc  = vsrc
       xmean = v0(kinv)
       xtop  = v0(kinv+1) + ssv0(kinv+1) * ( ps0(kinv)   - p0(kinv+1) )
       xbot  = v0(kinv-1) + ssv0(kinv-1) * ( ps0(kinv-1) - p0(kinv-1) )
       call fluxbelowinv( cbmf, ps0(0:mkx), mkx, kinv, dt, xsrc, xmean, xtop, xbot, xflx )
       vflx(0:kinv-1) = xflx(0:kinv-1)

       do m = 1, ncnst
          xsrc  = trsrc(m)
          xmean = tr0(kinv,m)
          xtop  = tr0(kinv+1,m) + sstr0(kinv+1,m) * ( ps0(kinv)   - p0(kinv+1) )
          xbot  = tr0(kinv-1,m) + sstr0(kinv-1,m) * ( ps0(kinv-1) - p0(kinv-1) )        
          call fluxbelowinv( cbmf, ps0(0:mkx), mkx, kinv, dt, xsrc, xmean, xtop, xbot, xflx )
          trflx(0:kinv-1,m) = xflx(0:kinv-1)
       enddo

       ! -------------------------------------------------------------- !
       ! 2. Non-buoyancy sorting fluxes : kinv <= k <= krel - 1         !
       !    Note that when 'krel = kinv', below block is never executed !
       !    as in a desirable, expected way ( but I must check  if this !
       !    is the case ). The non-buoyancy sorting fluxes are computed !
       !    only when 'krel > kinv'.                                    !
       ! -------------------------------------------------------------- !          

       uplus = 0._r8
       vplus = 0._r8
       do k = kinv, krel - 1
          kp1 = k + 1
          qtflx(k) = cbmf * ( qtsrc  - (  qt0(kp1) +  ssqt0(kp1) * ( ps0(k) - p0(kp1) ) ) )          
          slflx(k) = cbmf * ( thlsrc - ( thl0(kp1) + ssthl0(kp1) * ( ps0(k) - p0(kp1) ) ) ) * cp * exns0(k)
          uplus    = uplus + PGFc * ssu0(k) * ( ps0(k) - ps0(k-1) )
          vplus    = vplus + PGFc * ssv0(k) * ( ps0(k) - ps0(k-1) )
          uflx(k)  = cbmf * ( usrc + uplus -  (  u0(kp1)  +   ssu0(kp1) * ( ps0(k) - p0(kp1) ) ) ) 
          vflx(k)  = cbmf * ( vsrc + vplus -  (  v0(kp1)  +   ssv0(kp1) * ( ps0(k) - p0(kp1) ) ) )
          do m = 1, ncnst
             trflx(k,m) = cbmf * ( trsrc(m)  - (  tr0(kp1,m) +  sstr0(kp1,m) * ( ps0(k) - p0(kp1) ) ) )
          enddo          
       end do

       ! ------------------------------------------------------------------------ !
       ! 3. Buoyancy sorting fluxes : krel <= k <= kbup - 1                       !
       !    In case that 'kbup = krel - 1 ' ( or even in case 'kbup = krel' ),    ! 
       !    buoyancy sorting fluxes are not calculated, which is consistent,      !
       !    desirable feature.                                                    !  
       ! ------------------------------------------------------------------------ !

       do k = krel, kbup - 1      
          kp1 = k + 1
          slflx(k) = cp * exns0(k) * umf(k) * ( thlu(k) - ( thl0(kp1) + ssthl0(kp1) * ( ps0(k) - p0(kp1) ) ) )
          qtflx(k) = umf(k) * ( qtu(k) - ( qt0(kp1) + ssqt0(kp1) * ( ps0(k) - p0(kp1) ) ) )
          uflx(k)  = umf(k) * ( uu(k) - ( u0(kp1) + ssu0(kp1) * ( ps0(k) - p0(kp1) ) ) )
          vflx(k)  = umf(k) * ( vu(k) - ( v0(kp1) + ssv0(kp1) * ( ps0(k) - p0(kp1) ) ) )
          do m = 1, ncnst
             trflx(k,m) = umf(k) * ( tru(k,m) - ( tr0(kp1,m) + sstr0(kp1,m) * ( ps0(k) - p0(kp1) ) ) )
          enddo
       end do

       ! ------------------------------------------------------------------------- !
       ! 4. Penetrative entrainment fluxes : kbup <= k <= kpen - 1                 !
       !    The only confliction that can happen is when 'kbup = kinv-1'. For this !
       !    case, turbulent flux at kinv-1 is calculated  both from 'fluxbelowinv' !
       !    and here as penetrative entrainment fluxes.  Since penetrative flux is !
       !    calculated later, flux at 'kinv - 1 ' will be that of penetrative flux.!
       !    However, turbulent flux calculated at 'kinv - 1' from penetrative entr.!
       !    is less attractable,  since more reasonable turbulent flux at 'kinv-1' !
       !    should be obtained from 'fluxbelowinv', by considering  re-constructed ! 
       !    inversion base height. This conflicting problem can be solved if we can!
       !    initialize 'kbup = krel', instead of kbup = krel - 1. This choice seems!
       !    to be more reasonable since it is not conflicted with 'fluxbelowinv' in!
       !    calculating fluxes at 'kinv - 1' ( for this case, flux at 'kinv-1' is  !
       !    always from 'fluxbelowinv' ), and flux at 'krel-1' is calculated from  !
       !    the non-buoyancy sorting flux without being competed with penetrative  !
       !    entrainment fluxes. Even when we use normal cumulus flux instead of    !
       !    penetrative entrainment fluxes at 'kbup <= k <= kpen-1' interfaces,    !
       !    the initialization of kbup=krel perfectly works without any conceptual !
       !    confliction. Thus it seems to be much better to choose 'kbup = krel'   !
       !    initialization of 'kbup', which is current choice.                     !
       !    Note that below formula uses conventional updraft cumulus fluxes for   !
       !    shallow cumulus which did not overcome the first buoyancy barrier above!
       !    PBL top while uses penetrative entrainment fluxes for the other cases  !
       !    'kbup <= k <= kpen-1' interfaces. Depending on cases, however, I can   !
       !    selelct different choice.                                              !
       ! ------------------------------------------------------------------------------------------------------------------ !
       !   if( forcedCu ) then                                                                                              !
       !       slflx(k) = cp * exns0(k) * umf(k) * ( thlu(k) - ( thl0(kp1) + ssthl0(kp1) * ( ps0(k) - p0(kp1) ) ) )         !
       !       qtflx(k) =                 umf(k) * (  qtu(k) - (  qt0(kp1) +  ssqt0(kp1) * ( ps0(k) - p0(kp1) ) ) )         !
       !       uflx(k)  =                 umf(k) * (   uu(k) - (   u0(kp1) +   ssu0(kp1) * ( ps0(k) - p0(kp1) ) ) )         !
       !       vflx(k)  =                 umf(k) * (   vu(k) - (   v0(kp1) +   ssv0(kp1) * ( ps0(k) - p0(kp1) ) ) )         !
       !       do m = 1, ncnst                                                                                              !
       !          trflx(k,m) = umf(k) * ( tru(k,m) - ( tr0(kp1,m) + sstr0(kp1,m) * ( ps0(k) - p0(kp1) ) ) )                 !
       !       enddo                                                                                                        !
       !   else                                                                                                             !
       !       slflx(k) = cp * exns0(k) * emf(k) * ( thlu_emf(k) - ( thl0(k) + ssthl0(k) * ( ps0(k) - p0(k) ) ) )           !
       !       qtflx(k) =                 emf(k) * (  qtu_emf(k) - (  qt0(k) +  ssqt0(k) * ( ps0(k) - p0(k) ) ) )           !
       !       uflx(k)  =                 emf(k) * (   uu_emf(k) - (   u0(k) +   ssu0(k) * ( ps0(k) - p0(k) ) ) )           !
       !       vflx(k)  =                 emf(k) * (   vu_emf(k) - (   v0(k) +   ssv0(k) * ( ps0(k) - p0(k) ) ) )           !
       !       do m = 1, ncnst                                                                                              !
       !          trflx(k,m) = emf(k) * ( tru_emf(k,m) - ( tr0(k,m) + sstr0(k,m) * ( ps0(k) - p0(k) ) ) )                   !
       !       enddo                                                                                                        !
       !   endif                                                                                                            !
       !                                                                                                                    !
       !   if( use_uppenent ) then ! Combined Updraft + Penetrative Entrainment Flux                                        !
       !       slflx(k) = cp * exns0(k) * umf(k) * ( thlu(k)     - ( thl0(kp1) + ssthl0(kp1) * ( ps0(k) - p0(kp1) ) ) ) + & !
       !                  cp * exns0(k) * emf(k) * ( thlu_emf(k) - (   thl0(k) +   ssthl0(k) * ( ps0(k) - p0(k) ) ) )       !
       !       qtflx(k) =                 umf(k) * (  qtu(k)     - (  qt0(kp1) +  ssqt0(kp1) * ( ps0(k) - p0(kp1) ) ) ) + & !
       !                                  emf(k) * (  qtu_emf(k) - (    qt0(k) +    ssqt0(k) * ( ps0(k) - p0(k) ) ) )       !                   
       !       uflx(k)  =                 umf(k) * (   uu(k)     - (   u0(kp1) +   ssu0(kp1) * ( ps0(k) - p0(kp1) ) ) ) + & !
       !                                  emf(k) * (   uu_emf(k) - (     u0(k) +     ssu0(k) * ( ps0(k) - p0(k) ) ) )       !                      
       !       vflx(k)  =                 umf(k) * (   vu(k)     - (   v0(kp1) +   ssv0(kp1) * ( ps0(k) - p0(kp1) ) ) ) + & !
       !                                  emf(k) * (   vu_emf(k) - (     v0(k) +     ssv0(k) * ( ps0(k) - p0(k) ) ) )       !                     
       !       do m = 1, ncnst                                                                                              !
       !          trflx(k,m) = umf(k) * ( tru(k,m) - ( tr0(kp1,m) + sstr0(kp1,m) * ( ps0(k) - p0(kp1) ) ) ) + &             ! 
       !                       emf(k) * ( tru_emf(k,m) - ( tr0(k,m) + sstr0(k,m) * ( ps0(k) - p0(k) ) ) )                   ! 
       !       enddo                                                                                                        !
       ! ------------------------------------------------------------------------------------------------------------------ !

       do k = kbup, kpen - 1      
          kp1 = k + 1
          slflx(k) = cp * exns0(k) * emf(k) * ( thlu_emf(k) - ( thl0(k) + ssthl0(k) * ( ps0(k) - p0(k) ) ) )
          qtflx(k) =                 emf(k) * (  qtu_emf(k) - (  qt0(k) +  ssqt0(k) * ( ps0(k) - p0(k) ) ) ) 
          uflx(k)  =                 emf(k) * (   uu_emf(k) - (   u0(k) +   ssu0(k) * ( ps0(k) - p0(k) ) ) ) 
          vflx(k)  =                 emf(k) * (   vu_emf(k) - (   v0(k) +   ssv0(k) * ( ps0(k) - p0(k) ) ) )
          do m = 1, ncnst
             trflx(k,m) = emf(k) * ( tru_emf(k,m) - ( tr0(k,m) + sstr0(k,m) * ( ps0(k) - p0(k) ) ) ) 
          enddo
       end do

       ! ------------------------------------------- !
       ! Turn-off cumulus momentum flux as an option !
       ! ------------------------------------------- !

       if( .not. use_momenflx ) then
           uflx(0:mkx) = 0._r8
           vflx(0:mkx) = 0._r8
       endif       

       ! -------------------------------------------------------- !
       ! Condensate tendency by compensating subsidence/upwelling !
       ! -------------------------------------------------------- !
       
       uemf(0:mkx)         = 0._r8
       do k = 0, kinv - 2  ! Assume linear updraft mass flux within the PBL.
          uemf(k) = cbmf * ( ps0(0) - ps0(k) ) / ( ps0(0) - ps0(kinv-1) ) 
       end do
       uemf(kinv-1:krel-1) = cbmf
       uemf(krel:kbup-1)   = umf(krel:kbup-1)
       uemf(kbup:kpen-1)   = emf(kbup:kpen-1) ! Only use penetrative entrainment flux consistently.

       comsub(1:mkx) = 0._r8
       do k = 1, kpen
          comsub(k)  = 0.5_r8 * ( uemf(k) + uemf(k-1) ) 
       end do    

       do k = 1, kpen
          if( comsub(k) .ge. 0._r8 ) then
              if( k .eq. mkx ) then
                  thlten_sub = 0._r8
                  qtten_sub  = 0._r8
                  qlten_sub  = 0._r8
                  qiten_sub  = 0._r8
                  nlten_sub  = 0._r8
                  niten_sub  = 0._r8
              else
                  thlten_sub = g * comsub(k) * ( thl0(k+1) - thl0(k) ) / ( p0(k) - p0(k+1) )
                  qtten_sub  = g * comsub(k) * (  qt0(k+1) -  qt0(k) ) / ( p0(k) - p0(k+1) )
                  qlten_sub  = g * comsub(k) * (  ql0(k+1) -  ql0(k) ) / ( p0(k) - p0(k+1) )
                  qiten_sub  = g * comsub(k) * (  qi0(k+1) -  qi0(k) ) / ( p0(k) - p0(k+1) )
                  nlten_sub  = g * comsub(k) * (  tr0(k+1,ixnumliq) -  tr0(k,ixnumliq) ) / ( p0(k) - p0(k+1) )
                  niten_sub  = g * comsub(k) * (  tr0(k+1,ixnumice) -  tr0(k,ixnumice) ) / ( p0(k) - p0(k+1) )
              endif
          else
              if( k .eq. 1 ) then
                  thlten_sub = 0._r8
                  qtten_sub  = 0._r8
                  qlten_sub  = 0._r8
                  qiten_sub  = 0._r8
                  nlten_sub  = 0._r8
                  niten_sub  = 0._r8
              else
                  thlten_sub = g * comsub(k) * ( thl0(k) - thl0(k-1) ) / ( p0(k-1) - p0(k) )
                  qtten_sub  = g * comsub(k) * (  qt0(k) -  qt0(k-1) ) / ( p0(k-1) - p0(k) )
                  qlten_sub  = g * comsub(k) * (  ql0(k) -  ql0(k-1) ) / ( p0(k-1) - p0(k) )
                  qiten_sub  = g * comsub(k) * (  qi0(k) -  qi0(k-1) ) / ( p0(k-1) - p0(k) )
                  nlten_sub  = g * comsub(k) * (  tr0(k,ixnumliq) -  tr0(k-1,ixnumliq) ) / ( p0(k-1) - p0(k) )
                  niten_sub  = g * comsub(k) * (  tr0(k,ixnumice) -  tr0(k-1,ixnumice) ) / ( p0(k-1) - p0(k) )
              endif
          endif
          thl_prog = thl0(k) + thlten_sub * dt
          qt_prog  = max( qt0(k) + qtten_sub * dt, 1.e-12_r8 )
          call conden(p0(k),thl_prog,qt_prog,thj,qvj,qlj,qij,qse,id_check,qsat)
          if( id_check .eq. 1 ) then
              id_exit = .true.
              go to 333
          endif
        ! qlten_sink(k) = ( qlj - ql0(k) ) / dt
        ! qiten_sink(k) = ( qij - qi0(k) ) / dt
          qlten_sink(k) = max( qlten_sub, - ql0(k) / dt ) ! For consistency with prognostic macrophysics scheme
          qiten_sink(k) = max( qiten_sub, - qi0(k) / dt ) ! For consistency with prognostic macrophysics scheme
          nlten_sink(k) = max( nlten_sub, - tr0(k,ixnumliq) / dt ) 
          niten_sink(k) = max( niten_sub, - tr0(k,ixnumice) / dt )
       end do

       ! --------------------------------------------- !
       !                                               !
       ! Calculate convective tendencies at each layer ! 
       !                                               !
       ! --------------------------------------------- !
       
       ! ----------------- !
       ! Momentum tendency !
       ! ----------------- !
       
       do k = 1, kpen
          km1 = k - 1 
          uten(k) = ( uflx(km1) - uflx(k) ) * g / dp0(k)
          vten(k) = ( vflx(km1) - vflx(k) ) * g / dp0(k) 
          uf(k)   = u0(k) + uten(k) * dt
          vf(k)   = v0(k) + vten(k) * dt
        ! do m = 1, ncnst
        !    trten(k,m) = ( trflx(km1,m) - trflx(k,m) ) * g / dp0(k)
        !  ! Limit trten(k,m) such that negative value is not developed.
        !  ! This limitation does not conserve grid-mean tracers and future
        !  ! refinement is required for tracer-conserving treatment.
        !    trten(k,m) = max(trten(k,m),-tr0(k,m)/dt)              
        ! enddo
       end do        

       ! ----------------------------------------------------------------- !
       ! Tendencies of thermodynamic variables.                            ! 
       ! This part requires a careful treatment of bulk cloud microphysics.!
       ! Relocations of 'precipitable condensates' either into the surface ! 
       ! or into the tendency of 'krel' layer will be performed just after !
       ! finishing the below 'do-loop'.                                    !        
       ! ----------------------------------------------------------------- !
       
       rliq    = 0._r8
       rainflx = 0._r8
       snowflx = 0._r8

       do k = 1, kpen

          km1 = k - 1

          ! ------------------------------------------------------------------------------ !
          ! Compute 'slten', 'qtten', 'qvten', 'qlten', 'qiten', and 'sten'                !
          !                                                                                !
          ! Key assumptions made in this 'cumulus scheme' are :                            !
          ! 1. Cumulus updraft expels condensate into the environment at the top interface !
          !    of each layer. Note that in addition to this expel process ('source' term), !
          !    cumulus updraft can modify layer mean condensate through normal detrainment !
          !    forcing or compensating subsidence.                                         !
          ! 2. Expelled water can be either 'sustaining' or 'precipitating' condensate. By !
          !    definition, 'suataining condensate' will remain in the layer where it was   !
          !    formed, while 'precipitating condensate' will fall across the base of the   !
          !    layer where it was formed.                                                  !
          ! 3. All precipitating condensates are assumed to fall into the release layer or !
          !    ground as soon as it was formed without being evaporated during the falling !
          !    process down to the desinated layer ( either release layer of surface ).    !
          ! ------------------------------------------------------------------------------ !

          ! ------------------------------------------------------------------------- !     
          ! 'dwten(k)','diten(k)' : Production rate of condensate  within the layer k !
          !      [ kg/kg/s ]        by the expels of condensate from cumulus updraft. !
          ! It is important to note that in terms of moisture tendency equation, this !
          ! is a 'source' term of enviromental 'qt'.  More importantly,  these source !
          ! are already counted in the turbulent heat and moisture fluxes we computed !
          ! until now, assuming all the expelled condensate remain in the layer where ! 
          ! it was formed. Thus, in calculation of 'qtten' and 'slten' below, we MUST !
          ! NOT add or subtract these terms explicitly in order not to double or miss !
          ! count, unless some expelled condensates fall down out of the layer.  Note !
          ! this falling-down process ( i.e., precipitation process ) and  associated !
          ! 'qtten' and 'slten' and production of surface precipitation flux  will be !
          ! treated later in 'zm_conv_evap' in 'convect_shallow_tend' subroutine.     ! 
          ! In below, we are converting expelled cloud condensate into correct unit.  !
          ! I found that below use of '0.5 * (umf(k-1) + umf(k))' causes conservation !
          ! errors at some columns in global simulation. So, I returned to originals. !
          ! This will cause no precipitation flux at 'kpen' layer since umf(kpen)=0.  !
          ! ------------------------------------------------------------------------- !

          dwten(k) = dwten(k) * 0.5_r8 * ( umf(k-1) + umf(k) ) * g / dp0(k) ! [ kg/kg/s ]
          diten(k) = diten(k) * 0.5_r8 * ( umf(k-1) + umf(k) ) * g / dp0(k) ! [ kg/kg/s ]  

          ! dwten(k) = dwten(k) * umf(k) * g / dp0(k) ! [ kg/kg/s ]
          ! diten(k) = diten(k) * umf(k) * g / dp0(k) ! [ kg/kg/s ]

          ! --------------------------------------------------------------------------- !
          ! 'qrten(k)','qsten(k)' : Production rate of rain and snow within the layer k !
          !     [ kg/kg/s ]         by cumulus expels of condensates to the environment.!         
          ! This will be falled-out of the layer where it was formed and will be dumped !
          ! dumped into the release layer assuming that there is no evaporative cooling !
          ! while precipitable condensate moves to the relaes level. This is reasonable ! 
          ! assumtion if cumulus is purely vertical and so the path along which precita !
          ! ble condensate falls is fully saturared. This 're-allocation' process of    !
          ! precipitable condensate into the release layer is fully described in this   !
          ! convection scheme. After that, the dumped water into the release layer will !
          ! falling down across the base of release layer ( or LCL, if  exact treatment ! 
          ! is required ) and will be allowed to be evaporated in layers below  release !
          ! layer, and finally non-zero surface precipitation flux will be calculated.  !
          ! This latter process will be separately treated 'zm_conv_evap' routine.      !
          ! --------------------------------------------------------------------------- !

          qrten(k) = frc_rasn * dwten(k)
          qsten(k) = frc_rasn * diten(k) 
 
          ! ----------------------------------------------------------------------- !         
          ! 'rainflx','snowflx' : Cumulative rain and snow flux integrated from the ! 
          !     [ kg/m2/s ]       release leyer to the 'kpen' layer. Note that even !
          ! though wtw(kpen) < 0 (and umf(kpen) = 0) at the top interface of 'kpen' !
          ! layer, 'dwten(kpen)' and diten(kpen)  were calculated after calculating !
          ! explicit cloud top height. Thus below calculation of precipitation flux !
          ! is correct. Note that  precipitating condensates are formed only in the !
          ! layers from 'krel' to 'kpen', including the two layers.                 !
          ! ----------------------------------------------------------------------- !

          rainflx = rainflx + qrten(k) * dp0(k) / g
          snowflx = snowflx + qsten(k) * dp0(k) / g

          ! ------------------------------------------------------------------------ !
          ! 'slten(k)','qtten(k)'                                                    !
          !  Note that 'slflx(k)' and 'qtflx(k)' we have calculated already included !
          !  all the contributions of (1) expels of condensate (dwten(k), diten(k)), !
          !  (2) mass detrainment ( delta * umf * ( qtu - qt ) ), & (3) compensating !
          !  subsidence ( M * dqt / dz ). Thus 'slflx(k)' and 'qtflx(k)' we computed ! 
          !  is a hybrid turbulent flux containing one part of 'source' term - expel !
          !  of condensate. In order to calculate 'slten' and 'qtten', we should add !
          !  additional 'source' term, if any. If the expelled condensate falls down !
          !  across the base of the layer, it will be another sink (negative source) !
          !  term.  Note also that we included frictional heating terms in the below !
          !  calculation of 'slten'.                                                 !
          ! ------------------------------------------------------------------------ !
                   
          slten(k) = ( slflx(km1) - slflx(k) ) * g / dp0(k)
          if( k .eq. 1 ) then
              slten(k) = slten(k) - g / 4._r8 / dp0(k) * (                            &
                                    uflx(k)*(uf(k+1) - uf(k) + u0(k+1) - u0(k)) +     & 
                                    vflx(k)*(vf(k+1) - vf(k) + v0(k+1) - v0(k)))
          elseif( k .ge. 2 .and. k .le. kpen-1 ) then
              slten(k) = slten(k) - g / 4._r8 / dp0(k) * (                            &
                                    uflx(k)*(uf(k+1) - uf(k) + u0(k+1) - u0(k)) +     &
                                    uflx(k-1)*(uf(k) - uf(k-1) + u0(k) - u0(k-1)) +   &
                                    vflx(k)*(vf(k+1) - vf(k) + v0(k+1) - v0(k)) +     &
                                    vflx(k-1)*(vf(k) - vf(k-1) + v0(k) - v0(k-1)))
          elseif( k .eq. kpen ) then
              slten(k) = slten(k) - g / 4._r8 / dp0(k) * (                            &
                                    uflx(k-1)*(uf(k) - uf(k-1) + u0(k) - u0(k-1)) +   &
                                    vflx(k-1)*(vf(k) - vf(k-1) + v0(k) - v0(k-1)))
          endif
          qtten(k) = ( qtflx(km1) - qtflx(k) ) * g / dp0(k)

          ! ---------------------------------------------------------------------------- !
          ! Compute condensate tendency, including reserved condensate                   !
          ! We assume that eventual detachment and detrainment occurs in kbup layer  due !
          ! to downdraft buoyancy sorting. In the layer above the kbup, only penetrative !
          ! entrainment exists. Penetrative entrained air is assumed not to contain any  !
          ! condensate.                                                                  !
          ! ---------------------------------------------------------------------------- !
  
          ! Compute in-cumulus condensate at the layer mid-point.

          if( k .lt. krel .or. k .gt. kpen ) then
              qlu_mid = 0._r8
              qiu_mid = 0._r8
              qlj     = 0._r8
              qij     = 0._r8
          elseif( k .eq. krel ) then 
              call conden(prel,thlu(krel-1),qtu(krel-1),thj,qvj,qlj,qij,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  exit_conden(i) = 1._r8
                  id_exit = .true.
                  go to 333
              endif
              qlubelow = qlj       
              qiubelow = qij       
              call conden(ps0(k),thlu(k),qtu(k),thj,qvj,qlj,qij,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  exit_conden(i) = 1._r8
                  id_exit = .true.
                  go to 333
              end if
              qlu_mid = 0.5_r8 * ( qlubelow + qlj ) * ( prel - ps0(k) )/( ps0(k-1) - ps0(k) )
              qiu_mid = 0.5_r8 * ( qiubelow + qij ) * ( prel - ps0(k) )/( ps0(k-1) - ps0(k) )
          elseif( k .eq. kpen ) then 
              call conden(ps0(k-1)+ppen,thlu_top,qtu_top,thj,qvj,qlj,qij,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  exit_conden(i) = 1._r8
                  id_exit = .true.
                  go to 333
              end if
              qlu_mid = 0.5_r8 * ( qlubelow + qlj ) * ( -ppen )        /( ps0(k-1) - ps0(k) )
              qiu_mid = 0.5_r8 * ( qiubelow + qij ) * ( -ppen )        /( ps0(k-1) - ps0(k) )
              qlu_top = qlj
              qiu_top = qij
          else
              call conden(ps0(k),thlu(k),qtu(k),thj,qvj,qlj,qij,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  exit_conden(i) = 1._r8
                  id_exit = .true.
                  go to 333
              end if
              qlu_mid = 0.5_r8 * ( qlubelow + qlj )
              qiu_mid = 0.5_r8 * ( qiubelow + qij )
          endif
          qlubelow = qlj       
          qiubelow = qij       

          ! 1. Sustained Precipitation

          qc_l(k) = ( 1._r8 - frc_rasn ) * dwten(k) ! [ kg/kg/s ]
          qc_i(k) = ( 1._r8 - frc_rasn ) * diten(k) ! [ kg/kg/s ]

          ! 2. Detrained Condensate

          if( k .le. kbup ) then 
              qc_l(k) = qc_l(k) + g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * qlu_mid ! [ kg/kg/s ]
              qc_i(k) = qc_i(k) + g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * qiu_mid ! [ kg/kg/s ]
              qc_lm   =         - g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * ql0(k)  
              qc_im   =         - g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * qi0(k)
            ! Below 'nc_lm', 'nc_im' should be used only when frc_rasn = 1.
              nc_lm   =         - g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * tr0(k,ixnumliq)  
              nc_im   =         - g * 0.5_r8 * ( umf(k-1) + umf(k) ) * fdr(k) * tr0(k,ixnumice)
          else
              qc_lm   = 0._r8
              qc_im   = 0._r8
              nc_lm   = 0._r8
              nc_im   = 0._r8
          endif

          ! 3. Detached Updraft 

          if( k .eq. kbup ) then
              qc_l(k) = qc_l(k) + g * umf(k) * qlj     / ( ps0(k-1) - ps0(k) ) ! [ kg/kg/s ]
              qc_i(k) = qc_i(k) + g * umf(k) * qij     / ( ps0(k-1) - ps0(k) ) ! [ kg/kg/s ]
              qc_lm   = qc_lm   - g * umf(k) * ql0(k)  / ( ps0(k-1) - ps0(k) ) ! [ kg/kg/s ]
              qc_im   = qc_im   - g * umf(k) * qi0(k)  / ( ps0(k-1) - ps0(k) ) ! [ kg/kg/s ]
              nc_lm   = nc_lm   - g * umf(k) * tr0(k,ixnumliq)  / ( ps0(k-1) - ps0(k) ) ! [ kg/kg/s ]
              nc_im   = nc_im   - g * umf(k) * tr0(k,ixnumice)  / ( ps0(k-1) - ps0(k) ) ! [ kg/kg/s ]
          endif 

          ! 4. Cumulative Penetrative entrainment detrained in the 'kbup' layer
          !    Explicitly compute the properties detrained penetrative entrained airs in k = kbup layer.

          if( k .eq. kbup ) then
              call conden(p0(k),thlu_emf(k),qtu_emf(k),thj,qvj,ql_emf_kbup,qi_emf_kbup,qse,id_check,qsat)
              if( id_check .eq. 1 ) then
                  id_exit = .true.
                  go to 333
              endif
              if( ql_emf_kbup .gt. 0._r8 ) then
                  nl_emf_kbup = tru_emf(k,ixnumliq)
              else
                  nl_emf_kbup = 0._r8
              endif
              if( qi_emf_kbup .gt. 0._r8 ) then
                  ni_emf_kbup = tru_emf(k,ixnumice)
              else
                  ni_emf_kbup = 0._r8
              endif
              qc_lm   = qc_lm   - g * emf(k) * ( ql_emf_kbup - ql0(k) ) / ( ps0(k-1) - ps0(k) ) ! [ kg/kg/s ]
              qc_im   = qc_im   - g * emf(k) * ( qi_emf_kbup - qi0(k) ) / ( ps0(k-1) - ps0(k) ) ! [ kg/kg/s ]
              nc_lm   = nc_lm   - g * emf(k) * ( nl_emf_kbup - tr0(k,ixnumliq) ) / ( ps0(k-1) - ps0(k) ) ! [ kg/kg/s ]
              nc_im   = nc_im   - g * emf(k) * ( ni_emf_kbup - tr0(k,ixnumice) ) / ( ps0(k-1) - ps0(k) ) ! [ kg/kg/s ]
          endif 

          qlten_det   = qc_l(k) + qc_lm
          qiten_det   = qc_i(k) + qc_im

          ! --------------------------------------------------------------------------------- !
          ! 'qlten(k)','qiten(k)','qvten(k)','sten(k)'                                        !
          ! Note that falling of precipitation will be treated later.                         !
          ! The prevension of negative 'qv,ql,qi' will be treated later in positive_moisture. !
          ! --------------------------------------------------------------------------------- ! 

          if( use_expconten ) then
              if( use_unicondet ) then
                  qc_l(k) = 0._r8
                  qc_i(k) = 0._r8 
                  qlten(k) = frc_rasn * dwten(k) + qlten_sink(k) + qlten_det
                  qiten(k) = frc_rasn * diten(k) + qiten_sink(k) + qiten_det
              else 
                  qlten(k) = qc_l(k) + frc_rasn * dwten(k) + ( max( 0._r8, ql0(k) + ( qc_lm + qlten_sink(k) ) * dt ) - ql0(k) ) / dt
                  qiten(k) = qc_i(k) + frc_rasn * diten(k) + ( max( 0._r8, qi0(k) + ( qc_im + qiten_sink(k) ) * dt ) - qi0(k) ) / dt
                  trten(k,ixnumliq) = max( nc_lm + nlten_sink(k), - tr0(k,ixnumliq) / dt )
                  trten(k,ixnumice) = max( nc_im + niten_sink(k), - tr0(k,ixnumice) / dt )
              endif
          else
              if( use_unicondet ) then
                  qc_l(k) = 0._r8
                  qc_i(k) = 0._r8 
              endif                      
              qlten(k) = dwten(k) + ( qtten(k) - dwten(k) - diten(k) ) * ( ql0(k) / qt0(k) )
              qiten(k) = diten(k) + ( qtten(k) - dwten(k) - diten(k) ) * ( qi0(k) / qt0(k) )
          endif

          qvten(k) = qtten(k) - qlten(k) - qiten(k)
          sten(k)  = slten(k) + xlv * qlten(k) + xls * qiten(k)

          ! -------------------------------------------------------------------------- !
          ! 'rliq' : Verticall-integrated 'suspended cloud condensate'                 !
          !  [m/s]   This is so called 'reserved liquid water'  in other subroutines   ! 
          ! of CAM3, since the contribution of this term should not be included into   !
          ! the tendency of each layer or surface flux (precip)  within this cumulus   !
          ! scheme. The adding of this term to the layer tendency will be done inthe   !
          ! 'stratiform_tend', just after performing sediment process there.           !
          ! The main problem of these rather going-back-and-forth and stupid-seeming   ! 
          ! approach is that the sediment process of suspendened condensate will not   !
          ! be treated at all in the 'stratiform_tend'.                                !
          ! Note that 'precip' [m/s] is vertically-integrated total 'rain+snow' formed !
          ! from the cumulus updraft. Important : in the below, 1000 is rhoh2o ( water !
          ! density ) [ kg/m^3 ] used for unit conversion from [ kg/m^2/s ] to [ m/s ] !
          ! for use in stratiform.F90.                                                 !
          ! -------------------------------------------------------------------------- ! 

          qc(k)  =  qc_l(k) +  qc_i(k)   
          rliq   =  rliq    + qc(k) * dp0(k) / g / 1000._r8    ! [ m/s ]

       end do

          precip  =  rainflx + snowflx                       ! [ kg/m2/s ]
          snow    =  snowflx                                 ! [ kg/m2/s ] 

       ! ---------------------------------------------------------------- !
       ! Now treats the 'evaporation' and 'melting' of rain ( qrten ) and ! 
       ! snow ( qsten ) during falling process. Below algorithms are from !
       ! 'zm_conv_evap' but with some modification, which allows separate !
       ! treatment of 'rain' and 'snow' condensates. Note that I included !
       ! the evaporation dynamics into the convection scheme for complete !
       ! development of cumulus scheme especially in association with the ! 
       ! implicit CIN closure. In compatible with this internal treatment !
       ! of evaporation, I should modify 'convect_shallow',  in such that !
       ! 'zm_conv_evap' is not performed when I choose UW PBL-Cu schemes. !                                          
       ! ---------------------------------------------------------------- !

       evpint_rain    = 0._r8 
       evpint_snow    = 0._r8
       flxrain(0:mkx) = 0._r8
       flxsnow(0:mkx) = 0._r8
       ntraprd(:mkx)  = 0._r8
       ntsnprd(:mkx)  = 0._r8

       do k = mkx, 1, -1  ! 'k' is a layer index : 'mkx'('1') is the top ('bottom') layer
          
          ! ----------------------------------------------------------------------------- !
          ! flxsntm [kg/m2/s] : Downward snow flux at the top of each layer after melting.! 
          ! snowmlt [kg/kg/s] : Snow melting tendency.                                    !
          ! Below allows melting of snow when it goes down into the warm layer below.     !
          ! ----------------------------------------------------------------------------- !

          if( t0(k) .gt. 273.16_r8 ) then
              snowmlt = max( 0._r8, flxsnow(k) * g / dp0(k) ) 
          else
              snowmlt = 0._r8
          endif

          ! ----------------------------------------------------------------- !
          ! Evaporation rate of 'rain' and 'snow' in the layer k, [ kg/kg/s ] !
          ! where 'rain' and 'snow' are coming down from the upper layers.    !
          ! I used the same evaporative efficiency both for 'rain' and 'snow'.!
          ! Note that evaporation is not allowed in the layers 'k >= krel' by !
          ! assuming that inside of cumulus cloud, across which precipitation !
          ! is falling down, is fully saturated.                              !
          ! The asumptions in association with the 'evplimit_rain(snow)' are  !
          !   1. Do not allow evaporation to supersate the layer              !
          !   2. Do not evaporate more than the flux falling into the layer   !
          !   3. Total evaporation cannot exceed the input total surface flux !
          ! ----------------------------------------------------------------- !

          status = qsat(t0(k),p0(k),es(1),qs(1),gam(1), 1)          
          subsat = max( ( 1._r8 - qv0(k)/qs(1) ), 0._r8 )
          if( noevap_krelkpen ) then
              if( k .ge. krel ) subsat = 0._r8
          endif

          evprain  = kevp * subsat * sqrt(flxrain(k)+snowmlt*dp0(k)/g) 
          evpsnow  = kevp * subsat * sqrt(max(flxsnow(k)-snowmlt*dp0(k)/g,0._r8))

          evplimit = max( 0._r8, ( qw0_in(i,k) - qv0(k) ) / dt ) 

          evplimit_rain = min( evplimit,      ( flxrain(k) + snowmlt * dp0(k) / g ) * g / dp0(k) )
          evplimit_rain = min( evplimit_rain, ( rainflx - evpint_rain ) * g / dp0(k) )
          evprain = max(0._r8,min( evplimit_rain, evprain ))

          evplimit_snow = min( evplimit,   max( flxsnow(k) - snowmlt * dp0(k) / g , 0._r8 ) * g / dp0(k) )
          evplimit_snow = min( evplimit_snow, ( snowflx - evpint_snow ) * g / dp0(k) )
          evpsnow = max(0._r8,min( evplimit_snow, evpsnow ))

          if( ( evprain + evpsnow ) .gt. evplimit ) then
                tmp1 = evprain * evplimit / ( evprain + evpsnow )
                tmp2 = evpsnow * evplimit / ( evprain + evpsnow )
                evprain = tmp1
                evpsnow = tmp2
          endif

          evapc(k) = evprain + evpsnow

          ! ------------------------------------------------------------- !
          ! Vertically-integrated evaporative fluxes of 'rain' and 'snow' !
          ! ------------------------------------------------------------- !

          evpint_rain = evpint_rain + evprain * dp0(k) / g
          evpint_snow = evpint_snow + evpsnow * dp0(k) / g

          ! -------------------------------------------------------------- !
          ! Net 'rain' and 'snow' production rate in the layer [ kg/kg/s ] !
          ! -------------------------------------------------------------- !         

          ntraprd(k) = qrten(k) - evprain + snowmlt
          ntsnprd(k) = qsten(k) - evpsnow - snowmlt
         
          ! -------------------------------------------------------------------------------- !
          ! Downward fluxes of 'rain' and 'snow' fluxes at the base of the layer [ kg/m2/s ] !
          ! Note that layer index increases with height.                                     !
          ! -------------------------------------------------------------------------------- !

          flxrain(k-1) = flxrain(k) + ntraprd(k) * dp0(k) / g
          flxsnow(k-1) = flxsnow(k) + ntsnprd(k) * dp0(k) / g
          flxrain(k-1) = max( flxrain(k-1), 0._r8 )
          if( flxrain(k-1) .eq. 0._r8 ) ntraprd(k) = -flxrain(k) * g / dp0(k)
          flxsnow(k-1) = max( flxsnow(k-1), 0._r8 )         
          if( flxsnow(k-1) .eq. 0._r8 ) ntsnprd(k) = -flxsnow(k) * g / dp0(k)

          ! ---------------------------------- !
          ! Calculate thermodynamic tendencies !
          ! --------------------------------------------------------------------------- !
          ! Note that equivalently, we can write tendency formula of 'sten' and 'slten' !
          ! by 'sten(k)  = sten(k) - xlv*evprain  - xls*evpsnow - (xls-xlv)*snowmlt' &  !
          !    'slten(k) = sten(k) - xlv*qlten(k) - xls*qiten(k)'.                      !
          ! The above formula is equivalent to the below formula. However below formula !
          ! is preferred since we have already imposed explicit constraint on 'ntraprd' !
          ! and 'ntsnprd' in case that flxrain(k-1) < 0 & flxsnow(k-1) < 0._r8          !
          ! Note : In future, I can elborate the limiting of 'qlten','qvten','qiten'    !
          !        such that that energy and moisture conservation error is completely  !
          !        suppressed.                                                          !
          ! Re-storation to the positive condensate will be performed later below       !
          ! --------------------------------------------------------------------------- !

          qlten(k) = qlten(k) - qrten(k)
          qiten(k) = qiten(k) - qsten(k)
          qvten(k) = qvten(k) + evprain  + evpsnow
          qtten(k) = qlten(k) + qiten(k) + qvten(k)
          if( ( qv0(k) + qvten(k)*dt ) .lt. qmin(1) .or. &
              ( ql0(k) + qlten(k)*dt ) .lt. qmin(2) .or. &
              ( qi0(k) + qiten(k)*dt ) .lt. qmin(3) ) then
               limit_negcon(i) = 1._r8
          end if
          sten(k)  = sten(k) - xlv*evprain  - xls*evpsnow - (xls-xlv)*snowmlt
          slten(k) = sten(k) - xlv*qlten(k) - xls*qiten(k)

        !  slten(k) = slten(k) + xlv * ntraprd(k) + xls * ntsnprd(k)         
        !  sten(k)  = slten(k) + xlv * qlten(k)   + xls * qiten(k)

       end do

       ! ------------------------------------------------------------- !
       ! Calculate final surface flux of precipitation, rain, and snow !
       ! Convert unit to [m/s] for use in 'check_energy_chng'.         !  
       ! ------------------------------------------------------------- !

       precip  = ( flxrain(0) + flxsnow(0) ) / 1000._r8
       snow    =   flxsnow(0) / 1000._r8       

       ! --------------------------------------------------------------------------- !
       ! Until now, all the calculations are done completely in this shallow cumulus !
       ! scheme. If you want to use this cumulus scheme other than CAM3, then do not !
       ! perform below block. However, for compatible use with the other subroutines !
       ! in CAM3, I should subtract the effect of 'qc(k)' ('rliq') from the tendency !
       ! equation in each layer, since this effect will be separately added later in !
       ! in 'stratiform_tend' just after performing sediment process there. In order !
       ! to be consistent with 'stratiform_tend', just subtract qc(k)  from tendency !
       ! equation of each layer, but do not add it to the 'precip'. Apprently,  this !
       ! will violate energy and moisture conservations.    However, when performing !
       ! conservation check in 'tphysbc.F90' just after 'convect_shallow_tend',   we !
       ! will add 'qc(k)' ( rliq ) to the surface flux term just for the purpose  of !
       ! passing the energy-moisture conservation check. Explicit adding-back of 'qc'!
       ! to the individual layer tendency equation will be done in 'stratiform_tend' !
       ! after performing sediment process there. Simply speaking, in 'tphysbc' just !
       ! after 'convect_shallow_tend', we will dump 'rliq' into surface as a  'rain' !
       ! in order to satisfy energy and moisture conservation, and  in the following !
       ! 'stratiform_tend', we will restore it back to 'qlten(k)' ( 'ice' will go to !  
       ! 'water' there) from surface precipitation. This is a funny but conceptually !
       ! entertaining procedure. One concern I have for this complex process is that !
       ! output-writed stratiform precipitation amount will be underestimated due to !
       ! arbitrary subtracting of 'rliq' in stratiform_tend, where                   !
       ! ' prec_str = prec_sed + prec_pcw - rliq' and 'rliq' is not real but fake.   ! 
       ! However, as shown in 'srfxfer.F90', large scale precipitation amount (PRECL)!
       ! that is writed-output is corrected written since in 'srfxfer.F90',  PRECL = !
       ! 'prec_sed + prec_pcw', without including 'rliq'. So current code is correct.!
       ! Note also in 'srfxfer.F90', convective precipitation amount is 'PRECC =     ! 
       ! prec_zmc(i) + prec_cmf(i)' which is also correct.                           !
       ! --------------------------------------------------------------------------- !

       do k = 1, kpen       
          qtten(k) = qtten(k) - qc(k)
          qlten(k) = qlten(k) - qc_l(k)
          qiten(k) = qiten(k) - qc_i(k)
          slten(k) = slten(k) + ( xlv * qc_l(k) + xls * qc_i(k) )
          ! ---------------------------------------------------------------------- !
          ! Since all reserved condensates will be treated  as liquid water in the !
          ! 'check_energy_chng' & 'stratiform_tend' without an explicit conversion !
          ! algorithm, I should consider explicitly the energy conversions between !
          ! 'ice' and 'liquid' - i.e., I should convert 'ice' to 'liquid'  and the !
          ! necessary energy for this conversion should be subtracted from 'sten'. ! 
          ! Without this conversion here, energy conservation error come out. Note !
          ! that there should be no change of 'qvten(k)'.                          !
          ! ---------------------------------------------------------------------- !
          sten(k)  = sten(k)  - ( xls - xlv ) * qc_i(k)
       end do

       ! --------------------------------------------------------------- !
       ! Prevent the onset-of negative condensate at the next time step  !
       ! Potentially, this block can be moved just in front of the above !
       ! block.                                                          ! 
       ! --------------------------------------------------------------- !

       ! Modification : I should check whether this 'positive_moisture_single' routine is
       !                consistent with the one used in UW PBL and cloud macrophysics schemes.
       ! Modification : Below may overestimate resulting 'ql, qi' if we use the new 'qc_l', 'qc_i'
       !                in combination with the original computation of qlten, qiten. However,
       !                if we use new 'qlten,qiten', there is no problem.

        qv0_star(:mkx) = qv0(:mkx) + qvten(:mkx) * dt
        ql0_star(:mkx) = ql0(:mkx) + qlten(:mkx) * dt
        qi0_star(:mkx) = qi0(:mkx) + qiten(:mkx) * dt
        s0_star(:mkx)  =  s0(:mkx) +  sten(:mkx) * dt
        call positive_moisture_single( xlv, xls, mkx, dt, qmin(1), qmin(2), qmin(3), dp0, &
                                       qv0_star, ql0_star, qi0_star, s0_star, qvten, qlten, qiten, sten )
        qtten(:mkx)    = qvten(:mkx) + qlten(:mkx) + qiten(:mkx)
        slten(:mkx)    = sten(:mkx)  - xlv * qlten(:mkx) - xls * qiten(:mkx)

       ! --------------------- !
       ! Tendencies of tracers !
       ! --------------------- !

       do m = 4, ncnst

       if( m .ne. ixnumliq .and. m .ne. ixnumice ) then

          trmin = qmin(m)
#ifdef MODAL_AERO
          do mm = 1, ntot_amode
             if( m .eq. numptr_amode(mm) ) then
                 trmin = 1.e-5_r8
                 goto 55
             endif              
          enddo
       55 continue
#endif 
          trflx_d(0:mkx) = 0._r8
          trflx_u(0:mkx) = 0._r8           
          do k = 1, mkx-1
             if( cnst_get_type_byind(m) .eq. 'wet' ) then
                 pdelx = dp0(k)
             else
                 pdelx = dpdry0(k)
             endif
             km1 = k - 1
             dum = ( tr0(k,m) - trmin ) *  pdelx / g / dt + trflx(km1,m) - trflx(k,m) + trflx_d(km1)
             trflx_d(k) = min( 0._r8, dum )
          enddo
          do k = mkx, 2, -1
             if( cnst_get_type_byind(m) .eq. 'wet' ) then
                 pdelx = dp0(k)
             else
                 pdelx = dpdry0(k)
             endif
             km1 = k - 1
             dum = ( tr0(k,m) - trmin ) * pdelx / g / dt + trflx(km1,m) - trflx(k,m) + &
                                                           trflx_d(km1) - trflx_d(k) - trflx_u(k) 
             trflx_u(km1) = max( 0._r8, -dum ) 
          enddo
          do k = 1, mkx
             if( cnst_get_type_byind(m) .eq. 'wet' ) then
                 pdelx = dp0(k)
             else
                 pdelx = dpdry0(k)
             endif
             km1 = k - 1
           ! Check : I should re-check whether '_u', '_d' are correctly ordered in 
           !         the below tendency computation.
             trten(k,m) = ( trflx(km1,m) - trflx(k,m) + & 
                            trflx_d(km1) - trflx_d(k) + &
                            trflx_u(km1) - trflx_u(k) ) * g / pdelx
          enddo

       endif

       enddo

       ! ---------------------------------------------------------------- !
       ! Cumpute default diagnostic outputs                               !
       ! Note that since 'qtu(krel-1:kpen-1)' & 'thlu(krel-1:kpen-1)' has !
       ! been adjusted after detraining cloud condensate into environment ! 
       ! during cumulus updraft motion,  below calculations will  exactly !
       ! reproduce in-cloud properties as shown in the output analysis.   !
       ! ---------------------------------------------------------------- ! 
 
       call conden(prel,thlu(krel-1),qtu(krel-1),thj,qvj,qlj,qij,qse,id_check,qsat)
       if( id_check .eq. 1 ) then
           exit_conden(i) = 1._r8
           id_exit = .true.
           go to 333
       end if
       qcubelow = qlj + qij
       qlubelow = qlj       
       qiubelow = qij       
       rcwp     = 0._r8
       rlwp     = 0._r8
       riwp     = 0._r8

       ! --------------------------------------------------------------------- !
       ! In the below calculations, I explicitly considered cloud base ( LCL ) !
       ! and cloud top height ( ps0(kpen-1) + ppen )                           !
       ! ----------------------------------------------------------------------! 
       do k = krel, kpen ! This is a layer index
          ! ------------------------------------------------------------------ ! 
          ! Calculate cumulus condensate at the upper interface of each layer. !
          ! Note 'ppen < 0' and at 'k=kpen' layer, I used 'thlu_top'&'qtu_top' !
          ! which explicitly considered zero or non-zero 'fer(kpen)'.          !
          ! ------------------------------------------------------------------ ! 
          if( k .eq. kpen ) then 
              call conden(ps0(k-1)+ppen,thlu_top,qtu_top,thj,qvj,qlj,qij,qse,id_check,qsat)
          else
              call conden(ps0(k),thlu(k),qtu(k),thj,qvj,qlj,qij,qse,id_check,qsat)
          endif
          if( id_check .eq. 1 ) then
              exit_conden(i) = 1._r8
              id_exit = .true.
              go to 333
          end if
          ! ---------------------------------------------------------------- !
          ! Calculate in-cloud mean LWC ( qlu(k) ), IWC ( qiu(k) ),  & layer !
          ! mean cumulus fraction ( cufrc(k) ),  vertically-integrated layer !
          ! mean LWP and IWP. Expel some of in-cloud condensate at the upper !
          ! interface if it is largr than criqc. Note cumulus cloud fraction !
          ! is assumed to be twice of core updraft fractional area. Thus LWP !
          ! and IWP will be twice of actual value coming from our scheme.    !
          ! ---------------------------------------------------------------- !
          qcu(k)   = 0.5_r8 * ( qcubelow + qlj + qij )
          qlu(k)   = 0.5_r8 * ( qlubelow + qlj )
          qiu(k)   = 0.5_r8 * ( qiubelow + qij )
          cufrc(k) = ( ufrc(k-1) + ufrc(k) )
          if( k .eq. krel ) then
              cufrc(k) = ( ufrclcl + ufrc(k) )*( prel - ps0(k) )/( ps0(k-1) - ps0(k) )
          else if( k .eq. kpen ) then
              cufrc(k) = ( ufrc(k-1) + 0._r8 )*( -ppen )        /( ps0(k-1) - ps0(k) )
              if( (qlj + qij) .gt. criqc ) then           
                   qcu(k) = 0.5_r8 * ( qcubelow + criqc )
                   qlu(k) = 0.5_r8 * ( qlubelow + criqc * qlj / ( qlj + qij ) )
                   qiu(k) = 0.5_r8 * ( qiubelow + criqc * qij / ( qlj + qij ) )
              endif
          endif  
          rcwp = rcwp + ( qlu(k) + qiu(k) ) * ( ps0(k-1) - ps0(k) ) / g * cufrc(k)
          rlwp = rlwp +   qlu(k)            * ( ps0(k-1) - ps0(k) ) / g * cufrc(k)
          riwp = riwp +   qiu(k)            * ( ps0(k-1) - ps0(k) ) / g * cufrc(k)
          qcubelow = qlj + qij
          qlubelow = qlj
          qiubelow = qij
       end do
       ! ------------------------------------ !      
       ! Cloud top and base interface indices !
       ! ------------------------------------ !
       cnt = real( kpen, r8 )
       cnb = real( krel - 1, r8 )

       ! ------------------------------------------------------------------------- !
       ! End of formal calculation. Below blocks are for implicit CIN calculations ! 
       ! with re-initialization and save variables at iter_cin = 1._r8             !
       ! ------------------------------------------------------------------------- !
       
       ! --------------------------------------------------------------- !
       ! Adjust the original input profiles for implicit CIN calculation !
       ! --------------------------------------------------------------- !

       if( iter .ne. iter_cin ) then 

          ! ------------------------------------------------------------------- !
          ! Save the output from "iter_cin = 1"                                 !
          ! These output will be writed-out if "iter_cin = 1" was not performed !
          ! for some reasons.                                                   !
          ! ------------------------------------------------------------------- !

          qv0_s(:mkx)           = qv0(:mkx) + qvten(:mkx) * dt
          ql0_s(:mkx)           = ql0(:mkx) + qlten(:mkx) * dt
          qi0_s(:mkx)           = qi0(:mkx) + qiten(:mkx) * dt
          s0_s(:mkx)            = s0(:mkx)  +  sten(:mkx) * dt 
          u0_s(:mkx)            = u0(:mkx)  +  uten(:mkx) * dt
          v0_s(:mkx)            = v0(:mkx)  +  vten(:mkx) * dt 
          qt0_s(:mkx)           = qv0_s(:mkx) + ql0_s(:mkx) + qi0_s(:mkx)
          t0_s(:mkx)            = t0(:mkx)  +  sten(:mkx) * dt / cp
          do m = 1, ncnst
             tr0_s(:mkx,m)      = tr0(:mkx,m) + trten(:mkx,m) * dt
          enddo

          umf_s(0:mkx)          = umf(0:mkx)
          qvten_s(:mkx)         = qvten(:mkx)
          qlten_s(:mkx)         = qlten(:mkx)  
          qiten_s(:mkx)         = qiten(:mkx)
          sten_s(:mkx)          = sten(:mkx)
          uten_s(:mkx)          = uten(:mkx)  
          vten_s(:mkx)          = vten(:mkx)
          qrten_s(:mkx)         = qrten(:mkx)
          qsten_s(:mkx)         = qsten(:mkx)  
          precip_s              = precip
          snow_s                = snow
          evapc_s(:mkx)         = evapc(:mkx)
          cush_s                = cush
          cufrc_s(:mkx)         = cufrc(:mkx)  
          slflx_s(0:mkx)        = slflx(0:mkx)  
          qtflx_s(0:mkx)        = qtflx(0:mkx)  
          qcu_s(:mkx)           = qcu(:mkx)  
          qlu_s(:mkx)           = qlu(:mkx)  
          qiu_s(:mkx)           = qiu(:mkx)  
          fer_s(:mkx)           = fer(:mkx)  
          fdr_s(:mkx)           = fdr(:mkx)  
          cin_s                 = cin
          cinlcl_s              = cinlcl
          cbmf_s                = cbmf
          rliq_s                = rliq
          qc_s(:mkx)            = qc(:mkx)
          cnt_s                 = cnt
          cnb_s                 = cnb
          qtten_s(:mkx)         = qtten(:mkx)
          slten_s(:mkx)         = slten(:mkx)
          ufrc_s(0:mkx)         = ufrc(0:mkx) 

          uflx_s(0:mkx)         = uflx(0:mkx)  
          vflx_s(0:mkx)         = vflx(0:mkx)  
           
          ufrcinvbase_s         = ufrcinvbase
          ufrclcl_s             = ufrclcl 
          winvbase_s            = winvbase
          wlcl_s                = wlcl
          plcl_s                = plcl
          pinv_s                = ps0(kinv-1)
          plfc_s                = plfc        
          pbup_s                = ps0(kbup)
          ppen_s                = ps0(kpen-1) + ppen        
          qtsrc_s               = qtsrc
          thlsrc_s              = thlsrc
          thvlsrc_s             = thvlsrc
          emfkbup_s             = emf(kbup)
          cbmflimit_s           = cbmflimit
          tkeavg_s              = tkeavg
          zinv_s                = zs0(kinv-1)
          rcwp_s                = rcwp
          rlwp_s                = rlwp
          riwp_s                = riwp

          wu_s(0:mkx)           = wu(0:mkx)
          qtu_s(0:mkx)          = qtu(0:mkx)
          thlu_s(0:mkx)         = thlu(0:mkx)
          thvu_s(0:mkx)         = thvu(0:mkx)
          uu_s(0:mkx)           = uu(0:mkx)
          vu_s(0:mkx)           = vu(0:mkx)
          qtu_emf_s(0:mkx)      = qtu_emf(0:mkx)
          thlu_emf_s(0:mkx)     = thlu_emf(0:mkx)
          uu_emf_s(0:mkx)       = uu_emf(0:mkx)
          vu_emf_s(0:mkx)       = vu_emf(0:mkx)
          uemf_s(0:mkx)         = uemf(0:mkx)

          dwten_s(:mkx)         = dwten(:mkx)
          diten_s(:mkx)         = diten(:mkx)
          flxrain_s(0:mkx)      = flxrain(0:mkx)
          flxsnow_s(0:mkx)      = flxsnow(0:mkx)
          ntraprd_s(:mkx)       = ntraprd(:mkx)
          ntsnprd_s(:mkx)       = ntsnprd(:mkx)

          excessu_arr_s(:mkx)   = excessu_arr(:mkx)
          excess0_arr_s(:mkx)   = excess0_arr(:mkx)
          xc_arr_s(:mkx)        = xc_arr(:mkx)
          aquad_arr_s(:mkx)     = aquad_arr(:mkx)
          bquad_arr_s(:mkx)     = bquad_arr(:mkx)
          cquad_arr_s(:mkx)     = cquad_arr(:mkx)
          bogbot_arr_s(:mkx)    = bogbot_arr(:mkx)
          bogtop_arr_s(:mkx)    = bogtop_arr(:mkx)

          do m = 1, ncnst
             trten_s(:mkx,m)    = trten(:mkx,m)
             trflx_s(0:mkx,m)   = trflx(0:mkx,m)
             tru_s(0:mkx,m)     = tru(0:mkx,m)
             tru_emf_s(0:mkx,m) = tru_emf(0:mkx,m)
          enddo

          ! ----------------------------------------------------------------------------- ! 
          ! Recalculate environmental variables for new cin calculation at "iter_cin = 2" ! 
          ! using the updated state variables. Perform only for variables necessary  for  !
          ! the new cin calculation.                                                      !
          ! ----------------------------------------------------------------------------- !
          
          qv0(:mkx)   = qv0_s(:mkx)
          ql0(:mkx)   = ql0_s(:mkx)
          qi0(:mkx)   = qi0_s(:mkx)
          s0(:mkx)    = s0_s(:mkx)
          t0(:mkx)    = t0_s(:mkx)
      
          qt0(:mkx)   = (qv0(:mkx) + ql0(:mkx) + qi0(:mkx))
          thl0(:mkx)  = (t0(:mkx) - xlv*ql0(:mkx)/cp - xls*qi0(:mkx)/cp)/exn0(:mkx)
          thvl0(:mkx) = (1._r8 + zvir*qt0(:mkx))*thl0(:mkx)

          ssthl0      = slope(mkx,thl0,p0) ! Dimension of ssthl0(:mkx) is implicit
          ssqt0       = slope(mkx,qt0 ,p0)
          ssu0        = slope(mkx,u0  ,p0)
          ssv0        = slope(mkx,v0  ,p0)
          do m = 1, ncnst
             sstr0(:mkx,m) = slope(mkx,tr0(:mkx,m),p0)
          enddo

          do k = 1, mkx

             thl0bot = thl0(k) + ssthl0(k) * ( ps0(k-1) - p0(k) )
             qt0bot  = qt0(k)  + ssqt0(k)  * ( ps0(k-1) - p0(k) )
             call conden(ps0(k-1),thl0bot,qt0bot,thj,qvj,qlj,qij,qse,id_check,qsat)
             if( id_check .eq. 1 ) then
                 exit_conden(i) = 1._r8
                 id_exit = .true.
                 go to 333
             end if
             thv0bot(k)  = thj * ( 1._r8 + zvir*qvj - qlj - qij )
             thvl0bot(k) = thl0bot * ( 1._r8 + zvir*qt0bot )
          
             thl0top = thl0(k) + ssthl0(k) * ( ps0(k) - p0(k) )
             qt0top  =  qt0(k) + ssqt0(k)  * ( ps0(k) - p0(k) )
             call conden(ps0(k),thl0top,qt0top,thj,qvj,qlj,qij,qse,id_check,qsat)
             if( id_check .eq. 1 ) then
                 exit_conden(i) = 1._r8
                 id_exit = .true.
                 go to 333
             end if
             thv0top(k)  = thj * ( 1._r8 + zvir*qvj - qlj - qij )
             thvl0top(k) = thl0top * ( 1._r8 + zvir*qt0top )

          end do

       endif               ! End of 'if(iter .ne. iter_cin)' if sentence. 

     end do                ! End of implicit CIN loop (cin_iter)      

     ! ----------------------- !
     ! Update Output Variables !
     ! ----------------------- !

     umf_out(i,0:mkx)             = umf(0:mkx)
     slflx_out(i,0:mkx)           = slflx(0:mkx)
     qtflx_out(i,0:mkx)           = qtflx(0:mkx)
!the indices are not reversed, these variables go into compute_mcshallow_inv, this is why they are called "flxprc1" and "flxsnow1". 
     flxprc1_out(i,0:mkx)         = flxrain(0:mkx) + flxsnow(0:mkx)
     flxsnow1_out(i,0:mkx)        = flxsnow(0:mkx)
     qvten_out(i,:mkx)            = qvten(:mkx)
     qlten_out(i,:mkx)            = qlten(:mkx)
     qiten_out(i,:mkx)            = qiten(:mkx)
     sten_out(i,:mkx)             = sten(:mkx)
     uten_out(i,:mkx)             = uten(:mkx)
     vten_out(i,:mkx)             = vten(:mkx)
     qrten_out(i,:mkx)            = qrten(:mkx)
     qsten_out(i,:mkx)            = qsten(:mkx)
     precip_out(i)                = precip
     snow_out(i)                  = snow
     evapc_out(i,:mkx)            = evapc(:mkx)
     cufrc_out(i,:mkx)            = cufrc(:mkx)
     qcu_out(i,:mkx)              = qcu(:mkx)
     qlu_out(i,:mkx)              = qlu(:mkx)
     qiu_out(i,:mkx)              = qiu(:mkx)
     cush_inout(i)                = cush
     cbmf_out(i)                  = cbmf
     rliq_out(i)                  = rliq
     qc_out(i,:mkx)               = qc(:mkx)
     cnt_out(i)                   = cnt
     cnb_out(i)                   = cnb

     do m = 1, ncnst
        trten_out(i,:mkx,m)       = trten(:mkx,m)
     enddo
  
     ! ------------------------------------------------- !
     ! Below are specific diagnostic output for detailed !
     ! analysis of cumulus scheme                        !
     ! ------------------------------------------------- !

     fer_out(i,mkx:1:-1)          = fer(:mkx)  
     fdr_out(i,mkx:1:-1)          = fdr(:mkx)  
     cinh_out(i)                  = cin
     cinlclh_out(i)               = cinlcl
     qtten_out(i,mkx:1:-1)        = qtten(:mkx)
     slten_out(i,mkx:1:-1)        = slten(:mkx)
     ufrc_out(i,mkx:0:-1)         = ufrc(0:mkx)
     uflx_out(i,mkx:0:-1)         = uflx(0:mkx)  
     vflx_out(i,mkx:0:-1)         = vflx(0:mkx)  
     
     ufrcinvbase_out(i)           = ufrcinvbase
     ufrclcl_out(i)               = ufrclcl 
     winvbase_out(i)              = winvbase
     wlcl_out(i)                  = wlcl
     plcl_out(i)                  = plcl
     pinv_out(i)                  = ps0(kinv-1)
     plfc_out(i)                  = plfc    
     pbup_out(i)                  = ps0(kbup)        
     ppen_out(i)                  = ps0(kpen-1) + ppen            
     qtsrc_out(i)                 = qtsrc
     thlsrc_out(i)                = thlsrc
     thvlsrc_out(i)               = thvlsrc
     emfkbup_out(i)               = emf(kbup)
     cbmflimit_out(i)             = cbmflimit
     tkeavg_out(i)                = tkeavg
     zinv_out(i)                  = zs0(kinv-1)
     rcwp_out(i)                  = rcwp
     rlwp_out(i)                  = rlwp
     riwp_out(i)                  = riwp

     wu_out(i,mkx:0:-1)           = wu(0:mkx)
     qtu_out(i,mkx:0:-1)          = qtu(0:mkx)
     thlu_out(i,mkx:0:-1)         = thlu(0:mkx)
     thvu_out(i,mkx:0:-1)         = thvu(0:mkx)
     uu_out(i,mkx:0:-1)           = uu(0:mkx)
     vu_out(i,mkx:0:-1)           = vu(0:mkx)
     qtu_emf_out(i,mkx:0:-1)      = qtu_emf(0:mkx)
     thlu_emf_out(i,mkx:0:-1)     = thlu_emf(0:mkx)
     uu_emf_out(i,mkx:0:-1)       = uu_emf(0:mkx)
     vu_emf_out(i,mkx:0:-1)       = vu_emf(0:mkx)
     uemf_out(i,mkx:0:-1)         = uemf(0:mkx)

     dwten_out(i,mkx:1:-1)        = dwten(:mkx)
     diten_out(i,mkx:1:-1)        = diten(:mkx)
     flxrain_out(i,mkx:0:-1)      = flxrain(0:mkx)
     flxsnow_out(i,mkx:0:-1)      = flxsnow(0:mkx)
     ntraprd_out(i,mkx:1:-1)      = ntraprd(:mkx)
     ntsnprd_out(i,mkx:1:-1)      = ntsnprd(:mkx)

     excessu_arr_out(i,mkx:1:-1)  = excessu_arr(:mkx)
     excess0_arr_out(i,mkx:1:-1)  = excess0_arr(:mkx)
     xc_arr_out(i,mkx:1:-1)       = xc_arr(:mkx)
     aquad_arr_out(i,mkx:1:-1)    = aquad_arr(:mkx)
     bquad_arr_out(i,mkx:1:-1)    = bquad_arr(:mkx)
     cquad_arr_out(i,mkx:1:-1)    = cquad_arr(:mkx)
     bogbot_arr_out(i,mkx:1:-1)   = bogbot_arr(:mkx)
     bogtop_arr_out(i,mkx:1:-1)   = bogtop_arr(:mkx)

     do m = 1, ncnst
        trflx_out(i,mkx:0:-1,m)   = trflx(0:mkx,m)  
        tru_out(i,mkx:0:-1,m)     = tru(0:mkx,m)
        tru_emf_out(i,mkx:0:-1,m) = tru_emf(0:mkx,m)
     enddo

 333 if(id_exit) then ! Exit without cumulus convection

     exit_UWCu(i) = 1._r8

     ! --------------------------------------------------------------------- !
     ! Initialize output variables when cumulus convection was not performed.!
     ! --------------------------------------------------------------------- !
     
     umf_out(i,0:mkx)             = 0._r8   
     slflx_out(i,0:mkx)           = 0._r8
     qtflx_out(i,0:mkx)           = 0._r8
     qvten_out(i,:mkx)            = 0._r8
     qlten_out(i,:mkx)            = 0._r8
     qiten_out(i,:mkx)            = 0._r8
     sten_out(i,:mkx)             = 0._r8
     uten_out(i,:mkx)             = 0._r8
     vten_out(i,:mkx)             = 0._r8
     qrten_out(i,:mkx)            = 0._r8
     qsten_out(i,:mkx)            = 0._r8
     precip_out(i)                = 0._r8
     snow_out(i)                  = 0._r8
     evapc_out(i,:mkx)            = 0._r8
     cufrc_out(i,:mkx)            = 0._r8
     qcu_out(i,:mkx)              = 0._r8
     qlu_out(i,:mkx)              = 0._r8
     qiu_out(i,:mkx)              = 0._r8
     cush_inout(i)                = -1._r8
     cbmf_out(i)                  = 0._r8   
     rliq_out(i)                  = 0._r8
     qc_out(i,:mkx)               = 0._r8
     cnt_out(i)                   = 1._r8
     cnb_out(i)                   = real(mkx, r8)

     fer_out(i,mkx:1:-1)          = 0._r8  
     fdr_out(i,mkx:1:-1)          = 0._r8  
     cinh_out(i)                  = -1._r8 
     cinlclh_out(i)               = -1._r8 
     qtten_out(i,mkx:1:-1)        = 0._r8
     slten_out(i,mkx:1:-1)        = 0._r8
     ufrc_out(i,mkx:0:-1)         = 0._r8
     uflx_out(i,mkx:0:-1)         = 0._r8  
     vflx_out(i,mkx:0:-1)         = 0._r8  

     ufrcinvbase_out(i)           = 0._r8 
     ufrclcl_out(i)               = 0._r8 
     winvbase_out(i)              = 0._r8    
     wlcl_out(i)                  = 0._r8    
     plcl_out(i)                  = 0._r8    
     pinv_out(i)                  = 0._r8     
     plfc_out(i)                  = 0._r8     
     pbup_out(i)                  = 0._r8    
     ppen_out(i)                  = 0._r8    
     qtsrc_out(i)                 = 0._r8    
     thlsrc_out(i)                = 0._r8    
     thvlsrc_out(i)               = 0._r8    
     emfkbup_out(i)               = 0._r8
     cbmflimit_out(i)             = 0._r8    
     tkeavg_out(i)                = 0._r8    
     zinv_out(i)                  = 0._r8    
     rcwp_out(i)                  = 0._r8    
     rlwp_out(i)                  = 0._r8    
     riwp_out(i)                  = 0._r8    

     wu_out(i,mkx:0:-1)           = 0._r8    
     qtu_out(i,mkx:0:-1)          = 0._r8        
     thlu_out(i,mkx:0:-1)         = 0._r8         
     thvu_out(i,mkx:0:-1)         = 0._r8         
     uu_out(i,mkx:0:-1)           = 0._r8        
     vu_out(i,mkx:0:-1)           = 0._r8        
     qtu_emf_out(i,mkx:0:-1)      = 0._r8         
     thlu_emf_out(i,mkx:0:-1)     = 0._r8         
     uu_emf_out(i,mkx:0:-1)       = 0._r8          
     vu_emf_out(i,mkx:0:-1)       = 0._r8    
     uemf_out(i,mkx:0:-1)         = 0._r8    
   
     dwten_out(i,mkx:1:-1)        = 0._r8    
     diten_out(i,mkx:1:-1)        = 0._r8    
     flxrain_out(i,mkx:0:-1)      = 0._r8     
     flxsnow_out(i,mkx:0:-1)      = 0._r8    
     ntraprd_out(i,mkx:1:-1)      = 0._r8    
     ntsnprd_out(i,mkx:1:-1)      = 0._r8    

     excessu_arr_out(i,mkx:1:-1)  = 0._r8    
     excess0_arr_out(i,mkx:1:-1)  = 0._r8    
     xc_arr_out(i,mkx:1:-1)       = 0._r8    
     aquad_arr_out(i,mkx:1:-1)    = 0._r8    
     bquad_arr_out(i,mkx:1:-1)    = 0._r8    
     cquad_arr_out(i,mkx:1:-1)    = 0._r8    
     bogbot_arr_out(i,mkx:1:-1)   = 0._r8    
     bogtop_arr_out(i,mkx:1:-1)   = 0._r8    

     do m = 1, ncnst
        trten_out(i,:mkx,m)       = 0._r8
        trflx_out(i,mkx:0:-1,m)   = 0._r8  
        tru_out(i,mkx:0:-1,m)     = 0._r8
        tru_emf_out(i,mkx:0:-1,m) = 0._r8
     enddo

     end if

     end do                  ! end of big i loop for each column.

     ! ---------------------------------------- !
     ! Writing main diagnostic output variables !
     ! ---------------------------------------- !

     call outfld( 'qtflx_Cu'        , qtflx_out(:,mkx:0:-1),    mix,    lchnk ) 
     call outfld( 'slflx_Cu'        , slflx_out(:,mkx:0:-1),    mix,    lchnk ) 
     call outfld( 'uflx_Cu'         , uflx_out,                 mix,    lchnk ) 
     call outfld( 'vflx_Cu'         , vflx_out,                 mix,    lchnk ) 

     call outfld( 'qtten_Cu'        , qtten_out,                mix,    lchnk ) 
     call outfld( 'slten_Cu'        , slten_out,                mix,    lchnk ) 
     call outfld( 'uten_Cu'         , uten_out(:,mkx:1:-1),     mix,    lchnk ) 
     call outfld( 'vten_Cu'         , vten_out(:,mkx:1:-1),     mix,    lchnk ) 
     call outfld( 'qvten_Cu'        , qvten_out(:,mkx:1:-1),    mix,    lchnk ) 
     call outfld( 'qlten_Cu'        , qlten_out(:,mkx:1:-1),    mix,    lchnk )
     call outfld( 'qiten_Cu'        , qiten_out(:,mkx:1:-1),    mix,    lchnk )   

     call outfld( 'cbmf_Cu'         , cbmf_out,                 mix,    lchnk ) 
     call outfld( 'ufrcinvbase_Cu'  , ufrcinvbase_out,          mix,    lchnk ) 
     call outfld( 'ufrclcl_Cu'      , ufrclcl_out,              mix,    lchnk ) 
     call outfld( 'winvbase_Cu'     , winvbase_out,             mix,    lchnk ) 
     call outfld( 'wlcl_Cu'         , wlcl_out,                 mix,    lchnk ) 
     call outfld( 'plcl_Cu'         , plcl_out,                 mix,    lchnk ) 
     call outfld( 'pinv_Cu'         , pinv_out,                 mix,    lchnk ) 
     call outfld( 'plfc_Cu'         , plfc_out,                 mix,    lchnk ) 
     call outfld( 'pbup_Cu'         , pbup_out,                 mix,    lchnk ) 
     call outfld( 'ppen_Cu'         , ppen_out,                 mix,    lchnk ) 
     call outfld( 'qtsrc_Cu'        , qtsrc_out,                mix,    lchnk ) 
     call outfld( 'thlsrc_Cu'       , thlsrc_out,               mix,    lchnk ) 
     call outfld( 'thvlsrc_Cu'      , thvlsrc_out,              mix,    lchnk ) 
     call outfld( 'emfkbup_Cu'      , emfkbup_out,              mix,    lchnk )
     call outfld( 'cin_Cu'          , cinh_out,                 mix,    lchnk )  
     call outfld( 'cinlcl_Cu'       , cinlclh_out,              mix,    lchnk ) 
     call outfld( 'cbmflimit_Cu'    , cbmflimit_out,            mix,    lchnk ) 
     call outfld( 'tkeavg_Cu'       , tkeavg_out,               mix,    lchnk )
     call outfld( 'zinv_Cu'         , zinv_out,                 mix,    lchnk )  
     call outfld( 'rcwp_Cu'         , rcwp_out,                 mix,    lchnk )
     call outfld( 'rlwp_Cu'         , rlwp_out,                 mix,    lchnk )
     call outfld( 'riwp_Cu'         , riwp_out,                 mix,    lchnk )
     call outfld( 'tophgt_Cu'       , cush_inout,               mix,    lchnk )   

     call outfld( 'wu_Cu'           , wu_out,                   mix,    lchnk )
     call outfld( 'ufrc_Cu'         , ufrc_out,                 mix,    lchnk )
     call outfld( 'qtu_Cu'          , qtu_out,                  mix,    lchnk )
     call outfld( 'thlu_Cu'         , thlu_out,                 mix,    lchnk )
     call outfld( 'thvu_Cu'         , thvu_out,                 mix,    lchnk )
     call outfld( 'uu_Cu'           , uu_out,                   mix,    lchnk )
     call outfld( 'vu_Cu'           , vu_out,                   mix,    lchnk )
     call outfld( 'qtu_emf_Cu'      , qtu_emf_out,              mix,    lchnk )
     call outfld( 'thlu_emf_Cu'     , thlu_emf_out,             mix,    lchnk )
     call outfld( 'uu_emf_Cu'       , uu_emf_out,               mix,    lchnk )
     call outfld( 'vu_emf_Cu'       , vu_emf_out,               mix,    lchnk )
     call outfld( 'umf_Cu'          , umf_out(:,mkx:0:-1),      mix,    lchnk )
     call outfld( 'uemf_Cu'         , uemf_out,                 mix,    lchnk )
     call outfld( 'qcu_Cu'          , qcu_out(:,mkx:1:-1),      mix,    lchnk )
     call outfld( 'qlu_Cu'          , qlu_out(:,mkx:1:-1),      mix,    lchnk )
     call outfld( 'qiu_Cu'          , qiu_out(:,mkx:1:-1),      mix,    lchnk )
     call outfld( 'cufrc_Cu'        , cufrc_out(:,mkx:1:-1),    mix,    lchnk )  
     call outfld( 'fer_Cu'          , fer_out,                  mix,    lchnk )  
     call outfld( 'fdr_Cu'          , fdr_out,                  mix,    lchnk )  

     call outfld( 'dwten_Cu'        , dwten_out,                mix,    lchnk )
     call outfld( 'diten_Cu'        , diten_out,                mix,    lchnk )
     call outfld( 'qrten_Cu'        , qrten_out(:,mkx:1:-1),    mix,    lchnk )
     call outfld( 'qsten_Cu'        , qsten_out(:,mkx:1:-1),    mix,    lchnk )
     call outfld( 'flxrain_Cu'      , flxrain_out,              mix,    lchnk )
     call outfld( 'flxsnow_Cu'      , flxsnow_out,              mix,    lchnk )
     call outfld( 'ntraprd_Cu'      , ntraprd_out,              mix,    lchnk )
     call outfld( 'ntsnprd_Cu'      , ntsnprd_out,              mix,    lchnk )

     call outfld( 'excessu_Cu'      , excessu_arr_out,          mix,    lchnk )
     call outfld( 'excess0_Cu'      , excess0_arr_out,          mix,    lchnk )
     call outfld( 'xc_Cu'           , xc_arr_out,               mix,    lchnk )
     call outfld( 'aquad_Cu'        , aquad_arr_out,            mix,    lchnk )
     call outfld( 'bquad_Cu'        , bquad_arr_out,            mix,    lchnk )
     call outfld( 'cquad_Cu'        , cquad_arr_out,            mix,    lchnk )
     call outfld( 'bogbot_Cu'       , bogbot_arr_out,           mix,    lchnk )
     call outfld( 'bogtop_Cu'       , bogtop_arr_out,           mix,    lchnk )

     call outfld( 'exit_UWCu_Cu'    , exit_UWCu,                mix,    lchnk ) 
     call outfld( 'exit_conden_Cu'  , exit_conden,              mix,    lchnk ) 
     call outfld( 'exit_klclmkx_Cu' , exit_klclmkx,             mix,    lchnk ) 
     call outfld( 'exit_klfcmkx_Cu' , exit_klfcmkx,             mix,    lchnk ) 
     call outfld( 'exit_ufrc_Cu'    , exit_ufrc,                mix,    lchnk ) 
     call outfld( 'exit_wtw_Cu'     , exit_wtw,                 mix,    lchnk ) 
     call outfld( 'exit_drycore_Cu' , exit_drycore,             mix,    lchnk ) 
     call outfld( 'exit_wu_Cu'      , exit_wu,                  mix,    lchnk ) 
     call outfld( 'exit_cufilter_Cu', exit_cufilter,            mix,    lchnk ) 
     call outfld( 'exit_kinv1_Cu'   , exit_kinv1,               mix,    lchnk ) 
     call outfld( 'exit_rei_Cu'     , exit_rei,                 mix,    lchnk ) 

     call outfld( 'limit_shcu_Cu'   , limit_shcu,               mix,    lchnk ) 
     call outfld( 'limit_negcon_Cu' , limit_negcon,             mix,    lchnk ) 
     call outfld( 'limit_ufrc_Cu'   , limit_ufrc,               mix,    lchnk ) 
     call outfld( 'limit_ppen_Cu'   , limit_ppen,               mix,    lchnk ) 
     call outfld( 'limit_emf_Cu'    , limit_emf,                mix,    lchnk ) 
     call outfld( 'limit_cinlcl_Cu' , limit_cinlcl,             mix,    lchnk ) 
     call outfld( 'limit_cin_Cu'    , limit_cin,                mix,    lchnk ) 
     call outfld( 'limit_cbmf_Cu'   , limit_cbmf,               mix,    lchnk ) 
     call outfld( 'limit_rei_Cu'    , limit_rei,                mix,    lchnk ) 
     call outfld( 'ind_delcin_Cu'   , ind_delcin,               mix,    lchnk ) 

    return

  end subroutine compute_uwshcu

  ! ------------------------------ !
  !                                ! 
  ! Beginning of subroutine blocks !
  !                                !
  ! ------------------------------ !

  subroutine getbuoy(pbot,thv0bot,ptop,thv0top,thvubot,thvutop,plfc,cin)
  ! ----------------------------------------------------------- !
  ! Subroutine to calculate integrated CIN [ J/kg = m2/s2 ] and !
  ! 'cinlcl, plfc' if any. Assume 'thv' is linear in each layer !
  ! both for cumulus and environment. Note that this subroutine !
  ! only include positive CIN in calculation - if there are any !
  ! negative CIN, it is assumed to be zero.    This is slightly !
  ! different from 'single_cin' below, where both positive  and !
  ! negative CIN are included.                                  !
  ! ----------------------------------------------------------- !
    real(r8) pbot,thv0bot,ptop,thv0top,thvubot,thvutop,plfc,cin,frc

    if( thvubot .gt. thv0bot .and. thvutop .gt. thv0top ) then
        plfc = pbot
        return
    elseif( thvubot .le. thv0bot .and. thvutop .le. thv0top ) then 
        cin  = cin - ( (thvubot/thv0bot - 1._r8) + (thvutop/thv0top - 1._r8)) * (pbot - ptop) /        &
                     ( pbot/(r*thv0bot*exnf(pbot)) + ptop/(r*thv0top*exnf(ptop)) )
    elseif( thvubot .gt. thv0bot .and. thvutop .le. thv0top ) then 
        frc  = ( thvutop/thv0top - 1._r8 ) / ( (thvutop/thv0top - 1._r8) - (thvubot/thv0bot - 1._r8) )
        cin  = cin - ( thvutop/thv0top - 1._r8 ) * ( (ptop + frc*(pbot - ptop)) - ptop ) /             &
                     ( pbot/(r*thv0bot*exnf(pbot)) + ptop/(r*thv0top*exnf(ptop)) )
    else            
        frc  = ( thvubot/thv0bot - 1._r8 ) / ( (thvubot/thv0bot - 1._r8) - (thvutop/thv0top - 1._r8) )
        plfc = pbot - frc * ( pbot - ptop )
        cin  = cin - ( thvubot/thv0bot - 1._r8)*(pbot - plfc)/                                         & 
                     ( pbot/(r*thv0bot*exnf(pbot)) + ptop/(r*thv0top * exnf(ptop)))
    endif

    return
  end subroutine getbuoy

  function single_cin(pbot,thv0bot,ptop,thv0top,thvubot,thvutop)
  ! ------------------------------------------------------- !
  ! Function to calculate a single layer CIN by summing all ! 
  ! positive and negative CIN.                              !
  ! ------------------------------------------------------- ! 
    real(r8) :: single_cin
    real(r8)    pbot,thv0bot,ptop,thv0top,thvubot,thvutop 

    single_cin = ( (1._r8 - thvubot/thv0bot) + (1._r8 - thvutop/thv0top)) * ( pbot - ptop ) / &
                 ( pbot/(r*thv0bot*exnf(pbot)) + ptop/(r*thv0top*exnf(ptop)) )
    return
  end function single_cin   


  subroutine conden(p,thl,qt,th,qv,ql,qi,rvls,id_check,qsat)
  ! --------------------------------------------------------------------- !
  ! Calculate thermodynamic properties from a given set of ( p, thl, qt ) !
  ! --------------------------------------------------------------------- !
    implicit none
    real(r8), intent(in)  :: p
    real(r8), intent(in)  :: thl
    real(r8), intent(in)  :: qt
    real(r8), intent(out) :: th
    real(r8), intent(out) :: qv
    real(r8), intent(out) :: ql
    real(r8), intent(out) :: qi
    real(r8), intent(out) :: rvls
    integer , intent(out) :: id_check
    integer , external    :: qsat
    real(r8)              :: tc,temps,t
    real(r8)              :: leff, nu, qc
    integer               :: iteration
    real(r8)              :: es(1)              ! Saturation vapor pressure
    real(r8)              :: qs(1)              ! Saturation spec. humidity
    real(r8)              :: gam(1)             ! (L/cp)*dqs/dT
    integer               :: status             ! Return status of qsat call

    tc   = thl*exnf(p)
  ! Modification : In order to be compatible with the dlf treatment in stratiform.F90,
  !                we may use ( 268.15, 238.15 ) with 30K ramping instead of 20 K,
  !                in computing ice fraction below. 
  !                Note that 'cldwat_fice' uses ( 243.15, 263.15 ) with 20K ramping for stratus.
    nu   = max(min((268._r8 - tc)/20._r8,1.0_r8),0.0_r8)  ! Fraction of ice in the condensate. 
    leff = (1._r8 - nu)*xlv + nu*xls                      ! This is an estimate that hopefully speeds convergence

    ! --------------------------------------------------------------------------- !
    ! Below "temps" and "rvls" are just initial guesses for iteration loop below. !
    ! Note that the output "temps" from the below iteration loop is "temperature" !
    ! NOT "liquid temperature".                                                   !
    ! --------------------------------------------------------------------------- !

    temps  = tc
    status = qsat(temps,p,es(1),qs(1),gam(1), 1)
    rvls   = qs(1)

    if( qs(1) .ge. qt ) then  
        id_check = 0
        qv = qt
        qc = 0._r8
        ql = 0._r8
        qi = 0._r8
        th = tc/exnf(p)
    else 
        do iteration = 1, 10
           temps  = temps + ( (tc-temps)*cp/leff + qt - rvls )/( cp/leff + ep2*leff*rvls/r/temps/temps )
           status = qsat(temps,p,es(1),qs(1),gam(1),1)
           rvls   = qs(1)
        end do
        qc = max(qt - qs(1),0._r8)
        qv = qt - qc
        ql = qc*(1._r8 - nu)
        qi = nu*qc
        th = temps/exnf(p)
        if( abs((temps-(leff/cp)*qc)-tc) .ge. 1._r8 ) then
            id_check = 1
        else
            id_check = 0
        end if
    end if

    return
  end subroutine conden

  subroutine roots(a,b,c,r1,r2,status)
  ! --------------------------------------------------------- !
  ! Subroutine to solve the second order polynomial equation. !
  ! I should check this subroutine later.                     !
  ! --------------------------------------------------------- !
    real(r8), intent(in)  :: a
    real(r8), intent(in)  :: b
    real(r8), intent(in)  :: c
    real(r8), intent(out) :: r1
    real(r8), intent(out) :: r2
    integer , intent(out) :: status
    real(r8)              :: q

    status = 0

    if( a .eq. 0._r8 ) then                            ! Form b*x + c = 0
        if( b .eq. 0._r8 ) then                        ! Failure: c = 0
            status = 1
        else                                           ! b*x + c = 0
            r1 = -c/b
        endif
        r2 = r1
    else
        if( b .eq. 0._r8 ) then                        ! Form a*x**2 + c = 0
            if( a*c .gt. 0._r8 ) then                  ! Failure: x**2 = -c/a < 0
                status = 2  
            else                                       ! x**2 = -c/a 
                r1 = sqrt(-c/a)
            endif
            r2 = -r1
       else                                            ! Form a*x**2 + b*x + c = 0
            if( (b**2 - 4._r8*a*c) .lt. 0._r8 ) then   ! Failure, no real roots
                 status = 3
            else
                 q  = -0.5_r8*(b + sign(1.0_r8,b)*sqrt(b**2 - 4._r8*a*c))
                 r1 =  q/a
                 r2 =  c/q
            endif
       endif
    endif

    return
  end subroutine roots
  
  function slope(mkx,field,p0)
  ! ------------------------------------------------------------------ !
  ! Function performing profile reconstruction of conservative scalars !
  ! in each layer. This is identical to profile reconstruction used in !
  ! UW-PBL scheme but from bottom to top layer here.     At the lowest !
  ! layer near to surface, slope is defined using the two lowest layer !
  ! mid-point values. I checked this subroutine and it is correct.     !
  ! ------------------------------------------------------------------ !
    real(r8)             :: slope(mkx)
    integer,  intent(in) :: mkx
    real(r8), intent(in) :: field(mkx)
    real(r8), intent(in) :: p0(mkx)
    
    real(r8)             :: below
    real(r8)             :: above
    integer              :: k

    below = ( field(2) - field(1) ) / ( p0(2) - p0(1) )
    do k = 2, mkx
       above = ( field(k) - field(k-1) ) / ( p0(k) - p0(k-1) )
       if( above .gt. 0._r8 ) then
           slope(k-1) = max(0._r8,min(above,below))
       else 
           slope(k-1) = min(0._r8,max(above,below))
       end if
       below = above
    end do
    slope(mkx) = slope(mkx-1)

    return
  end function slope

  function qsinvert(qt,thl,psfc,qsat)
  ! ----------------------------------------------------------------- !
  ! Function calculating saturation pressure ps (or pLCL) from qt and !
  ! thl ( liquid potential temperature,  NOT liquid virtual potential ! 
  ! temperature) by inverting Bolton formula. I should check later if !
  ! current use of 'leff' instead of 'xlv' here is reasonable or not. !
  ! ----------------------------------------------------------------- !
    real(r8)          :: qsinvert    
    real(r8)             qt, thl, psfc
    real(r8)             ps, Pis, Ts, err, dlnqsdT, dTdPis
    real(r8)             dPisdps, dlnqsdps, derrdps, dps 
    real(r8)             Ti, rhi, TLCL, PiLCL, psmin, dpsmax
    integer              i
    integer, external :: qsat
    real(r8)          :: es(1)                     ! saturation vapor pressure
    real(r8)          :: qs(1)                     ! saturation spec. humidity
    real(r8)          :: gam(1)                    ! (L/cp)*dqs/dT
    integer           :: status                    ! return status of qsat call
    real(r8)          :: leff, nu

    psmin  = 100._r8*100._r8 ! Default saturation pressure [Pa] if iteration does not converge
    dpsmax = 1._r8           ! Tolerance [Pa] for convergence of iteration

    ! ------------------------------------ !
    ! Calculate best initial guess of pLCL !
    ! ------------------------------------ !

    Ti       =  thl*(psfc/p00)**rovcp
    status   =  qsat(Ti,psfc,es(1),qs(1),gam(1),1)
    rhi      =  qt/qs(1)      
    if( rhi .le. 0.01_r8 ) then
        write(iulog,*) 'Source air is too dry and pLCL is set to psmin in uwshcu.F90' 
#ifdef WRF_PORT
        call wrf_message(iulog)
#endif
        qsinvert = psmin
        return
    end if
    TLCL     =  55._r8 + 1._r8/(1._r8/(Ti-55._r8)-log(rhi)/2840._r8); ! Bolton's formula. MWR.1980.Eq.(22)
    PiLCL    =  TLCL/thl
    ps       =  p00*(PiLCL)**(1._r8/rovcp)

    do i = 1, 10
       Pis      =  (ps/p00)**rovcp
       Ts       =  thl*Pis
       status   =  qsat(Ts,ps,es(1),qs(1),gam(1),1)
       err      =  qt - qs(1)
       nu       =  max(min((268._r8 - Ts)/20._r8,1.0_r8),0.0_r8)        
       leff     =  (1._r8 - nu)*xlv + nu*xls                   
       dlnqsdT  =  gam(1)*(cp/leff)/qs(1)
       dTdPis   =  thl
       dPisdps  =  rovcp*Pis/ps 
       dlnqsdps = -1._r8/(ps - (1._r8 - ep2)*es(1))
       derrdps  = -qs(1)*(dlnqsdT * dTdPis * dPisdps + dlnqsdps)
       dps      = -err/derrdps
       ps       =  ps + dps
       if( ps .lt. 0._r8 ) then
           write(iulog,*) 'pLCL iteration is negative and set to psmin in uwshcu.F90', qt, thl, psfc 
#ifdef WRF_PORT
           call wrf_message(iulog)
#endif
           qsinvert = psmin
           return    
       end if
       if( abs(dps) .le. dpsmax ) then
           qsinvert = ps
           return
       end if
    end do
    write(iulog,*) 'pLCL does not converge and is set to psmin in uwshcu.F90', qt, thl, psfc 
#ifdef WRF_PORT
           call wrf_message(iulog)
#endif
    qsinvert = psmin
    return
  end function qsinvert

  real(r8) function compute_alpha(del_CIN,ke)
  ! ------------------------------------------------ !
  ! Subroutine to compute proportionality factor for !
  ! implicit CIN calculation.                        !   
  ! ------------------------------------------------ !
    real(r8) :: del_CIN, ke
    real(r8) :: x0, x1

    integer  :: iteration

    x0 = 0._r8
    do iteration = 1, 10
       x1 = x0 - (exp(-x0*ke*del_CIN) - x0)/(-ke*del_CIN*exp(-x0*ke*del_CIN) - 1._r8)
       x0 = x1
    end do
    compute_alpha = x0

    return

  end function compute_alpha

  real(r8) function compute_mumin2(mulcl,rmaxfrac,mulow)
  ! --------------------------------------------------------- !
  ! Subroutine to compute critical 'mu' (normalized CIN) such ! 
  ! that updraft fraction at the LCL is equal to 'rmaxfrac'.  !
  ! --------------------------------------------------------- !  
    real(r8) :: mulcl, rmaxfrac, mulow
    real(r8) :: x0, x1, ex, ef, exf, f, fs
    integer  :: iteration

    x0 = mulow
    do iteration = 1, 10
       ex = exp(-x0**2)
       ef = erfc(x0)
       ! if(x0.ge.3._r8) then
       !    compute_mumin2 = 3._r8 
       !    goto 20
       ! endif 
       exf = ex/ef
       f  = 0.5_r8*exf**2 - 0.5_r8*(ex/2._r8/rmaxfrac)**2 - (mulcl*2.5066_r8/2._r8)**2
       fs = (2._r8*exf**2)*(exf/sqrt(3.141592_r8)-x0) + (0.5_r8*x0*ex**2)/(rmaxfrac**2)
       x1 = x0 - f/fs     
       x0 = x1
    end do
    compute_mumin2 = x0

 20 return

  end function compute_mumin2

  real(r8) function compute_ppen(wtwb,D,bogbot,bogtop,rho0j,dpen)
  ! ----------------------------------------------------------- !
  ! Subroutine to compute critical 'ppen[Pa]<0' ( pressure dis. !
  ! from 'ps0(kpen-1)' to the cumulus top where cumulus updraft !
  ! vertical velocity is exactly zero ) by considering exact    !
  ! non-zero fer(kpen).                                         !  
  ! ----------------------------------------------------------- !  
    real(r8) :: wtwb, D, bogbot, bogtop, rho0j, dpen
    real(r8) :: x0, x1, f, fs, SB, s00
    integer  :: iteration

    ! Buoyancy slope
      SB = ( bogtop - bogbot ) / dpen
    ! Sign of slope, 'f' at x = 0
    ! If 's00>0', 'w' increases with height.
      s00 = bogbot / rho0j - D * wtwb

    if( D*dpen .lt. 1.e-8 ) then
        if( s00 .ge. 0._r8 ) then
            x0 = dpen       
        else
            x0 = max(0._r8,min(dpen,-0.5_r8*wtwb/s00))
        endif
    else
        if( s00 .ge. 0._r8 ) then
            x0 = dpen
        else 
            x0 = 0._r8
        endif
        do iteration = 1, 5
           f  = exp(-2._r8*D*x0)*(wtwb-(bogbot-SB/(2._r8*D))/(D*rho0j)) + &
                                 (SB*x0+bogbot-SB/(2._r8*D))/(D*rho0j)
           fs = -2._r8*D*exp(-2._r8*D*x0)*(wtwb-(bogbot-SB/(2._r8*D))/(D*rho0j)) + &
                                 (SB)/(D*rho0j)
           if( fs .ge. 0._r8 ) then
		fs = max(fs, 1.e-10_r8)
           else
           	fs = min(fs,-1.e-10_r8)
           endif
           x1 = x0 - f/fs     
           x0 = x1
      end do

    endif    

    compute_ppen = -max(0._r8,min(dpen,x0))

  end function compute_ppen

  subroutine fluxbelowinv(cbmf,ps0,mkx,kinv,dt,xsrc,xmean,xtopin,xbotin,xflx)   
  ! ------------------------------------------------------------------------- !
  ! Subroutine to calculate turbulent fluxes at and below 'kinv-1' interfaces.!
  ! Check in the main program such that input 'cbmf' should not be zero.      !  
  ! If the reconstructed inversion height does not go down below the 'kinv-1' !
  ! interface, then turbulent flux at 'kinv-1' interface  is simply a product !
  ! of 'cmbf' and 'qtsrc-xbot' where 'xbot' is the value at the top interface !
  ! of 'kinv-1' layer. This flux is linearly interpolated down to the surface !
  ! assuming turbulent fluxes at surface are zero. If reconstructed inversion !
  ! height goes down below the 'kinv-1' interface, subsidence warming &drying !
  ! measured by 'xtop-xbot', where  'xtop' is the value at the base interface !
  ! of 'kinv+1' layer, is added ONLY to the 'kinv-1' layer, using appropriate !
  ! mass weighting ( rpinv and rcbmf, or rr = rpinv / rcbmf ) between current !
  ! and next provisional time step. Also impose a limiter to enforce outliers !
  ! of thermodynamic variables in 'kinv' layer  to come back to normal values !
  ! at the next step.                                                         !
  ! ------------------------------------------------------------------------- !            
    integer,  intent(in)                     :: mkx, kinv 
    real(r8), intent(in)                     :: cbmf, dt, xsrc, xmean, xtopin, xbotin
    real(r8), intent(in),  dimension(0:mkx)  :: ps0
    real(r8), intent(out), dimension(0:mkx)  :: xflx  
    integer k
    real(r8) rcbmf, rpeff, dp, rr, pinv_eff, xtop, xbot, pinv, xtop_ori, xbot_ori

    xflx(0:mkx) = 0._r8
    dp = ps0(kinv-1) - ps0(kinv) 
#ifndef WRF_PORT    
    if( abs(xbotin-xtopin) .le. 1.e-13_r8 ) then
        xbot = xbotin - 1.e-13_r8
        xtop = xtopin + 1.e-13_r8
    else
        xbot = xbotin
        xtop = xtopin
    endif
#else
    xbot = xbotin
    xtop = xtopin
#endif
    ! -------------------------------------- !
    ! Compute reconstructed inversion height !
    ! -------------------------------------- !
    xtop_ori = xtop
    xbot_ori = xbot
    rcbmf = ( cbmf * g * dt ) / dp                  ! Can be larger than 1 : 'OK'      
#ifndef WRF_PORT
    rpeff = ( xmean - xtop ) / ( xbot - xtop ) 
#else
    rpeff = ( xmean - xtop ) / max(1.e-13_r8, (xbot - xtop) ) 
    rpeff = abs(rpeff)
#endif
    rpeff = min( max(0._r8,rpeff), 1._r8 )          ! As of this, 0<= rpeff <= 1   
    if( rpeff .eq. 0._r8 .or. rpeff .eq. 1._r8 ) then
        xbot = xmean
        xtop = xmean
    endif
    ! Below two commented-out lines are the old code replacing the above 'if' block.   
    ! if(rpeff.eq.1) xbot = xmean
    ! if(rpeff.eq.0) xtop = xmean    
    rr       = rpeff / rcbmf
    pinv     = ps0(kinv-1) - rpeff * dp             ! "pinv" before detraining mass
    pinv_eff = ps0(kinv-1) + ( rcbmf - rpeff ) * dp ! Effective "pinv" after detraining mass
    ! ----------------------------------------------------------------------- !
    ! Compute turbulent fluxes.                                               !
    ! Below two cases exactly converges at 'kinv-1' interface when rr = 1._r8 !
    ! ----------------------------------------------------------------------- !
    do k = 0, kinv - 1
       xflx(k) = cbmf * ( xsrc - xbot ) * ( ps0(0) - ps0(k) ) / ( ps0(0) - pinv )
    end do
    if( rr .le. 1._r8 ) then
        xflx(kinv-1) =  xflx(kinv-1) - ( 1._r8 - rr ) * cbmf * ( xtop_ori - xbot_ori )
    endif

    return
  end subroutine fluxbelowinv

  subroutine positive_moisture_single( xlv, xls, mkx, dt, qvmin, qlmin, qimin, dp, qv, ql, qi, s, qvten, qlten, qiten, sten )
  ! ------------------------------------------------------------------------------- !
  ! If any 'ql < qlmin, qi < qimin, qv < qvmin' are developed in any layer,         !
  ! force them to be larger than minimum value by (1) condensating water vapor      !
  ! into liquid or ice, and (2) by transporting water vapor from the very lower     !
  ! layer. '2._r8' is multiplied to the minimum values for safety.                  !
  ! Update final state variables and tendencies associated with this correction.    !
  ! If any condensation happens, update (s,t) too.                                  !
  ! Note that (qv,ql,qi,s) are final state variables after applying corresponding   !
  ! input tendencies and corrective tendencies                                      !
  ! ------------------------------------------------------------------------------- !
    implicit none
    integer,  intent(in)     :: mkx
    real(r8), intent(in)     :: xlv, xls
    real(r8), intent(in)     :: dt, qvmin, qlmin, qimin
    real(r8), intent(in)     :: dp(mkx)
    real(r8), intent(inout)  :: qv(mkx), ql(mkx), qi(mkx), s(mkx)
    real(r8), intent(inout)  :: qvten(mkx), qlten(mkx), qiten(mkx), sten(mkx)
    integer   k
    real(r8)  dql, dqi, dqv, sum, aa, dum 

    do k = mkx, 1, -1        ! From the top to the 1st (lowest) layer from the surface
       dql = max(0._r8,1._r8*qlmin-ql(k))
       dqi = max(0._r8,1._r8*qimin-qi(k))
       qlten(k) = qlten(k) +  dql/dt
       qiten(k) = qiten(k) +  dqi/dt
       qvten(k) = qvten(k) - (dql+dqi)/dt
       sten(k)  = sten(k)  + xlv * (dql/dt) + xls * (dqi/dt)
       ql(k)    = ql(k) +  dql
       qi(k)    = qi(k) +  dqi
       qv(k)    = qv(k) -  dql - dqi
       s(k)     = s(k)  +  xlv * dql + xls * dqi
       dqv      = max(0._r8,1._r8*qvmin-qv(k))
       qvten(k) = qvten(k) + dqv/dt
       qv(k)    = qv(k)   + dqv
       if( k .ne. 1 ) then 
           qv(k-1)    = qv(k-1)    - dqv*dp(k)/dp(k-1)
           qvten(k-1) = qvten(k-1) - dqv*dp(k)/dp(k-1)/dt
       endif
       qv(k) = max(qv(k),qvmin)
       ql(k) = max(ql(k),qlmin)
       qi(k) = max(qi(k),qimin)
    end do
    ! Extra moisture used to satisfy 'qv(i,1)=qvmin' is proportionally 
    ! extracted from all the layers that has 'qv > 2*qvmin'. This fully
    ! preserves column moisture. 
    if( dqv .gt. 1.e-20_r8 ) then
        sum = 0._r8
        do k = 1, mkx
           if( qv(k) .gt. 2._r8*qvmin ) sum = sum + qv(k)*dp(k)
        enddo
        aa = dqv*dp(1)/max(1.e-20_r8,sum)
        if( aa .lt. 0.5_r8 ) then
            do k = 1, mkx
               if( qv(k) .gt. 2._r8*qvmin ) then
                   dum      = aa*qv(k)
                   qv(k)    = qv(k) - dum
                   qvten(k) = qvten(k) - dum/dt
               endif
            enddo 
        else 
            write(iulog,*) 'Full positive_moisture is impossible in uwshcu'
#ifdef WRF_PORT
            call wrf_message(iulog)
#endif
        endif
    endif 

    return
  end subroutine positive_moisture_single

  subroutine findsp (lchnk, ncol, q, t, p, tsp, qsp)

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
                           cp, epsqs, ttrice

  !
  !     input arguments
  !
   integer, intent(in) :: lchnk                 ! chunk identifier
   integer, intent(in) :: ncol                  ! number of atmospheric columns

   real(r8), intent(in) :: q(pcols,pver)        ! water vapor (kg/kg)
   real(r8), intent(in) :: t(pcols,pver)        ! temperature (K)
   real(r8), intent(in) :: p(pcols,pver)        ! pressure    (Pa)
!
! output arguments
!
   real(r8), intent(out) :: tsp(pcols,pver)      ! saturation temp (K)
   real(r8), intent(out) :: qsp(pcols,pver)      ! saturation mixing ratio (kg/kg)
!
! local variables
!
   integer i                 ! work variable
   integer k                 ! work variable
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
   real(r8) hltalt(pcols,pver)   ! lat. heat. of vap.
   real(r8) tterm                ! work var.
   real(r8) qs                   ! spec. hum. of water vapor
   real(r8) tc                   ! crit temp of transition to ice
   real(r8) tt0 

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
   trinv = 1.0_r8/ttrice
   a1 = 7.5_r8*log(10._r8)
   rair =  287.04_r8
   c3 = rair*a1/cp
   dtm = 0._r8    ! needed for iter=0 blowup with f90 -ei
   dqm = 0._r8    ! needed for iter=0 blowup with f90 -ei
   dttol = 1.e-4_r8 ! the relative temp error tolerance required to quit the iteration
   dqtol = 1.e-4_r8 ! the relative moisture error tolerance required to quit the iteration
   tt0 = 273.15_r8  ! Freezing temperature 
!  tmin = 173.16 ! the coldest temperature we can deal with
!
! max number of times to iterate the calculation
   iter = 8
!
   do k = 1,pver

!
! first guess on the wet bulb temperature
!
      do i = 1,ncol

#ifdef DEBUG
         if ( (lchnk == lchnklook(nlook) ) .and. (i == icollook(nlook) ) ) then
            write(iulog,*) ' '
#ifdef WRF_PORT
            call wrf_message(iulog)
#endif
            write(iulog,*) ' level, t, q, p', k, t(i,k), q(i,k), p(i,k)
#ifdef WRF_PORT
            call wrf_message(iulog)
#endif
         endif
#endif
! limit the temperature range to that relevant to the sat vap pres tables
#if ( ! defined WACCM_MOZART )
         tlim(i) = min(max(t(i,k),173._r8),373._r8)
#else
         tlim(i) = min(max(t(i,k),128._r8),373._r8)
#endif
         es = estblf(tlim(i))
         denom = p(i,k) - omeps*es
         qs = epsqs*es/denom
         doit(i) = 0
         enout(i) = 1._r8
! make sure a meaningful calculation is possible
         if (p(i,k) > 5._r8*es .and. qs > 0._r8 .and. qs < 0.5_r8) then
!
! Saturation specific humidity
!
             qs = min(epsqs*es/denom,1._r8)
!
! "generalized" analytic expression for t derivative of es
!  accurate to within 1 percent for 173.16 < t < 373.16
!
! Weighting of hlat accounts for transition from water to ice
! polynomial expression approximates difference between es over
! water and es over ice from 0 to -ttrice (C) (min of ttrice is
! -40): required for accurate estimate of es derivative in transition
! range from ice to water also accounting for change of hlatv with t
! above freezing where const slope is given by -2369 j/(kg c) = cpv - cw
!
             tc     = tlim(i) - tt0
             lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
             weight(i) = min(-tc*trinv,1.0_r8)
             hlatsb = hlatv + weight(i)*hlatf
             hlatvp = hlatv - 2369.0_r8*tc
             if (tlim(i) < tt0) then
                hltalt(i,k) = hlatsb
             else
                hltalt(i,k) = hlatvp
             end if
             enin(i) = cp*tlim(i) + hltalt(i,k)*q(i,k)

! make a guess at the wet bulb temp using a UKMO algorithm (from J. Petch)
             tmp =  q(i,k) - qs
             c1 = hltalt(i,k)*c3
             c2 = (tlim(i) + 36._r8)**2
             r1b    = c2/(c2 + c1*qs)
             qvd   = r1b*tmp
             tsp(i,k) = tlim(i) + ((hltalt(i,k)/cp)*qvd)
#ifdef DEBUG
             if ( (lchnk == lchnklook(nlook) ) .and. (i == icollook(nlook) ) ) then
                write(iulog,*) ' relative humidity ', q(i,k)/qs
#ifdef WRF_PORT
                call wrf_message(iulog)
#endif
                write(iulog,*) ' first guess ', tsp(i,k)
#ifdef WRF_PORT
                call wrf_message(iulog)
#endif
             endif
#endif
             es = estblf(tsp(i,k))
             qsp(i,k) = min(epsqs*es/(p(i,k) - omeps*es),1._r8)
          else
             doit(i) = 1
             tsp(i,k) = tlim(i)
             qsp(i,k) = q(i,k)
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
               es = estblf(tsp(i,k))
!
! Saturation specific humidity
!
               qs = min(epsqs*es/(p(i,k) - omeps*es),1._r8)
!
! "generalized" analytic expression for t derivative of es
! accurate to within 1 percent for 173.16 < t < 373.16
!
! Weighting of hlat accounts for transition from water to ice
! polynomial expression approximates difference between es over
! water and es over ice from 0 to -ttrice (C) (min of ttrice is
! -40): required for accurate estimate of es derivative in transition
! range from ice to water also accounting for change of hlatv with t
! above freezing where const slope is given by -2369 j/(kg c) = cpv - cw
!
               tc     = tsp(i,k) - tt0
               lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
               weight(i) = min(-tc*trinv,1.0_r8)
               hlatsb = hlatv + weight(i)*hlatf
               hlatvp = hlatv - 2369.0_r8*tc
               if (tsp(i,k) < tt0) then
                  hltalt(i,k) = hlatsb
               else
                  hltalt(i,k) = hlatvp
               end if
               if (lflg) then
                  tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3)+tc*(pcf(4) + tc*pcf(5))))
               else
                  tterm = 0.0_r8
               end if
               desdt = hltalt(i,k)*es/(rgasv*tsp(i,k)*tsp(i,k)) + tterm*trinv
               dqsdt = (epsqs + omeps*qs)/(p(i,k) - omeps*es)*desdt
!              g = cp*(tlim(i)-tsp(i,k)) + hltalt(i,k)*q(i,k)- hltalt(i,k)*qsp(i,k)
               g = enin(i) - (cp*tsp(i,k) + hltalt(i,k)*qsp(i,k))
               dgdt = -(cp + hltalt(i,k)*dqsdt)
               t1 = tsp(i,k) - g/dgdt
               dt = abs(t1 - tsp(i,k))/t1
               tsp(i,k) = max(t1,tmin)
               es = estblf(tsp(i,k))
               q1 = min(epsqs*es/(p(i,k) - omeps*es),1._r8)
               dq = abs(q1 - qsp(i,k))/max(q1,1.e-12_r8)
               qsp(i,k) = q1
#ifdef DEBUG
               if ( (lchnk == lchnklook(nlook) ) .and. (i == icollook(nlook) ) ) then
                  write(iulog,*) ' rel chg lev, iter, t, q ', k, l, dt, dq, g
#ifdef WRF_PORT
                  call wrf_message(iulog)
#endif
               endif
#endif
               dtm = max(dtm,dt)
               dqm = max(dqm,dq)
! if converged at this point, exclude it from more iterations
               if (dt < dttol .and. dq < dqtol) then
                  doit(i) = 2
               endif
               enout(i) = cp*tsp(i,k) + hltalt(i,k)*qsp(i,k)
! bail out if we are too near the end of temp range
#if ( ! defined WACCM_MOZART )
               if (tsp(i,k) < 174.16_r8) then
#else
               if (tsp(i,k) < 130.16_r8) then
#endif
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
                  write(iulog,*) ' findsp not converging at point i, k ', i, k
#ifdef WRF_PORT
                  call wrf_message(iulog)
#endif
                  write(iulog,*) ' t, q, p, enin ', t(i,k), q(i,k), p(i,k), enin(i)
#ifdef WRF_PORT
                  call wrf_message(iulog)
#endif
                  write(iulog,*) ' tsp, qsp, enout ', tsp(i,k), qsp(i,k), enout(i)
#ifdef WRF_PORT
                  call wrf_message(iulog)
#endif
                  call endrun ('FINDSP')
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
               write(iulog,*) ' the enthalpy is not conserved for point ', &
                  i, k, enin(i), enout(i)
#ifdef WRF_PORT
               call wrf_message(iulog)
#endif
               write(iulog,*) ' t, q, p, enin ', t(i,k), q(i,k), p(i,k), enin(i)
#ifdef WRF_PORT
               call wrf_message(iulog)
#endif
               write(iulog,*) ' tsp, qsp, enout ', tsp(i,k), qsp(i,k), enout(i)
#ifdef WRF_PORT
               call wrf_message(iulog)
#endif
               call endrun ('FINDSP')
            endif
         end do
      endif
      
   end do                    ! level loop (k=1,pver)

   return
   end subroutine findsp

  ! ------------------------ !
  !                          ! 
  ! End of subroutine blocks !
  !                          !
  ! ------------------------ !

  end module uwshcu

