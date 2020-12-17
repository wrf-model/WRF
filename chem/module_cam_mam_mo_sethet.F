#define WRF_PORT
module mo_sethet

!
! LKE (10/11/2010): added HCN, CH3CN, HCOOH  to cesm1_0_beta07_offline version
!                   HCN, CH3CN have new Henry's Law coefficients, HCOOH is set to CH3COOH
! LKE (10/18/2010): SO2 washout corrected based on recommendation of R.Easter, PNNL
!
#ifndef WRF_PORT
  use cam_logfile, only: iulog
  use gas_wetdep_opts, only: gas_wetdep_cnt, gas_wetdep_method, gas_wetdep_list
#else
  use module_cam_support, only:iulog, gas_wetdep_method, gas_wetdep_list, gas_wetdep_cnt
#endif

  private
  public :: sethet_inti, sethet

  save

  integer :: h2o2_ndx, hno3_ndx, ch2o_ndx, ch3ooh_ndx, ch3coooh_ndx, &
       ho2no2_ndx, ch3cocho_ndx, xooh_ndx, onitr_ndx, glyald_ndx, &
       ch3cho_ndx, mvk_ndx, macr_ndx, pooh_ndx, c2h5ooh_ndx, &
       c3h7ooh_ndx, rooh_ndx, isopno3_ndx, onit_ndx, Pb_ndx, &
       macrooh_ndx, isopooh_ndx, ch3oh_ndx, c2h5oh_ndx, hyac_ndx, hydrald_ndx
  integer :: spc_h2o2_ndx, spc_hno3_ndx
  integer :: spc_so2_ndx

  integer :: alkooh_ndx, mekooh_ndx, tolooh_ndx, terpooh_ndx, ch3cooh_ndx
  integer :: so2_ndx, soa_ndx, so4_ndx, cb2_ndx, oc2_ndx, nh3_ndx, nh4no3_ndx, &
             sa1_ndx, sa2_ndx, sa3_ndx, sa4_ndx, nh4_ndx, h2so4_ndx
  integer :: xisopno3_ndx,xho2no2_ndx,xonitr_ndx,xhno3_ndx,xonit_ndx
  integer :: clono2_ndx, brono2_ndx, hcl_ndx, n2o5_ndx, hocl_ndx, hobr_ndx, hbr_ndx 
  integer :: ch3cn_ndx, hcn_ndx, hcooh_ndx
  integer, allocatable :: wetdep_map(:)
  logical :: do_wetdep
  

contains
  subroutine sethet_inti

    !-----------------------------------------------------------------------      
    !       ... intialize the wet removal rate constants routine
    !-----------------------------------------------------------------------      

    use mo_chem_utls, only : get_het_ndx, get_spc_ndx
#ifndef WRF_PORT
    use spmd_utils,   only : masterproc
    use abortutils,   only : endrun
#else
    use module_cam_support, only: masterproc, endrun
#endif

    integer :: k, m

    do_wetdep = gas_wetdep_cnt>0 .and. gas_wetdep_method=='MOZ'
    if ( .not. do_wetdep) return

    if(.not.allocated(wetdep_map))allocate( wetdep_map(gas_wetdep_cnt))

    do k=1,gas_wetdep_cnt
       m = get_het_ndx( trim(gas_wetdep_list(k))) 
       if (m>0) then
          wetdep_map(k) = m
       else
          call endrun('sethet_inti: cannot map '//trim(gas_wetdep_list(k)))
       endif
    enddo

    xisopno3_ndx = get_het_ndx( 'XISOPNO3' )
    xho2no2_ndx  = get_het_ndx( 'XHO2NO2' )
    xonitr_ndx   = get_het_ndx( 'XONITR' )
    xhno3_ndx    = get_het_ndx( 'XHNO3' )
    xonit_ndx    = get_het_ndx( 'XONIT' )

    spc_h2o2_ndx = get_spc_ndx( 'H2O2' )
    spc_hno3_ndx = get_spc_ndx( 'HNO3' )
    spc_so2_ndx  = get_spc_ndx( 'SO2' )

    clono2_ndx = get_het_ndx( 'CLONO2' )
    brono2_ndx = get_het_ndx( 'BRONO2' )
    hcl_ndx    = get_het_ndx( 'HCL' )
    n2o5_ndx   = get_het_ndx( 'N2O5' )
    hocl_ndx   = get_het_ndx( 'HOCL' )
    hobr_ndx   = get_het_ndx( 'HOBR' )
    hbr_ndx    = get_het_ndx( 'HBR' )

    h2o2_ndx   = get_het_ndx( 'H2O2' )
    hno3_ndx   = get_het_ndx( 'HNO3' )
    ch2o_ndx   = get_het_ndx( 'CH2O' )
    ch3ooh_ndx = get_het_ndx( 'CH3OOH' )
    ch3coooh_ndx = get_het_ndx( 'CH3COOOH' )
    ho2no2_ndx  = get_het_ndx( 'HO2NO2' )
    ch3cocho_ndx = get_het_ndx( 'CH3COCHO' )
    xooh_ndx    = get_het_ndx( 'XOOH' )
    onitr_ndx   = get_het_ndx( 'ONITR' )
    glyald_ndx  = get_het_ndx( 'GLYALD' )
    ch3cho_ndx  = get_het_ndx( 'CH3CHO' )
    mvk_ndx     = get_het_ndx( 'MVK' )
    macr_ndx    = get_het_ndx( 'MACR' )
    pooh_ndx    = get_het_ndx( 'POOH' )
    c2h5ooh_ndx = get_het_ndx( 'C2H5OOH' )
    c3h7ooh_ndx = get_het_ndx( 'C3H7OOH' )
    rooh_ndx    = get_het_ndx( 'ROOH' )
    isopno3_ndx = get_het_ndx( 'ISOPNO3' )
    onit_ndx    = get_het_ndx( 'ONIT' )
    Pb_ndx      = get_het_ndx( 'Pb' )
    macrooh_ndx = get_het_ndx( 'MACROOH' )
    isopooh_ndx = get_het_ndx( 'ISOPOOH' )
    ch3oh_ndx   = get_het_ndx( 'CH3OH' )
    c2h5oh_ndx  = get_het_ndx( 'C2H5OH' )
    hyac_ndx    = get_het_ndx( 'HYAC' )
    hydrald_ndx = get_het_ndx( 'HYDRALD' )
    alkooh_ndx  = get_het_ndx( 'ALKOOH' )
    mekooh_ndx  = get_het_ndx( 'MEKOOH' )
    tolooh_ndx  = get_het_ndx( 'TOLOOH' )
    terpooh_ndx = get_het_ndx( 'TERPOOH' )
    ch3cooh_ndx = get_het_ndx( 'CH3COOH' )
    so2_ndx     = get_het_ndx( 'SO2' )
    soa_ndx     = get_het_ndx( 'SOA' )
    so4_ndx     = get_het_ndx( 'SO4' )
    cb2_ndx     = get_het_ndx( 'CB2' )
    oc2_ndx     = get_het_ndx( 'OC2' )
    nh3_ndx     = get_het_ndx( 'NH3' )
    nh4no3_ndx  = get_het_ndx( 'NH4NO3' )
    nh4_ndx     = get_het_ndx( 'NH4' )
    h2so4_ndx   = get_het_ndx( 'H2SO4' )
    sa1_ndx     = get_het_ndx( 'SA1' )
    sa2_ndx     = get_het_ndx( 'SA2' )
    sa3_ndx     = get_het_ndx( 'SA3' )
    sa4_ndx     = get_het_ndx( 'SA4' )
    ch3cn_ndx   = get_het_ndx( 'CH3CN' )
    hcn_ndx     = get_het_ndx( 'HCN' )
    hcooh_ndx   = get_het_ndx( 'HCOOH' )

    if (masterproc) then
       write(iulog,*) 'sethet_inti: new ndx ',so2_ndx,soa_ndx,so4_ndx,cb2_ndx,oc2_ndx, &
            nh3_ndx,nh4no3_ndx,sa1_ndx,sa2_ndx,sa3_ndx,sa4_ndx
       write(iulog,*) ' '
       write(iulog,*) 'sethet_inti: diagnotics '
#ifndef WRF_PORT
       write(iulog,'(10i5)') h2o2_ndx, hno3_ndx, ch2o_ndx, ch3ooh_ndx, ch3coooh_ndx, &
            ho2no2_ndx, ch3cocho_ndx, xooh_ndx, onitr_ndx, glyald_ndx, &
            ch3cho_ndx, mvk_ndx, macr_ndx, pooh_ndx, c2h5ooh_ndx, &
            c3h7ooh_ndx, rooh_ndx, isopno3_ndx, onit_ndx, Pb_ndx, &
            macrooh_ndx, isopooh_ndx, ch3oh_ndx, c2h5oh_ndx, hyac_ndx, hydrald_ndx
#else
       write(iulog,'(26i5)') h2o2_ndx, hno3_ndx, ch2o_ndx, ch3ooh_ndx, ch3coooh_ndx, &
            ho2no2_ndx, ch3cocho_ndx, xooh_ndx, onitr_ndx, glyald_ndx, &
            ch3cho_ndx, mvk_ndx, macr_ndx, pooh_ndx, c2h5ooh_ndx, &
            c3h7ooh_ndx, rooh_ndx, isopno3_ndx, onit_ndx, Pb_ndx, &
            macrooh_ndx, isopooh_ndx, ch3oh_ndx, c2h5oh_ndx, hyac_ndx, hydrald_ndx

#endif
    endif

  end subroutine sethet_inti

  subroutine sethet( het_rates, press, zmid,  phis, tfld, &
                     cmfdqr, nrain, nevapr, delt, xhnm, &
                     qin, ncol, lchnk                   &
#ifdef WRF_PORT
                     ,rlat                              &
#endif
                     )
    !-----------------------------------------------------------------------      
    !       ... compute rainout loss rates (1/s)
    !-----------------------------------------------------------------------      

    use shr_kind_mod, only : r8 => shr_kind_r8
    use physconst,    only : rga,pi
#ifndef WRF_PORT
    use chem_mods,    only : hetcnt, gas_pcnst
    use ppgrid,       only : pver, pcols
    use phys_grid,    only : get_rlat_all_p
    use abortutils,   only : endrun
#else
    use module_cam_support,        only: pcols, pver, endrun, &
         gas_pcnst => gas_pcnst_modal_aero, hetcnt => gas_pcnst_modal_aero 
#endif

    implicit none
    !-----------------------------------------------------------------------      
    !       ... dummy arguments
    !-----------------------------------------------------------------------      
    integer, intent(in)   ::    ncol                        ! columns in chunk
    integer, intent(in)   ::    lchnk                       ! chunk index
    real(r8), intent(in)  ::    delt                        ! time step ( s )
    real(r8), intent(in)  ::    press(pcols,pver)           ! pressure in pascals
    real(r8), intent(in)  ::    cmfdqr(ncol,pver)           ! dq/dt for convection
    real(r8), intent(in)  ::    nrain(ncol,pver)            ! stratoform precip
    real(r8), intent(in)  ::    nevapr(ncol,pver)           ! evaporation
    real(r8), intent(in)  ::    qin(ncol,pver,gas_pcnst)    ! xported species ( vmr )
    real(r8), intent(in)  ::    zmid(ncol,pver)             ! midpoint geopot (km)
    real(r8), intent(in)  ::    phis(pcols)                 ! surf geopot
    real(r8), intent(in)  ::    tfld(pcols,pver)            ! temperature (k)
    real(r8), intent(in)  ::    xhnm(ncol,pver)             ! total atms density ( /cm^3)
    real(r8), intent(out) ::    het_rates(ncol,pver,hetcnt) ! rainout loss rates

    !-----------------------------------------------------------------------      
    !       ... local variables
    !-----------------------------------------------------------------------      
    real(r8), parameter ::  boltz = 1.38044e-16_r8      ! erg/K
    real(r8), parameter ::  avo   = 6.023e23_r8         ! 1/mol
    real(r8), parameter ::  xrm   = .189_r8             ! mean diameter of rain drop (cm)
    real(r8), parameter ::  xum   = 748._r8             ! mean rain drop terminal velocity (cm/s)
    real(r8), parameter ::  xvv   = 6.18e-2_r8          ! kinetic viscosity (cm^2/s)
    real(r8), parameter ::  xdg   = .112_r8             ! mass transport coefficient (cm/s)
    real(r8), parameter ::  t0    = 298._r8             ! reference temperature (k)
    real(r8), parameter ::  xph0  = 1.e-5_r8            ! cloud [h+]
    real(r8), parameter ::  satf_hno3  = .016_r8        ! saturation factor for hno3 in clouds 
    real(r8), parameter ::  satf_h2o2  = .016_r8        ! saturation factor for h2o2 in clouds 
    real(r8), parameter ::  satf_so2   = .016_r8        ! saturation factor for so2 in clouds 
    real(r8), parameter ::  satf_ch2o  = .1_r8          ! saturation factor for ch2o in clouds 
    real(r8), parameter ::  const0   = boltz * 1.e-6_r8 ! (atmospheres/deg k/cm^3)
    real(r8), parameter ::  hno3_diss = 15.4_r8         ! hno3 dissociation constant
    real(r8), parameter ::  geo_fac  = 6._r8            ! geometry factor (surf area/volume = geo_fac/diameter)
    real(r8), parameter ::  mass_air = 29._r8           ! mass of background atmosphere (amu)
    real(r8), parameter ::  mass_h2o = 18._r8           ! mass of water vapor (amu)
    real(r8), parameter ::  h2o_mol  = 1.e3_r8/mass_h2o ! (gm/mol water)
    real(r8), parameter ::  km2cm    = 1.e5_r8          ! convert km to cm
    real(r8), parameter ::  m2km     = 1.e-3_r8         ! convert m to km
    real(r8), parameter ::  cm3_2_m3 = 1.e-6_r8         ! convert cm^3 to m^3
    real(r8), parameter ::  m3_2_cm3 = 1.e6_r8          ! convert m^3 to cm^3
    real(r8), parameter ::  liter_per_gram = 1.e-3_r8
    real(r8), parameter ::  avo2  = avo * liter_per_gram * cm3_2_m3 ! (liter/gm/mol*(m/cm)^3)

    integer  ::      i, m, k, kk                 ! indicies
    real(r8) ::      xkgm                        ! mass flux on rain drop
    real(r8) ::      all1, all2                  ! work variables
    real(r8) ::      stay                        ! fraction of layer traversed by falling drop in timestep delt
    real(r8) ::      xeqca1, xeqca2, xca1, xca2, xdtm
    real(r8) ::      xxx1, xxx2, yhno3, yh2o2
    real(r8) ::      all3, xeqca3, xca3, xxx3, yso2, so2_diss

    real(r8), dimension(ncol)  :: &
         xk0, work1, work2, work3, zsurf
    real(r8), dimension(pver)  :: &
         xgas1, xgas2
    real(r8), dimension(pver)  :: xgas3
    real(r8), dimension(ncol)  :: &
         tmp0_rates, tmp1_rates
    real(r8), dimension(ncol,pver)  :: &
         delz, &              ! layer depth about interfaces (cm)
         xhno3, &             ! hno3 concentration (molecules/cm^3)
         xh2o2, &             ! h2o2 concentration (molecules/cm^3)
         xso2, &              ! so2 concentration (molecules/cm^3)
         xliq, &              ! liquid rain water content in a grid cell (gm/m^3)
         rain                 ! conversion rate of water vapor into rain water (molecules/cm^3/s)
    real(r8), dimension(ncol,pver)  :: &
         xhen_hno3, xhen_h2o2, xhen_ch2o, xhen_ch3ooh, xhen_ch3co3h, &
         xhen_ch3cocho, xhen_xooh, xhen_onitr, xhen_ho2no2, xhen_glyald, &
         xhen_ch3cho, xhen_mvk, xhen_macr
    real(r8), dimension(ncol,pver)  :: &
         xhen_nh3, xhen_ch3cooh
    real(r8), dimension(ncol,pver,3) :: tmp_hetrates
    real(r8), dimension(ncol,pver)  :: precip
    real(r8), dimension(ncol,pver)  :: xhen_hcn, xhen_ch3cn, xhen_so2

    integer    ::      ktop_all       
    integer    ::      ktop(ncol)                  ! 100 mb level
#ifndef WRF_PORT
    real(r8) :: rlat(pcols)                       ! latitude in radians for columns
#else
    real(r8), intent(in) :: rlat(pcols)                       ! latitude in radians for columns
#endif
    real(r8) :: p_limit
    real(r8), parameter :: d2r = pi/180._r8
!
! jfl : new variables for rescaling sum of positive values to actual amount
!
    real(r8) :: total_rain,total_pos
    character(len=3) :: hetcntstrg
    real(r8), parameter :: MISSING = -999999._r8
    integer ::  mm

!
    !-----------------------------------------------------------------
    !        note: the press array is in pascals and must be
    !              mutiplied by 10 to yield dynes/cm**2.
    !-----------------------------------------------------------------
    !       ... set wet deposition for
    !           1. h2o2         2. hno3
    !           3. ch2o         4. ch3ooh
    !           5. pooh         6. ch3coooh
    !           7. ho2no2       8. onit
    !           9. mvk         10. macr
    !          11. c2h5ooh     12. c3h7ooh
    !          13. rooh        14. ch3cocho
    !          15. pb          16. macrooh
    !          17. xooh        18. onitr
    !          19. isopooh     20. ch3oh
    !          21. c2h5oh      22. glyald
    !          23. hyac        24. hydrald
    !          25. ch3cho      26. isopno3
    !-----------------------------------------------------------------

    het_rates(:,:,:) = 0._r8

    if ( .not. do_wetdep) return
#ifndef WRF_PORT
    call get_rlat_all_p(lchnk, ncol, rlat)
#endif

    do mm = 1,gas_wetdep_cnt
       m = wetdep_map(mm)
       if ( m>0 ) then
          het_rates(:,:,m) = MISSING
       endif
    end do

    !-----------------------------------------------------------------
    !	... the 2 and .6 multipliers are from a formula by frossling (1938)
    !-----------------------------------------------------------------
    xkgm = xdg/xrm * 2._r8 + xdg/xrm * .6_r8 * sqrt( xrm*xum/xvv ) * (xvv/xdg)**(1._r8/3._r8) 

    !-----------------------------------------------------------------
    !	... Find 100 mb level
    !-----------------------------------------------------------------
    do i = 1,ncol
#ifndef WRF_PORT
       if ( abs(rlat(i)) > 60._r8*d2r ) then
#else
       if ( abs(rlat(i)) > 60._r8 ) then
#endif
          p_limit = 300.e2_r8
       else
          p_limit = 100.e2_r8
       endif
#ifdef WRF_PORT       
       !BSINGH - PMA advised on setting up the following limit for
       ! tropics in case of WRF model when press is always
       !greater than p_limit
       p_limit = max (p_limit, press(i,2)) 
#endif

       k_loop: do k = pver,1,-1
          if( press(i,k) < p_limit ) then
             ktop(i) = k
             exit k_loop
          end if
       end do k_loop
    end do
    ktop_all = minval( ktop(:) )
!
! jfl
!
! this is added to rescale the variable precip (which can only be positive)
! to the actual vertical integral of positive and negative values.  This
! removes point storms
!
    do i = 1,ncol
       total_rain = 0.
       total_pos  = 0.
       do k = 1,pver
          precip(i,k) = cmfdqr(i,k) + nrain(i,k) - nevapr(i,k)
          total_rain = total_rain + precip(i,k)
          if ( precip(i,k) < 0. ) precip(i,k) = 0.
          total_pos  = total_pos  + precip(i,k)
       end do
       if ( total_rain <= 0. ) then
          precip(i,:) = 0.
       else
          do k = 1,pver
             precip(i,k) = precip(i,k) * total_rain/total_pos
          end do
       end if
    end do

    do k = 1,pver
       !jfl       precip(:ncol,k) = cmfdqr(:ncol,k) + nrain(:ncol,k) - nevapr(:ncol,k)
       rain(:ncol,k)   = mass_air*precip(:ncol,k)*xhnm(:ncol,k) / mass_h2o
       xliq(:ncol,k)   = precip(:ncol,k) * delt * xhnm(:ncol,k) / avo*mass_air * m3_2_cm3
       if( spc_hno3_ndx > 0 ) then
          xhno3(:ncol,k)  = qin(:ncol,k,spc_hno3_ndx) * xhnm(:ncol,k)
       else
          xhno3(:ncol,k)  = 0._r8
       end if
       if( spc_h2o2_ndx > 0 ) then
          xh2o2(:ncol,k)  = qin(:ncol,k,spc_h2o2_ndx) * xhnm(:ncol,k)
       else
          xh2o2(:ncol,k)  = 0._r8
       end if
       if( spc_so2_ndx > 0 ) then
          xso2(:ncol,k)  = qin(:ncol,k,spc_so2_ndx) * xhnm(:ncol,k)
       else
          xso2(:ncol,k)  = 0._r8
       end if
    end do

    zsurf(:ncol) = m2km * phis(:ncol) * rga
    do k = ktop_all,pver-1
       delz(:ncol,k) = abs( (zmid(:ncol,k) - zmid(:ncol,k+1))*km2cm ) 
    end do
    delz(:ncol,pver) = abs( (zmid(:ncol,pver) - zsurf(:ncol) )*km2cm ) 

    !-----------------------------------------------------------------
    !       ... part 0b,  for temperature dependent of henrys
    !                     xxhe1 = henry con for hno3
    !                     xxhe2 = henry con for h2o2
    !lwh 10/00 -- take henry''s law constants from brasseur et al. [1999],
    !             appendix j. for hno3, also consider dissociation to
    !             get effective henry''s law constant; equilibrium
    !             constant for dissociation from brasseur et al. [1999],
    !             appendix k. assume ph=5 (set as xph0 above).
    !             heff = h*k/[h+] for hno3 (complete dissociation)
    !             heff = h for h2o2 (no dissociation)
    !             heff = h * (1 + k/[h+]) (in general)
    !-----------------------------------------------------------------
    do k = ktop_all,pver
       work1(:ncol) = (t0 - tfld(:ncol,k))/(t0*tfld(:ncol,k))
       !-----------------------------------------------------------------
       ! 	... effective henry''s law constants:
       !	hno3, h2o2, ch2o, ch3ooh, ch3coooh (brasseur et al., 1999)
       !       xooh, onitr, macrooh (j.-f. muller; brocheton, 1999)
       !       isopooh (equal to hno3, as for macrooh)
       !       ho2no2 (mozart-1)
       !       ch3cocho, hoch2cho (betterton and hoffman, environ. sci. technol., 1988)
       !       ch3cho (staudinger and roberts, crit. rev. sci. technol., 1996)
       !       mvk, macr (allen et al., environ. toxicol. chem., 1998)
       !-----------------------------------------------------------------
       xk0(:)             = 2.1e5_r8 *exp( 8700._r8*work1(:) )
       xhen_hno3(:,k)     = xk0(:) * ( 1._r8 + hno3_diss / xph0 )
       xhen_h2o2(:,k)     = 7.45e4_r8 * exp( 6620._r8 * work1(:) )
       xhen_ch2o(:,k)     = 6.3e3_r8 * exp( 6460._r8 * work1(:) )
       xhen_ch3ooh(:,k)   = 2.27e2_r8 * exp( 5610._r8 * work1(:) )
       xhen_ch3co3h(:,k)  = 4.73e2_r8 * exp( 6170._r8 * work1(:) )
       xhen_ch3cocho(:,k) = 3.70e3_r8 * exp( 7275._r8 * work1(:) )
       xhen_xooh(:,k)     = 90.5_r8 * exp( 5607._r8 * work1(:) )
       xhen_onitr(:,k)    = 7.51e3_r8 * exp( 6485._r8 * work1(:) )
       xhen_ho2no2(:,k)   = 2.e4_r8
       xhen_glyald(:,k)   = 4.1e4_r8 * exp( 4600._r8 * work1(:) )
       xhen_ch3cho(:,k)   = 1.4e1_r8 * exp( 5600._r8 * work1(:) )
       xhen_mvk(:,k)      = 21._r8 * exp( 7800._r8 * work1(:) )
       xhen_macr(:,k)     = 4.3_r8 * exp( 5300._r8 * work1(:) )
       xhen_ch3cooh(:,k)  = 4.1e3_r8 * exp( 6300._r8 * work1(:) )
       !
       ! calculation for NH3 using the parameters in drydep_tables.F90
       !
       xhen_nh3 (:,k)     = 1.e6
       xhen_ch3cn(:,k)     = 50._r8 * exp( 4000._r8 * work1(:) )
       xhen_hcn(:,k)       = 12._r8 * exp( 5000._r8 * work1(:) )
       do i = 1, ncol
          so2_diss        = 1.23e-2_r8 * exp( 1960._r8 * work1(i) )
          xhen_so2(i,k)   = 1.23_r8 * exp( 3120._r8 * work1(i) ) * ( 1._r8 + so2_diss / xph0 )
       end do
       !
       tmp_hetrates(:,k,:) = 0._r8
    end do

    !-----------------------------------------------------------------
    !       ... part 1, solve for high henry constant ( hno3, h2o2)
    !-----------------------------------------------------------------
    col_loop :  do i = 1,ncol
       xgas1(:) = xhno3(i,:)                     ! xgas will change during 
       xgas2(:) = xh2o2(i,:)                     ! different levels wash 
       xgas3(:) = xso2( i,:)
       level_loop1  : do kk = ktop(i),pver
          stay = 1._r8
          if( rain(i,kk) /= 0._r8 ) then            ! finding rain cloud           
             all1 = 0._r8                           ! accumulation to justisfy saturation
             all2 = 0._r8 
             all3 = 0._r8 
             stay = ((zmid(i,kk) - zsurf(i))*km2cm)/(xum*delt)
             stay = min( stay,1._r8 )
             !-----------------------------------------------------------------
             !       ... calculate the saturation concentration eqca
             !-----------------------------------------------------------------
             do k = kk,pver                      ! cal washout below cloud
                xeqca1 =  xgas1(k) &
                     / (xliq(i,kk)*avo2 + 1._r8/(xhen_hno3(i,k)*const0*tfld(i,k))) &
                     *  xliq(i,kk)*avo2
                xeqca2 =  xgas2(k) &
                     / (xliq(i,kk)*avo2 + 1._r8/(xhen_h2o2(i,k)*const0*tfld(i,k))) &
                     *  xliq(i,kk)*avo2
                xeqca3 =  xgas3(k) &
                     / (xliq(i,kk)*avo2 + 1._r8/(xhen_so2( i,k)*const0*tfld(i,k))) &
                     *  xliq(i,kk)*avo2
                !-----------------------------------------------------------------
                !       ... calculate ca; inside cloud concentration in #/cm3(air)
                !-----------------------------------------------------------------
                xca1 = geo_fac*xkgm*xgas1(k)/(xrm*xum)*delz(i,k) * xliq(i,kk) * cm3_2_m3
                xca2 = geo_fac*xkgm*xgas2(k)/(xrm*xum)*delz(i,k) * xliq(i,kk) * cm3_2_m3
                xca3 = geo_fac*xkgm*xgas3(k)/(xrm*xum)*delz(i,k) * xliq(i,kk) * cm3_2_m3
                !-----------------------------------------------------------------
                !       ... if is not saturated
                !               hno3(gas)_new = hno3(gas)_old - hno3(h2o)
                !           otherwise
                !               hno3(gas)_new = hno3(gas)_old
                !-----------------------------------------------------------------
                all1 = all1 + xca1
                all2 = all2 + xca2
                if( all1 < xeqca1 ) then
                   xgas1(k) = max( xgas1(k) - xca1,0._r8 )
                end if
                if( all2 < xeqca2 ) then
                   xgas2(k) = max( xgas2(k) - xca2,0._r8 )
                end if
                all3 = all3 + xca3
                if( all3 < xeqca3 ) then
                   xgas3(k) = max( xgas3(k) - xca3,0._r8 )
                end if
             end do
          end if
          !-----------------------------------------------------------------
          !       ... calculate the lifetime of washout (second)
          !             after all layers washout 
          !             the concentration of hno3 is reduced 
          !             then the lifetime xtt is calculated by
          !
          !                  xtt = (xhno3(ini) - xgas1(new))/(dt*xhno3(ini))
          !                  where dt = passing time (s) in vertical
          !                             path below the cloud
          !                        dt = dz(cm)/um(cm/s)
          !-----------------------------------------------------------------
          xdtm = delz(i,kk) / xum                     ! the traveling time in each dz
          xxx1 = (xhno3(i,kk) - xgas1(kk))
          xxx2 = (xh2o2(i,kk) - xgas2(kk))
          if( xxx1 /= 0._r8 ) then                       ! if no washout lifetime = 1.e29
             yhno3  = xhno3(i,kk)/xxx1 * xdtm    
          else
             yhno3  = 1.e29_r8
          end if
          if( xxx2 /= 0._r8 ) then                       ! if no washout lifetime = 1.e29
             yh2o2  = xh2o2(i,kk)/xxx2 * xdtm     
          else
             yh2o2  = 1.e29_r8
          end if
          tmp_hetrates(i,kk,1) = max( 1._r8 / yh2o2,0._r8 ) * stay
          tmp_hetrates(i,kk,2) = max( 1._r8 / yhno3,0._r8 ) * stay
          xxx3 = (xso2( i,kk) - xgas3(kk))
          if( xxx3 /= 0._r8 ) then                       ! if no washout lifetime = 1.e29
             yso2  = xso2( i,kk)/xxx3 * xdtm     
          else
             yso2  = 1.e29_r8
          end if
          tmp_hetrates(i,kk,3) = max( 1._r8 / yso2, 0._r8 ) * stay
       end do level_loop1
    end do col_loop

    !-----------------------------------------------------------------
    !       ... part 2, in-cloud solve for low henry constant
    !                   hno3 and h2o2 have both in and under cloud
    !-----------------------------------------------------------------
    level_loop2 : do k = ktop_all,pver
       Column_loop2 : do i=1,ncol
          if ( rain(i,k) <= 0._r8 ) then
             het_rates(i,k,:) =  0._r8 
             cycle
          endif

          work1(i) = avo2 * xliq(i,k)
          work2(i) = const0 * tfld(i,k)
          work3(i) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_ch2o(i,k)*work2(i)))),0._r8 ) &
               * satf_ch2o
          if( ch2o_ndx > 0 ) then
             het_rates(i,k,ch2o_ndx)  = work3(i)
          end if
          if( isopno3_ndx > 0 ) then
             het_rates(i,k,isopno3_ndx) = work3(i)
          end if
          if( xisopno3_ndx > 0 ) then
             het_rates(i,k,xisopno3_ndx) = work3(i)
          end if
          if( hyac_ndx > 0 ) then
             het_rates(i,k,hyac_ndx) = work3(i)
          end if
          if( hydrald_ndx > 0 ) then
             het_rates(i,k,hydrald_ndx) = work3(i)
          end if

          work3(i) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_ch3ooh(i,k)*work2(i)))),0._r8 )
          if( ch3ooh_ndx > 0 ) then
             het_rates(i,k,ch3ooh_ndx)  = work3(i)
          end if
          if( pooh_ndx > 0 ) then
             het_rates(i,k,pooh_ndx)  = work3(i)
          end if
          if( c2h5ooh_ndx > 0 ) then
             het_rates(i,k,c2h5ooh_ndx) = work3(i)
          end if
          if( c3h7ooh_ndx > 0 ) then
             het_rates(i,k,c3h7ooh_ndx) = work3(i)
          end if
          if( rooh_ndx > 0 ) then
             het_rates(i,k,rooh_ndx) = work3(i)
          end if
          if( ch3oh_ndx > 0 ) then
             het_rates(i,k,ch3oh_ndx) = work3(i)
          end if
          if( c2h5oh_ndx > 0 ) then
             het_rates(i,k,c2h5oh_ndx) = work3(i)
          end if
          if( alkooh_ndx  > 0 ) then
             het_rates(i,k,alkooh_ndx) = work3(i)
          end if
          if( mekooh_ndx  > 0 ) then
             het_rates(i,k,mekooh_ndx) = work3(i)
          end if
          if( tolooh_ndx  > 0 ) then
             het_rates(i,k,tolooh_ndx) = work3(i)
          end if
          if( terpooh_ndx > 0 ) then
             het_rates(i,k,terpooh_ndx) = work3(i)
          end if

          if( ch3coooh_ndx > 0 ) then
             het_rates(i,k,ch3coooh_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_ch3co3h(i,k)*work2(i)))),0._r8 )
          end if
          if( ho2no2_ndx > 0 ) then
             het_rates(i,k,ho2no2_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_ho2no2(i,k)*work2(i)))),0._r8 )
          end if
          if( xho2no2_ndx > 0 ) then
             het_rates(i,k,xho2no2_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_ho2no2(i,k)*work2(i)))),0._r8 )
          end if
          if( ch3cocho_ndx > 0 ) then
             het_rates(i,k,ch3cocho_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_ch3cocho(i,k)*work2(i)))),0._r8 )
          end if
          if( xooh_ndx > 0 ) then
             het_rates(i,k,xooh_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_xooh(i,k)*work2(i)))),0._r8 )
          end if
          if( onitr_ndx > 0 ) then
             het_rates(i,k,onitr_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_onitr(i,k)*work2(i)))),0._r8 )
          end if
          if( xonitr_ndx > 0 ) then
             het_rates(i,k,xonitr_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_onitr(i,k)*work2(i)))),0._r8 )
          end if
          if( glyald_ndx > 0 ) then
             het_rates(i,k,glyald_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_glyald(i,k)*work2(i)))),0._r8 )
          end if
          if( ch3cho_ndx > 0 ) then
             het_rates(i,k,ch3cho_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_ch3cho(i,k)*work2(i)))),0._r8 )
          end if
          if( mvk_ndx > 0 ) then
             het_rates(i,k,mvk_ndx)  = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_mvk(i,k)*work2(i)))),0._r8 )
          end if
          if( macr_ndx > 0 ) then
             het_rates(i,k,macr_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_macr(i,k)*work2(i)))),0._r8 )
          end if
          if( h2o2_ndx > 0 ) then
             work3(i) = satf_h2o2 * max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_h2o2(i,k)*work2(i)))),0._r8 )    
             het_rates(i,k,h2o2_ndx) =  work3(i) + tmp_hetrates(i,k,1)
          end if
          if( so2_ndx > 0 ) then
             work3(i) = satf_so2 * max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_so2( i,k)*work2(i)))),0._r8 )    
             het_rates(i,k,so2_ndx ) =  work3(i) + tmp_hetrates(i,k,3)
          end if

          work3(i) = tmp_hetrates(i,k,2) + satf_hno3 * &
               max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_hno3(i,k)*work2(i)))),0._r8 )    
          tmp0_rates(i)   = work3(i)
          tmp1_rates(i)   = .2_r8*work3(i)
          if( hno3_ndx > 0 ) then
             het_rates(i,k,hno3_ndx) = work3(i)
          end if
          if( xhno3_ndx > 0 ) then
             het_rates(i,k,xhno3_ndx) = work3(i)
          end if
          if( onit_ndx > 0 ) then
             het_rates(i,k,onit_ndx) = work3(i)
          end if
          if( xonit_ndx > 0 ) then
             het_rates(i,k,xonit_ndx) = work3(i)
          end if
          if( Pb_ndx > 0 ) then
             het_rates(i,k,Pb_ndx) = work3(i)
          end if
          if( macrooh_ndx > 0 ) then
             het_rates(i,k,macrooh_ndx) = work3(i)
          end if
          if( isopooh_ndx > 0 ) then
             het_rates(i,k,isopooh_ndx) = work3(i)
          end if

          if( clono2_ndx > 0 ) then
             het_rates(i,k, clono2_ndx) = work3(i)
          end if
          if( brono2_ndx > 0 ) then
             het_rates(i,k, brono2_ndx) = work3(i)
          end if
          if( hcl_ndx > 0 ) then
             het_rates(i,k, hcl_ndx) = work3(i)
          end if
          if( n2o5_ndx > 0 ) then
             het_rates(i,k, n2o5_ndx) = work3(i)
          end if
          if( hocl_ndx > 0 ) then
             het_rates(i,k, hocl_ndx) = work3(i)
          end if
          if( hobr_ndx > 0 ) then
             het_rates(i,k, hobr_ndx) = work3(i)
          end if
          if( hbr_ndx > 0 ) then
             het_rates(i,k, hbr_ndx) = work3(i)
          end if

          if( soa_ndx > 0 ) then
             het_rates(i,k,soa_ndx) = tmp1_rates(i)
          end if
          if( oc2_ndx > 0 ) then
             het_rates(i,k,oc2_ndx) = tmp1_rates(i)
          end if
          if( cb2_ndx > 0 ) then
             het_rates(i,k,cb2_ndx) = tmp1_rates(i)
          end if
          if( so4_ndx > 0 ) then
             het_rates(i,k,so4_ndx) = tmp1_rates(i)
          end if
          if( sa1_ndx > 0 ) then
             het_rates(i,k,sa1_ndx) = tmp1_rates(i)
          end if
          if( sa2_ndx > 0 ) then
             het_rates(i,k,sa2_ndx) = tmp1_rates(i)
          end if
          if( sa3_ndx > 0 ) then
             het_rates(i,k,sa3_ndx) = tmp1_rates(i)
          end if
          if( sa4_ndx > 0 ) then
             het_rates(i,k,sa4_ndx) = tmp1_rates(i)
          end if

          if( h2so4_ndx > 0 ) then
             het_rates(i,k,h2so4_ndx) = tmp0_rates(i)
          end if
          if( nh4_ndx > 0 ) then
             het_rates(i,k,nh4_ndx) = tmp0_rates(i)
          end if
          if( nh4no3_ndx > 0 ) then
             het_rates(i,k,nh4no3_ndx ) = tmp0_rates(i)
          end if
          if( nh3_ndx > 0 ) then
             het_rates(i,k,nh3_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1./(xhen_nh3(i,k)*work2(i)))),0._r8 )
          end if

          if( ch3cooh_ndx > 0 ) then
             het_rates(i,k,ch3cooh_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_ch3cooh(i,k)*work2(i)))),0._r8 )
          end if
          if( hcooh_ndx > 0 ) then
             het_rates(i,k,hcooh_ndx) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_ch3cooh(i,k)*work2(i)))),0._r8 )
          endif
          if ( hcn_ndx > 0 ) then
             het_rates(i,k,hcn_ndx     ) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_hcn(i,k)*work2(i)))),0._r8 )
          endif
          if ( ch3cn_ndx > 0 ) then
             het_rates(i,k,ch3cn_ndx   ) = max( rain(i,k) / (h2o_mol*(work1(i) + 1._r8/(xhen_ch3cn(i,k)*work2(i)))),0._r8 )
          endif

       end do Column_loop2
    end do level_loop2

    !-----------------------------------------------------------------
    !	... Set rates above tropopause = 0.
    !-----------------------------------------------------------------
    do mm = 1,gas_wetdep_cnt
       m = wetdep_map(mm)
       do i = 1,ncol
          do k = 1,ktop(i)
             het_rates(i,k,m) = 0._r8
          end do
       end do
       if ( any( het_rates(:ncol,:,m) == MISSING) ) then
          write(hetcntstrg,'(I3)') m
          do i = 1,ncol
             do k = 1,pver
             end do
          end do
          call endrun('sethet: het_rates (wet dep) not set for het reaction number : '//hetcntstrg)
       endif
    end do

  end subroutine sethet

end module mo_sethet
