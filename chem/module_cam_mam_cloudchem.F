#define WRF_PORT
!Future Modifications:
!1. do_cam_sulfchem is hardwired in the current code. It should be obtained from 
!   mz_aerosol_intr subroutine
!2. Cloud fraction compuations yeild only 0 or 1 currently, no fractional value for
!   cloud fraction 

module module_cam_mam_cloudchem

  use shr_kind_mod,       only: r8 => shr_kind_r8
  use module_cam_support, only: pcnst =>pcnst_runtime, pcols, pver, fieldname_len, &
       gas_pcnst => gas_pcnst_modal_aero,iam

  implicit none
  save

  private
  public :: cam_mam_cloudchem_driver
  public :: cam_mam_cloudchem_inti

  integer :: synoz_ndx, so4_ndx, h2o_ndx, o2_ndx, o_ndx, hno3_ndx, dst_ndx, cldice_ndx
  integer :: o3_ndx
  integer :: het1_ndx
  logical :: inv_o3, inv_oh, inv_no3, inv_ho2
  integer :: id_o3, id_oh, id_no3, id_ho2
  integer :: dgnum_idx       = 0
  integer :: dgnumwet_idx    = 0
  integer :: wetdens_ap_idx  = 0

contains

  subroutine cam_mam_cloudchem_inti()
    use mo_setsox, only : sox_inti
    implicit none

    !Call initialization for setsox
    call  sox_inti
    
  end subroutine cam_mam_cloudchem_inti
  
  subroutine cam_mam_cloudchem_driver(           &
       !Intent Outs
       dvmrdt_sv13d,dvmrcwdt_sv13d,              &
       !Intent in-outs
       chem,                                     &
       !Intent ins
       moist, scalar, p8w, prain3d, p_phy,       &
       t_phy, dtstepc, ktau,alt, f_ice_phy,      &
       f_rain_phy,cldfra, cldfra_mp_all,         &
       cldfrai, cldfral, is_CAMMGMP_used,        & 
       ids,ide, jds,jde, kds,kde,                &
       ims,ime, jms,jme, kms,kme,                &
       its,ite, jts,jte, kts,kte                 )
    !!-----------------------------------------------------------------------
    !!     ... Chem_solver advances the volumetric mixing ratio
    !!         forward one time step via a combination of explicit,
    !!         ebi, hov, fully implicit, and/or rodas algorithms.
    !!-----------------------------------------------------------------------
    use module_configure,          only: grid_config_rec_type
    use module_state_description,  only: num_moist, num_chem, p_qv, p_qc,    &
         p_qi, p_qs, p_qnc, p_qni, param_first_scalar, num_scalar, f_qc,     &
         f_qi,  f_qv, f_qs 
    use module_cam_support,        only: pcnst =>pcnst_runtime, pcols, pver, &
         pcnst_non_chem => pcnst_non_chem_modal_aero, nfs,                   &
         gas_pcnst => gas_pcnst_modal_aero,                                  &
         gas_pcnst_pos => gas_pcnst_modal_aero_pos
    use constituents,              only: cnst_get_ind
    use module_data_cam_mam_asect, only: lptr_chem_to_q, lptr_chem_to_qqcw,  &
         factconv_chem_to_q, mw_q_array
    use physconst,                 only: mwdry, avogad

    use mo_setsox,                 only: setsox, has_sox
    use modal_aero_data,           only: ntot_amode
    use infnan,                    only: nan

    !
    implicit none

    logical, intent(in) :: is_CAMMGMP_used
    !Scalar Intent-ins
    integer, intent(in) :: ktau       !Time step number
    integer, intent(in) ::          &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte

    real, intent(in) :: dtstepc       !Chemistry time step in seconds(s)

    !3D Intent-ins
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: p8w     !Hydrostatic Pressure at level interface (Pa)
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: prain3d !Rate of conversion of condensate to precipitation (kg/kg/s)
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: p_phy   !Hydrostatic pressure(Pa)
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: t_phy   !Temperature (K)
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: alt
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: F_ICE_PHY   !Fraction of ice
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: F_RAIN_PHY  !Fraction of rain
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: cldfra   !cloud fraction
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: cldfra_mp_all   !cloud fraction from MGMP micrpphysics
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: cldfrai
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: cldfral
    !4D Intent ins
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme, 1:num_moist )  :: moist  !Mixing ratios (kg/kg for mass species )
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme, 1:num_scalar ) :: scalar !Mixing ratios (#/kg for number species)
    !4D Intent-inouts
    real, intent(inout), dimension( ims:ime, kms:kme, jms:jme, 1:num_chem )   :: chem !Chem array
    !4D Intent-outs
    real, intent(out), dimension( ims:ime, kms:kme, jms:jme, 1:gas_pcnst_pos )   :: dvmrdt_sv13d,dvmrcwdt_sv13d 


    !!-----------------------------------------------------------------------
    !!        ... Dummy arguments
    !!-----------------------------------------------------------------------
    !Arguments which were Intent-in in the original CAM's interface
    integer            :: lchnk                         ! chunk index
    integer            :: ncol                          ! number columns in chunk
    integer            :: imozart                       ! gas phase start index in q
    real(r8)           :: delt                          ! timestep (s)
    real(r8)           :: tfld(pcols,kte)               ! midpoint temperature (K)
    real(r8)           :: pmid(pcols,kte)               ! midpoint pressures (Pa)
    real(r8)           :: pdel(pcols,kte)               ! pressure delta about midpoints (Pa)
    real(r8)           :: cldw(pcols,kte)               ! cloud water (kg/kg)
    real(r8)           :: ncldwtr(pcols,kte)            ! droplet number concentration (#/kg)

    !!-----------------------------------------------------------------------
    !!          ... Local variables
    !!-----------------------------------------------------------------------
    integer      ::  i, k, m, n
    real(r8)     ::  invariants(pcols,kte,nfs)
    real(r8)     ::  vmr(pcols,kte,gas_pcnst)              ! xported species (vmr)
    real(r8)     ::  vmrcw(pcols,kte,gas_pcnst)            ! cloud-borne aerosol (vmr)
    real(r8), dimension(pcols,kte) :: &
         mbar                                           ! mean wet atmospheric mass ( amu )
    real(r8), dimension(pcols,kte) :: &
         cwat, &                                           ! cloud water mass mixing ratio (kg/kg)
         cldnum, &                                         ! droplet number concentration (#/kg)
         cldfr, &                                          ! cloud fraction
         prain

    real(r8) :: qh2o(pcols,kte)               ! specific humidity (kg/kg)
    real(r8) :: dvmrdt_sv1(pcols,pver,gas_pcnst_pos)
    real(r8) :: dvmrcwdt_sv1(pcols,pver,gas_pcnst_pos)

    !Variables needed for porting CAM parameterization

    logical, parameter :: do_cam_sulfchem = .FALSE. !Forced it to FALSE so that setsox can execute,in CAM it is obtained from mz_aerosols_intr.F90 

    integer      ::  imozart_m1, icol, itsm1, itile_len
    integer      ::  iw, jw, kw, ktep1, kflip, l, l2, l3
    integer      ::  l_mo_h2so4, l_mo_soag, p1st, ichem
    real(r8)     ::  dp, multFrc, fconv
    real(r8)     ::  xhnm(pcols,kte)
    real(r8)     ::  state_q(pcols,kte,pcnst)
    real(r8)     ::  qqcw(pcols,kte,pcnst)      !cloud-borne aerosol


    !Time step for chemistry (following module_cam_mam_aerchem_driver.F)
    delt = dtstepc
    pver = kte


    !Following imozart computation was directly taken from module_cam_mam_aerchem_driver.F
    imozart_m1 = pcnst_non_chem
    imozart = imozart_m1 + 1

    !Following h2so4 and soag computations were directly taken from module_cam_mam_aerchem_driver.F
    call cnst_get_ind( 'h2so4', l_mo_h2so4, .false. )
    l_mo_h2so4 = l_mo_h2so4 - imozart_m1
    if ((l_mo_h2so4 < 1) .or. (l_mo_h2so4 > gas_pcnst)) &
         call wrf_error_fatal('cam_mam_cloudchem error -- no h2so4 species' )
    write(*,*) 'l_mo_h2so4 = ', l_mo_h2so4

    call cnst_get_ind( 'soag', l_mo_soag, .false. )
    l_mo_soag = l_mo_soag - imozart_m1
    if ((l_mo_soag < 1) .or. (l_mo_soag > gas_pcnst)) &
         call  wrf_error_fatal( 'cam_mam_cloudchem error -- no soag species' )
    write(*,*) 'l_mo_soag = ', l_mo_soag


    !Required assignments
    p1st = param_first_scalar ! Obtain CHEM array's first element's index


    ncol = pcols
    icol = ncol !Used in some CAM variables

    !This subroutine requires that ncol == 1
    if(ncol .NE. 1) then
       call wrf_error_fatal('Number of CAM Columns (NCOL) in CAM_MAM_CLOUDCHEM scheme must be 1')
    endif

    !Divide domain in chuncks and map WRF variables into CAM
    !Loop counters are named iw,jw,kw to represent that they index WRF sided variables
    
    itsm1     = its - 1
    itile_len = ite - itsm1
    do jw     = jts , jte
       do iw  = its , ite

          lchnk   = (jw - jts) * itile_len + (iw - itsm1)             !1-D index location from a 2-D tile
          ktep1   = kte + 1

          !Flip vertically quantities computed at the mid points
          do kw  = kts, kte
             kflip          = ktep1 - kw
             pmid(1,kflip)  = p_phy(iw,kw,jw)                   !Pressure at the mid-points (Pa) [state%pmid in CAM]
             dp             = p8w(iw,kw,jw) - p8w(iw,kw+1,jw)   !Change in pressure (Pa)
             pdel(1,kflip)  = dp
             tfld(1,kflip)  = t_phy(iw,kw,jw)                   !Temprature at the mid points (K) [state%t in CAM]

             !Following three formulas are obtained from ported CAM's ZM cumulus scheme
             !Values of 0 cause a crash in entropy
             multFrc              = 1._r8/(1._r8 + moist(iw,kw,jw,P_QV))
             state_q(1,kflip,1)   = max( moist(iw,kw,jw,P_QV)*multFrc, 1.0e-30_r8 ) !Specific humidity                       [state%q(:,:,1) in CAM]
             state_q(1,kflip,2)   = moist(iw,kw,jw,P_QC)*multFrc                    !Convert to moist mix ratio-cloud liquid [state%q(:,:,2) in CAM]
             state_q(1,kflip,3)   = moist(iw,kw,jw,P_QI)*multFrc                    !cloud ice                               [state%q(:,:,3) in CAM]
             state_q(1,kflip,4)   = scalar(iw,kw,jw,P_QNC)*multFrc                  !Liquid cloud number
             state_q(1,kflip,5)   = scalar(iw,kw,jw,P_QNI)*multFrc                  !Ice cloud number

             !Followiing formulas are obtained from Chemistry.F90 of CAM
             qh2o(1,kflip)        = state_q(1,kflip,1)
             cwat(1,kflip)        = state_q(1,kflip,2) + state_q(1,kflip,3)
             cldnum(1,kflip)      = state_q(1,kflip,4)

             !populate state_q and qqcw arrays
             !Following Do-Loop is obtained from chem/module_cam_mam_aerchem_driver.F
             do l = p1st, num_chem
                l2 = lptr_chem_to_q(l)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   state_q(1,kflip,l2) = chem(iw,kw,jw,l)*factconv_chem_to_q(l)
                end if
                l2 = lptr_chem_to_qqcw(l)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   qqcw(1,kflip,l2) = chem(iw,kw,jw,l)*factconv_chem_to_q(l)     !Cloud borne aerosols
                end if
             end do ! l

             prain(1,kflip)        = prain3d(iw,kw,jw)                      !Rate of conversion of condensate to precipitation (kg/kg/s)
             if(is_CAMMGMP_used) then
                cldfr(1,kflip)        = cldfral(iw,kw,jw)                       !Cloud fraction from CAMMGMP
             else
                cldfr(1,kflip)        = cldfra(iw,kw,jw)                        !Cloud fraction
             endif
             cldfr(1,kflip)        = min(max(cldfr(1,kflip),0._r8),1._r8)
             !invariants array is NEVER used in the computations when state_q is defined. Therefore it is set to nan
             invariants(1,kflip,:) = nan
             mbar(1,kflip)         = mwdry

             !xhnm is air density in molecules/cm3 (Formulated by RCE, codded by balwinder.singh@pnnl.gov)
             xhnm(1,kflip) = (avogad*1.0e-6_r8)/(mwdry*alt(iw,kw,jw)) !Note that avogad is in kmoles/moles
             !initialize dvmrdt_sv13d and dvmrcwdt_sv13d to zero
             dvmrdt_sv13d(iw,kw,jw,:)   = 0.0
             dvmrcwdt_sv13d(iw,kw,jw,:) = 0.0

             !initialize vmr and vmrc 
             vmr(icol,kflip,:)          = 0.0_r8
             vmrcw(icol,kflip,:)        = 0.0_r8

             !Following vmr and vmrcw computation was directly taken from module_cam_mam_aerchem_driver.F
             !BSINGH - I have changed the loop counters and structure to avoid accessing qqcw array for 
             !indices where qqcw is NOT defined (qqcw is defined only for _c1, _c2 and _c3 aerosols, rest 
             !of the array has junk values)

             !do l2 = pcnst_non_chem + 1, pcnst !Original loop counters
             do ichem = p1st , num_chem
                l2 = lptr_chem_to_q(ichem)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   l3                   = l2 - pcnst_non_chem
                   fconv                = mwdry/mw_q_array(l2)
                   vmr(icol,kflip,l3)   = state_q(icol,kflip,l2)*fconv
                endif

                if (iw*jw == 1 .and. kw == kts .and. l3 == l_mo_h2so4) then
                   write(*,'(a,1p,2e11.3)') '1,1,1 h2so4 q8 & vmr8', state_q(icol,pver,l2), vmr(icol,pver,l3)
                endif
                if (iw*jw == 1 .and. kw == kts .and. l3 == l_mo_soag) then
                   write(*,'(a,1p,2e11.3)') '1,1,1 soag  q8 & vmr8', state_q(icol,pver,l2), vmr(icol,pver,l3)
                endif                

                l2 = lptr_chem_to_qqcw(ichem)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   l3                   = l2 - pcnst_non_chem
                   fconv                = mwdry/mw_q_array(l2)
                   vmrcw(icol,kflip,l3) = qqcw(icol,kflip,l2)*fconv
                endif
             end do

          enddo !enddo for kw=kts,kte loop

          dvmrdt_sv1 = vmr
          dvmrcwdt_sv1 = vmrcw
          
          !Note: Only vmrcw and vm are the outputs from the setsox call
          if( has_sox .and. (.not. do_cam_sulfchem) ) then
             call setsox( ncol,   &
                  pmid,   &
                  delt,   &
                  tfld,   &
                  qh2o,   &
                  cwat,   &
                  lchnk,  &
                  pdel,   &
                  mbar,   &
                  prain,  &
                  cldfr,  &
                  cldnum, &
                  vmrcw,    &
                  imozart-1,&
                  xhnm, & !In original call, invariants(1,1,indexm), is being passed but it is replaced here with xhnm
                  vmr, &
                  invariants )

          endif
          dvmrdt_sv1 = (vmr - dvmrdt_sv1)/delt
          dvmrcwdt_sv1 = (vmrcw - dvmrcwdt_sv1)/delt

          !Post processing of the output from CAM's parameterization
          do l2 = pcnst_non_chem+1, pcnst
             l3                 = l2 - pcnst_non_chem
             fconv              = mw_q_array(l2)/mwdry
             state_q(icol,:,l2) = vmr(icol,:,l3)*fconv
             qqcw(icol,:,l2)    = vmrcw(icol,:,l3)*fconv
             if (iw*jw == 1 .and. kw == kts .and. l3 == l_mo_h2so4) &
                  write(*,'(a,1p,2e11.3)') '1,1,1 h2so4 q8 & vmr8', state_q(icol,pver,l2), vmr(icol,pver,l3)
             if (iw*jw == 1 .and. kw == kts .and. l3 == l_mo_soag) &
                  write(*,'(a,1p,2e11.3)') '1,1,1 soag  q8 & vmr8', state_q(icol,pver,l2), vmr(icol,pver,l3)
          end do


          do kw = kts , kte
             kflip = kte-kw+1

             do l = p1st, num_chem
                l2 = lptr_chem_to_q(l)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   chem(iw,kw,jw,l)         = state_q(1,kflip,l2)/factconv_chem_to_q(l)             
                end if
                l2 = lptr_chem_to_qqcw(l)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   chem(iw,kw,jw,l) = qqcw(1,kflip,l2)/factconv_chem_to_q(l)
                end if
             end do ! l
             do l = 1 , gas_pcnst
                dvmrdt_sv13d(iw,kw,jw,l)   = dvmrdt_sv1(1,kflip,l) 
                dvmrcwdt_sv13d(iw,kw,jw,l) = dvmrcwdt_sv1(1,kflip,l) 
             enddo

          end do !kw post processing do -loop


       enddo !iw do-loop
    enddo    !jw do-loop



  end subroutine cam_mam_cloudchem_driver

end module module_cam_mam_cloudchem
