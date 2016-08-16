#define WRF_PORT
!Future Modifications:

module module_cam_mam_gas_wetdep_driver

  use shr_kind_mod,       only: r8 => shr_kind_r8
  use module_cam_support, only: pcnst =>pcnst_runtime, pcols, pver

  implicit none
  save

  private
  public :: cam_mam_gas_wetdep_driver
  public :: cam_mam_gas_wetdep_inti

contains

  subroutine cam_mam_gas_wetdep_inti()
    use mo_sethet, only : sethet_inti
    implicit none

    !Call initialization for sethet
    call  sethet_inti
    
  end subroutine cam_mam_gas_wetdep_inti
  
  subroutine cam_mam_gas_wetdep_driver(          &
       !Intent in-outs
       chem,                                     &
       !Intent ins
       dtstepc, config_flags, ht, XLAT, nevapr3d,&
       rprdsh3d, rprddp3d, prain3d, z_sea_level, & 
       p_phy, t_phy, alt, moist, scalar,         &
       ids,ide, jds,jde, kds,kde,                &
       ims,ime, jms,jme, kms,kme,                &
       its,ite, jts,jte, kts,kte                 )


    use module_state_description,  only: num_moist, num_chem, p_qv, p_qc,    &
         p_qi, p_qnc, p_qni, param_first_scalar, num_scalar, CAMUWSHCUSCHEME,&
         CAMZMSCHEME

    use module_cam_support,        only: pcnst =>pcnst_runtime, pcols, pver, &
         pcnst_non_chem => pcnst_non_chem_modal_aero, nfs,                   &
         gas_pcnst => gas_pcnst_modal_aero, hetcnt => gas_pcnst_modal_aero !Hetcnt is same as gas_pcnst in CAM5.1

    use module_data_cam_mam_asect, only: lptr_chem_to_q, lptr_chem_to_qqcw,  &
         factconv_chem_to_q, mw_q_array
    use physconst,                 only: mwdry, avogad, gravit, rga

    use mo_sethet,                 only: sethet
    use infnan,                    only: nan
    use module_configure,          only: grid_config_rec_type

    !
    implicit none

    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

    !Scalar Intent-ins
    integer, intent(in) ::          &
         ids,ide, jds,jde, kds,kde, &
         ims,ime, jms,jme, kms,kme, &
         its,ite, jts,jte, kts,kte

    real, intent(in) :: dtstepc       !Chemistry time step in seconds(s)

    !2D Intent-ins
    real, dimension( ims:ime, jms:jme ), intent(in) :: ht     !Terrain height (m)
    real, dimension( ims:ime, jms:jme ), intent(in) :: XLAT   !Latitude

    !3D Intent-ins
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: prain3d     !Rate of conversion of condensate to precipitation (kg/kg/s)
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: p_phy       !Hydrostatic pressure(Pa)
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: t_phy       !Temperature (K)
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: alt         !inverse density(m3/kg)
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: z_sea_level !Height above sea level at mid-level (m) 
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: nevapr3d    !Evaporation rate of rain + snow (kg/kg/s)
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: rprdsh3d    !dq/dt due to deep and shallow convective rainout(kg/kg/s)
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: rprddp3d    !dq/dt due to deep convective rainout (kg/kg/s)

    !4D Intent ins
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme, 1:num_moist )  :: moist  !Mixing ratios (kg/kg for mass species )
    real, intent(in), dimension( ims:ime, kms:kme, jms:jme, 1:num_scalar ) :: scalar !Mixing ratios (#/kg for number species)

    !4D Intent-inouts
    real, intent(inout), dimension( ims:ime, kms:kme, jms:jme, 1:num_chem )   :: chem !Chem array


    !!-----------------------------------------------------------------------
    !!        ... Dummy arguments
    !!-----------------------------------------------------------------------
    !Arguments which were Intent-in in the original CAM's interface
    integer            :: lchnk                         ! chunk index
    integer            :: ncol                          ! number columns in chunk
    real(r8)           :: delt                          ! timestep (s)
    real(r8)           :: tfld(pcols,kte)               ! midpoint temperature (K)
    real(r8)           :: pmid(pcols,kte)               ! midpoint pressures (Pa)

    !!-----------------------------------------------------------------------
    !!          ... Local variables
    !!-----------------------------------------------------------------------
    real(r8), parameter :: m2km  = 1.e-3_r8

    integer  :: i, k, m, n
    real(r8) :: zsurf(pcols)                          ! surface height (m)
    real(r8) :: phis(pcols)    
    real(r8) :: cmfdqr(pcols, kte)
    real(r8) :: nevapr(pcols, kte)
    real(r8) :: rprdsh(pcols, kte)
    real(r8) :: rprddp(pcols, kte)
    real(r8) :: state_zm(pcols,kte)
    real(r8) :: zmid(pcols,kte)
    real(r8) :: invariants(pcols,kte,nfs)
    real(r8) :: vmr(pcols,kte,gas_pcnst)              ! xported species (vmr)
    real(r8) :: het_rates(pcols,kte,hetcnt)           !washout rate (1/s)
    real(r8) :: prain(pcols,kte)

    !Variables needed for porting CAM parameterization in WRF
    integer  ::  icol, itsm1, itile_len
    integer  ::  iw, jw, kw, ktep1, kflip, l, l2, l3
    integer  ::  p1st, ichem
    real(r8) ::  dp, multFrc, fconv
    real(r8) ::  rlat(pcols) 
    real(r8) ::  xhnm(pcols,kte)
    real(r8) ::  state_q(pcols,kte,pcnst)


    !Time step for chemistry (following module_cam_mam_aerchem_driver.F)
    delt = dtstepc ! converts to r8
    pver = kte

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

          phis(1) = ht(iw,jw)  * gravit 

          !Flip vertically quantities computed at the mid points
          do kw  = kts, kte
             kflip             = ktep1 - kw
             pmid(1,kflip)     = p_phy(iw,kw,jw)                   !Pressure at the mid-points (Pa) [state%pmid in CAM]
             tfld(1,kflip)     = t_phy(iw,kw,jw)                   !Temprature at the mid points (K) [state%t in CAM]
             state_zm(1,kflip) = z_sea_level(iw,kw,jw) - ht(iw,jw) !Height above the ground at the midpoints (m) [state%zm in CAM]
             rlat(1)           = xlat(iw,jw)

             !Following three formulas are obtained from ported CAM's ZM cumulus scheme
             !Values of 0 cause a crash in entropy
             multFrc              = 1._r8/(1._r8 + moist(iw,kw,jw,P_QV))
             state_q(1,kflip,1)   = max( moist(iw,kw,jw,P_QV)*multFrc, 1.0e-30_r8 ) !Specific humidity                       [state%q(:,:,1) in CAM]
             state_q(1,kflip,2)   = moist(iw,kw,jw,P_QC)*multFrc                    !Convert to moist mix ratio-cloud liquid [state%q(:,:,2) in CAM]
             state_q(1,kflip,3)   = moist(iw,kw,jw,P_QI)*multFrc                    !cloud ice                               [state%q(:,:,3) in CAM]
             state_q(1,kflip,4)   = scalar(iw,kw,jw,P_QNC)*multFrc                  !Liquid cloud number
             state_q(1,kflip,5)   = scalar(iw,kw,jw,P_QNI)*multFrc                  !Ice cloud number

             !populate state_q 
             !Following Do-Loop is obtained from chem/module_cam_mam_aerchem_driver.F
             do l = p1st, num_chem
                l2 = lptr_chem_to_q(l)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   state_q(1,kflip,l2) = chem(iw,kw,jw,l)*factconv_chem_to_q(l)
                end if
             end do ! l
             prain(1,kflip)        = prain3d(iw,kw,jw) 
             nevapr(1,kflip)       = nevapr3d(iw,kw,jw)                           !Evaporation rate of rain + snow (kg/kg/s)

             rprdsh(1,kflip)            = 0.0_r8
             
             if(config_flags%shcu_physics==CAMUWSHCUSCHEME) then
                !inputs from shallow convection
                rprdsh(1,kflip)         = rprdsh3d(iw,kw,jw)                     !dq/dt due to deep and shallow convective rainout(kg/kg/s)
             endif
             
             rprddp(1,kflip)            = 0.0_r8
             if(config_flags%cu_physics==CAMZMSCHEME)then
                !inputs from deep convection
                rprddp(1,kflip)         = rprddp3d(iw,kw,jw)                     !dq/dt due to deep convective rainout (kg/kg/s)
             endif

             !xhnm is air density in molecules/cm3 (Formulated by RCE, codded by balwinder.singh@pnnl.gov)
             xhnm(1,kflip) = (avogad*1.0e-6_r8)/(mwdry*alt(iw,kw,jw)) !Note that avogad is in kmoles/moles

             !invariants array is NEVER used in the computations when state_q is defined. Therefore it is set to nan
             invariants(1,kflip,:) = nan

             do ichem = p1st , num_chem
                l2 = lptr_chem_to_q(ichem)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   l3                   = l2 - pcnst_non_chem
                   fconv                = mwdry/mw_q_array(l2)
                   vmr(icol,kflip,l3)   = state_q(icol,kflip,l2)*fconv
                endif
             end do

          enddo !enddo for kw=kts, kte loop

          zsurf(:ncol) = rga * phis(:ncol)
          do k = 1,pver
             zmid(:ncol,k) = m2km * (state_zm(:ncol,k) + zsurf(:ncol))
          end do


          cmfdqr(:ncol,:) = rprddp(:ncol,:)  + rprdsh(:ncol,:)
          !Note: Only het_rates is the output from the sethet call
          call sethet( het_rates, pmid, zmid, phis, tfld, &
                   cmfdqr, prain, nevapr, delt, xhnm, & !In original call, invariants(1,1,indexm), is being passed but it is replaced here with xhnm
                   vmr, ncol, lchnk,rlat)
          
          do l2 = pcnst_non_chem+1, pcnst
             l3                 = l2 - pcnst_non_chem    
             het_rates(icol,:,l3) = min(max(0._r8, het_rates(icol,:,l3)),(1._r8-1.e-5_r8)/delt) !BSINGH - PMA advised to introduce limit on het_rates to keep state_q positive
             state_q(icol,:,l2) = state_q(icol,:,l2) - het_rates(icol,:,l3)*state_q(icol,:,l2)*delt
          end do

          !Post processing of the output from CAM's parameterization
          do kw = kts , kte
             kflip = kte-kw+1

             do l = p1st, num_chem
                l2 = lptr_chem_to_q(l)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   chem(iw,kw,jw,l) = state_q(1,kflip,l2)/factconv_chem_to_q(l)             
                end if
             end do ! l
          end do !kw post processing do -loop


       enddo !iw do-loop
    enddo    !jw do-loop



  end subroutine cam_mam_gas_wetdep_driver

end module module_cam_mam_gas_wetdep_driver
