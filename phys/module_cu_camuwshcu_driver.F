MODULE module_cu_camuwshcu_driver
  USE shr_kind_mod,    only: r8 => shr_kind_r8

! Roughly based on convect_shallow_tend in convect_shallow.F90 from CAM
! but tailored for the UW shallow cumulus scheme.

  IMPLICIT NONE

  PRIVATE                  !Default to private
  PUBLIC :: &              !Public entities
       camuwshcu_driver

CONTAINS

!------------------------------------------------------------------------
SUBROUTINE camuwshcu_driver(                                  &
              ids,ide, jds,jde, kds,kde                       &
             ,ims,ime, jms,jme, kms,kme                       &
             ,its,ite, jts,jte, kts,kte                       &
             ,num_moist, dt                                   &
             ,p, p8w, pi_phy, z, z_at_w, dz8w                 &
             ,t_phy, u_phy, v_phy                             &
             ,moist, qv, qc, qi                               &
             ,pblh_in, tke_pbl, cldfra, cldfra_old, cldfrash  &
             ,cush_inout, rainsh, pratesh, snowsh, icwmrsh    &
             ,cmfmc, cmfmc2_inout, rprdsh_inout, cbmf_inout   &
             ,cmfsl, cmflq, dlf, evapcsh_inout                &
             ,rliq, rliq2_inout, cubot, cutop                 &
             ,rushten, rvshten, rthshten                      &
             ,rqvshten, rqcshten, rqrshten                    &
             ,rqishten, rqsshten, rqgshten                    &
             ,ht                                              &
                                                              )
! This routine is based on convect_shallow_tend in CAM. It handles the
! mapping of variables from the WRF to the CAM framework for the UW
! shallow convective parameterization.
!
! Author: William.Gustafson@pnl.gov, Jan. 2010
!------------------------------------------------------------------------
  USE module_state_description, only: param_first_scalar, &
                                      p_qc, p_qr, p_qi, p_qs, p_qg
  USE module_cam_support,       only: pcols, pver
  USE constituents,             only: cnst_get_ind
  USE physconst,                only: cpair, gravit, latvap
  USE uwshcu,                   only: compute_uwshcu_inv
  USE wv_saturation,            only: fqsatd

! Subroutine arguments...
  INTEGER, INTENT(IN   ) ::    ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte,  &
                               num_moist

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ), INTENT(IN) :: &
                              moist    !moist tracer array

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: &
                             cldfra, & !cloud fraction
                         cldfra_old, & !previous time step cloud fraction
                               dz8w, & !height between layer interface (m)
                                  p, & !pressure at mid-level (Pa)
                                p8w, & !pressure at level interface (Pa)
                             pi_phy, & !exner function, (p0/p)^(R/cpair) (none)
                                 qv, & !water vapor mixing ratio (kg/kg-dry air)
                              t_phy, & !temperature (K)
                            tke_pbl, & !turbulent kinetic energy from PBL (m2/s2)
                              u_phy, & !zonal wind component on T points (m/s)
                              v_phy, & !meridional wind component on T points (m/s)
                                  z, & !height above sea level at mid-level (m)
                             z_at_w    !height above sea level at interface (m)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN), OPTIONAL :: &
                                 qc, & !cloud droplet mixing ratio (kg/kg-dry air)
                                 qi    !cloud ice crystal mixing ratio (kg/kg-dry air)

  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: &
                            pblh_in, & !height of PBL (m)
                            ht         !Terrain height (m)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
                           cldfrash, & !shallow convective cloud fraction
                              cmfmc, & !deep+shalow cloud fraction (already contains deep part from ZM)
                       cmfmc2_inout, & !shallow cloud fraction
                              cmflq, & !convective flux of total water in energy unit (~units)
                              cmfsl, & !convective flux of liquid water static energy (~units)
                                dlf, & !dq/dt due to export of cloud water (input=deep from ZM, output=deep+shallow)
                      evapcsh_inout, & !output array for evaporation of shallow convection precipitation (kg/kg/s)
                            icwmrsh, & !shallow cumulus in-cloud water mixing ratio (kg/m2)
                       rprdsh_inout, & !dq/dt due to deep(~?) & shallow convective rainout (~units?)
                            rushten, & !UNcoupled zonal wind tend from shallow Cu scheme (m/s2)
                            rvshten, & !UNcoupled meridional wind tend from shallow Cu scheme (m/s2)
                           rthshten, & !UNcoupled potential temperature tendendcy from shallow cu scheme (K/s)
                           rqvshten, & !UNcoupled water vapor mixing ratio tend from shallow Cu scheme (kg/kg/s)
                           rqcshten, & !UNcoupled clod droplet mixing ratio tend from shallow Cu scheme (kg/kg/s)
                           rqrshten, & !UNcoupled raindrop mixing ratio tend from shallow Cu scheme (kg/kg/s)
                           rqishten, & !UNcoupled ice crystal mixing ratio tend from shallow Cu scheme (kg/kg/s)
                           rqsshten, & !UNcoupled snow mixing ratio tend from shallow Cu scheme (kg/kg/s)
                           rqgshten    !UNcoupled graupel mixing ratio tend from shallow Cu scheme (kg/kg/s)

  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: &
                         cbmf_inout, & !cloud base mass flux (kg/m2/s)
                              cubot, & !level number of base of convection
                              cutop, & !level number of top of convection
                         cush_inout, & !convective scale height (~units?)
                            pratesh, & !time-step shallow cumulus precip rate at surface (mm/s)
                             rainsh, & !time-step shallow cumulus precip (rain+snow) at surface (mm)
                               rliq, & !vertically-integrated reserved cloud condensate (m/s)
                        rliq2_inout, & !vertically-integrated reserved cloud condensate for shallow (m/s)
                             snowsh    !accumulated convective snow rate at surface for shallow Cu (m/s) ~are these the units we should use for WRF?

  REAL, INTENT(IN) :: &
                                 dt    !time step (s)

! Local variables...
  !Variables dimensioned for input to CAM routines
  REAL(r8), DIMENSION(pcols, kte, 5) ::  &   ! dimensioned by 5(=ncnst) associated with CAM constituents (cnst_name size)
                             moist8, & !tracer array for CAM routines
                         tnd_tracer    !tracer tendency

  REAL(r8), DIMENSION(pcols, kte+1) ::  &
                              pint8, & !pressure at layer interface (Pa)
                                zi8, & !height above the ground at interfaces (m)
                               tke8, & !turbulent kinetic energy at level interfaces (m2/s2)
                              slflx, & !convective liquid water static energy flux (~units?)
                              qtflx    !convective total water flux (~units?)
                                                            


  REAL(r8), DIMENSION(pcols, kte) ::  &
                               cld8, & !cloud fraction
                            cldold8, & !previous time step cloud fraction ~should this be just the convective part?
                             cmfdqs, & !convective snow production (~units?)
                             cmfmc2, & !cloud fraction
                            evapcsh, & !evaporation of shallow convection precipitation >= 0. (kg/kg/s)
                           iccmr_uw, & !in-cloud cumulus LWC+IWC (kg/m2)
                           icwmr_uw, & !in-cloud cumulus LWC (kg/m2)
                           icimr_uw, & !in-cloud cumulus IWC (kg/m2)
                              pdel8, & !pressure difference between layer interfaces (Pa)
                           pdeldry8, & !pressure difference between layer interfaces for dry atm (Pa)
                              pmid8, & !pressure at layer middle (Pa)
                                qc2, & !dq/dt due to export of cloud water
                                qh8, & !specific humidity (kg/kg-moist air)
                                qc8, & !cloud liquid water (~units?)
                                qi8, & !cloud ice (~units?)
                              qhtnd, & !specific humidity tendency (kg/kg/s)
                              qctnd, & !cloud water mixing ratio tendency
                              qitnd, & !cloud ice mixing ratio tendency
                             rprdsh, & !dq/dt due to deep(~?) & shallow convective rainout (~units?)
                                 s8, & !dry static energy (J/kg)
                              shfrc, & !shallow cloud fraction
                               stnd, & !heating rate (dry static energy tendency, W/kg)
                                 t8, & !temperature (K)
                                 u8, & !environment zonal wind (m/s)
                               utnd, & !zonal wind tendency (m/s2)
                                 v8, & !environment meridional wind (m/s)
                               vtnd, & !meridional wind tendency (m/s2)
                                zm8    !height between interfaces (m)

  REAL(r8), DIMENSION(pcols) ::  &
                               cbmf, & !cloud base mass flux (kg/m2/s)
                               cnb2, & !bottom level of convective activity
                               cnt2, & !top level of convective activity
                               cush, & !convective scale height (~units?)
                               pblh, & !pblh height (m)
                              precc, & !convective precip (rain+snow) at surface for shallow Cu (m/s)
                              rliq2, & !vertically-integrated reserved cloud condensate for shallow (m/s)
                               snow    !convective snow rate at surface (m/s)

  !Other local vars
  REAL(r8) :: ztodt        !2 delta-t (s)
  INTEGER :: i, j, k, kflip, m, mp1
  INTEGER :: cnb, cnt      !index of cloud base and top in CAM world (indices decrease with height)
  INTEGER :: lchnk         !chunk identifier, used to map 2-D to 1-D arrays in WRF
  INTEGER :: ncnst         !number of tracers
  INTEGER :: ncol          !number of atmospheric columns in chunk
  CHARACTER(LEN=250) :: msg
!
! Initialize...
!
  ncol  = 1           !chunk size in WRF is 1 since we loop over all columns in a tile
! ncnst = num_moist-1 !currently we only handle the moist array for tracers
  ncnst = 5 ! this is associated with moist8 array
  ztodt = 2.*dt
!
! Map variables to inputs for zm_convr and call it...
! Loop over the points in the tile and treat them each as a CAM chunk.
!
  ij_loops : do j = jts,jte
     do i = its,ite
        lchnk = (j-jts)*(ite-its+1) + (i-its+1) !1-D index location from the 2-D tile

        !Flip variables on the layer interfaces
        do k = kts,kte+1
           kflip = kte-k+2

           pint8(1,kflip) = p8w(i,k,j)
           zi8(1,kflip)   = z_at_w(i,k,j) - ht(i,j) ! height above the ground at interfaces
        end do

        !Flip variables on the layer middles
        do k = kts,kte
           kflip = kte-k+1

           cld8(1,kflip)    = cldfra(i,k,j)
           cldold8(1,kflip) = cldfra_old(i,k,j)
           pdel8(1,kflip)   = p8w(i,k,j) - p8w(i,k+1,j)
           pmid8(1,kflip)   = p(i,k,j)
           qh8(1,kflip)     = max( qv(i,k,j)/(1. + qv(i,k,j)), 1e-30 ) !values of 0 cause a crash in entropy
           if( present(qc) ) then
              qc8(1,kflip)  = qc(i,k,j)/(1. + qv(i,k,j)) !Convert to moist mix ratio
           else
              qc8(1,kflip)  = 0.
           end if
           if( present(qi) ) then
              qi8(1,kflip)  = qi(i,k,j)/(1. + qv(i,k,j)) !Used in convtran, ditto for conversion
           else
              qi8(1,kflip)  = 0.
           end if
           pdeldry8(1,kflip)= pdel8(1,kflip)*(1._r8 - qh8(1,kflip))
           t8(1,kflip)      = t_phy(i,k,j)
           s8(1,kflip)      = cpair*t8(1,kflip) + gravit*(z(i,k,j)-ht(i,j))
           u8(1,kflip)      = u_phy(i,k,j)
           v8(1,kflip)      = v_phy(i,k,j)
           zm8(1,kflip)     = dz8w(i,k,j)
        end do

        !TKE in CAM is on the interfaces but in WRF it is on the layer
        !middle. We will interpolate the TKE from the to the interfaces
        !and then just use the lowest TKE for the surface and the highest
        !TKE at the top.
        tke8(1,kte+1) = tke_pbl(i,1,j)    !surface
        tke8(1,1)     = tke_pbl(i,kte,j)  !model top interface
        do k = kts,kte-1
           kflip = kte-k+1
           tke8(1,kflip) = 0.5*(tke_pbl(i,k,j) + tke_pbl(i,k+1,j))
        end do

        !Flip the tracer array -
        !shift tracer dimension down one to remove "blank" index and
        !convert to wet instead of dry mixing ratios.
        do k = kts,kte
           kflip = kte-k+1
!!$           do m = 1,ncnst
!!$              moist8(1,kflip,m) = moist(i,k,j,m+1)/(1. + qv(i,k,j))
!!$           end do

!~For now, send replicate part of the tracer array and send zeros for the
! rest since CAM treats condensate diagnostically compared to WRF that is
! prognostic. This avoids issues with hard-wired assumptions in the UW
! ShCu scheme. This should be looked at again when more time is available.
! Set to zero for most then overwrite as needed...
           moist8(1,kflip,1:ncnst) = 0.

           moist8(1,kflip,1) = qv(i,k,j)/(1. + qv(i,k,j))

           call cnst_get_ind( 'CLDLIQ', m )
           moist8(1,kflip,m) = qc(i,k,j)/(1. + qv(i,k,j))

           call cnst_get_ind( 'CLDICE', m )
           moist8(1,kflip,m) = qi(i,k,j)/(1. + qv(i,k,j))

           call cnst_get_ind( 'NUMLIQ', m )
           moist8(1,kflip,m) = 0.

           call cnst_get_ind( 'NUMICE', m )
           moist8(1,kflip,m) = 0.
        end do

        !Some remapping to get arrays to pass into the routine
        pblh(1) = pblh_in(i,j)
        cush(1) = cush_inout(i,j)
!
! Main guts of the routine...
! This is a bit inefficient because we are flippling the arrays and they
! will then get flipped back again by compute_uwshcu_inv. We are doing
! this to preserve the CAM code as much as possible for maintenance.
!
        call compute_uwshcu_inv(                        &
             pcols, pver, ncol, ncnst, ztodt,           &
             pint8, zi8, pmid8, zm8, pdel8,             &
             u8, v8, qh8, qc8, qi8,                     &
             t8, s8, moist8,                            &
             tke8, cld8, cldold8, pblh, cush,           &
             cmfmc2, slflx, qtflx,                      &
             qhtnd, qctnd, qitnd,                       &
             stnd, utnd, vtnd, tnd_tracer,              &
             rprdsh, cmfdqs, precc, snow,               &
             evapcsh, shfrc, iccmr_UW, icwmr_UW,        &
             icimr_UW, cbmf, qc2, rliq2,                &
             cnt2, cnb2, fqsatd, lchnk, pdeldry8        )
!
! Map output into WRF-dimensioned arrays...
!
        cush_inout(i,j) = cush(1)

        do k = kts,kte
           kflip = kte-k+1

           !Add shallow reserved cloud condensate to deep reserved cloud condensate
           ! dlf (kg/kg/s, qc in CAM),  rliq done below
           dlf(i,k,j)          = dlf(i,k,j) + qc2(1,kflip)

           evapcsh_inout(i,k,j)= evapcsh(1,kflip)
           icwmrsh(i,k,j)      = icwmr_uw(1,kflip)

           rprdsh(1,kflip)     = rprdsh(1,kflip) + cmfdqs(1,kflip)
           rprdsh_inout(i,k,j) = rprdsh(1,kflip)
           !Not doing rprdtot for now since not yet used by other CAM routines in WRF

           !Tendencies of winds, potential temperature, and moisture
           !fields treated specifically by UW scheme
           rushten(i,k,j)  = utnd(1,kflip)
           rvshten(i,k,j)  = vtnd(1,kflip)
           rthshten(i,k,j) = stnd(1,kflip)/cpair/pi_phy(i,k,j)
           rqvshten(i,k,j) = qhtnd(1,kflip)/(1. - qv(i,k,j))
           if( p_qc >= param_first_scalar ) &
                rqcshten(i,k,j) = qctnd(1,kflip)/(1. - qv(i,k,j))
           if( p_qi >= param_first_scalar ) &
                rqishten(i,k,j) = qitnd(1,kflip)/(1. - qv(i,k,j))

!~Turn off tendencies for most condensates since CAM treats them diagnostically.
!           !Tendencies of tracers except qv,qc,qi
!!~need to make sure qg tendency is propagated through to application
!           do m = 4,ncnst
!              mp1 = m+1 !shift to p_ value for the tracer
!              if( mp1==p_qr ) then
!                 rqrshten(i,k,j) = tnd_tracer(1,kflip,m)/(1. - qv(i,k,j))
!              else if( mp1==p_qs ) then
!                 rqsshten(i,k,j) = tnd_tracer(1,kflip,m)/(1. - qv(i,k,j))
!              else if( mp1==p_qg ) then
!                 rqgshten(i,k,j) = tnd_tracer(1,kflip,m)/(1. - qv(i,k,j))
!              else
!                 write(msg,'(a,i3)') "WARNING: UW shallow Cu cannot handle tracer ",m
!                 call wrf_debug(100, msg)
!              end if
!           end do

           !Combine shallow and deep cumulus updraft mass flux
           cmfmc2_inout(i,k,j) = cmfmc2(1,kflip)
           cmfmc(i,k,j)        = cmfmc(i,k,j) + cmfmc2(1,kflip)

        end do !k-loop to kte

        do k = kts,kte+1
           kflip = kte-k+2

           !Convective fluxes of 'sl' and 'qt' in energy unit
           cmfsl(i,k,j) = slflx(1,kflip)
           cmflq(i,k,j) = qtflx(1,kflip)*latvap
        end do !k-loop to kte+1

        !Calculate fractional occurance of shallow convection
        !~Not doing this since it would require adding time averaging ability across output times

        !Rain rate for shallow convection
!       rainsh(i,j)  = precc(1)*1e3
        pratesh(i,j) = precc(1)*1e3/dt !~this will need changing for adaptive time steps and cudt

        !Get indices of convection top and bottom based on deep+shallow
        !Note: cnt2 and cnb2 have indices decreasing with height, but
        !      cutop and cubot have indicies increasing with height
        kflip = kte - cutop(i,j) + 1
        cnt = kflip
        if( cnt2(1) < kflip ) cnt = cnt2(1)
        cutop(i,j) = kte - cnt + 1

        kflip = kte - cubot(i,j) + 1
        cnb = kflip
        if( cnb2(1) > kflip ) cnb = cnb2(1)
        cubot(i,j) = kte - cnb + 1

        !Add shallow reserved cloud condensate to deep reserved cloud condensate
        !dlf done above, rliq (m/s)
        rliq2_inout(i,j) = rliq2(1)
        rliq(i,j)        = rliq(i,j) + rliq2(1)

     end do
  end do ij_loops
END SUBROUTINE camuwshcu_driver

END MODULE module_cu_camuwshcu_driver
