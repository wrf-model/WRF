MODULE module_cu_camzm_driver

!Roughly based on zm_conv_intr.F90 from CAM

  USE module_cam_support, only: pcnst, pcols, pver, pverp

  IMPLICIT NONE

  PRIVATE                  !Default to private
  PUBLIC :: &              !Public entities
       camzm_driver, &
       zm_conv_init

CONTAINS

!------------------------------------------------------------------------
SUBROUTINE camzm_driver(                                      &
              ids,ide, jds,jde, kds,kde                       &
             ,ims,ime, jms,jme, kms,kme                       &
             ,its,ite, jts,jte, kts,kte                       &
             ,itimestep, bl_pbl_physics, sf_sfclay_physics    &
             ,th, t_phy, tsk, tke_pbl, ust, qv, qc, qi        &
             ,mavail, kpbl, pblh, xland, z                    &
             ,z_at_w, dz8w, ht, p, p8w, pi_phy, psfc          &
             ,u_phy, v_phy, hfx, qfx, cldfra                  &
             ,tpert_camuwpbl                                  &
             ,dx, dt                                          &
             ,cape_out, mu_out, md_out, zmdt, zmdq            &
             ,rliq_out, dlf_out                               &
             ,pconvb, pconvt, cubot, cutop, raincv, pratec    &
             ,rucuten, rvcuten                                &
             ,rthcuten, rqvcuten, rqccuten, rqicuten          &
             ,evaptzm, fzsntzm, evsntzm, evapqzm, zmflxprc    &
             ,zmflxsnw, zmntprpd, zmntsnpd, zmeiheat          &
             ,cmfmc, cmfmcdzm, preccdzm, precz                &
             ,zmmtu, zmmtv, zmupgu, zmupgd, zmvpgu, zmvpgd    &
             ,zmicuu, zmicud, zmicvu, zmicvd                  &
             ,zmdice, zmdliq                                  &
                                                              )
! This routine is based on zm_conv_tend in CAM. It handles the mapping of
! variables from the WRF to the CAM framework for the Zhang-McFarlane
! convective parameterization.
!
! Author: William.Gustafson@pnl.gov, Nov. 2009
! Last modified: William.Gustafson@pnl.gov, Nov. 2010
!------------------------------------------------------------------------
  USE shr_kind_mod,    only: r8 => shr_kind_r8
  USE physconst, only: cpair, gravit
  USE module_cu_camzm, only: convtran, momtran, zm_conv_evap, zm_convr

! Subroutine arguments...
  INTEGER, INTENT(IN   ) ::    ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte,  &
                     bl_pbl_physics, & !pbl scheme
                          itimestep, & !time step index
                  sf_sfclay_physics    !surface layer scheme

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: &
                             cldfra, & !cloud fraction
                               dz8w, & !height between interfaces (m)
                                  p, & !pressure at mid-level (Pa)
                                p8w, & !pressure at level interface (Pa)
                             pi_phy, & !exner function, (p0/p)^(R/cpair) (none)
                                 qv, & !water vapor mixing ratio (kg/kg-dry air)
                                 th, & !potential temperature (K)
                            tke_pbl, & !turbulent kinetic energy from PBL (m2/s2)
                              t_phy, & !temperature (K)
                              u_phy, & !zonal wind component on T points (m/s)
                              v_phy, & !meridional wind component on T points (m/s)
                                  z, & !height above sea level at mid-level (m)
                             z_at_w    !height above sea level at interface (m)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN), OPTIONAL :: &
                                 qc, & !cloud droplet mixing ratio (kg/kg-dry air)
                                 qi    !cloud ice crystal mixing ratio (kg/kg-dry air)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
                            dlf_out, & !detraining cloud water tendendcy
                            evaptzm, & !temp. tendency - evap/snow prod from ZM (K/s)
                            fzsntzm, & !temp. tendency - rain to snow conversion from ZM (K/s)
                            evsntzm, & !temp. tendency - snow to rain conversion from ZM (K/s)
                            evapqzm, & !spec. humidity tend. - evaporation from ZM (kg/kg/s)
                           zmflxprc, & !flux of precipitation from ZM (kg/m2/s)
                           zmflxsnw, & !flux of snow from ZM (kg/m2/s)
                           zmntprpd, & !net precipitation production from ZM (kg/kg/s)
                           zmntsnpd, & !net snow production from ZM (kg/kg/s)
                           zmeiheat, & !heating by ice and evaporation from ZM (W/kg)
                              cmfmc, & !convective mass flux--m sub c, deep here but ultimately deep+shallow (kg/m2/s)
                           cmfmcdzm, & !convection mass flux from ZM deep (kg/m2/s)
                             md_out, & !output convective downdraft mass flux (kg/m2/s)
                             mu_out, & !output convective updraft mass flux (kg/m2/s)
                            rucuten, & !UNcoupled zonal wind tendency due to Cu scheme (m/s2)
                            rvcuten, & !UNcoupled meridional wind tendency due to Cu scheme (m/s2)
                           rthcuten, & !UNcoupled potential temperature tendendcy due to cu scheme (K/s)
                           rqvcuten, & !UNcoupled water vapor mixing ratio tendency due to Cu scheme (kg/kg/s)
                               zmdt, & !temp. tendency from moist convection (K/s)
                               zmdq, & !spec. humidity tendency from moist convection (kg/kg/s)
                              zmmtu, & !U tendency from ZM convective momentum transport (m/s2)
                              zmmtv, & !V tendency from ZM convective momentum transport (m/s2)
                             zmupgu, & !zonal force from ZM updraft pressure gradient term (m/s2)
                             zmupgd, & !zonal force from ZM downdraft pressure gradient term (m/s2)
                             zmvpgu, & !meridional force from ZM updraft pressure gradient term (m/s2)
                             zmvpgd, & !meridional force from ZM downdraft pressure gradient term (m/s2)
                             zmicuu, & !ZM in-cloud U updrafts (m/s)
                             zmicud, & !ZM in-cloud U downdrafts (m/s)
                             zmicvu, & !ZM in-cloud V updrafts (m/s)
                             zmicvd, & !ZM in-cloud V downdrafts (m/s)
                             zmdice, & !ZM cloud ice tendency (kg/kg/s)
                             zmdliq    !ZM cloud liquid tendency (kg/kg/s)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT), OPTIONAL :: &
                           rqccuten, & !UNcoupled cloud droplet mixing ratio tendency due to Cu scheme (kg/kg/s)
                           rqicuten    !UNcoupled ice crystal mixing ratio tendency due to Cu scheme (kg/kg/s)

  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: &
                                hfx, & !upward heat flux at sfc (W/m2)
                                 ht, & !terrain height (m)
                              xland, & !land/water mask, 1=land, 2=water
                             mavail, & !soil moisture availability
                               pblh, & !planetary boundary layer height (m)
                               psfc, & !surface pressure (Pa)
                                qfx, & !upward moisture flux at sfc (kg/m2/s)
                     tpert_camuwpbl, & !temperature perturbation from UW CAM PBL
                                tsk, & !skin temperature (K)
                                ust    !u* in similarity theory (m/s)

  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: &
                           cape_out, & !convective available potential energy (J/kg)
                              cubot, & !level number of base of convection
                              cutop, & !level number of top of convection
                             pconvb, & !pressure of base of convection (Pa)
                             pconvt, & !pressure of top of convection (Pa)
                             pratec, & !rain rate returned to WRF for accumulation (mm/s)
                           preccdzm, & !convection precipitation rate from ZM deep (m/s)
                              precz, & !total precipitation rate from ZM (m/s)
                             raincv, & !time-step convective rain amount (mm)
                           rliq_out    !vertical integral of reserved cloud water

  REAL, INTENT(IN) :: &
                                 dt, & !domain time step (s)
                                 dx    !grid spacing (m)

  INTEGER, DIMENSION( ims:ime, jms:jme), INTENT(IN) :: &
                               kpbl    !index of PBL level

! Local variables...
  !Variables dimensioned for input to ZM routines
  REAL(r8), DIMENSION(pcols, kte+1) ::  &
                               mcon, & !convective mass flux--m sub c (sub arg out in CAM)
                               pflx, & !scattered precip flux at each level (sub arg out in CAM)
                              pint8, & !pressure at layer interface (Pa)
                                zi8    !height above sea level at mid-level (m)

  REAL(r8), DIMENSION(pcols, kte, pcnst) ::  &
                                qh8    !specific humidity (kg/kg-moist air)

  REAL(r8), DIMENSION(pcols, kte, 3) ::  &
                              cloud, & !holder for cloud water and ice (q in CAM)
                           cloudtnd, & !holder for cloud tendencies (ptend_loc%q in CAM)
                             fracis    !fraction of cloud species that are insoluble

  REAL(r8), DIMENSION(pcols, kte, 2) ::  &
                               icwu, & !in-cloud winds in upraft (m/s)
                               icwd, & !in-cloud winds in downdraft (m/s)
                             pguall, & !apparent force from updraft pres. gradient (~units?)
                             pgdall, & !apparent force from downdraft pres. gradient (~units?)
                              winds, & !wind components (m/s)
                         wind_tends    !wind component tendencies (m/s2)

  REAL(r8), DIMENSION(pcols, kte) ::  &
                               cld8, & !cloud fraction
                                cme, & !cmf condensation - evaporation (~units?, sub arg out in CAM)
                                dlf, & !scattered version of the detraining cld h2o tendency (~units?)
                         fake_dpdry, & !place holder for dpdry, delta pres. of dry atmos.
                            flxprec, & !evap outfld: convective-scale flux of precip at interfaces (kg/m2/s)
                            flxsnow, & !evap outfld: convective-scale flux of snow at interfaces (kg/m2/s)
                            ntprprd, & !evap outfld: net precip production in layer (kg/kg/s)
                            ntsnprd, & !evap outfld: net snow production in layer (kg/kg/s)
                              pdel8, & !pressure thickness of layer (between interfaces, Pa)
                              pmid8, & !pressure at layer middle (Pa)
                                ql8, & !cloud liquid water (~units?)
                                qi8, & !cloud ice (~units?)
                                 t8, & !temperature (K)
                                zm8, & !height above ground at mid-level (m)
                               qtnd, & !specific humidity tendency (kg/kg/s)
                               rprd, & !rain production rate (kg/kg/s, comes from pbuf array in CAM, add to restart?~)
                               stnd, & !heating rate (dry static energy tendency, W/kg)
                      tend_s_snwprd, & !heating rate of snow production (~units?)
                    tend_s_snwevmlt, & !heating rate of evap/melting of snow (~units?)
                                zdu    !detraining mass flux (~units? sub arg out in CAM)

  REAL(r8), DIMENSION(pcols) ::  &
                               cape, & !convective available potential energy (J/kg)
                              jctop, & !row of top-of-deep-convection indices passed out (sub arg out in CAM)
                              jcbot, & !row of base of cloud indices passed out (sub arg out in CAM)
                           landfrac, & !land fraction
                              pblh8, & !planetary boundary layer height (m)
                               phis, & !geopotential at terrain height (m2/s2)
                               prec, & !convective-scale precipitation rate from ZM (m/s)
                               rliq, & !reserved liquid (not yet in cldliq) for energy integrals (units? sub arg out in CAM)
                               snow, & !convective-scale snowfall rate from ZM (m/s)
                              tpert    !thermal (convective) temperature excess (K)

  !Variables that were declared at the module level of zm_conv_intr in
  !CAM. In that context they needed to be held between calls to
  !zm_conv_tend and zm_conv_tend2 at the chunk level. In WRF, these vars
  !are setup to hold the whole "memory" dimension, but as a 1-D vector
  !instead of a 2-D array as is typically done in WRF. This allows us to
  !leave the CAM routines in tact. For now, this forces us to immediately
  !call zm_conv_tend2 before leaving this module, but it allows us to
  !follow the WRF rules. We can deal with generalizing this for handling
  !tracer convective transport of aerosols later.~
  REAL(r8), DIMENSION(pcols, kte, (ime-ims+1)*(jme-jms+1)) :: &
                                 dp, & !layer pres. thickness between interfaces (mb)
                                 du, &
                                 ed, &
                                 eu, &
                                 md, &
                                 mu

  REAL(r8), DIMENSION(pcols, (ime-ims+1)*(jme-jms+1)) :: &
                            dsubcld    ! layer pres. thickness between LCL and maxi (mb)

  INTEGER, DIMENSION(pcols, (ime-ims+1)*(jme-jms+1)) :: &
                              ideep, & ! holds position of gathered points
                                 jt, & ! top-level index of deep cumulus convection
                               maxg    ! gathered values of maxi
                               
  INTEGER, DIMENSION((ime-ims+1)*(jme-jms+1)) :: &
                            lengath    ! number of gathered points

  !Other local vars
  INTEGER :: i, j, k, kflip, n, ncnst
  INTEGER :: lchnk         !chunk identifier, used to map 2-D to 1-D arrays in WRF
  INTEGER :: ncol          !number of atmospheric columns in chunk
  LOGICAL, DIMENSION(3) :: l_qt    !logical switches for constituent tendency presence
  LOGICAL, DIMENSION(2) :: l_windt !logical switches for wind tendency presence

!
! Initialize...
!
  ncol  = 1          !chunk size in WRF is 1 since we loop over all columns in a tile

  cape_out(its:ite, jts:jte)        = 0.
  mu_out(its:ite, kts:kte, jts:jte) = 0.
  md_out(its:ite, kts:kte, jts:jte) = 0.
  zmdt(its:ite, kts:kte, jts:jte)   = 0.
!
! Map variables to inputs for zm_convr and call it...
! Loop over the points in the tile and treat them each as a CAM chunk.
!
  do j = jts,jte
     do i = its,ite
        lchnk = (j-jts)*(ite-its+1) + (i-its+1) !1-D index location from the 2-D tile

        !Flip variables on the layer middles
        do k = kts,kte
           kflip = kte-k+1

           cld8(1,kflip)  = cldfra(i,k,j)
           pdel8(1,kflip) = p8w(i,k,j) - p8w(i,k+1,j)
           pmid8(1,kflip) = p(i,k,j)
           qh8(1,kflip,1) = max( qv(i,k,j)/(1.+qv(i,k,j)), 1e-30 ) !values of 0 cause a crash in entropy
           if( present(qc) ) then
              ql8(1,kflip) = qc(i,k,j)/(1.+qv(i,k,j)) !Convert to moist mix ratio
           else
              ql8(1,kflip) = 0.
           end if
           if( present(qi) ) then
              qi8(1,kflip) = qi(i,k,j)/(1.+qv(i,k,j)) !Used in convtran, ditto for conversion
           else
              qi8(1,kflip) = 0.
           end if
           t8(1,kflip)    = t_phy(i,k,j)
           zm8(1,kflip)   = z(i,k,j) - ht(i,j) !Height above the ground at midlevels
        end do

        !Flip variables on the layer interfaces
        do k = kts,kte+1
           kflip = kte-k+2

           pint8(1,kflip) = p8w(i,k,j)
           zi8(1,kflip)   = z_at_w(i,k,j) -ht(i,j) !Height above the ground at interfaces
        end do

        !Other necessary conversions for input to ZM
        if( xland(i,j)==2 ) then
           landfrac(1) = 1. !land, WRF is all or nothing
        else
           landfrac(1) = 0. !water
        end if
        pblh8(1) = pblh(i,j)
        phis(1)  = ht(i,j)*gravit

        call get_tpert(bl_pbl_physics, sf_sfclay_physics, dx, &
             mavail(i,j), kpbl(i,j), pblh(i,j), &
             dz8w(i,1,j), psfc(i,j), qv(i,1,j), t_phy(i,1,j), &
             th(i,1,j), tsk(i,j), tke_pbl(i,:,j), ust(i,j), &
             u_phy(i,1,j), v_phy(i,1,j), hfx(i,j), qfx(i,j), &
             tpert_camuwpbl(i,j), kte, &
             tpert(1))

        !Call the Zhang-McFarlane (1996) convection parameterization
        !NOTE: The 0.5*dt is correct and is a nuance of CAM typically
        !      using 2*dt for physics tendencies. Everywhere in zm_convr
        !      that dt is used, the dt is multiplied by 2 to get back to
        !      the 2*dt. Everywhere else in the CAM ZM interface the full
        !      2*dt is passed into the subroutines. In WRF we use 1*dt
        !      in place of CAM's 2*dt since the adjustment is made
        !      elsewhere.
        call zm_convr( lchnk, ncol, &
             t8, qh8, prec, jctop, jcbot, &
             pblh8, zm8, phis, zi8, qtnd, &
             stnd, pmid8, pint8, pdel8, &
             0.5_r8*real(dt,r8), mcon, cme, cape, &
             tpert, dlf, pflx, zdu, rprd, &
             mu(:,:,lchnk), md(:,:,lchnk),du(:,:,lchnk),eu(:,:,lchnk),ed(:,:,lchnk), &
             dp(:,:,lchnk), dsubcld(:,lchnk), jt(:,lchnk), maxg(:,lchnk), ideep(:,lchnk), &
             lengath(lchnk), ql8, rliq, landfrac )

        !Start mapping CAM output to WRF output variables. We follow the
        !same order as in CAM's zm_conv_tend to ease maintenance...
        do k=kts,kte
           kflip = kte-k+1
           dlf_out(i,k,j)  = dlf(1,kflip)
        end do
        cape_out(i,j) = cape(1)
        rliq_out(i,j) = rliq(1)

        !Convert mass flux from reported mb/s to kg/m2/s
        mcon(:ncol,:pver) = mcon(:ncol,:pver) * 100._r8/gravit

        ! Store upward and downward mass fluxes in un-gathered arrays
        ! + convert from mb/s to kg/m2/s
        do n=1,lengath(lchnk)
           do k=kts,kte
              kflip = kte-k+1
!              ii = ideep(n,lchnk) <--in WRF this is always 1 because chunk size=1
              mu_out(i,k,j) = mu(n,kflip,lchnk) * 100._r8/gravit
              md_out(i,k,j) = md(n,kflip,lchnk) * 100._r8/gravit
           end do
        end do

        do k=kts,kte
           kflip = kte-k+1
           zmdt(i,k,j) = stnd(1,kflip)/cpair
           zmdq(i,k,j) = qtnd(1,kflip)
        end do

        !Top and bottom pressure of convection
        pconvt(i,j) = p8w(i,1,j)
        pconvb(i,j) = p8w(i,1,j)
        do n = 1,lengath(lchnk)
           if (maxg(n,lchnk).gt.jt(n,lchnk)) then
              pconvt(i,j) = pmid8(ideep(n,lchnk),jt(n,lchnk))  ! gathered array (or jctop ungathered)
              pconvb(i,j) = pmid8(ideep(n,lchnk),maxg(n,lchnk))! gathered array
           endif
        end do
        cutop(i,j) = jctop(1)
        cubot(i,j) = jcbot(1)

        !Add tendency from this process to tendencies arrays. Also,
        !increment the local state arrays to reflect additional tendency
        !from zm_convr, i.e. the physics update call in CAM. Note that
        !we are not readjusting the pressure levels to hydrostatic
        !balance for the new virtual temperature like is done in CAM. We
        !will let WRF take care of such things later for the total
        !tendency.
        do k=kts,kte
           kflip = kte-k+1

           !Convert temperature to potential temperature and
           !specific humidity to water vapor mixing ratio
           rthcuten(i,k,j) = zmdt(i,k,j)/pi_phy(i,k,j)
           rqvcuten(i,k,j) = zmdq(i,k,j)/(1._r8 - zmdq(i,k,j))

           t8(1,kflip)    = t8(1,kflip) + zmdt(i,k,j)*real(dt,r8)
           qh8(1,kflip,1) = qh8(1,kflip,1) + zmdq(i,k,j)*real(dt,r8)
        end do

        ! Determine the phase of the precipitation produced and add latent heat of fusion
        ! Evaporate some of the precip directly into the environment (Sundqvist)
        ! Allow this to use the updated state1 (t8 and qh8 in WRF) and the fresh ptend_loc type
        ! heating and specific humidity tendencies produced
        qtnd = 0._r8 !re-initialize tendencies (i.e. physics_ptend_init(ptend_loc))
        stnd = 0._r8
        call zm_conv_evap(ncol, lchnk, &
             t8, pmid8, pdel8, qh8(:,:,1), &
             stnd, tend_s_snwprd, tend_s_snwevmlt, qtnd, &
             rprd, cld8, real(dt,r8), &
             prec, snow, ntprprd, ntsnprd , flxprec, flxsnow)

        ! Parse output variables from zm_conv_evap
        do k=kts,kte
           kflip = kte-k+1

           evaptzm(i,k,j)  = stnd(1,kflip)/cpair
           fzsntzm(i,k,j)  = tend_s_snwprd(1,kflip)/cpair
           evsntzm(i,k,j)  = tend_s_snwevmlt(1,kflip)/cpair
           evapqzm(i,k,j)  = qtnd(1,kflip)
           zmflxprc(i,k,j) = flxprec(1,kflip)
           zmflxsnw(i,k,j) = flxsnow(1,kflip)
           zmntprpd(i,k,j) = ntprprd(1,kflip)
           zmntsnpd(i,k,j) = ntsnprd(1,kflip)
           zmeiheat(i,k,j) = stnd(1,kflip) !Do we really need this and evaptzm?
           cmfmc(i,k,j)    = mcon(1,kflip) !Set to deep value here, shallow added in UW scheme
           cmfmcdzm(i,k,j) = mcon(1,kflip)
           preccdzm(i,j)   = prec(1)       !Rain rate from just deep
           precz(i,j)      = prec(1)       !Rain rate for total convection (just deep right now)
           pratec(i,j)     = prec(1)*1e3   !Rain rate used in WRF for accumulation (mm/s)
           raincv(i,j)     = pratec(i,j)*dt !Rain amount for time step returned back to WRF
        end do

        !Add tendency from zm_conv_evap to tendencies arrays. Also,
        !increment the local state arrays to reflect additional tendency
        !Note that we are not readjusting the pressure levels to hydrostatic
        !balance for the new virtual temperature like is done in CAM. We
        !will let WRF take care of such things later for the total
        !tendency.
        do k=kts,kte
           kflip = kte-k+1

           !Convert temperature to potential temperature and
           !specific humidity to water vapor mixing ratio
           rthcuten(i,k,j) = rthcuten(i,k,j) + &
                             evaptzm(i,k,j)/pi_phy(i,k,j)
           rqvcuten(i,k,j) = rqvcuten(i,k,j) + &
                             evapqzm(i,k,j)/(1. - qv(i,k,j))

           t8(1,kflip)    = t8(1,kflip) + evaptzm(i,k,j)*real(dt,r8)
           qh8(1,kflip,1) = qh8(1,kflip,1) + evapqzm(i,k,j)*real(dt,r8)
        end do

        ! Momentum transport
        stnd       = 0._r8     !Zero relevant tendencies in preparation
        wind_tends = 0._r8
        do k=kts,kte
           kflip = kte-k+1
           winds(1,k,1) = u_phy(i,kflip,j)
           winds(1,k,2) = v_phy(i,kflip,j)
        end do
        l_windt(1:2) = .true.

        call momtran (lchnk, ncol, &
             l_windt, winds, 2, mu(:,:,lchnk), md(:,:,lchnk), &
             du(:,:,lchnk), eu(:,:,lchnk), ed(:,:,lchnk), dp(:,:,lchnk), dsubcld(:,lchnk), &
             jt(:,lchnk),maxg(:,lchnk), ideep(:,lchnk), 1, lengath(lchnk), &
             itimestep, wind_tends, pguall, pgdall, icwu, icwd, real(dt,r8), stnd )
        
        !Add tendency from momtran to tendencies arrays. Also,
        !increment the local state arrays to reflect additional tendency
        !Note that we are not readjusting the pressure levels to hydrostatic
        !balance for the new virtual temperature like is done in CAM. We
        !will let WRF take care of such things later for the total
        !tendency.
        do k=kts,kte
           kflip = kte-k+1

           !Convert temperature to potential temperature and
           !specific humidity to water vapor mixing ratio
           rucuten(i,k,j)  = wind_tends(1,kflip,1)
           rvcuten(i,k,j)  = wind_tends(1,kflip,2)
           rthcuten(i,k,j) = rthcuten(i,k,j) + &
                                 stnd(1,kflip)/cpair/pi_phy(i,k,j)

           t8(1,kflip) = t8(1,kflip) + stnd(1,kflip)/cpair*real(dt,r8)
           !winds is not used again so do not bother updating them
        end do

        !Parse output arrays for momtran
        do k=kts,kte
           kflip = kte-k+1

           zmmtu(i,k,j) = wind_tends(1,kflip,1) !wind tendencies...
           zmmtv(i,k,j) = wind_tends(1,kflip,2)

           zmupgu(i,k,j) = pguall(1,kflip,1)   !apparent force pres. grads...
           zmupgd(i,k,j) = pgdall(1,kflip,1)
           zmvpgu(i,k,j) = pguall(1,kflip,2)
           zmvpgd(i,k,j) = pgdall(1,kflip,2)

           zmicuu(i,k,j) = icwu(1,kflip,1)     !in-cloud winds...
           zmicud(i,k,j) = icwd(1,kflip,1)
           zmicvu(i,k,j) = icwu(1,kflip,2)
           zmicvd(i,k,j) = icwd(1,kflip,2)
        end do

        !Setup for convective transport of cloud water and ice
        !~We should set this up to use an integer pointer instead of
        ! hard-coded numbers for each species so that we can easily
        ! handle other species. Something to come back to later...
        l_qt(1)   = .false.     !do not mix water vapor
        l_qt(2:3) = .true.      !do mix cloud water and ice
        cloudtnd = 0._r8
        fracis(1,:,1:3) = 0._r8 !all cloud liquid & ice is soluble
        ncnst = 3               !number of constituents in cloud array (including vapor)
        fake_dpdry = 0._r8      !delta pres. for dry atmos. (0 since assuming moist mix ratios)
        do k=kts,kte
           kflip = kte-k+1

           cloud(1,kflip,1) = qh8(1,kflip,1)  !We can either use moist mix ratios, as is
           cloud(1,kflip,2) = ql8(1,kflip)    !done here, or else use dry mix ratios, send
           cloud(1,kflip,3) = qi8(1,kflip)    !in appropriate dpdry values, and return the
                                              !approp. response from cnst_get_type_byind
        end do

        call convtran (lchnk, &
             l_qt, cloud, ncnst,  mu(:,:,lchnk), md(:,:,lchnk), &
             du(:,:,lchnk), eu(:,:,lchnk), ed(:,:,lchnk), dp(:,:,lchnk), dsubcld(:,lchnk), &
             jt(:,lchnk), maxg(:,lchnk), ideep(:,lchnk), 1, lengath(lchnk), &
             itimestep, fracis, cloudtnd, fake_dpdry)

        !Parse output for convtran and increment tendencies
        do k=kts,kte
           kflip = kte-k+1

           zmdice(i,k,j) = cloudtnd(1,kflip,3)
           zmdliq(i,k,j) = cloudtnd(1,kflip,2)

           !Convert cloud tendencies from wet to dry mix ratios
           if( present(rqccuten) ) then
              rqccuten(i,k,j) = cloudtnd(1,kflip,2)/(1. - qv(i,k,j))
           end if
           if( present(rqicuten) ) then
              rqicuten(i,k,j) = cloudtnd(1,kflip,3)/(1. - qv(i,k,j))
           end if
        end do
        
     end do !i-loop
  end do    !j-loop
END SUBROUTINE camzm_driver


!------------------------------------------------------------------------
SUBROUTINE zm_conv_init(rucuten, rvcuten, rthcuten, rqvcuten,           &
                     rqccuten, rqicuten,                                &
                     p_qc, p_qi, param_first_scalar,                    &
                     restart,                                           &
                     ids, ide, jds, jde, kds, kde,                      &
                     ims, ime, jms, jme, kms, kme,                      &
                     its, ite, jts, jte, kts, kte                    )
! Initialization routine for Zhang-McFarlane cumulus parameterization
! from CAM. The routine with this name in CAM is revamped here to give
! the needed functionality within WRF.
!
! Author: William.Gustafson@pnl.gov, Nov. 2009
!------------------------------------------------------------------------
  USE module_cam_esinti,  only: esinti
  USE physconst, only: epsilo, latvap, latice, rh2o, cpair, tmelt
  USE module_bl_camuwpbl_driver, only: vd_register
  USE module_cu_camzm, only: zm_convi, zmconv_readnl

  LOGICAL , INTENT(IN)           ::   restart
  INTEGER , INTENT(IN)           ::   ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte, &
                                      p_qc, p_qi, param_first_scalar

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(OUT) :: &
                                                           rucuten, &
                                                           rvcuten, &
                                                          rthcuten, &
                                                          rqvcuten, &
                                                          rqccuten, &
                                                          rqicuten

  integer :: i, itf, j, jtf, k, ktf
  integer :: limcnv

  jtf = min(jte,jde-1)
  ktf = min(kte,kde-1)
  itf = min(ite,ide-1)
!
! Initialize module_cam_support variables...
! This could be moved to a master "cam-init" subroutine once multiple CAM
! parameterizations are implemented in WRF. For now, it doesn't hurt to
! have these possibly initialized more than once since they are
! essentially constant.
!
  pver  = ktf-kts+1
  pverp = pver+1
!
! Initialize the saturation vapor pressure look-up table...
! This could be moved to a master cam-init subroutine too if it is needed
! by more than one CAM parameterization. In CAM this is called from
! phys_init.
!
  call esinti(epsilo, latvap, latice, rh2o, cpair, tmelt)

!~Need code here to set limcnv to max convective level of 40 mb... To 
! properly set an average value for the whole domain would involve doing
! a collective operation across the whole domain to get pressure averages
! by level, which is difficult within the WRF framework. So, we will just
! use the model top for now.
!
! Technically, limcnv should be calculated for each grid point at each
! time because the levels in WRF do not have a constant pressure, as
! opposed to CAM. But, that would involve making this variable local
! instead of at the module level as is currently done in CAM. For now,
! we will not worry about this because WRF rarely has domains higher than
! 40 mb. There is also little variability between grid points near the
! model top due to the underlying topography so a constant value would
! be OK across the comain. Also, remember that CAM levels are reversed in
! the vertical from WRF. So, setting limcnv to 1 means the top of the
! domain. Note that because of a bug in the parcel_dilute routine, limcnv
! must be >=2 instead of 1. Otherwise, an array out-of-bounds occurs.
  limcnv = 2
!
! Get the ZM namelist variables (hard-wired for now)...
!
  call zmconv_readnl("hard-wired")
! 
!~need to determine if convection should happen in PBL and set
! no_deep_pbl_in accordingly
  call zm_convi(limcnv, NO_DEEP_PBL_IN=.false.)

!
! Set initial values for arrays dependent on restart conditions
!
  if(.not.restart)then
     do j=jts,jtf
        do k=kts,ktf
           do i=its,itf
              rucuten(i,k,j)  = 0.
              rvcuten(i,k,j)  = 0.
              rthcuten(i,k,j) = 0.
              rqvcuten(i,k,j) = 0.
              if( p_qc > param_first_scalar ) rqccuten(i,k,j) = 0.
              if( p_qi > param_first_scalar ) rqicuten(i,k,j) = 0.
           enddo
        enddo
     enddo
  end if

END SUBROUTINE zm_conv_init


!------------------------------------------------------------------------
SUBROUTINE get_tpert(bl_pbl_physics, sf_sfclay_physics, dx, &
     mavail, kpbl, pblh, dzlowest, &
     psfc, qvlowest, t_phylowest, thlowest, tsk, tke_pbl, ust, &
     u_phylowest, v_phylowest, hfx, qfx, tpert_camuwpbl, kte, tpert)
! Calculates the temperature perturbation used to trigger convection.
! This should only be used if tpert is not already provided by CAM's PBL
! scheme. Right now, this only works with the Mellor-Yamada boundary
! layer scheme in combination with the MYJ surface scheme. This is due to
! the need of TKE for the vertical velocity perturbation. This scheme has
! not been generalized to handle all schemes that produce TKE at this
! time, and the non-TKE schemes, e.g. YSU, could probably have an
! appropriate tpert deduced but we have not taken the time yet to do it.
!
! Author: William.Gustafson@pnl.gov, Nov. 2009
! Last updated: William.Gustafson@pnl.gov, Nov. 2010
!------------------------------------------------------------------------
  USE shr_kind_mod,    only: r8 => shr_kind_r8
  USE module_model_constants, only: cp, ep_1, ep_2, g, r_d, rcp, &
                                    svp1, svp2, svp3, svpt0, xlv
  USE module_state_description, ONLY : SFCLAYSCHEME              &
                                      ,MYJSFCSCHEME              &
                                      ,GFSSFCSCHEME              &
                                      ,SLABSCHEME                &
                                      ,LSMSCHEME                 &
                                      ,RUCLSMSCHEME              &
                                      ,MYJPBLSCHEME              &
                                      ,CAMUWPBLSCHEME
!
! Subroutine arguments...
!
  real, dimension(:), intent(in) :: tke_pbl
  real, intent(in) :: dx, dzlowest, hfx, mavail, pblh, psfc, qvlowest, &
       tpert_camuwpbl, tsk, t_phylowest, thlowest, ust, u_phylowest, &
       v_phylowest
  integer, intent(in) :: bl_pbl_physics, kpbl, kte, sf_sfclay_physics
  real(r8),intent(inout) :: tpert
!
! Local vars...
!
  real, parameter :: fak      = 8.5   !Constant in surface temperature excess         
  real, parameter :: tfac     = 1.    !Ratio of 'tpert' to (w't')/wpert
  real, parameter :: wfac     = 1.    !Ratio of 'wpert' to sqrt(tke)
  real, parameter :: wpertmin = 1.e-6 !Min PBL eddy vertical velocity perturbation
  real :: ebrk                        !In CAM, net mean TKE of CL including
                                      !entrainment effect (m2/s2). In WRF,
                                      !average TKE within the PBL
  real :: br2, dthvdz, e1, flux, govrth, psfccmb, qfx, qsfc, rhox, thgb, &
       thv, tskv, tvcon, vconv, vsgd, wpert, wspd, za
  integer :: k
  character(len=250) :: msg
  logical :: UnstableOrNeutral
!
! We can get the perturbation values directly from CAMUWPBLSCHEME if
! available. Otherwise, we have to calculate them.
!
  if( bl_pbl_physics==CAMUWPBLSCHEME ) then
     tpert = tpert_camuwpbl
!
!...non-CAMUWPBL. Need MYJ SFC & PBL for now until other schemes
!   get coded to give perturbations too.
! First, we need to determine if the conditions are stable or unstable.
! We will do this by mimicing the bulk Richardson calculation from
! module_sf_sfclay.F because the MYJ scheme does not provide this info.
! The logic borrowed from the CuP shallow cumulus scheme. Non-MYJ sfc
! scheme code is commented out for now since we also require MYJ PBL
! scheme.
!
  elseif( bl_pbl_physics==MYJPBLSCHEME ) then

     UnstableOrNeutral = .false.
     sfclay_case: SELECT CASE (sf_sfclay_physics)
     CASE (MYJSFCSCHEME)
        ! The MYJ sfc scheme does not already provide a stability criteria.
        ! So, we will mimic the bulk Richardson calculation from
        ! module_sf_sfclay.F.

        if( pblh <= 0. ) call wrf_error_fatal( &
             "CAMZMSCHEME needs a PBL height from a PBL scheme.")

        za     = 0.5*dzlowest

        e1     = svp1*exp(svp2*(tsk-svpt0)/(tsk-svp3))
        psfccmb=psfc/1000.  !converts from Pa to cmb
        qsfc   = ep_2*e1/(psfccmb-e1)
        thgb   = tsk*(100./psfccmb)**rcp
        tskv   = thgb*(1.+ep_1*qsfc*mavail)
        tvcon  = 1.+ep_1*qvlowest
        thv    = thlowest*tvcon
        dthvdz = (thv-tskv)

        govrth = g/thlowest

        rhox   = psfc/(r_d*t_phylowest*tvcon)
        flux   = max(hfx/rhox/cp + ep_1*tskv*qfx/rhox,0.)
        vconv  = (g/tsk*pblh*flux)**.33
        vsgd   = 0.32 * (max(dx/5000.-1.,0.))**.33
        wspd   = sqrt(u_phylowest*u_phylowest+v_phylowest*v_phylowest)
        wspd   = sqrt(wspd*wspd+vconv*vconv+vsgd*vsgd)
        wspd   = max(wspd,0.1)

        !And finally, the bulk Richardson number...
        br2   = govrth*za*dthvdz/(wspd*wspd)

        if( br2 <= 0. ) UnstableOrNeutral = .true.

     CASE DEFAULT
        call wrf_error_fatal("CAMZMSCHEME requires MYJSFCSCHEME or else CAMUWPBLSCHEME.")

     END SELECT sfclay_case
!
! The perturbation temperature for unstable conditions...
! The calculation follows the one in caleddy inside eddy_diff.F90 from
! CAM.
!
     !Check that we are using the MJY BL scheme since we are hard-wired to
     !use TKE and u* from this scheme. At some point this dependency should
     !be broken and a way needs to be found for other schemes to provide
     !reasonable tpert values too.
     if( bl_pbl_physics /= MYJPBLSCHEME ) &
          call wrf_error_fatal("CAMZMSCHEME requires MYJPBLSCHEME or CAMUWPBLSCHEME")
   

     !Recalculate rhox in case a non-MYJ scheme is used to get
     !stability and rhox is not calculated above. Right now, this is
     !technically reduncant, but cheap.
     tvcon = 1.+ep_1*qvlowest
     rhox  = psfc/(r_d*t_phylowest*tvcon)

     if( UnstableOrNeutral ) then
        !1st, get avg TKE w/n the PBL as a proxy for ebrk variable in CAM
        ebrk = 0.
        do k=1,min(kpbl,kte)
           ebrk = ebrk + tke_pbl(k)
        end do
        ebrk = ebrk/real(kpbl)

        wpert = max( wfac*sqrt(ebrk), wpertmin )
        tpert = max( abs(hfx/rhox/cp)*tfac/wpert, 0. )
!
! Or, the perturbation temperature for stable conditions...
!
     else !Stable conditions
        tpert = max( hfx/rhox/cp*fak/ust, 0. )
     end if

  else
     call wrf_error_fatal("CAMZMSCHEME requires MYJPBLSCHEME or CAMUWPBLSCHEME")

  end if !PBL choice

END SUBROUTINE get_tpert

END MODULE module_cu_camzm_driver
