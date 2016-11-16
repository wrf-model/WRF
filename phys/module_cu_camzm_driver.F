MODULE module_cu_camzm_driver

  !Roughly based on zm_conv_intr.F90 from CAM
  
  !-------------------------------------------
  !Future Modifications and important warning (BSINGH:02/01/2013: Notes from WIG):
  !===========================================
  !1. Fracis is hardwired to 1 for first 3 constituents (vapor,liq mass,ice mass) 
  !   for WRF_CHEM simulations and first 5 constituents(liq # and ice #) for WRF
  !   physcis ONLY runs. Should be generalized for other constituents (like snow,graupel etc.)
  !
  !2. In CAM model, ZM scheme only acts up to 40 mb. In WRF, it will function all the way 
  !   to the model top no matter how high it is. We could do a better job at capping this 
  !   by checking to see where 40 mb roughly occurs during initialization. It won't change 
  !   too dramatically over time. Also, WRF often uses a top of 50 mb, so we rarely hit 40 mb.
  !   This is a minor issue.
  !3. ZM is set to never do convection w/in the PBL. In CAM model, this is dependent on the 
  !   use of UW shallow. Should this be a conditional setting in WRF or hard-wired as it 
  !   currently is?
  !-----------------------------------------


  USE module_cam_support, only: pcnst =>pcnst_runtime, pcols, pver, pverp
#if ( WRF_CHEM == 1 )
  USE module_cam_support, only: cam_mam_aerosols
#endif
  USE shr_kind_mod,    only: r8 => shr_kind_r8
  USE module_cu_camzm, only: convtran, momtran, zm_conv_evap, zm_convr

  IMPLICIT NONE

  PRIVATE                  !Default to private
  PUBLIC :: &              !Public entities
       camzm_driver  , &
#if ( WRF_CHEM == 1 )
       zm_conv_tend_2, &
#endif
       zm_conv_init

  !Module level variables which are required for zm_conv_tend_2 subrouine
  integer :: ixcldliq, ixcldice, ixnumliq, ixnumice
  
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
             ,u_phy, v_phy, hfx, qfx, cldfra, cldfra_mp_all   &
             ,is_CAMMGMP_used, tpert_camuwpbl                 &
             ,dx, dt, stepcu, cudt, curr_secs, adapt_step_flag&
             ,cudtacttime                                     & 
             ,cape_out, mu_out, md_out, zmdt, zmdq            &
             ,rliq_out, dlf_out                               &
             ,pconvb, pconvt, cubot, cutop, raincv, pratec    &
             ,rucuten, rvcuten                                &
             ,rthcuten, rqvcuten, rqccuten, rqicuten          &
             ,rqcncuten, rqincuten                            &
             ,evaptzm, fzsntzm, evsntzm, evapqzm, zmflxprc    &
             ,zmflxsnw, zmntprpd, zmntsnpd, zmeiheat          &
             ,cmfmc, cmfmcdzm, preccdzm, precz                &
             ,zmmtu, zmmtv, zmupgu, zmupgd, zmvpgu, zmvpgd    &
             ,zmicuu, zmicud, zmicvu, zmicvd                  &
             ,zmdice, zmdliq                                  &
             !Following intent-OUT variables are required by wet scavenging code of WRF_CHEM package- Balwinder.Singh@pnnl.gov
             ,evapcdp3d, icwmrdp3d, rprddp3d                  &
             !Following intent-OUT variables are required by zm_conv_tend_2 call 
             ,dp3d, du3d, ed3d, eu3d, md3d, mu3d, dsubcld2d   &
             ,ideep2d, jt2d, maxg2d, lengath2d                )

! This routine is based on zm_conv_tend in CAM. It handles the mapping of
! variables from the WRF to the CAM framework for the Zhang-McFarlane
! convective parameterization.
!
! Author: William.Gustafson@pnl.gov, Nov. 2009
! Last modified: William.Gustafson@pnl.gov, Nov. 2010
!------------------------------------------------------------------------
  USE physconst,     only: latice,cpair, gravit
  USE wv_saturation, only: fqsatd,epsqs, polysvp
  USE wv_saturation,            only: fqsatd,epsqs, polysvp

! Subroutine arguments...
  logical, intent(in)    ::    is_CAMMGMP_used
  INTEGER, INTENT(IN   ) ::    ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               its,ite, jts,jte, kts,kte,  &
                     bl_pbl_physics, & !pbl scheme
                          itimestep, & !time step index
                  sf_sfclay_physics, & !surface layer scheme
                             stepcu    !number of time steps between Cu calls

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: &
                             cldfra, & !cloud fraction
                      cldfra_mp_all, & !cloud fraction from CAMMGMP microphysics
                               dz8w, & !height between interfaces (m)
                                  p, & !pressure at mid-level (Pa)
                                p8w, & !pressure at level interface (Pa)
                             pi_phy, & !exner function, (p/p0)^(R/cpair) (none)
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
                           rqicuten, & !UNcoupled ice crystal mixing ratio tendency due to Cu scheme (kg/kg/s)
                          rqcncuten, & !PMA
                          rqincuten    !PMA


  !variables required by wet scavenging in WRF_CHEM -Balwinder.Singh@pnnl.gov
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT) :: & 
                          evapcdp3d, & !Evaporation of deep convective precipitation (kg/kg/s)
                          icwmrdp3d, & !Deep Convection in-cloud water mixing ratio (kg/m2)
                           rprddp3d    !dq/dt due to deep convective rainout (kg/kg/s)

  !variables required by zm_conv_tend_2 call
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT) :: &
                               dp3d, &
                               du3d, &
                               ed3d, &
                               eu3d, &
                               md3d, &
                               mu3d


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
  REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: &
                           dsubcld2d

  REAL, INTENT(IN) :: &
                               cudt, & !cumulus time step (min)
                          curr_secs, & !current forecast time (s)
                                 dt, & !domain time step (s)
                                 dx    !grid spacing (m)

  REAL, INTENT (INOUT) :: &
                        cudtacttime    !for adaptive time stepping, next to to run scheme (s)   

  INTEGER, DIMENSION( ims:ime, jms:jme), INTENT(IN) :: &
                               kpbl    !index of PBL level

  INTEGER, DIMENSION( ims:ime, jms:jme), INTENT(OUT) :: &
                           ideep2d, &
                           jt2d,    &
                           maxg2d,  &
                        lengath2d 

  LOGICAL, INTENT(IN), OPTIONAL :: &
                    adapt_step_flag    !using adaptive time steps?

! Local variables...
  !Variables dimensioned for input to ZM routines
  REAL(r8), DIMENSION(pcols, kte+1) ::  &
                               mcon, & !convective mass flux--m sub c (sub arg out in CAM)
                               pflx, & !scattered precip flux at each level (sub arg out in CAM)
                              pint8, & !pressure at layer interface (Pa)
                                zi8, & !height above sea level at mid-level (m)
                            flxprec, & !evap outfld: convective-scale flux of precip at interfaces (kg/m2/s)
                            flxsnow    !evap outfld: convective-scale flux of snow at interfaces (kg/m2/s)


  REAL(r8), DIMENSION(pcols, kte, pcnst) ::  &
                                qh8    !specific humidity (kg/kg-moist air)

  REAL(r8), DIMENSION(pcols, kte, 3) ::  &
                              cloud, & !holder for cloud water and ice (q in CAM)
                           cloudtnd    !holder for cloud tendencies (ptend_loc%q in CAM)

!BSINGH - 02/18/2013: FRACIS is used for qv,qc and qi for WRF_CHEM simulation but for
! WRF simulations it is used for qnc and qni also
#if ( WRF_CHEM == 1 )
 REAL(r8), DIMENSION(pcols, kte, 3) ::  &
#else
 REAL(r8), DIMENSION(pcols, kte, 5) ::  &
#endif
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
                            ntprprd, & !evap outfld: net precip production in layer (kg/kg/s)
                            ntsnprd, & !evap outfld: net snow production in layer (kg/kg/s)
                              pdel8, & !pressure thickness of layer (between interfaces, Pa)
                              pmid8, & !pressure at layer middle (Pa)
                                ql8, & !in cloud liquid water (~units?)
                             ql8prm, & !cloud liquid water (~units?)
                                qi8, & !cloud ice (~units?)
                                 t8, & !temperature (K)
                                zm8, & !height above ground at mid-level (m)
                               qtnd, & !specific humidity tendency (kg/kg/s)
                               rprd, & !rain production rate (kg/kg/s, comes from pbuf array in CAM, add to restart?~)
                               stnd, & !heating rate (dry static energy tendency, W/kg)
                      tend_s_snwprd, & !heating rate of snow production (~units?)
                    tend_s_snwevmlt, & !heating rate of evap/melting of snow (~units?)
                                zdu, & !detraining mass flux (~units? sub arg out in CAM)
                            evapcdp    !Evaporation of deep convective precipitation (kg/kg/s)

  REAL(r8), DIMENSION(pcols, kte) ::  &
                                esl, & !saturation vapor pressure
                                qvs, & !saturation specific humidity
                          qcten_det, &
                          qiten_det, &
                         qcnten_det, &
                         qinten_det, &
                          qsten_det
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
  REAL(r8):: dum1,cudts,hcudts
  INTEGER :: i, j, k, kflip, n, ncnst
  INTEGER :: lchnk         !chunk identifier, used to map 2-D to 1-D arrays in WRF
  INTEGER :: ncol          !number of atmospheric columns in chunk
  LOGICAL, DIMENSION(3) :: l_qt    !logical switches for constituent tendency presence
  LOGICAL, DIMENSION(2) :: l_windt !logical switches for wind tendency presence
  LOGICAL :: run_param , & !flag for handling alternate cumulus call freq.
             doing_adapt_dt , decided
!
! Check to see if this is a convection timestep...
!

!  Initialization for adaptive time step.

   if (cudt .eq. 0.) then
    cudts = dt
    hcudts=cudts*0.5_r8
   else 
    cudts=cudt*60._r8
    hcudts=cudts*0.5_r8
   end if

   doing_adapt_dt = .FALSE.
   IF ( PRESENT(adapt_step_flag) ) THEN
      IF ( adapt_step_flag ) THEN
         doing_adapt_dt = .TRUE.
         IF ( cudtacttime .EQ. 0. ) THEN
            cudtacttime = curr_secs + cudt*60.
         END IF
      END IF
   END IF

!  Do we run through this scheme or not?

!    Test 1:  If this is the initial model time, then yes.
!                ITIMESTEP=1
!    Test 2:  If the user asked for the cumulus to be run every time step, then yes.
!                CUDT=0 or STEPCU=1
!    Test 3:  If not adaptive dt, and this is on the requested cumulus frequency, then yes.
!                MOD(ITIMESTEP,STEPCU)=0
!    Test 4:  If using adaptive dt and the current time is past the last requested activate cumulus time, then yes.
!                CURR_SECS >= CUDTACTTIME

!  If we do run through the scheme, we set the flag run_param to TRUE and we set the decided flag
!  to TRUE.  The decided flag says that one of these tests was able to say "yes", run the scheme.
!  We only proceed to other tests if the previous tests all have left decided as FALSE.

!  If we set run_param to TRUE and this is adaptive time stepping, we set the time to the next
!  cumulus run.

   decided = .FALSE.
   run_param = .FALSE.
   IF ( ( .NOT. decided ) .AND. &
        ( itimestep .EQ. 1 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( ( cudt .EQ. 0. ) .OR. ( stepcu .EQ. 1 ) ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( .NOT. doing_adapt_dt ) .AND. &
        ( MOD(itimestep,stepcu) .EQ. 0 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( doing_adapt_dt ) .AND. &
        ( curr_secs .GE. cudtacttime ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
      cudtacttime = curr_secs + cudt*60
   END IF

  !Leave the subroutine if it is not yet time to call the cumulus param
  if( .not. run_param ) return
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
           if(is_CAMMGMP_used) then
              cld8(1,kflip)  = cldfra_mp_all(i,k,j)
           else
              cld8(1,kflip)  = cldfra(i,k,j)
           endif
		   if (itimestep .eq. 1) cld8(1,kflip) =0.
           pdel8(1,kflip) = p8w(i,k,j) - p8w(i,k+1,j)
           pmid8(1,kflip) = p(i,k,j)
           qh8(1,kflip,1) = max( qv(i,k,j)/(1.+qv(i,k,j)), 1e-30 ) !values of 0 cause a crash in entropy
           if( present(qc) ) then
              ql8prm(1,kflip) = qc(i,k,j)/(1.+qv(i,k,j)) !Convert to moist mix ratio
           else
              ql8prm(1,kflip) = 0.
           end if
           if( present(qi) ) then
              qi8(1,kflip) = qi(i,k,j)/(1.+qv(i,k,j)) !Used in convtran, ditto for conversion
           else
              qi8(1,kflip) = 0.
           end if
           t8(1,kflip)    = t_phy(i,k,j)
           zm8(1,kflip)   = z(i,k,j) - ht(i,j) !Height above the ground at midlevels
           
           dp(1,kflip,lchnk) = 0.0_r8
           du(1,kflip,lchnk) = 0.0_r8
           ed(1,kflip,lchnk) = 0.0_r8
           eu(1,kflip,lchnk) = 0.0_r8
           md(1,kflip,lchnk) = 0.0_r8
           mu(1,kflip,lchnk) = 0.0_r8
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
             th(i,1,j), tsk(i,j), tke_pbl(i,:,j), ust(i,j),   &
             u_phy(i,1,j), v_phy(i,1,j), hfx(i,j), qfx(i,j), &
             tpert_camuwpbl(i,j), kte, &
             tpert(1))

        !Call the Zhang-McFarlane (1996) convection parameterization
        !PMA: pass in 0.5 x cudt (sec)
        !Modified by Balwinder.Singh@pnnl.gov: We are no longer using 0.5*dt

        !PMA: pass in 0.5 x cudt (sec)
        call zm_convr( lchnk, ncol, &
             t8, qh8, prec, jctop, jcbot, &
             pblh8, zm8, phis, zi8, qtnd, &
             stnd, pmid8, pint8, pdel8, &
             hcudts, mcon, cme, cape, &
             tpert, dlf, pflx, zdu, rprd, &
             mu(:,:,lchnk), md(:,:,lchnk),du(:,:,lchnk),eu(:,:,lchnk),ed(:,:,lchnk), &
             dp(:,:,lchnk), dsubcld(:,lchnk), jt(:,lchnk), maxg(:,lchnk), ideep(:,lchnk), &
             lengath(lchnk), ql8, rliq, landfrac )

        !Start mapping CAM output to WRF output variables. We follow the
        !same order as in CAM's zm_conv_tend to ease maintenance...
        do k=kts,kte
           kflip = kte-k+1
           dlf(1,kflip)    = max(min(1.e-6_r8,dlf(1,kflip)),0._r8)
           dlf_out(i,k,j)  = dlf(1,kflip)
        end do
        cape_out(i,j) = cape(1)
        rliq_out(i,j) = rliq(1)

        !Convert mass flux from reported mb/s to kg/m2/s
        mcon(:ncol,:pverp) = mcon(:ncol,:pverp) * 100._r8/gravit

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
           rqvcuten(i,k,j) = zmdq(i,k,j)*(1._r8 + qv(i,k,j))**2

           t8(1,kflip)    = t8(1,kflip) + zmdt(i,k,j)*cudts   !PMA
           qh8(1,kflip,1) = qh8(1,kflip,1) + zmdq(i,k,j)*cudts   !PMA
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
             rprd, cld8, cudts, &                            !PMA: This subroutine uses delt not 2xdelt, so pass in cudts
             prec, snow, ntprprd, ntsnprd , flxprec, flxsnow)
        evapcdp(:ncol,:pver) = qtnd(:ncol,:pver) !Added by Balwinder.Singh@pnnl.gov for assisting wetscavenging in WRF_CHEM package

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
           !BSINGH - Following quantities are to be stored at interfaces
           !cmfmc(i,k,j)    = mcon(1,kflip) !Set to deep value here, shallow added in UW scheme
           !cmfmcdzm(i,k,j) = mcon(1,kflip)
           preccdzm(i,j)   = prec(1)       !Rain rate from just deep
           precz(i,j)      = prec(1)       !Rain rate for total convection (just deep right now)
           pratec(i,j)     = prec(1)*1e3   !Rain rate used in WRF for accumulation (mm/s)
           raincv(i,j)     = pratec(i,j)*dt !Rain amount for dynamic time step returned back to WRF   !wig: fixed wrong timestep usage 3-Jun-2016
        end do

        !BSINGH - storing quantities at interfaces
        do k = kts,kte+1
           kflip             = kte - k + 2
           cmfmc(i,k,j)    = mcon(1,kflip) !Set to deep value here, shallow added in UW scheme
           cmfmcdzm(i,k,j) = mcon(1,kflip)
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
                             evapqzm(i,k,j)*(1. + qv(i,k,j))**2

           t8(1,kflip)    = t8(1,kflip) + evaptzm(i,k,j)*cudts    !PMA
           qh8(1,kflip,1) = qh8(1,kflip,1) + evapqzm(i,k,j)*cudts !PMA
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
             itimestep, wind_tends, pguall, pgdall, icwu, icwd, hcudts, stnd )  !PMA
        
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

           t8(1,kflip) = t8(1,kflip) + stnd(1,kflip)/cpair*cudts   !PMA
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
        fracis(1,:,1:3) = 1._r8 !all cloud liquid & ice 
#if ( WRF_CHEM != 1 )
        !BSINGH:02/01/2013: For WRF Physics ONLY runs, the liq # and ice # should
        !also be transpoted. Please note that liq # and ice # are transported in the 
        !zm_conv_tend_2 call for WRF_CHEM simulations (ONLY works for MAM aerosols)
        !currently.
        fracis(1,:,4:5) = 1._r8
#endif
        ncnst = 3               !number of constituents in cloud array (including vapor)
        fake_dpdry = 0._r8      !delta pres. for dry atmos. (0 since assuming moist mix ratios)
        do k=kts,kte
           kflip = kte-k+1

           cloud(1,kflip,1) = qh8(1,kflip,1)  !We can either use moist mix ratios, as is
           cloud(1,kflip,2) = ql8prm(1,kflip)    !done here, or else use dry mix ratios, send
           cloud(1,kflip,3) = qi8(1,kflip)    !in appropriate dpdry values, and return the
                                              !approp. response from cnst_get_type_byind
        end do

        call convtran (lchnk, &
             l_qt, cloud, ncnst,  mu(:,:,lchnk), md(:,:,lchnk), &
             du(:,:,lchnk), eu(:,:,lchnk), ed(:,:,lchnk), dp(:,:,lchnk), dsubcld(:,lchnk), &
             jt(:,lchnk), maxg(:,lchnk), ideep(:,lchnk), 1, lengath(lchnk), &
             itimestep, fracis, cloudtnd, fake_dpdry)

        !Parse output for convtran and increment tendencies
!PMA>
        do k=kts,kte
           kflip = kte-k+1
           esl(1,kflip)     = polysvp(t8(1,kflip),0)
           qvs(1,kflip)     = epsqs*esl(1,kflip)/(pmid8(1,kflip)-(1._r8-epsqs)*esl(1,kflip))
            if( t8(1,kflip) > 268.15_r8 ) then
              dum1 = 0.0_r8
            elseif( t8(1,kflip) < 238.15_r8 ) then
              dum1 = 1.0_r8
            else
              dum1 = ( 268.15_r8 - t8(1,kflip) ) / 30._r8
            endif
           qcten_det(1,kflip) = dlf(1,kflip) * ( 1._r8 - dum1 )
           qiten_det(1,kflip) = dlf(1,kflip) * dum1
           qcnten_det(1,kflip) = 3._r8 * (dlf(1,kflip)    * ( 1._r8 - dum1 ) ) / (4._r8*3.14159_r8*(8.e-6_r8**3)*997._r8)
           qinten_det(1,kflip) = 3._r8 * (dlf(1,kflip)    *  dum1 ) / (4._r8*3.14159_r8*(25.e-6_r8**3)*500._r8)
           qsten_det(1,kflip)  =  dlf(1,kflip) * dum1 * latice                    ! liquid to ice heating
        end do
        do k=kts,kte
           kflip = kte-k+1

           !For assisting wetscavenging in WRF_CHEM package -Balwinder.Singh@pnnl.gov
           evapcdp3d(i,k,j) = evapcdp(1,kflip) 
           icwmrdp3d(i,k,j) = ql8(1,kflip)     
           rprddp3d(i,k,j)  = rprd(1,kflip)    

           zmdice(i,k,j) = cloudtnd(1,kflip,3)+qiten_det(1,kflip)
           zmdliq(i,k,j) = cloudtnd(1,kflip,2)+qcten_det(1,kflip)
           rthcuten(i,k,j) = rthcuten(i,k,j) + &
                            qsten_det(1,kflip)/cpair/pi_phy(i,k,j)

           !Convert cloud tendencies from wet to dry mix ratios
           if( present(rqccuten) ) then
              rqccuten(i,k,j) = (cloudtnd(1,kflip,2)+qcten_det(1,kflip))*(1. + qv(i,k,j))
           end if
           if( present(rqicuten) ) then
              rqicuten(i,k,j) = (cloudtnd(1,kflip,3)+qiten_det(1,kflip))*(1. + qv(i,k,j))
           end if
           if( present(rqcncuten) ) then
              rqcncuten(i,k,j) = qcnten_det(1,kflip)*(1. + qv(i,k,j)) !BSINGH - Added the denominator following qiten_det
           end if
           if( present(rqincuten) ) then
              rqincuten(i,k,j) = qinten_det(1,kflip)*(1. + qv(i,k,j)) !BSINGH - Added the denominator following qiten_det
           end if
           !Variables required by zm_conv_tend_2 call
           dp3d(i,k,j) = dp(1,kflip,lchnk)
           du3d(i,k,j) = du(1,kflip,lchnk)
           ed3d(i,k,j) = ed(1,kflip,lchnk)
           eu3d(i,k,j) = eu(1,kflip,lchnk)
           md3d(i,k,j) = md(1,kflip,lchnk)
           mu3d(i,k,j) = mu(1,kflip,lchnk)

           dsubcld2d(i,j) = dsubcld(1,lchnk)
           ideep2d(i,j)   = ideep(1,lchnk)
           jt2d(i,j)      = jt(1,lchnk)
           maxg2d(i,j)    = maxg(1,lchnk)
           lengath2d(i,j) = lengath(lchnk)
        end do
        
     end do !i-loop
  end do    !j-loop
END SUBROUTINE camzm_driver


!------------------------------------------------------------------------
SUBROUTINE zm_conv_init(DT, DX, rucuten, rvcuten, rthcuten, rqvcuten,   &
                     rqccuten, rqicuten, rqcncuten, rqincuten,          &
                     p_qc, p_qi, p_qnc, p_qni, param_first_scalar,      &
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
  USE physconst, only: epsilo, latvap, latice, rh2o, cpair, tmelt
  USE module_cu_camzm, only: zm_convi, zmconv_readnl
  USE constituents, only: cnst_get_ind

  LOGICAL , INTENT(IN)           ::   restart
  INTEGER , INTENT(IN)           ::   ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte, &
                                      p_qc, p_qi, p_qnc, p_qni,     &
                                      param_first_scalar

   REAL ,    INTENT(IN)        :: DT, DX


  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: &
                                                           rucuten, &
                                                           rvcuten, &
                                                          rthcuten, &
                                                          rqvcuten, &
                                                          rqccuten, &
                                                          rqicuten, &
                                                          rqcncuten,&
                                                          rqincuten

  integer :: i, itf, j, jtf, k, ktf
  integer :: limcnv

  jtf = min(jte,jde-1)
  ktf = min(kte,kde-1)
  itf = min(ite,ide-1)

  call cnst_get_ind('CLDLIQ', ixcldliq)
  call cnst_get_ind('CLDICE', ixcldice)       
  call cnst_get_ind('NUMLIQ', ixnumliq)
  call cnst_get_ind('NUMICE', ixnumice)
  
!
! Initialize module_cam_support variables...
! This could be moved to a master "cam-init" subroutine once multiple CAM
! parameterizations are implemented in WRF. For now, it doesn't hurt to
! have these possibly initialized more than once since they are
! essentially constant.
!
  pver  = ktf-kts+1
  pverp = pver+1
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
! be OK across the domain. Also, remember that CAM levels are reversed in
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
  call zm_convi(DT, DX, limcnv, NO_DEEP_PBL_IN=.true.)

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
              if( p_qc  > param_first_scalar ) rqccuten(i,k,j)  = 0.
              if( p_qi  > param_first_scalar ) rqicuten(i,k,j)  = 0.
              if( p_qnc > param_first_scalar ) rqcncuten(i,k,j) = 0.
              if( p_qni > param_first_scalar ) rqincuten(i,k,j) = 0.
           enddo
        enddo
     enddo
  end if

END SUBROUTINE zm_conv_init


!------------------------------------------------------------------------
SUBROUTINE get_tpert(bl_pbl_physics, sf_sfclay_physics, dx, &
     mavail, kpbl, pblh, dzlowest, &
     psfc, qvlowest, t_phylowest, thlowest, tsk, tke_pbl, ust,   &
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
        do k=1,min(kpbl,kte+1) !BSINGH - Changed KTE to KTE+1 as tke_pbl is @ interfaces
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


!=========================================================================================

#if ( WRF_CHEM == 1 )
subroutine zm_conv_tend_2(itimestep, dt, p8w, fracis3d, dp3d, du3d, ed3d, eu3d, md3d, mu3d, &
     dsubcld2d, ideep2d, jt2d, maxg2d, lengath2d, moist, scalar, chem,                      &
     ids,ide, jds,jde, kds,kde, &
     ims,ime, jms,jme, kms,kme, &
     its,ite, jts,jte, kts,kte)

  use module_state_description,  only: num_moist, num_scalar, num_chem, param_first_scalar, &
       P_QV, P_QC, P_QI, P_QNC, P_QNI, CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ,                &
       CBMZ_CAM_MAM7_NOAQ,CBMZ_CAM_MAM7_AQ
  use module_data_cam_mam_asect, only: lptr_chem_to_q, factconv_chem_to_q
  use module_mp_cammgmp_driver,  only: physics_update, physics_ptend_init

  implicit none
  
  ! Arguments

  !intent-ins
  integer, intent(in) :: itimestep
  integer, intent(in) :: ids,ide, jds,jde, kds,kde
  integer, intent(in) :: ims,ime, jms,jme, kms,kme
  integer, intent(in) :: its,ite, jts,jte, kts,kte

  integer, dimension(ims:ime,jms:jme), intent(in) :: ideep2d, jt2d, maxg2d, lengath2d
  
  real, intent(in) :: dt

  real, dimension(ims:ime,jms:jme), intent(in) :: dsubcld2d

  real, dimension(ims:ime,kms:kme,jms:jme), intent(in) :: p8w, dp3d, du3d, ed3d, eu3d, md3d
  real, dimension(ims:ime,kms:kme,jms:jme), intent(in) :: mu3d 

  real, dimension(ims:ime,kms:kme,jms:jme,pcnst), intent(in) :: fracis3d
  
  !intent-inouts
  real, dimension(ims:ime,kms:kme,jms:jme,num_moist),  intent(inout) :: moist
  real, dimension(ims:ime,kms:kme,jms:jme,num_scalar), intent(inout) :: scalar
  real, dimension(ims:ime,kms:kme,jms:jme,num_chem),   intent(inout) :: chem
  
  
  ! Local variables
  character(len=1000) :: msg
  character*24 :: ptend_name            !ptend%name in CAM5 - used in parameterization
  logical      :: ptend_ls              !ptend%ls   in CAM5 - used for calling physics_update subroutine
  logical      :: ptend_lq(pcnst)       !ptend%lq   in CAM5

  integer :: iw, jw, kw, kflip, l, l2
  integer :: i, lchnk, istat
  integer :: nstep

  real(r8) :: delta_p, multFrc, dtime 
  real(r8) :: dpdry(pcols,kte)
  real(r8) :: state_pdel(pcols,kte)                ! Pressure difference (Pa) 
  real(r8) :: state_pdeldry(pcols,kte)             ! dry pressure difference (1/Pa)
  real(r8) :: state_q(pcols,kte,pcnst)
  real(r8) :: state_s(pcols,kte)
  real(r8) :: ptend_s(pcols,kte)                   !Dummy arguments for physics_update call
  real(r8) :: ptend_q(pcols,kte,pcnst)
  
  real(r8) :: dp(pcols, kte, (ime-ims+1)*(jme-jms+1)) !layer pres. thickness between interfaces (mb)
  real(r8) :: du(pcols, kte, (ime-ims+1)*(jme-jms+1)) 
  real(r8) :: ed(pcols, kte, (ime-ims+1)*(jme-jms+1))
  real(r8) :: eu(pcols, kte, (ime-ims+1)*(jme-jms+1)) 
  real(r8) :: md(pcols, kte, (ime-ims+1)*(jme-jms+1))
  real(r8) :: mu(pcols, kte, (ime-ims+1)*(jme-jms+1))

  real(r8) :: dsubcld(pcols, (ime-ims+1)*(jme-jms+1)) ! layer pres. thickness between LCL and maxi (mb)

  integer :: ideep(pcols, (ime-ims+1)*(jme-jms+1)) ! holds position of gathered points
  integer :: jt(pcols, (ime-ims+1)*(jme-jms+1))    ! top-level index of deep cumulus convection
  integer :: maxg(pcols, (ime-ims+1)*(jme-jms+1))  ! gathered values of maxi
                               
  integer :: lengath((ime-ims+1)*(jme-jms+1))    ! number of gathered points

  
  ! physics buffer fields 
  integer itim, ifld
  real(r8), dimension(pcols,kte,pcnst) :: fracis  ! fraction of transported species that are insoluble

  !Time step is stored in the (r8) format in dtime
  dtime = dt

  ! Map variables for CAM subrouine call
  ! Loop over the points in the tile and treat them each as a CAM chunk.
  !

  !BSINGH:02/01/2013: Sanity check for Non-MAM simulations
  !This subroutine will not be called for chem_opt=0, so we dont need to worry about chem_opt = 0
  if(.NOT.cam_mam_aerosols) then
     write(msg,*)'CAMZM DRIVER - zm_conv_tend_2 is valid for only MAM aerosols ', &
          '(chem_opts:',CBMZ_CAM_MAM3_NOAQ,CBMZ_CAM_MAM3_AQ,CBMZ_CAM_MAM7_NOAQ,CBMZ_CAM_MAM7_AQ ,')'
     call wrf_error_fatal( msg )
  endif
  
  do jw = jts,jte
     do iw = its,ite
        lchnk = (jw-jts)*(ite-its+1) + (iw-its+1) !1-D index location from the 2-D tile
        
        !Flip variables on the layer middles
        do kw = kts,kte
           kflip = kte-kw+1

           !Following three formulas are obtained from ported CAM's ZM cumulus scheme
           !Values of 0 cause a crash in entropy
           multFrc              = 1._r8/(1._r8 + moist(iw,kw,jw,P_QV))
           state_q(1,kflip,1)   = moist(iw,kw,jw,P_QV)*multFrc      !Specific humidity
           state_q(1,kflip,ixcldliq) = moist(iw,kw,jw,P_QC)*multFrc !Convert to moist mix ratio-cloud liquid
           state_q(1,kflip,ixcldice) = moist(iw,kw,jw,P_QI)*multFrc !cloud ice                              
           state_q(1,kflip,ixnumliq) = scalar(iw,kw,jw,P_QNC)*multFrc
           state_q(1,kflip,ixnumice) = scalar(iw,kw,jw,P_QNI)*multFrc
           do l = 1, 5
              fracis(1,kflip,l)      = fracis3d(iw,kw,jw,l)
           enddo
           !populate state_q and qqcw arrays
           !Following Do-Loop is obtained from chem/module_cam_mam_aerchem_driver.F 
           do l = param_first_scalar, num_chem
              l2 = lptr_chem_to_q(l)
              if ((l2 >= 1) .and. (l2 <= pcnst)) then
                 fracis(1,kflip,l2)  = fracis3d(iw,kw,jw,l2)
                 state_q(1,kflip,l2) = chem(iw,kw,jw,l)*factconv_chem_to_q(l)
              end if
           end do ! l
           
           delta_p                 = p8w(iw,kw,jw) - p8w(iw,kw+1,jw) !Change in pressure (Pa) 
           state_pdel(  1,kflip)   = delta_p
           state_pdeldry(1,kflip)  = state_pdel(1,kflip)*(1._r8-state_q(1,kflip,1)) 
           !Variables required by zm_conv_tend_2 call
           dp(1,kflip,lchnk) = dp3d(iw,kw,jw) 
           du(1,kflip,lchnk) = du3d(iw,kw,jw)  
           ed(1,kflip,lchnk) = ed3d(iw,kw,jw)  
           eu(1,kflip,lchnk) = eu3d(iw,kw,jw)  
           md(1,kflip,lchnk) = md3d(iw,kw,jw)  
           mu(1,kflip,lchnk) = mu3d(iw,kw,jw)  
           
           dsubcld(1,lchnk)  = dsubcld2d(iw,jw)  
           ideep(1,lchnk)    = ideep2d(iw,jw)    
           jt(1,lchnk)       = jt2d(iw,jw)       
           maxg(1,lchnk)     = maxg2d(iw,jw)     
           lengath(lchnk)    = lengath2d(iw,jw) 
        enddo
        
        !
        ! Initialize
        !
        call physics_ptend_init(ptend_name,ptend_q,ptend_s,ptend_lq,ptend_ls,pcnst)  !! Initialize local ptend type 
                
        !
        ! Transport all constituents except cloud water and ice
        !
        
        nstep = itimestep
        
        !
        !     Convective transport of all trace species except cloud liquid 
        !     and cloud ice done here because we need to do the scavenging first
        !     to determine the interstitial fraction.
        !
        ptend_name  = 'convtran2'
        ptend_lq(:) = .true.
        ptend_lq(1)        = .false.
        ptend_lq(ixcldice) = .false.
        ptend_lq(ixcldliq) = .false.
        
        ! initialize dpdry for call to convtran
        ! it is used for tracers of dry mixing ratio type
        dpdry = 0._r8
        do i = 1,lengath(lchnk)
           dpdry(i,:) = state_pdeldry(ideep(i,lchnk),:)/100._r8
        end do
        
        call convtran (lchnk,                                        &
             ptend_lq,state_q, pcnst,  mu(:,:,lchnk), md(:,:,lchnk),   &
             du(:,:,lchnk), eu(:,:,lchnk), ed(:,:,lchnk), dp(:,:,lchnk), dsubcld(:,lchnk),  &
             jt(:,lchnk),maxg(:,lchnk),ideep(:,lchnk), 1, lengath(lchnk),  &
             nstep,   fracis,  ptend_q, dpdry)

        !Update chem array and state constiuents
        !populate state_s, ptend_s, ptend_ls with dummy values (zeros) for physics update call
        state_s(:,:) = 0.0_r8
        ptend_s(:,:) = 0.0_r8
        ptend_ls     = .FALSE.
        
        call physics_update(lchnk,dtime,state_q,ptend_q,state_s,ptend_s,ptend_name,ptend_lq,ptend_ls,pcnst)

        !Post processing of the output to update WRF arrays
          do kw=kts,kte
             
             kflip = kte-kw+1

             !Following equation are derived following UWPBL and CAMZM schemes
             moist(iw,kw,jw,P_QV)   = state_q(1,kflip,1) / (1.0_r8 - state_q(1,kflip,1)) 
             multFrc                = 1._r8 + moist(iw,kw,jw,P_QV)
             
             moist(iw,kw,jw,P_QC)   = state_q(1,kflip,2) * multFrc
             moist(iw,kw,jw,P_QI)   = state_q(1,kflip,3) * multFrc 
             
             scalar(iw,kw,jw,P_QNC) = state_q(1,kflip,4) * multFrc 
             scalar(iw,kw,jw,P_QNI) = state_q(1,kflip,5) * multFrc
             
             do l = param_first_scalar, num_chem
                l2 = lptr_chem_to_q(l)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   chem(iw,kw,jw,l) = state_q(1,kflip,l2)/factconv_chem_to_q(l)
                end if
             end do ! l
          end do



     enddo
  enddo
end subroutine zm_conv_tend_2
#endif
END MODULE module_cu_camzm_driver
