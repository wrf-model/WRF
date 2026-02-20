#if (NMM_CORE == 1)
MODULE module_diag_pld
CONTAINS
   SUBROUTINE diag_pld_stub
   END SUBROUTINE diag_pld_stub
END MODULE module_diag_pld
#else
!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_diag_pld
CONTAINS

   SUBROUTINE pld ( u,v,w,t,qv,zp,zb,pp,pb,p,pw,                    &
                    msfux,msfuy,msfvx,msfvy,msftx,msfty,            &
                    f,e,                                            &
                    use_tot_or_hyd_p,extrap_below_grnd,missing,     &  
                    num_press_levels,max_press_levels,press_levels, &
                    p_pl,u_pl,v_pl,t_pl,rh_pl,ght_pl,s_pl,td_pl,    &
                    q_pl,                                           &
#ifdef CORDEXDIAG
                    ! L. Fita, CIMA. March 2018
                    !! Added for CORDEX output
                    sina, cosa,                                     &
                    psfc, topo, alt, p_top,                         &
                    iqc, iqr, iqs, iqi, iqg, iqh,                   &
                    hus_pl, w_pl, uer_pl, ver_pl, ws_pl,            &
                    qc_pl, qr_pl, qs_pl, qi_pl, qg_pl, qh_pl,       &  ! v2. Adding other species
                    qv_pl_mc, qc_pl_mc, qr_pl_mc, qs_pl_mc,         &  ! v2. Adding mass-conservative 
                    qi_pl_mc, qg_pl_mc, qh_pl_mc,                   &  
#endif
                    ids,ide, jds,jde, kds,kde,                      &
                    ims,ime, jms,jme, kms,kme,                      &
                    its,ite, jts,jte, kts,kte                       )
   
      USE module_model_constants
#ifdef CORDEXDIAG
! It is not working
!      USE module_diagvar_cordex, ONLY: massconserv_vinterp1D, presbars_to_hgtbars
#endif   
      IMPLICIT NONE
   
   
      !  Input variables
   
      INTEGER, INTENT(IN   )                                          :: ids,ide, jds,jde, kds,kde, &
                                                                         ims,ime, jms,jme, kms,kme, &
                                                                         its,ite, jts,jte, kts,kte
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: msfux,msfuy,msfvx,msfvy,msftx,msfty, &
                                                                         f,e
      INTEGER, INTENT(IN   )                                          :: use_tot_or_hyd_p
      INTEGER, INTENT(IN   )                                          :: extrap_below_grnd
      REAL   , INTENT(IN   )                                          :: missing
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: u,v,w,t,qv,zp,zb,pp,pb,p,pw
      INTEGER, INTENT(IN   )                                          :: num_press_levels, max_press_levels
      REAL   , INTENT(IN   ) , DIMENSION(max_press_levels)            :: press_levels
#ifdef CORDEXDIAG
      REAL                                                            :: p_top
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: sina, cosa, topo, psfc
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: alt
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: iqc, iqr, iqs, iqi, iqg, iqh
#endif   
      !  Output variables
   
      REAL   , INTENT(  OUT) ,  DIMENSION(num_press_levels)                     :: p_pl
      REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_press_levels , jms:jme) :: u_pl,v_pl,t_pl,rh_pl,ght_pl,s_pl,td_pl,q_pl
#ifdef CORDEXDIAG
      REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_press_levels , jms:jme) :: hus_pl, w_pl,      &
        uer_pl, ver_pl, ws_pl
      REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_press_levels , jms:jme) :: qc_pl, qr_pl,      &
        qs_pl, qi_pl, qg_pl, qh_pl
      REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_press_levels , jms:jme) :: qv_pl_mc,          &
        qc_pl_mc, qr_pl_mc, qs_pl_mc, qi_pl_mc, qg_pl_mc, qh_pl_mc
#endif
   
      !  Local variables
   
      REAL, PARAMETER :: eps = 0.622, t_kelvin = svpt0 , s1 = 243.5, s2 = svp2 , s3 = svp1*10., s4 = 611.0, s5 = 5418.12
      REAL, PARAMETER :: zshul=75., tvshul=290.66
   
      INTEGER :: i, j, ke, kp, ke_h, ke_f
      REAL    :: pu, pd, pm , &
                 tu, td     , &
                 su, sd     , &
                 uu, ud     , &
                 vu, vd     , &
                 zu, zd     , &
                 qu, qd     , &
                 eu, ed, em , &
                 du, dd
#ifdef CORDEXDIAG
      INTEGER                                            :: dzstag
      REAL                                               :: wu, wd
      REAL, DIMENSION(kms:kme)                           :: press, zg, massv, rho, ohgts, obthgts
      INTEGER                                            :: fstrightlevel, Nright
      REAL, DIMENSION(num_press_levels)                  :: hgts, rightdespressv
      REAL, DIMENSION(num_press_levels,2)                :: bthgts2
      REAL, DIMENSION(num_press_levels)                  :: rhodes, valsdes
      REAL, DIMENSION(kms:kme)                           :: bthgts

#endif
      REAL    :: es, qs
      REAL    :: part, gammas, tvu, tvd
   
      !  Silly, but transfer the small namelist.input array into the grid structure for output purposes.
   
      DO kp = 1 , num_press_levels
         p_pl(kp) = press_levels(kp)
      END DO
   
      !  Initialize pressure level data to un-initialized
   
      DO j = jts , jte
         DO kp = 1 , num_press_levels
            DO i = its , ite
               u_pl  (i,kp,j) = missing
               v_pl  (i,kp,j) = missing
               t_pl  (i,kp,j) = missing
               rh_pl (i,kp,j) = missing
               ght_pl(i,kp,j) = missing
               s_pl  (i,kp,j) = missing
               td_pl (i,kp,j) = missing
               q_pl  (i,kp,j) = missing
#ifdef CORDEXDIAG
               ! L. Fita. CIMA. March 2018
               hus_pl  (i,kp,j) = missing
               w_pl  (i,kp,j) = missing
               uer_pl  (i,kp,j) = missing
               ver_pl  (i,kp,j) = missing
               ws_pl  (i,kp,j) = missing
               qc_pl  (i,kp,j) = missing
               qr_pl  (i,kp,j) = missing
               qs_pl  (i,kp,j) = missing
               qi_pl  (i,kp,j) = missing
               qg_pl  (i,kp,j) = missing
               qh_pl  (i,kp,j) = missing
               qv_pl_mc  (i,kp,j) = missing
               qc_pl_mc  (i,kp,j) = missing
               qr_pl_mc  (i,kp,j) = missing
               qs_pl_mc  (i,kp,j) = missing
               qi_pl_mc  (i,kp,j) = missing
               qg_pl_mc  (i,kp,j) = missing
               qh_pl_mc  (i,kp,j) = missing
#endif
            END DO
         END DO
      END DO
   
      !  Loop over each i,j location
   
      j_loop : DO j = jts , MIN(jte,jde-1)
         i_loop : DO i = its , MIN(ite,ide-1)
   
            !  For each i,j location, loop over the selected
            !  pressure levels to find
   
            ke_h = kts
            ke_f = kts
            kp_loop : DO kp = 1 , num_press_levels
   
               !  For this particular i,j and pressure level, find the
               !  eta levels that surround this point on half-levels.
   
               ke_loop_half : DO ke = ke_h , kte-2
   
                  IF      ( use_tot_or_hyd_p .EQ. 1 ) THEN     !  total pressure
                     pu = pp(i,ke+1,j)+pb(i,ke+1,j)
                     pd = pp(i,ke  ,j)+pb(i,ke  ,j)
                  ELSE IF ( use_tot_or_hyd_p .EQ. 2 ) THEN     !  hydrostatic pressure
                     pu = p(i,ke+1,j)
                     pd = p(i,ke  ,j)
                  END IF
                  pm = p_pl(kp)
                 
                  !  Added option to extrapolate below ground - GAC (AFWA)

                  IF ( ( extrap_below_grnd .EQ. 2 ) .AND.  &
                     ( ke .EQ. ke_h ) .AND. ( pm .GT. pd )) THEN

                     !  Requested pressure level is below ground.
                     !  Extrapolate adiabatically if requested in namelist.

                     !  Methodology derived from Unified Post Processor (UPP).
                     !  Simply conserve first level U, V, and RH below ground.
                     !  Assume adiabatic lapse rate of gamma = 6.5 K/km
                     !  below ground, using Shuell correction to gamma
                     !  ("gammas") to find geopotential height, which is
                     !  computed by hydrostatically integrating mean isobaric
                     !  virtual temperature downward from the model surface.
                     !  Temperature is found by reducing adiabatically
                     !  from the first level temperature.
                     !  Sources:
                     !    Chuang et al, NCEP's WRF Post Processor and
                     !      Verification Systems, MM5 Workshop Session 7, 2004.
                     !    Unipost source code: MDL2P.f

                     !  Z, T, Q, Tv at first half-eta level

                     zu = 0.5 * ( zp(i,ke  ,j) + zb(i,ke  ,j) + &
                                  zp(i,ke+1,j) + zb(i,ke+1,j) ) / g
                     tu = ( t(i,ke,j) + t0 ) * ( pd / p1000mb ) ** rcp
                     qu = MAX(qv(i,ke,j),0.)
                     tvu = tu * ( 1. + 0.608 * qu )

                     !  1. Geopotential height (m)

                     IF ( zu .GT. zshul ) THEN
                        tvd = tvu + zu * 6.5E-3
                        IF ( tvd .GT. tvshul ) THEN
                          IF ( tvu .GT. tvshul) THEN
                            tvd = tvshul - 5.E-3 * ( tvu - tvshul ) ** 2
                          ELSE
                            tvd = tvshul
                          ENDIF
                        ENDIF
                        gammas = ( tvu - tvd ) / zu
                     ELSE
                        gammas = 0.
                     ENDIF
                     part = ( r_d / g ) * ( ALOG (pm) - ALOG (pd) )
                     ght_pl(i,kp,j) = zu - tvu * part / &
                                      ( 1. + 0.5 * gammas * part )
 
                     !  2. Temperature (K)

                     t_pl(i,kp,j) = tu + ( zu - ght_pl(i,kp,j) ) * 6.5E-3

                     !  3. Speed (m s-1)

                     s_pl(i,kp,j) = 0.5 * SQRT ( ( u(i,ke  ,j)+ &
                                   u(i+1,ke  ,j) )**2 +         &
                                   ( v(i,ke  ,j) + v(i,ke  ,j+1) )**2 )

                     !  4. U and V (m s-1)

                     u_pl(i,kp,j) = 0.5 * ( u(i,ke  ,j) + u(i+1,ke  ,j) )
                     v_pl(i,kp,j) = 0.5 * ( v(i,ke  ,j) + v(i,ke  ,j+1) )
                     
                     !  5. Relative humidity (%)

                     es = s4 * exp(s5 * (1.0 / 273.0 - 1.0 / tu) )
                     qs = eps * es / (pd - es)
                     rh_pl(i,kp,j)   = MAX(qv(i,ke,j),0.) / qs * 100.

                     !  6. Mixing ratio (kg/kg)

                     es = s4 * exp(s5 * (1.0 / 273.0 - 1.0 / t_pl(i,kp,j)))
                     qs = eps * es / (pm - es)
                     q_pl(i,kp,j)   = rh_pl(i,kp,j) * qs / 100.
                      
                     !  7. Dewpoint (K) - Use Bolton's approximation
   
                     ed = q_pl(i,kp,j) * pm * 0.01 / ( eps + q_pl(i,kp,j) )
                     ed = max(ed, 0.001)   ! water vapor pressure in mb.
                     td_pl(i,kp,j) = t_kelvin + (s1 / ((s2 / log(ed/s3)) - 1.0))

#ifdef CORDEXDIAG
                     !  8. Specific humidty (1)

                     hus_pl(i,kp,j) = q_pl(i,kp,j) / (1. + q_pl(i,kp,j))

                     !  9. Earth-rotated U and V (m s-1)

                     uer_pl(i,kp,j) = u_pl(i,kp,j)*cosa(i,j) - v_pl(i,kp,j)*sina(i,j)
                     ver_pl(i,kp,j) = u_pl(i,kp,j)*sina(i,j) + v_pl(i,kp,j)*cosa(i,j)

                     !  10. Wind speed (m s-1)

                     ws_pl(i,kp,j) = SQRT(u_pl(i,kp,j)*u_pl(i,kp,j) + v_pl(i,kp,j)*v_pl(i,kp,j))

#endif

                     EXIT ke_loop_half
                  ELSEIF ( ( pd .GE. pm ) .AND. &
                       ( pu .LT. pm ) ) THEN
   
                     !  Found trapping pressure: up, middle, down.
                     !  We are doing first order interpolation.  
                     !  Now we just put in a list of diagnostics for this level.
   
                     !  1. Temperature (K)
   
                     tu = (t(i,ke+1,j)+t0)*(pu/p1000mb)**rcp
                     td = (t(i,ke  ,j)+t0)*(pd/p1000mb)**rcp
                     t_pl(i,kp,j) = ( tu * (pm-pd) + td * (pu-pm) ) / (pu-pd)
   
                     !  2. Speed (m s-1)
   
                     su = 0.5 * SQRT ( ( u(i,ke+1,j)+u(i+1,ke+1,j) )**2 + &
                                       ( v(i,ke+1,j)+v(i,ke+1,j+1) )**2 ) 
                     sd = 0.5 * SQRT ( ( u(i,ke  ,j)+u(i+1,ke  ,j) )**2 + &
                                       ( v(i,ke  ,j)+v(i,ke  ,j+1) )**2 ) 
                     s_pl(i,kp,j) = ( su * (pm-pd) + sd * (pu-pm) ) / (pu-pd)
   
                     !  3. U and V (m s-1)
   
                     uu = 0.5 * ( u(i,ke+1,j)+u(i+1,ke+1,j) )
                     ud = 0.5 * ( u(i,ke  ,j)+u(i+1,ke  ,j) )
                     u_pl(i,kp,j) = ( uu * (pm-pd) + ud * (pu-pm) ) / (pu-pd)
   
                     vu = 0.5 * ( v(i,ke+1,j)+v(i,ke+1,j+1) )
                     vd = 0.5 * ( v(i,ke  ,j)+v(i,ke  ,j+1) )
                     v_pl(i,kp,j) = ( vu * (pm-pd) + vd * (pu-pm) ) / (pu-pd)
   
                     !  4. Mixing ratio (kg/kg)

                     qu = MAX(qv(i,ke+1,j),0.)
                     qd = MAX(qv(i,ke  ,j),0.)
                     q_pl(i,kp,j) = ( qu * (pm-pd) + qd * (pu-pm) ) / (pu-pd)

                     !  5. Dewpoint (K) - Use Bolton's approximation
   
                     eu = qu * pu * 0.01 / ( eps + qu ) ! water vapor press (mb)
                     ed = qd * pd * 0.01 / ( eps + qd ) ! water vapor press (mb)
                     eu = max(eu, 0.001)
                     ed = max(ed, 0.001)
   
                     du = t_kelvin + ( s1 / ((s2 / log(eu/s3)) - 1.0) )
                     dd = t_kelvin + ( s1 / ((s2 / log(ed/s3)) - 1.0) )
                     td_pl(i,kp,j) = ( du * (pm-pd) + dd * (pu-pm) ) / (pu-pd)
   

                     !  6. Relative humidity (%)
   
                     es = s4 * exp(s5 * (1.0 / 273.0 - 1.0 / t_pl(i,kp,j)))
                     qs = eps * es / (pm - es)
                     rh_pl(i,kp,j)   = q_pl(i,kp,j) / qs * 100.
   
                     !em = qm * pm * 0.01 / ( eps + qm )                                       ! water vapor pressure at the level.
                     !es = s3 * exp( s2 * (t_pl(i,kp,j) - t_kelvin)/(t_pl(i,kp,j) - s4) )      ! sat vapor pressure over liquid water in mb.
                     !rh_pl(i,kp,j) = 100. * em * ( pm * 0.01 - es ) / ( es * ( pm * 0.01 - em ) )
                     
                     
#ifdef CORDEXDIAG
                     !  8. Specific humidty (1)

                     hus_pl(i,kp,j) = q_pl(i,kp,j) / (1. + q_pl(i,kp,j))

                     !  9. Earth-rotated U and V (m s-1)

                     uer_pl(i,kp,j) = u_pl(i,kp,j)*cosa(i,j) - v_pl(i,kp,j)*sina(i,j)
                     ver_pl(i,kp,j) = u_pl(i,kp,j)*sina(i,j) + v_pl(i,kp,j)*cosa(i,j)

                     !  10. Wind speed (m s-1)

                     ws_pl(i,kp,j) = SQRT(u_pl(i,kp,j)*u_pl(i,kp,j) + v_pl(i,kp,j)*v_pl(i,kp,j))

                     !  11. Other water species (kg/kg)

                     ! cloud
                     qu = MAX(iqc(i,ke+1,j),0.)
                     qd = MAX(iqc(i,ke  ,j),0.)
                     qc_pl(i,kp,j) = ( qu * (pm-pd) + qd * (pu-pm) ) / (pu-pd)

                     ! rain
                     qu = MAX(iqr(i,ke+1,j),0.)
                     qd = MAX(iqr(i,ke  ,j),0.)
                     qr_pl(i,kp,j) = ( qu * (pm-pd) + qd * (pu-pm) ) / (pu-pd)

                     ! snow
                     qu = MAX(iqs(i,ke+1,j),0.)
                     qd = MAX(iqs(i,ke  ,j),0.)
                     qs_pl(i,kp,j) = ( qu * (pm-pd) + qd * (pu-pm) ) / (pu-pd)

                     ! ice
                     qu = MAX(iqi(i,ke+1,j),0.)
                     qd = MAX(iqi(i,ke  ,j),0.)
                     qi_pl(i,kp,j) = ( qu * (pm-pd) + qd * (pu-pm) ) / (pu-pd)

                     ! graupel
                     qu = MAX(iqg(i,ke+1,j),0.)
                     qd = MAX(iqg(i,ke  ,j),0.)
                     qg_pl(i,kp,j) = ( qu * (pm-pd) + qd * (pu-pm) ) / (pu-pd)

                     ! hail
                     qu = MAX(iqh(i,ke+1,j),0.)
                     qd = MAX(iqh(i,ke  ,j),0.)
                     qh_pl(i,kp,j) = ( qu * (pm-pd) + qd * (pu-pm) ) / (pu-pd)

#endif

                     ke_h = ke
                     EXIT ke_loop_half
                  END IF
               END DO ke_loop_half
   
               ke_loop_full : DO ke = ke_f , kte-1

                  IF ( ( pw(i,ke  ,j) .GE. p_pl(kp) ) .AND. &
                       ( pw(i,ke+1,j) .LT. p_pl(kp) ) ) THEN
   
                     !  Found trapping pressure: up, middle, down.
                     !  We are doing first order interpolation.
   
                     pu = LOG(pw(i,ke+1,j))
                     pm = LOG(p_pl(kp))
                     pd = LOG(pw(i,ke  ,j))
   
                     !  Now we just put in a list of diagnostics for this level.
   
                     !  1. Geopotential height (m)
   
                     zu = ( zp(i,ke+1,j)+zb(i,ke+1,j) ) / g
                     zd = ( zp(i,ke  ,j)+zb(i,ke  ,j) ) / g
                     ght_pl(i,kp,j) = ( zu * (pm-pd) + zd * (pu-pm) ) / (pu-pd)
   
   
#ifdef CORDEXDIAG
                     ! L. Fita. CIMA. March 2018
                     !  2. Vertical wind speed (ms-1)
   
                     wu = w(i,ke+1,j)
                     wd = w(i,ke  ,j)
                     w_pl(i,kp,j) = ( wu * (pm-pd) + wd * (pu-pm) ) / (pu-pd)
#endif

                     ke_f = ke
                     EXIT ke_loop_full
                  END IF
               END DO ke_loop_full
   
            END DO kp_loop

#ifdef CORDEXDIAG
            ! Mass-conservative vertical interpolation of water species

            ! Transforming p-level bars to heights
            dzstag =  kte-1 - kts
            zg(1:dzstag) = zb(i,kts:kte-1,j) + zp(i,kts:kte-1,j)
            press(1:dzstag-1) = pb(i,kts:kte-2,j) + pp(i,kts:kte-2,j)
            CALL presbars_to_hgtbars(topo(i,j), dzstag, zg(1:dzstag), dzstag-1, press(1:dzstag-1),    &
              psfc(i,j), p_top, num_press_levels, p_pl, fstrightlevel, Nright,                        &
              rightdespressv, hgts, bthgts2)
         
            bthgts(1) = bthgts2(1,1)
            bthgts(2:Nright+1) = bthgts2(1:Nright,2)

            obthgts(1:dzstag) = zg(1:dzstag) / g
            ohgts(1:dzstag-1) = 0.5*(obthgts(1:dzstag-1)+obthgts(2:dzstag))

            ! density
            rho(1:dzstag-1) = 1. / alt(i,kts:kte-2,j)
            CALL massconserv_vinterp1D(dzstag-1, ohgts(1:dzstag-1), dzstag, obthgts(1:dzstag),        &
              rho(1:dzstag-1), Nright, rightdespressv(1:Nright), Nright+1,                            &
              bthgts(1:num_press_levels+1), rhodes(1:Nright))

            ! qv: Water vapour 
            massv(1:dzstag-1) = qv(i,kts:kte-2,j) * rho(1:dzstag-1)
            CALL massconserv_vinterp1D(dzstag-1, ohgts(1:dzstag-1), dzstag, obthgts(1:dzstag),        &
              massv(1:dzstag-1), Nright, rightdespressv, Nright+1, bthgts(1:num_press_levels+1),      &
              valsdes(1:Nright))
            ! There is no way to perform extrapolatoin below ground, so, fill since the first right level
            qv_pl_mc(i,fstrightlevel:fstrightlevel+Nright-1,j) = valsdes(1:Nright) / rhodes(1:Nright)

            ! qc: cloud
            massv(1:dzstag-1) = iqc(i,kts:kte-2,j) * rho(1:dzstag-1)
            CALL massconserv_vinterp1D(dzstag-1, ohgts(1:dzstag-1), dzstag, obthgts(1:dzstag),        &
              massv(1:dzstag-1), Nright, rightdespressv(1:Nright), Nright+1,                          &
              bthgts(1:num_press_levels+1), valsdes(1:Nright))
            qc_pl_mc(i,fstrightlevel:fstrightlevel+Nright-1,j) = valsdes(1:Nright) / rhodes(1:Nright)

            ! qr: rain
            massv(1:dzstag-1) = iqr(i,kts:kte-2,j) * rho(1:dzstag-1)
            CALL massconserv_vinterp1D(dzstag-1, ohgts(1:dzstag-1), dzstag, obthgts(1:dzstag),        &
              massv(1:dzstag-1), Nright, rightdespressv(1:Nright), Nright+1,                          &
              bthgts(1:num_press_levels+1), valsdes(1:Nright))
            qr_pl_mc(i,fstrightlevel:fstrightlevel+Nright-1,j) = valsdes(1:Nright) / rhodes(1:Nright)

            ! qs: snow
            massv(1:dzstag-1) = iqs(i,kts:kte-2,j) * rho(1:dzstag-1)
            CALL massconserv_vinterp1D(dzstag-1, ohgts(1:dzstag-1), dzstag, obthgts(1:dzstag),        &
              massv(1:dzstag-1), Nright, rightdespressv(1:Nright), Nright+1,                          &
              bthgts(1:num_press_levels+1), valsdes(1:Nright))
            qs_pl_mc(i,fstrightlevel:fstrightlevel+Nright-1,j) = valsdes(1:Nright) / rhodes(1:Nright)

            ! qi: ice
            massv(1:dzstag-1) = iqi(i,kts:kte-2,j) * rho(1:dzstag-1)
            CALL massconserv_vinterp1D(dzstag-1, ohgts(1:dzstag-1), dzstag, obthgts(1:dzstag),        &
              massv(1:dzstag-1), Nright, rightdespressv(1:Nright), Nright+1,                          &
              bthgts(1:num_press_levels+1), valsdes(1:Nright))
            qi_pl_mc(i,fstrightlevel:fstrightlevel+Nright-1,j) = valsdes(1:Nright) / rhodes(1:Nright)

            ! qg: graupel
            massv(1:dzstag-1) = iqg(i,kts:kte-2,j) * rho(1:dzstag-1)
            CALL massconserv_vinterp1D(dzstag-1, ohgts(1:dzstag-1), dzstag, obthgts(1:dzstag),        &
              massv(1:dzstag-1), Nright, rightdespressv(1:Nright), Nright+1,                          &
              bthgts(1:num_press_levels+1), valsdes(1:Nright))
            qg_pl_mc(i,fstrightlevel:fstrightlevel+Nright-1,j) = valsdes(1:Nright) / rhodes(1:Nright)

            ! qh: hail
            massv(1:dzstag-1) = iqh(i,kts:kte-2,j) * rho(1:dzstag-1)
            CALL massconserv_vinterp1D(dzstag-1, ohgts(1:dzstag-1), dzstag, obthgts(1:dzstag),        &
              massv(1:dzstag-1), Nright, rightdespressv(1:Nright), Nright+1,                          &
              bthgts(1:num_press_levels+1), valsdes(1:Nright))
            qh_pl_mc(i,fstrightlevel:fstrightlevel+Nright-1,j) = valsdes(1:Nright) / rhodes(1:Nright)

#endif

         END DO i_loop
      END DO j_loop

   END SUBROUTINE pld

#ifdef CORDEXDIAG
! L. Fita, February 2023
! this should not be here, but it seems that the order of compilation does not allow to do it in another
!   way.

  SUBROUTINE massconserv_vinterp1D(dorig, levorig, dorig1, btlevorig, valsorig, ddes, levdes,         &
    ddes1, btlevdes, valsdes)
  ! Subrotine to mass-conservative vertically interpolate a variable 1D
  
    IMPLICIT NONE
    
    INTEGER, INTENT(in)                                  :: dorig, ddes, dorig1, ddes1
    REAL, DIMENSION(dorig), INTENT(in)                   :: levorig, valsorig
    REAL, DIMENSION(dorig1), INTENT(in)                  :: btlevorig
    REAL, DIMENSION(ddes), INTENT(in)                    :: levdes
    REAL, DIMENSION(ddes1), INTENT(in)                   :: btlevdes
    REAL, DIMENSION(ddes), INTENT(out)                   :: valsdes
    
    ! Local
    INTEGER                                              :: ko, kd
    REAL                                                 :: deskm1, deskp1
    REAL                                                 :: orikm1, orikp1
    REAL                                                 :: Dlevorig, delta_levorig, Dlevdes
    REAL                                                 :: delta_desm1, delta_desp1
    REAL                                                 :: deltatop_levorig, deltabot_levorig
    CHARACTER(len=50)                                    :: fname

!!!!!!! Variables
! dorig: Original amount of vertical levels
! levorig: original values of the levels (centerd into the level) [m]
! valsorig: original values of the field at the original levels (as density or similar [kgm-3])
! dorig1: original amount of vertical levels + 1
! btlevorig: bottom, tops of the original cells [m]
!     btlev(k) < lev(k) < btlev(k+1)
! ddes: desired amount of vertical levels
! levdes: desired levels [m]
! ddes1: desired amount of vertical levels + 1
! btlevdes: bottom, tops of the desired cells [m]
! valsdest: mass-conservative interpolated values (as density or similar [kgm-3])
    
    fname = 'massconserv_vinterp1D'
    
    valsdes = 0.
    
    ! Let's be efficient
   
    ! Assuming surface -> top vertical layers and Xsfc < Xtop (as the case with height...)
   
    ! Here comes the proper integration, where the values of the original cells within the range
    !   of the desired cell, are added according to the percentage of the height of the original 
    !   cell that lays within the range of the desired cell
    !   
    !   valsdes(kd) = SUM_Nlev(delta_levorig(ko)*valsorig(ko)) + 
    !     deltatop_levorig(itopkin)*valsorig(itopkin) + deltabot_levorig(ibotkin)*valsorig(ibotkin)
    !
    !   valdes(kd) = valdes(kd) / Dlevdes
    !
    !     where:
    !        Dlevdes = deskp1 - deskm1
    !        Dlevorig = orikp1 - orikm1
    ! 
    !        delta_levorig(ko) = Dlevorig - delta_desm1 - delta_desp1
    !
    !        delta_desm1 = 
    !          deskm1 - orikm1;        deskm1 > orikm1
    !          0;                      deskm1 < orikm1   
    !        delta_desp1 = 
    !          orikp1 - deskp1;        deskp1 < orikp1
    !          0;                      deskp1 > orikp1
    !
    !        deltatop_levorig(itopkin) = orikp1 - deskm1
    !        deltabot_levorig(itopkin) = deskp1 - orikm1
    !
    !     being:
    !       orikm1: value at the base of the original cell
    !       orikp1: value at the top of the original cell
    !       deskm1: value at the base of the desired cell
    !       deskp1: value at the top of the desired cell
    !       Nlev: amount of original levels within the height of the kd-desired cell
    !         Nlev = orilev(ko) <= deskm1 .and. orilev(ko) > deskp1
   
    DO kd=1, ddes

      ! Cell's bottom          
      deskm1 = btlevdes(kd)
      ! Cell0s top
      deskp1 = btlevdes(kd+1)
      
      Dlevdes = deskp1 - deskm1
  
      ! Looking for the original cells within the range of the desired cell
      DO ko=1, dorig
        ! original cell's top
        orikp1 = btlevorig(ko+1)

        ! original cell's bottom          
        orikm1 = btlevorig(ko)

        IF (levorig(ko) >= deskm1 .AND. levorig(ko) < deskp1) THEN
          ! Center of the columns
          
          Dlevorig = orikp1 - orikm1
          IF (deskm1 > orikm1) THEN
            delta_desm1 = deskm1 - orikm1
          ELSE
            delta_desm1 = 0.
          END IF
          IF (deskp1 < orikp1) THEN
            delta_desp1 = orikp1 - deskp1
          ELSE
            delta_desp1 = 0.
          END IF
        
          delta_levorig = Dlevorig - delta_desm1 - delta_desp1
        
          valsdes(kd) = valsdes(kd) + delta_levorig*valsorig(ko)

        ELSE IF (orikp1 > deskm1 .AND. levorig(ko) < deskm1) THEN
          ! We might have only top of the original columns wihtin the range
       
          deltatop_levorig = orikp1 - deskm1
        
          valsdes(kd) = valsdes(kd) + deltatop_levorig*valsorig(ko)      

        ELSE IF (orikm1 < deskp1 .AND. levorig(ko) > deskp1) THEN
          ! We might have only base of the original columns wihtin the range

          deltabot_levorig = deskp1 - orikm1
          
          valsdes(kd) = valsdes(kd) + deltabot_levorig*valsorig(ko)      
        
        ELSE IF (orikm1 > deskp1) THEN
          EXIT
          
        END IF

      END DO
      
      valsdes(kd) = valsdes(kd)/Dlevdes
      
    END DO
  
    RETURN
    
  END SUBROUTINE massconserv_vinterp1D
  
  SUBROUTINE presbars_to_hgtbars(topo, dzs, zg, dz, oripres, psfc, ptop, dpres, despressv,            &
    firstrightlev, Nrightlevs, rightdespressv, hgts, bthgts)
  ! Subroutine to transform from desired bars defined by their central pressures to bars in height 
  !   coordinates using model's geopotential to define its heights
  
    IMPLICIT NONE
    
    INTEGER, INTENT(in)                                  :: dpres, dz, dzs
    REAL, DIMENSION(dpres), INTENT(in)                   :: despressv
    REAL, INTENT(in)                                     :: topo, psfc, ptop
    REAL, DIMENSION(dzs), INTENT(in)                     :: zg
    REAL, DIMENSION(dz), INTENT(in)                      :: oripres
    INTEGER, INTENT(out)                                 :: firstrightlev, Nrightlevs
    REAL, DIMENSION(dpres), INTENT(out)                  :: hgts, rightdespressv
    REAL, DIMENSION(dpres,2), INTENT(out)                :: bthgts
    
    ! Local
    INTEGER                                              :: k, ko, dzstag
    REAL                                                 :: botpressv
    REAL                                                 :: a, b
    REAL, DIMENSION(dz)                                  :: orihgt
    REAL, DIMENSION(:), ALLOCATABLE                      :: oribthgt
    CHARACTER(len=50)                                    :: fname 

!!!!!!! Variables
! topo: topography [m]
! dzs: amount of geopotential heights
! zg: geopotential heights [m2s-2]
! dz: amount of original pressure
! oripres: original pressure values [Pa]
! psfc: pressure at the surfrace [Pa]
! ptop: pressure at the top [Pa]
! dpres: amount of desired pressure levels 
! despressv: desired pressure levels [Pa]
! Nrightlevs: amount of right pressure levels (it might hapeen to want 'below' ground or 'above' top 
!   values)
! rightdespressv: right desired pressure values to provide heights
! hgts: heights at the desired pressure levels [m]
! bthgts: bottoms and tops of the cells at the desired pressure levels [m]
  
    fname = 'presbars_to_hgtbars'
  
    rightdespressv = 0.
    hgts = 0.
    bthgts = 0.
  
    IF (dzs == dz+1) THEN
      !IF (ALLOCATED(geopt)) DEALLOCATE(geopt) Efficiency ?
      dzstag = dzs
      ALLOCATE(oribthgt(dzstag))
      oribthgt = zg / 9.81
    ELSE
      !IF (ALLOCATED(geopt)) DEALLOCATE(geopt) Efficiency ?
      dzstag = dz+1
      ALLOCATE(oribthgt(dzstag))
      oribthgt(1) = zg(1) / 9.81
      DO k=2, dz
        oribthgt(k) = 0.5*(zg(k-1) + zg(k))/9.81
      END DO
      ! Let's get creative here. We could ask for p_top and get a standard atmosphere 
      !   conversion to geopotential... instead geopt(dz+1) = geopt(dz) + (zg(dz)-zg(dz-1))
      oribthgt(dzstag) = (zg(dz) + (zg(dz)-zg(dz-1)) ) / 9.81
    END IF
    orihgt = 0.5*(oribthgt(2:dzstag)+oribthgt(1:dzstag-1))
  
    ! Get the conversion
    firstrightlev = -1
    Nrightlevs = 0
    hgts(1) = topo
    DO k=1, dpres
      DO ko = 1, dz
        ! Filtering to avoid 'below ground' and 'above atmos top' interpolation values
        IF (oripres(ko) >= despressv(k) .AND. oripres(ko+1) < despressv(k) .AND.                      &
            psfc > despressv(k) .AND. ptop < despressv(k)) THEN
  
          Nrightlevs = Nrightlevs + 1
          IF (firstrightlev == -1) firstrightlev = k
          
          a = (oripres(ko+1)*orihgt(ko)-oripres(ko)*orihgt(ko+1))/(oripres(ko+1)-oripres(ko))
          b = (orihgt(ko+1)-orihgt(ko))/(oripres(ko+1)-oripres(ko))
          rightdespressv(Nrightlevs) = despressv(k)
          hgts(Nrightlevs) = a + despressv(k)*b
  
          EXIT
        END IF
      END DO
    END DO
  
    ! Giving the bottom / tops of only the right desired pressure values. Interpolating from pressure
    !   values and using then the geopotential to get the right height, as a way to be more precise
    bthgts(1,1) = topo
    DO k=2, Nrightlevs
    
      ! p-height of the cell
      botpressv = 0.5*(rightdespressv(k) + rightdespressv(k-1))
      
      ! Interprolation
      bthgts(k,1) = interpolate1D(dz, oripres, orihgt, botpressv)
      bthgts(k-1,2) = bthgts(k,1)
    END DO
    bthgts(Nrightlevs,2) = oribthgt(dzstag)

    DEALLOCATE(oribthgt) 

    RETURN

  END SUBROUTINE presbars_to_hgtbars
     
  REAL FUNCTION interpolate1D(Nvals, xvals, yvals, xpos)
  ! Function to interpolate 1D value
  
    IMPLICIT NONE
    
    INTEGER, INTENT(in)                                  :: Nvals
    REAL, DIMENSION(Nvals), INTENT(in)                   :: xvals, yvals
    REAL, INTENT(in)                                     :: xpos
    
    ! Local
    INTEGER                                              :: i
    REAL                                                 :: dx, dy, a, b
    CHARACTER(len=50)                                    :: fname
    
!!!!!!! Variables
! Nvals: Amount of values to use for the interpolation
! xvals: x-axis positions
! yvals: y-axis values
! xpos: position at which we want to interpolate

    fname = 'interpolate1D'

    interpolate1D = 1.e20
  
    IF (xvals(2) > xvals(1)) THEN
      DO i=1, Nvals-1
        IF (xvals(i) <= xpos .AND. xvals(i+1) > xpos) THEN
          dx = xvals(i+1)-xvals(i)
          dy = yvals(i+1)-yvals(i)
          a = (xvals(i+1)*yvals(i)-xvals(i)*yvals(i+1))/dx
          b = dy/dx
          interpolate1D = a + xpos*b
          EXIT
        END IF
      END DO
    ELSE
      DO i=1,Nvals
        IF (xvals(i) > xpos .AND. xvals(i+1) <= xpos) THEN
          dx = xvals(i)-xvals(i+1)
          dy = yvals(i)-yvals(i+1)
          a = (xvals(i)*yvals(i+1)-xvals(i+1)*yvals(i))/dx
          b = dy/dx
          interpolate1D = a + xpos*b
          EXIT
        END IF
      END DO
    END IF
    
    RETURN

  END FUNCTION interpolate1D

#endif

END MODULE module_diag_pld
#endif
