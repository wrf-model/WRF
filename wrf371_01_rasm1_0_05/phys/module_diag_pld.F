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
                    ids,ide, jds,jde, kds,kde,                      &
                    ims,ime, jms,jme, kms,kme,                      &
                    its,ite, jts,jte, kts,kte                       )
   
      USE module_model_constants
   
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
   
      !  Output variables
   
      REAL   , INTENT(  OUT) ,  DIMENSION(num_press_levels)                     :: p_pl
      REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , num_press_levels , jms:jme) :: u_pl,v_pl,t_pl,rh_pl,ght_pl,s_pl,td_pl,q_pl
   
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
   
                     ke_f = ke
                     EXIT ke_loop_full
                  END IF
               END DO ke_loop_full
   
            END DO kp_loop
         END DO i_loop
      END DO j_loop

   END SUBROUTINE pld

END MODULE module_diag_pld
#endif
