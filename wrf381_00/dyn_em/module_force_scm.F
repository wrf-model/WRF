MODULE module_force_scm

! AUTHOR: Josh Hacker (NCAR/RAL)
! Forces a single-column (3x3) version of WRF

CONTAINS

   SUBROUTINE force_scm(itimestep, dt, scm_force, dx, num_force_layers       &
                             , scm_th_adv, scm_qv_adv                        &
                             , scm_ql_adv                                    &
                             , scm_wind_adv, scm_vert_adv                    &
                             , scm_th_t_tend, scm_qv_t_tend                  &
                             , scm_soilT_force, scm_soilQ_force              &
                             , scm_force_th_largescale                       &
                             , scm_force_qv_largescale                       &
                             , scm_force_ql_largescale                       &
                             , scm_force_wind_largescale                     &
                             , u_base, v_base, z_base                        &
                             , z_force, z_force_tend                         &
                             , u_g, v_g                                      &
                             , u_g_tend, v_g_tend                            &
                             , w_subs, w_subs_tend                           &
                             , th_upstream_x, th_upstream_x_tend             &
                             , th_upstream_y, th_upstream_y_tend             &
                             , qv_upstream_x, qv_upstream_x_tend             &
                             , qv_upstream_y, qv_upstream_y_tend             &
                             , ql_upstream_x, ql_upstream_x_tend             &
                             , ql_upstream_y, ql_upstream_y_tend             &
                             , u_upstream_x, u_upstream_x_tend               &
                             , u_upstream_y, u_upstream_y_tend               &
                             , v_upstream_x, v_upstream_x_tend               &
                             , v_upstream_y, v_upstream_y_tend               &
                             , th_t_tend, qv_t_tend                          &
                             , tau_x, tau_x_tend                             &
                             , tau_y, tau_y_tend                             &
                             ,th_largescale                                  &
                             ,th_largescale_tend                             &
                             ,qv_largescale                                  &
                             ,qv_largescale_tend                             &
                             ,ql_largescale                                  &
                             ,ql_largescale_tend                             &
                             ,u_largescale                                   &
                             ,u_largescale_tend                              &
                             ,v_largescale                                   &
                             ,v_largescale_tend                              &
                             ,tau_largescale                                 &
                             ,tau_largescale_tend                            &
                             , num_force_soil_layers, num_soil_layers        &
                             , soil_depth_force, zs                          &
                             , tslb, smois                                   &
                             , t_soil_forcing_val, t_soil_forcing_tend       &
                             , q_soil_forcing_val, q_soil_forcing_tend       &
                             , tau_soil                                      &
                             , z, z_at_w, th, qv, ql, u, v                   &
                             , thten, qvten, qlten, uten, vten               &
                             , ids, ide, jds, jde, kds, kde                  &
                             , ims, ime, jms, jme, kms, kme                  &
                             , ips, ipe, jps, jpe, kps, kpe                  &
                             , kts, kte                                      &
                            )

! adds forcing to bl tendencies and also to base state/geostrophic winds.

   USE module_init_utilities, ONLY : interp_0
   IMPLICIT NONE


   INTEGER,    INTENT(IN   )                 :: itimestep
   INTEGER,    INTENT(IN   )                 :: num_force_layers, scm_force
   REAL,       INTENT(IN   )                 :: dt,dx
   LOGICAL,    INTENT(IN   )                 :: scm_th_adv, &
                                                scm_qv_adv, &
                                                scm_ql_adv, &
                                                scm_wind_adv, &
                                                scm_vert_adv, &
                                                scm_soilT_force, &
                                                scm_soilQ_force, &
                                                scm_force_th_largescale, &
                                                scm_force_qv_largescale, &
                                                scm_force_ql_largescale, &
                                                scm_force_wind_largescale,&
                                                scm_th_t_tend,&
                                                scm_qv_t_tend

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: z, th, qv, ql
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: u, v
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: z_at_w
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: thten, qvten
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: qlten
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: uten, vten
   REAL, DIMENSION( kms:kme ), INTENT(INOUT)               :: u_base, v_base
   REAL, DIMENSION( kms:kme ), INTENT(INOUT)               :: z_base
   REAL, DIMENSION(num_force_layers), INTENT (INOUT)       :: z_force
   REAL, DIMENSION(num_force_layers), INTENT (INOUT)       :: u_g,v_g

   REAL, DIMENSION(num_force_layers), INTENT (IN) :: z_force_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: u_g_tend,v_g_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: w_subs_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: th_upstream_x_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: th_upstream_y_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: qv_upstream_x_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: qv_upstream_y_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: ql_upstream_x_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: ql_upstream_y_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: u_upstream_x_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: u_upstream_y_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: v_upstream_x_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: v_upstream_y_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: th_t_tend 
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: qv_t_tend  
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: tau_x_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: tau_y_tend

   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: th_upstream_x
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: th_upstream_y
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: u_upstream_x
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: u_upstream_y
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: v_upstream_x
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: v_upstream_y
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: qv_upstream_x
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: qv_upstream_y
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: ql_upstream_x
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: ql_upstream_y
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: w_subs
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: tau_x
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: tau_y

! WA 1/8/10 for large-scale forcing
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: th_largescale
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: th_largescale_tend
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: u_largescale
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: u_largescale_tend
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: v_largescale
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: v_largescale_tend
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: qv_largescale
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: qv_largescale_tend
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: ql_largescale
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: ql_largescale_tend
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: tau_largescale
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: tau_largescale_tend

! WA 1/3/10 For soil forcing
   INTEGER,    INTENT(IN   )         :: num_force_soil_layers, num_soil_layers
   REAL, DIMENSION(ims:ime,num_soil_layers,jms:jme),INTENT(INOUT) :: tslb, smois
   REAL, DIMENSION(num_force_soil_layers), INTENT (INOUT) :: t_soil_forcing_val
   REAL, DIMENSION(num_force_soil_layers), INTENT (INOUT) :: t_soil_forcing_tend
   REAL, DIMENSION(num_force_soil_layers), INTENT (INOUT) :: q_soil_forcing_val
   REAL, DIMENSION(num_force_soil_layers), INTENT (INOUT) :: q_soil_forcing_tend
   REAL, DIMENSION(num_force_soil_layers), INTENT (INOUT) :: tau_soil
   REAL, DIMENSION(num_force_soil_layers), INTENT (IN   ) :: soil_depth_force
   REAL, DIMENSION(num_soil_layers),       INTENT (IN   ) :: zs        

   INTEGER,    INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                       kts,kte
   
! Local
   INTEGER                      :: i,j,k
   LOGICAL                      :: debug = .false.
   REAL                         :: t_x, t_y, qv_x, qv_y, ql_x, ql_y
   REAL                         :: u_x, u_y, v_x, v_y
   REAL, DIMENSION(kms:kme)     :: th_adv_tend, qv_adv_tend, ql_adv_tend
   REAL, DIMENSION(kms:kme)     :: u_adv_tend, v_adv_tend
   REAL, DIMENSION(kms:kme)     :: th_t_tend_interp, qv_t_tend_interp
   REAL, DIMENSION(kms:kme)     :: dthdz, dudz, dvdz, dqvdz, dqldz
   REAL                         :: w
   REAL, DIMENSION(kms:kme)     :: w_dthdz, w_dudz, w_dvdz, w_dqvdz, w_dqldz
   REAL, DIMENSION(kms:kme)     :: adv_timescale_x, adv_timescale_y
   CHARACTER*256                :: message
! Large-scale forcing WA 1/8/10
   REAL                         :: t_ls, qv_ls, ql_ls
   REAL                         :: u_ls, v_ls
   REAL, DIMENSION(kms:kme)     :: th_ls_tend, qv_ls_tend, ql_ls_tend
   REAL, DIMENSION(kms:kme)     :: u_ls_tend, v_ls_tend
   REAL, DIMENSION(kms:kme)     :: ls_timescale
! Soil forcing WA 1/3/10
   INTEGER                      :: ks
   REAL                         :: t_soil, q_soil
   REAL, DIMENSION(num_soil_layers) :: t_soil_tend, q_soil_tend
   REAL, DIMENSION(num_soil_layers) :: timescale_soil

   IF ( scm_force .EQ. 0 ) return
 
! NOTES
! z is kts:kte
! z_at_w is kms:kme

     ! this is a good place for checks on the configuration
     if ( z_force(1) > z(ids,1,jds) ) then
        CALL wrf_message("First forcing level must be lower than first WRF half-level")
        WRITE( message , * ) 'z forcing = ',z_force(1), 'z = ',z(ids,1,jds)
!       print*,"z forcing = ",z_force(1), "z = ",z(ids,1,jds)
        CALL wrf_error_fatal( message )
     endif

     z_force = z_force + dt*z_force_tend 
     u_g = u_g + dt*u_g_tend 
     v_g = v_g + dt*v_g_tend 
     tau_x = tau_x + dt*tau_x_tend 
     tau_y = tau_y + dt*tau_y_tend 
     tau_largescale = tau_largescale + dt*tau_largescale_tend 

     if ( scm_th_adv .AND. th_upstream_x(1) > 0.) then
       th_upstream_x = th_upstream_x + dt*th_upstream_x_tend
       th_upstream_y = th_upstream_y + dt*th_upstream_y_tend
     endif
     if ( scm_qv_adv .AND. qv_upstream_x(1) > 0.) then
       qv_upstream_x = qv_upstream_x + dt*qv_upstream_x_tend
       qv_upstream_y = qv_upstream_y + dt*qv_upstream_y_tend
     endif
     if ( scm_ql_adv .AND. ql_upstream_x(1) > 0.) then
       ql_upstream_x = ql_upstream_x + dt*ql_upstream_x_tend
       ql_upstream_y = ql_upstream_y + dt*ql_upstream_y_tend
     endif
     if ( scm_wind_adv .AND. u_upstream_x(1) > -900.) then
       u_upstream_x = u_upstream_x + dt*u_upstream_x_tend
       u_upstream_y = u_upstream_y + dt*u_upstream_y_tend
       v_upstream_x = v_upstream_x + dt*v_upstream_x_tend
       v_upstream_y = v_upstream_y + dt*v_upstream_y_tend
     endif
     if ( scm_vert_adv ) then
       w_subs = w_subs + dt*w_subs_tend
     endif

     if ( scm_force_th_largescale .AND. th_largescale(1) > 0.) then
       th_largescale = th_largescale + dt*th_largescale_tend
     endif
     if ( scm_force_qv_largescale .AND. qv_largescale(1) > 0.) then
       qv_largescale = qv_largescale + dt*qv_largescale_tend
     endif
     if ( scm_force_ql_largescale.AND. ql_largescale(1) > 0.) then
       ql_largescale = ql_largescale + dt*ql_largescale_tend
     endif
     if ( scm_force_wind_largescale .AND. u_largescale(1) > -900.) then
       u_largescale = u_largescale + dt*u_largescale_tend
       v_largescale = v_largescale + dt*v_largescale_tend
     endif

     if ( scm_soilT_force ) then
       t_soil_forcing_val = t_soil_forcing_val + dt*t_soil_forcing_tend
     endif
     if ( scm_soilQ_force ) then
       q_soil_forcing_val = q_soil_forcing_val + dt*q_soil_forcing_tend
     endif

! 0 everything in case we don't set it later
     th_adv_tend = 0.0
     qv_adv_tend = 0.0
     ql_adv_tend = 0.0
     u_adv_tend  = 0.0
     v_adv_tend  = 0.0
     th_ls_tend = 0.0
     qv_ls_tend = 0.0
     ql_ls_tend = 0.0
     u_ls_tend  = 0.0
     v_ls_tend  = 0.0
     w_dthdz     = 0.0
     w_dqvdz     = 0.0
     w_dqldz     = 0.0
     w_dudz      = 0.0
     w_dvdz      = 0.0
     adv_timescale_x = 0.0
     adv_timescale_y = 0.0
     th_t_tend_interp =0.0
     qv_t_tend_interp =0.0
 
! now interpolate forcing to model vertical grid

!    if ( debug ) print*,' z u_base v_base '
     CALL wrf_debug(100,'k z_base  u_base  v_base')
     do k = kms,kme-1
       z_base(k) = z(ids,k,jds)
       u_base(k) = interp_0(u_g,z_force,z_base(k),num_force_layers)
       v_base(k) = interp_0(v_g,z_force,z_base(k),num_force_layers)
!      if ( debug ) print*,z_base(k),u_base(k),v_base(k)
       WRITE( message, '(i4,3f12.4)' ) k,z_base(k),u_base(k),v_base(k)
       CALL wrf_debug ( 100, message )
    enddo

    if ( scm_th_adv .or. scm_qv_adv .or. scm_ql_adv .or. scm_wind_adv ) then
       if ( scm_th_adv ) CALL wrf_debug ( 100, 'k  tau_x  tau_y t_ups_x t_ups_y t_m ' )
       do k = kms,kme-1
          adv_timescale_x(k) = interp_0(tau_x,z_force,z(ids,k,jds),num_force_layers)
          adv_timescale_y(k) = interp_0(tau_y,z_force,z(ids,k,jds),num_force_layers)
       enddo
    endif

    if ( scm_th_adv ) then
       if ( th_upstream_x(1) > 0.) then
          do k = kms,kme-1
             t_x = interp_0(th_upstream_x,z_force,z(ids,k,jds),num_force_layers)
             t_y = interp_0(th_upstream_y,z_force,z(ids,k,jds),num_force_layers)

             th_adv_tend(k) = (t_x-th(ids,k,jds))/adv_timescale_x(k) + (t_y-th(ids,k,jds))/adv_timescale_y(k)
             WRITE( message, '(i4,5f12.4)' ) k,adv_timescale_x(k), adv_timescale_y(k), t_x, t_y, th(ids,k,jds)
             CALL wrf_debug ( 100, message )
          enddo
       else ! WA if upstream is empty, use tendency only not value+tend
          do k = kms,kme-1
             t_x = interp_0(dt*th_upstream_x_tend,z_force,z(ids,k,jds),num_force_layers)
             t_y = interp_0(dt*th_upstream_y_tend,z_force,z(ids,k,jds),num_force_layers)

             th_adv_tend(k) = t_x/adv_timescale_x(k) + t_y/adv_timescale_y(k)
             WRITE( message, '(i4,5f12.4)' ) k,adv_timescale_x(k), adv_timescale_y(k), t_x, t_y, th(ids,k,jds)
             CALL wrf_debug ( 100, message )
          enddo
       endif
    endif
     if (minval(tau_x) < 0) then
       print*,tau_x
       stop 'TAU_X'
     endif
     if (minval(tau_y) < 0) then
       print*,z_force
       print*,tau_y
       stop 'TAU_Y'
     endif

    if ( scm_qv_adv ) then
       if ( qv_upstream_x(1) > 0.) then
          do k = kms,kme-1
             qv_x = interp_0(qv_upstream_x,z_force,z(ids,k,jds),num_force_layers)
             qv_y = interp_0(qv_upstream_y,z_force,z(ids,k,jds),num_force_layers)

             qv_adv_tend(k) = (qv_x-qv(ids,k,jds))/adv_timescale_x(k) + (qv_y-qv(ids,k,jds))/adv_timescale_y(k)
             WRITE( message, * ) 'qv_adv_tend branch 1',k,adv_timescale_x(k), qv_upstream_x(k), adv_timescale_y(k), &
                                  qv_x, qv_y, qv(ids,k,jds), qv_adv_tend(k)
             CALL wrf_debug ( 100, message )
          enddo
       else ! WA if upstream is empty, use tendency only not value+tend
          do k = kms,kme-1
             qv_x = interp_0(dt*qv_upstream_x_tend,z_force,z(ids,k,jds),num_force_layers)
             qv_y = interp_0(dt*qv_upstream_y_tend,z_force,z(ids,k,jds),num_force_layers)

             qv_adv_tend(k) = qv_x/adv_timescale_x(k) + qv_y/adv_timescale_y(k)
             WRITE( message, * ) 'qv_adv_tend branch 2',k,adv_timescale_x(k), adv_timescale_y(k), qv_upstream_x(k), &
                                  qv_x, qv_y, qv(ids,k,jds), qv_adv_tend(k)
             CALL wrf_debug ( 100, message )
          enddo
       endif
    endif

    if ( scm_ql_adv ) then
       if ( ql_upstream_x(1) > 0.) then
          do k = kms,kme-1
             ql_x = interp_0(ql_upstream_x,z_force,z(ids,k,jds),num_force_layers)
             ql_y = interp_0(ql_upstream_y,z_force,z(ids,k,jds),num_force_layers)

             ql_adv_tend(k) = (ql_x-ql(ids,k,jds))/adv_timescale_x(k) + (ql_y-ql(ids,k,jds))/adv_timescale_y(k)
          enddo
       else ! WA if upstream is empty, use tendency only not value+tend
          do k = kms,kme-1
             ql_x = interp_0(dt*ql_upstream_x_tend,z_force,z(ids,k,jds),num_force_layers)
             ql_y = interp_0(dt*ql_upstream_y_tend,z_force,z(ids,k,jds),num_force_layers)

             ql_adv_tend(k) = ql_x/adv_timescale_x(k) + ql_y/adv_timescale_y(k)
          enddo
       endif
    endif

    if ( scm_wind_adv ) then
       if ( u_upstream_x(1) > -900.) then
          do k = kms,kme-1
             u_x = interp_0(u_upstream_x,z_force,z(ids,k,jds),num_force_layers)
             u_y = interp_0(u_upstream_y,z_force,z(ids,k,jds),num_force_layers)

             v_x = interp_0(v_upstream_x,z_force,z(ids,k,jds),num_force_layers)
             v_y = interp_0(v_upstream_y,z_force,z(ids,k,jds),num_force_layers)

             u_adv_tend(k) = (u_x-u(ids,k,jds))/adv_timescale_x(k) + (u_y-u(ids,k,jds))/adv_timescale_y(k)
             v_adv_tend(k) = (v_x-v(ids,k,jds))/adv_timescale_x(k) + (v_y-v(ids,k,jds))/adv_timescale_y(k)
          enddo
       else ! WA if upstream is empty, use tendency only not value+tend
          do k = kms,kme-1
             u_x = interp_0(dt*u_upstream_x_tend,z_force,z(ids,k,jds),num_force_layers)
             u_y = interp_0(dt*u_upstream_y_tend,z_force,z(ids,k,jds),num_force_layers)

             v_x = interp_0(dt*v_upstream_x_tend,z_force,z(ids,k,jds),num_force_layers)
             v_y = interp_0(dt*v_upstream_y_tend,z_force,z(ids,k,jds),num_force_layers)

             u_adv_tend(k) = u_x/adv_timescale_x(k) + u_y/adv_timescale_y(k)
             v_adv_tend(k) = v_x/adv_timescale_x(k) + v_y/adv_timescale_y(k)
          enddo
       endif
    endif



    if ( scm_th_t_tend ) then
       do k = kms,kme-1
          th_t_tend_interp(k) = interp_0(th_t_tend,z_force,z(ids,k,jds),num_force_layers) 
       enddo
    endif

    if ( scm_qv_t_tend ) then
       do k = kms,kme-1
          qv_t_tend_interp(k) = interp_0(qv_t_tend,z_force,z(ids,k,jds),num_force_layers) 
          write(*,'(i3, f20.15)') k, qv_t_tend_interp(k)
       enddo
    endif


! Large scale forcing starts here 1/8/10 WA
    if ( scm_force_th_largescale .or. scm_force_qv_largescale .or. scm_force_ql_largescale .or. scm_force_wind_largescale ) then
       do k = kms,kme-1
          ls_timescale(k) = interp_0(tau_largescale,z_force,z(ids,k,jds),num_force_layers)
       enddo
    endif

    if ( scm_force_th_largescale ) then
       if ( th_largescale(1) > 0.) then
          do k = kms,kme-1
             t_ls = interp_0(th_largescale,z_force,z(ids,k,jds),num_force_layers)
             th_ls_tend(k) = (t_ls-th(ids,k,jds))/ls_timescale(k)
          enddo
       else ! WA if upstream is empty, use tendency only not value+tend
          do k = kms,kme-1
             t_ls = interp_0(dt*th_largescale_tend,z_force,z(ids,k,jds),num_force_layers)
             th_ls_tend(k) = t_ls/ls_timescale(k)
          enddo
       endif
    endif

    if ( scm_force_qv_largescale ) then
       if ( qv_largescale(1) > 0.) then
          do k = kms,kme-1
             qv_ls = interp_0(qv_largescale,z_force,z(ids,k,jds),num_force_layers)
             qv_ls_tend(k) = (qv_ls-qv(ids,k,jds))/ls_timescale(k)
          enddo
       else ! WA if upstream is empty, use tendency only not value+tend
          do k = kms,kme-1
             qv_ls = interp_0(dt*qv_largescale_tend,z_force,z(ids,k,jds),num_force_layers)
             qv_ls_tend(k) = qv_ls/ls_timescale(k)
          enddo
       endif
    endif

    if ( scm_force_ql_largescale ) then
       if ( ql_largescale(1) > 0.) then
          do k = kms,kme-1
             ql_ls = interp_0(ql_largescale,z_force,z(ids,k,jds),num_force_layers)
             ql_ls_tend(k) = (ql_ls-ql(ids,k,jds))/ls_timescale(k)
          enddo
       else ! WA if upstream is empty, use tendency only not value+tend
          do k = kms,kme-1
             ql_ls = interp_0(dt*ql_largescale_tend,z_force,z(ids,k,jds),num_force_layers)
             ql_ls_tend(k) = ql_ls/ls_timescale(k)
          enddo
       endif
    endif

    if ( scm_force_wind_largescale ) then
       if ( u_largescale(1) > -900.) then
          do k = kms,kme-1
             u_ls = interp_0(u_largescale,z_force,z(ids,k,jds),num_force_layers)
             v_ls = interp_0(v_largescale,z_force,z(ids,k,jds),num_force_layers)
             u_ls_tend(k) = (u_ls-u(ids,k,jds))/ls_timescale(k)
             v_ls_tend(k) = (v_ls-v(ids,k,jds))/ls_timescale(k)
          enddo
       else ! WA if upstream is empty, use tendency only not value+tend
          do k = kms,kme-1
             u_ls = interp_0(dt*u_largescale_tend,z_force,z(ids,k,jds),num_force_layers)
             v_ls = interp_0(dt*v_largescale_tend,z_force,z(ids,k,jds),num_force_layers)
             u_ls_tend(k) = u_ls/ls_timescale(k)
             v_ls_tend(k) = v_ls/ls_timescale(k)
          enddo
       endif
    endif

! Now do vertical advection.  Note that no large-scale vertical advection
! is implemented at this time, may not make sense anyway (WA).
! loops are set so that the top and bottom (w=0) are handled correctly
! vertical derivatives
    do k = kms+1,kme-1
       dthdz(k) = (th(2,k,2)-th(2,k-1,2))/(z(2,k,2)-z(2,k-1,2))
       dqvdz(k) = (qv(2,k,2)-qv(2,k-1,2))/(z(2,k,2)-z(2,k-1,2))
       dqldz(k) = (ql(2,k,2)-ql(2,k-1,2))/(z(2,k,2)-z(2,k-1,2))
       dudz(k)  = (u(2,k,2)-u(2,k-1,2))/(z(2,k,2)-z(2,k-1,2))
       dvdz(k)  = (v(2,k,2)-v(2,k-1,2))/(z(2,k,2)-z(2,k-1,2))
    enddo

! w on full levels, then advect
    if ( scm_vert_adv ) then
       do k = kms+1,kme-1
          w = interp_0(w_subs,z_force,z_at_w(ids,k,jds),num_force_layers)
          w_dthdz(k) = -w*dthdz(k)
          w_dqvdz(k) = -w*dqvdz(k)
          w_dqldz(k) = -w*dqldz(k)
          w_dudz(k)  = -w*dudz(k)
          w_dvdz(k)  = -w*dvdz(k)
       enddo
    endif

! set tendencies for return
! vertical advection tendencies need to be interpolated back to half levels
    CALL wrf_debug ( 100, 'j, k, th_adv_ten, qv_adv_ten, ql_adv_ten, u_adv_ten, v_adv_ten')
    do j = jms,jme
    do k = kms,kme-1
    if(j==1) WRITE( message, * ) k,th_adv_tend(k),qv_adv_tend(k),ql_adv_tend(k), u_adv_tend(k),v_adv_tend(k)
    if(j==1) CALL wrf_debug ( 100, message )
    do i = ims,ime
       thten(i,k,j) = thten(i,k,j) + th_adv_tend(k) +              &
                      0.5*(w_dthdz(k) + w_dthdz(k+1)) + th_t_tend_interp(k)&
                      + th_ls_tend(k)
       qvten(i,k,j) = qvten(i,k,j) + qv_adv_tend(k) +              &
                      0.5*(w_dqvdz(k) + w_dqvdz(k+1)) + qv_t_tend_interp(k)&
                      + qv_ls_tend(k)
       qlten(i,k,j) = qlten(i,k,j) + ql_adv_tend(k) +              &
                      0.5*(w_dqldz(k) + w_dqldz(k+1))              &
                      + ql_ls_tend(k)
       uten(i,k,j)  = uten(i,k,j) + u_adv_tend(k) +                &
                      0.5*(w_dudz(k) + w_dudz(k+1))                &
                      + u_ls_tend(k)
       vten(i,k,j)  = vten(i,k,j) + v_adv_tend(k) +                &
                      0.5*(w_dvdz(k) + w_dvdz(k+1))                & 
                      + v_ls_tend(k)
    enddo
    enddo
    enddo

! soil forcing 1/3/10 WA
    if ( scm_soilT_force ) then
       do ks = 1,num_soil_layers
          t_soil = interp_0(t_soil_forcing_val,soil_depth_force,zs(ks),num_force_soil_layers)
          timescale_soil(ks) = interp_0(tau_soil,soil_depth_force,zs(ks),num_force_soil_layers)
          t_soil_tend(ks) = (t_soil-tslb(ids,ks,jds))/timescale_soil(ks)
       enddo
       do j = jms,jme
          do ks = 1,num_soil_layers
             do i = ims,ime
                tslb(ids,ks,jds) = tslb(ids,ks,jds) + t_soil_tend(ks)
             enddo
          enddo
       enddo
    endif
    if ( scm_soilQ_force ) then
       do ks = 1,num_soil_layers
          q_soil = interp_0(q_soil_forcing_val,soil_depth_force,zs(ks),num_force_soil_layers)
          timescale_soil(ks) = interp_0(tau_soil,soil_depth_force,zs(ks),num_force_soil_layers)
          q_soil_tend(ks) = (q_soil-smois(ids,ks,jds))/timescale_soil(ks)
       enddo
       do j = jms,jme
          do ks = 1,num_soil_layers
             do i = ims,ime
                smois(ids,ks,jds) = smois(ids,ks,jds) + q_soil_tend(ks)
             enddo
          enddo
       enddo
    endif

    RETURN

   END SUBROUTINE force_scm

END MODULE module_force_scm
