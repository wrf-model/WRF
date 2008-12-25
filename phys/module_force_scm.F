MODULE module_force_scm

! AUTHOR: Josh Hacker (NCAR/RAL)
! Forces a single-column (3x3) version of WRF

CONTAINS

   SUBROUTINE force_scm(itimestep, dt, scm_force, dx, num_force_layers       &
                             , scm_th_adv, scm_qv_adv                        &
                             , scm_wind_adv, scm_vert_adv                    &
                             , u_base, v_base, z_base                        &
                             , z_force, u_g, v_g                             &
                             , u_g_tend, v_g_tend                            &
                             , w_subs, w_subs_tend                           &
                             , th_upstream_x, th_upstream_x_tend             &
                             , th_upstream_y, th_upstream_y_tend             &
                             , qv_upstream_x, qv_upstream_x_tend             &
                             , qv_upstream_y, qv_upstream_y_tend             &
                             , u_upstream_x, u_upstream_x_tend               &
                             , u_upstream_y, u_upstream_y_tend               &
                             , v_upstream_x, v_upstream_x_tend               &
                             , v_upstream_y, v_upstream_y_tend               &
                             , z, z_at_w, th, qv, u, v                       &
                             , thten, qvten, uten, vten                      &
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
                                                scm_wind_adv, &
                                                scm_vert_adv

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: z, th, qv
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: u, v
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN   ) :: z_at_w
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: thten, qvten
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: uten, vten
   REAL, DIMENSION( kms:kme ), INTENT(INOUT)               :: u_base, v_base
   REAL, DIMENSION( kms:kme ), INTENT(INOUT)               :: z_base
   REAL, DIMENSION(num_force_layers), INTENT (IN)          :: z_force
   REAL, DIMENSION(num_force_layers), INTENT (INOUT)       :: u_g,v_g

   REAL, DIMENSION(num_force_layers), INTENT (IN) :: u_g_tend,v_g_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: w_subs_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: th_upstream_x_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: th_upstream_y_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: qv_upstream_x_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: qv_upstream_y_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: u_upstream_x_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: u_upstream_y_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: v_upstream_x_tend
   REAL, DIMENSION(num_force_layers), INTENT (IN) :: v_upstream_y_tend

   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: th_upstream_x
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: th_upstream_y
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: u_upstream_x
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: u_upstream_y
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: v_upstream_x
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: v_upstream_y
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: qv_upstream_x
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: qv_upstream_y
   REAL, DIMENSION(num_force_layers), INTENT (INOUT) :: w_subs

   INTEGER,    INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                       kts,kte
   
! Local
   INTEGER                      :: i,j,k
   LOGICAL                      :: debug = .false.
   REAL                         :: t_x, t_y, qv_x, qv_y 
   REAL                         :: u_x, u_y, v_x, v_y
   REAL, DIMENSION(kms:kme)     :: tau_u, tau_v
   REAL, DIMENSION(kms:kme)     :: th_adv_tend, qv_adv_tend
   REAL, DIMENSION(kms:kme)     :: u_adv_tend, v_adv_tend
   REAL, DIMENSION(kms:kme)     :: dthdz, dudz, dvdz, dqvdz
   REAL                         :: w
   REAL, DIMENSION(kms:kme)     :: w_dthdz, w_dudz, w_dvdz, w_dqvdz
   CHARACTER*256                :: message

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

     u_g = u_g + dt*u_g_tend 
     v_g = v_g + dt*v_g_tend 

     if ( scm_th_adv ) then
       th_upstream_x = th_upstream_x + dt*th_upstream_x_tend
       th_upstream_y = th_upstream_y + dt*th_upstream_y_tend
     endif
     if ( scm_qv_adv) then
       qv_upstream_x = qv_upstream_x + dt*qv_upstream_x_tend
       qv_upstream_y = qv_upstream_y + dt*qv_upstream_y_tend
     endif
     if ( scm_wind_adv ) then
       u_upstream_x = u_upstream_x + dt*u_upstream_x_tend
       u_upstream_y = u_upstream_y + dt*u_upstream_y_tend
       v_upstream_x = v_upstream_x + dt*v_upstream_x_tend
       v_upstream_y = v_upstream_y + dt*v_upstream_y_tend
     endif
     if ( scm_vert_adv ) then
       w_subs = w_subs + dt*w_subs_tend
     endif

! 0 everything in case we don't set it later
     th_adv_tend = 0.0
     qv_adv_tend = 0.0
     u_adv_tend  = 0.0
     v_adv_tend  = 0.0
     w_dthdz     = 0.0
     w_dqvdz     = 0.0
     w_dudz      = 0.0
     w_dvdz      = 0.0

! now interpolate forcing to model vertical grid

!    if ( debug ) print*,' z u_base v_base '
     CALL wrf_debug(100,'z_base  u_base  v_base')
     do k = kms,kme-1
       z_base(k) = z(ids,k,jds)
       u_base(k) = interp_0(u_g,z_force,z_base(k),num_force_layers)
       v_base(k) = interp_0(v_g,z_force,z_base(k),num_force_layers)
!      if ( debug ) print*,z_base(k),u_base(k),v_base(k)
       WRITE( message, '(3f12.4)' ) z_base(k),u_base(k),v_base(k)
       CALL wrf_debug ( 100, message )
    enddo

    if ( scm_th_adv .or. scm_qv_adv .or. scm_wind_adv ) then
       do k = kms,kme-1

          u_x = interp_0(u_upstream_x,z_force,z(ids,k,jds),num_force_layers)
          u_y = interp_0(u_upstream_y,z_force,z(ids,k,jds),num_force_layers)

          v_x = interp_0(v_upstream_x,z_force,z(ids,k,jds),num_force_layers)
          v_y = interp_0(v_upstream_y,z_force,z(ids,k,jds),num_force_layers)

          tau_u(k) = dx/abs(u(ids,k,jds))
          tau_v(k) = dx/abs(v(ids,k,jds))

          if ( scm_wind_adv ) then
             u_adv_tend(k) = (u_x-u(ids,k,jds))/tau_u(k) + (u_y-u(ids,k,jds))/tau_v(k)
             v_adv_tend(k) = (v_x-v(ids,k,jds))/tau_u(k) + (v_y-v(ids,k,jds))/tau_v(k)
          endif

       enddo
    endif

    if ( scm_th_adv ) then
       do k = kms,kme-1
          t_x = interp_0(th_upstream_x,z_force,z(ids,k,jds),num_force_layers)
          t_y = interp_0(th_upstream_y,z_force,z(ids,k,jds),num_force_layers)

          th_adv_tend(k) = (t_x-th(ids,k,jds))/tau_u(k) + (t_y-th(ids,k,jds))/tau_v(k)
       enddo
    endif

    if ( scm_qv_adv ) then
       do k = kms,kme-1
          qv_x = interp_0(qv_upstream_x,z_force,z(ids,k,jds),num_force_layers)
          qv_y = interp_0(qv_upstream_y,z_force,z(ids,k,jds),num_force_layers)

          qv_adv_tend(k) = (qv_x-qv(ids,k,jds))/tau_u(k) + (qv_y-qv(ids,k,jds))/tau_v(k)
       enddo
    endif

! loops are set so that the top and bottom (w=0) are handled correctly
! vertical derivatives
    do k = kms+1,kme-1
       dthdz(k) = (th(2,k,2)-th(2,k-1,2))/(z(2,k,2)-z(2,k-1,2))
       dqvdz(k) = (qv(2,k,2)-qv(2,k-1,2))/(z(2,k,2)-z(2,k-1,2))
       dudz(k)  = (u(2,k,2)-u(2,k-1,2))/(z(2,k,2)-z(2,k-1,2))
       dvdz(k)  = (v(2,k,2)-v(2,k-1,2))/(z(2,k,2)-z(2,k-1,2))
    enddo

! w on full levels, then advect
    if ( scm_vert_adv ) then
       do k = kms+1,kme-1
          w = interp_0(w_subs,z_force,z_at_w(ids,k,jds),num_force_layers)
          w_dthdz(k) = w*dthdz(k)
          w_dqvdz(k) = w*dqvdz(k)
          w_dudz(k)  = w*dudz(k)
          w_dvdz(k)  = w*dvdz(k)
       enddo
    endif

! set tendencies for return
! vertical advection tendencies need to be interpolated back to half levels
    do j = jms,jme
    do k = kms,kme-1
    do i = ims,ime
       thten(i,k,j) = thten(i,k,j) + th_adv_tend(k) +              &
                      0.5*(w_dthdz(k) + w_dthdz(k+1))
       qvten(i,k,j) = qvten(i,k,j) + qv_adv_tend(k) +              &
                      0.5*(w_dqvdz(k) + w_dqvdz(k+1))
       uten(i,k,j)  = uten(i,k,j) + u_adv_tend(k) +                &
                      0.5*(w_dudz(k) + w_dudz(k+1))
       vten(i,k,j)  = vten(i,k,j) + v_adv_tend(k) +                &
                      0.5*(w_dvdz(k) + w_dvdz(k+1))
    enddo
    enddo
    enddo

    RETURN

   END SUBROUTINE force_scm

END MODULE module_force_scm
