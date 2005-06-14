!WRF:MODEL_LAYER:DYNAMICS
!

MODULE module_em

   USE module_model_constants
   USE module_advect_em
   USE module_big_step_utilities_em
   USE module_state_description

CONTAINS

!------------------------------------------------------------------------

SUBROUTINE rk_step_prep  ( config_flags, rk_step,           &
                           u, v, w, t, ph, mu,              &
                           moist,                           &
                           ru, rv, rw, ww, php, alt, muu, muv,  &
                           mub, mut, phb, pb, p, al, alb,   &
                           cqu, cqv, cqw,                   &
                           msfu, msfv, msft,                &
                           fnm, fnp, dnw, rdx, rdy,         &
                           n_moist,                         &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte    )

   IMPLICIT NONE


   !  Input data.

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte

   INTEGER ,       INTENT(IN   ) :: n_moist, rk_step

   REAL ,          INTENT(IN   ) :: rdx, rdy

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ) ,                      &
                                               INTENT(IN   ) ::  u,       &
                                                                 v,       &
                                                                 w,       &
                                                                 t,       &
                                                                 ph,      &
                                                                 phb,     &
                                                                 pb,      &
                                                                 al,      &
                                                                 alb

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme  ) ,                     &
                                               INTENT(  OUT) ::  ru,      &
                                                                 rv,      &
                                                                 rw,      &
                                                                 ww,      &
                                                                 php,     &
                                                                 cqu,     &
                                                                 cqv,     &
                                                                 cqw,     &
                                                                 alt

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ) ,                      &
                                               INTENT(IN   ) ::  p
                                                                 



   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ), INTENT(   IN) :: &
                                                           moist

   REAL , DIMENSION( ims:ime , jms:jme ) ,    INTENT(IN   ) :: msft,   &
                                                               msfu,   &
                                                               msfv,   &
                                                               mu,     &
                                                               mub

   REAL , DIMENSION( ims:ime , jms:jme ) ,    INTENT(  OUT) :: muu,    &
                                                               muv,    &
                                                               mut

   REAL , DIMENSION( kms:kme ) ,    INTENT(IN   ) :: fnm, fnp, dnw

   integer :: k


!<DESCRIPTION>
!
!  rk_step_prep prepares a number of diagnostic quantities 
!  in preperation for a Runge-Kutta timestep.  subroutines called
!  by rk_step_prep calculate
!
!  (1) total column dry air mass (mut, call to calculate_full)
!
!  (2) total column dry air mass at u and v points 
!      (muu, muv, call to calculate_mu_uv)
!
!  (3) mass-coupled velocities for advection
!      (ru, rv, and rw, call to couple_momentum)
!
!  (4) omega (call to calc_ww_cp)
!
!  (5) moisture coefficients (cqu, cqv, cqw, call to calc_cq)
!
!  (6) inverse density (alt, call to calc_alt)
!
!  (7) geopotential at pressure points (php, call to calc_php)
!
!</DESCRIPTION>

   CALL calculate_full( mut, mub, mu,             &
                        ids, ide, jds, jde, 1, 2, &
                        ims, ime, jms, jme, 1, 1, &
                        its, ite, jts, jte, 1, 1 )

   CALL calc_mu_uv ( config_flags,                  &
                     mu, mub, muu, muv,             &
                     ids, ide, jds, jde, kds, kde,  &
                     ims, ime, jms, jme, kms, kme,  &
                     its, ite, jts, jte, kts, kte  )

   CALL couple_momentum( muu, ru, u, msfu,              &
                         muv, rv, v, msfv,              &
                         mut, rw, w, msft,              &
                         ids, ide, jds, jde, kds, kde,  &
                         ims, ime, jms, jme, kms, kme,  &
                         its, ite, jts, jte, kts, kte  )

!  new call, couples V with mu, also has correct map factors.  WCS, 3 june 2001
   CALL calc_ww_cp ( u, v, mu, mub, ww,               &
                     rdx, rdy, msft, msfu, msfv, dnw, &
                     ids, ide, jds, jde, kds, kde,    &
                     ims, ime, jms, jme, kms, kme,    &
                     its, ite, jts, jte, kts, kte    )

   CALL calc_cq ( moist, cqu, cqv, cqw, n_moist, &
                  ids, ide, jds, jde, kds, kde,  &
                  ims, ime, jms, jme, kms, kme,  &
                  its, ite, jts, jte, kts, kte  )

   CALL calc_alt ( alt, al, alb,                 &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   its, ite, jts, jte, kts, kte )

   CALL calc_php ( php, ph, phb,                 &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   its, ite, jts, jte, kts, kte )

END SUBROUTINE rk_step_prep

!-------------------------------------------------------------------------------

SUBROUTINE rk_tendency ( config_flags, rk_step,                           &
                         ru_tend, rv_tend, rw_tend, ph_tend, t_tend,      &
                         ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                         mu_tend, u_save, v_save, w_save, ph_save,        &
                         t_save, mu_save, RTHFTEN,                        &
                         ru, rv, rw, ww,                                  &
                         u, v, w, t, ph,                                  &
                         u_old, v_old, w_old, t_old, ph_old,              &
                         h_diabatic, phb,t_init,                          &
                         mu, mut, muu, muv, mub,                          &
                         al, alt, p, pb, php, cqu, cqv, cqw,              &
                         u_base, v_base, t_base, qv_base, z_base,         &
                         msfu, msfv, msft, f, e, sina, cosa,              &
                         fnm, fnp, rdn, rdnw,                             &
                         dt, rdx, rdy, khdif, kvdif, xkmhd,               &
                         dampcoef,zdamp,damp_opt,                         &
                         cf1, cf2, cf3, cfn, cfn1, n_moist,               &
                         non_hydrostatic, leapfrog,                       &
                         ids, ide, jds, jde, kds, kde,                    &
                         ims, ime, jms, jme, kms, kme,                    &
                         its, ite, jts, jte, kts, kte                    )

   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type)    ,           INTENT(IN   ) :: config_flags

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   LOGICAL ,               INTENT(IN   ) :: non_hydrostatic, leapfrog

   INTEGER ,               INTENT(IN   ) :: n_moist, rk_step

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) ,              &
                                        INTENT(IN   ) :: ru,      &
                                                         rv,      &
                                                         rw,      &
                                                         ww,      & 
                                                         u,       &
                                                         v,       &
                                                         w,       &
                                                         t,       &
                                                         ph,      &
                                                         u_old,   &
                                                         v_old,   &
                                                         w_old,   &
                                                         t_old,   &
                                                         ph_old,  &
                                                         phb,     &
                                                         al,      &
                                                         alt,     &
                                                         p,       &
                                                         pb,      &
                                                         php,     &
                                                         cqu,     &
                                                         cqv,     &
                                                         t_init,  &
                                                         xkmhd,  &
                                                         h_diabatic

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) ,              &
                                        INTENT(OUT  ) :: ru_tend, &
                                                         rv_tend, &
                                                         rw_tend, &
                                                         t_tend,  &
                                                         ph_tend, &
                                                         RTHFTEN, &
                                                          u_save, &
                                                          v_save, &
                                                          w_save, &
                                                         ph_save, &
                                                          t_save

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) ,               &
                                        INTENT(INOUT) :: ru_tendf, &
                                                         rv_tendf, &
                                                         rw_tendf, &
                                                         t_tendf,  &
                                                         ph_tendf, &
                                                         cqw

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(  OUT) :: mu_tend, &
                                                                    mu_save

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfu,    &
                                                                    msfv,    &
                                                                    msft,    &
                                                                    f,       &
                                                                    e,       &
                                                                    sina,    &
                                                                    cosa,    &
                                                                    mu,      &
                                                                    mut,     &
                                                                    mub,     &
                                                                    muu,     &
                                                                    muv

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fnm,     &
                                                                  fnp,     &
                                                                  rdn,     &
                                                                  rdnw,    &
                                                                  u_base,  &
                                                                  v_base,  &
                                                                  t_base,  &
                                                                  qv_base, &
                                                                  z_base

   REAL ,                                      INTENT(IN   ) :: rdx,     &
                                                                rdy,     &
                                                                dt,      &
                                                                khdif,   &
                                                                kvdif

   INTEGER, INTENT( IN ) :: damp_opt

   REAL, INTENT( IN ) :: zdamp, dampcoef

   REAL    :: kdift, khdq, kvdq, cfn, cfn1, cf1, cf2, cf3
   INTEGER :: i,j,k

!<DESCRIPTION>
!
!  rk_tendency computes the large-timestep tendency terms in the 
!  momentum, thermodynamic (theta), and geopotential equations.  
!  These terms include:
!
!  (1) advection (for u, v, w, theta - calls to advect_u, advect_v,
!                 advect_w, and advact_scalar).
!
!  (2) geopotential equation terms (advection and "gw" - call to rhs_ph).
!
!  (3) buoyancy term in vertical momentum equation (call to pg_buoy_w).
!
!  (4) Coriolis and curvature terms in u,v,w momentum equations
!      (calls to subroutines coriolis, curvature)
!
!  (5) 3D diffusion on coordinate surfaces.
!
!</DESCRIPTION>

   CALL zero_tend ( ru_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   CALL zero_tend ( rv_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   CALL zero_tend ( rw_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   CALL zero_tend ( t_tend,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   CALL zero_tend ( ph_tend,                      &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   CALL zero_tend ( u_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   CALL zero_tend ( v_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   CALL zero_tend ( w_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   CALL zero_tend ( ph_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   CALL zero_tend ( t_save,                       &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   CALL zero_tend ( mu_tend,                  &
                    ids, ide, jds, jde, 1, 1, &
                    ims, ime, jms, jme, 1, 1, &
                    its, ite, jts, jte, 1, 1 )

   CALL zero_tend ( mu_save,                  &
                    ids, ide, jds, jde, 1, 1, &
                    ims, ime, jms, jme, 1, 1, &
                    its, ite, jts, jte, 1, 1 )

   IF (.not. leapfrog ) THEN

     !  advection tendencies

     CALL advect_u ( u, u , ru_tend, ru, rv, ww,   &
                     mut, config_flags,            &
                     msfu, msfv, msft,             &
                     fnm, fnp, rdx, rdy, rdnw,     &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )

     CALL advect_v ( v, v , rv_tend, ru, rv, ww,   &
                     mut, config_flags,            &
                     msfu, msfv, msft,             &
                     fnm, fnp, rdx, rdy, rdnw,     &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )

     IF (non_hydrostatic)                          &
     CALL advect_w ( w, w, rw_tend, ru, rv, ww,    &
                     mut, config_flags,            &
                     msfu, msfv, msft,             &
                     fnm, fnp, rdx, rdy, rdn,      &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )

!  theta flux divergence

     CALL advect_scalar ( t, t, t_tend, ru, rv, ww,     &
                          mut, config_flags,            &
                          msfu, msfv, msft, fnm, fnp,   &
                          rdx, rdy, rdnw,               &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte ) 

     IF ( config_flags%cu_physics == GDSCHEME ) THEN

     ! theta advection only:

         CALL set_tend( RTHFTEN, t_tend, msft,           &
                        ids, ide, jds, jde, kds, kde,    &
                        ims, ime, jms, jme, kms, kme,    &
                        its, ite, jts, jte, kts, kte     )

     END IF

     CALL rhs_ph( ph_tend, u, v, ww, ph, ph, phb, w, &
                  mut, muu, muv,                     &
                  fnm, fnp,                          &
                  rdnw, cfn, cfn1, rdx, rdy, msft,   &
                  non_hydrostatic,                   &
                  config_flags,                      &
                  ids, ide, jds, jde, kds, kde,      &
                  ims, ime, jms, jme, kms, kme,      &
                  its, ite, jts, jte, kts, kte      )

  ELSE  ! leapfrog option

    CALL advect_u ( u, u_old, ru_tend, ru, rv, ww, &
                    mut, config_flags,             &
                    msfu, msfv, msft,              &
                    fnm, fnp, rdx, rdy, rdnw,      &
                    ids, ide, jds, jde, kds, kde,  &
                    ims, ime, jms, jme, kms, kme,  &
                    its, ite, jts, jte, kts, kte  )

    CALL advect_v ( v, v_old, rv_tend, ru, rv, ww, &
                    mut, config_flags,             &
                    msfu, msfv, msft,              &
                    fnm, fnp, rdx, rdy, rdnw,      &
                    ids, ide, jds, jde, kds, kde,  &
                    ims, ime, jms, jme, kms, kme,  &
                    its, ite, jts, jte, kts, kte  )

    IF (non_hydrostatic)                           &
    CALL advect_w ( w, w_old, rw_tend, ru, rv, ww, &
                    mut, config_flags,             &
                    msfu, msfv, msft,              &
                    fnm, fnp, rdx, rdy, rdn,       &
                    ids, ide, jds, jde, kds, kde,  &
                    ims, ime, jms, jme, kms, kme,  &
                    its, ite, jts, jte, kts, kte  )

!  theta flux divergence

    CALL advect_scalar ( t, t_old, t_tend, ru, rv, ww, &
                         mut, config_flags,            &
                         msfu, msfv, msft, fnm, fnp,   &
                         rdx, rdy, rdnw,               &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte )

    CALL rhs_ph( ph_tend, u, v, ww, ph, ph_old, phb, w, &
                 mut, muu, muv,                         &
                 fnm, fnp,                              &
                 rdnw, cfn, cfn1, rdx, rdy, msft,       &
                 non_hydrostatic,                       &
                 config_flags,                          &
                 ids, ide, jds, jde, kds, kde,          &
                 ims, ime, jms, jme, kms, kme,          &
                 its, ite, jts, jte, kts, kte          )

  END IF

  CALL horizontal_pressure_gradient( ru_tend,rv_tend,                &
                                     ph,alt,p,pb,al,php,cqu,cqv,     &
                                     muu,muv,mu,fnm,fnp,rdnw,        &
                                     cf1,cf2,cf3,rdx,rdy,msft,       &
                                     config_flags, non_hydrostatic,  &
                                     ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte   )

  IF (non_hydrostatic)                            &
  CALL pg_buoy_w( rw_tend, p, cqw, mu, mub,       &
                  rdnw, rdn, g, msft,             &
                  ids, ide, jds, jde, kds, kde,   &
                  ims, ime, jms, jme, kms, kme,   &
                  its, ite, jts, jte, kts, kte   )

  CALL w_damp   ( rw_tend, ww, w, mut, rdnw, dt,    &
                    config_flags%w_damping,         &
                    ids, ide, jds, jde, kds, kde,   &
                    ims, ime, jms, jme, kms, kme,   &
                    its, ite, jts, jte, kts, kte   )

  IF(config_flags%pert_coriolis) THEN

    CALL perturbation_coriolis ( ru, rv, rw,                   &
                                 ru_tend,  rv_tend,  rw_tend,  &
                                 config_flags,                 &
                                 u_base, v_base, z_base,       &
                                 muu, muv, phb, ph,            &
                                 f, e, sina, cosa, fnm, fnp,   &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte )
  ELSE

    CALL coriolis ( ru, rv, rw,                   &
                    ru_tend,  rv_tend,  rw_tend,  &
                    config_flags,                 &
                    f, e, sina, cosa, fnm, fnp,   &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   END IF


  CALL curvature ( ru, rv, rw, u, v, w,            &
                   ru_tend,  rv_tend,  rw_tend,    &
                   config_flags,                   &
                   msfu, msfv, fnm, fnp, rdx, rdy, &
                   ids, ide, jds, jde, kds, kde,   &
                   ims, ime, jms, jme, kms, kme,   &
                   its, ite, jts, jte, kts, kte   )

!**************************************************************
!
!  Next, the terms that we integrate only with forward-in-time
!  (evaluate with time t variables).
!
!**************************************************************

  forward_step: IF( rk_step == 1 ) THEN

    diff_opt1 : IF (config_flags%diff_opt .eq. 1) THEN
   
      leapfrog_test : IF( .not. leapfrog ) THEN

        CALL horizontal_diffusion ('u', u, ru_tendf, mut, config_flags, &
                                        msfu, msfv, msft,               &
                                        khdif, xkmhd, rdx, rdy,         &
                                        ids, ide, jds, jde, kds, kde,   &
                                        ims, ime, jms, jme, kms, kme,   &
                                        its, ite, jts, jte, kts, kte   )

        CALL horizontal_diffusion ('v', v, rv_tendf, mut, config_flags, &
                                        msfu, msfv, msft,               &
                                        khdif, xkmhd, rdx, rdy,         &
                                        ids, ide, jds, jde, kds, kde,   &
                                        ims, ime, jms, jme, kms, kme,   &
                                        its, ite, jts, jte, kts, kte   )

        CALL horizontal_diffusion ('w', w, rw_tendf, mut, config_flags, &
                                        msfu, msfv, msft,               &
                                        khdif, xkmhd, rdx, rdy,         &
                                        ids, ide, jds, jde, kds, kde,   &
                                        ims, ime, jms, jme, kms, kme,   &
                                        its, ite, jts, jte, kts, kte   )

        khdq = 3.*khdif
        CALL horizontal_diffusion_3dmp ( 'm', t, t_tendf, mut,         &
                                         config_flags, t_init,         &
                                         msfu, msfv, msft,             &
                                         khdq , xkmhd, rdx, rdy,       &
                                         ids, ide, jds, jde, kds, kde, &
                                         ims, ime, jms, jme, kms, kme, &
                                         its, ite, jts, jte, kts, kte )

        pbl_test : IF (config_flags%bl_pbl_physics .eq. 0) THEN

          CALL vertical_diffusion_u ( u, ru_tendf, config_flags,      &
                                      u_base,                         &
                                      alt, muu, rdn, rdnw, kvdif,     &
                                      ids, ide, jds, jde, kds, kde,   &
                                      ims, ime, jms, jme, kms, kme,   &
                                      its, ite, jts, jte, kts, kte   )

          CALL vertical_diffusion_v ( v, rv_tendf, config_flags,      &
                                      v_base,                         &
                                      alt, muv, rdn, rdnw, kvdif,     &
                                      ids, ide, jds, jde, kds, kde,   &
                                      ims, ime, jms, jme, kms, kme,   &
                                      its, ite, jts, jte, kts, kte   )

          IF (non_hydrostatic)                                           &
          CALL vertical_diffusion ( 'w', w, rw_tendf, config_flags,      &
                                    alt, mut, rdn, rdnw, kvdif,          &
                                    ids, ide, jds, jde, kds, kde,        &
                                    ims, ime, jms, jme, kms, kme,        &
                                    its, ite, jts, jte, kts, kte        )

          kvdq = 3.*kvdif
          CALL vertical_diffusion_3dmp ( t, t_tendf, config_flags, t_init,     &
                                         alt, mut, rdn, rdnw, kvdq ,           &
                                         ids, ide, jds, jde, kds, kde,         &
                                         ims, ime, jms, jme, kms, kme,         &
                                         its, ite, jts, jte, kts, kte         )

        ENDIF pbl_test

   !  Theta tendency computations.

      ELSE ! leapfrog diffusion

        CALL horizontal_diffusion ('u', u_old, ru_tendf, mut, config_flags, &
                                        msfu, msfv, msft,                   &
                                        khdif, xkmhd, rdx, rdy,                    &
                                        ids, ide, jds, jde, kds, kde,       &
                                        ims, ime, jms, jme, kms, kme,       &
                                        its, ite, jts, jte, kts, kte       )

        CALL horizontal_diffusion ('v', v_old, rv_tendf, mut, config_flags, &
                                        msfu, msfv, msft,                   &
                                        khdif, xkmhd, rdx, rdy,                    &
                                        ids, ide, jds, jde, kds, kde,       &
                                        ims, ime, jms, jme, kms, kme,       &
                                        its, ite, jts, jte, kts, kte       )

        IF (non_hydrostatic)                                                &
        CALL horizontal_diffusion ('w', w_old, rw_tendf, mut, config_flags, &
                                        msfu, msfv, msft,                   &
                                        khdif, xkmhd, rdx, rdy,                    &
                                        ids, ide, jds, jde, kds, kde,       &
                                        ims, ime, jms, jme, kms, kme,       &
                                        its, ite, jts, jte, kts, kte       )

        khdq = 3.*khdif
        CALL horizontal_diffusion_3dmp ( 'm', t_old, t_tendf, mut,     &
                                         config_flags, t_init,         &
                                         msfu, msfv, msft,             &
                                         khdq , xkmhd, rdx, rdy,              &
                                         ids, ide, jds, jde, kds, kde, &
                                         ims, ime, jms, jme, kms, kme, &
                                         its, ite, jts, jte, kts, kte )

        pbl_test_lf : IF (config_flags%bl_pbl_physics .eq. 0) THEN

          CALL vertical_diffusion_u ( u_old, ru_tendf, config_flags,      &
                                      u_base,                             &
                                      alt, muu, rdn, rdnw, kvdif,         &
                                      ids, ide, jds, jde, kds, kde,       &
                                      ims, ime, jms, jme, kms, kme,       &
                                      its, ite, jts, jte, kts, kte       )

          CALL vertical_diffusion_v ( v_old, rv_tendf, config_flags,      &
                                      v_base,                             &
                                      alt, muv, rdn, rdnw, kvdif,         &
                                      ids, ide, jds, jde, kds, kde,       &
                                      ims, ime, jms, jme, kms, kme,       &
                                      its, ite, jts, jte, kts, kte       )

          IF (non_hydrostatic)                                              &
          CALL vertical_diffusion ( 'w', w_old, rw_tendf, config_flags,     &
                                    alt, mut, rdn, rdnw, kvdif,             &
                                    ids, ide, jds, jde, kds, kde,           &
                                    ims, ime, jms, jme, kms, kme,           &
                                    its, ite, jts, jte, kts, kte           )

          kvdq = 3.*kvdif
          CALL vertical_diffusion_3dmp ( t_old, t_tendf, config_flags, t_init, &
                                         alt, mut, rdn, rdnw, kvdq ,           &
                                         ids, ide, jds, jde, kds, kde,         &
                                         ims, ime, jms, jme, kms, kme,         &
                                         its, ite, jts, jte, kts, kte         )
        ENDIF pbl_test_lf

      END IF leapfrog_test
        
    END IF diff_opt1

    IF( damp_opt .eq. 2 )                                      &
       CALL rk_rayleigh_damp( ru_tendf, rv_tendf,              &
                              rw_tendf, t_tendf,               &
                              u, v, w, t, t_init,              &
                              mut, muu, muv, ph, phb,          &
                              u_base, v_base, t_base, z_base,  &
                              dampcoef, zdamp,                 &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              its, ite, jts, jte, kts, kte   )

  END IF forward_step

END SUBROUTINE rk_tendency

!-------------------------------------------------------------------------------

SUBROUTINE rk_addtend_dry ( ru_tend, rv_tend, rw_tend, ph_tend, t_tend,      &
                            ru_tendf, rv_tendf, rw_tendf, ph_tendf, t_tendf, &
                            u_save, v_save, w_save, ph_save, t_save, rk_step,&
                            h_diabatic, mut, msft, msfu, msfv,               &
                            ids,ide, jds,jde, kds,kde,                       &
                            ims,ime, jms,jme, kms,kme,                       &
                            ips,ipe, jps,jpe, kps,kpe,                       &
                            its,ite, jts,jte, kts,kte                       )

   IMPLICIT NONE

   !  Input data.

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, &
                                            its, ite, jts, jte, kts, kte
   INTEGER ,               INTENT(IN   ) :: rk_step

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(INOUT) :: ru_tend, &
                                                                      rv_tend, &
                                                                      rw_tend, &
                                                                      ph_tend, &
                                                                      t_tend,  &
                                                                      ru_tendf, &
                                                                      rv_tendf, &
                                                                      rw_tendf, &
                                                                      ph_tendf, &
                                                                      t_tendf

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(IN   ) ::  u_save,  &
                                                                       v_save,  &
                                                                       w_save,  &
                                                                      ph_save,  &
                                                                       t_save,  &
                                                                      h_diabatic

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: mut, &
                                                                    msft, &
                                                                    msfu, &
                                                                    msfv


! Local
   INTEGER :: i, j, k


!<DESCRIPTION>
!
! rk_addtend_dry constructs the full large-timestep tendency terms for
! momentum (u,v,w), theta and geopotential equations.   This is accomplished
! by combining the physics tendencies (in *tendf; these are computed 
! the first RK substep, held fixed thereafter) with the RK tendencies 
! (in *tend, these include advection, pressure gradient, etc; 
! these change each rk substep).  Output is in *tend.
!
!</DESCRIPTION>

!  Finally, add the forward-step tendency to the rk_tendency

! u/v/w/save contain bc tendency that needs to be multiplied by msf
!  before adding it to physics tendency (*tendf)
! For momentum we need the final tendency to include an inverse msf
! physics/bc tendency needs to be divided, advection tendency already has it

! For scalars we need the final tendency to include an inverse msf
! advection tendency is OK, physics/bc tendency needs to be divided by msf

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,ite
     IF(rk_step == 1)ru_tendf(i,k,j) = ru_tendf(i,k,j) +  u_save(i,k,j)*msfu(i,j)
     ru_tend(i,k,j) = ru_tend(i,k,j) + ru_tendf(i,k,j)/msfu(i,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,jte
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     IF(rk_step == 1)rv_tendf(i,k,j) = rv_tendf(i,k,j) +  v_save(i,k,j)*msfv(i,j)
     rv_tend(i,k,j) = rv_tend(i,k,j) + rv_tendf(i,k,j)/msfv(i,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     IF(rk_step == 1)rw_tendf(i,k,j) = rw_tendf(i,k,j) +  w_save(i,k,j)*msft(i,j)
     rw_tend(i,k,j) = rw_tend(i,k,j) + rw_tendf(i,k,j)/msft(i,j)
     IF(rk_step == 1)ph_tendf(i,k,j) = ph_tendf(i,k,j) +  ph_save(i,k,j)
     ph_tend(i,k,j) = ph_tend(i,k,j) + ph_tendf(i,k,j)/msft(i,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     IF(rk_step == 1)t_tendf(i,k,j) = t_tendf(i,k,j) +  t_save(i,k,j)
      t_tend(i,k,j) =  t_tend(i,k,j) +  t_tendf(i,k,j)/msft(i,j)  &
                                     +  mut(i,j)*h_diabatic(i,k,j)/msft(i,j)
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE rk_addtend_dry

!-------------------------------------------------------------------------------

SUBROUTINE rk_scalar_tend ( scs, sce, config_flags,    &
                            rk_step, dt,                  &
                            ru, rv, ww, mut, alt,         &
                            scalar,                       &
                            scalar_tends, advect_tend,    &
                            RQVFTEN,                      &
                            base, moist_step, fnm, fnp,   &
                            msfu, msfv, msft,             &
                            rdx, rdy, rdn, rdnw,          &
                            khdif, kvdif, xkmhd,          &
                            leapfrog,                     &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte )

   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,                INTENT(IN   ) :: rk_step, scs, sce
   INTEGER ,                INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                             ims, ime, jms, jme, kms, kme, &
                                             its, ite, jts, jte, kts, kte

   LOGICAL , INTENT(IN   ) :: moist_step

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce ),                &
                                         INTENT(INOUT)  :: scalar

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce ),                      &
                                         INTENT(  OUT)  :: scalar_tends
                                                    
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme  ) :: advect_tend

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme  ), INTENT(OUT  ) :: RQVFTEN

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme  ), INTENT(IN   ) ::     ru,  &
                                                                      rv,  &
                                                                      ww,  &
                                                                      xkmhd,  &
                                                                      alt


   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fnm,  &
                                                                  fnp,  &
                                                                  rdn,  &
                                                                  rdnw, &
                                                                  base

   REAL , DIMENSION( ims:ime , jms:jme ) ,       INTENT(IN   ) :: msfu,    &
                                                                  msfv,    &
                                                                  msft,    &
                                                                  mut


   REAL ,                                        INTENT(IN   ) :: rdx,     &
                                                                  rdy,     &
                                                                  khdif,   &
                                                                  kvdif

   REAL ,                                        INTENT(IN   ) :: dt

   LOGICAL, INTENT(IN   ) :: leapfrog


   ! Local data
  
   INTEGER :: im, i,j,k

   REAL    :: khdq, kvdq, tendency

!<DESCRIPTION>
!
! rk_scalar_tend calls routines that computes scalar tendency from advection 
! and 3D mixing (TKE or fixed eddy viscosities).
!
!</DESCRIPTION>


   khdq = khdif/prandtl
   kvdq = kvdif/prandtl

   scalar_loop : DO im = scs, sce

     CALL zero_tend ( advect_tend(ims,kms,jms),     &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte )

     CALL advect_scalar     ( scalar(ims,kms,jms,im),        &
                              scalar(ims,kms,jms,im),        &
                              advect_tend(ims,kms,jms),      &
                              ru, rv, ww, mut, config_flags, &
                              msfu, msfv, msft, fnm, fnp,    &
                              rdx, rdy, rdnw,                &
                              ids, ide, jds, jde, kds, kde,  &
                              ims, ime, jms, jme, kms, kme,  &
                              its, ite, jts, jte, kts, kte  )

     IF( config_flags%cu_physics == GDSCHEME .and. moist_step .and. ( im == P_QV) ) THEN

        CALL set_tend( RQVFTEN, advect_tend, msft,     &
                       ids, ide, jds, jde, kds, kde,   &
                       ims, ime, jms, jme, kms, kme,   &
                       its, ite, jts, jte, kts, kte      )
     ENDIF

     diff_opt1 : IF (config_flags%diff_opt .eq. 1) THEN

       rk_step_1: IF( rk_step == 1 ) THEN


       CALL horizontal_diffusion ( 'm', scalar(ims,kms,jms,im),            &
                                        scalar_tends(ims,kms,jms,im), mut, &
                                        config_flags,                      &
                                        msfu, msfv, msft, khdq , xkmhd, rdx, rdy, &
                                        ids, ide, jds, jde, kds, kde,      &
                                        ims, ime, jms, jme, kms, kme,      &
                                        its, ite, jts, jte, kts, kte      )

       pbl_test : IF (config_flags%bl_pbl_physics .eq. 0) THEN

         IF( (moist_step) .and. ( im == P_QV)) THEN

            CALL vertical_diffusion_mp ( scalar(ims,kms,jms,im),       &
                                         scalar_tends(ims,kms,jms,im), &
                                         config_flags, base,           &
                                         alt, mut, rdn, rdnw, kvdq ,   &
                                         ids, ide, jds, jde, kds, kde, &
                                         ims, ime, jms, jme, kms, kme, &
                                         its, ite, jts, jte, kts, kte )

         ELSE 

            CALL vertical_diffusion (  'm', scalar(ims,kms,jms,im),       &
                                            scalar_tends(ims,kms,jms,im), &
                                            config_flags,                 &
                                            alt, mut, rdn, rdnw, kvdq,    &
                                            ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte )

         END IF

      ENDIF pbl_test

    ENDIF rk_step_1

  ENDIF diff_opt1

 END DO scalar_loop

END SUBROUTINE rk_scalar_tend

!-------------------------------------------------------------------------------

SUBROUTINE rk_update_scalar( scs, sce,                      &
                             scalar_1, scalar_2, sc_tend,   &
                             advect_tend, msft,             &
                             mu_old, mu_new, mu_base,       &
                             rk_step, dt, spec_zone,        &
                             epsts, leapfrog, config_flags, &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,                INTENT(IN   ) :: scs, sce, rk_step, spec_zone
   INTEGER ,                INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                             ims, ime, jms, jme, kms, kme, &
                                             its, ite, jts, jte, kts, kte

   REAL,                    INTENT(IN   ) :: dt, epsts

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce),                &
         INTENT(INOUT)                                  :: scalar_1,    &
                                                           scalar_2,  &
                                                           sc_tend

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ),                &
         INTENT(IN)                                  :: advect_tend

   REAL, DIMENSION(ims:ime, jms:jme  ), INTENT(IN   ) ::  mu_old,  &
                                                          mu_new,  &
                                                          mu_base, &
                                                          msft

   LOGICAL, INTENT(IN   ) :: leapfrog

   INTEGER :: i,j,k,im
   REAL    :: sc_middle, msfsq
   REAL, DIMENSION(its:ite) :: muold, r_munew

   REAL, DIMENSION(its:ite, kts:kte, jts:jte  ) :: tendency

   INTEGER :: i_start,i_end,j_start,j_end,k_start,k_end
   INTEGER :: i_start_spc,i_end_spc,j_start_spc,j_end_spc,k_start_spc,k_end_spc

!<DESCRIPTION>
!
!  rk_scalar_update advances the scalar equation given the time t value
!  of the scalar and the scalar tendency.  
!
!</DESCRIPTION>

!
!  set loop limits.

      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = jte
      k_start = kts
      k_end   = kte-1
      IF(j_end == jde) j_end = j_end - 1
      IF(i_end == ide) i_end = i_end - 1

      i_start_spc = i_start
      i_end_spc   = i_end
      j_start_spc = j_start
      j_end_spc   = j_end
      k_start_spc = k_start
      k_end_spc   = k_end

    IF( config_flags%nested .or. config_flags%specified ) THEN
      i_start = max( its,ids+spec_zone )
      i_end   = min( ite,ide-spec_zone-1 )
      j_start = max( jts,jds+spec_zone )
      j_end   = min( jte,jde-spec_zone-1 )
      k_start = kts
      k_end   = min( kte, kde-1 )
    ENDIF

    IF ( rk_step == 1 ) THEN

      !  replace t-dt values (in scalar_1) with t values scalar_2,
      !  then compute new values by adding tendency to values at t

      DO  im = scs,sce

       DO  j = jts, min(jte,jde-1)
       DO  k = kts, min(kte,kde-1)
       DO  i = its, min(ite,ide-1)
           tendency(i,k,j) = 0.
       ENDDO
       ENDDO
       ENDDO
   
       DO  j = j_start,j_end
       DO  k = k_start,k_end
       DO  i = i_start,i_end
           tendency(i,k,j) = advect_tend(i,k,j) * msft(i,j)
       ENDDO
       ENDDO
       ENDDO
   
       DO  j = j_start_spc,j_end_spc
       DO  k = k_start_spc,k_end_spc
       DO  i = i_start_spc,i_end_spc
           tendency(i,k,j) = tendency(i,k,j) + sc_tend(i,k,j,im)
       ENDDO
       ENDDO
       ENDDO
   
      DO  j = jts, min(jte,jde-1)

      DO  i = its, min(ite,ide-1)
        muold(i) = mu_old(i,j) + mu_base(i,j)
        r_munew(i) = 1./(mu_new(i,j) + mu_base(i,j))
      ENDDO

      DO  k = kts, min(kte,kde-1)
      DO  i = its, min(ite,ide-1)

        scalar_1(i,k,j,im) = scalar_2(i,k,j,im)
        scalar_2(i,k,j,im) = (muold(i)*scalar_1(i,k,j,im)   &
                             + dt*tendency(i,k,j))*r_munew(i)

      ENDDO
      ENDDO
      ENDDO

      ENDDO

    ELSE

      !  just compute new values, scalar_1 already at time t.

      DO  im = scs, sce

       DO  j = jts, min(jte,jde-1)
       DO  k = kts, min(kte,kde-1)
       DO  i = its, min(ite,ide-1)
           tendency(i,k,j) = 0.
       ENDDO
       ENDDO
       ENDDO
   
       DO  j = j_start,j_end
       DO  k = k_start,k_end
       DO  i = i_start,i_end
           tendency(i,k,j) = advect_tend(i,k,j) * msft(i,j)
       ENDDO
       ENDDO
       ENDDO
   
       DO  j = j_start_spc,j_end_spc
       DO  k = k_start_spc,k_end_spc
       DO  i = i_start_spc,i_end_spc
           tendency(i,k,j) = tendency(i,k,j) + sc_tend(i,k,j,im)
       ENDDO
       ENDDO
       ENDDO

      DO  j = jts, min(jte,jde-1)

      DO  i = its, min(ite,ide-1)
        muold(i) = mu_old(i,j) + mu_base(i,j)
        r_munew(i) = 1./(mu_new(i,j) + mu_base(i,j))
      ENDDO

      DO  k = kts, min(kte,kde-1)
      DO  i = its, min(ite,ide-1)

        scalar_2(i,k,j,im) = (muold(i)*scalar_1(i,k,j,im)   &
                             + dt*tendency(i,k,j))*r_munew(i)

      ENDDO
      ENDDO
      ENDDO

      ENDDO

    END IF

END SUBROUTINE rk_update_scalar

!------------------------------------------------------------

SUBROUTINE time_filter  ( u_1, u_2, u_save,             &
                          v_1, v_2, v_save,             &
                          w_1, w_2, w_save,             &
                          t_1, t_2, t_save,             &
                          ph_1, ph_2, ph_save,          &
                          mu_1, mu_2, mu_save,          &
                          epsts,                        & 
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

   IMPLICIT NONE

   !  Input data.


   INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ) ,                        &
                                               INTENT(INOUT) ::  u_1,       &
                                                                 v_1,       &
                                                                 w_1,       &
                                                                 t_1,       &
                                                                 ph_1,      &
                                                                 u_2,       &
                                                                 v_2,       &
                                                                 w_2,       &
                                                                 t_2,       &
                                                                 ph_2


   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ) ,                           &
                                               INTENT(IN   ) ::  u_save,       &
                                                                 v_save,       &
                                                                 w_save,       &
                                                                 t_save,       &
                                                                 ph_save


   REAL , DIMENSION(  ims:ime , jms:jme ) ,                                  &
                                               INTENT(INOUT) ::  mu_1,       &
                                                                 mu_2

   REAL , DIMENSION(  ims:ime , jms:jme ) ,                                  &
                                               INTENT(IN   ) ::  mu_save

   REAL , INTENT(IN   )  :: epsts

   INTEGER :: i,j,k

!<DESCRIPTION>
!
!  Asselin timefilter for momentum, theta, geopotential, and mu.
!  Used with leapfrog option.
!
!</DESCRIPTION>

      DO  j = jts, min(jte,jde-1)
      DO  k = kts, min(kte,kde-1)
      DO  i = its, ite

      u_1(i,k,j) = u_save(i,k,j) + epsts*( u_1(i,k,j)       &
                                          -2*u_save(i,k,j)  &
                                           +u_2(i,k,j)      )
      ENDDO
      ENDDO
      ENDDO

      DO  j = jts, jde
      DO  k = kts, min(kte,kde-1)
      DO  i = its, min(ite,ide-1)

      v_1(i,k,j) = v_save(i,k,j) + epsts*( v_1(i,k,j)       &
                                          -2*v_save(i,k,j)  &
                                          +v_2(i,k,j)      )
      ENDDO
      ENDDO
      ENDDO

      DO  j = jts, min(jte,jde-1)
      DO  k = kts, kte
      DO  i = its, min(ite,ide-1)

      w_1(i,k,j) = w_save(i,k,j) + epsts*( w_1(i,k,j)       &
                                          -2*w_save(i,k,j)  &
                                          +w_2(i,k,j)      )
      ph_1(i,k,j) = ph_save(i,k,j) + epsts*( ph_1(i,k,j)     &
                                          -2*ph_save(i,k,j)  &
                                          +ph_2(i,k,j)      )
      ENDDO
      ENDDO
      ENDDO

      DO  j = jts, min(jte,jde-1)
      DO  k = kts, min(kte,kde-1)
      DO  i = its, min(ite,ide-1)

      t_1(i,k,j) = t_save(i,k,j) + epsts*( t_1(i,k,j)       &
                                          -2*t_save(i,k,j)  &
                                          +t_2(i,k,j)      )

      ENDDO
      ENDDO
      ENDDO

      DO  j = jts, min(jte,jde-1)
      DO  i = its, min(ite,ide-1)

      mu_1(i,j) = mu_save(i,j) + epsts*( mu_1(i,j)       &
                                          -2*mu_save(i,j)  &
                                          +mu_2(i,j)      )

      ENDDO
      ENDDO

END SUBROUTINE time_filter

!-----------------------------------------------------------------------

SUBROUTINE init_zero_tendency(ru_tendf, rv_tendf, rw_tendf, ph_tendf,  &
                              t_tendf,  tke_tendf,                     &
                              moist_tendf,chem_tendf,scalar_tendf,     &
                              n_moist,n_chem,n_scalar,rk_step,         &
                              ids, ide, jds, jde, kds, kde,            &
                              ims, ime, jms, jme, kms, kme,            &
                              its, ite, jts, jte, kts, kte             )
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------

   INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte

   INTEGER ,       INTENT(IN   ) :: n_moist,n_chem,n_scalar,rk_step

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(INOUT) ::  &
                                                             ru_tendf, &
                                                             rv_tendf, &
                                                             rw_tendf, &
                                                             ph_tendf, &
                                                              t_tendf, &
                                                            tke_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),INTENT(INOUT)::&
                                                          moist_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_chem ),INTENT(INOUT)::&
                                                          chem_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar ),INTENT(INOUT)::&
                                                          scalar_tendf

! LOCAL VARS

   INTEGER :: im, ic, is

!<DESCRIPTION>
!
! init_zero_tendency 
! sets tendency arrays to zero for all prognostic variables.
!
!</DESCRIPTION>


   CALL zero_tend ( ru_tendf,                        &
                    ids, ide, jds, jde, kds, kde,    &
                    ims, ime, jms, jme, kms, kme,    &
                    its, ite, jts, jte, kts, kte     )

   CALL zero_tend ( rv_tendf,                        &
                    ids, ide, jds, jde, kds, kde,    &
                    ims, ime, jms, jme, kms, kme,    &
                    its, ite, jts, jte, kts, kte     )

   CALL zero_tend ( rw_tendf,                        &
                    ids, ide, jds, jde, kds, kde,    &
                    ims, ime, jms, jme, kms, kme,    &
                    its, ite, jts, jte, kts, kte     )

   CALL zero_tend ( ph_tendf,                        &
                    ids, ide, jds, jde, kds, kde,    &
                    ims, ime, jms, jme, kms, kme,    &
                    its, ite, jts, jte, kts, kte     )

   CALL zero_tend ( t_tendf,                         &
                    ids, ide, jds, jde, kds, kde,    &
                    ims, ime, jms, jme, kms, kme,    &
                    its, ite, jts, jte, kts, kte     )

   CALL zero_tend ( tke_tendf,                       &
                    ids, ide, jds, jde, kds, kde,    &
                    ims, ime, jms, jme, kms, kme,    &
                    its, ite, jts, jte, kts, kte     )

!   DO im=PARAM_FIRST_SCALAR,n_moist
   DO im=1,n_moist                      ! make sure first one is zero too
      CALL zero_tend ( moist_tendf(ims,kms,jms,im),  &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte  )
   ENDDO

!   DO ic=PARAM_FIRST_SCALAR,n_chem
   DO ic=1,n_chem                       ! make sure first one is zero too
      CALL zero_tend ( chem_tendf(ims,kms,jms,ic),   &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte  )
   ENDDO

!   DO ic=PARAM_FIRST_SCALAR,n_scalar
   DO ic=1,n_scalar                       ! make sure first one is zero too
      CALL zero_tend ( scalar_tendf(ims,kms,jms,ic),   &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte  )
   ENDDO

END SUBROUTINE init_zero_tendency

!===================================================================


SUBROUTINE dump_data( a, field, io_unit,            &
                      ims, ime, jms, jme, kms, kme, &
                      ids, ide, jds, jde, kds, kde )
implicit none
integer ::  ims, ime, jms, jme, kms, kme, &
            ids, ide, jds, jde, kds, kde 
real, dimension(ims:ime, kms:kme, jds:jde) :: a
character :: field
integer :: io_unit

integer :: is,ie,js,je,ks,ke

!<DESCRIPTION
!
! quick and dirty debug io utility
!
!</DESCRIPTION

is = ids
ie = ide-1
js = jds
je = jde-1
ks = kds
ke = kde-1

if(field == 'u') ie = ide
if(field == 'v') je = jde
if(field == 'w') ke = kde

write(io_unit) is,ie,ks,ke,js,je
write(io_unit) a(is:ie, ks:ke, js:je)

end subroutine dump_data

!-----------------------------------------------------------------------

SUBROUTINE calculate_phy_tend (config_flags,mu,pi3d,                   &
                     RTHRATEN,                                         &
                     RUBLTEN,RVBLTEN,RTHBLTEN,                         &
                     RQVBLTEN,RQCBLTEN,RQIBLTEN,                       &
                     RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,              &
                     RQICUTEN,RQSCUTEN,                                &
                     ids,ide, jds,jde, kds,kde,                        &
                     ims,ime, jms,jme, kms,kme,                        &
                     its,ite, jts,jte, kts,kte                         )
!-----------------------------------------------------------------------
      IMPLICIT NONE

      TYPE(grid_config_rec_type), INTENT(IN)     ::      config_flags

      INTEGER,  INTENT(IN   )   ::          ids,ide, jds,jde, kds,kde, &
                                            ims,ime, jms,jme, kms,kme, &
                                            its,ite, jts,jte, kts,kte

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )               , &
                INTENT(IN   )   ::                               pi3d
                                                                 
      REAL,     DIMENSION( ims:ime, jms:jme )                        , &
                INTENT(IN   )   ::                                 mu
      
                                                           
! radiation

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ),                &
                INTENT(INOUT)   ::                           RTHRATEN

! cumulus

      REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ),              &
                INTENT(INOUT)   ::                                     &
                                                             RTHCUTEN, &
                                                             RQVCUTEN, &
                                                             RQCCUTEN, &
                                                             RQRCUTEN, &
                                                             RQICUTEN, &
                                                             RQSCUTEN
! pbl

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )               , &
                INTENT(INOUT)   ::                            RUBLTEN, &
                                                              RVBLTEN, &
                                                             RTHBLTEN, &
                                                             RQVBLTEN, &
                                                             RQCBLTEN, &
                                                             RQIBLTEN

      INTEGER :: i,k,j
      INTEGER :: itf,ktf,jtf

!-----------------------------------------------------------------------

!<DESCRIPTION>
!
!  calculate_phy_tend couples the physics tendencies to the column mass (mu),
!  because prognostic equations are in flux form, but physics tendencies are
!  computed for uncoupled variables.
!
!</DESCRIPTION>

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      ktf=MIN(kte,kde-1)

! radiation

   IF (config_flags%ra_lw_physics .gt. 0 .or. config_flags%ra_sw_physics .gt. 0) THEN

      DO J=jts,jtf
      DO K=kts,ktf
      DO I=its,itf
         RTHRATEN(I,K,J)=mu(I,J)*RTHRATEN(I,K,J)
      ENDDO
      ENDDO
      ENDDO

   ENDIF

! cumulus

   IF (config_flags%cu_physics .gt. 0) THEN

      DO J=jts,jtf
      DO I=its,itf
      DO K=kts,ktf
         RTHCUTEN(I,K,J)=mu(I,J)*RTHCUTEN(I,K,J)
         RQVCUTEN(I,K,J)=mu(I,J)*RQVCUTEN(I,K,J)
      ENDDO
      ENDDO
      ENDDO

      IF (P_QC .ge. PARAM_FIRST_SCALAR)THEN
         DO J=jts,jtf
         DO I=its,itf
         DO K=kts,ktf
            RQCCUTEN(I,K,J)=mu(I,J)*RQCCUTEN(I,K,J)
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QR .ge. PARAM_FIRST_SCALAR)THEN
         DO J=jts,jtf
         DO I=its,itf
         DO K=kts,ktf
            RQRCUTEN(I,K,J)=mu(I,J)*RQRCUTEN(I,K,J)
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QI .ge. PARAM_FIRST_SCALAR)THEN
         DO J=jts,jtf
         DO I=its,itf
         DO K=kts,ktf
            RQICUTEN(I,K,J)=mu(I,J)*RQICUTEN(I,K,J)
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF(P_QS .ge. PARAM_FIRST_SCALAR)THEN
         DO J=jts,jtf
         DO I=its,itf
         DO K=kts,ktf
            RQSCUTEN(I,K,J)=mu(I,J)*RQSCUTEN(I,K,J)
         ENDDO
         ENDDO
         ENDDO
      ENDIF

   ENDIF

! pbl

   IF (config_flags%bl_pbl_physics .gt. 0) THEN

      DO J=jts,jtf
      DO K=kts,ktf
      DO I=its,itf
         RUBLTEN(I,K,J) =mu(I,J)*RUBLTEN(I,K,J)
         RVBLTEN(I,K,J) =mu(I,J)*RVBLTEN(I,K,J)
         RTHBLTEN(I,K,J)=mu(I,J)*RTHBLTEN(I,K,J)
      ENDDO
      ENDDO
      ENDDO

      IF (P_QV .ge. PARAM_FIRST_SCALAR) THEN
         DO J=jts,jtf
         DO K=kts,ktf
         DO I=its,itf
            RQVBLTEN(I,K,J)=mu(I,J)*RQVBLTEN(I,K,J)
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QC .ge. PARAM_FIRST_SCALAR) THEN
         DO J=jts,jtf
         DO K=kts,ktf
         DO I=its,itf
           RQCBLTEN(I,K,J)=mu(I,J)*RQCBLTEN(I,K,J)
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QI .ge. PARAM_FIRST_SCALAR) THEN
         DO J=jts,jtf
         DO K=kts,ktf
         DO I=its,itf
            RQIBLTEN(I,K,J)=mu(I,J)*RQIBLTEN(I,K,J)
         ENDDO
         ENDDO
         ENDDO
      ENDIF

    ENDIF

END SUBROUTINE calculate_phy_tend

!-----------------------------------------------------------------------

SUBROUTINE positive_definite_filter ( a,                          &
                                      ids,ide, jds,jde, kds,kde,  &
                                      ims,ime, jms,jme, kms,kme,  &
                                      its,ite, jts,jte, kts,kte  )

  IMPLICIT NONE

  INTEGER,  INTENT(IN   )   ::          ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme  ), INTENT(INOUT) :: a

  INTEGER :: i,k,j

!<DESCRIPTION>
!
! debug and testing code for bounding a variable
!
!</DESCRIPTION>

  DO j=jts,min(jte,jde-1)
  DO k=kts,kte-1
  DO i=its,min(ite,ide-1)
!    a(i,k,j) = max(a(i,k,j),0.)
    a(i,k,j) = min(1000.,max(a(i,k,j),0.))
  ENDDO
  ENDDO
  ENDDO

  END SUBROUTINE positive_definite_filter

!-----------------------------------------------------------------------

SUBROUTINE bound_tke ( tke, tke_upper_bound,       &
                       ids,ide, jds,jde, kds,kde,  &
                       ims,ime, jms,jme, kms,kme,  &
                       its,ite, jts,jte, kts,kte  )

  IMPLICIT NONE

  INTEGER,  INTENT(IN   )   ::          ids,ide, jds,jde, kds,kde, &
                                        ims,ime, jms,jme, kms,kme, &
                                        its,ite, jts,jte, kts,kte

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme  ), INTENT(INOUT) :: tke
  REAL, INTENT(   IN) :: tke_upper_bound

  INTEGER :: i,k,j

!<DESCRIPTION>
!
! bounds tke between zero and tke_upper_bound.
!
!</DESCRIPTION>

  DO j=jts,min(jte,jde-1)
  DO k=kts,kte-1
  DO i=its,min(ite,ide-1)
    tke(i,k,j) = min(tke_upper_bound,max(tke(i,k,j),0.))
  ENDDO
  ENDDO
  ENDDO

  END SUBROUTINE bound_tke



END MODULE module_em
