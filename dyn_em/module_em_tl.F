!WRF+/TL:MODEL_LAYER:DYNAMICS
!

MODULE g_module_em

   USE module_model_constants
   USE module_advect_em
   USE module_big_step_utilities_em
   USE module_state_description
   USE module_damping_em
   USE g_module_big_step_utilities_em
   USE g_module_advect_em

CONTAINS

!------------------------------------------------------------------------

!SUBROUTINE! g_rk_step_prep ( config_flags, rk_step,           &
!                            u, g_u, v, g_v, w, g_w, t, ph, mu, g_mu,    &
!                            moist,                           &
!                            ru, g_ru, rv, g_rv, rw, g_rw, ww, php, alt, &
!                            muu, g_muu, muv, g_muv,          &
!                            mub, mut, g_mut, phb, pb, p, al, alb,  &
!                            cqu, cqv, cqw,                   &
!                            msfux, msfuy,                    &
!                            msfvx, msfvx_inv, msfvy,         &
!                            msftx, msfty,                    &
!                            fnm, fnp, dnw, rdx, rdy,         &
!                            n_moist,                         &
!                            ids, ide, jds, jde, kds, kde,    &
!                            ims, ime, jms, jme, kms, kme,    &
!                            its, ite, jts, jte, kts, kte )

!   IMPLICIT NONE

!   !  Input data.
!   TYPE(grid_config_rec_type), INTENT(IN) :: config_flags
!   INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
!                          ims, ime, jms, jme, kms, kme, &
!                          its, ite, jts, jte, kts, kte
!   INTEGER, INTENT(IN) :: n_moist, rk_step
!   REAL,    INTENT(IN) :: rdx, rdy

!   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: g_u,       &
!                                                             g_v,       &
!                                                             g_w
!   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: u,       &
!                                                             v,       &
!                                                             w,       &
!                                                             t,       &
!                                                             p,       &
!                                                             ph,      &
!                                                             phb,     &
!                                                             pb,      &
!                                                             al,      &
!                                                             alb

!   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT) :: g_ru,      &
!                                                              g_rv,      &
!                                                              g_rw
!   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT) :: ru,      &
!                                                              rv,      &
!                                                              rw,      &
!                                                              ww,      &
!                                                              php,     &
!                                                              cqu,     &
!                                                              cqv,     &
!                                                              cqw,     &
!                                                              alt

!   REAL, DIMENSION(ims:ime, kms:kme, jms:jme, n_moist), INTENT(IN) :: moist
!   REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: g_mu
!   REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: msftx,     &
!                                                    msfty,     &
!                                                    msfux,     &
!                                                    msfuy,     &
!                                                    msfvx,     &
!                                                    msfvx_inv, &
!                                                    msfvy,     &
!                                                    mu,        &
!                                                    mub

!   REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: g_muu,    &
!                                                     g_muv,    &
!                                                     g_mut
!   REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: muu,    &
!                                                     muv,    &
!                                                     mut

!   REAL, DIMENSION(kms:kme),INTENT(IN) :: fnm, fnp, dnw

!   integer :: k


!   CALL g_calculate_full ( mut, g_mut, mub, mu, g_mu, &
!                           ids, ide, jds, jde, 1, 2,  &
!                           ims, ime, jms, jme, 1, 1,  &
!                           its, ite, jts, jte, 1, 1 )

!   CALL g_calc_mu_uv ( config_flags,  &
!                       mu, g_mu, mub, muu, g_muu, muv, g_muv, &
!                       ids, ide, jds, jde, kds, kde,  &
!                       ims, ime, jms, jme, kms, kme,  &
!                       its, ite, jts, jte, kts, kte  )

!   CALL g_couple_momentum ( muu, g_muu, ru, g_ru, u, g_u, msfuy,             & 
!                            muv, g_muv, rv, g_rv, v, g_v, msfvx, msfvx_inv,  &
!                            mut, g_mut, rw, g_rw, w, g_w, msfty,             &
!                            ids, ide, jds, jde, kds, kde,  &
!                            ims, ime, jms, jme, kms, kme,  &
!                            its, ite, jts, jte, kts, kte  )

!!  NPan 05/22/10 { 
!!  To debug WRF+ byd-related TL/AD codes, only write the above codes and comment the following computations. 
!!  new call, couples V with mu, also has correct map factors.  WCS, 3 june 2001
!!   CALL calc_ww_cp ( u, v, mu, mub, ww,               &
!!                     rdx, rdy, msftx, msfty,          &
!!                     msfux, msfuy, msfvx, msfvx_inv,  &
!!                     msfvy, dnw,                      &
!!                     ids, ide, jds, jde, kds, kde,    &
!!                     ims, ime, jms, jme, kms, kme,    &
!!                     its, ite, jts, jte, kts, kte    )

!!   CALL calc_cq ( moist, cqu, cqv, cqw, n_moist, &
!!                  ids, ide, jds, jde, kds, kde,  &
!!                  ims, ime, jms, jme, kms, kme,  &
!!                  its, ite, jts, jte, kts, kte  )

!!   CALL calc_alt ( alt, al, alb,                 &
!!                   ids, ide, jds, jde, kds, kde, &
!!                   ims, ime, jms, jme, kms, kme, &
!!                   its, ite, jts, jte, kts, kte )

!!   CALL calc_php ( php, ph, phb,                 &
!!                   ids, ide, jds, jde, kds, kde, &
!!                   ims, ime, jms, jme, kms, kme, &
!!                   its, ite, jts, jte, kts, kte )
!!  NPan }

!END SUBROUTINE g_rk_step_prep
 SUBROUTINE g_rk_step_prep(config_flags,rk_step,u,g_u,v,g_v,w,g_w,t, &
 g_t,ph,g_ph,mu,g_mu,moist,g_moist,ru,g_ru,rv,g_rv,rw,g_rw,ww, &
 g_ww,php,g_php,alt,g_alt,muu,g_muu,muv,g_muv,mub,mut,g_mut,phb,pb, &
 p,g_p,al,g_al,alb,cqu,g_cqu,cqv,g_cqv,cqw,g_cqw,msfux,msfuy,msfvx, &
 msfvx_inv,msfvy,msftx,msfty,fnm,fnp,dnw,rdx,rdy,n_moist,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 IMPLICIT NONE

 REAL :: Tmpv1,g_Tmpv1
 TYPE(grid_config_rec_type) :: config_flags
 INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
 INTEGER :: n_moist,rk_step
 REAL :: rdx,rdy
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: u,g_u,v,g_v,w,g_w,t,g_t,ph, &
 g_ph,phb,pb,al,g_al,alb
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: ru,g_ru,rv,g_rv,rw,g_rw,ww, &
 g_ww,php,g_php,cqu,g_cqu,cqv,g_cqv,cqw,g_cqw,alt,g_alt
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: p,g_p
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme,n_moist) :: moist,g_moist
 REAL,DIMENSION(ims:ime,jms:jme) :: msftx,msfty,msfux,msfuy,msfvx,msfvx_inv,msfvy,mu, &
 g_mu,mub
 REAL,DIMENSION(ims:ime,jms:jme) :: muu,g_muu,muv,g_muv,mut,g_mut
 REAL,DIMENSION(kms:kme) :: fnm,fnp,dnw
 integer :: k

 CALL g_calculate_full(mut,g_mut,mub,mu,g_mu,ids,ide,jds,jde,1,2,ims,ime,jms, &
 jme,1,1,its,ite,jts,jte,1,1)

 CALL g_calc_mu_uv(config_flags,mu,g_mu,mub,muu,g_muu,muv,g_muv,ids,ide, &
 jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 CALL g_couple_momentum(muu,g_muu,ru,g_ru,u,g_u,msfuy,muv,g_muv,rv, &
 g_rv,v,g_v,msfvx,msfvx_inv,mut,g_mut,rw,g_rw,w,g_w,msfty,ids,ide,jds, &
 jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 CALL g_calc_ww_cp(u,g_u,v,g_v,mu,g_mu,mub,ww,g_ww,rdx,rdy,msftx,msfty, &
 msfux,msfuy,msfvx,msfvx_inv,msfvy,dnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_calc_cq(moist,g_moist,cqu,g_cqu,cqv,g_cqv,cqw,g_cqw,n_moist, &
 ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 CALL g_calc_alt(alt,g_alt,al,g_al,alb,ids,ide,jds,jde,kds,kde,ims,ime,jms, &
 jme,kms,kme,its,ite,jts,jte,kts,kte)

 CALL g_calc_php(php,g_php,ph,g_ph,phb,ids,ide,jds,jde,kds,kde,ims,ime,jms, &
 jme,kms,kme,its,ite,jts,jte,kts,kte)

 END SUBROUTINE g_rk_step_prep

!-------------------------------------------------------------------------------

 SUBROUTINE g_rk_tendency(config_flags,rk_step,ru_tend,g_ru_tend,rv_tend, &
 g_rv_tend,rw_tend,g_rw_tend,ph_tend,g_ph_tend,t_tend,g_t_tend,ru_tendf, &
 g_ru_tendf,rv_tendf,g_rv_tendf,rw_tendf,g_rw_tendf,ph_tendf,g_ph_tendf, &
 t_tendf,g_t_tendf,mu_tend,g_mu_tend,u_save,g_u_save,v_save,g_v_save, &
 w_save,g_w_save,ph_save,g_ph_save,t_save,g_t_save,mu_save,g_mu_save, &
 RTHFTEN,g_RTHFTEN,ru,g_ru,rv,g_rv,rw,g_rw,ww,g_ww,u,g_u,v,g_v,w, &
 g_w,t,g_t,ph,g_ph,u_old,g_u_old,v_old,g_v_old,w_old,g_w_old,t_old, &
! Revised by Ning Pan, 2010-07-29
! g_t_old,ph_old,g_ph_old,h_diabatic,g_h_diabatic,phb,t_init,g_t_init,mu, &
 g_t_old,ph_old,g_ph_old,h_diabatic,g_h_diabatic,phb,t_init,mu, &
 g_mu,mut,g_mut,muu,g_muu,muv,g_muv,mub,al,g_al,alt,g_alt,p,g_p, &
 pb,php,g_php,cqu,g_cqu,cqv,g_cqv,cqw,g_cqw,u_base,v_base,t_base,qv_base, &
! Revised by Ning Pan, 2010-07-29
! z_base,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,xlat,g_xlat,f,e,sina,cosa, &
! fnm,fnp,rdn,rdnw,dt,rdx,rdy,khdif,kvdif,xkmhd,g_xkmhd,xkhh,g_xkhh,diff_6th_opt, &
! diff_6th_factor,g_diff_6th_factor,dampcoef,g_dampcoef,zdamp,g_zdamp, &
! damp_opt,cf1,cf2,cf3,cfn,cfn1,n_moist,non_hydrostatic,top_lid,u_frame,g_u_frame, &
! v_frame,g_v_frame,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte, &
! kts,kte,max_vert_cfl,g_max_vert_cfl,max_horiz_cfl,g_max_horiz_cfl)
 z_base,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,xlat,f,e,sina,cosa, &
 fnm,fnp,rdn,rdnw,dt,rdx,rdy,khdif,kvdif,xkmhd,g_xkmhd,xkhh,g_xkhh,diff_6th_opt, &
 diff_6th_factor,dampcoef,zdamp, &
 damp_opt,cf1,cf2,cf3,cfn,cfn1,n_moist,non_hydrostatic,top_lid,u_frame, &
 v_frame,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte, &
 kts,kte,max_vert_cfl,max_horiz_cfl)

 IMPLICIT NONE

 REAL :: Tmpv1,g_Tmpv1
 TYPE(grid_config_rec_type) :: config_flags
 INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
 LOGICAL :: non_hydrostatic,top_lid
 INTEGER :: n_moist,rk_step
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: ru,g_ru,rv,g_rv,rw,g_rw,ww, &
 g_ww,u,g_u,v,g_v,w,g_w,t,g_t,ph,g_ph,u_old,g_u_old,v_old, &
 g_v_old,w_old,g_w_old,t_old,g_t_old,ph_old,g_ph_old,phb,al,g_al,alt, &
! Revised by Ning Pan, 2010-07-29
! g_alt,p,g_p,pb,php,g_php,cqu,g_cqu,cqv,g_cqv,t_init,g_t_init,xkmhd, &
 g_alt,p,g_p,pb,php,g_php,cqu,g_cqu,cqv,g_cqv,t_init,xkmhd, &
 g_xkmhd,xkhh,g_xkhh,h_diabatic,g_h_diabatic
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: ru_tend,g_ru_tend,rv_tend,g_rv_tend, &
 rw_tend,g_rw_tend,t_tend,g_t_tend,ph_tend,g_ph_tend,RTHFTEN,g_RTHFTEN, &
 u_save,g_u_save,v_save,g_v_save,w_save,g_w_save,ph_save,g_ph_save,t_save, &
 g_t_save
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: ru_tendf,g_ru_tendf,rv_tendf, &
 g_rv_tendf,rw_tendf,g_rw_tendf,t_tendf,g_t_tendf,ph_tendf,g_ph_tendf,cqw,g_cqw
 REAL,DIMENSION(ims:ime,jms:jme) :: mu_tend,g_mu_tend,mu_save,g_mu_save
 REAL,DIMENSION(ims:ime,jms:jme) :: msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty, &
! Revised by Ning Pan, 2010-07-30
! xlat,g_xlat,f,e,sina,cosa,mu,g_mu,mut,g_mut,mub,muu,g_muu,muv,g_muv
 xlat,f,e,sina,cosa,mu,g_mu,mut,g_mut,mub,muu,g_muu,muv,g_muv
 REAL,DIMENSION(kms:kme) :: fnm,fnp,rdn,rdnw,u_base,v_base,t_base,qv_base,z_base
! Revised by Ning Pan, 2010-07-29
! REAL :: rdx,rdy,dt,u_frame,g_u_frame,v_frame,g_v_frame,khdif,kvdif
 REAL :: rdx,rdy,dt,u_frame,v_frame,khdif,kvdif
 INTEGER :: diff_6th_opt
! Revised by Ning Pan, 2010-07-29
! REAL :: diff_6th_factor,g_diff_6th_factor
 REAL :: diff_6th_factor
 INTEGER :: damp_opt
! Revised by Ning Pan, 2010-07-29
! REAL :: zdamp,g_zdamp,dampcoef,g_dampcoef
! REAL :: max_horiz_cfl,g_max_horiz_cfl
! REAL :: max_vert_cfl,g_max_vert_cfl
 REAL :: zdamp,dampcoef
 REAL :: max_horiz_cfl
 REAL :: max_vert_cfl
! Revised by Ning Pan, 2010-07-29
! REAL :: kdift,g_kdift,khdq,g_khdq,kvdq,g_kvdq,cfn,cfn1,cf1,cf2,cf3
 REAL :: kdift,khdq,kvdq,cfn,cfn1,cf1,cf2,cf3
 INTEGER :: i,j,k
 INTEGER :: time_step

 CALL g_zero_tend(ru_tend,g_ru_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_zero_tend(rv_tend,g_rv_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_zero_tend(rw_tend,g_rw_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_zero_tend(t_tend,g_t_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_zero_tend(ph_tend,g_ph_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_zero_tend(u_save,g_u_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_zero_tend(v_save,g_v_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_zero_tend(w_save,g_w_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_zero_tend(ph_save,g_ph_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_zero_tend(t_save,g_t_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_zero_tend(mu_tend,g_mu_tend,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,its, &
 ite,jts,jte,1,1)

 CALL g_zero_tend(mu_save,g_mu_save,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,its, &
 ite,jts,jte,1,1)

!This line is fail to be recognized
    CALL nl_get_time_step ( 1, time_step )

 CALL g_advect_u(u,g_u,u,g_u,ru_tend,g_ru_tend,ru,g_ru,rv,g_rv,ww, &
 g_ww,mut,g_mut,time_step,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm, &
 fnp,rdx,rdy,rdnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 CALL g_advect_v(v,g_v,v,g_v,rv_tend,g_rv_tend,ru,g_ru,rv,g_rv,ww, &
 g_ww,mut,g_mut,time_step,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm, &
 fnp,rdx,rdy,rdnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 IF(non_hydrostatic) CALL g_advect_w(w,g_w,w,g_w,rw_tend,g_rw_tend,ru, &
 g_ru,rv,g_rv,ww,g_ww,mut,g_mut,time_step,config_flags,msfux,msfuy,msfvx, &
 msfvy,msftx,msfty,fnm,fnp,rdx,rdy,rdn,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_advect_scalar(t,g_t,t,g_t,t_tend,g_t_tend,ru,g_ru,rv,g_rv, &
 ww,g_ww,mut,g_mut,time_step,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty, &
 fnm,fnp,rdx,rdy,rdnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 IF( config_flags%cu_physics == GDSCHEME  .OR.       &
          config_flags%cu_physics == G3SCHEME ) THEN

 CALL g_set_tend(RTHFTEN,g_RTHFTEN,t_tend,g_t_tend,msfty,ids,ide,jds,jde,kds, &
 kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 END IF

 CALL g_rhs_ph(ph_tend,g_ph_tend,u,g_u,v,g_v,ww,g_ww,ph,g_ph,ph, &
 g_ph,phb,w,g_w,mut,g_mut,muu,g_muu,muv,g_muv,fnm,fnp,rdnw,cfn,cfn1, &
 rdx,rdy,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,non_hydrostatic,config_flags, &
 ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 CALL g_horizontal_pressure_gradient(ru_tend,g_ru_tend,rv_tend,g_rv_tend,ph, &
 g_ph,alt,g_alt,p,g_p,pb,al,g_al,php,g_php,cqu,g_cqu,cqv,g_cqv, &
 muu,g_muu,muv,g_muv,mu,g_mu,fnm,fnp,rdnw,cf1,cf2,cf3,rdx,rdy,msfux,msfuy, &
 msfvx,msfvy,msftx,msfty,config_flags,non_hydrostatic,top_lid,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 IF(non_hydrostatic) THEN

 CALL g_pg_buoy_w(rw_tend,g_rw_tend,p,g_p,cqw,g_cqw,mu,g_mu,mub,rdnw, &
 rdn,g,msftx,msfty,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 ENDIF

! Revised by Ning Pan, 2010-07-30
! CALL g_w_damp(rw_tend,g_rw_tend,max_vert_cfl,g_max_vert_cfl,max_horiz_cfl, &
! g_max_horiz_cfl,u,g_u,v,g_v,ww,g_ww,w,g_w,mut,g_mut,rdnw,rdx,rdy, &
 CALL g_w_damp(rw_tend,g_rw_tend,max_vert_cfl,max_horiz_cfl, &
 u,g_u,v,g_v,ww,g_ww,w,g_w,mut,g_mut,rdnw,rdx,rdy, &
 msfux,msfuy,msfvx,msfvy,dt,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 IF(config_flags%pert_coriolis) THEN

 CALL g_perturbation_coriolis(ru,g_ru,rv,g_rv,rw,g_rw,ru_tend, &
 g_ru_tend,rv_tend,g_rv_tend,rw_tend,g_rw_tend,config_flags,u_base,v_base, &
 z_base,muu,g_muu,muv,g_muv,phb,ph,g_ph,msftx,msfty,msfux,msfuy,msfvx,msfvy, &
 f,e,sina,cosa,fnm,fnp,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts, &
 jte,kts,kte)
 ELSE

 CALL g_coriolis(ru,g_ru,rv,g_rv,rw,g_rw,ru_tend,g_ru_tend,rv_tend, &
 g_rv_tend,rw_tend,g_rw_tend,config_flags,msftx,msfty,msfux,msfuy,msfvx,msfvy,f, &
 e,sina,cosa,fnm,fnp,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 END IF

 CALL g_curvature(ru,g_ru,rv,g_rv,rw,g_rw,u,g_u,v,g_v,w,g_w, &
 ru_tend,g_ru_tend,rv_tend,g_rv_tend,rw_tend,g_rw_tend,config_flags,msfux, &
! Revised by Ning Pan, 2010-07-30: xlat is a constant array
! msfuy,msfvx,msfvy,msftx,msfty,xlat,g_xlat,fnm,fnp,rdx,rdy,ids,ide,jds,jde,kds,kde, &
 msfuy,msfvx,msfvy,msftx,msfty,xlat,fnm,fnp,rdx,rdy,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 IF(config_flags%ra_lw_physics == HELDSUAREZ) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Reamarked by Ning Pan, 2010-07-30 : JUST FOR DEBUGGING DYNAMICS OF WRF+   !!!
!!!              REMARK SHOULD BE REMOVED WHEN CONSTRUCTING PHYSICS OF WRF+   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CALL g_held_suarez_damp(ru_tend,g_ru_tend,rv_tend,g_rv_tend,ru,g_ru,rv, &
! g_rv,p,g_p,pb,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 END IF

 IF( rk_step == 1 ) THEN

 IF(config_flags%diff_opt .eq. 1) THEN

 CALL g_horizontal_diffusion('u',u,g_u,ru_tendf,g_ru_tendf,mut,g_mut, &
 config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,g_xkmhd, &
 rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 CALL g_horizontal_diffusion('v',v,g_v,rv_tendf,g_rv_tendf,mut,g_mut, &
 config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,g_xkmhd, &
 rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 CALL g_horizontal_diffusion('w',w,g_w,rw_tendf,g_rw_tendf,mut,g_mut, &
 config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,g_xkmhd, &
 rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

! g_khdq =0.0  ! Remarked by Ning Pan, 2010-07-30
 khdq =3. *khdif

 CALL g_horizontal_diffusion_3dmp('m',t,g_t,t_tendf,g_t_tendf,mut,g_mut, &
! Revised by Ning Pan, 2010-07-30
! config_flags,t_init,g_t_init,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdq, &
! g_khdq,xkhh,g_xkhh,rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its, &
 config_flags,t_init,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdq, &
 xkhh,g_xkhh,rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its, &
 ite,jts,jte,kts,kte)

 IF(config_flags%bl_pbl_physics .eq. 0) THEN

 CALL g_vertical_diffusion_u(u,g_u,ru_tendf,g_ru_tendf,config_flags,u_base, &
 alt,g_alt,muu,g_muu,rdn,rdnw,kvdif,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 CALL g_vertical_diffusion_v(v,g_v,rv_tendf,g_rv_tendf,config_flags,v_base, &
 alt,g_alt,muv,g_muv,rdn,rdnw,kvdif,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)

 IF(non_hydrostatic) CALL g_vertical_diffusion('w',w,g_w,rw_tendf,g_rw_tendf, &
 config_flags,alt,g_alt,mut,g_mut,rdn,rdnw,kvdif,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

! g_kvdq =0.0  ! Remarked by Ning Pan, 2010-07-30
 kvdq =3. *kvdif

 CALL g_vertical_diffusion_3dmp(t,g_t,t_tendf,g_t_tendf,config_flags,t_init, &
! Revised by Ning Pan, 2010-07-30
! g_t_init,alt,g_alt,mut,g_mut,rdn,rdnw,kvdq,g_kvdq,ids,ide,jds,jde,kds, &
 alt,g_alt,mut,g_mut,rdn,rdnw,kvdq,ids,ide,jds,jde,kds, &
 kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 ENDIF

 END IF

 IF( diff_6th_opt .NE. 0 ) THEN

 CALL g_sixth_order_diffusion('u',u,g_u,ru_tendf,g_ru_tendf,mut,g_mut,dt, &
! Revised by Ning Pan, 2010-07-30
! config_flags,diff_6th_opt,diff_6th_factor,g_diff_6th_factor,ids,ide,jds,jde,kds, &
 config_flags,diff_6th_opt,diff_6th_factor,ids,ide,jds,jde,kds, &
 kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 CALL g_sixth_order_diffusion('v',v,g_v,rv_tendf,g_rv_tendf,mut,g_mut,dt, &
! Revised by Ning Pan, 2010-07-30
! config_flags,diff_6th_opt,diff_6th_factor,g_diff_6th_factor,ids,ide,jds,jde,kds, &
 config_flags,diff_6th_opt,diff_6th_factor,ids,ide,jds,jde,kds, &
 kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 IF(non_hydrostatic) CALL g_sixth_order_diffusion('w',w,g_w,rw_tendf, &
 g_rw_tendf,mut,g_mut,dt,config_flags,diff_6th_opt,diff_6th_factor, &
! Revised by Ning Pan, 2010-07-30
! g_diff_6th_factor,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 CALL g_sixth_order_diffusion('m',t,g_t,t_tendf,g_t_tendf,mut,g_mut,dt, &
! Revised by Ning Pan, 2010-07-30
! config_flags,diff_6th_opt,diff_6th_factor,g_diff_6th_factor,ids,ide,jds,jde,kds, &
 config_flags,diff_6th_opt,diff_6th_factor,ids,ide,jds,jde,kds, &
 kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 ENDIF

 IF( damp_opt .eq. 2 ) CALL g_rk_rayleigh_damp(ru_tendf,g_ru_tendf,rv_tendf, &
 g_rv_tendf,rw_tendf,g_rw_tendf,t_tendf,g_t_tendf,u,g_u,v,g_v,w,g_w, &
! Revised by Ning Pan, 2010-07-23
! t,g_t,t_init,g_t_init,mut,g_mut,muu,g_muu,muv,g_muv,ph,g_ph,phb, &
! u_base,v_base,t_base,z_base,dampcoef,g_dampcoef,zdamp,g_zdamp,ids,ide,jds,jde, &
 t,g_t,t_init,mut,g_mut,muu,g_muu,muv,g_muv,ph,g_ph,phb, &
 u_base,v_base,t_base,z_base,dampcoef,zdamp,ids,ide,jds,jde, &
 kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
  END IF

 END SUBROUTINE g_rk_tendency

!-------------------------------------------------------------------------------

SUBROUTINE g_rk_addtend_dry ( ru_tend, g_ru_tend, rv_tend, g_rv_tend,                         &
                            rw_tend, g_rw_tend, ph_tend, g_ph_tend, t_tend, g_t_tend,         &
                            ru_tendf, g_ru_tendf, rv_tendf, g_rv_tendf, rw_tendf, g_rw_tendf, & 
                            ph_tendf, g_ph_tendf, t_tendf, g_t_tendf,               &
                            u_save, g_u_save, v_save, g_v_save, w_save, g_w_save,   & 
                            ph_save, g_ph_save, t_save, g_t_save,                   & 
                            mu_tend, g_mu_tend, mu_tendf, g_mu_tendf, rk_step,      &
                            h_diabatic, g_h_diabatic, mut, g_mut, msftx, msfty, msfux, msfuy,     &
                            msfvx, msfvx_inv, msfvy,                         &
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

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(INOUT) :: g_ru_tend, &
                                                                      g_rv_tend, &
                                                                      g_rw_tend, &
                                                                      g_ph_tend, &
                                                                      g_t_tend,  &
                                                                      g_ru_tendf, &
                                                                      g_rv_tendf, &
                                                                      g_rw_tendf, &
                                                                      g_ph_tendf, &
                                                                      g_t_tendf
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

   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(INOUT) :: g_mu_tend, &
                                                             g_mu_tendf
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(INOUT) :: mu_tend, &
                                                             mu_tendf

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(IN   ) ::  g_u_save,  &
                                                                       g_v_save,  &
                                                                       g_w_save,  &
                                                                      g_ph_save,  &
                                                                       g_t_save,  &
                                                                      g_h_diabatic
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(IN   ) ::  u_save,  &
                                                                       v_save,  &
                                                                       w_save,  &
                                                                      ph_save,  &
                                                                       t_save,  &
                                                                      h_diabatic

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: g_mut
   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: mut,       &
                                                                    msftx,     &
                                                                    msfty,     &
                                                                    msfux,     &
                                                                    msfuy,     &
                                                                    msfvx,     &
                                                                    msfvx_inv, &
                                                                    msfvy


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
! (u by msfuy, v by msfvx)
!  before adding it to physics tendency (*tendf)
! For momentum we need the final tendency to include an inverse msf
! physics/bc tendency needs to be divided, advection tendency already has it

! For scalars we need the final tendency to include an inverse msf (msfty)
! advection tendency is OK, physics/bc tendency needs to be divided by msf

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,ite
     ! multiply by my to uncouple u
     IF(rk_step == 1)g_ru_tendf(i,k,j) = g_ru_tendf(i,k,j) +  g_u_save(i,k,j)*msfuy(i,j)
     IF(rk_step == 1)ru_tendf(i,k,j) = ru_tendf(i,k,j) +  u_save(i,k,j)*msfuy(i,j)
     ! divide by my to couple u
     g_ru_tend(i,k,j) = g_ru_tend(i,k,j) + g_ru_tendf(i,k,j)/msfuy(i,j)
     ru_tend(i,k,j) = ru_tend(i,k,j) + ru_tendf(i,k,j)/msfuy(i,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,jte
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     ! multiply by mx to uncouple v
     IF(rk_step == 1)g_rv_tendf(i,k,j) = g_rv_tendf(i,k,j) +  g_v_save(i,k,j)*msfvx(i,j)
     IF(rk_step == 1)rv_tendf(i,k,j) = rv_tendf(i,k,j) +  v_save(i,k,j)*msfvx(i,j)
     ! divide by mx to couple v
     g_rv_tend(i,k,j) = g_rv_tend(i,k,j) + g_rv_tendf(i,k,j)*msfvx_inv(i,j)
     rv_tend(i,k,j) = rv_tend(i,k,j) + rv_tendf(i,k,j)*msfvx_inv(i,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     ! multiply by my to uncouple w
     IF(rk_step == 1)g_rw_tendf(i,k,j) = g_rw_tendf(i,k,j) +  g_w_save(i,k,j)*msfty(i,j)
     IF(rk_step == 1)rw_tendf(i,k,j) = rw_tendf(i,k,j) +  w_save(i,k,j)*msfty(i,j)
     ! divide by my to couple w
     g_rw_tend(i,k,j) = g_rw_tend(i,k,j) + g_rw_tendf(i,k,j)/msfty(i,j)
     rw_tend(i,k,j) = rw_tend(i,k,j) + rw_tendf(i,k,j)/msfty(i,j)

     IF(rk_step == 1)g_ph_tendf(i,k,j) = g_ph_tendf(i,k,j) +  g_ph_save(i,k,j)
     IF(rk_step == 1)ph_tendf(i,k,j) = ph_tendf(i,k,j) +  ph_save(i,k,j)
     ! divide by my to couple scalar
     g_ph_tend(i,k,j) = g_ph_tend(i,k,j) + g_ph_tendf(i,k,j)/msfty(i,j)
     ph_tend(i,k,j) = ph_tend(i,k,j) + ph_tendf(i,k,j)/msfty(i,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     IF(rk_step == 1)g_t_tendf(i,k,j) = g_t_tendf(i,k,j) +  g_t_save(i,k,j)
     IF(rk_step == 1)t_tendf(i,k,j) = t_tendf(i,k,j) +  t_save(i,k,j)
     ! divide by my to couple theta
! NPan 05/27/10 {
! If h_diabatic is an active variable, the statement for computing g_t_tend 
! should be replaced with the commentted statement.
      g_t_tend(i,k,j) =  g_t_tend(i,k,j) +  g_t_tendf(i,k,j)/msfty(i,j)  &
                                         +  g_mut(i,j)*h_diabatic(i,k,j)/msfty(i,j) &
                                         +  mut(i,j)*g_h_diabatic(i,k,j)/msfty(i,j) 
!      g_t_tend(i,k,j) =  g_t_tend(i,k,j) +  g_t_tendf(i,k,j)/msfty(i,j)  &
!                                         +  g_mut(i,j)*h_diabatic(i,k,j)/msfty(i,j)
! NPan }
      t_tend(i,k,j) =  t_tend(i,k,j) +  t_tendf(i,k,j)/msfty(i,j)  &
                                     +  mut(i,j)*h_diabatic(i,k,j)/msfty(i,j)
     ! divide by my to couple heating
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
! mu tendencies not coupled with 1/msf
      g_mu_tend(i,j) =  g_mu_tend(i,j) +  g_mu_tendf(i,j)
      mu_tend(i,j) =  mu_tend(i,j) +  mu_tendf(i,j)
   ENDDO
   ENDDO

END SUBROUTINE g_rk_addtend_dry

!-------------------------------------------------------------------------------

! Revised by Ning Pan, 2010-08-02
! SUBROUTINE g_rk_scalar_tend(scs,sce,config_flags,rk_step,dt,g_dt,ru,g_ru,rv, &
 SUBROUTINE g_rk_scalar_tend(scs,sce,config_flags,rk_step,dt,ru,g_ru,rv, &
 g_rv,ww,g_ww,mut,g_mut,mub,mu_old,g_mu_old,alt,g_alt,scalar_old, &
 g_scalar_old,scalar,g_scalar,scalar_tends,g_scalar_tends,advect_tend, &
 g_advect_tend,RQVFTEN,g_RQVFTEN,base,moist_step,fnm,fnp,msfux,msfuy,msfvx, &
 msfvx_inv,msfvy,msftx,msfty,rdx,rdy,rdn,rdnw,khdif,kvdif,xkmhd,g_xkmhd, &
! Revised by Ning Pan, 2010-08-02
! diff_6th_opt,diff_6th_factor,g_diff_6th_factor,adv_opt,ids,ide,jds,jde,kds,kde, &
 diff_6th_opt,diff_6th_factor,adv_opt,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 IMPLICIT NONE

 REAL :: Tmpv1,g_Tmpv1
 TYPE(grid_config_rec_type) :: config_flags
 INTEGER :: rk_step,scs,sce
 INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
 LOGICAL :: moist_step
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme,scs:sce) :: scalar,g_scalar,scalar_old, &
 g_scalar_old
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme,scs:sce) :: scalar_tends,g_scalar_tends
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: advect_tend,g_advect_tend
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: RQVFTEN,g_RQVFTEN
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: ru,g_ru,rv,g_rv,ww,g_ww,xkmhd, &
 g_xkmhd,alt,g_alt
 REAL,DIMENSION(kms:kme) :: fnm,fnp,rdn,rdnw,base
 REAL,DIMENSION(ims:ime,jms:jme) :: msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,mub, &
 mut,g_mut,mu_old,g_mu_old
 REAL :: rdx,rdy,khdif,kvdif
 INTEGER :: diff_6th_opt
! Revised by Ning Pan, 2010-08-02
! REAL :: diff_6th_factor,g_diff_6th_factor
! REAL :: dt,g_dt
 REAL :: diff_6th_factor
 REAL :: dt
 INTEGER :: adv_opt

 INTEGER :: im,i,j,k
 INTEGER :: time_step
 REAL :: khdq,kvdq,tendency,g_tendency

 khdq =khdif/prandtl

 kvdq =kvdif/prandtl

 DO im =scs,sce

 CALL g_zero_tend(advect_tend(ims,kms,jms),g_advect_tend(ims,kms,jms) &
,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
!This line is fail to be recognized
      CALL nl_get_time_step ( 1, time_step )

 IF( (rk_step == 3) .and. (adv_opt == POSITIVEDEF) ) THEN

 CALL g_advect_scalar_pd(scalar(ims,kms,jms,im),g_scalar(ims,kms,jms,im) &
 ,scalar_old(ims,kms,jms,im),g_scalar_old(ims,kms,jms,im),advect_tend(ims,kms,jms) &
 ,g_advect_tend(ims,kms,jms),ru,g_ru,rv,g_rv,ww,g_ww,mut,g_mut,mub, &
 mu_old,g_mu_old,time_step,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm, &
! Revised by Ning Pan, 2010-08-02
! fnp,rdx,rdy,rdnw,dt,g_dt,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite, &
 fnp,rdx,rdy,rdnw,dt,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite, &
 jts,jte,kts,kte)
 ELSE IF( (rk_step == 3) .and. (adv_opt == MONOTONIC) ) THEN

 CALL g_advect_scalar_mono(scalar(ims,kms,jms,im),g_scalar(ims,kms,jms,im) &
 ,scalar_old(ims,kms,jms,im),g_scalar_old(ims,kms,jms,im),advect_tend(ims,kms,jms) &
 ,g_advect_tend(ims,kms,jms),ru,g_ru,rv,g_rv,ww,g_ww,mut,g_mut,mub, &
 mu_old,g_mu_old,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm,fnp,rdx,rdy, &
! Revised by Ning Pan, 2010-08-02
! rdnw,dt,g_dt,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 rdnw,dt,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 ELSE

 CALL g_advect_scalar(scalar(ims,kms,jms,im),g_scalar(ims,kms,jms,im) &
 ,scalar(ims,kms,jms,im),g_scalar(ims,kms,jms,im),advect_tend(ims,kms,jms) &
 ,g_advect_tend(ims,kms,jms),ru,g_ru,rv,g_rv,ww,g_ww,mut,g_mut, &
 time_step,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm,fnp,rdx,rdy,rdnw,ids, &
 ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 END IF

 IF((config_flags%cu_physics == GDSCHEME .OR. config_flags%cu_physics == G3SCHEME)    &
! &  ! Remarked by Ning Pan, 2010-08-02
                     .and. moist_step .and. ( im == P_QV) ) THEN

 CALL g_set_tend(RQVFTEN,g_RQVFTEN,advect_tend,g_advect_tend,msfty,ids,ide, &
 jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 ENDIF

 IF( rk_step == 1 ) THEN

 IF(config_flags%diff_opt .eq. 1) THEN

 CALL g_horizontal_diffusion('m',scalar(ims,kms,jms,im),g_scalar(ims,kms,jms,im) &
 ,scalar_tends(ims,kms,jms,im),g_scalar_tends(ims,kms,jms,im),mut,g_mut, &
 config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdq,xkmhd,g_xkmhd,rdx, &
 rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 IF(config_flags%bl_pbl_physics .eq. 0) THEN

 IF( (moist_step) .and. ( im == P_QV)) THEN

 CALL g_vertical_diffusion_mp(scalar(ims,kms,jms,im),g_scalar(ims,kms,jms,im) &
 ,scalar_tends(ims,kms,jms,im),g_scalar_tends(ims,kms,jms,im),config_flags,base, &
 alt,g_alt,mut,g_mut,rdn,rdnw,kvdq,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
 kme,its,ite,jts,jte,kts,kte)
 ELSE

 CALL g_vertical_diffusion('m',scalar(ims,kms,jms,im),g_scalar(ims,kms,jms,im) &
 ,scalar_tends(ims,kms,jms,im),g_scalar_tends(ims,kms,jms,im),config_flags,alt, &
 g_alt,mut,g_mut,rdn,rdnw,kvdq,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme, &
 its,ite,jts,jte,kts,kte)
 END IF
 ENDIF
 ENDIF

 IF( diff_6th_opt .NE. 0 ) CALL g_sixth_order_diffusion('m',scalar(ims,kms,jms,im) &
 ,g_scalar(ims,kms,jms,im),scalar_tends(ims,kms,jms,im),g_scalar_tends(ims,kms, &
! Revised by Ning Pan, 2010-08-02
! jms,im),mut,g_mut,dt,g_dt,config_flags,diff_6th_opt,diff_6th_factor, &
! g_diff_6th_factor,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 jms,im),mut,g_mut,dt,config_flags,diff_6th_opt,diff_6th_factor, &
 ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
  ENDIF
 ENDDO

 END SUBROUTINE g_rk_scalar_tend

!-------------------------------------------------------------------------------

SUBROUTINE g_rk_update_scalar ( scs, sce,                      &
                                scalar_1, g_scalar_1, scalar_2, g_scalar_2, sc_tend, g_sc_tend,  &
                                advect_tend, g_advect_tend, msftx, msfty,     &
                                mu_old, g_mu_old, mu_new, g_mu_new, mu_base,  &
                                rk_step, dt, spec_zone,        &
                                config_flags,                  &
                                ids, ide, jds, jde, kds, kde,  &
                                ims, ime, jms, jme, kms, kme,  &
                                its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type), INTENT(IN) :: config_flags

   INTEGER, INTENT(IN) :: scs, sce, rk_step, spec_zone
   INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte

   REAL,    INTENT(IN) :: dt

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce),                &
         INTENT(INOUT)                                  :: g_scalar_1,  &
                                                           g_scalar_2
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce),                &
         INTENT(INOUT)                                  :: scalar_1,    &
                                                           scalar_2

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce),                &
         INTENT(IN)                                     :: g_sc_tend
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce),                &
         INTENT(IN)                                     :: sc_tend

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ),                &
         INTENT(IN)                                  :: g_advect_tend
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ),                &
         INTENT(IN)                                  :: advect_tend

   REAL, DIMENSION(ims:ime, jms:jme  ), INTENT(IN   ) ::  g_mu_old,  &
                                                          g_mu_new
   REAL, DIMENSION(ims:ime, jms:jme  ), INTENT(IN   ) ::  mu_old,  &
                                                          mu_new,  &
                                                          mu_base, &
                                                          msftx,   &
                                                          msfty

   INTEGER :: i,j,k,im
   REAL    :: sc_middle, msfsq
   REAL, DIMENSION(its:ite) :: g_muold, g_r_munew
   REAL, DIMENSION(its:ite) :: muold, r_munew

   REAL, DIMENSION(its:ite, kts:kte, jts:jte) :: g_tendency
   REAL, DIMENSION(its:ite, kts:kte, jts:jte) :: tendency

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
      i_end   = min(ite,ide-1)
      j_start = jts
      j_end   = min(jte,jde-1)
      k_start = kts
      k_end   = kte-1

      i_start_spc = i_start
      i_end_spc   = i_end
      j_start_spc = j_start
      j_end_spc   = j_end
      k_start_spc = k_start
      k_end_spc   = k_end

    IF( config_flags%nested .or. config_flags%specified ) THEN
      IF( .NOT. config_flags%periodic_x)i_start = max( its,ids+spec_zone )
      IF( .NOT. config_flags%periodic_x)i_end   = min( ite,ide-spec_zone-1 )
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
           g_tendency(i,k,j) = 0.
           tendency(i,k,j) = 0.
       ENDDO
       ENDDO
       ENDDO
   
       DO  j = j_start,j_end
       DO  k = k_start,k_end
       DO  i = i_start,i_end
          ! scalar was coupled with my
           g_tendency(i,k,j) = g_advect_tend(i,k,j) * msfty(i,j)
           tendency(i,k,j) = advect_tend(i,k,j) * msfty(i,j)
       ENDDO
       ENDDO
       ENDDO
   
       DO  j = j_start_spc,j_end_spc
       DO  k = k_start_spc,k_end_spc
       DO  i = i_start_spc,i_end_spc
           g_tendency(i,k,j) = g_tendency(i,k,j) + g_sc_tend(i,k,j,im)
           tendency(i,k,j) = tendency(i,k,j) + sc_tend(i,k,j,im)
       ENDDO
       ENDDO
       ENDDO
   
      DO  j = jts, min(jte,jde-1)

      DO  i = its, min(ite,ide-1)
        g_muold(i) = g_mu_old(i,j)
        muold(i) = mu_old(i,j) + mu_base(i,j)
        g_r_munew(i) = -g_mu_new(i,j) / ((mu_new(i,j)+mu_base(i,j))*(mu_new(i,j)+mu_base(i,j)))
        r_munew(i) = 1./(mu_new(i,j) + mu_base(i,j))
      ENDDO

      DO  k = kts, min(kte,kde-1)
      DO  i = its, min(ite,ide-1)

        g_scalar_1(i,k,j,im) = g_scalar_2(i,k,j,im)
        scalar_1(i,k,j,im) = scalar_2(i,k,j,im)
        g_scalar_2(i,k,j,im) = (muold(i)*g_scalar_1(i,k,j,im)+g_muold(i)*scalar_1(i,k,j,im)   &
                             + dt*g_tendency(i,k,j))*r_munew(i) &
                             + (muold(i)*scalar_1(i,k,j,im)   &
                             + dt*tendency(i,k,j))*g_r_munew(i)
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
           g_tendency(i,k,j) = 0.
           tendency(i,k,j) = 0.
       ENDDO
       ENDDO
       ENDDO
   
       DO  j = j_start,j_end
       DO  k = k_start,k_end
       DO  i = i_start,i_end
           ! scalar was coupled with my
           g_tendency(i,k,j) = g_advect_tend(i,k,j) * msfty(i,j)
           tendency(i,k,j) = advect_tend(i,k,j) * msfty(i,j)
       ENDDO
       ENDDO
       ENDDO
   
       DO  j = j_start_spc,j_end_spc
       DO  k = k_start_spc,k_end_spc
       DO  i = i_start_spc,i_end_spc
           g_tendency(i,k,j) = g_tendency(i,k,j) + g_sc_tend(i,k,j,im)
           tendency(i,k,j) = tendency(i,k,j) + sc_tend(i,k,j,im)
       ENDDO
       ENDDO
       ENDDO

      DO  j = jts, min(jte,jde-1)

      DO  i = its, min(ite,ide-1)
        g_muold(i) = g_mu_old(i,j)
        muold(i) = mu_old(i,j) + mu_base(i,j)
        g_r_munew(i) = -g_mu_new(i,j) / ((mu_new(i,j)+mu_base(i,j))*(mu_new(i,j)+mu_base(i,j)))
        r_munew(i) = 1./(mu_new(i,j) + mu_base(i,j))
      ENDDO

      DO  k = kts, min(kte,kde-1)
      DO  i = its, min(ite,ide-1)

        g_scalar_2(i,k,j,im) = (muold(i)*g_scalar_1(i,k,j,im)+g_muold(i)*scalar_1(i,k,j,im)   &
                             + dt*g_tendency(i,k,j))*r_munew(i)  &
                             + (muold(i)*scalar_1(i,k,j,im)      &
                             + dt*tendency(i,k,j))*g_r_munew(i)
        scalar_2(i,k,j,im) = (muold(i)*scalar_1(i,k,j,im)   &
                             + dt*tendency(i,k,j))*r_munew(i)

      ENDDO
      ENDDO
      ENDDO

      ENDDO

    END IF

END SUBROUTINE g_rk_update_scalar

!-------------------------------------------------------------------------------

 SUBROUTINE g_rk_update_scalar_pd(scs,sce,scalar,g_scalar,sc_tend,g_sc_tend, &
! Revised by Ning Pan, 2010-08-03
! mu_old,g_mu_old,mu_new,g_mu_new,mu_base,g_mu_base,rk_step,dt,spec_zone, &
 mu_old,g_mu_old,mu_new,g_mu_new,mu_base,rk_step,dt,spec_zone, &
 config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 IMPLICIT NONE

 REAL :: Tmpv1,g_Tmpv1,Tmpv2,g_Tmpv2,Tmpv3,g_Tmpv3
 TYPE(grid_config_rec_type) :: config_flags
 INTEGER :: scs,sce,rk_step,spec_zone
 INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
 REAL :: dt
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme,scs:sce) :: scalar,g_scalar,sc_tend,g_sc_tend
! Revised by Ning Pan, 2010-08-03
! REAL,DIMENSION(ims:ime,jms:jme) :: mu_old,g_mu_old,mu_new,g_mu_new,mu_base,g_mu_base
 REAL,DIMENSION(ims:ime,jms:jme) :: mu_old,g_mu_old,mu_new,g_mu_new,mu_base
 INTEGER :: i,j,k,im
 REAL :: sc_middle,g_sc_middle,msfsq,g_msfsq
 REAL,DIMENSION(its:ite) :: muold,g_muold,r_munew,g_r_munew
 REAL,DIMENSION(its:ite,kts:kte,jts:jte) :: tendency,g_tendency
 INTEGER :: i_start,i_end,j_start,j_end,k_start,k_end
 INTEGER :: i_start_spc,i_end_spc,j_start_spc,j_end_spc,k_start_spc,k_end_spc

 i_start =its

 i_end =min(ite,ide-1)

 j_start =jts

 j_end =min(jte,jde-1)

 k_start =kts

 k_end =kte-1

 i_start_spc =i_start

 i_end_spc =i_end

 j_start_spc =j_start

 j_end_spc =j_end

 k_start_spc =k_start

 k_end_spc =k_end

 IF( config_flags%nested .or. config_flags%specified ) THEN

 IF( .NOT. config_flags%periodic_x) i_start =max(its,ids+spec_zone)

 IF( .NOT. config_flags%periodic_x) i_end =min(ite,ide-spec_zone-1)

 j_start =max(jts,jds+spec_zone)

 j_end =min(jte,jde-spec_zone-1)

 k_start =kts

 k_end =min(kte,kde-1)
 ENDIF

 DO im =scs,sce
 DO j =jts,min(jte,jde-1)
 DO k =kts,min(kte,kde-1)
 DO i =its,min(ite,ide-1)

 g_tendency(i,k,j) =0.0
 tendency(i,k,j) =0.

 ENDDO
 ENDDO
 ENDDO

 DO j =j_start_spc,j_end_spc
 DO k =k_start_spc,k_end_spc
 DO i =i_start_spc,i_end_spc

 g_tendency(i,k,j) =g_tendency(i,k,j) +g_sc_tend(i,k,j,im)
 tendency(i,k,j) =tendency(i,k,j) +sc_tend(i,k,j,im)

 g_sc_tend(i,k,j,im) =0.0
 sc_tend(i,k,j,im) =0.

 ENDDO
 ENDDO
 ENDDO

 DO j =jts,min(jte,jde-1)
 DO i =its,min(ite,ide-1)

! Revised by Ning Pan, 2010-08-03
! g_muold(i) =g_mu_old(i,j) +g_mu_base(i,j)
 g_muold(i) =g_mu_old(i,j)
 muold(i) =mu_old(i,j) +mu_base(i,j)

! Revised by Ning Pan, 2010-08-03
! g_r_munew(i) =-1.*(g_mu_new(i,j) +g_mu_base(i,j))/((mu_new(i,j) &
! +mu_base(i,j))*(mu_new(i,j) +mu_base(i,j)))
 g_r_munew(i) =-1.*(g_mu_new(i,j))/((mu_new(i,j) &
 +mu_base(i,j))*(mu_new(i,j) +mu_base(i,j)))
 r_munew(i) =1./(mu_new(i,j) +mu_base(i,j))

 ENDDO

 DO k =kts,min(kte,kde-1)
 DO i =its,min(ite,ide-1)

 g_Tmpv1 =muold(i)*g_scalar(i,k,j,im) +g_muold(i)*scalar(i,k,j,im) 
 Tmpv1 =muold(i)*scalar(i,k,j,im)

 g_Tmpv2 =(Tmpv1 +dt*tendency(i,k,j))*g_r_munew(i) +(g_Tmpv1 +dt* &
 g_tendency(i,k,j))*r_munew(i) 
 Tmpv2 =(Tmpv1 +dt*tendency(i,k,j))*r_munew(i)

 g_scalar(i,k,j,im) =g_Tmpv2
 scalar(i,k,j,im) =Tmpv2

 ENDDO
 ENDDO
 ENDDO
 ENDDO

 END SUBROUTINE g_rk_update_scalar_pd

!------------------------------------------------------------

SUBROUTINE g_init_zero_tendency(ru_tendf,  g_ru_tendf, &
                                rv_tendf,  g_rv_tendf, &
                                rw_tendf,  g_rw_tendf, &
                                ph_tendf,  g_ph_tendf, &
                                t_tendf,   g_t_tendf,  &
                                tke_tendf, g_tke_tendf, &
                                mu_tendf,  g_mu_tendf,  &
                                moist_tendf, g_moist_tendf,  &
!  NPan - 05/26/10 {
!  Uncomment the corresponding args when chem or tracer is needed.   
!                                chem_tendf,  g_chem_tendf,   &
                                scalar_tendf,g_scalar_tendf, &
!                                tracer_tendf,g_tracer_tendf, &
!  NPan }
                                n_tracer,                   &
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

   INTEGER ,       INTENT(IN   ) :: n_moist,n_chem,n_scalar,n_tracer,rk_step

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(INOUT) ::  &
                                                             g_ru_tendf, &
                                                             g_rv_tendf, &
                                                             g_rw_tendf, &
                                                             g_ph_tendf, &
                                                              g_t_tendf, &
                                                            g_tke_tendf
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(INOUT) ::  &
                                                             ru_tendf, &
                                                             rv_tendf, &
                                                             rw_tendf, &
                                                             ph_tendf, &
                                                              t_tendf, &
                                                            tke_tendf

   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(INOUT) ::  g_mu_tendf
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(INOUT) ::  mu_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),INTENT(INOUT)::&
                                                          g_moist_tendf
   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),INTENT(INOUT)::&
                                                          moist_tendf

!  NPan - 05/26/10 {
!  Uncomment the corresponding definations when chem or tracer is needed.   
!   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_chem ),INTENT(INOUT)::&
!                                                          g_chem_tendf
!   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_chem ),INTENT(INOUT)::&
!                                                          chem_tendf
!   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_tracer ),INTENT(INOUT)::&
!                                                          g_tracer_tendf
!   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_tracer ),INTENT(INOUT)::&
!                                                          tracer_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar ),INTENT(INOUT)::&
                                                          g_scalar_tendf
   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar ),INTENT(INOUT)::&
                                                          scalar_tendf
!  NPan } 

! LOCAL VARS

   INTEGER :: im, ic, is

!<DESCRIPTION>
!
! init_zero_tendency 
! sets tendency arrays to zero for all prognostic variables.
!
!</DESCRIPTION>


   CALL g_zero_tend ( ru_tendf, g_ru_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL g_zero_tend ( rv_tendf, g_rv_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL g_zero_tend ( rw_tendf, g_rw_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL g_zero_tend ( ph_tendf, g_ph_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL g_zero_tend ( t_tendf, g_t_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL g_zero_tend ( tke_tendf, g_tke_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL g_zero_tend ( mu_tendf, g_mu_tendf,            &
                      ids, ide, jds, jde, kds, kds,    &
                      ims, ime, jms, jme, kms, kms,    &
                      its, ite, jts, jte, kts, kts     )

!   DO im=PARAM_FIRST_SCALAR,n_moist
   DO im=1,n_moist                      ! make sure first one is zero too
      CALL g_zero_tend ( moist_tendf(ims,kms,jms,im), g_moist_tendf(ims,kms,jms,im), &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte  )
   ENDDO

!  NPan - 05/26/10 {
!  Uncomment the corresponding statements when chem or tracer is needed.   
!!   DO ic=PARAM_FIRST_SCALAR,n_chem
!   DO ic=1,n_chem                       !! make sure first one is zero too
!      CALL g_zero_tend ( chem_tendf(ims,kms,jms,ic), g_chem_tendf(ims,kms,jms,ic), &
!                         ids, ide, jds, jde, kds, kde, &
!                         ims, ime, jms, jme, kms, kme, &
!                         its, ite, jts, jte, kts, kte  )
!   ENDDO

!!   DO ic=PARAM_FIRST_SCALAR,n_tracer
!   DO ic=1,n_tracer                     !! make sure first one is zero too
!      CALL g_zero_tend ( tracer_tendf(ims,kms,jms,ic), g_tracer_tendf(ims,kms,jms,ic), &
!                         ids, ide, jds, jde, kds, kde, &
!                         ims, ime, jms, jme, kms, kme, &
!                         its, ite, jts, jte, kts, kte  )
!   ENDDO

!   DO ic=PARAM_FIRST_SCALAR,n_scalar
   DO ic=1,n_scalar                       ! make sure first one is zero too
      CALL g_zero_tend ( scalar_tendf(ims,kms,jms,ic), g_scalar_tendf(ims,kms,jms,ic), &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte  )
   ENDDO
!  NPan }

END SUBROUTINE g_init_zero_tendency

!===================================================================


SUBROUTINE g_dump_data( a, field, io_unit,            &
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

END SUBROUTINE g_dump_data

!-----------------------------------------------------------------------

SUBROUTINE g_calculate_phy_tend (config_flags,mu,muu,muv,pi3d,           &
                     RTHRATEN,                                         &
                     RUBLTEN,RVBLTEN,RTHBLTEN,                         &
                     RQVBLTEN,RQCBLTEN,RQIBLTEN,                       &
                     RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,              &
                     RQICUTEN,RQSCUTEN,                                &
                     RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RQVNDGDTEN,        &
                     RMUNDGDTEN,                                       &
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
                INTENT(IN   )   ::                                 mu, &
                                                                  muu, &
                                                                  muv
      
                                                           
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

! fdda

      REAL,     DIMENSION( ims:ime, kms:kme, jms:jme )               , &
                INTENT(INOUT)   ::                            RUNDGDTEN, &
                                                              RVNDGDTEN, &
                                                             RTHNDGDTEN, &
                                                             RQVNDGDTEN
      REAL,     DIMENSION( ims:ime, jms:jme )               , &
                INTENT(INOUT)   ::                           RMUNDGDTEN

      INTEGER :: i,k,j
      INTEGER :: itf,ktf,jtf,itsu,jtsv

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
      itsu=MAX(its,ids+1)
      jtsv=MAX(jts,jds+1)

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

! fdda
! note fdda u and v tendencies are staggered, also only interior points have muu/muv,
!   so only couple those

   IF (config_flags%grid_fdda .gt. 0) THEN

      DO J=jts,jtf
      DO K=kts,ktf
      DO I=itsu,itf
!     if( i == itf/2 .AND. j == jtf/2 .AND. k == ktf/2 ) &
!     write(*,'(a,3i6,e15.5)') 'u_ten before=',i,k,j, RUNDGDTEN(i,k,j)
         RUNDGDTEN(I,K,J) =muu(I,J)*RUNDGDTEN(I,K,J)
!        if( i == itf/2 .AND. j == jtf/2 .AND. k==ktf/2 ) &
!          write(*,'(a,2f15.5)') 'mu, muu=',mu(i,j), muu(i,j)
!     if( i == itf/2 .AND. j == jtf/2 .AND. k == ktf/2 ) &
!     write(*,'(a,3i6,e15.5)') 'u_ten after=',i,k,j, RUNDGDTEN(i,k,j)
!     if( RUNDGDTEN(i,k,j) > 30.0 ) write(*,*) 'IKJ=',i,k,j
      ENDDO
      ENDDO
      ENDDO
!     write(*,'(a,e15.5)') 'u_ten MAXIMUM after=', maxval(RUNDGDTEN)
      DO J=jtsv,jtf
      DO K=kts,ktf
      DO I=its,itf
         RVNDGDTEN(I,K,J) =muv(I,J)*RVNDGDTEN(I,K,J)
      ENDDO
      ENDDO
      ENDDO
      DO J=jts,jtf
      DO K=kts,ktf
      DO I=its,itf
!     if( i == itf/2 .AND. j == jtf/2 .AND. k == ktf/2 ) &
!     write(*,'(a,3i6,e15.5)') 'th before=',i,k,j, RTHNDGDTEN(I,K,J)
         RTHNDGDTEN(I,K,J)=mu(I,J)*RTHNDGDTEN(I,K,J)
!        RMUNDGDTEN(I,J) - no coupling
!     if( i == itf/2 .AND. j == jtf/2 .AND. k == ktf/2 ) &
!     write(*,'(a,3i6,e15.5)') 'th after=',i,k,j, RTHNDGDTEN(I,K,J)
      ENDDO
      ENDDO
      ENDDO

      IF (P_QV .ge. PARAM_FIRST_SCALAR) THEN
         DO J=jts,jtf
         DO K=kts,ktf
         DO I=its,itf
            RQVNDGDTEN(I,K,J)=mu(I,J)*RQVNDGDTEN(I,K,J)
         ENDDO
         ENDDO
         ENDDO
      ENDIF

    ENDIF

END SUBROUTINE g_calculate_phy_tend

!-----------------------------------------------------------------------

SUBROUTINE g_positive_definite_filter ( a,                          &
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

  END SUBROUTINE g_positive_definite_filter

!-----------------------------------------------------------------------

! Revised by Ning Pan, 2010-08-03
! SUBROUTINE g_bound_tke(tke,g_tke,tke_upper_bound,g_tke_upper_bound,ids,ide, &
 SUBROUTINE g_bound_tke(tke,g_tke,tke_upper_bound,ids,ide, &
 jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 IMPLICIT NONE

 REAL :: Tmpv1,g_Tmpv1
 INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: tke,g_tke
! Revised by Ning Pan, 2010-08-03
! REAL :: tke_upper_bound,g_tke_upper_bound
 REAL :: tke_upper_bound
 INTEGER :: i,k,j

 DO j =jts,min(jte,jde-1)
 DO k =kts,kte-1
 DO i =its,min(ite,ide-1)

! g_tke(i,k,j) =(g_tke_upper_bound +(g_tke(i,k,j) +0.0 +(g_tke(i,k,j) -0.0) &
! *sign(1.0, tke(i,k,j) -(0.)))*0.5 -(g_tke_upper_bound -(g_tke(i,k,j) &
! +0.0 +(g_tke(i,k,j) -0.0)*sign(1.0, tke(i,k,j) -(0.)))*0.5)*sign(1.0, &
! tke_upper_bound -(max(tke(i,k,j),0.))))*0.5
 g_tke(i,k,j) =((g_tke(i,k,j) +0.0 +(g_tke(i,k,j) -0.0) &
 *sign(1.0, tke(i,k,j) -(0.)))*0.5 -(-(g_tke(i,k,j) &
 +0.0 +(g_tke(i,k,j) -0.0)*sign(1.0, tke(i,k,j) -(0.)))*0.5)*sign(1.0, &
 tke_upper_bound -(max(tke(i,k,j),0.))))*0.5
 tke(i,k,j) =min(tke_upper_bound,max(tke(i,k,j),0.))

 ENDDO
 ENDDO
 ENDDO

 END SUBROUTINE g_bound_tke


END MODULE g_module_em
