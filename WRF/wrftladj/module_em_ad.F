!WRF+/AD:MODEL_LAYER:DYNAMICS
!

MODULE a_module_em

   USE module_model_constants
  
   USE a_module_advect_em, only: a_advect_u, a_advect_v, a_advect_w, a_advect_scalar, a_advect_scalar_pd, a_advect_scalar_mono, &
        a_advect_weno_u, a_advect_weno_v, a_advect_weno_w, a_advect_scalar_weno,  a_advect_scalar_wenopd
   
   USE module_big_step_utilities_em, only: calculate_full, calc_mu_uv
   USE a_module_big_step_utilities_em, only: grid_config_rec_type, a_calculate_full, a_couple_momentum, a_calc_mu_uv, a_calc_ww_cp, a_calc_cq, a_calc_alt, a_calc_php, a_set_tend, a_rhs_ph, &
        a_horizontal_pressure_gradient, a_pg_buoy_w, a_w_damp, a_perturbation_coriolis, a_coriolis, a_curvature, a_horizontal_diffusion, a_horizontal_diffusion_3dmp, a_vertical_diffusion_u, &
        a_vertical_diffusion_v, a_vertical_diffusion, a_vertical_diffusion_3dmp, a_sixth_order_diffusion, a_rk_rayleigh_damp, a_theta_relaxation, a_vertical_diffusion_mp, a_zero_tend, a_zero_tend2d

   USE module_state_description, only: param_first_scalar, p_qr, p_qv, p_qc, p_qg, p_qi, p_qs, tiedtkescheme, ntiedtkescheme, heldsuarez, positivedef, &
        gdscheme, g3scheme, kfetascheme, mskfscheme, monotonic, wenopd_scalar, weno_scalar, weno_mom

   !USE module_damping_em, only: held_suarez_damp

CONTAINS

!------------------------------------------------------------------------

   SUBROUTINE a_rk_step_prep(config_flags,rk_step,u,a_u,v,a_v,w,a_w,t,a_t,ph, &
   a_ph,mu,a_mu,moist,a_moist,ru,a_ru,rv,a_rv,rw,a_rw,ww,a_ww,php,a_php, &
   alt,a_alt,muu,a_muu,muv,a_muv,mub,mut,a_mut,phb,pb,p,a_p,al,a_al,alb,cqu, &
   a_cqu,cqv,a_cqv,cqw,a_cqw,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,fnm, &
   fnp,dnw,rdx,rdy,n_moist,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts, &
   jte,kts,kte)

!PART I: DECLARATION OF VARIABLES

   IMPLICIT NONE

   INTEGER :: K0_ADJ,K1_ADJ,K2_ADJ,K3_ADJ
   TYPE(grid_config_rec_type) :: config_flags
   INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
   INTEGER :: n_moist,rk_step
   REAL :: rdx,rdy
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: u,a_u,v,a_v,w,a_w,t,a_t,ph,a_ph, &
   phb,pb,al,a_al,alb
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: ru,a_ru,rv,a_rv,rw,a_rw,ww,a_ww, &
   php,a_php,cqu,a_cqu,cqv,a_cqv,cqw,a_cqw,alt,a_alt
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: p,a_p
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme,n_moist) :: moist,a_moist
   REAL,DIMENSION(ims:ime,jms:jme) :: msftx,msfty,msfux,msfuy,msfvx,msfvx_inv,msfvy,mu, &
   a_mu,mub
   REAL,DIMENSION(ims:ime,jms:jme) :: muu,a_muu,muv,a_muv,mut,a_mut
   REAL,DIMENSION(kms:kme) :: fnm,fnp,dnw
   integer :: k

!PART II: CALCULATIONS OF B. S. TRAJECTORY

!!LPB[0]
!      CALL calculate_full( mut, mub, mu,               &
!                           ids, ide, jds, jde, 1, 2,   &
!                           ims, ime, jms, jme, 1, 1,   &
!                           its, ite, jts, jte, 1, 1 )
!      CALL calc_mu_uv ( config_flags,                    &
!                        mu, mub, muu, muv,               &
!                        ids, ide, jds, jde, kds, kde,    &
!                        ims, ime, jms, jme, kms, kme,    &
!                        its, ite, jts, jte, kts, kte  )
!      CALL couple_momentum( muu, ru, u, msfuy,               &
!                            muv, rv, v, msfvx, msfvx_inv,    &
!                            mut, rw, w, msfty,               &
!                            ids, ide, jds, jde, kds, kde,    &
!                            ims, ime, jms, jme, kms, kme,    &
!                            its, ite, jts, jte, kts, kte  )
!      CALL calc_ww_cp ( u, v, mu, mub, ww,                 &
!                        rdx, rdy, msftx, msfty,            &
!                        msfux, msfuy, msfvx, msfvx_inv,    &
!                        msfvy, dnw,                        &
!                        ids, ide, jds, jde, kds, kde,      &
!                        ims, ime, jms, jme, kms, kme,      &
!                        its, ite, jts, jte, kts, kte    )
!      CALL calc_cq ( moist, cqu, cqv, cqw, n_moist,   &
!                     ids, ide, jds, jde, kds, kde,    &
!                     ims, ime, jms, jme, kms, kme,    &
!                     its, ite, jts, jte, kts, kte  )
!      CALL calc_alt ( alt, al, alb,                   &
!                      ids, ide, jds, jde, kds, kde,   &
!                      ims, ime, jms, jme, kms, kme,   &
!                      its, ite, jts, jte, kts, kte )
!      CALL calc_php ( php, ph, phb,                   &
!                      ids, ide, jds, jde, kds, kde,   &
!                      ims, ime, jms, jme, kms, kme,   &
!                      its, ite, jts, jte, kts, kte )

!PART IV: REVERSE/BACKWARD ACCUMULATIONS

!LPB[0]
   CALL calculate_full(mut,mub,mu,ids,ide,jds,jde,1,2,ims,ime,jms,jme,1,1,its,ite,  &
   jts,jte,1,1)

   CALL calc_mu_uv(config_flags,mu,mub,muu,muv,ids,ide,jds,jde,kds,kde,ims,ime,jms,  &
   jme,kms,kme,its,ite,jts,jte,kts,kte)

!  Remarked by Ning Pan, 2010-07-13
!   CALL couple_momentum(muu,ru,u,msfuy,muv,rv,v,msfvx,msfvx_inv,mut,rw,w,msfty,ids,  &
!   ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   CALL calc_ww_cp(u,v,mu,mub,ww,rdx,rdy,msftx,msfty,msfux,msfuy,msfvx,msfvx_inv,  &
!   msfvy,dnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   CALL calc_cq(moist,cqu,cqv,cqw,n_moist,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
!   kms,kme,its,ite,jts,jte,kts,kte)

!   CALL calc_alt(alt,al,alb,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,  &
!   jts,jte,kts,kte)

!   CALL calc_php(php,ph,phb,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,  &
!   jts,jte,kts,kte)

   CALL a_calc_php(php,a_php,ph,a_ph,phb,ids,ide,jds,jde,kds,kde,ims,ime,jms,  &
   jme,kms,kme,its,ite,jts,jte,kts,kte)
   CALL a_calc_alt(alt,a_alt,al,a_al,alb,ids,ide,jds,jde,kds,kde,ims,ime,jms,  &
   jme,kms,kme,its,ite,jts,jte,kts,kte)
   CALL a_calc_cq(moist,a_moist,cqu,a_cqu,cqv,a_cqv,cqw,a_cqw,n_moist,ids,  &
   ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
   CALL a_calc_ww_cp(u,a_u,v,a_v,mu,a_mu,mub,ww,a_ww,rdx,rdy,msftx,msfty,  &
   msfux,msfuy,msfvx,msfvx_inv,msfvy,dnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
   kme,its,ite,jts,jte,kts,kte)
!   Revised by Ning Pan, 2010-07-13
!   CALL a_couple_momentum(muu,a_muu,ru,a_ru,u,a_u,msfuy,muv,a_muv,rv,  &
!   a_rv,v,a_v,msfvx,msfvx_inv,mut,a_mut,rw,a_rw,w,a_w,msfty,ids,ide,jds,jde,  &
!   kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
   CALL a_couple_momentum(muu,a_muu,a_ru,u,a_u,msfuy,muv,a_muv,  &
   a_rv,v,a_v,msfvx,msfvx_inv,mut,a_mut,a_rw,w,a_w,msfty,ids,ide,jds,jde,  &
   kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
!  Revised by Ning Pan, 2010-07-13
!   CALL a_calc_mu_uv(config_flags,mu,a_mu,mub,muu,a_muu,muv,a_muv,ids,ide,  &
!   jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
   CALL a_calc_mu_uv(config_flags,a_mu,a_muu,a_muv,ids,ide,  &
   jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
!  Revised by Ning Pan, 2010-07-13
!   CALL a_calculate_full(mut,a_mut,mub,mu,a_mu,ids,ide,jds,jde,1,2,ims,ime,jms,  &
!   jme,1,1,its,ite,jts,jte,1,1)
   CALL a_calculate_full(a_mut,a_mu,ids,ide,jds,jde,1,2,ims,ime,jms,  &
   jme,1,1,its,ite,jts,jte,1,1)

   END SUBROUTINE a_rk_step_prep

!-------------------------------------------------------------------------------

   SUBROUTINE a_rk_tendency(config_flags,rk_step,ru_tend,a_ru_tend,rv_tend, &
   a_rv_tend,rw_tend,a_rw_tend,ph_tend,a_ph_tend,t_tend,a_t_tend,ru_tendf, &
   a_ru_tendf,rv_tendf,a_rv_tendf,rw_tendf,a_rw_tendf,ph_tendf,a_ph_tendf, &
   t_tendf,a_t_tendf,mu_tend,a_mu_tend,u_save,a_u_save,v_save,a_v_save,w_save, &
   a_w_save,ph_save,a_ph_save,t_save,a_t_save,mu_save,a_mu_save,RTHFTEN, &
   a_RTHFTEN,ru,a_ru,rv,a_rv,rw,a_rw,ww,a_ww,u,a_u,v,a_v,w,a_w,t,a_t, &
   ph,a_ph,u_old,a_u_old,v_old,a_v_old,w_old,a_w_old,t_old,a_t_old,ph_old, &
! Revised by Ning Pan, 2010-07-30
!   a_ph_old,h_diabatic,a_h_diabatic,phb,t_init,a_t_init,mu,a_mu,mut,a_mut,muu, &
   a_ph_old,h_diabatic,a_h_diabatic,phb,t_init,mu,a_mu,mut,a_mut,muu, &
   a_muu,muv,a_muv,mub,al,a_al,alt,a_alt,p,a_p,pb,php,a_php,cqu,a_cqu,cqv, &
   a_cqv,cqw,a_cqw,u_base,v_base,t_base,qv_base,z_base,msfux,msfuy,msfvx,msfvx_inv, &
! Revised by Ning Pan, 2010-07-30
!   msfvy,msftx,msfty,xlat,a_xlat,f,e,sina,cosa,fnm,fnp,rdn,rdnw,dt,rdx,rdy,khdif, &
!   kvdif,xkmhd,a_xkmhd,xkhh,a_xkhh,diff_6th_opt,diff_6th_factor,a_diff_6th_factor, &
!   dampcoef,a_dampcoef,zdamp,a_zdamp,damp_opt,cf1,cf2,cf3,cfn,cfn1,n_moist, &
!   non_hydrostatic,top_lid,u_frame,a_u_frame,v_frame,a_v_frame,ids,ide,jds,jde,kds, &
!   kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte,max_vert_cfl,a_max_vert_cfl, &
!   max_horiz_cfl,a_max_horiz_cfl)
   msfvy,msftx,msfty,xlat,f,e,sina,cosa,fnm,fnp,rdn,rdnw,dt,rdx,rdy,khdif, &
   kvdif,xkmhd,a_xkmhd,xkhh,a_xkhh,diff_6th_opt,diff_6th_factor, &
   adv_opt,dampcoef,zdamp,damp_opt,rad_nudge,cf1,cf2,cf3,cfn,cfn1,n_moist, &
   non_hydrostatic,top_lid,u_frame,v_frame,ids,ide,jds,jde,kds, &
   kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte,max_vert_cfl, &
   max_horiz_cfl)

! PART I: DECLARATION OF VARIABLES

   IMPLICIT NONE

   INTEGER :: K0_ADJ,K1_ADJ,K2_ADJ,K3_ADJ
   TYPE(grid_config_rec_type) :: config_flags
   INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
   LOGICAL :: non_hydrostatic,top_lid
   INTEGER :: n_moist,rk_step
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: ru,a_ru,rv,a_rv,rw,a_rw,ww,a_ww,u, &
   a_u,v,a_v,w,a_w,t,a_t,ph,a_ph,u_old,a_u_old,v_old,a_v_old,w_old, &
   a_w_old,t_old,a_t_old,ph_old,a_ph_old,phb,al,a_al,alt,a_alt,p,a_p,pb,php, &
! Revised by Ning Pan, 2010-07-30
!   a_php,cqu,a_cqu,cqv,a_cqv,t_init,a_t_init,xkmhd,a_xkmhd,xkhh,a_xkhh, &
   a_php,cqu,a_cqu,cqv,a_cqv,t_init,xkmhd,a_xkmhd,xkhh,a_xkhh, &
   h_diabatic,a_h_diabatic
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: ru_tend,a_ru_tend,rv_tend,a_rv_tend, &
   rw_tend,a_rw_tend,t_tend,a_t_tend,ph_tend,a_ph_tend,RTHFTEN,a_RTHFTEN,u_save, &
   a_u_save,v_save,a_v_save,w_save,a_w_save,ph_save,a_ph_save,t_save,a_t_save
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: ru_tendf,a_ru_tendf,rv_tendf, &
   a_rv_tendf,rw_tendf,a_rw_tendf,t_tendf,a_t_tendf,ph_tendf,a_ph_tendf,cqw,a_cqw
   REAL,DIMENSION(ims:ime,jms:jme) :: mu_tend,a_mu_tend,mu_save,a_mu_save
   REAL,DIMENSION(ims:ime,jms:jme) :: msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty, &
! Revised by Ning Pan, 2010-07-30
!   xlat,a_xlat,f,e,sina,cosa,mu,a_mu,mut,a_mut,mub,muu,a_muu,muv,a_muv
   xlat,f,e,sina,cosa,mu,a_mu,mut,a_mut,mub,muu,a_muu,muv,a_muv
   REAL,DIMENSION(kms:kme) :: fnm,fnp,rdn,rdnw,u_base,v_base,t_base,qv_base,z_base
! Revised by Ning Pan, 2010-07-30
!   REAL :: rdx,rdy,dt,u_frame,a_u_frame,v_frame,a_v_frame,khdif,kvdif
   REAL :: rdx,rdy,dt,u_frame,v_frame,khdif,kvdif
   INTEGER :: diff_6th_opt
! Revised by Ning Pan, 2010-07-30
!   REAL :: diff_6th_factor,a_diff_6th_factor
   REAL :: diff_6th_factor
   INTEGER :: adv_opt
   INTEGER :: damp_opt,rad_nudge
! Revised by Ning Pan, 2010-07-30
!   REAL :: zdamp,a_zdamp,dampcoef,a_dampcoef
!   REAL :: max_horiz_cfl,a_max_horiz_cfl
!   REAL :: max_vert_cfl,a_max_vert_cfl
   REAL :: zdamp,dampcoef
   REAL :: max_horiz_cfl
   REAL :: max_vert_cfl
! Revised by Ning Pan, 2010-07-30
!   REAL :: kdift,a_kdift,khdq,a_khdq,kvdq,a_kvdq,cfn,cfn1,cf1,cf2,cf3
   REAL :: kdift,khdq,kvdq,cfn,cfn1,cf1,cf2,cf3
   INTEGER :: i,j,k
   INTEGER :: time_step

! Remarked by Ning Pan, 2010-07-30
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb0_u
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb0_v
!   REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb1_w   
!   REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb1_rw_tend
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb2_t   
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb2_t_tend
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb2_ru
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb2_rv   
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb4_ph_tend   
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb4_ru_tend   
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb4_rv_tend   
!   REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb5_rw_tend   
!   REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb5_cqw   
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb6_rw_tend   
!   REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb7_ru_tend   
!   REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb7_rv_tend   
!   REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb7_rw_tend   
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb8_ru_tend   
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb8_rv_tend   
!   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: Keep_Lpb8_rw_tend   
!   REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb9_ru_tend   
!   REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb9_rv_tend   
!!  REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb11_ru_tendf   
!!  REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb11_rv_tendf   
!!  REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb11_rw_tendf   
!!  REAL,DIMENSION(1,ims:ime,kms:kme,jms:jme) :: Keep_Lpb11_t_tendf   
!   INTEGER :: IX1,IX2,IX3
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv400
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv401
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv402
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv403
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv404
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv405
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv406
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv407
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv408
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv409
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv4010
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv4011
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv4012
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv4013
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv4014
!   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv4015

!   REAL :: Const_Diff_A0,Const_A0

! This line is fail to be recognized
      CALL nl_get_time_step ( 1, time_step )

! Remarked by Ning Pan, 2010-07-30 : Part II is not needed
!! PART! II: CALCULATIONS OF B. S. TRAJECTORY

!! LPB[0]
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb0_u(IX1,IX2,IX3) =u(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb0_v(IX1,IX2,IX3) =v(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!      CALL zero_tend ( ru_tend,                        &
!                       ids, ide, jds, jde, kds, kde,   &
!                       ims, ime, jms, jme, kms, kme,   &
!                       its, ite, jts, jte, kts, kte )
!      CALL zero_tend ( rv_tend,                        &
!                       ids, ide, jds, jde, kds, kde,   &
!                       ims, ime, jms, jme, kms, kme,   &
!                       its, ite, jts, jte, kts, kte )
!      CALL zero_tend ( rw_tend,                        &
!                       ids, ide, jds, jde, kds, kde,   &
!                       ims, ime, jms, jme, kms, kme,   &
!                       its, ite, jts, jte, kts, kte )
!      CALL zero_tend ( t_tend,                         &
!                       ids, ide, jds, jde, kds, kde,   &
!                       ims, ime, jms, jme, kms, kme,   &
!                       its, ite, jts, jte, kts, kte )
!      CALL zero_tend ( ph_tend,                        &
!                       ids, ide, jds, jde, kds, kde,   &
!                       ims, ime, jms, jme, kms, kme,   &
!                       its, ite, jts, jte, kts, kte )
!      CALL zero_tend ( u_save,                         &
!                       ids, ide, jds, jde, kds, kde,   &
!                       ims, ime, jms, jme, kms, kme,   &
!                       its, ite, jts, jte, kts, kte )
!      CALL zero_tend ( v_save,                         &
!                       ids, ide, jds, jde, kds, kde,   &
!                       ims, ime, jms, jme, kms, kme,   &
!                       its, ite, jts, jte, kts, kte )
!      CALL zero_tend ( w_save,                         &
!                       ids, ide, jds, jde, kds, kde,   &
!                       ims, ime, jms, jme, kms, kme,   &
!                       its, ite, jts, jte, kts, kte )
!      CALL zero_tend ( ph_save,                         &
!                       ids, ide, jds, jde, kds, kde,   &
!                       ims, ime, jms, jme, kms, kme,   &
!                       its, ite, jts, jte, kts, kte )
!      CALL zero_tend ( t_save,                         &
!                       ids, ide, jds, jde, kds, kde,   &
!                       ims, ime, jms, jme, kms, kme,   &
!                       its, ite, jts, jte, kts, kte )
!      CALL zero_tend ( mu_tend,                    &
!                       ids, ide, jds, jde, 1, 1,   &
!                       ims, ime, jms, jme, 1, 1,   &
!                       its, ite, jts, jte, 1, 1 )
!      CALL zero_tend ( mu_save,                    &
!                       ids, ide, jds, jde, 1, 1,   &
!                       ims, ime, jms, jme, 1, 1,   &
!                       its, ite, jts, jte, 1, 1 )
!      CALL advect_u ( u, u , ru_tend, ru, rv, ww,     &
!                      mut, time_step, config_flags,   &
!                      msfux, msfuy, msfvx, msfvy,     &
!                      msftx, msfty,                   &
!                      fnm, fnp, rdx, rdy, rdnw,       &
!                      ids, ide, jds, jde, kds, kde,   &
!                      ims, ime, jms, jme, kms, kme,   &
!                      its, ite, jts, jte, kts, kte )
!      CALL advect_v ( v, v , rv_tend, ru, rv, ww,     &
!                      mut, time_step, config_flags,   &
!                      msfux, msfuy, msfvx, msfvy,     &
!                      msftx, msfty,                   &
!                      fnm, fnp, rdx, rdy, rdnw,       &
!                      ids, ide, jds, jde, kds, kde,   &
!                      ims, ime, jms, jme, kms, kme,   &
!                      its, ite, jts, jte, kts, kte )

!LPB[1]
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb1_w(IX1,IX2,IX3) =w(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb1_rw_tend(IX1,IX2,IX3) =rw_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   IF (non_hydrostatic)                            &
!     CALL advect_w ( w, w, rw_tend, ru, rv, ww,      &
!                     mut, time_step, config_flags,   &
!                     msfux, msfuy, msfvx, msfvy,     &
!                     msftx, msfty,                   &
!                     fnm, fnp, rdx, rdy, rdn,        &
!                     ids, ide, jds, jde, kds, kde,   &
!                     ims, ime, jms, jme, kms, kme,   &
!                     its, ite, jts, jte, kts, kte )

!LPB[2]
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb2_t(IX1,IX2,IX3) =t(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb2_t_tend(IX1,IX2,IX3) =t_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb2_ru(IX1,IX2,IX3) =ru(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb2_rv(IX1,IX2,IX3) =rv(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!        CALL advect_scalar ( t, t, t_tend, ru, rv, ww,       &
!                             mut, time_step, config_flags,   &
!                             msfux, msfuy, msfvx, msfvy,     &
!                             msftx, msfty, fnm, fnp,         &
!                             rdx, rdy, rdnw,                 &
!                             ids, ide, jds, jde, kds, kde,   &
!                             ims, ime, jms, jme, kms, kme,   &
!                             its, ite, jts, jte, kts, kte ) 

!LPB[3]
!     IF ( config_flags%cu_physics == GDSCHEME  .OR.       &
!          config_flags%cu_physics == G3SCHEME ) THEN

!            CALL set_tend( RTHFTEN, t_tend, msfty,            &
!                           ids, ide, jds, jde, kds, kde,      &
!                           ims, ime, jms, jme, kms, kme,      &
!                           its, ite, jts, jte, kts, kte     )

!   END IF

!LPB[4]
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb4_ph_tend(IX1,IX2,IX3) =ph_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb4_ru_tend(IX1,IX2,IX3) =ru_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb4_rv_tend(IX1,IX2,IX3) =rv_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!        CALL rhs_ph( ph_tend, u, v, ww, ph, ph, phb, w,   &
!                     mut, muu, muv,                       &
!                     fnm, fnp,                            &
!                     rdnw, cfn, cfn1, rdx, rdy,           &
!                     msfux, msfuy, msfvx,                 &
!                     msfvx_inv, msfvy,                    &
!                     msftx, msfty,                        &
!                     non_hydrostatic,                     &
!                     config_flags,                        &
!                     ids, ide, jds, jde, kds, kde,        &
!                     ims, ime, jms, jme, kms, kme,        &
!                     its, ite, jts, jte, kts, kte      )
!        CALL horizontal_pressure_gradient( ru_tend,rv_tend,                  &
!                                            ph,alt,p,pb,al,php,cqu,cqv,       &
!                                            muu,muv,mu,fnm,fnp,rdnw,          &
!                                            cf1,cf2,cf3,cfn,cfn1,             &
!                                            rdx,rdy,msfux,msfuy,              &
!                                            msfvx,msfvy,msftx,msfty,          &
!                                            config_flags, non_hydrostatic,    &
!                                            top_lid,                          &
!                                            ids, ide, jds, jde, kds, kde,     &
!                                            ims, ime, jms, jme, kms, kme,     &
!                                            its, ite, jts, jte, kts, kte   )

!LPB[5]
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb5_rw_tend(IX1,IX2,IX3) =rw_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb5_cqw(IX1,IX2,IX3) =cqw(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!     IF (non_hydrostatic) THEN

!             CALL pg_buoy_w( rw_tend, p, cqw, mu, mub,         &
!                             rdnw, rdn, g, msftx, msfty,       &
!                             ids, ide, jds, jde, kds, kde,     &
!                             ims, ime, jms, jme, kms, kme,     &
!                             its, ite, jts, jte, kts, kte   )

!   ENDIF

!LPB[6]
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb6_rw_tend(IX1,IX2,IX3) =rw_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!        CALL w_damp   ( rw_tend, max_vert_cfl,              &
!                         max_horiz_cfl,                    &
!                         u, v, ww, w, mut, rdnw,           &
!                         rdx, rdy, msfux, msfuy, msfvx,    &
!                         msfvy, dt, config_flags,          &
!                         ids, ide, jds, jde, kds, kde,     &
!                         ims, ime, jms, jme, kms, kme,     &
!                         its, ite, jts, jte, kts, kte   )

!LPB[7]
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb7_ru_tend(IX1,IX2,IX3) =ru_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb7_rv_tend(IX1,IX2,IX3) =rv_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb7_rw_tend(IX1,IX2,IX3) =rw_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!     IF(config_flags%pert_coriolis) THEN

!             CALL perturbation_coriolis ( ru, rv, rw,                     &
!                                          ru_tend,  rv_tend,  rw_tend,    &
!                                          config_flags,                   &
!                                          u_base, v_base, z_base,         &
!                                          muu, muv, phb, ph,              &
!                                          msftx, msfty, msfux, msfuy,     &
!                                          msfvx, msfvy,                   &
!                                          f, e, sina, cosa, fnm, fnp,     &
!                                          ids, ide, jds, jde, kds, kde,   &
!                                          ims, ime, jms, jme, kms, kme,   &
!                                          its, ite, jts, jte, kts, kte )
!        ELSE
!             CALL coriolis ( ru, rv, rw,                     &
!                             ru_tend,  rv_tend,  rw_tend,    &
!                             config_flags,                   &
!                             msftx, msfty, msfux, msfuy,     &
!                             msfvx, msfvy,                   &
!                             f, e, sina, cosa, fnm, fnp,     &
!                             ids, ide, jds, jde, kds, kde,   &
!                             ims, ime, jms, jme, kms, kme,   &
!                             its, ite, jts, jte, kts, kte )

!   END IF

!LPB[8]
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb8_ru_tend(IX1,IX2,IX3) =ru_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb8_rv_tend(IX1,IX2,IX3) =rv_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb8_rw_tend(IX1,IX2,IX3) =rw_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!        CALL curvature ( ru, rv, rw, u, v, w,              &
!                          ru_tend,  rv_tend,  rw_tend,      &
!                          config_flags,                     &
!                          msfux, msfuy, msfvx, msfvy,       &
!                          msftx, msfty,                     &
!                          xlat, fnm, fnp, rdx, rdy,         &
!                          ids, ide, jds, jde, kds, kde,     &
!                          ims, ime, jms, jme, kms, kme,     &
!                          its, ite, jts, jte, kts, kte   )

!LPB[9]
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb9_ru_tend(IX1,IX2,IX3) =ru_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!       Keep_Lpb9_rv_tend(IX1,IX2,IX3) =rv_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!      IF (config_flags%ra_lw_physics == HELDSUAREZ) THEN

!            CALL held_suarez_damp ( ru_tend, rv_tend,                    &
!                                    ru,rv,p,pb,                       &
!                                    ids, ide, jds, jde, kds, kde,     &
!                                    ims, ime, jms, jme, kms, kme,     &
!                                    its, ite, jts, jte, kts, kte   )

!   END IF

!LPB[10]

!!LPB[11]
!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!    !  Keep_Lpb11_ru_tendf(IX1,IX2,IX3) =ru_tendf(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO
!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!    !  Keep_Lpb11_rv_tendf(IX1,IX2,IX3) =rv_tendf(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO
!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!    !  Keep_Lpb11_rw_tendf(IX1,IX2,IX3) =rw_tendf(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO
!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!    !  Keep_Lpb11_t_tendf(IX1,IX2,IX3) =t_tendf(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO

!   
!  forward_step: IF( rk_step == 1 ) THEN

!    diff_opt1 : IF (config_flags%diff_opt .eq. 1) THEN

!           CALL horizontal_diffusion ('u', u, ru_tendf, mut, config_flags,   &
!                                           msfux, msfuy, msfvx, msfvx_inv,   &
!                                           msfvy,msftx, msfty,               &
!                                           khdif, xkmhd, rdx, rdy,           &
!                                           ids, ide, jds, jde, kds, kde,     &
!                                           ims, ime, jms, jme, kms, kme,     &
!                                           its, ite, jts, jte, kts, kte   )
!           CALL horizontal_diffusion ('v', v, rv_tendf, mut, config_flags,   &
!                                           msfux, msfuy, msfvx, msfvx_inv,   &
!                                           msfvy,msftx, msfty,               &
!                                           khdif, xkmhd, rdx, rdy,           &
!                                           ids, ide, jds, jde, kds, kde,     &
!                                           ims, ime, jms, jme, kms, kme,     &
!                                           its, ite, jts, jte, kts, kte   )
!           CALL horizontal_diffusion ('w', w, rw_tendf, mut, config_flags,   &
!                                           msfux, msfuy, msfvx, msfvx_inv,   &
!                                           msfvy,msftx, msfty,               &
!                                           khdif, xkmhd, rdx, rdy,           &
!                                           ids, ide, jds, jde, kds, kde,     &
!                                           ims, ime, jms, jme, kms, kme,     &
!                                           its, ite, jts, jte, kts, kte   )
!           khdq = 3.*khdif
!           CALL horizontal_diffusion_3dmp ( 'm', t, t_tendf, mut,              &
!                                            config_flags, t_init,              &
!                                            msfux, msfuy, msfvx, msfvx_inv,    &
!                                            msfvy, msftx, msfty,               &
!                                            khdq , xkhh, rdx, rdy,             &
!                                            ids, ide, jds, jde, kds, kde,      &
!                                            ims, ime, jms, jme, kms, kme,      &
!                                            its, ite, jts, jte, kts, kte    )
!        pbl_test : IF (config_flags%bl_pbl_physics .eq. 0) THEN

!             CALL vertical_diffusion_u ( u, ru_tendf, config_flags,        &
!                                         u_base,                           &
!                                         alt, muu, rdn, rdnw, kvdif,       &
!                                         ids, ide, jds, jde, kds, kde,     &
!                                         ims, ime, jms, jme, kms, kme,     &
!                                         its, ite, jts, jte, kts, kte   )
!             CALL vertical_diffusion_v ( v, rv_tendf, config_flags,        &
!                                         v_base,                           &
!                                         alt, muv, rdn, rdnw, kvdif,       &
!                                         ids, ide, jds, jde, kds, kde,     &
!                                         ims, ime, jms, jme, kms, kme,     &
!                                         its, ite, jts, jte, kts, kte   )
!          IF (non_hydrostatic)                                             &
!          CALL vertical_diffusion ( 'w', w, rw_tendf, config_flags,        &
!                                    alt, mut, rdn, rdnw, kvdif,            &
!                                    ids, ide, jds, jde, kds, kde,          &
!                                    ims, ime, jms, jme, kms, kme,          &
!                                    its, ite, jts, jte, kts, kte        )

!             kvdq = 3.*kvdif
!             CALL vertical_diffusion_3dmp ( t, t_tendf, config_flags, t_init,       &
!                                            alt, mut, rdn, rdnw, kvdq ,             &
!                                            ids, ide, jds, jde, kds, kde,           &
!                                            ims, ime, jms, jme, kms, kme,           &
!                                            its, ite, jts, jte, kts, kte         )
!           ENDIF pbl_test
!       END IF diff_opt1
!    IF ( diff_6th_opt .NE. 0 ) THEN

!         CALL sixth_order_diffusion( 'u', u, ru_tendf, mut, dt,            &
!                                          config_flags,                    &
!                                          diff_6th_opt, diff_6th_factor,   &
!                                          ids, ide, jds, jde, kds, kde,    &
!                                          ims, ime, jms, jme, kms, kme,    &
!                                          its, ite, jts, jte, kts, kte )
!         CALL sixth_order_diffusion( 'v', v, rv_tendf, mut, dt,            &
!                                          config_flags,                    &
!                                          diff_6th_opt, diff_6th_factor,   &
!                                          ids, ide, jds, jde, kds, kde,    &
!                                          ims, ime, jms, jme, kms, kme,    &
!                                          its, ite, jts, jte, kts, kte )
!      IF (non_hydrostatic)                                               &
!      CALL sixth_order_diffusion( 'w', w, rw_tendf, mut, dt,            &
!                                       config_flags,                    &
!                                       diff_6th_opt, diff_6th_factor,   &
!                                       ids, ide, jds, jde, kds, kde,    &
!                                       ims, ime, jms, jme, kms, kme,    &
!                                       its, ite, jts, jte, kts, kte )

!         CALL sixth_order_diffusion( 'm', t,  t_tendf, mut, dt,            &
!                                          config_flags,                    &
!                                          diff_6th_opt, diff_6th_factor,   &
!                                          ids, ide, jds, jde, kds, kde,    &
!                                          ims, ime, jms, jme, kms, kme,    &
!                                          its, ite, jts, jte, kts, kte )
!       ENDIF
!    IF( damp_opt .eq. 2 )                                        &
!       CALL rk_rayleigh_damp( ru_tendf, rv_tendf,                &
!                              rw_tendf, t_tendf,                 &
!                              u, v, w, t, t_init,                &
!                              mut, muu, muv, ph, phb,            &
!                              u_base, v_base, t_base, z_base,    &
!                              dampcoef, zdamp,                   &
!                              ids, ide, jds, jde, kds, kde,      &
!                              ims, ime, jms, jme, kms, kme,      &
!                              its, ite, jts, jte, kts, kte   )

!   END IF

!PART III: INITIALIZATION OF LOCAL ADJOINT PERTURBATIONS

! Remarked by Ning Pan, 2010-07-30
!   a_kdift =0.0
!   a_khdq =0.0
!   a_kvdq =0.0

!PART IV: REVERSE/BACKWARD ACCUMULATIONS

!LPB[11]
!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  ru_tendf(IX1,IX2,IX3) =Keep_Lpb11_ru_tendf(IX1,IX2,IX3)
!  END DO
!  END DO
!  END DO
!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  rv_tendf(IX1,IX2,IX3) =Keep_Lpb11_rv_tendf(IX1,IX2,IX3)
!  END DO
!  END DO
!  END DO
!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  rw_tendf(IX1,IX2,IX3) =Keep_Lpb11_rw_tendf(IX1,IX2,IX3)
!  END DO
!  END DO
!  END DO
!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  t_tendf(IX1,IX2,IX3) =Keep_Lpb11_t_tendf(IX1,IX2,IX3)
!  END DO
!  END DO
!  END DO

! Remarked by Ning Pan, 2010-07-30
!   IF( rk_step == 1 ) THEN
!   IF(config_flags%diff_opt .eq. 1) THEN
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv400(IX1,IX2,IX3) =ru_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL horizontal_diffusion('u',u,ru_tendf,mut,config_flags,msfux,msfuy,msfvx,  &
!   msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,  &
!   jme,kms,kme,its,ite,jts,jte,kts,kte)

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv401(IX1,IX2,IX3) =rv_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL horizontal_diffusion('v',v,rv_tendf,mut,config_flags,msfux,msfuy,msfvx,  &
!   msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,  &
!   jme,kms,kme,its,ite,jts,jte,kts,kte)

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv402(IX1,IX2,IX3) =rw_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL horizontal_diffusion('w',w,rw_tendf,mut,config_flags,msfux,msfuy,msfvx,  &
!   msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,  &
!   jme,kms,kme,its,ite,jts,jte,kts,kte)

!   khdq =3.*khdif

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv403(IX1,IX2,IX3) =t_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL horizontal_diffusion_3dmp('m',t,t_tendf,mut,config_flags,t_init,msfux,msfuy,  &
!   msfvx,msfvx_inv,msfvy,msftx,msfty,khdq,xkhh,rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,  &
!   jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   IF(config_flags%bl_pbl_physics .eq. 0) THEN
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv404(IX1,IX2,IX3) =ru_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL vertical_diffusion_u(u,ru_tendf,config_flags,u_base,alt,muu,rdn,rdnw,kvdif,  &
!   ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv405(IX1,IX2,IX3) =rv_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL vertical_diffusion_v(v,rv_tendf,config_flags,v_base,alt,muv,rdn,rdnw,kvdif,  &
!   ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   IF(non_hydrostatic) THEN
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv406(IX1,IX2,IX3) =rw_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL vertical_diffusion('w',w,rw_tendf,config_flags,alt,mut,rdn,rdnw,kvdif,ids,  &
!   ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   END IF
!   kvdq =3.*kvdif

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv407(IX1,IX2,IX3) =t_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL vertical_diffusion_3dmp(t,t_tendf,config_flags,t_init,alt,mut,rdn,rdnw,kvdq,  &
!   ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   ENDIF
!   END IF
!   IF( diff_6th_opt .NE. 0 ) THEN
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv408(IX1,IX2,IX3) =ru_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL sixth_order_diffusion('u',u,ru_tendf,mut,dt,config_flags,diff_6th_opt,  &
!   diff_6th_factor,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv409(IX1,IX2,IX3) =rv_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL sixth_order_diffusion('v',v,rv_tendf,mut,dt,config_flags,diff_6th_opt,  &
!   diff_6th_factor,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   IF(non_hydrostatic) THEN
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv4010(IX1,IX2,IX3) =rw_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL sixth_order_diffusion('w',w,rw_tendf,mut,dt,config_flags,diff_6th_opt,  &
!   diff_6th_factor,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   END IF
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv4011(IX1,IX2,IX3) =t_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL sixth_order_diffusion('m',t,t_tendf,mut,dt,config_flags,diff_6th_opt,  &
!   diff_6th_factor,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   ENDIF
!   IF( damp_opt .eq. 2 ) THEN
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv4012(IX1,IX2,IX3) =ru_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv4013(IX1,IX2,IX3) =rv_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv4014(IX1,IX2,IX3) =rw_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv4015(IX1,IX2,IX3) =t_tendf(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL rk_rayleigh_damp(ru_tendf,rv_tendf,rw_tendf,t_tendf,u,v,w,t,t_init,mut,muu,  &
!   muv,ph,phb,u_base,v_base,t_base,z_base,dampcoef,zdamp,ids,ide,jds,jde,kds,kde,ims,  &
!   ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   END IF
!   END IF

!! temp NING
   IF( rk_step == 1 ) THEN

   IF( rad_nudge .eq. 1 )                                     &
       CALL a_theta_relaxation( t_tendf, a_t_tendf, t, a_t, t_init,  &
                              mut, a_mut, ph, a_ph, phb,       &
                              t_base, z_base,                  &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              its, ite, jts, jte, kts, kte   )

   IF( damp_opt .eq. 2 ) THEN

! Remarked by Ning Pan, 2010-07-30
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   t_tendf(IX1,IX2,IX3) =Tmpv4015(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rw_tendf(IX1,IX2,IX3) =Tmpv4014(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rv_tendf(IX1,IX2,IX3) =Tmpv4013(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   ru_tendf(IX1,IX2,IX3) =Tmpv4012(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

   CALL a_rk_rayleigh_damp(ru_tendf,a_ru_tendf,rv_tendf,a_rv_tendf,rw_tendf,  &
! Revised by Ning Pan, 2010-07-23
!   a_rw_tendf,t_tendf,a_t_tendf,u,a_u,v,a_v,w,a_w,t,a_t,t_init,a_t_init,  &
   a_rw_tendf,t_tendf,a_t_tendf,u,a_u,v,a_v,w,a_w,t,a_t,t_init,  &
   mut,a_mut,muu,a_muu,muv,a_muv,ph,a_ph,phb,u_base,v_base,t_base,z_base,  &
! Revised by Ning Pan, 2010-07-30
!   dampcoef,a_dampcoef,zdamp,a_zdamp,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
   dampcoef,zdamp,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
   kme,its,ite,jts,jte,kts,kte)

   END IF

   IF( diff_6th_opt .NE. 0 ) THEN

! Remarked by Ning Pan, 2010-07-30
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   t_tendf(IX1,IX2,IX3) =Tmpv4011(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

   CALL a_sixth_order_diffusion('m',t,a_t,t_tendf,a_t_tendf,mut,a_mut,dt,  &
! Revised by Ning Pan, 2010-07-30
!   config_flags,diff_6th_opt,diff_6th_factor,a_diff_6th_factor,ids,ide,jds,jde,kds,  &
   config_flags,diff_6th_opt,diff_6th_factor,ids,ide,jds,jde,kds,  &
   kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   IF(non_hydrostatic) THEN

! Remarked by Ning Pan, 2010-07-30
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rw_tendf(IX1,IX2,IX3) =Tmpv4010(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

   CALL a_sixth_order_diffusion('w',w,a_w,rw_tendf,a_rw_tendf,mut,a_mut,dt,  &
! Revised by Ning Pan, 2010-07-30
!   config_flags,diff_6th_opt,diff_6th_factor,a_diff_6th_factor,ids,ide,jds,jde,kds,  &
   config_flags,diff_6th_opt,diff_6th_factor,ids,ide,jds,jde,kds,  &
   kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   END IF

! Remarked by Ning Pan, 2010-07-30
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rv_tendf(IX1,IX2,IX3) =Tmpv409(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

   CALL a_sixth_order_diffusion('v',v,a_v,rv_tendf,a_rv_tendf,mut,a_mut,dt,  &
! Revised by Ning Pan, 2010-07-30
!   config_flags,diff_6th_opt,diff_6th_factor,a_diff_6th_factor,ids,ide,jds,jde,kds,  &
   config_flags,diff_6th_opt,diff_6th_factor,ids,ide,jds,jde,kds,  &
   kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

! Remarked by Ning Pan, 2010-07-30
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   ru_tendf(IX1,IX2,IX3) =Tmpv408(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

   CALL a_sixth_order_diffusion('u',u,a_u,ru_tendf,a_ru_tendf,mut,a_mut,dt,  &
! Revised by Ning Pan, 2010-07-30
!   config_flags,diff_6th_opt,diff_6th_factor,a_diff_6th_factor,ids,ide,jds,jde,kds,  &
   config_flags,diff_6th_opt,diff_6th_factor,ids,ide,jds,jde,kds,  &
   kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   ENDIF

   IF(config_flags%diff_opt .eq. 1) THEN

! Revised by Ning Pan, 2010-07-30 : reverse the adjoint computation order
!                                   revise actual arguments
!                                   remark useless recalculation
   IF(config_flags%bl_pbl_physics .eq. 0) THEN

   kvdq = 3.*kvdif
   CALL a_vertical_diffusion_3dmp(t,a_t,t_tendf,a_t_tendf,config_flags,t_init,  &
   alt,a_alt,mut,a_mut,rdn,rdnw,kvdq,ids,ide,jds,jde,kds,kde,  &
   ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   IF(non_hydrostatic) THEN
      CALL a_vertical_diffusion('w',w,a_w,rw_tendf,a_rw_tendf,config_flags,alt,  &
   a_alt,mut,a_mut,rdn,rdnw,kvdif,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,  &
   its,ite,jts,jte,kts,kte)
   END IF

   CALL a_vertical_diffusion_v(v,a_v,rv_tendf,a_rv_tendf,config_flags,v_base,  &
   alt,a_alt,muv,a_muv,rdn,rdnw,kvdif,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
   kme,its,ite,jts,jte,kts,kte)

   CALL a_vertical_diffusion_u(u,a_u,ru_tendf,a_ru_tendf,config_flags,u_base,  &
   alt,a_alt,muu,a_muu,rdn,rdnw,kvdif,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
   kme,its,ite,jts,jte,kts,kte)

   ENDIF

   khdq = 3.*khdif
   CALL a_horizontal_diffusion_3dmp('m',t,a_t,t_tendf,a_t_tendf,mut,a_mut,  &
   config_flags,t_init,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdq,  &
   xkhh,a_xkhh,rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,  &
   ite,jts,jte,kts,kte)

   CALL a_horizontal_diffusion('w',w,a_w,rw_tendf,a_rw_tendf,mut,a_mut,  &
   config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,a_xkmhd,rdx,  &
   rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
 
   CALL a_horizontal_diffusion('v',v,a_v,rv_tendf,a_rv_tendf,mut,a_mut,  &
   config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,a_xkmhd,rdx,  &
   rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   CALL a_horizontal_diffusion('u',u,a_u,ru_tendf,a_ru_tendf,mut,a_mut,  &
   config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,a_xkmhd,rdx,  &
   rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   ru_tendf(IX1,IX2,IX3) =Tmpv400(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL a_horizontal_diffusion('u',u,a_u,ru_tendf,a_ru_tendf,mut,a_mut,  &
!   config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,a_xkmhd,rdx,  &
!   rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rv_tendf(IX1,IX2,IX3) =Tmpv401(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL a_horizontal_diffusion('v',v,a_v,rv_tendf,a_rv_tendf,mut,a_mut,  &
!   config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,a_xkmhd,rdx,  &
!   rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rw_tendf(IX1,IX2,IX3) =Tmpv402(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL a_horizontal_diffusion('w',w,a_w,rw_tendf,a_rw_tendf,mut,a_mut,  &
!   config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdif,xkmhd,a_xkmhd,rdx,  &
!   rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
!   a_khdq =0.0

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   t_tendf(IX1,IX2,IX3) =Tmpv403(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL a_horizontal_diffusion_3dmp('m',t,a_t,t_tendf,a_t_tendf,mut,a_mut,  &
!   config_flags,t_init,a_t_init,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdq,  &
!   a_khdq,xkhh,a_xkhh,rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,  &
!   ite,jts,jte,kts,kte)

!   IF(config_flags%bl_pbl_physics .eq. 0) THEN

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   ru_tendf(IX1,IX2,IX3) =Tmpv404(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL a_vertical_diffusion_u(u,a_u,ru_tendf,a_ru_tendf,config_flags,u_base,  &
!   alt,a_alt,muu,a_muu,rdn,rdnw,kvdif,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
!   kme,its,ite,jts,jte,kts,kte)

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rv_tendf(IX1,IX2,IX3) =Tmpv405(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL a_vertical_diffusion_v(v,a_v,rv_tendf,a_rv_tendf,config_flags,v_base,  &
!   alt,a_alt,muv,a_muv,rdn,rdnw,kvdif,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
!   kme,its,ite,jts,jte,kts,kte)

!   IF(non_hydrostatic) THEN

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rw_tendf(IX1,IX2,IX3) =Tmpv406(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL a_vertical_diffusion('w',w,a_w,rw_tendf,a_rw_tendf,config_flags,alt,  &
!   a_alt,mut,a_mut,rdn,rdnw,kvdif,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,  &
!   its,ite,jts,jte,kts,kte)

!   END IF
!   a_kvdq =0.0

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   t_tendf(IX1,IX2,IX3) =Tmpv407(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL a_vertical_diffusion_3dmp(t,a_t,t_tendf,a_t_tendf,config_flags,t_init,  &
!   a_t_init,alt,a_alt,mut,a_mut,rdn,rdnw,kvdq,a_kvdq,ids,ide,jds,jde,kds,kde,  &
!   ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   ENDIF

   END IF

   END IF

!LPB[10]

!LPB[9]
! Remarked by Ning Pan, 2010-07-30
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   ru_tend(IX1,IX2,IX3) =Keep_Lpb9_ru_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rv_tend(IX1,IX2,IX3) =Keep_Lpb9_rv_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!  IF(config_flags%ra_lw_physics == HELDSUAREZ) THEN
!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  Tmpv400(IX1,IX2,IX3) =ru_tend(IX1,IX2,IX3)
!  END DO
!  END DO
!  END DO

!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  Tmpv401(IX1,IX2,IX3) =rv_tend(IX1,IX2,IX3)
!  END DO
!  END DO
!  END DO

!  CALL held_suarez_damp(ru_tend,rv_tend,ru,rv,p,pb,ids,ide,jds,jde,kds,kde,ims,ime,  &
!  jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!  END IF

   IF(config_flags%ra_lw_physics == HELDSUAREZ) THEN

! Remarked by Ning Pan, 2010-07-30
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rv_tend(IX1,IX2,IX3) =Tmpv401(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   ru_tend(IX1,IX2,IX3) =Tmpv400(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Reamarked by Ning Pan, 2010-07-30 : JUST FOR DEBUGGING DYNAMICS OF WRF+   !!!
!!!              REMARK SHOULD BE REMOVED WHEN CONSTRUCTING PHYSICS OF WRF+   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   CALL a_held_suarez_damp(ru_tend,a_ru_tend,rv_tend,a_rv_tend,ru,a_ru,rv,  &
!   a_rv,p,a_p,pb,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   END IF

!LPB[8]
! Remarked by Ning Pan, 2010-07-30
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   ru_tend(IX1,IX2,IX3) =Keep_Lpb8_ru_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rv_tend(IX1,IX2,IX3) =Keep_Lpb8_rv_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rw_tend(IX1,IX2,IX3) =Keep_Lpb8_rw_tend(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  Tmpv400(IX1,IX2,IX3) =ru_tend(IX1,IX2,IX3)
!  END DO
!  END DO
!  END DO

!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  Tmpv401(IX1,IX2,IX3) =rv_tend(IX1,IX2,IX3)
!  END DO
!  END DO
!  END DO

!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  Tmpv402(IX1,IX2,IX3) =rw_tend(IX1,IX2,IX3)
!  END DO
!  END DO
!  END DO

!  CALL curvature(ru,rv,rw,u,v,w,ru_tend,rv_tend,rw_tend,config_flags,msfux,msfuy,  &
!  msfvx,msfvy,msftx,msfty,xlat,fnm,fnp,rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
!  kms,kme,its,ite,jts,jte,kts,kte)

! Remarked by Ning Pan, 2010-07-30
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rw_tend(IX1,IX2,IX3) =Tmpv402(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rv_tend(IX1,IX2,IX3) =Tmpv401(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   ru_tend(IX1,IX2,IX3) =Tmpv400(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

   CALL a_curvature(ru,a_ru,rv,a_rv,rw,a_rw,u,a_u,v,a_v,w,ru_tend,  &
   a_ru_tend,rv_tend,a_rv_tend,rw_tend,a_rw_tend,config_flags,msfux,msfuy,msfvx,  &
! Revised by Ning Pan, 2010-07-30
!   msfvy,msftx,msfty,xlat,a_xlat,fnm,fnp,rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,  &
   msfvy,msftx,msfty,xlat,fnm,fnp,rdx,rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,  &
   jme,kms,kme,its,ite,jts,jte,kts,kte)

!LPB[7]
!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   ru_tend(IX1,IX2,IX3) =Keep_Lpb7_ru_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rv_tend(IX1,IX2,IX3) =Keep_Lpb7_rv_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rw_tend(IX1,IX2,IX3) =Keep_Lpb7_rw_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   IF(config_flags%pert_coriolis) THEN
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv400(IX1,IX2,IX3) =ru_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv401(IX1,IX2,IX3) =rv_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv402(IX1,IX2,IX3) =rw_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   CALL perturbation_coriolis(ru,rv,rw,ru_tend,rv_tend,rw_tend,config_flags,u_base,  &
!!   v_base,z_base,muu,muv,phb,ph,msftx,msfty,msfux,msfuy,msfvx,msfvy,f,e,sina,cosa,fnm,  &
!!   fnp,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!!   ELSE
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv403(IX1,IX2,IX3) =ru_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv404(IX1,IX2,IX3) =rv_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv405(IX1,IX2,IX3) =rw_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   CALL coriolis(ru,rv,rw,ru_tend,rv_tend,rw_tend,config_flags,msftx,msfty,msfux,  &
!!   msfuy,msfvx,msfvy,f,e,sina,cosa,fnm,fnp,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
!!   kme,its,ite,jts,jte,kts,kte)

!!   END IF

   IF(config_flags%pert_coriolis) THEN

!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rw_tend(IX1,IX2,IX3) =Tmpv402(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rv_tend(IX1,IX2,IX3) =Tmpv401(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   ru_tend(IX1,IX2,IX3) =Tmpv400(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

   CALL a_perturbation_coriolis(ru,a_ru,rv,a_rv,rw,a_rw,ru_tend,a_ru_tend,  &
   rv_tend,a_rv_tend,rw_tend,a_rw_tend,config_flags,u_base,v_base,z_base,muu,  &
   a_muu,muv,a_muv,phb,ph,a_ph,msftx,msfty,msfux,msfuy,msfvx,msfvy,f,e,sina,cosa,  &
   fnm,fnp,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   ELSE

!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rw_tend(IX1,IX2,IX3) =Tmpv405(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rv_tend(IX1,IX2,IX3) =Tmpv404(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   ru_tend(IX1,IX2,IX3) =Tmpv403(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

   CALL a_coriolis(ru,a_ru,rv,a_rv,rw,a_rw,ru_tend,a_ru_tend,rv_tend,  &
   a_rv_tend,rw_tend,a_rw_tend,config_flags,msftx,msfty,msfux,msfuy,msfvx,msfvy,f,e,  &
   sina,cosa,fnm,fnp,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   END IF

!LPB[6]
!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rw_tend(IX1,IX2,IX3) =Keep_Lpb6_rw_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!!  Tmpv400(IX1,IX2,IX3) =rw_tend(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO

!!  CALL w_damp(rw_tend,max_vert_cfl,max_horiz_cfl,u,v,ww,w,mut,rdnw,rdx,rdy,msfux,  &
!!  msfuy,msfvx,msfvy,dt,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,  &
!!  its,ite,jts,jte,kts,kte)

!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rw_tend(IX1,IX2,IX3) =Tmpv400(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

! Revised by Ning Pan, 2010-07-30
!   CALL a_w_damp(rw_tend,a_rw_tend,max_vert_cfl,a_max_vert_cfl,max_horiz_cfl,  &
!   a_max_horiz_cfl,u,a_u,v,a_v,ww,a_ww,w,a_w,mut,a_mut,rdnw,rdx,rdy,msfux,  &
   CALL a_w_damp(rw_tend,a_rw_tend,max_vert_cfl,max_horiz_cfl,  &
   u,a_u,v,a_v,ww,a_ww,w,a_w,mut,a_mut,rdnw,rdx,rdy,msfux,  &
   msfuy,msfvx,msfvy,dt,config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,  &
   its,ite,jts,jte,kts,kte)

!LPB[5]
!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rw_tend(IX1,IX2,IX3) =Keep_Lpb5_rw_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   cqw(IX1,IX2,IX3) =Keep_Lpb5_cqw(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!  IF(non_hydrostatic) THEN
!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!!  Tmpv400(IX1,IX2,IX3) =rw_tend(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO

!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!!  Tmpv401(IX1,IX2,IX3) =cqw(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO

!!  CALL pg_buoy_w(rw_tend,p,cqw,mu,mub,rdnw,rdn,g,msftx,msfty,ids,ide,jds,jde,kds,  &
!!  kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!!  ENDIF

   IF(non_hydrostatic) THEN

! Remarked by Ning Pan, 2010-07-30
!	  Const_A0=g
!   Const_Diff_A0=0.0

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   cqw(IX1,IX2,IX3) =Tmpv401(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rw_tend(IX1,IX2,IX3) =Tmpv400(IX1,IX2,IX3)
!   END DO
!   END DO
   CALL a_pg_buoy_w(rw_tend,a_rw_tend,p,a_p,cqw,a_cqw,mu,a_mu,mub,rdnw,rdn,  &
   g,msftx,msfty,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
!   END DO  ! Remarked by Ning Pan, 2010-07-30

   ENDIF

!LPB[4]
!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   ph_tend(IX1,IX2,IX3) =Keep_Lpb4_ph_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   ru_tend(IX1,IX2,IX3) =Keep_Lpb4_ru_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rv_tend(IX1,IX2,IX3) =Keep_Lpb4_rv_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv400(IX1,IX2,IX3) =ph_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   CALL rhs_ph(ph_tend,u,v,ww,ph,ph,phb,w,mut,muu,muv,fnm,fnp,rdnw,cfn,cfn1,rdx,rdy,  &
!!   msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,non_hydrostatic,config_flags,ids,ide,  &
!!   jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv401(IX1,IX2,IX3) =ru_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv402(IX1,IX2,IX3) =rv_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   CALL horizontal_pressure_gradient(ru_tend,rv_tend,ph,alt,p,pb,al,php,cqu,cqv,muu,  &
!!   muv,mu,fnm,fnp,rdnw,cf1,cf2,cf3,cfn,cfn1,rdx,rdy,msfux,msfuy,msfvx,msfvy,msftx,msfty,  &
!!   config_flags,non_hydrostatic,top_lid,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,  &
!!   its,ite,jts,jte,kts,kte)

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rv_tend(IX1,IX2,IX3) =Tmpv402(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   ru_tend(IX1,IX2,IX3) =Tmpv401(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

   CALL a_horizontal_pressure_gradient(ru_tend,a_ru_tend,rv_tend,a_rv_tend,ph,  &
   a_ph,alt,a_alt,p,a_p,pb,al,a_al,php,a_php,cqu,a_cqu,cqv,a_cqv,muu,  &
   a_muu,muv,a_muv,mu,a_mu,fnm,fnp,rdnw,cf1,cf2,cf3,cfn,cfn1,rdx,rdy,msfux,msfuy,msfvx,  &
! Revised by Ning Pan, 2010-07-30
!   msfvy,msftx,msfty,config_flags,,,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,  &
   msfvy,msftx,msfty,config_flags,non_hydrostatic,top_lid,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,  &
   ite,jts,jte,kts,kte)

!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   ph_tend(IX1,IX2,IX3) =Tmpv400(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

   CALL a_rhs_ph(ph_tend,a_ph_tend,u,a_u,v,a_v,ww,a_ww,ph,a_ph,ph,a_ph,  &
   phb,w,a_w,mut,a_mut,muu,a_muu,muv,a_muv,fnm,fnp,rdnw,cfn,cfn1,rdx,rdy,msfux,  &
! Remarked by Ning Pan, 2010-07-30
!   msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,,config_flags,ids,ide,jds,jde,kds,kde,ims,  &
   msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,non_hydrostatic,config_flags,ids,ide,jds,jde,kds,kde,ims,  &
   ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!LPB[3]

!!  IF( config_flags%cu_physics == GDSCHEME  .OR.      	          config_flags%cu_physics == G3SCHEME ) THEN
!!  CALL set_tend(RTHFTEN,t_tend,msfty,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
!!  kme,its,ite,jts,jte,kts,kte)

!!  END IF

   IF( config_flags%cu_physics == GDSCHEME  .OR.       &
       config_flags%cu_physics == G3SCHEME  .OR.       &
       config_flags%cu_physics == NTIEDTKESCHEME ) THEN

   CALL a_set_tend(RTHFTEN,a_RTHFTEN,t_tend,a_t_tend,msfty,ids,ide,jds,jde,kds,  &
   kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   END IF

!LPB[2]
!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   t(IX1,IX2,IX3) =Keep_Lpb2_t(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   t_tend(IX1,IX2,IX3) =Keep_Lpb2_t_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   ru(IX1,IX2,IX3) =Keep_Lpb2_ru(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rv(IX1,IX2,IX3) =Keep_Lpb2_rv(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!!  Tmpv400(IX1,IX2,IX3) =t(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO

!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!!  Tmpv401(IX1,IX2,IX3) =t_tend(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO

!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!!  Tmpv402(IX1,IX2,IX3) =ru(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO

!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!!  Tmpv403(IX1,IX2,IX3) =rv(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO

!!  CALL advect_scalar(t,t,t_tend,ru,rv,ww,mut,time_step,config_flags,msfux,msfuy,  &
!!  msfvx,msfvy,msftx,msfty,fnm,fnp,rdx,rdy,rdnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
!!  kms,kme,its,ite,jts,jte,kts,kte)

!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rv(IX1,IX2,IX3) =Tmpv403(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   ru(IX1,IX2,IX3) =Tmpv402(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   t_tend(IX1,IX2,IX3) =Tmpv401(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   t(IX1,IX2,IX3) =Tmpv400(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!hcl 11/2016 ERM: Use WENO for theta flux on 3rd RK step if using WENO_SCALAR or WENOPD_SCALAR
! to be consistent with other scalar fluxes
 IF(  ( config_flags%scalar_adv_opt == WENO_SCALAR           &
         .or. config_flags%scalar_adv_opt == WENOPD_SCALAR   &
         .or. config_flags%moist_adv_opt == WENO_SCALAR      &
         .or. config_flags%moist_adv_opt == WENOPD_SCALAR    &
                       )  .and. (rk_step == 3) ) THEN

   CALL a_advect_scalar_weno(t,a_t,t,a_t,t_tend,a_t_tend,ru,a_ru,rv,a_rv,ww,  &
   a_ww,mut,time_step,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm,  &
   fnp,rdx,rdy,rdnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 ELSE

   CALL a_advect_scalar(t,a_t,t,a_t,t_tend,a_t_tend,ru,a_ru,rv,a_rv,ww,  &
   a_ww,mut,time_step,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm,  &
   fnp,rdx,rdy,rdnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

 ENDIF

!LPB[1]
!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   w(IX1,IX2,IX3) =Keep_Lpb1_w(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rw_tend(IX1,IX2,IX3) =Keep_Lpb1_rw_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!  IF(non_hydrostatic) THEN
!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!!  Tmpv400(IX1,IX2,IX3) =w(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO

!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!!  Tmpv401(IX1,IX2,IX3) =rw_tend(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO

!!  CALL advect_w(w,w,rw_tend,ru,rv,ww,mut,time_step,config_flags,msfux,msfuy,msfvx,  &
!!  msfvy,msftx,msfty,fnm,fnp,rdx,rdy,rdn,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
!!  kme,its,ite,jts,jte,kts,kte)

!!  END IF

   IF(non_hydrostatic) THEN

!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   rw_tend(IX1,IX2,IX3) =Tmpv401(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   w(IX1,IX2,IX3) =Tmpv400(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

   IF( (rk_step == 3) .and. ( adv_opt == WENO_MOM ) ) THEN
   CALL a_advect_weno_w ( w, a_w, w, a_w, rw_tend, a_rw_tend, &
                     ru, a_ru, rv, a_rv, ww, a_ww, &
                     mut, time_step, config_flags, &
                     msfux, msfuy, msfvx, msfvy,   &
                     msftx, msfty,                 &
                     fnm, fnp, rdx, rdy, rdn,      &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )

   ELSE

   CALL a_advect_w(w,a_w,w,a_w,rw_tend,a_rw_tend,ru,a_ru,rv,a_rv,ww,  &
   a_ww,mut,time_step,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm,  &
   fnp,rdx,rdy,rdn,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   END IF

   END IF

!LPB[0]
!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   u(IX1,IX2,IX3) =Keep_Lpb0_u(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   v(IX1,IX2,IX3) =Keep_Lpb0_v(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   CALL zero_tend(ru_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,  &
!!   jts,jte,kts,kte)

!!   CALL zero_tend(rv_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,  &
!!   jts,jte,kts,kte)

!!   CALL zero_tend(rw_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,  &
!!   jts,jte,kts,kte)

!!   CALL zero_tend(t_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,  &
!!   jte,kts,kte)

!!   CALL zero_tend(ph_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,  &
!!   jts,jte,kts,kte)

!!   CALL zero_tend(u_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,  &
!!   jte,kts,kte)

!!   CALL zero_tend(v_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,  &
!!   jte,kts,kte)

!!   CALL zero_tend(w_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,  &
!!   jte,kts,kte)

!!   CALL zero_tend(ph_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,  &
!!   jts,jte,kts,kte)

!!   CALL zero_tend(t_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,  &
!!   jte,kts,kte)

!!   CALL zero_tend(mu_tend,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,its,ite,jts,jte,1,1)

!!   CALL zero_tend(mu_save,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,its,ite,jts,jte,1,1)

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv400(IX1,IX2,IX3) =u(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv401(IX1,IX2,IX3) =ru_tend(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   CALL advect_u(u,u,ru_tend,ru,rv,ww,mut,time_step,config_flags,msfux,msfuy,msfvx,  &
!!   msfvy,msftx,msfty,fnm,fnp,rdx,rdy,rdnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
!!   kme,its,ite,jts,jte,kts,kte)

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   Tmpv402(IX1,IX2,IX3) =v(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   CALL advect_v(v,v,rv_tend,ru,rv,ww,mut,time_step,config_flags,msfux,msfuy,msfvx,  &
!!   msfvy,msftx,msfty,fnm,fnp,rdx,rdy,rdnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
!!   kme,its,ite,jts,jte,kts,kte)

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   v(IX1,IX2,IX3) =Tmpv402(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

   IF( (rk_step == 3) .and. ( adv_opt == WENO_MOM ) ) THEN

   CALL a_advect_weno_v ( v, a_v, v, a_v, rv_tend, a_rv_tend, &
                 ru, a_ru, rv, a_rv, ww, a_ww, &
                 mut, a_mut, time_step, config_flags, &
                 msfux, msfuy, msfvx, msfvy,   &
                 msftx, msfty,                 &
                 fnm, fnp, rdx, rdy, rdnw,     &
                 ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte )

   ELSE

   CALL a_advect_v(v,a_v,v,a_v,rv_tend,a_rv_tend,ru,a_ru,rv,a_rv,ww,  &
   a_ww,mut,a_mut,time_step,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm,  &
   fnp,rdx,rdy,rdnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   ENDIF

!! Remarked by Ning Pan, 2010-07-30
!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   ru_tend(IX1,IX2,IX3) =Tmpv401(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO

!!   DO IX3=jms,jme
!!   DO IX2=kms,kme
!!   DO IX1=ims,ime
!!   u(IX1,IX2,IX3) =Tmpv400(IX1,IX2,IX3)
!!   END DO
!!   END DO
!!   END DO


   IF( (rk_step == 3) .and. ( adv_opt == WENO_MOM ) ) THEN

   CALL a_advect_weno_u ( u, a_u, u, a_u, ru_tend, a_ru_tend, &
               ru, a_ru, rv, a_rv, ww, a_ww, &
               mut, a_mut, time_step, config_flags, &
               msfux, msfuy, msfvx, msfvy,   &
               msftx, msfty,                 &
               fnm, fnp, rdx, rdy, rdnw,     &
               ids, ide, jds, jde, kds, kde, &
               ims, ime, jms, jme, kms, kme, &
               its, ite, jts, jte, kts, kte )
   ELSE

   CALL a_advect_u(u,a_u,u,a_u,ru_tend,a_ru_tend,ru,a_ru,rv,a_rv,ww,  &
   a_ww,mut,a_mut,time_step,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm,  &
   fnp,rdx,rdy,rdnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   ENDIF

! Added by Ning Pan, 2010-07-30
   CALL a_zero_tend2d(a_mu_save,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,  &
   its,ite,jts,jte,1,1)
   CALL a_zero_tend2d(a_mu_tend,ids,ide,jds,jde,1,1,ims,ime,jms,jme,1,1,  &
   its,ite,jts,jte,1,1)
   CALL a_zero_tend(a_t_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
   kme,its,ite,jts,jte,kts,kte)
   CALL a_zero_tend(a_ph_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
   kms,kme,its,ite,jts,jte,kts,kte)
   CALL a_zero_tend(a_w_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
   kme,its,ite,jts,jte,kts,kte)
   CALL a_zero_tend(a_v_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
   kme,its,ite,jts,jte,kts,kte)
   CALL a_zero_tend(a_u_save,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
   kme,its,ite,jts,jte,kts,kte)
   CALL a_zero_tend(a_ph_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
   kms,kme,its,ite,jts,jte,kts,kte)
   CALL a_zero_tend(a_t_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
   kme,its,ite,jts,jte,kts,kte)
   CALL a_zero_tend(a_rw_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
   kms,kme,its,ite,jts,jte,kts,kte)
   CALL a_zero_tend(a_rv_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
   kms,kme,its,ite,jts,jte,kts,kte)
   CALL a_zero_tend(a_ru_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
   kms,kme,its,ite,jts,jte,kts,kte)

   END SUBROUTINE a_rk_tendency

!-------------------------------------------------------------------------------

!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of rk_addtend_dry in reverse (adjoint) mode:
!   gradient     of useful results: ph_tendf rw_tendf u_save ph_save
!                w_save mu_tend rv_tendf ru_tend rw_tend h_diabatic
!                ru_tendf t_tend mu_tendf t_save v_save rv_tend
!                t_tendf mut ph_tend
!   with respect to varying inputs: ph_tendf rw_tendf u_save ph_save
!                w_save mu_tend rv_tendf ru_tend rw_tend h_diabatic
!                ru_tendf t_tend mu_tendf t_save v_save rv_tend
!                t_tendf mut ph_tend
!   RW status of diff variables: ph_tendf:in-out rw_tendf:in-out
!                u_save:incr ph_save:incr w_save:incr mu_tend:in-out
!                rv_tendf:in-out ru_tend:in-out rw_tend:in-out
!                h_diabatic:incr ru_tendf:in-out t_tend:in-out
!                mu_tendf:incr t_save:incr v_save:incr rv_tend:in-out
!                t_tendf:in-out mut:incr ph_tend:in-out
SUBROUTINE A_RK_ADDTEND_DRY(ru_tend, ru_tendb, rv_tend, rv_tendb, &
&  rw_tend, rw_tendb, ph_tend, ph_tendb, t_tend, t_tendb, ru_tendf, &
&  ru_tendfb, rv_tendf, rv_tendfb, rw_tendf, rw_tendfb, ph_tendf, &
&  ph_tendfb, t_tendf, t_tendfb, u_save, u_saveb, v_save, v_saveb, w_save&
&  , w_saveb, ph_save, ph_saveb, t_save, t_saveb, mu_tend, mu_tendb, &
&  mu_tendf, mu_tendfb, rk_step, h_diabatic, h_diabaticb, mut, mutb, &
&  msftx, msfty, msfux, msfuy, msfvx, msfvx_inv, msfvy, ids, ide, jds, &
&  jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, &
&  kpe, its, ite, jts, jte, kts, kte)
  IMPLICIT NONE
!  Input data.
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&  jme, kms, kme, ips, ipe, jps, jpe, kps, kpe, its, ite, jts, jte, kts, &
&  kte
  INTEGER, INTENT(IN) :: rk_step
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: ru_tend, &
&  rv_tend, rw_tend, ph_tend, t_tend, ru_tendf, rv_tendf, rw_tendf, &
&  ph_tendf, t_tendf
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: ru_tendb, rv_tendb, &
&  rw_tendb, ph_tendb, t_tendb, ru_tendfb, rv_tendfb, rw_tendfb, &
&  ph_tendfb, t_tendfb
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: mu_tend, mu_tendf
  REAL, DIMENSION(ims:ime, jms:jme) :: mu_tendb, mu_tendfb
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: u_save, &
&  v_save, w_save, ph_save, t_save, h_diabatic
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: u_saveb, v_saveb, &
&  w_saveb, ph_saveb, t_saveb, h_diabaticb
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: mut, msftx, msfty, &
&  msfux, msfuy, msfvx, msfvx_inv, msfvy
  REAL, DIMENSION(ims:ime, jms:jme) :: mutb
! Local
  INTEGER :: i, j, k
  INTEGER :: branch
  INTEGER :: ad_to
  INTEGER :: ad_to0
  INTEGER :: ad_to1
  INTEGER :: ad_to2
  INTEGER :: min8
  INTEGER :: min7
  INTEGER :: min6
  INTEGER :: min5
  INTEGER :: min4
  INTEGER :: min3
  INTEGER :: min2
  INTEGER :: min1
  IF (jte .GT. jde - 1) THEN
    min1 = jde - 1
  ELSE
    min1 = jte
  END IF
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
  DO j=jts,min1
    DO k=kts,kte-1
      DO i=its,ite
! multiply by my to uncouple u
        IF (rk_step .EQ. 1) THEN
          CALL PUSHCONTROL1B(0)
        ELSE
          CALL PUSHCONTROL1B(1)
        END IF
      END DO
    END DO
  END DO
  DO j=jts,jte
    DO k=kts,kte-1
      IF (ite .GT. ide - 1) THEN
        min2 = ide - 1
      ELSE
        min2 = ite
      END IF
      DO i=its,min2
! multiply by mx to uncouple v
        IF (rk_step .EQ. 1) THEN
          CALL PUSHCONTROL1B(0)
        ELSE
          CALL PUSHCONTROL1B(1)
        END IF
      END DO
      CALL PUSHINTEGER4(i - 1)
    END DO
  END DO
  IF (jte .GT. jde - 1) THEN
    min3 = jde - 1
  ELSE
    min3 = jte
  END IF
  DO j=jts,min3
    DO k=kts,kte
      IF (ite .GT. ide - 1) THEN
        min4 = ide - 1
      ELSE
        min4 = ite
      END IF
      DO i=its,min4
! multiply by my to uncouple w
        IF (rk_step .EQ. 1) THEN
          CALL PUSHCONTROL1B(0)
        ELSE
          CALL PUSHCONTROL1B(1)
        END IF
! divide by my to couple w
        IF (rk_step .EQ. 1) THEN
          CALL PUSHCONTROL1B(0)
        ELSE
          CALL PUSHCONTROL1B(1)
        END IF
      END DO
      CALL PUSHINTEGER4(i - 1)
    END DO
  END DO
  IF (jte .GT. jde - 1) THEN
    min5 = jde - 1
  ELSE
    min5 = jte
  END IF
  DO j=jts,min5
    DO k=kts,kte-1
      IF (ite .GT. ide - 1) THEN
        min6 = ide - 1
      ELSE
        min6 = ite
      END IF
      DO i=its,min6
        IF (rk_step .EQ. 1) THEN
          CALL PUSHCONTROL1B(0)
        ELSE
          CALL PUSHCONTROL1B(1)
        END IF
      END DO
      CALL PUSHINTEGER4(i - 1)
    END DO
  END DO
  IF (jte .GT. jde - 1) THEN
    min7 = jde - 1
  ELSE
    min7 = jte
  END IF
! divide by my to couple heating
  DO j=jts,min7
    IF (ite .GT. ide - 1) THEN
      min8 = ide - 1
    ELSE
      min8 = ite
    END IF
    i = min8 + 1
    CALL PUSHINTEGER4(i - 1)
  END DO
  DO j=min7,jts,-1
    CALL POPINTEGER4(ad_to2)
    DO i=ad_to2,its,-1
      mu_tendfb(i, j) = mu_tendfb(i, j) + mu_tendb(i, j)
    END DO
  END DO
  DO j=min5,jts,-1
    DO k=kte-1,kts,-1
      CALL POPINTEGER4(ad_to1)
      DO i=ad_to1,its,-1
        t_tendfb(i, k, j) = t_tendfb(i, k, j) + t_tendb(i, k, j)/msfty(i&
&          , j)
        h_diabaticb(i, k, j) = h_diabaticb(i, k, j) + mut(i, j)*t_tendb(&
&          i, k, j)/msfty(i, j)
        mutb(i, j) = mutb(i, j) + h_diabatic(i, k, j)*t_tendb(i, k, j)/&
&          msfty(i, j)
        CALL POPCONTROL1B(branch)
        IF (branch .EQ. 0) t_saveb(i, k, j) = t_saveb(i, k, j) + &
&            t_tendfb(i, k, j)
      END DO
    END DO
  END DO
  DO j=min3,jts,-1
    DO k=kte,kts,-1
      CALL POPINTEGER4(ad_to0)
      DO i=ad_to0,its,-1
        ph_tendfb(i, k, j) = ph_tendfb(i, k, j) + ph_tendb(i, k, j)/&
&          msfty(i, j)
        CALL POPCONTROL1B(branch)
        IF (branch .EQ. 0) ph_saveb(i, k, j) = ph_saveb(i, k, j) + &
&            ph_tendfb(i, k, j)
        rw_tendfb(i, k, j) = rw_tendfb(i, k, j) + rw_tendb(i, k, j)/&
&          msfty(i, j)
        CALL POPCONTROL1B(branch)
        IF (branch .EQ. 0) w_saveb(i, k, j) = w_saveb(i, k, j) + msfty(i&
&            , j)*rw_tendfb(i, k, j)
      END DO
    END DO
  END DO
  DO j=jte,jts,-1
    DO k=kte-1,kts,-1
      CALL POPINTEGER4(ad_to)
      DO i=ad_to,its,-1
        rv_tendfb(i, k, j) = rv_tendfb(i, k, j) + msfvx_inv(i, j)*&
&          rv_tendb(i, k, j)
        CALL POPCONTROL1B(branch)
        IF (branch .EQ. 0) v_saveb(i, k, j) = v_saveb(i, k, j) + msfvx(i&
&            , j)*rv_tendfb(i, k, j)
      END DO
    END DO
  END DO
  DO j=min1,jts,-1
    DO k=kte-1,kts,-1
      DO i=ite,its,-1
        ru_tendfb(i, k, j) = ru_tendfb(i, k, j) + ru_tendb(i, k, j)/&
&          msfuy(i, j)
        CALL POPCONTROL1B(branch)
        IF (branch .EQ. 0) u_saveb(i, k, j) = u_saveb(i, k, j) + msfuy(i&
&            , j)*ru_tendfb(i, k, j)
      END DO
    END DO
  END DO
END SUBROUTINE A_RK_ADDTEND_DRY
!-------------------------------------------------------------------------------

! Revised by Ning Pan, 2010-08-02
!   SUBROUTINE a_rk_scalar_tend(scs,sce,config_flags,rk_step,dt,a_dt,ru,a_ru,rv, &
   SUBROUTINE a_rk_scalar_tend(scs,sce,config_flags,tenddec,rk_step,dt,ru,a_ru,rv, &
   a_rv,ww,a_ww,mut,a_mut,mub,mu_old,a_mu_old,alt,a_alt,scalar_old, &
   a_scalar_old,scalar,a_scalar,scalar_tends,a_scalar_tends,advect_tend, &
   a_advect_tend,h_tendency,a_h_tendency,z_tendency,a_z_tendency, &
   RQVFTEN,a_RQVFTEN,base,moist_step,fnm,fnp,msfux,msfuy,msfvx, &
   msfvx_inv,msfvy,msftx,msfty,rdx,rdy,rdn,rdnw,khdif,kvdif,xkmhd,a_xkmhd, &
! Revised by Ning Pan, 2010-08-02
!   diff_6th_opt,diff_6th_factor,a_diff_6th_factor,adv_opt,ids,ide,jds,jde,kds,kde,ims, &
   diff_6th_opt,diff_6th_factor,adv_opt,ids,ide,jds,jde,kds,kde,ims, &
   ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!PART I: DECLARATION OF VARIABLES

   IMPLICIT NONE

   INTEGER :: K0_ADJ,K1_ADJ,K2_ADJ,K3_ADJ
   TYPE(grid_config_rec_type) :: config_flags
   LOGICAL :: tenddec
   INTEGER :: rk_step,scs,sce
   INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
   LOGICAL :: moist_step
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme,scs:sce) :: scalar,a_scalar,scalar_old, &
   a_scalar_old
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme,scs:sce) :: scalar_tends,a_scalar_tends
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: advect_tend,a_advect_tend
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: h_tendency, z_tendency 
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: a_h_tendency, a_z_tendency 
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: RQVFTEN,a_RQVFTEN
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: ru,a_ru,rv,a_rv,ww,a_ww,xkmhd, &
   a_xkmhd,alt,a_alt
   REAL,DIMENSION(kms:kme) :: fnm,fnp,rdn,rdnw,base
   REAL,DIMENSION(ims:ime,jms:jme) :: msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,mub, &
   mut,a_mut,mu_old,a_mu_old
   REAL :: rdx,rdy,khdif,kvdif
   INTEGER :: diff_6th_opt
! Revised by Ning Pan, 2010-08-02
!   REAL :: diff_6th_factor,a_diff_6th_factor
!   REAL :: dt,a_dt
   REAL :: diff_6th_factor
   REAL :: dt
   INTEGER :: adv_opt
   INTEGER :: im,i,j,k
   INTEGER :: time_step
   REAL :: khdq,kvdq,tendency,a_tendency

!  REAL,DIMENSION(ims:ime,kms:kme,jms:jme,scs:sce,ims:ime,kms:kme,jms:jme,scs:sce) &
!    :: Keep_Lpb1_scalar   
!  REAL,DIMENSION(ims:ime,kms:kme,jms:jme,scs:sce,ims:ime,kms:kme,jms:jme,scs:sce) &
!    :: Keep_Lpb1_scalar_old   
!  REAL,DIMENSION(scs:sce,ims:ime,kms:kme,jms:jme) :: Keep_Lpb1_ru   
!  REAL,DIMENSION(scs:sce,ims:ime,kms:kme,jms:jme) :: Keep_Lpb1_rv   
!  REAL,DIMENSION(ims:ime,kms:kme,jms:jme,scs:sce,ims:ime,kms:kme,jms:jme,scs:sce) &
!    :: Keep_Lpb1_scalar_tends   
   INTEGER :: IX1,IX2,IX3,IX4

   REAL :: Tmpv_1,Tmpv_2,Tmpv_3,Tmpv_4,Tmpv_5,Tmpv_6,Tmpv_7,Tmpv_8,Tmpv_9,Tmpv_10, &
   Tmpv_11,Tmpv_12
   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv400
   REAL,DIMENSION(jms:jme,kms:kme,ims:ime) :: Tmpv401

!This line is fail to be recognized
!        CALL nl_get_time_step ( 1, time_step )  ! Remarked by Ning Pan, 2010-08-02

!PART II: CALCULATIONS OF B. S. TRAJECTORY

!LPB[0]
      khdq = khdif/prandtl
      kvdq = kvdif/prandtl

!!LPB[1]
!      scalar_loop : DO im = scs, sce

!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!    !  Keep_Lpb1_ru(im,IX1,IX2,IX3) =ru(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO
!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!    !  Keep_Lpb1_rv(im,IX1,IX2,IX3) =rv(IX1,IX2,IX3)
!!  END DO
!!  END DO
!!  END DO
!!  DO IX4=scs,sce
!!  DO IX3=jms,jme
!!  DO IX2=kms,kme
!!  DO IX1=ims,ime
!    !  Keep_Lpb1_scalar_tends(ims,kms,jms,im,IX1,IX2,IX3,IX4) =scalar_tends(ims,kms,jms,im)(IX1,IX2,IX3,IX4)
!!  END DO
!!  END DO
!!  END DO
!!  END DO

!        CALL zero_tend ( advect_tend(ims,kms,jms),       &
!                         ids, ide, jds, jde, kds, kde,   &
!                         ims, ime, jms, jme, kms, kme,   &
!                         its, ite, jts, jte, kts, kte )
!      IF( (rk_step == 3) .and. (adv_opt == POSITIVEDEF) ) THEN

!           CALL advect_scalar_pd       ( scalar(ims,kms,jms,im),               &
!                                         scalar_old(ims,kms,jms,im),           &
!                                         advect_tend(ims,kms,jms),             &
!                                         ru, rv, ww, mut, mub, mu_old,         &
!                                         time_step, config_flags,              &
!                                         msfux, msfuy, msfvx, msfvy,           &
!                                         msftx, msfty, fnm, fnp,               &
!                                         rdx, rdy, rdnw,dt,                    &
!                                         ids, ide, jds, jde, kds, kde,         &
!                                         ims, ime, jms, jme, kms, kme,         &
!                                         its, ite, jts, jte, kts, kte     )
!         ELSE IF( (rk_step == 3) .and. (adv_opt == MONOTONIC) ) THEN
!           CALL advect_scalar_mono       ( scalar(ims,kms,jms,im),               &
!                                           scalar_old(ims,kms,jms,im),           &
!                                           advect_tend(ims,kms,jms),             &
!                                           ru, rv, ww, mut, mub, mu_old,         &
!                                           config_flags,                         &
!                                           msfux, msfuy, msfvx, msfvy,           &
!                                           msftx, msfty, fnm, fnp,               &
!                                           rdx, rdy, rdnw,dt,                    &
!                                           ids, ide, jds, jde, kds, kde,         &
!                                           ims, ime, jms, jme, kms, kme,         &
!                                           its, ite, jts, jte, kts, kte     )
!         ELSE
!           CALL advect_scalar     ( scalar(ims,kms,jms,im),          &
!                                    scalar(ims,kms,jms,im),          &
!                                    advect_tend(ims,kms,jms),        &
!                                    ru, rv, ww, mut, time_step,      &
!                                    config_flags,                    &
!                                    msfux, msfuy, msfvx, msfvy,      &
!                                    msftx, msfty, fnm, fnp,          &
!                                    rdx, rdy, rdnw,                  &
!                                    ids, ide, jds, jde, kds, kde,    &
!                                    ims, ime, jms, jme, kms, kme,    &
!                                    its, ite, jts, jte, kts, kte  )
!         END IF
!     IF((config_flags%cu_physics == GDSCHEME .OR. config_flags%cu_physics == G3SCHEME &
!)                        .and. moist_step .and. ( im == P_QV) ) THEN

!           CALL set_tend( RQVFTEN, advect_tend, msfty,      &
!                          ids, ide, jds, jde, kds, kde,     &
!                          ims, ime, jms, jme, kms, kme,     &
!                          its, ite, jts, jte, kts, kte      )
!        ENDIF
!     rk_step_1: IF( rk_step == 1 ) THEN

!       diff_opt1 : IF (config_flags%diff_opt .eq. 1) THEN

!          CALL horizontal_diffusion ( 'm', scalar(ims,kms,jms,im),              &
!                                           scalar_tends(ims,kms,jms,im), mut,   &
!                                           config_flags,                        &
!                                           msfux, msfuy, msfvx, msfvx_inv,      &
!                                           msfvy, msftx, msfty,                 &
!                                           khdq , xkmhd, rdx, rdy,              &
!                                           ids, ide, jds, jde, kds, kde,        &
!                                           ims, ime, jms, jme, kms, kme,        &
!                                           its, ite, jts, jte, kts, kte      )
!       pbl_test : IF (config_flags%bl_pbl_physics .eq. 0) THEN

!         IF( (moist_step) .and. ( im == P_QV)) THEN

!               CALL vertical_diffusion_mp ( scalar(ims,kms,jms,im),         &
!                                            scalar_tends(ims,kms,jms,im),   &
!                                            config_flags, base,             &
!                                            alt, mut, rdn, rdnw, kvdq ,     &
!                                            ids, ide, jds, jde, kds, kde,   &
!                                            ims, ime, jms, jme, kms, kme,   &
!                                            its, ite, jts, jte, kts, kte )
!            ELSE 
!               CALL vertical_diffusion (  'm', scalar(ims,kms,jms,im),         &
!                                               scalar_tends(ims,kms,jms,im),   &
!                                               config_flags,                   &
!                                               alt, mut, rdn, rdnw, kvdq,      &
!                                               ids, ide, jds, jde, kds, kde,   &
!                                               ims, ime, jms, jme, kms, kme,   &
!                                               its, ite, jts, jte, kts, kte )
!            END IF
!         ENDIF pbl_test
!       ENDIF diff_opt1
!    IF ( diff_6th_opt .NE. 0 )                                          &
!      CALL sixth_order_diffusion( 'm', scalar(ims,kms,jms,im),          &
!                                       scalar_tends(ims,kms,jms,im),    &
!                                       mut, dt, config_flags,           &
!                                       diff_6th_opt, diff_6th_factor,   &
!                                       ids, ide, jds, jde, kds, kde,    &
!                                       ims, ime, jms, jme, kms, kme,    &
!                                       its, ite, jts, jte, kts, kte )

!     ENDIF rk_step_1

!    END DO scalar_loop

!PART III: INITIALIZATION OF LOCAL ADJOINT PERTURBATIONS

!   a_tendency =0.0  ! Remarked by Ning Pan, 2010-08-02

!PART IV: REVERSE/BACKWARD ACCUMULATIONS

!LPB[1]
   DO im =sce, scs, -1
   CALL nl_get_time_step ( 1, time_step )  ! Added by Ning Pan, 2010-08-02

!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  ru(IX1,IX2,IX3) =Keep_Lpb1_ru(im,IX1,IX2,IX3)
!  END DO
!  END DO
!  END DO
!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  rv(IX1,IX2,IX3) =Keep_Lpb1_rv(im,IX1,IX2,IX3)
!  END DO
!  END DO
!  END DO
!  DO IX4=scs,sce
!  DO IX3=jms,jme
!  DO IX2=kms,kme
!  DO IX1=ims,ime
!  scalar_tends(ims,kms,jms,im)(IX1,IX2,IX3,IX4) =Keep_Lpb1_scalar_tends(ims,kms,jms,im,IX1,IX2,IX3,IX4)
!  END DO
!  END DO
!  END DO
!  END DO

! Remarked by Ning Pan, 2010-08-02 : useless recomputation
!   CALL zero_tend(advect_tend(ims,kms,jms),ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
!   kms,kme,its,ite,jts,jte,kts,kte)

!   IF( (rk_step == 3) .and. (adv_opt == POSITIVEDEF) ) THEN
!   Tmpv_1 =scalar(ims,kms,jms,im)
!   Tmpv_2 =scalar_old(ims,kms,jms,im)
!   Tmpv_3 =advect_tend(ims,kms,jms)
!   CALL advect_scalar_pd(scalar(ims,kms,jms,im),scalar_old(ims,kms,jms,im)  &
!   ,advect_tend(ims,kms,jms),ru,rv,ww,mut,mub,mu_old,time_step,config_flags,msfux,msfuy,  &
!   msfvx,msfvy,msftx,msfty,fnm,fnp,rdx,rdy,rdnw,dt,ids,ide,jds,jde,kds,kde,ims,ime,jms,  &
!   jme,kms,kme,its,ite,jts,jte,kts,kte)

!   ELSE IF( (rk_step == 3) .and. (adv_opt == MONOTONIC) ) THEN
!   Tmpv_4 =scalar(ims,kms,jms,im)
!   Tmpv_5 =scalar_old(ims,kms,jms,im)
!   Tmpv_6 =advect_tend(ims,kms,jms)
!   CALL advect_scalar_mono(scalar(ims,kms,jms,im),scalar_old(ims,kms,jms,im)  &
!   ,advect_tend(ims,kms,jms),ru,rv,ww,mut,mub,mu_old,config_flags,msfux,msfuy,msfvx,  &
!   msfvy,msftx,msfty,fnm,fnp,rdx,rdy,rdnw,dt,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
!   kms,kme,its,ite,jts,jte,kts,kte)

!   ELSE
!   Tmpv_7 =scalar(ims,kms,jms,im)
!   Tmpv_8 =advect_tend(ims,kms,jms)
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv400(IX1,IX2,IX3) =ru(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   Tmpv401(IX1,IX2,IX3) =rv(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   CALL advect_scalar(scalar(ims,kms,jms,im),scalar(ims,kms,jms,im),advect_tend(ims,  &
!   kms,jms),ru,rv,ww,mut,time_step,config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm,  &
!   fnp,rdx,rdy,rdnw,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   END IF
!   IF((config_flags%cu_physics == GDSCHEME .OR. config_flags%cu_physics == G3SCHEME)   	                     .and. moist_step .and. ( im == P_QV) ) THEN
!   CALL set_tend(RQVFTEN,advect_tend,msfty,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
!   kms,kme,its,ite,jts,jte,kts,kte)

!   ENDIF
!   IF( rk_step == 1 ) THEN
!   IF(config_flags%diff_opt .eq. 1) THEN
!   Tmpv_9 =scalar_tends(ims,kms,jms,im)
!   CALL horizontal_diffusion('m',scalar(ims,kms,jms,im),scalar_tends(ims,kms,jms,im)  &
!   ,mut,config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdq,xkmhd,rdx,rdy,  &
!   ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   IF(config_flags%bl_pbl_physics .eq. 0) THEN
!   IF( (moist_step) .and. ( im == P_QV)) THEN
!   Tmpv_10 =scalar_tends(ims,kms,jms,im)
!   CALL vertical_diffusion_mp(scalar(ims,kms,jms,im),scalar_tends(ims,kms,jms,im)  &
!   ,config_flags,base,alt,mut,rdn,rdnw,kvdq,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,  &
!   kme,its,ite,jts,jte,kts,kte)

!   ELSE
!   Tmpv_11 =scalar_tends(ims,kms,jms,im)
!   CALL vertical_diffusion('m',scalar(ims,kms,jms,im),scalar_tends(ims,kms,jms,im)  &
!   ,config_flags,alt,mut,rdn,rdnw,kvdq,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,  &
!   its,ite,jts,jte,kts,kte)

!   END IF
!   ENDIF
!   ENDIF
!   IF( diff_6th_opt .NE. 0 ) THEN
!   Tmpv_12 =scalar_tends(ims,kms,jms,im)
!   CALL sixth_order_diffusion('m',scalar(ims,kms,jms,im),scalar_tends(ims,kms,jms,im)  &
!   ,mut,dt,config_flags,diff_6th_opt,diff_6th_factor,ids,ide,jds,jde,kds,kde,ims,ime,  &
!   jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!   END IF
!   ENDIF

   IF( rk_step == 1 ) THEN

   IF( diff_6th_opt .NE. 0 ) THEN

!   scalar_tends(ims,kms,jms,im) =Tmpv_12  ! Remarked by Ning Pan, 2010-08-02

   CALL a_sixth_order_diffusion('m',scalar(ims,kms,jms,im),a_scalar(ims,kms,jms,  &
   im),scalar_tends(ims,kms,jms,im),a_scalar_tends(ims,kms,jms,im),mut,a_mut,dt,  &
! Revised by Ning Pan, 2010-08-02
!   a_dt,config_flags,diff_6th_opt,diff_6th_factor,a_diff_6th_factor,ids,ide,jds,jde,  &
   config_flags,diff_6th_opt,diff_6th_factor,ids,ide,jds,jde,  &
   kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   END IF

   IF(config_flags%diff_opt .eq. 1) THEN

   IF(config_flags%bl_pbl_physics .eq. 0) THEN

! Added by Ning Pan, 2010-08-02
   IF( (moist_step) .and. ( im == P_QV)) THEN
   CALL a_vertical_diffusion_mp(scalar(ims,kms,jms,im),a_scalar(ims,kms,jms,im)  &
   ,scalar_tends(ims,kms,jms,im),a_scalar_tends(ims,kms,jms,im),config_flags,base,alt,  &
   a_alt,mut,a_mut,rdn,rdnw,kvdq,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,  &
   its,ite,jts,jte,kts,kte)

   ELSE

!   scalar_tends(ims,kms,jms,im) =Tmpv_11  ! Remarked by Ning Pan, 2010-08-02

   CALL a_vertical_diffusion('m',scalar(ims,kms,jms,im),a_scalar(ims,kms,jms,im)  &
   ,scalar_tends(ims,kms,jms,im),a_scalar_tends(ims,kms,jms,im),config_flags,alt,  &
   a_alt,mut,a_mut,rdn,rdnw,kvdq,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,  &
   its,ite,jts,jte,kts,kte)

! Remarked by Ning Pan, 2010-08-02
!   IF( (moist_step) .and. ( im == P_QV)) THEN

!   scalar_tends(ims,kms,jms,im) =Tmpv_10

!   CALL a_vertical_diffusion_mp(scalar(ims,kms,jms,im),a_scalar(ims,kms,jms,im)  &
!   ,scalar_tends(ims,kms,jms,im),a_scalar_tends(ims,kms,jms,im),config_flags,base,alt,  &
!   a_alt,mut,a_mut,rdn,rdnw,kvdq,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,  &
!   its,ite,jts,jte,kts,kte)

   END IF

   ENDIF

!   scalar_tends(ims,kms,jms,im) =Tmpv_9  ! Remarked by Ning Pan, 2010-08-02

   CALL a_horizontal_diffusion('m',scalar(ims,kms,jms,im),a_scalar(ims,kms,jms,  &
   im),scalar_tends(ims,kms,jms,im),a_scalar_tends(ims,kms,jms,im),mut,a_mut,  &
   config_flags,msfux,msfuy,msfvx,msfvx_inv,msfvy,msftx,msfty,khdq,xkmhd,a_xkmhd,rdx,  &
   rdy,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   ENDIF

   ENDIF

    IF((config_flags%cu_physics == GDSCHEME .OR. config_flags%cu_physics == G3SCHEME .OR. &
         config_flags%cu_physics == KFETASCHEME .OR. &      ! new trigger in KF
         config_flags%cu_physics == MSKFSCHEME  .OR. &
         config_flags%cu_physics == TIEDTKESCHEME .OR. &    ! Tiedtke
         config_flags%cu_physics == NTIEDTKESCHEME)    &    ! new Tiedtke
         .and. moist_step .and. ( im == P_QV) ) THEN

   CALL a_set_tend(RQVFTEN,a_RQVFTEN,advect_tend,a_advect_tend,msfty,ids,ide,  &
   jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   ENDIF

   IF( (rk_step == 3) .and. (adv_opt == POSITIVEDEF) ) THEN

!   advect_tend(ims,kms,jms) =Tmpv_3  ! Remarked by Ning Pan, 2010-08-02

!   scalar_old(ims,kms,jms,im) =Tmpv_2  ! Remarked by Ning Pan, 2010-08-02

!   scalar(ims,kms,jms,im) =Tmpv_1  ! Remarked by Ning Pan, 2010-08-02

   CALL a_advect_scalar_pd(scalar(ims,kms,jms,im),a_scalar(ims,kms,jms,im)  &
   ,scalar_old(ims,kms,jms,im),a_scalar_old(ims,kms,jms,im),advect_tend(ims,kms,jms)  &
   ,a_advect_tend(ims,kms,jms),h_tendency(ims,kms,jms),a_h_tendency(ims,kms,jms),z_tendency(ims,kms,jms),a_z_tendency(ims,kms,jms) &
   ,ru,a_ru,rv,a_rv,ww,a_ww,mut,a_mut,mub,mu_old,  &
   a_mu_old,time_step,config_flags,tenddec,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm,fnp,rdx,  &
! Revised by Ning Pan, 2010-08-02
!   rdy,rdnw,dt,a_dt,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
   rdy,rdnw,dt,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   ELSE IF( (rk_step == 3) .and. (adv_opt == MONOTONIC) ) THEN

!   advect_tend(ims,kms,jms) =Tmpv_6  ! Remarked by Ning Pan, 2010-08-02

!   scalar_old(ims,kms,jms,im) =Tmpv_5  ! Remarked by Ning Pan, 2010-08-02

!   scalar(ims,kms,jms,im) =Tmpv_4  ! Remarked by Ning Pan, 2010-08-02

   CALL a_advect_scalar_mono(scalar(ims,kms,jms,im),a_scalar(ims,kms,jms,im)  &
   ,scalar_old(ims,kms,jms,im),a_scalar_old(ims,kms,jms,im),advect_tend(ims,kms,jms)  &
   ,a_advect_tend(ims,kms,jms),h_tendency(ims,kms,jms),a_h_tendency(ims,kms,jms),z_tendency(ims,kms,jms),a_z_tendency(ims,kms,jms) &
   ,ru,a_ru,rv,a_rv,ww,a_ww,mut,a_mut,mub,mu_old,  &
   a_mu_old,config_flags,tenddec,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm,fnp,rdx,rdy,rdnw,dt,  &
! Revised by Ning Pan, 2010-08-02
!   a_dt,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
   ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   ELSE IF( (rk_step == 3) .and. (adv_opt == WENO_SCALAR) ) THEN

   CALL a_advect_scalar_weno ( scalar(ims,kms,jms,im),        &
                               a_scalar(ims,kms,jms,im),      &
                               scalar(ims,kms,jms,im),        &
                               a_scalar(ims,kms,jms,im),      &
                               advect_tend(ims,kms,jms),      &
                               a_advect_tend(ims,kms,jms),    &
                               ru, a_ru, rv, a_rv, ww, a_ww,  &
                               mut, time_step,    &
                               config_flags,                  &
                               msfux, msfuy, msfvx, msfvy,    &
                               msftx, msfty, fnm, fnp,        &
                               rdx, rdy, rdnw,                &
                               ids, ide, jds, jde, kds, kde,  &
                               ims, ime, jms, jme, kms, kme,  &
                               its, ite, jts, jte, kts, kte  )

   ELSEIF( (rk_step == 3) .and. (adv_opt == WENOPD_SCALAR) ) THEN

   CALL a_advect_scalar_wenopd   ( scalar(ims,kms,jms,im),             &  
                                 a_scalar(ims,kms,jms,im),           &
                                 scalar_old(ims,kms,jms,im),         &    
                                 a_scalar_old(ims,kms,jms,im),       &
                                 advect_tend(ims,kms,jms),           &    
                                 a_advect_tend(ims,kms,jms),         &
                                 ru, a_ru, rv, a_rv, ww, a_ww,       &
                                 mut, a_mut, mub, mu_old, a_mu_old,  &
                                 time_step, config_flags,            &    
                                 msfux, msfuy, msfvx, msfvy,         &    
                                 msftx, msfty, fnm, fnp,             &    
                                 rdx, rdy, rdnw,dt,                  &    
                                 ids, ide, jds, jde, kds, kde,       &    
                                 ims, ime, jms, jme, kms, kme,       &    
                                 its, ite, jts, jte, kts, kte     )    

   ELSE

! Remarked by Ning Pan, 2010-08-02
!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   rv(IX1,IX2,IX3) =Tmpv401(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   DO IX3=jms,jme
!   DO IX2=kms,kme
!   DO IX1=ims,ime
!   ru(IX1,IX2,IX3) =Tmpv400(IX1,IX2,IX3)
!   END DO
!   END DO
!   END DO

!   advect_tend(ims,kms,jms) =Tmpv_8

!   scalar(ims,kms,jms,im) =Tmpv_7

   CALL a_advect_scalar(scalar(ims,kms,jms,im),a_scalar(ims,kms,jms,im)  &
   ,scalar(ims,kms,jms,im),a_scalar(ims,kms,jms,im),advect_tend(ims,kms,jms)  &
   ,a_advect_tend(ims,kms,jms),ru,a_ru,rv,a_rv,ww,a_ww,mut,time_step,  &
   config_flags,msfux,msfuy,msfvx,msfvy,msftx,msfty,fnm,fnp,rdx,rdy,rdnw,ids,ide,jds,  &
   jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

   END IF

! Added by Ning Pan, 2010-08-02
   CALL a_zero_tend(a_advect_tend,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
   kms,kme,its,ite,jts,jte,kts,kte)
   CALL a_zero_tend(a_h_tendency,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
   kms,kme,its,ite,jts,jte,kts,kte)
   CALL a_zero_tend(a_z_tendency,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,  &
   kms,kme,its,ite,jts,jte,kts,kte)

   ENDDO

!LPB[0]
!  khdq =khdif/prandtl
!  kvdq =kvdif/prandtl

   END SUBROUTINE a_rk_scalar_tend

!-------------------------------------------------------------------------------
!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.10 (r5498) - 20 Jan 2015 09:48
!
!  Differentiation of q_diabatic_add in reverse (adjoint) mode:
!   gradient     of useful results: qc_diabatic qv_diabatic scalar_tends
!                mu
!   with respect to varying inputs: qc_diabatic qv_diabatic scalar_tends
!                mu
!   RW status of diff variables: qc_diabatic:incr qv_diabatic:incr
!                scalar_tends:in-out mu:incr
SUBROUTINE a_Q_DIABATIC_ADD(scs, sce, dt, mu, mub, qv_diabatic, &
& qv_diabaticb, qc_diabatic, qc_diabaticb, scalar_tends, scalar_tendsb, &
& ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, its, ite, &
& jts, jte, kts, kte)
  IMPLICIT NONE
!  Input data.
  INTEGER, INTENT(IN) :: scs, sce
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
& jme, kms, kme, its, ite, jts, jte, kts, kte
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: mu
  REAL, DIMENSION(ims:ime, jms:jme) :: mub
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: qv_diabatic&
& , qc_diabatic
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: qv_diabaticb, &
& qc_diabaticb
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme, scs:sce), INTENT(INOUT) :: &
& scalar_tends
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme, scs:sce), INTENT(INOUT) :: &
& scalar_tendsb
  REAL, INTENT(IN) :: dt
! Local data
  INTEGER :: im, i, j, k
  INTEGER :: ad_to
  INTEGER :: ad_to0
  INTEGER :: ad_to1
  INTEGER :: ad_to2
  INTEGER :: branch
  INTEGER :: min4
  INTEGER :: min3
  INTEGER :: min2
  INTEGER :: min1
scalar_loop:DO im=scs,sce
    IF (im .EQ. p_qv) THEN
      IF (jte .GT. jde - 1) THEN
        min1 = jde - 1
      ELSE
        min1 = jte
      END IF
      DO j=jts,min1
        DO k=kts,kte-1
          IF (ite .GT. ide - 1) THEN
            min2 = ide - 1
          ELSE
            min2 = ite
          END IF
          i = min2 + 1
          CALL PUSHINTEGER4(i - 1)
        END DO
      END DO
      CALL PUSHINTEGER4(j - 1)
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (im .EQ. p_qc) THEN
      IF (jte .GT. jde - 1) THEN
        min3 = jde - 1
      ELSE
        min3 = jte
      END IF
      DO j=jts,min3
        DO k=kts,kte-1
          IF (ite .GT. ide - 1) THEN
            min4 = ide - 1
          ELSE
            min4 = ite
          END IF
          i = min4 + 1
          CALL PUSHINTEGER4(i - 1)
        END DO
      END DO
      CALL PUSHINTEGER4(j - 1)
      CALL PUSHCONTROL1B(1)
    ELSE
      CALL PUSHCONTROL1B(0)
    END IF
  END DO scalar_loop
  DO im=sce,scs,-1
    CALL POPCONTROL1B(branch)
    IF (branch .NE. 0) THEN
      CALL POPINTEGER4(ad_to2)
      DO j=ad_to2,jts,-1
        DO k=kte-1,kts,-1
          CALL POPINTEGER4(ad_to1)
          DO i=ad_to1,its,-1
            qc_diabaticb(i,k,j) = qc_diabaticb(i,k,j) + &
                                  mu(i,j)*scalar_tendsb(i,k,j,im)
            mub(i,j) = mub(i,j) + &
                       qc_diabatic(i,k,j)*scalar_tendsb(i,k,j,im)
          END DO
        END DO
      END DO
    END IF
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      CALL POPINTEGER4(ad_to0)
      DO j=ad_to0,jts,-1
        DO k=kte-1,kts,-1
          CALL POPINTEGER4(ad_to)
          DO i=ad_to,its,-1
            qv_diabaticb(i,k,j) = qv_diabaticb(i,k,j) + &
                                  mu(i,j)*scalar_tendsb(i,k,j,im)
            mub(i,j) = mub(i,j) + &
                       qv_diabatic(i,k,j)*scalar_tendsb(i,k,j,im)
          END DO
        END DO
      END DO
    END IF
  END DO
END SUBROUTINE a_Q_DIABATIC_ADD

!-------------------------------------------------------------------------------
!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.10 (r5498) - 20 Jan 2015 09:48
!
!  Differentiation of q_diabatic_subtr in reverse (adjoint) mode:
!   gradient     of useful results: qc_diabatic qv_diabatic scalar
!   with respect to varying inputs: qc_diabatic qv_diabatic scalar
!   RW status of diff variables: qc_diabatic:incr qv_diabatic:incr
!                scalar:in-out
SUBROUTINE a_Q_DIABATIC_SUBTR(scs, sce, dt, qv_diabatic, qv_diabaticb, &
& qc_diabatic, qc_diabaticb, scalar, scalarb, ids, ide, jds, jde, kds, &
& kde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte)
  IMPLICIT NONE
!  Input data.
  INTEGER, INTENT(IN) :: scs, sce
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
& jme, kms, kme, its, ite, jts, jte, kts, kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: qv_diabatic&
& , qc_diabatic
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: qv_diabaticb, &
& qc_diabaticb
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme, scs:sce), INTENT(INOUT) :: &
& scalar
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme, scs:sce), INTENT(INOUT) :: &
& scalarb
  REAL, INTENT(IN) :: dt
! Local data
  INTEGER :: im, i, j, k
  INTEGER :: ad_to
  INTEGER :: ad_to0
  INTEGER :: ad_to1
  INTEGER :: ad_to2
  INTEGER :: branch
  INTEGER :: min4
  INTEGER :: min3
  INTEGER :: min2
  INTEGER :: min1
scalar_loop:DO im=scs,sce
    IF (im .EQ. p_qv) THEN
      IF (jte .GT. jde - 1) THEN
        min1 = jde - 1
      ELSE
        min1 = jte
      END IF
      DO j=jts,min1
        DO k=kts,kte-1
          IF (ite .GT. ide - 1) THEN
            min2 = ide - 1
          ELSE
            min2 = ite
          END IF
          i = min2 + 1
          CALL PUSHINTEGER4(i - 1)
        END DO
      END DO
      CALL PUSHINTEGER4(j - 1)
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (im .EQ. p_qc) THEN
      IF (jte .GT. jde - 1) THEN
        min3 = jde - 1
      ELSE
        min3 = jte
      END IF
      DO j=jts,min3
        DO k=kts,kte-1
          IF (ite .GT. ide - 1) THEN
            min4 = ide - 1
          ELSE
            min4 = ite
          END IF
          i = min4 + 1
          CALL PUSHINTEGER4(i - 1)
        END DO
      END DO
      CALL PUSHINTEGER4(j - 1)
      CALL PUSHCONTROL1B(1)
    ELSE
      CALL PUSHCONTROL1B(0)
    END IF
  END DO scalar_loop
  DO im=sce,scs,-1
    CALL POPCONTROL1B(branch)
    IF (branch .NE. 0) THEN
      CALL POPINTEGER4(ad_to2)
      DO j=ad_to2,jts,-1
        DO k=kte-1,kts,-1
          CALL POPINTEGER4(ad_to1)
          DO i=ad_to1,its,-1
            qc_diabaticb(i,k,j) = qc_diabaticb(i,k,j) - &
                                  dt*scalarb(i,k,j,im)
          END DO
        END DO
      END DO
    END IF
    CALL POPCONTROL1B(branch)
    IF (branch .EQ. 0) THEN
      CALL POPINTEGER4(ad_to0)
      DO j=ad_to0,jts,-1
        DO k=kte-1,kts,-1
          CALL POPINTEGER4(ad_to)
          DO i=ad_to,its,-1
            qv_diabaticb(i,k,j) = qv_diabaticb(i,k,j) - &
                                  dt*scalarb(i,k,j,im)
          END DO
        END DO
      END DO
    END IF
  END DO
END SUBROUTINE a_Q_DIABATIC_SUBTR

!-------------------------------------------------------------------------------

SUBROUTINE a_rk_update_scalar ( scs, sce,                      &
                                scalar_1, a_scalar_1, scalar_2, a_scalar_2, sc_tend, a_sc_tend,  &
                                advh_t, a_advh_t, advz_t,  a_advz_t,             & 
                                advect_tend, a_advect_tend,    &
                                h_tendency, a_h_tendency, z_tendency, a_z_tendency,  & 
                                msftx, msfty,                  &
                                mu_old, a_mu_old, mu_new, a_mu_new, mu_base,  &
                                rk_step, dt, spec_zone,        &
                                config_flags,                  &
                                tenddec,                       &
                                ids, ide, jds, jde, kds, kde,  &
                                ims, ime, jms, jme, kms, kme,  &
                                its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE

   !  Input data.

   TYPE(grid_config_rec_type), INTENT(IN) :: config_flags
   LOGICAL :: tenddec

   INTEGER, INTENT(IN) :: scs, sce, rk_step, spec_zone
   INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte

   REAL,    INTENT(IN) :: dt

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce),                &
         INTENT(INOUT)                                  :: a_scalar_1,  &
                                                           a_scalar_2
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce),                &
         INTENT(IN)                                     :: scalar_1,    &
                                                           scalar_2

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce),                &
         INTENT(INOUT)                                  :: a_sc_tend
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme , scs:sce),                &
         INTENT(IN)                                     :: sc_tend

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ),                &
         INTENT(INOUT)                               :: a_advect_tend
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ),                &
         INTENT(IN)                                  :: advect_tend

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ), OPTIONAL :: advh_t,  advz_t ! accumulating for output
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ), OPTIONAL :: a_advh_t,  a_advz_t ! accumulating for output
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ) :: h_tendency, z_tendency ! from rk_scalar_tend
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ) :: a_h_tendency, a_z_tendency ! from rk_scalar_tend

   REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) ::  a_mu_old,  &
                                                        a_mu_new
   REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN   ) ::  mu_old,  &
                                                        mu_new,  &
                                                        mu_base, &
                                                        msftx,   &
                                                        msfty

   INTEGER :: i,j,k,im
   REAL    :: sc_middle, msfsq
   REAL, DIMENSION(its:ite) :: a_muold, a_r_munew
   REAL, DIMENSION(its:ite) :: muold, r_munew

   REAL, DIMENSION(its:ite, kts:kte, jts:jte  ) :: a_tendency
   REAL, DIMENSION(its:ite, kts:kte, jts:jte  ) :: tendency

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme, scs:sce) :: scalar_old

   INTEGER :: i_start,i_end,j_start,j_end,k_start,k_end
   INTEGER :: i_start_spc,i_end_spc,j_start_spc,j_end_spc,k_start_spc,k_end_spc

!<DESCRIPTION> !
!  Basic states: mu_old, mu_new, advect_tend, sc_tend, scalar_2(rk_step=1), scalar_1(rk_step/=1)
!
!</DESCRIPTION>

!  Initilize local adjoint variables
   a_muold = 0.0
   a_r_munew = 0.0
   a_tendency = 0.0
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

     DO  im = sce,scs,-1

!     Recalculate tendency
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
         ! scalar was coupled with my
          tendency(i,k,j) = advect_tend(i,k,j) * msfty(i,j)
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

!     Recalculate muold and r_munew
      DO  i = its, min(ite,ide-1)
        muold(i) = mu_old(i,j) + mu_base(i,j)
        r_munew(i) = 1./(mu_new(i,j) + mu_base(i,j))
      ENDDO

      DO  k = kts, min(kte,kde-1)
      DO  i = its, min(ite,ide-1)
!       Recalculate scalar_1 (i.e. scalar_old)
        scalar_old(i,k,j,im) = scalar_2(i,k,j,im)

        a_scalar_1(i,k,j,im) = a_scalar_1(i,k,j,im) + muold(i)*r_munew(i) * a_scalar_2(i,k,j,im)
        a_muold(i) = a_muold(i) + scalar_old(i,k,j,im)*r_munew(i) * a_scalar_2(i,k,j,im)
        a_tendency(i,k,j) = a_tendency(i,k,j) + dt*r_munew(i) * a_scalar_2(i,k,j,im)
        a_r_munew(i) = a_r_munew(i) + (muold(i)*scalar_old(i,k,j,im)+dt*tendency(i,k,j)) * a_scalar_2(i,k,j,im)
        a_scalar_2(i,k,j,im) = 0.0

        a_scalar_2(i,k,j,im) = a_scalar_2(i,k,j,im) + a_scalar_1(i,k,j,im)
        a_scalar_1(i,k,j,im) = 0.0
      ENDDO !i
      ENDDO !k

      DO  i = its, min(ite,ide-1)
        a_mu_new(i,j) = a_mu_new(i,j) - a_r_munew(i) / ((mu_new(i,j)+mu_base(i,j))*(mu_new(i,j)+mu_base(i,j)))
        a_r_munew(i) = 0.0

        a_mu_old(i,j) = a_mu_old(i,j) + a_muold(i)
        a_muold(i) = 0.0
      ENDDO

      ENDDO !j

      DO  j = j_start_spc,j_end_spc
      DO  k = k_start_spc,k_end_spc
      DO  i = i_start_spc,i_end_spc
          a_sc_tend(i,k,j,im) = a_sc_tend(i,k,j,im) + a_tendency(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO  j = j_start,j_end
      DO  k = k_start,k_end
      DO  i = i_start,i_end
         ! scalar was coupled with my
          a_advect_tend(i,k,j) = a_advect_tend(i,k,j) + msfty(i,j) * a_tendency(i,k,j)
          a_tendency(i,k,j) = 0.0
      ENDDO
      ENDDO
      ENDDO

      DO  j = jts, min(jte,jde-1)
      DO  k = kts, min(kte,kde-1)
      DO  i = its, min(ite,ide-1)
          a_tendency(i,k,j) = 0.
      ENDDO
      ENDDO
      ENDDO

     ENDDO !im

    ELSE

     DO  im = sce, scs, -1

!     Recalculate tendency
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
         ! scalar was coupled with my
          tendency(i,k,j) = advect_tend(i,k,j) * msfty(i,j)
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

!     Recalculate muold and r_munew
      DO  i = its, min(ite,ide-1)
        muold(i) = mu_old(i,j) + mu_base(i,j)
        r_munew(i) = 1./(mu_new(i,j) + mu_base(i,j))
      ENDDO

      ! This is separated from the k/i-loop above for better performance
      IF ( PRESENT(advh_t) .AND. PRESENT(advz_t) .AND. PRESENT(a_advh_t) .AND. PRESENT(a_advz_t) ) THEN
         IF (tenddec.and.rk_step.eq.config_flags%rk_ord) THEN
            DO k = kts, min(kte,kde-1)
            DO i = its, min(ite,ide-1)

               a_h_tendency(i,k,j) = a_h_tendency(i,k,j) + dt*msfty(i,j)*r_munew(i)*a_advh_t(i,k,j)
               a_r_munew(i) = a_r_munew(i) + (dt*h_tendency(i,k,j)* msfty(i,j))*a_advh_t(i,k,j)
               a_z_tendency(i,k,j) = a_z_tendency(i,k,j) + dt*msfty(i,j)*r_munew(i)*a_advz_t(i,k,j)
               a_r_munew(i) = a_r_munew(i) + (dt*z_tendency(i,k,j)* msfty(i,j))*a_advz_t(i,k,j)

            ENDDO
            ENDDO
         END IF
      END IF

      DO  k = kts, min(kte,kde-1)
      DO  i = its, min(ite,ide-1)

        a_scalar_1(i,k,j,im) = a_scalar_1(i,k,j,im) + muold(i)*r_munew(i) * a_scalar_2(i,k,j,im)
        a_muold(i) = a_muold(i) + scalar_1(i,k,j,im)*r_munew(i) * a_scalar_2(i,k,j,im)
        a_tendency(i,k,j) = a_tendency(i,k,j) + dt*r_munew(i) * a_scalar_2(i,k,j,im)
        a_r_munew(i) = a_r_munew(i) + (muold(i)*scalar_1(i,k,j,im)+dt*tendency(i,k,j)) * a_scalar_2(i,k,j,im)

        a_scalar_2(i,k,j,im) = 0.0

      ENDDO
      ENDDO

      DO  i = its, min(ite,ide-1)
        a_mu_new(i,j) = a_mu_new(i,j) - a_r_munew(i) / ((mu_new(i,j)+mu_base(i,j))*(mu_new(i,j)+mu_base(i,j)))
        a_r_munew(i) = 0.0

        a_mu_old(i,j) = a_mu_old(i,j) + a_muold(i)
        a_muold(i) = 0.0
      ENDDO

      ENDDO !j

       DO  j = j_start_spc,j_end_spc
       DO  k = k_start_spc,k_end_spc
       DO  i = i_start_spc,i_end_spc
          a_sc_tend(i,k,j,im) = a_sc_tend(i,k,j,im) + a_tendency(i,k,j)
       ENDDO
       ENDDO
       ENDDO

       DO  j = j_start,j_end
       DO  k = k_start,k_end
       DO  i = i_start,i_end
          ! scalar was coupled with my
          a_advect_tend(i,k,j) = a_advect_tend(i,k,j) + msfty(i,j) * a_tendency(i,k,j)
          a_tendency(i,k,j) = 0.0
       ENDDO
       ENDDO
       ENDDO

       DO  j = jts, min(jte,jde-1)
       DO  k = kts, min(kte,kde-1)
       DO  i = its, min(ite,ide-1)
           a_tendency(i,k,j) = 0.
       ENDDO
       ENDDO
       ENDDO

      ENDDO !im

    END IF

END SUBROUTINE a_rk_update_scalar

!-------------------------------------------------------------------------------

   SUBROUTINE a_rk_update_scalar_pd(scs,sce,scalar,a_scalar,sc_tend,a_sc_tend, &
   mu_old,a_mu_old,mu_new,a_mu_new,mu_base,rk_step,dt,spec_zone, &
   config_flags,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!PART I: DECLARATION OF VARIABLES

   IMPLICIT NONE

   INTEGER :: K0_ADJ,K1_ADJ,K2_ADJ,K3_ADJ
   TYPE(grid_config_rec_type) :: config_flags
   INTEGER :: scs,sce,rk_step,spec_zone
   INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
   REAL :: dt
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme,scs:sce) :: scalar,a_scalar,sc_tend,a_sc_tend
   REAL,DIMENSION(ims:ime,jms:jme) :: mu_old,a_mu_old,mu_new,a_mu_new,mu_base
   INTEGER :: i,j,k,im
   REAL :: sc_middle,sfsq
   REAL,DIMENSION(its:ite) :: muold,a_muold,r_munew,a_r_munew
   REAL,DIMENSION(its:ite,kts:kte,jts:jte) :: tendency,a_tendency
   INTEGER :: i_start,i_end,j_start,j_end,k_start,k_end
   INTEGER :: i_start_spc,i_end_spc,j_start_spc,j_end_spc,k_start_spc,k_end_spc

   REAL :: a_Tmpv1,Tmpv001,a_Tmpv2,Tmpv002,a_Tmpv3
   REAL,ALLOCATABLE,DIMENSION(:,:) :: Tmpv300
   REAL,DIMENSION(its:min(ite, ide-1),jts:min(jte, jde-1)) :: Tmpv301
   REAL,DIMENSION(its:min(ite, ide-1),kts:min(kte, kde-1),jts:min(jte, jde-1)) :: Tmpv400

!PART II: CALCULATIONS OF B. S. TRAJECTORY

!LPB[0]

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

!LPB[1]
    IF( config_flags%nested .or. config_flags%specified ) THEN
      IF( .NOT. config_flags%periodic_x)i_start = max( its,ids+spec_zone )
      IF( .NOT. config_flags%periodic_x)i_end   = min( ite,ide-spec_zone-1 )
         j_start = max( jts,jds+spec_zone )
         j_end   = min( jte,jde-spec_zone-1 )
         k_start = kts
         k_end   = min( kte, kde-1 )
   ENDIF

!PART III: INITIALIZATION OF LOCAL ADJOINT PERTURBATIONS

   a_muold =0.
   a_r_munew =0.

!PART IV: REVERSE/BACKWARD ACCUMULATIONS

!LPB[2]
   DO im =sce, scs, -1

   tendency(its:min(ite,ide-1),kts:min(kte,kde-1),jts:min(jte,jde-1)) =0.

   tendency(i_start_spc:i_end_spc,k_start_spc:k_end_spc,j_start_spc:j_end_spc) =tendency&
   (i_start_spc:i_end_spc,k_start_spc:k_end_spc,j_start_spc:j_end_spc) +sc_tend&
   (i_start_spc:i_end_spc,k_start_spc:k_end_spc,j_start_spc:j_end_spc,im)

   sc_tend(i_start_spc:i_end_spc,k_start_spc:k_end_spc,j_start_spc:j_end_spc,im) =0.

   ALLOCATE (Tmpv300(its:min(ite, ide-1),jts:min(jte, jde-1)))

   DO j =jts, min(jte, jde-1)
     DO i =its, min(ite, ide-1)
     Tmpv300(i,j) =mu_old(i,j) +mu_base(i,j)
     ENDDO

     DO k =kts, min(kte, kde-1)
     DO i =its, min(ite, ide-1)
     Tmpv400(i,k,j) =Tmpv300(i,j)*scalar(i,k,j,im)+dt*tendency(i,k,j)
     ENDDO
     ENDDO
   ENDDO

   DO j =min(jte, jde-1), jts, -1

   DO k =kts, min(kte, kde-1)
   DO i =its, min(ite, ide-1)
   a_r_munew(i) =a_r_munew(i) +Tmpv400(i,k,j)*a_scalar(i,k,j,im)
   a_Tmpv1 = a_scalar(i,k,j,im)/(mu_new(i,j)+mu_base(i,j))
   a_tendency(i,k,j) =dt*a_Tmpv1
   a_muold(i) =a_muold(i) +scalar(i,k,j,im)*a_Tmpv1
   a_scalar(i,k,j,im) =Tmpv300(i,j)*a_Tmpv1
   ENDDO
   ENDDO

   DO i =its, min(ite, ide-1)
   a_mu_new(i,j) =a_mu_new(i,j)-a_r_munew(i)/(mu_new(i,j)+mu_base(i,j))/(mu_new(i,j)+mu_base(i,j))
   ENDDO

   a_r_munew(its:min(ite,ide-1)) =0.0
   a_mu_old(its:min(ite,ide-1),j) =a_mu_old(its:min(ite,ide-1),j) +a_muold(its:min(ite,ide-1))
   a_muold(its:min(ite,ide-1)) =0.0

   ENDDO

   a_sc_tend(i_start_spc:i_end_spc,k_start_spc:k_end_spc,j_start_spc:j_end_spc,im) =a_tendency(&
   i_start_spc:i_end_spc,k_start_spc:k_end_spc,j_start_spc:j_end_spc)

   ENDDO

   DEALLOCATE (Tmpv300)

   END SUBROUTINE a_rk_update_scalar_pd

!------------------------------------------------------------

!        Generated by TAPENADE     (INRIA, Tropics team)
!  Tapenade 3.5 (r3785) - 22 Mar 2011 18:35
!
!  Differentiation of calculate_phy_tend in reverse (adjoint) mode:
!   gradient     of useful results: rthndgdten rublten rqvndgdten
!                rthraten rqccuten rthcuten rqicuten rvndgdten
!                rqscuten rqrshten rqvshten rucuten rvshten rqvblten
!                rvblten rqcshten rthshten rqgshten rqishten rqcblten
!                rthblten rqrcuten rqiblten rqsshten rqvcuten rvcuten
!                rushten muu muv rundgdten mu
!   with respect to varying inputs: rthndgdten rublten rqvndgdten
!                rthraten rqccuten rthcuten rqicuten rvndgdten
!                rqscuten rqrshten rqvshten rucuten rvshten rqvblten
!                rvblten rqcshten rthshten rqgshten rqishten rqcblten
!                rthblten rqrcuten rqiblten rqsshten rqvcuten rvcuten
!                rushten muu muv rundgdten mu
!   RW status of diff variables: rthndgdten:in-out rublten:in-out
!                rqvndgdten:in-out rthraten:in-out rqccuten:in-out
!                rthcuten:in-out rqicuten:in-out rvndgdten:in-out
!                rqscuten:in-out rqrshten:in-out rqvshten:in-out
!                rucuten:in-out rvshten:in-out rqvblten:in-out
!                rvblten:in-out rqcshten:in-out rthshten:in-out
!                rqgshten:in-out rqishten:in-out rqcblten:in-out
!                rthblten:in-out rqrcuten:in-out rqiblten:in-out
!                rqsshten:in-out rqvcuten:in-out rvcuten:in-out
!                rushten:in-out muu:incr muv:incr rundgdten:in-out
!                mu:incr
SUBROUTINE A_CALCULATE_PHY_TEND(config_flags, mu, mub, muu, muub, muv, &
&  muvb, pi3d, rthraten, rthratenb, rublten, rubltenb, rvblten, rvbltenb&
&  , rthblten, rthbltenb, rqvblten, rqvbltenb, rqcblten, rqcbltenb, &
&  rqiblten, rqibltenb, rucuten, rucutenb, rvcuten, rvcutenb, rthcuten, &
&  rthcutenb, rqvcuten, rqvcutenb, rqccuten, rqccutenb, rqrcuten, &
&  rqrcutenb, rqicuten, rqicutenb, rqscuten, rqscutenb, rushten, rushtenb&
&  , rvshten, rvshtenb, rthshten, rthshtenb, rqvshten, rqvshtenb, &
&  rqcshten, rqcshtenb, rqrshten, rqrshtenb, rqishten, rqishtenb, &
&  rqsshten, rqsshtenb, rqgshten, rqgshtenb, rundgdten, rundgdtenb, &
&  rvndgdten, rvndgdtenb, rthndgdten, rthndgdtenb, rqvndgdten, &
&  rqvndgdtenb, rmundgdten, ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&  jme, kms, kme, its, ite, jts, jte, kts, kte)
  IMPLICIT NONE
  TYPE(GRID_CONFIG_REC_TYPE), INTENT(IN) :: config_flags
  INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, ims, ime, jms, &
&  jme, kms, kme, its, ite, jts, jte, kts, kte
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: pi3d
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: mu, muu, muv
  REAL, DIMENSION(ims:ime, jms:jme) :: mub, muub, muvb
! radiation
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: rthraten
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: rthratenb
! cumulus
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: rucuten, &
&  rvcuten, rthcuten, rqvcuten, rqccuten, rqrcuten, rqicuten, rqscuten, &
&  rushten, rvshten, rthshten, rqvshten, rqcshten, rqrshten, rqishten, &
&  rqsshten, rqgshten
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: rucutenb, rvcutenb, &
&  rthcutenb, rqvcutenb, rqccutenb, rqrcutenb, rqicutenb, rqscutenb, &
&  rushtenb, rvshtenb, rthshtenb, rqvshtenb, rqcshtenb, rqrshtenb, &
&  rqishtenb, rqsshtenb, rqgshtenb
! pbl
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: rublten, &
&  rvblten, rthblten, rqvblten, rqcblten, rqiblten
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: rubltenb, rvbltenb, &
&  rthbltenb, rqvbltenb, rqcbltenb, rqibltenb
! fdda
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: rundgdten&
&  , rvndgdten, rthndgdten, rqvndgdten
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: rundgdtenb, rvndgdtenb, &
&  rthndgdtenb, rqvndgdtenb
  REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: rmundgdten
  INTEGER :: i, k, j
  INTEGER :: itf, ktf, jtf, itsu, jtsv
  INTEGER :: branch
  IF (ite .GT. ide - 1) THEN
    itf = ide - 1
  ELSE
    itf = ite
  END IF
  IF (jte .GT. jde - 1) THEN
    jtf = jde - 1
  ELSE
    jtf = jte
  END IF
  IF (kte .GT. kde - 1) THEN
    ktf = kde - 1
  ELSE
    ktf = kte
  END IF
  IF (its .LT. ids + 1) THEN
    itsu = ids + 1
  ELSE
    itsu = its
  END IF
  IF (jts .LT. jds + 1) THEN
    jtsv = jds + 1
  ELSE
    jtsv = jts
  END IF
! radiation
  IF (config_flags%ra_lw_physics .GT. 0 .OR. config_flags%ra_sw_physics &
&      .GT. 0) THEN
    CALL PUSHCONTROL1B(0)
  ELSE
    CALL PUSHCONTROL1B(1)
  END IF
! cumulus
  IF (config_flags%cu_physics .GT. 0) THEN
    IF (p_qc .GE. param_first_scalar) THEN
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (p_qr .GE. param_first_scalar) THEN
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (p_qi .GE. param_first_scalar) THEN
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (p_qs .GE. param_first_scalar) THEN
      CALL PUSHCONTROL2B(0)
    ELSE
      CALL PUSHCONTROL2B(1)
    END IF
  ELSE
    CALL PUSHCONTROL2B(2)
  END IF
! shallow cumulus
  IF (config_flags%shcu_physics .GT. 0) THEN
    IF (p_qc .GE. param_first_scalar) THEN
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (p_qr .GE. param_first_scalar) THEN
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (p_qi .GE. param_first_scalar) THEN
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (p_qs .GE. param_first_scalar) THEN
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (p_qg .GE. param_first_scalar) THEN
      CALL PUSHCONTROL2B(0)
    ELSE
      CALL PUSHCONTROL2B(1)
    END IF
  ELSE
    CALL PUSHCONTROL2B(2)
  END IF
! pbl
  IF (config_flags%bl_pbl_physics .GT. 0) THEN
    IF (p_qv .GE. param_first_scalar) THEN
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (p_qc .GE. param_first_scalar) THEN
      CALL PUSHCONTROL1B(0)
    ELSE
      CALL PUSHCONTROL1B(1)
    END IF
    IF (p_qi .GE. param_first_scalar) THEN
      CALL PUSHCONTROL2B(0)
    ELSE
      CALL PUSHCONTROL2B(1)
    END IF
  ELSE
    CALL PUSHCONTROL2B(2)
  END IF
! fdda
! note fdda u and v tendencies are staggered, also only interior points have muu/muv,
!   so only couple those
  IF (config_flags%grid_fdda .GT. 0) THEN
!        RMUNDGDTEN(I,J) - no coupling
!     if( i == itf/2 .AND. j == jtf/2 .AND. k == ktf/2 ) &
!     write(*,'(a,3i6,e15.5)') 'th after=',i,k,j, RTHNDGDTEN(I,K,J)
    IF (p_qv .GE. param_first_scalar) THEN
      DO j=jtf,jts,-1
        DO k=ktf,kts,-1
          DO i=itf,its,-1
            mub(i, j) = mub(i, j) + rqvndgdten(i, k, j)*rqvndgdtenb(i, k&
&              , j)
            rqvndgdtenb(i, k, j) = mu(i, j)*rqvndgdtenb(i, k, j)
          END DO
        END DO
      END DO
    END IF
    DO j=jtf,jts,-1
      DO k=ktf,kts,-1
        DO i=itf,its,-1
          mub(i, j) = mub(i, j) + rthndgdten(i, k, j)*rthndgdtenb(i, k, &
&            j)
          rthndgdtenb(i, k, j) = mu(i, j)*rthndgdtenb(i, k, j)
        END DO
      END DO
    END DO
    DO j=jtf,jtsv,-1
      DO k=ktf,kts,-1
        DO i=itf,its,-1
          muvb(i, j) = muvb(i, j) + rvndgdten(i, k, j)*rvndgdtenb(i, k, &
&            j)
          rvndgdtenb(i, k, j) = muv(i, j)*rvndgdtenb(i, k, j)
        END DO
      END DO
    END DO
    DO j=jtf,jts,-1
      DO k=ktf,kts,-1
        DO i=itf,itsu,-1
          muub(i, j) = muub(i, j) + rundgdten(i, k, j)*rundgdtenb(i, k, &
&            j)
          rundgdtenb(i, k, j) = muu(i, j)*rundgdtenb(i, k, j)
        END DO
      END DO
    END DO
  END IF
  CALL POPCONTROL2B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO k=ktf,kts,-1
        DO i=itf,its,-1
          mub(i, j) = mub(i, j) + rqiblten(i, k, j)*rqibltenb(i, k, j)
          rqibltenb(i, k, j) = mu(i, j)*rqibltenb(i, k, j)
        END DO
      END DO
    END DO
  ELSE IF (branch .NE. 1) THEN
    GOTO 100
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO k=ktf,kts,-1
        DO i=itf,its,-1
          mub(i, j) = mub(i, j) + rqcblten(i, k, j)*rqcbltenb(i, k, j)
          rqcbltenb(i, k, j) = mu(i, j)*rqcbltenb(i, k, j)
        END DO
      END DO
    END DO
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO k=ktf,kts,-1
        DO i=itf,its,-1
          mub(i, j) = mub(i, j) + rqvblten(i, k, j)*rqvbltenb(i, k, j)
          rqvbltenb(i, k, j) = mu(i, j)*rqvbltenb(i, k, j)
        END DO
      END DO
    END DO
  END IF
  DO j=jtf,jts,-1
    DO k=ktf,kts,-1
      DO i=itf,its,-1
        mub(i, j) = mub(i, j) + rvblten(i, k, j)*rvbltenb(i, k, j) + &
&          rublten(i, k, j)*rubltenb(i, k, j) + rthblten(i, k, j)*&
&          rthbltenb(i, k, j)
        rthbltenb(i, k, j) = mu(i, j)*rthbltenb(i, k, j)
        rvbltenb(i, k, j) = mu(i, j)*rvbltenb(i, k, j)
        rubltenb(i, k, j) = mu(i, j)*rubltenb(i, k, j)
      END DO
    END DO
  END DO
 100 CALL POPCONTROL2B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO i=itf,its,-1
        DO k=ktf,kts,-1
          mub(i, j) = mub(i, j) + rqgshten(i, k, j)*rqgshtenb(i, k, j)
          rqgshtenb(i, k, j) = mu(i, j)*rqgshtenb(i, k, j)
        END DO
      END DO
    END DO
  ELSE IF (branch .NE. 1) THEN
    GOTO 110
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO i=itf,its,-1
        DO k=ktf,kts,-1
          mub(i, j) = mub(i, j) + rqsshten(i, k, j)*rqsshtenb(i, k, j)
          rqsshtenb(i, k, j) = mu(i, j)*rqsshtenb(i, k, j)
        END DO
      END DO
    END DO
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO i=itf,its,-1
        DO k=ktf,kts,-1
          mub(i, j) = mub(i, j) + rqishten(i, k, j)*rqishtenb(i, k, j)
          rqishtenb(i, k, j) = mu(i, j)*rqishtenb(i, k, j)
        END DO
      END DO
    END DO
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO i=itf,its,-1
        DO k=ktf,kts,-1
          mub(i, j) = mub(i, j) + rqrshten(i, k, j)*rqrshtenb(i, k, j)
          rqrshtenb(i, k, j) = mu(i, j)*rqrshtenb(i, k, j)
        END DO
      END DO
    END DO
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO i=itf,its,-1
        DO k=ktf,kts,-1
          mub(i, j) = mub(i, j) + rqcshten(i, k, j)*rqcshtenb(i, k, j)
          rqcshtenb(i, k, j) = mu(i, j)*rqcshtenb(i, k, j)
        END DO
      END DO
    END DO
  END IF
  DO j=jtf,jts,-1
    DO i=itf,its,-1
      DO k=ktf,kts,-1
        mub(i, j) = mub(i, j) + rthshten(i, k, j)*rthshtenb(i, k, j) + &
&          rushten(i, k, j)*rushtenb(i, k, j) + rvshten(i, k, j)*rvshtenb&
&          (i, k, j) + rqvshten(i, k, j)*rqvshtenb(i, k, j)
        rqvshtenb(i, k, j) = mu(i, j)*rqvshtenb(i, k, j)
        rthshtenb(i, k, j) = mu(i, j)*rthshtenb(i, k, j)
        rvshtenb(i, k, j) = mu(i, j)*rvshtenb(i, k, j)
        rushtenb(i, k, j) = mu(i, j)*rushtenb(i, k, j)
      END DO
    END DO
  END DO
 110 CALL POPCONTROL2B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO i=itf,its,-1
        DO k=ktf,kts,-1
          mub(i, j) = mub(i, j) + rqscuten(i, k, j)*rqscutenb(i, k, j)
          rqscutenb(i, k, j) = mu(i, j)*rqscutenb(i, k, j)
        END DO
      END DO
    END DO
  ELSE IF (branch .NE. 1) THEN
    GOTO 120
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO i=itf,its,-1
        DO k=ktf,kts,-1
          mub(i, j) = mub(i, j) + rqicuten(i, k, j)*rqicutenb(i, k, j)
          rqicutenb(i, k, j) = mu(i, j)*rqicutenb(i, k, j)
        END DO
      END DO
    END DO
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO i=itf,its,-1
        DO k=ktf,kts,-1
          mub(i, j) = mub(i, j) + rqrcuten(i, k, j)*rqrcutenb(i, k, j)
          rqrcutenb(i, k, j) = mu(i, j)*rqrcutenb(i, k, j)
        END DO
      END DO
    END DO
  END IF
  CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO i=itf,its,-1
        DO k=ktf,kts,-1
          mub(i, j) = mub(i, j) + rqccuten(i, k, j)*rqccutenb(i, k, j)
          rqccutenb(i, k, j) = mu(i, j)*rqccutenb(i, k, j)
        END DO
      END DO
    END DO
  END IF
  DO j=jtf,jts,-1
    DO i=itf,its,-1
      DO k=ktf,kts,-1
        mub(i, j) = mub(i, j) + rthcuten(i, k, j)*rthcutenb(i, k, j) + &
&          rucuten(i, k, j)*rucutenb(i, k, j) + rvcuten(i, k, j)*rvcutenb&
&          (i, k, j) + rqvcuten(i, k, j)*rqvcutenb(i, k, j)
        rqvcutenb(i, k, j) = mu(i, j)*rqvcutenb(i, k, j)
        rthcutenb(i, k, j) = mu(i, j)*rthcutenb(i, k, j)
        rvcutenb(i, k, j) = mu(i, j)*rvcutenb(i, k, j)
        rucutenb(i, k, j) = mu(i, j)*rucutenb(i, k, j)
      END DO
    END DO
  END DO
 120 CALL POPCONTROL1B(branch)
  IF (branch .EQ. 0) THEN
    DO j=jtf,jts,-1
      DO k=ktf,kts,-1
        DO i=itf,its,-1
          mub(i, j) = mub(i, j) + rthraten(i, k, j)*rthratenb(i, k, j)
          rthratenb(i, k, j) = mu(i, j)*rthratenb(i, k, j)
        END DO
      END DO
    END DO
  END IF
END SUBROUTINE A_CALCULATE_PHY_TEND

!-----------------------------------------------------------

SUBROUTINE a_init_zero_tendency(a_ru_tendf, &
                                a_rv_tendf, &
                                a_rw_tendf, &
                                a_ph_tendf, &
                                a_t_tendf,  &
                                a_tke_tendf, &
                                a_mu_tendf,  &
                                a_moist_tendf,  &
!  NPan - 05/26/10 {
!  Uncomment the corresponding args when chem or tracer is needed.   
!                                a_chem_tendf,   &
                                a_scalar_tendf, &
                                a_tracer_tendf, &
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
                                                             a_ru_tendf, &
                                                             a_rv_tendf, &
                                                             a_rw_tendf, &
                                                             a_ph_tendf, &
                                                              a_t_tendf, &
                                                            a_tke_tendf

   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(INOUT) ::  a_mu_tendf

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_moist),INTENT(INOUT)::&
                                                          a_moist_tendf

!  NPan - 05/26/10 {
!  Uncomment the corresponding definations when chem is needed.   
!   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_chem ),INTENT(INOUT)::&
!                                                          a_chem_tendf
   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_tracer ),INTENT(INOUT)::&
                                                          a_tracer_tendf
   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_scalar ),INTENT(INOUT)::&
                                                          a_scalar_tendf
!  NPan } 

! LOCAL VARS

   INTEGER :: im, ic, is

!<DESCRIPTION>
!
! init_zero_tendency 
! sets tendency arrays to zero for all prognostic variables.
!
!</DESCRIPTION>


   CALL a_zero_tend ( a_ru_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL a_zero_tend ( a_rv_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL a_zero_tend ( a_rw_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL a_zero_tend ( a_ph_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL a_zero_tend ( a_t_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL a_zero_tend ( a_tke_tendf,            &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte     )

   CALL a_zero_tend2d ( a_mu_tendf,            &
                      ids, ide, jds, jde, kds, kds,    &
                      ims, ime, jms, jme, kms, kms,    &
                      its, ite, jts, jte, kts, kts     )

!   DO im=PARAM_FIRST_SCALAR,n_moist
   DO im=1,n_moist                      ! make sure first one is zero too
      CALL a_zero_tend ( a_moist_tendf(ims,kms,jms,im), &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte  )
   ENDDO

!  NPan - 05/26/10 {
!  Uncomment the corresponding statements when chem is needed.   
!!   DO ic=PARAM_FIRST_SCALAR,n_chem
!   DO ic=1,n_chem                       !! make sure first one is zero too
!      CALL a_zero_tend ( a_chem_tendf(ims,kms,jms,ic), &
!                         ids, ide, jds, jde, kds, kde, &
!                         ims, ime, jms, jme, kms, kme, &
!                         its, ite, jts, jte, kts, kte  )
!   ENDDO

!   DO ic=PARAM_FIRST_SCALAR,n_tracer
   DO ic=1,n_tracer                     !! make sure first one is zero too
      CALL a_zero_tend ( a_tracer_tendf(ims,kms,jms,ic), &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte  )
   ENDDO

!   DO ic=PARAM_FIRST_SCALAR,n_scalar
   DO ic=1,n_scalar                       ! make sure first one is zero too
      CALL a_zero_tend ( a_scalar_tendf(ims,kms,jms,ic), &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte  )
   ENDDO
!  NPan }

END SUBROUTINE a_init_zero_tendency

!-----------------------------------------------------------------------

! Revised by Ning Pan, 2010-08-03
!   SUBROUTINE a_bound_tke(tke,a_tke,tke_upper_bound,a_tke_upper_bound,ids,ide,jds, &
   SUBROUTINE a_bound_tke(tke,a_tke,tke_upper_bound,ids,ide,jds, &
   jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

!PART I: DECLARATION OF VARIABLES

   IMPLICIT NONE

   INTEGER :: K0_ADJ,K1_ADJ,K2_ADJ,K3_ADJ
   INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: tke,a_tke
! Revised by Ning Pan, 2010-08-03
!   REAL :: tke_upper_bound,a_tke_upper_bound
   REAL :: tke_upper_bound
   INTEGER :: i,k,j

   REAL :: a_Tmpv1,Tmpv001

!PART IV: REVERSE/BACKWARD ACCUMULATIONS

!LPB[0]
   DO j =min(jte, jde-1), jts, -1

!  DO k =kts, kte-1
!  DO i =its, min(ite, ide-1)
!  Tmpv001 =min(tke_upper_bound, max(tke(i,k,j), 0.))
!  tke(i,k,j) =Tmpv001

!  ENDDO
!  ENDDO

   DO k =kte-1, kts, -1
   DO i =min(ite, ide-1), its, -1
   a_Tmpv1 =a_tke(i,k,j)
   a_tke(i,k,j) =0.0
! Remarked by Ning Pan, 2010-08-03
!   a_tke_upper_bound =a_tke_upper_bound  +(1.0 -sign(1.0, tke_upper_bound -max(  &
!   tke(i,k,j), 0.)))*0.5*1.0*a_Tmpv1
   a_tke(i,k,j) =a_tke(i,k,j)  +(1.0 +sign(1.0, tke_upper_bound -max(tke(i,k,j)  &
   , 0.)))*0.5*(1.0 +(1.0)*sign(1.0, tke(i,k,j) -0.))*0.5*a_Tmpv1
   ENDDO
   ENDDO

   ENDDO

   END SUBROUTINE a_bound_tke


END MODULE a_module_em
