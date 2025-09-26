!WRF+/AD:MEDIATION_LAYER:SOLVER FOR AD
!Created by Ning Pan, 2010-08 

#define BENCH_START(A)
#define BENCH_END(A)

MODULE a_module_first_rk_step_part2

CONTAINS

  SUBROUTINE a_first_rk_step_part2 (   grid , config_flags               &
               , moist ,a_moist , moist_tend ,a_moist_tend             &
               , chem  ,a_chem  , chem_tend  ,a_chem_tend              &
               , tracer,a_tracer, tracer_tend,a_tracer_tend            &
               , scalar,a_scalar, scalar_tend,a_scalar_tend            &
               , fdda3d,a_fdda3d, fdda2d,     a_fdda2d                 &
               , ru_tendf,a_ru_tendf, rv_tendf,a_rv_tendf              &
               , rw_tendf,a_rw_tendf, t_tendf ,a_t_tendf               &
               , ph_tendf,a_ph_tendf, mu_tendf,a_mu_tendf              &
               , tke_tend,a_tke_tend                                     &
               , adapt_step_flag , curr_secs                               &
               , psim ,a_psim , psih ,a_psih ,        &
                 gz1oz0 ,a_gz1oz0 , chklowq,a_chklowq                &
               , cu_act_flag , hol ,a_hol, th_phy,a_th_phy             &
               , pi_phy ,a_pi_phy, p_phy ,a_p_phy , t_phy ,a_t_phy   &
               , dz8w ,a_dz8w , p8w ,a_p8w , t8w ,a_t8w              &
               , nba_mij,a_nba_mij, n_nba_mij                            &
               , nba_rij,a_nba_rij, n_nba_rij                            &
               , ids, ide, jds, jde, kds, kde     &
               , ims, ime, jms, jme, kms, kme     &
               , ips, ipe, jps, jpe, kps, kpe     &
               , imsx,imex,jmsx,jmex,kmsx,kmex    &
               , ipsx,ipex,jpsx,jpex,kpsx,kpex    &
               , imsy,imey,jmsy,jmey,kmsy,kmey    &
               , ipsy,ipey,jpsy,jpey,kpsy,kpey    &
               , k_start , k_end                  &
              )

    USE module_state_description
    USE module_model_constants
    USE module_domain, ONLY : domain
    USE module_configure, ONLY : grid_config_rec_type, model_config_rec
#ifdef DM_PARALLEL
    USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y, local_communicator_periodic, wrf_dm_maxval, wrf_err_message, local_communicator_x, local_communicator_y
    USE module_comm_dm, ONLY : halo_em_tke_c_ad_sub,halo_em_tke_d_ad_sub,halo_em_tke_e_ad_sub    &
            ,halo_em_phys_pbl_ad_sub &
            ,halo_em_phys_diffusion_ad_sub,halo_em_tke_3_ad_sub &
            ,halo_em_tke_5_ad_sub
#endif

    USE module_driver_constants
    USE a_module_diffusion_em, ONLY : a_phy_bc, a_cal_deform_and_div, &
            a_compute_diff_metrics, a_vertical_diffusion_2,              &
            a_horizontal_diffusion_2, a_calculate_km_kh, a_tke_rhs
!   USE module_em, ONLY : calculate_phy_tend
    USE a_module_em, ONLY : a_calculate_phy_tend
!   USE module_fddaobs_driver, ONLY : fddaobs_driver
!   USE module_bc, ONLY : set_physical_bc3d, set_physical_bc2d
    USE a_module_bc, ONLY : a_set_physical_bc3d, a_set_physical_bc2d
!   USE module_physics_addtendc, ONLY : update_phy_ten
    USE a_module_physics_addtendc, ONLY : a_update_phy_ten

!   USE module_sfs_driver
!   USE module_stoch, ONLY : update_stoch_ten, perturb_physics_tend,RAND_PERT_UPDATE

    USE a_module_sfs_driver

    IMPLICIT NONE

    TYPE ( domain ), INTENT(INOUT) :: grid
    TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags

    INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           ips, ipe, jps, jpe, kps, kpe,     &
                           imsx,imex,jmsx,jmex,kmsx,kmex,    &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex,    &
                           imsy,imey,jmsy,jmey,kmsy,kmey,    &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey


    LOGICAL ,INTENT(IN)                        :: adapt_step_flag
    REAL, INTENT(IN)                           :: curr_secs

    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_moist),INTENT(INOUT)   :: moist,a_moist
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_moist),INTENT(INOUT)   :: moist_tend,a_moist_tend
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_chem),INTENT(INOUT)   :: chem,a_chem
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_chem),INTENT(INOUT)   :: chem_tend,a_chem_tend
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer,a_tracer
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer_tend,a_tracer_tend
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar,a_scalar
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar_tend,a_scalar_tend
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_fdda3d),INTENT(INOUT)  :: fdda3d,a_fdda3d
    REAL,DIMENSION(ims:ime,1:1,jms:jme,num_fdda2d),INTENT(INOUT)      :: fdda2d,a_fdda2d
    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: psim,a_psim
    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: psih,a_psih
    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: gz1oz0,a_gz1oz0
    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: chklowq,a_chklowq
    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: hol,a_hol
    LOGICAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)      :: cu_act_flag

    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: th_phy,a_th_phy
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: pi_phy,a_pi_phy
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: p_phy,a_p_phy
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_phy,a_t_phy
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: dz8w,a_dz8w
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: p8w,a_p8w
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t8w,a_t8w

    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: ru_tendf,a_ru_tendf
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rv_tendf,a_rv_tendf
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rw_tendf,a_rw_tendf
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: ph_tendf,a_ph_tendf
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_tendf,a_t_tendf
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: tke_tend,a_tke_tend

    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: mu_tendf,a_mu_tendf

    INTEGER , INTENT(IN)                          ::  k_start, k_end

  INTEGER, INTENT(  IN ) :: n_nba_mij, n_nba_rij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_mij) &
  :: nba_mij,a_nba_mij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_rij) &
  :: nba_rij,a_nba_rij

! Local

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_mij) &
  :: nba_mij_bk

    REAL, DIMENSION( ims:ime, jms:jme ) :: ht_loc
    REAL :: scale_factor
    INTEGER, DIMENSION( ims:ime, jms:jme ) :: shadowmask 
    INTEGER                             :: ij
    INTEGER  num_roof_layers
    INTEGER  num_wall_layers
    INTEGER  num_road_layers
    INTEGER  iswater
    INTEGER  rk_step 
#if ( WRF_DFI_RADAR == 1 )
    INTEGER  i_start,i_end,j_start,j_end,i,j,k
#endif


    rk_step = 1

!***********************************************************************

       IF(config_flags%diff_opt .eq. 2) THEN

BENCH_START(adj_hor_diff_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = grid%num_tiles,1,-1

           CALL wrf_debug ( 200 , ' call a_horizontal_diffusion_2' )
           CALL a_horizontal_diffusion_2( t_tendf,a_t_tendf, ru_tendf,a_ru_tendf, &
                rv_tendf,a_rv_tendf, rw_tendf,a_rw_tendf, &
                tke_tend,a_tke_tend,                     &
                moist_tend,a_moist_tend, num_moist,      &
                chem_tend,a_chem_tend, num_chem,         &
                scalar_tend,a_scalar_tend, num_scalar,   &
                tracer_tend,a_tracer_tend, num_tracer,   &
                grid%t_2,grid%a_t_2, th_phy,a_th_phy,    &
                grid%mut,grid%a_mut, grid%tke_2,grid%a_tke_2, config_flags, &
                grid%defor11,grid%a_defor11, grid%defor22,grid%a_defor22,   &
                grid%defor12,grid%a_defor12,  &
                grid%defor13,grid%a_defor13, grid%defor23,grid%a_defor23,   &
                nba_mij,a_nba_mij, num_nba_mij,          &
                grid%div,grid%a_div,                       &
                moist,a_moist, chem,a_chem, scalar,a_scalar,tracer,a_tracer, &
                grid%msfux,grid%msfuy, grid%msfvx,grid%msfvy, grid%msftx,  &
                grid%msfty, grid%xkmh,grid%a_xkmh, grid%xkhh,grid%a_xkhh, config_flags%km_opt, &
                grid%rdx, grid%rdy, grid%rdz,grid%a_rdz, grid%rdzw,grid%a_rdzw,                &
                grid%fnm, grid%fnp, grid%cf1, grid%cf2, grid%cf3,          &
                grid%zx,grid%a_zx, grid%zy,grid%a_zy, grid%dn, grid%dnw,                       &
                grid%rho, grid%a_rho,                  &
                ids, ide, jds, jde, kds, kde,          &
                ims, ime, jms, jme, kms, kme,          &
                grid%i_start(ij), grid%i_end(ij),      &
                grid%j_start(ij), grid%j_end(ij),      &
                k_start    , k_end                    )
         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(adj_hor_diff_tim)

         IF (config_flags%bl_pbl_physics .eq. 0) THEN

BENCH_START(adj_vert_diff_tim)
           nba_mij = nba_mij_bk

           !$OMP PARALLEL DO   &
           !$OMP PRIVATE ( ij )
           DO ij = grid%num_tiles,1,-1

             CALL wrf_debug ( 200 , ' call a_vertical_diffusion_2 ' )
             CALL a_vertical_diffusion_2(ru_tendf,a_ru_tendf, &
                  rv_tendf,a_rv_tendf, rw_tendf,a_rw_tendf,   &
                  t_tendf,a_t_tendf, tke_tend,a_tke_tend,     &
                  moist_tend,a_moist_tend,num_moist,             &
                  chem_tend,a_chem_tend,num_chem,                &
                  scalar_tend,a_scalar_tend,num_scalar,          &
                  tracer_tend,a_tracer_tend,num_tracer,          &
                  grid%u_2,grid%a_u_2,grid%v_2,grid%a_v_2,          &
                  grid%t_2,grid%a_t_2,grid%u_base,grid%v_base,grid%t_base,grid%qv_base,&
                  grid%mut,grid%a_mut,grid%tke_2,grid%a_tke_2,config_flags, &
                  grid%defor13,grid%a_defor13,grid%defor23,grid%a_defor23,  &
                  grid%defor33,grid%a_defor33,               &
                  nba_mij,a_nba_mij, num_nba_mij,          &
                  grid%div,grid%a_div, moist,a_moist, chem,a_chem, scalar,a_scalar, tracer,a_tracer, &
                  grid%xkmv,grid%a_xkmv, grid%xkhv,grid%a_xkhv, config_flags%km_opt,                 &
                  grid%fnm, grid%fnp, grid%dn, grid%dnw, grid%rdz,grid%a_rdz, grid%rdzw,grid%a_rdzw, &
                  grid%hfx,grid%a_hfx, grid%qfx,grid%a_qfx, grid%ustm,grid%a_ustm, grid%rho,grid%a_rho,     &
                  ids, ide, jds, jde, kds, kde,              &
                  ims, ime, jms, jme, kms, kme,              &
                  grid%i_start(ij), grid%i_end(ij),          &
                  grid%j_start(ij), grid%j_end(ij),          &
                  k_start, k_end                             )

           ENDDO
           !$OMP END PARALLEL DO
BENCH_END(adj_vert_diff_tim)

         ENDIF

       ENDIF

       IF( config_flags%diff_opt .eq. 2 .and. config_flags%km_opt .eq. 2 ) THEN

BENCH_START(adj_tke_rhs_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = grid%num_tiles,1,-1

           CALL a_tke_rhs  ( tke_tend,a_tke_tend,grid%bn2,grid%a_bn2, &
                config_flags,grid%defor11,grid%a_defor11,grid%defor22,grid%a_defor22, &
                grid%defor33,grid%a_defor33,grid%defor12,grid%a_defor12, &
                grid%defor13,grid%a_defor13,grid%defor23,grid%a_defor23, &
                grid%u_2,grid%a_u_2,grid%v_2,grid%a_v_2,grid%w_2,grid%a_w_2, &
                grid%div,grid%a_div,grid%tke_2,grid%a_tke_2,grid%mut,grid%a_mut, &
                grid%c1h, grid%c2h,                                  &
                th_phy,a_th_phy,p_phy,a_p_phy,p8w,a_p8w,t8w,a_t8w,   &
                grid%z,grid%a_z,grid%fnm,                    & 
                grid%fnp,grid%cf1,grid%cf2,grid%cf3,         &     
                grid%msftx,grid%msfty,grid%xkmh,grid%a_xkmh, &
                grid%xkmv,grid%a_xkmv,grid%xkhv,grid%a_xkhv,grid%rdx,grid%rdy,   &
                grid%dx,grid%dy,grid%dt,grid%zx,grid%a_zx,grid%zy,grid%a_zy,     &
                grid%rdz,grid%a_rdz,grid%rdzw,grid%a_rdzw,grid%dn,               &
                grid%dnw,config_flags%mix_isotropic,         &
                grid%hfx,grid%a_hfx, grid%qfx,grid%a_qfx,    &
                moist(ims,kms,jms,P_QV),a_moist(ims,kms,jms,P_QV), &
                grid%ustm,grid%a_ustm, grid%rho,grid%a_rho,                  &
                ids, ide, jds, jde, kds, kde,                &
                ims, ime, jms, jme, kms, kme,                &
                grid%i_start(ij), grid%i_end(ij),            &
                grid%j_start(ij), grid%j_end(ij),            &
                k_start    , k_end                           )

         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(adj_tke_rhs_tim)

       ENDIF

#ifdef PLANET

       IF ( (config_flags%damp_opt == 101) .OR. &
            (config_flags%damp_opt == 103)      ) THEN
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
!!!!! Need adj of damptop, but cannot find declaration of damptop. Ning Pan, 2010-08-11
           CALL damptop( grid%u_2, grid%v_2, grid%t_2, &
                         grid%mut, grid%muu, grid%muv, &
                         pi_phy,                                &
                         t_tendf, ru_tendf, rv_tendf, P2SI,     &
                         ids, ide, jds, jde, kds, kde,          &
                         ims, ime, jms, jme, kms, kme,          &
                         grid%i_start(ij), grid%i_end(ij),      &
                         grid%j_start(ij), grid%j_end(ij),      &
                         k_start, k_end                         )
         END DO
         !$OMP END PARALLEL DO
       END IF
#endif

BENCH_START(adj_update_phy_ten_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = grid%num_tiles,1,-1

         CALL wrf_debug ( 200 , ' call a_update_phy_ten' )
         CALL a_update_phy_ten(ph_tendf,t_tendf, a_t_tendf, ru_tendf, a_ru_tendf,      &
                           rv_tendf,a_rv_tendf, moist_tend ,a_moist_tend,    &
                           scalar_tend, mu_tendf,                           &
                           grid%rthraten,grid%rthblten,grid%a_rthblten,     &
                           grid%rthcuten,grid%a_rthcuten,grid%rthshten,     &
                           grid%rublten,grid%a_rublten,                     &
                           grid%rucuten,grid%rushten,                       &
                           grid%rvblten,grid%a_rvblten,                     &
                           grid%rvcuten,grid%rvshten,                       &
                           grid%rqvblten,grid%a_rqvblten,                   &
                           grid%rqcblten,grid%rqiblten,                     &
                           grid%rqvcuten,grid%a_rqvcuten,grid%rqccuten,grid%rqrcuten,       &
                           grid%rqicuten,grid%rqscuten,                     &
                           grid%rqvshten,grid%rqcshten,grid%rqrshten,       &
                           grid%rqishten,grid%rqsshten,grid%rqgshten,       &
                           grid%RUNDGDTEN,                                  &
                           grid%RVNDGDTEN,grid%RTHNDGDTEN,grid%RPHNDGDTEN,  &
                           grid%RQVNDGDTEN,grid%RMUNDGDTEN,                 &
                           grid%rthfrten,grid%rqvfrten,                     &  ! fire
                           num_moist,num_scalar,config_flags,rk_step,       &
                           grid%adv_moist_cond,                             &
                           ids, ide, jds, jde, kds, kde,                    &
                           ims, ime, jms, jme, kms, kme,                    &
                           grid%i_start(ij), grid%i_end(ij),                &
                           grid%j_start(ij), grid%j_end(ij),                &
                           k_start, k_end                               )

#if ( WRF_DFI_RADAR == 1 )
         if (config_flags%cu_physics .gt. 0) then
           i_start = grid%i_start(ij)
           i_end   = min( grid%i_end(ij),ide-1 )
           j_start = grid%j_start(ij)
           j_end   = min( grid%j_end(ij),jde-1 )
           if (grid%dfi_stage == DFI_FWD ) &
                 CALL wrf_debug ( 200 , ' adj of Zero out cu_physics' )
           DO j = j_start, j_end
           DO k = k_start, min( k_end,kde-1 ) - 1
           DO i = i_start, i_end
             if (grid%dfi_stage ==DFI_FWD  &
             .and. grid%dfi_tten_rad(i,k,j) >= 1.0e-7 .and.  &
                   grid%dfi_tten_rad(i,k,j) <= 10.) then

                grid%a_rthcuten(i,k,j) = 0.0

             endif
           ENDDO
           ENDDO
           ENDDO
         ENDIF
#endif
       END DO
       !$OMP END PARALLEL DO
BENCH_END(adj_update_phy_ten_tim)

#ifdef DM_PARALLEL
       IF ( config_flags%bl_pbl_physics .ge. 1 ) THEN
#      include "HALO_EM_PHYS_PBL_AD.inc"
       ENDIF
       IF ( config_flags%diff_opt .ge. 1 ) THEN
#      include "HALO_EM_PHYS_DIFFUSION_AD.inc"
       ENDIF

       IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#       include "HALO_EM_TKE_3_AD.inc"
       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#       include "HALO_EM_TKE_5_AD.inc"
       ELSE
         WRITE(wrf_err_message,*)'solve_em_ad: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
         CALL wrf_error_fatal(TRIM(wrf_err_message))
       ENDIF
#endif


IF ( ( config_flags%sfs_opt .GT. 0 ) .AND. ( config_flags%diff_opt .eq. 2 ) ) THEN
 CALL a_sfs_driver( grid, config_flags,     &
                  nba_mij,a_nba_mij, n_nba_mij,     & 
                  nba_rij,a_nba_rij, n_nba_rij      ) 

ENDIF

BENCH_START(adj_phy_bc_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = grid%num_tiles,1,-1

         CALL wrf_debug ( 200 , ' call a_phy_bc' )
         CALL a_phy_bc (config_flags,grid%div,grid%a_div,grid%defor11,grid%a_defor11,&
                      grid%defor22,grid%a_defor22,grid%defor33,grid%a_defor33, &
                      grid%defor12,grid%a_defor12,grid%defor13,grid%a_defor13, &
                      grid%defor23,grid%a_defor23,                 &
                      grid%xkmh,grid%a_xkmh,grid%xkmv,grid%a_xkmv, &
                      grid%xkhh,grid%a_xkhh,grid%xkhv,grid%a_xkhv, &
                      grid%tke_2,grid%a_tke_2,                     &
                      grid%rublten,grid%a_rublten, grid%rvblten,grid%a_rvblten, &
                      grid%rucuten,grid%a_rucuten, grid%rvcuten,grid%a_rvcuten, &
                      grid%rushten,grid%a_rushten, grid%rvshten,grid%a_rvshten, &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      ips, ipe, jps, jpe, kps, kpe,                &
                      grid%i_start(ij), grid%i_end(ij),            &
                      grid%j_start(ij), grid%j_end(ij),            &
                      k_start    , k_end                           )
       ENDDO
       !$OMP END PARALLEL DO
BENCH_END(adj_phy_bc_tim)

       IF(config_flags%diff_opt .eq. 2 .OR. config_flags%diff_opt .eq. 1) THEN

#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_E_AD.inc"
#endif

BENCH_START(adj_calc_tke_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = grid%num_tiles,1,-1

           CALL wrf_debug ( 200 , ' call a_calculate_km_kh' )
           CALL a_calculate_km_kh( config_flags,grid%dt,grid%dampcoef,grid%zdamp, &
                config_flags%damp_opt,                                 &
                grid%xkmh,grid%a_xkmh,grid%xkmv,grid%a_xkmv,grid%xkhh,grid%a_xkhh, &
                grid%xkhv,grid%a_xkhv,grid%bn2,grid%a_bn2,             &
                grid%khdif,grid%kvdif,grid%div,grid%a_div,             &
                grid%defor11,grid%a_defor11,grid%defor22,grid%a_defor22, &
                grid%defor33,grid%a_defor33,grid%defor12,grid%a_defor12, &
                grid%defor13,grid%a_defor13,grid%defor23,grid%a_defor23, &
                grid%tke_2,grid%a_tke_2,p8w,a_p8w,t8w,a_t8w,th_phy,a_th_phy, &
                t_phy,a_t_phy,p_phy,a_p_phy,moist,a_moist,grid%dn,grid%dnw,  &
                grid%dx,grid%dy,grid%rdz,grid%a_rdz,grid%rdzw,grid%a_rdzw,            &
                config_flags%mix_isotropic,num_moist,                  &
                grid%cf1, grid%cf2, grid%cf3, grid%warm_rain,          &
                grid%mix_upper_bound,                                  &
                grid%msftx, grid%msfty,                                &
                grid%zx, grid%a_zx, grid%zy, grid%a_zy,                &
                ids,ide, jds,jde, kds,kde,                             &
                ims,ime, jms,jme, kms,kme,                             &
                grid%i_start(ij), grid%i_end(ij),                      &
                grid%j_start(ij), grid%j_end(ij),                      &
                k_start    , k_end                          )
         ENDDO
       !$OMP END PARALLEL DO
BENCH_END(adj_calc_tke_tim)

#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_D_AD.inc"
#endif

BENCH_START(adj_deform_div_tim)

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         DO ij = grid%num_tiles,1,-1

           CALL wrf_debug ( 200 , ' call a_cal_deform_and_div' )
           CALL a_cal_deform_and_div ( config_flags,grid%u_2,grid%a_u_2, &
                grid%v_2,grid%a_v_2,grid%w_2,grid%a_w_2,grid%div,grid%a_div,  &
                grid%defor11,grid%a_defor11,grid%defor22,grid%a_defor22, &
                grid%defor33,grid%a_defor33,grid%defor12,grid%a_defor12, &
                grid%defor13,grid%a_defor13,grid%defor23,grid%a_defor23, &
                nba_rij,a_nba_rij, n_nba_rij,                    &
                grid%u_base, grid%v_base,grid%msfux,grid%msfuy,    &
                grid%msfvx,grid%msfvy,grid%msftx,grid%msfty,       &
                grid%rdx, grid%rdy, grid%dn, grid%dnw, grid%rdz,grid%a_rdz,   &
                grid%rdzw,grid%a_rdzw,grid%fnm,grid%fnp,grid%cf1,grid%cf2,    &
                grid%cf3,grid%zx,grid%a_zx,grid%zy,grid%a_zy,                 &
                ids, ide, jds, jde, kds, kde,        &
                ims, ime, jms, jme, kms, kme,        &
                grid%i_start(ij), grid%i_end(ij),    &
                grid%j_start(ij), grid%j_end(ij),    &
                k_start    , k_end                  )
         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(adj_deform_div_tim)

BENCH_START(adj_tke_diff_bc_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         DO ij = grid%num_tiles,1,-1

           CALL wrf_debug ( 200 , ' call adj of bc for diffusion_metrics ' )
           CALL a_set_physical_bc2d( grid%a_ustm, 't', config_flags, &
                                   ids, ide, jds, jde,                 &
                                   ims, ime, jms, jme,                 &
                                   ips, ipe, jps, jpe,                 &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij)   )
           CALL a_set_physical_bc3d( grid%a_zy , 'w', config_flags,  &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start    , k_end                 )
           CALL a_set_physical_bc3d( grid%a_zx , 'w', config_flags,  &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start    , k_end                 )
           CALL a_set_physical_bc3d( grid%a_z , 'w', config_flags,   &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start    , k_end                 )
           CALL a_set_physical_bc3d( grid%a_rdz , 'w', config_flags, &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij), &
                                   grid%j_start(ij), grid%j_end(ij), &
                                   k_start    , k_end                 )
           CALL a_set_physical_bc3d( grid%a_rdzw , 'w', config_flags,&
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij),   &
                                   k_start    , k_end                 )

         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(adj_tke_diff_bc_tim)

#ifdef DM_PARALLEL
#  include "HALO_EM_TKE_C_AD.inc"
#endif

BENCH_START(adj_comp_diff_metrics_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = grid%num_tiles,1,-1
           CALL wrf_debug ( 200 , ' call a_compute_diff_metrics ' )
           CALL a_compute_diff_metrics ( config_flags, grid%ph_2,grid%a_ph_2, &
                grid%phb, grid%z,grid%a_z, grid%rdz,grid%a_rdz, grid%rdzw,grid%a_rdzw, &
                grid%zx,grid%a_zx, grid%zy,grid%a_zy, grid%rdx, grid%rdy, &
                ids, ide, jds, jde, kds, kde,          &
                ims, ime, jms, jme, kms, kme,          &
                grid%i_start(ij), grid%i_end(ij),      &
                grid%j_start(ij), grid%j_end(ij),      &
                k_start    , k_end                    )
         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(adj_comp_diff_metrics_tim)

       ENDIF

BENCH_START(adj_cal_phy_tend)
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = grid%num_tiles,1,-1

        CALL wrf_debug ( 200 , ' call a_calculate_phy_tend' )
        CALL a_calculate_phy_tend (config_flags,grid%mut,grid%a_mut,grid%muu,grid%a_muu, &
                     grid%muv,grid%a_muv,pi_phy,                       &
                     grid%rthraten, grid%a_rthraten,                   &
                     grid%rublten, grid%a_rublten,                     &
                     grid%rvblten, grid%a_rvblten,                     &
                     grid%rthblten, grid%a_rthblten,                   &
                     grid%rqvblten, grid%a_rqvblten,                   &
                     grid%rqcblten, grid%a_rqcblten,                   &
                     grid%rqiblten, grid%a_rqiblten,                   &
                     grid%rucuten, grid%a_rucuten,                     &
                     grid%rvcuten, grid%a_rvcuten,                     &
                     grid%rthcuten, grid%a_rthcuten,                   &
                     grid%rqvcuten, grid%a_rqvcuten,                   &
                     grid%rqccuten, grid%a_rqccuten,                   &
                     grid%rqrcuten, grid%a_rqrcuten,                   &
                     grid%rqicuten, grid%a_rqicuten,                   &
                     grid%rqscuten, grid%a_rqscuten,                   &
                     grid%rushten, grid%a_rushten,                     &
                     grid%rvshten, grid%a_rvshten,                     &
                     grid%rthshten, grid%a_rthshten,                   &
                     grid%rqvshten, grid%a_rqvshten,                   &
                     grid%rqcshten, grid%a_rqcshten,                   &
                     grid%rqrshten, grid%a_rqrshten,                   &
                     grid%rqishten, grid%a_rqishten,                   &
                     grid%rqsshten, grid%a_rqsshten,                   &
                     grid%rqgshten, grid%a_rqgshten,                   &
                     grid%RUNDGDTEN, grid%a_RUNDGDTEN,                 &
                     grid%RVNDGDTEN, grid%a_RVNDGDTEN,                 &
                     grid%RTHNDGDTEN, grid%a_RTHNDGDTEN,               &
                     grid%RQVNDGDTEN, grid%a_RQVNDGDTEN,               &
                     grid%RMUNDGDTEN,                                  &
                     ids,ide, jds,jde, kds,kde,                        &
                     ims,ime, jms,jme, kms,kme,                        &
                     grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
                     grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
                     k_start    , min(k_end,kde-1)                     )

      ENDDO
      !$OMP END PARALLEL DO
BENCH_END(adj_cal_phy_tend)

  END SUBROUTINE a_first_rk_step_part2

END MODULE a_module_first_rk_step_part2

