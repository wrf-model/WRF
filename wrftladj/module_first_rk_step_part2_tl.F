!WRF+/TL:MEDIATION_LAYER:SOLVER FOR TL
!Created by Ning Pan, 2010-08 

#define BENCH_START(A)
#define BENCH_END(A)

MODULE g_module_first_rk_step_part2

CONTAINS

  SUBROUTINE g_first_rk_step_part2 (   grid , config_flags              &
               , moist ,g_moist , moist_tend ,g_moist_tend               &
               , chem  ,g_chem  , chem_tend  ,g_chem_tend                &
               , tracer,g_tracer, tracer_tend,g_tracer_tend              &
               , scalar,g_scalar, scalar_tend,g_scalar_tend              &
               , fdda3d,g_fdda3d, fdda2d,     g_fdda2d                   &
               , ru_tendf,g_ru_tendf, rv_tendf,g_rv_tendf               &
               , rw_tendf,g_rw_tendf, t_tendf ,g_t_tendf                &
               , ph_tendf,g_ph_tendf, mu_tendf,g_mu_tendf               &
               , tke_tend,g_tke_tend           &
               , adapt_step_flag , curr_secs      &
               , psim ,g_psim , psih ,g_psih ,  &
                 gz1oz0 ,g_gz1oz0 , chklowq,g_chklowq         &
               , cu_act_flag , hol ,g_hol, th_phy,g_th_phy        &
               , pi_phy ,g_pi_phy, p_phy ,g_p_phy , t_phy ,g_t_phy  &
               , dz8w ,g_dz8w , p8w ,g_p8w , t8w ,g_t8w  &
               , nba_mij,g_nba_mij, n_nba_mij         &
               , nba_rij,g_nba_rij, n_nba_rij         &
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
    USE module_comm_dm, ONLY : halo_em_tke_c_sub,halo_em_tke_d_sub,halo_em_tke_e_sub            &
            ,halo_em_phys_pbl_sub,halo_em_phys_shcu_sub &
            ,halo_em_fdda_sub,halo_em_phys_diffusion_sub,halo_em_tke_3_sub &
            ,halo_em_tke_5_sub,halo_obs_nudge_sub,period_bdy_em_a1_sub,period_bdy_em_phy_bc_sub &
            ,period_bdy_em_fdda_bc_sub,period_bdy_em_chem_sub,halo_em_phys_cu_sub               &
            ,halo_em_tke_c_tl_sub,halo_em_phys_pbl_tl_sub                                       &
            ,halo_em_tke_d_tl_sub,halo_em_tke_e_tl_sub                                          &
            ,halo_em_phys_diffusion_tl_sub,halo_em_tke_3_tl_sub,halo_em_tke_5_tl_sub
#endif

    USE module_driver_constants
    USE g_module_diffusion_em, ONLY : g_phy_bc, g_cal_deform_and_div, &
            g_compute_diff_metrics, g_vertical_diffusion_2,              &
            g_horizontal_diffusion_2, g_calculate_km_kh, g_tke_rhs
    USE g_module_em, ONLY : g_calculate_phy_tend
    USE module_fddaobs_driver, ONLY : fddaobs_driver
    USE g_module_bc, ONLY : g_set_physical_bc3d, g_set_physical_bc2d
    USE g_module_physics_addtendc, ONLY : g_update_phy_ten

    USE g_module_sfs_driver
    USE module_stoch, ONLY : update_stoch_ten, perturb_physics_tend,RAND_PERT_UPDATE

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

    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_moist),INTENT(INOUT)   :: moist,g_moist
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_moist),INTENT(INOUT)   :: moist_tend,g_moist_tend
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_chem),INTENT(INOUT)   :: chem,g_chem
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_chem),INTENT(INOUT)   :: chem_tend,g_chem_tend
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer,g_tracer
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer_tend,g_tracer_tend
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar,g_scalar
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar_tend,g_scalar_tend
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_fdda3d),INTENT(INOUT)  :: fdda3d,g_fdda3d
    REAL,DIMENSION(ims:ime,1:1,jms:jme,num_fdda2d),INTENT(INOUT)      :: fdda2d,g_fdda2d
    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: psim,g_psim
    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: psih,g_psih
    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: gz1oz0,g_gz1oz0
    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: chklowq,g_chklowq
    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)         :: hol,g_hol
    LOGICAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT)      :: cu_act_flag

    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: th_phy,g_th_phy
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: pi_phy,g_pi_phy
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: p_phy,g_p_phy
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_phy,g_t_phy
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: dz8w,g_dz8w
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: p8w,g_p8w
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t8w,g_t8w

    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: ru_tendf,g_ru_tendf
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rv_tendf,g_rv_tendf
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: rw_tendf,g_rw_tendf
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: ph_tendf,g_ph_tendf
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: t_tendf,g_t_tendf
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: tke_tend,g_tke_tend

    REAL,DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: mu_tendf,g_mu_tendf

    INTEGER , INTENT(IN)                          ::  k_start, k_end

  INTEGER, INTENT(  IN ) :: n_nba_mij, n_nba_rij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_mij) &
  :: nba_mij,g_nba_mij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_rij) &
  :: nba_rij,g_nba_rij

! Local

    REAL, DIMENSION( ims:ime, jms:jme ) :: ht_loc,g_ht_loc
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

      IF ((grid%skebs_on==1).and.(grid%id .EQ. 1 )) then
          ! update and backtransform T
          CALL RAND_PERT_UPDATE(grid,'T',                                     &
                          grid%SPTFORCS,grid%SPTFORCC,                        &
                          grid%SPT_AMP,grid%ALPH_T,                           &
                          ips, ipe, jps, jpe, kps, kpe,                       &
                          ids, ide, jds, jde, kds, kde,                       &
                          ims, ime, jms, jme, kms, kme,                       &
                          k_start, k_end,                                     &
                          imsx,imex,jmsx,jmex,kmsx,kmex,                      &
                          ipsx,ipex,jpsx,jpex,kpsx,kpex,                      &
                          imsy,imey,jmsy,jmey,kmsy,kmey,                      &
                          ipsy,ipey,jpsy,jpey,kpsy,kpey,                      &
                          grid%num_stoch_levels,grid%num_stoch_levels,        &
                          grid%num_stoch_levels,grid%num_stoch_levels,        &
                          config_flags%restart, grid%iseedarr_skebs,          &
                          grid%DX,grid%DY,grid%skebs_vertstruc,               &
                          grid%rt_tendf_stoch,                                &
                          grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt,    &
                          grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPUV      )
          ! Update streamfunction, backtransform U
           CALL RAND_PERT_UPDATE(grid,'U',                                    &
                           grid%SPSTREAMFORCS,grid%SPSTREAMFORCC,             &
                           grid%SPSTREAM_AMP,grid%ALPH_PSI,                   &
                           ips, ipe, jps, jpe, kps, kpe,                      &
                           ids, ide, jds, jde, kds, kde,                      &
                           ims, ime, jms, jme, kms, kme,                      &
                           k_start, k_end,                                    &
                           imsx,imex,jmsx,jmex,kmsx,kmex,                     &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex,                     &
                           imsy,imey,jmsy,jmey,kmsy,kmey,                     &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey,                     &
                           grid% num_stoch_levels,grid% num_stoch_levels,     &
                           grid% num_stoch_levels,grid% num_stoch_levels,     &
                           config_flags%restart, grid%iseedarr_skebs,         &
                           grid%DX,grid%DY,grid%skebs_vertstruc,              &
                           grid%ru_tendf_stoch,                               &
                           grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt,   &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPUV     )
          ! Don't update streamfunction, backtransform V
           CALL RAND_PERT_UPDATE(grid,'V',                                    &
                           grid%SPSTREAMFORCS,grid%SPSTREAMFORCC,             &
                           grid%SPSTREAM_AMP,grid%ALPH_PSI,                   &
                           ips, ipe, jps, jpe, kps, kpe,                      &
                           ids, ide, jds, jde, kds, kde,                      &
                           ims, ime, jms, jme, kms, kme,                      &
                           k_start, k_end,                                    &
                           imsx,imex,jmsx,jmex,kmsx,kmex,                     &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex,                     &
                           imsy,imey,jmsy,jmey,kmsy,kmey,                     &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey,                     &
                           grid% num_stoch_levels,grid% num_stoch_levels,     &
                           grid% num_stoch_levels,grid% num_stoch_levels,     &
                           config_flags%restart, grid%iseedarr_skebs,         &
                           grid%DX,grid%DY,grid%skebs_vertstruc,              &
                           grid%rv_tendf_stoch,                               &
                           grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt,   &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT      )
       ENDIF !skebs_on 

     if ((grid%sppt_on==1).and.(grid%id .EQ. 1 )) then
          CALL RAND_PERT_UPDATE(grid,'T',                                     &
                          grid%SPPTFORCS,grid%SPPTFORCC,                      &
                          grid%SPPT_AMP,grid%ALPH_SPPT,                       &
                          ips, ipe, jps, jpe, kps, kpe,                       &
                          ids, ide, jds, jde, kds, kde,                       &
                          ims, ime, jms, jme, kms, kme,                       &
                          k_start, k_end,                                     &
                          imsx,imex,jmsx,jmex,kmsx,kmex,                      &
                          ipsx,ipex,jpsx,jpex,kpsx,kpex,                      &
                          imsy,imey,jmsy,jmey,kmsy,kmey,                      &
                          ipsy,ipey,jpsy,jpey,kpsy,kpey,                      &
                          grid%num_stoch_levels,grid%num_stoch_levels,        &
                          grid%num_stoch_levels,grid%num_stoch_levels,        &
                          config_flags%restart, grid%iseedarr_sppt,           &
                          grid%DX,grid%DY,grid%sppt_vertstruc,                &
                          grid%rstoch,                                        &
                          grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt,    &
                          grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT       )
       ENDIF !sppt_on

      if ((grid%rand_perturb_on==1).and.(grid%id .EQ. 1 )) then
           CALL RAND_PERT_UPDATE(grid,'T',                                     &
                           grid%SPFORCS,grid%SPFORCC,                          &
                           grid%SP_AMP,grid%ALPH_RAND,                         &
                           ips, ipe, jps, jpe, kps, kpe,                       &
                           ids, ide, jds, jde, kds, kde,                       &
                           ims, ime, jms, jme, kms, kme,                       &
                           k_start, k_end,                                     &
                           imsx,imex,jmsx,jmex,kmsx,kmex,                      &
                           ipsx,ipex,jpsx,jpex,kpsx,kpex,                      &
                           imsy,imey,jmsy,jmey,kmsy,kmey,                      &
                           ipsy,ipey,jpsy,jpey,kpsy,kpey,                      &
                           grid%num_stoch_levels,grid%num_stoch_levels,        &
                           grid%num_stoch_levels,grid%num_stoch_levels,        &
                           config_flags%restart, grid%iseedarr_rand_pert,      &
                           grid%DX,grid%DY,grid%rand_pert_vertstruc,           &
                           grid%RAND_PERT,                                     &
                           grid%stddev_cutoff_sppt,grid%gridpt_stddev_sppt,    &
                           grid%VERTSTRUCC,grid%VERTSTRUCS,grid%VERTAMPT       )
       ENDIF !rand_perturb_on

!!!!! REPLACE WITH g_??? WHEN CONSTRUCTING TL OF PHYSICS. Xing Zhang

BENCH_START(tl_cal_phy_tend)
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , grid%num_tiles

        CALL wrf_debug ( 200 , ' call g_calculate_phy_tend' )
        CALL g_calculate_phy_tend (config_flags,grid%mut,grid%g_mut,grid%muu,grid%g_muu, &
                     grid%muv,grid%g_muv,pi_phy,                       &
                     grid%rthraten, grid%g_rthraten,                   &
                     grid%rublten, grid%g_rublten,                     &
                     grid%rvblten, grid%g_rvblten,                     &
                     grid%rthblten, grid%g_rthblten,                   &
                     grid%rqvblten, grid%g_rqvblten,                   &
                     grid%rqcblten, grid%g_rqcblten,                   &
                     grid%rqiblten, grid%g_rqiblten,                   &
                     grid%rucuten, grid%g_rucuten,                     &
                     grid%rvcuten, grid%g_rvcuten,                     &
                     grid%rthcuten, grid%g_rthcuten,                   &
                     grid%rqvcuten, grid%g_rqvcuten,                   &
                     grid%rqccuten, grid%g_rqccuten,                   &
                     grid%rqrcuten, grid%g_rqrcuten,                   &
                     grid%rqicuten, grid%g_rqicuten,                   &
                     grid%rqscuten, grid%g_rqscuten,                   &
                     grid%rushten, grid%g_rushten,                     &
                     grid%rvshten, grid%g_rvshten,                     &
                     grid%rthshten, grid%g_rthshten,                   &
                     grid%rqvshten, grid%g_rqvshten,                   &
                     grid%rqcshten, grid%g_rqcshten,                   &
                     grid%rqrshten, grid%g_rqrshten,                   &
                     grid%rqishten, grid%g_rqishten,                   &
                     grid%rqsshten, grid%g_rqsshten,                   &
                     grid%rqgshten, grid%g_rqgshten,                   &
                     grid%RUNDGDTEN, grid%g_RUNDGDTEN,                 &
                     grid%RVNDGDTEN, grid%g_RVNDGDTEN,                 &
                     grid%RTHNDGDTEN, grid%g_RTHNDGDTEN,               &
                     grid%RQVNDGDTEN, grid%g_RQVNDGDTEN,               &
                     grid%RMUNDGDTEN,                                  &
                     ids,ide, jds,jde, kds,kde,                        &
                     ims,ime, jms,jme, kms,kme,                        &
                     grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
                     grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
                     k_start    , min(k_end,kde-1)                     )

      ENDDO
      !$OMP END PARALLEL DO
BENCH_END(tl_cal_phy_tend)

       IF(config_flags%diff_opt .eq. 2 .OR. config_flags%diff_opt .eq. 1) THEN

BENCH_START(tl_comp_diff_metrics_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
           CALL wrf_debug ( 200 , ' call g_compute_diff_metrics ' )
           CALL g_compute_diff_metrics ( config_flags, grid%ph_2,grid%g_ph_2, &
                grid%phb, grid%z,grid%g_z, grid%rdz,grid%g_rdz, grid%rdzw,grid%g_rdzw, &
                grid%zx,grid%g_zx, grid%zy,grid%g_zy, grid%rdx, grid%rdy, &
                ids, ide, jds, jde, kds, kde,          &
                ims, ime, jms, jme, kms, kme,          &
                grid%i_start(ij), grid%i_end(ij),      &
                grid%j_start(ij), grid%j_end(ij),      &
                k_start    , k_end                    )
         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(tl_comp_diff_metrics_tim)

#ifdef DM_PARALLEL
#  include "HALO_EM_TKE_C_TL.inc"
#  include "PERIOD_BDY_EM_A1.inc"
#endif

BENCH_START(tl_tke_diff_bc_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call tl of bc for diffusion_metrics ' )
           CALL g_set_physical_bc3d( grid%rdzw ,grid%g_rdzw, 'w', config_flags, &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij),   &
                                   k_start    , k_end                 )
           CALL g_set_physical_bc3d( grid%rdz ,grid%g_rdz, 'w', config_flags, &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij),   &
                                   k_start    , k_end                 )
           CALL g_set_physical_bc3d( grid%z ,grid%g_z, 'w', config_flags, &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij),   &
                                   k_start    , k_end                 )
           CALL g_set_physical_bc3d( grid%zx ,grid%g_zx, 'w', config_flags, &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij),   &
                                   k_start    , k_end                 )
           CALL g_set_physical_bc3d( grid%zy ,grid%g_zy, 'w', config_flags, &
                                   ids, ide, jds, jde, kds, kde,       &
                                   ims, ime, jms, jme, kms, kme,       &
                                   ips, ipe, jps, jpe, kps, kpe,       &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij),   &
                                   k_start    , k_end                 )
           CALL g_set_physical_bc2d( grid%ustm,grid%g_ustm, 't', config_flags, &
                                   ids, ide, jds, jde,                 &
                                   ims, ime, jms, jme,                 &
                                   ips, ipe, jps, jpe,                 &
                                   grid%i_start(ij), grid%i_end(ij),   &
                                   grid%j_start(ij), grid%j_end(ij)   )

         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(tl_tke_diff_bc_tim)

BENCH_START(tl_deform_div_tim)

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call g_cal_deform_and_div' )
           CALL g_cal_deform_and_div ( config_flags,grid%u_2,grid%g_u_2, &
                grid%v_2,grid%g_v_2,grid%w_2,grid%g_w_2,grid%div,grid%g_div,  &
                grid%defor11,grid%g_defor11,grid%defor22,grid%g_defor22, &
                grid%defor33,grid%g_defor33,grid%defor12,grid%g_defor12, &
                grid%defor13,grid%g_defor13,grid%defor23,grid%g_defor23, &
                nba_rij,g_nba_rij, n_nba_rij,                   &
                grid%u_base, grid%v_base,grid%msfux,grid%msfuy,    &
                grid%msfvx,grid%msfvy,grid%msftx,grid%msfty,       &
                grid%rdx, grid%rdy, grid%dn, grid%dnw, grid%rdz,grid%g_rdz,   &
                grid%rdzw,grid%g_rdzw,grid%fnm,grid%fnp,grid%cf1,grid%cf2,    &
                grid%cf3,grid%zx,grid%g_zx,grid%zy,grid%g_zy,                 &
                ids, ide, jds, jde, kds, kde,        &
                ims, ime, jms, jme, kms, kme,        &
                grid%i_start(ij), grid%i_end(ij),    &
                grid%j_start(ij), grid%j_end(ij),    &
                k_start    , k_end                  )
         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(tl_deform_div_tim)

#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_D_TL.inc"
#endif

BENCH_START(tl_calc_tke_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call g_calculate_km_kh' )
           CALL g_calculate_km_kh( config_flags,grid%dt,grid%dampcoef,grid%zdamp, &
                config_flags%damp_opt,                                 &
                grid%xkmh,grid%g_xkmh,grid%xkmv,grid%g_xkmv,grid%xkhh,grid%g_xkhh, &
                grid%xkhv,grid%g_xkhv,grid%bn2,grid%g_bn2,             &
                grid%khdif,grid%kvdif,grid%div,grid%g_div,             &
                grid%defor11,grid%g_defor11,grid%defor22,grid%g_defor22, &
                grid%defor33,grid%g_defor33,grid%defor12,grid%g_defor12, &
                grid%defor13,grid%g_defor13,grid%defor23,grid%g_defor23, &
                grid%tke_2,grid%g_tke_2,p8w,g_p8w,t8w,g_t8w,th_phy,g_th_phy, &
                t_phy,g_t_phy,p_phy,g_p_phy,moist,g_moist,grid%dn,grid%dnw,  &
                grid%dx,grid%dy,grid%rdz,grid%g_rdz,grid%rdzw,grid%g_rdzw,            &
                config_flags%mix_isotropic,num_moist,                  &
                grid%cf1, grid%cf2, grid%cf3, grid%warm_rain,          &
                grid%mix_upper_bound,                                  &
                grid%msftx, grid%msfty,                                &
                grid%zx, grid%g_zx, grid%zy, grid%g_zy,                &
                ids,ide, jds,jde, kds,kde,                             &
                ims,ime, jms,jme, kms,kme,                             &
                grid%i_start(ij), grid%i_end(ij),                      &
                grid%j_start(ij), grid%j_end(ij),                      &
                k_start    , k_end                          )
         ENDDO
       !$OMP END PARALLEL DO
BENCH_END(tl_calc_tke_tim)

#ifdef DM_PARALLEL
#     include "HALO_EM_TKE_E_TL.inc"
#endif

       ENDIF

#ifdef DM_PARALLEL
#      include "PERIOD_BDY_EM_PHY_BC.inc"
       IF ( config_flags%grid_fdda .eq. 1) THEN
#      include "PERIOD_BDY_EM_FDDA_BC.inc"
       ENDIF
#      include "PERIOD_BDY_EM_CHEM.inc"
#endif

BENCH_START(tl_phy_bc_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

         CALL wrf_debug ( 200 , ' call g_phy_bc' )
         CALL g_phy_bc (config_flags,grid%div,grid%g_div,grid%defor11,grid%g_defor11, &
                      grid%defor22,grid%g_defor22,grid%defor33,grid%g_defor33, &
                      grid%defor12,grid%g_defor12,grid%defor13,grid%g_defor13, &
                      grid%defor23,grid%g_defor23,                 &
                      grid%xkmh,grid%g_xkmh,grid%xkmv,grid%g_xkmv, &
                      grid%xkhh,grid%g_xkhh,grid%xkhv,grid%g_xkhv, &
                      grid%tke_2,grid%g_tke_2,                     &
                      grid%rublten,grid%g_rublten, grid%rvblten,grid%g_rvblten, &
                      grid%rucuten,grid%g_rucuten, grid%rvcuten,grid%g_rvcuten, & 
                      grid%rushten,grid%g_rushten, grid%rvshten,grid%g_rvshten, &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      ips, ipe, jps, jpe, kps, kpe,                &
                      grid%i_start(ij), grid%i_end(ij),            &
                      grid%j_start(ij), grid%j_end(ij),            &
                      k_start    , k_end                           )
       ENDDO
       !$OMP END PARALLEL DO
BENCH_END(tl_phy_bc_tim)

IF ( ( config_flags%sfs_opt .GT. 0 ) .AND. ( config_flags%diff_opt .eq. 2 ) ) THEN
 CALL g_sfs_driver( grid, config_flags,     &
                  nba_mij,g_nba_mij, n_nba_mij,     & 
                  nba_rij,g_nba_rij, n_nba_rij      ) 

ENDIF

#ifdef DM_PARALLEL
!-----------------------------------------------------------------------
!
! MPP for some physics tendency, km, kh, deformation, and divergence
!
!                                                         * * * * * * *
!                                            * * * * *    * * * * * * *
!               *                     *      * * * * *    * * * * * * *
!             * + *      * + *        +      * * + * *    * * * + * * *
!               *                     *      * * * * *    * * * * * * *
!                                            * * * * *    * * * * * * *
!                                                         * * * * * * *
!
! (for PBL)
! rublten                  x
! rvblten                             x
!
! (for Cumulus)
! rucuten                  x
! rvcuten                             x
!
! (for Shallow Cumulus)
! rushten                  x
! rvshten                             x
!
! (for FDDA)
! rundgdten     x
! rvndgdten     x
!
! (for TKE3)
! tke_2                                          x               
! (for TKE5)
! tke_2                                                         x
!
! (for diff_opt >= 1)
! defor11                  x
! defor22                             x
! defor12       x
! defor13                  x
! defor23                             x
! div           x
! xkmv          x
! xkmh          x
! xkhv          x
! xkhh          x
! tke           x
!
!-----------------------------------------------------------------------
       IF ( config_flags%bl_pbl_physics .ge. 1 ) THEN
#      include "HALO_EM_PHYS_PBL_TL.inc"
       ENDIF
       IF ( config_flags%shcu_physics .gt. 1 ) THEN
#      include "HALO_EM_PHYS_SHCU.inc"
       ENDIF
       IF ( config_flags%cu_physics == SASSCHEME      .or.   &
            config_flags%cu_physics == TIEDTKESCHEME  .or.   &
            config_flags%cu_physics == NTIEDTKESCHEME .or.   &
            config_flags%cu_physics == CAMZMSCHEME    .or.   &
            config_flags%cu_physics == NSASSCHEME ) THEN
#      include "HALO_EM_PHYS_CU.inc"
       ENDIF
       IF ( config_flags%grid_fdda .ge. 1) THEN
#      include "HALO_EM_FDDA.inc"
       ENDIF
       IF ( config_flags%diff_opt .ge. 1 ) THEN
#      include "HALO_EM_PHYS_DIFFUSION_TL.inc"
       ENDIF

       IF      ( config_flags%h_mom_adv_order <= 4 ) THEN
#       include "HALO_EM_TKE_3_TL.inc"
       ELSE IF ( config_flags%h_mom_adv_order <= 6 ) THEN
#       include "HALO_EM_TKE_5_TL.inc"
       ELSE
         WRITE(wrf_err_message,*)'solve_em: invalid h_mom_adv_order = ',config_flags%h_mom_adv_order
         CALL wrf_error_fatal(TRIM(wrf_err_message))
       ENDIF
#endif

BENCH_START(tl_update_phy_ten_tim)
       !$OMP PARALLEL DO   &
       !$OMP PRIVATE ( ij )

       DO ij = 1 , grid%num_tiles

         CALL wrf_debug ( 200 , ' call g_update_phy_ten' )
#if ( WRF_DFI_RADAR == 1 )
         if (config_flags%cu_physics .gt. 0) then
           i_start = grid%i_start(ij)
           i_end   = min( grid%i_end(ij),ide-1 )
           j_start = grid%j_start(ij)
           j_end   = min( grid%j_end(ij),jde-1 )
           if (grid%dfi_stage == DFI_FWD ) &
                 CALL wrf_debug ( 200 , ' Zero out cu_physics' )
           DO j = j_start, j_end
           DO k = k_start, min( k_end,kde-1 ) - 1
           DO i = i_start, i_end
             if (grid%dfi_stage ==DFI_FWD  &
             .and. grid%dfi_tten_rad(i,k,j) >= 1.0e-7 .and.  &
                   grid%dfi_tten_rad(i,k,j) <= 10.) then

                grid%g_rthcuten(i,k,j) = 0.0
                grid%rthcuten(i,k,j) = 0.0

             endif
           ENDDO
           ENDDO
           ENDDO
         ENDIF
#endif
         CALL g_update_phy_ten(ph_tendf,t_tendf, g_t_tendf, ru_tendf, g_ru_tendf,      &
                           rv_tendf,g_rv_tendf,moist_tend,g_moist_tend,    &
                           scalar_tend, mu_tendf,                           &
                           grid%rthraten,grid%rthblten,grid%g_rthblten,       &
                           grid%rthcuten,grid%g_rthcuten,grid%rthshten, &
                           grid%rublten,grid%g_rublten,grid%rucuten,grid%rushten,          &
                           grid%rvblten,grid%g_rvblten,grid%rvcuten,grid%rvshten,          &
                           grid%rqvblten,grid%g_rqvblten,grid%rqcblten,grid%rqiblten,       &
                           grid%rqvcuten,grid%g_rqvcuten,grid%rqccuten,grid%rqrcuten,       &
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

       END DO
       !$OMP END PARALLEL DO
BENCH_END(tl_update_phy_ten_tim)

#ifdef PLANET
       IF ( (config_flags%damp_opt == 101) .OR. &
            (config_flags%damp_opt == 103)      ) THEN
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles
!!!!! Need tl of damptop, but cannot find declaration of damptop. Ning Pan, 2010-08-11
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

       IF( config_flags%diff_opt .eq. 2 .and. config_flags%km_opt .eq. 2 ) THEN

BENCH_START(tl_tke_rhs_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           CALL g_tke_rhs  ( tke_tend,g_tke_tend,grid%bn2,grid%g_bn2, &
                config_flags,grid%defor11,grid%g_defor11,grid%defor22,grid%g_defor22, &
                grid%defor33,grid%g_defor33,grid%defor12,grid%g_defor12, &
                grid%defor13,grid%g_defor13,grid%defor23,grid%g_defor23, &
                grid%u_2,grid%g_u_2,grid%v_2,grid%g_v_2,grid%w_2,grid%g_w_2, &
                grid%div,grid%g_div,grid%tke_2,grid%g_tke_2,grid%mut,grid%g_mut, &
                th_phy,g_th_phy,p_phy,g_p_phy,p8w,g_p8w,t8w,g_t8w,   &
                grid%z,grid%g_z,grid%fnm,                    & 
                grid%fnp,grid%cf1,grid%cf2,grid%cf3,         &     
                grid%msftx,grid%msfty,grid%xkmh,grid%g_xkmh, &
                grid%xkmv,grid%g_xkmv,grid%xkhv,grid%g_xkhv,grid%rdx,grid%rdy,   &
                grid%dx,grid%dy,grid%dt,grid%zx,grid%g_zx,grid%zy,grid%g_zy,     &
                grid%rdz,grid%g_rdz,grid%rdzw,grid%g_rdzw,grid%dn,               &
                grid%dnw,config_flags%mix_isotropic,         &
                grid%hfx,grid%g_hfx, grid%qfx,grid%g_qfx,    &
                moist(ims,kms,jms,P_QV),g_moist(ims,kms,jms,P_QV), &
                grid%ustm,grid%g_ustm, grid%rho,grid%g_rho,                  &
                ids, ide, jds, jde, kds, kde,                &
                ims, ime, jms, jme, kms, kme,                &
                grid%i_start(ij), grid%i_end(ij),            &
                grid%j_start(ij), grid%j_end(ij),            &
                k_start    , k_end                           )

         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(tl_tke_rhs_tim)

       ENDIF

       IF(config_flags%diff_opt .eq. 2) THEN

         IF (config_flags%bl_pbl_physics .eq. 0) THEN

BENCH_START(tl_vert_diff_tim)
           !$OMP PARALLEL DO   &
           !$OMP PRIVATE ( ij )
           DO ij = 1 , grid%num_tiles

             CALL wrf_debug ( 200 , ' call g_vertical_diffusion_2 ' )
             CALL g_vertical_diffusion_2(ru_tendf,g_ru_tendf, &
                  rv_tendf,g_rv_tendf, rw_tendf,g_rw_tendf,   &
                  t_tendf,g_t_tendf, tke_tend,g_tke_tend,     &
                  moist_tend,g_moist_tend,num_moist,             &
                  chem_tend,g_chem_tend,num_chem,                &
                  scalar_tend,g_scalar_tend,num_scalar,          &
                  tracer_tend,g_tracer_tend,num_tracer,          &
                  grid%u_2,grid%g_u_2,grid%v_2,grid%g_v_2,          &
                  grid%t_2,grid%g_t_2,grid%u_base,grid%v_base,grid%t_base,grid%qv_base,&
                  grid%mut,grid%g_mut,grid%tke_2,grid%g_tke_2,config_flags, &
                  grid%defor13,grid%g_defor13,grid%defor23,grid%g_defor23,  &
                  grid%defor33,grid%g_defor33,               &
                  nba_mij,g_nba_mij, num_nba_mij,         &
                  grid%div,grid%g_div, moist,g_moist, chem,g_chem, scalar,g_scalar, tracer,g_tracer, &
                  grid%xkmv,grid%g_xkmv, grid%xkhv,grid%g_xkhv, config_flags%km_opt,                 &
                  grid%fnm, grid%fnp, grid%dn, grid%dnw, grid%rdz,grid%g_rdz, grid%rdzw,grid%g_rdzw, &
                  grid%hfx,grid%g_hfx, grid%qfx,grid%g_qfx, grid%ustm,grid%g_ustm, grid%rho,grid%g_rho,     &
                  ids, ide, jds, jde, kds, kde,              &
                  ims, ime, jms, jme, kms, kme,              &
                  grid%i_start(ij), grid%i_end(ij),          &
                  grid%j_start(ij), grid%j_end(ij),          &
                  k_start, k_end                             )

           ENDDO
           !$OMP END PARALLEL DO
BENCH_END(tl_vert_diff_tim)

         ENDIF

BENCH_START(tl_hor_diff_tim)
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )
         DO ij = 1 , grid%num_tiles

           CALL wrf_debug ( 200 , ' call g_horizontal_diffusion_2' )
           CALL g_horizontal_diffusion_2( t_tendf,g_t_tendf, ru_tendf,g_ru_tendf, &
                rv_tendf,g_rv_tendf, rw_tendf,g_rw_tendf, &
                tke_tend,g_tke_tend,                     &
                moist_tend,g_moist_tend, num_moist,      &
                chem_tend,g_chem_tend, num_chem,         &
                scalar_tend,g_scalar_tend, num_scalar,   &
                tracer_tend,g_tracer_tend, num_tracer,   &
                grid%t_2,grid%g_t_2, th_phy,g_th_phy,    &
                grid%mut,grid%g_mut, grid%tke_2,grid%g_tke_2, config_flags, &
                grid%defor11,grid%g_defor11, grid%defor22,grid%g_defor22,   &
                grid%defor12,grid%g_defor12,  &
                grid%defor13,grid%g_defor13, grid%defor23,grid%g_defor23,   &
                nba_mij,g_nba_mij, num_nba_mij,         &
                grid%div,grid%g_div,                       &
                moist,g_moist, chem,g_chem, scalar,g_scalar,tracer,g_tracer, &
                grid%msfux,grid%msfuy, grid%msfvx,grid%msfvy, grid%msftx,  &
                grid%msfty, grid%xkmh,grid%g_xkmh, grid%xkhh,grid%g_xkhh, config_flags%km_opt, &
                grid%rdx, grid%rdy, grid%rdz,grid%g_rdz, grid%rdzw,grid%g_rdzw,                &
                grid%fnm, grid%fnp, grid%cf1, grid%cf2, grid%cf3,          &
                grid%zx,grid%g_zx, grid%zy,grid%g_zy, grid%dn, grid%dnw,                       &
                ids, ide, jds, jde, kds, kde,          &
                ims, ime, jms, jme, kms, kme,          &
                grid%i_start(ij), grid%i_end(ij),      &
                grid%j_start(ij), grid%j_end(ij),      &
                k_start    , k_end                    )
         ENDDO
         !$OMP END PARALLEL DO
BENCH_END(tl_hor_diff_tim)
       ENDIF

       IF ( grid%obs_nudge_opt .EQ. 1 .AND. grid%xtime <= grid%fdda_end ) THEN
# ifdef DM_PARALLEL
#       include "HALO_OBS_NUDGE.inc"
#endif
!***********************************************************************
! This section for obs nudging
         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij )

         DO ij = 1 , grid%num_tiles

!!!!! Need to code g_fddaobs_driver for full wrf+. Ning Pan
           CALL fddaobs_driver (grid%grid_id, model_config_rec%grid_id, &
                   model_config_rec%parent_id, config_flags%restart,    &
                   config_flags,                                        &
                   grid%obs_nudge_opt,                                  &
                   grid%obs_ipf_errob,                                  &
                   grid%obs_ipf_nudob,                                  &
                   grid%fdda_start,                                     &
                   grid%fdda_end,                                       &
                   grid%obs_nudge_wind,                                 &
                   grid%obs_nudge_temp,                                 &
                   grid%obs_nudge_mois,                                 &
                   grid%obs_nudge_pstr,                                 &
                   grid%obs_coef_wind,                                  &
                   grid%obs_coef_temp,                                  &
                   grid%obs_coef_mois,                                  &
                   grid%obs_coef_pstr,                                  &             
                   grid%obs_rinxy,                                      &
                   grid%obs_rinsig,                                     &
                   grid%obs_npfi,                                       &
                   grid%obs_ionf,                                       &
                   grid%obs_prt_max,                                    &
                   grid%obs_prt_freq,                                   &
                   grid%obs_idynin,                                     &
                   grid%obs_dtramp,                                     &
                   grid%parent_grid_ratio,                              &
                   grid%max_dom, grid%itimestep,                        &
                   grid%xtime,                                          &
                   grid%dt, grid%gmt, grid%julday, grid%fdob,           &
                   grid%max_obs,                                        &
                   model_config_rec%nobs_ndg_vars,                      &
                   model_config_rec%nobs_err_flds,                      &
                   grid%fdob%nstat, grid%fdob%varobs, grid%fdob%errf,   &
                   grid%dx, grid%KPBL,grid%HT,                          &
                   grid%mut, grid%muu, grid%muv, grid%c1h, grid%c2h,    &
                   grid%msftx, grid%msfty, grid%msfux, grid%msfuy, grid%msfvx, grid%msfvy, &
                   p_phy, t_tendf, t0,                                  &
                   grid%u_2, grid%v_2, grid%t_2,                        &
                   moist(ims,kms,jms,P_QV),                             &
                   grid%pb, grid%p_top, grid%p, grid%phb, grid%ph_2,    &
                   grid%uratx, grid%vratx, grid%tratx,                  &
                   ru_tendf, rv_tendf,                                  &
                   moist_tend(ims,kms,jms,P_QV), grid%obs_savwt,        &
                   grid%regime, grid%pblh, grid%z_at_w, grid%z,         &
                   ids,ide, jds,jde, kds,kde,                           &
                   ims,ime, jms,jme, kms,kme,                           &
                   grid%i_start(ij), min(grid%i_end(ij),ide-1),         &
                   grid%j_start(ij), min(grid%j_end(ij),jde-1),         &
                   k_start    , min(k_end,kde-1)                     )
 
         ENDDO
         !$OMP END PARALLEL DO
       ENDIF  ! obs_nudge_opt .eq. 1
! 
!***********************************************************************

  END SUBROUTINE g_first_rk_step_part2

END MODULE g_module_first_rk_step_part2

