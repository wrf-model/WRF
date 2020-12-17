!WRF:MEDIATION_LAYER:adtl_grid_utilities
!

MODULE module_adtl_grid_utilities
USE module_domain, ONLY : domain, get_ijk_from_grid
USE module_configure, ONLY : grid_config_rec_type
USE module_state_description, ONLY : PARAM_FIRST_SCALAR, num_moist, num_scalar, num_tracer
USE module_tiles, ONLY : set_tiles
USE module_driver_constants, ONLY: max_sens

REAL, ALLOCATABLE :: s_u_2(:,:,:), s_v_2(:,:,:), s_w_2(:,:,:)
REAL, ALLOCATABLE :: s_ph_2(:,:,:), s_t_2(:,:,:), s_mu_2(:,:)
REAL, ALLOCATABLE :: s_moist(:,:,:,:), s_scalar(:,:,:,:), s_tracer(:,:,:,:)
REAL, ALLOCATABLE :: s_tke_2(:,:,:)

REAL, ALLOCATABLE :: s_p(:,:,:), s_al(:,:,:), s_h_diabatic(:,:,:)
REAL, ALLOCATABLE :: s_qv_diabatic(:,:,:), s_qc_diabatic(:,:,:)
REAL, ALLOCATABLE :: s_ustm(:,:), s_hfx(:,:), s_qfx(:,:)

REAL, ALLOCATABLE :: s_dtaux3d(:,:,:), s_dtauy3d(:,:,:)
REAL, ALLOCATABLE :: s_dusfcg(:,:), s_dvsfcg(:,:)

REAL, ALLOCATABLE :: s_u_bxs(:,:,:), s_u_bxe(:,:,:), s_u_bys(:,:,:), s_u_bye(:,:,:)
REAL, ALLOCATABLE :: s_u_btxs(:,:,:), s_u_btxe(:,:,:), s_u_btys(:,:,:), s_u_btye(:,:,:)
REAL, ALLOCATABLE :: s_v_bxs(:,:,:), s_v_bxe(:,:,:), s_v_bys(:,:,:), s_v_bye(:,:,:)
REAL, ALLOCATABLE :: s_v_btxs(:,:,:), s_v_btxe(:,:,:), s_v_btys(:,:,:), s_v_btye(:,:,:)
REAL, ALLOCATABLE :: s_w_bxs(:,:,:), s_w_bxe(:,:,:), s_w_bys(:,:,:), s_w_bye(:,:,:)
REAL, ALLOCATABLE :: s_w_btxs(:,:,:), s_w_btxe(:,:,:), s_w_btys(:,:,:), s_w_btye(:,:,:)
REAL, ALLOCATABLE :: s_ph_bxs(:,:,:), s_ph_bxe(:,:,:), s_ph_bys(:,:,:), s_ph_bye(:,:,:)
REAL, ALLOCATABLE :: s_ph_btxs(:,:,:), s_ph_btxe(:,:,:), s_ph_btys(:,:,:), s_ph_btye(:,:,:)
REAL, ALLOCATABLE :: s_t_bxs(:,:,:), s_t_bxe(:,:,:), s_t_bys(:,:,:), s_t_bye(:,:,:)
REAL, ALLOCATABLE :: s_t_btxs(:,:,:), s_t_btxe(:,:,:), s_t_btys(:,:,:), s_t_btye(:,:,:)
REAL, ALLOCATABLE :: s_mu_bxs(:,:,:), s_mu_bxe(:,:,:), s_mu_bys(:,:,:), s_mu_bye(:,:,:)
REAL, ALLOCATABLE :: s_mu_btxs(:,:,:), s_mu_btxe(:,:,:), s_mu_btys(:,:,:), s_mu_btye(:,:,:)
REAL, ALLOCATABLE :: s_moist_bxs(:,:,:,:), s_moist_bxe(:,:,:,:), s_moist_bys(:,:,:,:), s_moist_bye(:,:,:,:)
REAL, ALLOCATABLE :: s_moist_btxs(:,:,:,:), s_moist_btxe(:,:,:,:), s_moist_btys(:,:,:,:), s_moist_btye(:,:,:,:)
REAL, ALLOCATABLE :: s_scalar_bxs(:,:,:,:), s_scalar_bxe(:,:,:,:), s_scalar_bys(:,:,:,:), s_scalar_bye(:,:,:,:)
REAL, ALLOCATABLE :: s_scalar_btxs(:,:,:,:), s_scalar_btxe(:,:,:,:), s_scalar_btys(:,:,:,:), s_scalar_btye(:,:,:,:)
REAL, ALLOCATABLE :: s_tracer_bxs(:,:,:,:), s_tracer_bxe(:,:,:,:), s_tracer_bys(:,:,:,:), s_tracer_bye(:,:,:,:)
REAL, ALLOCATABLE :: s_tracer_btxs(:,:,:,:), s_tracer_btxe(:,:,:,:), s_tracer_btys(:,:,:,:), s_tracer_btye(:,:,:,:)

REAL, ALLOCATABLE :: b_u_2(:,:,:), b_v_2(:,:,:), b_w_2(:,:,:)
REAL, ALLOCATABLE :: b_ph_2(:,:,:), b_t_2(:,:,:), b_mu_2(:,:)
REAL, ALLOCATABLE :: b_moist(:,:,:,:), b_scalar(:,:,:,:), b_tracer(:,:,:,:)
REAL, ALLOCATABLE :: b_tke_2(:,:,:)

REAL, ALLOCATABLE :: b_p(:,:,:), b_al(:,:,:), b_h_diabatic(:,:,:)
REAL, ALLOCATABLE :: b_qv_diabatic(:,:,:), b_qc_diabatic(:,:,:)
REAL, ALLOCATABLE :: b_ustm(:,:), b_hfx(:,:), b_qfx(:,:)

REAL, ALLOCATABLE :: b_dtaux3d(:,:,:), b_dtauy3d(:,:,:)
REAL, ALLOCATABLE :: b_dusfcg(:,:), b_dvsfcg(:,:)

REAL, ALLOCATABLE :: b_u_bxs(:,:,:), b_u_bxe(:,:,:), b_u_bys(:,:,:), b_u_bye(:,:,:)
REAL, ALLOCATABLE :: b_u_btxs(:,:,:), b_u_btxe(:,:,:), b_u_btys(:,:,:), b_u_btye(:,:,:)
REAL, ALLOCATABLE :: b_v_bxs(:,:,:), b_v_bxe(:,:,:), b_v_bys(:,:,:), b_v_bye(:,:,:)
REAL, ALLOCATABLE :: b_v_btxs(:,:,:), b_v_btxe(:,:,:), b_v_btys(:,:,:), b_v_btye(:,:,:)
REAL, ALLOCATABLE :: b_w_bxs(:,:,:), b_w_bxe(:,:,:), b_w_bys(:,:,:), b_w_bye(:,:,:)
REAL, ALLOCATABLE :: b_w_btxs(:,:,:), b_w_btxe(:,:,:), b_w_btys(:,:,:), b_w_btye(:,:,:)
REAL, ALLOCATABLE :: b_ph_bxs(:,:,:), b_ph_bxe(:,:,:), b_ph_bys(:,:,:), b_ph_bye(:,:,:)
REAL, ALLOCATABLE :: b_ph_btxs(:,:,:), b_ph_btxe(:,:,:), b_ph_btys(:,:,:), b_ph_btye(:,:,:)
REAL, ALLOCATABLE :: b_t_bxs(:,:,:), b_t_bxe(:,:,:), b_t_bys(:,:,:), b_t_bye(:,:,:)
REAL, ALLOCATABLE :: b_t_btxs(:,:,:), b_t_btxe(:,:,:), b_t_btys(:,:,:), b_t_btye(:,:,:)
REAL, ALLOCATABLE :: b_mu_bxs(:,:,:), b_mu_bxe(:,:,:), b_mu_bys(:,:,:), b_mu_bye(:,:,:)
REAL, ALLOCATABLE :: b_mu_btxs(:,:,:), b_mu_btxe(:,:,:), b_mu_btys(:,:,:), b_mu_btye(:,:,:)
REAL, ALLOCATABLE :: b_moist_bxs(:,:,:,:), b_moist_bxe(:,:,:,:), b_moist_bys(:,:,:,:), b_moist_bye(:,:,:,:)
REAL, ALLOCATABLE :: b_moist_btxs(:,:,:,:), b_moist_btxe(:,:,:,:), b_moist_btys(:,:,:,:), b_moist_btye(:,:,:,:)
REAL, ALLOCATABLE :: b_scalar_bxs(:,:,:,:), b_scalar_bxe(:,:,:,:), b_scalar_bys(:,:,:,:), b_scalar_bye(:,:,:,:)
REAL, ALLOCATABLE :: b_scalar_btxs(:,:,:,:), b_scalar_btxe(:,:,:,:), b_scalar_btys(:,:,:,:), b_scalar_btye(:,:,:,:)
REAL, ALLOCATABLE :: b_tracer_bxs(:,:,:,:), b_tracer_bxe(:,:,:,:), b_tracer_bys(:,:,:,:), b_tracer_bye(:,:,:,:)
REAL, ALLOCATABLE :: b_tracer_btxs(:,:,:,:), b_tracer_btxe(:,:,:,:), b_tracer_btys(:,:,:,:), b_tracer_btye(:,:,:,:)

REAL, ALLOCATABLE :: b_rainnc(:,:), b_rainncv(:,:)
REAL, ALLOCATABLE :: b_rainc(:,:), b_raincv(:,:)

INTEGER :: sm31, em31, sm32, em32, sm33, em33, spec_bdy_width, kts, kte
INTEGER :: gwd_opt

INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe


CONTAINS

  SUBROUTINE init_domain_size ( grid, config_flags )
   TYPE (domain), INTENT(INOUT) :: grid
   TYPE (grid_config_rec_type), INTENT(IN) :: config_flags
   INTEGER :: ierr

!  Obtain dimension information stored in the grid data structure.

   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe     )

!  Compute these starting and stopping locations for each tile and number of tiles.
!  See: http://www.mmm.ucar.edu/wrf/WG2/topics/settiles
   CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

   sm31 = grid%sm31
   em31 = grid%em31
   sm32 = grid%sm32
   em32 = grid%em32
   sm33 = grid%sm33
   em33 = grid%em33

   kts = kps
   kte = kpe

   CALL nl_get_spec_bdy_width( 1, spec_bdy_width )
   CALL nl_get_gwd_opt( 1, gwd_opt )

  END SUBROUTINE init_domain_size

  SUBROUTINE allocate_grid ( )

   ALLOCATE ( s_scalar(sm31:em31, sm32:em32, sm33:em33, num_scalar), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_scalar(sm31:em31, sm32:em32, sm33:em33, num_scalar). ')
   ENDIF
   ALLOCATE ( s_tke_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_tke_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF

   ALLOCATE ( s_p(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_p(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_al(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_al(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_h_diabatic(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_h_diabatic(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_qv_diabatic(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_qv_diabatic(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_qc_diabatic(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_qc_diabatic(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF

   ALLOCATE ( s_ustm(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ustm(sm31:em31, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_hfx(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_hfx(sm31:em31, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_qfx(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_qfx(sm31:em31, sm33:em33). ')
   ENDIF

   IF (gwd_opt .EQ. 1) THEN

   ALLOCATE ( s_dtaux3d(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_dtaux3d(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_dtauy3d(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_dtauy3d(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_dusfcg(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_dusfcg(sm31:em31, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_dvsfcg(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_dvsfcg(sm31:em31, sm33:em33). ')
   ENDIF

   ENDIF

   !  Scalar Boundary
   ALLOCATE(s_scalar_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_scalar_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(s_scalar_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_scalar_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(s_scalar_bys(sm31:em31,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_scalar_bys(sm31:em31,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(s_scalar_bye(sm31:em31,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_scalar_bye(sm31:em31,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(s_scalar_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_scalar_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(s_scalar_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_scalar_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(s_scalar_btys(sm31:em31,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_scalar_btys(sm31:em31,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(s_scalar_btye(sm31:em31,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_scalar_btye(sm31:em31,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF

   ALLOCATE ( s_u_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_v_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_v_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_w_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_w_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_ph_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ph_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_t_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_t_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_mu_2(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_mu_2(sm31:em31, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_moist(sm31:em31, sm32:em32, sm33:em33, num_moist), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_moist(sm31:em31, sm32:em32, sm33:em33, num_moist). ')
   ENDIF
   ALLOCATE ( s_tracer(sm31:em31, sm32:em32, sm33:em33, num_tracer), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_tracer(sm31:em31, sm32:em32, sm33:em33, num_tracer). ')
   ENDIF
   !  U Boundary
   ALLOCATE(s_u_bxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_bxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_u_bxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_bxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_u_bys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_bys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_u_bye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_bye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_u_btxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_btxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_u_btxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_btxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_u_btys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_btys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_u_btye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_btye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF

   !  V Boundary
   ALLOCATE(s_v_bxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_bxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_v_bxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_bxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_v_bys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_bys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_v_bye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_bye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_v_btxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_btxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_v_btxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_btxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_v_btys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_btys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_v_btye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_btye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF

   !  T Boundary
   ALLOCATE(s_t_bxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_t_bxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_t_bxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_t_bxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_t_bys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_t_bys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_t_bye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_t_bye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_t_btxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_t_btxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_t_btxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_t_btxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_t_btys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_t_btys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_t_btye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_t_btye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF

   !  W Boundary
   ALLOCATE(s_w_bxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_w_bxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_w_bxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_w_bxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_w_bys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_w_bys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_w_bye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_w_bye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_w_btxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_w_btxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_w_btxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_w_btxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_w_btys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_w_btys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_w_btye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_w_btye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF

   !  PH Boundary
   ALLOCATE(s_ph_bxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ph_bxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_ph_bxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ph_bxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_ph_bys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ph_bys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_ph_bye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ph_bye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_ph_btxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ph_btxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_ph_btxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ph_btxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_ph_btys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ph_btys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_ph_btye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ph_btye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF

   !  MU Boundary
   ALLOCATE(s_mu_bxs(sm33:em33,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_mu_bxs(sm33:em33,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_mu_bxe(sm33:em33,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_mu_bxe(sm33:em33,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_mu_bys(sm31:em31,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_mu_bys(sm31:em31,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_mu_bye(sm31:em31,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_mu_bye(sm31:em31,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_mu_btxs(sm33:em33,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_mu_btxs(sm33:em33,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_mu_btxe(sm33:em33,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_mu_btxe(sm33:em33,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_mu_btys(sm31:em31,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_mu_btys(sm31:em31,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(s_mu_btye(sm31:em31,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_mu_btye(sm31:em31,1,spec_bdy_width). ')
   ENDIF

   !  Moist Boundary
   ALLOCATE(s_moist_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_moist_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(s_moist_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_moist_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(s_moist_bys(sm31:em31,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_moist_bys(sm31:em31,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(s_moist_bye(sm31:em31,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_moist_bye(sm31:em31,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(s_moist_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_moist_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(s_moist_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_moist_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(s_moist_btys(sm31:em31,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_moist_btys(sm31:em31,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(s_moist_btye(sm31:em31,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_moist_btye(sm31:em31,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF

   !  Tracer Boundary
   ALLOCATE(s_tracer_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_tracer_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(s_tracer_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_tracer_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(s_tracer_bys(sm31:em31,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_tracer_bys(sm31:em31,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(s_tracer_bye(sm31:em31,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_tracer_bye(sm31:em31,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(s_tracer_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_tracer_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(s_tracer_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_tracer_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(s_tracer_btys(sm31:em31,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_tracer_btys(sm31:em31,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(s_tracer_btye(sm31:em31,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_tracer_btye(sm31:em31,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF

   ALLOCATE ( b_scalar(sm31:em31, sm32:em32, sm33:em33, num_scalar), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_scalar(sm31:em31, sm32:em32, sm33:em33, num_scalar). ')
   ENDIF
   ALLOCATE ( b_tke_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_tke_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF

   ALLOCATE ( b_p(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_p(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_al(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_al(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_h_diabatic(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_h_diabatic(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_qv_diabatic(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_qv_diabatic(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_qc_diabatic(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_qc_diabatic(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF

   ALLOCATE ( b_ustm(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ustm(sm31:em31, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_hfx(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_hfx(sm31:em31, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_qfx(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_qfx(sm31:em31, sm33:em33). ')
   ENDIF

   IF (gwd_opt .EQ. 1) THEN

   ALLOCATE ( b_dtaux3d(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_dtaux3d(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_dtauy3d(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_dtauy3d(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_dusfcg(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_dusfcg(sm31:em31, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_dvsfcg(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_dvsfcg(sm31:em31, sm33:em33). ')
   ENDIF

   ENDIF

   !  Scalar Boundary
   ALLOCATE(b_scalar_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_scalar_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(b_scalar_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_scalar_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(b_scalar_bys(sm31:em31,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_scalar_bys(sm31:em31,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(b_scalar_bye(sm31:em31,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_scalar_bye(sm31:em31,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(b_scalar_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_scalar_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(b_scalar_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_scalar_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(b_scalar_btys(sm31:em31,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_scalar_btys(sm31:em31,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF
   ALLOCATE(b_scalar_btye(sm31:em31,sm32:em32,spec_bdy_width,num_scalar),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_scalar_btye(sm31:em31,sm32:em32,spec_bdy_width,num_scalar). ')
   ENDIF

   ALLOCATE ( b_u_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_v_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_v_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_w_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_w_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_ph_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ph_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_t_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_t_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_mu_2(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_mu_2(sm31:em31, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_moist(sm31:em31, sm32:em32, sm33:em33, num_moist), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_moist(sm31:em31, sm32:em32, sm33:em33, num_moist). ')
   ENDIF
   ALLOCATE ( b_tracer(sm31:em31, sm32:em32, sm33:em33, num_tracer), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_tracer(sm31:em31, sm32:em32, sm33:em33, num_tracer). ')
   ENDIF

   !  U Boundary
   ALLOCATE(b_u_bxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_bxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_u_bxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_bxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_u_bys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_bys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_u_bye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_bye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_u_btxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_btxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_u_btxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_btxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_u_btys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_btys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_u_btye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_btye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF

   !  V Boundary
   ALLOCATE(b_v_bxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_bxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_v_bxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_bxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_v_bys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_bys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_v_bye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_bye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_v_btxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_btxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_v_btxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_btxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_v_btys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_btys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_v_btye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_btye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF

   !  T Boundary
   ALLOCATE(b_t_bxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_t_bxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_t_bxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_t_bxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_t_bys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_t_bys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_t_bye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_t_bye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_t_btxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_t_btxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_t_btxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_t_btxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_t_btys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_t_btys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_t_btye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_t_btye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF

   !  W Boundary
   ALLOCATE(b_w_bxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_w_bxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_w_bxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_w_bxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_w_bys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_w_bys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_w_bye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_w_bye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_w_btxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_w_btxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_w_btxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_w_btxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_w_btys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_w_btys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_w_btye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_w_btye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF

   !  PH Boundary
   ALLOCATE(b_ph_bxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ph_bxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_ph_bxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ph_bxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_ph_bys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ph_bys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_ph_bye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ph_bye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_ph_btxs(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ph_btxs(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_ph_btxe(sm33:em33,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ph_btxe(sm33:em33,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_ph_btys(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ph_btys(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_ph_btye(sm31:em31,sm32:em32,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ph_btye(sm31:em31,sm32:em32,spec_bdy_width). ')
   ENDIF

   !  MU Boundary
   ALLOCATE(b_mu_bxs(sm33:em33,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_mu_bxs(sm33:em33,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_mu_bxe(sm33:em33,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_mu_bxe(sm33:em33,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_mu_bys(sm31:em31,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_mu_bys(sm31:em31,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_mu_bye(sm31:em31,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_mu_bye(sm31:em31,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_mu_btxs(sm33:em33,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_mu_btxs(sm33:em33,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_mu_btxe(sm33:em33,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_mu_btxe(sm33:em33,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_mu_btys(sm31:em31,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_mu_btys(sm31:em31,1,spec_bdy_width). ')
   ENDIF
   ALLOCATE(b_mu_btye(sm31:em31,1,spec_bdy_width),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_mu_btye(sm31:em31,1,spec_bdy_width). ')
   ENDIF

   !  Moist Boundary
   ALLOCATE(b_moist_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_moist_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(b_moist_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_moist_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(b_moist_bys(sm31:em31,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_moist_bys(sm31:em31,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(b_moist_bye(sm31:em31,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_moist_bye(sm31:em31,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(b_moist_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_moist_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(b_moist_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_moist_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(b_moist_btys(sm31:em31,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_moist_btys(sm31:em31,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF
   ALLOCATE(b_moist_btye(sm31:em31,sm32:em32,spec_bdy_width,num_moist),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_moist_btye(sm31:em31,sm32:em32,spec_bdy_width,num_moist). ')
   ENDIF

   !  Tracer Boundary
   ALLOCATE(b_tracer_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_tracer_bxs(sm33:em33,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(b_tracer_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_tracer_bxe(sm33:em33,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(b_tracer_bys(sm31:em31,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_tracer_bys(sm31:em31,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(b_tracer_bye(sm31:em31,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_tracer_bye(sm31:em31,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(b_tracer_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_tracer_btxs(sm33:em33,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(b_tracer_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_tracer_btxe(sm33:em33,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(b_tracer_btys(sm31:em31,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_tracer_btys(sm31:em31,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF
   ALLOCATE(b_tracer_btye(sm31:em31,sm32:em32,spec_bdy_width,num_tracer),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_tracer_btye(sm31:em31,sm32:em32,spec_bdy_width,num_tracer). ')
   ENDIF

   ALLOCATE(b_rainnc(sm31:em31,sm33:em33),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_rainnc(sm31:em31,sm33:em33). ')
   ENDIF
   ALLOCATE(b_rainncv(sm31:em31,sm33:em33),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_rainncv(sm31:em31,sm33:em33). ')
   ENDIF
   ALLOCATE(b_rainc(sm31:em31,sm33:em33),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_rainc(sm31:em31,sm33:em33). ')
   ENDIF
   ALLOCATE(b_raincv(sm31:em31,sm33:em33),STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_raincv(sm31:em31,sm33:em33). ')
   ENDIF

   RETURN
  END SUBROUTINE allocate_grid

  SUBROUTINE copy_grid_to_s ( grid, its, ite, jts, jte )
   TYPE (domain), INTENT(IN) :: grid
   INTEGER, INTENT(IN) :: its, ite, jts, jte

   s_u_2 = 0.0
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,ite
      s_u_2(i,k,j) = grid%u_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   s_v_2 = 0.0
   DO j = jts,jte
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
      s_v_2(i,k,j) = grid%v_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   s_w_2 = 0.0
   s_ph_2 = 0.0
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
      s_w_2(i,k,j) = grid%w_2(i,k,j)
      s_ph_2(i,k,j) = grid%ph_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   s_t_2 = 0.0
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
      s_t_2(i,k,j) = grid%t_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   s_mu_2 = 0.0
   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
      s_mu_2(i,j) = grid%mu_2(i,j)
   ENDDO
   ENDDO

   s_moist = 0.0
   DO mm = PARAM_FIRST_SCALAR, num_moist
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
      s_moist(i,k,j,mm) = grid%moist(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   IF ( gwd_opt .EQ. 1 ) THEN
      s_dtaux3d = 0.0_8
      s_dtauy3d = 0.0_8
      s_dusfcg = 0.0_8
      s_dvsfcg = 0.0_8
      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte-1
      DO i = its,MIN(ite,ide-1)
         s_dtaux3d(i,k,j) = grid%dtaux3d(i,k,j)
         s_dtauy3d(i,k,j) = grid%dtauy3d(i,k,j)
      ENDDO
      ENDDO
      ENDDO
      DO j = jts,MIN(jte,jde-1)
      DO i = its,MIN(ite,ide-1)
         s_dusfcg(i,j) = grid%dusfcg(i,j)
         s_dvsfcg(i,j) = grid%dvsfcg(i,j)
      ENDDO
      ENDDO
   ENDIF

   s_tracer = 0.0
   DO mm = PARAM_FIRST_SCALAR, num_tracer
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
      s_tracer(i,k,j,mm) = grid%tracer(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   s_scalar = 0.0
   DO mm = PARAM_FIRST_SCALAR, num_scalar
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
      s_scalar(i,k,j,mm) = grid%scalar(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   s_tke_2 = grid%tke_2
   s_p = grid%p
   s_al = grid%al
   s_h_diabatic = grid%h_diabatic
   s_qv_diabatic = grid%qv_diabatic
   s_qc_diabatic = grid%qc_diabatic
   s_ustm = grid%ustm
   s_hfx = grid%hfx
   s_qfx = grid%qfx
   
   s_u_bxs = grid%u_bxs; s_u_bxe = grid%u_bxe; s_u_bys = grid%u_bys; s_u_bye = grid%u_bye
   s_u_btxs = grid%u_btxs; s_u_btxe = grid%u_btxe; s_u_btys = grid%u_btys; s_u_btye = grid%u_btye
   s_v_bxs = grid%v_bxs; s_v_bxe = grid%v_bxe; s_v_bys =grid%v_bys; s_v_bye=grid%v_bye
   s_v_btxs = grid%v_btxs; s_v_btxe = grid%v_btxe; s_v_btys =grid%v_btys; s_v_btye=grid%v_btye
   s_w_bxs = grid%w_bxs; s_w_bxe = grid%w_bxe; s_w_bys = grid%w_bys; s_w_bye = grid%w_bye
   s_w_btxs = grid%w_btxs; s_w_btxe = grid%w_btxe; s_w_btys = grid%w_btys; s_w_btye = grid%w_btye
   s_ph_bxs = grid%ph_bxs; s_ph_bxe = grid%ph_bxe; s_ph_bys = grid%ph_bys; s_ph_bye = grid%ph_bye
   s_ph_btxs = grid%ph_btxs; s_ph_btxe = grid%ph_btxe; s_ph_btys = grid%ph_btys; s_ph_btye = grid%ph_btye
   s_t_bxs = grid%t_bxs; s_t_bxe = grid%t_bxe; s_t_bys = grid%t_bys; s_t_bye = grid%t_bye
   s_t_btxs = grid%t_btxs; s_t_btxe = grid%t_btxe; s_t_btys = grid%t_btys; s_t_btye = grid%t_btye
   s_mu_bxs = grid%mu_bxs; s_mu_bxe = grid%mu_bxe; s_mu_bys = grid%mu_bys; s_mu_bye = grid%mu_bye
   s_mu_btxs = grid%mu_btxs; s_mu_btxe = grid%mu_btxe; s_mu_btys = grid%mu_btys; s_mu_btye = grid%mu_btye
   s_moist_bxs = grid%moist_bxs; s_moist_bxe = grid%moist_bxe; s_moist_bys = grid%moist_bys; s_moist_bye = grid%moist_bye
   s_moist_btxs = grid%moist_btxs; s_moist_btxe = grid%moist_btxe; s_moist_btys = grid%moist_btys; s_moist_btye = grid%moist_btye
   s_scalar_bxs = grid%scalar_bxs; s_scalar_bxe = grid%scalar_bxe; s_scalar_bys = grid%scalar_bys; s_scalar_bye = grid%scalar_bye
   s_scalar_btxs = grid%scalar_btxs; s_scalar_btxe = grid%scalar_btxe; s_scalar_btys = grid%scalar_btys; s_scalar_btye = grid%scalar_btye
   s_tracer_bxs = grid%tracer_bxs; s_tracer_bxe = grid%tracer_bxe; s_tracer_bys = grid%tracer_bys; s_tracer_bye = grid%tracer_bye
   s_tracer_btxs = grid%tracer_btxs; s_tracer_btxe = grid%tracer_btxe; s_tracer_btys = grid%tracer_btys; s_tracer_btye = grid%tracer_btye
   
   RETURN
  END SUBROUTINE copy_grid_to_s

  SUBROUTINE copy_grid_to_b ( grid )
   TYPE (domain), INTENT(IN) :: grid

   b_u_2 = grid%u_2
   b_v_2 = grid%v_2
   b_w_2 = grid%w_2
   b_ph_2 = grid%ph_2
   b_t_2 = grid%t_2
   b_mu_2 = grid%mu_2
   b_moist = grid%moist
   b_tracer = grid%tracer

   b_h_diabatic = grid%h_diabatic
   b_qv_diabatic = grid%qv_diabatic
   b_qc_diabatic = grid%qc_diabatic

   b_p = grid%p
   b_rainnc = grid%rainnc
   b_rainncv = grid%rainncv
   b_rainc = grid%rainc
   b_raincv = grid%raincv

   b_scalar = grid%scalar

   IF ( gwd_opt .EQ. 1 ) THEN
      b_dtaux3d = grid%dtaux3d
      b_dtauy3d = grid%dtauy3d
      b_dusfcg = grid%dusfcg
      b_dvsfcg = grid%dvsfcg
   ENDIF

!   b_tke_2 = grid%tke_2

!   b_al = grid%al
   
   RETURN
  END SUBROUTINE copy_grid_to_b

  SUBROUTINE restore_grid ( grid )
   TYPE (domain), INTENT(INOUT) :: grid

   grid%u_2 = s_u_2
   grid%v_2 = s_v_2
   grid%w_2 = s_w_2
   grid%ph_2 = s_ph_2
   grid%t_2 = s_t_2
   grid%mu_2 = s_mu_2
   grid%moist = s_moist
   grid%tracer = s_tracer
   grid%scalar = s_scalar
   grid%tke_2 = s_tke_2

   grid%p = s_p
   grid%al = s_al
   grid%h_diabatic = s_h_diabatic
   grid%qv_diabatic = s_qv_diabatic
   grid%qc_diabatic = s_qc_diabatic
   grid%ustm = s_ustm
   grid%hfx = s_hfx
   grid%qfx = s_qfx

   IF ( gwd_opt .EQ. 1 ) THEN
      grid%dtaux3d = s_dtaux3d
      grid%dtauy3d = s_dtauy3d
      grid%dusfcg = s_dusfcg
      grid%dvsfcg = s_dvsfcg
   ENDIF

   grid%u_bxs = s_u_bxs; grid%u_bxe = s_u_bxe; grid%u_bys = s_u_bys; grid%u_bye = s_u_bye
   grid%u_btxs = s_u_btxs; grid%u_btxe = s_u_btxe; grid%u_btys = s_u_btys; grid%u_btye = s_u_btye
   grid%v_bxs = s_v_bxs; grid%v_bxe = s_v_bxe; grid%v_bys = s_v_bys; grid%v_bye = s_v_bye
   grid%v_btxs = s_v_btxs; grid%v_btxe = s_v_btxe; grid%v_btys = s_v_btys; grid%v_btye = s_v_btye
   grid%w_bxs = s_w_bxs; grid%w_bxe = s_w_bxe; grid%w_bys = s_w_bys; grid%w_bye = s_w_bye
   grid%w_btxs = s_w_btxs; grid%w_btxe = s_w_btxe; grid%w_btys = s_w_btys; grid%w_btye = s_w_btye
   grid%ph_bxs = s_ph_bxs; grid%ph_bxe = s_ph_bxe; grid%ph_bys = s_ph_bys; grid%ph_bye = s_ph_bye
   grid%ph_btxs = s_ph_btxs; grid%ph_btxe = s_ph_btxe; grid%ph_btys = s_ph_btys; grid%ph_btye = s_ph_btye
   grid%t_bxs = s_t_bxs; grid%t_bxe = s_t_bxe; grid%t_bys = s_t_bys; grid%t_bye = s_t_bye
   grid%t_btxs = s_t_btxs; grid%t_btxe = s_t_btxe; grid%t_btys = s_t_btys; grid%t_btye = s_t_btye
   grid%mu_bxs = s_mu_bxs; grid%mu_bxe = s_mu_bxe; grid%mu_bys = s_mu_bys; grid%mu_bye = s_mu_bye
   grid%mu_btxs = s_mu_btxs; grid%mu_btxe = s_mu_btxe; grid%mu_btys = s_mu_btys; grid%mu_btye = s_mu_btye
   grid%moist_bxs = s_moist_bxs; grid%moist_bxe = s_moist_bxe; grid%moist_bys = s_moist_bys; grid%moist_bye = s_moist_bye
   grid%moist_btxs = s_moist_btxs; grid%moist_btxe = s_moist_btxe; grid%moist_btys = s_moist_btys; grid%moist_btye = s_moist_btye
   grid%scalar_bxs = s_scalar_bxs; grid%scalar_bxe = s_scalar_bxe; grid%scalar_bys = s_scalar_bys; grid%scalar_bye = s_scalar_bye
   grid%scalar_btxs = s_scalar_btxs; grid%scalar_btxe = s_scalar_btxe; grid%scalar_btys = s_scalar_btys; grid%scalar_btye = s_scalar_btye
   grid%tracer_bxs = s_tracer_bxs; grid%tracer_bxe = s_tracer_bxe; grid%tracer_bys = s_tracer_bys; grid%tracer_bye = s_tracer_bye
   grid%tracer_btxs = s_tracer_btxs; grid%tracer_btxe = s_tracer_btxe; grid%tracer_btys = s_tracer_btys; grid%tracer_btye = s_tracer_btye

  END SUBROUTINE restore_grid

  SUBROUTINE copy_s_to_g_adjtest ( grid, factor )
   TYPE (domain), INTENT(INOUT) :: grid
   REAL, INTENT(IN) :: factor
   REAL :: factor_u, factor_v, factor_w, factor_ph, factor_t, factor_mu, & 
           factor_moist, factor_tracer
   
   if (grid%check_u) then
      factor_u =factor
   else
      factor_u = 0.0
   endif
   if (grid%check_v) then
      factor_v =factor
   else
      factor_v = 0.0
   endif
   if (grid%check_w) then
      factor_w =factor
   else
      factor_w = 0.0
   endif
   if (grid%check_ph) then
      factor_ph =factor
   else
      factor_ph = 0.0
   endif
   if (grid%check_t) then
      factor_t =factor
   else
      factor_t = 0.0
   endif
   if (grid%check_mu) then
      factor_mu =factor
   else
      factor_mu = 0.0
   endif
   if (grid%check_moist) then
      factor_moist =factor
   else
      factor_moist = 0.0
   endif
   if (grid%check_tracer) then
      factor_tracer =factor
   else
      factor_tracer = 0.0
   endif

! I&O variables
   
   grid%g_u_2 = factor_u*s_u_2
   grid%g_v_2 = factor_v*s_v_2
   grid%g_w_2 = factor_w*s_w_2
   grid%g_ph_2 = factor_ph*s_ph_2
   grid%g_t_2 = factor_t*s_t_2
   grid%g_mu_2 = factor_mu*s_mu_2
   grid%g_moist = factor_moist*s_moist
   grid%g_tracer = factor_tracer*s_tracer

   grid%g_scalar = 0.0*s_scalar
   grid%g_tke_2 = 0.0*s_tke_2
   grid%g_p = 0.0*s_p
   grid%g_al = 0.0*s_al

! I variables
   grid%g_ustm = 0.0*s_ustm
   grid%g_hfx = 0.0*s_hfx
   grid%g_qfx = 0.0*s_qfx
 
   IF ( gwd_opt .EQ. 1 ) THEN
      grid%g_dtaux3d = factor_u*s_dtaux3d
      grid%g_dtauy3d = factor_u*s_dtauy3d
      grid%g_dusfcg = factor_u*s_dusfcg
      grid%g_dvsfcg = factor_u*s_dvsfcg
   ENDIF

   grid%g_u_bxs = factor_u*s_u_bxs; grid%g_u_bxe = factor_u*s_u_bxe; grid%g_u_bys = factor_u*s_u_bys; grid%g_u_bye = factor_u*s_u_bye
   grid%g_u_btxs = factor_u*s_u_btxs; grid%g_u_btxe = factor_u*s_u_btxe; grid%g_u_btys = factor_u*s_u_btys; grid%g_u_btye = factor_u*s_u_btye
   grid%g_v_bxs = factor_v*s_v_bxs; grid%g_v_bxe = factor_v*s_v_bxe; grid%g_v_bys = factor_v*s_v_bys; grid%g_v_bye = factor_v*s_v_bye
   grid%g_v_btxs = factor_v*s_v_btxs; grid%g_v_btxe = factor_v*s_v_btxe; grid%g_v_btys = factor_v*s_v_btys; grid%g_v_btye = factor_v*s_v_btye
   grid%g_w_bxs = factor_w*s_w_bxs; grid%g_w_bxe = factor_w*s_w_bxe; grid%g_w_bys = factor_w*s_w_bys; grid%g_w_bye = factor_w*s_w_bye
   grid%g_w_btxs = factor_w*s_w_btxs; grid%g_w_btxe = factor_w*s_w_btxe; grid%g_w_btys = factor_w*s_w_btys; grid%g_w_btye = factor_w*s_w_btye
   grid%g_ph_bxs = factor_ph*s_ph_bxs; grid%g_ph_bxe = factor_ph*s_ph_bxe; grid%g_ph_bys = factor_ph*s_ph_bys; grid%g_ph_bye = factor_ph*s_ph_bye
   grid%g_ph_btxs = factor_ph*s_ph_btxs; grid%g_ph_btxe = factor_ph*s_ph_btxe; grid%g_ph_btys = factor_ph*s_ph_btys; grid%g_ph_btye = factor_ph*s_ph_btye
   grid%g_t_bxs = factor_t*s_t_bxs; grid%g_t_bxe = factor_t*s_t_bxe; grid%g_t_bys = factor_t*s_t_bys; grid%g_t_bye = factor_t*s_t_bye
   grid%g_t_btxs = factor_t*s_t_btxs; grid%g_t_btxe = factor_t*s_t_btxe; grid%g_t_btys = factor_t*s_t_btys; grid%g_t_btye = factor_t*s_t_btye
   grid%g_mu_bxs = factor_mu*s_mu_bxs; grid%g_mu_bxe = factor_mu*s_mu_bxe; grid%g_mu_bys = factor_mu*s_mu_bys; grid%g_mu_bye = factor_mu*s_mu_bye
   grid%g_mu_btxs = factor_mu*s_mu_btxs; grid%g_mu_btxe = factor_mu*s_mu_btxe; grid%g_mu_btys = factor_mu*s_mu_btys; grid%g_mu_btye = factor_mu*s_mu_btye
   grid%g_moist_bxs = factor_moist*s_moist_bxs; grid%g_moist_bxe = factor_moist*s_moist_bxe; grid%g_moist_bys = factor_moist*s_moist_bys; grid%g_moist_bye = factor_moist*s_moist_bye
   grid%g_moist_btxs = factor_moist*s_moist_btxs; grid%g_moist_btxe = factor_moist*s_moist_btxe; grid%g_moist_btys = factor_moist*s_moist_btys; grid%g_moist_btye = factor_moist*s_moist_btye
   grid%g_tracer_bxs = factor_tracer*s_tracer_bxs; grid%g_tracer_bxe = factor_tracer*s_tracer_bxe; grid%g_tracer_bys = factor_tracer*s_tracer_bys; grid%g_tracer_bye = factor_tracer*s_tracer_bye
   grid%g_tracer_btxs = factor_tracer*s_tracer_btxs; grid%g_tracer_btxe = factor_tracer*s_tracer_btxe; grid%g_tracer_btys = factor_tracer*s_tracer_btys; grid%g_tracer_btye = factor_tracer*s_tracer_btye

   grid%g_scalar_bxs = 0.0*s_scalar_bxs; grid%g_scalar_bxe = 0.0*s_scalar_bxe; grid%g_scalar_bys = 0.0*s_scalar_bys; grid%g_scalar_bye = 0.0*s_scalar_bye
   grid%g_scalar_btxs = 0.0*s_scalar_btxs; grid%g_scalar_btxe = 0.0*s_scalar_btxe; grid%g_scalar_btys = 0.0*s_scalar_btys; grid%g_scalar_btye = 0.0*s_scalar_btye

! O variables
   grid%g_u_1 = 0.0
   grid%g_v_1 = 0.0
   grid%g_w_1 = 0.0
   grid%g_ph_1 = 0.0
   grid%g_t_1 = 0.0
   grid%g_mu_1 = 0.0
   grid%g_tke_1 = 0.0

   grid%g_ru = 0.0
   grid%g_rv = 0.0
   grid%g_rw = 0.0
   grid%g_ww = 0.0
   grid%g_alt = 0.0
   grid%g_php = 0.0
   grid%g_mut = 0.0
   grid%g_muu = 0.0
   grid%g_muv = 0.0
   grid%g_p_hyd_w = 0.0
   grid%g_p_hyd = 0.0
   grid%g_z_at_w = 0.0
   grid%g_z = 0.0
   grid%g_zx = 0.0
   grid%g_zy = 0.0
   grid%g_rdz = 0.0
   grid%g_rdzw = 0.0
   grid%g_div = 0.0
   grid%g_nba_mij = 0.0
   grid%g_nba_rij = 0.0
   grid%g_defor11 = 0.0
   grid%g_defor22 = 0.0
   grid%g_defor33 = 0.0
   grid%g_defor12 = 0.0
   grid%g_defor13 = 0.0
   grid%g_defor23 = 0.0
   grid%g_bn2 = 0.0
   grid%g_xkmh = 0.0
   grid%g_xkmv = 0.0
   grid%g_xkhh = 0.0
   grid%g_xkhv = 0.0
   grid%g_ru_tend = 0.0
   grid%g_rv_tend = 0.0
   grid%g_u_save = 0.0
   grid%g_v_save = 0.0
   grid%g_t_save = 0.0
   grid%g_rthften = 0.0
   grid%g_rqvften = 0.0
   grid%g_muts = 0.0
   grid%g_mudf = 0.0
   grid%g_ru_m = 0.0
   grid%g_rv_m = 0.0
   grid%g_ww_m = 0.0
   
   grid%g_h_diabatic = 0.0
   grid%g_qv_diabatic = 0.0
   grid%g_qc_diabatic = 0.0
   grid%g_rainnc = 0.0
   grid%g_rainncv = 0.0
   grid%g_rainc = 0.0
   grid%g_raincv = 0.0

  END SUBROUTINE copy_s_to_g_adjtest

  SUBROUTINE copy_g_to_b_adjtest ( grid )
   TYPE (domain), INTENT(IN) :: grid

   b_u_2 = grid%g_u_2
   b_v_2 = grid%g_v_2
   b_w_2 = grid%g_w_2
   b_ph_2 = grid%g_ph_2
   b_t_2 = grid%g_t_2
   b_mu_2 = grid%g_mu_2
   b_tke_2 = grid%g_tke_2
   b_moist = grid%g_moist
   b_tracer = grid%g_tracer
   b_scalar = grid%g_scalar
   b_tke_2 = grid%g_tke_2

   b_p = grid%g_p
   b_al = grid%g_al

   b_ustm = grid%g_ustm
   b_hfx = grid%g_hfx
   b_qfx = grid%g_qfx
   b_h_diabatic = grid%g_h_diabatic
   b_qv_diabatic = grid%g_qv_diabatic
   b_qc_diabatic = grid%g_qc_diabatic
   
   IF ( gwd_opt .EQ. 1 ) THEN
      b_dtaux3d = grid%g_dtaux3d
      b_dtauy3d = grid%g_dtauy3d
      b_dusfcg = grid%g_dusfcg
      b_dvsfcg = grid%g_dvsfcg
   ENDIF

   b_u_bxs = grid%g_u_bxs; b_u_bxe = grid%g_u_bxe; b_u_bys = grid%g_u_bys; b_u_bye = grid%g_u_bye
   b_v_bxs = grid%g_v_bxs; b_v_bxe = grid%g_v_bxe; b_v_bys = grid%g_v_bys; b_v_bye = grid%g_v_bye
   b_w_bxs = grid%g_w_bxs; b_w_bxe = grid%g_w_bxe; b_w_bys = grid%g_w_bys; b_w_bye = grid%g_w_bye
   b_ph_bxs = grid%g_ph_bxs; b_ph_bxe = grid%g_ph_bxe; b_ph_bys = grid%g_ph_bys; b_ph_bye = grid%g_ph_bye
   b_t_bxs = grid%g_t_bxs; b_t_bxe = grid%g_t_bxe; b_t_bys = grid%g_t_bys; b_t_bye = grid%g_t_bye
   b_mu_bxs = grid%g_mu_bxs; b_mu_bxe = grid%g_mu_bxe; b_mu_bys = grid%g_mu_bys; b_mu_bye = grid%g_mu_bye
   b_moist_bxs = grid%g_moist_bxs; b_moist_bxe = grid%g_moist_bxe; b_moist_bys = grid%g_moist_bys; b_moist_bye = grid%g_moist_bye
   b_tracer_bxs = grid%g_tracer_bxs; b_tracer_bxe = grid%g_tracer_bxe; b_tracer_bys = grid%g_tracer_bys; b_tracer_bye = grid%g_tracer_bye
   b_u_btxs = grid%g_u_btxs; b_u_btxe = grid%g_u_btxe; b_u_btys = grid%g_u_btys; b_u_btye = grid%g_u_btye
   b_v_btxs = grid%g_v_btxs; b_v_btxe = grid%g_v_btxe; b_v_btys = grid%g_v_btys; b_v_btye = grid%g_v_btye
   b_w_btxs = grid%g_w_btxs; b_w_btxe = grid%g_w_btxe; b_w_btys = grid%g_w_btys; b_w_btye = grid%g_w_btye
   b_ph_btxs = grid%g_ph_btxs; b_ph_btxe = grid%g_ph_btxe; b_ph_btys = grid%g_ph_btys; b_ph_btye = grid%g_ph_btye
   b_t_btxs = grid%g_t_btxs; b_t_btxe = grid%g_t_btxe; b_t_btys = grid%g_t_btys; b_t_btye = grid%g_t_btye
   b_mu_btxs = grid%g_mu_btxs; b_mu_btxe = grid%g_mu_btxe; b_mu_btys = grid%g_mu_btys; b_mu_btye = grid%g_mu_btye
   b_moist_btxs = grid%g_moist_btxs; b_moist_btxe = grid%g_moist_btxe; b_moist_btys = grid%g_moist_btys; b_moist_btye = grid%g_moist_btye
   b_tracer_btxs = grid%g_tracer_btxs; b_tracer_btxe = grid%g_tracer_btxe; b_tracer_btys = grid%g_tracer_btys; b_tracer_btye = grid%g_tracer_btye

   b_scalar_bxs = grid%g_scalar_bxs; b_scalar_bxe = grid%g_scalar_bxe; b_scalar_bys = grid%g_scalar_bys; b_scalar_bye = grid%g_scalar_bye
   b_scalar_btxs = grid%g_scalar_btxs; b_scalar_btxe = grid%g_scalar_btxe; b_scalar_btys = grid%g_scalar_btys; b_scalar_btye = grid%g_scalar_btye

   RETURN
  END SUBROUTINE copy_g_to_b_adjtest

  SUBROUTINE inner_dot_g ( grid, save, its, ite, jts, jte )
   TYPE (domain), INTENT(IN) :: grid
   REAL, INTENT(INOUT) :: save
   INTEGER, INTENT(IN) :: its, ite, jts, jte
   INTEGER :: i, k, j, mm

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,ite
     save  = save &
                  + grid%g_u_2(i,k,j) * grid%g_u_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,jte
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + grid%g_v_2(i,k,j) * grid%g_v_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + grid%g_w_2(i,k,j) * grid%g_w_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + grid%g_ph_2(i,k,j) * grid%g_ph_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + grid%g_t_2(i,k,j) * grid%g_t_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO


   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + grid%g_mu_2(i,j) * grid%g_mu_2(i,j)
   ENDDO
   ENDDO

   DO mm = PARAM_FIRST_SCALAR, num_moist
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_moist(i,k,j,mm) * grid%g_moist(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   DO mm = PARAM_FIRST_SCALAR, num_tracer
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_tracer(i,k,j,mm) * grid%g_tracer(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_h_diabatic(i,k,j) * grid%g_h_diabatic(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_qv_diabatic(i,k,j) * grid%g_qv_diabatic(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_qc_diabatic(i,k,j) * grid%g_qc_diabatic(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_p(i,k,j) * grid%g_p(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_rainnc(i,j) * grid%g_rainnc(i,j) &
                  + grid%g_rainncv(i,j) * grid%g_rainncv(i,j)
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_rainc(i,j) * grid%g_rainc(i,j) &
                  + grid%g_raincv(i,j) * grid%g_raincv(i,j)
   ENDDO
   ENDDO

   IF ( gwd_opt .EQ. 1 ) THEN
      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte-1
      DO i = its,MIN(ite,ide-1)
        save  = save + grid%g_dtaux3d(i,k,j) * grid%g_dtaux3d(i,k,j) &
                     + grid%g_dtauy3d(i,k,j) * grid%g_dtauy3d(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,MIN(jte,jde-1)
      DO i = its,MIN(ite,ide-1)
        save  = save + grid%g_dusfcg(i,j) * grid%g_dusfcg(i,j) &
                     + grid%g_dvsfcg(i,j) * grid%g_dvsfcg(i,j)
      ENDDO
      ENDDO
   ENDIF

   RETURN
  END SUBROUTINE inner_dot_g

  SUBROUTINE inner_dot_g_adjtest ( grid, save, its, ite, jts, jte )
   TYPE (domain), INTENT(IN) :: grid
   REAL, INTENT(INOUT) :: save
   INTEGER, INTENT(IN) :: its, ite, jts, jte
   INTEGER :: i, k, j, mm

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,ite
     save  = save + grid%g_u_2(i,k,j) * grid%g_u_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,jte
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_v_2(i,k,j) * grid%g_v_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_w_2(i,k,j) * grid%g_w_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_ph_2(i,k,j) * grid%g_ph_2(i,k,j) 
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_t_2(i,k,j) * grid%g_t_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_mu_2(i,j) * grid%g_mu_2(i,j)
   ENDDO
   ENDDO

   DO mm = PARAM_FIRST_SCALAR, num_moist
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_moist(i,k,j,mm) * grid%g_moist(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   DO mm = PARAM_FIRST_SCALAR, num_tracer
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_tracer(i,k,j,mm) * grid%g_tracer(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO
  
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_h_diabatic(i,k,j) * grid%g_h_diabatic(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_qv_diabatic(i,k,j) * grid%g_qv_diabatic(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_qc_diabatic(i,k,j) * grid%g_qc_diabatic(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_p(i,k,j) * grid%g_p(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_rainnc(i,j) * grid%g_rainnc(i,j) &
                  + grid%g_rainncv(i,j) * grid%g_rainncv(i,j)
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_rainc(i,j) * grid%g_rainc(i,j) &
                  + grid%g_raincv(i,j) * grid%g_raincv(i,j)
   ENDDO
   ENDDO

   IF ( gwd_opt .EQ. 1 ) THEN
      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte-1
      DO i = its,MIN(ite,ide-1)
        save  = save + grid%g_dtaux3d(i,k,j) * grid%g_dtaux3d(i,k,j) &
                     + grid%g_dtauy3d(i,k,j) * grid%g_dtauy3d(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,MIN(jte,jde-1)
      DO i = its,MIN(ite,ide-1)
        save  = save + grid%g_dusfcg(i,j) * grid%g_dusfcg(i,j) &
                     + grid%g_dvsfcg(i,j) * grid%g_dvsfcg(i,j)
      ENDDO
      ENDDO
   ENDIF

   RETURN

  END SUBROUTINE inner_dot_g_adjtest

  SUBROUTINE add_grid ( grid, factor )
   TYPE (domain), INTENT(INOUT) :: grid
   REAL, INTENT(IN) :: factor
   REAL :: factor_u, factor_v, factor_w, factor_ph, factor_t, factor_mu, factor_moist, factor_tracer
   
   if (grid%check_u) then
      factor_u =factor
   else
      factor_u = 1.0
   endif
   if (grid%check_v) then
      factor_v =factor
   else
      factor_v = 1.0
   endif
   if (grid%check_w) then
      factor_w =factor
   else
      factor_w = 1.0
   endif
   if (grid%check_ph) then
      factor_ph =factor
   else
      factor_ph = 1.0
   endif
   if (grid%check_t) then
      factor_t =factor
   else
      factor_t = 1.0
   endif
   if (grid%check_mu) then
      factor_mu =factor
   else
      factor_mu = 1.0
   endif
   if (grid%check_moist) then
      factor_moist =factor
   else
      factor_moist = 1.0
   endif
   if (grid%check_tracer) then
      factor_tracer =factor
   else
      factor_tracer = 1.0
   endif

   grid%u_2 = factor_u*s_u_2
   grid%v_2 = factor_v*s_v_2
   grid%w_2 = factor_w*s_w_2
   grid%ph_2 = factor_ph*s_ph_2
   grid%t_2 = factor_t*s_t_2
   grid%mu_2 = factor_mu*s_mu_2
   grid%moist = factor_moist*s_moist
   grid%tracer = factor_tracer*s_tracer
   grid%scalar = s_scalar
   grid%tke_2 = s_tke_2

   grid%p = s_p
   grid%al = s_al
   grid%h_diabatic = s_h_diabatic
   grid%qv_diabatic = s_qv_diabatic
   grid%qc_diabatic = s_qc_diabatic
   grid%ustm = s_ustm
   grid%hfx = s_hfx
   grid%qfx = s_qfx
   
   IF ( gwd_opt .EQ. 1 ) THEN
      grid%dtaux3d = factor_u*s_dtaux3d
      grid%dtauy3d = factor_u*s_dtauy3d
      grid%dusfcg = factor_u*s_dusfcg
      grid%dvsfcg = factor_u*s_dvsfcg
   ENDIF

   grid%u_bxs = factor_u*s_u_bxs; grid%u_bxe = factor_u*s_u_bxe; grid%u_bys = factor_u*s_u_bys; grid%u_bye = factor_u*s_u_bye
   grid%u_btxs = factor_u*s_u_btxs; grid%u_btxe = factor_u*s_u_btxe; grid%u_btys = factor_u*s_u_btys; grid%u_btye = factor_u*s_u_btye
   grid%v_bxs = factor_v*s_v_bxs; grid%v_bxe = factor_v*s_v_bxe; grid%v_bys = factor_v*s_v_bys; grid%v_bye = factor_v*s_v_bye
   grid%v_btxs = factor_v*s_v_btxs; grid%v_btxe = factor_v*s_v_btxe; grid%v_btys = factor_v*s_v_btys; grid%v_btye = factor_v*s_v_btye
   grid%w_bxs = factor_w*s_w_bxs; grid%w_bxe = factor_w*s_w_bxe; grid%w_bys = factor_w*s_w_bys; grid%w_bye = factor_w*s_w_bye
   grid%w_btxs = factor_w*s_w_btxs; grid%w_btxe = factor_w*s_w_btxe; grid%w_btys = factor_w*s_w_btys; grid%w_btye = factor_w*s_w_btye
   grid%ph_bxs = factor_ph*s_ph_bxs; grid%ph_bxe = factor_ph*s_ph_bxe; grid%ph_bys = factor_ph*s_ph_bys; grid%ph_bye = factor_ph*s_ph_bye
   grid%ph_btxs = factor_ph*s_ph_btxs; grid%ph_btxe = factor_ph*s_ph_btxe; grid%ph_btys = factor_ph*s_ph_btys; grid%ph_btye = factor_ph*s_ph_btye
   grid%t_bxs = factor_t*s_t_bxs; grid%t_bxe = factor_t*s_t_bxe; grid%t_bys = factor_t*s_t_bys; grid%t_bye = factor_t*s_t_bye
   grid%t_btxs = factor_t*s_t_btxs; grid%t_btxe = factor_t*s_t_btxe; grid%t_btys = factor_t*s_t_btys; grid%t_btye = factor_t*s_t_btye
   grid%mu_bxs = factor_mu*s_mu_bxs; grid%mu_bxe = factor_mu*s_mu_bxe; grid%mu_bys = factor_mu*s_mu_bys; grid%mu_bye = factor_mu*s_mu_bye
   grid%mu_btxs = factor_mu*s_mu_btxs; grid%mu_btxe = factor_mu*s_mu_btxe; grid%mu_btys = factor_mu*s_mu_btys; grid%mu_btye = factor_mu*s_mu_btye
   grid%moist_bxs = factor_moist*s_moist_bxs; grid%moist_bxe = factor_moist*s_moist_bxe; grid%moist_bys = factor_moist*s_moist_bys; grid%moist_bye = factor_moist*s_moist_bye
   grid%moist_btxs = factor_moist*s_moist_btxs; grid%moist_btxe = factor_moist*s_moist_btxe; grid%moist_btys = factor_moist*s_moist_btys; grid%moist_btye = factor_moist*s_moist_btye
   grid%tracer_bxs = factor_tracer*s_tracer_bxs; grid%tracer_bxe = factor_tracer*s_tracer_bxe; grid%tracer_bys = factor_tracer*s_tracer_bys; grid%tracer_bye = factor_tracer*s_tracer_bye
   grid%tracer_btxs = factor_tracer*s_tracer_btxs; grid%tracer_btxe = factor_tracer*s_tracer_btxe; grid%tracer_btys = factor_tracer*s_tracer_btys; grid%tracer_btye = factor_tracer*s_tracer_btye
   grid%scalar_bxs = s_scalar_bxs; grid%scalar_bxe = s_scalar_bxe; grid%scalar_bys = s_scalar_bys; grid%scalar_bye = s_scalar_bye
   grid%scalar_btxs = s_scalar_btxs; grid%scalar_btxe = s_scalar_btxe; grid%scalar_btys = s_scalar_btys; grid%scalar_btye = s_scalar_btye

   RETURN
  END SUBROUTINE add_grid

  SUBROUTINE inner_dot ( grid, save, its, ite, jts, jte )
   TYPE (domain), INTENT(IN) :: grid
   INTEGER, INTENT(IN) :: its, ite, jts, jte
   REAL, INTENT(INOUT) :: save
   INTEGER :: i, k, j, mm

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,ite
     save  = save &
                  + (grid%u_2(i,k,j)-b_u_2(i,k,j)) * (grid%u_2(i,k,j)-b_u_2(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,jte
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + (grid%v_2(i,k,j)-b_v_2(i,k,j)) * (grid%v_2(i,k,j)-b_v_2(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + (grid%w_2(i,k,j)-b_w_2(i,k,j)) * (grid%w_2(i,k,j)-b_w_2(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + (grid%ph_2(i,k,j)-b_ph_2(i,k,j)) * (grid%ph_2(i,k,j)-b_ph_2(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + (grid%t_2(i,k,j)-b_t_2(i,k,j)) * (grid%t_2(i,k,j)-b_t_2(i,k,j))
   ENDDO
   ENDDO
   ENDDO


   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + (grid%mu_2(i,j)-b_mu_2(i,j)) * (grid%mu_2(i,j)-b_mu_2(i,j))
   ENDDO
   ENDDO

   DO mm = PARAM_FIRST_SCALAR, num_moist
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + (grid%moist(i,k,j,mm)-b_moist(i,k,j,mm)) * (grid%moist(i,k,j,mm)-b_moist(i,k,j,mm))
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   DO mm = PARAM_FIRST_SCALAR, num_tracer
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + (grid%tracer(i,k,j,mm)-b_tracer(i,k,j,mm)) * (grid%tracer(i,k,j,mm)-b_tracer(i,k,j,mm))
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + (grid%h_diabatic(i,k,j)-b_h_diabatic(i,k,j)) * (grid%h_diabatic(i,k,j)-b_h_diabatic(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + (grid%qv_diabatic(i,k,j)-b_qv_diabatic(i,k,j)) * (grid%qv_diabatic(i,k,j)-b_qv_diabatic(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + (grid%qc_diabatic(i,k,j)-b_qc_diabatic(i,k,j)) * (grid%qc_diabatic(i,k,j)-b_qc_diabatic(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save &
                  + (grid%p(i,k,j)-b_p(i,k,j)) * (grid%p(i,k,j)-b_p(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save + (grid%rainnc(i,j)-b_rainnc(i,j)) * (grid%rainnc(i,j)-b_rainnc(i,j)) &
                  + (grid%rainncv(i,j)-b_rainncv(i,j)) * (grid%rainncv(i,j)-b_rainncv(i,j))
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save + (grid%rainc(i,j)-b_rainc(i,j)) * (grid%rainc(i,j)-b_rainc(i,j)) &
                  + (grid%raincv(i,j)-b_raincv(i,j)) * (grid%raincv(i,j)-b_raincv(i,j))
   ENDDO
   ENDDO

   IF ( gwd_opt .EQ. 1 ) THEN
      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte-1
      DO i = its,MIN(ite,ide-1)
        save  = save + (grid%dtaux3d(i,k,j)-b_dtaux3d(i,k,j)) * (grid%dtaux3d(i,k,j)-b_dtaux3d(i,k,j)) &
                     + (grid%dtauy3d(i,k,j)-b_dtauy3d(i,k,j)) * (grid%dtauy3d(i,k,j)-b_dtauy3d(i,k,j))
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,MIN(jte,jde-1)
      DO i = its,MIN(ite,ide-1)
        save  = save + (grid%dusfcg(i,j)-b_dusfcg(i,j)) * (grid%dusfcg(i,j)-b_dusfcg(i,j)) &
                     + (grid%dvsfcg(i,j)-b_dvsfcg(i,j)) * (grid%dvsfcg(i,j)-b_dvsfcg(i,j))
      ENDDO
      ENDDO
   ENDIF

   RETURN
  END SUBROUTINE inner_dot

  SUBROUTINE copy_g_to_a_adjtest ( grid, its, ite, jts, jte )
   TYPE (domain), INTENT(INOUT) :: grid
   INTEGER, INTENT(IN) :: its, ite, jts, jte
   INTEGER :: i, k, j, mm

   CALL zero_out_ad ( grid )

      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte-1
      DO i = its,ite
         grid%a_u_2(i,k,j) = grid%g_u_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,jte
      DO k = kts,kte-1
      DO i = its,MIN(ite,ide-1)
         grid%a_v_2(i,k,j) = grid%g_v_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte
      DO i = its,MIN(ite,ide-1)
         grid%a_w_2(i,k,j) = grid%g_w_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte-1
      DO i = its,MIN(ite,ide-1)
         grid%a_t_2(i,k,j) = grid%g_t_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte
      DO i = its,MIN(ite,ide-1)
         grid%a_ph_2(i,k,j) = grid%g_ph_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j = jts,MIN(jte,jde-1)
      DO i = its,MIN(ite,ide-1)
         grid%a_mu_2(i,j) = grid%g_mu_2(i,j)
      ENDDO
      ENDDO

      DO mm = PARAM_FIRST_SCALAR, num_moist
      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte-1
      DO i = its,MIN(ite,ide-1)
         grid%a_moist(i,k,j,mm) = grid%g_moist(i,k,j,mm)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
   
      DO mm = PARAM_FIRST_SCALAR, num_tracer
      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte-1
      DO i = its,MIN(ite,ide-1)
         grid%a_tracer(i,k,j,mm) = grid%g_tracer(i,k,j,mm)
      ENDDO
      ENDDO
      ENDDO
      ENDDO
   
   DO mm = PARAM_FIRST_SCALAR, num_scalar
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
      grid%a_scalar(i,k,j,mm) = grid%g_scalar(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
      grid%a_h_diabatic(i,k,j)  = grid%g_h_diabatic(i,k,j)
      grid%a_qv_diabatic(i,k,j) = grid%g_qv_diabatic(i,k,j)
      grid%a_qc_diabatic(i,k,j) = grid%g_qc_diabatic(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
      grid%a_rainnc(i,j)  = grid%g_rainnc(i,j)
      grid%a_rainncv(i,j) = grid%g_rainncv(i,j)

      grid%a_rainc(i,j)  = grid%g_rainc(i,j)
      grid%a_raincv(i,j) = grid%g_raincv(i,j)
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
      grid%a_p(i,k,j) = grid%g_p(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   IF ( gwd_opt .EQ. 1 ) THEN
      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte-1
      DO i = its,MIN(ite,ide-1)
         grid%a_dtaux3d(i,k,j) = grid%g_dtaux3d(i,k,j)
         grid%a_dtauy3d(i,k,j) = grid%g_dtauy3d(i,k,j) 
      ENDDO
      ENDDO
      ENDDO
      DO j = jts,MIN(jte,jde-1)
      DO i = its,MIN(ite,ide-1)
         grid%a_dusfcg(i,j) = grid%g_dusfcg(i,j)
         grid%a_dvsfcg(i,j) = grid%g_dvsfcg(i,j)
      ENDDO
      ENDDO
   ENDIF

   RETURN
  END SUBROUTINE copy_g_to_a_adjtest

  SUBROUTINE zero_out_ad ( grid )
   TYPE (domain), INTENT(INOUT) :: grid

   grid%a_u_1 = 0.0
   grid%a_u_2 = 0.0

   grid%a_v_1 = 0.0
   grid%a_v_2 = 0.0

   grid%a_w_1 = 0.0
   grid%a_w_2 = 0.0

   grid%a_t_1 = 0.0
   grid%a_t_2 = 0.0

   grid%a_ph_1 = 0.0
   grid%a_ph_2 = 0.0

   grid%a_mu_1 = 0.0
   grid%a_mu_2 = 0.0

   grid%a_moist = 0.0
   grid%a_tracer = 0.0
   
   grid%a_scalar = 0.0

   grid%a_h_diabatic = 0.0
   grid%a_qv_diabatic = 0.0
   grid%a_qc_diabatic = 0.0

   grid%a_rainnc = 0.0
   grid%a_rainncv = 0.0
   grid%a_rainc = 0.0
   grid%a_raincv = 0.0
   grid%a_pratec = 0.0

   grid%a_p = 0.0

! I&O variables
   grid%a_tke_1 = 0.0
   grid%a_tke_2 = 0.0
   grid%a_al=0.0

! O variables
   grid%a_ru = 0.0
   grid%a_rv = 0.0
   grid%a_rw = 0.0
   grid%a_ww = 0.0
   grid%a_alt = 0.0
   grid%a_php = 0.0
   grid%a_mut = 0.0
   grid%a_muu = 0.0
   grid%a_muv = 0.0
   grid%a_p_hyd_w = 0.0
   grid%a_p_hyd = 0.0
   grid%a_z_at_w = 0.0
   grid%a_z = 0.0
   grid%a_zx = 0.0
   grid%a_zy = 0.0
   grid%a_rdz = 0.0
   grid%a_rdzw = 0.0
   grid%a_div = 0.0
   grid%a_nba_mij = 0.0
   grid%a_nba_rij = 0.0
   grid%a_defor11 = 0.0
   grid%a_defor22 = 0.0
   grid%a_defor33 = 0.0
   grid%a_defor12 = 0.0
   grid%a_defor13 = 0.0
   grid%a_defor23 = 0.0
   grid%a_bn2 = 0.0
   grid%a_xkmh = 0.0
   grid%a_xkmv = 0.0
   grid%a_xkhh = 0.0
   grid%a_xkhv = 0.0
   grid%a_ru_tend = 0.0
   grid%a_rv_tend = 0.0
   grid%a_u_save = 0.0
   grid%a_v_save = 0.0
   grid%a_t_save = 0.0
   grid%a_rthften = 0.0
   grid%a_rqvften = 0.0
   grid%a_muts = 0.0
   grid%a_mudf = 0.0
   grid%a_ru_m = 0.0
   grid%a_rv_m = 0.0
   grid%a_ww_m = 0.0

   grid%a_rublten = 0.0
   grid%a_rvblten = 0.0
   grid%a_rqvblten = 0.0
! I variables
   grid%a_ustm=0.0
   grid%a_hfx=0.0
   grid%a_qfx=0.0

   IF ( gwd_opt .EQ. 1 ) THEN
      grid%a_dtaux3d = 0.0
      grid%a_dtauy3d = 0.0
      grid%a_dusfcg = 0.0
      grid%a_dvsfcg = 0.0
   ENDIF

   grid%a_u_bxs = 0.0; grid%a_u_bxe = 0.0; grid%a_u_bys = 0.0; grid%a_u_bye = 0.0
   grid%a_v_bxs = 0.0; grid%a_v_bxe = 0.0; grid%a_v_bys = 0.0; grid%a_v_bye = 0.0
   grid%a_w_bxs = 0.0; grid%a_w_bxe = 0.0; grid%a_w_bys = 0.0; grid%a_w_bye = 0.0
   grid%a_ph_bxs = 0.0; grid%a_ph_bxe = 0.0; grid%a_ph_bys = 0.0; grid%a_ph_bye = 0.0
   grid%a_t_bxs = 0.0; grid%a_t_bxe = 0.0; grid%a_t_bys = 0.0; grid%a_t_bye = 0.0
   grid%a_mu_bxs = 0.0; grid%a_mu_bxe = 0.0; grid%a_mu_bys = 0.0; grid%a_mu_bye = 0.0
   grid%a_moist_bxs = 0.0; grid%a_moist_bxe = 0.0; grid%a_moist_bys = 0.0; grid%a_moist_bye = 0.0
   grid%a_tracer_bxs = 0.0; grid%a_tracer_bxe = 0.0; grid%a_tracer_bys = 0.0; grid%a_tracer_bye = 0.0
   grid%a_scalar_bxs = 0.0; grid%a_scalar_bxe = 0.0; grid%a_scalar_bys = 0.0; grid%a_scalar_bye = 0.0

   grid%a_u_btxs = 0.0; grid%a_u_btxe = 0.0; grid%a_u_btys = 0.0; grid%a_u_btye = 0.0
   grid%a_v_btxs = 0.0; grid%a_v_btxe = 0.0; grid%a_v_btys = 0.0; grid%a_v_btye = 0.0
   grid%a_w_btxs = 0.0; grid%a_w_btxe = 0.0; grid%a_w_btys = 0.0; grid%a_w_btye = 0.0
   grid%a_ph_btxs = 0.0; grid%a_ph_btxe = 0.0; grid%a_ph_btys = 0.0; grid%a_ph_btye = 0.0
   grid%a_t_btxs = 0.0; grid%a_t_btxe = 0.0; grid%a_t_btys = 0.0; grid%a_t_btye = 0.0
   grid%a_mu_btxs = 0.0; grid%a_mu_btxe = 0.0; grid%a_mu_btys = 0.0; grid%a_mu_btye = 0.0
   grid%a_moist_btxs = 0.0; grid%a_moist_btxe = 0.0; grid%a_moist_btys = 0.0; grid%a_moist_btye = 0.0
   grid%a_tracer_btxs = 0.0; grid%a_tracer_btxe = 0.0; grid%a_tracer_btys = 0.0; grid%a_tracer_btye = 0.0
   grid%a_scalar_btxs = 0.0; grid%a_scalar_btxe = 0.0; grid%a_scalar_btys = 0.0; grid%a_scalar_btye = 0.0

   RETURN
  END SUBROUTINE zero_out_ad


  SUBROUTINE zero_out_tl ( grid )
   TYPE (domain), INTENT(INOUT) :: grid

   grid%g_u_1 = 0.0
   grid%g_u_2 = 0.0

   grid%g_v_1 = 0.0
   grid%g_v_2 = 0.0

   grid%g_w_1 = 0.0
   grid%g_w_2 = 0.0

   grid%g_t_1 = 0.0
   grid%g_t_2 = 0.0

   grid%g_ph_1 = 0.0
   grid%g_ph_2 = 0.0

   grid%g_mu_1 = 0.0
   grid%g_mu_2 = 0.0

   grid%g_moist = 0.0
   grid%g_tracer = 0.0
   
   grid%g_scalar = 0.0

   grid%g_h_diabatic = 0.0
   grid%g_qv_diabatic = 0.0
   grid%g_qc_diabatic = 0.0

   grid%g_rainnc = 0.0
   grid%g_rainncv = 0.0
   grid%g_rainc = 0.0
   grid%g_raincv = 0.0
   grid%g_pratec = 0.0

   grid%g_p = 0.0

! I&O variables
   grid%g_tke_1 = 0.0
   grid%g_tke_2 = 0.0
   grid%g_al=0.0

! O variables
   grid%g_ru = 0.0
   grid%g_rv = 0.0
   grid%g_rw = 0.0
   grid%g_ww = 0.0
   grid%g_alt = 0.0
   grid%g_php = 0.0
   grid%g_mut = 0.0
   grid%g_muu = 0.0
   grid%g_muv = 0.0
   grid%g_p_hyd_w = 0.0
   grid%g_p_hyd = 0.0
   grid%g_z_at_w = 0.0
   grid%g_z = 0.0
   grid%g_zx = 0.0
   grid%g_zy = 0.0
   grid%g_rdz = 0.0
   grid%g_rdzw = 0.0
   grid%g_div = 0.0
   grid%g_nba_mij = 0.0
   grid%g_nba_rij = 0.0
   grid%g_defor11 = 0.0
   grid%g_defor22 = 0.0
   grid%g_defor33 = 0.0
   grid%g_defor12 = 0.0
   grid%g_defor13 = 0.0
   grid%g_defor23 = 0.0
   grid%g_bn2 = 0.0
   grid%g_xkmh = 0.0
   grid%g_xkmv = 0.0
   grid%g_xkhh = 0.0
   grid%g_xkhv = 0.0
   grid%g_ru_tend = 0.0
   grid%g_rv_tend = 0.0
   grid%g_u_save = 0.0
   grid%g_v_save = 0.0
   grid%g_t_save = 0.0
   grid%g_rthften = 0.0
   grid%g_rqvften = 0.0
   grid%g_muts = 0.0
   grid%g_mudf = 0.0
   grid%g_ru_m = 0.0
   grid%g_rv_m = 0.0
   grid%g_ww_m = 0.0

   grid%g_rublten = 0.0
   grid%g_rvblten = 0.0
   grid%g_rqvblten = 0.0

! I variables
   grid%g_ustm=0.0
   grid%g_hfx=0.0
   grid%g_qfx=0.0

   IF ( gwd_opt .EQ. 1 ) THEN
      grid%g_dtaux3d = 0.0
      grid%g_dtauy3d = 0.0
      grid%g_dusfcg = 0.0
      grid%g_dvsfcg = 0.0
   ENDIF

   grid%g_u_bxs = 0.0; grid%g_u_bxe = 0.0; grid%g_u_bys = 0.0; grid%g_u_bye = 0.0
   grid%g_v_bxs = 0.0; grid%g_v_bxe = 0.0; grid%g_v_bys = 0.0; grid%g_v_bye = 0.0
   grid%g_w_bxs = 0.0; grid%g_w_bxe = 0.0; grid%g_w_bys = 0.0; grid%g_w_bye = 0.0
   grid%g_ph_bxs = 0.0; grid%g_ph_bxe = 0.0; grid%g_ph_bys = 0.0; grid%g_ph_bye = 0.0
   grid%g_t_bxs = 0.0; grid%g_t_bxe = 0.0; grid%g_t_bys = 0.0; grid%g_t_bye = 0.0
   grid%g_mu_bxs = 0.0; grid%g_mu_bxe = 0.0; grid%g_mu_bys = 0.0; grid%g_mu_bye = 0.0
   grid%g_moist_bxs = 0.0; grid%g_moist_bxe = 0.0; grid%g_moist_bys = 0.0; grid%g_moist_bye = 0.0
   grid%g_tracer_bxs = 0.0; grid%g_tracer_bxe = 0.0; grid%g_tracer_bys = 0.0; grid%g_tracer_bye = 0.0
   grid%g_scalar_bxs = 0.0; grid%g_scalar_bxe = 0.0; grid%g_scalar_bys = 0.0; grid%g_scalar_bye = 0.0

   grid%g_u_btxs = 0.0; grid%g_u_btxe = 0.0; grid%g_u_btys = 0.0; grid%g_u_btye = 0.0
   grid%g_v_btxs = 0.0; grid%g_v_btxe = 0.0; grid%g_v_btys = 0.0; grid%g_v_btye = 0.0
   grid%g_w_btxs = 0.0; grid%g_w_btxe = 0.0; grid%g_w_btys = 0.0; grid%g_w_btye = 0.0
   grid%g_ph_btxs = 0.0; grid%g_ph_btxe = 0.0; grid%g_ph_btys = 0.0; grid%g_ph_btye = 0.0
   grid%g_t_btxs = 0.0; grid%g_t_btxe = 0.0; grid%g_t_btys = 0.0; grid%g_t_btye = 0.0
   grid%g_mu_btxs = 0.0; grid%g_mu_btxe = 0.0; grid%g_mu_btys = 0.0; grid%g_mu_btye = 0.0
   grid%g_moist_btxs = 0.0; grid%g_moist_btxe = 0.0; grid%g_moist_btys = 0.0; grid%g_moist_btye = 0.0
   grid%g_tracer_btxs = 0.0; grid%g_tracer_btxe = 0.0; grid%g_tracer_btys = 0.0; grid%g_tracer_btye = 0.0
   grid%g_scalar_btxs = 0.0; grid%g_scalar_btxe = 0.0; grid%g_scalar_btys = 0.0; grid%g_scalar_btye = 0.0

   RETURN
  END SUBROUTINE zero_out_tl

  SUBROUTINE inner_dot_a_b_adjtest ( grid, save, its, ite, jts, jte )
   TYPE (domain), INTENT(IN) :: grid
   INTEGER, INTENT(IN) :: its, ite, jts, jte
   REAL, INTENT(INOUT) :: save
   INTEGER :: i, k, j, mm
   INTEGER :: is, ie, js, je

   is = its
   ie = ite
   js = jts
   je = jte

   IF (its .eq. ids) is = ims
   IF (ite .eq. ide) ie = ime
   IF (jts .eq. jds) js = jms
   IF (jte .eq. jde) je = jme

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,ite
     save  = save + grid%a_u_2(i,k,j) * b_u_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,jte
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_v_2(i,k,j) * b_v_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_w_2(i,k,j) * b_w_2(i,k,j)    &
                  + grid%a_ph_2(i,k,j) * b_ph_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_t_2(i,k,j) * b_t_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_tke_2(i,k,j) * b_tke_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
      save  = save + grid%a_p(i,k,j) * b_p(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   !DO j = jts,MIN(jte,jde-1)
   !DO k = kts,kte-1
   !DO i = its,MIN(ite,ide-1)
   !  save  = save + grid%a_al(i,k,j) * b_al(i,k,j)
   !ENDDO
   !ENDDO
   !ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_h_diabatic(i,k,j) * b_h_diabatic(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_qv_diabatic(i,k,j) * b_qv_diabatic(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_qc_diabatic(i,k,j) * b_qc_diabatic(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   !DO j = jts,MIN(jte,jde-1)
   !DO i = its,MIN(ite,ide-1)
   !  save  = save + grid%a_ustm(i,j) * b_ustm(i,j)
   !ENDDO
   !ENDDO
 
   !DO j = jts,MIN(jte,jde-1)
   !DO i = its,MIN(ite,ide-1)
   !  save  = save + grid%a_hfx(i,j) * b_hfx(i,j)
   !ENDDO
   !ENDDO
 
   !DO j = jts,MIN(jte,jde-1)
   !DO i = its,MIN(ite,ide-1)
   !  save  = save + grid%a_qfx(i,j) * b_qfx(i,j)
   !ENDDO
   !ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_mu_2(i,j) * b_mu_2(i,j)
   ENDDO
   ENDDO

   DO mm = PARAM_FIRST_SCALAR, num_moist
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_moist(i,k,j,mm) * b_moist(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO
  
   DO mm = PARAM_FIRST_SCALAR, num_tracer
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_tracer(i,k,j,mm) * b_tracer(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO
  
   DO mm = 1, num_scalar
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_scalar(i,k,j,mm) * b_scalar(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   IF ( gwd_opt .EQ. 1 ) THEN
      DO j = jts,MIN(jte,jde-1)
      DO k = kts,kte-1
      DO i = its,MIN(ite,ide-1)
         save = save + grid%a_dtaux3d(i,k,j) * b_dtaux3d(i,k,j)
         save = save + grid%a_dtauy3d(i,k,j) * b_dtauy3d(i,k,j)
      ENDDO
      ENDDO
      ENDDO
      DO j = jts,MIN(jte,jde-1)
      DO i = its,MIN(ite,ide-1)
         save = save + grid%a_dusfcg(i,j) * b_dusfcg(i,j)
         save = save + grid%a_dvsfcg(i,j) * b_dvsfcg(i,j)
      ENDDO
      ENDDO
   ENDIF
  
   DO j=1,spec_bdy_width
   DO k=kts,kte-1
   DO i=jts,MIN(jte,jde-1)
      save=save + grid%a_u_bxs(i,k,j)*b_u_bxs(i,k,j)
      save=save + grid%a_u_bxe(i,k,j)*b_u_bxe(i,k,j)
      save=save + grid%a_u_btxs(i,k,j)*b_u_btxs(i,k,j)
      save=save + grid%a_u_btxe(i,k,j)*b_u_btxe(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j=1,spec_bdy_width
   DO k=kts,kte-1
   DO i=jts,jte
      save=save + grid%a_v_bxs(i,k,j)*b_v_bxs(i,k,j)
      save=save + grid%a_v_bxe(i,k,j)*b_v_bxe(i,k,j)
      save=save + grid%a_v_btxs(i,k,j)*b_v_btxs(i,k,j)
      save=save + grid%a_v_btxe(i,k,j)*b_v_btxe(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j=1,spec_bdy_width
   DO k=kts,kte-1
   DO i=jts,MIN(jte,jde-1)
      save=save + grid%a_t_bxs(i,k,j)*b_t_bxs(i,k,j)
      save=save + grid%a_t_bxe(i,k,j)*b_t_bxe(i,k,j)
      save=save + grid%a_t_btxs(i,k,j)*b_t_btxs(i,k,j)
      save=save + grid%a_t_btxe(i,k,j)*b_t_btxe(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j=1,spec_bdy_width
   DO k=kts,kte
   DO i=jts,MIN(jte,jde-1)
      save=save + grid%a_w_bxs(i,k,j)*b_w_bxs(i,k,j)
      save=save + grid%a_w_bxe(i,k,j)*b_w_bxe(i,k,j)
      save=save + grid%a_ph_bxs(i,k,j)*b_ph_bxs(i,k,j)
      save=save + grid%a_ph_bxe(i,k,j)*b_ph_bxe(i,k,j)
      save=save + grid%a_w_btxs(i,k,j)*b_w_btxs(i,k,j)
      save=save + grid%a_w_btxe(i,k,j)*b_w_btxe(i,k,j)


      save=save + grid%a_ph_btxs(i,k,j)*b_ph_btxs(i,k,j)
      save=save + grid%a_ph_btxe(i,k,j)*b_ph_btxe(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   !$OMP MASTER
   DO j=1,spec_bdy_width
   DO k=kts,kte-1
   DO i=its,ite
      save=save + grid%a_u_bys(i,k,j)*b_u_bys(i,k,j)
      save=save + grid%a_u_bye(i,k,j)*b_u_bye(i,k,j)
      save=save + grid%a_u_btys(i,k,j)*b_u_btys(i,k,j)
      save=save + grid%a_u_btye(i,k,j)*b_u_btye(i,k,j)
   ENDDO
   ENDDO
   ENDDO
   !$OMP END MASTER

   !$OMP MASTER
   DO j=1,spec_bdy_width
   DO k=kts,kte-1
   DO i=its,MIN(ite,ide-1)
      save=save + grid%a_v_bys(i,k,j)*b_v_bys(i,k,j)
      save=save + grid%a_v_bye(i,k,j)*b_v_bye(i,k,j)
      save=save + grid%a_v_btys(i,k,j)*b_v_btys(i,k,j)
      save=save + grid%a_v_btye(i,k,j)*b_v_btye(i,k,j)
   ENDDO
   ENDDO
   ENDDO
   !$OMP END MASTER

   !$OMP MASTER
   DO j=1,spec_bdy_width
   DO k=kts,kte-1
   DO i=its,MIN(ite,ide-1)
      save=save + grid%a_t_bys(i,k,j)*b_t_bys(i,k,j)
      save=save + grid%a_t_bye(i,k,j)*b_t_bye(i,k,j)
      save=save + grid%a_t_btys(i,k,j)*b_t_btys(i,k,j)
      save=save + grid%a_t_btye(i,k,j)*b_t_btye(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j=1,spec_bdy_width
   DO k=kts,kte
   DO i=its,MIN(ite,ide-1)
      save=save + grid%a_w_bys(i,k,j)*b_w_bys(i,k,j)
      save=save + grid%a_w_bye(i,k,j)*b_w_bye(i,k,j)
      save=save + grid%a_ph_bys(i,k,j)*b_ph_bys(i,k,j)
      save=save + grid%a_ph_bye(i,k,j)*b_ph_bye(i,k,j)
      save=save + grid%a_w_btys(i,k,j)*b_w_btys(i,k,j)
      save=save + grid%a_w_btye(i,k,j)*b_w_btye(i,k,j)
      save=save + grid%a_ph_btys(i,k,j)*b_ph_btys(i,k,j)
      save=save + grid%a_ph_btye(i,k,j)*b_ph_btye(i,k,j)
   ENDDO
   ENDDO
   ENDDO
   !$OMP END MASTER

   DO j=1,spec_bdy_width
   DO k=1,1
   DO i=jts,MIN(jte,jde-1)
      save=save + grid%a_mu_bxs(i,k,j)*b_mu_bxs(i,k,j)
      save=save + grid%a_mu_bxe(i,k,j)*b_mu_bxe(i,k,j)
      save=save + grid%a_mu_btxs(i,k,j)*b_mu_btxs(i,k,j)
      save=save + grid%a_mu_btxe(i,k,j)*b_mu_btxe(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   !$OMP MASTER
   DO j=1,spec_bdy_width
   DO k=1,1
   DO i=its,MIN(ite,ide-1)
      save=save + grid%a_mu_bys(i,k,j)*b_mu_bys(i,k,j)
      save=save + grid%a_mu_bye(i,k,j)*b_mu_bye(i,k,j)
      save=save + grid%a_mu_btys(i,k,j)*b_mu_btys(i,k,j)
      save=save + grid%a_mu_btye(i,k,j)*b_mu_btye(i,k,j)
   ENDDO
   ENDDO
   ENDDO
   !$OMP END MASTER

   DO mm=PARAM_FIRST_SCALAR, num_moist
   DO j=1,spec_bdy_width
   DO k=kts,kte-1
   DO i=jts,MIN(jte,jde-1)
      save=save + grid%a_moist_bxs(i,k,j,mm)*b_moist_bxs(i,k,j,mm)
      save=save + grid%a_moist_bxe(i,k,j,mm)*b_moist_bxe(i,k,j,mm)
      save=save + grid%a_moist_btxs(i,k,j,mm)*b_moist_btxs(i,k,j,mm)
      save=save + grid%a_moist_btxe(i,k,j,mm)*b_moist_btxe(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   !$OMP MASTER
   DO mm=PARAM_FIRST_SCALAR, num_moist
   DO j=1,spec_bdy_width
   DO k=kts,kte-1
   DO i=its,MIN(ite,ide-1)
      save=save + grid%a_moist_bys(i,k,j,mm)*b_moist_bys(i,k,j,mm)
      save=save + grid%a_moist_bye(i,k,j,mm)*b_moist_bye(i,k,j,mm)
      save=save + grid%a_moist_btys(i,k,j,mm)*b_moist_btys(i,k,j,mm)
      save=save + grid%a_moist_btye(i,k,j,mm)*b_moist_btye(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO
   !$OMP END MASTER

   DO mm=PARAM_FIRST_SCALAR, num_tracer
   DO j=1,spec_bdy_width
   DO k=kts,kte-1
   DO i=jts,MIN(jte,jde-1)
      save=save + grid%a_tracer_bxs(i,k,j,mm)*b_tracer_bxs(i,k,j,mm)
      save=save + grid%a_tracer_bxe(i,k,j,mm)*b_tracer_bxe(i,k,j,mm)
      save=save + grid%a_tracer_btxs(i,k,j,mm)*b_tracer_btxs(i,k,j,mm)
      save=save + grid%a_tracer_btxe(i,k,j,mm)*b_tracer_btxe(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   !$OMP MASTER
   DO mm=PARAM_FIRST_SCALAR, num_tracer
   DO j=1,spec_bdy_width
   DO k=kts,kte-1
   DO i=its,MIN(ite,ide-1)
      save=save + grid%a_tracer_bys(i,k,j,mm)*b_tracer_bys(i,k,j,mm)
      save=save + grid%a_tracer_bye(i,k,j,mm)*b_tracer_bye(i,k,j,mm)
      save=save + grid%a_tracer_btys(i,k,j,mm)*b_tracer_btys(i,k,j,mm)
      save=save + grid%a_tracer_btye(i,k,j,mm)*b_tracer_btye(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO
   !$OMP END MASTER

!  DO mm=1,num_scalar
!  DO j=1,spec_bdy_width
!  DO k=kds,kde
!  DO i=jms,jme
!     save=save + grid%a_scalar_bxs(i,k,j,mm)*b_scalar_bxs(i,k,j,mm)
!     save=save + grid%a_scalar_bxe(i,k,j,mm)*b_scalar_bxe(i,k,j,mm)
!     save=save + grid%a_scalar_btxs(i,k,j,mm)*b_scalar_btxs(i,k,j,mm)
!     save=save + grid%a_scalar_btxe(i,k,j,mm)*b_scalar_btxe(i,k,j,mm)
!  ENDDO
!  ENDDO
!  ENDDO
!  ENDDO

!  DO mm=1,num_scalar
!  DO j=1,spec_bdy_width
!  DO k=kds,kde
!  DO i=ims,ime
!     save=save + grid%a_scalar_bys(i,k,j,mm)*b_scalar_bys(i,k,j,mm)
!     save=save + grid%a_scalar_bye(i,k,j,mm)*b_scalar_bye(i,k,j,mm)
!     save=save + grid%a_scalar_btys(i,k,j,mm)*b_scalar_btys(i,k,j,mm)
!     save=save + grid%a_scalar_btye(i,k,j,mm)*b_scalar_btye(i,k,j,mm)
!  ENDDO
!  ENDDO
!  ENDDO
!  ENDDO

   RETURN
  END SUBROUTINE inner_dot_a_b_adjtest

  SUBROUTINE spot_force_ad ( grid, i, j, k, factor, nvars )
   TYPE (domain), INTENT(INOUT)       :: grid
   INTEGER, INTENT(IN)                :: i, j, k, nvars
   REAL, DIMENSION(nvars), INTENT(IN) :: factor
   INTEGER                            :: mm
   
   grid%a_u_2(i,k,j)  = factor(1)
   grid%a_v_2(i,k,j)  = factor(2)
   grid%a_w_2(i,k,j)  = factor(3)
   grid%a_t_2(i,k,j)  = factor(4)
   grid%a_ph_2(i,k,j) = factor(5)
   grid%a_mu_2(i,j)   = factor(6)

   DO mm = PARAM_FIRST_SCALAR, num_moist
      grid%a_moist(i,k,j,mm) = factor(6+mm)
   ENDDO
   DO mm = PARAM_FIRST_SCALAR, num_tracer
      grid%a_tracer(i,k,j,mm) = factor(6+mm+num_moist)
  ENDDO

  END SUBROUTINE spot_force_ad


  SUBROUTINE spot_force_tl ( grid, i, j, k, factor, nvars )
   TYPE (domain), INTENT(INOUT)       :: grid
   INTEGER, INTENT(IN)                :: i, j, k, nvars
   REAL, DIMENSION(nvars), INTENT(IN) :: factor
   INTEGER                            :: mm

   grid%g_u_2(i,k,j)  = factor(1)
   grid%g_v_2(i,k,j)  = factor(2)
   grid%g_w_2(i,k,j)  = factor(3)
   grid%g_t_2(i,k,j)  = factor(4)
   grid%g_ph_2(i,k,j) = factor(5)
   grid%g_mu_2(i,j)   = factor(6)

   DO mm = PARAM_FIRST_SCALAR, num_moist
      grid%g_moist(i,k,j,mm) = factor(6+mm)
   ENDDO
   DO mm = PARAM_FIRST_SCALAR, num_tracer
      grid%g_tracer(i,k,j,mm) = factor(6+mm+num_moist)
  ENDDO

  END SUBROUTINE spot_force_tl


  SUBROUTINE spot_force_nl ( grid, i, j, k, factor, nvars, nl_pert )
   TYPE (domain), INTENT(INOUT)       :: grid
   INTEGER, INTENT(IN)                :: i, j, k, nvars
   REAL, DIMENSION(nvars), INTENT(IN) :: factor
   REAL, INTENT(IN)                   :: nl_pert
   INTEGER                            :: mm

   grid%u_2(i,k,j)  = ( 1.0 + nl_pert * factor(1) ) * s_u_2(i,k,j)
   grid%v_2(i,k,j)  = ( 1.0 + nl_pert * factor(2) ) * s_v_2(i,k,j)
   grid%w_2(i,k,j)  = ( 1.0 + nl_pert * factor(3) ) * s_w_2(i,k,j)
   grid%t_2(i,k,j)  = ( 1.0 + nl_pert * factor(4) ) * s_t_2(i,k,j)
   grid%ph_2(i,k,j) = ( 1.0 + nl_pert * factor(5) ) * s_ph_2(i,k,j)
   grid%mu_2(i,j)   = ( 1.0 + nl_pert * factor(6) ) * s_mu_2(i,j)

   DO mm = PARAM_FIRST_SCALAR, num_moist
      grid%moist(i,k,j,mm) = ( 1.0 + nl_pert * factor(6+mm) ) * s_moist(i,k,j,mm)
   ENDDO
   DO mm = PARAM_FIRST_SCALAR, num_tracer
      grid%tracer(i,k,j,mm) = ( 1.0 + nl_pert * factor(6+mm+num_moist) ) * s_tracer(i,k,j,mm)
   ENDDO

  END SUBROUTINE spot_force_nl


  SUBROUTINE extract_ad_der ( grid, der, i, j , k, factor, nvars )
   TYPE (domain), INTENT(INOUT)       :: grid
   INTEGER, INTENT(IN)                :: i, j, k, nvars
   REAL, DIMENSION(nvars), INTENT(IN) :: factor
   REAL, INTENT(OUT)                  :: der
   INTEGER                            :: mm

   der = 0.0
   der = grid%a_u_2(i,k,j) * factor(1) + der
   der = grid%a_v_2(i,k,j) * factor(2) + der
   der = grid%a_w_2(i,k,j) * factor(3) + der
   der = grid%a_t_2(i,k,j) * factor(4) + der
   der = grid%a_ph_2(i,k,j) * factor(5) + der
   der = grid%a_mu_2(i,j) * factor(6) + der

   DO mm = PARAM_FIRST_SCALAR, num_moist
      der = grid%a_moist(i,k,j,mm) * factor(6+mm) + der
   ENDDO
   DO mm = PARAM_FIRST_SCALAR, num_tracer
      der = grid%a_tracer(i,k,j,mm) * factor(6+mm+num_moist) + der
   ENDDO

  END SUBROUTINE extract_ad_der


  SUBROUTINE extract_tl_der ( grid, der, i, j , k, factor, nvars )
   TYPE (domain), INTENT(INOUT)       :: grid
   INTEGER, INTENT(IN)                :: i, j, k, nvars
   REAL, DIMENSION(nvars), INTENT(IN) :: factor
   REAL, INTENT(OUT)                  :: der
   INTEGER                            :: mm
   der = 0.0
   der = grid%g_u_2(i,k,j) * factor(1) + der
   der = grid%g_v_2(i,k,j) * factor(2) + der
   der = grid%g_w_2(i,k,j) * factor(3) + der
   der = grid%g_t_2(i,k,j) * factor(4) + der
   der = grid%g_ph_2(i,k,j) * factor(5) + der
   der = grid%g_mu_2(i,j) * factor(6) + der

   DO mm = PARAM_FIRST_SCALAR, num_moist
      der = grid%g_moist(i,k,j,mm) * factor(6+mm) + der
   ENDDO
   DO mm = PARAM_FIRST_SCALAR, num_tracer
      der = grid%g_tracer(i,k,j,mm) * factor(6+mm+num_moist) + der
   ENDDO

  END SUBROUTINE extract_tl_der


  SUBROUTINE extract_nl_num ( grid, num, i, j , k, factor, nvars )
   TYPE (domain), INTENT(INOUT)       :: grid
   INTEGER, INTENT(IN)                :: i, j, k, nvars
   REAL, DIMENSION(nvars), INTENT(IN) :: factor
   REAL, INTENT(OUT)                  :: num
   INTEGER                            :: mm

   num = 0.0
   num = (grid%u_2(i,k,j) - b_u_2(i,k,j)) * factor(1) + num
   num = (grid%v_2(i,k,j) - b_v_2(i,k,j)) * factor(2) + num
   num = (grid%w_2(i,k,j) - b_w_2(i,k,j)) * factor(3) + num
   num = (grid%t_2(i,k,j) - b_t_2(i,k,j)) * factor(4) + num
   num = (grid%ph_2(i,k,j) - b_ph_2(i,k,j)) * factor(5) + num
   num = (grid%mu_2(i,j) - b_mu_2(i,j)) * factor(6) + num

   DO mm = PARAM_FIRST_SCALAR, num_moist
      num = (grid%moist(i,k,j,mm) - b_moist(i,k,j,mm)) * factor(6+mm) + num
   ENDDO
   DO mm = PARAM_FIRST_SCALAR, num_tracer
      num = (grid%tracer(i,k,j,mm) - b_tracer(i,k,j,mm)) * factor(6+mm+num_moist) + num
   ENDDO

  END SUBROUTINE extract_nl_num


  SUBROUTINE extract_nl_den ( den, i, j, k, factor, nvars, nl_pert )
   INTEGER, INTENT(IN)                :: i, j, k, nvars
   REAL, DIMENSION(nvars), INTENT(IN) :: factor
   REAL, INTENT(OUT)                  :: den
   REAL, INTENT(IN)                   :: nl_pert
   INTEGER                            :: mm

   den = 0.0
   den = nl_pert * s_u_2(i,k,j) * factor(1) + den
   den = nl_pert * s_v_2(i,k,j) * factor(2) + den
   den = nl_pert * s_w_2(i,k,j) * factor(3) + den
   den = nl_pert * s_t_2(i,k,j) * factor(4) + den
   den = nl_pert * s_ph_2(i,k,j) * factor(5) + den
   den = nl_pert * s_mu_2(i,j) * factor(6) + den

   DO mm = PARAM_FIRST_SCALAR, num_moist
      den = nl_pert * s_moist(i,k,j,mm) * factor(6+mm) + den
   ENDDO
   DO mm = PARAM_FIRST_SCALAR, num_tracer
      den = nl_pert * s_tracer(i,k,j,mm) * factor(6+mm+num_moist) + den
   ENDDO

  END SUBROUTINE extract_nl_den


  SUBROUTINE gen_scenario_matrix(config_flags, scenario, nvars, numer_vars, denom_vars)
   TYPE (grid_config_rec_type), INTENT(IN) :: config_flags
   INTEGER, INTENT(IN)                            :: nvars
   INTEGER, DIMENSION(nvars,nvars), INTENT(INOUT) :: scenario
   INTEGER, DIMENSION(max_sens), INTENT(IN)       :: numer_vars, denom_vars
   INTEGER                                        :: i, j, k, l

   IF(config_flags%scenario_type .EQ. 1) THEN
      !List form for test variables (more targeted)
      IF(config_flags%max_numer .NE. config_flags%max_denom) THEN
         CALL wrf_error_fatal ( &
              'share/module_adtl_grid_utilities.f: Failed gen_scenario' )
      ENDIF
      DO i = 1, config_flags%max_numer
         k = numer_vars(i)
         l = denom_vars(i)
         IF ( k .GT. nvars .OR. l .GT. nvars) CYCLE
         scenario(k,l) = 1
      ENDDO
   ELSE
      !Combination form for test variables (only use when you want to cross-reference every variable in the lists)
      DO i = 1, config_flags%max_numer
         k = numer_vars(i)
         IF ( k .GT. nvars ) CYCLE
         DO j = 1, config_flags%max_denom
            l = denom_vars(j)
            IF ( l .GT. nvars ) CYCLE
            scenario(k,l) = 1
         ENDDO
      ENDDO
   ENDIF


  END SUBROUTINE gen_scenario_matrix

  SUBROUTINE allocate_locations( filename, locations, nloc, ierr )
   INTEGER, EXTERNAL :: get_unused_unit

   CHARACTER(LEN=*), INTENT(IN) :: filename
   INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: locations
   INTEGER, INTENT(OUT) :: nloc, ierr
   CHARACTER*256 :: message
   INTEGER :: loc, iunit, index1, index2, index3

   iunit = get_unused_unit()
   OPEN(UNIT=iunit, &
        FILE=TRIM(filename), &
        FORM='FORMATTED', &
        ACCESS='SEQUENTIAL', &
        IOSTAT=ierr)

   IF( ierr .NE. 0 ) THEN
      WRITE(UNIT=message, FMT='(3(A))') &
          'Error reading file ',filename,' in allocate_locations.'
      CALL wrf_message(TRIM(message))
      CLOSE(UNIT=iunit)
      RETURN
   ENDIF
   loc = 0
   points: DO
      loc = loc + 1
      READ(UNIT=iunit, FMT='(3(I6))',IOSTAT=ierr) index1, index2, index3
      IF( ierr .NE. 0 ) exit points
      nloc = loc
   END DO points
   CLOSE(UNIT=iunit)

   IF( ierr .LT. 0 ) ALLOCATE(locations(nloc,3))

  END SUBROUTINE allocate_locations


  SUBROUTINE get_forc_locations( filename, locations, nloc, ierr )
   INTEGER, EXTERNAL :: get_unused_unit

   CHARACTER(LEN=*), INTENT(IN) :: filename
   INTEGER, DIMENSION(nloc,3), INTENT(INOUT) :: locations
   INTEGER, INTENT(IN) :: nloc
   INTEGER, INTENT(OUT) :: ierr
   CHARACTER*256 :: message
   INTEGER :: loc, iunit, index1, index2, index3

   iunit = get_unused_unit()
   OPEN(UNIT=iunit, &
        FILE=TRIM(filename), &
        FORM='FORMATTED', &
        ACCESS='SEQUENTIAL', &
        IOSTAT=ierr)

   IF( ierr .NE. 0 ) THEN
      WRITE(UNIT=message, FMT='(3(A))') &
          'Error reading file ',filename,' in get_forc_locations.'
      CALL wrf_message(TRIM(message))
      CLOSE(UNIT=iunit)
      RETURN
   ENDIF
   loc = 0
   points: DO    
      loc = loc + 1
      READ(UNIT=iunit, FMT='(3(I6))',IOSTAT=ierr) index1, index2, index3
      IF( ierr .NE. 0 ) exit points
      locations(loc,1) = index1
      locations(loc,2) = index2
      locations(loc,3) = index3
   END DO points
   CLOSE(UNIT=iunit)

  END SUBROUTINE get_forc_locations

  SUBROUTINE deallocate_grid ( )
   INTEGER :: ierr

   DEALLOCATE ( s_scalar, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_scalar. ')
   ENDIF
   DEALLOCATE ( s_tke_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_tke_2. ')
   ENDIF

   DEALLOCATE ( s_p, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_p. ')
   ENDIF
   DEALLOCATE ( s_al, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_al. ')
   ENDIF
   DEALLOCATE ( s_h_diabatic, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_h_diabatic. ')
   ENDIF
   DEALLOCATE ( s_qv_diabatic, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_qv_diabatic. ')
   ENDIF
   DEALLOCATE ( s_qc_diabatic, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_qc_diabatic. ')
   ENDIF

   DEALLOCATE ( s_ustm, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ustm. ')
   ENDIF
   DEALLOCATE ( s_hfx, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_hfx. ')
   ENDIF
   DEALLOCATE ( s_qfx, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_qfx. ')
   ENDIF

   IF ( gwd_opt .EQ. 1 ) THEN

   DEALLOCATE ( s_dtaux3d, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_dtaux3d. ')
   ENDIF
   DEALLOCATE ( s_dtauy3d, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_dtauy3d. ')
   ENDIF
   DEALLOCATE ( s_dusfcg, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_dusfcg. ')
   ENDIF
   DEALLOCATE ( s_dvsfcg, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_dvsfcg. ')
   ENDIF

   ENDIF

   !  Scalar Boundary
   DEALLOCATE(s_scalar_bxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_scalar_bxs. ')
   ENDIF
   DEALLOCATE(s_scalar_bxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_scalar_bxe. ')
   ENDIF
   DEALLOCATE(s_scalar_bys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_scalar_bys. ')
   ENDIF
   DEALLOCATE(s_scalar_bye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_scalar_bye. ')
   ENDIF
   DEALLOCATE(s_scalar_btxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_scalar_btxs. ')
   ENDIF
   DEALLOCATE(s_scalar_btxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_scalar_btxe. ')
   ENDIF
   DEALLOCATE(s_scalar_btys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_scalar_btys. ')
   ENDIF
   DEALLOCATE(s_scalar_btye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_scalar_btye. ')
   ENDIF

   DEALLOCATE ( s_u_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_2. ')
   ENDIF
   DEALLOCATE ( s_v_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_v_2. ')
   ENDIF
   DEALLOCATE ( s_w_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_w_2. ')
   ENDIF
   DEALLOCATE ( s_ph_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ph_2. ')
   ENDIF
   DEALLOCATE ( s_t_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_t_2. ')
   ENDIF
   DEALLOCATE ( s_mu_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_mu_2. ')
   ENDIF
   DEALLOCATE ( s_moist, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_moist. ')
   ENDIF
   DEALLOCATE ( s_tracer, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_tracer. ')
   ENDIF

   !  U Boundary
   DEALLOCATE(s_u_bxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_bxs. ')
   ENDIF
   DEALLOCATE(s_u_bxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_bxe. ')
   ENDIF
   DEALLOCATE(s_u_bys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_bys. ')
   ENDIF
   DEALLOCATE(s_u_bye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_bye. ')
   ENDIF
   DEALLOCATE(s_u_btxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_btxs. ')
   ENDIF
   DEALLOCATE(s_u_btxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_btxe. ')
   ENDIF
   DEALLOCATE(s_u_btys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_btys. ')
   ENDIF
   DEALLOCATE(s_u_btye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_btye. ')
   ENDIF

   !  V Boundary
   DEALLOCATE(s_v_bxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_bxs. ')
   ENDIF
   DEALLOCATE(s_v_bxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_bxe. ')
   ENDIF
   DEALLOCATE(s_v_bys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_bys. ')
   ENDIF
   DEALLOCATE(s_v_bye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_bye. ')
   ENDIF
   DEALLOCATE(s_v_btxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_btxs. ')
   ENDIF
   DEALLOCATE(s_v_btxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_btxe. ')
   ENDIF
   DEALLOCATE(s_v_btys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_btys. ')
   ENDIF
   DEALLOCATE(s_v_btye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_btye. ')
   ENDIF

   !  T Boundary
   DEALLOCATE(s_t_bxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_t_bxs. ')
   ENDIF
   DEALLOCATE(s_t_bxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_t_bxe. ')
   ENDIF
   DEALLOCATE(s_t_bys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_t_bys. ')
   ENDIF
   DEALLOCATE(s_t_bye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_t_bye. ')
   ENDIF
   DEALLOCATE(s_t_btxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_t_btxs. ')
   ENDIF
   DEALLOCATE(s_t_btxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_t_btxe. ')
   ENDIF
   DEALLOCATE(s_t_btys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_t_btys. ')
   ENDIF
   DEALLOCATE(s_t_btye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_t_btye. ')
   ENDIF

   !  W Boundary
   DEALLOCATE(s_w_bxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_w_bxs. ')
   ENDIF
   DEALLOCATE(s_w_bxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_w_bxe. ')
   ENDIF
   DEALLOCATE(s_w_bys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_w_bys. ')
   ENDIF
   DEALLOCATE(s_w_bye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_w_bye. ')
   ENDIF
   DEALLOCATE(s_w_btxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_w_btxs. ')
   ENDIF
   DEALLOCATE(s_w_btxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_w_btxe. ')
   ENDIF
   DEALLOCATE(s_w_btys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_w_btys. ')
   ENDIF
   DEALLOCATE(s_w_btye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_w_btye. ')
   ENDIF

   !  PH Boundary
   DEALLOCATE(s_ph_bxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ph_bxs. ')
   ENDIF
   DEALLOCATE(s_ph_bxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ph_bxe. ')
   ENDIF
   DEALLOCATE(s_ph_bys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ph_bys. ')
   ENDIF
   DEALLOCATE(s_ph_bye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ph_bye. ')
   ENDIF
   DEALLOCATE(s_ph_btxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ph_btxs. ')
   ENDIF
   DEALLOCATE(s_ph_btxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ph_btxe. ')
   ENDIF
   DEALLOCATE(s_ph_btys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ph_btys. ')
   ENDIF
   DEALLOCATE(s_ph_btye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ph_btye. ')
   ENDIF

   !  MU Boundary
   DEALLOCATE(s_mu_bxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_mu_bxs. ')
   ENDIF
   DEALLOCATE(s_mu_bxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_mu_bxe. ')
   ENDIF
   DEALLOCATE(s_mu_bys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_mu_bys. ')
   ENDIF
   DEALLOCATE(s_mu_bye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_mu_bye. ')
   ENDIF
   DEALLOCATE(s_mu_btxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_mu_btxs. ')
   ENDIF
   DEALLOCATE(s_mu_btxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_mu_btxe. ')
   ENDIF
   DEALLOCATE(s_mu_btys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_mu_btys. ')
   ENDIF
   DEALLOCATE(s_mu_btye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_mu_btye. ')
   ENDIF

   !  Moist Boundary
   DEALLOCATE(s_moist_bxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_moist_bxs. ')
   ENDIF
   DEALLOCATE(s_moist_bxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_moist_bxe. ')
   ENDIF
   DEALLOCATE(s_moist_bys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_moist_bys. ')
   ENDIF
   DEALLOCATE(s_moist_bye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_moist_bye. ')
   ENDIF
   DEALLOCATE(s_moist_btxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_moist_btxs. ')
   ENDIF
   DEALLOCATE(s_moist_btxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_moist_btxe. ')
   ENDIF
   DEALLOCATE(s_moist_btys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_moist_btys. ')
   ENDIF
   DEALLOCATE(s_moist_btye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_moist_btye. ')
   ENDIF

   !  Tracer Boundary
   DEALLOCATE(s_tracer_bxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_tracer_bxs. ')
   ENDIF
   DEALLOCATE(s_tracer_bxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_tracer_bxe. ')
   ENDIF
   DEALLOCATE(s_tracer_bys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_tracer_bys. ')
   ENDIF
   DEALLOCATE(s_tracer_bye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_tracer_bye. ')
   ENDIF
   DEALLOCATE(s_tracer_btxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_tracer_btxs. ')
   ENDIF
   DEALLOCATE(s_tracer_btxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_tracer_btxe. ')
   ENDIF
   DEALLOCATE(s_tracer_btys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_tracer_btys. ')
   ENDIF
   DEALLOCATE(s_tracer_btye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_tracer_btye. ')
   ENDIF

   DEALLOCATE ( b_scalar, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_scalar. ')
   ENDIF
   DEALLOCATE ( b_tke_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_tke_2. ')
   ENDIF

   DEALLOCATE ( b_p, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_p. ')
   ENDIF
   DEALLOCATE ( b_al, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_al. ')
   ENDIF
   DEALLOCATE ( b_h_diabatic, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_h_diabatic. ')
   ENDIF
   DEALLOCATE ( b_qv_diabatic, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_qv_diabatic. ')
   ENDIF
   DEALLOCATE ( b_qc_diabatic, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_qc_diabatic. ')
   ENDIF

   DEALLOCATE ( b_ustm, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_ustm. ')
   ENDIF
   DEALLOCATE ( b_hfx, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_hfx. ')
   ENDIF
   DEALLOCATE ( b_qfx, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_qfx. ')
   ENDIF

   IF ( gwd_opt .EQ. 1 ) THEN

   DEALLOCATE ( b_dtaux3d, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_dtaux3d. ')
   ENDIF
   DEALLOCATE ( b_dtauy3d, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_dtauy3d. ')
   ENDIF
   DEALLOCATE ( b_dusfcg, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_dusfcg. ')
   ENDIF
   DEALLOCATE ( b_dvsfcg, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_dvsfcg. ')
   ENDIF

   ENDIF

   !  Scalar Boundary
   DEALLOCATE(b_scalar_bxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_scalar_bxs. ')
   ENDIF
   DEALLOCATE(b_scalar_bxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_scalar_bxe. ')
   ENDIF
   DEALLOCATE(b_scalar_bys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_scalar_bys. ')
   ENDIF
   DEALLOCATE(b_scalar_bye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_scalar_bye. ')
   ENDIF
   DEALLOCATE(b_scalar_btxs,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_scalar_btxs. ')
   ENDIF
   DEALLOCATE(b_scalar_btxe,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_scalar_btxe. ')
   ENDIF
   DEALLOCATE(b_scalar_btys,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_scalar_btys. ')
   ENDIF
   DEALLOCATE(b_scalar_btye,STAT=ierr) 
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_scalar_btye. ')
   ENDIF

   DEALLOCATE ( b_u_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_u_2. ')
   ENDIF
   DEALLOCATE ( b_v_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_v_2. ')
   ENDIF
   DEALLOCATE ( b_w_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_w_2. ')
   ENDIF
   DEALLOCATE ( b_ph_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_ph_2. ')
   ENDIF
   DEALLOCATE ( b_t_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_t_2. ')
   ENDIF
   DEALLOCATE ( b_mu_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_mu_2. ')
   ENDIF
   DEALLOCATE ( b_moist, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_moist. ')
   ENDIF
   DEALLOCATE ( b_tracer, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_tracer. ')
   ENDIF
   DEALLOCATE ( b_rainnc, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_rainnc. ')
   ENDIF
   DEALLOCATE ( b_rainncv, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_rainncv. ')
   ENDIF
   DEALLOCATE ( b_rainc, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_rainc. ')
   ENDIF
   DEALLOCATE ( b_raincv, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_raincv. ')
   ENDIF

   RETURN
  END SUBROUTINE deallocate_grid

END MODULE module_adtl_grid_utilities

