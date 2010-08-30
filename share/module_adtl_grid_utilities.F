!WRF:MEDIATION_LAYER:adtl_grid_utilities
!

MODULE module_adtl_grid_utilities
USE module_domain, ONLY : domain, get_ijk_from_grid
USE module_configure, ONLY : num_moist, grid_config_rec_type
USE module_tiles, ONLY : set_tiles

REAL, ALLOCATABLE :: s_u_1(:,:,:), s_u_2(:,:,:), s_v_1(:,:,:), s_v_2(:,:,:), s_w_1(:,:,:), s_w_2(:,:,:)
REAL, ALLOCATABLE :: s_ph_1(:,:,:), s_ph_2(:,:,:), s_t_1(:,:,:), s_t_2(:,:,:), s_mu_1(:,:), s_mu_2(:,:)
REAL, ALLOCATABLE :: s_moist(:,:,:,:)

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

REAL, ALLOCATABLE :: b_u_1(:,:,:), b_u_2(:,:,:), b_v_1(:,:,:), b_v_2(:,:,:), b_w_1(:,:,:), b_w_2(:,:,:)
REAL, ALLOCATABLE :: b_ph_1(:,:,:), b_ph_2(:,:,:), b_t_1(:,:,:), b_t_2(:,:,:), b_mu_1(:,:), b_mu_2(:,:)
REAL, ALLOCATABLE :: b_moist(:,:,:,:)

INTEGER :: sm31, em31, sm32, em32, sm33, em33, spec_bdy_width, kts, kte

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

  END SUBROUTINE init_domain_size

  SUBROUTINE allocate_grid ( )

   ALLOCATE ( s_u_1(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_1(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_u_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_u_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_v_1(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_v_1(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_v_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_v_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_w_1(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_w_1(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_w_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_w_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_ph_1(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ph_1(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_ph_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_ph_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_t_1(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_t_1(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_t_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_t_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( s_mu_1(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate s_mu_1(sm31:em31, sm33:em33). ')
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


   ALLOCATE ( b_u_1(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_1(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_u_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_u_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_v_1(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_v_1(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_v_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_v_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_w_1(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_w_1(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_w_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_w_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_ph_1(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ph_1(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_ph_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_ph_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_t_1(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_t_1(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_t_2(sm31:em31, sm32:em32, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_t_2(sm31:em31, sm32:em32, sm33:em33). ')
   ENDIF
   ALLOCATE ( b_mu_1(sm31:em31, sm33:em33), STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to allocate b_mu_1(sm31:em31, sm33:em33). ')
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

   RETURN
  END SUBROUTINE allocate_grid

  SUBROUTINE copy_grid_to_s ( grid )
   TYPE (domain), INTENT(IN) :: grid

   s_u_1 = grid%u_1; s_u_2 = grid%u_2
   s_v_1 = grid%v_1; s_v_2 = grid%v_2
   s_w_1 = grid%w_1; s_w_2 = grid%w_2
   s_ph_1 = grid%ph_1; s_ph_2 = grid%ph_2
   s_t_1 = grid%t_1; s_t_2 = grid%t_2
   s_mu_1 = grid%mu_1; s_mu_2 = grid%mu_2
   s_moist = grid%moist
   
   s_u_bxs = grid%u_bxs; s_u_bxe = grid%u_bxe; s_u_bys = grid%u_bys; s_u_bye = grid%u_bye
   s_u_btxs = grid%u_btxs; s_u_btxe = grid%u_btxe; s_u_btys = grid%u_btys; s_u_btye = grid%u_btye
   s_v_bxs = grid%v_bxs; s_v_bxe = grid%v_bxe; s_v_bys =grid%v_bys; s_v_bye=grid%v_bye
   s_v_btxs = grid%v_btxs; s_v_btxe = grid%v_btxe; s_v_btys =grid%v_btys; s_v_btye=grid%v_btye
   s_w_bxs = grid%w_bxs; s_w_bxe = grid%w_bxe; s_w_bys = grid%w_bys; s_w_bye = grid%w_bye
   s_w_btxs = grid%w_btxs; s_w_btxe = grid%w_btxe; s_w_btys = grid%w_btys; s_w_btye = grid%w_btye
   s_ph_bxs = grid%ph_bxs; s_ph_bxe = grid%ph_bxe; s_ph_bys = grid%ph_bys; s_ph_bye = grid%ph_bye
   s_ph_btxs = grid%ph_btxs; s_ph_btxe = grid%ph_btxe; s_ph_btys = grid%ph_btys; s_ph_bye = grid%ph_btye
   s_t_bxs = grid%t_bxs; s_t_bxe = grid%t_bxe; s_t_bys = grid%t_bys; s_t_bye = grid%t_bye
   s_t_btxs = grid%t_btxs; s_t_btxe = grid%t_btxe; s_t_btys = grid%t_btys; s_t_btye = grid%t_btye
   s_mu_bxs = grid%mu_bxs; s_mu_bxe = grid%mu_bxe; s_mu_bys = grid%mu_bys; s_mu_bye = grid%mu_bye
   s_mu_btxs = grid%mu_btxs; s_mu_btxe = grid%mu_btxe; s_mu_btys = grid%mu_btys; s_mu_btye = grid%mu_btye
   s_moist_bxs = grid%moist_bxs; s_moist_bxe = grid%moist_bxe; s_moist_bys = grid%moist_bys; s_moist_bye = grid%moist_bye
   s_moist_btxs = grid%moist_btxs; s_moist_btxe = grid%moist_btxe; s_moist_btys = grid%moist_btys; s_moist_btye = grid%moist_btye
   
   RETURN
  END SUBROUTINE copy_grid_to_s

  SUBROUTINE copy_grid_to_b ( grid )
   TYPE (domain), INTENT(IN) :: grid

   b_u_1 = grid%u_1; b_u_2 = grid%u_2
   b_v_1 = grid%v_1; b_v_2 = grid%v_2
   b_w_1 = grid%w_1; b_w_2 = grid%w_2
   b_ph_1 = grid%ph_1; b_ph_2 = grid%ph_2
   b_t_1 = grid%t_1; b_t_2 = grid%t_2
   b_mu_1 = grid%mu_1; b_mu_2 = grid%mu_2
   b_moist = grid%moist
   
   RETURN
  END SUBROUTINE copy_grid_to_b

  SUBROUTINE restore_grid ( grid )
   TYPE (domain), INTENT(IN) :: grid

   grid%u_1 = s_u_1; grid%u_2 = s_u_2
   grid%v_1 = s_v_1; grid%v_2 = s_v_2
   grid%w_1 = s_w_1; grid%w_2 = s_w_2
   grid%ph_1 = s_ph_1; grid%ph_2 = s_ph_2
   grid%t_1 = s_t_1; grid%t_2 = s_t_2
   grid%mu_1 = s_mu_1; grid%mu_2 = s_mu_2
   grid%moist = s_moist

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

  END SUBROUTINE restore_grid

  SUBROUTINE copy_s_to_g ( grid, factor )
   TYPE (domain), INTENT(IN) :: grid
   REAL, INTENT(IN) :: factor
   REAL :: factor_u, factor_v, factor_w, factor_ph, factor_t, factor_mu, factor_moist
   
   factor_u =0.0
   factor_v =0.0
   factor_w =0.0
   factor_ph =0.0
   factor_t =0.0
   factor_mu =0.0
   factor_moist =0.0

   IF ( grid%check_u )     factor_u = factor
   IF ( grid%check_v )     factor_v = factor
   IF ( grid%check_w )     factor_w = factor
   IF ( grid%check_ph )    factor_ph = factor
   IF ( grid%check_t )     factor_t = factor
   IF ( grid%check_mu )    factor_mu = factor
   IF ( grid%check_moist ) factor_moist = factor

   grid%g_u_1 = factor_u*s_u_1; grid%g_u_2 = factor_u*s_u_2
   grid%g_v_1 = factor_v*s_v_1; grid%g_v_2 = factor_v*s_v_2
   grid%g_w_1 = factor_w*s_w_1; grid%g_w_2 = factor_w*s_w_2
   grid%g_ph_1 = factor_ph*s_ph_1; grid%g_ph_2 = factor_ph*s_ph_2
   grid%g_t_1 = factor_t*s_t_1; grid%g_t_2 = factor_t*s_t_2
   grid%g_mu_1 = factor_mu*s_mu_1; grid%g_mu_2 = factor_mu*s_mu_2
   grid%g_moist = factor_moist*s_moist
   
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

   RETURN
  END SUBROUTINE copy_s_to_g

  SUBROUTINE copy_g_to_b ( grid )
   TYPE (domain), INTENT(IN) :: grid

   b_u_1 = grid%g_u_1 ; b_u_2 = grid%g_u_2
   b_v_1 = grid%g_v_1 ; b_v_2 = grid%g_v_2 
   b_w_1 = grid%g_w_1 ; b_w_2 = grid%g_w_2
   b_ph_1 = grid%g_ph_1 ; b_ph_2 = grid%g_ph_2
   b_t_1 = grid%g_t_1 ; b_t_2 = grid%g_t_2
   b_mu_1 = grid%g_mu_1 ; b_mu_2 = grid%g_mu_2
   b_moist = grid%g_moist
   
   RETURN
  END SUBROUTINE copy_g_to_b

  SUBROUTINE inner_dot_g ( grid, save, its, ite, jts, jte )
   TYPE (domain), INTENT(IN) :: grid
   REAL, INTENT(INOUT) :: save
   INTEGER, INTENT(IN) :: its, ite, jts, jte
   INTEGER :: i, k, j, mm

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,ite
     save  = save + grid%g_u_1(i,k,j) * grid%g_u_1(i,k,j)    &
                  + grid%g_u_2(i,k,j) * grid%g_u_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,jte
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_v_1(i,k,j) * grid%g_v_1(i,k,j)    &
                  + grid%g_v_2(i,k,j) * grid%g_v_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_w_1(i,k,j) * grid%g_w_1(i,k,j)    &
                  + grid%g_w_2(i,k,j) * grid%g_w_2(i,k,j)    &
                  + grid%g_ph_1(i,k,j) * grid%g_ph_1(i,k,j)  & 
                  + grid%g_ph_2(i,k,j) * grid%g_ph_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_t_1(i,k,j) * grid%g_t_1(i,k,j)    &
                  + grid%g_t_2(i,k,j) * grid%g_t_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO


   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_mu_1(i,j) * grid%g_mu_1(i,j)    &
                  + grid%g_mu_2(i,j) * grid%g_mu_2(i,j)
   ENDDO
   ENDDO

   DO mm = 1, num_moist
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%g_moist(i,k,j,mm) * grid%g_moist(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO
  

   RETURN
  END SUBROUTINE inner_dot_g

  SUBROUTINE add_grid ( grid, factor )
   TYPE (domain), INTENT(INOUT) :: grid
   REAL, INTENT(IN) :: factor
   REAL :: factor_u, factor_v, factor_w, factor_ph, factor_t, factor_mu, factor_moist
   
   factor_u =1.0
   factor_v =1.0
   factor_w =1.0
   factor_ph =1.0
   factor_t =1.0
   factor_mu =1.0
   factor_moist =1.0

   IF ( grid%check_u )     factor_u = factor
   IF ( grid%check_v )     factor_v = factor
   IF ( grid%check_w )     factor_w = factor
   IF ( grid%check_ph )    factor_ph = factor
   IF ( grid%check_t )     factor_t = factor
   IF ( grid%check_mu )    factor_mu = factor
   IF ( grid%check_moist ) factor_moist = factor

   grid%u_1 = factor_u*s_u_1; grid%u_2 = factor_u*s_u_2
   grid%v_1 = factor_v*s_v_1; grid%v_2 = factor_v*s_v_2
   grid%w_1 = factor_w*s_w_1; grid%w_2 = factor_w*s_w_2
   grid%ph_1 = factor_ph*s_ph_1; grid%ph_2 = factor_ph*s_ph_2
   grid%t_1 = factor_t*s_t_1; grid%t_2 = factor_t*s_t_2
   grid%mu_1 = factor_mu*s_mu_1; grid%mu_2 = factor_mu*s_mu_2
   grid%moist = factor_moist*s_moist
   
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
     save  = save + (grid%u_1(i,k,j)-b_u_1(i,k,j)) * (grid%u_1(i,k,j)-b_u_1(i,k,j))    &
                  + (grid%u_2(i,k,j)-b_u_2(i,k,j)) * (grid%u_2(i,k,j)-b_u_2(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,jte
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + (grid%v_1(i,k,j)-b_v_1(i,k,j)) * (grid%v_1(i,k,j)-b_v_1(i,k,j))    &
                  + (grid%v_2(i,k,j)-b_v_2(i,k,j)) * (grid%v_2(i,k,j)-b_v_2(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     save  = save + (grid%w_1(i,k,j)-b_w_1(i,k,j)) * (grid%w_1(i,k,j)-b_w_1(i,k,j))    &
                  + (grid%w_2(i,k,j)-b_w_2(i,k,j)) * (grid%w_2(i,k,j)-b_w_2(i,k,j))    &
                  + (grid%ph_1(i,k,j)-b_ph_1(i,k,j)) * (grid%ph_1(i,k,j)-b_ph_1(i,k,j))  & 
                  + (grid%ph_2(i,k,j)-b_ph_2(i,k,j)) * (grid%ph_2(i,k,j)-b_ph_2(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + (grid%t_1(i,k,j)-b_t_1(i,k,j)) * (grid%t_1(i,k,j)-b_t_1(i,k,j))    &
                  + (grid%t_2(i,k,j)-b_t_2(i,k,j)) * (grid%t_2(i,k,j)-b_t_2(i,k,j))
   ENDDO
   ENDDO
   ENDDO


   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save + (grid%mu_1(i,j)-b_mu_1(i,j)) * (grid%mu_1(i,j)-b_mu_1(i,j))    &
                  + (grid%mu_2(i,j)-b_mu_2(i,j)) * (grid%mu_2(i,j)-b_mu_2(i,j))
   ENDDO
   ENDDO

   DO mm = 1, num_moist
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + (grid%moist(i,k,j,mm)-b_moist(i,k,j,mm)) * (grid%moist(i,k,j,mm)-b_moist(i,k,j,mm))
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   RETURN
  END SUBROUTINE inner_dot

  SUBROUTINE copy_g_to_a ( grid )
   TYPE (domain), INTENT(IN) :: grid

   grid%a_u_1 = grid%g_u_1 ; grid%a_u_2 = grid%g_u_2
   grid%a_v_1 = grid%g_v_1 ; grid%a_v_2 = grid%g_v_2 
   grid%a_w_1 = grid%g_w_1 ; grid%a_w_2 = grid%g_w_2
   grid%a_ph_1 = grid%g_ph_1 ; grid%a_ph_2 = grid%g_ph_2
   grid%a_t_1 = grid%g_t_1 ; grid%a_t_2 = grid%g_t_2
   grid%a_mu_1 = grid%g_mu_1 ; grid%a_mu_2 = grid%g_mu_2
   grid%a_moist = grid%g_moist
   
   grid%a_u_bxs = grid%g_u_bxs; grid%a_u_bxe = grid%g_u_bxe; grid%a_u_bys = grid%g_u_bys; grid%a_u_bye = grid%g_u_bye
   grid%a_u_btxs = grid%g_u_btxs; grid%a_u_btxe = grid%g_u_btxe; grid%a_u_btys = grid%g_u_btys; grid%a_u_btye = grid%g_u_btye
   grid%a_v_bxs = grid%g_v_bxs; grid%a_v_bxe = grid%g_v_bxe; grid%a_v_bys =grid%g_v_bys; grid%a_v_bye=grid%g_v_bye
   grid%a_v_btxs = grid%g_v_btxs; grid%a_v_btxe = grid%g_v_btxe; grid%a_v_btys =grid%g_v_btys; grid%a_v_btye=grid%g_v_btye
   grid%a_w_bxs = grid%g_w_bxs; grid%a_w_bxe = grid%g_w_bxe; grid%a_w_bys = grid%g_w_bys; grid%a_w_bye = grid%g_w_bye
   grid%a_w_btxs = grid%g_w_btxs; grid%a_w_btxe = grid%g_w_btxe; grid%a_w_btys = grid%g_w_btys; grid%a_w_btye = grid%g_w_btye
   grid%a_ph_bxs = grid%g_ph_bxs; grid%a_ph_bxe = grid%g_ph_bxe; grid%a_ph_bys = grid%g_ph_bys; grid%a_ph_bye = grid%g_ph_bye
   grid%a_ph_btxs = grid%g_ph_btxs; grid%a_ph_btxe = grid%g_ph_btxe; grid%a_ph_btys = grid%g_ph_btys; grid%a_ph_bye = grid%g_ph_btye
   grid%a_t_bxs = grid%g_t_bxs; grid%a_t_bxe = grid%g_t_bxe; grid%a_t_bys = grid%g_t_bys; grid%a_t_bye = grid%g_t_bye
   grid%a_t_btxs = grid%g_t_btxs; grid%a_t_btxe = grid%g_t_btxe; grid%a_t_btys = grid%g_t_btys; grid%a_t_btye = grid%g_t_btye
   grid%a_mu_bxs = grid%g_mu_bxs; grid%a_mu_bxe = grid%g_mu_bxe; grid%a_mu_bys = grid%g_mu_bys; grid%a_mu_bye = grid%g_mu_bye
   grid%a_mu_btxs = grid%g_mu_btxs; grid%a_mu_btxe = grid%g_mu_btxe; grid%a_mu_btys = grid%g_mu_btys; grid%a_mu_btye = grid%g_mu_btye
   grid%a_moist_bxs = grid%g_moist_bxs; grid%a_moist_bxe = grid%g_moist_bxe; grid%a_moist_bys = grid%g_moist_bys; grid%a_moist_bye = grid%g_moist_bye
   grid%a_moist_btxs = grid%g_moist_btxs; grid%a_moist_btxe = grid%g_moist_btxe; grid%a_moist_btys = grid%g_moist_btys; grid%a_moist_btye = grid%g_moist_btye

   RETURN
  END SUBROUTINE copy_g_to_a

  SUBROUTINE inner_dot_a_b ( grid, save, its, ite, jts, jte )
   TYPE (domain), INTENT(IN) :: grid
   INTEGER, INTENT(IN) :: its, ite, jts, jte
   REAL, INTENT(INOUT) :: save
   INTEGER :: i, k, j, mm

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,ite
     save  = save + grid%a_u_1(i,k,j) * b_u_1(i,k,j)    &
                  + grid%a_u_2(i,k,j) * b_u_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,jte
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_v_1(i,k,j) * b_v_1(i,k,j)    &
                  + grid%a_v_2(i,k,j) * b_v_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_w_1(i,k,j) * b_w_1(i,k,j)    &
                  + grid%a_w_2(i,k,j) * b_w_2(i,k,j)    &
                  + grid%a_ph_1(i,k,j) * b_ph_1(i,k,j)  & 
                  + grid%a_ph_2(i,k,j) * b_ph_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_t_1(i,k,j) * b_t_1(i,k,j)    &
                  + grid%a_t_2(i,k,j) * b_t_2(i,k,j)
   ENDDO
   ENDDO
   ENDDO


   DO j = jts,MIN(jte,jde-1)
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_mu_1(i,j) * b_mu_1(i,j)    &
                  + grid%a_mu_2(i,j) * b_mu_2(i,j)
   ENDDO
   ENDDO

   DO mm = 1, num_moist
   DO j = jts,MIN(jte,jde-1)
   DO k = kts,kte-1
   DO i = its,MIN(ite,ide-1)
     save  = save + grid%a_moist(i,k,j,mm) * b_moist(i,k,j,mm)
   ENDDO
   ENDDO
   ENDDO
   ENDDO

   RETURN
  END SUBROUTINE inner_dot_a_b
	
  SUBROUTINE deallocate_grid ( )
   INTEGER :: ierr

   DEALLOCATE ( s_u_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_1. ')
   ENDIF
   DEALLOCATE ( s_u_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_u_2. ')
   ENDIF
   DEALLOCATE ( s_v_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_v_1. ')
   ENDIF
   DEALLOCATE ( s_v_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_v_2. ')
   ENDIF
   DEALLOCATE ( s_w_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_w_1. ')
   ENDIF
   DEALLOCATE ( s_w_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_w_2. ')
   ENDIF
   DEALLOCATE ( s_ph_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ph_1. ')
   ENDIF
   DEALLOCATE ( s_ph_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_ph_2. ')
   ENDIF
   DEALLOCATE ( s_t_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_t_1. ')
   ENDIF
   DEALLOCATE ( s_t_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_t_2. ')
   ENDIF
   DEALLOCATE ( s_mu_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate s_mu_1. ')
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


   DEALLOCATE ( b_u_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_u_1. ')
   ENDIF
   DEALLOCATE ( b_u_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_u_2. ')
   ENDIF
   DEALLOCATE ( b_v_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_v_1. ')
   ENDIF
   DEALLOCATE ( b_v_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_v_2. ')
   ENDIF
   DEALLOCATE ( b_w_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_w_1. ')
   ENDIF
   DEALLOCATE ( b_w_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_w_2. ')
   ENDIF
   DEALLOCATE ( b_ph_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_ph_1. ')
   ENDIF
   DEALLOCATE ( b_ph_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_ph_2. ')
   ENDIF
   DEALLOCATE ( b_t_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_t_1. ')
   ENDIF
   DEALLOCATE ( b_t_2, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_t_2. ')
   ENDIF
   DEALLOCATE ( b_mu_1, STAT=ierr )
   IF (ierr.NE.0) THEN
     CALL wrf_error_fatal ( &
     'share/module_adtl_grid_utilities.f: Failed to deallocate b_mu_1. ')
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

   RETURN
  END SUBROUTINE deallocate_grid

END MODULE module_adtl_grid_utilities

